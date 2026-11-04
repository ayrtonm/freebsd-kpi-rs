# This makefile is now included twice. First it's included directly by the module makefile. That
# executes the first branch of this .if which includes bsd.kmod.mk. The patches in this repo then
# get bsd.kmod.mk to include this makefile again which executes the second branch here. That's what
# actually defines the rust build rules. ...it's convoluted, but as long as the build system needs
# to be patched it might be the best way to make sure there are obvious build errors if a developer
# forgets to patch the build system. Previously forgetting to patch the build system would just
# produce kernel modules from the .c files and silently ignore RUST_SRCS. Fortunately this nonsense
# is only temporarily needed for modules since the rest of the kernel goes through config(8).
.if !defined(__KLD_SHARED)

.include <bsd.kmod.mk>
.if !empty(RUST_SRCS) && !defined(RUST_OBJS)
.error Tried building a rust modules but RUST_OBJS is not defined. Did you apply patches/rust_mk_integration.diff?
.endif

.else

.include "${SYSDIR}/rust/kern.rust.mk"

# Override variables that are kernel-specific
RUST_MAKEFILE=	${SRCTOP}/sys/rust/kmod.rust.mk

.if ${__KLD_SHARED} == yes
RUSTFLAGS:=	${RUSTFLAGS:N-Crelocation-model=*} -Crelocation-model=pic
.endif

.for _rs in ${RUST_SRCS}
RLIBS+=	lib${_rs:R}.rlib

lib${_rs:R}.rlib: ${.CURDIR}/${_rs} ${RUST_DEFAULT_DEP} ${RUST_MAKEFILE}
	${NORMAL_R}
.endfor

.include "${SYSDIR}/rust/kern.rustroot.mk"

_RUST_LOCAL_FILES=	${RLIBS} ${RUSTROOT_OBJS} ${RUSTROOT_RS} ${BINDINGS_RS} \
	${BINDGEN_GENERATED_SRC} ${BINDGEN_DEPS} \
	${RUST_CORE} ${RUST_FAKE_BUILTINS} ${RUST_KPI}

.NOPATH: ${_RUST_LOCAL_FILES}

CLEANFILES+=	${_RUST_LOCAL_FILES}

.endif
