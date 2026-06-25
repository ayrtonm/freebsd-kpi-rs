# Rust support for kernel modules.
# Builds libcore, libcompiler_builtins, and libkpi locally rather than
# reusing artifacts from KERNBUILDDIR to avoid dependency tracking issues.

.include "${SYSDIR}/rust/kern.rust.mk"

# Override variables that are kernel-specific
RUST_MAKEFILE=	${SRCTOP}/sys/rust/kmod.rust.mk
RUST_OBJS=	${RUSTROOT_A}

.if ${__KLD_SHARED} == yes
RUSTFLAGS:=	${RUSTFLAGS:N-Crelocation-model=*} -Crelocation-model=pic
.endif

.for _rs in ${RUST_SRCS}
RLIBS+=	lib${_rs:R}.rlib

lib${_rs:R}.rlib: ${.CURDIR}/${_rs} ${RUST_DEFAULT_DEP} ${RUST_MAKEFILE}
	${NORMAL_R}
.endfor

.include "${SYSDIR}/rust/kern.rustroot.mk"

CLEANFILES+=	${RLIBS} ${RUSTROOT_A} ${RUSTROOT_RS} ${BINDINGS_RS} \
	${BINDGEN_GENERATED_SRC} ${BINDGEN_INLINE_SRC} ${BINDGEN_DEPS} \
	${RUST_CORE} ${RUST_FAKE_BUILTINS} ${RUST_KPI}
