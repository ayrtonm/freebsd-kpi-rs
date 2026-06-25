.if !defined(KERNBUILDDIR)
.error uh oh
.endif

.include "${SYSDIR}/rust/kern.rust.mk"

# Override paths to reuse prebuilt rust artifacts from the kernel build dir.
# kern.rust.mk sets RUST_SYSROOT to a relative path and RLIB_RULE uses
# $(PWD)/${RUST_SYSROOT}, so we need to override both.
RUST_SYSROOT=	${KERNBUILDDIR}/rust_sysroot
RUST_LIBDIR=	${RUST_SYSROOT}/lib/rustlib/${RUST_TARGET}/lib
RUST_CORE=	${RUST_LIBDIR}/libcore.rlib
RUST_FAKE_BUILTINS=	${RUST_LIBDIR}/libcompiler_builtins.rlib
RUST_KPI=	${KERNBUILDDIR}/libkpi.rlib
BINDINGS_RS=	${KERNBUILDDIR}/bindings.rs
RLIB_RULE=	${RUSTC} ${RUSTFLAGS} -Cdebuginfo=full --crate-type rlib --sysroot=${RUST_SYSROOT} -L. -L${KERNBUILDDIR} --emit link,dep-info=.depend.${.TARGET}
NORMAL_R=	${RLIB_RULE} ${RUST_KPI_FEATURES} -o ${.TARGET} ${.ALLSRC:[1]} \
	--extern kpi=${RUST_KPI}

.if ${__KLD_SHARED} == yes
RUSTFLAGS:= ${RUSTFLAGS:N-Crelocation-model=*} -Crelocation-model=pic
.endif

.for _rs in ${RUST_SRCS}
RLIBS+= lib${_rs:R}.rlib

lib${_rs:R}.rlib: ${.CURDIR}/${_rs} ${RUST_DEFAULT_DEP}
	${NORMAL_R}
.endfor

_LDFLAGS += --whole-archive

RLIBS += ${RUST_KPI}

.for rlib in ${RLIBS}
.-include ".depend.${rlib}"
.endfor

${RUSTROOT_RS}: ${SYSDIR}/rust/kmod.rust.mk ${RLIBS}
	printf "#![no_std]\n${RLIBS:T:S/.rlib/;/:S/lib/extern crate /}" > ${RUSTROOT_RS}

RUSTROOT_EXTERN=
.for _rlib in ${RLIBS}
RUSTROOT_EXTERN+= --extern ${_rlib:T:C/.rlib$//:C/^lib//}=${_rlib}
.endfor

${RUSTROOT_A}: ${RUSTROOT_RS} ${RUST_CORE} ${RUST_FAKE_BUILTINS} ${RLIBS}
	${RUSTC} ${RUSTFLAGS} --crate-type staticlib -o ${RUSTROOT_A} ${RUSTROOT_RS} \
		--sysroot=${RUST_SYSROOT} \
		${RUSTROOT_EXTERN} \
		-L. -L${KERNBUILDDIR}

RUST_OBJS= ${RUSTROOT_A}
CLEANFILES+= ${RLIBS} ${RUSTROOT_A} ${RUSTROOT_RS} ${BINDINGS_RS} \
	${BINDGEN_GENERATED_SRC} ${BINDGEN_INLINE_SRC} ${BINDGEN_DEPS}
