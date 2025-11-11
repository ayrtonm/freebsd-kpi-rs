BINDGEN_VERSION_EXPECT= bindgen 0.72.0
BINDGEN_VERSION= ${:!${BINDGEN} --version!}
.if ${BINDGEN_VERSION} != ${BINDGEN_VERSION_EXPECT}
.warn unexpected bindgen version: ${BINDGEN_VERSION}, expected: ${BINDGEN_VERSION_EXPECT}
.endif

RUSTC_VERSION_EXPECT= rustc 1.90.0-nightly (a7a1618e6 2025-07-22)
RUSTC_VERSION= ${:!${RUSTC} --version!}
.if ${RUSTC_VERSION} != ${RUSTC_VERSION_EXPECT}
.error wrong rust compiler version: ${RUSTC_VERSION}, expected: ${RUSTC_VERSION_EXPECT}
.endif

# crate-independent build flags
RUSTFLAGS= --target=${RUST_TARGET} --edition 2024 -Copt-level=3

.if ${TARGET_ARCH} == aarch64

RUST_TARGET= aarch64-unknown-none-softfloat
RUST_KPI_FEATURES= --cfg 'feature="intrng"'
RUSTFLAGS+= -Zfixed-x18 -Zbranch-protection=bti

.if ${MK_FDT} != "no"
RUST_KPI_FEATURES+= --cfg 'feature="fdt"'
.endif

.elif ${TARGET_ARCH} == amd64

RUST_TARGET= x86_64-unknown-none
RUST_KPI_FEATURES=
RUSTFLAGS+= -Ccode-model=kernel \
	-Crelocation-model=static \
	-Crelro-level=off \
	-Cno-redzone=true \
	-Cforce-frame-pointers=true \
	-Clink-args="-Wl,--as-needed -m64 -Wl,-z,noexecstack" \
	-Ctarget-feature="+soft-float,-mmx,-sse,-sse2,-sse3,-sse4.1,-sse4.2,-avx,-avx2" \

.else

.error Unknown TARGET_ARCH: ${TARGET_ARCH}

.endif

BINDGEN_INLINE_SRC = inlines.c
BINDGEN_FLAGS= \
	--use-core \
	--rust-edition 2024 \
	--rust-target 1.90 \
	--no-prepend-enum-name \
	--no-layout-tests \
	--with-derive-default \
	--no-derive-copy \
	--default-macro-constant-type signed \
	--merge-extern-blocks \
	--builtins \
	--blocklist-item 'device_t' \
	--blocklist-item '__va_list' \
	--blocklist-item 'mcontext_t' \
	--blocklist-item 'vfpstate' \
	--blocklist-item 'pcb' \
	--no-debug mdproc \
	--no-debug nvme_namespace_data \
	--no-debug pcpu \
	--experimental \
	--wrap-static-fns \
	--wrap-static-fns-path ${BINDGEN_INLINE_SRC}

RUST_COMPILER_DIR= ${SRCTOP}/sys/rust/compiler
# An arbitrary directory for the sysroot
RUST_SYSROOT= rust_sysroot
# sysroot path required by rustc
RUST_LIBDIR= ${RUST_SYSROOT}/lib/rustlib/${RUST_TARGET}/lib

RUST_CORE= ${RUST_LIBDIR}/libcore.rlib
RUST_FAKE_BUILTINS= ${RUST_LIBDIR}/libcompiler_builtins.rlib
RUST_KPI= libkpi.rlib
RUST_KPI_SOURCES= \
	${SRCTOP}/sys/rust/kpi/src/sync/mtx.rs \
	${SRCTOP}/sys/rust/kpi/src/sync/arc.rs \
	${SRCTOP}/sys/rust/kpi/src/sync/mod.rs \
	${SRCTOP}/sys/rust/kpi/src/ofw.rs \
	${SRCTOP}/sys/rust/kpi/src/bus/dma.rs \
	${SRCTOP}/sys/rust/kpi/src/bus/mod.rs \
	${SRCTOP}/sys/rust/kpi/src/lib.rs \
	${SRCTOP}/sys/rust/kpi/src/tty.rs \
	${SRCTOP}/sys/rust/kpi/src/taskq.rs \
	${SRCTOP}/sys/rust/kpi/src/vec.rs \
	${SRCTOP}/sys/rust/kpi/src/intr/intrng.rs \
	${SRCTOP}/sys/rust/kpi/src/intr/mod.rs \
	${SRCTOP}/sys/rust/kpi/src/device.rs \
	${SRCTOP}/sys/rust/kpi/src/interfaces/nvme.rs \
	${SRCTOP}/sys/rust/kpi/src/interfaces/mod.rs \
	${SRCTOP}/sys/rust/kpi/src/arm64.rs \
	${SRCTOP}/sys/rust/kpi/src/bindings.rs \
	${SRCTOP}/sys/rust/kpi/src/driver.rs \
	${SRCTOP}/sys/rust/kpi/src/macros.rs \
	${SRCTOP}/sys/rust/kpi/src/boxed.rs \
	${SRCTOP}/sys/rust/kpi/src/panic.rs \
	${SRCTOP}/sys/rust/kpi/src/ffi.rs \
	${SRCTOP}/sys/rust/kpi/src/malloc.rs \
	${SRCTOP}/sys/rust/kpi/src/tests.rs \

BINDINGS_RS= bindings.rs

RUSTROOT_RS= rustroot.rs
# The only rust build artifact that gets directly linked into the kernel is this static lib.
RUSTROOT_A= rustroot.a
# This must be here to ensure it gets added to SYSTEM_OBJS
RUST_OBJS= ${RUSTROOT_A} ${BINDGEN_INLINE_SRC:S/.c/.o/}
RUST_MAKEFILE= ${SRCTOP}/sys/rust/kern.rust.mk
RUSTROOT_DEP= ${RLIBS}

RUST_DEFAULT_DEP= ${RUST_KPI} ${RUST_CORE} ${RUST_FAKE_BUILTINS}
RLIB_RULE= ${RUSTC} ${RUSTFLAGS} -Cdebuginfo=full --crate-type rlib --sysroot=$(PWD)/${RUST_SYSROOT}

NORMAL_R= ${RLIB_RULE} -o ${.TARGET} ${.ALLSRC:M*.rs} \
	${.ALLSRC:T:Nlibcore.rlib:Nlibcompiler_builtins.rlib:M*.rlib:C/.rlib$//:C/^lib//:C/.*/--extern \0=lib\0.rlib/}

${RUST_CORE}:
	${RUSTC} ${RUSTFLAGS} --crate-name core ${RUST_COMPILER_DIR}/library/core/src/lib.rs \
		--crate-type rlib --out-dir ${RUST_LIBDIR} \
		-Cstrip=debuginfo -Cembed-bitcode=no -Zforce-unstable-if-unmarked

${RUST_FAKE_BUILTINS}: ${RUST_CORE}
	${RUSTC} ${RUSTFLAGS} ${SRCTOP}/sys/rust/compiler_builtins.rs --crate-type rlib \
		--extern core=${RUST_CORE} \
		-o ${RUST_LIBDIR}/libcompiler_builtins.rlib

${BINDGEN_INLINE_SRC}: ${BINDINGS_RS}

${BINDINGS_RS}: ${SRCTOP}/sys/rust/bindings.h ${RUST_MAKEFILE}
	${BINDGEN} ${BINDGEN_FLAGS} ${SRCTOP}/sys/rust/bindings.h -- ${CFLAGS} -DBINDGEN > bindings.rs

${RUST_KPI}: ${RUST_FAKE_BUILTINS} ${BINDINGS_RS} ${RUST_KPI_SOURCES}
	OUT_DIR=$(PWD) ${RLIB_RULE} ${SRCTOP}/sys/rust/kpi/src/lib.rs -o ${.TARGET}

${RUSTROOT_RS}: ${RUSTROOT_DEP} ${RUST_MAKEFILE}
	printf "#![no_std]\nextern crate kpi;\n${RLIBS:S/.rlib/;/:S/lib/extern crate /}" > ${RUSTROOT_RS}
