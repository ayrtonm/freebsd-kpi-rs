BINDGEN_VERSION_EXPECT= bindgen 0.72.0
BINDGEN_VERSION= ${:!${BINDGEN} --version!}
.if ${BINDGEN_VERSION} != ${BINDGEN_VERSION_EXPECT}
.warn unexpected bindgen version: ${BINDGEN_VERSION}, expected: ${BINDGEN_VERSION_EXPECT}
.endif

RUST_COMPILER_DIR= ${SRCTOP}/sys/rust/compiler
RUSTC_BIN_INFO_CMD= ${RUSTC} --version --verbose
# Figure out the rust repo commit hash that the host compiler was built from
RUSTC_BIN_HASH= ${:!${RUSTC_BIN_INFO_CMD}!:[*]:C/.*commit-hash://:[@]:[1]}

# Check if the host rust compiler is a nightly release
RUSTC_BIN_NIGHTLY= ${:!${RUSTC_BIN_INFO_CMD}!:[2]:M*nightly}
.if ${RUSTC_BIN_NIGHTLY} == ""
.error Host rust compiler must be a nightly release
.endif

# Get the host rust compiler version
RUSTC_BIN_VERSION= ${:!${RUSTC_BIN_INFO_CMD}!:[2]:R}

# Check if the user cloned the rust repo into sys/rust/compiler
.if !exists(${RUST_COMPILER_DIR})
.error Did not find rust compiler repo in expected location. Run\
	`git clone --depth 1 https://github.com/rust-lang/rust sys/rust/compiler` and checkout the\
	required commit for the host rust compiler (${RUSTC_BIN_HASH}).
.endif

# Figure out the currently checked out rust repo commit
RUSTC_REPO_HASH= ${:!${GIT_CMD} -C ${RUST_COMPILER_DIR} rev-parse HEAD!}

# Check if the user checked out the correct rust repo commit for the host compiler
.if ${RUSTC_BIN_HASH} != ${RUSTC_REPO_HASH}
.error Rust compiler repo checkout does not match host rust compiler. Please run\
	`git -C sys/rust/compiler fetch --depth 1 origin ${RUSTC_BIN_HASH} && git -C sys/rust/compiler checkout FETCH_HEAD`.
.endif

# crate-independent build flags
RUSTFLAGS= --target=${RUST_TARGET} --edition 2024 -Copt-level=3 -Ccodegen-units=1 -Cembed-bitcode=no

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
	-Ctarget-feature="-sse,-sse2,-sse3,-sse4.1,-sse4.2,-avx,-avx2" \

.else

.error Unsupported TARGET_ARCH: ${TARGET_ARCH}

.endif

BINDGEN_INLINE_SRC = inlines.c
BINDGEN_FLAGS= \
	--use-core \
	--rust-edition 2024 \
	--rust-target ${RUSTC_BIN_VERSION} \
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
	--blocklist-function 'fo_read' \
	--blocklist-function 'fo_write' \
	--blocklist-function 'fo_truncate' \
	--blocklist-function 'fo_ioctl' \
	--blocklist-function 'fo_poll' \
	--blocklist-function 'fo_kqfilter' \
	--blocklist-function 'fo_stat' \
	--blocklist-function 'fo_close' \
	--blocklist-function 'fo_chmod' \
	--blocklist-function 'fo_chown' \
	--blocklist-function 'fo_sendfile' \
	--no-debug _device \
	--no-debug mdproc \
	--no-debug nvme_namespace_data \
	--no-debug pcpu \
	--experimental \
	--wrap-static-fns \
	--wrap-static-fns-path ${BINDGEN_INLINE_SRC}

# An arbitrary directory for the sysroot
RUST_SYSROOT= rust_sysroot
# sysroot path required by rustc
RUST_LIBDIR= ${RUST_SYSROOT}/lib/rustlib/${RUST_TARGET}/lib

RUST_CORE= ${RUST_LIBDIR}/libcore.rlib
RUST_FAKE_BUILTINS= ${RUST_LIBDIR}/libcompiler_builtins.rlib
RUST_KPI= libkpi.rlib
RUST_KPI_SOURCES= \
	${SRCTOP}/sys/rust/kpi/src/kobj/casts.rs \
	${SRCTOP}/sys/rust/kpi/src/kobj/define_class.rs \
	${SRCTOP}/sys/rust/kpi/src/kobj/method_table.rs \
	${SRCTOP}/sys/rust/kpi/src/kobj/macros.rs \
	${SRCTOP}/sys/rust/kpi/src/kobj/mod.rs \
	${SRCTOP}/sys/rust/kpi/src/sync/mtx.rs \
	${SRCTOP}/sys/rust/kpi/src/sync/arc.rs \
	${SRCTOP}/sys/rust/kpi/src/sync/mod.rs \
	${SRCTOP}/sys/rust/kpi/src/ofw.rs \
	${SRCTOP}/sys/rust/kpi/src/collections.rs \
	${SRCTOP}/sys/rust/kpi/src/bus/dma.rs \
	${SRCTOP}/sys/rust/kpi/src/bus/mod.rs \
	${SRCTOP}/sys/rust/kpi/src/lib.rs \
	${SRCTOP}/sys/rust/kpi/src/tty.rs \
	${SRCTOP}/sys/rust/kpi/src/taskq.rs \
	${SRCTOP}/sys/rust/kpi/src/vec.rs \
	${SRCTOP}/sys/rust/kpi/src/intr/intrng.rs \
	${SRCTOP}/sys/rust/kpi/src/intr/mod.rs \
	${SRCTOP}/sys/rust/kpi/src/device.rs \
	${SRCTOP}/sys/rust/kpi/src/arm64.rs \
	${SRCTOP}/sys/rust/kpi/src/bindings.rs \
	${SRCTOP}/sys/rust/kpi/src/driver.rs \
	${SRCTOP}/sys/rust/kpi/src/macros.rs \
	${SRCTOP}/sys/rust/kpi/src/boxed.rs \
	${SRCTOP}/sys/rust/kpi/src/panic.rs \
	${SRCTOP}/sys/rust/kpi/src/ffi/mod.rs \
	${SRCTOP}/sys/rust/kpi/src/ffi/cstring.rs \
	${SRCTOP}/sys/rust/kpi/src/ffi/subclass.rs \
	${SRCTOP}/sys/rust/kpi/src/malloc.rs \
	${SRCTOP}/sys/rust/kpi/src/tests.rs \

BINDINGS_RS= bindings.rs

RUSTROOT_RS= rustroot.rs
# The only rust build artifact that gets directly linked into the kernel is this static lib.
RUSTROOT_A= rustroot.a
# This must be here to ensure it gets added to SYSTEM_OBJS
RUST_OBJS= ${RUSTROOT_A} ${BINDGEN_INLINE_SRC:S/.c/.o/}
RUST_MAKEFILE= ${SRCTOP}/sys/rust/kern.rust.mk

RUST_DEFAULT_DEP= ${RUST_KPI} ${RUST_CORE} ${RUST_FAKE_BUILTINS}
RLIB_RULE= ${RUSTC} ${RUSTFLAGS} -Cdebuginfo=full --crate-type rlib --sysroot=$(PWD)/${RUST_SYSROOT} -L.

NORMAL_R= ${RLIB_RULE} ${RUST_KPI_FEATURES} -o ${.TARGET} ${.ALLSRC:M*.rs} \
	${.ALLSRC:T:Nlibcore.rlib:Nlibcompiler_builtins.rlib:M*.rlib:C/.rlib$//:C/^lib//:C/.*/--extern \0=lib\0.rlib/}

${RUST_CORE}: ${RUST_MAKEFILE}
	${RUSTC} ${RUSTFLAGS} --crate-name core ${RUST_COMPILER_DIR}/library/core/src/lib.rs \
		--crate-type rlib --out-dir ${RUST_LIBDIR} \
		-Cstrip=debuginfo -Cembed-bitcode=no -Zforce-unstable-if-unmarked

${RUST_FAKE_BUILTINS}: ${RUST_CORE} ${RUST_MAKEFILE}
	${RUSTC} ${RUSTFLAGS} ${SRCTOP}/sys/rust/compiler_builtins.rs --crate-type rlib \
		--extern core=${RUST_CORE} \
		-o ${RUST_LIBDIR}/libcompiler_builtins.rlib

${BINDGEN_INLINE_SRC}: ${BINDINGS_RS} ${RUST_MAKEFILE}

${BINDINGS_RS}: ${SRCTOP}/sys/rust/bindings.h ${RUST_MAKEFILE}
	${BINDGEN} ${BINDGEN_FLAGS} ${SRCTOP}/sys/rust/bindings.h -- ${CFLAGS} -DBINDGEN > bindings.rs

${RUST_KPI}: ${RUST_FAKE_BUILTINS} ${BINDINGS_RS} ${RUST_KPI_SOURCES}
	OUT_DIR=$(PWD) ${RLIB_RULE} --crate-name kpi ${RUST_KPI_FEATURES} ${SRCTOP}/sys/rust/kpi/src/lib.rs -o ${.TARGET}
