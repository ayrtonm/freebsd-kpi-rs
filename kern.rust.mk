.ifndef BINDGEN
.error Please set the BINDGEN environment variable
.endif

.ifndef RUSTC
.error Please set the RUSTC environment variable
.endif

.ifndef RUSTFMT
.error Please set the RUSTFMT environment variable
.endif

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
RUSTFLAGS= --target=${RUST_TARGET} --edition 2024 -Copt-level=3 -Ccodegen-units=1 -Cembed-bitcode=yes -Clto=thin -Awarnings -Dunused_must_use -Cdebuginfo=full

.if ${TARGET_ARCH} == aarch64

RUST_TARGET= aarch64-unknown-none-softfloat
RUST_KPI_FEATURES= --cfg 'feature="intrng"'
RUSTFLAGS+= -Zfixed-x18 -Zbranch-protection=bti -Cforce-frame-pointers=true

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

# TODO: It would be better to have the existing build system set make flags and preprocessor defines
# based on the options in the KERNCONF instead of requiring the rust makefile to define these. The
# current solution should work for general KERNCONFs but adding new headers which are not available
# for all configs to bindings.h is a slight pain.
.if ${KERNCONF} == "APPLE"
BINDGEN_DEFINES= \
	-DKERNCONF_APPLE \
	-I${SRCTOP}/sys/contrib/device-tree/include \

.elif ${KERNCONF} == "VIRT"
BINDGEN_DEFINES= \
	-DWITH_VIRTIO \
	-DWITH_SOUND \

.elif ${KERNCONF} == "GENERIC"
BINDGEN_DEFINES= \
	-DWITH_VIRTIO \
	-DWITH_SOUND \

.elif ${KERNCONF} == "MINIMAL"

.endif

BINDGEN_DEPS= bindgen.d
BINDGEN_GENERATED_SRC= bindgen_inlines.c
CWARNFLAGS.${BINDGEN_GENERATED_SRC}+= -Wno-duplicate-decl-specifier -Wno-incompatible-pointer-types-discards-qualifiers
CFLAGS.${BINDGEN_GENERATED_SRC}+= ${BINDGEN_DEFINES}
BINDGEN_FLAGS= \
	--use-core \
	--depfile ${BINDGEN_DEPS} \
	--rust-edition 2024 \
	--rust-target ${RUSTC_BIN_VERSION} \
	--no-prepend-enum-name \
	--no-layout-tests \
	--with-derive-default \
	--default-macro-constant-type signed \
	--merge-extern-blocks \
	--builtins \
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
	--blocklist-function 'vm_inject_ud' \
	--blocklist-function 'vm_inject_gp' \
	--blocklist-function 'vm_inject_ac' \
	--blocklist-function 'vm_inject_ss' \
	--no-debug _device \
	--no-debug mdproc \
	--no-debug nvme_namespace_data \
	--no-debug pcpu \
	--experimental \
	--wrap-static-fns \
	--wrap-static-fns-path ${BINDGEN_GENERATED_SRC}

# Modules are shared objects on all architectures except amd64 (see __KLD_SHARED in kmod.mk) so
# they need PIC code and a separate sysroot. amd64 modules use the same flags as the kernel.
.if ${MACHINE_CPUARCH} != amd64
RUSTFLAGS_KMOD:= ${RUSTFLAGS:N-Crelocation-model=*} -Crelocation-model=pic
RUST_SYSROOT_KMOD_NAME= rust_sysroot_kmod
.else
RUSTFLAGS_KMOD:= ${RUSTFLAGS}
RUST_SYSROOT_KMOD_NAME= rust_sysroot
.endif

# Paths of the sysroot crates relative to the sysroot directory expected by rustc
_RUST_LIBSUBDIR= lib/rustlib/${RUST_TARGET}/lib
_RUST_CORE_RLIB= ${_RUST_LIBSUBDIR}/libcore.rlib
_RUST_BUILTINS_RLIB= ${_RUST_LIBSUBDIR}/libcompiler_builtins.rlib

# _RUST_SYSROOTS is a list of (sysroot directory, flags variable) pairs to build sysroots for.
# __KLD_SHARED is only defined when building a module.
.if defined(__KLD_SHARED)
RUSTFLAGS:= ${RUSTFLAGS_KMOD}
.if defined(KERNBUILDDIR)
# Modules built as part of a kernel build share the module sysroot built by the kernel
RUST_SYSROOT= ${KERNBUILDDIR}/${RUST_SYSROOT_KMOD_NAME}
.else
RUST_SYSROOT= ${.OBJDIR}/rust_sysroot
_RUST_SYSROOTS= ${RUST_SYSROOT} RUSTFLAGS
.endif
.else
RUST_SYSROOT= ${.OBJDIR}/rust_sysroot
_RUST_SYSROOTS= ${RUST_SYSROOT} RUSTFLAGS
.if ${RUST_SYSROOT_KMOD_NAME} != rust_sysroot
_RUST_SYSROOTS+= ${.OBJDIR}/${RUST_SYSROOT_KMOD_NAME} RUSTFLAGS_KMOD
.endif
.if !defined(NO_MODULES)
# The module sysroot must exist before building modules
modules-all: ${.OBJDIR}/${RUST_SYSROOT_KMOD_NAME}/${_RUST_BUILTINS_RLIB}
.endif
.endif

RUST_CORE= ${RUST_SYSROOT}/${_RUST_CORE_RLIB}
RUST_FAKE_BUILTINS= ${RUST_SYSROOT}/${_RUST_BUILTINS_RLIB}
RUST_KPI= libkpi.rlib
RUST_KPI_SOURCES= \
	${SRCTOP}/sys/rust/kpi/src/kobj/casts.rs \
	${SRCTOP}/sys/rust/kpi/src/kobj/define_interface.rs \
	${SRCTOP}/sys/rust/kpi/src/kobj/define_class.rs \
	${SRCTOP}/sys/rust/kpi/src/kobj/method_table.rs \
	${SRCTOP}/sys/rust/kpi/src/kobj/mod.rs \
	${SRCTOP}/sys/rust/kpi/src/sync/mtx.rs \
	${SRCTOP}/sys/rust/kpi/src/sync/sx.rs \
	${SRCTOP}/sys/rust/kpi/src/sync/arc.rs \
	${SRCTOP}/sys/rust/kpi/src/sync/mod.rs \
	${SRCTOP}/sys/rust/kpi/src/ofw.rs \
	${SRCTOP}/sys/rust/kpi/src/net.rs \
	${SRCTOP}/sys/rust/kpi/src/collections.rs \
	${SRCTOP}/sys/rust/kpi/src/bus/dma.rs \
	${SRCTOP}/sys/rust/kpi/src/bus/mod.rs \
	${SRCTOP}/sys/rust/kpi/src/lib.rs \
	${SRCTOP}/sys/rust/kpi/src/tty.rs \
	${SRCTOP}/sys/rust/kpi/src/vec.rs \
	${SRCTOP}/sys/rust/kpi/src/intr/intrng.rs \
	${SRCTOP}/sys/rust/kpi/src/intr/mod.rs \
	${SRCTOP}/sys/rust/kpi/src/device.rs \
	${SRCTOP}/sys/rust/kpi/src/module.rs \
	${SRCTOP}/sys/rust/kpi/src/taskqueue.rs \
	${SRCTOP}/sys/rust/kpi/src/arm64.rs \
	${SRCTOP}/sys/rust/kpi/src/cdev.rs \
	${SRCTOP}/sys/rust/kpi/src/bindings.rs \
	${SRCTOP}/sys/rust/kpi/src/driver.rs \
	${SRCTOP}/sys/rust/kpi/src/ffi/softc.rs \
	${SRCTOP}/sys/rust/kpi/src/ffi/cstring.rs \
	${SRCTOP}/sys/rust/kpi/src/ffi/subclass.rs \
	${SRCTOP}/sys/rust/kpi/src/ffi/mod.rs \
	${SRCTOP}/sys/rust/kpi/src/macros.rs \
	${SRCTOP}/sys/rust/kpi/src/boxed.rs \
	${SRCTOP}/sys/rust/kpi/src/panic.rs \
	${SRCTOP}/sys/rust/kpi/src/malloc.rs \

BINDINGS_RS= bindings.rs

RUSTROOT_RS= rustroot.rs
# The only rust build artifact that gets directly linked into the kernel is this static lib.
RUSTROOT_A= rustroot.a
# This must be here to ensure it gets added to SYSTEM_OBJS
RUST_OBJS= ${RUSTROOT_A} ${BINDGEN_GENERATED_SRC:S/.c/.o/}
RUST_MAKEFILE?= ${SRCTOP}/sys/rust/kern.rust.mk

RUST_DEFAULT_DEP= ${RUST_KPI} ${RUST_CORE} ${RUST_FAKE_BUILTINS}
# TODO: Use the depfile
RLIB_RULE= ${RUSTC} ${RUSTFLAGS} -Funstable-features -Cdebuginfo=full --crate-type rlib --sysroot=${RUST_SYSROOT} -L. --emit link,dep-info=.depend.${.TARGET}

NORMAL_R= ${RLIB_RULE} ${RUST_KPI_FEATURES} -o ${.TARGET} ${.ALLSRC:[1]} \
	${.ALLSRC:T:Nlibcore.rlib:Nlibcompiler_builtins.rlib:M*.rlib:C/.rlib$//:C/^lib//:C/.*/--extern \0=lib\0.rlib/}

.for _sysroot _flags in ${_RUST_SYSROOTS}
${_sysroot}/${_RUST_CORE_RLIB}: ${RUST_MAKEFILE}
	${RUSTC} ${${_flags}} --crate-name core ${RUST_COMPILER_DIR}/library/core/src/lib.rs \
		--crate-type rlib --out-dir ${.TARGET:H} \
		-Cstrip=debuginfo -Zforce-unstable-if-unmarked

${_sysroot}/${_RUST_BUILTINS_RLIB}: ${_sysroot}/${_RUST_CORE_RLIB} ${RUST_MAKEFILE}
	${RUSTC} ${${_flags}} ${SRCTOP}/sys/rust/compiler_builtins.rs --crate-type rlib \
		--extern core=${.ALLSRC:[1]} \
		-o ${.TARGET}
.endfor

# This tells make that generating bindings.rs by invoking bindgen also produces bindgen_inlines.c
# FIXME: If bindgen_inlines.c is deleted while bindings.rs is up-to-date this doesn't work
${BINDGEN_GENERATED_SRC}: ${BINDINGS_RS} ${RUST_MAKEFILE}

${BINDINGS_RS}: ${SRCTOP}/sys/rust/bindings.h ${RUST_MAKEFILE}
	${BINDGEN} ${BINDGEN_FLAGS} -o ${.TARGET} ${SRCTOP}/sys/rust/bindings.h -- ${CFLAGS} ${BINDGEN_DEFINES}

.-include "${.OBJDIR}/${BINDGEN_DEPS}"

${RUST_KPI}: ${RUST_FAKE_BUILTINS} ${BINDINGS_RS} ${RUST_KPI_SOURCES}
	OUT_DIR=${.OBJDIR} ${RLIB_RULE} --crate-name kpi ${RUST_KPI_FEATURES} ${SRCTOP}/sys/rust/kpi/src/lib.rs -o ${.TARGET}

_LDFLAGS += --whole-archive

# This file must be included after RLIBS is set. config(8) emits `libfoo.rlib: foo.rs
# ${RUST_DEFAULT_DEP} ...` before that point so RUST_DEFAULT_DEP expands to nothing there. Add it
# here instead, before RUST_KPI is added to RLIBS so libkpi.rlib doesn't depend on itself.
.if !empty(RLIBS)
${RLIBS}: ${RUST_DEFAULT_DEP}
.endif

RLIBS += ${RUST_KPI}

.for rlib in ${RLIBS}
.-include "${.OBJDIR}/.depend.${rlib}"
.endfor

${RUSTROOT_RS}: ${RUST_MAKEFILE} ${RLIBS}
	printf "#![no_std]\n${RLIBS:S/.rlib/;/:S/lib/extern crate /}" > ${RUSTROOT_RS}

${RUSTROOT_A}: ${RUSTROOT_RS} ${RUST_CORE} ${RUST_FAKE_BUILTINS} ${RLIBS}
	${RUSTC} ${RUSTFLAGS} --crate-type staticlib -o ${RUSTROOT_A} ${RUSTROOT_RS} \
		--sysroot=${RUST_SYSROOT} ${RLIBS:C/.rlib//:C/^lib//:C/.*/--extern \0=lib\0.rlib/} \
		-L.
