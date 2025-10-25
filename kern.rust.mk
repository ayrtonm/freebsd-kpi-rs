RSFILES= $S/rust/kpi/src/lib.rs \
	$S/arm64/apple/apple_mbox.rs \
	$S/arm64/apple/rtkit/src/lib.rs \
	$S/arm64/apple/apple_rtkit.rs \
	$S/arm64/apple/apple_aic.rs \
	$S/arm64/apple/apple_smc.rs \
	$S/dev/nvme/nvme_ans.rs \

CRATES=
# Turn .rs filenames into crate names
.for _rsf in ${RSFILES}
# If the crate a lib.rs go up two directories to find the crate name
## Otherwise use the filename as the crate name
CRATES+= ${_rsf:S/\/src\/lib.rs//:T:S/.rs//}
.endfor

# TODO: Find a better way to handle multifile crate dependencies 
KPI_CRATE= lib.rs \
	arm64.rs \
	bindings.rs \
	boxed.rs \
	bus/mod.rs \
	bus/dma.rs \
	cell.rs \
	device.rs \
	driver.rs \
	ffi.rs \
	interfaces/mod.rs \
	interfaces/nvme.rs \
	intr.rs \
	macros.rs \
	malloc.rs \
	ofw.rs \
	panic.rs \
	ptr.rs \
	sync/mod.rs \
	sync/arc.rs \
	sync/mtx.rs \
	taskq.rs \
	tty.rs \
	vec.rs

KPI_CRATE_FILES= ${KPI_CRATE:S/^/$S\/rust\/kpi\/src\//g}

RUSTFLAGS= -Awarnings

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

.if ${TARGET_ARCH} == aarch64
RUST_TARGET= aarch64-unknown-none-softfloat
RUST_KPI_FEATURES= --cfg 'feature="intrng"'
RUSTFLAGS+= -Zfixed-x18 -Zbranch-protection=bti
.elif ${TARGET_ARCH} == amd64
RUST_TARGET= x86_64-unknown-none
RUST_KPI_FEATURES=
.else
.error Unknown TARGET_ARCH: ${TARGET_ARCH}
.endif

.if ${MK_FDT} != "no"
RUST_KPI_FEATURES+= --cfg 'feature="fdt"'
.endif

# crate-independent build flags
RUSTFLAGS+= --target=${RUST_TARGET} --edition 2024 -Copt-level=3

#RUSTFLAGS+= -Clinker-plugin-lto

RUST_SYSROOT= rust_sysroot
RUST_LIBDIR= ${RUST_SYSROOT}/lib/rustlib/${RUST_TARGET}/lib
# the rustc invocation for rlibs
NORMAL_RS= ${RUSTC} ${RUSTFLAGS} ${RUST_KPI_FEATURES} -Cdebuginfo=full --crate-type rlib \
	-o ${.TARGET} --sysroot=$(PWD)/${RUST_SYSROOT}

# the only rust build artifact that gets directly linked into the kernel is this static lib
RUSTROOT_RS= rustroot.rs
RUSTROOT_A= rustroot.a
RUST_OBJS= ${RUSTROOT_A} inlines.o

RLIBS= ${CRATES:C/^.*$/lib\0.rlib/g}

EXTERN_ARGS= ${CRATES:C/^.*$/--extern \0=lib\0.rlib/g}

# TODO: bindgen's flag for static fn wrappers chokes too easily so I should add something like
# inlines.c to rustroot.a. but I'll also need the C decls to call them from rust which leads me back
# to the amalgamation question...
BINDGENFLAGS= \
    --use-core \
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
    --wrap-static-fns \
    --wrap-static-fns-path inlines.c

    #--blocklist-item 'system_segment_descriptor' # blocked because bindgen cannot derive Debug trait

bindings.rs: $S/rust/bindings.h
	${BINDGEN} ${BINDGENFLAGS} $S/rust/bindings.h -- ${CFLAGS} -DBINDGEN > bindings.rs

inlines.c: bindings.rs

${RUST_LIBDIR}/libcore.rlib:
	${RUSTC} ${RUSTFLAGS} --crate-name core $S/rust/compiler/library/core/src/lib.rs \
		--crate-type rlib --out-dir ${RUST_LIBDIR} \
		-Cstrip=debuginfo -Cembed-bitcode=no -Zforce-unstable-if-unmarked

${RUST_LIBDIR}/libcompiler_builtins.rlib: ${RUST_LIBDIR}/libcore.rlib
	${RUSTC} ${RUSTFLAGS} $S/rust/compiler_builtins.rs --crate-type rlib \
		--extern core=${RUST_LIBDIR}/libcore.rlib \
		-o ${RUST_LIBDIR}/libcompiler_builtins.rlib

# TODO: OUT_DIR env var used to include! bindings.rs in the idiomatic way. Maybe there's an existing
# env var to anchor the path
libkpi.rlib: ${KPI_CRATE_FILES} bindings.rs ${MFILES:T:S/.m$/.h/g} ${RUST_LIBDIR}/libcompiler_builtins.rlib
	OUT_DIR=$(PWD) ${NORMAL_RS} $S/rust/kpi/src/lib.rs

.for _rsf in ${RSFILES:N$S/rust/kpi/src/lib.rs}
# If the crate a lib.rs go up two directories to find the crate name
## Otherwise use the filename as the crate name
_crate= ${_rsf:S/\/src\/lib.rs//:T:S/.rs//}

.if ${_crate} == rtkit
_crate_deps= kpi apple_mbox

.elif ${_crate} == apple_mbox
_crate_deps= kpi

.elif ${_crate} == apple_rtkit
_crate_deps= kpi rtkit apple_mbox

.elif ${_crate} == apple_aic
_crate_deps= kpi

.elif ${_crate} == nvme_ans
_crate_deps= kpi rtkit

.elif ${_crate} == apple_smc
_crate_deps= kpi rtkit apple_mbox

.endif
${_crate}_rlibs:= ${_crate_deps:S/^/lib/g:S/$/.rlib/g}
${_crate}_extern_args:= ${_crate_deps:C/^.*$/--extern \0=lib\0.rlib/g}

lib${_crate}.rlib: ${_rsf} ${${_crate}_rlibs}
	${NORMAL_RS} ${_rsf} ${${_rsf:S/\/src\/lib.rs//:T:S/.rs//}_extern_args} \
		--crate-name ${_rsf:S/\/src\/lib.rs//:T:S/.rs//} -L .
.endfor

# TODO: This depends on the list of input rlibs
${RUSTROOT_RS}: ${RSFILES}
	printf "#![no_std]\n${CRATES:C/^.*$/extern crate \0;/g}" > ${RUSTROOT_RS}

${RUSTROOT_A}: ${RLIBS} ${RUSTROOT_RS} inlines.o
	${RUSTC} ${RUSTFLAGS} --crate-type staticlib -o ${RUSTROOT_A} ${RUSTROOT_RS} \
		${EXTERN_ARGS} --sysroot=$(PWD)/${RUST_SYSROOT}

LDFLAGS += -Wl,--whole-archive
