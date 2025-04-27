RSFILES= $S/rust/kpi/src/lib.rs \
	$S/arm64/apple/apple_aic.rs \
	$S/arm64/apple/apple_mbox.rs \
	$S/arm64/apple/rtkit.rs \
	$S/arm64/apple/apple_rtkit.rs


CRATES=
# Turn .rs filenames into crate names
.for _rsf in ${RSFILES}
# If the crate a lib.rs go up to directories to find the crate name
.if ${_rsf:T} == lib.rs
CRATES+= ${_rsf:S/\/src\/lib.rs//:T}
# Otherwise use the filename as the crate name
.else
CRATES+= ${_rsf:T:S/.rs//}
.endif
.endfor

# TODO: Find a better way to handle multifile crate dependencies 
KPI_CRATE= lib.rs \
	arm64.rs \
	bindings.rs \
	boxed.rs \
	bus.rs \
	cell.rs \
	device.rs \
	ffi.rs \
	intr.rs \
	macros.rs \
	malloc.rs \
	ofw.rs \
	panic.rs \
	sleep.rs \
	sync.rs \
	taskq.rs \
	tty.rs \
	vec.rs

KPI_CRATE_FILES= ${KPI_CRATE:S/^/$S\/rust\/kpi\/src\//g}

RUSTFLAGS=

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
RUSTFLAGS+= --target=${RUST_TARGET} --edition 2021 -Copt-level=3

RUST_SYSROOT= rust_sysroot
RUST_LIBDIR= ${RUST_SYSROOT}/lib/rustlib/${RUST_TARGET}/lib
# the rustc invocation for rlibs
NORMAL_RS= ${RUSTC} ${RUSTFLAGS} ${RUST_KPI_FEATURES} -Cdebuginfo=full --crate-type rlib \
	-o ${.TARGET} --sysroot=$(PWD)/${RUST_SYSROOT}

# the only rust build artifact that gets directly linked into the kernel is this static lib
RUSTROOT_RS= rustroot.rs
RUSTROOT_A= rustroot.a

RLIBS= ${CRATES:C/^.*$/lib\0.rlib/g}

EXTERN_ARGS= ${CRATES:C/^.*$/--extern \0=lib\0.rlib/g}

# TODO: bindgen's flag for static fn wrappers chokes too easily so I should add something like
# inlines.c to rustroot.a. but I'll also need the C decls to call them from rust which leads me back
# to the amalgamation question...
BINDGENFLAGS= \
    --use-core \
    --no-prepend-enum-name \
    --no-layout-tests \
    --no-derive-copy \
    --default-macro-constant-type signed \
    --merge-extern-blocks \
    --builtins \
    --blocklist-item '__va_list' \
    --blocklist-item 'mcontext_t' \
    --blocklist-item 'vfpstate' \
    --blocklist-item 'pcb' \
    --no-debug mdproc \
    --no-debug pcpu

    #--blocklist-item 'system_segment_descriptor' # blocked because bindgen cannot derive Debug trait

# doesn't really depend on the .o that's only to get bindgen to depend on changes to headers
bindings.rs: $S/rust/bindings.c bindings.o
	${BINDGEN} ${BINDGENFLAGS} $S/rust/bindings.c -- ${CFLAGS} -DBINDGEN > bindings.rs

libcore.rlib:
	mkdir -p ${RUST_LIBDIR}; \
	${RUSTC} ${RUSTFLAGS} --crate-name core $S/rust/compiler/library/core/src/lib.rs \
		--crate-type rlib --out-dir ${RUST_LIBDIR} \
		-Cstrip=debuginfo -Cembed-bitcode=no -Zforce-unstable-if-unmarked

libcompiler_builtins.rlib: libcore.rlib
	${RUSTC} ${RUSTFLAGS} $S/rust/compiler_builtins.rs --crate-type rlib \
		--extern core=${RUST_LIBDIR}/libcore.rlib \
		-o ${RUST_LIBDIR}/libcompiler_builtins.rlib

# TODO: OUT_DIR env var used to include! bindings.rs in the idiomatic way. Maybe there's an existing
# env var to anchor the path
libkpi.rlib: ${KPI_CRATE_FILES} bindings.rs ${MFILES:T:S/.m$/.h/g} libcompiler_builtins.rlib
	OUT_DIR=$(PWD) ${NORMAL_RS} $S/rust/kpi/src/lib.rs

.for _rsf in ${RSFILES:N$S/rust/kpi/src/lib.rs}
_crate= ${_rsf:T:S/.rs//}
.if ${_crate} == rtkit
_crate_deps= kpi apple_mbox

.elif ${_crate} == apple_mbox
_crate_deps= kpi

.elif ${_crate} == apple_rtkit
_crate_deps= kpi rtkit apple_mbox

.elif ${_crate} == apple_aic
_crate_deps= kpi

.endif
${_crate}_rlibs:= ${_crate_deps:S/^/lib/g:S/$/.rlib/g}
${_crate}_extern_args:= ${_crate_deps:C/^.*$/--extern \0=lib\0.rlib/g}
lib${_crate}.rlib: ${_rsf} ${${_crate}_rlibs}
	${NORMAL_RS} ${_rsf} ${${_rsf:T:S/.rs//}_extern_args} --crate-name ${_rsf:T:S/.rs//} -L .
.endfor

${RUSTROOT_RS}:
	printf "#![no_std]\n${CRATES:C/^.*$/extern crate \0;/g}" > ${RUSTROOT_RS}

${RUSTROOT_A}: ${RLIBS} ${RUSTROOT_RS}
	${RUSTC} ${RUSTFLAGS} --crate-type staticlib -o ${RUSTROOT_A} ${RUSTROOT_RS} \
		${EXTERN_ARGS} --sysroot=$(PWD)/${RUST_SYSROOT}
