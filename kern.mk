#    $S/arm64/apple/apple_aic.rs \

RSFILES= \
    $S/rust/kpi/lib.rs \
    $S/arm64/apple/apple_mbox.rs \
    $S/arm64/apple/rtkit.rs \
    $S/arm64/apple/apple_rtkit.rs \


CRATES=
# Turn .rs filenames into crate names
.for _rsf in ${RSFILES}
# If the crate a lib.rs use the directory as the crate name
.if ${_rsf:T} == lib.rs
CRATES+= ${_rsf:S/\/lib.rs//:T}
# Otherwise use the filename as the crate name
.else
CRATES+= ${_rsf:T:S/.rs//}
.endif
.endfor

# TODO: Find a better way to handle multifile crates
KPI_CRATE= lib.rs bus.rs cell.rs ofw.rs device.rs allocator.rs intr.rs arm64.rs macros.rs tty.rs sync.rs taskq.rs panic.rs ffi.rs sleep.rs
KPI_CRATE_FILES= ${KPI_CRATE:S/^/$S\/rust\/kpi\//g}

# crate-independent build flags
RUSTFLAGS= --target=aarch64-unknown-none-softfloat --edition 2021

# the rustc invocation for rlibs
NORMAL_RS= ${RUSTC} ${RUSTFLAGS} -Cdebuginfo=full -Copt-level=3 --crate-type rlib -o ${.TARGET}

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
    --blocklist-item 'pcb'

# doesn't really depend on the .o that's only to get bindgen to depend on changes to headers
bindings.rs: $S/rust/bindings.c bindings.o
	${BINDGEN} ${BINDGENFLAGS} $S/rust/bindings.c -- ${CFLAGS} -DBINDGEN > bindings.rs

# TODO: OUT_DIR env var used to include! bindings.rs in the idiomatic way. Maybe there's an existing
# env var to anchor the path
libkpi.rlib: ${KPI_CRATE_FILES} bindings.rs ${MFILES:T:S/.m$/.rs/g}
	OUT_DIR=$(PWD) ${NORMAL_RS} $S/rust/kpi/lib.rs

.for _rsf in ${RSFILES:N$S/rust/kpi/lib.rs}
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
	${RUSTC} ${RUSTFLAGS} --crate-type staticlib -o ${RUSTROOT_A} ${RUSTROOT_RS} ${EXTERN_ARGS}
