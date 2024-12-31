# FreeBSD KPI rust bindings

These are WIP bindings for writing FreeBSD kernel drivers in rust. They are intended for use with
the latest version of FreeBSD and rust 2021 Edition. Note that since the FreeBSD KPI is a moving
target this repository may be broken on the FreeBSD-src tip of tree. For a commit known to produce a
working build see my [mk-rs branch](https://github.com/ayrtonm/freebsd-src/tree/mk-rs). For the
latest use of the bindings see my [m2 branch](https://github.com/ayrtonm/freebsd-src/tree/m2). They
are initially intended for [FreeBSD Tier 1 platforms](https://www.freebsd.org/platforms/) although
currently only 64-bit ARM is supported and has been tested. The makefile in this repo also currently
hardcodes ARM-specific things so the x86-64 instructions below assume that the kernel is being
cross-compiled for ARM64.

## Installing rust and build tools

Using these bindings requires `rustc`, [`bindgen`](https://rust-lang.github.io/rust-bindgen/),
`rustfmt` and the [rust core library](https://doc.rust-lang.org/core/). When building the FreeBSD
kernel on a Linux or MacOS host the recommended way of obtaining rust tools is `rustup`. For an
x86-64 FreeBSD host `rustup` is the easiest way to get these tools. Building on an ARM64 FreeBSD
host currently requires building the compiler from source and the recommended approach is
[FreeBSD Ports](https://docs.freebsd.org/en/books/handbook/ports/#ports-using-installation-methods)
with a custom patch.

### Setup via `rustup`

First use [`rustup`](https://rustup.rs) to install `rustc` and `rustfmt`. This will install the
latest stable rustc for the host. To install the core library we need to add a
[cross-compilation target](https://rust-lang.github.io/rustup/cross-compilation.html) for the kernel
as follows

```
rustup target add aarch64-unknown-none-softfloat
```

### Setup via FreeBSD Ports

If the patch fails to apply cleanly check [review D48264](https://reviews.freebsd.org/D48264) for
the latest version.

```
git clone --depth 1 https://git.FreeBSD.org/ports.git /usr/ports
cd /usr/ports/lang/rust
git apply /path/to/this/repo/patches/ports_lang_rust.diff

# This step builds LLVM and rustc so it will take a while...
make install
```

### Installing bindgen

On a FreeBSD host run
```
pkg install rust-bindgen-cli
```

On Linux or MacOS after setting up rust run
```
cargo install bindgen-cli
```

Currently this repo is used with bindgen 0.71.1 though the exact version should not significantly
affect the generated source code.

## Patch FreeBSD src build system

Next the FreeBSD src makefiles need small patches to integrate the makefile in this repo.

```
cd /path/to/freebsd/src

# This is only needed to target ARM64 since libcore.rlib obtained via rustup won't have BTI
git apply /path/to/this/repo/patches/aarch64_disable_bti.diff

git apply /path/to/this/repo/patches/add_bindings_c.diff
git apply /path/to/this/repo/patches/rust_mk_integration.diff
```

The main thing the patches do is link the kernel object files with `rustroot.a` generated
[in kern.mk](https://github.com/ayrtonm/freebsd-kpi-rs/blob/main/kern.mk#L83).

Note that the patches currently do not add `-Wl,--whole-archive` due to an issue with the
compiler_builtins rlib. Ideally it should be added to avoid pitfalls with symbols defined in both a
C object file and the static archive. In practical terms this means that currently if C code defines
a non-static global variable or function and an identically named variable in rust has the
`#[no_mangle]` attribute, the rust definition will be silently dropped when linking the kernel.

## Adding a rust driver

TODO: clean up kern.mk, get m2 branch building again then document this better

main points:
- only single-file crates are supported
- add .rs file to `RSFILES` and at least kpi as crate dependency (no cylic crate deps)
- start .rs file with `#![no_std]`
- add `use kpi::driver;` and invoke `driver!` macro for driver crates
- or add `use kpi::prelude::*;` for non-driver crates
- cannot import anything from `std` or `alloc`. Only imports from `core` and `kpi`
- for allocations `kpi` will provide minimal versions of `Box`, `Vec`, `CString` and `Arc`
- allocation/reallocation should not happen without passing in `malloc_flags` and size
- rust KPI abstractions in flux but see
  [`apple_rtkit.rs`](https://github.com/ayrtonm/freebsd-src/blob/m2/sys/arm64/apple/apple_rtkit.rs)
  and
  [`apple_mbox.rs`](https://github.com/ayrtonm/freebsd-src/blob/m2/sys/arm64/apple/apple_mbox.rs)
  for the latest version
