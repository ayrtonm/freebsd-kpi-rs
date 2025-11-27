# FreeBSD KPI rust bindings

This repo contains bindings and makefiles for using rust in the FreeBSD kernel. It intends to
provide an on-ramp for developers that want to try using rust for new kernel code. It's currently
used out-of-tree for the
[virtio sound driver](https://github.com/ayrtonm/freebsd-src/tree/virtio_snd) and various
[Apple Silicon drivers](https://github.com/ayrtonm/freebsd-src/tree/apple).

## Why?

The goal of this experiment is to allow developers to use mostly safe rust for new code to avoid the
silly issues that are so common in C. This includes things like encoding existing ownership
semantics for pointers in the type system and avoiding data races. When the language helps catch
issues like these developers can focus on implementing the functionality that they actually care
about.

## Stability and non-goals

Spending time writing code is a risk if a developer can't rely on being able to build it in the
future. To enable maximal stability once the KPI bindings are in suitable shape, this repo only
depends on the rust [`core` library](https://doc.rust-lang.org/core/). This library is provided as
part of the rust compiler repo and cannot dynamically allocate memory. The KPI crate will only
allocate using malloc(9) if a function explicitly takes both

It does not depend on any
third-party code from the broader rust ecosystem (e.g. crates.io) and adding support for using cargo
in kernel builds will not be considered.



The bindings are intended for use with FreeBSD src tip of tree. The FreeBSD KPI is a moving target
so for a commit known to produce working builds see
[this branch](https://github.com/ayrtonm/freebsd-src/tree/main).

## Getting started


They are intended for use with the latest version of FreeBSD and rust 2024 edition. Note that since
the FreeBSD KPI is a moving target this repository may be broken on the FreeBSD src tip of tree. For
a commit known to produce working builds see my
[virtio_snd branch](https://github.com/ayrtonm/freebsd-src/tree/virtio_snd).


This repo is not ready for general developer use yet since it is currently missing a lot of
functionality and proper documentation. It is primarily being used with my
[m2 branch](https://github.com/ayrtonm/freebsd-src/tree/m2) and only tested on builtin drivers for
Apple silicon. While the goal is to eventually support both FreeBSD Tier 1 platforms (ARM64 and
x86-64), this repo's makefile currently hardcodes ARM-specific things so instructions below assume
the kernel is being cross-compiled for ARM64.

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
rustup default nightly-2025-07-23
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

### Tool versions

Currently this repo is used with rustc 1.90 and bindgen 0.72. Since this repo's KPI crate does not
use unstable features it does not require nightly rustc and should build with any rust compiler
which supports Edition 2024.  Bindgen generates a .rs file from C headers and the exact version
should not significantly affect the machine code that .rs file compiles down to.

## Patch FreeBSD src build system

Next the FreeBSD src makefiles need small patches to integrate the makefile in this repo.

```
cd /path/to/freebsd/src

# This is only needed to target ARM64 since libcore.rlib obtained via rustup won't have BTI
git apply /path/to/this/repo/patches/aarch64_disable_bti.diff

git apply /path/to/this/repo/patches/rust_mk_integration.diff
```

The main thing the patches do is link the kernel object files with `rustroot.a` generated in
[kern.rust.mk](https://github.com/ayrtonm/freebsd-kpi-rs/blob/main/kern.rust.mk).

Note that the patches currently do not add `-Wl,--whole-archive` due to an issue with the
compiler_builtins rlib. Ideally it should be added to avoid pitfalls with symbols defined in both a
C object file and the static archive. In practical terms this means that currently if C code defines
a non-static global variable or function and an identically named variable in rust has the
`#[no_mangle]` attribute, the rust definition will be silently dropped when linking the kernel.

## Test the build

First this repo must be cloned into sys/rust under the FreeBSD src directory. Then after patching
the build system the kernel can be built by defining environment variables pointing to `rustc`,
`bindgen` and `rustfmt` and passing `buildkernel` to `make`.

```
cd /path/to/src
git clone https://github.com/ayrtonm/freebsd-kpi-rs sys/rust
git clone --depth 1 https://github.com/rust-lang/rust sys/rust/compiler
cd sys/rust/compiler
git fetch origin a7a1618e6c835f1f00940ad72203d05808209a0d
git checkout FETCH_HEAD
git submodule update --depth 1 --init library/stdarch
cd ../../..
RUSTC=`which rustc` \
RUSTFMT=`which rustfmt` \
BINDGEN=`which bindgen` \
make -j8 buildkernel \
	KERNCONF=GENERIC \
	TARGET=arm64 \
	TARGET_ARCH=aarch64
```

To build on a non-FreeBSD host the same environment variables with `src/tools/build/make.py` as
described in this [wiki page](https://wiki.freebsd.org/BuildingOnNonFreeBSD).

A successful build will produce `bindings.rs`, `libkpi.rlib`, `rustroot.rs` and `rustroot.a` in the
same directory as the kernel. For each crate added as described below there will also be a
`lib$CRATE_NAME.rlib` in the build directory. Rlibs are essentially static archives with additional
rust-specific metadata. This means that tools like readelf/objdump/nm will work as usual. To
demangle symbols these tools usually accept a `-C` flag.

## Adding a rust driver

TODO: clean up kern.rust.mk, get m2 branch building again then document this better

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
