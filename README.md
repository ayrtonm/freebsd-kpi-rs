# FreeBSD Rust KPI

This repo contains the KPI crate (i.e. library) and makefiles for using rust in the FreeBSD kernel.
It's currently used for the out-of-tree [virtio sound driver](https://github.com/ayrtonm/freebsd-src/tree/virtio_snd)
and various [Apple Silicon drivers](https://github.com/ayrtonm/freebsd-src/tree/apple) and is
intended as an on-ramp for developers that want to try using rust in the kernel. It supports a
subset of the C KPI and is tested with amd64 and aarch64 on both [src](https://cgit.freebsd.org/src/)
tip-of-tree and the `releng/15.1` branch. To go straight to running demos see [docs/demos.md](docs/demos.md).

## Stability

Spending time writing code is obviously a risk if a developer can't rely on being able to build it
in the future. To allow eventually providing as much stability as the C KPI, the KPI crate only
depends on the rust [core library](https://doc.rust-lang.org/core/) which is part of the rust
[compiler repo](https://github.com/rust-lang/rust/). To avoid tying KPI stability to the wider rust
ecosystem using third-party code (e.g. from https://crates.io) is unsupported and adding support for
it will **not** be considered. The KPI crate also intentionally avoids unstable language features
and APIs. This means it is expected to work with any future rust compiler supporting the 2024
edition. The rust KPIs should not be considered stable yet since they are currently under
developement, but they are intended to provide similar levels of stability as the C KPI.

## Installing rust and build tools

Using rust requires `rustc`, [`bindgen`](https://rust-lang.github.io/rust-bindgen/) and `rustfmt`.
The compiler must be a ["nightly"](https://doc.rust-lang.org/book/appendix-07-nightly-rust.html)
release since the makefile builds [`core`](https://doc.rust-lang.org/core/) from source and uses
unstable flags for fixed-x18 and BTI on aarch64. This does not imply you need to update the compiler
in any particular cadence and developers can largely ignore rustc's 6 week release cycle.

Using `cargo` for kernel builds is not supported to avoid extraneous build system glue. `cargo` is
used for the KPI crate's userspace unit tests and may also be used for tests in other crates, but
the latter is not officially supported by this repo.

### Toolchain version

The minimum supported rust version (MSRV) is rustc 1.90.0-nightly-2025-07-23. The KPI crate avoids
unstable language features and APIs so using newer versions will also work. For the exact range of
rust compiler versions that are known to work check this repo's CI workflow file for
[userspace tests](.github/workflows/userspace_tests.yml).

When building the FreeBSD kernel on a Linux or MacOS host the suggested way of obtaining rust tools
is `rustup`. For an x86-64 FreeBSD host `pkg` is the easiest way to get these tools. Unfortunately
FreeBSD on aarch64 does not have `rust-nightly` available via `pkg` or `rustup` so building on those
hosts currently requires [building the compiler](https://rustc-dev-guide.rust-lang.org/building/how-to-build-and-run.html)
from source.

### Setup via `pkg`

On an x86-64 FreeBSD host run

```
pkg install rust-nightly
pkg install rust-bindgen-cli
```

This will install whatever version that week's `rust-nightly` package has. Note that libcore APIs
stabilized in version X might not be available on older rustc versions. For experimenting with rust
this is fine, but keep this in mind if you want others to reliably build your work without always
having to update to the newest `rust-nightly`. If in doubt the [libcore docs](https://doc.rust-lang.org/core/)
list the rustc versions at which each function was stabilized.

### Setup via `rustup`

First use [`rustup`](https://rustup.rs/) to install nightly `rustc` and `rustfmt`.

```
rustup default nightly-2025-07-23
cargo install bindgen-cli
```

This installs the nightly rustc from 2025-07-23 and sets it as the default toolchain.

## Set up the repos

First clone this repo into `sys/rust` under the FreeBSD src directory. Then clone the rust compiler
repo into `sys/rust/compiler` and checkout the corresponding commit for the compiler installed on
the host. The commit hash is found by running `rustc --version --verbose`. After that, initialize
the rust compiler repo's `library/stdarch` git submodule. If you forget to do this the makefile will
tell you what hash to checkout in the rust compiler repo.

Building for FreeBSD 15.1 is generally the easiest option since RELEASE can be more stable than
CURRENT. To do this checkout the `releng/15.1` branches in both src and this repo. Using this repo's
`releng/15.1` branch with src on FreeBSD 16 or vice-versa will give a compile-time error.

```
cd /path/to/src
git checkout releng/15.1
git clone https://github.com/ayrtonm/freebsd-kpi-rs sys/rust
git clone --depth 1 https://github.com/rust-lang/rust sys/rust/compiler

cd sys/rust
git checkout releng/15.1

cd /path/to/src/sys/rust/compiler
git fetch --depth 1 origin `rustc --version --verbose | grep commit-hash | awk '{print $2}'`
git checkout FETCH_HEAD
git submodule update --depth 1 --init library/stdarch

# If switching rustc versions it may be necessary to run the following between checking out
# different commits
# git submodule deinit -f library/stdarch/
```

Next FreeBSD src needs two patches to integrate the makefiles in this repo and support generating
make rules for .rs files with [`config(8)`](https://man.freebsd.org/cgi/man.cgi?config(8)).

```
cd /path/to/src
git apply sys/rust/patches/config_rust_support.diff
git apply sys/rust/patches/rust_mk_integration.diff
```

## Building a kernel

First build `kernel-toolchain` to make sure the build uses the updated `config(8)`.

```
make -j8 kernel-toolchain TARGET=amd64 TARGET_ARCH=amd64
```

To optionally verify the patched version was built run the updated version (located in the build
dir under `tmp/legacy/bin`) with `config -V`. This should show version 600019.

Then set environment variables for `RUSTC`, `RUSTFMT` and `BINDGEN` and run `buildkernel` normally.

```
export RUSTC=`which rustc`
export RUSTFMT=`which rustfmt`
export BINDGEN=`which bindgen`

# For a quicker test use KERNCONF=MINIMAL and add NO_MODULES=yes
make -j8 buildkernel KERNCONF=GENERIC TARGET=amd64 TARGET_ARCH=amd64
```

To build on a non-FreeBSD host define the same environment variables and run
`src/tools/build/make.py` as described in its
[wiki page](https://wiki.freebsd.org/BuildingOnNonFreeBSD). See this repo's CI workflow files for a
more concrete example of building on x86 Linux and cross-building for aarch64.

A successful build will produce the artifacts listed [here](docs/build_artifacts.md). See
[getting_started.md](docs/getting_started.md) to get started adding new rust code.
