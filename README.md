# FreeBSD Rust KPI

This repo contains the KPI crate (i.e. library) and makefiles for using rust in the FreeBSD kernel.
It's currently used for the out-of-tree
[virtio sound driver](https://github.com/ayrtonm/freebsd-src/tree/virtio_snd) and various
[Apple Silicon drivers](https://github.com/ayrtonm/freebsd-src/tree/apple) and is intended as an
on-ramp for developers that want to try using rust in the kernel. It supports a limited subset of
the C KPI and is tested with amd64 and aarch64 on src tip-of-tree.

Spending time writing code is a risk if a developer can't rely on being able to build it in the
future. To allow providing as much stability as the C KPI, the KPI crate only depends on the rust
[core library](https://doc.rust-lang.org/core/) which is part of the
[rust compiler repo](https://github.com/rust-lang/rust/). To avoid tying KPI stability to the wider
rust ecosystem using third-party code (e.g. from https://crates.io) is unsupported and adding
support for it will **not** be considered. The KPI crate also intentionally avoids unstable language
features and APIs. This means it is expected to work with any future rust compiler supporting the
2024 edition.

Using `cargo` for kernel builds is also not supported to avoid parallel build processes. `cargo` is
used for the KPI crate's userspace unit tests and may also be used for tests in other crates, but
the latter is not officially supported by this repo.

## Installing rust and build tools

Using rust requires `rustc`, [`bindgen`](https://rust-lang.github.io/rust-bindgen/) and `rustfmt`.
The compiler must be a ["nightly"](https://doc.rust-lang.org/book/appendix-07-nightly-rust.html)
release since the makefile builds [`core`](https://doc.rust-lang.org/core/) from source and uses
unstable flags for fixed-x18 and BTI on aarch64. The KPI crate avoids unstable language features and
APIs though so the exact version doesn't matter.

When building the FreeBSD kernel on a Linux or MacOS host the suggested way of obtaining rust tools
is `rustup`. For an x86-64 FreeBSD host `pkg` is the easiest way to get these tools. Building on
aarch64 FreeBSD hosts requires buildings the compiler from source.

### Setup via `pkg`

On an x86-64 FreeBSD host run

```
pkg install rust-nightly
pkg install rust-bindgen-cli
```

### Setup via `rustup`

First use [`rustup`](https://rustup.rs/) to install nightly `rustc` and `rustfmt`.

```
rustup install nightly
cargo install bindgen-cli
```

## Set up the repos

First clone this repo into `sys/rust` under the FreeBSD src directory. Then clone the rust compiler
repo into `sys/rust/compiler` and checkout the corresponding commit for the compiler installed on
the host. The commit hash is found by running `rustc --version --verbose`. After that, initialize
the rust compiler repo's `library/stdarch` git submodule.

```
cd /path/to/src
git clone https://github/ayrtonm/freebsd-kpi-rs sys/rust
git clone --depth 1 https://github/rust-lang/rust sys/rust/compiler

cd sys/rust/compiler
git fetch --depth 1 origin `rustc --version --verbose | grep commit-hash | awk '{print $2}'`
git checkout FETCH_HEAD
git submodule update --depth 1 --init library/stdarch
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

Optionally, to verify the patched version was built run the updated version (located in the build
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
more concrete example of building on Linux.

A successful build will produce the artifacts listed [here](docs/build_artifacts.md). See
[getting_started.md](docs/getting_started.md) for notes on adding new rust code. For notes on
creating safer bindings for existing C interfaces see the
[creating_bindings.md](docs/creating_bindings.md) doc. The KPI documentation is available at
https://ayrtonm.github.io/freebsd-kpi-rs for aarch64 and
https://ayrtonm.github.io/freebsd-kpi-rs/x86_64-unknown-linux-gnu-doc/kpi for x86-64.
