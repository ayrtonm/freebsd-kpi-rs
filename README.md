# WIP Rust bindings for FreeBSD KPI

This requires build system changes in src from [this branch](https://github.com/ayrton/freebsd-src/tree/m2).
```
# Copy this repo to src/sys/rust
git clone git@github.com:ayrtonm/freebsd-kpi-rs /path/to/src/sys/rust

# To cross-build on linux or macOS
XCC=clang \
XCXX=clang++ \
XCPP=clang-cpp \
XLD=/usr/bin/ld.lld \
RUSTC=/usr/bin/rustc \
BINDGEN=/usr/bin/bindgen \
RUSTFMT=/usr/bin/rustfmt \
MAKEOBJDIRPREFIX=/path/to/obj/directory \
./path/to/src/tools/build/make.py -j8 buildkernel \
    TARGET=arm64 \
    TARGET_ARCH=aarch64 \
    KERNCONF=APPLE \
    -DNO_CLEAN
```
