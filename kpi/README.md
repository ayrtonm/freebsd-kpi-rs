# FreeBSD KPI crate

This directory provides a way to build the KPI crate using cargo and a pre-generated bindings.rs.
This is only intended to be used for userspace unit tests for the bindings and generating
documentation. Please **do not** link any artifacts produced by cargo into any kernel and instead
use the makefile provided in the parent directory.

Note that the following instructions intentionally do not use the rust targets used by the makefile.
This simplifies the cargo invocation and rust toolchain components needed to run userspace unit
tests and regenerate documentation. Support for these optional steps is provided on a best-effort
basis and they may be less stable than the makefile which generates build artifacts that actually
get linked into the kernel. Obtaining cargo for a particular host is left as an exercise for the
reader.

## Run tests

To run tests run the following from this directory.

```
cargo test
```

## Browse documentation

Documentation for x86-64 is available at https://ayrtonm.github.io/freebsd-kpi-rs/

## Regenerate documentation

To generate docs for the host's native arch run the following and open
`kpi/target/doc/kpi/index.html`

```
cargo doc
```

To generate x86-64 docs on any host run the following and open
`kpi/target/x86_64-unknown-linux-gnu/doc/kpi/index.html`

```
cargo doc --target x86_64-unknown-linux-gnu
```

To generate arm64 docs on any host run the following and open
`kpi/target/aarch64-unknown-linux-gnu/doc/kpi/index.html`

```
cargo doc --target aarch64-unknown-linux-gnu
```
