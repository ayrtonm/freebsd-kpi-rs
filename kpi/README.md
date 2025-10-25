# FreeBSD KPI crate

This directory provides a way to build the KPI crate using cargo and a pre-generated bindings.rs.
This is only intended to be used for userspace unit tests for the bindings and generating
documentation. Please do not link any artifacts produced by cargo into any kernel and instead use
the makefile provided in the parent directory.
