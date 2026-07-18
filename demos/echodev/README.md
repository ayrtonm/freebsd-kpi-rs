# Character Device Echo Demo

This is a demo based off of https://github.com/bsdjhb/cdev_tutorial. To build follow the
instructions in the README.md in this repo's root directory. That should let you set up a build
environment for building rust into the kernel and modules. Since the modules aren't in the normal
directory I run `buildkernel` with `LOCAL_MODULES_DIR=/path/to/sys/rust/demos` and
`LOCAL_MODULES="echodev"`. For examples on how to cross-compile look at this repo's
`.github/workflows`.
