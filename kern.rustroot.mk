_LDFLAGS += --whole-archive

${RUSTROOT_RS}: ${RUST_MAKEFILE} ${RLIBS}
	printf "#![no_std]\nextern crate kpi;\n${RLIBS:S/.rlib/;/:S/lib/extern crate /}" > ${RUSTROOT_RS}

${RUSTROOT_A}: ${RUSTROOT_RS} ${RUST_CORE} ${RUST_FAKE_BUILTINS} ${RUST_KPI} ${RLIBS}
	${RUSTC} ${RUSTFLAGS} --crate-type staticlib -o ${RUSTROOT_A} ${RUSTROOT_RS} \
		--sysroot=$(PWD)/${RUST_SYSROOT} ${RLIBS:C/.rlib//:C/^lib//:C/.*/--extern \0=lib\0.rlib/} \
		--extern kpi=${RUST_KPI} -L.
