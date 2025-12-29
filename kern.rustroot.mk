_LDFLAGS += --whole-archive

RLIBS += ${RUST_KPI}

.for rlib in ${RLIBS}
.-include ".depend.${rlib}"
.endfor

${RUSTROOT_RS}: ${RUST_MAKEFILE} ${RLIBS}
	printf "#![no_std]\n${RLIBS:S/.rlib/;/:S/lib/extern crate /}" > ${RUSTROOT_RS}

${RUSTROOT_A}: ${RUSTROOT_RS} ${RUST_CORE} ${RUST_FAKE_BUILTINS} ${RLIBS}
	${RUSTC} ${RUSTFLAGS} --crate-type staticlib -o ${RUSTROOT_A} ${RUSTROOT_RS} \
		--sysroot=$(PWD)/${RUST_SYSROOT} ${RLIBS:C/.rlib//:C/^lib//:C/.*/--extern \0=lib\0.rlib/} \
		-L.
