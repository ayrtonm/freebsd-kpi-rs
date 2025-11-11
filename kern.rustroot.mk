LDFLAGS += -Wl,--whole-archive

${RUSTROOT_A}: ${RUSTROOT_RS} ${RUSTROOT_DEP} ${RUST_CORE} ${RUST_FAKE_BUILTINS} ${RUST_KPI}
	${RUSTC} ${RUSTFLAGS} --crate-type staticlib -o ${RUSTROOT_A} ${RUSTROOT_RS} \
		--sysroot=$(PWD)/${RUST_SYSROOT} ${RLIBS:C/.rlib//:C/^lib//:C/.*/--extern \0=lib\0.rlib/} \
		--extern kpi=${RUST_KPI} -L.
