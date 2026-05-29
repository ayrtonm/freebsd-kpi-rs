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

# Print the N largest stack frames in rustroot.a and error if any exceed the limit.
# Override STACKFRAMES_TOP to show more results; override STACKFRAMES_MAX to change the limit.
STACKFRAMES_TOP?=	20
STACKFRAMES_MAX?=	2048

stackframes: ${RUSTROOT_A}
	@${OBJDUMP:Uobjdump} -Cd ${RUSTROOT_A} | awk ' \
		/^[0-9a-f]+ <[^>]+>:/ { \
			f = $$0; gsub(/^[0-9a-f]+ <|>:$$/, "", f) \
		} \
		/sub\tsp, sp, #/ { \
			n = split($$0, a, "#"); s = a[n]; \
			gsub(/[[:space:]].*$$/, "", s); \
			print s, f \
		}' | \
	while read sz fn; do printf "%10d  %s\n" "$$sz" "$$fn"; done | \
	sort -rn | \
	awk -v top=${STACKFRAMES_TOP} -v max=${STACKFRAMES_MAX} ' \
		NR <= top { print } \
		$$1 > max && !err { bad_sz = $$1; bad_fn = $$2; err = 1 } \
		END { \
			if (err) { \
				printf "error: \"%s\" has a stack frame of %d bytes (limit: %d)\n", \
					bad_fn, bad_sz, max > "/dev/stderr"; \
				exit 1 \
			} \
		}'
