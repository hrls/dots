OS := $(shell uname -s)

ifeq ($(OS), Linux)
	SYS_UPD := apt-get
endif
ifeq ($(OS), Darwin)
	SYS_UPD := brew
endif

default: update

update: \
	update_os \
	update_rs \
	update_hs \
	update_rb \
	gc

update_os:
	$(SYS_UPD) update

update_rs:
	rustup update; exit 0

update_hs:
	cabal new-update || cabal update

update_rb:
	echo "gem update && gem cleanup"

gc:
	rm -fv .DS_Store .swp \
		.tig_history \
		.python_history # TODO: hide history files in ~/.local/var

machine:
	docker-machine create default \
		--virtualbox-cpu-count "4" \
		--virtualbox-memory "4096"
