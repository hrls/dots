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
	gc

update_os:
	$(SYS_UPD) update

update_rs:
	rustup update; true

update_hs:
	cabal new-update || cabal update

update_rb:
	gem update
	gem cleanup

gc:
	fd --hidden --no-ignore --type file --fixed-strings .DS_Store --exec rm {}
	rm -fv \
		.swp \
		.tig_history \
		.sh_history \
		.python_history # TODO: hide history files in ~/.local/var


machine:
	docker-machine create default \
		--virtualbox-cpu-count "6" \
		--virtualbox-memory "8192" \
		--virtualbox-disk-size "30000"
