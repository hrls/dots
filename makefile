OS := $(shell uname -s)

ifeq ($(OS), Linux)
	SYS_UPD := apt-get
endif
ifeq ($(OS), Darwin)
	SYS_UPD := brew
endif

default: update git_fetch

update: \
	update_os \
	update_rs \
	update_hs \
	update_rb \
	gc

update_os:
	$(SYS_UPD) update

update_rs:
	rustup update; true

update_hs:
	cabal new-update || cabal update

update_rb:
	# gem update && gem cleanup"

gc:
	rm -fv .DS_Store .swp \
		.tig_history \
		.sh_history \
		.python_history # TODO: hide history files in ~/.local/var


machine:
	docker-machine create default \
		--virtualbox-cpu-count "6" \
		--virtualbox-memory "8192" \
		--virtualbox-disk-size "30000"

git_fetch:
	git -C ~/src/iconic/core
	git -C ~/tmp/core
