SHELL = /bin/zsh

OS := $(shell uname -s)

ifeq ($(OS), Linux)
	SYS_UPD := apt-get
endif
ifeq ($(OS), Darwin)
	SYS_UPD := brew
endif

default: update

update: \
	update-macos \
	update-rust \
	gc

update-os:
	$(SYS_UPD) update

update-macos:
	brew update && brew outdated

macos-postupdate:
	$(shell brew --prefix)/opt/fzf/install \
		--xdg --key-bindings --completion \
		--no-update-rc --no-bash --no-fish

update-rust:
	rustup update; true

update-haskell:
	whence -p cabal && cabal new-update || cabal update

update-ruby:
	gem update
	gem cleanup

gc:
	fd --hidden --no-ignore --type file --fixed-strings .DS_Store --exec rm {}
	rm -fv \
		.swp \
		.tig_history \
		.sh_history \
		.python_history # TODO: hide history files in ~/.local/var


docker-machine:
	docker-machine create default \
		--virtualbox-cpu-count "6" \
		--virtualbox-memory "8192" \
		--virtualbox-disk-size "30000"
