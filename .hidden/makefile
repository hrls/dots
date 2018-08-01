dd:
	docker build --tag dd --file debian.dockerfile .

gg:
	docker build --tag gg --file gentoo.dockerfile .

aa:
	docker build 				     \
		--file alpine.dockerfile     \
		--build-arg with_haskell=no  \
		--build-arg with_rust=yes    \
		--tag aa  .


all: aa dd gg
