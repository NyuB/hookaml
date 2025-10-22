.PHONY: default build fmt test test-promote

# If INSTALL_ROOT is in your PATH, so will be the installed executable
INSTALL_ROOT=~/bin
INSTALL_SUFFIX=""

default: fmt test
os:
	echo $(OS)

# Build the executable then copy it under INSTALL_ROOT
ifeq ($(OS), Windows_NT)
install-%.exe:
	dune build ./$*.exe
	copy _build/default/$*.exe $(INSTALL_ROOT)/$*$(INSTALL_SUFFIX).exe
else
install-%.exe:
	dune build ./$*.exe
	cp _build/default/$*.exe $(INSTALL_ROOT)/$*$(INSTALL_SUFFIX).exe
	# Make the installed file writable to allow future deletion or replacement
	chmod +w $(INSTALL_ROOT)/$*$(INSTALL_SUFFIX).exe
endif

build:
	dune build

# Format sources, formatting can be configured in .ocamlformat
fmt:
	-dune fmt

test:
	dune test

# Update expected test results with whatever the current output is, run this once you are ok with the current behaviour
test-promote:
	dune test --auto-promote
