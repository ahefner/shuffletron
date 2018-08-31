SBCL?=sbcl
CCL?=ccl
CCL64?=ccl64

# Note: If you edit the install prefix, you may also have to edit the
# 'shuffletron' wrapper script so it can find its libraries.

PREFIX=/usr/local

# I preferred to build it with --no-userinit so as not to pull in
# unspecified random junk, but this is the easiest way to make it work
# with Quicklisp.

# SBCLFLAGS=--noinform --no-userinit

SBCLFLAGS?=--noinform

export LD_LIBRARY_PATH=$(CURDIR)/libs/

all:	shuffletron-bin

tidy:
	rm -f *~ *.fasl *.lx*fsl \#*\# src/*~ src/*.fasl src/*.lx*fsl src/\#*\#

clean: tidy
	rm -f shuffletron-bin shuffletron-ccl shuffletron-ccl64
	rm -f libs/gen*

install:
	install -m 0755 shuffletron shuffletron-bin $(PREFIX)/bin
	mkdir -p $(PREFIX)/lib/shuffletron
	install -m 0755 libs/*.so* $(PREFIX)/lib/shuffletron

.PHONY: install all clean distclean

shuffletron-bin: build-sbcl.lisp src/*.lisp
	$(SBCL) $(SBCLFLAGS) --disable-debugger \
	        --eval "(require :asdf)" \
	        --eval "(load \"build-sbcl.lisp\")"

shuffletron-ccl64: shuffletron.lisp build-ccl.lisp
	$(CCL64) -n -e "(require :asdf)" \
	            -e "(load \"build-ccl.lisp\")"

shuffletron-ccl: shuffletron.lisp build-ccl.lisp
	$(CCL) -n -e "(require :asdf)" \
	          -e "(load \"build-ccl.lisp\")"
