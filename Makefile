SBCL=sbcl
CCL=ccl
CCL64=ccl64
PREFIX=/usr/local

export LD_LIBRARY_PATH=.

all:	shuffletron-bin

tidy:
	rm -f *~ *.fasl *.lx*fsl \#*\#

clean: tidy
	rm -f shuffletron-bin shuffletron-ccl shuffletron-ccl64
	rm -f *~ *.fasl *.lx*fsl

install:
	install -m 0755 shuffletron shuffletron-bin $(PREFIX)/bin
#	mkdir -p $(PREFIX)/lib/shuffletron
#	install -m 0755 libmixalot-mpg123.so.0 $(PREFIX)/lib/shuffletron

.PHONY: install all clean distclean

shuffletron-bin: shuffletron.lisp build-sbcl.lisp
	$(SBCL) --noinform --no-userinit --disable-debugger \
	        --eval "(require :asdf)" \
	        --eval "(load \"build-sbcl.lisp\")"

shuffletron-ccl64: shuffletron.lisp build-ccl.lisp
	$(CCL64) -n -e "(require :asdf)" \
	            -e "(load \"build-ccl.lisp\")"

shuffletron-ccl: shuffletron.lisp build-ccl.lisp
	$(CCL) -n -e "(require :asdf)" \
	          -e "(load \"build-ccl.lisp\")"