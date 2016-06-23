all: rts-loader libmain.so

GHC_LIBDIR := $(shell ghc --print-libdir )
GHC_VERSION := $(shell ghc --numeric-version)

DEFINES = -D__GLASGOW_HASKELL__=$(shell echo $(GHC_VERSION) | sed -r 's/([0-9]+)\.([0-9])\.([0-9]+)/\10\2/;s/([0-9]+)\.([0-9]{2})\.([0-9]+)/\1\2/')

rts-loader: rts-loader.c
	gcc -std=gnu99 -Wall -g -I$(GHC_LIBDIR)/include $(DEFINES) $< -ldl -o $@

libmain.so: Main.hs
	ghc -dynamic -shared -fPIC -no-hs-main $< -o $@

clean:
	rm -f *.o *.hi rts-loader libmain.so
