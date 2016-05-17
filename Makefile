all: rts-loader libmain.so

rts-loader: rts-loader.c
	gcc -g -I/usr/lib/ghc/include/ $< -ldl -o $@

libmain.so: Main.hs
	ghc -dynamic -shared -fPIC -no-hs-main $< -o $@

clean:
	rm -f *.o *.hi rts-loader libmain.so
