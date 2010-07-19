
CC=ghc
CFLAGS=-lglut

skel: skel.hs Vector.hs
	$(CC) --make skel.hs $(CFLAGS)
clean:
	rm -f skel *.o *.hi
