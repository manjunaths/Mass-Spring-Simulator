
GHC=ghc
CFLAGS=-lglut
CC=gcc

quatrot: QuatRot.c
	$(CC) -o QuatRot.linux QuatRot.c -lglut -lGLU -lGL -lm


skel: skel.hs Vector.hs
	$(GHC) --make skel.hs  $(CFLAGS)

clean:
	rm -f skel *.o *.hi
