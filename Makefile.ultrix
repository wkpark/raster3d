# Raster3D makefile for Ultrix
#
LDFLAGS =
CFLAGS  = -g
FFLAGS  = -O -static -w1
LIBS    =	
DEFINES =

RIBOBJS =	ribbon.o ribbon1.o modsubs.o
PROGS   =	setup rods ribbon render

all:	$(PROGS)

clean:
	rm -f *.o *.u

install: $(PROGS)
	mv $(PROGS) /usr/local/bin
	mv docs/*.l /usr/local/man/manl

tar:
	(cd ..; tar -cvvf raster3d.tar raster3d)
	compress  ../raster3d.tar

ribbon:	$(RIBOBJS)
	f77 -O $(RIBOBJS) -o ribbon

render:	render.f local.c
	f77 $(FFLAGS) -Olimit 2000 render.f local.c  $(LIBS) -o render $(LDFLAGS)

