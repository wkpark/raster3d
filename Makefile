#
# Raster3D makefile for Ultrix
#
LDFLAGS =
CFLAGS  = -O
FFLAGS  = -O -static -w1
#
# If you choose to build Raster3D with support for direct output of
# TIFF image files, then add -DTIFF_SUPPORT to the DEFINES statement.
# You will also need to add -ltiff to the LIBS definition.
 LIBS    =	-ltiff
 DEFINES =	-DTIFF_SUPPORT
#LIBS    =
#DEFINES =
INCLUDES=	-I/usr/local/include/ -I/usr/local/include/tiff

RIBOBJS =	ribbon.o ribbon1.o modsubs.o
PROGS   =	balls normal3d rods ribbon render setup avs2ps

all:	$(PROGS)

clean:
	rm -f *.o *.u
	rm -f $(PROGS)

install: $(PROGS)
	mv $(PROGS) /usr/local/bin
	cp doc/*.l /usr/local/man/manl

tar:
	(cd ..; tar -cvvf raster3d.tar raster3d)
	compress  ../raster3d.tar

ribbon:	$(RIBOBJS)
	f77 -O $(RIBOBJS) -o ribbon

local.o: local.c
	cc $(CFLAGS) $(INCLUDES) $(DEFINES) -c local.c

render:	render.f local.o
	f77 $(FFLAGS) -Olimit 2000 render.f local.o  \
	$(LIBS) -o render $(LDFLAGS)

avs2ps:	avs2ps.c
	cc -O avs2ps.c -o avs2ps -lm

setup:	balls
	ln -s balls setup
