#
# Raster3D makefile for Alpha OSF/4.0
#          f77 compiler seems to be fine
#	   f90 compiler is broken above optimization level O1
#	       (you can see this in example6 - objects seen through transparent
#	        surface are not colored correctly)
#
LDFLAGS =
CFLAGS  = -O
#FC	= f90
#FFLAGS	= -O1 -fast -static -d_lines
FC	= f77
FFLAGS  = -O -fast -static -w1 -d_lines
#
# If you choose to build Raster3D with support for direct output of
# TIFF image files, then add -DTIFF_SUPPORT to the DEFINES statement.
# You will also need to add -ltiff to the LIBS definition.
# The TIFF_INVERT option is a work-around for broken programs
# (e.g. PhotoShop) that don't properly read the TIFF header.
#
 LIBS    =	-ltiff
 DEFINES =	-DTIFF_SUPPORT -DTIFF_INVERT
#LIBS    =
#DEFINES =

INCLUDES = -I/usr/local/include/

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
	$(FC) -O $(RIBOBJS) -o ribbon

local.o: local.c
	cc $(CFLAGS) $(INCLUDES) $(DEFINES) -c local.c

render:	render.f local.o
	$(FC) $(FFLAGS) render.f local.o  \
	$(LIBS) -o render $(LDFLAGS)

avs2ps:	avs2ps.c
	cc -O avs2ps.c -o avs2ps -lm

setup:	balls
	ln -s balls setup
