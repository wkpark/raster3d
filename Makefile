#
# Makefile.incl is initially empty
# It gets initialized by "Make OS"
#
default: all

include Makefile.incl
include VERSION

LIBS	= $(LIBDIRS) $(CLIBS) $(TLIBS) $(GDLIBS) $(SLIBS)
DEFINES	= $(GDEFS) $(IDEFS) $(TDEFS) $(GDDEFS) $(SDEFS) $(OSDEFS)

FLAGS	= $(INCDIRS) $(DEFINES)

PROGS   = avs2ps balls rastep render ribbon rings3d rods normal3d
SCRIPTS = stereo3d worms

clean:	
	rm -f *.o $(PROGS) core *~
	rm -f render_small render_small.f parse_small.f qinp_small.f parameters_small.incl
	rm -f lists.mod
	$(MAKE) -C examples clean

distclean: clean
	rm -f Makefile.incl 
	touch Makefile.incl
	if [ -e aix-patch ] ; then rm -f aix-patch; fi
	exit 0

help:
	@if test "$(OS)" ; \
	then \
		echo "Making Raster3D $(VERSION) for $(OS)" ; \
	else \
	echo "" ; \
	echo "Please start by typing '"make OS"', where OS is one of" ; \
	echo "	linux linux-pgf77 linux-ifort dec osx-intel osx-fink sun sun-forte irix5 irix6 cygwin aix hpux" ; \
	echo "If your OS is not shown but you have gfortran installed, try" ; \
	echo "	make linux" ; \
	echo "" ; \
	echo "If all goes well, you can now type 'make' to build the programs" ; \
	echo "followed by 'make install' (probably as root) to install them" ;\
	exit 1 ; \
	fi

#
# OS-specific initializations
#

linux:	
	@cp Makefile.template Makefile.incl
	@echo OS = linux                      >> Makefile.incl
	@echo CC = gcc                        >> Makefile.incl
	@echo CFLAGS = -g -Wall -Dgfortran    >> Makefile.incl
	@echo FC = gfortran                   >> Makefile.incl
	@echo FFLAGS = -g -w -O3 -Wtabs -ffixed-line-length-132 >> Makefile.incl
	@echo RM = /bin/rm -f                 >> Makefile.incl
	@echo OSDEFS =  -DLINUX -DNETWORKBYTEORDER       >> Makefile.incl
	@echo include Makefile.package        >> Makefile.incl
	@echo                                 >> Makefile.incl
	@echo qinp.o: qinp.f                  >> Makefile.incl
	@echo "	\$$(FC) -g -O0 -Wall -Wtabs -c -o qinp.o qinp.f" >> Makefile.incl
	@echo                                 >> Makefile.incl

linux-ifort:
	@cp Makefile.template Makefile.incl
	@echo OS = linux                      >> Makefile.incl
	@echo CC = gcc                        >> Makefile.incl
	@echo CFLAGS = -g -Wall               >> Makefile.incl
	@echo FC = ifort                      >> Makefile.incl
	@echo FFLAGS = -g -w -O3 -132 -static-intel >> Makefile.incl
	@echo RM = /bin/rm -f                 >> Makefile.incl
	@echo OSDEFS =  -DLINUX -DNETWORKBYTEORDER       >> Makefile.incl
	@echo include Makefile.package        >> Makefile.incl

linux-pgf77:
	@cp Makefile.template Makefile.incl
	@echo OS = linux-pgf77                >> Makefile.incl
	@echo CC = gcc                        >> Makefile.incl
	@echo CFLAGS = -g -m486 -w            >> Makefile.incl
	@echo FC = pgf77                      >> Makefile.incl
	@echo FFLAGS = -O -Munroll            >> Makefile.incl
	@echo RM = /bin/rm -f                 >> Makefile.incl
	@echo OSDEFS = -DLINUX -DNETWORKBYTEORDER        >> Makefile.incl
	@echo include Makefile.package        >> Makefile.incl

osx-intel:	
	@cp Makefile.template Makefile.incl
	@echo OS = osx                        >> Makefile.incl
	@echo CC = icc                        >> Makefile.incl
	@echo CFLAGS = -g -Wall               >> Makefile.incl
	@echo FC = ifort                      >> Makefile.incl
	@echo FFLAGS = -g -w -O3 -Wtabs -132  >> Makefile.incl
	@echo RM = /bin/rm -f                 >> Makefile.incl
	@echo OSDEFS =  -DOSX -DNETWORKBYTEORDER       >> Makefile.incl
	@echo include Makefile.package        >> Makefile.incl
	@echo                                 >> Makefile.incl
	@echo qinp.o: qinp.f                  >> Makefile.incl
	@echo "	\$$(FC) -g -O0 -w -Wtabs -132 -c -o qinp.o qinp.f" >> Makefile.incl
	@echo                                 >> Makefile.incl

osx-fink:	
	@cp Makefile.template Makefile.incl
	@echo .NOTPARALLEL:                   >> Makefile.incl
	@echo                                 >> Makefile.incl
	@echo OS = osx                        >> Makefile.incl
	@echo CC = /sw/bin/gcc-4              >> Makefile.incl
	@echo INCDIRS = -I/sw/include         >> Makefile.incl
	@echo LIBDIRS = -L/sw/lib             >> Makefile.incl
	@echo CFLAGS = -g -Wall -Dgfortran    >> Makefile.incl
	@echo FC = /sw/bin/gfortran           >> Makefile.incl
	@echo FFLAGS = -g -w -O3 -Wtabs -ffixed-line-length-132 >> Makefile.incl
	@echo RM = /bin/rm -f                 >> Makefile.incl
	@echo OSDEFS =  -DOSX -DNETWORKBYTEORDER       >> Makefile.incl
	@echo include Makefile.package        >> Makefile.incl
	@echo                                 >> Makefile.incl
	@echo qinp.o: qinp.f                  >> Makefile.incl
	@echo "	\$$(FC) -g -O0 -Wall -Wtabs -c -o qinp.o qinp.f" >> Makefile.incl
	@echo                                 >> Makefile.incl

irix5:	
	@cp Makefile.template Makefile.incl
	@echo OS=irix5 >> Makefile.incl
	@echo SHELL=/bin/sh >> Makefile.incl
	@echo CC = cc                         >> Makefile.incl
	@echo CFLAGS = -g -w                  >> Makefile.incl
	@echo FC = f77                        >> Makefile.incl
	@echo FFLAGS = -O -Olimit 4500        >> Makefile.incl
	@echo RM = /bin/rm -f                 >> Makefile.incl

irix6:	
	@cp Makefile.template Makefile.incl
	@echo OS=irix6 >> Makefile.incl
	@echo SHELL=/bin/sh >> Makefile.incl
	@echo CC = cc -n32                    >> Makefile.incl
	@echo CFLAGS = -g -w                  >> Makefile.incl
	@echo FC = f77                        >> Makefile.incl
	@echo FFLAGS = -O -n32 -OPT:Olimit=4500 >> Makefile.incl
	@echo LDFLAGS = -L/usr/lib32          >> Makefile.incl
	@echo RM = /bin/rm -f                 >> Makefile.incl

dec:	
	@cp Makefile.template Makefile.incl
	@echo OS=dec >> Makefile.incl
	@echo CC = cc                         >> Makefile.incl
	@echo CFLAGS = -O -w                  >> Makefile.incl
	@echo FC = f77                        >> Makefile.incl
	@echo FFLAGS = -O -static -fast -r8   >> Makefile.incl
	@echo RM = /bin/rm -f                 >> Makefile.incl
	@echo OSDEFS = -DNETWORKBYTEORDER     >> Makefile.incl
	@echo include Makefile.package        >> Makefile.incl

aix:	aix-patch 
	@cp Makefile.template Makefile.incl
	@echo OS=aix >> Makefile.incl
	@echo CC = cc                         >> Makefile.incl
	@echo CFLAGS = -g                     >> Makefile.incl
	@echo FC = xlf -qqcount -w            >> Makefile.incl
	@echo FFLAGS = -O -qtkq_size=2000 -qintlog -qsave  >> Makefile.incl
	@echo LDFLAGS = -lz -bloadmap:loadmap.lis          >> Makefile.incl
	@echo RM = /bin/rm -f                 >> Makefile.incl
	@echo OSDEFS = -Dlocal_=local -Dungz_=ungz >> Makefile.incl
aix-patch: 
	patch render.f < render_patch.aix
	mv render.f.orig render.f.bak
	@touch aix-patch

sun:	
	@cp Makefile.template Makefile.incl
	@echo OS = sun                        >> Makefile.incl
	@echo CC = gcc                        >> Makefile.incl
	@echo CFLAGS = -O                     >> Makefile.incl
	@echo FC = g77                        >> Makefile.incl
	@echo FFLAGS = -O                     >> Makefile.incl
	@echo RM = /bin/rm -f                 >> Makefile.incl
	@echo LDFLAGS = -L/usr/local/lib/gcc-lib/sparc-sun-sunos4.1.3_U1/2.6.2 -lgcc >> Makefile.incl

sun-forte:	
	@cp Makefile.template Makefile.incl
	@echo OS = sun                        >> Makefile.incl
	@echo CC = cc                         >> Makefile.incl
	@echo CFLAGS = -fast -`fpversion -foption`       >> Makefile.incl
	@echo FC = f77                        >> Makefile.incl
	@echo FFLAGS = \${CFLAGS}               >> Makefile.incl
	@echo RM = /bin/rm -f                 >> Makefile.incl


#
# These source files are dependent on parameters.incl
#
FINCLS = render.o parse.o quadric.o normal3d.o
$(FINCLS):	parameters.incl

#
# Actually build the programs
#

all:	help $(PROGS)

avs2ps:	avs2ps.c
	$(CC) $(CFLAGS) $(FLAGS) $(LDFLAGS) -o avs2ps avs2ps.c -lm

balls:	balls.f
	$(FC) $(FFLAGS) $(LDFLAGS) -o balls balls.f 

local.o:	Makefile.incl local.c
	$(CC) $(CFLAGS) $(FLAGS) -c local.c

ungz.o:	Makefile.incl ungz.c
	$(CC) $(CFLAGS) $(GDEFS) $(OSDEFS) -c ungz.c

ribbon:	ribbon.f ribbon1.f modsubs.f
	$(FC) $(FFLAGS) $(LDFLAGS) \
	ribbon.f ribbon1.f modsubs.f -o ribbon

rastep:	rastep.f quadric.o suv.o
	$(FC) $(FFLAGS) \
	rastep.f quadric.o suv.o $(LDFLAGS) \
	-o rastep 

render:	render.o local.o quadric.o parse.o r3dtogd.o ungz.o qinp.o
	$(FC) $(FFLAGS) \
	render.o local.o quadric.o parse.o r3dtogd.o ungz.o \
	qinp.o \
	$(LIBS) $(LDFLAGS) \
	-o render

normal3d:	normal3d.o quadric.o qinp.o ungz.o parameters.incl
	$(FC) $(FFLAGS) \
	normal3d.o quadric.o ungz.o qinp.o $(LDFLAGS) \
	-o normal3d

stereo3d:

#
# Install
#
install:	all
	if [ ! -e $(prefix)  ] ; then mkdir -p $(prefix) ; fi
	if [ ! -e $(bindir)  ] ; then mkdir -p $(bindir) ; fi
	chmod 755 $(PROGS);   cp $(PROGS) $(bindir)
	chmod 755 $(SCRIPTS); cp $(SCRIPTS) $(bindir)
	if [ ! -e $(datadir) ] ; then mkdir -p $(datadir) ; fi
	cp materials/* $(datadir)
	if [ ! -e $(mandir)  ] ; then mkdir -p $(mandir) ; fi
	cp doc/*.l $(mandir)
	if [ ! -e $(htmldir) ] ; then mkdir -p $(htmldir) ; fi
	cp html/* $(htmldir)
	if [ ! -e $(examdir) ] ; then mkdir -p $(examdir) ; fi
	if [ ! -e $(examdir)/msms ] ; then mkdir -p $(examdir)/msms ; fi
	cp -R examples/* $(examdir)
	@echo ""
	@echo "	********************************************"
	@echo "	* The examples subdirectory contains input *"
	@echo "	* scripts to verify your installation and  *"
	@echo "	* to serve as examples of use.             *"
	@echo "	********************************************"

tests: all render_small
	$(MAKE) -C examples
	$(MAKE) -j1 -C examples compare

example1.png: render_small
	./render_small

render_small: render_small.o local.o quadric.o parse_small.o r3dtogd.o ungz.o qinp_small.o
	$(FC) $(FFLAGS) $^ $(LIBS) $(LDFLAGS) -o $@

.SUFFIXES: .incl
SMALL=10
parameters_small.incl: parameters.incl
	sed 's/PARAMETER *(MAXOBJ *=.*)/PARAMETER (MAXOBJ=$(SMALL))/;' $< \
	  | sed 's/PARAMETER *(MAXDET *=.*)/PARAMETER (MAXDET=$(SMALL),MAXSDT=$(SMALL))/;' \
	  | sed 's/PARAMETER *(MAXSHR *=.*)/PARAMETER (MAXSHR=$(SMALL),MAXSSL=$(SMALL))/;' \
	  > $@

render_small.o parse_small.o qinp_small.o: parameters_small.incl

render_small.f parse_small.f qinp_small.f: %_small.f: %.f
	sed 's/parameters.incl/parameters_small.incl/;' $< >$@

