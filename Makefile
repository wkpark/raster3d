#
# Makefile.incl is initially empty
# It gets initialized by "Make OS"
#
default: all

include Makefile.incl
include VERSION

LIBS	= $(LIBDIRS) $(CLIBS) $(TLIBS) $(JLIBS) $(SLIBS) $(PLIBS)
DEFINES	= $(GDEFS) $(IDEFS) $(TDEFS) $(JDEFS) $(SDEFS) $(PDEFS) $(OSDEFS)

FLAGS	= $(INCDIRS) $(DEFINES)

PROGS   = avs2ps balls rastep render ribbon rings3d rods normal3d
SCRIPTS = label3d stereo3d

clean:	
	rm -f *.o $(PROGS) core *~

distclean: clean
	rm -f Makefile.incl 
	touch Makefile.incl
	if [ -e render.f.bak ] ; then mv -f render.f.bak render.f; fi
	if [ -e normal3d.f.bak ] ; then mv -f normal3d.f.bak normal3d.f; fi
	if [ -e rastep.f.bak ] ; then mv -f rastep.f.bak rastep.f; fi
	if [ -e aix-patch ] ; then rm -f aix-patch; fi
	exit 0

help:
	@if test "$(OS)" ; \
	then \
		echo "Making Raster3D $(VERSION) for $(OS)" ; \
	else \
	echo "" ; \
	echo "Please start by typing '"make OS"', where OS is one of" ; \
	echo "	linux linux-pgf77 dec sun irix5 irix6 cygwin aix hpux" ; \
	echo "If your OS is not shown but you have g77/gcc installed, try" ; \
	echo "	make linux" ; \
	echo "" ; \
	echo "If all goes well, you can now type 'make' to build the programs" ; \
	echo "followed by 'make install' (probably as root) to install them" ;\
	exit 1 ; \
	fi

#
# OS-specific initializations
#
linux:	strip-for-g77
	@cp Makefile.template Makefile.incl
	@echo OS = linux                      >> Makefile.incl
	@echo CC = gcc                        >> Makefile.incl
	@echo CFLAGS = -g -m486               >> Makefile.incl
	@echo FC = g77                        >> Makefile.incl
	@echo FFLAGS = -g -O -w -malign-double>> Makefile.incl
	@echo RM = /bin/rm -f                 >> Makefile.incl
	@echo OSDEFS =  -DLINUX -DNETWORKBYTEORDER       >> Makefile.incl
	@echo include Makefile.package        >> Makefile.incl

linux-pgf77:
	@cp Makefile.template Makefile.incl
	@echo OS = linux                      >> Makefile.incl
	@echo CC = gcc                        >> Makefile.incl
	@echo CFLAGS = -g -m486               >> Makefile.incl
	@echo FC = pgf77                      >> Makefile.incl
	@echo FFLAGS = -O -Munroll            >> Makefile.incl
	@echo RM = /bin/rm -f                 >> Makefile.incl
	@echo OSDEFS = -DLINUX -DNETWORKBYTEORDER        >> Makefile.incl
	@echo include Makefile.package        >> Makefile.incl

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

aix:	aix-patch strip-for-g77
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

sun:	strip-for-g77
	@cp Makefile.template Makefile.incl
	@echo OS = sun                        >> Makefile.incl
	@echo CC = gcc                        >> Makefile.incl
	@echo CFLAGS = -O                     >> Makefile.incl
	@echo FC = g77                        >> Makefile.incl
	@echo FFLAGS = -O                     >> Makefile.incl
	@echo RM = /bin/rm -f                 >> Makefile.incl
	@echo LDFLAGS = -L/usr/local/lib/gcc-lib/sparc-sun-sunos4.1.3_U1/2.6.2 -lgcc >> Makefile.incl


strip-for-g77: render.f.bak normal3d.f.bak rastep.f.bak
render.f.bak:
	mv render.f render.f.bak
	egrep -v '(CARRIAGECONTROL|DISPOSE)' render.f.bak > render.f
normal3d.f.bak:
	mv normal3d.f normal3d.f.bak
	egrep -v '(CARRIAGECONTROL|DISPOSE)' normal3d.f.bak > normal3d.f
rastep.f.bak:
	mv rastep.f rastep.f.bak
	egrep -v '(CARRIAGECONTROL|DISPOSE)' rastep.f.bak > rastep.f

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
	$(FC) $(FFLAGS) -o balls balls.f 

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

render:	render.o local.o quadric.o parse.o r3dtops.o ungz.o
	$(FC) $(FFLAGS) \
	render.o local.o quadric.o parse.o r3dtops.o ungz.o \
	$(LDFLAGS) $(LIBS) \
	-o render

normal3d:	normal3d.o quadric.o ungz.o parameters.incl
	$(FC) $(FFLAGS) \
	normal3d.o quadric.o ungz.o $(LDFLAGS) \
	-o normal3d

stereo3d:

label3d:

#
# Install
#
install:	all
	chmod 755 $(PROGS);   cp $(PROGS) $(bindir)
	chmod 755 $(SCRIPTS); cp $(SCRIPTS) $(bindir)
	if [ ! -e $(datadir) ] ; then mkdirhier $(datadir) ; fi
	cp materials/* $(datadir)
	if [ ! -e $(mandir)  ] ; then mkdirhier $(mandir) ; fi
	cp doc/*.l $(mandir)
	if [ ! -e $(htmldir) ] ; then mkdirhier $(htmldir) ; fi
	cp html/* $(htmldir)
	if [ ! -e $(examdir) ] ; then mkdirhier $(examdir) ; fi
	if [ ! -e $(examdir)/msms ] ; then mkdirhier $(examdir)/msms ; fi
	cp -R examples/* $(examdir)
	@echo ""
	@echo "	********************************************"
	@echo "	* The examples subdirectory contains input *"
	@echo "	* scripts to verify your installation and  *"
	@echo "	* to serve as examples of use.             *"
	@echo "	********************************************"

