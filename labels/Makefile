FC = f77 
BINDIR = /usr/local/bin

r3dtops:	r3dtops.f
		$(FC) -o r3dtops r3dtops.f

r3d_label:	r3d_label.c
		cc -o r3d_label -I/usr/src/glut-3.6/lib/glut/ r3d_label.c -lglut

install:	label3d r3dtops
		cp label3d $(BINDIR)/
		mv r3dtops $(BINDIR)/

clean:
		rm -rf *.o core 
		rm -rf r3dtops r3d_font
