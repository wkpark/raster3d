Summary: Raster3D photorealistic molecular graphics package
Name: Raster3D
Version: 2.6.3
Release: 1
Copyright: Source freely available, redistribution restricted
Source: Raster3D_2.6c.tar.gz
URL: http://www.bmsc.washington.edu/raster3d
Packager: Ethan A Merritt
Group: Graphics

%description
The Raster3D molecular graphics package consists of a core rendering program 
and a number of ancillary programs that produce input files for rendering 
from PDB (Protein Data Bank) files of atomic coordinates.  Raster3D can also 
render images composed using other programs such as Molscript or XtalView.
Raster3D uses a fast Z-buffer algorithm to produce high quality pixel images 
featuring one shadowing light source, additional non-shadowing light sources, 
specular highlighting, transparency, and Phong shaded surfaces.  Output is to
a pixel image with 24 bits of color information per pixel.
Raster3D does not depend on graphics hardware.

Program reference and requested citation: 
	Merritt & Bacon  (1997) Meth. Enzymol. 277, 505-524.

%build

%files
%doc README CHANGELOG BUGS R3D_manual.pdf VERSION

/usr/local/bin/avs2ps 
/usr/local/bin/balls 
/usr/local/bin/label3d
/usr/local/bin/rastep 
/usr/local/bin/render 
/usr/local/bin/ribbon 
/usr/local/bin/rings3d
/usr/local/bin/rods 
/usr/local/bin/normal3d
/usr/local/bin/stereo3d
/usr/local/man/manl/avs2ps.l
/usr/local/man/manl/balls.l
/usr/local/man/manl/label3d.l
/usr/local/man/manl/normal3d.l
/usr/local/man/manl/r3d_objects.l
/usr/local/man/manl/r3dtops.l
/usr/local/man/manl/rastep.l
/usr/local/man/manl/raster3d.l
/usr/local/man/manl/render.l
/usr/local/man/manl/ribbon.l
/usr/local/man/manl/rods.l
/usr/local/man/manl/stereo3d.l
/usr/local/share/Raster3D
/etc/profile.d/Raster3D.csh
/etc/profile.d/Raster3D.sh

%post
 
if [ -a /usr/lib/ImageMagick/modules/coders/delegates.mgk ] ;
then grep -q Raster3D /usr/lib/ImageMagick/modules/coders/delegates.mgk \
  || echo -e '# Raster3D 2.6 \nr3d=>\n	render -tiff %o < %i' \
  >> /usr/lib/ImageMagick/modules/coders/delegates.mgk ;
fi
if [ -a /usr/X11R6/share/ImageMagick/delegates.mgk ] ;
then grep -q Raster3D /usr/X11R6/share/ImageMagick/delegates.mgk \
  || echo -e '# Raster3D 2.6 \nr3d=>\n	render -tiff %o < %i' \
  >> /usr/X11R6/share/ImageMagick/delegates.mgk ;
fi
