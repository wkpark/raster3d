Summary: Raster3D photorealistic molecular graphics package
Name: Raster3D
Version: 2.6.5
Release: 2
%define  r3dver Raster3D_2.6e
Copyright: Source freely available, redistribution restricted
Source: %{r3dver}.tar.gz
URL: http://www.bmsc.washington.edu/raster3d
Packager: Ethan A Merritt
Group: Graphics
BuildRoot:%{_tmppath}/%{name}-%{version}-buildroot
%define _prefix /usr/local
%define _bindir /usr/local/bin
%define _mandir /usr/local/man
%define _datadir /usr/local/share

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

%prep
%setup -q -n %{r3dver}

%build
make clean
make linux-pgf77
make all FFLAGS='-O -Munroll -tp px'
mkdir -p $RPM_BUILD_ROOT%{_prefix}/bin
make install prefix=$RPM_BUILD_ROOT%{_prefix} \
             mandir=$RPM_BUILD_ROOT%{_mandir}/manl
mkdir -p $RPM_BUILD_ROOT/%{_sysconfdir}/profile.d
cp Raster3D.{csh,sh} $RPM_BUILD_ROOT/%{_sysconfdir}/profile.d/

%files
%doc README CHANGELOG BUGS doc/R3D_manual.pdf VERSION

%{_bindir}/avs2ps 
%{_bindir}/balls 
%{_bindir}/label3d
%{_bindir}/rastep 
%{_bindir}/render 
%{_bindir}/ribbon 
%{_bindir}/rings3d
%{_bindir}/rods 
%{_bindir}/normal3d
%{_bindir}/stereo3d
%{_mandir}/manl/avs2ps.l
%{_mandir}/manl/balls.l
%{_mandir}/manl/label3d.l
%{_mandir}/manl/normal3d.l
%{_mandir}/manl/r3d_objects.l
%{_mandir}/manl/r3dtops.l
%{_mandir}/manl/rastep.l
%{_mandir}/manl/raster3d.l
%{_mandir}/manl/render.l
%{_mandir}/manl/ribbon.l
%{_mandir}/manl/rods.l
%{_mandir}/manl/stereo3d.l
%{_datadir}/Raster3D
%{_sysconfdir}/profile.d/Raster3D.csh
%{_sysconfdir}/profile.d/Raster3D.sh

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
if [ -a /usr/lib/ImageMagick/delegates.mgk ] ;
then grep -q r3d /usr/lib/ImageMagick/delegates.mgk \
  || grep -q encode /usr/lib/ImageMagick/delegates.mgk \
  || echo -e '# Raster3D 2.6 \nr3d=>\n	render -tiff %o < %i' \
  >> /usr/lib/ImageMagick/delegates.mgk ;
fi

%changelog
* Fri May  3 2002 EAM
- modify for cross-platform build
* Fri Apr  3 2002 Won-kyu Park <wkpark@kldp.org>
- make raster3d.spec more compliant with conventions for building rpm

