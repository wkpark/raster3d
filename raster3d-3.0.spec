Summary: Raster3D photorealistic molecular graphics package
Name: Raster3D
Version: 3.0
Release: 2
%define  r3dver %{name}_%{version}-%{release}
License: Source freely available, redistribution restricted
Source: http://www.bmsc.washington.edu/raster3d/%{name}_%{version}-%{release}.tar.gz
URL: http://www.bmsc.washington.edu/raster3d
Packager: Ethan A Merritt
Group: Graphics
BuildRoot:%{_tmppath}/%{name}-%{version}-buildroot
BuildRequires: libgd-devel libtiff-devel
#
# You may or may not want the following definitions
%define _prefix /usr/local
%define _bindir  %{_prefix}/bin
%define _mandir  %{_prefix}/man
%define _datadir %{_prefix}/share

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

This version is dynamically linked.

Program reference and requested citation: 
	Merritt & Bacon  (1997) Meth. Enzymol. 277, 505-524.

%prep
%setup -q -n %{r3dver}

%build
make clean
if [ -x $PGI/linux86/bin/pgf77 ]; then
  make linux-pgf77
  make all FFLAGS='-O -Munroll -tp px' PLIBS='/usr/lib/libpng.a'
elif [ -x /usr/bin/gfortran ]; then
  make linux
  make all FFLAGS="-g -O3 -ffixed-line-length-132 -Wtabs -fno-range-check" CFLAGS="-Dgfortran $RPM_OPT_FLAGS"
else
  make linux
  make all FFLAGS="$RPM_OPT_FLAGS" CFLAGS="$RPM_OPT_FLAGS"
fi

# Changing R3D_LIB
grep -v R3D_LIB Raster3D.csh > Raster3D.csh.$$
echo "setenv R3D_LIB %{_datadir}/Raster3D/materials" >> Raster3D.csh.$$
mv Raster3D.csh.$$ Raster3D.csh
grep -v R3D_LIB Raster3D.sh > Raster3D.sh.$$
echo "export R3D_LIB=%{_datadir}/Raster3D/materials" >> Raster3D.sh.$$
mv Raster3D.sh.$$ Raster3D.sh

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT%{_datadir} 
mkdir -p $RPM_BUILD_ROOT%{_mandir}
mkdir -p $RPM_BUILD_ROOT%{_bindir}
make install prefix=$RPM_BUILD_ROOT%{_prefix} \
             datadir=$RPM_BUILD_ROOT%{_datadir}/Raster3D/materials \
             mandir=$RPM_BUILD_ROOT%{_mandir}/manl
mkdir -p $RPM_BUILD_ROOT/%{_sysconfdir}/profile.d
install -m755 Raster3D.{csh,sh} $RPM_BUILD_ROOT/%{_sysconfdir}/profile.d/

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr (-,root,root)
%doc README CHANGELOG BUGS doc/R3D_manual.pdf VERSION

%{_bindir}/avs2ps 
%{_bindir}/balls 
%{_bindir}/worms
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
MGK=
if   [ -a /usr/lib/ImageMagick/config/delegates.mgk ]; then
  MGK="/usr/lib/ImageMagick/config/delegates.mgk"
elif [ -a /usr/X11R6/share/ImageMagick/delegates.mgk ]; then
  MGK="/usr/X11R6/share/ImageMagick/delegates.mgk"
elif [ -a /usr/lib/ImageMagick/delegates.mgk ]; then
  MGK="/usr/lib/ImageMagick/delegates.mgk"
fi

OLD="# Raster3D 2.6 \nr3d=> \nrender -tiff %o < %i"
XML="  <delegate decode=\"r3d\" command='render -png \"%o\"<\"%i\"' />"
if [ -n "$MGK" ]; then
  if grep -q "xml" $MGK; then
    if ! grep -q "r3d" $MGK; then
      cp -f $MGK /tmp/delegates.bak.$$
      sed "/<\/delegatemap>/{x;s!^!$XML!;G;}" $MGK > /tmp/delegates.mgk.$$
      cp -f /tmp/delegates.mgk.$$ $MGK
      rm -f /tmp/delegates.mgk.$$
    fi
  else
    if grep -q "r3d" $MGK; then
      echo -e $OLD >> $MGK
    fi
  fi
fi

%postun
MGK=
if   [ -a /usr/lib/ImageMagick/config/delegates.mgk ]; then
  MGK="/usr/lib/ImageMagick/config/delegates.mgk"
elif [ -a /usr/X11R6/share/ImageMagick/delegates.mgk ]; then
  MGK="/usr/X11R6/share/ImageMagick/delegates.mgk"
elif [ -a /usr/lib/ImageMagick/delegates.mgk ]; then
  MGK="/usr/lib/ImageMagick/delegates.mgk"
fi

if [ -n "$MGK" ]; then
  grep -v r3d $MGK > /tmp/delegates.mgk.$$
  cp -f /tmp/delegates.mgk.$$ $MGK
  rm -f /tmp/delegates.mgk.$$  
fi


%changelog
* Thu Feb 03 2011 Ethan Merritt <merritt@u.washington.edu>
- release Raster3D-3.0-2 (dynamic array allocation; libgd support)
* Tue Dec 19 2010 Ethan Merritt <merritt@u.washington.edu>
- released Raster3D-3.0-1 (dynamic array allocation; libgd support)
* Tue Mar 16 2010 Christoph Champ <champc@u.washington.edu>
- released Raster3D-2.9-2 (dynamic)
* Fri Feb 12 2010 Christoph Champ <champc@u.washington.edu>
- released Raster3D-2.9-1 (static)
* Thu Jan 21 2010 Christoph Champ <champc@u.washington.edu>
- changed "mkdirhier" to "mkdir -p"
- added "-fno-range-check" to FFLAGS
* Fri Mar  7 2008 EAM
- More gfortran initialization problems
* Wed May  3 2006 EAM
- rastep was suffering from gfortran + uninitialized arrays
* Tue Apr 12 2006 EAM
- Update for 2.7d (gfortran, ImageMagick 6+)
* Thu Feb 12 2004 EAM
- rastep logical test of noerr fails in g77
* Fri Feb  6 2004 EAM
- modify label3d and stereo3d to handle change in ImageMagick output format
- built under Mandrake 9.2 against glibc.so.6
* Mon Oct 20 2003 EAM
- Add -background option to render; check for Uii=0 in rastep
* Thu Jun 19 2003 EAM
- Fix TMPDIR bug in normal3d and update docs to say PNG is default output
* Wed May  7 2003 EAM
- Bump versioning to 2.7a because rpm thinks 2.6g < 2.6.6 (2.6f)
- reduce problems with libpng versioning by linking to a static libpng.a
* Thu Jan  2 2003 MATSUURA Takanori <t-matsuu@estyle.ne.jp>
- prefix was changed to %%{_prefix}
- improved, more complete spec file
* Thu Nov  7 2002 <jpaint@u.washington.edu>
- modify post-install script for ImageMagick XML delegates file
* Wed Nov  6 2002 EAM
- update RPM distribution to match newer libs in Mandrake 8.2/9.0 Redhat 7.3/8.0
* Fri May  3 2002 EAM
- modify for cross-platform build
* Fri Apr  3 2002 Won-kyu Park <wkpark@kldp.org>
- make raster3d.spec more compliant with conventions for building rpm

