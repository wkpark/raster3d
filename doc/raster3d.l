.ad l
.nh
.TH Raster3D 1 "28 August 2001" "Raster3D V2.6"
.SH NAME
.PP
Raster3D molecular graphics package
.SH SYNOPSIS
.PP
The \fIRaster3D\fP molecular graphics package consists of a core
program \fIrender\fP 
and a number of ancillary programs 
(\fIballs\fP, \fIrastep\fP, \fIrods\fP, \fIribbon\fP)
which produce input files for
rendering from PDB (Protein Data Bank) files of atomic coordinates.
\fIRaster3D\fP can also render images composed using other programs
such as \fIMOLSCRIPT\fP [Per Kraulis (1991), J. Appl. Cryst. 24, 946-950].

\fIRaster3D\fP is freely available. If you use the package to prepare
figures for publication, please give proper credit to the authors.
The proper citation for the current version is Merritt & Bacon (1997).

     Bacon & Anderson (1988) J. Molec. Graphics 6, 219-220.
     Merritt & Murphy (1994) Acta Cryst. D50, 869-873.
     Merritt & Bacon  (1997) Meth. Enzymol. 277, 505-524.

.SH DESCRIPTION
.PP
\fIRaster3D\fP uses a fast Z-buffer algorithm to produce
high quality pixel images featuring one shadowing light source, 
additional non-shadowing light sources,
specular highlighting, transparency, and Phong shaded surfaces.
Output is in the form of a pixel image
with 24 bits of color information per pixel.
\fIRaster3D\fP does not depend on graphics hardware.

The following image output formats are supported: AVS, JPEG, PNG,
TIFF, and SGI libimage.
To actually view or manipulate the images produced,
you must also have installed an image viewing package (e.g. John
Cristy's ImageMagick or the SGI libimage utilities).
A filter utility \fIavs2ps\fP is included in the package which can
convert an AVS format output stream directly to a dithered monochrome
PostScript image. Other output formats are possible in conjunction
with ImageMagick.

Although Raster3D is not intended as a general purpose ray-tracing package,
nothing in the rendering process is specific to molecular graphics.

.SH EXAMPLES

Using only programs included in the Raster3D distribution one can
create and render space-filling models, ball-and-stick models, 
ribbon models, and figures composed of any combination of these.
The following set of commands would produce a composite figure
of an Fe-containing metalloprotein with a smoothly shaded ribbon
representation of the protein and spheres drawn for the Fe atoms:

     #
     # Draw smooth ribbon with default color scheme 2,
     # save description (with header records) in ribbon.r3d
     #
     cat protein.pdb | ribbon -d2 > ribbon.r3d
     #
     # Extract Fe atoms only, and draw as spheres.
     # Color info is taken from colorfile.
     # Save description (with no header records) in irons.r3d
     #
     grep "FE" protein.pdb | cat colorfile - | balls -h > irons.r3d
     #
     # combine the two descriptions and render as AVS image file
     #
     cat ribbon.r3d irons.r3d | render > picture.x

Integrated use of MOLSCRIPT/Raster3D/ImageMagick allows one to
describe, render, and view 3D representations of existing MOLSCRIPT
figures:

     molscript -r infile.dat | render | display png:-

A similar example using xv as an image viewer, and assuming that TIFF
support has been built into the render program:

     molscript -r infile.dat | render -tiff image.tif
     xv image.tif

.SH SOURCE
.B anonymous ftp site:	
 	ftp.bmsc.washington.edu

.B web URL:
 	http://www.bmsc.washington.edu/raster3d/raster3d.html
 
.B contact:		
 	Ethan A Merritt
 	Dept of Biological Structure Box 357742
 	University of Washington, Seattle WA 98195
 	merritt@u.washington.edu

.SH SEE ALSO
render(l), rastep(l), rods(l), ribbon(l), balls(l), avs2ps(l), 
label3d(l), stereo3d(l)
.PP
.SH AUTHORS
 Originally written by David J Bacon and Wayne F Anderson.
 Ancillary programs by Mark Israel, Stephen Samuel, 
Michael Murphy, Albert Berghuis, and Ethan A Merritt.  
Extensions, revisions, and modifications by Ethan A Merritt.

