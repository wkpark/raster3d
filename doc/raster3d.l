.ad l
.nh
.TH Raster3D 1 "23 November 1993" "Raster3D"
.SH NAME
.PP
Raster3D molecular graphics package
.SH SYNOPSIS
.PP
The \fIRaster3D\fP molecular graphics package consists of a core
program \fIrender\fP 
and a number of ancillary programs 
(\fIsetup\fP, \fIrods\fP, \fIribbon\fP)
which produce input files for
rendering from Brookhaven PDB files of atomic coordinates.
\fIRaster3D\fP can also render images composed using version 1.4 of
\fIMOLSCRIPT\fP [Per Kraulis (1991), J. Appl. Cryst. 24, 946-950].

.SH DESCRIPTION
.PP
\fIRaster3D\fP uses a fast Z-buffer algorithm to produce
high quality pixel images featuring two light sources, 
specular highlighting, Phong shaded surfaces, and shadowing.
Output is in the form of a pixel image
with 24 bits of color information per pixel.

\fIRaster3D\fP does not depend on graphics hardware for operation.
Four image output formats are supported: AVS, TIFF, SGI libimage, and
a private format which is backwards-compatible with earlier versions of 
\fIRaster3D\fP.  To actually view or manipulate the images produced,
you must also have installed an image viewing package (e.g. John
Cristy's ImageMagick or the SGI libimage utilities).

\fIRaster3D\fP Version 2.0
supports rendering of five object types: 
spheres, triangles, planes, smooth-ended cylinders, and round-ended
cylinders.  
In addition to various bug-fixes and additional object
types, Version 2.0 shadowing is improved over earlier versions, and the
utility programs are more tightly integrated with each other and with
external programs such as MOLSCRIPT.

Although ancillary programs are provided only for the generation of object
descriptions based on atomic coordinates stored in Brookhaven PDB
format,
the rendering program is not specific to molecular graphics.

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
     grep "FE" protein.pdb | cat colorfile - | setup -h > irons.r3d
     #
     # combine the two descriptions and render as AVS image file
     #
     cat ribbon.r3d irons.r3d | render > picture.x

Integrated use of MOLSCRIPT/Raster3D/ImageMagick allows one to
describe, render, and view 3D representations of existing MOLSCRIPT
figures:

     molscript -r infile.dat | render | display avs:-

A similar example using xv as an image viewer, and assuming that TIFF
support has been built into the render program:

     molscript -r infile.dat | render -tiff image.tif
     xv image.tif

.SH SOURCE
.TP 5
	anonymous ftp site:	
                stanzi.bchem.washington.edu   (128.95.12.38)
.TP 5
	contact:		
                Ethan A Merritt
                Dept of Biological Structure SM-20
                University of Washington, Seattle WA 98195
                merritt@u.washington.edu

.SH SEE ALSO
rods(1), ribbon(1), setup(1), ImageMagick(1), MOLSCRIPT(1)
.PP
.SH AUTHORS
	Originally written by David J. Bacon and Wayne F. Anderson.
	Ancillary programs by Mark Israel, Stephen Samuel, Michael Murphy,
	Albert Berghuis, and Ethan A Merritt.  Extensions, revisions, and 
	modifications by Ethan A Merritt.

