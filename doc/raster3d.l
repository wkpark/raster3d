.ad l
.nh
.TH Raster3D 1 "7 February 1996" "Raster3D"
.SH NAME
.PP
Raster3D molecular graphics package
.SH SYNOPSIS
.PP
The \fIRaster3D\fP molecular graphics package consists of a core
program \fIrender\fP 
and a number of ancillary programs 
(\fIballs\fP, \fIrods\fP, \fIribbon\fP)
which produce input files for
rendering from Brookhaven PDB files of atomic coordinates.
\fIRaster3D\fP can also render images composed using version 1.4 of
\fIMOLSCRIPT\fP [Per Kraulis (1991), J. Appl. Cryst. 24, 946-950].

\fIRaster3D\fP is freely available. If you use the package to prepare
figures for publication, please give proper credit to the authors:

     Bacon & Anderson (1988) J. Molec. Graphics 6, 219-220.
     Merritt & Murphy (1994) Acta Cryst. D50, 869-873.

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
A filter utility \fIavs2ps\fP is included in the package which can
convert an AVS format output stream directly to a dithered monochrome
PostScript image.

Although ancillary programs are provided only for the generation of object
descriptions based on atomic coordinates stored in Brookhaven PDB
format, the rendering program is not specific to molecular graphics.

\fIRaster3D\fP currently supports rendering of five object types: 
spheres, triangles, planes, smooth-ended cylinders, and round-ended
cylinders.  
Version 2.1 added support for two additional "object types"
which are really modifiers for characteristic properties of existing objects.  
One of these allows specification
of explicit surface normals at triangle vertices; the other allows specification
of surface reflectance properties specific to individual objects.  

Version 2.2: The major new feature in Raster3D Version 2.2 is support
for transparent objects. In conjunction with the provision for
describing surfaces in terms of triangles with explicit surface normals
this permits Raster3D rendering of transparent molecular surfaces
jointly with the existing provisions for depicting molecular structures.
In keeping with the philosophy of the package, the intention is not to
duplicate the power of more general programs in composing or describing
such surfaces, but rather to provide a tool for creating images with
greater resolution and photorealism than is generally possible using a
graphics workstation screen.

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

     molscript -r infile.dat | render | display avs:-

A similar example using xv as an image viewer, and assuming that TIFF
support has been built into the render program:

     molscript -r infile.dat | render -tiff image.tif
     xv image.tif

.SH SOURCE
.TP 5
	anonymous ftp site:	
                ftp.bmsc.washington.edu
.TP 5
	contact:		
                Ethan A Merritt
                Dept of Biological Structure SM-20
                University of Washington, Seattle WA 98195
                merritt@u.washington.edu

.SH SEE ALSO
rods(1), ribbon(1), balls(1), avs2ps(1), ImageMagick(1), MOLSCRIPT(1)
.PP
.SH AUTHORS
	Originally written by David J. Bacon and Wayne F. Anderson.
	Ancillary programs by Mark Israel, Stephen Samuel, Michael Murphy,
	Albert Berghuis, and Ethan A Merritt.  Extensions, revisions, and 
	modifications by Ethan A Merritt.

