.ad l
.nh
.TH LABEL3D 1 " 8 May 1999" "Raster3D"
.SH NAME

label3d - process a Raster3D scene containing labels

.SH SYNOPSIS

   label3d render_input_file
   display label3d.tiff

.PP
\fIlabel3d\fP is a shell script that processes a Raster3D input 
file containing label descriptors (object types 10, 11, 12)
into a TIFF image label3d.tiff containing both the molecular
graphics image and the associated labels.

label3d uses the Raster3D utilities \fIr3dtops\fP and \fIrender\fP, 
and requires that both ghostscript and ImageMagick be installed.
Three intermediate files are produced by normal3d: 
 	label3d.tmp - temporary copy of input stream
	label3d.ps  - PostScript description of labels created by r3dtops
 	render.tiff - TIFF image produced by render
 
Any existing files by these names will be destroyed.

.SH ENVIRONMENT
This script requires TIFF support in render, the unix utility nawk,
the ImageMagick image processing package, and ghostscript. 

.SH BUGS
The whole nature of label handling in Raster3D is not yet settled.
It would be much nicer if all the label processing were taken
care of by the render program itself.

The script assumes that the image is being prepared for printing
at 300 dpi (r3dtops is called with \fI-fontscale 3.0\fP).

Labels always appear on top of the molecular graphics image,
even labels which should be occluded by a foreground object.

At the moment the stereo3d script does not know to call label3d
if there are labels present.

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
render(l), r3dtops(l), Raster3D(l), stereo3d(l)
.PP
.SH AUTHORS

 Ethan A Merritt.

