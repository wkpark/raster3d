.ad l
.nh
.TH LABEL3D 1 " 9 Feb 2002" "Raster3D"
.SH NAME

label3d - process a Raster3D scene containing labels

.SH SYNOPSIS

   label3d [-tiff [outfile.tiff]] < infile.r3d > outfile.tiff
   label3d  -png  [outfile.png ]  < infile.r3d > outfile.png 

.PP
\fIlabel3d\fP is a shell script that processes a Raster3D input 
file containing label descriptors (object types 10, 11, 12)
into a TIFF or PNG image containing both the molecular
graphics image and the associated labels.

label3d uses the Raster3D utilities \fIr3dtops\fP and \fIrender\fP, 
and requires that both ghostscript and ImageMagick be installed.

.SH ENVIRONMENT
Scratch files are created in directory TMPDIR, if defined, 
otherwise in /usr/tmp.  Font sizes are scaled by the value of the
environmental parameter FONTSCALE.  FONTSCALE defaults to 3.

.SH BUGS
The whole nature of label handling in Raster3D is not yet settled.
It would be much nicer if all the label processing were taken
care of by the render program itself.

The script assumes that the image is being prepared for printing
at 300 dpi (r3dtops is called with \fI-fontscale 3.0\fP).

Labels always appear on top of the molecular graphics image,
even labels which should be occluded by a foreground object.

.SH SOURCE
 
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

