.ad l
.nh
.TH R3DTOPS 1 " 8 May 1999" "Raster3D"
.SH NAME

r3dtops - Raster3D to PostScript label conversion

.SH SYNOPSIS

   r3dtops [-fontscale xx] < infile.r3d > outfile.ps

.PP
\fIr3dtops\fP reads a Raster3D input file, selects out only the
label descriptors (object types 10, 11, 12), and creates a 
PostScript file containing these labels and nothing else.
The PostScript file can be converted into a raster3 image by
ghostscript and then recombined with the the molecular graphics
image produced by running the same input file through render.
This sequence of operations can be done by hand, but is usually
done automatically by the label3d script.

.SH OPTIONS

\fI-fontscale xx\fP -
It is problematic to interpret the font sizes requested so that
they produce the desired size on the eventual output device or
printer. By default the r3dtops program assumes that the eventual
output device is approximately 100dpi. However, printers generally
range from 300 to 1200 dpi. The parameter \fIxx\fP is a scale
factor by which the number of pixels per PostScript unit should
be increased.

.SH BUGS
The whole nature of label handling in Raster3D is not yet settled.
It would be much nicer if all the label processing would be taken
care of by the render program itself.

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
label3d(l), r3d_objects(l), render(l), Raster3D(l)
.PP
.SH AUTHORS

 Ethan A Merritt.

