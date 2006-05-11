.ad l
.nh
.TH AVS2PS 1 " 8 May 1999" "Raster3D"
.SH NAME

avs2ps - convert AVS image input on stdin to monochrome PostScript on stdout

.SH SYNOPSIS

   avs2ps [-b] [-dpi xxx] < infile.avs > outfile.ps

.PP
\fIavs2ps\fP converts a 24-bit color image file in AVS format into
a dithered monochrome PostScript image with the same number of pixels
as the input file.
\fIavs2ps\fP may be used as a filter for the output of the 
\fIrender\fP program to produce a PostScript file directly.

\fIavs2ps\fP is machine independent, and does not impose restrictions on
the tile size in the original image. 
It supersedes the programs \fImolras3d\fP and \fIhdither\fP in the
viewtools package.  

.PP
.SH EXAMPLES
.PP
Produce an unbordered black & white image suitable for printing on a
300 dpi PostScript printer.
.PP
     render < infile.r3d | avs2ps > image.ps
.PP
Add a border, and prepare image for a 400dpi printer
.PP
     render < infile.r3d | avs2ps -b -dpi 400 > image.ps
.SH OPTIONS

.B "-b"
.PP
Draw a border around the figure.
By default \fIavs2ps\fP will produce a borderless image 0.5 inch
in from the bottom left of the page.

.B "-dpi xxx"
.PP
By default \fIavs2ps\fP writes header records into the PostScript
output file which are correct for a 300dpi printer (e.g. an HP IIIsi).
If there is a mis-match between the header records and the actual
resolution of the printer the image quality is substantially degraded.
This option allows one to specify a different printer resolution
(e.g. -dpi 400 for a Next printer, or -dpi 95 for GhostScript on
a 1280x1024 workstation screen).

.SH DESCRIPTION
.PP 
\fIavs2ps\fP converts the input stream to a grey scale image and then
applies an empirical algorithm for dithering and error propagation to
produce a monochrome output image.

.SH SOURCE
.B anonymous ftp site:	
 	ftp.bmsc.washington.edu
 
.B web URL:
 	http://www.bmsc.washington.edu/raster3d/raster3d.html

.B contact:		
 	Ethan A Merritt
 	University of Washington, Seattle WA 98195
 	merritt@u.washington.edu

.SH SEE ALSO
render(l), raster3d(l), avs
.PP
.SH AUTHORS

Ethan A Merritt.  
 Dithering algorithm derived from code by Randy Read and Albert Berghuis.

