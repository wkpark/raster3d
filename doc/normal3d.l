.ad l
.nh
.TH NORMAL3D 1 " 9 February 1996" "Raster3D"
.SH NAME

normal3d - apply transformation matrix in Raster3D input file

.SH SYNOPSIS

   normal3d [-h] < infile.r3d > normalized.r3d

.SH DESCRIPTION
.PP
\fInormal3d\fP 
reads a Raster3D input file from stdin, applies any coordinate manipulations
specified in the header, and writes the modified file to stdout.
.PP
The output file from normal3d describes exactly the same image as the original
input file. Header records are left unchanged except for the transfomation
matrix (which becomes the identity matrix) and the format specifiers (which are
set to *). The -h flag suppresses all header records in the output file.
All objects in the input file are
also in the output file, but their coordinate descriptions have been
normalized (i.e. the original transformation matrix has been applied).
.PP
\fInormal3d\fP 
also reports the total number of objects in the input file by object type,
and gives the array sizes which would be required for the render program
to process the file.  It may therefore be used to determine how large the
array sizes in render should be set.

.SH OPTIONS

.B "-h"
.PP
Suppress header records in output.  
This option is useful for producing files which describe only part
of a scene, and which are to be later combined with descriptor files

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
render(1), Raster3D(1)
.PP
.SH AUTHORS

 Ethan A Merritt.
