.ad l
.nh
.TH STEREO3D 1 " 8 May 1999" "Raster3D"
.SH NAME

stereo3d - render a Raster3D scene as a side-by-side stereo pair

.SH SYNOPSIS

   stereo3d render_input_file
   display stereo3d.tiff

.PP
\fIstereo3d\fP is a shell script that renders a single Raster3D input 
file as a side-by-side stereo pair. The resulting image is placed in 
file stereo.tiff. 

stereo3d uses the Raster3D utilities \fInormal3d\fP and \fIrender\fP, 
and the utility programs identify, mogrify, and montage from John Cristy's 
ImageMagick image processing package. Three
intermediate files are produced by normal3d: 
 	stereo3d.tmp - normalized version of the input file 
 	left.r3d - header records for left eye view 
 	right.r3d - header records for right eye view 

The two views are separately rendered to yield images left.tiff and
right.tiff (deleted upon completion), given black borders, and merged to
form a single side-by-side stereo pair stereo.tiff.

.PP
.SH EXAMPLES

If the following line would render and display a single image: 
 	render -tiff single.tiff < input.r3d;  display single.tiff
then the following line would render and display the same scene as a 
stereo pair instead: 
 	stereo3d input.r3d; display stereo.tiff

NB: The output image is always called stereo.tiff; there is no provision at
present to specify some other name. 
.PP
.SH ENVIRONMENT

This script requires TIFF support in render, the unix utility nawk, and
the ImageMagick image processing package. The three intermediate files
listed above are created each time the script is run. Any exiting files
named left.tiff, right.tiff, or stereo.tiff will be destroyed.

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
normal3d(l), render(l), Raster3D(l)
.PP
.SH AUTHORS

 Ethan A Merritt.

