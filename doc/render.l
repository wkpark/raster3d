.l
.nh
.TH RENDER 1 "19 May 2003" "Raster3D"
.SH NAME
render - Raster3D molecular graphics package rendering program
.SH SYNOPSIS
.B "render"  < infile.r3d > outfile.png

.B "render"  -png < infile.r3d > outfile.png

.B "render"  -avs < infile.r3d > outfile.avs

.B "render"  [\fI-quality NN\fP] -jpeg < infile.r3d > outfile.jpeg

.B "render"   -tiff outfile.tiff < infile.r3d 

.B "render"   -sgi "[outfile.rgb]" < infile.r3d

.B "render"   -out outfile.xxx   < infile.r3d
.PP
\fIRender\fP reads an ascii file consisting of several header lines
followed by individual object descriptors. 
The objects are rendered using a fast Z-buffer algorithm to produce
a high quality pixel image which contains one light source, 
additional non-shadowing light sources,
specular highlighting, transparency, and Phong shaded surfaces.
Output is to stdout [or optional file] in the form of a pixel image
with 24 bits of color information per pixel.
As of version 2.7, the default output format is PNG.

.SH OPTIONS
.TP 5
.B "-aa"

Force anti-aliasing (SCHEME 4)

.TP 5
.B "-alpha"

Force output of transparency information (SCHEME 0). This only works
if the output image format supports an alpha channel (AVS, TIFF, but
not JPEG). Note that this turns off anti-aliasing.

.TP 5
.B "-avs [outfile.avs]"

By default \fIrender\fP will produce an PNG image on stdout.
The -avs flag will cause it to output an AVS image file instead, or
stream the output to stdout if no file is specified.

.TP 5
.B "-bg white|black|#RRGGBB"

Set background color. #RRGGBB is a hexadecimal number representing
the red, green and blue components.

.TP 5
.B "-draft"

Turn off anti-aliasing (SCHEME 1) to increase rendering speed.

.TP 5
.B "-fontscale"

Only meaningful in conjunction with the -labels option.
Modifies the interpretation of font sizes during label processing.
Defaults to -fontscale 3.0, which will generate PostScript
labels that are at the correct nominal fint size when printed
at 300 dpi.

.TP 5
.B "-gamma GG"

Apply gamma correction to output image.

.TP 5
.B "-help"

Prints a short summary of command line options.

.TP 5
.B "-invert"

Invert image top-to-bottom. This may be necessary if you are using
some odd viewing program.

.TP 5
.B "-jpeg [outfile.jpeg]"

Only if compiled with -DJPEG_SUPPORT.
By default \fIrender\fP will produce an PNG image on stdout.
The -jpeg flag will cause it to output a JPEG image file instead, or
stream the output to stdout if no file is specified.

.TP 5
.B "-labels [outfile.ps]"

Process labels (object types 10, 11, 12) and create a PostScript
output file label3d.ps in addition to the rendered image file.
Output filename defaults to \fIlabel3d.ps\fP.
This option is used by the \fIlabel3d\fP script.

.TP 5
.B "-out filename.xxx"

Only if compiled with -DIMAGE_PIPE.
The -out flag causes \fIrender\fP to pipe output to the ImageMagick
command convert, with instructions to convert it to an image file
whose type is determined by the filename extension xxx.

.TP 5
.B "-png [outfile.png]"

Only if compiled with -DPNG_SUPPORT.  This is now the default output format.
The -png flag will stream the output to stdout if no file is specified.

.TP 5
.B "-quality"

Only meaningful in conjunction with -jpeg option; sets the 
quality (compression level) of the output image.
Allowable values 1 - 100 (default 90).

.TP 5
.B "-[no]shadow"

Turn shadowing on or off.

.TP 5
.B "-size HHHxVVV"

Override the image size parameters (NTX,NTY,NPX,NPY) in file header
and produce an output image that is exactly HHH pixels in the
horizontal and VVV pixels in the vertical.

.TP 5
.B "-sgi [filename.rgb]"

Only if compiled with -DLIBIMAGE_SUPPORT.
The -sgi flag will cause render to output an SGI libimage style *.rgb file
instead of writing to stdout.
The filename defaults to render.rgb, but specifying a file
on the command line will override this default.

.TP 5
.B "-tiff filename"

Only if compiled with -DTIFF_SUPPORT.
The -tiff flag will cause render to output a TIFF image to the specified
file instead of writing to stdout.

.TP 5
.B "-transparent"

(same as -alpha).
Force output of transparency information (SCHEME 0). This only works
if the output image format supports an alpha channel (AVS, TIFF, but
not JPEG). Note that this turns off anti-aliasing.

.TP 5
.B "-zoom ZZ[%]

Rescale image by factor ZZ (ZZ% if the % sign is present).

.SH HEADER RECORDS
.TP 5 
.B     TITLE    
Anything you like, up to 80 characters.
.TP 5 
.B     NTX,NTY  
Number of "tiles" in each direction.  (The output
display is considered to be divided up into an
array of identical rectangular tiles.)
Maximum = 192 (controlled by MAXNTX, MAXNTY in render.f)
.TP 5 
.B     NPX,NPY  
Number of computing pixels per tile in each direction.
Maximum = 36 (controlled by MAXNPX, MAXNPY in render.f)
.TP 5 
.B     SCHEME   
Pixel averaging (anti-aliasing) scheme. Anti-aliasing
reduces the jaggedness of edges at the cost of additional
computation.
 0 no anti-aliasing, include alpha blend (matte) channel 
 1 no anti-alaising, no matte channel
 2 anti-alias by averaging 2x2 block of computed pixels for 
   each output pixel. Resulting image dimensions are 1/2 of
   the nominal requested size.
 3 anti-alias by averaging 3x3 block of computed pixels for 
   each 2x2 block of output pixels. Resulting image 
   dimensions are 2/3 of the nominal requested size.
   (obsolete - use scheme 4 instead)
 4 anti-alias as in scheme 3, but output image dimensions
   are exactly as requested.

I.e. schemes 0, 1, and 4 produce a NTX*NPX by NTY*NPY pixel image;
scheme 3 produces a (2/3)NTX*NPX by (2/3)NTY*NPY image.
Scheme 3 requires NPX and NPY to be divisible by 3.
Schemes 2 and 4 require NPX and NPY to be divisible by 2.
.TP 5 
.B     BKGND    
Background colour (red, green, and blue components,
each in the range 0 to 1).
.TP 5 
.B     SHADOW   
T to calculate shadowing within the scene, F to omit shadows
.TP 5 
.B     IPHONG   
Phong power (e.g., 25) for specular reflections.
A smaller value results in a larger spot.
.TP 5 
.B     STRAIT   
Straight-on (secondary) light source contribution
(e.g., 0.15).  The primary light source contribution
(see also SOURCE below) is given by PRIMAR = 1 - STRAIT.
.TP 5 
.B     AMBIEN   
Ambient reflection quantity (e.g., 0.05).
.TP 5 
.B     SPECLR   
Specular reflection quantity (e.g., 0.25).
The diffuse reflection quantity is given by
DIFFUS = 1 - (AMBIEN+SPECLR).  Ambient and diffuse
reflections are chromatic, taking on the specified
colour of each object, whereas specular reflections
are white.
.TP 5 
.B     EYEPOS   
You can think of the image produced by Raster3D as corresponding 
to a photograph taken by a camera placed a certain distance away
from the objects making up the scene. This distance is controlled
by the EYEPOS parameter.  EYEPOS = 4 describes a perspective corresponding
to a viewing distance 4 times the narrow dimension of the described scene.
EYEPOS = 0 disables perspective.
.TP 5 
.B     SOURCE   
Primary light source position (e.g., 1 1 1).  
This is a white light point
source at infinite distance in the direction of this vector
(see note on co-ordinate convention below).  
The secondary light source is always haed-on. 
Only the primary light source casts shadows.
.TP 5 
.B     TMAT     
Homogeneous global transformation for input objects,
given as a 4x4 matrix on 4 lines just as you would
write it if you intended it to be a postfix (suffix)
operator.  The upper left 3x3 submatrix expresses
a pure rotation, the lower left 1x3 submatrix
gives a translation, the upper right 3x1 submatrix
should be zero (otherwise extra perspective is
introduced), and the lower right scalar produces
global scaling.
Coordinate vectors [x y z] are extended with a 1
to make them homogeneous, and then postmultiplied
by the entire matrix; 
 i.e., if 
      [x' y' z' h'] = [x y z 1][TMAT],
 then the ultimate co-ordinates are
      [x" y" z"] = (1/h')[x' y' z'].
.TP 5 
.B     INMODE   
Object input mode (1, 2, or 3), where
mode 1 means that all objects are triangles,
mode 2 means that all objects are spheres,
and mode 3 means that each object will be preceded
by a record containing a single number indicating its type.
The Raster3D programs always use mode INMODE 3.
 type 1 = triangle
 type 2 = sphere
 type 3 = round-ended cylinder
 type 4   (not used)
 type 5 = flat-ended cylinder
 type 6 = plane (triangle with infinite extent)
 type 7 = normals at vertices of previous triangle
 type 8 = material properties for subsequent objects
 type 9 = terminate previous material properties
 type 10/11/12 = reserved for label processing
 type 13 = glow light source
 type 14 = quadric surface
 type 15 = do not apply TMAT to subsequent objects
 type 16 = global rendering properties
 type 17 = colors for vertices of preceding triangle or cylinder
 type 0 = end of input file
.TP 5 

.B INFMT or INFMTS
Object input format specifier(s). 
Normally * for free-format input.

.TP 1
.B SAMPLE HEADER

 Title (This is a 1280x1024 pixel anti-aliased image)
 80 64     tiles in x,y
 24 24     pixels (x,y) per tile
 3         anti-aliasing level 3;  3x3->2x2
 0 0 0     black background
 F         no shadows cast
 25        Phong power
 0.25      secondary light contribution
 0.05      ambient light contribution
 0.25      specular reflection component
 4.0       eye position
 1 1 1     main light source (from over right shoulder)
 1 0 0 0   TMAT matrix describing
 0 1 0 0        input coordinate transformation
 0 0 1 0
 0 0 0 0.6 enlarge by 40% (smaller scalar -> bigger objects)
 3         mixed objects
 *         (free format triangle descriptors)
 *         (free format sphere descriptors)
 *         (free format cylinder descriptors)


.PP
.SH FILE INDIRECTION
.PP
At any point in the input stream to render where an object descriptor
would be legal, it is also legal to insert a line beginning with `@'.
In this case the remainder of the line is interpreted as the
name of a file from which further input is taken. This mechanism makes
it possible to re-use standard objects in multiple rendered scenes, e.g.
a set of bounding planes or standard definitions of material properties.
When input from this level of file indirection is terminated by
encountering an object descriptor of type 0, control returns to the
previous input stream. Multiple levels of file indirection are possible.

Files are first searched for in the current directory. If this search
fails, they are sought relative to the library directory specified by
the environmental variable R3D_LIB.

.SH RASTER3D OBJECT TYPES
.PP
For Raster3D object types and object descriptor formats,
see the man page for \fBr3d_objects\fP.

.SH ERROR MESSAGES
.PP
Some error messages may be safely ignored.
.PP
	Possible shadowing error NSXMAX= xxx
.PP
This is most usually caused by an object which projects far out of the field
of view, for example a plane surface. In most cases the shadowing "error" 
refers to a shadow which lies outside of the image entirely.  However, if
your image does in fact contain missing or truncated shadows you can overcome
this problem by re-compiling the render program with larger values of
NSX and NSY as indicated by the error message.

.PP
If you are creating images of very large proteins, or if you are rendering
surfaces with many facets, then you may fill the storage arrays in render.
If so you will get an error message something like
.PP
	STOP 1234 **** too many objects - increase MAXOBJ and recompile
.PP
See the HTML or PostScript documentation for instructions on
increasing array sizes.

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
HTML and PostScript documentation
 r3d_objects(l), avs2ps(l), rastep(l), rods(l), ribbon(l), 
balls(l), label3d(l), stereo3d(l)
.PP
.SH AUTHORS
Originally written by David J. Bacon. 
 Extensions, revisions, and modifications by Ethan A Merritt.

