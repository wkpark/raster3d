.l
.nh
.TH R3D_OBJECTS 1 " 8 May 1999" "Raster3D"
.SH NAME
Object types used by the Raster3D package

These are the object descriptor types, and the required
parameters, recognized by the \fBrender\fP program and
other components of the Raster3D package.

.SH Object type 1 - triangle
x1, y1, z1, x2, y2, z2, x3, y3, z3, red, green, blue;

.SH Object type 2 - sphere
x, y, z, radius, red, green, blue;

.SH Object type 3 - cylinder with round ends
x1, y1, z1, R1, x2, y2, z2, R2, red, green, blue.
 (R1 is the cylinder radius, R2 is currently ignored).

.SH Object type 4 - not used
 
.SH Object type 5 - cylinder with flat ends
x1, y1, z1, R1, x2, y2, z2, R2, red, green, blue.
 (R1 is the cylinder radius, R2 is currently ignored).

.SH Object type 6 - plane
x1, y1, z1, x2, y2, z2, x3, y3, z3, red, green, blue;
 The plane is defined by any three points. It's color fades
from full intensity in the foreground to half-intensity at
the back.

.SH Object type 7 - Vertex normals
Explicit vertex normals for preceding triangle object. This 
object must directly follow the triangle object, and uses the 
same format.

.SH Object type 8 - Material properties. 
These values override the specification of
lighting and specular highlighting in the header records, allowing
some objects to have different surface properties from the rest.
The specified values will apply to all subsequent objects until
an object of type 9 is encountered.  The parameters are read as
free format floating point numbers from the next line of the
input stream:
 8
 MPHONG MSPEC SR SG SB CLRITY OPT1 OPT2 OPT3 OPT4
 
.TP 5
.B MPHONG
value >=0 overrides the global Phong power for specular highlighting
.TP 5
.B MSPEC
value >=0 overrides the global specular scattering contribution
.TP 5
.B SR,SG,SB 
RGB triple specifying color of reflected light
(by default all specular highlights are white)
A negative value for any component will default to 
the base colour component of the object being rendered.
.TP 5
.B CLRITY 
The degree of transparency for this material, with 0.0 indicating
an opaque surface and 1.0 a purely transparent one.
.TP 5
.B OPT1
Affects processing of transparent objects which belong to the
same material, but occlude each other (for instance the front
and back surfaces of a transparent molecular surface). 
 OPT1 = 0 will render all outward-facing transparent surfaces
 OPT1 = 1 will render only the topmost outward-facing surface
 OPT1 = 2 will render both the outward- and inward- facing surfaces of transparent spheres and cylinders
.TP 5
.B OPT2
Selects from several variant treatments of transparency as a function of 
alpha and the Z component ZN of the surface normal.
 OPT2 = 0    T(ZN) = 0.25(1+cos(pi*alpha*ZN)^2
 OPT2 = 1    T(ZN) = (1-abs(alpha*ZN))^2
 OPT2 = 2    T(ZN) = 1 - alpha ^ (cos(pi/4) / abs(ZN))
 OPT2 = 3    T(ZN) = 1 - alpha
Option 0 is an empirical function that I think looks good.
.TP 5
.B OPT3
Not used
.TP 5
.B OPT4
Non-zero to indicate that additional modifier records follow
immediately. Each modifier must constitute a single line,
and OPT4 tells how many of these lines there will be.
Here is a complete list of modifiers currently available, and their parameters
 SOLID		RED GREEN BLUE
 BACKFACE	RED GREEN BLUE MPHONG MSPEC
 FRONTCLIP	ZFRONT
 BACKCLIP	ZBACK
 BOUNDING_COLOR	RED GREEN BLUE
 BOUNDING_PLANE	BPTYPE X Y Z Xnorm Ynorm Znorm
 ORTEP_LIKE	used to create fancy ellipsoids (see rastep documentation)

.SH Object type 9 - End_material
Terminates application of all previously defined special material properties
(object type 8) or isolation from TMAT transformation (object type 15).

.SH Object types 10,11,12 - Label descriptors
These object types are used as label descriptors.
The current version of render ignores them unless the -labels
option is selected on the command line.

.TP 0
.B Object type 10 - "font_name" size "alignment"
Legal values subject to details of font processing implementation.
 \fBfont_name\fP is any font name recognized by libgd
 \fBsize\fP is the font size in points, but is modified by render's -fontscale option 
 \fBalignment\fP is "Left", "Right", "Center", or "Offset".

.TP 0
.B Object type 11 - Label text
 X Y Z R G B     (On one line) label position and color
 Label text      (On 2nd line)

.SH Object type 13 - Glow light source.
This is a colored, non-shadowing, light source with finite [x y z]
coordinates and a limited range of illumination. 
Control parameters are read in free format from a single line of 
input following the line specifying the object type. 
 13
 GLOWSRC(3) GLOWRAD GLOW GOPT GPHONG GLOWCOL(3)
.TP 5
.B GLOWSRC(3) 
[x y z] coordinates of light source 
.TP 5
.B GLOWRAD 
limiting radius of light source (see GOPT) 
.TP 5
.B GLOW 
fractional contribution (0.0 - 1.0) of glow light to total 
lighting model 
.TP 5
.B GOPT 
(integer 0/1/2/3/...) - controls functional form in which 
limiting radius is applied [under development] 
.TP 5
.B GPHONG 
Phong parameter controlling specular highlights from glow light
.TP 5
.B GLOWCOL(3) 
RGB triple specifying color of glow light source  


.SH Object type 14 - Quadric surface
  
x, y, z, limiting_radius, red, green, blue, A, B, C, D, E, F, G, H, I, J 
 
Quadric surfaces include spheres, cones, ellipsoids, paraboloids, 
and hyperboloids. The motivation for this code was to allow 
rendering thermal ellipsoids for atoms, so the other shapes have 
not been extensively tested. A quadric surface is described by 
10 parameters (A...J). Although these parameters are sufficient 
to describe a quadric surface located anywhere, for efficiency 
during rendering it is also useful to know the center and a 
bounding sphere. So by convention we require that (A...J) describe 
a surface centered at the origin, and add additional parameters 
x, y, z to add a translation component. Therefore
a QUADRIC descriptor to render has the 17 parameters listed above.

The surface is the set of points for which Q(x,y,z) = 0 
where
 
 Q(x,y,z) = Ax2 + By2 + Cz2 + 2Dxy + 2Eyz + 2Fzx + 2Gx + 2Hy + 2Iz + J 
 
Points further from the origin (prior to translation!) than the
limiting radius are not rendered. 

.SH Object type 15 - TMAT Isolation
Objects following this descriptor are interpreted as being in an 
absolute coordinate system centered at the origin and having unit 
extent in X, Y, and Z. If the image is rectangular, coordinates run
from -0.5 to +0.5 along both X and Y. If the image is rectangular,
coordinates run from -0.5 to +0.5 along the shorter dimension.
The rotation and translation described by the 
TMAT matrix is not applied. This isolation from TMAT applies to 
subsequent objects until an object of type 9 is encountered. 

.SH Object type 16 - Global property
Object type 16 is always followed by a single record that affects
the rendering of the entire scene. Currently the options are
FOG, FRONTCLIP, and BACKCLIP.  There are also options ROTATION and
TRANSLATION that affect only objects following their appearance in
the input stream.

.SH Object type 17 - Vertex colours
Explicit vertex colours for preceding triangle or cylinder object.
    R1 G1 B1   R2 G2 B2   R3 G3 B3

.SH Object type 18 - Vertex transparency
Explicit transparency at vertices of preceding object.
All three values are required, though only the first two are used
for cylinders, and only the first one for spheres.
    Trans1 Trans2 Trans3

.SH Object type 19 - TMAT Isolation
Similar to object type 15, except the the x and y scales are the
same only in the case of a square image.  Both the x and y axes run 
from -0.5 to +0.5 regardless of the aspect ratio of the rendered image.

.SH SEE ALSO
 PostScript and HTML documentation
 render(l) 
