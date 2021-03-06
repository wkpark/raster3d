.ad l
.nh
.TH RASTEP 1 " 14 Dec 2010" "Raster3D"

.SH NAME

rastep - (Raster3D Thermal Ellipsoid Program)
.SH SYNOPSIS
.PP
.B "rastep" [\fI-h\fP] [\fI-iso\fP] [\fI-Bcolor Bmin Bmax\fP] 
.B [\fI-prob Plevel\fP] [\fI-fancy[0-3]\fP] [\fI-radius R\fP]
.B < infile.pdb > ellipsoids.r3d
.PP 
.B "rastep" \fI-tabulate [tabfile]\fP [\fI-by_atomtype\fP]
.B [\fI-com [comtabfile]\fP] < infile.pdb > statistics.text
.PP
\fIRastep\fP reads a PDB coordinate file.
This file must contain ANISOU records describing atoms refined anisotropically.
\fIRastep\fP can either create an input file for the Raster3D render program
or perform a statistical analysis of the atomic anisotropy for various classes 
of input atoms.
By default the program creates an ellipsoid+stick scene description in which 
each atom  is represented by an ellipsoid enclosing an isosurface 
of the probability density function. 
These are commonly known as thermal ellipsoids. 
.PP
The program can be run in an alternate mode, controlled by the -tabulate option,
in which the primary output to stdout is a list of the Eigenvalues of the Uij 
matrix, followed by the corresponding atomic anisotropy and isotropic Ueq, 
for each atom in the input file with both an ATOM record and a
matching ANISOU record.
This mode is used by the validation tools Parvati and Skittls.
.PP
.SH EXAMPLES
.PP
To describe thermal ellipsoids at the 50% probability level, with default CPK
colors, and send it for immediate rendering into a PNG file
.PP
     rastep < infile.pdb | render -png picture.png
.PP
To describe the same ellipsoids colored by Biso, omiting header records so that
the resulting input file can be merged with other scene components
.PP
     rastep -h -Bcolor 10. 30. < infile.pdb > ellipsoids.r3d
     cat header.r3d ellipsoids.r3d otherstuff.r3d | render > picture.png
.SH OPTIONS

.B "-auto"
.PP
Auto-selection of viewing angle, chosen to minimize the spread of the atoms 
along the view direction. 

.B "-Bcolor Bmin Bmax"
.PP
Assign colors based on B values rather than mathcing ATOM records against
input or default COLOUR records. Atoms with B <= Bmin will be colored
dark blue; atoms with B >= Bmax will be colored light red; atoms with 
Bmin < B < Bmax will be assigned colors shading smoothly through the
spectrum from blue to red.

.B "-fancy[0-6]"
.PP
The \fI-fancy\fP option selects increasingly complex representations of
the rendered ellipsoids.
  -fancy0 [default] = solid surface
  -fancy1 = principal axes of ellipsoid, with transparent bounding surface
  -fancy2 = colored equatorial planes of the ellipsoid
  -fancy3 = colored equatorial planes with transparent bounding surface
  -fancy4 = transparent bounding surface containing longest principle axis
  -fancy5 = for ORTEP lovers, a solid ellipsoid with one octant missing
  -fancy6 = for ORTEP lovers who want the missing octant in a separate color

.B "-h"
.PP
Suppress header records in output.  
By default \fIrastep\fP will produce an output file which starts with 
header records containing a default set of scaling and processing
options.
The \fI-h\fP flag will suppress these header records.
This option is useful for producing files which describe only part
of a scene, and which are to be later combined with descriptor files
produced by other programs.

.B "-iso"
.PP
Force isotropic probability surfaces (spheres). By default rastep will look
for ANISOU records in the PDB file and use these to generate ellipsoids.
If no ANISOU record is present for a given atom, the B value given in the
ATOM/HETATM record will be used to generate a sphere instead. Selecting the
\fI-iso\fP option will force the program to use the B value in the ATOM
record even if an ANISOU record is also present.

.B "-mini"
.PP
Auto-orientation (as in -auto) and small size plot (176x208). 

.B "-nohydrogens"
.PP
Do not plot hydrogens, even if present in PDB file.

.B "-prob Plevel"
.PP
By default, isosurfaces are drawn to enclose the 50% probability level in the
density function described by the Uij values in the ANISOU record.
The \fI-prob\fP option allows you to select a different probability level
instead. If 0 < Plevel < 1 this value is interpreted as a fraction; 
if Plevel > 1 this value is interpreted as a percent.

.B "-radius R"
.PP
By default, rastep draws bonds with radius 0.10A between neighboring atoms
using the same algorithm as rods. This option allows you to change the 
radius of the bonds. If the radius is set to 0 no bonds are drawn.

.B "-tabulate [tabfile]"
.PP
The -tabulate option requests that the program accumulate and print
statistics on the distribution of anisotropy among atoms in the input
file rather than producing an input file for render. The principle axes
and anisotropy of each atom are written to stdout. An overall
statistical summary is written to tabfile if specified, otherwise to
stdout.

.B "-by_atomtype"
.PP
The -by_atomtype option is a modifier to -tabulate. It causes a further
subdivision of atoms by atom type in the preparation of statistical
summaries. Atom types are taken from columns 77:78 of the PDB ATOM
records.

.B "-com [comtabfile]"
.PP
Tabulate distribution of anisotropy in shells by distance from 
center-of-mass. Output to comtabfile if specified, otherwise to stdout.

.SH NOTES
.PP 
There is little, if any, consistency in format among the various programs 
which write out anisotropic displacement parameters. This program interprets
the Uij values in the order specified for ANISOU records in PDB format.
That is, columns 29-70 of the PDB record are interpreted as integers
representing 10000 * Uij, in the order U11 U22 U33 U12 U13 U23.
Note in particular that the order of the cross-terms is not the same as that
used by ORTEP or shelx, neither of which use PDB format anyway.
However, the program shelxpro will produce correctly formatted PDB records
from a shelx coordinate file.


.SH SOURCE
.B web URL:
  	http://www.bmsc.washington.edu/raster3d/raster3d.html
.B contact:		
 	Ethan A Merritt
 	University of Washington, Seattle WA 98195
 	merritt@u.washington.edu

.SH SEE ALSO
raster3d(l), render(l)
.PP
.SH AUTHORS
	Ethan A Merritt.

