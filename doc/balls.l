.ad l
.nh
.TH BALLS 1 "8 May 1999" "Raster3D"
.SH NAME

balls - preprocessor for space-filling models in Raster3D molecular graphics package
.SH SYNOPSIS
.PP
.B "balls" [\fI-h\fP]
.PP
\fIBalls\fP reads a file describing atom colours and/or a 
PDB coordinate file and produces a file containing Raster3D descriptor
records for each atom in the input file.  The file produced by \fIballs\fP
may be fed directly to \fIrender\fP or it may be combined with descriptor
files produced by other Raster3D utilities.
.PP
.SH EXAMPLES
.PP
To describe a simple space-filling model coloured by residue type:
.PP
     cat mycolours.pdb protein.pdb | balls | render > mypicture.avs
.PP
To include a pre-selected view matrix with the same model:
.PP
     cp  view1.matrix setup.matrix
     cat mycolours.pdb protein.pdb | balls | render > mypicture.avs
.PP
To prepend header records describing a pre-selected scale and view:
.PP
     cat mycolours.pdb protein.pdb | balls -h > balls.r3d
     cat header.r3d balls.r3d | render > mypicture.avs
.SH OPTIONS

.B "-h"
.PP
Suppress header records in output.  
By default \fIballs\fP will produce an output file which starts with 
header records containing a default set of scaling and processing
options.
The \fI-h\fP flag will suppress these header records so that the output
file contains only sphere descriptors.
This option is useful for producing files which describe only part
of a scene, and which are to be later combined with descriptor files
produced by other programs.

.SH DESCRIPTION
.PP 
The input to \fIballs\fP consists of a single text file
containing colour information and atomic coordinates in 
PDB data bank format.
Coordinates are output as Raster3D descriptor records
with colours and sphere radii assigned according to the COLO
records described below.  
By default atoms are assigned CPK colours.
By default the output file contains a set of header records as
required by the \fIrender\fP program.
The header is constructed to include a TMAT matrix corresponding to
the transformation matrix contained in
file \fIsetup.matrix\fP (if it exists), or to the Eulerian angles
contained in file \fIsetup.angles\fP (if it exists).

.PP 
Colours are assigned to atoms using a matching process,
using COLOUR records prepended to the input PDB file.
Raster3D uses a pseudo-PDB record type with 
with COLO in the first 4 columns:
 
   Columns
 
    1 -  4   COLO
 
    7 - 30   Mask (described below)
 
   31 - 38   Red component
 
   39 - 46   Green component
 
   47 - 54   Blue component
 
   55 - 60   van der Waals radius in Angstroms
 
   61 - 80   Comments
 
Note that the Red, Green, and Blue components are in the same positions
as the X, Y, and Z components of an ATOM or HETA record, and the van der
Waals radius goes in place of the Occupancy.  
The Red, Green, and Blue components must all be in the range 0 to 1.

The Mask field is used in the matching process as follows.  
First the program reads in and stores all the ATOM, HETA, and
COLO records in input order.  Then it goes through each stored
ATOM/HETA record in turn, and searches for a COLO record that matches
the ATOM/HETA record in all of columns 7 through 30.  The first such
COLO record to be found determines the colour and radius of the atom.

In order that one COLO record can provide colour and radius
specifications for more than one atom (e.g., based on residue or atom
type, or any other criterion for which labels can be given somewhere in
columns 7 through 30), the "#" symbol is used as a wildcard. I.e. a #
in a COLO record matches any character in the
corresponding column in an ATOM or HETA record.  All other characters
must match literally to count as a match.  Note that the very last COLO
record in the input should have # symbols in all of columns 7 through
30 in order to provide a colour for any atom whose ATOM/HETA record
fails to match any previous COLO record.  This idea of matching masks for
colour specifications is due to Colin Broughton.

Several files of COLO records, including one based on Bob Fletterick's
"Shapely Models" and another mimicking CPK model parts, are
provided as samples.
.SH ENVIRONMENT
.TP 5

The files setup.matrix and setup.angles, if they exist, affect the
header records produced by \fIballs\fP.

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
render(l), rods(l), ribbon(l)
.PP
.SH AUTHORS
Originally written by David J Bacon and Wayne F Anderson.
 Extensions and revisions by Ethan A Merritt.

