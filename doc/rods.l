.ad l
.nh
.TH RODS 1 "23 November 1993" "Raster3D"
.SH NAME

rods - preprocessor for ball-and-stick models in Raster3D molecular graphics package
.SH SYNOPSIS

.B "rods" [\fI-h\fP] [\fI-b\fP]
.PP
\fIRods\fP reads a file describing atom colours and/or a Brookhaven 
PDB coordinate file and produces a file containing Raster3D descriptor
records.
The file produced by \fIrods\fP
may be fed directly to \fIrender\fP or it may be combined with descriptor
files produced by other Raster3D utilities.
.PP
.SH EXAMPLES
.PP
To describe a simple bonds-only model coloured by residue type:
.PP
     cat mycolours.pdb protein.pdb | rods | render > mypicture.avs
.PP
To render the same molecule as ball-and-stick:
.PP
     cat mycolours.pdb protein.pdb | rods -b | render > mypicture.avs
.SH OPTIONS

.B "-h"
.PP
Suppress header records in output.  
By default \fIrods\fP will produce an output file which starts with 
header records containing a default set of scaling and processing
options.
The \fI-h\fP flag will suppress these header records so that the output
file contains only sphere descriptors.
This option is usefull for producing files which describe only part
of a scene, and which are to be later combined with descriptor files
produced by other programs.

.B "-b"
.PP
By default \fIrods\fP will describe bonds only; the -b flag will
cause it to include spheres at the atom positions also, yielding
a ball-and-stick representation.  

.SH DESCRIPTION
.PP 
The input to \fIrods\fP consists of a single text file
containing colour information and atomic coordinates in Brookhaven
PDB data bank format.
Coordinates are output as Raster3D descriptor records
with colours and sphere radii assigned according to the COLO
records described below.  
Ball-and-stick figures have atoms drawn
at 0.2 * VanderWaals radius, connected by rods with 0.2A cylindrical radius.
By default the output file contains a set of header records as
required by the \fIrender\fP program.
The header is constructed to include a TMAT matrix corresponding to
the transformation matrix contained in
file \fIsetup.matrix\fP (if it exists), or to the Eulerian angles
contained in file \fIsetup.angles\fP (if it exists).

.PP 
Colours are assigned to atoms using a matching process,
using COLOUR records prepended to the input PDB file.
We have defined a new PDB record type with the same basic
layout as the above but with COLO in the first 4 columns:
 
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
First the SETUP program reads in and remembers all the ATOM, HETA, and
COLO records in input order.  Then it goes through each remembered
ATOM/HETA record in turn, and searches for a COLO record that matches
the ATOM/HETA record in all of columns 7 through 30.  The first such
COLO record to be found determines the colour and radius of the atom.

In order that one COLO record can provide colour and radius
specifications for more than one atom (e.g., based on residue or atom
type, or any other criterion for which labels can be given somewhere in
columns 7 through 30), a "don't-care" character, the hash mark "#"
(pound sign, number sign, sharp sign, octothorpe) is employed.  This
character, when found in a COLO record, matches any character in the
corresponding column in an ATOM or HETA record.  All other characters
must match literally to count as a match.  Note that the very last COLO
record in the input should have hash marks in all of columns 7 through
30 in order to provide a colour for any atom whose ATOM/HETA record
fails to match any previous COLO record.  The idea of matching masks for
colour specifications is due to Colin Broughton.

.SH ENVIRONMENT
.TP 5

The files setup.matrix and setup.angles, if they exist, affect the
header records produced by \fIsetup\fP.

.SH SOURCE
.TP 5
	anonymous ftp site:	
                stanzi.bchem.washington.edu   (128.95.12.38)
.TP 5
	contact:		
                Ethan A Merritt
                Dept of Biological Structure SM-20
                University of Washington, Seattle WA 98195
                merritt@u.washington.edu

.SH SEE ALSO
render(1), ribbon(1), setup(1)
.PP
.SH AUTHORS
	Ethan A Merritt

