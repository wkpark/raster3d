.ad l
.nh
.TH RIBBON 1 "23 November 1993" "Raster3D"
.SH NAME

ribbon - preprocessor for ribbon models in Raster3D molecular graphics package
.SH SYNOPSIS

   "ribbon" [\fI-h\fP] [\fI-d[012345]\fP] pdbfile
 or
   "ribbon" [\fI-h\fP] \fI-d[012345]\fP -     (to take PDB records from stdin)

.PP
\fIRibbon\fP reads a Brookhaven PDB coordinate file and 
produces a file on stdout containing Raster3D descriptor
records for a ribbon representation constructed from a triangular mesh.
The file produced by \fIribbon\fP
may be fed directly to \fIrender\fP or it may be combined with descriptor
files produced by other Raster3D utilities.
.PP
.SH EXAMPLES
.PP
To describe a the entire protein chain as a single ribbon colored
smoothly from blue at the N-terminus to red at the C-terminus:
.PP
     ribbon -d2 protein.pdb | render > chain_picture.avs
.PP
To color a multi-chain protein with specified colors for each chain:
.PP
     cat chaincolors.pdb protein.pdb | ribbon -d5 - > chains.r3d
.SH OPTIONS

.B "-h"
.PP
Suppress header records in output.  
By default \fIribbon\fP will produce an output file which starts with 
header records containing a default set of scaling and processing
options.
The \fI-h\fP flag will suppress these header records so that the output
file contains only triangle descriptors.
This option is useful for producing files which describe only part
of a scene, and which are to be later combined with descriptor files
produced by other programs.

.B "-d[012345]"
.PP
By default \fIribbon\fP requires interactive input to select
ribbon parameters and coloring information.  Five default coloring
schemes are implemented, however, and these may be selected as a
command line option to bypass any interactive input.
     -d or -d0	same as -d2 below
     -d1 	solid color ribbon (defaults to blue)
     -d2	shade from blue at N-terminus to red at C-terminus
     -d3	one surface of ribbon is blue, other surface is grey
     -d4	shade front surface from blue to red, back surface is grey
     -d5	color separate chains using successive color cards from
		input stream. Note that pattern matching on the color
		records is _not_ done; colors are simply taken sequentially
		as new chains are encountered.

.SH DESCRIPTION
.PP 
The input to \fIribbon\fP consists of a single text file
containing colour information [optional] and atomic coordinates in Brookhaven
PDB data bank format. 
Only CA and carbonyl O atom records are required;
all other input atoms are ignored.
Ribbon parameters and colouring specified interactively when the
program is run.
Keyboard interaction may be bypassed by selecting one of the default
colouring schemes using the -d flag.
A triangular mesh ribbon is output as Raster3D descriptor records.
By default the output file contains a set of header records as
required by the \fIrender\fP program.
The header is constructed to include a TMAT matrix corresponding to
the transformation matrix contained in
file \fIsetup.matrix\fP (if it exists), or to the Eulerian angles
contained in file \fIsetup.angles\fP (if it exists).

\fIRibbon\fP produces a continuous smooth trace of the protein backbone.
For more complicated representations of protein secondary structure it
is better to use a different program, e.g. MOLSCRIPT, rather than \fIribbon\fP.
.SH ENVIRONMENT
.PP
The files setup.matrix and setup.angles, if they exist, affect the
header records produced by \fIribbon\fP.

.SH SOURCE
.TP 5
	anonymous ftp site:	
                ftp.bmsc.washington.edu
	web URL:
		http://www.bmsc.washington.edu/raster3d/raster3d.html
.TP 5
	contact:		
                Ethan A Merritt
                Dept of Biological Structure Box 357742
                University of Washington, Seattle WA 98195
                merritt@u.washington.edu

.SH SEE ALSO
MOLSCRIPT(1), render(1), rods(1), balls(1)
.PP
.SH AUTHORS

 Original ribbon code written by Phil Evans for the CCP4
 version of FRODO.  Modification to describe solid ribbons
 as triangular mesh for Phong shading in Raster3D package 
 by Ethan A Merritt.

