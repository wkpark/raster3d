#!/usr/bin/perl -w

use strict 'vars';

my ($msms_face, $msms_vert, $r3d, $red, $green, $blue);
my ($line, @words, $count, $max_count);
my (@vert_x, @vert_y, @vert_z);
my (@norm_x, @norm_y, @norm_z);
my ($v1, $v2, $v3, $type);

if ($#ARGV < 1) {
	print "Usage: msms2r3d msms_face msms_vert > surfacefile.r3d\n",
	exit(-1);
}

$red   = 0.866;
$green = 0.866;
$blue  = 0.866;

$msms_face = $ARGV[0];
$msms_vert = $ARGV[1];

# read vertices file and store all the vertices in a large array containing
# the xyz for the vertices and the xyz for the normals
open VERT, "<$msms_vert" or die "File not found: $msms_vert\n";

# skip the first 3 lines of the vertices file
$count = -3;

while ( $line = <VERT> ) {
	# increment the line counter
	$count++;

	# keep reading lines until the counter reaches 1
	next if ($count < 1);

	@words = split ( ' ', $line);

	# the first 3 columns have the x,y,z for the vertex
	$vert_x[$count] = $words[0];
	$vert_y[$count] = $words[1];
	$vert_z[$count] = $words[2];
	
	# the next 3 columns have the normals for each vertex
	$norm_x[$count] = $words[3];
	$norm_y[$count] = $words[4];
	$norm_z[$count] = $words[5];
}

$max_count = $count;

close VERT;


# now read the face file and reconstruct all the triangles into 
# the raster3d output file stream

open FACE, "<$msms_face" or die "File not found: $msms_face\n";

$count = -3;   # skip the 3 lines of the header

while ($line = <FACE>) {
	# increment the counter
	$count++;
	
	# skip lines until this reaches 1
	next if ($count < 1);

	@words = split (' ', $line);

	$v1 = $words[0];
	$v2 = $words[1];
	$v3 = $words[2];
	$type = $words[3];

	# make sure all vertices are within the range
	next if ($v1 > $max_count or $v2 > $max_count or $v3 > $max_count);

	# now output the coords and normals to the raster3d file

	# first, print xyz xyz xyz for three vertices of the triangle
	print "1\n";
	print "$vert_x[$v1] $vert_y[$v1] $vert_z[$v1] ";
	print "$vert_x[$v2] $vert_y[$v2] $vert_z[$v2] ";
	print "$vert_x[$v3] $vert_y[$v3] $vert_z[$v3] ";
	print "$red $green $blue\n";

	# next, print the normals to each vertex of the triangle
	print "7\n";
	print "$norm_x[$v1] $norm_y[$v1] $norm_z[$v1] ";
	print "$norm_x[$v2] $norm_y[$v2] $norm_z[$v2] ";
	print "$norm_x[$v3] $norm_y[$v3] $norm_z[$v3]\n";

}

close FACE;

exit(1);
