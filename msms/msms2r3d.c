/*
 * msms2r3d
 *
 *	Convert output files from molecular surface generation program msms
 * (written by Michael Sanner, Scripps Institute) into a Raster3D description
 * file.
 *	Msms creates paired files, xxx.vert and xxx.face
 * The *.vert file contains all the vertex coordinates and normals.
 * The *.face file contains triples of indices into the list of vertices;
 * each triple describes the corners of a triangle.  The entire set of
 * triangles so described constitues a mesh description of the molecular
 * surface.
 */
#include <stdio.h>
#include <math.h>

main()
{
FILE *vertfile, *facefile;
FILE *atomfile, *r3dfile;
char vertname[64], facename[64];
char atomname[64], r3dname[64];
char line[256], line1[256], line2[256], line3[256];
int  colorflag;

typedef struct 
	{
	float x, y, z;
	float xnorm, ynorm, znorm;
	float red, green, blue;
	int   nearest_atom;
	} VERTEX ;
VERTEX *vertex;

typedef struct
	{
	float x, y, z;
	float radius;
	float q;
	} ATOM ;
ATOM    *atom;

int	nvert, nface, natoms;
int	i;
int	t1, t2, t3;
int	ierr, junk;
int	iatom;
float	red, green, blue;
float	q, qmin, qmax;

/* Open input files */
    printf("\nFile containing vertices: ");
    scanf("%s", vertname);
    vertfile = fopen(vertname,"r");
    if (!vertfile) 
    	{
    	fprintf(stderr,"\nCannot open file %s\n",vertname);
	exit(-1);
	}

    printf("File containing faces   : ");
    scanf("%s", facename);
    facefile = fopen(facename,"r");
    if (!facefile) 
    	{
    	fprintf(stderr,"\nCannot open file %s\n",facename);
	exit(-1);
	}

    printf("Raster3D output file    : ");
    scanf("%s", r3dname);
    r3dfile = fopen(r3dname,"w");
    if (!r3dfile) 
    	{
    	fprintf(stderr,"\nCannot open file %s\n",r3dname);
	exit(-1);
	}


/* Have a look at the header records in the vertex file.          */
/* I suspect that the 2nd line should be parsed to determine the  */
/* actual file contents, but the current documentation doesn't    */
/* indicate what other items might be on the line.  So for now    */
/* I'll assume that it must contain the number of vertices as the */
/* first item on the line.                                        */
    fgets( line1, sizeof(line1), vertfile );
    printf("\n%s", line1);
    fgets( line2, sizeof(line2), vertfile );
    printf("%s", line2);
    fgets( line3, sizeof(line3), vertfile );
    printf("%s", line3);
    sscanf( line3, "%d %d", &nvert, &natoms );

    vertex = (VERTEX *)calloc( nvert+1, sizeof(VERTEX) );
    printf("Reading %d vertices from %s\n", nvert, vertname);
    for (i=1; i<=nvert; i++)
        {
    	fgets( line, sizeof(line), vertfile );
    	ierr = sscanf( line, "%f %f %f %f %f %f %d %d %d",
		&(vertex[i].x),&(vertex[i].y),&(vertex[i].z),
		&(vertex[i].xnorm),&(vertex[i].ynorm),&(vertex[i].znorm),
		&junk, &vertex[i].nearest_atom, &junk );
	if (ierr != 9) 
	    {
	    fprintf(stderr,"Error reading vertex %d",i);
	    exit(-1);
	    }
	}
    fclose(vertfile);

/* May need another file also, to generate coloring.       */
/* Note that I am assuming one more quantity per line than */
/* is described in the documentation for thie file.        */
/* This means that the pdb_to_xyzr script must be modified */
/* accordingly.                                            */
    printf("\nDo you want to color the surface? [y/N] ");
    scanf("%s",line);
    colorflag = (line[0] == 'Y' || line[0] == 'y');
    if (colorflag)
    	{
	qmin =  999999.;
	qmax = -999999.;

	printf("\nFile containing sphere (atom) centers and coloring information: ");
	scanf("%s", atomname);
	atomfile = fopen(atomname,"r");
	if (!atomfile)
	    {
	    fprintf(stderr,"\nCannot open file %s\n",atomname);
	    exit(-1);
	    }
	atom = (ATOM *)calloc( natoms+1, sizeof(ATOM) );
	printf("Reading %d atom centers from %s\n", natoms, atomname);
	for (i=1; i<=natoms; i++)
	    {
	    fgets( line, sizeof(line), atomfile );
	    ierr = sscanf( line, "%f %f %f %f %f",
	    	&(atom[i].x), &(atom[i].y), &(atom[i].z),
		&(atom[i].radius), &(atom[i].q) );
	    if (ierr != 5) 
	    	{
		fprintf(stderr, "Error reading atom %d", i);
		exit(-1);
		}
	    if (atom[i].q < qmin) qmin = atom[i].q;
	    if (atom[i].q > qmax) qmax = atom[i].q;
	    }
	fclose(atomfile);
	printf("Qmin = %8.3f\tQmax = %8.3f\n\n",qmin,qmax);
	}

/* Now we actually assign the colors for each vertex, based on the */
/* nearest atomic center.  This is not really all that great.      */
/* It would be better to take the coordinates of the vertex and    */
/* look up or caculate the coloring quantity directly, but that    */
/* will have to wait for another day.                              */
    if (colorflag)
    	{
	for (i=1; i<=nvert; i++)
	    {
	    iatom = vertex[i].nearest_atom;
	    q     = atom[iatom].q;
	    if (q > 0) 
	    	{
		vertex[i].red   =  0.454545 * (1.2 + q/qmax);
		vertex[i].green =  0.454545 * (1.2 - q/qmax);
		vertex[i].blue  =  0.454545 * (1.2 - q/qmax);
		}
	    else
	    	{
		vertex[i].red   =  0.454545 * (1.2 - q/qmin);
		vertex[i].green =  0.454545 * (1.2 - q/qmin);
		vertex[i].blue  =  0.454545 * (1.2 + q/qmin);
		}
	    vertex[i].red   = vertex[i].red   * vertex[i].red ;
	    vertex[i].green = vertex[i].green * vertex[i].green ;
	    vertex[i].blue  = vertex[i].blue  * vertex[i].blue ;
	    }
	}

/* So far, so good.  Initialize Raster3D output file.*/
    fprintf( r3dfile, "# Surface description converted by program msms2r3d\n");
    fprintf( r3dfile, "#%s", line1 );
    fprintf (r3dfile, "# \n" );

/* Since I don't yet know how to pick up colors, set them all to white */
    red = green = blue = 0.8 ;

/* Similar treatment of face file, except that as we go we convert  */
/* each line into a single Raster3D TRIANGLE descriptor with        */
/* explicit vertex normals.                                         */
/* The next step is obviously explicit colors, but I don't yet know */
/* where to pick them up from.                                      */
    fgets( line1, sizeof(line1), facefile );
    printf("%s", line1);
    fgets( line2, sizeof(line2), facefile );
    printf("%s", line2);
    fgets( line3, sizeof(line3), facefile );
    printf("%s", line3);
    sscanf( line3, "%d", &nface );

    printf("Reading %d faces from %s\n", nface, facename);
    for (i=0; i<nface; i++)
    	{
	fgets( line, sizeof(line), facefile );
	ierr = sscanf( line, "%d %d %d", &t1, &t2, &t3 );
	if (ierr != 3)
	    {
	    fprintf(stderr, "Error reading face %d",i);
	    exit(-1);
	    }
	fprintf( r3dfile, "1\n" );
	fprintf( r3dfile, 
	    "%9.3f %9.3f %9.3f  %9.3f %9.3f %9.3f  %9.3f %9.3f %9.3f",
	    vertex[t1].x, vertex[t1].y, vertex[t1].z,
	    vertex[t2].x, vertex[t2].y, vertex[t2].z,
	    vertex[t3].x, vertex[t3].y, vertex[t3].z );
	fprintf( r3dfile, " %4.2f %4.2f %4.2f\n", red, green, blue );
	fprintf( r3dfile, "7\n" );
	fprintf( r3dfile, 
	    "%9.3f %9.3f %9.3f  %9.3f %9.3f %9.3f  %9.3f %9.3f %9.3f\n",
	    vertex[t1].xnorm, vertex[t1].ynorm, vertex[t1].znorm,
	    vertex[t2].xnorm, vertex[t2].ynorm, vertex[t2].znorm,
	    vertex[t3].xnorm, vertex[t3].ynorm, vertex[t3].znorm );
	if (!colorflag) continue;
	fprintf( r3dfile, "17\n" );
	fprintf( r3dfile, 
	    "%9.3f %9.3f %9.3f  %9.3f %9.3f %9.3f  %9.3f %9.3f %9.3f\n",
	    vertex[t1].red, vertex[t1].green, vertex[t1].blue,
	    vertex[t2].red, vertex[t2].green, vertex[t2].blue,
	    vertex[t3].red, vertex[t3].green, vertex[t3].blue );
	}
    
    fprintf( r3dfile, "# End of molecular surface from\n" );
    fprintf( r3dfile, "#%s", line1 );


		

}

