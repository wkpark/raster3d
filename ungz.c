/*
 * Uncompress a raster3d input file into a temporary file
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __hpux
#define ungz_  ungz
#endif

int ungz_( origname,  tempname )
char  *origname, *tempname;
{
char   command[128];
char  *t;
int   ierr;

#ifdef GUNZIP
    t = tempnam( NULL, "R3D%%" );
    sprintf( command, "gunzip -c %s > %s", origname, t );
    ierr = system( command );
    strcpy( tempname, t );
#else
    fprintf(stderr," >> sorry, no decompression support\n");
    ierr = -1;
#endif

return(ierr);
}
