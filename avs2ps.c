/*
 * Filter routine to convert a 24-bit color AVS file on stdin
 * to a dithered monochrome PostScript file on stdout.
 * The intended use is to produce black&white pictures from Raster3D,
 * but I see no reason it shouldn't work with any other AVS input file.
 * Ethan A Merritt - Aug 1994
 */
#include	"stdio.h"
#include	"time.h"
#include	"math.h"

#ifdef WIN32
#define random rand
#endif

/*
 * I added a lookup table for warping, since calculating it on the fly
 * was taking about 20% of the CPU time on a DECstation 5000.
 * If you want to play with the warp function, just replace the following
 * define with		define warp(pixval) warpf(pixval)
 * and edit the function warpf() at the end of this file.
 */
#define		warp(pixval)	warpm[pixval]

#define	NO	(0)
#define	YES	(1)

main(argc,argv)
int  argc;
char     *argv[];
{
int		 xsize, ysize;
int		 i, j;
float		 red, green, blue, alpha;
extern float	 warpf();
extern float	 warpm[];
extern void	 dither();
float		*inbuf1, *inbuf2, *tbuf;
int		*index;
int		*done;
unsigned char	*obuf1, *out1;
unsigned char	 obyte;

float   pixinch = 300;		/* default to 300dpi printer resolution */
int	bflag   = NO;		/* default to no border around image */
int	usagerr = NO;
int	ny, nx, nb;
float	x_origin, y_origin, x_scale, y_scale;
time_t	date;

/* Command line options */
    for (i=1; i<argc; i++)
	{
	if (strncmp(argv[i],"-dpi",4) == 0)
	    pixinch = (float)atoi(argv[++i]);
	else if (strncmp(argv[i],"-b",2) == 0)
	    bflag = YES;
	else
	    usagerr = YES;
	}
    if (pixinch <= 72) usagerr = YES;
    if (usagerr)
	{
	fprintf( stderr, 
	    "\nusage:  avs2ps [-dpi xxx] [-b] < infile.avs > outfile.ps\n");
	exit(-1);
	}

/* Read in size of image from first two words on input stream */
	xsize = getw( stdin );
	ysize = getw( stdin );
#ifdef NETWORKBYTEORDER
	xsize = htonl(xsize);
	ysize = htonl(ysize);
#endif

/* Allocate input buffers accordingly */
    inbuf1 = (float *)calloc( xsize, sizeof(float) );
    inbuf2 = (float *)calloc( xsize, sizeof(float) );
    if (inbuf1 == NULL || inbuf2 == NULL)
	exit(-1);

/* Allocate output buffer too */
    nb = (xsize+7) / 8;
    nx = nb * 8;
    ny = ysize;
    obuf1 = (unsigned char *)calloc( nx, sizeof(char) );
    if (obuf1 == NULL)
	exit(-1);

/* Allocate bookkeeping arrays */
    index  = (int *)calloc( xsize, sizeof(int) );
    done   = (int *)calloc( xsize+2, sizeof(int) );
    if (index == NULL || done == NULL)
	exit(-1);
    done++ ;

/* write header of PostScript file */
    x_origin = 36.0;
    y_origin = 36.0;
    x_scale = 72.0 * xsize / pixinch;
    y_scale = 72.0 * ysize / pixinch;
    date = time(0);
    
    printf ("%%!PS-Adobe-2.0\n");
    printf ("%%%%Title: Raster3d picture dithered to monochrome\n");
    printf ("%%%%Creator: %s using avs2ps V2.5\n", 
  	   (char *)getenv("USER"));
    printf ("%%%%CreationDate: %s", ctime(&date));
    printf ("%%%%BoundingBox: %d %d %d %d\n", (int)x_origin,
  	   (int)y_origin, (int)(x_origin + x_scale + 1.0), 
  	   (int)(y_origin + y_scale + 1.0));
    printf ("%%%%Requirements: resolution(%d,%d)\n",(int)pixinch,(int)pixinch);
    printf ("%%%%EndComments\n");
    printf ("initgraphics\n");
    printf ("/picstr %d string def\n", nb);
    printf ("gsave\n");
    printf ("%f %f translate\n", x_origin, (y_origin + y_scale));
    printf ("%f %f scale\n", x_scale, -y_scale);
    printf ("%d %d 1\n", nx, ny);
    printf ("[%d 0 0 %d 0 0]\n", nx, ny);
    printf ("{ currentfile picstr readhexstring pop }\n");
    printf ("image\n");
  

/* Read in first row of image, converting to greyscale image as we go */
    for (i=0; i<xsize; i++)
	{
	alpha = getchar(); 			/* skip alpha byte */
	red   = warp( getchar() ); 
	green = warp( getchar() ); 
	blue  = warp( getchar() ); 
	inbuf1[i] = sqrt( 0.299 * red + 0.587 * green + 0.111 * blue );
	inbuf1[i] = sqrt( inbuf1[i] );
	}

/* Now for grand loop over image lines */
    for (j=1; j<=ysize; j++)
	{
	/* Read next row into inbuf2 */
	if (j != ysize)			/* No read on last+1th row */
	for (i=0; i<xsize; i++)
	    {
	    alpha = getchar(); 			/* skip alpha byte */
	    red   = warp( getchar() ); 
	    green = warp( getchar() ); 
	    blue  = warp( getchar() ); 
	    inbuf2[i] = sqrt( 0.299 * red + 0.587 * green + 0.111 * blue );
	    inbuf2[i] = sqrt( inbuf2[i] );
	    }

	/* Call dithering function to produce pixels for previous line */
	    dither( xsize, inbuf1, inbuf2, obuf1, index, done );
	
	/* Set any extra pixels on right margin to white */
	    for (i=xsize; i<nx; i++)
		obuf1[i] = 1;

	/* Now pack and dump output pixels for PostScript */
	    out1 = obuf1;
	    for (i=0; i<2*nb; i++)
		{
		obyte  = *out1++ << 3; obyte |= *out1++ << 2;
		obyte |= *out1++ << 1; obyte |= *out1++ << 0;
		if ((i%64) == 0) printf("\n");
		printf("%x",obyte);
		}

	/* Swap input buffer pointers in preparation for next row */
	    tbuf = inbuf1;  inbuf1 = inbuf2;  inbuf2 = tbuf;

	}

/* PostScript trailer */
    printf ("\ngrestore\n");
    if (bflag)
	{
   	printf ("newpath %d %d moveto %d %d lineto\n",
		(int)x_origin, (int)y_origin, 
		(int)x_origin, (int)(y_origin+y_scale+1.) );
   	printf ("        %d %d lineto %d %d lineto\n",
		(int)(x_origin+x_scale+1.), (int)(y_origin+y_scale+1.), 
		(int)(x_origin+x_scale+1.), (int)y_origin);
	printf ("closepath stroke\n");
	}
    printf("\nshowpage\n");

exit(0);
}


/*
 * This is a translation of the "new improved" dithering routine taken from
 * hdither1.f.   It is a variation on the theme of Floyd-Steinberg error
 * diffusion, with the wrinkle that the individual columns within a row
 * are traversed in random order.
 * (EAM - Aug 1994)
 * The following partial explaination is taken from comments in hdither.f:
 */
/*
 * What I am doing is choosing the pixels in LINE1 for processing in a
 * shuffled order.  I'm also changing how the error is distributed
 * slightly.  For a pixel in line 1, there can be up to 5 neighbours where
 * the error can be sent.  I take it that neighbouring pixels on the
 * diagonal are worth 1/sqrt(2) as much as vertically or horizontally
 * adjacent pixels.  All three neighbours on line 2 are always available,
 * and either or both of the adjacent pixels on line 1 may be, depending on
 * whether we've already chosen values for them.  As for edge effects, I'm
 * going to continue to throw away errors that I would normally throw at
 * unprocessed pixels, rather than trying to distribute the error "fairly"
 * among the pixels I still have available, because I don't want to end up
 * with strange edge effects arising from the fact that I'm not processing
 * the rasters in a random order!
 */

static float avail[4][5] =	{
		        0.226541, 0.160189, 0.226541, 0.160189, 0.226541,
		        0.000000, 0.207107, 0.292893, 0.207107, 0.292893,
		        0.292893, 0.207107, 0.292893, 0.207107, 0.000000,
		        0.000000, 0.292893, 0.414214, 0.292893, 0.000000 
			};


void
dither( nx, line1,  line2, pixels, index,  done )
int   		 nx;
float		*line1, *line2;
unsigned char	*pixels;
int		*index, *done;
{
    int 	i, j, k;
    int 	it;
    int 	iavail;
    float	error;
    float	propag[5];

    /* pick a random order in which to process pixels from this row */
	for (i=0; i<nx; i++)
	    {
	    index[i] = i;
	    done[i]  = NO ;
	    }
	done[-1] = done[nx] = NO ;

	for (i=0; i<nx; i++)
	    {
	    j  = random() % nx ;
	    it = index[i] ;
	    index[i] = index[j] ;
	    index[j] = it ;
	    }
	
    /* Now dither them, keeping track of which ones have been done */
	for (i=0; i<nx; i++)
	    {
	    k = index[i];
	    iavail = 0;
	    if (done[k-1]) iavail += 1;
	    if (done[k+1]) iavail += 2;
	    done[k] = YES ;

	    /* set output pixel to either 0 or 1 */
	    if (line1[k] > 0.5)
		pixels[k] = 1, error = line1[k] - 1.0 ;
	    else
		pixels[k] = 0, error = line1[k] - 0.0 ;
	    if (fabs(error) < 1.e-9)
		continue ;
	    for (j=0; j<5; j++)
		propag[j] = avail[iavail][j] * error ;
          
	    if (k > 0)
		{
		line1[k-1] = line1[k-1] + propag[0] ;
		line2[k-1] = line2[k-1] + propag[1] ;
		}
	    line2[k]       = line2[k]   + propag[2] ;
	    if (k < nx-1)
		{
		line1[k+1] = line1[k+1] + propag[3] ;
		line2[k+1] = line2[k+1] + propag[4] ;
		}
	    }

}

/*
 * This is the warping function which hdither.f was using.
 * It's computationally expensive (applied 3 times per pixel),
 * so I dumped it into a lookup table for speed.
 */
float warpf( rgbval )
unsigned char rgbval;
{
float temp;

    temp = (float)rgbval / 255. ;
    temp = (((0.533 * temp) - 1.657) * temp + 2.124) * temp ;
    temp *= 1.01;		/* Used to be 1.10 */
    temp = (temp < 0.) ? 0. :
	   (temp > 1.) ? 1. :
	    temp * temp ;
    return( temp );
}

float warpm[256] =
 {
 0.000000, 0.000070, 0.000280, 0.000625, 0.001105, 0.001716, 0.002456, 0.003322,
 0.004313, 0.005425, 0.006656, 0.008005, 0.009468, 0.011043, 0.012729, 0.014522,
 0.016422, 0.018425, 0.020529, 0.022732, 0.025033, 0.027429, 0.029918, 0.032498,
 0.035167, 0.037923, 0.040765, 0.043689, 0.046695, 0.049780, 0.052943, 0.056182,
 0.059495, 0.062880, 0.066335, 0.069859, 0.073450, 0.077106, 0.080826, 0.084608,
 0.088451, 0.092352, 0.096310, 0.100324, 0.104392, 0.108512, 0.112684, 0.116905,
 0.121175, 0.125491, 0.129852, 0.134258, 0.138706, 0.143195, 0.147724, 0.152291,
 0.156896, 0.161537, 0.166212, 0.170921, 0.175662, 0.180434, 0.185236, 0.190067,
 0.194926, 0.199810, 0.204721, 0.209655, 0.214613, 0.219592, 0.224593, 0.229614,
 0.234653, 0.239711, 0.244786, 0.249877, 0.254983, 0.260104, 0.265237, 0.270383,
 0.275541, 0.280710, 0.285888, 0.291075, 0.296271, 0.301473, 0.306683, 0.311898,
 0.317119, 0.322343, 0.327572, 0.332803, 0.338036, 0.343271, 0.348507, 0.353743,
 0.358979, 0.364213, 0.369446, 0.374676, 0.379904, 0.385128, 0.390347, 0.395563,
 0.400773, 0.405977, 0.411175, 0.416367, 0.421551, 0.426728, 0.431896, 0.437056,
 0.442207, 0.447348, 0.452479, 0.457600, 0.462711, 0.467810, 0.472897, 0.477973,
 0.483037, 0.488087, 0.493125, 0.498150, 0.503161, 0.508159, 0.513142, 0.518110,
 0.523064, 0.528003, 0.532927, 0.537835, 0.542728, 0.547604, 0.552464, 0.557308,
 0.562136, 0.566946, 0.571739, 0.576516, 0.581275, 0.586016, 0.590740, 0.595446,
 0.600134, 0.604804, 0.609456, 0.614089, 0.618705, 0.623301, 0.627880, 0.632439,
 0.636980, 0.641502, 0.646005, 0.650489, 0.654955, 0.659401, 0.663828, 0.668237,
 0.672626, 0.676996, 0.681347, 0.685680, 0.689993, 0.694287, 0.698562, 0.702818,
 0.707055, 0.711273, 0.715473, 0.719654, 0.723815, 0.727959, 0.732083, 0.736190,
 0.740277, 0.744347, 0.748398, 0.752431, 0.756446, 0.760443, 0.764422, 0.768383,
 0.772327, 0.776254, 0.780163, 0.784055, 0.787930, 0.791788, 0.795629, 0.799454,
 0.803262, 0.807054, 0.810830, 0.814590, 0.818335, 0.822064, 0.825777, 0.829476,
 0.833159, 0.836828, 0.840482, 0.844122, 0.847748, 0.851360, 0.854958, 0.858543,
 0.862114, 0.865673, 0.869219, 0.872752, 0.876273, 0.879782, 0.883280, 0.886766,
 0.890240, 0.893704, 0.897157, 0.900599, 0.904032, 0.907454, 0.910867, 0.914270,
 0.917665, 0.921051, 0.924428, 0.927797, 0.931158, 0.934512, 0.937858, 0.941197,
 0.944530, 0.947856, 0.951177, 0.954491, 0.957801, 0.961105, 0.964404, 0.967699,
 0.970990, 0.974277, 0.977561, 0.980842, 0.984120, 0.987396, 0.990669, 0.993941,
 0.997212, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000
 };
