/*
 * Raster3D V2.0
 * local.c
 *
 * Output from render.f is performed by calls to routine LOCAL,
 * which is implemented here.
 *
 * This version of local.c supports 3 output modes -
 *	AVS image file sent to stdout
 *		(2 integer header followed by AlphaRGB bytes)
 *	original Raster3D file to stdout
 *              (8 integer header followed by RGB bytes)
 *	calls to the libimage library (SGI implementation only)
 *
 * Note on conditional code for SGI:
 * This version of local.c uses Paul Haeberle's libimage library
 * distributed in 4DGifts for SGI workstations. If libimage.a is
 * not installed on your system you must first build it from the
 * source in 4DGifts/iristools/libimage.
 * If no output file is specified, output will go to a file
 * "render.rgb". As far as I know it is not possible to direct
 * libimage format files to stdout.
 * Output images may be viewed using the "ipaste" command, and
 * manuipulated using various other utilities provided under
 * 4DGifts/iristools/imgtools.
 */

#include	<stdio.h>
#include	<fcntl.h>
#include	<string.h>

#ifdef LIBIMAGE_SUPPORT
#include	<gl/image.h>
#endif

local_(option,buffer1,buffer2,buffer3)
    int		*option;
    short	*buffer1, *buffer2, *buffer3;
    {
    /* Everyone needs these */
    static int		xsize, ysize;
    static int		mode;
    int			i;

    /* For -original output mode only */
    static int header[8] = { 3, 1, 1, 0, 0, 0, 0, 0 };
    char  *c = (void *)header;

#ifdef LIBIMAGE_SUPPORT
    /* For -sgi output mode only */
    static IMAGE	*image;
    static char		*ofile;
    static int		 row;
#endif
   
/*
 * First call (option=0) is to determine the output mode
 */
    if (*option == 0)
	{
	if (strncmp( (char *)buffer1, "-sgi" , 4) == 0)
	    {
#ifdef      LIBIMAGE_SUPPORT
	    mode  = 2;
	    ofile = (char *)buffer2;
#else
	    fprintf(stderr,"\n This version of render does not support -sgi\n");
	    exit(-1);
#endif
	    }
	else if (strncmp( (char *)buffer1, "-orig", 5) == 0)
	    {
	    mode  = 1;
	    }
	else
	    mode  = 0;
	}

/*
 * Subsequent calls are treated differently depending on output mode
 */
    else switch (*option)
	{
	case 1:	/* open image file for output */
	    xsize = *buffer1;
	    ysize = *buffer2;
#ifdef      LIBIMAGE_SUPPORT
	    if (mode == 2)
		{
		if (*ofile != ' ')
		    ofile = strtok( ofile, " " );
		else
		    ofile = "render.rgb";
		image = iopen(ofile,"w",RLE(1),3,xsize,ysize,3);
		row = 0;
		}
	    else
#endif
	    if (mode == 1)
		{
		header[3] = xsize;
		header[4] = ysize;
		for (i=0; i<sizeof(header); i++)
		    putchar(*c++);
		}
	    else
		{
		putw( xsize, stdout );
		putw( ysize, stdout );
		}
	    break;

	case 2:	/* write RGB values to image file */
	    /* -sgi option (libimage format) */
#ifdef      LIBIMAGE_SUPPORT
	    if (mode == 2)
		{
		putrow(image,buffer1,row,0);
		putrow(image,buffer2,row,1);
		putrow(image,buffer3,row,2);
		row++;
		}
	    else
#endif
	    /* original RGB bytes */
	    if (mode == 1)
		{
		for (i=0; i<xsize; i++)
		    {
		    putchar( buffer1[i] );
		    putchar( buffer2[i] );
		    putchar( buffer3[i] );
		    }
		}
	    /* AVS image file (AlphaRGB) bytes */
	    else
		{
		for (i=0; i<xsize; i++)
		    {
		    putchar( 0 );
		    putchar( buffer1[i] );
		    putchar( buffer2[i] );
		    putchar( buffer3[i] );
		    }
		}
	    break;

	case 3:	/* close up shop */
#ifdef      LIBIMAGE_SUPPORT
	    if (mode == 2)
		iclose(image);
#endif
	    break;

	case 4:	/* load title into image file */
#ifdef      LIBIMAGE_SUPPORT
	    if (mode == 2)
		isetname(image,buffer1);
#endif
	    break;

	}

    return( mode );
    }
