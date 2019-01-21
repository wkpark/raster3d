/*
 * Raster3D V3.0
 * local.c
 *
 * Output from render.f is performed by calls to routine LOCAL.
 *
 * This version of local.c supports 6 output modes, 5 of which
 * are controlled by conditional compilation directives.
 *
 *	mode 0	AVS image file sent to stdout
 *		(2 integer header followed by AlphaRGB bytes)
 *		Alpha byte is set to 255 if no explicit alpha
 *		values are passed from the caller
 *
 *	mode 1	original, long-obsolete, private format
 *
 *	mode 2	#ifdef LIBIMAGE_SUPPORT
 *		calls to the libimage library if LIBIMAGE_SUPPORT is defined
 *		(probably only makes sense on an SGI machine)
 *
 *	mode 3	#ifdef TIFF_SUPPORT
 *		calls to the TIFF library if TIFF_SUPPORT is defined
 *
 *	mode 4	#ifdef GD_SUPPORT
 *		JPEG image output to file (defaults to stdout)
 *
 *	mode 5	#ifdef GD_SUPPORT
 *		libgd PNG image output to file (defaults to stdout)
 *
 * Command line switches other than output file format handled elsewhere
 * here we recognize
 *		-invert	         invert y coordinate axis
 *		-jpeg [filename] for jpeg output
 *		-png  [filename] for png output
 *		-avs  [filename] for AVS output
 *		-sgi  [filename] SGI libimage format output
 *		-tiff [filename] TIFF output format
 */

#include	<stdio.h>
#include	<fcntl.h>
#include	<string.h>
#include	<ctype.h>
#include	<time.h>
#include	<stdlib.h>
#ifdef NETWORKBYTEORDER
#include	<netinet/in.h>
#endif

#ifdef LIBIMAGE_SUPPORT
#include	<gl/image.h>
#endif

#ifdef TIFF_SUPPORT
#include        <tiff.h>
#include        <tiffio.h>
#if defined(LINUX) || defined(OSX)
#include	<signal.h>
#endif
#endif /* TIFF_SUPPORT */

#if defined GD_SUPPORT
#include "gd.h"
#endif

/* Pick up version number from same include file used by Makefile */
char *
#include "VERSION"
;

/* Define bits in returned status */
#define		ANTIALIAS	007
#define		INVERT		010
#define		DEBUGGING	020

/* Define bits passed in 3rd parameter of mode 1 */
#define		ALPHACHANNEL	040

int		alpha_channel = 0;

/* Loaded by mode 5; used to initialize image file by mode 1 */
static int bkg_r = 0, bkg_g = 0, bkg_b = 0;

/* Define bits to be pass to both the local_() and addlabel_() functions */
static int xsize = 0, ysize = 0;
static int mode = -1; /* this is needed by goth local_() and addlabel_() */
#if defined GD_SUPPORT
  /* For -png OR -jpeg (using libgd) output option only */
  static gdImagePtr molecule_img = NULL;
  static gdImagePtr label_img = NULL;
#endif

/* HPUX lacks Fortran intrinsic functions AND and OR for some reason, */
/* so I put a copy here. On the other hand HPUX has an unusually sane */
/* calling convention for Fortran subroutine names.                   */
#if defined(__hpux)
#define local_ local
int and(i,j) int *i,*j; {return (*i & *j);}
int or(i,j)  int *i,*j; {return (*i | *j);}
#endif

#if defined(gfortran)
int and_(i,j) int *i,*j; {return (*i & *j);}
int or_(i,j)  int *i,*j; {return (*i | *j);}
#endif

size_t trimwhitespace(char *out, size_t len, const char *str);

int local_(option,buffer1,buffer2,buffer3,buffer4)
     int	*option;
     short	*buffer1, *buffer2, *buffer3, *buffer4;
{
  
  /* Everyone needs these */
  int	        i;
  static char	*ofile;
  int		status = 0;
  int		invert = 0;
  int		bits;
  static int	quality = 90;
 
  static time_t	start_time, end_time;
  static char program_name[20] = "Raster3D         G";
  
  /* For -original output mode only */
  static int header[8] = { 3, 1, 1, 0, 0, 0, 0, 0 };
  char  *c = (void *)header;
  
#ifdef LIBIMAGE_SUPPORT
  /* For -sgi output mode only */
  static IMAGE	*image;
  static int	row=0;
#endif
  
#ifdef TIFF_SUPPORT
  /* For -tiff output option only */
  static TIFF   *tfile;
  static unsigned char *scanline;
  unsigned short  rows_per_strip;
  void my_write_tiff();
#endif


/****************************************************************/
/* The action taken by this subroutine is determined by the	*/
/* option parameter. The first call (option=0) determines the 	*/
/* output mode.							*/
/* As of V2.2.1 multiple bits may be set in the value returned.	*/
/****************************************************************/
if (*option == 0) 
    {
    strncpy( &program_name[9], VERSION, strlen(VERSION)+1 );
    
    if (strncmp( (char *)buffer1, "-invert", 7) ==0)
      {
      invert = !invert;
      buffer1 = buffer2;   buffer2 = buffer3;   buffer3 = buffer4;
      }

    if (strncmp( (char *)buffer1, "-tiff", 5) == 0)
      {
#ifdef TIFF_SUPPORT
	mode  = 3;
	ofile = (char *)buffer2;
#else
	fprintf(stderr,
		"\n This copy of render was not built with tiff support\n");
	exit(-1);
#endif
#ifndef TIFF_INVERT
	invert = !invert;
#endif
      }

    /* Version 2.7a - AVS used to be the default, but no longer */
    else if (strncmp( (char *)buffer1, "-avs" , 4) == 0) {
	mode = 0;
    }

    else if (strncmp( (char *)buffer1, "-sgi" , 4) == 0)
      {
#ifdef LIBIMAGE_SUPPORT
	mode  = 2;
	ofile = (char *)buffer2;
	invert = !invert;
#else
	fprintf(stderr,
		"\n This copy of render was not built with sgi libimage support\n");
	exit(-1);
#endif
      }

    else if (strncmp( (char *)buffer1, "-jpeg" , 5) == 0)
      {
#ifdef GD_SUPPORT /* JPEG */
	mode  = 4;
	ofile = (char *)buffer2;
#else
	fprintf(stderr,
		"\n This copy of render was not built with libgd jpeg support\n");
	exit(-1);
#endif
      }

    else if (strncmp( (char *)buffer1, "-png" , 4) == 0)
      {
#ifdef GD_SUPPORT /* PNG */
	mode  = 5;
	ofile = (char *)buffer2;
#else
	fprintf(stderr,
		"\n This copy of render was not built with libgd png support\n");
	exit(-1);
#endif
      }

    /* This option is long since deprecated */
    else if (strncmp( (char *)buffer1, "-orig", 5) == 0)
      {
      	mode = 1;
	invert = !invert;
      }
    else if (strncmp( (char *)buffer1, "  ", 2) != 0)
      {
	fprintf(stderr, "\n%s",program_name);
      	if (strncmp( (char *)buffer1, "-help", 5) != 0)
	    fprintf(stderr, "\n Unfamiliar switch: %12.12s", (char *)buffer1);
	fprintf(stderr, "\n\n Usage:");
	fprintf(stderr, "\n   input from stdin; output mode controlled from command line \n");
	fprintf(stderr,
		"\n     render [-png [outfile]]       PNG image to stdout (default) or file");
	fprintf(stderr,
		"\n     render -jpeg [outfile]        JPEG image to stdout (default) or file");
	fprintf(stderr,
		"\n     render -avs                   AVS image to stdout");
#ifdef LIBIMAGE_SUPPORT
	fprintf(stderr,
		"\n     render -sgi  [outfile]        output to SGI libimage file (defaults to render.rgb)");
#endif
#ifdef TIFF_SUPPORT
	fprintf(stderr,
		"\n     render -tiff [outfile]        output to TIFF file (defaults to render.tif)");
#endif
	fprintf(stderr,"\n");
	fprintf(stderr,"\n Options:");
	fprintf(stderr,"\n   these over-ride contents of input stream header records \n");
	fprintf(stderr,"\n    -aa                   anti-aliasing (SCHEME 4)");
	fprintf(stderr,"\n    -alpha                alpha channel in output image (SCHEME 0)");
	fprintf(stderr,"\n    -bg white|black|<col> set background color (<col> is hex #RRGGBB)");
	fprintf(stderr,"\n    -debug                verbose output while running");
	fprintf(stderr,"\n    -draft                no anti-aliasing (SCHEME 1)");
	fprintf(stderr,"\n    -fontscale FF         multiplier for libgd font size [default 1.0]");
	fprintf(stderr,"\n    -gamma GG             gamma correction applied to output image");
	fprintf(stderr,"\n    -invert               invert y axis");
	fprintf(stderr,"\n    -labels               pass labels to libgd to composite with molecular image");
	fprintf(stderr,"\n    -quality QQ           0 < QQ < 95  jpeg compression [default 90]");
	fprintf(stderr,"\n    -[no]shadow           enable or disable shadowing");
	fprintf(stderr,"\n    -size HHHxVVV         specify size of output image in pixels");
	fprintf(stderr,"\n    -transparent          same as -alpha (SCHEME 0)");
	fprintf(stderr,"\n    -zoom ZZ[%%]           rescale image by ZZ      ");
	fprintf(stderr,"\n");
	exit(-1);
      }

    else /* default to png */
      {
#ifdef GD_SUPPORT /* PNG */
	mode  = 5;
	ofile = (char *)buffer2;
#else
	fprintf(stderr,
		"\n This copy of render was not built with libgd png support\n");
	fprintf(stderr,
		"Defaulting to AVS instead\n");
	mode  = 0;
#endif
      }


    if (invert) status |= INVERT;

    return( status );
  }
  
/****************************************************************/
/* Subsequent calls are treated differently depending on mode	*/
/****************************************************************/
if (mode < 0) 
    {
    fprintf(stderr,"\n Output mode not set before output request\n");
    exit(-1);
    }
  
/****************************************************************/
/* Open output file and initialize image descriptor information	*/
/****************************************************************/
else if (*option == 1)
    {
    xsize   = *(int *)buffer1;
    ysize   = *(int *)buffer2;
    bits    = *(int *)buffer3;
    quality = *(int *)buffer4;

    if (bits & ALPHACHANNEL) alpha_channel = 1;
      
    if (mode == 0)	/* avs */
	{
#ifdef NETWORKBYTEORDER
	putw( htonl(xsize), stdout );
	putw( htonl(ysize), stdout );
#else
	putw( xsize, stdout );
	putw( ysize, stdout );
#endif
	}
     
    if (mode == 1)   /* original */
	{
	header[3] = xsize;
	header[4] = ysize;
	for (i=0; i<sizeof(header); i++)
	    putchar(*c++);
	}
    else

#ifdef LIBIMAGE_SUPPORT
    if (mode == 2)   /* sgi rgb mode */
	{
	if (*ofile != ' ')
	    ofile = strtok( ofile, " " );
	else
	    ofile = "render.rgb";
	    image = iopen(ofile,"w",RLE(1),3,xsize,ysize,alpha_channel?4:3);
	}
    else
#endif
	  
#ifdef TIFF_SUPPORT
#define TIFFSET(A,B,C) if (!(TIFFSetField(A,B,C))) fprintf(stderr,"TIFF library error\n");
    if (mode == 3)   /* tiff */
	{
	if (*ofile != ' ')
	    ofile = strtok( ofile, " " );
	else
	    ofile = "render.tiff";
	tfile=TIFFOpen(ofile,"w");
	if (!tfile) exit(-1);
	TIFFSET(tfile,TIFFTAG_DOCUMENTNAME,ofile);
	TIFFSET(tfile,TIFFTAG_SOFTWARE,program_name);
	TIFFSET(tfile,TIFFTAG_BITSPERSAMPLE,8);
	TIFFSET(tfile,TIFFTAG_SAMPLESPERPIXEL,(alpha_channel ? 4 : 3));
	TIFFSET(tfile,TIFFTAG_PHOTOMETRIC,PHOTOMETRIC_RGB);
	TIFFSET(tfile,TIFFTAG_IMAGEWIDTH,xsize);
	TIFFSET(tfile,TIFFTAG_IMAGELENGTH,ysize);
	TIFFSET(tfile,TIFFTAG_RESOLUTIONUNIT,2);
	TIFFSET(tfile,TIFFTAG_XRESOLUTION,300.);
	TIFFSET(tfile,TIFFTAG_YRESOLUTION,300.);
#ifdef __alpha
	TIFFSET(tfile,TIFFTAG_FILLORDER,FILLORDER_MSB2LSB);
#endif
#ifdef	TIFF_INVERT
	TIFFSET(tfile,TIFFTAG_ORIENTATION,ORIENTATION_TOPLEFT);
#else
	TIFFSET(tfile,TIFFTAG_ORIENTATION,ORIENTATION_BOTLEFT);
#endif
	TIFFSET(tfile,TIFFTAG_PLANARCONFIG,PLANARCONFIG_CONTIG);
	TIFFSET(tfile,TIFFTAG_COMPRESSION,COMPRESSION_LZW);
	rows_per_strip = ysize;
	TIFFSET(tfile,TIFFTAG_ROWSPERSTRIP,rows_per_strip);
	if (alpha_channel)
	    {
	    uint16 extra_samples, sample_info[1];
	    extra_samples=1;
	    sample_info[0]=EXTRASAMPLE_ASSOCALPHA;
	    TIFFSetField(tfile,TIFFTAG_EXTRASAMPLES,extra_samples,&sample_info[0]);
	    }
	scanline=(unsigned char *) malloc(TIFFScanlineSize(tfile));
	if (scanline == (unsigned char *) NULL)
	    {
	    fprintf(stderr,"\nMemory allocation error\n");
	    return(-1);
	    }
  	}
    else
#endif

#ifdef GD_SUPPORT /* JPEG + PNG */
    if (mode == 4 || mode == 5) {
	int transparent;
	molecule_img = gdImageCreateTrueColor(xsize, ysize);
	label_img = gdImageCreateTrueColor(xsize, ysize);
	gdImageAlphaBlending(label_img, 0);
/*
	if (!molecule_img)
	{
	    fprintf(stderr,"PNG initialization failed - die\n");
	    exit(-1);
	}
*/

        /* Signal that all freetype font calls in this program will receive
         * fontconfig patterns rather than filenames of font files */
        gdFTUseFontConfig(1);

	transparent = gdImageColorResolve(label_img, bkg_r, bkg_g, bkg_b);
        gdImageColorTransparent(label_img, transparent);
	gdImageFilledRectangle(label_img,0,0,xsize,ysize,transparent);

	ofile = strtok( ofile, " " );
    }
    else
#endif

    /* NOP */ ;
    start_time = time(NULL);
    return(1);
    }
      
/****************************************************************/
/* Write out a single row of output pixels			*/
/****************************************************************/
else if (*option == 2)
    {
      
    if (mode == 0) /* AVS image file (AlphaRGB) bytes */
	{
	if (alpha_channel)
	    for (i=0; i<xsize; i++)
		{
		putchar( buffer4[i] );
		putchar( buffer1[i] );
		putchar( buffer2[i] );
		putchar( buffer3[i] );
		}
	else
	    for (i=0; i<xsize; i++)
		{
		putchar( 255 );
		putchar( buffer1[i] );
		putchar( buffer2[i] );
		putchar( buffer3[i] );
		}
	}
    else

    if (mode == 1)	/* original RGB bytes */
	{
	for (i=0; i<xsize; i++)
	    {
	    putchar( buffer1[i] );
	    putchar( buffer2[i] );
	    putchar( buffer3[i] );
	    }
	}
    else

#ifdef LIBIMAGE_SUPPORT  
    if (mode == 2)      /* -sgi option (libimage format) */
	{
	putrow(image,buffer1,row,0);
	putrow(image,buffer2,row,1);
	putrow(image,buffer3,row,2);
	if (alpha_channel)
	    putrow(image,buffer4,row,3);
	row++;
	}
    else
#endif

#ifdef TIFF_SUPPORT
    if (mode == 3)
	{
	my_write_tiff(tfile,buffer1,buffer2,buffer3,buffer4,xsize,scanline);
	}
    else
#endif

#ifdef GD_SUPPORT /* JPEG + PNG */
    if (mode == 4 || mode == 5)
	{
/* TODO: Speed up using full-line-at-a-time copy instead of pixel-by-pixel */
	int pixel_color;
	static int j = 0;
	int transparent;

	if (alpha_channel)
#if 1
	    for (i=0; i<xsize; i++) {
		pixel_color = gdImageColorResolveAlpha(molecule_img,
			buffer1[i], buffer2[i], buffer3[i], 
			(255-buffer4[i])>>1); /* inverse bitwise shift */
		gdImageSetPixel(molecule_img, i, j, pixel_color);
	    }

/* TODO: Check the following to see if it is faster than the above. */
#else
	    for (i=0; i<xsize; i++) {
		molecule_img->tpixels[j][i] = gdTrueColorAlpha(
			buffer1[i], buffer2[i], buffer3[i], 
			(255-buffer4[i])>>1);
	    }
#endif
	else
	    for (i=0; i<xsize; i++) {
		pixel_color = gdImageColorResolve(molecule_img,
			buffer1[i], buffer2[i], buffer3[i]);
		gdImageSetPixel(molecule_img, i, j, pixel_color);
	    }

	if (alpha_channel) {
	    transparent = gdImageColorResolveAlpha(molecule_img, 0, 0, 0, 127);
	    gdImageColorTransparent(molecule_img, transparent);
	}

	j++;
	}
    else
#endif

    if (mode >= 7)
    	{
    	fprintf(stderr,"\n local.c: illegal output mode\n");
	exit(-1);
	}
      
    return(1);
    }

/****************************************************************/
/* Close output file if necessary				*/
/****************************************************************/
else if (*option == 3)
    {
      
#ifdef LIBIMAGE_SUPPORT
    if (mode == 2)
	iclose(image);
    else
#endif

#ifdef TIFF_SUPPORT
    if (mode == 3)
	{
	(void) TIFFFlushData(tfile);
#if defined(LINUX) || defined(OSX)
	signal( SIGSEGV, SIG_IGN );
#endif /* LINUX */
	(void) TIFFClose(tfile);
	}
    else
#endif

#ifdef GD_SUPPORT /* JPEG + PNG */
    if (mode == 4 || mode == 5) {

	if (label_img) {
	    /*
	    gdImageAlphaBlending(molecule_img, 0);
	    gdImageSaveAlpha(molecule_img, 1);
	    */
	    gdImageSetTile(molecule_img, label_img);
	    gdImageFilledRectangle(molecule_img, 0, 0, xsize, ysize, gdTiled);
	}

	/*if (*ofile != ' ' && *ofile != '-')*/
	if (ofile) {
	    FILE *fd = fopen(ofile,"wb");
            if (!fd) {
                fprintf(stderr,"Could not open output file %s\n",ofile);
                exit(-1);
            }
	    if (mode == 4)
		gdImageJpeg(molecule_img, fd, quality);
	    else
		gdImagePng(molecule_img, fd);
	    fclose(fd);

	    if (label_img) {
		gdImageDestroy(label_img);
	    }
	}
	else
	    if (mode == 4)
		gdImageJpeg(molecule_img, stdout, quality);
	    else
		gdImagePng(molecule_img, stdout);
	gdImageDestroy(molecule_img);
    }
    else
#endif

    /* NOP */ ;
    end_time = time(NULL);
    fprintf(stderr,"rendering time - %5d sec\n",(int)(end_time-start_time));

    return(1);
    }
      
/****************************************************************/
/* Add title to image file					*/
/****************************************************************/
else if (*option == 4)
    {
      
#ifdef LIBIMAGE_SUPPORT
    if (mode == 2)
	isetname(image,buffer1);
    else
#endif
    
#ifdef TIFF_SUPPORT
    if (mode == 3)
	TIFFSetField(tfile,TIFFTAG_IMAGEDESCRIPTION,buffer1);
    else
#endif
    
    /* NOP */ ;
    return(1);
    }

/****************************************************************/
/* Load background color to local storage			*/
/****************************************************************/
else if (*option == 5)
    {
    
    bkg_r  = *buffer1;
    bkg_g  = *buffer2;
    bkg_b  = *buffer3;
/*
    fprintf(stderr,"Loading background color %d %d %d\n", bkg_r, bkg_g, bkg_b);
 */
    return(1);
    }

return 0;
}

#ifdef TIFF_SUPPORT
void my_write_tiff(fp, buf1, buf2, buf3, buf4, size, scanline)
TIFF		*fp;
short		buf1[], buf2[], buf3[], buf4[];
int  		size;
unsigned char 	scanline[];
{
static int row=0;
int i; 
int j = 0;

  if (alpha_channel)
    for (i=0; i<size; i++) {
	scanline[j++] = buf1[i];
	scanline[j++] = buf2[i];
	scanline[j++] = buf3[i];
	scanline[j++] = buf4[i];
    }
  else
    for (i=0; i<size; i++) {
	scanline[j++] = buf1[i];
	scanline[j++] = buf2[i];
	scanline[j++] = buf3[i];
    }

  if (TIFFWriteScanline(fp,scanline,row,0) < 0)
    fprintf (stderr, "\nBad return code from TIFF write\n");

row++;
}
#endif

/*===========================================================================*/
/*
 * _addlabel()
 */
int addlabel_(fontname, fontsize, fontscale, fontalign, xp, yp, zp, red, grn, blu,
	labelstring, font_len, label_len)
	int *fontalign;
	float *fontsize, *fontscale, *xp, *yp, *zp, *red, *grn, *blu;
	char *fontname, *labelstring;
	long int font_len, label_len;
{
#ifdef GD_SUPPORT
	static double last_x = 0.0, last_y = 0.0, last_z = 0.0;

	double x = *xp, y = *yp, z = *zp;
        float angle = 0.0;
	int brect[8], font_color;
	char *err, font[128], instring[128], *justify;
	size_t out_size; /* out_size is just a dummy variable */
	char *string;

	if (mode != 4 && mode != 5)
	    return 0;

	/* TODO: If FONTNAME does not exist in the GDFONTPATH, fail gracefully */
	out_size = trimwhitespace(font, 128, fontname);
	out_size = trimwhitespace(instring, 128, labelstring);
	string = instring;

	(void)out_size;		/* prevents compiler complaints */
	(void)last_z;		/* about unused variables */
	(void)justify;

	/* Allocate colours */
	/* FIXME: how to pass non-zero alpha value? */
	font_color = gdImageColorAllocateAlpha(label_img, 
	    (double)*red*255.0, (double)*grn*255.0, (double)*blu*255.0, 0);

        /* Draw once with a NULL image to get the bounding rectangle */
        err = gdImageStringFT(NULL, &brect[0], font_color,
		font, (double)*fontsize*(double)*fontscale, angle, 0, 0, string);
	if (err)
		fprintf(stderr,"gdImageStringFT: %s while printing string %s with font %s\n",
			err, string, font);

	/* Align the string */
	if ((int)*fontalign == 1) {
		/* Justify string center */
		justify = "Center";
		x -= (brect[2]-brect[0]) / 2.;
		y += (brect[3]-brect[1]) / 2.;
	}
	else if ((int)*fontalign == 2) {
		/* Justify string right */
		justify = "Right";
		x -= (brect[2]-brect[0]);
		y += (brect[3]-brect[1]);
	}
	else if ((int)*fontalign == 3) {
		justify = "Offset";
		x += last_x;
		y += last_y;
	}
	else { 
		/* Do nothing (default) */ 
		justify = "Left";
	}

	/* TODO: Process super/subscripts */
	while (strpbrk(string, "{}^_")) {
	    char *mark = strpbrk(string, "{}^_");
	    char save_mark = *mark;

	    /* dump up to the first special character */
	    *mark = '\0';
	    err = gdImageStringFT(label_img, &brect[0], font_color,
		font, (double)*fontsize*(double)*fontscale, angle, x, y, string);
	    x += (brect[2] - brect[0]);
	    string = mark+1;

	    /* start sub/superscript placement */
	    last_y = y;
	    if (save_mark == '^')
		y -= 0.5 * (*fontsize) * (*fontscale);
	    else if (save_mark == '_')
		y += 0.5 * (*fontsize) * (*fontscale);
	    /* dump either a single character (careful: UTF-8!) */
	    /* or the string enclosed by {} */
	    if (*string == '{') {
		string++;
		if ((mark = strchr(string,'}')) != NULL) {
		    *mark = '\0';
		    err = gdImageStringFT(label_img, &brect[0], font_color,
			font, (double)*fontsize*(double)*fontscale, angle, x, y, string);
		    x += (brect[2] - brect[0]);
		    y = last_y;
		    string = mark+1;
		} else {
		    /* No closing '}', just print the rest of the string */
		}

	    } else {
	    	/* FIXME: check if it's not really UTF-8 */
		char utf8[6] = {0,0,0,0,0,0};
		char *u = utf8;
		*u = *string++;
		if ((*u & 0xC0) == 0xC0) { /* Start of a UTF-8 sequence */
		    *++u = *string++;
		    if ((*string & 0xc0) == 0x80)
			*++u = *string++;
		    if ((*string & 0xc0) == 0x80)
			*++u = *string++;
		    if ((*string & 0xc0) == 0x80)
			*++u = *string++;
		}
		err = gdImageStringFT(label_img, &brect[0], font_color,
			font, (double)*fontsize*(double)*fontscale, angle, x, y, utf8);
		x += (brect[2] - brect[0]);
		y = last_y;
	    }

	}

	/* Now render the remaining string, if any */
	err = gdImageStringFT(label_img, &brect[0], font_color,
		font, (double)*fontsize*(double)*fontscale, angle, x, y, string);
	if (err)
		fprintf(stderr,"gdImageStringFT: %s while printing string %s with font %s\n",
			err, string, font);

	last_x = x + (brect[2] - brect[0]);
	last_y = y;
	last_z = z;

#endif
	return 0;
}

size_t trimwhitespace(char *out, size_t len, const char *str)
{
	/* A simple function to trim the whitespace from the right-side of
	 * a string.
	 */

	const char *end;
	size_t out_size;

	/* Trim leading space */
	/*while(isspace(*str)) str++;*/

	/* Trim trailing space */
	end = str + strlen(str) - 1;
	while(end > str && isspace(*end)) end--;
	end++;

	/* Set output size to minimum of trimmed string length and buffer size
	 * minus 1 */
	out_size = (end - str) < len-1 ? (end - str) : len-1;

	/* Copy trimmed string and add null terminator */
	memcpy(out, str, out_size);
	out[out_size] = 0;

	return out_size;
}
