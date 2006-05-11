/*
 * Raster3D V2.7
 * local.c
 *
 * Output from render.f is performed by calls to routine LOCAL.
 *
 * This version of local.c supports 7 output modes, 5 of which
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
 *	mode 4	#ifdef JPEG_SUPPORT
 *		JPEG image output to file (defaults to stdout)
 *
 *	mode 5	#ifdef IMAGEPIPE
 *		AVS image file piped to ImageMagick for conversion to
 *		some other image type determined by suffix
 *
 *	mode 6	#ifdef PNG_SUPPORT
 *		PNG image output to file (defaults to stdout)
 *
 * Command line switches other than output file format handled elsewhere
 * here we recognize
 *		-invert	         invert y coordinate axis
 *		-jpeg [filename] for jpeg output
 *		-png  [filename] for png output
 *		-avs  [filename] for AVS output
 *		-sgi  [filename] SGI libimage format output
 *		-tiff [filename] TIFF output format
 *		-out   file.xxx  pipe output to ImageMagick
 */

#include	<stdio.h>
#include	<fcntl.h>
#include	<string.h>
#include	<time.h>
#include	<stdlib.h>
#include	<netinet/in.h>

#ifdef LIBIMAGE_SUPPORT
#include	<gl/image.h>
#endif

#ifdef TIFF_SUPPORT
#include        <tiff.h>
#include        <tiffio.h>
#ifdef LINUX
#include	<signal.h>
#endif /* LINUX */
#endif /* TIFF_SUPPORT */

#ifdef JPEG_SUPPORT
#include	<jpeglib.h>
#include	<jerror.h>
#ifdef BROKEN_LIBJPEG
#undef jpeg_create_compress
#endif
  /* JPEG data structures */
  struct jpeg_compress_struct	cinfo;
  struct jpeg_error_mgr      	jerr;
  JSAMPLE			*jpeg_pixels;
  JSAMPROW			jpeg_row[1];
#endif

#ifdef PNG_SUPPORT
#include	<png.h>
#ifndef png_jmpbuf
#  define png_jmpbuf(png_ptr) ((png_ptr)->jmpbuf)
#endif
  /* PNG data structures */
  png_structp png_ptr;
  png_infop   info_ptr;
  png_text    text_ptr[2];
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


int local_(option,buffer1,buffer2,buffer3,buffer4)
     int	*option;
     short	*buffer1, *buffer2, *buffer3, *buffer4;
{
  
  /* Everyone needs these */
  static int	xsize, ysize;
  static int	mode    = -1;
  int	        i;
  static char	*ofile;
  int		status = 0;
  int		invert = 0;
  int		bits;
  int		quality;
 
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

#ifdef JPEG_SUPPORT
  /* For -jpeg output option only */
  JSAMPLE *q;
  static FILE *jpegfile;
#endif

#ifdef IMAGEPIPE
  /* For -out output option only */
  static FILE *pipe;
  char convert_command[128];
#endif

#ifdef PNG_SUPPORT
  /* For -png output option only */
  static png_bytep pngline;
  static FILE *pngfile;
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
    else if (strncmp( (char *)buffer1, "-avs" , 4) == 0)
      {
	mode  = 0;
      }

    else if (strncmp( (char *)buffer1, "-sgi" , 4) == 0)
      {
#ifdef      LIBIMAGE_SUPPORT
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
#ifdef      JPEG_SUPPORT
	mode  = 4;
	ofile = (char *)buffer2;
#else
	fprintf(stderr,
		"\n This copy of render was not built with jpeg support\n");
	exit(-1);
#endif
      }

    else if (strncmp( (char *)buffer1, "-png" , 4) == 0)
      {
#ifdef      PNG_SUPPORT
	mode  = 6;
	ofile = (char *)buffer2;
#else
	fprintf(stderr,
		"\n This copy of render was not built with png support\n");
	exit(-1);
#endif
      }

    else if (strncmp( (char *)buffer1, "-out", 4) == 0)
      {
#ifdef	IMAGEPIPE
	mode  = 5;
	ofile = strtok( (char *)buffer2, " " );
	if (ofile)
		fprintf(stderr,"ofile = %s\n",ofile);
	else
		fprintf(stderr,"ofile = NULL\n");
#else
	fprintf(stderr,
		"\n This copy of render was not built with support\n");
	fprintf(stderr,
		" for piping output to ImageMagick (-out file)\n");
	exit(-1);
#endif
      }

    else if (strncmp( (char *)buffer1, "-orig", 5) == 0)
      {
      	mode  = 1;
	invert = !invert;
      }
    else if (strncmp( (char *)buffer1, "  ", 2) != 0)
      {
	fprintf(stderr, "\n%s",program_name);
      	if (strncmp( (char *)buffer1, "-help", 5) != 0)
	    fprintf(stderr, "\n Unfamiliar switch: %12.12s", buffer1);
	fprintf(stderr, "\n\n Usage:");
	fprintf(stderr, "\n   input from stdin; output mode controlled from command line \n");
	fprintf(stderr,
		"\n     render [-png [outfile]]       PNG image to stdout (default!) or file");
	fprintf(stderr,
		"\n     render -avs                   AVS image to stdout");
	fprintf(stderr,
		"\n     render -jpeg [outfile]        JPEG image to outfile (defaults to stdout)");
	fprintf(stderr,
		"\n     render -sgi  [outfile]        output to SGI libimage file (defaults to render.rgb)");
	fprintf(stderr,
		"\n     render -tiff [outfile]        output to TIFF file (defaults to render.tif)");
	fprintf(stderr,
		"\n     render -out outfile.xxx       pipe output via ImageMagick to get image type xxx");
	fprintf(stderr,"\n");
	fprintf(stderr,"\n Options:");
	fprintf(stderr,"\n   these over-ride contents of input stream header records \n");
	fprintf(stderr,"\n    -aa                   anti-aliasing (SCHEME 4)");
	fprintf(stderr,"\n    -alpha                alpha channel in output image (SCHEME 0)");
	fprintf(stderr,"\n    -bg white|black|<col> set background color (<col> is hex #RRGGBB)");
	fprintf(stderr,"\n    -debug                verbose output while running");
	fprintf(stderr,"\n    -draft                no anti-aliasing (SCHEME 1)");
	fprintf(stderr,"\n    -fontscale FF         multiplier for PostScript font size");
	fprintf(stderr,"\n    -gamma GG             gamma correction applied to output image");
	fprintf(stderr,"\n    -invert               invert y axis");
	fprintf(stderr,"\n    -labels               write labels to PostScript file label3d.ps");
	fprintf(stderr,"\n    -quality QQ           0 < QQ < 95  jpeg compression [default 90]");
	fprintf(stderr,"\n    -[no]shadow           enable or disable shadowing");
	fprintf(stderr,"\n    -size HHHxVVV         specify size of output image in pixels");
	fprintf(stderr,"\n    -transparent          same as -alpha (SCHEME 0)");
	fprintf(stderr,"\n    -zoom ZZ[%%]           rescale image by ZZ      ");
	fprintf(stderr,"\n");
	exit(-1);
      }

    else /* Version 2.7a - default to png */
      {
#ifdef      PNG_SUPPORT
	mode  = 6;
	ofile = (char *)buffer2;
#else
	fprintf(stderr,
		"\n This copy of render was not built with png support\n");
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
    xsize   = *buffer1;
    ysize   = *buffer2;
    bits    = *buffer3;
    quality = *buffer4;

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

#ifdef      LIBIMAGE_SUPPORT
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
    if (mode == 3)   /* tiff */
	{
	if (*ofile != ' ')
	    ofile = strtok( ofile, " " );
	else
	    ofile = "render.tif";
	tfile=TIFFOpen(ofile,"w");
	if (!tfile) exit(-1);
	TIFFSetField(tfile,TIFFTAG_DOCUMENTNAME,ofile);
	TIFFSetField(tfile,TIFFTAG_SOFTWARE,program_name);
	TIFFSetField(tfile,TIFFTAG_BITSPERSAMPLE,8);
	TIFFSetField(tfile,TIFFTAG_SAMPLESPERPIXEL,(alpha_channel ? 4 : 3));
	TIFFSetField(tfile,TIFFTAG_PHOTOMETRIC,PHOTOMETRIC_RGB);
	TIFFSetField(tfile,TIFFTAG_IMAGEWIDTH,xsize);
	TIFFSetField(tfile,TIFFTAG_IMAGELENGTH,ysize);
	TIFFSetField(tfile,TIFFTAG_RESOLUTIONUNIT,2);
	TIFFSetField(tfile,TIFFTAG_XRESOLUTION,300.);
	TIFFSetField(tfile,TIFFTAG_YRESOLUTION,300.);
#ifdef __alpha
	TIFFSetField(tfile,TIFFTAG_FILLORDER,FILLORDER_MSB2LSB);
#endif
#ifdef	TIFF_INVERT
	TIFFSetField(tfile,TIFFTAG_ORIENTATION,ORIENTATION_TOPLEFT);
#else
	TIFFSetField(tfile,TIFFTAG_ORIENTATION,ORIENTATION_BOTLEFT);
#endif
	TIFFSetField(tfile,TIFFTAG_PLANARCONFIG,PLANARCONFIG_CONTIG);
	TIFFSetField(tfile,TIFFTAG_COMPRESSION,COMPRESSION_LZW);
	rows_per_strip = ysize;
	TIFFSetField(tfile,TIFFTAG_ROWSPERSTRIP,rows_per_strip);
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

#ifdef	JPEG_SUPPORT
    if (mode == 4)
	{
	if (*ofile != ' ' && *ofile != '-')
	    {
	    ofile = strtok( ofile, " " );
	    jpegfile = fopen(ofile, "wb");
	    if (!jpegfile)
	    	{
		fprintf(stderr,"Could not open output file %s\n",ofile);
		exit(-1);
		}
	    }
	else
	    jpegfile = stdout;
	jpeg_pixels = (JSAMPLE *) malloc(xsize*3*sizeof(JSAMPLE));
	if (jpeg_pixels == (JSAMPLE *) NULL)
	    {
	      fprintf(stderr,"\nMemory allocation error\n");
	      return(-1);
	    }
	cinfo.err = jpeg_std_error(&jerr);
	jpeg_create_compress(&cinfo);
	jpeg_stdio_dest(&cinfo,jpegfile);
	cinfo.image_width      = xsize;
	cinfo.image_height     = ysize;
	cinfo.input_components = 3;
	cinfo.in_color_space   = JCS_RGB;
	jpeg_set_defaults(&cinfo);
	jpeg_set_quality(&cinfo, quality, TRUE );

	/* The following choices affect efficiency on a given machine */
/*	cinfo.optimize_coding	= TRUE; */
	cinfo.dct_method	= JDCT_FLOAT;

	jpeg_simple_progression(&cinfo);
	jpeg_start_compress(&cinfo, TRUE);
	}
    else
#endif
	
#ifdef	IMAGEPIPE
    if (mode == 5)   /* pipe to ImageMagick */
	{
	if (!ofile || *ofile==' ' || *ofile=='-') 
	    { 
	    fprintf(stderr,"render: illegal output file name\n");
	    exit(-1);
	    }
	sprintf( convert_command, "convert -verbose -quality %d avs:- ", 
		 quality );
	strncat( convert_command, ofile, 96 );
	fprintf(stderr,"\n Opening pipe to | %s\n",convert_command);
	pipe = popen( convert_command, "w" );
	if (!pipe) 
	    {
	    fprintf(stderr," pipe failed - die\n");
	    exit(-1);
	    }
#ifdef NETWORKBYTEORDER
	putw( htonl(xsize), pipe );
	putw( htonl(ysize), pipe );
#else
	putw( xsize, pipe );
	putw( ysize, pipe );
#endif
	fflush(pipe);
	}
    else
#endif

#ifdef PNG_SUPPORT
    if (mode == 6) 
    	{
	if (*ofile != ' ' && *ofile != '-')
	    {
	    ofile = strtok( ofile, " " );
	    pngfile = fopen(ofile, "wb");
	    if (!pngfile)
	    	{
		fprintf(stderr,"Could not open output file %s\n",ofile);
		exit(-1);
		}
	    }
	else
	    pngfile = stdout;
	png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	if (png_ptr) info_ptr = png_create_info_struct(png_ptr);
	if (!png_ptr || !info_ptr)
	    {
	    fprintf(stderr,"PNG initialization failed - die\n");
	    exit(-1);
	    }
	if (setjmp(png_jmpbuf(png_ptr)))
	    {
	    fprintf(stderr,"Error writing PNG file - die\n");
	    exit(-1);
	    }
	png_init_io(png_ptr,pngfile);
	png_set_IHDR(png_ptr, info_ptr, (png_uint_32)xsize, (png_uint_32)ysize, 8, 
		     (alpha_channel ? PNG_COLOR_TYPE_RGB_ALPHA : PNG_COLOR_TYPE_RGB),
		     PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);
	pngline=(png_bytep) calloc(xsize, sizeof(png_byte) * (alpha_channel ? 4 : 3));
	if (!pngline)
	    {
	    fprintf(stderr,"\nMemory allocation error\n");
	    return(-1);
	    }
	if (quality == 100) png_set_compression_level( png_ptr, Z_NO_COMPRESSION );
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
    if (mode ==3)
	{
	my_write_tiff(tfile,buffer1,buffer2,buffer3,buffer4,xsize,scanline);
	}
    else
#endif

#ifdef JPEG_SUPPORT
    if (mode == 4)
	{
	jpeg_row[0] = (JSAMPROW) jpeg_pixels;
	q = jpeg_pixels;
	for (i=0; i<xsize; i++) 
	    {
	    *q++ = (JSAMPLE) buffer1[i];
	    *q++ = (JSAMPLE) buffer2[i];
	    *q++ = (JSAMPLE) buffer3[i];
	    }
	jpeg_write_scanlines( &cinfo, jpeg_row, 1 );
	}
    else
#endif 
	
#ifdef IMAGEPIPE
    if (mode == 5) 	/* out pipe to ImageMagick */
	{
	if (alpha_channel)
	    for (i=0; i<xsize; i++)
		{
		putc( buffer4[i], pipe );
		putc( buffer1[i], pipe );
		putc( buffer2[i], pipe );
		putc( buffer3[i], pipe );
		}
	else
	    for (i=0; i<xsize; i++)
		{
		putc( 255, pipe );
		putc( buffer1[i], pipe );
		putc( buffer2[i], pipe );
		putc( buffer3[i], pipe );
		}
	}
    else
#endif
  
#ifdef PNG_SUPPORT
    if (mode == 6)
	{
	int j = 0;

	if (alpha_channel)
	    for (i=0; i<xsize; i++) {
		pngline[j++] = buffer1[i];
		pngline[j++] = buffer2[i];
		pngline[j++] = buffer3[i];
		pngline[j++] = buffer4[i];
	    }
	else
	    for (i=0; i<xsize; i++) {
		pngline[j++] = buffer1[i];
		pngline[j++] = buffer2[i];
		pngline[j++] = buffer3[i];
	    }
	png_write_rows(png_ptr, &pngline, 1);

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
#ifdef LINUX
	signal( SIGSEGV, SIG_IGN );
#endif /* LINUX */
	(void) TIFFClose(tfile);
	}
    else
#endif
      
#ifdef JPEG_SUPPORT
    if (mode == 4)
	{
	jpeg_finish_compress(&cinfo);
	}
    else
#endif
  
#ifdef IMAGEPIPE
    if (mode == 5)
	{
	fflush(pipe);
	pclose(pipe);
        }
    else
#endif

#ifdef PNG_SUPPORT
    if (mode == 6)
    	{
	png_write_end(png_ptr, info_ptr);
	png_destroy_write_struct(&png_ptr, &info_ptr);
	free(pngline);
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
    
#ifdef JPEG_SUPPORT
    if (mode == 4)
	{
	jpeg_write_marker(&cinfo,JPEG_COM,(unsigned char*)program_name,strlen(program_name));
	jpeg_write_marker(&cinfo,JPEG_COM,"\n",2);
	jpeg_write_marker(&cinfo,JPEG_COM,(unsigned char *)buffer1,80);
	}
    else
#endif

#ifdef PNG_SUPPORT
    if (mode == 6)
    	{
	text_ptr[0].key  = "Software";
	text_ptr[0].text = (unsigned char *)program_name;
	text_ptr[0].compression = PNG_TEXT_COMPRESSION_NONE;
	text_ptr[0].text_length = strlen(program_name);
	text_ptr[1].key  = "Title";
	text_ptr[1].text = (unsigned char *)buffer1;
	text_ptr[1].compression = PNG_TEXT_COMPRESSION_NONE;
	text_ptr[1].text_length = 79;
	png_set_text(png_ptr, info_ptr, text_ptr, 2);
	png_write_info(png_ptr, info_ptr);
	}
    else
#endif

    /* NOP */ ;
    return(1);
    }
  
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
