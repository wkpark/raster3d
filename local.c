/*
 * Raster3D V2.4j
 * local.c
 *
 * Output from render.f is performed by calls to routine LOCAL,
 * which is implemented here.
 *
 * This version of local.c supports 5 output modes, 3 of which
 * are controlled by conditional compilation directives
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
 *		JPEG image output to stdout
 *
 *	mode 5	#ifdef IMAGEPIPE
 *		AVS image file piped to ImageMagick for conversion to
 *		some other image type determined by suffix
 *
 * V2.4(alpha)  These are for debugging, not intended for general use
 *	command line options
 *		-debug  passes flag back to render
 *		-aaN, 	where N=(0,1,2,3,4) forces anti-aliasing option
 *		-invert	invert y coordinate axis
 * additional parameter passed in mode 1 calls to inform local() of options
 * set in render code (e.g. alpha channel)
 *
 * V2.4d	Set TIFF resolution flag to 300dpi
 *		This may be incorrect, of course, but it's more likely to
 *		be correct than the default treatment of 72dpi in programs
 *		like PhotoShop
 * V2.4e	Bugfix for TIFF output image (blue channel shifted by 1 pixel)
 * V2.4j	Major re-working of code
 *		-jpeg folded back into main render version
 *		-out file.xxx  to pipe output to ImageMagick
 */

#include	<stdio.h>
#include	<fcntl.h>
#include	<string.h>

#ifdef LIBIMAGE_SUPPORT
#include	<gl/image.h>
#endif

#ifdef TIFF_SUPPORT
#include        <tiff.h>
#include        <tiffio.h>
#ifdef LINUX
#include	<signal.h>
#endif LINUX
#endif TIFF_SUPPORT

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

/* Define bits in returned status */
#define		ANTIALIAS	007
#define		INVERT		010
#define		DEBUGGING	020

/* Define bits passed in 3rd parameter of mode 1 */
#define		ALPHACHANNEL	040

int		alpha_channel = 0;
char *version = "Raster3D V2.4j" ;

/* HPUX lacks Fortran intrinsic functions AND and OR for some reason, */
/* so I put a copy here. On the other hand HPUX has an unusually sane */
/* calling convention for Fortran subroutine names.                   */
#ifdef __hpux
#define local_ local
int and(i,j) int *i,*j; {return (*i & *j);}
int or(i,j)  int *i,*j; {return (*i | *j);}
#endif


local_(option,buffer1,buffer2,buffer3,buffer4)
     int	*option;
     short	*buffer1, *buffer2, *buffer3, *buffer4;
{
  
  /* Everyone needs these */
  static int	xsize, ysize;
  static int	mode    = -1;
  static int	quality = 95;
  int	        i;
  static char	*ofile;
  int		status = 0;
  int		invert = 0;
  int		bits;
  
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
#endif

#ifdef IMAGEPIPE
  /* For -out output option only */
  static FILE *pipe;
  char convert_command[128];
#endif


/****************************************************************/
/* The action taken by this subroutine is determined by the	*/
/* option parameter. The first call (option=0) determines the 	*/
/* output mode.							*/
/* As of V2.2.1 multiple bits may be set in the value returned.	*/
/****************************************************************/
if (*option == 0) 
    {
    
    if (strncmp( (char *)buffer1, "-debug", 6) ==0)
      {
      fprintf(stderr,"\nDebugging mode selected\n");
      status |= DEBUGGING;
      buffer1 = buffer2;   buffer2 = buffer3;   buffer3 = buffer4;
      }

    if (strncmp( (char *)buffer1, "-aa",3) ==0)
      {
      if (((char *)buffer1)[3] == '1') status |= 1;
      if (((char *)buffer1)[3] == '2') status |= 2;
      if (((char *)buffer1)[3] == '3') status |= 3;
      if (((char *)buffer1)[3] == '4') status |= 4;
      if (status & ANTIALIAS == 0) status |= 4;
      buffer1 = buffer2;   buffer2 = buffer3;   buffer3 = buffer4;
      }

    else if (strncmp( (char *)buffer1, "-invert", 7) ==0)
      {
      invert = !invert;
      buffer1 = buffer2;   buffer2 = buffer3;   buffer3 = buffer4;
      }

    if (strncmp( (char *)buffer1, "-quality", 8) == 0)
      {
      i = sscanf((char *)buffer2,"%d",&quality);
      if (i != 1 || quality <= 0 || quality > 100) {
	fprintf(stderr,"\n JPEG quality must be an integer from 1 to 100\n");
	exit(-1);
	}
      buffer1 = buffer3;   buffer2 = buffer4;
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
#else
	fprintf(stderr,
		"\n This copy of render was not built with jpeg support\n");
	exit(-1);
#endif
      }

    else if (strncmp( (char *)buffer1, "-out", 4) == 0)
      {
#ifdef	IMAGEPIPE
	mode  = 5;
	ofile = strtok( (char *)buffer2, " " );
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
	fprintf(stderr,
		"\n Unfamiliar switch: %12.12s", buffer1);
	fprintf(stderr,
		"\n Usage: render < infile                       (AVS image to stdout)");
	fprintf(stderr,
		"\n    or  render [-quality NN] -jpeg < infile   (JPEG image to stdout)");
	fprintf(stderr,
		"\n    or  render -out outfile.xxx < infile      (pipe output to ImageMagick");
	fprintf(stderr,
		"\n                                               for conversion to image type xxx)");
	fprintf(stderr,
		"\n    or  render [-sgi outfile] [-tiff outfile] < infile");
	fprintf(stderr,"\n\n");
	exit(-1);
      }

    else /* default avs mode */
      mode  = 0;

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
    xsize = *buffer1;
    ysize = *buffer2;
    bits  = *buffer3;

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
	TIFFSetField(tfile,TIFFTAG_DOCUMENTNAME,ofile);
	TIFFSetField(tfile,TIFFTAG_SOFTWARE,version);
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
#ifdef	OLD_CODE
	rows_per_strip=8192/TIFFScanlineSize(tfile);
	if (rows_per_strip == 0)
	    rows_per_strip=1;
#else
	rows_per_strip = ysize;
#endif
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
	jpeg_pixels = (JSAMPLE *) malloc(xsize*3*sizeof(JSAMPLE));
	if (jpeg_pixels == (JSAMPLE *) NULL)
	    {
	      fprintf(stderr,"\nMemory allocation error\n");
	      return(-1);
	    }
	cinfo.err = jpeg_std_error(&jerr);
	jpeg_create_compress(&cinfo);
	jpeg_stdio_dest(&cinfo,stdout);
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
	sprintf( convert_command, "convert -verbose -quality %d avs:- ", 
		 quality );
/*	strcpy(  convert_command, "convert -verbose avs:- "); */
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
   
    if (mode >= 6)
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
#endif LINUX
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
	jpeg_write_marker(&cinfo,JPEG_COM,(unsigned char*)version,strlen(version));
	jpeg_write_marker(&cinfo,JPEG_COM,"\n",2);
	jpeg_write_marker(&cinfo,JPEG_COM,(unsigned char *)buffer1,80);
	}
    else
#endif
  
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
    fprintf (stderr, "\n", "Bad return code from TIFF write\n");

row++;
}
#endif
