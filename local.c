/*
 * Raster3D V2.4(alpha)
 * local.c
 *
 * Output from render.f is performed by calls to routine LOCAL,
 * which is implemented here.
 *
 * This version of local.c supports 4 output modes, 2 of which
 * are controlled by conditional compilation directives -
 *	AVS image file sent to stdout
 *		(2 integer header followed by AlphaRGB bytes)
 *		====> As of V2.2 Alpha byte set to 255 <====
 *	original Raster3D file to stdout
 *              (8 integer header followed by RGB bytes)
 *	calls to the libimage library if LIBIMAGE_SUPPORT is defined
 *		(probably only makes sense on an SGI machine)
 *	calls to the TIFF library if TIFF_SUPPORT is defined
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
 * manipulated using various other utilities provided under
 * 4DGifts/iristools/imgtools.
 *
 * Note on conditional code for Linux (redhat):
 * On my redhat 4.2 installation, TIFFClose dumps core on a __cfree_()
 * deallocation error after successfully closing the TIFF file.
 * Don't ask me why.  But since the program is in the process of exiting
 * anyway we can safely ignore the error and continue to exit cleanly.
 *
 * V2.4(alpha)  These are for debugging, not intended for general use
 *	command line options
 *		-debug  passes flag back to render
 *		-aaN, 	where N=(0,1,2,3,4) forces anti-aliasing option
 *		-invert	invert y coordinate axis
 * additional parameter passed in mode 1 calls to inform local() of options
 * set in render code (e.g. alpha channel)
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

/* Define bits in returned status */
#define		ANTIALIAS	007
#define		INVERT		010
#define		DEBUGGING	020

/* Define bits passed in 3rd parameter of mode 1 */
/* (New in version 2.4a)                         */
#define		ALPHACHANNEL	040

/* HPUX lacks Fortran intrinsic functions AND and OR for some reason, */
/* so I put a copy here. On the other hand HPUX has an unusually sane */
/* calling convention for Fortran subroutine names.                   */
#ifdef __hpux
#define local_ local
int and(i,j) int *i,*j; {return (*i & *j);}
int or(i,j)  int *i,*j; {return (*i | *j);}
#endif

int		alpha_channel = 0;

local_(option,buffer1,buffer2,buffer3,buffer4)
     int	*option;
     short	*buffer1, *buffer2, *buffer3, *buffer4;
{
  
  /* Everyone needs these */
  static int	xsize, ysize;
  static int	mode= -1;
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
  /* For -tiff option only */
  static TIFF   *tfile;
  static unsigned char *scanline;
  unsigned short  rows_per_strip;
  void my_write_tiff();
#endif
  
  /*
   * First call (option=0) is to determine the output mode.
   * As of V2.2.1 multiple bits may be set in the value returned.
   */
  
  if (*option == 0) {
    
    if (strncmp( (char *)buffer1, "-debug", 6) ==0)
      {
      fprintf(stderr,"\nDebugging mode selected\n");
      status |= DEBUGGING;
      buffer1 = buffer2;   buffer2 = buffer3;
      }

    if (strncmp( (char *)buffer1, "-aa",3) ==0)
      {
      if (((char *)buffer1)[3] == '1') status |= 1;
      if (((char *)buffer1)[3] == '2') status |= 2;
      if (((char *)buffer1)[3] == '3') status |= 3;
      if (((char *)buffer1)[3] == '4') status |= 4;
      if (status & ANTIALIAS == 0) status |= 4;
      buffer1 = buffer2;   buffer2 = buffer3;
      }

    else if (strncmp( (char *)buffer1, "-invert", 7) ==0)
      {
      invert = !invert;
      buffer1 = buffer2;   buffer2 = buffer3;
      }
    
    if (strncmp( (char *)buffer1, "-tiff", 5) == 0)
      {
#ifdef TIFF_SUPPORT
	mode  = 3;
	ofile = (char *)buffer2;
#else
	fprintf(stderr,
		"\n This copy of render was not built with -tiff library\n");
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
		"\n This copy of render was not built with -sgi library\n");
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
		"\n\n Unfamiliar switch: %12.12s", buffer1);
	fprintf(stderr,
		"\n Usage: render [-sgi outfile] [-tiff outfile] < infile");
	fprintf(stderr,
		"\n    or  render [-orig] < infile > outfile \n");
	exit(-1);
      }
    else /* default avs mode */
      mode  = 0;

    if (invert) status |= INVERT;

    return( status );
  }
  
  /*
   * Subsequent calls are treated differently depending on output mode
   */
  
  if (mode < 0) {
    fprintf(stderr,"\n Output mode not set before output request\n");
    exit(-1);
  }
  
  switch (*option)
    {
      
      /*      
	option 1: open image file for output depending on mode
	*/
      
    case 1:
      
      xsize = *buffer1;
      ysize = *buffer2;
      bits  = *buffer3;

      if (bits & ALPHACHANNEL) alpha_channel = 1;
      
#ifdef TIFF_SUPPORT
      if (mode == 3)   /* tiff */
	{
	  if (*ofile != ' ')
	    ofile = strtok( ofile, " " );
	  else
	    ofile = "render.tif";
	  tfile=TIFFOpen(ofile,"w");
	  TIFFSetField(tfile,TIFFTAG_DOCUMENTNAME,ofile);
	  TIFFSetField(tfile,TIFFTAG_SOFTWARE,"Raster3D Version 2.4alpha");
	  TIFFSetField(tfile,TIFFTAG_BITSPERSAMPLE,8);
	  TIFFSetField(tfile,TIFFTAG_SAMPLESPERPIXEL,(alpha_channel ? 4 : 3));
	  TIFFSetField(tfile,TIFFTAG_PHOTOMETRIC,PHOTOMETRIC_RGB);
	  TIFFSetField(tfile,TIFFTAG_IMAGEWIDTH,xsize);
	  TIFFSetField(tfile,TIFFTAG_IMAGELENGTH,ysize);
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
	  
	  if (mode == 1)   /* original */
	    {
	      header[3] = xsize;
	      header[4] = ysize;
	      for (i=0; i<sizeof(header); i++)
		putchar(*c++);
	    }
      
	  else        /* avs */
	    {
#ifdef NETWORKBYTEORDER
	      putw( htonl(xsize), stdout );
	      putw( htonl(ysize), stdout );
#else
	      putw( xsize, stdout );
	      putw( ysize, stdout );
#endif
	    }
      
      break;
      
      
      /*      
	option 2: wite out a row of pixels depending on mode
	*/
      
    case 2:
      
#ifdef TIFF_SUPPORT
      if (mode ==3)
	{
	  my_write_tiff(tfile,buffer1,buffer2,buffer3,buffer4,xsize,scanline);
	}
      else
#endif
	
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
	  
	  if (mode == 1)	/* original RGB bytes */
	    {
	      for (i=0; i<xsize; i++)
		{
		  putchar( buffer1[i] );
		  putchar( buffer2[i] );
		  putchar( buffer3[i] );
		}
	    }
      
	  else      /* AVS image file (AlphaRGB) bytes */
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
      break;
      
      /* option 3: close output file if neccesary
       */
      
    case 3:
      
#ifdef TIFF_SUPPORT
      if (mode == 3)
	{
	  (void) TIFFFlushData(tfile);
#ifdef LINUX
	  signal( SIGSEGV, SIG_IGN );
#endif LINUX
	  (void) TIFFClose(tfile);
	}
#endif
      
#ifdef LIBIMAGE_SUPPORT
      if (mode == 2)
	iclose(image);
#endif

      break;
      
      /* option 4: add title to image file */
      
    case 4:
      
#ifdef TIFF_SUPPORT
      if (mode == 3)
	TIFFSetField(tfile,TIFFTAG_IMAGEDESCRIPTION,buffer1);
#endif
      
#ifdef LIBIMAGE_SUPPORT
      if (mode == 2)
	isetname(image,buffer1);
#endif
      break;
      
    }
  
  return( mode );
}

#ifdef TIFF_SUPPORT
void my_write_tiff(fp, buf1, buf2, buf3, buf4, size, scanline)
     TIFF *fp;
     short buf1[], buf2[], buf3[], buf4[];
     int   size;
     unsigned char scanline[];

{
static int row=0;
int i, j= -1;

  if (alpha_channel)
    for (i=0; i<size; i++) {
	scanline[j++] = buf4[i];
	scanline[j++] = buf1[i];
	scanline[j++] = buf2[i];
	scanline[j++] = buf3[i];
    }
  else
    for (i=0; i<size; i++) {
	scanline[j++] = buf3[i];
	scanline[j++] = buf1[i];
	scanline[j++] = buf2[i];
    }

if (TIFFWriteScanline(fp,scanline,row,0) < 0)
  fprintf (stderr, "\n", "Bad return code from TIFF write\n");
row++;
}
#endif
