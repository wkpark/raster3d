/*
 * Raster3D V2.0
 * local.c
 *
 * Output from render.f is performed by calls to routine LOCAL,
 * which is implemented here.
 *
 * This version of local.c supports 4 output modes, 2 of which
 * are controlled by conditional compilation directives -
 *	AVS image file sent to stdout
 *		(2 integer header followed by AlphaRGB bytes)
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
 * manuipulated using various other utilities provided under
 * 4DGifts/iristools/imgtools.
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
#endif

local_(option,buffer1,buffer2,buffer3)
     int	*option;
     short	*buffer1, *buffer2, *buffer3;
{
  
  /* Everyone needs these */
  static int	xsize, ysize;
  static int	mode= -1;
  int	        i;
  static char	*ofile;
  
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
   * First call (option=0) is to determine the output mode
   */
  
  if (*option == 0) {
    
    if (strncmp( (char *)buffer1, "-tiff", 5) == 0)
      {
#ifdef TIFF_SUPPORT
	mode  = 3;
	ofile = (char *)buffer2;
#else
	fprintf(stderr,"\n This version of render does not support -tiff\n");
	exit(-1);
#endif
      }
    else if (strncmp( (char *)buffer1, "-sgi" , 4) == 0)
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
      mode  = 1;
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
    return( mode );
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
      
#ifdef TIFF_SUPPORT
      if (mode == 3)   /* tiff */
	{
	  if (*ofile != ' ')
	    ofile = strtok( ofile, " " );
	  else
	    ofile = "render.tif";
	  tfile=TIFFOpen(ofile,"w");
	  TIFFSetField(tfile,TIFFTAG_DOCUMENTNAME,ofile);
	  TIFFSetField(tfile,TIFFTAG_SOFTWARE,"Raster3D Version 2.0");
	  TIFFSetField(tfile,TIFFTAG_BITSPERSAMPLE,8);
	  TIFFSetField(tfile,TIFFTAG_SAMPLESPERPIXEL,3);
	  TIFFSetField(tfile,TIFFTAG_PHOTOMETRIC,PHOTOMETRIC_RGB);
	  TIFFSetField(tfile,TIFFTAG_IMAGEWIDTH,xsize);
	  TIFFSetField(tfile,TIFFTAG_IMAGELENGTH,ysize);
/*	  TIFFSetField(tfile,TIFFTAG_FILLORDER,FILLORDER_MSB2LSB); */
	  TIFFSetField(tfile,TIFFTAG_ORIENTATION,ORIENTATION_BOTLEFT);
	  TIFFSetField(tfile,TIFFTAG_PLANARCONFIG,PLANARCONFIG_CONTIG);
	  TIFFSetField(tfile,TIFFTAG_COMPRESSION,COMPRESSION_LZW);
	  rows_per_strip=8192/TIFFScanlineSize(tfile);
	  if (rows_per_strip == 0)
	    rows_per_strip=1;
	  TIFFSetField(tfile,TIFFTAG_ROWSPERSTRIP,rows_per_strip);
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
	    image = iopen(ofile,"w",RLE(1),3,xsize,ysize,3);
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
	      putw( xsize, stdout );
	      putw( ysize, stdout );
	    }
      
      break;
      
      
      /*      
	option 2: wite out a row of pixels depending on mode
	*/
      
    case 2:
      
#ifdef TIFF_SUPPORT
      if (mode ==3)
	{
	  my_write_tiff(tfile,buffer1,buffer2,buffer3,xsize,scanline);
	}
      else
#endif
	
#ifdef LIBIMAGE_SUPPORT  
	if (mode == 2)      /* -sgi option (libimage format) */
	  {
	    putrow(image,buffer1,row,0);
	    putrow(image,buffer2,row,1);
	    putrow(image,buffer3,row,2);
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
	      for (i=0; i<xsize; i++)
		{
		  putchar( 0 );
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
void my_write_tiff(fp, buf1, buf2, buf3, size, scanline)
     TIFF *fp;
     short buf1[], buf2[], buf3[];
     int   size;
     unsigned char scanline[];

{
static int row=0;
int i, j= -1;

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
