/*
 * Raster3D V2.4(alpha)
 * local.c
 *
 * Output from render.f is performed by calls to routine LOCAL,
 * which is implemented here.
 *
 * This version of local.c sends a JPEG image file to stdout
 * (no other output modes supported)
 *
 * command line options
 *		-debug  passes flag back to render
 *		-quality JPEG encoding quality (1-100)
 *		-aaN, 	where N=(0,1,2,3,4) forces anti-aliasing option
 *		-invert	invert y coordinate axis
 */

#include	<stdio.h>
#include	<fcntl.h>
#include	<string.h>

#include	<jpeglib.h>
#include	<jerror.h>

/* Define bits in returned status */
#define		ANTIALIAS	007
#define		INVERT		010
#define		DEBUGGING	020

/* HPUX lacks Fortran intrinsic functions AND and OR for some reason, */
/* so I put a copy here. On the other hand HPUX has an unusually sane */
/* calling convention for Fortran subroutine names.                   */
#ifdef __hpux
#define local_ local
int and(i,j) int *i,*j; {return (*i & *j);}
int or(i,j)  int *i,*j; {return (*i | *j);}
#endif

int		alpha_channel = 0;
char *version = "Raster3D V2.4a\n" ;

/* JPEG data structures */
  struct jpeg_compress_struct	cinfo;
  struct jpeg_error_mgr      	jerr;
  JSAMPLE			*jpeg_pixels;
  JSAMPROW			jpeg_row[1];

local_(option,buffer1,buffer2,buffer3,buffer4)
     int	*option;
     short	*buffer1, *buffer2, *buffer3, *buffer4;
{
  
  /* Everyone needs these */
  static int	xsize, ysize;
  static int	mode= -1;
  static int	quality = 95;
  int	        i;
  int		status;
  int		invert;
  
  JSAMPLE	*q;

  /*
   * First call (option=0) is to determine the output mode.
   * As of V2.2.1 multiple bits may be set in the value returned.
   */
  
  if (*option == 0) {
   
    status  =  0;
    invert  =  0;

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

    if (strncmp( (char *)buffer1, "-invert", 7) ==0)
      {
      invert = !invert;
      buffer1 = buffer2;   buffer2 = buffer3;
      }

    if (strncmp( (char *)buffer1, "-quality", 8) == 0)
      {
      i = sscanf(buffer2,"%d",&quality);
      if (i != 1 || quality <= 0 || quality > 100) {
	fprintf(stderr,"\n JPEG quality must be an integer from 1 to 100\n");
	exit(-1);
	}

      buffer1 = buffer3;   buffer2 = buffer4;
      }
    
    if (strncmp( (char *)buffer1, "-tiff", 5) == 0)
      {
	fprintf(stderr,
		"\n This copy of render was not built with -tiff library\n");
	exit(-1);
      }
    else if (strncmp( (char *)buffer1, "  ", 2) != 0)
      {
	fprintf(stderr,
		"\n\n Unfamiliar switch: %12.12s", buffer1);
	fprintf(stderr,
		"\n Usage: render [-quality xx] < infile.r3d > outfile.jpeg\n");
	exit(-1);
      }
    else /* default jpeg mode */
      mode  = 4;

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
      
    case 1: /* open image file for output depending on mode */
      
      xsize = *buffer1;
      ysize = *buffer2;
      
	/* JPEG */
	{
	jpeg_pixels = (JSAMPLE *) malloc(xsize*3*sizeof(JSAMPLE));
	if (jpeg_pixels == (JSAMPLE *) NULL)
	    {
	      fprintf(stderr,"\nMemory allocation error\n");
	      return(-1);
	    }
	cinfo.err = jpeg_std_error(&jerr);
	jpeg_create_compress(&cinfo);
/*	jpeg_stdio_dest(&cinfo,fopen("test.jpeg","wb")); */
	jpeg_stdio_dest(&cinfo,stdout);
	cinfo.image_width      = xsize;
	cinfo.image_height     = ysize;
	cinfo.input_components = 3;
	cinfo.in_color_space   = JCS_RGB;
	jpeg_set_defaults(&cinfo);
	jpeg_set_quality(&cinfo, quality, TRUE );

/* ImageMagick sets the following, but I don't understand why */
/*	for (i=0; i<3; i++)
	    {
	    cinfo.comp_info[i].h_samp_factor = 1;
	    cinfo.comp_info[i].v_samp_factor = 1;
	    }
 */
/* The following choices affect efficiency on a given machine */
/*	cinfo.optimize_coding	= TRUE; */
	cinfo.dct_method	= JDCT_FLOAT;

	jpeg_simple_progression(&cinfo);
	jpeg_start_compress(&cinfo, TRUE);
	}
      
      break;
      
      
    case 2: /* write out a row of pixels depending on mode */
      
      /* JPEG data */
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

      break;
      
      
    case 3: /* close output file if neccesary */
      
      /* JPEG */
	{
	jpeg_finish_compress(&cinfo);
	}
      
      break;
      
      
    case 4: /* add title to image file */
      
      /* JPEG */
	{
	jpeg_write_marker(&cinfo,JPEG_COM,(unsigned char *)version,strlen(version));
	jpeg_write_marker(&cinfo,JPEG_COM,(unsigned char *)buffer1,80);
	}
      
      break;
      
    }
  
  return( mode );
}
