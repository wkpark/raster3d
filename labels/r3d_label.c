/*
 * This bit of code is inspired by Molscript V2
 * It's just for me to get a feel for how one uses the stroke font commands
 * in GLUT to generate characters.
 *
 * I've coded in recognition of escape sequences \n (newline) \b (backspace)
 * and \t (tab = 4 spaces).  If I were to eventually go with this approach,
 * I'd probably want a full set of TeX/LaTeX style markups.
 * So "C_{i} -- C_{i+1}" would produce a subscripts and an "em" dash.
 * This wouldn't really be very hard, but is pointless until the font selection
 * is better.
 *
 * glutstroke.h is from the GLUT distribution
 *
 * Here is a suggested make command
 *  r3d_label:	r3d_label.c
 *		cc -o r3d_label -I/usr/src/glut-3.6/lib/glut/ r3d_label.c -lglut
 *
 * parameters:
 *	label	   is a pointer to a null-terminated string of ascii chars
 *	p[3] 	   holds the x,y,z coordinates of the label start point
 *	labelsize  is a multiplier for the intrinsic font size
 *	rgb[3]	   holds the red/green/blue components of the color
 *
 */
#include <stdio.h>
#include "glutstroke.h"
extern StrokeFontRec glutStrokeRoman;

#define Vinit( V,V1,V2,V3) {V[0]=V1; V[1]=V2; V[2]=V3;}
#define Vscale(V,scale)    {V[0]*=scale; V[1]*=scale; V[2]*=scale;}
#define Vcopy( V,U)        {V[0]=U[0]; V[1]=U[1]; V[2]=U[2];}
#define Vadd(  V,U)        {V[0]+=U[0]; V[1]+=U[1]; V[2]+=U[2];}

#define FONTSCALE_FACTOR 2.75e-5
#define RADIUS_FACTOR    2.40e-4
#define CONSTANT         5.0

void
r3d_label (char *label, float p[3], float labelsize, float rgb[3])
{
  const StrokeCharRec *ch;
  const StrokeRec     *stroke;
  const CoordRec      *coord;
  StrokeFontPtr        fontinfo = &glutStrokeRoman;

  extern float labeloffset[3];
  int    i, j;
  float  pos1[3], pos2[3];
  float  font_scale, radius;
  float  spacing = 10.0;
  float  offset  = 0.0;

  if (!label || !*label) return;


    font_scale = FONTSCALE_FACTOR * CONSTANT * labelsize;
    radius     = RADIUS_FACTOR    * CONSTANT * labelsize;


  for (; *label; label++) {
    if (*label >= fontinfo->num_chars) continue;

    if (*label == '\\') {
	label++;
	if (*label == 'n') {
	  offset = 0.0;
	  p[1] -= font_scale * (fontinfo->top + spacing);
	  continue;
	} else if (*label == 'b') {
	  offset -= font_scale * (ch->right + spacing);
	  continue;
	} else if (*label == 't') {
	  ch = &(fontinfo->ch[' ']);
	  offset += font_scale * 4 * (ch->right + spacing);
	  continue;
	}
    }

    ch = &(fontinfo->ch[*label]);
    if (ch) {
      for (i = ch->num_strokes, stroke = ch->stroke; i > 0; i--, stroke++) {
	coord = stroke->coord;
	Vinit (pos1, coord->x, coord->y, 0.0);
	Vscale (pos1, font_scale);
	Vadd (pos1, p);
	Vadd (pos1, labeloffset);
	pos1[0] += offset;
	coord++;
	for (j = stroke->num_coords - 1; j > 0; j--, coord++) {
	  Vinit (pos2, coord->x, coord->y, 0.0);
	  Vscale (pos2, font_scale);
	  Vadd (pos2, p);
	  Vadd (pos2, labeloffset);
	  pos2[0] += offset;

	  printf( "3\n %8.4f %8.4f %8.4f %8.4f   %8.4f %8.4f %8.4f %8.4f",
		   pos1[0], pos1[1], pos1[2], radius,
		   pos2[0], pos2[1], pos2[2], radius);
	  printf( " %5.3f %5.3f %5.3f\n", rgb[0], rgb[1], rgb[2] );

	  Vcopy (pos1, pos2);
	}
      }
      offset += font_scale * (ch->right + spacing);
    }
  }

}


/* This would normally be exported by the calling program */
float   labeloffset[3];	

/*
 * Simple test program to convert command line arg to a label
 * in the form of a Raster3D input stream containing cylinders tracing
 * out the font strokes.
 */
main( int argc, char *argv[] )
{
float where[3];
float rgb[3];
float size;

	if ((argc < 3) || !(sscanf(argv[1],"%f",&size))) {
	    fprintf(stderr,"usage: r3d_label fontsize 'label' > objfile.r3d\n"); exit(-1);
	    }
	fprintf(stderr,"Converting size %4.1f label:\n\t %s\n",size,argv[2]);

	/* Specify an xyz coordinate, a font size, and a color */
	Vinit( where, -0.5, 0.1, 0.0 );
	Vinit( rgb, 1.0, 0.5, 0.0 );

	/* Write identifying comment to output stream, followed by the label itself */
	printf( "# BEGIN r3d_label: %s\n",argv[2] );
	r3d_label( argv[2], where, size, rgb );
	printf( "# END   r3d_label \n" );

}

