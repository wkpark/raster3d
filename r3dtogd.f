*******************************************************************************
*               Support routines for libgd labels                             *
*******************************************************************************
*     Version 3.0
*
* PCC Apr 2010	- Initial version (some code came from r3dtops.f)
* PCC May 2010	- Allow for LABELSTRING to have null pointers
* EAM Dec 2010	- Fix antialiasing scales
*******************************************************************************
*
* These routines are called from render.f to handle object types 10, 11 and 12.
* The parsed information will be passed onto the mylabel_() function in local.c
*
* The "libgd file" describes a canvas with the same dimension in pixels as
* the image created by render. The libgd canvas can be composited on top
* of the rendered image to produce a labeled figure.
*
*	Object types 10 and 11 are used for specifying labels.
*	Label object types are
*	  - type 10: Font_Name size alignment
*	  - type 11: XYZ RGB on first line
*		     label (ascii characters enclosed in quotes) on second line
*	Object type 12 is reserved to go with this, as I have a nagging
*	suspicion more information may turn out to be necessary.
*
*******************************************************************************
      SUBROUTINE LOPEN( FILENAME )
      CHARACTER*132 FILENAME
      RETURN
      END

      SUBROUTINE LCLOSE( )
      RETURN
      END

      SUBROUTINE LSETUP( PSCALE, BKGND, TITLE )
      REAL     PSCALE
      REAL     BKGND(3)
      CHARACTER*80 TITLE
      COMMON /OPTIONS/ FONTSCALE, GAMMA, ZOOM, NSCHEME, SHADOWFLAG, XBG,
     &                 NAX, NAY, OTMODE, QUALITY, INVERT, LFLAG
      REAL             FONTSCALE, GAMMA, ZOOM
      INTEGER          NSCHEME, SHADOWFLAG, XBG
      INTEGER*4        NAX, NAY, OTMODE, QUALITY
      LOGICAL*2        INVERT, LFLAG
C     Might as well always handle labels?
C	LFLAG = .TRUE.
      RETURN
      END
*******************************************************************************

      SUBROUTINE LINP( INPUT, INTYPE, MATCOL, RGBMAT )
      IMPLICIT NONE

      INTEGER  I, J, LEN
      INTEGER  INPUT, INTYPE
      LOGICAL  MATCOL
      REAL     RGBMAT(3)
      REAL     AASCALE
*
*     Input transformation
      COMMON /MATRICES/ XCENT, YCENT, SCALE, EYEPOS, SXCENT, SYCENT,
     &                  TMAT, TINV, TINVT, SROT, SRTINV, SRTINVT
     &                 ,RAFTER, TAFTER
      REAL   XCENT, YCENT, SCALE, SXCENT, SYCENT
*     Transformation matrix, inverse of transpose, and transposed inverse
      REAL   TMAT(4,4), TINV(4,4), TINVT(4,4)
*     Shortest rotation from light source to +z axis
      REAL   SROT(4,4), SRTINV(4,4), SRTINVT(4,4)
*     Post-hoc transformation on top of original TMAT
      REAL   RAFTER(4,4), TAFTER(3)
      EXTERNAL DET
      REAL     DET
*     Distance (in +z) of viewing eye
      REAL   EYEPOS
*
      EXTERNAL PERSP
      REAL     PERSP, PFAC
*
*     Stuff for labels
      CHARACTER*80  FONTNAME, FONTALIGN
      CHARACTER*128 LABELSTRING
      SAVE          FONTNAME
      SAVE          LABELSTRING
      INTEGER       MAXLABLEN
      PARAMETER    (MAXLABLEN = 128)
      INTEGER      IALIGN
      SAVE         IALIGN
      INTEGER      FONT, LABEL
      PARAMETER   (FONT = 10, LABEL = 11)
      REAL         XA, YA, ZA, RED, GRN, BLU
      REAL         FONTSIZE
      SAVE         FONTSIZE
*
*     Keep track of actual coordinate limits
      COMMON /NICETIES/ TRULIM, ZLIM, FRONTCLIP, BACKCLIP, ISOLATION
      REAL              TRULIM(3,2), ZLIM(2), FRONTCLIP, BACKCLIP
      INTEGER           ISOLATION
*
*     Command line options (Aug 1999) NB: nax,nay,quality MUST be integer*2
      COMMON /OPTIONS/ FONTSCALE, GAMMA, ZOOM, NSCHEME, SHADOWFLAG, XBG,
     &                 NAX, NAY, OTMODE, QUALITY, INVERT, LFLAG
      REAL             FONTSCALE, GAMMA, ZOOM
      INTEGER          NSCHEME, SHADOWFLAG, XBG
      INTEGER*4        NAX, NAY, OTMODE, QUALITY
      LOGICAL*2        INVERT, LFLAG
*
*     Copy of NOISE for ASSERT to see
      COMMON /ASSCOM/ NOISE, VERBOSE
      INTEGER NOISE
      LOGICAL VERBOSE

c*****DEFAULTS:
c FONTSCALE = 1.0
c
c     Read in next object
      IF (INTYPE .EQ. FONT) THEN
	READ (INPUT,*,END=50) FONTNAME, FONTSIZE, FONTALIGN
	IF (FONTALIGN(1:1).EQ.'C') THEN
	    IALIGN=1
	ELSE IF (FONTALIGN(1:1).EQ.'R') THEN
	    IALIGN=2
	ELSE IF (FONTALIGN(1:1).EQ.'O') THEN
	    IALIGN=3
	ELSE
	    IALIGN=0
	ENDIF
c
* TODO: The following can probably be removed
c	len = 0
c	DO i=1,80
c	    if (fontname(i:i).ne.' ') len = i
c	enddo

      ELSE IF (INTYPE .EQ. LABEL ) THEN
	READ (INPUT,*,END=50) XA, YA, ZA, RED, GRN, BLU
	IF (MATCOL) THEN
	    RED = RGBMAT(1)
	    GRN = RGBMAT(2)
	    BLU = RGBMAT(3)
	ENDIF
c
c       Here is where Perl would shine
c
	READ (INPUT,'(A)',END=50) LABELSTRING
	len = len_trim(LABELSTRING)
c
c Isolated objects not transformed by TMAT, but still subject to inversion.
        IF (ISOLATION.GT.0) THEN
          IF (INVERT) YA = -YA
	  if (isolation.eq.2) then
	    if (xcent.gt.ycent) xa = xa * xcent / ycent
	    if (xcent.lt.ycent) ya = ya * ycent / xcent
	  endif
        ELSE
c modify the input, as it were
	  IF (IALIGN.NE.3) THEN
	    CALL TRANSF (XA,YA,ZA, TMAT)
c           YA = -YA
	  ENDIF
        ENDIF
c perspective
        IF (EYEPOS.GT.0) THEN
	    PFAC = PERSP(ZA)
	ELSE
	    PFAC = 1.0
	ENDIF
c
	AASCALE = 1.0
	IF (NSCHEME.EQ.2) AASCALE = 0.5
	IF (NSCHEME.EQ.3) AASCALE = 2./3.
	IF (NSCHEME.EQ.4) AASCALE = 2./3.

	IF (IALIGN.EQ.3) THEN
	    XA = XA * SCALE
	    YA = YA * SCALE
	ELSE IF (ISOLATION.GT.0) THEN
	    XA = XA * SCALE + XCENT
	    YA = YA * SCALE + YCENT
	ELSE
c scale and translate to pixel space
	    XA = XA * PFAC * SCALE + XCENT
	    YA = YA * PFAC * SCALE + YCENT
	    ZA = ZA
	ENDIF

c allow for the antialiasing
	XA = XA * AASCALE
	YA = YA * AASCALE
	ZA = ZA * AASCALE

c
c 	IF (ZA * SCALE .LT. BACKCLIP .OR. ZA * SCALE .GT. FRONTCLIP) RETURN
 
	CALL CHKRGB( RED, GRN, BLU, 'invalid label color')
c	WRITE (0,*) 'COLOR VALUES = ', RED, GRN, BLU
	RED = SQRT(RED)
	GRN = SQRT(GRN)
	BLU = SQRT(BLU)
c
c
C =============================================================================
C Ready to pass on information to libgd via local.c
c
	CALL ADDLABEL(FONTNAME//CHAR(0), FONTSIZE, FONTSCALE, IALIGN,
     &  XA,YA,ZA, RED,GRN,BLU, LABELSTRING//CHAR(0))
c
C =============================================================================
c
c
  800   FORMAT(A,'-x,y,z: ',3F10.3)
  801   FORMAT(A,4F10.1)
      ENDIF
      RETURN

c
c Error handling
c
 50     WRITE (NOISE,*) '>>> Unrecognized label command'
        RETURN

        END
