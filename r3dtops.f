*******************************************************************************
*               Support routines for PostScript labels                        *
*******************************************************************************
*     Version 2.5b
*
* EAM Dec 1996	- Initial version (called labels3d, later changed)
* EAM May 1999	- Updated to match V 2.4j as stand-alone program
* EAM Nov 1999	- V2.5 called from render.f as part of normal processing
*******************************************************************************
*
* These routines are called from render.f to handle object types 10, 11 and 12.
* The PostScript file describes a canvas with the same dimension in pixels as
* the image created by render.  The PostScript canvas can be composited on top
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
*
      IMPLICIT NONE
      REAL     PSCALE
      REAL     BKGND(3)
      CHARACTER*80 FILENAME
      CHARACTER*80 TITLE
*
*
      INTEGER  I, J, LEN, IBEG
      INTEGER  INPUT, INTYPE, KEEP
*
*     Input transformation
      COMMON /MATRICES/ XCENT, YCENT, SCALE, EYEPOS, SXCENT, SYCENT,
     &                  TMAT, TINV, TINVT, SROT, SRTINV, SRTINVT
      REAL   XCENT, YCENT, SCALE, SXCENT, SYCENT
*     Transformation matrix, inverse, and transponsed inverse
      REAL   TMAT(4,4), TINV(4,4), TINVT(4,4)
*     Shortest rotation from light source to +z axis
      REAL   SROT(4,4), SRTINV(4,4), SRTINVT(4,4)
*     Distance (in +z) of viewing eye
      REAL   EYEPOS
*
      EXTERNAL PERSP
      REAL     PERSP, PFAC
*
      COMMON /NICETIES/ TRULIM,      ZLIM,    FRONTCLIP, BACKCLIP
     &                , ISOLATE
      REAL              TRULIM(3,2), ZLIM(2), FRONTCLIP, BACKCLIP
      LOGICAL           ISOLATE
*
*     Command line options
      COMMON /OPTIONS/ NSCHEME, NAX, NAY, INVERT, OTMODE, QUALITY
     &               , LFLAG, FONTSCALE
      INTEGER          NSCHEME
      INTEGER*2        NAX, NAY, OTMODE, QUALITY
      LOGICAL          INVERT, LFLAG
      REAL             FONTSCALE
*
*     Stuff for labels
      COMMON /LABELS/ LABOUT
      INTEGER         LABOUT

      CHARACTER*80 FONTNAME, LABELSTRING, FONTALIGN
      INTEGER      IALIGN
      SAVE         IALIGN
      INTEGER      FONT, LABEL
      PARAMETER   (FONT = 10, LABEL = 11)
      REAL         XA, YA, ZA, RED, GRN, BLU
      REAL         FONTSIZE
      REAL         PSSCALE
      SAVE         PSSCALE
*
*     Copy of NOISE for ASSERT to see
      COMMON /ASSCOM/ NOISE, VERBOSE
      INTEGER NOISE
      LOGICAL VERBOSE
*
*     Initial entry
*     Open file for PostScript output
*
      DO I = 80,2,-1
         IF (FILENAME(I:I).EQ.' ') LEN = I - 1
      END DO
      OPEN( UNIT=LABOUT, FILE=FILENAME(1:LEN), STATUS='UNKNOWN', ERR=99)
      WRITE (NOISE,*) 'Writing PostScript labels to file ',
     &                FILENAME(1:LEN)
      RETURN
   99 CONTINUE
      WRITE (NOISE,100) FILENAME
  100 FORMAT('>>> Cannot open ',A,' for writing labels')
      CALL EXIT
*
*     Don't write PostScript header until we've read R3D header
*
      ENTRY LSETUP( PSCALE, BKGND, TITLE )
	PSSCALE = PSCALE
*     For some reason ImageMagick messes up image composition if the
*     background is pure white. Work-around is to tweak the background.
	if (bkgnd(1).eq.1.0) bkgnd(1) = 0.99
	RED = sqrt( bkgnd(1) )
	GRN = sqrt( bkgnd(2) )
	BLU = sqrt( bkgnd(3) )
*
c
c     Write out PostScript prolog records
c	To be minimally-conforming, there should also be a 
c	%%DocumentFonts: (atend)
c	record and record-keeping of all fonts used.
c
600   FORMAT(A,1X,A)
601   FORMAT(A,I6,A)
602   FORMAT(A,2I6,A)
603   FORMAT(A,F6.3,A)
604   FORMAT(3F6.3,A)
605   FORMAT('/',A,' findfont',F6.2,' FontScale setfont')

      WRITE(LABOUT,600) '%!PS-Adobe-3.0'
      WRITE(LABOUT,600) '%%Creator: Raster3D V2.5b rendering program'
      WRITE(LABOUT,600) '%%Title:',TITLE
      WRITE(LABOUT,600) '%%Pages: 1'
      WRITE(LABOUT,602) '%%BoundingBox: 0 0',nax,nay
      WRITE(LABOUT,600) '%%DocumentFonts: (atend)'
      WRITE(LABOUT,600) '%%EndComments'
      WRITE(LABOUT,600) '%%BeginProlog'
      WRITE(LABOUT,600) '% These are the only control parameters'
      WRITE(LABOUT,603) '/FontSize ',FONTSCALE,' def'
      WRITE(LABOUT,601) '/UnitHeight ',nay/2,' def'
      WRITE(LABOUT,601) '/UnitWidth  ',nax/2,' def'
      WRITE(LABOUT,601) '% '
      WRITE(LABOUT,601) '% This should be dynamic, but how???'
      WRITE(LABOUT,601) '/FontHeight 30 def'
      WRITE(LABOUT,601) '/FontWidth  30 def'
      WRITE(LABOUT,601) '% '
      WRITE(LABOUT,601) '/FontScale { FontSize mul scalefont '
      WRITE(LABOUT,601) ' } bind def'
      WRITE(LABOUT,601) '/Center {'
      WRITE(LABOUT,601) ' dup stringwidth'
      WRITE(LABOUT,601) ' exch -2 div exch -2 div rmoveto'
      WRITE(LABOUT,601) ' } bind def'
      WRITE(LABOUT,601) '/Right {'
      WRITE(LABOUT,601) ' dup stringwidth'
      WRITE(LABOUT,601) ' exch -1 mul exch -1 mul rmoveto'
      WRITE(LABOUT,601) ' } bind def'
      WRITE(LABOUT,601) '/XYZmove {'
      WRITE(LABOUT,601) '  pop moveto'
      WRITE(LABOUT,601) ' } bind def'
      WRITE(LABOUT,601) '/XYZrmove {'
      WRITE(LABOUT,601) '  pop rmoveto'
      WRITE(LABOUT,601) ' } bind def'
      WRITE(LABOUT,601) '%%EndProlog'
      WRITE(LABOUT,601) '%%BeginSetup'
      WRITE(LABOUT,601) 'gsave'
      WRITE(LABOUT,601) 'UnitWidth UnitHeight translate'
      WRITE(LABOUT,604) RED,GRN,BLU,' setrgbcolor'
      WRITE(LABOUT,601) 
     &	'UnitWidth -1 mul dup UnitHeight -1 mul newpath moveto'
      WRITE(LABOUT,601)
     &	'UnitWidth UnitHeight -1 mul lineto UnitWidth UnitHeight lineto'
      WRITE(LABOUT,601) 'UnitHeight lineto closepath fill'
      WRITE(LABOUT,605) 'Times-Bold',10.
      WRITE(LABOUT,601) '%%Endsetup'
      WRITE(LABOUT,601) '%%Page: 1 1'
     
      RETURN


      ENTRY LINP( INPUT, INTYPE )
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
c	Here is where Perl would shine
c
	DO i=1,80
	    if (fontname(i:i).ne.' ') len = i
	enddo
	WRITE (LABOUT,701) FONTNAME(1:len), FONTSIZE
701	FORMAT('/',A,' findfont',F6.2,' FontScale setfont')

      ELSE IF (INTYPE .EQ. LABEL ) THEN
	READ (INPUT,*,END=50) XA, YA, ZA, RED, GRN, BLU
c
c	Here is where Perl would shine
c
	READ (INPUT,'(A)',END=50) LABELSTRING
	do j= 80,1,-1
	    len = j
	    if (LABELSTRING(len:len).ne.' ') goto 702
	enddo
702	continue
c
c       Isolated objects not transformed by TMAT, but still subject to inversion.
c       Then again, PostScript y-axis convention is upside-down from screen coords.
        IF (ISOLATE) THEN
          IF (.not.INVERT) YA = -YA
        ELSE
c         modify the input, as it were
	  IF (IALIGN.NE.3) THEN
	    CALL TRANSF (XA,YA,ZA, TMAT)
            YA = -YA
	  ENDIF
        ENDIF
c       perspective
        IF (EYEPOS.GT.0) THEN
	    PFAC = PERSP(ZA)
	ELSE
	    PFAC = 1.0
	ENDIF
c
	XA = XA * PFAC * PSSCALE
	YA = YA * PFAC * PSSCALE
	ZA = ZA * PFAC * PSSCALE
c
	IF  (ZA * (SCALE/PSSCALE) .LT. BACKCLIP 
     &  .OR. ZA * (SCALE/PSSCALE) .GT. FRONTCLIP) RETURN
c
	CALL CHKRGB( RED, GRN, BLU, 'invalid label color')
	RED = SQRT(RED)
	GRN = SQRT(GRN)
	BLU = SQRT(BLU)
c
	IF (IALIGN.EQ.3) THEN
	    WRITE (LABOUT,802) RED,GRN,BLU,XA,YA,ZA
	ELSE
	    WRITE (LABOUT,801) RED,GRN,BLU,XA,YA,ZA
	ENDIF
801	FORMAT(3f6.3,' setrgbcolor',3f10.4,' XYZmove')
802	FORMAT(3f6.3,' setrgbcolor',3f10.4,' XYZrmove')
c
c	At this point I should loop over string looking for
c	escape sequences, control characters, etc.
c
	IBEG = 1
  81	CONTINUE
  	DO I = IBEG, LEN-1
	  IF  ( LABELSTRING(I:I)    .EQ.'\\'
     &    .AND. LABELSTRING(I+1:I+1).EQ.'n') THEN
	    WRITE(LABOUT,601) 'gsave'
	    IF (IALIGN.EQ.1) THEN
	        WRITE (LABOUT,803) LABELSTRING(IBEG:I-1),'Center'
	    ELSE IF (IALIGN.EQ.2) THEN
	        WRITE (LABOUT,803) LABELSTRING(IBEG:I-1),'Right'
	    ELSE
	        WRITE (LABOUT,803) LABELSTRING(IBEG:I-1),' '
	    ENDIF
	    WRITE(LABOUT,601) 'grestore'
	    WRITE(LABOUT,601) '0 FontHeight -1 mul rmoveto'
	    IBEG = I+2
	    GOTO 81
	  ENDIF
	  IF  ( LABELSTRING(I:I)    .EQ.'\\'
     &    .AND. LABELSTRING(I+1:I+1).EQ.'v') THEN
	    IF (IALIGN.EQ.1) THEN
	        WRITE (LABOUT,803) LABELSTRING(IBEG:I-1),'Center'
	    ELSE IF (IALIGN.EQ.2) THEN
	        WRITE (LABOUT,803) LABELSTRING(IBEG:I-1),'Right'
	    ELSE
	        WRITE (LABOUT,803) LABELSTRING(IBEG:I-1),' '
	    ENDIF
	    WRITE(LABOUT,601) '0 FontHeight 0.5 mul rmoveto'
	    IBEG = I+2
	    GOTO 81
	  ENDIF
	  IF  ( LABELSTRING(I:I)    .EQ.'\\'
     &    .AND. LABELSTRING(I+1:I+1).EQ.'b') THEN
	    IF (IALIGN.EQ.1) THEN
	        WRITE (LABOUT,803) LABELSTRING(IBEG:I-1),'Center'
	    ELSE IF (IALIGN.EQ.2) THEN
	        WRITE (LABOUT,803) LABELSTRING(IBEG:I-1),'Right'
	    ELSE
	        WRITE (LABOUT,803) LABELSTRING(IBEG:I-1),' '
	    ENDIF
	    WRITE(LABOUT,601) 'FontWidth -0.5 mul 0 rmoveto'
	    IBEG = I+2
	    GOTO 81
	  ENDIF
	ENDDO
c
c	End proposed escape interpretation loop
c
	IF (IALIGN.EQ.1) THEN
	    WRITE (LABOUT,803) LABELSTRING(IBEG:LEN),'Center'
	ELSE IF (IALIGN.EQ.2) THEN
	    WRITE (LABOUT,803) LABELSTRING(IBEG:LEN),'Right'
	ELSE
	    WRITE (LABOUT,803) LABELSTRING(IBEG:LEN),' '
	ENDIF
803	FORMAT('(',A,') ',A6,'  show')
      ENDIF
      
      RETURN

c
c Error handling
c
 50	WRITE (NOISE,*) '>>> Unrecognized label command'
 	RETURN



c
c All done, finish off PostScript file and report success
c
      ENTRY LCLOSE( KEEP )
c
c     Finish off PostScript output
      WRITE (LABOUT,600) '%'
      WRITE (LABOUT,600) 'showpage'
      WRITE (LABOUT,600) '%%Trailer'
      WRITE (LABOUT,600) '%%DocumentFonts: Times-Bold'
      WRITE (LABOUT,600) '%%EOF'

      IF (KEEP.GT.0) THEN
          CLOSE (UNIT=LABOUT)
      ELSE
          CLOSE (UNIT=LABOUT,STATUS='DELETE')
      ENDIF
*
      end
