*******************************************************************************
*               Support routines for PostScript labels                        *
*******************************************************************************
*     Version 2.5d
*
* EAM Dec 1996	- Initial version (called labels3d, later changed)
* EAM May 1999	- Updated to match V 2.4j as stand-alone program
* EAM Nov 1999	- V2.5 called from render.f as part of normal processing
* EAM Feb 2000	- iso-8859-1 encodings for Å
*		  TeX-like syntax for greek, superscript, subscript
*		  sub- and super- scripts use 0.8 * current font size
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
      INCLUDE 'VERSION.incl'
*
      INTEGER  I, J, LEN, IBEG
      INTEGER  INPUT, INTYPE, KEEP
      LOGICAL  MATCOL
      REAL     RGBMAT(3)
*
*     Input transformation
      COMMON /MATRICES/ XCENT, YCENT, SCALE, EYEPOS, SXCENT, SYCENT,
     &                  TMAT, TINV, TINVT, SROT, SRTINV, SRTINVT
      REAL   XCENT, YCENT, SCALE, SXCENT, SYCENT
*     Transformation matrix, inverse, and transposed inverse
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
     &                , ISOLATION
      REAL              TRULIM(3,2), ZLIM(2), FRONTCLIP, BACKCLIP
      INTEGER           ISOLATION
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
      COMMON /LABELS/ LB
      INTEGER         LB

      CHARACTER*80  FONTNAME, FONTALIGN
      CHARACTER*128 LABELSTRING
      INTEGER       MAXLABLEN
      PARAMETER    (MAXLABLEN = 128)
      CHARACTER*1   LTEX,TEXSTRING
      EXTERNAL      LTEX
      INTEGER      LEVEL
      REAL         SSSIZE
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
      OPEN( UNIT=LB, FILE=FILENAME(1:LEN), STATUS='UNKNOWN', ERR=99)
      WRITE (NOISE,*) 'Writing PostScript labels to file ',
     &                FILENAME(1:LEN),' with scale',FONTSCALE
      RETURN
   99 CONTINUE
      WRITE (NOISE,100) FILENAME
  100 FORMAT('>>> Cannot open ',A,' for writing labels')
      CALL EXIT(-1)
*
*     Don't write PostScript header until we've read R3D header
*
      ENTRY LSETUP( PSCALE, BKGND, TITLE )
	PSSCALE = PSCALE
*     For some reason ImageMagick messes up image composition if the
*     background is pure white or pure black. 
*     Work-around is to tweak the background.
	if (bkgnd(1).eq.1.0) bkgnd(1) = 0.99
	if (bkgnd(1).eq.0.0) bkgnd(1) = 0.01
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
6     FORMAT(A,1X,A)
600   FORMAT(A,1X,A,1X,A)
601   FORMAT(A,I6,A)
602   FORMAT(A,2I6,A)
603   FORMAT(A,F6.3,A)
604   FORMAT(3F6.3,A)
605   FORMAT('/',A,' findfont',F6.2,' FontScale setfont')
606   FORMAT('/CurrentFont /',A,' def /CurrentSize ',F6.2,' def ',A)
607   FORMAT((A))

      WRITE(LB,600) '%!PS-Adobe-3.0 EPSF-3.0'
      WRITE(LB,600) '%%Creator: Raster3D',VERSION,'rendering program'
      WRITE(LB,600) '%%Title:',TITLE
      WRITE(LB,600) '%%Pages: 1'
      WRITE(LB,602) '%%BoundingBox: 0 0',nax,nay
      WRITE(LB,600) '%%DocumentFonts: (atend)'
      WRITE(LB,600) '%%EndComments'
      WRITE(LB,600) '%%BeginProlog'
      WRITE(LB,600) '% These are the only control parameters'
      WRITE(LB,603) '/FontSize ',FONTSCALE,' def'
      WRITE(LB,601) '/UnitHeight ',nay/2,' def'
      WRITE(LB,601) '/UnitWidth  ',nax/2,' def'
      WRITE(LB,607) '% ',
     & '% This should be dynamic, but how???',
     & '/FontHeight 30 def',
     & '/FontWidth  30 def',
     & '% ',
     & '/FontScale { FontSize mul scalefont } bind def',
     & '/Center {',
     & ' dup stringwidth exch -2 div exch -2 div rmoveto',
     & ' } bind def',
     & '/Right {',
     & ' dup stringwidth exch -1 mul exch -1 mul rmoveto',
     & ' } bind def',
     & '/Skip { stringwidth 1.1 mul rmoveto } bind def',
     & '/ShrinkFont {',
     & '  CurrentFont findfont CurrentSize 0.8 mul FontScale setfont',
     & ' } bind def',
     & '/RestoreFont {',
     & '  CurrentFont findfont CurrentSize FontScale setfont',
     & ' } bind def',
     & '/XYZmove { pop moveto } bind def',
     & '/XYZrmove { pop rmoveto } bind def'
c
c This is one way to do it
c
c     WRITE(LB,607)
c    &      '%',
c    &      '% Add Angstrom sign to commonly used fonts',
c    &      '% using iso-8859-1 encoding (Å = 197,  305 octal)',
c    &      '%',
c    &      '/reencsmalldict 12 dict def',
c    &      '/ReEncodeSmall',
c    &      '  { reencsmalldict begin',
c    &      '    /newcodesandnames exch def ',
c    &      '    /newfontname exch def',
c    &      '    /basefontname exch def ',
c    &      '    /basefontdict basefontname findfont def',
c    &      '    /newfont basefontdict maxlength dict def',
c    &      '    basefontdict',
c    &      '      { exch dup /FID ne',
c    &      '	{ dup /Encoding eq',
c    &      '	  { exch dup length array copy newfont 3 1 roll put }',
c    &      '	  { exch newfont 3 1 roll put }',
c    &      '	  ifelse',
c    &      '        }',
c    &      '        { pop pop }',
c    &      '        ifelse',
c    &      '      } forall',
c    &      '    newfont /FontName newfontname put',
c    &      '    newcodesandnames aload pop',
c    &      '    newcodesandnames length 2 idiv',
c    &      '      { newfont /Encoding get 3 1 roll put }',
c    &      '      repeat',
c    &      '    newfontname newfont definefont pop',
c    &      '    end',
c    &      '  } def',
c    &      '/symbvec [',
c    &      '  8#305 /Aring',
c    &      '  ] def',
c    &      '/AddSymbs { dup symbvec ReEncodeSmall } def',
c    &      '/Times-Roman AddSymbs',
c    &      '/Times-Bold AddSymbs',
c    &      '/Times-Italic AddSymbs',
c    &      '/Times-BoldItalic AddSymbs',
c    &      '/Helvetica AddSymbs',
c    &      '/Helvetica-Bold AddSymbs',
c    &      '/Helvetica-Narrow AddSymbs',
c    &      '/Helvetica-Narrow-Bold AddSymbs',
c    &      '% End re-encoding'
c
c This is another way to do it
c
      WRITE(LB,607)
     &      '%',
     &      '% Switch common fonts to iso-8859-1 encoding',
     &      '%',
     &      '/Latin1 {',
     &      '  findfont dup length dict begin',
     &      '    {1 index /FID ne {def} {pop pop} ifelse} forall',
     &      '    /Encoding ISOLatin1Encoding def',
     &      '    currentdict',
     &      '  end',
     &      '} def',
     &      '/Times-Roman           dup Latin1 definefont pop',
     &      '/Times-Bold            dup Latin1 definefont pop',
     &      '/Times-Italic          dup Latin1 definefont pop',
     &      '/Times-BoldItalic      dup Latin1 definefont pop',
     &      '/Helvetica             dup Latin1 definefont pop',
     &      '/Helvetica-Bold        dup Latin1 definefont pop',
     &      '/Helvetica-Narrow      dup Latin1 definefont pop',
     &      '/Helvetica-Narrow-Bold dup Latin1 definefont pop',
     &      '/Helvetica-Oblique     dup Latin1 definefont pop',
     &      '/Helvetica-BoldOblique dup Latin1 definefont pop',
     &      '% End Re-encoding','%'
c
c
      WRITE(LB,600) '%%EndProlog'
      WRITE(LB,600) '%%BeginSetup'
      WRITE(LB,600) 'gsave'
      WRITE(LB,600) 'UnitWidth UnitHeight translate'
      WRITE(LB,604) RED,GRN,BLU,' setrgbcolor'
      WRITE(LB,600) 
     &	'UnitWidth -1 mul dup UnitHeight -1 mul newpath moveto'
      WRITE(LB,600)
     &	'UnitWidth UnitHeight -1 mul lineto UnitWidth UnitHeight lineto'
      WRITE(LB,600) 'UnitHeight lineto closepath fill'
      WRITE(LB,606) 'Times-Bold',10.,'RestoreFont'
      WRITE(LB,600) '/LabelStart gstate def'
      WRITE(LB,600) '%%Endsetup'
      WRITE(LB,600) '%%Page: 1 1'
     
      RETURN


      ENTRY LINP( INPUT, INTYPE, MATCOL, RGBMAT )
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
	WRITE (LB,606) FONTNAME(1:len), FONTSIZE, 'RestoreFont'

      ELSE IF (INTYPE .EQ. LABEL ) THEN
	READ (INPUT,*,END=50) XA, YA, ZA, RED, GRN, BLU
	IF (MATCOL) THEN
	    RED = RGBMAT(1)
	    GRN = RGBMAT(2)
	    BLU = RGBMAT(3)
	ENDIF
c
c	Here is where Perl would shine
c
	READ (INPUT,'(A)',END=50) LABELSTRING
	do j= MAXLABLEN,1,-1
	    len = j
	    if (LABELSTRING(len:len).ne.' ') goto 702
	enddo
702	continue
c
c       Isolated objects not transformed by TMAT, but still subject to inversion.
c       Then again, PostScript y-axis convention is upside-down from screen coords.
        IF (ISOLATION.GT.0) THEN
          IF (.not.INVERT) YA = -YA
	  if (isolation.eq.2) then
	    if (xcent.gt.ycent) xa = xa * xcent / ycent
	    if (xcent.lt.ycent) ya = ya * ycent / xcent
	  endif
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
	    WRITE (LB,802) RED,GRN,BLU,XA,YA,ZA
	ELSE
	    WRITE (LB,801) RED,GRN,BLU,XA,YA,ZA
	ENDIF
801	FORMAT(3f6.3,' setrgbcolor',3f10.4,' XYZmove')
802	FORMAT(3f6.3,' setrgbcolor',3f10.4,' XYZrmove')
c
c	At this point I should loop over string looking for
c	escape sequences, control characters, etc.
c
	WRITE (LB,600) 'LabelStart currentgstate pop'
	LEVEL  = 0
	IBEG = 1
  81	CONTINUE
  	I = IBEG
	IF (I.GT.LEN) RETURN
  82	CONTINUE
c	
c	27-Feb-2000
c	TeX-like escape sequence processing
c	Unfortunately this is not easily made compatible with anything other
c	that Left-Align.
c	Possibly these problems can be fixed by additional PostScript code?
c
	  if (labelstring(i:i) .eq. '\\') then
	    j = i
   83	    j = j + 1
   	    if (labelstring(j:j).ge.'A' .and. labelstring(j:j).le.'Z')
     &         goto 83
   	    if (labelstring(j:j).ge.'a' .and. labelstring(j:j).le.'z')
     &         goto 83
	    if (j.gt.i+2 .and. j.le.len+1) then
	      texstring = ltex( labelstring(i+1:j-1) )
	      if (texstring.eq.char(0)) goto 90 
	      if (ibeg.lt.i) 
     &           write(LB,804) labelstring(ibeg:i-1),'show'
     	      sssize = FONTSIZE
	      if (level.ne.0) sssize = sssize * 0.8
	      write(LB,605) 'Symbol',sssize
	      write(LB,804) texstring,'show RestoreFont'
	      if (level.ne.0) write(LB,600)'ShrinkFont'
	      if (labelstring(j:j).eq.' ') j = j + 1
	      ibeg = j
	      goto 81
	    endif
	  endif

	  if (labelstring(i:i) .eq. '_') then
	    if (ibeg.lt.i) 
     &         write(LB,804) labelstring(ibeg:i-1),'show'
	    write(LB,600) '0 FontHeight -0.3 mul rmoveto'
	    write(LB,600) 'ShrinkFont'
	    i = i + 1
	    if (labelstring(i:i) .eq. '{') then
	      level = -1
	      ibeg = i + 1
	      goto 81
	    else
	      if (labelstring(i:i).eq.'\\') labelstring(i:i)='^'
	      write(LB,804) labelstring(i:i),'show'
	      write(LB,600) 'RestoreFont'
	      write(LB,600) '0 FontHeight 0.3 mul rmoveto'
	      ibeg = i + 1
	      goto 81
	    endif
	  endif

	  if (labelstring(i:i) .eq. '^') then
	    if (ibeg.lt.i) 
     &         write(LB,804) labelstring(ibeg:i-1),'show'
	    write(LB,600) '0 FontHeight 0.3 mul rmoveto'
	    write(LB,600) 'ShrinkFont'
	    i = i + 1
	    if (labelstring(i:i) .eq. '{') then
	      level = 1
	      ibeg = i + 1
	      goto 81
	    else
	      if (labelstring(i:i).eq.'\\') labelstring(i:i)='^'
	      write(LB,804) labelstring(i:i),'show'
	      write(LB,600) 'RestoreFont'
	      write(LB,600) '0 FontHeight -0.3 mul rmoveto'
	      ibeg = i + 1
	      goto 81
	    endif
	  endif

	  if (labelstring(i:i) .eq. '}') then
	    if (ibeg.lt.i)
     &         write(LB,804) labelstring(ibeg:i-1),'show'
	    write(LB,600) 'RestoreFont'
	    write(LB,603) '0 FontHeight ',-0.3*level,'  mul rmoveto'
	    level = 0
	    ibeg = i + 1
	    goto 81
	  endif
c
c	End of TeX-like escape processing
c
   90	CONTINUE
	  IF  ( LABELSTRING(I:I)    .EQ.'\\'
     &    .AND. LABELSTRING(I+1:I+1).EQ.'n') THEN
	    IF (IBEG.LT.I) THEN
	      IF (IALIGN.EQ.1) THEN
	        WRITE (LB,803) LABELSTRING(IBEG:I-1),'Center'
	      ELSE IF (IALIGN.EQ.2) THEN
	        WRITE (LB,803) LABELSTRING(IBEG:I-1),'Right'
	      ELSE
	        WRITE (LB,803) LABELSTRING(IBEG:I-1),' '
	      ENDIF
	    ENDIF
	    WRITE(LB,600) 'LabelStart setgstate',
     &                    '0 FontHeight -1 mul rmoveto',
     &	                  'LabelStart currentgstate pop'
	    IBEG = I+2
	    GOTO 81
	  ENDIF
	  IF  ( LABELSTRING(I:I)    .EQ.'\\'
     &    .AND. LABELSTRING(I+1:I+1).EQ.'v') THEN
	    IF (IBEG.LT.I) THEN
	      IF (IALIGN.EQ.1) THEN
	        WRITE (LB,803) LABELSTRING(IBEG:I-1),'Center'
	      ELSE IF (IALIGN.EQ.2) THEN
	        WRITE (LB,803) LABELSTRING(IBEG:I-1),'Right'
	      ELSE
	        WRITE (LB,803) LABELSTRING(IBEG:I-1),' '
	      ENDIF
	    ENDIF
	    WRITE(LB,600) '0 FontHeight 0.5 mul rmoveto'
	    IBEG = I+2
	    GOTO 81
	  ENDIF
	  IF  ( LABELSTRING(I:I)    .EQ.'\\'
     &    .AND. LABELSTRING(I+1:I+1).EQ.'b') THEN
	    IF (IBEG.LT.I) THEN
	      IF (IALIGN.EQ.1) THEN
	        WRITE (LB,803) LABELSTRING(IBEG:I-1),'Center'
	      ELSE IF (IALIGN.EQ.2) THEN
	        WRITE (LB,803) LABELSTRING(IBEG:I-1),'Right'
	      ELSE
	        WRITE (LB,803) LABELSTRING(IBEG:I-1),' '
	      ENDIF
	    ENDIF
	    WRITE(LB,600) 'FontWidth -0.5 mul 0 rmoveto'
	    IBEG = I+2
	    GOTO 81
	  ENDIF
	  IF  ( LABELSTRING(I:I)    .EQ.'\\'
     &    .AND. LABELSTRING(I+1:I+1).EQ.'A') THEN
     	    LABELSTRING(I+1:I+1) = CHAR(197)
     	  ENDIF
	I = I + 1
	IF (I.LE.LEN) GOTO 82
c
c	End proposed escape interpretation loop
c
	IF (IALIGN.EQ.1) THEN
	    WRITE (LB,803) LABELSTRING(IBEG:LEN),'Center'
	ELSE IF (IALIGN.EQ.2) THEN
	    WRITE (LB,803) LABELSTRING(IBEG:LEN),'Right'
	ELSE
	    WRITE (LB,803) LABELSTRING(IBEG:LEN),' '
	ENDIF
803	FORMAT('(',A,') ',A6,'  show')
804	FORMAT('(',A,') ',A)
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
      WRITE (LB,600) '%'
      WRITE (LB,600) 'showpage'
      WRITE (LB,600) '%%Trailer'
      WRITE (LB,600) '%%DocumentFonts: Times-Bold'
      WRITE (LB,600) '%%EOF'

      IF (KEEP.GT.0) THEN
          CLOSE (UNIT=LB)
      ELSE
          CLOSE (UNIT=LB,STATUS='DELETE')
      ENDIF
*
      end

C
C     Map TeX escape sequences to the corresponding character in the
C     standard PostScript SYmbol font.
C     Most greek letters map to their own first letter, so we don't
C     need to explicitly search for them.
C     We explicitly map \nu to distinguish it from \n = newline,
C     and \beta to distinguish it from \b = backspace. 
C
      function ltex( symbolstring )
      character*1 ltex
      character*(*) symbolstring
      ltex = symbolstring(1:1)
c
      if (ltex.eq.'b') ltex = char(0)
      if (ltex.eq.'n') ltex = char(0)
      if (ltex.eq.'v') ltex = char(0)
c
      if (symbolstring.eq.'beta') then 
          ltex = 'b'
      else if (symbolstring.eq.'eta') then
          ltex = 'h'
      else if (symbolstring.eq.'nu') then
          ltex = 'n'
      else if (symbolstring.eq.'theta') then
          ltex = 'q'
      else if (symbolstring.eq.'phi') then
          ltex = 'j'
      else if (symbolstring.eq.'psi') then
          ltex = 'y'
      else if (symbolstring.eq.'omega') then
          ltex = 'w'
      else if (symbolstring.eq.'Eta') then 
          ltex = 'H'
      else if (symbolstring.eq.'Theta') then
          ltex = 'Q'
      else if (symbolstring.eq.'Phi') then
          ltex = 'F'
      else if (symbolstring.eq.'Psi') then
          ltex = 'Y'
      else if (symbolstring.eq.'Omega') then
          ltex = 'W'
      else if (symbolstring.eq.'infty') then
          ltex = char(165)
C         ltex = '¥'
      else if (symbolstring.eq.'nabla') then
          ltex = char(165)
C         ltex = 'Ñ'
      else if (symbolstring.eq.'ellipses') then
          ltex = char(188)
C         ltex = '¼'
      else if (symbolstring.eq.'partial') then
          ltex = char(182)
C         ltex = '¶'
      else if (symbolstring.eq.'degree') then
          ltex = char(176)
C         ltex = '°'
      else if (symbolstring.eq.'func') then
          ltex = char(166)
C         ltex = '¦'
      else if (symbolstring.eq.'sqrt') then
          ltex = char(214)
C         ltex = 'Ö'
      else if (symbolstring.eq.'aleph') then
          ltex = char(192)
C         ltex = 'À'
      endif
      return
      end
