      PROGRAM LABELS3D
*
*     Version 2.3b (beta test version)
*
* EAM Dec 1996	- Initial version
*
*	Usage: labels3d [-fontscale xxx] < input.r3d > labels.ps
*
*	This program is part of the Raster3D package.
*	It is simply a stripped down version of the input section of 
*	render.  It reads in a render input file, applies
*	the transformation matrix specified in the header, and 
*	looks though the file for label records (types 10,11,12).
*	It writes out a PostScript file which places these labels
*	appropriately in the raster described by the header records.
*	After running labels3d, the PostScript file can be run through
*	ghostscript to produce an actual raster image, and then
*	composited onto the Raster3D image.
*
*	Since what we are doing is largely a text formatting task,
*	I suspect this whole affair would better be done in a Perl script.
*
*     Input (line by line except where noted):
*
*     - TITLE    anything you like
*     - NTX,NTY  tiles in each direction
*     - NPX,NPY  pixels per tile to compute in each direction
*     - SCHEME   pixel averaging scheme (0, 1, 2, 3 or 4)
*       - 0 no anti-aliasing, maintain alpha channel
*       - 1 no anti-aliasing, no alpha channel
*       - 2 means 2x2 computing pixels for 1 output pixel
*       - 3 means 3x3 computing pixels for 2x2 output pixels
*       - 4 like 3, but NTX..NPY specify final rather than initial raster
*     - BKGND    background colour (r,g,b in range 0 to 1)
*     - SHADOW   "shadow mode?" (T or F)
*     - IPHONG   Phong power (e.g., 20)
*     - STRAIT   straight-on (2ndary) light component (e.g., 0.1)
*     - AMBIEN   ambient light component (e.g., 0.05)
*     - SPECLR   specular reflection component (e.g., 0.30)
*     - EYEPOS   eye position (e.g., 4)
*       - relative to 1=narrow dimension of screen
*       - used for perspective
*       - don't put it in the transformation matrix yourself
*     - SOURCE   main light source position (x,y,z components)
*       - vector length ignored, point source is at infinity
*     - TMAT     global transformation on input objects
*       - postfix 4x4 matrix on 4 lines, as you would write it
*       - upper left 3x3 must be pure rotation
*       - lower left 1x3 is translation
*       - lower right 1x1 is global scaling (reduction)
*       - upper right 3x1 causes extra perspective (should be 0)
*       - applies to homogeneous co-ordinates (x,y,z,1)
*       - radii are only scaled down by global scaling TMAT(4,4)
*     - INMODE   object input mode (1, 2, or 3)
*       - mode 1:  all objects are triangles
*       - mode 2:  all objects are spheres
*       - mode 3:  each object will be preceded by type
*    	only mode 3 makes sense to this program
*     - INFMT or INFMTS   object input format(s), 1 per line
*       - one format for modes 1 and 2, or three for mode 3
*       - each is fortran format in parentheses, or single *
*       - for 3 formats, the order of formats and details is:
*         - triangle:  x1,y1,z1,x2,y2,z2,x3,y3,z3,r,g,b
*         - sphere:    x,y,z,radius,r,g,b
*         - trcone:    x1,y1,z1,rad1,x2,y2,z2,rad2,r,g,b
*	  - cylinder:  as truncated cone, but 2nd radius ignored
*     - objects
*	object types 1-9 are ignored
*         - type 1:  triangle (to be read with 1st format)
*         - type 2:  sphere (to be read with 2nd format)
*         - type 3:  cylinder with rounded ends (3rd format) EAM
*         - type 4:  trcone made of spheres (3rd format) not implemented
*         - type 5:  cylinder with flat ends (3rd format) EAM
*         - type 6:  plane (=triangle with infinite extent) (1st format) EAM
*         - type 7:  normal vectors for previous triangle (1st format) EAM
*         - type 8:  material definition which applies to subsequent objects EAM
*         - type 9:  end previous material EAM
*         - type 0:  no more objects (equivalent to eof)
*	label object types are
*	  - type 10: Font_Name size alignment
*	  - type 11: XYZ RGB on first line
*		     label (ascii characters enclosed in quotes) on second line
*	  - type 12: (reserved for more label information)
*	other types (ignored)
*	  - type 13: glow light
*
*-----------------------------------------------------------------------------
*
*     Overkill:
      IMPLICIT REAL*4 (A-H, O-Z)
*
*     I/O units for control input, file indirection, data output, info output
      INTEGER STDIN, INPUT0, OUTPUT, NOISE
      PARAMETER (STDIN=5, INPUT0=7, OUTPUT=6, NOISE=0)
*     Allowable levels of file indirection.
      PARAMETER (MAXLEV=3)
      INTEGER    ILEVEL
*
*     Codes for triangle, sphere, truncated cone, and string of pearls
      INTEGER TRIANG, SPHERE, TRCONE, PEARLS, CYLIND, CYLFLAT
      INTEGER PLANE, MXTYPE
      PARAMETER (TRIANG = 1, SPHERE = 2, TRCONE = 3, PEARLS = 4)
      PARAMETER (CYLIND = 3, CYLFLAT= 5)
      PARAMETER (PLANE    = 6)
      PARAMETER (NORMS    = 7)
      PARAMETER (MATERIAL = 8)
      PARAMETER (MATEND   = 9)
      PARAMETER (FONT     = 10)
      PARAMETER (LABEL    = 11)
      PARAMETER (GLOWLIGHT= 13)
      PARAMETER (MXTYPE   = 13)
*
*     $$$$$$$$$$$$$$$$$  END OF LIMITS  $$$$$$$$$$$$$$$$$$$$$$$
*
*
*     Title for run
      CHARACTER*80 TITLE
*
*     Number of tiles
      INTEGER NTX, NTY
*
*     Pixel averaging scheme
      INTEGER SCHEME
*
*     Background colour
      REAL BKGND(3)
*
*     "Shadow mode?"
      LOGICAL SHADOW
*
*     Phong power
      INTEGER IPHONG
*
*     Straight-on (secondary) light source contribution
      REAL STRAIT
*
*     Ambient light contribution
      REAL AMBIEN
*
*     Specular reflection component
      REAL SPECLR
*
*     Distance (in +z) of viewing eye
      REAL*4 EYEPOS
*
*     Primary light source position
      REAL*4 SOURCE(3)
*
*     Input transformation
      REAL*4 TMAT(4,4)
*
*     Input mode
      INTEGER INMODE
*
*     Buffer one line of input for decoding
      CHARACTER*80 LINE
*
*     Input format(s)
      CHARACTER*80 INFMTS(MXTYPE),INFMT
*
*     Free-format input flag
      LOGICAL INFLGS(MXTYPE),INFLG
*
*     Shortest rotation from light source to +z axis
      REAL*4 SROT(3,3)
*
*     Input buffer for details
      REAL*4 BUF(100)
*
*     Intermediate storage for output header
      INTEGER*2 NX,NY
*
*     Command line args
      CHARACTER*80 FLAGS
*
*     Stuff for labels
      CHARACTER*80 FONTNAME, LABELSTRING, FONTALIGN
      REAL*4       FONTSIZE, FONTSCALE
*
*     Copy of NOISE for ASSERT to see
      INTEGER ASSOUT
      COMMON /ASSCOM/ ASSOUT
      SAVE /ASSCOM/
*
*     The number of "details" each object type is supposed to have
      INTEGER IDET(MXTYPE)
*
      IDET(TRIANG) = 12
      IDET(SPHERE) = 7
      IDET(PEARLS) = 11
      IDET(TRCONE) = 11
      IDET(CYLIND) = 11
      IDET(PLANE)  = 12
      IDET(NORMS ) = 9
      IDET(MATERIAL) = 10
      IDET(LABEL)  = 6
      IDET(GLOWLIGHT) = 10
*
*     Copy the info (also error reporting) unit number to common
      ASSOUT = NOISE
      WRITE (NOISE,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%',
     &                '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      WRITE (NOISE,*) '%           Raster3D labeling program ',
     &                'V2.3b (beta test version)     %'
      WRITE (NOISE,*) '%              -----------------------',
     &                '-----------------             %'
      WRITE (NOISE,*) '% Suggested commands to combine output',
     &                ' with rendered image:         %'
      WRITE (NOISE,*) '%   alias gs3d gs -sDEVICE=ppm -dNOPAUSE',
     &                ' -q -sOutputFile=labels.ppm %'
      WRITE (NOISE,*) '%   gs3d -gNXxNY thisfile.ps -c quit  ',
     &                '                              %'
      WRITE (NOISE,*) '%   combine -compose over render.tiff ',
     &                'labels.ppm final.tiff         %'
      WRITE (NOISE,*) '%              -----------------------',
     &                '-----------------             %'
      WRITE (NOISE,*) '% comments & suggestions to:     ',
     &                '  merritt@u.washington.edu','         %'
      WRITE (NOISE,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%',
     &                '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
c
c
      FONTSCALE = 1.0
      NARG  = IARGC()
      DO i = 1, NARG
        CALL GETARG(I,FLAGS)
        IF (FLAGS(1:4) .EQ. '-fon') then
	    CALL GETARG(I+1,FLAGS)
	    READ (FLAGS,*) FONTSCALE
	ENDIF
      ENDDO
*
*     Ready for input records
      ILEVEL = 0
      INPUT  = STDIN
*
*     Get title
1     FORMAT (A80)
      READ (INPUT,1) TITLE
*
*     Get number of tiles
      READ (INPUT,1) LINE
      READ (LINE,*) NTX,NTY
      CALL ASSERT (NTX.GT.0, 'ntx.le.0')
      CALL ASSERT (NTY.GT.0, 'nty.le.0')
*
*     Get number of pixels per tile
      READ (INPUT,1) LINE
      READ (LINE,*) NPX,NPY
      CALL ASSERT (NPX.GT.0, 'npx.le.0')
      CALL ASSERT (NPY.GT.0, 'npy.le.0')
*
*     Get pixel averaging scheme
      READ (INPUT,1) LINE
      READ (LINE,*) SCHEME
      CALL ASSERT (SCHEME.GE.0 .AND. SCHEME.LE.4, 'bad scheme')
      IF (SCHEME.LE.1) THEN
        NOX = NPX
        NOY = NPY
      ELSEIF (SCHEME.EQ.2) THEN
        NOX = NPX/2
        NOY = NPY/2
        CALL ASSERT (MOD(NPX,2).EQ.0,'scheme 2 requires NPX even')
        CALL ASSERT (MOD(NPY,2).EQ.0,'scheme 2 requires NPY even')
      ELSEIF (SCHEME.EQ.3) THEN
        NOX = 2*(NPX/3)
        NOY = 2*(NPY/3)
        CALL ASSERT (MOD(NPX,3).EQ.0,'scheme 3 requires mod(NPX,3)=0')
        CALL ASSERT (MOD(NPY,3).EQ.0,'scheme 3 requires mod(NPY,3)=0')
      ELSEIF (SCHEME.EQ.4) THEN
	NOX = NPX
	NOY = NPY
	NPX = NPX + NPX/2
	NPY = NPY + NPY/2
	CALL ASSERT (MOD(NPX,2).EQ.0,'scheme 4 requires NPX even')
	CALL ASSERT (MOD(NPY,2).EQ.0,'scheme 4 requires NPY even')
	SCHEME = 3
      ELSE
        CALL ASSERT (.FALSE.,'crash 2')
      ENDIF
*
      NX = NOX*NTX
      NY = NOY*NTY
*
*     Some derived parameters
*     NB:  unlike in render, this has to be OUTPUT raster size (NOX,NOY)
      XCENT = NTX*NOX/2.
      YCENT = NTY*NOY/2.
      SCALE = 2.*MIN(XCENT,YCENT)
*
*     Get background colour
      READ (INPUT,1) LINE
      READ (LINE,*) BKGND
      CALL ASSERT (BKGND(1).GE.0., 'bkgnd(1) < 0')
      CALL ASSERT (BKGND(2).GE.0., 'bkgnd(2) < 0')
      CALL ASSERT (BKGND(3).GE.0., 'bkgnd(3) < 0')
      CALL ASSERT (BKGND(1).LE.1., 'bkgnd(1) > 1')
      CALL ASSERT (BKGND(2).LE.1., 'bkgnd(2) > 1')
      CALL ASSERT (BKGND(3).LE.1., 'bkgnd(3) > 1')
*
*     Get "shadows" flag
      READ (INPUT,1) LINE
      READ (LINE,*) SHADOW
*
*     Get Phong power
      READ (INPUT,1) LINE
      READ (LINE,*) IPHONG
      CALL ASSERT (IPHONG.GE.0, 'iphong < 0')
*
*     Get contribution of straight-on (secondary) light source
      READ (INPUT,1) LINE
      READ (LINE,*) STRAIT
      CALL ASSERT (STRAIT.GE.0., 'strait < 0')
      CALL ASSERT (STRAIT.LE.1., 'strait > 1')
*
*     Derive contribution of primary light source
      PRIMAR = 1. - STRAIT
*
*     Get contribution of ambient light
      READ (INPUT,1) LINE
      READ (LINE,*) AMBIEN
      CALL ASSERT (AMBIEN.GE.0., 'ambien < 0')
      CALL ASSERT (AMBIEN.LE.1., 'ambien > 1')
*
*     Get component of specular reflection
      READ (INPUT,1) LINE
      READ (LINE,*) SPECLR
      CALL ASSERT (SPECLR.GE.0., 'speclr < 0')
      CALL ASSERT (SPECLR.LE.1., 'speclr > 1')
*
*     Get distance of viewing eye
      READ (INPUT,1) LINE
      READ (LINE,*) EYEPOS
      CALL ASSERT (EYEPOS.GT.0., 'eyepos.le.0')
*
*     Get position of primary light source
      READ (INPUT,1) LINE
      READ (LINE,*) SOURCE
      SMAG = SQRT(SOURCE(1)**2 + SOURCE(2)**2 + SOURCE(3)**2)
      SOURCE(1) = SOURCE(1) / SMAG
      SOURCE(2) = SOURCE(2) / SMAG
      SOURCE(3) = SOURCE(3) / SMAG
*
*     Get input transformation
      WRITE (NOISE,*) 'tmat (v'' = v * tmat):'
      DO 11 I=1,4
        READ (INPUT,*) (TMAT(I,J),J=1,4)
        WRITE (NOISE,*) (TMAT(I,J),J=1,4)
11    CONTINUE
*
*     Get input mode
      READ (INPUT,1) LINE
      READ (LINE,*) INMODE
      CALL ASSERT (INMODE.GE.3.,'bad inmode')
      IF (INMODE.GT.3) WRITE (NOISE,*) 'Non-standard INMODE',INMODE
*
*     Get input format(s)
      IF (INMODE.EQ.1.OR.INMODE.EQ.2) THEN
        READ (INPUT,'(A)') INFMT
        II = 0
2       CONTINUE
        IF (INFMT(1:1).EQ.' ') THEN
          INFMT(1:79) = INFMT(2:80)
          INFMT(80:80) = ' '
          II = II + 1
          IF (II.LT.80) GO TO 2
        ENDIF
        IF (INFMT(1:1).EQ.'*') THEN
          INFLG = .TRUE.
        ELSE
          INFLG = .FALSE.
        ENDIF
      ELSEIF (INMODE.GE.3) THEN
        DO 4 I=1,3
          READ (INPUT,'(A)') INFMTS(I)
          II = 0
3         CONTINUE
          IF (INFMTS(I)(1:1).EQ.' ') THEN
            INFMTS(I)(1:79) = INFMTS(I)(2:80)
            INFMTS(I)(80:80) = ' '
            II = II + 1
            IF (II.LT.80) GO TO 3
          ENDIF
          IF (INFMTS(I)(1:1).EQ.'*') THEN
            INFLGS(I) = .TRUE.
          ELSE
            INFLGS(I) = .FALSE.
          ENDIF
4       CONTINUE
	INFLGS(PLANE) = INFLGS(TRIANG)
	INFMTS(PLANE) = INFMTS(TRIANG)
	INFLGS(NORMS) = INFLGS(TRIANG)
	INFMTS(NORMS) = INFMTS(TRIANG)
	INFLGS(MATERIAL) = .TRUE.
      ELSE
        CALL ASSERT (.FALSE.,'crash 4')
      ENDIF
*
*     Compute the rotation matrix which takes the light
*     source to the +z axis (i.e., to the viewpoint).
*     first make p = source cross z (and normalize p)
      P1 = SOURCE(2)
      P2 = -SOURCE(1)
*     p3 = 0
      PLEN = SQRT(P1**2 + P2**2)
      IF (PLEN .GT. 0.0) P1 = P1 / PLEN
      IF (PLEN .GT. 0.0) P2 = P2 / PLEN
*     phi is the angle between source and z (shortest route)
      COSPHI = SOURCE(3)
      SINPHI = PLEN
      SROT(1,1) = P1**2 + (1.-P1**2)*COSPHI
      SROT(1,2) = P1*P2*(1.-COSPHI)
      SROT(1,3) = P2*SINPHI
      SROT(2,1) = SROT(1,2)
      SROT(2,2) = P2**2 + (1.-P2**2)*COSPHI
      SROT(2,3) = -P1*SINPHI
      SROT(3,1) = -SROT(1,3)
      SROT(3,2) = -SROT(2,3)
      SROT(3,3) = COSPHI
*
c
c     Write out PostScript prolog records
c	The only computed values here are NX, NY, and FONTSCALE
c	Background is used as given, however
c
601   FORMAT(A,I6,A)
602   FORMAT(A,2I6,A)
603   FORMAT(A,F6.3,A)
604   FORMAT(3F6.3,A)
      WRITE(OUTPUT,601) '%!PS-Adobe-3.0'
      WRITE(OUTPUT,601) '%%Creator: Labels3d'
      WRITE(OUTPUT,601) '%%Pages: 1'
      WRITE(OUTPUT,602) '%%BoundingBox: 0 0',nx,ny
      WRITE(OUTPUT,601) '%%BeginProlog'
      WRITE(OUTPUT,601) '% These are the only control parameters'
      WRITE(OUTPUT,603) '/FontSize ',FONTSCALE,' def'
      WRITE(OUTPUT,601) '/UnitHeight ',ny/2,' def'
      WRITE(OUTPUT,601) '/UnitWidth  ',nx/2,' def'
      WRITE(OUTPUT,601) '% '
      WRITE(OUTPUT,601) '/FontScale { FontSize mul scalefont } bind def'
      WRITE(OUTPUT,601) '/Center {'
      WRITE(OUTPUT,601) ' dup stringwidth'
      WRITE(OUTPUT,601) ' exch -2 div exch -2 div rmoveto'
      WRITE(OUTPUT,601) ' } bind def'
      WRITE(OUTPUT,601) '/Right {'
      WRITE(OUTPUT,601) ' dup stringwidth'
      WRITE(OUTPUT,601) ' exch -1 mul exch -1 mul rmoveto'
      WRITE(OUTPUT,601) ' } bind def'
      WRITE(OUTPUT,601) '/XYZmove {'
      WRITE(OUTPUT,601) '  pop moveto'
      WRITE(OUTPUT,601) ' } bind def'
      WRITE(OUTPUT,601) '/XYZrmove {'
      WRITE(OUTPUT,601) '  pop rmoveto'
      WRITE(OUTPUT,601) ' } bind def'
      WRITE(OUTPUT,601) '%%EndProlog'
      WRITE(OUTPUT,601) '%%BeginSetup'
      WRITE(OUTPUT,601) 'gsave'
      WRITE(OUTPUT,601) 'UnitWidth UnitHeight translate'
      WRITE(OUTPUT,604) BKGND(1),BKGND(2),BKGND(3),' setrgbcolor'
      WRITE(OUTPUT,601) 
     &	'UnitWidth -1 mul dup UnitHeight -1 mul newpath moveto'
      WRITE(OUTPUT,601)
     &	'UnitWidth UnitHeight -1 mul lineto UnitWidth UnitHeight lineto'
      WRITE(OUTPUT,601) 'UnitHeight lineto closepath fill'
      WRITE(OUTPUT,701) 'Times-Bold',12.
      WRITE(OUTPUT,601) '%%Endsetup'
      
      nlabels = 0
c
c     Objects in, and count up objects that may impinge on each tile
      N = 0
c
c     Read in next object
7     CONTINUE
c     Aug 1996 - allow file indirection
      READ (INPUT,'(A)',END=50) LINE
      IF (LINE(1:1) .NE. '@') THEN
 	READ (LINE,*) INTYPE
      ELSE
 	CALL ASSERT(ILEVEL.LT.MAXLEV, 
     &	            'Too many levels of indirection')
 	DO I=80,2,-1
	  IF (LINE(I:I).NE.' ') J = I
 	  IF (LINE(I:I).EQ.' ') LINE(I:I) = CHAR(0)
 	ENDDO
 	OPEN (UNIT=INPUT0+ILEVEL,ERR=71,STATUS='OLD',READONLY,
     &	      FILE=LINE(J:80))
 	WRITE (NOISE,'(A,A)') '  + Opening input file ',LINE(2:80)
	INPUT  = INPUT0+ILEVEL
 	ILEVEL = ILEVEL + 1
 	GOTO 7
   71	WRITE (NOISE,'(A,A)') ' >> Cannot open file ',LINE(2:80)
 	GOTO 7
      ENDIF
c
      IF (INTYPE.EQ.0) GO TO 50
      CALL ASSERT (INTYPE.GE.1.AND.INTYPE.LE.MXTYPE,'bad object')
c
c     Deal here with newer object types that may not be numeric data
c
      IF (INTYPE .EQ. MATEND) THEN
	GOTO 7
      ELSE IF (INTYPE .EQ. FONT) THEN
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
	WRITE (OUTPUT,701) FONTNAME(1:len), FONTSIZE
701	FORMAT('/',A,' findfont',F6.2,' FontScale setfont')
	GOTO 7
      ELSE IF (INTYPE .EQ. LABEL ) THEN
	READ (INPUT,*,END=50) (BUF(I),I=1,IDET(LABEL))
c
c	Here is where Perl would shine
c
	READ (INPUT,702,END=50) len,LABELSTRING
702	FORMAT(Q,A)
	nlabels = nlabels + 1
c
        XA = BUF(1)
        YA = BUF(2)
        ZA = BUF(3)
        CALL TRANSF (XA,YA,ZA, TMAT)
	PFAC = 1./(1.-ZA/EYEPOS)
	XA = XA * PFAC * SCALE
	YA = YA * PFAC * SCALE
	ZA = ZA * PFAC * SCALE
c
        RED = BUF(4)
        GRN = BUF(5)
        BLU = BUF(6)
        CALL ASSERT (RED.GE.0., 'red < 0 in label')
        CALL ASSERT (GRN.GE.0., 'grn < 0 in label')
        CALL ASSERT (BLU.GE.0., 'blu < 0 in label')
        CALL ASSERT (RED.LE.1., 'red > 1 in label')
        CALL ASSERT (GRN.LE.1., 'grn > 1 in label')
        CALL ASSERT (BLU.LE.1., 'blu > 1 in label')
c
	IF (IALIGN.EQ.3) THEN
	    WRITE (OUTPUT,802) RED,GRN,BLU,XA,YA,ZA
	ELSE
	    WRITE (OUTPUT,801) RED,GRN,BLU,XA,YA,ZA
	ENDIF
801	FORMAT(3f6.3,' setrgbcolor',3f10.4,' XYZmove')
802	FORMAT(3f6.3,' setrgbcolor',3f10.4,' XYZrmove')
	IF (IALIGN.EQ.1) THEN
	    WRITE (OUTPUT,803) LABELSTRING(1:len),'Center'
	ELSE IF (IALIGN.EQ.2) THEN
	    WRITE (OUTPUT,803) LABELSTRING(1:len),'Right'
	ELSE
	    WRITE (OUTPUT,803) LABELSTRING(1:len),' '
	ENDIF
803	FORMAT('(',A,') ',A6,'  show')
	GOTO 7
      ENDIF
c
c All other objects we can just skip
c
      IF (INTYPE .EQ. MATERIAL) THEN
        READ (INPUT,*,END=50) (BUF(I),I=1,IDET(INTYPE))
	IF (BUF(10).GT.0) THEN
	  DO I = 1,BUF(10)
	  READ (INPUT,'(A)',END=50) LINE
	  ENDDO
	ENDIF
      ELSEIF (INTYPE .EQ. GLOWLIGHT) THEN
        READ (INPUT,*,END=50) (BUF(I),I=1,IDET(INTYPE))
      ELSE
      	INFMT = INFMTS(INTYPE)
      	INFLG = INFLGS(INTYPE)
      	IF (INFLG) THEN
          READ (INPUT,*,END=50) (BUF(I),I=1,IDET(INTYPE))
      	ELSE
          READ (INPUT,INFMT,END=50) (BUF(I),I=1,IDET(INTYPE))
	ENDIF
      ENDIF
C
9     CONTINUE
      N = N + 1
      GOTO 7
*
*     here for end of objects
*
50    CONTINUE
      IF (ILEVEL.GT.0) THEN
	ILEVEL = ILEVEL - 1
	WRITE (NOISE,*) ' - closing indirect input file'
	CLOSE(INPUT)
	IF (ILEVEL.EQ.0) THEN
	  INPUT = STDIN
	ELSE
	  INPUT = INPUT - 1
	ENDIF
	GOTO 7
      ENDIF
*
c
c     Finish off PostScript output
      WRITE (OUTPUT,601) '%'
      WRITE (OUTPUT,601) 'showpage'
*
      WRITE(NOISE,*)'-------------------------------'
      WRITE (NOISE,501) NX,NY
501   FORMAT(' Raster size:    ',I5,' x',I5)
      WRITE(NOISE,*) NLABELS,' labels processed'
      WRITE(NOISE,*)'-------------------------------'
*
*
      end

      SUBROUTINE ASSERT (LOGIC, DAMMIT)
      LOGICAL LOGIC
      CHARACTER*(*) DAMMIT
      INTEGER ASSOUT
      COMMON /ASSCOM/ ASSOUT
      SAVE /ASSCOM/
      IF (LOGIC) RETURN
      WRITE (ASSOUT,*) '*** ',DAMMIT
      STOP 1234
      END

      SUBROUTINE TRANSF (X,Y,Z, T)
      REAL*4 X,Y,Z,T(4,4)
      REAL*4 H(4)
      H(1) = X*T(1,1) + Y*T(2,1) + Z*T(3,1) + T(4,1)
      H(2) = X*T(1,2) + Y*T(2,2) + Z*T(3,2) + T(4,2)
      H(3) = X*T(1,3) + Y*T(2,3) + Z*T(3,3) + T(4,3)
      H(4) = X*T(1,4) + Y*T(2,4) + Z*T(3,4) + T(4,4)
      CALL ASSERT (H(4).NE.0.,'infinite vector')
      X = H(1) / H(4)
      Y = H(2) / H(4)
      Z = H(3) / H(4)
      RETURN
      END

