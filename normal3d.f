      PROGRAM NORMAL3D
*
*     Version 2.4(alpha) (29 Nov 1997)
*
* EAM Jan 1996	- Initial release as part of version 2.2
* EAM Aug 1996	- Add -expand option to deal with file indirection
* EAM Feb 1997	- Handle additional MATERIAL records
*		  -stereo flag to generate left.r3d and right.r3d
* EAM Apr 1997	- now handles labels and glowlights
* EAM May 1997	- (well, now it does) V2.3c
* EAM Jul 1997	- quadric surfaces, ISOLATE
* EAM Oct 1997	- fix bug that prevented handling CYLFLAT objects
* EAM Nov 1997	- handle VERTEXRGB records, allow # as comment
*
*	This program is part of the Raster3D package.
*	It is simply a stripped down version of the input section of 
*	render.  It reads in a render input file, applies
*	the transformation matrix specified in the header, and writes
*	out a modified render input file describing the same image
*	using a unitary transformation matrix and normalized object
*	coordinates. All header records are basically passed through
*	without interpretation except for the transformation matrix
*	(which changes to the identity matrix) and the format lines
*	(which are set to *).
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
*       - used for perspective (0.0 = no perspective)
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
*     - INFMT or INFMTS   object input format(s), 1 per line
*       - one format for modes 1 and 2, or three for mode 3
*       - each is fortran format in parentheses, or single *
*       - for 3 formats, the order of formats and details is:
*         - triangle:  x1,y1,z1,x2,y2,z2,x3,y3,z3,r,g,b
*         - sphere:    x,y,z,radius,r,g,b
*         - trcone:    x1,y1,z1,rad1,x2,y2,z2,rad2,r,g,b
*	  - cylinder:  as truncated cone, but 2nd radius ignored
*     - objects
*       - modes 1,2:  each object starts on a new line
*         - read according to the single format given
*       - mode 3:  each object is preceded by a line giving type
*         - type 1:  triangle 
*         - type 2:  sphere 
*         - type 3:  cylinder with rounded ends 
*         - type 4:  trcone made of spheres (not implemented)
*         - type 5:  cylinder with flat ends
*         - type 6:  plane (=triangle with infinite extent)
*         - type 7:  normal vectors for previous triangle
*         - type 8:  material definition which applies to subsequent objects
*         - type 9:  end previous material
*	  - type 10: font specifier, we need to note coordinate convention
*	  - type 11: label, we may have to modify coordinates
*	  - type 12: reserved for more labeling options
*	  - type 13: glowlight
*	  - type 14: quadric surface
*	  - type 15: don't apply TMAT to subsequent objects
*	  - type 17: vertex colors for previous triangle
*         - type 0:  no more objects (equivalent to eof)
*
*-----------------------------------------------------------------------------
*
*     Overkill:
      IMPLICIT REAL   (A-H, O-Z)
*
*     I/O units for control input, file indirection, data output, info output
      INTEGER STDIN, INPUT0, OUTPUT, NOISE
      PARAMETER (STDIN=5, INPUT0=7, OUTPUT=6, NOISE=0)
      INTEGER LEFT, RIGHT
      PARAMETER (LEFT=1, RIGHT=2)
*     Allowable levels of file indirection.
      PARAMETER (MAXLEV=5)
      INTEGER    ILEVEL
*
      REAL  	HUGE
      PARAMETER (HUGE = 1.0e37)
*
*     Codes for triangle, sphere, truncated cone, and string of pearls
      INTEGER TRIANG, SPHERE, TRCONE, PEARLS, CYLIND, CYLFLAT
      INTEGER PLANE, QUADRIC, MXTYPE
      PARAMETER (TRIANG = 1, SPHERE = 2, TRCONE = 3, PEARLS = 4)
      PARAMETER (CYLIND = 3, CYLFLAT= 5)
      PARAMETER (PLANE    = 6)
      PARAMETER (NORMS    = 7)
      PARAMETER (MATERIAL = 8)
      PARAMETER (MATEND   = 9)
      PARAMETER (FONT     = 10, LABEL = 11 )
      PARAMETER (GLOWLIGHT= 13)
      PARAMETER (QUADRIC  = 14)
      PARAMETER (NOTRANS  = 15)
      PARAMETER (MXTYPE   = 17)
      PARAMETER (VERTEXRGB= 17)
*
*     $$$$$$$$$$$$$$$$$  END OF LIMITS  $$$$$$$$$$$$$$$$$$$$$$$
*
*
*     Title for run
      CHARACTER*80 TITLE
*
*     Number of tiles
      COMMON /RASTER/ NTX, NTY, NPX, NPY
      INTEGER         NTX, NTY, NPX, NPY
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
*     Primary light source position
      REAL   SOURCE(3)
*
*     Input transformation
      COMMON /MATRICES/ XCENT, YCENT, SCALE, EYEPOS, SXCENT, SYCENT,
     &                  TMAT, TINV, TINVT, SROT, SRTINV, SRTINVT
      REAL   XCENT, YCENT, SCALE, SXCENT, SYCENT
*     Transformation matrix, inverse, and transponsed inverse
      REAL   TMAT(4,4), TINV(4,4), TINVT(4,4)
*     Shortest rotation from light source to +z axis
      REAL   SROT(4,4), SRINV(4,4), SRINVT(4,4)
*
*     Distance (in +z) of viewing eye
      REAL   EYEPOS
*
*     Input mode
      INTEGER INMODE
*
*     Buffer one line of input for decoding
      CHARACTER*132 LINE
*
*     Input format(s)
      CHARACTER*80 INFMTS(MXTYPE),INFMT
*
*     Free-format input flag
      LOGICAL INFLGS(MXTYPE),INFLG
*
*     Stuff for shading
      REAL   NL(3),NORMAL(3),LDOTN
      REAL RGBCUR(3),RGBSHD(3),RGBFUL(3)
      REAL SPECOL(3)
*
*     Support for transparency
      REAL OPT(4)
*
*     Support for quadratic surfaces
      REAL QTRANS(17)
*
*     Support for NOTRANS (isolate objects from TMAT transformation)
      LOGICAL ISOLATE
*
*     Input buffer for details
      REAL   BUF(100)
*
*     Intermediate storage for output header
      INTEGER*2 NX,NY
*
*     Copy of NOISE for ASSERT to see
      INTEGER ASSOUT
      LOGICAL VERBOSE
      COMMON /ASSCOM/ ASSOUT, VERBOSE
      SAVE /ASSCOM/
*
*     The number of "details" each object type is supposed to have
*     :       input,  object, shadow
      INTEGER IDET(MXTYPE), SDET(MXTYPE)
*
c
c
      LOGICAL      HFLAG, XFLAG, SFLAG
      CHARACTER*80 FLAGS
*
*     Stuff for labels
      CHARACTER*80 FONTNAME, LABELSTRING, FONTALIGN
      REAL         FONTSIZE, FONTSCALE
*
* Support for a "glow" light source 
      REAL 	GLOWSRC(3), GLOWCOL(3), GDIST(3), GLOWRAD, GLOW
      INTEGER	GOPT, GPHONG
*
*     Keep track of actual coordinate limits (for information only)
      COMMON /NICETIES/ TRULIM
      REAL              TRULIM(3,2)
      TRULIM (1,1) = HUGE
      TRULIM (2,1) = HUGE
      TRULIM (3,1) = HUGE
      TRULIM (1,2) = -HUGE
      TRULIM (2,2) = -HUGE
      TRULIM (3,2) = -HUGE
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
      IDET(QUADRIC)   = 17
      IDET(VERTEXRGB) = 9
      SDET(TRIANG) = 13
      SDET(SPHERE) = 4
      SDET(CYLIND) = 8
      SDET(QUADRIC)= 14
      SDET(PLANE)  = 1
      SDET(NORMS ) = 1
      SDET(MATERIAL)  = 1
      SDET(GLOWLIGHT) = 1
      SDET(VERTEXRGB) = 1
*
*     Copy the info (also error reporting) unit number to common
      ASSOUT = NOISE
      WRITE (NOISE,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%',
     &                '%%%%%%%%%%%%%%%%%%%%%%%%%%'
      WRITE (NOISE,*) '%             Raster3D normalization ',
     &                'program V2.4b             %'
      WRITE (NOISE,*) '%            -------------------------',
     &                '-------------            %'
      WRITE (NOISE,*) '% If you publish figures generated by this ',
     &                'program please cite %'
      WRITE (NOISE,*) '%   Merritt & Bacon (1997)  ',
     &                'Meth. Enzymol. 277, 505-524.','       %'
      WRITE (NOISE,*) '%            -------------------------',
     &                '-------------            %'
      WRITE (NOISE,*) '% comments & suggestions to:  ',
     &                '  merritt@u.washington.edu','       %'
      WRITE (NOISE,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%',
     &                '%%%%%%%%%%%%%%%%%%%%%%%%%%'
c
c	-h option suppresses header records in output
c	-expand option expands and in-lines all file indirection
c		(and normalizes the in-lined data)
c	-noexpand (the default) simply copies file indirection commands
c		to output file
c	-stereo will create parallel files left.r3d and right.r3d
c
      HFLAG = .FALSE.
      XFLAG = .FALSE.
      SFLAG = .FALSE.
      NARG  = IARGC()
      DO i = 1, NARG
	CALL GETARG(I,FLAGS)
	IF (FLAGS(1:2) .EQ. '-h')   HFLAG = .TRUE.
	IF (FLAGS(1:4) .EQ. '-exp') XFLAG = .TRUE.
	IF (FLAGS(1:2) .EQ. '-s')   SFLAG = .TRUE.
      ENDDO
      IF (SFLAG) THEN
	HFLAG = .TRUE.
	OPEN(UNIT=LEFT,FILE='left.r3d',
     &	     CARRIAGECONTROL='LIST',
     &	     STATUS='UNKNOWN')
	OPEN(UNIT=RIGHT,FILE='right.r3d',
     &	     CARRIAGECONTROL='LIST',
     &	     STATUS='UNKNOWN')
      ENDIF
*
*     Ready for input records
      ILEVEL = 0
      INPUT  = STDIN
      ISOLATE = .FALSE.
*
*     Get title
1     FORMAT (A80)
      READ (INPUT,1) TITLE
      IF (.NOT.HFLAG) WRITE(OUTPUT,1) TITLE
      IF (SFLAG) WRITE(LEFT,1)  TITLE
      IF (SFLAG) WRITE(RIGHT,1) TITLE
*
*     Get number of tiles
      READ (INPUT,1,END=101) LINE
      READ (LINE,*,ERR=101) NTX,NTY
      IF (.NOT.HFLAG) WRITE (OUTPUT,1) LINE
      IF (SFLAG) WRITE (LEFT,1)  LINE
      IF (SFLAG) WRITE (RIGHT,1) LINE
      CALL ASSERT (NTX.GT.0, 'ntx.le.0')
      CALL ASSERT (NTY.GT.0, 'nty.le.0')
      GOTO 102
101   CALL ASSERT(.FALSE.,
     &           '>>> This doesnt look like a Raster3D input file! <<<')
102   CONTINUE
*
*     Get number of pixels per tile
      READ (INPUT,1) LINE
      READ (LINE,*) NPX,NPY
      IF (.NOT.HFLAG) WRITE (OUTPUT,1) LINE
      IF (SFLAG) WRITE (LEFT,1)  LINE
      IF (SFLAG) WRITE (RIGHT,1) LINE
      CALL ASSERT (NPX.GT.0, 'npx.le.0')
      CALL ASSERT (NPY.GT.0, 'npy.le.0')
*
*     Get pixel averaging scheme
      READ (INPUT,1) LINE
      READ (LINE,*) SCHEME
      IF (.NOT.HFLAG) WRITE (OUTPUT,1) LINE
      IF (SFLAG) WRITE (LEFT,1)  LINE
      IF (SFLAG) WRITE (RIGHT,1) LINE
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
	CALL ASSERT (MOD(NPX,2).EQ.0,'scheme 4 requires NPX even')
	CALL ASSERT (MOD(NPY,2).EQ.0,'scheme 4 requires NPY even')
	NPX = NPX + NPX/2
	NPY = NPY + NPY/2
	SCHEME = 3
      ELSE
        CALL ASSERT (.FALSE.,'crash 2')
      ENDIF
*
      NX = NOX*NTX
      NY = NOY*NTY
*
*     Some derived parameters
      XCENT = NTX*NPX/2.
      YCENT = NTY*NPY/2.
      SCALE = 2.*MIN(XCENT,YCENT)
*
*     Get background colour
      READ (INPUT,1) LINE
      READ (LINE,*) BKGND
      IF (.NOT.HFLAG) WRITE (OUTPUT,1) LINE
      IF (SFLAG) WRITE (LEFT,1)  LINE
      IF (SFLAG) WRITE (RIGHT,1) LINE
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
      IF (.NOT.HFLAG) WRITE (OUTPUT,1) LINE
      IF (SFLAG) WRITE (LEFT,1)  LINE
      IF (SFLAG) WRITE (RIGHT,1) LINE
*
*     Get Phong power
      READ (INPUT,1) LINE
      READ (LINE,*) IPHONG
      IF (.NOT.HFLAG) WRITE (OUTPUT,1) LINE
      IF (SFLAG) WRITE (LEFT,1)  LINE
      IF (SFLAG) WRITE (RIGHT,1) LINE
      CALL ASSERT (IPHONG.GE.0, 'iphong < 0')
*     A derived constant for numerical purposes in applying the
*     Phong power in the shading algorithm.
*     The idea is that any specular contribution less than
*     1E-9 (hence the 9 in 9./IPHONG) is insignificant:
      IF (IPHONG .NE. 0) PHOBND = 0.1**(9./IPHONG)
      IF (IPHONG .EQ. 0) PHOBND = 0.
*
*     Get contribution of straight-on (secondary) light source
      READ (INPUT,1) LINE
      READ (LINE,*) STRAIT
      IF (.NOT.HFLAG) WRITE (OUTPUT,1) LINE
      IF (SFLAG) WRITE (LEFT,1)  LINE
      IF (SFLAG) WRITE (RIGHT,1) LINE
      CALL ASSERT (STRAIT.GE.0., 'strait < 0')
      CALL ASSERT (STRAIT.LE.1., 'strait > 1')
*
*     Derive contribution of primary light source
      PRIMAR = 1. - STRAIT
*
*     Get contribution of ambient light
      READ (INPUT,1) LINE
      READ (LINE,*) AMBIEN
      IF (.NOT.HFLAG) WRITE (OUTPUT,1) LINE
      IF (SFLAG) WRITE (LEFT,1)  LINE
      IF (SFLAG) WRITE (RIGHT,1) LINE
      CALL ASSERT (AMBIEN.GE.0., 'ambien < 0')
      CALL ASSERT (AMBIEN.LE.1., 'ambien > 1')
*
*     Get component of specular reflection
      READ (INPUT,1) LINE
      READ (LINE,*) SPECLR
      IF (.NOT.HFLAG) WRITE (OUTPUT,1) LINE
      IF (SFLAG) WRITE (LEFT,1)  LINE
      IF (SFLAG) WRITE (RIGHT,1) LINE
      CALL ASSERT (SPECLR.GE.0., 'speclr < 0')
      CALL ASSERT (SPECLR.LE.1., 'speclr > 1')
*
*     Derive component of diffuse reflection
      CALL ASSERT (AMBIEN+SPECLR.LE.1., 'ambien+speclr > 1')
      DIFFUS = 1. - (AMBIEN+SPECLR)
*
*     Note distance of viewing eye, but don't apply it here
      READ (INPUT,1) LINE
      READ (LINE,*) EYEPOS
      IF (.NOT.HFLAG) WRITE (OUTPUT,1) LINE
      IF (SFLAG) WRITE (LEFT,1)  LINE
      IF (SFLAG) WRITE (RIGHT,1) LINE
      CALL ASSERT (EYEPOS.GE.0., 'eyepos.lt.0')
      EYEPOS = 0.0
*
*     Get position of primary light source
      READ (INPUT,1) LINE
      READ (LINE,*) SOURCE
      IF (.NOT.HFLAG) WRITE (OUTPUT,1) LINE
      IF (SFLAG) WRITE (LEFT,1)  LINE
      IF (SFLAG) WRITE (RIGHT,1) LINE
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
      IF (.NOT.HFLAG) THEN
	WRITE (OUTPUT,*) ' 1. 0. 0.   0.'
      	WRITE (OUTPUT,*) ' 0. 1. 0.   0.'
      	WRITE (OUTPUT,*) ' 0. 0. 1.   0.'
      	WRITE (OUTPUT,*) ' 0. 0. 0.   1.'
      ENDIF
      IF (SFLAG) THEN
	WRITE (LEFT,*)  ' 1.   0. 0.   0.'
      	WRITE (LEFT,*)  ' 0.   1. 0.   0.'
      	WRITE (LEFT,*)  ' 0.03 0. 1.   0.         Left eye view'
      	WRITE (LEFT,*)  ' 0.   0. 0.   1.'
	WRITE (RIGHT,*) ' 1.   0. 0.   0.'
      	WRITE (RIGHT,*) ' 0.   1. 0.   0.'
      	WRITE (RIGHT,*) '-0.03 0. 1.   0.         Right eye view'
      	WRITE (RIGHT,*) ' 0.   0. 0.   1.'
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
      SROT(1,4) = 0.0
      SROT(2,4) = 0.0
      SROT(3,4) = 0.0
      SROT(4,1) = 0.0
      SROT(4,2) = 0.0
      SROT(4,3) = 0.0
      SROT(4,4) = 1.0
*
*     Quadrics will require the inverse matrix also (and its transpose)
      CALL QSETUP
*
*     Get input mode
      READ (INPUT,1) LINE
      READ (LINE,*) INMODE
      IF (.NOT.HFLAG) WRITE (OUTPUT,1) LINE
      IF (SFLAG) WRITE (LEFT,1)  LINE
      IF (SFLAG) WRITE (RIGHT,1) LINE
      CALL ASSERT (INMODE.GE.1.,'bad inmode')
      IF (INMODE.GT.3) WRITE (NOISE,*) 'Non-standard INMODE',INMODE
*
*     Get input format(s)
      IF (INMODE.EQ.1.OR.INMODE.EQ.2) THEN
        READ (INPUT,'(A)') INFMT
        IF (.NOT.HFLAG) WRITE (OUTPUT,'(A1)') '*'
        IF (SFLAG) WRITE (LEFT, '(A1)') '*'
        IF (SFLAG) WRITE (RIGHT,'(A1)') '*'
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
          IF (.NOT.HFLAG) WRITE (OUTPUT,'(A1)') '*'
          IF (SFLAG) WRITE (LEFT, '(A1)') '*'
          IF (SFLAG) WRITE (RIGHT,'(A1)') '*'
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
	INFLGS(PLANE)     = INFLGS(TRIANG)
	INFMTS(PLANE)     = INFMTS(TRIANG)
	INFLGS(NORMS)     = INFLGS(TRIANG)
	INFMTS(NORMS)     = INFMTS(TRIANG)
	INFLGS(VERTEXRGB) = INFLGS(TRIANG)
	INFMTS(VERTEXRGB) = INFMTS(TRIANG)
	INFLGS(MATERIAL)  = .TRUE.
	INFLGS(FONT)      = .TRUE.
	INFLGS(LABEL)     = .TRUE.
	INFLGS(GLOWLIGHT) = .TRUE.
 	INFLGS(QUADRIC)   = .TRUE.
      ELSE
        CALL ASSERT (.FALSE.,'crash 4')
      ENDIF
*
      IF (SFLAG) CLOSE(LEFT)
      IF (SFLAG) CLOSE(RIGHT)
*
*
c
      npropm  = 0
      ntransp = 0
      nsphere = 0
      ncylind = 0
      nplanes = 0
      ntriang = 0
      nquads  = 0
c
c     Objects in, and count up objects that may impinge on each tile
      NDET = 0
      MDET = 0
      N = 0
c
c     Read in next object
7     CONTINUE
      IF (INMODE.EQ.1.OR.INMODE.EQ.2) THEN
        INTYPE = INMODE
	GOTO 8
      ENDIF
c     Aug 1996 - allow file indirection
      READ (INPUT,'(A)',END=50) LINE
      IF (LINE(1:1) .EQ. '#') THEN
	WRITE (OUTPUT,1) LINE
	GOTO 7
      ELSE IF (LINE(1:1) .NE. '@') THEN
 	READ (LINE,*) INTYPE
      ELSE IF (.NOT.XFLAG) THEN
	WRITE (OUTPUT,1) LINE
	GOTO 7
      ELSE
 	CALL ASSERT(ILEVEL.LT.MAXLEV, 
     &	            'Too many levels of indirection')
 	DO I=80,2,-1
	  IF (LINE(I:I).NE.' ') J = I
 	  IF (LINE(I:I).EQ.' ') LINE(I:I) = CHAR(0)
 	ENDDO
C f90 	OPEN (UNIT=INPUT0+ILEVEL,ERR=71,STATUS='OLD',ACTION='READ',
	OPEN (UNIT=INPUT0+ILEVEL,ERR=71,STATUS='OLD',
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
      	IF (INTYPE .EQ. MATEND) THEN
	    WRITE (OUTPUT,1) LINE 
	    ISOLATE = .FALSE.
	    GOTO 7
	ELSE
	    WRITE (OUTPUT,'(I2)') INTYPE
	END IF
	IF (INTYPE.EQ.CYLFLAT) INTYPE = CYLIND
        CALL ASSERT (INTYPE.GE.1.AND.INTYPE.LE.MXTYPE,'bad object')
        CALL ASSERT (INTYPE.NE.PEARLS,'sorry, no pearls yet')
c
8     CONTINUE
        INFMT = INFMTS(INTYPE)
        INFLG = INFLGS(INTYPE)
c
      IF (INTYPE.EQ.FONT) THEN
*	handle non-numerical input elsewhere
	GOTO 9
      ELSE IF (INTYPE.EQ.NOTRANS) THEN
	ISOLATE = .TRUE.
	GOTO 7
      ELSE IF (INFLG) THEN
        READ (INPUT,*,END=50) (BUF(I),I=1,IDET(INTYPE))
      ELSE
        READ (INPUT,INFMT,END=50) (BUF(I),I=1,IDET(INTYPE))
      ENDIF
*     Allow an all-zeroes record to terminate input for the
*     benefit of those of us who might inadvertently supply
*     a series of blank records after our real input as a
*     side-effect of tape blocking or sloppiness or ...
      DO I=1,IDET(INTYPE)
        IF (BUF(I).NE.0.) GO TO 9
      ENDDO
      GO TO 50
9     CONTINUE
      N = N + 1
*     From this point on, we'll use the symbolic codes for objects
      IF (INTYPE.EQ.TRIANG .or. INTYPE.EQ.PLANE) THEN
*       triangle as read in
	IF (INTYPE.EQ.TRIANG) NTRIANG = NTRIANG + 1
	IF (INTYPE.EQ.PLANE)  NPLANE  = NPLANE  + 1
	NDET = NDET + IDET(INTYPE)
	MDET = MDET + SDET(INTYPE)
        X1A = BUF(1)
        Y1A = BUF(2)
        Z1A = BUF(3)
        X2A = BUF(4)
        Y2A = BUF(5)
        Z2A = BUF(6)
        X3A = BUF(7)
        Y3A = BUF(8)
        Z3A = BUF(9)
        RED = BUF(10)
        GRN = BUF(11)
        BLU = BUF(12)
	CALL CHKRGB( RED,GRN,BLU,'invalid triangle color')
        CALL ASSERT (IDET(INTYPE).EQ.12,'idet(1).ne.12')
*	Isolated objects not transformed by TMAT
	IF (.NOT.ISOLATE) THEN
*	update true coordinate limits
	  TRULIM(1,1) = MIN( TRULIM(1,1), X1A,X2A,X3A)
	  TRULIM(1,2) = MAX( TRULIM(1,2), X1A,X2A,X3A)
	  TRULIM(2,1) = MIN( TRULIM(2,1), Y1A,Y2A,Y3A)
	  TRULIM(2,2) = MAX( TRULIM(2,2), Y1A,Y2A,Y3A)
	  TRULIM(3,1) = MIN( TRULIM(3,1), Z1A,Z2A,Z3A)
	  TRULIM(3,2) = MAX( TRULIM(3,2), Z1A,Z2A,Z3A)
*       modify the input, so to speak
          CALL TRANSF (X1A,Y1A,Z1A, TMAT)
          CALL TRANSF (X2A,Y2A,Z2A, TMAT)
          CALL TRANSF (X3A,Y3A,Z3A, TMAT)
	ENDIF
*       write it back out again
        WRITE (OUTPUT,91) X1A,Y1A,Z1A, X2A,Y2A,Z2A, X3A,Y3A,Z3A, 
     &                    RED,GRN,BLU
91      FORMAT( 3F9.5,1X,3F9.5,1X,3F9.5,1X,3F6.3 )
      ELSEIF (INTYPE.EQ.SPHERE) THEN
*       sphere as read in
	NSPHERE = NSPHERE + 1
	NDET = NDET + IDET(INTYPE)
	MDET = MDET + SDET(INTYPE)
        XA = BUF(1)
        YA = BUF(2)
        ZA = BUF(3)
        RA = BUF(4)
        RED = BUF(5)
        GRN = BUF(6)
        BLU = BUF(7)
	CALL CHKRGB( RED,GRN,BLU,'invalid sphere color')
        CALL ASSERT (IDET(INTYPE).EQ.7,'idet(2).ne.7')
*	Isolated objects not transformed by TMAT
	IF (.NOT.ISOLATE) THEN
*	update true coordinate limits
	  TRULIM(1,1) = MIN( TRULIM(1,1), XA )
	  TRULIM(1,2) = MAX( TRULIM(1,2), XA )
	  TRULIM(2,1) = MIN( TRULIM(2,1), YA )
	  TRULIM(2,2) = MAX( TRULIM(2,2), YA )
	  TRULIM(3,1) = MIN( TRULIM(3,1), ZA )
	  TRULIM(3,2) = MAX( TRULIM(3,2), ZA )
*       modify the input, as it were
          CALL TRANSF (XA,YA,ZA, TMAT)
          RA = RA / TMAT(4,4)
	ENDIF
*       write it back out again
	WRITE (OUTPUT,92) XA,YA,ZA, RA, RED,GRN,BLU
92      FORMAT( 3F9.5,1X,F9.5,1X,3F6.3 )
      ELSEIF (INTYPE.EQ.CYLIND) THEN
	NCYLIND = NCYLIND + 1
	NDET = NDET + IDET(INTYPE)
	MDET = MDET + SDET(INTYPE)
        X1A = BUF(1)
        Y1A = BUF(2)
        Z1A = BUF(3)
	R1A = BUF(4)
        X2A = BUF(5)
        Y2A = BUF(6)
        Z2A = BUF(7)
	R2A = R1A
        RED = BUF(9)
        GRN = BUF(10)
        BLU = BUF(11)
	CALL CHKRGB( RED,GRN,BLU,'invalid cylinder color')
        CALL ASSERT (IDET(INTYPE).EQ.11,'idet(1).ne.11')
*	Isolated objects not transformed by TMAT
	IF (.NOT.ISOLATE) THEN
*	update true coordinate limits
	  TRULIM(1,1) = MIN( TRULIM(1,1), X1A,X2A)
	  TRULIM(1,2) = MAX( TRULIM(1,2), X1A,X2A)
	  TRULIM(2,1) = MIN( TRULIM(2,1), Y1A,Y2A)
	  TRULIM(2,2) = MAX( TRULIM(2,2), Y1A,Y2A)
	  TRULIM(3,1) = MIN( TRULIM(3,1), Z1A,Z2A)
	  TRULIM(3,2) = MAX( TRULIM(3,2), Z1A,Z2A)
*       modify the input, so to speak
          CALL TRANSF (X1A,Y1A,Z1A, TMAT)
          CALL TRANSF (X2A,Y2A,Z2A, TMAT)
          R1A = R1A / TMAT(4,4)
          R2A = R2A / TMAT(4,4)
	ENDIF
*       write it back out again
        WRITE (OUTPUT,93) X1A,Y1A,Z1A, R1A, X2A,Y2A,Z2A, R2A, 
     &                    RED,GRN,BLU
93      FORMAT ( 3F9.5,1X,F9.5,1X,3F9.5,1X,F9.5,1X,3F6.3 )
      ELSEIF (INTYPE.EQ.NORMS) THEN
*	vertex normals as given (these belong to previous triangle)
	NNORMS = NNORMS + 1
	NDET = NDET + IDET(INTYPE)
	MDET = MDET + SDET(INTYPE)
        X1A = BUF(1)
        Y1A = BUF(2)
        Z1A = BUF(3)
        X2A = BUF(4)
        Y2A = BUF(5)
        Z2A = BUF(6)
        X3A = BUF(7)
        Y3A = BUF(8)
        Z3A = BUF(9)
*	Apply rotation matrix, but not translation components
	X1B = X1A*TMAT(1,1) + Y1A*TMAT(2,1) + Z1A*TMAT(3,1)
	Y1B = X1A*TMAT(1,2) + Y1A*TMAT(2,2) + Z1A*TMAT(3,2)
	Z1B = X1A*TMAT(1,3) + Y1A*TMAT(2,3) + Z1A*TMAT(3,3)
	X2B = X2A*TMAT(1,1) + Y2A*TMAT(2,1) + Z2A*TMAT(3,1)
	Y2B = X2A*TMAT(1,2) + Y2A*TMAT(2,2) + Z2A*TMAT(3,2)
	Z2B = X2A*TMAT(1,3) + Y2A*TMAT(2,3) + Z2A*TMAT(3,3)
	X3B = X3A*TMAT(1,1) + Y3A*TMAT(2,1) + Z3A*TMAT(3,1)
	Y3B = X3A*TMAT(1,2) + Y3A*TMAT(2,2) + Z3A*TMAT(3,2)
	Z3B = X3A*TMAT(1,3) + Y3A*TMAT(2,3) + Z3A*TMAT(3,3)
*       write it back out again
	WRITE (OUTPUT,91) X1B,Y1B,Z1B, X2B,Y2B,Z2B, X3B,Y3B,Z3B
*
*	Vertex colors for preceding triangle 
*	(no transformation needed)
*
      ELSEIF (INTYPE .EQ. VERTEXRGB) THEN
	NDET = NDET + IDET(INTYPE)
	MDET = MDET + SDET(INTYPE)
	CALL CHKRGB( BUF(1),BUF(2),BUF(3),'invalid vertex color')
	CALL CHKRGB( BUF(4),BUF(5),BUF(6),'invalid vertex color')
	CALL CHKRGB( BUF(7),BUF(8),BUF(9),'invalid vertex color')
	WRITE (OUTPUT,91) BUF(1),BUF(2),BUF(3),BUF(4),BUF(5),BUF(6),
     &	                  BUF(7),BUF(8),BUF(9)
*
*	Material properties are saved with no further manipulation
*
      ELSEIF (INTYPE .EQ. MATERIAL) THEN
	NPROPM = NPROPM + 1
	NDET = NDET + IDET(INTYPE)
	MDET = MDET + SDET(INTYPE)
	SPHONG = BUF(1)
	SSPEC  = BUF(2)
	SR     = BUF(3)
	SG     = BUF(4)
	SB     = BUF(5)
	CLRITY = BUF(6)
	OPT(1) = BUF(7)
	OPT(2) = BUF(8)
	OPT(3) = BUF(9)
	OPT(4) = BUF(10)
	WRITE (OUTPUT,94) SPHONG, SSPEC, SR,SG,SB, CLRITY,
     &                    OPT(1),OPT(2),OPT(3),OPT(4)
94      FORMAT (F5.0,F6.3,1X,3F7.3,1X,F6.3,3X,4F6.1)
*	24-Feb-1997 OPT(4) signals additional MATERIAL records
	IF (OPT(4).GT.0) THEN
	  DO I = 1, OPT(4)
            READ (INPUT,'(A)',END=50) LINE
	    WRITE(OUTPUT,1) LINE
	  ENDDO
	ENDIF
*
*     Font alignment options:
*	C - centered, have to transform coords
*	L - left,     ""
*	R - right,    ""
*	O - offset,   leave font offsets alone!
      ELSEIF (INTYPE .EQ. FONT) THEN
        READ (INPUT,'(A)',END=50) LINE
	READ (LINE,*) FONTNAME, FONTSIZE, FONTALIGN
	IF (FONTALIGN(1:1).EQ.'C') THEN
	    IALIGN=1
	ELSE IF (FONTALIGN(1:1).EQ.'R') THEN
	    IALIGN=2
	ELSE IF (FONTALIGN(1:1).EQ.'O') THEN
	    IALIGN=3
	ELSE
	    IALIGN=0
	ENDIF
	WRITE(OUTPUT,1) LINE
	GOTO 7
      ELSEIF (INTYPE .EQ. LABEL ) THEN
	READ (INPUT,702,END=50) len,LABELSTRING
702	FORMAT(Q,A)
	nlabels = nlabels + 1
        XA = BUF(1)
        YA = BUF(2)
        ZA = BUF(3)
	IF (.NOT.ISOLATE) THEN
          IF (IALIGN.NE.3) CALL TRANSF (XA,YA,ZA, TMAT)
	ENDIF
        RED = BUF(4)
        GRN = BUF(5)
        BLU = BUF(6)
	CALL CHKRGB( RED,GRN,BLU,'invalid label color')
	WRITE (OUTPUT,801) XA,YA,ZA, RED,GRN,BLU
801	FORMAT(3f10.4,2x,3f6.3)
	WRITE (OUTPUT,1) LABELSTRING
	GOTO 7
 
      ELSEIF (INTYPE .EQ. GLOWLIGHT) THEN 
 	GLOWSRC(1) = BUF(1)
 	GLOWSRC(2) = BUF(2)
 	GLOWSRC(3) = BUF(3)
 	GLOWRAD    = BUF(4)
 	GLOW       = BUF(5)
 	GOPT       = BUF(6)
 	GPHONG     = BUF(7)
*	Isolated objects not transformed by TMAT
	IF (.NOT.ISOLATE) THEN
 	  CALL TRANSF(GLOWSRC(1),GLOWSRC(2),GLOWSRC(3),TMAT)
 	  GLOWRAD = GLOWRAD/TMAT(4,4)
	ENDIF
 	RED = BUF(8)
 	GRN = BUF(9)
 	BLU = BUF(10)
	CALL CHKRGB( RED,GRN,BLU,'invalid glow light color')
 	WRITE (OUTPUT,802) GLOWSRC,GLOWRAD,GLOW,GOPT,GPHONG,RED,GRN,BLU
802	FORMAT(4f10.4,1x,f4.2,1x,i4,1x,i4,2x,3f6.3)
 	GOTO 7
*
      ELSEIF (INTYPE.EQ.QUADRIC) THEN
 	NQUADS = NQUADS + 1
 	call QINP( BUF(1), QTRANS(1), .FALSE., BUF(50) )
 	xa = (qtrans(1) - xcent) / scale
 	ya = (qtrans(2) - ycent) / scale
 	za =  qtrans(3) / scale
 	ra =  qtrans(4) / scale
 	write (output,803) xa,ya,za,ra, (qtrans(i),i=5,17)
  803	format(4f10.4,4x,3f6.3,/,10f10.4)
*
      ELSEIF (INTYPE.EQ.TRCONE) THEN
        CALL ASSERT(.FALSE.,'trcones coming soon...')
      ELSEIF (INTYPE.EQ.PEARLS) THEN
        CALL ASSERT(.FALSE.,'pearls coming soon...')
      ELSE
        CALL ASSERT(.FALSE.,'crash 50')
      ENDIF
      GO TO 7
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
      WRITE(NOISE,*)'-------------------------------'
      IF (NPLANES.NE.0) WRITE(NOISE,*) 'planes            =',NPLANES
      IF (NSPHERE.NE.0) WRITE(NOISE,*) 'spheres           =',NSPHERE
      IF (NCYLIND.NE.0) WRITE(NOISE,*) 'cylinders         =',NCYLIND
      IF (NTRIANG.NE.0) WRITE(NOISE,*) 'triangles         =',NTRIANG
      IF (NQUADS .NE.0) WRITE(NOISE,*) 'quadratic surfaces=',NQUADS
      IF (NTRANSP.NE.0) WRITE(NOISE,*) 'transparent objs  =',NTRANSP
      WRITE(NOISE,*)'-------------------------------'
*
      WRITE (NOISE,*) 'Compare these to the array dimensions in render:'
      IF (NPROPM.NE.0) WRITE(NOISE,*)  'special materials =',NPROPM,
     &                '   (check against MAXMAT)'
      WRITE(NOISE,*)  'total objects     =',N,
     &                '   (check against MAXOBJ)'
      WRITE (NOISE,*) 'details           =',NDET,
     &                '   (check against MAXDET)'
      WRITE (NOISE,*) 'shadow details    =',MDET,
     &                '   (check against MAXSDT)'
      WRITE(NOISE,*)'-------------------------------'
*
*
*     Sort objects, fill in "short lists" as indices into main list
*     (note that it would lend itself better to "parallel
*     processing" to form the short lists first and then
*    
      WRITE (NOISE,*) 'True center of input coordinates (not used):'
	XA = (TRULIM(1,1) + TRULIM(1,2)) / 2.0
	YA = (TRULIM(2,1) + TRULIM(2,2)) / 2.0
	ZA = (TRULIM(3,1) + TRULIM(3,2)) / 2.0
      WRITE (NOISE,*) XA, YA, ZA
*
      end

      SUBROUTINE ASSERT (LOGIC, DAMMIT)
      LOGICAL LOGIC
      CHARACTER*(*) DAMMIT
      INTEGER ASSOUT
      LOGICAL VERBOSE
      COMMON /ASSCOM/ ASSOUT, VERBOSE
      SAVE /ASSCOM/
      IF (LOGIC) RETURN
      WRITE (ASSOUT,*) '*** ',DAMMIT
      STOP 1234
      END

      SUBROUTINE TRANSF (X,Y,Z, T)
      REAL   X,Y,Z,T(4,4)
      REAL   H(4)
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

	SUBROUTINE CHKRGB( RED, GRN, BLU, MESSAGE )
	REAL RED, GRN, BLU
	CHARACTER*(*) MESSAGE
        CALL ASSERT (RED.GE.0., MESSAGE)
        CALL ASSERT (GRN.GE.0., MESSAGE)
        CALL ASSERT (BLU.GE.0., MESSAGE)
        CALL ASSERT (RED.LE.1., MESSAGE)
        CALL ASSERT (GRN.LE.1., MESSAGE)
        CALL ASSERT (BLU.LE.1., MESSAGE)
	RETURN
	END