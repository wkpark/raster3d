      PROGRAM NORMAL3D
*
* EAM Jan 1996	- 
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
*     - SCHEME   pixel averaging scheme (1, 2, or 3)
*       - 1 means 1 computing pixel for 1 output pixel
*       - 2 means 2x2 computing pixels for 1 output pixel
*       - 3 means 3x3 computing pixels for 2x2 output pixels
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
*
*-----------------------------------------------------------------------------
*
*     Overkill:
      IMPLICIT REAL*4 (A-H, O-Z)
*
*     I/O units for control input, image output, info output
      INTEGER INPUT, OUTPUT, NOISE
      PARAMETER (INPUT=5, OUTPUT=6, NOISE=0)
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
      PARAMETER (MXTYPE   = 9)
*
*     $$$$$$$$$$$$$$$$$  END OF LIMITS  $$$$$$$$$$$$$$$$$$$$$$$
*
*
*     Title for run
      CHARACTER*80 TITLE
      CHARACTER*80 LINE
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
*     Input format(s)
      CHARACTER*80 INFMTS(MXTYPE),INFMT
*
*     Free-format input flag
      LOGICAL INFLGS(MXTYPE),INFLG
*
*     Shortest rotation from light source to +z axis
      REAL*4 SROT(3,3)
*
*     Stuff for shading
      REAL*4 NL(3),NORMAL(3),LDOTN
      REAL RGBCUR(3),RGBSHD(3),RGBFUL(3)
      REAL SPECOL(3)
*
*     Support for transparency
      REAL OPT(4)
*
*     Input buffer for details
      REAL*4 BUF(100)
*
*     Intermediate storage for output header
      INTEGER*2 NX,NY
*
*     Copy of NOISE for ASSERT to see
      INTEGER ASSOUT
      COMMON /ASSCOM/ ASSOUT
      SAVE /ASSCOM/
*
*     The number of "details" each object type is supposed to have
*     :       input,  object, shadow
      INTEGER IDET(MXTYPE), SDET(MXTYPE)
*
c
c	-h option suppresses header records in output
c
      LOGICAL      HFLAG
      CHARACTER*80 FLAGS
*
*     Keep track of actual coordinate limits (for information only)
      REAL*4 TRULIM(3,2)
      DATA TRULIM / 9999.,9999.,9999.,-9999.,-9999.,-9999. /
*
      IDET(TRIANG) = 12
      IDET(SPHERE) = 7
      IDET(PEARLS) = 11
      IDET(TRCONE) = 11
      IDET(CYLIND) = 11
      IDET(PLANE)  = 12
      IDET(NORMS ) = 9
      IDET(MATERIAL) = 10
      SDET(TRIANG) = 13
      SDET(SPHERE) = 4
      SDET(CYLIND) = 8
      SDET(PLANE)  = 1
      SDET(NORMS ) = 1
      SDET(MATERIAL) = 1
*
*     Copy the info (also error reporting) unit number to common
      ASSOUT = NOISE
      WRITE (NOISE,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%',
     &                '%%%%%%%%%%%%%%%%%%%%%%%%%%'
      WRITE (NOISE,*) '%             Raster3D normalization program ',
     &                'V2.2              %'
      WRITE (NOISE,*) '%            -------------------------',
     &                '-------------            %'
      WRITE (NOISE,*) '% If you publish figures generated by this ',
     &                'program please cite %'
      WRITE (NOISE,*) '%   Bacon & Anderson (1988) ',
     &                'J. Molec. Graphics 6, 219-220 and  %'
      WRITE (NOISE,*) '%   Merritt & Murphy (1994) ',
     &                'Acta Cryst. D50, 869-873.','          %'
      WRITE (NOISE,*) '%            -------------------------',
     &                '-------------            %'
      WRITE (NOISE,*) '% comments & suggestions to:  ',
     &                '  merritt@u.washington.edu','       %'
      WRITE (NOISE,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%',
     &                '%%%%%%%%%%%%%%%%%%%%%%%%%%'
c
c	-h option suppresses header records in output
c
      HFLAG = .FALSE.
      NARG  = IARGC()
      DO i = 1, NARG
	CALL GETARG(I,FLAGS)
	IF (FLAGS(1:2) .EQ. '-h') HFLAG = .TRUE.
      ENDDO
*
*     Get title
1     FORMAT (A80)
      READ (INPUT,1) TITLE
      IF (.NOT.HFLAG) WRITE(OUTPUT,1) TITLE
*
*     Get number of tiles
      READ (INPUT,1) LINE
      READ (LINE,*) NTX,NTY
      IF (.NOT.HFLAG) WRITE (OUTPUT,1) LINE
      CALL ASSERT (NTX.GT.0, 'ntx.le.0')
      CALL ASSERT (NTY.GT.0, 'nty.le.0')
*
*     Get number of pixels per tile
      READ (INPUT,1) LINE
      READ (LINE,*) NPX,NPY
      IF (.NOT.HFLAG) WRITE (OUTPUT,1) LINE
      CALL ASSERT (NPX.GT.0, 'npx.le.0')
      CALL ASSERT (NPY.GT.0, 'npy.le.0')
*
*     Get pixel averaging scheme
      READ (INPUT,1) LINE
      READ (LINE,*) SCHEME
      IF (.NOT.HFLAG) WRITE (OUTPUT,1) LINE
      CALL ASSERT (SCHEME.GE.1 .AND. SCHEME.LE.3, 'bad scheme')
      IF (SCHEME.EQ.1) THEN
        NOX = NPX
        NOY = NPY
      ELSEIF (SCHEME.EQ.2) THEN
        NOX = NPX/2
        NOY = NPY/2
        CALL ASSERT (MOD(NPX,2).EQ.0,'mod(npx,2).ne.0')
        CALL ASSERT (MOD(NPY,2).EQ.0,'mod(npy,2).ne.0')
      ELSEIF (SCHEME.EQ.3) THEN
        NOX = 2*(NPX/3)
        NOY = 2*(NPY/3)
        CALL ASSERT (MOD(NPX,3).EQ.0,'mod(npx,3).ne.0')
        CALL ASSERT (MOD(NPY,3).EQ.0,'mod(npy,3).ne.0')
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
*
*     Get Phong power
      READ (INPUT,1) LINE
      READ (LINE,*) IPHONG
      IF (.NOT.HFLAG) WRITE (OUTPUT,1) LINE
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
      CALL ASSERT (AMBIEN.GE.0., 'ambien < 0')
      CALL ASSERT (AMBIEN.LE.1., 'ambien > 1')
*
*     Get component of specular reflection
      READ (INPUT,1) LINE
      READ (LINE,*) SPECLR
      IF (.NOT.HFLAG) WRITE (OUTPUT,1) LINE
      CALL ASSERT (SPECLR.GE.0., 'speclr < 0')
      CALL ASSERT (SPECLR.LE.1., 'speclr > 1')
*
*     Derive component of diffuse reflection
      CALL ASSERT (AMBIEN+SPECLR.LE.1., 'ambien+speclr > 1')
      DIFFUS = 1. - (AMBIEN+SPECLR)
*
*     Get distance of viewing eye
      READ (INPUT,1) LINE
      READ (LINE,*) EYEPOS
      IF (.NOT.HFLAG) WRITE (OUTPUT,1) LINE
      CALL ASSERT (EYEPOS.GT.0., 'eyepos.le.0')
*
*     Get position of primary light source
      READ (INPUT,1) LINE
      READ (LINE,*) SOURCE
      IF (.NOT.HFLAG) WRITE (OUTPUT,1) LINE
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
      IF (.NOT.HFLAG) WRITE (OUTPUT,*) ' 1. 0. 0.   0.'
      IF (.NOT.HFLAG) WRITE (OUTPUT,*) ' 0. 1. 0.   0.'
      IF (.NOT.HFLAG) WRITE (OUTPUT,*) ' 0. 0. 1.   0.'
      IF (.NOT.HFLAG) WRITE (OUTPUT,*) ' 0. 0. 0.   1.'
*
*     Get input mode
      READ (INPUT,1) LINE
      READ (LINE,*) INMODE
      IF (.NOT.HFLAG) WRITE (OUTPUT,1) LINE
      CALL ASSERT (INMODE.GE.1.,'bad inmode')
      IF (INMODE.GT.3) WRITE (NOISE,*) 'Non-standard INMODE',INMODE
*
*     Get input format(s)
      IF (INMODE.EQ.1.OR.INMODE.EQ.2) THEN
        READ (INPUT,'(A)') INFMT
        IF (.NOT.HFLAG) WRITE (OUTPUT,'(A1)') '*'
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
      npropm  = 0
      ntransp = 0
      nsphere = 0
      ncylind = 0
      nplanes = 0
      ntriang = 0
c
c     Objects in, and count up objects that may impinge on each tile
      NDET = 0
      MDET = 0
      N = 0
c
7     CONTINUE
      IF (INMODE.EQ.1.OR.INMODE.EQ.2) THEN
        INTYPE = INMODE
      ELSEIF (INMODE.GE.3) THEN
        READ (INPUT,'(A80)',END=50) LINE
	READ (LINE,*) INTYPE
	IF (INTYPE .EQ. MATEND) THEN
	    WRITE (OUTPUT,*) LINE 
	    GOTO 7
	ELSE
	    WRITE (OUTPUT,'(I2)') INTYPE
	END IF
        IF (INTYPE.EQ.0) GO TO 50
        CALL ASSERT (INTYPE.GE.1.AND.INTYPE.LE.MXTYPE,'bad object')
c       CALL ASSERT (INTYPE.NE.TRCONE,'sorry, no trcones yet')
        CALL ASSERT (INTYPE.NE.PEARLS,'sorry, no pearls yet')
        INFMT = INFMTS(INTYPE)
        INFLG = INFLGS(INTYPE)
      ELSE
        CALL ASSERT (.FALSE.,'crash 8')
      ENDIF
      IF (INFLG) THEN
        READ (INPUT,*,END=50) (BUF(I),I=1,IDET(INTYPE))
      ELSE
        READ (INPUT,INFMT,END=50) (BUF(I),I=1,IDET(INTYPE))
      ENDIF
*     Allow an all-zeroes record to terminate input for the
*     benefit of those of us who might inadvertently supply
*     a series of blank records after our real input as a
*     side-effect of tape blocking or sloppiness or ...
      DO 8 I=1,IDET(INTYPE)
        IF (BUF(I).NE.0.) GO TO 9
8     CONTINUE
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
        CALL ASSERT (RED.GE.0., 'red < 0 in triangle')
        CALL ASSERT (GRN.GE.0., 'grn < 0 in triangle')
        CALL ASSERT (BLU.GE.0., 'blu < 0 in triangle')
        CALL ASSERT (RED.LE.1., 'red > 1 in triangle')
        CALL ASSERT (GRN.LE.1., 'grn > 1 in triangle')
        CALL ASSERT (BLU.LE.1., 'blu > 1 in triangle')
        CALL ASSERT (IDET(INTYPE).EQ.12,'idet(1).ne.12')
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
*       write it back out again
        WRITE (OUTPUT,91) X1A,Y1A,Z1A, X2A,Y2A,Z2A, X3A,Y3A,Z3A, 
     &                    RED,GRN,BLU
91      FORMAT( 3F9.5,X,3F9.5,X,3F9.5,X,3F6.3 )
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
        CALL ASSERT (RED.GE.0., 'red < 0 in sphere')
        CALL ASSERT (GRN.GE.0., 'grn < 0 in sphere')
        CALL ASSERT (BLU.GE.0., 'blu < 0 in sphere')
        CALL ASSERT (RED.LE.1., 'red > 1 in sphere')
        CALL ASSERT (GRN.LE.1., 'grn > 1 in sphere')
        CALL ASSERT (BLU.LE.1., 'blu > 1 in sphere')
        CALL ASSERT (IDET(INTYPE).EQ.7,'idet(2).ne.7')
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
*       write it back out again
	WRITE (OUTPUT,92) XA,YA,ZA, RA, RED,GRN,BLU
92      FORMAT( 3F9.5,X,F9.5,X,3F6.3 )
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
        CALL ASSERT (RED.GE.0., 'red < 0 in cylinder')
        CALL ASSERT (GRN.GE.0., 'grn < 0 in cylinder')
        CALL ASSERT (BLU.GE.0., 'blu < 0 in cylinder')
        CALL ASSERT (RED.LE.1., 'red > 1 in cylinder')
        CALL ASSERT (GRN.LE.1., 'grn > 1 in cylinder')
        CALL ASSERT (BLU.LE.1., 'blu > 1 in cylinder')
        CALL ASSERT (IDET(INTYPE).EQ.11,'idet(1).ne.11')
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
*       write it back out again
        WRITE (OUTPUT,93) X1A,Y1A,Z1A, R1A, X2A,Y2A,Z2A, R2A, 
     &                    RED,GRN,BLU
93      FORMAT ( 3F9.5,X,F9.5,X,3F9.5,X,F9.5,X,3F6.3 )
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
94      FORMAT (F5.0,F6.3,X,3F6.3,X,F6.3,3X,4F3.0)
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
*
      WRITE(NOISE,*)'-------------------------------'
      IF (NPLANES.NE.0) WRITE(NOISE,*) 'planes            =',NPLANES
      IF (NSPHERE.NE.0) WRITE(NOISE,*) 'spheres           =',NSPHERE
      IF (NCYLIND.NE.0) WRITE(NOISE,*) 'cylinders         =',NCYLIND
      IF (NTRIANG.NE.0) WRITE(NOISE,*) 'triangles         =',NTRIANG
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

