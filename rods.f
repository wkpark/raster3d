      PROGRAM RODS
*------------------------------------------------------------------------------
*       Program to set up input for RENDER with CYLINDERs drawn between
*	each pair of atoms lying closer than 0.6 * (sum of VanderWaals radii). 
*	This program is the same as SETUP, except for what is generated.
*	Input matrix or angles are taken from setup.matrix or setup.angles
*	(NB: same files as setup)
*
* Eric Swanson   Oct 1991
*	Modified to generate cylinders with half bond colors, where needed.
* EAM Feb 1997
*	-radius XX to set cylinder radius
* EAM Sep 1997
*	-default colors; option for coloring by B-value
*	-more generous output formats (FORMATs 130 and 140)
* EAM Jun 1999
*	-brad XX to set ball radius as fraction of Van der Waals radius
* EAM Jul 1999
*	don't draw bonds across alternate chain locations
*
*------------------------------------------------------------------------------
*     I/O units for colour/co-ordinate input, specs output, user output
      INTEGER INPUT, OUTPUT, NOISE
      PARAMETER (INPUT=5, OUTPUT=6, NOISE=0)
      PARAMETER (MAXCOL=5000, MAXATM=10000)
      REAL RGB(3,MAXCOL),VDW(MAXCOL)
      REAL SPAM(6,MAXATM)
      REAL CEN(3)
      CHARACTER*24 MASK(MAXCOL),TEST
      CHARACTER*80 ATOM(MAXATM),CARD
      LOGICAL MATCH
C
C	flags include 
C		-b (ball and stick)
C		-h (suppress header records in output)
C		-radius XX (set cylinder radius)
C		-brad XX (set ball radius as fraction of VdW)
C
      character*64 options
      logical      bflag, hflag, bcflag, brflag
      real	   cylrad, ballrad
c
c	Read in 3x3 view matrix from file setup.matrix.  
c	Matrix is applied before finding translation, center, and scale.  
c	Afterwards the input matrix to RENDER is therefore the identity matrix.
C                                                     
	common /matrix/ matrix, coords
	real*4		matrix(3,3), coords(3)
C
C     Default to CPK colors and VDW radii
      character*60 defcol(7)
      data defcol /
     & 'COLOUR#######C################   0.625   0.625   0.625  1.70',
     & 'COLOUR#######N################   0.125   0.125   1.000  1.60',
     & 'COLOUR#######O################   0.750   0.050   0.050  1.50',
     & 'COLOUR#######S################   1.000   1.000   0.025  1.85',
     & 'COLOUR#######H################   1.000   1.000   1.000  1.20',
     & 'COLOUR#######P################   0.400   1.000   0.400  1.80',
     & 'COLOUR########################   1.000   0.000   1.000  2.00'
     &            /
c
    3	format(a,a)
c
	bflag  = .FALSE.
	bcflag = .FALSE.
	hflag  = .FALSE.
	brflag = .FALSE.
	cylrad = 0.2
	ballrad= 0.2
	narg  = iargc()
	i = 1
  500	continue
            call getarg( i, options )
            if (options(1:2) .eq. '-h') hflag = .true.
            if (options(1:5) .eq. '-Bcol') then
                bcflag = .true.
                i = i + 1
                if (i.gt.narg) goto 701
                call getarg( i, options )
                read (options,*,err=701) Bmin
                i = i + 1
                if (i.gt.narg) goto 701
                call getarg( i, options )
                read (options,*,err=701) Bmax
            endif
            if (options(1:2) .eq. '-b') bflag = .true.
	    if (options(1:2) .eq. '-r') then
		i = i + 1
		if (i.gt.narg) goto 701
		call getarg( i, options )
		read (options,*,err=701) cylrad
		if (cylrad.le.0) stop 'illegal radius value'
	    end if
	    if (options(1:3) .eq. '-br') then
		i = i + 1
		if (i.gt.narg) goto 701
		call getarg( i, options )
		read (options,*,err=701) ballrad
		if (ballrad.le.0) stop 'illegal ball radius value'
		bflag = .true.
	    end if
	i = i + 1
	if (i.le.narg) goto 500
c
	goto 799
  701	write (noise,'(A)')
     &	'syntax: rods [-h] [-b] [-Bcolor Bmin Bmax] [-radius R]'
	call exit(-1)
  799	continue
c
      write (noise,*) 'Raster3D rods program V2.5b'
      if (bcflag) then
        write (noise,*) 'Atom colors will be assigned based on Biso'
        write (noise,*) '    from dark blue = Bmin =', Bmin
        write (noise,*) '      to light red = Bmax =', Bmax
      endif
c
C
	do i=1,3
	do j=1,3
	    matrix(i,j)=0.
	enddo
	matrix(i,i)=1.
	enddo
	call view_matrix
c
      if (.not. hflag) then
	WRITE(OUTPUT,'(A)') 'rods V2.5b'
	WRITE(OUTPUT,'(A)') '80  64    tiles in x,y'
	WRITE(OUTPUT,'(A)') ' 8   8    pixels (x,y) per tile'
	WRITE(OUTPUT,'(A)') '4         anti-aliasing'
	WRITE(OUTPUT,'(A)') '0 0 0     black background'
	WRITE(OUTPUT,'(A)') 'F         no, shadowed rods look funny'
	WRITE(OUTPUT,'(A)') '25        Phong power'
	WRITE(OUTPUT,'(A)') '0.15      secondary light contribution'
	WRITE(OUTPUT,'(A)') '0.05      ambient light contribution'
	WRITE(OUTPUT,'(A)') '0.25      specular reflection component'
	WRITE(OUTPUT,'(A)') '4.0       eye position'
	WRITE(OUTPUT,'(A)') '1 1 1     main light source position'
      end if
c
      ASPECT = 1280./1024.
      NCOL = 0
      NATM = 0
10    CONTINUE
        READ(INPUT,'(A80)',END=50) CARD
        IF (CARD(1:4).EQ.'COLO') THEN
          NCOL = NCOL + 1
          IF (NCOL.GT.MAXCOL) THEN
            WRITE(NOISE,*) 'Colour table overflow.  Increase ',
     &                     'MAXCOL and recompile.'
            STOP 10
          ENDIF
          READ(CARD,'(6X,A24,3F8.3,F6.2)') MASK(NCOL),
     &          (RGB(I,NCOL),I=1,3),VDW(NCOL)
        ELSEIF (CARD(1:4).EQ.'ATOM'.OR.CARD(1:4).EQ.'HETA') THEN
          NATM = NATM + 1
          IF (NATM.GT.MAXATM) THEN
            WRITE(NOISE,*) 'Atom array overflow.  Increase ',
     &                     'MAXATM and recompile.'
            STOP 20
          ENDIF
          ATOM(NATM) = CARD
        ELSEIF (CARD(1:3).EQ.'END') THEN
          GO TO 50
        ENDIF
        GO TO 10
*     Come here when EOF or 'END' record is reached
50    CONTINUE
      IF (NATM.EQ.0) THEN
        WRITE(NOISE,*) 'No atoms in input.'
        STOP 30
      ENDIF
*     Load default colors after any that were read in
      IF (NCOL.LT.MAXCOL-8) THEN
        DO i = 1,7
          NCOL = NCOL + 1
          READ(defcol(i),'(6X,A24,3F8.3,F6.2)') MASK(NCOL),
     &          (RGB(J,NCOL),J=1,3), VDW(NCOL)
        ENDDO
      ENDIF
*
      IF (NCOL.EQ.0) THEN
        WRITE(NOISE,*) 'No colours in input.'
        STOP 40
      ENDIF
      XMAX = -1E20
      XMIN =  1E20
      YMAX = -1E20
      YMIN =  1E20
      ZMAX = -1E20
      ZMIN =  1E20
      DO 100 IATM=1,NATM
        CARD = ATOM(IATM)
        TEST = CARD(7:30)
        DO 80 ICOL=1,NCOL
          IF (MATCH(TEST,MASK(ICOL))) THEN
c           READ(CARD,'(30X,3F8.3)') X,Y,Z
c EAM Oct88
            READ(CARD,'(30X,3F8.3,6X,F8.2)') coords, Biso
		x = coords(1)*matrix(1,1) + coords(2)*matrix(2,1) 
     1  	  + coords(3)*matrix(3,1)
		y = coords(1)*matrix(1,2) + coords(2)*matrix(2,2) 
     1  	  + coords(3)*matrix(3,2)
		z = coords(1)*matrix(1,3) + coords(2)*matrix(2,3)
     1  	  + coords(3)*matrix(3,3)
c EAM Oct88
            RAD = VDW(ICOL)
            SPAM(1,IATM) = X
            SPAM(2,IATM) = Y
            SPAM(3,IATM) = Z
            SPAM(4,IATM) = RAD
            SPAM(5,IATM) = ICOL
	    SPAM(6,IATM) = Biso
            XMAX = MAX(XMAX,X+RAD)
            XMIN = MIN(XMIN,X-RAD)
            YMAX = MAX(YMAX,Y+RAD)
            YMIN = MIN(YMIN,Y-RAD)
            ZMAX = MAX(ZMAX,Z+RAD)
            ZMIN = MIN(ZMIN,Z-RAD)
            GO TO 100
          ENDIF
80      CONTINUE
        WRITE(NOISE,*) 'No colour table mask matches this atom:'
        WRITE(NOISE,*) ATOM(IATM)
        STOP 90
100   CONTINUE
      XMID = (XMAX+XMIN)/2.
      YMID = (YMAX+YMIN)/2.
      ZMID = (ZMAX+ZMIN)/2.
      TX = -XMID
      TY = -YMID
      TZ = -ZMID
      IF (ASPECT.GE.1.) THEN
*       The X direction is wider than the Y
        XROOM = ASPECT
        YROOM = 1.
        ZROOM = 2.
      ELSE
        XROOM = 1.
        YROOM = ASPECT
        ZROOM = 2.
      ENDIF
      XSPAN = XMAX-XMIN
      YSPAN = YMAX-YMIN
      ZSPAN = ZMAX-ZMIN
      SCALE = MAX(XSPAN/XROOM,YSPAN/YROOM,ZSPAN/ZROOM)
*     Leave a little extra room as a border:
      SCALE = SCALE / 0.90
c
      if (.not. hflag) then
	WRITE(OUTPUT,120) TX,TY,TZ,SCALE
120	FORMAT('1 0 0 0   input co-ordinate + radius transformation'/
     &       '0 1 0 0'/
     &       '0 0 1 0'/
     &       4F10.3)
	WRITE(OUTPUT,'(A)') '3         mixed object types'
	WRITE(OUTPUT,'(A)') '*'
	WRITE(OUTPUT,'(A)') '*'
	WRITE(OUTPUT,'(A)') '*'
      end if
C
C	Here's the real loop.  
C	Look for pairs closer to each other than 0.60 times the
C	sum of the vanderWaals radii.
C	Draw all cylinders with 0.2A cylindrical radius.
C	For ball and stick pictures, shrink vanderWaals radius
C	of balls by 0.20
C	If two atoms of different colors are bonded, make half-bond
C	cylinders with each color.
C
      CLOSE = 1.6 * 1.6
C
      IF (BFLAG) THEN
      DO 135 IATM=1,NATM
	RAD  = SPAM(4,IATM) * ballrad
	ICOL = SPAM(5,IATM)
	if (bcflag) then
	    call B2RGB( SPAM(6,IATM), Bmin, Bmax, RED, GREEN, BLUE )
	    RED   = RED*RED
	    GREEN = GREEN*GREEN
	    BLUE  = BLUE*BLUE
	else
	    RED   = RGB(1,ICOL)
	    GREEN = RGB(2,ICOL)
	    BLUE  = RGB(3,ICOL)
	endif
	WRITE(OUTPUT,130)
     2         SPAM(1,IATM),SPAM(2,IATM),SPAM(3,IATM),RAD,
     3         RED,GREEN,BLUE
C130	FORMAT(1H2,/,7f8.3)
130	FORMAT(1H2,/,7(1X,F8.3))
135   CONTINUE
      ENDIF
C
      DO 160 IATM=1,NATM
      DO 150 JATM=IATM+1,NATM
	DX = SPAM(1,IATM) - SPAM(1,JATM)
	DY = SPAM(2,IATM) - SPAM(2,JATM)
	DZ = SPAM(3,IATM) - SPAM(3,JATM)
	DIST  = DX*DX + DY*DY + DZ*DZ
	CLOSE = 0.6 * (SPAM(4,IATM) + SPAM(4,JATM)) 
	CLOSE = CLOSE**2
	IF (ATOM(IATM)(17:17).NE.' ' .AND. ATOM(JATM)(17:17).NE.' '
     &     .AND. ATOM(IATM)(17:17).NE.ATOM(JATM)(17:17)) GOTO 150
	IF (DIST .LE. CLOSE) THEN
	  if (bcflag) then
	    ICOL = 1
	    JCOL = 2
	    call B2RGB( SPAM(6,IATM), Bmin, Bmax, RED, GREEN, BLUE )
	    RGB(1,ICOL) = RED*RED
	    RGB(2,ICOL) = GREEN*GREEN
	    RGB(3,ICOL) = BLUE*BLUE
	    call B2RGB( SPAM(6,JATM), Bmin, Bmax, RED, GREEN, BLUE )
	    RGB(1,JCOL) = RED*RED
	    RGB(2,JCOL) = GREEN*GREEN
	    RGB(3,JCOL) = BLUE*BLUE
	  else
	    ICOL = SPAM(5,IATM)
	    JCOL = SPAM(5,JATM)
	  endif
	  IF(ICOL.EQ.JCOL) THEN
	    WRITE(OUTPUT,140)
     1         SPAM(1,IATM),SPAM(2,IATM),SPAM(3,IATM),CYLRAD,
     2         SPAM(1,JATM),SPAM(2,JATM),SPAM(3,JATM),CYLRAD,
     3         RGB(1,ICOL),RGB(2,ICOL),RGB(3,ICOL)
	  ELSE
	    DO 136 K=1,3
136	    CEN(K) = (SPAM(K,IATM)+SPAM(K,JATM))/2
	    WRITE(OUTPUT,140)
     1         SPAM(1,IATM),SPAM(2,IATM),SPAM(3,IATM),CYLRAD,
     2         CEN(1),CEN(2),CEN(3),CYLRAD,
     3         RGB(1,ICOL),RGB(2,ICOL),RGB(3,ICOL)
	    WRITE(OUTPUT,140)
     1         CEN(1),CEN(2),CEN(3),CYLRAD,
     2         SPAM(1,JATM),SPAM(2,JATM),SPAM(3,JATM),CYLRAD,
     3         RGB(1,JCOL),RGB(2,JCOL),RGB(3,JCOL)
	  ENDIF
	ENDIF

C140   FORMAT(1H3,/,11f8.3)
140   FORMAT(1H3,/,2(1X,F8.3,1X,F8.3,1X,F8.3,1X,F7.3),3F7.3)
150   CONTINUE
160   CONTINUE
C
C
C
	write (noise,'(/)')
	write (noise,156) 'X  min max:', XMIN, XMAX
	write (noise,156) 'Y  min max:', YMIN, YMAX
	write (noise,156) 'Z  min max:', ZMIN, ZMAX
	write (noise,156) '     scale:', SCALE
  156	format(1x,a,3f8.2)
      END
      LOGICAL FUNCTION MATCH (SUBJ, MASK)
      CHARACTER*24 SUBJ,MASK
      MATCH = .FALSE.
      DO 10 I = 1, 24
        IF (SUBJ(I:I).NE.MASK(I:I) .AND. MASK(I:I).NE.'#') RETURN
10    CONTINUE
      MATCH = .TRUE.
      RETURN
      END

	subroutine view_matrix
c
	common /matrix/ matrix, coords
	real*4		matrix(3,3), coords(3)
c
	real*4		phiX, phiY, phiZ
	integer    noise
	parameter (noise = 0)
	parameter (R2D = 180./3.1415927)

	open (unit=3, file='setup.matrix', status='OLD', err=100)
		read (3,*) ((matrix(i,j),i=1,3),j=1,3)
		write (noise,'(1x,3f9.5)') ((matrix(i,j),i=1,3),j=1,3)
		close (3)

		det = matrix(1,1) * matrix(2,2) * matrix(3,3)
     1  	    + matrix(1,2) * matrix(2,3) * matrix(3,1)
     2  	    + matrix(2,1) * matrix(3,2) * matrix(1,3)
     3  	    - matrix(1,3) * matrix(2,2) * matrix(3,1)
     4  	    - matrix(1,2) * matrix(2,1) * matrix(3,3)
     5  	    - matrix(1,1) * matrix(2,3) * matrix(3,2)
		write (noise,'(''       determinant ='',f8.3)') det

		phiX = atan2( -matrix(3,2), matrix(3,3) )
		phiY = atan2(  matrix(3,1), matrix(3,3) / cos(phiX) )
		phiZ = atan2( -matrix(2,1), matrix(1,1) )
		write (noise,3) ' View Angles from matrix',' '
		write (noise,2) phiZ*R2D, phiY*R2D, phiX*R2D
		return
  100	continue

	open (unit=3, file='setup.angles', status='OLD', err=200)
		read (3,*) phiZ, phiY, phiX
		close (3)
		write (noise,2) phiZ, phiY, phiX
		cx = cos(phiX/R2D)
		sx = sin(phiX/R2D)
		cy = cos(phiY/R2D)
		sy = sin(phiY/R2D)
		cz = cos(phiZ/R2D)
		sz = sin(phiZ/R2D)
		matrix(1,1) = cz*cy
		matrix(1,2) = sz*cx + cz*sy*sx
		matrix(1,3) = sz*sx - cz*sy*cx
		matrix(2,1) = -sz*cy
		matrix(2,2) = cz*cx - sx*sy*sz
		matrix(2,3) = cz*sx + sz*sy*cx
		matrix(3,1) = sy
		matrix(3,2) = -sx*cy
		matrix(3,3) = cx*cy
		write (noise,3) ' View Matrix from angles',' '
		write (noise,'(1x,3f9.5)') ((matrix(i,j),i=1,3),j=1,3)
		return
  200	continue

    2 	format(1x,'   phiZ =',f8.2,'   phiY =',f8.2,'   phiX =',f8.2)
    3	format(/a,a)

    	write (noise,*) ' No view matrix or angles provided'
	return
	end

CCC     Return RGB triple that runs from dark blue at Bmin
CC      to light red at Bmax
C
	subroutine B2RGB( Biso, Bmin, Bmax, R, G, B )
	real              Biso, Bmin, Bmax, R, G, B
c
	real fraction, h, s, v
c
	fraction = (Biso-Bmin) / (Bmax-Bmin)
	if (fraction.lt.0.) fraction = 0.
	if (fraction.gt.1.) fraction = 1.
        h = 240. * (1.-fraction)
        s = 0.8
        v = 0.5 + fraction/2.
        call hsv2rgb( h, s, v, r, g, b )
        return
	end


CCC	Color format conversion from Hue/Saturation/Value to Red/Green/Blue
CC	minimal (i.e. NO) error checking
C
	subroutine hsv2rgb( h, s, v, r, g, b )
	real                h, s, v, r, g, b
c
	real    f, p, q, t
	integer i
c
	i = h /60.
	f = h/60. - float(i)
	p = v * (1. - s)
	q = v * (1. - s*f)
	t = v * (1. - s*(1. - f))
	if (i.eq.5) then
	    r = v
	    g = p
	    b = q
	else if (i.eq.4) then
	    r = t
	    g = p
	    b = v
	else if (i.eq.3) then
	    r = p
	    g = q
	    b = v
	else if (i.eq.2) then
	    r = p
	    g = v
	    b = t
	else if (i.eq.1) then
	    r = q
	    g = v
	    b = p
	else
	    r = v
	    g = t
	    b = p
	endif
	return 
	end
