 
      PROGRAM BALLS
*
*         Program to set up input for RENDER
*
* This program takes a Brookhaven PDB-format file of atomic coordinates
* concatenated with a "colours" file that specifies the colouring and spherical
* radius of atoms according to atom type, residue type, or any other criterion,
* and produces a file that can be used directly as input to RENDER.
*
* Usage:	cat colors.pdb protein.pdb | balls [-h] | render
*
* auxiliary files:	setup.matrix	optional file containing 3x3 view matrix
*			setup.angles	optional file defining view matrix
*					in terms of 3 rotation angles
*
*     I/O units for colour/co-ordinate input, specs output, user output
      INTEGER INPUT, OUTPUT, NOISE
      PARAMETER (INPUT=5, OUTPUT=6, NOISE=0)
      PARAMETER (MAXCOL=5000, MAXATM=300000)
      REAL RGB(3,MAXCOL),RADIUS(MAXCOL)
      REAL SPAM(7,MAXATM)
      CHARACTER*24 MASK(MAXCOL),TEST
      CHARACTER*80 ATOM(MAXATM),CARD
      LOGICAL MATCH
      logical		hflag
      character*80	flags
c
C	Ethan Merritt	Oct 1988
C	Modified to read in 3x3 view matrix (e.g. from CCP FRODO view command)
C	from file setup.matrix.  Matrix is applied before
C	finding translation, center, and scale.  Afterwards the input matrix
C	to RENDER is therefore the identity matrix.
C                                                     
	common /matrix/ matrix, postrn, coords
	real*4		matrix(3,3), postrn(3), coords(3)
	data		matrix / 1.,0.,0.,0.,1.,0.,0.,0.,1. /
c
    3	format(a,a)
c
	call view_matrix
c
c	Ethan Merritt Apr 1992
c	-h option suppresses header records in output
c
	hflag = .false.
	narg = iargc()
	do i = 1, narg
	    call getarg( i, flags )
	    if (flags(1:2) .eq. '-h') hflag = .true.
	end do
c
      if (.not. hflag) then
	WRITE(OUTPUT,'(A)') 'A colour molecule picture'
	WRITE(OUTPUT,'(A)') '80  64    tiles in x,y'
	WRITE(OUTPUT,'(A)') ' 8   8    pixels (x,y) per tile'
	WRITE(OUTPUT,'(A)') '4         anti-aliasing 3x3 -> 2x2 pixels'
	WRITE(OUTPUT,'(A)') '0 0 0     black background'
	WRITE(OUTPUT,'(A)') 'T         yes, I LIKE shadows!'
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
     &          (RGB(I,NCOL),I=1,3),RADIUS(NCOL)
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
            READ(CARD,'(30X,3F8.3)') coords
		x = coords(1)*matrix(1,1) + coords(2)*matrix(2,1) 
     1  	  + coords(3)*matrix(3,1)
		y = coords(1)*matrix(1,2) + coords(2)*matrix(2,2) 
     1  	  + coords(3)*matrix(3,2)
		z = coords(1)*matrix(1,3) + coords(2)*matrix(2,3)
     1  	  + coords(3)*matrix(3,3)
c EAM Oct88
            RAD = RADIUS(ICOL)
            SPAM(1,IATM) = X
            SPAM(2,IATM) = Y
            SPAM(3,IATM) = Z
            SPAM(4,IATM) = RAD
            SPAM(5,IATM) = RGB(1,ICOL)
            SPAM(6,IATM) = RGB(2,ICOL)
            SPAM(7,IATM) = RGB(3,ICOL)
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
c
      DO 150 IATM=1,NATM
	SPAM(1,IATM) = SPAM(1,IATM) + postrn(1)
	SPAM(2,IATM) = SPAM(2,IATM) + postrn(2)
	SPAM(3,IATM) = SPAM(3,IATM) + postrn(3)
        WRITE(OUTPUT,140) (SPAM(I,IATM),I=1,7)
140   FORMAT(1H2,/,7(1X,F8.3))
150   CONTINUE
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
	common /matrix/ matrix, postrn, coords
	real*4		matrix(3,3), postrn(3), coords(3)
c
	real*4		phiX, phiY, phiZ
	integer    noise
	parameter (noise = 0)
	parameter (R2D = 180./3.1415927)

	postrn(1) = 0.
	postrn(2) = 0.
	postrn(3) = 0.

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
		read (3,*,err=101,end=101) postrn(1), postrn(2), postrn(3)
  101		continue
		close (3)
		write (noise,2) phiZ, phiY, phiX
		write (noise,4) postrn(1), postrn(2), postrn(3)
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
    4 	format(1x,'  tranX =',f8.2,'  tranY =',f8.2,'  tranZ =',f8.2)

    	write (noise,*) ' No view matrix or angles provided'
	return
	end
