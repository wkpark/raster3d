 
*	PROGRAM RIBBON
*
*       Program to set up input for RENDER (RASTER3D package)
*	to draw ribbon diagram.  The RIBBON routine itself is simply
*	extracted from CCP FRODO.  The original invoked a bspline feature
*	of the ps300;  I have replaced this with a spline equation gotten
*	from Larry Andrews.  Conversion from ribbon edges to solid rendering
*	is my own hacking.
*				Ethan Merritt	-  8-Nov-1988
*	Slightly modified code to guarantee output of triangles with
*	vertices in correct order for triangular mesh algorithms EAM Sep 90
*
*	Usage:	ribbon [-h] [-dn] pdbfile > setup.r3d
*		ribbon [-h] -dn -         to take pdb records from stdin
*
*	Input:	pdbfile
*     			Brookhaven PDB-format file of atomic co-ordinates
*			only C-alpha and O atoms are needed
*		setup.matrix or setup.angles
*			rotation matrix or angles applied to PDB coords
*			(see writeup for SETUP/RENDER).
*	Output:	stdout (new for DS5000 version)
*			file suitable for input to RENDER
*
*	Interactive parameters:
*		WIDTH of ribbon in Angstroms
*		NUMBER of interpolated coordinates between successive C-alphas.
*		COLOR scheme for ribbon
*		    0 or 1: solid color (RGB values from color1 below)
*		    2:	    shade from color1 at 1st res to color2 at last res
*		    3:      front of ribbon is color1, back of ribbon is color2
*		    4:      shade front as in scheme 2, back is color 3
*                   5:      each chain is new color (from successive input
*                               (COLOR cards at start of input file)
*		    6:      use prefixed COLOR cards (as in SETUP/RENDER)
*				(implemented 4-Aug-1997 EAM)
*		COLOR1,COLOR2,COLOR3	RGB components (9f8.0)
*
      INCLUDE 'VERSION.incl'
c                                             
      INTEGER INPUT, OUTPUT, NOISE
      PARAMETER (OUTPUT=6, NOISE=0)
      PARAMETER (MAXCOL=5000, MAXATM=10000)
C     REAL RGB(3,MAXCOL)
      REAL RADIUS(MAXCOL)
      CHARACTER*24 MASK(MAXCOL),TEST
      CHARACTER*80 ATOM(MAXATM),CARD
      LOGICAL SMATCH
c
C	Ethan Merritt	Oct 1988
C	Modified to read in 3x3 view matrix (e.g. from CCP FRODO view command)
C	from file.  Matrix is applied before
C	finding translation, center, and scale.  Afterwards the input matrix
C	to RENDER is therefore the identity matrix.
C EAM Aug 1997 - Honor COLOUR requests
C EAM Nov 1999 - remove all (q) formats
C                                                     
c
	common /COLORS/ ischeme, cindex, COLOR1(3), COLOR2(3), COLOR3(3)
     &  		,RGB(3,MAXCOL)
 	integer		cindex
	common /SPAM/   natm, SPAM(4,MAXATM), SCAM(MAXATM)
	integer		SCAM
	common /FLAGS/  mflag, hflag, dflag
	logical		mflag, hflag, dflag
c
	character*64	in_file, out_file
	character*8	mode
	character*32	flags
	character*80	line
	common /matrix/ matrix, coords
	real		matrix(3,3), coords(3)
	data		matrix / 1.,0.,0.,0.,1.,0.,0.,0.,1. /
c
c	-h causes the header records not to be printed
c	-m [now obsolete because always in force] uses format
c	   mixed object types in output file
c	-d suppresses interactive input
c
	hflag = .FALSE.
	dflag = .FALSE.
	mflag = .TRUE.
c
	narg  = iargc()
	do i = 1,narg
	    call getarg( i, flags )
	    if (flags(1:2) .eq. '-h') then
	    	hflag = .TRUE.
	    else if (flags(1:2) .eq. '-d') then
	    	dflag = .TRUE.
		read (flags(3:4),'(I1)') ischeme
	    end if
	end do
c
	call getarg( narg, in_file )
	if (in_file(1:1) .eq. '-') then
	    INPUT = 5
	else
	    INPUT = 1
	    open( unit=INPUT, file=in_file, status='OLD' )
	end if
c
    3	format(a,a)
c
	call view_matrix
c
	NCOL = 0
	NATM = 0
	ASPECT = 1280./1024.
c
	if (hflag) goto 10
c
      WRITE(OUTPUT,'(A,A)') 'C-alpha ribbon - Raster3D ',VERSION
      WRITE(OUTPUT,'(A)') '80 64     tiles in x,y'
      WRITE(OUTPUT,'(A)') ' 8  8     pixels (x,y) per tile'
      WRITE(OUTPUT,'(A)') '4         anti-aliasing 3x3 into 2x2 pixels'
      WRITE(OUTPUT,'(A)') '0 0 0     black background'
      WRITE(OUTPUT,'(A)') 'F         no, ribbons cast funny shadows'
      WRITE(OUTPUT,'(A)') '25        Phong power'
      WRITE(OUTPUT,'(A)') '0.15      secondary light contribution'
      WRITE(OUTPUT,'(A)') '0.05      ambient light contribution'
      WRITE(OUTPUT,'(A)') '0.25      specular reflection component'
      WRITE(OUTPUT,'(A)') '4.0       eye position'
      WRITE(OUTPUT,'(A)') '1 1 1     main light source position'
c
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
        ELSEIF ((CARD(1:4).EQ.'ATOM') .AND.
     &         ( CARD(14:16).EQ.'CA ' .OR. CARD(14:16).EQ.'O  ')) THEN
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
      ELSE
	WRITE(NOISE,*) NATM,' atoms accepted from input.'
      ENDIF
      IF (NCOL.EQ.0) THEN
        WRITE(NOISE,*) 'No colours in input.'
c       STOP 40
      ENDIF
C
      XMAX = -1E20
      XMIN =  1E20
      YMAX = -1E20
      YMIN =  1E20
      ZMAX = -1E20
      ZMIN =  1E20

      DO 100 IATM=1,NATM
        CARD = ATOM(IATM)
        TEST = CARD(7:30)
            READ(CARD,82) coords
   82	    format(30x,3f8.3)
		x = coords(1)*matrix(1,1) + coords(2)*matrix(2,1) 
     1  	  + coords(3)*matrix(3,1)
		y = coords(1)*matrix(1,2) + coords(2)*matrix(2,2) 
     1  	  + coords(3)*matrix(3,2)
		z = coords(1)*matrix(1,3) + coords(2)*matrix(2,3)
     1  	  + coords(3)*matrix(3,3)
            RAD = RADIUS(ICOL)
            SPAM(1,IATM) = X
            SPAM(2,IATM) = Y
            SPAM(3,IATM) = Z
            SPAM(4,IATM) = RAD
C
C	    EAM Aug 1997 - finally get around to honoring atom colors
	    DO 84 ICOL = 1, NCOL
		IF (SMATCH(TEST,MASK(ICOL))) THEN
		    SCAM(IATM) = ICOL
		    GOTO 86
		ENDIF
   84	    CONTINUE
   86	    CONTINUE
C
            XMAX = MAX(XMAX,X+RAD)
            XMIN = MIN(XMIN,X-RAD)
            YMAX = MAX(YMAX,Y+RAD)
            YMIN = MIN(YMIN,Y-RAD)
            ZMAX = MAX(ZMAX,Z+RAD)
            ZMIN = MIN(ZMIN,Z-RAD)
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
      if (hflag) goto 129
      WRITE(OUTPUT,120) TX,TY,TZ,SCALE
120   FORMAT('1 0 0 0   input co-ordinate + radius transformation'/
     &       '0 1 0 0'/
     &       '0 0 1 0'/
     &       4F10.3)
      if (mflag) then
	WRITE (OUTPUT,'(A)') '3         mixed object types'
	WRITE (OUTPUT,'(A)') '(9F8.3,2x,3f5.2)'
	WRITE (OUTPUT,'(A)') '(11F8.3)'
	WRITE (OUTPUT,'(A)') '(11F8.3)'
      else
	WRITE (OUTPUT,'(A)') '1         all objects are triangles'
	WRITE (OUTPUT,'(A)') '(9F8.3,2x,3f5.2)'
      end if
  129	continue
	write (noise,'(/)')
	write (noise,153) 'X  min max:', XMIN, XMAX
	write (noise,153) 'Y  min max:', YMIN, YMAX
	write (noise,153) 'Z  min max:', ZMIN, ZMAX
	write (noise,153) '     scale:', SCALE
  153	format(1x,a,3f8.2)
c
c
	if (dflag) then
	    width = 1.5
	    offset = 1.2
	    nchord = 5
	    if (ischeme .le. 0 .or. ischeme .gt. 6) ischeme = 2
	     call vload( color1, 0.0, 0.0, 0.4 )
	     call vload( color2, 0.5, 0.0, 0.0 )
	     call vload( color3, 0.6, 0.6, 0.6 )
	else
	width = 0
	write (noise,3) 'Width of ribbon (default 1.5A): '
	read  (5,'(A80)') line
	read  (line,*,end=154,err=154) width
  154	continue
	if (width.le.0) width = 1.5
c	Original RIBBON used bspline smoothing, which requires "offset"
c	because smoothed curve doesn't go through guide points.  
	write (noise,3) 'Offset from CA position (default 1.2A): '
	read  (5,'(A80)') line
	read  (line,*,end=156,err=156) offset
  156	continue
	if (offset.le.0) offset = 1.2
	write (noise,3) 'Chords per residue (default = 10): '
	read  (5,'(A80)') line
	read  (line,*,end=158,err=158) nchord
  158	continue
	if (nchord.le.1) nchord = 10
  159	continue
	write (noise,160)
  160	format(' Coloring schemes available:',
     1	/,' 0 or 1: solid color (RGB values from color1 below)',
     2	/,'      2: shade from color1 at 1st res to color2 at last res',
     3	/,'      3: front of ribbon is color1, back of ribbon is color2',
     4	/,'      4: shade front as in scheme 2, back is color 3',
     5	/,'      5: new color for each chain (requires COLOUR cards)')
	write (noise,3) 'Coloring scheme: '
	read  (5,'(A80)') line
	read  (line,*,end=161,err=161) ischeme
  161	continue
	if (ischeme.le.0 .or. ischeme.gt.6) ischeme = 1
	if (ischeme .eq. 1) write (noise,3)
     1      'COLOR1 (RGB values, 3f8.0): '
	if (ischeme .eq. 2) write (noise,3)
     1      'COLOR1, COLOR2 (RGB values, 6f8.0): '
	if (ischeme .eq. 3) write (noise,3)
     1      'COLOR1, COLOR2 (RGB values, 6f8.0): '
	if (ischeme .eq. 4) write (noise,3) 
     1      'COLOR1, COLOR2, COLOR3 (RGB values, 9f8.0): '
	if (ischeme .lt. 5) then
	    read  (5,'(A80)') line
	    if (line.eq.' ') goto 163
	    read  (line,*,end=163,err=163) color1,color2,color3
        endif
	goto 164
  163	continue
	     call vload( color1, 0.0, 0.0, 0.4 )
	     call vload( color2, 0.5, 0.0, 0.0 )
	     call vload( color3, 0.6, 0.6, 0.6 )
  164	continue
	if (ischeme .eq. 3) then
		color3(1) = color2(1)
		color3(2) = color2(2)
		color3(3) = color2(3)
	end if
c	end of -d suppression
	end if
	write (noise,169) ischeme,color1,color2,color3
  169	format(' color scheme',i3,/,3(3x,3f6.3))
	cindex = 1
c
	call ribbon( 2, width, nchord, offset, natm )
c
      END
      LOGICAL FUNCTION SMATCH (SUBJ, MASK)
      CHARACTER*24 SUBJ,MASK
      SMATCH = .FALSE.
      DO 10 I = 1, 24
        IF (SUBJ(I:I).NE.MASK(I:I) .AND. MASK(I:I).NE.'#') RETURN
10    CONTINUE
      SMATCH = .TRUE.
      RETURN
      END

	subroutine view_matrix
c
	common /matrix/ matrix, coords
	real		matrix(3,3), coords(3)
c
	real		phiX, phiY, phiZ
	parameter (noise = 0)
	parameter (R2D = 180./3.1415927)

	open (unit=3, file='setup.matrix', status='OLD', err=100)
		write (noise,3) ' View Matrix from file '
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
		write (noise,3) ' View Angles from file '
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


C
	SUBROUTINE RIBDRW(GUIDE,NRIB,MAXRES,NPT,NCHORD)
	integer	npt			! number of guide points
	real	guide(4,MAXRES,NRIB)    ! 4 dim because E&S wanted it that way
	integer	nchord			! how many interpolations per guide pt
	parameter (MAXCOL = 5000)
	integer	OUTPUT
	parameter (OUTPUT = 6)
C                               
C	splining from Larry Andrews 7-Nov-1988
C
	parameter (nspln = 5000)	! maximum of (npt*nchord)
	parameter (ndata =  500)	! maximum # guidepoints 
	parameter (ndata1 = 501)
c
	common /COLORS/ ischeme, cindex, COLOR1(3), COLOR2(3), COLOR3(3)
     &			,RGB(3,MAXCOL)
 	integer		cindex
	common /FLAGS/  mflag, hflag, dflag
	logical		mflag, hflag, dflag
c
	real	s(ndata1)
	REAL	XP(4,NDATA)
	REAL	TEMP1(NDATA), TEMP2(NDATA)
	real	smooth(4,nspln,2)	! npt*nchord points on splined curve
	real	color(3)
c
	if (npt .gt. ndata) stop 'spline - TOO MANY GUIDE POINTS'
	if (npt*nchord .gt. nspln) stop 'spline - NPT*NCHORD > 5000'
c
c	fill 4th coord with fraction of chain traced
c
	color_inc = 1.000 / float(npt)
	fraction  = 0.0
	if (ischeme.le.5) then
	    do i = 1, npt
		guide(4,i,1) = fraction
		guide(4,i,2) = fraction
		fraction = fraction + color_inc
	    end do
	endif
c
c	calculate spline segments
c
	tinc = 1./float(nchord)
	do 1000 irib = 1, 2
	iout = 1
	do  900  ipt = 2, npt-1
	t = 0.0
	do i = 1, nchord
	    iout = iout + 1
	    call bspline( guide(1,ipt-1,irib), guide(1,ipt,irib),
     1     		  guide(1,ipt+1,irib), t, smooth(1,iout,irib) )
	    t = t + tinc
	end do
  900	continue
 1000	continue
c
c	Add end segments (splines go midpoint-to-midpoint)
c
	iout = iout + 1
	do 1100 irib = 1, 2
	    do i = 1, 4
		smooth(i, 1,    irib) = guide(i, 1,   irib )
	        smooth(i, iout, irib) = guide(i, npt, irib )
	    end do
 1100	continue
C

	if (mflag) then
		assign 3 to iformat
	else
		assign 2 to iformat
	end if
    2	format(9f8.3,2x,3f5.2)
    3	format('1',/,9f8.3,2x,3f5.2)
c
c	Start loop over spline segments
c
	ires = 1
	jres = 1
	kres = 2
 2000	continue
c	do 2100 ires = 1, iout-1
		fraction =   smooth(4,ires,  1)
c
c	Make sure the two sides of the ribbon stay in register
c
		inext = ires + 1
   55		dist0 = dist(smooth(1,inext,1),smooth(1,kres,2))
		dist1 = dist(smooth(1,inext,1),smooth(1,kres+1,2))
		if ((dist1 .lt. dist0) .and. (kres .lt. iout)) then
			kres = kres + 1
			goto 55
		end if
   56		dist0 = dist(smooth(1,inext,1),smooth(1,kres,2))
		dist1 = dist(smooth(1,inext+1,1),smooth(1,kres,2))
		if ((dist1 .lt. dist0) .and. (inext .lt. iout)) then
			inext = inext + 1
			goto 56
		end if
c
		call colorit( color, fraction,
     1  	  smooth(1,ires,1), smooth(1,jres,2), smooth(1,inext,1))
c
		write (output,iformat) (smooth(i,ires,  1),i=1,3),
     1  		          (smooth(i,jres,  2),i=1,3),
     2  		          (smooth(i,inext,1),i=1,3),
     3  		          color
c
		if (jres .eq. kres) goto 2100
		call colorit( color, fraction,
     1  	  smooth(1,kres,2), smooth(1,inext,1), smooth(1,jres,2))
		write (output,iformat) (smooth(i,jres,  2),i=1,3),
     1  		          (smooth(i,inext,1),i=1,3),
     2  		          (smooth(i,kres,  2),i=1,3),
     3  		          color
		jres = kres
		if (kres .lt. iout) kres = kres + 1
 2100	continue
	ires = inext
	if (ires .lt. iout) goto 2000
c
c	End loop over spline segments
c
	cindex = cindex + 1
	return
	end


 	function dist(v1, v2)
	real diff(3)
	call vdif(diff,v1,v2)
	dist = dot(diff,diff)
	return
	end

	subroutine vload( v, s1, s2, s3 )
	real v(3)
	v(1) = s1
	v(2) = s2
	v(3) = s3
	return
	end

	subroutine colorit( color, fraction, point1, point2, point3 )
	real	color(3), point1(3), point2(3), point3(3)
c
c	   scheme 1	solid color (COLOR1)
c	   scheme 2	shade from COLOR1 at 1st residue to COLOR2 at last
c	   scheme 3	COLOR1 on front, COLOR3 (=COLOR2) on back
c	   scheme 4	combination of 2 and 3 above
c	   scheme 5	color each new chain a new color from RGB 
c
        PARAMETER (MAXCOL=5000, MAXATM=10000)
	common /COLORS/ ischeme, cindex, COLOR1(3), COLOR2(3), COLOR3(3)
     &			,RGB(3,MAXCOL)
 	integer		cindex
	common /SPAM/ NATM, SPAM(4,MAXATM), SCAM(MAXATM)
	integer SCAM
	real	vec1(3), vec2(3), vec3(3)
c
	if ((ischeme .eq. 3) .or. (ischeme .eq. 4)) then 
		call vdif( vec1, point2, point1 )
		call vdif( vec2, point3, point1 )
		call cross( vec1, vec2, vec3 )
		if (vec3(3) .lt. 0) then
			color(1) = color3(1)
			color(2) = color3(2)
			color(3) = color3(3)
		else if (ischeme .eq. 4) then
			color(1) = fraction*color2(1) 
     &				 + (1.-fraction)*color1(1)
	 		color(2) = fraction*color2(2) 
     &				 + (1.-fraction)*color1(2)
			color(3) = fraction*color2(3) 
     &				 + (1.-fraction)*color1(3)
		else
			color(1) = color1(1)
			color(2) = color1(2)
			color(3) = color1(3)
		end if
	else if (ischeme .eq. 2) then
		color(1) = fraction*color2(1) + (1.-fraction)*color1(1)
		color(2) = fraction*color2(2) + (1.-fraction)*color1(2)
		color(3) = fraction*color2(3) + (1.-fraction)*color1(3)
	else if (ischeme .eq. 5) then
		call vload( color, 
     &			    RGB(1,cindex), RGB(2,cindex), RGB(3,cindex))
c	else if (ischeme .eq. 6) then
c		ICOL = SCAM(fraction)
c		color(1) = RGB(1,icol)
c		color(2) = RGB(2,icol)
c		color(3) = RGB(3,icol)
	else
		call vload( color, color1(1), color1(2), color1(3) )
	end if
	return
	end

	subroutine bspline( v1, v2, v3, t, v4 )
	real v1(4), v2(4), v3(4)
	real t
	real v4(4)
c
	frac3 = 0.5 * t*t
	frac1 = 0.5 * (1.-t) * (1.-t)
	frac2 = 1. - (frac1 + frac3)
	do i = 1, 4
		v4(i) = frac1 * v1(i) + frac2 * v2(i) + frac3 * v3(i)
	end do
	return
	end
