      PROGRAM RASTEP
********************************************************************************
*
* Usage: 
*    rastep [-h] [-iso] [-Bcolor Bmin Bmax] [-prob xx] [-radius r] [-fancy[0-9]]
*           [-tabulate histogram.file] [-by_atomtype]
*
*
*	-h		suppresses header records in output
*	-iso		forces isotropic B values (spheres rather than
*			ellipsoids) even if ANISOU cards present
*	-Bcolor Bmin Bmax 
*			color by Biso values; Bmin = dark blue, Bmax = light red
*	-prob xx	draws ellipsoids to enclose this 
*			probability level (default = 0.50)
*	-radius		draws bonds with this radius in Angstroms
*			(default = 0.10)
*	-fancy[0-9]	increasingly complex rendition of ellipsoids
*			fancy0  [default] solid surface only
*			fancy1  principle axes + transparent bounding ellipsoid
*			fancy2	equatorial planes only
*			fancy3  equatorial planes + transparent ellipsoid
*			fancy4  longest principle axis only
*	-tabulate [file]instead of creating a Raster3D input file, 
*			list all atoms with principle axes and anisotropy.
*			Optionally write a histogram of anisotropy to speficied 
*			output file; otherwise output is to stderr 
*
* The following options are under development
*	-com [file]	find <anisotropy> in shells from center of mass
*
********************************************************************************
*
* EAM Jul 97	- initial version
* EAM Dec 97	- version 2.4b release
* EAM Jan 98	- add tabulation option
* EAM May 98	- integrate with PARVATI script
* 
*     I/O units for colour/co-ordinate input, specs output, user output
      INTEGER INPUT, OUTPUT, NOISE
      PARAMETER (INPUT=5, OUTPUT=6, NOISE=0)
      PARAMETER (MAXCOL=5000, MAXATM=300000)
      REAL RGB(3,MAXCOL), VDW(MAXCOL)
      REAL SPAM(5,MAXATM)
      real center(3)
      CHARACTER*24 MASK(MAXCOL),TEST
      CHARACTER*80 ATOM(MAXATM),CARD
      character*3  resname
      LOGICAL MATCH
      logical		hflag, ellipses, bcflag, tflag, atflag, comflag
      integer           fancy
      character*80	flags
c
      COMMON /ASSCOM/ assout, verbose
      integer         assout
      logical         verbose
c
      real	quadric(10), anisou(6)
      real      eigens(4), evecs(4,4), evecinv(4,4), evecit(4,4)
      real      qq(4,4), qp(4,4), temp(4,4)
c
      external	anitoquad
      integer	anitoquad
c
      real	problevel(50)
c
      real start(3),end(3)
      real MARGIN
      parameter (MARGIN = 1.15)
c
      real	Uprin(3), Umean, Usigma, anisotropy, ellipticity
      integer	histogram(20), hislun
      real	anisi(MAXATM), sum_a, sum_a2, anis_mean, anis_sigma
      real	sum_b, sum_b2, sum_ab
      integer	nanis, niso
c
      character*2 atomtype
      integer     natype
c
      real*8	wsum, xsum, ysum, zsum
      real*8	xcom, ycom, zcom
      real	adist(110)
      integer	hdist(100), comlun, dshells
c
c     Default to CPK colors and VDW radii
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
c     Critical values for probability ellipsoids of a trivariate normal
c     distribution. From Table 6.1 of ORTEP-III manual (Oak Ridge National
c     Laboratory Report ORNL-6895, 1996). Tabulated below in increments of
c     2% in probability.  Default contours enclose a probability level of
c     50% (critical value 1.5382).
c
	data	problevel /     0.4299, 0.5479, 0.6334, 0.7035, 0.7644, 
     &				0.8192, 0.8694, 0.9162, 0.9605, 1.0026,
     &				1.0430, 1.0821, 1.1200, 1.1570, 1.1932,
     &				1.2288, 1.2638, 1.2985, 1.3330, 1.3672,
     &				1.4013, 1.4354, 1.4695, 1.5037, 1.5382,
     &				1.5729, 1.6080, 1.6436, 1.6797, 1.7164,
     &				1.7540, 1.7924, 1.8318, 1.8724, 1.9144,
     &				1.9580, 2.0034, 2.0510, 2.1012, 2.1544,
     &				2.2114, 2.2730, 2.3404, 2.4153, 2.5003,
     &				2.5997, 2.7216, 2.8829, 3.1365, 6.0000 /
c
c
	assout   = noise
	verbose  = .false.
	hflag    = .false.
	bcflag   = .false.
	tflag    = .false.
	atflag   = .false.
	comflag  = .false.
	ellipses = .true.
	fancy    = 0
	prob     = 0.50
	radius   = 0.10
	narg = iargc()
	i = 1
    5	continue
	    call getarg( i, flags )
	    if (flags(1:5) .eq. '-help') goto 701
	    if (flags(1:6) .eq. '-debug') verbose = .true.
	    if (flags(1:2) .eq. '-h') hflag = .true.
	    if (flags(1:4) .eq. '-iso') ellipses = .false.
	    if (flags(1:4) .eq. '-rad') then
		i = i + 1
		if (i.gt.narg) goto 701
		call getarg( i, flags )
		read (flags,*,err=701) radius
		if (radius.lt.0) radius = 0.0
	    end if
	    if (flags(1:4) .eq. '-pro') then
		i = i + 1
		if (i.gt.narg) goto 701
		call getarg( i, flags )
		read (flags,*,err=701) prob
		if (prob.le.0.) stop 'illegal probability level'
*		If prob > 1 assume they meant it in percent
		if (prob.gt.1.) prob = prob / 100.
	    end if
	    if (flags(1:5) .eq. '-Bcol') then
		bcflag = .true.
		i = i + 1
		if (i.gt.narg) goto 701
		call getarg( i, flags )
		read (flags,*,err=701) Bmin
		i = i + 1
		if (i.gt.narg) goto 701
		call getarg( i, flags )
		read (flags,*,err=701) Bmax
	    endif
	    if (flags(1:6) .eq. '-fancy') then
		if (flags(7:7).eq.'0') then
		    fancy = 0
		else if (flags(7:7).eq.'1') then
		    fancy = 1
		else if (flags(7:7).eq.'2') then
		    fancy = 2
		else if (flags(7:7).eq.'3') then
		    fancy = 3
		else if (flags(7:7).eq.'4') then
		    fancy = 4
		else
		    fancy = 1
		endif
	    endif
	    if (flags(1:4) .eq. '-tab') then
	    	tflag  = .true.
	    	hflag  = .true.
		bcflag = .false.
		fancy  = 0
		do j=1,MAXATM
		    anisi(j) = 0.0
		end do
		hislun = NOISE
		if (i.ge.narg) goto 799
		call getarg(i+1,flags)
		if (flags(1:1) .ne. '-') then
		    hislun = 1
		    open(unit=hislun,file=flags,status='UNKNOWN'
     &			, CARRIAGECONTROL='LIST'
     &              )
		endif
	    endif
	    if (flags(1:4) .eq. '-com') then
		comflag = .true.
		if (i.ge.narg) goto 799
		call getarg(i+1,flags)
		comlun = NOISE
		call getarg(i+1,flags)
		if (flags(1:1) .ne. '-') then
		    comlun = 2
		    open(unit=comlun,file=flags,status='UNKNOWN'
     &			, CARRIAGECONTROL='LIST'
     &              )
		endif
	    endif
	    if (flags(1:8) .eq. '-by_atom') then
		atflag = .true. 
	    endif
c
	i = i + 1
	if (i.le.narg) goto 5
	goto 799
  701	write (noise,'(A)')
     &	'syntax: rastep [-h] [-iso] [-Bcolor Bmin Bmax] [-prob Plevel]'
	write (noise,'(A)')
     &  ' [-fancy[0-3]] [-radius R]'
	write (noise,'(A,A)')
     &  ' [-tabulate [tabfile]] [-by_atomtype]'
	call exit(-1)
  799	continue

c
c Critical values for the radius corresponding to a sphere
c enclosing the requested probability level are taken from
c Table 6.1 of the ORTEP manual
	iprob = (prob+0.01)*50.
	pradius = problevel(iprob)

c
	write (noise,800)
	write (noise,*) 'Raster3D Thermal Ellipsoid Program ',
     &                  'V2.4i'
	write (noise,*) 'E A Merritt - 15 Oct 1998'
	write (noise,800)
  800	format('************************************************')
c
	if (.not.ellipses) then
	  write (noise,801) float(iprob)/50.
  801	  format(' Spheres will bound Biso probability level', f5.2)
	else
	  write (noise,802) float(iprob)/50.
  802	  format(' Ellipsoids will bound probability level', f5.2)
	endif
	write (noise,803) pradius
  803	format(' Corresponding critical value           ', f7.4)
c
      if (bcflag) then
	write (noise,*) 'Atom colors will be assigned based on Biso'
	write (noise,*) '    from dark blue = Bmin =', Bmin
	write (noise,*) '      to light red = Bmax =', Bmax
	Umin = Bmin / (8. * 3.14159*3.14159)
	Umax = Bmax / (8. * 3.14159*3.14159)
	Umin = Umin * pradius
	Umax = Umax * pradius
      endif
c
      if (.not. hflag) then
	WRITE(OUTPUT,'(A,I5,A)') 
     &     'Raster3D thermal ellipsoid program V2.4i',
     &     INT(prob*100.+0.5), '% probability bounds'
	WRITE(OUTPUT,'(A)') '80  64    tiles in x,y'
	WRITE(OUTPUT,'(A)') ' 8   8    pixels (x,y) per tile'
	WRITE(OUTPUT,'(A)') '4         3x3 virtual pixels -> 2x2 pixels'
	WRITE(OUTPUT,'(A)') '1 1 1     white background'
	WRITE(OUTPUT,'(A)') 'F         no, shadows are dorky'
	WRITE(OUTPUT,'(A)') '25        Phong power'
	WRITE(OUTPUT,'(A)') '0.15      secondary light contribution'
	WRITE(OUTPUT,'(A)') '0.05      ambient light contribution'
	WRITE(OUTPUT,'(A)') '0.25      specular reflection component'
	WRITE(OUTPUT,'(A)') '0.0       No perspective'
	WRITE(OUTPUT,'(A)') '1 1 1     main light source position'
      end if
c
	ASPECT = 1280./1024.
	NCOL = 0
	NATM = 0
	NANI = 0
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
     &          (RGB(I,NCOL),I=1,3), VDW(NCOL)
	ELSEIF (CARD(1:6).EQ.'ANISOU') THEN
	  NATM = NATM + 1
	  NANI = NANI + 1
	  ATOM(NATM) = CARD
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
	IF (CARD(1:4).NE.'ATOM' .AND. CARD(1:4).NE.'HETA') 
     &     GOTO 100
c
c	Do a little pre-processing to make later bookkeeping easier
c	At least screen out non-conformant PDB files that contain
c	something obviously not an element type in columns 77:78
     	if (atflag) then
	    resname = card(18:20)
c	    if (resname.eq.'MET' .and. card(14:15).eq.'SD') then
c	    	atom(iatm)(77:78) = 'SD'
c	    end if
	    if ( resname.eq.'HOH' .or. resname.eq.'H2O'
     &	    .or. resname.eq.'WAT') then
     		atom(iatm)(77:78) = 'OW'
	    end if
	    if (atom(iatm)(78:78).ge.'0' .and. atom(iatm)(78:78).le.'9')
     &		atom(iatm)(77:78) = '  '
	end if
c
        TEST = CARD(7:30)
        DO 80 ICOL=1,NCOL
          IF (MATCH(TEST,MASK(ICOL))) THEN
            READ(CARD,'(30X,3F8.3,6X,F8.2)') X,Y,Z, BISO
            RAD = BISO / (8. * 3.14159*3.14159)
	    RAD = RAD * PRADIUS
            SPAM(1,IATM) = X
            SPAM(2,IATM) = Y
            SPAM(3,IATM) = Z
	    SPAM(4,IATM) = RAD
            SPAM(5,IATM) = ICOL
            XMAX = MAX(XMAX,X+RAD)
            XMIN = MIN(XMIN,X-RAD)
            YMAX = MAX(YMAX,Y+RAD)
            YMIN = MIN(YMIN,Y-RAD)
            ZMAX = MAX(ZMAX,Z+RAD)
            ZMIN = MIN(ZMIN,Z-RAD)
c	    atomtype = CARD(77:78)
c	    if (atomtype.ne.'  ') then
c		weight = amass(atomtype)
c	    else
		weight = 13.4
	    wsum = wsum + weight
	    xsum = xsum + weight * X
	    ysum = ysum + weight * Y
	    zsum = zsum + weight * Z
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
      xcom = xsum / wsum
      ycom = ysum / wsum
      zcom = zsum / wsum
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
*     
*     These are for the center-of-mass table
      DMAX  = MAX( ABS(XMAX-XCOM), ABS(XMIN-XCOM) )**2
     &      + MAX( ABS(YMAX-YCOM), ABS(YMIN-YCOM) )**2
     &      + MAX( ABS(ZMAX-ZCOM), ABS(ZMIN-ZCOM) )**2
      DMAX  = SQRT(DMAX)
      dshells = 100
*
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
      if (.not. tflag) then
	WRITE(OUTPUT,'(A)') 
     &	      '# Thermal ellipsoids from Rastep Version 2.4i'
	WRITE(OUTPUT,'(A,F5.2)') '# Probability level',float(iprob)/50.
      end if
c
c Write ellipsoids to input file for render
c
      IF (fancy.eq.0 .and. .not.tflag) GOTO 139
c
c First, optional pass, to write fancy stuff associated with ellipsoids
c
      IATM = 1
  130 CONTINUE
      IF (ATOM(IATM)(1:4).EQ.'ATOM' .OR.
     &    ATOM(IATM)(1:4).EQ.'HETA') THEN
	X = SPAM(1,IATM)
	Y = SPAM(2,IATM)
	Z = SPAM(3,IATM)
	ICOL = SPAM(5,IATM)
	if (bcflag) then
	    call U2RGB( SPAM(4,IATM), Umin, Umax, RED, GREEN, BLUE )
	    RED   = RED*RED
	    GREEN = GREEN*GREEN
	    BLUE  = BLUE*BLUE
	else
	    RED   = RGB(1,ICOL)
	    GREEN = RGB(2,ICOL)
	    BLUE  = RGB(3,ICOL)
	endif
	IF (ellipses .and.(ATOM(IATM+1)(1:6).EQ.'ANISOU')) THEN
	    read (atom(iatm+1)(29:70),*) (anisou(i),i=1,6)
	    do i=1,6
		anisou(i) = anisou(i) * 0.0001
	    enddo
	    if (anitoquad(anisou,pradius,quadric,eigens,evecs).lt.0)then
	        write(noise,*) '*** Non-positive definite ellipsoid - ',
     &				atom(iatm+1)(13:27)
		goto 138
	    endif
	    radlim = pradius * max( eigens(1),eigens(2),eigens(3) )
	    radlim = radlim * MARGIN
c
c	Only for debugging ellipsoids
	    if (verbose) then
	  	write (noise,901) 'ANISOU ',X,Y,Z,ANISOU
	  	write (noise,902) 'QUADRIC',QUADRIC
	  	write (noise,903) 'Eigenvalues', (EIGENS(i),i=1,3),
     &                 'prob', prob,'limiting radius', radlim
     		write (noise,904) 'Evecs ',((evecs(i,j),i=1,3),j=1,3)
	    endif
901	    format(a,3f8.3,6f8.4)
902	    format(a,10f8.3)
903	    format(a,3f8.3,4x,a,f8.3,4x,a,f8.3)
904	    format(a,9f7.3)
c
c	Tabulate principal axes of ellipsoid for each atom
	  if (tflag) then
	    do i=1,3
	    	Uprin(i) = eigens(i)**2
	    enddo
	    if (Uprin(2).gt.Uprin(1)) then
	    	Umean    = Uprin(1)
		Uprin(1) = Uprin(2)
		Uprin(2) = Umean
	    endif
	    if (Uprin(3).gt.Uprin(1)) then
	    	Umean    = Uprin(1)
		Uprin(1) = Uprin(3)
		Uprin(3) = Umean
	    endif
	    if (Uprin(3).gt.Uprin(2)) then
	    	Umean    = Uprin(2)
		Uprin(2) = Uprin(3)
		Uprin(3) = Umean
	    endif
c
c	  Anisotropy we define as Umin / Umax
c	  as in shelxpro output
	    anisotropy  = min(Uprin(1),Uprin(2),Uprin(3))
     &                  / max(Uprin(1),Uprin(2),Uprin(3))
c
c	  But don't count atoms which are perfectly isotropic
	    if (anisotropy .eq. 1.0) then
	    	niso  = niso + 1
	    else
	    	anisi(iatm) = anisotropy
	    	sum_A = sum_A + anisotropy
		sum_B = sum_B + SPAM(4,iatm)
	    	nanis = nanis + 1
	    end if
c
c	  Ellipticity we define as 1 / anisotropy
	    if (anisotropy.eq.0) then
		ellipticity = 0
	    else
		ellipticity = 1. / anisotropy
	    end if
c
c	  Longhi et al (1997) JMB 268, 779-799.
c	  proposed another measure A = sigU / meanU
	    Umean  = (Uprin(1) + Uprin(2) + Uprin(3)) / 3.0
	    Usigma = (Uprin(1)-Umean)**2 
     &		   + (Uprin(2)-Umean)**2 + (Uprin(3)-Umean)**2
	    Usigma = sqrt( Usigma ) / 3.0
	    alonghi = Usigma / Umean
c
c	  Might want to check correlation with Uiso
	    Uiso = SPAM(4,iatm)
c
c	  Cosmetic changes to atom identifier for the sake of sorting
c	  We will force there to be exactly three entities printed.
c	  PDB format is just a mess:
c	  cols 13:16	atom
c	  col     17	alternate conf
c	  cols 18:20	residue
c	  col     22	chain
c	  cols 23:27	resnum
c
	    do i = 16, 13, -1
		if (ATOM(iatm)(i:i) .ne. ' ') j = i
	    enddo
	    do i = 13, 17
		if (ATOM(iatm)(i:i) .ne. ' ') k = i
	    enddo
	    do i = j, k
		if (ATOM(iatm)(i:i) .eq. ' ') ATOM(iatm)(i:i) = '_'
	    enddo
c	    if (ATOM(iatm)(17:17) .ne. ' ') then
c	      do i = 18,19
c		if (ATOM(iatm)(i:i) .eq. ' ') ATOM(iatm)(i:i) = '_'
c	      enddo
c	    endif
	    if (ATOM(iatm)(22:22) .ne. ' ') then
	      do i = 23,25
		if (ATOM(iatm)(i:i) .eq. ' ') ATOM(iatm)(i:i) = '_'
	      enddo
	    endif
	    write (output,905) ATOM(iatm)(13:17),ATOM(iatm)(18:27),
     &            Uprin(1),Uprin(2),Uprin(3),anisotropy,alonghi,Uiso
905	  format(A5,1X,A10,3F9.4,2X,F9.4,F9.4,F9.4)
c
c	  Also make a histogram of anisotropies
	    i = anisotropy * 20. + 1
	    histogram(i) = histogram(i) + 1
c
c	  And a table of <anis> by distance from center of mass
	    if (comflag .and. anisotropy.lt.1.0) then
	      dist = sqrt( (SPAM(1,iatm)-XCOM)**2 
     &                   + (SPAM(2,iatm)-YCOM)**2 
     &	                 + (SPAM(3,iatm)-ZCOM)**2 )
     	      i = (dist/dmax) * float(dshells) + 1
	      adist(i) = adist(i) + anisotropy
	      hdist(i) = hdist(i) + 1
	    endif
c
	  endif
c
c	Draw principal axes inside bounding ellipsoid
	  if (fancy.eq.1) then
	    do i=1,3
	      size = eigens(i) * pradius - 0.02
	      start(1) = x - size*evecs(1,i)
	      start(2) = y - size*evecs(2,i)
	      start(3) = z - size*evecs(3,i)
	      end(1)   = x + size*evecs(1,i)
	      end(2)   = y + size*evecs(2,i)
	      end(3)   = z + size*evecs(3,i)
	      write (output,907) start, end
907	      format(' 3',/,
     &             3f8.4,' 0.02',3f8.3,' 0.02','  0.5 1.0 0.3')
	    enddo
	  endif
c
c	Draw longest principle axis only
c	(experimental use only - not supported or documented)
	  if (fancy.eq.4) then
	    imax = 1
	    if (eigens(2).gt.eigens(imax)) imax = 2
	    if (eigens(3).gt.eigens(imax)) imax = 3
	    size = eigens(imax) * pradius 
	    imin = 1
	    if (eigens(2).lt.eigens(imin)) imin = 2
	    if (eigens(3).lt.eigens(imin)) imin = 3
	    imed = 6 - (imax+imin)
	    size = size * eigens(imax)/eigens(imed)
	    start(1) = x - size*evecs(1,imax)
	    start(2) = y - size*evecs(2,imax)
	    start(3) = z - size*evecs(3,imax)
	    end(1)   = x + size*evecs(1,imax)
	    end(2)   = y + size*evecs(2,imax)
	    end(3)   = z + size*evecs(3,imax)
	    write (output,907) start, end
	  endif
c
c	Construct 3 quadrics corresponding to the 3 orthogonal planes
c	through the center of our ellipsoid
	if (fancy.eq.2 .or. fancy.eq.3) then
	  eigens(4) = 1.0
	  evecs(4,4)= 1.0
	  det = tinv4( evecinv, evecs )
	  call trnsp4( evecit, evecinv )
	  do k = 1,3
	    do i = 1,4
	    do j = 1,4
		QQ(i,j) = 0.0
	    enddo
	    QQ(i,i) = 1. / (eigens(i)*eigens(i))
	    enddo
	  QQ(k,k) = 1000.
	  QQ(4,4) = -pradius*pradius
	  call tmul4( TEMP, QQ, evecinv )
	  call tmul4( QP, evecit, TEMP )
	  write (output,151) 14, X,Y,Z, radlim, red, green, blue
	  write (output,152) QP(1,1),QP(2,2),QP(3,3),QP(1,2),QP(2,3),
     &                       QP(1,3),QP(1,4),QP(2,4),QP(3,4),QP(4,4)
	  enddo
	endif
      endif
      ENDIF
  138 continue
      IATM = IATM + 1
      IF (IATM.LE.NATM) GOTO 130
c
c     Set transparency for enclosing ellipoids
      if (fancy.eq.1 .or. fancy.eq.3) then
	write (output,'(A,/,A)') '9 Begin transparent ellipsoids','8 '
	write (output,'(A)') ' 15.  0.6   1.0 1.0 1.0   0.6   0 0 0 0'
      else if (fancy.eq.2 .or. fancy.eq.4) then
	goto 160
      endif
c
  139 CONTINUE
c
c If we're just tabulating ellipticity, start making tables
c
      if (tflag) then
	total = 0
	do i = 1,20
	    total = total + histogram(i)
	end do
	if (total.eq.0) then
	    write (noise,*)  'No ANISOU records found'
	    call exit(-1)
	end if
	if (nanis.eq.0) then
	   write (noise,*)   'No anisotropic atoms found'
	   call exit(-1)
	end if
	write (hislun,'(A)') '# Anisotropy  Fraction   Number'
	write (hislun,'(A)') '#   range     of atoms of atoms'
	do i = 1,20
	    write (hislun,'(2F5.2,3X,F8.3,I10)') 
     &		(float(i)-1.)/20., float(i)/20., 
     &		float(histogram(i))/total, histogram(i)
	end do
c       Calculate mean and sigma of distribution
	sum_a2 = 0.0
	sum_b2 = 0.0
	sum_ab = 0.0
	anis_mean  = sum_A / float(nanis)
	anis_sigma = 0.0
	biso_mean  = sum_B / float(nanis)
	ccoef      = 0.0
	do i = 1,NATM
	    if (anisi(i).ne.0) then
		sum_a2 = sum_a2 + (anisi(i) - anis_mean)**2
		sum_b2 = sum_b2 + (SPAM(4,i) - biso_mean)**2
		sum_ab = sum_ab 
     &		       + (anisi(i)-anis_mean) * (SPAM(4,i)-biso_mean)
	    end if
	end do
	biso_mean  = biso_mean * (3.14159*3.14159*8.0) / PRADIUS
	if (nanis.gt.1) then
	    anis_sigma = sqrt( sum_a2 / float(nanis-1) )
	    ccoef = sum_ab / sqrt(sum_a2 * sum_b2)
	end if

	write (hislun,'(A)')         '#'
	write (hislun,'(A,I10)')     '# number of ANISOU records:',nanis+niso
	write (hislun,'(A,I10)')     '#      non-isotropic atoms:',nanis
	write (hislun,'(A,I10)')     '#          isotropic atoms:',niso
	write (hislun,'(A)') '#'
	write (hislun,'(A,F7.3)')
     &       '# correlation between anisotropy and B_iso:', ccoef
	write (hislun,'(A)') '#'
	write (hislun,'(A)') '#              Anisotropy  B_iso'
	write (hislun,'(A)') '#  AtomType   mean  sigma   mean  number'
	write (hislun,'(A)') '# ---------   ----------- ------  ------'
	write (hislun,'(A,F7.3,F7.3,F7.2,I8)')    
     &       '#|    Total',anis_mean,anis_sigma,biso_mean,nanis

c
c     If we're tabulating ellipticity, but not by atom_type, then we're done
c
	if (.not. atflag) goto 145
  140	continue
c
C     Find an atom type we haven't done yet, exit if none left
c     This only works if the PDB file contains the atom type in
c     columns 77:78 (standard since sometime in 1997, but many
c     files do not conform to this standard)
c
	do i = 1, NATM
	    if (anisi(i).ne.0.0 .and. atom(i)(77:78).ne.'  ') then
	    	atomtype = atom(i)(77:78)
		goto 141
	    end if
	end do
	goto 145
  141	continue
	if (atomtype .eq. ' H') 
     &	    write(noise,*) 'You seem to have anisotropic hydrogens',
     &                     ' - is this some kind of joke?'

	sum_A     = 0.0
	sum_a2    = 0.0
	biso_mean = 0.0
	natype    = 0
c
c     Loop over atoms looking for the right ones
c
	do i = 1, NATM
	    if (anisi(i).ne.0.0 .and. atomtype.eq.atom(i)(77:78)) then
		natype = natype + 1
		sum_A = sum_A + anisi(i)
	    end if
	end do
	anis_mean  = sum_A / float(natype)
	anis_sigma = 0.0
	do i = 1, NATM
	    if (anisi(i).ne.0.0 .and. atomtype.eq.atom(i)(77:78)) then
		sum_a2 = sum_a2 + (anisi(i)-anis_mean)**2
		biso_mean = biso_mean + SPAM(4,i)
		atom(i)(77:78) = '  '
	    end if
	end do
	if (natype.gt.1) anis_sigma = sqrt( sum_a2 / float(natype-1) )
	biso_mean  = biso_mean / float(natype)
	biso_mean  = biso_mean * (3.14159*3.14159*8.0) / PRADIUS

	write (hislun,'(A,4X,A,F7.3,F7.3,F7.2,I8)')    
     &       '#|   ',atomtype,anis_mean,anis_sigma,biso_mean,natype
	goto 140
      end if
      IATM = 1
      goto 150
c
c Write out plot of mean anisotropy as a function of distance from c.o.m.
c The hisclean routine is not strictly necessary; it collapses shells at
c the extreme ranges of distance that contain only a few atoms.
c
  145 continue
      if (comflag) then
      	write (comlun,'(A/A,A/A/A)') '#',
     &	    '# Mean anisotropy as a function of distance',
     &      ' from center of mass',
     &	    '#',
     &	    '#  Distance   <anis>   #atoms'
	call hisclean( adist, hdist, dshells, nanis )
	do i = 1, dshells
	    if (hdist(i).gt.0) then
		adist(i) = adist(i) / float(hdist(i))
		dmid = (dmax/float(dshells))*(float(i)-0.5)
	    	write (comlun,146) dmid, adist(i), hdist(i)
	    endif
	enddo
  146	format(F10.3,F10.3,I10)
      endif
      call exit(0)

c
c Second pass write a single sphere or ellipsoid for each atom
c
  150 CONTINUE
      IF (ATOM(IATM)(1:4).EQ.'ATOM' .OR.
     &    ATOM(IATM)(1:4).EQ.'HETA') THEN
	X = SPAM(1,IATM)
	Y = SPAM(2,IATM)
	Z = SPAM(3,IATM)
	ICOL = SPAM(5,IATM)
	if (bcflag) then
	    call U2RGB( SPAM(4,IATM), Umin, Umax, RED, GREEN, BLUE )
	    RED   = RED*RED
	    GREEN = GREEN*GREEN
	    BLUE  = BLUE*BLUE
	else
	    RED   = RGB(1,ICOL)
	    GREEN = RGB(2,ICOL)
	    BLUE  = RGB(3,ICOL)
	endif
	IF (.not. ellipses) THEN
            WRITE(OUTPUT,151) 2, X,Y,Z,SPAM(4,IATM),RED,GREEN,BLUE
	ELSE IF (ATOM(IATM+1)(1:6).EQ.'ANISOU') THEN
	    read (atom(iatm+1)(31:80),*) (anisou(i),i=1,6)
	    do i=1,6
		anisou(i) = anisou(i) * 0.0001
	    enddo
	    if (anitoquad(anisou,pradius,quadric,eigens,evecs).lt.0)then
	        write(noise,*) '*** Non-positive definite ellipsoid - ',
     &				atom(iatm+1)(13:26)
	    endif
	    radlim = pradius * max( eigens(1),eigens(2),eigens(3) )
	    radlim = radlim * MARGIN
	    write (output,151) 14, x,y,z,radlim,red,green,blue
	    write (output,152) (quadric(i),i=1,10)
	ELSE
            WRITE(OUTPUT,151) 2, X,Y,Z,SPAM(4,IATM),RED,GREEN,BLUE
  151	FORMAT(I2,/,7F8.3)
  152	FORMAT(10F12.4)
	ENDIF
      ENDIF
      IATM = IATM + 1
      IF (IATM.LE.NATM) GOTO 150
      IF (fancy.eq.1 .or. fancy.eq.3) then
	write (output,'(A)') '9 end transparent ellipsoids'
      endif
  160 continue
c
c Write bonds to file also. Atoms are considered bonded if they lie
c closer to each other than 0.6 * sum of VDW radii.
C If two atoms of different colors are bonded, make half-bond
C cylinders with each color.
C
      if (radius.eq.0.0) goto 210
c
      DO 202 IATM=1,NATM
	IF (ATOM(IATM)(1:4).NE.'ATOM'.AND.ATOM(IATM)(1:4).NE.'HETA') 
     &      GOTO 202
      DO 201 JATM=IATM+1,NATM
	IF (ATOM(JATM)(1:4).NE.'ATOM'.AND.ATOM(JATM)(1:4).NE.'HETA') 
     &      GOTO 201
	DX = SPAM(1,IATM) - SPAM(1,JATM)
	DY = SPAM(2,IATM) - SPAM(2,JATM)
	DZ = SPAM(3,IATM) - SPAM(3,JATM)
	DIST  = DX*DX + DY*DY + DZ*DZ
	ICOL  = SPAM(5,IATM)
	JCOL  = SPAM(5,JATM)
	CLOSE = 0.6 * (VDW(ICOL) + VDW(JCOL))
	CLOSE = CLOSE**2
	IF (DIST .LE. CLOSE) THEN
	  IF(RGB(1,ICOL) .EQ. RGB(1,JCOL) .AND.
     1       RGB(2,ICOL) .EQ. RGB(2,JCOL) .AND.
     2       RGB(3,ICOL) .EQ. RGB(3,JCOL)) THEN
	    WRITE(OUTPUT,211)
     1         SPAM(1,IATM),SPAM(2,IATM),SPAM(3,IATM),radius,
     2         SPAM(1,JATM),SPAM(2,JATM),SPAM(3,JATM),radius,
     3         RGB(1,ICOL),RGB(2,ICOL),RGB(3,ICOL)
	  ELSE
	    DO K=1,3
		center(K) = (SPAM(K,IATM)+SPAM(K,JATM))/2
	    ENDDO
	    WRITE(OUTPUT,211)
     1         SPAM(1,IATM),SPAM(2,IATM),SPAM(3,IATM),radius,
     2         center(1),center(2),center(3),radius,
     3         RGB(1,ICOL),RGB(2,ICOL),RGB(3,ICOL)
	    WRITE(OUTPUT,211)
     1         center(1),center(2),center(3),radius,
     2         SPAM(1,JATM),SPAM(2,JATM),SPAM(3,JATM),radius,
     3         RGB(1,JCOL),RGB(2,JCOL),RGB(3,JCOL)
	  ENDIF
	ENDIF
  201 CONTINUE
  202 CONTINUE
  210 CONTINUE

211   FORMAT(1H3,/,11f8.3)
c
c
	write (noise,'(/)')
	write (noise,156) 'X  min max center-of-mass:', XMIN, XMAX, xcom
	write (noise,156) 'Y  min max center-of-mass:', YMIN, YMAX, ycom
	write (noise,156) 'Z  min max center-of-mass:', ZMIN, ZMAX, zcom
	write (noise,156) '     scale:', SCALE
  156	format(1x,a,2f8.2,f10.3)
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

      SUBROUTINE ASSERT (LOGIC, DAMMIT)
      LOGICAL LOGIC
      CHARACTER*(*) DAMMIT
      COMMON /ASSOUT/ NOISE
      IF (LOGIC) RETURN
      WRITE (NOISE,*) '*** ',DAMMIT
      STOP 1234
      END


CCC	Return RGB triple that runs from dark blue at Bmin 
CC	to light red at Bmax
C
	subroutine U2rgb( Uiso, Umin, Umax, r, g, b )
	real              Uiso, Umin, Umax, r, g, b
c
	real    fraction, h, s, v
c
	fraction = (Uiso-Umin) / (Umax-Umin)
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

CCC	This subroutine is not strictly necessary.
CC	It smooths the histogram/curve of Anisotropy by 
C	distance from center of mass

	subroutine hisclean( value, number, shells, nanis )
c
	integer shells
	real    value(shells)
	integer number(shells)
	integer nanis
c
	integer minshell, maxshell, min10, max10
	integer nsum
	real    vsum
c
	if (nanis .lt. 1200) then
	    shells = shells / 2
	    do i = 1, shells
		number(i) = number(2*i-1) + number(2*i)
		value(i)  = value(2*i-1) + value(2*i)
	    enddo
	endif
c
	do i = shells, 1, -1
	    if (number(i).gt.0) minshell = i
	enddo
	do i = 1, shells
	    if (number(i).gt.0) maxshell = i
	enddo
	nsum = 0
	min10 = minshell
	do i = minshell, shells
	    if (nsum + number(i) .gt. 10) then
		goto 101
	    else
	    	nsum  = nsum + number(i)
		min10 = i
	    endif
	enddo
101	continue
	nsum = 0
	max10 = maxshell
	do i = maxshell, 1, -1
	    if (nsum + number(i) .gt. 10) then
		goto 102
	    else
	    	nsum  = nsum + number(i)
		max10 = i
	    endif
	enddo
102	continue
c
	nsum = 0
	vsum = 0.0
	do i = minshell, min10
	    nsum = nsum + number(i)
	    vsum = vsum + value(i)
	    number(i) = 0
	    value(i)  = 0.0
	enddo
	i = (minshell + min10) / 2
	number(i) = nsum
	value(i)  = vsum
c
	nsum = 0
	vsum = 0.0
	do i = maxshell, max10, -1
	    nsum = nsum + number(i)
	    vsum = vsum + value(i)
	    number(i) = 0
	    value(i)  = 0.0
	enddo
	i = (maxshell + max10) / 2
	number(i) = nsum
	value(i)  = vsum
c
	return
	end

