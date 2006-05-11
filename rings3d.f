	PROGRAM RINGS3D
*------------------------------------------------------------------------------
*       Program to set up input for RENDER by filling in 5- and 6-member rings
*	with triangles.
*
* EAM May 1999	- initial version
*		  residue types are hard-wired in DATA statements
*		  planes are not smoothed
*------------------------------------------------------------------------------
	implicit none
c
	include 'VERSION.incl'
c
c     I/O units and environmental control
c
	integer input, output, noise
	parameter (input=5, output=6, noise=0)
	integer 	narg
	integer 	iargc
c	external	iargc
	character*64	options
	logical		bflag, pflag, sflag
c
c     Data arrays
c
	integer		 MAXATOM,       MAXCOL
	parameter	(MAXATOM=10000, MAXCOL=1000)
	real	    rgb(3,MAXCOL), vdw(MAXCOL)
	real        x(MAXATOM), y(MAXATOM), z(MAXATOM)
	integer	    resno(MAXATOM), color(MAXATOM)
	character*1 chain(MAXATOM)
	character*4 resname(MAXATOM), name(MAXATOM)
c
	character*24 mask(MAXCOL),test
	character*80 card
	logical match
c
c     General storage
c
	integer	i,j,n
	integer ncol, icol, nrings 
	integer natm, iatm, jatm
	integer ind(10)
	integer ringsize
	real    red,grn,blu
C
C     Known residue types containing one or two rings
C     Each row contains a residue name (type) in the first entry
C	followed by up to 9 atom names which define the ring[s].
C	A blank name terminates the list for this residue type.
C
C     Five-membered rings (e.g. furanose)
C	connect atoms (1,2,5)(2,3,5)(3,4,5)
C	=> atoms 1 and/or 4 can be out-of-plane, but not 2,3,5
C     Six-membered rings (e.g. pyranose)
C	connect atoms (1,2,6)(2,3,5)(2,5,6)(3,4,5)
C	=> atoms 1 and/or 4 can be out-of-plane, but not 2,3,5,6
C     Nine-membered rings are assumed to be a conjugated ring system
C
C
C     To add a new residue type:
c		increase NTYPES by 1
c		add row of atom names to type array
c		adjust start/end indices of protein/base/sugar flags
c		optional:  add default colour record
C
	integer	   NTYPES
	parameter (NTYPES=21)
	character*4	type( 10, NTYPES )
	integer		nhits( NTYPES )
	integer		p_start, p_end, b_start, b_end, s_start, s_end
C
C     Default to solid color
C
	integer    DEFCOLS
	parameter (DEFCOLS=6)
	character*60 defcol(DEFCOLS)
	data defcol /
     & 'COLOUR###########  A##########   0.501   0.501   1.000  2.00',
     & 'COLOUR###########  C##########   1.000   0.398   0.063  2.00',
     & 'COLOUR###########  G##########   1.000   0.101   0.101  2.00',
     & 'COLOUR###########  T##########   0.501   1.000   0.501  2.00',
     & 'COLOUR###########  U##########   0.501   1.000   0.501  2.00',
     & 'COLOUR########################   0.800   0.000   0.000  2.00'
     &            /
c
	data type /
     &  ' HIS',' CG ',' ND1',' CE1',' NE2',' CD2',   ' ',' ',' ',' ',
     &  ' PHE',' CG ',' CD1',' CE1',' CZ ',' CE2',' CD2',' ',' ',' ',
     &  ' TYR',' CG ',' CD1',' CE1',' CZ ',' CE2',' CD2',' ',' ',' ',
     &  ' TRP',' CZ2',' CH2',' CZ3',' CE3',' CD2',' CE2',
     &         ' CG ',' CD1',' NE1',
     &	' GAL',' C1 ',' C2 ',' C3 ',' C4 ',' C5 ',' O5 ',' ',' ',' ',
     &	' NAG',' C1 ',' C2 ',' C3 ',' C4 ',' C5 ',' O5 ',' ',' ',' ',
     &	' NGA',' C1 ',' C2 ',' C3 ',' C4 ',' C5 ',' O5 ',' ',' ',' ',
     &	' GLC',' C1 ',' C2 ',' C3 ',' C4 ',' C5 ',' O5 ',' ',' ',' ',
     &	' MAN',' C1 ',' C2 ',' C3 ',' C4 ',' C5 ',' O5 ',' ',' ',' ',
     &	' MNG',' C1 ',' C2 ',' C3 ',' C4 ',' C5 ',' O5 ',' ',' ',' ',
     &	' SIA',' C2 ',' C3 ',' C4 ',' C5 ',' C6 ',' O6 ',' ',' ',' ',
     &  '  +C',' N1 ',' C2 ',' N3 ',' C4 ',' C5 ',' C6 ',' ',' ',' ',
     &  '  +T',' N1 ',' C2 ',' N3 ',' C4 ',' C5 ',' C6 ',' ',' ',' ',
     &  '  +U',' N1 ',' C2 ',' N3 ',' C4 ',' C5 ',' C6 ',' ',' ',' ',
     &  '  +A',' N3 ',' C2 ',' N1 ',' C6 ',' C5 ',' C4 ',
     &         ' N7 ',' C8 ',' N9 ',
     &  '  +G',' N3 ',' C2 ',' N1 ',' C6 ',' C5 ',' C4 ',
     &         ' N7 ',' C8 ',' N9 ',
     &  '   C',' N1 ',' C2 ',' N3 ',' C4 ',' C5 ',' C6 ',' ',' ',' ',
     &  '   T',' N1 ',' C2 ',' N3 ',' C4 ',' C5 ',' C6 ',' ',' ',' ',
     &  '   U',' N1 ',' C2 ',' N3 ',' C4 ',' C5 ',' C6 ',' ',' ',' ',
     &  '   A',' N3 ',' C2 ',' N1 ',' C6 ',' C5 ',' C4 ',
     &         ' N7 ',' C8 ',' N9 ',
     &  '   G',' N3 ',' C2 ',' N1 ',' C6 ',' C5 ',' C4 ',
     &         ' N7 ',' C8 ',' N9 '
     &  / 
c
	data p_start, p_end / 1,  4 /
	data s_start, s_end / 5, 11 /
	data b_start, b_end /12, 21 /
c
	bflag  = .FALSE.
	pflag  = .FALSE.
	sflag  = .FALSE.
	narg  = iargc()
	i = 0
  100	continue
	i = i + 1
	if (i.gt.narg) goto 199
	      call getarg( i, options )
	      if (options(1:2) .eq. '-b') then
		bflag = .true.
	      else if (options(1:2) .eq. '-p') then
		pflag = .true.
	      else if (options(1:2) .eq. '-s') then
		sflag = .true.
	      else
		goto 101
	      endif
	goto 100
c
  101	continue
	write (noise,'(A)') 
     &	     'syntax: rings3d [-options] < infile.pdb > outfile.r3d'
	write (noise,'(A)') 
     &	     '        -bases    A/C/G/T/U'
	write (noise,'(A)') 
     &	     '        -proteinn HIS/PHE/TRP/TYR'
	write (noise,'(A)') 
     &	     '        -sugars   GAL/GLC/MAN/NAG/NGA/SIA'

	call exit(-1)
  199	continue
	if (.not.bflag .and. .not.pflag) sflag = .true.
c
	write (noise,*) 'Raster3D rings3d program ',VERSION
c
c     Read in colours first
c
	ncol = 0
	natm = 0
  200	continue
	  read(input,'(a80)',end=250) card

	  if (card(1:4).eq.'COLO') then
	    ncol = ncol + 1
	    if (ncol.gt.MAXCOL) then
	      write(noise,*) 'Colour table overflow.  Increase ',
     &                     'MAXCOL and recompile.'
	      stop 10
	    endif
	    read(card,'(6x,a24,3f8.3,f6.2)') mask(ncol),
     &          (rgb(i,ncol),i=1,3),vdw(ncol)

	  elseif (card(1:4).eq.'ATOM'.or.card(1:4).eq.'HETA') then
*	    Load default colors after any that were read in
	    if (natm.eq.0.and.ncol.le.MAXCOL-DEFCOLS) then
		do i = 1,DEFCOLS
		  ncol = ncol + 1
		  read(defcol(I),'(6x,a24,3f8.3,f6.2)') mask(ncol),
     &            (rgb(j,ncol),j=1,3), vdw(ncol)
		enddo
	    endif
*	    Now we read in ATOM record
	    natm = natm + 1
	    if (natm.gt.MAXATOM) then
	      write(noise,*) 'Atom array overflow.  Increase ',
     &                     'MAXATOM and recompile.'
	      stop 20
	    endif
	    test = card(7:30)
	    do icol = 1, ncol
	      if (match(test,mask(icol))) then
	      	color(natm)   = icol
	      	read(card,901) resno(natm),x(natm),y(natm),z(natm)
901	        format(22x,i4,4x,3f8.3)
		name(natm)    = card(13:16)
		resname(natm) = card(17:20)
		chain(natm)   = card(22:22)
		goto 220
	      endif
	    enddo
  220	    continue

	  elseif (card(1:3).eq.'END') then
	    go to 250
	  endif

	  go to 200

*     Come here when EOF or 'END' record is reached
  250	continue
	if (natm.eq.0) then
	  write(noise,*) 'No atoms in input.'
	  stop 30
	else
	  write(noise,*) 'Input atoms: ',natm
	endif
	
c
C	Here's the real loop.  
C
	nrings = 0
	jatm = 0
  300	continue
  	iatm = jatm + 1
	if (iatm.ge.natm) goto 399
c
	do j = iatm, natm
	  if (resno(j).eq.resno(iatm)
     &    .and.chain(j).eq.chain(iatm)) then
	    jatm = j
	  else
	    goto 301
	  endif
	enddo
  301	continue

	if (bflag) then
	  do n = b_start, b_end
	    if (resname(iatm)(2:4).eq.type(1,n)(2:4)) goto 310
	  enddo
	endif
	if (pflag) then
	  do n = p_start, p_end
	    if (resname(iatm)(2:4).eq.type(1,n)(2:4)) goto 310
	  enddo
	endif
	if (sflag) then
	  do n = s_start, s_end
	    if (resname(iatm)(2:4).eq.type(1,n)(2:4)) goto 310
	  enddo
	endif
	goto 300

  310	continue
  	ringsize = 9
	do i = 2, 10
	  if (type(i,n).eq.' ') then
	    ringsize = i - 2
	    goto 320
	  endif
	  do j = iatm, jatm
	    if (type(i,n).eq.name(j)) then
	      ind(i-1) = j
	      goto 312
	    endif
	  enddo
	  write(noise,*) 'Cannot find atom ',type(i,n),' in',
     &                   type(1,n), resno(iatm), chain(iatm)
	  goto 300
  312	continue
	enddo
  320	continue
c
c	Found one
c
	nrings   = nrings + 1
	nhits(n) = nhits(n) + 1
  	red = rgb(1,color(ind(1)))
  	grn = rgb(2,color(ind(1)))
  	blu = rgb(3,color(ind(1)))
	write(output,321) type(1,n),resno(iatm),chain(iatm)
  321	format('# ',a4,2x,i4,a1)
  322	format('1',/,3(3f8.2),3f6.3)

	if (ringsize.eq.5) then
	  write(output,322) 
     &      x(ind(1)),y(ind(1)),z(ind(1)),
     &	    x(ind(2)),y(ind(2)),z(ind(2)),
     &      x(ind(5)),y(ind(5)),z(ind(5)),
     &	    red,grn,blu
	  write(output,322) 
     &	    x(ind(3)),y(ind(3)),z(ind(3)),
     &      x(ind(4)),y(ind(4)),z(ind(4)),
     &      x(ind(5)),y(ind(5)),z(ind(5)),
     &	    red,grn,blu
	  write(output,322) 
     &      x(ind(2)),y(ind(2)),z(ind(2)),
     &      x(ind(3)),y(ind(3)),z(ind(3)),
     &	    x(ind(5)),y(ind(5)),z(ind(5)),
     &	    red,grn,blu
	endif
	if (ringsize.ge.6) then
	  write(output,322) 
     &	    x(ind(2)),y(ind(2)),z(ind(2)),
     &      x(ind(3)),y(ind(3)),z(ind(3)),
     &      x(ind(5)),y(ind(5)),z(ind(5)),
     &	    red,grn,blu
	  write(output,322) 
     &	    x(ind(1)),y(ind(1)),z(ind(1)),
     &      x(ind(2)),y(ind(2)),z(ind(2)),
     &      x(ind(6)),y(ind(6)),z(ind(6)),
     &	    red,grn,blu
	  write(output,322) 
     &	    x(ind(3)),y(ind(3)),z(ind(3)),
     &      x(ind(4)),y(ind(4)),z(ind(4)),
     &      x(ind(5)),y(ind(5)),z(ind(5)),
     &	    red,grn,blu
	  write(output,322) 
     &	    x(ind(2)),y(ind(2)),z(ind(2)),
     &      x(ind(5)),y(ind(5)),z(ind(5)),
     &      x(ind(6)),y(ind(6)),z(ind(6)),
     &	    red,grn,blu
	endif
	if (ringsize.eq.9) then
	  write(output,322) 
     &      x(ind(5)),y(ind(5)),z(ind(5)),
     &	    x(ind(7)),y(ind(7)),z(ind(7)),
     &      x(ind(8)),y(ind(8)),z(ind(8)),
     &	    red,grn,blu
	  write(output,322) 
     &	    x(ind(6)),y(ind(6)),z(ind(6)),
     &      x(ind(8)),y(ind(8)),z(ind(8)),
     &      x(ind(9)),y(ind(9)),z(ind(9)),
     &	    red,grn,blu
	  write(output,322) 
     &	    x(ind(5)),y(ind(5)),z(ind(5)),
     &      x(ind(6)),y(ind(6)),z(ind(6)),
     &      x(ind(8)),y(ind(8)),z(ind(8)),
     &	    red,grn,blu
	endif

	goto 300
  399	continue

	write (noise,*) 'Filled rings for',nrings,' residues'
	do i = 1,NTYPES
	  if (nhits(i).gt.0) write (noise,*) '    ',type(1,i),nhits(i)
	enddo
c
c
c
	end


	LOGICAL FUNCTION MATCH (SUBJ, MASK)
	CHARACTER*24 SUBJ,MASK
	MATCH = .FALSE.
	DO I = 1, 24
	  IF (SUBJ(I:I).NE.MASK(I:I) .AND. MASK(I:I).NE.'#') RETURN
	ENDDO
	MATCH = .TRUE.
	RETURN
	END
