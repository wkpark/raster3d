	program ungrasp
C************************************************************************
C									*
C UNGRASP -                                                             *
C									*
C	This program will take the information from a GRASP surface     *
C file and convert it to a form usable by the render program in         *
C Raster3D.  Note that the 2nd + 3rd records list the file contents,    *
C but I haven't added code here to interpret all of them.  The program  *
C can currently handle potentials and vertexcolors.                     *
C									*
C 30 Sep 1994	Ethan A Merritt - initial version                       *
C 24 Jan 1995	EAM - option to output grid rather than surface		*
C 16 Feb 1996	EAM - fixed scale conversion of potential to color info *
C		      coordinated with Raster3d Version 2.2             *
C		      Recognize GRASP Version 1.2 files (format2) also	*
C									*
C************************************************************************
C
	parameter (MAXPTS = 200000)
C
	real*4	vertex(3,MAXPTS)
	real*4	normal(3,MAXPTS)
	real*4	access(3,MAXPTS)
	real*4	potent(MAXPTS)
	real*4	curvature(MAXPTS)
	real*4	distances(MAXPTS)
	real*4	gprop1(MAXPTS)
	real*4	gprop2(MAXPTS)
	real*4	discol(MAXPTS)
	integer*2	triangl2(3,MAXPTS)
	integer*4	triangle(3,MAXPTS)
C
	integer		i,j,k
	real*4		xlo,xhi,ylo,yhi,zlo,zhi
	logical		clip
	logical		grid
	logical		pots, potcol
	logical		cols, vercol
	character*80	line, line3
	character*64	infile, outfile
	integer*4	nvert, ntriangles, gridsize
	real*4		lattice, center(3)
	integer		filetype
C
	integer		indexc(256)
	real*4		coltab(256,3)
C
	
	type 1,'Input file: '
1	format($,a)
	accept '(a)', infile
	open (unit=1, file=infile, form='unformatted')
	
c
c	Can we read this file?
	read (1) line
	type *,'header line 1:',line
	if (line(1:8).eq."format=1") then
	    filetype = 1
	else if (line(1:8).eq."format=2") then
	    filetype = 2
	else
	    type *,'Sorry, unrecognized file format'
	    call exit
	endif
c
	read (1) line
	type *,'header line 2:',line
c
	read (1) line3
	type *,'header line 3:',line3

	read (1) line
C	type *,'header line 4:',line
	read (line,*) nvert, ntriangles, gridsize, lattice
	type *, 'vertices, triangles, gridsize, lattice spacing'
	type *,  nvert, ntriangles, gridsize, lattice

	read (1) line
C	type *,'header line 5:',line
	read (line,*) center
	type *, 'center'
	type *, center(1), center(2), center(3)
C
C	Interpret line2 to find contents of file
C
	read (1) (vertex(1,i),vertex(2,i),vertex(3,i), i=1,nvert)
	read (1) (access(1,i),access(2,i),access(3,i), i=1,nvert)
	read (1) (normal(1,i),normal(2,i),normal(3,i), i=1,nvert)
	if (filetype.eq.1) then
	    read (1) (triangl2(1,i),triangl2(2,i),triangl2(3,i), 
     &               i=1,ntriangles)
	    do i=1,ntriangles
		triangle(1,i) = triangl2(1,i)
		triangle(2,i) = triangl2(2,i)
		triangle(3,i) = triangl2(3,i)
	    enddo
	endif
	if (filetype.eq.2) then
	    read (1) (triangle(1,i),triangle(2,i),triangle(3,i), 
     &               i=1,ntriangles)
	endif
C
C	Interpret line3 to find contents of file
C
	pots = .false.
	cols = .false.
	l = 1
   11	continue
	if (line3(l:l).eq." ") then
		goto 12
	else if (line3(l:l).eq.",") then
		l = l+1
	else if (line3(l:l+9).eq."potentials") then
		pots = .true.
		l = l+10
	else if (line3(l:l+10).eq."vertexcolor") then
		cols = .true.
		l = l+11
	else
		type *,'Don''t know how to handle ', line3(l:80)
		goto 12
	endif
	goto 11
   12	continue

	if (pots) then
	    potmin =  999.
	    potmax = -999.
	    type *,'This file contains potentials'
	    read (1) (potent(i), i=1,nvert)
	    do i=1,nvert
		if (potent(i).gt.potmax) potmax=potent(i)
		if (potent(i).lt.potmin) potmin=potent(i)
	    enddo
	    type *,'Potential range:',potmin,potmax
	    type 1,'Color by potential? '
	    accept '(a80)',line
	    if (line(1:1).eq.'y' .or. line(1:1).eq.'Y') then
	    	potcol =  .true.
		type 1,'What did GRASP say the max potential was? '
		accept *,potxxx
		potmax =  potxxx
		potmin = -potxxx
	    else
	    	potcol = .false.
		potxxx = 1.0
	    endif
	    nclip = 0
	    do i=1,nvert
		if (potent(i) .gt.  potxxx) then
		    potent(i) =  potxxx
		    nclip = nclip + 1
		endif
		if (potent(i) .lt. -potxxx) then
		    potent(i) = -potxxx
		    nclip = nclip + 1
		endif
	    enddo
	    type *,nclip,' truncated color values'
	endif

	if (cols) then
	    type *,'This file contains discrete vertex colors'
	    read (1) (discol(i), i=1,nvert)
	    do j=1,256
		indexc(j) = 0
	    enddo
	    do i=1,nvert
		j = discol(i)
		indexc(j) = indexc(j) + 1
	    enddo
	    type 1,'Use vertex colours? '
	    accept '(a80)',line
	    if (line(1:1).eq.'y' .or. line(1:1).eq.'Y') then
	    	vercol =  .true.
	    	do i=1,256
		    if (indexc(i).gt.0) then
			type '($,a,i4,a)','RGB components for color',i,': '
			accept *, coltab(i,1),coltab(i,2),coltab(i,3)
		    endif
	    	enddo
	    endif
	endif

C
C DEBUG - print out first 10 of each
C
  	goto 100
	do i=1,10
	    type 2, i,	(vertex(j,i),j=1,3) 
    2	    format(' Vertex',i4,':',t20,3f8.2)
	enddo
	do i=1,10
	    type 4, i,	(normal(j,i),j=1,3) 
    4	    format(' Normal',i4,':',t20,3f8.2)
	enddo
	do i=1,10
	    type 6, i, (triangle(j,i),j=1,3)
    6	    format(' Triangle',i4,':',t20,3i8)
	enddo
C
	do i=1,10
	    i1 = triangle(1,i)
	    i2 = triangle(2,i)
	    i3 = triangle(3,i)
	    type 8, i, (vertex(j,i1),j=1,3), (vertex(k,i2),k=1,3),
     *	 	    (vertex(l,i3),l=1,3)
    8	    format(' Triangle',i4,':',t20,3f8.2,2(/,t20,3f8.2))
	enddo
C
	if (cols) type *, 'Vertex colours',(discol(i),i=1,10)
  100	continue
C
C Allow windowing of triangles output, either to select a
C specific subset of the original file contents, or to 
C reduce the processing overhead in Raster3D by omitting
C triangles which will be out of the figure anyway.
C
	type 1,'Clipping bounds (xlo,xhi,ylo,yhi,zlo,zhi:'
	accept '(a80)',line
	read (line,*,err=101) xlo,xhi,ylo,yhi,zlo,zhi
  101	continue
	if (xhi.gt.xlo .and. yhi.gt.ylo .and. zhi.gt.zlo) then
	    clip = .TRUE.
	    type 102,xlo,xhi,ylo,yhi,zlo,zhi
  102	    format('Clipping limits will be: ', 6f10.3)
	else
	    clip = .FALSE.
	    type *,'No clipping limits'
	end if
C
C Allow two output modes
C  1)	Surface made up of triangles with explicit normals
C  2)	Grid/mesh made up of thin cylinders along edges of the triangles
C
	type 1,'Output grid rather than surface?'
	accept '(a80)',line
	if (line(1:1).eq.'y' .or. line(1:1).eq.'Y') then
	    grid =  .true.
	else
	    grid = .false.
C
C Grasp seems a bit chaotic about the direction of the normal vectors
C it puts out.  Allow inversion here.
C
	type 1,'Invert normals?'
	accept '(a80)',line
	if (line(1:1).eq.'y' .or. line(1:1).eq.'Y') then
	    invert = -1
	else
	    invert =  1
	end if
	end if
C
C Write out a Raster3D format file consisting of triangles 
C and surface normals (obviously needs a new version of render!)
C
	type 1,'Output file:'
	accept '(a)', outfile
	open (unit=2, file=outfile, form='FORMATTED',
     *	      carriagecontrol='LIST', status='UNKNOWN')
C
C Assume constant colour for surface, unless discrete vertex colors are
C in the GRASP file.
C
	red   = 0.855
	green = 0.855
	blue  = 0.855
	radius = 0.018
	
	do i=1,ntriangles
	    i1 = triangle(1,i)
	    i2 = triangle(2,i)
	    i3 = triangle(3,i)

	    if (clip) then
		if (vertex(1,i1).lt.xlo .or. vertex(1,i1).gt.xhi) goto 200
		if (vertex(2,i1).lt.ylo .or. vertex(2,i1).gt.yhi) goto 200
		if (vertex(3,i1).lt.zlo .or. vertex(3,i1).gt.zhi) goto 200
	    end if

	    if (potcol) then
		pot   = (potent(i1) + potent(i2) + potent(i3)) / 3.
		if (pot.lt.0) then
		    red   = 0.9 + 0.1*(pot/potmin)
		    green = 0.9 - 0.8*(pot/potmin)
		    blue  = 0.9 - 0.8*(pot/potmin)
		else
		    blue  = 0.9 + 0.1*(pot/potmin)
		    green = 0.9 - 0.8*(pot/potmax)
		    red   = 0.9 - 0.8*(pot/potmax)
		endif
	    endif

	    if (vercol) then
		j1 = discol(i1)
		j2 = discol(i2)
		j3 = discol(i3)
		red   = (coltab(j1,1) + coltab(j2,1) + coltab(j3,1)) / 3.
		green = (coltab(j1,2) + coltab(j2,2) + coltab(j3,2)) / 3.
		blue  = (coltab(j1,3) + coltab(j2,3) + coltab(j3,3)) / 3.
	    endif

C	    /* Grid of lines along triangle edges */
	    if (grid) then
	    	write (2,10)	(vertex(j,i1),j=1,3), radius,
     *				(vertex(k,i2),k=1,3), radius,
     *				red, green, blue
	    	write (2,10)	(vertex(k,i2),k=1,3), radius,
     *				(vertex(l,i3),l=1,3), radius,
     *				red, green, blue
	    	write (2,10)	(vertex(l,i3),l=1,3), radius,
     *				(vertex(j,i1),j=1,3), radius,
     *				red, green, blue
   10	    	format('3',/, 3f8.3, f8.3, 3f8.3, f8.3, 1x, 3f6.2)

C	    /* Surface made up of triangles with explicit normals */
	    else
	    	write (2,14)	(vertex(j,i1),j=1,3), 
     *				(vertex(k,i2),k=1,3),
     *	 	    		(vertex(l,i3),l=1,3),
     *				red, green, blue
   14	    	format('1',/, 3f8.3, 3f8.3, 3f8.3, 1x, 3f6.2)
	    	write (2,16)	(invert * normal(j,i1),j=1,3), 
     *				(invert * normal(k,i2),k=1,3),
     *	 	    		(invert * normal(l,i3),l=1,3)
   16	    	format('7',/, 3f8.3, 3f8.3, 3f8.3, 1x, 3f6.2)
	    end if

  200	continue
	enddo

C
	end
