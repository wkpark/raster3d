	subroutine parse
c	Version 2.6a
c
      COMMON /OPTIONS/ FONTSCALE, ZOOM, NSCHEME, SHADOWFLAG, 
     &                 NAX, NAY, OTMODE, QUALITY, INVERT, LFLAG
      REAL             FONTSCALE, ZOOM
      INTEGER          NSCHEME, SHADOWFLAG
      INTEGER*2        NAX, NAY, OTMODE, QUALITY
      LOGICAL*2        INVERT, LFLAG
c
	common /asscom/  assout, verbose
	integer          assout
	logical                  verbose
c
	character*64     args(4), option, labelfile
	save             args
c
	integer    INVERTFLAG
	parameter (INVERTFLAG=8)
c
	do larg = 1,4
	    args(larg) = ' '
	end do
	larg = 0
	verbose = .false.
	nax = -1
	nay = -1
	nscheme = -1
	shadowflag = -1
	quality = 90
	lflag = .false.
	zoom = 0.
c
c	Default font scale is 3.0 (appropriate for 300 dpi printers)
c	This is superseded by the environmental variable FONTSCALE
c	or a specific command line options -fontscale XX
	call getenv('FONTSCALE',option)
	read (option,*,err=8,end=8) fontscale 
	goto 9
    8	fontscale = 3.0
    9	continue
c
	iarg  = 1
	nargs = iargc()
   10	continue
	if (iarg .gt. nargs) goto 11
	    call getarg( iarg, option )
	    if (option(1:6).eq.'-debug') then
	    	verbose = .true.
	    else if (option(1:3).eq.'-aa') then
	    	if (option(4:4).eq.'0') then
		    nscheme = 0
		else if (option(4:4).eq.'1') then
		    nscheme = 1
		else
		    nscheme = 4
		end if
	    else if (option(1:6).eq.'-draft') then
	    	nscheme = 1
	    else if (option(1:6).eq.'-alpha') then
	    	nscheme = 0
	    else if (option(1:7).eq.'-transp') then
	    	nscheme = 0
	    else if (option(1:5).eq.'-qual') then
	    	iarg = iarg + 1
		call getarg( iarg, option )
		read (option,*,err=10,end=10) quality
		if (quality.le.0 .or. quality.gt.100) quality = 95
	    else if (option(1:5).eq.'-size') then
	    	iarg = iarg + 1
		call getarg( iarg, option )
		kbrk = 2
		do k = 15,2,-1
		    if (option(k:k).eq.'x') kbrk = k
		    if (option(k:k).eq.'X') kbrk = k
		end do
		read (option(1:kbrk-1),*,err=10,end=10) nax
		read (option(kbrk+1:15),*,err=10,end=10) nay
		if (nscheme.lt.0) nscheme = -4
	    else if (option(1:6).eq.'-label') then
	    	lflag = .true.
		call getarg( iarg+1, option )
		if (iarg.lt.nargs .and. option(1:1).ne.'-') then
		    labelfile = option
		    iarg = iarg + 1
		else
		    labelfile = 'label3d.ps'
		end if
	    else if (option(1:10).eq.'-fontscale') then
	        iarg = iarg + 1
		call getarg( iarg, option )
		read (option,*,err=10,end=10) fontscale
		if (fontscale.le.0) fontscale = 3.0
	    else if (option(1:5).eq.'-zoom') then
	    	iarg = iarg + 1
		call getarg( iarg, option )
		kbrk = 15
		do k = 15,2,-1
		    if (option(k:k).eq.'%') kbrk = k
		end do
		read (option(1:kbrk-1),*,err=10,end=10) zoom
		if (zoom.le.0.) zoom = 0.
		if (option(kbrk:kbrk).eq.'%') zoom = -zoom
	    else if (option(1:7).eq.'-shadow') then
	    	shadowflag = 1
	    else if (option(1:9).eq.'-noshadow') then
	    	shadowflag = 0
	    else 
	    	larg = larg + 1
		args(larg) = option
	    end if
	iarg = iarg + 1
	goto 10
   11	continue
c
	if (lflag) call lopen(labelfile)
c
	otmode = local(0, args(1), args(2), args(3), args(4))
c
	if (and(otmode,invertflag).ne.0) then
	    invert = .false.
	else
	    invert = .true.
	end if
c
	call autotile( nax, nay )
	end

	subroutine autotile( nax, nay )
c
	implicit NONE
c
	include 'parameters.incl'
c
	common /raster/  ntx,nty,npx,npy
	integer          ntx,nty,npx,npy
c
	common /asscom/  assout, verbose
	integer          assout
	logical                  verbose
c
	integer*2 nax, nay
c
	if (nax .gt. 0) then
	    npx = 2
	    ntx = (nax+1) / 2
   21	    continue
	    if (mod(ntx,2).eq.0) then
	    	ntx = ntx / 2
		npx = npx * 2
	    else if (mod(ntx,3).eq.0) then
	    	ntx = ntx / 3
		npx = npx * 3
	    else if (mod(ntx,5).eq.0) then
	    	ntx = ntx / 5
		npx = npx * 5
	    else
	    	ntx = ntx + 1
	    end if
	    if (ntx.gt.MAXNTX) goto 21
	    if (npx.lt.6 .and. ( mod(ntx,2).eq.0
     &	        .or. mod(ntx,3).eq.0 .or. mod(ntx,5).eq.0))
     &		goto 21
	    if (npx.gt.MAXNPX) then
		npx = MAXNPX
		ntx = (nax + MAXNPX - 1) / MAXNPX
	    endif
	endif
	if (nay .gt. 0) then
	    npy = 2
	    nty = (nay+1) / 2
   31	    continue
	    if (mod(nty,2).eq.0) then
	    	nty = nty / 2
		npy = npy * 2
	    else if (mod(nty,3).eq.0) then
	    	nty = nty / 3
		npy = npy * 3
	    else if (mod(nty,5).eq.0) then
	    	nty = nty / 5
		npy = npy * 5
	    else
	    	nty = nty + 1
	    end if
	    if (nty.gt.MAXNTY) goto 31
	    if (npy.lt.6 .and. ( mod(nty,2).eq.0
     &	        .or. mod(nty,3).eq.0 .or. mod(nty,5).eq.0))
     &		goto 31
	    if (npy.gt.MAXNPY) then
		npy = MAXNPY
		nty = (nay + MAXNPY - 1) / MAXNPY
	    endif
	endif
	if (verbose .and. (nax.gt.0 .or. nay.gt.0)) then
	    write(0,32) 'X',ntx,npx,ntx*npx
	    write(0,32) 'Y',nty,npy,nty*npy
   32	    format('Autotiling on ',A1,': ',i3,' x ',i3,' >= ',i5)
	end if
c
	end
