	subroutine parse
c
	common /options/ nscheme, nax, nay, invert, otmode, quality
     &                 , lflag, fontscale
	integer          nscheme
	integer*2        nax, nay, otmode, quality
	logical          invert, lflag
	real             fontscale
c
	common /raster/  ntx,nty,npx,npy
	integer          ntx,nty,npx,npy
c
	common /asscom/  assout, verbose
	integer          assout
	logical                  verbose
c
	PARAMETER (MAXNTX=256,MAXNTY=256)
c
	character*64     args(4), option
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
	quality = 95
	lflag = .false.
	fontscale = 3.0
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
	    else if (option(1:5).eq.'-qual') then
	    	iarg = iarg + 1
		call getarg( iarg, option )
		read (option,*,err=10) quality
		if (quality.le.0 .or. quality.gt.100) quality = 95
	    else if (option(1:5).eq.'-size') then
	    	iarg = iarg + 1
		call getarg( iarg, option )
		do k = 15,2,-1
		    if (option(k:k).eq.'x') kbrk = k
		end do
		read (option(1:kbrk-1),*,err=10) nax
		read (option(kbrk+1:15),*,err=10) nay
	    else if (option(1:6).eq.'-label') then
	    	lflag = .true.
		call getarg( iarg+1, option )
		if (iarg.lt.nargs .and. option(1:1).ne.'-') then
		    iarg = iarg + 1
		else
		    option = 'label3d.ps'
		end if
		call lopen(option)
	    else if (option(1:10).eq.'-fontscale') then
	        iarg = iarg + 1
		call getarg( iarg, option )
		read (option,*,err=10) fontscale
		if (fontscale.le.0) then fontscale = 3.0
	    else 
	    	larg = larg + 1
		args(larg) = option
	    end if
	iarg = iarg + 1
	goto 10
   11	continue
c
	otmode = local(0, args(1), args(2), args(3), args(4))
c
	if (and(otmode,invertflag).ne.0) then
	    invert = .false.
	else
	    invert = .true.
	end if
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
	endif
	if (verbose .and. (nax.gt.0 .or. nay.gt.0)) then
	    write(0,32) 'X',ntx,npx,ntx*npx
	    write(0,32) 'Y',nty,npy,nty*npy
   32	    format('Autotiling on ',A1,': ',i3,' x ',i3,' >= ',i5)
	end if
c
	end
