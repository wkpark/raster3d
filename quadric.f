************************************************************************
*              Support routines for quadric surfaces                   *
************************************************************************
* EAM Jun 1997	- initial version, supports version 2.4(alpha) of render 
* EAM May 1998	- additional error checking to go with Parvati/rastep
* EAM Jan 1999	- version 2.4i
************************************************************************
*
* Quadric surfaces include spheres, cones, ellipsoids, paraboloids, and
* hyperboloids.  The motivation for this code was to allow rendering 
* thermal ellipsoids for atoms, so the other shapes have not been
* extensively tested.
* A quadric surface is described by 10 parameters (A ... J).
* For efficiency during rendering it is also useful to know the center and 
* a bounding sphere. So a QUADRIC descriptor to render has 17 parameters:
* 14 (object type QUADRIC)
*    X Y Z RADLIM RED GRN BLU
*    A B C D E F G H I J
*
* The surface itself is the set of points for which Q(x,y,z) = 0
* where 
*      Q(x,y,z)	=  A*x^2 +  B*y^2 +  C*z^2 
*         	+ 2D*x*y + 2E*y*z + 2F*z*x
*         	+ 2G*x   + 2H*y   + 2I*z   
*         	+  J
* 
* It is convenient to store this information in a matrix QQ
*	| QA QD QF QG |		QA = A    QB = B    QC = C
* QQ =	| QD QB QE QH |		QD = D    QE = E    QF = F  
*	| QF QE QC QI |		QG = G    QH = H    QI = I  
*	| QG QH QI QJ |		QJ = J
*
* Then Q(x,y,x) = XT*QQ*X	where X = (x,y,z,1)
* The point of this is that a 4x4 homogeneous transformation T can be 
* applied to QQ by matrix multiplication:     QQ' = TinvT * QQ * Tinv
* 
* The surface normal is easily found by taking the partial derivatives
* of Q(x,y,z) at the point of interest.
************************************************************************
* TO DO:
*	- can we distinguish an ellipsoid from other quadrics? Do we care?
************************************************************************


CCC	Calculate the matrices for quadric transformation
CC	QQ' = TINVT * QQ * TINV      (TINV is inverse of transposed TMAT)
C
	subroutine qsetup
*
      COMMON /MATRICES/ XCENT, YCENT, SCALE, EYEPOS, SXCENT, SYCENT,
     &                  TMAT, TINV, TINVT, SROT, SRTINV, SRTINVT
     &                 ,RAFTER, TAFTER
      REAL   XCENT, YCENT, SCALE, EYEPOS, SXCENT, SYCENT
      REAL   TMAT(4,4), TINV(4,4),   TINVT(4,4) 
      REAL   SROT(4,4), SRTINV(4,4), SRTINVT(4,4)
      REAL   RAFTER(4,4), TAFTER(3)
*
      COMMON /ASSCOM/ noise, verbose
      integer         noise
      logical         verbose
*
	real   TRAN(4,4), POST(4,4), det
*
*	TMAT is a post-multiplier matrix, but unfortunately the
*	quadric surface math was worked out for pre-multipliers
*	So the quadric math uses the transpose of TMAT
	call trnsp4( TRAN, TMAT )
*
*	Remove translation component from TMAT.
*	I had to make a choice whether the X,Y,Z "center" of the quadric
*	is implicitly coded into the coefficients, or whether it is to be
*	maintained explicitly.  Since all other object types do the latter,
*	I have chosen to keep all translation components out of the quadric
*	coefficients
	tran(1,4) = 0.0
	tran(2,4) = 0.0
	tran(3,4) = 0.0
*
*	July 1999 - Allow post-hoc rotation matrix also
	call tmul4( POST, RAFTER, TRAN )
*
	det = tinv4( TINV, POST ) / TMAT(4,4)
	call trnsp4( TINVT, TINV )
*
*	While we're at it, check for legality of rotation matrix
	if (abs(1. - abs(det)) .gt. 0.02) then
	    write (noise,901) abs(det)
  901	    format('>>> Warning: Determinant of rotation matrix =',
     &           F7.3,' <<<')
	endif
	if (TMAT(1,4).ne.0.or.TMAT(2,4).ne.0.or.TMAT(3,4).ne.0) then
	    write (noise,902)
  902       format('>>> Warning: Non-zero cross terms in 4th column of',
     &             ' TMAT <<<')
	endif
*
*	Same thing again for shadow rotation matrix
	call  trnsp4( TRAN, SROT )
	det = tinv4( SRTINV, TRAN )
	call  trnsp4( SRTINVT, SRTINV )
	if (abs(1. - abs(det)) .gt. 0.02) then
	    write (noise,903) abs(det)
  903	    format('>>> Warning: Determinant of shadow matrix =',
     &           F7.3,' <<<')
	endif
*
	return
	end
	

CCC     Process single object descriptor during input phase
CC
C
	function qinp( buf, detail, shadow, sdtail )
*
	IMPLICIT NONE
	logical  qinp
	real    buf(100)
	real    detail(17), sdtail(14)
	logical shadow
*
	real    QQ(4,4), QP(4,4), QT(4,4)
	real  	xq, yq, zq, radlim, red, grn, blu
	real    xc, yc, zc, rc
	real    xr, yr, zr, xs, ys, zs, rs
	real	pfac
	integer ix,iy,ixlo,ixhi,iylo,iyhi
*
*     Array sizes
      INCLUDE 'parameters.incl'
*
      EXTERNAL PERSP
      REAL     PERSP
*
      COMMON /RASTER/ NTX,NTY,NPX,NPY
      INTEGER         NTX,NTY,NPX,NPY
*
      COMMON /MATRICES/ XCENT, YCENT, SCALE, EYEPOS, SXCENT, SYCENT,
     &                  TMAT, TINV, TINVT, SROT, SRTINV, SRTINVT
     &                 ,RAFTER, TAFTER
      REAL   XCENT, YCENT, SCALE, EYEPOS, SXCENT, SYCENT
      REAL   TMAT(4,4), TINV(4,4),   TINVT(4,4) 
      REAL   SROT(4,4), SRTINV(4,4), SRTINVT(4,4)
      REAL   RAFTER(4,4), TAFTER(3)
*
      COMMON /LISTS/ KOUNT, MOUNT, TTRANS, ISTRANS
      INTEGER KOUNT(MAXNTX,MAXNTY), MOUNT(NSX,NSY)
      INTEGER TTRANS(MAXNTX,MAXNTY), ISTRANS
*
      COMMON /NICETIES/ TRULIM,      ZLIM,    FRONTCLIP, BACKCLIP
     &                , ISOLATION
      REAL              TRULIM(3,2), ZLIM(2), FRONTCLIP, BACKCLIP
      LOGICAL           ISOLATION
*
* Assume this is legitimate
	qinp = .TRUE.
*
* Update limits (have to trust the center coords)
*
	xq     = buf(1)
	yq     = buf(2)
	zq     = buf(3)
	radlim = buf(4)
	call assert(radlim.ge.0,'limiting radius < 0 in quadric')
	if (radlim.gt.0) then
	    trulim(1,1) = MIN( trulim(1,1), xq)
	    trulim(1,2) = MAX( trulim(1,2), xq)
	    trulim(2,1) = MIN( trulim(2,1), yq)
	    trulim(2,2) = MAX( trulim(2,2), yq)
	    trulim(3,1) = MIN( trulim(3,1), zq)
	    trulim(3,2) = MAX( trulim(3,2), zq)
	endif
*
* Standard color checks
*
	red    = buf(5)
	grn    = buf(6)
	blu    = buf(7)
	call assert(red.ge.0,'red < 0 in quadric')
	call assert(red.le.1,'red > 1 in quadric')
	call assert(grn.ge.0,'grn < 0 in quadric')
	call assert(grn.le.1,'grn > 1 in quadric')
	call assert(blu.ge.0,'blu < 0 in quadric')
	call assert(blu.le.1,'blu > 1 in quadric')
*
* Transform center before saving
* (But can we deal with perspective????)
*
	call transf (xq, yq, zq)
	radlim = radlim / TMAT(4,4)
	if (eyepos.gt.0) then
c	    pfac = 1./(1.-zq/eyepos)
	    pfac = persp(zq)
	    xq = xq * pfac
	    yq = yq * pfac
	    zq = zq * pfac
	    radlim = radlim * pfac
	endif
	xc = xq * scale + xcent
	yc = yq * scale + ycent
	zc = zq * scale
	rc = radlim * scale
*	save transformed Z limits
	zlim(1) = min( zlim(1), zc )
	zlim(2) = max( zlim(2), zc )
*
*	check for Z-clipping
	if (zc.gt.FRONTCLIP .or. zc.lt.BACKCLIP) then
	    qinp = .FALSE.
	    return
	endif
*
	detail(1) = xc
	detail(2) = yc
	detail(3) = zc
	detail(4) = rc
	detail(5) = red
	detail(6) = grn
	detail(7) = blu
*
* This is a terrible kludge, but necessary if called from normal3d -size BIGxBIG
* Thes test should really be if we are called from normal3d but no flag for that
	if (ntx.gt.MAXNTX .or. nty.gt.MAXNTY) goto 101
*
* Tally for tiles the object might impinge on
* Again we are relying on the correctness of the center coordinates
*
	ixlo = (xc-rc) / npx + 1
	ixhi = (xc+rc) / npx + 1
	iylo = (yc-rc) / npy + 1
	iyhi = (yc+rc) / npy + 1
	if (ixlo.lt.1)   ixlo = 1
	if (ixlo.gt.NTX) goto 101
	if (ixhi.lt.1)   goto 101
	if (ixhi.gt.NTX) ixhi = NTX
	if (iylo.lt.1)   iylo = 1
	if (iylo.gt.NTY) goto 101
	if (iyhi.lt.1)   goto 101
	if (iyhi.gt.NTY) iyhi = NTY
	do iy = iylo,iyhi
	do ix = ixlo,ixhi
	    KOUNT(ix,iy) = KOUNT(ix,iy) + 1
	    TTRANS(ix,iy) = TTRANS(ix,iy) + istrans
	enddo
	enddo
101	continue
*
* build matrix from coeffients describing quadric surface in standard form
*
	qq(1,1) = buf(8)
	qq(2,2) = buf(9)
	qq(3,3) = buf(10)
	qq(1,2) = buf(11)
	qq(2,1) = buf(11)
	qq(2,3) = buf(12)
	qq(3,2) = buf(12)
	qq(1,3) = buf(13)
	qq(3,1) = buf(13)
	qq(1,4) = buf(14)
	qq(4,1) = buf(14)
	qq(2,4) = buf(15)
	qq(4,2) = buf(15)
	qq(3,4) = buf(16)
	qq(4,3) = buf(16)
	qq(4,4) = buf(17)
*
* Transformed matrix QP = TINV(Transpose) * QQ * TINV
* where TINV is the inverse of TMAT
*
	call tmul4( qt, qq,  tinv )
	call tmul4( qp, tinvt, qt )
CD	noise = 0
CD	write (noise,191) 'QT  ',((QT(i,j),j=1,4),i=1,4)
CD 191	format(a,4(/,4f8.4))
CD	write (noise,191) 'QP  ',((QP(i,j),j=1,4),i=1,4)
*
* Save components of quadric surface built from transformed matrix 
* for use during rendering
*
	detail(8)  = qp(1,1)
	detail(9)  = qp(2,2)
	detail(10) = qp(3,3)
	detail(11) = qp(1,2)
	detail(12) = qp(2,3)
	detail(13) = qp(1,3)
	detail(14) = qp(1,4)
	detail(15) = qp(2,4)
	detail(16) = qp(3,4)
	detail(17) = qp(4,4)
*
* Do it all over again for the shadow buffers NOT TESTED YET!  
* (since I'm a little confused about what transformation
* I need to apply to the QQ matrix in shadow space)
*
	if (.not.shadow) return
*	first transform center and limiting sphere
	xr = srot(1,1)*xq + srot(1,2)*yq + srot(1,3)*zq
	yr = srot(2,1)*xq + srot(2,2)*yq + srot(2,3)*zq
	zr = srot(3,1)*xq + srot(3,2)*yq + srot(3,3)*zq
	xs = xr * scale + sxcent
	ys = yr * scale + sycent
	zs = zr * scale
	rs = radlim * scale
	sdtail(1) = xs
	sdtail(2) = ys
	sdtail(3) = zs
	sdtail(4) = rs
*	tally shadow tiles the object might impinge on
	ixlo = (xs-rs) / npx + 1
	ixhi = (xs+rs) / npx + 1
	iylo = (ys-rs) / npy + 1
	iyhi = (ys+rs) / npy + 1
	if (ixlo.lt.1)   ixlo = 1
	if (ixlo.gt.NSX) goto 209
	if (ixhi.lt.1)   goto 209
	if (ixhi.gt.NSX) ixhi = NSX
	if (iylo.lt.1)   iylo = 1
	if (iylo.gt.NSY) goto 209
	if (iyhi.lt.1)   goto 209
	if (iyhi.gt.NSY) iyhi = NSY
	do iy = iylo,iyhi
	do ix = ixlo,ixhi
	    MOUNT(ix,iy) = MOUNT(ix,iy) + 1
	enddo
	enddo
*	transform QQ into shadow space as well
	call tmul4( qt, qp, srtinv )
	call tmul4( qp, srtinvt, qt )
	sdtail(5)  = qp(1,1)
	sdtail(6)  = qp(2,2)
	sdtail(7)  = qp(3,3)
	sdtail(8)  = qp(1,2)
	sdtail(9)  = qp(2,3)
	sdtail(10) = qp(1,3)
	sdtail(11) = qp(1,4)
	sdtail(12) = qp(2,4)
	sdtail(13) = qp(3,4)
	sdtail(14) = qp(4,4)
*
 209	continue
*
* Done with input
*
	return
	end


************************************************************************
* This routine does the exact test for pixel impingement of a quadric  *
* in both pixel and shadow space.                                      *
* Find Z coordinate of point on quadric surface with given X, Y        *
* Also return surface normal at that point                             *
************************************************************************
CCC
CC
C
	function qtest( center, coeffs, xp, yp, zp, qnorm, shadowspace,
     &                  backside )
	IMPLICIT NONE
	logical  qtest, shadowspace
	logical  backside
	real    center(3), coeffs(10), xp, yp, zp, qnorm(3)
*
      COMMON /MATRICES/ XCENT, YCENT, SCALE, EYEPOS, SXCENT, SYCENT,
     &                  TMAT, TINV, TINVT, SROT, SRTINV, SRTINVT
     &                 ,RAFTER, TAFTER
      REAL   XCENT, YCENT, SCALE, EYEPOS, SXCENT, SYCENT
      REAL   TMAT(4,4), TINV(4,4),   TINVT(4,4) 
      REAL   SROT(4,4), SRTINV(4,4), SRTINVT(4,4)
      REAL   RAFTER(4,4), TAFTER(3)
*
	real    QA,QB,QC,QD,QE,QF,QG,QH,QI,QJ
	real    AA,BB,CC,DD,EE
	real    x,y,z
*
	QA = coeffs(1)
	QB = coeffs(2)
	QC = coeffs(3)
	QD = coeffs(4)
	QE = coeffs(5)
	QF = coeffs(6)
	QG = coeffs(7)
	QH = coeffs(8)
	QI = coeffs(9)
	QJ = coeffs(10)
*
*	xp and yp are in pixel coordinates, rather than in normalized ones
*	First displace center of quadric surface from the implicit origin
*	to the point specified by center(3), then convert to pixel coords
*
	x = (xp - center(1)) / scale
	y = (yp - center(2)) / scale
*
	AA = QC
	BB = 2.0 * (QF*x + QE*y + QI)
	CC = QA*x*x + QB*y*y + 2.0*(QD*x*y + QG*x + QH*y) + QJ
	DD = BB*BB - 4*AA*CC
	if (DD.LT.0) then 
	    qtest = .false.
	    return
	else
	    qtest = .true.
	    EE = sqrt(DD)
	endif
	if (AA .eq. 0.) then
CDEBUG	    Can this happen????
	    z = 9999.
	else if (backside) then
	    if (AA .le. 0.) then
	    	z = (-BB + EE) / (2*AA)
	    else
	    	z = (-BB - EE) / (2*AA)
	    endif
	else
	    if (AA .gt. 0.) then
	    	z = (-BB + EE) / (2*AA)
	    else
	    	z = (-BB - EE) / (2*AA)
	    endif
	endif
	zp = z * scale
	zp = zp + center(3)
*
*	Surface normal comes from partial derivatives at this point
	if (.not. shadowspace) then
	    qnorm(1) = QA*x + QD*y + QF*z + QG
	    qnorm(2) = QB*y + QD*x + QE*z + QH
	    qnorm(3) = QC*z + QF*x + QE*y + QI
	endif
*
	return
	end

CCC	Convert ANISOU description of anisotropic displacement parameters
CC	into quadric surface enclosing a given probability volume
C	Returns -1 if the Uij are non-positive definite, 1 otherwise
C
	function anitoquad( ANISOU, PROB, QUADRIC, EIGENS, EVECS)
	implicit NONE
	integer  anitoquad
	real     ANISOU(6), PROB, QUADRIC(10)
	real     EIGENS(3), EVECS(4,4)
c
	COMMON /ASSCOM/ noise, verbose
	integer         noise
	logical         verbose
c
	real	UU(4,4), UINV(4,4)
	integer i
c
	real     tinv4, det
c
c	Save FPU traps later by checking that ANISOU is non-zero
	do i = 1,3
	    EIGENS(i) = 0.0
	end do
	if (ANISOU(1).eq.0.and.ANISOU(2).eq.0.and.ANISOU(3).eq.0 .and.
     &      ANISOU(4).eq.0.and.ANISOU(5).eq.0.and.ANISOU(6).eq.0) then
     		anitoquad = -2
		return
	end if
c
c	Build matrix from Uij coefficients and invert it
	UU(1,1) = ANISOU(1)
	UU(2,2) = ANISOU(2)
	UU(3,3) = ANISOU(3)
 	UU(4,4) = -(1/PROB**2)
	UU(1,2) = ANISOU(4)
	UU(2,1) = ANISOU(4)
	UU(2,3) = ANISOU(6)
	UU(3,2) = ANISOU(6)
	UU(1,3) = ANISOU(5)
	UU(3,1) = ANISOU(5)
	UU(1,4) = 0.0
	UU(4,1) = 0.0
	UU(2,4) = 0.0
	UU(4,2) = 0.0
	UU(3,4) = 0.0
	UU(4,3) = 0.0
	det = tinv4( UINV, UU )
c
c	and from that we extract the coefficients of the surface
	QUADRIC(1)  = UINV(1,1)
	QUADRIC(2)  = UINV(2,2)
	QUADRIC(3)  = UINV(3,3)
	QUADRIC(4)  = UINV(1,2)
	QUADRIC(5)  = UINV(2,3)
	QUADRIC(6)  = UINV(1,3)
	QUADRIC(7)  = UINV(1,4)
	QUADRIC(8)  = UINV(2,4)
	QUADRIC(9)  = UINV(3,4)
	QUADRIC(10) = UINV(4,4)
c
c	Find eigenvalues of the ellipsoid
	call jacobi( UU,   3, 4, EIGENS, EVECS )
c
c	Units of this matrix are A**2; we want values in A
	anitoquad = 1
	do i = 1,3
	    if (EIGENS(i).gt.0.) then
		EIGENS(i) = sqrt(EIGENS(i))
	    else
C		write (noise,*) 'Non-positive definite ellipsoid!'
		EIGENS(i) = 0.
		anitoquad = -1
	    endif
	enddo
c
	return
	end

CCC	Matrix manipulation routines for 4x4 homogeneous transformations
CC
C
	subroutine tmul4( C, A, B )
	real   C(4,4), A(4,4), B(4,4)
	integer i,j
	do i = 1,4
	do j = 1,4
	    C(i,j) = A(i,1)*B(1,j) + A(i,2)*B(2,j)
     &		   + A(i,3)*B(3,j) + A(i,4)*B(4,j)
	enddo
	enddo
	return
	end

	subroutine trnsp4( B, A )
	real   B(4,4), A(4,4)
	integer i,j
	do i=1,4
	do j=1,4
	    B(i,j) = A(j,i)
	enddo
	enddo
	return
	end

CCC	Matrix inversion for a 4x4 matrix A
CC
C
	function tinv4( AI, A )
	real     tinv4
	real     AI(4,4), A(4,4)
c
	real	TMP(4,4), D
	integer index(4)
c
	do i=1,4
	    do j=1,4
		TMP(i,j) = A(i,j)
		AI(i,j) = 0.
	    enddo
	    AI(i,i) = 1.
	enddo
	call ludcmp( TMP, 4, index, D )
	tinv4 = D * TMP(1,1)*TMP(2,2)*TMP(3,3)*TMP(4,4)
	do j=1,4
	    call lubksb( TMP, 4, index, AI(1,j) )
	enddo
	return
	end

************************************************************************
*              Matrix inversion via LU decomposition                   *
*	adapted from Numerical Recipes in Fortran (1986)               *
************************************************************************
*
CCC	input  NxN matrix A is replaced by its LU decomposition
CC	output index(N) records row permutation due to pivoting
C	output D is parity of row permutations 
c
	subroutine ludcmp( A, n, index, D )
	implicit  NONE
	integer   n,index(n)
	real      A(n,n)
	real      D
c
	integer   i,imax,j,k
	real      aamax,dum,sum
	integer    NMAX
	parameter (NMAX=10)
	real      vv(NMAX)
c
	d = 1.
	do i=1,n
	    aamax = 0.
	    do j=1,n
	    	if (abs(A(i,j)).gt.aamax) aamax = abs(A(i,j))
	    enddo
	    call assert(aamax.ne.0.,'Singular matrix')
	    vv(i) = 1. / aamax
	enddo
c
	do j=1,n
	    if (j.gt.1) then
		do i=1,j-1
		    sum = A(i,j)
		    if (i.gt.1) then
			do k=1,i-1
			    sum = sum - A(i,k)*A(k,j)
			enddo
			A(i,j) = sum
		    endif
		enddo
	    endif
	    aamax = 0.
	    do i=j,n
		sum = A(i,j)
		if (j.gt.1) then
		    do k=1,j-1
			sum = sum - A(i,k)*A(k,j)
		    enddo
		    A(i,j) = sum
		endif
		dum = vv(i) * abs(sum)
		if (dum.ge.aamax) then
		    imax = i
		    aamax = dum
		endif
	    enddo
	    if (j.ne.imax) then
		do k=1,n
		    dum = A(imax,k)
		    A(imax,k) = A(j,k)
		    A(j,k) = dum
		enddo
		d = -d
		vv(imax) = vv(j)
	    endif
	    index(j) = imax
	    call assert(A(j,j).ne.0.,'Singular matrix')
	    if (j.ne.n) then
		dum = 1. / A(j,j)
		do i=j+1,n
		    A(i,j) = A(i,j) * dum
		enddo
	    endif
	enddo
	return
	end

CCC	corresponding back-substitution routine
CC
C
	subroutine lubksb( A, N, index, B )
	implicit NONE
	integer  n, index(n)
	real     A(n,n), B(n)
c
	integer  i,ii,j,ll
	real     sum
c
	ii = 0
	do i=1,n
	    ll = index(i)
	    sum = B(ll)
	    B(ll) = B(i)
	    if (ii.ne.0) then
		do j=ii,i-1
		    sum = sum - A(i,j)*B(j)
		enddo
	    else if (sum.ne.0.) then
		ii = i
	    endif
	    B(i) = sum
	enddo
c
	do i=n,1,-1
	    sum = B(i)
	    if (i.lt.n) then
		do j=i+1,n
		    sum = sum - A(i,j)*B(j)
		enddo
	    endif
	    B(i) = sum / A(i,i)
	enddo
	return
	end

************************************************************************
* Find eigenvalues and eigenvectors of nxn symmetric matrix using      *
* cyclic Jacobi method. NP is (Fortran) physical storage dimension.    *
* On return A is destroyed, D contains eigenvalues,and each column of  *
* V is a normalized eigenvector                                        *
* Adapted from Numerical Recipes in Fortran (1986)                     *
* We only need it for 3x3 symmetric matrices, so it's overkill.        *
************************************************************************
CCC
CC
C
	subroutine jacobi( A, n, np, D, V )
	PARAMETER (NMAX=4)
c	Machine dependent! (converge when off-diagonal sum is less than this)
	PARAMETER (TINY = 1.e-37)
	real A(np,np), D(n), V(np,np)
	real B(NMAX), Z(NMAX)
c
	call assert(n.le.NMAX,'Matrix too big for eigenvector routine')
c
c	Initialize V to identity matrix, B and D to diagonal of A
	do ip = 1,n
	do iq = 1,n
	    V(ip,iq) = 0.0
	enddo
	V(ip,ip) = 1.0
	B(ip) = A(ip,ip)
	D(ip) = B(ip)
	Z(ip) = 0.0
	enddo
	nrot = 0
c
c	50 sweeps is never expected to happen; Press et al (1986) claim
c	6 to 10 are typical for moderate matrix sizes
c	Empirical trials of rastep show 6 sweeps, 8-10 rotations
	do i = 1,50
	    sm = 0.0
	    do ip = 1,n-1
	    do iq = ip+1,n
		sm = sm+abs(A(ip,iq))
	    enddo
	    enddo
	    if (sm .lt. TINY) return
c	    After 4 sweeps skip rotation if the off-diagonal is small
	    if (i .lt. 4) then
		thresh = 0.2*sm/(n*n)
	    else
		thresh = 0.0
	    endif
	    do ip = 1,n-1
	    do iq = ip+1,n
		g = 100. * abs(A(ip,iq))
		if ((i.gt.4) .and.
     &		    (abs(D(ip)) + g .eq. abs(D(ip))) .and.
     &		    (abs(D(iq)) + g .eq. abs(D(iq)))) then
		    A(ip,iq) = 0.0
		else if (abs(A(ip,iq)).gt.thresh) then
		    h = D(iq) - D(ip)
		    if (abs(h) + g .eq. abs(h)) then
			t = A(ip,iq) / h
		    else
			theta = 0.5 * h / A(ip,iq)
			t = 1. / (abs(theta) + sqrt(1.+theta*theta))
			if (theta.lt.0.) t = -t
		    endif
		    c = 1./sqrt(1.+t*t)
		    s = t * c
		    tau = s/(1.+c)
		    h = t * A(ip,iq)
		    Z(ip) = Z(ip) - h
		    Z(iq) = Z(iq) + h
		    D(ip) = D(ip) - h
		    D(iq) = D(iq) + h
		    A(ip,iq) = 0.0
		    do j = 1,ip-1
			g = A(j,ip)
			h = A(j,iq)
			A(j,ip) = g - s*(h+g*tau)
			A(j,iq) = h + s*(g-h*tau)
		    enddo
		    do j = ip+1,iq-1
			g = A(ip,j)
			h = A(j, iq)
			A(ip,j) = g - s*(h+g*tau)
			A(j,iq) = h + s*(g-h*tau)
		    enddo
		    do j = iq+1,n
			g = A(ip,j)
			h = A(iq,j)
			A(ip,j) = g - s*(h+g*tau)
			A(iq,j) = h + s*(g-h*tau)
		    enddo
		    do j = 1,n
			g = V(j,ip)
			h = V(j,iq)
			V(j,ip) = g - s*(h+g*tau)
			V(j,iq) = h + s*(g-h*tau)
		    enddo
		    nrot = nrot + 1
		endif
	    enddo
	    enddo
c
c	    Update D with sum and reinitialize Z
	    do ip = 1,n
		B(ip) = B(ip) + Z(ip)
		D(ip) = B(ip)
		Z(ip) = 0.0
	    enddo
	enddo
c
	call assert(.false.,'Failed to find eigenvectors')
	return
	end
c	


C	This one really has nothing to do with quadrics per se,
C	but since it's invoked by qinp, both render and rastep
C	need to be able to see it.
C	Should really be in separate file of support routines
C
	FUNCTION PERSP( Z )
	REAL PERSP, Z
	COMMON /MATRICES/ XCENT, YCENT, SCALE, EYEPOS, SXCENT, SYCENT,
     &                  TMAT, TINV, TINVT, SROT, SRTINV, SRTINVT
     &                 ,RAFTER, TAFTER
	REAL   XCENT, YCENT, SCALE, EYEPOS, SXCENT, SYCENT
	REAL   TMAT(4,4), TINV(4,4),   TINVT(4,4) 
	REAL   SROT(4,4), SRTINV(4,4), SRTINVT(4,4)
	REAL   RAFTER(4,4), TAFTER(3)
	IF (Z/EYEPOS .GT. 0.999) THEN
	    PERSP = 1000.
	ELSE
	    PERSP = 1. / (1. - Z/EYEPOS)
	ENDIF
	RETURN
	END

