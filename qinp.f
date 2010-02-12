************************************************************************
*              Support routines for quadric surfaces                   *
************************************************************************
* EAM Jun 1997	- initial version, supports version 2.4(alpha) of render 
* EAM May 1998	- additional error checking to go with Parvati/rastep
* EAM Jan 1999	- version 2.4i
* EAM Mar 2008	- Gfortran optimization breaks the object accounting.
*		  No solution yet.  Break qinp.f into separate file
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
*	- fix optimization problems with qinp
************************************************************************

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
	real    xq, yq, zq, radlim, red, grn, blu
	real    xc, yc, zc, rc
	real    xr, yr, zr, xs, ys, zs, rs
	real    pfac
*
	integer  ix,iy,ixlo,ixhi,iylo,iyhi
c	VOLATILE ix,iy,ixlo,ixhi,iylo,iyhi
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

