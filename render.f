      PROGRAM RENDER
*
*     Version 2.6f (15 May 2002)
*
* EAM May 1990	- add object type CYLIND (cylinder with rounded ends)
*		  and CYLFLAT (cylinder with flat ends)
*		- use Phong shading on triangles if they are sequentially
*		  adjoining.
* EAM Feb 1991	- port to Ultrix (minor changes) and modify output format
*		  to depend on code in separate module "local".
* EAM Mar 1993	- fix embarrassingly stupid bug in cylinder shadowing
*		  add object type PLANE (triangle with infinite extent)
* EAM Nov 1993	- Version 2.0 (beta test)
*		  fix bug which allowed objects to shadow themselves
*		  Command line options for 3 output modes
* EAM Apr 1994	- Version 2.0 release
*		  TIFF output support in local.c
*		  minor changes to fortran source to make IBM xlf compiler happy
* EAM Sep 1994	- EXPERIMENTAL VERSION WITH OBJECT TYPES 7, 8, 9
* EAM Jan 1995	- move DATA statement to make linux happy
* EAM Mar 1995	- fix bug in routine CYLTEST which took bites out of cylinder ends
* EAM May 1995	- fold object types 7/8/9 back into distributed code for V2.1
* EAM Jan 1996	- Version 2.2
*		  Add code for transparency and faster MATERIAL bookkeeping
*		  Also fix major problems with explicit surface normals
*		  object type 8 expanded to describe transparency
* EAM May 1996	- antialiasing scheme 4, file indirection, 
*		  minor changes to accommodate HPUX
* EAM Oct 1996  - trap and forgive shadowing error due to too small NSX,NSY
* EAM Nov 1996	- scheme 0 causes alpha blend channel in output image
*		  per-tile count of transparent objects
* EAM Jan 1997	- zero length cylinders treated as spheres
*		  Object types 10 + 11 (fonts and labels) accepted but ignored
*		  Object type  12 reserved for other label information
*		  Material properties can override object colors
* 		- Material OPT(1) = 1 transparency option
*		  OPT(4) = continuation lines for more material properties
* EAM Mar 1997	- Make SLOP larger, and dependent on tile size
* 		- GLOW light source specified by object type 13
* EAM May 1997	- V2.3c allow multiple glow lights; make cyltest a function;
*		  V2.3d remove DATA statements; more terse output; 
*		  BACKFACE material option; EYEPOS = 0.0 disables perspective
* EAM Jul 1997	- add commons LISTS MATRICES NICETIES RASTER
* 		- D_LINES code for quadric surfaces, ISOLATION 
* EAM Sep 1997	- fix normals of flat cylinder ends (thanks to Takaaki Fukami)
* EAM Nov 1997	- add VERTEXRGB object type to extend triangle descriptions,
*		  allow # as comment delimiter in input stream
* EAM Dec 1997	- Release V2.4b
* EAM Feb 1998	- fixed bug in check against limiting radius of quadrics
* EAM Jul 1998	- Object type 16 (GLOBAL PROPERTIES); fog
* EAM Aug 1998	- render back side of transparent flat-ended cylinders
*		  by duplicating object with INSIDE flag bit set.
* EAM Oct 1998	- check environmental variable R3D_LIB for input file indirection
*		- V2.4g includes some preliminary code to support Z-clipping
* EAM Nov 1998	- more work on Z-clipping
* EAM Feb 1999	- re-work output module local.c to support -jpeg and -out
* EAM May 1999	- allow explicit vertex colors for cylinders also
* EAM Jul 1999	- 2.4l preliminary work towards an after-the-fact rotation option
* EAM Sep 1999	- 2.5a command line parsing in separate routine parse()
*		  label processing folded into render; routines in r3dtops.f
* EAM Jan 2000	- 2.5b general release
* EAM Feb 2000	- 2.5c (bug fixes to 2.5b)
* EAM Mar 2000	- object types VTRANSP (18) and ISOLATE2 (19)
* EAM Sep 2000	- 2.5e uncompress indirect files ending with .Z or .gz
*		  discard BACKCLIP objects on input, implement BACKCLIP material
* EAM Nov 2000	- 2.5f more bug-fixes to rotation of surface normals
* EAM Feb 2001	- 2.5g command line shadows
* EAM Mar 2001	- V2.6 (alpha test)
*		  bounding planes
*		  revamped transparency code, remove limit of 2 stacked objects
*		  break out shared maximum dimensions to paramters.incl
*		  make back surface HIDING (former INMODE=4) the default for
*		  	non-bounded opaque triangles 
* EAM Jul 2001	- V2.6b first release
* EAM Aug 2001	- V2.6c bug-fix for MOPT1 processing
*		  PNG output format ( -DPNG_SUPPORT )
* EAM Feb 2002	- allow a little RIBBONSLOP in testing for ribbon triangles
*		  fix up complicated corner cases in bounded surface algorithm
* EAM Apr 2002	- clean up auto-tiling, and allow zero NPX or NPY to trigger it
*		  
*     General organization:
*
*     - read in control parameters and initial output image file
*     - read in list of objects
*       - count objects that may impinge on each tile
*       - do this for both pixel and rotated "shadow" space
*     - sort objects
*     - go through main object list in sorted order
*       - fill in short lists of objects
*     - repeat the sort etc. for the objects in shadow space
*     - that's it for the "cheap" part
*     - for each tile:
*       - for each pixel:
*         - search objects to find highest point for pixel
*	  - if it's transparent find the next one down as well
*         - transform resulting (x,y,z) to shadow space
*         - find closest z' for new x',y'
*         - this tells you if the pixel is in shadow or not
*         - shade accordingly
*       - copy tile to output buffer
*
*
*     Easy-to-change constants (kept in file parameters.incl):
*
*     - maximum number of tiles in each direction  MAXNTX, MAXNTY
*     - number of shadow tiles in each direction   NSX, NSY
*     - maximum number of pixels in a tile         MAXNPX, MAXNPY
*     - maximum number of objects                  MAXOBJ
*     - maximum number of material specifications  MAXMAT
*     - maximum depth of stack transparent objects MAXTRANSP
*
*     Input (line by line except where noted):
*
*     - TITLE    anything you like
*     - NTX,NTY  tiles in each direction
*     - NPX,NPY  pixels per tile to compute in each direction
*     - SCHEME   pixel averaging scheme (1, 2, or 3)
*       - 0 no anti-aliasing, use alpha channel
*       - 1 no anti-aliasing, no alpha channel
*       - 2 means 2x2 computing pixels for 1 output pixel
*       - 3 means 3x3 computing pixels for 2x2 output pixels
*	- 4 same as 3, but NTX,NTY expanded inside program
*     - BKGND    background colour (r,g,b in range 0 to 1)
*     - SHADOW   "shadow mode?" (T or F)
*     - IPHONG   Phong power (e.g., 20)
*     - STRAIT   straight-on (2ndary) light component (e.g., 0.1)
*     - AMBIEN   ambient light component (e.g., 0.05)
*     - SPECLR   specular reflection component (e.g., 0.30)
*     - EYEPOS   eye position along +z coordinate (e.g., 4)
*       - relative to 1=narrow dimension of screen
*       - used for perspective, EYEPOS = 0.0 disables perspective
*     - SOURCE   main light source position (x,y,z components)
*       - vector length ignored, point source is at infinity
*     - TMAT     global transformation on input objects
*       - postfix 4x4 matrix on 4 lines, as you would write it
*       - upper left 3x3 must be pure rotation
*       - lower left 1x3 is translation
*       - lower right 1x1 is global scaling (reduction)
*       - upper right 3x1 causes extra perspective (should be 0)
*       - applies to homogeneous co-ordinates (x,y,z,1)
*       - radii are only scaled down by global scaling TMAT(4,4)
*     - INMODE   object input mode (1, 2, or 3)
*       - mode 1:  all objects are triangles
*       - mode 2:  all objects are spheres
*       - mode 3:  each object will be preceded by type
*     - INFMT or INFMTS   object input format(s), 1 per line
*       - one format for modes 1 and 2, or three for mode 3
*       - each is fortran format in parentheses, or single *
*       - for 3 formats, the order of formats and details is:
*         - triangle:  x1,y1,z1,x2,y2,z2,x3,y3,z3,r,g,b
*         - sphere:    x,y,z,radius,r,g,b
*         - trcone:    x1,y1,z1,rad1,x2,y2,z2,rad2,r,g,b
*	  - cylinder:  as truncated cone, but 2nd radius ignored
*     - objects
*       - modes 1,2:  each object starts on a new line
*         - read according to the single format given
*       - mode 3:  each object is preceded by a line giving type
*         - type  1: triangle (to be read with 1st format)
*         - type  2: sphere (to be read with 2nd format)
*         - type  3: cylinder with rounded ends (3rd format)
*         - type  4: [not implemented: truncated cone]
*         - type  5: cylinder with flat ends (3rd format)
*         - type  6: plane (=triangle with infinite extent) (1st format)
*         - type  7: normal vectors for previous triangle (1st format)
*         - type  8: material definition which applies to subsequent objects
*         - type  9: end previous material
*         - type 10: font selection (ignored in render)
*         - type 11: label (ignored other than to count them)
*         - type 12: (reserved for additional label processing)
*         - type 13: glow light source
*	  - type 14: quadric surface (usually an ellipsoid)
*	  - type 15: disable coordinate transformation of subsequent objects
*	  - type 16: global properties (e.g. FOG)
*	  - type 17: RGB triple for each vertex of preceding object
*	  - type 18: transparency at each vertex of preceding object
*	  - type 19: variant of type 15; forces unitary coordinate sytem
*         - type  0: no more objects (equivalent to eof)
*
*-----------------------------------------------------------------------------
* EAM Sep 1994
*
*	1) Object type 7 signals an extra record giving explicit vertex normals
*	for a single triangle. This extra record must directly follow the 
*	corresponding triangle and uses the same format.
*
*	2) Object type 8 signals an extra record giving extra or more explicit
*	material properties object.  Current (trial) contents of record are:
*	    MPHONG	- overrides global Phong power for specular reflections
*	    MSPEC	- overrides global specular scattering contribution
*	    SR,SG,SB	- red/green/blue components for specular highlighting
*			  (values <0 cause highlights to match object colour)
*           CLRITY	- 0.0 (opaque) => 1.0 (transparent)
*	    CLROPT	- [reserved] suboptions for transparency handling
*	    OPT2	- [reserved] suboptions for bounding planes
*	    OPT3	- [reserved]
*	    OPT4	- # of additional modifier records immediately following
*	These material properties remain in effect for subsequent objects 
*	until object type 9 appears to terminate the effect.
*
*	3) Object type 9 terminates effect of previous materials property
*
*-----------------------------------------------------------------------------
*	Object types 10 and 11 are used for specifying labels.
*	Label object types are
*	  - type 10: Font_Name size alignment
*	  - type 11: XYZ RGB on first line
*		     label (ascii characters enclosed in quotes) on second line
*	Object type 12 is reserved to go with this, as I have a nagging
*	suspicion more information may turn out to be necessary.
*-----------------------------------------------------------------------------
*	Object type 13 specifies a "glow" light source; i.e. a non-shadowing
*	light source with finite origin GLOWSRC and illumination range GLOWRAD.
*	Specular highlights for this source specified by GLOWCOL and GPHONG.
*	0.0 < GLOW < 1.0   = contribution of glow to total lighting model.
*	13
*	 GLOWSRC(3)  GLOWRAD  GLOW  GOPT GPHONG  GLOWCOL(3)
*-----------------------------------------------------------------------------
* V2.4
*	Object type 14 specifies a quadric surface
*	    QQ = Ax^2 + By^2 + Cz^2 + 2Dxy + 2Eyz + 2Fxz + 2Gx + 2Hy + 2Iz + J
*	centered at XC,YC,ZC and truncated at bounding radius RC. Supporting
*	code is in file quadric.f
*	14
*	  XC YC ZC RC  RED GRN BLU
*	  A B C D E F G H I J
*
*	Object type 15 is a single line signaling that subsequent objects are
*	not to be transformed by the TMAT matrix in the header. This isolation
*	from TMAT is terminated by an end material record (object type 9).
*-----------------------------------------------------------------------------
* V2.6
*	Object type 4 is used internally to implement bounding planes.
*	The BOUNDING_PLANE definition is given in the input stream as
*	a modifier to a MATERIAL descriptor.  At the time it is read in
*	render creates a new object of type 4 to hold the bounding 
*	plane parameters and modifiers.
*	DETAIL(K+...) is loaded with the following parameters:
*	SUBTYPE, BPTYPE, X,Y,Z, XNORM, YNORM, ZNORM, RED, GREEN, BLUE
*-----------------------------------------------------------------------------
*
*     Object space convention:
*
*     - this is the space TMAT is supposed to map your data to
*     - centre of "virtual screen" is (0,0,0)
*     - x to right, y to top, z towards viewer
*     - the smaller of the x and y dimensions goes from -.5 to +.5
*     - z cuts off at +1 and -1 by default, but is modified by FRONTCLIP, BACKCLIP
*     - shadow box dimensions determined by NSX/NTX, NSY/NTY
*
*-----------------------------------------------------------------------------
* David Bacon's comments from Version 1.0
*
*     Bugs:
*
*     - perspective is applied to raw objects, giving wrong lighting
*     - perspective unity factor is always at z=0 in object space
*     - shadow box doesn't necessarily enclose entire view prism
*     - some ASSERT calls are commented out for extra speed
*     - SLOP parameter is empirical fix to imprecision in shadowing
*
*     Deferred priorities:
*
*     - superior pixel averaging (you should use another pass?)
*     - better assignment of triangles to tiles (do clipping?)
*     - better estimate of max. triangle elevation within tile
*
*
*     Why I don't do shadowing properly:
*
*       Although you might not notice it as a casual observer,
*     the main light source is (in effect) in different places
*     for different parts of the picture.  More precisely,
*     perspective is applied to the objects comprising the
*     picture first, and THEN the lighting from a distant
*     light source is applied.  The lighting would be correct
*     if there were no perspective, because the angle doesn't
*     change across the picture.  With a scene in perspective,
*     however, the angle of the light beam, from the eye's
*     viewpoint, should vary a little.
*       The reason I allow this error is because the use of "tiles"
*     is implicitly a parallel projection, so I have to apply the
*     perspective to the objects initially.
*       Conceivably I could "undo" this whenever taking the light
*     source point of view (this would result in using a slightly
*     different light source position for different parts of the
*     picture), but that would cause another problem:
*       Perspective distorts spheres differently depending on whether
*     you ask about each point on the surface individually or use one
*     perspective factor for all points based on the centre.  Consider
*     even a sphere in the plane where perspective is supposed to be
*     unity (z=0 for us).  It swells slightly when perspective is
*     applied point by point.
*       This is a serious problem for us because although I
*     generate non-swelled spheres by applying the perspective to
*     all of them initially, I end up asking about individual
*     points on them when wanting the light source point of view.
*     You might argue that I could simply calculate the amount
*     of swelling that has to be accounted for (by drawing tangents
*     from the eye and seeing where they intersect the constant-z
*     plane that passes through the sphere centre or something),
*     but the "swelling" is complicated in the sense that it is
*     in effect a "stretching" of the surface closest to the eye
*     and a "shrinking" in back.
*       Even if I could see how to compensate for all this, I
*     don't think it would be worth it.
*       It's probably not even worth trying to implement a better
*     approximate solution by changing the light source position
*     slightly for each object in the picture.  The ambiguity
*     as to whether the obscuring or the obscured object should have
*     the modified light source position applied would make it
*     difficult to assign objects to shadow tiles.  Trying to
*     implement full "antiperspective" for the light source would
*     just shift the problems of perspective (swelling spheres,
*     etc.) to a different locale without solving them.
*-----------------------------------------------------------------------------
*
*     Overkill:
      IMPLICIT REAL   (A-H, O-Z)
*
      INCLUDE 'VERSION.incl'
*
*     I/O units for control input, info output, label processing
      INTEGER INPUT, INPUT0, NOISE, LUNIT
      PARAMETER (INPUT0=5, NOISE=0, LUNIT=4)
*
*     Descriptor codes for the various object types
      INTEGER TRIANG, SPHERE, INTERNAL, CYLIND, CYLFLAT
      INTEGER PLANE, QUADRIC, MXTYPE, FONT, GLOWLIGHT
      INTEGER NORMS, VERTEXRGB, VERTRANSP
      PARAMETER (TRIANG   = 1)
      PARAMETER (SPHERE   = 2)
      PARAMETER (CYLIND   = 3)
      PARAMETER (INTERNAL = 4)
      PARAMETER (CYLFLAT  = 5)
      PARAMETER (PLANE    = 6)
      PARAMETER (NORMS    = 7)
      PARAMETER (MATERIAL = 8)
      PARAMETER (MATEND   = 9)
      PARAMETER (FONT     = 10, LABEL = 11)
      PARAMETER (GLOWLIGHT= 13)
      PARAMETER (QUADRIC  = 14)
      PARAMETER (ISOLATE1 = 15)
      PARAMETER (GPROP    = 16)
      PARAMETER (VERTEXRGB= 17)
      PARAMETER (VERTRANSP= 18)
      PARAMETER (ISOLATE2 = 19)
      PARAMETER (MXTYPE   = 19)
*
*     Bit definitions for FLAG array
      INTEGER    FLAT,      RIBBON,    SURFACE,   PROPS
      PARAMETER (FLAT=2,    RIBBON=4,  SURFACE=8, PROPS=16)
      INTEGER    TRANSP,    HIDDEN,    INSIDE,    MOPT1
      PARAMETER (TRANSP=32, HIDDEN=64, INSIDE=128,MOPT1=256)
      INTEGER	 VCOLS,     CLIPPED,      VTRANS,      BOUNDED
      PARAMETER	(VCOLS=512, CLIPPED=1024, VTRANS=2048, BOUNDED=4096)
*
*     Bit definitions for OTMODE passed to local(1,...)
      INTEGER    ALPHACHANNEL
      PARAMETER (ALPHACHANNEL=32)
*
*     $$$$$$$$$$$$$   ARRAY SIZE LIMITS $$$$$$$$$$$$$$
*
      INCLUDE 'parameters.incl'
      INTEGER OUTSIZ
      PARAMETER (OUTSIZ = MAXNTX*MAXNPX*MAXNPY)
*
*
*     Command line options (Aug 1999) NB: nax,nay,quality MUST be integer*2
      COMMON /OPTIONS/ FONTSCALE, GAMMA, ZOOM, NSCHEME, SHADOWFLAG, 
     &                 NAX, NAY, OTMODE, QUALITY, INVERT, LFLAG
      REAL             FONTSCALE, GAMMA, ZOOM
      INTEGER          NSCHEME, SHADOWFLAG
      INTEGER*2        NAX, NAY, OTMODE, QUALITY
      LOGICAL*2        INVERT, LFLAG
*
*     Title for run
      CHARACTER*132 TITLE
*
*     Number of tiles, pixels per tile
      COMMON /RASTER/ NTX,NTY,NPX,NPY
      INTEGER         NTX,NTY,NPX,NPY
*
*     Pixels per tile after anti-aliasing, output buffer line length
      INTEGER         NOX, NOY, NX
*
*     Actual image size in pixels (may include partial tiling at the edges)
*     (MUST BE INTEGER*2 for call to local()!!!)
C     INTEGER*2 NAX, NAY
*
*     One lonely tile
      REAL TILE(3,MAXNPX,MAXNPY)
*
*     With an alpha blend channel
      REAL ACHAN(MAXNPX,MAXNPY)
*
*     Pixel averaging scheme
      INTEGER SCHEME
*
*     Background colour
      REAL   BKGND(3)
*
*     "Shadow mode?"
      LOGICAL SHADOW
      INTEGER NSXMAX,NSYMAX
*
*     Phong power
      INTEGER IPHONG
*
*     Straight-on (secondary) light source contribution
      REAL   STRAIT
*
*     Ambient light contribution
      REAL   AMBIEN
*
*     Specular reflection component
      REAL   SPECLR
*
*     Primary light source position
      REAL   SOURCE(3)
*
*     Input transformation
      COMMON /MATRICES/ XCENT, YCENT, SCALE, EYEPOS, SXCENT, SYCENT,
     &                  TMAT, TINV, TINVT, SROT, SRTINV, SRTINVT
     &                 ,RAFTER, TAFTER
      REAL   XCENT, YCENT, SCALE, SXCENT, SYCENT
*     Transformation matrix, inverse of transpose, and transposed inverse
      REAL   TMAT(4,4), TINV(4,4), TINVT(4,4)
*     Shortest rotation from light source to +z axis
      REAL   SROT(4,4), SRTINV(4,4), SRTINVT(4,4)
*     Post-hoc transformation on top of original TMAT
      REAL   RAFTER(4,4), TAFTER(3)
      EXTERNAL DET
      REAL     DET
*
*     Distance (in +z) of viewing eye
      REAL   EYEPOS
*
*     Input mode
      INTEGER INMODE
*
*     Buffer one line of input for decoding
      CHARACTER*132 LINE
*
*     Input format(s)
      CHARACTER*80 INFMTS(MXTYPE),INFMT
*
*     Free-format input flag
      LOGICAL INFLGS(MXTYPE),INFLG
*
*     Allow very long names for file indirection
      CHARACTER*128 FULLNAME
*
*     Stuff for shading
      REAL   NL(3),NORMAL(3),LDOTN
      REAL   RGBCUR(3),RGBSHD(3),RGBFUL(3)
      REAL   SPECOL(3)
*
*     FOG parameters 
*     (fogtype  -1 = none, 0 = linear depthcuing, 1 = exponential model)
*     (fogfront  0 = front object, else fraction of front clipping plane)
*     (fogback   0 = back object,  else fraction of back clipping plane)
*
      COMMON /FOGCOM/ FOGTYPE,FOGFRONT,FOGBACK,FOGDEN,FOGLIM,FOGRGB
      INTEGER FOGTYPE
      REAL    FOGFRONT, FOGBACK, FOGDEN, FOGLIM(2), FOGRGB(3)
*
*     The s & m guys are for the shadow box in the following
*
*     Object list, consists of pointers (less 1) into detail, sdtail
      INTEGER LIST(MAXOBJ), MIST(MAXOBJ)
*
*     Object types and flags, parallel to list
      INTEGER   TYPE(MAXOBJ)
      INTEGER*4 FLAG(MAXOBJ)
*
*     Keep a separate list of special materials
*     and remember any special props of current material on input
CDEBUG MPARITY gets its own array because it's used in a per-pixel loop
CDEBUG (using DETAIL(LIST(MLIST(MAT))+18) cost 5% in execution time)
      INTEGER MLIST(MAXMAT), MPARITY(MAXMAT)
      LOGICAL MATCOL, BACKFACE
      LOGICAL CLIPPING, MAYCLIP, JUSTCLIPPED
      REAL    RGBMAT(3)
*
*     Object details, shadow object details
      REAL   DETAIL(MAXDET), SDTAIL(MAXSDT)
*
*     Input buffer for details
      REAL   BUF(100)
*
*     Number of objects in each tile's short list (m... are for shadows)
      COMMON /LISTS/ KOUNT, MOUNT, TTRANS, ISTRANS
      INTEGER KOUNT(MAXNTX,MAXNTY), MOUNT(NSX,NSY)
      INTEGER TTRANS(MAXNTX,MAXNTY), ISTRANS
*
*     Pointer to where each tile's objects start
      INTEGER KSTART(MAXNTX,MAXNTY), MSTART(NSX,NSY)
*
*     Pointer to where each tile's objects end
      INTEGER KSTOP(MAXNTX,MAXNTY), MSTOP(NSX,NSY)
*
*     Short list heap
      INTEGER KSHORT(MAXSHR), MSHORT(MAXSSL)
*
*     Temporary for sorting
      REAL   ZTEMP(MAXOBJ)
*
*     Where the permutation representing the sort is stored
      INTEGER ZINDEX(MAXOBJ)
*
*     The number of "details" each object type is supposed to have
*     :       input,        object,       shadow
      INTEGER IDET(MXTYPE), KDET(MXTYPE), SDET(MXTYPE)
*
*     Support for cylinders
      EXTERNAL CYLTEST
      LOGICAL  CYLTEST, ISCYL
*
*     Support for quadrics
      REAL     QNORM(3)
      EXTERNAL QINP, QTEST
      LOGICAL  QINP, QTEST, ISQUAD
*
*     Support for transparency
      COMMON /TRANS/ NTRANSP, INDEPTH, INDTOP, TRANOVFL, ZTOP, ZHIGH,
     &               INDLIST(MAXTRANSP), ZLIST(MAXTRANSP), 
     &		     NORMLIST(3,MAXTRANSP)
      INTEGER        NTRANSP, INDEPTH, INDTOP, INDLIST, TRANOVFL
      REAL           ZTOP, ZHIGH, ZLIST, NORMLIST
      REAL    SBLEND, RGBLND(3)
*
* Support for a "glow" light source 
      REAL 	GLOWSRC(3), GLOWCOL(3), GDIST(3), GLOWRAD, GLOW, GLOWMAX
      INTEGER	GOPT, GPHONG
      INTEGER	GLOWLIST(MAXGLOWS), NGLOWS
*
* Support for decompression on the fly
      EXTERNAL ungz
      INTEGER  ungz
*
* Support for BOUNDING_PLANE internal object type
      REAL     BPLANE(3), BPNORM(3), BPRGB(3)
      REAL     xn, yn, zn, xnb, ynb, znb
      INTEGER  BPTYPE, NBOUNDS, BPIND
      LOGICAL  ORTEPLIKE
      INTEGER  ORTEP
      PARAMETER (ORTEP=1)
      EXTERNAL INBOUNDS
      LOGICAL  INBOUNDS
      REAL     TEMPNORM(3)
*
*     Output buffer
      INTEGER*2 OUTBUF(OUTSIZ,4)
*
*     Copy of NOISE for ASSERT to see
      INTEGER ASSOUT
      LOGICAL VERBOSE
      COMMON /ASSCOM/ ASSOUT, VERBOSE
      SAVE /ASSCOM/
*
*     For label processing
      COMMON /LABELS/ LABOUT
      INTEGER         LABOUT
*
*     Gamma correction
      INTEGER GAMMA_MAP(256)
      PARAMETER (MAXRGB=255.0)
      LOGICAL GAMMACORRECTION
*
*     Keep track of actual coordinate limits
      COMMON /NICETIES/ TRULIM,      ZLIM,    FRONTCLIP, BACKCLIP
     &                , ISOLATION
      REAL              TRULIM(3,2), ZLIM(2), FRONTCLIP, BACKCLIP
      INTEGER           ISOLATION
*
      TRULIM (1,1) = HUGE
      TRULIM (2,1) = HUGE
      TRULIM (3,1) = HUGE
      TRULIM (1,2) = -HUGE
      TRULIM (2,2) = -HUGE
      TRULIM (3,2) = -HUGE
      ZLIM(1)      = HUGE
      ZLIM(2)      = -HUGE
*
      IDET(TRIANG)   = 12
      IDET(SPHERE)   = 7
      IDET(CYLFLAT)  = 11
      IDET(CYLIND)   = 11
      IDET(PLANE)    = 12
      IDET(NORMS )   = 9
      IDET(MATERIAL) = 10
      IDET(GLOWLIGHT)= 10
      IDET(QUADRIC)  = 17
      IDET(VERTEXRGB)= 9
      IDET(VERTRANSP)= 3
      IDET(INTERNAL) = 20
*
      KDET(TRIANG)   = 16
      KDET(SPHERE)   = 7
      KDET(CYLIND)   = 11
      KDET(PLANE)    = 7
      KDET(NORMS )   = 9
      KDET(MATERIAL) = 18
      KDET(GLOWLIGHT)= 10
      KDET(QUADRIC)  = 17
      KDET(VERTEXRGB)= 9
      KDET(VERTRANSP)= 3
      KDET(INTERNAL) = 20
*
      SDET(TRIANG)   = 13
      SDET(SPHERE)   = 4
      SDET(CYLIND)   = 8
      SDET(QUADRIC)  = 14
      SDET(INTERNAL) = 20
*     These object types really have no shadow details, 
*     but indexing seems to require a nonzero value
      SDET(PLANE)    = 1
      SDET(NORMS )   = 1
      SDET(MATERIAL) = 1
      SDET(GLOWLIGHT)= 1
      SDET(VERTEXRGB)= 1
      SDET(VERTRANSP)= 1
*
*     Copy the info (also error reporting) unit number to common
      ASSOUT = NOISE
      WRITE (NOISE,*) ' '
*
*     Initialize to level 0 of file indirection
      INPUT = INPUT0
*
*     Initialize unit number for label processing
      LABOUT = LUNIT
*
*     Initialize to no special material properties
      MSTATE    = 0
      MATCOL    = .FALSE.
      ISOLATION = 0
      CLIPPING  = .FALSE.
      NBOUNDS   = 0
      ORTEPLIKE = .FALSE.
      CLRITY    = 0.0
      GLOWMAX   = 0.0
*
*     Initialize to no perspective. EYEPOS > 0 will add perspective
      PFAC  = 1.0
      PFAC1 = 1.0
      PFAC2 = 1.0
      PFAC3 = 1.0
*
*     Initialize global properties
      FOGTYPE = -1
*
*     EAM Aug 1999 - break out command line parsing into new routine
	call parse
*
*     Get title
  100 CONTINUE
      READ (INPUT,'(A)',END=104,ERR=104) TITLE
      IF (TITLE(1:1) .EQ. '#') GOTO 100
      IF (TITLE(1:1) .EQ. '@') THEN
	K = 132
	DO I=132,2,-1
	  IF (TITLE(I:I).EQ.'	') TITLE(I:I) = ' '
	  IF (TITLE(I:I).NE.' ') J = I
	  IF (TITLE(I:I).EQ.'#') K = I-1
	  IF (TITLE(I:I).EQ.'!') K = I-1
	ENDDO
	DO WHILE (TITLE(K:K).EQ.' ')
	  K = K -1
	ENDDO
	OPEN (UNIT=INPUT+1,ERR=101,STATUS='OLD',FILE=TITLE(J:K))
	WRITE (NOISE,'(A,A)') '  + Opening input file ',TITLE(J:K)
	INPUT = INPUT + 1
	READ (INPUT,'(A)',ERR=101) TITLE
	IF (TITLE(1:1) .EQ. '#') GOTO 100
      ENDIF
      GOTO 102
  101 WRITE (NOISE,'(A,A)')' >> Cannot open or read file ',TITLE(2:K)
      CALL EXIT(-1)
  102 CONTINUE
      K = 132
      DO WHILE (TITLE(K:K).EQ.' ')
	K = K -1
      ENDDO
      WRITE (NOISE,103) TITLE(1:K)
  103 FORMAT('title="',A,'"')
*
*     Get number of tiles
      READ (INPUT,*,ERR=104,END=104) NTX,NTY
      CALL ASSERT (NTX.GT.0, 'ntx.le.0')
      CALL ASSERT (NTY.GT.0, 'nty.le.0')
      GOTO 105
104   CALL ASSERT(.FALSE.,
     &           '>>> This doesnt look like a Raster3D input file! <<<')
105   CONTINUE
*
*     Get number of pixels per tile - 0 means autotile from values in NTX, NTY
      READ (INPUT,*,ERR=104) NPX,NPY
      if (npx.eq.0 .and. nax.le.0) nax = ntx
      if (npy.eq.0 .and. nay.le.0) nay = nty
*
*     Get pixel averaging scheme
      READ (INPUT,*,ERR=104) SCHEME
      if (nscheme.ge.0) scheme = nscheme
      CALL ASSERT (SCHEME.GE.0 .AND. SCHEME.LE.4, 'bad scheme')
*
* Set up tiling and anti-aliasing.
* If NAX, NAY are set, then use them to autotile
      IF (SCHEME.LE.1) THEN
        call autotile( nax, nay, 2 )
        NOX = NPX
        NOY = NPY
	if (nax.lt.0) nax = npx * ntx
	if (nay.lt.0) nay = npy * nty
      ELSEIF (SCHEME.EQ.2) THEN
	if (nax.gt.0) nax = nax * 2
	if (nay.gt.0) nay = nay * 2
	if (nax.le.0) nax = NPX * NTX
	if (nay.le.0) nay = NPY * NTY
        call autotile( nax, nay, 2 )
	nax = nax / 2
	nay = nay / 2
        NOX = NPX/2
        NOY = NPY/2
      ELSEIF (SCHEME.EQ.3 .and. nscheme.ne.-4 .and. nax.le.0 ) THEN
C     Old style scheme 3 with exact tiling specified
	nax = NPX * NTX
	nay = NPY * NTY
	call autotile( nax, nay, 3 )
	nax = (nax * 2 + 2) / 3
	nay = (nay * 2 + 2) / 3
	NOX = (NPX * 2) / 3
	NOY = (NPY * 2) / 3
      ELSE
C     Either scheme 4 or -size and anti-aliasing selected on command line
	if (nax.gt.0) nax = (nax + (nax+1)/2)
	if (nay.gt.0) nay = (nay + (nay+1)/2)
	if (nax.le.0) nax = NPX*NTX + (NPX*NTX+1)/2
	if (nay.le.0) nay = NPY*NTY + (NPY*NTY+1)/2
	call autotile( nax, nay, 3 )
	nax = (nax * 2 + 2) / 3
	nay = (nay * 2 + 2) / 3
	NOX = (NPX * 2) / 3
	NOY = (NPY * 2) / 3
	SCHEME = 3
      ENDIF
*
	call assert (nax.gt.0, 'nax <= 0')
	call assert (nay.gt.0, 'nay <= 0')
*
      CALL ASSERT (NTX.GT.0.,'Tiling failure - ntx = 0')
      CALL ASSERT (NTY.GT.0.,'Tiling failure - nty = 0')
      CALL ASSERT (NPX.GT.0.,'Tiling failure - npx = 0')
      CALL ASSERT (NPY.GT.0.,'Tiling failure - npy = 0')
      CALL ASSERT (NTX.LE.MAXNTX,'Tiling failure - ntx>maxntx')
      CALL ASSERT (NTY.LE.MAXNTY,'Tiling failure - nty>maxnty')
      CALL ASSERT (NPX.LE.MAXNPX,'Tiling failure - npx>maxnpx')
      CALL ASSERT (NPY.LE.MAXNPY,'Tiling failure - npy>maxnpy')
*
      IF (VERBOSE) THEN
	WRITE (NOISE,*) 'ntx=',NTX,' nty=',NTY
	WRITE (NOISE,*) 'npx=',NPX,' npy=',NPY
	WRITE (NOISE,*) 'scheme=',SCHEME
	WRITE (NOISE,*) 'nox=',NOX,' noy=',NOY
      END IF
      if (nax.lt.0) nax = nox*ntx
      if (nay.lt.0) nay = noy*nty
      NX = nox*ntx
      LINOUT = 0
      WRITE (NOISE,1105) 'Rendered raster size =',NPX*NTX,NPY*NTY
      WRITE (NOISE,1105) '  Output raster size =',NAX,NAY
1105  FORMAT(A,I6,' x',I6)
C
      CALL ASSERT (OUTSIZ.GE.NOY*NOX*NTX,
     &             'image too large for output buffer')
C
C	Header records and picture title
      IF (SCHEME.EQ.0) OTMODE = OR(OTMODE,ALPHACHANNEL)
      IERR = LOCAL(1, NAX, NAY, OTMODE, QUALITY)
      IERR = LOCAL(4, TITLE)
*
*     Some derived parameters
      XCENT  = NTX*NPX/2.
      YCENT  = NTY*NPY/2.
      SXCENT = NSX*NPX/2.
      SYCENT = NSY*NPY/2.
      SCALE  = 2.*MIN(XCENT,YCENT)
*     This was always true; now it's explicit
      BACKCLIP  = -(SCALE+1.0)
      FRONTCLIP =  HUGE
*
*     Get background colour
      READ (INPUT,*,ERR=104) BKGND
      IF (VERBOSE) THEN
      	WRITE (NOISE,1106) 'bkgnd=',BKGND
      END IF
1106  FORMAT(A,4F10.4,(/,4F10.4))
      CALL ASSERT (BKGND(1).GE.0., 'bkgnd(1) < 0')
      CALL ASSERT (BKGND(2).GE.0., 'bkgnd(2) < 0')
      CALL ASSERT (BKGND(3).GE.0., 'bkgnd(3) < 0')
      CALL ASSERT (BKGND(1).LE.1., 'bkgnd(1) > 1')
      CALL ASSERT (BKGND(2).LE.1., 'bkgnd(2) > 1')
      CALL ASSERT (BKGND(3).LE.1., 'bkgnd(3) > 1')
*
*     Get "shadows" flag
      READ (INPUT,*,ERR=104) SHADOW
      IF (SHADOWFLAG.EQ.0) SHADOW = .FALSE.
      IF (SHADOWFLAG.EQ.1) SHADOW = .TRUE.
      IF (VERBOSE) THEN
      	WRITE (NOISE,*) 'shadow=',SHADOW
      END IF
*
*     Get Phong power
      READ (INPUT,*,ERR=104) PHONG
      IPHONG = PHONG
      CALL ASSERT (IPHONG.GE.0, 'iphong < 0')
*     A derived constant for numerical purposes in applying the
*     Phong power in the shading algorithm.
*     The idea is that any specular contribution less than
*     1E-9 (hence the 9 in 9./IPHONG) is insignificant:
      IF (IPHONG .NE. 0) PHOBND = 0.1**(9./IPHONG)
      IF (IPHONG .EQ. 0) PHOBND = 0.
*
*     Get contribution of straight-on (secondary) light source
      READ (INPUT,*,ERR=104) STRAIT
      CALL ASSERT (STRAIT.GE.0., 'strait < 0')
      CALL ASSERT (STRAIT.LE.1., 'strait > 1')
*
*     Derive contribution of primary light source
      PRIMAR = 1. - STRAIT
*
*     Get contribution of ambient light
      READ (INPUT,*,ERR=104) AMBIEN
      CALL ASSERT (AMBIEN.GE.0., 'ambien < 0')
      CALL ASSERT (AMBIEN.LE.1., 'ambien > 1')
*
*     Get component of specular reflection
      READ (INPUT,*,ERR=104) SPECLR
      CALL ASSERT (SPECLR.GE.0., 'speclr < 0')
      CALL ASSERT (SPECLR.LE.1., 'speclr > 1')
*
      IF (VERBOSE) THEN
     	WRITE (NOISE,1104) 'iphong=',float(IPHONG),'strait=',STRAIT,
     &                'ambien=',AMBIEN,'speclr=',SPECLR
1104  FORMAT(2(4X,A,F10.4))
      END IF
*
*     Derive component of diffuse reflection
      CALL ASSERT (AMBIEN+SPECLR.LE.1., 'ambien+speclr > 1')
      DIFFUS = 1. - (AMBIEN+SPECLR)
*
*     Get distance of viewing eye
      READ (INPUT,*,ERR=104) EYEPOS
      CALL ASSERT (EYEPOS.GE.0., 'eyepos.lt.0')
*
*     Get position of primary light source
      READ (INPUT,*,ERR=104) SOURCE
      SMAG = SQRT(SOURCE(1)**2 + SOURCE(2)**2 + SOURCE(3)**2)
      SOURCE(1) = SOURCE(1) / SMAG
      SOURCE(2) = SOURCE(2) / SMAG
      SOURCE(3) = SOURCE(3) / SMAG
      IF (VERBOSE) THEN
     	WRITE (NOISE,1106) 'eyepos=',EYEPOS
     	WRITE (NOISE,1106) 'source=',SOURCE
     	WRITE (NOISE,1106) 'normalized source=',SOURCE
      END IF
*
*     Get input transformation
      DO I=1,4
        READ (INPUT,*,ERR=104) (TMAT(I,J),J=1,4)
      END DO
      IF (VERBOSE) THEN
     	WRITE (NOISE,*) 'tmat (v'' = v * tmat):'
     	DO I=1,4
            WRITE (NOISE,'(4f9.4)') (TMAT(I,J),J=1,4)
     	END DO
      END IF
*
*     Allow command line rescaling option
      IF (ZOOM.LT.0.) ZOOM = -ZOOM / 100.
      IF (ZOOM.GT.0.) TMAT(4,4) = TMAT(4,4) / ZOOM
      IF (VERBOSE .AND. ZOOM.NE.0.) THEN
      	WRITE (NOISE,1106) 'zoom factor = ',ZOOM
      ENDIF
*
*     EAM - The original output mode was "upside down" compared
*     to what most graphics programs expect to see.  It is messy 
*     to change the evaluation order everywhere so that pixels can be 
*     streamed to stdout, so instead I invert the Y axis in TMAT and SOURCE
*     here.  
*     The actual decision whether or not to invert is done in local.c
*     and returned as a bit in the status word returned by local(0,...)
      IF (INVERT) THEN
	DO I = 1,4
	  TMAT(I,2) = -TMAT(I,2)
	ENDDO
	SOURCE(2) = -SOURCE(2)
      ENDIF
*
*     By popular demand, add a post-hoc rotation/translation option
*     that uses matrices of the form used by O and molscript
*     Initialized here to identity matrix; set by GPROP options.
      DO I = 1,4
      DO J = 1,4
        RAFTER(I,J) = 0.0
      ENDDO
      RAFTER(I,I) = 1.0
      ENDDO
      TAFTER(1) = 0.0
      TAFTER(2) = 0.0
      TAFTER(3) = 0.0
*
*     Compute the rotation matrix which takes the light
*     source to the +z axis (i.e., to the viewpoint).
*     first make p = source cross z (and normalize p)
      P1 = SOURCE(2)
      P2 = -SOURCE(1)
*     p3 = 0
      PLEN = SQRT(P1**2 + P2**2)
      IF (PLEN .GT. 0.0) P1 = P1 / PLEN
      IF (PLEN .GT. 0.0) P2 = P2 / PLEN
*     phi is the angle between source and z (shortest route)
      COSPHI = SOURCE(3)
      SINPHI = PLEN
      SROT(1,1) = P1**2 + (1.-P1**2)*COSPHI
      SROT(1,2) = P1*P2*(1.-COSPHI)
      SROT(1,3) = P2*SINPHI
      SROT(2,1) = SROT(1,2)
      SROT(2,2) = P2**2 + (1.-P2**2)*COSPHI
      SROT(2,3) = -P1*SINPHI
      SROT(3,1) = -SROT(1,3)
      SROT(3,2) = -SROT(2,3)
      SROT(3,3) = COSPHI
      SROT(1,4) = 0.0
      SROT(2,4) = 0.0
      SROT(3,4) = 0.0
      SROT(4,1) = 0.0
      SROT(4,2) = 0.0
      SROT(4,3) = 0.0
      SROT(4,4) = 1.0
*
*     Quadrics will require the inverse matrix also (and its transpose)
*     This is also a convenient place to check legality of TMAT
      CALL QSETUP
*
*     Get input mode
      READ (INPUT,*,ERR=104) INMODE
C     WRITE (NOISE,*) 'inmode=',INMODE
      CALL ASSERT (INMODE.GE.1,'bad inmode')
*
*     Get input format(s)
      IF (INMODE.EQ.1.OR.INMODE.EQ.2) THEN
        READ (INPUT,'(A)',ERR=104) INFMT
C       WRITE (NOISE,*) 'infmt=',INFMT
        II = 0
2       CONTINUE
        IF (INFMT(1:1).EQ.' ') THEN
          INFMT(1:79) = INFMT(2:80)
          INFMT(80:80) = ' '
          II = II + 1
          IF (II.LT.80) GO TO 2
        ENDIF
        IF (INFMT(1:1).EQ.'*') THEN
          INFLG = .TRUE.
        ELSE
          INFLG = .FALSE.
        ENDIF
      ELSEIF (INMODE.GE.3) THEN
C       WRITE (NOISE,*) 'infmts:'
        DO 4 I=1,3
          READ (INPUT,'(A)',ERR=104) INFMTS(I)
C         WRITE (NOISE,*) INFMTS(I)
          II = 0
3         CONTINUE
          IF (INFMTS(I)(1:1).EQ.' ') THEN
            INFMTS(I)(1:79) = INFMTS(I)(2:80)
            INFMTS(I)(80:80) = ' '
            II = II + 1
            IF (II.LT.80) GO TO 3
          ENDIF
          IF (INFMTS(I)(1:1).EQ.'*') THEN
            INFLGS(I) = .TRUE.
          ELSE
            INFLGS(I) = .FALSE.
          ENDIF
4       CONTINUE
	INFLGS(PLANE)     = INFLGS(TRIANG)
	INFMTS(PLANE)     = INFMTS(TRIANG)
	INFLGS(NORMS)     = INFLGS(TRIANG)
	INFMTS(NORMS)     = INFMTS(TRIANG)
	INFLGS(VERTEXRGB) = INFLGS(TRIANG)
	INFMTS(VERTEXRGB) = INFMTS(TRIANG)
	INFLGS(VERTRANSP) = INFLGS(TRIANG)
	INFMTS(VERTRANSP) = INFMTS(TRIANG)
	INFLGS(MATERIAL)  = .TRUE.
	INFLGS(GLOWLIGHT) = .TRUE.
	INFLGS(QUADRIC)   = .TRUE.
      ELSE
        CALL ASSERT (.FALSE.,'bad inmode')
      ENDIF
c
c     Done with header records
c	Do we force-close the input file, so that we can borrow headers,
c	or should we keep going as long as the file continues?
c	The following 4 lines implement the former, so that the initial
c	@file command essentially means 'use his header records for me too'.
c
c     IF (INPUT.NE.INPUT0) THEN
c	CLOSE(INPUT)
c	INPUT = INPUT0
c     ENDIF
c     As of V2.5e, however, we keep reading.
c     >>> This is a change! <<<
c
c     End of header processing
*
*     Give them a notice to stare at while the program cranks along
      WRITE (NOISE,'(1X)')
      WRITE (NOISE,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%',
     &                '%%%%%%%%%%%%%%%%%%%%%%%%%%'
      WRITE (NOISE,*) '%                      Raster3D    ',VERSION,
     &                '                    %'
      WRITE (NOISE,*) '%            -------------------------',
     &                '-------------            %'
      WRITE (NOISE,*) '% If you publish figures generated by this ',
     &                'program please cite %'
      WRITE (NOISE,*) '%   Merritt & Bacon (1997)  ',
     &                'Meth. Enzymol. 277, 505-524.','       %'
      WRITE (NOISE,*) '%            -------------------------',
     &                '-------------            %'
      WRITE (NOISE,*) '%                  Raster3D distribution site',
     &                '                  %'
      WRITE (NOISE,*) '%          http://www.bmsc.washington.edu/',
     &                'raster3d/            %'
      WRITE (NOISE,*) '% comments & suggestions to:  ',
     &                '        merritt@u.washington.edu %'
      WRITE (NOISE,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%',
     &                '%%%%%%%%%%%%%%%%%%%%%%%%%%'
*
*     If label processing is selected on command line, 
*     initialize PostScript output file
*
      IF (LFLAG) THEN
        PSSCALE = 2.0 * MIN( NTX*NOX/2., NTY*NOY/2. )
      	CALL LSETUP( PSSCALE, BKGND, TITLE )
      ENDIF
      WRITE (NOISE,'(1X)')
*
*     Initialize gamma correction table
      GAMMACORRECTION = .FALSE.
      IF (GAMMA.LT.0.99 .OR. GAMMA.GT.1.01) GAMMACORRECTION = .TRUE.
      IF (GAMMACORRECTION) THEN
      	DO I=0,MAXRGB
	     G = I
	     GAMMA_MAP(I+1) = (G/MAXRGB) ** (1.0/GAMMA) * MAXRGB + 0.5
	ENDDO
      ENDIF
*
*
*     Initialize counters
      DO 5 J=1,NTY
      DO 5 I=1,NTX
        KOUNT(I,J) = 0
	TTRANS(I,J) = 0
5     CONTINUE
      DO 6 J=1,NSY
      DO 6 I=1,NSX
        MOUNT(I,J) = 0
6     CONTINUE
c
      DO 662 I = 1,MAXOBJ
	FLAG(I) = 0
  662 CONTINUE
      nprops  = 0
      npropm  = 0
      ntransp = 0
      nsphere = 0
      ncylind = 0
      nplanes = 0
      nhidden = 0
      ninside = 0
      mstate  = 0
      nlabels = 0
      nglows  = 0
      nquads  = 0
      nclip   = 0
      nvtrans = 0
      nbplanes = 0
      tranovfl = 0
c
c     Objects in, and count up objects that may impinge on each tile
      NDET = 0
      IF (SHADOW) MDET = 0
      N = 0
c
c     Read in next object
7     CONTINUE
      IF (INMODE.EQ.1.OR.INMODE.EQ.2) THEN
        INTYPE = INMODE
	GOTO 8
      ENDIF
C
C     READ (INPUT,*,END=50) INTYPE
      READ (INPUT,'(A)',END=50,ERR=47) LINE
c     Nov 1997 - allow # comments
      IF (LINE(1:1) .EQ. '#') THEN
      	GOTO 7
c     May 1996 - allow file indirection
      ELSE IF (LINE(1:1) .EQ. '@') THEN
        J = 1
	K = 132
	DO I=132,2,-1
	  IF (LINE(I:I).NE.' ') J = I
	  IF (LINE(I:I).EQ.'#') K = I-1
	  IF (LINE(I:I).EQ.'!') K = I-1
	  IF (LINE(I:I).EQ.CHAR(0)) K = I-1
	  IF (LINE(I:I).EQ.'	') LINE(I:I) = ' '
	ENDDO
	IF (J.EQ.1) GOTO 7
	DO I=J,K
	  IF (LINE(I:I).NE.' ') L = I
	ENDDO
	K = L
	IF (LINE(K:K).eq.'Z' .or. LINE(K-1:K).eq.'gz') THEN
c	    ungz will uncompress into a temporary file, which ought to 
c	    be deleted later.  Unfortunately, that's hard to do in g77 
c	    since it doesn't support dispose='DELETE'.
	    line(k+1:k+1) = char(0)
	    if (0 .gt. ungz( line(j:k+1), fullname )) goto 73
	    j = 1
	    k = 132
	    do i=132,2,-1
	      if (fullname(i:i).eq.' ')     k = i-1
	      if (fullname(i:i).eq.char(0)) k = i-1
	    enddo
	    if (verbose) 
     &		write(noise,*) 'Creating temporary file: ',fullname(j:k)
	    open (unit=input+1,err=73,status='OLD',
     &            DISPOSE='DELETE',
     &		  file=fullname(j:k))
	    fullname = line(2:132)
	    goto 72
	ENDIF
   70	CONTINUE
	OPEN (UNIT=INPUT+1,ERR=71,STATUS='OLD',
     &	      FILE=LINE(J:K))
	FULLNAME = LINE(J:K)
	GOTO 72
   71	CONTINUE
	IF (LINE(K-3:K).NE.'.r3d') THEN
	    K = K + 4
	    LINE(K-3:K) = '.r3d'
	    GOTO 70
	ENDIF
   	CALL LIBLOOKUP( LINE(J:K), FULLNAME )
	OPEN (UNIT=INPUT+1,ERR=73,STATUS='OLD',FILE=FULLNAME)
   72	CONTINUE
	DO I=132,2,-1
	  IF (FULLNAME(I:I).EQ.' ') J = I
	ENDDO
	WRITE (NOISE,'(A,A)') '  + Opening input file ',FULLNAME(1:J)
	INPUT = INPUT + 1
	CALL ASSERT(INPUT-INPUT0.LE.MAXLEV, 
     &	            'Too many levels of indirection')
	GOTO 7
   73	WRITE (NOISE,'(A,A)') ' >> Cannot open file ',LINE(J:K)
	GOTO 7
      ELSE
	READ (LINE,*,END=50,ERR=74) INTYPE
	GOTO 76
   74	WRITE (NOISE,'(A,A)') ' >> Unrecognized line: ',LINE
	GOTO 7
   76	CONTINUE
      ENDIF
      IF (INTYPE.EQ.0) GO TO 50
      IF (INTYPE .EQ. CYLFLAT) THEN
	INTYPE = CYLIND
	FLAG(N+1) = OR( FLAG(N+1), FLAT )
      ELSEIF (INTYPE .EQ. MATEND) THEN
	MSTATE    = 0
	CLRITY    = 0
	CLROPT    = 0
	MATCOL    = .FALSE.
	ISOLATION = 0
	CLIPPING  = .FALSE.
	NBOUNDS   = 0
	ORTEPLIKE = .FALSE.
	GOTO 7
      ELSEIF (INTYPE .EQ. FONT) THEN
	IF (LFLAG) THEN
	  CALL LINP( INPUT, INTYPE, .FALSE., RGBMAT )
	ELSE
          READ (INPUT,'(A)',END=50) LINE
	ENDIF
	GOTO 7
      ELSEIF (INTYPE .EQ. LABEL) THEN
	NLABELS = NLABELS + 1
	IF (LFLAG) THEN
	  IF (MSTATE.EQ.MATERIAL .AND. MATCOL) THEN
	    CALL LINP( INPUT, INTYPE, .TRUE., RGBMAT )
	  ELSE
	    CALL LINP( INPUT, INTYPE, .FALSE., RGBMAT )
	  ENDIF
	ELSE
          READ (INPUT,'(A)',END=50) LINE
          READ (INPUT,'(A)',END=50) LINE
	ENDIF
	GOTO 7
      ELSEIF (INTYPE .EQ. ISOLATE1) THEN
	ISOLATION = 1
	GOTO 7
      ELSEIF (INTYPE .EQ. ISOLATE2) THEN
	ISOLATION = 2
	GOTO 7
c     Global Properties
      ELSEIF (INTYPE .EQ. GPROP) THEN
	READ (INPUT,'(A)',END=50) LINE
	DO I = 132, 1, -1
	  IF (LINE(I:I).NE.' '.AND.LINE(I:I).NE.'	') L = I
	ENDDO
	IF (LINE(L:L+2).EQ.'FOG') THEN
	  READ (LINE(L+4:74),*,ERR=771,END=771) 
     &          FOGTYPE, FOGFRONT, FOGBACK, FOGDEN
  771	  CONTINUE
     	  FOGRGB(1) = BKGND(1)
     	  FOGRGB(2) = BKGND(2)
     	  FOGRGB(3) = BKGND(3)
	  CALL CHKRGB(FOGRGB(1),FOGRGB(2),FOGRGB(3),'invalid fog color')
	  IF (FOGTYPE.NE.1)  FOGTYPE = 0
	  IF (FOGDEN.LE.0.0) FOGDEN = 0.5
	ELSE IF (LINE(L:L+8).EQ.'FRONTCLIP') THEN
	  READ (LINE(L+10:132),*,ERR=75) FRONTCLIP
	  FRONTCLIP = FRONTCLIP * SCALE / TMAT(4,4)
	ELSE IF (LINE(L:L+7).EQ.'BACKCLIP') THEN
	  READ (LINE(L+9:132),*,ERR=75) BACKCLIP
	  BACKCLIP = BACKCLIP * SCALE / TMAT(4,4)
	ELSE IF (LINE(L:L+7).EQ.'ROTATION') THEN
	  READ (LINE(L+9:132),*,ERR=773,END=773) 
     &		((RAFTER(I,J),J=1,3),I=1,3)
	  GOTO 774
  773	  READ (INPUT,*,ERR=75) ((RAFTER(I,J),J=1,3),I=1,3)
  774	  WRITE (NOISE,775) ((RAFTER(I,J),J=1,3),I=1,3)
  775	  FORMAT('Post-rotation matrix:  ',3(/,3F10.4))
	  D = DET(RAFTER)
	  IF (ABS(1.0-ABS(D)).GT.0.02) WRITE (NOISE,*) 
     &	    '>>> Warning: Post-rotation matrix has determinant',D
	  IF (INVERT) THEN
	    RAFTER(1,2) = -RAFTER(1,2)
	    RAFTER(2,1) = -RAFTER(2,1)
	    RAFTER(2,3) = -RAFTER(2,3)
	    RAFTER(3,2) = -RAFTER(3,2)
	  ENDIF
	  CALL QSETUP
	ELSE IF (LINE(L:L+10).EQ.'TRANSLATION') THEN
	  READ (LINE(L+12:132),*,ERR=776,END=776) 
     &		(TAFTER(I),I=1,3)
	  GOTO 777
  776	  READ (INPUT,*,ERR=75) (TAFTER(I),I=1,3)
  777	  WRITE (NOISE,778) (TAFTER(I),I=1,3)
  778	  FORMAT('Post-translation:      ',1(/,3F10.4))
	  IF (INVERT) TAFTER(2) = -TAFTER(2)
	ELSE IF (LINE(L:L+4).EQ.'DUMMY') THEN
	ELSE
	  GOTO 75
	ENDIF
	GOTO 7

   75	CONTINUE
	WRITE(NOISE,'(A,A)') 
     &         '>> Unrecognized or incomplete GPROP option ',LINE
	GOTO 7
*
      ENDIF
*
COLD  CALL ASSERT (INTYPE.GE.1.AND.INTYPE.LE.MXTYPE,'bad object')
      IF (INTYPE.LT.1 .OR. INTYPE.GT.MXTYPE) GOTO 47
      CALL ASSERT (INTYPE.NE.INTERNAL,'object type 4 not available')
c
c     Read in object details, now we know what kind it is.
c     Allow an all-zeroes record to terminate input for the
c     benefit of those of us who might inadvertently supply
c     a series of blank records after our real input as a
c     side-effect of tape blocking or sloppiness or ...
8     CONTINUE
      IF (INMODE.GE.3) THEN
	INFMT = INFMTS(INTYPE)
	INFLG = INFLGS(INTYPE)
      ENDIF
      IF (INFLG) THEN
        READ (INPUT,*,END=50) (BUF(I),I=1,IDET(INTYPE))
      ELSE
        READ (INPUT,INFMT,END=50) (BUF(I),I=1,IDET(INTYPE))
      ENDIF
c     15-Dec-1999 This was supposed to check for all-zero line and exit
c                 but all zeros is legal for [at least!] LABELs
      IF (INTYPE.EQ.LABEL) GOTO 9
      DO I=1,IDET(INTYPE)
        IF (BUF(I).NE.0.) GO TO 9
      ENDDO
      GO TO 50
9     CONTINUE
      CALL ASSERT (NDET+KDET(INTYPE).LE.MAXDET,
     & 'too many object details - increase MAXDET and recompile')
      IF (SHADOW) CALL ASSERT (MDET+SDET(INTYPE).LE.MAXSDT,
     & 'too many shadow object details - increase MAXSDT and recompile')
      N = N + 1
      CALL ASSERT (N.LE.MAXOBJ,
     & 'too many objects - increase MAXOBJ and recompile')
C     20-Feb-1997 Save both object type and material type 
      TYPE(N) = INTYPE
      IF (MSTATE.EQ.MATERIAL) FLAG(N) = FLAG(N) + 65536 * NPROPM
      LIST(N) = NDET
      IF (SHADOW) MIST(N) = MDET
      ISTRANS  = 0
*     From this point on, we'll use the symbolic codes for objects
      IF (INTYPE.EQ.TRIANG .or. INTYPE.EQ.PLANE) THEN
*       triangle as read in
        X1A = BUF(1)
        Y1A = BUF(2)
        Z1A = BUF(3)
        X2A = BUF(4)
        Y2A = BUF(5)
        Z2A = BUF(6)
        X3A = BUF(7)
        Y3A = BUF(8)
        Z3A = BUF(9)
        RED = BUF(10)
        GRN = BUF(11)
        BLU = BUF(12)
	CALL CHKRGB( RED,GRN,BLU,'invalid triangle color' )
        CALL ASSERT (IDET(INTYPE).EQ.12,'idet(1).ne.12')
	IF (MSTATE.EQ.MATERIAL) THEN
	  FLAG(N) = OR(FLAG(N),PROPS)
	  NPROPS  = NPROPS + 1
	  IF (CLRITY.GT.0) THEN
	    FLAG(N) = OR(FLAG(N),TRANSP)
	    IF (CLROPT.EQ.1) FLAG(N) = OR(FLAG(N),MOPT1)
	    NTRANSP = NTRANSP + 1
	    ISTRANS = 1
	  ENDIF
	  IF (CLIPPING) THEN
	    FLAG(N) = OR(FLAG(N),CLIPPED)
	  ENDIF
	  IF (NBOUNDS.GT.0) FLAG(N) = OR(FLAG(N),BOUNDED)
	  IF (MATCOL) THEN
	    RED = RGBMAT(1)
	    GRN = RGBMAT(2)
	    BLU = RGBMAT(3)
	  ENDIF
	ENDIF
*	Isolated objects not transformed by TMAT, but still subject to inversion
	IF (ISOLATION.GT.0) THEN
	  CALL ISOLATE(X1A,Y1A)
	  CALL ISOLATE(X2A,Y2A)
	  CALL ISOLATE(X3A,Y3A)
	ELSE
*	update true coordinate limits
	  TRULIM(1,1) = MIN( TRULIM(1,1), X1A,X2A,X3A)
	  TRULIM(1,2) = MAX( TRULIM(1,2), X1A,X2A,X3A)
	  TRULIM(2,1) = MIN( TRULIM(2,1), Y1A,Y2A,Y3A)
	  TRULIM(2,2) = MAX( TRULIM(2,2), Y1A,Y2A,Y3A)
	  TRULIM(3,1) = MIN( TRULIM(3,1), Z1A,Z2A,Z3A)
	  TRULIM(3,2) = MAX( TRULIM(3,2), Z1A,Z2A,Z3A)
*       modify the input, so to speak
	  CALL TRANSF (X1A,Y1A,Z1A)
	  CALL TRANSF (X2A,Y2A,Z2A)
	  CALL TRANSF (X3A,Y3A,Z3A)
	ENDIF
*       perspective factor for each corner
	IF (EYEPOS.GT.0) THEN
          PFAC1 = PERSP( Z1A )
          PFAC2 = PERSP( Z2A )
          PFAC3 = PERSP( Z3A )
	END IF
*       apply perspective
        X1B = X1A * PFAC1
        Y1B = Y1A * PFAC1
        Z1B = Z1A * PFAC1
        X2B = X2A * PFAC2
        Y2B = Y2A * PFAC2
        Z2B = Z2A * PFAC2
        X3B = X3A * PFAC3
        Y3B = Y3A * PFAC3
        Z3B = Z3A * PFAC3
*       scale and translate to pixel space
        X1C = X1B * SCALE + XCENT
        Y1C = Y1B * SCALE + YCENT
        Z1C = Z1B * SCALE
        X2C = X2B * SCALE + XCENT
        Y2C = Y2B * SCALE + YCENT
        Z2C = Z2B * SCALE
        X3C = X3B * SCALE + XCENT
        Y3C = Y3B * SCALE + YCENT
        Z3C = Z3B * SCALE
*	save transformed Z limits
	ZLIM(1) = MIN( ZLIM(1), Z1C,Z2C,Z3C )
	ZLIM(2) = MAX( ZLIM(2), Z1C,Z2C,Z3C )
*
*	check for Z-clipping
	JUSTCLIPPED = .FALSE.
	IF (INTYPE.NE.PLANE) THEN
	  IF ((MIN(Z1C,Z2C,Z3C) .GT. FRONTCLIP)
     &    .OR.(MAX(Z1C,Z2C,Z3C) .LT. BACKCLIP)) THEN
	      JUSTCLIPPED = .TRUE.
	      GOTO 45
	  ENDIF
	  IF (CLIPPING) THEN
	    MIND = LIST(MLIST(NPROPM))
	    IF ((MIN(Z1C,Z2C,Z3C) .GT. DETAIL(MIND+16))
     &      .OR.(MAX(Z1C,Z2C,Z3C) .LT. DETAIL(MIND+17))) THEN
	      JUSTCLIPPED = .TRUE.
	      GOTO 45
	    ENDIF
	  ENDIF
	ENDIF
*
*       solve for coefficients of plane eqn z=Ax+By+C
        CALL PLANER(X1C,Y1C,Z1C,
     &             X2C,Y2C,Z2C,
     &             X3C,Y3C,Z3C, A,B,C,D)
*       save results for PLANE object
*	PLANE impinges on all tiles, but casts no shadows
	IF (INTYPE.EQ. PLANE) THEN
	    NPLANES = NPLANES + 1
	    DETAIL(NDET+1) = A
	    DETAIL(NDET+2) = B
	    DETAIL(NDET+3) = C
	    DETAIL(NDET+4) = D
	    DETAIL(NDET+5) = RED
	    DETAIL(NDET+6) = GRN
	    DETAIL(NDET+7) = BLU
	    CALL ASSERT(KDET(INTYPE).EQ.7,'kdet(6).ne.7')
	    NDET = NDET + KDET(INTYPE)
	    DO IY = 1,NTY
	    DO IX = 1,NTX
		KOUNT(IX,IY) = KOUNT(IX,IY) + 1
		TTRANS(IX,IY) = TTRANS(IX,IY) + ISTRANS
	    ENDDO
	    ENDDO
	    IF (SHADOW) THEN
              MDET = MDET + SDET(INTYPE)
	    ENDIF
	    GOTO 7
	ENDIF
*       save results for normal triangles
        DETAIL(NDET+1) = X1C
        DETAIL(NDET+2) = Y1C
        DETAIL(NDET+3) = Z1C
        DETAIL(NDET+4) = X2C
        DETAIL(NDET+5) = Y2C
        DETAIL(NDET+6) = Z2C
        DETAIL(NDET+7) = X3C
        DETAIL(NDET+8) = Y3C
        DETAIL(NDET+9) = Z3C
        DETAIL(NDET+10) = A
        DETAIL(NDET+11) = B
        DETAIL(NDET+12) = C
        DETAIL(NDET+13) = D
        DETAIL(NDET+14) = RED
        DETAIL(NDET+15) = GRN
        DETAIL(NDET+16) = BLU
        CALL ASSERT (KDET(INTYPE).EQ.16,'kdet(1).ne.16')
        NDET = NDET + KDET(INTYPE)
*       tally for tiles the object might impinge on
        IXLO = MIN(X1C,X2C,X3C)/NPX + 1
        IXHI = MAX(X1C,X2C,X3C)/NPX + 1
        IYLO = MIN(Y1C,Y2C,Y3C)/NPY + 1
        IYHI = MAX(Y1C,Y2C,Y3C)/NPY + 1
        IF (IXLO.LT.1  ) IXLO=1
        IF (IXLO.GT.NTX) GO TO 11
        IF (IXHI.LT.1  ) GO TO 11
        IF (IXHI.GT.NTX) IXHI=NTX
        IF (IYLO.LT.1  ) IYLO=1
        IF (IYLO.GT.NTY) GO TO 11
        IF (IYHI.LT.1  ) GO TO 11
        IF (IYHI.GT.NTY) IYHI=NTY
        DO 10 IY=IYLO,IYHI
        DO 10 IX=IXLO,IXHI
          KOUNT(IX,IY) = KOUNT(IX,IY) + 1
	  TTRANS(IX,IY) = TTRANS(IX,IY) + ISTRANS
10      CONTINUE
11      CONTINUE
*       repeat for shadow buffer if necessary
        IF (SHADOW) THEN
*         rotate light source to z to take light source viewpoint
          X1R = SROT(1,1)*X1B+SROT(1,2)*Y1B+SROT(1,3)*Z1B
          Y1R = SROT(2,1)*X1B+SROT(2,2)*Y1B+SROT(2,3)*Z1B
          Z1R = SROT(3,1)*X1B+SROT(3,2)*Y1B+SROT(3,3)*Z1B
          X2R = SROT(1,1)*X2B+SROT(1,2)*Y2B+SROT(1,3)*Z2B
          Y2R = SROT(2,1)*X2B+SROT(2,2)*Y2B+SROT(2,3)*Z2B
          Z2R = SROT(3,1)*X2B+SROT(3,2)*Y2B+SROT(3,3)*Z2B
          X3R = SROT(1,1)*X3B+SROT(1,2)*Y3B+SROT(1,3)*Z3B
          Y3R = SROT(2,1)*X3B+SROT(2,2)*Y3B+SROT(2,3)*Z3B
          Z3R = SROT(3,1)*X3B+SROT(3,2)*Y3B+SROT(3,3)*Z3B
*         scale and translate for shadow space
          X1S = X1R * SCALE + SXCENT
          Y1S = Y1R * SCALE + SYCENT
          Z1S = Z1R * SCALE
          X2S = X2R * SCALE + SXCENT
          Y2S = Y2R * SCALE + SYCENT
          Z2S = Z2R * SCALE
          X3S = X3R * SCALE + SXCENT
          Y3S = Y3R * SCALE + SYCENT
          Z3S = Z3R * SCALE
*         solve plane eqn etc.
          CALL PLANER(X1S,Y1S,Z1S,
     &               X2S,Y2S,Z2S,
     &               X3S,Y3S,Z3S, A,B,C,D)
*         save results etc.
          SDTAIL(MDET+1) = X1S
          SDTAIL(MDET+2) = Y1S
          SDTAIL(MDET+3) = Z1S
          SDTAIL(MDET+4) = X2S
          SDTAIL(MDET+5) = Y2S
          SDTAIL(MDET+6) = Z2S
          SDTAIL(MDET+7) = X3S
          SDTAIL(MDET+8) = Y3S
          SDTAIL(MDET+9) = Z3S
          SDTAIL(MDET+10) = A
          SDTAIL(MDET+11) = B
          SDTAIL(MDET+12) = C
          SDTAIL(MDET+13) = D
          CALL ASSERT (SDET(INTYPE).EQ.13,'sdet(1).ne.13')
          MDET = MDET + SDET(INTYPE)
*         tally for shadow tiles the object might impinge on
          IXLO = MIN(X1S,X2S,X3S)/NPX + 1
          IXHI = MAX(X1S,X2S,X3S)/NPX + 1
          IYLO = MIN(Y1S,Y2S,Y3S)/NPY + 1
          IYHI = MAX(Y1S,Y2S,Y3S)/NPY + 1
          IF (IXLO.LT.1  ) IXLO=1
          IF (IXLO.GT.NSX) GO TO 16
          IF (IXHI.LT.1  ) GO TO 16
          IF (IXHI.GT.NSX) IXHI=NSX
          IF (IYLO.LT.1  ) IYLO=1
          IF (IYLO.GT.NSY) GO TO 16
          IF (IYHI.LT.1  ) GO TO 16
          IF (IYHI.GT.NSY) IYHI=NSY
          DO 15 IY=IYLO,IYHI
          DO 15 IX=IXLO,IXHI
            MOUNT(IX,IY) = MOUNT(IX,IY) + 1
15        CONTINUE
16        CONTINUE
        ENDIF
      ELSEIF (INTYPE.EQ.SPHERE) THEN
*       sphere as read in
        XA = BUF(1)
        YA = BUF(2)
        ZA = BUF(3)
        RA = BUF(4)
        RED = BUF(5)
        GRN = BUF(6)
        BLU = BUF(7)
	CALL CHKRGB (RED,GRN,BLU,'invalid sphere color')
        CALL ASSERT (IDET(INTYPE).EQ.7,'idet(2).ne.7')
	IF (MSTATE.EQ.MATERIAL) THEN
	  FLAG(N) = OR(FLAG(N),PROPS)
	  NPROPS  = NPROPS + 1
	  IF (CLRITY.GT.0) THEN
	    FLAG(N) = OR(FLAG(N),TRANSP)
	    IF (CLROPT.EQ.1) FLAG(N) = OR(FLAG(N),MOPT1)
	    NTRANSP = NTRANSP + 1
	    ISTRANS = 1
	  ENDIF
	  IF (CLIPPING) THEN
	    FLAG(N) = OR(FLAG(N),CLIPPED)
	  ENDIF
	  IF (NBOUNDS.GT.0) FLAG(N) = OR(FLAG(N),BOUNDED)
	  IF (MATCOL) THEN
	    RED = RGBMAT(1)
	    GRN = RGBMAT(2)
	    BLU = RGBMAT(3)
	  ENDIF
	ENDIF
*	Isolated objects not transformed by TMAT, but still subject to inversion
	IF (ISOLATION.GT.0) THEN
	  CALL ISOLATE(XA,YA)
	ELSE
*	update true coordinate limits
	  TRULIM(1,1) = MIN( TRULIM(1,1), XA )
	  TRULIM(1,2) = MAX( TRULIM(1,2), XA )
	  TRULIM(2,1) = MIN( TRULIM(2,1), YA )
	  TRULIM(2,2) = MAX( TRULIM(2,2), YA )
	  TRULIM(3,1) = MIN( TRULIM(3,1), ZA )
	  TRULIM(3,2) = MAX( TRULIM(3,2), ZA )
*       modify the input, as it were
          CALL TRANSF (XA,YA,ZA)
          RA = RA / TMAT(4,4)
	ENDIF
*       perspective
	IF (EYEPOS.GT.0) PFAC = PERSP(ZA)
        XB = XA * PFAC
        YB = YA * PFAC
        ZB = ZA * PFAC
        RB = RA * PFAC
*       scale & translate
        XC = XB * SCALE + XCENT
        YC = YB * SCALE + YCENT
        ZC = ZB * SCALE
        RC = RB * SCALE
*	save transformed Z limits
	ZLIM(1) = MIN( ZLIM(1), ZC )
	ZLIM(2) = MAX( ZLIM(2), ZC )
*	check for Z-clipping
	IF (ZC .GT. FRONTCLIP .OR. ZC .LT. BACKCLIP) THEN
	    JUSTCLIPPED = .TRUE.
	    GOTO 45
	ELSE
	    JUSTCLIPPED = .FALSE.
	ENDIF
*       save results
        DETAIL(NDET+1) = XC
        DETAIL(NDET+2) = YC
        DETAIL(NDET+3) = ZC
        DETAIL(NDET+4) = RC
        DETAIL(NDET+5) = RED
        DETAIL(NDET+6) = GRN
        DETAIL(NDET+7) = BLU
        CALL ASSERT (KDET(INTYPE).EQ.7,'kdet(2).ne.7')
        NDET = NDET + KDET(INTYPE)
	nsphere = nsphere + 1
*       tally for tiles the object might impinge on
        IXLO = (XC-RC)/NPX + 1
        IXHI = (XC+RC)/NPX + 1
        IYLO = (YC-RC)/NPY + 1
        IYHI = (YC+RC)/NPY + 1
        IF (IXLO.LT.1  ) IXLO=1
        IF (IXLO.GT.NTX) GO TO 21
        IF (IXHI.LT.1  ) GO TO 21
        IF (IXHI.GT.NTX) IXHI=NTX
        IF (IYLO.LT.1  ) IYLO=1
        IF (IYLO.GT.NTY) GO TO 21
        IF (IYHI.LT.1  ) GO TO 21
        IF (IYHI.GT.NTY) IYHI=NTY
        DO 20 IY=IYLO,IYHI
        DO 20 IX=IXLO,IXHI
          KOUNT(IX,IY) = KOUNT(IX,IY) + 1
	  TTRANS(IX,IY) = TTRANS(IX,IY) + ISTRANS
20      CONTINUE
21      CONTINUE
*       repeat for shadow buffer if necessary
        IF (SHADOW) THEN
*         rotate light source to z to take light source viewpoint
          XR = SROT(1,1)*XB+SROT(1,2)*YB+SROT(1,3)*ZB
          YR = SROT(2,1)*XB+SROT(2,2)*YB+SROT(2,3)*ZB
          ZR = SROT(3,1)*XB+SROT(3,2)*YB+SROT(3,3)*ZB
          RR = RB
*         scale and translate for shadow space
          XS = XR * SCALE + SXCENT
          YS = YR * SCALE + SYCENT
          ZS = ZR * SCALE
          RS = RR * SCALE
*         save results
          SDTAIL(MDET+1) = XS
          SDTAIL(MDET+2) = YS
          SDTAIL(MDET+3) = ZS
          SDTAIL(MDET+4) = RS
          CALL ASSERT (SDET(INTYPE).EQ.4,'sdet(2).ne.4')
          MDET = MDET + SDET(INTYPE)
*         tally for shadow tiles the object might impinge on
          IXLO = (XS-RS)/NPX + 1
          IXHI = (XS+RS)/NPX + 1
          IYLO = (YS-RS)/NPY + 1
          IYHI = (YS+RS)/NPY + 1
          IF (IXLO.LT.1  ) IXLO=1
          IF (IXLO.GT.NSX) GO TO 26
          IF (IXHI.LT.1  ) GO TO 26
          IF (IXHI.GT.NSX) IXHI=NSX
          IF (IYLO.LT.1  ) IYLO=1
          IF (IYLO.GT.NSY) GO TO 26
          IF (IYHI.LT.1  ) GO TO 26
          IF (IYHI.GT.NSY) IYHI=NSY
          DO 25 IY=IYLO,IYHI
          DO 25 IX=IXLO,IXHI
            MOUNT(IX,IY) = MOUNT(IX,IY) + 1
25        CONTINUE
26        CONTINUE
        ENDIF

c	30-Dec-99 duplicate transparent spheres, if requested, so that
c	the inside surface can be rendered also. BUF() is still loaded
c	with specs for the current object; we just need to set flags.
	IF (  AND(FLAG(N),TRANSP).NE.0 .AND. CLROPT.EQ.2
     &  .AND. AND(FLAG(N),INSIDE).EQ.0) THEN
	    FLAG(N+1) = INSIDE
	    NINSIDE   = NINSIDE + 1
	    GOTO 9
	ENDIF

      ELSEIF (INTYPE.EQ.CYLIND) THEN
*	EAM May 1990 cylinder as read in
        X1A = BUF(1)
        Y1A = BUF(2)
        Z1A = BUF(3)
	R1A = BUF(4)
        X2A = BUF(5)
        Y2A = BUF(6)
        Z2A = BUF(7)
	R2A = R1A
        RED = BUF(9)
        GRN = BUF(10)
        BLU = BUF(11)
	CALL CHKRGB (RED,GRN,BLU,'invalid cylinder color')
        CALL ASSERT (IDET(INTYPE).EQ.11,'idet(1).ne.11')
*	Zero length cylinder is better treated as sphere
*	EAM 22-Nov-96
	IF ((AND(FLAG(N),FLAT).EQ.0) .AND.
     &	    (X1A.EQ.X2A).AND.(Y1A.EQ.Y2A).AND.(Z1A.EQ.Z2A)) THEN
		BUF(5) = BUF(9)
		BUF(6) = BUF(10)
		BUF(7) = BUF(11)
		INTYPE = SPHERE
		N = N-1
		GOTO 9
	ENDIF
	IF (MSTATE.EQ.MATERIAL) THEN
	  FLAG(N) = OR(FLAG(N),PROPS)
	  NPROPS  = NPROPS + 1
	  IF (CLRITY.GT.0) THEN
	    FLAG(N) = OR(FLAG(N),TRANSP)
	    IF (CLROPT.EQ.1) FLAG(N) = OR(FLAG(N),MOPT1)
	    NTRANSP = NTRANSP + 1
	    ISTRANS = 1
	  ENDIF
	  IF (CLIPPING) THEN
	    FLAG(N) = OR(FLAG(N),CLIPPED)
	  ENDIF
	  IF (NBOUNDS.GT.0) FLAG(N) = OR(FLAG(N),BOUNDED)
	  IF (MATCOL) THEN
	    RED = RGBMAT(1)
	    GRN = RGBMAT(2)
	    BLU = RGBMAT(3)
	  ENDIF
	ENDIF
*	Isolated objects not transformed by TMAT, but still subject to inversion
	IF (ISOLATION.GT.0) THEN
	  CALL ISOLATE(X1A,Y1A)
	  CALL ISOLATE(X2A,Y2A)
	ELSE
*	update true coordinate limits
	  TRULIM(1,1) = MIN( TRULIM(1,1), X1A,X2A)
	  TRULIM(1,2) = MAX( TRULIM(1,2), X1A,X2A)
	  TRULIM(2,1) = MIN( TRULIM(2,1), Y1A,Y2A)
	  TRULIM(2,2) = MAX( TRULIM(2,2), Y1A,Y2A)
	  TRULIM(3,1) = MIN( TRULIM(3,1), Z1A,Z2A)
	  TRULIM(3,2) = MAX( TRULIM(3,2), Z1A,Z2A)
*       modify the input, so to speak
          CALL TRANSF (X1A,Y1A,Z1A)
          CALL TRANSF (X2A,Y2A,Z2A)
          R1A = R1A / TMAT(4,4)
          R2A = R2A / TMAT(4,4)
	ENDIF
*       perspective factor for each corner
	IF (EYEPOS.GT.0) THEN
          PFAC1 = PERSP( Z1A )
          PFAC2 = PERSP( Z2A )
	END IF
*       apply perspective
        X1B = X1A * PFAC1
        Y1B = Y1A * PFAC1
        Z1B = Z1A * PFAC1
        R1B = R1A * PFAC1
        X2B = X2A * PFAC2
        Y2B = Y2A * PFAC2
        Z2B = Z2A * PFAC2
        R2B = R2A * PFAC2
*       scale and translate to pixel space
        X1C = X1B * SCALE + XCENT
        Y1C = Y1B * SCALE + YCENT
        Z1C = Z1B * SCALE
        R1C = R1B * SCALE
        X2C = X2B * SCALE + XCENT
        Y2C = Y2B * SCALE + YCENT
        Z2C = Z2B * SCALE
        R2C = R2B * SCALE
*	save transformed Z limits
	ZLIM(1) = MIN( ZLIM(1), Z1C,Z2C )
	ZLIM(2) = MAX( ZLIM(2), Z1C,Z2C )
*	check for Z-clipping
	IF ((MIN(Z1C,Z2C) .GT. FRONTCLIP)
     &  .OR.(MAX(Z1C,Z2C) .LT. BACKCLIP)) THEN
	    JUSTCLIPPED = .TRUE.
	    GOTO 45
	ELSE
	    JUSTCLIPPED = .FALSE.
	ENDIF
*       save results
        DETAIL(NDET+1) = X1C
        DETAIL(NDET+2) = Y1C
        DETAIL(NDET+3) = Z1C
        DETAIL(NDET+4) = R1C
        DETAIL(NDET+5) = X2C
        DETAIL(NDET+6) = Y2C
        DETAIL(NDET+7) = Z2C
	DETAIL(NDET+8) = R2C
* EAM   save anything else?
        DETAIL(NDET+9)  = RED
        DETAIL(NDET+10) = GRN
        DETAIL(NDET+11) = BLU
        CALL ASSERT (KDET(INTYPE).EQ.11,'kdet(1).ne.11')
        NDET = NDET + KDET(INTYPE)
	ncylind = ncylind + 1
*       tally for tiles the object might impinge on
        IXLO = MIN(X1C-R1C,X2C-R2C)  / NPX + 1
        IXHI = MAX(X1C+R1C,X2C+R2C)  / NPX + 1
        IYLO = MIN(Y1C-R1C,Y2C-R2C)  / NPY + 1
        IYHI = MAX(Y1C+R1C,Y2C+R2C)  / NPY + 1
        IF (IXLO.LT.1  ) IXLO=1
        IF (IXLO.GT.NTX) GO TO 711
        IF (IXHI.LT.1  ) GO TO 711
        IF (IXHI.GT.NTX) IXHI=NTX
        IF (IYLO.LT.1  ) IYLO=1
        IF (IYLO.GT.NTY) GO TO 711
        IF (IYHI.LT.1  ) GO TO 711
        IF (IYHI.GT.NTY) IYHI=NTY
        DO 710 IY=IYLO,IYHI
        DO 710 IX=IXLO,IXHI
          KOUNT(IX,IY) = KOUNT(IX,IY) + 1
	  TTRANS(IX,IY) = TTRANS(IX,IY) + ISTRANS
710     CONTINUE
711     CONTINUE
*       repeat for shadow buffer if necessary
        IF (SHADOW) THEN
*         rotate light source to z to take light source viewpoint
          X1R = SROT(1,1)*X1B+SROT(1,2)*Y1B+SROT(1,3)*Z1B
          Y1R = SROT(2,1)*X1B+SROT(2,2)*Y1B+SROT(2,3)*Z1B
          Z1R = SROT(3,1)*X1B+SROT(3,2)*Y1B+SROT(3,3)*Z1B
          X2R = SROT(1,1)*X2B+SROT(1,2)*Y2B+SROT(1,3)*Z2B
          Y2R = SROT(2,1)*X2B+SROT(2,2)*Y2B+SROT(2,3)*Z2B
          Z2R = SROT(3,1)*X2B+SROT(3,2)*Y2B+SROT(3,3)*Z2B
*         scale and translate for shadow space
          X1S = X1R * SCALE + SXCENT
          Y1S = Y1R * SCALE + SYCENT
          Z1S = Z1R * SCALE
          R1S = R1B * SCALE
          X2S = X2R * SCALE + SXCENT
          Y2S = Y2R * SCALE + SYCENT
          Z2S = Z2R * SCALE
          R2S = R2B * SCALE
*         save results etc.
          SDTAIL(MDET+1) = X1S
          SDTAIL(MDET+2) = Y1S
          SDTAIL(MDET+3) = Z1S
          SDTAIL(MDET+4) = R1S
          SDTAIL(MDET+5) = X2S
          SDTAIL(MDET+6) = Y2S
          SDTAIL(MDET+7) = Z2S
	  SDTAIL(MDET+8) = R2S
          CALL ASSERT (SDET(INTYPE).EQ.8,'sdet(1).ne.8')
          MDET = MDET + SDET(INTYPE)
*         tally for shadow tiles the object might impinge on
          IXLO = MIN(X1S-R1S,X2S-R2S)  / NPX + 1
          IXHI = MAX(X1S+R1S,X2S+R2S)  / NPX + 1
          IYLO = MIN(Y1S-R1S,Y2S-R2S)  / NPY + 1
          IYHI = MAX(Y1S+R1S,Y2S+R2S)  / NPY + 1
          IF (IXLO.LT.1  ) IXLO=1
          IF (IXLO.GT.NSX) GO TO 716
          IF (IXHI.LT.1  ) GO TO 716
          IF (IXHI.GT.NSX) IXHI=NSX
          IF (IYLO.LT.1  ) IYLO=1
          IF (IYLO.GT.NSY) GO TO 716
          IF (IYHI.LT.1  ) GO TO 716
          IF (IYHI.GT.NSY) IYHI=NSY
          DO 715 IY=IYLO,IYHI
          DO 715 IX=IXLO,IXHI
            MOUNT(IX,IY) = MOUNT(IX,IY) + 1
715       CONTINUE
716       CONTINUE
        ENDIF
c
c	20-Aug-98 duplicate any transparent flat-ended cylinders so that
c	the inside surface can be rendered also. BUF() is still loaded
c	with specs for the current object; we just need to set flags.
	IF (  AND(FLAG(N),TRANSP).NE.0 .AND. AND(FLAG(N),FLAT).NE.0
     &  .AND. AND(FLAG(N),INSIDE).EQ.0) THEN
	    FLAG(N+1) = FLAT + INSIDE
	    NINSIDE   = NINSIDE + 1
	    GOTO 9
	ENDIF
*
      ELSEIF (INTYPE.EQ.NORMS) THEN
*	vertex normals as given (these belong to previous triangle)
	IF (JUSTCLIPPED) GOTO 46
	IPREV = N - 1
	IF ((TYPE(IPREV).EQ.VERTEXRGB).OR.(TYPE(IPREV).EQ.VERTRANSP))
     &      IPREV = IPREV - 1
	IF ((TYPE(IPREV).EQ.VERTEXRGB).OR.(TYPE(IPREV).EQ.VERTRANSP))
     &      IPREV = IPREV - 1
	CALL ASSERT (TYPE(IPREV).EQ.TRIANG,'orphan normals')
*	Isolated objects not transformed by TMAT, but still subject to inversion
	IF (ISOLATION.GT.0) THEN
          X1C = BUF(1)
          Y1C = BUF(2)
          Z1C = BUF(3)
          X2C = BUF(4)
          Y2C = BUF(5)
          Z2C = BUF(6)
          X3C = BUF(7)
          Y3C = BUF(8)
          Z3C = BUF(9)
	  IF (INVERT) THEN
	    Y1C = -Y1C
	    Y2C = -Y2C
	    Y3C = -Y3C
	  ENDIF
	ELSE
          X1A = BUF(1)
          Y1A = BUF(2)
          Z1A = BUF(3)
          X2A = BUF(4)
          Y2A = BUF(5)
          Z2A = BUF(6)
          X3A = BUF(7)
          Y3A = BUF(8)
          Z3A = BUF(9)
*	Apply rotation matrix, but not translation components
	  X1B = X1A*TMAT(1,1) + Y1A*TMAT(2,1) + Z1A*TMAT(3,1)
	  Y1B = X1A*TMAT(1,2) + Y1A*TMAT(2,2) + Z1A*TMAT(3,2)
	  Z1B = X1A*TMAT(1,3) + Y1A*TMAT(2,3) + Z1A*TMAT(3,3)
	  X2B = X2A*TMAT(1,1) + Y2A*TMAT(2,1) + Z2A*TMAT(3,1)
	  Y2B = X2A*TMAT(1,2) + Y2A*TMAT(2,2) + Z2A*TMAT(3,2)
	  Z2B = X2A*TMAT(1,3) + Y2A*TMAT(2,3) + Z2A*TMAT(3,3)
	  X3B = X3A*TMAT(1,1) + Y3A*TMAT(2,1) + Z3A*TMAT(3,1)
	  Y3B = X3A*TMAT(1,2) + Y3A*TMAT(2,2) + Z3A*TMAT(3,2)
	  Z3B = X3A*TMAT(1,3) + Y3A*TMAT(2,3) + Z3A*TMAT(3,3)
*	Also apply post-rotation, if any
	  X1C = RAFTER(1,1)*X1B + RAFTER(1,2)*Y1B + RAFTER(1,3)*Z1B
	  Y1C = RAFTER(2,1)*X1B + RAFTER(2,2)*Y1B + RAFTER(2,3)*Z1B
	  Z1C = RAFTER(3,1)*X1B + RAFTER(3,2)*Y1B + RAFTER(3,3)*Z1B
	  X2C = RAFTER(1,1)*X2B + RAFTER(1,2)*Y2B + RAFTER(1,3)*Z2B
	  Y2C = RAFTER(2,1)*X2B + RAFTER(2,2)*Y2B + RAFTER(2,3)*Z2B
	  Z2C = RAFTER(3,1)*X2B + RAFTER(3,2)*Y2B + RAFTER(3,3)*Z2B
	  X3C = RAFTER(1,1)*X3B + RAFTER(1,2)*Y3B + RAFTER(1,3)*Z3B
	  Y3C = RAFTER(2,1)*X3B + RAFTER(2,2)*Y3B + RAFTER(2,3)*Z3B
	  Z3C = RAFTER(3,1)*X3B + RAFTER(3,2)*Y3B + RAFTER(3,3)*Z3B
	ENDIF
C
C	If all three Z components are negative, it's facing away from us.
C	Old default treatment was to assume the normals were screwed up.
C	V2.6:     default appropriate cases (e.g. opaque triangles with no 
C		  associated bounding planes) to hidden by assumption and 
C		  thus not needing to be rendered. Mark as HIDDEN.
C		  Default treatment is overridden by CLROPT in material spec
C
	IF (Z1C.GE.0 .AND. Z2C.GE.0 .AND. Z3C.GE.0) GOTO 718
 	IF (Z1C.LT.-.01 .AND. Z2C.LT.-.01 .AND. Z3C.LT.-.01) THEN
	    IF (CLROPT.EQ.2) THEN
		NINSIDE = NINSIDE + 1
		FLAG(IPREV) = OR( FLAG(IPREV), INSIDE )
	    ELSE IF ((AND(FLAG(IPREV),BOUNDED).EQ.0) 
     &          .AND.(CLRITY.EQ.0 .OR. CLROPT.EQ.1)) THEN
		NHIDDEN = NHIDDEN + 1
		FLAG(IPREV) = OR( FLAG(IPREV), HIDDEN )
	    ELSE
		NINSIDE = NINSIDE + 1
		FLAG(IPREV) = OR( FLAG(IPREV), INSIDE )
	    ENDIF
	    GOTO 718
	ENDIF
C
C	Mixed + and - Z means the triangle "wrapped around" the edge.
C	For solid objects the best we can do is pretend the edge is right here.
C	For transparent objects or 2-sided surfaces we need to invert the 
C	normals also.  The value of EDGESLOP is purely empirical; setting it 
C	either too low or too high makes some edges get coloured wrongly.  
C	Setting the HIDDEN flag for this record (NB: for the NORMALS, not for
C	the triangle itself) causes the triangle to have flat shading.
	IF (Z1C+Z2C+Z3C .LT. 0) THEN
	    IF (Z1C .GT. EDGESLOP) FLAG(N) = HIDDEN
	    IF (Z2C .GT. EDGESLOP) FLAG(N) = HIDDEN
	    IF (Z3C .GT. EDGESLOP) FLAG(N) = HIDDEN
	    Z1C = MIN(Z1C,0.)
	    Z2C = MIN(Z2C,0.)
	    Z3C = MIN(Z3C,0.)
	ELSE
	    IF (Z1C .LT. -EDGESLOP) FLAG(N) = HIDDEN
	    IF (Z2C .LT. -EDGESLOP) FLAG(N) = HIDDEN
	    IF (Z3C .LT. -EDGESLOP) FLAG(N) = HIDDEN
	    Z1C = MAX(Z1C,0.)
	    Z2C = MAX(Z2C,0.)
	    Z3C = MAX(Z3C,0.)
	ENDIF
C
718	CONTINUE
	DETAIL(NDET+1) = X1C
	DETAIL(NDET+2) = Y1C
	DETAIL(NDET+3) = Z1C
	DETAIL(NDET+4) = X2C
	DETAIL(NDET+5) = Y2C
	DETAIL(NDET+6) = Z2C
	DETAIL(NDET+7) = X3C
	DETAIL(NDET+8) = Y3C
	DETAIL(NDET+9) = Z3C
	NDET = NDET + KDET(INTYPE)
	IF (SHADOW) THEN
	  MDET = MDET + SDET(INTYPE)
	ENDIF
*
*	Allow specification of RGB triple for each vertex of preceding
*	triangle or cylinder. Overrides base RGB.
*	Also overrides MATERIAL RGB, which is arguably a bug.
*
      ELSEIF (INTYPE .EQ. VERTEXRGB) THEN
	IF (JUSTCLIPPED) GOTO 46
	CALL CHKRGB(BUF(1),BUF(2),BUF(3),'invalid vertex color')
	CALL CHKRGB(BUF(4),BUF(5),BUF(6),'invalid vertex color')
	CALL CHKRGB(BUF(7),BUF(8),BUF(9),'invalid vertex color')
	IPREV = N - 1
	IF ((TYPE(IPREV).EQ.NORMS).OR.(TYPE(IPREV).EQ.VERTRANSP))
     &      IPREV = IPREV - 1
	IF ((TYPE(IPREV).EQ.NORMS).OR.(TYPE(IPREV).EQ.VERTRANSP))
     &      IPREV = IPREV - 1
c	we should only see a SPHERE is if it's a collapsed cylinder
	IF (TYPE(IPREV).EQ.SPHERE) THEN
	    K = LIST(IPREV)
	    DETAIL(K+5) = BUF(1)
	    DETAIL(K+6) = BUF(2)
	    DETAIL(K+7) = BUF(3)
	    GOTO 7
	ENDIF
	CALL ASSERT (TYPE(IPREV).EQ.TRIANG .OR. TYPE(IPREV).EQ.CYLIND,
     &		'orphan vertex colours')
	FLAG(IPREV) = OR( FLAG(IPREV), VCOLS )
	DETAIL(NDET+1) = BUF(1)
	DETAIL(NDET+2) = BUF(2)
	DETAIL(NDET+3) = BUF(3)
	DETAIL(NDET+4) = BUF(4)
	DETAIL(NDET+5) = BUF(5)
	DETAIL(NDET+6) = BUF(6)
	DETAIL(NDET+7) = BUF(7)
	DETAIL(NDET+8) = BUF(8)
	DETAIL(NDET+9) = BUF(9)
	NDET = NDET + KDET(INTYPE)
	IF (SHADOW) THEN
	  MDET = MDET + SDET(INTYPE)
	ENDIF
*
*	EAM - 30-Dec-1999
*	Allow specification of transparency at each vertex of preceding
*	triangle or cylinder. Overrides any MATERIAL properties.
*
      ELSEIF (INTYPE .EQ. VERTRANSP) THEN
	IF (JUSTCLIPPED) GOTO 46
	IPREV = N - 1
	IF (TYPE(IPREV).EQ.NORMS .OR. TYPE(IPREV).EQ.VERTEXRGB)
     &      IPREV = IPREV - 1
	IF (TYPE(IPREV).EQ.NORMS .OR. TYPE(IPREV).EQ.VERTEXRGB)
     &      IPREV = IPREV - 1
	CALL ASSERT (TYPE(IPREV).EQ.TRIANG .OR. TYPE(IPREV).EQ.CYLIND
     &           .OR.TYPE(IPREV).EQ.SPHERE,
     &		'orphan vertex transparency')
	NVTRANS = NVTRANS + 1
	IF (AND(FLAG(IPREV),TRANSP).EQ.0) NTRANSP = NTRANSP + 1
	FLAG(IPREV) = OR( FLAG(IPREV), TRANSP )
	FLAG(IPREV) = OR( FLAG(IPREV), VTRANS )
	DETAIL(NDET+1) = BUF(1)
	DETAIL(NDET+2) = BUF(2)
	DETAIL(NDET+3) = BUF(3)
	NDET = NDET + KDET(INTYPE)
	IF (SHADOW) THEN
	  MDET = MDET + SDET(INTYPE)
	ENDIF
*
*	Material properties are saved after enforcing legality
*
      ELSEIF (INTYPE .EQ. MATERIAL) THEN
*	Mark this object as current material
	MSTATE = MATERIAL
	NPROPM = NPROPM + 1
	CALL ASSERT(NPROPM.LT.MAXMAT,
     &   'too many materials - increase MAXMAT and recompile')
	MLIST(NPROPM) = N
*	Clear any previous material properties
	FLAG(N)  = NPROPM*65536
	MATCOL   = .FALSE.
	CLIPPING = .FALSE.
	NBOUNDS  = 0
	ORTEPLIKE= .FALSE.
	BPRGB(1) = -1
*	Phong power defaults to value in header
	IF (BUF(1).LT.0) BUF(1) = IPHONG
	DETAIL(NDET+1) = BUF(1)
*	Specular reflection component defaults to value in header
	IF (BUF(2).LT.0 .OR. BUF(2).GT.1) BUF(2) = SPECLR
	DETAIL(NDET+2) = BUF(2) 
*	Negative values for specular highlighting indicate default to object
        CALL ASSERT (BUF(3).LE.1., 'red > 1 in material')
        CALL ASSERT (BUF(4).LE.1., 'grn > 1 in material')
        CALL ASSERT (BUF(5).LE.1., 'blu > 1 in material')
	DETAIL(NDET+3) = BUF(3)
	DETAIL(NDET+4) = BUF(4)
	DETAIL(NDET+5) = BUF(5)
	CLRITY = BUF(6)
	CALL ASSERT (CLRITY.GE.0., 'clarity < 0 in material')
	CALL ASSERT (CLRITY.LE.1., 'clarity > 1 in material')
	DETAIL(NDET+6) = CLRITY
*	Transparency processing is necessarily a compromise, and several
*	possible approximations may be useful; allow a choice among them
	CLROPT = BUF(7)
	DETAIL(NDET+7) = BUF(7)
*	The next one is used in conjunction with bounding planes
	DETAIL(NDET+8) = BUF(8)
*	One remaining field reserved for future expansion
	DETAIL(NDET+9) = BUF(9)
*	Initialize clipping planes, only used if CLIPPING is set below
	DETAIL(NDET+16) = FRONTCLIP
	DETAIL(NDET+17) = BACKCLIP
*	Additional properties may continue on extra lines
	IF (INT(BUF(10)).GT.0) THEN
	  DO I = 1,INT(BUF(10))
	  READ (INPUT,'(A)',END=50) LINE
	  DO J = 132, 1, -1
	    IF (LINE(J:J).NE.' '.AND.LINE(J:J).NE.'	') L = J
	  ENDDO
	  IF (LINE(L:L+4).EQ.'SOLID') THEN
	    MATCOL = .TRUE.
	    READ (LINE(L+6:132),*,END=720) RGBMAT(1),RGBMAT(2),RGBMAT(3)
	  ELSE IF (LINE(L:L+7).EQ.'BACKFACE') THEN
	    FLAG(N) = OR(FLAG(N),INSIDE)
	    READ (LINE(L+9:132),*,END=720) RED, GRN, BLU, PHONGM, SPECM
	    MPHONG = PHONGM
	    IF (PHONGM.LT.0) MPHONG = IPHONG
	    IF (SPECM.LT.0.OR.SPECM.GT.1.) SPECM  = SPECLR
	    DETAIL(NDET+11) = RED
	    DETAIL(NDET+12) = GRN
	    DETAIL(NDET+13) = BLU
	    DETAIL(NDET+14) = MPHONG
	    DETAIL(NDET+15) = SPECM
	  ELSE IF (LINE(L:L+8).EQ.'FRONTCLIP') THEN
	    CLIPPING = .TRUE.
	    ZCLIP = FRONTCLIP
	    READ (LINE(L+10:132),*,END=720) ZCLIP
	    ZCLIP = ZCLIP * SCALE / TMAT(4,4)
	    DETAIL(NDET+16) = ZCLIP
	  ELSE IF (LINE(L:L+7).EQ.'BACKCLIP') THEN
	    CLIPPING = .TRUE.
	    ZCLIP = BACKCLIP
	    READ (LINE(L+9:132),*,END=720) ZCLIP
	    ZCLIP = ZCLIP * SCALE / TMAT(4,4)
	    DETAIL(NDET+17) = ZCLIP
   	  ELSE IF (LINE(L:L+9).EQ.'ORTEP_LIKE') THEN
	    ORTEPLIKE = .TRUE.
	  ELSE IF (LINE(L:L+13).EQ.'BOUNDING_COLOR') THEN
	    READ (LINE(L+15:132),*,END=720) BPRGB(1),BPRGB(2),BPRGB(3)
	    CALL CHKRGB(BPRGB(1),BPRGB(2),BPRGB(3),
     &			'Invalid bounding color')
	  ELSE IF (LINE(L:L+13).EQ.'BOUNDING_PLANE') THEN
	    NBOUNDS  = NBOUNDS + 1
	    nbplanes = nbplanes + 1
	    CALL ASSERT(NDET+NBOUNDS*KDET(INTERNAL).LE.MAXDET,'BP Oops')
	    IF (SHADOW) 
     &      CALL ASSERT(MDET+NBOUNDS*SDET(INTERNAL).LE.MAXSDT,'BP Oops')
	    NB = N + NBOUNDS
	    CALL ASSERT( NB.LE.MAXDET, 'BP Oops')
c	    OK, we've established there's room to store this bound;
c	    Flag all properties belonging to the parent material
	      TYPE(NB) = INTERNAL
	      FLAG(NB) = OR(FLAG(N),PROPS)
	      IF(CLRITY.GT.0.0)THEN
		FLAG(NB)=OR(FLAG(NB),TRANSP)
		IF(CLROPT.EQ.1)FLAG(NB)=OR(FLAG(NB),MOPT1)
	      ENDIF
	      NBDET = KDET(MATERIAL) + (NBOUNDS-1)*KDET(INTERNAL)
	      LIST(NB) = NDET + NBDET
	      IF (SHADOW) THEN
	        NBSDT = SDET(MATERIAL) + (NBOUNDS-1)*SDET(INTERNAL)
	      	MIST(NB) = MDET + NBSDT
	      ENDIF
c	    Read in details, transform, and save
	      READ (LINE(L+15:132),*,END=720) BPTYPE,XB,YB,ZB,xn,yn,zn
*	    Transform bounding plane along with objects
	      CALL TRANSF(XB,YB,ZB)
*	    Rotate but don't translate normal, including post-rotation
	      xnb = xn*TMAT(1,1) + yn*TMAT(2,1) + zn*TMAT(3,1)
	      ynb = xn*TMAT(1,2) + yn*TMAT(2,2) + zn*TMAT(3,2)
	      znb = xn*TMAT(1,3) + yn*TMAT(2,3) + zn*TMAT(3,3)
	      xn = RAFTER(1,1)*xnb + RAFTER(1,2)*ynb + RAFTER(1,3)*znb
	      yn = RAFTER(2,1)*xnb + RAFTER(2,2)*ynb + RAFTER(2,3)*znb
	      zn = RAFTER(3,1)*xnb + RAFTER(3,2)*ynb + RAFTER(3,3)*znb
	      temp = sqrt(xn*xn + yn*yn + zn*zn)
	      xn = xn/temp
	      yn = yn/temp
	      zn = zn/temp
	      IF (ORTEPLIKE .AND. ZN.LT.0) THEN
	      	XN = -XN
		YN = -YN
		ZN = -ZN
	      ENDIF
*	    Save data in same arrays as for regular objects
*	    ISUBTYPE currently only used to flag ORTEP_LIKE ellipsoids
*	    BPTYPE is loaded on input but not currently used for anything 
		ISUBTYPE = -1
		IF (ORTEPLIKE) ISUBTYPE = ORTEP
		DETAIL(NDET + NBDET + 1) = ISUBTYPE
		DETAIL(NDET + NBDET + 2) = BPTYPE
		DETAIL(NDET + NBDET + 3) = XB * SCALE + XCENT
		DETAIL(NDET + NBDET + 4) = YB * SCALE + YCENT
		DETAIL(NDET + NBDET + 5) = ZB * SCALE
		DETAIL(NDET + NBDET + 6) = XN
		DETAIL(NDET + NBDET + 7) = YN
		DETAIL(NDET + NBDET + 8) = ZN
c		Most of the time BPRGB(1) is -1 to signal no special color
		DETAIL(NDET + NBDET + 9)  = BPRGB(1)
		DETAIL(NDET + NBDET + 10) = BPRGB(2)
		DETAIL(NDET + NBDET + 11) = BPRGB(3)
	      IF (SHADOW) THEN
	      	XR = SROT(1,1)*XB+SROT(1,2)*YB+SROT(1,3)*ZB
		YR = SROT(2,1)*XB+SROT(2,2)*YB+SROT(2,3)*ZB
		ZR = SROT(3,1)*XB+SROT(3,2)*YB+SROT(3,3)*ZB
	      	BPNORM(1) = SROT(1,1)*XN+SROT(1,2)*YN+SROT(1,3)*ZN
		BPNORM(2) = SROT(2,1)*XN+SROT(2,2)*YN+SROT(2,3)*ZN
		BPNORM(3) = SROT(3,1)*XN+SROT(3,2)*YN+SROT(3,3)*ZN
		SDTAIL(MDET + NBSDT + 1) = ISUBTYPE
		SDTAIL(MDET + NBSDT + 2) = BPTYPE
		SDTAIL(MDET + NBSDT + 3) = XR * SCALE + SXCENT
		SDTAIL(MDET + NBSDT + 4) = YR * SCALE + SYCENT
		SDTAIL(MDET + NBSDT + 5) = ZR * SCALE
		SDTAIL(MDET + NBSDT + 6) = BPNORM(1)
		SDTAIL(MDET + NBSDT + 7) = BPNORM(2)
		SDTAIL(MDET + NBSDT + 8) = BPNORM(3)
	      ENDIF
	  ELSE IF (LINE(L:L+6).EQ.'BUMPMAP') THEN
	    WRITE(NOISE,*) '>> Sorry, no bumpmaps (dont you wish!)'
	  ELSE
	    GOTO 720
	  ENDIF
	  GOTO 721
720	  WRITE(NOISE,'(A,A)') 
     &          '>> Unrecognized or incomplete MATERIAL option ',LINE
721	  CONTINUE
	  ENDDO
	ENDIF
*       Update array pointers for material object itself
	  DETAIL(NDET+18) = NBOUNDS
	  NDET = NDET + KDET(INTYPE)
	  IF (SHADOW) MDET = MDET + SDET(INTYPE)
*	Update array pointers to allow for bounding planes and any other
*	objects inserted by MATERIAL processing
	  NDET = NDET + NBOUNDS*IDET(INTERNAL)
	  IF (SHADOW) MDET = MDET + NBOUNDS*KDET(INTERNAL)
	  N = N + NBOUNDS
*
      ELSEIF (INTYPE.EQ.GLOWLIGHT) THEN
	NGLOWS = NGLOWS + 1
	CALL ASSERT(NGLOWS.LE.MAXGLOWS,'too many glow lights')
	GLOWLIST(NGLOWS) = N
	GLOWSRC(1) = BUF(1)
	GLOWSRC(2) = BUF(2)
	GLOWSRC(3) = BUF(3)
	GLOWRAD    = BUF(4)
	GLOW       = BUF(5)
	GOPT       = BUF(6)
	GPHONG     = BUF(7)
	RED        = BUF(8)
	GRN        = BUF(9)
	BLU        = BUF(10)
	CALL ASSERT (GLOW.GE.0,'illegal glow value')
	CALL ASSERT (GLOW.LE.1,'illegal glow value')
	IF (GLOW.GT.GLOWMAX) GLOWMAX = GLOW
	CALL CHKRGB (RED,GRN,BLU,'invalid glow light color')
*	Isolated objects not transformed by TMAT, but still subject to inversion
	IF (ISOLATION.GT.0) THEN
	  CALL ISOLATE(GLOWSRC(1),GLOWSRC(2))
	ELSE
*	transform coordinates and radius of glow source
	  CALL TRANSF(GLOWSRC(1),GLOWSRC(2),GLOWSRC(3))
	  GLOWRAD = GLOWRAD / TMAT(4,4)
	ENDIF
	IF (EYEPOS.GT.0) PFAC = PERSP( GLOWSRC(3) )
*	save for rendering
	DETAIL(NDET+1)  = GLOWSRC(1) * PFAC * SCALE + XCENT
	DETAIL(NDET+2)  = GLOWSRC(2) * PFAC * SCALE + YCENT
	DETAIL(NDET+3)  = GLOWSRC(3) * PFAC * SCALE
	DETAIL(NDET+4)  = GLOWRAD    * PFAC * SCALE
	DETAIL(NDET+5)  = GLOW
	DETAIL(NDET+6)  = GOPT
	DETAIL(NDET+7)  = GPHONG
	DETAIL(NDET+8)  = RED
	DETAIL(NDET+9)  = GRN
	DETAIL(NDET+10) = BLU
	NDET = NDET + KDET(INTYPE)
	IF (SHADOW) THEN
	  MDET = MDET + SDET(INTYPE)
	ENDIF

      ELSEIF (INTYPE.EQ.QUADRIC) THEN
	NQUADS = NQUADS + 1
	IF (MSTATE.EQ.MATERIAL) THEN
	  FLAG(N) = OR(FLAG(N),PROPS)
	  NPROPS  = NPROPS + 1
	  IF (CLRITY.GT.0) THEN
	    FLAG(N) = OR(FLAG(N),TRANSP)
	    IF (CLROPT.EQ.1) FLAG(N) = OR(FLAG(N),MOPT1)
	    NTRANSP = NTRANSP + 1
  	    ISTRANS = 1
	  ENDIF
	  IF (CLIPPING) THEN
	    FLAG(N) = OR(FLAG(N),CLIPPED)
	  ENDIF
	  IF (NBOUNDS.GT.0) FLAG(N) = OR(FLAG(N),BOUNDED)
	  IF (MATCOL) THEN
	    BUF(5) = RGBMAT(1)
	    BUF(6) = RGBMAT(2)
	    BUF(7) = RGBMAT(3)
	  ENDIF
	ENDIF
*
	ISQUAD = QINP( BUF(1), DETAIL(NDET+1), SHADOW, SDTAIL(MDET+1) )
*
	IF (.NOT. ISQUAD) GOTO 45
	NDET = NDET + KDET(INTYPE)
	IF (SHADOW) THEN
	  MDET = MDET + SDET(INTYPE)
	ENDIF

C
C New object types go here!
C

      ELSEIF (INTYPE.EQ.INTERNAL) THEN
        CALL ASSERT(.FALSE.,'object type 4 not available')
      ELSE
        CALL ASSERT(.FALSE.,'crash 50')
      ENDIF
      GO TO 7
c
c     here to discard object due to clipping planes
c
45    CONTINUE
      NCLIP = NCLIP + 1
46    FLAG(N) = 0
      N = N - 1
      GO TO 7

c
c     26-Aug-1999 attempt error recovery and reporting 
c		  if input line is not recognized
47    continue
      write (noise,'(A,A)') 'Unrecognized line: ',LINE
      goto 7

*
*     here for end of objects
*
50    CONTINUE
      IF (INPUT.GT.INPUT0) THEN
	IF (VERBOSE) WRITE (NOISE,*) ' - closing indirect input file'
	CLOSE(INPUT)
	INPUT = INPUT - 1
	GOTO 7
      ENDIF
*    
*     help people re-center objects
*
	XA = (TRULIM(1,1) + TRULIM(1,2)) / 2.0
	YA = (TRULIM(2,1) + TRULIM(2,2)) / 2.0
	ZA = (TRULIM(3,1) + TRULIM(3,2)) / 2.0
	CALL TRANSF( XA, YA, ZA )
	XA = TMAT(4,1) - XA * TMAT(4,4)
	YA = TMAT(4,2) - YA * TMAT(4,4)
	ZA = TMAT(4,3) - ZA * TMAT(4,4)
	IF (INVERT) YA = -YA
      WRITE (NOISE,*) 'To center objects in rendered scene, ',
     &                'change translation to:'
      WRITE (NOISE,'(3F10.4)') XA, YA, ZA
*
*     Now we can set depth-cueing 
      WRITE (LINE,577)    'Z limits (unscaled) before clipping:',
     &		ZLIM(1)*TMAT(4,4)/SCALE,ZLIM(2)*TMAT(4,4)/SCALE
      WRITE (NOISE,*) LINE(2:57)
      WRITE (LINE,577)    'Z-clipping limits:                  ', 
     &		BACKCLIP*TMAT(4,4)/SCALE
      IF (FRONTCLIP.EQ.HUGE) THEN
      		WRITE (LINE(48:57),'(A10)') '      none'
      ELSE
      		WRITE (LINE(48:57),'(F10.2)') FRONTCLIP*TMAT(4,4)/SCALE
      ENDIF
      WRITE (NOISE,*) LINE(2:57)
      IF (VERBOSE) WRITE (NOISE,*) 'Scale: ', SCALE
      IF (VERBOSE.AND.GAMMACORRECTION) WRITE (NOISE,*) 'Gamma: ', Gamma
      IF (FOGTYPE .GE. 0) THEN
        IF (FOGBACK .EQ. 0) THEN
	    FOGLIM(1) = ZLIM(1)
	ELSE
	    FOGLIM(1) = FOGBACK * BACKCLIP
	ENDIF
	IF (FOGFRONT .EQ. 0) THEN
	    FOGLIM(2) = ZLIM(2)
	ELSE IF (FRONTCLIP .LT. HUGE) THEN
	    FOGLIM(2) = FOGFRONT * FRONTCLIP
	ELSE
	    FOGLIM(2) = FOGFRONT * SCALE
	ENDIF
	IF (FOGTYPE.EQ.1) THEN
	   WRITE(LINE,577) 'Fog (exponential) limits, density:'
	ELSE
	   WRITE(LINE,577) 'Fog (linear) limits, density:     '
	ENDIF
	WRITE (LINE(38:67),578) 
     &	   FOGLIM(1)*TMAT(4,4)/SCALE,FOGLIM(2)*TMAT(4,4)/SCALE,FOGDEN
	WRITE (NOISE,*) LINE(2:67)
      ENDIF
  577 FORMAT(1X,A,2F10.2)
  578 FORMAT(2F10.2,F10.2)
*
*     Check list for special objects 
*     Triangle types first (vanilla/ribbon/surface)
      NRIB = 0
      NSUR = 0
      NTRI = 0
      DO 55 I = 1, N-1
	IF (TYPE(I).EQ.TRIANG) THEN
	  NTRI = NTRI + 1
*	  Allow IPHONG=0 to disable special processing of triangles
	  IF (IPHONG.EQ.0) GOTO 54
*	  Check for surface triangle (explicit normals in next record)
CDEBUG	  "I+1" should be replaced by INEXT processing
	  IF (TYPE(I+1).EQ.NORMS.AND.AND(FLAG(I+1),HIDDEN).EQ.0) THEN
            FLAG(I) = OR( FLAG(I), SURFACE )
	    GOTO 54
	  END IF
	  IF (I.EQ.1) GOTO 54
*	  Check for ribbon triangles,
*	  can't possibly be one unless surrounded by other triangles
	  IPREV = I - 1
	  INEXT = I + 1
	  IF (TYPE(IPREV).NE.TRIANG .OR. TYPE(INEXT).NE.TRIANG) THEN
	    FLAG(I) = AND( FLAG(I), NOT(RIBBON) )
	    GOTO 54
	  END IF
*         trailing vertex must match one in previous triangle
	  J = LIST(IPREV)
	  K = LIST(I)
	  L = LIST(INEXT)
	  DO II = 1, 3
	    KK = K+II
	    if (  abs(detail(kk)-detail(j+ii+3)).gt.RIBBONSLOP
     &	    .and. abs(detail(kk)-detail(j+ii+6)).gt.RIBBONSLOP) goto 54
C	    IF   (DETAIL(KK).NE.DETAIL(J+II+3)
C    &      .AND. DETAIL(KK).NE.DETAIL(J+II+6)) GOTO 54
	  END DO
*         leading vertex must match one in following triangle
	  DO II = 7, 9
	    KK = K+II
	    IF   (abs(DETAIL(KK)-DETAIL(L+II-3)).gt.RIBBONSLOP
     &      .AND. abs(DETAIL(KK)-DETAIL(L+II-6)).gt.RIBBONSLOP) GOTO 54
C	    IF   (DETAIL(KK).NE.DETAIL(L+II-3)
C    &      .AND. DETAIL(KK).NE.DETAIL(L+II-6)) GOTO 54
	  END DO
	  FLAG(I) = OR(FLAG(I),RIBBON)
54	  CONTINUE
	  IF (AND(FLAG(I),RIBBON).NE.0)  NRIB = NRIB + 1
	  IF (AND(FLAG(I),SURFACE).NE.0) NSUR = NSUR + 1
	END IF
55    CONTINUE
      IF (TYPE(N).EQ.TRIANG) NTRI = NTRI + 1
56    CONTINUE
 
*     Set GLOW to maximum requested by glow light sources and bump up
*     ambient contribution to compensate for darkening applied later
      AMBIEN = AMBIEN * (1. + GLOWMAX)
*
      WRITE(NOISE,*)'-------------------------------'
      IF (NSPHERE.NE.0) WRITE(NOISE,57) 'spheres           =',NSPHERE
      IF (NCYLIND.NE.0) WRITE(NOISE,57) 'cylinders         =',NCYLIND
      NTRI = NTRI - (NRIB + NSUR)
      IF (NPLANES.NE.0) WRITE(NOISE,57) 'planes            =',NPLANES
      IF (NTRI.NE.0) WRITE(NOISE,57)    'plain triangles   =',NTRI
      IF (NRIB.NE.0) WRITE(NOISE,57)    'ribbon triangles  =',NRIB
      IF (NSUR.NE.0) WRITE(NOISE,57)    'surface triangles =',NSUR
      IF (NQUADS.NE.0) WRITE(NOISE,57)  'quadric surfaces  =',NQUADS
      IF (NPROPM.NE.0) WRITE(NOISE,57)  'special materials =',NPROPM
      IF (NCLIP.NE.0)   WRITE(NOISE,57) 'Z-clipped objects =',NCLIP
      IF (NTRANSP.NE.0) WRITE(NOISE,57) 'transparent objs  =',NTRANSP
      IF (NHIDDEN.NE.0) WRITE(NOISE,57) 'hidden surfaces   =',NHIDDEN
      IF (NINSIDE.NE.0) WRITE(NOISE,57) 'inside surfaces   =',NINSIDE
      IF (nbplanes.NE.0) WRITE(NOISE,57)'bounding planes   =',nbplanes
      WRITE(NOISE,57)                   'total objects     =',N
      WRITE(NOISE,*)'-------------------------------'
      IF (NGLOWS.GT.0)  WRITE(NOISE,57) 'glow lights       =',NGLOWS
      IF (LFLAG) THEN
	CALL LCLOSE( NLABELS )
      	WRITE(NOISE,57)                 'PostScript labels =',NLABELS
        WRITE(NOISE,*)'-------------------------------'
      ELSEIF (NLABELS.NE.0) THEN
        WRITE(NOISE,57) 'labels (ignored)  =',NLABELS
        WRITE(NOISE,*)'-------------------------------'
      ENDIF
57    FORMAT(2X,A,I8)
*
      ZSLOP = SLOP * MAX(NPX,NPY)
      IF (VERBOSE) THEN
        WRITE (NOISE,*) 'ndet  =',NDET,' MAXDET=',MAXDET
        IF (SHADOW) WRITE (NOISE,*) 'mdet  =',MDET,' MAXSDT=',MAXSDT
	WRITE (NOISE,*) 'EDGESLOP =',EDGESLOP
	WRITE (NOISE,*) '   ZSLOP =',   ZSLOP
      ENDIF
*
*
*     Sort objects, fill in "short lists" as indices into main list
*     (note that it would lend itself better to "parallel
*     processing" to form the short lists first and then
*     sort each one - maybe even slightly more efficient in
*     the present context!)
      DO 60 I = 1, N
        K = LIST(I)
        CALL ASSERT (K.GE.0,'k<0')
        CALL ASSERT (K.LT.NDET,'k>=ndet')
        IF (TYPE(I).EQ.TRIANG) THEN
          Z1 = DETAIL(K+3)
          Z2 = DETAIL(K+6)
          Z3 = DETAIL(K+9)
          ZTEMP(I) = MAX(Z1,Z2,Z3)
        ELSEIF (TYPE(I).EQ.SPHERE) THEN
          Z = DETAIL(K+3)
          R = DETAIL(K+4)
          ZTEMP(I) = Z + R
	ELSEIF (TYPE(I).EQ.CYLIND) THEN
*	  EAM May 1990
	  Z1 = DETAIL(K+3)
	  Z2 = DETAIL(K+7)
	  R1 = DETAIL(K+4)
	  R2 = DETAIL(K+8)
	  ZTEMP(I) = MAX(Z1+R1,Z2+R2)
	ELSEIF (TYPE(I).EQ.PLANE
     &     .OR. TYPE(I).EQ.NORMS 
     &     .OR. TYPE(I).EQ.MATERIAL
     &     .OR. TYPE(I).EQ.VERTRANSP
     &     .OR. TYPE(I).EQ.VERTEXRGB
     &     .OR. TYPE(I).EQ.GLOWLIGHT
     &     .OR. TYPE(I).EQ.INTERNAL) THEN
*	  EAM Mar 1994 (not sure this is necessary)
	  ZTEMP(I) = SCALE + 1.0
        ELSEIF (TYPE(I).EQ.QUADRIC) THEN
          Z = DETAIL(K+3)
          R = DETAIL(K+4)
          ZTEMP(I) = Z + R
        ELSE
          CALL ASSERT(.FALSE.,'crash 60')
        ENDIF
60    CONTINUE
      CALL HSORTD (N, ZTEMP, ZINDEX)
      KNTTOT = 0
      KNTMAX = 0
      DO J = 1, NTY
      DO I = 1, NTX
        KNTTOT = KNTTOT + KOUNT(I,J)
	IF (KOUNT(I,J).GT.KNTMAX) KNTMAX = KOUNT(I,J)
      ENDDO
      ENDDO
      IF (VERBOSE) THEN
	write (noise,*) 'max/avg length of short lists=',
     &                   kntmax,(knttot/(ntx*nty))+1
      	WRITE (NOISE,*) 'knttot=',KNTTOT,' MAXSHR=',MAXSHR
      ENDIF
      CALL ASSERT (KNTTOT.LE.MAXSHR,'short list overflow')
      K = 0
      DO J = 1, NTY
      DO I = 1, NTX
        KSTART(I,J) = K+1
        KSTOP(I,J) = K
        K = K + KOUNT(I,J)
      ENDDO
      ENDDO
      CALL ASSERT (K.EQ.KNTTOT,'k.ne.knttot')
      DO 90 I = 1, N
        IND = ZINDEX(N-I+1)
        CALL ASSERT (IND.GE.1,'ind<1')
        CALL ASSERT (IND.LE.N,'ind>n')
        K = LIST(IND)
        CALL ASSERT (K.GE.0,'k<0')
        CALL ASSERT (K.LT.NDET,'k>=ndet')
*       impingement tests here must be same as above
        IF (TYPE(IND).EQ.TRIANG) THEN
          X1 = DETAIL(K+1)
          Y1 = DETAIL(K+2)
          Z1 = DETAIL(K+3)
          X2 = DETAIL(K+4)
          Y2 = DETAIL(K+5)
          Z2 = DETAIL(K+6)
          X3 = DETAIL(K+7)
          Y3 = DETAIL(K+8)
          Z3 = DETAIL(K+9)
          IXLO = MIN(X1,X2,X3)/NPX + 1
          IXHI = MAX(X1,X2,X3)/NPX + 1
          IYLO = MIN(Y1,Y2,Y3)/NPY + 1
          IYHI = MAX(Y1,Y2,Y3)/NPY + 1
        ELSEIF (TYPE(IND).EQ.SPHERE) THEN
          X = DETAIL(K+1)
          Y = DETAIL(K+2)
          Z = DETAIL(K+3)
          R = DETAIL(K+4)
          IXLO = (X-R)/NPX + 1
          IXHI = (X+R)/NPX + 1
          IYLO = (Y-R)/NPY + 1
          IYHI = (Y+R)/NPY + 1
	ELSEIF (TYPE(IND).EQ.CYLIND) THEN
          X1 = DETAIL(K+1)
          Y1 = DETAIL(K+2)
          Z1 = DETAIL(K+3)
	  R1 = DETAIL(K+4)
          X2 = DETAIL(K+5)
          Y2 = DETAIL(K+6)
          Z2 = DETAIL(K+7)
	  R2 = DETAIL(K+8)
          IXLO = MIN(X1-R1,X2-R2) / NPX + 1
          IXHI = MAX(X1+R1,X2+R2) / NPX + 1
          IYLO = MIN(Y1-R1,Y2-R2) / NPY + 1
          IYHI = MAX(Y1+R1,Y2+R2) / NPY + 1
        ELSEIF (TYPE(IND).EQ.PLANE) THEN
	  IXLO = 1
	  IXHI = NTX
	  IYLO = 1
	  IYHI = NTY
        ELSEIF (TYPE(IND).EQ.NORMS) THEN
	  GOTO 81
        ELSEIF (TYPE(IND).EQ.VERTEXRGB) THEN
	  GOTO 81
        ELSEIF (TYPE(IND).EQ.VERTRANSP) THEN
	  GOTO 81
        ELSEIF (TYPE(IND).EQ.MATERIAL) THEN
	  GOTO 81
        ELSEIF (TYPE(IND).EQ.GLOWLIGHT) THEN
	  GOTO 81
	ELSEIF (TYPE(IND).EQ.QUADRIC) THEN
          X = DETAIL(K+1)
          Y = DETAIL(K+2)
          Z = DETAIL(K+3)
          R = DETAIL(K+4)
          IXLO = (X-R)/NPX + 1
          IXHI = (X+R)/NPX + 1
          IYLO = (Y-R)/NPY + 1
          IYHI = (Y+R)/NPY + 1
        ELSEIF (TYPE(IND).EQ.INTERNAL) THEN
	  GOTO 81
	ELSE
          CALL ASSERT(.FALSE.,'crash 80')
        ENDIF
        IF (IXLO.LT.1  ) IXLO=1
        IF (IXLO.GT.NTX) GO TO 81
        IF (IXHI.LT.1  ) GO TO 81
        IF (IXHI.GT.NTX) IXHI=NTX
        IF (IYLO.LT.1  ) IYLO=1
        IF (IYLO.GT.NTY) GO TO 81
        IF (IYHI.LT.1  ) GO TO 81
        IF (IYHI.GT.NTY) IYHI=NTY
        DO 80 IY=IYLO,IYHI
        DO 80 IX=IXLO,IXHI
          KSTOP(IX,IY) = KSTOP(IX,IY) + 1
          KSHORT(KSTOP(IX,IY)) = IND
80      CONTINUE
81      CONTINUE
90    CONTINUE
      DO 95 J=1,NTY
      DO 95 I=1,NTX
        K1 = KSTART(I,J)
        K2 = KSTOP(I,J)
        K3 = KOUNT(I,J)
        CALL ASSERT (K2-K1.EQ.K3-1,'k2-k1.ne.kount(i,j)-1')
        CALL ASSERT (K1.GE.1.AND.K1.LE.KNTTOT+1,'kstart(i,j)')
        CALL ASSERT (K2.GE.0.AND.K2.LE.KNTTOT,'kstop(i,j)')
95    CONTINUE
*
*     Do the short list business for shadow space too if required
      IF (SHADOW) THEN
        DO 160 I = 1, N
          K = MIST(I)
          CALL ASSERT (K.GE.0,'k.lt.0')
          CALL ASSERT (K.LT.MDET,'k.ge.mdet')
          IF (TYPE(I).EQ.TRIANG) THEN
            Z1 = SDTAIL(K+3)
            Z2 = SDTAIL(K+6)
            Z3 = SDTAIL(K+9)
            ZTEMP(I) = MAX(Z1,Z2,Z3)
          ELSEIF (TYPE(I).EQ.SPHERE) THEN
            Z = SDTAIL(K+3)
            R = SDTAIL(K+4)
            ZTEMP(I) = Z + R
	  ELSEIF (TYPE(I).EQ.CYLIND) THEN
	    Z1 = SDTAIL(K+3)
	    Z2 = SDTAIL(K+7)
	    R1 = SDTAIL(K+4)
	    R2 = SDTAIL(K+8)
	    ZTEMP(I) = MAX(Z1+R1,Z2+R2)
	  ELSEIF (TYPE(I).EQ.PLANE) THEN
*	    no shadows for plane surface
	  ELSEIF (TYPE(I).EQ.NORMS) THEN
*	    and certainly not for normals
	  ELSEIF (TYPE(I).EQ.VERTEXRGB) THEN
	  ELSEIF (TYPE(I).EQ.VERTRANSP) THEN
	  ELSEIF (TYPE(I).EQ.MATERIAL) THEN
*	    or surface properties
	  ELSEIF (TYPE(I).EQ.GLOWLIGHT) THEN
*	    you want a shadow on a light source???
          ELSEIF (TYPE(I).EQ.QUADRIC) THEN
            Z = SDTAIL(K+3)
            R = SDTAIL(K+4)
            ZTEMP(I) = Z + R
	  ELSEIF (TYPE(I).EQ.INTERNAL) THEN
          ELSE
            CALL ASSERT(.FALSE.,'crash 160')
          ENDIF
160     CONTINUE
        CALL HSORTD (N, ZTEMP, ZINDEX)
        MNTTOT = 0
        DO 170 J = 1, NSY
        DO 170 I = 1, NSX
          MNTTOT = MNTTOT + MOUNT(I,J)
170     CONTINUE
        IF (VERBOSE) WRITE (NOISE,*) 'mnttot=',MNTTOT,' MAXSSL=',MAXSSL
        CALL ASSERT (MNTTOT.LE.MAXSSL,'shadow short list overflow')
        K = 0
        DO 175 J = 1, NSY
        DO 175 I = 1, NSX
          MSTART(I,J) = K+1
          MSTOP(I,J) = K
          K = K + MOUNT(I,J)
175     CONTINUE
        CALL ASSERT (K.EQ.MNTTOT,'k.ne.mnttot')
        DO 190 I = 1, N
          IND = ZINDEX(N-I+1)
          CALL ASSERT (IND.GE.1,'ind.lt.1')
          CALL ASSERT (IND.LE.N,'ind.gt.n')
          K = MIST(IND)
          CALL ASSERT (K.GE.0,'k.lt.0')
          CALL ASSERT (K.LT.MDET,'k.ge.mdet')
*         impingement tests here must be same as above
          IF (TYPE(IND).EQ.TRIANG) THEN
            X1 = SDTAIL(K+1)
            Y1 = SDTAIL(K+2)
            Z1 = SDTAIL(K+3)
            X2 = SDTAIL(K+4)
            Y2 = SDTAIL(K+5)
            Z2 = SDTAIL(K+6)
            X3 = SDTAIL(K+7)
            Y3 = SDTAIL(K+8)
            Z3 = SDTAIL(K+9)
            IXLO = MIN(X1,X2,X3)/NPX + 1
            IXHI = MAX(X1,X2,X3)/NPX + 1
            IYLO = MIN(Y1,Y2,Y3)/NPY + 1
            IYHI = MAX(Y1,Y2,Y3)/NPY + 1
          ELSEIF (TYPE(IND).EQ.SPHERE) THEN
            X = SDTAIL(K+1)
            Y = SDTAIL(K+2)
            Z = SDTAIL(K+3)
            R = SDTAIL(K+4)
            IXLO = (X-R)/NPX + 1
            IXHI = (X+R)/NPX + 1
            IYLO = (Y-R)/NPY + 1
            IYHI = (Y+R)/NPY + 1
          ELSEIF (TYPE(IND).EQ.CYLIND) THEN
            X1 = SDTAIL(K+1)
            Y1 = SDTAIL(K+2)
            Z1 = SDTAIL(K+3)
	    R1 = SDTAIL(K+4)
            X2 = SDTAIL(K+5)
            Y2 = SDTAIL(K+6)
            Z2 = SDTAIL(K+7)
	    R2 = SDTAIL(K+8)
            IXLO = MIN(X1-R1,X2-R2) / NPX + 1
            IXHI = MAX(X1+R1,X2+R2) / NPX + 1
            IYLO = MIN(Y1-R1,Y2-R2) / NPY + 1
            IYHI = MAX(Y1+R1,Y2+R2) / NPY + 1
	  ELSEIF (TYPE(IND).EQ.PLANE) THEN
*           no shadows for plane surface
	    GOTO 181
          ELSEIF (TYPE(IND).EQ.NORMS) THEN
	    GOTO 181
          ELSEIF (TYPE(IND).EQ.VERTEXRGB) THEN
	    GOTO 181
          ELSEIF (TYPE(IND).EQ.VERTRANSP) THEN
	    GOTO 181
          ELSEIF (TYPE(IND).EQ.MATERIAL) THEN
	    GOTO 181
          ELSEIF (TYPE(IND).EQ.GLOWLIGHT) THEN
	    GOTO 181
          ELSEIF (TYPE(IND).EQ.QUADRIC) THEN
            X = SDTAIL(K+1)
            Y = SDTAIL(K+2)
            Z = SDTAIL(K+3)
            R = SDTAIL(K+4)
            IXLO = (X-R)/NPX + 1
            IXHI = (X+R)/NPX + 1
            IYLO = (Y-R)/NPY + 1
            IYHI = (Y+R)/NPY + 1
          ELSEIF (TYPE(IND).EQ.INTERNAL) THEN
	    GOTO 181
          ELSE
            CALL ASSERT(.FALSE.,'crash 180')
          ENDIF
          IF (IXLO.LT.1  ) IXLO=1
          IF (IXLO.GT.NSX) GO TO 181
          IF (IXHI.LT.1  ) GO TO 181
          IF (IXHI.GT.NSX) IXHI=NSX
          IF (IYLO.LT.1  ) IYLO=1
          IF (IYLO.GT.NSY) GO TO 181
          IF (IYHI.LT.1  ) GO TO 181
          IF (IYHI.GT.NSY) IYHI=NSY
          DO 180 IY=IYLO,IYHI
          DO 180 IX=IXLO,IXHI
            MSTOP(IX,IY) = MSTOP(IX,IY) + 1
            MSHORT(MSTOP(IX,IY)) = IND
180       CONTINUE
181       CONTINUE
190     CONTINUE
        DO 195 J=1,NSY
        DO 195 I=1,NSX
          K1 = MSTART(I,J)
          K2 = MSTOP(I,J)
          K3 = MOUNT(I,J)
          CALL ASSERT (K2-K1.EQ.K3-1,'k2-k1.ne.mount(i,j)-1')
          CALL ASSERT (K1.GE.1.AND.K1.LE.MNTTOT+1,'mstart(i,j)')
          CALL ASSERT (K2.GE.0.AND.K2.LE.MNTTOT,'mstop(i,j)')
195     CONTINUE
      ENDIF
*
*     Paint the tiles one by one
      DO 600 JTILE = 1, NTY
      DO 500 ITILE = 1, NTX
*       bounds of this tile in pixel space
        XLO = (ITILE-1)*NPX
        XHI = ITILE*NPX
        YLO = (JTILE-1)*NPY
        YHI = JTILE*NPY
*       initialize tile to background colour
        DO 200 J = 1, NPY
        DO 200 I = 1, NPX
        DO 199 IC = 1, 3
          TILE(IC,I,J) = BKGND(IC)
199     CONTINUE
	ACHAN(I,J) = 0.0
200     CONTINUE
*       test for no objects in tile
        IF (KOUNT(ITILE,JTILE).EQ.0) GO TO 400
	NTRANSP = TTRANS(ITILE,JTILE) + nvtrans
	IJSTART = KSTART(ITILE,JTILE)
	IJSTOP  = KSTOP(ITILE,JTILE)
*       process non-empty tile
        DO 300 J = 1, NPY
        DO 300 I = 1, NPX
*         location of the pixel in pixel space
          XP = XLO + 0.5 + (I-1)
          YP = YLO + 0.5 + (J-1)
*         starting value of "highest z so far"
	  ZTOP = BACKCLIP
*         the index of the object that has it
          INDTOP = 0
*	  index of highest opaque object
	  ZHIGH   = ZTOP
*         and number of transparent objects above it
	  INDEPTH = 0
C	  Clear parity counter for all BOUNDED materials
	  IF (NBPLANES.GT.0) THEN
	      DO M = 1, NPROPM
	  	MPARITY(M) = 0
	      ENDDO
	  ENDIF
*         find the highest pixel, using the tile's sorted list
C         DO 240 IK = KSTART(ITILE,JTILE), KSTOP(ITILE,JTILE)
          DO 240 IK = IJSTART, IJSTOP
            IND = KSHORT(IK)
            K     = LIST(IND)
	    IFLAG = FLAG(IND)
*           skip if hidden surface
	    IF (NHIDDEN.GT.0 .AND. AND(IFLAG,HIDDEN).NE.0) goto 240
*	    further tests depend on object type
            IF (TYPE(IND).EQ.TRIANG) THEN
              X1 = DETAIL(K+1)
              Y1 = DETAIL(K+2)
              Z1 = DETAIL(K+3)
              X2 = DETAIL(K+4)
              Y2 = DETAIL(K+5)
              Z2 = DETAIL(K+6)
              X3 = DETAIL(K+7)
              Y3 = DETAIL(K+8)
              Z3 = DETAIL(K+9)
*             cheap check for done pixel
	      IF (Z1.LT.ZHIGH .AND. Z2.LT.ZHIGH .AND. Z3.LT.ZHIGH)
     &		  GOTO 250
              A = DETAIL(K+10)
              B = DETAIL(K+11)
              C = DETAIL(K+12)
              D = DETAIL(K+13)
*             skip object if degenerate triangle
              IF (D.EQ.0) GO TO 240
*             skip object if z not a new high
              ZP = A*XP + B*YP + C
              IF (ZP.LE.ZHIGH) GO TO 240
*             Rigorous test to see if this point is interior to triangle
*	      NOTE: when lots of triangles are present, the following 3 lines
*	      account for the largest single chunk of rendering time (>10%)!
              S = (X2-X1)*(YP-Y1) - (Y2-Y1)*(XP-X1)
              T = (X3-X2)*(YP-Y2) - (Y3-Y2)*(XP-X2)
              U = (X1-X3)*(YP-Y3) - (Y1-Y3)*(XP-X3)
              IF ( (S.LT.0. .OR. T.LT.0. .OR. U.LT.0.) .AND.
     &             (S.GT.0. .OR. T.GT.0. .OR. U.GT.0.) ) GO TO 240
*	      Z-clipped triangles are easy
	      IF (AND(IFLAG,CLIPPED).NE.0) THEN
		MIND = LIST(MLIST(IFLAG/65536))
		IF ( ZP.GT.DETAIL(MIND+16)) GOTO 240
		IF ( ZP.LT.DETAIL(MIND+17)) GOTO 240
	      ENDIF
*	      Use Phong shading for surface and ribbon triangles.
	      IF (AND(IFLAG,SURFACE+VCOLS+VTRANS).NE.0) THEN
		V = (Y3-Y1)*(X2-X1) - (X3-X1)*(Y2-Y1)
		W = (XP-X1)*(Y3-Y1) - (YP-Y1)*(X3-X1)
		ALPHA = W / V
		BETA  = S / V
	      ENDIF
	      IF (AND(IFLAG,VCOLS+VTRANS).NE.0) THEN
		DETAIL(14 + LIST(IND)) = ALPHA
		DETAIL(15 + LIST(IND)) = BETA
	      ENDIF
	      IF (AND(IFLAG,SURFACE).NE.0) THEN
C		CALL ASSERT(TYPE(IND+1).EQ.NORMS,'lost normals')
		A1 = DETAIL(1 + LIST(IND+1))
		B1 = DETAIL(2 + LIST(IND+1))
		C1 = DETAIL(3 + LIST(IND+1))
		A2 = DETAIL(4 + LIST(IND+1))
		B2 = DETAIL(5 + LIST(IND+1))
		C2 = DETAIL(6 + LIST(IND+1))
		A3 = DETAIL(7 + LIST(IND+1))
		B3 = DETAIL(8 + LIST(IND+1))
		C3 = DETAIL(9 + LIST(IND+1))
		TEMPNORM(1) = A1 + ALPHA*(A2-A1) + BETA*(A3-A1)
		TEMPNORM(2) = B1 + ALPHA*(B2-B1) + BETA*(B3-B1)
		TEMPNORM(3) = C1 + ALPHA*(C2-C1) + BETA*(C3-C1)
*	      For ribbon triangles we take this normal for "middle" vertex,
*	      normal of previous triangle for "trailing" vertex normal,
*	      normal of next triangle for "leading" vertex normal.
*	      Then we use linear interpolation of vertex normals.
	      ELSE IF (AND(IFLAG,RIBBON).NE.0) THEN
		IPREV = IND - 1
		INEXT = IND + 1
C		CALL ASSERT(TYPE(IPREV).EQ.TRIANG,'lost triangle')
C		CALL ASSERT(TYPE(INEXT).EQ.TRIANG,'lost triangle')
		V = (Y3-Y1)*(X2-X1) - (X3-X1)*(Y2-Y1)
		W = (XP-X1)*(Y3-Y1) - (YP-Y1)*(X3-X1)
		ALPHA = W / V
		BETA  = S / V
		AT = DETAIL(10 + LIST(IPREV))
		BT = DETAIL(11 + LIST(IPREV))
		AL = DETAIL(10 + LIST(INEXT))
		BL = DETAIL(11 + LIST(INEXT))
		TEMPNORM(1) = -AT -ALPHA*(A-AT) -BETA*(AL-AT)
		TEMPNORM(2) = -BT -ALPHA*(B-BT) -BETA*(BL-BT)
		TEMPNORM(3) = 1.
	      ELSE
              	TEMPNORM(1) = -A
              	TEMPNORM(2) = -B
              	TEMPNORM(3) = 1.
	      END IF
*	      Check bounding planes.
*	      This is different for triangles than for other shapes, as we assume
*	      that each triangle is only a facet of a larger shape that is really
*	      the 'object' being bounded. This means that rather than checking top
*	      and bottom surfaces of the current object, we have to search for 
*	      them in other triangle/facets of the same bounded material.
	      IF (AND(IFLAG,BOUNDED).NE.0) THEN
	        MAT = IFLAG/65536
		M = MLIST(MAT)+1
		IF (.NOT.INBOUNDS( M, TYPE, LIST, DETAIL,
     &		         XP,YP,ZP,DX,DY,DZ,ZP, BPIND )) THEN
		   GOTO 240
		ENDIF
c		If this surface was above bounding plane, track parity
		MIND = LIST(MLIST(MAT))
		IF (BPIND.NE.0) THEN
		    IF (MPARITY(MAT).EQ.0) THEN
			MPARITY(MAT) = 1
			IF (ZP.LE.ZHIGH) GO TO 240
	            	TEMPNORM(1) = DX
	            	TEMPNORM(2) = DY
	            	TEMPNORM(3) = DZ
CORTEP		      Very ugly code to force bounding plane colors to be used
CORTEP		      but only if they are present.
			IF ( (BPIND.GT.0)
     &			.AND.(DETAIL(LIST(BPIND)+9).GE.0.0))
     &				IND = BPIND
		    ELSE
			MPARITY(MAT) = 0
			IF (FLAG(INDTOP)/65536.EQ.MAT) THEN
			  IF (AND(IFLAG,TRANSP).EQ.0) THEN
     			    INDTOP = 0
			    ZHIGH  = BACKCLIP
			    ZTOP   = BACKCLIP
			    INDEPTH= 0
			  ELSE IF (INDEPTH.LE.1) THEN
     			    INDTOP = 0
			    ZHIGH  = BACKCLIP
			    ZTOP   = BACKCLIP
			    INDEPTH= 0
			  ELSE
			    INDEPTH = INDEPTH - 1
			    DO L = 1, INDEPTH
			      INDLIST(L) = INDLIST(L+1)
			      ZLIST(L)   = ZLIST(L+1)
			      NORMLIST(1,L) = NORMLIST(1,L+1)
			      NORMLIST(2,L) = NORMLIST(2,L+1)
			      NORMLIST(3,L) = NORMLIST(3,L+1)
			    ENDDO
			    ZTOP    = ZLIST(1)
			  ENDIF
			ENDIF
			GOTO 240
		    ENDIF
		ENDIF
	      ENDIF
*             update values for object having highest z here yet
*	      19-Feb-2002 Must wait til here to set NORMAL
		NORMAL(1) = TEMPNORM(1)
		NORMAL(2) = TEMPNORM(2)
		NORMAL(3) = TEMPNORM(3)
	      IF (NTRANSP.GT.0) THEN
		CALL RANK( IND, ZP, NORMAL, FLAG )
	      ELSE
		ZHIGH  = ZP
		INDTOP = IND
	      ENDIF
            ELSEIF (TYPE(IND).EQ.SPHERE) THEN
              X = DETAIL(K+1)
              Y = DETAIL(K+2)
              Z = DETAIL(K+3)
              R = DETAIL(K+4)
*             cheap check for done pixel
              IF (Z+R.LE.ZHIGH) GO TO 250
*             skip object if point exterior
              DX = XP-X
              DY = YP-Y
              DX2 = DX**2
              DY2 = DY**2
              R2 = R**2
              IF (DX2+DY2 .GE. R2) GO TO 240
*             skip object if z not a new high
              DZ = SQRT(R2-(DX2+DY2))
C	      Triggered by CLROPT=2
	      IF (AND(IFLAG,TRANSP).NE.0 .AND.
     &	          AND(IFLAG,INSIDE).NE.0)       DZ = -DZ
              ZP = Z+DZ
              IF (ZP.LE.ZHIGH) GO TO 240
*	      Check bounding planes.
	      IF (AND(IFLAG,BOUNDED).NE.0) THEN
	        ZBACK = Z-DZ
		M = MLIST(IFLAG/65536)+1
		IF (.NOT.INBOUNDS( M, TYPE, LIST, DETAIL,
     &		         XP,YP,ZP,DX,DY,DZ,ZBACK, BPIND))
     &		   GOTO 240
	      ENDIF
*	      Z-clipped spheres aren't too bad
	      IF (AND(IFLAG,CLIPPED).NE.0) THEN
		MIND = LIST(MLIST(IFLAG/65536))
		IF (ZP.GT.DETAIL(MIND+16)) THEN
		  ZP = Z-DZ
		  IF (ZP.LE.ZHIGH) GOTO 240
		  IF (ZP.GT.DETAIL(MIND+16)) GOTO 240
		  DZ = -DZ
		ENDIF
		IF (ZP.LT.DETAIL(MIND+17)) GOTO 240
	      ENDIF
*             update values for object having highest z here yet
              NORMAL(1) = DX
              NORMAL(2) = DY
              NORMAL(3) = DZ
	      IF (NTRANSP.GT.0) THEN
		CALL RANK( IND, ZP, NORMAL, FLAG )
	      ELSE
		ZHIGH  = ZP
		INDTOP = IND
	      ENDIF
	    ELSEIF (TYPE(IND).EQ.CYLIND) THEN
*             EAM May 1990
              X1 = DETAIL(K+1)
              Y1 = DETAIL(K+2)
              Z1 = DETAIL(K+3)
              R1 = DETAIL(K+4)
              X2 = DETAIL(K+5)
              Y2 = DETAIL(K+6)
              Z2 = DETAIL(K+7)
	      R2 = R1
*	      EAM Mar 1993 with a better understanding of how this works
*	      add truly cheap test for cylinder entirely below current ZTOP
	      TEMP1 = MAX(Z1+R1,Z2+R2)
	      IF (TEMP1 .LE. ZHIGH) GOTO 250
*             2nd (relatively cheap) test
*	      is to check distance to cylinder axis in projection
		IF (X1.EQ.X2 .AND. Y1.EQ.Y2) THEN
			TEMP1 = 0.0
		ELSE
			TEMP1 = ((XP-X1)*(Y2-Y1) - (YP-Y1)*(X2-X1))**2
     &			      / ((X2-X1)*(X2-X1) + (Y2-Y1)*(Y2-Y1))
		ENDIF
		IF (TEMP1 .GT. R1*R1) GOTO 240
*	      Now find Z coord in pixel space of point on surface of
*	      cylinder with these X and Y coords (ZP)
*	      Also get coords of closest point on cylinder axis (XYZA).
		ISCYL = CYLTEST( IFLAG, AXFRAC,
     &			   X1,Y1,Z1, X2,Y2,Z2, XP,YP,ZP, R1, XA,YA,ZA )
		IF (.NOT.ISCYL) GO TO 240
*             skip object if z not a new high
              IF (ZP.LE.ZHIGH) GO TO 240
              DX = XP - XA
              DY = YP - YA
              DZ = ZP - ZA
*	      Check bounding planes. Unfortunately we have to get the
*	      back surface first which means dummying up a call to CYLTEST
	      IF (AND(IFLAG,BOUNDED).NE.0) THEN
	        ZBACK = ZP
		ISCYL = CYLTEST( OR(IFLAG,INSIDE+TRANSP), AXFRAC,
     &		        X1,Y1,Z1, X2,Y2,Z2, XP,YP,ZBACK, R1, XA,YA,ZA)
		M = MLIST(IFLAG/65536)+1
		IF (.NOT.INBOUNDS( M, TYPE, LIST, DETAIL,
     &		         XP,YP,ZP,DX,DY,DZ,ZBACK, BPIND ))
     &		   GOTO 240
	      ENDIF
*	      Z-clipped cylinders are messy
	      IF (AND(IFLAG,CLIPPED).NE.0) THEN
		MIND = LIST(MLIST(IFLAG/65536))
		IF (ZP.GT.DETAIL(MIND+16)) THEN
		  ISCYL = CYLTEST( OR(IFLAG,INSIDE+TRANSP), AXFRAC,
     &			   X1,Y1,Z1, X2,Y2,Z2, XP,YP,ZP, R1, XA,YA,ZA )
		  IF (ZP.LE.ZHIGH) GOTO 240
		  IF (ZP.GT.DETAIL(MIND+16)) GOTO 240
		ENDIF
		IF (ZP.LT.DETAIL(MIND+17)) GOTO 240
                DX = XP - XA
                DY = YP - YA
                DZ = ZP - ZA
	      ENDIF
              NORMAL(1) = DX
              NORMAL(2) = DY
              NORMAL(3) = DZ
*	      if explicit vertex colors, need to keep fractional position
	      IF (AND(IFLAG,OR(VCOLS,VTRANS)).NE.0) DETAIL(K+8) = AXFRAC
*             update values for object having highest z here yet
	      IF (NTRANSP.GT.0) THEN
		CALL RANK( IND, ZP, NORMAL, FLAG )
	      ELSE
		ZHIGH  = ZP
		INDTOP = IND
	      ENDIF
	    ELSEIF (TYPE(IND).EQ.PLANE) THEN
	      A = DETAIL(K+1)
	      B = DETAIL(K+2)
	      C = DETAIL(K+3)
	      D = DETAIL(K+4)
	      IF (D.EQ.0) GOTO 240
	      ZP = A*XP + B*YP + C
	      IF (ZP.LE.ZHIGH) GOTO 240
	      NORMAL(1) = -A
	      NORMAL(2) = -B
	      NORMAL(3) = 1.
	      IF (NTRANSP.GT.0) THEN
		CALL RANK( IND, ZP, NORMAL, FLAG )
	      ELSE
		ZHIGH  = ZP
		INDTOP = IND
	      ENDIF
	    ELSEIF (TYPE(IND).EQ.QUADRIC) THEN
*	      First do cheap checks against projection of limiting sphere
		X = DETAIL(K+1)
		Y = DETAIL(K+2)
		Z = DETAIL(K+3)
		R = DETAIL(K+4)
		IF (Z+R.LE.ZHIGH) GO TO 250
		DX2 = (XP-X)**2
		DY2 = (YP-Y)**2
		R2 = R**2
		IF (DX2 + DY2 .GE. R2) GO TO 240
*	      Now find Z coord (ZP) in pixel space of point on quadric surface
*	      with these X and Y coords
		ISQUAD = QTEST( DETAIL(K+1), DETAIL(K+8), 
     &                          XP, YP, ZP, QNORM, .FALSE., .FALSE. )
		IF (.NOT.ISQUAD) GO TO 240
		IF (ZP.LE.ZHIGH) GO TO 240
*	      Check bounding planes.
	        IF (AND(IFLAG,BOUNDED).NE.0) THEN
		  M = MLIST(IFLAG/65536)
		  IF (DETAIL(LIST(M+1)+1) .EQ. ORTEP) THEN
		    CALL ORTEPBOUNDS( M+1, TYPE, LIST, DETAIL, XP,YP,ZP,
     &		       QNORM(1),QNORM(2),QNORM(3),ZBACK, BPIND)
CORTEP		  Very ugly code to force bounding plane colors to be used
CORTEP		  but only if they are present.
CORTEP		  An alternative would be to always set IND = BPIND, but
CORTEP		  check for presence of coloring info later, in which case
CORTEP		  IND itself needs to be temporarily saved somewhere.
CORTEP		  Or maybe just cache BPIND now and use it later if non-zero?
		    IF ( (BPIND.GT.0)
     &		       .AND.(DETAIL(LIST(BPIND)+9).GE.0.0))
     &			  IND = BPIND
		  ELSE
		    ISQUAD = QTEST( DETAIL(K+1), DETAIL(K+8), 
     &                          XP, YP, ZBACK, QNORM, .FALSE., .TRUE. )
		    IF (.NOT.INBOUNDS( M+1, TYPE, LIST, DETAIL, XP,YP,ZP,
     &		       QNORM(1),QNORM(2),QNORM(3),ZBACK, BPIND))
     &		       GOTO 240
		  ENDIF
	        ENDIF
C	      Z-clipping of quadric surfaces is encountered more frequently
C	      than for other object types, as the limiting sphere can also 
C	      cause clipping.
C	      Check against limiting sphere in 3D
		MAYCLIP = .FALSE.
		DZ2 = (ZP-Z)**2
		IF (DX2+DY2+DZ2 .GT. R2) MAYCLIP = .TRUE.
		IF (AND(IFLAG,CLIPPED).NE.0) THEN
		  MIND = LIST(MLIST(IFLAG/65536))
		  IF (ZP.GT.DETAIL(MIND+16)) MAYCLIP = .TRUE.
		ENDIF
		IF (MAYCLIP) THEN
		    ISQUAD = QTEST( DETAIL(K+1), DETAIL(K+8), 
     &                          XP, YP, ZP, QNORM, .FALSE., .TRUE. )
		    IF (.NOT.ISQUAD) GO TO 240
		    IF (ZP.LE.ZHIGH) GO TO 240
		    DZ2 = (ZP-Z)**2
 		    IF (DX2+DY2+DZ2 .GT. R2) GO TO 240
		    IF (AND(IFLAG,CLIPPED).NE.0) THEN
			IF (ZP.GT.DETAIL(MIND+16)) GO TO 240
			IF (ZP.LT.DETAIL(MIND+17)) GO TO 240
		    ENDIF
		ENDIF
		NORMAL(1) = QNORM(1)
		NORMAL(2) = QNORM(2)
		NORMAL(3) = QNORM(3)
*             update values for object having highest z here yet
		IF (NTRANSP.GT.0) THEN
		  CALL RANK( IND, ZP, NORMAL, FLAG )
		ELSE
		  ZHIGH  = ZP
		  INDTOP = IND
		ENDIF
            ELSE
              CALL ASSERT(.FALSE.,'crash 240')
            ENDIF
240       CONTINUE
250       CONTINUE
*         Background colour if we never found an object in this line of sight
          IF (INDTOP.EQ.0) GO TO 299
C	  We now know this is not a background pixel so set alpha channel to 1
C	  Modify later if it turns out the object is transparent
	  ACHAN(I,J) = 1.0
C         Transparency processing revamped Mar 2001
C	  If the top object is transparent we will have to come back here
C         later and do this all again for each object in INDLIST
          IF (NTRANSP.NE.0) THEN
C	      CALL ASSERT(INDEPTH.GT.0,'INDEPTH = 0')
	      ITPASS = 1
	      ZHIGH  = ZLIST(INDEPTH)
	      INDTOP = INDLIST(INDEPTH)
	      NORMAL(1) = NORMLIST(1,INDEPTH)
	      NORMAL(2) = NORMLIST(2,INDEPTH)
	      NORMAL(3) = NORMLIST(3,INDEPTH)
	  ENDIF
*         ZP is the "height" of the chosen pixel,
*         and indtop tells us which object it came from:
	  ZTOP = ZHIGH
	  ZP = ZTOP
255       CONTINUE
C
C	  Shadowing code - look for objects that shadow the one we just found
C
          IF (SHADOW) THEN
*           locate pixel in shadow space
*           take out object translation & scaling
            XT = (XP - XCENT) / SCALE
            YT = (YP - YCENT) / SCALE
            ZT = ZP / SCALE
*           rotate light source position to z axis
            XR = SROT(1,1)*XT+SROT(1,2)*YT+SROT(1,3)*ZT
            YR = SROT(2,1)*XT+SROT(2,2)*YT+SROT(2,3)*ZT
            ZR = SROT(3,1)*XT+SROT(3,2)*YT+SROT(3,3)*ZT
*           scale and translate for shadow space
            XS = XR * SCALE + SXCENT
            YS = YR * SCALE + SYCENT
            ZS = ZR * SCALE
*           determine appropriate shadow tile
            ISTILE = XS/NPX + 1
            JSTILE = YS/NPY + 1
*	    Just to get proper error message
	      IF (JSTILE.LE.0) JSTILE = NSY + 1 - JSTILE
	      IF (ISTILE.LE.0) ISTILE = NSX + 1 - ISTILE
	    IF (JSTILE.GE.NSY) THEN
	      NSYMAX = MAX(JSTILE,NSYMAX)
              INDSTP = 0
	      GOTO 270
	    END IF
	    IF (ISTILE.GE.NSX) THEN
	      NSXMAX = MAX(ISTILE,NSXMAX)
              INDSTP = 0
	      GOTO 270
	    END IF
*           starting value of "highest shadow space z so far"
*           and the index of the object that has it
            ZSTOP = 2.0*BACKCLIP
            INDSTP = 0
*
            DO 260 IK = MSTART(ISTILE,JSTILE), MSTOP(ISTILE,JSTILE)
              IND   = MSHORT(IK)
	      IFLAG = FLAG(IND)
*             Ignore transparent objects except for the one being shaded
	      IF (AND(IFLAG,TRANSP).NE.0 .AND. IND.NE.INDTOP) GOTO 260
              K = MIST(IND)
              IF (TYPE(IND).EQ.TRIANG) THEN
                X1 = SDTAIL(K+1)
                Y1 = SDTAIL(K+2)
                Z1 = SDTAIL(K+3)
                X2 = SDTAIL(K+4)
                Y2 = SDTAIL(K+5)
                Z2 = SDTAIL(K+6)
                X3 = SDTAIL(K+7)
                Y3 = SDTAIL(K+8)
                Z3 = SDTAIL(K+9)
                A = SDTAIL(K+10)
                B = SDTAIL(K+11)
                C = SDTAIL(K+12)
                D = SDTAIL(K+13)
*               cheap check for done "pixel"
		IF (Z1.LT.ZSTOP .AND. Z2.LT.ZSTOP .AND. Z3.LT.ZSTOP)
     &		    GOTO 270
*               skip object if degenerate triangle
                IF (D.EQ.0) GO TO 260
*               skip object if z not a new high
                ZTEST = A*XS + B*YS + C
                IF (ZTEST.LE.ZSTOP) GO TO 260
*               skip object if point exterior
                S = (X2-X1)*(YS-Y1)-(Y2-Y1)*(XS-X1)
                T = (X3-X2)*(YS-Y2)-(Y3-Y2)*(XS-X2)
                U = (X1-X3)*(YS-Y3)-(Y1-Y3)*(XS-X3)
                IF ( (S.LT.0. .OR. T.LT.0. .OR. U.LT.0.) .AND.
     &               (S.GT.0. .OR. T.GT.0. .OR. U.GT.0.) ) GO TO 260
*		Check bounding planes
		IF (AND(IFLAG,BOUNDED).NE.0) THEN
		  MAT = IFLAG/65536
		  M = MLIST(MAT)+1
		  IF (.NOT.INBOUNDS( M, TYPE, MIST, SDTAIL,
     &		           XS,YS,ZTEST,DX,DY,DZ,ZTEST, BPIND )) THEN
			GOTO 260
		  ENDIF
		  MIND = MIST(MLIST(MAT))
		  IF (BPIND.NE.0) THEN
		      IF (MPARITY(MAT).GE.0) THEN
			MPARITY(MAT) = -1
		      ELSE
			MPARITY(MAT) = 0
			IF (FLAG(INDSTP)/65536.EQ.MAT) THEN
			  INDSTP = 0
			  ZSTOP  = 2.*BACKCLIP
			ENDIF
			GOTO 260
		      ENDIF
		  ENDIF
		ENDIF
*               update values for object having highest z here yet
                ZSTOP = ZTEST
                INDSTP = IND
              ELSEIF (TYPE(IND).EQ.SPHERE) THEN
                X = SDTAIL(K+1)
                Y = SDTAIL(K+2)
                Z = SDTAIL(K+3)
                R = SDTAIL(K+4)
*               cheap check for done "pixel"
                IF (Z+R.LT.ZSTOP) GO TO 270
*               skip object if point exterior
                DX = XS-X
                DY = YS-Y
                DX2 = DX**2
                DY2 = DY**2
                R2 = R**2
                IF (DX2+DY2 .GE. R2) GO TO 260
*               skip object if z not a new high
                DZ = SQRT(R2-(DX2+DY2))
                ZTEST = Z+DZ
                IF (ZTEST.LE.ZSTOP) GO TO 260
*	        Check bounding planes.
	        IF (AND(IFLAG,BOUNDED).NE.0) THEN
	          ZBACK = Z-DZ
		  M = MLIST(IFLAG/65536)+1
		  IF (.NOT.INBOUNDS( M, TYPE, MIST, SDTAIL,
     &		           XS,YS,ZTEST,DX,DY,DZ,ZBACK, BPIND))
     &		     GOTO 260
	        ENDIF
*               update values for object having highest z here yet
                ZSTOP = ZTEST
                INDSTP = IND
	      ELSEIF (TYPE(IND).EQ.CYLIND) THEN
*	        EAM May 1990
                X1 = SDTAIL(K+1)
                Y1 = SDTAIL(K+2)
                Z1 = SDTAIL(K+3)
                R1 = SDTAIL(K+4)
                X2 = SDTAIL(K+5)
                Y2 = SDTAIL(K+6)
                Z2 = SDTAIL(K+7)
		R2 = R1
*		EAM Feb 93 - Test first to see if entire cylinder is below 
*		current top object in shadow space
		IF (MAX( Z1+R1, Z2+R2 ) .LT. ZSTOP) GOTO 270
*	        Now find Z coord (ZTEST) in pixel space of point on
*	        surface of cylinder with these X and Y coords
		ISCYL = CYLTEST( IFLAG, AXFRAC,
     &		   	X1,Y1,Z1, X2,Y2,Z2, XS,YS,ZTEST, R1, XA,YA,ZA )
		IF (.NOT.ISCYL) GO TO 260
*               skip object if z not a new high
                IF (ZTEST.LE.ZSTOP) GO TO 260
*	        Check bounding planes.
	        IF (AND(IFLAG,BOUNDED).NE.0) THEN
		  ISCYL = CYLTEST( OR(IFLAG,INSIDE+TRANSP), AXFRAC,
     &		          X1,Y1,Z1, X2,Y2,Z2, XS,YS,ZBACK, R1, XA,YA,ZA)
		  M = MLIST(IFLAG/65536)+1
		  IF (.NOT.INBOUNDS( M, TYPE, MIST, SDTAIL,
     &		           XS,YS,ZTEST,DX,DY,DZ,ZBACK, BPIND ))
     &		   GOTO 260
	        ENDIF
*               update values for object having highest z here yet
                ZSTOP = ZTEST
                INDSTP = IND
	      ELSEIF (TYPE(IND).EQ.QUADRIC) THEN
                X = SDTAIL(K+1)
                Y = SDTAIL(K+2)
                Z = SDTAIL(K+3)
                R = SDTAIL(K+4)
*               cheap check against limiting sphere
                IF (Z+R.LT.ZSTOP) GO TO 270
                DX = XS-X
                DY = YS-Y
                R2 = R**2
	        IF (DX**2 + DY**2 .GE. R2) GO TO 260
*	        Now find Z coord (ZTEST) in shadow pixel space of point on
*	        surface with these X and Y coords
		ISQUAD = QTEST( SDTAIL(K+1), SDTAIL(K+5), 
     &                          XS, YS, ZTEST, QNORM, .TRUE., .FALSE. )
CDEBUG                          XS, YS, ZTEST, QNORM, .TRUE., .TRUE. )
CDEBUG		16-Dec-1998 I inverted the BACKSIDE = TRUE/FALSE flags from
CDEBUG		what they "ought" to be to remove buggy shadows from a test
CDEBUG		case parabolic hyperboloid.  I don't understand why this would be
CDEBUG		necessary, and worry a bit that it breaks something else.
CDEBUG
		IF (.NOT.ISQUAD) GO TO 260
*               skip object if z not a new high
                IF (ZTEST.LE.ZSTOP) GO TO 260
*	        Check bounding planes.
	        IF (AND(IFLAG,BOUNDED).NE.0) THEN
		  M = MLIST(IFLAG/65536)
		  IF (DETAIL(LIST(M)+1) .EQ. ORTEP) THEN
		    ISQUAD = QTEST( SDTAIL(K+1), SDTAIL(K+5),
     &                     XS, YS, ZBACK, QNORM, .TRUE., .TRUE. )
		    IF (.NOT.INBOUNDS(M+1, 
     &			   TYPE, MIST, SDTAIL, XS,YS,ZTEST,
     &		           QNORM(1),QNORM(2),QNORM(3),ZBACK, BPIND))
     &		       GOTO 260
		  ENDIF
	        ENDIF
*		Test against bounding sphere in 3D
*		and if surface nearest to light is clipped, check back also
		DZ = ZTEST-Z
		IF (DX**2 + DY**2 + DZ**2 .GE. R2) THEN
		    ISQUAD = QTEST( SDTAIL(K+1), SDTAIL(K+5),
     &                          XS, YS, ZTEST, QNORM, .TRUE., .TRUE. )
CDEBUG                          XS, YS, ZTEST, QNORM, .TRUE., .FALSE. )
		    IF (.NOT.ISQUAD) GO TO 260
		    IF (ZTEST.LE.ZSTOP) GO TO 260
		    DZ = ZTEST - Z
		    IF (DX**2 + DY**2 + DZ**2 .GE. R2) GO TO 260
		ENDIF
*               update values for object having highest z here yet
                ZSTOP = ZTEST
                INDSTP = IND
C	      No more legal object types; should never happen
              ELSE
                CALL ASSERT(.FALSE.,'shadow tile error, crash 260')
              ENDIF
260         CONTINUE
270         CONTINUE
C	  End of search for objects that shadow this one
	    if ((zstop+zslop).lt.zs .and. indstp.ne.0) nslow = nslow + 1
          ELSE
            ZS = 0.
            ZSTOP = 0.
            INDSTP = INDTOP
          ENDIF
*         if roundoff made us miss the object, we are probably
*         at a pixel that is very near the edge of the object
*         from the point of view of the light source, so just
*         treat it as if not in shadow
          IF (INDSTP.EQ.0) THEN
            ZS = 0.
            ZSTOP = 0.
            INDSTP = INDTOP
          ENDIF
*
*         Pick up colours of object to be shaded
*
          K = LIST(INDTOP)
          IF (TYPE(INDTOP).EQ.TRIANG) THEN
	    IF (AND(FLAG(INDTOP),VCOLS).NE.0) THEN
	      ALPHA = DETAIL(K+14)
	      BETA  = DETAIL(K+15)
	      INEXT = INDTOP + 1
	      IF ((TYPE(INEXT).EQ.NORMS).OR.(TYPE(INEXT).EQ.VERTRANSP))
     &            INEXT = INEXT + 1
	      IF ((TYPE(INEXT).EQ.NORMS).OR.(TYPE(INEXT).EQ.VERTRANSP))
     &            INEXT = INEXT + 1
	      K = LIST(INEXT)
	      CALL ASSERT(TYPE(INEXT).EQ.VERTEXRGB,'lost vertex colors')
	      RGBCUR(1) = DETAIL(K+1) 
     &			+ ALPHA*(DETAIL(K+4)-DETAIL(K+1))     
     &			+  BETA*(DETAIL(K+7)-DETAIL(K+1))     
	      RGBCUR(2) = DETAIL(K+2) 
     &			+ ALPHA*(DETAIL(K+5)-DETAIL(K+2))     
     &			+  BETA*(DETAIL(K+8)-DETAIL(K+2))     
	      RGBCUR(3) = DETAIL(K+3) 
     &			+ ALPHA*(DETAIL(K+6)-DETAIL(K+3))     
     &			+  BETA*(DETAIL(K+9)-DETAIL(K+3))     
	    ELSE
              RGBCUR(1) = DETAIL(K+14)
              RGBCUR(2) = DETAIL(K+15)
              RGBCUR(3) = DETAIL(K+16)
	    ENDIF
          ELSEIF (TYPE(INDTOP).EQ.SPHERE) THEN
            RGBCUR(1) = DETAIL(K+5)
            RGBCUR(2) = DETAIL(K+6)
            RGBCUR(3) = DETAIL(K+7)
	  ELSEIF (TYPE(INDTOP).EQ.CYLIND) THEN
	    IF (AND(FLAG(INDTOP),VCOLS).NE.0) THEN
	      FRAC = DETAIL(K+8)
	      INEXT = INDTOP + 1
	      IF (TYPE(INEXT).EQ.VERTRANSP) INEXT = INEXT + 1
	      K = LIST(INEXT)
	      CALL ASSERT(TYPE(INEXT).EQ.VERTEXRGB,'lost vertex colors')
              RGBCUR(1) = FRAC*DETAIL(K+4) + (1.-FRAC)*DETAIL(K+1)
              RGBCUR(2) = FRAC*DETAIL(K+5) + (1.-FRAC)*DETAIL(K+2)
              RGBCUR(3) = FRAC*DETAIL(K+6) + (1.-FRAC)*DETAIL(K+3)
	    ELSE
              RGBCUR(1) = DETAIL(K+9)
              RGBCUR(2) = DETAIL(K+10)
              RGBCUR(3) = DETAIL(K+11)
	    ENDIF
*	  EAM Mar 1993 PLANE is shaded from full colour in foreground
*	               to half-saturation at horizon
	  ELSEIF (TYPE(INDTOP).EQ.PLANE) THEN
	    FADE = (ZP + 3.*SCALE) / (4.*SCALE)
	    RGBCUR(1) = FADE * DETAIL(K+5) + (1.-FADE) * BKGND(1)
	    RGBCUR(2) = FADE * DETAIL(K+6) + (1.-FADE) * BKGND(2)
	    RGBCUR(3) = FADE * DETAIL(K+7) + (1.-FADE) * BKGND(3)
	  ELSEIF (TYPE(INDTOP).EQ.QUADRIC) THEN
            RGBCUR(1) = DETAIL(K+5)
            RGBCUR(2) = DETAIL(K+6)
            RGBCUR(3) = DETAIL(K+7)
	  ELSEIF (TYPE(INDTOP).EQ.INTERNAL) THEN
            RGBCUR(1) = DETAIL(K+9)
            RGBCUR(2) = DETAIL(K+10)
            RGBCUR(3) = DETAIL(K+11)
          ELSE
	    WRITE(LINE,*) 'Top object claims to be type',TYPE(INDTOP)
            CALL ASSERT(.FALSE.,LINE)
          ENDIF
*
*       Get shading components.
*

*
*         11-May-1997 As of now, treat negative normal(3) as indicating the
*	  back side of a material.  Default is to shrug and invert the normal.
*	  Some material have explicit BACKFACE proterties, however.
*
	  BACKFACE = .FALSE.
          IF (NORMAL(3).LE.0) THEN
            NORMAL(1) = -NORMAL(1)
            NORMAL(2) = -NORMAL(2)
            NORMAL(3) = -NORMAL(3)
	    BACKFACE = .TRUE.
	    IF (AND(FLAG(INDTOP),PROPS).NE.0) THEN
		K = FLAG(INDTOP) / 65536
		CALL ASSERT(K.GT.0,'lost material definition')
		IF (AND(FLAG(MLIST(K)),INSIDE).NE.0) THEN
		  K = LIST(MLIST(K))
		  RGBCUR(1) = DETAIL(K+11)
		  RGBCUR(2) = DETAIL(K+12)
		  RGBCUR(3) = DETAIL(K+13)
		END IF
	    END IF
	  END IF
*
          ABSN = SQRT(NORMAL(1)**2 + NORMAL(2)**2 + NORMAL(3)**2)
c	  CALL ASSERT(ABS(ABSN-1.0).LT.0.02,'>> Abnormal normal')
          NL(1) = NORMAL(1) / ABSN
          NL(2) = NORMAL(2) / ABSN
          NL(3) = NORMAL(3) / ABSN
          SDIFF = NL(3) * STRAIT*DIFFUS
          SSP = 2.*NL(3)**2 - 1.
*         We do the value check like this to avoid floating-point underflows 
*	  in the Phonging.  We also save calculation time this way, because
*	  the 0 case will occur often for reasonably high Phong powers.
*	  Note that PHOBND**IPHONG should evaluate to the cutoff value
*         between significant and insignificant specular contribution. 
*         The contributions that are actually computed here
*         can be smaller by a factor of STRAIT*SPECLR:
	  IF (SSP.LT.PHOBND .OR. IPHONG.EQ.0) THEN
            SSPEC = 0.
          ELSE
            SSPEC = SSP**IPHONG * STRAIT*SPECLR
          ENDIF
          LDOTN = SOURCE(1)*NL(1)+SOURCE(2)*NL(2)+SOURCE(3)*NL(3)
          IF (LDOTN.LE.0.) THEN
            PDIFF = 0.
            PSPEC = 0.
          ELSE
            PDIFF = LDOTN * PRIMAR*DIFFUS
            PSP = 2.*LDOTN*NL(3) - SOURCE(3)
*           Comments as for SSPEC apply, but substitute PRIMAR for STRAIT:
	    IF (PSP.LT.PHOBND .OR. IPHONG.EQ.0) THEN
              PSPEC = 0.
            ELSE
              PSPEC = PSP**IPHONG * PRIMAR*SPECLR
            ENDIF
          ENDIF
*
*         experience has shown the "spots" on dark objects to be rather
*         overpowering, especially by comparison to those on brighter
*         objects.  hence the specular reflections on dark objects are
*         now artificially scaled down by a function which relates
*         directly to the "brightness" of the object.
*         this makes such objects duller, but their
*         colour seems to come through more clearly, and they don't
*         appear more specular than the brighter objects.
*         the funny coefficients are ntsc:
          BRIGHT = 0.2 + 0.8 * SQRT(0.299*RGBCUR(1) +
     &                              0.587*RGBCUR(2) +
     &                              0.114*RGBCUR(3))
          SSPEC = SSPEC * BRIGHT
          PSPEC = PSPEC * BRIGHT
*
*	  The usual case is white lighting, no transparency
*
	  SPECOL(1) = 1.0
	  SPECOL(2) = 1.0
	  SPECOL(3) = 1.0
	  SBLEND    = 1.0
	  CLRITY    = 0.0
*
*	  Extra properties make specular highlighting calculation a
*	  bit more complex. First we have to find the MATERIAL description.
*
	  IF (AND(FLAG(INDTOP),PROPS).NE.0) THEN
	    K = FLAG(INDTOP) / 65536
	    CALL ASSERT(K.GT.0,'lost material definition')
	    IF (AND(FLAG(MLIST(K)),INSIDE).NE.0  .AND.
     &		AND(FLAG(INDTOP),  INSIDE).NE.0) THEN
	      K = LIST(MLIST(K))
	      MPHONG  = DETAIL(K+14)
	      SPECM   = DETAIL(K+15)
	    ELSE
	      K = LIST(MLIST(K))
	      MPHONG  = DETAIL(K+1)
	      SPECM   = DETAIL(K+2)
	    ENDIF
	    SPECOL(1) = DETAIL(K+3)
	    SPECOL(2) = DETAIL(K+4)
	    SPECOL(3) = DETAIL(K+5)
	    IF (SPECOL(1).LT.0) SPECOL(1) = RGBCUR(1)
	    IF (SPECOL(2).LT.0) SPECOL(2) = RGBCUR(2)
	    IF (SPECOL(3).LT.0) SPECOL(3) = RGBCUR(3)
	    CLRITY    = DETAIL(K+6)
*	    not currently used, as MOPT(1)=1 already marked in FLAG,
*	    but future interpretations of MOPT(1) might need it
	    CLROPT    = DETAIL(K+7)
	  ENDIF
*
*	  20-Feb-2000 Allow per-vertex transparency (obj type 18)
	  IF (AND(FLAG(INDTOP),VTRANS).NE.0) THEN
	      IF (TYPE(INDTOP).EQ.CYLIND) THEN
                K = LIST(INDTOP)
	        FRAC = DETAIL(K+8)
	        INEXT = INDTOP + 1
  	        IF (TYPE(INEXT).EQ.VERTEXRGB) INEXT = INEXT + 1
	        CALL ASSERT(TYPE(INEXT).EQ.VERTRANSP,'lost vertex transp')
	        K = LIST(INEXT)
	        CLRITY = FRAC*DETAIL(K+1) + (1.-FRAC)*DETAIL(K+2)
	      ELSE IF (TYPE(INDTOP).EQ.TRIANG) THEN
	        K = LIST(INDTOP)
		ALPHA = DETAIL(K+14)
		BETA  = DETAIL(K+15)
		INEXT = INDTOP + 1
		IF ((TYPE(INEXT).EQ.VERTEXRGB).OR.(TYPE(INEXT).EQ.NORMS))
     &              INEXT = INEXT + 1
		IF ((TYPE(INEXT).EQ.VERTEXRGB).OR.(TYPE(INEXT).EQ.NORMS))
     &              INEXT = INEXT + 1
     		CALL ASSERT(TYPE(INEXT).EQ.VERTRANSP,'lost vertex transp')
		K = LIST(INEXT)
		CLRITY = DETAIL(K+1)
     &		       + ALPHA * (DETAIL(K+2)-DETAIL(K+1))
     &		       + BETA  * (DETAIL(K+3)-DETAIL(K+1))
     		CALL ASSERT(CLRITY.LE.1 .AND. CLRITY.GE.0.0, 'illegal transp')
	      ELSE
	        CALL ASSERT(.FALSE.,'illegal vertex transp')
	      ENDIF
	  ENDIF

C
C	EAM February 1996
C	This is the only computationally intensive code (as opposed to mere
C	bookkeeping) involved in rendering transparent objects. The blend
C	factor must be some function of the clarity/transparency, but I'm not
C	sure exactly what the equation ought to be.  The cosine
C	function below was chosen after purely empirical tests of the
C	resulting image quality. If your machine bogs down incredibly due to
C	the cosine call, then comment out that line and uncomment the line
C	that currently begins with C-ALT.
C	Then re-compile (type "make render") and you should be all set.
C
	  IF (CLRITY.NE.0) THEN
            SBLEND = .25*(1.+COS(3.1416*CLRITY*NL(3)))**2
C-ALT	    SBLEND = (1. - CLRITY*ABS(NL(3)))**2
	  ENDIF
C
C	  Final calculation of specular properties of special materials
C
	  IF (AND(FLAG(INDTOP),PROPS).NE.0) THEN
	    DIFFM     = 1. - (SPECM + AMBIEN)
	    SDIFF     = SDIFF * DIFFM / DIFFUS
	    PDIFF     = PDIFF * DIFFM / DIFFUS
	    SSPEC     = 0.0
	    PSPEC     = 0.0
	    IF (SSP .GE. PHOBND) 
     &          SSPEC = SSP**MPHONG * STRAIT*SPECM
	    IF ((PSP .GE. PHOBND) .AND. (LDOTN.GT.0))
     &          PSPEC = PSP**MPHONG * PRIMAR*SPECM
*	    de-emphasize highlights from inside surface of transparent objects
*	    Could use BACKFACE flag instead of INSIDE to catch non-triangles
	    IF (AND(FLAG(INDTOP),INSIDE).NE.0) THEN
		SSPEC = SSPEC * (1.-SPECM)
		PSPEC = PSPEC * (1.-SPECM)
	    ENDIF
	  END IF
*
*         We now return you to your regular processing
*
          DO 280 KC = 1, 3
            C2ND = SBLEND*RGBCUR(KC)*(AMBIEN+SDIFF) + SSPEC*SPECOL(KC)
            CSUN = SBLEND*RGBCUR(KC)*PDIFF          + PSPEC*SPECOL(KC)
            RGBSHD(KC) = C2ND
            RGBFUL(KC) = C2ND + CSUN
280       CONTINUE

C EAM March 1997 - Support additional non-shadowing light sources
C which lie within figure and have a finite range of illumination.
	  IF (NGLOWS.GT.0) THEN
	    DO KC = 1,3
	        RGBSHD(KC) = (1.-GLOWMAX)*RGBSHD(KC)
	        RGBFUL(KC) = (1.-GLOWMAX)*RGBFUL(KC)
	    ENDDO
*	  Recover glow light parameters
	  DO NG = 1, NGLOWS
              IG = LIST(GLOWLIST(NG))
	      GLOWSRC(1) = DETAIL(IG+1)
	      GLOWSRC(2) = DETAIL(IG+2)
	      GLOWSRC(3) = DETAIL(IG+3)
	      GLOWRAD    = DETAIL(IG+4)
	      GLOW       = DETAIL(IG+5)
	      GOPT       = DETAIL(IG+6)
	      GPHONG     = DETAIL(IG+7)
	      GLOWCOL(1) = DETAIL(IG+8)
	      GLOWCOL(2) = DETAIL(IG+9)
	      GLOWCOL(3) = DETAIL(IG+10)
*
	      GDIST(1) = GLOWSRC(1) - XP
	      GDIST(2) = GLOWSRC(2) - YP
	      GDIST(3) = GLOWSRC(3) - ZP
	      ABSN = SQRT(GDIST(1)**2 + GDIST(2)**2 + GDIST(3)**2)
	      GDIST(1) = GDIST(1) / ABSN
	      GDIST(2) = GDIST(2) / ABSN
	      GDIST(3) = GDIST(3) / ABSN
	      LDOTN = GDIST(1)*NL(1) + GDIST(2)*NL(2) + GDIST(3)*NL(3) 
	      IF (LDOTN.LE.0) THEN
	        GDIFF = 0.
	        GSPEC = 0.
	      ELSE
C 		Might want separate diffuse param for glow; (always 1.0?)
C 	        GDIFF = LDOTN * DIFFUS
	        GDIFF = LDOTN
	        GSP   = 2.*LDOTN*NL(3) - GDIST(3)
	        IF (GSP.LT.PHOBND .OR. GPHONG.EQ.0) THEN
		    GSPEC = 0.
	        ELSE
		    GSPEC = GSP**GPHONG * SPECLR
	        ENDIF
	      ENDIF
C 	    Limit glow effect by some function of ABSN, GLOWRAD 
	      IF (GOPT.EQ.3) THEN
		GFADE = MAX( 0., 1. - ABSN/GLOWRAD )
	      ELSE IF (GOPT.EQ.2) THEN
		GFADE = 1./(ABSN/GLOWRAD + 1.)
	      ELSE IF (GOPT.EQ.1) THEN
		GFADE = 1./(ABSN/GLOWRAD + 1.)**2
	      ELSE
		GFADE = MIN( 1., 1./(ABSN/GLOWRAD)**2 )
	      ENDIF
	      DO KC = 1,3
C 		This isn't right for transparent surfaces
	        CGLO = SBLEND*RGBCUR(KC)*GDIFF + GSPEC
		CGLO = GFADE * GLOWCOL(KC) * CGLO
	        RGBSHD(KC) = RGBSHD(KC) + CGLO
	        RGBFUL(KC) = RGBFUL(KC) + CGLO
	      ENDDO
*	    End of this glow light
	    ENDDO
          ENDIF
*
*         That does it for the shading computation.
*         ZS should still be a shadow-space co-ordinate of the pixel 
*         whose shading we were interested in, and zstop should be a
*         shadow-space object's co-ordinate no further than that from
*         the primary light source (modulo the empirical slop factor).
*
          IF (INDTOP.EQ.INDSTP) THEN
            TILE(1,I,J) = RGBFUL(1)
            TILE(2,I,J) = RGBFUL(2)
            TILE(3,I,J) = RGBFUL(3)
          ELSE IF (ZS+ZSLOP.GE.ZSTOP) THEN
	    nzslop = nzslop + 1
            TILE(1,I,J) = RGBFUL(1)
            TILE(2,I,J) = RGBFUL(2)
            TILE(3,I,J) = RGBFUL(3)
          ELSE
            TILE(1,I,J) = RGBSHD(1)
            TILE(2,I,J) = RGBSHD(2)
            TILE(3,I,J) = RGBSHD(3)
          ENDIF
C
C       Transparency processing totally overhauled Feb 2001
C	The first pass is sufficient if top object is opaque.
	  IF (NTRANSP .EQ. 0) GOTO 299
	  IF (INDEPTH.EQ.1 .AND. AND(FLAG(INDTOP),TRANSP).EQ.0) GOTO 299
	  IF (ITPASS.EQ.1) THEN
	      RGBLND(1)  = (1.-SBLEND)*BKGND(1) + TILE(1,I,J)
	      RGBLND(2)  = (1.-SBLEND)*BKGND(2) + TILE(2,I,J)
	      RGBLND(3)  = (1.-SBLEND)*BKGND(3) + TILE(3,I,J)
	      ACHAN(I,J) = SBLEND
	  ELSE
	      RGBLND(1)  = (1.-SBLEND)*RGBLND(1) + TILE(1,I,J)
	      RGBLND(2)  = (1.-SBLEND)*RGBLND(2) + TILE(2,I,J)
	      RGBLND(3)  = (1.-SBLEND)*RGBLND(3) + TILE(3,I,J)
	      ACHAN(I,J) = 1. - (1.-ACHAN(I,J))*(1.-SBLEND)
	  ENDIF
C	  CALL ASSERT(ITPASS.LE.INDEPTH,'Ran off end of INDEPTH')
	  IF (ITPASS.GE.INDEPTH) THEN
	      TILE(1,I,J) = RGBLND(1)
	      TILE(2,I,J) = RGBLND(2)
	      TILE(3,I,J) = RGBLND(3)
	      ZTOP = ZP
	  ELSE
	      ZP        = ZLIST(INDEPTH-ITPASS)
	      INDTOP    = INDLIST(INDEPTH-ITPASS)
	      NORMAL(1) = NORMLIST(1,INDEPTH-ITPASS)
	      NORMAL(2) = NORMLIST(2,INDEPTH-ITPASS)
	      NORMAL(3) = NORMLIST(3,INDEPTH-ITPASS)
	      ITPASS    = ITPASS + 1
	      GOTO 255
	  ENDIF
C	End of transparency processing
C
299	CONTINUE

C
C	Fog processing added July 1998
C	Should have glow lights brighten fog?
	IF (FOGTYPE .GE. 0) THEN
     	    FOGDIM = FOGGY( FOGLIM(2) - ZTOP )
	    TILE(1,I,J) = (1.-FOGDIM)*TILE(1,I,J) + FOGDIM*FOGRGB(1)
	    TILE(2,I,J) = (1.-FOGDIM)*TILE(2,I,J) + FOGDIM*FOGRGB(2)
	    TILE(3,I,J) = (1.-FOGDIM)*TILE(3,I,J) + FOGDIM*FOGRGB(3)
	ENDIF

C
300     CONTINUE
400     CONTINUE
*       do tile averaging and save output tile in outbuf
C	For now fold schemes 0 and 1 together; later split for efficiency?
        IF (SCHEME.LE.1) THEN
          K = (ITILE-1)*NOX
          DO 420 J = 1, NOY
          DO 415 I = 1, NOX
          K = K + 1
C         CALL ASSERT (K.LE.OUTSIZ,'k>outsiz')
C
          DO 410 IC = 1, 3
            ICK = 256. * SQRT(TILE(IC,I,J))
            IF (ICK.LT.0) ICK = 0
            IF (ICK.GT.255) ICK = 255
	    IF (GAMMACORRECTION) ICK = GAMMA_MAP(ICK+1)
            OUTBUF(K,IC) = ICK
  410     CONTINUE
C
	  IF (SCHEME.EQ.0) THEN
	    ICK = 255. * ACHAN(I,J)
	    IF (ICK.LT.0)   ICK = 0
	    IF (ICK.GT.255) ICK = 255
	    OUTBUF(K,4) = ICK
	  END IF
C
415       CONTINUE
          K = K + NOX*(NTX-1)
420       CONTINUE
        ELSEIF (SCHEME.EQ.2) THEN
          K = (ITILE-1)*NOX
          DO 440 J = 1, NOY
          DO 435 I = 1, NOX
          K = K + 1
          DO 430 IC = 1, 3
*           I'm not quite convinced by this pixel averaging
*           (is a corner worth too much in this setup?):
            TMP = (TILE(IC,2*I-1,2*J-1) +
     &             TILE(IC,2*I  ,2*J-1) +
     &             TILE(IC,2*I-1,2*J  ) +
     &             TILE(IC,2*I  ,2*J  )) / 4.
            ICK = 256. * SQRT(TMP)
            IF (ICK.LT.0) ICK = 0
            IF (ICK.GT.255) ICK = 255
	    IF (GAMMACORRECTION) ICK = GAMMA_MAP(ICK+1)
            OUTBUF(K,IC)   = ICK
430       CONTINUE
435       CONTINUE
          K = K + NOX*(NTX-1)
440       CONTINUE
        ELSEIF (SCHEME.EQ.3) THEN
          NHX = NOX/2
          NHY = NOY/2
          K = (ITILE-1)*NOX
          DO 460 J = 1, NHY
          DO 455 I = 1, NHX
          DO 450 IC = 1, 3
*           Bad pixel averaging?:
            TMP1 = (TILE(IC,3*I-2,3*J-2)    +
     &              TILE(IC,3*I-1,3*J-2)/2. +
     &              TILE(IC,3*I-2,3*J-1)/2. +
     &              TILE(IC,3*I-1,3*J-1)/4.) / (9./4.)
            TMP2 = (TILE(IC,3*I-1,3*J-2)/2. +
     &              TILE(IC,3*I  ,3*J-2)    +
     &              TILE(IC,3*I-1,3*J-1)/4. +
     &              TILE(IC,3*I  ,3*J-1)/2.) / (9./4.)
            TMP3 = (TILE(IC,3*I-2,3*J-1)/2. +
     &              TILE(IC,3*I-1,3*J-1)/4. +
     &              TILE(IC,3*I-2,3*J  )    +
     &              TILE(IC,3*I-1,3*J  )/2.) / (9./4.)
            TMP4 = (TILE(IC,3*I-1,3*J-1)/4. +
     &              TILE(IC,3*I  ,3*J-1)/2. +
     &              TILE(IC,3*I-1,3*J  )/2. +
     &              TILE(IC,3*I  ,3*J  )   ) / (9./4.)
            ICK1 = MIN(MAX(INT(256.*SQRT(TMP1)),0),255)
            ICK2 = MIN(MAX(INT(256.*SQRT(TMP2)),0),255)
            ICK3 = MIN(MAX(INT(256.*SQRT(TMP3)),0),255)
            ICK4 = MIN(MAX(INT(256.*SQRT(TMP4)),0),255)
	    IF (GAMMACORRECTION) THEN
	    	ICK1 = GAMMA_MAP(ICK1+1)
	    	ICK2 = GAMMA_MAP(ICK2+1)
	    	ICK3 = GAMMA_MAP(ICK3+1)
	    	ICK4 = GAMMA_MAP(ICK4+1)
	    ENDIF
            OUTBUF(   K+1,IC) = ICK1
            OUTBUF(   K+2,IC) = ICK2
            OUTBUF(NX+K+1,IC) = ICK3
            OUTBUF(NX+K+2,IC) = ICK4
450       CONTINUE
          K = K + 2
455       CONTINUE
          K = K + NOX*(2*NTX - 1)
460       CONTINUE
        ELSE
          CALL ASSERT(.FALSE.,'crash 500')
        ENDIF
500   CONTINUE
*     Ready to write when we have completed a row of tiles
      K = 0
      DO 550 J=1,NOY
	LINOUT = LINOUT + 1
	IF (LINOUT.GT.NAY) GOTO 600
	IERR = LOCAL ( 2, OUTBUF(K+1,1), OUTBUF(K+1,2), OUTBUF(K+1,3),
     &                    OUTBUF(K+1,4) )
        K = K + NX
C       CALL ASSERT (K.LE.OUTSIZ,'k>outsiz')
550   CONTINUE
600   CONTINUE
*
*     Report any soft failures
      IF (  NSXMAX.GT.0 .OR. NSYMAX.GT.0 
     & .OR. TRANOVFL.GT.0  ) THEN
	WRITE(NOISE,*)'   >>> WARNINGS <<<'
      END IF
      IF (NSXMAX.GT.0) WRITE(NOISE,*)
     &                '   Possible shadow error NSXMAX=',NSXMAX      
      IF (NSYMAX.GT.0) WRITE(NOISE,*)
     &                '   Possible shadow error NSYMAX=',NSYMAX      
      IF (TRANOVFL.GT.0) WRITE(NOISE,601)
     &  '   Transparency processing truncated at MAXTRANSP=',
     &  MAXTRANSP, '  for', TRANOVFL,' pixels'
601	FORMAT(A,I3,A,I10,A)
*
*     Debugging information
      if (verbose) then
	if (nzslop.gt.0) write(noise,*)'   NZSLOP failures=',nzslop
	if (nslow.gt.0)  write(noise,*)'   NSLOW  failures=',nslow
      endif
*
*     close up shop
      IERR = LOCAL(3)
*
      END

      SUBROUTINE TRANSF (X,Y,Z)
*     Input transformation
      COMMON /MATRICES/ XCENT, YCENT, SCALE, EYEPOS, SXCENT, SYCENT,
     &                  TMAT, TINV, TINVT, SROT, SRTINV, SRTINVT
     &                 ,RAFTER, TAFTER
      REAL   XCENT, YCENT, SCALE, EYEPOS, SXCENT, SYCENT
*     Transformation matrix, inverse of transpose, and transposed inverse
      REAL   TMAT(4,4), TINV(4,4), TINVT(4,4)
*     Shortest rotation from light source to +z axis
      REAL   SROT(4,4), SRTINV(4,4), SRTINVT(4,4)
*     Post-hoc transformation on top of original TMAT
      REAL   RAFTER(4,4), TAFTER(3)
      REAL   X,Y,Z
      REAL   G(4),H(4)
      H(1) = X*TMAT(1,1) + Y*TMAT(2,1) + Z*TMAT(3,1) + TMAT(4,1)
      H(2) = X*TMAT(1,2) + Y*TMAT(2,2) + Z*TMAT(3,2) + TMAT(4,2)
      H(3) = X*TMAT(1,3) + Y*TMAT(2,3) + Z*TMAT(3,3) + TMAT(4,3)
      H(4) = X*TMAT(1,4) + Y*TMAT(2,4) + Z*TMAT(3,4) + TMAT(4,4)
*     Apply post-hoc rotation and translation also
      G(1) = RAFTER(1,1)*H(1) + RAFTER(1,2)*H(2) + RAFTER(1,3)*H(3)
     &     + TAFTER(1)
      G(2) = RAFTER(2,1)*H(1) + RAFTER(2,2)*H(2) + RAFTER(2,3)*H(3)
     &     + TAFTER(2)
      G(3) = RAFTER(3,1)*H(1) + RAFTER(3,2)*H(2) + RAFTER(3,3)*H(3)
     &     + TAFTER(3)
      CALL ASSERT (H(4).NE.0.,'infinite vector')
      X = G(1) / H(4)
      Y = G(2) / H(4)
      Z = G(3) / H(4)
      RETURN
      END

      SUBROUTINE ISOLATE( X, Y )
*     Expand X and Y coordinates to fill image regardless of aspect ratio
      COMMON /OPTIONS/ FONTSCALE, GAMMA, ZOOM, NSCHEME, SHADOWFLAG, 
     &                 NAX, NAY, OTMODE, QUALITY, INVERT, LFLAG
      REAL             FONTSCALE, GAMMA, ZOOM
      INTEGER          NSCHEME, SHADOWFLAG
      INTEGER*2        NAX, NAY, OTMODE, QUALITY
      LOGICAL*2        INVERT, LFLAG
      COMMON /MATRICES/ XCENT, YCENT, SCALE, EYEPOS, SXCENT, SYCENT,
     &                  TMAT, TINV, TINVT, SROT, SRTINV, SRTINVT
     &                 ,RAFTER, TAFTER
      REAL   XCENT, YCENT, SCALE, EYEPOS, SXCENT, SYCENT
      COMMON /NICETIES/ TRULIM,      ZLIM,    FRONTCLIP, BACKCLIP
     &                , ISOLATION
      REAL              TRULIM(3,2), ZLIM(2), FRONTCLIP, BACKCLIP
      INTEGER           ISOLATION
*
      IF (INVERT) Y = -Y
      IF (ISOLATION.EQ.2) THEN
          ASPECT = XCENT/YCENT
	  IF (ASPECT.GT.1.0) X = X * ASPECT
	  IF (ASPECT.LT.1.0) Y = Y / ASPECT
      ENDIF
      RETURN
      END



      SUBROUTINE HSORTD (N, A, NDEX)
      INTEGER N
      REAL   A(N)
      INTEGER NDEX(N)
*
*     this formulation of heapsort is based on n. wirth,
*     "algorithms + data structures = programs" (p. 75).
*
*     the caller supplies an array, a, containing n elements, and an
*     index array with space for n integers.
*     a and n are considered "read-only" by the subroutine, but ndex
*     is filled by the subroutine with the sequence of indices of a
*     that obtain the elements of a in ascending order.  this is
*     similar to the apl unary "tree" operator.  thus a(ndex(1)) is the
*     smallest element after the sort, and a(ndex(n)) is the largest.
*
      INTEGER L, R, T
      DO 10 I = 1, N
10    NDEX(I) = I
      L = N/2 + 1
      R = N
20    IF (L .LE. 1) GO TO 30
      L = L - 1
      CALL HSIFTD (N, A, NDEX, L, R)
      GO TO 20
30    IF (R .LE. 1) RETURN
      T = NDEX(1)
      NDEX(1) = NDEX(R)
      NDEX(R) = T
      R = R - 1
      CALL HSIFTD (N, A, NDEX, L, R)
      GO TO 30
      END
      SUBROUTINE HSIFTD (N, A, NDEX, L, R)
*     used by hsortd
      INTEGER N
      REAL   A(N)
      INTEGER NDEX(N), L, R
      INTEGER I, J, X
      I = L
      J = I + I
      X = NDEX(I)
10    IF (J .GT. R) GO TO 20
      IF (J .GE. R) GO TO 15
      IF (A(NDEX(J)) .LT. A(NDEX(J+1))) J = J + 1
15    IF (A(X) .GE. A(NDEX(J))) GO TO 20
      NDEX(I) = NDEX(J)
      I = J
      J = I + I
      GO TO 10
20    NDEX(I) = X
      RETURN
      END

      SUBROUTINE PLANER (X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3, A,B,C,D)
      IMPLICIT REAL   (A-Z)
*     solve for coefficients of plane eqn z=ax+by+c
*     and yield d=0 in case of degenerate ("edge-on") triangle
      D1 = Z1*(Y2-Y3)       - Y1*(Z2-Z3)       + Z2*Y3-Y2*Z3
      D2 = X1*(Z2-Z3)       - Z1*(X2-X3)       + X2*Z3-Z2*X3
      D3 = X1*(Y2*Z3-Z2*Y3) - Y1*(X2*Z3-Z2*X3) + Z1*(X2*Y3-Y2*X3)
      D  = X1*(Y2-Y3)       - Y1*(X2-X3)       + X2*Y3-Y2*X3
      A = 0.
      B = 0.
      C = 0.
      IF (D.NE.0.) THEN
        A = D1/D
        B = D2/D
        C = D3/D
      ENDIF
      IF (ABS(A)+ABS(B)+ABS(C).GT.1E10) THEN
      	D = 0.0
      ENDIF
      RETURN
      END

      SUBROUTINE ASSERT (LOGIC, DAMMIT)
      LOGICAL LOGIC
      CHARACTER*(*) DAMMIT
      INTEGER ASSOUT
      LOGICAL VERBOSE
      COMMON /ASSCOM/ ASSOUT, VERBOSE
      SAVE /ASSCOM/
      IF (LOGIC) RETURN
      WRITE (ASSOUT,*) 'ERROR >>>>>> ',DAMMIT
      CALL EXIT(-1)
      END

C
C Find Z coord of point on surface of cylinder with known X and Y coords
C cylinder axis is X2 - X1, cylinder radius is R
C Need to find Z coord ZB.
C flag is 0 if cylinder had rounded ends, FLAT if it has flat ends,
C Also find nearest point XYZA on cylinder axis and fraction along it.
C
	FUNCTION CYLTEST  ( flag, axfrac,
     &			 x1,y1,z1,  x2,y2,z2,  xb,yb,zb,  R,  xa,ya,za )
	LOGICAL  CYLTEST
c	implicit NONE
*
*     Bit definitions for FLAG array
      INTEGER    FLAT,      RIBBON,    SURFACE,   PROPS
      PARAMETER (FLAT=2,    RIBBON=4,  SURFACE=8, PROPS=16)
      INTEGER    TRANSP,    HIDDEN,    INSIDE,    MOPT1
      PARAMETER (TRANSP=32, HIDDEN=64, INSIDE=128,MOPT1=256)
      INTEGER	 VCOLS,     CLIPPED,      VTRANS,      BOUNDED
      PARAMETER	(VCOLS=512, CLIPPED=1024, VTRANS=2048, BOUNDED=4096)
      INTEGER	 TFI
      PARAMETER (TFI = TRANSP + INSIDE)
c
	INTEGER flag
	REAL  	x1,y1,z1, x2,y2,z2, xb,yb,zb
	REAL  	R
	REAL  	xa,ya,za
c
	REAL  	ca,cb,cg,dx,dy,dz,d2
	REAL  	A0,A1,A2,Q
	REAL  	p1,r2,dx2,dy2
	REAL  	dd1,dd2
c
c	start with direction cosines * d2
	ca = x2 - x1
	cb = y2 - y1
	cg = z2 - z1
c
c	other useful quantities
c	(note: if d2==0 must be degenerate cylinder, really a disk)
	r2 = R*R
	d2 = ca*ca + cb*cb + cg*cg
	dx = xb - x1
	dy = yb - y1
	dx2 = dx**2
	dy2 = dy**2
c
c	use these to find coefficients of quadratic equation for ZB
c	EAM Jan 1997 test and handle dx-dy=0
	if (ca.eq.0. .and. cb.eq.0.) then
	    if (z2.gt.z1) p1 =  1.0
	    if (z2.lt.z1) p1 = -1.0
	    goto 100
	end if
c
	A0 = (dx*cb - dy*ca)**2 + (dy2 + dx2)*cg*cg - r2*d2
	A1 = -2.0 * (dy*cg*cb + dx*ca*cg)
	A2 = ca*ca + cb*cb
	Q  = A1*A1 - 4.0*A0*A2
	if (Q .lt. 0) then
C		zb = -99999.
		cyltest = .false.
		return
	else
		if (  and(flag,TFI) .eq. TFI) then
			dz = (-sqrt(Q) - A1) / (2.0 * A2)
		else
			dz = ( sqrt(Q) - A1) / (2.0 * A2)
		endif
		zb = z1 + dz
	end if
c
c	now find nearest point on cylinder axis
c	p1 is fraction along axis from x1 to x2
c	0 < p1 < 1 means point is on wall of cylinder
c
	dd1 = dx2 + dy2 + dz*dz
	dd2 = (x2-xb)**2 + (y2-yb)**2 + (z2-zb)**2
c
	p1 = (dd1 - r2) / d2
	if (p1 .le. 0.0) then
     		p1 = 0.0
	else
		p1 = sqrt(p1)
	end if
c
	if ((dd2 .gt. (d2+r2)) .and. (dd2 .gt. dd1)) p1 = -p1
c
	if (p1 .ge. 0 .and. p1 .le. 1.0) then
		xa = p1*ca + x1
		ya = p1*cb + y1
		za = p1*cg + z1
		cyltest = .true.
		return
	end if
c
c	point is either on end cap, or missed entirely
c
  100	continue
	if (p1 .ge. 1.0) then
		xa = x2
		ya = y2
		za = z2
		dx = xb - x2
		dy = yb - y2
		dx2 = dx**2
		dy2 = dy**2
	else if (p1 .le. 0.0) then
		xa = x1
		ya = y1
		za = z1
	end if
c
c Rounded cylinder end
	if (AND(flag,FLAT) .eq. 0) then
		if (dx2+dy2 .gt. r2) then
			cyltest = .false.
			return
		else
		    if (  and(flag,TFI) .eq. TFI) then
			zb = za - sqrt(r2 - (dx2+dy2))
		    else
			zb = za + sqrt(r2 - (dx2+dy2))
		    end if
		end if
C
C Flat cylinder end
C
	else
		if (cg .eq. 0.) then
C		    zb = -99999.
		    cyltest = .false.
		    return
		endif
		zb = (cg*za - ca*dx - cb*dy) / cg
		if (dx2 + dy2 + (zb-za)**2 .ge. r2) then
C		    zb = -99999.
		    cyltest = .false.
		    return
		endif
		if (p1 .ge. 1.0) then
		    xa = xb - (x2 - x1)
		    ya = yb - (y2 - y1)
		    za = zb - (z2 - z1)
		else if (p1 .le. 0.0) then
		    xa = xb - (x1 - x2)
		    ya = yb - (y1 - y2)
		    za = zb - (z1 - z2)
		endif
	end if
c
	if (p1.gt.1.0) p1 = 1.0
	if (p1.lt.0.0) p1 = 0.0
	axfrac = p1
c
	cyltest = .true.
	return
	end

C Bookkeeping for transparency
C 5-Mar-2001
C New version that is not limited to 3-deep transparent objects.
C On exit:
C	INDTOP, ZTOP   contain the top object so far, and its height
C	ZHIGH          height of top opaque object
C
	subroutine rank( ind, zp, normal, flag )
*
	implicit NONE
	integer  ind
	real     zp, normal(3)
	integer*4  flag(1)
*
	integer  i,j,k
*
      INTEGER ASSOUT
      LOGICAL VERBOSE
      COMMON /ASSCOM/ ASSOUT, VERBOSE
*
      INCLUDE 'parameters.incl'
*
*     Support for transparency
      COMMON /TRANS/ NTRANSP, INDEPTH, INDTOP, TRANOVFL, ZTOP, ZHIGH, 
     &               INDLIST(MAXTRANSP), ZLIST(MAXTRANSP), 
     &		     NORMLIST(3,MAXTRANSP)
      INTEGER        NTRANSP, INDEPTH, INDTOP, INDLIST, TRANOVFL
      REAL           ZTOP, ZHIGH, ZLIST, NORMLIST
*
*     Bit definitions for FLAG(MAXOBJ) array
      INTEGER    FLAT,      RIBBON,    SURFACE,   PROPS
      PARAMETER (FLAT=2,    RIBBON=4,  SURFACE=8, PROPS=16)
      INTEGER    TRANSP,    HIDDEN,    INSIDE,    MOPT1
      PARAMETER (TRANSP=32, HIDDEN=64, INSIDE=128,MOPT1=256)
      INTEGER	 VCOLS,     CLIPPED,      VTRANS,      BOUNDED
      PARAMETER	(VCOLS=512, CLIPPED=1024, VTRANS=2048, BOUNDED=4096)
*
*     The MOPT1 flag signals an alternative mode of transparency
	if (and(flag(ind),mopt1).ne.0) goto 400
*
	do i = 1, indepth
      	    if (zp.gt.zlist(i)) then
	    	if (and(flag(ind),transp).eq.0) then
		    indepth    = i
		    zhigh = zp
		    goto 345
		else
		    do j = indepth, i, -1
		    	indlist(j+1)   = indlist(j)
			zlist(j+1)     = zlist(j)
			normlist(1,j+1)= normlist(1,j)
			normlist(2,j+1)= normlist(2,j)
			normlist(3,j+1)= normlist(3,j)
		    enddo
		    indepth = indepth + 1
		    goto 344
		endif
	    else if (and(flag(indlist(i)),TRANSP).eq.0) then
	    	return
	    endif
	enddo
c     If the rest of the list is transparent, add this at the end
	indepth = indepth + 1
	i = indepth
c
  344	continue
	if (indepth.ge.MAXTRANSP) then
	    indepth  = MAXTRANSP - 1
	    tranovfl = tranovfl + 1
	    return
	endif
c
  345	continue
	indlist(i) = ind
	zlist(i)   = zp
	normlist(1,i) = normal(1)
    	normlist(2,i) = normal(2)
	normlist(3,i) = normal(3)
	if (and(flag(ind),transp).eq.0) zhigh = zp
	indtop = indlist(1)
  	ztop   = zlist(1)
	return
c
c     MOPT1 version. Same routine, except this time we have the extra
c     overhead of having to check for duplication of material.
  400	continue
	do i = 1, indepth
      	    if (zp.gt.zlist(i)) then
	        if (flag(ind)/65536 .eq. flag(indlist(i))/65536) then
		    goto 345
		endif
c		Handle case where two MOPT1 surfaces have intervening transp obj
c		In this case overwrite the lower MOPT1 surface
		do k = i, indepth-1
		   if (flag(ind)/65536 .eq. flag(indlist(k+1))/65536) 
     &		   	goto 401
		enddo
		if (indepth .ge. MAXTRANSP-1) then
	    	    tranovfl = tranovfl + 1
		else
		    k = indepth
		    indepth = indepth + 1
		endif
  401		continue
		do j = k, i, -1
		   indlist(j+1)   = indlist(j)
		   zlist(j+1)     = zlist(j)
		   normlist(1,j+1)= normlist(1,j)
		   normlist(2,j+1)= normlist(2,j)
		   normlist(3,j+1)= normlist(3,j)
		enddo
		goto 345
	    else if (and(flag(indlist(i)),TRANSP).eq.0) then
	    	return
	    else if (flag(ind)/65536 .eq. flag(indlist(i))/65536) then
	    	return
	    endif
	enddo
c	If the rest of the list is transparent, add this at the end
	indepth = indepth + 1
	i = indepth
	goto 344
c
	end
c

	SUBROUTINE CHKRGB( RED, GRN, BLU, MESSAGE )
	REAL RED, GRN, BLU
	CHARACTER*(*) MESSAGE
        CALL ASSERT (RED.GE.0., MESSAGE)
        CALL ASSERT (GRN.GE.0., MESSAGE)
        CALL ASSERT (BLU.GE.0., MESSAGE)
        CALL ASSERT (RED.LE.1., MESSAGE)
        CALL ASSERT (GRN.LE.1., MESSAGE)
        CALL ASSERT (BLU.LE.1., MESSAGE)
	RETURN
	END


	FUNCTION FOGGY( DEPTH )
	REAL     FOGGY, DEPTH
	COMMON /FOGCOM/ FOGTYPE,FOGFRONT,FOGBACK,FOGDEN,FOGLIM,FOGRGB
	INTEGER FOGTYPE
	REAL    FOGFRONT, FOGBACK, FOGDEN, FOGLIM(2), FOGRGB(3)
	REAL    FOGDIM
c
	IF (FOGTYPE .EQ. 0) 
     &	    FOGDIM = FOGDEN * DEPTH / (FOGLIM(2)-FOGLIM(1))
	IF (FOGTYPE .GT. 0)
     &	    FOGDIM = 1. - EXP(-FOGDEN * DEPTH/(FOGLIM(2)-FOGLIM(1)))
	FOGDIM = MAX( 0.0, FOGDIM )
	FOGDIM = MIN( 1.0, FOGDIM )
	FOGGY  = FOGDIM
	RETURN
	END

	FUNCTION DET( A )
	REAL     DET, A(4,4)
	DET = A(1,1)*A(2,2)*A(3,3) + A(1,2)*A(2,3)*A(3,1) 
     &	    + A(2,1)*A(3,2)*A(1,3) - A(1,1)*A(2,3)*A(3,2)
     &	    - A(3,3)*A(1,2)*A(2,1) - A(1,3)*A(2,2)*A(3,1)
	RETURN
	END


	subroutine liblookup( name, fullname )
c
	character*(*) name
	character*128 fullname
	character*132 R3DLIB
c
	call getenv('R3D_LIB',R3DLIB)
c
	fullname = ' '
	len = 0
	do i = 1, 132
	    if (R3DLIB(i:i).ne.' ') len = i
	enddo
	if (len.eq.0) then
	    fullname = name
	    return
	else
	    fullname(1:len) = R3DLIB(1:len)
	    fullname(len+1:len+1) = '/'
	    j = len+2
	endif
c
	len = 0
  100	continue
	len = len + 1
	if (name(len:len).ne.' ') goto 100
	len = len - 1
c
	fullname(j:j+len-1) = name(1:len)
	len = j+len-1
c
	return
	end

C EAM - 21 Feb 2001	Version 2.6
C Test for bounding planes
C Returns .FALSE. if the point does not have to be rendered
C         .TRUE.  if the point is to be rendered, in which case
C                 ZP, DX, DY, DZ have been updated if on bounding plane
C		  ZP = height of point being rendered
C		  DX,DY,DZ = surface normal at point (XP,YP,ZP)
C		  BPIND is the object number of the bounding plane 
C		        or 0 if the bounding planes don't trigger
C
	FUNCTION INBOUNDS( MAT, TYPE, LIST, DETAIL, 
     &	                   XP,YP,ZP, DX,DY,DZ, ZBACK, BPIND )
     	IMPLICIT NONE
	LOGICAL  INBOUNDS
	INTEGER             MAT, M, TYPE(1), LIST(1), BPIND
	REAL                DETAIL(1), XP,YP,ZP, DX,DY,DZ, ZBACK
c
c	Object type used for bounding planes (may change)
	INTEGER    INTERNAL
	PARAMETER (INTERNAL = 4)
c
	REAL BPLANE(3), BPNORM(3)
	REAL XN,YN,ZN, TEMP
	INTEGER MIND
	LOGICAL TESTBACK
c
	INBOUNDS = .FALSE.
	BPIND    = 0
	TESTBACK = (ZBACK.NE.ZP)
	M = MAT
	DO WHILE (TYPE(M).EQ.INTERNAL)
	  MIND = LIST(M)
     	  BPLANE(1) = DETAIL(MIND+3)
     	  BPLANE(2) = DETAIL(MIND+4)
     	  BPLANE(3) = DETAIL(MIND+5)
	  BPNORM(1) = DETAIL(MIND+6)
	  BPNORM(2) = DETAIL(MIND+7)
	  BPNORM(3) = DETAIL(MIND+8)
	  XN = XP - BPLANE(1)
	  YN = YP - BPLANE(2)
	  ZN = ZP - BPLANE(3)
	  TEMP = XN*BPNORM(1) + YN*BPNORM(2) + ZN*BPNORM(3)
	  IF (TEMP.GT.0) THEN
	    IF (TESTBACK) THEN
	      ZP = ZBACK
	      ZN = ZP - BPLANE(3)
	      TEMP = XN*BPNORM(1) + YN*BPNORM(2) + ZN*BPNORM(3)
	      IF (TEMP.GT.0) RETURN
	    ENDIF
	    DX = BPNORM(1)
	    DY = BPNORM(2)
	    DZ = BPNORM(3)
	    ZP = (XN*DX + YN*DY) / (-DZ) + BPLANE(3)
	    BPIND = M
	  ENDIF
	M = M + 1
	ENDDO
	INBOUNDS = .TRUE.
	RETURN
	END
C
C Equivalent test for bounding planes in ORTEP mode (only the octant clipped
C by all three bounding planes is removed). Only intended for ellipsoids.
C This tests the AND (rather than the OR) of multiple bounding planes.
C
	SUBROUTINE ORTEPBOUNDS( MAT, TYPE, LIST, DETAIL, 
     &	                        XP,YP,ZP, DX,DY,DZ, ZBACK, BPIND )
     	IMPLICIT NONE
	INTEGER             MAT, M, TYPE(1), LIST(1), BPIND
	REAL                DETAIL(1), XP,YP,ZP, DX,DY,DZ, ZBACK
c
c	Object type used for bounding planes (may change)
	INTEGER    INTERNAL
	PARAMETER (INTERNAL = 4)
c
	REAL BPLANE(3), BPNORM(3)
	REAL XN,YN,ZN, TEMP, ZMAX, DXMAX, DYMAX, DZMAX
	INTEGER MIND
c
	M     = MAT
	ZMAX  = -1.E10
	DXMAX = DX
	DYMAX = DY
	DZMAX = DZ
	DO WHILE (TYPE(M).EQ.INTERNAL)
	  MIND = LIST(M)
     	  BPLANE(1) = DETAIL(MIND+3)
     	  BPLANE(2) = DETAIL(MIND+4)
     	  BPLANE(3) = DETAIL(MIND+5)
	  BPNORM(1) = DETAIL(MIND+6)
	  BPNORM(2) = DETAIL(MIND+7)
	  BPNORM(3) = DETAIL(MIND+8)
	  XN = XP - BPLANE(1)
	  YN = YP - BPLANE(2)
	  ZN = ZP - BPLANE(3)
	  TEMP = XN*BPNORM(1) + YN*BPNORM(2) + ZN*BPNORM(3)
	  IF (TEMP.LT.0) THEN
	      BPIND = 0
	      RETURN
	  ENDIF
	  TEMP = (XN*BPNORM(1)+YN*BPNORM(2)) / (-BPNORM(3)) + BPLANE(3)
	  IF (TEMP.GT.ZMAX) THEN
	      ZMAX = TEMP
	      DXMAX = BPNORM(1)
	      DYMAX = BPNORM(2)
	      DZMAX = BPNORM(3)
	      BPIND = M
	  ENDIF
	  M = M + 1
	ENDDO
	ZP = ZMAX
	DX = DXMAX
	DY = DYMAX
	DZ = DZMAX
	CALL ASSERT(DZ.GT.0,'ORTEP bounds incorrectly initialized')
	RETURN
	END
