C graphics.f
C
C MolScript v1.4, copyright (C) 1993 Per Kraulis
C
C Graphical database handling procedures.
C
C Per Kraulis, Dept Molecular Biology, Uppsala University, Sweden.
C  17-Dec-1990  first attempts
C  20-Dec-1993  modifications for Raster3D, due to Ethan Merritt
C   4-Aug-1994  dotted lines for Raster3D, due to Ethan Merritt
C  19-Aug-1996  mods to Raster3D routine RASPLN, Ethan Merritt
C
C GRINIT  initialize graphics database
C GRINMP  init parameter and colour names and paths
C GRIGST  increment graphics state
C GRPPAR  prepare for parameter value modification
C GRMPAR  modify parameter value
C GRPCOL  prepare for colour specification
C GRSCOL  set colour specified
C GRLINE  create line segment
C GRLIND  create dashed line segment, for Raster3D
C GRSPHE  create sphere segment
C GRPLAN  create plane segment
C RASPLN  create plane segment, for Raster3D
C GRCHPL  change type of last created plane segment
C GRLABL  create label segment
C GRSTIK  create stick segment
C GRSEG   enter segment into graphics database
C GRPOST  output graphics database to PostScript file
C GRCOMP  comparison function for depth sort
C GRDCUE  compute depth cue from z coordinate
C GRRAST  output graphics database to Raster3D file
C GRSHAD  shade colour
C GRSWIN  set window to encompass segments
C GRSCLI  set clip slab to encompass segments
C
C
C----------------------
      SUBROUTINE GRINIT
C
      INCLUDE 'molscript.dim'
      INCLUDE 'molscript.inc'
      INCLUDE 'graphics.inc'
C
C Help variable
C
      INTEGER SLOT
C
C Default plot area, aspect ratio
C
      IF (RASTER) THEN
        AREA (1) = -0.5
        AREA (2) = -0.5
        AREA (3) =  0.5
        AREA (4) =  0.5
      ELSE
        AREA (1) =  50.0
        AREA (2) = 100.0
        AREA (3) = 550.0
        AREA (4) = 700.0
      END IF
      ASPECT = (AREA (4) - AREA (2)) / (AREA (3) - AREA (1))
C
C Default frame drawn
C
      FRAME = .TRUE.
C
C Default background colour white
C
      BCKCOL (1) = 1.0
      BCKCOL (2) = 1.0
      BCKCOL (3) = 1.0
C
C Graphical object parameters
C
      BONDDI = 1.9
      SEGMNT = 6
      SMSTEP = 2
      HERMFC = 1.0
      HLXWID = 2.4
      CILRAD = 0.2
      STWID  = 1.0
      STTHK  = 0.3
      HELTHK = 0.15
C
C Graphics state start values
C
      TOTGST = 1
      IF (RASTER) THEN
        LINEWD (1) = 1.0 / 25.0
      ELSE
        LINEWD (1) = 1.0
      END IF
      CALL V3INIT (LINCOL (1, 1), 0.0, 0.0, 0.0)
      LINDSH (1) = 0.0
      DCUEFC (1) = 0.75
      DCCOLR (1) = 0.0
      CALL V3INIT (PLPCOL (1, 1), 1.0, 1.0, 1.0)
      CALL V3INIT (PLSCOL (1, 1), 0.5, 0.5, 0.5)
      SHDEXP = 1.5
      SHADNG = 0.5
      CALL V3INIT (LABOFF, 0.0, 0.0, 0.0)
      LABSIZ (1) = 20.0
      LABCTR (1) = .TRUE.
      DO 100 SLOT = 1, 80
        LABMSK (SLOT, 1) = 0
100   CONTINUE
      LABCLP (1) = .FALSE.
      LABROT (1) = .FALSE.
      STKRAD (1) = 0.2
      STKTAP (1) = 0.75
C
C Parameter change indicators
C
      PARNUM = 0
      COLNUM = 0
C
C Init segments
C
      TOTSEG = 0
      TOTLIN = 0
      TOTSPH = 0
      TOTPLA = 0
      TOTLAB = 0
      TOTSTK = 0
C
C Undefined window and clip size
C
      WIND = -1.0
      CLIP = -1.0
C
C Previous plane was not clipped; it didn't exist
C
      PLACLP = .FALSE.
C
      RETURN
      END
C
C
C----------------------
      BLOCK DATA GRINMP
C
      INCLUDE 'molscript.dim'
      INCLUDE 'graphics.inc'
C
      DATA PARID /'atomcolour', 'atomradius', 'bonddistance',
     $ 'coilradius', 'colourdepthcue', 'depthcue', 'helixthickness',
     $ 'helixwidth', 'labelcentre', 'labelclip', 'labelmask',
     $ 'labeloffset', 'labelrotation', 'labelsize', 'linecolour',
     $ 'linedash', 'linewidth', 'plane2colour', 'planecolour',
     $ 'segments','shading', 'shadingexponent', 'smoothsteps',
     $ 'splinefactor', 'stickradius', 'sticktaper', 'strandthickness',
     $ 'strandwidth'/
      DATA PARPTH /'%s', '%a', '%r', '%r', '%r', '%r', '%r', '%r', '%b',
     $ '%b', '%l', '%v', '%b', '%r', '%c', '%r', '%r', '%c', '%c', '%i',
     $ '%r', '%r', '%i', '%r', '%r', '%r', '%r', '%r'/
C
      DATA COLID /'black', 'blue', 'cyan', 'gray', 'green', 'grey',
     $  'hsb', 'purple', 'red', 'rgb', 'white', 'yellow'/
      DATA COLPTH /'%n', '%n', '%n', '%r', '%n', '%r', '%v', '%n',
     $  '%n', '%v', '%n', '%n'/
C
      END
C
C
C----------------------
      SUBROUTINE GRIGST
C
      INCLUDE 'molscript.dim'
      INCLUDE 'graphics.inc'
C
C Help variable
C
      INTEGER SLOT
C
      IF (TOTGST .GE. MAXGST)
     $  CALL MABORT ('no space for modified parameters; MAXGST')
C
C Increment and copy over previous values
C
      TOTGST = TOTGST + 1
C
      LINEWD (TOTGST) = LINEWD (TOTGST - 1)
      CALL V3COPY (LINCOL (1, TOTGST), LINCOL (1, TOTGST - 1))
      LINDSH (TOTGST) = LINDSH (TOTGST - 1)
      DCUEFC (TOTGST) = DCUEFC (TOTGST - 1)
      DCCOLR (TOTGST) = DCCOLR (TOTGST - 1)
      CALL V3COPY (PLPCOL (1, TOTGST), PLPCOL (1, TOTGST - 1))
      CALL V3COPY (PLSCOL (1, TOTGST), PLSCOL (1, TOTGST - 1))
      LABSIZ (TOTGST) = LABSIZ (TOTGST - 1)
      LABCTR (TOTGST) = LABCTR (TOTGST - 1)
      DO 100 SLOT = 1, 80
        LABMSK (SLOT, TOTGST) = LABMSK (SLOT, TOTGST - 1)
100   CONTINUE
      LABCLP (TOTGST) = LABCLP (TOTGST - 1)
      LABROT (TOTGST) = LABROT (TOTGST - 1)
      STKRAD (TOTGST) = STKRAD (TOTGST - 1)
      STKTAP (TOTGST) = STKTAP (TOTGST - 1)
C
      RETURN
      END
C
C
C----------------------------
      SUBROUTINE GRPPAR (PAR)
C
      INCLUDE 'molscript.dim'
      INCLUDE 'graphics.inc'
C
      CHARACTER*(*) PAR
C
C PAR  (In) parameter to init modification of
C
C Externally defined functions
C
      INTEGER BFINDS
C
C Help variables
C
      INTEGER ERRCOD
C
C Find parameter
C
      PARNUM = BFINDS (PAR, PARID, TOTPAR)
      IF (PARNUM .LT. 1) CALL MABORT ('no such parameter')
C
C Set syntax path for parameter value
C
      CALL SXPARS (PARPTH (PARNUM), 0, ERRCOD)
      IF (ERRCOD .NE. 0)
     $  CALL MABORT ('internal: parsing parameter syntax path')
C
      RETURN
      END
C
C
C----------------------
      SUBROUTINE GRMPAR
C
      INCLUDE 'molscript.dim'
      INCLUDE 'molscript.inc'
      INCLUDE 'graphics.inc'
C
C Externally defined function
C
      INTEGER ACOUNT
C
C Help variables
C
      INTEGER AT, SLOT
C
      IF (PARID (PARNUM) .EQ. 'bonddistance') THEN
        IF (RVALUE .LE. 0.0) GOTO 900
        BONDDI = RVALUE
C
      ELSE IF (PARID (PARNUM) .EQ. 'linewidth') THEN
        IF (RVALUE .LT. 0.0) GOTO 900
        IF (RASTER) THEN
          LINEWD (TOTGST) = RVALUE / 25.0
        ELSE
          LINEWD (TOTGST) = RVALUE
        END IF
C
      ELSE IF (PARID (PARNUM) .EQ. 'linecolour') THEN
        CALL GRSCOL
        CALL V3COPY (LINCOL (1, TOTGST), VECTOR)
C
      ELSE IF (PARID (PARNUM) .EQ. 'linedash') THEN
        IF (RVALUE .LT. 0.0) GOTO 900
        LINDSH (TOTGST) = RVALUE
C
      ELSE IF (PARID (PARNUM) .EQ. 'stickradius') THEN
        IF (RVALUE .LE. 0.0) GOTO 900
        STKRAD (TOTGST) = RVALUE
C
      ELSE IF (PARID (PARNUM) .EQ. 'sticktaper') THEN
        IF (RVALUE .LT. 0.0) GOTO 900
        IF (RVALUE .GT. 1.0) GOTO 900
        STKTAP (TOTGST) = RVALUE
C
      ELSE IF (PARID (PARNUM) .EQ. 'depthcue') THEN
        IF (RVALUE .LT. 0.0) GOTO 900
        IF (RVALUE .GT. 1.0) GOTO 900
        DCUEFC (TOTGST) = RVALUE
C
      ELSE IF (PARID (PARNUM) .EQ. 'colourdepthcue') THEN
        IF (RVALUE .LT. -1.0) GOTO 900
        IF (RVALUE .GT. 1.0) GOTO 900
        DCCOLR (TOTGST) = RVALUE
C
      ELSE IF (PARID (PARNUM) .EQ. 'atomradius') THEN
        IF (RVALUE .LE. 0.0) GOTO 900
        CALL MSGINT (ACOUNT (1))
        CALL MSGSTR ('atoms selected for atomradius')
        CALL MSGOUT
        DO 100 AT = 1, TOTATM
          IF (ATFLAG (AT, 1)) ATRAD (AT) = RVALUE
100     CONTINUE
        TOTASF = 0
C
      ELSE IF (PARID (PARNUM) .EQ. 'atomcolour') THEN
        CALL GRSCOL
        CALL MSGINT (ACOUNT (1))
        CALL MSGSTR ('atoms selected for atomcolour')
        CALL MSGOUT
        DO 200 AT = 1, TOTATM
          IF (ATFLAG (AT, 1)) CALL V3COPY (ATCOL (1, AT), VECTOR)
200     CONTINUE
        TOTASF = 0
C
      ELSE IF (PARID (PARNUM) .EQ. 'planecolour') THEN
        CALL GRSCOL
        CALL V3COPY (PLPCOL (1, TOTGST), VECTOR)
C
      ELSE IF (PARID (PARNUM) .EQ. 'plane2colour') THEN
        CALL GRSCOL
        CALL V3COPY (PLSCOL (1, TOTGST), VECTOR)
C
      ELSE IF (PARID (PARNUM) .EQ. 'segments') THEN
        IF (IVALUE .LT. 1) GOTO 900
        SEGMNT = IVALUE
C
      ELSE IF (PARID (PARNUM) .EQ. 'helixwidth') THEN
        IF (RVALUE .LT. 0.1) GOTO 900
        HLXWID = RVALUE
C
      ELSE IF (PARID (PARNUM) .EQ. 'coilradius') THEN
        IF (RVALUE .LT. 0.01) GOTO 900
        CILRAD = RVALUE
C
      ELSE IF (PARID (PARNUM) .EQ. 'strandwidth') THEN
        IF (RVALUE .LT. 0.02) GOTO 900
        STWID = RVALUE / 2.0
C
      ELSE IF (PARID (PARNUM) .EQ. 'strandthickness') THEN
        IF (RVALUE .LT. 0.01) GOTO 900
        STTHK = RVALUE / 2.0
C
      ELSE IF (PARID (PARNUM) .EQ. 'helixthickness') THEN
C       IF (RVALUE .LT. 0.0) GOTO 900
        HELTHK = RVALUE / 2.0
C
      ELSE IF (PARID (PARNUM) .EQ. 'smoothsteps') THEN
        IF (IVALUE .LT. 1) GOTO 900
        SMSTEP = IVALUE
C
      ELSE IF (PARID (PARNUM) .EQ. 'splinefactor') THEN
        IF (RVALUE .LE. 0.01) GOTO 900
        HERMFC = RVALUE
C
      ELSE IF (PARID (PARNUM) .EQ. 'shadingexponent') THEN
        IF (RVALUE .LT. 0.0) GOTO 900
        SHDEXP = RVALUE
C
      ELSE IF (PARID (PARNUM) .EQ. 'shading') THEN
        IF (RVALUE .LT. 0.0) GOTO 900
        IF (RVALUE .GT. 1.0) GOTO 900
        SHADNG = RVALUE
C
      ELSE IF (PARID (PARNUM) .EQ. 'labeloffset') THEN
        CALL V3COPY (LABOFF, VECTOR)
C
      ELSE IF (PARID (PARNUM) .EQ. 'labelsize') THEN
        IF (RVALUE .LT. 1.0) GOTO 900
        LABSIZ (TOTGST) = RVALUE
C
      ELSE IF (PARID (PARNUM) .EQ. 'labelcentre') THEN
        LABCTR (TOTGST) = LVALUE
C
      ELSE IF (PARID (PARNUM) .EQ. 'labelmask') THEN
        DO 300 SLOT = 1, IVALUE
          IF (NAME80 (SLOT : SLOT) .EQ. ' ') THEN
            LABMSK (SLOT, TOTGST) = 3 * (LABMSK (SLOT, TOTGST) / 3)
          ELSE IF (NAME80 (SLOT : SLOT) .EQ. 'l') THEN
            LABMSK (SLOT, TOTGST) = 3 * (LABMSK (SLOT, TOTGST) / 3) + 1
          ELSE IF (NAME80 (SLOT : SLOT) .EQ. 'u') THEN
            LABMSK (SLOT, TOTGST) = 3 * (LABMSK (SLOT, TOTGST) / 3) + 2
          ELSE IF (NAME80 (SLOT : SLOT) .EQ. 'g') THEN
            LABMSK (SLOT, TOTGST) = MOD (LABMSK (SLOT, TOTGST), 3) + 3
          ELSE IF (NAME80 (SLOT : SLOT) .EQ. 'r') THEN
            LABMSK (SLOT, TOTGST) = MOD (LABMSK (SLOT, TOTGST), 3)
          ELSE
            CALL MABORT ('invalid labelmask')
          END IF
300     CONTINUE
C
      ELSE IF (PARID (PARNUM) .EQ. 'labelclip') THEN
        LABCLP (TOTGST) = LVALUE
C
      ELSE IF (PARID (PARNUM) .EQ. 'labelrotation') THEN
        LABROT (TOTGST) = LVALUE
C
      END IF
C
      RETURN
C
900   CALL MABORT ('invalid value for parameter')
      END
C
C
C----------------------------
      SUBROUTINE GRPCOL (COL)
C
      INCLUDE 'molscript.dim'
      INCLUDE 'molscript.inc'
      INCLUDE 'graphics.inc'
C
      CHARACTER*(*) COL
C
C COL  (In) colour specification
C
C Externally defined functions
C
      INTEGER BFINDS
C
C Help variables
C
      INTEGER ERRCOD
C
C Find parameter
C
      COLNUM = BFINDS (COL, COLID, TOTCOL)
      IF (COLNUM .LT. 1) CALL MABORT ('no such colour')
C
C Set syntax path for colour specification
C
      CALL SXPARS (COLPTH (COLNUM), 0, ERRCOD)
      IF (ERRCOD .NE. 0)
     $  CALL MABORT ('internal: parsing colour syntax path')
C
      RETURN
      END
C
C
C----------------------
      SUBROUTINE GRSCOL
C
      INCLUDE 'molscript.dim'
      INCLUDE 'molscript.inc'
      INCLUDE 'graphics.inc'
C
C Help variable
C
      INTEGER SLOT
C
C Skip if no colour specification
C
      IF (COLNUM .EQ. 0) RETURN
C
      IF (COLID (COLNUM) .EQ. 'gray'  .OR.
     $    COLID (COLNUM) .EQ. 'grey') THEN
        IF (RVALUE .LT. 0.0) GOTO 900
        IF (RVALUE .GT. 1.0) GOTO 900
        CALL V3INIT (VECTOR, RVALUE, RVALUE, RVALUE)
C
      ELSE IF (COLID (COLNUM) .EQ. 'rgb') THEN
        DO 100 SLOT = 1, 3
          IF (VECTOR (SLOT) .LT. 0.0) GOTO 900
          IF (VECTOR (SLOT) .GT. 1.0) GOTO 900
100     CONTINUE
C
C Add on offset to first component to indicate HSB specification
C
      ELSE IF (COLID (COLNUM) .EQ. 'hsb') THEN
        DO 200 SLOT = 1, 3
          IF (VECTOR (SLOT) .LT. 0.0) GOTO 900
          IF (VECTOR (SLOT) .GT. 1.0) GOTO 900
200     CONTINUE
        IF (RASTER) THEN
          CALL MSGSTR ('converting HSB colour to RGB for Raster3D')
          CALL MSGOUT
          VECTOR (1) = 360.0 * VECTOR (1)
          CALL HSVRGB (VECTOR, VECTOR)
        ELSE
          VECTOR (1) = VECTOR (1) + 10.0
        END IF
C
      ELSE IF (COLID (COLNUM) .EQ. 'black') THEN
        CALL V3INIT (VECTOR, 0.0, 0.0, 0.0)
C
      ELSE IF (COLID (COLNUM) .EQ. 'white') THEN
        CALL V3INIT (VECTOR, 1.0, 1.0, 1.0)
C
      ELSE IF (COLID (COLNUM) .EQ. 'red') THEN
        CALL V3INIT (VECTOR, 1.0, 0.0, 0.0)
C
      ELSE IF (COLID (COLNUM) .EQ. 'green') THEN
        CALL V3INIT (VECTOR, 0.0, 1.0, 0.0)
C
      ELSE IF (COLID (COLNUM) .EQ. 'blue') THEN
        CALL V3INIT (VECTOR, 0.0, 0.0, 1.0)
C
      ELSE IF (COLID (COLNUM) .EQ. 'yellow') THEN
        CALL V3INIT (VECTOR, 1.0, 1.0, 0.0)
C
      ELSE IF (COLID (COLNUM) .EQ. 'cyan') THEN
        CALL V3INIT (VECTOR, 0.0, 1.0, 1.0)
C
      ELSE IF (COLID (COLNUM) .EQ. 'purple') THEN
        CALL V3INIT (VECTOR, 1.0, 0.0, 1.0)
      END IF
C
      COLNUM = 0
      RETURN
C
900   CALL MABORT ('invalid value for colour specification')
      END
C
C
C-----------------------------------
      SUBROUTINE GRLINE (COO1, COO2)
C
      INCLUDE 'molscript.dim'
      INCLUDE 'graphics.inc'
C
      REAL COO1 (3), COO2 (3)
C
C COO1  (In) line endpoint coordinates
C COO2  (In)
C
C Ethan A Merritt Aug 1994
C	for Raster3D output draw dashed lines as string of spheres
      IF (RASTER .AND. (LINDSH(TOTGST).NE.0)) THEN
	CALL GRLIND( COO1, COO2 )
	RETURN
      END IF
C
C Ignore if entirely outside of view
C
      IF (WIND .GT. 0.0  .AND.  .NOT. RASTER) THEN
        IF (COO1 (1) .LT. - ASWIND (1)  .AND.
     $      COO2 (1) .LT. - ASWIND (1)) RETURN
        IF (COO1 (2) .LT. - ASWIND (2)  .AND.
     $      COO2 (2) .LT. - ASWIND (2)) RETURN
        IF (COO1 (1) .GT. ASWIND (1)  .AND.
     $      COO2 (1) .GT. ASWIND (1)) RETURN
        IF (COO1 (2) .GT. ASWIND (2)  .AND.
     $      COO2 (2) .GT. ASWIND (2)) RETURN
      END IF
C
      IF (CLIP .GT. 0.0) THEN
        IF (COO1 (3) .LT. -CLIP  .AND.
     $      COO2 (3) .LT. -CLIP) RETURN
        IF (COO1 (3) .GT.  CLIP  .AND.
     $      COO2 (3) .GT.  CLIP) RETURN
      END IF
C
C Enter into graphics database
C
      IF (TOTLIN .GE. MAXLIN) CALL MABORT ('no space for line; MAXLIN')
C
      TOTLIN = TOTLIN + 1
      CALL V3COPY (LINCOO (1, 1, TOTLIN), COO1)
      CALL V3COPY (LINCOO (1, 2, TOTLIN), COO2)
C
      CALL GRSEG (LINE, TOTLIN, (COO1 (3) + COO2 (3)) / 2.0)
C
      RETURN
      END
C
C Ethan A Merritt Aug 1994
C only used for Raster3D - draw dotted line as chain of spheres
C interpret "linedash" parameter as spacing between spheres
C-----------------------------------
      SUBROUTINE GRLIND (COO1, COO2)
C
      INCLUDE 'molscript.dim'
      INCLUDE 'graphics.inc'
C
      REAL COO1 (3), COO2 (3)
C
C COO1  (In) line endpoint coordinates
C COO2  (In)
C
      REAL    V3DIFF
      REAL    RAD, COO(3), CINC(3)
      INTEGER I, NDOTS
C
      RAD = LINEWD(TOTGST)
      NDOTS = V3DIFF( COO1, COO2 ) / (RAD * (LINDSH(TOTGST)+1))
      IF (NDOTS.LE.1.0) NDOTS = 1
      CALL V3SUBT( CINC, COO2, COO1 )
      CALL V3SCAL( CINC, 1./FLOAT(NDOTS), CINC )
C
      CALL V3COPY( COO, COO1 )
      DO I = 1, NDOTS-1
	CALL V3ADD( COO, COO, CINC )
	CALL GRSPHE( COO, RAD, LINCOL(1,TOTGST) )
      END DO
C
      RETURN
      END
C
C
C--------------------------------------
      SUBROUTINE GRSPHE (COO, RAD, COL)
C
      INCLUDE 'molscript.dim'
      INCLUDE 'graphics.inc'
C
      REAL COO (3), RAD, COL (3)
C
C COO  (In)  coordinate
C RAD  (In)  radius
C COL  (In)  colour specification
C
C Ignore if entirely outside of view
C
      IF (WIND .GT. 0.0  .AND.  .NOT. RASTER) THEN
        IF (COO (1) + RAD  .LT. - ASWIND (1)) RETURN
        IF (COO (1) - RAD  .GT. ASWIND (1)) RETURN
        IF (COO (2) + RAD  .LT. - ASWIND (2)) RETURN
        IF (COO (2) - RAD  .GT. ASWIND (2)) RETURN
      END IF
C
      IF (CLIP .GT. 0.0) THEN
        IF (COO (3) + RAD  .LT. -CLIP) RETURN
        IF (COO (3) - RAD  .GT.  CLIP) RETURN
      END IF
C
C Enter into graphics database
C
      IF (TOTSPH .GE. MAXSPH) CALL MABORT('no space for sphere; MAXSPH')
C
      TOTSPH = TOTSPH + 1
      CALL V3COPY (SPHCOO (1, TOTSPH), COO)
      SPHRAD (TOTSPH) = RAD
      CALL V3COPY (SPHCOL (1, TOTSPH), COL)
C
C Depth sort: centre plus half radius
C
      CALL GRSEG (SPHERE, TOTSPH, COO (3) + 0.5 * RAD)
C
      RETURN
      END
C
C
C------------------------------------------------------
      SUBROUTINE GRPLAN (COO, JOIN, PLTYPE, PRIM, ZDIR)
C
      INCLUDE 'molscript.dim'
      INCLUDE 'graphics.inc'
C
      REAL    COO (3, 4), ZDIR
      LOGICAL JOIN, PRIM
      INTEGER PLTYPE
C
C COO     (In) coordinates
C JOIN    (In) join plane with previous
C PLTYPE  (In) type of plane; which bounding lines to draw
C PRIM    (In) primary surface, otherwise secondary
C ZDIR    (In) z component of plane direction vector, for shading
C
C Externally defined functions
C
      REAL V3DIFF, V3DOT
C
C Help variables
C
      REAL VEC1 (3), VEC2 (3), PREV2 (3), PREV3 (3)
      REAL ZCOO
      SAVE PREV2, PREV3
C
C Ignore if entirely outside of view
C
      IF (WIND .GT. 0.0  .AND.  .NOT. RASTER) THEN
        IF (COO (1, 1) .LT. - ASWIND (1)  .AND.
     $      COO (1, 2) .LT. - ASWIND (1)  .AND.
     $      COO (1, 3) .LT. - ASWIND (1)  .AND.
     $      COO (1, 4) .LT. - ASWIND (1)) GOTO 900
        IF (COO (2, 1) .LT. - ASWIND (2)  .AND.
     $      COO (2, 2) .LT. - ASWIND (2)  .AND.
     $      COO (2, 3) .LT. - ASWIND (2)  .AND.
     $      COO (2, 4) .LT. - ASWIND (2)) GOTO 900
        IF (COO (1, 1) .GT. ASWIND (1)  .AND.
     $      COO (1, 2) .GT. ASWIND (1)  .AND.
     $      COO (1, 3) .GT. ASWIND (1)  .AND.
     $      COO (1, 4) .GT. ASWIND (1)) GOTO 900
        IF (COO (2, 1) .GT. ASWIND (2)  .AND.
     $      COO (2, 2) .GT. ASWIND (2)  .AND.
     $      COO (2, 3) .GT. ASWIND (2)  .AND.
     $      COO (2, 4) .GT. ASWIND (2)) GOTO 900
      END IF
C
      IF (CLIP .GT. 0.0) THEN
        IF (COO (3, 1) .LT. -CLIP  .AND.
     $      COO (3, 2) .LT. -CLIP  .AND.
     $      COO (3, 3) .LT. -CLIP  .AND.
     $      COO (3, 4) .LT. -CLIP) GOTO 900
        IF (COO (3, 1) .GT.  CLIP  .AND.
     $      COO (3, 2) .GT.  CLIP  .AND.
     $      COO (3, 3) .GT.  CLIP  .AND.
     $      COO (3, 4) .GT.  CLIP) GOTO 900
      END IF
C
C Join: use only one slot
C
      IF (JOIN) THEN
C
C Previous plane was clipped; put in its end points before joining
C
        IF (PLACLP) THEN
          IF (TOTPLA .GE. MAXPLA)
     $      CALL MABORT ('no space for plane; MAXPLA')
          TOTPLA = TOTPLA + 1
          CALL V3COPY (PLACOO (1, 1, TOTPLA), PREV2)
          CALL V3COPY (PLACOO (1, 2, TOTPLA), PREV3)
        END IF
C
        IF (TOTPLA .GE. MAXPLA)
     $    CALL MABORT ('no space for plane; MAXPLA')
        TOTPLA = TOTPLA + 1
C
C Modify coordinate for previous plane; join end-points that are closest
C
        IF (V3DIFF (COO (1, 1), PLACOO (1, 1, TOTPLA - 1)) .LT.
     $      V3DIFF (COO (1, 4), PLACOO (1, 1, TOTPLA - 1))) THEN
          CALL V3ADD  (VEC1, COO (1, 1), PLACOO (1, 1, TOTPLA - 1))
          CALL V3SCAL (PLACOO (1, 1, TOTPLA - 1), 0.5, VEC1)
          CALL V3ADD  (VEC1, COO (1, 4), PLACOO (1, 2, TOTPLA - 1))
          CALL V3SCAL (PLACOO (1, 2, TOTPLA - 1), 0.5, VEC1)
        ELSE
          CALL V3ADD  (VEC1, COO (1, 4), PLACOO (1, 1, TOTPLA - 1))
          CALL V3SCAL (PLACOO (1, 1, TOTPLA - 1), 0.5, VEC1)
          CALL V3ADD  (VEC1, COO (1, 1), PLACOO (1, 2, TOTPLA - 1))
          CALL V3SCAL (PLACOO (1, 2, TOTPLA - 1), 0.5, VEC1)
        END IF
C
C Plane should not be twisted; swap coordinates if necessary
C
        CALL V3SUBT (VEC1, PLACOO (1, 2, TOTPLA - 1),
     $                     PLACOO (1, 1, TOTPLA - 1))
        CALL V3SUBT (VEC2, COO (1, 3), COO (1, 2))
C
        IF (V3DOT (VEC1, VEC2) .GT. 0.0) THEN
          CALL V3COPY (PLACOO (1, 1, TOTPLA), COO (1, 2))
          CALL V3COPY (PLACOO (1, 2, TOTPLA), COO (1, 3))
        ELSE
          CALL V3COPY (PLACOO (1, 1, TOTPLA), COO (1, 3))
          CALL V3COPY (PLACOO (1, 2, TOTPLA), COO (1, 2))
        END IF
C
C No join; use two slots
C
      ELSE
        IF (TOTPLA + 1  .GE. MAXPLA)
     $    CALL MABORT ('no space for plane; MAXPLA')
        TOTPLA = TOTPLA + 2
C
        CALL V3COPY (PLACOO (1, 1, TOTPLA - 1), COO (1, 1))
        CALL V3COPY (PLACOO (1, 1, TOTPLA), COO (1, 2))
        CALL V3COPY (PLACOO (1, 2, TOTPLA), COO (1, 3))
        CALL V3COPY (PLACOO (1, 2, TOTPLA - 1), COO (1, 4))
      END IF
C
C Plane shading from normal z value, shading coefficient and exponent
C
      PLASHD (TOTPLA) = SHADNG * ABS (ZDIR) **SHDEXP + 1.0 - SHADNG
C
C Z coordinate for depth sort
C
      ZCOO = MAX (PLACOO (3, 1, TOTPLA - 1), PLACOO (3, 2, TOTPLA - 1),
     $            PLACOO (3, 1, TOTPLA), PLACOO (3, 2, TOTPLA))
C
C Enter plane, with type, primary or secondary
C
      IF (PRIM) THEN
        CALL GRSEG (PLTYPE, TOTPLA, ZCOO)
      ELSE
        CALL GRSEG (- PLTYPE, TOTPLA, ZCOO)
      END IF
C
C This plane not clipped; reset flag
C
      PLACLP = .FALSE.
C
      RETURN
C
C Plane clipped; save end coords, for later joining
C
900   PLACLP = .TRUE.
      CALL V3COPY (PREV2, COO (1, 2))
      CALL V3COPY (PREV3, COO (1, 3))
C
      RETURN
      END
C
C
C--------------------------------------------------
      SUBROUTINE RASPLN (PLANE, PNORM, ZDIR, NPASS)
C
      INCLUDE 'molscript.dim'
      INCLUDE 'molscript.inc'
      INCLUDE 'graphics.inc'
C
      REAL    PLANE (3, 4), PNORM (3)
      REAL    ZDIR
      INTEGER NPASS
C
C PLANE	 (In) the four corners of the plane
C PNORM	 (In) normal to plane surface
C ZDIR	 (In) eventually passed through to GRPLAN for simplicity
C NPASS	 (In) draw ribbon surface in 1st pass, edges in 2nd pass
C
C Ethan A Merritt - Nov 1993
C RASPLN is a substitute for GRPLAN to be called in Raster3D mode.
C It will construct the pieces of a plane segment with rounded edges
C and thickness determined by helixthickness.  Properly the endpoints
C should be offset by the average of the normals of the two adjacent
C surfaces, but that would entail a lot more bookkeeping.  Instead I
C settle for slightly recessing the surfaces so that the difference
C in the surface junctions and the siderail junctions is not noticed.
C If helixthickness = 0.0 then simply produce a single plane segment.
C
C Ethan A Merritt - July 1996
C This is an updated version of RASPLN which fixes a
C bug/feature in optimizing alpha helix rendition via Raster3D.
C The previous version drew only the front surface of the helix
C (to save time and space during rendering), but this had two
C drawbacks:
C 1) If the SEGMENTS parameter was too small, sometimes there
C    were pieces of the surface missing at the edges where it
C    coiled around out of view
C 2) Once you created the Raster3D input file from Molscript
C    you couldn't further rotate the view angles, as you would
C    then be looking around the back-side of something only
C    rendered on the front side
C The current version has two fixes for these problems:
C -  The front and back surfaces are overlapped by one segment,
C    so rendering should be less sensitive to SEGMENTS. 
C    Also very small rotations (e.g. stereo pairs) should be OK.
C -  Setting HELIXTHICKNESS to a negative value will draw the
C    back side of the helix surfaces, so two passes though a
C    helix will allow you to view it later from any angle.
C    (NB: There will be one incorrectly shaded segment for each
C    coil of helix, but I hope that is not too noticeable).
C
C Help variables
C
      REAL SURF (3, 4), OFFSET (3), ONORM (3)
      SAVE ONORM
C 
C The following hard-wired parameter could be made into a user-setable
C parameter, e.g. "set helixrecess yy"
C
      REAL       RECESS
      PARAMETER (RECESS = 0.9)
C
C Simplest case is zero thickness plane, just pass through to GRPLAN
C
      IF (HELTHK .EQ. 0.0) THEN
        CALL GRPLAN (PLANE, .FALSE., PL1234, .TRUE., ZDIR)
        RETURN
      END IF
C
C Form sides using cylinders to connect the four corners
C
      IF (NPASS .EQ. 2) THEN
        CALL GRLINE (PLANE (1,1), PLANE (1,2))
        CALL GRLINE (PLANE (1,3), PLANE (1,4))
        RETURN
      END IF
C
C If either the new or the old segment ends on the front side of the helix,
C draw another front side segment
C
      IF (ONORM(3) .GE. 0 .OR. PNORM(3) .GE. 0) THEN
        CALL V3SCAL (OFFSET, RECESS * HELTHK, ONORM)
        CALL V3ADD  (SURF (1,1), OFFSET, PLANE (1,1) )
        CALL V3ADD  (SURF (1,4), OFFSET, PLANE (1,4) )
        CALL V3SCAL (OFFSET, RECESS * HELTHK, PNORM)
        CALL V3ADD  (SURF (1,2), OFFSET, PLANE (1,2) )
        CALL V3ADD  (SURF (1,3), OFFSET, PLANE (1,3) )
        CALL GRPLAN (SURF, .FALSE., PL1234, .TRUE., ZDIR)
      END IF
C
C If either the new or the old segment ends on the back side of the helix,
C draw another back side segment
C
      IF (ONORM(3) .LE. 0 .OR. PNORM(3) .LE. 0) THEN
        CALL V3SCAL (OFFSET, -RECESS * HELTHK, ONORM)
        CALL V3ADD  (SURF (1,1), OFFSET, PLANE (1,1) )
        CALL V3ADD  (SURF (1,4), OFFSET, PLANE (1,4) )
        CALL V3SCAL (OFFSET, -RECESS * HELTHK, PNORM)
        CALL V3ADD  (SURF (1,2), OFFSET, PLANE (1,2) )
        CALL V3ADD  (SURF (1,3), OFFSET, PLANE (1,3) )
        CALL GRPLAN (SURF, .FALSE., PL1234, .TRUE., ZDIR)
      END IF
C
C Save plane normal for next time
C
      CALL V3COPY (ONORM, PNORM)
C
      RETURN
      END
C
C
C-------------------------------
      SUBROUTINE GRCHPL (NEWTYP)
C
      INCLUDE 'molscript.dim'
      INCLUDE 'graphics.inc'
C
      INTEGER NEWTYP
C
C NEWTYP  (In) new plane type
C
C Retain primary or secondary characteristic
C
C Skip if previous graphical segment is not plane
C
      IF (TOTSEG .EQ. 0) RETURN
      IF (ABS (SEGTYP (TOTSEG)) .LT. PL1234) RETURN
      IF (ABS (SEGTYP (TOTSEG)) .GT. PL) RETURN
C
      IF (SEGTYP (TOTSEG) .LT. 0) THEN
        SEGTYP (TOTSEG) = - NEWTYP
      ELSE
        SEGTYP (TOTSEG) = NEWTYP
      END IF
C
      RETURN
      END
C
C
C---------------------------------
      SUBROUTINE GRLABL (STR, XYZ)
C
      INCLUDE 'molscript.dim'
      INCLUDE 'graphics.inc'
C
      CHARACTER*(*) STR
      REAL          XYZ (3)
C
C STR  (In)  character string
C XYZ  (In)  position
C
C Ignore if position outside of view
C
      IF (WIND .GT. 0.0  .AND.  .NOT. RASTER) THEN
        IF (XYZ (1) .LT. - ASWIND (1)) RETURN
        IF (XYZ (1) .GT. ASWIND (1)) RETURN
        IF (XYZ (2) .LT. - ASWIND (2)) RETURN
        IF (XYZ (2) .GT. ASWIND (2)) RETURN
      END IF
C
      IF (CLIP .GT. 0.0) THEN
        IF (XYZ (3) .LT. -CLIP) RETURN
        IF (XYZ (3) .GT.  CLIP) RETURN
      END IF
C
      IF (TOTLAB .GE. MAXLAB) CALL MABORT ('no space for label; MAXLAB')
C
      TOTLAB = TOTLAB + 1
      LABSTR (TOTLAB) = STR
      LABLEN (TOTLAB) = LEN (STR)
      CALL V3ADD (LABCOO (1, TOTLAB), XYZ, LABOFF)
C
      CALL GRSEG (LABEL, TOTLAB, XYZ (3) + LABOFF (3))
C
      RETURN
      END
C
C
C-------------------------------------------------
      SUBROUTINE GRSTIK (ACOO1, ACOO2, RAD1, RAD2)
C
      INCLUDE 'molscript.dim'
      INCLUDE 'molscript.inc'
      INCLUDE 'graphics.inc'
C
      REAL ACOO1 (3), ACOO2 (3), RAD1, RAD2
C
C ACOO1  (In) atom coordinates
C ACOO2
C RAD1   (In) atom radii
C RAD2
C
C Externally defined functions
C
      REAL V3DIFF
C
C Help variables
C
      REAL RADIUS, SHORT1, SHORT2
      REAL VEC (3), COO1 (3), COO2 (3), PERP (3), PLANE (3, 4)
C
C Ignore if entirely outside of view
C
      IF (WIND .GT. 0.0  .AND.  .NOT. RASTER) THEN
        IF (ACOO1 (1) .LT. - ASWIND (1)  .AND.
     $      ACOO2 (1) .LT. - ASWIND (1)) RETURN
        IF (ACOO1 (2) .LT. - ASWIND (2)  .AND.
     $      ACOO2 (2) .LT. - ASWIND (2)) RETURN
        IF (ACOO1 (1) .GT. ASWIND (1)  .AND.
     $      ACOO2 (1) .GT. ASWIND (1)) RETURN
        IF (ACOO1 (2) .GT. ASWIND (2)  .AND.
     $      ACOO2 (2) .GT. ASWIND (2)) RETURN
      END IF
C
      IF (CLIP .GT. 0.0) THEN
        IF (ACOO1 (3) .LT. -CLIP  .AND.
     $      ACOO2 (3) .LT. -CLIP) RETURN
        IF (ACOO1 (3) .GT.  CLIP  .AND.
     $      ACOO2 (3) .GT.  CLIP) RETURN
      END IF
C
C Stick vector; skip if hidden by either ball
C
      CALL V3SUBT (VEC, ACOO2, ACOO1)
      IF (SQRT (VEC (1) **2 + VEC (2) **2) .LE. MIN (RAD1, RAD2)) RETURN
      CALL V3NORM (VEC, VEC)
C
C If stick is nearly flat on xy plane, then output as plane
C
      IF (ACOS (ABS (VEC (3)))  .GT.  84.0 * TORAD   .AND.
     $    .NOT. RASTER) THEN
C
C Reduce stick radius somewhat when plane, depending on tapering factor
C
        RADIUS = (1.0 - STKTAP (TOTGST) * 0.2) * STKRAD (TOTGST)
C
C Shorten stick; skip if negative length
C
        SHORT1 = SQRT (MAX (0.0, RAD1 **2 - RADIUS **2))
        SHORT2 = SQRT (MAX (0.0, RAD2 **2 - RADIUS **2))
        IF (SHORT1 + SHORT2 .GE. V3DIFF (ACOO1, ACOO2)) RETURN
C
C Shorten stick end points projected onto xy plane
C
        VEC (3) = 0.0
        CALL V3NORM (VEC, VEC)
        CALL V3SCAL (PERP, SHORT1, VEC)
        CALL V3ADD  (COO1, ACOO1, PERP)
        CALL V3SCAL (PERP, SHORT2, VEC)
        CALL V3SUBT (COO2, ACOO2, PERP)
C
        CALL V3CROS (PERP, VEC, Z)
        CALL V3NORM (PERP, PERP)
        CALL V3SCAL (PERP, RADIUS, PERP)
C
        CALL V3ADD  (PLANE (1, 1), COO1, PERP)
        CALL V3ADD  (PLANE (1, 2), COO2, PERP)
        CALL V3SUBT (PLANE (1, 3), COO2, PERP)
        CALL V3SUBT (PLANE (1, 4), COO1, PERP)
C
C To put plane in correct position for depth sort
C
        PLANE (3, 1) = MAX (ACOO1 (3) + 0.5 * RAD1,
     $                      ACOO2 (3) + 0.5 * RAD2) + 0.001
C
        CALL GRPLAN (PLANE, .FALSE., PL1234, .TRUE., 1.0)
C
C Proper stick
C
      ELSE
C
C Shorten stick; skip if negative length
C
        RADIUS = STKRAD (TOTGST)
        SHORT1 = SQRT (MAX (0.0, RAD1 **2 - RADIUS **2))
        SHORT2 = SQRT (MAX (0.0, RAD2 **2 - RADIUS **2))
        IF (SHORT1 + SHORT2 .GE. V3DIFF (ACOO1, ACOO2)) RETURN
C
        IF (TOTSTK .GE. MAXSTK)
     $    CALL MABORT ('no space for stick; MAXSTK')
C
        TOTSTK = TOTSTK + 1
C
C Closest coordinate first
C
        IF (ACOO1 (3) .GT. ACOO2 (3)) THEN
          CALL V3COPY (STKCOO (1, 1, TOTSTK), ACOO1)
          CALL V3SCAL (VEC, SHORT2, VEC)
          CALL V3SUBT (STKCOO (1, 2, TOTSTK), ACOO2, VEC)
        ELSE
          CALL V3COPY (STKCOO (1, 1, TOTSTK), ACOO2)
          CALL V3SCAL (VEC, SHORT1, VEC)
          CALL V3ADD  (STKCOO (1, 2, TOTSTK), ACOO1, VEC)
        END IF
C
        CALL GRSEG (STICK, TOTSTK, (ACOO1 (3) + 0.5 * RAD1 +
     $                              ACOO2 (3) + 0.5 * RAD2) / 2.0)
C
      END IF
C
      RETURN
      END
C
C
C---------------------------------------
      SUBROUTINE GRSEG (TYPE, NUM, ZCOO)
C
      INCLUDE 'molscript.dim'
      INCLUDE 'graphics.inc'
C
      INTEGER TYPE, NUM
      REAL    ZCOO
C
C TYPE  (In) segment type
C NUM   (In) segment number
C ZCOO  (In) segment z coordinate
C
      IF (TOTSEG .GE. MAXSEG)
     $  CALL MABORT ('no space for segment; MAXSEG')
C
      TOTSEG = TOTSEG + 1
      SEGZ   (TOTSEG) = ZCOO
C
      SEGTYP (TOTSEG) = TYPE
      SEGNUM (TOTSEG) = NUM
      SEGGST (TOTSEG) = TOTGST
C
      RETURN
      END
C
C
C----------------------
      SUBROUTINE GRPOST
C
      INCLUDE 'molscript.dim'
      INCLUDE 'molscript.inc'
      INCLUDE 'graphics.inc'
C
C Externally defined function
C
      INTEGER  GRCOMP
      EXTERNAL GRCOMP
      REAL     GRDCUE
C
C Help variables
C
      REAL    RVAL, SCALE, FSCALE
      REAL    SHADE (3), DIR (3), VOFF (3), POS (3)
      INTEGER PTR, SEG, TYP, NUM, GST, SUMPLA, PREV, SLOT, ERRCOD
      INTEGER SEGPTR (MAXSEG), QSTACK (MAXQST)
      LOGICAL SYMBOL
C
C Check if any segments at all
C
      IF (TOTSEG .EQ. 0) CALL MABORT ('no graphical segments to output')
C
C Set window and slab sizes, if not explicitly set
C
      IF (WIND .LT. 0.0) CALL GRSWIN
      IF (CLIP .LT. 0.0) CALL GRSCLI
C
C Compute scale factor from Angstrom to PostScript default unit
C
      IF (ASPECT .GT. 1.0) THEN
        SCALE = (AREA (3) - AREA (1)) / (2.0 * WIND)
      ELSE
        SCALE = (AREA (4) - AREA (2)) / (2.0 * WIND)
      END IF
C
C Depth sort; for hidden line/surface removal
C
      DO 100 SEG = 1, TOTSEG
        SEGPTR (SEG) = SEG
100   CONTINUE
      CALL QSORTP (SEGPTR, TOTSEG, QSTACK, MAXQST, GRCOMP, ERRCOD)
      IF (ERRCOD .NE. 0) CALL MABORT ('too small qsort stack; MAXQST')
C
C Set PostScript state variable values
C
      CALL PSIPAR
C
C Save PostScript VM state
C
      CALL PSSTR ('/MolScriptPlotSave save def')
      CALL PSOUT
C
C Create plot area path
C
      CALL PSREAL (AREA (1), 2)
      CALL PSREAL (AREA (2), 2)
      CALL PSSTR ('moveto')
      CALL PSREAL (AREA (3), 2)
      CALL PSREAL (AREA (2), 2)
      CALL PSSTR ('lineto')
      CALL PSREAL (AREA (3), 2)
      CALL PSREAL (AREA (4), 2)
      CALL PSSTR ('lineto')
      CALL PSREAL (AREA (1), 2)
      CALL PSREAL (AREA (4), 2)
      CALL PSSTR ('lineto closepath gsave')
      CALL PSOUT
C
C Fill in background colour and set clip path
C
      CALL PSSTR ('gsave')
      CALL PSCOLR (BCKCOL)
      CALL PSSTR ('fill grestore')
      CALL PSOUT
      CALL PSSTR ('clip newpath')
      CALL PSOUT
C
C Set view transformation; cube volume to conserve plane vectors
C
      CALL C3INIT (.TRUE.)
      CALL C3OPRJ (-WIND, WIND, -WIND, WIND, WIND, -WIND)
      RVAL = MIN ((AREA (3) - AREA (1)) / 2.0,
     $            (AREA (4) - AREA (2)) / 2.0)
      CALL C3SCAL (RVAL, RVAL, RVAL)
      CALL C3TRAN ((AREA (1) + AREA (3)) / 2.0,
     $             (AREA (2) + AREA (4)) / 2.0, 0.0)
C
C Transform coordinates to view coordinate system; not for sticks
C
      DO 200 SEG = 1, TOTLIN
        CALL C3TRFC (LINCOO (1, 1, SEG))
        CALL C3TRFC (LINCOO (1, 2, SEG))
200   CONTINUE
      DO 210 SEG = 1, TOTSPH
        CALL C3TRFC (SPHCOO (1, SEG))
210   CONTINUE
      DO 220 SEG = 1, TOTPLA
        CALL C3TRFC (PLACOO (1, 1, SEG))
        CALL C3TRFC (PLACOO (1, 2, SEG))
220   CONTINUE
      DO 230 SEG = 1, TOTLAB
        CALL C3TRFC (LABCOO (1, SEG))
230   CONTINUE
C
C Output graphical segments to PostScript
C
      CALL MSGSTR ('writing graphical segments to PostScript output...')
      CALL MSGOUT
C
      SUMPLA = 0
C
      DO 1000 PTR = 1, TOTSEG
C
C Get segment and its number and graphics state from pointer
C
        SEG = SEGPTR (PTR)
        TYP = SEGTYP (SEG)
        NUM = SEGNUM (SEG)
        GST = SEGGST (SEG)
C
C Jump to segment output; ignore primary or secondary
C
        GOTO (1, 2, 3, 3, 3, 3, 3, 8, 9) ABS (TYP)
C
        CALL ERROR ('internal')
        CALL MSGSTR ('unknown graphical segment in GRPOST')
        CALL MSGINT (TYP)
        CALL MSGOUT
        CALL MABORT (' ')
C
C Line; apply depth cue to line width
C
   1    CALL PSSLWD (GRDCUE (SEGZ (SEG), GST, .FALSE.) * LINEWD (GST),
     $               LINDSH (GST))
        CALL PSSLNC (LINCOL (1, GST))
C
        CALL PSREAL (LINCOO (1, 1, NUM), 2)
        CALL PSREAL (LINCOO (2, 1, NUM), 2)
        CALL PSREAL (LINCOO (1, 2, NUM), 2)
        CALL PSREAL (LINCOO (2, 2, NUM), 2)
        CALL PSSTR ('Line')
        GOTO 8000
C
C Sphere; apply depth cue to line width and fill colour
C
   2    CALL PSSLWD (GRDCUE (SEGZ (SEG), GST, .FALSE.) * LINEWD (GST),
     $               LINDSH (GST))
        CALL PSSLNC (LINCOL (1, GST))
C
        CALL GRSHAD (SHADE, SPHCOL (1, NUM),
     $               GRDCUE (SEGZ (SEG), GST, .TRUE.))
        CALL PSSPHC (SHADE)
C
        CALL PSREAL (SPHCOO (1, NUM), 2)
        CALL PSREAL (SPHCOO (2, NUM), 2)
C
C Convert radius into view coordinate system length
C
        CALL PSREAL (SCALE * SPHRAD (NUM), 2)
        CALL PSSTR ('Sphere')
        GOTO 8000
C
C Plane segment; apply depth cue to line width
C
   3    CALL PSSLWD (GRDCUE (SEGZ (SEG), GST, .FALSE.) * LINEWD (GST),
     $               LINDSH (GST))
        CALL PSSLNC (LINCOL (1, GST))
C
C Shade colour; primary or secondary colour from sign of segment type
C
        IF (TYP .GT. 0) THEN
          CALL GRSHAD (SHADE, PLPCOL (1, GST), PLASHD (NUM))
        ELSE
          CALL GRSHAD (SHADE, PLSCOL (1, GST), PLASHD (NUM))
        END IF
C
C Apply depth cue to shade colour
C
        CALL GRSHAD (SHADE, SHADE, GRDCUE (SEGZ (SEG), GST, .TRUE.))
        CALL PSSPLC (SHADE)
C
C Order fits definition of Plane procedure; reverse of 1, 2, 3, 4
C
        CALL PSREAL (PLACOO (1, 2, NUM - 1), 2)
        CALL PSREAL (PLACOO (2, 2, NUM - 1), 2)
        CALL PSREAL (PLACOO (1, 2, NUM), 2)
        CALL PSREAL (PLACOO (2, 2, NUM), 2)
        CALL PSREAL (PLACOO (1, 1, NUM), 2)
        CALL PSREAL (PLACOO (2, 1, NUM), 2)
        CALL PSREAL (PLACOO (1, 1, NUM - 1), 2)
        CALL PSREAL (PLACOO (2, 1, NUM - 1), 2)
C
C Decide which plane definition to use based on type
C
        TYP = ABS (TYP)
        IF (TYP .EQ. PL1234) THEN
          CALL PSSTR ('Pl1234')
        ELSE IF (TYP .EQ. PL13) THEN
          CALL PSSTR ('Pl13')
        ELSE IF (TYP .EQ. PL123) THEN
          CALL PSSTR ('Pl123')
        ELSE IF (TYP .EQ. PL134) THEN
          CALL PSSTR ('Pl134')
        ELSE IF (TYP .EQ. PL) THEN
          CALL PSSTR ('Plane')
        ELSE
          CALL MABORT ('internal: no such plane type')
        END IF
        SUMPLA = SUMPLA + 1
        GOTO 8000
C
C Label; postpone output if not to be clipped
C
   8    IF (.NOT. LABCLP (GST)) GOTO 8000
C
C Linecolour applies to characters, depth cue to font scale
C
        CALL PSSLNC (LINCOL (1, GST))
        FSCALE = GRDCUE (SEGZ (SEG), GST, .FALSE.) * LABSIZ (GST)
C
C No change/lowcase/upcase according to mask
C
        DO 1800 SLOT = 1, LABLEN (NUM)
          PREV = MOD (LABMSK (SLOT, GST), 3)
          IF (PREV .EQ. 1) THEN
            CALL SLOCAS (LABSTR (NUM) (SLOT : SLOT))
          ELSE IF (PREV .EQ. 2) THEN
            CALL SUPCAS (LABSTR (NUM) (SLOT : SLOT))
          END IF
1800    CONTINUE
C
C Position and rotation for label; approximate that both fonts are same
C
        CALL PSCPOS (LABSTR (NUM) (: LABLEN (NUM)), FSCALE,
     $               LABCOO (1, NUM), LABCTR (GST), LABROT (GST))
C
C Print label using font according to mask
C
        PREV = 1
        SYMBOL = LABMSK (1, GST) / 3 .EQ. 1
C
C Output only when change of font
C
        DO 1810 SLOT = 2, LABLEN (NUM)
          IF (LABMSK (SLOT, GST) / 3 .EQ. 1  .NEQV. SYMBOL) THEN
            CALL PSPRNT (LABSTR (NUM) (PREV : SLOT - 1), FSCALE, SYMBOL)
            SYMBOL = .NOT. SYMBOL
            PREV = SLOT
          END IF
1810    CONTINUE
C
C Output last string and remove rotation, if any
C
        CALL PSPRNT (LABSTR (NUM) (PREV : LABLEN (NUM)), FSCALE, SYMBOL)
        IF (LABROT (GST)) CALL PSSTR ('grestore')
        GOTO 8000
C
C Stick; compute direction vector
C
   9    CALL V3SUBT (DIR, STKCOO (1, 2, NUM), STKCOO (1, 1, NUM))
        CALL V3NORM (DIR, DIR)
C
C Compute perpendicular vector
C
        CALL V3CROS (VOFF, Z, DIR)
        CALL V3NORM (VOFF, VOFF)
        RVAL = SCALE * STKRAD (GST)
        CALL V3SCAL (VOFF, RVAL, VOFF)
C
C Transform coordinates
C
        CALL C3TRFC (STKCOO (1, 1, NUM))
        CALL C3TRFC (STKCOO (1, 2, NUM))
C
C Apply depth cue to linewidth; apply linecolour
C
        CALL PSSLWD (GRDCUE (SEGZ (SEG), GST, .FALSE.) * LINEWD (GST),
     $               LINDSH (GST))
        CALL PSSLNC (LINCOL (1, GST))
C
C Output elliptic arc part of stick; tapering off
C
        CALL PSREAL (STKCOO (1, 2, NUM), 2)
        CALL PSREAL (STKCOO (2, 2, NUM), 2)
        CALL PSREAL (RVAL * ((1.0 - STKTAP (GST)) +
     $                       STKTAP (GST) * ACOS (ABS (DIR (3))) /
     $                       (90.0 * TORAD)), 2)
        RVAL = DIR (1) / SQRT (DIR (1) **2 + DIR (2) **2)
        IF (DIR (2) .GE. 0.0) THEN
          CALL PSREAL (ACOS (RVAL) / TORAD, 2)
        ELSE
          CALL PSREAL (360.0 - ACOS (RVAL) / TORAD, 2)
        END IF
        CALL PSREAL (ABS (DIR (3)), 2)
C
C Output straight line part of stick, and make path
C
        CALL V3SUBT (POS, STKCOO (1, 1, NUM), VOFF)
        CALL PSREAL (POS (1), 2)
        CALL PSREAL (POS (2), 2)
        CALL V3ADD  (POS, STKCOO (1, 1, NUM), VOFF)
        CALL PSREAL (POS (1), 2)
        CALL PSREAL (POS (2), 2)
        CALL PSSTR ('Stickpath')
        CALL PSOUT
C
C Fill colour from primary plane; apply depth cue
C
        CALL GRSHAD (SHADE, PLPCOL (1, GST),
     $               GRDCUE (SEGZ (SEG), GST, .TRUE.))
        CALL PSSPLC (SHADE)
C
C Finish stick; fill and stroke stick
C
        CALL PSSTR ('Stickfill')
        GOTO 8000
C
8000    CALL PSOUT
C
1000  CONTINUE
C
C Output those labels that are not to be clipped
C
      DO 2000 PTR = 1, TOTSEG
C
C Get segment and its number and graphics state from pointer
C
        SEG = SEGPTR (PTR)
        TYP = SEGTYP (SEG)
        NUM = SEGNUM (SEG)
        GST = SEGGST (SEG)
C
C Skip if not label, and clipped
C
        IF (TYP .NE. LABEL) GOTO 2000
        IF (LABCLP (GST)) GOTO 2000
C
C Linecolour applies, depth cue applies to font scale
C
        CALL PSSLNC (LINCOL (1, GST))
        FSCALE = GRDCUE (SEGZ (SEG), GST, .FALSE.) * LABSIZ (GST)
C
C No change/lowcase/upcase according to mask
C
        DO 2800 SLOT = 1, LABLEN (NUM)
          PREV = MOD (LABMSK (SLOT, GST), 3)
          IF (PREV .EQ. 1) THEN
            CALL SLOCAS (LABSTR (NUM) (SLOT : SLOT))
          ELSE IF (PREV .EQ. 2) THEN
            CALL SUPCAS (LABSTR (NUM) (SLOT : SLOT))
          END IF
2800     CONTINUE
C
C Position and rotation for label; approximate that both fonts are same
C
        CALL PSCPOS (LABSTR (NUM) (: LABLEN (NUM)), FSCALE,
     $               LABCOO (1, NUM), LABCTR (GST), LABROT (GST))
C
C Print label using font according to mask
C
        PREV = 1
        SYMBOL = LABMSK (1, GST) / 3 .EQ. 1
C
C Output only when change of font
C
        DO 2810 SLOT = 2, LABLEN (NUM)
          IF (LABMSK (SLOT, GST) / 3 .EQ. 1  .NEQV. SYMBOL) THEN
            CALL PSPRNT (LABSTR (NUM) (PREV : SLOT - 1), FSCALE, SYMBOL)
            SYMBOL = .NOT. SYMBOL
            PREV = SLOT
          END IF
2810     CONTINUE
C
C Output last string and remove rotation, if any
C
        CALL PSPRNT (LABSTR (NUM) (PREV : LABLEN (NUM)), FSCALE, SYMBOL)
        IF (LABROT (GST)) CALL PSSTR ('grestore')
        CALL PSOUT
        
2000  CONTINUE
C
C Output frame, if any
C
      IF (FRAME) THEN
        CALL PSSTR ('grestore stroke')
      ELSE
        CALL PSSTR ('grestore newpath')
      END IF
      CALL PSOUT
C
C Reset PostScript VM state
C
      CALL PSSTR ('MolScriptPlotSave restore')
      CALL PSOUT
C
C Update bounding box; irrelevant if Encapsulated PostScript
C
      BOUBOX (1) = MIN (BOUBOX (1), AREA (1))
      BOUBOX (2) = MIN (BOUBOX (2), AREA (2))
      BOUBOX (3) = MAX (BOUBOX (3), AREA (3))
      BOUBOX (4) = MAX (BOUBOX (4), AREA (4))
C
C Info on output segments
C
      CALL MSGINT (TOTLIN)
      CALL MSGSTR ('lines,')
      CALL MSGINT (TOTSPH)
      CALL MSGSTR ('spheres,')
      CALL MSGINT (SUMPLA)
      CALL MSGSTR ('planes,')
      CALL MSGINT (TOTSTK)
      CALL MSGSTR ('sticks and')
      CALL MSGINT (TOTLAB)
      CALL MSGSTR ('labels written')
      CALL MSGOUT
C
      RETURN
      END
C
C
C----------------------
      SUBROUTINE GRRAST
C
      INCLUDE 'molscript.dim'
      INCLUDE 'molscript.inc'
      INCLUDE 'graphics.inc'
C
C Externally defined function
C
      REAL GRDCUE
C
C Help variables
C
      REAL    RVAL, SCALE
      REAL    SHADE (3)
      INTEGER PTR, SEG, TYP, NUM, GST, SUMPLA
      INTEGER SEGPTR (MAXSEG)
C
C Set window and slab sizes, if not explicitly set
C
      IF (WIND .LT. 0.0) CALL GRSWIN
      IF (CLIP .LT. 0.0) CALL GRSCLI
C
C Compute scale factor from Angstrom to output units
C
      IF (ASPECT .GT. 1.0) THEN
        SCALE = (AREA (3) - AREA (1)) / (2.0 * WIND)
      ELSE
        SCALE = (AREA (4) - AREA (2)) / (2.0 * WIND)
      END IF
C
C Set view transformation; cube volume to conserve plane vectors
C
      CALL C3INIT (.TRUE.)
      CALL C3OPRJ (-WIND, WIND, -WIND, WIND, WIND, -WIND)
      RVAL = MIN ((AREA (3) - AREA (1)) / 2.0,
     $            (AREA (4) - AREA (2)) / 2.0)
      CALL C3SCAL (RVAL, RVAL, RVAL)
      CALL C3TRAN ((AREA (1) + AREA (3)) / 2.0,
     $             (AREA (2) + AREA (4)) / 2.0, 0.0)
C
C Set up segment pointers
C
      DO 100 SEG = 1, TOTSEG
        SEGPTR (SEG) = SEG
100   CONTINUE
C
C Transform coordinates to view coordinate system
C
      DO 200 SEG = 1, TOTSPH
        CALL C3TRFC (SPHCOO (1, SEG))
200   CONTINUE
      DO 210 SEG = 1, TOTPLA
        CALL C3TRFC (PLACOO (1, 1, SEG))
        CALL C3TRFC (PLACOO (1, 2, SEG))
210   CONTINUE
C
C Also sticks and lines for Raster3D v2.0 (Ethan Merritt)
C
      DO 220 SEG = 1, TOTSTK
        CALL C3TRFC (STKCOO (1, 1, SEG))
        CALL C3TRFC (STKCOO (1, 2, SEG))
 220  CONTINUE
      DO 230 SEG = 1, TOTLIN
        CALL C3TRFC (LINCOO (1, 1, SEG))
        CALL C3TRFC (LINCOO (1, 2, SEG))
 230  CONTINUE
C
C Output graphical segments Raster3D file
C
      CALL MSGSTR ('writing graphical segments to Raster3D output...')
      CALL MSGOUT
C
      SUMPLA = 0
C
      DO 1000 PTR = 1, TOTSEG
C
C Get segment and its number and graphics state from pointer
C
        SEG = SEGPTR (PTR)
        TYP = SEGTYP (SEG)
        NUM = SEGNUM (SEG)
        GST = SEGGST (SEG)
C
C Jump to segment output; ignore primary or secondary
C
        GOTO (1, 2, 3, 3, 3, 3, 3, 8, 9) ABS (TYP)
C
        CALL ERROR ('internal')
        CALL MSGSTR ('unknown graphical segment in GRRAST')
        CALL MSGINT (TYP)
        CALL MSGOUT
        CALL MABORT (' ')
C
C Lines are usually output as round-ended cylinders
C a negative value for the line width flags use of a flat-ended cylinder instead
C
   1    CONTINUE
        RVAL = SCALE * LINEWD (GST)
	IF (RVAL.GE.0) THEN
	  CALL PSSTR ('3')
	ELSE
	  RVAL = -RVAL
	  CALL PSSTR ('5')
	ENDIF
        CALL PSOUT
C
C Output start point and radius
C
        CALL PSREAL (LINCOO (1, 1, NUM), 6)
        CALL PSREAL (LINCOO (2, 1, NUM), 6)
        CALL PSREAL (LINCOO (3, 1, NUM), 6)
        CALL PSREAL (RVAL, 6)
C
C Output end point and residue
C
        CALL PSREAL (LINCOO (1, 2, NUM), 6)
        CALL PSREAL (LINCOO (2, 2, NUM), 6)
        CALL PSREAL (LINCOO (3, 2, NUM), 6)
        CALL PSREAL (RVAL, 6)
C
C Line colour from primary line, RGB, depth cue;
C square component values to match behaviour of Raster3D
C
        CALL GRSHAD (SHADE, LINCOL (1, GST),
     $               GRDCUE (SEGZ (SEG), GST, .TRUE.))
        CALL PSREAL (SHADE (1) **2, 4)
        CALL PSREAL (SHADE (2) **2, 4)
        CALL PSREAL (SHADE (3) **2, 4)
        GOTO 8000
C
C Sphere
C
   2    CALL PSSTR ('2')
        CALL PSOUT
C
        CALL PSREAL (SPHCOO (1, NUM), 6)
        CALL PSREAL (SPHCOO (2, NUM), 6)
        CALL PSREAL (SPHCOO (3, NUM), 6)
C
C Convert radius into view coordinate system length
C
        CALL PSREAL (SCALE * SPHRAD (NUM), 6)
C
C Sphere colour, RGB; depth cue
C square component values to match behaviour of Raster3D
C
        CALL GRSHAD (SHADE, SPHCOL (1, NUM),
     $               GRDCUE (SEGZ (SEG), GST, .TRUE.))
        CALL PSREAL (SHADE (1) **2, 4)
        CALL PSREAL (SHADE (2) **2, 4)
        CALL PSREAL (SHADE (3) **2, 4)
        GOTO 8000
C
C Plane segment; depth cue colour;
C square component values to match behaviour of Raster3D
C
   3    IF (TYP .GT. 0) THEN
          CALL GRSHAD (SHADE, PLPCOL (1, GST),
     $                 GRDCUE (SEGZ (SEG), GST, .TRUE.))
        ELSE
          CALL GRSHAD (SHADE, PLSCOL (1, GST),
     $                 GRDCUE (SEGZ (SEG), GST, .TRUE.))
        END IF
        SHADE (1) = SHADE (1) **2
        SHADE (2) = SHADE (2) **2
        SHADE (3) = SHADE (3) **2
C
C Output as two triangles
C
        CALL PSSTR ('1')
        CALL PSOUT
        CALL PSREAL (PLACOO (1, 1, NUM - 1), 6)
        CALL PSREAL (PLACOO (2, 1, NUM - 1), 6)
        CALL PSREAL (PLACOO (3, 1, NUM - 1), 6)
        CALL PSREAL (PLACOO (1, 2, NUM - 1), 6)
        CALL PSREAL (PLACOO (2, 2, NUM - 1), 6)
        CALL PSREAL (PLACOO (3, 2, NUM - 1), 6)
        CALL PSREAL (PLACOO (1, 1, NUM), 6)
        CALL PSREAL (PLACOO (2, 1, NUM), 6)
        CALL PSREAL (PLACOO (3, 1, NUM), 6)
C
        CALL PSREAL (SHADE (1), 4)
        CALL PSREAL (SHADE (2), 4)
        CALL PSREAL (SHADE (3), 4)
        CALL PSOUT
C
        CALL PSSTR ('1')
        CALL PSOUT
        CALL PSREAL (PLACOO (1, 2, NUM - 1), 6)
        CALL PSREAL (PLACOO (2, 2, NUM - 1), 6)
        CALL PSREAL (PLACOO (3, 2, NUM - 1), 6)
        CALL PSREAL (PLACOO (1, 1, NUM), 6)
        CALL PSREAL (PLACOO (2, 1, NUM), 6)
        CALL PSREAL (PLACOO (3, 1, NUM), 6)
        CALL PSREAL (PLACOO (1, 2, NUM), 6)
        CALL PSREAL (PLACOO (2, 2, NUM), 6)
        CALL PSREAL (PLACOO (3, 2, NUM), 6)
C
        CALL PSREAL (SHADE (1), 4)
        CALL PSREAL (SHADE (2), 4)
        CALL PSREAL (SHADE (3), 4)
C
        SUMPLA = SUMPLA + 2
        GOTO 8000
C
C Label; not implemented
C
   8    CALL MABORT ('internal: tried to output label to Raster3D file')
C
C Stick
C
   9    CALL PSSTR ('3')
        CALL PSOUT
C
C Output start point and radius
C
        CALL PSREAL (STKCOO (1, 1, NUM), 6)
        CALL PSREAL (STKCOO (2, 1, NUM), 6)
        CALL PSREAL (STKCOO (3, 1, NUM), 6)
        RVAL = SCALE * STKRAD (GST)
        CALL PSREAL (RVAL, 6)
C
C Output end point and radius
C
        CALL PSREAL (STKCOO (1, 2, NUM), 6)
        CALL PSREAL (STKCOO (2, 2, NUM), 6)
        CALL PSREAL (STKCOO (3, 2, NUM), 6)
        CALL PSREAL (RVAL, 6)
C
C Stick colour from primary plane, RGB; depth cue;
C square component values to match behaviour of Raster3D
C
        CALL GRSHAD (SHADE, PLPCOL (1, GST),
     $               GRDCUE (SEGZ (SEG), GST, .TRUE.))
        CALL PSREAL (SHADE (1) **2, 4)
        CALL PSREAL (SHADE (2) **2, 4)
        CALL PSREAL (SHADE (3) **2, 4)
        GOTO 8000
C
8000    CALL PSOUT
C
1000  CONTINUE
C
      CALL MSGINT (TOTLIN)
      CALL MSGSTR ('lines,')
      CALL MSGINT (TOTSPH)
      CALL MSGSTR ('spheres,')
      CALL MSGINT (SUMPLA)
      CALL MSGSTR ('planes and ')
      CALL MSGINT (TOTSTK)
      CALL MSGSTR ('sticks written')
      CALL MSGOUT
C
      RETURN
      END
C
C
C-----------------------------------------
      INTEGER FUNCTION GRCOMP (SEG1, SEG2)
C
      INCLUDE 'molscript.dim'
      INCLUDE 'graphics.inc'
C
      INTEGER SEG1, SEG2
C
C SEG  (In) segments
C
      IF (SEGZ (SEG1) .LT. SEGZ (SEG2)) THEN
        GRCOMP = -1
      ELSE IF (SEGZ (SEG1) .GT. SEGZ (SEG2)) THEN
        GRCOMP = 1
      ELSE
        GRCOMP = 0
      END IF
C
      RETURN
      END
C
C
C---------------------------------------------
      REAL FUNCTION GRDCUE (ZCOO, GST, COLOUR)
C
      INCLUDE 'molscript.dim'
      INCLUDE 'graphics.inc'
C
      REAL    ZCOO
      INTEGER GST
      LOGICAL COLOUR
C
C ZCOO    (In) z coordinate
C GST     (In) graphics state slot
C COLOUR  (In) colour shading depth cue, otherwise general
C
C Clip slab not yet determined; no depth cue
C
      IF (CLIP .LE. 0.0) THEN
        GRDCUE = 1.0
C
C Colour shading depth cue; positive or negative
C
      ELSE IF (COLOUR) THEN
        IF (DCCOLR (GST) .GE. 0.0) THEN
          GRDCUE = (MIN (1.0, MAX (-1.0, ZCOO / CLIP)) / 2.0 + 0.5) *
     $             DCCOLR (GST) + 1.0 - DCCOLR (GST)
        ELSE
          GRDCUE = (MIN (1.0, MAX (-1.0, - ZCOO / CLIP)) / 2.0 + 0.5) *
     $             (- DCCOLR (GST)) + 1.0 + DCCOLR (GST)
        END IF
C
C General depth cue
C
      ELSE
        GRDCUE = (MIN (1.0, MAX (-1.0, ZCOO / CLIP)) / 2.0 + 0.5) *
     $           DCUEFC (GST) + 1.0 - DCUEFC (GST)
      END IF
C
      RETURN
      END
C
C
C----------------------------------------------
      SUBROUTINE GRSHAD (SHADE, COLOUR, FACTOR)
C
      REAL SHADE (3), COLOUR (3), FACTOR
C
C SHADE   (In) shaded colour
C COLOUR  (In) colour, RGB or HSB
C FACTOR  (In) shading factor
C
C HSB specification; change brightness
C
      IF (COLOUR (1) .GE. 10.0) THEN
        CALL V3COPY (SHADE, COLOUR)
        SHADE (3) = SHADE (3) * FACTOR
C
C RGB specification; shade proportionally
C
      ELSE
        CALL V3SCAL (SHADE, FACTOR, COLOUR)
      END IF
C
      RETURN
      END
C
C
C----------------------
      SUBROUTINE GRSWIN
C
      INCLUDE 'molscript.dim'
      INCLUDE 'graphics.inc'
C
C Help variables
C
      INTEGER SEG, NUM, GST
      REAL    ASP1, ASP2
C
C Compute aspect ratio factors for window
C
      IF (ASPECT .GE. 1.0) THEN
        ASP1 = 1.0
        ASP2 = 1.0 / ASPECT
      ELSE
        ASP1 = ASPECT
        ASP2 = 1.0
      END IF
C
C Loop through all segments
C
      DO 100 SEG = 1, TOTSEG
C
        NUM = SEGNUM (SEG)
        GST = SEGGST (SEG)
C
C Jump to segment type; ignore primary or secondary
C
        GOTO (1, 2, 3, 3, 3, 3, 3, 8, 9) ABS (SEGTYP (SEG))
C
        CALL ERROR ('internal')
        CALL MSGSTR ('unknown graphical segment in GRSWIN')
        CALL MSGINT (SEGTYP (SEG))
        CALL MSGOUT
        CALL MABORT (' ')
C
C Line
C
   1    WIND = MAX (WIND, ASP1 * ABS (LINCOO (1, 1, NUM)),
     $                    ASP2 * ABS (LINCOO (2, 1, NUM)))
        WIND = MAX (WIND, ASP1 * ABS (LINCOO (1, 2, NUM)),
     $                    ASP2 * ABS (LINCOO (2, 2, NUM)))
        GOTO 100
C
C Sphere
C
   2    WIND = MAX (WIND, ASP1 * (ABS (SPHCOO (1, NUM)) + SPHRAD (NUM)),
     $                    ASP2 * (ABS (SPHCOO (2, NUM)) + SPHRAD (NUM)))
        GOTO 100
C
C Plane or plane segment
C
   3    WIND = MAX (WIND, ASP1 * ABS (PLACOO (1, 1, NUM - 1)),
     $                    ASP2 * ABS (PLACOO (2, 1, NUM - 1)))
        WIND = MAX (WIND, ASP1 * ABS (PLACOO (1, 2, NUM - 1)),
     $                    ASP2 * ABS (PLACOO (2, 2, NUM - 1)))
        WIND = MAX (WIND, ASP1 * ABS (PLACOO (1, 1, NUM)),
     $                    ASP2 * ABS (PLACOO (2, 1, NUM)))
        WIND = MAX (WIND, ASP1 * ABS (PLACOO (1, 2, NUM)),
     $                    ASP2 * ABS (PLACOO (2, 2, NUM)))
        GOTO 100
C
C Label
C
   8    WIND = MAX (WIND, ASP1 * ABS (LABCOO (1, NUM)),
     $                    ASP2 * ABS (LABCOO (2, NUM)))
        GOTO 100
C
C Stick ignored; has always two spheres attached to it
C
   9    GOTO 100
C
100   CONTINUE
C
C Make fit less snug
C
      WIND = WIND + 2.0
C
      CALL MSGSTR ('setting window to')
      CALL MSGREL (2.0 * WIND, 2)
      CALL MSGOUT
C
      RETURN
      END
C
C
C----------------------
      SUBROUTINE GRSCLI
C
      INCLUDE 'molscript.dim'
      INCLUDE 'graphics.inc'
C
C Help variables
C
      INTEGER SEG, NUM, GST
C
C Loop through all segments
C
      DO 100 SEG = 1, TOTSEG
C
        NUM = SEGNUM (SEG)
        GST = SEGGST (SEG)
C
C Jump to segment type; ignore primary or secondary
C
        GOTO (1, 2, 3, 3, 3, 3, 3, 8, 9) ABS (SEGTYP (SEG))
C
        CALL ERROR ('internal')
        CALL MSGSTR ('unknown graphical segment in GRSCLI')
        CALL MSGINT (SEGTYP (SEG))
        CALL MSGOUT
        CALL MABORT (' ')
C
C Line
C
   1    CLIP = MAX (CLIP, ABS (LINCOO (3, 1, NUM)))
        CLIP = MAX (CLIP, ABS (LINCOO (3, 2, NUM)))
        GOTO 100
C
C Sphere
C
   2    CLIP = MAX (CLIP, ABS (SPHCOO (3, NUM)) + SPHRAD (NUM))
        GOTO 100
C
C Plane or plane segment
C
   3    CLIP = MAX (CLIP, ABS (PLACOO (3, 1, NUM - 1)))
        CLIP = MAX (CLIP, ABS (PLACOO (3, 2, NUM - 1)))
        CLIP = MAX (CLIP, ABS (PLACOO (3, 1, NUM)))
        CLIP = MAX (CLIP, ABS (PLACOO (3, 2, NUM)))
        GOTO 100
C
C Label
C
   8    CLIP = MAX (CLIP, ABS (LABCOO (3, NUM)))
        GOTO 100
C
C Stick ignored; has always two spheres attached to it
C
   9    GOTO 100
C
100   CONTINUE
C
      CALL MSGSTR ('setting slab to')
      CALL MSGREL (2.0 * CLIP, 2)
      CALL MSGOUT
C
      RETURN
      END
