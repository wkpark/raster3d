C-----------------------------------
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
