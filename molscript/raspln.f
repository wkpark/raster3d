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
C NPASS	 (In) draw ribbon surface in 1st pass, edges in 2nd pass,
C             R3DOPT>0 will cause a 3rd pass to draw the back surface
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
C Ethan A Merritt - August 1996
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
C -  Setting R3DOPT = 1 will cause this routine to draw both
C    helix surfaces, so you can view it later from any angle.
C    This requires a third pass through the code in the HELIX
C    routine of schematic.f
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
      INTEGER    R3DOPT
      PARAMETER (R3DOPT = 0)
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
      IF (NPASS .EQ. 2 .AND. HELTHK .GT. 0) THEN
        CALL GRLINE (PLANE (1,1), PLANE (1,2))
        CALL GRLINE (PLANE (1,3), PLANE (1,4))
        RETURN
      END IF
C
C If R3DOPT = 1, draw front surface of the helix during the first pass.
C Otherwise if either the new or the old segment ends on the front side 
C of the helix, draw another front side segment.
C
      IF (NPASS .EQ. 1) THEN
      IF (    (R3DOPT.GT.0 )
     &   .OR. (ONORM(3).GE.0 .OR. PNORM(3).GE.0)  ) THEN
        CALL V3SCAL (OFFSET, RECESS * HELTHK, ONORM)
        CALL V3ADD  (SURF (1,1), OFFSET, PLANE (1,1) )
        CALL V3ADD  (SURF (1,4), OFFSET, PLANE (1,4) )
        CALL V3SCAL (OFFSET, RECESS * HELTHK, PNORM)
        CALL V3ADD  (SURF (1,2), OFFSET, PLANE (1,2) )
        CALL V3ADD  (SURF (1,3), OFFSET, PLANE (1,3) )
        CALL GRPLAN (SURF, .FALSE., PL1234, .TRUE., ZDIR)
      END IF
      END IF
C
C If R3DOPT = 1, draw back surface of the helix during the third pass.
C Otherwise, if either the new or the old segment ends on the back side 
C of the helix,  draw another back side segment.
C
      IF (    (R3DOPT.GT.0 .AND. NPASS.EQ.3)
     &   .OR. (ONORM(3).LE.0 .OR. PNORM(3).LE.0)  ) THEN
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
