C === RIBBON ===
C	(extracted from frodo.tlb in CCP program package)
C
      SUBROUTINE RIBBON(NRIB,RIBWID,NCHORD,OFFSET,NATOM)
C     ==================================================
C
C Generate guide points for protein ribbon, based on ideas on
C Carson & Bugg, J.Molec.Graphics 4,121-122 (1986)
C
C  Guide points for Bspline are generated along a line passing
C through each CA and along the average of the two peptide planes
C
C   NRIB     number of strands in ribbon (maximum=MAXRIB=15)
C   RIBWID   total ribbon width
C   NCHORD   number of chords/residue
C   OFFSET   amount to offset guide points away from CA positions
C   NATOM    number of atoms stored in arrays
C
      PARAMETER (MAXRIB=5,MAXRES=1500)
      PARAMETER (NOISE=0)
      DIMENSION GUIDE(4,MAXRES,MAXRIB)
      DIMENSION XCA(3,2),XO(3,2),A(3),B(3),C(3),D(3),E(3),F(3),
     .   G(3),H(3),P(3)
C
C Maximum CA-CA distance **2
      PARAMETER (DISMAX=6.**2)
C
      IF(NATOM.LE.0) THEN
	    WRITE(NOISE,1005)
1005	    FORMAT(' No atoms selected')
	    RETURN
      ENDIF
C
      IF(NRIB.GT.MAXRIB) THEN
	    WRITE(NOISE,1001) NRIB,MAXRIB
1001	    FORMAT(' Too many ribbon strands',I6,' reset to ',I6)
	    NRIB=MAXRIB
      ENDIF
C
      WRITE(NOISE,1002) NRIB,RIBWID,NCHORD,OFFSET
1002  FORMAT(' Ribbon drawn with',I4,' strands, width ',F6.2,
     . 'A'/'   Number of chords =',I3,', offset = ',F6.2,'A')
C
C Strand separation
      DRIB=0.
      IF(NRIB.GT.1) DRIB=RIBWID/(NRIB-1)
      RIB2=FLOAT(NRIB+1)/2.
C
      NAT=1
C
C Get first CA and O
1     CALL GETCAO(XCA(1,1),XO(1,1),NAT,NATOM,IERR)
CEAM  IF(NAT.LE.0) RETURN
CEAM  IF(IERR.NE.0) GO TO 1
      IF(IERR.NE.0) RETURN
      I=0
C
C  Loop for residues
10    I=I+1
C  Get CA and O for residue I+1
      CALL GETCAO(XCA(1,2),XO(1,2),NAT,NATOM,IERR)
C Set LEND = 1 for end of chain under 3 conditions:
C  (a) all atoms done; (b) one fo CA or O missing; (c) break in chain
      IF(NAT.LT.0.OR.IERR.NE.0) THEN
	    LEND=1
      ELSE
	    LEND=0
      ENDIF
C
      IF(LEND.EQ.0) THEN
C Not last one unless CA-CA distance too large
C   A is vector CAi to Ci+1
	    CALL VDIF(A,XCA(1,2),XCA(1,1))
	    IF(DOT(A,A).GT.DISMAX) LEND=1
      ENDIF
      IF(LEND.EQ.0) THEN
C Not last one
C   B is vector CAi to Oi
	    CALL VDIF(B,XO(1,1),XCA(1,1))
C   C = A x B;  D = C x A
	    CALL CROSS(A,B,C)
	    CALL CROSS(C,A,D)
	    CALL UNIT(D)
C
	    IF(I.EQ.1) THEN
C  First peptide, no previous one to average with
		  CALL VSET(E,D)
C  No offset for first CA
		  CALL ZEROI(P,3)
	    ELSE
C  Not first, ribbon cross vector is average of peptide plane
C  with previous one
		  CALL SCALEV(B,SIGN(1.,DOT(D,G)),D)
		  CALL VSUM(E,G,B)
C  Offset is along bisector of CA-CA-CA vectors A (H is Ai-1)
		  CALL VDIF(P,H,A)
		  CALL UNIT(P)
	    ENDIF
      ELSE
C  Last one, just use last plane
	    CALL VSET(E,G)
C  No offset for last CA
	    CALL ZEROI(P,3)
      ENDIF
C Normalise vector E
      CALL UNIT(E)
C      WRITE(NOISE,1003) I,G,D,B,E
C1003  FORMAT(' I,G,D,B,E',I4,4(3X,3F8.2)/)
C
C
C Generate guide points
      CALL SCALEV(P,OFFSET,P)
      CALL VSUM(P,XCA(1,1),P)
C
      DO 20,J=1,NRIB
      FR=(FLOAT(J)-RIB2)*DRIB
      CALL SCALEV(F,FR,E)
      CALL VSUM(GUIDE(1,I,J),P,F)
C     EAM - Maybe should be NAT-2 ??
      guide(4,i,j) = NAT - 3
20    CONTINUE
C
C Store things for next residue
      CALL VSET(XCA(1,1),XCA(1,2))
      CALL VSET(XO(1,1),XO(1,2))
      CALL VSET(G,E)
      CALL VSET(H,A)
C
      IF(LEND.EQ.0) GO TO 10
C
      NPT=I
      CALL RIBDRW(GUIDE,NRIB,MAXRES,NPT,NCHORD)
C
C Loop chains if required
CEAM  IF(NAT.GT.0) GO TO 1
      IF (IERR.EQ.0) GOTO 1
C
      RETURN
      END
C
C
      SUBROUTINE pdb_GETCAO(XCA,XO,NAT,NATOM,IERR)
C     ========================================
C
C Get coordinates of CA in XCA, O in XO, 
C Modified to read sequential CA and O records in PDB format from file
C
C  On exit: NAT next atom 
C           IERR  =0 if succesfull, else = 1
C
      DIMENSION XCA(3),XO(3)
C
	integer		PDBFILE
	parameter	(PDBFILE = 1)
	character*1	a1, rescode(2)
	character*3	resname(2)
	character*4	reclabel, atname
	integer		resno(2)
C
	ierr=0

	read (pdbfile,2,end=100) reclabel, nat, atname, a1, resname(1),
     1			 a1, resno(1), rescode(1), xca(1), xca(2), xca(3)
	read (pdbfile,2,end=100) reclabel, nat, atname, a1, resname(2),
     1			 a1, resno(2), rescode(2), xo(1), xo(2), xo(3)
    2	format(a4,2x,i5,1x,a4,a1,a3,1x,a1,i4,a1,3x,5f8.3,2f6.2,1x,i3)

	if (resname(1) .ne. resname(2)) ierr = 1
	if (resno(1)   .ne. resno(2))   ierr = 1
	if (rescode(1) .ne. rescode(2)) ierr = 1
	return

  100	continue
	ierr = 1
	nat = -1
	return

	end



      SUBROUTINE GETCAO(XCA,XO,NAT,NATOM,IERR)
C     ========================================
C
C Get coordinates of CA in XCA, O in XO, 
C modified to get coords from common /SPAM/
C
C  On exit: NAT next atom 
C           IERR  =0 if succesfull, else = 1
C
      DIMENSION XCA(3),XO(3)
C
	parameter	(MAXATOM=10000)
	common /SPAM/ natm, SPAM(4,MAXATOM), SCAM(MAXATOM)
	integer SCAM
c
	if ((nat .gt. natm) .or. (nat .gt. natom-1)) then
	     ierr = 1
CEAM	     nat = -1
	     return
	end if

	do i=1,3
	    xca(i) = spam(i,nat)
	    xo(i)  = spam(i,nat+1)
	end do
	nat  = nat + 2
	ierr = 0
	return

	end

	subroutine zeroi( a, nwords )
	integer*4 a(nwords)
	do i = 1,nwords
	    a(i) = 0
	end do
	return
	end
