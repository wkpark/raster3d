	FUNCTION CCuij(UU,VV)
C       ===============================================================
C	Correlation coefficient between two (3x3) anisotropic 
C	displacement matrices Uij and Vij.
C	See Merritt (1999) Acta Crystallographica D55, 1997-2004.
C	Return 0 if any of the tensors involved are NPD
C       ===============================================================
	REAL CCuij V3VINV
	REAL UU(3,3), VV(3,3), WW(3,3), UI(3,3), VI(3,3), WI(3,3)
	REAL DUI, DVI, DWI
	DUI = 1. / V3INV(UI,UU)
	DVI = 1. / V3INV(VI,VV)
C	Sylvester's criterion for NPD tensor
	IF (DUI.LE.0 .OR. UU(1,1).LE.0 
     &   .OR. (UU(1,1)*UU(2,2)-UU(1,2)*UU(2,1)).LE.0) THEN
		CCuij = 0
		RETURN
	ENDIF
	IF (DVI.LE.0 .OR. VV(1,1).LE.0 
     &   .OR. (VV(1,1)*VV(2,2)-VV(1,2)*VV(2,1)).LE.0) THEN
		CCuij = 0
		RETURN
	ENDIF
	CALL M3ADD(WI,UI,VI)
	DWI = V3INV(WW,WI)
	IF (DWI.LE.0 .OR. WW(1,1).LE.0
     &   .OR. (WW(1,1)*WW(2,2)-WW(1,2)*WW(2,1)).LE.0) THEN
		CCuij = 0
		RETURN
	ENDIF
	CCuij 	= sqrt(sqrt(DUI*DVI))
     &		/ sqrt(0.125*DWI)
	RETURN
	END


	FUNCTION CCuv(U,V)
C       ===============================================================
C	Wrapper for CCuij()
C       ===============================================================
	REAL CCuv, CCuij
	REAL U(6), V(6)
	REAL UU(3,3), VV(3,3)
	UU(1,1) = U(1)
	UU(2,2) = U(2)
	UU(3,3) = U(3)
	UU(1,2) = U(4)
	UU(2,1) = U(4)
	UU(1,3) = U(5)
	UU(3,1) = U(5)
	UU(2,3) = U(6)
	UU(3,2) = U(6)
	VV(1,1) = V(1)
	VV(2,2) = V(2)
	VV(3,3) = V(3)
	VV(1,2) = V(4)
	VV(2,1) = V(4)
	VV(1,3) = V(5)
	VV(3,1) = V(5)
	VV(2,3) = V(6)
	VV(3,2) = V(6)
	CCuv = CCuij(UU,VV)
	RETURN
	END

	FUNCTION Suv(U,V)
C       ===============================================================
C	Normalized correlation coefficient ("similarity") between two
C	anisotropic displacement vectors Uij and Vij.
C	See Merritt (1999) Acta Crystallographica D55, 1997-2004.
C       ===============================================================
	REAL Suv, CCuij
	REAL U(6), V(6)
	REAL UU(3,3), VV(3,3), Uiso(3,3), Viso(3,3), WW(3,3)
	REAL Ueq, Veq
	UU(1,1) = U(1)
	UU(2,2) = U(2)
	UU(3,3) = U(3)
	UU(1,2) = U(4)
	UU(2,1) = U(4)
	UU(1,3) = U(5)
	UU(3,1) = U(5)
	UU(2,3) = U(6)
	UU(3,2) = U(6)
	VV(1,1) = V(1)
	VV(2,2) = V(2)
	VV(3,3) = V(3)
	VV(1,2) = V(4)
	VV(2,1) = V(4)
	VV(1,3) = V(5)
	VV(3,1) = V(5)
	VV(2,3) = V(6)
	VV(3,2) = V(6)
	Ueq = (UU(1,1) + UU(2,2) + UU(3,3)) / 3.
	Veq = (VV(1,1) + VV(2,2) + VV(3,3)) / 3.
	DO I = 1,3
	DO J = 1,3
		WW(I,J) = VV(I,J) * Ueq / Veq
		Uiso(I,J) = 0
		Viso(I,J) = 0
	ENDDO
	ENDDO
	DO I = 1,3
		Uiso(I,I) = Ueq
		Viso(I,I) = Veq
	ENDDO
	Suv_top = CCuij(UU,WW)
	Suv_bot = (CCuij(UU,Uiso) * CCuij(VV,Viso))
	if (Suv_top.eq.0 .or. Suv_bot.eq.0) then
		Suv = 0
	else
		Suv = Suv_top / Suv_bot
	endif
	RETURN
	END

C	=======================================================
C	The rest of the file is just generic 3x3 matrix algebra
C	=======================================================

	SUBROUTINE V3CROSS(B,C,A)
C	CROSS PRODUCT OF TWO VECTORS
C	=======================
	REAL A(3),B(3),C(3)
	A(1)=B(2)*C(3)-C(2)*B(3)
	A(2)=B(3)*C(1)-C(3)*B(1)
	A(3)=B(1)*C(2)-C(1)*B(2)
	RETURN
	END
C
	FUNCTION V3DOT(A,B)
C	DOT PRODUCT OF TWO VECTORS
C	================
	REAL A(3),B(3)
	V3DOT=A(1)*B(1)+A(2)*B(2)+A(3)*B(3)
	RETURN
	END
C
	SUBROUTINE M3ADD(A,B,C)
C	A=B+C
C	========================
	REAL A(3,3),B(3,3),C(3,3)
	DO I=1,3
	DO J=1,3
	  A(I,J) = B(I,J) + C(I,J)
	ENDDO
	ENDDO
	RETURN
	END
C
	SUBROUTINE M3MUL(A,B,C)
C	A=B*C
C	========================
	REAL A(3,3),B(3,3),C(3,3),S
	DO I=1,3
	DO J=1,3
	  S=0
	  DO K=1,3
	    S=S+B(I,K)*C(K,J)
	  ENDDO
	  A(I,J)=S
	ENDDO
	ENDDO
	RETURN
	END
C
	FUNCTION V3INV(A,B)
C	INVERT A GENERAL 3X3 MATRIX AND RETURN DETERMINANT
C	A=(B)-1
C	======================
	REAL A(3,3),B(3,3),C(3,3),D
	CALL V3CROSS(B(1,2),B(1,3),C(1,1))
	CALL V3CROSS(B(1,3),B(1,1),C(1,2))
	CALL V3CROSS(B(1,1),B(1,2),C(1,3))
	D=V3DOT(B(1,1),C(1,1))
	IF (ABS(D).LE.1.E-30) THEN
	    V3INV=0.0
	    RETURN
	ENDIF
	DO I=1,3
	DO J=1,3
	    A(I,J)=C(J,I)/D
	ENDDO
	ENDDO
	V3INV=D
	RETURN
	END
C
