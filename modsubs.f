      SUBROUTINE CROSS(B,C,A)
C     =======================
C
C
      DIMENSION A(3),B(3),C(3)
      A(1)=B(2)*C(3)-C(2)*B(3)
      A(2)=B(3)*C(1)-C(3)*B(1)
      A(3)=B(1)*C(2)-C(1)*B(2)
      RETURN
      END
C
      FUNCTION DOT(A,B)
C     ================
C
C     DOT PRODUCT OF TWO VECTORS
C
      DIMENSION A(3),B(3)
      DOT=A(1)*B(1)+A(2)*B(2)+A(3)*B(3)
      RETURN
      END
	SUBROUTINE MATMUL(A,B,C)
C	========================
C
C	MULTIPLY 2 3X3 MATRICES
C
C	A=BC
C
	REAL A(3,3),B(3,3),C(3,3)
	DO 1 I=1,3
	DO 2 J=1,3
	S=0
	DO 3 K=1,3
3	S=S+B(I,K)*C(K,J)
2	A(I,J)=S
1	CONTINUE
	RETURN
	END
	SUBROUTINE MATVEC(V,A,B)
C	========================
C
C	POST-MULTIPLY A 3X3 MATRIX BY A VECTOR
C
C	V=AB
C
	REAL A(3,3),B(3),V(3)
	DO 1 I=1,3
	S=0
	DO 2 J=1,3
2	S=S+A(I,J)*B(J)
1	V(I)=S
	RETURN
	END
	SUBROUTINE MINV(A,B,D)
C	======================
C
C	INVERT A GENERAL 3X3 MATRIX AND RETURN DETERMINANT IN D
C
C	A=(B)-1
C
	REAL A(3,3),B(3,3),C(3,3)
	CALL CROSS(B(1,2),B(1,3),C(1,1))
	CALL CROSS(B(1,3),B(1,1),C(1,2))
	CALL CROSS(B(1,1),B(1,2),C(1,3))
	D=DOT(B(1,1),C(1,1))
C
C	TEST DETERMINANT
C
	IF(ABS(D).GT.1.E-30)GO TO 10
	D=0.0
	RETURN
C
C	DETERMINANT IS NON-ZERO
C
10	DO 20 I=1,3
	DO 20 J=1,3
20	A(I,J)=C(J,I)/D
	RETURN
	END
C
      SUBROUTINE SCALEV(A,X,B)
C     ========================
C
C     SCALE A VECTOR B WITH SCALAR X AND PUT RESULT IN A
C
      DIMENSION A(3),B(3)
      DO 10 I=1,3
      A(I)=B(I)*X
   10 CONTINUE
      RETURN
      END
	SUBROUTINE TRANSP(A,B)
C	======================
C
C	TRANSPOSE A 3X3 MATRIX
C
C	A=BT
C
	REAL A(3,3),B(3,3)
	DO 1 I=1,3
	DO 1 J=1,3
1	A(I,J)=B(J,I)
	RETURN
	END
      SUBROUTINE UNIT(V)
C     =================
C
C
      REAL V(3)
C****VECTOR V REDUCED TO UNIT VECTOR
      VMOD=V(1)**2+V(2)**2+V(3)**2
      VMOD=SQRT(VMOD)
      DO 1 I=1,3
    1 V(I)=V(I)/VMOD
      RETURN
      END
      SUBROUTINE VDIF(A,B,C)
C     =====================
C
C
C     SUBTRACT TWO VECTORS
C
      DIMENSION A(3),B(3),C(3)
      DO 10 I=1,3
      A(I)=B(I)-C(I)
   10 CONTINUE
      RETURN
      END
      SUBROUTINE VSET(A,B)
C     ====================
C
C
C     MOVE A VECTOR FROM B TO A
C
      DIMENSION A(3),B(3)
      DO 10 I=1,3
   10 A(I)=B(I)
      RETURN
      END
      SUBROUTINE VSUM(A,B,C)
C     ======================
C
C
C     ADD TWO VECTORS
C
      DIMENSION A(3),B(3),C(3)
      DO 10 I=1,3
      A(I)=B(I)+C(I)
   10 CONTINUE
      RETURN
      END
