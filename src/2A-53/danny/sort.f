      SUBROUTINE SORT(A,II,JJ)
C     SORTS VECTOR A INTO INCREASING ORDER,FROM A(II) TO A(JJ)
C     ORDERING IS BY INTEGER SUBTRACTION, THUS FLOATING POINT
C     NUMBERS MUST BE IN NORMALIZED FORM.
C     VECTORS TU(K) AND IL(K) PERMIT SORTING UP TO 2**(K+1)-1 ELEMENTS.
      DIMENSION A(JJ),IU(32),IL(32)
      INTEGER A,T,TT
      M=1
      I=II
      J=JJ
 5    IF(I.GE.J)GOTO 70
 10   K=I
      IJ=(J+I)/2
      T=A(IJ)
      IF(A(I).LE.T)GOTO 20
      A(IJ)=A(I)
      A(I)=T
      T=A(IJ)
 20   L=J
      IF(A(J).GE.T)GOTO 40
      A(IJ)=A(J)
      A(J)=T
      T=A(IJ)
      IF(A(I).LE.T)GOTO 40
      A(IJ)=A(I)
      A(I)=T
      T=A(IJ)
      GOTO 40
 30   A(L)=A(K)
      A(K)=TT
 40   L=L-1
      IF(A(L).GT.T)GOTO 40
      TT=A(L)
 50   K=K+1
      IF(A(K).LT.T)GOTO 50
      IF(K.LE.L)GOTO 30
      IF(L-I.LE.J-K)GOTO 60
      IL(M)=I
      IU(M)=L
      I=K
      M=M+1
      GOTO 80
 60   IL(M)=K
      IU(M)=J
      J=L
      M=M+1
      GOTO 80
 70   M=M-1
      IF(M.EQ.0)RETURN
      I=IL(M)
      J=IU(M)
 80   IF(J-I.GE.11)GOTO 10
      IF(I.EQ.II)GOTO 5
      I=I-1
 90   I=I+1
      IF(I.EQ.J)GOTO 70
      T=A(I+1)
      IF(A(I).LE.T)GOTO 90
      K=I
 100  A(K+1)=A(K)
      K=K-1
      IF(T.LT.A(K))GOTO 100
      A(K+1)=T
      GOTO 90
C**  THIS PROGRAM VALID ON FTN4 AND FTN5 **
      END

