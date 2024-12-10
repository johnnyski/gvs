	SUBROUTINE GETZH(JBINSW,IRAYSW,ICRANGE,ICAZ,AL,BL,RL,AH,BH,RH)

	INCLUDE 'voswin.par'

	DIMENSION HW(steps),IZHW(steps)
	DIMENSION HL(steps),ZL(steps),HH(steps),ZH(steps)

	AL=0
	BL=0
	RL=0
	AH=0
	BH=0
	RH=0

	DO ISTEP=1,steps
	HW(ISTEP)=IFAN2H(ISTEP,ICRANGE)/10.
	IZHW(ISTEP)=0
	ENDDO

	DO IRAY=1,IRAYSW
	  IAZ=ICAZ+IRAY-IRAYSW/2
	  IF(IAZ.LT.1)IAZ=IAZ+360
	  DO IBIN=1,JBINSW
	    IR=ICRANGE+IBIN-JBINSW/2
	    ISM=IRH(IR)
	    IF(NSTEPS.LT.ISM)ISM=NSTEPS
	    DO ISTEP=1,ISM       ! loop on the elevation steps
	      IGBIN=IKM2BIN(IR,ISTEP)
	      IZ=Z(IGBIN,IAZ,ISTEP)    !  build column of reflectivity
C  Build the Z-H statistics
	      IF(IZHW(ISTEP).LT.IZ)IZHW(ISTEP)=IZ
	    ENDDO
	  ENDDO
	ENDDO

	NL=0
	NH=0
	NZH=0
	DO I=1,steps
	IF(IZHW(I).GT.0)NZH=I
	ENDDO
	IF(NZH.LT.NSTEPS)NZH=NZH+1
	do i=1,nzh
c	write(6,*)'STEP,H,Z=',I,HW(I),IZHW(I)/10.
	enddo
	DO I=1,NZH
	   IF(HW(I).LT.HBBF+1.5)THEN
	   NL=NL+1
	   ZL(NL)=IZHW(I)/10.
	   HL(NL)=HW(I)
	ELSE
	   NH=NH+1
	   ZH(NH)=IZHW(I)/10.
	   HH(NH)=HW(I)
	   ENDIF
	ENDDO
c	IF(NL.GE.3)THEN
	   CALL LISTQ(HL,ZL,NL,AL,BL)
	   CALL CORREL(HL,ZL,NL,RL)
C	write(6,*)'L: N,A,B,R=',NL,AL,BL,RL
	   CALL LISTQ(HH,ZH,NH,AH,BH)
	   CALL CORREL(HH,ZH,NH,RH)
C	write(6,*)'H: N,A,B,R=',NH,AH,BH,RH
c	   ENDIF

	RETURN
	END

      SUBROUTINE CORREL(X,Y,N,R)
      PARAMETER (TINY=1.E-20)
      DIMENSION X(N),Y(N)
	IF(N.LT.2)THEN
	   R=0
	   RETURN
	   ENDIF
      AX=0.
      AY=0.
      DO 11 J=1,N
        AX=AX+X(J)
        AY=AY+Y(J)
11    CONTINUE
      AX=AX/N
      AY=AY/N
      SXX=0.
      SYY=0.
      SXY=0.
      DO 12 J=1,N
        XT=X(J)-AX
        YT=Y(J)-AY
        SXX=SXX+XT**2
        SYY=SYY+YT**2
        SXY=SXY+XT*YT
12    CONTINUE
	SXXYY=SXX*SYY
	IF(SXXYY.LT.0.0000001)THEN
	   R=0
	ELSE
           R=SXY/SQRT(SXX*SYY)
	   ENDIF
        RETURN
      END
	SUBROUTINE LISTQ(X,Y,N,A,B)

C Calculate least squares regression line

	DIMENSION X(N),Y(N)

	IF(N.LT.2)THEN
	   A=0
	   B=0
	   RETURN
	   ENDIF
	XSUM=0
	YSUM=0
	DO I=1,N
	XSUM=XSUM+X(I)
	YSUM=YSUM+Y(I)
	ENDDO
	XM=XSUM/N
	YM=YSUM/N

	S1=0
	S2=0
	DO I=1,N
	S1=S1+(X(I)-XM)*(Y(I)-YM)
	S2=S2+(X(I)-XM)**2
	ENDDO

	IF(S2.EQ.0)THEN
	   S2=0.00000001
	   ENDIF

	B=S1/S2
	A=YM-B*XM

	RETURN
	END

