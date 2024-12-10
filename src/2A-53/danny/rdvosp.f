	SUBROUTINE RDVOSP(MATPZ,MATPZF,MATPH,MATPHZM,MATPH30,MATPZHZM)

	INCLUDE 'voswin.par'

	INTEGER*2 MATPZ(0:rays,kms),MATPZHZM(0:rays,kms),
     +	MATPH(0:rays,kms),MATPHZM(0:rays,kms),MATPH30(0:rays,kms),
     +	MATPZF(0:rays,kms)

	DO 50 IRAY=1,rays
	DO 40 IR=1,kms
C  Interpolate MATPZ from the two closest steps
	IF(ISBINTR(1,IR).EQ.ISBINTR(2,IR))THEN
	  MATPZ(IRAY,IR)=Z(IR,IRAY,ISBINTR(2,IR))    ! refl. at cloud base level CAPPI
	ELSE
	  ISB1=ISBINTR(1,IR)
	  ISB2=ISBINTR(2,IR)
	  IZZ1=Z(IR,IRAY,ISB1)
	  IZZ3=Z(IR,IRAY,ISB2)
	  IF(IZZ1.NE.0.OR.IZZ3.NE.0)THEN
C	    H1=IFAN2H(ISB1,IR)
C	    H2=CBASE*10
C	    H3=IFAN2H(ISB2,IR)
	TILT1=ISTEP2EL(ISB1)/10.
	TILT2=HBAS2TILT(IR)
	TILT3=ISTEP2EL(ISB2)/10.
	    Z1=DBZ102Z(IZZ1)
	    Z3=DBZ102Z(IZZ3)
C	    CALL INTERPOL(H1,Z1,H2,Z2,H3,Z3)
	    CALL INTERPOLB(TILT1,Z1,TILT2,Z2,TILT3,Z3,DELTAZ)
	    MATPZ(IRAY,IR)=100*ALOG10(Z2)
	    IF(MATPZ(IRAY,IR).LT.NOISDBZ)MATPZ(IRAY,IR)=0
	  ELSE
	    MATPZ(IRAY,IR)=0
	    ENDIF
	  ENDIF


C  Interpolate MATPZF from the two closest steps
	IF(ISFINTR(1,IR).EQ.ISFINTR(2,IR))THEN
	  ISF1=ISFINTR(1,IR)
	  MATPZF(IRAY,IR)=Z(IR,IRAY,ISF1)    ! refl. at above freezing level CAPPI
	ELSE
	  ISF1=ISFINTR(1,IR)
	  ISF2=ISFINTR(2,IR)
	  IZZ1=Z(IR,IRAY,ISF1)
	  IZZ3=Z(IR,IRAY,ISF2)
	  IF(IZZ1.NE.0.OR.IZZ3.NE.0)THEN
C	    H1=IFAN2H(ISF1,IR)
C	    H2=HBBF1*10
C	    H3=IFAN2H(ISF2,IR)
	TILT1=ISTEP2EL(ISF1)/10.
	TILT2=HBBF12TILT(IR)
	TILT3=ISTEP2EL(ISF2)/10.
	    Z1=DBZ102Z(IZZ1)
	    Z3=DBZ102Z(IZZ3)
C	    CALL INTERPOL(H1,Z1,H2,Z2,H3,Z3)
	CALL INTERPOLB(TILT1,Z1,TILT2,Z2,TILT3,Z3,DELTAZ)
	    MATPZF(IRAY,IR)=100*ALOG10(Z2)
	    IF(MATPZF(IRAY,IR).LT.NOISDBZ)MATPZF(IRAY,IR)=0
	  ELSE
	    MATPZF(IRAY,IR)=0
	    ENDIF
	  ENDIF


	MATPZHZM(IRAY,IR)=IZMAX(IR,IRAY)  ! max refl. at the vertical

	IF(ITOP(IR,IRAY).EQ.0)THEN
	   MATPH(IRAY,IR)=0
	   MATPHZM(IRAY,IR)=0
	   MATPH30(IRAY,IR)=0
	ELSE
	   MATPH(IRAY,IR)=IFAN2H(ITOP(IR,IRAY),IR)  ! echo top height
	   MATPHZM(IRAY,IR)=IFAN2H(IHZMAX(IR,IRAY),IR)  ! Height of max reflectivity
c	   IF(MATPHZM(IRAY,IR).EQ.MATPH(IRAY,IR))MATPHZM(IRAY,IR)=0
c  units of 0.1 km
	   IF(ITOP30(IR,IRAY).EQ.0)THEN
	      MATPH30(IRAY,IR)=0
	   ELSE
	      MATPH30(IRAY,IR)=IFAN2H(ITOP30(IR,IRAY),IR)  ! echo top height
	      ENDIF 

	   ENDIF
 40	CONTINUE
 50	CONTINUE

	RETURN
	END
