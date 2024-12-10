	SUBROUTINE SETVOS(NSTEPS1,TILTS,IDRADAR)
C   This subroutine initializes the look up tables for VOS calculations
C   IADRNGDLY is the minimum range of data saving
C   NSTEPS is the number of elevation steps
C
C ********************* FOR/CHECK=NOOVERFLOW **********************************

	INCLUDE 'voswin.par'
C
	DIMENSION HB(kms),HF(kms),HF1(kms),IEL12(0:steps),HP(ncpi,KMS)
	REAL TILTS(NSTEPS1)
C
	DEG2RAD=3.141592/180.

	DO I=1,kms
	DO JH=1,ncpi
	HP(JH,I)=9999
	HB(I)=9999
	HF(I)=9999
	HF1(I)=9999
	ENDDO
	ENDDO

	DO I=1,ncpi
	HCPI(I)=I*1.5
	ENDDO

	DO 20 ISTEP=1,NSTEPS
	EL=TILTS(ISTEP)		! EL is set to the elevation of the antenna
	ISTEP2EL(ISTEP)=EL*10
c	write(6,*)'istep,iel=',istep,ISTEP2EL(ISTEP)
	EL=EL*0.0174533		! convert to radians
	SINEL=SIN(EL)
	COSEL=COS(EL)
	RANGE=RNGDLY-BINLENG
	DO 10 NBIN=1,kms		! loop on all the original bins
c	RANGE=RANGE+BINLENG
	RANGE=NBIN
	IBIN2KM(NBIN,ISTEP)=-1
	IKM2BIN(NBIN,ISTEP)=-1
	IF(RANGE.GT.kms)GOTO 10
	IF(RANGE.LT.RNGDLY)GOTO 10
	NKM=RANGE+0.5		! range in km start count from 1
	IF(NKM.LT.1)GOTO 10
	H=(RANGE**2)*0.5/8492.+RANGE*SINEL+RADELEV	! calculate height in km
C	   earth curvature + antenna tilt + radar elevation
	IH=H*10+0.5
	IF(ISTEP.EQ.NSTEPS)THEN
	    IH=IOR(IH,1)	! odd number for highest tilt
	ELSE
	    IH=IAND(IH,4094)	! even number for in field
	    ENDIF
	IFAN2H(ISTEP,NKM)=IH
	IBIN2KM(NBIN,ISTEP)=RANGE*COSEL+0.5 ! horizontal range inside
	IF(IBIN2KM(NBIN,ISTEP).GT.kms)IBIN2KM(NBIN,ISTEP)=-1    ! the field
	IF(IBIN2KM(NBIN,ISTEP).LT.1)IBIN2KM(NBIN,ISTEP)=1    ! the field
	IKM2BIN(NKM,ISTEP)=RANGE/COSEL+0.5 ! slant bin number inside
	IF(NKM.LT.kms)IKM2BIN(NKM+1,ISTEP)=IKM2BIN(NKM,ISTEP)
	IF(IKM2BIN(NKM,ISTEP).GT.kms)IKM2BIN(NKM,ISTEP)=-1    ! the field
	IF(IKM2BIN(NKM,ISTEP).LT.1)IKM2BIN(NKM,ISTEP)=1    ! the field
	IF(ABS(CBASE-H).LT.HB(NKM))THEN
	    IBASTEP(NKM)=ISTEP
	    HB(NKM)=ABS(CBASE-H)
	    ENDIF
c	write(6,*)nkm,istep,h,h-hbbf,HF(NKM),IHFSTEP(NKM)
	IF(ABS(HBBF-H).LT.HF(NKM))THEN
	    IHFSTEP(NKM)=ISTEP
	    HF(NKM)=ABS(HBBF-H)
c	write(6,*)'update'
	    ENDIF
	IF(ABS(HBBF1-H).LT.HF1(NKM))THEN
	    IHFSTEP1(NKM)=ISTEP
	    HF1(NKM)=ABS(HBBF1-H)
c	write(6,*)'update'
	    ENDIF
	    DO 12 JH=1,ncpi
	    IF(ABS(HCPI(JH)-H).LT.HP(JH,NKM))THEN
		ICPISTEP(JH,NKM)=ISTEP
		HP(JH,NKM)=ABS(HCPI(JH)-H)
		ENDIF
 12	    CONTINUE
 10	CONTINUE
 20	CONTINUE
	DO 21 NKM=1,kms
	JH=ncpi+1
 22	JH=JH-1
	IF(JH.LT.2)GOTO 21
	IF(ICPISTEP(JH,NKM).EQ.NSTEPS.AND.ICPISTEP(JH-1,NKM).EQ.
     +	NSTEPS)ICPISTEP(JH,NKM)=NSTEPS+1
	GOTO 22
 21	CONTINUE
	DO JH=1,ncpi
	DO NKM=1,kms
	ISTEP=ICPISTEP(JH,NKM)
	IF(ISTEP.NE.0)ICPI2H(JH,NKM)=IFAN2H(ISTEP,NKM)
	ENDDO
	ENDDO

	DO 25 ISTEP=1,NSTEPS
	DO 27 IBIN=1,bins
	IF(IBIN2KM(IBIN,ISTEP).LE.0)GOTO 27
	IFIRSTB(ISTEP)=IBIN
	GOTO 25
 27	CONTINUE
 25	CONTINUE
C	WRITE(6,105)(I,IFIRSTB(I),I=1,NSTEPS)
C105	FORMAT(10(I5,I3))


C  Assign elevation to antenna step look up table
	IEL12(0)=0
	DO I=1,NSTEPS-1
	IEL12(I)=(ISTEP2EL(I)+ISTEP2EL(I+1))/2
	ENDDO
	IEL12(NSTEPS)=ISTEP2EL(NSTEPS)*2-ISTEP2EL(NSTEPS-1)+10
	IF(IEL12(NSTEPS).GT.900)IEL12(NSTEPS)=900
	DO 50 ISTEP=1,NSTEPS
	I1=IEL12(ISTEP-1)+1
	I2=IEL12(ISTEP)
	DO J=I1,I2
	IEL2STEP(J)=ISTEP
	ENDDO
 50	CONTINUE
	IEL2STEP(0)=1
	DO 75 IS=1,NSTEPS
	ANG=ISTEP2EL(IS)*0.00174533
C  IBH is the maximal range on the ray where echo information may be found
	IBH(IS)=21./SIN(ANG)
	IF(IBH(IS).GT.kms)IBH(IS)=kms
 75	CONTINUE
	DO I=1,kms
	IRH(I)=0
	ENDDO
	DO 80 ISTEP=NSTEPS,2,-1
	I1=IBH(ISTEP)
	IF(I1.GT.kms)I1=kms
	I2=IBH(ISTEP-1)
	IF(I2.GT.kms)I2=kms
	IF(I1.NE.kms)THEN
	    DO I=I1,I2
	    IRH(I)=ISTEP
	    ENDDO
	    ENDIF
 80	CONTINUE
	DO I=1,kms
	IF(IRH(I).EQ.0)IRH(I)=NSTEPS
	ENDDO
c	write(6,*)'IRH=',(IRH(I),I=1,kms)

c  Calculate weights for interpolation of the low level CAPPI

	ICBASE=CBASE*10		! Height of base level [km/10]
	DO NKM=1,kms

	RANGE=NKM
        TANEL=(CBASE-RADELEV-(RANGE**2)*0.5/8492)/RANGE   !calc angle to cbase
	HBAS2TILT(NKM)=ATAN(TANEL*DEG2RAD)
C	write(6,*)nkm,HBAS2TILT(NKM)

	IBSTEP=IBASTEP(NKM)	! Closest step to base level
	IF(IFAN2H(IBSTEP,NKM).LT.ICBASE)THEN
	  ISB1=IBSTEP
	  ISB2=IBSTEP+1
	  IF(ISB2.GT.NSTEPS)ISB2=NSTEPS
	ELSE IF(IFAN2H(IBSTEP,NKM).GT.ICBASE)THEN
	  ISB2=IBSTEP
	  ISB1=IBSTEP-1
	  IF(ISB1.LT.1)ISB1=1
	ELSE
	  ISB1=IBSTEP
	  ISB2=IBSTEP
	  ENDIF
C  limit the reflectivity for rain maps to the two lowest tilts
C  for WSR-88D radars, due to the incorrect clutter cancellation
C  at the higher tilts
	IF(IDRADAR.EQ.28)THEN
	   IF(ISB1.GT.2)ISB1=2
	   IF(ISB2.GT.2)ISB2=2
	   ENDIF
	ISBINTR(1,NKM)=ISB1
	ISBINTR(2,NKM)=ISB2
c	WRITE(6,*)NKM,' ISB=',ISB1,ISB2,'  HB=',IFAN2H(ISB1,NKM)/10.,
c     +	IFAN2H(ISB2,NKM)/10.,ISBINTR(1,NKM),ISBINTR(2,NKM)
	ENDDO

c  Calculate weights for interpolation of the high level CAPPI

	IHBBF1=HBBF1*10		! Height of CAPPI level [km/10]
	DO NKM=1,kms

	RANGE=NKM
        TANEL=(HBBF1-RADELEV-(RANGE**2)*0.5/8492)/RANGE   !calc angle to cbase
	HBBF12TILT(NKM)=ATAN(TANEL*DEG2RAD)
C	write(6,*)nkm,HBBF12TILT(NKM)

	IFSTEP1=IHFSTEP1(NKM)	! Closest step to base level
	IF(IFAN2H(IFSTEP1,NKM).LT.IHBBF1)THEN
	  ISF1=IFSTEP1
	  ISF2=IFSTEP1+1
	  IF(ISF2.GT.NSTEPS)ISF2=NSTEPS
	ELSE IF(IFAN2H(IFSTEP1,NKM).GT.IHBBF1)THEN
	  ISF2=IFSTEP1
	  ISF1=IFSTEP1-1
	  IF(ISF1.LT.1)ISF1=1
	ELSE
	  ISF1=IFSTEP1
	  ISF2=IFSTEP1
	  ENDIF
	ISFINTR(1,NKM)=ISF1
	ISFINTR(2,NKM)=ISF2
C	WRITE(6,*)NKM,' ISF=',ISF1,ISF2,'  HF=',IFAN2H(ISF1,NKM)/10.,
C     +	IFAN2H(ISF2,NKM)/10.
	ENDDO

c	do ikm=1,kms
c	write(6,'(10i5)')ikm,(IKM2BIN(ikm,ISTEP),istep=1,irh(ikm))
c	enddo

c	do istep=1,nsteps
c	write(6,*)'istep,ibh(istep)=',istep,ibh(istep)
c	enddo

	RETURN
	END
