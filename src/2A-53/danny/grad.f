	SUBROUTINE GRAD(IDBZ1,MH2,IZ,P,T,DP,NRAYS,NBINS,IDELTAZ,
     +	BINLENGTH,IHMIN1,RG,RGIN,CONDEFF,IHEIGHTMEAN,IRANGE,IZMDS,IAG,
     +	VERBOSE)
c     *****************************************************************
c     This subroutine calculates for each (NRAYS*NBINS)domain: 
c     1. The average dBZ radial & athimuthal gradient (RG).
c     2. The mean convective efficiency (CONDEFF) by subroutine Pseudoadiabat
c        and average echo tops (IHEIGHTMEAN).
c     ******************************************************************
c
c      written by Eyal Amitai,    last modification 10-Apr-94
c
c     ******************************************************************

      PARAMETER (rays=11,bins=7)
      PARAMETER (ideltah=10,iee=3,kms=300)

      INTEGER IDBZ1(rays,bins),IDBZ(rays,bins),MH2(rays,bins) 
      INTEGER IMH2(rays,bins),IZMDS(kms) 
      DIMENSION EE(1500),NTOPS(3),IHT(3),CE(3)
      COMMON/CDDZ/IFIRST,EE,IZBase
	INTEGER VERBOSE
      DATA IFIRST/0/

      IF(IFIRST.EQ.0)THEN
        CALL PSEUDOADIABAT(EE,IZBase,IZ,P,T,DP)
        IFIRST=1
	RETURN
      ENDIF


      DO 20 J=1,nrays
      DO 20 I=1,nbins
	IDBZ(J,I)=IDBZ1(J,I)
	IF(IDBZ(J,I).EQ.0)IDBZ(J,I)=-990
        IMH2(J,I)=MH2(J,I)*100
20    CONTINUE


      NZ=0
      NH=0
      DO 35 IBIN=1,NBINS
        IR=IRANGE-(NBINS/2.+0.5)+IBIN
	IF(IR.LT.1)IR=1
        DO 30 IRAY=1,NRAYS
          II=IDBZ(IRAY,IBIN)
          IF(II.GE.IZMDS(IR))NZ=NZ+1
          IF(CONDEFF.NE.10)THEN
            IIH=MH2(IRAY,IBIN)
            IF(IIH.GT.0)NH=NH+1
          ENDIF
 30     CONTINUE
 35   CONTINUE           

ccccccccccccccccccc calculating condeff ccccccccccccccccccccccccccccccccccccc

      IF(CONDEFF.NE.10)THEN
        IF(NH.GT.0 .AND. IEE.NE.0)THEN
          IHEIGHTMEAN=0
          IHMIN=IHMIN1*100    !Changing IHMIN units from [km/10] to [m]
          DO 257 I=1,3
            CE(I)=8.888
            NTOPS(I)=0
            IHT(I)=0
 257      CONTINUE
          DO 265 IBIN=1,NBINS
          IR=IRANGE-(NBINS/2.+0.5)+IBIN
          DO 255 IRAY=1,NRAYS
            IH=IMH2(IRAY,IBIN)
            IF(IH.GT.0)THEN
              I=IDBZ(IRAY,IBIN)
              IF(IEE.EQ.1 .OR. IEE.EQ.4)THEN
                IF(I.GE.IZMDS(IR))CALL HEIGHT(NTOPS(1),IHT(1),IH)
              ENDIF
              IF(IEE.EQ.2 .OR. IEE.EQ.4)CALL 
     +           HEIGHT(NTOPS(2),IHT(2),IH)
              IF(IEE.EQ.3 .OR. IEE.EQ.4)THEN
                IF(IH.GT.IHMin)CALL HEIGHT(NTOPS(3),IHT(3),IH)
              ENDIF
            ENDIF
 255      CONTINUE
 265      CONTINUE
          DO 285 L=1,3
            IF(NTOPS(L).GT.0)THEN
              IHeightMEAN=IHT(L)/NTOPS(L)
              I=(IHeightMEAN-IZBase+IDeltaH/2)/IDeltaH
              IF(I.GT.1500)I=1500            !for heights above 15 km.
	      IF(I.LT.1)I=1
              CE(L)=EE(I)
            ENDIF
 285      CONTINUE
C                                        CE(3)=CE(<Hi> ; i>IHMin)
          CONDEFF=CE(iee)                 
        ELSE                      	!if(ntops(iee).eq.0)condeff=8.888 
          CONDEFF=8.888
        ENDIF
      ELSE
        CONDEFF=9.999
      ENDIF

ccccccccccccccccccc calculating  RG   ccccccccccccccccccccccccccccccccccccccc
      RG=99.99
      IF(NZ.GT.0)THEN

C       Calculating inner RG
        RGINNER=88.88  !separate pixels only
        ITotalGRAD=0
        NTotalGRAD=0
        DO 190 IBIN=2,NBINS
        IR=IRANGE-(NBINS/2.+0.5)+IBIN
	  IF(IR.LT.2)IR=2
          DO 180 IRAY=1,NRAYS
            II=IDBZ(IRAY,IBIN)
            I=IDBZ(IRAY,IBIN-1)
            IF(II.GE.IZMDS(IR) .AND. I.GE.IZMDS(IR-1))THEN
              IGRAD1=ABS(II-I)
              ITotalGRAD=ITotalGRAD+IGRAD1
              NTotalGRAD=NTotalGRAD+1
            ENDIF
 180      CONTINUE
 190    CONTINUE           
        IF(NTotalGRAD.GT.0)RGINNER=(ITotalGRAD/10.)
     +    /(NTotalGRAD*BINLENGTH)
        RGINNERR=RGINNER               !Add on 18/8/94
        RGIN=RGINNER                   !Add on 18/8/94
        NTotalGRADR=NTotalGRAD         !Add on 18/8/94

C       Calculating final RG
        ITotalGRAD=0
        NTotalGRAD=0
        DO 195 IBIN=2,NBINS
        IR=IRANGE-(NBINS/2.+0.5)+IBIN
	IF(IR.LT.2)IR=2
          DO 185 IRAY=1,NRAYS
            II=IDBZ(IRAY,IBIN)
            I=IDBZ(IRAY,IBIN-1)
            IF(II.GE.IZMDS(IR) .OR. I.GE.IZMDS(IR-1))THEN
              IF(I.GE.IZMDS(IR-1) .AND. II.LT.IZMDS(IR))THEN
C               Calculating new value for II
                IF(RGINNER.NE.88.88)THEN
		  II=I-10*RGINNER+0.5
                  IF(II.GE.IZMDS(IR))II=IZMDS(IR)-10
		ELSE   !separate pixels only
                  II=IZMDS(IR)-10  
                ENDIF
              ENDIF
              IF(I.LT.IZMDS(IR-1) .AND. II.GE.IZMDS(IR))THEN
C               Calculating new value for I
                IF(RGINNER.NE.88.88)THEN
		  I=II-10*RGINNER+0.5
                  IF(I.GE.IZMDS(IR-1))I=IZMDS(IR-1)-10
		ELSE   !separate pixels only
                  I=IZMDS(IR-1)-10  
                ENDIF
              ENDIF
              IGRAD1=ABS(II-I)
              ITotalGRAD=ITotalGRAD+IGRAD1
              NTotalGRAD=NTotalGRAD+1
            ENDIF
 185      CONTINUE
 195    CONTINUE           
        IF(NTotalGRAD.GT.0)RG=(ITotalGRAD/10.)
     +    /(NTotalGRAD*BINLENGTH)

        IF(IAG.EQ.1)THEN
C         Calculating inner AG   !AZIMUTHAL GRADIENT
          RGINNER=88.88  !separate pixels only
          TotalGRAD=0
          NTotalGRAD=0
C         ITGPR=0      !Toatal Grad Per Ray

          DO 390 IBIN=1,NBINS
            IR=IRANGE-(NBINS/2.+0.5)+IBIN
	    IF(IR.LT.1)IR=1
            AZDISTANCE=IR*0.001745
            DO 380 IRAY=2,NRAYS
              II=IDBZ(IRAY,IBIN)
              I=IDBZ(IRAY-1,IBIN)
              IF(II.GE.IZMDS(IR) .AND. I.GE.IZMDS(IR))THEN
                GRAD1=ABS(II-I)/AZDISTANCE
                TotalGRAD=TotalGRAD+GRAD1
C               ITGPR=ITGPR+ABS(II-I)
                NTotalGRAD=NTotalGRAD+1
              ENDIF
 380        CONTINUE
 390      CONTINUE           
C         IF(NTotalGRAD.GT.0)GradAzRay=(ITGPR/10.)/NTotalGRAD)
          IF(NTotalGRAD.GT.0)RGINNER=(TotalGRAD/10.)/
     +                               (NTotalGRAD*IDELTAZ)

          RGINNERA=RGINNER               !Add on 18/8/94
          NTotalGRADA=NTotalGRAD         !Add on 18/8/94

          IF(NTotalGRADR.GE.3 .AND. NTotalGRADA.GE.3)THEN  !Add on 18/8/94
            IF(RGINNERR.GT.RGINNERA)THEN
              RGIN=RGINNERR
            ELSE
              RGIN=RGINNERA
            ENDIF
          ELSE
            IF(NTotalGRADR.GE.NTotalGRADA)THEN
              RGIN=RGINNERR
            ELSE
              RGIN=RGINNERA
            ENDIF
          ENDIF

        


C         Calculating final AG
          TotalGRAD=0
          NTotalGRAD=0
C         RGINNER=GradAzRay
C         ITGPR=0      !Toatal Grad Per Ray
          DO 395 IBIN=1,NBINS
            IR=IRANGE-(NBINS/2.+0.5)+IBIN
	    IF(IR.LT.1)IR=1
            AZDISTANCE=IR*0.001745
            DO 385 IRAY=2,NRAYS
              II=IDBZ(IRAY,IBIN)
              I=IDBZ(IRAY-1,IBIN)
              IF(II.GE.IZMDS(IR) .OR. I.GE.IZMDS(IR))THEN
                IF(I.GE.IZMDS(IR) .AND. II.LT.IZMDS(IR))THEN
C                 Calculating new value for II
                  IF(RGINNER.NE.88.88)THEN
		    II=I-10*RGINNER+.5
                    IF(II.GE.IZMDS(IR))II=IZMDS(IR)-10
		  ELSE   !separate pixels only
                    II=IZMDS(IR)-10  
                  ENDIF
                ENDIF
                IF(I.LT.IZMDS(IR) .AND. II.GE.IZMDS(IR))THEN
C                 Calculating new value for I
                  IF(RGINNER.NE.88.88)THEN
		    I=II-10*RGINNER+0.5
                    IF(I.GE.IZMDS(IR))I=IZMDS(IR)-10
		  ELSE   !separate pixels only
                    I=IZMDS(IR)-10  
                  ENDIF
                ENDIF
                GRAD1=ABS(II-I)/AZDISTANCE
                TotalGRAD=TotalGRAD+GRAD1
C               ITGPR=ITGPR+ABS(II-I)
                NTotalGRAD=NTotalGRAD+1
              ENDIF
 385        CONTINUE
 395      CONTINUE           
C         IF(NTotalGRAD.GT.0)GradAzRay=(ITGPR/10.)/NTotalGRAD)
          IF(NTotalGRAD.GT.0)AG=(TotalGRAD/10.)/
     +                          (NTotalGRAD*IDELTAZ)

C         AG=GradAzRay
          IF(AG.GT.RG)RG=AG
 
        ENDIF
      ENDIF
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      RETURN
 1001 STOP 'Cant open input directivies file'
 1003 STOP 'Cant open output data file'
      END	           		   
 

      SUBROUTINE HEIGHT(N,IHT,IH)
      N=N+1
      IHT=IHT+IH
      RETURN
      END


      SUBROUTINE PSEUDOADIABAT(EE,IZBase,IZ,P,T,DP)
c     *******************************************************************
c     This subroutine calculate the pseudoadiabats pressure, temperature,
c     mixing ratio and the effective efficiency parameter for each
c     increment height. 
C     Input data: height, pressure, temperature and dew point for any 
C     level under cloud base.
c    
c     Last modification 4/6/92 by Eyal Amitai
c     *******************************************************************
c
      PARAMETER (DRYLAP=0.0098,IR=287,ICP=1005,EPSILON=0.622)
      PARAMETER     (IDeltaH=10)
      REAL L
      DIMENSION EE(1500)
C
C      READ(1,*)IZ,P,T,DP       !D

C parameters for Darwin  ===============================================start
c	IZ=0
c	P=1010
c	T=305
c	DP=297
C parameters for Darwin  ===============================================end

C     calculate cloud base data
      CALL MIXINGRATIO (P,DP,WBase)
 10   IZ=IZ+IDeltaH
      T0=T
      T=T-DRYLAP*IDeltaH
      P=P*(T/T0)**3.5
      CALL MIXINGRATIO (P,T,W)
      IF(W.GT.WBase)GOTO 10
      IF(VERBOSE.EQ.1)WRITE(6,*)
     +	' CLOUD BASE DATA: HEIGHT  PRESSURE  TEMP.'
      IZBase=IZ
      IF(VERBOSE.EQ.1)WRITE(6,111)IZBase,P,T
 111  FORMAT(16X,I7,F10.1,F8.1)
      IF(VERBOSE.EQ.1)WRITE(6,*)

c     calculate the pseudoadiabats
C      WRITE(6,*)' HEIGHT  PRESSURE  TEMP.   MIXING RATIO   EE'  
      DO 100 I=1,1500
        L=3133541-2317*T  
        A=1+L*W/(IR*T)
        B=1+L**2*EPSILON*W/(IR*ICP*T**2)
        PLAP=DRYLAP*A/B

        W=W+IDeltaH*ICP/L*(PLAP-DRYLAP/(1+0.6*W))
        EE(I)=(WBase-W)/WBase
        T=T-PLAP*IDeltaH
        P=P*EXP(-IDeltaH*9.8/(IR*T*(1+0.6*W)))
        IZ=IZ+IDeltaH
        IF(MOD(IZ,100).EQ.0)THEN
C          WRITE(6,112)IZ,P,T,W,EE(I)
 112      FORMAT(I7,F10.1,F8.1,F10.4,6X,F5.3)
        ENDIF
 100  CONTINUE
C      write(6,*) 'returning control to Main program'
      RETURN           !D
      END		

      SUBROUTINE MIXINGRATIO (P,T,W)
      ES=6.112*EXP(17.67*(T-273)/(T-29.5))
      W=0.622*ES/(P-ES)
C      WRITE(6,*)ES,W
      RETURN
      END

