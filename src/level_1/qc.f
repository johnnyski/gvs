	SUBROUTINE QC(MATQC,HTHRESH2,HTHRESH3,ZTHRESH0,
     +	ZTHRESH1,ZTHRESH2,ZTHRESH3)
C **************************************************************************
C * Perform the QC on the matrix MATQC. MATQC is a three-dimensional     
C * radar window.
C **************************************************************************
C * The algorithm was developed by Daniel Rosenfeld and David B. Wolff
C **************************************************************************
C * This subroutine written by Daniel Rosenfeld. 
C * This subroutine was cleaned up and commented by David B. Wolff
C **************************************************************************
C * Version last modified: Friday, August 30, 1996
C **************************************************************************
C *                      PERFORMING QC ON THE NEXRAD DATA
C **************************************************************************
C * Definition of variables
C *		max_qc_THRESH = Maximum height to clear data above rejected point
C *		min_qc_THRESH = MInimum max height of any window  
C *
C * 	wrays     = Close range count of rays in window
C *		wbins     = Close rangne of bins in window
C *     IWZ       = Intermediate variable
C * 	RADELEV   = Radar elevation above MSL
C *		ITOP      = Echo top height in km/10
C *		ITOP1     = Echo top height in km/10
C *     IZMAX     = Max reflectivity in the vertical
C *     IWZMAX    = Max Z in the vertical for this entire window
C * 	IZBASE    =  cloud base reflectivity in dBZ/3
C *		IZCPI(1)  = Z at the 3.0 km CAPPI
C * 	IZCPI(2)  =  Z at the 4.5 km CAPPI
C *     IKZMAXCP0 = Max Z in the 1.5 km CAPPI
C *     IKZMAXCP1 = Max Z in the 3.0 km CAPPI 
C *     IKZMAXCP2 = MAx Z in the 4.5 km CAPPI
C *     ISTOP1    = 
C *     IFAN2H    = Converts, range, elevation to height [km].
C *     MATQC     = Three-Dimensional matrix containing polar 
C *                 reflectivity data
C **************************************************************************
C **************************************************************************
	  
	  INCLUDE 'vosqc.par'
	  PARAMETER (wrays=11, wbins=7)
	  PARAMETER (maxh_qc_THRESH = 50) ! Max height for qc rejection
	  PARAMETER (minh_qc_THRESH = 25) ! Min height for plausible clouds
	  
	  INTEGER*2 MATQC(kms,rays)
	  INTEGER   IWZ(wrays,wbins)
	  INTEGER   IWZMAX(wrays,wbins)
	  INTEGER   ISTOP1(wrays,wbins)

	  IF(VERBOSE .EQ. 1) THEN
		 WRITE(6,*) "DBZNOISE  = ", DBZNOISE
		 WRITE(6,*) "HTHRESH1  = ", HTHRESH1
		 WRITE(6,*) "HTHRESH2  = ", HTHRESH2
		 WRITE(6,*) "HTHRESH3  = ", HTHRESH3
		 WRITE(6,*) "ZTHRESH0  = ", ZTHRESH0
		 WRITE(6,*) "ZTHRESH1  = ", ZTHRESH1
		 WRITE(6,*) "ZTHRESH2  = ", ZTHRESH2
		 WRITE(6,*) "ZTHRESH3  = ", ZTHRESH3
	  ENDIF
C *** Set MAXH_QC = H3, MINH_QC = H2
C
	  maxh_qc=(HTHRESH3+RADELEV)*10	! Max height for qc search
	  minh_qc=(HTHRESH2+RADELEV)*10	! Min height for plausible clouds
C     
C **** Z1, Z2 and Z3 come in this routine as floats in km
C    
	  IZT0=ZTHRESH0*10
	  IZT1=ZTHRESH1*10
	  IZT2=ZTHRESH2*10
	  IZT3=ZTHRESH3*10
C     
C *** Initialize the window to 0's
C
	  DO II=1,rays
		 DO J=1,kms
			MATQC(J,II)=0
		 ENDDO
	  ENDDO

C
C ***  Initialize first bin above radar to 1's - i.e., 'edit' the data (JCET fix for Darwin, 01/98)
C
          DO II=1,rays
                MATQC(1,II)=1
          ENDDO

C     
C *** Loop on the Z data, creating the rain maps.
C
	  JBINSTEP=3	! Number of bins in a box that is classified at once.
	  IRAYSTEP=3	! Number of rays in a box that is classified at once.
	  JBINSW=5		! Number of bins in a classification window.
	  IRAYSW=11		! Number of rays in a classification window.
	  IKM3=0

	  DO 310 JBIN=2,kms,JBINSTEP
		 IF(JBIN.GT.15)IRAYSW=9
		 IF(JBIN.GT.30)IRAYSW=7
		 IF(JBIN.GT.60)IRAYSW=5
		 IF(JBIN.GT.100)IRAYSW=3
		 IRANGE=JBIN
		 IKM3=IKM3+1
		 IRAY3=0

		 DO 320 IRAY=1,rays,IRAYSTEP
			IRAY3=IRAY3+1
C     
C *** Check if there is any reflectivity data, worth the 
C *** effort of dealing with the window
C
			JJ1=JBIN-JBINSTEP/2
			JJ2=JBIN+JBINSTEP/2
			II1=IRAY-IRAYSTEP/2
			II2=IRAY+IRAYSTEP/2
C     
C *** Here we are in a new classification window, of JBINSWxIRAYSW
C *** Build the classification window.
C
			J1=JBIN-JBINSW/2
			J2=JBIN+JBINSW/2
C 
C *** Keep range coordinates within dimension bounds
C
			IF(J1.LT.1)THEN
				J2=J2+J1+1
	      		J1=2
	      	ENDIF
	   		IF(J2.GT.kms)THEN
			   J1=J1-(J2-kms)
			   J2=kms
	      	ENDIF
			
	   		I1=IRAY-IRAYSW/2
	   		I2=IRAY+IRAYSW/2
	   		JW=0
			KTOP1W=0
			KTOP1WS=0
			KZMAXCP0=0
			KZMAXCP1=0
			KZMAXCP2=0
			
			DO 331 J=J1,J2
			   JW=JW+1
			   IW=0
			   DO 330 I=I1,I2
				  IW=IW+1
				  II=I
				  IF(II.LT.1)II=II+rays
				  IF(II.GT.rays)II=II-rays
				  IWZMAX(IW,JW)=IZMAX(J,II)
				  
				  IWZ(IW,JW)=IZBASE(J,II)
				  IF(KZMAXCP0.LT.IWZ(IW,JW))KZMAXCP0=IWZ(IW,JW)
				  
				  K=IZCPI(1,J,II)
				  IF(KZMAXCP1.LT.K)KZMAXCP1=IZCPI(1,J,II)
				  K=IZCPI(2,J,II)
				  IF(KZMAXCP2.LT.K)KZMAXCP2=IZCPI(2,J,II)
				  
				  KTOP1=ITOP1(J,II)
				  IF(KTOP1WS.LT.KTOP1)KTOP1WS=KTOP1
				  ISTOP1(IW,JW)=IFAN2H(KTOP1,J)
				  IF(KTOP1W.LT.ISTOP1(IW,JW))KTOP1W=ISTOP1(IW,JW)
				  
 330		   CONTINUE
 331		CONTINUE
C     
C *** QC the window here...
C
			 ISKIP=0
			 ITOPVOS=IFAN2H(NSTEPS,JBIN)
			 ICPI2STEP=ICPISTEP(2,JBIN)
			 
			 IF((KTOP1W.LT.maxh_qc.AND.NSTEPS.GT.KTOP1WS.OR.
     1           KZMAXCP2.LE.IZT1.AND.NSTEPS.GE.ICPI2STEP).
     2			  AND.KZMAXCP0.LT.IZT3)ISKIP=1
			 
			 IF(KTOP1W.LT.minh_qc.AND.NSTEPS.GT.KTOP1WS.
     1			  OR.KZMAXCP1.LE.IZT0)ISKIP=1
			 
			 IF(NSTEPS.LE.ICPI2STEP.AND.NSTEPS.EQ.KTOP1WS.
     1			  AND.KZMAXCP0.LT.IZT2)ISKIP=1
			 
			 DO 431 J=J1,J2
				JW=JW+1
				IW=0
				DO 430 I=I1,I2
				   IW=IW+1
C     
C *** Keep azimuth coordinates within dimension bounds
C
	  					II=I
	  					IF(II.LT.1)II=II+rays
	  					IF(II.GT.rays)II=II-rays
	  					MATQC(J,II)=ISKIP
 430				 CONTINUE
 431			  CONTINUE
 320		   CONTINUE
 310	CONTINUE

        RETURN
        END
