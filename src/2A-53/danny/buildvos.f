	SUBROUTINE BUILDVOS
C
c  last update 12 April 1994, updating the permanent echo cleaning
c  Added the flag IZCLEAN.

	INCLUDE 'voswin.par'

	INTEGER*2 ICOLUMN(steps+1)
	INTEGER*2 IFAN1(steps,kms),IFAN2(steps,kms),IFAN3(steps,kms)
C
	DO J=1,NSTEPS
	DO I=1,kms
	IFAN1(J,I)=0
	IFAN2(J,I)=0
	IFAN3(J,I)=0
	ENDDO
	ENDDO
	DO I=1,NSTEPS+1
	ICOLUMN(I)=0
	ENDDO

	    DO J=1,rays
	    DO I=1,kms
	    IHBASE(I,J)=0
	    IZMAX(I,J)=-999
	    ENDDO
	    ENDDO

	IRANGE1=RNGDLY
	IF(IRANGE1.EQ.0)IRANGE1=1
C Initialize the IFAN2 and IFAN3
	DO 116 ISTEP=1,nsteps 
	IRAY3=rays
	IRM=IBH(ISTEP)
	DO IR=IRANGE1,IRM
	IRGROUND=IKM2BIN(IR,ISTEP)
	IFAN2(ISTEP,IRGROUND)=Z(IR,IRAY3,ISTEP)
	ENDDO
	IRAY3=1
	DO IR=IRANGE1,IRM
	IRGROUND=IKM2BIN(IR,ISTEP)
	IFAN3(ISTEP,IRGROUND)=Z(IR,IRAY3,ISTEP)
	ENDDO
 116	CONTINUE

	DO 40 IRAY=1,rays	! loop on all the rays
	DO 45 ISTEP=1,nsteps
	IRM=IBH(ISTEP)
	iclean=1
	if(iclean.eq.1)then
	   DO IR=IRANGE1,IRM
	   IFAN1(ISTEP,IR)=IFAN2(ISTEP,IR)
	   IFAN2(ISTEP,IR)=IFAN3(ISTEP,IR)
	   ENDDO
C  Copy the next ray into FAN3
	   IRAY3=IRAY+1
	   IF(IRAY3.EQ.rays+1)IRAY3=1
	   DO 130 IR=IRANGE1,IRM
	   IRGROUND=IKM2BIN(IR,ISTEP)
	   IFAN3(ISTEP,IRGROUND)=Z(IR,IRAY3,ISTEP)
 130	   CONTINUE
	else
	   DO 131 IR=IRANGE1,IRM
	   IRGROUND=IKM2BIN(IR,ISTEP)
	   IFAN2(ISTEP,IRGROUND)=Z(IR,IRAY3,ISTEP)
 131	   CONTINUE
	   endif
 45	CONTINUE
C
C  Clean noise
	    DO 140 ISTEP=1,nsteps
	    IRM=IBH(ISTEP)
	    IRM1=IRM-1
	    DO 150 IR=IRANGE1+1,IRM1
	    IF(IFAN2(ISTEP,IR).EQ.0)GOTO 150
	if(iclean.eq.1)then
C  Here we have data. See if it is completely isolated, thus noise.
C                       lateral check
	    IF(IFAN2(ISTEP,IR-1).NE.0.OR.IFAN2(ISTEP,IR+1).
     +	    NE.0)GOTO 150
	    IF(IFAN1(ISTEP,IR).NE.0.OR.IFAN3(ISTEP,IR).NE.0)
     +	    GOTO 150
C			vertical check
	    IF(NSTEPS.GT.1)THEN
	       IF(ISTEP.GT.1.AND.IFAN2(ISTEP-1,IR).NE.0)GOTO 150
	       IF(ISTEP.LT.nsteps.AND.IFAN2(ISTEP+1,IR).NE.0)GOTO 150
C  Here we have 3 D isolated data point. Purge it.
	       IFAN2(ISTEP,IR)=0
	       ENDIF
	    ENDIF
 150	    CONTINUE
 140	    CONTINUE

	DO 50 IKM=IRANGE1,kms  ! loop on all the ranges

	ISM=IRH(IKM)
	IF(NSTEPS.LT.ISM)ISM=NSTEPS
	DO 59 ISTEP=1,ISM       ! loop on the elevation steps
	ICOLUMN(ISTEP)=IFAN2(ISTEP,IKM)    !  build column of reflectivity
 59	CONTINUE

	   JTOPZ=ISM    ! search top of corrected reflectivity
	   DO ISTEP=ISM,1,-1
	   IF(ICOLUMN(ISTEP).NE.0)THEN
	      JTOPZ=ISTEP
	      GOTO 58
	      ENDIF
	   ENDDO
	   JTOPZ=0

 58	JTOP=0
	JTOP30=0

	DO 60 ISTEP=1,JTOPZ       ! loop on the elevation steps
	IF(ICOLUMN(ISTEP).NE.0)THEN
	   JTOP=ISTEP  ! echo top step number
	   IF(ICOLUMN(ISTEP).GT.300)JTOP30=ISTEP
	   IF(IHBASE(IKM,IRAY).EQ.0)IHBASE(IKM,IRAY)=ISTEP
	   IF(ICOLUMN(ISTEP).GE.IZMAX(IKM,IRAY).AND.
     +	   IZMAX(IKM,IRAY).NE.0)THEN
	      IZMAX(IKM,IRAY)=ICOLUMN(ISTEP)
	      IHZMAX(IKM,IRAY)=ISTEP
	      ENDIF
	ELSE
	   ICOLUMN(ISTEP)=0
	   ENDIF
 60	CONTINUE
	IF(IZMAX(IKM,IRAY).EQ.-999)IZMAX(IKM,IRAY)=0

C		    IF(JTOP.EQ.0)GOTO 50
		    IZGR(IKM,IRAY)=ICOLUMN(1)   ! ground level reflectivity
		    IZBASE(IKM,IRAY)=ICOLUMN(IBASTEP(IKM))	! cloud base level dbz
c		    DO JH=1,ncpi
c		    IZCPI(JH,IKM,IRAY)=ICOLUMN(ICPISTEP(JH,IKM))
c		    ENDDO

C  Look for first top
			ISTBAS=IBASTEP(IKM)
			JTOP1=0
			JBASE2=0
			DO 70 ISTEP=IHBASE(IKM,IRAY),JTOP
			if(IHBASE(IKM,IRAY).eq.0)goto 73
			IF(ICOLUMN(ISTEP).EQ.0)THEN
			    IF(ISTEP.LE.ISTBAS)GOTO 70
			    JTOP1=ISTEP-1
			    GOTO 73
			    ENDIF
 70			CONTINUE
 73			IF(JTOP1.EQ.0)THEN	! No upper dec
			    JTOP1=JTOP
			    GOTO 80
			ELSE
C   Look for second base
			    JTOP11=JTOP1+1
			    DO 75 ISTEP=JTOP11,JTOP
			    IF(ICOLUMN(ISTEP).EQ.0)GOTO 75
			    JBASE2=ISTEP
			    GOTO 80
 75			    CONTINUE
			    ENDIF
 80			IF(JTOP1.EQ.0)JTOP1=JTOP
			ITOP1(IKM,IRAY)=JTOP1
			ITOP(IKM,IRAY)=JTOP
			ITOP30(IKM,IRAY)=JTOP30
			IF(JBASE2.EQ.0)JBASE2=IHBASE(IKM,IRAY)
			IHBASE2(IKM,IRAY)=JBASE2
C	if(iray.ge.285.and.iray.le.295.and.ikm.ge.85.
C     +	and.ikm.le.100)then
C	   write(6,*)'ikm,iray,itop=',ikm,iray,JTOP,JTOP1,JTOP30,
C     +	IZMAX(IKM,IRAY),IHZMAX(IKM,IRAY),IHBASE(IKM,IRAY)
C	   write(6,*)(icolumn(i),i=1,nsteps)
C	   endif

 50		    CONTINUE
 40		CONTINUE

	RETURN
	END
