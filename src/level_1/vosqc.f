
      SUBROUTINE VOSWIN(IVERBOSE, HTHRESH1, HTHRESH2, HTHRESH3,
     +     ZTHRESH0, ZTHRESH1, ZTHRESH2, ZTHRESH3,
     +     RAYWIDTH, RANGE1, BINLENGTH,
     +     DBZNOISE, RADELEV1, NBINS1, NRAYS1, NSTEPS1,
     +     Z1, AZIM, TILTS, BADVALUE, STATUS)

	INCLUDE 'vosqc.par'

C The main subroutine, which accepts ordered VOS in Z(kms,rays,steps) and:

C  Prameters:
C  HFREEZE	Altitude of 0 C isotherm
C  RANGE1	Range of first bin [km]
C  BINLENGH	Range bin length [km]
C  DBZNOISE	Minimum reflectivity to process [dBZ]
C  RADELEV	Elevation of the radar [km]
C  NBINS	Number of bins in a ray
C  NRAYS	Number of rays in a sweep
C  NSTEPS	Number of elevation steps
C  Z		The 3-D array of reflectivities [dBZ/10]

C  	Returns:

      INTEGER*2 Z1(NBINS1,NRAYS1,NSTEPS1)
      REAL AZIM(NRAYS1,NSTEPS1)
      REAL TILTS(NSTEPS1)
      INTEGER*2 BADVALUE, STATUS

	REAL SWEEP(bins,raysmax)
	INTEGER IRAY2INRAY(0:360,steps)
	INTEGER*2 II1,II2,II3,LINE(raysmax)
	REAL DBZ102Z(-320:1000)
	REAL RAY1(200)

	INTEGER IBIN2KM1(bins),IBIN2KM2(bins)
	REAL WBIN2KM1(bins),WBIN2KM2(bins)
 	INTEGER*2 MATQC(kms,rays)

C     Check that the VOS dimensions do not exceed the limits set in
C     file 'vosqc.par'.
      IF (NSTEPS1 .GT. steps) THEN
         WRITE(6,*) 'sweeps=', NSTEPS1, ' QC sweeps limit=', steps
         STATUS = -1
         RETURN
      END IF
      IF (NRAYS1 .GT. raysmax) THEN
         WRITE(6,*) 'rays=', NRAYS1, ' QC rays limit=', raysmax
         STATUS = -1
         RETURN
      END IF
      IF (NBINS1 .GT. bins) THEN
         WRITE(6,*) 'bins=', NBINS1, ' QC bins limit=', bins
         STATUS = -1
         RETURN
      END IF

      STATUS = 0

	CBASE=HTHRESH1+RADELEV1
	HBASE=CBASE
	NBINS=NBINS1
	NRAYS=NRAYS1
	NSTEPS=NSTEPS1
	RADELEV=RADELEV1
	RNGDLY=RANGE1
	DELTAZ=RAYWIDTH
	IDELTAZ=DELTAZ*10+0.5
	NOISDBZ=DBZNOISE*10
	NOISDBZ0=NOISDBZ
	IF(NOISDBZ0.LT.0)NOISDBZ0=0
	BINLENG=BINLENGTH

      HFREEZE = HTHRESH3+1
	HBBF=HFREEZE			! The height up to which qc cleans data.
	IF(IVERBOSE.EQ.1)THEN
	print*,'HELLO FROM VOSWIN....'
	print*,'NBINS',nbins,'   NRAYS', nrays, '   NSTEPS', nsteps
	WRITE(6,*)'HBASE=',HBASE


	WRITE(6,*)'HTHRESH1=',HTHRESH1
	WRITE(6,*)'HTHRESH2=',HTHRESH2
	WRITE(6,*)'HTHRESH3=',HTHRESH3
	WRITE(6,*)'ZTHRESH0=',ZTHRESH0
	WRITE(6,*)'ZTHRESH1=',ZTHRESH1
	WRITE(6,*)'ZTHRESH2=',ZTHRESH2
	WRITE(6,*)'ZTHRESH3=',ZTHRESH3
	WRITE(6,*)'RAYWIDTH=',RAYWIDTH
	WRITE(6,*)'RANGE1=',RANGE1
	WRITE(6,*)'BINLENGTH=',BINLENGTH
	WRITE(6,*)'DBZNOISE=',DBZNOISE
	WRITE(6,*)'RADELEV=',RADELEV
	WRITE(6,*)'NBINS=',NBINS
	WRITE(6,*)'NRAYS=',NRAYS
	WRITE(6,*)'NSTEPS=',NSTEPS
	ENDIF
*
* 0.0 present in AZIM means that data for that ray is not present.
* This is typical of when some sweeps contain few rays than others.
* The dimension of AZIM, as well as Z, represents the maximum index
* encountered when constructing them.
*

C Reject calibration signal from Darwin data
	IF(NBINS.EQ.656.AND.BINLENGTH.EQ.0.3)THEN
	  DO K=1,NSTEPS
	  DO J=1,NRAYS
	  Z1(656,J,K)=0
	  ENDDO
	  ENDDO
	  ENDIF

	IF(IVERBOSE.EQ.1)THEN
      do 10 j = 1, nsteps
	WRITE(6,*)'J,TILTS(J)=',J,TILTS(J)
      do 10 i = 1, nrays
c         print'(''AZIM('',i3.3,'','',i3.3,'') = '',f7.2)',
c     +        i, j, azim(i,j)
 10   continue
	ENDIF

	I0=0
	IF(I0.EQ.1)THEN
	iizmax=-999
	maxi=0
	maxj=0
	maxk=0
	do k=1,NSTEPS
	do j=1,NRAYS
	do i=1,NBINS
c	Z(I,J,K)=Z1(I,J,K)
	IF(IVERBOSE.EQ.1)THEN
	if(z1(i,j,k).gt.iizmax .and. z1(i,j,k) .ne. BADVALUE)then
	   iizmax=Z1(i,j,k)
c	   write(6,*)'i,j,k,zmax=',i,j,k,iizmax
	   maxi=i
	   maxj=j
	   maxk=k
	   endif
	ENDIF
	IF(Z1(I,J,K).LT.NOISDBZ)Z1(I,J,K)=BADVALUE
	enddo
	enddo
	enddo
	IF(IVERBOSE.EQ.1)write(6,*)
     +	'Max input Z, km, az,step=',iizmax,maxi,maxj,maxk
	ENDIF

C	write(6,*)'VOSWIN: NKMS,rays,NSTEPS=',kms,rays,NSTEPS

	isee=0
	if(isee.eq.1)then
	write(6,*)
	write(6,*)'Z1 Matrix'
	ik1=10
	ik2=100
	ir1=001
	ir2=ir1+23
	do ir=ir1,ir2
	line(ir)=azim(ir,1)+0.5
	enddo
	write(6,'(4X,25I4)')(line(ir),ir=ir1,ir2)
	write(6,'(4X,25I4)')(ir,ir=ir1,ir2)
	do ik=ik1,ik2
	write(6,'(25I4)')ik,(Z1(ik,ir,1),ir=ir1,ir2)
	enddo
	write(6,'(4X,25I4)')(ir,ir=ir1,ir2)
	endif

c******************************************************************************
C  Build the regular VOS array
	IF(IVERBOSE.EQ.1)WRITE(6,*)'Build the regular VOS array'

	IF(BINLENGTH.EQ.0.OR.BINLENGTH.GT.1)THEN
	  WRITE(6,*)'BINLENGTH =',BINLENGTH
	  STOP' Invalid BINLENGTH'
	  ENDIF

	IF(BINLENGTH.LT.1)THEN
C  Pre-calculate the distribution from BINLENGTH bins to 1 km bins
	  RANGE=RANGE1
	  DO IBIN=1,NBINS
	  RANGSTART=RANGE+(IBIN-1)*BINLENGTH
	  RANGEND=RANGSTART+BINLENGTH
	  IRANGSTART=RANGSTART
	  IRANGEND=RANGEND
	  IF(IRANGSTART.EQ.IRANGEND)THEN
	    IBIN2KM1(IBIN)=IRANGSTART+1
	    WBIN2KM1(IBIN)=BINLENGTH
	    IBIN2KM2(IBIN)=0
	    WBIN2KM2(IBIN)=0.
	  ELSE
	    IBIN2KM1(IBIN)=IRANGSTART+1
	    WBIN2KM1(IBIN)=IRANGEND-RANGSTART
	    IBIN2KM2(IBIN)=IRANGEND+1
	    WBIN2KM2(IBIN)=RANGEND-IRANGEND
	    ENDIF
c	write(6,*)
c     +	IBIN,IBIN2KM1(IBIN),WBIN2KM1(IBIN),IBIN2KM2(IBIN),WBIN2KM2(IBIN)
	  ENDDO
	  ENDIF

	DO I=NOISDBZ,1000
	DBZ=I/100.
	DBZ102Z(I)=10.**DBZ
	ENDDO
	ZNOISE=DBZ102Z(NOISDBZ)
	ZNOISE0=DBZ102Z(NOISDBZ0)
	DBZ102Z(0)=0

C  Build a regular array for the sweeps

	DO ISTEP=1,NSTEPS
	IF(IVERBOSE.EQ.1)write(6,*)'ISTEP=',ISTEP

	  DO IRAY=1,360
	  IRAY2INRAY(IRAY,ISTEP)=0
	  ENDDO

	  DO INRAY=1,NRAYS
c	write(6,*)'ISTEP,INRAY,AZIM=',ISTEP,INRAY,AZIM(INRAY,ISTEP)
	    IF(AZIM(INRAY,ISTEP).LT.0)THEN
	      IRAY=0
	    ELSE
	      IRAY=AZIM(INRAY,ISTEP)+0.5
	      IF(IRAY.EQ.0)IRAY=360
C  IRAY contains the rounded azimuth of the INRAY input ray
	      IRAY2INRAY(IRAY,ISTEP)=INRAY
	      ENDIF
C  IRAY2INRAY contains the input ray number with the closest azimuth to IRAY
	  ENDDO !  INRAY=1,NRAYS

C  Fill zeros in IRAY2INRAY
	DO IRAY=1,360
C	write(6,*)IRAY,IRAY2INRAY(IRAY,ISTEP)
	IF(IRAY2INRAY(IRAY,ISTEP).EQ.0)THEN
	  IRAYM1=IRAY-1
	  IF(IRAYM1.EQ.0)IRAYM1=360
	  IRAYP1=IRAY+1
	  IF(IRAYP1.EQ.361)IRAYP1=1
	  INRAYM1=IRAY2INRAY(IRAYM1,ISTEP)
	  INRAYP1=IRAY2INRAY(IRAYP1,ISTEP)
C     I can't follow this code, but his array indices sometimes
C     are invalid. Added following escape clause. MJK 03/27/2000
      IF (INRAYM1.EQ.0 .OR. INRAYP1.EQ.0) THEN
         STATUS = -1
         RETURN
      ENDIF
	  AZM1=AZIM(INRAYM1,ISTEP)
	  AZP1=AZIM(INRAYP1,ISTEP)
	  AZ=IRAY
	  IF(ABS(AZ-AZP1).LT.ABS(AZ-AZM1))THEN
	    IRAY2INRAY(IRAY,ISTEP)=INRAYP1
	  ELSE
	    IRAY2INRAY(IRAY,ISTEP)=INRAYM1
	    ENDIF
	  ENDIF
	ENDDO

	  DO IRAY=1,360
C  Search for the two closest rays to the IRAY azimuth
	    DO IBIN=1,NBINS
	    SWEEP(IBIN,IRAY)=0
	    ENDDO
	  AZ2=IRAY
	  INRAY=IRAY2INRAY(IRAY,ISTEP)
	  AZ=AZIM(INRAY,ISTEP)
c	write(6,*)'IRAY,INRAY,AZ=',IRAY,INRAY,AZ
	  IF(AZ.EQ.0)THEN
C  Leave the ray empty
	  ELSE IF(AZ.EQ.AZ2)THEN
C  Copy the ray as is from INRAY to IRAY
	    DO IBIN=1,NBINS
	    II2=Z1(IBIN,INRAY,ISTEP)
	    IF(II2.GT.NOISDBZ0)SWEEP(IBIN,IRAY)=DBZ102Z(II2)
	    ENDDO
	  ELSE
	    INRAYM1=INRAY-1
	    IF(INRAYM1.LT.1)INRAYM1=NRAYS
	    AZM1=AZIM(INRAYM1,ISTEP)
	    IF(AZM1.NE.0.AND.AZM1.GT.AZ2)AZM1=AZM1-360.
	    INRAYP1=INRAY+1
	    IF(INRAYP1.GT.NRAYS)INRAYP1=1
	    AZP1=AZIM(INRAYP1,ISTEP)
	    IF(AZP1.NE.0.AND.AZP1.LT.AZ2)AZP1=AZP1+360.
C	write(6,'(I4,3F8.2)')IRAY,AZM1,AZ,AZP1
	    IF(AZM1.NE.0.AND.AZ.LE.AZ2)THEN
	      INRAYM1=INRAY
	      AZM1=AZ
	      ENDIF
	    IF(AZP1.NE.0.AND.AZ.GE.AZ2)THEN
	      INRAYP1=INRAY
	      AZP1=AZ
	      ENDIF
C	write(6,'(I4,3F8.2)')IRAY,AZM1,AZ,AZP1

	    IF(AZM1.EQ.0)THEN
C  Copy the ray as is from INRAYP1 to IRAY
	      DO IBIN=1,NBINS
	      II2=Z1(IBIN,INRAYP1,ISTEP)
	      IF(II2.GT.NOISDBZ0)SWEEP(IBIN,IRAY)=DBZ102Z(II2)
	      ENDDO
	    ELSE IF(AZP1.EQ.0)THEN
C  Copy the ray as is from INRAYM1 to IRAY
	      DO IBIN=1,NBINS
	      II2=Z1(IBIN,INRAYM1,ISTEP)
	      IF(II2.GT.NOISDBZ0)SWEEP(IBIN,IRAY)=DBZ102Z(II2)
	      ENDDO
	    ELSE IF(INRAYP1.EQ.INRAYM1)THEN
C  Copy the ray as is from INRAYP1 to IRAY
	      DO IBIN=1,NBINS
	      II2=Z1(IBIN,INRAYP1,ISTEP)
	      IF(II2.GT.NOISDBZ0)SWEEP(IBIN,IRAY)=DBZ102Z(II2)
	      ENDDO
	    ELSE
	      AZ1=AZM1
	      AZ3=AZP1
	      INRAY1=INRAYM1
	      INRAY3=INRAYP1
C	write(6,'(3F8.2)')AZ1,AZ2,AZ3
C  Interpolate the ray from INRAYM1 and INRAYP1 into IRAY
	      DO IBIN=1,NBINS
	      II1=Z1(IBIN,INRAYM1,ISTEP)
	      II3=Z1(IBIN,INRAYP1,ISTEP)
	      IF(II1.LE.NOISDBZ0.AND.II3.LE.NOISDBZ0)THEN
	        SWEEP(IBIN,IRAY)=0
	      ELSE
	        ZZ1=0
	        IF(II1.GT.NOISDBZ0)ZZ1=DBZ102Z(II1)
	         ZZ3=0
	        IF(II3.GT.NOISDBZ0)ZZ3=DBZ102Z(II3)
	        CALL INTERPOL(AZ1,ZZ1,AZ2,ZZ2,AZ3,ZZ3)
c	write(6,'(I4,6F8.2)')IBIN,AZ1,AZ2,AZ3,ZZ1,ZZ2,ZZ3
	        IF(ZZ2.GT.ZNOISE0)THEN
	          SWEEP(IBIN,IRAY)=ZZ2
	        ELSE
	          SWEEP(IBIN,IRAY)=0
	          ENDIF
	        ENDIF
	      ENDDO
	      ENDIF
	    ENDIF
	  ENDDO ! IRAY=1,360

C  Copy here from the sweep [Z] to the volume scan [dBZ/10]
C  Add here the conversion of bin length

	  IF(BINLENGTH.EQ.1)THEN
	    DO IRAY=1,360
	    DO IBIN=1,NBINS
	    IF(SWEEP(IBIN,IRAY).EQ.0)THEN
	      Z(IBIN,IRAY,ISTEP)=0
	    ELSE
	      Z(IBIN,IRAY,ISTEP)=100.*ALOG10(SWEEP(IBIN,IRAY))+0.5
	      ENDIF
	    ENDDO
	    ENDDO

	  ELSE  ! BINLENGTH.NE.1 here
	    DO IRAY=1,360

	      DO IKM=1,200
	      RAY1(IKM)=0
	      ENDDO

	      DO IBIN=1,NBINS
	      IF(SWEEP(IBIN,IRAY).NE.0)THEN
	        IKM1=IBIN2KM1(IBIN)
		IF(IKM1.LE.200)THEN
	          RAY1(IKM1)=RAY1(IKM1)+WBIN2KM1(IBIN)*SWEEP(IBIN,IRAY)
	          ENDIF
	        IKM2=IBIN2KM2(IBIN)
		IF(IKM2.LE.200)THEN
	          IF(IKM2.NE.0)
     +            RAY1(IKM2)=RAY1(IKM2)+WBIN2KM2(IBIN)*SWEEP(IBIN,IRAY)
	          ENDIF
	        ENDIF
	      ENDDO

	      DO IKM=1,200
	      IF(RAY1(IKM).LE.ZNOISE0)THEN
	        Z(IKM,IRAY,ISTEP)=0
	      ELSE
	        Z(IKM,IRAY,ISTEP)=100.*ALOG10(RAY1(IKM))+0.5
	        ENDIF
	      ENDDO

	    ENDDO
	    ENDIF

	ENDDO ! ISTEP=1,NSTEPS

C  Finish build the regular VOS array
c******************************************************************************

	IF(IVERBOSE.EQ.1)THEN
	iizmax=-999
	maxi=0
	maxj=0
	maxk=0
	do k=1,NSTEPS
	do j=1,rays
	do i=1,kms
c	Z(I,J,K)=Z1(I,J,K)
	if(Z(i,j,k).gt.iizmax)then
	   iizmax=Z(i,j,k)
c	   write(6,*)'i,j,k,zmax=',i,j,k,iizmax
	   maxi=i
	   maxj=j
	   maxk=k
	   endif
c	IF(Z(I,J,K).LT.NOISDBZ)Z(I,J,K)=0
	enddo
	enddo
	enddo
	write(6,*)'Max gridded Z, km, az,step=',iizmax,maxi,maxj,maxk
	ENDIF


	if(isee.eq.1)then
	write(6,*)
	write(6,*)
	write(6,*)'Z Matrix'
C	ik1=10
C	ik2=110
	ir11=ir1
	ir22=ir11+23
	write(6,'(4X,25I4)')(IRAY2INRAY(ir,1),ir=ir11,ir22)
	write(6,'(4X,25I4)')(ir,ir=ir11,ir22)
	do ik=ik1,ik2
	write(6,'(25I4)')ik,(Z(ik,ir,1),ir=ir11,ir22)
	enddo
	write(6,'(4X,25I4)')(ir,ir=ir11,ir22)
	endif

	IF(IVERBOSE.EQ.1)WRITE(6,*)'Call SETVOS'
	CALL SETVOS(NSTEPS1,TILTS)

	IF(IVERBOSE.EQ.1)WRITE(6,*)'Call BUILDVOS'
	CALL BUILDVOS

	if(isee.eq.1)then
	write(6,*)
	write(6,*)
	write(6,*)'itop Matrix'
C	ik1=10
C	ik2=110
	ir11=ir1
	ir22=ir11+23
	write(6,'(4X,25I4)')(IRAY2INRAY(ir,1),ir=ir11,ir22)
	write(6,'(4X,25I4)')(ir,ir=ir11,ir22)
	do ik=ik1,ik2
	write(6,'(25I4)')ik,(itop(ik,ir),ir=ir11,ir22)
	enddo
	write(6,'(4X,25I4)')(ir,ir=ir11,ir22)
	endif

 	IF(IVERBOSE.EQ.1)WRITE(6,*)'Call QC'

	CALL QC(MATQC,HTHRESH2,HTHRESH3,ZTHRESH0,
     +	ZTHRESH1,ZTHRESH2,ZTHRESH3)

	if(isee.eq.1)then
	write(6,*)
	write(6,*)
	write(6,*)'QC Matrix'
C	ik1=10
C	ik2=110
	ir11=ir1
	ir22=ir11+23
	write(6,'(4X,25I4)')(ir,ir=ir11,ir22)
	do ik=ik1,ik2
	write(6,'(25I4)')ik,(MATQC(ik,ir),ir=ir11,ir22)
	enddo
	write(6,'(4X,25I4)')(ir,ir=ir11,ir22)
	endif

 	IF(IVERBOSE.EQ.1)WRITE(6,*)'Apply the QC'

C  Apply the qc
c  Loop on elevetaion steps
	DO ISTEP=1,NSTEPS
C  Loop on rays
	DO INRAY=1,NRAYS
	IF(AZIM(INRAY,ISTEP).GE.0)THEN
	  IRAY=AZIM(INRAY,ISTEP)+0.5
	  IF(IRAY.EQ.0)IRAY=360
	  IF(IRAY.EQ.361)IRAY=1
C  Loop on bins
	  IKM_MAX=ISTEPF2KM(ISTEP)
	  IBINMAX=IKM_MAX/BINLENGTH
	  IF(IBINMAX.GT.NBINS)IBINMAX=NBINS
	  RANGE=RNGDLY

	  DO INBIN=1,IBINMAX
	  RANGE=RANGE+BINLENGTH
	  IKM=RANGE+0.5
	  IF(IKM.LE.200)THEN
	    IF(Z1(INBIN,INRAY,ISTEP).NE.BADVALUE)THEN
C  Check if needs to be zapped by the qc algorithm
C  Calculate the horizontal range of the bin, in integer km.
	      IKM_HORIZ=IBIN2KM(IKM,ISTEP)
C  Perform the qc
C  19Aug98 MJK Changed the following statement, since
C  IKM_HORIZ turns out to be 0 for the 1st bin if bin spacing
C  is too small (250m for MIT scans) .
C	      IF(IKM_HORIZ.LE.200)THEN
	      IF ((IKM_HORIZ.LE.200) .AND. (IKM_HORIZ.GE.1)) THEN
	        IF(MATQC(IKM_HORIZ,IRAY).EQ.1)Z1(INBIN,INRAY,ISTEP)
     +	        =BADVALUE
	        ENDIF
	      ENDIF
	    ENDIF
	  ENDDO		! IBIN=1,IBINMAX

	  ENDIF  	! AZIM(INRAY,ISTEP).NE.0
	ENDDO		! IRAY=1,INRAYS

C  Loop on rays
	DO IRAY=1,rays
C  Loop on bins
	  IKM_MAX=ISTEPF2KM(ISTEP)
	  IF(IKM_MAX.GT.200)IKM_MAX=200
	  RANGE=RNGDLY
	  DO IBIN=1,IKM_MAX
	  RANGE=RANGE+1
	  IKM=RANGE+0.5
	  IF(Z(IBIN,IRAY,ISTEP).NE.0)THEN
C  Check if needs to be zapped by the qc algorithm
C  Calculate the horizontal range of the bin, in integer km.
	    IKM_HORIZ=IBIN2KM(IKM,ISTEP)
C  Perform the qc
	    IF(MATQC(IKM_HORIZ,IRAY).EQ.1)Z(IBIN,IRAY,ISTEP)=0
	    ENDIF
	  ENDDO		! IBIN=1,IKM_MAX
	ENDDO		! IRAY=1,rays

	ENDDO		! ISTEP=1,NSTEPS

	if(isee.eq.1)then
	write(6,*)
	write(6,*)
	write(6,*)'QCd Z Matrix'
c	ik1=10
c	ik2=110
	ir11=ir1
	ir22=ir11+23
	write(6,'(4X,25I4)')(ir,ir=ir11,ir22)
	do ik=ik1,ik2
	write(6,'(25I4)')ik,(Z(ik,ir,1),ir=ir11,ir22)
	enddo
	write(6,'(4X,25I4)')(ir,ir=ir11,ir22)


	write(6,*)
	write(6,*)
	write(6,*)'QCd Z1 Matrix'
c	ik1=10
c	ik2=110
	ir11=ir1
	ir22=ir11+23
	write(6,'(4X,25I4)')(ir,ir=ir11,ir22)
	do ik=ik1,ik2
	write(6,'(25I4)')ik,(Z1(ik,ir,1),ir=ir11,ir22)
	enddo
	write(6,'(4X,25I4)')(ir,ir=ir11,ir22)
	endif

 	IF(IVERBOSE.EQ.1)WRITE(6,*)'Finish the QC'

      RETURN
      END

        SUBROUTINE INTERPOL(X1,Z1,X2,Z2,X3,Z3)
        IF(X3.EQ.X1)THEN
           Z2=(Z1+Z3)/2.
           RETURN
           ENDIF
        Z2=(Z3-Z1)*(X2-X1)/(X3-X1)+Z1
        RETURN
        END


