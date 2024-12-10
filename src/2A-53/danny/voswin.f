      SUBROUTINE VOSWIN(HBASE, HFREEZE, RAYWIDTH, RANGE1, BINLENGTH,
     +     DBZNOISE, RADLAT1, RADLON1, RADELEV1, 
     +     NBINS1, NRAYS1, NSTEPS1, PRADAR1, TRADAR1, DEWPOINT1, Z1,
     +     ZRFILE1, GAGFILE1, GWINFILE1, CSMAP,
     +     XKMP1, ALPHA1, MPONLY1, AZIM, TILTS, VERBOSE1,
     +     IYEAR, IDAY, IHOUR, IMINUTE, ISECOND,
     +     RAINMAP)

	INCLUDE 'voswin.par'

C The main subroutine, which accepts ordered VOS in Z(kms,rays,steps) and:
C  1. Calculates the classification parameters
C  2. Calculates the reference reflectivity map
C  3. Write out the rain gauge windows, for further ZeR calculations.
C  4. Applies the ZeR tables for production of polar rain maps.

C  Prameters:
C  IYEAR
C  IDAY		Julian date
C  IHOUR
C  IMINUTE
C  ISECOND
C  HBASE	Altitude for reflectivity map, to be used for rainfall.
C  HFREEZE	Altitude of 0 C isotherm
C  RANGE1	Range of first bin [km]
C  BINLENGH	Range bin length [km]
C  DBZNOISE	Minimum reflectivity to process [dBZ]
C  RADELEV	Elevation of the radar [km]
C  NBINS	Number of bins in a ray
C  NRAYS	Number of rays in a sweep
C  NSTEPS	Number of elevation steps
C  PRADAR	Surface pressure at the radar [mb]
C  TRADAR	Surface temperature at the radar [degrees kelvin]
C  DEWPOINT	Surface dew point at the radar [degrees kelvin]
C  Z		The 3-D array of reflectivities [dBZ/10]
C  ZRFILE	String with the Ze-R input file name.
C  GAGFILE      String with the Gauge coordinates file name.
C  GWINFILE	String with the gauge windows output file name.
C  XKMP		Optional K for a power law Z-R.
C  ALPHA	Optional ALPHA for a power law Z-R.
C  MPONLY	Flag for WPMM (0), or a power-law Z-R (1).

C  	Returned matrices:
C  RAINMATC	Cartesian 2x2 km matrix (151,151) of rain intensities [mm/hr]

C  	The next returned fields are all polar matrix (360 ray by 250 bins),
c  	filled out to 160 km.
C  RAINMATP	Rain intensities [mm/hr]
c  MATPZ	Base level reflectivities [dBZ/10]
c  MATPZF	Freezing level reflectivities [dBZ/10]
C  MATPH	Echo top height [km/10]
C  MATPH30	30 dBZ echo top height [km/10]
C  MATPHZM	Height of max reflectivity in the vertical [km/10]
C  MATPZHZM     Value of max reflectivity in the vertical [dBZ]
C  BBFM		Bright band fraction [0 - 1.]
C  RGM		Horizontal reflectivity gradients [dB/km]
C  CONDEFM	Effectvive Efficiency - (Qb-Qt)/Qb - index for echo top (0-1.)


      INTEGER*2 Z1(NBINS1,NRAYS1,NSTEPS1)
      REAL AZIM(NRAYS1,NSTEPS1)
      REAL TILTS(NSTEPS1)
      REAL MPONLY1
      INTEGER*2 CSMAP(151,151)
      INTEGER VERBOSE1

      REAL RAINMAP(151,151)
	INTEGER LINE(raysmax)

      character *(*) zrfile1, gagfile1, gwinfile1

	INTEGER*2 MATPZ(0:rays,kms),MATPZHZM(0:rays,kms),
     +	MATPH(0:rays,kms),MATPHZM(0:rays,kms),MATPH30(0:rays,kms),
     +	MATPZF(0:rays,kms)

	REAL RAINMAP_POL(0:rays,kms),
     +	CONDEFM(rays,kms),BBFM(rays,kms),RGM(rays,kms),RGF(rays,kms)

        DIMENSION ZMDS(kms)

	REAL SWEEP(bins,raysmax)
	INTEGER IRAY2INRAY(0:360,steps)
	INTEGER*2 II1,II2,II3
	REAL RAY1(200)

	INTEGER IBIN2KM1(bins),IBIN2KM2(bins)
	REAL WBIN2KM1(bins),WBIN2KM2(bins)
	BYTE MAPCS(ydim,xdim),MAPCSP(rays,kms)
	REAL*8 DECDAY
	DIMENSION IVEC(360*3)

	KMSHIFT=1
	IAZSHIFT=1

	NEWMDS=1

	VERBOSE=VERBOSE1
	NBINS=NBINS1
	NRAYS=NRAYS1
	NSTEPS=NSTEPS1
	RADLAT=RADLAT1
	RADLON=RADLON1
	RADELEV=RADELEV1
	PRADAR=PRADAR1
	TRADAR=TRADAR1
	DEWPOINT=DEWPOINT1
	RNGDLY=RANGE1
	DELTAZ=RAYWIDTH
	IDELTAZ=DELTAZ*10+0.5
	CBASE=HBASE
	HBBF=HFREEZE
	HBBF1=HBBF+1.5
	NOISDBZ=DBZNOISE*10
	BINLENG=BINLENGTH
	XKMP=XKMP1
	ALPHA=ALPHA1

	MPONLY=MPONLY1
	ZRFILE=ZRFILE1
	GAGFILE=GAGFILE1
	GWINFILE=GWINFILE1
	RADLAT=RADLAT1
	RADLON=RADLON1

c	WRITE(6,*)'HELLO FROM VOSWIN....'
	DECDAY=1000*MOD(IYEAR,100)+FLOAT(IDAY)+IHOUR/24.+IMINUTE/1440.
	IF(VERBOSE.EQ.1)THEN
	  WRITE(6,101)IYEAR, IDAY, IHOUR, IMINUTE, ISECOND
 101	  FORMAT('Year=',I4,'  Jday=',I3,'  Time=',I2,':',I2,':',I2)  
	  WRITE(6,*)'NBINS',nbins,'   NRAYS', nrays, '   NSTEPS', nsteps
	  WRITE(6,*)'HBASE=',HBASE
	  WRITE(6,*)'HFREEZE=',HFREEZE
	  WRITE(6,*)'RAYWIDTH=',RAYWIDTH
	  WRITE(6,*)'RANGE1=',RANGE1
	  WRITE(6,*)'BINLENGTH=',BINLENGTH
	  WRITE(6,*)'DBZNOISE=',DBZNOISE
	  WRITE(6,*)'RADLAT=',RADLAT
	  WRITE(6,*)'RADLON=',RADLON
	  WRITE(6,*)'RADELEV=',RADELEV
	  WRITE(6,*)'NBINS=',NBINS
	  WRITE(6,*)'NRAYS=',NRAYS
	  WRITE(6,*)'NSTEPS=',NSTEPS
	  WRITE(6,*)'PRADAR=',PRADAR
	  WRITE(6,*)'TRADAR=',TRADAR
	  WRITE(6,*)'DEWPOINT=',DEWPOINT
	  WRITE(6,*)'ZRFILE=',ZRFILE
	  WRITE(6,*)'GAGFILE=',GAGFILE
	  WRITE(6,*)'GWINFILE=',GWINFILE
	  WRITE(6,*)'XKMP=',XKMP
	  WRITE(6,*)'ALPHA=',ALPHA
	  WRITE(6,*)'MPONLY=',MPONLY
	  ENDIF
c ********************** NEXRAD deos not transfere the correct coordinates!
	IDRADAR=RADLAT
        IF(IDRADAR.EQ.28)THEN
           RADLAT   =  28.1131     !Melbourne NEXRAD, Florida. KMLB
           RADLON   = -80.6544
        ELSE IF(IDRADAR.EQ.-11.OR.IDRADAR.EQ.-12)THEN
           RADLAT   = -12.457     !DARWIN
           RADLON   = 130.925
	   ENDIF
c ********************** NEXRAD deos not transfere the correct coordinates!

*
* 0.0 present in AZIM means that data for that ray is not present.
* This is typical of when some sweeps contain few rays than others.
* The dimension of AZIM, as well as Z, represents the maximum index
* encountered when constructing them.
*

      do 10 j = 1, NSTEPS
	IF(VERBOSE.EQ.1)WRITE(6,*)'J,TILTS(J)=',J,TILTS(J)
      do 10 i = 1, nrays
c         print'(''AZIM('',i3.3,'','',i3.3,'') = '',f7.2)',
c     +        i, j, azim(i,j)
 10   continue

C  Eliminate duplicated tilt sequence
	NS=NSTEPS
	DO J=2,NS
	IF(TILTS(J).LT.TILTS(J-1))THEN
	   NS=J-1
	   GOTO 12
	   ENDIF
	ENDDO
 12	NSTEPS=NS

	iizmax=-999
	maxi=0
	maxj=0
	maxk=0
	do k=1,NSTEPS
	do j=1,NRAYS
	do i=1,NBINS
c	Z(I,J,K)=Z1(I,J,K)
	if(z1(i,j,k).gt.iizmax)then
	   iizmax=Z1(i,j,k)
c	   write(6,*)'i,j,k,zmax=',i,j,k,iizmax
	   maxi=i
	   maxj=j
	   maxk=k
	   endif
	IF(Z1(I,J,K).LT.NOISDBZ)Z1(I,J,K)=0
	enddo
	enddo
	enddo
	IF(VERBOSE.EQ.1)
     +	write(6,*)'Max input Z, km, az,step=',iizmax,maxi,maxj,maxk

C	write(6,*)'VOSWIN: NKMS,rays,NSTEPS=',kms,rays,NSTEPS
	isee=0
	if(isee.eq.1)then
	ik1=30
	ik2=50
	ir1=320
	ir2=ir1+23

	do istep=1,1
	write(6,*)
	write(6,*)'Z1 Matrix'
	write(6,*)'ISTEP=',istep
	do ir=ir1,ir2
	line(ir)=azim(ir,istep)+0.5
	enddo
	write(6,'(4X,25I4)')(line(ir),ir=ir1,ir2)
	write(6,'(4X,25I4)')(ir,ir=ir1,ir2)
	do ik=ik1,ik2
	write(6,'(25I4)')ik,(Z1(ik,ir,istep),ir=ir1,ir2)
	enddo
	write(6,'(4X,25I4)')(ir,ir=ir1,ir2)
	enddo
	endif


c******************************************************************************
C  Build the regular VOS array
	IF(VERBOSE.EQ.1)WRITE(6,*)'Build the regular VOS array'

	IF(BINLENGTH.EQ.0.OR.BINLENGTH.GT.1)THEN
	  WRITE(6,*)'BINLENGTH =',BINLENGTH
	  STOP' Invalid BINLENGTH'
	  ENDIF

	IF(BINLENGTH.LT.1)THEN
C  Pre-calculate the distribution from BINLENGTH bins to 1 km bins
	  RANGE=RANGE1-KMSHIFT
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
	  ENDDO
	  ENDIF

	NOISDBZ=DBZNOISE*10
	DO I=-320,1000
	DBZ=I/100.
	DBZ102Z(I)=10.**DBZ
	ENDDO
	ZNOISE=DBZ102Z(NOISDBZ)
	DBZ102Z(0)=0

C  Build a regular array for the sweeps

	DO ISTEP=1,NSTEPS
	IF(VERBOSE.EQ.1)write(6,*)'ISTEP=',ISTEP

	  DO IRAY=1,360
	  IRAY2INRAY(IRAY,ISTEP)=0
	  ENDDO

	  DO INRAY=1,NRAYS
	    IF(AZIM(INRAY,ISTEP).EQ.0)THEN
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
	  AZM1=AZIM(INRAYM1,ISTEP)
	  AZP1=AZIM(INRAYP1,ISTEP)
	  AZ=IRAY
	  IF(ABS(AZ-AZP1).LT.ABS(AZ-AZM1))THEN
	    IRAY2INRAY(IRAY,ISTEP)=INRAYP1
	  ELSE
	    IRAY2INRAY(IRAY,ISTEP)=INRAYM1
	    ENDIF
C	write(6,*)IRAY,IRAY2INRAY(IRAY,ISTEP),' ***********************'
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
C	write(6,*)'IRAY,INRAY,AZ=',IRAY,INRAY,AZ
	  IF(AZ.EQ.0)THEN
C  Leave the ray empty
	  ELSE IF(AZ.EQ.AZ2)THEN
C  Copy the ray as is from INRAY to IRAY
	    DO IBIN=1,NBINS
	    II2=Z1(IBIN,INRAY,ISTEP)
	    IF(II2.GT.NOISDBZ)SWEEP(IBIN,IRAY)=DBZ102Z(II2)
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
	      IF(II2.GT.NOISDBZ)SWEEP(IBIN,IRAY)=DBZ102Z(II2)
	      ENDDO
	    ELSE IF(AZP1.EQ.0)THEN
C  Copy the ray as is from INRAYM1 to IRAY
	      DO IBIN=1,NBINS
	      II2=Z1(IBIN,INRAYM1,ISTEP)
	      IF(II2.GT.NOISDBZ)SWEEP(IBIN,IRAY)=DBZ102Z(II2)
	      ENDDO
	    ELSE IF(INRAYP1.EQ.INRAYM1)THEN
C  Copy the ray as is from INRAYP1 to IRAY
	      DO IBIN=1,NBINS
	      II2=Z1(IBIN,INRAYP1,ISTEP)
	      IF(II2.GT.NOISDBZ)SWEEP(IBIN,IRAY)=DBZ102Z(II2)
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
	      IF(II1.LE.NOISDBZ.AND.II3.LE.NOISDBZ)THEN
	        SWEEP(IBIN,IRAY)=0
	      ELSE
	        ZZ1=0
	        IF(II1.GT.NOISDBZ)ZZ1=DBZ102Z(II1)
	         ZZ3=0
	        IF(II3.GT.NOISDBZ)ZZ3=DBZ102Z(II3)
	        CALL INTERPOLB(AZ1,ZZ1,AZ2,ZZ2,AZ3,ZZ3,DELTAZ)
c	write(6,'(I4,6F8.2)')IBIN,AZ1,AZ2,AZ3,ZZ1,ZZ2,ZZ3
	        IF(ZZ2.GT.ZNOISE)THEN
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
	    IRAYC=IRAY+IAZSHIFT
	    IF(IRAYC.LT.1)IRAYC=IRAYC+360
	    IF(IRAYC.GT.360)IRAYC=IRAYC-360
	    DO IBIN=1,NBINS
	    IBINSHIFT=IBIN+KMSHIFT
	    IF(IBINSHIFT.LT.1)IBINSHIFT=1
	    IF(IBINSHIFT.GT.NBINS)IBINSHIFT=NBINS
	    IF(SWEEP(IBINSHIFT,IRAYC).EQ.0)THEN
	      Z(IBIN,IRAY,ISTEP)=0
	    ELSE
	      Z(IBIN,IRAY,ISTEP)=100.*ALOG10(SWEEP(IBINSHIFT,IRAYC))+0.5
	      ENDIF
	    ENDDO
	    ENDDO

	  ELSE  ! BINLENGTH.NE.1 here
	    DO IRAY=1,360
	    IRAYC=IRAY+IAZSHIFT
	    IF(IRAYC.LT.1)IRAYC=IRAYC+360
	    IF(IRAYC.GT.360)IRAYC=IRAYC-360

	      DO IKM=1,200
	      RAY1(IKM)=0
	      ENDDO

	      DO IBIN=1,NBINS
	      IF(SWEEP(IBIN,IRAYC).NE.0)THEN
	        IKM1=IBIN2KM1(IBIN)
	        RAY1(IKM1)=RAY1(IKM1)+WBIN2KM1(IBIN)*SWEEP(IBIN,IRAYC)
	        IKM2=IBIN2KM2(IBIN)
	        IF(IKM2.NE.0)
     +          RAY1(IKM2)=RAY1(IKM2)+WBIN2KM2(IBIN)*SWEEP(IBIN,IRAYC)
	        ENDIF
	      ENDDO

	      DO IKM=1,200
	      IF(RAY1(IKM).LE.ZNOISE)THEN
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
	IF(VERBOSE.EQ.1)
     +	write(6,*)'Max gridded Z, km, az,step=',iizmax,maxi,maxj,maxk

	if(isee.eq.1)then
C	ik1=10
C	ik2=110
	ir11=ir1
	ir22=ir11+23
	write(6,*)
	do istep=1,10
	write(6,*)
	write(6,*)'Z Matrix'
	write(6,*)'ISTEP=',istep
	write(6,'(4X,25I4)')(IRAY2INRAY(ir,istep),ir=ir11,ir22)
	write(6,'(4X,25I4)')(ir,ir=ir11,ir22)
	do ik=ik1,ik2
	write(6,'(25I4)')ik,(Z(ik,ir,istep),ir=ir11,ir22)
	enddo
	write(6,'(4X,25I4)')(ir,ir=ir11,ir22)
	enddo
	endif

	IF(VERBOSE.EQ.1)WRITE(6,*)'Call SETVOS'
	CALL SETVOS(NSTEPS1,TILTS,IDRADAR)
	IF(VERBOSE.EQ.1)WRITE(6,*)'Call BUILDVOS'
	CALL BUILDVOS

	CALL MDS(ZMDS,NEWMDS)
	IF(VERBOSE.EQ.1)WRITE(6,*)'MDS at bin 100 is',ZMDS(100),' dBZ'
	NEWMDS=0

	CALL RDVOSP(MATPZ,MATPZF,MATPH,MATPHZM,MATPH30,MATPZHZM)

	CALL CSHOUZE(MATPZ,CSMAP,MAPCS,MAPCSP,IDELTAZ)

C  Find the reflectivity overhead the radar
	KS=3
	IF(KS.GT.NSTEPS1)KS=NSTEPS1
	J=0
	DO K=1,KS
	DO IR=1,360
	J=J+1
	IVEC(J)=Z(1,IR,K)
	ENDDO
	ENDDO

	CALL SORT(IVEC,1,J)
	J75=J*3/4
	IZ_OVERHEAD=IVEC(J75)
c	write(6,*)ivec
	IF(VERBOSE.EQ.1)WRITE(6,*)'IZ_OVERHEAD=',IZ_OVERHEAD

	CALL SWIN(MATPZ,MATPZF,MATPH,MATPHZM,MATPH30,MATPZHZM,ZMDS,
     +	MAPCSP,RAINMAP_POL,CONDEFM,BBFM,RGM,RGF,DECDAY,IZ_OVERHEAD)

	if(isee.eq.1)then

	write(6,*)
	write(6,*)
	write(6,*)'Z base polar Matrix'
C	ik1=10
C	ik2=110
	ir11=ir1
	ir22=ir11+23
	write(6,'(4X,25I4)')(ir,ir=ir11,ir22)
	do ik=ik1,ik2
	do ir=ir11,ir22
	line(ir)=MATPZ(ir,ik)
	enddo
	write(6,'(25I4)')ik,(line(ir),ir=ir11,ir22)
	enddo
	write(6,'(4X,25I4)')(ir,ir=ir11,ir22)

	write(6,*)
	write(6,*)
	write(6,*)'RAIN POLAR Matrix'
C	ik1=10
C	ik2=110
	ir11=ir1
	ir22=ir11+23
	write(6,'(4X,25I4)')(ir,ir=ir11,ir22)
	do ik=ik1,ik2
	do ir=ir11,ir22
	line(ir)=10*RAINMAP_POL(ir,ik)
	enddo
	write(6,'(25I4)')ik,(line(ir),ir=ir11,ir22)
	enddo
	write(6,'(4X,25I4)')(ir,ir=ir11,ir22)
	endif

	CALL POL2CART(RAINMAP_POL,RAINMAP,IDELTAZ)

	IF(VERBOSE.EQ.1)THEN
	if(isee.eq.1)then
	j1=1	! Y
	j2=j1+150
	i1=75	! X
	i2=i1+19

	write(6,*)
	write(6,*)'RAIN 2x2 km cartesian-field'
	write(6,'(3x,24i5)')(i,i=i1,i2)
	do j=j1,j2
	write(6,'(i3,24F5.1)')j,(RAINMAP(i,j),i=i1,i2)
	enddo
	write(6,'(3x,24i5)')(i,i=i1,i2)

	write(6,*)
	write(6,*)'CS 2x2 km cartesian-field'
	do j=j1,j2
	write(6,'(i3,24I5)')j,(MAPCS(i,j),i=i1,i2)
	enddo
	write(6,'(3x,24i5)')(i,i=i1,i2)
	endif  ! (0.eq.1)
	ENDIF

	CALL GWIN(IYEAR,IDAY,IHOUR,IMINUTE,ISECOND,ZMDS,MAPCSP,
     +	IZ_OVERHEAD)

	IWRCLASS=1
	IWRPDF=0
	IWRZ=1
c	write(6,*)'CALL WRMAP'
c	CALL WRMAP(RAINMAP_POL,IYEAR,IWRCLASS,IDAY,
c     +	IHOUR,IMINUTE,IWRZ,MATPZ,MATPH,MAPCSP)


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

        SUBROUTINE INTERPOLB(X1,Z1,X2,Z2,X3,Z3,BWIDTH)
	COMMON/BINT/ IBFIRST,GG(0:3600)
	DATA IBFIRST/1/

	IF(IBFIRST.EQ.1)THEN
	   IBFIRST=0
	   PHI0=BWIDTH/2
	   PHI02=PHI0**2
C  INITIALIZE LOBE LOOKUP TABLE
c	   WRITE(6,*)'START INITIALIZE GTABLE'
	   CALL Lobe (1.,0.0,PHI0,PHI02,G)
	   XLong=0
	   DO 2 ILAT=0,3600
	   XLAT=ILAT/10.
	   CALL Lobe (XLong,XLat,PHI0,PHI02,G)
	   GG(ILAT)=G
c	   IF(G.GT.0)WRITE(6,*)XLAT,G,10*ALOG10(G)
 2	   CONTINUE
c	   WRITE(6,*)'FINISH INITIALIZE GTABLE'

	   ENDIF

        IF(X3.EQ.X1)THEN
           Z2=(Z1+Z3)/2.
           RETURN
           ENDIF

	IDAZ1=ABS(X2-X1)*10+0.5
	IDAZ3=ABS(X2-X3)*10+0.5
	W1=GG(IDAZ1)
	W3=GG(IDAZ3)
	Z2=(W1*Z1+W3*Z3)/(W1+W3)
        RETURN
        END

      SUBROUTINE LOBE (Long,Lat,PHI0,PHI02,G)
C  EQUATION FOR ANTENNA GAIN AS A FUNCTION OF DEVIATION FROM ANTENNA
C  AXIS (BC)     REFERENCE: BATTAN (1973), P.168  EQ.9.5
      REAL Long,Lat
	DEG2RAD=3.141592/180.
      BC=ACOS(COS(Lat*DEG2RAD)*COS(Long*DEG2RAD))

      G=10**(-0.6*(BC/PHI0)**2) + 
     +    10E-4 * 10**(-2.4*((BC-4*PHI0)**2)/PHI02)    +
     +    4 * 10E-6 * 10**(-2.4*((BC-6*PHI0)**2)/PHI02)  +
     +    10E-6 * 10**(-2.4*((BC-8*PHI0)**2)/PHI02) 
      RETURN
      END
