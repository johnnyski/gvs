	SUBROUTINE SWIN(MATPZ,MATPZF,MATPH,MATPHZM,MATPH30,MATPZHZM,ZMDS,
     +	MAPCSP,RAINMATP,CONDEFM,BBFM,RGM,RGF,DECDAY,IZ_OVERHEAD)

	INCLUDE 'voswin.par'

        PARAMETER (wrays=11, wbins=7)

	INTEGER*2 MATPZ(0:rays,kms),MATPZHZM(0:rays,kms),
     +	MATPH(0:rays,kms),MATPHZM(0:rays,kms),MATPH30(0:rays,kms),
     +	MATPZF(0:rays,kms)

        DIMENSION ZMDS(kms),IZMDS(kms)

        INTEGER IWZ(wrays,wbins),IWH(wrays,wbins),IWHZM(wrays,wbins),
     +	IWZF(wrays,wbins)

C     +	,IWHCAPPI(wrays,wbins,elevs),IWZMAX(wrays,wbins),
C     +	IWZBASE(wrays,wbins),IWSZM(wrays,wbins),
C     +	ISTOP(wrays,wbins),ISTOP1(wrays,wbins)

	REAL CONDEFM(rays,kms),BBFM(rays,kms),
     +	RGM(rays,kms),RGMIN(rays,kms),RGF(rays,kms)
	REAL RAINMATP(0:rays,kms)

	DIMENSION RANGLIMITS(2),RGLIMITS(2),HFLIMITS(2),EELIMITS(2),
     +	RGFLIMITS(2),RGVLLIMITS(2),RGVHLIMITS(2)
	DIMENSION INTPAR(10)
	DIMENSION LINE(rays)

	COMMON/CSWIN/IFIRST
	BYTE MAPCSP(rays,kms)
	REAL*8 DDELTAZ,DECDAY

c	DIMENSION CLASS(kms3,rays3,11)
	DATA IFIRST/0/
C saved CLASS parameters:
C 1: RADGRAD
C 2: EE
C 3: P_BB
C 4: HTOP
C 5: H30
C 6: AV_HZMAX
C 7: RADGRADIN
C 8: Z_HZMAX
C 9: dZ/dH below FL+1
C 10: dZ/dH above FL+1
C 11: RADGRAD FREEZING

	DDELTAZ=DELTAZ*0.0174533
	REFRANGE=1./DSIN(DDELTAZ)
C	WRITE(6,*)DELTAZ,DSIN(DDELTAZ),REFRANGE

	DO J=1,kms
	IZMDS(J)=ZMDS(J)
	DO I=1,rays
	RAINMATP(I,J)=0
	CONDEFM(I,J)=0
	BBFM(I,J)=0
	RGM(I,J)=0
	RGMIN(I,J)=0
	ENDDO
	ENDDO

	IHF=HBBF*1000
	BINLENGTH=1

	IF(IFIRST.EQ.0)THEN
	   IFIRST=1
	   IRTYPE=0
	   RADGRADF=IZ_OVERHEAD/10.
	   CALL READZR(IDBZ,IRANGE,P_BB,EE,RADGRAD,IHF,RADGRADF,
     +	   RGVL,RGVH,ICS,IRTYPE,RR,RRMP,ZRFILE,INTPAR,RANGLIMITS,
     +	   EELIMITS,RGLIMITS,HFLIMITS,RGFLIMITS,RGVLLIMITS,RGVHLIMITS,
     +	   MPONLY,XKMP,ALPHA,VERBOSE,DECDAY)

	   IF(VERBOSE.EQ.1)THEN
	     WRITE(6,*)'Finished initialization of READZR'
	     WRITE(6,*)'RANGLIMITS=',RANGLIMITS
	     WRITE(6,*)'RGLIMITS=',RGLIMITS
	     WRITE(6,*)'HFLIMITS=',HFLIMITS
	     WRITE(6,*)'EELIMITS=',EELIMITS
	     ENDIF
	   ENDIF

C  Loop on the Z data, creating the rain maps.
	JBINSTEP=JBINSTP	! Number of bins in a box that is classified at onece.
	IRAYSTEP=IRAYSTP	! Number of rays in a box that is classified at onece.
	JBINSW=7	! Number of bins in a classification window.
	IRAYSW=11	! Number of rays in a classification window.
	IKM3=0
	DO 310 JBIN=3,200,JBINSTEP
	IF(JBIN.GT.60)IRAYSW=9
	IF(JBIN.GT.90)IRAYSW=7
	IF(JBIN.GT.120)IRAYSW=5
	IF(JBIN.GT.150)IRAYSW=3
	IRANGE=JBIN*BINLENGTH
	IKM3=IKM3+1
	IRAY3=0
	DO 320 IRAY=1,rays,IRAYSTEP
	IRAY3=IRAY3+1


C    Check if there is any reflectivity data, worth the effort of dealing with 
C    the window
	JJ1=JBIN-JBINSTEP/2
	JJ2=JBIN+JBINSTEP/2

	II1=IRAY-IRAYSTEP/2
	II2=IRAY+IRAYSTEP/2
	NZ=0
	NH=0
	IZHZM=-99
	IHMAX=0
	IHMAXZ=0
	NHMAXZ=0
	IHMAX30=0
	DO 325 J=JJ1,JJ2
	IF(J.LT.1.OR.J.GT.kms)GOTO 325
	DO I=II1,II2
C       keep azimuth coordinates within dimension bounds
	II=I
	IF(II.LT.1)II=II+rays
	IF(II.GT.rays)II=II-rays
	IF(IZHZM.LT.MATPZHZM(II,J))IZHZM=MATPZHZM(II,J)
	IF(MATPZ(II,J).NE.0)NZ=NZ+1
	IF(MATPH(II,J).NE.0)THEN
	   NH=NH+1
	   IF(IHMAX.LT.MATPH(II,J))IHMAX=MATPH(II,J)
	   IF(MATPH30(II,J).NE.0)THEN
	      IF(IHMAX30.LT.MATPH30(II,J))IHMAX30=MATPH30(II,J)
	      ENDIF

	   NHMAXZ=NHMAXZ+1
	   IHMAXZ=IHMAXZ+MATPHZM(II,J)
	   ENDIF

	ENDDO
 325	CONTINUE
	IF(NHMAXZ.GT.0)IHMAXZ=IHMAXZ/NHMAXZ

	IF(NZ.GT.0.OR.NH.GT.0)THEN
C	   here we are in a new classification window, of JBINSWxIRAYSW
C	   Build the classification window.
	   J1=JBIN-JBINSW/2
	   J2=JBIN+JBINSW/2
C          keep range coordinates within dimension bounds
	   IF(J1.LT.1)THEN
	      J2=J2+1+J1
	      J1=1
	      ENDIF
	   IF(J2.GT.kms)THEN
	      J1=J1-(J2-kms)
	      J2=kms
	      ENDIF
	   I1=IRAY-IRAYSW/2
	   I2=IRAY+IRAYSW/2
	   JW=0
	   DO 330 J=J1,J2
	   JW=JW+1
	   IW=0
	   DO 330 I=I1,I2
	   IW=IW+1
C          keep azimuth coordinates within dimension bounds
	   II=I
	   IF(II.LT.1)II=II+rays
	   IF(II.GT.rays)II=II-rays
	   IWZ(IW,JW)=MATPZ(II,J)
	   IWZF(IW,JW)=MATPZF(II,J)
	   IWH(IW,JW)=MATPH(II,J)
	   IWHZM(IW,JW)=MATPHZM(II,J)


c	   IWZMAX(IW,JW)=IZMAX(J,I)
c	   IWZBASE(IW,JW)=IZBASE(J,I)
c	   IWSZM(IW,JW)=IHZMAX(J,I)
c	   ISTOP(IW,JW)=ITOP(J,I)
c	   ISTOP1(IW,JW)=ITOP1(J,I)
c	   do istep=1,12
c	   IWHCAPPI(IW,JW,istep)=IZCPI(istep,J,I)
c	   enddo

 330	   CONTINUE

	if(0.eq.1.and.JBIN.GT.10.AND.NZ.GT.0)then

	write(6,*)
	write(6,*)
	write(6,*)'km1,km2=',J1,J2,'  ray1,ray2=',I1,I2
	write(6,*)
	write(6,*)'Z 7X11 WINDOW,  NZ=',NZ
	do j=1,7
	write(6,'(20i3)')(iwz(i,j)/10,i=1,11)
	enddo

	write(6,*)
	write(6,*)'H 7X11 WINDOW'
	do j=1,7
	write(6,'(20i3)')(iwh(i,j),i=1,11)
	enddo

c	write(6,*)
c	write(6,*)'H TOP STEP 7X11 WINDOW'
c	do j=1,7
c	write(6,'(20i3)')(ISTOP(i,j),i=1,11)
c	enddo
c
c	write(6,*)
c	write(6,*)'H TOP1 STEP 7X11 WINDOW'
c	do j=1,7
c	write(6,'(20i3)')(ISTOP1(i,j),i=1,11)
c	enddo

	write(6,*)
	write(6,*)'HZM height 7X11 WINDOW'
	do j=1,7
	write(6,'(20i3)')(iwhzm(i,j),i=1,11)
	enddo

c	write(6,*)
c	write(6,*)'HZM step 7X11 WINDOW'
c	do j=1,7
c	write(6,'(20i3)')(iwszm(i,j),i=1,11)
c	enddo
c
c	write(6,*)
c	write(6,*)'ZMAX 7X11 WINDOW'
c	do j=1,7
c	write(6,'(20i3)')(iwzmax(i,j)/10,i=1,11)
c	enddo
c
c	write(6,*)
c	write(6,*)'ZBASE 7X11 WINDOW'
c	do j=1,7
c	write(6,'(20i3)')(iwzbase(i,j)/10,i=1,11)
c	enddo
c
c	do istep=1,12
c	write(6,*)
c	write(6,*)'Z step',istep
c	do j=1,7
c	write(6,'(20i3)')(iwhcappi(i,j,istep)/10,i=1,11)
c	enddo
c	enddo

	endif  ! (0.eq.1) 
	   NZ3=0
	   NZF3=0
	   DO J=JBIN-1,JBIN+1
	   DO I=IRAY-1,IRAY+1
	   IF(MATPZ(I,J).NE.0)NZ3=NZ3+1
	   IF(MATPZF(I,J).NE.0)NZF3=NZF3+1
	   ENDDO
	   ENDDO

           IHRADAR1=RADELEV*1000
           PRADAR1=PRADAR
           TRADAR1=TRADAR
           DEWPOINT1=DEWPOINT
	   IHMIN=HMIN*10
           CALL GRAD(IWZ,IWH,IHRADAR1,PRADAR1,TRADAR1,DEWPOINT1,
     +     IRAYSW,JBINSW,IDELTAZ,BINLENGTH,IHMIN,RADGRAD,RGIN,EE,
     +     IHMEAN,IRANGE,IZMDS,IAZGRAD,VERBOSE)
c	   RADGRAD=RADGRAD*IRANGE/REFRANGE

	   IF(NZF3.NE.0)THEN
	      CALL GRAD(IWZF,IWH,IHRADAR1,PRADAR1,TRADAR1,DEWPOINT1,
     +        IRAYSW,JBINSW,IDELTAZ,BINLENGTH,IHMIN,RADGRADF,RGINF,EE,
     +        IHMEAN,IRANGE,IZMDS,IAZGRADF,VERBOSE)
c	      RADGRADF=RADGRADF*IRANGE/REFRANGE
	   ELSE
	      RADGRADF=0
	      RGINF=0
	      IAZGRADF=0
	      ENDIF

	   CALL BBF(IWHZM,JBINSW,IRAYSW,HBBF,P_BB,AV_HZMAX,
     +	   IRANGE,IRAY)

C	   IF(NZ3.GT.0)THEN
	      CALL GETZH(JBINSW,IRAYSW,IRANGE,IRAY,AL,RGVL,RL,AH,RGVH,RH)
C	   ELSE
C	      AL=0
C	      RGVL=0
C	      RL=0
C	      AH=0
C	      RGVH=0
C	      RH=0
C	      ENDIF

	if(0.eq.1.and.irange.gt.50.and.irange.lt.75)then

	write(6,*)
	write(6,*)'IWZ='
	do j=1,jbinsw
	write(6,'(11i4)')(iwz(i,j),i=1,iraysw)
	enddo

	write(6,*)'IWZF='
	do j=1,jbinsw
	write(6,'(11i4)')(iwzf(i,j),i=1,iraysw)
	enddo
	write(6,*)'RADGRAD,RADGRADF=',RADGRAD,RADGRADF
	write(6,*)'RGVL,RGVH,BBF=',RGVL,RGVH,P_BB
	endif

	IF(NZ.EQ.0)RADGRAD=99.99

C   Do here quality control and more clutter rejection.
	ISKIP=0
C   clutter rejection by gradients:
c	IF(RADGRAD.GT.20.0.AND.RADGRAD.LT.99.OR.EE.LT.0.2)ISKIP=1


	  CLASS(IKM3,IRAY3,1)=RADGRAD
	  CLASS(IKM3,IRAY3,2)=EE
	  CLASS(IKM3,IRAY3,3)=P_BB
	  CLASS(IKM3,IRAY3,4)=IHMAX
	  CLASS(IKM3,IRAY3,5)=IHMAX30
	  CLASS(IKM3,IRAY3,6)=IHMAXZ
	  CLASS(IKM3,IRAY3,7)=RGIN
	  CLASS(IKM3,IRAY3,8)=IZHZM
	  CLASS(IKM3,IRAY3,9)=RGVL
	  CLASS(IKM3,IRAY3,10)=RGVH
	  CLASS(IKM3,IRAY3,11)=RADGRADF

C          Integrate the rainfall within the simultaneously classified domain
	   DO 340 J=JJ1,JJ2
	   IF(J.LT.1.OR.J.GT.kms)GOTO 340
	   IRANGE=J*BINLENGTH
	   DO 335 I=II1,II2
C          keep azimuth coordinates within dimension bounds
	   II=I
	   IF(II.LT.1)II=II+rays
	   IF(II.GT.rays)II=II-rays
	   IF(MATPZ(II,J).NE.0.AND.EE.LE.1.0.AND.RADGRAD.LT.99)THEN
	   IDBZ=(MATPZ(II,J))

	   IF(IDBZ.GT.0)THEN
	     EE1=EE
	     RADGRAD1=RADGRAD
	     IRANGE1=IRANGE
	     P_BB1=P_BB
	     IF(EE1.LT.EELIMITS(1))EE1=EELIMITS(1)
c	     IF(RADGRAD1.LT.RGLIMITS(1))RADGRAD1=RGLIMITS(1)
	     IF(RADGRAD1.GE.RGLIMITS(2))RADGRAD1=RGLIMITS(2)
c	     IF(IRANGE1.GT.RANGLIMITS(2))IRANGE1=RGLIMITS(2)
c	     IF(P_BB1.LT.0)P_BB1=0
c	     IF(P_BB1.GT.1)P_BB1=0
c	     if(0.eq.1)write(6,*)ii,irange,P_BB,EE,RADGRAD,IHF
	     IF(IDBZ.GT.700)THEN
	       WRITE(6,*)
     +	       'Warning. Excessive Reflectivity. range,ray, dBZ=',
     +	       IRANGE1,II,IDBZ/10.
	       WRITE(6,*)'Reflectivity set to 0'
	       IDBZ=0
	       ENDIF

	     ICS=MAPCSP(II,J)

	     IF(ICS.EQ.0.AND.IRANGE.LE.150)ICS=1
	     RADGRADF=IZ_OVERHEAD/10.
	     CALL READZR(IDBZ,IRANGE1,P_BB1,EE1,RADGRAD1,IHF,RADGRADF,
     +	     RGVL,RGVH,ICS,IRTYPE,RR,RRMP,ZRFILE,INTPAR,RANGLIMITS,
     +	     EELIMITS,RGLIMITS,HFLIMITS,RGFLIMITS,RGVLLIMITS,RGVHLIMITS,
     +	     MPONLY,XKMP,ALPHA,VERBOSE,DECDAY)

	     IF(MPONLY.EQ.0)RAINMATP(II,J)=RR
	     IF(MPONLY.EQ.1)RAINMATP(II,J)=RRMP
	     ENDIF

c	if(idbz.gt.300)then
c	write(6,*)' IDBZ,IRANGE,P_BB,EE,RADGRAD,IHF=',
c     +  IDBZ,IRANGE,P_BB,EE,RADGRAD,IHF
c	write(6,*)'RR,RRMP=',RR,RRMP,RAINMATP(II,J)
c	endif

c	      ELSE
c	        IF(MATPZ(II,J).GT.50)
c     +		WRITE(6,*)'II,J,Z,EE,RADGRAD=',
c     +		II,J,MATPZ(II,J),EE,RADGRAD
	      ENDIF

    
	   CONDEFM(II,J)=EE
	   BBFM(II,J)=P_BB
	   RGM(II,J)=RADGRAD
	   RGMIN(II,J)=RGIN
	   RGF(II,J)=RADGRADF

 335	   CONTINUE
 340	   CONTINUE

	ENDIF
	

 320	CONTINUE
 310	CONTINUE

	ISEEW=0
	IF(ISEEW.EQ.1)THEN

	MIN_WRANGE=50
	MAX_WRANGE=70
	MIN_WRAY=021
	MAX_WRAY=044
	j1=MIN_WRANGE
	j2=MAX_WRANGE
	i1=MIN_WRAY
	i2=MAX_WRAY

	write(6,*)
	write(6,*)'dBZ/10 sub-field'
	do j=j1,j2
	do i=i1,i2
	line(i)=0
	   IF(MATPZ(i,J).NE.0)THEN
	      line(i)=MATPZ(i,J)/10
	      endif
	enddo
	write(6,'(i3,24I5)')j,(line(i),i=i1,i2)
	enddo
	write(6,'(3x,24i5)')(i,i=i1,i2)

	write(6,*)
	write(6,*)'CSHOUZE sub-field'
	do j=j1,j2
	do i=i1,i2
	line(i)=MAPCSP(i,J)
	enddo
	write(6,'(i3,24I5)')j,(line(i),i=i1,i2)
	enddo
	write(6,'(3x,24i5)')(i,i=i1,i2)

	write(6,*)
	write(6,*)'RAIN sub-field'
	do j=j1,j2
	write(6,'(i3,24F5.1)')j,(RAINMATP(i,j),i=i1,i2)
	enddo
	write(6,'(3x,24i5)')(i,i=i1,i2)

	write(6,*)
	write(6,*)'EE sub-field'
	do j=j1,j2
	write(6,'(i3,24F5.2)')j,(CONDEFM(i,j),i=i1,i2)
	enddo
	write(6,'(3x,24i5)')(i,i=i1,i2)

	write(6,*)
	write(6,*)'BBF sub-field'
	do j=j1,j2
	write(6,'(i3,24F5.2)')j,(BBFM(i,j),i=i1,i2)
	enddo
	write(6,'(3x,24i5)')(i,i=i1,i2)

	write(6,*)
	write(6,*)'RG sub-field'
	do j=j1,j2
	write(6,'(i3,24F5.1)')j,(RGM(i,j),i=i1,i2)
	enddo
	write(6,'(3x,24i5)')(i,i=i1,i2)

	write(6,*)
	write(6,*)'RGIN sub-field'
	do j=j1,j2
	write(6,'(i3,24F5.1)')j,(RGMIN(i,j),i=i1,i2)
	enddo
	write(6,'(3x,24i5)')(i,i=i1,i2)

	write(6,*)
	write(6,*)'RGF sub-field'
	do j=j1,j2
	write(6,'(i3,24F5.1)')j,(RGF(i,j),i=i1,i2)
	enddo
	write(6,'(3x,24i5)')(i,i=i1,i2)

	write(6,*)
	write(6,*)'dBZF/10 sub-field'
	do j=j1,j2
	do i=i1,i2
	line(i)=0
	   IF(MATPZF(i,J).NE.0)THEN
	      line(i)=MATPZF(i,J)/10
	      endif
	enddo
	write(6,'(i3,24I5)')j,(line(i),i=i1,i2)
	enddo
	write(6,'(3x,24i5)')(i,i=i1,i2)

	write(6,*)
	write(6,*)'H sub-field'
	JJ1=J1/3
	JJ2=J2/3
	II1=I1/3+1
	II2=I2/3+1
	do j=JJ1,JJ2
	do I=II1,II2
	LINE(I)=CLASS(j,i,4)
	enddo
	write(6,'(i3,24I5)')j*3+1,(LINE(I),I=II1,II2)
	enddo
	write(6,'(3x,24i5)')(i,i=I1,I2+3,3)

	write(6,*)
	write(6,*)'H30 sub-field'
	JJ1=J1/3
	JJ2=J2/3
	II1=I1/3+1
	II2=I2/3+1
	do j=JJ1,JJ2
	do I=II1,II2
	LINE(I)=CLASS(j,i,5)
	enddo
	write(6,'(i3,24I5)')j*3+1,(LINE(I),I=II1,II2)
	enddo
	write(6,'(3x,24i5)')(i,i=I1,I2+3,3)

	write(6,*)
	write(6,*)'HZMAX sub-field'
	JJ1=J1/3
	JJ2=J2/3
	II1=I1/3+1
	II2=I2/3+1
	do j=JJ1,JJ2
	do I=II1,II2
	LINE(I)=CLASS(j,i,6)
	enddo
	write(6,'(i3,24I5)')j*3+1,(LINE(I),I=II1,II2)
	enddo
	write(6,'(3x,24i5)')(i,i=I1,I2+3,3)

	write(6,*)
	write(6,*)'BBF sub-field'
	JJ1=J1/3
	JJ2=J2/3
	II1=I1/3+1
	II2=I2/3+1
	do j=JJ1,JJ2
	do I=II1,II2
	LINE(I)=CLASS(j,i,3)*100
	enddo
	write(6,'(i3,24I5)')j*3+1,(LINE(I),I=II1,II2)
	enddo
	write(6,'(3x,24i5)')(i,i=I1,I2+3,3)

	write(6,*)
	write(6,*)'Z HZmax sub-field'
	JJ1=J1/3
	JJ2=J2/3
	II1=I1/3+1
	II2=I2/3+1
	do j=JJ1,JJ2
	do I=II1,II2
	LINE(I)=CLASS(j,i,8)
	enddo
	write(6,'(i3,24I5)')j*3-1,(LINE(I),I=II1,II2)
	enddo
	write(6,'(3x,24i5)')(i,i=I1,I2+3,3)

	write(6,*)
	write(6,*)'dZ/dH below HF+1 sub-field'
	JJ1=J1/3
	JJ2=J2/3
	II1=I1/3+1
	II2=I2/3+1
	do j=JJ1,JJ2
	do I=II1,II2
	LINE(I)=CLASS(j,i,9)*10
	enddo
	write(6,'(i3,24F5.1)')j*3-1,(LINE(I)/10.,I=II1,II2)
	enddo
	write(6,'(3x,24i5)')(i,i=I1,I2+3,3)

	write(6,*)
	write(6,*)'dZ/dH above HF+1 sub-field'
	JJ1=J1/3
	JJ2=J2/3
	II1=I1/3+1
	II2=I2/3+1
	do j=JJ1,JJ2
	do I=II1,II2
	LINE(I)=CLASS(j,i,10)*10
	enddo
	write(6,'(i3,24F5.1)')j*3-1,(LINE(I)/10.,I=II1,II2)
	enddo
	write(6,'(3x,24i5)')(i,i=I1,I2+3,3)



	ENDIF 	! (ISEEW.EQ.1)


        RETURN
        END

