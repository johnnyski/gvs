 	SUBROUTINE READZR(IDBZ1,IRANGE,BB,EE,RGB,IFL,RGF,RGVL,RGVH,ICS,
     +  IRTYPE,RR,RRMP,ZRFILE,INTPAR,RANGELIMITS,EELIMITS,RGBLIMITS,
     +  FLLIMITS,RGFLIMITS,RGVLLIMITS,RGVHLIMITS,MPONLY,KMP,ALPHA,
     +  VERBOSE,DAY)
c **********************************************************************
C *  THIS SUBROUTINE upon first call, reads the input directives 
C *  from the .INP files and the Ze-R relationships from the     
C *  ZR*.OUT file. Upon subsequent calls, it is beeing called for
C *  each pixel, with the relevant classification parameters and 
C *  with the reflectivity, and return rain intensity.           

C *******************************************************
C *    PROGRAM WRITTEN BY:  EYAL AMITAI (HEB. UNIV.)    *
C *******************************************************
C *       PROGRAM WRITTEN:  JULY 15, 1993	        *
C *******************************************************
C * VERSION LAST MODIFIED:  MAY  8,  1996   	        *
C *******************************************************

        PARAMETER       (ncolum=100)
        PARAMETER       (intpar1max=20)
	DIMENSION RI(0:700,ncolum),RMP(0:700)

        DIMENSION RANGEMIN(ncolum),RANGEMAX(ncolum)
        DIMENSION BBMIN(ncolum),BBMAX(ncolum)
        DIMENSION EEMIN(ncolum),EEMAX(ncolum)
        DIMENSION RGBMIN(ncolum),RGBMAX(ncolum)
        DIMENSION FLMIN(ncolum),FLMAX(ncolum)
        DIMENSION RGFMIN(ncolum),RGFMAX(ncolum)
        DIMENSION RGVLMIN(ncolum),RGVLMAX(ncolum)
        DIMENSION RGVHMIN(ncolum),RGVHMAX(ncolum)
        DIMENSION CSMIN(ncolum),CSMAX(ncolum)
        DIMENSION RTYPEMIN(ncolum),RTYPEMAX(ncolum)

        DIMENSION IRANGEMEAN(ncolum)

	CHARACTER*80	ZRFILE,LINE
        REAL      KMP
	INTEGER VERBOSE
	REAL*8 DAY

        COMMON/CREADZR/IFIRST,RI,RMP,RANGEMIN,RANGEMAX,BBMIN,
     +  BBMAX,EEMIN,EEMAX,RGBMIN,RGBMAX,FLMIN,FLMAX,RGFMIN,RGFMAX,
     +  RGVLMIN,RGVLMAX,RGVHMIN,RGVHMAX,CSMIN,CSMAX,RTYPEMIN,RTYPEMAX,
     +  NCOL,IFREEZLEVEL,IRANGEMEAN,NDAYINPINT,NDAYCALINT

	DIMENSION RANGELIMITS(2),EELIMITS(2),RGBLIMITS(2),FLLIMITS(2)
	DIMENSION RGFLIMITS(2),RGVLLIMITS(2),RGVHLIMITS(2)
	DIMENSION INTPAR(10)
 	DIMENSION DAYCAL1(0:100),DAYCAL2(0:100),DELTADBZ(0:100)
	REAL*8 DAYSTART(0:100),DAYEND(0:100),DECDAY

        DATA IFIRST/1/

        IF(IFIRST.EQ.1)THEN
	  IF(VERBOSE.EQ.1)WRITE(6,*)'READZR: open ZRFILE ',ZRFILE
C *****   OPEN THE OPTION FILE
          IFIRST=0
C         OPEN(UNIT=1,FILE='readzr.inp',STATUS='OLD',READONLY,ERR=1001)
C	  READ(INPUNIT,201) ZRFILE
 201      FORMAT(A50)
	  WRITE(6,*)'KMP,ALPHA,MPONLY=',KMP,ALPHA,MPONLY
          IF(MPONLY.EQ.0)THEN
	    WRITE(6,204) ZRFILE
 204	    FORMAT(' Read Ze-R from ',A50)
	    OPEN(UNIT=12,FILE=ZRFILE,STATUS='OLD',ERR=1002)

	NDAYINPINT=0
        DO WHILE(DAYSTART(NDAYINPINT).GT.-999)
	   NDAYINPINT=NDAYINPINT+1
	   READ(12,*)DAYSTART(NDAYINPINT),DAYEND(NDAYINPINT)
	   IF(DAYSTART(NDAYINPINT).GT.-999)THEN
	      IF(VERBOSE.EQ.1)Write(6,*)'Reject between days ',
     +        DAYSTART(NDAYINPINT),DAYEND(NDAYINPINT)
	      ENDIF
        ENDDO
	NDAYINPINT=NDAYINPINT-1


      NDAYCALINT=0
        DO WHILE(DAYCAL1(NDAYCALINT).GT.-999)
	   NDAYCALINT=NDAYCALINT+1
           READ(12,*)DAYCAL1(NDAYCALINT),DAYCAL2(NDAYCALINT),
     +	   DELTADBZ(NDAYCALINT)
	   IF(DAYCAL1(NDAYCALINT).GT.-999)THEN
	      IF(VERBOSE.EQ.1)WRITE(6,*)'ZR: Add',DELTADBZ(NDAYCALINT),
     +        ' dBZ between days',
     +	      DAYCAL1(NDAYCALINT),DAYCAL2(NDAYCALINT)
	      ENDIF
        ENDDO
	NDAYCALINT=NDAYCALINT-1
c	write(6,*)'ZR: NDAYINPINT,NDAYCALINT=',NDAYINPINT,NDAYCALINT


            READ(12,214)RADLAT,RADLONG
 214        FORMAT(13X,2F10.3)
            READ(12,215)IYB,IDAB,IHRB,IMINB,IYE,IDAE,IHRE,IMINE
 215        FORMAT(6X,I4,6X,I3,6X,I2,1X,I2,4X,6X,I4,6X,I3,6X,I2,1X,I2)
            READ(12,*)HINP,PINP,TINP,DPINP,CBASE	!added 6/8/93
            READ(12,211)NPARAMETER
 211        FORMAT(13X,I2)
            READ(12,212)INTPAR
 212        FORMAT(13X,10I3)
	    NCOL=1
	    DO I=1,NPARAMETER
              NCOL=NCOL*INTPAR(I)
              READ(12,'(A80)')LINE
	    ENDDO
            READ(12,213)(RANGEMIN(I),RANGEMAX(I),I=1,NCOL)
 213        FORMAT(13X,200F10.3)
            READ(12,213)(BBMIN(I),BBMAX(I),I=1,NCOL)
            READ(12,213)(EEMIN(I),EEMAX(I),I=1,NCOL)
            READ(12,213)(RGBMIN(I),RGBMAX(I),I=1,NCOL)
            READ(12,213)(FLMIN(I),FLMAX(I),I=1,NCOL)
            READ(12,213)(RGFMIN(I),RGFMAX(I),I=1,NCOL)
            READ(12,213)(RGVLMIN(I),RGVLMAX(I),I=1,NCOL)
            READ(12,213)(RGVHMIN(I),RGVHMAX(I),I=1,NCOL)
            READ(12,213)(CSMIN(I),CSMAX(I),I=1,NCOL)
            READ(12,213)(RTYPEMIN(I),RTYPEMAX(I),I=1,NCOL)
	  
	  IF(VERBOSE.EQ.1)THEN
	    write(6,*)'column,parameter,min,max,NCOL=',NCOL
	    write(6,*)'range=',(RANGEMIN(I),RANGEMAX(I),I=1,NCOL)
	    write(6,*)'BB   =',(BBMIN(I),BBMAX(I),I=1,NCOL)
	    write(6,*)'EE   =',(EEMIN(I),EEMAX(I),I=1,NCOL)
	    write(6,*)'RGB  =',(RGBMIN(I),RGBMAX(I),I=1,NCOL)
	    write(6,*)'FL   =',(FLMIN(I),FLMAX(I),I=1,NCOL)
	    write(6,*)'RGF  =',(RGFMIN(I),RGFMAX(I),I=1,NCOL)
	    write(6,*)'RGVL =',(RGVLMIN(I),RGVLMAX(I),I=1,NCOL)
	    write(6,*)'RGVH =',(RGVHMIN(I),RGVHMAX(I),I=1,NCOL)
	    write(6,*)'CS   =',(CSMIN(I),CSMAX(I),I=1,NCOL)
	    write(6,*)'RTYPE=',(RTYPEMIN(I),RTYPEMAX(I),I=1,NCOL)
	    ENDIF

 	    RANGELIMITS(1)=RANGEMIN(1)
	    RANGELIMITS(2)=RANGEMAX(NCOL)
	    EELIMITS(1)=EEMIN(1)
	    EELIMITS(2)=EEMAX(NCOL)
	    RGBLIMITS(1)=RGBMIN(1)
	    RGBLIMITS(2)=RGBMAX(NCOL)
	    FLLIMITS(1)=FLMIN(1)
	    FLLIMITS(2)=FLMAX(NCOL)
	    RGFLIMITS(1)=RGFMIN(1)
	    RGFLIMITS(2)=RGFMAX(NCOL)
	    RGVLLIMITS(1)=RGVLMIN(1)
	    RGVLLIMITS(2)=RGVLMAX(NCOL)
	    RGVHLIMITS(1)=RGVHMIN(1)
	    RGVHLIMITS(2)=RGVHMAX(NCOL)

            DO I=1,NCOL
              RI(0,I)=0
              IRANGEMEAN(I)=(RANGEMIN(I)+RANGEMAX(I))/2.+0.5
            ENDDO

            DO 200 N=1,700
              READ(12,117)DBZ,(RI(N,I),ICOUNT,I=1,NCOL)
 117          FORMAT(F13.1,100(F10.1,I10))
              DO I=1,NCOL
                IF(RI(N,I).EQ.0 .AND. RI(N-1,I).NE.0)RI(N,I)=RI(N-1,I)
              ENDDO
 200        CONTINUE
          ELSE
	    WRITE(6,202)KMP,ALPHA
 202        FORMAT(' Using only MP with  K=',F4.0,'  ALPHA=',F4.2)
          ENDIF

          IF (KMP .EQ. 0) THEN 
             WRITE(7, *) 'KMP = 0. UNABLE TO CONTINUE.'
             CALL EXIT
          ENDIF
          A=10*ALOG10(KMP)


          IF (ALPHA .EQ. 0) THEN 
             WRITE(7, *) 'ALPHA = 0. UNABLE TO CONTINUE.'
             CALL EXIT
          ENDIF
          DO 328 I=0,700
            RMP(I)=10.**(0.1/ALPHA*(I/10.-A))
 328      CONTINUE
          RETURN
        ENDIF


	IDBZ=IDBZ1

	DO N=1,NDAYINPINT
	IF(DAY.GE.DAYSTART(N).AND.DAY.LE.DAYEND(N))IDBZ=0
	ENDDO

	DO N=1,NDAYCALINT
	IF(DAY.GE.DAYCAL1(N).AND.DAY.LE.DAYCAL2(N))THEN
	   IF(IDBZ.NE.0)IDBZ=IDBZ1+DELTADBZ(N)*10
	   GOTO 4
	   ENDIF
	ENDDO

 4        IF(MPONLY.EQ.0)THEN
          I=0
 118      I=I+1 
          IF(I.EQ.NCOL+1)THEN
            WRITE(6,120)IRANGE,BB,EE,RGB,IFL,RGF,RGVL,RGVH,ICS,IRTYPE,
     +                  IDBZ
 120        FORMAT(' Classification parameters out of range:Range=',I6,'
     +      BB=',F6.2,'    EE=',F6.2,'   RGB=',F6.2,'FL=',I7,'   RGF=',
     +      F6.2,'  RGVL=',F6.2,'  RGVH=',F6.2,'   ICS=',I4,'  IRTYPE=',
     +      I4,'  IDBZ=',I4)

	    do j=1,NCOL
	      write(6,*)'column,parameter,min,max,current'
	      write(6,*)j,' range =',RANGEMIN(j),RANGEMAX(j),IRANGE
	      write(6,*)j,' BB    =',BBMIN(j),BBMAX(j),BB
	      write(6,*)j,' EE    =',EEMIN(j),EEMAX(j),EE
	      write(6,*)j,' RGB   =',RGBMIN(j),RGBMAX(j),RGB
	      write(6,*)j,' FL    =',FLMIN(j),FLMAX(j),IFL
	      write(6,*)j,' RGF   =',RGFMIN(j),RGFMAX(j),RGF
	      write(6,*)j,' RGVL  =',RGVLMIN(j),RGVLMAX(j),RGVL
	      write(6,*)j,' RGVH  =',RGVHMIN(j),RGVHMAX(j),RGVH
	      write(6,*)j,' CS    =',CSMIN(j),CSMAX(j),ICS
	      write(6,*)j,' RTYPE =',RTYPEMIN(j),RTYPEMAX(j),IRTYPE
	    enddo

            STOP 'Classification parameters out or range. -- FATAL'
          ENDIF

          if(1.eq.0)then
	    IF(IRANGE.LT.RANGEMIN(I).OR.IRANGE.GT.RANGEMAX(I))GOTO 118
            IF(BB.LT.BBMIN(I).OR.BB.GT.BBMAX(I))GOTO 118
            IF(EE.LT.EEMIN(I) .OR. EE.GT.EEMAX(I))GOTO 118
            IF(RGB.LT.RGBMIN(I) .OR. RGB.GT.RGBMAX(I))GOTO 118
            IF(IFL.LT.FLMIN(I) .OR. IFL.GT.FLMAX(I))GOTO 118
          endif   !(1.eq.0)then

	  IF(IRANGE.LT.RANGEMIN(I).OR.IRANGE.GT.RANGEMAX(I))GOTO 118

          IF(BBMAX(I).EQ.BBMAX(NCOL))THEN
	    IF(BB.LT.BBMIN(I).OR.BB.GT.BBMAX(I))GOTO 118
          ELSE
	    IF(BB.LT.BBMIN(I).OR.BB.GE.BBMAX(I))GOTO 118
          ENDIF

          IF(EEMAX(I).EQ.EEMAX(NCOL))THEN
	    IF(EE.LT.EEMIN(I).OR.EE.GT.EEMAX(I))GOTO 118
          ELSE
	    IF(EE.LT.EEMIN(I).OR.EE.GE.EEMAX(I))GOTO 118
          ENDIF

          IF(RGBMAX(I).EQ.RGBMAX(NCOL))THEN
            IF(RGB.LT.RGBMIN(I) .OR. RGB.GT.RGBMAX(I))GOTO 118
          ELSE
            IF(RGB.LT.RGBMIN(I) .OR. RGB.GE.RGBMAX(I))GOTO 118
          ENDIF

          IF(FLMAX(I).EQ.FLMAX(NCOL))THEN
            IF(IFL.LT.FLMIN(I) .OR. IFL.GT.FLMAX(I))GOTO 118
          ELSE
            IF(IFL.LT.FLMIN(I) .OR. IFL.GE.FLMAX(I))GOTO 118
          ENDIF

          IF(RGFMAX(I).EQ.RGFMAX(NCOL))THEN
            IF(RGF.LT.RGFMIN(I) .OR. RGF.GT.RGFMAX(I))GOTO 118
          ELSE
            IF(RGF.LT.RGFMIN(I) .OR. RGF.GE.RGFMAX(I))GOTO 118
          ENDIF

          IF(RGVLMAX(I).EQ.RGVLMAX(NCOL))THEN
            IF(RGVL.LT.RGVLMIN(I) .OR. RGVL.GT.RGVLMAX(I))GOTO 118
          ELSE
            IF(RGVL.LT.RGVLMIN(I) .OR. RGVL.GE.RGVLMAX(I))GOTO 118
          ENDIF

          IF(RGVHMAX(I).EQ.RGVHMAX(NCOL))THEN
            IF(RGVH.LT.RGVHMIN(I) .OR. RGVH.GT.RGVHMAX(I))GOTO 118
          ELSE
            IF(RGVH.LT.RGVHMIN(I) .OR. RGVH.GE.RGVHMAX(I))GOTO 118
          ENDIF

          IF(ICS.LT.CSMIN(I) .OR. ICS.GT.CSMAX(I))GOTO 118

          IF(IRTYPE.LT.RTYPEMIN(I) .OR. IRTYPE.GT.RTYPEMAX(I))GOTO 118

          NCOLUMN=I
          RR=RI(IDBZ,NCOLUMN)

CCCCCCCCCCCCCCCCCCCCCCCCCCC
          NDELCOL=NCOL/INTPAR(1)
          NCOL1=I-NDELCOL
          NCOL2=I+NDELCOL
          IF(IRANGE.LT.IRANGEMEAN(NCOLUMN) .AND. NCOL1.GE.1)THEN
            XX1=IRANGEMEAN(NCOL1)
	    Z1=RI(IDBZ,NCOL1)
	    XX2=IRANGE
            XX3=IRANGEMEAN(NCOLUMN)
            Z3=RI(IDBZ,NCOLUMN)
            CALL INTERPOL(XX1,Z1,XX2,Z2,XX3,Z3)
            RR=Z2
          ELSE
            IF(IRANGE.GT.IRANGEMEAN(I) .AND. NCOL2.LE.NCOL)THEN
              XX1=IRANGEMEAN(NCOLUMN)
	      Z1=RI(IDBZ,NCOLUMN)
	      XX2=IRANGE
              XX3=IRANGEMEAN(NCOL2)
              Z3=RI(IDBZ,NCOL2)
              CALL INTERPOL(XX1,Z1,XX2,Z2,XX3,Z3)
              RR=Z2
            ENDIF
          ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCC
        ENDIF

        RRMP=RMP(IDBZ)
c	write(6,*)IDBZ,RGVL,RRMP,RR
        RETURN
 1001   STOP 'CANT OPEN READZR.INP FILE'
 1002   STOP 'CANT OPEN INPUT Ze-R DATA FILE'
        END
