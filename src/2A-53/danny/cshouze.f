        SUBROUTINE CSHOUZE(MATPZ,CSMAP,MAPCS,MAPCSP,IDELTAZ)
C   Calculate convective/stratiform according to Stiener and Houze
        INCLUDE 'voswin.par'
        PARAMETER (convective=2, stratiform=1)

        INTEGER*2 MATPZ(0:rays,kms)
	INTEGER*2 CSMAP(151,151)
        REAL CARTZ(ydim,xdim)

	BYTE MAPCS(ydim,xdim),MAPCSP(rays,kms)

        DIMENSION IDX(-5:5)
        DATA IDX/2,3,4,5,5,5,5,5,4,3,2/

c        write(6,*)'STIENER-HOUZE classification'

        CALL ZPOL2CART(MATPZ,CARTZ,IDELTAZ)

	INCSMAP=0
	DO J=1,151
	DO I=1,151
	IF(CSMAP(I,J).NE.0)INCSMAP=1
	I1=152-I
	MAPCS(J,I)=CSMAP(J,I1)
	IF(MAPCS(J,I).EQ.-99)MAPCS(J,I)=0
	ENDDO
	ENDDO

	IF(INCSMAP.EQ.0)THEN
           DO J=1,ydim
           DO I=1,xdim
           MAPCS(I,J)=0
	   ENDDO
	   ENDDO

           DO 10 J=1,ydim
           DO 20 I=1,xdim
           IF(CARTZ(I,J).EQ.0)GOTO 20
           IF(MAPCS(I,J).NE.convective)MAPCS(I,J)=stratiform

C  calculate the average Z inside the 11 km radius
           N=0
           ZSUM=0
           DO 30 JJ=-5,5
           JJJ=J+JJ
           IF(JJJ.LT.1.OR.JJJ.GT.ydim)GOTO 30
           I1=I-IDX(JJ)
           I2=I+IDX(JJ)
           DO 40 II=I1,I2
           IF(II.LT.1.OR.II.GT.xdim)GOTO 40
           IF(CARTZ(II,JJJ).EQ.0)GOTO 40
           N=N+1
           ZSUM=ZSUM+CARTZ(II,JJJ)
 40        CONTINUE
 30        CONTINUE
           ZAVR=0
           IF(N.GT.0)ZAVR=ZSUM/N
           IF(CARTZ(I,J).LT.ZAVR.AND.CARTZ(I,J).LT.10000)GOTO 20
           DBZAVR=10*ALOG10(ZAVR)
           DBZ=10*ALOG10(CARTZ(I,J))
           IF(DBZ.LT.0)THEN
              DZ=10
           ELSE IF(IDBZ.GT.42.43)THEN
              DZ=0
           ELSE
              DZ=10-(DBZ**2)/180.
              ENDIF

           IF(DBZ.GE.40)MAPCS(I,J)=convective
           IF(DBZ-DBZAVR.GT.DZ)THEN
C  mark the convective core
              MAPCS(I,J)=convective
              RADIUS=(DBZ-17.5)/5.
              DO 130 JJ=-5,5
              JJJ=J+JJ
              IF(JJJ.LT.1.OR.JJJ.GT.ydim)GOTO 130
              DO 140 II=-IDX(JJ),IDX(JJ)
              III=I+II
              IF(III.LT.1.OR.III.GT.xdim)GOTO 140
              DR=SQRT((JJ+0.5)**2+(II+0.5)**2)
              IF(DR.LE.RADIUS)MAPCS(III,JJJ)=convective
 140          CONTINUE
 130          CONTINUE
              ENDIF

 20        CONTINUE
 10        CONTINUE
	   ENDIF

	CALL CART2POL(MAPCS,MAPCSP)

        RETURN
        END


	SUBROUTINE ZPOL2CART(MATPZ,CARTZ,IDELTAZ)

	INCLUDE 'voswin.par'

	INTEGER*2 MATPZ(0:rays,kms)
	REAL CARTZ(ydim,xdim)

	CALL SETPOLCART(IDELTAZ)

	J=0
	DO JX=1,nxpage,4
	JX4=JX+3
	J=J+1
	I=0
	  DO IY=1,nypage,4
	  IY4=IY+3
	  I=I+1
	  ZZ=0
	  N=0
	  CARTZ(I,J)=0
	    DO JJ=JX,JX4
	    DO II=IY,IY4
	    IAZ=MAZCART(JJ,II)
	    IR = MRCART(JJ,II)
	    IF(IR.GT.0)THEN
	      N=N+1
	      ZZ=ZZ+DBZ102Z(MATPZ(IAZ,IR))
	      ENDIF
	    ENDDO
	    ENDDO
c	  IF(N.GT.0)CARTZ(I,J)=10*ALOG10(ZZ/N)
	  IF(N.GT.0)CARTZ(I,J)=ZZ/N
	  ENDDO
	ENDDO

	if(0.eq.1)then
	j1=45	! X
	j2=j1+19
	i1=55	! Y
	i2=i1+19

	write(6,*)
	write(6,*)'CARTZ 2x2 km cartesian-field'
	do j=j1,j2
	write(6,'(i3,24F5.1)')j,(CARTZ(i,j),i=i1,i2)
	enddo
	write(6,'(3x,24i5)')(i,i=i1,i2)
	endif  ! (0.eq.1)

	RETURN
	END


	SUBROUTINE CART2POL(MAPCS,MAPCSP)
	INCLUDE 'voswin.par'

	BYTE MAPCS(ydim,xdim),MAPCSP(rays,kms)

	XCENTER=xdim/2.
	YCENTER=ydim/2.

	DO IRAY=1,rays
	DO IKM=1,151
	MAPCSP(IRAY,IKM)=0
	ENDDO
	ENDDO

	DEG2RAD=3.141592/180.
	DO IRAY=1,rays
	DO IKM=1,151
	RANGE2=IKM/2.
	AZ=IRAY*DEG2RAD
	X=XCENTER+RANGE2*SIN(AZ)
	Y=YCENTER-RANGE2*COS(AZ)
	IX=X+0.5
	IY=Y+0.5
	IF(IX.GT.0.AND.IX.LE.xdim.AND.IY.GT.0.AND.IY.le.ydim)THEN
	   IF(MAPCSP(IRAY,IKM).EQ.0)MAPCSP(IRAY,IKM)=MAPCS(IX,IY)
	   ENDIF
	ENDDO
	ENDDO

	RETURN
	END
