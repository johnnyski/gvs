	SUBROUTINE POL2CART(RAINMATP,RAINMATC,IDELTAZ)

	INCLUDE 'voswin.par'

	REAL RAINMATP(0:rays,kms),RAINMATC(ydim,xdim)

	CALL SETPOLCART(IDELTAZ)

	J=0
	DO JX=1,nxpage,4
	JX4=JX+3
	J=J+1
	I=0
	  DO IY=1,nypage,4
	  IY4=IY+3
	  I=I+1
	  R=0
	  N=0
* Initialize cartesean rainmap field to -99 -- out-of-bounds (OOB)
	  RAINMATC(I,J)=-99 
	    DO JJ=JX,JX4
	    DO II=IY,IY4
	    IAZ=MAZCART(JJ,II)
	    IR = MRCART(JJ,II)
	    IF(IR.GT.0)THEN
	      N=N+1
	      R=R+RAINMATP(IAZ,IR)
	      ENDIF
	    ENDDO
	    ENDDO
	  IF(N.GT.0)RAINMATC(I,J)=R/N
	  ENDDO
	ENDDO

	if(0.eq.1)then
	j1=45	! X
	j2=j1+19
	i1=55	! Y
	i2=i1+19

	write(6,*)
	write(6,*)'RAIN 2x2 km cartesian-field'
	do j=j1,j2
	write(6,'(i3,24F5.1)')j,(RAINMATC(i,j),i=i1,i2)
	enddo
	write(6,'(3x,24i5)')(i,i=i1,i2)
	endif  ! (0.eq.1)

	RETURN
	END

	SUBROUTINE SETPOLCART(IDELTAZ)

	INCLUDE 'voswin.par'
C  The following statements are included in 'voswin.par'
C	PARAMETER (nypage=600, nxpage=600)
C	INTEGER xdim,ydim
C	PARAMETER (xdim=nxpage/4, ydim=nypage/4)
C	PARAMETER (fine_size = 0.5) 	! Cartesian grid size [km]
C	COMMON/SETMAP/MRCART(nypage,nxpage),MAZCART(nypage,nxpage)
C	INTEGER*2 MRCART,MAZCART

	COMMON /LSETMAP/IFIRST
	DATA IFIRST/0/

	IF(IFIRST.EQ.1)RETURN
	IFIRST=1

	BINLENGTH=1
	IXCENT=nxpage/2
	JYCENT=nypage/2
c	WRITE(6,*)'RADAR CENTER (X,Y OF PAGE MAP)=',IXCENT,JYCENT
	BLOWX=BINLENGTH/fine_size
	BLOWY=BLOWX
c	WRITE(6,*)'BLOWUP FACTOR=',BLOWX,BLOWY
C
C   POLAR TO CARTESIAN
C
      DO 80 JY=1,nypage
      DY=JY-JYCENT
      IF(DY.EQ.0)DY=0.00001
      DY=DY/BLOWY
      DO 70 IX=1,nxpage
      MRCART(JY,IX)=0
      MAZCART(JY,IX)=0
      DX=(IX-IXCENT)/BLOWX
      A=ATAN(DX/DY)/0.0174533
      IF(DY.LT.0)THEN
         A=360-A
      ELSE
         A=180-A
         ENDIF
      R=SQRT(DX*DX+DY*DY)
      IA=A+0.5
      IF(IA.EQ.0)IA=360
      IF(IA.GT.360)IA=IA-360
      IR=R+.5
	IF(IR.EQ.0)IR=1

* Checking against xdim and ydim force OOB assignments.

	IF(IA.LT.0.OR.IA.GT.360.OR.IR.LT.0.OR.IR.GT.xdim .or.
     +	 ir.gt.ydim)GOTO 70
	IF(IDELTAZ.EQ.14)THEN
	   IA=A*256./360.+0.5
           IF(IA.EQ.0)IA=256
           IF(IA.GT.256)IA=IA-256
	   ENDIF
	 MRCART(JY,IX)=IR
	 MAZCART(JY,IX)=IA
 70	CONTINUE
 80	CONTINUE
	IF(VERBOSE.EQ.1)
     +	WRITE(6,*)'Finish initialization of polar to cartesian matrix'
	RETURN
	END
