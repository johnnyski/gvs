	SUBROUTINE MDS(ZMDS,NEWMDS)

C  Calculate the minimum detectable reflectivity as
C  a function of range.

	INCLUDE 'voswin.par'

	COMMON /CMDS/ MDSAVE(kms)
	DIMENSION MDS2(kms),ZMDS(kms)

	DO I=1,kms
	MDS2(I)=999
	ENDDO

	DO ISTEP=1,NSTEPS
	DO IRAY=1,rays
	DO IKM=1,kms
	IZ=Z(IRAY,IKM,ISTEP)
	IF(IZ.NE.0.AND.IZ.LT.MDS2(IKM))MDS2(IKM)=IZ
	ENDDO
	ENDDO
	ENDDO


	MINMDS=MDS2(kms)
	DO IKM=kms,1,-1
	IF(MINMDS.LT.MDS2(IKM))THEN
	   MDS2(IKM)=MINMDS
	ELSE
	   MINMDS=MDS2(IKM)
	   ENDIF
	ENDDO

	IF(0.EQ.1)THEN
	DO I=1,kms
	IF(MDSAVE(I).NE.999)WRITE(6,*)I,MDS2(I)/10.,MDSAVE(I)/10.
	ENDDO
	ENDIF

C   check if sensitivity is reduced by testing whther MDS2
C   is greater than MDSAVE.  If so, save the MDS2.
	IF(MDS2(100).GT.MDSAVE(100)+10.OR.NEWMDS.EQ.1)THEN
	   IF(VERBOSE.EQ.1)WRITE(6,*)' Use new MDS set'
c	write(6,*)
c     +	'MDS2(100),MDSAVE(100),NEWMDS=',MDS2(100),MDSAVE(100),NEWMDS
	   DO I=1,kms
	   IF(MDS2(I).NE.999)MDSAVE(I)=MDS2(I)
	   ENDDO
	ELSE
C   update MDSAVE with possibly lower values
	   WRITE(6,*)' Update existing MDS set'
	   DO I=1,kms
	   IF(MDS2(I).LT.MDSAVE(I).OR.MDSAVE(I).EQ.0)MDSAVE(I)=MDS2(I)
	   ENDDO
	   ENDIF

	DO I=1,kms
	IF(MDSAVE(I).NE.999)THEN
	   ZMDS(I)=MDSAVE(I)/10.
	ELSE
	   ZMDS(I)=0
	   ENDIF
	ENDDO

	RETURN
	END
