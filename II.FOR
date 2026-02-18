C
	EXTERNAL UFO_OPEN
C
C
	CHARACTER TWELV*1/12/,PUT*3,AINTV/'1'/,
	1	PO*3/'[5i'/,POF*3/'[4i'/,ES/27/,STR*4,STRL*4
C
ccccccccccccccccccccccccccccccccc
	INTEGER*4	DATETIME,LEN,WBILL,PLANT,CARS,NUM
	INTEGER*4	OCARS,UCARS,CUT1(6),CUT2(6) 
	INTEGER*4	CUT4(6),CARSX,SPLITC(2)
	INTEGER*4	NUMX,PLANTX,DESTX,CHECKN,CHECKNX
	INTEGER*4	FLUXL(16),FLUXH(16),ACIDL(16),ACIDH(16)
C
cccccccccccccccccccccccccccccccccc
C
C
	DOUBLE PRECISION SILL,MOIST,IRON,TONS,TTT(4),TTTX(4)
C
C
C
	DOUBLE PRECISION SX(21),SX2(21),MEAN(21),SMB1,SMA1,SMB2,SMA2,SD(21),
	1	MEANU(21)
C
	DOUBLE PRECISION MCC(150),OIC(150),NM(200),NY(200),TEMP,
	1	MCC3(150),OIC3(150),NM3(200),NY3(200)
C
	INTEGER N(21),MNTH,DAT(2),IS,IH,S4,H4,ENDB(2),DAYB(2)
C
	CHARACTER HSKEY*4,YR*4,KY*4,KD*6,enda*11,FILE*23,DAYX/'1'/,
	1		NTK*7,EK*7,SCEK*6
C
C
 	DOUBLE PRECISION SCREEN(14,3,2,5),COMP(2,3,2,5),
	1  CAB(2,3,5),SIL(3,5),FILT2(5,3),FILT3(5,3),BL2,BL3,
	1  FLUXG,FCA(2,3),FMG(2,3),FAL(2,3),FSI(2,3),
	1  PA(3,5),PC(3,5),PM(3,5),X
C
	CHARACTER MON(12)*3/'JAN','FEB','MAR','APR','MAY','JUN','JUL',
	1			'AUG','SEP','OCT','NOV','DEC'/
c
	structure /bozo/
c
		character*4	key		! yymm	(primary key)
		real		msil(3,31)	! mine ind. sio2 by shift,day
		real		cr12(3,31)	! crusher +1/2 by shift,day
c
	end structure
c
	record /bozo/ hsd
C
c
	structure /bozo2/
c
		character*4	key		! yymm	(primary key)
		real		msil(3,31)	! mine ind. sio2 by shift,day
		real		cr12(3,31)	! crusher +1/2 by shift,day
		real 		flfr(3,31)	! flf davis tube recovery
		real		flfs(3,31)	! flf davis tube sio2
		real		fls(3,31)	! flf sio2
		real		flts(3,31)	! flot tails sio2
		real		aeff(4,3,31)	! amine efficiency by cell
c
c
	end structure
c
	record /bozo2/ hs2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
	STRUCTURE /X/
		CHARACTER 	KEY*7   	!STEP, YYMMDD
		REAL		POINTS(4,50)	! NOLA VALUES
		REAL		TARG(4,50)	! TARGET VALUES
		INTEGER		DATIM(2,4,50)	!binary time for each sample
		INTEGER		K		!daily entry count (1-50)
		INTEGER 	ME		! END OF MONTH INDICATOR
	END STRUCTURE
C
	RECORD /X/ NOL
C
C
C
C
C
C
	OPEN (UNIT=91,FILE='MET_P:HALFSIL.DAT',STATUS='OLD',
	1	ACCESS='KEYED',SHARED,ERR=88)
	OPEN (UNIT=92,FILE='MET_P:HALFSIL2.DAT',STATUS='OLD',
	1	ACCESS='KEYED',SHARED,ERR=88)
C
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
	CALL SYS$BINTIM (DAYX,DAYB)
C
C
C
	KY='9201'
C
C
	read (91,keyge=ky)hsd
	go to 44
c
40	read (91,err=200) hsd
c
44	read (92,key=hsd.key,err=11) hs2
11	hs2.key=hsd.key
	do 55 id=1,31
	do 55 is=1,3 
		hs2.msil(is,id)=hsd.msil(is,id)
		hs2.cr12(is,id)=hsd.cr12(is,id)
55	continue
C
	write (92) hs2
	go to 40
C
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
C
C
200	STOP
88	stop ' open error'
	END
