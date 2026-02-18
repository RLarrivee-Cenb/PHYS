CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       CONFIDENTIAL
C       Property of United States Steel Corporation
C       Copyright 2010 United States Steel Corporation
C       All rights reserved.
C******************************************************************************
C               MMC.FOR
C	GATHERS INFO FOR STRUKEL AND WRITES IT TO FORMATTED FILE 
C	'MET_P:MMC.DATA'.
C	The crusher info in mmc.dat is written once a day from 
C	'met_P:metcol.for'.  some flotation data is written once a day
C	from 'met_p:tails-vt2.for'.  Other data is written in by
C	[metrep.lab45]mt3test.for and met_P:mmc_bent.for
C******************************************************************************
C Revisions:
C       20100809 IM322852  SPA6635 changed COMP2(2,3,2,5) to COMP2(3,3,2,5)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
	CHARACTER ANS*9,AVL*6,ANS2,KD*8
c
c
C	  this structure is in MMC.DAT, the key is a DESCENDING string
	STRUCTURE /XX/
		CHARACTER DATE*7/'       '/	!YYMMDDS    key 0
		REAL	  FFDATA(4)	! AL2O3,CAO,MGO,MNO
		REAL	  FCDATA(7)	!SIL,AL,CAL,MG,MN,-270
		REAL	  BENT(5)	!BENT. LBS/TON
		REAL	  CTONS         !CRUSHER SCANNER TONS
		REAL 	  CSIL          !MINE IND. SILICA
		REAL	  CMF           !MINE IND. MAG FE
		REAL	  CWTR          !MINE PREDIC. RECOVERY
		REAL	  CA            !MINE "A" FACTOR
		REAL 	  CT		!AVG. COARSE TAILS
		REAL	  FT		!AVG. FINE TAILS
		REAL 	  FR		!FROTH REGRIND % OP TIME
		REAL	  AM		!AMINE LBS/TON
	END STRUCTURE
C
	RECORD /XX/ DATA
c
C
C
	integer SHIFT
C
C
 	DOUBLE PRECISION CT(18),FT(18),SCREEN(14,3,2,5),COMP(2,3,2,5),
	1  CAB(2,3,5),SIL(3,5),FILT2(5,3),BL2,FILT3(5,3),BL3,TOTAL,
	1  H2O,RMF,FTMF,BOIC(150),HOIC(150),CX(5),MX(5),AX(5),SX(5),
	1  FLUXG,P34,P12,FCA(2,3),FMG(2,3),FAL(2,3),FSI(2,3),
	1  PA(3,5),PC(3,5),PM(3,5),SCWT(3,2,5),SCREEX(14,3,2,5),
c	1  WWT(5),TONS4(3,2,5),GHRS(3,2,5),COMP2(2,3,2,5)
 	1  WWT(5),TONS4(3,2,5),GHRS(3,2,5),COMP2(3,3,2,5)
C
C
C
C
	CHARACTER MON(13)*3/'JAN','FEB','MAR','APR','MAY','JUN','JUL',
	1'AUG','SEP','OCT','NOV','DEC','   '/,A6*6,AONE/'1'/,AT*11
C
	INTEGER BD(2),BONE(2),DAT(2)
C
	CHARACTER KEY*7
C
C
C
C
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
	record /bozo2/ hsd
c
C
C
	CHARACTER*60 LABEL(79)
C
	DATA LABEL /
	1'TOTAL CRUDE TONS                                            ',
	1'MINE INDICATED SILICA                                       ',
	1'MINE INDICATED MAG IRON                                     ',
	1'MINE PREDICTED RECOVERY                                     ',
	1'"A" FACTOR                                                  ',
	1'BLANK                                                       ',
	1'BLANK                                                       ',
	1'BLANK                                                       ',
	1'BLANK                                                       ',
	1'BLANK                                                       ',
	1'FLOTATION FEED SILICA                                       ',
	1'FLOTATION FEED AL2O3                                        ',
	1'FLOTATION FEED CAO                                          ',
	1'FLOTATION FEED MGO                                          ',
	1'FLOTATION FEED MN                                           ',
	1'FLOTATION FEED %-270 MESH                                   ',
	1'FLOTATION FEED DT %SIO2                                     ',
	1'FLOTATION FEED DT % RECOVERY                                ',
	1'COARSE TAILS % MAG IRON                                     ',
	1'FINE TAILS % MAG IRON                                       ',
	1'FLOTATION TAILS % SIO2                                      ',
	1'FROTH REGRIND % OPERATING TIME                              ',
	1'FLOTATION CONCENTRATE % SIO2                                ',
	1'FLOTATION CONCENTRATE % AL2O3                               ',
	1'FLOTATION CONCENTRATE % CAO                                 ',
	1'FLOTATION CONCENTRATE % MGO                                 ',
	1'FLOTATION CONCENTRATE % MN                                  ',
	1'FLOTATION CONCENTRATE % -270 MESH                           ',
	1'AMINE #/TON                                                 ',
	1'STEP 2 FILT CAKE CA/MG                                      ',
	1'STEP 3 FILT CAKE CA/MG                                      ',
	1'BLANK                                                       ',
	1'LINE 3  FIRED PELLET BT % +1/2                              ',
	1'LINE 3  FIRED PELLET BT % +1/4                              ',
	1'LINE 3  FIRED PELLET AT % +1/4                              ',
	1'LINE 3  FIRED PELLET COMPRESSIVE STRENGTH- LBS.             ',
	1'LINE 3  FIRED PELLET % -200 LB COMPRESSIONS                 ',
	1'LINE 3  BENTONITE LBS/TON                                   ',
	1'LINE 4  FIRED PELLET BT % +1/2                              ',
	1'LINE 4  FIRED PELLET BT % +1/4                              ',
	1'LINE 4  FIRED PELLET AT % +1/4                              ',
	1'LINE 4  FIRED PELLET COMPRESSIVE STRENGTH- LBS.             ',
	1'LINE 4  FIRED PELLET % -200 LB COMPRESSIONS                 ',
	1'LINE 4  BENTONITE LBS/TON                                   ',
	1'LINE 5  FIRED PELLET BT % +1/2                              ',
	1'LINE 5  FIRED PELLET BT % +1/4                              ',
	1'LINE 5  FIRED PELLET AT % +1/4                              ',
	1'LINE 5  FIRED PELLET COMPRESSIVE STRENGTH- LBS.             ',
	1'LINE 5  FIRED PELLET % -200 LB COMPRESSIONS                 ',
	1'LINE 5  BENTONITE LBS/TON                                   ',
	1'LINE 6  FIRED PELLET BT % +1/2                              ',
	1'LINE 6  FIRED PELLET BT % +1/4                              ',
	1'LINE 6  FIRED PELLET AT % +1/4                              ',
	1'LINE 6  FIRED PELLET COMPRESSIVE STRENGTH- LBS.             ',
	1'LINE 6  FIRED PELLET % -200 LB COMPRESSIONS                 ',
	1'LINE 6  BENTONITE LBS/TON                                   ',
	1'LINE 7  FIRED PELLET BT % +1/2                              ',
	1'LINE 7  FIRED PELLET BT % +1/4                              ',
	1'LINE 7  FIRED PELLET AT % +1/4                              ',
	1'LINE 7  FIRED PELLET COMPRESSIVE STRENGTH- LBS.             ',
	1'LINE 7  FIRED PELLET % -200 LB COMPRESSIONS                 ',
	1'LINE 7  BENTONITE LBS/TON                                   ',
	1'STEP 1/2 FILTER CAKE % -270 MESH                            ',
	1'STEP 1/2 FILTER CAKE FLUX RATIO                             ',
	1'STEP 1/2 FILTER CAKE B/A                                    ',
	1'STEP 3   FILTER CAKE % -270 MESH                            ',
	1'STEP 3   FILTER CAKE FLUX RATIO                             ',
	1'STEP 3   FILTER CAKE B/A                                    ',
	1'FLUX STONE % -200 MESH                                      ',
	1'LINE 3  FIRED PELLET BT % -1/2+3/8                          ',
	1'LINE 3  FIRED PELLET BT % -3/8+1/4                          ',
	1'LINE 4  FIRED PELLET BT % -1/2+3/8                          ',
	1'LINE 4  FIRED PELLET BT % -3/8+1/4                          ',
	1'LINE 5  FIRED PELLET BT % -1/2+3/8                          ',
	1'LINE 5  FIRED PELLET BT % -3/8+1/4                          ',
	1'LINE 6  FIRED PELLET BT % -1/2+3/8                          ',
	1'LINE 6  FIRED PELLET BT % -3/8+1/4                          ',
	1'LINE 7  FIRED PELLET BT % -1/2+3/8                          ',
	1'LINE 7  FIRED PELLET BT % -3/8+1/4                          '/
C
C
	CHARACTER A1DATE*11,A2DATE*11,KS*4
	INTEGER BSTART(2),BEND(2),RECNO
	REAL VAL(100)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
C
C
	CALL SYS$BINTIM (AONE,BONE)
C
C
C
	WRITE (6,*) ' MINE/MILL CORRELATION DATA'
	WRITE (6,*) ' THIS PROGRAM WILL ALLOW YOU TO SELECT THE '
	WRITE (6,*) ' BEGINNING AND ENDING DATES FOR THE DATA.'
	WRITE (6,*) ' IT WILL STORE THE DATA IN D1.PRN thru D7.PRN'
	WRITE (6,*) ' '
	WRITE (6,*) ' '
C	WRITE (6,*) ' WARNING: THE SCREEN FRACTION DATA WILL BE CORRECT '
C	WRITE (6,*) '          FOR THE YEAR 2001 AND LATER ONLY.  ANY '
C	WRITE (6,*) '           SCREEN DATA PRIOR TO 2001 WILL BE WRONG.'
C	WRITE (6,*) '           (this was caused by the addition of the'
C	WRITE (6,*) '           7/16 size.  The fix is being worked on)'
	WRITE (6,*) ' '  
	WRITE (6,*) ' '
	WRITE (6,*) ' ENTER BEGINNING DATE   (ex. 06-JUN-1995)'
C
C
	READ (5,103) J,A1DATE
103	FORMAT (Q,A)
C
	IF (J .EQ. 10) THEN
		A1DATE='0'//A1DATE(1:10)
	ELSE IF (J .NE. 11) THEN
		WRITE (6,*) ' INPUT DATE B.O.'
		STOP
	END IF
C
C
	WRITE (6,*) ' '
	WRITE (6,*) ' '
	WRITE (6,*) ' ENTER ENDING DATE   (ex. 06-JUN-1995)'
C
C
	READ (5,103) J,A2DATE
C
	IF (J .EQ. 10) THEN
		A2DATE='0'//A2DATE(1:10)
	ELSE IF (J .NE. 11) THEN
		WRITE (6,*) ' INPUT DATE B.O.'
		STOP
	END IF
C         
C
	CALL SYS$BINTIM (A1DATE,BSTART)
	CALL SYS$BINTIM (A2DATE,BEND)
C
	CALL LIB$DAY (ISTARTNO,BSTART,)
	CALL LIB$DAY (IENDNO,BEND,)
C
C
C
C
	OPEN (UNIT=10,FILE='MET_P:MMC.DAT',ACCESS='KEYED',STATUS='OLD',
	1		SHARED)
C
C
	OPEN (11,FILE='MET_SCREEN:SCREEN.DAT',
	1	ACCESS='KEYED',STATUS='OLD',shared)
C
C
C
	OPEN (UNIT=12,FILE='MET_P:HALFSIL2.DAT',STATUS='OLD',
	1	ACCESS='KEYED',SHARED,READONLY)
C
C
C
	OPEN (UNIT=20,FILE='MET_P:TOT.PRN',STATUS='NEW',
	1	ACCESS='DIRECT',FORM='FORMATTED',RECL=2000)
C
C	OPEN (UNIT=21,FILE='MET_P:D2.PRN',STATUS='NEW',
C	1	ACCESS='DIRECT',FORM='FORMATTED',RECL=200)
C
C	OPEN (UNIT=22,FILE='MET_P:D3.PRN',STATUS='NEW',
C	1	ACCESS='DIRECT',FORM='FORMATTED',RECL=200)
C
C	OPEN (UNIT=23,FILE='MET_P:D4.PRN',STATUS='NEW',
C	1	ACCESS='DIRECT',FORM='FORMATTED',RECL=200)
C
C	OPEN (UNIT=24,FILE='MET_P:D5.PRN',STATUS='NEW',
C	1	ACCESS='DIRECT',FORM='FORMATTED',RECL=200)
C
C	OPEN (UNIT=25,FILE='MET_P:D6.PRN',STATUS='NEW',
C	1	ACCESS='DIRECT',FORM='FORMATTED',RECL=200)
C
C	OPEN (UNIT=26,FILE='MET_P:D7.PRN',STATUS='NEW',
C	1	ACCESS='DIRECT',FORM='FORMATTED',RECL=200)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
	RECNO=1
	KS='    '
1	SHIFT=3
C
111	CALL SYS$ASCTIM (,A2DATE,BEND,)
	IF (A2DATE(1:1) .EQ. ' ') A2DATE(1:1)='0'
C
	DO 1818 J=1,12
		IF (MON(J) .EQ. A2DATE(4:6)) GO TO 1819
1818	CONTINUE
C
	STOP ' DATE ERROR'
C
1819	WRITE (KEY(3:4),'(I2)') J
	IF (KEY(3:3) .EQ. ' ') KEY(3:3)='0'
C
C
CCCCCCCCCCCCCCCCCCCCCCC
C
C
C
123	WRITE (KEY(7:7),'(I1)') SHIFT
C
	KEY=A2DATE(10:11)//KEY(3:4)//A2DATE(1:2)//KEY(7:7)
C
C
C
C	
c	mmc data
C
	IF (DATA.DATE .NE. KEY)
	1 read (10,KEY=KEY,ERR=200) DATA	!get latest record to 
C					        get latest date-shift
C
C
	VAL(1)=DATA.CTONS
	VAL(2)=DATA.CSIL
	VAL(3)=DATA.CMF
	VAL(4)=DATA.CWTR
	VAL(5)=DATA.CA
	VAL(19)=DATA.CT
	VAL(20)=DATA.FT
C
	DO 20 I=1,4
		VAL(I+11)=DATA.FFDATA(I)
20	CONTINUE
C
	DO 21 I=1,7
		VAL(I+22)=DATA.FCDATA(I)
21	CONTINUE
C
	DO 22 I=1,5
		VAL(38+((I-1)*6))=DATA.BENT(I)
22	CONTINUE
C
C
300	CONTINUE
C
C
	VAL(22)=DATA.FR		!FROTH REGRIND % OP TIME
	VAL(29)=DATA.AM		!AMINE LBS/TON
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
200	IF (KEY(1:6) .NE. KD(3:8)) THEN
C
	IF (KEY(1:2) .LT. '30') then
		KD='20'//KEY(1:2)//KEY(3:6)
	else
		KD='19'//KEY(1:2)//KEY(3:6)
	end if
	READ (11,KEY=KD) 
	1			    KD,DAT,IS,IH,SCREEN,COMP,CAB,SIL,FILT2,
	1			    FILT3,BL2,BL3,FLUXG,FCA,FMG,FAL,FSI,   
	1			    PA,PC,PM,S4,H4,SCWT,TONS4,GHRS,COMP2
C
	END IF                                   
C
	DO 400 LN=1,5
C
C
		bt1=0
		bt2=0
		at1=0
		at2=0
		do 88 k2=1,7
			bt1=bt1+screen(k2,shift,1,ln) 
			bt2=bt2+screen(k2,shift,2,ln) 
			at1=at1+screen(k2+7,shift,1,ln) 
			at2=at2+screen(k2+7,shift,2,ln)
88		continue 
C
C
	IF (SCREEN(2,SHIFT,1,LN) .GT. 0 .AND. SCREEN (2,SHIFT,2,LN) .GT. 0)
	1							    THEN
C
C
	VAL(33+((LN-1)*6))=(((SCREEN(1,SHIFT,1,LN)+SCREEN(2,SHIFT,1,LN))
	1		/BT1*100)
	1		+((SCREEN(1,SHIFT,2,LN)+SCREEN(2,SHIFT,2,LN))
	1		/BT2*100))/2
	VAL(70+((LN-1)*2))=(((SCREEN(3,SHIFT,1,LN))
	1		/BT1*100)
	1		+((SCREEN(3,SHIFT,2,LN))
	1		/BT2*100))/2
	VAL(71+((LN-1)*2))=(((SCREEN(4,SHIFT,1,LN))
	1		/BT1*100)
	1		+((SCREEN(4,SHIFT,2,LN))
	1		/BT2*100))/2
C
	IF (KD(1:4) .GE. '2001') THEN
	VAL(34+((LN-1)*6))=(((SCREEN(1,SHIFT,1,LN)+SCREEN (2,SHIFT,1,LN)+
	1		SCREEN(3,SHIFT,1,LN)+SCREEN(4,SHIFT,1,LN)+
	1		SCREEN(5,SHIFT,1,LN))/BT1*100)+
	1		((SCREEN(1,SHIFT,2,LN)+SCREEN(2,SHIFT,2,LN)+
	1		SCREEN(3,SHIFT,2,LN)+SCREEN(4,SHIFT,2,LN)+
	1		SCREEN(5,SHIFT,2,LN))/BT2*100)
	1		)/2
	VAL(35+((LN-1)*6))=(((SCREEN(8,SHIFT,1,LN)+SCREEN(9,SHIFT,1,LN)+
	1		SCREEN(10,SHIFT,1,LN)+SCREEN(11,SHIFT,1,LN)+
	1		SCREEN(12,SHIFT,1,LN))/AT1*100)+
	1		((SCREEN(8,SHIFT,2,LN)+SCREEN(9,SHIFT,2,LN)+
	1		SCREEN(10,SHIFT,2,LN)+SCREEN(11,SHIFT,2,LN)+
	1		SCREEN(12,SHIFT,2,LN))/AT2*100)
	1		)/2
	VAL(70+((LN-1)*2))=(((SCREEN(3,SHIFT,1,LN)+SCREEN(4,SHIFT,1,LN))
	1		/BT1*100)
	1		+((SCREEN(3,SHIFT,2,LN)+SCREEN(4,SHIFT,2,LN))
	1		/BT2*100))/2
	VAL(71+((LN-1)*2))=(((SCREEN(5,SHIFT,1,LN))
	1		/BT1*100)
	1		+((SCREEN(5,SHIFT,2,LN))
	1		/BT2*100))/2

	ELSE
	VAL(34+((LN-1)*6))=(((SCREEN(1,SHIFT,1,LN)+SCREEN (2,SHIFT,1,LN)+
	1		SCREEN(3,SHIFT,1,LN)+SCREEN(4,SHIFT,1,LN))/BT1*100)+
	1		((SCREEN(1,SHIFT,2,LN)+SCREEN(2,SHIFT,2,LN)+
	1		SCREEN(3,SHIFT,2,LN)+SCREEN(4,SHIFT,2,LN))/BT2*100)
	1		)/2
	VAL(35+((LN-1)*6))=(((SCREEN(8,SHIFT,1,LN)+SCREEN(9,SHIFT,1,LN)+
	1		SCREEN(10,SHIFT,1,LN)+SCREEN(11,SHIFT,1,LN))/AT1*100)+
	1		((SCREEN(8,SHIFT,2,LN)+SCREEN(9,SHIFT,2,LN)+
	1		SCREEN(10,SHIFT,2,LN)+SCREEN(11,SHIFT,2,LN))/AT2*100)
	1		)/2
	END IF
C
C
	ELSE IF (SCREEN(2,SHIFT,1,LN) .GT. 0 .AND. 
	1			SCREEN (2,SHIFT,2,LN) .EQ. 0) THEN
C
	VAL(33+((LN-1)*6))=(SCREEN(1,SHIFT,1,LN)+SCREEN(2,SHIFT,1,LN))
	1			/BT1*100
	VAL(70+((LN-1)*2))=(SCREEN(3,SHIFT,1,LN))
	1		/BT1*100
	VAL(71+((LN-1)*2))=(SCREEN(4,SHIFT,1,LN))
	1		/BT1*100
C
C
	IF (KD(1:4) .GE. '2001') THEN
	VAL(34+((LN-1)*6))=(SCREEN(1,SHIFT,1,LN)+SCREEN (2,SHIFT,1,LN)+
	1		SCREEN(3,SHIFT,1,LN)+SCREEN(4,SHIFT,1,LN)+
	1		SCREEN(5,SHIFT,1,LN))/BT1*100
	VAL(35+((LN-1)*6))=(SCREEN(8,SHIFT,1,LN)+SCREEN(9,SHIFT,1,LN)+
	1		SCREEN(10,SHIFT,1,LN)+SCREEN(11,SHIFT,1,LN)+
	1		SCREEN(12,SHIFT,1,LN))/AT1*100
	VAL(70+((LN-1)*2))=(SCREEN(3,SHIFT,1,LN)+SCREEN(4,SHIFT,1,LN))
	1		/BT1*100
	VAL(71+((LN-1)*2))=(SCREEN(5,SHIFT,1,LN))
	1		/BT1*100

	ELSE
	VAL(34+((LN-1)*6))=(SCREEN(1,SHIFT,1,LN)+SCREEN (2,SHIFT,1,LN)+
	1		SCREEN(3,SHIFT,1,LN)+SCREEN(4,SHIFT,1,LN))/BT1*100
	VAL(35+((LN-1)*6))=(SCREEN(8,SHIFT,1,LN)+SCREEN(9,SHIFT,1,LN)+
	1		SCREEN(10,SHIFT,1,LN)+SCREEN(11,SHIFT,1,LN))/AT1*100
	END IF
C
C
	ELSE IF (SCREEN(2,SHIFT,1,LN) .EQ. 0 .AND. 
	1			SCREEN (2,SHIFT,2,LN) .GT. 0) THEN
C
	VAL(33+((LN-1)*6))=(SCREEN(1,SHIFT,2,LN)+SCREEN(2,SHIFT,2,LN))
	1			/BT2*100
	VAL(70+((LN-1)*2))=(SCREEN(3,SHIFT,2,LN))
	1		/BT2*100
	VAL(71+((LN-1)*2))=(SCREEN(4,SHIFT,2,LN))
	1		/BT2*100
C
C
	IF (KD(1:4) .GE. '2001') THEN
	VAL(34+((LN-1)*6))=(SCREEN(1,SHIFT,2,LN)+SCREEN (2,SHIFT,2,LN)+
	1		SCREEN(3,SHIFT,2,LN)+SCREEN(4,SHIFT,2,LN)+
	1		SCREEN(5,SHIFT,2,LN))/BT2*100
	VAL(35+((LN-1)*6))=(SCREEN(8,SHIFT,2,LN)+SCREEN(9,SHIFT,2,LN)+
	1		SCREEN(10,SHIFT,2,LN)+SCREEN(11,SHIFT,2,LN)+
	1		SCREEN(12,SHIFT,2,LN))/AT2*100
	VAL(70+((LN-1)*2))=(SCREEN(3,SHIFT,2,LN)+SCREEN(4,SHIFT,2,LN))
	1		/BT2*100
	VAL(71+((LN-1)*2))=(SCREEN(5,SHIFT,2,LN))
	1		/BT2*100
	ELSE
	VAL(34+((LN-1)*6))=(SCREEN(1,SHIFT,2,LN)+SCREEN (2,SHIFT,2,LN)+
	1		SCREEN(3,SHIFT,2,LN)+SCREEN(4,SHIFT,2,LN))/BT2*100
	VAL(35+((LN-1)*6))=(SCREEN(8,SHIFT,2,LN)+SCREEN(9,SHIFT,2,LN)+
	1		SCREEN(10,SHIFT,2,LN)+SCREEN(11,SHIFT,2,LN))/AT2*100
	END IF
C
	ELSE
C
	VAL(33+((LN-1)*6))=0
	VAL(70+((LN-1)*2))=0
	VAL(71+((LN-1)*2))=0
	VAL(34+((LN-1)*6))=0
	VAL(35+((LN-1)*6))=0
C
	END IF
C
C
400	CONTINUE
C
C
	DO 401 LN=1,5
C
	IF (COMP2(1,SHIFT,1,LN) .GT. 0 .AND. COMP2(1,SHIFT,2,LN) .GT. 0) THEN
C
	VAL(36+((LN-1)*6))=(COMP2(1,SHIFT,1,LN)+COMP2(1,SHIFT,2,LN))/2
	VAL(37+((LN-1)*6))=(COMP2(2,SHIFT,1,LN)+COMP2(2,SHIFT,2,LN))/2
C
	ELSE IF (COMP2(1,SHIFT,1,LN) .GT. 0 .AND. 
	1				    COMP2(1,SHIFT,2,LN) .EQ. 0) THEN
C
	VAL(36+((LN-1)*6))=COMP2(1,SHIFT,1,LN)
	VAL(37+((LN-1)*6))=COMP2(2,SHIFT,1,LN)
C	
	ELSE IF (COMP2(1,SHIFT,1,LN) .EQ. 0 .AND. 
	1				    COMP2(1,SHIFT,2,LN) .GT. 0) THEN
C
	VAL(36+((LN-1)*6))=COMP2(1,SHIFT,2,LN)
	VAL(37+((LN-1)*6))=COMP2(2,SHIFT,2,LN)
C
C
	END IF
C
C
401	CONTINUE
C
C
C		filter cake nos.
C
	VAL(63)=FILT2(2,SHIFT)
	VAL(66)=FILT3(2,SHIFT)
	VAL(64)=((FCA(1,SHIFT)+FMG(1,SHIFT))-.6)/.5377
	VAL(64)=VAL(64)/(100-VAL(64))
	VAL(67)=((FCA(2,SHIFT)+FMG(2,SHIFT))-.6)/.5377
	VAL(67)=VAL(67)/(100-VAL(67))
	IF ((FILT2(1,SHIFT)+FAL(1,SHIFT)) .LE. 0) GO TO 2288
	VAL(65)=(FCA(1,SHIFT)+FMG(1,SHIFT))/(FILT2(1,SHIFT)+FAL(1,SHIFT))
2288	IF ((FILT3(1,SHIFT)+FAL(2,SHIFT)) .LE. 0) GO TO 2289
	VAL(68)=(FCA(2,SHIFT)+FMG(2,SHIFT))/(FILT3(1,SHIFT)+FAL(2,SHIFT))
2289	VAL(69)=FLUXG
C
	IF (FMG(1,SHIFT) .LE. 0) GO TO 1516
	VAL(31)=FCA(1,SHIFT)/FMG(1,SHIFT)
1516	IF (FMG(2,SHIFT) .LE. 0) GO TO 1517
	VAL(32)=FCA(2,SHIFT)/FMG(2,SHIFT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
1517	IF (KEY(1:4) .NE. KS) THEN
		READ (12,KEY=KEY(1:4),IOSTAT=M) HSD
		KS=KEY(1:4)
	END IF
C
	READ (KEY(5:6),'(I2)') IDAY
C
	VAL(11)=HSD.FLS(SHIFT,IDAY)
	VAL(17)=HSD.FLFS(SHIFT,IDAY)
	VAL(18)=HSD.FLFR(SHIFT,IDAY)
	VAL(21)=HSD.FLTS(SHIFT,IDAY)
C
C
	VAL(16)=FSI(2,SHIFT)
C
C	  end of calculations for this shift
C
C
	WRITE (20,2020,REC=RECNO)KEY,(VAL(J),J=1,100)
2020	FORMAT (' ',A7,100F12.3)
C	WRITE (21,2021,REC=RECNO)(VAL(J),J=16,30)
C2021	FORMAT (' ',15F12.3)
C	WRITE (22,2021,REC=RECNO)(VAL(J),J=31,45)
C	WRITE (23,2021,REC=RECNO)(VAL(J),J=46,60)
C	WRITE (24,2021,REC=RECNO)(VAL(J),J=61,75)
C	WRITE (25,2021,REC=RECNO)(VAL(J),J=76,90)
C	WRITE (26,2022,REC=RECNO)(VAL(J),J=91,100)
C2022	FORMAT (' ',10F12.3)
C
	RECNO=RECNO+1
C
	SHIFT=SHIFT-1
	IF (SHIFT .GT. 0) GO TO 123
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C
C
C
	CALL LIB$SUB_TIMES (BEND,BONE,BEND)
C
	CALL LIB$DAY (J2,BEND,)
C
	IF (J2 .GE. ISTARTNO) GO TO 1
C
C
	STOP
	END
