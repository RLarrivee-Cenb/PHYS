*********************************************************************
* This program LOADS AN 3 SHIFT RECORDS INTO THE ORACLE TABLE STREND
* THIS PROGRAM IS TURNED ON INSIDE MREPO.COM AND RUNS AT 6:00 AM
*********************************************************************
 
      PROGRAM STRENDO
C

      EXTERNAL strenD
      INTEGER*4 SQLITR
      INTEGER*4 SQHSTV(21)
      INTEGER*4 SQHSTL(21)
      INTEGER*4 SQINDV(21)
      INTEGER*4 SQHARM(21)
      INTEGER*4 SQHARC(21)
      CHARACTER*338 SQ0001
      INTEGER*4 SQLCTX
      INTEGER*4 IAPSUC,IAPFAI,IAPFTL
      INTEGER*2 SQC000(198)
      CHARACTER*11 SQLFNM
      COMMON / strenI / IAPSUC,IAPFAI,IAPFTL,SQLCTX,SQC000
      COMMON / strenC / SQLFNM, SQ0001
        INTEGER*4       UFO_OPEN,I,J,K,L,M,N
C
        CHARACTER*12    DATEX
C
        REAL*4          VALU(24)
C
C     EXEC SQL INCLUDE SQLCA
C
C     
C      sqlca.for 
C     
C     *****************************************************************
C     *                                                               *
C     *               S  Q  L  C  A  .  F  O  R                       *
C     *                                                               *
C     *****************************************************************
 
C     *****************************************************************
C     *  Note: some compilers do  not  allow CHAR and INT data in the *
C     *  same common block;  therefore the CHAR vars are declared as  *
C     *  LOGICAL*1.  Sigh.                                            *
C     *****************************************************************
 
       LOGICAL*1   SQLAID  (8)
       INTEGER*4   SQLABC
       INTEGER*4   SQLCDE
C    SQLERRM
       INTEGER*2   SQLEML
       LOGICAL*1   SQLEMC (70)
       LOGICAL*1   SQLERP  (8)
       INTEGER*4   SQLERD  (6)
C    SQLWRN(8)
       LOGICAL*1   SQLWN0, SQLWN1, SQLWN2, SQLWN3,
     1             SQLWN4, SQLWN5, SQLWN6, SQLWN7
       LOGICAL*1   SQLEXT  (8)
C
       COMMON /SQLCA/
     1   SQLAID,
     2   SQLABC,
     3   SQLCDE,
     4   SQLEML,
     5   SQLEMC,
     6   SQLERP,
     7   SQLERD,
     8   SQLWN0, SQLWN1, SQLWN2, SQLWN3,
     9   SQLWN4, SQLWN5, SQLWN6, SQLWN7,
     1   SQLEXT
C
C     *****************************************************************
C     *  The following common block is provided for use by Dynamic    *
C     *  Method 4.  Along with the SQLCA, it must be present in any   *
C     *  subroutine which executes a dynamic SQL statement using      *
C     *  descriptors.                                                 *
C     *****************************************************************
C
       INTEGER *4   DSC2N
       INTEGER *4   DSC2V
       INTEGER *4   DSC2L
       INTEGER *4   DSC2T
       INTEGER *4   DSC2I
       INTEGER *4   DSC2F
       INTEGER *4   DSC2S
       INTEGER *4   DSC2M
       INTEGER *4   DSC2C
       INTEGER *4   DSC2X
       INTEGER *4   DSC2Y
       INTEGER *4   DSC2Z
C
       COMMON /DSC2/   DSC2N, DSC2V, DSC2L, DSC2T, DSC2I, DSC2F,
     1                 DSC2S, DSC2M, DSC2C, DSC2X, DSC2Y, DSC2Z
C
C     EXEC SQL BEGIN DECLARE SECTION
         CHARACTER*10   UID
         CHARACTER*10   PWD
         CHARACTER*10   DBSTR1
         REAL*4         S12MINE
         REAL*4         S3MINE
         REAL*4         TOTMINE
         REAL*4         S2TARGET
         REAL*4         S2COMPOS
         REAL*4         S3TARGET
         REAL*4         S3COMPOS
         REAL*4         S12PEL
         REAL*4         S12FCAKE
         REAL*4         S3PEL
         REAL*4         S3FCAKE
         REAL*4         FLOTFEED
         REAL*4         FF270M
         REAL*4         DTREC
         REAL*4         DTSIL
         REAL*4         S12TRAIN
         REAL*4         S3TRAIN
         REAL*4         AMINE
         REAL*4         FFTAILS
         REAL*4         SHIFT
         CHARACTER*11   SDATE
C     EXEC SQL END DECLARE SECTION
C
C
        EXTERNAL        UFO_OPEN
C
C     EXEC SQL WHENEVER SQLERROR GOTO 9000
C     LOG ON TO ORACLE.
      UID = 'TOLIVE'
      PWD = 'MANAGER2'
      DBSTR1 = 'PROD2'
C
C     EXEC SQL DECLARE DBNAM1 DATABASE
C
C     EXEC SQL CONNECT :UID IDENTIFIED BY :PWD
C    1          AT DBNAM1 USING :DBSTR1
C
C
      CALL SQLADR(%REF(UID), SQHSTV(1))
      SQHSTL(1) = 10
      SQINDV(1) = 0
      SQHARM(1) = 0
      CALL SQLADR(%REF(PWD), SQHSTV(2))
      SQHSTL(2) = 10
      SQINDV(2) = 0
      SQHARM(2) = 0
      CALL SQLADR(%REF(DBSTR1), SQHSTV(3))
      SQHSTL(3) = 10
      SQINDV(3) = 0
      SQHARM(3) = 0
      CALL SQLFIN(11, %REF(SQLFNM))
      SQLITR = 10
      CALL SQLFX7(6, %REF(SQLCTX), 3, SQLITR, 2, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), 0,
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
      IF (SQLCDE .LT. 0) GO TO 9000
        OPEN    (UNIT=1,FILE='LOGS:STREND.DAT',
     +           STATUS='OLD',
     +           ACCESS='DIRECT',RECORDTYPE='FIXED',
     +           FORM='UNFORMATTED',RECL=27,SHARED)
C
        DO 1000 I=1,3
C
                READ    (UNIT=1,REC=I)DATEX,VALU
C
                SDATE=DATEX(1:11)       !ASCII DATE FOR THIS RECORD
                IF (DATEX(12:12) .EQ. '3')SHIFT=3
                IF (DATEX(12:12) .EQ. '2')SHIFT=2
                IF (DATEX(12:12) .EQ. '1')SHIFT=1
C
                S12MINE=VALU(1)         !LOAD VALUES FOR RECORD
                S3MINE=VALU(2)
                TOTMINE=VALU(3)
                S2TARGET=VALU(4)
                S2COMPOS=VALU(5)
                S3TARGET=VALU(6)
                S3COMPOS=VALU(7)
                S12PEL=VALU(8)
                S12FCAKE=VALU(9)
                S3PEL=VALU(10)
                S3FCAKE=VALU(11)
                FLOTFEED=VALU(12)
                FF270M=VALU(13)
                DTREC=VALU(14)
                DTSIL=VALU(15)
                S12TRAIN=VALU(16)
                S3TRAIN=VALU(17)
                AMINE=VALU(18)
                FFTAILS=VALU(19)
C
C     EXEC SQL AT DBNAM1 INSERT INTO STREND (SDATE,SHIFT,S12MINE,S3MINE,
C    1    TOTMINE,S2TARGET,S2COMPOS,S3TARGET,S3COMPOS,S12PEL,S12FCAKE,
C    2    S3PEL,S3FCAKE,FLOTFEED,FF270M,DTREC,DTSIL,S12TRAIN,
C    3    S3TRAIN,AMINE,FFTAILS)
C    4     VALUES(TO_DATE(:SDATE,'DD-MON-YYYY'),:SHIFT,:S12MINE,:S3MINE,
C    5     :TOTMINE,:S2TARGET,:S2COMPOS,:S3TARGET,:S3COMPOS,:S12PEL,
C    6     :S12FCAKE,:S3PEL,:S3FCAKE,:FLOTFEED,:FF270M,:DTREC,:DTSIL,
C    7     :S12TRAIN,:S3TRAIN,:AMINE,:FFTAILS)
C
      CALL SQLADR(%REF(SDATE), SQHSTV(1))
      SQHSTL(1) = 11
      SQINDV(1) = 0
      SQHARM(1) = 0
      CALL SQLADR(%REF(SHIFT), SQHSTV(2))
      SQHSTL(2) = 4
      SQINDV(2) = 0
      SQHARM(2) = 0
      CALL SQLADR(%REF(S12MINE), SQHSTV(3))
      SQHSTL(3) = 4
      SQINDV(3) = 0
      SQHARM(3) = 0
      CALL SQLADR(%REF(S3MINE), SQHSTV(4))
      SQHSTL(4) = 4
      SQINDV(4) = 0
      SQHARM(4) = 0
      CALL SQLADR(%REF(TOTMINE), SQHSTV(5))
      SQHSTL(5) = 4
      SQINDV(5) = 0
      SQHARM(5) = 0
      CALL SQLADR(%REF(S2TARGET), SQHSTV(6))
      SQHSTL(6) = 4
      SQINDV(6) = 0
      SQHARM(6) = 0
      CALL SQLADR(%REF(S2COMPOS), SQHSTV(7))
      SQHSTL(7) = 4
      SQINDV(7) = 0
      SQHARM(7) = 0
      CALL SQLADR(%REF(S3TARGET), SQHSTV(8))
      SQHSTL(8) = 4
      SQINDV(8) = 0
      SQHARM(8) = 0
      CALL SQLADR(%REF(S3COMPOS), SQHSTV(9))
      SQHSTL(9) = 4
      SQINDV(9) = 0
      SQHARM(9) = 0
      CALL SQLADR(%REF(S12PEL), SQHSTV(10))
      SQHSTL(10) = 4
      SQINDV(10) = 0
      SQHARM(10) = 0
      CALL SQLADR(%REF(S12FCAKE), SQHSTV(11))
      SQHSTL(11) = 4
      SQINDV(11) = 0
      SQHARM(11) = 0
      CALL SQLADR(%REF(S3PEL), SQHSTV(12))
      SQHSTL(12) = 4
      SQINDV(12) = 0
      SQHARM(12) = 0
      CALL SQLADR(%REF(S3FCAKE), SQHSTV(13))
      SQHSTL(13) = 4
      SQINDV(13) = 0
      SQHARM(13) = 0
      CALL SQLADR(%REF(FLOTFEED), SQHSTV(14))
      SQHSTL(14) = 4
      SQINDV(14) = 0
      SQHARM(14) = 0
      CALL SQLADR(%REF(FF270M), SQHSTV(15))
      SQHSTL(15) = 4
      SQINDV(15) = 0
      SQHARM(15) = 0
      CALL SQLADR(%REF(DTREC), SQHSTV(16))
      SQHSTL(16) = 4
      SQINDV(16) = 0
      SQHARM(16) = 0
      CALL SQLADR(%REF(DTSIL), SQHSTV(17))
      SQHSTL(17) = 4
      SQINDV(17) = 0
      SQHARM(17) = 0
      CALL SQLADR(%REF(S12TRAIN), SQHSTV(18))
      SQHSTL(18) = 4
      SQINDV(18) = 0
      SQHARM(18) = 0
      CALL SQLADR(%REF(S3TRAIN), SQHSTV(19))
      SQHSTL(19) = 4
      SQINDV(19) = 0
      SQHARM(19) = 0
      CALL SQLADR(%REF(AMINE), SQHSTV(20))
      SQHSTL(20) = 4
      SQINDV(20) = 0
      SQHARM(20) = 0
      CALL SQLADR(%REF(FFTAILS), SQHSTV(21))
      SQHSTL(21) = 4
      SQINDV(21) = 0
      SQHARM(21) = 0
      CALL SQLFIN(11, %REF(SQLFNM))
      SQLITR = 1
      CALL SQLFX7(6, %REF(SQLCTX), 21, SQLITR, 39, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), %REF(SQ0001),
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
      IF (SQLCDE .LT. 0) GO TO 9000
1000    CONTINUE
C
        CLOSE   (UNIT=1)
C
C     EXEC SQL AT DBNAM1 COMMIT WORK RELEASE 
      CALL SQLFIN(11, %REF(SQLFNM))
      SQLITR = 1
      CALL SQLFX7(6, %REF(SQLCTX), 0, SQLITR, 148, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), 0,
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
      IF (SQLCDE .LT. 0) GO TO 9000
      GOTO 9999

9000  PRINT '(70A1)', SQLEMC
C     EXEC SQL WHENEVER SQLERROR CONTINUE
C     EXEC SQL AT DBNAM1 ROLLBACK RELEASE
C
      CALL SQLFIN(11, %REF(SQLFNM))
      SQLITR = 1
      CALL SQLFX7(6, %REF(SQLCTX), 0, SQLITR, 173, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), 0,
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
9999  CONTINUE
      END

      BLOCK DATA strenD
      INTEGER*2 SQCNT
      CHARACTER*338 SQ0001
      INTEGER*4 SQLCTX
      INTEGER*4 IAPSUC,IAPFAI,IAPFTL
      INTEGER*2 SQC000(198)
      CHARACTER*11 SQLFNM
      COMMON / strenI / IAPSUC,IAPFAI,IAPFTL,SQLCTX,SQC000
      COMMON / strenC / SQLFNM, SQ0001
      DATA SQLCTX / 5156 /
      DATA IAPSUC / 0 /, IAPFAI / 1403 /, IAPFTL / 535 /
      DATA SQ0001 /'INSERT INTO STREND (SDATE,SHIFT,S12MINE,S3MINE,    T
     +OTMINE,S2TARGET,S2COMPOS,S3TARGET,S3COMPOS,S12PEL,S12FCAKE,    S3P
     +EL,S3FCAKE,FLOTFEED,FF270M,DTREC,DTSIL,S12TRAIN,    S3TRAIN,AMINE,
     +FFTAILS)     VALUES(TO_DATE(:b1,''DD-MON-YYYY''),:b2,:b3,:b4,     
     +:b5,:b6,:b7,:b8,:b9,:b10,     :b11,:b12,:b13,:b14,:b15,:b16,:b17, 
     +    :b18,:b19,:b20,:b21)'/
      DATA SQLFNM /'strendo.PFO'/
      DATA (SQC000(SQCNT), SQCNT=1, 198) /6,4130,2,8,0,0,0,0,0,0,0,0,0,
     +0,27,54,3,3,0,0,6,68,66,78,65,77,49,1,1,0,0,1,1,0,0,1,1,0,0,39,8,
     +0,0,0,0,0,0,0,0,1,338,3,92,21,21,0,3,6,68,66,78,65,77,49,1,1,0,0,
     +1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,
     +1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,
     +1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,148,8,0,0,0,0,0,0,0,0,1,0,30,105,
     +0,0,0,3,6,68,66,78,65,77,49,173,8,0,0,0,0,0,0,0,0,1,0,32,110,0,0,
     +0,3,6,68,66,78,65,77,49/
      END
