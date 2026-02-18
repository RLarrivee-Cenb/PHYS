***************************************************************
* Sample Program 1:  Simple Query
*
* This program logs on to ORACLE and extracts the average pellet
* train silicas by plant, by shift, for the last nine shifts in
* the STREND file.  Only flux trains are processed.
***************************************************************
      PROGRAM QUERY
      IMPLICIT NONE
C     EXEC SQL BEGIN DECLARE SECTION

      EXTERNAL sam1D
      INTEGER*4 SQLITR
      INTEGER*4 SQHSTV(3)
      INTEGER*4 SQHSTL(3)
      INTEGER*4 SQINDV(3)
      INTEGER*4 SQHARM(3)
      INTEGER*4 SQHARC(3)
      CHARACTER*252 SQ0001
      CHARACTER*252 SQ0002
      INTEGER*4 SQLCTX
      INTEGER*4 IAPSUC,IAPFAI,IAPFTL
      INTEGER*2 SQC000(133)
      CHARACTER*8 SQLFNM
      COMMON / sam1I / IAPSUC,IAPFAI,IAPFTL,SQLCTX,SQC000
      COMMON / sam1C / SQLFNM, SQ0001, SQ0002
          CHARACTER*10   UID
          CHARACTER*10   PWD
          CHARACTER*17   TOX
          CHARACTER*17   FROMX
          INTEGER        WB
          REAL           TONS
          REAL           SIO2
          REAL           PROD
C     EXEC SQL END DECLARE SECTION
      INTEGER        TOTAL,J
      CHARACTER*12      DATEX(9),DATX
      CHARACTER*17      FROMD(9)
      CHARACTER*17      TOD(9)
      REAL*4            VALU(24),SIL1(9),SIL2(9)
      INTEGER*4         STIME(2),STIMEX(2),DIFF(2)
      CHARACTER*23      PREDICT,DATETIME
      CHARACTER*1       DAYSX/'1'/
C
C     EXEC SQL INCLUDE SQLCA
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
C     EXEC SQL WHENEVER SQLERROR DO CALL SQLERR
C     LOG ON TO ORACLE.
      UID = 'WJB'
      PWD = 'WJB'
C     EXEC SQL CONNECT :UID IDENTIFIED BY :PWD
C      PRINT 1800, UID
C 1800 FORMAT (/, ' CONNECTED TO ORACLE AS USER:  ', A)
C     QUERY LOOP REPEATS UNTIL THE USER ENTERS A 0
C      PRINT 2100
C 2100 FORMAT (//, ' ENTER WAYBILL NUMBER (0 TO QUIT):  ', $)
C      ACCEPT 2200, WB
C 2200 FORMAT (I10)
C      IF (WB .EQ. 0) CALL SIGNOFF (TOTAL)
      CALL SQLADR(%REF(UID), SQHSTV(1))
      SQHSTL(1) = 10
      SQINDV(1) = 0
      SQHARM(1) = 0
      CALL SQLADR(%REF(PWD), SQHSTV(2))
      SQHSTL(2) = 10
      SQINDV(2) = 0
      SQHARM(2) = 0
      CALL SQLFIN(8, %REF(SQLFNM))
      SQLITR = 10
      CALL SQLFX7(6, %REF(SQLCTX), 3, SQLITR, 2, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), 0,
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
      IF (SQLCDE .LT. 0) CALL SQLERR
      TOTAL = 0
C
        OPEN    (UNIT=1,FILE='LOGS:STREND.DAT',
     +   STATUS='OLD',
     +   ACCESS='DIRECT',RECORDTYPE='FIXED',
     +   FORM='UNFORMATTED',RECL=27,SHARED)
C
        DO 80 J=1,9     !READ IN LAST NINE SHIFTS FROM STREND AND SAVE DATES
C
        READ    (1,REC=J)DATEX(J),VALU
C
80      CONTINUE
C
        CLOSE   (UNIT=1)
C
        DO 90   J=1,9   !SET UP FROM AND TO DATES FOR QUERIES
                IF (DATEX(J)(12:12) .EQ. '1')THEN
                        TOD(J)(1:11)=DATEX(J)(1:11)
                        TOD(J)(12:17)=' 06:29'
                        FROMD(J)(12:17)=' 22:30'
                        DATETIME(1:11)=DATEX(J)(1:11)
                        DATETIME(12:23)=' 00:00:00.00'
                        CALL    SYS$BINTIM(DATETIME,STIME)
                        CALL    SYS$BINTIM(DAYSX,STIMEX)
                        CALL    LIB$SUB_TIMES(STIME,STIMEX,DIFF)
                        CALL    SYS$ASCTIM(,PREDICT,DIFF,)
                        FROMD(J)(1:11)=PREDICT(1:11)
                END IF
C
                IF (DATEX(J)(12:12) .EQ. '2')THEN
                        TOD(J)(1:11)=DATEX(J)(1:11)
                        TOD(J)(12:17)=' 14:29'
                        FROMD(J)(1:11)=DATEX(J)(1:11)
                        FROMD(J)(12:17)=' 06:30'
                END IF
C
                IF (DATEX(J)(12:12) .EQ. '3')THEN
                        TOD(J)(1:11)=DATEX(J)(1:11)
                        TOD(J)(12:17)=' 22:29'
                        FROMD(J)(1:11)=DATEX(J)(1:11)
                        FROMD(J)(12:17)=' 14:30'
                END IF
C
90      CONTINUE
C
 2000 CONTINUE
        DO 222  J=1,9
                FROMX=FROMD(J)
                TOX=TOD(J)
                SIL1(J)=0
C     EXEC SQL WHENEVER NOT FOUND GOTO 7000
C     EXEC SQL SELECT (SUM(TONS*SIO2)/SUM(TONS))
C    1    INTO :SIO2
C    2    FROM WBILL,WAYBILL
C    3    WHERE WB >= WAYBILL.LO
C    4    AND WB <= WAYBILL.HI
C    5    AND GRADE=1 AND POCKET=2
C    6    AND LDATE >= TO_DATE(:FROMX,
C    7    'DD-MON-YYYY HH24:MI')
C    8    AND LDATE <= TO_DATE(:TOX,
C    9    'DD-MON-YYYY HH24:MI')
      CALL SQLADR(%REF(FROMX), SQHSTV(1))
      SQHSTL(1) = 17
      SQINDV(1) = 0
      SQHARM(1) = 0
      CALL SQLADR(%REF(TOX), SQHSTV(2))
      SQHSTL(2) = 17
      SQINDV(2) = 0
      SQHARM(2) = 0
      CALL SQLADR(%REF(SIO2), SQHSTV(3))
      SQHSTL(3) = 4
      SQINDV(3) = 0
      SQHARM(3) = 0
      CALL SQLFIN(8, %REF(SQLFNM))
      SQLITR = 1
      CALL SQLFX7(6, %REF(SQLCTX), 3, SQLITR, 33, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), %REF(SQ0001),
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
      IF (SQLCDE .EQ. 1403) GO TO 7000
      IF (SQLCDE .LT. 0) CALL SQLERR
        SIL1(J)=SIO2
        SIO2=0
222     CONTINUE
C
        DO 333  J=1,9
C               WRITE (6,444)SIL1(J),FROMD(J),TOD(J)
444             FORMAT  (' ',2X,F5.2,1X,A,1X,A)
333     CONTINUE
C
        DO 555  J=1,9
                FROMX=FROMD(J)
                TOX=TOD(J)
                SIL2(J)=0
C     EXEC SQL WHENEVER NOT FOUND GOTO 7000
C     EXEC SQL SELECT (SUM(TONS*SIO2)/SUM(TONS))
C    1    INTO :SIO2
C    2    FROM WBILL,WAYBILL
C    3    WHERE WB >= WAYBILL.LO
C    4    AND WB <= WAYBILL.HI
C    5    AND GRADE=1 AND POCKET=1
C    6    AND LDATE >= TO_DATE(:FROMX,
C    7    'DD-MON-YYYY HH24:MI')
C    8    AND LDATE <= TO_DATE(:TOX,
C    9    'DD-MON-YYYY HH24:MI')
      CALL SQLADR(%REF(FROMX), SQHSTV(1))
      SQHSTL(1) = 17
      SQINDV(1) = 0
      SQHARM(1) = 0
      CALL SQLADR(%REF(TOX), SQHSTV(2))
      SQHSTL(2) = 17
      SQINDV(2) = 0
      SQHARM(2) = 0
      CALL SQLADR(%REF(SIO2), SQHSTV(3))
      SQHSTL(3) = 4
      SQINDV(3) = 0
      SQHARM(3) = 0
      CALL SQLFIN(8, %REF(SQLFNM))
      SQLITR = 1
      CALL SQLFX7(6, %REF(SQLCTX), 3, SQLITR, 64, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), %REF(SQ0002),
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
      IF (SQLCDE .EQ. 1403) GO TO 7000
      IF (SQLCDE .LT. 0) CALL SQLERR
        SIL2(J)=SIO2
        SIO2=0
555     CONTINUE
C
        DO 666  J=1,9
C               WRITE (6,777)SIL2(J),FROMD(J),TOD(J)
777             FORMAT  (' ',2X,F5.2,1X,A,1X,A)
666     CONTINUE
C
        OPEN    (UNIT=1,FILE='LOGS:STREND.DAT',
     +   STATUS='OLD',
     +   ACCESS='DIRECT',RECORDTYPE='FIXED',
     +   FORM='UNFORMATTED',RECL=27,SHARED)
C
        DO 888 J=1,9    !READ IN LAST NINE SHIFTS FROM STREND AND SAVE DATES
C
        READ    (1,REC=J)DATEX(J),VALU
        VALU(16)=SIL1(J)        !UPDATE TRAIN SIO2 THIS SHIFT STEP12
        VALU(17)=SIL2(J)        !UPDATE TRAIN SIO2 THIS SHIFT STEP 3
        WRITE   (1,REC=J)DATEX(J),VALU
C
888     CONTINUE
C
        CLOSE   (UNIT=1)
C
      CALL SIGNOFF (TOTAL)
 7000 CONTINUE
      PRINT 7200
 7200 FORMAT (/, ' NOT A VALID WAYBILL NUMBER - TRY AGAIN.')
      CALL SIGNOFF (TOTAL)
      END
      SUBROUTINE SIGNOFF (NUMQ)

      EXTERNAL sam1D
      INTEGER*4 SQLITR
      INTEGER*4 SQHSTV(1)
      INTEGER*4 SQHSTL(1)
      INTEGER*4 SQINDV(1)
      INTEGER*4 SQHARM(1)
      INTEGER*4 SQHARC(1)
      CHARACTER*252 SQ0001
      CHARACTER*252 SQ0002
      INTEGER*4 SQLCTX
      INTEGER*4 IAPSUC,IAPFAI,IAPFTL
      INTEGER*2 SQC000(133)
      CHARACTER*8 SQLFNM
      COMMON / sam1I / IAPSUC,IAPFAI,IAPFTL,SQLCTX,SQC000
      COMMON / sam1C / SQLFNM, SQ0001, SQ0002
      INTEGER NUMQ
C     EXEC SQL INCLUDE SQLCA
C      PRINT 8000, NUMQ
C 8000 FORMAT (/, ' TOTAL NUMBER QUERIED WAS: ', I4)
C      PRINT 8200
C 8200 FORMAT (/, ' HAVE A GOOD DAY.', /)
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
C     EXEC SQL COMMIT WORK RELEASE 
      CALL SQLFIN(8, %REF(SQLFNM))
      SQLITR = 1
      CALL SQLFX7(6, %REF(SQLCTX), 0, SQLITR, 95, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), 0,
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
      IF (SQLCDE .LT. 0) CALL SQLERR
      STOP
      END
      SUBROUTINE SQLERR
C     EXEC SQL INCLUDE SQLCA
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
 

      EXTERNAL sam1D
      INTEGER*4 SQLITR
      INTEGER*4 SQHSTV(1)
      INTEGER*4 SQHSTL(1)
      INTEGER*4 SQINDV(1)
      INTEGER*4 SQHARM(1)
      INTEGER*4 SQHARC(1)
      CHARACTER*252 SQ0001
      CHARACTER*252 SQ0002
      INTEGER*4 SQLCTX
      INTEGER*4 IAPSUC,IAPFAI,IAPFTL
      INTEGER*2 SQC000(133)
      CHARACTER*8 SQLFNM
      COMMON / sam1I / IAPSUC,IAPFAI,IAPFTL,SQLCTX,SQC000
      COMMON / sam1C / SQLFNM, SQ0001, SQ0002
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
C     EXEC SQL WHENEVER SQLERROR CONTINUE
C      PRINT 9000
C 9000 FORMAT (/, ' NO TRAINS THIS POCKET... THIS SHIFT')
C      PRINT 9200, SQLEMC
C 9200 FORMAT (' ', A70)
C     EXEC SQL ROLLBACK WORK RELEASE
      CALL SQLFIN(8, %REF(SQLFNM))
      SQLITR = 1
      CALL SQLFX7(6, %REF(SQLCTX), 0, SQLITR, 114, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), 0,
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
      RETURN
      END

      BLOCK DATA sam1D
      INTEGER*2 SQCNT
      CHARACTER*252 SQ0001
      CHARACTER*252 SQ0002
      INTEGER*4 SQLCTX
      INTEGER*4 IAPSUC,IAPFAI,IAPFTL
      INTEGER*2 SQC000(133)
      CHARACTER*8 SQLFNM
      COMMON / sam1I / IAPSUC,IAPFAI,IAPFTL,SQLCTX,SQC000
      COMMON / sam1C / SQLFNM, SQ0001, SQ0002
      DATA SQLCTX / 2322 /
      DATA IAPSUC / 0 /, IAPFAI / 1403 /, IAPFTL / 535 /
      DATA SQ0001 /'SELECT (SUM(TONS*SIO2)/SUM(TONS))    INTO :b1 FROM W
     +BILL,WAYBILL    WHERE WB >= WAYBILL.LO    AND WB <= WAYBILL.HI    
     +AND GRADE=1 AND POCKET=2    AND LDATE >= TO_DATE(:b2,    ''DD-MON-
     +YYYY HH24:MI'')    AND LDATE <= TO_DATE(:b3,    ''DD-MON-YYYY HH24
     +:MI'')'/
      DATA SQ0002 /'SELECT (SUM(TONS*SIO2)/SUM(TONS))    INTO :b1 FROM W
     +BILL,WAYBILL    WHERE WB >= WAYBILL.LO    AND WB <= WAYBILL.HI    
     +AND GRADE=1 AND POCKET=1    AND LDATE >= TO_DATE(:b2,    ''DD-MON-
     +YYYY HH24:MI'')    AND LDATE <= TO_DATE(:b3,    ''DD-MON-YYYY HH24
     +:MI'')'/
      DATA SQLFNM /'sam1.PFO'/
      DATA (SQC000(SQCNT), SQCNT=1, 133) /6,4130,2,8,0,0,0,0,0,0,0,0,0,
     +0,27,34,3,3,0,0,0,1,1,0,0,1,1,0,0,1,10,0,0,33,8,0,0,0,0,0,0,0,0,
     +1,252,4,94,3,2,0,1,0,1,1,0,0,1,1,0,0,2,4,0,0,64,8,0,0,0,0,0,0,0,
     +0,2,252,4,118,3,2,0,1,0,1,1,0,0,1,1,0,0,2,4,0,0,95,8,0,0,0,0,0,0,
     +0,0,2,0,30,166,0,0,0,1,0,114,8,0,0,0,0,0,0,0,0,2,0,32,176,0,0,0,
     +1,0/
      END
