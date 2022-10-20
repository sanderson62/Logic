       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMAMF3A
       AUTHOR.     PABLO.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CSR-FILE         ASSIGN TO SYS007
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ERACCT           ASSIGN TO ERACCT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AM-CONTROL-PRIMARY
                                   FILE STATUS IS ERACCT-FILE-STATUS.

           SELECT ERCOMP           ASSIGN TO ERCOMP
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CO-CONTROL-PRIMARY
                                   FILE STATUS IS ERCOMP-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  ERACCT.

                                       COPY ERCACCT.

       FD  ERCOMP.

                                       COPY ERCCOMP.

       FD  DISK-DATE                   COPY ELCDTEFD.

       FD  CSR-FILE
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F. 

       01  CSR-REC.
           05  CSR-CARRIER             PIC X.
           05  FILLER                  PIC X(12).
           05  CSR-STATE               PIC XX.
           05  FILLER                  PIC X(11).
           05  CSR-ACCOUNT             PIC X(10).
           05  FILLER                  PIC X(14).
           05  CSR-BUS-GRP             PIC XX.
           05  CSR-CSR                 PIC X(4).

       WORKING-STORAGE SECTION.
       77  WS-TABLE-EOF-SW             PIC X  VALUE SPACES.
           88  END-OF-TABLE               VALUE 'Y'.
       77  WS-CSR                      PIC X(4)  VALUE SPACES.
       77  WS-CO-PREV-KEY              PIC X(29) VALUE LOW-VALUES.
       01  WS-STATUS-CODES.
           05  ERACCT-FILE-STATUS      PIC XX  VALUE SPACES.
           05  WS-ERACCT-SW            PIC X   VALUE SPACES.
               88  END-OF-ERACCT               VALUE 'Y'.
           05  ERCOMP-FILE-STATUS      PIC XX  VALUE SPACES.
           05  WS-ERCOMP-SW            PIC X   VALUE SPACES.
               88  END-OF-ERCOMP               VALUE 'Y'.

       01  WS-AM-KEY.
           05  WS-CARR                 PIC X.
           05  WS-STATE                PIC XX.
           05  WS-ACCOUNT              PIC X(10).

       01  WS-WORK-FIELDS.
           05  PGM-SUB                 PIC S9(4)   VALUE +548.
           05  WS-RETURN-CODE          PIC S9(3)   VALUE ZERO.
           05  WS-ABEND-MESSAGE        PIC X(80)   VALUE SPACES.
           05  WS-ZERO                 PIC S9      VALUE ZERO.
           05  WS-ABEND-FILE-STATUS    PIC XX      VALUE ZERO.
           05  WS-HIGH-SEQ             PIC X         VALUE ' '.
               88  FOUND-HIGH-SEQ-NO                 VALUE 'Y'.
           05  WS-ABEND-FLD            PIC 9         VALUE ZEROS.
           05  WS-ZERO-FLD             PIC 9         VALUE ZEROS.
           05  WS-ERACCT-RECS-IN       PIC 9(9)      VALUE ZEROS.
           05  WS-ERCOMP-RECS-IN       PIC 9(9)      VALUE ZEROS.
           05  WS-ERACCT-RECS-FIX      PIC 9(9)      VALUE ZEROS.
           05  WS-ERCOMP-RECS-FIX      PIC 9(9)      VALUE ZEROS.
           05  WS-ERACCT-RECS-ADD      PIC 9(9)      VALUE ZEROS.
           05  WS-ERACCT-RECS-DEL      PIC 9(9)      VALUE ZEROS.
           05  WS-DISPLAY-CNT          PIC Z,ZZZ,ZZ9 VALUE ZEROS.
           05  WS-CURRENT-AM-KEY       PIC X(19)     VALUE LOW-VALUES.

       01  T1                          PIC S9(5)  COMP-3 VALUE +0.
       01  T1M                         PIC S9(5)  COMP-3 VALUE +0.
       01  WS-TABLE-1.
           05  WS-CSR-TABLE OCCURS 430 INDEXED BY S1
              ASCENDING KEY IS WS-TBL1-KEY.
               10  WS-TBL1-KEY.
                   15  WS-TBL1-CARRIER PIC X.
                   15  WS-TBL1-STATE   PIC XX.
                   15  WS-TBL1-ACCT    PIC X(10).
               10  WS-TBL1-CSR         PIC X(4).
               10  WS-TBL1-BUS         PIC XX.

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

       0000-LOAD-DATE-CARD.            COPY ELCDTERX.

       0000-MAINLINE.

           PERFORM 0100-OPEN-FILES     THRU 0100-EXIT
           PERFORM 0200-INITIALIZE     THRU 0200-EXIT
           PERFORM 0300-LOAD-TABLES    THRU 0300-EXIT
           PERFORM 0500-PROCESS        THRU 0500-EXIT UNTIL
              END-OF-ERACCT
           PERFORM 3000-CLOSE-FILES    THRU 3000-EXIT
           PERFORM 4000-FINAL-TOTALS   THRU 4000-EXIT
           GOBACK

           .
       0000-EXIT.
           EXIT.

       0100-OPEN-FILES.

PEMTST     OPEN I-O   ERACCT ERCOMP
PEMTST*    OPEN INPUT ERACCT
           OPEN INPUT CSR-FILE

           IF ERACCT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERACCT - OPEN ' ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           IF ERCOMP-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERCOMP - OPEN ' ERCOMP-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-INITIALIZE.

           MOVE SPACES                 TO WS-ERACCT-SW
                                          WS-ERCOMP-SW
           MOVE ZEROS                  TO WS-ERACCT-RECS-IN
                                          WS-ERACCT-RECS-FIX
                                          WS-ERACCT-RECS-ADD
                                          WS-ERACCT-RECS-DEL
                                          WS-ERCOMP-RECS-IN
                                          WS-ERCOMP-RECS-FIX

           PERFORM 1100-START-ERACCT   THRU 1100-EXIT
           PERFORM 1200-READNEXT-ERACCT
                                       THRU 1200-EXIT

           .
       0200-EXIT.
           EXIT.

       0300-LOAD-TABLES.

           PERFORM VARYING T1 FROM +1 BY +1 UNTIL
              END-OF-TABLE
              READ CSR-FILE AT END
                 SET END-OF-TABLE      TO TRUE
              END-READ
              IF NOT END-OF-TABLE
                 MOVE CSR-CARRIER      TO WS-TBL1-CARRIER (T1)
                 MOVE CSR-STATE        TO WS-TBL1-STATE   (T1)
                 MOVE CSR-ACCOUNT      TO WS-TBL1-ACCT    (T1)
                 MOVE CSR-CSR          TO WS-TBL1-CSR     (T1)
                 MOVE CSR-BUS-GRP      TO WS-TBL1-BUS     (T1)
              ELSE
                 MOVE 'ZZZZZZZZZZZZZ'  TO WS-TBL1-KEY     (T1)
              END-IF
           END-PERFORM

           SUBTRACT +2                 FROM T1
           MOVE T1                     TO T1M
           DISPLAY ' NUMBER OF  CSR  RECORDS ' T1

           MOVE ' '                    TO WS-TABLE-EOF-SW
           MOVE +1                     TO T1

           .
       0300-EXIT.
           EXIT.

       0500-PROCESS.

           PERFORM 1000-PROCESS        THRU 1000-EXIT

           PERFORM 1200-READNEXT-ERACCT
                                       THRU 1200-EXIT

           .
       0500-EXIT.
           EXIT.

       1000-PROCESS.

           MOVE SPACES                 TO WS-CSR
           MOVE AM-CARRIER             TO WS-CARR
           MOVE AM-STATE               TO WS-STATE
           MOVE AM-ACCOUNT             TO WS-ACCOUNT

           SEARCH ALL WS-CSR-TABLE AT END
              MOVE SPACES              TO WS-CSR
            WHEN WS-AM-KEY = WS-TBL1-KEY (S1)
               MOVE WS-TBL1-CSR (S1)   TO WS-CSR
           END-SEARCH

           IF WS-CSR NOT = SPACES
      *       IF WS-TBL1-BUS (S1) NOT = AM-GPCD
      *          DISPLAY ' BUS TYPE NOT EQUAL ' WS-AM-KEY ' '
      *            WS-TBL1-BUS (S1) ' ' AM-GPCD
      *       END-IF
              MOVE WS-CSR              TO AM-CSR-CODE
              PERFORM 1500-UPDATE-ERCOMP
                                       THRU 1500-EXIT
              PERFORM 2100-REWRITE-ERACCT
                                       THRU 2100-EXIT
      *    ELSE
      *       DISPLAY ' NO  TABLE REC FOR ERACCT ' WS-AM-KEY
           END-IF

           .
       1000-EXIT.
           EXIT.

       1100-START-ERACCT.

           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD

           START ERACCT KEY >= AM-CONTROL-PRIMARY

           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERACCT - START ' ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       1100-EXIT.
           EXIT.

       1200-READNEXT-ERACCT.

           READ ERACCT NEXT RECORD

           IF (ERACCT-FILE-STATUS = '10' OR '23')
              OR (AM-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              DISPLAY ' REACHING END ' ERACCT-FILE-STATUS
              SET END-OF-ERACCT        TO TRUE
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERACCT - READNEXT '
                    ERACCT-FILE-STATUS
                 PERFORM 9999-ABEND-RTN
              ELSE
                 ADD 1                 TO WS-ERACCT-RECS-IN
              END-IF
           END-IF

           .
       1200-EXIT.
           EXIT.

       1300-READ-ERCOMP.

           READ ERCOMP

           IF (ERCOMP-FILE-STATUS = '10' OR '23')
              DISPLAY ' ERROR - ERCOMP - READ ' ERCOMP-FILE-STATUS
                 '  ' CO-CONTROL CO-TYPE
           ELSE
              IF ERCOMP-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERCOMP - READ '
                    ERCOMP-FILE-STATUS
                 PERFORM 9999-ABEND-RTN
              ELSE
                 ADD 1                 TO WS-ERCOMP-RECS-IN
              END-IF
           END-IF

           .
       1300-EXIT.
           EXIT.

       1400-REWRITE-ERCOMP.

           DISPLAY 'ERCOMP REWRITE ' CO-CONTROL ' ' CO-CSR-CODE
      *    DISPLAY ' '

PEMTST     REWRITE COMPENSATION-MASTER
PEMTST*    MOVE '00' TO ERCOMP-FILE-STATUS

           IF ERCOMP-FILE-STATUS = '00'
              ADD 1                    TO WS-ERCOMP-RECS-FIX
           ELSE
              DISPLAY ' ERROR - ERCOMP - REWRITE ' ERCOMP-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       1400-EXIT.
           EXIT.

       1500-UPDATE-ERCOMP.

           MOVE AM-COMPANY-CD          TO CO-COMPANY-CD
           MOVE AM-CARRIER             TO CO-CARRIER
           MOVE AM-GROUPING            TO CO-GROUPING
           MOVE AM-ACCOUNT             TO CO-ACCOUNT
           MOVE 'A'                    TO CO-TYPE
           IF AM-REMIT-TO NOT NUMERIC
              MOVE ZEROS               TO AM-REMIT-TO
           END-IF
           IF AM-REMIT-TO = ZEROS
              MOVE 01                  TO AM-REMIT-TO
           END-IF
           MOVE AM-AGT (AM-REMIT-TO)   TO CO-RESP-NO
           IF CO-CONTROL-PRIMARY NOT = WS-CO-PREV-KEY
              MOVE CO-CONTROL-PRIMARY  TO WS-CO-PREV-KEY
              PERFORM 1300-READ-ERCOMP THRU 1300-EXIT
              IF ERCOMP-FILE-STATUS = '00'
                 MOVE AM-CSR-CODE      TO CO-CSR-CODE
                 PERFORM 1400-REWRITE-ERCOMP
                                       THRU 1400-EXIT
              END-IF
           END-IF

           .
       1500-EXIT.
           EXIT.

       2100-REWRITE-ERACCT.

           DISPLAY 'ERACCT REWRITE ' AM-CONTROL-A ' ' AM-CSR-CODE
      *    DISPLAY ' '

PEMTST     REWRITE ACCOUNT-MASTER
PEMTST*    MOVE '00' TO ERACCT-FILE-STATUS

           IF ERACCT-FILE-STATUS = '00'
              ADD 1                    TO WS-ERACCT-RECS-FIX
           ELSE
              DISPLAY ' ERROR - ERACCT - REWRITE ' ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       2100-EXIT.
           EXIT.

       2200-WRITE-ERACCT.

           DISPLAY 'ERACCT   WRITE ' AM-CONTROL-A
           DISPLAY ' '

           WRITE ACCOUNT-MASTER

           IF ERACCT-FILE-STATUS = '00'
              ADD 1                    TO WS-ERACCT-RECS-ADD
           ELSE
              DISPLAY ' ERROR - ERACCT - WRITE ' ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       2200-EXIT.
           EXIT.

       2300-DELETE-ERACCT.

           DISPLAY 'ERACCT  DELETE ' AM-CONTROL-A
           DISPLAY ' '

           DELETE ERACCT

           IF ERACCT-FILE-STATUS = '00'
              ADD 1                    TO WS-ERACCT-RECS-DEL
      *       SET END-OF-ERACCT        TO TRUE
           ELSE
              DISPLAY ' ERROR - ERACCT - DELETE ' ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       2300-EXIT.
           EXIT.

       3000-CLOSE-FILES.

           CLOSE ERACCT CSR-FILE ERCOMP

           IF ERACCT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERACCT - CLOSE ' ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           IF ERCOMP-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERCOMP - CLOSE ' ERCOMP-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       3000-EXIT.
           EXIT.

       4000-FINAL-TOTALS.

           DISPLAY '**************************************************'
           DISPLAY '***                                            ***'

           MOVE WS-ERACCT-RECS-IN      TO WS-DISPLAY-CNT
           DISPLAY '***  ERACCT MASTER RECS READ     = ' WS-DISPLAY-CNT

           MOVE WS-ERACCT-RECS-FIX     TO WS-DISPLAY-CNT
           DISPLAY '***  ERACCT MASTER RECS FIXED    = ' WS-DISPLAY-CNT

           MOVE WS-ERCOMP-RECS-IN      TO WS-DISPLAY-CNT
           DISPLAY '***  ERCOMP MASTER RECS READ     = ' WS-DISPLAY-CNT

           MOVE WS-ERCOMP-RECS-FIX     TO WS-DISPLAY-CNT
           DISPLAY '***  ERCOMP MASTER RECS FIXED    = ' WS-DISPLAY-CNT

           MOVE WS-ERACCT-RECS-ADD     TO WS-DISPLAY-CNT
           DISPLAY '***  ERACCT MASTER RECS ADDED    = ' WS-DISPLAY-CNT

           MOVE WS-ERACCT-RECS-DEL     TO WS-DISPLAY-CNT
           DISPLAY '***  ERACCT MASTER RECS DELETED  = ' WS-DISPLAY-CNT

           DISPLAY '***                                            ***'
           DISPLAY '**************************************************'

           .
       4000-EXIT.
           EXIT.

       9999-ABEND-RTN.

           DISPLAY '*** ACCOUNT MSTR CONVERSION PROGRAM ABENDING ***'
           DISPLAY '************************************************'

           MOVE 1                      TO WS-ABEND-FLD
           MOVE 0                      TO WS-ZERO-FLD

           COMPUTE WS-ABEND-FLD = WS-ABEND-FLD / WS-ZERO-FLD

           .
       9999-EXIT.
           EXIT.
       ABEND-PGM.
                                       COPY ELCABEND.
