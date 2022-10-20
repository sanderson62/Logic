       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPSCOU1.
       AUTHOR.     PABLO
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
020310* 020310  CR2008100900004  PEMA  ADD REF4 EXTRACT PROCESSING
120710* 120710  CR2010050400001  PEMA  ADD ZERO E TO EXTRACT
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILE-IN   ASSIGN TO SYS010
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE ASSIGN TO SYS019.

           SELECT ERCOMP    ASSIGN ERCOMP
                            ORGANIZATION IS INDEXED
                            ACCESS IS DYNAMIC
                            RECORD KEY IS CO-CONTROL-PRIMARY
                            FILE STATUS IS ERCOMP-FILE-STATUS.
           SELECT PRINTER   ASSIGN TO SYS008.

       DATA DIVISION.

       FILE SECTION.

       FD  FILE-IN
           RECORDING MODE IS F
           RECORD CONTAINS 57 CHARACTERS
           LABEL RECORDS ARE OMITTED.

       01  CPS-KEYS-REC.
           05  SAV-CID-CC              PIC 99.
           05  SAV-CID-YY              PIC 99.
           05  SAV-CID-MM              PIC 99.
           05  CID-SCAN-SEQ-NO         PIC 9(7).
           05  SR-DEL1                 PIC X.
           05  CID-STMT-TYPE           PIC X.
           05  SR-DEL2                 PIC X.
           05  CID-CARRIER             PIC X.
           05  SR-DEL3                 PIC X.
           05  CID-GROUP               PIC X(6).
           05  SR-DEL4                 PIC X.
           05  CID-FIN-RESP            PIC X(10).
           05  SR-DEL5                 PIC X.
           05  CID-ACCOUNT             PIC X(10).
           05  SR-DEL6                 PIC X.
           05  CID-AMT-DUE             PIC 9(7).99.


       FD  ERCOMP
           LABEL RECORDS ARE STANDARD.
                                       COPY ERCCOMP.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  PRINTER
                                       COPY ELCPRTFD.

       WORKING-STORAGE SECTION.

       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CPSCOU1 WORKING-STORAGE      '.
       77  FILLER  PIC X(32) VALUE '************ VM 2.001 **********'.

       01  WORK-AREAS.
           12  WS-RETURN-CODE          PIC XXXX.
           12  WS-EOF-SW               PIC X VALUE SPACES.
               88  END-OF-INPUT             VALUE 'Y'.
           12  WS-CURRENT-BIN-DT      PIC XX.
           12  WS-ABEND-MESSAGE       PIC X(80).
           12  WS-ABEND-FILE-STATUS   PIC XX  VALUE ZEROS.
           12  WS-ZERO                PIC S9  VALUE ZERO COMP-3.
           12  ERCOMP-FILE-STATUS     PIC XX  VALUE ZEROS.

           12  ERROR-SW               PIC X    VALUE SPACE.
               88  ERROR-OCCURRED              VALUE 'E'.
               88  NO-ERRORS                   VALUE ' '.
           12  WS-SAVE-PRINT-RECORD   PIC X(133) VALUE SPACES.
       01  WS-COMMENT-2.
           05  WS-STMT-TYPE            PIC X(7)  VALUE SPACES.
           05  FILLER                  PIC X(16) VALUE
                                           'AUTO BALANCED - '.
           05  WS-COMMENT-DATE         PIC X(10).

       01  DTE-INTERFACE-CODES.
           05  X                 PIC X           VALUE SPACE.
           05  PGM-SUB           PIC S9(4)  COMP VALUE +504.
           05  ABEND-CODE        PIC 9999        VALUE ZERO.
           05  ABEND-OPTION      PIC X           VALUE SPACE.
           05  OLC-REPORT-NAME   PIC X(6)        VALUE 'EL504'.

           05 WS-IN              PIC 9(5) VALUE ZEROS.
           05 WS-OUT             PIC 9(5) VALUE ZEROS.

       01  COMP-3-WORK-AREA.
           05  K1                 PIC S9(7)  VALUE +1.
           05  K2                 PIC S9(7)  VALUE +2.
           05  WS-COMP-RECS       PIC S9(7)  VALUE +0.
           05  RECORD-COUNT       PIC S9(7)  VALUE +0.
           05  LINE-CNT           PIC S9(7)  VALUE +0.
           05  DELETE-COUNT       PIC S9(7)  VALUE +0.
           05  DATE-ERROR-COUNT   PIC S9(7)  VALUE +0.
                                   COPY ELCDTECX.

                                   COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0100-INITIALIZE     THRU 0100-EXIT

           PERFORM 0300-PROCESS-INPUT  THRU 0300-EXIT UNTIL
              END-OF-INPUT

           PERFORM 0600-FINALIZE       THRU 0600-EXIT

           DISPLAY ' INPUT RECORDS ' WS-COMP-RECS

           GOBACK

           .
       0100-INITIALIZE.

           OPEN INPUT FILE-IN
              OUTPUT PRINTER

           OPEN I-O   ERCOMP

           IF ERCOMP-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              MOVE ERCOMP-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              MOVE ' ERROR - ERCOMP - OPEN   '
                                       TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF

           STRING RUN-MO '/' RUN-DA '/' RUN-CCYY 
              DELIMITED BY SIZE INTO WS-COMMENT-DATE
           END-STRING

           PERFORM 0200-READ-INPUT     THRU 0200-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-INPUT.

           READ FILE-IN AT END
               SET END-OF-INPUT        TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD +1                   TO WS-COMP-RECS
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-PROCESS-INPUT.

           MOVE LOW-VALUES             TO CO-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO CO-COMPANY-CD
           MOVE CID-CARRIER            TO CO-CARRIER
           MOVE CID-GROUP              TO CO-GROUPING
           MOVE CID-FIN-RESP           TO CO-RESP-NO
           MOVE CID-ACCOUNT            TO CO-ACCOUNT
           MOVE 'A'                    TO CO-TYPE

           READ ERCOMP
           IF ERCOMP-FILE-STATUS = '00'
              EVALUATE CID-STMT-TYPE
                 WHEN 'P'
                    MOVE 'PEND - '        TO WS-STMT-TYPE
                    MOVE WS-COMMENT-2     TO CO-GA-COMMENT-2
                    DISPLAY ' ABOUT TO UPDATE COMMENT 2 - PENDING '
                       CPS-KEYS-REC
                 WHEN '1'
                    MOVE 'REF1 - '        TO WS-STMT-TYPE
                    MOVE WS-COMMENT-2     TO CO-GA-COMMENT-2
                    DISPLAY ' ABOUT TO UPDATE COMMENT 2 - REFUND1 '
                       CPS-KEYS-REC
120710           WHEN '2'
120710              MOVE 'ZERO E-'        TO WS-STMT-TYPE
120710              MOVE WS-COMMENT-2     TO CO-GA-COMMENT-2
120710              DISPLAY ' ABOUT TO UPDATE COMMENT 2 - ZERO E  '
120710                 CPS-KEYS-REC
                 WHEN '4'
                    MOVE 'REF4 - '        TO WS-STMT-TYPE
                    MOVE WS-COMMENT-2     TO CO-GA-COMMENT-2
                    DISPLAY ' ABOUT TO UPDATE COMMENT 2 - REFUND4 '
                       CPS-KEYS-REC
              END-EVALUATE
              REWRITE COMPENSATION-MASTER
              IF ERCOMP-FILE-STATUS = '00'
                 CONTINUE
              ELSE
                 DISPLAY ' ERROR - ERCOMP - REWRITE '
                    ERCOMP-FILE-STATUS ' ' CPS-KEYS-REC
              END-IF
           ELSE
              DISPLAY ' ERROR - ERCOMP - READ '
                 ERCOMP-FILE-STATUS ' ' CPS-KEYS-REC
           END-IF

           PERFORM 0200-READ-INPUT     THRU 0200-EXIT

           .
       0300-EXIT.
           EXIT.

       0600-FINALIZE.

           CLOSE FILE-IN
                 ERCOMP
                 PRINTER

           .
       0600-EXIT.
           EXIT.

       ABEND-PGM  SECTION.  COPY ELCABEND  SUPPRESS.

