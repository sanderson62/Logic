       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDEAB1.
       AUTHOR.        PABLO.
       DATE-COMPILED.
      *REMARKS.
      
      *   THIS PROGRAM READS AN EXTRACT FILE FROM AN ACCESS EXTRACT
      *   FROM CPSPRINT.MDB AND POPULATES THE EREADR FILE.


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EXTR-IN      ASSIGN TO SYS010
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ERCOMP       ASSIGN TO ERCOMP
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERCOMP-FILE-STATUS
                               RECORD KEY IS CO-CONTROL-PRIMARY.

           SELECT ERACCT3      ASSIGN TO ERACCT3
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERACCT-FILE-STATUS
                               RECORD KEY IS AM-VG-KEY3.

           SELECT EREADR       ASSIGN TO EREADR
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS EREADR-FILE-STATUS
                               RECORD KEY IS EA-CONTROL-PRIMARY.

           SELECT DISK-DATE    ASSIGN TO SYS019.
                                                                        

       DATA DIVISION.
       FILE SECTION.

       FD  EXTR-IN
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

       01  EXTR-IN-REC                 PIC X(100).

       FD  ERCOMP.
                                       COPY ERCCOMP.

       FD  ERACCT3.
                                       COPY ERCACCT.

       FD  EREADR.
                                       COPY ERCEADR.
       FD  DISK-DATE                                                    
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CIDEAB1 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  WS-WORK-SEQ                 PIC S9(7) COMP-3 VALUE +0.
       77  ERCOMP-FILE-STATUS          PIC XX   VALUE '00'.
       77  ERACCT-FILE-STATUS          PIC XX   VALUE '00'.
       77  EREADR-FILE-STATUS          PIC XX   VALUE '00'.
       77  WS-CURRENT-BIN-DATE         PIC XX   VALUE LOW-VALUES.
       77  WS-NEXT-CYCLE-BIN-DATE      PIC XX   VALUE LOW-VALUES.
       77  WS-SELECT-BIN-DATE          PIC XX   VALUE LOW-VALUES.
       77  WS-STOP-SW                  PIC X   VALUE SPACES.
           88  I-SAY-TO-STOP               VALUE 'Y'.
       77  WS-FIND-SW                  PIC X   VALUE SPACES.
           88  I-FOUND-IT                  VALUE 'Y'.
       77  WS-HYPHEN-CNTR              PIC S999 COMP-3 VALUE +0.
       77  I1                          PIC S999 COMP-3 VALUE +0.
       77  O1                          PIC S999 COMP-3 VALUE +0.
       77  WS-END-SW                   PIC X   VALUE SPACES.
           88  END-OF-ACCOUNT             VALUE 'Y'.
       77  WS-RANGE-SW                 PIC X   VALUE SPACES.
           88  END-OF-RANGE               VALUE 'Y'.
       77  WS-PREV-CARRIER             PIC X  VALUE SPACES.
       77  WS-PREV-ACCOUNT             PIC X(10) VALUE SPACES.
       77  WS-SAVE-RESP-NO             PIC X(10) VALUE SPACES.
       77  WS-SAVE-ACCOUNT             PIC X(10) VALUE SPACES.
       01  WS-MISC.
           05  WS-CHECK-AMT            PIC X(09).
           05  WS-CHECK-AMT-N REDEFINES WS-CHECK-AMT
                                       PIC 9(07)V99.
           05  WS-EOF-SW               PIC X      VALUE SPACES.
               88  END-OF-INPUT                   VALUE 'Y'.
           05  WS-EXTR-IN              PIC 9(7)   VALUE ZEROS.
           05  WS-EREADR-OUT           PIC 9(7)   VALUE ZEROS.

       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

       01  WORK-ABEND-CODE.
           12  WAC-1                   PIC X.
           12  WAC-2                   PIC X.
           12  WAC-3-4.
               16  WAC-3               PIC X.
               16  WAC-4               PIC X.


       01  EXTRACT-RECORD.
           12  EX-LO-ACCOUNT           PIC X(10).
           12  F                       PIC X.
           12  EX-HI-ACCOUNT           PIC X(10).
           12  F                       PIC X.
           12  EX-NAME                 PIC X(30).
           12  F                       PIC X.
           12  EX-EMAIL-ADDR           PIC X(45).
           12  F                       PIC X.
           12  EX-EOR                  PIC X.


       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

                                       COPY ELCFUNDT.
                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.
                                       COPY ELCDTERX.

       0000-BEGIN.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT
           PERFORM 0040-INIT           THRU 0040-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
                (END-OF-INPUT)
PEMTST*         OR (WS-EXTR-IN > 1000)

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY ' RECORDS IN    ' WS-EXTR-IN
           DISPLAY ' RECORDS  OUT  ' WS-EREADR-OUT
           GOBACK

           .
       0002-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT EXTR-IN ERCOMP ERACCT3
               I-O    EREADR
               
           IF ERCOMP-FILE-STATUS  = '00'  OR  '97'                     
              CONTINUE
           ELSE                                                       
              DISPLAY ' BAD OPEN FOR ERCOMP ' ERCOMP-FILE-STATUS       
              PERFORM ABEND-PGM
           END-IF

           IF ERACCT-FILE-STATUS  = '00'  OR  '97'                     
              CONTINUE
           ELSE                                                       
              DISPLAY ' BAD OPEN - ERACCT3 ' ERACCT-FILE-STATUS       
              PERFORM ABEND-PGM
           END-IF

           IF EREADR-FILE-STATUS  = '00'  OR  '97'                     
              CONTINUE
           ELSE                                                       
              DISPLAY ' BAD OPEN FOR EREADR ' EREADR-FILE-STATUS       
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE EXTR-IN EREADR ERCOMP ERACCT3

           IF EREADR-FILE-STATUS  = '00'
              CONTINUE
           ELSE                                                       
              DISPLAY ' BAD CLOSE FOR EREADR ' EREADR-FILE-STATUS       
              PERFORM ABEND-PGM
           END-IF
               
           IF ERCOMP-FILE-STATUS  = '00'
              CONTINUE
           ELSE                                                       
              DISPLAY ' BAD CLOSE - ERCOMP ' ERCOMP-FILE-STATUS       
              PERFORM ABEND-PGM
           END-IF

           IF ERACCT-FILE-STATUS  = '00'
              CONTINUE
           ELSE                                                       
              DISPLAY ' BAD CLOSE - ERACCT3 ' ERACCT-FILE-STATUS       
              PERFORM ABEND-PGM
           END-IF

           .
       0030-EXIT.
           EXIT.

       0040-INIT.

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           DISPLAY ' FUNCTION DATE CYMD ' WS-FN-DATE
           MOVE WS-FN-DATE             TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-CURRENT-BIN-DATE
           ELSE
              DISPLAY ' ERROR CONVERTING CURRENT DATE '
           END-IF

           PERFORM 0110-READ-INPUT     THRU 0110-EXIT

           .
       0040-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           IF EX-LO-ACCOUNT > EX-HI-ACCOUNT
              DISPLAY ' RANGE MESSED UP ON ' EXTRACT-RECORD (1:50)
           ELSE
              IF EX-LO-ACCOUNT NOT = EX-HI-ACCOUNT
                 PERFORM 0060-RANGE    THRU 0060-EXIT
              ELSE
                 PERFORM 0070-SINGLE   THRU 0070-EXIT
              END-IF
           END-IF

           PERFORM 0110-READ-INPUT     THRU 0110-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-RANGE.

           MOVE SPACES                 TO WS-SAVE-RESP-NO
                                          WS-SAVE-ACCOUNT
                                          WS-END-SW
                                          WS-RANGE-SW
           MOVE EX-LO-ACCOUNT          TO AM-VG3-ACCOUNT
                                          WS-PREV-ACCOUNT
           MOVE LOW-VALUES             TO AM-VG3-EXP-DT
           START ERACCT3 KEY >= AM-VG-KEY3
           IF ERACCT-FILE-STATUS = '10' OR '23'
              DISPLAY ' NO ERACCT3 RECORD FOR ' EXTRACT-RECORD (1:50)
              GO TO 0070-EXIT
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY ' BAD START - ERACCT3 ' ERACCT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0060-READ.

           READ ERACCT3 NEXT RECORD

           .
       0060-EVALUATE.

           EVALUATE TRUE
              WHEN (ERACCT-FILE-STATUS = '10' OR '23')
                 OR (AM-ACCOUNT > EX-HI-ACCOUNT)
                 SET END-OF-RANGE      TO TRUE
                 SET END-OF-ACCOUNT    TO TRUE
              
              WHEN ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY ' BAD START - ERACCT ' ERACCT-FILE-STATUS
                 PERFORM ABEND-PGM

              WHEN AM-ACCOUNT > WS-PREV-ACCOUNT
                 SET END-OF-ACCOUNT    TO TRUE

              WHEN (AM-COMPANY-CD = DTE-CLASIC-COMPANY-CD)
                 AND (AM-ACCOUNT = WS-PREV-ACCOUNT)
                 MOVE AM-CARRIER       TO WS-PREV-CARRIER
                 IF AM-REMIT-TO NOT NUMERIC
                    MOVE 01            TO AM-REMIT-TO
                 END-IF
                 MOVE AM-AGT (AM-REMIT-TO)
                                       TO WS-SAVE-RESP-NO
                 MOVE AM-AGT (1)       TO WS-SAVE-ACCOUNT
           END-EVALUATE

           IF NOT END-OF-ACCOUNT
              GO TO 0060-READ
           END-IF

           IF WS-SAVE-RESP-NO = SPACES
              DISPLAY ' COULD NOT FIND ERACCT3 ON ' WS-PREV-ACCOUNT
                ' ' EXTRACT-RECORD (1:50)
              GO TO 0060-CONTINUE
           END-IF

           MOVE DTE-CLASIC-COMPANY-CD  TO CO-CONTROL-PRIMARY
           MOVE WS-PREV-CARRIER        TO CO-CARRIER
           MOVE '000000'               TO CO-GROUPING
           MOVE WS-SAVE-RESP-NO        TO CO-RESP-NO
           MOVE WS-SAVE-ACCOUNT        TO CO-ACCOUNT
           MOVE 'A'                    TO CO-TYPE

           READ ERCOMP

           IF ERCOMP-FILE-STATUS = '10' OR '23'
              DISPLAY ' COULD NOT FIND ERCOMP ON ' WS-SAVE-RESP-NO
                ' ' EXTRACT-RECORD (1:50)
              GO TO 0060-EXIT
           ELSE
              IF ERCOMP-FILE-STATUS NOT = '00'
                 DISPLAY ' BAD READ  FOR ERCOMP ' ERCOMP-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           PERFORM 0080-UPDATE-EMAIL-ADDRESS
                                       THRU 0080-EXIT
           .
       0060-CONTINUE.

           IF END-OF-RANGE
              GO TO 0060-EXIT
           END-IF

           MOVE AM-ACCOUNT             TO WS-PREV-ACCOUNT
           MOVE AM-CARRIER             TO WS-PREV-CARRIER
           MOVE SPACES                 TO WS-SAVE-RESP-NO
                                          WS-SAVE-ACCOUNT
                                          WS-END-SW

           GO TO 0060-EVALUATE

           .
       0060-EXIT.
           EXIT.

       0070-SINGLE.

           MOVE SPACES                 TO WS-SAVE-RESP-NO
                                          WS-SAVE-ACCOUNT
                                          WS-END-SW
           MOVE EX-LO-ACCOUNT          TO AM-VG3-ACCOUNT
           MOVE LOW-VALUES             TO AM-VG3-EXP-DT
           START ERACCT3 KEY >= AM-VG-KEY3
           IF ERACCT-FILE-STATUS = '10' OR '23'
              DISPLAY ' NO ERACCT3 RECORD FOR ' EXTRACT-RECORD (1:50)
              GO TO 0070-EXIT
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY ' BAD START - ERACCT3 ' ERACCT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0070-READ.

           READ ERACCT3 NEXT RECORD

           IF (ERACCT-FILE-STATUS = '10' OR '23')
              OR (AM-ACCOUNT NOT = EX-LO-ACCOUNT)
              SET END-OF-ACCOUNT TO TRUE
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY ' BAD START - ERACCT ' ERACCT-FILE-STATUS
                 PERFORM ABEND-PGM
              ELSE
                 IF (AM-COMPANY-CD = DTE-CLASIC-COMPANY-CD)
                    AND (AM-ACCOUNT = EX-LO-ACCOUNT)
                    MOVE AM-CARRIER       TO WS-PREV-CARRIER
                    IF AM-REMIT-TO NOT NUMERIC
                       MOVE 01            TO AM-REMIT-TO
                    END-IF
                    MOVE AM-AGT (AM-REMIT-TO)
                                       TO WS-SAVE-RESP-NO
                    MOVE AM-AGT (1)    TO WS-SAVE-ACCOUNT
                 END-IF
              END-IF
           END-IF

           IF NOT END-OF-ACCOUNT
              GO TO 0070-READ
           END-IF

           IF WS-SAVE-RESP-NO = SPACES
              DISPLAY ' COULD NOT FIND ERACCT3 ON '
                EXTRACT-RECORD (1:50)
              GO TO 0070-EXIT
           END-IF

           MOVE DTE-CLASIC-COMPANY-CD  TO CO-CONTROL-PRIMARY
           MOVE WS-PREV-CARRIER        TO CO-CARRIER
           MOVE '000000'               TO CO-GROUPING
           MOVE WS-SAVE-RESP-NO        TO CO-RESP-NO
           MOVE WS-SAVE-ACCOUNT        TO CO-ACCOUNT
           MOVE 'A'                    TO CO-TYPE

           READ ERCOMP

           IF ERCOMP-FILE-STATUS = '10' OR '23'
              DISPLAY ' COULD NOT FIND ERCOMP ON ' WS-SAVE-RESP-NO
                ' ' EXTRACT-RECORD (1:50)
              GO TO 0070-EXIT
           ELSE
              IF ERCOMP-FILE-STATUS NOT = '00'
                 DISPLAY ' BAD READ  FOR ERCOMP ' ERCOMP-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           PERFORM 0080-UPDATE-EMAIL-ADDRESS
                                       THRU 0080-EXIT

           .
       0070-EXIT.
           EXIT.

       0080-UPDATE-EMAIL-ADDRESS.

           MOVE CO-COMPANY-CD          TO EA-CONTROL-PRIMARY
           MOVE 'CO'                   TO EA-RECORD-TYPE
           MOVE CO-CONTROL-PRIMARY (2:28)
                                       TO EA-ERCOMP-EMAIL-KEY
           READ EREADR
           IF EREADR-FILE-STATUS = '00'
              PERFORM VARYING I1 FROM +1 BY +1 UNTIL
                 (I1 > +10)
                 OR (EA-PERSONS-EMAIL (I1) = SPACES)
                 OR ((EX-NAME = EA-PERSONS-NAME (I1))
                  AND (EX-EMAIL-ADDR = EA-PERSONS-EMAIL (I1)))
              END-PERFORM
              IF I1 < +11
                 MOVE EX-NAME          TO EA-PERSONS-NAME (I1)
                 MOVE EX-EMAIL-ADDR    TO EA-PERSONS-EMAIL (I1)
                 PERFORM 0085-REWRITE-EREADR
                                       THRU 0085-EXIT
              ELSE
                 DISPLAY ' MORE THAN 10 EMAILS FOR '
                    EA-ERCOMP-EMAIL-KEY
              END-IF
              GO TO 0080-EXIT
           ELSE
              IF EREADR-FILE-STATUS NOT = '23'
                 DISPLAY ' BAD READ  FOR EREADR ' EREADR-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           MOVE 'EA'                   TO EMAIL-ADDRESS
           MOVE CO-COMPANY-CD          TO EA-COMPANY-CD
           MOVE 'CO'                   TO EA-RECORD-TYPE
           MOVE CO-CONTROL-PRIMARY (2:28)
                                       TO EA-ERCOMP-EMAIL-KEY
           
           MOVE EX-NAME                TO EA-PERSONS-NAME (1)
           MOVE EX-EMAIL-ADDR          TO EA-PERSONS-EMAIL (1)
           PERFORM 0090-WRITE-EREADR   THRU 0090-EXIT

           .
       0080-EXIT.
           EXIT.

       0085-REWRITE-EREADR.

           REWRITE EMAIL-ADDRESS

           IF EREADR-FILE-STATUS  = '00'
              ADD 1                    TO WS-EREADR-OUT
           ELSE
              DISPLAY ' BAD REWRITE FOR EREADR ' EREADR-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0085-EXIT.
           EXIT.

       0090-WRITE-EREADR.

           WRITE EMAIL-ADDRESS

           IF EREADR-FILE-STATUS  = '00'
              ADD 1                    TO WS-EREADR-OUT
           ELSE
              DISPLAY ' BAD WRITE FOR EREADR ' EREADR-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0090-EXIT.
           EXIT.

       0110-READ-INPUT.

           READ EXTR-IN INTO EXTRACT-RECORD AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD +1                   TO WS-EXTR-IN
           END-IF

           .
       0110-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.

