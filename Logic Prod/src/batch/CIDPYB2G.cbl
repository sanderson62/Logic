       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDPYB2G
       AUTHOR.        PABLO.
       DATE-COMPILED.
      *REMARKS.
      
      *   THIS PROGRAM READS AN EXTRACT FILE FROM A .NET APPLICATION
      *   AND CREATES ERPYAJ RECORDS.

060815******************************************************************
060815*                   C H A N G E   L O G
060815*
060815* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
060815*-----------------------------------------------------------------
060815*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
060815* EFFECTIVE    NUMBER
060815*-----------------------------------------------------------------
060815* 060815  IR2015060500002  PEMA  ADD action indicator processing
062121* 062121  CR2020060800001  PEMA  Add additional processing for CR.
060815******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EXTR-IN      ASSIGN TO SYS010
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ERPYAJ       ASSIGN TO ERPYAJ
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERPYAJ-FILE-STATUS
                               RECORD KEY IS PY-CONTROL-PRIMARY.

           SELECT DISK-DATE    ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  EXTR-IN
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

       01  EXTR-IN-REC                 PIC X(293).

       FD  ERPYAJ.
                                       COPY ERCPYAJ.
       FD  DISK-DATE                                                    
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '    CIDPYB2G WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  WS-WORK-SEQ                 PIC S9(7) COMP-3 VALUE +0.
       77  ERPYAJ-FILE-STATUS          PIC XX   VALUE '00'.
       77  WS-CURRENT-BIN-DATE         PIC XX   VALUE LOW-VALUES.
       77  WS-NEXT-CYCLE-BIN-DATE      PIC XX   VALUE LOW-VALUES.
       77  WS-SELECT-BIN-DATE          PIC XX   VALUE LOW-VALUES.
       77  WS-HYPHEN-CNTR              PIC S999 COMP-3 VALUE +0.
       77  I1                          PIC S999 COMP-3 VALUE +0.
       77  O1                          PIC S999 COMP-3 VALUE +0.
       01  WS-MISC.
           05  WS-CHECK-AMT            PIC X(09).
           05  WS-CHECK-AMT-N REDEFINES WS-CHECK-AMT
                                       PIC 9(07)V99.
           05  WS-EOF-SW               PIC X      VALUE SPACES.
               88  END-OF-INPUT                   VALUE 'Y'.
           05  WS-EXTR-IN              PIC 9(7)   VALUE ZEROS.
           05  WS-ERPYAJ-OUT           PIC 9(7)   VALUE ZEROS.

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
           12  EX-ACCT-NAME            PIC X(30).
           12  EX-MAIL-NAME            PIC X(30).
           12  EX-ADDR-1               PIC X(30).
           12  EX-ADDR-2               PIC X(30).
           12  EX-ADDR-3               PIC X(29).
           12  EX-ZIP                  PIC 9(9).
           12  EX-INV-DATE             PIC X(10).
           12  EX-CHECK-AMT            PIC X(12).
           12  EX-GL-NUM               PIC X(10).
           12  EX-DIV                  PIC XXX.
           12  EX-CENTER               PIC X(5).
           12  EX-LOB                  PIC X(7).
           12  EX-STATE                PIC XX.
           12  EX-CARRIER              PIC X.
           12  EX-GROUPING             PIC X(6).
           12  EX-RESP-NO              PIC X(10).
           12  EX-ACCOUNT              PIC X(10).
           12  EX-SPEC-INST            PIC X(10).
060815     12  ex-filler-1             pic x(25).
060815     12  ex-filler-2             pic x.
060815     12  EX-ACTION-FLAG          PIC X.
060815     12  EX-EOR                  PIC X.

062121 01  filler.
062121     05  ws-work-date.
062121         10  ws-work-month       pic xx.
062121         10  fill                pic x value '/'.
062121         10  ws-work-day         pic xx.
062121         10  fill                pic x value '/'.
062121         10  ws-work-ccyy        pic xxxx.

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
           DISPLAY ' RECORDS  OUT  ' WS-ERPYAJ-OUT
           GOBACK

           .
       0002-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT EXTR-IN
               I-O    ERPYAJ
               
           IF ERPYAJ-FILE-STATUS  = '00'  OR  '97'                     
              CONTINUE
           ELSE                                                       
              DISPLAY ' BAD OPEN FOR ERPYAJ ' ERPYAJ-FILE-STATUS       
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE EXTR-IN ERPYAJ

           IF ERPYAJ-FILE-STATUS  = '00'
              CONTINUE
           ELSE                                                       
              DISPLAY ' BAD CLOSE FOR ERPYAJ ' ERPYAJ-FILE-STATUS       
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
              DISPLAY ' DAY OF WEEK = ' DC-DAY-OF-WEEK
              MOVE +1                  TO DC-ELAPSED-DAYS
              IF DC-DAY-OF-WEEK = 6
                 ADD +2                TO DC-ELAPSED-DAYS
              ELSE
                 IF DC-DAY-OF-WEEK = 7
                    ADD +1             TO DC-ELAPSED-DAYS
                 END-IF
              END-IF
              DISPLAY ' ELAPSED DAYS ' DC-ELAPSED-DAYS
              MOVE WS-CURRENT-BIN-DATE TO DC-BIN-DATE-1
              MOVE '6'                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-2    TO WS-NEXT-CYCLE-BIN-DATE
              ELSE
                 DISPLAY ' PROBLEMS CONVERTING NEXT DATE '
                 PERFORM ABEND-PGM
              END-IF
           ELSE
              DISPLAY ' PROBLEMS CONVERTING CURRENT DATE '
              PERFORM ABEND-PGM
           END-IF

           MOVE CLASIC-CREDIT-EOM-DT   TO WS-SELECT-BIN-DATE
           PERFORM 0110-READ-INPUT     THRU 0110-EXIT

           .
       0040-EXIT.
           EXIT.

       0050-PROCESS-FILE.

060815     if ex-action-flag = 'D'
060815        display ' bypassing dropped record ' EX-CARRIER ' '
060815              EX-GROUPING ' ' EX-RESP-NO ' ' EX-ACCOUNT
060815        go to 0050-continue
060815     end-if

           ADD +1                      TO WS-WORK-SEQ
           MOVE SPACES                 TO PENDING-PAY-ADJ
           MOVE 'PY'                   TO PY-RECORD-ID
           MOVE DTE-CLASIC-COMPANY-CD  TO PY-COMPANY-CD
           MOVE EX-CARRIER             TO PY-CARRIER
           MOVE EX-GROUPING            TO PY-GROUPING
           MOVE EX-RESP-NO             TO PY-FIN-RESP
           IF EX-ACCOUNT = SPACES
              MOVE LOW-VALUES          TO PY-ACCOUNT
           ELSE
              MOVE EX-ACCOUNT          TO PY-ACCOUNT
           END-IF
           MOVE 'C'                    TO PY-RECORD-TYPE
           MOVE +0                     TO WS-HYPHEN-CNTR
           INSPECT EX-CHECK-AMT
              REPLACING ALL ' ' BY ZEROS

           INSPECT EX-CHECK-AMT TALLYING WS-HYPHEN-CNTR
              FOR ALL '-'
           IF WS-HYPHEN-CNTR > +0
              INSPECT EX-CHECK-AMT
                 REPLACING ALL '-' BY ZEROS
           END-IF

           MOVE +9                     TO O1
           PERFORM VARYING I1 FROM +12 BY -1 UNTIL
              (O1 < +1)
              IF EX-CHECK-AMT (I1:1) NUMERIC
                 MOVE EX-CHECK-AMT (I1:1)
                                       TO WS-CHECK-AMT (O1:1)
                 SUBTRACT +1           FROM O1
              END-IF
           END-PERFORM
      *    MOVE EX-CHECK-AMT (1:1)     TO WS-CHECK-AMT (1:1)
      *    MOVE EX-CHECK-AMT (3:3)     TO WS-CHECK-AMT (2:3)
      *    MOVE EX-CHECK-AMT (7:3)     TO WS-CHECK-AMT (5:3)
      *    MOVE EX-CHECK-AMT (11:2)    TO WS-CHECK-AMT (8:2)
      *    DISPLAY ' IN AMT ' EX-CHECK-AMT ' OUT AMT ' WS-CHECK-AMT

           MOVE WS-CHECK-AMT-N         TO PY-ENTRY-AMT
           IF WS-HYPHEN-CNTR > +0
062121*       COMPUTE PY-ENTRY-AMT = PY-ENTRY-AMT * -1
062121        MOVE 'R'                 TO PY-RECORD-TYPE
           END-IF

           MOVE WS-NEXT-CYCLE-BIN-DATE TO PY-LAST-MAINT-DT
                                          PY-INPUT-DT
                                          
           MOVE 'AUTO'                 TO PY-LAST-MAINT-BY
           MOVE +180000                TO PY-LAST-MAINT-HHMMSS
           MOVE LOW-VALUES             TO PY-CREDIT-ACCEPT-DT
                                          PY-BILLED-DATE
                                          PY-AR-DATE
                                          PY-REPORTED-DT
                                          PY-CHECK-WRITTEN-DT
           MOVE ZEROS                  TO PY-CHECK-QUE-CONTROL
                                          PY-CHECK-QUE-SEQUENCE
062121*    MOVE '1825011300'           TO PY-GL-ACCOUNT
062121     move EX-GL-NUM              to py-gl-account
           MOVE 'MONTHLY CK'           TO PY-GL-COMMENT
062121     if ex-spec-inst <> spaces
062121        move ex-spec-inst        to py-gl-comment
062121     end-if
           MOVE WS-SELECT-BIN-DATE     TO PY-CREDIT-SELECT-DT
           MOVE WS-WORK-SEQ            TO PY-FILE-SEQ-NO
           PERFORM 0080-WRITE-ERPYAJ-OUT
                                       THRU 0080-EXIT

           .
       0050-continue.

           PERFORM 0110-READ-INPUT     THRU 0110-EXIT

           .
       0050-EXIT.
           EXIT.

       0080-WRITE-ERPYAJ-OUT.

           WRITE PENDING-PAY-ADJ

           IF ERPYAJ-FILE-STATUS  = '00'
              ADD 1                    TO WS-ERPYAJ-OUT
           ELSE
              IF ERPYAJ-FILE-STATUS = '22'
                 ADD +1 TO WS-WORK-SEQ
                 MOVE WS-WORK-SEQ TO PY-FILE-SEQ-NO
                 GO TO 0080-WRITE-ERPYAJ-OUT
              ELSE
                 DISPLAY ' BAD WRITE FOR ERPYAJ ' ERPYAJ-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0080-EXIT.
           EXIT.

       0110-READ-INPUT.

           READ EXTR-IN INTO EXTRACT-RECORD AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

062121     if end-of-input
062121        go to 0110-exit
062121     end-if
062121
062121     ADD +1                      TO WS-EXTR-IN
062121     unstring extr-in-rec delimited by ';' into
062121        EX-ACCT-NAME
062121        EX-MAIL-NAME
062121        EX-ADDR-1
062121        EX-ADDR-2
062121        EX-ADDR-3
062121        EX-ZIP
062121        EX-INV-DATE
062121        EX-CHECK-AMT
062121        EX-GL-NUM
062121        EX-DIV
062121        EX-CENTER
062121        EX-LOB
062121        EX-STATE
062121        EX-CARRIER
062121        EX-GROUPING
062121        EX-RESP-NO
062121        EX-ACCOUNT
062121        EX-SPEC-INST
062121        ex-filler-1
062121        ex-filler-2
062121        EX-ACTION-FLAG
062121        EX-EOR
062121     end-unstring
062121
062121     unstring ex-inv-date delimited by '/' into
062121        ws-work-month
062121        ws-work-day
062121        ws-work-ccyy
062121     end-unstring
062121
062121     if ws-work-month(2:1) = ' '
062121        move ws-work-month(1:1)  to ws-work-month(2:1)
062121        move '0'                 to ws-work-month(1:1)
062121     end-if
062121
062121     if ws-work-day(2:1) = ' '
062121        move ws-work-day(1:1)    to ws-work-day(2:1)
062121        move '0'                 to ws-work-day(1:1)
062121     end-if
062121
062121     move ws-work-date           to ex-inv-date

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
