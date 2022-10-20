       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDPYX1.
       AUTHOR.        CENTRAL STATES HEALTH AND LIFE.
       DATE-COMPILED.
      *REMARKS.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 030404                   PEMA  New Program
      ******************************************************************
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      
           SELECT ERPYAJ       ASSIGN TO ERPYAJ
                               ACCESS IS SEQUENTIAL
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS FILEIN-FILE-STATUS
                               RECORD KEY IS PY-CONTROL-PRIMARY.
      
           SELECT DISK-DATE    ASSIGN TO SYS019.
      
           SELECT FILE-OUT     ASSIGN TO FILEOT
               ORGANIZATION IS LINE SEQUENTIAL.
      
       DATA DIVISION.
       FILE SECTION.
      
       FD  FILE-OUT
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.
      
       01  FILE-OUT-REC                PIC X(300).
      
       FD  DISK-DATE
                                       COPY ELCDTEFD.
      
       FD  ERPYAJ.
      
                                       COPY ERCPYAJ.
      
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CIDPYX1 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.
      
       01  WS-MISC.
           05  WS-EOF-SW               PIC X      VALUE SPACES.
               88  END-OF-INPUT                   VALUE 'Y'.
           05  FILEIN-FILE-STATUS      PIC XX     VALUE '00'.
           05  ws-work-century         pic 99     value zeros.
           05  SUB1            COMP-3  PIC S9(3)  VALUE +0.
      
           05  WS-WORK-CITY-ST.
               10  WS-BYTE OCCURS 29   PIC X.
           05  WS-RECS-IN              PIC 9(7)   VALUE ZEROS.
           05  WS-RECS-OUT             PIC 9(7)   VALUE ZEROS.
           05  ws-work-time            pic 9(7).
           05  filler redefines ws-work-time.
               10  filler              pic x.
               10  ws-hh               pic 99.
               10  ws-mm               pic 99.
               10  ws-ss               pic 99.
      
      
       01  WS-SAVE-FILEOT              PIC X(300) VALUE LOW-VALUES.
       01  FILEOT-DETAIL-RECORD.
           12  EX-CARRIER              PIC X.
           12  EX-GROUPING             PIC X(6).
           12  EX-RESP-NO              PIC X(10).
           12  EX-ACCOUNT              PIC X(10).
           12  EX-SEQ-NO               PIC 9(7).
           12  EX-REC-TYPE             PIC X.
           12  EX-PMT-TYPE             pic x.
           12  ex-bill-inv             pic x(6).
           12  EX-ref-no               PIC X(12).
           12  EX-last-maint-dt        PIC X(10).
           12  EX-last-maint-by        PIC X(4).
           12  EX-amount               PIC -9(7).99.
           12  EX-gl-acct-no           PIC X(10).
           12  EX-gl-comment           PIC X(10).
           12  EX-comp-type            PIC X.
           12  EX-cred-sel-dt          PIC X(10).
           12  EX-cred-acc-dt          PIC X(10).
           12  EX-billed-dt            PIC x(10).
           12  EX-reported-dt          PIC X(10).
           12  EX-pmt-applied          PIC X.
           12  EX-input-dt             PIC X(10).
           12  EX-check-no             PIC X(6).
           12  EX-void-ind             PIC X.
           12  EX-check-orig           PIC X.
           12  EX-check-written-dt     PIC X(10).
           12  EX-bill-flag            PIC X.
           12  EX-EOR                  PIC X.
      
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
      
      
       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.
      
                                       COPY ELCDATE.
      
                                       COPY ELCDTECX.
      
                                       COPY ELCDTEVR.
      
       PROCEDURE DIVISION.
      
                                       COPY ELCDTERX.
      
           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT
           PERFORM 0040-INIT           THRU 0040-EXIT
      
           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
                (END-OF-INPUT)
      *         OR (WS-RECS-IN > 1000)
      
           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT
      
           DISPLAY ' RECORDS IN    ' WS-RECS-IN
           DISPLAY ' RECORDS  OUT  ' WS-RECS-OUT
           GOBACK
      
           .
       0002-EXIT.
           EXIT.
      
       0020-OPEN-FILES.
      
           OPEN INPUT ERPYAJ
               OUTPUT FILE-OUT
      
           IF FILEIN-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' FILEIN OPEN ERROR ' FILEIN-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF
      
           .
       0020-EXIT.
           EXIT.
      
       0030-CLOSE-FILES.
      
           CLOSE ERPYAJ FILE-OUT
      
           IF FILEIN-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' FILEIN CLOSE ERROR ' FILEIN-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF
      
           .
       0030-EXIT.
           EXIT.
      
       0040-INIT.
      
           MOVE SPACES                 TO FILEOT-DETAIL-RECORD
           MOVE 'E'                    TO EX-EOR

           move zeros                  to EX-amount
      
           MOVE FILEOT-DETAIL-RECORD   TO WS-SAVE-FILEOT
      
           string
              'Carrier'                 ';'
              'Grouping'                   ';'
              'FinResp'                    ';'
              'Account'                    ';'
              'SeqNo'                      ';'
              'RecordType'                 ';'
              'PaymentType'                ';'
              'BillInv'                    ';'
              'RefNo'                      ';'
              'LastMaintDate'              ';'
              'LastMaintBy'                ';'
              'Amount'                     ';'
              'GLAccountNo'                ';'
              'GLComment'                  ';'
              'CompType'                   ';'
              'CreditSelectDate'           ';'
              'CreditAcceptDate'           ';'
              'BilledDate'                 ';'
              'ReportedDate'               ';'
              'PmtApplied'                 ';'
              'InputDate'                  ';'
              'CheckNo'                    ';'
              'VoidInd'                    ';'
              'CheckOrigin'                ';'
              'CheckWrittenDate'           ';'
              'BillFlag'                   ';'
              'EOR' delimited by size into file-out-rec
           end-string
      
           write file-out-rec
      
           move spaces                 to file-out-rec
           PERFORM 0120-START-FILEIN   THRU 0120-EXIT
           PERFORM 0110-READ-FILEIN    THRU 0110-EXIT
      
           .
       0040-EXIT.
           EXIT.
      
       0050-PROCESS-FILE.
      
           MOVE WS-SAVE-FILEOT         TO FILEOT-DETAIL-RECORD
      
           MOVE PY-CARRIER             TO EX-CARRIER
           MOVE PY-GROUPING            TO EX-GROUPING
           MOVE PY-FIN-RESP            TO EX-RESP-NO
           IF PY-ACCOUNT = LOW-VALUES
              MOVE SPACES              TO EX-ACCOUNT
           ELSE
              MOVE PY-ACCOUNT          TO EX-ACCOUNT
           END-IF
           move py-file-seq-no         to ex-seq-no
           MOVE py-record-type         TO EX-rec-type
           move py-pymt-type           to ex-pmt-type
           MOVE py-bil-inv             TO EX-bill-inv
           MOVE py-ref-no              TO EX-ref-no
           MOVE py-last-maint-dt       TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-last-maint-dt
           ELSE
              MOVE SPACES              TO EX-last-maint-dt
           END-IF
           move py-last-maint-by       to ex-last-maint-by
           move py-entry-amt           to ex-amount
           move py-gl-account          to ex-gl-acct-no
           move py-gl-comment          to ex-gl-comment
           move py-ercomp-type         to ex-comp-type

           MOVE py-credit-select-dt    TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-cred-sel-dt
           ELSE
              MOVE SPACES              TO EX-cred-sel-dt
           END-IF

           MOVE py-credit-accept-dt    TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-cred-acc-dt
           ELSE
              MOVE SPACES              TO EX-cred-acc-dt
           END-IF

           MOVE py-billed-date         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-billed-dt
           ELSE
              MOVE SPACES              TO EX-billed-dt      
           END-IF

           MOVE py-reported-dt         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-reported-dt
           ELSE
              MOVE SPACES              TO EX-reported-dt
           END-IF

           move py-pmt-applied         to ex-pmt-applied

           MOVE py-input-dt            TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-input-dt
           ELSE
              MOVE SPACES              TO EX-input-dt
           END-IF
           move py-check-number        to ex-check-no
           move py-void-sw             to ex-void-ind
           move py-check-origin-sw     to ex-check-orig

           MOVE py-check-written-dt    TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-check-written-dt
           ELSE
              MOVE SPACES              TO EX-check-written-dt
           END-IF

           move py-bill-flag           to ex-bill-flag
      
           PERFORM 0080-WRITE-FILE-OUT THRU 0080-EXIT
           PERFORM 0110-READ-FILEIN    THRU 0110-EXIT
      
           .
       0050-EXIT.
           EXIT.
      
       0080-WRITE-FILE-OUT.
      
           INSPECT FILEOT-DETAIL-RECORD REPLACING
              ALL ';'                  BY ' '
              ALL X'00'                BY ' '
              ALL X'09'                BY ' '
      
           string
              EX-CARRIER          ';'
              EX-GROUPING         ';'
              EX-RESP-NO          ';'
              EX-ACCOUNT          ';'
              EX-SEQ-NO           ';'
              EX-REC-TYPE         ';'
              EX-PMT-TYPE         ';'
              ex-bill-inv         ';'
              EX-ref-no           ';'
              EX-last-maint-dt    ';'
              EX-last-maint-by    ';'
              EX-amount           ';'
              EX-gl-acct-no       ';'
              EX-gl-comment       ';'
              EX-comp-type        ';'
              EX-cred-sel-dt      ';'
              EX-cred-acc-dt      ';'
              EX-billed-dt        ';'
              EX-reported-dt      ';'
              EX-pmt-applied      ';'
              EX-input-dt         ';'
              EX-check-no         ';'
              EX-void-ind         ';'
              EX-check-orig       ';'
              EX-check-written-dt ';'
              EX-bill-flag        ';'
              EX-EOR
              delimited by size into file-out-rec
           end-string
      
           WRITE file-OUT-REC
           ADD 1 TO WS-recs-out
      
           .
       0080-EXIT.
           EXIT.
      
       0110-READ-FILEIN.
      
           READ ERPYAJ NEXT RECORD
      
           IF (FILEIN-FILE-STATUS = '10' OR '23')
              OR (PY-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF FILEIN-FILE-STATUS NOT = '00'
                 DISPLAY ' FILEIN READ ERROR ' FILEIN-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF
      
           IF NOT END-OF-INPUT
              ADD +1                   TO WS-RECS-IN
           END-IF
      
           .
       0110-EXIT.
           EXIT.
      
       0120-START-FILEIN.
      
           MOVE LOW-VALUES             TO PY-CONTROL-PRIMARY
      
           MOVE DTE-CLASIC-COMPANY-CD  TO PY-COMPANY-CD
      
           START ERPYAJ KEY >= PY-CONTROL-PRIMARY
      
           IF FILEIN-FILE-STATUS = '10' OR '23'
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF FILEIN-FILE-STATUS NOT = '00'
                 DISPLAY ' FILEIN START ERROR ' FILEIN-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF
      
           .
       0120-EXIT.
           EXIT.
      
       8510-DATE-CONVERSION.
      
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA
      
           .
       8590-EXIT.
           EXIT.
      
       ABEND-PGM.
                                       COPY ELCABEND.
      
