       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDDEX2.
       AUTHOR.     PABLO.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EXTR-FILE-IN     ASSIGN TO EXTRIN.

           SELECT EXTR-FILE-OUT    ASSIGN TO EXTROT
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE        ASSIGN TO SYS019.
                                                                        
           EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  EXTR-FILE-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY ECSEXT01.
      /

       FD  EXTR-FILE-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  EXTR-FILE-OUT-HEAD          PIC X(350).
       01  EXTR-FILE-OUT-REC           PIC X(307).

       FD  DISK-DATE                                                    
                                   COPY ELCDTEFD.                       

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDDEX2  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-EXTR               VALUE 'Y'.
       77  EXT-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  EXT-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.
       77  PGM-SUB                 PIC S999    COMP    VALUE +020.
       77  WS-EFF-BIN              PIC XX    VALUE LOW-VALUES.
       77  WS-CAN-BIN              PIC XX    VALUE LOW-VALUES.
       77  WS-EXP-BIN              PIC XX    VALUE LOW-VALUES.
       77  WS-EARN-TERM            PIC 999   VALUE ZEROS.
       77  WS-WORK-TERM            PIC S9(3) VALUE +0 COMP-3.
       77  WS-REM-TERM             PIC S9(3) VALUE +0 COMP-3.
      /


       01  EXTR-DETAIL-RECORD.
           12  EX-STATE                PIC XX.
           12  EX-TAB1                 PIC X.
           12  EX-CARRIER              PIC X.
           12  EX-TAB2                 PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-TAB3                 PIC X.
           12  EX-PROC-MONTH           PIC XX.
           12  EX-TAB4                 PIC X.
           12  EX-CERT                 PIC X(11).
           12  EX-TAB5                 PIC X.
           12  EX-EFF                  PIC X(10).
           12  EX-TAB6                 PIC X.
           12  EX-LNAME                PIC X(15).
           12  EX-TAB7                 PIC X.
           12  EX-FNAME                PIC X(10).
           12  EX-TAB8                 PIC X.
           12  EX-AH-REF               PIC -9(9).99.
           12  EX-TAB9                 PIC X.
           12  EX-LF-REF               PIC -9(9).99.
           12  EX-TAB10                PIC X.
           12  EX-EARN-TERM            PIC 999.
           12  EX-TAB11                PIC X.
           12  EX-CANC-DTE             PIC X(10).
           12  EX-TAB12                PIC X.
           12  EX-ISS-YEAR             PIC XXXX.
           12  EX-TAB13                PIC X.
           12  EX-CAN-YEAR             PIC XXXX.
           12  EX-TAB14                PIC X.
           12  EX-REPORT-CODE-1        PIC X(10).
           12  EX-TAB15                PIC X.
           12  EX-REPORT-CODE-2        PIC X(10).
           12  EX-TAB16                PIC X.
           12  EX-STAR                 PIC X.
      ******************************************************************
       01  EXTR-DETAIL-HEADER.
           12  FILLER                  PIC X(05) VALUE
                                               'STATE'.
           12  EX-HTAB1                PIC X.
           12  FILLER                  PIC X(04) VALUE
                                               'CARR'.
           12  EX-HTAB2                PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'ACCOUNT'.
           12  EX-HTAB3                PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'PROC MO'.
           12  EX-HTAB4                PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'CERT NO'.
           12  EX-HTAB5                PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'EFF DTE'.
           12  EX-HTAB6                PIC X.
           12  FILLER                  PIC X(09) VALUE
                                               'LAST NAME'.
           12  EX-HTAB7                PIC X.
           12  FILLER                  PIC X(10) VALUE
                                               'FIRST NAME'.
           12  EX-HTAB8                PIC X.
           12  FILLER                  PIC X(10) VALUE
                                               'AH REF AMT'.
           12  EX-HTAB9                PIC X.
           12  FILLER                  PIC X(10) VALUE
                                               'LF REF AMT'.
           12  EX-HTAB10               PIC X.
           12  FILLER                  PIC X(09) VALUE
                                               'EARN MTHS'.
           12  EX-HTAB11               PIC X.
           12  FILLER                  PIC X(10) VALUE
                                               'CANCEL DTE'.
           12  EX-HTAB12               PIC X.
           12  FILLER                  PIC X(08) VALUE
                                               'ISS YEAR'.
           12  EX-HTAB13               PIC X.
           12  FILLER                  PIC X(09) VALUE
                                               'CANC YEAR'.
           12  EX-HTAB14               PIC X.
           12  FILLER                  PIC X(08) VALUE
                                               'RPT CDE1'.
           12  EX-HTAB15               PIC X.
           12  FILLER                  PIC X(08) VALUE
                                               'RPT CDE2'.
           12  EX-HTAB16               PIC X.
           12  FILLER                  PIC X     VALUE '*'.
      ******************************************************************
       01  WS-MISC.
           05  WS-SAVE-EXTR            PIC X(307) VALUE LOW-VALUES.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
           05  FILLER REDEFINES WS-DATE.
               10  FILLER              PIC 999.
               10  WS-CCYY             PIC 9999.
               10  WS-MM               PIC 99.
               10  WS-DD               PIC 99.

       01  WS-ABEND-FIELDS.
           12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.
           12  WS-ZERO                 PIC S9          VALUE ZERO.
           12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.
           12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.


                                       COPY ELCCALC.
                                       COPY ELCDATE.
                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

           EJECT
       PROCEDURE DIVISION.

                                       COPY ELCDTERX.                       

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0050-PROCESS        THRU 0050-EXIT UNTIL
              (END-OF-EXTR)
PEMTST*       OR (EXT-RECS-IN > 50000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' EXTR RECORDS READ    '  EXT-RECS-IN
           DISPLAY ' EXTR RECORDS WRITTEN '  EXT-RECS-OUT
           GOBACK
           .

       0050-PROCESS.
       
           IF (DE-REIN = ' ')
              AND (DE-TRANS = 'C')
              PERFORM 0100-PROCESS-EXTR
                                       THRU 0100-EXIT
           END-IF

           PERFORM 0200-READ-EXTR      THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.
           
                                      
       0100-PROCESS-EXTR.

           MOVE WS-SAVE-EXTR           TO EXTR-DETAIL-RECORD

           MOVE LOW-VALUES             TO WS-EFF-BIN
                                          WS-CAN-BIN

           MOVE ZEROS                  TO WS-EARN-TERM

           MOVE DE-CARRIER             TO EX-CARRIER
           MOVE DE-STATE               TO EX-STATE
           MOVE DE-ACCOUNT             TO EX-ACCOUNT
           MOVE DE-PROC-DT             TO WS-DATE
           MOVE WS-MM                  TO EX-PROC-MONTH
           MOVE DE-EFF                 TO WS-DATE
                                          DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-EFF-BIN
           ELSE
              DISPLAY ' DATE CONVERT ERROR, EFF DATE ' DE-EFF
           END-IF

           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EX-EFF
           END-STRING
           MOVE WS-CCYY                TO EX-ISS-YEAR

           MOVE DE-CERT                TO EX-CERT
           MOVE DE-LNAME               TO EX-LNAME
           MOVE DE-FNAME               TO EX-FNAME
           MOVE DE-LF-RFND             TO EX-LF-REF
           MOVE DE-AH-RFND             TO EX-AH-REF

           IF DE-LF-CANC-DTE NOT = ZEROS
              MOVE DE-LF-CANC-DTE      TO WS-DATE
                                          DC-GREG-DATE-CYMD
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                 INTO EX-CANC-DTE
              END-STRING
           ELSE
              MOVE DE-AH-CANC-DTE      TO WS-DATE
                                          DC-GREG-DATE-CYMD
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                 INTO EX-CANC-DTE
              END-STRING
           END-IF
           MOVE WS-CCYY                TO EX-CAN-YEAR
                         
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-CAN-BIN
           ELSE
              DISPLAY ' DATE CONVERT ERROR, CAN DATE '
                 DC-GREG-DATE-CYMD
           END-IF

           MOVE WS-EFF-BIN             TO DC-BIN-DATE-1
           MOVE WS-CAN-BIN             TO DC-BIN-DATE-2
           MOVE '1'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-ELAPSED-MONTHS   TO WS-EARN-TERM
           ELSE
              DISPLAY ' DATE CONVERT ERROR, EARN TERM '
                 DE-CERT-NO
           END-IF
           MOVE WS-EARN-TERM           TO EX-EARN-TERM

           MOVE DE-REPORT-CODE-1       TO EX-REPORT-CODE-1
           MOVE DE-REPORT-CODE-2       TO EX-REPORT-CODE-2

           PERFORM 0300-WRITE-EXTR     THRU 0300-EXIT

           .
       0100-EXIT.
           EXIT.

           
       0200-READ-EXTR.

           READ EXTR-FILE-IN AT END
              SET END-OF-EXTR          TO TRUE
           END-READ


           IF NOT END-OF-EXTR
              ADD 1 TO EXT-RECS-IN
           END-IF

           .

       0200-EXIT.
           EXIT.

       0300-WRITE-EXTR.

           IF DE-REIN = ' '
              WRITE EXTR-FILE-OUT-REC  FROM EXTR-DETAIL-RECORD
              ADD 1                    TO EXT-RECS-OUT
           END-IF

           .

       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT EXTR-FILE-IN
               OUTPUT EXTR-FILE-OUT

           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE EXTR-FILE-IN EXTR-FILE-OUT

           .

       0500-EXIT.
           EXIT.


       0600-INITIALIZE.

           MOVE SPACES                 TO EXTR-DETAIL-RECORD
           MOVE  ';'                   TO EX-TAB1
                                          EX-TAB2
                                          EX-TAB3
                                          EX-TAB4
                                          EX-TAB5
                                          EX-TAB6
                                          EX-TAB7
                                          EX-TAB8
                                          EX-TAB9
                                          EX-TAB10
                                          EX-TAB11
                                          EX-TAB12
                                          EX-TAB13
                                          EX-TAB14
                                          EX-TAB15
                                          EX-TAB16
           MOVE '*'                    TO EX-STAR
           MOVE  ';'                   TO EX-HTAB1
                                          EX-HTAB2
                                          EX-HTAB3
                                          EX-HTAB4
                                          EX-HTAB5
                                          EX-HTAB6
                                          EX-HTAB7
                                          EX-HTAB8
                                          EX-HTAB9
                                          EX-HTAB10
                                          EX-HTAB11
                                          EX-HTAB12
                                          EX-HTAB13
                                          EX-HTAB14
                                          EX-HTAB15
                                          EX-HTAB16

           MOVE EXTR-DETAIL-RECORD     TO WS-SAVE-EXTR
           MOVE EXTR-DETAIL-HEADER     TO EXTR-FILE-OUT-HEAD
      *    WRITE EXTR-FILE-OUT-HEAD

           PERFORM 0200-READ-EXTR      THRU 0200-EXIT
           .

       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.

       ABEND-PGM SECTION.
                                     COPY ELCABEND.

