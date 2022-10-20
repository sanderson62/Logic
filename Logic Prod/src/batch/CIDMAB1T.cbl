       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDMAB1.
      *AUTHOR.     PABLO.
      *REMARKS.
      * THIS PROGRAM READS AN EXTRACT FILE FROM THE ERPNDM THEN
      * IF LOOKS UP THE ELCERT RECORD AND THE ERMAIL RECORD.
      * IF A MATCH IS FOUND WITH THE ERMAIL FILE THEN THE PGM CHECKS
      * TO SEE IF THERE IS A CRED BENE NAME AND ADDRS AND IF NOT IT 
      * WILL UPDATE THE CRED BENE NAME AND ADDRESS ON THE ERMAIL
      * FILE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT DISK-DATE        ASSIGN TO SYS019.
           SELECT RPT-FILE         ASSIGN TO SYS008.

           SELECT POST-CARD-IN     ASSIGN TO SYS010
              ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT ELCERT           ASSIGN TO ELCERT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CM-CONTROL-PRIMARY
                                   FILE STATUS IS ELCERT-FILE-STATUS.

           SELECT ERMAIL           ASSIGN TO ERMAIL
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS MA-CONTROL-PRIMARY
                                   FILE STATUS IS ERMAIL-FILE-STATUS.

       DATA DIVISION.

       FILE SECTION.

       FD  DISK-DATE                   COPY ELCDTEFD.

       FD  RPT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 133 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS RPT-REC-OUT.

       01  RPT-REC-OUT.
           05  RPT-REC                  PIC X(132).

       FD  POST-CARD-IN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD.

       01  POST-CARD-RECORD.
           12  EX-CARRIER              PIC X.
           12  FILLER                  PIC X.
           12  EX-GROUP                PIC X(6).
           12  FILLER                  PIC X.
           12  EX-STATE                PIC XX.
           12  FILLER                  PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  FILLER                  PIC X.
           12  EX-EFF-DT               PIC X(10).
           12  FILLER                  PIC X.
           12  EX-CERT-NO              PIC X(11).
           12  FILLER                  PIC X.
           12  EX-CRED-BENE-NAME       PIC X(30).
           12  FILLER                  PIC X.
           12  EX-CRED-BENE-ADDR1      PIC X(30).
           12  FILLER                  PIC X.
           12  EX-CRED-BENE-ADDR2      PIC X(30).
           12  FILLER                  PIC X.
           12  EX-CRED-BENE-CITYST     PIC X(30).
           12  FILLER                  PIC X.
           12  EX-CRED-BENE-ZIP        PIC X(9).
           12  FILLER                  PIC X.
           12  EX-EOR                  PIC X.

       FD  ELCERT.
                                       COPY ELCCERT.                         

       FD  ERMAIL.
                                       COPY ERCMAIL.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '         WORKING-STORAGE        '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  SAVE-CERT                   PIC X(11) VALUE SPACES.
       77  SAVE-EFF-DT                 PIC X(6)  VALUE SPACES.
       77  SAVE-STATE                  PIC XX    VALUE SPACES.
       77  PGM-SUB                     PIC S999  COMP   VALUE +511.    
       77  WS-ABEND-FILE-STATUS        PIC XX            VALUE ZERO.    
       77  WS-ABEND-MESSAGE            PIC X(80)         VALUE SPACES.  
       77  WS-ABEND-PROGRAM            PIC X(8)          VALUE SPACES.  
       77  WS-RETURN-CODE              PIC S9(4)         VALUE +0.      
       77  WS-ZERO                     PIC S9     COMP-3 VALUE +0.      
       77  IN-CNT                 PIC 9999999   VALUE ZEROS.
       77  CERT-CNT               PIC 9999      VALUE ZEROS.
       77  S1                     PIC S999      VALUE +0 COMP-3.
       77  WS-WORK-SEQ                 PIC S9(7) COMP-3 VALUE +0.
       77  ELCERT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
           88  ELCERT-FOUND                    VALUE '00'.
       77  ERMAIL-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  AGT-SUB                     PIC S999 COMP-3 VALUE +0.
       77  WS-CURRENT-BIN-DATE         PIC XX VALUE LOW-VALUES.
       77  WS-BIN-COMPARE-DT           PIC XX VALUE LOW-VALUES.
       77  WS-COMPARE-TYPE             PIC X  VALUE SPACES.
       77  WS-COMPARE-STATUS           PIC X  VALUE SPACES.
       77  WS-DUP-SW                   PIC X  VALUE SPACES.
           88  DUPLICATE-MAILING              VALUE 'Y'.
       77  WS-CERT-CANCELLED-SW        PIC X  VALUE SPACES.
           88  CERT-ALREADY-CANCELLED         VALUE 'Y'.
       77  WS-ADDRESS-ONLY-SW          PIC X  VALUE SPACES.
           88  UPDATE-ADDRESS-ONLY        VALUE 'Y'.
       77  WS-BYPASS-UPD                PIC 9(7) VALUE ZEROS.
       77  WS-SAVE-ELCERT-KEY          PIC X(33)  VALUE LOW-VALUES.
       77  WS-BYPASS-SW                PIC X  VALUE SPACES.
           88  BYPASS-REC                VALUE 'Y'.
       01  W-MISC.
           05  WORK-DATE-X.
               10  WORK-MO-X       PIC XX.
               10  WORK-DA-X       PIC XX.
               10  WORK-YR-X       PIC XX.
           05  WORK-DATE-N    REDEFINES   WORK-DATE-X.
               10  WORK-MO-N       PIC 99.
               10  WORK-DA-N       PIC 99.
               10  WORK-YR-N       PIC 99.
           05  WS-WORK-DT             PIC 9(11).
           05  WS-WORK-DT-A REDEFINES WS-WORK-DT.
               10  FILLER             PIC XXX.
               10  V-ISS-CCYR         PIC X(4).
               10  V-ISS-MO           PIC XX.
               10  V-ISS-DA           PIC XX.
           05  WS-WORK-DATE          PIC 9(11).
           05  FILLER REDEFINES WS-WORK-DATE.
               10  FILLER            PIC X(5).
               10  WS-WORK-DATE-YR   PIC XX.
               10  WS-WORK-DATE-MO   PIC XX.
               10  WS-WORK-DATE-DA   PIC XX.
           05  CERT-KEY.
               10  CERT-STATE         PIC XX.
               10  CERT-DATE          PIC 9(11).
               10  CERT-CERT          PIC X(11).
           05  WS-DATE-ALPH.
               10  FILLER             PIC XXX VALUE '000'.
               10  WS-WORK-CENT       PIC XX.
               10  WS-WORK-YR         PIC XX.
               10  WS-WORK-MO         PIC XX.
               10  WS-WORK-DA         PIC XX.
           05  WS-DATE-NUM REDEFINES WS-DATE-ALPH
                                      PIC 9(11).

           05  WS-BIN-EFF             PIC XX VALUE LOW-VALUES.
           05  EXTR-IN-CNT            PIC 9(7) VALUE ZEROS.
           05  ELCERT-IN-CNT          PIC 9(7) VALUE ZEROS.
           05  ERMAIL-DP-CNT          PIC 9(7) VALUE ZEROS.
           05  ERMAIL-OT-CNT          PIC 9(7) VALUE ZEROS.
           05  ERMAIL-UD-CNT          PIC 9(7) VALUE ZEROS.
           05  ELCERT-UD-CNT          PIC 9(7) VALUE ZEROS.
           05  ELCERT-AC-CNT          PIC 9(7) VALUE ZEROS.
           05  WS-INPUT-SW            PIC X VALUE ' '.
               88  END-OF-INPUT             VALUE 'Y'.
                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

                                       COPY ELCDATE.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0040-OPEN-FILES     THRU 0040-EXIT
           PERFORM 0050-INIT           THRU 0050-EXIT

           PERFORM 0020-PROCESS        THRU 0020-EXIT UNTIL
              (END-OF-INPUT)
      *       OR (IN-CNT > 10)
           PERFORM 0060-CLOSE-FILES    THRU 0060-EXIT

           DISPLAY ' EXTR RECS READ       ' EXTR-IN-CNT
           DISPLAY ' ELCERT RECS FOUND    ' ELCERT-IN-CNT
           DISPLAY ' ERMAIL RECS ADDED    ' ERMAIL-OT-CNT
           DISPLAY ' ERMAIL RECS UPDATED  ' ERMAIL-UD-CNT
           DISPLAY ' ELCERT RECS UPDATED  ' ELCERT-UD-CNT
           DISPLAY ' BYPASS UPD           ' WS-BYPASS-UPD

           GOBACK

           .
       0015-READ-ELCERT.

           MOVE DTE-CLASIC-COMPANY-CD  TO CM-COMPANY-CD
           MOVE EX-CARRIER             TO CM-CARRIER
           MOVE EX-GROUP               TO CM-GROUPING
           MOVE EX-STATE               TO CM-STATE
           MOVE EX-ACCOUNT             TO CM-ACCOUNT
           MOVE EX-CERT-NO             TO CM-CERT-NO

           MOVE EX-EFF-DT (7:4)        TO DC-GREG-DATE-CYMD-R (1:4)
           MOVE EX-EFF-DT (1:2)        TO DC-GREG-DATE-CYMD-R (5:2)
           MOVE EX-EFF-DT (4:2)        TO DC-GREG-DATE-CYMD-R (7:2)
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
           IF NOT NO-CONVERSION-ERROR
              DISPLAY ' INVALID EFF DATE ' EX-EFF-DT '  ' EX-CERT-NO
              PERFORM ABEND-PGM
           ELSE
              MOVE DC-BIN-DATE-1       TO CM-CERT-EFF-DT
           END-IF

           MOVE SPACES                 TO WS-CERT-CANCELLED-SW
           MOVE CM-CONTROL-PRIMARY     TO WS-SAVE-ELCERT-KEY
           READ ELCERT

           IF ELCERT-FILE-STATUS = '00'
              ADD 1                    TO ELCERT-IN-CNT
           ELSE
              IF (ELCERT-FILE-STATUS = '23' OR '10')
                 OR (CM-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
PEMTST           DISPLAY ' NO ELCERT    ' EX-STATE ' '
                    EX-ACCOUNT '  ' EX-CERT-NO
PEMTST*          CONTINUE
              ELSE
                 DISPLAY ' BAD READ ELCERT ' CM-CONTROL-PRIMARY '  '
                    ELCERT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0015-EXIT.
           EXIT.

       0017-READ-INPUT.

           READ POST-CARD-IN AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1                    TO EXTR-IN-CNT
           END-IF

           .
       0017-EXIT.
           EXIT.

       0020-PROCESS.

           PERFORM 0015-READ-ELCERT THRU 0015-EXIT
           
           IF ELCERT-FOUND
              PERFORM 0070-BUILD-ERMAIL
                                       THRU 0070-EXIT
              PERFORM 0030-REWRITE-ELCERT
                                       THRU 0030-EXIT
           ELSE
              PERFORM 0070-BUILD-ERMAIL
                                       THRU 0070-EXIT
           END-IF

           PERFORM 0017-READ-INPUT     THRU 0017-EXIT

           .
       0020-EXIT.
            EXIT.

       0030-REWRITE-ELCERT.
       
           IF CM-INSURED-ADDRESS-SW NOT = '1'
              MOVE '1'                 TO CM-INSURED-ADDRESS-SW
PEMTST*       MOVE '00'                TO ELCERT-FILE-STATUS
PEMTST        REWRITE CERTIFICATE-MASTER
              IF ELCERT-FILE-STATUS = '00'
PEMTST           DISPLAY ' REWRITE CERT ' EX-ACCOUNT '  ' EX-CERT-NO
                 ADD 1                 TO ELCERT-UD-CNT
              ELSE
                 DISPLAY ' BAD REWRITE ELCERT ' CM-CONTROL-PRIMARY
                    '  ' ELCERT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0030-EXIT.
            EXIT.

       0040-OPEN-FILES.
       
           OPEN INPUT POST-CARD-IN
PEMTST*               ELCERT ERMAIL
PEMTST     OPEN I-O ELCERT ERMAIL

           IF ELCERT-FILE-STATUS NOT = '00'
              DISPLAY ' ELCERT OPEN ' ELCERT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

PEMTST*    OPEN I-O ERMAIL

           IF ERMAIL-FILE-STATUS NOT = '00'
              DISPLAY ' ERMAIL OPEN ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0040-EXIT.
           EXIT.

       0050-INIT.
       
           DISPLAY ' ACCEPT DATE ' WS-ACCEPT-DATE

           MOVE WS-ACCEPT-DATE         TO DC-GREG-DATE-1-YMD
           MOVE '3'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-CURRENT-BIN-DATE
           ELSE
              DISPLAY ' ERROR CONVERTING CURRENT DATE '
           END-IF

           PERFORM 0017-READ-INPUT     THRU 0017-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-CLOSE-FILES.
       
           CLOSE ELCERT
                 ERMAIL
                 POST-CARD-IN

           IF ELCERT-FILE-STATUS NOT = '00'
              DISPLAY ' ELCERT CLOSE ' ELCERT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERMAIL-FILE-STATUS NOT = '00'
              DISPLAY ' ERMAIL CLOSE ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0060-EXIT.
           EXIT.

       0070-BUILD-ERMAIL.

           PERFORM 0071-BUILD-ERMAIL-KEY
                                       THRU 0071-EXIT
           READ ERMAIL

           MOVE SPACES                 TO WS-BYPASS-SW
           IF ERMAIL-FILE-STATUS = '00'
              PERFORM 0075-UPDATE-ERMAIL-BODY
                                       THRU 0075-EXIT
              PERFORM 0073-REWRITE-ERMAIL
                                       THRU 0073-EXIT
           ELSE
              IF ERMAIL-FILE-STATUS = '23' OR '10'
                 MOVE SPACES           TO MAILING-DATA
                 PERFORM 0071-BUILD-ERMAIL-KEY
                                       THRU 0071-EXIT
                 PERFORM 0072-BUILD-ERMAIL-BODY
                                       THRU 0072-EXIT
                 PERFORM 0075-UPDATE-ERMAIL-BODY
                                       THRU 0075-EXIT
                 MOVE ' '              TO MA-ADDRESS-CORRECTED
                 PERFORM 0074-WRITE-ERMAIL
                                       THRU 0074-EXIT
              ELSE
                 DISPLAY ' BAD READ ERMAIL ' MA-CONTROL-PRIMARY '  '
                    DC-GREG-DATE-CYMD '  ' ERMAIL-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0070-EXIT.
            EXIT.

       0071-BUILD-ERMAIL-KEY.
       
           MOVE CM-CONTROL-PRIMARY     TO MA-CONTROL-PRIMARY

           .
       0071-EXIT.
           EXIT.

       0072-BUILD-ERMAIL-BODY.
       
           DISPLAY ' BUILDING ERMAIL REC ' EX-STATE ' ' EX-ACCOUNT
              ' ' EX-CERT-NO
           MOVE 'CR'                   TO MA-SOURCE-SYSTEM
           MOVE ZEROS                  TO MA-INSURED-ISSUE-AGE
                                          MA-PHONE-NO
           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +7
              MOVE LOW-VALUES          TO MA-MAIL-DATE (S1)
           END-PERFORM

           MOVE WS-CURRENT-BIN-DATE    TO MA-RECORD-ADD-DT
                                          MA-LAST-MAINT-DT
           MOVE 'CONV'                 TO MA-RECORD-ADDED-BY
           MOVE +190000                TO MA-LAST-MAINT-HHMMSS

           .
       0072-EXIT.
           EXIT.

       0073-REWRITE-ERMAIL.

           IF BYPASS-REC
              GO TO 0073-EXIT
           END-IF

PEMTST*    MOVE '00'                   TO ERMAIL-FILE-STATUS
PEMTST     REWRITE MAILING-DATA

           IF ERMAIL-FILE-STATUS = '00'
PEMTST        DISPLAY ' REWRITE ERMAIL ' EX-ACCOUNT '  ' EX-CERT-NO
              ADD 1                    TO ERMAIL-UD-CNT
           ELSE
              DISPLAY ' BAD REWRITE ERMAIL ' MA-CONTROL-PRIMARY '  '
                 DC-GREG-DATE-CYMD '  ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0073-EXIT.
           EXIT.
                      
       0074-WRITE-ERMAIL.

           MOVE 'MA'                   TO MA-RECORD-ID
                                          
PEMTST*    MOVE '00'                   TO ERMAIL-FILE-STATUS
PEMTST     WRITE MAILING-DATA

           IF ERMAIL-FILE-STATUS = '00'
PEMTST        DISPLAY ' WRITE ERMAIL   ' EX-ACCOUNT '  ' EX-CERT-NO
              ADD 1                    TO ERMAIL-OT-CNT
           ELSE
              DISPLAY ' BAD WRITE ERMAIL ' MA-CONTROL-PRIMARY '  '
                 DC-GREG-DATE-CYMD '  ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0074-EXIT.
           EXIT.

       0075-UPDATE-ERMAIL-BODY.

           IF (MA-CRED-BENE-NAME NOT = SPACES)
              AND (MA-CRED-BENE-ADDR NOT = SPACES)
              ADD 1         TO WS-BYPASS-UPD
              SET BYPASS-REC           TO TRUE
              GO TO 0075-EXIT
           END-IF

           MOVE EX-CRED-BENE-NAME      TO MA-CRED-BENE-NAME
           MOVE EX-CRED-BENE-ADDR1     TO MA-CRED-BENE-ADDR
           MOVE EX-CRED-BENE-ADDR2     TO MA-CRED-BENE-ADDR2
           MOVE EX-CRED-BENE-CITYST    TO MA-CRED-BENE-CTYST
           MOVE EX-CRED-BENE-ZIP       TO MA-CRED-BENE-ZIP

           .
       0075-EXIT.
           EXIT.

       8500-DATE-CONVERT.

           CALL 'ELDATCX'              USING DATE-CONVERSION-DATA.

       8500-EXIT.
           EXIT.
           EJECT

       ABEND-PGM.   COPY ELCABEND.

