       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMMAB3.
      *AUTHOR.     PABLO.
      *REMARKS.
      * THIS PROGRAM READS THE BATCH CERT FILE AND FOR EACH RECORD
      * THAT HAS A "BL" IN THE ENTRY OR EXIT BATCHES IT WILL
      * READ THE ELCERT FILE. IF THE ELCERT FILE HAS A BL THEN IT 
      * ASSUMES THAT IT IS ALREADY FIXED.  OTHERWISE IT WILL BUILD
      * A CERT NOTE RECORD AND REWRITE THE ELCERT RECORD IF NEED BE
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
           12  EX-FNAME                PIC X(10).
           12  FILLER                  PIC X.
           12  EX-MID                  PIC X.
           12  FILLER                  PIC X.
           12  EX-LNAME                PIC X(15).
           12  FILLER                  PIC X.
           12  EX-ADDRESS1             PIC X(30).
           12  FILLER                  PIC X.
           12  EX-ADDRESS2             PIC X(30).
           12  FILLER                  PIC X.
           12  EX-CITY-STATE           PIC X(32).
           12  FILLER                  PIC X.
           12  EX-ZIP                  PIC X(11).
           12  FILLER                  PIC X.
           12  EX-CERT                 PIC X(11).
           12  FILLER                  PIC X.
           12  EX-EFF                  PIC X(10).
           12  FILLER                  PIC X.
           12  EX-EXP                  PIC X(10).
           12  FILLER                  PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  FILLER                  PIC X.
           12  EX-ACCOUNT-NAME         PIC X(30).
           12  EX-CARRIER-GRP-ST       PIC X(9).
           12  FILLER                  PIC X.
           12  EX-CLM-IND              PIC X.
           12  FILLER                  PIC X.
           12  EX-MAIL-DATE            PIC X(10).
           12  FILLER                  PIC X.
           12  EX-MAIL-STATUS          PIC XX.
           12  EX-MAIL-TYPE            PIC X(15).
           12  EX-END                  PIC X.

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
           DISPLAY ' ERMAIL RECS DUPS     ' ERMAIL-DP-CNT
           DISPLAY ' ERMAIL RECS ADDED    ' ERMAIL-OT-CNT
           DISPLAY ' ERMAIL RECS UPDATED  ' ERMAIL-UD-CNT
           DISPLAY ' ELCERT RECS UPDATED  ' ELCERT-UD-CNT
           DISPLAY ' ELCERT RECS PREV CAN ' ELCERT-AC-CNT

           GOBACK

           .
       0015-READ-ELCERT.

           MOVE DTE-CLASIC-COMPANY-CD  TO CM-COMPANY-CD
           MOVE EX-CARRIER-GRP-ST      TO CM-CONTROL-PRIMARY (2:9)
           MOVE EX-ACCOUNT             TO CM-ACCOUNT
           MOVE EX-CERT                TO CM-CERT-NO

           MOVE EX-EFF (7:4)           TO DC-GREG-DATE-CYMD-R (1:4)
           MOVE EX-EFF (1:2)           TO DC-GREG-DATE-CYMD-R (5:2)
           MOVE EX-EFF (4:2)           TO DC-GREG-DATE-CYMD-R (7:2)
      *    MOVE EX-EFF                 TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
           IF NOT NO-CONVERSION-ERROR
              DISPLAY ' INVALID EFF DATE ' EX-EFF '  ' EX-CERT
      *       PERFORM ABEND-PGM
           ELSE
              MOVE DC-BIN-DATE-1       TO CM-CERT-EFF-DT
           END-IF

           MOVE SPACES                 TO WS-CERT-CANCELLED-SW
           READ ELCERT

           IF ELCERT-FILE-STATUS = '00'
              ADD 1                    TO ELCERT-IN-CNT
              IF (CM-LF-CANCEL-APPLIED OR CM-AH-CANCEL-APPLIED)
                 AND (CERT-AS-LOADED)
                 SET CERT-ALREADY-CANCELLED
                                       TO TRUE
                 DISPLAY ' CERT ALREADY CANCELLED ' CM-CERT-NO
              END-IF
           ELSE
              IF (ELCERT-FILE-STATUS = '23' OR '10')
                 OR (CM-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
PEMTST*          DISPLAY ' NO ELCERT    ' EX-ACCOUNT '  ' EX-CERT
PEMTST           CONTINUE
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
PEMTST        MOVE '00'                TO ELCERT-FILE-STATUS
PEMTST*       REWRITE CERTIFICATE-MASTER
              IF ELCERT-FILE-STATUS = '00'
      *          DISPLAY ' REWRITE CERT ' EX-ACCOUNT '  ' EX-CERT
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
PEMTST             ELCERT
PEMTST*            ERMAIL
PEMTST*    OPEN I-O ELCERT

           IF ELCERT-FILE-STATUS NOT = '00'
              DISPLAY ' ELCERT OPEN ' ELCERT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

PEMTST     OPEN I-O ERMAIL

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

           IF ERMAIL-FILE-STATUS = '00'
              PERFORM 0077-CHECK-FOR-DUPS
                                       THRU 0077-EXIT
              IF DUPLICATE-MAILING
                 ADD 1                 TO ERMAIL-DP-CNT
      *          DISPLAY ' DUPLICATES FOUND ' EX-CERT
              ELSE              
                 DISPLAY ' UPDATING ERMAIL  ' EX-CERT
                 PERFORM 0075-UPDATE-ERMAIL-BODY
                                       THRU 0075-EXIT
                 PERFORM 0073-REWRITE-ERMAIL
                                       THRU 0073-EXIT
              END-IF
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
                 DISPLAY ' ADDING   ERMAIL  ' EX-CERT
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
       
           MOVE 'CR'                   TO MA-SOURCE-SYSTEM
           MOVE ZEROS                  TO MA-INSURED-ISSUE-AGE
                                          MA-PHONE-NO
           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +7
              MOVE LOW-VALUES          TO MA-MAIL-DATE (S1)
           END-PERFORM

           MOVE WS-CURRENT-BIN-DATE    TO MA-RECORD-ADD-DT
                                          MA-LAST-MAINT-DT
           MOVE 'POST'                 TO MA-RECORD-ADDED-BY
           MOVE +120000                TO MA-LAST-MAINT-HHMMSS

           .
       0072-EXIT.
           EXIT.

       0073-REWRITE-ERMAIL.
       
           PERFORM 0076-CHK-FOR-HEX7E  THRU 0076-EXIT

PEMTST*    MOVE '00'                   TO ERMAIL-FILE-STATUS
PEMTST     REWRITE MAILING-DATA

           IF ERMAIL-FILE-STATUS = '00'
              DISPLAY ' REWRITE ERMAIL ' EX-ACCOUNT '  ' EX-CERT
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
       
           PERFORM 0076-CHK-FOR-HEX7E  THRU 0076-EXIT

           MOVE 'MA'                   TO MA-RECORD-ID
                                          
PEMTST*    MOVE '00'                   TO ERMAIL-FILE-STATUS
PEMTST     WRITE MAILING-DATA

           IF ERMAIL-FILE-STATUS = '00'
              DISPLAY ' WRITE ERMAIL   ' EX-ACCOUNT '  ' EX-CERT
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
       
           IF CERT-ALREADY-CANCELLED
              ADD 1                    TO ELCERT-AC-CNT
           END-IF

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              (S1 > +7)
              OR (MA-MAIL-TYPE (S1) = SPACES)
           END-PERFORM

           IF S1 > +7
              DISPLAY ' POST CARD INFO FULL FOR ' MA-ACCOUNT
              '  ' MA-CERT-NO
              DISPLAY ' OR MAYBE IT ALREADY GOT UPDATED '
           ELSE
              MOVE WS-CURRENT-BIN-DATE TO MA-LAST-MAINT-DT
              MOVE 'POST'              TO MA-LAST-MAINT-BY
              IF EX-MAIL-TYPE (1:1) = 'E'
                 MOVE '2'              TO MA-MAIL-TYPE (S1)
              ELSE
                 MOVE '1'              TO MA-MAIL-TYPE (S1)
              END-IF
              IF EX-MAIL-STATUS = 'M '
                 MOVE '1'              TO MA-MAIL-STATUS (S1)
              ELSE
                 IF EX-MAIL-STATUS = 'R '
                    MOVE '2'           TO MA-MAIL-STATUS (S1)
                 ELSE
                    MOVE '3'           TO MA-MAIL-STATUS (S1)
                 END-IF
              END-IF
              IF EX-MAIL-STATUS = 'M ' OR 'R '
                 MOVE EX-MAIL-DATE (7:4)
                                       TO V-ISS-CCYR
                 MOVE EX-MAIL-DATE (1:2)
                                       TO V-ISS-MO
                 MOVE EX-MAIL-DATE (4:2)
                                       TO V-ISS-DA
                 MOVE WS-WORK-DT       TO DC-GREG-DATE-CYMD
                 MOVE 'L'              TO DC-OPTION-CODE
                 PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
                 IF NOT NO-CONVERSION-ERROR
                    DISPLAY ' INVALID MAIL DATE ' EX-MAIL-DATE
                       '  ' EX-CERT
                    PERFORM ABEND-PGM
                 ELSE
                    MOVE DC-BIN-DATE-1 TO MA-MAIL-DATE (S1)
                 END-IF
              ELSE
                 MOVE LOW-VALUES       TO MA-MAIL-DATE (S1)
              END-IF
           END-IF

           IF EX-ADDRESS1 NOT = MA-ADDRESS-LINE-1
              MOVE EX-ADDRESS1         TO MA-ADDRESS-LINE-1
              MOVE 'Y'                 TO MA-ADDRESS-CORRECTED
           END-IF
           IF EX-ADDRESS2 NOT = MA-ADDRESS-LINE-2
              MOVE EX-ADDRESS2         TO MA-ADDRESS-LINE-2
              MOVE 'Y'                 TO MA-ADDRESS-CORRECTED
           END-IF
           IF EX-CITY-STATE NOT = MA-CITY-STATE
              MOVE EX-CITY-STATE       TO MA-CITY-STATE
              MOVE 'Y'                 TO MA-ADDRESS-CORRECTED
           END-IF
           IF EX-ZIP (1:9) NOT = MA-ZIP
              MOVE EX-ZIP (1:9)        TO MA-ZIP
              MOVE 'Y'                 TO MA-ADDRESS-CORRECTED
           END-IF

           .
       0075-EXIT.
           EXIT.

       0076-CHK-FOR-HEX7E.

      *  I KNOW, I KNOW, UR ASKING Y I ADDED THIS.
      *  WELL, I WAS GETTING SOME ~ CHARACTERS FROM THE 
      *  EXTRACT FILE FROM SDGA, SO I ADDED THIS CODE

           INSPECT MA-ADDRESS-LINE-1
              REPLACING ALL X'7E' BY SPACES
           INSPECT MA-ADDRESS-LINE-2
              REPLACING ALL X'7E' BY SPACES
           INSPECT MA-CITY-STATE
              REPLACING ALL X'7E' BY SPACES
           INSPECT MA-ZIP
              REPLACING ALL X'7E' BY SPACES
              
           .
       0076-EXIT.
           EXIT.

       0077-CHECK-FOR-DUPS.

           MOVE SPACES                 TO WS-DUP-SW
           MOVE EX-MAIL-DATE (7:4)     TO V-ISS-CCYR
           MOVE EX-MAIL-DATE (1:2)     TO V-ISS-MO
           MOVE EX-MAIL-DATE (4:2)     TO V-ISS-DA
           MOVE WS-WORK-DT             TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-BIN-COMPARE-DT
           ELSE
              DISPLAY ' ERROR DURING DT CONV FOR DUPS ' EX-CERT
              MOVE LOW-VALUES          TO WS-BIN-COMPARE-DT
           END-IF
           EVALUATE EX-MAIL-TYPE (1:1)
              WHEN 'E'
                 MOVE '2'              TO WS-COMPARE-TYPE
              WHEN OTHER
                 MOVE '1'              TO WS-COMPARE-TYPE
           END-EVALUATE

           EVALUATE EX-MAIL-STATUS
              WHEN 'M '
                 MOVE '1'              TO WS-COMPARE-STATUS
              WHEN 'R '
                 MOVE '2'              TO WS-COMPARE-STATUS
              WHEN OTHER
                 MOVE '3'              TO WS-COMPARE-STATUS
           END-EVALUATE

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              (S1 > +7)
              OR (MA-MAIL-TYPE (S1) = SPACES)
              OR (DUPLICATE-MAILING)
              IF (WS-BIN-COMPARE-DT = MA-MAIL-DATE (S1))
                 AND (WS-COMPARE-TYPE = MA-MAIL-TYPE (S1))
                 AND (WS-COMPARE-STATUS = MA-MAIL-STATUS (S1))
                 SET DUPLICATE-MAILING TO TRUE
              END-IF
           END-PERFORM

           .
       0077-EXIT.
           EXIT.

       8500-DATE-CONVERT.

           CALL 'ELDATCX'              USING DATE-CONVERSION-DATA.

       8500-EXIT.
           EXIT.
           EJECT

       ABEND-PGM.   COPY ELCABEND.

