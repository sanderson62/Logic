       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDCNF1.
       AUTHOR.     PABLO.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERNOTE           ASSIGN TO ERNOTE
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CN-CONTROL-PRIMARY
                                   FILE STATUS IS ERNOTE-FILE-STATUS.

           SELECT ERNOTE2          ASSIGN TO ERNOTE2
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CN2-CONTROL-PRIMARY
                                   FILE STATUS IS ERNOTE2-FILE-STATUS.

           SELECT DISK-DATE     ASSIGN TO SYS019-FBA1-S-SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  ERNOTE.

                                       COPY ERCNOTE.

       FD  ERNOTE2.

       01  CERTIFICATE-NOTE2.
           12  CN2-RECORD-ID                PIC  XX.
               88  VALID-CN2-ID                  VALUE 'CN'.
     
           12  CN2-CONTROL-PRIMARY.
               16  CN2-COMPANY-CD          PIC X.  
               16  CN2-CARRIER             PIC X.  
               16  CN2-GROUPING            PIC X(6).
               16  CN2-STATE               PIC XX. 
               16  CN2-ACCOUNT             PIC X(10).
               16  CN2-CERT-EFF-DT         PIC XX. 
               16  CN2-CERT-NO.                     
                   20  CN2-CERT-PRIME      PIC X(10).
                   20  CN2-CERT-SFX        PIC X.
     
           12  CN2-BILLING-START-LINE-NO   PIC 99.
           12  CN2-BILLING-END-LINE-NO     PIC 99.
     
           12  CN2-LINES.
               16  CN2-LINE OCCURS 10      PIC X(77).
     
           12  CN2-LAST-MAINT-DT           PIC XX.
           12  CN2-LAST-MAINT-HHMMSS       PIC S9(7)   COMP-3.
           12  CN2-LAST-MAINT-USER         PIC X(4).
           12  FILLER                      PIC X(6).





082603 FD  DISK-DATE                   COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDCNF1  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-INPUT              VALUE 'Y'.
       77  WS-RECS-IN                  PIC 9(9) VALUE ZEROS.
       77  WS-RECS-FIX                 PIC 9(9) VALUE ZEROS.
       77  WS-RECS-DEL                 PIC 9(9) VALUE ZEROS.
       77  WS-RECS-ADD                 PIC 9(9) VALUE ZEROS.
       77  WS-BAD-LINES                PIC 9(9) VALUE ZEROS.
       77  S1                          PIC S999 VALUE +0 COMP-3.
       77  S2                          PIC S999 VALUE +0 COMP-3.
       77  WS-CERT-EFF-DT              PIC X(10)  VALUE SPACES.
       77  WS-REWRITE-SW               PIC X  VALUE SPACES.
           88  REWRITE-RECORD             VALUE 'Y'.

       01  WS-MISC.
           05  PGM-SUB                 PIC S9(4)   VALUE +548.
           05  WS-RETURN-CODE          PIC S9(3)   VALUE ZERO.
           05  WS-ABEND-MESSAGE        PIC X(80)   VALUE SPACES.
           05  WS-ZERO                 PIC S9      VALUE ZERO.
           05  WS-ABEND-FILE-STATUS    PIC XX      VALUE ZERO.

      ******************************************************************
       01  WS-HOLD-ERNOTE              PIC X(825).
       01  FILLER.
           05  WS-WORK-SEQ             PIC X.
           05  WS-NUM-SEQ REDEFINES WS-WORK-SEQ PIC 9.
           05  ERNOTE-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  ERNOTE2-FILE-STATUS     PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-INPUT  THRU 0100-EXIT UNTIL
                 (END-OF-INPUT)
PEMTST*          OR (WS-RECS-IN > 1000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' RECORDS READ    '  WS-RECS-IN
           DISPLAY ' RECORDS FIXED   '  WS-RECS-FIX
           DISPLAY ' RECORDS DELETED '  WS-RECS-DEL
           DISPLAY ' RECORDS ADDED   '  WS-RECS-ADD
           DISPLAY ' BILL LINES WITH GARBAGE ' WS-BAD-LINES
           GOBACK

           .
       0100-PROCESS-INPUT.

           PERFORM 0105-BUILD-EXTRACT  THRU 0105-EXIT

           PERFORM 0200-READ-ERNOTE    THRU 0200-EXIT

           .
       0100-EXIT.
           EXIT.

       0105-BUILD-EXTRACT.

           MOVE ' '                    TO WS-REWRITE-SW

           MOVE CN-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO WS-CERT-EFF-DT
           ELSE
              MOVE '01/01/1965'        TO WS-CERT-EFF-DT
              DISPLAY ' ERROR - EFF DT CONVERT ' CN-CARRIER ' '
                 CN-GROUPING ' ' CN-STATE ' ' CN-ACCOUNT ' '
                 CN-CERT-NO ' DELETING RECORD '
              DELETE ERNOTE
              IF ERNOTE-FILE-STATUS = '00'
                 ADD 1 TO WS-RECS-DEL
              ELSE
                 DISPLAY ' ERROR - ERNOTE - DELETE ' ERNOTE-FILE-STATUS
              END-IF
              GO TO 0105-EXIT
           END-IF

           IF CN-CERT-SFX = X'00'
              DISPLAY ' FOUND LV CERT SUFFIX ' CN-CARRIER ' '
                 CN-GROUPING ' ' CN-STATE ' ' CN-ACCOUNT ' '
                 WS-CERT-EFF-DT ' ' CN-CERT-NO
              PERFORM 0110-CHECK-FOR-DUP THRU 0110-EXIT
              GO TO 0105-EXIT
           END-IF

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +10
              PERFORM VARYING S2 FROM +1 BY +1 UNTIL
                 S2 > +75
                 IF CN-LINE (S1) (S2:3) = X'434E04' OR X'434E9C'
                    DISPLAY ' REMOVING BILL LINE ' S1 ' ' CN-CARRIER ' '
                       CN-GROUPING ' ' CN-STATE ' ' CN-ACCOUNT ' '
                       WS-CERT-EFF-DT ' ' CN-CERT-NO ' ' CN-LINE (S1)
                    MOVE SPACES        TO CN-LINE (S1)
                    MOVE +76           TO S2
                    ADD 1              TO WS-BAD-LINES
                    SET REWRITE-RECORD TO TRUE
                 END-IF
              END-PERFORM
           END-PERFORM

           IF REWRITE-RECORD
              PERFORM 0300-REWRITE-RECORD
                                       THRU 0300-EXIT
           END-IF

           .
       0105-EXIT.
           EXIT.

       0110-CHECK-FOR-DUP.

           DISPLAY ' ORIGINAL ' CN-LINE (1) ' ' CN-LINE (2)

           MOVE CERTIFICATE-NOTE       TO WS-HOLD-ERNOTE
           DELETE ERNOTE
           IF ERNOTE-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERNOTE - DELETE ' ERNOTE-FILE-STATUS
              PERFORM ABEND-PGM
           ELSE
              ADD 1 TO WS-RECS-DEL
           END-IF
           MOVE WS-HOLD-ERNOTE         TO CERTIFICATE-NOTE
           MOVE ' '                    TO CN-CERT-SFX
           WRITE CERTIFICATE-NOTE
           IF ERNOTE-FILE-STATUS NOT = '00' AND '22'
              DISPLAY ' ERROR - ERNOTE - WRITE ' ERNOTE-FILE-STATUS
              PERFORM ABEND-PGM
           ELSE
              ADD 1 TO WS-RECS-ADD
           END-IF

      *    MOVE CN-CONTROL-PRIMARY     TO CN2-CONTROL-PRIMARY
      *    MOVE SPACE                  TO CN2-CERT-SFX
      *    READ ERNOTE2
      *    IF ERNOTE2-FILE-STATUS = '00'
      *       DISPLAY ' FOUND DUPLICATE ' CN-LINE (1) ' ' CN-LINE (2)
      *       DISPLAY ' ORIGINAL        ' CN2-LINE (1) ' ' CN2-LINE (2)
      *    ELSE
      *       DISPLAY ' SHOULD CONVERT TO SPACE ' CN-LINE (1) ' '
      *          CN-LINE (2)
      *    END-IF

           .
       0110-EXIT.
           EXIT.

       0200-READ-ERNOTE.

           READ ERNOTE NEXT RECORD

           IF ERNOTE-FILE-STATUS = '10' OR '23'
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERNOTE-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERNOTE READ ' ERNOTE-FILE-STATUS
                 SET END-OF-INPUT      TO TRUE
              ELSE
                 IF CN-COMPANY-CD > DTE-CLASIC-COMPANY-CD
                    SET END-OF-INPUT  TO TRUE
                 END-IF
              END-IF
           END-IF

           IF NOT END-OF-INPUT
              ADD 1 TO WS-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-REWRITE-RECORD.

PEMTST     REWRITE CERTIFICATE-NOTE
PEMTST*    MOVE '00'                   TO ERNOTE-FILE-STATUS

           IF ERNOTE-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERNOTE - REWRITE  ' ERNOTE-FILE-STATUS
              SET END-OF-INPUT         TO TRUE
           ELSE
              ADD 1                    TO WS-RECS-FIX
           END-IF

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

PEMTST     OPEN I-O ERNOTE
PEMTST*    OPEN INPUT ERNOTE
                INPUT ERNOTE2

           IF ERNOTE-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERNOTE - OPEN  ' ERNOTE-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERNOTE2-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERNOTE2 - OPEN  ' ERNOTE2-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ERNOTE

           .
       0500-EXIT.
           EXIT.

       0550-START-ERNOTE.

           MOVE DTE-CLASIC-COMPANY-CD  TO CN-CONTROL-PRIMARY

           START ERNOTE KEY > CN-CONTROL-PRIMARY

           IF ERNOTE-FILE-STATUS = '10' OR '23'
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERNOTE-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERNOTE - START  ' ERNOTE-FILE-STATUS
                 SET END-OF-INPUT      TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.


       0600-INITIALIZE.

           PERFORM 0550-START-ERNOTE   THRU 0550-EXIT
           PERFORM 0200-READ-ERNOTE    THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM SECTION.              COPY ELCABEND.
