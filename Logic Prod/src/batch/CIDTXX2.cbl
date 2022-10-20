       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDTXX2.
       AUTHOR.     PABLO.
       DATE-COMPILED.
030912******************************************************************
030912*                   C H A N G E   L O G
030912*
030912* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
030912*-----------------------------------------------------------------
030912*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
030912* EFFECTIVE    NUMBER
030912*-----------------------------------------------------------------
030912* 030912    2011022800001  AJRA  ACCT SERV NAPERSOFT
080113* 080113    2013062000003  AJRA  ADD BARCODE,RETURN ENV FLAG
052417* 052417  IR2017052400001  PEMA  Correct reading beyond EOF
030912******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELLETR           ASSIGN TO ELLETR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS TX-CONTROL-PRIMARY
                                   FILE STATUS IS ELLETR-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT ELLETR-OUT       ASSIGN TO ELLETROT
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  ELLETR.

                                       COPY ELCTEXT.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  ELLETR-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

080113 01  ELLETR-OUT-REC              PIC X(50).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDTXX2  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-ELLETR             VALUE 'Y'.
       77  LET-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  LET-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  PGM-SUB                     PIC S9(5) COMP-3 VALUE +515.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.

       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04) COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

030912*01  W-Z-CONTROL-DATA.
030912                COPY ELCZREC.

080113 01  WS-SAVE-ELLETR              PIC X(50) VALUE SPACES.
       01  ELLETR-DETAIL-RECORD.
           12  EX-LETR-NO              PIC XXXX.
           12  EX-TAB1                 PIC X.
           12  EX-NO-OF-COPIES         PIC X.
           12  EX-TAB2                 PIC X.
           12  EX-DAYS-TO-FU           PIC XXX.
           12  EX-TAB3                 PIC X.
           12  EX-DAYS-TO-RS           PIC XXX.
           12  EX-TAB4                 PIC X.
           12  EX-FORM-TO-RS           PIC XXXX.
           12  EX-TAB5                 PIC X.
           12  EX-PROMPT-LTR           PIC X.
           12  EX-TAB6                 PIC X.
           12  EX-ENC-CD               PIC XXX.
           12  EX-TAB7                 PIC X.
           12  EX-AUTO-CLOSE-IND       PIC X.
           12  EX-TAB8                 PIC X.
           12  EX-LTR-TO-BENE          PIC X.
           12  EX-TAB9                 PIC X.
030912     12  EX-LTR-TO-ACCT          PIC X.
030912     12  EX-TAB10                PIC X.
030912     12  EX-LTR-TYPE             PIC X.
030912     12  EX-TAB11                PIC X.
030912     12  EX-PRINT-CERT           PIC X.
030912     12  EX-TAB12                PIC X.
030912     12  EX-REFUND-REQ           PIC X.
030912     12  EX-TAB13                PIC X.
030912     12  EX-ONBASE-CODE          PIC XX.
030912     12  EX-TAB14                PIC X.
072312     12  EX-ACCT-SUMM            PIC X.
072312     12  EX-TAB15                PIC X.
072312     12  EX-CSO-SUMM             PIC X.
072312     12  EX-TAB16                PIC X.
080113     12  EX-ADD-BAR-CODE         PIC X.
080113     12  EX-TAB17                PIC X.
080113     12  EX-HAS-RETURN-ENV       PIC X.
080113     12  EX-TAB18                PIC X.
           12  EX-EOR                  PIC X.


      ******************************************************************
       01  WS-MISC.
           05  WS-WORK-SEQ             PIC X.
           05  WS-NUM-SEQ REDEFINES WS-WORK-SEQ PIC 9.
           05  ELLETR-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.

                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-ELLETR THRU 0100-EXIT UNTIL
                 (END-OF-ELLETR)
PEMTST*          OR (LET-RECS-IN > 1000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' LETR  RECORDS READ    '  LET-RECS-IN
           DISPLAY ' LETR  RECORDS WRITTEN '  LET-RECS-OUT
           GOBACK

           .
       0100-PROCESS-ELLETR.

           IF (TEXT-FILE-ID = 'TL')
              AND (TX-LINE-SQUEEZE-CONTROL = 'Z')
              PERFORM 0105-BUILD-EXTRACT THRU 0105-EXIT
           END-IF

           PERFORM 0200-READ-ELLETR    THRU 0200-EXIT

           .
       0100-EXIT.
           EXIT.

       0105-BUILD-EXTRACT.

           MOVE WS-SAVE-ELLETR         TO ELLETR-DETAIL-RECORD

           MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA

           MOVE TX-LETTER-NO           TO EX-LETR-NO 
           MOVE W-NUMBER-OF-COPIES     TO EX-NO-OF-COPIES
           MOVE W-DAYS-TO-FOLLOW-UP    TO EX-DAYS-TO-FU
030912     MOVE W-DAYS-TO-RESEND       TO EX-DAYS-TO-RS
           MOVE W-FORM-TO-RESEND       TO EX-FORM-TO-RS
           MOVE W-PROMPT-LETTER        TO EX-PROMPT-LTR
           MOVE W-ENCLOSURE-CD         TO EX-ENC-CD
           MOVE W-AUTO-CLOSE-IND       TO EX-AUTO-CLOSE-IND
           MOVE W-LETTER-TO-BENE       TO EX-LTR-TO-BENE
030912     MOVE W-LETTER-TO-ACCT       TO EX-LTR-TO-ACCT
030912     MOVE W-LETTER-TYPE          TO EX-LTR-TYPE
030912     MOVE W-PRINT-CERTIFICATE    TO EX-PRINT-CERT
030912     MOVE W-REFUND-REQUIRED      TO EX-REFUND-REQ
030912     MOVE W-ONBASE-CODE          TO EX-ONBASE-CODE
072312     MOVE W-ACCT-SUMM            TO EX-ACCT-SUMM
072312     MOVE W-CSO-SUMM             TO EX-CSO-SUMM
080113     MOVE W-ADD-BAR-CODE         TO EX-ADD-BAR-CODE
080113     MOVE W-HAS-RETURN-ENV       TO EX-HAS-RETURN-ENV
           IF EX-NO-OF-COPIES NOT NUMERIC
              MOVE ZEROS               TO EX-NO-OF-COPIES
           END-IF
           IF EX-DAYS-TO-FU NOT NUMERIC
              MOVE ZEROS               TO EX-DAYS-TO-FU
           END-IF
           IF EX-DAYS-TO-RS NOT NUMERIC
              MOVE ZEROS               TO EX-DAYS-TO-RS
           END-IF

           PERFORM 0300-WRITE-LETR     THRU 0300-EXIT

           .
       0105-EXIT.
           EXIT.

       0200-READ-ELLETR.

           READ ELLETR NEXT RECORD

           IF (ELLETR-FILE-STATUS = '10' OR '23')
              OR (TX-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-ELLETR        TO TRUE
           ELSE
              IF ELLETR-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ELLETR READ NEXT ' ELLETR-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           IF NOT END-OF-ELLETR
              ADD 1 TO LET-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-LETR.

           WRITE ELLETR-OUT-REC        FROM ELLETR-DETAIL-RECORD
           ADD 1 TO LET-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT ELLETR
               OUTPUT ELLETR-OUT

           IF ELLETR-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY 'ELLETR OPEN ERR  ' ELLETR-FILE-STATUS
           END-IF

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ELLETR ELLETR-OUT

           .
       0500-EXIT.
           EXIT.

       0550-START-ELLETR.

           MOVE DTE-CLASIC-COMPANY-CD  TO TX-CONTROL-PRIMARY

           START ELLETR KEY IS > TX-CONTROL-PRIMARY

           IF ELLETR-FILE-STATUS = '10' OR '23'
              SET END-OF-ELLETR        TO TRUE
           ELSE
              IF ELLETR-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ELLETR - START  ' ELLETR-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.

       0600-INITIALIZE.

           MOVE SPACES                 TO ELLETR-DETAIL-RECORD

           MOVE ';'                    TO EX-TAB1
                                          EX-TAB2
                                          EX-TAB3
                                          EX-TAB4
                                          EX-TAB5
                                          EX-TAB6
                                          EX-TAB7
                                          EX-TAB8
                                          EX-TAB9
030912                                    EX-TAB10
030912                                    EX-TAB11
030912                                    EX-TAB12
030912                                    EX-TAB13
030912                                    EX-TAB14
072312                                    EX-TAB15
072312                                    EX-TAB16
080113                                    EX-TAB17
080113                                    EX-TAB18
           MOVE 'E'                    TO EX-EOR

           MOVE ELLETR-DETAIL-RECORD   TO WS-SAVE-ELLETR
           PERFORM 0550-START-ELLETR   THRU 0550-EXIT
052417     if not end-of-elletr
052417        PERFORM 0200-READ-ELLETR THRU 0200-EXIT
052417     end-if

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
