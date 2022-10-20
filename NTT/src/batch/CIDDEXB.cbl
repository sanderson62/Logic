       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDDEXB.
       AUTHOR.     PABLO.
       DATE-COMPILED.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
030722* 030722    2022030100003  PEMA  Chg LnOffCd to 5 positions
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

030722     SELECT CERT-FILE-IN     ASSIGN TO CERTIN.
           SELECT EXTR-FILE-IN     ASSIGN TO EXTRIN.

           SELECT EXTR-FILE-OUT    ASSIGN TO EXTROT
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE        ASSIGN TO SYS019.
                                                                        
       DATA DIVISION.
       FILE SECTION.

030722 FD  CERT-FILE-IN
030722     RECORDING MODE F
030722     LABEL RECORDS STANDARD
030722     BLOCK CONTAINS 0 RECORDS.
030722
030722                                 COPY ECSCRT01.

       FD  EXTR-FILE-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

                                       COPY ECSEXT01.

       FD  EXTR-FILE-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  EXTR-FILE-OUT-REC           PIC X(515).

       FD  DISK-DATE                                                    
                                   COPY ELCDTEFD.                       

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDDEXB  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-EXTR               VALUE 'Y'.
030722 77  WS-CERT-SW              PIC X VALUE SPACES.
030722     88  END-OF-CERT               VALUE 'Y'.
030722 77  CRT-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  EXT-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  EXT-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.
       77  a1                      pic s999  value +0 comp-3.
       77  PGM-SUB                 PIC S999    COMP    VALUE +020.
       77  WS-EFF-BIN              PIC XX    VALUE LOW-VALUES.
       77  WS-CAN-BIN              PIC XX    VALUE LOW-VALUES.
       77  WS-EXP-BIN              PIC XX    VALUE LOW-VALUES.
       77  WS-EARN-TERM            PIC 999   VALUE ZEROS.
       77  WS-WORK-TERM            PIC S9(3) VALUE +0 COMP-3.
       77  WS-REM-TERM             PIC S9(3) VALUE +0 COMP-3.

       01  WS-SAVE-EXTR                PIC X(515) VALUE LOW-VALUES.
       01  EXTR-DETAIL-RECORD.
           12  EX-CARRIER              PIC X.
           12  EX-GROUP                PIC X(6).
           12  EX-STATE                PIC XX.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-EFF-DATE             PIC X(10).
           12  EX-CERT-NO              PIC X(11).
           12  ex-type                 pic x.
           12  EX-LNAME                PIC X(15).
           12  EX-FNAME                PIC X(10).
           12  ex-mid-init             pic x.
           12  EX-AGE                  PIC 999.
           12  EX-GENDER               PIC X.
           12  EX-LF-SINJNT            PIC X.
           12  EX-LF-BENCD             PIC XX.
           12  EX-LF-TERM              PIC 999.
           12  EX-LF-ISS-BEN           PIC 9(9).99.
           12  EX-LF-ISS-PREM          PIC 9999999.99.
           12  EX-LF-REF-PREM          PIC -9999999.99.
           12  EX-AH-BENCD             PIC XX.
           12  EX-AH-TERM              PIC 999.
           12  EX-AH-ISS-BEN           PIC 9(9).99.
           12  EX-AH-ISS-PREM          PIC 9999999.99.
           12  EX-AH-REF-PREM          PIC -9999999.99.
           12  EX-PROC-DT              PIC X(10).
           12  EX-LF-CAN-DT            PIC X(10).
           12  EX-AH-CAN-DT            PIC X(10).
           12  EX-LOAN-OFF             PIC X(5).
           12  EX-ENTRY-STATUS         PIC X.
           12  EX-REPORT-CD-1          PIC X(10).
           12  EX-REPORT-CD-2          PIC X(10).
           12  FILLER OCCURS 10.
               16  EX-AGT              PIC X(10).
               16  EX-AGT-TYP          PIC X.
               16  EX-LF-COM           PIC .99999.
               16  EX-AH-COM           PIC .99999.
           12  EX-EOR                  PIC X.
      ******************************************************************
      ******************************************************************
       01  WS-MISC.
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

                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

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
              AND (DE-TRANS = 'I' OR 'C')
              AND (DE-ENTRY-STATUS NOT = 'D' AND 'V' AND '9')
              PERFORM 0100-PROCESS-EXTR
                                       THRU 0100-EXIT
           END-IF

           PERFORM 0200-READ-EXTR      THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-EXTR.

           IF DE-ENTRY-STATUS = '5'
              AND DE-TRANS = 'I'
              go to 0100-exit
           end-if

030722     perform 0105-match-cert     thru 0105-exit
           perform 0110-build-common   thru 0110-exit
           IF DE-TRANS = 'I'
              perform 0120-build-issue thru 0120-exit
           else
              perform 0130-build-canc  thru 0130-exit
           end-if

           PERFORM 0300-WRITE-EXTR     THRU 0300-EXIT

           .
       0100-EXIT.
           EXIT.

030722 0105-MATCH-CERT.
030722
030722     evaluate true
030722        when cr-full-control < de-control
030722           perform 0250-read-cert
030722                                 thru 0250-exit
030722           go to 0105-match-cert
030722        when cr-full-control > de-control
030722           display ' no cert for extract ? '
030722           display '     cert ' cr-acct-control ' ' cr-cert-no
030722           display '    extr  ' de-cntrl1 ' ' de-cert-no
030722           perform abend-pgm
030722     end-evaluate
030722
030722**  We must have found a match then 
030722
030722     .
030722 0105-EXIT.
030722     EXIT.

       0110-build-common.

           MOVE WS-SAVE-EXTR           TO EXTR-DETAIL-RECORD

           MOVE DE-CARRIER             TO EX-CARRIER
           MOVE DE-GROUPING            TO EX-GROUP
           MOVE DE-STATE               TO EX-STATE
           MOVE DE-ACCOUNT             TO EX-ACCOUNT
           MOVE DE-EFF                 TO WS-DATE

           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EX-EFF-DATE
           END-STRING

           MOVE DE-CERT                TO EX-CERT-NO
           MOVE DE-LNAME               TO EX-LNAME
           MOVE DE-FNAME               TO EX-FNAME
           move de-init                to ex-mid-init
           move de-age                 to ex-age
           move de-sex                 to ex-gender
030722*    move de-ln-officer          to ex-loan-off
030722     move cr-loan-officer        to ex-loan-off
           move de-entry-status        to ex-entry-status
           move de-report-code-1       to ex-report-cd-1
           move de-report-code-2       to ex-report-cd-2

           if de-lf-type not = spaces and zeros
              perform varying clas-indexl from clas-startl by +1 until
                 clas-i-ben (clas-indexl) = de-lf-type
                 or clas-indexl > clas-maxl
              end-perform
              if clas-i-ben (clas-indexl) = de-lf-type
                 if clas-i-joint (clas-indexl) = 'J'           
                    move 'J'           to ex-lf-sinjnt
                 else
                    move 'S'           to ex-lf-sinjnt
                 end-if
              end-if
           end-if

           move de-lf-type             to ex-lf-bencd
           move de-lf-term             to ex-lf-term
           compute ex-lf-iss-ben = de-lf-ben + de-lf-ben-alt
           COMPUTE EX-LF-iss-PREM = DE-LF-PRM + DE-LF-PRM-ALT
           move de-lf-rfnd             to ex-lf-ref-prem

           move de-ah-type             to ex-ah-bencd
           move de-ah-term             to ex-ah-term
           move de-ah-ben              to ex-ah-iss-ben
           MOVE DE-AH-PRM              TO EX-AH-iss-PREM
           move de-ah-rfnd             to ex-ah-ref-prem

           MOVE DE-proc-dt             TO WS-DATE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EX-proc-dt
           END-STRING

030722     perform varying sub1 from +1 by +1 until sub1 > +10
              if de-agt (sub1) = spaces or low-values
                 move zeros            to de-agt (sub1)
              end-if
           end-perform

           perform varying a1 from +1 by +1 until a1 > +10
              MOVE DE-AGT (a1)         TO EX-AGT (a1)
              MOVE DE-AGT-TYPE (a1)    TO EX-AGT-TYP (a1)
              MOVE DE-L-PC (a1)        TO EX-LF-COM  (a1)
              MOVE DE-A-PC (a1)        TO EX-AH-COM  (a1)
           end-perform

           MOVE 'E'                    TO EX-EOR

           .
       0110-exit.
           exit.
       0120-build-issue.

           move 'I'                    to ex-type
           move zeros                  to ex-lf-ref-prem
                                          ex-ah-ref-prem

           .
       0120-exit.
           exit.
       0130-build-canc.

           move 'C'                    to ex-type
           move zeros                  to ex-lf-iss-prem
                                          ex-ah-iss-prem
           if de-lf-canc-dte not = zeros
              MOVE de-lf-canc-dte         TO WS-DATE
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                  INTO EX-lf-can-dt
              END-STRING
           end-if

           if de-ah-canc-dte not = zeros
              MOVE de-ah-canc-dte         TO WS-DATE
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                  INTO EX-ah-can-dt
              END-STRING
           end-if

           .
       0130-exit.
           exit.

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

030722 0250-READ-CERT.
030722
030722     READ CERT-FILE-IN AT END
030722        SET END-OF-CERT          TO TRUE
030722     END-READ
030722
030722
030722     IF NOT END-OF-CERT
030722        ADD 1 TO CRT-RECS-IN
030722     END-IF
030722
030722     .
030722 0250-EXIT.
030722     EXIT.

       0300-WRITE-EXTR.

           string
              EX-CARRIER         ';'
              EX-GROUP           ';'
              EX-STATE           ';'
              EX-ACCOUNT         ';'
              EX-EFF-DATE        ';'
              EX-CERT-NO         ';'
              ex-type            ';'
              EX-LNAME           ';'
              EX-FNAME           ';'
              ex-mid-init        ';'
              EX-AGE             ';'
              EX-GENDER          ';'
              EX-LF-SINJNT       ';'
              EX-LF-BENCD        ';'
              EX-LF-TERM         ';'
              EX-LF-ISS-BEN      ';'
              EX-LF-ISS-PREM     ';'
              EX-LF-REF-PREM     ';'
              EX-AH-BENCD        ';'
              EX-AH-TERM         ';'
              EX-AH-ISS-BEN      ';'
              EX-AH-ISS-PREM     ';'
              EX-AH-REF-PREM     ';'
              EX-PROC-DT         ';'
              EX-LF-CAN-DT       ';'
              EX-AH-CAN-DT       ';'
              EX-LOAN-OFF        ';'
              EX-ENTRY-STATUS    ';'
              EX-REPORT-CD-1     ';'
              EX-REPORT-CD-2     ';'
              EX-AGT       (1)   ';'
              EX-AGT-TYP   (1)   ';'
              EX-LF-COM    (1)   ';'
              EX-AH-COM    (1)   ';'
              EX-AGT       (2)   ';'
              EX-AGT-TYP   (2)   ';'
              EX-LF-COM    (2)   ';'
              EX-AH-COM    (2)   ';'
              EX-AGT       (3)   ';'
              EX-AGT-TYP   (3)   ';'
              EX-LF-COM    (3)   ';'
              EX-AH-COM    (3)   ';'
              EX-AGT       (4)   ';'
              EX-AGT-TYP   (4)   ';'
              EX-LF-COM    (4)   ';'
              EX-AH-COM    (4)   ';'
              EX-AGT       (5)   ';'
              EX-AGT-TYP   (5)   ';'
              EX-LF-COM    (5)   ';'
              EX-AH-COM    (5)   ';'
              EX-AGT       (6)   ';'
              EX-AGT-TYP   (6)   ';'
              EX-LF-COM    (6)   ';'
              EX-AH-COM    (6)   ';'
              EX-AGT       (7)   ';'
              EX-AGT-TYP   (7)   ';'
              EX-LF-COM    (7)   ';'
              EX-AH-COM    (7)   ';'
              EX-AGT       (8)   ';'
              EX-AGT-TYP   (8)   ';'
              EX-LF-COM    (8)   ';'
              EX-AH-COM    (8)   ';'
              EX-AGT       (9)   ';'
              EX-AGT-TYP   (9)   ';'
              EX-LF-COM    (9)   ';'
              EX-AH-COM    (9)   ';'
              EX-AGT       (10)  ';'
              EX-AGT-TYP   (10)  ';'
              EX-LF-COM    (10)  ';'
              EX-AH-COM    (10)  ';'
              EX-EOR             ';'

              delimited by size into extr-file-out-rec

           end-string

           WRITE EXTR-FILE-OUT-REC
           ADD 1                       TO EXT-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN
              INPUT
030722           CERT-FILE-IN
                 EXTR-FILE-IN
              OUTPUT
                 EXTR-FILE-OUT

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE
030722        CERT-FILE-IN
              EXTR-FILE-IN
              EXTR-FILE-OUT

           .
       0500-EXIT.
           EXIT.

       0600-INITIALIZE.

           MOVE SPACES                 TO EXTR-DETAIL-RECORD

           MOVE 'E'                    TO EX-EOR

           MOVE ZEROS                  TO EX-LF-iss-PREM
                                          EX-AH-iss-PREM
                                          EX-LF-ref-prem
                                          EX-AH-REF-prem

           MOVE EXTR-DETAIL-RECORD     TO WS-SAVE-EXTR

           PERFORM 0200-READ-EXTR      THRU 0200-EXIT
030722     PERFORM 0250-READ-CERT      THRU 0250-EXIT

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

