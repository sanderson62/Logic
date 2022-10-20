       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCCDEXA.
       AUTHOR.     PABLO.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EXTR-FILE-IN     ASSIGN TO SYS010.

           SELECT EXTR-FILE-OUT    ASSIGN TO SYS011
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE        ASSIGN TO SYS019.
                                                                        
       DATA DIVISION.
       FILE SECTION.

       FD  EXTR-FILE-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

                                       COPY ECSEXX01.

       FD  EXTR-FILE-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  EXTR-FILE-OUT-REC           PIC X(600).

       FD  DISK-DATE                                                    
                                   COPY ELCDTEFD.                       

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   DCCDEXA  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-EXTR               VALUE 'Y'.
       77  EXT-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  EXT-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  S1                      PIC S999 COMP-3 VALUE +0.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.
       77  PGM-SUB                 PIC S999    COMP    VALUE +020.
       77  WS-EFF-BIN              PIC XX    VALUE LOW-VALUES.
       77  WS-CAN-BIN              PIC XX    VALUE LOW-VALUES.
       77  WS-EXP-BIN              PIC XX    VALUE LOW-VALUES.
       77  WS-EARN-TERM            PIC 999   VALUE ZEROS.
       77  WS-WORK-TERM            PIC S9(3) VALUE +0 COMP-3.
       77  WS-REM-TERM             PIC S9(3) VALUE +0 COMP-3.

       01  WS-SAVE-EXTR                PIC X(600) VALUE LOW-VALUES.
       01  EXTR-DETAIL-RECORD.
           12  EX-REC-TYPE             PIC X.
           12  EX-CARRIER              PIC X.
           12  EX-GROUP                PIC X(6).
           12  EX-STATE                PIC XX.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-EFF-DATE             PIC X(10).
           12  EX-CERT-NO              PIC X(11).
           12  EX-LNAME                PIC X(15).
           12  EX-FNAME                PIC X(10).
           12  ex-lf-bencd             pic xx.
           12  ex-ah-bencd             pic xx.
           12  EX-LF-PREM              PIC -9999999.99.
           12  EX-AH-PREM              PIC -9999999.99.
           12  EX-LF-CLP               PIC -9(7).99.
           12  EX-AH-CLP               PIC -9(7).99.
           12  filler occurs 10.
               16  EX-AGT              PIC X(10).
               16  EX-AGT-TYP          PIC X.
               16  EX-LF-COM           PIC -999.99.
               16  EX-AH-COM           PIC -999.99.
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
       
           IF (DX-REIN = ' ')
              AND (DX-TRANS = 'I' OR 'C')
              PERFORM 0100-PROCESS-EXTR
                                       THRU 0100-EXIT
           END-IF

           PERFORM 0200-READ-EXTR      THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-EXTR.

           MOVE WS-SAVE-EXTR           TO EXTR-DETAIL-RECORD

           move DX-trans               to ex-rec-type
           MOVE DX-CARRIER             TO EX-CARRIER
           MOVE DX-GROUPING            TO EX-GROUP
           MOVE DX-STATE               TO EX-STATE
           MOVE DX-ACCOUNT             TO EX-ACCOUNT
           MOVE DX-EFF                 TO WS-DATE

           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EX-EFF-DATE
           END-STRING

           MOVE DX-CERT                TO EX-CERT-NO
           MOVE DX-LNAME               TO EX-LNAME
           MOVE DX-FNAME               TO EX-FNAME

           IF (DX-ENTRY-STATUS = '5')
              AND (DX-TRANS = 'I')
              MOVE ZEROS               TO DX-LF-PRM
                                          DX-LF-PRM-ALT
                                          DX-AH-PRM
           END-IF

           if dx-lf-type not = '00'
              MOVE DX-LF-TYPE          TO EX-LF-BENCD
           end-if

           if dx-ah-type not = '00'
              MOVE DX-AH-TYPE          TO EX-AH-BENCD
           end-if

           IF DX-TRANS = 'I'
              COMPUTE EX-LF-PREM = DX-LF-PRM + DX-LF-PRM-ALT
              MOVE DX-AH-PRM           TO EX-AH-PREM
              MOVE DX-REI-LFPRM        TO EX-LF-CLP
              MOVE DX-REI-AHPRM        TO EX-AH-CLP
           ELSE
              COMPUTE EX-LF-PREM = DX-LF-RFND * -1
              COMPUTE EX-AH-PREM = DX-AH-RFND * -1
              compute ex-lf-clp  = DX-rei-lfrfnd * -1           
              compute ex-ah-clp  = DX-rei-ahrfnd * -1           
           END-IF

           perform varying s1 from +1 by +1 until s1 > +10
              move DX-agt (s1)         to ex-agt (s1)
              move DX-agt-type (s1)    to ex-agt-typ (s1)
              if DX-trans = 'I'
                 move DX-l-pc (s1)     to ex-lf-com (s1)
                 move DX-a-pc (s1)     TO EX-AH-COM (S1)
              else
                 compute ex-lf-com (s1) = DX-l-pc (s1) * -1
                 compute ex-ah-com (s1) = DX-a-pc (s1) * -1
              end-if
           end-perform

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

           move spaces                 to extr-file-out-rec
           string     EX-REC-TYPE     ';'
                      EX-CARRIER      ';'
                      EX-GROUP        ';'
                      EX-STATE        ';'
                      EX-ACCOUNT      ';'
                      EX-EFF-DATE     ';'
                      EX-CERT-NO      ';'
                      EX-LNAME        ';'
                      EX-FNAME        ';'
                      EX-LF-BENCD     ';'
                      EX-AH-BENCD     ';'
                      EX-LF-PREM      ';'
                      EX-AH-PREM      ';'
                      EX-LF-CLP       ';'
                      EX-AH-CLP       ';'
                      ex-agt     (01) ';'
                      ex-agt-typ (01) ';'
                      ex-lf-com  (01) ';'
                      ex-ah-com  (01) ';'
                      ex-agt     (02) ';'
                      ex-agt-typ (02) ';'
                      ex-lf-com  (02) ';'
                      ex-ah-com  (02) ';'
                      ex-agt     (03) ';'
                      ex-agt-typ (03) ';'
                      ex-lf-com  (03) ';'
                      ex-ah-com  (03) ';'
                      ex-agt     (04) ';'
                      ex-agt-typ (04) ';'
                      ex-lf-com  (04) ';'
                      ex-ah-com  (04) ';'
                      ex-agt     (05) ';'
                      ex-agt-typ (05) ';'
                      ex-lf-com  (05) ';'
                      ex-ah-com  (05) ';'
                      ex-agt     (06) ';'
                      ex-agt-typ (06) ';'
                      ex-lf-com  (06) ';'
                      ex-ah-com  (06) ';'
                      ex-agt     (07) ';'
                      ex-agt-typ (07) ';'
                      ex-lf-com  (07) ';'
                      ex-ah-com  (07) ';'
                      ex-agt     (08) ';'
                      ex-agt-typ (08) ';'
                      ex-lf-com  (08) ';'
                      ex-ah-com  (08) ';'
                      ex-agt     (09) ';'
                      ex-agt-typ (09) ';'
                      ex-lf-com  (09) ';'
                      ex-ah-com  (09) ';'
                      ex-agt     (10) ';'
                      ex-agt-typ (10) ';'
                      ex-lf-com  (10) ';'
                      ex-ah-com  (10) ';'
                      ex-eor
              delimited by size into extr-file-out-rec
           end-string

           WRITE EXTR-FILE-OUT-REC
           ADD 1                       TO EXT-RECS-OUT

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

           string      'REC TYP'         ';'
                       'CARRIER'         ';'
                       'GROUP'           ';'
                       'STATE'           ';'
                       'ACCOUNT'         ';'
                       'EFF DTE'         ';'
                       'CERT NO'         ';'
                       'LAST NAME'       ';'
                       'FIRST NAME'      ';'
                       'LF BEN CD'       ';'
                       'AH BEN CD'       ';'
                       'LF PREM'         ';'
                       'AH PREM'         ';'
                       'LF CLP  '        ';'
                       'AH CLP  '        ';'
                       'AGT1 '           ';'
                       'AGT1 TYP'        ';'
                       'LF COMM1'        ';'
                       'AH COMM1'        ';'
                       'AGT2 '           ';'
                       'AGT2 TYP'        ';'
                       'LF COMM2'        ';'
                       'AH COMM2'        ';'
                       'AGT3 '           ';'
                       'AGT3 TYP'        ';'
                       'LF COMM3'        ';'
                       'AH COMM3'        ';'
                       'AGT4 '           ';'
                       'AGT4 TYP'        ';'
                       'LF COMM4'        ';'
                       'AH COMM4'        ';'
                       'AGT5 '           ';'
                       'AGT5 TYP'        ';'
                       'LF COMM5'        ';'
                       'AH COMM5'        ';'
                       'AGT6 '           ';'
                       'AGT6 TYP'        ';'
                       'LF COMM6'        ';'
                       'AH COMM6'        ';'
                       'AGT7 '           ';'
                       'AGT7 TYP'        ';'
                       'LF COMM7'        ';'
                       'AH COMM7'        ';'
                       'AGT8 '           ';'
                       'AGT8 TYP'        ';'
                       'LF COMM8'        ';'
                       'AH COMM8'        ';'
                       'AGT9 '           ';'
                       'AGT9 TYP'        ';'
                       'LF COMM9'        ';'
                       'AH COMM9'        ';'
                       'AGT10'           ';'
                       'AGT10 TYP'       ';'
                       'LF COMM10'       ';'
                       'AH COMM10'       ';'
                       'EOR'
              delimited by size into EXTR-FILE-OUT-REC
           END-STRING

           WRITE EXTR-FILE-OUT-REC

           MOVE SPACES                 TO EXTR-DETAIL-RECORD

           MOVE 'E'                    TO EX-EOR

           MOVE ZEROS                  TO EX-LF-PREM
                                          EX-AH-PREM
                                          EX-LF-CLP
                                          EX-AH-CLP
           PERFORM VARYING S1 FROM +1 BY +1 UNTIL S1 > +10
              MOVE ZEROS               TO EX-LF-COM (S1)
                                          EX-AH-COM (S1)
           END-PERFORM

           MOVE EXTR-DETAIL-RECORD     TO WS-SAVE-EXTR

           PERFORM 0200-READ-EXTR      THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

       8500-DATE-CONVERT.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8500-EXIT.
           EXIT.

       ABEND-PGM SECTION.
                                     COPY ELCABEND.

