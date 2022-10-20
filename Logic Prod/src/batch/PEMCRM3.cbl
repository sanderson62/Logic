       IDENTIFICATION DIVISION.                                         00000100
       PROGRAM-ID.                PEMCRM3.                              00000200
      *AUTHOR.     PABLO.                                               00000400
      *REMARKS.                                                         00000900
      *     THIS PROGRAM READS THE CERT, MATCHES TO ELCERT AND WRITES
      *       OUT A NEW CERT.
       ENVIRONMENT DIVISION.                                            00001500
       INPUT-OUTPUT SECTION.                                            00001600
       FILE-CONTROL.                                                    00001700

           SELECT ELCERT-IN            ASSIGN TO SYS010.

           SELECT CERT-IN              ASSIGN TO SYS011.

           SELECT CERT-OUT             ASSIGN TO SYS013.
                                                                        00002200
       DATA DIVISION.                                                   00002400
                                                                        00002500
       FILE SECTION.                                                    00002600

       FD  CERT-IN                                                      00004060
           RECORDING MODE IS F                                          00004070
           LABEL RECORDS ARE STANDARD                                   00004080
           BLOCK CONTAINS 0 RECORDS.                                    00004091

                                      COPY ECSCRT01.                    00004094

       FD  CERT-OUT                                                     00004060
           RECORDING MODE IS F                                          00004070
           LABEL RECORDS ARE STANDARD                                   00004080
           BLOCK CONTAINS 0 RECORDS.                                    00004091

       01  CERT-OUT-RECORD            PIC X(1056).                      00004094

       FD  ELCERT-IN                                                    00004060
           RECORDING MODE IS F                                          00004070
           LABEL RECORDS ARE STANDARD                                   00004080
           RECORD CONTAINS 450 CHARACTERS                               00004090
           BLOCK CONTAINS 0 RECORDS.                                    00004091

                                       COPY ELCCERT.

       WORKING-STORAGE SECTION.                                         00007200
       77  FILLER  PIC X(32) VALUE '********************************'.  00007300
       77  FILLER  PIC X(32) VALUE '         WORKING-STORAGE        '.  00007400
       77  FILLER  PIC X(32) VALUE '********************************'.  00007500

       01  WS-COMPARE-CERT-KEY.
           05  WS-CCK-CARRIER          PIC X.
           05  WS-CCK-GROUP            PIC X(6).
           05  WS-CCK-STATE            PIC XX.
           05  WS-CCK-ACCOUNT          PIC X(10).
           05  WS-CCK-EFF-DT           PIC 9(11) COMP-3.
           05  WS-CCK-CERT-NO          PIC X(11).

       01  W-MISC.
           05  WS-WORK-DT              PIC 9(8)  VALUE ZEROS.
           05  WS-WORK-DTR REDEFINES WS-WORK-DT.
               10  WS-WORK-DT-CCYY      PIC X(4).
               10  WS-WORK-DT-MM        PIC XX.
               10  WS-WORK-DT-DD        PIC XX.
           05  WS-DATE-ALPH.
               10  FILLER             PIC XXX VALUE '000'.
               10  WS-WORK-CENT       PIC XX.
               10  WS-WORK-YR         PIC XX.
               10  WS-WORK-MO         PIC XX.
               10  WS-WORK-DA         PIC XX.
           05  WS-DATE-NUM REDEFINES WS-DATE-ALPH
                                      PIC 9(11).
           05  WS-WORK-TERM           PIC 999 VALUE ZEROS.
           05  WS-BIN-EFF             PIC XX VALUE LOW-VALUES.
           05  WS-BIN-CNC             PIC XX VALUE LOW-VALUES.

           05  ELCERT-IN-CNT          PIC 9(9) VALUE ZEROS.
           05  CERT-IN-CNT             PIC 9(9) VALUE ZEROS.
           05  CERT-UPD-CNT            PIC 9(9) VALUE ZEROS.
           05  CERT-MATCH-CNT          PIC 9(9) VALUE ZEROS.
           05  CERT-PC-Y-CNT           PIC 9(9) VALUE ZEROS.
           05  CERT-IN-PC-Y-CNT        PIC 9(9) VALUE ZEROS.
           05  CERT-OT-CNT             PIC 9(9) VALUE ZEROS.
           05  WS-EOF-SW              PIC X VALUE SPACES.
               88  END-OF-INPUT             VALUE 'Y'.
           05  WS-CRT-SW              PIC X VALUE ' '.
               88  END-OF-CERT              VALUE 'Y'.
           05  W-PREM-N               PIC 9(7)V99.                      00010840
           05  W-PREM-X      REDEFINES  W-PREM-N.                       00010841
               10  W-PREM-DOL.                                          00010842
                   15  W-PREM-DOL2    PIC X(02).                        00010843
                   15  W-PREM-DOL5    PIC X(05).                        00010844
               10  W-PREM-CEN         PIC X(02).                        00010845
           05  WS-ALPHA-AMT.
               10  WS-DOLLARS         PIC X(5).
               10  WS-CENTS           PIC XX.
           05  WS-NUM-AMT REDEFINES WS-ALPHA-AMT
                                      PIC 9(5)V99.

                                       COPY ELCDATE.

       PROCEDURE DIVISION.

           OPEN INPUT ELCERT-IN CERT-IN

           OPEN OUTPUT CERT-OUT

           PERFORM 0010-READ-ELCERT    THRU 0010-EXIT
           PERFORM 0015-READ-CERT      THRU 0015-EXIT

           PERFORM 0020-PROCESS THRU 0020-EXIT UNTIL
              (END-OF-INPUT)
              AND (END-OF-CERT)

           CLOSE   ELCERT-IN   CERT-IN  CERT-OUT

           DISPLAY ' CERT RECS READ       ' CERT-IN-CNT
           DISPLAY ' ELCERT  RECS READ    ' ELCERT-IN-CNT
           DISPLAY ' CERT RECS OUT        ' CERT-OT-CNT
           DISPLAY ' CERT RECS UPDATED    ' CERT-UPD-CNT
           DISPLAY ' CERT RECS MATCHED    ' CERT-MATCH-CNT
           DISPLAY ' CERT IN RECS WITH Y  ' CERT-IN-PC-Y-CNT
           DISPLAY ' CERT OUT RECS WITH Y ' CERT-PC-Y-CNT

           GOBACK

           .
       0010-READ-ELCERT.

           READ ELCERT-IN AT END
              SET END-OF-INPUT         TO TRUE
              MOVE HIGH-VALUES         TO WS-COMPARE-CERT-KEY
           END-READ

           IF NOT END-OF-INPUT
              IF CM-COMPANY-CD > X'04'
                 SET END-OF-INPUT      TO TRUE
                 MOVE HIGH-VALUES      TO WS-COMPARE-CERT-KEY
              END-IF
           END-IF

           IF NOT END-OF-INPUT
              MOVE CM-CONTROL-PRIMARY (2:19)
                                       TO WS-COMPARE-CERT-KEY
              MOVE CM-CERT-EFF-DT      TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-CYMD
                                       TO WS-CCK-EFF-DT
              ELSE
                 DISPLAY ' ERROR - EFF DT CONVERT ' CM-CERT-NO
                 MOVE ZEROS            TO WS-CCK-EFF-DT
              END-IF
              MOVE CM-CERT-NO          TO WS-CCK-CERT-NO
              ADD  1                   TO ELCERT-IN-CNT
           END-IF
           
           .
       0010-EXIT.
           EXIT.

       0015-READ-CERT.

           READ CERT-IN AT END
              SET END-OF-CERT          TO TRUE
              MOVE HIGH-VALUES         TO CR-FULL-CONTROL
           END-READ

           IF NOT END-OF-CERT
              ADD  1                   TO CERT-IN-CNT
           END-IF

           IF NOT END-OF-CERT
              IF CR-POST-CARD-IND = 'Y'
                 ADD 1                 TO CERT-IN-PC-Y-CNT
              END-IF
           END-IF

           .
       0015-EXIT.
           EXIT.

       0020-PROCESS.

      *    DISPLAY ' EXT KEY = ' EXT-KEY
      *    DISPLAY 'CERT KEY = ' CERT-KEY

           IF CR-FULL-CONTROL < WS-COMPARE-CERT-KEY
              PERFORM 0040-WRITE-CERT  THRU 0040-EXIT
              PERFORM 0015-READ-CERT   THRU 0015-EXIT
           ELSE
              IF CR-FULL-CONTROL > WS-COMPARE-CERT-KEY
      *          DISPLAY ' ERROR - NO MATCHING CERT ' WS-CCK-CARRIER
      *          ' ' WS-CCK-STATE ' ' WS-CCK-ACCOUNT ' ' WS-CCK-EFF-DT
      *          ' ' WS-CCK-CERT-NO
      *          DISPLAY ' FULL CONTROL ' CR-ACCT-CONTROL ' '
      *            CR-DT ' ' CR-CERT-NO
                 PERFORM 0010-READ-ELCERT
                                       THRU 0010-EXIT
              ELSE
                 PERFORM 0030-MATCHED  THRU 0030-EXIT
                 PERFORM 0040-WRITE-CERT
                                       THRU 0040-EXIT
                 PERFORM 0010-READ-ELCERT
                                       THRU 0010-EXIT
                 PERFORM 0015-READ-CERT
                                       THRU 0015-EXIT
              END-IF
           END-IF

           .
       0020-EXIT.
            EXIT.

       0030-MATCHED.

           ADD 1                       TO CERT-MATCH-CNT

           IF CR-POST-CARD-IND = 'Y'
              CONTINUE
           ELSE
              IF CM-POST-CARD-IND = 'Y' OR 'N'
                 ADD 1                 TO CERT-UPD-CNT
                 MOVE CM-POST-CARD-IND TO CR-POST-CARD-IND
              ELSE
                 MOVE SPACES           TO CR-POST-CARD-IND
              END-IF
           END-IF

      *    INSPECT EXT-TOT-FEE REPLACING ALL ' ' BY ZEROS
      *    INSPECT EXT-LF-FEE  REPLACING ALL ' ' BY ZEROS
      *    INSPECT EXT-AH-FEE  REPLACING ALL ' ' BY ZEROS

      *    MOVE EXT-TOT-FEE (1:5)      TO WS-DOLLARS
      *    MOVE EXT-TOT-FEE (7:2)      TO WS-CENTS
      *    MOVE WS-NUM-AMT             TO CR-MOB-NET-TOT-FEES

      *    MOVE EXT-LF-FEE (1:5)       TO WS-DOLLARS
      *    MOVE EXT-LF-FEE (7:2)       TO WS-CENTS
      *    MOVE WS-NUM-AMT             TO CR-LFPRM

      *    MOVE EXT-AH-FEE (1:5)       TO WS-DOLLARS
      *    MOVE EXT-AH-FEE (7:2)       TO WS-CENTS
      *    MOVE WS-NUM-AMT             TO CR-AHPRM

           .   
       0030-EXIT.                                                       00119300
            EXIT.                                                       00119400

       0040-WRITE-CERT.
       
           IF CR-POST-CARD-IND NOT = 'Y' AND 'N' AND ' '
              MOVE SPACES              TO CR-POST-CARD-IND
           END-IF

           IF CR-POST-CARD-IND = 'Y'
              ADD 1                    TO CERT-PC-Y-CNT
           END-IF

           WRITE CERT-OUT-RECORD       FROM CERTIFICATE-RECORD
           ADD 1                       TO CERT-OT-CNT

           .
       0040-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.
         