       IDENTIFICATION DIVISION.                                         10/08/97
                                                                        ECS216
       PROGRAM-ID.                 PEMAMM1.                                LV002
      *                                                                 ECS216
      *AUTHOR.        PABLO                                             ECS216
      *               OMAHA, NEBRASKA        GO HUSKERS                 ECS216
      *                                                                 ECS216
      *DATE-COMPILED.                                                   ECS216
      *                                                                 ECS216
      *SECURITY.   *****************************************************ECS216
      *            *                                                   *ECS216
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *ECS216
      *            *                                                   *ECS216
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS216
      *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *ECS216
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO             *ECS216
      *            *                                                   *ECS216
      *            *****************************************************ECS216
      *                                                                 ECS216
      *REMARKS.                                                         ECS216
      *        THIS PROGRAM READS 2 SEQ ACCT FILES AND COMPARES THE     ECS216
      *        COMMISSION STRUCTURE. THIS IS FOR UNI AND JMA ONLY.      ECS216
      *     I USED THIS FOR IR2007040500001                         
                                                                        ECS216
       ENVIRONMENT DIVISION.                                            ECS216
       INPUT-OUTPUT SECTION.                                            ECS216
       FILE-CONTROL.                                                    ECS216
                                                                        ECS216
           SELECT  ACCT-IN1      ASSIGN TO SYS010.                      ECS216
           SELECT  ACCT-IN2      ASSIGN TO SYS011.                      ECS216
       EJECT                                                            ECS216
       DATA DIVISION.                                                   ECS216
       FILE SECTION.                                                    ECS216
                                                                        ECS216
       FD  ACCT-IN1                                                     ECS216
           RECORDING MODE F                                             ECS216
           LABEL RECORDS STANDARD                                       ECS216
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS ACCT-REC-IN1.                                 ECS216

       01  ACCT-REC-IN1.
           05  FILLER                  PIC XXX.
           05  ACCT-IN1-KEY            PIC X(25).
           05  FILLER                  PIC X(1972).

       FD  ACCT-IN2                                                     ECS216
           RECORDING MODE F                                             ECS216
           LABEL RECORDS STANDARD                                       ECS216
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS ACCT-REC-IN2.                                 ECS216

       01  ACCT-REC-IN2.
           05  FILLER                  PIC XXX.
           05  ACCT-IN2-KEY            PIC X(25).
           05  FILLER                  PIC X(1972).

       WORKING-STORAGE SECTION.                                         ECS216
       77  FILLER  PIC X(32) VALUE '********************************'.  ECS216
       77  FILLER  PIC X(32) VALUE '  PEMAMM1   WORKING-STORAGE     '.  ECS216
       77  FILLER  PIC X(32) VALUE '*********** VMOD=2.001. ********'.
       77  WS-STATUS-CODE PIC XX COMP-5.
       77  S1                          PIC 999 VALUE ZEROS.
       77  S2                          PIC S999 COMP-3 VALUE +0.
       77  S3                          PIC S999 COMP-3 VALUE +0.

       77  WS-EOF-SW1                  PIC X  VALUE SPACES.
           88  END-OF-INPUT1              VALUE '1'.

       77  WS-EOF-SW2                  PIC X  VALUE SPACES.
           88  END-OF-INPUT2              VALUE '1'.

       01  WS1-ACCT-EXP-DT             PIC X(10)  VALUE SPACES.
       01  WS2-ACCT-EXP-DT             PIC X(10)  VALUE SPACES.

       01  WS1-COMM-STRUC.
           05  WS1-AGT-COMMS       OCCURS 10 TIMES.
               10  WS1-AGT              PIC X(10).
               10  WS1-COM-TYP            PIC X.
               10  WS1-L-COM              PIC SV9(5)     COMP-3.
               10  WS1-J-COM              PIC SV9(5)     COMP-3.
               10  WS1-A-COM              PIC SV9(5)     COMP-3.
               10  WS1-RECALC-LV-INDIC    PIC X.
               10  WS1-RETRO-LV-INDIC     PIC X.
               10  WS1-GL-CODES           PIC X.
               10  WS1-COMM-CHARGEBACK    PIC 99.
               10  FILLER                 PIC X.

       01  WS2-COMM-STRUC.
           05  WS2-AGT-COMMS       OCCURS 10 TIMES.
               10  WS2-AGT              PIC X(10).
               10  WS2-COM-TYP            PIC X.
               10  WS2-L-COM              PIC SV9(5)     COMP-3.
               10  WS2-J-COM              PIC SV9(5)     COMP-3.
               10  WS2-A-COM              PIC SV9(5)     COMP-3.
               10  WS2-RECALC-LV-INDIC    PIC X.
               10  WS2-RETRO-LV-INDIC     PIC X.
               10  WS2-GL-CODES           PIC X.
               10  WS2-COMM-CHARGEBACK    PIC 99.
               10  FILLER                 PIC X.

       01  MISC.
           05  WS-IN-RECS1             PIC 9(13) VALUE ZEROS.
           05  WS-IN-RECS2             PIC 9(13) VALUE ZEROS.
           05  WS-OT-RECS              PIC 9(13) VALUE ZEROS.

                                       COPY ELCDATE.
                                       COPY ERCACCT.

       PROCEDURE DIVISION.

       0002-INPUT.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0060-READ-ACCT1     THRU 0060-EXIT
           PERFORM 0065-READ-ACCT2     THRU 0065-EXIT

           PERFORM 0080-PROCESS-ACCTS  THRU 0080-EXIT UNTIL
              END-OF-INPUT1 AND END-OF-INPUT2

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT
           DISPLAY ' RECS IN1 ' WS-IN-RECS1
           DISPLAY ' RECS IN2 ' WS-IN-RECS2
           DISPLAY ' RECS OT  ' WS-OT-RECS
           
           GOBACK

           .
       0002-EXIT.
           EXIT.


       0020-OPEN-FILES.

           OPEN INPUT ACCT-IN1 ACCT-IN2

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE ACCT-IN1 ACCT-IN2

           .
       0030-EXIT.
           EXIT.

       0060-READ-ACCT1.

           READ ACCT-IN1 AT END
               MOVE HIGH-VALUES        TO ACCT-IN1-KEY
               SET END-OF-INPUT1       TO TRUE
           END-READ

           IF NOT END-OF-INPUT1
              ADD 1                    TO WS-IN-RECS1
              MOVE ACCT-REC-IN1        TO ACCOUNT-MASTER
              IF AM-REPORT-CODE-3 = 'UNI' OR 'JM&A'
                 CONTINUE
              ELSE
                 GO TO 0060-READ-ACCT1
              END-IF
           END-IF
           
           .
       0060-EXIT.
           EXIT.

       0065-READ-ACCT2.

           READ ACCT-IN2 AT END
               MOVE HIGH-VALUES        TO ACCT-IN2-KEY
               SET END-OF-INPUT2       TO TRUE
           END-READ

           IF NOT END-OF-INPUT2
              ADD 1                    TO WS-IN-RECS2
              MOVE ACCT-REC-IN2        TO ACCOUNT-MASTER
              IF AM-REPORT-CODE-3 = 'UNI' OR 'JM&A'
                 CONTINUE
              ELSE
                 GO TO 0065-READ-ACCT2
              END-IF
           END-IF
           
           .
       0065-EXIT.
           EXIT.

       0080-PROCESS-ACCTS.

           IF ACCT-IN1-KEY < ACCT-IN2-KEY
              PERFORM 0060-READ-ACCT1  THRU 0060-EXIT
           ELSE
              IF ACCT-IN1-KEY > ACCT-IN2-KEY
                 PERFORM 0065-READ-ACCT2
                                       THRU 0065-EXIT
              ELSE
      *          DISPLAY ' FOUND MATCH ' ACCT-IN1-KEY (1:19)
                 PERFORM 0090-COMPARE-COMM
                                       THRU 0090-EXIT
                 PERFORM 0060-READ-ACCT1
                                       THRU 0060-EXIT
              END-IF
           END-IF

           .
       0080-EXIT.
           EXIT.

       0090-COMPARE-COMM.

           MOVE ACCT-REC-IN1           TO ACCOUNT-MASTER
           MOVE AM-COMM-STRUCTURE      TO WS1-COMM-STRUC

           IF AM-EXPIRATION-DT = HIGH-VALUES
              MOVE '12/31/9999'        TO WS1-ACCT-EXP-DT
           ELSE
              MOVE AM-EXPIRATION-DT    TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT
                                       TO WS1-ACCT-EXP-DT
              ELSE
                 MOVE 'ERROR'          TO WS1-ACCT-EXP-DT
              END-IF
           END-IF

           MOVE ACCT-REC-IN2           TO ACCOUNT-MASTER
           MOVE AM-COMM-STRUCTURE      TO WS2-COMM-STRUC

           IF AM-EXPIRATION-DT = HIGH-VALUES
              MOVE '12/31/9999'        TO WS2-ACCT-EXP-DT
           ELSE
              MOVE AM-EXPIRATION-DT    TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT
                                       TO WS2-ACCT-EXP-DT
              ELSE
                 MOVE 'ERROR'          TO WS2-ACCT-EXP-DT
              END-IF
           END-IF

           IF WS1-COMM-STRUC NOT = WS2-COMM-STRUC
      *       DISPLAY ' FOUND DIFFERENCE ' AM-STATE ' ' AM-ACCOUNT
              PERFORM 0100-COMM-DIFF   THRU 0100-EXIT
           END-IF

           PERFORM 0110-CHECK-FOR-DUPS THRU 0110-EXIT

           .
       0090-EXIT.
           EXIT.

       0100-COMM-DIFF.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL
              S1 > 10
              IF (WS1-AGT (S1) NOT = WS2-AGT (S1))
                           AND
                 ((WS1-AGT (S1) (1:5) = '00012')
                 OR (WS2-AGT (S1) (1:5) = '00012'))
                 DISPLAY ' ' AM-STATE ' ' AM-ACCOUNT ' ' WS1-ACCT-EXP-DT
                    ' LEVEL ' S1 ' DIFF OLD '
                    WS1-AGT (S1) ' NEW ' WS2-AGT (S1)
              END-IF
           END-PERFORM

           .
       0100-EXIT.
           EXIT.

       0110-CHECK-FOR-DUPS.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL
              S1 > 10
              COMPUTE S2 = S1 + 1
              PERFORM VARYING S2 FROM S2 BY 1 UNTIL
                 S2 > 10
                 IF (WS2-AGT (S1) NOT = SPACES AND ZEROS)
                    AND (WS2-AGT (S1) = WS2-AGT (S2))
                    DISPLAY ' DUP ' AM-STATE ' ' AM-ACCOUNT ' '
                       WS2-AGT (S1)
                 END-IF
              END-PERFORM
           END-PERFORM

           .
       0110-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.
