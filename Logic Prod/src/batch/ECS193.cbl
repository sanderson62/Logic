       IDENTIFICATION DIVISION.
       PROGRAM-ID. ECS193.
       AUTHOR.     CENTRAL STATES HEALTH AND LIFE.
       DATE-COMPILED.
      *SECURITY.   *****************************************************
      *            *                                                  *
      *            *  THIS PROGRAM IS THE PROPERTY OF CENTRAL STATES  *
      *            *                                                  *
      *            *  USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *  OF CSO        IS EXPRESSLY PROHIBITED WITHOUT   *
      *            *  THE PRIOR WRITTEN PERMISSION OF CENTRAL STATES. *
      *            *                                                  *
      *            *****************************************************
      *REMARKS.

      *       GENERAL FUNCTION OF THIS PROGRAM IS TO READ THE
      *       COMPENSATION MASTER AFTER MONTH-END AND CREATE A
      *       REPORT OF CURRENT OVER/UNDER AMOUNTS.

      *       INPUT FILES-    COMPENSATION MASTER
      *                       DISK-DATE

      *       OUTPUT-         REPORT FILE

           EJECT
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT DISK-DATE        ASSIGN TO SYS019-FBA1-S-SYS019.

           SELECT PRINT-FILE       ASSIGN TO SYS008-UR-1403-S-SYS008.
           SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.
           SELECT COMP-IN          ASSIGN TO SYS010-UT-2400-S-SYS010.

           SELECT PYAJ-OUT         ASSIGN TO SYS011-UT-2400-S-SYS011.

           EJECT
       DATA DIVISION.

       FILE SECTION.

       FD  DISK-DATE                   COPY ELCDTEFD.

           EJECT
       FD  COMP-IN
           RECORDING MODE F.
                                       COPY ERCCOMP.
           EJECT
       FD  PYAJ-OUT
           RECORDING MODE F.
       01  PYAJ-OUT-REC                PIC X(200).

           EJECT
       FD  PRINT-FILE
                                       COPY ELCPRTFD.
       EJECT
       FD  FICH
                                       COPY ELCFCHFD.
       EJECT
       WORKING-STORAGE SECTION.
       01  LCP-ABND-CODE               PIC S999 COMP VALUE +519.

       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '    ECS193 WORKING-STORAGE      '.
       77  FILLER  PIC X(32) VALUE '******** VMOD=2.001 ************'.

       77  X                           PIC X       VALUE ' '.
       77  PAGE-CNT                    PIC S9(3)   COMP-3  VALUE +0.
       77  LINE-CNT                    PIC S9(3)   COMP-3  VALUE +99.
       01  WS-SWITCH-AREA.
           12  WS-COMP-READ-CNT        PIC 9(7)   VALUE ZERO.
           12  WS-PYAJ-OUT-CNT         PIC 9(7)   VALUE ZERO.

       01  STANDARD-AREAS.
           12  WS-SAVE-CARRIER         PIC X       VALUE SPACES.
           12  WS-GRAND-CREDITS        PIC S9(11)V99 COMP-3 VALUE +0.
           12  WS-GRAND-DEBITS         PIC S9(11)V99 COMP-3 VALUE +0.
           12  WS-CARR-CREDITS         PIC S9(11)V99 COMP-3 VALUE +0.
           12  WS-CARR-DEBITS          PIC S9(11)V99 COMP-3 VALUE +0.
           12  WS-EOF-SW               PIC X       VALUE SPACES.
               88  THERE-ARE-NO-MORE-RECORDS       VALUE 'Y'.
           12  WS-ABEND-MESSAGE        PIC X(80)   VALUE SPACES.
           12  WS-ABEND-FILE-STATUS    PIC XX      VALUE ZERO.

           12  WS-RETURN-CODE          PIC S9(4) COMP VALUE ZERO.
           12  WS-ZERO                 PIC S9(4) COMP VALUE ZERO.
           12  WS-SAVE-DATE            PIC XX.
           12  WS-SAVE-TIME            PIC 9(6).
           12  ABEND-CODE              PIC XXXX   VALUE SPACES.
           12  ABEND-OPTION            PIC X      VALUE SPACES.
           12  PGM-SUB                 PIC S999 COMP VALUE +193.
           12  WS-CR-MONTH-END-DT      PIC XX.

           12  WS-CURRENT-DT           PIC XX     VALUE '  '.

           12  WS-HOLD-DESCRIPTION     PIC X(30)  VALUE SPACES.

           EJECT
       01  HEAD-LINE-1.
           12  FILLER              PIC  X(44)          VALUE SPACES.
           12  FILLER              PIC  X(36)          VALUE
                   'ACCOUNTS WITH BALANCES WRITTEN OFF  '.
           12  FILLER              PIC  X(44)          VALUE SPACES.
           12  FILLER              PIC  X(7)           VALUE 'ECS-193'.
           12  FILLER              PIC  X              VALUE SPACE.

       01  HEAD-LINE-2.
           12  FILLER              PIC  X(47)          VALUE SPACES.
           12  HD-CLIENT           PIC  X(30).
           12  FILLER              PIC  X(47)          VALUE SPACES.
           12  HD-RUN              PIC  X(8).

       01  HEAD-LINE-3.
           12  FILLER              PIC  X(53)          VALUE SPACES.
           12  HD-DATE             PIC  X(18).
           12  FILLER              PIC  X(41)          VALUE SPACES.
           12  FILLER              PIC  X(5)           VALUE 'PAGE'.
           12  HD-PAGE             PIC ZZ,ZZZ.
           12  FILLER              PIC  X(9)           VALUE SPACES.

       01  HEAD-LINE-4.
           12  FILLER              PIC  X(51)          VALUE SPACES.
           12  FILLER              PIC  X(22)          VALUE
                   'CREDIT           DEBIT'.

       01  HEAD-LINE-5.
           12  FILLER              PIC  X(44)          VALUE
                   '    CAR  GROUP    FIN. RESP.       ACCOUNT  '.
           12  FILLER              PIC  X(34)          VALUE
                   '  ACCTS RECEIVABLE   CASH CLEARING'.

       01  DETAIL-LINE.
           12  FILLER              PIC  X(5).
           12  DET-CARR            PIC  X.
           12  FILLER              PIC  X(3).
           12  DET-COMP            PIC  X(6).
           12  DET-DESC.
               16  FILLER          PIC  X(3).
               16  DET-RESP        PIC  X(10).
               16  FILLER          PIC  XX.
               16  DET-FLD-REP     PIC  X.
               16  FILLER          PIC  XX.
               16  DET-ACCT        PIC  X(10).
           12  FILLER              PIC  X(3).
           12  DET-AMOUNT-CR       PIC ZZZ,ZZZ,ZZZ.99.
           12  DET-MINUS1          PIC  X.
           12  FILLER              PIC  XX.
           12  DET-AMOUNT-DB       PIC ZZZ,ZZZ,ZZZ.99.
           12  DET-MINUS2          PIC  X.
           12  FILLER              PIC  X(54).

       01  DETAIL-GL-LINE.
           12  FILLER              PIC  X(20)          VALUE SPACES.
           12  FILLER              PIC  X(18)          VALUE
                   '**********      **'.
           12  DET-AR-GLNO         PIC  X(7).
           12  FILLER              PIC  X(10)          VALUE
                   '**      **'.
           12  DET-CC-GLNO         PIC  X(7).
           12  FILLER              PIC  XX             VALUE '**'.
           12  FILLER              PIC  X(68)          VALUE SPACES.
       EJECT
           COPY ELCDATE.

           EJECT
           COPY ELCDTECX.

           COPY ELCDTEVR.

           EJECT
       PROCEDURE DIVISION.

       0000-BEGIN-DATE.
                                       COPY ELCDTERX.

           PERFORM 1000-INITIALIZE-ROUTINE
                                       THRU 1000-EXIT

           PERFORM 2000-PROCESS-LOOP   THRU 2000-EXIT UNTIL
                 THERE-ARE-NO-MORE-RECORDS

           PERFORM 3000-FINISH         THRU 3000-EXIT

           MOVE ZEROS                  TO RETURN-CODE
           GOBACK

           .
       0000-EXIT.
            EXIT.

       1000-INITIALIZE-ROUTINE.

           MOVE +0                     TO WS-GRAND-CREDITS
                                          WS-GRAND-DEBITS
                                          WS-CARR-CREDITS
                                          WS-CARR-DEBITS

           MOVE BIN-RUN-DATE           TO WS-CURRENT-DT

           MOVE ALPH-DATE              TO  HD-DATE
           MOVE WS-CURRENT-DATE        TO  HD-RUN
           MOVE COMPANY-NAME           TO  HD-CLIENT

           OPEN INPUT  COMP-IN
                OUTPUT PYAJ-OUT PRINT-FILE

           PERFORM 1010-READ-ERCOMP    THRU 1010-EXIT
           MOVE CO-CARRIER             TO WS-SAVE-CARRIER

           .
       1000-EXIT.
            EXIT.

       1010-READ-ERCOMP.

           READ COMP-IN AT END
              SET THERE-ARE-NO-MORE-RECORDS TO TRUE
           END-READ

           IF NOT THERE-ARE-NO-MORE-RECORDS
              ADD 1 TO WS-COMP-READ-CNT
           END-IF

           .
       1010-EXIT.
            EXIT.

       2000-PROCESS-LOOP.

           IF CO-CUR-OVR-UNDR NOT = ZERO
              IF WS-SAVE-CARRIER NOT = CO-CARRIER
                 MOVE SPACES           TO DETAIL-LINE
                 MOVE 'CARRIER TOTALS '
                                       TO DET-DESC
                 MOVE WS-CARR-CREDITS  TO DET-AMOUNT-CR
                 MOVE WS-CARR-DEBITS   TO DET-AMOUNT-DB
                 IF WS-CARR-CREDITS NEGATIVE
                    MOVE '-'           TO DET-MINUS1
                 END-IF
                 IF WS-CARR-DEBITS NEGATIVE
                    MOVE '-'           TO DET-MINUS2
                 END-IF
                 MOVE '-'              TO X
                 MOVE DETAIL-LINE      TO P-DATA
                 PERFORM 3200-PRINT-ROUTINE
                                       THRU 3299-EXIT
                 MOVE CO-CARRIER       TO WS-SAVE-CARRIER
                 COMPUTE WS-GRAND-CREDITS =
                         WS-GRAND-CREDITS + WS-CARR-CREDITS
                 COMPUTE WS-GRAND-DEBITS  =
                         WS-GRAND-DEBITS  + WS-CARR-DEBITS
                 MOVE +0               TO WS-CARR-CREDITS
                                          WS-CARR-DEBITS
              END-IF
              MOVE SPACES              TO DETAIL-LINE
              MOVE CO-CARRIER          TO DET-CARR
              MOVE CO-GROUPING         TO DET-COMP
              MOVE CO-RESP-NO          TO DET-RESP
              MOVE CO-ACCOUNT          TO DET-ACCT

              COMPUTE WS-CARR-CREDITS =
                      WS-CARR-CREDITS + CO-CUR-OVR-UNDR
              COMPUTE WS-CARR-DEBITS  =
                      WS-CARR-DEBITS  + CO-CUR-OVR-UNDR
              MOVE CO-CUR-OVR-UNDR     TO DET-AMOUNT-CR
                                          DET-AMOUNT-DB
              IF CO-CUR-OVR-UNDR NEGATIVE
                  MOVE '-'             TO DET-MINUS1
                                          DET-MINUS2
              END-IF

              MOVE ' '                 TO X
              MOVE DETAIL-LINE         TO P-DATA

              PERFORM 3200-PRINT-ROUTINE
                                       THRU  3299-EXIT
           END-IF

           PERFORM 1010-READ-ERCOMP    THRU 1010-EXIT

           .
       2000-EXIT.
           EXIT.
           EJECT
       3000-FINISH.

           MOVE SPACES                 TO DETAIL-LINE
           MOVE 'CARRIER TOTALS '      TO DET-DESC
           MOVE WS-CARR-CREDITS        TO DET-AMOUNT-CR
           MOVE WS-CARR-DEBITS         TO DET-AMOUNT-DB
           IF WS-CARR-CREDITS NEGATIVE
              MOVE '-'                 TO DET-MINUS1
           END-IF
           IF WS-CARR-DEBITS NEGATIVE
              MOVE '-'                 TO DET-MINUS2
           END-IF
           MOVE '-'                    TO X
           MOVE DETAIL-LINE            TO P-DATA
           PERFORM 3200-PRINT-ROUTINE
                                       THRU 3299-EXIT
           COMPUTE WS-GRAND-CREDITS =
                   WS-GRAND-CREDITS + WS-CARR-CREDITS
           COMPUTE WS-GRAND-DEBITS =
                   WS-GRAND-DEBITS + WS-CARR-DEBITS

           MOVE SPACES                 TO DETAIL-LINE
           MOVE 'GRAND   TOTALS '      TO DET-DESC
           MOVE WS-GRAND-CREDITS       TO DET-AMOUNT-CR
           MOVE WS-GRAND-DEBITS        TO DET-AMOUNT-DB
           IF WS-GRAND-CREDITS NEGATIVE
              MOVE '-'                 TO DET-MINUS1
           END-IF
           IF WS-GRAND-DEBITS NEGATIVE
              MOVE '-'                 TO DET-MINUS2
           END-IF
           MOVE '-'                    TO X
           MOVE DETAIL-LINE            TO P-DATA
           PERFORM 3200-PRINT-ROUTINE
                                       THRU 3299-EXIT

           DISPLAY 'COMP RECORDS READ      ' WS-COMP-READ-CNT
           DISPLAY 'ERPYAJ RECORDS WRITTEN ' WS-PYAJ-OUT-CNT

                                   COPY ELCPRTC.

           CLOSE COMP-IN
                 PYAJ-OUT
                 PRINT-FILE

           .
       3000-EXIT.
           EXIT.
           EJECT

       3100-HEADING-ROUTINE.

           MOVE ZEROS                  TO LINE-CNT

           ADD 1                       TO PAGE-CNT

           MOVE '1'                    TO X
           MOVE PAGE-CNT               TO HD-PAGE
           MOVE HEAD-LINE-1            TO P-DATA
           PERFORM 3200-PRINT-ROUTINE  THRU 3299-EXIT

           MOVE ' '                    TO X
           MOVE HEAD-LINE-2            TO P-DATA
           PERFORM 3200-PRINT-ROUTINE  THRU 3299-EXIT

           MOVE ' '                    TO X
           MOVE HEAD-LINE-3            TO P-DATA
           PERFORM 3200-PRINT-ROUTINE  THRU 3299-EXIT

           MOVE '-'                    TO X
           MOVE HEAD-LINE-4            TO P-DATA
           PERFORM 3200-PRINT-ROUTINE  THRU 3299-EXIT

           MOVE ' '                    TO X
           MOVE HEAD-LINE-5            TO P-DATA
           PERFORM 3200-PRINT-ROUTINE  THRU 3299-EXIT

           MOVE '0'                    TO X
           MOVE SPACES                 TO P-DATA
           PERFORM 3200-PRINT-ROUTINE  THRU 3299-EXIT

           .
       3199-XIT.
           EXIT.

       3200-PRINT-ROUTINE.

           IF X = ' '
              ADD +1                   TO LINE-CNT
           ELSE
              IF X = '0'
                 ADD +2                TO LINE-CNT
              ELSE
                 IF X = '-'
                    ADD +3             TO LINE-CNT
                 ELSE
                    MOVE +0            TO LINE-CNT
                 END-IF
              END-IF
           END-IF

           IF LINE-CNT GREATER +52
               PERFORM 3100-HEADING-ROUTINE
                                       THRU  3199-XIT
           END-IF

                                   COPY ELCPRT2.

           .
       3299-EXIT.
           EXIT.

       6000-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA.

           IF DC-ERROR-CODE NOT = SPACE
               MOVE ZEROS              TO DC-CONVERSION-DATES
           END-IF

           .
       6000-EXIT.
           EXIT.
           EJECT
      *----------------------------------------------------------------
       ABEND-PGM SECTION. COPY ELCABEND.
      *----------------------------------------------------------------
