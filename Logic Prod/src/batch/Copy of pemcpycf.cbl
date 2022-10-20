       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMCPYCF.
       AUTHOR.     PABLO.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELCNTL           ASSIGN TO ELCNTL
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CF-CONTROL-PRIMARY
                                   FILE STATUS IS ELCNTL-FILE-STATUS.

           SELECT CNTL-OUT         ASSIGN TO CNTLOUT.

           EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  ELCNTL.

           COPY ELCCNTL.
      /

       FD  CNTL-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  CNTL-OUT-REC                PIC X(0750).



           EJECT

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMCPYCF WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  THERE-ARE-NO-MORE-RECORDS VALUE 'Y'.
           88  THERE-ARE-MORE-RECORDS    VALUE ' '.
       77  CTL-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  CTL-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  IN-SUB                  PIC S9(5) VALUE +0 COMP-3.
       77  OT-SUB                  PIC S9(5) VALUE +0 COMP-3.
       77  MAX-SUB                 PIC S9(5) VALUE +0 COMP-3.
      /
       01  WS-IN-TABLE-AREA.
           05  WS-IN-RECORD OCCURS 500.
               10  WS-IN-BENEFIT           PIC XX.
               10  FILLER                  PIC X(051).
       01  WS-OT-TABLE-AREA.
           05  WS-OT-RECORD OCCURS 500.
               10  WS-OT-BENEFIT           PIC XX.
               10  FILLER                  PIC X(051).
      /
       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

       01  WORK-ABEND-CODE.
           12  WAC-1                   PIC X.
           12  WAC-2                   PIC X.
           12  WAC-3-4.
               16  WAC-3               PIC X.
               16  WAC-4               PIC X.

       01  WS-MISC.
           05  WS-SAVE-ELCNTL          PIC X(0750) VALUE LOW-VALUES.
           05  ELCNTL-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.

                                       COPY ELCDATE.

           EJECT
       PROCEDURE DIVISION.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-CNTL   THRU 0100-EXIT UNTIL
                 THERE-ARE-NO-MORE-RECORDS

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' CNTL RECORDS READ    '  CTL-RECS-IN
           DISPLAY ' CNTL RECORDS WRITTEN '  CTL-RECS-OUT
           GOBACK
           .

       0100-PROCESS-CNTL.

           IF (CF-RECORD-TYPE = '4') and
              (cf-company-id = 'CID')
              PERFORM 0110-PROCESS-LF  THRU 0110-EXIT
              MOVE HIGH-VALUES            TO WS-IN-TABLE-AREA
                                             WS-OT-TABLE-AREA
              move +0 to in-sub ot-sub max-sub
           END-IF

           IF (CF-RECORD-TYPE = '5') and
              (cf-company-id = 'CID')
              PERFORM 0150-PROCESS-AH  THRU 0150-EXIT
           END-IF

           PERFORM 0300-WRITE-CNTL     THRU 0300-EXIT
           PERFORM 0200-READ-CNTL      THRU 0200-EXIT

      *    IF CTL-RECS-IN > 200
      *       SET THERE-ARE-NO-MORE-RECORDS TO TRUE
      *    END-IF

           .

       0100-EXIT.
           EXIT.

       0110-PROCESS-LF.

           MOVE +1                     TO OT-SUB
           DISPLAY ' BEGIN BUILD LF '
           PERFORM 0120-BUILD-LF       THRU 0120-EXIT UNTIL
              (CF-RECORD-TYPE NOT = '4')

           MOVE CONTROL-FILE           TO WS-SAVE-ELCNTL
           MOVE OT-SUB                 TO MAX-SUB
           MOVE +1                     TO OT-SUB
           DISPLAY ' BEGIN SORT  LF '
           PERFORM 0130-SORT-LF        THRU 0130-EXIT UNTIL
              (OT-SUB > MAX-SUB)

           MOVE +1                     TO IN-SUB
           DISPLAY ' BEGIN UNLOAD LF '
           PERFORM 0140-UNLOAD-LF      THRU 0140-EXIT UNTIL
              (IN-SUB > MAX-SUB) OR
              (WS-OT-BENEFIT (IN-SUB) = HIGH-VALUES)

           MOVE WS-SAVE-ELCNTL         TO CONTROL-FILE

           DISPLAY ' FINISH UNLOAD LF '
           .
       0110-EXIT.
           EXIT.
       0120-BUILD-LF.

           MOVE +1                     TO IN-SUB
           PERFORM VARYING IN-SUB FROM +1 BY +1 UNTIL
               (IN-SUB > +8)
               IF CF-BENEFIT-CODE (IN-SUB) NOT = SPACES AND ZEROS
                  MOVE CF-BENEFIT-CONTROLS (IN-SUB)
                                       TO WS-IN-RECORD (OT-SUB)
                  ADD +1               TO OT-SUB
               END-IF
               IF IN-SUB > +500
                  DISPLAY ' LF SUB BLOWN '
                  PERFORM ABEND-PGM
               END-IF
           END-PERFORM

           PERFORM 0200-READ-CNTL      THRU 0200-EXIT

           .
       0120-EXIT.
           EXIT.
       0130-SORT-LF.

           MOVE +1                   TO IN-SUB
           PERFORM VARYING IN-SUB FROM +1 BY +1 UNTIL
              (IN-SUB > MAX-SUB) OR
              (IN-SUB > +500)
              IF WS-IN-BENEFIT (IN-SUB) < WS-OT-BENEFIT (OT-SUB)
                 MOVE WS-IN-RECORD (IN-SUB)
                                     TO WS-OT-RECORD (OT-SUB)
              END-IF
           END-PERFORM

           DISPLAY ' OT SUB DURING SORT ' OT-SUB
           MOVE +1                   TO IN-SUB
           PERFORM VARYING IN-SUB FROM +1 BY +1 UNTIL
              (WS-IN-BENEFIT (IN-SUB) = WS-OT-BENEFIT (OT-SUB)) OR
              (IN-SUB > MAX-SUB)
           END-PERFORM
           MOVE HIGH-VALUES           TO WS-IN-RECORD (IN-SUB)
           ADD +1          TO OT-SUB


           .
       0130-EXIT.
           EXIT.
       0140-UNLOAD-LF.

           MOVE SPACES                 TO CONTROL-FILE
           MOVE +1          TO OT-SUB
           PERFORM VARYING OT-SUB FROM +1 BY +1 UNTIL
              (OT-SUB > +8) OR
              (WS-OT-BENEFIT (IN-SUB) = HIGH-VALUES)
              IF WS-OT-BENEFIT (IN-SUB) NOT = ZEROS AND SPACES
                 MOVE WS-OT-RECORD (IN-SUB)
                                       TO CF-BENEFIT-CONTROLS (OT-SUB)
                 ADD +1                TO IN-SUB
              END-IF
           END-PERFORM
           MOVE 'CF'                   TO CF-RECORD-ID
           MOVE 'CID'                  TO CF-COMPANY-ID
           MOVE '4'                    TO CF-RECORD-TYPE
           MOVE WS-OT-BENEFIT (IN-SUB - +1)
                                       TO CF-HI-BEN-IN-REC
           MOVE 'PEMA'                 TO CF-LAST-MAINT-BY
           MOVE +170000                TO CF-LAST-MAINT-HHMMSS
           MOVE X'983B'                TO CF-LAST-MAINT-DT
           perform varying ot-sub from +1 by +1 until
              (ot-sub > +8)
              if cf-benefit-code (ot-sub) = spaces
                 move zeros            to cf-benefit-code (ot-sub)
              end-if
           end-perform


           PERFORM 0300-WRITE-CNTL     THRU 0300-EXIT

           .
       0140-EXIT.
           EXIT.

       0150-PROCESS-ah.

           MOVE +1                     TO OT-SUB
           DISPLAY ' BEGIN BUILD ah '
           PERFORM 0160-BUILD-ah       THRU 0160-EXIT UNTIL
              (CF-RECORD-TYPE NOT = '5')

           MOVE CONTROL-FILE           TO WS-SAVE-ELCNTL
           MOVE OT-SUB                 TO MAX-SUB
           MOVE +1                     TO OT-SUB
           DISPLAY ' BEGIN SORT  ah '
           PERFORM 0170-SORT-ah        THRU 0170-EXIT UNTIL
              (OT-SUB > MAX-SUB)

           MOVE +1                     TO IN-SUB
           DISPLAY ' BEGIN UNLOAD ah '
           PERFORM 0180-UNLOAD-ah      THRU 0180-EXIT UNTIL
              (IN-SUB > MAX-SUB) OR
              (WS-OT-BENEFIT (IN-SUB) = HIGH-VALUES)

           MOVE WS-SAVE-ELCNTL         TO CONTROL-FILE

           DISPLAY ' FINISH UNLOAD ah '
           .
       0150-EXIT.
           EXIT.
       0160-BUILD-ah.

           MOVE +1                     TO IN-SUB
           PERFORM VARYING IN-SUB FROM +1 BY +1 UNTIL
               (IN-SUB > +8)
               IF CF-BENEFIT-CODE (IN-SUB) NOT = SPACES AND ZEROS
                  MOVE CF-BENEFIT-CONTROLS (IN-SUB)
                                       TO WS-IN-RECORD (OT-SUB)
                  ADD +1               TO OT-SUB
               END-IF
               IF IN-SUB > +500
                  DISPLAY ' ah SUB BLOWN '
                  PERFORM ABEND-PGM
               END-IF
           END-PERFORM

           PERFORM 0200-READ-CNTL      THRU 0200-EXIT

           .
       0160-EXIT.
           EXIT.
       0170-SORT-ah.

           MOVE +1                   TO IN-SUB
           PERFORM VARYING IN-SUB FROM +1 BY +1 UNTIL
              (IN-SUB > MAX-SUB) OR
              (IN-SUB > +500)
              IF WS-IN-BENEFIT (IN-SUB) < WS-OT-BENEFIT (OT-SUB)
                 MOVE WS-IN-RECORD (IN-SUB)
                                     TO WS-OT-RECORD (OT-SUB)
              END-IF
           END-PERFORM

           DISPLAY ' OT SUB DURING SORT ' OT-SUB
           MOVE +1                   TO IN-SUB
           PERFORM VARYING IN-SUB FROM +1 BY +1 UNTIL
              (WS-IN-BENEFIT (IN-SUB) = WS-OT-BENEFIT (OT-SUB)) OR
              (IN-SUB > MAX-SUB)
           END-PERFORM
           MOVE HIGH-VALUES           TO WS-IN-RECORD (IN-SUB)
           ADD +1          TO OT-SUB


           .
       0170-EXIT.
           EXIT.
       0180-UNLOAD-ah.

           MOVE SPACES                 TO CONTROL-FILE
           MOVE +1          TO OT-SUB
           PERFORM VARYING OT-SUB FROM +1 BY +1 UNTIL
              (OT-SUB > +8) OR
              (WS-OT-BENEFIT (IN-SUB) = HIGH-VALUES)
              IF WS-OT-BENEFIT (IN-SUB) NOT = ZEROS AND SPACES
                 MOVE WS-OT-RECORD (IN-SUB)
                                       TO CF-BENEFIT-CONTROLS (OT-SUB)
                 ADD +1                TO IN-SUB
              END-IF
           END-PERFORM
           MOVE 'CF'                   TO CF-RECORD-ID
           MOVE 'CID'                  TO CF-COMPANY-ID
           MOVE '5'                    TO CF-RECORD-TYPE
           MOVE WS-OT-BENEFIT (IN-SUB - +1)
                                       TO CF-HI-BEN-IN-REC
           MOVE 'PEMA'                 TO CF-LAST-MAINT-BY
           MOVE +170000                TO CF-LAST-MAINT-HHMMSS
           MOVE X'983B'                TO CF-LAST-MAINT-DT
           perform varying ot-sub from +1 by +1 until
              (ot-sub > +8)
              if cf-benefit-code (ot-sub) = spaces
                 move zeros            to cf-benefit-code (ot-sub)
              end-if
           end-perform

           PERFORM 0300-WRITE-CNTL     THRU 0300-EXIT

           .
       0180-EXIT.
           EXIT.


       0200-READ-CNTL.

           READ ELCNTL NEXT RECORD

           IF ELCNTL-FILE-STATUS = '10' OR '23'
              SET THERE-ARE-NO-MORE-RECORDS TO TRUE
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 DISPLAY 'ELCNTL READ NEXT ' ELCNTL-FILE-STATUS
                 SET THERE-ARE-NO-MORE-RECORDS TO TRUE
              END-IF
           END-IF

           IF THERE-ARE-MORE-RECORDS
              ADD 1 TO CTL-RECS-IN
           END-IF

           .

       0200-EXIT.
           EXIT.

       0300-WRITE-CNTL.

           WRITE CNTL-OUT-REC FROM CONTROL-FILE
           ADD 1 TO CTL-RECS-OUT

           .

       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT ELCNTL
               OUTPUT CNTL-OUT

           IF ELCNTL-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY 'ELCNTL OPEN      ' ELCNTL-FILE-STATUS
              SET THERE-ARE-NO-MORE-RECORDS TO TRUE
           END-IF

           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ELCNTL CNTL-OUT

           .

       0500-EXIT.
           EXIT.

       0550-START-CNTL.

           MOVE LOW-VALUES             TO CF-CONTROL-PRIMARY
           MOVE 'CID'                  TO CF-COMPANY-ID

           START ELCNTL KEY IS NOT < CF-CONTROL-PRIMARY

           IF ELCNTL-FILE-STATUS = '10' OR '23'
              SET THERE-ARE-NO-MORE-RECORDS TO TRUE
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 DISPLAY 'ELCNTL START     ' ELCNTL-FILE-STATUS
                 SET THERE-ARE-NO-MORE-RECORDS TO TRUE
              END-IF
           END-IF

           .

       0550-EXIT.
           EXIT.

       0600-INITIALIZE.

           MOVE HIGH-VALUES            TO WS-IN-TABLE-AREA
                                          WS-OT-TABLE-AREA
           PERFORM 0550-START-CNTL     THRU 0550-EXIT
           PERFORM 0200-READ-CNTL      THRU 0200-EXIT

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
           EJECT

