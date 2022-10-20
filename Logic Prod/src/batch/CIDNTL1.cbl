       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDNTL1.
       AUTHOR.     PABLO.
       DATE-COMPILED.
      *REMARKS.
      *  THE INPUT FILE TO THIS PROGRAM IS ASSUMED TO BE
      *  SORTED BY ACCT KEY IN ASCENDING SEQ AND THE 
      *  SEQUENCE NUMBER IN DESCENDING SEQ.

070709******************************************************************
070709*                   C H A N G E   L O G
070709*
070709* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
070709*-----------------------------------------------------------------
070709*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
070709* EFFECTIVE    NUMBER
070709*-----------------------------------------------------------------
070709* 070709                   PEMA  NEW PROGRAM
070709******************************************************************       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT  FILE-IN             ASSIGN TO SYS010.
           SELECT  FILE-OUT            ASSIGN TO SYS011.
       
       DATA DIVISION.
       FILE SECTION.
       
       FD  FILE-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       
                                       COPY ERCACNT.
       
       FD  FILE-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       
       01  OUT-RECORD                  PIC X(120).
       
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMNTL1  WORKING-STORAGE     '.
       77  FILLER  PIC X(32) VALUE '*********** VMOD=2.001. ********'.
       
       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-INPUT                  VALUE 'Y'.
       77  RECS-IN                     PIC 9(9) VALUE ZEROS.
       77  RECS-OUT                    PIC 9(9) VALUE ZEROS.
       77  RECS-CVT                    PIC 9(9) VALUE ZEROS.
       77  RECS-DEL                    PIC 9(9) VALUE ZEROS.
       77  WS-SEQ-NO                   PIC S9(4) COMP VALUE +0.
       77  WS-PREV-KEY                 PIC X(20) VALUE SPACES.
       
       PROCEDURE DIVISION.
       
       0000-MAIN.
       
           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT
       
           PERFORM 0020-INITIALIZE     THRU 0020-EXIT
       
           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
              END-OF-INPUT
       
           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT
       
           DISPLAY ' RECORDS READ      ' RECS-IN
           DISPLAY ' RECORDS WRITTEN   ' RECS-OUT
           DISPLAY ' RECORDS CONVERTED ' RECS-CVT
           DISPLAY ' RECORDS DELETED   ' RECS-DEL
           GOBACK
       
            .
       0010-OPEN-FILES.
       
           OPEN INPUT  FILE-IN
                OUTPUT FILE-OUT
       
           .
       0010-EXIT.
           EXIT.
       
       0020-INITIALIZE.
       
           PERFORM 0200-READ-INPUT     THRU 0200-EXIT
           MOVE +4096                  TO WS-SEQ-NO
       
           .
       0020-EXIT.
           EXIT.

       0050-PROCESS-FILE.
       
           IF NT-RECORD-TYPE = '1'
              PERFORM 0100-PROCESS-FILE
                                       THRU 0100-EXIT
           END-IF

           IF NT-RECORD-TYPE NOT = '1' AND '2' AND '3'
              DISPLAY ' BOGUS RECORD ' NT-ACCT-NOTE-KEY ' '
                 NT-RECORD-TYPE
           ELSE
              PERFORM 0300-WRITE-OUTPUT
                                      THRU 0300-EXIT
           END-IF

           PERFORM 0200-READ-INPUT     THRU 0200-EXIT
       
           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-FILE.
       
           IF NT-CONTROL-PRIMARY (1:20) NOT = WS-PREV-KEY
              MOVE +4096               TO WS-SEQ-NO
              MOVE NT-CONTROL-PRIMARY (1:20)
                                       TO WS-PREV-KEY
           END-IF
      *    DISPLAY ' CHANGING KEY FROM ' NT-CARRIER ' ' NT-STATE ' '
      *       NT-ACCOUNT ' ' NT-LINE-SEQUENCE ' TO ' WS-SEQ-NO
           MOVE WS-SEQ-NO              TO NT-LINE-SEQUENCE
           SUBTRACT +1                 FROM WS-SEQ-NO       
       
           .
       0100-EXIT.
           EXIT.
       
       0200-READ-INPUT.
       
           READ FILE-IN AT END
              SET END-OF-INPUT         TO TRUE
           END-READ
       
           IF NOT END-OF-INPUT
              ADD 1                    TO RECS-IN
           END-IF
       
           .
       0200-EXIT.
           EXIT.
       
       0300-WRITE-OUTPUT.
       
           WRITE OUT-RECORD            FROM NOTE-FILE
           ADD 1                       TO RECS-OUT
       
           .
       0300-EXIT.
           EXIT.
       
       0500-CLOSE-FILES.
       
           CLOSE FILE-IN FILE-OUT
       
           .
       0500-EXIT.
           EXIT.

