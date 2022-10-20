       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMAMC1.
       AUTHOR.     PABLO.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ACCTIN           ASSIGN TO SYS010.
           SELECT ACCTXRF          ASSIGN TO SYS011.
           SELECT ACCTOT           ASSIGN TO SYS012.

           EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  ACCTIN.

       01  ACCT-RECORD-IN              PIC X(2000).

       FD  ACCTXRF
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
      *  LRECL = 26
       01  ACCT-RECORD.
           05  ACCT-KEY.
               10  ACCT-PRIME-KEY  PIC X(19).
               10  ACCT-CLP-STATE  PIC XX.
           05  ACCT-CNTR           PIC 9(5).

       FD  ACCTOT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  ERACCT-OUT-REC              PIC X(2000).

           EJECT

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMAMC1  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-INPUT              VALUE 'Y'.
       77  WS-XRF-SW               PIC X VALUE SPACES.
           88  END-OF-XRF                VALUE 'Y'.
       77  WS-RECS-IN              PIC 9(9) VALUE ZEROS.
       77  WS-RECS-OUT             PIC 9(9) VALUE ZEROS.
       77  WS-XRF-IN               PIC 9(9) VALUE ZEROS.
       77  S1                      PIC S9(5) VALUE +0 COMP-3.
       77  MAX-S1                  PIC S9(5) VALUE +0 COMP-3.
       77  WS-MATCHING-KEY         PIC X(19) VALUE LOW-VALUES.
       77  WS-DISPLAY-NUM          PIC ZZZ99 VALUE ZEROS.
       77  WS-SAVE-OUT-KEY         PIC X(25) VAlUE LOW-VALUES.
      /


      ******************************************************************
       01  WS-MISC.
           05  WS-ST-TABLE.
               10  WS-TBL-STATE OCCURS 50
                                       PIC XX.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.

                                       COPY ERCACCT.
                                       COPY ELCDATE.

           EJECT
       PROCEDURE DIVISION.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-INPUT  THRU 0100-EXIT UNTIL
                 (END-OF-INPUT)
PEMTST*          OR (WS-RECS-IN > 1000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' ACCT RECORDS READ    '  WS-RECS-IN 
           DISPLAY ' XRF  RECORDS READ    '  WS-XRF-IN 
           DISPLAY ' ACCT RECORDS WRITTEN '  WS-RECS-OUT 
           GOBACK
           .

       0100-PROCESS-INPUT. 

           IF AM-CONTROL-A < ACCT-PRIME-KEY
              PERFORM 0300-WRITE-OUTPUT
                                       THRU 0300-EXIT
              PERFORM 0200-READ-INPUT  THRU 0200-EXIT
           ELSE
              IF AM-CONTROL-A > ACCT-PRIME-KEY
                 DISPLAY ' NO MATCH ' ACCT-KEY
                 PERFORM 0250-READ-XRF THRU 0250-EXIT
              ELSE
      *  THEY HAVE TO BE = AT THIS POINT
                 DISPLAY 'WE HAVE A WINNER ' ACCT-KEY '  '
                 AM-CONTROL-A
                 PERFORM 0110-MATCH    THRU 0110-EXIT
              END-IF
           END-IF

           .
       0100-EXIT.
           EXIT.

       0110-MATCH.

           MOVE SPACES                 TO WS-ST-TABLE
           MOVE ACCT-PRIME-KEY         TO WS-MATCHING-KEY
           
           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              (S1 > +50)
              OR (WS-MATCHING-KEY NOT = ACCT-PRIME-KEY)
              OR (END-OF-XRF)
              MOVE ACCT-CLP-STATE      TO WS-TBL-STATE (S1)
              PERFORM 0250-READ-XRF    THRU 0250-EXIT
           END-PERFORM

           IF S1 > +0
              SUBTRACT +1 FROM S1
           END-IF
           
           IF S1 > +50
              DISPLAY ' ACCT XREF TABLE BLOWN '
              DISPLAY ' INCREASE AND RESTART JOB '
              DIVIDE S1 BY +0 GIVING S1
           ELSE
              IF END-OF-XRF
                 DISPLAY ' FOUND EOF ON XRF ' ACCT-KEY '  '
                 AM-CONTROL-A '  ' WS-MATCHING-KEY
                 MOVE ALL 'Z'          TO ACCT-PRIME-KEY
              END-IF
              MOVE S1                  TO MAX-S1
           END-IF

           MOVE S1                     TO WS-DISPLAY-NUM
           DISPLAY ' S1 IS ' WS-DISPLAY-NUM

           PERFORM UNTIL
              (WS-MATCHING-KEY NOT = AM-CONTROL-A)
              PERFORM 0300-WRITE-OUTPUT
                                       THRU 0300-EXIT

              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 (S1 > MAX-S1)
                 OR (S1 > +50)
                 MOVE WS-TBL-STATE (S1)
                                       TO AM-STATE
                                          AM-VG-STATE
                 PERFORM 0300-WRITE-OUTPUT
                                       THRU 0300-EXIT
              END-PERFORM
              PERFORM 0200-READ-INPUT  THRU 0200-EXIT
           END-PERFORM
                                          
           .
       0110-EXIT.
           EXIT.
       0200-READ-INPUT. 

           READ ACCTIN INTO ACCOUNT-MASTER AT END
              SET END-OF-INPUT TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1 TO WS-RECS-IN 
           END-IF

           .
       0200-EXIT.
           EXIT.

       0250-READ-XRF. 

           READ ACCTXRF AT END
              MOVE ALL 'Z'             TO ACCT-PRIME-KEY
              SET END-OF-XRF TO TRUE
           END-READ

           IF NOT END-OF-XRF
              ADD 1 TO WS-XRF-IN 
           END-IF

           .
       0250-EXIT.
           EXIT.

       0300-WRITE-OUTPUT.

           DISPLAY ' WRITING     ' AM-CONTROL-A
           IF AM-MSTR-CNTRL = WS-SAVE-OUT-KEY
              DISPLAY ' DROPPING DUPLICATE ' AM-CONTROL-A
           ELSE
              MOVE AM-MSTR-CNTRL       TO WS-SAVE-OUT-KEY
              WRITE ERACCT-OUT-REC     FROM ACCOUNT-MASTER
              ADD 1 TO WS-RECS-OUT
           END-IF

           .
       0300-EXIT.
           EXIT.


       0400-OPEN-FILES.

           OPEN INPUT ACCTIN ACCTXRF
               OUTPUT ACCTOT

           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ACCTIN ACCTOT ACCTXRF

           .

       0500-EXIT.
           EXIT.


       0600-INITIALIZE.

           MOVE SPACES                 TO WS-ST-TABLE
           PERFORM 0200-READ-INPUT     THRU 0200-EXIT
           PERFORM 0250-READ-XRF       THRU 0250-EXIT

           .

       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.

