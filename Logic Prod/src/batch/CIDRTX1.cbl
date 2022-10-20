       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDRTX1.
       AUTHOR.     PABLO.
       DATE-COMPILED.
012904******************************************************************
012904*                   C H A N G E   L O G
012904*
012904* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
012904*-----------------------------------------------------------------
012904*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
012904* EFFECTIVE    NUMBER
012904*-----------------------------------------------------------------
012904* 012904                   SMVA  ADD TABS TO RATE OUT REC FILE
052704* 052704 IR2004052400001   SMVA  CHG DELIMITER FROM TAB TO ;  
042309* 042309 CR2008122900001   PEMA  ADD 12 AND 60 MONTH RATES
032111* 032111 CR2011031700002   PEMA  ADD 24,36,48....120 RATES
012904******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERRATE           ASSIGN TO ERRATE
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS RT-CONTROL-PRIMARY
                                   FILE STATUS IS ERRATE-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT RATE-OUT         ASSIGN TO RATEOUT
                 ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  ERRATE.

                                       COPY ERCRATE.

       FD  RATE-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  RATE-OUT-REC.
           12  OT-STATE                  PIC XX.
012904     12  OT-TAB1                   PIC X.
           12  OT-CLASS                  PIC XX.
012904     12  OT-TAB2                   PIC X.
           12  OT-DEV                    PIC XXX.
012904     12  OT-TAB3                   PIC X.
           12  OT-L-AH                   PIC X.
012904     12  OT-TAB4                   PIC X.
           12  OT-BEN-CODE               PIC XX.
012904     12  OT-TAB5                   PIC X.
           12  OT-HI-AGE                 PIC XX.
012904     12  OT-TAB6                   PIC X.
           12  OT-HI-AMT                 PIC 999999.
012904     12  OT-TAB7                   PIC X.
           12  OT-SEX                    PIC X.
012904     12  OT-TAB8                   PIC X.
           12  OT-EXP-DATE               PIC X(10).
012904     12  OT-TAB9                   PIC X.
           12  OT-MAX-AGE                PIC XX.
012904     12  OT-TAB10                  PIC X.
           12  OT-DISCOUNT-OPT           PIC X.
012904     12  OT-TAB11                  PIC X.
           12  OT-DISCOUNT-RATE          PIC -99.99999.
012904     12  OT-TAB12                  PIC X.
           12  OT-DIS-OB-RATE            PIC -99.99999.
042309     12  OT-TAB13                  PIC X.
           12  OT-MORT-CODE              PIC X(4).
           12  FILLER OCCURS 10.
               16  OT-TABS               PIC X.
               16  OT-RATES              PIC -99.99999.

       FD  DISK-DATE
                                   COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDRTX1 WORKING STORAGE      '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                    PIC X VALUE SPACES.
           88  THERE-ARE-NO-MORE-RECORDS VALUE 'Y'.
           88  THERE-ARE-MORE-RECORDS    VALUE ' '.
       77  RAT-RECS-IN                  PIC 9(9) VALUE ZEROS.
       77  RAT-RECS-OUT                 PIC 9(9) VALUE ZEROS.
       77  S1                           PIC S999 VALUE +0 COMP-3.
       77  S2                           PIC S999 VALUE +0 COMP-3.
       77  PGM-SUB                      PIC S999    COMP    VALUE +506.      

       01  WS-MISC.
           12  WS-RETURN-CODE           PIC S9(4)   COMP   VALUE +0.
           12  WS-ABEND-MESSAGE         PIC X(80)          VALUE SPACES.
           12  WS-ABEND-FILE-STATUS     PIC XX             VALUE ZEROS.
           12  WS-ZERO                  PIC S9      COMP-3 VALUE +0.
           12  WS-ABEND-CODE            PIC 9(4).
           12  ABEND-CODE  REDEFINES  WS-ABEND-CODE.
               16  ABEND-CODE-1         PIC XX.
               16  ABEND-CODE-2.
                   20  AC2-ONE          PIC X.
                   20  AC2-TWO          PIC X.
           12  ERRATE-FILE-STATUS       PIC XX    VALUE ZEROS.
           12  WS-DATE                  PIC 9(11) VALUE ZEROS.
031711     12  WS-SAVE-RATEOUT-REC-INIT PIC X(167). 

                                   COPY ELCDTECX.

                                   COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                   COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES THRU 0400-EXIT

           PERFORM 0600-INITIALIZE THRU 0600-EXIT

           PERFORM 0100-PROCESS-RATE THRU 0100-EXIT UNTIL
                 THERE-ARE-NO-MORE-RECORDS

           PERFORM 0500-CLOSE-FILES THRU 0500-EXIT

           DISPLAY ' RATE RECORDS READ    '  RAT-RECS-IN
           DISPLAY ' RATE RECORDS WRITTEN '  RAT-RECS-OUT
           GOBACK

           .
       0100-PROCESS-RATE.

012904     MOVE WS-SAVE-RATEOUT-REC-INIT TO RATE-OUT-REC

           MOVE RT-ST-CODE             TO OT-STATE
           MOVE RT-ST-CLASS            TO OT-CLASS
           MOVE RT-ST-DEV              TO OT-DEV
           MOVE RT-L-AH                TO OT-L-AH
           MOVE RT-LAH-NUM             TO OT-BEN-CODE
           MOVE RT-HIGH-AGE            TO OT-HI-AGE
           MOVE RT-HIGH-AMT            TO OT-HI-AMT
           MOVE RT-SEX                 TO OT-SEX
           MOVE RT-EXPIRY-DATE         TO WS-DATE
           MOVE WS-DATE (8:2)          TO OT-EXP-DATE (1:2)
           MOVE '/'                    TO OT-EXP-DATE (3:1)
                                          OT-EXP-DATE (6:1)
           MOVE WS-DATE (10:2)         TO OT-EXP-DATE (4:2)
           MOVE WS-DATE (4:4)          TO OT-EXP-DATE (7:4)
           IF OT-EXP-DATE = '99/99/9999'
              MOVE '12/31/9999'        TO OT-EXP-DATE
           END-IF
           MOVE RT-MAX-AGE             TO OT-MAX-AGE
           MOVE RT-DISCOUNT-OPTION     TO OT-DISCOUNT-OPT
           IF RT-DISCOUNT-RATE NOT NUMERIC
              MOVE +0                  TO RT-DISCOUNT-RATE
           END-IF
           IF RT-DISCOUNT-OB-RATE NOT NUMERIC
              MOVE +0                  TO RT-DISCOUNT-OB-RATE
           END-IF
           MOVE RT-DISCOUNT-RATE       TO OT-DISCOUNT-RATE
           MOVE RT-DISCOUNT-OB-RATE    TO OT-DIS-OB-RATE

           IF RT-L-AH = 'L'
              MOVE RT-LIFE-MORT-CODE   TO OT-MORT-CODE
           END-IF
           MOVE +1                     TO S2
           PERFORM VARYING S1 FROM +12 BY +12 UNTIL
              S1 > +120
042309        IF RT-L-AH = 'L'
042309           MOVE RT-L-RATE (S1)      TO OT-RATES (S2)
042309        ELSE
042309           MOVE RT-AH-RATE (S1)     TO OT-RATES (S2)
042309        END-IF
              ADD +1 TO S2
           END-PERFORM

PEMTST     PERFORM 0300-WRITE-RATE     THRU 0300-EXIT
           PERFORM 0200-READ-RATE      THRU 0200-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-RATE.

           READ ERRATE NEXT RECORD

           IF (ERRATE-FILE-STATUS = '10' OR '23')
              OR (RT-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
              SET THERE-ARE-NO-MORE-RECORDS TO TRUE
           ELSE
              IF ERRATE-FILE-STATUS NOT = '00'
                 DISPLAY 'ERRATE READ NEXT ' ERRATE-FILE-STATUS
                 SET THERE-ARE-NO-MORE-RECORDS TO TRUE
              END-IF
           END-IF

           IF THERE-ARE-MORE-RECORDS
              ADD +1 TO RAT-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-RATE.

           WRITE RATE-OUT-REC
           ADD +1 TO RAT-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT ERRATE
               OUTPUT RATE-OUT

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ERRATE RATE-OUT

           .
       0500-EXIT.
           EXIT.

       0550-START-RATE.

           MOVE LOW-VALUES             TO RT-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO RT-COMPANY-CD

           START ERRATE KEY IS NOT < RT-CONTROL-PRIMARY

           IF (ERRATE-FILE-STATUS = '10' OR '23')
              OR (RT-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
              SET THERE-ARE-NO-MORE-RECORDS TO TRUE
           ELSE
              IF ERRATE-FILE-STATUS NOT = '00'
                 DISPLAY 'ERRATE START     ' ERRATE-FILE-STATUS
                 SET THERE-ARE-NO-MORE-RECORDS TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.

       0600-INITIALIZE.

012904     INITIALIZE RATE-OUT-REC
042309     MOVE ';'                    TO OT-TAB1
052704                                    OT-TAB2
052704                                    OT-TAB3 
052704                                    OT-TAB4 
052704                                    OT-TAB5 
052704                                    OT-TAB6 
052704                                    OT-TAB7 
052704                                    OT-TAB8 
052704                                    OT-TAB9 
052704                                    OT-TAB10
052704                                    OT-TAB11
052704                                    OT-TAB12
042309                                    OT-TAB13
           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +10
              MOVE ';'                 TO OT-TABS (S1)
           END-PERFORM
012904
012904     MOVE RATE-OUT-REC TO WS-SAVE-RATEOUT-REC-INIT 

           PERFORM 0550-START-RATE THRU 0550-EXIT
           PERFORM 0200-READ-RATE THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.
       ABEND-PGM.
                                   COPY ELCABEND.
