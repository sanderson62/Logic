       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDRTX2.
       AUTHOR.     PABLO.
       DATE-COMPILED.
      *REMARKS.                                                         
      *        THIS PROGRAM CREATES A RATE FILE OF CURRENT RATES ONLY
      *    PRIMARILY USED IN THE CIEE DATABASE
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 022609 CR2009020500001   PEMA  NEW PROGRAM
032111* 032111 CR2011031700002   PEMA  ADD 24,36,48....120 RATES
021314* 021314 IR2014021100001   PEMA  add sort to pgm due to age rates
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERRATE           ASSIGN TO ERRATE
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS ERRATE-KEY
                                   FILE STATUS IS ERRATE-FILE-STATUS.

021314     SELECT  SORT-FILE       ASSIGN TO SORTWK1.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT RATE-OUT         ASSIGN TO RATEOUT
                 ORGANIZATION IS LINE SEQUENTIAL.

           EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  ERRATE.

       01  ERRATE-IN-RECORD.
           05  FILLER                  PIC XX.
           05  ERRATE-KEY.
               10  ERRATE-COMP-CD      PIC X.
               10  ERRATE-STATE        PIC XX.
               10  FILLER              PIC X(5).
               10  ERRATE-TYPE         PIC X.
               10  ERRATE-BEN-CODE     PIC XX.
               10  ERRATE-HI-AGE       PIC 99.
               10  FILLER              PIC X(9).
               10  ERRATE-EXP-DT       PIC 9(11) COMP-3.
           05  FILLER                  PIC X(1735).

021314 SD  SORT-FILE                            
021314     RECORDING MODE IS F                  
021314     RECORD CONTAINS 1765 CHARACTERS.
021314                                          
021314 01  SORT-REC.                            
021314     12  filler              pic xx.
021314     12  sort-key.
021314         16  sw-comp-cd      pic x.
021314         16  sw-state-cd     pic xx.
021314         16  sw-class-cd     pic xx.
021314         16  sw-dev-cd       pic xxx.
021314         16  sw-ben-type     pic x.
021314         16  sw-ben-cd       pic xx.
021314         16  sw-fill-1       pic xx.
021314         16  sw-hi-amt       pic 9(6).
021314         16  sw-fill-2       pic xxx.
021314         16  sw-exp-date     pic 9(11)  comp-3.
021314     12  rest-of-record      pic x(1735).

       FD  RATE-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  RATE-OUT-REC.
           12  OT-STATE                  PIC XX.
           12  OT-TAB1                   PIC X.
           12  OT-CLASS                  PIC XX.
           12  OT-TAB2                   PIC X.
           12  OT-DEV                    PIC XXX.
           12  OT-TAB3                   PIC X.
           12  OT-L-AH                   PIC X.
           12  OT-TAB4                   PIC X.
           12  OT-BEN-CODE               PIC XX.
           12  OT-TAB5                   PIC X.
           12  OT-HI-AMT                 PIC 9(9).
           12  OT-TAB6                   PIC X.
           12  OT-EXP-DATE               PIC X(10).
           12  OT-TAB7                   PIC X.
           12  OT-DISCOUNT-OPT           PIC X.
           12  OT-TAB8                   PIC X.
           12  OT-DISCOUNT-RATE          PIC 99.99999.
           12  OT-TAB9                   PIC X.
           12  OT-DIS-OB-RATE            PIC 99.99999.
           12  OT-TAB10                  PIC X.
           12  OT-MORT-CODE              PIC XXXX.
           12  OT-TAB11                  PIC X.
           12  FILLER OCCURS 10.
               16  OT-RATES              PIC 99.99999.
               16  OT-TABS               PIC X.
           12  OT-EOR                    PIC X.

       FD  DISK-DATE
                                   COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDRTX2 WORKING STORAGE      '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                    PIC X VALUE SPACES.
           88  END-OF-INPUT              VALUE 'Y'.
       77  RAT-RECS-IN                  PIC 9(9) VALUE ZEROS.
       77  RAT-RECS-OUT                 PIC 9(9) VALUE ZEROS.
021314 77  ws-rel-recs                  pic 9(9) value zeros.
021314 77  ws-ret-recs                  pic 9(9) value zeros.
       77  S1                           PIC S999  VALUE +0 COMP-3.
       77  S2                           PIC S999  VALUE +0 COMP-3.
       77  PGM-SUB                      PIC S999    COMP    VALUE +506.      

                                       COPY ERCRATE.

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
012904     12  WS-SAVE-RATEOUT-REC-INIT PIC X(160). 

                                   COPY ELCDTECX.

                                   COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

021314     SORT SORT-FILE ON ASCENDING KEY
021314                                 sw-comp-cd    
021314                                 sw-state-cd   
021314                                 sw-class-cd   
021314                                 sw-dev-cd     
021314                                 sw-ben-type   
021314                                 sw-ben-cd     
021314                                 sw-hi-amt     
021314                                 sw-exp-date   
021314        INPUT PROCEDURE
021314           0010-INPUT-ROUTINE    THRU 0010-EXIT
021314        OUTPUT PROCEDURE
021314           0030-output-routine   THRU 0030-EXIT
021314
021314     IF SORT-RETURN  IS NOT EQUAL TO  ZEROS
021314        MOVE '0101'              TO WS-RETURN-CODE
021314        MOVE 'BAD SORT RETURN CODE'
021314                                 TO WS-ABEND-MESSAGE
021314        GO TO abend-pgm
021314     end-if

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' RATE RECORDS READ    '  RAT-RECS-IN
           DISPLAY ' RATE RECORDS WRITTEN '  RAT-RECS-OUT
           display ' rate recs released   '  ws-rel-recs
           display ' rate recs returned   '  ws-ret-recs
           GOBACK

           .
021314 0010-input-routine.
021314
021314     PERFORM 0600-INITIALIZE     THRU 0600-EXIT
021314     PERFORM 0550-START-RATE     THRU 0550-EXIT
021314     PERFORM 0200-READ-RATE      THRU 0200-EXIT
021314
021314     perform 0020-process-input  thru 0020-exit until
021314        end-of-input
021314
021314     move ' '                    to ws-eof-sw
021314
021314     .
021314 0010-exit.
021314     exit.

       0020-process-input.

           IF ERRATE-STATE NOT = 'BB'
              move errate-in-record    to sort-rec
              move spaces              to sw-fill-1
                                          sw-fill-2
              release sort-rec
              add 1 to ws-rel-recs
           end-if
           PERFORM 0200-READ-RATE      THRU 0200-EXIT

           .
       0020-exit.
           exit.

021314 0030-output-routine.
021314
021314     perform 0060-return-recs    thru 0060-exit
021314     MOVE ERRATE-IN-RECORD       TO RATE-RECORD
021314     perform 0060-return-recs    thru 0060-exit
021314
021314     perform 0050-process-rate   thru 0050-exit until
021314        end-of-input
021314
021314     PERFORM 0100-PROCESS-RATE   THRU 0100-EXIT
021314
021314     .
021314 0030-exit.
           exit.

       0050-PROCESS-RATE.

           IF ERRATE-KEY (1:22) = RT-CONTROL-PRIMARY (1:22)
              MOVE ERRATE-IN-RECORD    TO RATE-RECORD
           ELSE
              PERFORM 0100-PROCESS-RATE
                                       THRU 0100-EXIT
              MOVE ERRATE-IN-RECORD    TO RATE-RECORD
           END-IF

           perform 0060-return-recs    thru 0060-exit

           .
       0050-EXIT.
           EXIT.

021314 0060-return-recs.
021314
021314     return sort-file into errate-in-record at end
021314        set end-of-input to true
021314
021314     if not end-of-input
021314        add 1 to ws-ret-recs
021314     end-if
021314
021314     .
021314 0060-exit.
021314     exit.


       0100-PROCESS-RATE.

           MOVE WS-SAVE-RATEOUT-REC-INIT TO RATE-OUT-REC

           MOVE RT-ST-CODE             TO OT-STATE
           MOVE RT-ST-CLASS            TO OT-CLASS
           MOVE RT-ST-DEV              TO OT-DEV
           MOVE RT-L-AH                TO OT-L-AH
           MOVE RT-LAH-NUM             TO OT-BEN-CODE
           MOVE RT-HIGH-AMT            TO OT-HI-AMT
           MOVE RT-EXPIRY-DATE         TO WS-DATE
           MOVE WS-DATE (8:2)          TO OT-EXP-DATE (1:2)
           MOVE '/'                    TO OT-EXP-DATE (3:1)
                                          OT-EXP-DATE (6:1)
           MOVE WS-DATE (10:2)         TO OT-EXP-DATE (4:2)
           MOVE WS-DATE (4:4)          TO OT-EXP-DATE (7:4)
           IF OT-EXP-DATE = '99/99/9999'
              MOVE '12/31/9999'        TO OT-EXP-DATE
           END-IF
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
              IF RT-L-AH = 'L'
                 MOVE RT-L-RATE (S1)      TO OT-RATES (S2)
              ELSE
                 MOVE RT-AH-RATE (S1)     TO OT-RATES (S2)
              END-IF
              ADD +1 TO S2
           END-PERFORM

           IF (OT-STATE NOT = '00' AND '  ')
      *       AND (OT-BEN-CODE (1:1) NOT = 'Q')
              PERFORM 0300-WRITE-RATE     THRU 0300-EXIT
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-READ-RATE.

           READ ERRATE NEXT RECORD

           IF (ERRATE-FILE-STATUS = '10' OR '23')
              OR (ERRATE-COMP-CD > DTE-CLASIC-COMPANY-CD)
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERRATE-FILE-STATUS NOT = '00'
                 DISPLAY 'ERRATE READ NEXT ' ERRATE-FILE-STATUS
                 SET END-OF-INPUT      TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-INPUT
              IF (ERRATE-TYPE = 'L')
                 AND (ERRATE-BEN-CODE (1:1) = 'Q')
                 AND (ERRATE-HI-AGE NOT = 44)
                 DISPLAY ' BYPASSING RATE ' ERRATE-KEY (2:21) ' '
                   ERRATE-EXP-DT                 
                 GO TO 0200-READ-RATE
              END-IF
           END-IF

           IF NOT END-OF-INPUT
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

           MOVE LOW-VALUES             TO ERRATE-KEY
           MOVE DTE-CLASIC-COMPANY-CD  TO ERRATE-COMP-CD

           START ERRATE KEY >= ERRATE-KEY

           IF (ERRATE-FILE-STATUS = '10' OR '23')
              OR (ERRATE-COMP-CD > DTE-CLASIC-COMPANY-CD)
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERRATE-FILE-STATUS NOT = '00'
                 DISPLAY 'ERRATE START     ' ERRATE-FILE-STATUS
                 SET END-OF-INPUT      TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.

       0600-INITIALIZE.

           INITIALIZE RATE-OUT-REC
           MOVE ';'                    TO OT-TAB1 
                                          OT-TAB2 
                                          OT-TAB3 
                                          OT-TAB4 
                                          OT-TAB5 
                                          OT-TAB6 
                                          OT-TAB7 
                                          OT-TAB8 
                                          OT-TAB9 
                                          OT-TAB10
                                          OT-TAB11

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +10
              MOVE ';'                 TO OT-TABS (S1)
           END-PERFORM

           MOVE 'E'                    TO OT-EOR
      
           MOVE RATE-OUT-REC           TO WS-SAVE-RATEOUT-REC-INIT

           .
       0600-EXIT.
           EXIT.
       ABEND-PGM.
                                   COPY ELCABEND.
