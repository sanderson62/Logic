       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDNTU1.
       AUTHOR.     PABLO.
       DATE-COMPILED.
      *REMARKS.
070709******************************************************************
070709*                   C H A N G E   L O G
070709*
070709* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
070709*-----------------------------------------------------------------
070709*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
070709* EFFECTIVE    NUMBER
070709*-----------------------------------------------------------------
070709* 070709                   PEMA  NEW PROGRAM
032714* 032714    2013081500001  AJRA  FIX VENDOR TICKET ACCT NOTE
070709******************************************************************       

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILE-IN          ASSIGN TO SYS010
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PRNTR            ASSIGN TO SYS008.

           SELECT ERACCT           ASSIGN TO ERACCT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AM-CONTROL-PRIMARY
                                   FILE STATUS IS ERACCT-FILE-STATUS.

           SELECT ERACNT           ASSIGN TO ERACNT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS NT-CONTROL-PRIMARY
                                   FILE STATUS IS ERACNT-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  FILE-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FILE-IN-REC.
           05  IN-CARRIER              PIC X.
           05  IN-STATE                PIC XX.
           05  IN-ACCOUNT              PIC X(10).
           05  IN-USER-ID              PIC X(4).
           05  IN-LAST-MAINT-DT        PIC X(8).
           05  IN-COMMENT              PIC X(60).

       FD  ERACNT.

                                       COPY ERCACNT.

       FD  ERACCT.

                                       COPY ERCACCT.

       FD  PRNTR
                                       COPY ELCPRTFD.

       FD  DISK-DATE                   COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  ERACCT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ERACNT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  WS-PREV-ERACNT-KEY          PIC X(19)     VALUE LOW-VALUES.
       77  WS-ERACNT-RECS-ADD          PIC 9(9)      VALUE ZEROS.
       77  WS-CURRENT-BIN-DATE         PIC XX   VALUE LOW-VALUES.
       77  WS-DIS-EFF-DT               PIC X(10)  VALUE SPACES.
       77  WS-DIS-EXP-DT               PIC X(10)  VALUE SPACES.
       77  WS-SEQ-NO                   PIC S9(4)   COMP VALUE +0.
       77  A1                          PIC S999 COMP-3 VALUE +0.
       77  WS-PAGE                     PIC S999 COMP-3 VALUE +0.
       77  WS-LINE-COUNT               PIC S999 COMP-3 VALUE +60.
       77  WS-LINE-COUNT-MAX           PIC S999 COMP-3 VALUE +55.
       77  WS-EOF-SW                   PIC X   VALUE SPACES.
           88  END-OF-INPUT                VALUE 'Y'.
032714 77  WS-IND-SAVE-REC             PIC X   VALUE 'N'.
032714     88  SAVE-RECORD-FOUND           VALUE 'Y'.
032714 77  WS-TRIED-CID                PIC X   VALUE SPACES.
032714     88  ALREADY-TRIED-CID           VALUE 'X'.
032714 77  WS-ACCOUNT-FOUND            PIC X   VALUE SPACES.
032714     88  ACCOUNT-NOT-FOUND           VALUE 'X'.

       01  PRT-LINES.                                                  
           12  HDR-1.                                                  
               16  FILLER          PIC X(55)           VALUE '1'.
               16  HD-1A           PIC X(24)           VALUE
                       'ACCOUNT NOTE PAD UPDATE '.                       
               16  FILLER          PIC X(43)           VALUE SPACES.   
               16  FILLER          PIC X(7)            VALUE 'CIDNTU1'.
           12  HDR-2.                                                  
               16  FILLER          PIC X(52)           VALUE SPACES.   
               16  H2-COMPANY      PIC X(30).                          
               16  FILLER          PIC X(38)           VALUE SPACES.   
               16  H2-IPL          PIC X(10).                           
           12  HDR-3.                                                  
               16  FILLER          PIC X(58)           VALUE SPACES.   
               16  H3-DATE         PIC X(18).                          
               16  FILLER          PIC X(44)           VALUE SPACES.   
               16  FILLER          PIC X(5)            VALUE 'PAGE '.  
               16  H3-PAGE         PIC ZZ,ZZ9.                         

           12  HDR-4.
               16  FILLER          PIC X(90)  VALUE '0                  
      -        '         CARRIER    GROUPING     STATE      ACCOUNT     
      -        '    COMMENT'.

           12  DTL-1.                                                 
               16  FILLER              PIC X(32)  VALUE SPACES.
               16  H1-CARRIER          PIC X      VALUE SPACES.
               16  FILLER              PIC X(8)   VALUE SPACES.
               16  H1-GROUPING         PIC X(6)   VALUE SPACES.
               16  FILLER              PIC X(7)   VALUE SPACES.
               16  H1-STATE            PIC XX     VALUE SPACES.
               16  FILLER              PIC X(7)   VALUE SPACES.
               16  H1-ACCOUNT          PIC X(10)  VALUE SPACES.
               16  FILLER              PIC X(6)   VALUE SPACES.
               16  H1-COMMENT          PIC X(50)  VALUE SPACES.

       01  WS-IN-KEY.
           05  WS-IN-CARRIER           PIC X.
           05  WS-IN-GROUPING          PIC X(6).
           05  WS-IN-STATE             PIC XX.
           05  WS-IN-ACCOUNT           PIC X(10).

032714 01  WS-SAVE-NOTE.
032714     05  WS-SAVE-USER-ID         PIC X(4).
032714     05  WS-SAVE-LAST-MAINT-DT   PIC X(2).
032714     05  WS-SAVE-LAST-MAINT-TIME PIC S9(7) COMP-3.
032714     05  WS-SAVE-COMMENT         PIC X(60).
032714
       01  WS-WORK-FIELDS.
           05  PGM-SUB                 PIC S9(4)   VALUE +548.
           05  WS-RETURN-CODE          PIC S9(3)   VALUE ZERO.
           05  WS-ABEND-MESSAGE        PIC X(80)   VALUE SPACES.
           05  WS-ZERO                 PIC S9      VALUE ZERO.
           05  WS-ABEND-FILE-STATUS    PIC XX      VALUE ZERO.
           05  WS-HIGH-SEQ             PIC X         VALUE ' '.
               88  FOUND-HIGH-SEQ-NO                 VALUE 'Y'.
           05  WS-ABEND-FLD            PIC 9         VALUE ZEROS.
           05  WS-ZERO-FLD             PIC 9         VALUE ZEROS.
           05  WS-RECS-IN              PIC 9(9)      VALUE ZEROS.
           05  WS-DISPLAY-CNT          PIC Z,ZZZ,ZZ9 VALUE ZEROS.
           05  WS-CURRENT-AM-KEY       PIC X(19)     VALUE LOW-VALUES.


                                       COPY ELCFUNDT.
                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

       0000-LOAD-DATE-CARD.            COPY ELCDTERX.

       0000-MAINLINE.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT
           PERFORM 0020-INITIALIZE     THRU 0020-EXIT
           PERFORM 0500-PROCESS        THRU 0500-EXIT UNTIL
              END-OF-INPUT
           PERFORM 3000-CLOSE-FILES    THRU 3000-EXIT
           PERFORM 4000-FINAL-TOTALS   THRU 4000-EXIT
           GOBACK

           .
       0000-EXIT.
           EXIT.

       0010-OPEN-FILES.

PEMTST     OPEN INPUT FILE-IN ERACCT
      *         ERACNT
PEMTST     OPEN I-O   ERACNT
           OPEN OUTPUT PRNTR

           IF ERACNT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERACNT - OPEN ' ERACNT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERACCT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERACCT - OPEN ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0010-EXIT.
           EXIT.

       0020-INITIALIZE.

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           DISPLAY ' FUNCTION DATE CYMD ' WS-FN-DATE
           STRING WS-FN-MO '/' WS-FN-DA '/' WS-FN-CCYR
              DELIMITED BY SIZE        INTO H2-IPL
           END-STRING
           MOVE WS-FN-DATE             TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-CURRENT-BIN-DATE
           ELSE
              DISPLAY ' ERROR CONVERTING CURRENT DATE '
           END-IF

           MOVE COMPANY-NAME           TO H2-COMPANY
           MOVE ALPH-DATE              TO H3-DATE

           PERFORM 0200-READ-INPUT     THRU 0200-EXIT

           .
       0020-EXIT.
           EXIT.

       0200-READ-INPUT.
       
           READ FILE-IN AT END
              SET END-OF-INPUT         TO TRUE
           END-READ
       
           IF NOT END-OF-INPUT
              ADD 1                    TO WS-RECS-IN
              MOVE IN-CARRIER          TO WS-IN-CARRIER
              MOVE ZEROS               TO WS-IN-GROUPING
              MOVE IN-STATE            TO WS-IN-STATE
              MOVE IN-ACCOUNT          TO WS-IN-ACCOUNT
           END-IF
       
           .
       0200-EXIT.
           EXIT.
       
       0500-PROCESS.

           PERFORM 1000-PROCESS        THRU 1000-EXIT

           PERFORM 0200-READ-INPUT     THRU 0200-EXIT

           .
       0500-EXIT.
           EXIT.

       1000-PROCESS.
032714
032714     MOVE SPACES TO WS-TRIED-CID
032713                    WS-ACCOUNT-FOUND
032714     PERFORM 2600-FIND-ACCOUNT   THRU 2600-EXIT
032714     IF ACCOUNT-NOT-FOUND
032714         DISPLAY ' NO ACCOUNT FOUND FOR ' WS-IN-CARRIER ' '
032714           WS-IN-STATE ' ' WS-IN-ACCOUNT
032714         GO TO 1000-EXIT
031714     END-IF

           PERFORM 2500-GET-ERACNT-SEQ-NO
                                       THRU 2500-EXIT
           PERFORM 2400-BUILD-ERACNT   THRU 2400-EXIT
              
           .
       1000-EXIT.
           EXIT.

       2400-BUILD-ERACNT.

032714     MOVE AM-COMPANY-CD          TO NT-COMPANY-CD
           MOVE '1'                    TO NT-RECORD-TYPE
           MOVE WS-IN-CARRIER          TO NT-CARRIER
           MOVE WS-IN-GROUPING         TO NT-GROUPING
           MOVE WS-IN-STATE            TO NT-STATE
           MOVE WS-IN-ACCOUNT          TO NT-ACCOUNT
           MOVE WS-SEQ-NO              TO NT-LINE-SEQUENCE
           MOVE IN-USER-ID             TO NT-LAST-MAINT-BY
           MOVE WS-CURRENT-BIN-DATE    TO NT-LAST-MAINT-DT
           MOVE +220000                TO NT-LAST-MAINT-HHMMSS
           MOVE IN-COMMENT             TO NT-NOTE-LINE
           PERFORM 2450-WRITE-ERACNT   THRU 2450-EXIT
032714
032714     IF SAVE-RECORD-FOUND
032714         SUBTRACT +1 FROM WS-SEQ-NO
032714         MOVE WS-SEQ-NO          TO NT-LINE-SEQUENCE
032714         MOVE WS-SAVE-USER-ID    TO NT-LAST-MAINT-BY
032714         MOVE WS-SAVE-LAST-MAINT-DT TO NT-LAST-MAINT-DT
032714         MOVE WS-SAVE-LAST-MAINT-TIME TO NT-LAST-MAINT-HHMMSS
032714         MOVE WS-SAVE-COMMENT    TO NT-NOTE-LINE
032714         MOVE 'N'                TO WS-IND-SAVE-REC
032714         PERFORM 2460-WRITE-SAVE-ERACNT   THRU 2460-EXIT
032714     END-IF
032714
           .
       2400-EXIT.
           EXIT.

       2450-WRITE-ERACNT.

PEMTST*    MOVE '00' TO ERACNT-FILE-STATUS
032714     IF SAVE-RECORD-FOUND
032714         REWRITE NOTE-FILE
032714     ELSE
PEMTST     WRITE NOTE-FILE     
032714     END-IF

           IF ERACNT-FILE-STATUS = '00'
              ADD 1                    TO WS-ERACNT-RECS-ADD
              DISPLAY ' ADDING ' IN-COMMENT ' TO ' WS-IN-CARRIER ' '
                 WS-IN-STATE ' ' WS-IN-ACCOUNT ' ' WS-SEQ-NO
           ELSE
              IF ERACNT-FILE-STATUS = '22'
                 DISPLAY ' SOMETHING WENT WRONG '
                    'WITH SEQ NO ' WS-IN-CARRIER ' ' WS-IN-STATE
                    ' ' WS-IN-ACCOUNT
                 PERFORM ABEND-PGM
              ELSE
                 DISPLAY ' ERROR - ERACNT - WRITE ' ERACNT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       2450-EXIT.
           EXIT.

032714
032714 2460-WRITE-SAVE-ERACNT.
032714
032714     WRITE NOTE-FILE     
032714
032714     IF ERACNT-FILE-STATUS = '00'
032714        CONTINUE
032714     ELSE
032714        IF ERACNT-FILE-STATUS = '22'
032714           DISPLAY ' SOMETHING WENT WRONG '
032714              'WITH SAVE REC ' WS-IN-CARRIER ' ' WS-IN-STATE
032714              ' ' WS-IN-ACCOUNT
032714           PERFORM ABEND-PGM
032714        ELSE
032714           DISPLAY ' ERROR - ERACNT - WRITE ' ERACNT-FILE-STATUS
032714           PERFORM ABEND-PGM
032714        END-IF
032714     END-IF
032714
032714     .
032714 2460-EXIT.
032714     EXIT.
032714
       2500-GET-ERACNT-SEQ-NO.

           MOVE +4096                  TO WS-SEQ-NO
032714     MOVE 'N'                    TO WS-IND-SAVE-REC
           PERFORM 2510-START-ERACNT   THRU 2510-EXIT
           PERFORM 2520-READNEXT-ERACNT
                                       THRU 2520-EXIT
           IF (NT-CONTROL-PRIMARY (2:19) = WS-IN-KEY)
              AND (NT-RECORD-TYPE = '1')
032714        IF NT-NOTE-LINE (1:2) = '--'  AND
032714         NT-LINE-SEQUENCE > +1
032714           MOVE NT-NOTE-LINE TO WS-SAVE-COMMENT
032714           MOVE NT-LAST-MAINT-BY TO WS-SAVE-USER-ID
032714           MOVE NT-LAST-MAINT-DT TO WS-SAVE-LAST-MAINT-DT
032714           MOVE NT-LAST-MAINT-HHMMSS TO WS-SAVE-LAST-MAINT-TIME
032714           SET SAVE-RECORD-FOUND TO TRUE
032714           MOVE NT-LINE-SEQUENCE TO WS-SEQ-NO
032714        ELSE
              IF NT-LINE-SEQUENCE > +1
                 COMPUTE WS-SEQ-NO = NT-LINE-SEQUENCE - +1
              ELSE
                 MOVE IN-CARRIER       TO H1-CARRIER
032714           MOVE WS-IN-GROUPING   TO H1-GROUPING
                 MOVE IN-STATE         TO H1-STATE
                 MOVE IN-ACCOUNT       TO H1-ACCOUNT
                 MOVE ' BAD SEQUENCE NUMBER '
                                       TO H1-COMMENT
                 PERFORM 9300-PRT-RTN  THRU 9300-EXIT
              END-IF
032714        END-IF
           ELSE
              DISPLAY ' NO CURRENT NOTES FOR ' WS-IN-CARRIER ' '
                 WS-IN-STATE ' ' WS-IN-ACCOUNT
           END-IF

           .
       2500-EXIT.
           EXIT.

       2510-START-ERACNT.

032714     MOVE AM-COMPANY-CD          TO NT-COMPANY-CD
           MOVE '1'                    TO NT-RECORD-TYPE
           MOVE WS-IN-CARRIER          TO NT-CARRIER
           MOVE WS-IN-GROUPING         TO NT-GROUPING
           MOVE WS-IN-STATE            TO NT-STATE
           MOVE WS-IN-ACCOUNT          TO NT-ACCOUNT
           MOVE +0                     TO NT-LINE-SEQUENCE

           START ERACNT KEY >= NT-CONTROL-PRIMARY

           IF ERACNT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERACNT - START ' ERACNT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       2510-EXIT.
           EXIT.

       2520-READNEXT-ERACNT.

           READ ERACNT NEXT RECORD

           IF (ERACNT-FILE-STATUS = '10' OR '23')
032714        OR (NT-COMPANY-CD NOT = AM-COMPANY-CD)
              DISPLAY ' REACHING END OF ERACNT' ERACNT-FILE-STATUS
           ELSE
              IF ERACNT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERACNT - READNEXT '
                    ERACNT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       2520-EXIT.
           EXIT.

032714 2600-FIND-ACCOUNT.

           PERFORM 2610-START-ERACCT   THRU 2610-EXIT
           PERFORM 2620-READNEXT-ERACCT
                                       THRU 2620-EXIT
           IF AM-CONTROL-PRIMARY (2:19) = WS-IN-KEY
              CONTINUE
           ELSE
032714       IF AM-COMPANY-CD = X'04'
032714          MOVE 'X'               TO WS-TRIED-CID
032714          GO TO 2600-FIND-ACCOUNT
032714       ELSE
032714        MOVE 'X'                 TO WS-ACCOUNT-FOUND
              MOVE IN-CARRIER          TO H1-CARRIER
032714        MOVE WS-IN-GROUPING      TO H1-GROUPING
              MOVE IN-STATE            TO H1-STATE
              MOVE IN-ACCOUNT          TO H1-ACCOUNT
              MOVE ' NO ACCOUNT FOUND '
                                       TO H1-COMMENT
              PERFORM 9300-PRT-RTN     THRU 9300-EXIT
032714       END-IF
           END-IF

           .
       2600-EXIT.
           EXIT.

       2610-START-ERACCT.

032714     IF WS-IN-CARRIER LESS THAN '8'
032714         MOVE X'05'              TO AM-COMPANY-CD
032714     ELSE
032714       IF ALREADY-TRIED-CID
032714          MOVE X'06'             TO AM-COMPANY-CD
032714       ELSE
032714          MOVE X'04'             TO AM-COMPANY-CD
032714       END-IF
032714     END-IF
           MOVE WS-IN-CARRIER          TO AM-CARRIER
           MOVE WS-IN-GROUPING         TO AM-GROUPING
           MOVE WS-IN-STATE            TO AM-STATE
           MOVE WS-IN-ACCOUNT          TO AM-ACCOUNT
           MOVE LOW-VALUES             TO AM-CNTRL-B

           START ERACCT KEY >= AM-CONTROL-PRIMARY

           IF ERACCT-FILE-STATUS NOT = '00'
              MOVE IN-CARRIER          TO H1-CARRIER
032714        MOVE WS-IN-GROUPING      TO H1-GROUPING
              MOVE IN-STATE            TO H1-STATE
              MOVE IN-ACCOUNT          TO H1-ACCOUNT
              MOVE ' NO ACCOUNT FOUND '
                                       TO H1-COMMENT

              PERFORM 9300-PRT-RTN     THRU 9300-EXIT
              DISPLAY ' ERROR - ERACCT - START ' ERACCT-FILE-STATUS
           END-IF

           .
       2610-EXIT.
           EXIT.

       2620-READNEXT-ERACCT.

           READ ERACCT NEXT RECORD

           IF (ERACCT-FILE-STATUS = '10' OR '23')
032714*       OR (AM-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              MOVE IN-CARRIER          TO H1-CARRIER
032714        MOVE WS-IN-GROUPING      TO H1-GROUPING
              MOVE IN-STATE            TO H1-STATE
              MOVE IN-ACCOUNT          TO H1-ACCOUNT
              MOVE ' NO ACCOUNT FOUND '
                                       TO H1-COMMENT
              PERFORM 9300-PRT-RTN     THRU 9300-EXIT
              DISPLAY ' REACHING END OF ERACCT' ERACCT-FILE-STATUS
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERACCT - READNEXT '
                    ERACCT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       2620-EXIT.
           EXIT.

       3000-CLOSE-FILES.

           CLOSE FILE-IN ERACNT ERACCT PRNTR

           IF ERACNT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERACNT - CLOSE ' ERACNT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERACCT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERACCT - CLOSE ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       3000-EXIT.
           EXIT.

       4000-FINAL-TOTALS.

           DISPLAY '**************************************************'
           DISPLAY '***                                            ***'

           MOVE WS-RECS-IN             TO WS-DISPLAY-CNT
           DISPLAY '***  INPUT RECORDS READ          = ' WS-DISPLAY-CNT

           MOVE WS-ERACNT-RECS-ADD     TO WS-DISPLAY-CNT
           DISPLAY '***  ERACNT MASTER RECS ADDED    = ' WS-DISPLAY-CNT

           DISPLAY '***                                            ***'
           DISPLAY '**************************************************'

           .
       4000-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.


       9200-HDR-RTN.

           ADD +1                      TO WS-PAGE
           MOVE WS-PAGE                TO H3-PAGE
           MOVE HDR-1                  TO PRT
           WRITE PRT
           MOVE HDR-2                  TO PRT
           WRITE PRT
           MOVE HDR-3                  TO PRT
           WRITE PRT
           MOVE HDR-4                  TO PRT
           WRITE PRT
           MOVE +6                     TO WS-LINE-COUNT

           .
       9200-EXIT.
           EXIT.                                       

       9300-PRT-RTN.                                   

           IF WS-LINE-COUNT > WS-LINE-COUNT-MAX
              PERFORM 9200-HDR-RTN     THRU 9200-EXIT
              MOVE +0                  TO WS-LINE-COUNT
           END-IF 

           MOVE DTL-1                  TO PRT
           WRITE PRT
           ADD +1 TO WS-LINE-COUNT

           .
       9300-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
