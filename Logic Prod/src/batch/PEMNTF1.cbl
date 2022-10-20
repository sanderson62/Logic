       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMNTF1.
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
070709******************************************************************       

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERACNT           ASSIGN TO ERACNT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS NT-CONTROL-PRIMARY
                                   FILE STATUS IS ERACNT-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.


       FD  ERACNT.

                                       COPY ERCACNT.

       FD  DISK-DATE                   COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
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

PEMTST*    OPEN INPUT ERACNT
PEMTST     OPEN I-O   ERACNT

           IF ERACNT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERACNT - OPEN ' ERACNT-FILE-STATUS
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

           PERFORM 2510-START-ERACNT   THRU 2510-EXIT
           PERFORM 2520-READNEXT-ERACNT THRU 2520-EXIT

           .
       0020-EXIT.
           EXIT.

       0500-PROCESS.

           IF NT-RECORD-TYPE = '1'
              AND (NT-NOTE-LINE =
                 '... ALSO CHANGED GA FROM 957400 TO 928900 ')
              PERFORM 1000-PROCESS     THRU 1000-EXIT
           END-IF

           PERFORM 2520-READNEXT-ERACNT THRU 2520-EXIT

           .
       0500-EXIT.
           EXIT.

       1000-PROCESS.

           MOVE '... ALSO CHANGED GA FROM 957400 TO 628900 '
                                       TO NT-NOTE-LINE
           DISPLAY ' FIXING ' NT-CARRIER ' ' NT-STATE ' '
              NT-ACCOUNT ' ' NT-NOTE-LINE

           PERFORM 2450-REWRITE-ERACNT THRU 2450-EXIT
              
           .
       1000-EXIT.
           EXIT.

       2450-REWRITE-ERACNT.

PEMTST*    MOVE '00' TO ERACNT-FILE-STATUS
PEMTST     REWRITE NOTE-FILE     

           IF ERACNT-FILE-STATUS = '00'
              ADD 1                    TO WS-ERACNT-RECS-ADD
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

       2510-START-ERACNT.

           MOVE DTE-CLASIC-COMPANY-CD  TO NT-COMPANY-CD
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
              OR (NT-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-INPUT TO TRUE
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


       3000-CLOSE-FILES.

           CLOSE ERACNT

           IF ERACNT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERACNT - CLOSE ' ERACNT-FILE-STATUS
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



       ABEND-PGM.
                                       COPY ELCABEND.
