       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMAMFRTC.
       AUTHOR.     PABLO.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

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

       FD  ERACCT.

                                       COPY ERCACCT.

       FD  ERACNT.

                                       COPY ERCACNT.

       FD  DISK-DATE                   COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  ERACNT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  WS-PREV-ERACNT-KEY          PIC X(19)     VALUE LOW-VALUES.
       77  WS-ERACNT-RECS-ADD          PIC 9(9)      VALUE ZEROS.
       77  WS-ERACCTS-FOUND            PIC 9(9)      VALUE ZEROS.
       77  WS-CURRENT-BIN-DATE         PIC XX   VALUE LOW-VALUES.
       77  WS-DIS-EFF-DT               PIC X(10)  VALUE SPACES.
       77  WS-DIS-EXP-DT               PIC X(10)  VALUE SPACES.
       77  WS-SEQ-NO                   PIC S9(4)   COMP VALUE +0.
       77  A1                          PIC S999 COMP-3 VALUE +0.
       77  WS-NEW-REIN                 PIC XXX  VALUE SPACES.
       77  WS-REWRITE-ERACCT-SW        PIC X  VALUE SPACES.
           88  REWRITE-ERACCT             VALUE 'Y'.

       01  WS-STATUS-CODES.
           05  ERACCT-FILE-STATUS      PIC XX  VALUE SPACES.
           05  WS-ERACCT-SW            PIC X   VALUE SPACES.
               88  END-OF-ERACCT               VALUE 'Y'.

       01  WS-TABLE-KEY.
           05  WS-GROUP-A              PIC X(6).
           05  WS-GROUP-B              PIC X(5).
           05  WS-MATCH-STATE-YN       PIC XX.

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
           05  WS-ERACCT-RECS-IN       PIC 9(9)      VALUE ZEROS.
           05  WS-ERACCT-LVL-UPD       PIC 9(9)      VALUE ZEROS.
           05  WS-ERACCT-RECS-FIX      PIC 9(9)      VALUE ZEROS.
           05  WS-ERACCT-RECS-ADD      PIC 9(9)      VALUE ZEROS.
           05  WS-ERACCT-RECS-DEL      PIC 9(9)      VALUE ZEROS.
           05  WS-DISPLAY-CNT          PIC Z,ZZZ,ZZ9 VALUE ZEROS.
           05  WS-CURRENT-AM-KEY       PIC X(19)     VALUE LOW-VALUES.

       01  T1                          PIC S999   COMP-3 VALUE +0.
       01  T1M                         PIC S999   COMP-3 VALUE +68.
       01  FILLER.     
           05  WS-TABLE-1.
               10  FILLER        PIC X(16)  VALUE 'EBPROVEBACTNNEBA'.
               10  FILLER        PIC X(16)  VALUE 'EBPROVEBACTYYEBB'.
               10  FILLER        PIC X(16)  VALUE 'EBPROVEBGA NNEBC'.
               10  FILLER        PIC X(16)  VALUE 'EBPROVEBGA YYEBD'.

               10  FILLER        PIC X(16)  VALUE 'EBBANKEBACTNNEBE'.
               10  FILLER        PIC X(16)  VALUE 'EBBANKEBACTYYEBF'.
               10  FILLER        PIC X(16)  VALUE 'EBBANKEBGA NNEBG'.
               10  FILLER        PIC X(16)  VALUE 'EBBANKEBGA YYEBH'.

               10  FILLER        PIC X(16)  VALUE 'EBFCONEBACTNNEBI'.
               10  FILLER        PIC X(16)  VALUE 'EBFCONEBACTYYEBJ'.
100109         10  FILLER        PIC X(16)  VALUE 'EBFCONEBGA NNEBK'.
100109         10  FILLER        PIC X(16)  VALUE 'EBFCONEBGA YYEBL'.

               10  FILLER        PIC X(16)  VALUE 'EBVGRDEBACTNNEBM'.
               10  FILLER        PIC X(16)  VALUE 'EBVGRDEBACTYYEBN'.
               10  FILLER        PIC X(16)  VALUE 'EBVGRDEBGA NNEBO'.
               10  FILLER        PIC X(16)  VALUE 'EBVGRDEBGA YYEBP'.

               10  FILLER        PIC X(16)  VALUE 'EBPROFEBACTNNEBQ'.
               10  FILLER        PIC X(16)  VALUE 'EBPROFEBACTYYEBR'.
               10  FILLER        PIC X(16)  VALUE 'EBPROFEBGA NNEBS'.
               10  FILLER        PIC X(16)  VALUE 'EBPROFEBGA YYEBT'.

100109         10  FILLER        PIC X(16)  VALUE 'EBNORBEBACTNNEBU'.
100109         10  FILLER        PIC X(16)  VALUE 'EBNORBEBACTYYEBV'.
100109         10  FILLER        PIC X(16)  VALUE 'EBNORBEBGA NNEBW'.
100109         10  FILLER        PIC X(16)  VALUE 'EBNORBEBGA YYEBX'.

               10  FILLER        PIC X(16)  VALUE 'EBPERFEBACTNNEBY'.
               10  FILLER        PIC X(16)  VALUE 'EBPERFEBACTYYEBZ'.
               10  FILLER        PIC X(16)  VALUE 'EBPERFEBGA NNEMA'.
               10  FILLER        PIC X(16)  VALUE 'EBPERFEBGA YYEMB'.

               10  FILLER        PIC X(16)  VALUE 'EBAUTOEBACTNNEMC'.
               10  FILLER        PIC X(16)  VALUE 'EBAUTOEBACTYYEMD'.
               10  FILLER        PIC X(16)  VALUE 'EBAUTOEBGA NNEME'.
               10  FILLER        PIC X(16)  VALUE 'EBAUTOEBGA YYEMF'.

               10  FILLER        PIC X(16)  VALUE 'EBNEDSEBACTNNEMG'.
               10  FILLER        PIC X(16)  VALUE 'EBNEDSEBACTYYEMH'.
               10  FILLER        PIC X(16)  VALUE 'EBNEDSEBGA NNEMI'.
               10  FILLER        PIC X(16)  VALUE 'EBNEDSEBGA YYEMJ'.

               10  FILLER        PIC X(16)  VALUE 'EBINDSEBACTNNEMK'.
               10  FILLER        PIC X(16)  VALUE 'EBINDSEBACTYYEML'.
               10  FILLER        PIC X(16)  VALUE 'EBINDSEBGA NNEMM'.
               10  FILLER        PIC X(16)  VALUE 'EBINDSEBGA YYEMN'.

               10  FILLER        PIC X(16)  VALUE 'EBDMKTEBACTNNEMO'.
               10  FILLER        PIC X(16)  VALUE 'EBDMKTEBACTYYEMP'.
               10  FILLER        PIC X(16)  VALUE 'EBDMKTEBGA NNEMQ'.
               10  FILLER        PIC X(16)  VALUE 'EBDMKTEBGA YYEMR'.

100109         10  FILLER        PIC X(16)  VALUE 'EBNWDCEBACTNNEMS'.
100109         10  FILLER        PIC X(16)  VALUE 'EBNWDCEBACTYYEMT'.
100109         10  FILLER        PIC X(16)  VALUE 'EBNWDCEBGA NNEMU'.
100109         10  FILLER        PIC X(16)  VALUE 'EBNWDCEBGA YYEMV'.

100109         10  FILLER        PIC X(16)  VALUE 'EBKBFFEBACTNNEMW'.
100109         10  FILLER        PIC X(16)  VALUE 'EBKBFFEBACTYYEMX'.
100109         10  FILLER        PIC X(16)  VALUE 'EBKBFFEBGA NNEMY'.
100109         10  FILLER        PIC X(16)  VALUE 'EBKBFFEBGA YYEMZ'.

               10  FILLER        PIC X(16)  VALUE 'EBDPRFEBACTNNERA'.
               10  FILLER        PIC X(16)  VALUE 'EBDPRFEBACTYYERB'.
               10  FILLER        PIC X(16)  VALUE 'EBDPRFEBGA NNERC'.
               10  FILLER        PIC X(16)  VALUE 'EBDPRFEBGA YYERD'.

100109         10  FILLER        PIC X(16)  VALUE 'EBFAIREBACTNNERE'.
100109         10  FILLER        PIC X(16)  VALUE 'EBFAIREBACTYYERF'.
100109         10  FILLER        PIC X(16)  VALUE 'EBFAIREBGA NNERG'.
100109         10  FILLER        PIC X(16)  VALUE 'EBFAIREBGA YYERH'.

100109         10  FILLER        PIC X(16)  VALUE 'EBSDRKEBACTNNERI'.
100109         10  FILLER        PIC X(16)  VALUE 'EBSDRKEBACTYYERJ'.
100109         10  FILLER        PIC X(16)  VALUE 'EBSDRKEBGA NNERK'.
100109         10  FILLER        PIC X(16)  VALUE 'EBSDRKEBGA YYERL'.

100109         10  FILLER        PIC X(16)  VALUE 'EBAMDGEBACTNNERM'.
100109         10  FILLER        PIC X(16)  VALUE 'EBAMDGEBACTYYERN'.
100109         10  FILLER        PIC X(16)  VALUE 'EBAMDGEBGA NNERO'.
100109         10  FILLER        PIC X(16)  VALUE 'EBAMDGEBGA YYERP'.
           05  FILLER REDEFINES WS-TABLE-1 OCCURS 68.
               10  WS-GROUPA-KEY       PIC X(13).
               10  WS-REIN-TABLE       PIC XXX.

                                       COPY ELCFUNDT.
                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

       0000-LOAD-DATE-CARD.            COPY ELCDTERX.

       0000-MAINLINE.

           PERFORM 0100-OPEN-FILES     THRU 0100-EXIT
           PERFORM 0200-INITIALIZE     THRU 0200-EXIT
           PERFORM 0500-PROCESS        THRU 0500-EXIT UNTIL
              END-OF-ERACCT
           PERFORM 3000-CLOSE-FILES    THRU 3000-EXIT
           PERFORM 4000-FINAL-TOTALS   THRU 4000-EXIT
           GOBACK

           .
       0000-EXIT.
           EXIT.

       0100-OPEN-FILES.

PEMTST     OPEN I-O   ERACNT  ERACCT
PEMTST*    OPEN INPUT ERACCT ERACNT

           IF ERACCT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERACCT - OPEN ' ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           IF ERACNT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERACNT - OPEN ' ERACNT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-INITIALIZE.

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           DISPLAY ' FUNCTION DATE CYMD ' WS-FN-DATE
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

           MOVE SPACES                 TO WS-ERACCT-SW

           MOVE ZEROS                  TO WS-ERACCT-RECS-IN
                                          WS-ERACCT-RECS-FIX
                                          WS-ERACCT-RECS-ADD
                                          WS-ERACCT-RECS-DEL

           PERFORM 1100-START-ERACCT   THRU 1100-EXIT
           PERFORM 1200-READNEXT-ERACCT
                                       THRU 1200-EXIT

           .
       0200-EXIT.
           EXIT.

       0500-PROCESS.

           IF AM-REI-GROUP-B = 'EBGA  ' OR 'EBACT '
              DISPLAY ' FOUND EBGA OR EBACT ' AM-CARRIER ' '
              AM-STATE ' ' AM-ACCOUNT ' ' AM-REI-GROUP-B
           END-IF

           MOVE AM-REI-GROUP-A         TO WS-GROUP-A
           MOVE AM-REI-GROUP-B         TO WS-GROUP-B
           IF AM-STATE = 'MD' OR 'NV'
              MOVE 'YY'                TO WS-MATCH-STATE-YN
           ELSE
              MOVE 'NN'                TO WS-MATCH-STATE-YN
           END-IF

           PERFORM VARYING T1 FROM +1 BY +1 UNTIL
              (T1 > T1M)
              OR (WS-GROUPA-KEY (T1) = WS-TABLE-KEY)
           END-PERFORM
           IF T1 <= T1M
              PERFORM 1000-PROCESS     THRU 1000-EXIT
           END-IF

           PERFORM 1200-READNEXT-ERACCT
                                       THRU 1200-EXIT

           .
       0500-EXIT.
           EXIT.

       1000-PROCESS.

           IF WS-REIN-TABLE (T1) NOT = SPACES
              MOVE WS-REIN-TABLE (T1)  TO WS-NEW-REIN
              IF (AM-STATE NOT = 'MD' AND 'NV' AND 'KY')
                 AND (AM-REPORT-CODE-2 = 'MONTROSEAU')
                 MOVE '1EB'            TO WS-NEW-REIN
              END-IF
              IF AM-STATE = 'KY'
                 MOVE SPACES           TO WS-NEW-REIN
              END-IF
              IF WS-NEW-REIN NOT = AM-REI-TABLE
                 PERFORM 1300-DISPLAY-ACCOUNT-INFO
                                       THRU 1300-EXIT
                 MOVE WS-NEW-REIN          
                                       TO AM-REI-TABLE
                                       
PEMTMP           MOVE 'Y'              TO AM-RECALC-REIN
                 PERFORM 2100-REWRITE-ERACCT
                                       THRU 2100-EXIT
                 PERFORM 2500-GET-ERACNT-SEQ-NO
                                       THRU 2500-EXIT
PEMTST           PERFORM 2400-BUILD-ERACNT
PEMTST                                 THRU 2400-EXIT
              END-IF
           END-IF

           .
       1000-EXIT.
           EXIT.

       1100-START-ERACCT.

           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD

           START ERACCT KEY >= AM-CONTROL-PRIMARY

           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERACCT - START ' ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       1100-EXIT.
           EXIT.

       1200-READNEXT-ERACCT.

           READ ERACCT NEXT RECORD

           IF (ERACCT-FILE-STATUS = '10' OR '23')
              OR (AM-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              DISPLAY ' REACHING END ' ERACCT-FILE-STATUS
              SET END-OF-ERACCT        TO TRUE
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERACCT - READNEXT '
                    ERACCT-FILE-STATUS
                 PERFORM 9999-ABEND-RTN
              ELSE
                 ADD 1                 TO WS-ERACCT-RECS-IN
              END-IF
           END-IF

           .
       1200-EXIT.
           EXIT.

       1300-DISPLAY-ACCOUNT-INFO.

           MOVE AM-EFFECTIVE-DT        TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO WS-DIS-EFF-DT
           ELSE
              MOVE 'XX/XX/XXXX'        TO WS-DIS-EFF-DT
              DISPLAY ' ERROR CONVERTING EFFECT  DATE ' AM-STATE
              ' ' AM-ACCOUNT
           END-IF

           IF AM-EXPIRATION-DT = HIGH-VALUES
              MOVE '12/31/9999'        TO WS-DIS-EXP-DT
           ELSE
              MOVE AM-EXPIRATION-DT    TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              MOVE +0                  TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT
                                       TO WS-DIS-EXP-DT
              ELSE
                 MOVE 'XX/XX/XXXX'     TO WS-DIS-EXP-DT
                 DISPLAY ' ERROR CONVERTING EXPIRE  DATE ' AM-STATE
                 ' ' AM-ACCOUNT
              END-IF
           END-IF

           DISPLAY ' ABOUT TO CHG REIN TABLE ON '
              AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
                 ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
              ' FROM ' AM-REI-TABLE ' TO ' WS-NEW-REIN

           .
       1300-EXIT.
           EXIT.

       2100-REWRITE-ERACCT.

           MOVE 'CONV'                 TO AM-LAST-MAINT-USER
           MOVE WS-CURRENT-BIN-DATE    TO AM-LAST-MAINT-DT
           MOVE +220000                TO AM-LAST-MAINT-HHMMSS

PEMTST     REWRITE ACCOUNT-MASTER
PEMTST*    MOVE '00' TO ERACCT-FILE-STATUS

           IF ERACCT-FILE-STATUS = '00'
              ADD 1                    TO WS-ERACCT-RECS-FIX
           ELSE
              DISPLAY ' ERROR - ERACCT - REWRITE ' ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       2100-EXIT.
           EXIT.

       2200-WRITE-ERACCT.

           DISPLAY 'ERACCT   WRITE ' AM-CONTROL-A
           DISPLAY ' '

           WRITE ACCOUNT-MASTER

           IF ERACCT-FILE-STATUS = '00'
              ADD 1                    TO WS-ERACCT-RECS-ADD
           ELSE
              DISPLAY ' ERROR - ERACCT - WRITE ' ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       2200-EXIT.
           EXIT.

       2300-DELETE-ERACCT.

           DISPLAY 'ERACCT  DELETE ' AM-CONTROL-A
           DISPLAY ' '

           DELETE ERACCT

           IF ERACCT-FILE-STATUS = '00'
              ADD 1                    TO WS-ERACCT-RECS-DEL
           ELSE
              DISPLAY ' ERROR - ERACCT - DELETE ' ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       2300-EXIT.
           EXIT.

       2400-BUILD-ERACNT.

           IF WS-PREV-ERACNT-KEY = AM-CNTRL-1
              CONTINUE     
           ELSE
              MOVE AM-CNTRL-1          TO WS-PREV-ERACNT-KEY
              MOVE AM-COMPANY-CD       TO NT-COMPANY-CD
              MOVE '1'                 TO NT-RECORD-TYPE
              MOVE AM-CARRIER          TO NT-CARRIER
              MOVE AM-GROUPING         TO NT-GROUPING
              MOVE AM-STATE            TO NT-STATE
              MOVE AM-ACCOUNT          TO NT-ACCOUNT
              MOVE WS-SEQ-NO           TO NT-LINE-SEQUENCE
              MOVE 'CONV'              TO NT-LAST-MAINT-BY
              MOVE WS-CURRENT-BIN-DATE TO NT-LAST-MAINT-DT
              MOVE +220000             TO NT-LAST-MAINT-HHMMSS
              MOVE SPACES              TO NT-NOTE-LINE
              STRING 'CHANGED REIN TABLE TO ' WS-NEW-REIN
                 DELIMITED BY SIZE INTO NT-NOTE-LINE
              END-STRING
              MOVE '22'                TO ERACNT-FILE-STATUS
              PERFORM 2450-WRITE-ERACNT
                                       THRU 2450-EXIT UNTIL
                 (ERACNT-FILE-STATUS NOT = '22')
           END-IF

           .
       2400-EXIT.
           EXIT.

       2450-WRITE-ERACNT.

           WRITE NOTE-FILE     

           IF ERACNT-FILE-STATUS = '00'
              ADD 1                    TO WS-ERACNT-RECS-ADD
           ELSE
              IF ERACNT-FILE-STATUS = '22'
                 DISPLAY ' SOMETHING WENT WRONG '
                    'WITH SEQ NO ' AM-CARRIER ' ' AM-STATE
                    ' ' AM-ACCOUNT
                 PERFORM 9999-ABEND-RTN
              ELSE
                 DISPLAY 'ERACNT, BAD   WRITE '
                 DISPLAY '*** STATUS CODE IS ' ERACNT-FILE-STATUS
                 PERFORM 9999-ABEND-RTN
              END-IF
           END-IF

           .
       2450-EXIT.
           EXIT.

       2500-GET-ERACNT-SEQ-NO.

           MOVE +4096                  TO WS-SEQ-NO
           PERFORM 2510-START-ERACNT   THRU 2510-EXIT
           PERFORM 2520-READNEXT-ERACNT
                                       THRU 2520-EXIT
           IF (NT-CONTROL-PRIMARY (1:20) = AM-CONTROL-PRIMARY (1:20))
              AND (NT-RECORD-TYPE = '1')
              IF NT-LINE-SEQUENCE > +1
                 COMPUTE WS-SEQ-NO = NT-LINE-SEQUENCE - +1
              ELSE
                 DISPLAY ' SEQUENCE NO PROBLEM ' AM-CARRIER ' '
                    AM-STATE ' ' AM-ACCOUNT
              END-IF
           ELSE
              DISPLAY ' NO CURRENT NOTES FOR ' AM-CARRIER ' '
                 AM-STATE ' ' AM-ACCOUNT
           END-IF

           .
       2500-EXIT.
           EXIT.

       2510-START-ERACNT.

           MOVE AM-COMPANY-CD       TO NT-COMPANY-CD
           MOVE '1'                 TO NT-RECORD-TYPE
           MOVE AM-CARRIER          TO NT-CARRIER
           MOVE AM-GROUPING         TO NT-GROUPING
           MOVE AM-STATE            TO NT-STATE
           MOVE AM-ACCOUNT          TO NT-ACCOUNT
           MOVE +0                  TO NT-LINE-SEQUENCE

           START ERACNT KEY >= NT-CONTROL-PRIMARY

           IF ERACNT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERACNT - START ' ERACNT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       2510-EXIT.
           EXIT.

       2520-READNEXT-ERACNT.

           READ ERACNT NEXT RECORD

           IF (ERACNT-FILE-STATUS = '10' OR '23')
              OR (NT-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              DISPLAY ' REACHING END OF ERACNT' ERACNT-FILE-STATUS
           ELSE
              IF ERACNT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERACNT - READNEXT '
                    ERACNT-FILE-STATUS
                 PERFORM 9999-ABEND-RTN
              END-IF
           END-IF

           .
       2520-EXIT.
           EXIT.

       3000-CLOSE-FILES.

           CLOSE ERACCT ERACNT

           IF ERACCT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERACCT - CLOSE ' ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           IF ERACNT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERACNT - CLOSE ' ERACNT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       3000-EXIT.
           EXIT.

       4000-FINAL-TOTALS.

           DISPLAY '**************************************************'
           DISPLAY '***                                            ***'

           MOVE WS-ERACCTS-FOUND       TO WS-DISPLAY-CNT
           DISPLAY '***  ERACCT RECS FOUND           = ' WS-DISPLAY-CNT

           MOVE WS-ERACCT-RECS-IN      TO WS-DISPLAY-CNT
           DISPLAY '***  ERACCT MASTER RECS READ     = ' WS-DISPLAY-CNT

           MOVE WS-ERACCT-RECS-FIX     TO WS-DISPLAY-CNT
           DISPLAY '***  ERACCT MASTER RECS FIXED    = ' WS-DISPLAY-CNT

           MOVE WS-ERACCT-LVL-UPD      TO WS-DISPLAY-CNT
           DISPLAY '***  ERACCT MASTER LVLS FIXED    = ' WS-DISPLAY-CNT

           MOVE WS-ERACCT-RECS-ADD     TO WS-DISPLAY-CNT
           DISPLAY '***  ERACCT MASTER RECS ADDED    = ' WS-DISPLAY-CNT

           MOVE WS-ERACCT-RECS-DEL     TO WS-DISPLAY-CNT
           DISPLAY '***  ERACCT MASTER RECS DELETED  = ' WS-DISPLAY-CNT

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

       9999-ABEND-RTN.

           DISPLAY '*** ACCOUNT MSTR CONVERSION PROGRAM ABENDING ***'
           DISPLAY '************************************************'

           MOVE 1                      TO WS-ABEND-FLD
           MOVE 0                      TO WS-ZERO-FLD

           COMPUTE WS-ABEND-FLD = WS-ABEND-FLD / WS-ZERO-FLD

           .
       9999-EXIT.
           EXIT.
       ABEND-PGM.
                                       COPY ELCABEND.
