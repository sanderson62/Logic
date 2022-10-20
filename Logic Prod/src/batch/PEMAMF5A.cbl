       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMAMF5A.
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

       DATA DIVISION.
       FILE SECTION.

       FD  ERACCT.

                                       COPY ERCACCT.

       WORKING-STORAGE SECTION.
       77  S1                          PIC S9(3) COMP-3 VALUE +0.
       77  WS-STATE-SW                 PIC X  VALUE ' '.
           88  VALID-STATE               VALUE 'Y'.
       77  WS-DIS-EFF-DT               PIC X(10)  VALUE SPACES.
       77  WS-DIS-EXP-DT               PIC X(10)  VALUE SPACES.
       01  WS-STATUS-CODES.
           05  ERACCT-FILE-STATUS      PIC XX  VALUE SPACES.
           05  WS-EOF-SW               PIC X   VALUE SPACES.
               88  END-OF-ERACCT               VALUE 'Y'.
               88  MORE-ERACCT                 VALUE ' '.

      *****************************************************************
      *                                                               *
      *                     STATE CODE TABLE                          *
      *                                                               *
      *  CAUTION:  THIS TABLE MUST BE IN SEQUENCE BY STATE OR THE     *
      *            SEARCH VERB WILL NOT WORK PROPERLY !               *
      *                                                               *
      *****************************************************************

       01  STATE-VALUES.
           05  PIC X(24)  VALUE 'AAAA                    '.
           05  PIC X(24)  VALUE 'AEAE                    '.
           05  PIC X(24)  VALUE 'AKAKALASKA              '.
           05  PIC X(24)  VALUE 'ALALALABAMA             '.
           05  PIC X(24)  VALUE 'APAP                    '.
           05  PIC X(24)  VALUE 'ARARARKANSAS            '.
           05  PIC X(24)  VALUE 'ASASAMERICAN SAMOA      '.
           05  PIC X(24)  VALUE 'AZAZARIZONA             '.
           05  PIC X(24)  VALUE 'CACACALIFORNIA          '.
           05  PIC X(24)  VALUE 'CDCNCANADA              '.
           05  PIC X(24)  VALUE 'CNCNCANADA              '.
           05  PIC X(24)  VALUE 'COCOCOLORADO            '.
           05  PIC X(24)  VALUE 'CTCTCONNECTICUT         '.
           05  PIC X(24)  VALUE 'DCDCDISTRICT OF COLUMBIA'.
           05  PIC X(24)  VALUE 'DEDEDELAWARE            '.
           05  PIC X(24)  VALUE 'FCOTOTHER ALIEN         '.
           05  PIC X(24)  VALUE 'FLFLFLORIDA             '.
           05  PIC X(24)  VALUE 'GAGAGEORGIA             '.
           05  PIC X(24)  VALUE 'GUGUGUAM                '.
           05  PIC X(24)  VALUE 'HIHIHAWAII              '.
           05  PIC X(24)  VALUE 'IAIAIOWA                '.
           05  PIC X(24)  VALUE 'IDIDIDAHO               '.
           05  PIC X(24)  VALUE 'ILILILLINOIS            '.
           05  PIC X(24)  VALUE 'INININDIANA             '.
           05  PIC X(24)  VALUE 'KSKSKANSAS              '.
           05  PIC X(24)  VALUE 'KYKYKENTUCKY            '.
           05  PIC X(24)  VALUE 'LALALOUISIANA           '.
           05  PIC X(24)  VALUE 'MAMAMASSACHUSETTS       '.
           05  PIC X(24)  VALUE 'MDMDMARYLAND            '.
           05  PIC X(24)  VALUE 'MEMEMAINE               '.
           05  PIC X(24)  VALUE 'MIMIMICHIGAN            '.
           05  PIC X(24)  VALUE 'MNMNMINNESOTA           '.
           05  PIC X(24)  VALUE 'MOMOMISSOURI            '.
           05  PIC X(24)  VALUE 'MSMSMISSISSIPPI         '.
           05  PIC X(24)  VALUE 'MTMTMONTANA             '.
           05  PIC X(24)  VALUE 'NCNCNORTH CAROLINA      '.
           05  PIC X(24)  VALUE 'NDNDNORTH DAKOTA        '.
           05  PIC X(24)  VALUE 'NENENEBRASKA            '.
           05  PIC X(24)  VALUE 'NHNHNEW HAMPSHIRE       '.
           05  PIC X(24)  VALUE 'NJNJNEW JERSEY          '.
           05  PIC X(24)  VALUE 'NMNMNEW MEXICO          '.
           05  PIC X(24)  VALUE 'NVNVNEVADA              '.
           05  PIC X(24)  VALUE 'NYNYNEW YORK            '.
           05  PIC X(24)  VALUE 'OFOTOTHER ALIEN         '.
           05  PIC X(24)  VALUE 'OHOHOHIO                '.
           05  PIC X(24)  VALUE 'OKOKOKLAHOMA            '.
           05  PIC X(24)  VALUE 'OROROREGON              '.
           05  PIC X(24)  VALUE 'OTOTOTHER ALIEN         '.
           05  PIC X(24)  VALUE 'PAPAPENNSYLVANIA        '.
           05  PIC X(24)  VALUE 'PRPRPUERTO RICO         '.
           05  PIC X(24)  VALUE 'RIRIRHODE ISLAND        '.
           05  PIC X(24)  VALUE 'SCSCSOUTH CAROLINA      '.
           05  PIC X(24)  VALUE 'SDSDSOUTH DAKOTA        '.
           05  PIC X(24)  VALUE 'TNTNTENNESSEE           '.
           05  PIC X(24)  VALUE 'TXTXTEXAS               '.
           05  PIC X(24)  VALUE 'UTUTUTAH                '.
           05  PIC X(24)  VALUE 'VAVAVIRGINIA            '.
           05  PIC X(24)  VALUE 'VIVIUS VIRGIN ISLANDS   '.
           05  PIC X(24)  VALUE 'VTVTVERMONT             '.
           05  PIC X(24)  VALUE 'WAWAWASHINGTON          '.
           05  PIC X(24)  VALUE 'WIWIWISCONSIN           '.
           05  PIC X(24)  VALUE 'WVWVWEST VIRGINIA       '.
           05  PIC X(24)  VALUE 'WYWYWYOMING             '.

       01  FILLER REDEFINES STATE-VALUES.
           05  STATE-TABLE
                   OCCURS 63 TIMES
                   ASCENDING KEY IS ST-STATE
                   INDEXED BY ST-INDEX.
               10  ST-STATE          PIC XX.
               10  ST-ALT-STATE      PIC XX.
               10  ST-NAME           PIC X(20).

                                       COPY ELCDATE.

       01  WS-HOLD-CITY-STATE          PIC X(30)  VALUE SPACES.
       01  WS-WORK-CITY-ST.
           05  WS-WORK-CITY            PIC X(28)  VALUE SPACES.
           05  WS-WORK-STATE           PIC XX     VALUE SPACES.
       01  WS-HOLD-ERACCT              PIC X(2000) VALUE LOW-VALUES.
       01  WS-WORK-FIELDS.
           05  WS-ABEND-FLD            PIC 9         VALUE ZEROS.
           05  WS-ZERO-FLD             PIC 9         VALUE ZEROS.
           05  WS-ERACCT-RECS-IN       PIC 9(9)      VALUE ZEROS.
           05  WS-ERACCT-RECS-FIX      PIC 9(9)      VALUE ZEROS.
           05  WS-ERACCT-RECS-DEL      PIC 9(9)      VALUE ZEROS.
           05  WS-DISPLAY-CNT          PIC Z,ZZZ,ZZ9 VALUE ZEROS.
           05  WS-INVALID-ST-CNT       PIC 9(9)      VALUE ZEROS.

       PROCEDURE DIVISION.
       0000-MAINLINE.

           PERFORM 0100-OPEN-FILES     THRU 0100-EXIT
           PERFORM 0200-INITIALIZE     THRU 0200-EXIT
           PERFORM 0500-PROCESS        THRU 0500-EXIT UNTIL
              END-OF-ERACCT
PEMTST*       OR WS-ERACCT-RECS-FIX > 500
           PERFORM 3000-CLOSE-FILES    THRU 3000-EXIT
           PERFORM 4000-FINAL-TOTALS   THRU 4000-EXIT
           GOBACK

           .
       0000-EXIT.
           EXIT.

       0100-OPEN-FILES.

PEMTST     OPEN I-O   ERACCT
PEMTST*    OPEN INPUT ERACCT

           IF ERACCT-FILE-STATUS = '00'
               CONTINUE
           ELSE
               DISPLAY '*** ERROR OPENING ERACCT FILE ***'
               DISPLAY '*** STATUS CODE IS ' ERACCT-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-INITIALIZE.

           MOVE SPACES                 TO WS-EOF-SW
           MOVE ZEROS                  TO WS-ERACCT-RECS-IN
                                          WS-ERACCT-RECS-FIX
                                          WS-ERACCT-RECS-DEL

           PERFORM 1100-START-ERACCT   THRU 1100-EXIT

           PERFORM 1200-READ-ERACCT    THRU 1200-EXIT

           .
       0200-EXIT.
           EXIT.

       0500-PROCESS.

           IF AM-CITY NOT = SPACES
              PERFORM 1000-PROCESS     THRU 1000-EXIT
           END-IF

           PERFORM 1200-READ-ERACCT    THRU 1200-EXIT

           .
       0500-EXIT.
           EXIT.

       1000-PROCESS.

           MOVE SPACES                 TO WS-WORK-CITY-ST
           MOVE AM-CITY                TO WS-HOLD-CITY-STATE

      ** CHECK FOR GUAM AND CHANGE TO GU **
           PERFORM VARYING S1 FROM +27 BY -1 UNTIL
              (S1 = +1)
              OR (AM-CITY (S1:4) = 'GUAM')
           END-PERFORM
           IF AM-CITY (S1:4) = 'GUAM'
              MOVE 'GU  '              TO AM-CITY (S1:4)
              DISPLAY ' FIXED GUAM ' WS-HOLD-CITY-STATE ' '
                 AM-CITY
              MOVE AM-CITY             TO WS-HOLD-CITY-STATE
           END-IF

           PERFORM VARYING S1 FROM +30 BY -1 UNTIL
              (AM-CITY (S1:1) NOT = SPACE AND '.')
              OR (S1 = +1)
           END-PERFORM
           IF S1 > 3
              SUBTRACT +1 FROM S1
              MOVE AM-CITY (S1:2)
                                       TO WS-WORK-STATE
              SUBTRACT +1 FROM S1
              PERFORM VARYING S1 FROM S1 BY -1 UNTIL
                 AM-CITY (S1:1) NOT = SPACE
              END-PERFORM
              IF S1 > 2
                 IF AM-CITY (S1:1) = ',' OR ';' OR '.'
                    MOVE AM-CITY (1:S1 - 1) TO WS-WORK-CITY
                 ELSE
                    MOVE AM-CITY (1:S1) TO WS-WORK-CITY
                 END-IF
              END-IF
              MOVE SPACE               TO WS-STATE-SW
              PERFORM 1500-EDIT-STATE  THRU 1500-EXIT
              IF VALID-STATE
                 MOVE WS-WORK-CITY     TO AM-ADDR-CITY
                 MOVE WS-WORK-STATE    TO AM-ADDR-STATE
                 PERFORM 2100-REWRITE-ERACCT
                                       THRU 2100-EXIT
              ELSE
                 IF AM-STATUS = '0'
                    DISPLAY 'ACTIVE INVALID STATE CODE: ' WS-WORK-STATE
                       ' ' WS-HOLD-CITY-STATE ' ' AM-CONTROL-A ' '
                       WS-DIS-EXP-DT ' ' WS-DIS-EFF-DT
                 ELSE
                    DISPLAY 'INACTIVE INVALID STATE CODE: '
                      WS-WORK-STATE ' ' WS-HOLD-CITY-STATE ' '
                      AM-CONTROL-A ' ' WS-DIS-EXP-DT ' ' WS-DIS-EFF-DT
                 END-IF
              END-IF
           ELSE
              DISPLAY ' WEIRD CITY STATE ' AM-CONTROL-A
                 ' ' AM-CITY
           END-IF


           .
       1000-EXIT.
           EXIT.

       1100-START-ERACCT.

           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY
           MOVE X'04'                  TO AM-COMPANY-CD

           START ERACCT KEY IS NOT <
                            AM-CONTROL-PRIMARY
           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERACCT, BAD START '
                    ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .

       1100-EXIT.
           EXIT.

       1200-READ-ERACCT.

           READ ERACCT NEXT RECORD

           IF ERACCT-FILE-STATUS = '10' OR '23'
              SET END-OF-ERACCT TO TRUE
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY 'ERACCT, BAD READ NEXT '
                      ERACCT-FILE-STATUS
                 PERFORM 9999-ABEND-RTN
              ELSE
                 ADD 1              TO WS-ERACCT-RECS-IN
              END-IF
           END-IF

           .
       1200-EXIT.
           EXIT.

       1500-EDIT-STATE.

           SEARCH ALL STATE-TABLE AT END
              PERFORM 1650-CONVERT-DATES THRU 1650-EXIT
              ADD 1 TO WS-INVALID-ST-CNT
              WHEN ST-STATE (ST-INDEX) = WS-WORK-STATE
                 SET VALID-STATE TO TRUE
           END-SEARCH

           .
       1500-EXIT.
           EXIT.

       1650-CONVERT-DATES.

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

           .
       1650-EXIT.
           EXIT.

       2100-REWRITE-ERACCT.

           DISPLAY ' OLD ' WS-HOLD-CITY-STATE ' NEW ' WS-WORK-CITY-ST
PEMTST     REWRITE ACCOUNT-MASTER
PEMTST*    MOVE '00' TO ERACCT-FILE-STATUS

           IF ERACCT-FILE-STATUS = '00'
               ADD 1                   TO WS-ERACCT-RECS-FIX
           ELSE
               DISPLAY 'ERACCT, BAD REWRITE '
               DISPLAY '*** STATUS CODE IS ' ERACCT-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF
           .
       2100-EXIT.
           EXIT.

       2200-DELETE-ERACCT.

           DELETE  ERACCT

           IF ERACCT-FILE-STATUS = '00'
               ADD 1                   TO WS-ERACCT-RECS-DEL
           ELSE
               DISPLAY 'ERACCT, BAD DELETE  '
               DISPLAY '*** STATUS CODE IS ' ERACCT-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF
           .
       2200-EXIT.
           EXIT.


       3000-CLOSE-FILES.

           CLOSE ERACCT

           .
       3000-EXIT.
           EXIT.

       4000-FINAL-TOTALS.

           DISPLAY '**************************************************'
           DISPLAY '***                                            ***'

           MOVE WS-ERACCT-RECS-IN      TO WS-DISPLAY-CNT
           DISPLAY '***  ERACCT RECORDS   IN         = ' WS-DISPLAY-CNT

           MOVE WS-ERACCT-RECS-FIX     TO WS-DISPLAY-CNT
           DISPLAY '***  ERACCT RECORDS   FIXED      = ' WS-DISPLAY-CNT

           MOVE WS-INVALID-ST-CNT      TO WS-DISPLAY-CNT
           DISPLAY '***  INVALID STATES              = ' WS-DISPLAY-CNT

           MOVE WS-ERACCT-RECS-DEL     TO WS-DISPLAY-CNT
           DISPLAY '***  ERACCT RECORDS   DELETED    = ' WS-DISPLAY-CNT

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

           DISPLAY '*** ERACCT  FILE CONVERSION PROGRAM ABENDING ***'
           DISPLAY '************************************************'

           MOVE 1                      TO WS-ABEND-FLD
           MOVE 0                      TO WS-ZERO-FLD

           COMPUTE WS-ABEND-FLD = WS-ABEND-FLD / WS-ZERO-FLD
           .
       9999-EXIT.
           EXIT.
