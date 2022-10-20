       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMATF3.
       AUTHOR.     PABLO.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ELMSTR           ASSIGN TO ELMSTR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CL-CONTROL-PRIMARY
                                   FILE STATUS IS ELMSTR-FILE-STATUS.

           SELECT ELTRLR           ASSIGN TO ELTRLR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AT-CONTROL-PRIMARY
                                   FILE STATUS IS ELTRLR-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  ELMSTR.

                                       COPY ELCMSTR.

       FD  ELTRLR.

                                       COPY ELCTRLR.

       WORKING-STORAGE SECTION.
       77  S1                          PIC S9(3) COMP-3 VALUE +0.
       77  WS-STATE-SW                 PIC X  VALUE ' '.
           88  VALID-STATE               VALUE 'Y'.
       01  WS-STATUS-CODES.
           05  ELMSTR-FILE-STATUS      PIC XX  VALUE SPACES.
           05  ELTRLR-FILE-STATUS      PIC XX  VALUE SPACES.
           05  WS-EOF-SW               PIC X   VALUE SPACES.
               88  END-OF-ELTRLR               VALUE 'Y'.
               88  MORE-ELTRLR                 VALUE ' '.

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

       01  WS-HOLD-CITY-STATE          PIC X(30)  VALUE SPACES.
       01  WS-WORK-CITY-ST.
           05  WS-WORK-CITY            PIC X(28)  VALUE SPACES.
           05  WS-WORK-STATE           PIC XX     VALUE SPACES.
       01  WS-HOLD-ELTRLR              PIC X(200) VALUE LOW-VALUES.
       01  WS-WORK-FIELDS.
           05  WS-ABEND-FLD            PIC 9         VALUE ZEROS.
           05  WS-ZERO-FLD             PIC 9         VALUE ZEROS.
           05  WS-ELTRLR-RECS-IN       PIC 9(9)      VALUE ZEROS.
           05  WS-ELTRLR-RECS-FIX      PIC 9(9)      VALUE ZEROS.
           05  WS-ELTRLR-RECS-DEL      PIC 9(9)      VALUE ZEROS.
           05  WS-DISPLAY-CNT          PIC Z,ZZZ,ZZ9 VALUE ZEROS.
           05  WS-INVALID-ST-CNT       PIC 9(9)      VALUE ZEROS.

       PROCEDURE DIVISION.
       0000-MAINLINE.

           PERFORM 0100-OPEN-FILES     THRU 0100-EXIT
           PERFORM 0200-INITIALIZE     THRU 0200-EXIT
           PERFORM 0500-PROCESS        THRU 0500-EXIT UNTIL
              END-OF-ELTRLR
PEMTST*       OR WS-ELTRLR-RECS-FIX > 500
           PERFORM 3000-CLOSE-FILES    THRU 3000-EXIT
           PERFORM 4000-FINAL-TOTALS   THRU 4000-EXIT
           GOBACK

           .
       0000-EXIT.
           EXIT.

       0100-OPEN-FILES.

PEMTST     OPEN I-O   ELTRLR
PEMTST*    OPEN INPUT ELTRLR

           OPEN INPUT ELMSTR

           IF ELTRLR-FILE-STATUS = '00'
               CONTINUE
           ELSE
               DISPLAY '*** ERROR OPENING ELTRLR FILE ***'
               DISPLAY '*** STATUS CODE IS ' ELTRLR-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF

           IF ELMSTR-FILE-STATUS = '00'
               CONTINUE
           ELSE
               DISPLAY '*** ERROR OPENING ELMSTR FILE ***'
               DISPLAY '*** STATUS CODE IS ' ELMSTR-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-INITIALIZE.

           MOVE SPACES                 TO WS-EOF-SW
           MOVE ZEROS                  TO WS-ELTRLR-RECS-IN
                                          WS-ELTRLR-RECS-FIX
                                          WS-ELTRLR-RECS-DEL

           PERFORM 1100-START-ELTRLR   THRU 1100-EXIT

           PERFORM 1200-READ-ELTRLR    THRU 1200-EXIT

           .
       0200-EXIT.
           EXIT.

       0500-PROCESS.

           IF (AT-TRAILER-TYPE = '5')
              AND (AT-CITY-STATE NOT = SPACES)
              PERFORM 1000-PROCESS     THRU 1000-EXIT
           END-IF

           PERFORM 1200-READ-ELTRLR    THRU 1200-EXIT

           .
       0500-EXIT.
           EXIT.

       1000-PROCESS.

           MOVE SPACES                 TO WS-WORK-CITY-ST
           MOVE AT-CITY-STATE          TO WS-HOLD-CITY-STATE
      ** CHECK FOR GUAM AND CHANGE TO GU **
           PERFORM VARYING S1 FROM +27 BY -1 UNTIL
              (S1 = +1)
              OR (AT-CITY-STATE (S1:4) = 'GUAM')
           END-PERFORM
           IF AT-CITY-STATE (S1:4) = 'GUAM'
              MOVE 'GU  '              TO AT-CITY-STATE (S1:4)
              DISPLAY ' FIXED GUAM ' WS-HOLD-CITY-STATE ' '
                 AT-CITY-STATE
              MOVE AT-CITY-STATE       TO WS-HOLD-CITY-STATE
           END-IF

           PERFORM VARYING S1 FROM +30 BY -1 UNTIL
              (AT-CITY-STATE (S1:1) NOT = SPACE AND '.')
              OR (S1 = +1)
           END-PERFORM
           IF S1 > 3
              SUBTRACT +1 FROM S1
              MOVE AT-CITY-STATE (S1:2)
                                       TO WS-WORK-STATE
              SUBTRACT +1 FROM S1
              PERFORM VARYING S1 FROM S1 BY -1 UNTIL
                 AT-CITY-STATE (S1:1) NOT = SPACE
              END-PERFORM
              IF S1 > 2
                 IF AT-CITY-STATE (S1:1) = ',' OR ';' OR '.'
                    MOVE AT-CITY-STATE (1:S1 - 1) TO WS-WORK-CITY
                 ELSE
                    MOVE AT-CITY-STATE (1:S1) TO WS-WORK-CITY
                 END-IF
              END-IF
              MOVE SPACE               TO WS-STATE-SW
              PERFORM 1500-EDIT-STATE  THRU 1500-EXIT
              IF VALID-STATE
                 MOVE WS-WORK-CITY     TO AT-CITY
                 MOVE WS-WORK-STATE    TO AT-STATE
                 PERFORM 2100-REWRITE-ELTRLR
                                       THRU 2100-EXIT
              ELSE
                 PERFORM 1300-READ-ELMSTR
                                       THRU 1300-EXIT
              END-IF
           ELSE
              DISPLAY ' WEIRD CITY STATE ' AT-CONTROL-PRIMARY (2:19)
                 ' ' AT-SEQUENCE-NO ' ' AT-CITY-STATE
           END-IF


           .
       1000-EXIT.
           EXIT.

       1100-START-ELTRLR.

           MOVE LOW-VALUES             TO AT-CONTROL-PRIMARY
           MOVE X'04'                  TO AT-COMPANY-CD

           START ELTRLR KEY IS NOT <
                            AT-CONTROL-PRIMARY
           IF ELTRLR-FILE-STATUS NOT = '00'
              DISPLAY ' ELTRLR, BAD START '
                    ELTRLR-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .

       1100-EXIT.
           EXIT.

       1200-READ-ELTRLR.

           READ ELTRLR NEXT RECORD

           IF ELTRLR-FILE-STATUS = '10' OR '23'
              SET END-OF-ELTRLR TO TRUE
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 DISPLAY 'ELTRLR, BAD READ NEXT '
                      ELTRLR-FILE-STATUS
                 PERFORM 9999-ABEND-RTN
              ELSE
                 ADD 1              TO WS-ELTRLR-RECS-IN
              END-IF
           END-IF

           .
       1200-EXIT.
           EXIT.

       1300-READ-ELMSTR.

           MOVE AT-CONTROL-PRIMARY (1:20)
                                       TO CL-CONTROL-PRIMARY

           READ ELMSTR

           IF (ELMSTR-FILE-STATUS = '00')
              IF CLAIM-IS-OPEN
                 DISPLAY 'OPEN CLAIM WITH INVALID STATE CODE: '
                 WS-WORK-STATE
                 ' ' WS-HOLD-CITY-STATE ' ' CL-CONTROL-PRIMARY (2:19)
              ELSE
                 DISPLAY 'INVALID STATE CODE: ' WS-WORK-STATE
                 ' ' WS-HOLD-CITY-STATE ' ' CL-CONTROL-PRIMARY (2:19)
              END-IF
           ELSE
              DISPLAY ' CLAIM NOT FOUND ' CL-CONTROL-PRIMARY (2:19)
           END-IF

           .

       1300-EXIT.
           EXIT.

       1500-EDIT-STATE.

           SEARCH ALL STATE-TABLE AT END
                 ADD 1 TO WS-INVALID-ST-CNT
              WHEN ST-STATE (ST-INDEX) = WS-WORK-STATE
                 SET VALID-STATE TO TRUE
           END-SEARCH

           .
       1500-EXIT.
           EXIT.

       2100-REWRITE-ELTRLR.

      *    DISPLAY ' OLD ' WS-HOLD-CITY-STATE ' NEW ' WS-WORK-CITY-ST
PEMTST     REWRITE ACTIVITY-TRAILERS
PEMTST*    MOVE '00' TO ELTRLR-FILE-STATUS

           IF ELTRLR-FILE-STATUS = '00'
               ADD 1                   TO WS-ELTRLR-RECS-FIX
           ELSE
               DISPLAY 'ELTRLR, BAD REWRITE '
               DISPLAY '*** STATUS CODE IS ' ELTRLR-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF
           .
       2100-EXIT.
           EXIT.

       2200-DELETE-ELTRLR.

           DELETE  ELTRLR

           IF ELTRLR-FILE-STATUS = '00'
               ADD 1                   TO WS-ELTRLR-RECS-DEL
           ELSE
               DISPLAY 'ELTRLR, BAD DELETE  '
               DISPLAY '*** STATUS CODE IS ' ELTRLR-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF
           .
       2200-EXIT.
           EXIT.


       3000-CLOSE-FILES.

           CLOSE ELTRLR ELMSTR

           .
       3000-EXIT.
           EXIT.

       4000-FINAL-TOTALS.

           DISPLAY '**************************************************'
           DISPLAY '***                                            ***'

           MOVE WS-ELTRLR-RECS-IN      TO WS-DISPLAY-CNT
           DISPLAY '***  TRLR RECORDS     IN         = ' WS-DISPLAY-CNT

           MOVE WS-ELTRLR-RECS-FIX     TO WS-DISPLAY-CNT
           DISPLAY '***  TRLR RECORDS     FIXED      = ' WS-DISPLAY-CNT

           MOVE WS-INVALID-ST-CNT      TO WS-DISPLAY-CNT
           DISPLAY '***  INVALID STATES              = ' WS-DISPLAY-CNT

           MOVE WS-ELTRLR-RECS-DEL     TO WS-DISPLAY-CNT
           DISPLAY '***  TRLR RECORDS     DELETED    = ' WS-DISPLAY-CNT


           DISPLAY '***                                            ***'
           DISPLAY '**************************************************'
           .
       4000-EXIT.
           EXIT.

       9999-ABEND-RTN.

           DISPLAY '*** ELTRLR  FILE CONVERSION PROGRAM ABENDING ***'
           DISPLAY '************************************************'

           MOVE 1                      TO WS-ABEND-FLD
           MOVE 0                      TO WS-ZERO-FLD

           COMPUTE WS-ABEND-FLD = WS-ABEND-FLD / WS-ZERO-FLD
           .
       9999-EXIT.
           EXIT.
