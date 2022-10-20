       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMBEF1.
       AUTHOR.     PABLO.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ZIP-FILE-IN          ASSIGN TO SYS010
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ELBENE           ASSIGN TO ELBENE
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS BE-CONTROL-PRIMARY
                                   FILE STATUS IS ELBENE-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  ZIP-FILE-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F. 

       01  ZIP-IN-REC.
           05  ZIP-CODE               PIC X(05).
           05  ZIP-STATE              PIC XX.
           05  FILLER                 PIC X.
           05  ZIP-CITY               PIC X(34).
           05  FILLER                 PIC X.

       FD  ELBENE.

                                       COPY ELCBENE.

       WORKING-STORAGE SECTION.
       77  S1                          PIC S9(3) COMP-3 VALUE +0.
       77  WS-STATE-SW                 PIC X  VALUE ' '.
           88  VALID-STATE               VALUE 'Y'.
       77  WS-DIS-EFF-DT               PIC X(10)  VALUE SPACES.
       77  WS-DIS-EXP-DT               PIC X(10)  VALUE SPACES.
       77  WS-REWRITE-SW               PIC X      VALUE SPACES.
           88  REWRITE-BE                 VALUE 'Y'.
       77  WS-ADDR-SW                  PIC X VALUE ' '.
           88  INS-ADDR                  VALUE '1'.
           88  BENE-ADDR                 VALUE '2'.
       77  WS-TABLE-EOF-SW             PIC X  VALUE SPACES.
           88  END-OF-TABLE               VALUE 'Y'.
       77  WS-INS-FIX                  PIC X  VALUE ' '.
           88  INS-FIXED                VALUE 'Y'.
       77  WS-BEN-FIX                  PIC X  VALUE ' '.
           88  BEN-FIXED                VALUE 'Y'.
       01  WS-STATUS-CODES.
           05  ELBENE-FILE-STATUS      PIC XX  VALUE SPACES.
           05  WS-EOF-SW               PIC X   VALUE SPACES.
               88  END-OF-ELBENE               VALUE 'Y'.
               88  MORE-ELBENE                 VALUE ' '.

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


       01  Z1                          PIC S9(5) COMP-3 VALUE +0.
       01  FILLER.
           05  WS-ZIP-TABLE OCCURS 42078.
               10  WS-ZIP-CODE         PIC X(5).
               10  WS-ZIP-STATE        PIC XX.
               10  WS-ZIP-CITY         PIC X(28).




       01  WS-HOLD-CITY-STATE          PIC X(30)  VALUE SPACES.
       01  WS-WORK-CITY-ST.
           05  WS-WORK-CITY            PIC X(28)  VALUE SPACES.
           05  WS-WORK-STATE           PIC XX     VALUE SPACES.
       01  WS-HOLD-ELBENE              PIC X(374) VALUE LOW-VALUES.
       01  WS-WORK-FIELDS.
           05  WS-ABEND-FLD            PIC 9         VALUE ZEROS.
           05  WS-ZERO-FLD             PIC 9         VALUE ZEROS.
           05  WS-ELBENE-RECS-IN       PIC 9(9)      VALUE ZEROS.
           05  WS-ELBENE-RECS-FIX      PIC 9(9)      VALUE ZEROS.
           05  WS-ELBENE-RECS-DEL      PIC 9(9)      VALUE ZEROS.
           05  WS-DISPLAY-CNT          PIC Z,ZZZ,ZZ9 VALUE ZEROS.
           05  WS-INVALID-ST-CNT       PIC 9(9)      VALUE ZEROS.

       PROCEDURE DIVISION.
       0000-MAINLINE.

           PERFORM 0100-OPEN-FILES     THRU 0100-EXIT
           PERFORM 0200-INITIALIZE     THRU 0200-EXIT
           PERFORM 0300-BUILD-TABLE    THRU 0300-EXIT
           PERFORM 0500-PROCESS        THRU 0500-EXIT UNTIL
              END-OF-ELBENE
PEMTST*       OR WS-ELBENE-RECS-FIX > 10000
           PERFORM 3000-CLOSE-FILES    THRU 3000-EXIT
           PERFORM 4000-FINAL-TOTALS   THRU 4000-EXIT
           GOBACK

           .
       0000-EXIT.
           EXIT.

       0100-OPEN-FILES.

PEMTST     OPEN I-O   ELBENE
PEMTST*    OPEN INPUT ELBENE

           OPEN INPUT ZIP-FILE-IN

           IF ELBENE-FILE-STATUS = '00'
               CONTINUE
           ELSE
               DISPLAY '*** ERROR OPENING ELBENE FILE ***'
               DISPLAY '*** STATUS CODE IS ' ELBENE-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-INITIALIZE.

           MOVE SPACES                 TO WS-EOF-SW
           MOVE ZEROS                  TO WS-ELBENE-RECS-IN
                                          WS-ELBENE-RECS-FIX
                                          WS-ELBENE-RECS-DEL

           PERFORM 1100-START-ELBENE   THRU 1100-EXIT

           PERFORM 1200-READ-ELBENE    THRU 1200-EXIT

           .
       0200-EXIT.
           EXIT.

       0300-BUILD-TABLE.

           PERFORM VARYING Z1 FROM +1 BY +1 UNTIL
              END-OF-TABLE
              READ ZIP-FILE-IN AT END
                 SET END-OF-TABLE      TO TRUE
              END-READ
              IF NOT END-OF-TABLE
                 MOVE ZIP-CODE         TO WS-ZIP-CODE  (Z1)
                 MOVE ZIP-STATE        TO WS-ZIP-STATE (Z1)
                 MOVE ZIP-CITY         TO WS-ZIP-CITY  (Z1)
              END-IF
           END-PERFORM

           SUBTRACT +2                 FROM Z1

           DISPLAY ' NUMBER OF ZIP RECORDS ' Z1

           .
       0300-EXIT.
           EXIT.

       0500-PROCESS.

           IF (BE-CITY-STATE NOT = SPACES)
              OR (BE-CITY-STATE2 NOT = SPACES)
              PERFORM 1000-PROCESS     THRU 1000-EXIT
           END-IF

           PERFORM 1200-READ-ELBENE    THRU 1200-EXIT

           .
       0500-EXIT.
           EXIT.

       1000-PROCESS.

           MOVE ' '                    TO WS-REWRITE-SW
           IF BE-CITY-STATE NOT = SPACES
              MOVE SPACES              TO WS-WORK-CITY-ST
              MOVE BE-CITY-STATE       TO WS-HOLD-CITY-STATE
   
      ** CHECK FOR GUAM AND CHANGE TO GU **
              PERFORM VARYING S1 FROM +27 BY -1 UNTIL
                 (S1 = +1)
                 OR (BE-CITY-STATE (S1:4) = 'GUAM')
              END-PERFORM
              IF BE-CITY-STATE (S1:4) = 'GUAM'
                 MOVE 'GU  '              TO BE-CITY-STATE (S1:4)
                 DISPLAY ' FIXED GUAM ' WS-HOLD-CITY-STATE ' '
                    BE-CITY-STATE
                 MOVE BE-CITY-STATE       TO WS-HOLD-CITY-STATE
              END-IF

              PERFORM VARYING S1 FROM +30 BY -1 UNTIL
                 (BE-CITY-STATE (S1:1) NOT = SPACE AND '.')
                 OR (S1 = +1)
              END-PERFORM
              IF S1 > 3
                 SUBTRACT +1 FROM S1
                 MOVE BE-CITY-STATE (S1:2)
                                          TO WS-WORK-STATE
                 SUBTRACT +1 FROM S1
                 PERFORM VARYING S1 FROM S1 BY -1 UNTIL
                    BE-CITY-STATE (S1:1) NOT = SPACE
                 END-PERFORM
                 IF S1 > 2
                    IF BE-CITY-STATE (S1:1) = ',' OR ';' OR '.'
                       MOVE BE-CITY-STATE (1:S1 - 1) TO WS-WORK-CITY
                    ELSE
                       MOVE BE-CITY-STATE (1:S1) TO WS-WORK-CITY
                    END-IF
                 END-IF
                 MOVE SPACE            TO WS-STATE-SW
                 MOVE '1'              TO WS-ADDR-SW
                 PERFORM 1500-EDIT-STATE  THRU 1500-EXIT
                 IF VALID-STATE
                    MOVE WS-WORK-CITY  TO BE-CITY
                    MOVE WS-WORK-STATE TO BE-STATE
                    DISPLAY ' 1  OLD ' WS-HOLD-CITY-STATE
                       ' NEW ' BE-CITY-STATE
                    SET REWRITE-BE TO TRUE
      *          ELSE
      *             PERFORM 1650-CONVERT-DATES
      *                                THRU 1650-EXIT
      *             DISPLAY ' 1 INVALID STATE CODE: ' WS-WORK-STATE
      *                ' ' WS-HOLD-CITY-STATE ' '
      *                   BE-CONTROL-PRIMARY (2:11)
                 END-IF
              ELSE
                 DISPLAY ' 1 WEIRD CITY STATE ' WS-HOLD-CITY-STATE ' '
                          BE-CONTROL-PRIMARY (2:11)
              END-IF
           END-IF

           IF BE-CITY-STATE2 NOT = SPACES
              MOVE SPACES              TO WS-WORK-CITY-ST
              MOVE BE-CITY-STATE2      TO WS-HOLD-CITY-STATE
   
      ** CHECK FOR GUAM AND CHANGE TO GU **
              PERFORM VARYING S1 FROM +27 BY -1 UNTIL
                 (S1 = +1)
                 OR (BE-CITY-STATE2 (S1:4) = 'GUAM')
              END-PERFORM
              IF BE-CITY-STATE2 (S1:4) = 'GUAM'
                 MOVE 'GU  '              TO BE-CITY-STATE2 (S1:4)
                 DISPLAY ' FIXED GUAM ' WS-HOLD-CITY-STATE ' '
                    BE-CITY-STATE2
                 MOVE BE-CITY-STATE2      TO WS-HOLD-CITY-STATE
              END-IF

              PERFORM VARYING S1 FROM +30 BY -1 UNTIL
                 (BE-CITY-STATE2 (S1:1) NOT = SPACE AND '.')
                 OR (S1 = +1)
              END-PERFORM
              IF S1 > 3
                 SUBTRACT +1 FROM S1
                 MOVE BE-CITY-STATE2 (S1:2)
                                          TO WS-WORK-STATE
                 SUBTRACT +1 FROM S1
                 PERFORM VARYING S1 FROM S1 BY -1 UNTIL
                    BE-CITY-STATE2 (S1:1) NOT = SPACE
                 END-PERFORM
                 IF S1 > 2
                    IF BE-CITY-STATE2 (S1:1) = ',' OR ';' OR '.'
                       MOVE BE-CITY-STATE2 (1:S1 - 1) TO WS-WORK-CITY
                    ELSE
                       MOVE BE-CITY-STATE2 (1:S1) TO WS-WORK-CITY
                    END-IF
                 END-IF
                 MOVE SPACE            TO WS-STATE-SW
                 MOVE '2'              TO WS-ADDR-SW
                 PERFORM 1500-EDIT-STATE  THRU 1500-EXIT
                 IF VALID-STATE
                    MOVE WS-WORK-CITY  TO BE-CITY2
                    MOVE WS-WORK-STATE TO BE-STATE2
                    DISPLAY ' 2  OLD ' WS-HOLD-CITY-STATE
                       ' NEW ' BE-CITY-STATE2
                    SET REWRITE-BE TO TRUE
      *          ELSE
      *             PERFORM 1650-CONVERT-DATES
      *                                THRU 1650-EXIT
      *             DISPLAY ' 2 INVALID STATE CODE: ' WS-WORK-STATE
      *                ' ' WS-HOLD-CITY-STATE ' '
      *                   BE-CONTROL-PRIMARY (2:11)
                 END-IF
              ELSE
                 DISPLAY ' 2 WEIRD CITY STATE ' WS-HOLD-CITY-STATE ' '
                          BE-CONTROL-PRIMARY (2:11)
              END-IF
           END-IF

           IF REWRITE-BE
              PERFORM 2100-REWRITE-ELBENE
                                          THRU 2100-EXIT
           END-IF

           .
       1000-EXIT.
           EXIT.

       1100-START-ELBENE.

           MOVE LOW-VALUES             TO BE-CONTROL-PRIMARY
           MOVE X'04'                  TO BE-COMPANY-CD

           START ELBENE KEY IS NOT <
                            BE-CONTROL-PRIMARY
           IF ELBENE-FILE-STATUS NOT = '00'
              DISPLAY ' ELBENE, BAD START '
                    ELBENE-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       1100-EXIT.
           EXIT.

       1200-READ-ELBENE.

           READ ELBENE NEXT RECORD

           IF ELBENE-FILE-STATUS = '10' OR '23'
              SET END-OF-ELBENE TO TRUE
           ELSE
              IF ELBENE-FILE-STATUS NOT = '00'
                 DISPLAY 'ELBENE, BAD READ NEXT '
                      ELBENE-FILE-STATUS
                 PERFORM 9999-ABEND-RTN
              ELSE
                 ADD 1              TO WS-ELBENE-RECS-IN
              END-IF
           END-IF

           .
       1200-EXIT.
           EXIT.

       1500-EDIT-STATE.

           SEARCH ALL STATE-TABLE AT END
              ADD 1 TO WS-INVALID-ST-CNT
              WHEN ST-STATE (ST-INDEX) = WS-WORK-STATE
                 SET VALID-STATE TO TRUE
           END-SEARCH

           IF NOT VALID-STATE
              PERFORM 1700-TRY-TO-FIX  THRU 1700-EXIT
           END-IF

           .
       1500-EXIT.
           EXIT.

       1700-TRY-TO-FIX.

           IF WS-ADDR-SW = '1'
              IF (BE-ZIP-PRIME NUMERIC)
                 AND (BE-ZIP-PRIME NOT = ZEROS)
                 PERFORM VARYING Z1 FROM +1 BY +1 UNTIL
                    (Z1 > 42078)
                    OR (BE-ZIP-PRIME = WS-ZIP-CODE (Z1))
                 END-PERFORM
                 IF BE-ZIP-PRIME = WS-ZIP-CODE (Z1)
                    DISPLAY ' 1 - FIXING CITY ST FROM '
                       WS-HOLD-CITY-STATE
                       ' TO ' WS-ZIP-CITY (Z1) ' ' WS-ZIP-STATE (Z1) ' '
                       BE-CONTROL-PRIMARY (2:11)
                    MOVE WS-ZIP-CITY (Z1) TO BE-CITY
                    MOVE WS-ZIP-STATE (Z1) TO BE-STATE
                    SET REWRITE-BE TO TRUE
                 ELSE
                    DISPLAY ' 1 - INVALID STATE CODE: ' WS-WORK-STATE
                    ' ' WS-HOLD-CITY-STATE ' '
                    BE-CONTROL-PRIMARY (2:11)
                 END-IF
              ELSE
                 MOVE ' ' TO WS-INS-FIX

      ** CHECK FOR SACRAMENTO            **
                 PERFORM VARYING S1 FROM +21 BY -1 UNTIL
                    (S1 = +1)
                    OR (BE-CITY-STATE (S1:10) = 'SACRAMENTO')
                 END-PERFORM
                 IF BE-CITY-STATE (S1:10) = 'SACRAMENTO'
                    MOVE 'CA'                TO BE-STATE
                    DISPLAY ' INS FIXED SACRAMENTO ' WS-HOLD-CITY-STATE
                       ' ' BE-CITY-STATE
                    MOVE BE-CITY-STATE       TO WS-HOLD-CITY-STATE
                    SET REWRITE-BE TO TRUE
                    SET INS-FIXED TO TRUE
                 END-IF

      ** CHECK FOR CHARLOTTE             **
                 PERFORM VARYING S1 FROM +22 BY -1 UNTIL
                    (S1 = +1)
                    OR (BE-CITY-STATE (S1:9) = 'CHARLOTTE')
                 END-PERFORM
                 IF BE-CITY-STATE (S1:9) = 'CHARLOTTE'
                    MOVE 'NC'                TO BE-STATE
                    DISPLAY ' 1 FIXED CHARLOTTE  ' WS-HOLD-CITY-STATE
                      ' ' BE-CITY-STATE
                    MOVE BE-CITY-STATE       TO WS-HOLD-CITY-STATE
                    SET REWRITE-BE TO TRUE
                    SET INS-FIXED TO TRUE
                 END-IF
                 IF NOT INS-FIXED
                    DISPLAY '1 - INVALID ZIP CANT FIX ' WS-WORK-STATE
                       ' ' WS-HOLD-CITY-STATE ' '
                       BE-CONTROL-PRIMARY (2:11) ' ' BE-ZIP-PRIME
                 END-IF
              END-IF
           END-IF


           IF WS-ADDR-SW = '2'
              IF (BE-ZIP-PRIME2 NUMERIC)
                 AND (BE-ZIP-PRIME2 NOT = ZEROS)
                 PERFORM VARYING Z1 FROM +1 BY +1 UNTIL
                    (Z1 > 42078)
                    OR (BE-ZIP-PRIME2 = WS-ZIP-CODE (Z1))
                 END-PERFORM
                 IF BE-ZIP-PRIME2 = WS-ZIP-CODE (Z1)
                    DISPLAY ' BEN - FIXING CITY ST FROM '
                       WS-HOLD-CITY-STATE
                       ' TO ' WS-ZIP-CITY (Z1) ' ' WS-ZIP-STATE (Z1) ' '
                       BE-CONTROL-PRIMARY (2:11)
                    MOVE WS-ZIP-CITY (Z1) TO BE-CITY2
                    MOVE WS-ZIP-STATE (Z1) TO BE-STATE2
                    SET REWRITE-BE TO TRUE
                 ELSE
                    DISPLAY ' 2 - INVALID STATE CODE: ' WS-WORK-STATE
                    ' ' WS-HOLD-CITY-STATE ' '
                    BE-CONTROL-PRIMARY (2:11)
                 END-IF
              ELSE
                 MOVE ' ' TO WS-BEN-FIX

      ** CHECK FOR SACRAMENTO            **
                 PERFORM VARYING S1 FROM +21 BY -1 UNTIL
                    (S1 = +1)
                    OR (BE-CITY-STATE2 (S1:10) = 'SACRAMENTO')
                 END-PERFORM
                 IF BE-CITY-STATE2 (S1:10) = 'SACRAMENTO'
                    MOVE 'CA'                TO BE-STATE2
                    DISPLAY ' 2 FIXED SACRAMENTO ' WS-HOLD-CITY-STATE
                      ' ' BE-CITY-STATE2
                    MOVE BE-CITY-STATE2  TO WS-HOLD-CITY-STATE
                    SET REWRITE-BE TO TRUE
                    SET BEN-FIXED TO TRUE
                 END-IF

      ** CHECK FOR CHARLOTTE             **
                 PERFORM VARYING S1 FROM +22 BY -1 UNTIL
                    (S1 = +1)
                    OR (BE-CITY-STATE2 (S1:9) = 'CHARLOTTE')
                 END-PERFORM
                 IF BE-CITY-STATE2 (S1:9) = 'CHARLOTTE'
                    MOVE 'NC'                TO BE-STATE2
                    DISPLAY ' 2 FIXED CHARLOTTE  ' WS-HOLD-CITY-STATE
                    ' ' BE-CITY-STATE2
                    MOVE BE-CITY-STATE2 TO WS-HOLD-CITY-STATE
                    SET REWRITE-BE TO TRUE
                    SET BEN-FIXED TO TRUE
                 END-IF
                 IF NOT BEN-FIXED
                    DISPLAY ' 2 - INVALID ZIP CANT FIX ' WS-WORK-STATE
                       ' ' WS-HOLD-CITY-STATE ' '
                       BE-CONTROL-PRIMARY (2:11) ' ' BE-ZIP-PRIME2
                 END-IF
              END-IF
           END-IF

           .
       1700-EXIT.
           EXIT.

       2100-REWRITE-ELBENE.

PEMTST     REWRITE BENEFICIARY-MASTER
PEMTST*    MOVE '00' TO ELBENE-FILE-STATUS

           IF ELBENE-FILE-STATUS = '00'
               ADD 1                   TO WS-ELBENE-RECS-FIX
           ELSE
               DISPLAY 'ELBENE, BAD REWRITE '
               DISPLAY '*** STATUS CODE IS ' ELBENE-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF
           .
       2100-EXIT.
           EXIT.

       2200-DELETE-ELBENE.

           DELETE  ELBENE

           IF ELBENE-FILE-STATUS = '00'
               ADD 1                   TO WS-ELBENE-RECS-DEL
           ELSE
               DISPLAY 'ELBENE, BAD DELETE  '
               DISPLAY '*** STATUS CODE IS ' ELBENE-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF
           .
       2200-EXIT.
           EXIT.


       3000-CLOSE-FILES.

           CLOSE ELBENE ZIP-FILE-IN

           .
       3000-EXIT.
           EXIT.

       4000-FINAL-TOTALS.

           DISPLAY '**************************************************'
           DISPLAY '***                                            ***'

           MOVE WS-ELBENE-RECS-IN      TO WS-DISPLAY-CNT
           DISPLAY '***  ELBENE RECORDS   IN         = ' WS-DISPLAY-CNT

           MOVE WS-ELBENE-RECS-FIX     TO WS-DISPLAY-CNT
           DISPLAY '***  ELBENE RECORDS   FIXED      = ' WS-DISPLAY-CNT

           MOVE WS-INVALID-ST-CNT      TO WS-DISPLAY-CNT
           DISPLAY '***  INVALID STATES              = ' WS-DISPLAY-CNT

           MOVE WS-ELBENE-RECS-DEL     TO WS-DISPLAY-CNT
           DISPLAY '***  ELBENE RECORDS   DELETED    = ' WS-DISPLAY-CNT

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

           DISPLAY '*** ELBENE  FILE CONVERSION PROGRAM ABENDING ***'
           DISPLAY '************************************************'

           MOVE 1                      TO WS-ABEND-FLD
           MOVE 0                      TO WS-ZERO-FLD

           COMPUTE WS-ABEND-FLD = WS-ABEND-FLD / WS-ZERO-FLD
           .
       9999-EXIT.
           EXIT.
