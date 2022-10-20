       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMMAF1.
       AUTHOR.     PABLO.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ZIP-FILE-IN          ASSIGN TO SYS010
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ERMAIL           ASSIGN TO ERMAIL
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS MA-CONTROL-PRIMARY
                                   FILE STATUS IS ERMAIL-FILE-STATUS.

           SELECT ELCERT           ASSIGN TO ELCERT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CM-CONTROL-PRIMARY
                                   FILE STATUS IS ELCERT-FILE-STATUS.

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

       FD  ERMAIL.

                                       COPY ERCMAIL.

       FD  ELCERT.

                                       COPY ELCCERT.

       WORKING-STORAGE SECTION.
       77  S1                          PIC S9(3) COMP-3 VALUE +0.
       77  WS-STATE-SW                 PIC X  VALUE ' '.
           88  VALID-STATE               VALUE 'Y'.
       77  WS-DIS-EFF-DT               PIC X(10)  VALUE SPACES.
       77  WS-DIS-EXP-DT               PIC X(10)  VALUE SPACES.
       77  WS-REWRITE-SW               PIC X      VALUE SPACES.
           88  REWRITE-MA                 VALUE 'Y'.
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
           05  ERMAIL-FILE-STATUS      PIC XX  VALUE SPACES.
           05  ELCERT-FILE-STATUS      PIC XX  VALUE SPACES.
           05  WS-EOF-SW               PIC X   VALUE SPACES.
               88  END-OF-ERMAIL               VALUE 'Y'.
               88  MORE-ERMAIL                 VALUE ' '.

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
       01  WS-HOLD-ERMAIL              PIC X(374) VALUE LOW-VALUES.
       01  WS-WORK-FIELDS.
           05  WS-ABEND-FLD            PIC 9         VALUE ZEROS.
           05  WS-ZERO-FLD             PIC 9         VALUE ZEROS.
           05  WS-ERMAIL-RECS-IN       PIC 9(9)      VALUE ZEROS.
           05  WS-ERMAIL-RECS-FIX      PIC 9(9)      VALUE ZEROS.
           05  WS-ERMAIL-RECS-DEL      PIC 9(9)      VALUE ZEROS.
           05  WS-DISPLAY-CNT          PIC Z,ZZZ,ZZ9 VALUE ZEROS.
           05  WS-INVALID-ST-CNT       PIC 9(9)      VALUE ZEROS.

       PROCEDURE DIVISION.
       0000-MAINLINE.

           PERFORM 0100-OPEN-FILES     THRU 0100-EXIT
           PERFORM 0200-INITIALIZE     THRU 0200-EXIT
           PERFORM 0300-BUILD-TABLE    THRU 0300-EXIT
           PERFORM 0500-PROCESS        THRU 0500-EXIT UNTIL
              END-OF-ERMAIL
PEMTST*       OR WS-ERMAIL-RECS-FIX > 10000
           PERFORM 3000-CLOSE-FILES    THRU 3000-EXIT
           PERFORM 4000-FINAL-TOTALS   THRU 4000-EXIT
           GOBACK

           .
       0000-EXIT.
           EXIT.

       0100-OPEN-FILES.

PEMTST     OPEN I-O   ERMAIL
PEMTST*    OPEN INPUT ERMAIL

           OPEN INPUT ELCERT ZIP-FILE-IN

           IF ERMAIL-FILE-STATUS = '00'
               CONTINUE
           ELSE
               DISPLAY '*** ERROR OPENING ERMAIL FILE ***'
               DISPLAY '*** STATUS CODE IS ' ERMAIL-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF

           IF ELCERT-FILE-STATUS = '00'
               CONTINUE
           ELSE
               DISPLAY '*** ERROR OPENING ELCERT FILE ***'
               DISPLAY '*** STATUS CODE IS ' ELCERT-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-INITIALIZE.

           MOVE SPACES                 TO WS-EOF-SW
           MOVE ZEROS                  TO WS-ERMAIL-RECS-IN
                                          WS-ERMAIL-RECS-FIX
                                          WS-ERMAIL-RECS-DEL

           PERFORM 1100-START-ERMAIL   THRU 1100-EXIT

           PERFORM 1200-READ-ERMAIL    THRU 1200-EXIT

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

           IF (MA-CITY-STATE NOT = SPACES)
              OR (MA-CRED-BENE-CTYST NOT = SPACES)
              PERFORM 1000-PROCESS     THRU 1000-EXIT
           END-IF

           PERFORM 1200-READ-ERMAIL    THRU 1200-EXIT

           .
       0500-EXIT.
           EXIT.

       1000-PROCESS.

           MOVE ' '                    TO WS-REWRITE-SW
           IF MA-CITY-STATE NOT = SPACES
              MOVE SPACES              TO WS-WORK-CITY-ST
              MOVE MA-CITY-STATE       TO WS-HOLD-CITY-STATE
   
      ** CHECK FOR GUAM AND CHANGE TO GU **
              PERFORM VARYING S1 FROM +27 BY -1 UNTIL
                 (S1 = +1)
                 OR (MA-CITY-STATE (S1:4) = 'GUAM')
              END-PERFORM
              IF MA-CITY-STATE (S1:4) = 'GUAM'
                 MOVE 'GU  '              TO MA-CITY-STATE (S1:4)
                 DISPLAY ' FIXED GUAM ' WS-HOLD-CITY-STATE ' '
                    MA-CITY-STATE
                 MOVE MA-CITY-STATE       TO WS-HOLD-CITY-STATE
              END-IF

              PERFORM VARYING S1 FROM +30 BY -1 UNTIL
                 (MA-CITY-STATE (S1:1) NOT = SPACE AND '.')
                 OR (S1 = +1)
              END-PERFORM
              IF S1 > 3
                 SUBTRACT +1 FROM S1
                 MOVE MA-CITY-STATE (S1:2)
                                          TO WS-WORK-STATE
                 SUBTRACT +1 FROM S1
                 PERFORM VARYING S1 FROM S1 BY -1 UNTIL
                    MA-CITY-STATE (S1:1) NOT = SPACE
                 END-PERFORM
                 IF S1 > 2
                    IF MA-CITY-STATE (S1:1) = ',' OR ';' OR '.'
                       MOVE MA-CITY-STATE (1:S1 - 1) TO WS-WORK-CITY
                    ELSE
                       MOVE MA-CITY-STATE (1:S1) TO WS-WORK-CITY
                    END-IF
                 END-IF
                 MOVE SPACE            TO WS-STATE-SW
                 MOVE '1'              TO WS-ADDR-SW
                 PERFORM 1500-EDIT-STATE  THRU 1500-EXIT
                 IF VALID-STATE
                    MOVE WS-WORK-CITY  TO MA-CITY
                    MOVE WS-WORK-STATE TO MA-ADDR-STATE
      *             DISPLAY ' MA OLD ' WS-HOLD-CITY-STATE
      *                ' NEW ' MA-CITY-STATE
                    SET REWRITE-MA TO TRUE
      *          ELSE
      *             PERFORM 1650-CONVERT-DATES
      *                                THRU 1650-EXIT
      *             DISPLAY 'INVALID STATE CODE: ' WS-WORK-STATE
      *                ' ' WS-HOLD-CITY-STATE ' '
      *                   MA-CONTROL-PRIMARY (2:19) ' ' WS-DIS-EFF-DT
      *                   ' ' MA-CERT-NO
                 END-IF
              ELSE
                 PERFORM 1600-READ-ELCERT THRU 1600-EXIT
      *          DISPLAY ' WEIRD CITY STATE ' WS-HOLD-CITY-STATE ' '
      *                   MA-CONTROL-PRIMARY (2:19) ' ' WS-DIS-EFF-DT
      *                   ' ' MA-CERT-NO
              END-IF
           END-IF


           IF MA-CRED-BENE-CTYST NOT = SPACES
              MOVE SPACES              TO WS-WORK-CITY-ST
              MOVE MA-CRED-BENE-CTYST  TO WS-HOLD-CITY-STATE
   
      ** CHECK FOR GUAM AND CHANGE TO GU **
              PERFORM VARYING S1 FROM +27 BY -1 UNTIL
                 (S1 = +1)
                 OR (MA-CRED-BENE-CTYST (S1:4) = 'GUAM')
              END-PERFORM
              IF MA-CRED-BENE-CTYST (S1:4) = 'GUAM'
                 MOVE 'GU  '           TO MA-CRED-BENE-CTYST (S1:4)
                 DISPLAY ' FIXED GUAM ' WS-HOLD-CITY-STATE ' '
                    MA-CRED-BENE-CTYST
                 MOVE MA-CRED-BENE-CTYST  TO WS-HOLD-CITY-STATE
              END-IF
   
              PERFORM VARYING S1 FROM +30 BY -1 UNTIL
                 (MA-CRED-BENE-CTYST (S1:1) NOT = SPACE AND '.')
                 OR (S1 = +1)
              END-PERFORM
              IF S1 > 3
                 SUBTRACT +1 FROM S1
                 MOVE MA-CRED-BENE-CTYST (S1:2)
                                          TO WS-WORK-STATE
                 SUBTRACT +1 FROM S1
                 PERFORM VARYING S1 FROM S1 BY -1 UNTIL
                    MA-CRED-BENE-CTYST (S1:1) NOT = SPACE
                 END-PERFORM
                 IF S1 > 2
                    IF MA-CRED-BENE-CTYST (S1:1) = ',' OR ';' OR '.'
                       MOVE MA-CRED-BENE-CTYST (1:S1 - 1)
                                       TO WS-WORK-CITY
                    ELSE
                       MOVE MA-CRED-BENE-CTYST (1:S1)
                                       TO WS-WORK-CITY
                    END-IF
                 END-IF
                 MOVE SPACE            TO WS-STATE-SW
                 MOVE '2'              TO WS-ADDR-SW
                 PERFORM 1500-EDIT-STATE  THRU 1500-EXIT
                 IF VALID-STATE
                    MOVE WS-WORK-CITY  TO MA-CRED-BENE-CITY
                    MOVE WS-WORK-STATE TO MA-CRED-BENE-STATE
      *             DISPLAY ' CB OLD ' WS-HOLD-CITY-STATE
      *                ' NEW ' MA-CRED-BENE-CTYST
                    SET REWRITE-MA TO TRUE
      *          ELSE
      *             PERFORM 1650-CONVERT-DATES
      *                                THRU 1650-EXIT
      *             DISPLAY 'INVALID STATE CODE: ' WS-WORK-STATE
      *                ' ' WS-HOLD-CITY-STATE ' '
      *                   MA-CONTROL-PRIMARY (2:19) ' ' WS-DIS-EFF-DT
      *                   ' ' MA-CERT-NO
                 END-IF
              ELSE
                 PERFORM 1600-READ-ELCERT THRU 1600-EXIT
      *          DISPLAY ' WEIRD CITY STATE ' WS-HOLD-CITY-STATE ' '
      *                   MA-CONTROL-PRIMARY (2:19) ' ' WS-DIS-EFF-DT
      *                   ' ' MA-CERT-NO
              END-IF
           END-IF

           IF REWRITE-MA
              PERFORM 2100-REWRITE-ERMAIL
                                          THRU 2100-EXIT
           END-IF

           .
       1000-EXIT.
           EXIT.

       1100-START-ERMAIL.

           MOVE LOW-VALUES             TO MA-CONTROL-PRIMARY
           MOVE X'04'                  TO MA-COMPANY-CD

           START ERMAIL KEY IS NOT <
                            MA-CONTROL-PRIMARY
           IF ERMAIL-FILE-STATUS NOT = '00'
              DISPLAY ' ERMAIL, BAD START '
                    ERMAIL-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .

       1100-EXIT.
           EXIT.

       1200-READ-ERMAIL.

           READ ERMAIL NEXT RECORD

           IF ERMAIL-FILE-STATUS = '10' OR '23'
              SET END-OF-ERMAIL TO TRUE
           ELSE
              IF ERMAIL-FILE-STATUS NOT = '00'
                 DISPLAY 'ERMAIL, BAD READ NEXT '
                      ERMAIL-FILE-STATUS
                 PERFORM 9999-ABEND-RTN
              ELSE
                 ADD 1              TO WS-ERMAIL-RECS-IN
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
              PERFORM 1600-READ-ELCERT THRU 1600-EXIT
           END-IF

           .
       1500-EXIT.
           EXIT.

       1600-READ-ELCERT.

           MOVE MA-CONTROL-PRIMARY     TO CM-CONTROL-PRIMARY
           READ ELCERT

           IF ELCERT-FILE-STATUS = '00'
              IF (CM-LF-CANCEL-EXIT-DT = X'0000'
                 AND CM-AH-CANCEL-EXIT-DT = X'0000'
                 AND CM-LF-DEATH-EXIT-DT = X'0000'
                 AND CM-AH-SETTLEMENT-EXIT-DT = X'0000')
                         AND
                 ((CM-LF-LOAN-EXPIRE-DT NOT = X'0000' AND > X'A59F')
                         OR
                 (CM-AH-LOAN-EXPIRE-DT NOT = X'0000' AND > X'A59F'))

                 PERFORM 1650-CONVERT-DATES
                                       THRU 1650-EXIT
                 PERFORM 1700-TRY-TO-FIX THRU 1700-EXIT
      *          IF WS-ADDR-SW = '1'
      *             DISPLAY 'INS - INVALID STATE CODE: ' WS-WORK-STATE
      *             ' ' WS-HOLD-CITY-STATE ' '
      *             MA-CONTROL-PRIMARY (2:19) ' ' WS-DIS-EFF-DT
      *             ' ' MA-CERT-NO
      *          ELSE
      *             DISPLAY 'BEN - INVALID STATE CODE: ' WS-WORK-STATE
      *             ' ' WS-HOLD-CITY-STATE ' '
      *             MA-CONTROL-PRIMARY (2:19) ' ' WS-DIS-EFF-DT
      *             ' ' MA-CERT-NO
      *          END-IF
              END-IF
           ELSE
              IF ERMAIL-FILE-STATUS = '22' OR '10'
                 DISPLAY ' NO CERT RECORD ' MA-CONTROL-PRIMARY (2:19)
                    ' ' WS-DIS-EFF-DT ' ' MA-CERT-NO
              ELSE
                 IF ERMAIL-FILE-STATUS NOT = '00'
                    DISPLAY 'ELCERT, BAD READ '
                      ELCERT-FILE-STATUS
                    PERFORM 9999-ABEND-RTN
                 END-IF
              END-IF
           END-IF

           .
       1600-EXIT.
           EXIT.

       1650-CONVERT-DATES.

           MOVE MA-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO WS-DIS-EFF-DT
           ELSE
              MOVE 'XX/XX/XXXX'        TO WS-DIS-EFF-DT
              DISPLAY ' ERROR CONVERTING EFFECT  DATE ' MA-ADDR-STATE
              ' ' MA-ACCOUNT ' ' MA-CERT-NO
           END-IF

           .
       1650-EXIT.
           EXIT.

       1700-TRY-TO-FIX.

           IF WS-ADDR-SW = '1'
              IF (MA-ZIP-CODE NUMERIC)
                 AND (MA-ZIP-CODE NOT = ZEROS)
                 PERFORM VARYING Z1 FROM +1 BY +1 UNTIL
                    (Z1 > 42078)
                    OR (MA-ZIP-CODE = WS-ZIP-CODE (Z1))
                 END-PERFORM
                 IF MA-ZIP-CODE = WS-ZIP-CODE (Z1)
                    DISPLAY ' INS - FIXING CITY ST FROM '
                       WS-HOLD-CITY-STATE
                       ' TO ' WS-ZIP-CITY (Z1) ' ' WS-ZIP-STATE (Z1) ' '
                       MA-CONTROL-PRIMARY (2:19) ' ' WS-DIS-EFF-DT
                       ' ' MA-CERT-NO
                    MOVE WS-ZIP-CITY (Z1) TO MA-CITY
                    MOVE WS-ZIP-STATE (Z1) TO MA-ADDR-STATE
                    SET REWRITE-MA TO TRUE
                 ELSE
                    DISPLAY 'INS - INVALID STATE CODE: ' WS-WORK-STATE
                    ' ' WS-HOLD-CITY-STATE ' '
                    MA-CONTROL-PRIMARY (2:19) ' ' WS-DIS-EFF-DT
                    ' ' MA-CERT-NO
                 END-IF
              ELSE
                 MOVE ' ' TO WS-INS-FIX

      ** CHECK FOR SACRAMENTO            **
                 PERFORM VARYING S1 FROM +21 BY -1 UNTIL
                    (S1 = +1)
                    OR (MA-CITY-STATE (S1:10) = 'SACRAMENTO')
                 END-PERFORM
                 IF MA-CITY-STATE (S1:10) = 'SACRAMENTO'
                    MOVE 'CA'                TO MA-ADDR-STATE
                    DISPLAY ' INS FIXED SACRAMENTO ' WS-HOLD-CITY-STATE ' '
                       MA-CITY-STATE
                    MOVE MA-CITY-STATE       TO WS-HOLD-CITY-STATE
                    SET REWRITE-MA TO TRUE
                    SET INS-FIXED TO TRUE
                 END-IF

      ** CHECK FOR CHARLOTTE             **
                 PERFORM VARYING S1 FROM +22 BY -1 UNTIL
                    (S1 = +1)
                    OR (MA-CITY-STATE (S1:9) = 'CHARLOTTE')
                 END-PERFORM
                 IF MA-CITY-STATE (S1:9) = 'CHARLOTTE'
                    MOVE 'NC'                TO MA-ADDR-STATE
                    DISPLAY ' INS FIXED CHARLOTTE  ' WS-HOLD-CITY-STATE ' '
                       MA-CITY-STATE
                    MOVE MA-CITY-STATE       TO WS-HOLD-CITY-STATE
                    SET REWRITE-MA TO TRUE
                    SET INS-FIXED TO TRUE
                 END-IF
                 IF NOT INS-FIXED
                    DISPLAY 'INS - INVALID ZIP CANT FIX ' WS-WORK-STATE
                       ' ' WS-HOLD-CITY-STATE ' '
                       MA-CONTROL-PRIMARY (2:19) ' ' WS-DIS-EFF-DT
                       ' ' MA-CERT-NO ' ' MA-ZIP-CODE
                 END-IF
              END-IF
           END-IF


           IF WS-ADDR-SW = '2'
              IF (MA-CB-ZIP-CODE NUMERIC)
                 AND (MA-CB-ZIP-CODE NOT = ZEROS)
                 PERFORM VARYING Z1 FROM +1 BY +1 UNTIL
                    (Z1 > 42078)
                    OR (MA-CB-ZIP-CODE = WS-ZIP-CODE (Z1))
                 END-PERFORM
                 IF MA-CB-ZIP-CODE = WS-ZIP-CODE (Z1)
                    DISPLAY ' BEN - FIXING CITY ST FROM '
                       WS-HOLD-CITY-STATE
                       ' TO ' WS-ZIP-CITY (Z1) ' ' WS-ZIP-STATE (Z1) ' '
                       MA-CONTROL-PRIMARY (2:19) ' ' WS-DIS-EFF-DT
                       ' ' MA-CERT-NO
                    MOVE WS-ZIP-CITY (Z1) TO MA-CRED-BENE-CITY
                    MOVE WS-ZIP-STATE (Z1) TO MA-CRED-BENE-STATE
                    SET REWRITE-MA TO TRUE
                 ELSE
                    DISPLAY 'BEN - INVALID STATE CODE: ' WS-WORK-STATE
                    ' ' WS-HOLD-CITY-STATE ' '
                    MA-CONTROL-PRIMARY (2:19) ' ' WS-DIS-EFF-DT
                    ' ' MA-CERT-NO
                 END-IF
              ELSE
                 MOVE ' ' TO WS-BEN-FIX

      ** CHECK FOR SACRAMENTO            **
                 PERFORM VARYING S1 FROM +21 BY -1 UNTIL
                    (S1 = +1)
                    OR (MA-CRED-BENE-CTYST (S1:10) = 'SACRAMENTO')
                 END-PERFORM
                 IF MA-CRED-BENE-CTYST (S1:10) = 'SACRAMENTO'
                    MOVE 'CA'                TO MA-CRED-BENE-STATE
                    DISPLAY ' BEN FIXED SACRAMENTO ' WS-HOLD-CITY-STATE
                      ' ' MA-CRED-BENE-CTYST
                    MOVE MA-CRED-BENE-CTYST   TO WS-HOLD-CITY-STATE
                    SET REWRITE-MA TO TRUE
                    SET BEN-FIXED TO TRUE
                 END-IF

      ** CHECK FOR CHARLOTTE             **
                 PERFORM VARYING S1 FROM +22 BY -1 UNTIL
                    (S1 = +1)
                    OR (MA-CRED-BENE-CTYST (S1:9) = 'CHARLOTTE')
                 END-PERFORM
                 IF MA-CRED-BENE-CTYST (S1:9) = 'CHARLOTTE'
                    MOVE 'NC'                TO MA-CRED-BENE-STATE
                    DISPLAY ' BEN FIXED CHARLOTTE  ' WS-HOLD-CITY-STATE
                    ' ' MA-CRED-BENE-CTYST
                    MOVE MA-CRED-BENE-CTYST TO WS-HOLD-CITY-STATE
                    SET REWRITE-MA TO TRUE
                    SET BEN-FIXED TO TRUE
                 END-IF
                 IF NOT BEN-FIXED
                    DISPLAY 'BEN - INVALID ZIP CANT FIX ' WS-WORK-STATE
                       ' ' WS-HOLD-CITY-STATE ' '
                       MA-CONTROL-PRIMARY (2:19) ' ' WS-DIS-EFF-DT
                       ' ' MA-CERT-NO ' ' MA-CB-ZIP-CODE
                 END-IF
              END-IF
           END-IF

           .
       1700-EXIT.
           EXIT.

       2100-REWRITE-ERMAIL.

PEMTST     REWRITE MAILING-DATA
PEMTST*    MOVE '00' TO ERMAIL-FILE-STATUS

           IF ERMAIL-FILE-STATUS = '00'
               ADD 1                   TO WS-ERMAIL-RECS-FIX
           ELSE
               DISPLAY 'ERMAIL, BAD REWRITE '
               DISPLAY '*** STATUS CODE IS ' ERMAIL-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF
           .
       2100-EXIT.
           EXIT.

       2200-DELETE-ERMAIL.

           DELETE  ERMAIL

           IF ERMAIL-FILE-STATUS = '00'
               ADD 1                   TO WS-ERMAIL-RECS-DEL
           ELSE
               DISPLAY 'ERMAIL, BAD DELETE  '
               DISPLAY '*** STATUS CODE IS ' ERMAIL-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF
           .
       2200-EXIT.
           EXIT.


       3000-CLOSE-FILES.

           CLOSE ERMAIL ELCERT ZIP-FILE-IN

           .
       3000-EXIT.
           EXIT.

       4000-FINAL-TOTALS.

           DISPLAY '**************************************************'
           DISPLAY '***                                            ***'

           MOVE WS-ERMAIL-RECS-IN      TO WS-DISPLAY-CNT
           DISPLAY '***  ERMAIL RECORDS   IN         = ' WS-DISPLAY-CNT

           MOVE WS-ERMAIL-RECS-FIX     TO WS-DISPLAY-CNT
           DISPLAY '***  ERMAIL RECORDS   FIXED      = ' WS-DISPLAY-CNT

           MOVE WS-INVALID-ST-CNT      TO WS-DISPLAY-CNT
           DISPLAY '***  INVALID STATES              = ' WS-DISPLAY-CNT

           MOVE WS-ERMAIL-RECS-DEL     TO WS-DISPLAY-CNT
           DISPLAY '***  ERMAIL RECORDS   DELETED    = ' WS-DISPLAY-CNT

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

           DISPLAY '*** ERMAIL  FILE CONVERSION PROGRAM ABENDING ***'
           DISPLAY '************************************************'

           MOVE 1                      TO WS-ABEND-FLD
           MOVE 0                      TO WS-ZERO-FLD

           COMPUTE WS-ABEND-FLD = WS-ABEND-FLD / WS-ZERO-FLD
           .
       9999-EXIT.
           EXIT.
