       IDENTIFICATION DIVISION.

       PROGRAM-ID.                 ELRSC.


      *AUTHOR.     SUZAN VUKOV.
      *DATE-COMPILED.
      ******************************************************************
      *REMARKS.
      *
      *     INPUT:   ELMSTR
      *              ELTRLR
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 071802                   SMVA
      ******************************************************************

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT DISK-DATE          ASSIGN TO SYS019-FBA1-S-SYS019.

           SELECT ELMSTR-INFILE      ASSIGN TO ELMSTR
                                     ORGANIZATION IS INDEXED
                                     ACCESS IS DYNAMIC
                                     RECORD KEY IS CL-CONTROL-PRIMARY
                                     FILE STATUS IS
                                       WS-ELMSTR-FILE-STATUS.

           SELECT ELTRLR-INFILE      ASSIGN TO ELTRLR
                                     ORGANIZATION IS INDEXED
                                     ACCESS IS DYNAMIC
                                     RECORD KEY IS AT-CONTROL-PRIMARY
                                     FILE STATUS IS
                                       WS-ELTRLR-FILE-STATUS.

           SELECT PRNTR              ASSIGN TO SYS008-UR-1403-S-SYS008.

           SELECT SORT-FILE          ASSIGN TO SYS001-UT-3380-S-SORTWK1.


       DATA DIVISION.

       FILE SECTION.

       FD  DISK-DATE
                                COPY ELCDTEFD.

       FD  ELMSTR-INFILE.
                                COPY ELCMSTR.

       FD  ELTRLR-INFILE.
                                COPY ELCTRLR.


       FD  PRNTR
                                COPY ELCPRTFD.


       SD  SORT-FILE.
       01  SORT-RECORD.
           05  SORT-KEY.
               10  SORT-CLAIM-TYPE         PIC X(01).
               10  SORT-KEY-CLOSE-DT-CEN   PIC X(02).
               10  SORT-KEY-CLOSE-DT-YEAR  PIC X(02).
               10  SORT-STATE              PIC X(02).
               10  SORT-AGENT-NO           PIC X(10).
           05  SORT-CLOSE-DT               PIC X(08).
           05  SORT-CLAIM-NO               PIC X(07).
           05  SORT-CERT-NO                PIC X(11).
           05  SORT-FORM-IDS               PIC X(32).



       WORKING-STORAGE SECTION.

       77  FILLER  PIC X(32)   VALUE '********************************'.
       77  FILLER  PIC X(32)   VALUE '*   ELRSC WORKING STORAGE      *'.
       77  FILLER  PIC X(32)   VALUE '********** V/M 2.008 ***********'.

       01  FILLER                          COMP-3.
           05  WS-LINE-COUNT               PIC S9(03)    VALUE +55.
           05  WS-LINE-COUNT-MAX           PIC S9(03)    VALUE +55.
           05  WS-PAGE                     PIC S9(05)    VALUE +0.

       01  FILLER                          COMP-3.
           05  WS-DIS-RESCIND-CNT          PIC S9(05)    VALUE +0.
           05  WS-LIFE-RESCIND-CNT         PIC S9(05)    VALUE +0.
           05  WS-GRAND-TOT-RESCIND-CNT    PIC S9(05)    VALUE +0.
           05  WS-YEAR-RESCIND-CNT         PIC S9(05)    VALUE +0.

           05  WS-RETURN-CODE              PIC S9(03)    VALUE +0.
           05  WS-ZERO                     PIC S9(01)    VALUE +0.

       01  FILLER                          COMP SYNC.
           05  PGM-SUB                     PIC S9(04)    VALUE +586.
           05  WS-FORMID-SUB               PIC S9(04)    VALUE +0.
           05  WS-SAVE-SUB                 PIC S9(04)    VALUE +0.

       01  FILLER.
           05  WS-CYCLE-DT-BINARY          PIC X(02)     VALUE SPACES.

           05  WS-CUTOFF-DT-BINARY         PIC X(02)     VALUE SPACES.

           05  WS-CYCLE-DT.
               10  WS-CYCLE-DT-CC          PIC 9(02)     VALUE ZEROS.
               10  WS-CYCLE-DT-YY          PIC 9(02)     VALUE ZEROS.
               10  WS-CYCLE-DT-MM          PIC 9(02)     VALUE ZEROS.
               10  WS-CYCLE-DT-DD          PIC 9(02)     VALUE ZEROS.

           05  WS-EDITED-CYCLE-DT.
               10  WS-EDITED-CYCLE-DT-MM   PIC X(02)     VALUE SPACES.
               10  FILLER                  PIC X(01)     VALUE '/'.
               10  WS-EDITED-CYCLE-DT-DD   PIC X(02)     VALUE SPACES.
               10  FILLER                  PIC X(01)     VALUE '/'.
               10  WS-EDITED-CYCLE-DT-YY   PIC X(02)     VALUE SPACES.

           05  WS-CUTOFF-DT.
               10  WS-CUTOFF-DT-CC         PIC 9(02)     VALUE ZEROS.
               10  WS-CUTOFF-DT-YY         PIC 9(02)     VALUE ZEROS.
               10  WS-CUTOFF-DT-MM         PIC 9(02)     VALUE ZEROS.
               10  WS-CUTOFF-DT-DD         PIC 9(02)     VALUE ZEROS.

           05  WS-EDITED-CUTOFF-DT.
               10  WS-EDITED-CUTOFF-DT-MM  PIC X(02)     VALUE SPACES.
               10  FILLER                  PIC X(01)     VALUE '/'.
               10  WS-EDITED-CUTOFF-DT-DD  PIC X(02)     VALUE SPACES.
               10  FILLER                  PIC X(01)     VALUE '/'.
               10  WS-EDITED-CUTOFF-DT-YY  PIC X(02)     VALUE SPACES.

           05  WS-EOF1-SW                  PIC X(01)     VALUE SPACE.
               88  END-OF-ELMSTR                         VALUE 'Y'.

           05  WS-EOF2-SW                  PIC X(01)     VALUE SPACE.
               88  END-OF-ELTRLR                         VALUE 'Y'.

           05  WS-EOF3-SW                  PIC X(01)     VALUE SPACE.
               88  END-OF-SORTFILE                       VALUE 'Y'.

           05  WS-ELTRLR-FILE-STATUS       PIC X(02)     VALUE ZERO.
           05  WS-ELMSTR-FILE-STATUS       PIC X(02)     VALUE ZERO.

           05  WS-ELTRLR-KEY-SW            PIC X(01)     VALUE SPACE.
               88  NEW-ELTRLR-KEY                        VALUE 'N'.
               88  ELTRLR-KEY-CHANGE                     VALUE 'C'.

           05  WS-FIRST-TIME-SW            PIC X(01)     VALUE 'Y'.
               88  FIRST-SORT-RECORD                     VALUE 'Y'.
               88  NOT-FIRST-RECORD                      VALUE 'N'.

           05  WS-RESCIND-SW               PIC X(01)     VALUE SPACE.
               88  RESCISSION                            VALUE 'Y'.

           05  WS-HOLD-CLOSE-DT-YY         PIC 9(02).
           05  WS-HOLD-CLAIM-TYPE          PIC X(01)     VALUE SPACE.

           05  WS-FORMID-TABLE.
               10  WS-FORMID OCCURS 8 TIMES PIC X(06)    VALUE SPACES.

           05  WS-ABEND-MESSAGE            PIC X(80)     VALUE SPACES.
           05  WS-ABEND-FILE-STATUS        PIC X(02)     VALUE ZERO.

           05  WS-DIS-LABEL                PIC X(17)     VALUE
               'CREDIT DISABILITY'.

           05  WS-LIFE-LABEL               PIC X(17)     VALUE
               'CREDIT LIFE      '.

           05  WS-DIS-TOTAL-LABEL          PIC X(38)     VALUE
               '    CREDIT DISABILITY RESCISSIONS     '.

           05  WS-LIFE-TOTAL-LABEL         PIC X(38)     VALUE
               '          CREDIT LIFE RESCISSIONS     '.

           05  WS-GRAND-TOTAL-LABEL        PIC X(38)     VALUE
               '***** CID GRAND TOTAL RESCISSIONS     '.

           05  WS-YEAR-SUBTOT-LABEL.
               10  FILLER                  PIC X(05)     VALUE SPACES.
               10  WS-SUBTOT-CC            PIC X(02)     VALUE SPACES.
               10  WS-SUBTOT-YY            PIC X(02)     VALUE SPACES.
               10  WS-SUBTOT-LABEL-VAR     PIC X(29)     VALUE
                   ' DISABILITY RESCISSIONS      '.

           05  WS-LIFE-SUBTOT-LABEL        PIC X(29)     VALUE
                   ' CREDIT LIFE RESCISSIONS     '.

       01  WS-HEADING1.
           05  FILLER                      PIC X(01)     VALUE '1'.
           05  FILLER                      PIC X(50)     VALUE SPACES.
           05  WS-H1-TITLE                 PIC X(23)     VALUE
               'CLAIMS RESCINDED SINCE '.
           05  WS-H1-SINCE-DT              PIC X(10)     VALUE SPACES.
           05  FILLER                      PIC X(37)     VALUE SPACES.
           05  WS-H1-REPORT-ID             PIC X(05)     VALUE 'ELRSC'.
           05  FILLER                      PIC X(07)     VALUE SPACES.


       01  WS-HEADING2.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  FILLER                      PIC X(48)     VALUE SPACES.
           05  WS-H2-COMPANY-NAME          PIC X(30)     VALUE SPACES.
           05  FILLER                      PIC X(42)     VALUE SPACES.
           05  WS-H2-DATE                  PIC X(08)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE SPACES.


       01  WS-HEADING3.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  FILLER                      PIC X(120)    VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE 'PAGE'.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  WS-H3-PAGE                  PIC ZZ,ZZ9.
           05  FILLER                      PIC X(01)     VALUE SPACE.


       01  WS-HEADING4.
           05  FILLER                      PIC X(01)     VALUE '-'.
           05  FILLER                      PIC X(28)     VALUE SPACES.
           05  WS-H4-RESCIND-CC-VAR        PIC X(02)     VALUE SPACES.
           05  WS-H4-RESCIND-YY-VAR        PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(17)     VALUE
               ' RESCISSIONS FOR '.
           05  WS-H4-SUBHEAD-VAR           PIC X(17)     VALUE SPACES.
           05  FILLER                      PIC X(66)     VALUE SPACES.


       01  WS-HEADING5.
           05  FILLER                      PIC X(01)     VALUE '0'.
           05  FILLER                      PIC X(12)     VALUE SPACES.
           05  FILLER                      PIC X(15)     VALUE
               'RESCISSION DATE'.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'STATE'.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(12)     VALUE
               'AGENT NUMBER'.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(12)     VALUE
               'CLAIM NUMBER'.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(18)     VALUE
               'CERTIFICATE NUMBER'.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(48)     VALUE
               'FORMS SENT'.


       01  WS-DETAIL1.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  FILLER                      PIC X(12)     VALUE SPACES.
           05  WS-D1-CLOSE-DT-CYMD         PIC X(08)     VALUE SPACES.
           05  FILLER                      PIC X(09)     VALUE SPACES.
           05  WS-D1-STATE                 PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE SPACES.
           05  WS-D1-AGENT-NO              PIC X(10)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE SPACES.
           05  WS-D1-CLAIM-NO              PIC X(07)     VALUE SPACES.
           05  FILLER                      PIC X(07)     VALUE SPACES.
           05  WS-D1-CERT-NO               PIC X(11)     VALUE SPACES.
           05  FILLER                      PIC X(09)     VALUE SPACES.
           05  WS-D1-FORMIDS               PIC X(48)     VALUE SPACES.


       01  WS-SUMMARY-DETAIL.
           05  FILLER                  PIC X(01)         VALUE SPACE.
           05  FILLER                  PIC X(32)         VALUE SPACES.
           05  WS-SD-LABEL             PIC X(38)         VALUE SPACES.
           05  WS-SD-COUNT             PIC ZZZ9.
           05  FILLER                  PIC X(58)         VALUE SPACES.


      *              *************
      *              ELCDTECX: LAYOUT FOR DISK-DATE FILE
                     COPY ELCDTECX.


                     COPY ELCDTEVR.

      *              *************
      *              ELCDATE: LAYOUT OF DATA PASSED TO DATE CONV RTN
                     COPY ELCDATE.


       LINKAGE SECTION.

       01  PARM.
           05  PARM-LENGTH        COMP     PIC S9(04)    VALUE ZEROS.
           05  PARM-CUTOFF-DT              PIC X(08)     VALUE SPACES.
           05  PARM-CYCLE-DT               PIC X(08)     VALUE SPACES.

      ******************************************************************
      ********************************
       PROCEDURE DIVISION USING PARM.

      ****************READ DISK-DATE FILE
       0000-DATE-CARD-READ. COPY ELCDTERX.

       1000-MAIN-LOGIC.

           PERFORM 1500-EDIT-CYCLE-DATE       THRU 1500-EXIT

           PERFORM OPEN-FILES THRU OPEN-FILES-EXIT

           MOVE COMPANY-NAME                  TO WS-H2-COMPANY-NAME
           MOVE WS-CURRENT-DATE               TO WS-H2-DATE

           SORT SORT-FILE  ASCENDING KEY SORT-KEY
              INPUT  PROCEDURE 2000-INPUT-PROCEDURE  THRU 2000-EXIT
              OUTPUT PROCEDURE 3000-OUTPUT-PROCEDURE THRU 3000-EXIT


           PERFORM CLOSE-FILES THRU CLOSE-FILES-EXIT


           GOBACK.


       1500-EDIT-CYCLE-DATE.

           IF PARM-LENGTH = +0
               DISPLAY 'CYCLE DATE INPUT PARMS MISSING'
               PERFORM ABEND-PGM            THRU APS-EXIT
           END-IF

           MOVE PARM-CYCLE-DT               TO DC-GREG-DATE-CYMD
           MOVE 'L'                         TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION     THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-1           TO WS-CYCLE-DT-BINARY
               MOVE PARM-CYCLE-DT           TO WS-CYCLE-DT
               MOVE WS-CYCLE-DT-MM          TO WS-EDITED-CYCLE-DT-MM
               MOVE WS-CYCLE-DT-DD          TO WS-EDITED-CYCLE-DT-DD
               MOVE WS-CYCLE-DT-YY          TO WS-EDITED-CYCLE-DT-YY
           ELSE
               DISPLAY 'INVALID CYCLE DATE ' PARM-CYCLE-DT
               PERFORM ABEND-PGM            THRU APS-EXIT
           END-IF


           MOVE PARM-CUTOFF-DT              TO DC-GREG-DATE-CYMD
           MOVE 'L'                         TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION     THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-1           TO WS-CUTOFF-DT-BINARY
               MOVE PARM-CUTOFF-DT          TO WS-CUTOFF-DT
               MOVE WS-CUTOFF-DT-MM         TO WS-EDITED-CUTOFF-DT-MM
               MOVE WS-CUTOFF-DT-DD         TO WS-EDITED-CUTOFF-DT-DD
               MOVE WS-CUTOFF-DT-YY         TO WS-EDITED-CUTOFF-DT-YY
           ELSE
               DISPLAY 'INVALID CUTOFF DATE ' PARM-CUTOFF-DT
               PERFORM ABEND-PGM            THRU APS-EXIT
           END-IF

           .
       1500-EXIT.
           EXIT.


       2000-INPUT-PROCEDURE.

           PERFORM 2010-GET-RECS-FOR-SORT   THRU 2010-EXIT
               UNTIL END-OF-ELMSTR

           IF SORT-RETURN  NOT = ZEROS
              MOVE 'INTERNAL SORT ABORTED'  TO  WS-ABEND-MESSAGE
              MOVE '0101'                   TO  WS-RETURN-CODE
              PERFORM ABEND-PGM             THRU APS-EXIT
           END-IF

           .
       2000-EXIT.
           EXIT.


       2010-GET-RECS-FOR-SORT.

           MOVE LOW-VALUES                  TO CL-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD       TO CL-COMPANY-CD
           START ELMSTR-INFILE KEY NOT < CL-CONTROL-PRIMARY
               INVALID KEY
                   DISPLAY 'INVALID KEY ON START ELMSTR'
                                            CL-CONTROL-PRIMARY
                   PERFORM ABEND-PGM        THRU APS-EXIT
           END-START

           PERFORM 2050-READ-ELMSTR         THRU 2100-EXIT
               UNTIL END-OF-ELMSTR

           .
       2010-EXIT.
           EXIT.


       2050-READ-ELMSTR.

           READ ELMSTR-INFILE NEXT RECORD
               AT END
                   SET END-OF-ELMSTR        TO TRUE
                   GO TO 2050-EXIT
           END-READ

           IF CL-CLAIM-TYPE = SPACE OR LOW-VALUE
               DISPLAY 'NO CLM TYP FOR CNTL PRI ' CL-CONTROL-PRIMARY
               GO TO 2050-EXIT
           END-IF

      ******* LOOKING FOR CLOSED CLAIMS DUE TO DENIAL
           IF CL-LAST-CLOSE-DT >= WS-CUTOFF-DT-BINARY
               CONTINUE
           ELSE
               GO TO 2050-EXIT
           END-IF

      ******* DENIAL
           IF CL-LAST-CLOSE-REASON = '2'
               CONTINUE
           ELSE
               GO TO 2050-EXIT
           END-IF

      ******* WAS CLAIM REOPENED?
           IF CL-LAST-REOPEN-DT > CL-LAST-CLOSE-DT
               GO TO 2050-EXIT
           END-IF

           MOVE LOW-VALUES                  TO AT-CONTROL-PRIMARY
           MOVE CL-CONTROL-PRIMARY          TO AT-CONTROL-PRIMARY (1:20)
           START ELTRLR-INFILE KEY NOT < AT-CONTROL-PRIMARY
               INVALID KEY
                   DISPLAY 'INVALID KEY ON START ELTRLR'
                                            CL-CONTROL-PRIMARY
                   PERFORM ABEND-PGM        THRU APS-EXIT
           END-START

           SET NEW-ELTRLR-KEY               TO TRUE
           MOVE +0                          TO WS-FORMID-SUB
           MOVE SPACES                      TO WS-FORMID-TABLE
           PERFORM 2100-READ-ELTRLR         THRU 2100-EXIT
               UNTIL ELTRLR-KEY-CHANGE  OR
                     END-OF-ELTRLR

           IF RESCISSION
               PERFORM 2200-BUILD-SORT-RECORD THRU 2200-EXIT
               RELEASE SORT-RECORD
               MOVE SPACE                   TO WS-RESCIND-SW
           END-IF

           .
       2050-EXIT.
           EXIT.

       2100-READ-ELTRLR.

           READ ELTRLR-INFILE NEXT RECORD
               AT END
                   SET END-OF-ELTRLR        TO TRUE
                   GO TO 2100-EXIT
           END-READ


           IF AT-CONTROL-PRIMARY (1:20) = CL-CONTROL-PRIMARY
               CONTINUE
           ELSE
               SET ELTRLR-KEY-CHANGE        TO TRUE
               GO TO 2100-EXIT
           END-IF

           IF CORRESPONDENCE-TR
               EVALUATE TRUE
               WHEN AT-STD-LETTER-FORM = 'REAL'
                   SET RESCISSION           TO TRUE
                   PERFORM 2150-SAVE-FORMID-IN-TABLE THRU 2150-EXIT
               WHEN AT-STD-LETTER-FORM = 'REA1'
                   SET RESCISSION TO TRUE
                   PERFORM 2150-SAVE-FORMID-IN-TABLE THRU 2150-EXIT
               WHEN AT-STD-LETTER-FORM = 'REA2'
                   SET RESCISSION TO TRUE
                   PERFORM 2150-SAVE-FORMID-IN-TABLE THRU 2150-EXIT
               WHEN AT-STD-LETTER-FORM = 'REC2'
                   SET RESCISSION TO TRUE
                   PERFORM 2150-SAVE-FORMID-IN-TABLE THRU 2150-EXIT
               WHEN AT-STD-LETTER-FORM = 'REC3'
                   SET RESCISSION TO TRUE
                   PERFORM 2150-SAVE-FORMID-IN-TABLE THRU 2150-EXIT
               WHEN AT-STD-LETTER-FORM = 'REC4'
                   SET RESCISSION TO TRUE
                   PERFORM 2150-SAVE-FORMID-IN-TABLE THRU 2150-EXIT
               WHEN AT-STD-LETTER-FORM = 'REC8'
                   SET RESCISSION TO TRUE
                   PERFORM 2150-SAVE-FORMID-IN-TABLE THRU 2150-EXIT
               WHEN AT-STD-LETTER-FORM = 'REC9'
                   SET RESCISSION TO TRUE
                   PERFORM 2150-SAVE-FORMID-IN-TABLE THRU 2150-EXIT
               END-EVALUATE
           END-IF

           .
       2100-EXIT.
           EXIT.

       2150-SAVE-FORMID-IN-TABLE.

           ADD +1                           TO WS-FORMID-SUB
           MOVE AT-STD-LETTER-FORM          TO WS-FORMID(WS-FORMID-SUB)

           .
       2150-EXIT.
           EXIT.

       2200-BUILD-SORT-RECORD.

           MOVE CL-CLAIM-TYPE               TO SORT-CLAIM-TYPE

           MOVE CL-LAST-CLOSE-DT            TO DC-BIN-DATE-1
           MOVE SPACE                       TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION     THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-CYMD-CEN             TO SORT-KEY-CLOSE-DT-CEN
               MOVE DC-CYMD-YEAR            TO SORT-KEY-CLOSE-DT-YEAR
               MOVE DC-GREG-DATE-CYMD       TO SORT-CLOSE-DT
           ELSE
               DISPLAY 'CONVERSION ERROR ON CLOSE DATE ' DC-ERROR-CODE
           END-IF

           MOVE CL-CERT-STATE               TO SORT-STATE
           MOVE CL-CERT-ACCOUNT             TO SORT-AGENT-NO
           MOVE CL-CLAIM-NO                 TO SORT-CLAIM-NO
           MOVE CL-CERT-NO                  TO SORT-CERT-NO
           MOVE WS-FORMID-TABLE             TO SORT-FORM-IDS

           .
       2200-EXIT.
           EXIT.
       2800-WRITE.

           EVALUATE TRUE
           WHEN P-CTL = '1'
               MOVE +1                      TO WS-LINE-COUNT

           WHEN P-CTL = SPACE
               ADD +1                       TO WS-LINE-COUNT

           WHEN P-CTL = '0'
               ADD +2                       TO WS-LINE-COUNT

           WHEN OTHER
               ADD +3                       TO WS-LINE-COUNT
           END-EVALUATE


           WRITE PRT

           .
       2800-EXIT.
           EXIT.



       3000-OUTPUT-PROCEDURE.

           MOVE ZERO                           TO WS-PAGE
           MOVE +55                            TO WS-LINE-COUNT

           PERFORM 3020-RETURN-SORT            THRU 3020-EXIT
               UNTIL END-OF-SORTFILE

           PERFORM 3800-PRINT-HEADINGS         THRU 3800-EXIT
           PERFORM 3200-PRINT-TOTALS           THRU 3200-EXIT

           .
       3000-EXIT.
           EXIT.

       3020-RETURN-SORT.

           RETURN SORT-FILE AT END
               SET END-OF-SORTFILE          TO TRUE
               GO TO 3020-EXIT
           END-RETURN


           IF FIRST-SORT-RECORD
               MOVE SORT-CLAIM-TYPE         TO WS-HOLD-CLAIM-TYPE
               MOVE SORT-KEY-CLOSE-DT-YEAR  TO WS-HOLD-CLOSE-DT-YY
               MOVE WS-EDITED-CUTOFF-DT     TO WS-H1-SINCE-DT
               MOVE SORT-KEY-CLOSE-DT-CEN   TO WS-H4-RESCIND-CC-VAR
               MOVE SORT-KEY-CLOSE-DT-YEAR  TO WS-H4-RESCIND-YY-VAR
               MOVE WS-DIS-LABEL            TO WS-H4-SUBHEAD-VAR
               SET NOT-FIRST-RECORD         TO TRUE
           END-IF

           IF SORT-CLAIM-TYPE = WS-HOLD-CLAIM-TYPE
               CONTINUE
           ELSE
               IF SORT-CLAIM-TYPE = 'L'
                   PERFORM 3070-SUBTOTAL-FOR-YEAR THRU 3070-EXIT
                   MOVE +0                     TO WS-YEAR-RESCIND-CNT
                   MOVE SORT-KEY-CLOSE-DT-CEN  TO WS-H4-RESCIND-CC-VAR
                   MOVE SORT-KEY-CLOSE-DT-YEAR TO WS-H4-RESCIND-YY-VAR
                   MOVE WS-LIFE-LABEL          TO WS-H4-SUBHEAD-VAR
                   MOVE WS-LIFE-SUBTOT-LABEL   TO WS-SUBTOT-LABEL-VAR
                   MOVE +55                    TO WS-LINE-COUNT
                   MOVE SORT-CLAIM-TYPE        TO WS-HOLD-CLAIM-TYPE
                   MOVE SORT-KEY-CLOSE-DT-YEAR TO WS-HOLD-CLOSE-DT-YY
               END-IF
           END-IF

           IF SORT-KEY-CLOSE-DT-YEAR = WS-HOLD-CLOSE-DT-YY
               CONTINUE
           ELSE
               PERFORM 3070-SUBTOTAL-FOR-YEAR THRU 3070-EXIT
               MOVE +0                      TO WS-YEAR-RESCIND-CNT
               MOVE SORT-KEY-CLOSE-DT-CEN   TO WS-H4-RESCIND-CC-VAR
               MOVE SORT-KEY-CLOSE-DT-YEAR  TO WS-H4-RESCIND-YY-VAR
               MOVE SORT-KEY-CLOSE-DT-YEAR  TO WS-HOLD-CLOSE-DT-YY
               MOVE +55                     TO WS-LINE-COUNT
           END-IF

           PERFORM 3050-MOVE-SORT-FIELDS    THRU 3050-EXIT
           PERFORM 3100-PRINT-DETAIL        THRU 3100-EXIT

           .
       3020-EXIT.
           EXIT.


       3050-MOVE-SORT-FIELDS.

           INITIALIZE WS-DETAIL1

           MOVE SORT-CLOSE-DT               TO WS-D1-CLOSE-DT-CYMD
           MOVE SORT-STATE                  TO WS-D1-STATE
           MOVE SORT-AGENT-NO               TO WS-D1-AGENT-NO
           MOVE SORT-CLAIM-NO               TO WS-D1-CLAIM-NO
           MOVE SORT-CERT-NO                TO WS-D1-CERT-NO
           MOVE SORT-FORM-IDS               TO WS-D1-FORMIDS

           IF SORT-CLAIM-TYPE = 'A'
               ADD +1                       TO WS-DIS-RESCIND-CNT
           ELSE
               ADD +1                       TO WS-LIFE-RESCIND-CNT
           END-IF
           ADD +1                           TO WS-GRAND-TOT-RESCIND-CNT
           ADD +1                           TO WS-YEAR-RESCIND-CNT

           .
       3050-EXIT.
           EXIT.


       3070-SUBTOTAL-FOR-YEAR.

           MOVE WS-H4-RESCIND-CC-VAR        TO WS-SUBTOT-CC
           MOVE WS-H4-RESCIND-YY-VAR        TO WS-SUBTOT-YY
           MOVE WS-YEAR-SUBTOT-LABEL        TO WS-SD-LABEL
           MOVE WS-YEAR-RESCIND-CNT         TO WS-SD-COUNT
           MOVE WS-SUMMARY-DETAIL           TO PRT
           MOVE '-'                         TO P-CTL
           PERFORM 3900-WRITE               THRU 3900-EXIT

           .
       3070-EXIT.
           EXIT.


       3100-PRINT-DETAIL.

           IF WS-LINE-COUNT >= WS-LINE-COUNT-MAX
               PERFORM 3800-PRINT-HEADINGS  THRU 3800-EXIT
               MOVE WS-HEADING4             TO PRT
               PERFORM 3900-WRITE           THRU 3900-EXIT
               MOVE WS-HEADING5             TO PRT
               PERFORM 3900-WRITE           THRU 3900-EXIT
           END-IF

           MOVE WS-DETAIL1                  TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

           .
       3100-EXIT.
           EXIT.


       3200-PRINT-TOTALS.

           INITIALIZE WS-SUMMARY-DETAIL
           MOVE WS-DIS-TOTAL-LABEL          TO WS-SD-LABEL
           MOVE WS-DIS-RESCIND-CNT          TO WS-SD-COUNT
           MOVE WS-SUMMARY-DETAIL           TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

           INITIALIZE WS-SUMMARY-DETAIL
           MOVE WS-LIFE-TOTAL-LABEL         TO WS-SD-LABEL
           MOVE WS-LIFE-RESCIND-CNT         TO WS-SD-COUNT
           MOVE WS-SUMMARY-DETAIL           TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

           INITIALIZE WS-SUMMARY-DETAIL
           MOVE WS-GRAND-TOTAL-LABEL        TO WS-SD-LABEL
           MOVE WS-GRAND-TOT-RESCIND-CNT    TO WS-SD-COUNT
           MOVE WS-SUMMARY-DETAIL           TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

           .
       3200-EXIT.
           EXIT.

       3800-PRINT-HEADINGS.

           MOVE WS-HEADING1                 TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

           MOVE WS-HEADING2                 TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

           ADD +1                           TO WS-PAGE
           MOVE WS-PAGE                     TO WS-H3-PAGE
           MOVE WS-HEADING3                 TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

           .
       3800-EXIT.
           EXIT.

       3900-WRITE.

           EVALUATE TRUE
           WHEN P-CTL = '1'
               MOVE +1                      TO WS-LINE-COUNT

           WHEN P-CTL = SPACE
               ADD +1                       TO WS-LINE-COUNT

           WHEN P-CTL = '0'
               ADD +2                       TO WS-LINE-COUNT

           WHEN OTHER
               ADD +3                       TO WS-LINE-COUNT
           END-EVALUATE

           WRITE PRT

           .
       3900-EXIT.
           EXIT.


       8500-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8500-EXIT.
           EXIT.


       OPEN-FILES.

           OPEN INPUT  ELMSTR-INFILE
                       ELTRLR-INFILE
                OUTPUT PRNTR


           IF WS-ELMSTR-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE 'OPEN ERROR OCCURRED ON ELMSTR'
                                              TO WS-ABEND-MESSAGE
              MOVE WS-ELMSTR-FILE-STATUS      TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM               THRU APS-EXIT
           END-IF


           IF WS-ELTRLR-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE 'OPEN ERROR OCCURRED ON ELTRLR'
                                              TO WS-ABEND-MESSAGE
              MOVE WS-ELTRLR-FILE-STATUS      TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM               THRU APS-EXIT
           END-IF

           .
       OPEN-FILES-EXIT.
           EXIT.


       CLOSE-FILES.


           CLOSE ELMSTR-INFILE
                 ELTRLR-INFILE
                 PRNTR

            .
       CLOSE-FILES-EXIT.
           EXIT.

       ABEND-PGM. COPY ELCABEND.
