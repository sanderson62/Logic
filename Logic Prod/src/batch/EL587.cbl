00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL587.


      *AUTHOR.     SUZAN VUKOV.
      ******************************************************************
00025 *REMARKS.
00026 *     THIS PROGRAM CREATES A MONTH-END CID CLAIM FILE PROVIDING:
      *          1) OPEN CLAIM, CLOSED CLAIM, AND COMBINED OPEN & CLOSED
      *                COUNTS BY INCURRED YEAR AND MONTH
      *
      *          2) TOTAL BENEFIT AND TOTAL PAID AMOUNTS BY INCURRED
      *                YEAR AND MONTH
      *             (LIFE AND DISABILTIY COUNTS AND TOTALS ARE SEPARATE)
      *
      *
      *     INPUT:   ELMSTR - CLAIM MASTER
      *              ELCERT - CERTIFICATE MASTER
      *              ELTRLR - CLAIM ACTIVITY
      *              DATE CARD FILE - CI.DD.ER.DATECARD
      *
      *    OUTPUT:   CIDOPEN.DAT - USED BY ACTUARY IN ACCESS
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 021502    2001120300002  SMVA  INITIAL IMPL FOR UNIKIX
      *                                REWRITE OF MAINFRAME CULPRIT
122302* 122302    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
110503* 110503                   SMVA  DONT BREAK OUT CPD FOR CID       
121603* 121603    2003080800002  SMVA  ADD PROCESSING FOR NEW CLM TYPE G
022804* 022804                   SMVA  HANDLE FILE STATUS 23 ON ELMSTR
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
      ******************************************************************
00031
00032  ENVIRONMENT DIVISION.
00033  CONFIGURATION SECTION.
00034
00035  INPUT-OUTPUT SECTION.
00036
00037  FILE-CONTROL.

           SELECT ELMSTR-INFILE       ASSIGN TO ELMSTR
                                      ORGANIZATION IS INDEXED
                                      ACCESS IS DYNAMIC
                                      RECORD KEY IS CL-CONTROL-PRIMARY
                                      FILE STATUS IS
                                      WS-ELMSTR-FILE-STATUS.


           SELECT ELCERT-INFILE       ASSIGN TO ELCERT
                                      ORGANIZATION IS INDEXED
                                      ACCESS IS DYNAMIC
                                      RECORD KEY IS CM-CONTROL-PRIMARY
                                      FILE STATUS IS
                                      WS-ELCERT-FILE-STATUS.


           SELECT ELTRLR-INFILE       ASSIGN TO ELTRLR
                                      ORGANIZATION IS INDEXED
                                      ACCESS IS DYNAMIC
                                      RECORD KEY IS AT-CONTROL-PRIMARY
                                      FILE STATUS IS
                                      WS-ELTRLR-FILE-STATUS.


00042      SELECT SORT-FILE           ASSIGN TO
                                      SYS001-UT-FBA1-S-SORTWK1.
00044
00045      SELECT DISK-DATE           ASSIGN TO SYS019-UT-FBA1-S-SYS019.
00046
00049      SELECT CIDOPEN-OUTFILE     ASSIGN TO SYS020-UT-2400-S-SYS020
                                      ORGANIZATION IS LINE SEQUENTIAL.
00050

00064  DATA DIVISION.
00065
00066  FILE SECTION.
00067
       FD  ELMSTR-INFILE.
                                COPY ELCMSTR.

       FD  ELCERT-INFILE.
                                COPY ELCCERT.

       FD  ELTRLR-INFILE.
                                COPY ELCTRLR.

00073  SD  SORT-FILE.
       01  SORT-RECORD.
           05  SORT-KEY.
               10  SORT-BENEFIT-TYPE       PIC X(06)     VALUE SPACES.
               10  SORT-PAID-STATUS        PIC X(04)     VALUE SPACES.
               10  SORT-CLAIM-STATUS       PIC X(06)     VALUE SPACES.
               10  SORT-INCUR-YR           PIC X(04)     VALUE SPACES.
               10  SORT-INCUR-MO           PIC X(02)     VALUE SPACES.
           05  SORT-TOTAL-PAID-AMT         PIC S9(10)V99 VALUE ZEROS.
           05  SORT-BENEFIT-AMT            PIC S9(09)V99 VALUE ZEROS.


00076  FD  DISK-DATE                       COPY ELCDTEFD.
00077
       FD  CIDOPEN-OUTFILE.
       01  CIDOPEN-RECORD                  PIC X(53).


00089  WORKING-STORAGE SECTION.
00090
00091  77  FILLER PIC X(32) VALUE '********************************'.
00092  77  FILLER PIC X(32) VALUE '*     EL587 WORKING-STORAGE    *'.
00093  77  FILLER PIC X(32) VALUE '***********VMOD=2.014 **********'.
00094
00113
00114  01  FILLER                          COMP-3.
00123      05  WS-RETURN-CODE              PIC S9(03)    VALUE +0.
00124      05  WS-ZERO                     PIC S9(01)    VALUE +0.

122302 01  WS-SUB               COMP       PIC S9(04)    VALUE +0.

00195
00196  01  FILLER                          COMP SYNC.
00197      05  PGM-SUB                     PIC S9(04)    VALUE +587.
00200
00201  01  FILLER.
           05  WS-CYCLE-DT-BINARY          PIC X(02)     VALUE SPACES.

           05  WS-ELMSTR-FILE-STATUS       PIC X(02)     VALUE SPACES.
           05  WS-ELCERT-FILE-STATUS       PIC X(02)     VALUE SPACES.
           05  WS-ELTRLR-FILE-STATUS       PIC X(02)     VALUE SPACES.

           05  WS-EOF-SW1                  PIC X(01)     VALUE SPACE.
               88  END-OF-ELMSTR                         VALUE 'Y'.

           05  WS-EOF-SW2                  PIC X(01)     VALUE SPACE.
               88  BEGIN-ELTRLR                          VALUE 'Y'.
               88  DONE                                  VALUE 'D'.

           05  WS-EOF-SW3                  PIC X(01)     VALUE SPACE.
               88  END-OF-SORTFILE                       VALUE 'Y'.

           05  WS-FIRST-TIME-SW            PIC X(01)     VALUE 'Y'.
               88  FIRST-SORT-RECORD                     VALUE 'Y'.
               88  NOT-FIRST-SORT-RECORD                 VALUE 'N'.

           05  WS-COMPANY-CD-CHG-SW        PIC X(01)     VALUE SPACE.
               88  CHANGE-IN-COMPANY-CD                  VALUE 'Y'.

122302     05  WS-SEARCH-SW                PIC X(01)     VALUE SPACE.
122302         88  FOUND                                 VALUE 'Y'.
122302         88  NEW-SEARCH                            VALUE 'N'.

00220      05  WS-ABEND-MESSAGE            PIC X(80)     VALUE SPACES.
00221
00222      05  WS-ABEND-FILE-STATUS        PIC X(02)     VALUE ZERO.
00223
00224      05  WS-FILE-ERROR-MESSAGE.
00225          10  FILLER                  PIC X(24)     VALUE
00226              'ERROR OCCURED OPENING - '.
00227          10  WS-FEM-FILE-NAME        PIC X(08).

           05  WS-CLAIM-COUNT              PIC S9(06)    VALUE +0.
           05  WS-RELEASE-COUNT            PIC S9(06)    VALUE +0.

       01  WS-CIDOPEN-RECORD.
           05  WS-BENEFIT-TYPE             PIC X(06)     VALUE SPACES.
           05  WS-PAID-STATUS              PIC X(04)     VALUE SPACES.
           05  WS-CLAIM-STATUS             PIC X(06)     VALUE SPACES.
           05  WS-INCUR-YR                 PIC X(04)     VALUE SPACES.
           05  WS-INCUR-MO                 PIC X(02)     VALUE SPACES.
           05  WS-TOTAL-PAID-AMT           PIC S9(10)V99 VALUE ZEROS.
           05  WS-BENEFIT-AMT              PIC S9(09)V99 VALUE ZEROS.


       01  WS-HOLD-CIDOPEN-RECORD.
           05  WS-HOLD-BENEFIT-TYPE        PIC X(06)     VALUE SPACES.
           05  WS-HOLD-PAID-STATUS         PIC X(04)     VALUE SPACES.
           05  WS-HOLD-CLAIM-STATUS        PIC X(06)     VALUE SPACES.
           05  WS-HOLD-INCUR-YR            PIC X(04)     VALUE SPACES.
           05  WS-HOLD-INCUR-MO            PIC X(02)     VALUE SPACES.
           05  WS-HOLD-CLAIM-COUNT         PIC ZZZZZ9.
           05  WS-HOLD-TOTAL-PAID-AMT      PIC ZZZZZZZZZ9.99.
           05  WS-HOLD-BENEFIT-AMT         PIC ZZZZZZZZ9.99.
00569
00570      COPY ELCDTECX.
00571
00572      COPY ELCDTEVR.
00573
00574      COPY ELCDATE.


       LINKAGE SECTION.

       01  PARM.
           05  PARM-LENGTH        COMP     PIC S9(04)    VALUE ZEROS.
           05  PARM-CYCLE-DT               PIC X(08)     VALUE SPACES.

      ******************************************************************
      ********************************


00576  PROCEDURE DIVISION USING PARM.
00577
00585  0000-DATE-CARD-READ. COPY ELCDTERX.
00586
00601  1000-MAIN-LOGIC.
00602
00603      PERFORM OPEN-FILES                THRU OPEN-FILES-EXIT

           PERFORM 1500-EDIT-CYCLE-DATE      THRU 1500-EXIT
00604
00611      SORT SORT-FILE
00612          ON ASCENDING KEY SORT-KEY
00619          INPUT PROCEDURE  2000-INPUT-PROCEDURE  THRU 2000-EXIT
00620          OUTPUT PROCEDURE 3000-OUTPUT-PROCEDURE THRU 3000-EXIT
00621
00622      IF SORT-RETURN > ZERO
022804        AND WS-RELEASE-COUNT NOT = +0
00623          MOVE 'SORT FAILED'            TO WS-ABEND-MESSAGE
00624          MOVE SORT-RETURN              TO WS-RETURN-CODE
00625          PERFORM ABEND-PGM             THRU APS-EXIT
           END-IF
00626
00627      PERFORM CLOSE-FILES               THRU CLOSE-FILES-EXIT
00628
00646      GOBACK

           .
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
           ELSE
               DISPLAY 'INVALID CYCLE DATE ' PARM-CYCLE-DT
               PERFORM ABEND-PGM            THRU APS-EXIT
           END-IF

           .
       1500-EXIT.
           EXIT.


00649  2000-INPUT-PROCEDURE.

           MOVE DTE-CLASIC-COMPANY-CD        TO CL-COMPANY-CD
           START ELMSTR-INFILE KEY NOT < CL-COMPANY-CD

022804     EVALUATE TRUE
022804     WHEN WS-ELMSTR-FILE-STATUS = '00'
022804         CONTINUE

022804     WHEN WS-ELMSTR-FILE-STATUS = '23'
022804         SET END-OF-ELMSTR TO TRUE
022804         GO TO 2000-EXIT

022804     WHEN OTHER
022804        DISPLAY ' ELMSTR START ' WS-ELMSTR-FILE-STATUS
              DISPLAY 'INVALID KEY ON START ELMSTR ' CL-COMPANY-CD
              PERFORM ABEND-PGM               THRU APS-EXIT
022804     END-EVALUATE

           PERFORM 2010-BUILD-CIDOPEN-FILE   THRU 2010-EXIT
               UNTIL CHANGE-IN-COMPANY-CD OR
                   END-OF-ELMSTR

           .
       2000-EXIT.
           EXIT.
00650
       2010-BUILD-CIDOPEN-FILE.

           READ ELMSTR-INFILE NEXT RECORD
               AT END
                   SET END-OF-ELMSTR        TO TRUE
                   GO TO 2010-EXIT
           END-READ


           IF CL-COMPANY-CD = DTE-CLASIC-COMPANY-CD
               CONTINUE
           ELSE
               SET CHANGE-IN-COMPANY-CD     TO TRUE
               GO TO 2010-EXIT
           END-IF

           IF (CL-FILE-ESTABLISH-DT > WS-CYCLE-DT-BINARY OR
               CL-INCURRED-DT       > WS-CYCLE-DT-BINARY)
               GO TO 2010-EXIT
           END-IF


           MOVE CL-COMPANY-CD               TO CM-COMPANY-CD
           MOVE CL-CERT-KEY-DATA            TO CM-CONTROL-PRIMARY (2:21)
           MOVE CL-CERT-NO                  TO CM-CERT-NO

           READ ELCERT-INFILE

           IF CL-CLAIM-TYPE = SPACE OR LOW-VALUE
      ********* EDIT IS NOW IN PLACE TO PREVENT BLANK CLAIM TYPE
               DISPLAY 'NO CLM TYP FOR CNTL PRI ' CL-CONTROL-PRIMARY
               GO TO 2010-EXIT
           END-IF

122302     EVALUATE TRUE
122302     WHEN CL-CLAIM-TYPE = 'A'
122302         PERFORM 2100-GET-CALC-TYPE   THRU 2100-EXIT
122302             VARYING WS-SUB FROM +1 BY +1
122302             UNTIL FOUND 
122302         IF CLAS-I-CALC-TYPE(WS-SUB - +1) NOT = 'C'
110503            OR  DTE-CLIENT = 'CID'
                   MOVE 'CIDDIS'            TO WS-BENEFIT-TYPE
122302         ELSE
122302             MOVE 'CIDCPD'            TO WS-BENEFIT-TYPE
122302         END-IF
122302*        DISPLAY 'benefit after' CLAS-I-BEN(WS-SUB - +1) 
               MOVE CM-AH-BENEFIT-AMT       TO WS-BENEFIT-AMT
122302         SET NEW-SEARCH               TO TRUE

121603     WHEN CL-CLAIM-TYPE = 'G'
121603         MOVE 'CIDGAP'                TO WS-BENEFIT-TYPE
121603         MOVE CM-AH-BENEFIT-AMT       TO WS-BENEFIT-AMT

122302     WHEN CL-CLAIM-TYPE = 'I'
122302         MOVE 'CIDIU '                TO WS-BENEFIT-TYPE
122302         MOVE CM-AH-BENEFIT-AMT       TO WS-BENEFIT-AMT
052614
052614     WHEN CL-CLAIM-TYPE = 'F'
052614         MOVE 'CIDFAM'                TO WS-BENEFIT-TYPE
052614         MOVE CM-AH-BENEFIT-AMT       TO WS-BENEFIT-AMT
100518
022122     WHEN CL-CLAIM-TYPE = 'B'
022122         MOVE 'CIDBRV'                TO WS-BENEFIT-TYPE
022122         MOVE CM-AH-BENEFIT-AMT       TO WS-BENEFIT-AMT
022122
022122     WHEN CL-CLAIM-TYPE = 'H'
022122         MOVE 'CIDHOS'                TO WS-BENEFIT-TYPE
022122         MOVE CM-AH-BENEFIT-AMT       TO WS-BENEFIT-AMT
100518
100518     WHEN CL-CLAIM-TYPE = 'O'
100518         MOVE 'CIDOTH'                TO WS-BENEFIT-TYPE
100518         MOVE CM-LF-BENEFIT-AMT       TO WS-BENEFIT-AMT

122302     WHEN CL-CLAIM-TYPE = 'L'
               MOVE 'CIDLIF'                TO WS-BENEFIT-TYPE
               MOVE CM-LF-BENEFIT-AMT       TO WS-BENEFIT-AMT
122302     END-EVALUATE

           IF (CL-LAST-CLOSE-DT > WS-CYCLE-DT-BINARY OR
               CL-CLAIM-STATUS = 'O')
               MOVE 'OPEN  '                TO WS-CLAIM-STATUS
           ELSE
               IF (CL-LAST-REOPEN-DT > WS-CYCLE-DT-BINARY OR
                   CL-CLAIM-STATUS = 'C')
                   MOVE 'CLOSED'            TO WS-CLAIM-STATUS
               END-IF
           END-IF

           MOVE CL-INCURRED-DT              TO DC-BIN-DATE-1
           MOVE SPACE                       TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION     THRU 8500-EXIT

           IF (NO-CONVERSION-ERROR OR
               DATE-IS-ZERO)
               MOVE DC-EDITA-CCYY           TO WS-INCUR-YR
               MOVE DC-EDITA-MONTH          TO WS-INCUR-MO
           ELSE
               DISPLAY 'INVALID INCURRED DATE ' CL-INCURRED-DT
                                                CL-CONTROL-PRIMARY
           END-IF

           MOVE CL-TOTAL-PAID-AMT           TO WS-TOTAL-PAID-AMT

           IF CL-LAST-PMT-DT <= WS-CYCLE-DT-BINARY
               PERFORM 2025-GET-PAID-STATUS THRU 2025-EXIT
           ELSE
               MOVE CL-CONTROL-PRIMARY      TO AT-CONTROL-PRIMARY
               MOVE CL-TRAILER-SEQ-CNT      TO AT-SEQUENCE-NO
               START ELTRLR-INFILE KEY NOT < AT-CONTROL-PRIMARY
                   INVALID KEY
                       DISPLAY 'INVALID KEY ON START ELTRLR '
                                               AT-CONTROL-PRIMARY
                       PERFORM ABEND-PGM    THRU APS-EXIT
               END-START
               SET BEGIN-ELTRLR             TO TRUE
               PERFORM 2200-READ-ELTRLR     THRU 2200-EXIT
                   UNTIL DONE
               PERFORM 2025-GET-PAID-STATUS THRU 2025-EXIT
           END-IF

           .
       2010-EXIT.
           EXIT.


       2025-GET-PAID-STATUS.

           IF WS-TOTAL-PAID-AMT NOT= +0
               MOVE 'PAID'              TO WS-PAID-STATUS
           ELSE
               MOVE 'ZERO'              TO WS-PAID-STATUS
           END-IF

           PERFORM 2300-RELEASE-SORT    THRU 2300-EXIT
           MOVE 'INCR'                  TO WS-PAID-STATUS
           PERFORM 2300-RELEASE-SORT    THRU 2300-EXIT

           INITIALIZE WS-CIDOPEN-RECORD

           .
       2025-EXIT.
           EXIT.


122302 2100-GET-CALC-TYPE.

122302     IF CLAS-I-BEN(WS-SUB) = CM-AH-BENEFIT-CD
122302*        DISPLAY 'benefit found' CLAS-I-BEN(WS-SUB)
122302         SET FOUND                TO TRUE
122302     END-IF

122302     .
122302 2100-EXIT.
122302     EXIT.

       2200-READ-ELTRLR.

           READ ELTRLR-INFILE NEXT RECORD
               AT END
                   SET DONE             TO TRUE
                   GO TO 2200-EXIT
           END-READ


           IF AT-RECORDED-DT <= WS-CYCLE-DT-BINARY
               SET DONE TO TRUE
               GO TO 2200-EXIT
           END-IF


           IF AT-TRAILER-TYPE NOT= '2'
               GO TO 2200-EXIT
           END-IF


           IF AT-VOID-DT > SPACES
               GO TO 2200-EXIT
           END-IF

      **** DON'T WANT TO INCLUDE PAYMENTS AFTER THE CURRENT MONTHEND
      **** DATE THAT HAVE ALREADY BEEN ADDED TO THE CLAIM MASTER RECORD
           SUBTRACT AT-AMOUNT-PAID           FROM WS-TOTAL-PAID-AMT

           .
       2200-EXIT.
           EXIT.


00708  2300-RELEASE-SORT.
00709
00710      RELEASE SORT-RECORD               FROM WS-CIDOPEN-RECORD
022804     ADD +1                            TO WS-RELEASE-COUNT
00711
00712      IF SORT-RETURN > ZERO
00713          MOVE 'ERROR ON RELEASE SORT'  TO WS-ABEND-MESSAGE
00714          MOVE SORT-RETURN              TO WS-RETURN-CODE
00715          PERFORM ABEND-PGM             THRU APS-EXIT
           END-IF

           .
00738  2300-EXIT.
00739      EXIT.
00740

00741  3000-OUTPUT-PROCEDURE.

           PERFORM 3010-RETURN-SORT            THRU 3010-EXIT
               UNTIL END-OF-SORTFILE

      ****** WRITE LAST RECORD
           MOVE WS-CLAIM-COUNT                 TO WS-HOLD-CLAIM-COUNT
           MOVE WS-TOTAL-PAID-AMT              TO WS-HOLD-TOTAL-PAID-AMT
           MOVE WS-BENEFIT-AMT                 TO WS-HOLD-BENEFIT-AMT

           WRITE CIDOPEN-RECORD FROM WS-HOLD-CIDOPEN-RECORD

           .
       3000-EXIT.
           EXIT.

00742
       3010-RETURN-SORT.

00745      RETURN SORT-FILE
00746          AT END
                   SET END-OF-SORTFILE       TO TRUE
                   GO TO 3010-EXIT
           END-RETURN
00750
00751      IF SORT-RETURN > ZERO
00752          MOVE 'ERROR ON RETURN SORT'   TO WS-ABEND-MESSAGE
00753          MOVE SORT-RETURN              TO WS-RETURN-CODE
00754          PERFORM ABEND-PGM             THRU APS-EXIT
           END-IF

00766      PERFORM 3100-COUNT-AND-TOTAL      THRU 3100-EXIT

           .
       3010-EXIT.
           EXIT.


       3100-COUNT-AND-TOTAL.

           IF FIRST-SORT-RECORD
               MOVE SORT-BENEFIT-TYPE          TO WS-HOLD-BENEFIT-TYPE
               MOVE SORT-PAID-STATUS           TO WS-HOLD-PAID-STATUS
               MOVE SORT-CLAIM-STATUS          TO WS-HOLD-CLAIM-STATUS
               MOVE SORT-INCUR-YR              TO WS-HOLD-INCUR-YR
               MOVE SORT-INCUR-MO              TO WS-HOLD-INCUR-MO
               SET NOT-FIRST-SORT-RECORD       TO TRUE
           END-IF


           IF SORT-BENEFIT-TYPE = WS-HOLD-BENEFIT-TYPE
               CONTINUE
           ELSE
      ********* CHANGING FROM DISABILTIY TO LIFE
               PERFORM 3800-RELOAD-HOLDS-AND-WRITE THRU 3800-EXIT
           END-IF


           IF SORT-PAID-STATUS = WS-HOLD-PAID-STATUS
               CONTINUE
           ELSE
      ********* CHANGING FROM PAID TO ZERO (ZERO AMOUNT PAID)
               PERFORM 3800-RELOAD-HOLDS-AND-WRITE THRU 3800-EXIT
           END-IF


           IF SORT-CLAIM-STATUS = WS-HOLD-CLAIM-STATUS
               CONTINUE
           ELSE
      ********* CHANGING FROM CLOSED TO OPEN
               PERFORM 3800-RELOAD-HOLDS-AND-WRITE THRU 3800-EXIT
           END-IF


           IF SORT-INCUR-YR = WS-HOLD-INCUR-YR
               CONTINUE
           ELSE
               PERFORM 3800-RELOAD-HOLDS-AND-WRITE THRU 3800-EXIT
           END-IF


           IF SORT-INCUR-MO = WS-HOLD-INCUR-MO
               CONTINUE
           ELSE
               PERFORM 3800-RELOAD-HOLDS-AND-WRITE THRU 3800-EXIT
           END-IF

           ADD +1                              TO WS-CLAIM-COUNT
           ADD SORT-TOTAL-PAID-AMT             TO WS-TOTAL-PAID-AMT
           ADD SORT-BENEFIT-AMT                TO WS-BENEFIT-AMT

           .
       3100-EXIT.
           EXIT.

       3800-RELOAD-HOLDS-AND-WRITE.

           MOVE WS-CLAIM-COUNT                 TO WS-HOLD-CLAIM-COUNT
           MOVE WS-TOTAL-PAID-AMT              TO WS-HOLD-TOTAL-PAID-AMT
           MOVE WS-BENEFIT-AMT                 TO WS-HOLD-BENEFIT-AMT

           WRITE CIDOPEN-RECORD FROM WS-HOLD-CIDOPEN-RECORD

           MOVE SORT-BENEFIT-TYPE              TO WS-HOLD-BENEFIT-TYPE
           MOVE SORT-PAID-STATUS               TO WS-HOLD-PAID-STATUS
           MOVE SORT-CLAIM-STATUS              TO WS-HOLD-CLAIM-STATUS
           MOVE SORT-INCUR-YR                  TO WS-HOLD-INCUR-YR
           MOVE SORT-INCUR-MO                  TO WS-HOLD-INCUR-MO
           MOVE ZEROS                          TO WS-HOLD-CLAIM-COUNT
           MOVE ZEROS                          TO WS-HOLD-TOTAL-PAID-AMT
           MOVE ZEROS                          TO WS-HOLD-BENEFIT-AMT
           MOVE ZEROS                          TO WS-CLAIM-COUNT
           MOVE ZEROS                          TO WS-TOTAL-PAID-AMT
           MOVE ZEROS                          TO WS-BENEFIT-AMT

           .
       3800-EXIT.
           EXIT.


01680  8500-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8500-EXIT.
           EXIT.
01681
01837  OPEN-FILES.
01838
01840      OPEN  INPUT  ELMSTR-INFILE
                        ELCERT-INFILE
                        ELTRLR-INFILE
01841            OUTPUT CIDOPEN-OUTFILE


           IF WS-ELMSTR-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE 'OPEN ERROR OCCURRED ON ELMSTR'
                                              TO WS-ABEND-MESSAGE
              MOVE WS-ELMSTR-FILE-STATUS      TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM               THRU APS-EXIT
           END-IF


           IF WS-ELCERT-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE 'OPEN ERROR OCCURRED ON ELCERT'
                                              TO WS-ABEND-MESSAGE
              MOVE WS-ELCERT-FILE-STATUS      TO WS-ABEND-FILE-STATUS
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
01897  OPEN-FILES-EXIT.
01898      EXIT.
01899
01901
01902  CLOSE-FILES.

01906      CLOSE ELMSTR-INFILE
                 ELCERT-INFILE
                 ELTRLR-INFILE
01907            CIDOPEN-OUTFILE

            .
       CLOSE-FILES-EXIT.
           EXIT.
01908
01912  ABEND-PGM SECTION. COPY ELCABEND .
