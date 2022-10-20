00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL588.


      *AUTHOR.     SUZAN VUKOV.
      ******************************************************************
00025 *REMARKS.
00026 *        THIS PROGRAM CREATES A MONTH-END OPEN LIFE CLAIMS REPORT
      *        SORTED BY INCURRED DATE WITH SUBTOTALS BY QUARTER.
      *        IT ALSO PROVIDES AN OPEN DISABILITY CLAIM COUNT.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 011002    2001100100006  SMVA  INITIAL IMPL FOR UNIKIX
      *                                REWRITE OF MAINFRAME CULPRIT
100302* 100302                   SMVA  ADD END-IF W/I CLOSE PARAGRAPH
122302* 122302    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
122302*                                FOR DCC
121603* 121603    2003080800002  SMVA  ADD PROCESSING FOR NEW CLM TYP G
052804* 052804  CR2004051200002  SMVA  INCR SZ OF ELCEXTR TO ADD RPT CD 1
061504* 061504                   SMVA  ADD CERT EFF DTE TO REPORT
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
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
00038
00039      SELECT REPORTS-EXTRACT-FILE
                                      ASSIGN TO SYS010-UT-2400-S-SYS010.
00041
00042      SELECT SORT-FILE           ASSIGN TO
                                       SYS001-UT-FBA1-S-SORTWK1.
00044
00045      SELECT DISK-DATE           ASSIGN TO SYS019-UT-FBA1-S-SYS019.
00046
00047      SELECT PRNTR               ASSIGN TO SYS008-UR-1403-S-SYS008.
00048
00049      SELECT FICH                ASSIGN TO SYS020-UT-2400-S-SYS020.
00050

00064  DATA DIVISION.
00065
00066  FILE SECTION.
00067
00068  FD  REPORTS-EXTRACT-FILE
00069                                      COPY ELCEXTFD.
052804 01  FILLER                          PIC X(319).

00072
00073  SD  SORT-FILE.
                                           COPY ELCEXTR.


00076  FD  DISK-DATE                       COPY ELCDTEFD.
00077

00078  FD  PRNTR                           COPY ELCPRTFD .
00079

00080  FD  FICH                            COPY ELCFCHFD .


00089  WORKING-STORAGE SECTION.
00090
00091  77  FILLER PIC X(32) VALUE '********************************'.
00092  77  FILLER PIC X(32) VALUE '*     EL588 WORKING-STORAGE    *'.
00093  77  FILLER PIC X(32) VALUE '***********VMOD=2.014 **********'.
00094
00113
00114  01  FILLER                          COMP-3.
           05  WS-LINE-COUNT               PIC S9(03)    VALUE +56.
           05  WS-LINE-COUNT-MAX           PIC S9(03)    VALUE +55.
           05  WS-PAGE                     PIC S9(05)    VALUE +0.

           05  WS-OPEN-LIFE-CNT            PIC S9(05)    VALUE +0.
           05  WS-OPEN-DIS-CNT             PIC S9(05)    VALUE +0.
121603     05  WS-OPEN-GP-CNT              PIC S9(05)    VALUE +0.
122302     05  WS-OPEN-IU-CNT              PIC S9(05)    VALUE +0.
052614     05  WS-OPEN-FL-CNT              PIC S9(05)    VALUE +0.
022122     05  WS-OPEN-BR-CNT              PIC S9(05)    VALUE +0.
022122     05  WS-OPEN-HS-CNT              PIC S9(05)    VALUE +0.
100518     05  WS-OPEN-OT-CNT              PIC S9(05)    VALUE +0.
00125
00131      05  WS-QUARTER-TOTAL            PIC S9(07)V99 VALUE +0.
00139      05  WS-GRAND-TOTAL              PIC S9(09)V99 VALUE +0.

00123      05  WS-RETURN-CODE              PIC S9(03)    VALUE +0.
00124      05  WS-ZERO                     PIC S9(01)    VALUE +0.

00195
00196  01  FILLER                          COMP SYNC.
00197      05  PGM-SUB                     PIC S9(04)    VALUE +588.
00200
00201  01  FILLER.
           05  WS-EOF-SW1                  PIC X(01)     VALUE SPACE.
               88  END-OF-EXTRACT                        VALUE 'Y'.
00209
           05  WS-EOF-SW2                  PIC X(01)     VALUE SPACE.
               88  END-OF-SORTFILE                       VALUE 'Y'.

           05  WS-CERT-STATUS-SW           PIC X(01)     VALUE SPACE.
               88  ACTIVE-STATUS                         VALUE
                   '1' '2' '3' '4' '5' '9'.
               88  REIN-OR-NOT-IN-FORCE                  VALUE
                   'D' 'V'.

           05  WS-FIRST-TIME-SW            PIC X(01)     VALUE 'Y'.
               88  FIRST-SORT-RECORD                     VALUE 'Y'.
               88  NOT-FIRST-SORT-RECORD                 VALUE 'N'.

00209
00220      05  WS-ABEND-MESSAGE            PIC X(80)     VALUE SPACES.
00221
00222      05  WS-ABEND-FILE-STATUS        PIC X(02)     VALUE ZERO.
00223
00224      05  WS-FILE-ERROR-MESSAGE.
00225          10  FILLER                  PIC X(24)     VALUE
00226              'ERROR OCCURED OPENING - '.
00227          10  WS-FEM-FILE-NAME        PIC X(08).

           05  WS-HOLD-INCUR-QUARTER       PIC X(01)     VALUE SPACE.
           05  WS-HOLD-INCUR-YEAR          PIC X(04)     VALUE SPACES.

           05  WS-QUARTER-NO               PIC X(01)     VALUE SPACE.
00228
           05  WS-QUARTER-HEADER.
               10  FILLER                  PIC X(12)     VALUE
                   '*** QUARTER '.
               10  WS-QUARTER-HDR-VAR1     PIC X(01)     VALUE SPACE.
               10  FILLER                  PIC X(02)     VALUE SPACES.
               10  WS-QUARTER-HDR-VAR2     PIC X(04)     VALUE SPACES.
               10  FILLER                  PIC X(01)     VALUE ':'.

122302     05  WS-GRAND-TOTAL-HDR.
122302         10  WS-GT-CO-ID             PIC X(03)     VALUE
122302             'CID'.
122302         10  FILLER                  PIC X(23)     VALUE
122302             ' OPEN LIFE GRAND TOTAL:'.

           05  WS-TITLE-2                  PIC X(24)     VALUE
               'OPEN CLAIMS DISTRIBUTION'.


121603     05  WS-TOTAL-CNT-LIF-HDR        PIC X(29)     VALUE
               '      TOTAL OPEN LIFE CLAIMS:'.

121603     05  WS-TOTAL-CNT-DIS-HDR        PIC X(29)     VALUE
               'TOTAL OPEN DISABILITY CLAIMS:'.

121603     05  WS-TOTAL-CNT-GAP-HDR        PIC X(29)     VALUE
121603         '       TOTAL OPEN GAP CLAIMS:'.

121603     05  WS-TOTAL-CNT-IU-HDR         PIC X(29)     VALUE
122302         '        TOTAL OPEN IU CLAIMS:'.
052614
052614     05  WS-TOTAL-CNT-FAM-HDR        PIC X(29)     VALUE
052614         '       TOTAL OPEN FAM CLAIMS:'.
100518
022122     05  WS-TOTAL-CNT-BRV-HDR        PIC X(29)     VALUE
022122         '       TOTAL OPEN BRV CLAIMS:'.
022122
022122     05  WS-TOTAL-CNT-HOS-HDR        PIC X(29)     VALUE
022122         '       TOTAL OPEN HOS CLAIMS:'.
100518
100518     05  WS-TOTAL-CNT-OTH-HDR        PIC X(29)     VALUE
100518         '       TOTAL OPEN OTH CLAIMS:'.


       01  WS-HEADING1.
           05  FILLER                      PIC X(01)     VALUE '1'.
           05  FILLER                      PIC X(52)     VALUE SPACES.
           05  WS-H1-TITLE                 PIC X(24)     VALUE
               'OPEN CREDIT LIFE CLAIMS'.
           05  FILLER                      PIC X(44)     VALUE SPACES.
           05  WS-H1-REPORT-ID             PIC X(05)     VALUE 'EL588'.
           05  FILLER                      PIC X(07)     VALUE SPACES.


       01  WS-HEADING2.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  FILLER                      PIC X(49)     VALUE SPACES.
           05  WS-H2-COMPANY-NAME          PIC X(30)     VALUE SPACES.
           05  FILLER                      PIC X(41)     VALUE SPACES.
           05  WS-H2-DATE                  PIC X(08)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE SPACES.


       01  WS-HEADING3.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  FILLER                      PIC X(51)     VALUE SPACES.
           05  FILLER                      PIC X(10)     VALUE
               'MONTH-END '.
           05  WS-H3-MONTH-END-DT          PIC X(18)     VALUE SPACES.
           05  FILLER                      PIC X(41)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE 'PAGE'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-H3-PAGE                  PIC ZZ,ZZ9.
           05  FILLER                      PIC X(01)     VALUE SPACE.


       01  WS-HEADING4.
           05  FILLER                      PIC X(01)     VALUE '-'.
           05  FILLER                      PIC X(09)     VALUE
               'ACCOUNT #'.
061504     05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(07)     VALUE
               'CLAIM #'.
061504     05  FILLER                      PIC X(04)     VALUE SPACES.
           05  FILLER                      PIC X(10)     VALUE
               ' POLICY # '.
061504     05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'STATE'.
061504     05  FILLER                      PIC X(03)     VALUE SPACES.
061504     05  FILLER                      PIC X(13)     VALUE
061504         'CERT EFF DATE'.
061504     05  FILLER                      PIC X(03)     VALUE SPACES.
           05  FILLER                      PIC X(13)     VALUE
               'INCURRED DATE'.
061504     05  FILLER                      PIC X(03)     VALUE SPACES.
           05  FILLER                      PIC X(13)     VALUE
               'REPORTED DATE'.
           05  FILLER                      PIC X(04)     VALUE SPACES.
           05  FILLER                      PIC X(17)     VALUE
               'OPEN CLAIM AMOUNT'.
           05  FILLER                      PIC X(30)     VALUE SPACES.


       01  WS-DETAIL1.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  WS-D1-ACCOUNT-NO            PIC X(06)     VALUE SPACES.
061504     05  FILLER                      PIC X(04)     VALUE SPACES.
           05  WS-D1-CLAIM-NO              PIC X(07)     VALUE SPACES.
061504     05  FILLER                      PIC X(04)     VALUE SPACES.
           05  WS-D1-POLICY-NO             PIC X(10)     VALUE SPACES.
061504     05  FILLER                      PIC X(04)     VALUE SPACES.
           05  WS-D1-STATE                 PIC X(02)     VALUE SPACES.
061504     05  FILLER                      PIC X(05)     VALUE SPACES.
061504     05  WS-D1-CERT-EFF-DT           PIC X(10)     VALUE SPACES.
061504     05  FILLER                      PIC X(06)     VALUE SPACES.
           05  WS-D1-INCURRED-DT.
               10  WS-D1-INCURRED-MM       PIC X(02)     VALUE SPACES.
               10  FILLER                  PIC X(04)     VALUE SPACES.
               10  WS-D1-INCURRED-CCYY     PIC X(04)     VALUE SPACES.
061504     05  FILLER                      PIC X(06)     VALUE SPACES.
           05  WS-D1-REPORTED-DT           PIC X(10)     VALUE SPACES.
           05  FILLER                      PIC X(07)     VALUE SPACES.
           05  WS-D1-OPEN-CLAIM-AMT        PIC Z,ZZZ,ZZ9.99-.
061504     05  FILLER                      PIC X(28)     VALUE SPACES.


       01  WS-TOTAL-LN.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  FILLER                      PIC X(91)     VALUE SPACES.
           05  WS-TOTAL-LN-VAR2.
               10  FILLER                  PIC X(06)     VALUE SPACES.
               10  WS-TOTAL-LN-VAR         PIC X(20)     VALUE SPACES.
               10  WS-TOTAL-LN-OPEN-AMT    PIC $$Z,ZZZ,ZZ9.99-.
00569
       01  WS-TOTAL-CNT-LN.
           05  FILLER                      PIC X(01)     VALUE '0'.
           05  FILLER                      PIC X(10)     VALUE SPACES.
           05  WS-TOTAL-CNT-LN-VAR         PIC X(29)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE SPACES.
           05  WS-TOTAL-COUNT              PIC Z,ZZ9.
           05  FILLER                      PIC X(83)     VALUE SPACES.
00569
00570      COPY ELCDTECX.
00571
00572      COPY ELCDTEVR.
00573
00574      COPY ELCDATE.


00576  PROCEDURE DIVISION.
00577
00585  0000-DATE-CARD-READ. COPY ELCDTERX.
00586
00601  1000-MAIN-LOGIC.
00602
00603      PERFORM OPEN-FILES                THRU OPEN-FILES-EXIT
00604
030612     MOVE DTE-CLIENT                     TO WS-GT-CO-ID
030612
122302     IF DTE-CLIENT = 'DCC'
122302         MOVE '  OPEN DCC LIFE CLAIMS  ' TO WS-H1-TITLE
122302         MOVE 'DCC'                      TO WS-GT-CO-ID
122302     END-IF

00611      SORT SORT-FILE
00612          ON ASCENDING KEY EX-AA-INCURRED-DT
00619          INPUT PROCEDURE  2000-INPUT-PROCEDURE  THRU 2000-EXIT
00620          OUTPUT PROCEDURE 3000-OUTPUT-PROCEDURE THRU 3000-EXIT
00621
122302     IF WS-OPEN-LIFE-CNT NOT = +0
00622          IF SORT-RETURN > ZERO
00623              MOVE 'SORT FAILED'        TO WS-ABEND-MESSAGE
00624              MOVE SORT-RETURN          TO WS-RETURN-CODE
00625              PERFORM ABEND-PGM         THRU APS-EXIT
               END-IF
122302     END-IF  

122302******* OPEN LIFE AND DISABILITY COUNTS
122302     MOVE WS-TITLE-2                   TO WS-H1-TITLE
122302     MOVE WS-HEADING1                  TO PRT
122302     PERFORM 3900-WRITE                THRU 3900-EXIT
122302
122302     MOVE WS-HEADING2                  TO PRT
122302     PERFORM 3900-WRITE                THRU 3900-EXIT
122302
122302     MOVE 1                            TO WS-H3-PAGE
122302     MOVE WS-HEADING3                  TO PRT
122302     PERFORM 3900-WRITE                THRU 3900-EXIT
122302
122302     MOVE WS-TOTAL-CNT-LIF-HDR         TO WS-TOTAL-CNT-LN-VAR
122302     MOVE WS-OPEN-LIFE-CNT             TO WS-TOTAL-COUNT
122302     MOVE WS-TOTAL-CNT-LN              TO PRT
122302     PERFORM 3900-WRITE                THRU 3900-EXIT
122302
122302     MOVE WS-TOTAL-CNT-DIS-HDR         TO WS-TOTAL-CNT-LN-VAR
122302     MOVE WS-OPEN-DIS-CNT              TO WS-TOTAL-COUNT
122302     MOVE WS-TOTAL-CNT-LN              TO PRT
122302     PERFORM 3900-WRITE                THRU 3900-EXIT
122302
122302     IF DTE-CLIENT = 'DCC'
121603         MOVE WS-TOTAL-CNT-GAP-HDR     TO WS-TOTAL-CNT-LN-VAR
121603         MOVE WS-OPEN-GP-CNT           TO WS-TOTAL-COUNT
121603         MOVE WS-TOTAL-CNT-LN          TO PRT
121603         PERFORM 3900-WRITE            THRU 3900-EXIT

121603         MOVE WS-TOTAL-CNT-IU-HDR      TO WS-TOTAL-CNT-LN-VAR
122302         MOVE WS-OPEN-IU-CNT           TO WS-TOTAL-COUNT
122302         MOVE WS-TOTAL-CNT-LN          TO PRT
122302         PERFORM 3900-WRITE            THRU 3900-EXIT
052614
052614         MOVE WS-TOTAL-CNT-FAM-HDR     TO WS-TOTAL-CNT-LN-VAR
052614         MOVE WS-OPEN-FL-CNT           TO WS-TOTAL-COUNT
052614         MOVE WS-TOTAL-CNT-LN          TO PRT
052614         PERFORM 3900-WRITE            THRU 3900-EXIT

022122         MOVE WS-TOTAL-CNT-BRV-HDR     TO WS-TOTAL-CNT-LN-VAR
022122         MOVE WS-OPEN-BR-CNT           TO WS-TOTAL-COUNT
022122         MOVE WS-TOTAL-CNT-LN          TO PRT
022122         PERFORM 3900-WRITE            THRU 3900-EXIT

022122         MOVE WS-TOTAL-CNT-HOS-HDR     TO WS-TOTAL-CNT-LN-VAR
022122         MOVE WS-OPEN-HS-CNT           TO WS-TOTAL-COUNT
022122         MOVE WS-TOTAL-CNT-LN          TO PRT
022122         PERFORM 3900-WRITE            THRU 3900-EXIT

122302     END-IF

100518
100518     MOVE WS-TOTAL-CNT-OTH-HDR     TO WS-TOTAL-CNT-LN-VAR
100518     MOVE WS-OPEN-OT-CNT           TO WS-TOTAL-COUNT
100518     MOVE WS-TOTAL-CNT-LN          TO PRT
100518     PERFORM 3900-WRITE            THRU 3900-EXIT
 
           DISPLAY 'OPEN DISABILITY COUNT IS ' WS-OPEN-DIS-CNT

00627      PERFORM CLOSE-FILES               THRU CLOSE-FILES-EXIT
00628
00646      GOBACK

           .
00649  2000-INPUT-PROCEDURE.

           PERFORM 2010-GET-RESERVE-EXTRACTS THRU 2010-EXIT
               UNTIL END-OF-EXTRACT

           .
       2000-EXIT.
           EXIT.
00650
       2010-GET-RESERVE-EXTRACTS.

00658      READ REPORTS-EXTRACT-FILE         INTO REPORTS-EXTRACT-RECORD
               AT END
                   SET END-OF-EXTRACT        TO TRUE
                   GO TO 2010-EXIT
00661      END-READ
00663
00664      IF EX-POSITIONING-CODE > '1'
               SET END-OF-EXTRACT            TO TRUE
00665          GO TO 2010-EXIT
           END-IF
00666

00667      IF EX-EXTRACT-CODE > 'A'
               SET END-OF-EXTRACT            TO TRUE
00668          GO TO 2010-EXIT
           END-IF
00669

00670      IF EX-COMPANY-CD < DTE-CLASIC-COMPANY-CD
00671          GO TO 2010-EXIT
           END-IF
00672

00673      IF EX-COMPANY-CD > DTE-CLASIC-COMPANY-CD
               SET END-OF-EXTRACT            TO TRUE
00674          GO TO 2010-EXIT
           END-IF
00675

00676      IF EX-RECORD-TYPE > 'A'
               SET END-OF-EXTRACT            TO TRUE
00677          GO TO 2010-EXIT
           END-IF
00678
00687      MOVE EX-AA-CERT-STATUS            TO WS-CERT-STATUS-SW

122302     EVALUATE TRUE
122302     WHEN EX-AA-CLAIM-TYPE = 'L'
               CONTINUE

122302     WHEN ACTIVE-STATUS AND EX-AA-CLAIM-TYPE = 'A'
               ADD +1                        TO WS-OPEN-DIS-CNT
               GO TO 2010-EXIT

121603     WHEN ACTIVE-STATUS AND EX-AA-CLAIM-TYPE = 'G'
121603         ADD +1                        TO WS-OPEN-GP-CNT
121603         GO TO 2010-EXIT

122302     WHEN ACTIVE-STATUS AND EX-AA-CLAIM-TYPE = 'I'
122302         ADD +1                        TO WS-OPEN-IU-CNT
122302         GO TO 2010-EXIT
052614
052614     WHEN ACTIVE-STATUS AND EX-AA-CLAIM-TYPE = 'F'
052614         ADD +1                        TO WS-OPEN-FL-CNT
052614         GO TO 2010-EXIT
100518
022122     WHEN ACTIVE-STATUS AND EX-AA-CLAIM-TYPE = 'B'
022122         ADD +1                        TO WS-OPEN-BR-CNT
022122         GO TO 2010-EXIT
022122
022122     WHEN ACTIVE-STATUS AND EX-AA-CLAIM-TYPE = 'H'
022122         ADD +1                        TO WS-OPEN-HS-CNT
022122         GO TO 2010-EXIT
100518
100518     WHEN ACTIVE-STATUS AND EX-AA-CLAIM-TYPE = 'O'
100518         ADD +1                        TO WS-OPEN-OT-CNT
100518         GO TO 2010-EXIT

122302     WHEN OTHER
122302         GO TO 2010-EXIT
122302     END-EVALUATE
00681

           IF ACTIVE-STATUS
               ADD +1                        TO WS-OPEN-LIFE-CNT
00694          PERFORM 2200-RELEASE-SORT     THRU 2200-EXIT
           END-IF
00695
00707      .
       2010-EXIT.
           EXIT.


00708  2200-RELEASE-SORT.
00709
00710      RELEASE REPORTS-EXTRACT-RECORD
00711
00712      IF SORT-RETURN > ZERO
00713          MOVE 'ERROR ON RELEASE SORT'  TO WS-ABEND-MESSAGE
00714          MOVE SORT-RETURN              TO WS-RETURN-CODE
00715          PERFORM ABEND-PGM             THRU APS-EXIT
           END-IF
00716
           .
00738  2200-EXIT.
00739      EXIT.
00740

00741  3000-OUTPUT-PROCEDURE.

           MOVE COMPANY-NAME                 TO WS-H2-COMPANY-NAME
           MOVE WS-CURRENT-DATE              TO WS-H2-DATE
           MOVE ALPH-DATE                    TO WS-H3-MONTH-END-DT

00767      IF WS-OPEN-LIFE-CNT = +0
               PERFORM 3500-PRINT-HEADINGS   THRU 3500-EXIT
00770          MOVE '- NO OPEN LIFE CLAIMS TO REPORT ON'
                                             TO PRT
00771          PERFORM 3900-WRITE            THRU 3900-EXIT
               GO TO 3000-EXIT
           END-IF


           PERFORM 3010-RETURN-SORT          THRU 3010-EXIT
               UNTIL END-OF-SORTFILE

      ****** LAST SUBTOTAL
           PERFORM 3300-SUBTOTAL             THRU 3300-EXIT

           MOVE WS-GRAND-TOTAL-HDR           TO WS-TOTAL-LN-VAR2
           MOVE WS-GRAND-TOTAL               TO WS-TOTAL-LN-OPEN-AMT
           MOVE WS-TOTAL-LN                  TO PRT
           MOVE '-'                          TO P-CTL
           PERFORM 3900-WRITE                THRU 3900-EXIT

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

00755
00766      PERFORM 3200-BUILD-RPT-DETAIL     THRU 3200-EXIT

           .
       3010-EXIT.
           EXIT.
00773
01122  3200-BUILD-RPT-DETAIL.

           MOVE SPACES                         TO WS-DETAIL1
01123
01142      MOVE EX-AA-ACCOUNT(5:10)            TO WS-D1-ACCOUNT-NO
01143      MOVE EX-SA-CLAIM-NO                 TO WS-D1-CLAIM-NO
01144      MOVE EX-SA-CERT-NO                  TO WS-D1-POLICY-NO
01141      MOVE EX-AA-STATE                    TO WS-D1-STATE

061504     MOVE EX-AA-CERT-EFF-DT              TO DC-BIN-DATE-1
061504     MOVE SPACES                         TO DC-OPTION-CODE
061504     PERFORM 8500-DATE-CONVERSION        THRU 8500-EXIT
       
061504     IF NO-CONVERSION-ERROR
061504        MOVE DC-GREG-DATE-A-EDIT         TO WS-D1-CERT-EFF-DT
061504     ELSE
061504         DISPLAY 'INVALID CERT EFF DATE FOR CLAIM ' EX-SA-CLAIM-NO
061504         PERFORM ABEND-PGM               THRU APS-EXIT
061504     END-IF

01155      MOVE EX-AA-INCURRED-DT              TO DC-BIN-DATE-1
01156      MOVE SPACES                         TO DC-OPTION-CODE
01157      PERFORM 8500-DATE-CONVERSION        THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
01158         MOVE DC-GREG-DATE-A-EDIT         TO WS-D1-INCURRED-DT
           ELSE
               DISPLAY 'INVALID INCURRED DATE FOR CLAIM ' EX-SA-CLAIM-NO
               PERFORM ABEND-PGM               THRU APS-EXIT
           END-IF

01238      MOVE EX-AA-REPORTED-DT              TO DC-BIN-DATE-1
01239      MOVE SPACES                         TO DC-OPTION-CODE
01240      PERFORM 8500-DATE-CONVERSION        THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
01158         MOVE DC-GREG-DATE-A-EDIT         TO WS-D1-REPORTED-DT
           ELSE
               DISPLAY 'INVALID REPORTED DATE FOR CLAIM ' EX-SA-CLAIM-NO
               PERFORM ABEND-PGM               THRU APS-EXIT
           END-IF

           MOVE EX-AA-REMAINING-BENEFIT        TO WS-D1-OPEN-CLAIM-AMT
           ADD EX-AA-REMAINING-BENEFIT         TO WS-GRAND-TOTAL

           EVALUATE TRUE
           WHEN WS-D1-INCURRED-MM = '01' OR '02' OR '03'
               MOVE '1'                        TO WS-QUARTER-NO


           WHEN WS-D1-INCURRED-MM = '04' OR '05' OR '06'
               MOVE '2'                        TO WS-QUARTER-NO


           WHEN WS-D1-INCURRED-MM = '07' OR '08' OR '09'
               MOVE '3'                        TO WS-QUARTER-NO


           WHEN WS-D1-INCURRED-MM = '10' OR '11' OR '12'
               MOVE '4'                        TO WS-QUARTER-NO

           END-EVALUATE


           IF FIRST-SORT-RECORD
               MOVE WS-D1-INCURRED-CCYY        TO WS-HOLD-INCUR-YEAR
               MOVE WS-D1-INCURRED-CCYY        TO WS-QUARTER-HDR-VAR2
               MOVE WS-QUARTER-NO              TO WS-HOLD-INCUR-QUARTER
               SET NOT-FIRST-SORT-RECORD       TO TRUE
           END-IF


           IF WS-D1-INCURRED-CCYY = WS-HOLD-INCUR-YEAR
               IF WS-QUARTER-NO = WS-HOLD-INCUR-QUARTER
                   ADD EX-AA-REMAINING-BENEFIT TO WS-QUARTER-TOTAL
               ELSE
                   PERFORM 3300-SUBTOTAL       THRU 3300-EXIT
               END-IF
           ELSE
               PERFORM 3300-SUBTOTAL           THRU 3300-EXIT
               MOVE WS-D1-INCURRED-CCYY        TO WS-HOLD-INCUR-YEAR
           END-IF

           IF WS-LINE-COUNT > WS-LINE-COUNT-MAX
               PERFORM 3500-PRINT-HEADINGS     THRU 3500-EXIT
           END-IF

01191      MOVE WS-DETAIL1                     TO PRT
           PERFORM 3900-WRITE                  THRU 3900-EXIT
01242
01413      .
       3200-EXIT.
01414      EXIT.

       3300-SUBTOTAL.

           MOVE WS-HOLD-INCUR-QUARTER          TO WS-QUARTER-HDR-VAR1
           MOVE WS-HOLD-INCUR-YEAR             TO WS-QUARTER-HDR-VAR2
           MOVE WS-QUARTER-HEADER              TO WS-TOTAL-LN-VAR
           MOVE WS-QUARTER-TOTAL               TO WS-TOTAL-LN-OPEN-AMT
           MOVE WS-TOTAL-LN                    TO PRT
           MOVE '0'                            TO P-CTL
           PERFORM 3900-WRITE                  THRU 3900-EXIT

           MOVE SPACES                         TO PRT
           MOVE '-'                            TO P-CTL
           PERFORM 3900-WRITE                  THRU 3900-EXIT

           MOVE WS-QUARTER-NO                  TO WS-HOLD-INCUR-QUARTER
           MOVE +0                             TO WS-QUARTER-TOTAL
           MOVE +0                             TO WS-TOTAL-LN-OPEN-AMT
           MOVE EX-AA-REMAINING-BENEFIT        TO WS-QUARTER-TOTAL

           .
       3300-EXIT.
           EXIT.


       3500-PRINT-HEADINGS.

           MOVE WS-HEADING1                 TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

           MOVE WS-HEADING2                 TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

           ADD +1                           TO WS-PAGE
           MOVE WS-PAGE                     TO WS-H3-PAGE
           MOVE WS-HEADING3                 TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

           MOVE WS-HEADING4                 TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

           MOVE SPACES                      TO PRT
           MOVE '0'                         TO P-CTL
           PERFORM 3900-WRITE               THRU 3900-EXIT

           .
       3500-EXIT.
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



           EVALUATE TRUE

      **************************** P = HARDCOPY ONLY
           WHEN DTE-PRT-OPT = 'P'
               WRITE PRT

      **************************** B = SEQUENTIAL FILE ARCHIVE (USED TO
      *                                 BE FICHE) AND HARDCOPY
           WHEN DTE-PRT-OPT = 'B'
               PERFORM 4000-FICH-ARCHIVE    THRU 4000-EXIT
               WRITE PRT

      **************************** S = ONLINE REPORT FILE ONLY (ELREPT)
      **************************** T = ONLINE REPORT FILE AND HARDCOPY
           WHEN OTHER
               DISPLAY 'PGM EL588 ONLY SET UP FOR PRT OPT P AND B'
               PERFORM ABEND-PGM            THRU APS-EXIT

           END-EVALUATE

           .
       3900-EXIT.
           EXIT.


       4000-FICH-ARCHIVE.
      ******************************************************************
      * WHILE FICHE IS NO LONGER BEING PRODUCED, THE FICHE LOGIC IS
      * BEING USED TO CREATE A SEQUENTIAL FILE REPORT ARCHIVE WHEN
      * THE DTE-PRT-OPT IS SET TO 'B'
      ******************************************************************
           IF FICH-NO
               GO TO 4000-EXIT
           ELSE
               IF FICH-OPEN = SPACE
                   MOVE 'X'                 TO FICH-OPEN
                   OPEN OUTPUT FICH
               END-IF
           END-IF

           MOVE SPACE                       TO P-CTL
           WRITE FICH-REC FROM PRT

           .
       4000-EXIT.
           EXIT.


01680  8500-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8500-EXIT.
           EXIT.
01681
01837  OPEN-FILES.
01838
01840      OPEN  INPUT  REPORTS-EXTRACT-FILE
01841            OUTPUT PRNTR
01842
           .
01897  OPEN-FILES-EXIT.
01898      EXIT.
01899
01901
01902  CLOSE-FILES.

           IF FICH-OPEN NOT = SPACE
               CLOSE FICH
100302     END-IF

01906      CLOSE REPORTS-EXTRACT-FILE
01907            PRNTR

            .
       CLOSE-FILES-EXIT.
           EXIT.
01908
01912  ABEND-PGM SECTION. COPY ELCABEND .
