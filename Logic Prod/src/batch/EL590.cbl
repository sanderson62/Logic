00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL590.


      *AUTHOR.     SUZAN VUKOV.
      ******************************************************************
00025 *REMARKS.
00026 *        THIS PROGRAM CREATES AN OPEN LIFE CLAIMS REPORT
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 092702    2002083000006  SMVA  NEW REQUEST
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
00045      SELECT DISK-DATE           ASSIGN TO SYS019-UT-FBA1-S-SYS019.
00046
00047      SELECT PRNTR               ASSIGN TO SYS008-UR-1403-S-SYS008.
00048

00064  DATA DIVISION.
00065
00066  FILE SECTION.
00067
00068  FD  REPORTS-EXTRACT-FILE
00069                                      COPY ELCEXTFD.
                                           COPY ELCEXTR.

00072
00076  FD  DISK-DATE                       COPY ELCDTEFD.
00077

00078  FD  PRNTR                           COPY ELCPRTFD .
00079

00089  WORKING-STORAGE SECTION.
00090
00091  77  FILLER PIC X(32) VALUE '********************************'.
00092  77  FILLER PIC X(32) VALUE '*     EL590 WORKING-STORAGE    *'.
00093  77  FILLER PIC X(32) VALUE '***********VMOD=2.014 **********'.
00094
00113
00114  01  FILLER                          COMP-3.
           05  WS-LINE-COUNT               PIC S9(03)    VALUE +56.
           05  WS-LINE-COUNT-MAX           PIC S9(03)    VALUE +55.
           05  WS-PAGE                     PIC S9(05)    VALUE +0.

00122      05  WS-TOT-LARGE-LIFE-CLAIMS    PIC S9(05)    VALUE +0.
00125
00123      05  WS-RETURN-CODE              PIC S9(03)    VALUE +0.
00124      05  WS-ZERO                     PIC S9(01)    VALUE +0.

00196  01  FILLER                          COMP SYNC.
00197      05  PGM-SUB                     PIC S9(04)    VALUE +590.
00200
00201  01  FILLER.
           05  WS-EOF-SW1                  PIC X(01)     VALUE SPACE.
               88  END-OF-EXTRACT                        VALUE 'Y'.
00209
           05  WS-CERT-STATUS-SW           PIC X(01)     VALUE SPACE.
               88  ACTIVE-STATUS                         VALUE
                   '1' '2' '3' '4' '5' '9'.
               88  REIN-OR-NOT-IN-FORCE                  VALUE
                   'D' 'V'.

00220      05  WS-ABEND-MESSAGE            PIC X(80)     VALUE SPACES.
00221
00222      05  WS-ABEND-FILE-STATUS        PIC X(02)     VALUE ZERO.
00223

       01  WS-HEADING1.
           05  FILLER                      PIC X(01)     VALUE '1'.
           05  FILLER                      PIC X(50)     VALUE SPACES.
           05  WS-H1-TITLE                 PIC X(29)     VALUE
               'OPEN LIFE CLAIMS OVER $20,000'.
           05  FILLER                      PIC X(41)     VALUE SPACES.
           05  WS-H1-REPORT-ID             PIC X(05)     VALUE 'EL590'.
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
           05  FILLER                      PIC X(46)     VALUE SPACES.
           05  FILLER                      PIC X(20)     VALUE
               'FOR THE WEEK ENDING '.
           05  WS-H3-WEEK-END-DT           PIC X(18)     VALUE SPACES.
           05  FILLER                      PIC X(36)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE 'PAGE'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-H3-PAGE                  PIC ZZ,ZZ9.
           05  FILLER                      PIC X(01)     VALUE SPACE.


       01  WS-HEADING4.
           05  FILLER                      PIC X(01)     VALUE '-'.
           05  FILLER                      PIC X(09)     VALUE
               'ACCOUNT #'.
           05  FILLER                      PIC X(05)     VALUE SPACES.
           05  FILLER                      PIC X(07)     VALUE
               'CLAIM #'.
           05  FILLER                      PIC X(05)     VALUE SPACES.
           05  FILLER                      PIC X(10)     VALUE
               ' POLICY # '.
           05  FILLER                      PIC X(05)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'STATE'.
           05  FILLER                      PIC X(05)     VALUE SPACES.
           05  FILLER                      PIC X(13)     VALUE
               'INCURRED DATE'.
           05  FILLER                      PIC X(04)     VALUE SPACES.
           05  FILLER                      PIC X(13)     VALUE
               'REPORTED DATE'.
           05  FILLER                      PIC X(04)     VALUE SPACES.
           05  FILLER                      PIC X(17)     VALUE
               'OPEN CLAIM AMOUNT'.
           05  FILLER                      PIC X(30)     VALUE SPACES.


       01  WS-DETAIL1.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  WS-D1-ACCOUNT-NO            PIC X(06)     VALUE SPACES.
           05  FILLER                      PIC X(08)     VALUE SPACES.
           05  WS-D1-CLAIM-NO              PIC X(07)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE SPACES.
           05  WS-D1-POLICY-NO             PIC X(10)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE SPACES.
           05  WS-D1-STATE                 PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(07)     VALUE SPACES.
           05  WS-D1-INCURRED-DT.
               10  WS-D1-INCURRED-MM       PIC X(02)     VALUE SPACES.
               10  FILLER                  PIC X(04)     VALUE SPACES.
               10  WS-D1-INCURRED-CCYY     PIC X(04)     VALUE SPACES.
           05  FILLER                      PIC X(07)     VALUE SPACES.
           05  WS-D1-REPORTED-DT           PIC X(10)     VALUE SPACES.
           05  FILLER                      PIC X(07)     VALUE SPACES.
           05  WS-D1-OPEN-CLAIM-AMT        PIC Z,ZZZ,ZZ9.99-.
           05  FILLER                      PIC X(30)     VALUE SPACES.


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
00603      PERFORM OPEN-FILES                 THRU OPEN-FILES-EXIT

           MOVE COMPANY-NAME                  TO WS-H2-COMPANY-NAME
           MOVE WS-CURRENT-DATE               TO WS-H2-DATE
           MOVE ALPH-DATE                     TO WS-H3-WEEK-END-DT 
00604
00619      PERFORM  2000-GET-RESERVE-EXTRACTS THRU 2000-EXIT
               UNTIL END-OF-EXTRACT

00767      IF WS-TOT-LARGE-LIFE-CLAIMS = +0
               PERFORM 3500-PRINT-HEADINGS   THRU 3500-EXIT
00770          MOVE '- NO LIFE CLAIMS OVER $20,000 TO REPORT ON'
                                             TO PRT
00771          PERFORM 3900-WRITE            THRU 3900-EXIT
           END-IF
00621
00627      PERFORM CLOSE-FILES                THRU CLOSE-FILES-EXIT
00628
00646      GOBACK

           .

       2000-GET-RESERVE-EXTRACTS.

00658      READ REPORTS-EXTRACT-FILE 
               AT END
                   SET END-OF-EXTRACT        TO TRUE
                   GO TO 2000-EXIT
00661      END-READ
00663
00664      IF EX-POSITIONING-CODE > '1'
               SET END-OF-EXTRACT            TO TRUE
00665          GO TO 2000-EXIT
           END-IF
00666

00667      IF EX-EXTRACT-CODE > 'A'
               SET END-OF-EXTRACT            TO TRUE
00668          GO TO 2000-EXIT
           END-IF
00669

00670      IF EX-COMPANY-CD < DTE-CLASIC-COMPANY-CD
00671          GO TO 2000-EXIT
           END-IF
00672

00673      IF EX-COMPANY-CD > DTE-CLASIC-COMPANY-CD
               SET END-OF-EXTRACT            TO TRUE
00674          GO TO 2000-EXIT
           END-IF
00675

00676      IF EX-RECORD-TYPE > 'A'
               SET END-OF-EXTRACT            TO TRUE
00677          GO TO 2000-EXIT
           END-IF
00678
00687      MOVE EX-AA-CERT-STATUS            TO WS-CERT-STATUS-SW

           IF EX-AA-CLAIM-TYPE = 'L'
               CONTINUE
           ELSE
               GO TO 2000-EXIT
           END-IF
00681

           IF (ACTIVE-STATUS AND
               EX-AA-REMAINING-BENEFIT > +20000) 
               ADD +1                        TO WS-TOT-LARGE-LIFE-CLAIMS
               PERFORM 3000-BUILD-RPT-DETAIL THRU 3000-EXIT
           END-IF
00695
00707      .
       2000-EXIT.
           EXIT.


01122  3000-BUILD-RPT-DETAIL.

           MOVE SPACES                         TO WS-DETAIL1
01123
01142      MOVE EX-AA-ACCOUNT(5:10)            TO WS-D1-ACCOUNT-NO
01143      MOVE EX-SA-CLAIM-NO                 TO WS-D1-CLAIM-NO
01144      MOVE EX-SA-CERT-NO                  TO WS-D1-POLICY-NO
01141      MOVE EX-AA-STATE                    TO WS-D1-STATE

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

           IF WS-LINE-COUNT > WS-LINE-COUNT-MAX
               PERFORM 3500-PRINT-HEADINGS     THRU 3500-EXIT
           END-IF

01191      MOVE WS-DETAIL1                     TO PRT
           PERFORM 3900-WRITE                  THRU 3900-EXIT
01242
01413      .
       3000-EXIT.
01414      EXIT.


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


           WRITE PRT


           .
       3900-EXIT.
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

01906      CLOSE REPORTS-EXTRACT-FILE
01907            PRNTR

            .
       CLOSE-FILES-EXIT.
           EXIT.
01908
01912  ABEND-PGM SECTION. COPY ELCABEND .
