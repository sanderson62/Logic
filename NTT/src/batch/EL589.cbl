       IDENTIFICATION DIVISION.

       PROGRAM-ID.                 EL589.


      *AUTHOR.     SUZAN VUKOV.
      ******************************************************************
      *REMARKS.
      *        THIS PROGRAM CREATES A MONTH-END OPEN A&H CLAIMS FILE
      *        SORTED BY PLAN CODE, INCURRED YEAR, AND INCURRED MONTH
      *        USED BY ACTUARY IN FOXPRO TO CREATE CASE RESERVE REPORTS
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 031902    2001120300002  SMVA  INITIAL IMPL FOR UNIKIX
      *                                REWRITE OF MAINFRAME CULPRIT
      * 080102    2002080100004  SMVA  REMOVE ABEND ROUTINE WHEN DATE
      *                                CONV. FAILS AND BYPASS RECORD
122302* 122302    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
121603* 121603    2003080800002  SMVA  ADD PROCESSING FOR NEW CLM TYP G
041105* 041105                   PEMA  ADD PTC RESERVE TO EXTRACT FILE
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT REPORTS-EXTRACT-FILE
                                      ASSIGN TO SYS010-UT-2400-S-SYS010.

           SELECT SORT-FILE           ASSIGN TO
                                       SYS001-UT-FBA1-S-SORTWK1.

           SELECT DISK-DATE           ASSIGN TO SYS019-UT-FBA1-S-SYS019.

           SELECT CICLCR-TXTFILE-OUT  ASSIGN TO SYS008-UR-1403-S-SYS011
                                       ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.

       FILE SECTION.

       FD  REPORTS-EXTRACT-FILE            COPY ELCEXTFD.
                                           COPY ELCEXTR.


       FD  DISK-DATE                       COPY ELCDTEFD.


       SD  SORT-FILE.
       01  SORT-RECORD.
           05  SORT-CLAIM-NUMBER            PIC X(10)      VALUE SPACES.
           05  FILLER                       PIC X(10)      VALUE SPACES.
           05  SORT-PLAN-CODE               PIC X(03)      VALUE SPACES.
           05  FILLER                       PIC X(11)      VALUE SPACES.
           05  SORT-INCURRED-DT.
               10  SORT-INCURRED-CCYY       PIC X(04)      VALUE SPACES.
               10  SORT-INCURRED-MM         PIC X(02)      VALUE SPACES.
041105     05  FILLER                       PIC X(88)      VALUE SPACES.


       FD  CICLCR-TXTFILE-OUT.
       01  CICLCR-RECORD                    PIC X(128).


       WORKING-STORAGE SECTION.

       77  FILLER PIC X(32) VALUE '********************************'.
       77  FILLER PIC X(32) VALUE '*     EL589 WORKING-STORAGE    *'.
       77  FILLER PIC X(32) VALUE '***********VMOD=2.014 **********'.

       01  WS-WORK-FIELDS                   COMP-3.
           05  WS-MONTHS                    PIC S9(03)     VALUE +0.
           05  WS-TOTAL-MONTHS              PIC S9(03)     VALUE +0.
           05  WS-YEARS                     PIC S9(03)     VALUE +0.
           05  WS-YEARS-IN-MONTHS           PIC S9(03)     VALUE +0.
           05  WS-YEAR1-WORKFIELD           PIC S9(04)     VALUE +0.
           05  WS-YEAR2-WORKFIELD           PIC S9(04)     VALUE +0.
           05  WS-MONTH1-WORKFIELD          PIC S9(02)     VALUE +0.
           05  WS-MONTH2-WORKFIELD          PIC S9(02)     VALUE +0.

           05  WS-MAX-BENEFIT-AMT           PIC S9(08)V99  VALUE +0.
           05  WS-RESERVE-LIABILITY         PIC S9(06)V99  VALUE +0.
           05  WS-BENEFIT-DUR               PIC S9(03)     VALUE +0.
           05  WS-BEN-DUR-REMAIN-IN-MOS     PIC S9(03)     VALUE +0.
           05  WS-BEN-DUR-REMAIN-IN-YRS     PIC S9(03)     VALUE +0.
           05  WS-ODD-MOS-REMAINING         PIC S9(03)     VALUE +0.

       01  FILLER                           COMP-3.
           05  WS-OPEN-DIS-CNT              PIC S9(05)     VALUE +0.
121603     05  WS-OPEN-GAP-CNT              PIC S9(05)     VALUE +0.
122302     05  WS-OPEN-IU-CNT               PIC S9(05)     VALUE +0.
052614     05  WS-OPEN-FAM-CNT              PIC S9(05)     VALUE +0.
022122     05  WS-OPEN-BRV-CNT              PIC S9(05)     VALUE +0.
022122     05  WS-OPEN-HOS-CNT              PIC S9(05)     VALUE +0.

           05  WS-RETURN-CODE               PIC S9(03)     VALUE +0.
           05  WS-ZERO                      PIC S9(01)     VALUE +0.

       01  FILLER                           COMP SYNC.
           05  PGM-SUB                      PIC S9(04)     VALUE +589.

       01  FILLER.
           05  WS-POLICY-EXPIRE-DT          PIC X(02)      VALUE SPACES.

           05  WS-EOF-SW1                   PIC X(01)      VALUE SPACE.
               88  END-OF-EXTRACT                          VALUE 'Y'.

           05  WS-EOF-SW2                   PIC X(01)      VALUE SPACE.
               88  END-OF-SORTFILE                         VALUE 'Y'.

           05  WS-ABEND-MESSAGE             PIC X(80)      VALUE SPACES.
           05  WS-ABEND-FILE-STATUS         PIC X(02)      VALUE ZERO.


       01  WS-DETAIL1.
           05  WS-D1-CLAIM-NO               PIC X(10)      VALUE SPACES.
           05  WS-D1-POLICY-NO              PIC X(10)      VALUE SPACES.
           05  WS-D1-PLAN-CD-POS1           PIC X(01)      VALUE SPACE.
           05  WS-D1-PLAN-CD-POS2-3         PIC X(02)      VALUE SPACE.
           05  FILLER                       PIC X(03)      VALUE SPACES.
           05  WS-D1-EFF-DT-MDCY            PIC X(08)      VALUE SPACES.
           05  WS-D1-INCURRED-DT.
               10  WS-D1-INCURRED-CCYY      PIC X(04)      VALUE SPACES.
               10  WS-D1-INCURRED-MM        PIC X(02)      VALUE SPACES.
           05  FILLER                       PIC X(02)      VALUE SPACES.
      *    05  WS-D1-STATE                  PIC X(02)      VALUE SPACES.
           05  WS-D1-MONTHLY-BENEFIT        PIC ZZZZ9.99   VALUE ZEROS.
           05  WS-D1-TOTAL-PAID-AMT         PIC ZZZZZZ9.99 VALUE ZEROS.
           05  WS-D1-RESERVE-LIABILITY      PIC ZZZZZZ9.99 VALUE ZEROS.
           05  WS-D1-INSURED-NAME           PIC X(20)      VALUE SPACES.
           05  WS-D1-INSURED-DOB-CYMD       PIC X(08)      VALUE SPACES.
           05  WS-D1-AGE-AT-DISABILITY      PIC 9(02)      VALUE ZEROS.
           05  WS-D1-BENEFIT-DUR            PIC 9(03)      VALUE ZEROS.
           05  WS-D1-REMAIN-BEN-IN-MOS      PIC 9(03)      VALUE ZEROS.
           05  WS-D1-REMAIN-BEN-IN-YRS      PIC 9(03)      VALUE ZEROS.
           05  WS-D1-CLAIM-AGE-YEARS        PIC 9(03)      VALUE ZEROS.
           05  WS-D1-CLAIM-AGE-IN-MOS       PIC 9(03)      VALUE ZEROS.
           05  WS-D1-EXCEPTION-FLAG         PIC X(01)      VALUE SPACE.
           05  WS-D1-RESERVE-CAT            PIC X(02)      VALUE SPACES.
           05  WS-D1-PTC-RESERVE            PIC ZZZZZZ9.99 VALUE ZEROS.

           COPY ELCDTECX.

           COPY ELCDTEVR.

           COPY ELCDATE.


       LINKAGE SECTION.

       01  PARM.
           05  PARM-LENGTH        COMP      PIC S9(04)     VALUE ZEROS.
           05  PARM-MONTH-END-DT.
               10  PARM-MONTH-END-CCYY      PIC X(04)      VALUE SPACES.
               10  PARM-MONTH-END-MM        PIC X(02)      VALUE SPACES.
               10  PARM-MONTH-END-DD        PIC X(02)      VALUE SPACES.


       PROCEDURE DIVISION USING PARM.

       0000-DATE-CARD-READ.                     COPY ELCDTERX.

       1000-MAIN-LOGIC.

           PERFORM OPEN-FILES                   THRU OPEN-FILES-EXIT

           PERFORM 1500-EDIT-CYCLE-DATE         THRU 1500-EXIT

           SORT SORT-FILE
               ON ASCENDING KEY SORT-PLAN-CODE   SORT-INCURRED-DT
                                        SORT-CLAIM-NUMBER
               INPUT PROCEDURE  2000-INPUT-PROCEDURE  THRU 2000-EXIT
               OUTPUT PROCEDURE 3000-OUTPUT-PROCEDURE THRU 3000-EXIT

           IF SORT-RETURN > ZERO
               MOVE 'SORT FAILED'               TO WS-ABEND-MESSAGE
               MOVE SORT-RETURN                 TO WS-RETURN-CODE
               PERFORM ABEND-PGM                THRU APS-EXIT
           END-IF

           DISPLAY 'OPEN DISABILITY CLAIM COUNT IS ' WS-OPEN-DIS-CNT
121603     DISPLAY 'OPEN   GAP      CLAIM COUNT IS ' WS-OPEN-GAP-CNT
122302     DISPLAY 'OPEN UNEMPLOYMENT CLAIM COUNT IS ' WS-OPEN-IU-CNT
052614     DISPLAY 'OPEN FAMILY LEAVE CLAIM COUNT IS ' WS-OPEN-FAM-CNT
022122     DISPLAY 'OPEN BEREAVEMENT  CLAIM COUNT IS ' WS-OPEN-BRV-CNT
022122     DISPLAY 'OPEN HOSPITAL     CLAIM COUNT IS ' WS-OPEN-HOS-CNT

           PERFORM CLOSE-FILES                  THRU CLOSE-FILES-EXIT

           GOBACK

           .
       1000-EXIT.
           EXIT.


       1500-EDIT-CYCLE-DATE.

           IF PARM-LENGTH = +0
               DISPLAY 'DATE PARM MISSING'
               PERFORM ABEND-PGM                THRU APS-EXIT
           END-IF

           MOVE PARM-MONTH-END-DT               TO DC-GREG-DATE-CYMD
           MOVE 'L'                             TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION         THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               CONTINUE
           ELSE
               DISPLAY 'INVALID PARM DATE ' PARM-MONTH-END-DT
               PERFORM ABEND-PGM                THRU APS-EXIT
           END-IF

           .
       1500-EXIT.
           EXIT.


       2000-INPUT-PROCEDURE.

           PERFORM 2010-GET-RESERVE-EXTRACTS THRU 2010-EXIT
               UNTIL END-OF-EXTRACT

           .
       2000-EXIT.
           EXIT.


       2010-GET-RESERVE-EXTRACTS.

           READ REPORTS-EXTRACT-FILE
               AT END
                   SET END-OF-EXTRACT        TO TRUE
                   GO TO 2010-EXIT
           END-READ

           IF EX-POSITIONING-CODE > '1'
               SET END-OF-EXTRACT            TO TRUE
               GO TO 2010-EXIT
           END-IF


           IF EX-EXTRACT-CODE > 'A'
               SET END-OF-EXTRACT            TO TRUE
               GO TO 2010-EXIT
           END-IF


           IF EX-COMPANY-CD < DTE-CLASIC-COMPANY-CD
               GO TO 2010-EXIT
           END-IF


           IF EX-COMPANY-CD > DTE-CLASIC-COMPANY-CD
               SET END-OF-EXTRACT            TO TRUE
               GO TO 2010-EXIT
           END-IF


           IF EX-RECORD-TYPE > 'A'
               SET END-OF-EXTRACT            TO TRUE
               GO TO 2010-EXIT
           END-IF


052614     IF ((EX-AA-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
022122         OR 'B' OR 'H')   AND
                EX-AA-ORIG-TERM NOT = SPACES     AND
                EX-AA-ORIG-TERM NOT = LOW-VALUES AND
                EX-AA-ORIG-TERM NOT = ZEROS)
               CONTINUE
           ELSE
               GO TO 2010-EXIT
           END-IF

121603     EVALUATE TRUE 
121603     WHEN EX-AA-CLAIM-TYPE = 'A'
               ADD +1                        TO WS-OPEN-DIS-CNT

121603     WHEN EX-AA-CLAIM-TYPE = 'G'
121603         ADD +1                        TO WS-OPEN-GAP-CNT

121603     WHEN EX-AA-CLAIM-TYPE = 'I'
122302         ADD +1                        TO WS-OPEN-IU-CNT
052614
052614     WHEN EX-AA-CLAIM-TYPE = 'F'
052614         ADD +1                        TO WS-OPEN-FAM-CNT

022122     WHEN EX-AA-CLAIM-TYPE = 'B'
022122         ADD +1                        TO WS-OPEN-BRV-CNT
022122
022122     WHEN EX-AA-CLAIM-TYPE = 'H'
022122         ADD +1                        TO WS-OPEN-HOS-CNT

121603     END-EVALUATE

           PERFORM 2100-BUILD-OUTFILE-REC    THRU 2100-EXIT

           .
       2010-EXIT.
           EXIT.


       2100-BUILD-OUTFILE-REC.

           INITIALIZE WS-DETAIL1
           INITIALIZE WS-WORK-FIELDS

           MOVE EX-SA-CLAIM-NO               TO WS-D1-CLAIM-NO
           MOVE EX-SA-CERT-NO(2:11)          TO WS-D1-POLICY-NO
           MOVE EX-AA-IND-GRP                TO WS-D1-PLAN-CD-POS1
           MOVE EX-AA-BENEFIT-CODE           TO WS-D1-PLAN-CD-POS2-3

           MOVE EX-AA-CERT-EFF-DT            TO DC-BIN-DATE-1
           MOVE SPACE                        TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION      THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-MDCY        TO WS-D1-EFF-DT-MDCY
           ELSE
               DISPLAY 'INVALID CERT EFF DATE FOR CERT ' EX-SA-CERT-NO
080102         GO TO 2100-EXIT
           END-IF


           IF (EX-AA-INCURRED-DT NOT= LOW-VALUES AND
               EX-AA-INCURRED-DT NOT= SPACES)
               CONTINUE
           ELSE
               GO TO 2100-EXIT
           END-IF

           MOVE EX-AA-INCURRED-DT            TO DC-BIN-DATE-1
           MOVE SPACE                        TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION      THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-CYMD(1:6)   TO WS-D1-INCURRED-DT
           ELSE
               DISPLAY 'INVALID INCURRED DATE FOR CLAIM ' EX-SA-CLAIM-NO
080102         GO TO 2100-EXIT
           END-IF

      ***** COMPUTE CLAIM AGE FROM INCURRED TO CURRENT YEAR AND MONTH
           IF PARM-MONTH-END-CCYY >= WS-D1-INCURRED-CCYY
               MOVE PARM-MONTH-END-CCYY      TO WS-YEAR1-WORKFIELD
               MOVE WS-D1-INCURRED-CCYY      TO WS-YEAR2-WORKFIELD
               MOVE PARM-MONTH-END-MM        TO WS-MONTH1-WORKFIELD
               MOVE WS-D1-INCURRED-MM        TO WS-MONTH2-WORKFIELD
               COMPUTE WS-YEARS-IN-MONTHS =
                   (WS-YEAR1-WORKFIELD - WS-YEAR2-WORKFIELD) * 12
               END-COMPUTE

               SUBTRACT WS-MONTH2-WORKFIELD  FROM WS-MONTH1-WORKFIELD
                   GIVING WS-MONTHS

               ADD WS-YEARS-IN-MONTHS TO WS-MONTHS
                   GIVING WS-TOTAL-MONTHS

               DIVIDE WS-TOTAL-MONTHS BY 12
                   GIVING WS-YEARS ROUNDED

               MOVE WS-YEARS                 TO WS-D1-CLAIM-AGE-YEARS
               MOVE WS-TOTAL-MONTHS          TO WS-D1-CLAIM-AGE-IN-MOS
           ELSE
               GO TO 2100-EXIT
           END-IF

      ***** EX-AA-STATE IS NOT POLICY OWNER STATE
      *    MOVE EX-AA-STATE                  TO WS-D1-STATE
           MOVE EX-AA-BENEFIT-AMT            TO WS-D1-MONTHLY-BENEFIT
           MOVE EX-AA-TOTAL-PAID-AMT         TO WS-D1-TOTAL-PAID-AMT
           MOVE EX-AA-INSURED-NAME           TO WS-D1-INSURED-NAME


           IF (EX-AA-INSURED-BIRTH-DT NOT= LOW-VALUES AND
               EX-AA-INSURED-BIRTH-DT NOT= SPACES)
               MOVE EX-AA-INSURED-BIRTH-DT   TO DC-BIN-DATE-1
               MOVE SPACE                    TO DC-OPTION-CODE
               PERFORM 8500-DATE-CONVERSION  THRU 8500-EXIT

               IF NO-CONVERSION-ERROR
                   MOVE DC-GREG-DATE-CYMD    TO WS-D1-INSURED-DOB-CYMD
               ELSE
                   DISPLAY 'INVALID INSURED BIRTH DATE FOR CLAIM '
                                             EX-SA-CLAIM-NO
080102             GO TO 2100-EXIT
               END-IF
           END-IF


           MOVE EX-AA-INCURRED-AGE           TO WS-D1-AGE-AT-DISABILITY


      ***** CALCULATE TERM OF POLICY / LOAN EXPIRATION DATE
           MOVE EX-AA-CERT-EFF-DT            TO DC-BIN-DATE-1
           MOVE EX-AA-ORIG-TERM              TO DC-ELAPSED-MONTHS
           MOVE '6'                          TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION      THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-2            TO WS-POLICY-EXPIRE-DT
           ELSE
               DISPLAY 'ERROR ON EXPIRE DATE CALCULATION ' EX-SA-CERT-NO
080102         GO TO 2100-EXIT
           END-IF


      ***** CALCULATE POSSIBLE BENEFIT DURATION FROM DISABILITY INCURRED
      ***** DATE TO LOAN EXPIRATION DATE/ TERM OF POLICY
           MOVE EX-AA-INCURRED-DT            TO DC-BIN-DATE-1
           MOVE WS-POLICY-EXPIRE-DT          TO DC-BIN-DATE-2
           MOVE '1'                          TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION      THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               IF DC-ODD-DAYS-OVER > 0
                   ADD +1                    TO DC-ELAPSED-MONTHS
                   MOVE DC-ELAPSED-MONTHS    TO WS-D1-BENEFIT-DUR
               ELSE
                   MOVE DC-ELAPSED-MONTHS    TO WS-D1-BENEFIT-DUR
               END-IF
           ELSE
               DISPLAY 'ERROR IN BENEFIT DUR CALCULATION ' EX-SA-CERT-NO
080102         GO TO 2100-EXIT
           END-IF

           MOVE DC-ELAPSED-MONTHS            TO WS-BENEFIT-DUR

      ***** COMPUTE REMAINING BENEFIT DURATION
      ***** IF AGE OF CLAIM < MAXIMUM BENEFIT DURATION
           IF WS-TOTAL-MONTHS < WS-BENEFIT-DUR
               SUBTRACT WS-TOTAL-MONTHS      FROM WS-BENEFIT-DUR
                   GIVING WS-BEN-DUR-REMAIN-IN-MOS

               IF WS-BEN-DUR-REMAIN-IN-MOS >= +12
                   DIVIDE WS-BEN-DUR-REMAIN-IN-MOS BY 12
                       GIVING WS-BEN-DUR-REMAIN-IN-YRS ROUNDED
               ELSE
                   MOVE +0                   TO WS-BEN-DUR-REMAIN-IN-YRS
               END-IF

               COMPUTE WS-ODD-MOS-REMAINING =
                   WS-BEN-DUR-REMAIN-IN-MOS -
                                       (WS-BEN-DUR-REMAIN-IN-YRS * 12)

               IF WS-ODD-MOS-REMAINING = +0
                   CONTINUE
               ELSE
                   ADD +1                    TO WS-BEN-DUR-REMAIN-IN-YRS
               END-IF

      ************* IF REMAINING BENEFIT DURATION IS > ZERO MONTHS
      ************* ANY NUMBER OF MONTHS < 12 AND > ZERO BECOMES A YEAR
      ************* THIS PROGRAM TAKES A FUTURE RESERVE AMOUNT OFF OF
      ************* ELEXTR FILE THAT WAS PREVIOUSLY CALCULATED FROM CDT
               IF WS-BEN-DUR-REMAIN-IN-YRS >= +1
                   MOVE EX-AA-FUTURE-RESERVE TO WS-RESERVE-LIABILITY
               ELSE
                   MOVE +0                   TO WS-RESERVE-LIABILITY
               END-IF

           ELSE
      ******** AGE OF CLAIM IS > MAXIMUM BENEFIT DURATION
      ******** ESTIMATE REMAINING LIABILITY
               MOVE +0                       TO WS-BEN-DUR-REMAIN-IN-MOS
               MOVE +0                       TO WS-BEN-DUR-REMAIN-IN-YRS
               MOVE '*'                      TO WS-D1-EXCEPTION-FLAG
               MULTIPLY EX-AA-BENEFIT-AMT
                   BY WS-BENEFIT-DUR
                       GIVING WS-MAX-BENEFIT-AMT

               SUBTRACT EX-AA-TOTAL-PAID-AMT
                   FROM WS-MAX-BENEFIT-AMT
                       GIVING WS-RESERVE-LIABILITY

               IF WS-RESERVE-LIABILITY <= +0
                   MOVE +0                   TO WS-RESERVE-LIABILITY
               ELSE
                   DIVIDE WS-RESERVE-LIABILITY
                       BY +100
                           GIVING WS-RESERVE-LIABILITY ROUNDED
               END-IF
           END-IF

           MOVE WS-RESERVE-LIABILITY         TO WS-D1-RESERVE-LIABILITY
041105     MOVE EX-AA-PAY-CURRENT-RESERVE    TO WS-D1-PTC-RESERVE
           MOVE WS-BEN-DUR-REMAIN-IN-MOS     TO WS-D1-REMAIN-BEN-IN-MOS
           MOVE WS-BEN-DUR-REMAIN-IN-YRS     TO WS-D1-REMAIN-BEN-IN-YRS
122302****** Reserve category 40 is for disability and 
122302******  44 is for unemployment
122302     IF EX-AA-CLAIM-TYPE = 'A'
               MOVE '40'                     TO WS-D1-RESERVE-CAT
122302     ELSE
122302         MOVE '44'                     TO WS-D1-RESERVE-CAT
122302     END-IF

           MOVE WS-DETAIL1                   TO SORT-RECORD
           RELEASE SORT-RECORD

           IF SORT-RETURN > ZERO
               MOVE 'ERROR ON RELEASE SORT'  TO WS-ABEND-MESSAGE
               MOVE SORT-RETURN              TO WS-RETURN-CODE
               PERFORM ABEND-PGM             THRU APS-EXIT
           END-IF


           .
       2100-EXIT.
           EXIT.


       3000-OUTPUT-PROCEDURE.

           PERFORM 3010-RETURN-SORT          THRU 3010-EXIT
               UNTIL END-OF-SORTFILE

           .
       3000-EXIT.
           EXIT.

       3010-RETURN-SORT.

           RETURN SORT-FILE
               AT END
                   SET END-OF-SORTFILE       TO TRUE
                   GO TO 3010-EXIT
           END-RETURN

           IF SORT-RETURN > ZERO
               MOVE 'ERROR ON RETURN SORT'   TO WS-ABEND-MESSAGE
               MOVE SORT-RETURN              TO WS-RETURN-CODE
               PERFORM ABEND-PGM             THRU APS-EXIT
           END-IF

           WRITE CICLCR-RECORD               FROM SORT-RECORD

           .
       3010-EXIT.
           EXIT.


       8500-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8500-EXIT.
           EXIT.

       OPEN-FILES.

           OPEN  INPUT  REPORTS-EXTRACT-FILE
                 OUTPUT CICLCR-TXTFILE-OUT

           .
       OPEN-FILES-EXIT.
           EXIT.


       CLOSE-FILES.

           CLOSE REPORTS-EXTRACT-FILE
                 CICLCR-TXTFILE-OUT

            .
       CLOSE-FILES-EXIT.
           EXIT.

       ABEND-PGM SECTION. COPY ELCABEND .

