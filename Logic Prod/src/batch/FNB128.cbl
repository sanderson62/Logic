       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FNB128.
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      *                                                                        :
      *  DESCRIPTION:                                                          :
      *      THIS PROGRAM SUMMARIZES ANY FREEDOM GENERAL JOURNAL               :
      *      TRANSACTION FILE BY MAJOR ACCOUNT AND LINE OF BUSINESS.           :
      *                                                                        :
      *  INPUT:                                                                :
      *      1. DETAIL FREEDOM GENERAL JOURNAL TRANSACTIONS                    :
      *                                                                        :
      *  OUTPUT:                                                               :
      *      1. ACCOUNT SUMMARY REPORT BY MAJOR ACCOUNT / PRODUCT.             :
      *                                                                        :
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      *    DATE    BY  MODIFICATION                                            :
      * ========== === ========================================================:
      *            DAN               NEW - COPIED FROM FNB101                  :
      * 04/16/1999 DAN 1999040200001 DO NOT ABEND ON AN EMPTY FILE             :
      * 09/12/2003 DJN CR2003020300003 CONVERT FROM MAINFRAME.                 :
      * 07/28/2006 DJN DO NOT ADVANCE TO NEW PAGE ON FIRST PAGE OF REPORTS.    :
081806* 08/18/2006 AJR REMOVE FROM CLAIMS SYSTEM.                              :
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT SORT-FILE          ASSIGN TO EXTERNAL SORTWK.
           SELECT TRANSACTION-DETAIL ASSIGN TO EXTERNAL SYS010
                                     ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ACCOUNT-REPORT     ASSIGN TO EXTERNAL SYS007
                                     ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       SD  SORT-FILE.
       01  SORT-RECORD.
           COPY FNC019 REPLACING LEADING ==GL== BY ==SR==.

       FD  TRANSACTION-DETAIL
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 151 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  FILLER           PIC X(151).

       FD  ACCOUNT-REPORT
           LABEL RECORDS ARE OMITTED
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  PRINT-RECORD     PIC X(132).


       WORKING-STORAGE SECTION.

       01 RC                       PIC S9(9)   COMP-5 VALUE 0.
       01 LIST-REC                 PIC X(132).

       01  FILLER.
           05  NEW-ACCT-SW          PIC X     VALUE 'Y'.
               88  NEW-ACCOUNT                VALUE 'Y'.

       01  FILLER                 COMP-3.
           05  SUB                PIC S9(3)      VALUE ZERO.
           05  LINE-COUNT         PIC S9(3)      VALUE ZERO.
           05  PAGE-COUNT         PIC S9(5)      VALUE ZERO.
           05  INPUT-COUNT        PIC S9(5)      VALUE ZERO.
               88  FIRST-RECORD                  VALUE +1.
           05  TOTALS             OCCURS 3 TIMES.
               10  WS-COUNT       PIC S9(7)      COMP-3.
               10  WS-DR-AMOUNT   PIC S9(11)V99  COMP-3.
               10  WS-CR-AMOUNT   PIC S9(11)V99  COMP-3.

      *****************************************************************
      *                                                               *
      *      FREEDOM GENERAL LEDGER V5.10 - JOURNAL TRANSACTION       *
      *****************************************************************

       01  WS-SUMMARY-RECORD.
           COPY FNC019 REPLACING LEADING ==GL== BY ==WS==.

       01  HEADING-LINE-1.
           05  FILLER      PIC X(09)   VALUE 'FNB128 - '.
           05  HDG-NOTE    PIC X(35)   VALUE SPACE.
           05  FILLER      PIC X(47)   VALUE
               'A C C O U N T   S U M M A R Y   L I S T'.
           05  FILLER      PIC X(17)   VALUE SPACE.
           05  HDG-DATE    PIC XX/XX/XXXX   VALUE SPACE.
           05  FILLER      PIC X(8)    VALUE '   PAGE '.
           05  HDG-PAGE    PIC ZZ,ZZ9  VALUE ZERO.

       01  HEADING-LINE-2.
           05  FILLER      PIC X(13)   VALUE 'ACCOUNT NO   '.
           05  FILLER      PIC X(09)   VALUE 'PRODUCT  '.
           05  FILLER      PIC X(18)   VALUE '       DR AMOUNT  '.
           05  FILLER      PIC X(18)   VALUE '       CR AMOUNT  '.
           05  FILLER      PIC X(18)   VALUE '             NET  '.
           05  FILLER      PIC X(09)   VALUE '    COUNT'.

       01  HEADING-LINE-3.
           05  FILLER      PIC X(13)   VALUE '----------   '.
           05  FILLER      PIC X(09)   VALUE '------   '.
           05  FILLER      PIC X(18)   VALUE '----------------  '.
           05  FILLER      PIC X(18)   VALUE '----------------  '.
           05  FILLER      PIC X(18)   VALUE '----------------  '.
           05  FILLER      PIC X(18)   VALUE '---------'.

       01  ACCOUNT-SUMMARY-LINE   VALUE SPACE.
           05  AS-MAJ-ACCT        PIC X(10).
           05  FILLER             PIC XXX.
           05  AS-PRODUCT         PIC X(6).
           05  FILLER             PIC X(3).
           05  AS-DR-AMT          PIC Z,ZZZ,ZZZ,ZZZ.99.
           05  FILLER             PIC XX.
           05  AS-CR-AMT          PIC Z,ZZZ,ZZZ,ZZZ.99.
           05  FILLER             PIC XX.
           05  AS-NET-AMT         PIC Z,ZZZ,ZZZ,ZZZ.99-.
           05  FILLER             PIC XX.
           05  AS-COUNT           PIC Z,ZZZ,ZZ9.

081806*    EXEC SQL INCLUDE ISTDWORK.INC END-EXEC.
081806 01  PARM.
           05  PARM-NOTE          PIC X(15) VALUE 'CSO PAID DRAFTS'.

       PROCEDURE DIVISION.

081806*    CALL IGETPARM USING IG-P1
081806*    MOVE IG-P1 TO HDG-NOTE
081806     MOVE PARM-NOTE TO HDG-NOTE

           SORT SORT-FILE
             ON ASCENDING KEY SR-MAJ-ACCT SR-PRODUCT
               INPUT PROCEDURE  IS 1000-INPUT
               OUTPUT PROCEDURE IS 2000-PRINT-REPORT.

           PERFORM 9000-END

081806*    GOBACK GIVING RC.
081806     STOP RUN GIVING RC.

       1000-INPUT.

           OPEN INPUT TRANSACTION-DETAIL
081706     CALL 'FNBLIST' USING 'O' ' '
           PERFORM UNTIL EXIT
             READ TRANSACTION-DETAIL INTO SORT-RECORD
               AT END EXIT PERFORM
               NOT AT END RELEASE SORT-RECORD
             END-READ
           END-PERFORM
           CLOSE TRANSACTION-DETAIL.
081706     CALL 'FNBLIST' USING 'C' ' '.

       2000-PRINT-REPORT.

           OPEN OUTPUT ACCOUNT-REPORT
           INITIALIZE TOTALS (1) TOTALS (2) TOTALS (3).

           PERFORM UNTIL EXIT
             RETURN SORT-FILE
               AT END EXIT PERFORM
             END-RETURN
             ADD +1 TO INPUT-COUNT

             IF FIRST-RECORD
               MOVE SR-JOURNAL-DATE TO HDG-DATE
               PERFORM 8000-HEADING
               MOVE SORT-RECORD TO WS-SUMMARY-RECORD
             END-IF

             IF SR-MAJ-ACCT = WS-MAJ-ACCT
               IF SR-PRODUCT NOT = WS-PRODUCT
                 PERFORM 3000-SUBTOT-PRODUCT
               END-IF
             ELSE
               PERFORM 4000-SUBTOT-ACCT
             END-IF

             ADD +1 TO WS-COUNT (1)
             IF SR-AMOUNT IS NEGATIVE
               ADD SR-AMOUNT TO WS-CR-AMOUNT (1)
             ELSE
               ADD SR-AMOUNT TO WS-DR-AMOUNT (1)
             END-IF
           END-PERFORM.

       3000-SUBTOT-PRODUCT.

           IF NEW-ACCOUNT
             MOVE WS-MAJ-ACCT TO AS-MAJ-ACCT
             MOVE 'N' TO NEW-ACCT-SW
           ELSE
             MOVE SPACES TO AS-MAJ-ACCT
           END-IF
           MOVE WS-PRODUCT       TO AS-PRODUCT
           MOVE WS-COUNT     (1) TO AS-COUNT
           MOVE WS-DR-AMOUNT (1) TO AS-DR-AMT
           MOVE WS-CR-AMOUNT (1) TO AS-CR-AMT
           ADD WS-CR-AMOUNT (1), WS-DR-AMOUNT (1) GIVING AS-NET-AMT
           WRITE PRINT-RECORD FROM ACCOUNT-SUMMARY-LINE
                   AFTER ADVANCING 1 LINE
           ADD +1 TO LINE-COUNT
           IF LINE-COUNT > +55
             PERFORM 8000-HEADING
           END-IF

           ADD WS-COUNT     (1) TO  WS-COUNT     (2)
           ADD WS-DR-AMOUNT (1) TO  WS-DR-AMOUNT (2)
           ADD WS-CR-AMOUNT (1) TO  WS-CR-AMOUNT (2)
           MOVE ZERO   TO  WS-COUNT     (1)
           MOVE ZERO   TO  WS-DR-AMOUNT (1)
           MOVE ZERO   TO  WS-CR-AMOUNT (1)
           MOVE SR-PRODUCT TO  WS-PRODUCT.

       4000-SUBTOT-ACCT.

           PERFORM 3000-SUBTOT-PRODUCT
           MOVE SPACES           TO AS-MAJ-ACCT
           MOVE 'TOTAL'          TO AS-PRODUCT
           MOVE WS-COUNT     (2) TO AS-COUNT
           MOVE WS-DR-AMOUNT (2) TO AS-DR-AMT
           MOVE WS-CR-AMOUNT (2) TO AS-CR-AMT
           ADD WS-CR-AMOUNT (2), WS-DR-AMOUNT (2) GIVING AS-NET-AMT
           WRITE PRINT-RECORD FROM ACCOUNT-SUMMARY-LINE
                   AFTER ADVANCING 1 LINE
           MOVE SPACES TO PRINT-RECORD
           WRITE PRINT-RECORD AFTER ADVANCING 1 LINE
           ADD +2 TO LINE-COUNT
           IF LINE-COUNT > +55
             PERFORM 8000-HEADING
           END-IF

           ADD WS-COUNT     (2) TO WS-COUNT     (3)
           ADD WS-DR-AMOUNT (2) TO WS-DR-AMOUNT (3)
           ADD WS-CR-AMOUNT (2) TO WS-CR-AMOUNT (3)
           MOVE ZERO        TO WS-COUNT     (2)
           MOVE ZERO        TO WS-DR-AMOUNT (2)
           MOVE ZERO        TO WS-CR-AMOUNT (2)
           MOVE SR-MAJ-ACCT TO WS-MAJ-ACCT
           SET NEW-ACCOUNT TO TRUE.

       8000-HEADING.

           ADD +1 TO PAGE-COUNT
           MOVE PAGE-COUNT TO HDG-PAGE
           IF PAGE-COUNT = 1
             WRITE PRINT-RECORD FROM HEADING-LINE-1
                                                 AFTER ADVANCING 0 LINES
           ELSE
             WRITE PRINT-RECORD FROM HEADING-LINE-1 AFTER ADVANCING PAGE
           END-IF
           WRITE PRINT-RECORD FROM HEADING-LINE-2
                                                 AFTER ADVANCING 2 LINES
           WRITE PRINT-RECORD FROM HEADING-LINE-3 AFTER ADVANCING 1 LINE
           MOVE SPACES TO PRINT-RECORD
           WRITE PRINT-RECORD AFTER ADVANCING 1 LINE
           MOVE +5 TO LINE-COUNT.

       9000-END.

           PERFORM 9000-GRAND-TOTAL
           CLOSE ACCOUNT-REPORT
           IF INPUT-COUNT = ZERO
             DISPLAY '*** INPUT FILE IS EMPTY ***' UPON SYSERR
             MOVE    '*** INPUT FILE IS EMPTY ***' TO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
           END-IF.

       9000-GRAND-TOTAL.

           IF LINE-COUNT > +50
             PERFORM 8000-HEADING
           END-IF
           PERFORM 4000-SUBTOT-ACCT
           MOVE '*** BATCH TOTAL ***' TO ACCOUNT-SUMMARY-LINE
           MOVE WS-DR-AMOUNT (3) TO AS-DR-AMT
           MOVE WS-CR-AMOUNT (3) TO AS-CR-AMT
           MOVE WS-COUNT     (3) TO AS-COUNT
           ADD WS-CR-AMOUNT (3), WS-DR-AMOUNT (3) GIVING AS-NET-AMT
           WRITE PRINT-RECORD FROM ACCOUNT-SUMMARY-LINE
                   AFTER ADVANCING 3 LINES.
