       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FNB180.

      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      *                                                                        :
      *  THIS PROGRAM PRINTS A REPORT OF GENERAL LEDGER TRANSACTIONS           :
      *  BY ACCOUNT NUMBER                                                     :
      *                                                                        :
      *  INPUT:   DETAIL FREEDOM GENERAL JOURNAL TRANSACTIONS                  :
      *                                                                        :
      *  OUTPUT:  SUMMARY REPORT BY ACCOUNT NUMBER                             :
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      *    DATE    BY  MODIFICATION                                            :
      * ========== === ========================================================:
      * 02/03/2003 DJN CR2003020300003 CONVERT FROM MAINFRAME.                 :
      * 07/28/2006 DJN DO NOT ADVANCE TO NEW PAGE ON FIRST PAGE OF REPORTS.    :
081606* 08/16/2006 AJR REMOVE FROM CLAIMS SYSTEM                               :
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT SORT-FILE          ASSIGN TO EXTERNAL SORTWK.

           SELECT TRANSACTION-DETAIL ASSIGN TO EXTERNAL SYS010
                                     ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SUMMARY-REPORT     ASSIGN TO EXTERNAL SYS007
                                     ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       SD  SORT-FILE.
       01  SORT-RECORD.
           COPY FNC019.

       FD  TRANSACTION-DETAIL
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 151 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  FILLER           PIC X(151).

       FD  SUMMARY-REPORT
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  PRINT-RECORD     PIC X(132).

       WORKING-STORAGE SECTION.

       01 RC                       PIC S9(9)   COMP-5 VALUE 0.
       01 LIST-REC                 PIC X(132).

       01  FILLER.
           05  WS-WORK-ACCT.
               10  WS-MAJ-ACCT    PIC X(10)      VALUE SPACE.
               10  FILLER         PIC X          VALUE '-'.
               10  WS-DIV         PIC X(2)       VALUE SPACE.
               10  FILLER         PIC X          VALUE '-'.
               10  WS-CENTER      PIC X(4)       VALUE SPACE.
               10  FILLER         PIC X          VALUE '-'.
               10  WS-PRODUCT     PIC X(6)       VALUE SPACE.
               10  FILLER         PIC X          VALUE '-'.
               10  WS-STATE       PIC X(2)       VALUE SPACE.

       01  FILLER                 COMP-3.
           05  SUB                PIC S9(3)      VALUE ZERO.
           05  LINE-COUNT         PIC S9(3)      VALUE ZERO.
           05  PAGE-COUNT         PIC S9(5)      VALUE ZERO.
           05  INPUT-COUNT        PIC S9(5)      VALUE ZERO.
               88  FIRST-RECORD                  VALUE +1.
           05  TOTALS             OCCURS 2 TIMES.
               10  WS-COUNT       PIC S9(7)      COMP-3.
               10  WS-DR-AMOUNT   PIC S9(11)V99  COMP-3.
               10  WS-CR-AMOUNT   PIC S9(11)V99  COMP-3.

       01  SUMMARY-RECORD.
           COPY FNC019 REPLACING LEADING ==GL== BY ==SUM==.

       01  HEADING-LINE-1.
           05  FILLER      PIC X(09)   VALUE 'FNB180 - '.
           05  HDG-NOTE    PIC X(35)   VALUE SPACE.
           05  FILLER      PIC X(47)   VALUE
               'A C C O U N T   S U M M A R Y   L I S T'.
           05  FILLER      PIC X(17)   VALUE SPACE.
           05  HDG-DATE    PIC XX/XX/XXXX.
           05  FILLER      PIC X(8)    VALUE '   PAGE '.
           05  HDG-PAGE    PIC ZZ,ZZ9  VALUE ZERO.

       01  HEADING-LINE-2.
           05  FILLER      PIC X(28)   VALUE 'ACCOUNT NO'.
           05  FILLER      PIC XXX     VALUE SPACE.
           05  FILLER      PIC X(16)   VALUE '       DR AMOUNT'.
           05  FILLER      PIC XX      VALUE SPACE.
           05  FILLER      PIC X(16)   VALUE '       CR AMOUNT'.
           05  FILLER      PIC XX      VALUE SPACE.
           05  FILLER      PIC X(16)   VALUE '             NET'.
           05  FILLER      PIC XXX     VALUE SPACE.
           05  FILLER      PIC X(09)   VALUE '    COUNT'.

       01  HEADING-LINE-3.
           05  FILLER      PIC X(28)   VALUE ALL '-'.
           05  FILLER      PIC XXX     VALUE SPACE.
           05  FILLER      PIC X(16)   VALUE ALL '-'.
           05  FILLER      PIC XX      VALUE SPACE.
           05  FILLER      PIC X(16)   VALUE ALL '-'.
           05  FILLER      PIC XX      VALUE SPACE.
           05  FILLER      PIC X(16)   VALUE ALL '-'.
           05  FILLER      PIC XXX     VALUE SPACE.
           05  FILLER      PIC X(9)    VALUE ALL '-'.

       01  PRINT-LINE             VALUE SPACE.
           05  PRT-ACCOUNT        PIC X(28).
           05  FILLER             PIC XXX.
           05  PRT-DR-AMT         PIC Z,ZZZ,ZZZ,ZZZ.99.
           05  FILLER             PIC XX.
           05  PRT-CR-AMT         PIC Z,ZZZ,ZZZ,ZZZ.99.
           05  FILLER             PIC XX.
           05  PRT-NET-AMT        PIC Z,ZZZ,ZZZ,ZZZ.99-.
           05  FILLER             PIC XX.
           05  PRT-COUNT          PIC Z,ZZZ,ZZ9.

081606*    EXEC SQL INCLUDE ISTDWORK.INC END-EXEC.

       01  PARM.
081606*    05  PARM-NOTE      PIC X(50).
081606     05  PARM-NOTE      PIC X(50)  VALUE 'CSO PAYROLL'.

       PROCEDURE DIVISION.

081606*    CALL IGETPARM USING IG-P1
081606*    MOVE IG-P1 TO PARM-NOTE

081606     CALL 'FNBLIST' USING 'O' LIST-REC
           MOVE PARM-NOTE TO HDG-NOTE
           OPEN OUTPUT SUMMARY-REPORT
           INITIALIZE TOTALS (1) TOTALS (2)

           SORT SORT-FILE ON ASCENDING KEY GL-ACCOUNT-NO
                   USING TRANSACTION-DETAIL
                   OUTPUT PROCEDURE IS 1000-PRINT-REPORT

           IF SORT-RETURN NOT = +0
             DISPLAY '*** FNB180 INTERNAL SORT FAILED ***' UPON SYSERR
             MOVE    '*** FNB180 INTERNAL SORT FAILED ***' TO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
081606       CALL 'FNBLIST' USING 'C' LIST-REC
             MOVE 16 TO RC
081606*      GOBACK GIVING RC
081606       STOP RUN GIVING RC
           END-IF

           IF INPUT-COUNT = ZERO
             DISPLAY '*** INPUT FILE IS EMPTY ***' UPON SYSERR
             MOVE    '*** INPUT FILE IS EMPTY ***' TO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
081606       CALL 'FNBLIST' USING 'C' LIST-REC
             MOVE 4 TO RC
081606*      GOBACK GIVING RC
081606       STOP RUN GIVING RC
           ELSE
             PERFORM 2000-ACCOUNT-TOTAL
             IF LINE-COUNT > +50
               PERFORM 8000-HEADING
             END-IF
             MOVE '*** BATCH TOTAL ***' TO PRINT-LINE
             MOVE WS-DR-AMOUNT (2) TO PRT-DR-AMT
             MOVE WS-CR-AMOUNT (2) TO PRT-CR-AMT
             MOVE WS-COUNT     (2) TO PRT-COUNT
             ADD WS-CR-AMOUNT (2), WS-DR-AMOUNT (2) GIVING PRT-NET-AMT
             WRITE PRINT-RECORD FROM PRINT-LINE AFTER ADVANCING 3 LINES
           END-IF

           CLOSE SUMMARY-REPORT
081606     CALL 'FNBLIST' USING 'C' LIST-REC

081606*    GOBACK GIVING RC.
081606     STOP RUN GIVING RC.

       1000-PRINT-REPORT.

           PERFORM UNTIL EXIT
             RETURN SORT-FILE
               AT END EXIT PERFORM
             END-RETURN
             ADD +1 TO INPUT-COUNT

             IF FIRST-RECORD
               MOVE SORT-RECORD TO SUMMARY-RECORD
               MOVE GL-JOURNAL-DATE TO HDG-DATE
               PERFORM 8000-HEADING
             END-IF

             IF GL-ACCOUNT-NO NOT = SUM-ACCOUNT-NO
               PERFORM 2000-ACCOUNT-TOTAL
             END-IF

             ADD +1 TO WS-COUNT (1)
             IF GL-AMOUNT IS NEGATIVE
               ADD GL-AMOUNT TO WS-CR-AMOUNT (1)
             ELSE
               ADD GL-AMOUNT TO WS-DR-AMOUNT (1)
             END-IF
           END-PERFORM.

       2000-ACCOUNT-TOTAL.

           MOVE SUM-MAJ-ACCT     TO WS-MAJ-ACCT
           MOVE SUM-DIV          TO WS-DIV
           MOVE SUM-CENTER       TO WS-CENTER
           MOVE SUM-PRODUCT      TO WS-PRODUCT
           MOVE SUM-STATE        TO WS-STATE
           MOVE WS-WORK-ACCT     TO PRT-ACCOUNT
           MOVE WS-COUNT     (1) TO PRT-COUNT
           MOVE WS-DR-AMOUNT (1) TO PRT-DR-AMT
           MOVE WS-CR-AMOUNT (1) TO PRT-CR-AMT
           ADD WS-CR-AMOUNT (1), WS-DR-AMOUNT (1) GIVING PRT-NET-AMT
           WRITE PRINT-RECORD FROM PRINT-LINE AFTER ADVANCING 1 LINE
           ADD +1 TO LINE-COUNT
           IF LINE-COUNT > +55
             PERFORM 8000-HEADING
           END-IF

           ADD WS-COUNT     (1) TO  WS-COUNT     (2)
           ADD WS-DR-AMOUNT (1) TO  WS-DR-AMOUNT (2)
           ADD WS-CR-AMOUNT (1) TO  WS-CR-AMOUNT (2)
           MOVE +0  TO  WS-COUNT     (1)
           MOVE +0  TO  WS-DR-AMOUNT (1)
           MOVE +0  TO  WS-CR-AMOUNT (1)
           MOVE SORT-RECORD TO SUMMARY-RECORD.

       8000-HEADING.

           ADD +1 TO PAGE-COUNT
           MOVE PAGE-COUNT TO HDG-PAGE
           IF PAGE-COUNT = 1
             WRITE PRINT-RECORD FROM HEADING-LINE-1
                                                 AFTER ADVANCING 0 LINES
           ELSE
             WRITE PRINT-RECORD FROM HEADING-LINE-1 AFTER ADVANCING PAGE
           END-IF
           WRITE PRINT-RECORD FROM HEADING-LINE-2 AFTER ADVANCING 2 LINE
           WRITE PRINT-RECORD FROM HEADING-LINE-3 AFTER ADVANCING 1 LINE
           MOVE SPACES TO PRINT-RECORD
           WRITE PRINT-RECORD AFTER ADVANCING 1 LINE
           MOVE +5 TO LINE-COUNT.
