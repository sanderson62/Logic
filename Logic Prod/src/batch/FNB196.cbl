       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FNB196.

      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      *                                                                        :
      *  DESCRIPTION:                                                          :
      *    CONVERT PAYROLL TAPE RECEIVED FROM ADP INTO GENERAL JOURNAL         :
      *    TRANSACTIONS TO BE DOWNLOADED TO THE FREEDOM GENERAL LEDGER         :
      *    SYSTEM.                                                             :
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      *    DATE    BY  MODIFICATION                                            :
      * ========== === ========================================================:
      * 02/03/2003 DJN CR2003020300003 CONVERT FROM MAINFRAME.                 :
      * 08/16/2006 AJR CR2006081600000 REMOVE FROM CLAIMS SYSTEM               :
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ADP-PAYROLL  ASSIGN TO EXTERNAL SYS010
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS SYS010-STATUS.

           SELECT LEDGER-TRANS ASSIGN TO EXTERNAL SYS011
                               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  ADP-PAYROLL
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 100 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  ADP-RECORD             PIC X(200).

       FD  LEDGER-TRANS
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 151 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  LEDGER-RECORD.
           COPY FNC019.

       WORKING-STORAGE SECTION.

       01 RC                       PIC S9(9)   COMP-5 VALUE 0.
       01 LIST-REC                 PIC X(132).

       01  FILLER                  COMP-3.
           05  WS-DR-AMT           PIC S9(9)V99   VALUE +0.
           05  WS-CR-AMT           PIC S9(9)V99   VALUE +0.
           05  WS-IN-COUNT         PIC S9(7)      VALUE +0.
           05  WS-COUNT            PIC S9(7)      VALUE +0.

       01  FILLER.
           05  SYS010-STATUS       PIC XX     VALUE '00'.
               88  EOF                        VALUE '10'.
           05  ED-AMT              PIC ZZZ,ZZZ,ZZZ.99-.
           05  ED-CNT              PIC ZZ,ZZ9-.
           05  WS-POSTING-DATE.
               10  WS-POST-MO      PIC XX.
               10  WS-POST-DAY     PIC XX.
               10  WS-POST-YYY     PIC XXX.
               10  WS-POST-YR      PIC X.
           05  SYSTEM-DATE         PIC 9(8).
           05  WS-CENTER.
               10  FILLER          PIC X(5).
               10  WS-CENTER-6     PIC X.

       01  WORK-ADP-RECORD.
           05  ADP-FIRST-FIELD.
               10  FILLER         PIC X.
               10  ADP-CO-CODE    PIC X(5).
           05  ADP-ACCOUNT-NO.
               10  ADP-ACCT-TYPE  PIC X.
               10  FILLER         PIC X(9).
               10  ADP-CENTER     PIC X(6).
           05  ADP-PR-CODE        PIC X(2).
           05  ADP-PAY-DATE.
               10  ADP-PAY-MO     PIC XX.
               10  ADP-PAY-DAY    PIC XX.
               10  ADP-PAY-YR     PIC X.
           05  ADP-AMOUNTX        PIC X(15).
           05  ADP-AMOUNT         PIC S9(9)V99.
           05  ADP-REFERENCE-1    PIC X(6).
           05  ADP-DESCR          PIC X(45).
           05  ADP-REFERENCE-2    PIC X(6).
           05  ADP-REFERENCE-3    PIC X(6).

081606 01  WORK-DATE.
081606     05  WRK-YR    PIC 9999.
081606     05  WRK-MO    PIC 99.
081606     05  WRK-DAY   PIC 99.
081606
081606 01  SYS-DATE.
081606     05  SYS-MO    PIC 99.
081606     05  SYS-DAY   PIC 99.
081606     05  SYS-YR    PIC 9999.

       PROCEDURE DIVISION.

081606     CALL 'FNBLIST' USING 'O' LIST-REC
           OPEN INPUT ADP-PAYROLL
           IF SYS010-STATUS NOT = '00'
             DISPLAY '*** OPEN ERROR ' SYS010-STATUS ' ON SYS010'
                     UPON SYSERR
             MOVE SPACES TO LIST-REC
             STRING  '*** OPEN ERROR ' SYS010-STATUS ' ON SYS010'
               DELIMITED BY SIZE INTO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
081606       CALL 'FNBLIST' USING 'C' LIST-REC
             MOVE 16 TO RC
081606*      GOBACK GIVING RC
081606       STOP RUN GIVING RC
           END-IF

           OPEN OUTPUT LEDGER-TRANS

081606     ACCEPT WORK-DATE FROM DATE YYYYMMDD
081606     MOVE WRK-YR  TO SYS-YR
081606     MOVE WRK-MO  TO SYS-MO
081606     MOVE WRK-DAY TO SYS-DAY
081606
081606     MOVE SYS-DATE TO SYSTEM-DATE
081606*    CALL 'SYSDATE' USING SYSTEM-DATE

           PERFORM UNTIL EXIT
             READ ADP-PAYROLL
               AT END
                 EXIT PERFORM
             END-READ
             PERFORM 1000-PROCESS-PAYROLL
           END-PERFORM

           CLOSE ADP-PAYROLL
                 LEDGER-TRANS

           MOVE WS-IN-COUNT TO ED-CNT
           DISPLAY 'RECORDS IN:  ' ED-CNT UPON SYSERR
           MOVE SPACES TO LIST-REC
           STRING  'RECORDS IN:  ' ED-CNT
             DELIMITED BY SIZE INTO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC
           MOVE WS-COUNT TO ED-CNT
           DISPLAY 'RECORDS OUT: ' ED-CNT UPON SYSERR
           MOVE SPACES TO LIST-REC
           STRING  'RECORDS OUT: ' ED-CNT
             DELIMITED BY SIZE INTO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC
           MOVE WS-DR-AMT TO ED-AMT
           DISPLAY '  DR AMOUNT: ' ED-AMT UPON SYSERR
           MOVE SPACES TO LIST-REC
           STRING  '  DR AMOUNT: ' ED-AMT
             DELIMITED BY SIZE INTO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC
           MOVE WS-CR-AMT TO ED-AMT
           DISPLAY '  CR AMOUNT: ' ED-AMT UPON SYSERR
           MOVE SPACES TO LIST-REC
           STRING  '  CR AMOUNT: ' ED-AMT
             DELIMITED BY SIZE INTO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC
081606     CALL 'FNBLIST' USING 'C' LIST-REC

081606*    GOBACK GIVING RC.
081606     STOP RUN GIVING RC.

       1000-PROCESS-PAYROLL.

           ADD +1 TO WS-IN-COUNT

           MOVE SPACES TO WORK-ADP-RECORD

           UNSTRING ADP-RECORD DELIMITED BY '","'
               INTO ADP-FIRST-FIELD, ADP-ACCOUNT-NO,  ADP-PR-CODE,
                    ADP-PAY-DATE,    ADP-AMOUNTX,     ADP-REFERENCE-1,
                    ADP-DESCR,       ADP-REFERENCE-2, ADP-REFERENCE-3
           END-UNSTRING

           IF ADP-ACCOUNT-NO = '9999999999999991'
             MOVE '2717000112000000' TO ADP-ACCOUNT-NO
           END-IF

           MOVE FUNCTION NUMVAL (ADP-AMOUNTX) TO ADP-AMOUNT

           IF ADP-CO-CODE NOT = 'MFN01'
      *      FOR SOME REASON THE FIRST RECORD IS EMPTY
             IF WS-IN-COUNT = 1
               EXIT PARAGRAPH
             END-IF
             DISPLAY '*** WRONG INPUT TAPE - JOB CANCELLED ***'
                     UPON SYSERR
             MOVE    '*** WRONG INPUT TAPE - JOB CANCELLED ***'
                     TO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
081606       CALL 'FNBLIST' USING 'C' LIST-REC
             MOVE 16 TO RC
081606*      GOBACK GIVING RC
081606       STOP RUN GIVING RC
           END-IF

           MOVE ADP-PAY-MO  TO WS-POST-MO
           MOVE ADP-PAY-DAY TO WS-POST-DAY
           MOVE ADP-PAY-YR  TO WS-POST-YR
      *    MOVE ADP-HDR-YEAR(1:3) TO WS-POST-YYY
           MOVE SYSTEM-DATE(5:3)  TO WS-POST-YYY
           IF WS-POSTING-DATE IS NOT NUMERIC
             DISPLAY '*** INVALID DATE IN RECORD: ' WS-POSTING-DATE
                     UPON SYSERR
             MOVE SPACES TO LIST-REC
             STRING  '*** INVALID DATE IN RECORD: ' WS-POSTING-DATE
               DELIMITED BY SIZE INTO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
081606       CALL 'FNBLIST' USING 'C' LIST-REC
             MOVE 16 TO RC
081606*      GOBACK GIVING RC
081606       STOP RUN GIVING RC
           END-IF

           MOVE ADP-ACCOUNT-NO  TO GL-MAJ-ACCT
TEST*****  MOVE ZEROS           TO GL-DIV
TEST*****  MOVE ADP-CENTER      TO GL-CENTER
           MOVE ZEROS           TO GL-PRODUCT
           MOVE ZEROS           TO GL-STATE

012799     MOVE '0'             TO GL-DIV(1:1)
012799     MOVE ADP-CENTER(2:1) TO GL-DIV(2:1)
012799     MOVE ADP-CENTER(3:4) TO GL-CENTER

           MOVE SYSTEM-DATE     TO GL-JOURNAL-DATE
           MOVE WS-POSTING-DATE TO GL-POSTING-DATE
           MOVE ADP-AMOUNT      TO GL-AMOUNT
           MOVE SPACES          TO GL-REFERENCE
           MOVE ADP-DESCR       TO GL-DESCRIPTION
           MOVE 'PAYROL'        TO GL-SOURCE
           MOVE SPACES          TO GL-SUNDRY
           MOVE SPACES          TO GL-REVERSE-FLAG
           MOVE SPACES          TO GL-SUSPENSE
           MOVE SPACES          TO GL-ALLOC-CODE
           WRITE LEDGER-RECORD
           ADD +1 TO WS-COUNT
           IF ADP-AMOUNT IS NEGATIVE
             ADD ADP-AMOUNT TO WS-CR-AMT
           ELSE
             ADD ADP-AMOUNT TO WS-DR-AMT
           END-IF.
