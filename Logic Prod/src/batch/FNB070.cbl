       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FNB070.
       DATE-WRITTEN.  MAY, 2000.

      ******************************************************************
      *
      *       CREATE FIRST NATIONAL BANK FILE OF ISSUED CHECKS
070902*        THIS FILE IS TRANSFERRED TO FNB BY OPERATIONS
070902******************************************************************
070902*                   C H A N G E   L O G
070902*
070902* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
070902*-----------------------------------------------------------------
070902*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
070902* EFFECTIVE    NUMBER
070902*-----------------------------------------------------------------
070902* 070902    2002031800003  SMVA  MOVE FROM MAINFRAME TO LAN
092302* 092302                   SMVA  CHANGE COMPARISON VALUE FOR CHECK
092302*                        AMOUNT FROM ZERO TO 0.00 W/LEADING SPACES
100702* 100702    2002100700003  SMVA  ADD VOIDS TO PROCESS FOR BANK    
070902******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

100702*    SELECT MICR-DRAFT-FILE
100702     SELECT MICR-TRNSFR-FILE
               ASSIGN TO TRNSFR
070902         ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS MICR-STATUS.

           SELECT FNB-ISSUE-FILE
               ASSIGN TO SYS011
070902         ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS SYS011-STATUS.


       DATA DIVISION.
       FILE SECTION.

       FD  FNB-ISSUE-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 54 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  FNB-ISSUE-RECORD   PIC X(54).

100702*FD  MICR-DRAFT-FILE
100702 FD  MICR-TRNSFR-FILE
100702     RECORD IS VARYING FROM 430 TO 1858 CHARACTERS.
100702*    RECORD IS VARYING FROM 650 TO 3000 CHARACTERS.
100702*01  MICR-DRAFT-RECORD.
100702 01  MICR-TRNSFR-RECORD.
070902     COPY FNMICR.  
100702*01  FILLER.
100702*    05  MICR-KEY         PIC X(0019).
100702*    05  FILLER           PIC X(2981).


      /
       WORKING-STORAGE SECTION.

       01  FILLER.
           05  MICR-STATUS         PIC XX     VALUE '00'.
               88  EOF                        VALUE '10'.
           05  SYS011-STATUS       PIC XX     VALUE '00'.
           05  TOT-COUNT           PIC S9(7)  COMP-3 VALUE +0.
070902*    05  ED-CNT              PIC ZZ,ZZZ,ZZZ,ZZ9.
070902*    05  ED-AMT              PIC ZZZ,ZZZ,ZZZ.99.
           05  WS-DATE.
               10  WS-MO           PIC XX.
               10  FILLER          PIC X.
               10  WS-DAY          PIC XX.
               10  FILLER          PIC X.
               10  WS-YEAR         PIC XXXX.
           05  WS-CHECK-DATE.
               10  WS-CHECK-YEAR   PIC XXXX.
               10  WS-CHECK-MO     PIC XX.
               10  WS-CHECK-DAY    PIC XX.

       01  FNB-WORK-RECORD.
           05  FNB-VOID-IND    PIC X         VALUE '0'.
           05  FNB-ACCOUNT     PIC X(10)     VALUE '0013238089'.
           05  FNB-CHECK-NO    PIC 9(10)     VALUE ZERO.
           05  FNB-ISSUE-DATE  PIC 9(8)      VALUE ZERO.
           05  FNB-CHECK-AMT   PIC 9(8)V99   VALUE ZERO.
           05  FILLER          PIC X(15)     VALUE ZERO.

       01  FNB-TOTAL-RECORD.
           05  FNB-TOTAL-IND   PIC X         VALUE 'T'.
           05  FNB-TOTAL-AMT   PIC 9(10)V99  VALUE ZERO.
           05  FILLER          PIC X(41)     VALUE ZERO.



      *
       PROCEDURE DIVISION.
      *
           PERFORM 0000-INIT THRU 0000-EXIT

           PERFORM 1000-PROCESS-FILE THRU 1000-EXIT
             UNTIL EOF

           PERFORM 9000-END THRU 9000-EXIT

070902     GOBACK.

       0000-INIT.
      *
100702*    OPEN INPUT MICR-DRAFT-FILE
100702     OPEN INPUT MICR-TRNSFR-FILE
           IF MICR-STATUS = '00' OR '97'
              CONTINUE
           ELSE
100702*       DISPLAY 'OPEN ERROR ' MICR-STATUS ' ON MICRDRFT'
100702        DISPLAY 'OPEN ERROR ' MICR-STATUS ' ON TRNSFR'
070902     END-IF

           OPEN OUTPUT FNB-ISSUE-FILE
           IF SYS011-STATUS NOT = '00'
              DISPLAY 'OPEN ERROR ' SYS011-STATUS ' ON SYS011'
070902     END-IF

           .
       0000-EXIT.
           EXIT.

       1000-PROCESS-FILE.
      *
100702*    READ MICR-DRAFT-FILE
100702     READ MICR-TRNSFR-FILE
             AT END GO TO 1000-EXIT
070902     END-READ

           IF FNMICR-FORM NOT = '0031'
              GO TO 1000-EXIT
070902     END-IF

092302     IF FNMICR-AMOUNT-PAID = '          0.00'
              GO TO 1000-EXIT
070902     END-IF

100702     MOVE '0'                TO FNB-VOID-IND
           MOVE FNMICR-DRAFT-NO    TO FNB-CHECK-NO
           MOVE FNMICR-CHECK-DATE  TO WS-DATE
           MOVE WS-YEAR            TO WS-CHECK-YEAR
           MOVE WS-MO              TO WS-CHECK-MO
           MOVE WS-DAY             TO WS-CHECK-DAY
           MOVE WS-CHECK-DATE      TO FNB-ISSUE-DATE
           MOVE FNMICR-AMOUNT-PAID TO FNB-CHECK-AMT
100702     IF FNMICR-VOID-SW = 'V'
100702         MOVE FNMICR-VOID-SW TO FNB-VOID-IND  
100702     ELSE
               ADD +1 TO TOT-COUNT
               ADD FNB-CHECK-AMT TO FNB-TOTAL-AMT
100702     END-IF

           MOVE FNMICR-DRAFT-NO    TO FNB-CHECK-NO
           MOVE FNMICR-CHECK-DATE  TO WS-DATE
           WRITE FNB-ISSUE-RECORD  FROM FNB-WORK-RECORD

           .
       1000-EXIT.
           EXIT.

       9000-END.
      *
           WRITE FNB-ISSUE-RECORD FROM FNB-TOTAL-RECORD
100702*    CLOSE MICR-DRAFT-FILE
100702     CLOSE MICR-TRNSFR-FILE
           CLOSE FNB-ISSUE-FILE
           .
       9000-EXIT.
           EXIT.
