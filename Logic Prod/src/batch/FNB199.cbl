       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FNB199.
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      *  DESCRIPTION:                                                          :
      *      THIS PROGRAM GENERATES A REPORT FOR                               :
      *                                                                        :
      *      THE RECORDS ARE SORTED BY SUSPENSE CODE WITHIN ACCOUNT.           :
      *                                                                        :
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      *    DATE    BY  MODIFICATION                                            :
      * ========== === ========================================================:
      * 01/06/1999 VXO CREATION DATE                                           :
      * 03/19/1999 DAN CR1999030800006 FIXED PAGING PROBLEM                    :
      * 03/19/1999 DAN CR1999030800008 CHG WORKSITE TO UNDERWRITING            :
DAN03 * 10/22/1999 DAN IR1999101300003 2719000300 RPT TO AGY BEN               :
      * 02/24/2003 DJN CONVERT FROM MAINFRAME TO MICROFOCUS COBOL.             :
      * 11/06/2003 DJN CR2003110500005 INACTIVATE SEVERAL ACCOUNTS.            :
      *                NO MORE REPORT FOR UNDERWRITING.                        :
082506* 08/25/2006 AJR REMOVE FROM CLAIMS SYSTEM.                              :
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * INPUT FILES
           SELECT GL-SUSPENSE-TRANS-FILE
               ASSIGN TO EXTERNAL SYS010
               ORGANIZATION IS LINE SEQUENTIAL
               STATUS IS SYS010-STATUS.

      * OUTPUTS BY DEPARTMENT
           SELECT FNB199-GLS-POS
               ASSIGN TO EXTERNAL SYS007
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FNB199-GLS-AGY-BENEFITS
               ASSIGN TO EXTERNAL SYS008
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FNB199-GLS-CORP-FINANCE
               ASSIGN TO EXTERNAL SYS011
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FNB199-GLS-AGY-ACCT
               ASSIGN TO EXTERNAL SYS012
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FNB199-GLS-CID
               ASSIGN TO EXTERNAL SYS013
               ORGANIZATION IS LINE SEQUENTIAL.

082506**** PARM FILE
082506     SELECT PARM-FILE        ASSIGN       TO EXTERNAL IPARM
082506                             ORGANIZATION IS LINE SEQUENTIAL
082506                             FILE STATUS  IS PARM-STATUS.
082506

       DATA DIVISION.
       FILE SECTION.

       FD  GL-SUSPENSE-TRANS-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 120 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  GL-SUSPENSE-TRANS-RECORD.

           05  GLS-KEY.
               10  GLS-SUSPENSE-CODE     PIC X(15).
               10  GLS-ACCOUNT-NO.
                   15  GLS-MAJ-ACCT      PIC X(10).
                   15  GLS-DIV           PIC X(2).
                   15  GLS-CENTER        PIC X(4).
                   15  GLS-PRODUCT       PIC X(6).
                   15  GLS-STATE         PIC X(2).
               10  GLS-TRANS-DATE        PIC X(8).
               10  GLS-TRANS-TIME        PIC X(8).
           05  GLS-TRANS-AMOUNT          PIC S9(12)V99
                                             SIGN TRAILING SEPARATE.
           05  GLS-DESCRIPTION           PIC X(30).
           05  GLS-REFERENCE             PIC X(10).
           05  GLS-SOURCE                PIC X(6).
           05  FILLER                    PIC X(4).

       FD  FNB199-GLS-POS
           LABEL RECORDS ARE OMITTED
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  GLS-POS-RPT               PIC X(132).

       FD  FNB199-GLS-AGY-BENEFITS
           LABEL RECORDS ARE OMITTED
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  GLS-AGY-BENE-RPT          PIC X(132).

       FD  FNB199-GLS-CORP-FINANCE
           LABEL RECORDS ARE OMITTED
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  GLS-CORP-FIN-RPT          PIC X(132).

       FD  FNB199-GLS-AGY-ACCT
           LABEL RECORDS ARE OMITTED
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  GLS-AGY-ACCT-RPT          PIC X(132).

       FD  FNB199-GLS-CID
           LABEL RECORDS ARE OMITTED
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  GLS-CID-RPT               PIC X(132).

082506**** PARM FILE
082506 FD  PARM-FILE
082506     LABEL RECORDS ARE STANDARD
082506     RECORDING MODE IS F
082506     BLOCK CONTAINS 0 RECORDS.
082506 01  PARM.
082506     05  PARM-CYCLE-DATE PIC X(10) VALUE SPACES.
082506

       WORKING-STORAGE SECTION.

       01 RC                       PIC S9(9)   COMP-5 VALUE 0.
       01 LIST-REC                 PIC X(132).

       01  FILLER.
           05  SYS010-STATUS      PIC XX         VALUE ZERO.
               88  EOF                           VALUE '10'.
           05  WS-PREV-ACCOUNT    PIC X(24)      VALUE SPACE.
082506     05  PARM-STATUS        PIC XX         VALUE SPACES.

       01  FILLER.
           05  LINE-COUNT         PIC S9(3)      VALUE ZERO.
           05  PAGE-COUNT         PIC S9(5)      VALUE ZERO.
           05  OUTPUT-COUNT       PIC S9(9)      VALUE ZERO.
           05  ERROR-COUNT        PIC S9(9)      VALUE ZERO.
           05  INPUT-COUNT        PIC S9(9)      VALUE ZERO.

       01  WS-CTRL-SUSP-CODE      PIC X(15)      VALUE SPACES.
       01  WS-CTRL-MAJ-ACCT       PIC X(10)      VALUE SPACES.
       01  WS-PREVIOUS-ACCT       PIC X(10)      VALUE SPACES.
       01  MAJ-ACCT-CTRL-BREAK    PIC X          VALUE 'N'.
       01  SUSP-CD-CTRL-BREAK     PIC X          VALUE 'N'.

       01  WS-ACCUM-AMT           PIC S9(12)V99  VALUE ZEROES.
       01  WS-ACCUM-TOTAL-AMT     PIC S9(12)V99  VALUE ZEROES.
       01  WS-TOTAL-AMT           PIC S9(12)V99  VALUE ZEROES.

       01  FILLER.
           05  WS-CYCLE-DATE       PIC X(10).

       01  WS-CURRENT-DATE.
           05  WS-CURR-MM         PIC 99         VALUE ZEROES.
           05  WS-CURR-DD         PIC 99         VALUE ZEROES.
           05  WS-CURR-CC         PIC 99         VALUE ZEROES.
           05  WS-CURR-YY         PIC 99         VALUE ZEROES.

       01  WK-TRANS-DATE.
           05  WK-CC              PIC 99.
           05  WK-YY              PIC 99.
           05  WK-MM              PIC 99.
           05  WK-DD              PIC 99.

082506 01  WORK-DATE.
082506     05  WRK-YR    PIC 9999.
082506     05  WRK-MO    PIC 99.
082506     05  WRK-DAY   PIC 99.
082506
082506 01  SYSTEM-DATE.
082506     05  SYS-MO    PIC 99.
082506     05  SYS-DAY   PIC 99.
082506     05  SYS-YR    PIC 9999.

       01  PRINT-SUSPENSE-CODE    PIC X          VALUE ZERO.
           88  PRINT-SUSP-CODE                   VALUE '1'.
           88  DO-NOT-PRINT-CODE                 VALUE '2'.

       01  ACTIVE-ACCOUNT-INICATORS.
           05  GLS-2719000300-IND     PIC X      VALUE 'N'.
           05  GLS-2719000301-IND     PIC X      VALUE 'N'.
           05  GLS-2719000302-IND     PIC X      VALUE 'N'.
           05  GLS-2719000307-IND     PIC X      VALUE 'N'.
           05  GLS-2719000311-IND     PIC X      VALUE 'N'.
           05  GLS-2719000322-IND     PIC X      VALUE 'N'.
           05  GLS-1825011300-IND     PIC X      VALUE 'N'.

DAN01  01  HDR-VALUES.
           05  PIC X(10) VALUE '2719000300'.
           05  PIC X(38) VALUE 'CLEARING ACCOUNT - DAILY CYCLE        '.
           05  PIC X(10) VALUE '2719000301'.
           05  PIC X(38) VALUE 'CLEARING ACCOUNT - DEATH              '.
           05  PIC X(10) VALUE '2719000302'.
           05  PIC X(38) VALUE 'CLEARING ACCOUNT - LOANS              '.
           05  PIC X(10) VALUE '2719000307'.
           05  PIC X(38) VALUE 'CLEARING ACCOUNT - SURRENDERS         '.
           05  PIC X(10) VALUE '2719000311'.
           05  PIC X(38) VALUE 'CWA SUSPENSE                          '.
           05  PIC X(10) VALUE '2719000322'.
           05  PIC X(38) VALUE 'UNAPPLIED CASH                        '.

       01  REDEFINES HDR-VALUES.
           05  ACCT-DESCR-TABLE OCCURS 6 TIMES INDEXED BY AT-INDEX.
               10  AT-ACCT      PIC X(10).
DAN01          10  AT-DESCR     PIC X(38).

      *****************************************************************
      *  REPORT HEADER DEFINITION                                     *
      *****************************************************************
       01  HDR-1.
           05  FILLER           PIC X(06)   VALUE 'DATE: '.
           05  WS-RPT-DATE.
               10  WS-RPT-MM    PIC X(02)   VALUE SPACES.
               10  FILLER       PIC X(01)   VALUE '/'.
               10  WS-RPT-DD    PIC X(02)   VALUE SPACES.
               10  FILLER       PIC X(01)   VALUE '/'.
               10  WS-RPT-CC    PIC X(02)   VALUE SPACES.
               10  WS-RPT-YY    PIC X(02)   VALUE SPACES.
           05  FILLER           PIC X(20)   VALUE SPACES.
           05  FILLER           PIC X(07)   VALUE 'AS OF: '.
           05  WS-CYC-DATE      PIC X(10)   VALUE SPACES.
           05  FILLER           PIC X(69)   VALUE SPACES.
           05  FILLER           PIC X(05)   VALUE 'PAGE '.
           05  WS-PAGE-NO       PIC ZZZZ9   VALUE ZEROES.

       01  HDR-2.
           05  HDR-2-VALUE      PIC X(38)   VALUE SPACES.
           05  FILLER           PIC X(95)   VALUE SPACES.

       01  HDR-3.
           05  FILLER           PIC X(13)   VALUE 'MSA ACCOUNT: '.
           05  WS-MSA-ACCT      PIC X(07)   VALUE SPACES.
           05  FILLER           PIC X(04)   VALUE SPACES.
           05  FILLER           PIC X(17)   VALUE 'FREEDOM ACCOUNT: '.
           05  WS-FREEDOM-ACCT  PIC X(13)   VALUE SPACES.
           05  FILLER           PIC X(78)   VALUE SPACES.
      *
       01  HDR-4.
           05  FILLER           PIC X(14)  VALUE 'DISTRIBUTION: '.
           05  WS-DISTRIBUTION  PIC X(21)  VALUE
                                           '                     '.
           05  FILLER           PIC X(97)  VALUE SPACES.

       01  DETAIL-HDR1.
           05  FILLER           PIC X(10)  VALUE '   MAJOR  '.
           05  FILLER           PIC X(03)  VALUE SPACES.
           05  FILLER           PIC X(08)  VALUE ' TRANS  '.
           05  FILLER           PIC X(03)  VALUE SPACES.
           05  FILLER           PIC X(15)  VALUE '  TRANSACTION  '.
           05  FILLER           PIC X(03)  VALUE SPACES.
           05  FILLER           PIC X(30)  VALUE SPACES.
           05  FILLER           PIC X(04)  VALUE SPACES.
           05  FILLER           PIC X(06)  VALUE 'SOURCE'.
           05  FILLER           PIC X(04)  VALUE SPACES.
           05  FILLER           PIC X(15)  VALUE '    SUSPENSE   '.
           05  FILLER           PIC X(03)  VALUE SPACES.
           05  FILLER           PIC X(11)  VALUE 'TRANSACTION'.
           05  FILLER           PIC X(02)  VALUE SPACES.
           05  FILLER           PIC X(15)  VALUE ' SUSPENSE CODE '.

       01  DETAIL-HDR1B.
           05  FILLER           PIC X(10)  VALUE '  ACCOUNT '.
           05  FILLER           PIC X(03)  VALUE SPACES.
           05  FILLER           PIC X(08)  VALUE '  DATE  '.
           05  FILLER           PIC X(03)  VALUE SPACES.
           05  FILLER           PIC X(15)  VALUE '     AMOUNT    '.
           05  FILLER           PIC X(03)  VALUE SPACES.
           05  FILLER           PIC X(30)  VALUE
                                'TRANSACTION DESCRIPTION       '.
           05  FILLER           PIC X(04)  VALUE SPACES.
           05  FILLER           PIC X(06)  VALUE ' CODE '.
           05  FILLER           PIC X(04)  VALUE SPACES.
           05  FILLER           PIC X(15)  VALUE '      CODE     '.
           05  FILLER           PIC X(03)  VALUE SPACES.
           05  FILLER           PIC X(11)  VALUE ' REFERENCE '.
           05  FILLER           PIC X(02)  VALUE SPACES.
           05  FILLER           PIC X(15)  VALUE '  TOTAL AMOUNT '.
       01  DETAIL-HDR2.

           05  FILLER           PIC X(10)  VALUE ALL '*'.
           05  FILLER           PIC X(03)  VALUE SPACES.
           05  FILLER           PIC X(08)  VALUE ALL '*'.
           05  FILLER           PIC X(03)  VALUE SPACES.
           05  FILLER           PIC X(15)  VALUE ALL '*'.
           05  FILLER           PIC X(03)  VALUE SPACES.
           05  FILLER           PIC X(30)  VALUE ALL '*'.
           05  FILLER           PIC X(04)  VALUE SPACES.
           05  FILLER           PIC X(06)  VALUE ALL '*'.
           05  FILLER           PIC X(04)  VALUE SPACES.
           05  FILLER           PIC X(15)  VALUE ALL '*'.
           05  FILLER           PIC X(03)  VALUE SPACES.
           05  FILLER           PIC X(11)  VALUE ALL '*'.
           05  FILLER           PIC X(02)  VALUE SPACES.
           05  FILLER           PIC X(15)  VALUE ALL '*'.

       01  DETAIL-HDR3.
           05  FILLER           PIC X(132) VALUE ALL '-'.

       01  DETAIL-HDR4.
           05  FILLER           PIC X(40) VALUE ALL SPACES.
           05  FILLER           PIC X(53) VALUE
           '*** NO RECORDS TO PROCESS FOR THIS ACCOUNT NUMBER ***'.
           05  FILLER           PIC X(39) VALUE ALL SPACES.

       01  BLANK-LINE.
           05  FILLER           PIC X(132) VALUE SPACES.

       01  DETAIL-1.
           05  WS-MAJOR-ACCT    PIC X(10)                VALUE SPACES.
           05  FILLER           PIC X(03)                VALUE SPACES.
           05  WS-TRANS-DATE.
               10  WS-TRANS-MM  PIC X(02)                VALUE SPACES.
               10  FILLER       PIC X(01)                VALUE '/'.
               10  WS-TRANS-DD  PIC X(02)                VALUE SPACES.
               10  FILLER       PIC X(01)                VALUE '/'.
               10  WS-TRANS-YY  PIC X(02)                VALUE SPACES.
           05  FILLER           PIC X(03)                VALUE SPACES.
           05  WS-TRANS-AMOUNT  PIC ----,---,--9.99      VALUE ZEROES.
           05  FILLER           PIC X(03)                VALUE SPACES.
           05  WS-TRANS-DESC    PIC X(30)                VALUE SPACES.
           05  FILLER           PIC X(04)                VALUE SPACES.
           05  WS-SOURCE-CODE   PIC X(06)                VALUE SPACES.
           05  FILLER           PIC X(04)                VALUE SPACES.
           05  WS-SUSP-CODE     PIC X(15)                VALUE SPACES.
           05  FILLER           PIC X(03)                VALUE SPACES.
           05  WS-REFERENCE     PIC X(11)                VALUE SPACES.
           05  FILLER           PIC X(18)                VALUE SPACES.

       01  TOTAL-1.
           05  FILLER           PIC X(117)               VALUE SPACES.
           05  WS-SUSP-TOTAL    PIC ----,---,--9.99      VALUE ZEROES.

       01  TOTAL-2.
           05  FILLER           PIC X(03)                VALUE SPACES.
           05  FILLER           PIC X(26)                VALUE
                                        'REPORT TRANSACTION TOTAL: '.
           05  WS-TRANS-TOTAL   PIC ----,---,--9.99      VALUE ZEROES.
           05  FILLER           PIC X(88)                VALUE SPACES.

082506*    EXEC SQL INCLUDE ISTDWORK.INC END-EXEC.

       PROCEDURE DIVISION.

           PERFORM 0000-START

           PERFORM 1000-GENERATE-REPORT UNTIL EOF

           PERFORM 7000-PROCESS-ACCT-CTRL-BREAK

           PERFORM 9000-PROCESS-BLANK-REPORTS

           PERFORM 9999-END

082506*    GOBACK GIVING RC.
082506     STOP RUN GIVING RC.

      ***---------------------------------------------------------***
      ***--  PROCESS 0000-START:                                --***
      ***--  1.  OPEN INPUT AND OUTPUT FILE.                    --***
      ***--  2.  CHECK OPEN STATUS ON INPUT FILE.               --***
      ***--  3.  INITIALIZE COUNTS.                             --***
      ***---------------------------------------------------------***
       0000-START.

082506     CALL 'FNBLIST' USING 'O' LIST-REC
           OPEN   INPUT  GL-SUSPENSE-TRANS-FILE

           IF SYS010-STATUS NOT = '00'
             DISPLAY 'SYS010 OPEN ERROR ' SYS010-STATUS UPON SYSERR
             MOVE SPACES TO LIST-REC
             STRING  'SYS010 OPEN ERROR ' SYS010-STATUS
               DELIMITED BY SIZE INTO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
082506       CALL 'FNBLIST' USING 'C' LIST-REC
             MOVE 16 TO RC
082506*      GOBACK GIVING RC
082506       STOP RUN GIVING RC
           END-IF

           OPEN   OUTPUT FNB199-GLS-POS
                  OUTPUT FNB199-GLS-AGY-BENEFITS
                  OUTPUT FNB199-GLS-CORP-FINANCE
                  OUTPUT FNB199-GLS-AGY-ACCT
                  OUTPUT FNB199-GLS-CID.

           MOVE  ZEROES   TO  INPUT-COUNT
           MOVE  ZEROES   TO  OUTPUT-COUNT
           MOVE  ZEROES   TO  LINE-COUNT
           MOVE  ZEROES   TO  PAGE-COUNT
           MOVE  ZEROES   TO  ERROR-COUNT

082506     ACCEPT WORK-DATE FROM DATE YYYYMMDD
082506     MOVE WRK-YR  TO SYS-YR
082506     MOVE WRK-MO  TO SYS-MO
082506     MOVE WRK-DAY TO SYS-DAY
082506
082506     MOVE SYSTEM-DATE TO WS-CURRENT-DATE
082506*    CALL 'SYSDATE' USING WS-CURRENT-DATE
           MOVE  WS-CURR-MM   TO  WS-RPT-MM
           MOVE  WS-CURR-DD   TO  WS-RPT-DD
           MOVE  WS-CURR-CC   TO  WS-RPT-CC
           MOVE  WS-CURR-YY   TO  WS-RPT-YY

082506*    CALL IGETPARM USING IG-P1
082506*    MOVE IG-P1 TO WS-CYCLE-DATE
082506*
082506*    MOVE WS-CYCLE-DATE TO IV-P2
082506*    MOVE 'MM/DD/YEAR'  TO IV-P3
082506*    INITIALIZE            IV-P4
082506*    CALL IVERDATE USING IV-P1 IV-P2 IV-P3 IV-P4
082506*    IF IV-P1 = 'Y'
082506     OPEN INPUT PARM-FILE.
082506     IF PARM-STATUS NOT = '00'
082506       STRING  'ERROR OPENING PARM FILE, STATUS = '
082506              PARM-STATUS
082506         DELIMITED BY SIZE INTO LIST-REC
082506       CALL 'FNBLIST' USING 'W' LIST-REC
082506       CALL 'FNBLIST' USING 'C' LIST-REC
082506       MOVE 16 TO RC
082506       STOP RUN GIVING RC
082506     END-IF.
082506     READ PARM-FILE.
082506     IF PARM-CYCLE-DATE GREATER THAN SPACES
082506       MOVE PARM-CYCLE-DATE TO WS-CYCLE-DATE
             MOVE WS-CYCLE-DATE TO WS-CYC-DATE
           ELSE
             MOVE SPACES TO LIST-REC
             STRING '*** INVALID DATE CARD: ' WS-CYCLE-DATE
               DELIMITED BY SIZE INTO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
082506       CALL 'FNBLIST' USING 'C' LIST-REC
             MOVE 16 TO RC
082506*      GOBACK GIVING RC
082506       STOP RUN GIVING RC
           END-IF.

      ***---------------------------------------------------------***
      ***--  PROCESS 1000-GENERATE-REPORT:                      --***
      ***--  1. READ INPUT FILE AND ACCUMULATE INPUT COUNT.     --***
      ***--  2. ONLY PROCESS RECORDS W/SPECIFIED ACCOUNT #.     --***
      ***--  3. PERFORM CONTROL CHECK ROUTINE FOR MAJ ACCT NO.  --***
      ***--  4. PERFORM CONTROL CHECK ROUTINE FOR SUSPENSE CD.  --***
      ***--  5. PERFORM ROUTINE TO PROCESS VALID RECORDS.       --***
      ***--  6. PERFORM ROUTINE TO WRITE VALID OUTPUT RECORDS.  --***
      ***---------------------------------------------------------***
       1000-GENERATE-REPORT.

           READ GL-SUSPENSE-TRANS-FILE
             AT END EXIT PARAGRAPH
           END-READ

           ADD  +1   TO  INPUT-COUNT

           IF GLS-ACCOUNT-NO = '271900030000000000000000' OR
                               '271900030100000000000000' OR
                               '271900030200000000000000' OR
                               '271900030700000000000000' OR
                               '271900031100000000000000' OR
                               '271900032200000000000000' OR
                               '182501130002000000000000'
             CONTINUE
           ELSE
             ADD  +1   TO  ERROR-COUNT
             EXIT PARAGRAPH
           END-IF

           PERFORM 5000-MAJ-ACCT-BREAK-CHECK

           IF MAJ-ACCT-CTRL-BREAK = 'Y'
             PERFORM 7000-PROCESS-ACCT-CTRL-BREAK
             PERFORM 8000-PRINT-HEADERS
             PERFORM 5500-SUSP-CODE-BREAK-CHECK
             PERFORM 3000-PROCESS-DETAIL-LINE
             PERFORM 4000-WRITE-DETAIL-LINE
             MOVE WS-CTRL-MAJ-ACCT TO WS-PREVIOUS-ACCT
             EXIT PARAGRAPH
           END-IF

           PERFORM 5500-SUSP-CODE-BREAK-CHECK

           IF SUSP-CD-CTRL-BREAK = 'Y'
             PERFORM 7500-PROCESS-SUSP-CTRL-BREAK
           END-IF

           PERFORM 3000-PROCESS-DETAIL-LINE

           PERFORM 4000-WRITE-DETAIL-LINE.

      ***---------------------------------------------------------***
      ***--  PROCESS 3000-PROCESS-DETAIL-LINE:                  --***
      ***--  1.  PERFORM ROUTINE TO FORMAT THE TRANSACTION      --***
      ***--      DATE AS MM/DD/YY.                              --***
      ***--  2.  MOVE INPUT DATA TO OUTPUT FIELDS.              --***
      ***--  3.  SUSPENSE CODE SHOULD ONLY BE PRINTED ON FIRST  --***
      ***--      LINE FOR OF THAT GROUPING.                     --***
      ***--  4.  ACCUMULATE TOTAL FOR EACH GROUP OF SUSPENSE    --***
      ***--      CODES AND A TOTAL OF TRANSACTION AMOUNTS FOR   --***
      ***--      EACH ACCOUNT NUMBER.                           --***
      ***---------------------------------------------------------***
       3000-PROCESS-DETAIL-LINE.

           PERFORM 6000-FORMAT-TRANS-DATE

           MOVE GLS-MAJ-ACCT     TO WS-MAJOR-ACCT
           MOVE GLS-TRANS-AMOUNT TO WS-TRANS-AMOUNT
           MOVE GLS-DESCRIPTION  TO WS-TRANS-DESC
           MOVE GLS-REFERENCE    TO WS-REFERENCE
           MOVE GLS-SOURCE       TO WS-SOURCE-CODE

           ADD  GLS-TRANS-AMOUNT TO WS-ACCUM-AMT
           ADD  GLS-TRANS-AMOUNT TO WS-ACCUM-TOTAL-AMT

           IF PRINT-SUSP-CODE
             MOVE GLS-SUSPENSE-CODE TO WS-SUSP-CODE
           ELSE
             MOVE SPACES            TO WS-SUSP-CODE
           END-IF.

      ***---------------------------------------------------------***
      ***--  PROCESS 4000-WRITE-DETAIL-LINE.                    --***
      ***--  1.  DETERMINE IF HEADERS SHOULD BE PRINTED.        --***
      ***--  2.  WRITE OUTPUT RECORDS.                          --***
      ***--  3.  ACCUMULATE LINE AND OUTPUT COUNTS.             --***
      ***---------------------------------------------------------***
       4000-WRITE-DETAIL-LINE.

           IF LINE-COUNT > +55
             PERFORM 8000-PRINT-HEADERS
           END-IF

           EVALUATE GLS-MAJ-ACCT
      *      REPORT TO CORP FINANCE/CID
             WHEN '1825011300'
               MOVE 'Y' TO GLS-1825011300-IND
               WRITE GLS-CID-RPT FROM DETAIL-1 AFTER ADVANCING 1 LINE
               WRITE GLS-CORP-FIN-RPT FROM DETAIL-1
               AFTER ADVANCING 1 LINE
      *      REPORT TO POS
             WHEN '2719000300'
               MOVE 'Y' TO GLS-2719000300-IND
DAN03          WRITE GLS-AGY-BENE-RPT FROM DETAIL-1
               AFTER ADVANCING 1 LINE
DAN02          MOVE SPACES TO WS-REFERENCE
               WRITE GLS-POS-RPT  FROM DETAIL-1 AFTER ADVANCING 1 LINE
      *      REPORT TO POS/AGY BENEFITS
             WHEN '2719000301'
               MOVE 'Y' TO GLS-2719000301-IND
               WRITE GLS-AGY-BENE-RPT FROM DETAIL-1
               AFTER ADVANCING 1 LINE
DAN02          MOVE SPACES TO WS-REFERENCE
               WRITE GLS-POS-RPT FROM DETAIL-1 AFTER ADVANCING 1 LINE
      *      REPORT TO POS
             WHEN '2719000302'
               MOVE 'Y' TO GLS-2719000302-IND
DAN02          MOVE SPACES TO WS-REFERENCE
               WRITE GLS-POS-RPT FROM DETAIL-1 AFTER ADVANCING 1 LINE
      *      REPORT TO POS
             WHEN '2719000307'
               MOVE 'Y' TO GLS-2719000307-IND
DAN02          MOVE SPACES TO WS-REFERENCE
               WRITE GLS-POS-RPT FROM DETAIL-1 AFTER ADVANCING 1 LINE
      *      REPORT TO AGENCY ACCOUNTING
             WHEN '2719000311'
               MOVE 'Y' TO GLS-2719000311-IND
               WRITE GLS-AGY-ACCT-RPT FROM DETAIL-1
               AFTER ADVANCING 1 LINE
      *      REPORT TO POS
             WHEN '2719000322'
               MOVE 'Y' TO GLS-2719000322-IND
DAN02          MOVE SPACES TO WS-REFERENCE
               WRITE GLS-POS-RPT FROM DETAIL-1 AFTER ADVANCING 1 LINE
           END-EVALUATE

           ADD   +1   TO  OUTPUT-COUNT
           ADD   +1   TO  LINE-COUNT.

      ***---------------------------------------------------------***
      ***--  PROCESS 5000-MAJ-ACCT-BREAK-CHECK.                 --***
      ***--  1.  CHECK FOR FIRST RECORD PROCESSING.             --***
      ***--  2.  CHECK FOR BREAK IN THE CENTER NUMBERS TO       --***
      ***--      DETERMINE A CONTROL BREAK, AND SET INDICATOR.  --***
      ***---------------------------------------------------------***
       5000-MAJ-ACCT-BREAK-CHECK.

           IF OUTPUT-COUNT IS EQUAL TO ZERO
             MOVE  GLS-MAJ-ACCT      TO  WS-CTRL-MAJ-ACCT
             MOVE  WS-CTRL-MAJ-ACCT  TO  WS-PREVIOUS-ACCT
             MOVE  ZERO  TO  PAGE-COUNT
             MOVE 'N' TO  MAJ-ACCT-CTRL-BREAK
             PERFORM 8000-PRINT-HEADERS
             EXIT PARAGRAPH
           END-IF

           IF  GLS-MAJ-ACCT   =  WS-CTRL-MAJ-ACCT
             MOVE  'N'  TO  MAJ-ACCT-CTRL-BREAK
           ELSE
             MOVE  ZERO    TO  PAGE-COUNT
             MOVE  'Y'     TO  MAJ-ACCT-CTRL-BREAK
           END-IF

           MOVE  GLS-MAJ-ACCT      TO  WS-CTRL-MAJ-ACCT.

      ***---------------------------------------------------------***
      ***--  PROCESS 5500-SUSP-CODE-BREAK-CHECK.                --***
      ***--  1.  CHECK FOR FIRST RECORD PROCESSING.             --***
      ***--  2.  SET PRINT SUSPENSE CODE INDICATOR.             --***
      ***--  3.  CHECK FOR BREAK IN THE CENTER NUMBERS TO       --***
      ***--      DETERMINE A CONTROL BREAK, AND SET INDICATOR.  --***
      ***---------------------------------------------------------***
       5500-SUSP-CODE-BREAK-CHECK.

           IF  OUTPUT-COUNT IS EQUAL TO ZERO
             MOVE  GLS-SUSPENSE-CODE  TO  WS-CTRL-SUSP-CODE
             MOVE 'N' TO  SUSP-CD-CTRL-BREAK
             MOVE  '1'  TO  PRINT-SUSPENSE-CODE
             EXIT PARAGRAPH
           END-IF

           IF GLS-SUSPENSE-CODE = WS-CTRL-SUSP-CODE
             MOVE 'N' TO SUSP-CD-CTRL-BREAK
             MOVE '2' TO PRINT-SUSPENSE-CODE
           ELSE
             MOVE '1' TO PRINT-SUSPENSE-CODE
             MOVE 'Y' TO SUSP-CD-CTRL-BREAK
           END-IF

           MOVE GLS-SUSPENSE-CODE TO WS-CTRL-SUSP-CODE.

      ***---------------------------------------------------------***
      ***--  PROCESS 6000-FORMAT-TRANS-DATE.                    --***
      ***--  1.  MOVE INPUT DATE TO OUTPUT FIELDS.              --***
      ***---------------------------------------------------------***
       6000-FORMAT-TRANS-DATE.

           MOVE GLS-TRANS-DATE TO WK-TRANS-DATE
           MOVE WK-MM          TO WS-TRANS-MM
           MOVE WK-DD          TO WS-TRANS-DD
           MOVE WK-YY          TO WS-TRANS-YY.

      ***---------------------------------------------------------***
      ***--  PROCESS 7000-PROCESS-ACCT-CTRL-BREAK.              --***
      ***--  1. MOVE ACCUMULATED AMOUNTS TO THE SUSPENSE AND    --***
      ***--     TRANSACTION TOTAL OUTPUT FIELDS.                --***
      ***--  2. PRINT LAST LINE OF THE SUSPENSE TOTAL AMOUNT    --***
      ***--     FOR THE PREVIOUS ACCOUNT NUMBER.                --***
      ***--  3. PRINT LAST LINE OF THE TRANSACTION TOTAL AMOUNT --***
      ***--     FOR THE PREVIOUS ACCOUNT NUMBER.                --***
      ***--  4. ZERO OUT LINE COUNT & ACCUMULATED AMOUNT FIELDS.--***
      ***---------------------------------------------------------***
       7000-PROCESS-ACCT-CTRL-BREAK.

           MOVE WS-ACCUM-AMT        TO  WS-SUSP-TOTAL
           MOVE WS-ACCUM-TOTAL-AMT  TO  WS-TRANS-TOTAL

           EVALUATE WS-PREVIOUS-ACCT
      *      REPORT TO CORP FINANCE/CID
             WHEN '1825011300'
               WRITE GLS-CID-RPT FROM TOTAL-1     AFTER ADVANCING 1 LINE
               WRITE GLS-CID-RPT FROM DETAIL-HDR3 AFTER ADVANCING 1 LINE
               WRITE GLS-CID-RPT FROM TOTAL-2     AFTER ADVANCING 1 LINE
               WRITE GLS-CORP-FIN-RPT FROM TOTAL-1
                     AFTER ADVANCING 1 LINE
               WRITE GLS-CORP-FIN-RPT FROM DETAIL-HDR3
                     AFTER ADVANCING 1 LINE
               WRITE GLS-CORP-FIN-RPT FROM TOTAL-2
                     AFTER ADVANCING 1 LINE
      *      REPORT TO POS/AGY BENEFITS
             WHEN '2719000300'
               WRITE GLS-POS-RPT FROM TOTAL-1     AFTER ADVANCING 1 LINE
               WRITE GLS-POS-RPT FROM DETAIL-HDR3 AFTER ADVANCING 1 LINE
               WRITE GLS-POS-RPT FROM TOTAL-2     AFTER ADVANCING 1 LINE
DAN03          WRITE GLS-AGY-BENE-RPT FROM TOTAL-1
                     AFTER ADVANCING 1 LINE
DAN03          WRITE GLS-AGY-BENE-RPT FROM DETAIL-HDR3
                     AFTER ADVANCING 1 LINE
DAN03          WRITE GLS-AGY-BENE-RPT FROM TOTAL-2
                     AFTER ADVANCING 1 LINE
      *      REPORT TO POS/AGY BENEFITS
             WHEN '2719000301'
               WRITE GLS-POS-RPT FROM TOTAL-1     AFTER ADVANCING 1 LINE
               WRITE GLS-POS-RPT FROM DETAIL-HDR3 AFTER ADVANCING 1 LINE
               WRITE GLS-POS-RPT FROM TOTAL-2     AFTER ADVANCING 1 LINE
               WRITE GLS-AGY-BENE-RPT FROM TOTAL-1
                     AFTER ADVANCING 1 LINE
               WRITE GLS-AGY-BENE-RPT FROM DETAIL-HDR3
                     AFTER ADVANCING 1 LINE
               WRITE GLS-AGY-BENE-RPT FROM TOTAL-2
                     AFTER ADVANCING 1 LINE
      *      REPORT TO POS
             WHEN '2719000302'
               WRITE GLS-POS-RPT FROM TOTAL-1     AFTER ADVANCING 1 LINE
               WRITE GLS-POS-RPT FROM DETAIL-HDR3 AFTER ADVANCING 1 LINE
               WRITE GLS-POS-RPT FROM TOTAL-2     AFTER ADVANCING 1 LINE
      *      REPORT TO POS
             WHEN '2719000307'
               WRITE GLS-POS-RPT FROM TOTAL-1     AFTER ADVANCING 1 LINE
               WRITE GLS-POS-RPT FROM DETAIL-HDR3 AFTER ADVANCING 1 LINE
               WRITE GLS-POS-RPT FROM TOTAL-2     AFTER ADVANCING 1 LINE
      *      REPORT TO AGENCY ACCOUNTING
             WHEN '2719000311'
               WRITE GLS-AGY-ACCT-RPT FROM TOTAL-1
                     AFTER ADVANCING 1 LINE
               WRITE GLS-AGY-ACCT-RPT FROM DETAIL-HDR3
                     AFTER ADVANCING 1 LINE
               WRITE GLS-AGY-ACCT-RPT FROM TOTAL-2
                     AFTER ADVANCING 1 LINE
      *      REPORT TO POS
             WHEN '2719000322'
               WRITE GLS-POS-RPT FROM TOTAL-1     AFTER ADVANCING 1 LINE
               WRITE GLS-POS-RPT FROM DETAIL-HDR3 AFTER ADVANCING 1 LINE
               WRITE GLS-POS-RPT FROM TOTAL-2     AFTER ADVANCING 1 LINE
           END-EVALUATE.

           MOVE ZEROES TO LINE-COUNT
           MOVE ZEROES TO WS-ACCUM-AMT
           MOVE ZEROES TO WS-ACCUM-TOTAL-AMT.

      ***---------------------------------------------------------***
      ***--  PROCESS 7500-PROCESS-SUSP-CTRL-BREAK.              --***
      ***--  1. RESET SUSPENSE CODE CONTROL BREAK INDICATOR.    --***
      ***--  2. MOVE SUSPENSE CODE ACCUMULATED AMT TO OUTPUT.   --***
      ***--  2. PRINT SUSPENSE CODE TOTAL LINE.                 --***
      ***---------------------------------------------------------***
       7500-PROCESS-SUSP-CTRL-BREAK.

           MOVE  'N'           TO  SUSP-CD-CTRL-BREAK
           MOVE  WS-ACCUM-AMT  TO  WS-SUSP-TOTAL

           EVALUATE GLS-MAJ-ACCT
      *      REPORT TO CORP FINANCE/CID
             WHEN '1825011300'
               WRITE GLS-CID-RPT FROM TOTAL-1     AFTER ADVANCING 1 LINE
               WRITE GLS-CID-RPT FROM DETAIL-HDR3 AFTER ADVANCING 1 LINE
               WRITE GLS-CORP-FIN-RPT FROM TOTAL-1
                     AFTER ADVANCING 1 LINE
               WRITE GLS-CORP-FIN-RPT FROM DETAIL-HDR3
                     AFTER ADVANCING 1 LINE
      *      REPORT TO POS/AGY BENEFITS
             WHEN '2719000300'
               WRITE GLS-POS-RPT FROM TOTAL-1     AFTER ADVANCING 1 LINE
               WRITE GLS-POS-RPT FROM DETAIL-HDR3 AFTER ADVANCING 1 LINE
DAN03          WRITE GLS-AGY-BENE-RPT FROM TOTAL-1
                     AFTER ADVANCING 1 LINE
DAN03          WRITE GLS-AGY-BENE-RPT FROM DETAIL-HDR3
                     AFTER ADVANCING 1 LINE
      *      REPORT TO POS/AGY BENEFITS
             WHEN '2719000301'
               WRITE GLS-POS-RPT FROM TOTAL-1     AFTER ADVANCING 1 LINE
               WRITE GLS-POS-RPT FROM DETAIL-HDR3 AFTER ADVANCING 1 LINE
               WRITE GLS-AGY-BENE-RPT FROM TOTAL-1
                     AFTER ADVANCING 1 LINE
               WRITE GLS-AGY-BENE-RPT FROM DETAIL-HDR3
                     AFTER ADVANCING 1 LINE
      *      REPORT TO POS
             WHEN '2719000302'
               WRITE GLS-POS-RPT FROM TOTAL-1     AFTER ADVANCING 1 LINE
               WRITE GLS-POS-RPT FROM DETAIL-HDR3 AFTER ADVANCING 1 LINE
      *      REPORT TO POS
             WHEN '2719000307'
               WRITE GLS-POS-RPT FROM TOTAL-1     AFTER ADVANCING 1 LINE
               WRITE GLS-POS-RPT FROM DETAIL-HDR3 AFTER ADVANCING 1 LINE
      *      REPORT TO AGENCY ACCOUNTING
             WHEN '2719000311'
               WRITE GLS-AGY-ACCT-RPT FROM TOTAL-1
                     AFTER ADVANCING 1 LINE
               WRITE GLS-AGY-ACCT-RPT FROM DETAIL-HDR3
                     AFTER ADVANCING 1 LINE
      *      REPORT TO POS
             WHEN '2719000322'
               WRITE GLS-POS-RPT FROM TOTAL-1     AFTER ADVANCING 1 LINE
               WRITE GLS-POS-RPT FROM DETAIL-HDR3 AFTER ADVANCING 1 LINE
           END-EVALUATE

           ADD +2                  TO  LINE-COUNT

           MOVE ZEROES  TO WS-SUSP-TOTAL
                           WS-ACCUM-AMT.

      ***---------------------------------------------------------***
      ***--  PROCESS 8000-PRINT-HEADERS.                        --***
      ***--  1. RESET LINE COUNT TO ZERO.                       --***
      ***--  2. ADD 1 TO PAGE COUNT AND MOVE TO OUTPUT FIELD.   --***
      ***--  3. PERFORM ROUTINE TO DETERMINE OUTPUT FILE.       --***
      ***---------------------------------------------------------***
       8000-PRINT-HEADERS.

           MOVE ZEROES     TO LINE-COUNT
           ADD  +1         TO PAGE-COUNT
           MOVE PAGE-COUNT TO WS-PAGE-NO

           PERFORM 8400-EVALUATE-ACCOUNT

           ADD +10 TO LINE-COUNT.

      ***---------------------------------------------------------***
      ***--  PROCESS 8400-EVALUATE-ACCOUNT.                     --***
      ***--  1.  EVALUATE USED TO DETERMINE ACCOUNT NUMBER      --***
      ***--      BEING PROCESSED.  MOVE APPROPRIATE HEADER      --***
      ***--      INFO TO THE OUTPUT HEADER FIELDS.              --***
      ***--  2.  WRITE HEADERS.                                 --***
      ***---------------------------------------------------------***
       8400-EVALUATE-ACCOUNT.

DAN01      SET AT-INDEX TO +1
DAN01      SEARCH ACCT-DESCR-TABLE
DAN01        AT END
DAN01          MOVE 'CID SPECIAL CLEARING ACCOUNT' TO HDR-2-VALUE
DAN01        WHEN AT-ACCT (AT-INDEX) = GLS-MAJ-ACCT
DAN01          MOVE AT-DESCR (AT-INDEX) TO HDR-2-VALUE
DAN01      END-SEARCH

           EVALUATE GLS-MAJ-ACCT
             WHEN '1825011300'
                 MOVE '1209000'               TO WS-MSA-ACCT
                 MOVE '1825011300-02'         TO WS-FREEDOM-ACCT
                 MOVE 'CID                  ' TO WS-DISTRIBUTION
                 WRITE GLS-CID-RPT FROM HDR-1 AFTER ADVANCING PAGE
                 WRITE GLS-CID-RPT FROM HDR-2 AFTER ADVANCING 1 LINE
                 WRITE GLS-CID-RPT FROM HDR-3 AFTER ADVANCING 1 LINE
                 WRITE GLS-CID-RPT FROM BLANK-LINE
                      AFTER ADVANCING 1 LINE
                 WRITE GLS-CID-RPT FROM HDR-4
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-CID-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 2 LINES
                 WRITE  GLS-CID-RPT  FROM  DETAIL-HDR1
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-CID-RPT  FROM  DETAIL-HDR1B
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-CID-RPT  FROM  DETAIL-HDR2
                      AFTER  ADVANCING 1 LINE
                 MOVE 'CORPORATE FINANCE    '  TO  WS-DISTRIBUTION
                 WRITE  GLS-CORP-FIN-RPT  FROM  HDR-1
                      AFTER  ADVANCING PAGE
                 WRITE  GLS-CORP-FIN-RPT  FROM  HDR-2
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-CORP-FIN-RPT  FROM  HDR-3
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-CORP-FIN-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-CORP-FIN-RPT  FROM  HDR-4
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-CORP-FIN-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 2 LINES
                 WRITE  GLS-CORP-FIN-RPT  FROM  DETAIL-HDR1
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-CORP-FIN-RPT  FROM  DETAIL-HDR1B
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-CORP-FIN-RPT  FROM  DETAIL-HDR2
                      AFTER  ADVANCING 1 LINE
             WHEN '2719000300'
                 MOVE '2200015'                TO  WS-MSA-ACCT
                 MOVE '2719000300   '          TO  WS-FREEDOM-ACCT
                 MOVE 'POS DEPARTMENT       '  TO  WS-DISTRIBUTION
                 WRITE  GLS-POS-RPT  FROM  HDR-1
                      AFTER  ADVANCING PAGE
                 WRITE  GLS-POS-RPT  FROM  HDR-2
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  HDR-3
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  HDR-4
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 2 LINES
                 WRITE  GLS-POS-RPT  FROM  DETAIL-HDR1
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  DETAIL-HDR1B
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  DETAIL-HDR2
                      AFTER  ADVANCING 1 LINE
DAN03            MOVE 'AGENCY BENEFITS      '  TO  WS-DISTRIBUTION
                 WRITE  GLS-AGY-BENE-RPT  FROM  HDR-1
                      AFTER  ADVANCING PAGE
                 WRITE  GLS-AGY-BENE-RPT  FROM  HDR-2
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-AGY-BENE-RPT  FROM  HDR-3
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-AGY-BENE-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-AGY-BENE-RPT  FROM  HDR-4
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-AGY-BENE-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 2 LINES
                 WRITE  GLS-AGY-BENE-RPT  FROM  DETAIL-HDR1
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-AGY-BENE-RPT  FROM  DETAIL-HDR1B
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-AGY-BENE-RPT  FROM  DETAIL-HDR2
                      AFTER  ADVANCING 1 LINE
             WHEN '2719000301'
                 MOVE '2200018'                TO  WS-MSA-ACCT
                 MOVE '2719000301   '          TO  WS-FREEDOM-ACCT
                 MOVE 'POS DEPARTMENT       '  TO  WS-DISTRIBUTION
                 WRITE  GLS-POS-RPT  FROM  HDR-1
                      AFTER  ADVANCING PAGE
                 WRITE  GLS-POS-RPT  FROM  HDR-2
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  HDR-3
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  HDR-4
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 2 LINES
                 WRITE  GLS-POS-RPT  FROM  DETAIL-HDR1
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  DETAIL-HDR1B
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  DETAIL-HDR2
                      AFTER  ADVANCING 1 LINE
                 MOVE 'AGENCY BENEFITS      '  TO  WS-DISTRIBUTION
                 WRITE  GLS-AGY-BENE-RPT  FROM  HDR-1
                      AFTER  ADVANCING PAGE
                 WRITE  GLS-AGY-BENE-RPT  FROM  HDR-2
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-AGY-BENE-RPT  FROM  HDR-3
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-AGY-BENE-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-AGY-BENE-RPT  FROM  HDR-4
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-AGY-BENE-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 2 LINES
                 WRITE  GLS-AGY-BENE-RPT  FROM  DETAIL-HDR1
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-AGY-BENE-RPT  FROM  DETAIL-HDR1B
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-AGY-BENE-RPT  FROM  DETAIL-HDR2
                      AFTER  ADVANCING 1 LINE
             WHEN '2719000302'
                 MOVE '2200016'                TO  WS-MSA-ACCT
                 MOVE '2719000302'             TO  WS-FREEDOM-ACCT
                 MOVE 'POS DEPARTMENT       '  TO  WS-DISTRIBUTION
                 WRITE  GLS-POS-RPT  FROM  HDR-1
                      AFTER  ADVANCING PAGE
                 WRITE  GLS-POS-RPT  FROM  HDR-2
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  HDR-3
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  HDR-4
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 2 LINES
                 WRITE  GLS-POS-RPT  FROM  DETAIL-HDR1
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  DETAIL-HDR1B
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  DETAIL-HDR2
                      AFTER  ADVANCING 1 LINE
             WHEN '2719000307'
                 MOVE '2200017'                TO  WS-MSA-ACCT
                 MOVE '2719000307   '          TO  WS-FREEDOM-ACCT
                 MOVE 'POS DEPARTMENT       '  TO  WS-DISTRIBUTION
                 WRITE  GLS-POS-RPT  FROM  HDR-1
                      AFTER  ADVANCING PAGE
                 WRITE  GLS-POS-RPT  FROM  HDR-2
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  HDR-3
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  HDR-4
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 2 LINES
                 WRITE  GLS-POS-RPT  FROM  DETAIL-HDR1
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  DETAIL-HDR1B
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  DETAIL-HDR2
                      AFTER  ADVANCING 1 LINE
             WHEN '2719000311'
                 MOVE '2200001'                TO  WS-MSA-ACCT
                 MOVE '2719000311   '          TO  WS-FREEDOM-ACCT
                 MOVE 'AGENCY ACCOUNTING    '  TO  WS-DISTRIBUTION
                 WRITE  GLS-AGY-ACCT-RPT    FROM  HDR-1
                      AFTER  ADVANCING PAGE
                 WRITE  GLS-AGY-ACCT-RPT    FROM  HDR-2
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-AGY-ACCT-RPT    FROM  HDR-3
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-AGY-ACCT-RPT    FROM  BLANK-LINE
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-AGY-ACCT-RPT    FROM  HDR-4
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-AGY-ACCT-RPT    FROM  BLANK-LINE
                      AFTER  ADVANCING 2 LINES
                 WRITE  GLS-AGY-ACCT-RPT    FROM  DETAIL-HDR1
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-AGY-ACCT-RPT    FROM  DETAIL-HDR1B
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-AGY-ACCT-RPT    FROM  DETAIL-HDR2
                      AFTER  ADVANCING 1 LINE
             WHEN '2719000322'
                 MOVE '2200010'                TO  WS-MSA-ACCT
                 MOVE '2719000322   '          TO  WS-FREEDOM-ACCT
                 MOVE 'POS DEPARTMENT       '  TO  WS-DISTRIBUTION
                 WRITE  GLS-POS-RPT  FROM  HDR-1
                      AFTER  ADVANCING PAGE
                 WRITE  GLS-POS-RPT  FROM  HDR-2
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  HDR-3
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  HDR-4
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 2 LINES
                 WRITE  GLS-POS-RPT  FROM  DETAIL-HDR1
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  DETAIL-HDR1B
                      AFTER  ADVANCING 1 LINE
                 WRITE  GLS-POS-RPT  FROM  DETAIL-HDR2
                      AFTER  ADVANCING 1 LINE
           END-EVALUATE.

      ***---------------------------------------------------------***
      ***--  PROCESS 9000-PROCESS-BLANK-REPORTS                 --***
      ***--  1. CLOSE FILES.                                    --***
      ***--  2. DISPLAY VARIOUS RECORD COUNTS.                  --***
      ***---------------------------------------------------------***
       9000-PROCESS-BLANK-REPORTS.

031999     MOVE 1 TO WS-PAGE-NO
           IF GLS-2719000300-IND   =   'N'
              MOVE '2200015'                TO  WS-MSA-ACCT
              MOVE '2719000300   '          TO  WS-FREEDOM-ACCT
              MOVE 'POS DEPARTMENT       '  TO  WS-DISTRIBUTION
              WRITE GLS-POS-RPT  FROM  HDR-1
                     AFTER  ADVANCING PAGE
              WRITE GLS-POS-RPT  FROM  HDR-2
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-POS-RPT  FROM  HDR-3
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-POS-RPT  FROM  BLANK-LINE
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-POS-RPT  FROM  HDR-4
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-POS-RPT  FROM  BLANK-LINE
                     AFTER  ADVANCING 2 LINES
              WRITE GLS-POS-RPT  FROM  DETAIL-HDR1
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-POS-RPT  FROM  DETAIL-HDR1B
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-POS-RPT  FROM  DETAIL-HDR2
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-POS-RPT  FROM  DETAIL-HDR4
                     AFTER  ADVANCING 5 LINES
           END-IF.
DAN03      MOVE 'AGENCY BENEFITS      '  TO  WS-DISTRIBUTION
           WRITE GLS-AGY-BENE-RPT FROM HDR-1 AFTER ADVANCING PAGE
           WRITE GLS-AGY-BENE-RPT FROM HDR-2 AFTER ADVANCING 1 LINE
           WRITE GLS-AGY-BENE-RPT FROM HDR-3 AFTER ADVANCING 1 LINE
           WRITE GLS-AGY-BENE-RPT FROM BLANK-LINE AFTER ADVANCING 1 LINE
           WRITE GLS-AGY-BENE-RPT FROM HDR-4      AFTER ADVANCING 1 LINE
           WRITE GLS-AGY-BENE-RPT FROM BLANK-LINE
                 AFTER ADVANCING 2 LINES
           WRITE GLS-AGY-BENE-RPT  FROM  DETAIL-HDR1
                 AFTER  ADVANCING 1 LINE
           WRITE GLS-AGY-BENE-RPT  FROM  DETAIL-HDR1B
                 AFTER  ADVANCING 1 LINE
           WRITE GLS-AGY-BENE-RPT  FROM  DETAIL-HDR2
                 AFTER  ADVANCING 1 LINE
           WRITE GLS-AGY-BENE-RPT  FROM  DETAIL-HDR4
                 AFTER  ADVANCING 5 LINES.
           IF GLS-2719000301-IND   =   'N'
              MOVE '2200018'                TO  WS-MSA-ACCT
              MOVE '2719000301   '          TO  WS-FREEDOM-ACCT
              MOVE 'POS DEPARTMENT       '  TO  WS-DISTRIBUTION
              WRITE GLS-POS-RPT  FROM  HDR-1
                     AFTER  ADVANCING PAGE
              WRITE GLS-POS-RPT  FROM  HDR-2
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-POS-RPT  FROM  HDR-3
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-POS-RPT  FROM  BLANK-LINE
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-POS-RPT  FROM  HDR-4
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-POS-RPT  FROM  BLANK-LINE
                     AFTER  ADVANCING 2 LINES
              WRITE GLS-POS-RPT  FROM  DETAIL-HDR1
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-POS-RPT  FROM  DETAIL-HDR1B
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-POS-RPT  FROM  DETAIL-HDR2
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-POS-RPT  FROM  DETAIL-HDR4
                     AFTER  ADVANCING 5 LINES
              MOVE 'AGENCY BENEFITS      '  TO  WS-DISTRIBUTION
              WRITE GLS-AGY-BENE-RPT  FROM  HDR-1
                     AFTER  ADVANCING PAGE
              WRITE GLS-AGY-BENE-RPT  FROM  HDR-2
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-AGY-BENE-RPT  FROM  HDR-3
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-AGY-BENE-RPT  FROM  BLANK-LINE
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-AGY-BENE-RPT  FROM  HDR-4
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-AGY-BENE-RPT  FROM  BLANK-LINE
                     AFTER  ADVANCING 2 LINES
              WRITE GLS-AGY-BENE-RPT  FROM  DETAIL-HDR1
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-AGY-BENE-RPT  FROM  DETAIL-HDR1B
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-AGY-BENE-RPT  FROM  DETAIL-HDR2
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-AGY-BENE-RPT  FROM  DETAIL-HDR4
                     AFTER  ADVANCING 5 LINES.
           IF GLS-2719000302-IND   =   'N'
              MOVE '2200016'                TO  WS-MSA-ACCT
              MOVE '2719000302   '          TO  WS-FREEDOM-ACCT
              MOVE 'POS DEPARTMENT       '  TO  WS-DISTRIBUTION
              WRITE GLS-POS-RPT  FROM  HDR-1
                     AFTER  ADVANCING PAGE
              WRITE GLS-POS-RPT  FROM  HDR-2
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-POS-RPT  FROM  HDR-3
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-POS-RPT  FROM  BLANK-LINE
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-POS-RPT  FROM  HDR-4
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-POS-RPT  FROM  BLANK-LINE
                     AFTER  ADVANCING 2 LINES
              WRITE GLS-POS-RPT  FROM  DETAIL-HDR1
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-POS-RPT  FROM  DETAIL-HDR1B
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-POS-RPT  FROM  DETAIL-HDR2
                     AFTER  ADVANCING 1 LINE
              WRITE GLS-POS-RPT  FROM  DETAIL-HDR4
                     AFTER  ADVANCING 5 LINES.
           IF  GLS-2719000307-IND   =   'N'
               MOVE '2200017'                TO  WS-MSA-ACCT
               MOVE '2719000307   '          TO  WS-FREEDOM-ACCT
               MOVE 'POS DEPARTMENT       '  TO  WS-DISTRIBUTION
               WRITE GLS-POS-RPT  FROM  HDR-1
                      AFTER  ADVANCING PAGE
               WRITE GLS-POS-RPT  FROM  HDR-2
                      AFTER  ADVANCING 1 LINE
               WRITE GLS-POS-RPT  FROM  HDR-3
                      AFTER  ADVANCING 1 LINE
               WRITE GLS-POS-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 1 LINE
               WRITE GLS-POS-RPT  FROM  HDR-4
                      AFTER  ADVANCING 1 LINE
               WRITE GLS-POS-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 2 LINES
               WRITE GLS-POS-RPT  FROM  DETAIL-HDR1
                      AFTER  ADVANCING 1 LINE
               WRITE GLS-POS-RPT  FROM  DETAIL-HDR1B
                      AFTER  ADVANCING 1 LINE
               WRITE GLS-POS-RPT  FROM  DETAIL-HDR2
                      AFTER  ADVANCING 1 LINE
               WRITE GLS-POS-RPT  FROM  DETAIL-HDR4
                      AFTER  ADVANCING 5 LINES.
           IF  GLS-2719000311-IND   =   'N'
               MOVE '2200001'                TO  WS-MSA-ACCT
               MOVE '2719000311   '          TO  WS-FREEDOM-ACCT
               MOVE 'AGENCY ACCOUNTING    '  TO  WS-DISTRIBUTION
               WRITE GLS-AGY-ACCT-RPT  FROM  HDR-1
                      AFTER  ADVANCING PAGE
               WRITE GLS-AGY-ACCT-RPT  FROM  HDR-2
                      AFTER  ADVANCING 1 LINE
               WRITE GLS-AGY-ACCT-RPT  FROM  HDR-3
                      AFTER  ADVANCING 1 LINE
               WRITE GLS-AGY-ACCT-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 1 LINE
               WRITE GLS-AGY-ACCT-RPT  FROM  HDR-4
                      AFTER  ADVANCING 1 LINE
               WRITE GLS-AGY-ACCT-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 2 LINES
               WRITE GLS-AGY-ACCT-RPT  FROM  DETAIL-HDR1
                      AFTER  ADVANCING 1 LINE
               WRITE GLS-AGY-ACCT-RPT  FROM  DETAIL-HDR1B
                      AFTER  ADVANCING 1 LINE
               WRITE GLS-AGY-ACCT-RPT  FROM  DETAIL-HDR2
                      AFTER  ADVANCING 1 LINE
               WRITE GLS-AGY-ACCT-RPT  FROM  DETAIL-HDR4
                      AFTER  ADVANCING 5 LINES.
           IF  GLS-2719000322-IND   =   'N'
               MOVE '2200010'                TO  WS-MSA-ACCT
               MOVE '2719000322   '          TO  WS-FREEDOM-ACCT
               MOVE 'POS DEPARTMENT       '  TO  WS-DISTRIBUTION
               WRITE GLS-POS-RPT  FROM  HDR-1
                      AFTER  ADVANCING PAGE
               WRITE GLS-POS-RPT  FROM  HDR-2
                      AFTER  ADVANCING 1 LINE
               WRITE GLS-POS-RPT  FROM  HDR-3
                      AFTER  ADVANCING 1 LINE
               WRITE GLS-POS-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 1 LINE
               WRITE GLS-POS-RPT  FROM  HDR-4
                      AFTER  ADVANCING 1 LINE
               WRITE GLS-POS-RPT  FROM  BLANK-LINE
                      AFTER  ADVANCING 2 LINES
               WRITE GLS-POS-RPT  FROM  DETAIL-HDR1
                      AFTER  ADVANCING 1 LINE
               WRITE GLS-POS-RPT  FROM  DETAIL-HDR1B
                      AFTER  ADVANCING 1 LINE
               WRITE GLS-POS-RPT  FROM  DETAIL-HDR2
                      AFTER  ADVANCING 1 LINE
               WRITE GLS-POS-RPT  FROM  DETAIL-HDR4
                      AFTER  ADVANCING 5 LINES.
           IF  GLS-1825011300-IND   =   'N'
               MOVE '1209000'                TO  WS-MSA-ACCT
               MOVE '1825011300-02'          TO  WS-FREEDOM-ACCT
               MOVE 'CID DEPARTMENT       '  TO  WS-DISTRIBUTION
               WRITE  GLS-CID-RPT  FROM  HDR-1
                       AFTER  ADVANCING PAGE
               WRITE  GLS-CID-RPT  FROM  HDR-2
                       AFTER  ADVANCING 1 LINE
               WRITE  GLS-CID-RPT  FROM  HDR-3
                       AFTER  ADVANCING 1 LINE
               WRITE  GLS-CID-RPT  FROM  BLANK-LINE
                       AFTER  ADVANCING 1 LINE
               WRITE  GLS-CID-RPT  FROM  HDR-4
                       AFTER  ADVANCING 1 LINE
               WRITE  GLS-CID-RPT  FROM  BLANK-LINE
                       AFTER  ADVANCING 2 LINES
               WRITE  GLS-CID-RPT  FROM  DETAIL-HDR1
                       AFTER  ADVANCING 1 LINE
               WRITE  GLS-CID-RPT  FROM  DETAIL-HDR1B
                       AFTER  ADVANCING 1 LINE
               WRITE  GLS-CID-RPT  FROM  DETAIL-HDR2
                       AFTER  ADVANCING 1 LINE
               WRITE  GLS-CID-RPT  FROM  DETAIL-HDR4
                       AFTER  ADVANCING 5 LINES
               MOVE 'CORPORATE FINANCE    '  TO  WS-DISTRIBUTION
               WRITE  GLS-CORP-FIN-RPT   FROM  HDR-1
                       AFTER  ADVANCING PAGE
               WRITE  GLS-CORP-FIN-RPT   FROM  HDR-2
                       AFTER  ADVANCING 1 LINE
               WRITE  GLS-CORP-FIN-RPT   FROM  HDR-3
                       AFTER  ADVANCING 1 LINE
               WRITE  GLS-CORP-FIN-RPT   FROM  BLANK-LINE
                       AFTER  ADVANCING 1 LINE
               WRITE  GLS-CORP-FIN-RPT   FROM  HDR-4
                       AFTER  ADVANCING 1 LINE
               WRITE  GLS-CORP-FIN-RPT   FROM  BLANK-LINE
                       AFTER  ADVANCING 2 LINES
               WRITE  GLS-CORP-FIN-RPT   FROM  DETAIL-HDR1
                       AFTER  ADVANCING 1 LINE
               WRITE  GLS-CORP-FIN-RPT   FROM  DETAIL-HDR1B
                       AFTER  ADVANCING 1 LINE
               WRITE  GLS-CORP-FIN-RPT   FROM  DETAIL-HDR2
                       AFTER  ADVANCING 1 LINE
               WRITE  GLS-CORP-FIN-RPT   FROM  DETAIL-HDR4
                       AFTER  ADVANCING 1 LINE.

      ***---------------------------------------------------------***
      ***--  PROCESS 9999-END.                                  --***
      ***--  1. CLOSE FILES.                                    --***
      ***--  2. DISPLAY VARIOUS RECORD COUNTS.                  --***
      ***---------------------------------------------------------***
       9999-END.

      *    DISPLAY '***  GLS-2719000300-IND = ' GLS-2719000300-IND.
      *    DISPLAY '***  GLS-2719000301-IND = ' GLS-2719000301-IND.
      *    DISPLAY '***  GLS-2719000302-IND = ' GLS-2719000302-IND.
      *    DISPLAY '***  GLS-2719000307-IND = ' GLS-2719000307-IND.
      *    DISPLAY '***  GLS-2719000311-IND = ' GLS-2719000311-IND.
      *    DISPLAY '***  GLS-2719000322-IND = ' GLS-2719000322-IND.
      *    DISPLAY '***  GLS-1825011300-IND = ' GLS-1825011300-IND.

           DISPLAY '*=====================================*' UPON SYSERR
           DISPLAY '* FNB199 - CID SPECIAL CLEARING ACCT =*' UPON SYSERR
           DISPLAY '*=====================================*' UPON SYSERR
           DISPLAY '      INPUT RECORDS READ: ' INPUT-COUNT  UPON SYSERR
           DISPLAY 'RECORDS W/INVALID ACCT #: ' ERROR-COUNT  UPON SYSERR
           DISPLAY '  OUTPUT RECORDS WRITTEN: ' OUTPUT-COUNT UPON SYSERR
           DISPLAY '*=====================================*' UPON SYSERR

           MOVE    '*=====================================*' TO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC
           MOVE    '* FNB199 - CID SPECIAL CLEARING ACCT =*' TO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC
           MOVE    '*=====================================*' TO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC
           MOVE SPACES TO LIST-REC
           STRING  '      INPUT RECORDS READ: ' INPUT-COUNT
             DELIMITED BY SIZE INTO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC
           MOVE SPACES TO LIST-REC
           STRING  'RECORDS W/INVALID ACCT #: ' ERROR-COUNT
             DELIMITED BY SIZE INTO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC
           MOVE SPACES TO LIST-REC
           STRING  '  OUTPUT RECORDS WRITTEN: ' OUTPUT-COUNT
             DELIMITED BY SIZE INTO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC
           MOVE    '*=====================================*' TO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC

           CLOSE GL-SUSPENSE-TRANS-FILE
                 FNB199-GLS-POS
                 FNB199-GLS-AGY-BENEFITS
                 FNB199-GLS-CORP-FINANCE
                 FNB199-GLS-AGY-ACCT
                 FNB199-GLS-CID.
082506     CLOSE PARM-FILE.
082506     CALL 'FNBLIST' USING 'C' LIST-REC.

