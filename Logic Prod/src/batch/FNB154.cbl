       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     FNB154.
       AUTHOR.         CSO.
       INSTALLATION.   CENTRAL STATES HEALTH & LIFE CO. OF OMAHA
       DATE-WRITTEN.   01-19-1998.
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      * PGM DESCRIPTION:                                                       :
      * THIS PGM WILL CONVERT DAILY FIRST NATIONAL BANK PAID                   :
      * CHECKS INTO JOURNAL TRANSACTIONS FOR INPUT TO THE "FREEDOM"            :
      * PERSONAL COMPUTER (PC) BASED GL ACCOUNTING SYSTEM.                     :
      * THIS PGM, FNB154, WILL CREATE 1 OUTPUT FILE AND 1 ERROR REPORT         :
      * USING 1 INPUT FILE.                                                    :
      * INPUT FILE IS.....: FN.IMPORT.FNB.CHECKS   SEQ FILE                    :
      *                     (DO NOT SORT THE INPUT FILE)                       :
      * 1ST OUTPUT FILE IS: FN.DD.CSO.PDCHCKS.FNB  SEQ FILE-CSO                :
      * 1ST OUTPUT REPORT : SENT TO THE PRINTER                                :
      * NOTE:                                                                  :
      * THIS PGM CREATES A INTERNAL TABLE SETUP TO HOLD 100 ENTRIES.           :
      * THIS TABLE WILL CONTAIN VARIOUS FNB-BANK-ACCT FROM THE INPUT.          :
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      *    DATE    BY  MODIFICATION                                            :
      * ========== === ========================================================:
      * 01/19/1998 BXH INITIAL IMPLEMENTATION                                  :
      * 02/12/1999 DAN CHANGED PRINT RECORD FROM 133 TO 132                    :
      * 09/12/2003 DJN CONVERT FROM MAINFRAME TO MICROFOCUS COBOL.             :
      * 12/17/2003 DJN CR2003112000002 NEW ACCOUNT NUMBER '1825093000' FOR     :
      *                SETTLEMENT CHECKS.                                      :
      * 08/31/2005 DJN CR2003112000002 REMOVE PROCESSING FOR ACCOUNT NUMBER    :
      *                '1825093000' FOR SETTLEMENT CHECKS.                     :
112105* 11/21/2005 AJR CR2005111400003 ADD CSI/DCC ACCOUNT NUMBER.             :
041806* 04/18/2006 AJR CHANGE BANK CODE FROM FNB TO DCC FOR CSI/DCC            :
052506* 05/25/2006 AJR CR2006052300001 ADD LPAC/DCC ACCOUNT NUMBER.            :
      * 07/28/2006 DJN DO NOT ADVANCE TO NEW PAGE ON FIRST PAGE OF REPORTS.    :
081706* 08/17/2006 AJR REMOVE FROM CLAIMS SYSTEM                               :
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      **** INPUT FILE
           SELECT CHECKS-FILE      ASSIGN       TO EXTERNAL SYS010
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS  IS IN-STATUS.

      **** OUTPUT FILE - CENTRAL STATES
           SELECT CS-FILE          ASSIGN       TO EXTERNAL SYS020
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS  IS CS-STATUS.

      **** OUTPUT ERROR REPORT
           SELECT RPT-ERR-FILE     ASSIGN       TO EXTERNAL SYS030
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS  IS ER-STATUS.

      **** OUTPUT REPORT
           SELECT RPT-FILE         ASSIGN       TO EXTERNAL SYS040
                                   ORGANIZATION IS LINE SEQUENTIAL
                                   FILE STATUS  IS RP-STATUS.

081706**** PARM FILE
081706     SELECT PARM-FILE        ASSIGN       TO EXTERNAL IPARM
081706                             ORGANIZATION IS LINE SEQUENTIAL
081706                             FILE STATUS  IS PARM-STATUS.
081706
       DATA DIVISION.
       FILE SECTION.

      **** INPUT SEQUENTIAL FIRST NATIONAL BANK CHECK FILE
       FD  CHECKS-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.

           COPY FNC011.

      **** OUTPUT SEQUENTIAL FILE (FREEDOM GENERAL LEDGER JOURNAL FILE)
       FD  CS-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CSO-RECORD          PIC X(51).

      **** OUTPUT REPORT
       FD  RPT-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
021299 01  RPT-REC             PIC X(132).

      **** OUTPUT ERROR REPORT
       FD  RPT-ERR-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
021299 01  RPT-ERR-REC         PIC X(132).

081706**** PARM FILE
081706 FD  PARM-FILE
081706     LABEL RECORDS ARE STANDARD
081706     RECORDING MODE IS F
081706     BLOCK CONTAINS 0 RECORDS.
081706 01  PARM.
081706     05  PARM-CYCLE-DATE PIC X(10) VALUE SPACES.
081706
       WORKING-STORAGE SECTION.

       01 RC                       PIC S9(9)   COMP-5 VALUE 0.
       01 LIST-REC                 PIC X(132).

       01  PGM-SWITCHES.
           05  ERROR-FND-SW    PIC X       VALUE 'N'.
               88  ERROR-FND               VALUE 'Y'.
           05  END-OF-FILE-SW  PIC X       VALUE 'N'.
               88  END-OF-FILE             VALUE 'Y'.
           05  END-OF-TABLE-SW PIC X       VALUE 'N'.
               88  END-OF-TABLE            VALUE 'Y'.

       01  FS-FILE-STATUS.
           05  IN-STATUS       PIC XX      VALUE SPACES.
           05  CS-STATUS       PIC XX      VALUE SPACES.
           05  ER-STATUS       PIC XX      VALUE SPACES.
           05  RP-STATUS       PIC XX      VALUE SPACES.
081706     05  PARM-STATUS     PIC XX      VALUE SPACES.

       01  WS-AA-TOTALS.
           05  WS-CS-TOT       PIC 9(09)V99  VALUE 0.
           05  WS-ACCT-TOT     PIC 9(09)V99  VALUE 0.
           05  WS-BATCH-TOT    PIC 9(12)V99  VALUE 0.

       01  WS-ACCOUNTS.
           05  WS-CS-ACCOUNT   PIC X(10).
           05  PRV-FNB-ACCT-NO PIC X(10).

       01  ACCT-TABLE.
           05  ACCT-NDX        PIC 9(02)      COMP-3  VALUE 0.
           05  ACCT-TABLE-ENTRIES OCCURS 100 TIMES.
               10  ACCT-NO     PIC X(10)      VALUE   SPACES.
               10  ACCT-TOTAL  PIC S9(09)V99  COMP-3  VALUE +0.
               10  ACCT-CNT    PIC S9(04)     COMP-3  VALUE +0.

       01  CNT-COUNTERS.
           05  CNT-RECS-READ   PIC S9(06)  COMP-3  VALUE +0.
           05  CNT-GRP-TOT     PIC S9(04)  COMP-3  VALUE +0.
           05  CNT-CS-RECS     PIC S9(04)  COMP-3  VALUE +0.
           05  CNT-RECS-PROC   PIC S9(04)  COMP-3  VALUE +0.
           05  CNT-CS-RECS-WTN PIC S9(04)  COMP-3  VALUE +0.
           05  CNT-OTHER-RECS  PIC S9(04)  COMP-3  VALUE +0.
           05  CNT-FNB-TOT-REC PIC S9(04)  COMP-3  VALUE +0.
           05  CNT-ERR-RPT-PG  PIC S9(04)  COMP-3  VALUE +0.
           05  CNT-RPT-PG      PIC S9(04)  COMP-3  VALUE +0.
           05  CNT-ERR-RPT-LN  PIC S9(04)  COMP-3  VALUE +0.
           05  CNT-RPT-LN      PIC S9(04)  COMP-3  VALUE +0.
           05  CNT-ERRORS      PIC S9(04)  COMP-3  VALUE +0.

       01  DSP-COUNTERS.
           05  DSP-RECS-READ   PIC Z,ZZZ,ZZ9.

       01  ERR-MESSAGES.
           05  ERR-CODE        PIC XX      VALUE SPACES.
           05  ERR-MSG-1       PIC X(50)   VALUE SPACES.
           05  ERR-MSG-2       PIC X(50)   VALUE
              'ERROR WRITING THE ABOVE CSO  CHECK RECORD.        '.

       01  WS-DATE-CCYY.
           05  WS-DATE-CC      PIC 99.
           05  WS-DATE-YY      PIC 99.

       01  SYSIN-DATE          PIC X(10).

041806 01  WS-BANK-CODE        PIC X(3) VALUE 'FNB'.

      * GET THE BANK RECONCILIATION COPYBOOK.
           COPY FNC002.

      ******************************************************************
      * REPORT HEADING LAYOUT DEFINITION
      ******************************************************************
       01  HDG-1.
           05  FILLER          PIC X       VALUE SPACES.
           05  FILLER          PIC X(10)   VALUE 'RUN DATE: '.
           05  HDG-1-DATE      PIC X(10)   VALUE SPACES.
           05  FILLER          PIC X(25)   VALUE SPACES.
           05  FILLER          PIC X(42)   VALUE
              'CENTRAL STATES HEALTH & LIFE INSURANCE CO'.
           05  FILLER          PIC X(36)   VALUE SPACES.
           05  FILLER          PIC X(8)    VALUE 'FNB154'.
           05  FILLER          PIC X       VALUE SPACES.

       01  HDG-2.
           05  FILLER          PIC X       VALUE SPACES.
           05  FILLER          PIC X(40)   VALUE SPACES.
           05  FILLER          PIC X(48)   VALUE
              ' FIRST NATIONAL BANK PAID CHECKS ERROR REPORT'.
           05  FILLER          PIC X(35)   VALUE SPACES.
           05  FILLER          PIC X(05)   VALUE 'PAGE '.
           05  HDG-2-PAGE      PIC ZZZ9.

       01  HDG-3.
           05  FILLER          PIC X       VALUE SPACES.
           05  FILLER          PIC X(55)   VALUE SPACES.
           05  FILLER          PIC X(11)   VALUE 'DATE CARD: '.
           05  HDG-3-DATE      PIC X(10)   VALUE SPACES.
           05  FILLER          PIC X(55)   VALUE SPACES.

       01  HDG-4.
           05  FILLER          PIC X       VALUE SPACES.
           05  FILLER          PIC X(132)  VALUE
               'REC NUM  ERROR MESSAGE'.

       01  HDG-5.
           05  FILLER           PIC X      VALUE SPACES.
           05  FILLER           PIC X(07)  VALUE ALL '-'.
           05  FILLER           PIC X(02)  VALUE SPACES.
           05  FILLER           PIC X(123) VALUE ALL '-'.

      ******************************************************************
      * ERROR REPORT DETAIL LINE DEFINITION
      ******************************************************************
       01  DL1-DETAIL-LINE.
           05  FILLER           PIC X(01)  VALUE SPACES.
           05  DL1-CNT          PIC ZZZ,ZZZ.
           05  FILLER           PIC X(02)  VALUE SPACES.
           05  DL1-ERR-MSG      PIC X(124) VALUE SPACES.

      ******************************************************************
      * REPORT HEADING LAYOUT DEFINITION
      ******************************************************************
       01  HDG2-1.
           05  FILLER          PIC X       VALUE SPACES.
           05  FILLER          PIC X(10)   VALUE 'RUN DATE: '.
           05  HDG2-1-DATE     PIC X(10)   VALUE SPACES.
           05  FILLER          PIC X(25)   VALUE SPACES.
           05  FILLER          PIC X(42)   VALUE
              'CENTRAL STATES HEALTH & LIFE INSURANCE CO'.
           05  FILLER          PIC X(36)   VALUE SPACES.
           05  FILLER          PIC X(8)    VALUE 'FNB154'.
           05  FILLER          PIC X       VALUE SPACES.

       01  HDG2-2.
           05  FILLER          PIC X       VALUE SPACES.
           05  FILLER          PIC X(40)   VALUE SPACES.
           05  FILLER          PIC X(48)   VALUE
              '           RECONCILIATION FILE DOWNLOAD'.
           05  FILLER          PIC X(35)   VALUE SPACES.
           05  FILLER          PIC X(05)   VALUE 'PAGE '.
           05  HDG2-2-PAGE     PIC ZZZ9.

       01  HDG2-3.
           05  FILLER          PIC X       VALUE SPACES.
           05  FILLER          PIC X(55)   VALUE SPACES.
           05  FILLER          PIC X(11)   VALUE 'DATE CARD: '.
           05  HDG2-3-DATE     PIC X(10)   VALUE SPACES.
           05  FILLER          PIC X(55)   VALUE SPACES.

       01  HDG2-4.
           05  FILLER          PIC X       VALUE  SPACES.
           05  FILLER          PIC X(12)   VALUE 'ACCOUNT NO. '.
           05  FILLER          PIC X(04)   VALUE  SPACES.
           05  FILLER          PIC X(12)   VALUE 'AMOUNT      '.
           05  FILLER          PIC X(02)   VALUE  SPACES.
           05  FILLER          PIC X(12)   VALUE 'COUNT       '.

       01  HDG2-5.
           05  FILLER          PIC X       VALUE SPACES.
           05  FILLER          PIC X(12)   VALUE ALL '-'.
           05  FILLER          PIC X(04)   VALUE SPACE.
           05  FILLER          PIC X(12)   VALUE ALL '-'.
           05  FILLER          PIC X(02)   VALUE SPACE.
           05  FILLER          PIC X(12)   VALUE ALL '-'.

      ******************************************************************
      * REPORT DETAIL LINE DEFINITION
      ******************************************************************
       01  DL2-DETAIL-LINE.
           05  FILLER          PIC X       VALUE SPACES.
           05  DL2-ACCT-NO     PIC X(10)   VALUE SPACES.
           05  FILLER          PIC XX      VALUE SPACES.
           05  DL2-TOT         PIC Z,ZZZ,ZZZ,ZZZ.99.
           05  FILLER          PIC XX      VALUE SPACES.
           05  DL2-COUNT       PIC Z,ZZZ,ZZ9.

       01  DL2A-DETAIL-LINE.
           05  FILLER          PIC X       VALUE SPACES.
           05  DL2A-MESSAGE    PIC X(12)   VALUE
               'BATCH TOTAL '.
           05  DL2A-BATCH-TOT  PIC Z,ZZZ,ZZZ,ZZZ.99.
           05  FILLER          PIC XX      VALUE SPACES.
           05  DL2A-BATCH-CNT  PIC Z,ZZZ,ZZ9.

       01  DL3A-DETAIL-LINE.
           05  FILLER          PIC X       VALUE SPACES.
           05  DL3A-MESSAGE    PIC X(50)   VALUE SPACES.
           05  FILLER          PIC X       VALUE SPACES.
           05  DL3A-CNT        PIC Z,ZZZ,ZZ9.

081706*    EXEC SQL INCLUDE ISTDWORK.INC END-EXEC.
081706
081706 01  WS-ACCEPT-DATE.
081706     05  WS-AC-YY        PIC 99.
081706     05  WS-AC-MM        PIC 99.
081706     05  WS-AC-DD        PIC 99.
081706
081706 01  WS-RUN-DATE.
081706     05  WS-RN-MM        PIC 99.
081706     05  FILLER          PIC X(1)    VALUE '/'.
081706     05  WS-RN-DD        PIC 99.
081706     05  FILLER          PIC X(3)    VALUE '/20'.
081706     05  WS-RN-YY        PIC 99.
081706
       PROCEDURE DIVISION.

           PERFORM 1000-INITIALIZE

           PERFORM 2000-MAIN-PROCESS UNTIL END-OF-FILE.

           MOVE +1 TO ACCT-NDX.
           INITIALIZE DL2-DETAIL-LINE
                      DL2A-DETAIL-LINE.
           MOVE ' PGM FNB154 TOTAL AMOUNTS AND COUNTS: ' TO RPT-REC.
           WRITE RPT-REC AFTER ADVANCING 4 LINES.
           PERFORM 8400-PRINT-TABLE UNTIL END-OF-TABLE.

           PERFORM 3000-END-PROCESS

081706*    GOBACK GIVING RC.
081706     STOP RUN GIVING RC.

      *-----------------------------------------------------------------
      * PROCESS THE INPUT SYSIN DATE CARD, GET THE SYSTEM DATE, OPEN
      * FILES AND READ A RECORD.
      *-----------------------------------------------------------------
       1000-INITIALIZE.

081706*    CALL IGETPARM USING IG-P1
081706*    MOVE IG-P1 TO SYSIN-DATE

081706*    MOVE SYSIN-DATE   TO IV-P2
081706*    MOVE 'MM*DD*YEAR' TO IV-P3
081706*    INITIALIZE IV-P4
081706*    CALL IVERDATE USING IV-P1 IV-P2 IV-P3 IV-P4
081706*    IF NOT IV-P1 = 'Y'
081706     OPEN INPUT PARM-FILE.
081706     IF PARM-STATUS NOT = '00'
081706       MOVE   PARM-STATUS                TO  ERR-CODE
081706       MOVE  'ERROR OPENING PARM FILE'   TO  ERR-MSG-1
081706       PERFORM 9999-ABEND-PGM
081706     END-IF
081706     READ PARM-FILE.
081706     IF PARM-CYCLE-DATE NOT GREATER THAN SPACES
             DISPLAY 'INVALID INPUT DATE FOR PGM FNB154, DATE ='
                     SYSIN-DATE UPON SYSERR
             MOVE SPACES TO LIST-REC
             STRING  'INVALID INPUT DATE FOR PGM FNB154, DATE ='
                     SYSIN-DATE DELIMITED BY SIZE INTO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
             DISPLAY 'DATE FORMAT SHOULD BE: MM/DD/YEAR' UPON SYSERR
             MOVE    'DATE FORMAT SHOULD BE: MM/DD/YEAR' TO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
             DISPLAY 'PROGRAM FNB154 WILL END WITH CC 0016' UPON SYSERR
             MOVE    'PROGRAM FNB154 WILL END WITH CC 0016' TO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
             MOVE 16 TO RC
081706*      GOBACK GIVING RC
081706       STOP RUN GIVING RC
           END-IF

081706     MOVE PARM-CYCLE-DATE TO SYSIN-DATE.
081706     CLOSE PARM-FILE.
           MOVE SYSIN-DATE TO HDG-3-DATE
                              HDG2-3-DATE.

      **** GET THE SYSTEM DATE AND PUT IT IN HEADINGS & WORK AREA.
081706*    MOVE "MM/DD/YEAR" TO ID-P3
081706*    INITIALIZE  ID-P2 ID-P4 ID-P5 ID-P6 ID-P7 ID-P8
081706*    CALL IDATE USING ID-P1 ID-P2 ID-P3 ID-P4
081706*                     ID-P5 ID-P6 ID-P7 ID-P8
081706     ACCEPT WS-ACCEPT-DATE FROM DATE.
081706     MOVE WS-AC-MM    TO WS-RN-MM.
081706     MOVE WS-AC-DD    TO WS-RN-DD.
081706     MOVE WS-AC-YY    TO WS-RN-YY.
081706     MOVE WS-RUN-DATE TO HDG-1-DATE
081706*    MOVE ID-P1(1:10) TO HDG-1-DATE
                               HDG2-1-DATE.

      **** PREPARE FILES FOR PROCESSING.
      **** IF AN ERROR OCCURS WHILE OPENING FILES ABORT PGM!
           OPEN INPUT CHECKS-FILE.
           IF IN-STATUS NOT = '00'
             MOVE   IN-STATUS                 TO ERR-CODE
             MOVE  'ERROR OPENING INPUT FILE' TO ERR-MSG-1
             PERFORM 9999-ABEND-PGM
           END-IF.

           OPEN OUTPUT CS-FILE, RPT-ERR-FILE, RPT-FILE.
           IF CS-STATUS NOT = '00'
             MOVE   CS-STATUS                     TO ERR-CODE
             MOVE  'ERROR OPENING CK OUTPUT FILE' TO ERR-MSG-1
             PERFORM 9999-ABEND-PGM
           END-IF.

           IF ER-STATUS NOT = '00'
             MOVE  ER-STATUS                            TO ERR-CODE
             MOVE 'ERROR OPENING ERROR RPT OUTPUT FILE' TO ERR-MSG-1
             PERFORM 9999-ABEND-PGM
           END-IF.

           IF RP-STATUS NOT = '00'
             MOVE  RP-STATUS                         TO ERR-CODE
             MOVE 'ERROR OPENING REPORT OUTPUT FILE' TO ERR-MSG-1
             PERFORM 9999-ABEND-PGM
           END-IF.

           PERFORM 8800-WRITE-ERR-HDGS

           PERFORM 8900-WRITE-HDGS
           MOVE ' FNB TOTAL AMOUNTS AND COUNTS: ' TO RPT-REC.
           WRITE RPT-REC AFTER ADVANCING 2 LINES.

           PERFORM 8000-READ-INPUT

           MOVE FNB-BANK-ACCT TO PRV-FNB-ACCT-NO.

      *-----------------------------------------------------------------
      * MAIN LOGIC OF PGM.
      * PROCESS RECORDS WHERE ACCOUNT NUMBER:
      * 1323808970 CSO ACCOUNT
      *-----------------------------------------------------------------
       2000-MAIN-PROCESS.

           PERFORM 2100-PROC-SPEC-ACCTS

           IF FNB-BANK-ACCT = '9999999999'
             PERFORM 8300-LOAD-TABLE
           ELSE
             ADD FNB-AMOUNT TO WS-ACCT-TOT
             ADD +1         TO CNT-GRP-TOT
           END-IF.

           MOVE FNB-BANK-ACCT TO PRV-FNB-ACCT-NO.

           PERFORM 8000-READ-INPUT.

      *-----------------------------------------------------------------
      * END PGM FNB154 PROCESSING.
      *-----------------------------------------------------------------
       3000-END-PROCESS.

           IF ERROR-FND
             MOVE SPACES  TO  RPT-ERR-REC
             WRITE RPT-ERR-REC
             MOVE CNT-ERRORS              TO DL1-CNT
             MOVE 'ERRORS FOUND IN DATA!' TO DL1-ERR-MSG
           END-IF.

           IF CNT-RECS-READ = 0
             MOVE ' ERROR! NO DATA RECORDS FOUND FOR THIS RUN.'
               TO  DL1-DETAIL-LINE
             WRITE RPT-ERR-REC FROM DL1-DETAIL-LINE
           END-IF.

           IF NOT ERROR-FND AND CNT-RECS-READ > 0
             MOVE ' NO DATA ERRORS FOUND FOR THIS RUN.'
               TO  DL1-DETAIL-LINE
             WRITE RPT-ERR-REC FROM DL1-DETAIL-LINE
           END-IF.

           MOVE SPACES        TO RPT-REC.
           MOVE WS-BATCH-TOT  TO DL2A-BATCH-TOT.
           MOVE CNT-RECS-PROC TO DL2A-BATCH-CNT.
           WRITE RPT-REC    FROM DL2A-DETAIL-LINE
             AFTER ADVANCING 3 LINES.

           PERFORM 9100-WRITE-STATS

           CLOSE CHECKS-FILE, CS-FILE, RPT-ERR-FILE, RPT-FILE.

      *-----------------------------------------------------------------
      * PROCESS SPECIFIC ACCOUNTS
      *-----------------------------------------------------------------
       2100-PROC-SPEC-ACCTS.

           EVALUATE FNB-BANK-ACCT
      *        CSO ACCOUNT
             WHEN '1323808970'
041806         MOVE 'FNB' TO WS-BANK-CODE
               ADD +1 TO CNT-CS-RECS
               PERFORM 6000-PROCESS-CS-REC

112105*        CSI/DCC ACCOUNT
112105       WHEN '1019220670'
041806         MOVE 'DCC' TO WS-BANK-CODE
112105         ADD +1 TO CNT-CS-RECS
112105         PERFORM 6000-PROCESS-CS-REC
112105
052506*        LPAC/DCC ACCOUNT
052506       WHEN '1020464470'
052506         MOVE 'DCL' TO WS-BANK-CODE
052506         ADD +1 TO CNT-CS-RECS
052506         PERFORM 6000-PROCESS-CS-REC
052506
      *        FNB TOTAL RECORD
             WHEN '9999999999'
               PERFORM 8200-WRITE-FNB-TOT-REC
               ADD +1 TO CNT-FNB-TOT-REC

             WHEN OTHER
               ADD +1 TO CNT-OTHER-RECS
           END-EVALUATE.

      *-----------------------------------------------------------------
      * CREATE CSO ACCOUNT RECORDS AND WRITE TO SEQUENTIAL WORK OUTPUT
      * FILE: FN.DD.CSO.PDCHECKS.FNB
      * 1323808970 CSO ACCOUNT
      *-----------------------------------------------------------------
       6000-PROCESS-CS-REC.

           MOVE SPACES TO BANK-RECONCILIATION-RECORD
041806*    MOVE 'FNB'          TO  BR-BANK-CODE
041806     MOVE WS-BANK-CODE   TO  BR-BANK-CODE
           MOVE FNB-CHECK-NO   TO  BR-CHECK-NO
           MOVE SPACES         TO  BR-CHECK-DATE
           MOVE FNB-AMOUNT     TO  BR-CHECK-AMOUNT
           MOVE FNB-PAID-MM    TO  BR-PAID-MO
           MOVE FNB-PAID-DD    TO  BR-PAID-DAY
           MOVE '20'           TO  WS-DATE-CC
           MOVE FNB-PAID-YY    TO  WS-DATE-YY
           MOVE WS-DATE-CCYY   TO  BR-PAID-YR

           WRITE CSO-RECORD FROM BANK-RECONCILIATION-RECORD
           IF CS-STATUS NOT = '00'
             PERFORM 6100-CS-WRITE-ERR
           END-IF

           ADD +1  TO  CNT-CS-RECS-WTN.

      *-----------------------------------------------------------------
      * PARAGRAPH TO REPORT ERROR WRITING PAID CHECKS RECORD
      *-----------------------------------------------------------------
       6100-CS-WRITE-ERR.

           MOVE 'Y'               TO  ERROR-FND-SW
           MOVE FNB-CHECK-RECORD  TO  DL1-ERR-MSG
           PERFORM 8100-WRITE-ERR-DETAIL

           ADD +1              TO  CNT-ERRORS
           MOVE CNT-RECS-READ  TO  DL1-CNT
           MOVE ERR-MSG-2      TO  DL1-ERR-MSG
           PERFORM 8100-WRITE-ERR-DETAIL.

      *-----------------------------------------------------------------
      * READ INPUT FILE: FN.IMPORT.FNB.DRAFTS(0)
      *-----------------------------------------------------------------
       8000-READ-INPUT.

           INITIALIZE FNB-CHECK-RECORD
           READ CHECKS-FILE
             AT END
               MOVE 'Y' TO END-OF-FILE-SW
               EXIT PARAGRAPH
           END-READ

           ADD +1 TO CNT-RECS-READ.

      *-----------------------------------------------------------------
      * WRITE ERROR DETAIL LINE
      *-----------------------------------------------------------------
       8100-WRITE-ERR-DETAIL.

           IF CNT-ERR-RPT-LN > +60
             PERFORM 8800-WRITE-ERR-HDGS
           ELSE
             ADD +1 TO CNT-ERR-RPT-LN
             WRITE RPT-ERR-REC FROM DL1-DETAIL-LINE
             INITIALIZE DL1-DETAIL-LINE
           END-IF.

      *-----------------------------------------------------------------
      * WRITE FIRST NATIONAL BANK TOTAL RECORDS TO REPORT 1ST.
      * THE FNB-CHECK-NO HERE IS REALLY THE TOTAL COUNT OF RECORDS FOR
      * THE PREVIOUS ACCOUNT NUMBER RECORDS.
      * IT IS STORED ON THE RECORD UNDER ACCT 9999999999.
      *-----------------------------------------------------------------
       8200-WRITE-FNB-TOT-REC.

           IF CNT-RPT-LN > +60
             PERFORM 8900-WRITE-HDGS
           ELSE
             MOVE  PRV-FNB-ACCT-NO TO DL2-ACCT-NO
             MOVE  FNB-AMOUNT      TO DL2-TOT
             MOVE  FNB-CHECK-NO    TO DL2-COUNT
             WRITE RPT-REC       FROM DL2-DETAIL-LINE
             ADD   +1              TO CNT-RPT-LN
             INITIALIZE RPT-REC
           END-IF.

      *-----------------------------------------------------------------
      * LOAD TOTALS INTO A TABLE FOR SPECIFIC GROUP OF RECORDS.
      *-----------------------------------------------------------------
       8300-LOAD-TABLE.

           ADD  +1              TO ACCT-NDX
           MOVE PRV-FNB-ACCT-NO TO ACCT-NO    (ACCT-NDX)
           MOVE WS-ACCT-TOT     TO ACCT-TOTAL (ACCT-NDX)
           MOVE CNT-GRP-TOT     TO ACCT-CNT   (ACCT-NDX)

           INITIALIZE WS-ACCT-TOT
                      CNT-GRP-TOT.

      *-----------------------------------------------------------------
      * PRINT THE TABLE OF ACCOUNT INFORMATION
      *-----------------------------------------------------------------
       8400-PRINT-TABLE.

           IF CNT-RPT-LN > +60
             PERFORM 8900-WRITE-HDGS
           END-IF

           IF ACCT-NO (ACCT-NDX) EQUAL SPACES
             MOVE 'Y' TO END-OF-TABLE-SW
           ELSE
             MOVE  ACCT-NO    (ACCT-NDX)  TO DL2-ACCT-NO
             MOVE  ACCT-TOTAL (ACCT-NDX)  TO DL2-TOT
             MOVE  ACCT-CNT   (ACCT-NDX)  TO DL2-COUNT
             WRITE RPT-REC              FROM DL2-DETAIL-LINE
             ADD   +1                     TO CNT-RPT-LN
             ADD   ACCT-TOTAL (ACCT-NDX)  TO WS-BATCH-TOT
             ADD   ACCT-CNT   (ACCT-NDX)  TO CNT-RECS-PROC
             ADD   +1  TO  ACCT-NDX
             INITIALIZE RPT-REC
           END-IF.

      *-----------------------------------------------------------------
      * WRITE ERROR REPORT HEADINGS
      *-----------------------------------------------------------------
       8800-WRITE-ERR-HDGS.

           ADD +1              TO CNT-ERR-RPT-PG
           MOVE CNT-ERR-RPT-PG TO HDG-2-PAGE

           IF CNT-ERR-RPT-PG = 1
             WRITE RPT-ERR-REC FROM HDG-1 AFTER ADVANCING 0 LINES
           ELSE
             WRITE RPT-ERR-REC FROM HDG-1 AFTER ADVANCING PAGE
           END-IF
           WRITE RPT-ERR-REC FROM HDG-2 AFTER ADVANCING 1 LINES
           WRITE RPT-ERR-REC FROM HDG-3 AFTER ADVANCING 1 LINES
           WRITE RPT-ERR-REC FROM HDG-4 AFTER ADVANCING 2 LINE
           WRITE RPT-ERR-REC FROM HDG-5 AFTER ADVANCING 1 LINE

           MOVE 6  TO  CNT-ERR-RPT-LN.

      *-----------------------------------------------------------------
      * WRITE HEADINGS FOR RECONCILIATION FILE DOWNLOAD RPT
      *-----------------------------------------------------------------
       8900-WRITE-HDGS.

           ADD +1          TO CNT-RPT-PG
           MOVE CNT-RPT-PG TO HDG2-2-PAGE

           IF CNT-RPT-PG = 1
             WRITE RPT-REC FROM HDG2-1 AFTER ADVANCING 0 LINES
           ELSE
             WRITE RPT-REC FROM HDG2-1 AFTER ADVANCING PAGE
           END-IF
           WRITE RPT-REC FROM HDG2-2 AFTER ADVANCING 1 LINES
           WRITE RPT-REC FROM HDG2-3 AFTER ADVANCING 1 LINES
           WRITE RPT-REC FROM HDG2-4 AFTER ADVANCING 2 LINE
           WRITE RPT-REC FROM HDG2-5 AFTER ADVANCING 1 LINE

           MOVE 6 TO CNT-RPT-LN.

      *-----------------------------------------------------------------
      * WRITE STATS FOR PGM FNB154 PROCESS
      *-----------------------------------------------------------------
       9100-WRITE-STATS.

           ADD +1          TO CNT-RPT-PG
           MOVE CNT-RPT-PG TO HDG2-2-PAGE

           IF CNT-RPT-PG = 1
             WRITE RPT-REC FROM HDG2-1 AFTER ADVANCING 0 LINES
           ELSE
             WRITE RPT-REC FROM HDG2-1 AFTER ADVANCING PAGE
           END-IF
           WRITE RPT-REC FROM HDG2-2 AFTER ADVANCING 1 LINES
           WRITE RPT-REC FROM HDG2-3 AFTER ADVANCING 1 LINES

           MOVE '*------------------------------------------------*'
             TO DL3A-MESSAGE
           WRITE RPT-REC FROM DL3A-DETAIL-LINE
             AFTER ADVANCING 2 LINES

           MOVE '               STATS FOR PGM FNB154               '
             TO DL3A-MESSAGE
           WRITE RPT-REC FROM DL3A-DETAIL-LINE AFTER ADVANCING 1 LINE
           MOVE SPACES TO RPT-REC
           WRITE RPT-REC

           MOVE 'TOTAL RECORDS READ...............................:'
             TO DL3A-MESSAGE
           MOVE CNT-RECS-READ TO DL3A-CNT
           WRITE RPT-REC FROM DL3A-DETAIL-LINE AFTER ADVANCING 1 LINE.
           MOVE SPACES TO RPT-REC
           WRITE RPT-REC

           MOVE 'CENTRAL STATES RECORDS READ......................:'
             TO DL3A-MESSAGE
           MOVE CNT-CS-RECS TO DL3A-CNT
           WRITE RPT-REC FROM DL3A-DETAIL-LINE AFTER ADVANCING 1 LINE
           MOVE SPACES TO RPT-REC
           WRITE RPT-REC

           MOVE 'OTHER ACCOUNT RECORDS READ.......................:'
             TO DL3A-MESSAGE
           MOVE CNT-OTHER-RECS TO DL3A-CNT
           WRITE RPT-REC FROM DL3A-DETAIL-LINE AFTER ADVANCING 1 LINE
           MOVE SPACES TO RPT-REC
           WRITE RPT-REC

           MOVE 'FNB RECORDS WITH TOTAL AMOUNT AND COUNTS READ....:'
             TO DL3A-MESSAGE
           MOVE CNT-FNB-TOT-REC TO DL3A-CNT
           WRITE RPT-REC FROM DL3A-DETAIL-LINE AFTER ADVANCING 1 LINE
           MOVE SPACES TO RPT-REC
           WRITE RPT-REC

           MOVE 'CENTRAL STATES RECORDS WRITTEN...................:'
             TO DL3A-MESSAGE
           MOVE CNT-CS-RECS-WTN TO DL3A-CNT
           WRITE RPT-REC FROM DL3A-DETAIL-LINE AFTER ADVANCING 1 LINE
           MOVE SPACES TO RPT-REC
           WRITE RPT-REC

           MOVE 'RECORDS WITH ERRORS FOUND IN THIS RUN............:'
             TO DL3A-MESSAGE
           MOVE CNT-ERRORS TO DL3A-CNT
           WRITE RPT-REC FROM DL3A-DETAIL-LINE AFTER ADVANCING 1 LINE
           MOVE SPACES TO RPT-REC.

      *-----------------------------------------------------------------
      * ABORT PGM IF ERROR OCURRS.
      *-----------------------------------------------------------------
       9999-ABEND-PGM.

           MOVE CNT-RECS-READ TO DSP-RECS-READ
           DISPLAY 'FILE STATUS......: ' ERR-CODE      UPON SYSERR
           DISPLAY 'ERROR MESSAGE....: ' ERR-MSG-1     UPON SYSERR
           DISPLAY 'PROCESSING RECORD: ' DSP-RECS-READ UPON SYSERR
           MOVE 16 TO RC
081706*    GOBACK GIVING RC.
081706     STOP RUN GIVING RC.
