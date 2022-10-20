       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     FNB153.
       AUTHOR.         CSO.
       INSTALLATION.   CENTRAL STATES HEALTH & LIFE CO. OF OMAHA
       DATE-WRITTEN.   01-19-1998.
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      * PGM DESCRIPTION:                                                       :
      * THIS PGM WILL CONVERT THE DAILY FIRST NATIONAL BAND PAID DRAFTS        :
      * INTO JOURNAL TRANSACTIONS FOR INPUT TO THE "FREEDOM" PERSONAL          :
      * COMPUTER (PC) BASED GL ACCOUNTING SYSTEM.                              :
      * THIS PGM, FNB153, WILL CREATE 1 OUTPUT FILE AND 1 ERROR REPORT         :
      * USING 1 INPUT FILE.                                                    :
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      *    DATE    BY  MODIFICATION                                            :
      * ========== === ========================================================:
      * 01/19/1998 BXH INITIAL IMPLEMENTATION                                  :
      * 01/06/1999 DAN SUSPENSE CODE WAS CHANGED FROM X(10) TO X(15)           :
      * 02/12/1999 DAN CHANGED PRINT RECORD FROM 133 TO 132                    :
DJN01 * 04/01/2000 DJN CR2000030100009 DRAFT NUMBER EXPANSION                  :
      * 09/12/2003 DJN CONVERT FROM MAINFRAME TO MICROFOCUS COBOL.             :
112105* 11/21/2005 AJR CR2005111400004 ADD CSI/DCC ACCOUNT NUMBER.             :
012406* 01/24/2006 AJR REMOVED CHECK NUMBER SHIFT FOR DRAFT EXPANSION.         :
051906* 05/19/2006 AJR CR2006051800001 ADD LPAC DCC ACCOUNT NUMBER.            :
      * 07/28/2006 DJN DO NOT ADVANCE TO NEW PAGE ON FIRST PAGE OF REPORTS.    :
081706* 08/17/2006 AJR REMOVE FROM CLAIMS SYSTEM.                              :
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANS-FILE   ASSIGN       TO EXTERNAL SYS010
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS  IS IN-STATUS.
           SELECT GL-FILE      ASSIGN       TO EXTERNAL SYS020
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS  IS GL-STATUS.
           SELECT RPT-ERR-FILE ASSIGN       TO EXTERNAL SYS040
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS  IS RP-STATUS.
081706**** PARM FILE
081706*     SELECT PARM-FILE        ASSIGN       TO EXTERNAL IPARM
081706*                             ORGANIZATION IS LINE SEQUENTIAL
081706*                             FILE STATUS  IS PARM-STATUS.
081706*

       DATA DIVISION.
       FILE SECTION.

       FD  TRANS-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY FNBOREC.

       FD  GL-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  GL-RECORD.
           COPY FNC019.

       FD  RPT-ERR-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-ERR-REC         PIC X(132).

081706**** PARM FILE
081706* FD  PARM-FILE
081706*     LABEL RECORDS ARE STANDARD
081706*     RECORDING MODE IS F
081706*     BLOCK CONTAINS 0 RECORDS.
081706* 01  PARM.
081706*     05  PARM-CYCLE-DATE PIC X(10) VALUE SPACES.
081706*
       WORKING-STORAGE SECTION.
       01 RC                       PIC S9(9)   COMP-5 VALUE 0.
       01 LIST-REC                 PIC X(132).

       01  PGM-SWITCHES.
           05  ERROR-FND-SW    PIC X       VALUE 'N'.
               88  ERROR-FND               VALUE 'Y'.
           05  END-OF-FILE-SW  PIC X       VALUE 'N'.
               88  END-OF-FILE             VALUE 'Y'.

       01  FS-FILE-STATUS.
           05  IN-STATUS       PIC XX      VALUE SPACES.
           05  GL-STATUS       PIC XX      VALUE SPACES.
           05  RP-STATUS       PIC XX      VALUE SPACES.
081706     05  PARM-STATUS     PIC XX      VALUE SPACES.

       01  CNT-COUNTERS.
           05  CNT-RD-RECS     PIC S9(06)  COMP-3  VALUE +0.
           05  CNT-DT-RECS     PIC S9(06)  COMP-3  VALUE +0.
           05  CNT-TR-RECS     PIC S9(06)  COMP-3  VALUE +0.
           05  CNT-CR-RECS     PIC S9(04)  COMP-3  VALUE +0.
           05  CNT-DB-RECS     PIC S9(04)  COMP-3  VALUE +0.
           05  CNT-RPT-PGS     PIC S9(04)  COMP-3  VALUE +0.
           05  CNT-RPT-LINES   PIC S9(04)  COMP-3  VALUE +0.
           05  CNT-ERRORS      PIC S9(04)  COMP-3  VALUE +0.

       01  DSP-COUNTERS.
           05  DSP-RD-RECS     PIC Z,ZZZ,ZZ9.
           05  DSP-DT-RECS     PIC Z,ZZZ,ZZ9.
           05  DSP-TR-RECS     PIC Z,ZZZ,ZZ9.
           05  DSP-CR-RECS     PIC Z,ZZZ,ZZ9.
           05  DSP-DB-RECS     PIC Z,ZZZ,ZZ9.
           05  DSP-PD-RECS     PIC Z,ZZZ,ZZ9.

       01  ERR-MESSAGES.
           05  ERR-CODE        PIC XX      VALUE SPACES.
           05  ERR-MSG-1       PIC X(50)   VALUE SPACES.
           05  ERR-MSG-2       PIC X(50)   VALUE
              'INVALID RECORD TYPE. ABOVE RECORD NOT PROCESSED.  '.
           05  ERR-MSG-3       PIC X(50)   VALUE
              'ERROR WRITING THE ABOVE GENERAL JOURNAL RECORD.   '.
           05  ERR-MSG-4       PIC X(50)   VALUE
              'ERROR WRITTING THE ABOVE PAID DRAFT RECORD.       '.

       01  WS-POSTING-DATE.
           05  WS-POST-MM      PIC XX.
           05  WS-POST-DD      PIC XX.
           05  WS-POST-YEAR    PIC XXXX.

       01  WS-JOURNAL-DATE.
           05  WS-JRNL-MM      PIC XX.
           05  WS-JRNL-DD      PIC XX.
           05  WS-JRNL-YEAR    PIC XXXX.

       01  SYSIN-DATE.
           05  SYSIN-MM        PIC XX.
           05  FILLER          PIC X.
           05  SYSIN-DD        PIC XX.
           05  FILLER          PIC X.
           05  SYSIN-YEAR      PIC XXXX.

       01  WS-PAID-DATE.
           05  WS-PD-STRING    PIC X(16)   VALUE
               'PAID DRAFTS OF: '.
           05  WS-PD-MM        PIC X(02).
           05  FILLER          PIC X(01)   VALUE '/'.
           05  WS-PD-DD        PIC X(02).
           05  FILLER          PIC X(01)   VALUE '/'.
           05  WS-PD-CC        PIC X(02).
           05  WS-PD-YY        PIC X(02).

       01  WS-TOTALS.
           05  WS-TOTAL-CR     PIC S9(10)V99
                               SIGN TRAILING SEPARATE VALUE +0.

112105 01  WS-PREV-ACCT-NUM    PIC X(10) VALUE SPACES.
112105 01  WS-CR-MAJ-ACCT      PIC X(10).
112105 01  WS-GL-MAJ-ACCT      PIC X(10).
112105
       01  CR-RECORD.
           COPY FNC019 REPLACING LEADING ==GL== BY ==CR==.

      ******************************************************************
      * REPORT HEADING LAYOUT DEFINITION
      ******************************************************************
       01  HDG-1.
           05  FILLER          PIC X       VALUE SPACES.
           05  FILLER          PIC X(10)   VALUE 'RUN DATE: '.
           05  HDG-DATE        PIC X(10)   VALUE SPACES.
           05  FILLER          PIC X(25)   VALUE SPACES.
           05  FILLER          PIC X(42)   VALUE
              'CENTRAL STATES HEALTH & LIFE INSURANCE CO'.
           05  FILLER          PIC X(36)   VALUE SPACES.
           05  FILLER          PIC X(8)    VALUE 'FNB153'.
           05  FILLER          PIC X       VALUE SPACES.

       01  HDG-2.
           05  FILLER          PIC X       VALUE SPACES.
           05  FILLER          PIC X(40)   VALUE SPACES.
           05  FILLER          PIC X(48)   VALUE
              '      FIRST NATIONAL BANK DRAFTS ERROR REPORT'.
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
      * REPORT DETAIL LINE DEFINITION
      ******************************************************************
       01  DL1-DETAIL-LINE.
           05  FILLER           PIC X(01)  VALUE SPACES.
           05  DL1-CNT          PIC ZZZ,ZZZ.
           05  FILLER           PIC X(02)  VALUE SPACES.
           05  DL1-ERR-MSG      PIC X(124) VALUE SPACES.

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
081706     05  FILLER          PIC X(1)    VALUE '/'.
081706     05  WS-RN-YYYY.
081706         10  WS-RN-CC    PIC 99      VALUE 20.
081706         10  WS-RN-YY    PIC 99.
081706
                                                                     
       LINKAGE SECTION.                                              
                                                                     
       01  PARM.                                                     
           05  PARM-LENGTH       PIC S9(4) BINARY.                      
           05  PARM-CYCLE-DATE   PIC X(10).                              
  
       PROCEDURE DIVISION USING PARM.

           PERFORM 1000-INITIALIZE

           PERFORM UNTIL EXIT
             READ TRANS-FILE
               AT END
                 EXIT PERFORM
               NOT AT END
                 IF FNB-REC-ID IS NOT NUMERIC
                   EXIT PERFORM CYCLE
                 END-IF
                 ADD +1 TO CNT-RD-RECS
                 PERFORM 2000-MAIN-PROCESS
             END-READ
           END-PERFORM

           PERFORM 6010-WRITE-CR-TOTAL

           PERFORM 3000-END-PROCESS

081706*    GOBACK GIVING RC.
081706     STOP RUN GIVING RC.

      *-----------------------------------------------------------------
      * PROCESS THE INPUT SYSIN DATE CARD, GET THE SYSTEM DATE, OPEN
      * FILES AND READ A RECORD.
      *-----------------------------------------------------------------
       1000-INITIALIZE.

081706     CALL 'FNBLIST' USING 'O' ' '

081706*    CALL IGETPARM USING IG-P1
081706*    MOVE IG-P1 TO SYSIN-DATE

081706*    MOVE SYSIN-DATE   TO IV-P2
081706*    MOVE 'MM*DD*YEAR' TO IV-P3
081706*    INITIALIZE IV-P4
081706*    CALL IVERDATE USING IV-P1 IV-P2 IV-P3 IV-P4
081706*    IF NOT IV-P1 = 'Y'
081706*     OPEN INPUT PARM-FILE.
081706*     IF PARM-STATUS NOT = '00'
081706*       MOVE   PARM-STATUS                TO  ERR-CODE
081706*       MOVE  'ERROR OPENING PARM FILE'   TO  ERR-MSG-1
081706*       PERFORM 9999-ABEND-PGM
081706*     END-IF
081706*     READ PARM-FILE.
           MOVE PARM-CYCLE-DATE TO SYSIN-DATE
081706     IF PARM-CYCLE-DATE NOT GREATER THAN SPACES
             DISPLAY 'INVALID INPUT DATE FOR PGM FNB153, DATE ='
                     SYSIN-DATE UPON SYSERR
             MOVE SPACES TO LIST-REC
             STRING  'INVALID INPUT DATE FOR PGM FNB153, DATE ='
                     SYSIN-DATE DELIMITED BY SIZE INTO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
             DISPLAY 'DATE FORMAT SHOULD BE: MM/DD/YEAR' UPON SYSERR
             MOVE    'DATE FORMAT SHOULD BE: MM/DD/YEAR' TO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
             DISPLAY 'PROGRAM FNB153 WILL END WITH CC 0016' UPON SYSERR
             MOVE    'PROGRAM FNB153 WILL END WITH CC 0016' TO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
             MOVE 16 TO RC
081706*      GOBACK GIVING RC
081706       STOP RUN GIVING RC
           END-IF

           MOVE SYSIN-DATE TO HDG-3-DATE
           MOVE SYSIN-MM   TO WS-POST-MM
           MOVE SYSIN-DD   TO WS-POST-DD
           MOVE SYSIN-YEAR TO WS-POST-YEAR

      **** GET THE SYSTEM DATE AND PUT IT IN HEADINGS & WORK AREA.
081706*    MOVE "MM/DD/YEAR" TO ID-P3
081706*    INITIALIZE  ID-P2 ID-P4 ID-P5 ID-P6 ID-P7 ID-P8
081706*    CALL IDATE USING ID-P1 ID-P2 ID-P3 ID-P4
081706*                     ID-P5 ID-P6 ID-P7 ID-P8
081706*    MOVE ID-P1(1:10) TO HDG-DATE
081706     ACCEPT WS-ACCEPT-DATE FROM DATE.
081706     MOVE WS-AC-MM    TO WS-RN-MM.
081706     MOVE WS-AC-DD    TO WS-RN-DD.
081706     MOVE WS-AC-YY    TO WS-RN-YY.
081706     MOVE WS-RUN-DATE TO HDG-DATE

081706*    MOVE ID-P1(1:2) TO WS-JRNL-MM
081706*    MOVE ID-P1(4:2) TO WS-JRNL-DD
081706*    MOVE ID-P1(7:4) TO WS-JRNL-YEAR
081706     MOVE WS-RN-MM   TO WS-JRNL-MM
081706     MOVE WS-RN-DD   TO WS-JRNL-DD
081706     MOVE WS-RN-YYYY TO WS-JRNL-YEAR

      **** PREPARE FILES FOR PROCESSING.
      **** IF AN ERROR OCCURS WHILE OPENING FILES ABORT PGM!
           OPEN INPUT TRANS-FILE
           IF IN-STATUS NOT = '00'
             MOVE   IN-STATUS                  TO  ERR-CODE
             MOVE  'ERROR OPENING INPUT FILE'  TO  ERR-MSG-1
             PERFORM 9999-ABEND-PGM
           END-IF

           OPEN OUTPUT GL-FILE, RPT-ERR-FILE.
           IF GL-STATUS NOT = '00'
             MOVE   GL-STATUS                      TO  ERR-CODE
             MOVE  'ERROR OPENING GJ OUTPUT FILE'  TO  ERR-MSG-1
             PERFORM 9999-ABEND-PGM
           END-IF

           IF RP-STATUS NOT = '00'
             MOVE   RP-STATUS                          TO  ERR-CODE
             MOVE  'ERROR OPENING REPORT OUTPUT FILE'  TO  ERR-MSG-1
             PERFORM 9999-ABEND-PGM
           END-IF

           PERFORM 8800-WRITE-HEADINGS
           MOVE 0 TO CR-AMOUNT.

      *-----------------------------------------------------------------
      * MAIN LOGIC OF PGM.
      * PROCESS ONLY DETAIL RECORDS '00'.  TRAILER RECORDS ARE '99'.
      *-----------------------------------------------------------------
       2000-MAIN-PROCESS.

112105     IF WS-PREV-ACCT-NUM EQUAL SPACES
112105         MOVE FNB-BANK-ACCT TO WS-PREV-ACCT-NUM
112105     END-IF
112105
112105     IF FNB-BANK-ACCT NOT EQUAL WS-PREV-ACCT-NUM
112105         PERFORM 6010-WRITE-CR-TOTAL
112105         MOVE ZERO TO CR-AMOUNT
112105         MOVE FNB-BANK-ACCT TO WS-PREV-ACCT-NUM
112105     END-IF
112105
           EVALUATE TRUE
             WHEN DETAIL-REC
               ADD +1 TO CNT-DT-RECS
112105         PERFORM 5000-SET-ACCT-NUMBER
               PERFORM 6000-PROCESS-CR-REC
               PERFORM 6100-PROCESS-DB-REC
             WHEN TRAILER-REC
               ADD +1 TO CNT-TR-RECS
             WHEN OTHER
               MOVE 'Y'            TO ERROR-FND-SW
               MOVE FNB-RECORD     TO DL1-ERR-MSG
               PERFORM 8100-WRITE-DETAIL
               ADD +1              TO CNT-ERRORS
               MOVE CNT-RD-RECS    TO DL1-CNT
               MOVE ERR-MSG-2      TO DL1-ERR-MSG
               PERFORM 8100-WRITE-DETAIL 2 TIMES
           END-EVALUATE.

      *-----------------------------------------------------------------
      * END PGM FNB153 PROCESSING.
      *-----------------------------------------------------------------
       3000-END-PROCESS.

           IF ERROR-FND
             MOVE  SPACES TO RPT-ERR-REC
             WRITE RPT-ERR-REC
             MOVE  CNT-ERRORS              TO  DL1-CNT
             MOVE 'ERRORS FOUND IN DATA!'  TO  DL1-ERR-MSG
             WRITE RPT-ERR-REC FROM DL1-DETAIL-LINE
           END-IF

           IF CNT-RD-RECS = 0
             MOVE 'ERROR! NO DATA RECORDS FOUND FOR THIS RUN.'
               TO  DL1-DETAIL-LINE
             WRITE RPT-ERR-REC FROM DL1-DETAIL-LINE
           END-IF

           IF NOT ERROR-FND AND CNT-RD-RECS > 0
             MOVE  SPACES TO RPT-ERR-REC
             WRITE RPT-ERR-REC
             MOVE 'NO DATA ERROR(S) FOUND FOR THIS RUN.'
               TO  DL1-DETAIL-LINE
             WRITE RPT-ERR-REC FROM DL1-DETAIL-LINE
           END-IF

           DISPLAY '*-------------------------------------*' UPON SYSERR
           DISPLAY '              STATS FOR PGM FNB153'      UPON SYSERR
           DISPLAY 'TOTALS: '                                UPON SYSERR
           MOVE     CNT-RD-RECS               TO DSP-RD-RECS
           DISPLAY 'RECORDS READ.............. ' DSP-RD-RECS UPON SYSERR
           MOVE     CNT-DT-RECS               TO DSP-DT-RECS
           DISPLAY 'DETAIL RECORDS READ....... ' DSP-DT-RECS UPON SYSERR
           MOVE     CNT-TR-RECS               TO DSP-TR-RECS
           DISPLAY 'TRAILER RECORDS READ...... ' DSP-TR-RECS UPON SYSERR
           DISPLAY ' '                                       UPON SYSERR
           MOVE     CNT-CR-RECS               TO DSP-CR-RECS
           DISPLAY 'FREEDOM CR RECORDS WRITTEN ' DSP-CR-RECS UPON SYSERR
           MOVE     CNT-DB-RECS               TO DSP-DB-RECS
           DISPLAY 'FREEDOM DB RECORDS WRITTEN ' DSP-DB-RECS UPON SYSERR
           DISPLAY '*-------------------------------------*' UPON SYSERR
           DISPLAY 'PGM FNB153 DONE PROCESSING'              UPON SYSERR

           CLOSE TRANS-FILE, GL-FILE, RPT-ERR-FILE.
081706*     CLOSE PARM-FILE.
081706     CALL 'FNBLIST' USING 'C' ' '.

112105*-----------------------------------------------------------------
112105* SET THE FREEDOM ACCT NUM BASED ON THE FNB-BANK-ACCT
112105*-----------------------------------------------------------------
112105 5000-SET-ACCT-NUMBER.
112105
112105     EVALUATE FNB-BANK-ACCT
112105****CSI DCC ACCOUNT
112105         WHEN '0009104318'
112105             MOVE '1108121220'    TO WS-CR-MAJ-ACCT
112105             MOVE '2725040150'    TO WS-GL-MAJ-ACCT
051906****LPAC DCC ACCOUNT
051906         WHEN '0009104680'
051906             MOVE '1108121270'    TO WS-CR-MAJ-ACCT
051906             MOVE '2725040350'    TO WS-GL-MAJ-ACCT
112105****DEFAULT TO CSO ACCOUNT
112105         WHEN OTHER
112105             MOVE '1108121040'    TO WS-CR-MAJ-ACCT
112105             MOVE '2724500150'    TO WS-GL-MAJ-ACCT
112105     END-EVALUATE.
112105
      *-----------------------------------------------------------------
      * CREATE CREDIT (ASSET) RECORDS AND WRITE TO SEQUENTIAL WORK
      *-----------------------------------------------------------------
       6000-PROCESS-CR-REC.

112105*    MOVE '1108121040'    TO CR-MAJ-ACCT
112105     MOVE WS-CR-MAJ-ACCT  TO CR-MAJ-ACCT
           MOVE '00'            TO CR-DIV
           MOVE '0000'          TO CR-CENTER
           MOVE '000000'        TO CR-PRODUCT
           MOVE '00'            TO CR-STATE
010699     MOVE WS-JOURNAL-DATE TO CR-JOURNAL-DATE
010699     MOVE WS-POSTING-DATE TO CR-POSTING-DATE
           MULTIPLY FNB-AMOUNT  BY -1 GIVING WS-TOTAL-CR
           ADD WS-TOTAL-CR      TO CR-AMOUNT
           MOVE 'PD-DRAFT'      TO CR-REFERENCE
           MOVE FNB-PAID-MM     TO WS-PD-MM
           MOVE FNB-PAID-DD     TO WS-PD-DD
           MOVE '20'            TO WS-PD-CC
           MOVE FNB-PAID-YY     TO WS-PD-YY
           MOVE WS-PAID-DATE    TO CR-DESCRIPTION
           MOVE 'PD-DRF'        TO CR-SOURCE
           MOVE SPACES          TO CR-SUNDRY
                                   CR-REVERSE-FLAG
                                   CR-SUSPENSE
           MOVE SPACES          TO CR-ALLOC-CODE.

      *-----------------------------------------------------------------
      * WRITE TOTAL CREDIT RECORD
      *-----------------------------------------------------------------
       6010-WRITE-CR-TOTAL.

           WRITE GL-RECORD FROM CR-RECORD
           IF GL-STATUS NOT = '00'
             PERFORM 6200-CRDB-WRITE-ERR
           END-IF
           ADD +1  TO  CNT-CR-RECS.

      *-----------------------------------------------------------------
      * CREATE DEBIT RECORDS
      *-----------------------------------------------------------------
       6100-PROCESS-DB-REC.

           INITIALIZE GL-RECORD.
112105*    MOVE '2724500150'    TO GL-MAJ-ACCT
112105     MOVE WS-GL-MAJ-ACCT  TO GL-MAJ-ACCT
           MOVE '00'            TO GL-DIV
           MOVE '0000'          TO GL-CENTER
           MOVE '000000'        TO GL-PRODUCT
           MOVE '00'            TO GL-STATE
010699     MOVE WS-JOURNAL-DATE TO GL-JOURNAL-DATE
010699     MOVE WS-POSTING-DATE TO GL-POSTING-DATE
           MULTIPLY FNB-AMOUNT  BY +1 GIVING GL-AMOUNT
           MOVE 'PD-DRAFT'      TO GL-REFERENCE
           MOVE FNB-PAID-MM     TO WS-PD-MM
           MOVE FNB-PAID-DD     TO WS-PD-DD
           MOVE '20'            TO WS-PD-CC
           MOVE FNB-PAID-YY     TO WS-PD-YY
           MOVE WS-PAID-DATE    TO GL-DESCRIPTION
           MOVE 'PD-DRF'        TO GL-SOURCE
           MOVE SPACES          TO GL-SUNDRY
                                   GL-REVERSE-FLAG
012406*    IF FNB-CHECK-NO(1:2) = '00'
012406*      MOVE FNB-CHECK-NO(3:8) TO GL-SUSPENSE
012406*    ELSE
DJN01        MOVE FNB-CHECK-NO      TO GL-SUSPENSE
012406*    END-IF
           MOVE SPACES          TO GL-ALLOC-CODE

           WRITE GL-RECORD
           IF GL-STATUS NOT = '00'
             PERFORM 6200-CRDB-WRITE-ERR
           END-IF
           ADD +1 TO CNT-DB-RECS.

      *-----------------------------------------------------------------
      * PARAGRAPH TO REPORT ERROR WRITING CR/DB TRANSACTION RECORDS
      *-----------------------------------------------------------------
       6200-CRDB-WRITE-ERR.

           MOVE 'Y'           TO ERROR-FND-SW
           MOVE FNB-RECORD    TO DL1-ERR-MSG
           PERFORM 8100-WRITE-DETAIL

           ADD +1           TO CNT-ERRORS
           MOVE CNT-RD-RECS TO DL1-CNT
           MOVE ERR-MSG-3   TO DL1-ERR-MSG
           PERFORM 8100-WRITE-DETAIL 2 TIMES.

       8100-WRITE-DETAIL.

           IF CNT-RPT-LINES > +60
             PERFORM 8800-WRITE-HEADINGS
           ELSE
             WRITE RPT-ERR-REC FROM DL1-DETAIL-LINE
             ADD +1 TO CNT-RPT-LINES
             INITIALIZE DL1-DETAIL-LINE
                        RPT-ERR-REC
           END-IF.

       8800-WRITE-HEADINGS.

           ADD +1           TO CNT-RPT-PGS
           MOVE CNT-RPT-PGS TO HDG-2-PAGE
           MOVE 7           TO CNT-RPT-LINES

           IF CNT-RPT-PGS = 1
             WRITE RPT-ERR-REC FROM HDG-1 AFTER ADVANCING 0 LINES
           ELSE
             WRITE RPT-ERR-REC FROM HDG-1 AFTER ADVANCING PAGE
           END-IF
           WRITE RPT-ERR-REC FROM HDG-2 AFTER ADVANCING 1 LINES
           WRITE RPT-ERR-REC FROM HDG-3 AFTER ADVANCING 1 LINES
           WRITE RPT-ERR-REC FROM HDG-4 AFTER ADVANCING 3 LINE
           WRITE RPT-ERR-REC FROM HDG-5 AFTER ADVANCING 1 LINE.

       9999-ABEND-PGM.

           MOVE CNT-RD-RECS TO DSP-RD-RECS
           DISPLAY 'FILE STATUS......: ' ERR-CODE    UPON SYSERR
           DISPLAY 'ERROR MESSAGE....: ' ERR-MSG-1   UPON SYSERR
           DISPLAY 'PROCESSING RECORD: ' DSP-RD-RECS UPON SYSERR
           MOVE 16 TO RC
081706*    GOBACK GIVING RC.
081706     STOP RUN GIVING RC.
