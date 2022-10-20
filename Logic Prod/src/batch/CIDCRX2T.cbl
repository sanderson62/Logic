       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDCRX2T.
       AUTHOR.     PABLO.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CERT-FILE-IN     ASSIGN TO CERTIN.

           SELECT ERACCT           ASSIGN TO ERACCTT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AM-CONTROL-PRIMARY
                                   FILE STATUS IS ERACCT-FILE-STATUS.

           SELECT ERMAIL           ASSIGN TO ERMAIL
                                   ACCESS IS DYNAMIC
                                   ORGANIZATION IS INDEXED              
                                   FILE STATUS IS ERMAIL-FILE-STATUS   
                                   RECORD KEY IS MA-CONTROL-PRIMARY.    

           SELECT EXTR-FILE-OUT1   ASSIGN TO EXTROT1
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE        ASSIGN TO SYS019.
                                                                        
           EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  CERT-FILE-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

                                       COPY ECSCRT01.

       FD  ERACCT.
                                       COPY ERCACCT.

       FD  ERMAIL.                                                      
                                       COPY ERCMAIL.


       FD  EXTR-FILE-OUT1
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  EXTR-FILE-OUT-HEAD1         PIC X(700).
       01  EXTR-FILE-OUT-REC1          PIC X(800).


       FD  DISK-DATE                                                    
                                   COPY ELCDTEFD.                       

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDCRX2  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-SAVE-ELMSTR-KEY      PIC X(12)  VALUE SPACES.
       77  WS-CLAIM-SW             PIC X   VALUE ' '.
           88  CLAIM-FOUND                 VALUE 'Y'.
       77  WS-CLAIM-OPEN-SW        PIC X   VALUE ' '.
           88  CLAIM-OPEN                  VALUE 'Y'.
       77  ERACCT-FILE-STATUS      PIC XX  VALUE LOW-VALUES.
       77  ELMSTR-FILE-STATUS      PIC XX  VALUE LOW-VALUES.
       77  ERMAIL-FILE-STATUS      PIC XX  VALUE LOW-VALUES.
       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-CERT               VALUE 'Y'.
       77  CRT-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  EXT-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  EXT-RECS-ACT            PIC 9(9) VALUE ZEROS.
031109 77  EXT-RECS-EXP            PIC 9(9) VALUE ZEROS.
       77  EXT-RECS-CAN            PIC 9(9) VALUE ZEROS.
       77  EXT-RECS-DTH            PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.
       77  PGM-SUB                 PIC S999    COMP    VALUE +020.
       77  WS-EXPIRE-DT            PIC 9(11)   VALUE ZEROS.
       77  WS-EFF-BIN              PIC XX    VALUE LOW-VALUES.
       77  WS-CAN-BIN              PIC XX    VALUE LOW-VALUES.
       77  WS-EXP-BIN              PIC XX    VALUE LOW-VALUES.
       77  WS-EARN-TERM            PIC 999   VALUE ZEROS.
       77  WS-WORK-TERM            PIC S9(3) VALUE +0 COMP-3.
       77  WS-REM-TERM             PIC S9(3) VALUE +0 COMP-3.
       77  WS-LAST-MONTH-DT        PIC 9(8)  VALUE ZEROS.
       77  WS-12-MONTHS-AGO        PIC 9(8)  VALUE ZEROS.
       77  WS-13-MONTHS-AGO        PIC 9(8)  VALUE ZEROS.
031109 77  WS-CERT-DT              PIC 9(8)  VALUE ZEROS.
031109 77  WS-RUN-DT               PIC 9(8)  VALUE ZEROS.
       77  WS-BAD-W-CLM            PIC 9(9)  VALUE ZEROS.
       77  WS-TYPE-EXTRACT         PIC X     VALUE SPACES.
           88  WS-EXPIRE                     VALUE 'E'.
           88  WS-ANNIV                      VALUE 'A'.
031109     88  WS-SPECIAL                    VALUE 'S'.
       77  WS-LF-STATUS                PIC X(10).
       77  WS-AH-STATUS                PIC X(10).
       77  WS-LF-TERM-DATE             PIC X(10).
       77  WS-AH-TERM-DATE             PIC X(10).
       01  WS-BUS-TYPE                 PIC 99.
           88  WS-AUTO                     VALUE 01.
           88  WS-BANK                     VALUE 02.
           88  WS-FIN-COMP                 VALUE 03.
           88  WS-CREDIT-UNION             VALUE 04.
           88  WS-SAVINGS-LOAN             VALUE 05.
           88  WS-MARINE                   VALUE 06.
           88  WS-TV                       VALUE 07.
           88  WS-MUSIC                    VALUE 08.
           88  WS-MOBILE-HOME              VALUE 09.
           88  WS-HOME-IMPR                VALUE 10.
           88  WS-SWIMMING-POOL            VALUE 11.
           88  WS-USED-CARS                VALUE 14.
           88  WS-MOTORCYCLE               VALUE 15.
           88  WS-RECREATION-VEH           VALUE 18.
           88  WS-FURNITURE                VALUE 19.
           88  WS-BANK-GROUP               VALUE 02 03 04 05.
           88  WS-AUTO-GROUP               VALUE 01 06 07 08 09
                                              10 11 14 15 18 19.
       01  WS-WORK-PHONE               PIC 9(11) COMP-3.
       01  FILLER REDEFINES WS-WORK-PHONE.
           05  WS-WORK-PHONE-ALPHA     PIC X(6).
       01  EXTR-DETAIL-RECORD.
           12  EX-BUS-TYPE             PIC X(5).
           12  EX-TAB52                PIC X.
           12  EX-STATUS               PIC X(10).
           12  EX-TAB53                PIC X.
           12  EX-ENTRY-DATE           PIC X(10).
           12  EX-TAB54                PIC X.
           12  EX-FNAME                PIC X(10).
           12  EX-TAB1                 PIC X.
           12  EX-MID                  PIC X.
           12  EX-TAB2                 PIC X.
           12  EX-LNAME                PIC X(15).
           12  EX-TAB3                 PIC X.
           12  EX-DOB                  PIC X(10).
           12  EX-TAB4                 PIC X.
           12  EX-SSN                  PIC X(10).
           12  EX-TAB5                 PIC X.
           12  EX-ADDRESS1             PIC X(30).
           12  EX-TAB6                 PIC X.
           12  EX-ADDRESS2             PIC X(30).
           12  EX-TAB7                 PIC X.
           12  EX-CITY                 PIC X(28).
           12  EX-TAB8                 PIC X.
           12  EX-STATE                PIC XX.
           12  EX-TAB9                 PIC X.
           12  EX-ZIP                  PIC X(11).
           12  EX-TAB10                PIC X.
           12  EX-TELE                 PIC X(11).
           12  EX-TAB11                PIC X.
           12  EX-LF-POL-TYPE          PIC X(09).
           12  EX-TAB12                PIC X.
           12  EX-AH-POL-TYPE          PIC X(10).
           12  EX-TAB13                PIC X.
           12  EX-CERT                 PIC X(11).
           12  EX-TAB14                PIC X.
           12  EX-EFF                  PIC X(10).
           12  EX-TAB15                PIC X.
           12  EX-LF-TERM              PIC 999.
           12  EX-TAB16                PIC X.
           12  EX-AH-TERM              PIC 999.
           12  EX-TAB17                PIC X.
           12  EX-LF-AMT               PIC ZZZ,999.99.
           12  EX-TAB18                PIC X.
           12  EX-AH-AMT               PIC ZZZ,999.99.
           12  EX-TAB51                PIC X.
           12  EX-LF-AMT-ALT           PIC ZZZZZZ9.99.
           12  EX-TAB19                PIC X.
           12  EX-LF-PREM-RATE         PIC Z9.9(5).
           12  EX-TAB20                PIC X.
           12  EX-LF-PREM-RATE-ALT     PIC Z9.9(5).
           12  EX-TAB21                PIC X.
           12  EX-AH-PREM-RATE         PIC Z9.9(5).
           12  EX-TAB22                PIC X.
           12  EX-LF-PREM              PIC ZZZZZZ9.99.
           12  EX-TAB23                PIC X.
           12  EX-LF-PREM-ALT          PIC ZZZZZZ9.99.
           12  EX-TAB24                PIC X.
           12  EX-AH-PREM              PIC ZZZZZZ9.99.
           12  EX-TAB25                PIC X.
           12  EX-LF-EXP               PIC X(10).
           12  EX-TAB26                PIC X.
           12  EX-AH-EXP               PIC X(10).
           12  EX-TAB27                PIC X.
           12  EX-TERM-DATE            PIC X(10).
           12  EX-TAB28                PIC X.
           12  EX-TERM-CAUSE           PIC X(10).
           12  EX-TAB29                PIC X.
           12  EX-LF-REFUND            PIC ZZZZZZ9.99.
           12  EX-TAB30                PIC X.
           12  EX-AH-REFUND            PIC ZZZZZZ9.99.
           12  EX-TAB31                PIC X.
           12  EX-LF-REF-DT            PIC X(10).
           12  EX-TAB32                PIC X.
           12  EX-LF-REF-EXT-DT        PIC X(10).
           12  EX-TAB33                PIC X.
           12  EX-AH-REF-DT            PIC X(10).
           12  EX-TAB34                PIC X.
           12  EX-AH-REF-EXT-DT        PIC X(10).
           12  EX-TAB35                PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-TAB36                PIC X.
           12  EX-ACCOUNT-NAME         PIC X(30).
           12  EX-TAB37                PIC X.
           12  EX-ACCT-PERSON          PIC X(30).
           12  EX-TAB38                PIC X.
           12  EX-ACCT-ADDR1           PIC X(30).
           12  EX-TAB39                PIC X.
           12  EX-ACCT-ADDR2           PIC X(30).
           12  EX-TAB40                PIC X.
           12  EX-ACCT-CITY            PIC X(28).
           12  EX-TAB41                PIC X.
           12  EX-ACCT-ST              PIC XX.
           12  EX-TAB42                PIC X.
           12  EX-ACCT-ZIP             PIC X(10).
           12  EX-TAB43                PIC X.
           12  EX-ACCT-TELE            PIC X(11).
           12  EX-TAB44                PIC X.
           12  EX-LEND-NAME            PIC X(30).
           12  EX-TAB45                PIC X.
           12  EX-LEND-ADDR1           PIC X(30).
           12  EX-TAB46                PIC X.
           12  EX-LEND-ADDR2           PIC X(30).
           12  EX-TAB47                PIC X.
           12  EX-LEND-CITY            PIC X(28).
           12  EX-TAB48                PIC X.
           12  EX-LEND-ST              PIC XX.
           12  EX-TAB49                PIC X.
           12  EX-LEND-ZIP             PIC X(10).
           12  EX-TAB50                PIC X.
           12  EX-END                  PIC X.

      ******************************************************************
       01  EXTR-DETAIL-HEADER.
           12  F                       PIC X(8) VALUE
                                               'BUS TYPE'.
           12  EX-HTAB52               PIC X.
           12  F                       PIC X(6) VALUE
                                               'STATUS'.
           12  EX-HTAB53               PIC X.
           12  F                       PIC X(10) VALUE
                                               'ENTRY DATE'.
           12  EX-HTAB54               PIC X.
           12  F                       PIC X(10) VALUE
                                               'FIRST NAME'.
           12  EX-HTAB1                PIC X.
           12  FILLER                  PIC X(03) VALUE
                                               'MID'.
           12  EX-HTAB2                PIC X.
           12  FILLER                  PIC X(09) VALUE
                                               'LAST NAME'.
           12  EX-HTAB3                PIC X.
           12  F                       PIC X(7)  VALUE
                                               'INS DOB'.
           12  EX-HTAB4                PIC X.
           12  F                       PIC X(7)  VALUE
                                               'INS SSN'.
           12  EX-HTAB5                PIC X.
           12  FILLER                  PIC X(09) VALUE
                                               'ADDRESS 1'.
           12  EX-HTAB6                PIC X.
           12  FILLER                  PIC X(09) VALUE
                                               'ADDRESS 2'.
           12  EX-HTAB7                PIC X.
           12  FILLER                  PIC X(4) VALUE
                                               'CITY'.
           12  EX-HTAB8                PIC X.
           12  FILLER                  PIC X(5) VALUE
                                               'STATE'.
           12  EX-HTAB9                PIC X.
           12  FILLER                  PIC X(08) VALUE
                                               'ZIP CODE'.
           12  EX-HTAB10               PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'TELE NO'.
           12  EX-HTAB11               PIC X.
           12  FILLER                  PIC X(11) VALUE
                                               'LF POL TYPE'.
           12  EX-HTAB12               PIC X.
           12  FILLER                  PIC X(11) VALUE
                                               'AH POL TYPE'.
           12  EX-HTAB13               PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'CERT NO'.
           12  EX-HTAB14               PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'EFF DTE'.
           12  EX-HTAB15               PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'LF TERM'.
           12  EX-HTAB16               PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'AH TERM'.
           12  EX-HTAB17               PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'LF AMT '.
           12  EX-HTAB18               PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'AH AMT '.
           12  EX-HTAB51               PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'BAL AMT'.
           12  EX-HTAB19               PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'LF RATE'.
           12  EX-HTAB20               PIC X.
           12  FILLER                  PIC X(08) VALUE
                                               'BAL RATE'.
           12  EX-HTAB21               PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'AH RATE'.
           12  EX-HTAB22               PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'LF PREM'.
           12  EX-HTAB23               PIC X.
           12  FILLER                  PIC X(08) VALUE
                                               'BAL PREM'.
           12  EX-HTAB24               PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'AH PREM'.
           12  EX-HTAB25               PIC X.
           12  FILLER                  PIC X(09) VALUE
                                               'LF EXP DT'.
           12  EX-HTAB26               PIC X.
           12  FILLER                  PIC X(09) VALUE
                                               'AH EXP DT'.
           12  EX-HTAB27               PIC X.
           12  FILLER                  PIC X(09) VALUE
                                               'TERM DATE'.
           12  EX-HTAB28               PIC X.
           12  FILLER                  PIC X(10) VALUE
                                               'TERM CAUSE'.
           12  EX-HTAB29               PIC X.
           12  FILLER                  PIC X(09) VALUE
                                               'LF REFUND'.
           12  EX-HTAB30               PIC X.
           12  FILLER                  PIC X(09) VALUE
                                               'AH REFUND'.
           12  EX-HTAB31               PIC X.
           12  FILLER                  PIC X(09) VALUE
                                               'LF REF DT'.
           12  EX-HTAB32               PIC X.
           12  FILLER                  PIC X(13) VALUE
                                               'LF REF EXT DT'.
           12  EX-HTAB33               PIC X.
           12  FILLER                  PIC X(09) VALUE
                                               'AH REF DT'.
           12  EX-HTAB34               PIC X.
           12  FILLER                  PIC X(13) VALUE
                                               'AH REF EXT DT'.
           12  EX-HTAB35               PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'ACCT NO'.
           12  EX-HTAB36               PIC X.
           12  FILLER                  PIC X(09) VALUE
                                               'ACCT NAME'.
           12  EX-HTAB37               PIC X.
           12  FILLER                  PIC X(11) VALUE
                                               'ACCT PERSON'.
           12  EX-HTAB38               PIC X.
           12  FILLER                  PIC X(10) VALUE
                                               'ACCT ADDR1'.
           12  EX-HTAB39               PIC X.
           12  FILLER                  PIC X(10) VALUE
                                               'ACCT ADDR2'.
           12  EX-HTAB40               PIC X.
           12  FILLER                  PIC X(10) VALUE
                                               'ACCT CITY '.
           12  EX-HTAB41               PIC X.
           12  FILLER                  PIC X(10) VALUE
                                               'ACCT STATE'.
           12  EX-HTAB42               PIC X.
           12  FILLER                  PIC X(10) VALUE
                                               'ACCT ZIP  '.
           12  EX-HTAB43               PIC X.
           12  FILLER                  PIC X(10) VALUE
                                               'ACCT PHONE'.
           12  EX-HTAB44               PIC X.
           12  FILLER                  PIC X(09) VALUE
                                               'LEND NAME'.
           12  EX-HTAB45               PIC X.
           12  FILLER                  PIC X(10) VALUE
                                               'LEND ADDR1'.
           12  EX-HTAB46               PIC X.
           12  FILLER                  PIC X(10) VALUE
                                               'LEND ADDR2'.
           12  EX-HTAB47               PIC X.
           12  FILLER                  PIC X(10) VALUE
                                               'LEND CITY '.
           12  EX-HTAB48               PIC X.
           12  FILLER                  PIC X(10) VALUE
                                               'LEND STATE'.
           12  EX-HTAB49               PIC X.
           12  FILLER                  PIC X(10) VALUE
                                               'LEND ZIP  '.
           12  EX-HTAB50               PIC X.
           12  FILLER                  PIC X(03) VALUE
                                               'EOR'.
      ******************************************************************
       01  WS-MISC.
           05  WS-SAVE-EXTR            PIC X(800) VALUE LOW-VALUES.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
           05  FILLER REDEFINES WS-DATE.
               10  FILLER              PIC 999.
               10  WS-CCYY             PIC 9999.
               10  WS-MM               PIC 99.
               10  WS-DD               PIC 99.

       01  WS-ABEND-FIELDS.
           12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.
           12  WS-ZERO                 PIC S9          VALUE ZERO.
           12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.
           12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.


                                       COPY ELCCALC.
                                       COPY ELCDATE.
                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

           EJECT
       PROCEDURE DIVISION.

                                       COPY ELCDTERX.                       

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0050-PROCESS        THRU 0050-EXIT UNTIL
              (END-OF-CERT)
PEMTST*       OR (EXT-RECS-OUT1 > 5000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' CERT RECORDS READ       '  CRT-RECS-IN
           DISPLAY ' EXTRACT OUT RECS        '  EXT-RECS-OUT
           DISPLAY ' ACTIVE EXTRACT RECS     '  EXT-RECS-ACT 
031109     DISPLAY ' EXPIRED EXTRACT RECS    '  EXT-RECS-EXP 
           DISPLAY ' CANCELLED EXTRACT RECS  '  EXT-RECS-CAN
           DISPLAY ' DEATH EXTRACT RECS      '  EXT-RECS-DTH
           GOBACK

           .
       0050-PROCESS.
       
022208     MOVE ZERO                   TO WS-EXPIRE-DT
031109     MOVE SPACE                  TO WS-TYPE-EXTRACT
           IF (CR-STATE NOT = 'MN')
              OR ((CR-LFTYP = SPACES OR ZEROS)
                 AND (CR-AHTYP = SPACES OR ZEROS))
              OR (CR-ENTRY-STATUS = 'D' OR 'V' OR '9')
              OR (CR-LFTYP = '09' OR '1A' OR '1B' OR '57' OR '58')
              OR (CR-AHTYP = '09' OR '44' OR '45' OR '47' OR '5B')
      *       OR (CR-LF-TERM = 1)
      *       OR (CR-AH-TERM = 1)
              OR ((CR-LF-EXPIRE-DATE NOT = ZEROS)
                    AND (CR-LF-EXPIRE-DATE < 20050101))
              OR ((CR-AH-EXPIRE-DATE NOT = ZEROS)
                    AND (CR-AH-EXPIRE-DATE < 20050101))
              OR ((CR-LF-CANCEL-EXIT-DATE NOT = ZEROS)
                    AND (CR-LF-CANCEL-EXIT-DATE < 20050101))
              OR ((CR-AH-CANCEL-EXIT-DATE NOT = ZEROS)
                    AND (CR-AH-CANCEL-EXIT-DATE < 20050101))
              OR ((CR-LF-CLAIM-EXIT-DATE NOT = ZEROS)
                    AND (CR-LF-CLAIM-EXIT-DATE < 20050101))
              CONTINUE
           ELSE
              IF CR-ENTRY-STATUS = '5'
                 MOVE ZEROS            TO CR-LFAMT
                                          CR-LFPRM
                                          CR-AHAMT
                                          CR-AHPRM
              END-IF
              PERFORM 0100-PROCESS-CERT   THRU 0100-EXIT
           END-IF

           PERFORM 0280-READ-CERT      THRU 0280-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-CERT.

           PERFORM 0175-SYNC-CERT-ERACCT
                                       THRU 0175-EXIT

           MOVE SPACES TO WS-LF-STATUS WS-AH-STATUS
                          WS-LF-TERM-DATE WS-AH-TERM-DATE
           MOVE WS-SAVE-EXTR           TO EXTR-DETAIL-RECORD

           MOVE AM-GPCD                TO WS-BUS-TYPE

           IF WS-AUTO-GROUP
              MOVE 'AUTO'              TO EX-BUS-TYPE
           ELSE
              MOVE 'BANK'              TO EX-BUS-TYPE
           END-IF

           IF CR-LFTYP NOT = ZEROS AND SPACES
              MOVE CR-LFTYP            TO EX-LF-POL-TYPE
              IF CR-ENTRY-STATUS NOT = '5'
                 MOVE 'ACTIVE'         TO WS-LF-STATUS
              END-IF
           END-IF
      *       PERFORM VARYING CLAS-INDEXL FROM CLAS-STARTL BY +1 UNTIL
      *          (CLAS-INDEXL > CLAS-MAXL)
      *          OR (CR-LFTYP = CLAS-I-BEN (CLAS-INDEXL))
      *       END-PERFORM
      *       IF CLAS-INDEXL > CLAS-MAXL
      *          DISPLAY ' LF BEN NOT FOUND ' CR-LFTYP
      *          COMPUTE WS-ZERO = WS-ZERO / WS-ZERO
      *       ELSE
      *          STRING CR-LFTYP ' ' CLAS-I-RL-AH (CLAS-INDEXL)
      *             DELIMITED BY SIZE INTO EX-LF-REF-TYPE
      *          END-STRING
      *       END-IF
      *    END-IF

           IF CR-AHTYP NOT = ZEROS AND SPACES
              MOVE CR-AHTYP            TO EX-AH-POL-TYPE
              IF CR-ENTRY-STATUS NOT = '5'
                 MOVE 'ACTIVE'         TO WS-AH-STATUS
              END-IF
           END-IF
      *       PERFORM VARYING CLAS-INDEXA FROM CLAS-STARTA BY +1 UNTIL
      *          (CLAS-INDEXA > CLAS-MAXA)
      *          OR (CR-AHTYP = CLAS-I-BEN (CLAS-INDEXA))
      *       END-PERFORM
      *       IF CLAS-INDEXA > CLAS-MAXA
      *          DISPLAY ' AH BEN NOT FOUND ' CR-AHTYP
      *          COMPUTE WS-ZERO = WS-ZERO / WS-ZERO
      *       ELSE
      *          STRING CR-AHTYP ' ' CLAS-I-RL-AH (CLAS-INDEXA)
      *             DELIMITED BY SIZE INTO EX-AH-REF-TYPE
      *          END-STRING
      *       END-IF
      *    END-IF

           MOVE CR-FNAME               TO EX-FNAME               
           MOVE CR-INIT                TO EX-MID                 
           MOVE CR-LNAME               TO EX-LNAME
           IF (CR-SOC-SEC (1:2) NOT = CR-STATE)
              AND (CR-SOC-SEC (1:2) NUMERIC)
              MOVE CR-SOC-SEC          TO EX-SSN
           END-IF

           MOVE CR-CERT-NO             TO EX-CERT                
           MOVE CR-ENTRY-DATE          TO WS-DATE
           STRING WS-MM '/' WS-DD '/' WS-CCYY
              DELIMITED BY SIZE      INTO EX-ENTRY-DATE
           MOVE CR-DT                  TO WS-DATE
           STRING WS-MM '/' WS-DD '/' WS-CCYY
              DELIMITED BY SIZE      INTO EX-EFF
           MOVE CR-LF-TERM             TO EX-LF-TERM             
           MOVE CR-AH-TERM             TO EX-AH-TERM             
           MOVE CR-LFAMT               TO EX-LF-AMT              
           MOVE CR-LFAMT-ALT           TO EX-LF-AMT-ALT          
           MOVE CR-LFPRM-RATE          TO EX-LF-PREM-RATE        
           MOVE CR-LFPRM-RATE-ALT      TO EX-LF-PREM-RATE-ALT    
           MOVE CR-AHPRM-RATE          TO EX-AH-PREM-RATE        
           MOVE CR-LFPRM               TO EX-LF-PREM             
           MOVE CR-LFPRM-ALT           TO EX-LF-PREM-ALT         
           MOVE CR-AHAMT               TO EX-AH-AMT              
           MOVE CR-AHPRM               TO EX-AH-PREM             
           IF CR-LF-EXPIRE-DATE NOT = ZEROS
              MOVE CR-LF-EXPIRE-DATE      TO WS-DATE
              STRING WS-MM '/' WS-DD '/' WS-CCYY
                 DELIMITED BY SIZE      INTO EX-LF-EXP
           END-IF

           IF CR-AH-EXPIRE-DATE NOT = ZEROS
              MOVE CR-AH-EXPIRE-DATE      TO WS-DATE
              STRING WS-MM '/' WS-DD '/' WS-CCYY
                 DELIMITED BY SIZE      INTO EX-AH-EXP
           END-IF
           MOVE CR-LFRFND              TO EX-LF-REFUND           
           MOVE CR-AHRFND              TO EX-AH-REFUND           

           IF (CR-LFRFND > ZEROS)
              OR (CR-LF-CANCEL-EXIT-DATE > ZEROS)
              IF CR-LF-CANC-DT NOT = ZEROS
                 MOVE CR-LF-CANC-DT          TO WS-DATE
                 STRING WS-MM '/' WS-DD '/' WS-CCYY
                    DELIMITED BY SIZE  INTO EX-LF-REF-DT
              END-IF
              IF CR-LF-CANCEL-EXIT-DATE NOT = ZEROS
                 MOVE CR-LF-CANCEL-EXIT-DATE TO WS-DATE
                 STRING WS-MM '/' WS-DD '/' WS-CCYY
                    DELIMITED BY SIZE  INTO EX-LF-REF-EXT-DT
              END-IF
              MOVE 'CANCEL'            TO WS-LF-STATUS
              MOVE EX-LF-REF-EXT-DT    TO WS-LF-TERM-DATE
           END-IF

           IF (CR-AHRFND > ZEROS)
              OR (CR-AH-CANCEL-EXIT-DATE > ZEROS)
              IF CR-AH-CANC-DT NOT = ZEROS
                 MOVE CR-AH-CANC-DT          TO WS-DATE
                 STRING WS-MM '/' WS-DD '/' WS-CCYY
                    DELIMITED BY SIZE  INTO EX-AH-REF-DT
              END-IF
              IF CR-AH-CANCEL-EXIT-DATE NOT = ZEROS
                 MOVE CR-AH-CANCEL-EXIT-DATE TO WS-DATE
                 STRING WS-MM '/' WS-DD '/' WS-CCYY
                    DELIMITED BY SIZE  INTO EX-AH-REF-EXT-DT
              END-IF
              MOVE 'CANCEL'            TO WS-AH-STATUS
              MOVE EX-AH-REF-EXT-DT    TO WS-AH-TERM-DATE
           END-IF

           MOVE CR-ACCOUNT             TO EX-ACCOUNT
           MOVE AM-NAME                TO EX-ACCOUNT-NAME
           MOVE AM-CONTROL-NAME        TO EX-ACCT-PERSON
           MOVE AM-ADDRS               TO EX-ACCT-ADDR1
           MOVE AM-ADDR-CITY           TO EX-ACCT-CITY
           MOVE AM-ADDR-STATE          TO EX-ACCT-ST
           MOVE AM-TEL-NO              TO EX-ACCT-TELE
           MOVE AM-ZIP                 TO EX-ACCT-ZIP
           MOVE CR-BENEFICIARY         TO EX-LEND-NAME

           IF CR-DTHAMT > ZEROS
              MOVE 'DEATH'             TO EX-TERM-CAUSE
              IF CR-LF-CLAIM-EXIT-DATE NOT = ZEROS
                 MOVE CR-LF-CLAIM-EXIT-DATE TO WS-DATE
                 STRING WS-MM '/' WS-DD '/' WS-CCYY
                    DELIMITED BY SIZE  INTO EX-TERM-DATE
              END-IF
           END-IF

           PERFORM 0110-GET-ERMAIL     THRU 0110-EXIT
           IF ERMAIL-FILE-STATUS = '00'
              MOVE MA-ADDRESS-LINE-1   TO EX-ADDRESS1
              MOVE MA-ADDRESS-LINE-2   TO EX-ADDRESS2
              MOVE MA-CITY             TO EX-CITY
              MOVE AM-ADDR-STATE       TO EX-STATE
              MOVE MA-ZIP              TO EX-ZIP
              MOVE MA-PHONE-NO         TO WS-WORK-PHONE
              IF MA-PHONE-NO NUMERIC
                 AND WS-WORK-PHONE-ALPHA NOT = SPACES
                 AND MA-PHONE-NO NOT = ZEROS
                 AND MA-PHONE-NO > 20
                 MOVE MA-PHONE-NO      TO EX-TELE
              END-IF
      *       MOVE MA-PHONE-NO         TO EX-TELE
      *       MOVE ZEROS               TO EX-DOB

              IF (MA-INSURED-SOC-SEC-NO NOT = ZEROS AND SPACES)
                 AND(MA-INSURED-SOC-SEC-NO (1:2) NUMERIC)
                 MOVE MA-INSURED-SOC-SEC-NO TO EX-SSN
              END-IF
              IF MA-INSURED-BIRTH-DT NOT = LOW-VALUES AND SPACES
                 MOVE MA-INSURED-BIRTH-DT TO DC-BIN-DATE-1
                 MOVE ' '              TO DC-OPTION-CODE
                 PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
                 IF NO-CONVERSION-ERROR
                    MOVE DC-GREG-DATE-1-EDIT TO EX-DOB
                 END-IF
              END-IF
              MOVE MA-CRED-BENE-NAME   TO EX-LEND-NAME
              MOVE MA-CRED-BENE-ADDR   TO EX-LEND-ADDR1
              MOVE MA-CRED-BENE-ADDR2  TO EX-LEND-ADDR2
              MOVE MA-CRED-BENE-CITY   TO EX-LEND-CITY
              MOVE MA-CRED-BENE-STATE  TO EX-LEND-ST
              IF MA-CRED-BENE-ZIP NOT = ZEROS
                 MOVE MA-CRED-BENE-ZIP TO EX-LEND-ZIP
              END-IF
           END-IF

           IF (CR-LF-EXPIRE-DATE NOT = ZERO)
              AND (CR-LF-EXPIRE-DATE <= 20101130)
              AND (WS-LF-STATUS NOT = 'CANCEL')
              MOVE 'EXPIRED'           TO WS-LF-STATUS
           END-IF

           IF (CR-AH-EXPIRE-DATE NOT = ZERO)
              AND (CR-AH-EXPIRE-DATE <= 20101130)
              AND (WS-AH-STATUS NOT = 'CANCEL')
              MOVE 'EXPIRED'           TO WS-AH-STATUS
           END-IF

           EVALUATE TRUE
              WHEN EX-TERM-CAUSE = 'DEATH'
                 MOVE 'CLAIM'          TO EX-STATUS
                 ADD 1 TO EXT-RECS-DTH
              WHEN (WS-LF-STATUS = 'ACTIVE')
                 OR (WS-AH-STATUS = 'ACTIVE')
                 MOVE 'ACTIVE'         TO EX-STATUS
                 ADD 1 TO EXT-RECS-ACT
              WHEN (WS-LF-STATUS = 'CANCEL')
                 OR (WS-AH-STATUS = 'CANCEL')
                 MOVE 'CANCEL  '       TO EX-STATUS
                                          EX-TERM-CAUSE
                 ADD 1 TO EXT-RECS-CAN
                 IF WS-LF-TERM-DATE NOT = SPACES
                    MOVE WS-LF-TERM-DATE TO EX-TERM-DATE
                 ELSE
                    MOVE WS-AH-TERM-DATE TO EX-TERM-DATE
                 END-IF
              WHEN (WS-LF-STATUS = 'EXPIRED')
                 OR (WS-AH-STATUS = 'EXPIRED')
                 MOVE 'EXPIRED '       TO EX-STATUS
                 ADD 1 TO EXT-RECS-EXP
              WHEN CR-ENTRY-STATUS = '5'
                 MOVE 'REISSUE'        TO EX-STATUS
              WHEN OTHER
                 MOVE 'OTHER '         TO EX-STATUS
           END-EVALUATE
                                    

           PERFORM 0300-WRITE-EXTR     THRU 0300-EXIT

           .
       0100-EXIT.
           EXIT.
           
       0110-GET-ERMAIL.

           MOVE LOW-VALUES             TO MA-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO MA-COMPANY-CD
           MOVE CR-CARRIER             TO MA-CARRIER
           MOVE CR-GROUPING            TO MA-GROUPING
           MOVE CR-STATE               TO MA-STATE
           MOVE CR-ACCOUNT             TO MA-ACCOUNT
           MOVE CR-CERT-NO             TO MA-CERT-NO
           MOVE CR-DT                  TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO MA-CERT-EFF-DT
              READ ERMAIL
              IF ERMAIL-FILE-STATUS = '00' OR '23' OR '22' OR '10'
                 CONTINUE
              ELSE
                 DISPLAY ' ERMAIL ERROR - READ ' ERMAIL-FILE-STATUS
                    '  ' CR-CERT-NO
           ELSE
              DISPLAY ' BAD EFF DATE ' CR-DT '  ' CR-CERT-NO   
           END-IF
           
           .
       0110-EXIT.
           EXIT.

       0125-START-ERACCT.

           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD

           START ERACCT KEY >= AM-CONTROL-PRIMARY

           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERACCT ERROR - START ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0125-EXIT.
           EXIT.

       0150-READ-ERACCT.

           READ ERACCT NEXT RECORD

           IF (ERACCT-FILE-STATUS = '10' OR '23')
              OR (AM-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
              MOVE HIGH-VALUES         TO AM-CONTROL-PRIMARY
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERACCT ERROR - READ ' ERACCT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0150-EXIT.
           EXIT.

       0175-SYNC-CERT-ERACCT.

           IF (CR-ACCT-CONTROL > AM-CONTROL-A)
              OR ((CR-ACCT-CONTROL = AM-CONTROL-A)
                 AND (CR-DT >= AM-EXPIRE-DT))
                 PERFORM 0150-READ-ERACCT
                                       THRU 0150-EXIT
                 GO TO 0175-SYNC-CERT-ERACCT
           ELSE
              IF (CR-ACCT-CONTROL < AM-CONTROL-A)
                 OR ((CR-ACCT-CONTROL = AM-CONTROL-A)
                    AND (CR-DT < AM-EFFECT-DT))
                 DISPLAY ' NO MATCHING ACCOUNT '
                 DISPLAY ' AM CONTROL          ' AM-CONTROL-A
                 DISPLAY ' CR CONTROL          ' CR-ACCT-CONTROL
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0175-EXIT.
           EXIT.




       0280-READ-CERT.

           READ CERT-FILE-IN AT END
              SET END-OF-CERT          TO TRUE
           END-READ


           IF NOT END-OF-CERT
              ADD 1                    TO CRT-RECS-IN
           END-IF

           .
       0280-EXIT.
           EXIT.

       0300-WRITE-EXTR.

           INSPECT EXTR-DETAIL-RECORD REPLACING
              ALL ';'         BY ' '
              ALL X'00'       BY ' '
              ALL X'01'       BY ' '
              ALL X'02'       BY ' '
              ALL X'03'       BY ' '
              ALL X'04'       BY ' '
              ALL X'05'       BY ' '
              ALL X'06'       BY ' '
              ALL X'07'       BY ' '
              ALL X'08'       BY ' '
              ALL X'0C'       BY ' '
              ALL X'14'       BY ' '
              ALL X'1B'       BY ' '
              ALL X'1C'       BY ' '
              ALL X'1E'       BY ' '
              ALL X'9C'       BY ' '
              ALL X'09'       BY ';'


031109     MOVE 'E' TO EX-END
           WRITE EXTR-FILE-OUT-REC1 FROM EXTR-DETAIL-RECORD
           ADD 1                    TO EXT-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT CERT-FILE-IN ERACCT ERMAIL
               OUTPUT EXTR-FILE-OUT1

           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERACCT ERROR - OPEN ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF


           IF ERMAIL-FILE-STATUS NOT = '00'
              DISPLAY ' ERMAIL ERROR - OPEN ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE CERT-FILE-IN EXTR-FILE-OUT1
                 ERACCT ERMAIL

           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERACCT ERROR - CLOSE ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERMAIL-FILE-STATUS NOT = '00'
              DISPLAY ' ERMAIL ERROR - CLOSE ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0500-EXIT.
           EXIT.


       0600-INITIALIZE.

           MOVE SPACES                 TO EXTR-DETAIL-RECORD
           MOVE X'09'                  TO EX-TAB1
                                          EX-TAB2
                                          EX-TAB3
                                          EX-TAB4
                                          EX-TAB5
                                          EX-TAB6
                                          EX-TAB7
                                          EX-TAB8
                                          EX-TAB9
                                          EX-TAB10
                                          EX-TAB11
                                          EX-TAB12
                                          EX-TAB13
                                          EX-TAB14
                                          EX-TAB15
                                          EX-TAB16
                                          EX-TAB17
                                          EX-TAB18
                                          EX-TAB19
                                          EX-TAB20
                                          EX-TAB21
                                          EX-TAB22
                                          EX-TAB23
                                          EX-TAB24
                                          EX-TAB25
                                          EX-TAB26
                                          EX-TAB27
                                          EX-TAB28
                                          EX-TAB29
                                          EX-TAB30
                                          EX-TAB31
                                          EX-TAB32
                                          EX-TAB33
                                          EX-TAB34
                                          EX-TAB35
                                          EX-TAB36
                                          EX-TAB37
                                          EX-TAB38
                                          EX-TAB39
                                          EX-TAB40
                                          EX-TAB41
                                          EX-TAB42
                                          EX-TAB43
                                          EX-TAB44
                                          EX-TAB45
                                          EX-TAB46
                                          EX-TAB47
                                          EX-TAB48
                                          EX-TAB49
                                          EX-TAB50
                                          EX-TAB51
                                          EX-TAB52
                                          EX-TAB53
                                          EX-TAB54

           MOVE 'E'                    TO EX-END
           MOVE ';'                    TO EX-HTAB1
                                          EX-HTAB2
                                          EX-HTAB3
                                          EX-HTAB4
                                          EX-HTAB5
                                          EX-HTAB6
                                          EX-HTAB7
                                          EX-HTAB8
                                          EX-HTAB9
                                          EX-HTAB10
                                          EX-HTAB11
                                          EX-HTAB12
                                          EX-HTAB13
                                          EX-HTAB14
                                          EX-HTAB15
                                          EX-HTAB16
                                          EX-HTAB17
                                          EX-HTAB18
                                          EX-HTAB19
                                          EX-HTAB20
                                          EX-HTAB21
                                          EX-HTAB22
                                          EX-HTAB23
                                          EX-HTAB24
                                          EX-HTAB25
                                          EX-HTAB26
                                          EX-HTAB27
                                          EX-HTAB28
                                          EX-HTAB29
                                          EX-HTAB30
                                          EX-HTAB31
                                          EX-HTAB32
                                          EX-HTAB33
                                          EX-HTAB34
                                          EX-HTAB35
                                          EX-HTAB36
                                          EX-HTAB37
                                          EX-HTAB38
                                          EX-HTAB39
                                          EX-HTAB40
                                          EX-HTAB41
                                          EX-HTAB42
                                          EX-HTAB43
                                          EX-HTAB44
                                          EX-HTAB45
                                          EX-HTAB46
                                          EX-HTAB47
                                          EX-HTAB48
                                          EX-HTAB49
                                          EX-HTAB50
                                          EX-HTAB51
                                          EX-HTAB52
                                          EX-HTAB53
                                          EX-HTAB54

           MOVE EXTR-DETAIL-RECORD     TO WS-SAVE-EXTR

           MOVE EXTR-DETAIL-HEADER     TO EXTR-FILE-OUT-HEAD1
           WRITE EXTR-FILE-OUT-HEAD1


           PERFORM 0280-READ-CERT      THRU 0280-EXIT
           PERFORM 0125-START-ERACCT   THRU 0125-EXIT
           PERFORM 0150-READ-ERACCT    THRU 0150-EXIT

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              DISPLAY ' BINARY RUN DATE ' DC-GREG-DATE-CYMD
           END-IF

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE '6'                    TO DC-OPTION-CODE
           MOVE -1                     TO DC-ELAPSED-MONTHS
           MOVE '1'                    TO DC-END-OF-MONTH
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-2       TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              MOVE ZEROS               TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-CYMD   TO WS-LAST-MONTH-DT
                 DISPLAY ' THIS MONTH END DATE ' RUN-DATE
                 DISPLAY ' LAST MONTH END DATE ' WS-LAST-MONTH-DT
              ELSE
                 DISPLAY ' ERROR CREATING LAST MONTH END DATE'
                 PERFORM ABEND-PGM
              END-IF
           END-IF
031109     MOVE RUN-DATE               TO WS-RUN-DT

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE '6'                    TO DC-OPTION-CODE
           MOVE -12                    TO DC-ELAPSED-MONTHS
           MOVE '1'                    TO DC-END-OF-MONTH
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-2       TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              MOVE ZEROS               TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-CYMD   TO WS-12-MONTHS-AGO
                 DISPLAY ' 12 MONTHS AGO       ' WS-12-MONTHS-AGO
              ELSE
                 DISPLAY ' ERROR CREATING 12 MONTHS AGO DATE'
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE '6'                    TO DC-OPTION-CODE
           MOVE -13                    TO DC-ELAPSED-MONTHS
           MOVE '1'                    TO DC-END-OF-MONTH
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-2       TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              MOVE ZEROS               TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-CYMD   TO WS-13-MONTHS-AGO
                 DISPLAY ' 13 MONTHS AGO       ' WS-13-MONTHS-AGO
              ELSE
                 DISPLAY ' ERROR CREATING 13 MONTHS AGO DATE'
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM SECTION.
                                     COPY ELCABEND.

