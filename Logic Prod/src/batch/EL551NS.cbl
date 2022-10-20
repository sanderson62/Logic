       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EL551NS.
       AUTHOR.        PABLO.
       DATE-COMPILED.

      *REMARKS.
      *    THIS PROGRAM READS THE LETTER ARCHIVE FILE AND CREATES
      *    2 REPORTS AND 2 EXTRACT FILES THAT MAKE UP THE DATA ON 
      *    EACH REPORT. FIRST REPORT IS OF THE LETTERS THAT SOMETHING
      *    IS GOING TO HAPPEN THE NEXT BUSINESS DAY.
      *    THE SECOND REPORT IS OF WHAT HAPPENED THAT BUSINESS DAY.
011013******************************************************************
011013*                   C H A N G E   L O G
011013*
011013* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
011013*-----------------------------------------------------------------
011013*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
011013* EFFECTIVE    NUMBER
011013*-----------------------------------------------------------------
011013* 011013    2012122700004  AJRA  IGNORE TEMP LETTERS
011013******************************************************************
            
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT DISK-DATE            ASSIGN TO SYS019.
           SELECT PRINTX               ASSIGN TO SYS008.
           SELECT FICH                 ASSIGN TO SYS020.
           SELECT ERARCH               ASSIGN TO ERARCH
                  ACCESS IS DYNAMIC
                  ORGANIZATION IS INDEXED
                  FILE STATUS IS ERARCH-FILE-STATUS
                  RECORD KEY IS ERARCH-KEY.

           SELECT SORTFL               ASSIGN TO SORTWK1.

           SELECT PENDING-EXTRACT      ASSIGN TO SYS011
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ACTIVITY-EXTRACT     ASSIGN TO SYS012
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  PRINTX
                                       COPY ELCPRTFD.

       FD  FICH
                                       COPY ELCFCHFD.

       FD  ERARCH.

       01  ERARCH-IN-RECORD.
           12  FILLER                  PIC XX.
           12  ERARCH-KEY              PIC X(5).
           12  FILLER                  PIC X(243).
           
       SD  SORTFL.

       01  SRT-REC.
           12  SRT-REC-TYPE            PIC X.
           12  SRT-ARCH-REC.
               16  FILLER              PIC X(73).
               16  SRT-PROC-ID         PIC X(4).
               16  FILLER              PIC X(173).

       FD  PENDING-EXTRACT
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PENDING-RECORD              PIC X(250).

       FD  ACTIVITY-EXTRACT
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ACTIVITY-RECORD             PIC X(250).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '      EL551NS WORKING STORAGE   '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  WS-CURRENT-BIN-DATE         PIC XX  VALUE LOW-VALUES.
       77  WS-CURRENT-DATE-PLUS-5      PIC XX  VALUE LOW-VALUES.    
       77  WS-CURRENT-DATE-MINUS-5     PIC XX  VALUE LOW-VALUES.    
       77  WS-PASS-RESEND-DT           PIC XX  VALUE LOW-VALUES.
       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-ERARCH              VALUE 'Y'.
           88  MORE-ERARCH                VALUE 'N'.
       77  WS-A                       PIC S9(03) COMP-3 VALUE +0.
       77  IYR                         PIC S9(03) COMP-3 VALUE +0.
       77  VYR                         PIC S9(03) COMP-3 VALUE +0.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  SPACE-NP                    PIC X      VALUE '1'.
       77  SPACE-1                     PIC X      VALUE ' '.
       77  SPACE-2                     PIC X      VALUE '0'.
       77  SPACE-3                     PIC X      VALUE '-'.
       77  X                           PIC X      VALUE ' '.
       77  LNCTR                       PIC S999   VALUE +60 COMP-3.
       77  PGCTR                       PIC S9(5)  VALUE +0 COMP-3.
       77  ERARCH-IN-CNT               PIC 9(11)   VALUE ZEROS.
       77  RETURN-CNT                  PIC 9(11)   VALUE ZEROS.
       77  RELEASE-CNT                 PIC 9(11)   VALUE ZEROS.
       77  WS-SAVE-PROC-ID             PIC X(04)   VALUE SPACES.
       77  WS-HOLD-TYPE                PIC X(20)   VALUE SPACES.
       77  WS-PREV-REC-TYPE            PIC X       VALUE ' '.
       77  WS-DAY-OF-WEEK              PIC S9      VALUE ZEROS.
       77  WS-NEXT-CYCLE-BIN           PIC XX      VALUE LOW-VALUES.
       77  WS-CURR-CYCLE-BIN           PIC XX      VALUE LOW-VALUES.

       01  HDA-1.
           12  FILLER              PIC  X(44)      VALUE SPACES.
           12  FILLER              PIC  X(42)      VALUE
                   '      PENDING CORRESPONDENCE REPORT       '.
           12  FILLER              PIC  X(31)      VALUE SPACES.
           12  FILLER              PIC  X(08)      VALUE 'EL551A '.

       01  HDB-1.
           12  FILLER              PIC  X(44)      VALUE SPACES.
           12  FILLER              PIC  X(42)      VALUE
                   '      CORRESPONDENCE ACTIVITY REPORT      '.
           12  FILLER              PIC  X(31)      VALUE SPACES.
           12  FILLER              PIC  X(08)      VALUE 'EL551B '.

       01  HD2.
           12  FILLER              PIC  X(51)      VALUE SPACES.
           12  HD-CO               PIC  X(30).
           12  FILLER              PIC  X(38)      VALUE SPACES.
           12  HD-RUN-DT           PIC  X(08)      VALUE SPACES.

       01  HD3.
           12  FILLER              PIC  X(57)      VALUE SPACES.
           12  HD-DT               PIC  X(18).
           12  FILLER              PIC  X(44)      VALUE SPACES.
           12  FILLER              PIC  X(05)      VALUE 'PAGE '.
           12  HD-PG               PIC ZZ,ZZ9.

       01  HD4.
           12  FILLER              PIC X(15)   VALUE SPACES.
           12  FILLER              PIC X(15)   VALUE 'PROCESSOR ID - '.
           12  HD4-PROC-ID         PIC X(4)    VALUE SPACES.

       01  HDA-5.
           12  FILLER                  PIC X(098)  VALUE
               '      CERT       ACCT            FORM  ARCHIVE  ORIG    
      -        '  ORIG  RESEND SCHEDULED            FINAL '.

       01  HDA-6.
           12  FILLER                  PIC X(121)  VALUE
               ' CAR NUMBER      NUMBER    SRCE  TYPE  NUMBER  CREATED  
      -        ' PRINT   FORM  TO RESEND  RESENT   ACTION  CSR   TYPE   
      -        '   STATUS'.

       01  HDB-5.
           12  FILLER                  PIC X(126)  VALUE
               '      CERT       ACCT            FORM  ARCHIVE  ORIG    
      -        '  ORIG     SCHEDULED            FINAL      ACTIVITY  ACT
      -        'IVITY  CURRENT'.

       01  HDB-6.
           12  FILLER                  PIC X(126)  VALUE
               ' CAR NUMBER      NUMBER    SRCE  TYPE  NUMBER  CREATED  
      -        ' PRINT      RE-SEND   RESENT   ACTION  CSR   TYPE     DA
      -        'TE      STATUS'.


       01  WS-DETAIL                   PIC X(133)  VALUE SPACES.        
                                                                        
       01  WS-DETAILA REDEFINES WS-DETAIL.
           12  FILLER                  PIC XX.
           12  WS-DA-CARRIER           PIC X.
           12  FILLER                  PIC X.                           
           12  WS-DA-CERT-NO           PIC X(11).                       
           12  FILLER                  PIC X.                           
           12  WS-DA-ACCT-NO           PIC X(10).
           12  FILLER                  PIC X.                          
           12  WS-DA-DATA-SOURCE       PIC X(4).                        
           12  FILLER                  PIC XX.
           12  WS-DA-FORM-TYPE         PIC X(4).                        
           12  FILLER                  PIC X.                           
           12  WS-DA-ARCHIVE-NO        PIC Z(7)9.                       
           12  WS-DA-ARCHIVE-NO-X          REDEFINES                    
               WS-DA-ARCHIVE-NO        PIC X(8).                        
           12  FILLER                  PIC X.                          
           12  WS-DA-ORIG-CREATE-DT    PIC X(8).                        
           12  FILLER                  PIC XX.                          
           12  WS-DA-ORIG-PRINT-DT     PIC X(8).                    
           12  FILLER                  PIC X.
           12  WS-DA-RESEND-FORM       PIC X(4).
           12  FILLER                  PIC XX.
           12  WS-DA-RESEND-SCHED      PIC X(8).
           12  FILLER                  PIC X.
           12  WS-DA-RESENT            PIC X(8).
           12  F                       PIC X.
           12  WS-DA-FINAL-ACTION-DT   PIC X(8).
           12  F                       PIC X.
           12  WS-DA-CSR               PIC X(4).
           12  F                       PIC XX.
           12  WS-DA-TYPE              PIC X(9).
           12  F                       PIC X.
           12  WS-DA-STATUS            PIC X(10).

       01  WS-DETAILB REDEFINES WS-DETAIL.
           12  FILLER                  PIC XX.
           12  WS-DB-CARRIER           PIC X.
           12  FILLER                  PIC X.                           
           12  WS-DB-CERT-NO           PIC X(11).                       
           12  FILLER                  PIC X.                           
           12  WS-DB-ACCT-NO           PIC X(10).
           12  FILLER                  PIC X.                          
           12  WS-DB-DATA-SOURCE       PIC X(4).                        
           12  FILLER                  PIC XX.
           12  WS-DB-FORM-TYPE         PIC X(4).                        
           12  FILLER                  PIC X.                           
           12  WS-DB-ARCHIVE-NO        PIC Z(7)9.                       
           12  WS-DB-ARCHIVE-NO-X          REDEFINES                    
               WS-DB-ARCHIVE-NO        PIC X(8).                        
           12  FILLER                  PIC X.                          
           12  WS-DB-ORIG-CREATE-DT    PIC X(8).                        
           12  FILLER                  PIC XX.                          
           12  WS-DB-ORIG-PRINT-DT     PIC X(8).                    
           12  FILLER                  PIC XXX.
           12  WS-DB-RESEND-SCHED      PIC X(8).
           12  FILLER                  PIC XX.
           12  WS-DB-RESENT            PIC X(8).
           12  F                       PIC X.
           12  WS-DB-FINAL-ACTION-DT   PIC X(8).
           12  F                       PIC X.
           12  WS-DB-CSR               PIC X(4).
           12  F                       PIC X.
           12  WS-DB-TYPE              PIC X(8).
           12  F                       PIC X.
           12  WS-DB-ACTIVITY-DT       PIC X(8).
           12  F                       PIC XX.
           12  WS-DB-STATUS            PIC X(10).

       01  WS-REPORT-WORK.
           12  WS-CARRIER              PIC X.
           12  WS-CERT-NO              PIC X(11).                       
           12  WS-ACCT-NO              PIC X(10).
           12  WS-DATA-SOURCE          PIC X(4).                        
           12  WS-FORM-TYPE            PIC X(4).                        
           12  WS-ARCHIVE-NO           PIC Z(7)9.                       
           12  WS-ARCHIVE-NO-X             REDEFINES                    
               WS-ARCHIVE-NO           PIC X(8).                        
           12  WS-ORIG-CREATE-DT       PIC X(8).                        
           12  WS-ORIG-PRINT-DT        PIC X(8).                    
           12  WS-RESEND-FORM          PIC X(4).
           12  WS-RESEND-SCHED         PIC X(8).
           12  WS-RESENT               PIC X(8).
           12  WS-FINAL-ACTION-DT      PIC X(8).
           12  WS-CSR                  PIC X(4).
           12  WS-TYPE                 PIC X(9).
           12  WS-STATUS               PIC X(10).
           12  WS-ACTIVITY-DT          PIC X(8).
                                                                        
       01  WS-MISC.
           05  ERARCH-FILE-STATUS      PIC XX.
           05  WS-HOLD-PRT             PIC X(133).
           05  INTERMED                PIC S9(9)V9(6)  COMP-3.
           05  WS-BIN-CR-DT            PIC XX  VALUE LOW-VALUES.
           05  WS-BIN-LF-END-DATE      PIC XX  VALUE LOW-VALUES.
           05  WS-BIN-AH-END-DATE      PIC XX  VALUE LOW-VALUES.
           05  WS-END-YEAR             PIC 9(11)  VALUE ZEROS.
           05  FILLER REDEFINES WS-END-YEAR.
               10  WS-CCYY             PIC 9(7).
               10  WS-MMDD             PIC 9(4).
       01  WS-DEBUG-AREA.
           12  WS-DEBUG-SW             PIC X(01) VALUE ' '.
               88  DEBUG-IS-ON                VALUES '1' '2' '3' '4'.
               88  DEBUG-LF-INFORCE-CNT       VALUE '1'.
               88  DEBUG-AH-INFORCE-CNT       VALUE '2'.
               88  DEBUG-LF-STATUTORY         VALUE '3'.
               88  DEBUG-AH-STATUTORY         VALUE '4'.

       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

       01  WORK-ABEND-CODE.
           12  WAC-1                   PIC X.
           12  WAC-2                   PIC X.
           12  WAC-3-4.
               16  WAC-3               PIC X.
               16  WAC-4               PIC X.

       01  DATE-AREAS.
           05  WS-NEXT-WORK-DT         PIC XX.
           05  WS-CURR-WORK-DT         PIC XX.
           05  WS-TEST-RUN-DATE        PIC 9(11).
           05  WS-WORK-DATE            PIC 9(11).
           05  FILLER REDEFINES WS-WORK-DATE.
               10  FILLER              PIC XXX.
               10  WS-WORK-CCYY        PIC 9(4).
               10  WS-WORK-MMDD        PIC 9(4).
           05  WS-ISSUE-DATE           PIC 9(11).
           05  FILLER REDEFINES WS-ISSUE-DATE.
               10  FILLER              PIC XXX.
               10  WS-ISSUE-CCYY       PIC 9(4).
               10  WS-ISSUE-MMDD       PIC 9(4).
           05  WS-EXPIRE-DATE          PIC 9(11).
           05  FILLER REDEFINES WS-EXPIRE-DATE.
               10  FILLER              PIC XXX.
               10  WS-EXPIRE-CCYY      PIC 9(4).
               10  WS-EXPIRE-MMDD      PIC 9(4).
           05  WS-ENTRY-DATE           PIC 9(11).
           05  FILLER REDEFINES WS-ENTRY-DATE.
               10  FILLER              PIC XXX.
               10  WS-ENTRY-CCYY       PIC 9(4).
               10  WS-ENTRY-MMDD       PIC 9(4).
           05  WS-CANCEL-DATE          PIC 9(11).
           05  FILLER REDEFINES WS-CANCEL-DATE.
               10  FILLER              PIC XXX.
               10  WS-CANCEL-CCYY      PIC 9(4).
               10  WS-CANCEL-MMDD      PIC 9(4).
           05  WS-CLAIM-DATE           PIC 9(11).
           05  FILLER REDEFINES WS-CLAIM-DATE.
               10  FILLER              PIC XXX.
               10  WS-CLAIM-CCYY       PIC 9(4).
               10  WS-CLAIM-MMDD       PIC 9(4).
           05  WS-POSTING-DATE         PIC X(8).
           05  WS-HI-ISSUE-CCYY        PIC 9(4) VALUE ZEROS.
           05  WS-LO-ISSUE-CCYY        PIC 9(4) VALUE ZEROS.
           05  WS-HI-VALUATION-CCYY    PIC 9(4) VALUE ZEROS.
           05  WS-LO-VALUATION-CCYY    PIC 9(4) VALUE ZEROS.
           05  WS-BIN-VAL-DATES OCCURS 50
                                       PIC XX.

       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +064.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

                                       COPY ERCARCH.

                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       LINKAGE SECTION.

       01  PARM.
           05  PARM-LENGTH             PIC 9(04) BINARY VALUE ZEROS.
           05  PARM-CURR-CYCLE-DT      PIC X(08)     VALUE SPACES.
           05  PARM-NEXT-CYCLE-DT      PIC X(08)     VALUE SPACES.

       PROCEDURE DIVISION USING PARM.

       0001-DT-CRD-READ.
                                       COPY ELCDTERX.

           SORT SORTFL ON ASCENDING KEY SRT-REC-TYPE SRT-PROC-ID
              INPUT PROCEDURE
                 0001-INPUT-RTN        THRU 0001-EXIT
              OUTPUT PROCEDURE
                 0100-OUTPUT-RTN       THRU 0100-EXIT.

           IF SORT-RETURN NOT = ZEROES
              MOVE +0101               TO WS-RETURN-CODE
              MOVE '   SORT ABORTED'   TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF
           
           GOBACK
           .
       0001-INPUT-RTN.
       
           DISPLAY ' BEGIN SORT INPUT ROUTINE '

           PERFORM 0010-OPEN-ERARCH    THRU 0010-EXIT
           PERFORM 0020-INITIALIZE     THRU 0020-EXIT

           PERFORM 0025-SELECT-RECS    THRU 0025-EXIT UNTIL
              END-OF-ERARCH
           
           SET MORE-ERARCH             TO TRUE
           DISPLAY ' ERARCH RECORDS READ ' ERARCH-IN-CNT
           DISPLAY ' RECORDS RELEASED    ' RELEASE-CNT

           PERFORM 0070-CLOSE-ERARCH   THRU 0070-EXIT

           .
       0001-EXIT.
           EXIT.

       0010-OPEN-ERARCH.

           OPEN INPUT ERARCH

           IF ERARCH-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON ERARCH OPEN  '
                 ERARCH-FILE-STATUS
              MOVE ' ERROR ON ERARCH OPEN  '
                                       TO WS-ABEND-MESSAGE
              MOVE ERARCH-FILE-STATUS  TO WS-ABEND-FILE-STATUS      
              PERFORM ABEND-PGM
           END-IF

           .
       0010-EXIT.
           EXIT.

       0020-INITIALIZE.

           DISPLAY ' ACCEPT DATE ' WS-ACCEPT-DATE

           MOVE WS-ACCEPT-DATE         TO DC-GREG-DATE-1-YMD
           MOVE '3'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-CURRENT-BIN-DATE
                                          WS-CURR-WORK-DT
              MOVE DC-DAY-OF-WEEK      TO WS-DAY-OF-WEEK
           ELSE
              DISPLAY ' ERROR CONVERTING CURRENT DATE '
           END-IF

           MOVE COMPANY-NAME           TO HD-CO
011013*    MOVE ALPH-DATE              TO HD-DT
           MOVE WS-CURRENT-DATE        TO HD-RUN-DT

           MOVE RUN-DATE               TO WS-TEST-RUN-DATE

           IF PARM-LENGTH = ZEROS
              DISPLAY ' MISSING OR INVALID PARM '
              PERFORM ABEND-PGM
           END-IF

           DISPLAY ' CURR CYCLE DT ' PARM-CURR-CYCLE-DT
           DISPLAY ' NEXT CYCLE DT ' PARM-NEXT-CYCLE-DT
           MOVE PARM-CURR-CYCLE-DT     TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-DAYS
                                          DC-ELAPSED-MONTHS

           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           IF DATE-CONVERSION-ERROR
              DISPLAY 'CURR CYCLE DATE CONVERSION ERROR: '
                 DC-ERROR-CODE  '  USING CURR DT ' PARM-CURR-CYCLE-DT
           ELSE
              MOVE DC-BIN-DATE-1      TO WS-CURR-CYCLE-BIN
011013        MOVE DC-GREG-DATE-1-ALPHA   TO HD-DT
           END-IF

           MOVE PARM-NEXT-CYCLE-DT     TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-DAYS
                                          DC-ELAPSED-MONTHS

           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           IF DATE-CONVERSION-ERROR
              DISPLAY 'NEXT CYCLE DATE CONVERSION ERROR: '
                 DC-ERROR-CODE  '  USING NEXT DT ' PARM-NEXT-CYCLE-DT
           ELSE
              MOVE DC-BIN-DATE-1      TO WS-NEXT-CYCLE-BIN
           END-IF

           PERFORM 0050-START-ERARCH   THRU 0050-EXIT
           PERFORM 0060-READ-ERARCH    THRU 0060-EXIT

           .
       0020-EXIT.
           EXIT.

       0025-SELECT-RECS.

           IF LA-FORM-A3 = '9999'
              GO TO 0025-CONTINUE
           END-IF
011013
011013     IF LA-TEMP
011013        GO TO 0025-CONTINUE
011013     END-IF

           IF WS-CURR-CYCLE-BIN = LA-INITIAL-PRINT-DATE OR LA-SENT-DATE
              OR LA-PURGED-DATE OR LA-VOIDED-DATE OR LA-FINAL-ACT-DATE
              OR LA-REPLY-DATE
              MOVE 'A'                 TO SRT-REC-TYPE
              MOVE LETTER-ARCHIVE      TO SRT-ARCH-REC
              DISPLAY ' ABOUT TO RELEASE A ' LA-CERT-NO-A2
              RELEASE SRT-REC
              ADD 1                    TO RELEASE-CNT
           END-IF

           IF LA-STATUS = 'A'
           IF (LA-FINAL-ACT-DATE > WS-CURR-CYCLE-BIN
              AND <= WS-NEXT-CYCLE-BIN)
                        OR
              (LA-RESEND-DATE > WS-CURR-CYCLE-BIN
              AND <= WS-NEXT-CYCLE-BIN)
              MOVE 'B'              TO SRT-REC-TYPE
              MOVE LETTER-ARCHIVE   TO SRT-ARCH-REC
              DISPLAY ' ABOUT TO RELEASE B ' LA-CERT-NO-A2
              RELEASE SRT-REC
              ADD 1                 TO RELEASE-CNT
           END-IF
           END-IF

           .
       0025-CONTINUE.

           PERFORM 0060-READ-ERARCH    THRU 0060-EXIT

           .
       0025-EXIT.
           EXIT.

       0050-START-ERARCH.

           MOVE LOW-VALUES             TO ERARCH-KEY
           MOVE DTE-CLASIC-COMPANY-CD  TO ERARCH-KEY (1:1)
           START ERARCH KEY >= ERARCH-KEY
           
           IF ERARCH-FILE-STATUS = '10' OR '23'
              SET END-OF-ERARCH        TO TRUE
           ELSE
              IF ERARCH-FILE-STATUS NOT = '00'
                 MOVE ' ERROR ON ERARCH  START     '
                                       TO WS-ABEND-MESSAGE
                 MOVE ERARCH-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS      
                 DISPLAY WS-ABEND-MESSAGE '  ' WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF
           
           .
       0050-EXIT.
           EXIT.

       0060-READ-ERARCH.

           READ ERARCH NEXT RECORD INTO LETTER-ARCHIVE

           IF (ERARCH-FILE-STATUS = '10' OR '23')
              OR (LA-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-ERARCH        TO TRUE
           ELSE
              IF ERARCH-FILE-STATUS NOT = '00'
                 MOVE ' ERROR ON ERARCH READ NEXT  '
                                       TO WS-ABEND-MESSAGE
                 MOVE ERARCH-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS      
                 DISPLAY WS-ABEND-MESSAGE '  ' WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              ELSE
                 ADD +1                TO ERARCH-IN-CNT
              END-IF
           END-IF
           
           .
       0060-EXIT.
           EXIT.

       0070-CLOSE-ERARCH.

           CLOSE ERARCH

           IF ERARCH-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON ERARCH CLOSE '
                 ERARCH-FILE-STATUS
              MOVE ' ERROR ON ERARCH CLOSE '
                                       TO WS-ABEND-MESSAGE
              MOVE ERARCH-FILE-STATUS  TO WS-ABEND-FILE-STATUS      
              PERFORM ABEND-PGM
           END-IF

           .
       0070-EXIT.
           EXIT.

       0100-OUTPUT-RTN.

           DISPLAY ' BEGIN SORT OUTPUT ROUTINE '

           PERFORM 0110-OPEN-OUTPUT    THRU 0110-EXIT

           PERFORM 0120-INITIALIZE     THRU 0120-EXIT

           PERFORM 0130-PROCESS-FILE   THRU 0130-EXIT UNTIL
              END-OF-ERARCH

           PERFORM 0200-CLOSE-OUTPUT   THRU 0200-EXIT

           DISPLAY ' RETURNED RECORDS   ' RETURN-CNT

           .
       0100-EXIT.
           EXIT.

       0110-OPEN-OUTPUT.

           OPEN OUTPUT PRINTX PENDING-EXTRACT ACTIVITY-EXTRACT

           .
       0110-EXIT.
           EXIT.

       0120-INITIALIZE.

           PERFORM 0170-RETURN-ERARCH  THRU 0170-EXIT

           MOVE LA-PROCESSOR-CD        TO WS-SAVE-PROC-ID
                                          HD4-PROC-ID
           MOVE SRT-REC-TYPE           TO WS-PREV-REC-TYPE

           .
       0120-EXIT.
           EXIT.

       0130-PROCESS-FILE.

           IF SRT-REC-TYPE NOT = WS-PREV-REC-TYPE
              MOVE SRT-REC-TYPE        TO WS-PREV-REC-TYPE
              MOVE +60                 TO LNCTR
           END-IF

           IF LA-PROCESSOR-CD NOT = WS-SAVE-PROC-ID
              MOVE LA-PROCESSOR-CD     TO HD4-PROC-ID
                                          WS-SAVE-PROC-ID
              MOVE +60                 TO LNCTR
           END-IF

           PERFORM 0140-BUILD-COMMON   THRU 0140-EXIT

           IF WS-PREV-REC-TYPE = 'A'
              PERFORM 0150-REPORT-A    THRU 0150-EXIT
           ELSE
              PERFORM 0160-REPORT-B    THRU 0160-EXIT
           END-IF

           PERFORM 0170-RETURN-ERARCH  THRU 0170-EXIT

           .
       0130-EXIT.
           EXIT.

       0140-BUILD-COMMON.

           MOVE SPACES                 TO WS-REPORT-WORK

           MOVE LA-CARRIER-A2          TO WS-CARRIER
           MOVE LA-CERT-NO-A2          TO WS-CERT-NO
           MOVE LA-ACCOUNT-A2          TO WS-ACCT-NO
           EVALUATE LA-DATA-SOURCE
              WHEN '1'
                 MOVE 'ACCT'           TO WS-DATA-SOURCE
              WHEN '2'
                 MOVE 'CERT'           TO WS-DATA-SOURCE
011013        WHEN '3'
                 MOVE 'COMP'           TO WS-DATA-SOURCE
011013        WHEN '4'
                 MOVE 'PEND'           TO WS-DATA-SOURCE
              WHEN OTHER
                 MOVE 'ERR '           TO WS-DATA-SOURCE
           END-EVALUATE

           MOVE LA-FORM-A3             TO WS-FORM-TYPE
           MOVE LA-ARCHIVE-NO          TO WS-ARCHIVE-NO
           IF LA-CREATION-DATE NOT = LOW-VALUES AND SPACES
              MOVE LA-CREATION-DATE    TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT TO WS-ORIG-CREATE-DT
              ELSE
                 MOVE ' ERROR  '       TO WS-ORIG-CREATE-DT
              END-IF
           ELSE
              MOVE '********'          TO WS-ORIG-CREATE-DT
           END-IF

           IF LA-INITIAL-PRINT-DATE NOT = LOW-VALUES AND SPACES
              MOVE LA-INITIAL-PRINT-DATE
                                       TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT TO WS-ORIG-PRINT-DT
              ELSE
                 MOVE ' ERROR  '       TO WS-ORIG-PRINT-DT
              END-IF
           ELSE
              MOVE '********'          TO WS-ORIG-PRINT-DT
           END-IF

           MOVE LA-RESEND-LETR         TO WS-RESEND-FORM

           IF LA-RESEND-DATE NOT = LOW-VALUES AND SPACES
              MOVE LA-RESEND-DATE      TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT TO WS-RESEND-SCHED
              ELSE
                 MOVE ' ERROR  '        TO WS-RESEND-SCHED
              END-IF
           ELSE
              MOVE '********'           TO WS-RESEND-SCHED
           END-IF

           IF LA-SENT-DATE NOT = LOW-VALUES AND SPACES
              MOVE LA-SENT-DATE        TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT TO WS-RESENT
              ELSE
                 MOVE ' ERROR  '       TO WS-RESENT
              END-IF
           ELSE
              MOVE '********'          TO WS-RESENT
           END-IF

           IF LA-FINAL-ACT-DATE NOT = LOW-VALUES AND SPACES
              MOVE LA-FINAL-ACT-DATE   TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT TO WS-FINAL-ACTION-DT
              ELSE
                 MOVE ' ERROR  '       TO WS-FINAL-ACTION-DT
              END-IF
           ELSE
              MOVE '********'          TO WS-FINAL-ACTION-DT
           END-IF

           MOVE LA-PROCESSOR-CD        TO WS-CSR

           IF SRT-REC-TYPE = 'A'
              EVALUATE TRUE
                 WHEN LA-REPLY-DATE = WS-CURR-CYCLE-BIN
                    MOVE 'REPLY'          TO WS-TYPE
                 WHEN LA-FINAL-ACT-DATE = WS-CURR-CYCLE-BIN
                    MOVE 'FINAL'          TO WS-TYPE
                 WHEN LA-VOIDED-DATE = WS-CURR-CYCLE-BIN
                    MOVE 'VOIDED'         TO WS-TYPE
                 WHEN LA-PURGED-DATE = WS-CURR-CYCLE-BIN
                    MOVE 'PURGED'         TO WS-TYPE
                 WHEN LA-SENT-DATE = WS-CURR-CYCLE-BIN
                    MOVE 'RESENT'         TO WS-TYPE
                 WHEN LA-INITIAL-PRINT-DATE = WS-CURR-CYCLE-BIN
                    MOVE 'INITIAL'        TO WS-TYPE
              END-EVALUATE
           ELSE
              EVALUATE TRUE
                 WHEN LA-FINAL-ACT-DATE > WS-CURR-CYCLE-BIN
                    AND <= WS-NEXT-CYCLE-BIN
                    MOVE 'FINAL'       TO WS-TYPE

                    MOVE LA-FINAL-ACT-DATE
                                       TO DC-BIN-DATE-1
                    MOVE ' '           TO DC-OPTION-CODE
                    PERFORM 8500-DATE-CONVERT THRU 8590-EXIT
                    IF NO-CONVERSION-ERROR
                       MOVE DC-GREG-DATE-1-EDIT
                                       TO WS-ACTIVITY-DT
                    ELSE
                       MOVE '********' TO WS-ACTIVITY-DT
                    END-IF
                 WHEN LA-RESEND-DATE > WS-CURR-CYCLE-BIN
                    AND <= WS-NEXT-CYCLE-BIN
                    MOVE 'RESEND'      TO WS-TYPE
                    MOVE LA-RESEND-DATE
                                       TO DC-BIN-DATE-1
                    MOVE ' '           TO DC-OPTION-CODE
                    PERFORM 8500-DATE-CONVERT THRU 8590-EXIT
                    IF NO-CONVERSION-ERROR
                       MOVE DC-GREG-DATE-1-EDIT
                                       TO WS-ACTIVITY-DT
                    ELSE
                       MOVE '********' TO WS-ACTIVITY-DT
                    END-IF
              END-EVALUATE
           END-IF

           EVALUATE LA-STATUS
              WHEN 'A'
                 MOVE 'ACTIVE'         TO WS-STATUS
              WHEN 'C'
                 MOVE 'COMPLETE'       TO WS-STATUS
              WHEN 'H'
                 MOVE 'ON HOLD'        TO WS-STATUS
              WHEN 'X'
                 MOVE 'TO BE PURGED'   TO WS-STATUS
              WHEN 'P'
                 MOVE 'PURGED'         TO WS-STATUS
              WHEN 'V'
                 MOVE 'VOIDED'         TO WS-STATUS
              WHEN OTHER
                 MOVE 'INVALID'        TO WS-STATUS
           END-EVALUATE


           .
       0140-EXIT.
           EXIT.

       0150-REPORT-A.

           MOVE SPACES                 TO WS-DETAILA
           MOVE WS-CARRIER             TO WS-DA-CARRIER
           MOVE WS-CERT-NO             TO WS-DA-CERT-NO
           MOVE WS-ACCT-NO             TO WS-DA-ACCT-NO
           MOVE WS-DATA-SOURCE         TO WS-DA-DATA-SOURCE
           MOVE WS-FORM-TYPE           TO WS-DA-FORM-TYPE
           MOVE WS-ARCHIVE-NO-X        TO WS-DA-ARCHIVE-NO-X
           MOVE WS-ORIG-CREATE-DT      TO WS-DA-ORIG-CREATE-DT
           MOVE WS-ORIG-PRINT-DT       TO WS-DA-ORIG-PRINT-DT
           MOVE WS-RESEND-FORM         TO WS-DA-RESEND-FORM
           MOVE WS-RESEND-SCHED        TO WS-DA-RESEND-SCHED
           MOVE WS-RESENT              TO WS-DA-RESENT
           MOVE WS-FINAL-ACTION-DT     TO WS-DA-FINAL-ACTION-DT
           MOVE WS-CSR                 TO WS-DA-CSR
           MOVE WS-TYPE                TO WS-DA-TYPE
           MOVE WS-STATUS              TO WS-DA-STATUS

           MOVE WS-DETAILA             TO P-DATA
           MOVE SPACE-1                TO P-CTL
           PERFORM 8800-PRT-RTN        THRU 8800-EXIT

           .
       0150-EXIT.
           EXIT.


       0160-REPORT-B.

           MOVE SPACES                 TO WS-DETAILB
           MOVE WS-CARRIER             TO WS-DB-CARRIER
           MOVE WS-CERT-NO             TO WS-DB-CERT-NO
           MOVE WS-ACCT-NO             TO WS-DB-ACCT-NO
           MOVE WS-DATA-SOURCE         TO WS-DB-DATA-SOURCE
           MOVE WS-FORM-TYPE           TO WS-DB-FORM-TYPE
           MOVE WS-ARCHIVE-NO-X        TO WS-DB-ARCHIVE-NO-X
           MOVE WS-ORIG-CREATE-DT      TO WS-DB-ORIG-CREATE-DT
           MOVE WS-ORIG-PRINT-DT       TO WS-DB-ORIG-PRINT-DT

           MOVE WS-RESEND-SCHED        TO WS-DB-RESEND-SCHED
           MOVE WS-RESENT              TO WS-DB-RESENT
           MOVE WS-FINAL-ACTION-DT     TO WS-DB-FINAL-ACTION-DT
           MOVE WS-CSR                 TO WS-DB-CSR
           MOVE WS-TYPE                TO WS-DB-TYPE
           MOVE WS-STATUS              TO WS-DB-STATUS
           MOVE WS-ACTIVITY-DT         TO WS-DB-ACTIVITY-DT

           MOVE WS-DETAILB             TO P-DATA
           MOVE SPACE-1                TO P-CTL
           PERFORM 8800-PRT-RTN        THRU 8800-EXIT

           .
       0160-EXIT.
           EXIT.

       0170-RETURN-ERARCH.
       
           RETURN SORTFL AT END
              SET END-OF-ERARCH        TO TRUE
           END-RETURN

           IF NOT END-OF-ERARCH
              MOVE SRT-ARCH-REC        TO LETTER-ARCHIVE
              ADD 1                    TO RETURN-CNT
           END-IF

           .
       0170-EXIT.
           EXIT.

       0200-CLOSE-OUTPUT.

           CLOSE PRINTX  PENDING-EXTRACT ACTIVITY-EXTRACT

           .
       0200-EXIT.
           EXIT.

       8600-HD-RTN.
           MOVE +0                     TO LNCTR
           IF WS-PREV-REC-TYPE = 'A'
              MOVE HDA-1               TO P-DATA
           ELSE
              MOVE HDB-1               TO P-DATA
           END-IF
           MOVE SPACE-NP               TO P-CTL
                                                
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT
                                                
           ADD +1                      TO PGCTR
                                                
           MOVE PGCTR                  TO HD-PG
           MOVE HD2                    TO P-DATA
           MOVE SPACE-1                TO P-CTL
                                                
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT
                                                
           MOVE HD3                    TO P-DATA
           MOVE SPACE-1                TO P-CTL
                                                
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT

           MOVE HD4                    TO P-DATA
           MOVE SPACE-1                TO P-CTL
                                                
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT

           IF WS-PREV-REC-TYPE = 'A'
              MOVE HDA-5               TO P-DATA
           ELSE
              MOVE HDB-5               TO P-DATA
           END-IF
           MOVE SPACE-2                TO P-CTL
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT
                                                
           IF WS-PREV-REC-TYPE = 'A'
              MOVE HDA-6               TO P-DATA
           ELSE
              MOVE HDB-6               TO P-DATA
           END-IF
           MOVE SPACE-1                TO P-CTL
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT
                                                
           MOVE SPACES                 TO P-DATA
           MOVE SPACE-1                TO P-CTL
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT
                                                
           MOVE +10                    TO LNCTR
      *    MOVE SPACE-2                TO P-CTL

           .
       8699-EXIT.
           EXIT.

       8800-PRT-RTN.

           IF LNCTR > +56
              MOVE PRT                 TO WS-HOLD-PRT
              PERFORM 8600-HD-RTN      THRU 8699-EXIT
              MOVE WS-HOLD-PRT         TO PRT
           END-IF

           MOVE P-CTL                  TO X
           IF P-CTL = SPACE-1
              ADD +1                   TO LNCTR
           ELSE
              IF P-CTL = SPACE-2
                 ADD +2                TO LNCTR
              ELSE
                 IF P-CTL = SPACE-3
                    ADD +3             TO LNCTR
                 END-IF
              END-IF
           END-IF
            
           .
       8850-COPY-PRT-RTN.
                                       COPY ELCPRT2.
                         
       8800-EXIT.        
           EXIT.         

       8500-DATE-CONVERT.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.

