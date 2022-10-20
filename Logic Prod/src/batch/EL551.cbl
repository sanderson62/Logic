       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EL551.
       AUTHOR.        PABLO.
       DATE-COMPILED.

      *REMARKS.
      *    THIS PROGRAM READS THE LETTER ARCHIVE FILE AND CREATES
      *    A REPORT OF ALL LETTERS THAT HAVE A FOLLOW UP DATE IN
      *    THE NEXT 5 DAYS.
            
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

           SELECT LETTER-EXTRACT       ASSIGN TO SYS012
              ORGANIZATION IS LINE SEQUENTIAL.
                                                                        
       DATA DIVISION.
       FILE SECTION.


       EJECT
       FD  DISK-DATE
                                       COPY ELCDTEFD.
           EJECT
       FD  PRINTX
                                       COPY ELCPRTFD.
           EJECT
       FD  FICH
                                       COPY ELCFCHFD.
       EJECT
       FD  ERARCH.

       01  ERARCH-IN-RECORD.
           12  FILLER                  PIC XX.
           12  ERARCH-KEY              PIC X(5).
           12  FILLER                  PIC X(243).
           
       SD  SORTFL.

       01  SRT-REC.
           12  FILLER                  PIC X(73).
           12  SRT-PROC-ID             PIC X(4).
           12  FILLER                  PIC X(173).

       FD  LETTER-EXTRACT                                              
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  LETTER-RECORD               PIC X(250).                              
                                                                        
       EJECT 
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '      EL551   WORKING STORAGE   '.
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

       01  HD1.
           12  FILLER              PIC  X(44)      VALUE SPACES.
           12  FILLER              PIC  X(44)      VALUE
                   '     CORRESPONDENCE FOLLOW-UP REPORT       '.
           12  FILLER              PIC  X(31)      VALUE SPACES.
           12  FILLER              PIC  X(08)      VALUE 'EL551  '.

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

       01  HD3-5.
           12  FILLER              PIC X(15)   VALUE SPACES.
           12  FILLER              PIC X(15)   VALUE 'PROCESSOR ID - '.
           12  HD3-5-PROC-ID       PIC X(4)    VALUE SPACES.
           
       01  HD4.
           12  FILLER                  PIC X(133)  VALUE                
               '      ACCT       CERT            FORM  ARCHIVE    LETTER
      -        '    SCHEDULED                '.                            
                                                                        
       01  HD5.
           12  FILLER                  PIC X(133)  VALUE                
               ' CAR  NUMBER    NUMBER     SRCE  TYPE  NUMBER    CREATE 
      -        'DT   SUB REQ   BY   TYPE        STATUS'.
                                                                        

       01  WS-DETAIL                   PIC X(133)  VALUE SPACES.        
                                                                        
       01  WS-DETAIL1               REDEFINES                    
           WS-DETAIL.                                                  
           12  FILLER                  PIC XX.                          
           12  WS-B1-CARRIER           PIC X.                           
           12  FILLER                  PIC X.                           
           12  WS-B1-ACCOUNT-NO        PIC X(10).                       
           12  FILLER                  PIC X.                           
           12  WS-B1-CERT-NO           PIC X(11).                       
           12  FILLER                  PIC XX.                          
           12  WS-B1-DATA-SOURCE       PIC X(4).                        
           12  FILLER                  PIC XX.                          
           12  WS-B1-FORM-TYPE         PIC X(4).                        
           12  FILLER                  PIC X.                           
           12  WS-B1-ARCHIVE-NO        PIC Z(7)9.                       
           12  WS-B1-ARCHIVE-NO-X          REDEFINES                    
               WS-B1-ARCHIVE-NO        PIC X(8).                        
           12  FILLER                  PIC XX.                          
           12  WS-B1-ORIG-SEND-DATE    PIC X(8).                        
           12  WS-B1-NOT-PRINTED-FLAG  PIC X.                           
           12  FILLER                  PIC XX.                          
           12  WS-B1-SCHEDULED-RESEND-DATE PIC X(8).                    
           12  FILLER                  PIC XX.                          
           12  WS-B1-BY                PIC X(4).                        
           12  FILLER                  PIC XX.                          
           12  WS-B1-TYPE              PIC X(10).
           12  FILLER                  PIC XX.
           12  WS-B1-STATUS            PIC X(10).
           12  FILLER                  PIC X(50).                       
                                                                        
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

           EJECT


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

       01  WS-EXTRACT-RECORD.
                                       COPY FNC022.

       01  DATE-AREAS.
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

       PROCEDURE DIVISION.

       0001-DT-CRD-READ.
                                       COPY ELCDTERX.

           SORT SORTFL ON ASCENDING KEY SRT-PROC-ID
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
           ELSE
              DISPLAY ' ERROR CONVERTING CURRENT DATE '
           END-IF

           MOVE COMPANY-NAME           TO HD-CO
           MOVE ALPH-DATE              TO HD-DT
           MOVE WS-CURRENT-DATE        TO HD-RUN-DT

           MOVE RUN-DATE               TO WS-TEST-RUN-DATE

           MOVE WS-CURRENT-BIN-DATE    TO DC-BIN-DATE-1
           MOVE -5                     TO DC-ELAPSED-DAYS
           MOVE ZERO                   TO DC-ELAPSED-MONTHS
           MOVE '6'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           MOVE DC-BIN-DATE-2          TO WS-CURRENT-DATE-MINUS-5

           MOVE WS-CURRENT-BIN-DATE    TO DC-BIN-DATE-1
           MOVE +5                     TO DC-ELAPSED-DAYS
           MOVE ZERO                   TO DC-ELAPSED-MONTHS
           MOVE '6'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           MOVE DC-BIN-DATE-2          TO WS-CURRENT-DATE-PLUS-5

           PERFORM 0050-START-ERARCH   THRU 0050-EXIT
           PERFORM 0060-READ-ERARCH    THRU 0060-EXIT

           .
       0020-EXIT.
           EXIT.

       0025-SELECT-RECS.

           PERFORM 0030-SELECT-RECS    THRU 0030-EXIT

           PERFORM 0060-READ-ERARCH    THRU 0060-EXIT

           .
       0025-EXIT.
           EXIT.

       0030-SELECT-RECS.

           IF (LA-FORM-A3 = '9999')
                        OR
              (LA-STATUS = 'C' OR 'V')
                       OR
              (LA-REPLY-DATE NOT = SPACES AND LOW-VALUES)
              GO TO 0030-EXIT
           END-IF

           IF (LA-INITIAL-PRINT-DATE = LOW-VALUES)
                           OR
              ((LA-FOLLOW-UP-DATE >= WS-CURRENT-DATE-MINUS-5)
              AND (LA-FOLLOW-UP-DATE <= WS-CURRENT-DATE-PLUS-5))
                           OR
              ((LA-SENT-DATE = LOW-VALUES)
               AND (LA-RESEND-DATE > LOW-VALUES)
               AND (LA-RESEND-DATE <= WS-CURRENT-DATE-PLUS-5))
               PERFORM 0040-RELEASE     THRU 0040-EXIT
           END-IF

           .
       0030-EXIT.
           EXIT.

       0040-RELEASE.

           IF LA-FORM-A3 (1:3) = 'EL0'
              CONTINUE
           ELSE
              MOVE 'KRHA'              TO LA-PROCESSOR-CD
           END-IF
           RELEASE SRT-REC             FROM LETTER-ARCHIVE
           ADD 1                       TO RELEASE-CNT

           .
       0040-EXIT.
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

           DISPLAY ' BEGIN SORT INPUT ROUTINE '

           PERFORM 0110-OPEN-OUTPUT    THRU 0110-EXIT

           PERFORM 0120-INITIALIZE     THRU 0120-EXIT

           PERFORM 0130-PROCESS-FILE   THRU 0130-EXIT UNTIL
              END-OF-ERARCH

           PERFORM 0170-CLOSE-OUTPUT   THRU 0170-EXIT

           DISPLAY ' RETURNED RECORDS   ' RETURN-CNT
           .
       0100-EXIT.
           EXIT.

       0110-OPEN-OUTPUT.

           OPEN OUTPUT PRINTX LETTER-EXTRACT

           .
       0110-EXIT.
           EXIT.

       0120-INITIALIZE.

           PERFORM 0140-RETURN-ERARCH  THRU 0140-EXIT

           MOVE LA-PROCESSOR-CD        TO WS-SAVE-PROC-ID
                                          HD3-5-PROC-ID
           .
       0120-EXIT.
           EXIT.

       0130-PROCESS-FILE.

           IF LA-PROCESSOR-CD NOT = WS-SAVE-PROC-ID
              MOVE LA-PROCESSOR-CD     TO HD3-5-PROC-ID
                                          WS-SAVE-PROC-ID
              MOVE +60                 TO LNCTR
           END-IF
           
           IF (LA-FOLLOW-UP-DATE >= WS-CURRENT-DATE-MINUS-5
              AND <= WS-CURRENT-DATE-PLUS-5)
              MOVE 'FOLLOW UP'         TO WS-HOLD-TYPE
              PERFORM 0150-FOLLOW-UP   THRU 0150-EXIT
           END-IF

           IF LA-INITIAL-PRINT-DATE = LOW-VALUES
              MOVE ' INITIAL '         TO WS-HOLD-TYPE
              PERFORM 0160-RESENDS     THRU 0160-EXIT
           END-IF

           IF (LA-SENT-DATE = LOW-VALUES)
              AND (LA-RESEND-DATE > LOW-VALUES)
              DISPLAY ' FOUND ONE ' LA-CERT-NO-A2
              MOVE LA-RESEND-DATE      TO WS-PASS-RESEND-DT
              MOVE ' SUB REQ '         TO WS-HOLD-TYPE
              PERFORM 0160-RESENDS     THRU 0160-EXIT
           END-IF

           PERFORM 0140-RETURN-ERARCH  THRU 0140-EXIT

           .
       0130-EXIT.
           EXIT.

       0140-RETURN-ERARCH.
       
           RETURN SORTFL INTO LETTER-ARCHIVE AT END
              SET END-OF-ERARCH        TO TRUE
           END-RETURN

           IF NOT END-OF-ERARCH
              ADD 1                    TO RETURN-CNT
           END-IF
                      
           .
       0140-EXIT.
           EXIT.
           

       0150-FOLLOW-UP.

           DISPLAY ' MADE IT TO FOLLOW UP'
           MOVE SPACES                 TO WS-DETAIL

           MOVE LA-FOLLOW-UP-DATE      TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO WS-B1-SCHEDULED-RESEND-DATE
           ELSE
              MOVE SPACES              TO WS-B1-SCHEDULED-RESEND-DATE
           END-IF           

           PERFORM 0165-COMMON-FORMAT  THRU 0165-EXIT

           .
       0150-EXIT.
           EXIT.

       0160-RESENDS.  

           DISPLAY ' MADE IT TO RESENDS  '
           MOVE SPACES                 TO WS-DETAIL

           MOVE WS-PASS-RESEND-DT      TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO WS-B1-SCHEDULED-RESEND-DATE
           ELSE
              MOVE SPACES              TO WS-B1-SCHEDULED-RESEND-DATE
           END-IF           

           PERFORM 0165-COMMON-FORMAT  THRU 0165-EXIT

           .
       0160-EXIT.
           EXIT.

       0165-COMMON-FORMAT.

           IF LA-CARRIER-A2 NOT = LOW-VALUES
              MOVE LA-CARRIER-A2       TO WS-B1-CARRIER
           END-IF
           IF LA-ACCOUNT-A2 NOT = LOW-VALUES
              MOVE LA-ACCOUNT-A2       TO WS-B1-ACCOUNT-NO
           END-IF
           IF LA-CERT-NO-A2 NOT = LOW-VALUES
              MOVE LA-CERT-NO-A2       TO WS-B1-CERT-NO
           END-IF

           MOVE LA-FORM-A3             TO WS-B1-FORM-TYPE
           MOVE LA-ARCHIVE-NO          TO WS-B1-ARCHIVE-NO

           MOVE LA-PROCESSOR-CD        TO WS-B1-BY

           MOVE WS-HOLD-TYPE           TO WS-B1-TYPE

           MOVE LA-CREATION-DATE       TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO WS-B1-ORIG-SEND-DATE
           ELSE
              MOVE SPACES              TO WS-B1-ORIG-SEND-DATE
           END-IF           

           IF LA-INITIAL-PRINT-DATE = LOW-VALUES
              MOVE '*'                 TO WS-B1-NOT-PRINTED-FLAG
           ELSE
              MOVE ' '                 TO WS-B1-NOT-PRINTED-FLAG
           END-IF

           EVALUATE LA-DATA-SOURCE
              WHEN '1'
                 MOVE 'ACCT'           TO WS-B1-DATA-SOURCE
              WHEN '2'
                 MOVE 'CERT'           TO WS-B1-DATA-SOURCE
              WHEN '3'
                 MOVE 'COMP'           TO WS-B1-DATA-SOURCE
              WHEN '4'
                 MOVE 'PEND'           TO WS-B1-DATA-SOURCE
              WHEN '5'
                 MOVE 'CHEK'           TO WS-B1-DATA-SOURCE
              WHEN '6'
                 MOVE 'PYAJ'           TO WS-B1-DATA-SOURCE
              WHEN OTHER
                 MOVE 'OTHR'           TO WS-B1-DATA-SOURCE
           END-EVALUATE

           EVALUATE LA-STATUS
              WHEN 'A'
                 MOVE 'ACTIVE'         TO WS-B1-STATUS
              WHEN 'C'
                 MOVE 'COMPLETED'      TO WS-B1-STATUS
              WHEN 'H'
                 MOVE 'ON HOLD'        TO WS-B1-STATUS
              WHEN 'X'
                 MOVE 'TO BE PURGED'   TO WS-B1-STATUS
              WHEN 'P'
                 MOVE 'PURGED'         TO WS-B1-STATUS
              WHEN 'V'
                 MOVE 'VOIDED'         TO WS-B1-STATUS
              WHEN OTHER
                 MOVE 'OTHER'          TO WS-B1-STATUS
           END-EVALUATE

           MOVE WS-DETAIL1              TO P-DATA
           MOVE SPACE-1                 TO P-CTL
           PERFORM 8800-PRT-RTN         THRU 8800-EXIT

           .
       0165-EXIT.
           EXIT.

       0170-CLOSE-OUTPUT.

           CLOSE PRINTX LETTER-EXTRACT

           .
       0170-EXIT.
           EXIT.

       0350-CREATE-EXTRACT.

           DISPLAY ' MADE IT TO 0350 CREATE FREE '

           MOVE SPACES                 TO WS-EXTRACT-RECORD
           MOVE 'LPACDUEPC'            TO FX-SYSTEM
           MOVE 'LOGIC '               TO FX-SOURCE-CODE
           MOVE '11'                   TO FX-DIVISION
           MOVE '15'                   TO FX-SUB-TYPE

           MOVE 'Y'                    TO FX-LOC-CODE
           
           MOVE WS-POSTING-DATE        TO FX-POSTING-DATE                          

           IF DTE-CLIENT = 'CID'
              MOVE 'S'                 TO FX-FY-REN
           ELSE
              MOVE ' '                 TO FX-FY-REN
           END-IF

           MOVE ZEROS                  TO FX-AMOUNT

           .
       0350-EXIT.
           EXIT.

       8600-HD-RTN.
           MOVE +0                     TO LNCTR
           MOVE HD1                    TO  P-DATA
           MOVE SPACE-NP               TO  P-CTL
                                                
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT
                                                
           ADD +1                      TO  PGCTR
                                                
           MOVE PGCTR                  TO  HD-PG
           MOVE HD2                    TO  P-DATA
           MOVE SPACE-1                TO  P-CTL
                                                
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT
                                                
           MOVE HD3                    TO  P-DATA
           MOVE SPACE-1                TO  P-CTL
                                                
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT

           MOVE HD3-5                  TO  P-DATA
           MOVE SPACE-1                TO  P-CTL
                                                
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT

           MOVE SPACE-2                TO  P-CTL
           MOVE HD4                    TO  P-DATA
                                                
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT
                                                
           MOVE HD5                    TO  P-DATA
           MOVE SPACE-1                TO  P-CTL
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT
                                                
           MOVE SPACES                 TO  P-DATA
           MOVE SPACE-1                TO  P-CTL
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT
                                                
           MOVE +10                    TO  LNCTR
      *    MOVE SPACE-2                TO  P-CTL

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

