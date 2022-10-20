       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.       EL694B.
      *AUTHOR.           PABLO.
      *                  COLLEYVILLE, TX.                                  CL**6
      *DATE-COMPILED.                                                      CL**6
      *SECURITY.   *****************************************************   CL**6
      *            *                                                   *   CL**6
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO.            *   CL**7
      *            *                                                   *   CL**6
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**6
      *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *   CL**6
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO             *   CL**6
      *            *                                                   *   CL**6
      *            *****************************************************   CL**6
      *REMARKS. TRANSACTION EXM5 - LETTER PRINTER                       EL6942
      *        THIS PROGRAM IS USED TO PRINT THE STORED LETTERS AND     EL6942
      *        LABELS  DEPENDING ON THE VALUE OF THE PI-ENTRY-CODES.    EL6942
                                                                        EL6942
      *        PRINT INITIAL LETTERS   CODE-1 = 1                       EL6942
      *                                CODE-2 = 1                       EL6942
                                                                        EL6942
      *        PRINT FOLLOW-UP LETTERS CODE-1 = 1                       EL6942
      *                                CODE-2 = 2                       EL6942
                                                                        EL6942
      *        RE-PRINT LETTERS        CODE-1 = 0                       EL6942
      *                                CODE-2 = 3                       EL6942
                                                                        EL6942
      *        PRINT ADDRESS LABELS    CODE-1 = 0                       EL6942
      *                                CODE-2 = 2                       EL6942
                                       EJECT                            EL6942
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 072308    2007110500003  PEMA  NEW PROGRAM
      ******************************************************************
       ENVIRONMENT DIVISION.                                            EL6942
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    

           SELECT PRNTR          ASSIGN TO SYS008-UR-1403-S-SYS008.     

           SELECT DISK-DATE      ASSIGN TO SYS019-FBA1-S-SYS019.        
      
           SELECT LETTER-SORT    ASSIGN TO SYS020-UR-1403-S-SYS020.

           SELECT ERARCH         ASSIGN TO ERARCH
                                    ORGANIZATION INDEXED                
                                    ACCESS       DYNAMIC                
                                    RECORD KEY   LA-CONTROL-PRIMARY     
                                    FILE STATUS  ERARCH-FILE-STATUS.
                                                                        
           SELECT ERARCT          ASSIGN TO ERARCT
                                    ORGANIZATION INDEXED                
                                    ACCESS       DYNAMIC                
                                    RECORD KEY   LT-CONTROL-PRIMARY     
                                    FILE STATUS  ERARCT-FILE-STATUS.    
                                                                        
           SELECT ELCNTL          ASSIGN TO ELCNTL
                                    ORGANIZATION INDEXED                
                                    ACCESS       DYNAMIC                
                                    RECORD KEY   CF-CONTROL-PRIMARY     
                                    FILE STATUS  ELCNTL-FILE-STATUS.    

       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD  PRNTR
           RECORDING MODE F
           LABEL RECORDS OMITTED
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  PRT.
           12  P-CTL               PIC  X.
           12  P-DATA              PIC  X(79).
                                                                        
       FD  DISK-DATE                   COPY ELCDTEFD.                   

      
       FD  LETTER-SORT
           RECORDING MODE F
           LABEL RECORDS OMITTED
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.
       01  LETTER-SORT-REC             PIC X(80).
      
       FD  ERARCH.                                                      
                                       COPY ERCARCH.                    

       FD  ERARCT.                                                      
                                       COPY ERCARCT.                    

       FD  ELCNTL.                                                      
                                       COPY ELCCNTL.                    

       WORKING-STORAGE SECTION.
       77  FILLER  PIC  X(32) VALUE '********************************'.
       77  FILLER  PIC  X(32) VALUE '*   EL694B WORKING STORAGE     *'.
       77  FILLER  PIC  X(32) VALUE '********* V/M 2.001 ************'.

       77  WS-EOF-SW                   PIC X   VALUE SPACES.
           88  END-OF-INPUT                    VALUE 'Y'.
       77  WS-LETTER-SW                PIC X   VALUE SPACES.
           88  END-OF-LETTER                   VALUE 'Y'.
       77  ERARCH-FILE-STATUS          PIC XX     VALUE LOW-VALUES.
       77  ERARCT-FILE-STATUS          PIC XX     VALUE LOW-VALUES.
       77  ELCNTL-FILE-STATUS          PIC XX     VALUE LOW-VALUES.
       77  PGM-SUB                     PIC S9(4)  VALUE +311   COMP.
       77  ABEND-CODE                  PIC X(4)   VALUE SPACE.
       77  ABEND-OPTION                PIC X      VALUE SPACE.
       77  WS-RETURN-CODE              PIC S9(3)  VALUE ZEROS.
       77  WS-ABEND-MESSAGE            PIC X(80)  VALUE SPACES.
       77  WS-ABEND-FILE-STATUS        PIC XX     VALUE ZEROS.
       77  WS-PRTLINECOUNTER           PIC S9(3)  VALUE +0 COMP-3.
       77  WS-ZERO                     PIC S9(3)  VALUE +0 COMP-3.
       77  WS-NUM-LINES                PIC 9(5)   VALUE ZEROS.

       01  W-PROGRAM-CONSTANTS.
           12  FILLER                  PIC  X(18)                       EL6942
                                       VALUE 'PROGRAM CONSTANTS:'.      EL6942
                                                                        EL6942
           12  W-ZEROS                 PIC S9(04)  COMP VALUE +0.       EL6942

       01  W-PROGRAM-WORK-AREA.                                         EL6942
           12  THIS-PGM                PIC  X(8)  VALUE 'EL6942'.          CL**7
           12  FILLER                  PIC  X(18)                       EL6942
                                       VALUE 'PROGRAM WORK AREA:'.      EL6942
                                                                        EL6942
           12  W-ASKTIME-CTR           PIC S9(04)  COMP.                EL6942
           12  W-ARCHIVE-SAVE          PIC S9(08)  COMP.                EL6942
           12  W-COPIES                PIC  9.                          EL6942
           12  W-DELAY-INTERVAL        PIC S9(07)  COMP-3 VALUE +2.     EL6942
           12  W-NDX                   PIC S9(04)  COMP   VALUE +0.     EL6942
           12  W-NUM-OF-TEXT-RECORDS   PIC S9(04)  COMP   VALUE +0.     EL6942
           12  W-NUMBER-OF-LINES       PIC S9(04)  COMP.                EL6942
           12  W-RECORD-COUNT          PIC S9(04)         VALUE +0.     EL6942
           12  W-SAVE-ARCH-NO          PIC S9(08)  COMP   VALUE +0.     EL6942
           12  W-SKIP                  PIC  9(02).                      EL6942
           12  W-SUB                   PIC S9(04)  COMP.                EL6942
           12  W-LETTER-TOTALS         PIC  9(07)  COMP-3 VALUE 0.      EL6942
                                                                        EL6942
           12  W-ASTERISK-LINE1.                                           CL**6
               16  FILLER              PIC  X(78)  VALUE ALL '*'.       EL6942
           12  W-ASTERISK-LINE.                                         EL6942
               16  FILLER              PIC  X(01)  VALUE SPACES.        EL6942
               16  FILLER              PIC  X(78)  VALUE ALL '*'.       EL6942
           12  W-CALL-PGM              PIC  X(08).                      EL6942
           12  W-CURRENT-SAVE          PIC  X(02).                      EL6942
           12  W-ERROR-LINE            PIC  X(80).                      EL6942
           12  W-LAST-RESENT-PRINT-DATE                                 EL6942
                                       PIC  X(02)  VALUE SPACES.        EL6942
                                                                        EL6942
           12  W-LABEL-HOLD-AREA.                                       EL6942
               16  W-LABEL-LINES OCCURS 6 TIMES INDEXED BY W-L-NDX.     EL6942
                   20  W-LABEL-ZIP.                                     EL6942
                       24  W-LABEL-1ST-ZIP                              EL6942
                                       PIC  X(04).                      EL6942
                       24  W-LABEL-2ND-ZIP                              EL6942
                                       PIC  X(05).                      EL6942
                   20  FILLER          PIC  X(12).                      EL6942
                   20  WS-LAST-ZIP.                                     EL6942
                       24  WS-LAST-1ST-ZIP                              EL6942
                                       PIC  X(04).                      EL6942
                       24  WS-LAST-2ND-ZIP                              EL6942
                                       PIC  X(05).                      EL6942
                                                                        EL6942
           12  W-SAVE-CURRENT-DATE     PIC  X(08)  VALUE SPACES.        EL6942
           12  W-SAVE-CURRENT-BIN-DATE PIC  X(02)  VALUE SPACES.        EL6942
           12  W-SAVE-LETTER-ARCHIVE   PIC X(250)  VALUE SPACES.        EL6942
           12  W-TOTAL-LINE.                                            EL6942
               20  FILLER              PIC  X(01)  VALUE SPACES.        EL6942
               20  FILLER              PIC  X(20)                       EL6942
                   VALUE 'PROCESS COMPLETED.  '.                        EL6942
               20  W-TOTAL-LINE-DESC   PIC  X(26)                          CL**3
                   VALUE 'LETTERS PRINTED TOTAL   - '.                     CL**3
               20  W-TOTAL-LETTERS     PIC Z,ZZZ,ZZ9.                   EL6942
           12  W-WORKING-RESEND-DATE   PIC  X(02)  VALUE SPACES.        EL6942
           12  W-LABEL-LINE-DESC   PIC  X(26)                              CL**3
                   VALUE 'LABELS PRINTED TOTAL    - '.                     CL**3
                                                                        EL6942
       01  W-PROGRAM-KEYS.                                              EL6942
           12  FILLER                  PIC  X(13)                       EL6942
                                       VALUE 'PROGRAM KEYS:'.           EL6942
           12  W-ARCH-KEY.                                              EL6942
               20  W-ARCH-COMPANY-CD   PIC  X(01).                      EL6942
               20  W-ARCH-NUMBER       PIC S9(08)     COMP.             EL6942
                                                                        EL6942
           12  W-ARCT-KEY.                                              EL6942
               16  W-ARCT-PARTIAL-KEY.                                  EL6942
                   20  W-ARCT-COMPANY-CD                                EL6942
                                       PIC  X(01).                      EL6942
                   20  W-ARCT-NUMBER   PIC S9(08)     COMP.             EL6942
               16  W-ARCT-REC-ID       PIC  X(01).                      EL6942
               16  W-ARCT-SEQ          PIC S9(04)     COMP VALUE +0.    EL6942
                                                                        EL6942
           12  W-CNTL-KEY.                                              EL6942
               16  W-CNTL-COMPANY-ID   PIC  X(03).                      EL6942
               16  W-CNTL-RECORD-TYPE  PIC  X(01)  VALUE '1'.           EL6942
               16  W-CNTL-GENL.                                         EL6942
                   20  W-CNTL-GEN1     PIC  X(02)  VALUE SPACES.        EL6942
                   20  W-CNTL-GEN2.                                     EL6942
                       24 W-CNTL-GEN3  PIC  X(01)  VALUE SPACES.        EL6942
                       24 W-CNTL-GEN4  PIC  X(01)  VALUE SPACES.        EL6942
               16  W-CNTL-SEQ          PIC S9(04)  VALUE +0    COMP.    EL6942
                                                                        EL6942
       01  W-PROGRAM-SWITCES.                                           EL6942
           12  FILLER                  PIC  X(17)                       EL6942
                                       VALUE 'PROGRAM SWITCHES:'.       EL6942
                                                                        EL6942
           12  W-ENDBR-SW              PIC  X(01)          VALUE ' '.   EL6942
               88  W-ENDBR                                 VALUE 'Y'.   EL6942
           12  W-FIRST-FORM-SW         PIC  X(01)          VALUE ' '.   EL6942
               88  W-THIS-IS-FIRST-FORM                    VALUE ' '.   EL6942
               88  W-THIS-IS-NOT-FIRST-FORM                VALUE 'Y'.   EL6942
           12  W-HEADER-BROWSE-STARTED PIC  X(01)          VALUE 'N'.   EL6942
           12  W-HEADER-SW             PIC  X(01)          VALUE SPACE. EL6942
               88  W-HEADER-REC-FOUND                      VALUE SPACE. EL6942
           12  W-PRINT-SW              PIC S9(01) COMP-3   VALUE ZERO.  EL6942
           12  W-PROCESSING-SW         PIC S9(01) COMP-3   VALUE ZERO.  EL6942
               88  W-PROCESS-BY-KEY                        VALUE +3.    EL6942
           12  W-TEXT-BROWSE-STARTED   PIC  X(01)          VALUE 'N'.   EL6942
           12  W-TOP-FORM-SW           PIC  X(01)          VALUE SPACE. EL6942
               88  W-TOP-FORM-SET                          VALUE 'T'.   EL6942
           12  W-OPTION-CODES          PIC  X(02).                      EL6942
               88  W-PRINT-INITIAL                         VALUE '11'.  EL6942
               88  W-PRINT-FOLLOW-UP                       VALUE '12'.  EL6942
               88  W-PRINT-LABELS                          VALUE ' 2'.  EL6942
               88  W-REPRINT-LETTERS                       VALUE ' 3'.  EL6942
                                       EJECT                            EL6942
       01  FILLER                      PIC  X(25)                       EL6942
                                   VALUE 'PROGRAM INTERFACE STARTS:'.   EL6942
                                       COPY ELCINTF.                    EL6942
           12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.                    EL6942
      **********************************************************        EL6942
               16  PI-6942-ALIGNMENT-COPIES                             EL6942
                                       PIC S9(01) COMP-3.               EL6942
               16  PI-6942-PRINT-DATE  PIC  X(08).                      EL6942
               16  PI-6942-PRINT-DATE-BIN                               EL6942
                                       PIC  X(02).                      EL6942
               16  PI-6942-PRINT-BY-KEY-IND                             EL6942
                                       PIC  X(01).                      EL6942
                   88  PI-6942-PRINT-BY-KEY      VALUE 'C' 'G' 'S' 'A'. EL6942
                   88  PI-6942-PRINT-BY-CARRIER  VALUE 'C'.             EL6942
                   88  PI-6942-PRINT-BY-GROUPING VALUE 'G'.             EL6942
                   88  PI-6942-PRINT-BY-STATE    VALUE 'S'.             EL6942
                   88  PI-6942-PRINT-BY-ACCOUNT  VALUE 'A'.             EL6942
               16  PI-6942-PRINT-BY-PROCESSOR-IND                       EL6942
                                       PIC  X(01).                      EL6942
                   88  PI-6942-PRINT-BY-PROCESSOR                       EL6942
                                                 VALUE 'Y'.             EL6942
               16  PI-6942-PRINT-ID    PIC  X(04).                      EL6942
               16  PI-6942-PRINT-KEY.                                   EL6942
                   20  PI-6942-PRINT-CARRIER                            EL6942
                                       PIC  X(01).                      EL6942
                   20  PI-6942-PRINT-GROUPING                           EL6942
                                       PIC  X(06).                      EL6942
                   20  PI-6942-PRINT-STATE                              EL6942
                                       PIC  X(02).                      EL6942
                   20  PI-6942-PRINT-ACCOUNT                            EL6942
                                       PIC  X(10).                      EL6942
               16  PI-6942-PRINT-PROCESSOR                              EL6942
                                       PIC  X(04).                      EL6942
               16  PI-6942-LETTER-FORM PIC  X(04).                      EL6942
               16  PI-6942-LETTER-TYPE PIC  X(01).                      EL6942
               16  PI-6942-STARTING-ARCH-NO                             EL6942
                                       PIC S9(08) COMP.                 EL6942
               16  PI-6942-ENTRY.                                       EL6942
                   20  PI-6942-FILLER  PIC  X(02).                      EL6942
                   20  PI-6942-QUE-CONTROL                              EL6942
                                       PIC S9(08) COMP.                 EL6942
               16  FILLER              PIC X(585).                         CL**6
                                       EJECT                            EL6942
           COPY ELPRTCVD.                                               EL6942
       01  FILLER.                                                      EL6942
           16  FILLER                  PIC  X(200)                      EL6942
               VALUE 'THIS IS PART OF THE BUFFER ZONE'.                 EL6942
                                                                        EL6942
                                       EJECT                            EL6942
       01  FILLER                      PIC  X(18)                       EL6942
                                   VALUE 'WORK TABLE STARTS:'.          EL6942
                                                                        EL6942
       01  W-ADJUST-AREA.                                               EL6942
           12  FILLER                  PIC  X(07).                         CL**6
           12  W-AD-PRINT-AREA         PIC  X(70).                      EL6942
           12  FILLER                  PIC  X(03).                      EL6942
                                                                        EL6942
       01  W-WORK-TABLE.                                                EL6942
           12  W-WORK-LINE OCCURS 300 TIMES                             EL6942
                                INDEXED BY W-WK-NDX.                    EL6942
               16  W-TEXT-LINE         PIC  X(70).                      EL6942
               16  W-SKIP-CONTROL      PIC  X(02).                      EL6942
                   88  W-NO-LINES-SKIPPED            VALUE SPACES.      EL6942
                   88  W-SKIP-TO-NEXT-PAGE           VALUE '99'.        EL6942

                                                                        EL6942
       01  FILLER                      PIC  X(16)                       EL6942
                                   VALUE 'WORK TABLE ENDS:'.            EL6942

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT
           PERFORM 0020-INIT           THRU 0020-EXIT
           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT UNTIL
              END-OF-INPUT

           PERFORM 0200-CLOSE-FILES    THRU 0200-EXIT

           DISPLAY ' '
           DISPLAY ' '

           GOBACK

           .
       0010-OPEN-FILES.

           OPEN I-O ERARCH
                INPUT ERARCT ELCNTL

           OPEN OUTPUT PRNTR LETTER-SORT

           IF ERARCT-FILE-STATUS   = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERARCT - OPEN ' ERARCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERARCH-FILE-STATUS   = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERARCH - OPEN ' ERARCH-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELCNTL-FILE-STATUS   = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ELCNTL - OPEN ' ELCNTL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0010-EXIT.
           EXIT.

       0020-INIT.

           PERFORM 0030-STARTBR-ERARCH THRU 0030-EXIT
           PERFORM 0040-READNXT-ERARCH THRU 0040-EXIT

           .
       0020-EXIT.
           EXIT.

       0030-STARTBR-ERARCH.

           MOVE LOW-VALUES             TO LA-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO LA-COMPANY-CD
           START ERARCH KEY >= LA-CONTROL-PRIMARY
           IF ERARCH-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERARCH - STARTBR ' ERARCH-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0030-EXIT.
           EXIT.

       0040-READNXT-ERARCH.

           READ ERARCH NEXT RECORD
           IF (ERARCH-FILE-STATUS = '10' OR '23')
              OR (LA-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERARCH-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERARCH - READNXT ' ERARCH-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF
           .
       0040-EXIT.
           EXIT.

       0050-PROCESS-INPUT.

           IF (LA-CREATION-DATE NOT = SPACES AND LOW-VALUES)
              AND (LA-INITIAL-PRINT-DATE = LOW-VALUES)
      *       AND (LA-FORM-A3 = 'NH01')
              DISPLAY ' FOUND LETTER TO PROCESS '
                 LA-CONTROL-BY-KEY-FIELDS (2:19) ' '
                 LA-CERT-NO-A2
              PERFORM 0100-PROCESS-LETTER
                                       THRU 0100-EXIT
           END-IF

           PERFORM 0040-READNXT-ERARCH THRU 0040-EXIT

           .
       0050-EXIT.
           EXIT.

01117  0100-PROCESS-LETTER.

           MOVE ' '                    TO WS-LETTER-SW
           MOVE ZEROS                  TO W-NUM-OF-TEXT-RECORDS
           SET W-WK-NDX TO WS-ZERO

           MOVE 2 TO LA-NO-OF-COPIES
           IF LA-NO-OF-COPIES = ZEROS
              MOVE 1                   TO LA-NO-OF-COPIES
           END-IF

           MOVE LA-NO-OF-COPIES        TO W-COPIES

01128      MOVE BIN-RUN-DATE           TO LA-INITIAL-PRINT-DATE

01130      IF LA-RESEND-DATE = LOW-VALUES
01131         MOVE 'C'                 TO LA-STATUS
           END-IF

           PERFORM 0110-STARTBR-ERARCT THRU 0110-EXIT
           PERFORM 0120-READNXT-ERARCT THRU 0120-EXIT

           PERFORM 0130-CREATE-PRINT-TABLES
                                       THRU 0130-EXIT

           PERFORM 0160-PRINT-ARCHIVE-RECORDS
                                       THRU 0160-EXIT
              W-COPIES TIMES

           DISPLAY ' ABOUT TO REWRITE '
           REWRITE LETTER-ARCHIVE

           .
       0100-EXIT.
           EXIT.

       0110-STARTBR-ERARCT.

           MOVE LOW-VALUES             TO LT-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO LT-COMPANY-CD
           MOVE LA-ARCHIVE-NO          TO LT-ARCHIVE-NO

           MOVE '2'                    TO LT-RECORD-TYPE

           START ERARCT KEY >= LT-CONTROL-PRIMARY
           IF ERARCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERARCT - STARTBR ' ERARCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0110-EXIT.
           EXIT.

       0120-READNXT-ERARCT.

           READ ERARCT NEXT RECORD
           IF (ERARCT-FILE-STATUS = '10' OR '23')
              OR (LT-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              OR (LT-ARCHIVE-NO NOT = LA-ARCHIVE-NO)
              SET END-OF-LETTER        TO TRUE
           ELSE
              IF ERARCT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERARCT - READNXT ' ERARCT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0120-EXIT.
           EXIT.

       0130-CREATE-PRINT-TABLES.

           DISPLAY ' MADE IT TO 0130 '
           PERFORM 0140-BUILD          THRU 0140-EXIT UNTIL
              END-OF-LETTER

           .
       0130-EXIT.
           EXIT.

       0140-BUILD.

           MOVE LT-NUM-LINES-ON-RECORD TO WS-NUM-LINES
           DISPLAY ' MADE IT TO 0140, NO OF LINES ' WS-NUM-LINES
           
           PERFORM 0150-PROCESS-RECORD THRU 0150-EXIT VARYING
              LT-NDX FROM +1 BY +1 UNTIL
               LT-NDX > LT-NUM-LINES-ON-RECORD

           PERFORM 0120-READNXT-ERARCT THRU 0120-EXIT

           .
       0140-EXIT.
           EXIT.

       0150-PROCESS-RECORD.

           SET W-WK-NDX UP BY +1
           ADD +1                      TO W-NUM-OF-TEXT-RECORDS
           MOVE LT-LETTER-TEXT (LT-NDX)
                                       TO W-WORK-LINE (W-WK-NDX)
           .
       0150-EXIT.
           EXIT.

       0160-PRINT-ARCHIVE-RECORDS.

           PERFORM 0170-PROCESS-TABLE  THRU 0170-EXIT VARYING
              W-WK-NDX FROM 1 BY 1 UNTIL
              W-WK-NDX > W-NUM-OF-TEXT-RECORDS

           .
02447  0160-EXIT.
02448      EXIT.

02450  0170-PROCESS-TABLE.

02452      IF (W-SKIP-CONTROL (W-WK-NDX) > '00')
02453         AND (W-SKIP-CONTROL (W-WK-NDX) < '99')
02456         MOVE W-SKIP-CONTROL (W-WK-NDX)
02457                                  TO W-SKIP
              PERFORM 0190-PRINT-BLANK-LINES
                                       THRU 0190-EXIT W-SKIP TIMES
02459      ELSE
02460         IF W-SKIP-CONTROL (W-WK-NDX) = '99'
      *          MOVE '1'              TO PRT
      *          MOVE SPACES           TO P-DATA
                 MOVE '1'              TO P-CTL
                 MOVE W-TEXT-LINE (W-WK-NDX) TO P-DATA
                 WRITE PRT AFTER ADVANCING PAGE
                 GO TO 0170-EXIT
      *          WRITE PRT
              END-IF
           END-IF

           MOVE SPACES TO P-DATA
           MOVE W-TEXT-LINE (W-WK-NDX) TO P-DATA (7:70)
           MOVE ' '                    TO P-CTL
      *    MOVE W-TEXT-LINE (W-WK-NDX) TO PRT
           PERFORM 0180-PRINT          THRU 0180-EXIT

           .
       0170-EXIT.
           EXIT.

       0180-PRINT.

      *    WRITE PRT
           WRITE PRT                   AFTER ADVANCING 1 LINES
           ADD 1                       TO WS-PRTLINECOUNTER

           .
       0180-EXIT.
           EXIT.


       0190-PRINT-BLANK-LINES.

           MOVE SPACES                 TO PRT
      *    WRITE PRT
           WRITE PRT                   AFTER ADVANCING 1 LINES
           ADD 1                       TO WS-PRTLINECOUNTER

           .
       0190-EXIT.
           EXIT.

       0200-CLOSE-FILES.

           CLOSE ERARCH ERARCT ELCNTL LETTER-SORT PRNTR

           .
       0200-EXIT.
           EXIT.

       ABEND-PGM SECTION.              COPY ELCABEND.                      

       8500-DATE-CONVERT.

           CALL 'ELDATCX'              USING DATE-CONVERSION-DATA

           .
       8500-EXIT.
           EXIT.
