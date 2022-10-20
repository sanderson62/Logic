       IDENTIFICATION DIVISION.
       PROGRAM-ID. EL309F.
       AUTHOR.     PABLO.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT HISTORY-INPUT-FILE                                    EL309
               ASSIGN TO SYS011-UT-2400-S-SYS011.                       EL309
                                                                        EL309
           SELECT HISTORY-OUTPUT-FILE                                   EL309
               ASSIGN TO SYS012-UT-2400-S-SYS012.                       EL309
                                                                        EL309
           EJECT                                                        EL309
       DATA DIVISION.                                                   EL309
                                                                        EL309
       FILE SECTION.                                                    EL309
                                                                        EL309
       FD  HISTORY-INPUT-FILE          COPY ELCHAF.                     EL309
                                                                        EL309
           EJECT                                                        EL309
       FD  HISTORY-OUTPUT-FILE         COPY ELCHAF REPLACING            EL309
           HISTORY-INPUT-RECORD        BY  HISTORY-OUTPUT-RECORD        EL309
           HISTORY-INPUT-CLAIM-RECORD  BY  HISTORY-OUTPUT-CLAIM-RECORD  EL309
           HISTORY-INPUT-CERT-RECORD   BY  HISTORY-OUTPUT-CERT-RECORD   EL309
           HISTORY-INPUT-TRAILER-RECORD BY HISTORY-OUTPUT-TRAILER-RECORDEL309
           HISTORY-INPUT-LETTER-RECORD BY  HISTORY-OUTPUT-LETTER-RECORD EL309
           HIR-COMPANY-ID              BY  HOR-COMPANY-ID               EL309
           HIR-CLAIM-KEY               BY  HOR-CLAIM-KEY                EL309
           HIR-DATE-ARCHIVED           BY  HOR-DATE-ARCHIVED            EL309
           HIR-PURGED-CLAIM            BY  HOR-PURGED-CLAIM             EL309
           HIR-RECORD-ID               BY  HOR-RECORD-ID                EL309
           HIR-CLAIM-RECORD            BY  HOR-CLAIM-RECORD             EL309
           HIR-CERTIFICATE-RECORD      BY  HOR-CERTIFICATE-RECORD       EL309
           HIR-ACTIVITY-TRAILER-RECORD BY  HOR-ACTIVITY-TRAILER-RECORD  EL309
           HIR-LETTER-ARCHIVE-RECORD   BY  HOR-LETTER-ARCHIVE-RECORD.   EL309
                                                                        EL309
       01  HISTORY-OUTPUT-RECORD2          PIC X(476).                  EL309
      *01  HISTORY-OUTPUT-RECORD2          PIC X(1226).                 EL309
                                                                        EL309
           EJECT                                                        EL309
       WORKING-STORAGE SECTION.                                         EL309
       77  FILLER  PIC X(32)   VALUE '********************************'.EL309
       77  FILLER  PIC X(32)   VALUE '*     EL309F WORKING STORAGE   *'.EL309
       77  FILLER   PIC X(32) VALUE  '******** VMOD=2.001 ************'.EL309
                                                                        EL309
       01  FILLER                          COMP-3.                      EL309
           05  WS-LINE-COUNT               PIC S9(3)   VALUE +99.       EL309
           05  WS-LINE-COUNT-MAX           PIC S9(3)   VALUE +60.       EL309
           05  WS-PAGE                     PIC S9(5)   VALUE ZERO.      EL309
           05  WS-REPORT-SW                PIC S9      VALUE +1.        EL309
           05  WS-RECORD-COUNT             PIC S9(9)   VALUE ZERO.      EL309
           05  WS-RETURN-CODE              PIC S9(3)   VALUE ZERO.      EL309
           05  WS-ZERO                     PIC S9      VALUE ZERO.      EL309
           05  WS-BYPASS-SW                PIC S9      VALUE ZERO.      EL309
           05  WS-DUP-SW                   PIC S9      VALUE ZERO.      EL309
           05  WS-CURRENT-TIME             PIC S9(7)   VALUE ZERO.      EL309
                                                                        EL309
           05  WS-WORK                     PIC S9(7)   VALUE ZERO.      EL309
                                                                        EL309
           05  WS-HISTORY-FILE-OPEN        PIC S9      VALUE +0.
               88  END-OF-HISTORY-FILE                 VALUE +1.
                                                                        EL309
           05  WS-HISTORY-INPUT-COUNT      PIC S9(9)   VALUE ZERO.      EL309
           05  WS-HISTORY-OUTPUT-COUNT     PIC S9(9)   VALUE ZERO.      EL309
                                                                        EL309
           05  WS-TOTAL-HISTORY-INPUT      PIC S9(9)   VALUE ZERO.      EL309
           05  WS-TOTAL-HISTORY-OUTPUT     PIC S9(9)   VALUE ZERO.      EL309
                                                                        EL309
           05  WS-CL-HISTORY-INPUT         PIC S9(9)   VALUE ZERO.      EL309
           05  WS-CL-HISTORY-OUTPUT        PIC S9(9)   VALUE ZERO.      EL309
           05  WS-CM-HISTORY-INPUT         PIC S9(9)   VALUE ZERO.      EL309
           05  WS-CM-HISTORY-OUTPUT        PIC S9(9)   VALUE ZERO.      EL309
           05  WS-PM-HISTORY-INPUT         PIC S9(9)   VALUE ZERO.      EL309
           05  WS-PM-HISTORY-OUTPUT        PIC S9(9)   VALUE ZERO.      EL309
           05  WS-AT-HISTORY-INPUT         PIC S9(9)   VALUE ZERO.      EL309
           05  WS-AT-HISTORY-OUTPUT        PIC S9(9)   VALUE ZERO.      EL309
           05  WS-LA-HISTORY-INPUT         PIC S9(9)   VALUE ZERO.      EL309
           05  WS-LA-HISTORY-OUTPUT        PIC S9(9)   VALUE ZERO.      EL309
           05  WS-OTHER-HISTORY-INPUT      PIC S9(9)   VALUE ZERO.      EL309
           05  WS-OTHER-HISTORY-OUTPUT     PIC S9(9)   VALUE ZERO.      EL309
           EJECT                                                        EL309
       01  FILLER   COMP   SYNC.                                        EL309
           05  PGM-SUB                     PIC S9(4)   VALUE +309.      EL309
           05  WS-INDEX                    PIC S9(4)   VALUE ZERO.      EL309
           05  WS-LENGTH                   REDEFINES                    EL309
               WS-INDEX                    PIC S9(4).                   EL309
           05  MAX-TABLE-INDX              PIC S9(4)   VALUE +100.      EL309
                                                                        EL309
       01  FILLER.                                                      EL309
           05  WS-LETTER-MESSAGE.                                       EL309
               10  FILLER              PIC X(25)  VALUE                 EL309
                        'ARCHIVE LETTER NOT FOUND '.                    EL309
               10  WS-NUMBER-DISPLAY   PIC ZZZZZZZ9  VALUE ZEROS.       EL309
               10  FILLER              PIC X(27)  VALUE SPACES.         EL309
                                                                        EL309
                                                                        EL309
           05  WS-DISPLAY-TIME             PIC 99B99B99.                EL309
                                                                        EL309
           05  WS-ACCEPT-TIME.                                          EL309
               10  WS-ACCEPT-HHMMSS        PIC 9(06)   VALUE ZEROS.     EL309
               10  FILLER                  PIC 9(02)   VALUE ZEROS.     EL309
                                                                        EL309
           05  X                           PIC X.                       EL309
           05  ABEND-CODE                  PIC X(4).                    EL309
           05  ABEND-OPTION                PIC X.                       EL309
           05  OLC-REPORT-NAME             PIC X(8) VALUE 'EL- 309'.    EL309
                                                                        EL309
           05  WS-SAVE-PRINT-RECORD        PIC X(133)  VALUE SPACES.    EL309
                                                                        EL309
                                                                        EL309
           05  WS-ABEND-MESSAGE            PIC X(80)   VALUE SPACES.    EL309
                                                                        EL309
           05  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZERO.      EL309
                                                                        EL309
                                                                        EL309
           05  WS-CURRENT-DATE             PIC XX  VALUE LOW-VALUES.    EL309
           05  WS-RUN-DATE                 PIC XX  VALUE LOW-VALUES.    EL309
                                                                        EL309
           05  WS-COMPANY-ID.                                           EL309
               10 WS-FIRST-TWO             PIC X(2).                    EL309
               10 WS-COMPANY-INCREMENT     PIC X.                       EL309
           05  WS-COMPANY-CD               PIC X.                       EL309
                                                                        EL309
                                                                        EL309
           05  WS-COMPANY-NAME.                                         EL309
               10  WS-CN-CHAR              PIC X                        EL309
                   OCCURS 30 TIMES         INDEXED BY CN1.              EL309
                                                                        EL309
                                                                        EL309
           EJECT                                                        EL309

                                       COPY ELCDATE.                    EL309

       PROCEDURE DIVISION.                                              EL309

       0000-MAIN-LOGIC SECTION.                                         EL309

           PERFORM 0005-OPEN-FILES     THRU 0005-EXIT
           PERFORM 0010-INIT           THRU 0010-EXIT

           PERFORM 0020-PROCESS-HISTORY-FILE
                                       THRU 0020-EXIT UNTIL
              END-OF-HISTORY-FILE

           PERFORM 0050-CLOSE-FILES    THRU 0050-EXIT

           DISPLAY ' HIST  IN ' WS-TOTAL-HISTORY-INPUT
           DISPLAY ' AT    IN ' WS-AT-HISTORY-INPUT
           DISPLAY ' LA    IN ' WS-LA-HISTORY-INPUT
           DISPLAY ' CM    IN ' WS-CM-HISTORY-INPUT
           DISPLAY ' PM    IN ' WS-PM-HISTORY-INPUT
           DISPLAY ' CL    IN ' WS-CL-HISTORY-INPUT
           DISPLAY ' OTHER IN ' WS-OTHER-HISTORY-INPUT

           DISPLAY ' HIST  OT ' WS-TOTAL-HISTORY-OUTPUT
           DISPLAY ' AT    OT ' WS-AT-HISTORY-OUTPUT
           DISPLAY ' LA    OT ' WS-LA-HISTORY-OUTPUT
           DISPLAY ' CM    OT ' WS-CM-HISTORY-OUTPUT
           DISPLAY ' PM    OT ' WS-PM-HISTORY-OUTPUT
           DISPLAY ' CL    OT ' WS-CL-HISTORY-OUTPUT
           DISPLAY ' OTHER OT ' WS-OTHER-HISTORY-OUTPUT



           GOBACK

           .
       0005-OPEN-FILES.

           OPEN INPUT HISTORY-INPUT-FILE
           OPEN OUTPUT HISTORY-OUTPUT-FILE

           .
       0005-EXIT.
           EXIT.

       0010-INIT.

           MOVE 'DCC'                  TO  WS-COMPANY-NAME
                                           WS-COMPANY-ID

           MOVE X'05'                  TO  WS-COMPANY-CD

           ACCEPT WS-ACCEPT-TIME       FROM TIME
           MOVE WS-ACCEPT-HHMMSS       TO WS-DISPLAY-TIME
           INSPECT WS-DISPLAY-TIME CONVERTING SPACES TO '.'

           DISPLAY 'EL309-BEGIN PROCESSING ' WS-COMPANY-NAME ' AT '
                   WS-DISPLAY-TIME UPON CONSOLE

           ACCEPT DC-GREG-DATE-1-YMD   FROM DATE
           MOVE '3'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
           MOVE DC-BIN-DATE-1          TO WS-CURRENT-DATE
                                          WS-RUN-DATE
      *    MOVE DC-GREG-DATE-1-ALPHA   TO WS-H3-DATE

           ACCEPT WS-CURRENT-TIME      FROM TIME

           MOVE +99                    TO WS-LINE-COUNT

           PERFORM 0030-READ-INPUT     THRU 0030-EXIT

           .
       0010-EXIT.
           EXIT.



       0020-PROCESS-HISTORY-FILE.
       
           IF HIR-COMPANY-ID = 'CID'
              PERFORM 0040-WRITE-HIST  THRU 0040-EXIT
           ELSE
              DISPLAY ' DROPPING RECORD ' HIR-COMPANY-ID '  '
                 HIR-RECORD-ID '  ' HIR-CLAIM-KEY (2:19)
           END-IF
           
           PERFORM 0030-READ-INPUT     THRU 0030-EXIT
                                      
           .
       0020-EXIT.
           EXIT.

       0030-READ-INPUT.
       
           READ HISTORY-INPUT-FILE AT END
              SET END-OF-HISTORY-FILE TO TRUE
           END-READ

           IF NOT END-OF-HISTORY-FILE
              ADD +1                   TO WS-TOTAL-HISTORY-INPUT
              EVALUATE HIR-RECORD-ID
                 WHEN 'AT'
                    ADD +1             TO WS-AT-HISTORY-INPUT
                 WHEN 'LA'
                    ADD +1             TO WS-LA-HISTORY-INPUT
                 WHEN 'CM'
                    ADD +1             TO WS-CM-HISTORY-INPUT
                 WHEN 'PM'
                    ADD +1             TO WS-PM-HISTORY-INPUT
                 WHEN 'CL'
                    ADD +1             TO WS-CL-HISTORY-INPUT
                 WHEN OTHER
                    ADD +1             TO WS-OTHER-HISTORY-INPUT
              END-EVALUATE
           END-IF

           .
       0030-EXIT.
           EXIT.
                      
       0040-WRITE-HIST.
       
           ADD +1                   TO WS-TOTAL-HISTORY-OUTPUT

           EVALUATE HIR-RECORD-ID
              WHEN 'AT'
                 MOVE HISTORY-INPUT-TRAILER-RECORD
                                    TO HISTORY-OUTPUT-TRAILER-RECORD
                 WRITE HISTORY-OUTPUT-TRAILER-RECORD
                 ADD +1             TO WS-AT-HISTORY-OUTPUT
              WHEN 'LA'
                 MOVE HISTORY-INPUT-LETTER-RECORD
                                    TO HISTORY-OUTPUT-LETTER-RECORD
                 WRITE HISTORY-OUTPUT-LETTER-RECORD
                 ADD +1             TO WS-LA-HISTORY-OUTPUT
              WHEN 'CM'
                 MOVE HISTORY-INPUT-CERT-RECORD
                                    TO HISTORY-OUTPUT-CERT-RECORD
                 WRITE HISTORY-OUTPUT-CERT-RECORD
                 ADD +1             TO WS-CM-HISTORY-OUTPUT
              WHEN 'PM'
                 ADD +1             TO WS-PM-HISTORY-OUTPUT
              WHEN 'CL'
                 MOVE HISTORY-INPUT-CLAIM-RECORD
                                    TO HISTORY-OUTPUT-CLAIM-RECORD
                 WRITE HISTORY-OUTPUT-CLAIM-RECORD
                 ADD +1             TO WS-CL-HISTORY-OUTPUT
              WHEN OTHER
                 ADD +1             TO WS-OTHER-HISTORY-OUTPUT
           END-EVALUATE

           .
       0040-EXIT.
           EXIT.
           
       0050-CLOSE-FILES.

           CLOSE HISTORY-INPUT-FILE
                 HISTORY-OUTPUT-FILE

          .
       0050-EXIT.
           EXIT.

       8500-DATE-CONVERSION.           COPY ELCDCS.
       
       ABEND-PGM SECTION.              COPY ELCABEND SUPPRESS.          EL309
                                                                        EL309
