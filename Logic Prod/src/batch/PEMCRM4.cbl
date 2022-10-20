       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMCRM2.
      *AUTHOR.     PABLO.
      *REMARKS.
      *     THIS PROGRAM READS A CERT EXTRACT FROM PEMCRXGC AND MATCHES
      *      IT TO THE CLMS FILE TO GET THE CLAIM NUMBER AND THEN 
      *      WRITES OUT A NEW CERT EXTRACT
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EXTR-IN              ASSIGN TO SYS010
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE      ASSIGN TO SYS019.
           SELECT CLMS-IN              ASSIGN TO SYS011.

           SELECT EXTR-OUT             ASSIGN TO SYS013
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  CLMS-IN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.

                                       COPY ECSEXT01.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  EXTR-OUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  EXTR-OUT-RECORD             PIC X(122).

       FD  EXTR-IN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 066 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.

       01  EXTR-RECORD                PIC X(122).

       WORKING-STORAGE SECTION.                                         00007200
       77  FILLER  PIC X(32) VALUE '********************************'.  00007300
       77  FILLER  PIC X(32) VALUE '         WORKING-STORAGE        '.  00007400
       77  FILLER  PIC X(32) VALUE '********************************'.  00007500
       77  WS-BIN-EFF-DT           PIC XX  VALUE LOW-VALUES.
       77  WS-BIN-INC-DT           PIC XX  VALUE LOW-VALUES.
       77  PGM-SUB                 PIC S999 COMP  VALUE +158.
       77  WS-RETURN-CODE         PIC S9(4)     COMP     VALUE ZERO.         
       77  WS-ABEND-MESSAGE            PIC X(80) VALUE SPACES.               
       77  WS-ABEND-FILE-STATUS        PIC XX    VALUE ZERO.                 
       77  WS-ZERO                PIC S9        COMP-3   VALUE ZERO.         

       01  WS-IN-RECORD.
           12  WS-COMPANY          PIC X(32).
           12  WS-TAB1             PIC X.
           12  WS-GPN              PIC X.
           12  WS-TAB2             PIC X.
           12  WS-CERT-NO          PIC X(11).
           12  WS-TAB3             PIC X.
           12  WS-CLAIM-NO         PIC X(7).
           12  WS-TAB4             PIC X.
           12  WS-DOB              PIC X(10).
           12  WS-TAB5             PIC X.
           12  WS-INS-AGE          PIC 99.
           12  WS-TAB6             PIC X.
           12  WS-SEX              PIC X.
           12  WS-TAB7             PIC X.
           12  WS-CLMNT-DOB        PIC X(10).
           12  WS-TAB8             PIC X.
           12  WS-CLMNT-AGE        PIC 99.
           12  WS-TAB9             PIC X.
           12  WS-CLMNT-SEX        PIC X.
           12  WS-TAB10            PIC X.
           12  WS-COV-TYPE         PIC XX.
           12  WS-TAB11            PIC X.
           12  WS-SIN-JNT          PIC X.
           12  WS-TAB12            PIC X.
           12  WS-DTH-DATE         PIC X(10).
           12  WS-TAB13            PIC X.
           12  WS-CLM-AMT          PIC 999999999.99.
           12  WS-TAB14            PIC X.
           12  WS-UW               PIC X.
           12  WS-TAB15            PIC X.
           12  WS-BUS-TYPE         PIC X.
           12  WS-TAB16            PIC X.
           12  WS-STATE            PIC XX.



       01  W-MISC.
           05  EXT-KEY.
               10  EXT-CERT           PIC X(32).
           05  CLMS-KEY.
               10  CLMS-CARRIER        PIC X.
               10  CLMS-STATE          PIC XX.
               10  CLMS-ACCOUNT        PIC X(10).
               10  CLMS-EFF-DT         PIC 9(8).
               10  CLMS-CERT-NO        PIC X(11).
           05  EXT-MATCH-CNT           PIC 9(7) VALUE ZEROS.
           05  EXT-DUP-CNT             PIC 9(7) VALUE ZEROS.
           05  EXT-IN-CNT              PIC 9(7) VALUE ZEROS.
           05  CLMS-IN-CNT             PIC 9(7) VALUE ZEROS.
           05  EXTR-OT-CNT             PIC 9(7) VALUE ZEROS.
           05  WS-EOF-SW               PIC X VALUE SPACES.
               88  END-OF-EXTR              VALUE 'Y'.
           05  WS-CRT-SW              PIC X VALUE ' '.
               88  END-OF-CLMS              VALUE 'Y'.

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.
                                       COPY ELCDTERX.

           PERFORM 0005-INIT           THRU 0005-EXIT
           
           PERFORM 0020-PROCESS THRU 0020-EXIT UNTIL
              (END-OF-EXTR)
              AND (END-OF-CLMS)
           CLOSE   EXTR-IN   CLMS-IN  EXTR-OUT
              
           DISPLAY ' CLMS RECS READ ' CLMS-IN-CNT
           DISPLAY ' MATCHED RECS   ' EXT-MATCH-CNT
           DISPLAY ' EXT  RECS READ ' EXT-IN-CNT
           DISPLAY ' EXTR RECS OUT  ' EXTR-OT-CNT
           DISPLAY ' EXTR RECS DUP  ' EXT-DUP-CNT

           GOBACK

           .
       0005-INIT.

           OPEN INPUT EXTR-IN CLMS-IN
                OUTPUT EXTR-OUT

           PERFORM 0010-READ-EXT       THRU 0010-EXIT
           PERFORM 0015-READ-CLMS      THRU 0015-EXIT

           .
       0005-EXIT.
           EXIT.

       0010-READ-EXT.

           READ EXTR-IN INTO WS-IN-RECORD AT END
              SET END-OF-EXTR          TO TRUE
              MOVE HIGH-VALUES         TO EXT-KEY
           END-READ

           IF NOT END-OF-EXTR
              MOVE WS-COMPANY          TO EXT-KEY
              ADD 1                    TO EXT-IN-CNT
           END-IF
           
           .
       0010-EXIT.
           EXIT.

       0015-READ-CLMS.

           READ CLMS-IN AT END
              SET END-OF-CLMS          TO TRUE
              MOVE HIGH-VALUES         TO CLMS-KEY
           END-READ

           IF NOT END-OF-CLMS
              IF (DE-REIN = 'R')
                 OR (DE-TRANS NOT = 'X')
                 GO TO 0015-READ-CLMS
              END-IF
              MOVE DE-CARRIER          TO CLMS-CARRIER
              MOVE DE-STATE            TO CLMS-STATE
              MOVE DE-ACCOUNT          TO CLMS-ACCOUNT
              MOVE DE-EFF              TO CLMS-EFF-DT
              MOVE DE-CERT             TO CLMS-CERT-NO
              ADD 1                    TO CLMS-IN-CNT
           END-IF

           .
       0015-EXIT.
           EXIT.

       0020-PROCESS.

           IF EXT-KEY = CLMS-KEY
              PERFORM 0030-MATCHED     THRU 0030-EXIT
              PERFORM 0040-WRITE-EXTR  THRU 0040-EXIT
              PERFORM 0015-READ-CLMS   THRU 0015-EXIT
              PERFORM 0010-READ-EXT    THRU 0010-EXIT
           ELSE
              IF EXT-KEY > CLMS-KEY
                 PERFORM 0015-READ-CLMS
                                       THRU 0015-EXIT
              ELSE
                 PERFORM 0080-NOT-ON-LOGIC
                                       THRU 0080-EXIT
                 PERFORM 0040-WRITE-EXTR
                                       THRU 0040-EXIT
                 PERFORM 0010-READ-EXT
                                       THRU 0010-EXIT
              END-IF
           END-IF

           .
       0020-EXIT.
            EXIT.

       0030-MATCHED.

           MOVE DE-CNUM                TO WS-CLAIM-NO
           IF DE-CLM-AGE NOT = ZEROS
              MOVE DE-CLM-AGE          TO WS-CLMNT-AGE
           END-IF

           IF WS-CLMNT-AGE = ZERO
              MOVE DE-EFF              TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1    TO WS-BIN-EFF-DT
              END-IF
              MOVE DE-INCUR            TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1    TO WS-BIN-INC-DT
              END-IF
              MOVE WS-BIN-EFF-DT       TO DC-BIN-DATE-1
              MOVE WS-BIN-INC-DT       TO DC-BIN-DATE-2
              MOVE '1'                 TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
              IF NO-CONVERSION-ERROR
                 COMPUTE WS-CLMNT-AGE = DE-AGE +
                    (DC-ELAPSED-MONTHS / 12)
                 DISPLAY ' FIXING CLMNT AGE ' DE-AGE ' ' WS-CLMNT-AGE
                    ' ' DC-ELAPSED-MONTHS
              END-IF
           END-IF

           ADD 1 TO EXT-MATCH-CNT

           .
       0030-EXIT.
            EXIT.

       0040-WRITE-EXTR.
       
           MOVE 'CENTRAL STATES HEALTH AND LIFE'
                                       TO WS-COMPANY
           WRITE EXTR-OUT-RECORD       FROM WS-IN-RECORD
           ADD 1 TO EXTR-OT-CNT

           .
       0040-EXIT.
           EXIT.

       0070-NOT-ON-BOW.

           .
       0070-EXIT.
            EXIT.

       0080-NOT-ON-LOGIC.

           DISPLAY ' NO EXTR TO CLMS MATCH ' WS-COMPANY

           .
       0080-EXIT.
            EXIT.

       8500-DATE-CONVERT.
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA.

       8500-EXIT.
           EXIT.

       ABEND-PGM.   COPY ELCABEND.
