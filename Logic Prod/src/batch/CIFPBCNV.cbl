       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIFPBCNV.
       AUTHOR.     PABLO.
       DATE-COMPILED.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 051506                   PEMA  ADD FPB LOAN NUMBER
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT  FPB-IN              ASSIGN TO SYS010
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT  FPB-OUT             ASSIGN TO SYS012.
       DATA DIVISION.

       FILE SECTION.

       FD  FPB-IN
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.

       01  FPB-RECORD-IN               PIC X(936).

       FD  FPB-OUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS FPB-OUT-RECORD.

       01  FPB-OUT-RECORD          PIC X(936).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '         WORKING-STORAGE        '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-FPB                     VALUE 'Y'.
       77  RECS-IN                     PIC 9(7)  VALUE ZEROS.
       77  RECS-OUT                    PIC 9(7)  VALUE ZEROS.
       77  S1                          PIC S999  VALUE +0 COMP-3.
       77  S2                          PIC S999  VALUE +0 COMP-3.
       77  WS-WORK-CERT                PIC X(10).
       01  DISPLAY-LINE                PIC X(90).
       
                                       COPY ERCFPBRL.

       01  FPB-RECORD                  PIC X(936)  VALUE SPACES.

       PROCEDURE DIVISION.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT
           PERFORM 0020-INIT           THRU 0020-EXIT
           PERFORM 0030-PROCESS-FPB    THRU 0030-EXIT UNTIL
              END-OF-FPB
           PERFORM 0015-CLOSE-FILES    THRU 0015-EXIT
           
           DISPLAY ' RECORDS IN  ' RECS-IN
           DISPLAY ' RECORDS OUT ' RECS-OUT
           
           GOBACK
           .
       0010-OPEN-FILES.

           OPEN INPUT  FPB-IN
                OUTPUT FPB-OUT

           .
       0010-EXIT.
           EXIT.

       0015-CLOSE-FILES.

           CLOSE FPB-IN  FPB-OUT

           .
       0015-EXIT.
           EXIT.

       0020-INIT.
       
           PERFORM 0040-READ-FPB       THRU 0040-EXIT
           
           .
       0020-EXIT.
           EXIT.

       0030-PROCESS-FPB.

           MOVE SPACES                 TO DISPLAY-LINE

           EVALUATE FP-RECORD-ID
              WHEN '1'
                 STRING 'RECSIZE ' FP1-REC-SIZE
                    ' BLOCK FACTOR ' FP1-BLOCK-FACTOR
                    ' INST NUM ' FP1-INST-NUMBER
                    DELIMITED BY SIZE INTO DISPLAY-LINE
                 END-STRING
                 DISPLAY 'REC 1 - ' DISPLAY-LINE
              WHEN '3'
                 STRING 'DATE CREATED ' FP3-DATE-CREATED
                    ' TIME CREATED ' FP3-TIME-CREATED
                    DELIMITED BY SIZE INTO DISPLAY-LINE
                 END-STRING
                 DISPLAY 'REC 3 - ' DISPLAY-LINE
              WHEN '4'
                 STRING ' PROGRAM NAME ' FP4-PROGRAM-NAME
                    ' FP4-PROGRAM-RELEASE ' FP4-PROGRAM-RELEASE
                    DELIMITED BY SIZE INTO DISPLAY-LINE
                 END-STRING
                 DISPLAY 'REC 4 - ' DISPLAY-LINE
              WHEN '7'
                 PERFORM 0050-WRITE-FPB
                                       THRU 0050-EXIT
              WHEN '9'
                 STRING ' RECORD COUNT ' FP9-REC-COUNT
                    DELIMITED BY SIZE INTO DISPLAY-LINE
                 END-STRING
                 DISPLAY 'REC 9 - ' DISPLAY-LINE
                 IF FP9-REC-COUNT NOT NUMERIC
                    MOVE ZEROS         TO FP9-REC-COUNT
                 END-IF
                 IF FP9-REC-COUNT NOT = RECS-IN
                    DISPLAY 'ERROR IN RECORD COUNT'
                       ' ACTUAL ' RECS-IN
                 END-IF
              WHEN OTHER
                 CONTINUE
           END-EVALUATE
           
           PERFORM 0040-READ-FPB       THRU 0040-EXIT
           
           .
       0030-EXIT.
           EXIT.
                
       0040-READ-FPB.

           READ FPB-IN INTO FIRST-PREMIER-INPUT AT END
                SET END-OF-FPB         TO TRUE
           END-READ

           IF NOT END-OF-FPB
              ADD 1                    TO RECS-IN
           END-IF

           .
       0040-EXIT.
           EXIT.

       0050-WRITE-FPB.
       
           WRITE FPB-OUT-RECORD        FROM FPB-RECORD-IN
           ADD 1                       TO RECS-OUT
           
           .
       0050-EXIT.
           EXIT.
           
       0060-BUILD-ACCOUNT.
       
      * BUILD ACCOUNT NUMBER HERE


           .
       0060-EXIT.
           EXIT.       

       0070-FIX-CERT-NO.

      *  FIX CERTIFICATE NUMBER HERE
             
           .
       0070-EXIT.
           EXIT.       




