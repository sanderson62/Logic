       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMCRXGC.
       AUTHOR.     PABLO
       DATE-COMPILED.
      *REMARKS
      *  PROGRAM READS THE CERT FILE AND CREATES A CLMS SOA EXTRACT
      *  OF ONLY CERTS WITH DEATH CLAIMS. THE OUTPUT OF THIS PROGRAM
      *  SHOULD BE RUN THROUGH PEMCRM4 TO GET THE CLAIM NUMBER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CERT-IN        ASSIGN TO SYS010.
           SELECT DISK-DATE      ASSIGN TO SYS019.

           SELECT EXTRACT-OUT    ASSIGN TO SYS011
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  CERT-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

                                       COPY ECSCRT01.
       FD  DISK-DATE
                                       COPY ELCDTEFD.
       FD  EXTRACT-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  EXTRACT-OUT-REC             PIC X(265).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMCRXGC WORKING-STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

00197  77  WS-ZERO                PIC S9        COMP-3   VALUE ZERO.         
00198  77  WS-RETURN-CODE         PIC S9(4)     COMP     VALUE ZERO.         
       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-INPUT              VALUE 'Y'.
00199  77  WS-ABEND-MESSAGE            PIC X(80) VALUE SPACES.               
00200  77  WS-ABEND-FILE-STATUS        PIC XX    VALUE ZERO.                 
       77  CERT-RECS-IN            PIC 9(9) VALUE ZEROS.
       77  EXTR-RECS-OUT           PIC 9(9) VALUE ZEROS.
       77  WS-WORK-DATE                PIC 9(8)  VALUE ZEROS.
       77  PGM-SUB                 PIC S999 COMP  VALUE +158.

       01  WS-KEY.
           12  WS-CARRIER              PIC X.
           12  WS-ST                   PIC XX.
           12  WS-ACCOUNT              PIC X(10).
           12  WS-EFF-DT               PIC 9(8).
           12  WS-CRT-NO               PIC X(11).
       01  WS-INIT-EXTR            PIC X(265)   VALUE SPACES.
       01  WS-OUT-RECORD.
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

                                       COPY ELCCALC.

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.
                                       COPY ELCDTERX.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT

           PERFORM 0020-INITIALIZE     THRU 0020-EXIT

           PERFORM 0030-PROCESS-INPUT  THRU 0030-EXIT UNTIL
              (END-OF-INPUT)
PEMTST*       OR (CERT-RECS-IN > 10000)

           PERFORM 0070-CLOSE-FILES    THRU 0070-EXIT

           DISPLAY ' CERT RECORDS READ    ' CERT-RECS-IN
           DISPLAY ' EXTR RECORDS WRITTEN ' EXTR-RECS-OUT
           GOBACK

           .
       0010-OPEN-FILES.

           OPEN INPUT CERT-IN
               OUTPUT EXTRACT-OUT

           .
       0010-EXIT.
           EXIT.

       0020-INITIALIZE.

           MOVE SPACES                 TO WS-OUT-RECORD
           MOVE ','                    TO WS-TAB1
                                          WS-TAB2
                                          WS-TAB3
                                          WS-TAB4
                                          WS-TAB5
                                          WS-TAB6
                                          WS-TAB7
                                          WS-TAB8
                                          WS-TAB9
                                          WS-TAB10
                                          WS-TAB11
                                          WS-TAB12
                                          WS-TAB13
                                          WS-TAB14
                                          WS-TAB15
                                          WS-TAB16

           MOVE 'CENTRAL STATES HEALTH AND LIFE'
                                       TO WS-COMPANY
           MOVE 'U'                    TO WS-SEX WS-CLMNT-SEX
           MOVE 'N'                    TO WS-UW
                                          
           MOVE ZEROS                  TO WS-CLM-AMT
                                          WS-INS-AGE
                                          WS-CLMNT-AGE

           MOVE WS-OUT-RECORD          TO WS-INIT-EXTR

           PERFORM 0050-READ-CERT      THRU 0050-EXIT

           .
       0020-EXIT.
           EXIT.

       0030-PROCESS-INPUT.

      *    IF (CR-ENTRY-DATE > 20061231)
      *       OR (CR-DTH-DT = ZEROS OR LOW-VALUES)
      *       OR (CR-DTHAMT = ZEROS)
      *       OR (CR-LFTYP = SPACES OR ZEROS)
      *       OR (CR-ENTRY-STATUS NOT = '1' AND '3' AND '4')
      *       OR (CR-LF-EXPIRE-DATE < 20030101)
      *       OR ((CR-LF-CANCEL-EXIT-DATE NOT = ZEROS)
      *                     AND
      *          (CR-LF-CANCEL-EXIT-DATE < 20030101))
      *       OR ((CR-LF-CLAIM-EXIT-DATE NOT = ZEROS)
      *                     AND
      *          (CR-LF-CLAIM-EXIT-DATE < 20030101))
      *       CONTINUE


           IF (CR-ENTRY-DATE > 20061231)
              OR (CR-DTH-DT = ZEROS OR LOW-VALUES)
              OR (CR-DTHAMT = ZEROS)
              OR (CR-LFTYP = SPACES OR ZEROS)
              OR (CR-ENTRY-STATUS NOT = '1' AND '3' AND '4')
              OR (CR-LF-EXPIRE-DATE < 20030101)
              OR ((CR-LF-CANC-DT NOT = ZEROS)
                            AND
                 (CR-LF-CANC-DT < 20030101))
              OR ((CR-DTH-DT NOT = ZEROS)
                            AND
                 (CR-DTH-DT < 20030101))
              CONTINUE
           ELSE
              PERFORM 0040-PROCESS-CERT THRU 0040-EXIT
           END-IF

           PERFORM 0050-READ-CERT      THRU 0050-EXIT

           .
       0030-EXIT.
           EXIT.

       0040-PROCESS-CERT.

           PERFORM VARYING CLAS-INDEXL FROM CLAS-STARTL BY +1 UNTIL
              (CR-LFTYP = CLAS-I-BEN (CLAS-INDEXL))
              OR (CLAS-INDEXL > CLAS-MAXL)
           END-PERFORM
           
           IF CLAS-INDEXL > CLAS-MAXL
              DISPLAY 'INVALID LIFE - ' CR-LFTYP ' ' CR-STATE ' '
                 CR-ACCOUNT ' ' CR-CERT-NO
              PERFORM ABEND-PGM
           END-IF

           MOVE CR-CERT-NO             TO WS-CERT-NO
           MOVE CR-AGE                 TO WS-INS-AGE
           MOVE CR-DTH-AGE             TO WS-CLMNT-AGE
           MOVE CR-DTHAMT              TO WS-CLM-AMT
           MOVE CR-STATE               TO WS-STATE

           EVALUATE TRUE
              WHEN CR-GRPTYP = '01' OR '14'
                 MOVE 'A'              TO WS-BUS-TYPE
              WHEN CR-GRPTYP = '02' OR '05'
                 MOVE 'B'              TO WS-BUS-TYPE
              WHEN CR-GRPTYP = '04'
                 MOVE 'C'              TO WS-BUS-TYPE
              WHEN CR-GRPTYP = '06' OR '07' OR '08' OR '09' OR '15'
                            OR '18' OR '19'
                 MOVE 'D'              TO WS-BUS-TYPE
              WHEN CR-GRPTYP = '03'
                 MOVE 'F'              TO WS-BUS-TYPE
              WHEN CR-GRPTYP = '10' OR '11' OR '12' OR '13' OR '17'
                            OR '21' OR '98' OR '99'
                 MOVE 'O'              TO WS-BUS-TYPE
              WHEN OTHER
                 DISPLAY ' BAD BUS TYPE ' CR-GRPTYP ' ' CR-STATE ' '
                    CR-ACCOUNT ' ' CR-CERT-NO
                 MOVE 'U'              TO WS-BUS-TYPE
           END-EVALUATE

           MOVE CR-DTH-DT              TO WS-WORK-DATE
           STRING WS-WORK-DATE (5:2) '/' WS-WORK-DATE (7:2) '/'
                                         WS-WORK-DATE (1:4)
              DELIMITED BY SIZE INTO WS-DTH-DATE
           END-STRING

           IF CLAS-I-JOINT (CLAS-INDEXL) = 'J'
              MOVE 'J'                 TO WS-SIN-JNT
           ELSE
              MOVE 'S'                 TO WS-SIN-JNT
           END-IF

           MOVE CLAS-I-EP (CLAS-INDEXL)    TO CP-EARNING-METHOD
           MOVE CLAS-I-RL-AH (CLAS-INDEXL) TO CP-BENEFIT-TYPE
           MOVE CLAS-I-CALC-TYPE (CLAS-INDEXL)
                                           TO CP-SPECIAL-CALC-CD

           EVALUATE TRUE
              WHEN (CP-EARN-AS-NET-PAY)
                 AND (NOT CP-TRUNCATED-LIFE)
                 AND (CP-REDUCING-LIFE)
                 MOVE 'ND'             TO WS-COV-TYPE
              WHEN (CP-EARN-AS-NET-PAY)
                 AND (CP-TRUNCATED-LIFE)
                 MOVE 'TN'             TO WS-COV-TYPE
              WHEN CP-LEVEL-LIFE
                 MOVE 'GL'             TO WS-COV-TYPE
              WHEN CP-REDUCING-LIFE
                 MOVE 'GD'             TO WS-COV-TYPE
              WHEN OTHER
                 DISPLAY ' CHECK COVERAGE TYPE ' CR-LFTYP ' ' CR-STATE
                    ' ' CR-ACCOUNT ' ' CR-CERT-NO
           END-EVALUATE

           MOVE CR-CARRIER             TO WS-CARRIER
           MOVE CR-STATE               TO WS-ST
           MOVE CR-ACCOUNT             TO WS-ACCOUNT
           MOVE CR-DT                  TO WS-EFF-DT
           MOVE CR-CERT-NO             TO WS-CRT-NO
           MOVE WS-KEY                 TO WS-COMPANY

           PERFORM 0060-WRITE-EXTR     THRU 0060-EXIT

           .
       0040-EXIT.
           EXIT.

       0050-READ-CERT.

           READ CERT-IN AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1                    TO CERT-RECS-IN
           END-IF

           .
       0050-EXIT.
           EXIT.


       0060-WRITE-EXTR.

           WRITE EXTRACT-OUT-REC       FROM WS-OUT-RECORD
           ADD 1                       TO EXTR-RECS-OUT

           .
       0060-EXIT.
           EXIT.

       0070-CLOSE-FILES.

           CLOSE CERT-IN EXTRACT-OUT

           .
       0070-EXIT.
           EXIT.

       8500-DATE-CONVERT.
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA.

       8500-EXIT.
           EXIT.

       ABEND-PGM.   COPY ELCABEND.

