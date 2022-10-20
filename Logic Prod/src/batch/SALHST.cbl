       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SALHST.
       AUTHOR.        SUZAN VUKOV.
       DATE-COMPILED.

      *REMARKS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT SAL-LOGICCLMS-IN     ASSIGN TO SYS010.
           SELECT SAL-LOGICCERT-IN     ASSIGN TO SYS012.
           SELECT SAL-LOGICCERT-OUT    ASSIGN TO SYS011.
           SELECT DISK-DATE            ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  SAL-LOGICCLMS-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.


                                       COPY ECSEXT01.

       FD  SAL-LOGICCERT-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  SAL-LOGICCERT-IN-REC        PIC X(1056).


       FD  SAL-LOGICCERT-OUT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  SAL-LOGICCERT-OUT-REC       PIC X(1056).


       FD  DISK-DATE
                                       COPY ELCDTEFD.
           EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     SALHST  WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW1                  PIC X  VALUE SPACES.
           88  END-OF-CLMS                VALUE 'Y'.
       77  WS-EOF-SW2                  PIC X  VALUE SPACES.
           88  END-OF-CERT                VALUE 'Y'.
       77  WS-MATCH-SW                 PIC X  VALUE SPACES.
           88  CERT-FOUND                 VALUE 'Y'.
       77  INCUR-INDEX                 PIC S9(03) COMP-3 VALUE +0.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  WS-CLM-IN-CNT               PIC 9(9)   VALUE ZEROS.
       77  WS-CERT-IN-CNT              PIC 9(9)   VALUE ZEROS.
       77  WS-CERT-OUT-CNT             PIC 9(9)   VALUE ZEROS.

       01  WS-MISC.
           05  WS-WORK-DAYS            PIC 999   VALUE ZEROS.
           05  WS-WORK-DAYS-X REDEFINES WS-WORK-DAYS.
               10  FILLER              PIC X.
               10  WS-WORK-DAYS-C      PIC XX.
           05  WS-WORK-AMT.
               10  WS-AMT1             PIC 9(8).
               10  FILLER              PIC X.
               10  WS-AMT2             PIC 99.
           05  WS-WORK-AMT-X           PIC X(10).
           05  WS-WORK-AMT-N REDEFINES WS-WORK-AMT-X
                                       PIC 9(8)V99.
           05  WS-WORK-APR-X           PIC X(7).
           05  WS-WORK-APR-N REDEFINES WS-WORK-APR-X
                                       PIC 9(3)V9(4).
           05  WS-WORK-COMM-X          PIC X(8).
           05  WS-WORK-COMM-N REDEFINES WS-WORK-COMM-X
                                       PIC 9(3)V9(5).
           05  WS-BIN-EFF-DT           PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-LF-EXP-DT        PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-AH-EXP-DT        PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-LF-CANC-EXIT-DT  PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-AH-CANC-EXIT-DT  PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-LF-CANC-DT       PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-AH-CANC-DT       PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-LF-CLM-EXIT-DT   PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-AH-CLM-EXIT-DT   PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-1ST-PMT-DT       PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-ENTRY-DT         PIC XX   VALUE LOW-VALUES.
           05  WS-WORK-DATE            PIC X(8).
           05  WS-WORK-DATE-N REDEFINES WS-WORK-DATE
                                       PIC 9(8).
           05  WS-DISPLAY-DATE         PIC ZZZ9(8).
           05  WS-DISPLAY-TERM         PIC Z99.
           05  WS-DISPLAY-RTERM        PIC Z99.99.
           05  WS-DISPLAY-PRM          PIC ZZZZZZZ.99.
           05  WS-DISPLAY-UEP          PIC ZZZZZZZ.99.

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


       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

           EJECT
                                       COPY ELCDATE.
      /
                                       COPY ELCDTECX.
      /
                                       COPY ELCDTEVR.
 
                                       COPY ECSCRT01.
      /
       PROCEDURE DIVISION.

       0001-DT-CRD-READ SECTION.
                                       COPY ELCDTERX.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT
           PERFORM 0080-READ-CLMS      THRU 0080-EXIT
           PERFORM 0060-PROCESS-INPUT  THRU 0060-EXIT 
               UNTIL END-OF-CLMS
           IF NOT END-OF-CERT
              PERFORM 0082-FINISH-CERT THRU 0082-EXIT
                  UNTIL END-OF-CERT
           END-IF

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           GOBACK

           .

       0020-OPEN-FILES.

           OPEN INPUT  SAL-LOGICCLMS-IN 
                       SAL-LOGICCERT-IN
                OUTPUT SAL-LOGICCERT-OUT

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY ' CLAIM RECORDS IN  ' WS-CLM-IN-CNT
           DISPLAY ' CERT RECORDS IN  ' WS-CERT-IN-CNT
           DISPLAY ' CERT RECORDS OUT ' WS-CERT-OUT-CNT

           CLOSE SAL-LOGICCLMS-IN
                 SAL-LOGICCERT-IN
                 SAL-LOGICCERT-OUT

           .
       0030-EXIT.
           EXIT.

       0060-PROCESS-INPUT.

           PERFORM 0070-FIND-CERT      THRU 0070-EXIT 
               UNTIL CERT-FOUND 
                     OR END-OF-CERT

           PERFORM 0080-READ-CLMS      THRU 0080-EXIT

           MOVE 'N'                    TO WS-MATCH-SW 

           .
       0060-EXIT.
           EXIT.


       0070-FIND-CERT.

           PERFORM 0083-READ-CERT      THRU 0083-EXIT
           IF END-OF-CERT
               GO TO 0070-EXIT
           END-IF

           IF CR-ACCT-CONTROL <= DE-CNTRL1
               CONTINUE
           ELSE
               DISPLAY 'CERT ACCT CNTRL should not be GT Claim'
               DISPLAY ' CERT CNTL ' CR-ACCT-CONTROL
               DISPLAY ' CLM CNTL ' DE-CNTRL1
               PERFORM ABEND-PGM
           END-IF
  
           IF CR-ACCT-CONTROL = DE-CNTRL1
               CONTINUE
           ELSE
               PERFORM 0085-WRITE-CERT THRU 0085-EXIT
               GO TO 0070-EXIT
           END-IF

           IF CR-CERT-NO <= DE-CERT
               CONTINUE
           ELSE
               DISPLAY 'CERT NUM should not be GT Claim cert num'
               DISPLAY ' CERT NUM ' CR-CERT-NO
               DISPLAY ' CLM CERT ' DE-CERT
               PERFORM ABEND-PGM
           END-IF

           IF CR-CERT-NO = DE-CERT
               CONTINUE
           ELSE
               PERFORM 0085-WRITE-CERT THRU 0085-EXIT
               GO TO 0070-EXIT
           END-IF

           IF CR-DT = DE-EFF
               CONTINUE
           ELSE
               DISPLAY ' NO MATCH on CERT EFF DATE ' CR-FULL-CONTROL
               DISPLAY ' CLAIM CONTROL ' DE-CONTROL
           END-IF

           SET CERT-FOUND              TO TRUE
 
           IF DE-DEATH
               MOVE DE-INCUR           TO CR-DTH-DT
               MOVE DE-REPORTED        TO CR-DTH-RPT-DT
               MOVE DE-PAY             TO CR-DTH-PAY-DT
               MOVE DE-CLAIM-AMT       TO CR-DTHAMT
               MOVE DE-PAY-CODE        TO CR-DTH-PAY-CD
               MOVE DE-CLM-CAUSE       TO CR-DEATH-CAUSE
           END-IF

           IF DE-DISABILITY
               MOVE DE-INCUR           TO CR-DIS-DT
               MOVE DE-REPORTED        TO CR-DIS-RPT-DT
               MOVE DE-PAY             TO CR-DIS-PAY-DT
               MOVE DE-PAID-TO         TO CR-DIS-PTO-DT
               MOVE DE-CLAIM-AMT       TO CR-DISAMT
               MOVE DE-DAYS-DISAB      TO CR-DAYS-DISAB
               MOVE DE-PAY-CODE        TO CR-DIS-PAY-CD
               MOVE DE-CLM-CAUSE       TO CR-DISAB-CAUSE

               PERFORM 0075-INCUR-DETAIL THRU 0075-EXIT
                   VARYING INCUR-INDEX FROM +1 BY +1
                   UNTIL INCUR-INDEX > +5
           END-IF

           PERFORM 0085-WRITE-CERT     THRU 0085-EXIT
 
           .
       0070-EXIT.
           EXIT.
           
       0075-INCUR-DETAIL.

           IF CR-DIS-INCUR-DT (INCUR-INDEX) = ZEROS
               MOVE DE-INCUR           TO CR-DIS-INCUR-DT (INCUR-INDEX)
               MOVE DE-CLAIM-AMT       TO CR-INCUR-DISAMT (INCUR-INDEX)
               MOVE +6                 TO INCUR-INDEX
           END-IF
 
           .
       0075-EXIT.
           EXIT.
           
       0080-READ-CLMS.

           READ SAL-LOGICCLMS-IN 
               AT END SET END-OF-CLMS  TO TRUE
               GO TO 0080-EXIT
           END-READ

           ADD +1                      TO WS-CLM-IN-CNT

           .
       0080-EXIT.
           EXIT.

       0082-FINISH-CERT.

           PERFORM 0083-READ-CERT      THRU 0083-EXIT
           IF NOT END-OF-CERT
               PERFORM 0085-WRITE-CERT THRU 0085-EXIT
           END-IF

           .
       0082-EXIT.
           EXIT.

       0083-READ-CERT.

           READ SAL-LOGICCERT-IN INTO CERTIFICATE-RECORD
               AT END SET END-OF-CERT  TO TRUE
               GO TO 0083-EXIT
           END-READ

           ADD +1                      TO WS-CERT-IN-CNT

           .
       0083-EXIT.
           EXIT.

       0085-WRITE-CERT.
       
           WRITE SAL-LOGICCERT-OUT-REC FROM CERTIFICATE-RECORD
           ADD +1                      TO WS-CERT-OUT-CNT

           .
       0085-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.


       ABEND-PGM.
                                       COPY ELCABEND.

           EJECT
