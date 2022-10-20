       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EL546 .
       AUTHOR.        CENTRAL STATES HEALTH AND LIFE.
                      OMAHA, NEBRASKA.
       DATE-COMPILED.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CENTRAL STATES. *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CENTRAL STATES  *
      *            *                                                   *
      *            *****************************************************
      *REMARKS.
      *        THIS PROGRAM READS THE ON LINE (VSAM) PENDING BUSINESS
      *        FILE AND
      *        CREATES AN EXTRACT RECORD FOR EACH RECORD THAT HAS A
      *        MICROFILM NUMBER GREATER THAN ZERO.
      *                                     ONCE THE EXTRACT HAS BEEN
      *        WRITTEN THE PENDING BUSINESS RECORD IS REWRITTEN WITH
      *        ZEROS IN THE MICROFILM NUMBER FIELD.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EXTRACT-FILE     ASSIGN TO SYS010
               ORGANIZATION LINE SEQUENTIAL.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT ERPNDB           ASSIGN TO    ERPNDB
                                   ORGANIZATION INDEXED
                                   ACCESS       DYNAMIC
                                   RECORD KEY   PB-CONTROL-PRIMARY
                                   FILE STATUS  ERPNDB-FILE-STATUS.

           SELECT ERPNDM           ASSIGN TO    ERPNDM
                                   ORGANIZATION INDEXED
                                   ACCESS       DYNAMIC
                                   RECORD KEY   PM-CONTROL-PRIMARY
                                   FILE STATUS  ERPNDM-FILE-STATUS.

           SELECT ERACCT           ASSIGN TO    ERACCT
                                   ORGANIZATION INDEXED
                                   ACCESS       DYNAMIC
                                   RECORD KEY   AM-CONTROL-PRIMARY
                                   FILE STATUS  ERACCT-FILE-STATUS.

       EJECT
       DATA DIVISION.

       FILE SECTION.

       FD  EXTRACT-FILE
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  EXTRACT-RECORD              PIC X(448).
       EJECT
       FD  DISK-DATE
                                       COPY ELCDTEFD.
       FD  ERPNDB.

           COPY ERCPNDB.
       FD  ERPNDM.

           COPY ERCPNDM.
       FD  ERACCT.

           COPY ERCACCT.
       EJECT
       WORKING-STORAGE SECTION.
       01  LCP-ABND-CODE               PIC S999 COMP VALUE +519.
       77  LCP-ONCTR-01                PIC S9(3) COMP-3 VALUE ZERO.
       77  LCP-ONCTR-02                PIC S9(3) COMP-3 VALUE ZERO.
       77  FILLER PIC X(32) VALUE '********************************'.
       77  FILLER PIC X(32) VALUE '*     EL546 WORKING-STORAGE    *'.
       77  FILLER PIC X(32) VALUE '********** VMOD=2.001 **********'.


       01  FILLER                      COMP-3.
           12  WS-REPORT-SW            PIC S9      VALUE +1.
           12  WS-OUT-CNT              PIC S9(9)   VALUE ZERO.
           12  WS-RW-CNT               PIC S9(9)   VALUE ZERO.
           12  WS-IN-CNT               PIC S9(9)   VALUE ZERO.
           12  WS-RETURN-CODE          PIC S9(3)   VALUE ZERO.
           12  WS-ZERO                 PIC S9      VALUE ZERO.


       01  FILLER                      COMP SYNC.
           12  PGM-SUB                 PIC S9(4)   VALUE +546.
           12  WS-INDEX                PIC S9(4)   VALUE ZERO.

       01  FILLER.
           12  WS-WORK-MICROFILM       PIC 9(9)    VALUE ZEROS.
           12  FILLER REDEFINES WS-WORK-MICROFILM.
               16  FILLER              PIC X(4).
               16  WS-WORK-CID-NO      PIC X(5).
           12  WS-DISPLAY-CNT          PIC ZZZ,ZZZ,ZZ9.
           12  WS-EOF-SW               PIC X       VALUE ' '.
               88  THERE-ARE-NO-MORE-RECORDS       VALUE 'Y'.
           12  ERPNDB-FILE-STATUS      PIC X(2)  VALUE ZEROS.
           12  ERPNDM-FILE-STATUS      PIC X(2)  VALUE ZEROS.
           12  ERACCT-FILE-STATUS      PIC X(2)  VALUE ZEROS.
           12  ABEND-CODE              PIC X(4).
           12  ABEND-OPTION            PIC X.
           12  OLC-REPORT-NAME         PIC X(5)  VALUE 'EL546'.
           12  X                       PIC X     VALUE SPACE.
           12  WS-COMPANY-CD           PIC X     VALUE SPACE.
           12  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.
           12  WS-ABEND-FILE-STATUS    PIC XX     VALUE ZERO.

           12  WS-FILE-ERROR-MESSAGE.
               16  FILLER              PIC X(24)  VALUE
                   'ERROR OCCURED OPENING - '.
               16  WS-FEM-FILE-NAME    PIC X(8).
       EJECT

       01  EXT-RECORD.
           12  EXT-CARRIER             PIC X.
           12  EXT-STATE               PIC XX.
           12  EXT-ACCOUNT             PIC X(10).
           12  EXT-CERT-NO             PIC X(11).
           12  EXT-CID-NUMBER          PIC X(5).
           12  EXT-INS-LAST-NAME       PIC X(30).
           12  EXT-INS-FIRST-NAME      PIC X(15).
           12  EXT-INS-MID-INIT        PIC X.
           12  EXT-INS-AGE             PIC ZZ9.
           12  EXT-INS-ADDR1           PIC X(30).
           12  EXT-INS-ADDR2           PIC X(30).
           12  EXT-INS-CITY-ST         PIC X(30).
           12  EXT-INS-ZIP             PIC X(9).
           12  EXT-JT-LAST-NAME        PIC X(30).
           12  EXT-JT-FIRST-NAME       PIC X(15).
           12  EXT-JT-MID-INIT         PIC X.
           12  EXT-JT-AGE              PIC ZZ9.
           12  EXT-EFF-DATE            PIC X(10).
           12  EXT-EXP-DATE            PIC X(10).
           12  EXT-1ST-PMT-DATE        PIC X(10).
           12  EXT-TERM                PIC ZZ9.
           12  EXT-APR                 PIC ZZ9.9(4).
           12  EXT-LOAN-TERM           PIC ZZ9.
           12  EXT-LF-D-L-N-T          PIC X.
           12  EXT-LF-SIN-JT           PIC X.
           12  EXT-LF-BENEFIT          PIC ZZZ,ZZZ,ZZ9.99.
           12  EXT-LF-PREMIUM          PIC Z,ZZZ,ZZZ.99.
           12  EXT-LF-ALT-BENEFIT      PIC ZZZ,ZZZ,ZZ9.99.
           12  EXT-LF-ALT-PREMIUM      PIC Z,ZZZ,ZZZ.99.
           12  EXT-AH-SIN-JT           PIC X.
           12  EXT-AH-BENEFIT          PIC Z,ZZZ,ZZZ.99.
           12  EXT-AH-PREMIUM          PIC Z,ZZZ,ZZZ.99.
           12  EXT-FULL-TERM           PIC X.
           12  EXT-CRITICAL-PERIOD     PIC ZZ9.
           12  EXT-AH-WAIT             PIC XXX.
           12  EXT-CRED-BENE-NAME      PIC X(25).
           12  EXT-CRED-BENE-ADDR      PIC X(30).
           12  EXT-CRED-BENE-CITYST    PIC X(30).
           12  EXT-CRED-BENE-ZIP       PIC X(09).

       EJECT
           COPY ELCDATE.

           COPY ELCDTECX.

           COPY ELCDTEVR.

       EJECT
       PROCEDURE DIVISION.

       0000-DATE-CARD-READ SECTION.    COPY ELCDTERX.

       0010-MAIN-LOGIC.

           PERFORM 0020-INITIALIZE     THRU 0020-EXIT

           PERFORM 0040-PROCESS-ERPNDB THRU 0040-EXIT UNTIL
              THERE-ARE-NO-MORE-RECORDS

           PERFORM 5000-CLOSE-FILES    THRU 5000-EXIT

           MOVE WS-IN-CNT              TO WS-DISPLAY-CNT
           DISPLAY ' RECORDS READ       ' WS-DISPLAY-CNT
           MOVE WS-RW-CNT              TO WS-DISPLAY-CNT
           DISPLAY ' RECORDS RE WRITTEN ' WS-DISPLAY-CNT
           MOVE WS-OUT-CNT             TO WS-DISPLAY-CNT
           DISPLAY ' RECORDS WRITTEN    ' WS-DISPLAY-CNT

           GOBACK
           .
       0020-INITIALIZE.

           MOVE ' '                    TO WS-EOF-SW
           PERFORM 4000-OPEN-FILES     THRU 4000-EXIT
           PERFORM 0030-START-ERPNDB   THRU 0030-EXIT
           PERFORM 0050-READ-ERPNDB    THRU 0050-EXIT

           .
       0020-EXIT.
           EXIT.

       0030-START-ERPNDB.

           MOVE LOW-VALUES             TO PB-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO PB-COMPANY-CD

           START ERPNDB KEY IS NOT < PB-CONTROL-PRIMARY

           IF ERPNDB-FILE-STATUS = '00'
              CONTINUE
           ELSE
              MOVE ' ERPNDB START ERROR '
                                       TO WS-ABEND-MESSAGE
              MOVE ERPNDB-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0030-EXIT.
           EXIT.

       0040-PROCESS-ERPNDB.

           IF PB-ISSUE
              CONTINUE
      *       IF PB-I-MICROFILM-NO NOT NUMERIC
      *          MOVE +0               TO PB-I-MICROFILM-NO
      *       END-IF
      *       IF PB-I-MICROFILM-NO > +0
      *          PERFORM 0060-BUILD-EXTRACT
      *                                THRU 0060-EXIT
      *          PERFORM 0080-WRITE-EXTRACT
      *                                THRU 0080-EXIT
      *          MOVE +0               TO PB-I-MICROFILM-NO
      *          PERFORM 0070-REWRITE-ERPNDB
      *                                THRU 0070-EXIT
      *       END-IF
           END-IF

           PERFORM 0050-READ-ERPNDB THRU 0050-EXIT

           .
       0040-EXIT.
           EXIT.

       0050-READ-ERPNDB.

           READ ERPNDB NEXT RECORD

           IF ERPNDB-FILE-STATUS = '10' OR '23'
              SET THERE-ARE-NO-MORE-RECORDS TO TRUE
           ELSE
              IF ERPNDB-FILE-STATUS NOT = '00'
                 MOVE ' ERPNDB READ  ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ERPNDB-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           IF NOT THERE-ARE-NO-MORE-RECORDS
              ADD +1                   TO WS-IN-CNT
           END-IF

           .
       0050-EXIT.
           EXIT.
       0060-BUILD-EXTRACT.

           IF (PB-I-LF-BENEFIT-CD NOT = CLAS-I-BEN (CLAS-INDEXL)) AND
              (PB-I-LF-BENEFIT-CD NOT = SPACES AND ZEROS)
              PERFORM VARYING CLAS-INDEXL FROM CLAS-STARTL BY +1 UNTIL
                (PB-I-LF-BENEFIT-CD = CLAS-I-BEN (CLAS-INDEXL)) OR
                (CLAS-INDEXL > CLAS-MAXL)
              END-PERFORM
           END-IF

           IF (PB-I-AH-BENEFIT-CD NOT = CLAS-I-BEN (CLAS-INDEXA)) AND
              (PB-I-AH-BENEFIT-CD NOT = SPACES AND ZEROS)
              PERFORM VARYING CLAS-INDEXA FROM CLAS-STARTA BY +1 UNTIL
                (PB-I-AH-BENEFIT-CD = CLAS-I-BEN (CLAS-INDEXA)) OR
                (CLAS-INDEXA > CLAS-MAXA)
              END-PERFORM
           END-IF

           MOVE SPACES                 TO EXTRACT-RECORD
                                          EXT-RECORD
           MOVE PB-CARRIER             TO EXT-CARRIER
           MOVE PB-STATE               TO EXT-STATE
           MOVE PB-ACCOUNT             TO EXT-ACCOUNT
           MOVE PB-CERT-NO             TO EXT-CERT-NO
           MOVE PB-I-INSURED-LAST-NAME TO EXT-INS-LAST-NAME
           MOVE PB-I-INSURED-FIRST-NAME
                                       TO EXT-INS-FIRST-NAME
           MOVE PB-I-INSURED-MIDDLE-INIT
                                       TO EXT-INS-MID-INIT
           MOVE PB-I-AGE               TO EXT-INS-AGE
           MOVE PB-I-JOINT-LAST-NAME   TO EXT-JT-LAST-NAME
           MOVE PB-I-JOINT-FIRST-NAME  TO EXT-JT-FIRST-NAME
           MOVE PB-I-JOINT-MIDDLE-INIT TO EXT-JT-MID-INIT
           MOVE PB-I-JOINT-AGE         TO EXT-JT-AGE
           MOVE PB-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EXT-EFF-DATE
           ELSE
              MOVE SPACES              TO EXT-EFF-DATE
           END-IF

           IF PB-I-LF-EXPIRE-DT    = LOW-VALUES OR SPACES
              MOVE PB-I-AH-EXPIRE-DT   TO DC-BIN-DATE-1
           ELSE
              MOVE PB-I-LF-EXPIRE-DT   TO DC-BIN-DATE-1
           END-IF
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EXT-EXP-DATE
           END-IF

           MOVE PB-I-1ST-PMT-DT        TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EXT-1ST-PMT-DATE
           END-IF

           IF (PB-I-LF-TERM > +0) AND
              (CLAS-I-EP (CLAS-INDEXL) = 'B')
              COMPUTE PB-I-LF-TERM = PB-I-LF-TERM + +1
           END-IF
           IF PB-I-LF-TERM = +0
              MOVE PB-I-AH-TERM        TO EXT-TERM
           ELSE
              MOVE PB-I-LF-TERM        TO EXT-TERM
           END-IF

           MOVE PB-I-LOAN-APR          TO EXT-APR
           MOVE PB-I-LOAN-TERM         TO EXT-LOAN-TERM
           IF PB-I-LF-BENEFIT-CD = ZEROS OR SPACES
              CONTINUE
           ELSE
              MOVE 'S'                 TO EXT-LF-SIN-JT
              IF CLAS-I-JOINT (CLAS-INDEXL) = 'J'
                 MOVE 'J'              TO EXT-LF-SIN-JT
              END-IF
              IF CLAS-I-RL-AH (CLAS-INDEXL) = 'R'
                 MOVE 'D'              TO EXT-LF-D-L-N-T
              ELSE
                 IF CLAS-I-RL-AH (CLAS-INDEXL) = 'L'
                    MOVE 'L'           TO EXT-LF-D-L-N-T
                 END-IF
              END-IF
              IF CLAS-I-EP (CLAS-INDEXL) = 'N'
                 MOVE 'N'              TO EXT-LF-D-L-N-T
              END-IF
              IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'T' OR 'U' OR 'V'
                 MOVE 'T'              TO EXT-LF-D-L-N-T
              END-IF
              IF CLAS-I-EP (CLAS-INDEXL) = 'B'
                 MOVE PB-I-LF-ALT-BENEFIT-AMT
                                       TO EXT-LF-ALT-BENEFIT
                 MOVE PB-I-LF-ALT-PREMIUM-AMT
                                       TO EXT-LF-ALT-PREMIUM
              END-IF
           END-IF

           MOVE PB-I-LF-BENEFIT-AMT    TO EXT-LF-BENEFIT
           MOVE PB-I-LF-PREMIUM-AMT    TO EXT-LF-PREMIUM
           MOVE PB-I-AH-BENEFIT-AMT    TO EXT-AH-BENEFIT
           MOVE PB-I-AH-PREMIUM-AMT    TO EXT-AH-PREMIUM

           IF PB-I-AH-BENEFIT-CD = SPACES OR ZEROS
              CONTINUE
           ELSE
              MOVE 'S'                 TO EXT-AH-SIN-JT
              IF CLAS-I-JOINT (CLAS-INDEXA) = 'J'
                 MOVE 'J'              TO EXT-AH-SIN-JT
              END-IF
              IF CLAS-I-CALC-TYPE (CLAS-INDEXA) NOT = 'C'
                 MOVE 'Y'              TO EXT-FULL-TERM
              ELSE
                 MOVE PB-I-AH-CRIT-PER TO EXT-CRITICAL-PERIOD
              END-IF
              MOVE CLAS-I-AB3 (CLAS-INDEXA)
                                       TO EXT-AH-WAIT
           END-IF

           MOVE PB-CONTROL-PRIMARY     TO PM-CONTROL-PRIMARY
           READ ERPNDM
           IF ERPNDM-FILE-STATUS = '10' OR '23'
              MOVE SPACES              TO EXT-INS-ADDR1
                                          EXT-INS-ADDR2
                                          EXT-INS-CITY-ST
                                          EXT-INS-ZIP
                                          EXT-CRED-BENE-NAME
                                          EXT-CRED-BENE-ADDR
                                          EXT-CRED-BENE-CITYST
                                          EXT-CRED-BENE-ZIP
           ELSE
              IF ERPNDM-FILE-STATUS NOT = '00'
                 MOVE  ' ERPNDM READ    ERROR - JOB WILL ABEND'
                                       TO WS-ABEND-MESSAGE
                 MOVE ERPNDM-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              ELSE
                 MOVE PM-ADDRESS-LINE-1
                                       TO EXT-INS-ADDR1
                 MOVE PM-ADDRESS-LINE-2
                                       TO EXT-INS-ADDR2
                 MOVE PM-CITY-STATE    TO EXT-INS-CITY-ST
                 MOVE PM-ZIP           TO EXT-INS-ZIP
                 MOVE PM-CRED-BENE-NAME
                                       TO EXT-CRED-BENE-NAME
                 MOVE PM-CRED-BENE-ADDR
                                       TO EXT-CRED-BENE-ADDR
                 MOVE PM-CRED-BENE-CTYST
                                       TO EXT-CRED-BENE-CITYST
                 MOVE PM-CRED-BENE-ZIP TO EXT-CRED-BENE-ZIP
              END-IF
           END-IF
           IF EXT-CRED-BENE-NAME = SPACES
              PERFORM 0090-START-ERACCT
                                       THRU 0090-EXIT
              PERFORM 0095-READ-ERACCT THRU 0095-EXIT
              IF ERACCT-FILE-STATUS = '00'
                 IF (PB-COMPANY-CD = AM-COMPANY-CD) AND
                    (PB-CARRIER    = AM-CARRIER)    AND
                    (PB-GROUPING   = AM-GROUPING)   AND
                    (PB-STATE      = AM-STATE)      AND
                    (PB-ACCOUNT    = AM-ACCOUNT)    AND
                    (PB-CERT-EFF-DT < AM-EXPIRATION-DT) AND
                    (PB-CERT-EFF-DT NOT < AM-EFFECTIVE-DT)
                    MOVE AM-NAME        TO EXT-CRED-BENE-NAME
                    MOVE AM-ADDRS       TO EXT-CRED-BENE-ADDR
                    MOVE AM-CITY        TO EXT-CRED-BENE-CITYST
                    MOVE AM-ZIP         TO EXT-CRED-BENE-ZIP
                 END-IF
              END-IF
           END-IF

      *    MOVE PB-I-MICROFILM-NO       TO WS-WORK-MICROFILM
           MOVE WS-WORK-CID-NO          TO EXT-CID-NUMBER

           .
       0060-EXIT.
           EXIT.

       0070-REWRITE-ERPNDB.

           REWRITE PENDING-BUSINESS

           IF ERPNDB-FILE-STATUS  = '00'
              CONTINUE
           ELSE
              MOVE  ' ERPNDB REWRITE ERROR - JOB WILL ABEND'
                                       TO WS-ABEND-MESSAGE
              MOVE ERPNDB-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           ADD +1                      TO WS-RW-CNT

           .
       0070-EXIT.
           EXIT.

       0080-WRITE-EXTRACT.

           MOVE EXT-RECORD             TO EXTRACT-RECORD
           WRITE EXTRACT-RECORD
           ADD +1                      TO WS-OUT-CNT

           .
       0080-EXIT.
           EXIT.

       0090-START-ERACCT.

           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY
           MOVE PB-COMPANY-CD          TO AM-COMPANY-CD
           MOVE PB-CARRIER             TO AM-CARRIER
           MOVE PB-GROUPING            TO AM-GROUPING
           MOVE PB-STATE               TO AM-STATE
           MOVE PB-ACCOUNT             TO AM-ACCOUNT
           MOVE PB-CERT-EFF-DT         TO AM-EXPIRATION-DT
           START ERACCT KEY IS > AM-CONTROL-PRIMARY
           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY 'ERROR CITY'
           END-IF

           .
       0090-EXIT.
           EXIT.

       0095-READ-ERACCT.

           READ ERACCT NEXT RECORD

           IF ERACCT-FILE-STATUS = '00' OR '10' OR '23'
              CONTINUE
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 MOVE ' ERACCT READ  ERROR '
                                       TO WS-ABEND-MESSAGE
                 MOVE ERACCT-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0095-EXIT.
           EXIT.

       4000-OPEN-FILES.

           OPEN I-O    ERPNDB
                INPUT  ERPNDM
                       ERACCT
                OUTPUT EXTRACT-FILE

           IF ERPNDB-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE  ' ERPNDB OPEN ERROR - JOB WILL ABEND'
                                       TO WS-ABEND-MESSAGE
              MOVE ERPNDB-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERPNDM-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE  ' ERPNDM OPEN ERROR - JOB WILL ABEND'
                                       TO WS-ABEND-MESSAGE
              MOVE ERPNDM-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERACCT-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE  ' ERACCT OPEN ERROR - JOB WILL ABEND'
                                       TO WS-ABEND-MESSAGE
              MOVE ERACCT-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       4000-EXIT.
           EXIT.

       5000-CLOSE-FILES.

           CLOSE EXTRACT-FILE
                 ERPNDB
                 ERPNDM
                 ERACCT

           IF ERPNDB-FILE-STATUS  = '00'
              CONTINUE
           ELSE
              MOVE  ' ERPNDB CLOSE ERROR - JOB WILL ABEND'
                                       TO WS-ABEND-MESSAGE
              MOVE ERPNDB-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERPNDM-FILE-STATUS  = '00'
              CONTINUE
           ELSE
              MOVE  ' ERPNDM CLOSE ERROR - JOB WILL ABEND'
                                       TO WS-ABEND-MESSAGE
              MOVE ERPNDM-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERACCT-FILE-STATUS  = '00'
              CONTINUE
           ELSE
              MOVE  ' ERACCT CLOSE ERROR - JOB WILL ABEND'
                                       TO WS-ABEND-MESSAGE
              MOVE ERACCT-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       5000-EXIT.
           EXIT.

       8500-DATE-CONVERSION SECTION.   COPY ELCDCS.


       EJECT

       ABEND-PGM SECTION.          COPY ELCABEND.
