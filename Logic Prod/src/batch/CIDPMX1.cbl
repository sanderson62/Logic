       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDPMX1.
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
      * 111108   2008102900001   PEMA  NEW PROGRAM.
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERPNDM           ASSIGN TO ERPNDM
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS PM-CONTROL-PRIMARY
                                   FILE STATUS IS ERPNDM-FILE-STATUS.

           SELECT ERPNDB           ASSIGN TO ERPNDB
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS PB-CONTROL-PRIMARY
                                   FILE STATUS IS ERPNDB-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT EXTR-OUT         ASSIGN TO EXTROT
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  ERPNDM.

           COPY ERCPNDM.

       FD  ERPNDB.

           COPY ERCPNDB.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  EXTR-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  EXTR-OUT-REC                PIC X(800).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDPMX2  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  WS-DIS-DATE                 PIC 9(8)  VALUE ZEROS.

           01  WS-MISC.
           05  WS-WORK-DT              PIC 9(8)  VALUE ZEROS.
           05  WS-WORK-DTR REDEFINES WS-WORK-DT.
               10  WS-WORK-DT-CCYY      PIC X(4).
               10  WS-WORK-DT-MM        PIC XX.
               10  WS-WORK-DT-DD        PIC XX.
           05  PGM-SUB                 PIC S999 COMP  VALUE +158.
           05  WS-EOF-SW               PIC X      VALUE SPACES.
               88  END-OF-ERPNDM                  VALUE 'Y'.
           05  ERPNDM-RECS-IN          PIC 9(9)   VALUE ZEROS.
           05  EXTR-RECS-OUT           PIC 9(9)   VALUE ZEROS.
           05  S1                      PIC S999   VALUE +0 COMP-3.

072403     05  WS-SAVE-EXTR            PIC X(800) VALUE LOW-VALUES.
072403     05  ERPNDM-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  ERPNDB-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  WS-DATE                 PIC 9(11)  VALUE ZEROS.

030404**** PROGRAM ABEND FIELDS
030404     05  WS-RETURN-CODE          PIC S9(03) VALUE +0.
030404     05  WS-ZERO                 PIC S9(01) VALUE +0.
030404     05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.
030404     05  WS-ABEND-FILE-STATUS    PIC X(02)  VALUE ZERO.


       01  EXTR-DETAIL-RECORD.
           12  EX-CARRIER              PIC X.
           12  EX-TAB1                 PIC X.
           12  EX-GROUPING             PIC X(6).
           12  EX-TAB2                 PIC X.
           12  EX-STATE                PIC XX.
           12  EX-TAB3                 PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-TAB4                 PIC X.
           12  EX-CERT-EFF-DT          PIC X(10).
           12  EX-TAB5                 PIC X.
           12  EX-CERT-NO              PIC X(11).
           12  EX-TAB6                 PIC X.
           12  EX-CRED-BENE-NAME       PIC X(30).
           12  EX-TAB7                 PIC X.
           12  EX-CRED-BENE-ADDR       PIC X(30).
           12  EX-TAB8                 PIC X.
           12  EX-CRED-BENE-ADDR2      PIC X(30).
           12  EX-TAB9                 PIC X.
           12  EX-CRED-BENE-CITYST     PIC X(30).
           12  EX-TAB10                PIC X.
           12  EX-CRED-BENE-ZIP        PIC X(9).
           12  EX-TAB11                PIC X.
           12  EX-EOR                  PIC X.

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

030404 PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0050-PROCESS-ERPNDM THRU 0050-EXIT UNTIL
              (END-OF-ERPNDM)
PEMTST*       OR (ERPNDM-RECS-IN > 1000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' MAIL RECORDS READ    '  ERPNDM-RECS-IN
           DISPLAY ' EXTR RECORDS WRITTEN '  EXTR-RECS-OUT

           GOBACK

           .
       0050-PROCESS-ERPNDM.

           IF PM-CRED-BENE-NAME NOT = SPACES
              PERFORM 0100-PROCESS-ERPNDM
                                       THRU 0100-EXIT
           END-IF

           PERFORM 0200-READ-ERPNDM    THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-ERPNDM.

           MOVE WS-SAVE-EXTR           TO EXTR-DETAIL-RECORD

           MOVE PM-CONTROL-PRIMARY     TO PB-CONTROL-PRIMARY
           READ ERPNDB
           IF ERPNDB-FILE-STATUS NOT = '00'
              DISPLAY ' ERPNDB NOT FOUND ' PM-ENTRY-BATCH ' '
                 PM-BATCH-SEQ-NO
              GO TO 0100-EXIT
           END-IF

           MOVE PB-CARRIER             TO EX-CARRIER
           MOVE PB-GROUPING            TO EX-GROUPING
           MOVE PB-STATE               TO EX-STATE
           MOVE PB-ACCOUNT             TO EX-ACCOUNT
           MOVE PB-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT
                                       TO EX-CERT-EFF-DT
           END-IF
           MOVE PB-CERT-NO             TO EX-CERT-NO

           MOVE PM-CRED-BENE-NAME      TO EX-CRED-BENE-NAME
           MOVE PM-CRED-BENE-ADDR      TO EX-CRED-BENE-ADDR
           MOVE PM-CRED-BENE-ADDR2     TO EX-CRED-BENE-ADDR2
           MOVE PM-CRED-BENE-CTYST     TO EX-CRED-BENE-CITYST
           MOVE PM-CRED-BENE-ZIP       TO EX-CRED-BENE-ZIP

           PERFORM 0300-WRITE-EXTR     THRU 0300-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-ERPNDM.

           READ ERPNDM NEXT RECORD

           IF (ERPNDM-FILE-STATUS = '10' OR '23')
              OR (PM-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-ERPNDM        TO TRUE
           ELSE
              IF ERPNDM-FILE-STATUS NOT = '00'
                 DISPLAY 'ERROR ON ERPNDM - READ NEXT '
                    ERPNDM-FILE-STATUS
                 SET END-OF-ERPNDM     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-ERPNDM
              ADD 1                    TO ERPNDM-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-EXTR.

           WRITE EXTR-OUT-REC          FROM EXTR-DETAIL-RECORD
           ADD 1 TO EXTR-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

072403     OPEN INPUT ERPNDM ERPNDB
               OUTPUT EXTR-OUT

           IF ERPNDM-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY 'ERROR ON ERPNDM - OPEN ' ERPNDM-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERPNDB-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY 'ERROR ON ERPNDB - OPEN ' ERPNDB-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE EXTR-OUT ERPNDM ERPNDB

           IF ERPNDM-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY 'ERROR ON ERPNDM - CLOSE ' ERPNDM-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERPNDB-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY 'ERROR ON ERPNDB - CLOSE ' ERPNDB-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0500-EXIT.
           EXIT.

       0550-START-ERPNDM.

           MOVE LOW-VALUES             TO PM-CONTROL-PRIMARY

030404     MOVE DTE-CLASIC-COMPANY-CD  TO PM-COMPANY-CD

           START ERPNDM KEY IS NOT < PM-CONTROL-PRIMARY

           IF ERPNDM-FILE-STATUS = '10' OR '23'
              SET END-OF-ERPNDM        TO TRUE
           ELSE
              IF ERPNDM-FILE-STATUS NOT = '00'
                 DISPLAY 'ERROR ON ERPNDM - START ' ERPNDM-FILE-STATUS
                 SET END-OF-ERPNDM     TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.


       0600-INITIALIZE.

           MOVE SPACES                 TO EXTR-DETAIL-RECORD
052704     MOVE ';'                    TO EX-TAB1
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
           MOVE 'E'                    TO EX-EOR

           MOVE EXTR-DETAIL-RECORD     TO WS-SAVE-EXTR
           PERFORM 0550-START-ERPNDM   THRU 0550-EXIT
           PERFORM 0200-READ-ERPNDM    THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

030404 ABEND-PGM. COPY ELCABEND.
