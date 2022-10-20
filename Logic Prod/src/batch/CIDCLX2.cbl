       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDCLX2.
       AUTHOR.     PABLO.
       DATE-COMPILED.
      *REMARKS.

      *             TTTTTTT     BBBBBBB     DDDDDD
      *                T        B      B    D     D
      *                T        B      B    D     D
      *                T        BBBBBBBB    D     D
      *                T        B      B    D     D
      *                T        B      B    D     D
      *                T        BBBBBBB     DDDDDD

      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 030909 CR2008100900001   PEMA  NEW PROGRAM
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELMSTR           ASSIGN TO ELMSTR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CL-CONTROL-PRIMARY
                                   FILE STATUS IS ELMSTR-FILE-STATUS.

           SELECT ELTRLR           ASSIGN TO ELTRLR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AT-CONTROL-PRIMARY
                                   FILE STATUS IS ELTRLR-FILE-STATUS.

           SELECT ELMSTR-OUT       ASSIGN TO SYS011
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE        ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  ELMSTR.

                                       COPY ELCMSTR.

       FD  ELTRLR.

                                       COPY ELCTRLR.

       FD  ELMSTR-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  ELMSTR-OUT-REC              PIC X(253).

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDCLX2  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-ELMSTR             VALUE 'Y'.
       77  CLM-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  TRL-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  CLM-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.
       77  WS-GOT-IT-SW            PIC X     VALUE ' '.
           88  WE-GOT-IT-ALL                 VALUE 'Y'.

       01  WS-SAVE-ELMSTR              PIC X(253) VALUE SPACES.
       01  ELMSTR-DETAIL-RECORD.
           12  EX-CARRIER              PIC X.
           12  EX-TAB1                 PIC X.
           12  EX-CLAIM-NO             PIC X(7).
           12  EX-TAB2                 PIC X.
           12  EX-CERT-NO              PIC X(11).
           12  EX-TAB3                 PIC X.
           12  EX-CERT-EFF-DT          PIC X(10).
           12  EX-TAB18                PIC X.
           12  EX-DENIAL-CODE          PIC XXXX.
           12  EX-TAB4                 PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-TAB5                 PIC X.
           12  EX-INSURED-LAST-NAME    PIC X(15).
           12  EX-TAB6                 PIC X.
           12  EX-INSURED-1ST-NAME     PIC X(11).
           12  EX-TAB7                 PIC X.
           12  EX-CLAIM-STATUS         PIC X.
           12  EX-TAB8                 PIC X.
           12  EX-CLAIM-TYPE           PIC X.
           12  EX-TAB9                 PIC X.
           12  EX-CERT-STATE           PIC XX.
           12  EX-TAB10                PIC X.
           12  EX-DENIAL-DT            PIC X(10).
           12  EX-TAB11                PIC X.
           12  EX-DENIAL-TYPE          PIC X.
           12  EX-TAB12                PIC X.
           12  EX-INCURRED-DT          PIC X(10).
           12  EX-TAB13                PIC X.
           12  EX-REPORTED-DT          PIC X(10).
           12  EX-TAB14                PIC X.
           12  EX-RECORDED-DT          PIC X(10).
           12  EX-TAB15                PIC X.
           12  EX-DENIAL-INFO-LINE-1   PIC X(60).
           12  EX-TAB16                PIC X.
           12  EX-DIAGNOSIS            PIC X(60).
           12  EX-TAB17                PIC X.
           12  EX-EOR                  PIC X.

      ******************************************************************
       01  WS-MISC.
           05  ELMSTR-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  ELTRLR-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  WS-DATE                 PIC 9(11)  VALUE ZEROS.
           05  PGM-SUB          COMP-3 PIC S9(04) VALUE +585.
           05  WS-RETURN-CODE   COMP   PIC S9(03) VALUE +0.
           05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.
           05  WS-ZERO          COMP-3 PIC S9(01) VALUE +0.
           05  WS-ABEND-FILE-STATUS    PIC X(02)  VALUE ZERO.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

                                       COPY ELCDATE.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT

           PERFORM 0020-INITIALIZE     THRU 0020-EXIT

           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT UNTIL
                 (END-OF-ELMSTR)
PEMTST*          OR (CLM-RECS-IN > 1000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' CLAIM RECORDS READ    '  CLM-RECS-IN
           DISPLAY ' TRLR  RECORDS READ    '  TRL-RECS-IN
           DISPLAY ' CLAIM RECORDS WRITTEN '  CLM-RECS-OUT
           GOBACK

           .
       0010-OPEN-FILES.

           OPEN INPUT ELMSTR ELTRLR
              OUTPUT ELMSTR-OUT

           .
       0010-EXIT.
           EXIT.

       0020-INITIALIZE.

           MOVE SPACES                 TO ELMSTR-DETAIL-RECORD
           MOVE ';'                    TO EX-TAB1
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
                                          EX-TAB12
                                          EX-TAB13
                                          EX-TAB14
                                          EX-TAB15
                                          EX-TAB16
                                          EX-TAB17
                                          EX-TAB18
           MOVE 'E'                    TO EX-EOR

           MOVE ELMSTR-DETAIL-RECORD   TO WS-SAVE-ELMSTR
           PERFORM 0550-START-ELMSTR   THRU 0550-EXIT
           PERFORM 0200-READ-ELMSTR    THRU 0200-EXIT

           .
       0020-EXIT.
           EXIT.

       0050-PROCESS-INPUT.

           IF CLAIM-DENIED
              PERFORM 0100-PROCESS-ELMSTR
                                       THRU 0100-EXIT
           END-IF

           PERFORM 0200-READ-ELMSTR    THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.
       0100-PROCESS-ELMSTR.

           MOVE WS-SAVE-ELMSTR         TO ELMSTR-DETAIL-RECORD

           MOVE CL-CARRIER             TO EX-CARRIER
           MOVE CL-CLAIM-NO            TO EX-CLAIM-NO
           MOVE CL-CERT-NO             TO EX-CERT-NO
           MOVE CL-CERT-ACCOUNT        TO EX-ACCOUNT
           MOVE CL-INSURED-LAST-NAME   TO EX-INSURED-LAST-NAME
           MOVE CL-INSURED-1ST-NAME    TO EX-INSURED-1ST-NAME
           MOVE CL-CLAIM-STATUS        TO EX-CLAIM-STATUS
           MOVE CL-CLAIM-TYPE          TO EX-CLAIM-TYPE
           MOVE CL-CERT-STATE          TO EX-CERT-STATE
           MOVE CL-DENIAL-TYPE         TO EX-DENIAL-TYPE

           MOVE CL-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT
                                       TO EX-CERT-EFF-DT
           END-IF

           MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT
                                       TO EX-INCURRED-DT
           END-IF

           MOVE CL-REPORTED-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT
                                       TO EX-REPORTED-DT
           END-IF

           PERFORM 0205-GET-DIAG       THRU 0205-EXIT
           IF EX-DIAGNOSIS NOT = SPACES
              INSPECT EX-DIAGNOSIS REPLACING
                 ALL X'00' BY SPACES
                 ALL ';'   BY ' '
           END-IF
           PERFORM 0210-GET-ELTRLR     THRU 0210-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-ELMSTR.

           READ ELMSTR NEXT RECORD

           IF (ELMSTR-FILE-STATUS = '10' OR '23')
              OR (CL-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-ELMSTR        TO TRUE
           ELSE
              IF ELMSTR-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ELMSTR - READ ' ELMSTR-FILE-STATUS
                 SET END-OF-ELMSTR     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-ELMSTR
              ADD 1                    TO CLM-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0205-GET-DIAG.

           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY
           MOVE +90                    TO AT-SEQUENCE-NO
           READ ELTRLR
           IF ELTRLR-FILE-STATUS = '00'
              MOVE AT-INFO-LINE-1      TO EX-DIAGNOSIS
           ELSE
              MOVE SPACES              TO EX-DIAGNOSIS
           END-IF

           .
       0205-EXIT.
           EXIT.

       0210-GET-ELTRLR.

           MOVE SPACES                 TO WS-GOT-IT-SW

           PERFORM 0220-START-ELTRLR   THRU 0220-EXIT
           IF (ELTRLR-FILE-STATUS = '00')
      *       AND (CL-CONTROL-PRIMARY = AT-CONTROL-PRIMARY (1:20))
              PERFORM 0230-READ-ELTRLR THRU 0230-EXIT UNTIL
                 WE-GOT-IT-ALL
           END-IF

           .
       0210-EXIT.
           EXIT.

       0220-START-ELTRLR.

           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY
           MOVE +93                    TO AT-SEQUENCE-NO
           START ELTRLR KEY >= AT-CONTROL-PRIMARY
           IF ELTRLR-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ELTRLR - START ' ELTRLR-FILE-STATUS
                 ' ' CL-CONTROL-PRIMARY (2:19)
           END-IF

           .
       0220-EXIT.
           EXIT.

           
       0230-READ-ELTRLR.

           READ ELTRLR NEXT RECORD

           IF (ELTRLR-FILE-STATUS = '00')
              AND (CL-CONTROL-PRIMARY = AT-CONTROL-PRIMARY (1:20))
              IF (DENIAL-TR)
                 AND (AT-DENIAL-DT NOT = SPACES AND LOW-VALUES)
                 AND (AT-RETRACTION-DT = SPACES OR LOW-VALUES)
                 MOVE AT-DENIAL-INFO-1 TO EX-DENIAL-INFO-LINE-1
                 INSPECT EX-DENIAL-INFO-LINE-1 REPLACING
                    ALL X'00' BY SPACES
                    ALL ';'   BY ' '
                 IF AT-DENIAL-REASON-CODE = LOW-VALUES
                    MOVE SPACES        TO AT-DENIAL-REASON-CODE
                 END-IF
                 MOVE AT-DENIAL-REASON-CODE
                                       TO EX-DENIAL-CODE
                 MOVE AT-DENIAL-DT     TO DC-BIN-DATE-1
                 MOVE ' '              TO DC-OPTION-CODE
                 PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
                 IF NO-CONVERSION-ERROR
                    MOVE DC-GREG-DATE-A-EDIT
                                       TO EX-DENIAL-DT
                 END-IF
                 MOVE AT-RECORDED-DT   TO DC-BIN-DATE-1
                 MOVE ' '              TO DC-OPTION-CODE
                 PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
                 IF NO-CONVERSION-ERROR
                    MOVE DC-GREG-DATE-A-EDIT
                                       TO EX-RECORDED-DT
                 END-IF
                 PERFORM 0300-WRITE-MSTR
                                       THRU 0300-EXIT
031810           SET WE-GOT-IT-ALL     TO TRUE
              END-IF
           ELSE
              SET WE-GOT-IT-ALL        TO TRUE
           END-IF

           .
       0230-EXIT.
           EXIT.

       0300-WRITE-MSTR.

           WRITE ELMSTR-OUT-REC        FROM ELMSTR-DETAIL-RECORD
           ADD 1 TO CLM-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ELMSTR ELMSTR-OUT ELTRLR

           .
       0500-EXIT.
           EXIT.

       0550-START-ELMSTR.

           MOVE LOW-VALUES             TO CL-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO CL-COMPANY-CD

           START ELMSTR KEY >= CL-CONTROL-PRIMARY

           IF ELMSTR-FILE-STATUS = '10' OR '23'
              SET END-OF-ELMSTR        TO TRUE
           ELSE
              IF ELMSTR-FILE-STATUS NOT = '00'
                 DISPLAY 'ELMSTR START     ' ELMSTR-FILE-STATUS
                 SET END-OF-ELMSTR     TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM. COPY ELCABEND.
