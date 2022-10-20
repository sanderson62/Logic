       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCCDEX1.
       AUTHOR.     PABLO.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EXTR-FILE-IN     ASSIGN TO EXTRIN.

           SELECT EXTR-FILE-OUT    ASSIGN TO EXTROT
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ERACCT          ASSIGN TO ERACCTT
                   ORGANIZATION INDEXED
                   ACCESS DYNAMIC
                   RECORD KEY AM-CONTROL-PRIMARY
                   FILE STATUS ERACCT-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  EXTR-FILE-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

                                       COPY ECSEXT01.

       FD  EXTR-FILE-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  EXTR-FILE-OUT-REC           PIC X(307).

       FD  ERACCT.

                                       COPY ERCACCT.

       FD  DISK-DATE                                                    
                                   COPY ELCDTEFD.                       

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   DCCDEX1  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-EXTR               VALUE 'Y'.
       77  EXT-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  EXT-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.
       77  ERACCT-FILE-STATUS      PIC XX    VALUE '00'.
       77  PGM-SUB                 PIC S999 COMP  VALUE +158.

       01  WS-WORK-DATE                PIC 9(11).
       01  FILLER REDEFINES WS-WORK-DATE.
           05  FILLER                  PIC XXX.
           05  WS-WORK-DT.
               10  WS-WORK-DT-CC       PIC 99.
               10  WS-WORK-DT-YY       PIC 99.
               10  WS-WORK-DT-MM       PIC 99.
               10  WS-WORK-DT-DD       PIC 99.

       01  EXTR-DETAIL-RECORD.
           12  EX-TRAN-CODE            PIC X.
           12  EX-TAB1                 PIC X.
           12  EX-COV-CODE             PIC XX.
           12  EX-TAB2                 PIC X.
           12  EX-LOB                  PIC X(4).
           12  EX-TAB3                 PIC X.
           12  EX-CLAIM-NO             PIC X(7).
           12  EX-TAB4                 PIC X.
           12  EX-CERT-NO              PIC X(11).
           12  EX-TAB5                 PIC X.
           12  EX-REG-CODE             PIC X.
           12  EX-TAB6                 PIC X.
           12  EX-CARRIER              PIC X.
           12  EX-TAB7                 PIC X.
           12  EX-TRANS-ACC-DT         PIC X(8).
           12  EX-TAB8                 PIC X.
           12  EX-CLAIM-AMT            PIC -99999999.99.
           12  EX-TAB9                 PIC X.
           12  EX-CLP-STATE            PIC XX.
           12  EX-TAB10                PIC X.
           12  EX-ASS-CLAIM-AMT        PIC X.
           12  EX-TAB11                PIC X.
           12  EX-ASS-REIN-COMP        PIC X.
           12  EX-TAB12                PIC X.
           12  EX-CEDED-CLAIM-AMT      PIC X.
           12  EX-TAB13                PIC X.
           12  EX-CEDED-REIN-COMP      PIC XXX.
           12  EX-TAB14                PIC X.
           12  EX-CLAIM-INC-DT         PIC X(8).
           12  EX-TAB15                PIC X.
           12  EX-CLAIM-RPT-DT         PIC X(8).
           12  EX-TAB16                PIC X.
           12  EX-POL-NO               PIC X.
           12  EX-TAB17                PIC X.
           12  EX-CHECK-NO             PIC X(7).
           12  EX-TAB18                PIC X.
           12  EX-SAL-REC              PIC X.
           12  EX-TAB19                PIC X.
           12  EX-EOR                  PIC X.
      ******************************************************************
       01  WS-MISC.
           05  WS-ERACCT-SW            PIC X VALUE ' '.
               88  ERACCT-FINISHED           VALUE 'Y'.
           05  WS-SAVE-EXTR            PIC X(307) VALUE LOW-VALUES.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
           05  WS-DATE-R REDEFINES WS-DATE.
               10  FILLER              PIC 999.
               10  WS-CCYY             PIC 9999.
               10  WS-MM               PIC 99.
               10  WS-DD               PIC 99.

       01  WS-ABEND-FIELDS.
           12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.
           12  WS-ZERO                 PIC S9          VALUE ZERO.
           12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.
           12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.

                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.                       

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0050-PROCESS-EXTR   THRU 0050-EXIT UNTIL
              (END-OF-EXTR)
PEMTST*          OR (EXT-RECS-IN > 5000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' EXTR RECORDS READ    '  EXT-RECS-IN
           DISPLAY ' EXTR RECORDS WRITTEN '  EXT-RECS-OUT
           GOBACK

           .
       0050-PROCESS-EXTR.

           IF (DE-TRANS = 'X')
              AND (DE-REIN NOT = 'R')
              AND (DE-CLM-PROC-DT > 20061231)
              AND (DE-CLM-PROC-DT <= 20071231)
              PERFORM 0100-PROCESS-EXTR
                                       THRU 0100-EXIT
           END-IF

           PERFORM 0200-READ-EXTR      THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-EXTR.

           MOVE WS-SAVE-EXTR           TO EXTR-DETAIL-RECORD
           MOVE 'AH'                   TO EX-COV-CODE
           MOVE '0171'                 TO EX-LOB
           MOVE DE-CNUM                TO EX-CLAIM-NO
           MOVE DE-CERT                TO EX-CERT-NO
           MOVE DE-CARRIER             TO EX-CARRIER

           MOVE DE-CLM-PROC-DT         TO WS-WORK-DATE
           MOVE WS-WORK-DT             TO EX-TRANS-ACC-DT
      *    MOVE '20071231'             TO EX-TRANS-ACC-DT

           MOVE DE-CLAIM-AMT           TO EX-CLAIM-AMT
           MOVE DE-STATE               TO EX-CLP-STATE
           IF DE-CLP-STATE NOT = SPACES
              MOVE DE-CLP-STATE        TO EX-CLP-STATE
           END-IF
           MOVE DE-INCUR               TO WS-WORK-DATE
           MOVE WS-WORK-DT             TO EX-CLAIM-INC-DT
           MOVE DE-REPORTED            TO WS-WORK-DT (3:6)
           IF WS-WORK-DT-YY < 09
              MOVE 20                  TO WS-WORK-DT-CC
           ELSE
              MOVE 19                  TO WS-WORK-DT-CC
           END-IF
           MOVE WS-WORK-DT             TO EX-CLAIM-RPT-DT
           MOVE DE-CHECK               TO EX-CHECK-NO
           MOVE 'E'                    TO EX-EOR


           IF (DE-CNTRL1 = AM-CONTROL-A) AND
              ((DE-EFF < AM-EXPIRE-DT) AND
              (DE-EFF >= AM-EFFECT-DT))
              CONTINUE
           ELSE
              MOVE DTE-CLASIC-COMPANY-CD
                                       TO AM-COMPANY-CD
              MOVE DE-CONTROL (1:25)   TO AM-CONTROL-PRIMARY (2:25)
              PERFORM 0360-STARTBR-ERACCT
                                       THRU 0360-EXIT
              PERFORM 0370-READNEXT-ERACCT
                                       THRU 0370-EXIT
              PERFORM 0350-MATCH-TO-ERACCT
                                       THRU 0350-EXIT UNTIL
                 ERACCT-FINISHED
           END-IF

           MOVE AM-REI-TABLE           TO EX-CEDED-REIN-COMP
           PERFORM 0300-WRITE-EXTR     THRU 0300-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-EXTR.

           READ EXTR-FILE-IN AT END
              SET END-OF-EXTR          TO TRUE
           END-READ


           IF NOT END-OF-EXTR
              ADD 1 TO EXT-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-EXTR.

           WRITE EXTR-FILE-OUT-REC     FROM EXTR-DETAIL-RECORD
           ADD 1                       TO EXT-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0350-MATCH-TO-ERACCT.

           IF (DE-CNTRL1 > AM-CONTROL-A)
              PERFORM 0370-READNEXT-ERACCT
                                       THRU 0370-EXIT
           ELSE
              IF (DE-CNTRL1 = AM-CONTROL-A) AND
                 ((DE-EFF < AM-EXPIRE-DT)   AND
                 (DE-EFF >= AM-EFFECT-DT))
                 SET ERACCT-FINISHED TO TRUE
              ELSE
                 IF DE-CNTRL1 < AM-CONTROL-A
                    MOVE 'NOT FOUND'   TO AM-REPORT-CODE-1
                                          AM-REPORT-CODE-2
                    SET ERACCT-FINISHED TO TRUE
                 ELSE
                    PERFORM 0370-READNEXT-ERACCT
                                       THRU 0370-EXIT
                 END-IF
              END-IF
           END-IF

           .
       0350-EXIT.
           EXIT.

       0360-STARTBR-ERACCT.

           MOVE ' '                    TO WS-ERACCT-SW
           START ERACCT KEY >= AM-CONTROL-PRIMARY

           IF ERACCT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERACCT - STARTBR ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0360-EXIT.
           EXIT.

       0370-READNEXT-ERACCT.

           READ ERACCT NEXT RECORD

           IF ERACCT-FILE-STATUS = '00'
              IF AM-COMPANY-CD > DTE-CLASIC-COMPANY-CD
                 MOVE HIGH-VALUES      TO AM-CONTROL-PRIMARY
              END-IF
           ELSE
              IF ERACCT-FILE-STATUS = '10' OR '23'
                 MOVE HIGH-VALUES      TO AM-CONTROL-PRIMARY
              ELSE
                 DISPLAY ' ERROR - ERACCT - READNEXT '
                    ERACCT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0370-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT EXTR-FILE-IN
                      ERACCT
               OUTPUT EXTR-FILE-OUT

           IF ERACCT-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERACCT - OPEN ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE EXTR-FILE-IN EXTR-FILE-OUT ERACCT

           IF ERACCT-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERACCT - CLOSE ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0500-EXIT.
           EXIT.

       0600-INITIALIZE.

           MOVE SPACES                 TO EXTR-DETAIL-RECORD
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
                                          EX-TAB19

           MOVE EXTR-DETAIL-RECORD     TO WS-SAVE-EXTR
           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD
           PERFORM 0360-STARTBR-ERACCT THRU 0360-EXIT
           PERFORM 0370-READNEXT-ERACCT
                                       THRU 0370-EXIT
           PERFORM 0200-READ-EXTR      THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM SECTION.
00001 ***************************************************************** 04/14/98
00002 *                                                               * ELCABEND
00003 *                            ELCABEND.                          *    LV003
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE               CL**3
00005 *                            VMOD 2.002                              CL**2
00006 *                                                               * ELCABEND
00007 *                THIS SECTION DISPLAYS THE NECESSARY MESSAGES   * ELCABEND
00008 *            AND THEN ABENDS.                                   * ELCABEND
CIDMOD*                                                               * ELCABEND
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCABEND                        * ELCABEND
CIDMOD*                                                               * ELCABEND
00009 ***************************************************************** ELCABEND
00010 *APS-010.                                                         ELCABEND
00011      DISPLAY WS-ABEND-MESSAGE.                                    ELCABEND
00012      DISPLAY WS-ABEND-MESSAGE UPON CONSOLE.                       ELCABEND
00013                                                                   ELCABEND
00014      IF WS-ABEND-FILE-STATUS NOT = ZERO                           ELCABEND
00015          DISPLAY 'FILE STATUS = ' WS-ABEND-FILE-STATUS            ELCABEND
00016          DISPLAY 'FILE STATUS = ' WS-ABEND-FILE-STATUS            ELCABEND
00017                                  UPON CONSOLE.                    ELCABEND
00018                                                                   ELCABEND
00019      IF WS-RETURN-CODE NOT = ZERO                                 ELCABEND
00020          DISPLAY 'RETURN CODE = '  WS-RETURN-CODE                 ELCABEND
00021          DISPLAY 'RETURN CODE = '  WS-RETURN-CODE                 ELCABEND
00022                                  UPON CONSOLE.                    ELCABEND
00023                                                                   ELCABEND
00024      DISPLAY 'PROGRAM WILL NOW ABEND **************'              ELCABEND
00025      DISPLAY 'PROGRAM WILL NOW ABEND **************'              ELCABEND
00026                                  UPON CONSOLE.                    ELCABEND
00027                                                                   ELCABEND
00028      DIVIDE WS-ZERO BY WS-ZERO GIVING WS-ZERO.                    ELCABEND
           CALL 'ABORTME'
      *    GOBACK RETURNING 9999
           .
00030  APS-EXIT.                                                        ELCABEND
00031      EXIT.                                                        ELCABEND
