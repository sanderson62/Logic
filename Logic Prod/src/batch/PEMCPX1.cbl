       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PEMCPX1.
       AUTHOR.        PABLO.
       DATE-COMPILED.

      *REMARKS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT RCALC        ASSIGN TO SYS010.
           SELECT EXTRACT      ASSIGN TO SYS011
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  RCALC 
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
           
       01  WORK-REC.
           12  W-SEQ.
               16  W-ACCT-CNTL.
                   20  W-CARR              PIC X.
                   20  W-GROUP             PIC X(6).
                   20  W-ST                PIC XX.
                   20  W-ACCT              PIC X(10).
               16  W-IG                    PIC 9.
               16  W-TYPE                  PIC XXX.
           12  W-CODE                      PIC 9.
           12  W-AMTS COMP-3.
               16  W-AMT                   PIC S9(9)V99  COMP-3.
               16  W-BASE                  PIC S9(7)V99  COMP-3.
               16  W-OVER                  PIC S9(7)V99  COMP-3.
061004         16  W-DLR-INC               PIC S9(7)V99.
061004         16  W-LMBA-FEE              PIC S9(7)V99.
061004         16  W-BANK-FEE              PIC S9(7)V99.
           12  W-PROCESS-DATE              PIC 9(7)      COMP-3.
           12  W-RECALC                    PIC X.
020305     12  W-ACCT-TYPE                 PIC X.
020305     12  W-OVER-TYPE                 PIC X.
020305     12  W-CLP-STATE                 PIC XX.
120305     12  FILLER                      PIC X.
           12  W-SPPDD-CLP                 PIC S9(7)V99  COMP-3.
           12  FILLER                      PIC X(95).

       FD  EXTRACT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  EXTRACT-RECORD-OUT          PIC X(150).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     PEMCPX1 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-RCALC               VALUE 'Y'.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  RCALC-IN-CNT                PIC 9(11)  VALUE ZEROS.
       77  EXTR-OUT-CNT                PIC 9(11)  VALUE ZEROS.

       01  WS-INIT-EXTRACT             PIC X(150).
       01  EXTRACT-RECORD.
           05  EXT-CARRIER             PIC X.
           05  EXT-TAB1                PIC X.
           05  EXT-GROUP               PIC X(6).
           05  EXT-TAB2                PIC X.
           05  EXT-STATE               PIC XX.
           05  EXT-TAB3                PIC X.
           05  EXT-ACCOUNT             PIC X(10).
           05  EXT-TAB4                PIC X.
           05  EXT-IG                  PIC X.
           05  EXT-TAB5                PIC X.
           05  EXT-TYPE                PIC XXX.
           05  EXT-TAB6                PIC X.
           05  EXT-CODE                PIC X.
           05  EXT-TAB7                PIC X.
           05  EXT-AMT                 PIC -ZZZ,ZZ9.99.
           05  EXT-TAB8                PIC X.
           05  EXT-BASE                PIC -ZZZ,ZZ9.99.
           05  EXT-TAB9                PIC X.
           05  EXT-OVER                PIC -ZZZ,ZZ9.99.
           05  EXT-TAB10               PIC X.
           05  EXT-DLR                 PIC -ZZZ,ZZ9.99.
           05  EXT-TAB11               PIC X.
           05  EXT-LMBA                PIC -ZZZ,ZZ9.99.
           05  EXT-TAB12               PIC X.
           05  EXT-BANK                PIC -ZZZ,ZZ9.99.
           05  EXT-TAB13               PIC X.
           05  EXT-RECALC              PIC X.
           05  EXT-TAB14               PIC X.
           05  EXT-ACCT-TYPE           PIC X.
           05  EXT-TAB15               PIC X.
           05  EXT-OVER-TYPE           PIC X.
           05  EXT-TAB16               PIC X.
           05  EXT-CLP-STATE           PIC XX.
           05  EXT-TAB17               PIC X.
           05  EXT-SPPDD-CLP           PIC -ZZZ,ZZ9.99.
           05  EXT-TAB18               PIC X.
           05  EXT-END                 PIC X.

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

       01  DATE-AREAS.
           05  WS-WORK-DATE            PIC 9(11).
           05  FILLER REDEFINES WS-WORK-DATE.
               10  FILLER              PIC XXX.
               10  WS-WORK-CCYY        PIC X(4).
               10  WS-WORK-MM          PIC XX.
               10  WS-WORK-DD          PIC XX.

       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

                                       COPY ELCDATE.
                                       COPY ELCDTECX.

       PROCEDURE DIVISION.

       0002-INPUT.

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0060-READ-RCALC     THRU 0060-EXIT

           PERFORM 0080-PROCESS-RCALC  THRU 0080-EXIT UNTIL
                 (END-OF-RCALC)
      *          OR (RCALC-IN-CNT > 10000)

           PERFORM 0030-CLOSE-FILES     THRU 0030-EXIT

           GOBACK

           .
       0002-EXIT.
           EXIT.

       0010-INITIALIZE.

           MOVE SPACES                 TO EXTRACT-RECORD
           MOVE ';'                    TO EXT-TAB1
                                          EXT-TAB2
                                          EXT-TAB3  
                                          EXT-TAB4 
                                          EXT-TAB5
                                          EXT-TAB6
                                          EXT-TAB7
                                          EXT-TAB8
                                          EXT-TAB9
                                          EXT-TAB10
                                          EXT-TAB11
                                          EXT-TAB12
                                          EXT-TAB13
                                          EXT-TAB14
                                          EXT-TAB15
                                          EXT-TAB16
                                          EXT-TAB17
                                          EXT-TAB18
           MOVE 'E'                    TO EXT-END
           MOVE EXTRACT-RECORD         TO WS-INIT-EXTRACT

           .
       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT RCALC
               OUTPUT EXTRACT
               
           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY ' RCALC IN RECORDS  ' RCALC-IN-CNT
           DISPLAY ' EXTR OUT RECORDS ' EXTR-OUT-CNT
           CLOSE RCALC
               EXTRACT

           .
       0030-EXIT.
           EXIT.

       0060-READ-RCALC.

           READ RCALC AT END
               SET END-OF-RCALC TO TRUE
           END-READ

           IF NOT END-OF-RCALC
              ADD 1 TO RCALC-IN-CNT
           END-IF

           .
       0060-EXIT.
           EXIT.

       0080-PROCESS-RCALC.

           PERFORM 0090-BUILD-EXTRACT  THRU 0090-EXIT

           PERFORM 0060-READ-RCALC     THRU 0060-EXIT

           .
       0080-EXIT.
           EXIT.

       0090-BUILD-EXTRACT.

           MOVE WS-INIT-EXTRACT        TO EXTRACT-RECORD
           MOVE W-CARR                 TO EXT-CARRIER
           MOVE W-GROUP                TO EXT-GROUP
           MOVE W-ST                   TO EXT-STATE
           MOVE W-ACCT                 TO EXT-ACCOUNT
           MOVE W-IG                   TO EXT-IG
           MOVE W-TYPE                 TO EXT-TYPE
           MOVE W-CODE                 TO EXT-CODE
           MOVE W-AMT                  TO EXT-AMT
           MOVE W-BASE                 TO EXT-BASE
           MOVE W-OVER                 TO EXT-OVER
           MOVE W-DLR-INC              TO EXT-DLR
           MOVE W-LMBA-FEE             TO EXT-LMBA
           MOVE W-BANK-FEE             TO EXT-BANK
           MOVE W-ACCT-TYPE            TO EXT-ACCT-TYPE
           MOVE W-OVER-TYPE            TO EXT-OVER-TYPE
           MOVE W-CLP-STATE            TO EXT-CLP-STATE
           MOVE W-RECALC               TO EXT-RECALC
           MOVE W-SPPDD-CLP            TO EXT-SPPDD-CLP

           MOVE 'E'                    TO EXT-END
           PERFORM 0100-WRITE-EXTRACT  THRU 0100-EXIT

           .
       0090-EXIT.
           EXIT.

       0100-WRITE-EXTRACT.

           WRITE EXTRACT-RECORD-OUT    FROM EXTRACT-RECORD
           ADD 1 TO EXTR-OUT-CNT

           .
       0100-EXIT.
           EXIT.


       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
