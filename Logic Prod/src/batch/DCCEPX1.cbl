       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DCCEPX1.
       AUTHOR.        PABLO.
       DATE-COMPILED.

      *REMARKS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EPECS        ASSIGN TO SYS010.
           SELECT EXTRACT      ASSIGN TO SYS011
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE        ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  EPECS
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
                                       COPY ECSEPC01.

       FD  EXTRACT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  EXTRACT-RECORD-OUT          PIC X(220).

       FD  DISK-DATE                                                    
                                   COPY ELCDTEFD.                       

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     DCCEPX1 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-EPEC                VALUE 'Y'.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  EPEC-IN-CNT                 PIC 9(11)  VALUE ZEROS.
       77  EXTR-OUT-CNT                PIC 9(11)  VALUE ZEROS.

       01  WS-INIT-EXTRACT             PIC X(220). 
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
           12  EX-CLAIM-AMT            PIC 99999999.99-.
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
                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.                       

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0060-READ-EPEC      THRU 0060-EXIT

           PERFORM 0080-PROCESS-EPEC   THRU 0080-EXIT UNTIL
                 (END-OF-EPEC)
      *          OR (EPEC-IN-CNT > 10000)

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT
           GOBACK

           .
       0002-EXIT.
           EXIT.

       0010-INITIALIZE.

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

           MOVE 'E'                    TO EX-EOR
           MOVE EXTR-DETAIL-RECORD     TO WS-INIT-EXTRACT

           .
       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT EPECS
               OUTPUT EXTRACT
               
           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY ' EPEC IN RECORDS  ' EPEC-IN-CNT
           DISPLAY ' EXTR OUT RECORDS ' EXTR-OUT-CNT
           CLOSE EPECS
               EXTRACT

           .
       0030-EXIT.
           EXIT.

       0060-READ-EPEC.

           READ EPECS AT END
               SET END-OF-EPEC TO TRUE
           END-READ

           IF NOT END-OF-EPEC
              ADD 1 TO EPEC-IN-CNT
           END-IF

           .
       0060-EXIT.
           EXIT.


       0080-PROCESS-EPEC.

           IF EP-RECORD-ID = 'EP'
              IF (EP-REIN = 'R')
                 AND (EP-CARRIER NOT = '3' AND '4')
                 AND (EP-PURGE = ' ')
                 AND (EP-RUN-DTE = RUN-DATE)
                 AND (EP-REINCO = '300' OR '500')
                 PERFORM 0090-BUILD-EXTRACT
                                       THRU 0090-EXIT
              END-IF
           END-IF

           PERFORM 0060-READ-EPEC      THRU 0060-EXIT

           .
       0080-EXIT.
           EXIT.

       0090-BUILD-EXTRACT.

           MOVE 'AH'                   TO EX-COV-CODE
           MOVE '0171'                 TO EX-LOB
           MOVE EP-CARRIER             TO EX-CARRIER
           MOVE '20071231'             TO EX-TRANS-ACC-DT
           COMPUTE EX-CLAIM-AMT = EP-CLM-DU + EP-CLM-PV +
              EP-CLM-IBNR + EP-LOSS-RESV
           MOVE EP-STATE               TO EX-CLP-STATE
           MOVE 'E'                    TO EX-EOR
                  
           PERFORM 0100-WRITE-EXTRACT  THRU 0100-EXIT

           .
       0090-EXIT.
           EXIT.

       0100-WRITE-EXTRACT.

           WRITE EXTRACT-RECORD-OUT    FROM EXTR-DETAIL-RECORD
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
