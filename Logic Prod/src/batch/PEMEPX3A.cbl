       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PEMEPX3A.
       AUTHOR.        PABLO.
       DATE-COMPILED.

      *REMARKS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EPECS        ASSIGN TO SYS010.
           SELECT EXTRACT      ASSIGN TO SYS011
               ORGANIZATION IS LINE SEQUENTIAL.
                                                                        
              

       DATA DIVISION.
       FILE SECTION.

       FD  EPECS
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
                                       COPY ECSEPC01.

           EJECT
       FD  EXTRACT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  EXTRACT-RECORD-OUT          PIC X(285).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     PEMEPX3A WORKING STORAGE   '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-EPEC                VALUE 'Y'.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  EPEC-IN-CNT                 PIC 9(11)  VALUE ZEROS.
       77  EXTR-OUT-CNT                PIC 9(11)  VALUE ZEROS.
       77  WS-WORK-RETRO               PIC S9(9)V99 VALUE +0 COMP-3.

       01  WS-INIT-EXTRACT             PIC X(285). 
       01  EXTRACT-RECORD.
           05  EXT-CARRIER             PIC X.
           05  EXT-TABA                PIC X.
           05  EXT-STATE               PIC XX.
           05  EXT-TABB                PIC X.
           05  EXT-ACCOUNT             PIC X(10).
           05  EXT-TAB1                PIC X.
           05  EXT-EXP-DT              PIC 9(11).
           05  EXT-TAB2                PIC X.
           05  EXT-EFF-DT              PIC 9(11).
           05  EXT-TAB3                PIC X.
           05  EXT-REIN-COMP           PIC X(6).
           05  EXT-TAB4                PIC X.
           05  EXT-RUN-DATE            PIC 9(11).
           05  EXT-TAB5                PIC X.
           05  EXT-RCD-TYPE            PIC X.
           05  EXT-TAB6                PIC X.
           05  EXT-BEN-CODE            PIC XX.
           05  EXT-TAB7                PIC X.
           05  EXT-PURGE-CODE          PIC X.
           05  EXT-TAB8                PIC X.
           05  EXT-REC-TYPE            PIC XX.
           05  EXT-TAB9                PIC X.
           05  EXT-COMM-SEQ            PIC X.
           05  EXT-TABC                PIC X.
           05  EXT-ISS-CNT             PIC 9999999.
           05  EXT-TAB10               PIC X.
           05  EXT-ISS-PRM             PIC ZZ,ZZZ,ZZ9.99.
           05  EXT-TABD                PIC X.
           05  EXT-CNC-CNT             PIC 9999999.
           05  EXT-TAB11               PIC X.
           05  EXT-CNC-PRM             PIC ZZ,ZZZ,ZZ9.99.
           05  EXT-TAB12               PIC X.
           05  EXT-EPR-78              PIC ZZZ,ZZ9.99.
           05  EXT-TAB13               PIC X.
           05  EXT-EPR-PR              PIC ZZZ,ZZ9.99.
           05  EXT-TAB14               PIC X.
           05  EXT-EPR-ST              PIC ZZZ,ZZ9.99.
           05  EXT-TAB15               PIC X.
           05  EXT-DU                  PIC ZZZ,ZZ9.99.
           05  EXT-TAB16               PIC X.
           05  EXT-PV                  PIC ZZZ,ZZ9.99.
           05  EXT-TAB17               PIC X.
           05  EXT-IBNR                PIC ZZZ,ZZ9.99.
           05  EXT-TAB18               PIC X.
           05  EXT-LR                  PIC ZZZ,ZZ9.99.
           05  EXT-TAB19               PIC X.
           05  EXT-CLM-AMT             PIC ZZZ,ZZ9.99.
           05  EXT-TAB20               PIC X.
           05  EXT-RETRO-EXP           PIC ZZZ,ZZ9.99.
           05  EXT-TAB21               PIC X.
           05  EXT-RETRO-PMTS          PIC ZZZ,ZZ9.99.
           05  EXT-TAB22               PIC X.
           05  EXT-RETRO-OTH           PIC ZZZ,ZZ9.99.
           05  EXT-TAB23               PIC X.
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
                                          EXT-TAB19
                                          EXT-TAB20
                                          EXT-TAB21
                                          EXT-TAB22
                                          EXT-TAB23
                                          EXT-TABA
                                          EXT-TABB
                                          EXT-TABC
                                          EXT-TABD
           MOVE 'E'                    TO EXT-END
           MOVE EXTRACT-RECORD         TO WS-INIT-EXTRACT

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

           IF (EP-RECORD-ID = 'EP')
              AND (EP-STATE = 'MN')
              AND (EP-RUN-DTE = 20101130)
              AND (EP-REIN NOT = 'R')
              AND (EP-RCD-TYPE = 'A')
              AND (EP-BEN-CODE = '02')
              PERFORM 0090-BUILD-EXTRACT
                                       THRU 0090-EXIT
           END-IF

      *    IF (EP-RECORD-ID = 'EP')
      *       AND (EP-ACCOUNT = '0000845800' OR '0001060400')
      *       COMPUTE WS-WORK-RETRO = EP-RETRO-EXPENSES +
      *          EP-RETRO-PAYMENTS + EP-RETRO-OTH-COMM
      *       IF WS-WORK-RETRO > ZEROS
      *          PERFORM 0090-BUILD-EXTRACT
      *                                THRU 0090-EXIT
      *       END-IF
      *    END-IF

           PERFORM 0060-READ-EPEC      THRU 0060-EXIT

           .
       0080-EXIT.
           EXIT.

       0090-BUILD-EXTRACT.

           MOVE WS-INIT-EXTRACT        TO EXTRACT-RECORD

           MOVE EP-CARRIER             TO EXT-CARRIER
           MOVE EP-STATE               TO EXT-STATE
           MOVE EP-ACCOUNT             TO EXT-ACCOUNT
           MOVE EP-EXP-DTE             TO EXT-EXP-DT
           MOVE EP-EFF-DTE             TO EXT-EFF-DT
           MOVE EP-REI-CO              TO EXT-REIN-COMP
           MOVE EP-RUN-DTE             TO EXT-RUN-DATE
           MOVE EP-RCD-TYPE            TO EXT-RCD-TYPE
           MOVE EP-BEN-CODE            TO EXT-BEN-CODE
           MOVE EP-PURGE               TO EXT-PURGE-CODE
           MOVE EP-RECORD-ID           TO EXT-REC-TYPE
           MOVE EP-ISS-CNT             TO EXT-ISS-CNT
           MOVE EP-CNC-CNT             TO EXT-CNC-CNT
           MOVE EP-ISS-PRM             TO EXT-ISS-PRM
           MOVE EP-CNC-PRM             TO EXT-CNC-PRM
           MOVE EP-PRM-78              TO EXT-EPR-78
           MOVE EP-PRM-PR              TO EXT-EPR-PR
           MOVE EP-PRM-ST              TO EXT-EPR-ST
           MOVE EP-CLM-DU              TO EXT-DU
           MOVE EP-CLM-PV              TO EXT-PV
           MOVE EP-CLM-IBNR            TO EXT-IBNR
           MOVE EP-LOSS-RESV           TO EXT-LR
           MOVE EP-CLM-AMT             TO EXT-CLM-AMT
           MOVE EP-RETRO-EXPENSES      TO EXT-RETRO-EXP
           MOVE EP-RETRO-PAYMENTS      TO EXT-RETRO-PMTS
           MOVE EP-RETRO-OTH-COMM      TO EXT-RETRO-OTH
           MOVE ZEROS                  TO EXT-COMM-SEQ
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
