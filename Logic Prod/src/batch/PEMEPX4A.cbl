       PROGRAM-ID.    PEMEPX4.
       AUTHOR.        PABLO.
       DATE-COMPILED.

      *REMARKS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EPECS            ASSIGN TO SYS010.
           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT EXTRACT          ASSIGN TO SYS011
               ORGANIZATION IS LINE SEQUENTIAL.
                                                                        
              

       DATA DIVISION.
       FILE SECTION.

       FD  EPECS
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
                                       COPY ECSEPC01.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  EXTRACT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS V.

       01  EXTRACT-HEAD-OUT            PIC X(400).
       01  EXTRACT-RECORD-OUT          PIC X(400).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '    PEMEPX4A WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-EPEC                VALUE 'Y'.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  EPEC-IN-CNT                 PIC 9(11)  VALUE ZEROS.
       77  EXTR-OUT-CNT                PIC 9(11)  VALUE ZEROS.
       77  WS-LAST-MONTH-DT            PIC 9(11)  VALUE ZEROS.
       77  S1                          PIC S9(5)  VALUE +0 COMP-3.
       77  WS-S1                       PIC S999   VALUE +0 COMP-3.
       77  WS-S2                       PIC S999   VALUE +0 COMP-3.
       77  WS-S3                       PIC S999   VALUE +0 COMP-3.
       77  WS-WORK-TAX                 PIC S9(7)V9(5) VALUE +0 COMP-3.
       77  ERACCT-FILE-STATUS          PIC XX VALUE LOW-VALUES.
       77  ERRTBL-FILE-STATUS          PIC XX VALUE LOW-VALUES.
       77  ERACCT-EOF-SW               PIC X  VALUE SPACES.
           88  END-OF-ERACCT                  VALUE 'Y'.
       77  ERRTBL-EOF-SW               PIC X  VALUE SPACES.
           88  END-OF-ERRTBL                  VALUE 'Y'.
       77  WS-START-SW                 PIC X  VALUE ' '.
           88  STARTED-EXTR              VALUE 'Y'.
       01  WS-LAST-YEAR-END-DT         PIC 9(11)  VALUE ZEROS.
       01  WS-CURRENT-KEY.
           05  WS-CONTROL              PIC X(31)  VALUE LOW-VALUES.
           05  WS-REI-CO               PIC X(6)   VALUE LOW-VALUES.

       01  HEAD-RECORD.
           05  FILLER                 PIC X(4)    VALUE 'CARR'.
           05  HD-TAB1                PIC X.
           05  FILLER                 PIC X(5)    VALUE 'GROUP'.
           05  HD-TAB2                PIC X.
           05  FILLER                 PIC X(5)    VALUE 'STATE'.
           05  HD-TAB3                PIC X.
           05  FILLER                 PIC X(7)    VALUE 'ACCOUNT'.
           05  HD-TAB4                PIC X.
           05  FILLER                 PIC X(6)    VALUE 'EXP DT'.
           05  HD-TAB5                PIC X.
           05  FILLER                 PIC X(6)    VALUE 'EFF DT'.
           05  HD-TAB6                PIC X.
           05  FILLER                 PIC X(9)    VALUE 'REIN COMP'.
           05  HD-TAB7                PIC X.
           05  FILLER                 PIC X(4)    VALUE 'TYPE'.
           05  HD-TAB8                PIC X.
           05  FILLER                 PIC X(6)    VALUE 'BEN CD'.
           05  HD-TAB9                PIC X.
           05  FILLER                 PIC X(6)    VALUE 'SEQ NO'.
           05  HD-TAB10               PIC X.
           05  F                      PIC X(7)    VALUE 'ISS PRM'.
           05  HD-TAB12A              PIC X.
           05  F                      PIC X(7)    VALUE 'CNC PRM'.
           05  HD-TAB12B              PIC X.
           05  FILLER                 PIC X(10)   VALUE 'AGT1'.
           05  HD-TAB11               PIC X.
           05  FILLER                 PIC X(10)   VALUE 'TYP1'.
           05  HD-TAB12               PIC X.
           05  FILLER                 PIC X(11)   VALUE 'ISSC1'.
           05  HD-TAB13               PIC X.
           05  FILLER                 PIC X(11)   VALUE 'CNCC1'.
           05  HD-TAB13A              PIC X.
           05  FILLER                 PIC X(4)    VALUE 'R781'.
           05  HD-TAB13B              PIC X.
           05  FILLER                 PIC X(4)    VALUE 'PRO1'.
           05  HD-TAB13C              PIC X.
           05  FILLER                 PIC XXX     VALUE 'ST1'.
           05  HD-TAB13D              PIC X.
           05  FILLER                 PIC X(10)   VALUE 'AGT2'.
           05  HD-TAB14               PIC X.
           05  FILLER                 PIC X(10)   VALUE 'TYP2'.
           05  HD-TAB15               PIC X.
           05  FILLER                 PIC X(11)   VALUE 'ISSC2'.
           05  HD-TAB16               PIC X.
           05  FILLER                 PIC X(11)   VALUE 'CNCC2'.
           05  HD-TAB16D              PIC X.
           05  FILLER                 PIC X(4)    VALUE 'R782'.
           05  HD-TAB16A              PIC X.
           05  FILLER                 PIC X(4)    VALUE 'PRO2'.
           05  HD-TAB16B              PIC X.
           05  FILLER                 PIC XXX     VALUE 'ST2'.
           05  HD-TAB16C              PIC X.
           05  FILLER                 PIC X(10)   VALUE 'AGT3'.
           05  HD-TAB17               PIC X.
           05  FILLER                 PIC X(10)   VALUE 'TYP3'.
           05  HD-TAB18               PIC X.
           05  FILLER                 PIC X(11)   VALUE 'ISSC3'.
           05  HD-TAB19               PIC X.
           05  FILLER                 PIC X(11)   VALUE 'CNCC3'.
           05  HD-TAB19D              PIC X.
           05  FILLER                 PIC X(4)    VALUE 'R783'.
           05  HD-TAB19A              PIC X.
           05  FILLER                 PIC X(4)    VALUE 'PRO3'.
           05  HD-TAB19B              PIC X.
           05  FILLER                 PIC XXX     VALUE 'ST3'.
           05  HD-TAB19C              PIC X.
           05  FILLER                 PIC X(10)   VALUE 'AGT4'.
           05  HD-TAB20               PIC X.
           05  FILLER                 PIC X(10)   VALUE 'TYP4'.
           05  HD-TAB21               PIC X.
           05  FILLER                 PIC X(11)   VALUE 'ISSC4'.
           05  HD-TAB22               PIC X.
           05  FILLER                 PIC X(11)   VALUE 'CNCC4'.
           05  HD-TAB23               PIC X.
           05  FILLER                 PIC X(10)   VALUE 'AGT5'.
           05  HD-TAB24               PIC X.
           05  FILLER                 PIC X(10)   VALUE 'TYP5'.
           05  HD-TAB25               PIC X.
           05  FILLER                 PIC X(11)   VALUE 'ISSC5'.
           05  HD-TAB26               PIC X.
           05  FILLER                 PIC X(11)   VALUE 'CNCC5'.
           05  HD-TAB27               PIC X.
           05  FILLER                 PIC X(10)   VALUE 'RUN DATE'.
           05  HD-TAB28               PIC X.
           05  FILLER                 PIC XXX     VALUE 'EOR'.

       01  WS-INIT-EXTRACT             PIC X(400).
       01  EXTRACT-RECORD.
           05  EXT-CARRIER             PIC X.
           05  EXT-TAB1                PIC X.
           05  EXT-GROUP               PIC X(6).
           05  EXT-TAB2                PIC X.
           05  EXT-STATE               PIC XX.
           05  EXT-TAB3                PIC X.
           05  EXT-ACCOUNT             PIC X(10).
           05  EXT-TAB4                PIC X.
           05  EXT-EXP-DT              PIC X(10).
           05  EXT-TAB5                PIC X.
           05  EXT-EFF-DT              PIC X(10).
           05  EXT-TAB6                PIC X.
           05  EXT-REIN-COMP           PIC X(6).
           05  EXT-TAB7                PIC X.
           05  EXT-RCD-TYPE            PIC X.
           05  EXT-TAB8                PIC X.
           05  EXT-BEN-CODE            PIC XX.
           05  EXT-TAB9                PIC X.
           05  EXT-SEQ-NO              PIC 99999.
           05  EXT-TAB10               PIC X.
           05  EXT-ISS-PRM             PIC ------9.99.
           05  EXT-TAB12A              PIC X.
           05  EXT-CNC-PRM             PIC ------9.99.
           05  EXT-TAB12B              PIC X.
           05  EXT-AGT1                PIC X(10).
           05  EXT-TAB11               PIC X.
           05  EXT-AGT1-TYP            PIC X.
           05  EXT-TAB12               PIC X.
           05  EXT-COMM1               PIC ------9.99.
           05  EXT-TAB13               PIC X.
           05  EXT-CNC-COMM1           PIC ------9.99.
           05  EXT-TAB13D              PIC X.
           05  EXT-781                 PIC ------9.99.
           05  EXT-TAB13A              PIC X.
           05  EXT-PR1                 PIC ------9.99.
           05  EXT-TAB13B              PIC X.
           05  EXT-ST1                 PIC ------9.99.
           05  EXT-TAB13C              PIC X.
           05  EXT-AGT2                PIC X(10).
           05  EXT-TAB14               PIC X.
           05  EXT-AGT2-TYP            PIC X.
           05  EXT-TAB15               PIC X.
           05  EXT-COMM2               PIC ------9.99.
           05  EXT-TAB16               PIC X.
           05  EXT-CNC-COMM2           PIC ------9.99.
           05  EXT-TAB16D              PIC X.
           05  EXT-782                 PIC ------9.99.
           05  EXT-TAB16A              PIC X.
           05  EXT-PR2                 PIC ------9.99.
           05  EXT-TAB16B              PIC X.
           05  EXT-ST2                 PIC ------9.99.
           05  EXT-TAB16C              PIC X.
           05  EXT-AGT3                PIC X(10).
           05  EXT-TAB17               PIC X.
           05  EXT-AGT3-TYP            PIC X.
           05  EXT-TAB18               PIC X.
           05  EXT-COMM3               PIC ------9.99.
           05  EXT-TAB19               PIC X.
           05  EXT-CNC-COMM3           PIC ------9.99.
           05  EXT-TAB19D              PIC X.
           05  EXT-783                 PIC ------9.99.
           05  EXT-TAB19A              PIC X.
           05  EXT-PR3                 PIC ------9.99.
           05  EXT-TAB19B              PIC X.
           05  EXT-ST3                 PIC ------9.99.
           05  EXT-TAB19C              PIC X.
           05  EXT-AGT4                PIC X(10).
           05  EXT-TAB20               PIC X.
           05  EXT-AGT4-TYP            PIC X.
           05  EXT-TAB21               PIC X.
           05  EXT-COMM4               PIC ------9.99.
           05  EXT-TAB22               PIC X.
           05  EXT-CNC-COMM4           PIC ------9.99.
           05  EXT-TAB23               PIC X.
           05  EXT-AGT5                PIC X(10).
           05  EXT-TAB24               PIC X.
           05  EXT-AGT5-TYP            PIC X.
           05  EXT-TAB25               PIC X.
           05  EXT-COMM5               PIC ------9.99.
           05  EXT-TAB26               PIC X.
           05  EXT-CNC-COMM5           PIC ------9.99.
           05  EXT-TAB27               PIC X.
           05  EXT-RUN-DATE            PIC X(10).
           05  EXT-TAB28               PIC X.
           05  EXT-EOR                 PIC X.

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
               10  WS-WORK-CCYY-N REDEFINES WS-WORK-CCYY
                                       PIC 9(4).
               10  WS-WORK-MM          PIC XX.
               10  WS-WORK-DD          PIC XX.

       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

           EJECT
                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.
       0002-INPUT.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0060-READ-EPEC      THRU 0060-EXIT

           PERFORM 0070-PROCESS-INPUT  THRU 0070-EXIT UNTIL
                 (END-OF-EPEC)
      *          OR (EPEC-IN-CNT > 10000)

           IF STARTED-EXTR
              PERFORM 0100-WRITE-EXTRACT THRU 0100-EXIT
           END-IF

      *    PERFORM 0090-BUILD-EXTRACT  THRU 0090-EXIT
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
                                          EXT-TAB12A
                                          EXT-TAB12B
                                          EXT-TAB13
                                          EXT-TAB13A
                                          EXT-TAB13B
                                          EXT-TAB13C
                                          EXT-TAB13D
                                          EXT-TAB14
                                          EXT-TAB15
                                          EXT-TAB16
                                          EXT-TAB16A
                                          EXT-TAB16B
                                          EXT-TAB16C
                                          EXT-TAB16D
                                          EXT-TAB17
                                          EXT-TAB18
                                          EXT-TAB19
                                          EXT-TAB19A
                                          EXT-TAB19B
                                          EXT-TAB19C
                                          EXT-TAB19D
                                          EXT-TAB20
                                          EXT-TAB21
                                          EXT-TAB22
                                          EXT-TAB23
                                          EXT-TAB24
                                          EXT-TAB25
                                          EXT-TAB26
                                          EXT-TAB27
                                          EXT-TAB28

           MOVE 'E'                    TO EXT-EOR
           MOVE EXTRACT-RECORD         TO WS-INIT-EXTRACT

           MOVE ';'                    TO HD-TAB1
                                          HD-TAB2
                                          HD-TAB3
                                          HD-TAB4
                                          HD-TAB5
                                          HD-TAB6
                                          HD-TAB7
                                          HD-TAB8
                                          HD-TAB9
                                          HD-TAB10
                                          HD-TAB11
                                          HD-TAB12
                                          HD-TAB12A
                                          HD-TAB12B
                                          HD-TAB13
                                          HD-TAB13A
                                          HD-TAB13B
                                          HD-TAB13C
                                          HD-TAB13D
                                          HD-TAB14
                                          HD-TAB15
                                          HD-TAB16
                                          HD-TAB16A
                                          HD-TAB16B
                                          HD-TAB16C
                                          HD-TAB16D
                                          HD-TAB17
                                          HD-TAB18
                                          HD-TAB19
                                          HD-TAB19A
                                          HD-TAB19B
                                          HD-TAB19C
                                          HD-TAB19D
                                          HD-TAB20
                                          HD-TAB21
                                          HD-TAB22
                                          HD-TAB23
                                          HD-TAB24
                                          HD-TAB25
                                          HD-TAB26
                                          HD-TAB27
                                          HD-TAB28
                                          
           WRITE EXTRACT-HEAD-OUT      FROM HEAD-RECORD
           
           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE -1                     TO DC-ELAPSED-MONTHS
           MOVE +0                     TO DC-ELAPSED-DAYS
           MOVE '6'                    TO DC-OPTION-CODE
           MOVE '1'                    TO DC-END-OF-MONTH
           PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-CYMD   TO WS-LAST-MONTH-DT
           ELSE
              DISPLAY ' LAST MONTH DATE ERROR '
              PERFORM ABEND-PGM
           END-IF

           MOVE RUN-DATE               TO WS-WORK-DATE
           MOVE 12                     TO WS-WORK-MM
           MOVE 31                     TO WS-WORK-DD
           SUBTRACT 1                  FROM WS-WORK-CCYY-N
           MOVE WS-WORK-DATE           TO WS-LAST-YEAR-END-DT

           DISPLAY ' LAST MONTH ' WS-LAST-MONTH-DT
           DISPLAY ' LAST YEAR  ' WS-LAST-YEAR-END-DT

           MOVE +1                     TO S1

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
           CLOSE EPECS EXTRACT

           .
       0030-EXIT.
           EXIT.

       0060-READ-EPEC.

           READ EPECS AT END
               SET END-OF-EPEC         TO TRUE
           END-READ

           IF NOT END-OF-EPEC
              ADD 1                    TO EPEC-IN-CNT
              MOVE EP-CONTROL          TO WS-CONTROL
              MOVE EP-REI-CO           TO WS-REI-CO
           END-IF

           .
       0060-EXIT.
           EXIT.

       0070-PROCESS-INPUT.

           IF (EC-RECORD-ID = 'EC' OR 'EP')
              AND (EC-REIN NOT = 'R')
              AND (EC-ACCOUNT = '0990000190' OR '0900733030')
              AND (EC-RUN-DTE = 20100831)
              PERFORM 0080-BUILD-EXTRACT
                                       THRU 0080-EXIT
           END-IF

           PERFORM 0060-READ-EPEC      THRU 0060-EXIT

           .
       0070-EXIT.
           EXIT.

       0080-BUILD-EXTRACT.

           IF EC-RECORD-ID = 'EC'
              PERFORM 0090-BUILD-EC    THRU 0090-EXIT
           ELSE
              PERFORM 0095-BUILD-EP    THRU 0095-EXIT
           END-IF

           .
       0080-EXIT.
           EXIT.

       0090-BUILD-EC.

           IF STARTED-EXTR
              PERFORM 0100-WRITE-EXTRACT THRU 0100-EXIT
           END-IF

           MOVE 'Y'                    TO WS-START-SW

           MOVE EC-CARRIER             TO EXT-CARRIER
           MOVE EC-GROUPING            TO EXT-GROUP
           MOVE EC-STATE               TO EXT-STATE
           MOVE EC-ACCOUNT             TO EXT-ACCOUNT
           MOVE EC-EFF-DTE             TO WS-WORK-DATE
           STRING WS-WORK-MM '/' WS-WORK-DD '/' WS-WORK-CCYY
              DELIMITED BY SIZE INTO EXT-EFF-DT
           END-STRING
           MOVE EC-EXP-DTE             TO WS-WORK-DATE
           STRING WS-WORK-MM '/' WS-WORK-DD '/' WS-WORK-CCYY
              DELIMITED BY SIZE INTO EXT-EXP-DT
           END-STRING
           MOVE EC-REI-CO              TO EXT-REIN-COMP
           MOVE EC-RCD-TYPE            TO EXT-RCD-TYPE
           MOVE EC-BEN-CODE            TO EXT-BEN-CODE
           MOVE EC-SEQ-NBR             TO EXT-SEQ-NO
           MOVE EC-AGT-NO (1)          TO EXT-AGT1
           MOVE EC-AGT-TYPE (1)        TO EXT-AGT1-TYP
           MOVE EC-ISS-COMM (1)        TO EXT-COMM1
           MOVE EC-CNC-COMM (1)        TO EXT-CNC-COMM1
           MOVE EC-COMM-78 (1)         TO EXT-781
           MOVE EC-COMM-PR (1)         TO EXT-PR1
           MOVE EC-COMM-ST (1)         TO EXT-ST1
           MOVE EC-AGT-NO (2)          TO EXT-AGT2
           MOVE EC-AGT-TYPE (2)        TO EXT-AGT2-TYP
           MOVE EC-ISS-COMM (2)        TO EXT-COMM2
           MOVE EC-CNC-COMM (2)        TO EXT-CNC-COMM2
           MOVE EC-COMM-78 (2)         TO EXT-782
           MOVE EC-COMM-PR (2)         TO EXT-PR2
           MOVE EC-COMM-ST (2)         TO EXT-ST2
           MOVE EC-AGT-NO (3)          TO EXT-AGT3
           MOVE EC-AGT-TYPE (3)        TO EXT-AGT3-TYP
           MOVE EC-ISS-COMM (3)        TO EXT-COMM3
           MOVE EC-CNC-COMM (3)        TO EXT-CNC-COMM3
           MOVE EC-COMM-78 (3)         TO EXT-783
           MOVE EC-COMM-PR (3)         TO EXT-PR3
           MOVE EC-COMM-ST (3)         TO EXT-ST3
           MOVE EC-AGT-NO (4)          TO EXT-AGT4
           MOVE EC-AGT-TYPE (4)        TO EXT-AGT4-TYP
           MOVE EC-ISS-COMM (4)        TO EXT-COMM4
           MOVE EC-CNC-COMM (4)        TO EXT-CNC-COMM4
           IF EC-ISS-COMM (5) NOT NUMERIC
              MOVE ZEROS               TO EC-ISS-COMM (5)
           END-IF
           IF EC-CNC-COMM (5) NOT NUMERIC
              MOVE ZEROS               TO EC-CNC-COMM (5)
           END-IF
           MOVE EC-AGT-NO (5)          TO EXT-AGT5
           MOVE EC-AGT-TYPE (5)        TO EXT-AGT5-TYP
           MOVE EC-ISS-COMM (5)        TO EXT-COMM5
           MOVE EC-CNC-COMM (5)        TO EXT-CNC-COMM5
           MOVE EC-RUN-DTE             TO WS-WORK-DATE
           STRING WS-WORK-MM '/' WS-WORK-DD '/' WS-WORK-CCYY
              DELIMITED BY SIZE INTO EXT-RUN-DATE
           END-STRING
           MOVE 'E'                    TO EXT-EOR

      *    PERFORM 0100-WRITE-EXTRACT  THRU 0100-EXIT

           .
       0090-EXIT.
           EXIT.

       0095-BUILD-EP.

           MOVE EP-ISS-PRM             TO EXT-ISS-PRM
           MOVE EP-CNC-PRM             TO EXT-CNC-PRM

           .
       0095-EXIT.
           EXIT.

       0100-WRITE-EXTRACT.

           WRITE EXTRACT-RECORD-OUT    FROM EXTRACT-RECORD
           ADD 1                       TO EXTR-OUT-CNT

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
