       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDCRX1.
       AUTHOR.        PABLO.
       DATE-COMPILED.

      *REMARKS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CERTS        ASSIGN TO SYS010.
           SELECT EXTRACT      ASSIGN TO SYS011
               ORGANIZATION IS LINE SEQUENTIAL.
                                                                        
              

       DATA DIVISION.
       FILE SECTION.

       FD  CERTS
                                       COPY ECSCRIFD.
                                       COPY ECSCRT01.

           EJECT
       FD  EXTRACT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  EXTRACT-RECORD-out          PIC x(400).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CIDCRX1 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-CERT                VALUE 'Y'.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  CERT-IN-CNT                 PIC 9(11)  VALUE ZEROS.
       77  EXTR-OUT-CNT                PIC 9(11)  VALUE ZEROS.

       01  WS-INIT-EXTRACT             PIC X(405).
       01  EXTRACT-RECORD.
           05  EXT-CARRIER             PIC X.
           05  EXT-TAB                 PIC X.
           05  EXT-STATE               PIC XX.
           05  EXT-TAB0                PIC X.
           05  EXT-ACCOUNT             PIC X(10).
           05  EXT-TAB1                PIC X.
           05  EXT-EFF-DT              PIC X(10).
           05  EXT-TAB1A               PIC X.
           05  EXT-CERT-NO             PIC X(11).
           05  EXT-TAB1B               PIC X.
           05  EXT-ENT-DT              PIC X(10).
           05  EXT-TAB2                PIC X.
           05  EXT-1ST-PAY-DT          PIC X(10).
           05  EXT-TAB2A               PIC X.
           05  EXT-ENTRY-STATUS        PIC X.
           05  EXT-TAB2B               PIC X.
           05  EXT-LF-BEN              PIC XX.
           05  EXT-TAB2C               PIC X.
           05  EXT-LF-TERM             PIC 999.
           05  EXT-TAB2D               PIC X.
           05  EXT-LF-AMT              PIC ZZZ,ZZ9.99.
           05  EXT-TAB2E               PIC X.
           05  EXT-LF-PREM             PIC ZZZ,ZZ9.99.
           05  EXT-TAB3                PIC X.
           05  EXT-LF-REFUND           PIC ZZZ,ZZ9.99.
           05  EXT-TAB4                PIC X.
           05  EXT-LF-EXP-DT           PIC X(10).
           05  EXT-TAB4A               PIC X.
           05  EXT-LF-CAN-DT           PIC X(10).
           05  EXT-TAB4B               PIC X.
           05  EXT-LF-DTH-DT           PIC X(10).
           05  EXT-TAB4C               PIC X.
           05  EXT-LF-CUR-STATUS       PIC X.
           05  EXT-TAB4D               PIC X.
           05  EXT-AH-BEN              PIC XX.
           05  EXT-TAB4E               PIC X.
           05  EXT-AH-TERM             PIC 999.
           05  EXT-TAB4F               PIC X.
           05  EXT-AH-AMT              PIC ZZZ,ZZ9.99.
           05  EXT-TAB4G               PIC X.
           05  EXT-AH-PREM             PIC ZZZ,ZZ9.99.
           05  EXT-TAB5                PIC X.
           05  EXT-AH-REFUND           PIC ZZZ,ZZ9.99.
           05  EXT-TAB6                PIC X.
           05  EXT-AH-EXP-DT           PIC X(10).
           05  EXT-TAB6A               PIC X.
           05  EXT-AH-CAN-DT           PIC X(10).
           05  EXT-TAB6B               PIC X.
           05  EXT-AH-DIS-DT           PIC X(10).
           05  EXT-TAB6C               PIC X.
           05  EXT-AH-CUR-STATUS       PIC X.
           05  EXT-TAB6D               PIC X.
           05  EXT-AGT1                PIC X(10).
           05  EXT-TAB7                PIC X.
           05  EXT-LF-COM1             PIC .99999.
           05  EXT-TAB8                PIC X.
           05  EXT-AH-COM1             PIC .99999.
           05  EXT-TAB9                PIC X.
           05  EXT-AGT2                PIC X(10).
           05  EXT-TAB10               PIC X.
           05  EXT-LF-COM2             PIC .99999.
           05  EXT-TAB11               PIC X.
           05  EXT-AH-COM2             PIC .99999.
           05  EXT-TAB12               PIC X.
           05  EXT-AGT3                PIC X(10).
           05  EXT-TAB13               PIC X.
           05  EXT-LF-COM3             PIC .99999.
           05  EXT-TAB14               PIC X.
           05  EXT-AH-COM3             PIC .99999.
           05  EXT-TAB15               PIC X.
           05  EXT-AGT4                PIC X(10).
           05  EXT-TAB16               PIC X.
           05  EXT-LF-COM4             PIC .99999.
           05  EXT-TAB17               PIC X.
           05  EXT-AH-COM4             PIC .99999.
           05  EXT-TAB18               PIC X.
           05  EXT-AGT5                PIC X(10).
           05  EXT-TAB19               PIC X.
           05  EXT-LF-COM5             PIC .99999.
           05  EXT-TAB20               PIC X.
           05  EXT-AH-COM5             PIC .99999.
           05  EXT-TAB21               PIC X.
           05  EXT-END                 PIC X.


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

       01  DATE-AREAS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
           05  FILLER REDEFINES WS-DATE.
               10  FILLER              PIC 999.
               10  WS-CCYY             PIC 9999.
               10  WS-MM               PIC 99.
               10  WS-DD               PIC 99.
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

           EJECT
                                       COPY ELCDATE.
      /
                                       COPY ELCDTECX.
      /
       PROCEDURE DIVISION.

       0002-INPUT.

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0060-READ-CERT      THRU 0060-EXIT

           PERFORM 0080-PROCESS-CERT   THRU 0080-EXIT UNTIL
                 (END-OF-CERT)
      *          OR (CERT-IN-CNT > 10000)

           GOBACK
           .
       0002-EXIT.
           EXIT.

       0010-INITIALIZE.

           MOVE SPACES                 TO EXTRACT-RECORD
           MOVE X'09'                  TO EXT-TAB1
                                          EXT-TAB
                                          EXT-TAB0
                                          EXT-TAB1A
                                          EXT-TAB1B
                                          EXT-TAB2
                                          EXT-TAB2A
                                          EXT-TAB2B
                                          EXT-TAB2C
                                          EXT-TAB2D
                                          EXT-TAB2E
                                          EXT-TAB3
                                          EXT-TAB4
                                          EXT-TAB4A
                                          EXT-TAB4B
                                          EXT-TAB4C
                                          EXT-TAB4D
                                          EXT-TAB4E
                                          EXT-TAB4F
                                          EXT-TAB4G
                                          EXT-TAB5
                                          EXT-TAB6
                                          EXT-TAB6A
                                          EXT-TAB6B
                                          EXT-TAB6C
                                          EXT-TAB6D
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
           MOVE '*'                    TO EXT-END
           MOVE EXTRACT-RECORD         TO WS-INIT-EXTRACT
           .

       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT CERTS
               OUTPUT EXTRACT
               
           .

       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY ' CERT IN RECORDS  ' CERT-IN-CNT
           DISPLAY ' EXTR OUT RECORDS ' EXTR-OUT-CNT
           CLOSE CERTS
               EXTRACT

           .

       0030-EXIT.
           EXIT.

       0060-READ-CERT.

           READ CERTS AT END
               SET END-OF-CERT TO TRUE
           END-READ

           IF NOT END-OF-CERT
              ADD 1 TO CERT-IN-CNT
           END-IF

           .

       0060-EXIT.
           EXIT.


       0080-PROCESS-CERT.

           PERFORM 0090-BUILD-EXTRACT  THRU 0090-EXIT

           PERFORM 0060-READ-CERT      THRU 0060-EXIT

           .
       0080-EXIT.
           EXIT.

       0090-BUILD-EXTRACT.

           MOVE WS-INIT-EXTRACT        TO EXTRACT-RECORD
           MOVE CR-CARRIER             TO EXT-CARRIER
           MOVE CR-STATE               TO EXT-STATE
           MOVE CR-ACCOUNT             TO EXT-ACCOUNT
           MOVE CR-CERT-NO             TO EXT-CERT-NO


           MOVE CR-DT                  TO WS-DATE
           MOVE ' '                    TO DC-OPTION-CODE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EXT-EFF-DT
           END-STRING
           STRING CR-1ST-PMT-MO '/' CR-1ST-PMT-DA '/'
              CR-1ST-PMT-YR DELIMITED BY SIZE
               INTO EXT-1ST-PAY-DT
           END-STRING

           MOVE CR-ENTRY-DATE          TO WS-DATE
           MOVE ' '                    TO DC-OPTION-CODE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EXT-ENT-DT
           END-STRING
           MOVE CR-ENTRY-STATUS        TO EXT-ENTRY-STATUS
           MOVE CR-LFTYP               TO EXT-LF-BEN
           MOVE CR-LFAMT               TO EXT-LF-AMT
           MOVE CR-LF-TERM             TO EXT-LF-TERM           
           MOVE CR-LFPRM               TO EXT-LF-PREM
           MOVE CR-LFRFND              TO EXT-LF-REFUND
           MOVE CR-LF-EXPIRE-DATE      TO WS-DATE
           MOVE ' '                    TO DC-OPTION-CODE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EXT-LF-EXP-DT
           END-STRING
           MOVE CR-LF-CANC-DT          TO WS-DATE
           MOVE ' '                    TO DC-OPTION-CODE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EXT-LF-CAN-DT
           END-STRING
           MOVE CR-DTH-DT              TO WS-DATE
           MOVE ' '                    TO DC-OPTION-CODE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EXT-LF-DTH-DT
           END-STRING
           MOVE CR-LF-CURRENT-STATUS   TO EXT-LF-CUR-STATUS
           MOVE CR-AHTYP               TO EXT-AH-BEN
           MOVE CR-AHAMT               TO EXT-AH-AMT
           MOVE CR-AH-TERM             TO EXT-AH-TERM
           MOVE CR-AHPRM               TO EXT-AH-PREM
           MOVE CR-AHRFND              TO EXT-AH-REFUND
           MOVE CR-AH-EXPIRE-DATE      TO WS-DATE
           MOVE ' '                    TO DC-OPTION-CODE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EXT-AH-EXP-DT
           END-STRING
           MOVE CR-AH-CANC-DT          TO WS-DATE
           MOVE ' '                    TO DC-OPTION-CODE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EXT-AH-CAN-DT
           END-STRING
           MOVE CR-DIS-DT              TO WS-DATE
           MOVE ' '                    TO DC-OPTION-CODE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EXT-AH-DIS-DT
           END-STRING
           MOVE CR-AH-CURRENT-STATUS   TO EXT-AH-CUR-STATUS
           MOVE CR-COM-AGT (1)         TO EXT-AGT1
           MOVE CR-LCOM-L (1)          TO EXT-LF-COM1
           MOVE CR-LCOM-AH (1)         TO EXT-AH-COM1
           MOVE CR-COM-AGT (2)         TO EXT-AGT2
           MOVE CR-LCOM-L (2)          TO EXT-LF-COM2
           MOVE CR-LCOM-AH (2)         TO EXT-AH-COM2
           MOVE CR-COM-AGT (3)         TO EXT-AGT3
           MOVE CR-LCOM-L (3)          TO EXT-LF-COM3
           MOVE CR-LCOM-AH (3)         TO EXT-AH-COM3
           MOVE CR-COM-AGT (4)         TO EXT-AGT4
           MOVE CR-LCOM-L (4)          TO EXT-LF-COM4
           MOVE CR-LCOM-AH (4)         TO EXT-AH-COM4
           MOVE CR-COM-AGT (5)         TO EXT-AGT5
           MOVE CR-LCOM-L (5)          TO EXT-LF-COM5
           MOVE CR-LCOM-AH (5)         TO EXT-AH-COM5
                  
      *    MOVE CR-DT                  TO WS-WORK-DATE
      *    STRING WS-WORK-MM '/' WS-WORK-DD '/' WS-WORK-CCYY
      *       DELIMITED BY SIZE INTO EXT-EFF-DT
      *    END-STRING
           MOVE '*'                    TO EXT-END
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

           EJECT
