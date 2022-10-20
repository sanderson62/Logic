       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDCOX2.
       AUTHOR.        PABLO.
       DATE-COMPILED.
      *REMARKS.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
060506* 060506    2002061100007  PEMA  ADD CODES TO ERCOMP FILE
100307* 100307    2007080700001  PEMA  ADD CSR PROCESSING
020310* 020310  CR2008100900004  PEMA  ADD REF4 EXTRACT PROCESSING
011719* 011719  CR2019011100002  PEMA  COMBINE EXT3 WITH EXT2
052419* 052419  CR2019050600002  PEMA  REMOVE PREV chgs
062519  062519  CR2019061100002  PEMA  Add field to extract for sort
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERCOMP       ASSIGN TO ERCOMP
                               ACCESS IS SEQUENTIAL
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERCOMP-FILE-STATUS
                               RECORD KEY IS CO-CONTROL-PRIMARY.

           SELECT CID-BYPASS-FILE ASSIGN TO SYS010
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE    ASSIGN TO SYS019.

           SELECT COMP-OUT1    ASSIGN TO COMPOT1
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT COMP-OUT2    ASSIGN TO COMPOT2.
      *        ORGANIZATION IS LINE SEQUENTIAL.

052419     SELECT COMP-OUT3    ASSIGN TO COMPOT3.
052419*        ORGANIZATION IS LINE SEQUENTIAL.

           SELECT COMP-OUT4    ASSIGN TO COMPOT4
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  CID-BYPASS-FILE
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.                                            

       01  CID-BYPASS-RECORD.
           05  SAV-CID-CC              PIC 99.
           05  SAV-CID-YY              PIC 99.
           05  SAV-CID-MM              PIC 99.
           05  CID-SCAN-SEQ-NO         PIC 9(7).
           05  SR-DEL1                 PIC X.
           05  CID-STMT-TYPE           PIC X.
           05  SR-DEL2                 PIC X.
           05  CID-CARRIER             PIC X.
           05  SR-DEL3                 PIC X.
           05  CID-GROUP               PIC X(6).
           05  SR-DEL4                 PIC X.
           05  CID-FIN-RESP            PIC X(10).
           05  SR-DEL5                 PIC X.
           05  CID-ACCOUNT             PIC X(10).
           05  SR-DEL6                 PIC X.
           05  CID-AMT-DUE             PIC 9(7).99.

       FD  COMP-OUT1
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

062519 01  COMP-OUT-REC1               PIC X(263).

       FD  COMP-OUT2
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

062519 01  COMP-OUT-REC2               PIC X(268).

052419 FD  COMP-OUT3
052419     RECORDING MODE F
052419     BLOCK CONTAINS 0 RECORDS.
052419
062519 01  COMP-OUT-REC3               PIC X(281).

       FD  COMP-OUT4
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

062519 01  COMP-OUT-REC4               PIC X(263).

       FD  ERCOMP.

                                       COPY ERCCOMP.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CIDCOX2 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  WS-BYPASS-SW                PIC X  VALUE SPACES.
           88  BYPASS-ERCOMP-REC         VALUE 'Y'.
       77  WS-TABLE-EOF-SW             PIC X  VALUE SPACES.
           88  END-OF-TABLE              VALUE 'Y'.
       77  WS-END-BAL                  PIC S9(7)V99 COMP-3 VALUE +0.
       01  WS-MISC.
           05  WS-EOF-SW               PIC X      VALUE SPACES.
               88  END-OF-ERCOMP                  VALUE 'Y'.
           05  ERCOMP-FILE-STATUS      PIC XX     VALUE '00'.
           05  SUB1            COMP-3  PIC S9(3)  VALUE +0.
           05  WS-COMPANY-CD           PIC X(01)  VALUE SPACE.

           05  WS-WORK-CITY-ST.
               10  WS-BYTE OCCURS 29   PIC X.
           05  WS-SAVE-ERCOMP          PIC X(270) VALUE LOW-VALUES.
           05  WS-ERCOMP-IN            PIC 9(7)   VALUE ZEROS.
           05  WS-ERCOMP-OUT1          PIC 9(7)   VALUE ZEROS.
           05  WS-ERCOMP-OUT2          PIC 9(7)   VALUE ZEROS.
052419     05  WS-ERCOMP-OUT3          PIC 9(7)   VALUE ZEROS.
           05  WS-ERCOMP-OUT4          PIC 9(7)   VALUE ZEROS.


062519 01  ERCOMP-DETAIL-RECORD-ext3.
062519     12  ex3-CSR-CODE            PIC X(4).
062519     12  ex3-TAB0                PIC X.
062519     12  ex3-ACCT-NAME           PIC X(30).
062519     12  ex3-TAB1                PIC X.
062519     12  ex3-MAIL-NAME           PIC X(30).
062519     12  ex3-TAB2                PIC X.
062519     12  ex3-ADDR-1              PIC X(30).
062519     12  ex3-TAB3                PIC X.
062519     12  ex3-ADDR-2              PIC X(30).
062519     12  ex3-TAB4                PIC X.
062519     12  ex3-ADDR-3              PIC X(29).
062519     12  ex3-TAB5                PIC X.
062519     12  ex3-ZIP                 PIC 9(9).
062519     12  ex3-TAB6                PIC X.
062519     12  ex3-INV-DATE            PIC X(10).
062519     12  ex3-TAB7                PIC X.
062519     12  ex3-CHECK-AMT           PIC Z,ZZZ,ZZZ.99.
062519     12  ex3-TAB8                PIC X.
062519     12  ex3-GL-NUM              PIC X(10).
062519     12  ex3-TAB9                PIC X.
062519     12  ex3-DIV                 PIC XXX.
062519     12  ex3-TAB10               PIC X.
062519     12  ex3-CENTER              PIC X(5).
062519     12  ex3-TAB11               PIC X.
062519     12  ex3-LOB                 PIC X(7).
062519     12  ex3-TAB12               PIC X.
062519     12  ex3-STATE               PIC XX.
062519     12  ex3-TAB13               PIC X.
062519     12  ex3-CARRIER             PIC X.
062519     12  ex3-TAB14               PIC X.
062519     12  ex3-GROUPING            PIC X(6).
062519     12  ex3-TAB15               PIC X.
062519     12  ex3-RESP-NO             PIC X(10).
062519     12  ex3-TAB16               PIC X.
062519     12  ex3-ACCOUNT             PIC X(10).
062519     12  ex3-TAB17               PIC X.
062519     12  ex3-SPEC-INST           PIC X(10).
062519     12  ex3-TAB18               PIC X.
062519     12  ex3-report-group-id     pic x(12).
062519     12  ex3-tab19               pic x.
062519     12  ex3-EOR                 PIC X.

       01  ERCOMP-DETAIL-RECORD.
           12  EX-CSR-CODE             PIC X(4).
           12  EX-TAB0                 PIC X.
           12  EX-ACCT-NAME            PIC X(30).
           12  EX-TAB1                 PIC X.
           12  EX-MAIL-NAME            PIC X(30).
           12  EX-TAB2                 PIC X.
           12  EX-ADDR-1               PIC X(30).
           12  EX-TAB3                 PIC X.
           12  EX-ADDR-2               PIC X(30).
           12  EX-TAB4                 PIC X.
           12  EX-ADDR-3               PIC X(29).
           12  EX-TAB5                 PIC X.
           12  EX-ZIP                  PIC 9(9).
           12  EX-TAB6                 PIC X.
           12  EX-INV-DATE             PIC X(10).
           12  EX-TAB7                 PIC X.
           12  EX-CHECK-AMT            PIC Z,ZZZ,ZZZ.99.
           12  EX-TAB8                 PIC X.
           12  EX-GL-NUM               PIC X(10).
           12  EX-TAB9                 PIC X.
           12  EX-DIV                  PIC XXX.
           12  EX-TAB10                PIC X.
           12  EX-CENTER               PIC X(5).
           12  EX-TAB11                PIC X.
           12  EX-LOB                  PIC X(7).
           12  EX-TAB12                PIC X.
           12  EX-STATE                PIC XX.
           12  EX-TAB13                PIC X.
           12  EX-CARRIER              PIC X.
           12  EX-TAB14                PIC X.
           12  EX-GROUPING             PIC X(6).
           12  EX-TAB15                PIC X.
           12  EX-RESP-NO              PIC X(10).
           12  EX-TAB16                PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-TAB17                PIC X.
           12  EX-SPEC-INST            PIC X(10).
           12  EX-TAB18                PIC X.
           12  EX-EOR                  PIC X.

       01  B1                          PIC S999 COMP-3 VALUE +0.
       01  BM1                         PIC S999 COMP-3 VALUE +0.
       01  BYPASS-TABLE.
           05  BYPASS-PROCESS-TABLE OCCURS 400.
               10  WS-BP-CARRIER       PIC X.
               10  WS-BP-GROUP         PIC X(6).
               10  WS-BP-FIN-RESP      PIC X(10).
               10  WS-BP-ACCOUNT       PIC X(10).

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

                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

       0000-BEGIN.

                                       COPY ELCDTERX.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT
           PERFORM 0040-INIT           THRU 0040-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
                (END-OF-ERCOMP)
PEMTST*         OR (WS-ERCOMP-IN > 1000)

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY ' RECORDS IN     ' WS-ERCOMP-IN
           DISPLAY ' RECORDS  OUT 1 ' WS-ERCOMP-OUT1
           DISPLAY ' RECORDS  OUT 2 ' WS-ERCOMP-OUT2
052419     DISPLAY ' RECORDS  OUT 3 ' WS-ERCOMP-OUT3
           DISPLAY ' RECORDS  OUT 4 ' WS-ERCOMP-OUT4
           GOBACK
           .
       0002-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT ERCOMP
                      CID-BYPASS-FILE
               OUTPUT COMP-OUT1
               OUTPUT COMP-OUT2
052419         OUTPUT COMP-OUT3
               OUTPUT COMP-OUT4

           IF ERCOMP-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERCOMP OPEN ERROR ' ERCOMP-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE ERCOMP
                 COMP-OUT1
                 COMP-OUT2
052419           COMP-OUT3
                 COMP-OUT4
                 CID-BYPASS-FILE

           IF ERCOMP-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERCOMP CLOSE ERROR ' ERCOMP-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0030-EXIT.
           EXIT.

       0040-INIT.

           MOVE SPACES                 TO BYPASS-TABLE
           MOVE SPACES                 TO ERCOMP-DETAIL-RECORD
           MOVE ';'                    TO EX-TAB0
                                          EX-TAB1
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
           MOVE '*'                    TO EX-EOR
062519     MOVE ';'                    TO ex3-TAB0
062519                                    ex3-TAB1
062519                                    ex3-TAB2
062519                                    ex3-TAB3
062519                                    ex3-TAB4
062519                                    ex3-TAB5
062519                                    ex3-TAB6
062519                                    ex3-TAB7
062519                                    ex3-TAB8
062519                                    ex3-TAB9
062519                                    ex3-TAB10
062519                                    ex3-TAB11
062519                                    ex3-TAB12
062519                                    ex3-TAB13
062519                                    ex3-TAB14
062519                                    ex3-TAB15
062519                                    ex3-TAB16
062519                                    ex3-TAB17
062519                                    ex3-TAB18
062519                                    ex3-tab19
062519     MOVE '*'                    TO ex3-EOR

           MOVE ERCOMP-DETAIL-RECORD   TO WS-SAVE-ERCOMP

           PERFORM 0045-LOAD-BYPASS-TABLE
                                       THRU 0045-EXIT
           PERFORM 0120-START-ERCOMP   THRU 0120-EXIT
           PERFORM 0110-READ-ERCOMP    THRU 0110-EXIT

           .
       0040-EXIT.
           EXIT.

       0045-LOAD-BYPASS-TABLE.

           MOVE ' '                    TO WS-TABLE-EOF-SW

           PERFORM VARYING B1 FROM +1 BY +1 UNTIL
              END-OF-TABLE
              READ CID-BYPASS-FILE AT END
                 SET END-OF-TABLE      TO TRUE
              END-READ
              IF NOT END-OF-TABLE
                 MOVE CID-CARRIER      TO WS-BP-CARRIER  (B1)
                 MOVE CID-GROUP        TO WS-BP-GROUP    (B1)
                 MOVE CID-FIN-RESP     TO WS-BP-FIN-RESP (B1)
                 MOVE CID-ACCOUNT      TO WS-BP-ACCOUNT  (B1)
              END-IF
           END-PERFORM

           SUBTRACT +2                 FROM B1
           MOVE B1                     TO BM1
           DISPLAY ' NUMBER OF BYPASS RECORDS ' BM1

           .
       0045-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           MOVE SPACES                 TO WS-BYPASS-SW

           IF CO-TYPE = 'A'
              PERFORM 0060-CHECK-BYPASS
                                       THRU 0060-EXIT
           END-IF

           IF BYPASS-ERCOMP-REC
              DISPLAY ' BYPASSING RECORD ' CO-CONTROL-PRIMARY (2:28)
              GO TO 0050-CONTINUE
           END-IF

           MOVE WS-SAVE-ERCOMP         TO ERCOMP-DETAIL-RECORD

           MOVE CO-CARRIER             TO EX-CARRIER
           MOVE CO-GROUPING            TO EX-GROUPING
           MOVE CO-RESP-NO             TO EX-RESP-NO
           IF CO-ACCOUNT = LOW-VALUES
              MOVE SPACES              TO EX-ACCOUNT
           ELSE
              MOVE CO-ACCOUNT          TO EX-ACCOUNT
           END-IF

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-INV-DATE
           END-IF

           MOVE CO-END-BAL             TO EX-CHECK-AMT
           MOVE '1825011300'           TO EX-GL-NUM
           MOVE '02'                   TO EX-DIV
           MOVE ZEROS                  TO EX-CENTER
           MOVE ZEROS                  TO EX-LOB
           MOVE ZEROS                  TO EX-STATE
           MOVE 'CC: CID'              TO EX-SPEC-INST
           MOVE CO-CSR-CODE            TO EX-CSR-CODE
           MOVE CO-ACCT-NAME           TO EX-ACCT-NAME
           IF CO-MAIL-NAME = LOW-VALUES
              MOVE SPACES              TO EX-MAIL-NAME
           ELSE
              MOVE CO-MAIL-NAME        TO EX-MAIL-NAME
           END-IF
           INSPECT EX-ACCT-NAME REPLACING ALL '*' BY ' '
           INSPECT EX-MAIL-NAME REPLACING ALL '*' BY ' '
           MOVE CO-ADDR-1              TO EX-ADDR-1
           MOVE CO-ADDR-2              TO EX-ADDR-2
           MOVE CO-ADDR-3              TO WS-WORK-CITY-ST
           PERFORM VARYING SUB1 FROM +29 BY -1 UNTIL
              (SUB1 < +1)
              OR (WS-BYTE (SUB1) NOT = ' ' AND '.' AND ',')
           END-PERFORM
           IF SUB1 > +2
              MOVE WS-WORK-CITY-ST (SUB1 - 1:2)
                                       TO EX-STATE
              MOVE WS-WORK-CITY-ST (1:SUB1 - 2)
                                       TO EX-ADDR-3
           END-IF
           INSPECT EX-ADDR-3 REPLACING ALL ',' BY ' '
           MOVE CO-ZIP                 TO EX-ZIP

           PERFORM 0080-WRITE-COMP-OUT THRU 0080-EXIT

           .
       0050-CONTINUE.

           PERFORM 0110-READ-ERCOMP    THRU 0110-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-CHECK-BYPASS.

           PERFORM VARYING B1 FROM +1 BY +1 UNTIL
              (B1 > BM1)
                 OR
                 ((CO-CARRIER     = WS-BP-CARRIER  (B1))
                 AND (CO-GROUPING = WS-BP-GROUP    (B1))
                 AND (CO-RESP-NO  = WS-BP-FIN-RESP (B1))
                 AND (CO-ACCOUNT  = WS-BP-ACCOUNT  (B1)))
           END-PERFORM

           IF B1 > BM1
              CONTINUE
           ELSE
              SET BYPASS-ERCOMP-REC    TO TRUE
           END-IF

           .
       0060-EXIT.
           EXIT.


       0080-WRITE-COMP-OUT.

      ****  FILE 1 REQUIREMENTS
      ****  IF WE OWE MORE THAN $1.00 AND BILL CODE = 'B', 'C' OR 'E'
      ****     AND WE OWE FOR THE CURRENT MONTHS PRODUCTION

           IF CO-TYPE = 'A'
              IF CO-BILL-SW = 'B'
060506                     OR 'C' OR 'E'
                 IF CO-END-BAL < -1.00
                    COMPUTE WS-END-BAL = CO-CUR-CHG - CO-CUR-COM
                    IF (WS-END-BAL <= (CO-END-BAL + .10))
                       AND (WS-END-BAL >= (CO-END-BAL - .10))
                       AND (CO-STMT-TYPE NOT = 'RF4')
                       WRITE COMP-OUT-REC1
062519                               FROM ERCOMP-DETAIL-RECORD (6:263)
                       ADD 1         TO WS-ERCOMP-OUT1
                    ELSE
                       IF CO-STMT-TYPE = 'RF4'
                          WRITE COMP-OUT-REC4
062519                               FROM ERCOMP-DETAIL-RECORD (6:263)
                          ADD 1      TO WS-ERCOMP-OUT4
                       ELSE
                          WRITE COMP-OUT-REC2
                                     FROM ERCOMP-DETAIL-RECORD
                          ADD 1      TO WS-ERCOMP-OUT2
                       END-IF
                    END-IF
                 END-IF
              ELSE
                 IF CO-BILL-SW = 'S'
                    IF CO-END-BAL < -1.00
062519                 move ERCOMP-DETAIL-RECORD
062519                                 to ERCOMP-DETAIL-RECORD-ext3
052419                 if co-stmt-owner = spaces or low-values
052419                    move co-csr-code
052419                                 to co-stmt-owner
052419                 end-if
062519                 if co-report-group-id = zeros or low-values
062519                    move spaces  to co-report-group-id
062519                 end-if
052419                 move co-stmt-owner
062519                                 to ex3-csr-code
062519                 move co-report-group-id
062519                                 to ex3-report-group-id
062519                 move ';'        to ex3-tab19
062519                 move '*'        to ex3-eor
052419                 WRITE COMP-OUT-REC3
062519                           FROM ERCOMP-DETAIL-RECORD-ext3
052419                 ADD 1           TO WS-ERCOMP-OUT3
                    END-IF
                 END-IF
              END-IF
           END-IF

           .
       0080-EXIT.
           EXIT.

       0110-READ-ERCOMP.

           READ ERCOMP NEXT RECORD

           IF (ERCOMP-FILE-STATUS = '10' OR '23')
              OR (CO-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-ERCOMP        TO TRUE
           ELSE
              IF ERCOMP-FILE-STATUS NOT = '00'
                 DISPLAY ' ERCOMP READ ERROR ' ERCOMP-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           IF NOT END-OF-ERCOMP
              ADD +1                   TO WS-ERCOMP-IN
           END-IF

           .
       0110-EXIT.
           EXIT.

       0120-START-ERCOMP.

           MOVE LOW-VALUES             TO CO-CONTROL-PRIMARY

           MOVE DTE-CLASIC-COMPANY-CD  TO CO-COMPANY-CD

           START ERCOMP KEY >= CO-CONTROL-PRIMARY

           IF ERCOMP-FILE-STATUS = '10' OR '23'
              SET END-OF-ERCOMP        TO TRUE
           ELSE
              IF ERCOMP-FILE-STATUS NOT = '00'
                 DISPLAY ' ERCOMP START ERROR ' ERCOMP-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0120-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
