       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDCOX3.
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
080107* 080107    2004020600003  PEMA  NEW PROGRAM
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERCOMP       ASSIGN TO ERCOMP
                               ACCESS IS SEQUENTIAL
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERCOMP-FILE-STATUS
                               RECORD KEY IS CO-CONTROL-PRIMARY.

           SELECT DISK-DATE    ASSIGN TO SYS019.

           SELECT COMP-OUT     ASSIGN TO COMPOT
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT COMP-OUT-DD  ASSIGN TO COMPOTDD
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  COMP-OUT
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

       01  COMP-OUT-REC                PIC X(296).

       FD  COMP-OUT-DD
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

       01  COMP-OUT-DD-REC             PIC X(296).

       FD  ERCOMP.

                                       COPY ERCCOMP.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CIDCOX3 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  WS-WORK-WITHOLD             PIC S9(7)V99 VALUE +0 COMP-3.

       01  WS-MISC.
           05  WS-EOF-SW               PIC X      VALUE SPACES.
               88  END-OF-ERCOMP                  VALUE 'Y'.
           05  ERCOMP-FILE-STATUS      PIC XX     VALUE '00'.
           05  SUB1            COMP-3  PIC S9(3)  VALUE +0.
           05  WS-COMPANY-CD           PIC X(01)  VALUE SPACE.

           05  WS-WORK-CITY-ST.
               10  WS-BYTE OCCURS 29   PIC X.
           05  WS-SAVE-ERCOMP          PIC X(301) VALUE LOW-VALUES.
           05  WS-ERCOMP-IN            PIC 9(7)   VALUE ZEROS.
           05  WS-ERCOMP-OUT           PIC 9(7)   VALUE ZEROS.
           05  WS-ERCOMP-DD-OUT        PIC 9(7)   VALUE ZEROS.
           05  WS-WITHOLD-PCT          PIC Z.999   VALUE ZEROS.


       01  ERCOMP-DETAIL-RECORD.
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
           12  EX-CHECK-AMT            PIC ---------.99.
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
           12  EX-COMMENT              PIC X(25).
           12  EX-TAB19                PIC X.
           12  EX-CHECK-TYPE           PIC X.
           12  EX-TAB20                PIC X.
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

           DISPLAY '   RECORDS IN ' WS-ERCOMP-IN
           DISPLAY ' RECORDS  OUT ' WS-ERCOMP-OUT
           GOBACK

           .
       0002-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT ERCOMP
               OUTPUT COMP-OUT COMP-OUT-DD

           IF ERCOMP-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERCOMP OPEN ERROR ' ERCOMP-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE ERCOMP COMP-OUT COMP-OUT-DD

           IF ERCOMP-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERCOMP CLOSE ERROR ' ERCOMP-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0030-EXIT.
           EXIT.

       0040-INIT.

           MOVE SPACES                 TO ERCOMP-DETAIL-RECORD
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
                                          EX-TAB20
           MOVE 'E'                    TO EX-EOR

           MOVE ERCOMP-DETAIL-RECORD   TO WS-SAVE-ERCOMP
           PERFORM 0120-START-ERCOMP   THRU 0120-EXIT
           PERFORM 0110-READ-ERCOMP    THRU 0110-EXIT

           .
       0040-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           IF (CO-TYPE = 'G')
              AND (CO-CREATE-AP-CHECK = 'Y')
      *       AND (CO-RESP-NO NOT = '0000736801' AND '0000738801'
      *                         AND '0000812700' AND '0000812800')
              IF CO-CURRENT-END-BAL < ZEROS
                 PERFORM 0060-PROCESS-FILE
                                       THRU 0060-EXIT
              END-IF
              PERFORM 0065-CHECK-MD-AMT
                                       THRU 0065-EXIT
           END-IF

           PERFORM 0110-READ-ERCOMP    THRU 0110-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-PROCESS-FILE.

           PERFORM 0070-BUILD-EXTR-REC THRU 0070-EXIT

           MOVE +0                     TO WS-WORK-WITHOLD

           IF CO-GA-WITHOLD-PCT NOT NUMERIC
              MOVE +0                  TO CO-GA-WITHOLD-PCT
           END-IF

      ***  I want to get the balance to a positive number for the entries
           COMPUTE CO-CURRENT-END-BAL = CO-CURRENT-END-BAL * -1

           COMPUTE WS-WORK-WITHOLD ROUNDED = CO-CURRENT-END-BAL
              * CO-GA-WITHOLD-PCT

           COMPUTE EX-CHECK-AMT = CO-CURRENT-END-BAL - WS-WORK-WITHOLD
           MOVE '1825011300'           TO EX-GL-NUM
           MOVE 'Commission Amount'    TO EX-COMMENT

           PERFORM 0080-WRITE-COMP-OUT THRU 0080-EXIT

           IF WS-WORK-WITHOLD NOT = ZEROS
              MOVE WS-WORK-WITHOLD     TO EX-CHECK-AMT
              MOVE CO-GA-WITHOLD-PCT   TO WS-WITHOLD-PCT
              STRING 'Withholding ' WS-WITHOLD-PCT '%'
                 INTO EX-COMMENT
              END-STRING
              PERFORM 0080-WRITE-COMP-OUT
                                       THRU 0080-EXIT
              MOVE '2718000120'        TO EX-GL-NUM
              COMPUTE EX-CHECK-AMT = WS-WORK-WITHOLD * -1
              PERFORM 0080-WRITE-COMP-OUT
                                       THRU 0080-EXIT
           END-IF

           .
       0060-EXIT.
           EXIT.

       0065-CHECK-MD-AMT.

           PERFORM 0070-BUILD-EXTR-REC THRU 0070-EXIT

           IF CO-MD-AMT NOT NUMERIC
              MOVE ZEROS               TO CO-MD-AMT
           END-IF

           IF CO-MD-AMT NOT = ZEROS
              MOVE CO-MD-AMT           TO EX-CHECK-AMT
              MOVE '1825011300'        TO EX-GL-NUM
              MOVE CO-MD-DIV           TO EX-DIV
              MOVE CO-MD-CENTER        TO EX-CENTER
              MOVE ZEROS               TO EX-LOB
                                          EX-STATE
              MOVE 'Misc. Deduction'   TO EX-COMMENT
              PERFORM 0080-WRITE-COMP-OUT
                                       THRU 0080-EXIT
              COMPUTE EX-CHECK-AMT = CO-MD-AMT * -1
              MOVE CO-MD-GL-ACCT       TO EX-GL-NUM
              PERFORM 0080-WRITE-COMP-OUT
                                       THRU 0080-EXIT
           END-IF

           .
       0065-EXIT.
           EXIT.

       0070-BUILD-EXTR-REC.

           MOVE WS-SAVE-ERCOMP         TO ERCOMP-DETAIL-RECORD

           MOVE CO-CARRIER             TO EX-CARRIER
           MOVE CO-GROUPING            TO EX-GROUPING
           MOVE CO-RESP-NO             TO EX-RESP-NO
           MOVE SPACES                 TO EX-ACCOUNT

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-INV-DATE
           END-IF

           MOVE '02'                   TO EX-DIV
           MOVE ZEROS                  TO EX-CENTER
           MOVE ZEROS                  TO EX-LOB
           MOVE ZEROS                  TO EX-STATE
           IF CO-DELIVER-CK-TO-MEL = 'Y'
              MOVE 'CK CC MEL'         TO EX-SPEC-INST
           ELSE
              MOVE 'ENC CC'            TO EX-SPEC-INST
           END-IF

           MOVE CO-ACCT-NAME           TO EX-ACCT-NAME
           MOVE CO-MAIL-NAME           TO EX-MAIL-NAME

           INSPECT EX-ACCT-NAME REPLACING ALL '*' BY ' '
           INSPECT EX-MAIL-NAME REPLACING ALL '*' BY ' '

           IF EX-MAIL-NAME = EX-ACCT-NAME
              MOVE SPACES              TO EX-MAIL-NAME
           END-IF

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

           IF EX-ADDR-1 = SPACES
              MOVE EX-ADDR-2           TO EX-ADDR-1
              MOVE SPACES              TO EX-ADDR-2
           END-IF

           .
       0070-EXIT.
           EXIT.

       0080-WRITE-COMP-OUT.

           IF CO-GA-DIRECT-DEP = 'Y'
              MOVE 'E'                 TO EX-CHECK-TYPE
              WRITE COMP-OUT-DD-REC    FROM ERCOMP-DETAIL-RECORD
              ADD 1                    TO WS-ERCOMP-DD-OUT
           ELSE
              MOVE 'P'                 TO EX-CHECK-TYPE
              WRITE COMP-OUT-REC       FROM ERCOMP-DETAIL-RECORD
              ADD 1                    TO WS-ERCOMP-OUT
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
