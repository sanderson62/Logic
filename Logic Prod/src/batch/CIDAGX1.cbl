       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDAGX1.
       AUTHOR.        CENTRAL STATES HEALTH AND LIFE.
       DATE-COMPILED.
      *REMARKS.
030404******************************************************************
030404*                   C H A N G E   L O G
030404*
030404* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
030404*-----------------------------------------------------------------
030404*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
030404* EFFECTIVE    NUMBER
030404*-----------------------------------------------------------------
030404* 030404                   SMVA  ADD PROCESSING FOR DCC
052704* 052704  IR2004052400001  SMVA  CHG DELIMITER FROM TAB TO ;
083105* 083105  CR2005031100005  PEMA  ADD FIELDS TO EXTRACT
011807* 011807                   PEMA  ADD DATEFILE PROCESSING
030404******************************************************************

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

       DATA DIVISION.
       FILE SECTION.

       FD  COMP-OUT
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

       01  COMP-OUT-REC                PIC X(590).

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  ERCOMP.

                                       COPY ERCCOMP.

           EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CIDCOX1 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

030404 01  WS-MISC.
           05  WS-EOF-SW               PIC X      VALUE SPACES.
               88  END-OF-ERCOMP                  VALUE 'Y'.
           05  ERCOMP-FILE-STATUS      PIC XX     VALUE '00'.
           05  SUB1            COMP-3  PIC S9(3)  VALUE +0.

           05  WS-WORK-CITY-ST.
               10  WS-BYTE OCCURS 29   PIC X.
           05  WS-SAVE-ERCOMP          PIC X(590) VALUE LOW-VALUES.
           05  WS-ERCOMP-IN            PIC 9(7)   VALUE ZEROS.
           05  WS-ERCOMP-OUT           PIC 9(7)   VALUE ZEROS.


       01  ERCOMP-DETAIL-RECORD.
           12  EX-CARRIER              PIC X.
           12  EX-TAB1                 PIC X.
           12  EX-GROUPING             PIC X(6).
           12  EX-TAB2                 PIC X.
           12  EX-RESP-NO              PIC X(10).
           12  EX-TAB3                 PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-TAB4                 PIC X.
           12  EX-TYPE                 PIC X.
           12  EX-TAB5                 PIC X.
           12  EX-BALANCE-CONTROL      PIC X.
           12  EX-TAB6                 PIC X.
           12  EX-ACCT-NAME            PIC X(30).
           12  EX-TAB7                 PIC X.
           12  EX-MAIL-NAME            PIC X(30).
           12  EX-TAB8                 PIC X.
           12  EX-ADDR-1               PIC X(30).
           12  EX-TAB9                 PIC X.
           12  EX-ADDR-2               PIC X(30).
           12  EX-TAB10                PIC X.
           12  EX-ADDR-3               PIC X(29).
           12  EX-TAB11                PIC X.
           12  EX-CSO-1099             PIC X.
           12  EX-TAB12                PIC X.
           12  EX-ZIP                  PIC 9(9).
           12  EX-TAB13                PIC X.
           12  EX-SOC-SEC              PIC X(13).
           12  EX-TAB14                PIC X.
           12  EX-TELEPHONE            PIC X(10).
           12  EX-TAB15                PIC X.
           12  EX-BAL-FWD              PIC -9(7).99.
           12  EX-TAB16                PIC X.
           12  EX-CUR-COM              PIC -9(7).99.
           12  EX-TAB17                PIC X.
           12  EX-CUR-CHG              PIC -9(7).99.
           12  EX-TAB18                PIC X.
           12  EX-CUR-PMT              PIC -9(7).99.
           12  EX-TAB19                PIC X.
           12  EX-END-BAL              PIC -9(7).99.
           12  EX-TAB20                PIC X.
           12  EX-YTD-COM              PIC -9(7).99.
           12  EX-TAB21                PIC X.
           12  EX-YTD-OV               PIC -9(7).99.
           12  EX-TAB22                PIC X.
           12  EX-CSR-CODE             PIC X(4).
           12  EX-TAB23                PIC X.
           12  EX-FAXNO                PIC X(10).
           12  EX-TAB24                PIC X.
           12  EX-BILL-SW              PIC X.
           12  EX-TAB25                PIC X.
           12  EX-CONTROL-NAME         PIC X(30).
           12  EX-TAB26                PIC X.
           12  EX-STATE                PIC XX.
           12  EX-TAB27                PIC X.
           12  EX-STATUS-CODE          PIC X.
           12  EX-TAB28                PIC X.
           12  EX-EFF-DT               PIC X(10).
           12  EX-TAB29                PIC X.
           12  EX-EXP-DT               PIC X(10).
           12  EX-TAB30                PIC X.
           12  EX-COMMENT-1            PIC X(40).
           12  EX-TAB31                PIC X.
           12  EX-COMMENT-2            PIC X(40).
           12  EX-TAB32                PIC X.
           12  EX-COMMENT-3            PIC X(40).
           12  EX-TAB33                PIC X.
           12  EX-COMMENT-4            PIC X(40).
           12  EX-TAB34                PIC X.
083105     12  EX-CLP-STATE            PIC XX.
           12  EX-TAB35                PIC X.
           12  EX-BANK-FEE             PIC -9(5).99.
           12  EX-TAB36                PIC X.
           12  EX-BANK-FEE-LEASE       PIC -9(5).99.
           12  EX-TAB37                PIC X.
           12  EX-LAST-MAINT-USER      PIC XXXX.
           12  EX-TAB38                PIC X.
           12  EX-LAST-MAINT-DT        PIC X(10).

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

                                       COPY ELCDTERX.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT
           PERFORM 0040-INIT           THRU 0040-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
                (END-OF-ERCOMP)
PEMTST*         OR (WS-ERCOMP-IN > 1000)

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY ' RECORDS IN    ' WS-ERCOMP-IN
           DISPLAY ' RECORDS  OUT  ' WS-ERCOMP-OUT
           GOBACK

           .
       0002-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT ERCOMP
               OUTPUT COMP-OUT

           IF ERCOMP-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERCOMP OPEN ERROR ' ERCOMP-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE ERCOMP COMP-OUT

           IF ERCOMP-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERCOMP CLOSE ERROR ' ERCOMP-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0030-EXIT.
           EXIT.

       0040-INIT.

           MOVE SPACES                 TO ERCOMP-DETAIL-RECORD
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
                                          EX-TAB12
                                          EX-TAB13
                                          EX-TAB14
                                          EX-TAB15
                                          EX-TAB16
                                          EX-TAB17
                                          EX-TAB18
                                          EX-TAB19
                                          EX-TAB20
                                          EX-TAB21
                                          EX-TAB22
                                          EX-TAB23
                                          EX-TAB24
                                          EX-TAB25
                                          EX-TAB26
                                          EX-TAB27
                                          EX-TAB28
                                          EX-TAB29
                                          EX-TAB30
                                          EX-TAB31
                                          EX-TAB32
                                          EX-TAB33
                                          EX-TAB34
                                          EX-TAB35
                                          EX-TAB36
                                          EX-TAB37
                                          EX-TAB38
           MOVE '**'                   TO EX-STATE

           MOVE ERCOMP-DETAIL-RECORD   TO WS-SAVE-ERCOMP
           PERFORM 0120-START-ERCOMP   THRU 0120-EXIT
           PERFORM 0110-READ-ERCOMP    THRU 0110-EXIT

           .
       0040-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           MOVE WS-SAVE-ERCOMP         TO ERCOMP-DETAIL-RECORD

           MOVE CO-CARRIER             TO EX-CARRIER
           MOVE CO-GROUPING            TO EX-GROUPING
           MOVE CO-RESP-NO             TO EX-RESP-NO
           IF CO-ACCOUNT = LOW-VALUES
              MOVE SPACES              TO EX-ACCOUNT
           ELSE
              MOVE CO-ACCOUNT          TO EX-ACCOUNT
           END-IF
           MOVE CO-TYPE                TO EX-TYPE
           MOVE CO-BALANCE-CONTROL     TO EX-BALANCE-CONTROL
           MOVE CO-ACCT-NAME           TO EX-ACCT-NAME
           IF CO-MAIL-NAME = LOW-VALUES
              MOVE SPACES              TO EX-MAIL-NAME
           ELSE
              MOVE CO-MAIL-NAME        TO EX-MAIL-NAME
           END-IF
           MOVE CO-ADDR-1              TO EX-ADDR-1
           MOVE CO-ADDR-2              TO EX-ADDR-2
           move co-addr-3              to ws-work-city-st
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
      *    MOVE CO-ADDR-3              TO EX-ADDR-3
           MOVE CO-CSO-1099            TO EX-CSO-1099
           MOVE CO-ZIP                 TO EX-ZIP
           IF CO-SOC-SEC (1:5) = LOW-VALUES
              MOVE SPACES              TO EX-SOC-SEC
           ELSE
              MOVE CO-SOC-SEC          TO EX-SOC-SEC
           END-IF
           MOVE CO-TELEPHONE           TO EX-TELEPHONE
           MOVE CO-BAL-FWD             TO EX-BAL-FWD
           MOVE CO-CUR-COM             TO EX-CUR-COM
           MOVE CO-CUR-CHG             TO EX-CUR-CHG
           MOVE CO-CUR-PMT             TO EX-CUR-PMT
           MOVE CO-END-BAL             TO EX-END-BAL
           MOVE CO-YTD-COM             TO EX-YTD-COM
           MOVE CO-YTD-OV              TO EX-YTD-OV
           IF CO-CSR-CODE = LOW-VALUES
              MOVE SPACES              TO EX-CSR-CODE
           ELSE
              MOVE CO-CSR-CODE         TO EX-CSR-CODE
           END-IF
           IF CO-FAXNO = LOW-VALUES
              MOVE SPACES              TO EX-FAXNO
           ELSE
              MOVE CO-FAXNO            TO EX-FAXNO
           END-IF
           MOVE CO-BILL-SW             TO EX-BILL-SW
           INSPECT CO-CONTROL-NAME
              REPLACING ALL X'0C' BY SPACES
                        ALL X'00' BY SPACES
           MOVE CO-CONTROL-NAME        TO EX-CONTROL-NAME

           MOVE CO-GA-STATUS-CODE      TO EX-STATUS-CODE

           MOVE CO-GA-EFFECTIVE-DT     TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-EFF-DT
           ELSE
              MOVE SPACES              TO EX-EFF-DT
           END-IF

           IF CO-GA-TERMINATION-DT = HIGH-VALUES
              MOVE '12/31/9999'        TO EX-EXP-DT
           ELSE
              MOVE CO-GA-TERMINATION-DT
                                       TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT
                                       TO EX-EXP-DT
              ELSE
                 MOVE SPACES           TO EX-EXP-DT
              END-IF
           END-IF
           MOVE CO-GA-COMMENT-1        TO EX-COMMENT-1
           MOVE CO-GA-COMMENT-2        TO EX-COMMENT-2
           MOVE CO-GA-COMMENT-3        TO EX-COMMENT-3
           MOVE CO-GA-COMMENT-4        TO EX-COMMENT-4

083105     IF DTE-CLIENT = 'DCC'
083105        IF CO-MAX-BANK-FEE NOT NUMERIC
083105           MOVE +0               TO CO-MAX-BANK-FEE
083105        END-IF
083105        IF CO-MAX-BANK-FEE-LEASE NOT NUMERIC
083105           MOVE +0               TO CO-MAX-BANK-FEE-LEASE
083105        END-IF
083105        IF CO-CLP-STATE = SPACES OR ZEROS OR LOW-VALUES
083105           MOVE SPACES           TO CO-CLP-STATE
083105        END-IF
083105        MOVE CO-CLP-STATE        TO EX-CLP-STATE
083105        MOVE CO-MAX-BANK-FEE     TO EX-BANK-FEE
083105        MOVE CO-MAX-BANK-FEE-LEASE
083105                                 TO EX-BANK-FEE-LEASE
083105     ELSE
083105        MOVE ZEROS               TO EX-BANK-FEE
083105                                    EX-BANK-FEE-LEASE
083105        MOVE SPACES              TO EX-CLP-STATE
083105     END-IF

083105     MOVE CO-LAST-MAINT-USER     TO EX-LAST-MAINT-USER
083105     MOVE CO-LAST-MAINT-DT       TO DC-BIN-DATE-1
083105     MOVE ' '                    TO DC-OPTION-CODE
083105     PERFORM 8510-DATE-CONVERSION
083105                                 THRU 8590-EXIT
083105     IF NO-CONVERSION-ERROR
083105        MOVE DC-GREG-DATE-A-EDIT TO EX-LAST-MAINT-DT
083105     ELSE
083105        MOVE SPACES              TO EX-LAST-MAINT-DT
083105     END-IF

           PERFORM 0080-WRITE-COMP-OUT THRU 0080-EXIT
           PERFORM 0110-READ-ERCOMP    THRU 0110-EXIT

           .
       0050-EXIT.
           EXIT.

       0080-WRITE-COMP-OUT.

           WRITE COMP-OUT-REC          FROM ERCOMP-DETAIL-RECORD
           ADD 1 TO WS-ERCOMP-OUT

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

