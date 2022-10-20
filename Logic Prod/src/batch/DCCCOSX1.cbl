       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DCCCOSX1
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

           SELECT COMP-IN      ASSIGN TO COMPIN.

           SELECT DISK-DATE    ASSIGN TO SYS019.

           SELECT COMP-OUT     ASSIGN TO COMPOT
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  COMP-IN
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

                                       COPY ERCCOMP.

       FD  COMP-OUT
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

       01  COMP-OUT-REC                PIC X(296).

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   DCCCOSX1  WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  WS-WORK-WITHOLD             PIC S9(7)V99 VALUE +0 COMP-3.

       01  WS-MISC.
           05  WS-EOF-SW               PIC X      VALUE SPACES.
               88  END-OF-INPUT                   VALUE 'Y'.
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
           12  EX-CARRIER              PIC X.
           12  EX-TAB1                 PIC X.
           12  EX-GROUPING             PIC X(6).
           12  EX-TAB2                 PIC X.
           12  EX-RESP-NO              PIC X(10).
           12  EX-TAB3                 PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-TAB4                 PIC X.
           12  EX-ACCT-NAME            PIC X(30).
           12  EX-TAB5                 PIC X.
           12  EX-BAL-FWD              PIC -------.99.
           12  EX-TAB6                 PIC X.
           12  EX-CUR-COM              PIC -------.99.
           12  EX-TAB7                 PIC X.
           12  EX-CUR-CHG              PIC -------.99.
           12  EX-TAB8                 PIC X.
           12  EX-CUR-PMT              PIC -------.99.
           12  EX-TAB9                 PIC X.
           12  EX-END-BAL              PIC -------.99.
           12  EX-TAB10                PIC X.
           12  EX-CURRENT-BAL-FWD      PIC -------.99.
           12  EX-TAB11                PIC X.
           12  EX-CURRENT-CUR-COM      PIC -------.99.
           12  EX-TAB12                PIC X.
           12  EX-CURRENT-CUR-CHG      PIC -------.99.
           12  EX-TAB13                PIC X.
           12  EX-CURRENT-CUR-PMT      PIC -------.99.
           12  EX-TAB14                PIC X.
           12  EX-CURRENT-END-BAL      PIC -------.99.
           12  EX-TAB15                PIC X.
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
                (END-OF-INPUT)
PEMTST*         OR (WS-ERCOMP-IN > 1000)

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY '   RECORDS IN ' WS-ERCOMP-IN
           DISPLAY ' RECORDS  OUT ' WS-ERCOMP-OUT
           GOBACK

           .
       0002-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT COMP-IN
               OUTPUT COMP-OUT

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE COMP-IN COMP-OUT

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
           MOVE 'E'                    TO EX-EOR

           MOVE ERCOMP-DETAIL-RECORD   TO WS-SAVE-ERCOMP
           PERFORM 0110-READ-ERCOMP    THRU 0110-EXIT

           .
       0040-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           IF CO-RESP-NO = '0005500363'
              PERFORM 0060-PROCESS-FILE
                                       THRU 0060-EXIT
           END-IF

           PERFORM 0110-READ-ERCOMP    THRU 0110-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-PROCESS-FILE.

           MOVE WS-SAVE-ERCOMP         TO ERCOMP-DETAIL-RECORD

           MOVE CO-CARRIER             TO EX-CARRIER
           MOVE CO-GROUPING            TO EX-GROUPING
           MOVE CO-RESP-NO             TO EX-RESP-NO
           MOVE CO-ACCOUNT             TO EX-ACCOUNT
           MOVE CO-ACCT-NAME           TO EX-ACCT-NAME

           MOVE CO-BAL-FWD             TO EX-BAL-FWD
           MOVE CO-CUR-COM             TO EX-CUR-COM
           MOVE CO-CUR-CHG             TO EX-CUR-CHG
           MOVE CO-CUR-PMT             TO EX-CUR-PMT
           MOVE CO-END-BAL             TO EX-END-BAL
           
           MOVE CO-CURRENT-BAL-FWD     TO EX-CURRENT-BAL-FWD
           MOVE CO-CURRENT-CUR-COM     TO EX-CURRENT-CUR-COM
           MOVE CO-CURRENT-CUR-CHG     TO EX-CURRENT-CUR-CHG
           MOVE CO-CURRENT-CUR-PMT     TO EX-CURRENT-CUR-PMT
           MOVE CO-CURRENT-END-BAL     TO EX-CURRENT-END-BAL

           INSPECT EX-ACCT-NAME REPLACING ALL '*' BY ' '

           PERFORM 0080-WRITE-COMP-OUT THRU 0080-EXIT

           .
       0060-EXIT.
           EXIT.

       0080-WRITE-COMP-OUT.

           WRITE COMP-OUT-REC          FROM ERCOMP-DETAIL-RECORD
           ADD 1                       TO WS-ERCOMP-OUT

           .
       0080-EXIT.
           EXIT.

       0110-READ-ERCOMP.

           READ COMP-IN AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD +1                   TO WS-ERCOMP-IN
           END-IF

           .
       0110-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
