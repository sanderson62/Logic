       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDMAX2.
       AUTHOR.     PABLO.
       DATE-COMPILED.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 111108   2008102900001   PEMA  NEW PROGRAM.
092820* 092820   2020092800002   PEMA  Fix x'17'.
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERMAIL           ASSIGN TO ERMAIL
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS MA-CONTROL-PRIMARY
                                   FILE STATUS IS ERMAIL-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT EXTR-OUT         ASSIGN TO EXTROT
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  ERMAIL.

           COPY ERCMAIL.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  EXTR-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  EXTR-OUT-REC                PIC X(800).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDMAX2  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  WS-DIS-DATE                 PIC 9(8)  VALUE ZEROS.

           01  WS-MISC.
           05  WS-WORK-DT              PIC 9(8)  VALUE ZEROS.
           05  WS-WORK-DTR REDEFINES WS-WORK-DT.
               10  WS-WORK-DT-CCYY      PIC X(4).
               10  WS-WORK-DT-MM        PIC XX.
               10  WS-WORK-DT-DD        PIC XX.
           05  PGM-SUB                 PIC S999 COMP  VALUE +158.
           05  WS-EOF-SW               PIC X      VALUE SPACES.
               88  END-OF-ERMAIL                  VALUE 'Y'.
           05  ERMAIL-RECS-IN          PIC 9(9)   VALUE ZEROS.
           05  EXTR-RECS-OUT           PIC 9(9)   VALUE ZEROS.
           05  S1                      PIC S999   VALUE +0 COMP-3.

072403     05  ERMAIL-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  WS-DATE                 PIC 9(11)  VALUE ZEROS.

030404**** PROGRAM ABEND FIELDS
030404     05  WS-RETURN-CODE          PIC S9(03) VALUE +0.
030404     05  WS-ZERO                 PIC S9(01) VALUE +0.
030404     05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.
030404     05  WS-ABEND-FILE-STATUS    PIC X(02)  VALUE ZERO.


       01  WS-SAVE-EXTR                PIC X(800) VALUE LOW-VALUES.
       01  EXTR-DETAIL-RECORD.
           12  EX-CARRIER              PIC X.
           12  EX-TAB1                 PIC X.
           12  EX-GROUPING             PIC X(6).
           12  EX-TAB2                 PIC X.
           12  EX-STATE                PIC XX.
           12  EX-TAB3                 PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-TAB4                 PIC X.
           12  EX-CERT-EFF-DT          PIC X(10).
           12  EX-TAB5                 PIC X.
           12  EX-CERT-NO              PIC X(11).
           12  EX-TAB6                 PIC X.
           12  EX-CRED-BENE-NAME       PIC X(30).
           12  EX-TAB7                 PIC X.
           12  EX-CRED-BENE-ADDR       PIC X(30).
           12  EX-TAB8                 PIC X.
           12  EX-CRED-BENE-ADDR2      PIC X(30).
           12  EX-TAB9                 PIC X.
           12  EX-CRED-BENE-CITYST     PIC X(30).
           12  EX-TAB10                PIC X.
           12  EX-CRED-BENE-ZIP        PIC X(9).
           12  EX-TAB11                PIC X.
           12  EX-INS-FIRST-NAME       PIC X(10).
           12  EX-TAB12                PIC X.
           12  EX-INS-MID-INIT         PIC X.
           12  EX-TAB13                PIC X.
           12  EX-INS-LAST-NAME        PIC X(15).
           12  EX-TAB14                PIC X.
           12  EX-INS-FULL-NAME        PIC X(30).
           12  EX-TAB15                PIC X.
           12  EX-INS-ADDR1            PIC X(30).
           12  EX-TAB16                PIC X.
           12  EX-INS-ADDR2            PIC X(30).
           12  EX-TAB17                PIC X.
           12  EX-INS-CITYST           PIC X(30).
           12  EX-TAB18                PIC X.
           12  EX-INS-ZIP              PIC X(9).
           12  EX-TAB19                PIC X.
           12  EX-INS-CITY             PIC X(28).
           12  EX-TAB20                PIC X.
           12  EX-INS-STATE            PIC XX.
           12  EX-TAB21                PIC X.
           12  EX-EOR                  PIC X.

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

030404 PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0050-PROCESS-ERMAIL THRU 0050-EXIT UNTIL
              (END-OF-ERMAIL)
PEMTST*       OR (ERMAIL-RECS-IN > 10000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' MAIL RECORDS READ    '  ERMAIL-RECS-IN
           DISPLAY ' EXTR RECORDS WRITTEN '  EXTR-RECS-OUT

           GOBACK

           .
       0050-PROCESS-ERMAIL.

           IF (MA-CRED-BENE-NAME    NOT = SPACES)
              OR (MA-ADDRESS-LINE-1 NOT = SPACES)
              OR (MA-ADDRESS-LINE-2 NOT = SPACES)
              OR (MA-CITY-STATE     NOT = SPACES)
              PERFORM 0100-PROCESS-ERMAIL
                                       THRU 0100-EXIT
           END-IF

           PERFORM 0200-READ-ERMAIL    THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-ERMAIL.

           MOVE WS-SAVE-EXTR           TO EXTR-DETAIL-RECORD
           MOVE MA-CARRIER             TO EX-CARRIER
           MOVE MA-GROUPING            TO EX-GROUPING
           MOVE MA-STATE               TO EX-STATE
           MOVE MA-ACCOUNT             TO EX-ACCOUNT
           MOVE MA-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-CERT-EFF-DT
           END-IF

           MOVE MA-CERT-NO             TO EX-CERT-NO
           MOVE MA-CRED-BENE-NAME      TO EX-CRED-BENE-NAME
           MOVE MA-CRED-BENE-ADDR      TO EX-CRED-BENE-ADDR
           MOVE MA-CRED-BENE-ADDR2     TO EX-CRED-BENE-ADDR2
           MOVE MA-CRED-BENE-CTYST     TO EX-CRED-BENE-CITYST
           MOVE MA-CRED-BENE-ZIP       TO EX-CRED-BENE-ZIP
           MOVE MA-INSURED-FIRST-NAME  TO EX-INS-FIRST-NAME
           MOVE MA-INSURED-MIDDLE-INIT TO EX-INS-MID-INIT
           MOVE MA-INSURED-LAST-NAME   TO EX-INS-LAST-NAME

           IF MA-INSURED-MIDDLE-INIT NOT = SPACES
              STRING MA-INSURED-FIRST-NAME ' ' MA-INSURED-MIDDLE-INIT
                 ' ' MA-INSURED-LAST-NAME DELIMITED BY '  '
                 INTO EX-INS-FULL-NAME
              END-STRING
           ELSE
              STRING MA-INSURED-FIRST-NAME ' ' MA-INSURED-LAST-NAME
                 DELIMITED BY '  ' INTO EX-INS-FULL-NAME
              END-STRING
           END-IF
           MOVE MA-ADDRESS-LINE-1      TO EX-INS-ADDR1
           MOVE MA-ADDRESS-LINE-2      TO EX-INS-ADDR2
           STRING MA-CITY ' ' MA-ADDR-STATE DELIMITED BY '  '
              INTO EX-INS-CITYST
           END-STRING
           MOVE MA-ZIP                 TO EX-INS-ZIP
           MOVE MA-CITY                TO EX-INS-CITY
           MOVE MA-ADDR-STATE          TO EX-INS-STATE

           PERFORM 0300-WRITE-EXTR     THRU 0300-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-ERMAIL.

           READ ERMAIL NEXT RECORD

           IF (ERMAIL-FILE-STATUS = '10' OR '23')
              OR (MA-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-ERMAIL        TO TRUE
           ELSE
              IF ERMAIL-FILE-STATUS NOT = '00'
                 DISPLAY 'ERROR ON ERMAIL - READ NEXT '
                    ERMAIL-FILE-STATUS
                 SET END-OF-ERMAIL     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-ERMAIL
              ADD 1                    TO ERMAIL-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-EXTR.

092820     inspect extr-detail-record
092820        replacing all X'17' by spaces
092820     inspect extr-detail-record
092820        replacing all X'00' by spaces

           INSPECT EXTR-DETAIL-RECORD
              REPLACING ALL ';' BY SPACES
           INSPECT EXTR-DETAIL-RECORD
              REPLACING ALL X'09' BY ';'           

           WRITE EXTR-OUT-REC          FROM EXTR-DETAIL-RECORD
           ADD 1 TO EXTR-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

072403     OPEN INPUT ERMAIL
               OUTPUT EXTR-OUT

           IF ERMAIL-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY 'ERROR ON ERMAIL - OPEN ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE EXTR-OUT ERMAIL

           IF ERMAIL-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY 'ERROR ON ERMAIL - CLOSE ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0500-EXIT.
           EXIT.

       0550-START-ERMAIL.

           MOVE LOW-VALUES             TO MA-CONTROL-PRIMARY

030404     MOVE DTE-CLASIC-COMPANY-CD  TO MA-COMPANY-CD

           START ERMAIL KEY >= MA-CONTROL-PRIMARY

           IF ERMAIL-FILE-STATUS = '10' OR '23'
              SET END-OF-ERMAIL        TO TRUE
           ELSE
              IF ERMAIL-FILE-STATUS NOT = '00'
                 DISPLAY 'ERROR ON ERMAIL - START ' ERMAIL-FILE-STATUS
                 SET END-OF-ERMAIL     TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.


       0600-INITIALIZE.

           MOVE SPACES                 TO EXTR-DETAIL-RECORD
052704     MOVE X'09'                  TO EX-TAB1
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
           MOVE 'E'                    TO EX-EOR

           MOVE EXTR-DETAIL-RECORD     TO WS-SAVE-EXTR
           PERFORM 0550-START-ERMAIL   THRU 0550-EXIT
           PERFORM 0200-READ-ERMAIL    THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

030404 ABEND-PGM. COPY ELCABEND.
