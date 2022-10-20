       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMGRX3.
       AUTHOR.     PABLO.
       DATE-COMPILED.
011604******************************************************************
011604*                   C H A N G E   L O G
011604*
011604* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
011604*-----------------------------------------------------------------
011604*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
011604* EFFECTIVE    NUMBER
011604*-----------------------------------------------------------------
011604*******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT GAAP-FILE-IN     ASSIGN TO GAAPIN.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT ELCNTL           ASSIGN TO ELCNTL
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CF-CONTROL-PRIMARY
                                   FILE STATUS IS ELCNTL-FILE-STATUS.

           SELECT GAAP-FILE-OUT    ASSIGN TO GAAPOT
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SUMM-FILE-OUT    ASSIGN TO SUMMOT
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  GAAP-FILE-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY ECSGAP01.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  ELCNTL.

           COPY ELCCNTL.

       FD  GAAP-FILE-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

031704 01  GAAP-FILE-OUT-REC           PIC X(292).

       FD  SUMM-FILE-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

031704 01  SUMM-FILE-OUT-REC           PIC X(116).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMGRX3  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-GAAP                   VALUE 'Y'.
       77  GAP-RECS-IN                 PIC 9(9) VALUE ZEROS.
       77  GAP-RECS-OUT                PIC 9(9) VALUE ZEROS.
       77  S1                          PIC S9(5) VALUE +0 COMP-3.
       77  PGM-SUB                     PIC S9(5) COMP-3 VALUE +515.
       77  ELCNTL-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  WS-ELCNTL-SW                PIC X   VALUE ' '.
           88  END-OF-ELCNTL                  VALUE 'Y'.
       77  ELCNTL-RECS-IN              PIC 9(5) VALUE ZEROS.
       77  WS-TEMP-TAX1                PIC S9(7)V99  COMP-3 VALUE +0.
       77  WS-TEMP-TAX                 PIC S9(7)V99  COMP-3 VALUE +0.
       77  WS-TAX-FACTOR               PIC S99V9(5)  COMP-3 VALUE +0.

       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04) COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

       01  FILLER.
           05  WS-ST-TABLE OCCURS 70.
               10  WS-STATE            PIC XX.
               10  WS-TAX-RATE         PIC S9V9(4) COMP-3.
               10  WS-UEP              PIC S9(11)V99 COMP-3.
               10  WS-UEC              PIC S9(11)V99 COMP-3.
               10  WS-UET              PIC S9(11)V99 COMP-3.
               10  WS-NRL              PIC S9(11)V99 COMP-3.
               10  WS-MORT-RES         PIC S9(11)V99 COMP-3.
               10  WS-ADDL-MORT-RES    PIC S9(11)V99 COMP-3.
               10  WS-TOT-MORT-RES     PIC S9(11)V99 COMP-3.
               
       01  WS-SAVE-SUMM                PIC X(116) VALUE LOW-VALUES.
       01  SUMM-DETAIL-RECORD.
           12  SM-STATE                PIC XX.
           12  SM-TAB1                 PIC X.
           12  SM-TAX-RATE             PIC 9.9999.
           12  SM-TAB2                 PIC X.
           12  SM-UEP                  PIC ----------9.99.
           12  SM-TAB3                 PIC X.
           12  SM-UEC                  PIC ----------9.99.
           12  SM-TAB4                 PIC X.
           12  SM-UET                  PIC ----------9.99.
           12  SM-TAB5                 PIC X.
           12  SM-NRL                  PIC ----------9.99.
           12  SM-TAB6                 PIC X.
           12  SM-MORT                 PIC ----------9.99.
           12  SM-TAB7                 PIC X.
           12  SM-ADDL-MORT            PIC ----------9.99.
           12  SM-TAB8                 PIC X.
           12  SM-TOT-MORT             PIC ----------9.99.
           12  SM-TAB9                 PIC X.
           12  SM-EOR                  PIC X.

       01  WS-SAVE-GAAP                PIC X(292) VALUE LOW-VALUES.
       01  GAAP-DETAIL-RECORD.
           12  EX-CARRIER              PIC X.
           12  EX-TAB1                 PIC X.
           12  EX-GROUPING             PIC X(6).
           12  EX-TAB2                 PIC X.
           12  EX-STATE                PIC XX.
           12  EX-TAB3                 PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-TAB4                 PIC X.
           12  EX-EFF                  PIC X(10).
           12  EX-TAB5                 PIC X.
           12  EX-CERT-NO              PIC X(11).
           12  EX-TAB6                 PIC X.
           12  EX-AGE                  PIC 99.
           12  EX-TAB7                 PIC X.
           12  EX-VAGE                 PIC 99.
           12  EX-TAB8                 PIC X.
           12  EX-APR                  PIC ZZ9.9999.
           12  EX-TAB9                 PIC X.
           12  EX-LFTYP                PIC XX.
           12  EX-TAB10                PIC X.
           12  EX-LF-TERM              PIC 999.
           12  EX-TAB11                PIC X.
           12  EX-LF-REMTERM           PIC 999.
           12  EX-TAB12                PIC X.
           12  EX-LFBEN                PIC -9(9).99.
           12  EX-TAB13                PIC X.
           12  EX-LFPRM                PIC -9(7).99.
           12  EX-TAB14                PIC X.
           12  EX-REM-AMT              PIC -9(8).99.
           12  EX-TAB15                PIC X.
           12  EX-SLFPRM               PIC -9(7).99.
           12  EX-TAB16                PIC X.
           12  EX-SLFCOM               PIC -9(7).99.
011604     12  EX-TAB17                PIC X.
           12  EX-LFTAX                PIC -9(7).99.
           12  EX-TAB18                PIC X.
           12  EX-SLFTAX               PIC -9(7).99.
           12  EX-TAB18A               PIC X.
011604     12  EX-MORT-RESV            PIC -9(7).99.
031704     12  EX-TAB19                PIC X.
011604     12  EX-MO-DEC               PIC -9(7).99.
           12  EX-TAB20                PIC X.
           12  EX-TABLE                PIC XXXX.
           12  EX-TAB21                PIC X.
           12  EX-ALT-TABLE            PIC XXXX.
           12  EX-TAB22                PIC X.
           12  EX-ALT-MORT-RESV        PIC -9(7).99.
           12  EX-TAB23                PIC X.
           12  EX-FIT-RESV             PIC -9(7).99.
           12  EX-TAB24                PIC X.
           12  EX-EOR                  PIC X.

      ******************************************************************
       01  WS-MISC.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
           05  FILLER REDEFINES WS-DATE.
               10  FILLER              PIC 999.
               10  WS-CCYY             PIC 9999.
               10  WS-MM               PIC 99.
               10  WS-DD               PIC 99.

                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.
                                       COPY ELCDATE.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0050-PROCESS-GAAP   THRU 0050-EXIT UNTIL
                 (END-OF-GAAP)
PEMTST*          OR (GAP-RECS-OUT > 5999)

           PERFORM 0700-FINISH-TABLE   THRU 0700-EXIT
           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' GAAP RECORDS READ    '  GAP-RECS-IN
           DISPLAY ' GAAP RECORDS WRITTEN '  GAP-RECS-OUT
           GOBACK

           .
       0050-PROCESS-GAAP.

           IF (GR-REIN = 'P')
      *       AND (GR-LFTYP = '1E')
      *       AND (GR-EFF > 20091231)
      *       AND (GR-STATE = 'AL')
              AND (GR-LFTYP NOT = '  ' AND '00')
              PERFORM 0100-PROCESS-GAAP
                                       THRU 0100-EXIT
           END-IF

           PERFORM 0200-READ-GAAP      THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-GAAP.

           MOVE WS-SAVE-GAAP           TO GAAP-DETAIL-RECORD
           MOVE GR-CARRIER             TO EX-CARRIER
           MOVE GR-GROUPING            TO EX-GROUPING
           MOVE GR-STATE               TO EX-STATE
           MOVE GR-ACCOUNT             TO EX-ACCOUNT
           MOVE GR-EFF                 TO WS-DATE
           MOVE ' '                    TO DC-OPTION-CODE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EX-EFF
           END-STRING

           MOVE GR-CERT-NO             TO EX-CERT-NO
           MOVE GR-APR                 TO EX-APR
           MOVE GR-AGE                 TO EX-AGE
           MOVE GR-MORT-AGE            TO EX-VAGE
           MOVE GR-LFTYP               TO EX-LFTYP
           MOVE GR-LF-TERM             TO EX-LF-TERM
           MOVE GR-LF-REMTERM          TO EX-LF-REMTERM
           MOVE GR-LFBEN               TO EX-LFBEN
           MOVE GR-LFPRM               TO EX-LFPRM
           MOVE GR-REM-AMT             TO EX-REM-AMT
           MOVE GRS-LFPRM              TO EX-SLFPRM
           MOVE GRS-LFCOM              TO EX-SLFCOM
011604     MOVE GR-RESV                TO EX-MORT-RESV
           MOVE GR-MO-DEC              TO EX-MO-DEC
           MOVE GR-MORT-CODE           TO EX-TABLE
           MOVE GR-ALT-MORT-CODE       TO EX-ALT-TABLE
           MOVE GR-ALT-RESV            TO EX-ALT-MORT-RESV
           MOVE GR-LFTAX               TO EX-LFTAX
           MOVE GR-LFSRV               TO EX-FIT-RESV

           COMPUTE WS-TEMP-TAX ROUNDED =
                    (GRS-LFPRM / GR-LFPRM) * GR-LFTAX
           MOVE WS-TEMP-TAX            TO EX-SLFTAX

      *    IF GR-STATE = WS-STATE (S1)
      *       CONTINUE
      *    ELSE
      *       PERFORM VARYING S1 FROM +1 BY +1 UNTIL
      *          (GR-STATE = WS-STATE (S1))
      *          OR (S1 > +70)
      *       END-PERFORM
      *    END-IF
      *    IF S1 <= +70
      *       COMPUTE WS-TEMP-TAX = GRS-LFPRM * WS-TAX-RATE (S1)
      *       MOVE WS-TEMP-TAX         TO EX-SLFTAX
      *    ELSE
      *       DISPLAY ' STATE NOT FOUND ' GR-STATE
      *    END-IF
      *
      *    IF (GR-LFTAX = ZERO)
      *       AND (GR-LFPRM NOT = ZERO)
      *       DISPLAY ' TAX ZERO BUT PREM NOT ' GR-STATE
      *          ' ' GR-ACCOUNT ' ' GR-CERT-NO
      *    END-IF
      *
      *    IF GR-LFPRM NOT = ZERO
      *       COMPUTE WS-TAX-FACTOR = GR-LFTAX / GR-LFPRM
      *       COMPUTE WS-TEMP-TAX1 = WS-TAX-FACTOR * GRS-LFPRM
      *       MOVE WS-TEMP-TAX1        TO EX-SLFTAX-CALC
      *    ELSE
      *       DISPLAY ' LF PREM ZEROS ' GR-ACCOUNT ' ' GR-CERT-NO
      *    END-IF

           MOVE 'E'                    TO EX-EOR

           COMPUTE WS-UEP (S1) = WS-UEP (S1) + GRS-LFPRM
           COMPUTE WS-UEC (S1) = WS-UEC (S1) + GRS-LFCOM
           COMPUTE WS-UET (S1) = WS-UET (S1) + WS-TEMP-TAX
           COMPUTE WS-MORT-RES (S1) = WS-MORT-RES (S1) + GR-RESV

           PERFORM 0300-WRITE-GAAP     THRU 0300-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-GAAP.

           READ GAAP-FILE-IN AT END
              SET END-OF-GAAP          TO TRUE
           END-READ


           IF NOT END-OF-GAAP
              ADD 1 TO GAP-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-GAAP.

           WRITE GAAP-FILE-OUT-REC     FROM GAAP-DETAIL-RECORD
           ADD 1 TO GAP-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT GAAP-FILE-IN ELCNTL
               OUTPUT GAAP-FILE-OUT SUMM-FILE-OUT

           IF ELCNTL-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ELCNTL - OPEN ' ELCNTL-FILE-STATUS
           END-IF

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE GAAP-FILE-IN GAAP-FILE-OUT ELCNTL SUMM-FILE-OUT

           IF ELCNTL-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ELCNTL - CLOSE ' ELCNTL-FILE-STATUS
           END-IF

           .
       0500-EXIT.
           EXIT.


       0600-INITIALIZE.

           MOVE SPACES                 TO GAAP-DETAIL-RECORD
           MOVE SPACES                 TO SUMM-DETAIL-RECORD
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
                                          EX-TAB18A
                                          EX-TAB19
                                          EX-TAB20
                                          EX-TAB21
                                          EX-TAB22
                                          EX-TAB23
                                          EX-TAB24
                                          SM-TAB1
                                          SM-TAB2
                                          SM-TAB3
                                          SM-TAB4
                                          SM-TAB5
                                          SM-TAB6
                                          SM-TAB7
                                          SM-TAB8
                                          SM-TAB9

           MOVE GAAP-DETAIL-RECORD     TO WS-SAVE-GAAP
           MOVE SUMM-DETAIL-RECORD     TO WS-SAVE-SUMM

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +70
              MOVE LOW-VALUES          TO WS-STATE         (S1)
              MOVE +0                  TO WS-TAX-RATE      (S1)
                                          WS-UEP           (S1)
                                          WS-UEC           (S1)
                                          WS-UET           (S1)
                                          WS-NRL           (S1)
                                          WS-MORT-RES      (S1)
                                          WS-ADDL-MORT-RES (S1)
                                          WS-TOT-MORT-RES  (S1)
           END-PERFORM

           PERFORM 0200-READ-GAAP      THRU 0200-EXIT

           PERFORM 0610-START-ELCNTL   THRU 0610-EXIT
           PERFORM 0620-READ-ELCNTL    THRU 0620-EXIT
           MOVE +0                     TO S1
           PERFORM 0615-BUILD-TAX      THRU 0615-EXIT UNTIL
              END-OF-ELCNTL

           DISPLAY ' TAX TABLE BUILT ' S1
           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              (WS-STATE (S1) = LOW-VALUES)
              OR (S1 > +70)
              DISPLAY ' ' WS-STATE (S1) '  ' WS-TAX-RATE (S1)
           END-PERFORM

           MOVE +1                     TO S1

           .
       0600-EXIT.
           EXIT.

       0610-START-ELCNTL.

           MOVE LOW-VALUES             TO CF-CONTROL-PRIMARY
           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE '3'                    TO CF-RECORD-TYPE
           START ELCNTL KEY >= CF-CONTROL-PRIMARY

           IF ELCNTL-FILE-STATUS = '10' OR '23'
              SET END-OF-ELCNTL        TO TRUE
              DISPLAY ' INFO - ELCNTL - START ' ELCNTL-FILE-STATUS
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ELCNTL - START ' ELCNTL-FILE-STATUS
                 SET END-OF-ELCNTL     TO TRUE
              END-IF
           END-IF

           .
       0610-EXIT.
           EXIT.

       0615-BUILD-TAX.


           IF (CF-RECORD-TYPE = '3')
              AND (DTE-CLIENT = CF-COMPANY-ID)
              ADD +1                   TO S1
              IF CF-ST-LF-PREM-TAX NOT NUMERIC
                 MOVE ZEROS            TO CF-ST-LF-PREM-TAX
              END-IF
              MOVE CF-STATE-CODE       TO WS-STATE (S1)
              MOVE CF-ST-LF-PREM-TAX   TO WS-TAX-RATE   (S1)
           END-IF

           PERFORM 0620-READ-ELCNTL    THRU 0620-EXIT
           
           .
       0615-EXIT.
           EXIT.

       0620-READ-ELCNTL.

           READ ELCNTL NEXT RECORD

           IF ELCNTL-FILE-STATUS = '10' OR '23'
              SET END-OF-ELCNTL        TO TRUE
              DISPLAY ' INFO - ELCNTL - READ NEXT ' ELCNTL-FILE-STATUS
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ELCNTL - READ NEXT '
                    ELCNTL-FILE-STATUS
                 SET END-OF-ELCNTL     TO TRUE
              ELSE
                 IF (CF-COMPANY-ID NOT = DTE-CLIENT)
                    OR (CF-RECORD-TYPE > '3')
                    DISPLAY ' INFO - ELCNTL - READ NEXT ' CF-COMPANY-ID
                    '  ' CF-RECORD-TYPE
                    SET END-OF-ELCNTL TO TRUE
                 END-IF
              END-IF
           END-IF

           IF NOT END-OF-ELCNTL
              ADD 1 TO ELCNTL-RECS-IN
           END-IF

           .
       0620-EXIT.
           EXIT.

       0700-FINISH-TABLE.

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              (S1 > +70)
              IF WS-STATE (S1) NOT = LOW-VALUES
                 COMPUTE WS-NRL (S1) = WS-UEP (S1) - WS-UEC (S1)
                    - WS-UET (S1)
                 IF WS-NRL (S1) > WS-MORT-RES (S1)
                    COMPUTE WS-ADDL-MORT-RES (S1) =
                       WS-NRL (S1) - WS-MORT-RES (S1)
                 ELSE
                    MOVE ZEROS         TO WS-ADDL-MORT-RES (S1)
                 END-IF
                 COMPUTE WS-TOT-MORT-RES (S1) = WS-MORT-RES (S1) +
                    WS-ADDL-MORT-RES (S1)
                 PERFORM 0800-WRITE-SUMM
                                       THRU 0800-EXIT
              END-IF
           END-PERFORM
           DISPLAY ' FINSHED BUILDING TABLE '

           .
       0700-EXIT.
           EXIT.

       0800-WRITE-SUMM.

           MOVE WS-STATE (S1)          TO SM-STATE
           MOVE WS-TAX-RATE (S1)       TO SM-TAX-RATE
           MOVE WS-UEP (S1)            TO SM-UEP
           MOVE WS-UEC (S1)            TO SM-UEC
           MOVE WS-UET (S1)            TO SM-UET
           MOVE WS-MORT-RES (S1)       TO SM-MORT
           MOVE WS-NRL (S1)            TO SM-NRL
           MOVE WS-ADDL-MORT-RES (S1)  TO SM-ADDL-MORT
           MOVE WS-TOT-MORT-RES (S1)   TO SM-TOT-MORT
           MOVE 'E'                    TO SM-EOR

           WRITE SUMM-FILE-OUT-REC FROM SUMM-DETAIL-RECORD

           .
       0800-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
