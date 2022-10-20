       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMRTXEM.
       AUTHOR.     PABLO.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT DAT-IN           ASSIGN TO DATIN
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT RATE-IN          ASSIGN TO RATEIN
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ERRATE           ASSIGN TO ERRATE
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS RT-CONTROL-PRIMARY
                                   FILE STATUS IS ERRATE-FILE-STATUS.

           SELECT DISK-DATE      ASSIGN TO SYS019-UT-FBA1-S-SYS019.

           SELECT INS-OUT          ASSIGN TO INSOUT
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DAT-OUT          ASSIGN TO DATOUT
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  DAT-IN 
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  DAT-IN-REC                  PIC X(100).

       FD  ERRATE.

                                       COPY ERCRATE.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  RATE-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  RATE-IN-REC                 PIC X(100).

       FD  INS-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  INS-OUT-REC                 PIC X(1645).

       FD  DAT-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  DAT-OUT-REC                 PIC X(10).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '  PEMRTXEM WORKING STORAGE      '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-ERRATE                 VALUE 'Y'.
       77  WS-INPUT-SW                 PIC X.
           88  END-OF-INPUT              VALUE 'Y'.
       77  WS-FOUND-MATCH              PIC X VALUE ' '.
           88  FOUND-MATCH               VALUE 'Y'.
       77  WS-TBL-RECS                 PIC 9(9) VALUE ZEROS.
       77  WS-IN-RECS                  PIC 9(9) VALUE ZEROS.
       77  INS-RECS-OUT                PIC 9(9) VALUE ZEROS.
       77  DAT-RECS-OUT                PIC 9(9) VALUE ZEROS.
       77  S1                          PIC S9(5) VALUE +0 COMP-3.
       77  T1                          PIC S9(5) VALUE +0 COMP-3.
       77  PGM-SUB                     PIC S999    COMP    VALUE +083.
       77  IP-RECS                     PIC S9(5) COMP-3 VALUE +0.
       77  WS-STATE                    PIC XX  VALUE SPACES.


       01  T1M                         PIC S9(5) VALUE +0 COMP-3.
       01  WS-TABLE1-AREA.
           12  RATE-IN-TABLE OCCURS 500.
               16  RATE-TABLE.
                   20  RTI-STATE       PIC XX.
                   20  RTI-CLASS       PIC XX.
                   20  RTI-DEV         PIC XXX.
                   20  RTI-TRUNC-AT-AGE
                                       PIC X.
                   20  RTI-X-MOS-INT   PIC 99.
                   20  RTI-X-MOS-PMT   PIC 99.
                   20  RTI-CREATE-LF-RATE-TABLE
                                       PIC X.
                   20  RTI-RT-TYPE-SW  PIC X.
                   20  RTI-TYPE        PIC X.
                   20  RTI-BEN-CODE    PIC XX.
                   20  RTI-MAX-ELIG-AGE PIC 99.
                   20  RTI-ATT-AGE     PIC 99.
                   20  RTI-THRU-AGE1   PIC 99.
                   20  RTI-THRU-TERM1  PIC 999.
                   20  RTI-MAX-AH-BEN1 PIC 9(5).
                   20  RTI-MAX-LF-BEN1 PIC 9(7).
                   20  RTI-THRU-AGE2   PIC 99.
                   20  RTI-THRU-TERM2  PIC 999.
                   20  RTI-MAX-AH-BEN2 PIC 9(5).
                   20  RTI-MAX-LF-BEN2 PIC 9(7).
                   20  RTI-THRU-AGE3   PIC 99.
                   20  RTI-THRU-TERM3  PIC 999.
                   20  RTI-MAX-AH-BEN3 PIC 9(5).
                   20  RTI-MAX-LF-BEN3 PIC 9(7).
                   20  RTI-COV-TYPE    PIC XXX.
                   20  RTI-RATE-DESC   PIC X(25).

       01  T2M                         PIC S9(5) VALUE +0 COMP-3.
       01  WS-TABLE2-AREA.
           12  DAT-IN-TABLE OCCURS 1000.
               16  DAT-TABLE.
                   20  DAT-OPTIONS     PIC X(10)  VALUE SPACES.
                   20  DAT-OPTIONS-RATES REDEFINES DAT-OPTIONS
                                       PIC ZZ99.99999 BLANK WHEN ZERO.
                   20  DAT-OPTIONS-NUM REDEFINES DAT-OPTIONS
                                       PIC 9999999999 BLANK WHEN ZERO.

       01  WS-LF-SD-RATE               PIC 9V99999  VALUE ZEROS.
       01  WS-LF-SL-RATE               PIC 9V99999  VALUE ZEROS.
       01  WS-LF-JD-RATE               PIC 9V99999  VALUE ZEROS.
       01  WS-LF-JL-RATE               PIC 9V99999  VALUE ZEROS.

       01  WS-LF-SN-RATE               PIC 99V99999 VALUE ZEROS.
       01  WS-LF-JN-RATE               PIC 99V99999 VALUE ZEROS.

       01  WS-TRUNC-AT-AGE             PIC X   VALUE SPACES.
       01  WS-RT-TYPE-SW               PIC X   VALUE ZEROS.
       01  WS-X-MOS-INT                PIC 99  VALUE ZEROS.
       01  WS-X-MOS-PMTS               PIC 99  VALUE ZEROS.
       01  WS-CREATE-LF-TABLE          PIC X   VALUE SPACES.

       01  WS-AH-MAX-ELIG-AGE          PIC 99.
       01  WS-AH-ATT-AGE               PIC 99.
       01  WS-AH-AGE1                  PIC 99.
       01  WS-AH-TERM1                 PIC 999.
       01  WS-AH-AMT1                  PIC 9(5).
       01  WS-AH-TOT-AMT1              PIC 9(7).

       01  WS-AH-AGE2                  PIC 99.
       01  WS-AH-TERM2                 PIC 999.
       01  WS-AH-AMT2                  PIC 9(5).
       01  WS-AH-TOT-AMT2              PIC 9(7).

       01  WS-AH-AGE3                  PIC 99.
       01  WS-AH-TERM3                 PIC 999.
       01  WS-AH-AMT3                  PIC 9(5).
       01  WS-AH-TOT-AMT3              PIC 9(7).


       01  WS-RATE-SW                  PIC X  VALUE SPACES.
           88  MOB-RATE                  VALUE 'Y'.
       01  WS-LF-MAX-ELIG-AGE          PIC 99.
       01  WS-LF-ATT-AGE               PIC 99.
       01  WS-LF-AGE1                  PIC 99.
       01  WS-LF-TERM1                 PIC 999.
       01  WS-LF-AMT1                  PIC 9(7).

       01  WS-LF-AGE2                  PIC 99.
       01  WS-LF-TERM2                 PIC 999.
       01  WS-LF-AMT2                  PIC 9(7).

       01  WS-LF-AGE3                  PIC 99.
       01  WS-LF-TERM3                 PIC 999.
       01  WS-LF-AMT3                  PIC 9(7).
       01  A1                          PIC S999 COMP-3 VALUE +1.
       01  L1                          PIC S999 COMP-3 VALUE +1.
      * 1=7R, 2=7E, 3=14R, 4=14E, 5=30R, 6=30E
       01  WS-AH-RATE-TABLES.
           05  FILLER OCCURS 6.
               10  WS-AH-TABLE         PIC X(10).

       01  WS-LF-RATE-TABLES.
           05  FILLER OCCURS 4.
               10  WS-LF-TABLE         PIC X(10).

       01  RATE-OUT-EXTRACT.
           12  EX-STATE                PIC XXX.
           12  EX-BENCD-DESC           PIC X(10).
           12  EX-D1                   PIC X.
           12  EX-MAX-MOS              PIC 999.
           12  EX-D2                   PIC X.
           12  EX-MIN-MOS              PIC 999.
           12  EX-D3                   PIC X.
           12  FILLER OCCURS 180.
               16  EX-RATE             PIC 99.99999.
               16  EX-D4               PIC X.

       01  WS-MISC.
           05  WS-RETURN-CODE          PIC S9(4)   COMP.
           05  WS-ABEND-MESSAGE        PIC X(80).
           05  WS-ABEND-FILE-STATUS    PIC XX      VALUE ZEROS.
           05  WS-ZERO                 PIC S9      VALUE +0 COMP-3.
           05  RATEIN-RECS             PIC 9(5)    VALUE ZEROS.
           05  WS-SAVE-ERRATE          PIC X(1645) VALUE SPACES.
           05  ERRATE-FILE-STATUS      PIC XX      VALUE ZEROS.
           05  WS-DATE                 PIC 9(6)    VALUE ZEROS.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.
                                   
           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-RATE   THRU 0100-EXIT UNTIL
              END-OF-ERRATE

           PERFORM 0800-BUILD-DAT-OUT  THRU 0800-EXIT

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' TABLE RECORDS READ   '  WS-TBL-RECS
           DISPLAY ' RATE RECORDS READ    '  WS-IN-RECS
           DISPLAY ' INS  RECORDS WRITTEN '  INS-RECS-OUT
           GOBACK

           .
       0100-PROCESS-RATE.

           MOVE ' '                    TO WS-FOUND-MATCH

           PERFORM VARYING T1 FROM +1 BY +1 UNTIL
              (T1 > T1M)
              OR ((RT-ST-CODE = RTI-STATE (T1))
                 AND (RT-ST-CLASS = RTI-CLASS (T1))
                 AND (RT-ST-DEV = RTI-DEV (T1))
                 AND (RT-L-AH = RTI-TYPE (T1))
                 AND (RT-LAH-NUM = RTI-BEN-CODE (T1))
                 AND (RT-EXPIRY-DATE = 99999999999))
           END-PERFORM
           IF T1 <= T1M
              SET FOUND-MATCH          TO TRUE
           END-IF

           IF (FOUND-MATCH)
              IF RTI-TYPE (T1) = 'A'
                 PERFORM 0150-PROCESS-AH-RATE
                                       THRU 0150-EXIT
              ELSE
                 IF (RTI-TYPE (T1) = 'L')
                    AND (RTI-CREATE-LF-RATE-TABLE (T1)) = 'Y'
                    PERFORM 0160-PROCESS-LF-RATE
                                       THRU 0160-EXIT
                 END-IF
              END-IF
              PERFORM 0110-UPDATE-LIMS THRU 0110-EXIT
           END-IF

           PERFORM 0200-READ-RATE      THRU 0200-EXIT

           .
       0100-EXIT.
           EXIT.


       0110-UPDATE-LIMS.

           DISPLAY ' I AM AT UPDATE LIMS ' RATE-TABLE (T1)

           IF RTI-TYPE (T1) = 'L'
              MOVE RTI-TRUNC-AT-AGE (T1)
                                       TO WS-TRUNC-AT-AGE
              MOVE RTI-X-MOS-INT (T1)  TO WS-X-MOS-INT
              MOVE RTI-X-MOS-PMT (T1)  TO WS-X-MOS-PMTS
              MOVE RTI-CREATE-LF-RATE-TABLE (T1)
                                       TO WS-CREATE-LF-TABLE
              MOVE RTI-RT-TYPE-SW (T1) TO WS-RT-TYPE-SW
              MOVE RTI-MAX-ELIG-AGE (T1)
                                        TO WS-LF-MAX-ELIG-AGE
              MOVE RTI-ATT-AGE (T1)     TO WS-LF-ATT-AGE
              MOVE RTI-THRU-AGE1 (T1)   TO WS-LF-AGE1
              MOVE RTI-THRU-TERM1 (T1)  TO WS-LF-TERM1
              MOVE RTI-MAX-LF-BEN1 (T1) TO WS-LF-AMT1
              MOVE RTI-THRU-AGE2 (T1)   TO WS-LF-AGE2
              MOVE RTI-THRU-TERM2 (T1)  TO WS-LF-TERM2
              MOVE RTI-MAX-LF-BEN2 (T1) TO WS-LF-AMT2
              MOVE RTI-THRU-AGE3 (T1)   TO WS-LF-AGE3
              MOVE RTI-THRU-TERM3 (T1)  TO WS-LF-TERM3
              MOVE RTI-MAX-LF-BEN3 (T1) TO WS-LF-AMT3
              IF RTI-STATE (T1) NOT = 'VA'
                EVALUATE TRUE
                 WHEN RTI-COV-TYPE (T1) = 'SDL'
                   MOVE RT-L-RATE (12)  TO WS-LF-SD-RATE
                   DISPLAY ' FOUND SD ' WS-LF-SD-RATE
                 WHEN RTI-COV-TYPE (T1) = 'JDL'
                   MOVE RT-L-RATE (12)  TO WS-LF-JD-RATE
                   DISPLAY ' FOUND JD ' WS-LF-JD-RATE
                 WHEN RTI-COV-TYPE (T1) = 'SLL'
                   MOVE RT-L-RATE (12)  TO WS-LF-SL-RATE
                   DISPLAY ' FOUND SL ' WS-LF-SL-RATE
                 WHEN RTI-COV-TYPE (T1) = 'JLL'
                   MOVE RT-L-RATE (12)  TO WS-LF-JL-RATE
                   DISPLAY ' FOUND JL ' WS-LF-JL-RATE
                 WHEN RTI-COV-TYPE (T1) = 'SNL'
                   MOVE RT-DISCOUNT-OB-RATE
                                        TO WS-LF-SN-RATE
                   DISPLAY ' FOUND SN ' WS-LF-SN-RATE
                 WHEN RTI-COV-TYPE (T1) = 'JNL'
                   MOVE RT-DISCOUNT-OB-RATE
                                        TO WS-LF-JN-RATE
                   DISPLAY ' FOUND JN ' WS-LF-JN-RATE
                 WHEN OTHER
                   DISPLAY ' LIFE TYPE OTHER THAN SDL JDL SLL JLL '
                END-EVALUATE
              END-IF
           ELSE
              MOVE RTI-MAX-ELIG-AGE (T1) TO WS-AH-MAX-ELIG-AGE
              MOVE RTI-ATT-AGE (T1)     TO WS-AH-ATT-AGE
              MOVE RTI-THRU-AGE1 (T1)   TO WS-AH-AGE1
              MOVE RTI-THRU-TERM1 (T1)  TO WS-AH-TERM1
              MOVE RTI-MAX-AH-BEN1 (T1) TO WS-AH-AMT1
              MOVE RTI-MAX-LF-BEN1 (T1) TO WS-AH-TOT-AMT1
              MOVE RTI-THRU-AGE2 (T1)   TO WS-AH-AGE2
              MOVE RTI-THRU-TERM2 (T1)  TO WS-AH-TERM2
              MOVE RTI-MAX-AH-BEN2 (T1) TO WS-AH-AMT2
              MOVE RTI-MAX-LF-BEN1 (T1) TO WS-AH-TOT-AMT2
              MOVE RTI-THRU-AGE3 (T1)   TO WS-AH-AGE3
              MOVE RTI-THRU-TERM3 (T1)  TO WS-AH-TERM3
              MOVE RTI-MAX-AH-BEN3 (T1) TO WS-AH-AMT3
              MOVE RTI-MAX-LF-BEN1 (T1) TO WS-AH-TOT-AMT3
           END-IF

           .
       0110-EXIT.
           EXIT.

       0150-PROCESS-AH-RATE.

           MOVE WS-SAVE-ERRATE         TO RATE-OUT-EXTRACT
           MOVE RT-ST-CODE             TO EX-STATE

           EVALUATE RTI-RATE-DESC (T1)
              WHEN '7R'
                 MOVE +1               TO A1
              WHEN '7E'
                 MOVE +2               TO A1
              WHEN '14R'
                 MOVE +3               TO A1
              WHEN '14E'
                 MOVE +4               TO A1
              WHEN '30R'
                 MOVE +5               TO A1
              WHEN '30E'
                 MOVE +6               TO A1
              WHEN OTHER
                 DISPLAY 'INVALID AH RATE DESCRIPTION '
                    RTI-RATE-DESC (T1) ' DEFAULT IS 14R '
                 MOVE +3               TO A1
           END-EVALUATE

           MOVE WS-STATE               TO WS-AH-TABLE (A1) (1:2)
           MOVE RTI-RATE-DESC (T1)     TO EX-BENCD-DESC
                                          WS-AH-TABLE (A1) (3:8)

           MOVE ZEROS                  TO EX-MAX-MOS
           MOVE 1                      TO EX-MIN-MOS

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              (S1 > +180)
              IF RT-AH-RATE (S1) NOT NUMERIC
                 MOVE ZEROS            TO RT-AH-RATE (S1)
              END-IF
              IF RT-AH-RATE (S1) NOT = ZEROS
                 MOVE S1               TO EX-MAX-MOS
              END-IF
              MOVE RT-AH-RATE (S1)     TO EX-RATE (S1)
           END-PERFORM

           PERFORM 0300-WRITE-RATE  THRU 0300-EXIT

           .
       0150-EXIT.
           EXIT.

       0160-PROCESS-LF-RATE.

           MOVE WS-SAVE-ERRATE         TO RATE-OUT-EXTRACT
           MOVE RT-ST-CODE             TO EX-STATE
           MOVE RTI-RATE-DESC (T1)     TO EX-BENCD-DESC
           EVALUATE RTI-COV-TYPE (T1)
              WHEN 'SDL'
                 MOVE +1               TO L1
              WHEN 'JDL'
                 MOVE +2               TO L1
              WHEN 'SLL'
                 MOVE +3               TO L1
              WHEN 'JLL'
                 MOVE +4               TO L1
           END-EVALUATE

           MOVE RT-ST-CODE             TO WS-LF-TABLE (L1)
           MOVE RTI-RATE-DESC (T1)     TO WS-LF-TABLE (L1) (3:8)
              
           MOVE ZEROS                  TO EX-MAX-MOS
           MOVE 1                      TO EX-MIN-MOS

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              (S1 > +180)
              IF RT-L-RATE (S1) NOT NUMERIC
                 MOVE ZEROS            TO RT-L-RATE (S1)
              END-IF
              IF RT-L-RATE (S1) NOT = ZEROS
                 MOVE S1               TO EX-MAX-MOS
              END-IF
              MOVE RT-L-RATE (S1)      TO EX-RATE (S1)
           END-PERFORM

           PERFORM 0300-WRITE-RATE  THRU 0300-EXIT

           .
       0160-EXIT.
           EXIT.

       0200-READ-RATE.

           READ ERRATE NEXT RECORD

           IF ERRATE-FILE-STATUS = '10' OR '23'
              SET END-OF-ERRATE        TO TRUE
           ELSE
              IF ERRATE-FILE-STATUS NOT = '00'
                 DISPLAY 'ERRATE READ NEXT ' ERRATE-FILE-STATUS
                 SET END-OF-ERRATE     TO TRUE
              ELSE
                 IF RT-COMPANY-CD > DTE-CLASIC-COMPANY-CD
                    SET END-OF-ERRATE  TO TRUE
                 END-IF
              END-IF
           END-IF

           IF NOT END-OF-ERRATE
              ADD 1                    TO WS-IN-RECS
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-RATE.

           WRITE INS-OUT-REC           FROM RATE-OUT-EXTRACT
           ADD 1                       TO INS-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT ERRATE RATE-IN DAT-IN
               OUTPUT INS-OUT DAT-OUT

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ERRATE INS-OUT RATE-IN DAT-IN DAT-OUT

           .
       0500-EXIT.
           EXIT.

       0550-START-RATE.

           MOVE LOW-VALUES             TO RT-CONTROL-PRIMARY

           MOVE DTE-CLASIC-COMPANY-CD  TO RT-COMPANY-CD
           START ERRATE KEY >= RT-CONTROL-PRIMARY

           IF ERRATE-FILE-STATUS = '10' OR '23'
              SET END-OF-ERRATE TO TRUE
           ELSE
              IF ERRATE-FILE-STATUS NOT = '00'
                 DISPLAY 'ERRATE START     ' ERRATE-FILE-STATUS
                 SET END-OF-ERRATE TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.

       0600-INITIALIZE.

           PERFORM VARYING A1 FROM +1 BY +1 UNTIL
              A1 > +6
              MOVE 'AH0000'            TO WS-AH-TABLE (A1)
           END-PERFORM

           MOVE 'SD0000'               TO WS-LF-TABLE (1)
           MOVE 'JD0000'               TO WS-LF-TABLE (2)
           MOVE 'SL0000'               TO WS-LF-TABLE (3)
           MOVE 'JL0000'               TO WS-LF-TABLE (4)

           MOVE SPACES                 TO RATE-OUT-EXTRACT
           MOVE ';'                    TO EX-D1
                                          EX-D2
                                          EX-D3
           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              (S1 > +180)
              MOVE ZEROS               TO EX-RATE (S1)
              MOVE ';'                 TO EX-D4   (S1)
           END-PERFORM

           MOVE RATE-OUT-EXTRACT       TO WS-SAVE-ERRATE

           PERFORM 0700-BUILD-TABLES   THRU 0700-EXIT

           PERFORM 0550-START-RATE     THRU 0550-EXIT
           PERFORM 0200-READ-RATE      THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

       0700-BUILD-TABLES.

           MOVE ' '                    TO WS-INPUT-SW
           
           PERFORM VARYING T1 FROM +1 BY +1 UNTIL
              END-OF-INPUT
              READ RATE-IN AT END
                 SET END-OF-INPUT      TO TRUE
              END-READ
              IF NOT END-OF-INPUT
                 MOVE RATE-IN-REC      TO RATE-TABLE (T1)
              END-IF
           END-PERFORM

           SUBTRACT +2                 FROM T1
           MOVE T1                     TO T1M
           DISPLAY ' NUMBER OF RATE DETAIL RECORDS ' T1M
           MOVE RTI-STATE (T1)         TO WS-STATE

           MOVE ' '                    TO WS-INPUT-SW

           PERFORM VARYING T1 FROM +1 BY +1 UNTIL
              END-OF-INPUT
              READ DAT-IN AT END
                 SET END-OF-INPUT      TO TRUE
              END-READ
              IF NOT END-OF-INPUT
                 READ DAT-IN AT END
                    SET END-OF-INPUT   TO TRUE
                 END-READ
              END-IF
              IF NOT END-OF-INPUT
                 MOVE DAT-IN-REC       TO DAT-OPTIONS (T1)
              END-IF
           END-PERFORM

           SUBTRACT +2                 FROM T1
           MOVE T1                     TO T2M
           DISPLAY ' NUMBER OF DAT RECORDS         ' T2M

           .
       0700-EXIT.
           EXIT.

       0800-BUILD-DAT-OUT.

           IF WS-TRUNC-AT-AGE NOT NUMERIC
              MOVE ZEROS               TO WS-TRUNC-AT-AGE
           END-IF
           MOVE WS-TRUNC-AT-AGE        TO DAT-OPTIONS (272)
           IF WS-X-MOS-PMTS NOT NUMERIC
              MOVE ZEROS               TO WS-X-MOS-PMTS
           END-IF
           IF WS-X-MOS-PMTS > ZEROS
              MOVE WS-X-MOS-PMTS       TO DAT-OPTIONS (83)
           END-IF

           IF WS-X-MOS-INT NOT NUMERIC
              MOVE ZEROS               TO WS-X-MOS-INT
           END-IF
           IF WS-X-MOS-INT > ZEROS
              MOVE WS-X-MOS-INT        TO DAT-OPTIONS (84)
           END-IF
           IF WS-RT-TYPE-SW = 'Y'
              MOVE '1'                 TO DAT-OPTIONS (403)
           ELSE
              MOVE '0'                 TO DAT-OPTIONS (403)
           END-IF

           IF WS-CREATE-LF-TABLE = 'Y'
              MOVE '1'                 TO DAT-OPTIONS (80)
           END-IF
           
           IF WS-LF-SN-RATE > ZEROS
              OR WS-LF-JN-RATE > ZEROS
              MOVE '3'                 TO DAT-OPTIONS (80)
              MOVE '1'                 TO DAT-OPTIONS (81)
              MOVE '6'                 TO DAT-OPTIONS (82)
           END-IF

           MOVE WS-LF-SD-RATE          TO DAT-OPTIONS-RATES (91)
           MOVE WS-LF-SL-RATE          TO DAT-OPTIONS-RATES (92)
           MOVE WS-LF-JD-RATE          TO DAT-OPTIONS-RATES (93)
           MOVE WS-LF-JL-RATE          TO DAT-OPTIONS-RATES (94)
           
           IF WS-LF-SN-RATE NOT = ZEROS
              MOVE WS-LF-SN-RATE       TO DAT-OPTIONS-RATES (95)
           END-IF

           IF WS-LF-JN-RATE NOT = ZEROS
              MOVE WS-LF-JN-RATE       TO DAT-OPTIONS-RATES (96)
           END-IF

           IF DAT-OPTIONS (91) = SPACES
              MOVE '0'                 TO DAT-OPTIONS (91)
           END-IF
           IF DAT-OPTIONS (92) = SPACES
              MOVE '0'                 TO DAT-OPTIONS (92)
           END-IF
           IF DAT-OPTIONS (93) = SPACES
              MOVE '0'                 TO DAT-OPTIONS (93)
           END-IF
           IF DAT-OPTIONS (94) = SPACES
              MOVE '0'                 TO DAT-OPTIONS (94)
           END-IF

           MOVE WS-LF-MAX-ELIG-AGE     TO DAT-OPTIONS (230)
           MOVE WS-LF-AMT1             TO DAT-OPTIONS (100)
           MOVE WS-LF-AGE1             TO DAT-OPTIONS (281)
           MOVE WS-LF-TERM1            TO DAT-OPTIONS (102)

           MOVE WS-AH-MAX-ELIG-AGE     TO DAT-OPTIONS (231)
           MOVE WS-AH-TOT-AMT1         TO DAT-OPTIONS (104)
           MOVE WS-AH-AMT1             TO DAT-OPTIONS (106)
           MOVE WS-AH-TERM1            TO DAT-OPTIONS (108)

           MOVE WS-LF-ATT-AGE          TO DAT-OPTIONS (189)
           MOVE WS-AH-ATT-AGE          TO DAT-OPTIONS (190)
           
           MOVE WS-LF-AMT2             TO DAT-OPTIONS (282)
           MOVE WS-LF-TERM2            TO DAT-OPTIONS (325)

           MOVE WS-AH-AMT2             TO DAT-OPTIONS (322)
           MOVE WS-AH-TERM2            TO DAT-OPTIONS (331)

           
           
           MOVE WS-AH-TABLE (1)        TO DAT-OPTIONS (132)
           MOVE WS-AH-TABLE (2)        TO DAT-OPTIONS (133)
           MOVE WS-AH-TABLE (3)        TO DAT-OPTIONS (134)
           MOVE WS-AH-TABLE (4)        TO DAT-OPTIONS (135)
           MOVE WS-AH-TABLE (5)        TO DAT-OPTIONS (136)
           MOVE WS-AH-TABLE (6)        TO DAT-OPTIONS (137)

           IF WS-LF-TABLE (1) NOT = SPACES
              MOVE WS-LF-TABLE (1)     TO DAT-OPTIONS (128)
           END-IF
           IF WS-LF-TABLE (2) NOT = SPACES
              MOVE WS-LF-TABLE (2)     TO DAT-OPTIONS (129)
           END-IF
           IF WS-LF-TABLE (3) NOT = SPACES
              MOVE WS-LF-TABLE (3)     TO DAT-OPTIONS (130)
           END-IF
           IF WS-LF-TABLE (4) NOT = SPACES
              MOVE WS-LF-TABLE (4)     TO DAT-OPTIONS (131)
           END-IF

           DISPLAY ' WS STATE IS ' WS-STATE

           IF WS-STATE = 'IN' OR 'TN' OR 'MD'
              MOVE 25000               TO DAT-OPTIONS-NUM (409)
           END-IF

           PERFORM VARYING T1 FROM +1 BY +1 UNTIL
              T1 > T2M
              PERFORM 0850-WRITE-DAT-OUT
                                       THRU 0850-EXIT
           END-PERFORM
              
           .
       0800-EXIT.
           EXIT.

       0850-WRITE-DAT-OUT.

           WRITE DAT-OUT-REC           FROM DAT-OPTIONS (T1)
           ADD 1                       TO DAT-RECS-OUT

           .
       0850-EXIT.
           EXIT.

       ABEND-PGM.
                           COPY ELCABEND SUPPRESS.
