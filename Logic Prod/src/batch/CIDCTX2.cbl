       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDCTX2.
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
      * 020910 CR2009062900001   PEMA  NEW PROGRAM TO RUN DAILY
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERCTBL           ASSIGN TO ERCTBL
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CT-CONTROL-PRIMARY
                                   FILE STATUS IS ERCTBL-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT EXTR-OUT         ASSIGN TO CTBLOUT
                 ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  ERCTBL.

                                       COPY ERCCTBL.
       FD  EXTR-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  EXTR-OUT-REC                PIC X(400).

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDCTX2 WORKING STORAGE      '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-INPUT              VALUE 'Y'.
       77  WS-RECS-IN                  PIC 9(9) VALUE ZEROS.
       77  WS-RECS-OUT                 PIC 9(9) VALUE ZEROS.
       77  T1                          PIC S999  VALUE +0 COMP-3.
       77  T2                          PIC S999  VALUE +0 COMP-3.
       77  T3                          PIC S999  VALUE +0 COMP-3.
       77  S1                          PIC S9(5) VALUE +0 COMP-3.
       77  PGM-SUB                     PIC S999    COMP    VALUE +506.      

       01  WS-PREV-TABLE               PIC XXX  VALUE LOW-VALUES.
       01  WS-LF-COMM                  PIC SV9(5) COMP-3 VALUE +0.
       01  WS-AH-COMM                  PIC SV9(5) COMP-3 VALUE +0.
       01  WS-MISC.
           12  WS-RETURN-CODE          PIC S9(4)   COMP   VALUE +0.
           12  WS-ABEND-MESSAGE        PIC X(80)          VALUE SPACES.
           12  WS-ABEND-FILE-STATUS    PIC XX             VALUE ZEROS.
           12  WS-ZERO                 PIC S9      COMP-3 VALUE +0.
           12  WS-ABEND-CODE           PIC 9(4).
           12  ABEND-CODE  REDEFINES  WS-ABEND-CODE.
               16  ABEND-CODE-1        PIC XX.
               16  ABEND-CODE-2.
                   20  AC2-ONE         PIC X.
                   20  AC2-TWO         PIC X.
           12  ERCTBL-FILE-STATUS      PIC XX    VALUE ZEROS.
           12  WS-DATE                 PIC 9(11) VALUE ZEROS.

       01  EXTR-REC-INIT               PIC X(400).
       01  EXTR-REC.
           05  EX-TABLE                PIC XXX.
           05  EX-SC1                  PIC X.
           05  EX-LF-AH                PIC X.
           05  EX-SC2                  PIC X.
           05  EX-BEN-CODE             PIC XX.
           05  EX-SC3                  PIC X.
           05  FILLER OCCURS 3.
               10  EX-BENEFIT          PIC 9999999.99.
               10  EX-SC4              PIC X.
               10  FILLER OCCURS 3.
                   15  EX-AGE          PIC 99.
                   15  EX-SC5          PIC X.
                   15  FILLER OCCURS 3.
                       20  EX-TERM     PIC 999.
                       20  EX-SC6      PIC X.
                       20  EX-COMM-PCT PIC .99999.
                       20  EX-SC7      PIC X.
           05  EX-EOR                  PIC X.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT

           PERFORM 0020-INITIALIZE     THRU 0020-EXIT

           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT UNTIL
              END-OF-INPUT

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' RECORDS READ         '  WS-RECS-IN
           DISPLAY ' RECORDS WRITTEN      '  WS-RECS-OUT
           GOBACK

           .
       0010-OPEN-FILES.

           OPEN INPUT ERCTBL
               OUTPUT EXTR-OUT

           .
       0010-EXIT.
           EXIT.

       0020-INITIALIZE.

           INITIALIZE EXTR-REC
           MOVE ';'                    TO EX-SC1
                                          EX-SC2
                                          EX-SC3
           MOVE 'E'                    TO EX-EOR

           MOVE +1 TO T1 T2 T3 S1

           PERFORM UNTIL T1 > +3
              MOVE ';'                 TO EX-SC4 (T1)
              MOVE ZEROS               TO EX-BENEFIT (T1)
              PERFORM UNTIL T2 > +3
                 MOVE ';'              TO EX-SC5 (T1 T2)
                 MOVE ZEROS            TO EX-AGE (T1 T2)
                 PERFORM UNTIL T3 > +3
                    MOVE ';'           TO EX-SC6 (T1 T2 T3)
                                          EX-SC7 (T1 T2 T3)
                    MOVE ZEROS         TO EX-TERM (T1 T2 T3)
                                          EX-COMM-PCT (T1 T2 T3)
                    ADD +1 TO T3
                 END-PERFORM
                 MOVE +1 TO T3
                 ADD +1 TO T2
              END-PERFORM
              MOVE +1 TO T2
              ADD +1 TO T1
           END-PERFORM

           MOVE EXTR-REC               TO EXTR-REC-INIT

           PERFORM 0550-START-INPUT    THRU 0550-EXIT
           PERFORM 0200-READ-INPUT     THRU 0200-EXIT

           MOVE CT-TABLE               TO WS-PREV-TABLE

           .
       0020-EXIT.
           EXIT.

       0050-PROCESS-INPUT.

           MOVE +1                     TO S1 T1 T2 T3
           MOVE EXTR-REC-INIT          TO EXTR-REC
           MOVE CT-TABLE               TO EX-TABLE
           MOVE CT-BEN-TYPE            TO EX-LF-AH
           MOVE CT-BEN-CODE            TO EX-BEN-CODE
           

           PERFORM UNTIL T1 > +3
              MOVE CT-TBF (T1)         TO EX-BENEFIT (T1)
              PERFORM UNTIL T2 > +3
                 MOVE CT-AGE (T2)      TO EX-AGE (T1 T2)
                 PERFORM UNTIL T3 > +3
                    IF S1 > +27
                       DISPLAY 'WE HAVE A PROBLEM ' T1 ' ' T2
                          ' ' T3 ' ' S1
                       PERFORM ABEND-PGM
                    END-IF
                    IF CT-RT (S1) NOT NUMERIC
                       MOVE ZEROS TO CT-RT (S1)
                    END-IF
                    MOVE CT-TRM (T3)   TO EX-TERM (T1 T2 T3)
                    MOVE CT-RT (S1)    TO EX-COMM-PCT (T1 T2 T3)
                    ADD +1 TO S1
                    ADD +1 TO T3
                 END-PERFORM
                 MOVE +1 TO T3
                 ADD +1 TO T2
              END-PERFORM
              MOVE +1 TO T2
              ADD +1 TO T1
           END-PERFORM

           PERFORM 0300-WRITE-EXTR     THRU 0300-EXIT

           PERFORM 0200-READ-INPUT     THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.

       0200-READ-INPUT.

           READ ERCTBL NEXT RECORD

           IF (ERCTBL-FILE-STATUS = '10' OR '23')
              OR (CT-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERCTBL-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERCTBL - READ ' ERCTBL-FILE-STATUS
                 SET END-OF-INPUT      TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-INPUT
              ADD +1                   TO WS-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-EXTR.

           WRITE EXTR-OUT-REC          FROM EXTR-REC

           ADD +1                      TO WS-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ERCTBL EXTR-OUT

           .
       0500-EXIT.
           EXIT.

       0550-START-INPUT.

           MOVE LOW-VALUES             TO CT-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO CT-COMPANY-CD

           START ERCTBL KEY >= CT-CONTROL-PRIMARY

           IF (ERCTBL-FILE-STATUS = '10' OR '23')
              OR (CT-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERCTBL-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERCTBL - START ' ERCTBL-FILE-STATUS
                 SET END-OF-INPUT      TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.

       ABEND-PGM.
                                   COPY ELCABEND.
