       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDCTX1.
       AUTHOR.     PABLO.
       DATE-COMPILED.
122007******************************************************************
122007*                   C H A N G E   L O G
122007*
122007* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122007*-----------------------------------------------------------------
122007*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122007* EFFECTIVE    NUMBER
122007*-----------------------------------------------------------------
122007* 122007 CR2007110800002   PEMA  NEW PROGRAM TO RUN DAILY
122007******************************************************************
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

       01  EXTR-OUT-REC                PIC X(23).

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDCTX1 WORKING STORAGE      '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-INPUT              VALUE 'Y'.
       77  WS-RECS-IN                  PIC 9(9) VALUE ZEROS.
       77  WS-RECS-OUT                 PIC 9(9) VALUE ZEROS.
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

       01  EXTR-REC-INIT               PIC X(23).
       01  EXTR-REC.
           12  EX-TABLE                PIC XXX.
           12  EX-SC1                  PIC X.
           12  EX-LF-IND               PIC X.
           12  EX-SC2                  PIC X.
           12  EX-LF-MAX-COMM          PIC .99999.
           12  EX-SC3                  PIC X.
           12  EX-AH-IND               PIC X.
           12  EX-SC4                  PIC X.
           12  EX-AH-MAX-COMM          PIC .99999.
           12  EX-SC5                  PIC X.
           12  EX-EOR                  PIC X.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT UNTIL
              END-OF-INPUT

           PERFORM 0100-PROCESS-INPUT  THRU 0100-EXIT

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' RECORDS READ         '  WS-RECS-IN
           DISPLAY ' RECORDS WRITTEN      '  WS-RECS-OUT
           GOBACK

           .
       0050-PROCESS-INPUT.

           IF CT-TABLE NOT = WS-PREV-TABLE
              PERFORM 0100-PROCESS-INPUT
                                       THRU 0100-EXIT
              MOVE CT-TABLE            TO WS-PREV-TABLE
              MOVE ZEROS               TO WS-LF-COMM
                                          WS-AH-COMM
           ELSE
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 S1 > +27
                 IF CT-RT (S1) NOT NUMERIC
                    MOVE ZEROS         TO CT-RT (S1)
                 END-IF
                 IF CT-BEN-TYPE = 'L'
                    IF CT-RT (S1) > WS-LF-COMM
                       MOVE CT-RT (S1) TO WS-LF-COMM
                    END-IF
                 ELSE
                    IF CT-RT (S1) > WS-AH-COMM
                       MOVE CT-RT (S1) TO WS-AH-COMM
                    END-IF
                 END-IF
              END-PERFORM
           END-IF
           
           PERFORM 0200-READ-INPUT     THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-INPUT.

           MOVE EXTR-REC-INIT          TO EXTR-REC

           MOVE WS-PREV-TABLE          TO EX-TABLE
           MOVE 'L'                    TO EX-LF-IND
           MOVE WS-LF-COMM             TO EX-LF-MAX-COMM
           MOVE 'A'                    TO EX-AH-IND
           MOVE WS-AH-COMM             TO EX-AH-MAX-COMM
           
           PERFORM 0300-WRITE-EXTR     THRU 0300-EXIT

           .
       0100-EXIT.
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

       0400-OPEN-FILES.

           OPEN INPUT ERCTBL
               OUTPUT EXTR-OUT

           .
       0400-EXIT.
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

       0600-INITIALIZE.

           INITIALIZE EXTR-REC
           MOVE ';'                    TO EX-SC1
                                          EX-SC2
                                          EX-SC3
                                          EX-SC4
                                          EX-SC5
           MOVE 'E'                    TO EX-EOR

           MOVE EXTR-REC               TO EXTR-REC-INIT

           PERFORM 0550-START-INPUT    THRU 0550-EXIT
           PERFORM 0200-READ-INPUT     THRU 0200-EXIT

           MOVE CT-TABLE               TO WS-PREV-TABLE

           .
       0600-EXIT.
           EXIT.

       ABEND-PGM.
                                   COPY ELCABEND.
