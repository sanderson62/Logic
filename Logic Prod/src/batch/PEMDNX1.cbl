       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMDNX1.
       AUTHOR.     PABLO.
       DATE-COMPILED.
      *REMARKS.

      *             TTTTTTT     BBBBBBB     DDDDDD
      *                T        B      B    D     D
      *                T        B      B    D     D
      *                T        BBBBBBBB    D     D
      *                T        B      B    D     D
      *                T        B      B    D     D
      *                T        BBBBBBB     DDDDDD

      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 030909 CR2008100900001   PEMA  NEW PROGRAM
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELDENY           ASSIGN TO ELDENY
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS DN-CONTROL-BY-TYPE
                                   FILE STATUS IS ELDENY-FILE-STATUS.

           SELECT EXTR-OUT         ASSIGN TO SYS011
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE        ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  ELDENY.

                                       COPY ELCDENY.

       FD  EXTR-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  EXTR-OUT-REC                PIC X(87).


       FD  DISK-DATE
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMDNX1  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-ELDENY             VALUE 'Y'.
       77  DNY-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  EXT-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.

       01  WS-SAVE-EXTR                PIC X(87) VALUE SPACES.
       01  EXTR-DETAIL-RECORD.
           12  EX-DEN-TYPE             PIC X.
           12  EX-TAB1                 PIC X.
           12  EX-DEN-CODE             PIC XXXX.
           12  EX-TAB2                 PIC X.
           12  EX-DEN-DESC             PIC X(50).
           12  EX-TAB3                 PIC X.
           12  EX-EOR                  PIC X.

      ******************************************************************
       01  WS-MISC.
           05  ELDENY-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  WS-DATE                 PIC 9(11)  VALUE ZEROS.
           05  PGM-SUB          COMP-3 PIC S9(04) VALUE +585.
           05  WS-RETURN-CODE   COMP   PIC S9(03) VALUE +0.
           05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.
           05  WS-ZERO          COMP-3 PIC S9(01) VALUE +0.
           05  WS-ABEND-FILE-STATUS    PIC X(02)  VALUE ZERO.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

                                       COPY ELCDATE.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT

           PERFORM 0020-INITIALIZE     THRU 0020-EXIT

           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT UNTIL
                 (END-OF-ELDENY)
PEMTST*          OR (CLM-RECS-IN > 1000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' ELDENY RECORDS READ   '  DNY-RECS-IN
           DISPLAY '  EXTR RECORDS WRITTEN '  EXT-RECS-OUT
           GOBACK

           .
       0010-OPEN-FILES.

           OPEN INPUT ELDENY
              OUTPUT EXTR-OUT

           .
       0010-EXIT.
           EXIT.

       0020-INITIALIZE.

           MOVE SPACES                 TO EXTR-DETAIL-RECORD
           MOVE ';'                    TO EX-TAB1
                                          EX-TAB2
                                          EX-TAB3
           MOVE 'E'                    TO EX-EOR

           MOVE EXTR-DETAIL-RECORD   TO WS-SAVE-EXTR
           PERFORM 0550-START-ELDENY   THRU 0550-EXIT
           PERFORM 0200-READ-ELDENY    THRU 0200-EXIT

           .
       0020-EXIT.
           EXIT.

       0050-PROCESS-INPUT.

           PERFORM 0100-PROCESS-ELDENY THRU 0100-EXIT

           PERFORM 0200-READ-ELDENY    THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.
       0100-PROCESS-ELDENY.

           MOVE WS-SAVE-EXTR           TO EXTR-DETAIL-RECORD

           MOVE DN-RECORD-TYPE         TO EX-DEN-TYPE
           MOVE DN-DENIAL-CODE-A1      TO EX-DEN-CODE
           MOVE DN-DESCRIPTION         TO EX-DEN-DESC
           PERFORM 0300-WRITE-EXTR     THRU 0300-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-ELDENY.

           READ ELDENY NEXT RECORD

           IF (ELDENY-FILE-STATUS = '10' OR '23')
              OR (DN-COMPANY-CD-A1 NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-ELDENY        TO TRUE
           ELSE
              IF ELDENY-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ELDENY - READ ' ELDENY-FILE-STATUS
                 SET END-OF-ELDENY     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-ELDENY
              ADD 1                    TO DNY-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-EXTR.

           WRITE EXTR-OUT-REC          FROM EXTR-DETAIL-RECORD
           ADD 1                       TO EXT-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ELDENY EXTR-OUT

           .
       0500-EXIT.
           EXIT.

       0550-START-ELDENY.

           MOVE LOW-VALUES             TO DN-CONTROL-BY-TYPE
           MOVE DTE-CLASIC-COMPANY-CD  TO DN-COMPANY-CD-A1

           START ELDENY KEY >= DN-CONTROL-BY-TYPE

           IF ELDENY-FILE-STATUS = '10' OR '23'
              SET END-OF-ELDENY        TO TRUE
           ELSE
              IF ELDENY-FILE-STATUS NOT = '00'
                 DISPLAY 'ELDENY START     ' ELDENY-FILE-STATUS
                 SET END-OF-ELDENY     TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM. COPY ELCABEND.
