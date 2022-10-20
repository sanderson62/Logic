       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMBLB1.
      *AUTHOR.     PABLO.
      *REMARKS.
      * THIS PROGRAM READS THE ERCOMP FILE AND CHECKS FOR
      * A STMT OWNER THEN READS THE ERCOBI FILE AND POPULATES
      * THE ACCOUNT NAME IN ERCOBI FROM ERCOMP ACCOUNT NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT ERCOMP           ASSIGN TO ERCOMP
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CO-CONTROL-PRIMARY
                                   FILE STATUS IS ERCOMP-FILE-STATUS.
           
           SELECT ERCOBI           ASSIGN TO ERCOBI
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS BL-CONTROL-PRIMARY
                                   FILE STATUS IS ERCOBI-FILE-STATUS.

       DATA DIVISION.

       FILE SECTION.

       FD  DISK-DATE                   COPY ELCDTEFD.

       FD  ERCOMP.
                                       COPY ERCCOMP.

       FD  ERCOBI.
                                       COPY ERCCOBI.



       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '         WORKING-STORAGE        '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  SAVE-CERT                   PIC X(11) VALUE SPACES.
       77  SAVE-EFF-DT                 PIC X(6)  VALUE SPACES.
       77  SAVE-STATE                  PIC XX    VALUE SPACES.
       77  PGM-SUB                     PIC S999  COMP   VALUE +511.    
       77  WS-ABEND-FILE-STATUS        PIC XX            VALUE ZERO.    
       77  WS-ABEND-MESSAGE            PIC X(80)         VALUE SPACES.  
       77  WS-ABEND-PROGRAM            PIC X(8)          VALUE SPACES.  
       77  WS-RETURN-CODE              PIC S9(4)         VALUE +0.      
       77  WS-ZERO                     PIC S9     COMP-3 VALUE +0.      
       77  IN-CNT                 PIC 9999999   VALUE ZEROS.
       77  CERT-CNT               PIC 9999      VALUE ZEROS.
       77  S1                     PIC S999      VALUE +0 COMP-3.
       77  WS-WORK-SEQ                 PIC S9(7) COMP-3 VALUE +0.
       77  ERCOMP-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ERCOBI-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  AGT-SUB                     PIC S999 COMP-3 VALUE +0.
       77  WS-CURRENT-BIN-DATE         PIC XX VALUE LOW-VALUES.
       77  WS-BIN-COMPARE-DT           PIC XX VALUE LOW-VALUES.
       77  WS-COMPARE-TYPE             PIC X  VALUE SPACES.
       77  WS-COMPARE-STATUS           PIC X  VALUE SPACES.
       01  W-MISC.
           05  WORK-DATE-X.
               10  WORK-MO-X       PIC XX.
               10  WORK-DA-X       PIC XX.
               10  WORK-YR-X       PIC XX.
           05  WORK-DATE-N    REDEFINES   WORK-DATE-X.
               10  WORK-MO-N       PIC 99.
               10  WORK-DA-N       PIC 99.
               10  WORK-YR-N       PIC 99.
           05  WS-WORK-DT             PIC 9(11).
           05  WS-WORK-DT-A REDEFINES WS-WORK-DT.
               10  FILLER             PIC XXX.
               10  V-ISS-CCYR         PIC X(4).
               10  V-ISS-MO           PIC XX.
               10  V-ISS-DA           PIC XX.
           05  WS-WORK-DATE          PIC 9(11).
           05  FILLER REDEFINES WS-WORK-DATE.
               10  FILLER            PIC X(5).
               10  WS-WORK-DATE-YR   PIC XX.
               10  WS-WORK-DATE-MO   PIC XX.
               10  WS-WORK-DATE-DA   PIC XX.
           05  CERT-KEY.
               10  CERT-STATE         PIC XX.
               10  CERT-DATE          PIC 9(11).
               10  CERT-CERT          PIC X(11).
           05  WS-DATE-ALPH.
               10  FILLER             PIC XXX VALUE '000'.
               10  WS-WORK-CENT       PIC XX.
               10  WS-WORK-YR         PIC XX.
               10  WS-WORK-MO         PIC XX.
               10  WS-WORK-DA         PIC XX.
           05  WS-DATE-NUM REDEFINES WS-DATE-ALPH
                                      PIC 9(11).

           05  WS-BIN-EFF             PIC XX VALUE LOW-VALUES.
           05  FILE-IN-CNT            PIC 9(7) VALUE ZEROS.
           05  FILE-OUT-CNT           PIC 9(7) VALUE ZEROS.
           05  FILE-FIX-CNT           PIC 9(7) VALUE ZEROS.
           05  WS-INPUT-SW            PIC X VALUE ' '.
               88  END-OF-INPUT             VALUE 'Y'.
                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

                                       COPY ELCDATE.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0040-OPEN-FILES     THRU 0040-EXIT
           PERFORM 0050-INIT           THRU 0050-EXIT

           PERFORM 0020-PROCESS        THRU 0020-EXIT UNTIL
              (END-OF-INPUT)
      *       OR (IN-CNT > 10)
           PERFORM 0060-CLOSE-FILES    THRU 0060-EXIT

           DISPLAY ' FILE IN CNT          ' FILE-IN-CNT
           DISPLAY ' FILE OUT CNT         ' FILE-OUT-CNT
           DISPLAY ' FILE FIX CNT         ' FILE-FIX-CNT

           GOBACK

           .
       0014-START-INPUT.

           MOVE LOW-VALUES             TO CO-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO CO-COMPANY-CD
           START ERCOMP KEY >= CO-CONTROL-PRIMARY

           IF ERCOMP-FILE-STATUS = '10' OR '23'
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERCOMP-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERCOMP - START '
                    ERCOMP-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0014-EXIT.
           EXIT.

       0017-READ-INPUT.

           READ ERCOMP NEXT RECORD

           IF ERCOMP-FILE-STATUS = '10' OR '23'
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERCOMP-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERCOMP - READNEXT '
                    ERCOMP-FILE-STATUS ' ' CO-CONTROL
                 PERFORM ABEND-PGM
              ELSE
                 ADD 1                 TO FILE-IN-CNT
              END-IF
           END-IF

           .
       0017-EXIT.
           EXIT.

       0020-PROCESS.

           IF (CO-STMT-OWNER NOT = SPACES)
              AND (CO-REPORT-GROUP-ID NOT = SPACES)
              PERFORM 0070-BUILD-ERCOBI   THRU 0070-EXIT
           END-IF

           PERFORM 0017-READ-INPUT     THRU 0017-EXIT

           .
       0020-EXIT.
            EXIT.


       0040-OPEN-FILES.
       
           OPEN INPUT ERCOMP
PEMTST*               ERCOBI
PEMTST     OPEN I-O ERCOBI

           IF ERCOMP-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERCOMP OPEN ' ERCOMP-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERCOBI-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERCOBI OPEN ' ERCOBI-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0040-EXIT.
           EXIT.

       0050-INIT.
       
           DISPLAY ' ACCEPT DATE ' WS-ACCEPT-DATE

           MOVE WS-ACCEPT-DATE         TO DC-GREG-DATE-1-YMD
           MOVE '3'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-CURRENT-BIN-DATE
           ELSE
              DISPLAY ' ERROR CONVERTING CURRENT DATE '
           END-IF

           PERFORM 0014-START-INPUT    THRU 0014-EXIT
           PERFORM 0017-READ-INPUT     THRU 0017-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-CLOSE-FILES.
       
           CLOSE ERCOMP
                 ERCOBI

           IF ERCOMP-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERCOMP CLOSE ' ERCOMP-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERCOBI-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERCOBI CLOSE ' ERCOBI-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0060-EXIT.
           EXIT.

       0070-BUILD-ERCOBI.

           MOVE CO-COMPANY-CD          TO BL-COMPANY-CD
           MOVE CO-STMT-OWNER          TO BL-STMT-OWNER
           MOVE CO-REPORT-GROUP-ID     TO BL-REPORT-GROUP-ID

           READ ERCOBI

           IF ERCOBI-FILE-STATUS = '00'
              MOVE CO-ACCT-NAME        TO BL-ACCOUNT-NAME
              PERFORM 0073-REWRITE-ERCOBI
                                       THRU 0073-EXIT
           ELSE
              IF ERCOBI-FILE-STATUS = '23' OR '10'
                 MOVE SPACES           TO COMP-BILLING-INSTRUCTIONS
                 MOVE CO-COMPANY-CD    TO BL-COMPANY-CD
                 MOVE CO-STMT-OWNER    TO BL-STMT-OWNER
                 MOVE CO-REPORT-GROUP-ID TO BL-REPORT-GROUP-ID
                 MOVE CO-ACCT-NAME     TO BL-ACCOUNT-NAME
                 PERFORM 0074-WRITE-ERCOBI
                                       THRU 0074-EXIT
              ELSE
                 DISPLAY ' ERROR - ERCOBI - READ ' CO-STMT-OWNER ' '
                    CO-REPORT-GROUP-ID ' ' ERCOBI-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0070-EXIT.
            EXIT.

       0073-REWRITE-ERCOBI.

           DISPLAY ' ABOUT TO UPDATE NAME ' BL-STMT-OWNER ' '
                 BL-REPORT-GROUP-ID ' ' BL-ACCOUNT-NAME

PEMTST*    MOVE '00'                   TO ERCOBI-FILE-STATUS
PEMTST     REWRITE COMP-BILLING-INSTRUCTIONS

           IF ERCOBI-FILE-STATUS = '00'
              ADD 1                    TO FILE-FIX-CNT
           ELSE
              DISPLAY ' ERROR - ERCOBI - REWRITE ' CO-STMT-OWNER ' '
                 CO-REPORT-GROUP-ID ' ' ERCOBI-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0073-EXIT.
           EXIT.
                      
       0074-WRITE-ERCOBI.
       
           DISPLAY ' ABOUT TO ADD    NAME ' BL-STMT-OWNER ' '
                 BL-REPORT-GROUP-ID ' ' BL-ACCOUNT-NAME

PEMTST*    MOVE '00'                   TO ERCOBI-FILE-STATUS
PEMTST     WRITE COMP-BILLING-INSTRUCTIONS

           IF ERCOBI-FILE-STATUS = '00'
              ADD 1                    TO FILE-OUT-CNT
           ELSE
              DISPLAY ' ERROR - ERCOBI - WRITE ' CO-STMT-OWNER ' '
                 CO-REPORT-GROUP-ID ' ' ERCOBI-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0074-EXIT.
           EXIT.

       8500-DATE-CONVERT.

           CALL 'ELDATCX'              USING DATE-CONVERSION-DATA.

       8500-EXIT.
           EXIT.
           EJECT

       ABEND-PGM.   COPY ELCABEND.

