       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMBLF1.
      *AUTHOR.     PABLO.
      *REMARKS.
      * THIS PROGRAM READS THE ERCOBI FILE AND CHANGES THE 
      * STMT OWNER 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT DISK-DATE        ASSIGN TO SYS019.
           
           SELECT ERCOBI           ASSIGN TO ERCOBI
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS BL-CONTROL-PRIMARY
                                   FILE STATUS IS ERCOBI-FILE-STATUS.

       DATA DIVISION.

       FILE SECTION.

       FD  DISK-DATE                   COPY ELCDTEFD.

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
       77  ERCOBI-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  AGT-SUB                     PIC S999 COMP-3 VALUE +0.
       77  WS-CURRENT-BIN-DATE         PIC XX VALUE LOW-VALUES.
       77  WS-BIN-COMPARE-DT           PIC XX VALUE LOW-VALUES.
       77  WS-COMPARE-TYPE             PIC X  VALUE SPACES.
       77  WS-COMPARE-STATUS           PIC X  VALUE SPACES.
       01  WS-SAVE-ERCOBI              PIC X(620).
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

           MOVE DTE-CLASIC-COMPANY-CD  TO BL-CONTROL-PRIMARY
           START ERCOBI KEY > BL-CONTROL-PRIMARY

           IF ERCOBI-FILE-STATUS = '10' OR '23'
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERCOBI-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERCOBI - START '
                    ERCOBI-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0014-EXIT.
           EXIT.

       0017-READ-INPUT.

           READ ERCOBI NEXT RECORD

           IF ERCOBI-FILE-STATUS = '10' OR '23'
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERCOBI-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERCOBI - READNEXT '
                    ERCOBI-FILE-STATUS ' ' BL-CONTROL-PRIMARY (2:16)
                 PERFORM ABEND-PGM
              ELSE
                 ADD 1                 TO FILE-IN-CNT
              END-IF
           END-IF

           .
       0017-EXIT.
           EXIT.

       0020-PROCESS.

           IF (BL-STMT-OWNER = 'DADA')
              PERFORM 0070-BUILD-ERCOBI   THRU 0070-EXIT
           END-IF

           PERFORM 0017-READ-INPUT     THRU 0017-EXIT

           .
       0020-EXIT.
            EXIT.


       0040-OPEN-FILES.

PEMTST*    OPEN INPUT ERCOBI
PEMTST     OPEN I-O ERCOBI

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
       
           CLOSE ERCOBI

           IF ERCOBI-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERCOBI CLOSE ' ERCOBI-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0060-EXIT.
           EXIT.

       0070-BUILD-ERCOBI.

           MOVE COMP-BILLING-INSTRUCTIONS
                                       TO WS-SAVE-ERCOBI
PEMTST*    MOVE '00'                   TO ERCOBI-FILE-STATUS
PEMTST     DELETE ERCOBI
           IF ERCOBI-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERCOBI - DELETE ' ERCOBI-FILE-STATUS
                 ' ' BL-CONTROL-PRIMARY (2:16)
              PERFORM ABEND-PGM
           END-IF

           MOVE WS-SAVE-ERCOBI         TO COMP-BILLING-INSTRUCTIONS
           MOVE 'KRHA'                 TO BL-STMT-OWNER
           PERFORM 0074-WRITE-ERCOBI   THRU 0074-EXIT

           .
       0070-EXIT.
            EXIT.

                     
       0074-WRITE-ERCOBI.
       
           DISPLAY ' ABOUT TO ADD    NAME ' BL-STMT-OWNER ' '
                 BL-REPORT-GROUP-ID ' ' BL-ACCOUNT-NAME

PEMTST*    MOVE '00'                   TO ERCOBI-FILE-STATUS
PEMTST     WRITE COMP-BILLING-INSTRUCTIONS

           IF ERCOBI-FILE-STATUS = '22'
              DISPLAY ' ERROR - ERCOBI - WRITE - ALREADY ON FILE '
                 ERCOBI-FILE-STATUS ' ' BL-CONTROL-PRIMARY (2:16)
           ELSE
              IF ERCOBI-FILE-STATUS = '00'
                 ADD 1                 TO FILE-OUT-CNT
              ELSE
                 DISPLAY ' ERROR - ERCOBI - WRITE ' BL-STMT-OWNER ' '
                    BL-REPORT-GROUP-ID ' ' ERCOBI-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
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

