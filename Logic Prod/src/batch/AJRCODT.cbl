       IDENTIFICATION DIVISION.
       PROGRAM-ID.    AJRCODT.
       AUTHOR.        AJR.
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
032406* INITIALIZE CO-FIRST-WRITTEN-DT TO X'9F01' (01/01/06)
030404******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERCOMP       ASSIGN TO ERCOMP
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERCOMP-FILE-STATUS
                               RECORD KEY IS CO-CONTROL-PRIMARY.

       DATA DIVISION.
       FILE SECTION.

       FD  ERCOMP.

                                       COPY ERCCOMP.

           EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     AJRCODT WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.


030404 01  WS-MISC.
           05  WS-EOF-SW               PIC X      VALUE SPACES.
               88  END-OF-ERCOMP                  VALUE 'Y'.
           05  ERCOMP-FILE-STATUS      PIC XX     VALUE '00'.
           05  SUB1            COMP-3  PIC S9(3)  VALUE +0.
030404     05  WS-COMPANY-CD           PIC X(01)  VALUE SPACE.

032406     05  WS-INITIAL-DT           PIC XX     VALUE X'9F01'.

           05  WS-ERCOMP-IN            PIC 9(7)   VALUE ZEROS.
           05  WS-ERCOMP-FIX           PIC 9(7)   VALUE ZEROS.

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

030404 PROCEDURE DIVISION.

       0000-BEGIN.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT
           PERFORM 0040-INIT           THRU 0040-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
                (END-OF-ERCOMP)

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY ' RECORDS IN    ' WS-ERCOMP-IN
           DISPLAY ' RECORDS FIXED ' WS-ERCOMP-FIX
           GOBACK
           .
       0002-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN I-O ERCOMP

           IF ERCOMP-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERCOMP OPEN ERROR ' ERCOMP-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .

       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE ERCOMP

           IF ERCOMP-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERCOMP CLOSE ERROR ' ERCOMP-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .

       0030-EXIT.
           EXIT.

       0040-INIT.

           PERFORM 0120-START-ERCOMP   THRU 0120-EXIT
           PERFORM 0110-READ-ERCOMP    THRU 0110-EXIT
           .

       0040-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           IF CO-RESP-NO = '0005500108' OR '0005500166' OR
               '0005500189' OR '0005500190' OR '0005500193' OR 
               '0005500199' OR '0005500329' OR '0005500341' OR
               '0005500342' OR '0005500352' OR '0005500353' OR 
               '05500365CE' OR '05500367CE' OR '0005500161' OR
               '0005500165' OR '0005500188' OR '0005500197' OR 
               '0005500356' OR '0005500357' OR '0005500358' OR
               '0005500167' OR '0005500177' OR '0005500179' OR
               '0005500181' OR '0005500187' OR '0005500196' OR
               '0005500182' OR '0005500159' OR '0005500160' OR
               '0005500162' OR '0005500163' OR '0005500180' OR
               '0005500184' OR '0005500330' OR '0005500359' OR
               '0005500113' OR '0005500114' OR '0005500115' OR
               '0005500116' OR '0005500119' OR '0005500200' OR
               '0005500168' OR '0005500178' OR '0005500126' OR
               '0005500128' OR '0005500122' OR '0005500123' OR
               '0005500124' OR '0005500343' 
                 MOVE LOW-VALUES TO CO-FIRST-WRITTEN-DT
           ELSE              
                 MOVE WS-INITIAL-DT TO CO-FIRST-WRITTEN-DT
           END-IF

           PERFORM 0080-REWRITE-ERCOMP THRU 0080-EXIT
           
           PERFORM 0110-READ-ERCOMP    THRU 0110-EXIT

           .

       0050-EXIT.
           EXIT.

       0080-REWRITE-ERCOMP.

           REWRITE COMPENSATION-MASTER
           
           IF ERCOMP-FILE-STATUS = '00'
              ADD 1                    TO WS-ERCOMP-FIX
           ELSE
              DISPLAY ' ERCOMP REWRITE ERROR ' ERCOMP-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0080-EXIT.
           EXIT.

       0110-READ-ERCOMP.

           READ ERCOMP NEXT RECORD

           IF (ERCOMP-FILE-STATUS = '10' OR '23')
              SET END-OF-ERCOMP        TO TRUE
           ELSE
              IF ERCOMP-FILE-STATUS NOT = '00'
                 DISPLAY ' ERCOMP READ ERROR ' ERCOMP-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           IF NOT END-OF-ERCOMP
               ADD +1              TO WS-ERCOMP-IN
           END-IF

           .
       0110-EXIT.
           EXIT.

       0120-START-ERCOMP.

           MOVE LOW-VALUES             TO CO-CONTROL-PRIMARY
           MOVE X'01'                  TO CO-COMPANY-CD

           START ERCOMP KEY NOT < CO-CONTROL-PRIMARY

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

           EJECT
