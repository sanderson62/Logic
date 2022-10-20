       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDLOX1.
       AUTHOR.     PABLO.
       DATE-COMPILED.
060608*SECURITY.   *****************************************************
060608*            *                                                   *
060608*            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
060608*            *                                                   *
060608*            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
060608*            *   OF     CSO     IS EXPRESSLY PROHIBITED WITHOUT  *
060608*            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
060608*            *                                                   *
060608*            *****************************************************

060608******************************************************************
060608*REMARKS.                                                        *
060608*        THIS PROGRAM RUNS DAILY AND CREATES AN EXTRACT          *
060608*        OF LOAN OFFICER (ERLOFC) RECORDS                        *
060608******************************************************************
060608*                   C H A N G E   L O G
060608*
060608* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
060608*-----------------------------------------------------------------
060608*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
060608* EFFECTIVE    NUMBER
060608*-----------------------------------------------------------------
060608* 060608   2008060300002   PEMA  NEW PROGRAM
032612* 032612   2011110200001   PEMA  AHL CHANGES
060608******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERLOFC           ASSIGN TO ERLOFC
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS LO-CONTROL-PRIMARY
                                   FILE STATUS IS ERLOFC-FILE-STATUS.

           SELECT LOFC-OUT         ASSIGN TO LOFCOUT
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE        ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  ERLOFC.

                                       COPY ERCLOFC.

       FD  LOFC-OUT
           RECORDING MODE V
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  LOFC-OUT-REC                PIC X(0065).
       01  LOFC-HEAD-REC               PIC X(0062).


       FD  DISK-DATE                                                    
                                   COPY ELCDTEFD.                       

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDLOX1 WORKING STORAGE      '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-ERLOFC             VALUE 'Y'.
       77  PGM-SUB                 PIC S999    COMP    VALUE +539.
       77  LOF-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  LOF-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.

       01  LOFC-DETAIL-RECORD.
           12  LOFC-CARRIER            PIC X.
           12  LOFC-TAB1               PIC X.
           12  LOFC-GROUPING           PIC X(6).
           12  LOFC-TAB2               PIC X.
           12  LOFC-STATE              PIC XX.
           12  LOFC-TAB3               PIC X.
           12  LOFC-ACCOUNT            PIC X(10).
           12  LOFC-TAB4               PIC X.
           12  LOFC-OFF-CODE           PIC X(5).
           12  LOFC-TAB5               PIC X.
           12  LOFC-OFF-NAME           PIC X(30).
           12  LOFC-TAB6               PIC X.
           12  LOFC-DETAIL             PIC X.
           12  LOFC-TAB7               PIC X.
           12  LOFC-COMP               PIC X.
           12  LOFC-TAB8               PIC X.
           12  LOFC-EOR                PIC X.

       01  LOFC-HEADER-RECORD.
           12  HEAD-CARRIER            PIC X(7)  VALUE 'CARRIER'.
           12  HEAD-TAB1               PIC X.
           12  HEAD-GROUPING           PIC X(5)  VALUE 'GROUP'.
           12  HEAD-TAB2               PIC X.
           12  HEAD-STATE              PIC X(5)  VALUE 'STATE'.
           12  HEAD-TAB3               PIC X.
           12  HEAD-ACCOUNT            PIC X(4)  VALUE 'ACCT'.
           12  HEAD-TAB4               PIC X.
           12  HEAD-OFF-CODE           PIC X(6)  VALUE ' CODE '.
           12  HEAD-TAB5               PIC X.
           12  HEAD-OFF-NAME           PIC X(10) VALUE ' NAME '.
           12  HEAD-TAB6               PIC X.
           12  HEAD-DETAIL             PIC X(7)  VALUE 'DET SW '.
           12  HEAD-TAB7               PIC X.
           12  HEAD-COMP               PIC X(7)  VALUE 'COMP SW'.
           12  HEAD-TAB8               PIC X.
           12  HEAD-EOR                PIC XXX   VALUE 'EOR'.

       01  WS-ABEND-FIELDS.
           12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.
           12  WS-ZERO                 PIC S9          VALUE ZERO.
           12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.
           12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.

       01  WS-MISC.
           05  WS-SAVE-ERLOFC          PIC X(0065) VALUE LOW-VALUES.
           05  ERLOFC-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.

                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.
                                       COPY ELCDTERX.                       

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-LOFC   THRU 0100-EXIT UNTIL
              END-OF-ERLOFC

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' LOFC RECORDS READ    '  LOF-RECS-IN
           DISPLAY ' LOFC RECORDS WRITTEN '  LOF-RECS-OUT
           GOBACK

           .
       0100-PROCESS-LOFC.

           MOVE WS-SAVE-ERLOFC         TO LOFC-DETAIL-RECORD
           MOVE LO-CARRIER             TO LOFC-CARRIER
           MOVE LO-GROUPING            TO LOFC-GROUPING
           MOVE LO-STATE               TO LOFC-STATE
           MOVE LO-ACCOUNT             TO LOFC-ACCOUNT
           MOVE LO-OFFICER-CODE        TO LOFC-OFF-CODE
           MOVE LO-OFFICER-NAME        TO LOFC-OFF-NAME
           MOVE LO-DETAIL-CONTROL      TO LOFC-DETAIL
           MOVE LO-COMP-CONTROL        TO LOFC-COMP

           PERFORM 0300-WRITE-LOFC     THRU 0300-EXIT

           PERFORM 0200-READ-LOFC      THRU 0200-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-LOFC.

           READ ERLOFC NEXT RECORD

           IF (ERLOFC-FILE-STATUS = '10' OR '23')
              OR (LO-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
              SET END-OF-ERLOFC        TO TRUE
           ELSE
              IF ERLOFC-FILE-STATUS NOT = '00'
                 DISPLAY 'ERLOFC READ NEXT ' ERLOFC-FILE-STATUS
                 SET END-OF-ERLOFC     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-ERLOFC
              ADD 1                    TO LOF-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-LOFC.

           INSPECT LOFC-DETAIL-RECORD REPLACING
              ALL ';'     BY ' '
              ALL X'00'   BY ' '
              ALL X'09'   BY ';'

           WRITE LOFC-OUT-REC          FROM LOFC-DETAIL-RECORD
           ADD 1                       TO LOF-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT ERLOFC
               OUTPUT LOFC-OUT

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ERLOFC LOFC-OUT

           .
       0500-EXIT.
           EXIT.

       0550-START-LOFC.

           MOVE LOW-VALUES             TO LO-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO LO-COMPANY-CD

           START ERLOFC KEY >= LO-CONTROL-PRIMARY

           IF (ERLOFC-FILE-STATUS = '10' OR '23')
              OR (LO-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
              SET END-OF-ERLOFC        TO TRUE
           ELSE
              IF ERLOFC-FILE-STATUS NOT = '00'
                 DISPLAY 'ERLOFC START     ' ERLOFC-FILE-STATUS
                 SET END-OF-ERLOFC     TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.

       0600-INITIALIZE.

           MOVE SPACES                 TO LOFC-DETAIL-RECORD
           MOVE X'09'                  TO LOFC-TAB1
                                          LOFC-TAB2
                                          LOFC-TAB3
                                          LOFC-TAB4
                                          LOFC-TAB5
                                          LOFC-TAB6
                                          LOFC-TAB7
                                          LOFC-TAB8
           MOVE 'E'                    TO LOFC-EOR
           MOVE LOFC-DETAIL-RECORD     TO WS-SAVE-ERLOFC

           MOVE ';'                    TO HEAD-TAB1
                                          HEAD-TAB2
                                          HEAD-TAB3
                                          HEAD-TAB4
                                          HEAD-TAB5
                                          HEAD-TAB6
                                          HEAD-TAB7
                                          HEAD-TAB8

           WRITE LOFC-HEAD-REC         FROM LOFC-HEADER-RECORD

           PERFORM 0550-START-LOFC     THRU 0550-EXIT
032612     if not end-of-erlofc
              PERFORM 0200-READ-LOFC   THRU 0200-EXIT
032612     end-if

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM SECTION.
                                     COPY ELCABEND.
