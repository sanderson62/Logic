       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDLOFEX.
       AUTHOR.     AJRA
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERLOFC           ASSIGN TO ERLOFC
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS LO-CONTROL-PRIMARY
                                   FILE STATUS IS ERLOFC-FILE-STATUS.

           SELECT  MSTR-OUT        ASSIGN TO MSTROT
                                   ORGANIZATION IS LINE SEQUENTIAL.
                                   
       EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  ERLOFC.
                                   COPY ERCLOFC.

       FD  MSTR-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  MSTR-RECORD              PIC X(59).
           
       EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDLOFEX  WORKING-STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-FILE               VALUE 'Y'.
       77  MSTR-RECS-IN            PIC 9(9) VALUE ZEROS.
       77  MSTR-RECS-OUT           PIC 9(9) VALUE ZEROS.
              
       01  ERLOFC-FILE-STATUS      PIC XX    VALUE ZEROS.

       01  MSTR-REC.
           05  MS-CARRIER              PIC X(1). 
           05  FILLER                  PIC X(1) VALUE ';'.
           05  MS-GROUPING             PIC X(6).        
           05  FILLER                  PIC X(1) VALUE ';'.
           05  MS-STATE                PIC X(2).   
           05  FILLER                  PIC X(1) VALUE ';'.
           05  MS-ACCOUNT              PIC X(10).
           05  FILLER                  PIC X(1) VALUE ';'.
           05  MS-OFFICER-CODE         PIC X(5).
           05  FILLER                  PIC X(1) VALUE ';'.
           05  MS-OFFICER-NAME         PIC X(30).


       PROCEDURE DIVISION.

       0000-MAIN.

           PERFORM 0400-OPEN-FILES THRU 0400-EXIT

           PERFORM 0600-INITIALIZE THRU 0600-EXIT

           PERFORM 0100-PROCESS-MSTR THRU 0100-EXIT UNTIL
                 END-OF-FILE

           PERFORM 0500-CLOSE-FILES THRU 0500-EXIT

           DISPLAY ' MSTR RECORDS READ    ' MSTR-RECS-IN
           DISPLAY ' MSTR RECORDS WRITTEN ' MSTR-RECS-OUT
           GOBACK

           .
       0100-PROCESS-MSTR.

           PERFORM 0300-WRITE-MSTR THRU 0300-EXIT

           PERFORM 0200-READ-MSTR THRU 0200-EXIT

           .

       0100-EXIT.
           EXIT.

       0200-READ-MSTR.

           READ ERLOFC NEXT RECORD

           IF ERLOFC-FILE-STATUS = '10' OR '23'
              SET END-OF-FILE        TO TRUE
           ELSE
              IF ERLOFC-FILE-STATUS NOT = '00'
                 DISPLAY 'ERLOFC READ NEXT ' ERLOFC-FILE-STATUS
                 SET END-OF-FILE     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-FILE
              ADD 1 TO MSTR-RECS-IN
           END-IF

           .

       0200-EXIT.
           EXIT.

       0300-WRITE-MSTR.

           MOVE LO-CARRIER         TO MS-CARRIER
           MOVE LO-GROUPING        TO MS-GROUPING
           MOVE LO-STATE           TO MS-STATE
           MOVE LO-ACCOUNT         TO MS-ACCOUNT
           MOVE LO-OFFICER-CODE    TO MS-OFFICER-CODE
           MOVE LO-OFFICER-NAME    TO MS-OFFICER-NAME
           WRITE MSTR-RECORD FROM MSTR-REC
           ADD 1 TO MSTR-RECS-OUT

           .

       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT ERLOFC
               OUTPUT MSTR-OUT

           IF ERLOFC-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY 'ERLOFC OPEN ERROR  ' ERLOFC-FILE-STATUS
           END-IF

           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ERLOFC
                 MSTR-OUT

           .

       0500-EXIT.
           EXIT.

       0550-START-ERLOFC.

           MOVE LOW-VALUES             TO LO-CONTROL-PRIMARY
           MOVE X'04'                  TO LO-COMPANY-CD

           START ERLOFC KEY IS NOT < LO-CONTROL-PRIMARY

           IF ERLOFC-FILE-STATUS = '10' OR '23'
              SET END-OF-FILE        TO TRUE
           ELSE
              IF ERLOFC-FILE-STATUS NOT = '00'
                 DISPLAY 'ERLOFC START     ' ERLOFC-FILE-STATUS
                 SET END-OF-FILE     TO TRUE
              END-IF
           END-IF

           .

       0550-EXIT.
           EXIT.


       0600-INITIALIZE.

           PERFORM 0550-START-ERLOFC   THRU 0550-EXIT
           PERFORM 0200-READ-MSTR THRU 0200-EXIT

           .

       0600-EXIT.
           EXIT.

