       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CERTQRY.
       DATE-COMPILED.

      *REMARKS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CERTS        ASSIGN TO SYS010.
           SELECT PRTR         ASSIGN TO SYS008.
                                                                        
       DATA DIVISION.
       FILE SECTION.

       FD  CERTS
                                       COPY ECSCRIFD.
                                       COPY ECSCRT01.
       FD  PRTR
                                COPY ELCPRTFD.



       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CERTQRY WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-CERT                VALUE 'Y'.
       77  X                           PIC X      VALUE ' '.

       01  PRT-WORK.
           05  PRT-CC                  PIC X(01) VALUE '1'.
           05  PRT-ACCT                PIC X(10).
           05  FILLER                  PIC X(03).
           05  PRT-CERT                PIC X(10).
           05  FILLER                  PIC X(03).
           05  PRT-LAST-NAME           PIC X(15).
           05  FILLER                  PIC X(03).
           05  PRT-LFTYP               PIC X(02).
           05  FILLER                  PIC X(03).
           05  PRT-LFRFND              PIC ZZZ99.99.
           05  FILLER                  PIC X(03).
           05  PRT-AHTYP               PIC X(02).
           05  FILLER                  PIC X(03).
           05  PRT-AHRFND              PIC ZZZ99.99.
           05  FILLER                  PIC X(03).
           05  PRT-STATUS              PIC X(01).
           05  FILLER                  PIC X(03).
           05  PRT-AHRFND-CALC         PIC ZZZ99.99.
           05  FILLER                  PIC X(03).
           05  PRT-AH-REFUND-TYPE      PIC X(01).
           05  FILLER                  PIC X(03).
           05  PRT-AH-CUR-STATUS       PIC X(01).

        
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

       01  DATE-AREAS.
           05  WS-WORK-DATE            PIC 9(11).
           05  FILLER REDEFINES WS-WORK-DATE.
               10  FILLER              PIC XXX.
               10  WS-WORK-CCYY        PIC X(4).
               10  WS-WORK-MM          PIC XX.
               10  WS-WORK-DD          PIC XX.

       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

           EJECT
                                       COPY ELCDATE.
      /
                                       COPY ELCDTECX.
      /
       PROCEDURE DIVISION.

       0002-INPUT.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0060-READ-CERT      THRU 0060-EXIT

           PERFORM 0080-PROCESS-CERT   THRU 0080-EXIT UNTIL
                 (END-OF-CERT)

           CLOSE CERTS
               PRTR   

           GOBACK
           .
       0002-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT CERTS
               OUTPUT PRTR   
               
           .

       0020-EXIT.
           EXIT.

       0060-READ-CERT.

           READ CERTS AT END
               SET END-OF-CERT TO TRUE
           END-READ

           .

       0060-EXIT.
           EXIT.


       0080-PROCESS-CERT.

           IF (CR-ACCOUNT = '0000693500')
      *       AND (CR-DT < 19940722)
      *       AND (CR-CERT = '0003011352')
               DISPLAY 'target cert'
               MOVE CR-ACCOUNT   TO PRT-ACCT
               DISPLAY 'PRT-ACCT ' PRT-ACCT
               MOVE CR-CERT      TO PRT-CERT
               MOVE CR-LNAME     TO PRT-LAST-NAME
               MOVE CR-LFTYP     TO PRT-LFTYP
               MOVE CR-LFRFND    TO PRT-LFRFND
               MOVE CR-AHTYP     TO PRT-AHTYP
               MOVE CR-AHRFND    TO PRT-AHRFND
               MOVE CR-ENTRY-STATUS TO PRT-STATUS
               MOVE CR-AHRFND-CALC  TO PRT-AHRFND-CALC
               MOVE CR-AH-REFUND-TYPE  TO PRT-AH-REFUND-TYPE 
               MOVE CR-AH-CURRENT-STATUS TO PRT-AH-CUR-STATUS
               WRITE PRT FROM PRT-WORK 
               MOVE SPACE TO PRT-CC
           END-IF

           PERFORM 0060-READ-CERT      THRU 0060-EXIT

           .
       0080-EXIT.
           EXIT.


       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.




       ABEND-PGM.
                                       COPY ELCABEND.

           EJECT
