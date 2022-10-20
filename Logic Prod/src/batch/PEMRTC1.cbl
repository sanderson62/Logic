       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMCOF4.
       AUTHOR.     PABLO.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT RATE-IN        ASSIGN TO SYS010.

           SELECT RATE-OUT       ASSIGN TO SYS011.

       DATA DIVISION.                                                   
                                                                        
       FILE SECTION.                                                    
                                                                        
       FD  RATE-IN
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

                                       COPY ERCRATE.
                                                                        
       FD  RATE-OUT
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

       01  RATE-OUT-REC                PIC X(1765).


       WORKING-STORAGE SECTION.                                         
                                                                        
       77  FILLER  PIC X(32) VALUE '********************************'.  
       77  FILLER  PIC X(32) VALUE '   PEMRTC1  WORKING-STORAGE     '.  
       77  FILLER  PIC X(32) VALUE '***********VMOD=2.001 **********'.  
                                                                        
       77  WS-RECS-IN                 PIC 9(7)   VALUE ZEROS.
       77  WS-RECS-OUT                PIC 9(7)   VALUE ZEROS.
       77  WS-FILE-SW                 PIC X      VALUE ' '.
           88  END-OF-INPUT                      VALUE 'Y'.

       PROCEDURE DIVISION.                                              

           PERFORM 0000-INITIALIZE     THRU 0000-EXIT

           PERFORM 0200-PROCESS-INPUT  THRU 0200-EXIT UNTIL
              END-OF-INPUT

           CLOSE RATE-IN RATE-OUT

           DISPLAY ' RECORDS READ     ' WS-RECS-IN
           DISPLAY ' RECORDS OUT      ' WS-RECS-OUT

           GOBACK

           .
       0000-INITIALIZE.

           OPEN INPUT  RATE-IN
                OUTPUT RATE-OUT

           PERFORM 0300-READ-INPUT    THRU 0300-EXIT

          .
       0000-EXIT.
           EXIT.

       0200-PROCESS-INPUT.

           IF (RT-COMPANY-CD = X'04')
              AND (RT-STATE-CODE = 'CO48000')
              AND (RT-L-AH = 'A')
              AND (RT-LAH-NUM = '01' OR '02' OR '04' OR '05' OR '11')
              PERFORM 0400-WRITE-OUTPUT THRU 0400-EXIT
           END-IF

           PERFORM 0300-READ-INPUT    THRU 0300-EXIT

          .
       0200-EXIT.
           EXIT.

       0300-READ-INPUT.

           READ RATE-IN AT END
              SET END-OF-INPUT TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1                    TO WS-RECS-IN
           END-IF

           .
       0300-EXIT.
           EXIT.

       0400-WRITE-OUTPUT.

           WRITE RATE-OUT-REC          FROM RATE-RECORD
           ADD 1 TO WS-RECS-OUT

           .
       0400-EXIT.
           EXIT.
