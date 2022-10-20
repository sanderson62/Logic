       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.                PEMCRM6.
      *AUTHOR.     PABLO.                                               
      *REMARKS.                                                         
      *     THIS PROGRAM READS THE RESV, MATCHES TO THE CERT, UPDATES
      *       THE RESV AND WRITES OUT A NEW RESV
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT RESV-IN              ASSIGN TO SYS010.

           SELECT CERT-IN              ASSIGN TO SYS011.

           SELECT RESV-OUT             ASSIGN TO SYS013.
                                                                        
       DATA DIVISION.                                                   
                                                                        
       FILE SECTION.                                                    
                                                                        
       FD  CERT-IN                                                      
           RECORDING MODE IS F                                          
           LABEL RECORDS ARE STANDARD                                   
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
                                      COPY ECSCRT01.                    
                                                                        
       FD  RESV-OUT
           RECORDING MODE IS F                                          
           LABEL RECORDS ARE STANDARD                                   
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  RESV-OUT-RECORD            PIC X(510).
                                                                        
       FD  RESV-IN                                                      
           RECORDING MODE IS F                                          
           LABEL RECORDS ARE STANDARD                                   
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
                                       COPY ECSEXT01.
                                                                        
       WORKING-STORAGE SECTION.                                         
       77  FILLER  PIC X(32) VALUE '********************************'.  
       77  FILLER  PIC X(32) VALUE '         WORKING-STORAGE        '.  
       77  FILLER  PIC X(32) VALUE '********************************'.  

       01  WS-CERT-DT                PIC 9(8)  VALUE ZEROS.
       01  WS-EXTR-DT                PIC 9(8)  VALUE ZEROS.
       01  W-MISC.                                                      
           05  WS-DATE-ALPH.
               10  FILLER             PIC XXX VALUE '000'.
               10  WS-WORK-CENT       PIC XX.
               10  WS-WORK-YR         PIC XX.
               10  WS-WORK-MO         PIC XX.
               10  WS-WORK-DA         PIC XX.
           05  WS-DATE-NUM REDEFINES WS-DATE-ALPH
                                      PIC 9(11).

           05  RESV-IN-CNT             PIC 9(9) VALUE ZEROS.
           05  CERT-IN-CNT             PIC 9(9) VALUE ZEROS.
           05  RESV-FIX-CNT            PIC 9(9) VALUE ZEROS.
           05  RESV-OT-CNT             PIC 9(9) VALUE ZEROS.
           05  RESV-SEL-CNT            PIC 9(9) VALUE ZEROS.
           05  WS-EOF-SW              PIC X VALUE SPACES.
               88  END-OF-RESV              VALUE 'Y'.
           05  WS-CRT-SW              PIC X VALUE ' '.
               88  END-OF-CERT              VALUE 'Y'.
                                                                        
                                       COPY ELCDATE.
       PROCEDURE DIVISION.                                              

           OPEN INPUT    RESV-IN    CERT-IN

           OPEN OUTPUT      RESV-OUT

           PERFORM 0010-READ-RESV      THRU 0010-EXIT
           PERFORM 0015-READ-CERT      THRU 0015-EXIT

           PERFORM 0020-PROCESS THRU 0020-EXIT UNTIL
              END-OF-RESV

           CLOSE   RESV-IN   CERT-IN  RESV-OUT
              
           DISPLAY ' CERT RECS READ ' CERT-IN-CNT
           DISPLAY ' RESV RECS READ ' RESV-IN-CNT
           DISPLAY ' RESV RECS SEL  ' RESV-SEL-CNT
           DISPLAY ' RESV RECS OUT  ' RESV-OT-CNT
           DISPLAY ' RESV RECS FIX  ' RESV-FIX-CNT

           GOBACK

           .
       0010-READ-RESV.
                                                                        
           READ RESV-IN AT END                                                   
              SET END-OF-RESV TO TRUE
           END-READ                                                     
                                                                        
           IF NOT END-OF-RESV
              ADD  1    TO RESV-IN-CNT
              IF (DE-REIN NOT = 'R')
                 AND (DE-TRANS = 'X' OR 'Y')
                 AND (DE-CERT NOT = 'ACTBENIBNR')
                 ADD  1                TO RESV-SEL-CNT
              ELSE
                 GO TO 0010-READ-RESV
              END-IF
           END-IF
           
           .
       0010-EXIT.
           EXIT.

       0015-READ-CERT.                                                  
                                                                        
           READ CERT-IN AT END
              SET END-OF-CERT TO TRUE
           END-READ                                                     
                                                                        
           IF NOT END-OF-CERT
              ADD  1    TO CERT-IN-CNT                                  
           END-IF

           .
       0015-EXIT.
           EXIT.
                                                                        
       0020-PROCESS.                                                    
                                                                        
      *    DISPLAY ' EXT KEY = ' EXT-KEY
      *    DISPLAY 'CERT KEY = ' CERT-KEY
           IF DE-CONTROL = CR-FULL-CONTROL
              PERFORM 0030-MATCHED     THRU 0030-EXIT
              PERFORM 0010-READ-RESV   THRU 0010-EXIT
           ELSE
              IF DE-CONTROL > CR-FULL-CONTROL
                 PERFORM 0015-READ-CERT
                                       THRU 0015-EXIT
              ELSE
                 MOVE CR-DT            TO WS-CERT-DT
                 MOVE DE-EFF           TO WS-EXTR-DT
                 DISPLAY ' NO MATCH ' DE-CONTROL (1:19) ' ' WS-EXTR-DT
                    ' ' DE-CERT '**' CR-FULL-CONTROL (1:19) ' '
                    WS-CERT-DT ' ' CR-CERT-NO
                 PERFORM 0010-READ-RESV THRU 0010-EXIT
              END-IF
           END-IF

           .
       0020-EXIT.                                                       
            EXIT.                                                       
                                                                        
       0030-MATCHED.

           ADD 1 TO RESV-FIX-CNT
           
           IF DE-TRANS = 'Y'
              MOVE CR-RATING-CLASS     TO DE-RATE-CLASS
              IF DE-LIFE-RSV
                 MOVE CR-LF-DEV-CODE   TO DE-DEV-CODE
              ELSE
                 MOVE CR-AH-DEV-CODE   TO DE-DEV-CODE
              END-IF
           ELSE
              MOVE CR-RATING-CLASS     TO DE-CLM-CLASS
              IF DE-DEATH
                 MOVE CR-LF-DEV-CODE   TO DE-CLM-DEV
              ELSE
                 MOVE CR-AH-DEV-CODE   TO DE-CLM-DEV
              END-IF
           END-IF
           PERFORM 0040-WRITE-RESV     THRU 0040-EXIT

           .   
       0030-EXIT.                                                       
            EXIT.                                                       

       0040-WRITE-RESV.
       
           WRITE RESV-OUT-RECORD FROM DETAIL-EXTRACT
           ADD 1 TO RESV-OT-CNT

           .
       0040-EXIT.
           EXIT.