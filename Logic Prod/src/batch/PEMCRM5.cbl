       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.                PEMCRM5.
      *AUTHOR.     PABLO.                                               
      *REMARKS.                                                         
      *     THIS PROGRAM READS THE GAAP, MATCHES TO THE CERT, UPDATES
      *       THE GAAP AND WRITES OUT A NEW GAAP
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT  GAAP-IN         ASSIGN TO SYS010.

           SELECT CERT-IN          ASSIGN TO SYS011.

           SELECT GAAP-OUT          ASSIGN TO SYS013.
                                                                        
       DATA DIVISION.                                                   
                                                                        
       FILE SECTION.                                                    
                                                                        
       FD  CERT-IN                                                      
           RECORDING MODE IS F                                          
           LABEL RECORDS ARE STANDARD                                   
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
                                      COPY ECSCRT01.                    
                                                                        
       FD  GAAP-OUT
           RECORDING MODE IS F                                          
           LABEL RECORDS ARE STANDARD                                   
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
       01  GAAP-OUT-RECORD            PIC X(365).
                                                                        
       FD  GAAP-IN                                                      
           RECORDING MODE IS F                                          
           LABEL RECORDS ARE STANDARD                                   
           BLOCK CONTAINS 0 RECORDS.                                    
                                                                        
                                       COPY ECSGAP01.
                                                                        
       WORKING-STORAGE SECTION.                                         
       77  FILLER  PIC X(32) VALUE '********************************'.  
       77  FILLER  PIC X(32) VALUE '         WORKING-STORAGE        '.  
       77  FILLER  PIC X(32) VALUE '********************************'.  

       01  W-MISC.                                                      
           05  WS-DATE-ALPH.
               10  FILLER             PIC XXX VALUE '000'.
               10  WS-WORK-CENT       PIC XX.
               10  WS-WORK-YR         PIC XX.
               10  WS-WORK-MO         PIC XX.
               10  WS-WORK-DA         PIC XX.
           05  WS-DATE-NUM REDEFINES WS-DATE-ALPH
                                      PIC 9(11).

           05  GAAP-IN-CNT             PIC 9(9) VALUE ZEROS.
           05  CERT-IN-CNT             PIC 9(9) VALUE ZEROS.
           05  GAAP-FIX-CNT            PIC 9(9) VALUE ZEROS.
           05  GAAP-OT-CNT             PIC 9(9) VALUE ZEROS.
           05  WS-EOF-SW              PIC X VALUE SPACES.
               88  END-OF-GAAP              VALUE 'Y'.
           05  WS-CRT-SW              PIC X VALUE ' '.
               88  END-OF-CERT              VALUE 'Y'.
                                                                        
                                       COPY ELCDATE.
       PROCEDURE DIVISION.                                              

           OPEN INPUT    GAAP-IN    CERT-IN

           OPEN OUTPUT      GAAP-OUT

           PERFORM 0010-READ-GAAP      THRU 0010-EXIT
           PERFORM 0015-READ-CERT      THRU 0015-EXIT

           PERFORM 0020-PROCESS THRU 0020-EXIT UNTIL
              END-OF-GAAP

           CLOSE   GAAP-IN   CERT-IN  GAAP-OUT
              
           DISPLAY ' CERT RECS READ ' CERT-IN-CNT
           DISPLAY ' GAAP RECS READ ' GAAP-IN-CNT
           DISPLAY ' GAAP RECS OUT  ' GAAP-OT-CNT
           DISPLAY ' GAAP RECS FIX  ' GAAP-FIX-CNT

           GOBACK

           .
       0010-READ-GAAP.
                                                                        
           READ GAAP-IN AT END                                                   
              DISPLAY ' FOUND END OF GAAP '
              SET END-OF-GAAP TO TRUE
           END-READ                                                     
                                                                        
           IF NOT END-OF-GAAP
              ADD  1    TO GAAP-IN-CNT
              IF GR-REIN = 'R'
                 GO TO 0010-READ-GAAP
              END-IF
           END-IF
           
           .
       0010-EXIT.
           EXIT.

       0015-READ-CERT.                                                  
                                                                        
           READ CERT-IN AT END
              DISPLAY ' FOUND END OF CERT '
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
           IF GR-CONTROL = CR-FULL-CONTROL
              PERFORM 0030-MATCHED     THRU 0030-EXIT
              PERFORM 0010-READ-GAAP   THRU 0010-EXIT
           ELSE
              IF GR-CONTROL > CR-FULL-CONTROL
                 PERFORM 0015-READ-CERT
                                       THRU 0015-EXIT
              ELSE
                 DISPLAY ' NO MATCH ' GR-CONTROL '  ' CR-FULL-CONTROL
                 PERFORM 0010-READ-GAAP THRU 0010-EXIT
              END-IF
           END-IF

           .
       0020-EXIT.                                                       
            EXIT.                                                       
                                                                        
       0030-MATCHED.

           ADD 1 TO GAAP-FIX-CNT
           
           MOVE CR-RATING-CLASS        TO GR-INS-NAME (1:2)
           MOVE CR-LF-DEV-CODE         TO GR-INS-NAME (3:3)
           MOVE CR-AH-DEV-CODE         TO GR-INS-NAME (6:3)
           PERFORM 0040-WRITE-GAAP     THRU 0040-EXIT
           .   
       0030-EXIT.                                                       
            EXIT.                                                       

       0040-WRITE-GAAP.
       
           WRITE GAAP-OUT-RECORD FROM GAAP-RECORD
           ADD 1 TO GAAP-OT-CNT

           .
       0040-EXIT.
           EXIT.
