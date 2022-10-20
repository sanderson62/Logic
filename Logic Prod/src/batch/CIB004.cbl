       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.    CIB004.                                           
       DATE-WRITTEN.  MAY, 2000.                                        
                                                                        
      ***************************************************************** 
      *                                                               * 
      *                                                               * 
      ***************************************************************** 
                                                                        
      /                                                                 
       ENVIRONMENT DIVISION.                                            
                                                                        
       INPUT-OUTPUT SECTION.                                            
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT PRINT-LINES-IN                                        
               ASSIGN TO SYS010                                         
               FILE STATUS IS SYS010-STATUS.                            
                                                                        
           SELECT STATE-TOTALS
              organization is line sequential
               ASSIGN TO SYS011.                                        
                                                                        
           SELECT FINAL-TOTALS
              organization is line sequential
               ASSIGN TO SYS012.                                        
                                                                        
      /                                                                 
       DATA DIVISION.                                                   
                                                                        
       FILE SECTION.                                                    
                                                                        
       FD  PRINT-LINES-IN                                               
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  FILLER.                                                      
           05  CC            PIC X.                                     
           05  INPUT-RECORD  PIC X(132).                                
                                                                        
       FD  STATE-TOTALS                                                 
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  STATE-RECORD      PIC X(132).                                
                                                                        
       FD  FINAL-TOTALS                                                 
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  FINAL-RECORD      PIC X(132).                                
                                                                        
                                                                        
                                                                        
           EJECT                                                        
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
       01  FILLER.                                                      
           05  S0C7                PIC X       VALUE SPACE.             
           05  FORCE-DUMP REDEFINES S0C7 PIC S9 COMP-3.                 
           05  SYS010-STATUS       PIC X(2)    VALUE SPACE.             
               88  EOF                         VALUE '10'.              
           05  PRINT-SW            PIC X       VALUE SPACE.             
               88  DO-NOT-PRINT-PAGE           VALUE ' '.               
               88  STATE-PAGE                  VALUE 'S'.               
               88  FINAL-PAGE                  VALUE 'F'.               
           05  SAVE-RECORD         PIC X(132)  VALUE SPACE.             
                                                                        
                                                                        
      /                                                                 
       PROCEDURE DIVISION.                                              
                                                                        
           PERFORM 0000-INIT-ROUTINE                                    
              THRU 0000-EXIT                                            
                                                                        
           PERFORM 1000-PROCESS-FILE                                    
              THRU 1000-EXIT UNTIL EOF                                  
                                                                        
           STOP RUN.                                                    
                                                                        
                                                                        
      /                                                                 
       0000-INIT-ROUTINE.                                               
      *                                                                 
           OPEN INPUT PRINT-LINES-IN                                    
           IF SYS010-STATUS NOT = '00'                                  
              DISPLAY 'OPEN ERROR ' SYS010-STATUS ' ON SYS010'          
              ADD +1 TO FORCE-DUMP.                                     
                                                                        
           OPEN OUTPUT STATE-TOTALS                                     
                       FINAL-TOTALS.                                    
                                                                        
       0000-EXIT.                                                       
           EXIT.                                                        
                                                                        
                                                                        
                                                                        
      /                                                                 
       1000-PROCESS-FILE.                                               
      *                                                                 
           READ PRINT-LINES-IN                                          
                AT END GO TO 1000-EXIT.                                 
                                                                        
           IF CC = '1'                                                  
              PERFORM 2000-NEW-PAGE THRU 2000-EXIT.                     
                                                                        
           IF PRINT-SW = 'S'                                            
              WRITE STATE-RECORD FROM INPUT-RECORD                      
           ELSE                                                         
           IF PRINT-SW = 'F'                                            
              WRITE FINAL-RECORD FROM INPUT-RECORD.                     
                                                                        
       1000-EXIT.                                                       
           EXIT.                                                        
                                                                        
                                                                        
      /                                                                 
       2000-NEW-PAGE.                                                   
      *                                                                 
           MOVE INPUT-RECORD TO SAVE-RECORD                             
           READ PRINT-LINES-IN                                          
           IF INPUT-RECORD(12:5) = 'STATE'                              
              MOVE 'S' TO PRINT-SW                                      
              WRITE STATE-RECORD FROM SAVE-RECORD                       
           ELSE                                                         
              IF INPUT-RECORD(1:12) = 'FINAL TOTALS'                    
                 MOVE 'F' TO PRINT-SW                                   
                 WRITE FINAL-RECORD FROM SAVE-RECORD                    
              ELSE                                                      
                 MOVE ' ' TO PRINT-SW                                   
              END-IF                                                    
           END-IF                                                       
           .                                                            
       2000-EXIT.                                                       
           EXIT.                                                        
                                                                        
