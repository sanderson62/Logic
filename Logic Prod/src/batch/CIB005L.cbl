       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.    CIB005.                                           
       DATE-WRITTEN.  MAY, 2000.                                        
                                                                        
      ***************************************************************** 
      *                                                               * 
      *  THIS PROGRAM SEARCHES FOR THE PARAMETER IN THE FIRST 3 LINES * 
      *  OF A REPORT.  IF THE PARMATER IS FOUND, THE ENTIRE PAGE IS   * 
      *  IS PRINTED.                                                  * 
      *                                                               * 
      ***************************************************************** 
                                                                        
      /                                                                 
       ENVIRONMENT DIVISION.                                            
                                                                        
       INPUT-OUTPUT SECTION.                                            
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT PRINT-LINES-IN                                        
               ASSIGN TO SYS010                                         
               FILE STATUS IS SYS010-STATUS.                            
                                                                        
           SELECT PRINT-LINES-OUT  
               ORGANIZATION IS LINE SEQUENTIAL                                     
               ASSIGN TO SYS011.                                        
                                                                        
                                                                        
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
                                                                        
       FD  PRINT-LINES-OUT                                              
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  OUTPUT-RECORD     PIC X(132).                                
                                                                        
                                                                        
                                                                        
           EJECT                                                        
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
       01  FILLER.                                                      
           05  S0C7             PIC X      VALUE SPACE.                 
           05  FORCE-DUMP REDEFINES S0C7 PIC S9 COMP-3.                 
           05  SYS010-STATUS    PIC X(2)   VALUE SPACE.                 
               88  EOF                     VALUE '10'.                  
           05  STRT             PIC S9(4)  BINARY SYNC VALUE +0.        
           05  END-LINE         PIC S9(4)  BINARY SYNC VALUE +0.        
           05  PRINT-SW         PIC X(3)   VALUE SPACE.                 
           05  WORK-AREA.                                               
               10  SAVE-REC OCCURS 3 TIMES PIC X(132).                  
                                                                        
                                                                        
           EJECT                                                        
                                                                        
       LINKAGE SECTION.                                                 
                                                                        
       01  PARM.                                                        
           05  PARM-LENGTH BINARY  PICTURE IS S9(4).                    
           05  PARM-VALUE  DISPLAY PICTURE IS X(20).                    
                                                                        
                                                                        
      /                                                                 
       PROCEDURE DIVISION USING PARM.                                   
                                                                        
           PERFORM 0000-INIT-ROUTINE                                    
              THRU 0000-EXIT                                            
                                                                        
           PERFORM 1000-PROCESS-FILE                                    
              THRU 1000-EXIT UNTIL EOF                                  
                                                                        
           STOP RUN.                                                    
                                                                        
                                                                        
      /                                                                 
       0000-INIT-ROUTINE.                                               
      *                                                                 
           IF PARM-LENGTH = +0                                          
              DISPLAY 'PARAMETER MUST BE ENTERED'                       
              ADD +1 TO FORCE-DUMP.                                     
                                                                        
           IF PARM-LENGTH > +20                                         
              DISPLAY 'PARAMETER CANNOT BE LONGER THAN 20 CHARACTERS'   
              ADD +1 TO FORCE-DUMP.                                     
                                                                        
      *    396 = 132 * 3                                                
           COMPUTE END-LINE = 396 - PARM-LENGTH                         
                                                                        
           OPEN INPUT PRINT-LINES-IN                                    
           IF SYS010-STATUS NOT = '00'                                  
              DISPLAY 'OPEN ERROR ' SYS010-STATUS ' ON SYS010'          
              ADD +1 TO FORCE-DUMP.                                     
                                                                        
           OPEN OUTPUT PRINT-LINES-OUT.                                 
                                                                        
       0000-EXIT.                                                       
           EXIT.                                                        
                                                                        
                                                                        
                                                                        
      /                                                                 
       1000-PROCESS-FILE.                                               
      *                                                                 
           READ PRINT-LINES-IN                                          
                AT END GO TO 1000-EXIT.                                 
                                                                        
           IF CC = '1'                                                  
              PERFORM 2000-NEW-PAGE THRU 2000-EXIT.                     
                                                                        
           IF PRINT-SW = 'YES'                                          
              WRITE OUTPUT-RECORD FROM INPUT-RECORD.                    
                                                                        
       1000-EXIT.                                                       
           EXIT.                                                        
                                                                        
                                                                        
      /                                                                 
       2000-NEW-PAGE.                                                   
      *                                                                 
           MOVE INPUT-RECORD TO SAVE-REC(1)                             
           READ PRINT-LINES-IN                                          
           MOVE INPUT-RECORD TO SAVE-REC(2)                             
           READ PRINT-LINES-IN                                          
           MOVE INPUT-RECORD TO SAVE-REC(3)                             
                                                                        
           MOVE 'NO' TO PRINT-SW                                        
                                                                        
           PERFORM VARYING STRT FROM 1 BY 1                             
                     UNTIL PRINT-SW = 'YES'                             
                        OR STRT > END-LINE                              
               IF WORK-AREA(STRT:PARM-LENGTH) =                         
                  PARM-VALUE(1:PARM-LENGTH)                             
                    MOVE 'YES' TO PRINT-SW                              
               END-IF                                                   
           END-PERFORM                                                  
                                                                        
           IF PRINT-SW = 'YES'                                          
              WRITE OUTPUT-RECORD FROM SAVE-REC(1)                      
              WRITE OUTPUT-RECORD FROM SAVE-REC(2)                      
           END-IF                                                       
           .                                                            
       2000-EXIT.                                                       
           EXIT.                                                        
                                                                        
