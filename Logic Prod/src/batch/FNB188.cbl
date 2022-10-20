       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.    FNB188.                                           
       AUTHOR.        DAN DRYDEN.                                       
       DATE-WRITTEN.  DECEMBER, 1998.                                   
                                                                        
      ***************************************************************** 
      *  THIS PROGRAM SUBSTITUTES YYYYMMDDHHMMSS WITH THE CURRENT     * 
      *  DATE AND TIME.                                               * 
      ***************************************************************** 
                                                                        
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT DNLOAD-DATA                                           
               ASSIGN TO SYS001                                         
               STATUS IS SYS001-STATUS.                                 
                                                                        
                                                                        
      /                                                                 
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD  DNLOAD-DATA                                                  
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           RECORD CONTAINS 80 CHARACTERS                                
           BLOCK CONTAINS 0 RECORDS.                                    
       01  DNLOAD-RECORD      PIC X(80).                                
                                                                        
                                                                        
      /                                                                 
       WORKING-STORAGE SECTION.                                         
                                                                        
       01  FILLER.                                                      
           05  SUB              PIC S9(8)  BINARY VALUE +0.             
           05  S0C7             PIC X      VALUE SPACE.                 
           05  FORCE-DUMP REDEFINES S0C7   PIC S9  COMP-3.              
           05  SYS001-STATUS    PIC XX     VALUE ZERO.                  
               88  EOF                     VALUE '10'.                  
           05  UPDATE-SW        PIC X      VALUE ZERO.                  
               88  RECORD-UPDATED          VALUE '1'.                   
               88  END-OF-RECORD           VALUE '2'.                   
           05  YYYYMMDDHHMMSS.                                          
               10  YYYYMMDD     PIC 9(8)   VALUE ZERO.                  
               10  HHMMSS       PIC 9(8)   VALUE ZERO.                  
                                                                        
                                                                        
                                                                        
                                                                        
      *                                                                 
       PROCEDURE DIVISION.                                              
      *                                                                 
           OPEN I-O DNLOAD-DATA                                         
           IF SYS001-STATUS NOT = '00'                                  
              DISPLAY 'OPEN ERROR ' SYS001-STATUS ' ON SYS001'          
              ADD +1 TO FORCE-DUMP                                      
           END-IF                                                       
                                                                        
           CALL 'IGZEDT4' USING YYYYMMDD                                
           ACCEPT HHMMSS FROM TIME                                      
                                                                        
           PERFORM UNTIL EOF                                            
              READ DNLOAD-DATA                                          
              IF NOT EOF                                                
                 MOVE ZERO TO UPDATE-SW                                 
                 PERFORM VARYING SUB FROM 1 BY 1                        
                   UNTIL END-OF-RECORD OR RECORD-UPDATED                
                     IF DNLOAD-RECORD(SUB:14) = 'YYYYMMDDHHMMSS'        
                        MOVE YYYYMMDDHHMMSS TO DNLOAD-RECORD(SUB:14)    
                        REWRITE DNLOAD-RECORD                           
                        SET RECORD-UPDATED TO TRUE                      
                     END-IF                                             
                     IF SUB = 60                                        
                        SET END-OF-RECORD TO TRUE                       
                     END-IF                                             
                 END-PERFORM                                            
              END-IF                                                    
           END-PERFORM                                                  
                                                                        
           STOP RUN.                                                    
                                                                        
