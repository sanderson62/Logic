       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.    cidls.
       AUTHOR.        Pablo.
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
      *                                                                 
           SELECT CID-FILE-IN
               ASSIGN TO SYS010                                         
               FILE STATUS IS SYS010-STATUS.                            
                                                                        
                                                                        
           SELECT CID-FILE-OUT
               ASSIGN TO SYS011
pemuni         organization is line sequential.
                                                                        
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD  CID-FILE-IN
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  CID-RECORD-IN               PIC X(80).
                                                                        
       FD  CID-FILE-OUT
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  CID-RECORD-OUT              PIC X(80).
                                                                        
       EJECT                                                            
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
       01  FILLER                  BINARY.                              
           05  SUB                 PIC S9(4)    VALUE +0.               
                                                                        
       01  FILLER.                                                      
           05  S0C7                PIC X        VALUE ' '.              
           05  FORCE-DUMP REDEFINES S0C7 PIC S9 COMP-3.                 
           05  SYS010-STATUS       PIC XX       VALUE '00'.             
               88  EOF                          VALUE '10'.             
           EJECT                                                        
      *                                                                 
       PROCEDURE DIVISION.
      *                                                                 
           PERFORM 0000-HOUSEKEEPING THRU 0000-EXIT                     
                                                                        
           PERFORM 1000-PROCESS THRU 1000-EXIT UNTIL EOF
                                                                        
           CLOSE CID-FILE-IN   CID-FILE-OUT
           GOBACK
                                                                        
                                                                        
           EJECT                                                        
           .
       1000-PROCESS.                                                    
      *                                                                 
           READ CID-FILE-IN
              AT END GO TO 1000-EXIT.                                   
                                                                        
           WRITE CID-RECORD-OUT FROM CID-RECORD-IN
           .
       1000-EXIT.                                                       
           EXIT.                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
      *                                                                 
       0000-HOUSEKEEPING.                                               
      *                                                                 
                                                                        
           OPEN INPUT CID-FILE-IN
           IF SYS010-STATUS NOT = '00'                                  
              DISPLAY 'OPEN ERROR ' SYS010-STATUS ' ON SYS010'          
              ADD +1 TO FORCE-DUMP.                                     
                                                                        
                                                                        
           OPEN OUTPUT CID-FILE-OUT
           .                                                            
       0000-EXIT.                                                       
           EXIT.                                                        
                                                                        
