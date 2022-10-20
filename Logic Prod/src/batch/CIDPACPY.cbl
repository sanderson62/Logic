       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.    CIDPACPY.
       AUTHOR.        Pablo.
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
      *                                                                 
           SELECT CID-FILE-IN
               ASSIGN TO SYS010                                         
               FILE STATUS IS SYS010-STATUS
               organization is line sequential.
                                                                        
                                                                        
           SELECT CID-FILE-OUT
               ASSIGN TO SYS011.
                                                                        
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD  CID-FILE-IN
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  CID-RECORD-IN               PIC X(800).
                                                                        
       FD  CID-FILE-OUT
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  CID-RECORD-OUT              PIC X(800).
                                                                        
       EJECT                                                            
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
       01  FILLER                  BINARY.                              
           05  SUB                 PIC S9(4)    VALUE +0.               
                                                                        
       01  vend-record-in.
           05  vend-byte  occurs 800   pic x.

       01  FILLER.
           05  sub1                pic s9(3) comp-3 value +0.
           05  cid-in-cnt          pic 9(5) value zeros.
           05  cid-out-cnt         pic 9(5) value zeros.
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
           display ' RECORDS IN   ' cid-in-cnt
           display ' RECORDS OUT  ' cid-out-cnt
UNIX       CLOSE CID-FILE-IN  CID-FILE-OUT
           GOBACK
                                                                        
                                                                        
           EJECT                                                        
           .
       1000-PROCESS.                                                    
      *                                                                 
           READ CID-FILE-IN into vend-record-in
              AT END GO TO 1000-EXIT.
           add 1 to cid-in-cnt
           perform varying sub1 from +1 by +1 until
              sub1 > +800
              if vend-byte (sub1) = low-values
                 move spaces to vend-byte (sub1)
              end-if
           end-perform
           WRITE CID-RECORD-OUT FROM vend-record-in
           add 1 to cid-out-cnt
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
                                                                        
