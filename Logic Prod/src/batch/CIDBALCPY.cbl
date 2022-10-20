       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.    cidlgcpy.
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
       01  CID-RECORD-IN               PIC X(172).
                                                                        
       FD  CID-FILE-OUT
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  CID-RECORD-OUT              PIC X(174).
                                                                        
       EJECT                                                            
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
       01  FILLER                  BINARY.                              
           05  SUB                 PIC S9(4)    VALUE +0.               
                                                                        
       01  BALLARD-REC.
           05  FILLER                      PIC X(131).
           05  BAL-EFFDT                   PIC X(8).
           05  FILLER                      PIC X(33).

       01  BALLARD-REC-OUT.
           05  FILLER                      PIC X(131).
           05  BAL-EFFDTO                  PIC X(10).
           05  FILLER                      PIC X(33).

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
           display ' records in   ' cid-in-cnt
           display ' records out  ' cid-out-cnt
           CLOSE CID-FILE-IN   CID-FILE-OUT
           GOBACK
                                                                        
                                                                        
           EJECT                                                        
           .
       1000-PROCESS.                                                    
      *                                                                 
           READ CID-FILE-IN
            INTO BALLARD-REC
              AT END GO TO 1000-EXIT.
           add 1 to cid-in-cnt
           MOVE BALLARD-REC (1:137) TO BALLARD-REC-OUT (1:137)
           MOVE '19'                TO BAL-EFFDTO (7:2)
           MOVE BAL-EFFDT (7:2)     TO BAL-EFFDTO (9:2)
           IF BAL-EFFDT (7:2) < '30'
              MOVE '20'          TO BAL-EFFDTO (7:2)
           END-IF
           MOVE BALLARD-REC (140:33) TO BALLARD-REC-OUT (142:33)

           WRITE CID-RECORD-OUT FROM BALLARD-REC-OUT
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
                                                                        
