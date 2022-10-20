       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. CIB009S.
       AUTHOR.     PABLO.
                                                                        
      ***************************************************************** 
      *                                                               * 
      *  THIS PROGRAM SEARCHES THE FIRST 10 LINES OF A REPORT LOOKING * 
      *  FOR THE VALUE IN THE PARAMETER.  IF THE PARM IS FOUND, THE   * 
      *  ENTIRE PAGE IS PRINTED.                                      * 
      *                                                               * 
      *  THE PROGRAM ALSO INSERTS BLANK LINES INSTEAD OF PRINTER      * 
      *  CARRIAGE CONTROL CHARACTERS.                                 * 
      *                                                               * 
      ***************************************************************** 
                                                                        
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT IN-FILE              ASSIGN TO SYS010.

           SELECT OUT-FILE1            ASSIGN TO SYS011.

           SELECT OUT-FILE2            ASSIGN TO SYS012.

           SELECT OUT-FILE3            ASSIGN TO SYS013.

           SELECT OUT-FILE4            ASSIGN TO SYS014.

       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD  IN-FILE
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  IN-REC.
           05  PRINT-CONTROL    PIC X.                                  
           05  FICHE-RECORD     PIC X(132).                             
                                                                        
       FD  OUT-FILE1
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  OUT-RECORD1                 PIC X(133).

       FD  OUT-FILE2
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  OUT-RECORD2                 PIC X(133).

       FD  OUT-FILE3
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  OUT-RECORD3                 PIC X(133).

       FD  OUT-FILE4
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  OUT-RECORD4                 PIC X(133).

       WORKING-STORAGE SECTION.                                         
                                                                        
       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-INPUT              VALUE 'Y'.
       01  BINARY.                                                      
           05  SUB         PIC S9(4)  VALUE +0.                         
           05  STRT        PIC S9(4)  VALUE +0.                         
           05  END-LINE    PIC S9(4)  VALUE +0.                         
                                                                        
       01  FILLER.                                                      
           05  WS-ZERO         PIC S999  COMP-3 VALUE +0.
           05  BLANK-LINE      PIC X     VALUE SPACE.                   
           05  S0C7            PIC X     VALUE SPACE.                   
           05  FORCE-DUMP REDEFINES S0C7 PIC S9 COMP-3.                 
           05  SYS010-STATUS   PIC XX    VALUE ZERO.                    
               88  EOF                   VALUE '10'.                    
           05  SYS011-STATUS   PIC XX    VALUE ZERO.                    
           05  IN-CNT          PIC S9(9) COMP-3 VALUE +0.               
           05  OUT-CNT         PIC S9(9) COMP-3 VALUE +0.               
           05  WS-CNTR         PIC S9(7) COMP-3 VALUE +0.
           05  WS-ON-FILE      PIC 9     VALUE 1.
           05  PRINT-SW        PIC X(3)  VALUE SPACE.
           05  WORKAREA.                                                
               10  SAVE-REC OCCURS 10 TIMES PIC X(133).                 

       LINKAGE SECTION.                                                 
                                                                        
       01  PARM.                                                        
           05  PARM-LENGTH BINARY  PICTURE IS S9(4).                    
           05  PARM-VALUE  DISPLAY PICTURE IS X(100).                   

       PROCEDURE DIVISION USING PARM.                                   

           PERFORM 0000-INIT           THRU 0000-EXIT
           PERFORM 0100-PROCESS        THRU 0100-EXIT
              UNTIL END-OF-INPUT
           PERFORM 0900-END-PROGRAM    THRU 0900-EXIT
           GOBACK

          .
       0000-INIT.                                                        
      *                                                                 
           OPEN INPUT IN-FILE                                        
                                                                        
           OPEN OUTPUT OUT-FILE1 OUT-FILE2 OUT-FILE3 OUT-FILE4
                                                                        
           COMPUTE END-LINE = LENGTH OF WORKAREA - PARM-LENGTH
           if parm-length > +0
              DISPLAY ' PARM LENGTH = ' PARM-LENGTH
              
              inspect parm-value replacing
                 all '-' by ' '
           end-if
           PERFORM 0050-READ-INPUT     THRU 0050-EXIT

           .                                                            
       0000-EXIT.                                                        
           EXIT.                                                        
                                                                        
       0050-READ-INPUT.                                                  

           READ IN-FILE AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD +1                   TO IN-CNT
                                          WS-CNTR
           END-IF                                                             

           .                                                            
       0050-EXIT.                                                        
           EXIT.                                                        

       0100-PROCESS.                                                     

           IF IN-REC (1:PARM-LENGTH) = PARM-VALUE (1:PARM-LENGTH)
              IF WS-CNTR > +88220
                 MOVE ZEROS            TO WS-CNTR
                 ADD 1                 TO WS-ON-FILE
              END-IF
           END-IF

           EVALUATE TRUE
              WHEN WS-ON-FILE = 1
                 WRITE OUT-RECORD1     FROM IN-REC
              WHEN WS-ON-FILE = 2
                 WRITE OUT-RECORD2     FROM IN-REC
              WHEN WS-ON-FILE = 3
                 WRITE OUT-RECORD3     FROM IN-REC
              WHEN WS-ON-FILE = 4
                 WRITE OUT-RECORD4     FROM IN-REC
              WHEN OTHER
                 DISPLAY ' PROBLEM WITH ON FILE ' WS-ON-FILE
                 CALL 'DUMMY'
           END-EVALUATE

           ADD +1                      TO OUT-CNT

           PERFORM 0050-READ-INPUT     THRU 0050-EXIT

           .                                                            
       0100-EXIT.                                                        
           EXIT.                                                        
                                                                        
                                                                        
       0900-END-PROGRAM.                                                 

           CLOSE IN-FILE OUT-FILE1 OUT-FILE2 OUT-FILE3 OUT-FILE4
           DISPLAY 'RECORDS IN....' IN-CNT
           DISPLAY 'RECORDS OUT...' OUT-CNT

           .                                                            
       0900-EXIT.                                                        
           EXIT.                                                        
                                                                        
