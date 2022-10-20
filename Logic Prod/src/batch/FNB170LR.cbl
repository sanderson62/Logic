       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.    FNB170LR.                                           
                                                                        
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT TRANSACTION-REPORT                                    
               ASSIGN TO SYS007.                                        
                                                                        
           SELECT TRANSACTION-DETAIL                                    
               ASSIGN TO SYS010
pemuni         organization is line sequential
               FILE STATUS IS SYS010-STATUS.                            
                                                                        
           SELECT SORT-FILE                                             
               ASSIGN TO SORTWK1.                                       
                                                                        
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD  TRANSACTION-REPORT                                           
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
pemuni*    RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.                                    
pemuni 01  PRINT-RECORD     PIC X(133).
                                                                        
       FD  TRANSACTION-DETAIL                                           
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  FILLER           PIC X(250).                                 
                                                                        
       SD  SORT-FILE.                                                   
       01  SORT-RECORD.                                                 
           COPY FNC022.                                                 
                                                                        
                                                                        
                                                                        
      /                                                                 
       WORKING-STORAGE SECTION.                                         
                                                                        
       01  FILLER.                                                      
           05  SYS010-STATUS        PIC XX    VALUE '00'.               
               88  EOF                        VALUE '10'.               
           05  S0C7                 PIC X     VALUE ' '.                
           05  FORCE-DUMP REDEFINES S0C7 PIC S9    COMP-3.              
           05  PREV-TRAN-TYPE       PIC XX    VALUE SPACE.              
           05  PREV-SUB-TYPE        PIC XX    VALUE SPACE.              
           05  PREV-STATE           PIC XX    VALUE '  '.
                                                                        
       01  FILLER                 COMP-3.                               
           05  LINE-COUNT         PIC S9(3)      VALUE ZERO.            
           05  PAGE-COUNT         PIC S9(5)      VALUE ZERO.            
           05  INPUT-COUNT        PIC S9(7)      VALUE ZERO.            
               88  FIRST-RECORD                  VALUE +1.              
           05  TOTALS             OCCURS 4 TIMES.                       
               10  WS-COUNT       PIC S9(7)      COMP-3.                
               10  WS-DR-AMOUNT   PIC S9(11)V99  COMP-3.                
               10  WS-CR-AMOUNT   PIC S9(11)V99  COMP-3.                
                                                                        
       01  HEADING-LINE-1.                                              
pemuni     05  filler      pic x       value spaces.
           05  HDG-SYSTEM  PIC X(10)   VALUE SPACE.
           05  FILLER      PIC X(24)   VALUE SPACE.
           05  FILLER      PIC X(50)   VALUE                            
               'F R E E D O M   I N T E R F A C E   S U M M A R Y'.     
           05  FILLER      PIC X(24)   VALUE SPACE.                     
           05  HDG-DATE    PIC XX/XX/XXXX VALUE SPACE.                  
           05  FILLER      PIC X(8)    VALUE '   PAGE '.                
           05  HDG-PAGE    PIC ZZ,ZZ9  VALUE ZERO.                      
                                                                        
       01  HEADING-LINE-2.                                              
pemuni     05  filler      pic x       value spaces.
           05  FILLER      PIC X(11)   VALUE 'TRAN TYPE  '.             
           05  FILLER      PIC X(10)   VALUE 'SUB TYPE  '.
           05  FILLER      PIC X(5)    VALUE 'STATE'.
           05  FILLER      PIC X(16)   VALUE '   DR AMOUNT    '.
           05  FILLER      PIC X(22)   VALUE '   CR AMOUNT          '.
           05  FILLER      PIC X(08)   VALUE 'NET     '.
           05  FILLER      PIC X(07)   VALUE 'RECORDS'.                 
                                                                        
       01  HEADING-LINE-3.                                              
pemuni     05  filler      pic x       value spaces.
           05  FILLER      PIC X(11)   VALUE '---------  '.             
      *                                          XX
           05  FILLER      PIC X(10)   VALUE '--------  '.
      *                                          XX
           05  FILLER      PIC X(5)    VALUE '-----'.              
      *                                         XX
           05  FILLER      PIC X(16)   VALUE '   ---------    '.        
      *                                        XXXXXXXXXXXXX                                            
           05  FILLER      PIC X(22)   VALUE '---------             '.        
      *                                        XXXXXXXXXXXXX
           05  FILLER      PIC X(08)   VALUE '---     '.                
           05  FILLER      PIC X(07)   VALUE '-------'.               
                                                                        
       01  PRINT-LINE             VALUE SPACE.                          
pemuni     05  filler      pic x.
           05  FILLER             PIC XXX.
           05  PRT-TRAN-TYPE      PIC XX.                             
           05  FILLER             PIC X(9).                             
           05  PRT-SUB-TYPE       PIC XX.                             
           05  FILLER             PIC X(7).                             
           05  PRT-STATE          PIC XX.
           05  FILLER             PIC XX.                               
           05  PRT-DR-AMT         PIC ZZZ,ZZZ,ZZZ.99.                   
           05  FILLER             PIC XXX.                              
           05  PRT-CR-AMT         PIC ZZZ,ZZZ,ZZZ.99.                   
           05  FILLER             PIC X(8).                             
           05  PRT-NET-AMT        PIC ZZZ,ZZZ,ZZZ.99-.                  
           05  FILLER             PIC XX.                               
           05  PRT-COUNT          PIC Z,ZZZ,ZZ9.                        
                                                                        
                                                                        
                                                                        
      /                                                                 
       PROCEDURE DIVISION.                                              
      *                                                                 
       MAINLINE SECTION.                                                
                                                                        
           PERFORM 0000-HOUSEKEEPING THRU 0000-EXIT.                    
                                                                        
           SORT SORT-FILE                                               
             ON ASCENDING KEY FX-TRAN-TYPE, FX-SUB-TYPE, FX-STATE                 
               USING TRANSACTION-DETAIL                                 
               OUTPUT PROCEDURE IS 1000-PRINT-REPORT THRU 1000-EXIT.    
                                                                        
           IF SORT-RETURN NOT = ZERO                                    
               DISPLAY ' '                                              
               DISPLAY ' '                                              
               DISPLAY 'INTERNAL SORT IN PROGRAM FNB170 FAILED'         
               DISPLAY 'TRY INCREASING SORTWORK SPACE'                  
               DISPLAY 'JOB CANCELLED...'                               
               ADD +1 TO FORCE-DUMP.                                    
                                                                        
           PERFORM 9000-PRINT-GRAND-TOTALS THRU 9000-EXIT.              
                                                                        
           GOBACK.
                                                                        
                                                                        
                                                                        
      /                                                                 
       1000-PRINT-REPORT.                                               
      *                                                                 
           RETURN SORT-FILE                                             
               AT END GO TO 1000-EXIT.                                  
           ADD +1 TO INPUT-COUNT.                                       
                                                                        
           IF FIRST-RECORD                                              
               MOVE FX-SYSTEM          TO HDG-SYSTEM                      
               MOVE FX-POSTING-DATE    TO HDG-DATE                        
               PERFORM 8000-HEADING    THRU 8000-EXIT                      
               MOVE FX-TRAN-TYPE       TO PREV-TRAN-TYPE                      
               MOVE FX-SUB-TYPE        TO PREV-SUB-TYPE
               MOVE FX-STATE           TO PREV-STATE
            END-IF
                                                                        
           IF FX-TRAN-TYPE             = PREV-TRAN-TYPE                             
              IF FX-SUB-TYPE           = PREV-SUB-TYPE                           
                 IF FX-STATE           = PREV-STATE
                    CONTINUE
                 ELSE
                    PERFORM 1500-TOTAL-STATE THRU 1500-EXIT
                 END-IF
              ELSE                                                     
                 PERFORM 2000-TOTAL-SUB-TYPE THRU 2000-EXIT
              END-IF
           ELSE                                                         
              PERFORM 3000-TOTAL-TRAN-TYPE THRU 3000-EXIT
           END-IF
                                                                        
           ADD +1                      TO WS-COUNT (1)

           IF FX-AMOUNT IS NEGATIVE                                     
              ADD FX-AMOUNT            TO WS-CR-AMOUNT (1)                        
           ELSE                                                         
              ADD FX-AMOUNT            TO WS-DR-AMOUNT (1)
           END-IF
                                                                        
           MOVE FX-TRAN-TYPE           TO PREV-TRAN-TYPE                          
           MOVE FX-SUB-TYPE            TO PREV-SUB-TYPE
           MOVE FX-STATE               TO PREV-STATE
           GO TO 1000-PRINT-REPORT.                                     
                                                                        
       1000-EXIT.                                                       
           EXIT.                                                        
                                                                        
       1500-TOTAL-STATE.
      *                                                                 
           MOVE PREV-STATE             TO  PRT-STATE
           MOVE WS-DR-AMOUNT (1)       TO  PRT-DR-AMT.                        
           MOVE WS-CR-AMOUNT (1)       TO  PRT-CR-AMT.                        
           ADD WS-CR-AMOUNT (1), WS-DR-AMOUNT (1) GIVING PRT-NET-AMT.   
           MOVE WS-COUNT (1)           TO  PRT-COUNT.                         
           WRITE PRINT-RECORD FROM PRINT-LINE                           
               AFTER ADVANCING 1 LINE.                                  
           ADD +1 TO LINE-COUNT.                                        
           IF LINE-COUNT > +55                                          
               PERFORM 8000-HEADING THRU 8000-EXIT.                     
                                                                        
           ADD WS-COUNT (1)     TO  WS-COUNT (2)                        
           ADD WS-DR-AMOUNT (1) TO  WS-DR-AMOUNT (2)                    
           ADD WS-CR-AMOUNT (1) TO  WS-CR-AMOUNT (2)                    
           MOVE ZERO  TO  WS-COUNT (1)                                  
           MOVE ZERO  TO  WS-DR-AMOUNT (1)                              
           MOVE ZERO  TO  WS-CR-AMOUNT (1)                              
           MOVE SPACES TO PRINT-LINE                                    
           .                                                            
       1500-EXIT.                                                       
           EXIT.                                                        
                                                                        
                                                                        
      /                                                                 
                                                                        
       2000-TOTAL-SUB-TYPE.                                             
      *                                                                 
           PERFORM 1500-TOTAL-STATE    THRU 1500-EXIT
           MOVE PREV-SUB-TYPE          TO  PRT-SUB-TYPE                       
           MOVE WS-DR-AMOUNT (2)       TO  PRT-DR-AMT.                        
           MOVE WS-CR-AMOUNT (2)       TO  PRT-CR-AMT.                        
           ADD WS-CR-AMOUNT (2), WS-DR-AMOUNT (2) GIVING PRT-NET-AMT.   
           MOVE WS-COUNT (2)           TO  PRT-COUNT.                         
           WRITE PRINT-RECORD FROM PRINT-LINE                           
               AFTER ADVANCING 1 LINE.                                  
           ADD +1 TO LINE-COUNT.                                        
           IF LINE-COUNT > +55                                          
               PERFORM 8000-HEADING THRU 8000-EXIT.                     
                                                                        
           ADD WS-COUNT (2)     TO  WS-COUNT (3)                        
           ADD WS-DR-AMOUNT (2) TO  WS-DR-AMOUNT (3)                    
           ADD WS-CR-AMOUNT (2) TO  WS-CR-AMOUNT (3)                    
           MOVE ZERO  TO  WS-COUNT (2)                                  
           MOVE ZERO  TO  WS-DR-AMOUNT (2)                              
           MOVE ZERO  TO  WS-CR-AMOUNT (2)                              
           MOVE SPACES TO PRINT-LINE                                    
           .                                                            
       2000-EXIT.                                                       
           EXIT.                                                        
                                                                        
                                                                        
      /                                                                 
       3000-TOTAL-TRAN-TYPE.                                            
      *                                                                 
           PERFORM 2000-TOTAL-SUB-TYPE THRU 2000-EXIT                   
           MOVE ' TOTAL'         TO  PRINT-LINE
           MOVE WS-DR-AMOUNT (3) TO  PRT-DR-AMT.                        
           MOVE WS-CR-AMOUNT (3) TO  PRT-CR-AMT.                        
           ADD WS-CR-AMOUNT (3), WS-DR-AMOUNT (3) GIVING PRT-NET-AMT.   
           MOVE WS-COUNT (3)     TO  PRT-COUNT.                         
           WRITE PRINT-RECORD FROM PRINT-LINE                           
               AFTER ADVANCING 1 LINE.                                  
           MOVE SPACES TO PRINT-RECORD                                  
           WRITE PRINT-RECORD                                           
               AFTER ADVANCING 1 LINE.                                  
           ADD +2 TO LINE-COUNT.                                        
           IF LINE-COUNT > +55                                          
               PERFORM 8000-HEADING THRU 8000-EXIT.                     
                                                                        
           ADD WS-COUNT     (3) TO WS-COUNT     (4)                     
           ADD WS-DR-AMOUNT (3) TO WS-DR-AMOUNT (4)                     
           ADD WS-CR-AMOUNT (3) TO WS-CR-AMOUNT (4)                     
           MOVE ZERO  TO  WS-COUNT     (3)                              
           MOVE ZERO  TO  WS-DR-AMOUNT (3)                              
           MOVE ZERO  TO  WS-CR-AMOUNT (3)                              
                                                                        
           MOVE FX-TRAN-TYPE TO PRT-TRAN-TYPE                           
           MOVE FX-SUB-TYPE  TO PRT-SUB-TYPE                            
           .                                                            
       3000-EXIT.                                                       
           EXIT.                                                        
                                                                        
                                                                        
      /                                                                 
       8000-HEADING.                                                    
      *                                                                 
           ADD +1 TO PAGE-COUNT.                                        
           MOVE PAGE-COUNT TO HDG-PAGE.                                 
           WRITE PRINT-RECORD FROM HEADING-LINE-1                       
               AFTER ADVANCING PAGE.                                    
           WRITE PRINT-RECORD FROM HEADING-LINE-2                       
               AFTER ADVANCING 4 LINES.                                 
           WRITE PRINT-RECORD FROM HEADING-LINE-3                       
               AFTER ADVANCING 1 LINE.                                  
           MOVE SPACES TO PRINT-RECORD.                                 
           WRITE PRINT-RECORD                                           
               AFTER ADVANCING 2 LINES.                                 
           MOVE +8 TO LINE-COUNT.                                       
                                                                        
           MOVE FX-TRAN-TYPE TO PRT-TRAN-TYPE                           
           MOVE FX-SUB-TYPE  TO PRT-SUB-TYPE.                           
                                                                        
       8000-EXIT.                                                       
           EXIT.                                                        
                                                                        
                                                                        
                                                                        
      /                                                                 
       0000-HOUSEKEEPING.                                               
      *                                                                 
           OPEN OUTPUT TRANSACTION-REPORT.                              
           INITIALIZE TOTALS (1) TOTALS (2) TOTALS (3)
                      TOTALS (4).
                                                                        
       0000-EXIT.                                                       
           EXIT.                                                        
                                                                        
                                                                        
      /                                                                 
       9000-PRINT-GRAND-TOTALS.                                         
      *                                                                 
           PERFORM 3000-TOTAL-TRAN-TYPE THRU 3000-EXIT.                 
           MOVE ' GRAND TOTAL'   TO  PRINT-LINE
           MOVE WS-DR-AMOUNT (4) TO  PRT-DR-AMT                         
           MOVE WS-CR-AMOUNT (4) TO  PRT-CR-AMT                         
           MOVE WS-COUNT (4)     TO  PRT-COUNT                          
           ADD WS-CR-AMOUNT (4), WS-DR-AMOUNT (4) GIVING PRT-NET-AMT.   
           WRITE PRINT-RECORD FROM PRINT-LINE                           
               AFTER ADVANCING 3 LINES.                                 
                                                                        
           CLOSE TRANSACTION-REPORT TRANSACTION-DETAIL.                                    
           IF INPUT-COUNT = ZERO                                        
               DISPLAY '*** INPUT FILE IS EMPTY ***'                    
pemmod***      MOVE +04 TO RETURN-CODE                                  
DAN*******     ADD +1 TO FORCE-DUMP.                                    
           END-IF.                                                      
                                                                        
       9000-EXIT.                                                       
           EXIT.                                                        
                                                                        
