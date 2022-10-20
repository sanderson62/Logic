       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.    FNB170.                                           
       AUTHOR.        PABLO.
062403******************************************************************
062403*                   C H A N G E   L O G
062403*
062403* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
062403*-----------------------------------------------------------------
062403*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
062403* EFFECTIVE    NUMBER
062403*-----------------------------------------------------------------
111307* 111307    2007110500002  PEMA  NEW PROGRAM
062403******************************************************************
                                                                        
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT TRANSACTION-REPORT                                    
               ASSIGN TO SYS007.                                        
                                                                        
           SELECT TRANSACTION-DETAIL                                    
               ASSIGN TO SYS010
               ORGANIZATION IS LINE SEQUENTIAL
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
       01  FILLER           PIC X(255).

       SD  SORT-FILE.                                                   
       01  SORT-RECORD.                                                 
           COPY FNC022184.

       WORKING-STORAGE SECTION.                                         
                                                                        
       01  FILLER.                                                      
           05  SYS010-STATUS        PIC XX    VALUE '00'.               
               88  EOF                        VALUE '10'.               
           05  S0C7                 PIC X     VALUE ' '.                
           05  FORCE-DUMP REDEFINES S0C7 PIC S9    COMP-3.              
           05  PREV-TRAN-TYPE       PIC XX    VALUE SPACE.              
           05  PREV-SUB-TYPE        PIC XX    VALUE SPACE.              
                                                                        
       01  FILLER                 COMP-3.                               
           05  LINE-COUNT         PIC S9(3)      VALUE ZERO.            
           05  PAGE-COUNT         PIC S9(5)      VALUE ZERO.            
           05  INPUT-COUNT        PIC S9(7)      VALUE ZERO.            
               88  FIRST-RECORD                  VALUE +1.              
           05  TOTALS             OCCURS 3 TIMES.                       
               10  WS-COUNT       PIC S9(7)      COMP-3.                
               10  WS-DR-AMOUNT   PIC S9(11)V99  COMP-3.                
               10  WS-CR-AMOUNT   PIC S9(11)V99  COMP-3.                
                                                                        
062403 01  WS-NO-INPUT-MSG.
062403     05  FILLER             PIC X(01)      VALUE '0'.
062403     05  FILLER             PIC X(132)     VALUE
062403     'THERE WERE NO CLAIM RECORDS TO PROCESS'.

011604 01  HEADING-LINE-PGM-ID.                                              
011604     05  FILLER             PIC X(01)   VALUE '1'.
011604     05  FILLER             PIC X(54)   VALUE SPACES.
011604     05  HDG-CLIENT-ID      PIC X(03)   VALUE SPACES.
011604     05  FILLER             PIC X(01)   VALUE SPACE.
011604     05  HDG-FILE-ID        PIC X(06)   VALUE SPACES.
011604     05  FILLER             PIC X(62)   VALUE SPACES.
011604     05  HDG-PGM-ID         PIC X(06)   VALUE 'FNB170'.

       01  HEADING-LINE-1.                                              
pemuni     05  filler      pic x       value spaces.
           05  HDG-SYSTEM  PIC X(10)   VALUE SPACE.
           05  FILLER      PIC X(24)   VALUE SPACE.
           05  FILLER      PIC X(50)   VALUE                            
               'F R E E D O M   I N T E R F A C E   S U M M A R Y '.     
           05  FILLER      PIC X(24)   VALUE SPACE.                     
           05  HDG-DATE    PIC XX/XX/XXXX VALUE SPACE.                  
           05  FILLER      PIC X(8)    VALUE '   PAGE '.                
           05  HDG-PAGE    PIC ZZ,ZZ9  VALUE ZERO.                      
                                                                        
       01  HEADING-LINE-2.                                              
pemuni     05  filler      pic x       value spaces.
           05  FILLER      PIC X(11)   VALUE 'TRAN TYPE  '.             
           05  FILLER      PIC X(15)   VALUE 'SUB TYPE       '.         
           05  FILLER      PIC X(16)   VALUE 'DR AMOUNT       '.        
           05  FILLER      PIC X(22)   VALUE 'CR AMOUNT             '.  
           05  FILLER      PIC X(08)   VALUE 'NET     '.                
           05  FILLER      PIC X(07)   VALUE 'RECORDS'.                 
                                                                        
       01  HEADING-LINE-3.                                              
pemuni     05  filler      pic x       value spaces.
           05  FILLER      PIC X(11)   VALUE '---------  '.             
           05  FILLER      PIC X(10)   VALUE '--------  '.              
           05  FILLER      PIC X(16)   VALUE '--------------  '.        
           05  FILLER      PIC X(16)   VALUE '--------------  '.        
           05  FILLER      PIC X(17)   VALUE '--------------   '.       
           05  FILLER      PIC X(09)   VALUE '---------'.               
                                                                        
       01  PRINT-LINE             VALUE SPACE.                          
pemuni     05  filler      pic x.
           05  PRT-TRAN-TYPE      PIC X(9).                             
           05  FILLER             PIC X(2).                             
           05  PRT-SUB-TYPE       PIC X(8).                             
           05  FILLER             PIC X(2).                             
           05  PRT-DR-AMT         PIC ZZZ,ZZZ,ZZZ.99.                   
           05  FILLER             PIC XX.                               
           05  PRT-CR-AMT         PIC ZZZ,ZZZ,ZZZ.99.                   
           05  FILLER             PIC XX.                               
           05  PRT-NET-AMT        PIC ZZZ,ZZZ,ZZZ.99-.                  
           05  FILLER             PIC XX.                               
           05  PRT-COUNT          PIC Z,ZZZ,ZZ9.                        
                                                                        
011604 LINKAGE SECTION.
011604
011604 01  PARM.
011604     05  PARM-LENGTH         PIC S9(04)   COMP.
011604     05  PARM-CLIENT-ID      PIC X(03).
011604     05  PARM-FILE-ID        PIC X(06).

011604 PROCEDURE DIVISION USING PARM.                                              

       MAINLINE SECTION.                                                
                                                                        
           PERFORM 0000-HOUSEKEEPING THRU 0000-EXIT.                    
                                                                        
           SORT SORT-FILE                                               
             ON ASCENDING KEY FX-TRAN-TYPE, FX-SUB-TYPE                 
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
                                                                        
           GOBACK

           .
       1000-PRINT-REPORT.                                               

           RETURN SORT-FILE                                             
               AT END GO TO 1000-EXIT.                                  
           ADD +1 TO INPUT-COUNT.                                       
                                                                        
           IF FIRST-RECORD                                              
               MOVE FX-SYSTEM        TO HDG-SYSTEM                      
               MOVE FX-POSTING-DATE  TO HDG-DATE                        
               PERFORM 8000-HEADING THRU 8000-EXIT                      
               MOVE FX-TRAN-TYPE TO PREV-TRAN-TYPE                      
               MOVE FX-SUB-TYPE  TO PREV-SUB-TYPE.                      
                                                                        
           IF FX-TRAN-TYPE = PREV-TRAN-TYPE                             
               IF FX-SUB-TYPE = PREV-SUB-TYPE                           
                   CONTINUE                                             
               ELSE                                                     
                   PERFORM 2000-TOTAL-SUB-TYPE THRU 2000-EXIT           
           ELSE                                                         
               PERFORM 3000-TOTAL-TRAN-TYPE THRU 3000-EXIT.             
                                                                        
           ADD +1 TO WS-COUNT (1).                                      
           IF FX-AMOUNT IS NEGATIVE                                     
               ADD FX-AMOUNT TO WS-CR-AMOUNT (1)                        
           ELSE                                                         
               ADD FX-AMOUNT TO WS-DR-AMOUNT (1).                       
                                                                        
           MOVE FX-TRAN-TYPE TO PREV-TRAN-TYPE                          
           MOVE FX-SUB-TYPE  TO PREV-SUB-TYPE                           
           GO TO 1000-PRINT-REPORT

           .                                                                        
       1000-EXIT.                                                       
           EXIT.                                                        

       2000-TOTAL-SUB-TYPE.                                             

           MOVE PREV-SUB-TYPE    TO  PRT-SUB-TYPE                       
           MOVE WS-DR-AMOUNT (1) TO  PRT-DR-AMT.                        
           MOVE WS-CR-AMOUNT (1) TO  PRT-CR-AMT.                        
           ADD WS-CR-AMOUNT (1), WS-DR-AMOUNT (1) GIVING PRT-NET-AMT.   
           MOVE WS-COUNT (1)     TO  PRT-COUNT.                         
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
       2000-EXIT.                                                       
           EXIT.                                                        

       3000-TOTAL-TRAN-TYPE.                                            

           PERFORM 2000-TOTAL-SUB-TYPE THRU 2000-EXIT                   
           MOVE ' TOTAL'         TO  PRINT-LINE
           MOVE WS-DR-AMOUNT (2) TO  PRT-DR-AMT.                        
           MOVE WS-CR-AMOUNT (2) TO  PRT-CR-AMT.                        
           ADD WS-CR-AMOUNT (2), WS-DR-AMOUNT (2) GIVING PRT-NET-AMT.   
           MOVE WS-COUNT (2)     TO  PRT-COUNT.                         
           WRITE PRINT-RECORD FROM PRINT-LINE                           
               AFTER ADVANCING 1 LINE.                                  
           MOVE SPACES TO PRINT-RECORD                                  
           WRITE PRINT-RECORD                                           
               AFTER ADVANCING 1 LINE.                                  
           ADD +2 TO LINE-COUNT.                                        
           IF LINE-COUNT > +55                                          
               PERFORM 8000-HEADING THRU 8000-EXIT.                     
                                                                        
           ADD WS-COUNT     (2) TO WS-COUNT     (3)                     
           ADD WS-DR-AMOUNT (2) TO WS-DR-AMOUNT (3)                     
           ADD WS-CR-AMOUNT (2) TO WS-CR-AMOUNT (3)                     
           MOVE ZERO  TO  WS-COUNT     (2)                              
           MOVE ZERO  TO  WS-DR-AMOUNT (2)                              
           MOVE ZERO  TO  WS-CR-AMOUNT (2)                              
                                                                        
           MOVE FX-TRAN-TYPE TO PRT-TRAN-TYPE                           
           MOVE FX-SUB-TYPE  TO PRT-SUB-TYPE                            

           .                                                            
       3000-EXIT.                                                       
           EXIT.                                                        

       8000-HEADING.                                                    

           ADD +1 TO PAGE-COUNT.                                        
011604     WRITE PRINT-RECORD FROM HEADING-LINE-PGM-ID
082207         AFTER ADVANCING PAGE.                  
           MOVE PAGE-COUNT TO HDG-PAGE.                                 
           WRITE PRINT-RECORD FROM HEADING-LINE-1                       
011604         AFTER ADVANCING 1 LINE.                                    
           WRITE PRINT-RECORD FROM HEADING-LINE-2                       
               AFTER ADVANCING 4 LINES.                                 
           WRITE PRINT-RECORD FROM HEADING-LINE-3                       
               AFTER ADVANCING 1 LINE.                                  
           MOVE SPACES TO PRINT-RECORD.                                 
           WRITE PRINT-RECORD                                           
               AFTER ADVANCING 2 LINES.                                 
011604     MOVE +9 TO LINE-COUNT.                                       
                                                                        
           MOVE FX-TRAN-TYPE TO PRT-TRAN-TYPE                           
           MOVE FX-SUB-TYPE  TO PRT-SUB-TYPE

           .                                                                        
       8000-EXIT.                                                       
           EXIT.                                                        

       0000-HOUSEKEEPING.                                               

011604     MOVE PARM-CLIENT-ID TO HDG-CLIENT-ID.
011604     MOVE PARM-FILE-ID   TO HDG-FILE-ID.
           OPEN OUTPUT TRANSACTION-REPORT.                              
           INITIALIZE TOTALS (1) TOTALS (2) TOTALS (3)
           .                                                                        
       0000-EXIT.                                                       
           EXIT.                                                        

       9000-PRINT-GRAND-TOTALS.                                         

062403     IF INPUT-COUNT = ZERO                                        
062403         DISPLAY '*** INPUT FILE IS EMPTY ***'                    
062403         PERFORM 8000-HEADING THRU 8000-EXIT
062403         MOVE WS-NO-INPUT-MSG TO PRINT-RECORD
062403         WRITE PRINT-RECORD                                           
062403             AFTER ADVANCING 2 LINES
062403         CLOSE TRANSACTION-REPORT TRANSACTION-DETAIL
062403         GO TO 9000-EXIT
062403     END-IF

           PERFORM 3000-TOTAL-TRAN-TYPE THRU 3000-EXIT.                 
           MOVE ' GRAND TOTAL'   TO  PRINT-LINE
           MOVE WS-DR-AMOUNT (3) TO  PRT-DR-AMT                         
           MOVE WS-CR-AMOUNT (3) TO  PRT-CR-AMT                         
           MOVE WS-COUNT (3)     TO  PRT-COUNT                          
           ADD WS-CR-AMOUNT (3), WS-DR-AMOUNT (3) GIVING PRT-NET-AMT.   
           WRITE PRINT-RECORD FROM PRINT-LINE                           
               AFTER ADVANCING 3 LINES.                                 
                                                                        
           CLOSE TRANSACTION-REPORT TRANSACTION-DETAIL

           .                                                                        
       9000-EXIT.                                                       
           EXIT.                                                        
                                                                        
