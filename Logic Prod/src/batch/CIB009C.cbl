       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. CIB009C.
       AUTHOR.     DAN DRYDEN.                                          
                                                                        
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
                                                                        
           SELECT OPTIONAL FICHE-FILE                                   
               ASSIGN TO SYS010                                         
               FILE STATUS IS SYS010-STATUS.                            
                                                                        
           SELECT REPORT-FILE                                           
               ASSIGN TO SYS011
               FILE STATUS IS SYS011-STATUS.                            
                                                                        
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD  FICHE-FILE                                                   
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  FICHE-REC.                                                   
           05  PRINT-CONTROL    PIC X.                                  
           05  FICHE-RECORD     PIC X(132).                             
                                                                        
       FD  REPORT-FILE                                                  
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  REPORT-RECORD    PIC X(133).                                 
                                                                        
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
       01  BINARY.                                                      
           05  SUB         PIC S9(4)  VALUE +0.                         
           05  STRT        PIC S9(4)  VALUE +0.                         
           05  END-LINE    PIC S9(4)  VALUE +0.                         
                                                                        
       01  FILLER.                                                      
           05  BLANK-LINE      PIC X     VALUE SPACE.                   
           05  S0C7            PIC X     VALUE SPACE.                   
           05  FORCE-DUMP REDEFINES S0C7 PIC S9 COMP-3.                 
           05  SYS010-STATUS   PIC XX    VALUE ZERO.                    
               88  EOF                   VALUE '10'.                    
           05  SYS011-STATUS   PIC XX    VALUE ZERO.                    
           05  IN-CNT          PIC S9(9) COMP-3 VALUE +0.               
           05  OUT-CNT         PIC S9(9) COMP-3 VALUE +0.               
           05  PRINT-SW        PIC X(3)  VALUE SPACE.                   
           05  WORKAREA.                                                
               10  SAVE-REC OCCURS 10 TIMES PIC X(133).                 
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       LINKAGE SECTION.                                                 
                                                                        
       01  PARM.                                                        
           05  PARM-LENGTH BINARY  PICTURE IS S9(4).                    
           05  PARM-VALUE  DISPLAY PICTURE IS X(100).                   
                                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       PROCEDURE DIVISION USING PARM.                                   
      *                                                                 
           PERFORM 000-INIT THRU 000-EXIT                               
           PERFORM 100-PROCESS THRU 100-EXIT                            
               UNTIL EOF                                                
           PERFORM 900-END-PROGRAM THRU 900-EXIT                        
           STOP RUN.                                                    
                                                                        
                                                                        
      /                                                                 
       000-INIT.                                                        
      *                                                                 
           OPEN INPUT FICHE-FILE                                        
           IF SYS010-STATUS NOT = '00'                                  
              DISPLAY 'OPEN ERROR ' SYS010-STATUS ' ON SYS010'          
              MOVE '10' TO SYS010-STATUS                                
           END-IF                                                       
                                                                        
           OPEN OUTPUT REPORT-FILE                                      
                                                                        
           COMPUTE END-LINE = LENGTH OF WORKAREA - PARM-LENGTH
           if parm-length > +0
              inspect parm-value replacing
                 all '-' by ' '
           end-if

           .                                                            
       000-EXIT.                                                        
           EXIT.                                                        
                                                                        
       100-PROCESS.                                                     
      *                                                                 
           READ FICHE-FILE                                              
                AT END GO TO 100-EXIT.                                  
           ADD +1 TO IN-CNT                                             
                                                                        
           IF PRINT-CONTROL = '1'                                       
              PERFORM 200-NEW-PAGE THRU 200-EXIT.                       
                                                                        
           IF PRINT-SW = 'YES'                                          
              WRITE REPORT-RECORD FROM FICHE-REC                     
              ADD +1 TO OUT-CNT                                         
           END-IF                                                       
           .                                                            
       100-EXIT.                                                        
           EXIT.                                                        
                                                                        
                                                                        
       200-NEW-PAGE.                                                    
      *                                                                 
           IF PARM-LENGTH = +0                                          
              MOVE 'YES' TO PRINT-SW                                    
              GO TO 200-EXIT.                                           
                                                                        
           IF PARM-VALUE(1:PARM-LENGTH) = 'ALL'                         
              MOVE 'YES' TO PRINT-SW                                    
              GO TO 200-EXIT.                                           
                                                                        
           MOVE 'NO' TO PRINT-SW                                        
                                                                        
           MOVE SPACES     TO WORKAREA                                  
           MOVE FICHE-REC  TO SAVE-REC(1)                               
           PERFORM VARYING SUB FROM 2 BY 1                              
             UNTIL SUB > 10 OR EOF                                      
              READ FICHE-FILE INTO SAVE-REC(SUB)                        
           END-PERFORM                                                  
                                                                        
           PERFORM VARYING STRT FROM 1 BY 1                             
             UNTIL PRINT-SW = 'YES' OR STRT > END-LINE                  
                IF WORKAREA(STRT:PARM-LENGTH) =                         
                   PARM-VALUE(1:PARM-LENGTH)                            
                      MOVE 'YES' TO PRINT-SW                            
                END-IF                                                  
           END-PERFORM                                                  
                                                                        
           IF PRINT-SW = 'YES'                                          
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 9             
                WRITE REPORT-RECORD FROM SAVE-REC(SUB)           
                ADD +1 TO OUT-CNT                                       
              END-PERFORM                                               
           END-IF                                                       
           .                                                            
       200-EXIT.                                                        
           EXIT.                                                        
                                                                        
                                                                        
      /                                                                 
       900-END-PROGRAM.                                                 
      *                                                                 
           CLOSE REPORT-FILE                                            
           DISPLAY 'RECORDS IN....' IN-CNT                              
           DISPLAY 'RECORDS OUT...' OUT-CNT                             
           .                                                            
       900-EXIT.                                                        
           EXIT.                                                        
                                                                        
