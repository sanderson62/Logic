       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID. CIDFGRF.                                                     
       AUTHOR.     PABLO                                                        
       DATE-COMPILED.                                                           
       ENVIRONMENT DIVISION.                                                    
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
                                                                                
           SELECT EXTR-IN          ASSIGN TO EXTRIN.                              
           SELECT DISK-DATE        ASSIGN TO SYS019.
           SELECT EXTR-OUT         ASSIGN TO EXTROT                              
                                   ORGANIZATION IS LINE SEQUENTIAL.    

           SELECT ERACCT           ASSIGN TO ERACCTT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AM-CONTROL-PRIMARY
                                   FILE STATUS IS ERACCT-FILE-STATUS.

       EJECT                                                                    
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
                                                                                
       FD  EXTR-IN                                                              
           RECORDING MODE F                                                     
           LABEL RECORDS STANDARD                                               
           BLOCK CONTAINS 0 RECORDS.                                            
                                                                                
       01  EXTR-RECORD-IN.
           05  EXTRACT-REPORT-CODE-1   PIC X(10).
           05  EXTRACT-ACCT-KEY        PIC X(19).
           05  EXTRACT-DATA            PIC X(61).

       FD  DISK-DATE                                                    
                                       COPY ELCDTEFD.                   

       FD  EXTR-OUT                                                             
           RECORDING MODE F                                                     
           LABEL RECORDS STANDARD                                               
           BLOCK CONTAINS 0 RECORDS.                                            
       01  EXTR-RECORD-OUT             PIC X(90).

       FD  ERACCT.

           COPY ERCACCT.

       EJECT                                                                    
       WORKING-STORAGE SECTION.                                                 
       77  FILLER  PIC X(32) VALUE '********************************'.          
       77  FILLER  PIC X(32) VALUE '   CIDFGRF WORKING-STORAGE    '.          
       77  FILLER  PIC X(32) VALUE '********************************'.          
                                                                                
       77  WS-EXTR-EOF             PIC X VALUE SPACES.                          
           88  END-OF-EXTRS              VALUE 'Y'.                             
       77  WS-ACCT-EOF             PIC X VALUE SPACES.                          
           88  END-OF-ERACCT             VALUE 'Y'.                             
       77  WS-ABEND-FLD            PIC 9         VALUE ZEROS.
       77  WS-ZERO-FLD             PIC 9         VALUE ZEROS.
       77  ERACCT-FILE-STATUS      PIC XX   VALUE '00'.
       77  WS-ERACCT-RECS-IN       PIC 9(9) VALUE ZEROS.
       77  EXTR-RECS-IN            PIC 9(9) VALUE ZEROS.                        
       77  EXTR-RECS-OUT           PIC 9(9) VALUE ZEROS.                        
       77  EXTR-RECS-FIX           PIC 9(9) VALUE ZEROS.                        
       77  EXTR-CO-FIX             PIC 9(9) VALUE ZEROS.                        
       77  SUB1                    PIC S999 VALUE +0 COMP-3.
       77  PGM-SUB                 PIC S999  COMP   VALUE +035.
       77  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.      
       77  WS-ZERO                 PIC S9          VALUE ZERO.      
       77  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.    
       77  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.      
       77  WS-PREV-EQUAL-SW        PIC X    VALUE 'N'.
           88  PREVIOUSLY-EQUAL             VALUE 'Y'.
       01  SAVE-REPORT-CODE-1      PIC X(10)  VALUE ZEROS.
                                       COPY ELCDTECX.                   

                                       COPY ELCDTEVR.                   

                                                                                
       PROCEDURE DIVISION.                                                      
                                                                                
                                   COPY ELCDTERX.
                                   
       0000-MAIN.                                                               
                                                                                
           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT                               
                                                                                
           PERFORM 0600-INITIALIZE     THRU 0600-EXIT                               
                                                                                
           PERFORM 0100-PROCESS-EXTR   THRU 0100-EXIT UNTIL                       
                 END-OF-EXTRS
                                                                                
           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT                              
                                                                                
           DISPLAY ' EXTR RECORDS READ    ' EXTR-RECS-IN                        
           DISPLAY ' EXTR RECORDS WRITTEN ' EXTR-RECS-OUT                       
           DISPLAY ' EXTR RECORDS FIXED   ' EXTR-RECS-FIX                       
           GOBACK                                                               
                                                                                
           .                                                                    
       0100-PROCESS-EXTR.                                                       
                                                                                
           IF AM-CONTROL-A < EXTRACT-ACCT-KEY
              MOVE 'N'                 TO WS-PREV-EQUAL-SW
              PERFORM 1200-READ-ERACCT THRU 1200-EXIT
           ELSE
              IF (AM-CONTROL-A > EXTRACT-ACCT-KEY)
                 IF (PREVIOUSLY-EQUAL)
                    MOVE SAVE-REPORT-CODE-1
                                       TO EXTRACT-REPORT-CODE-1
                    PERFORM 0300-WRITE-EXTR
                                       THRU 0300-EXIT
                    PERFORM 0200-READ-EXTR
                                       THRU 0200-EXIT
                 ELSE
                    DISPLAY ' MISSING ACCOUNT MASTER FOR '
                       EXTRACT-ACCT-KEY
                    PERFORM 0200-READ-EXTR
                                       THRU 0200-EXIT
      *              PERFORM ABEND-PGM
                 END-IF
              ELSE
                 IF AM-CONTROL-A = EXTRACT-ACCT-KEY
                    MOVE AM-REPORT-CODE-1
                                       TO SAVE-REPORT-CODE-1
                    SET PREVIOUSLY-EQUAL TO TRUE
                    PERFORM 1200-READ-ERACCT
                                       THRU 1200-EXIT
                 ELSE
                    DISPLAY ' I THINK PROBLEM ' EXTRACT-ACCT-KEY
                 END-IF   
              END-IF
           END-IF
                                                                                
                                                                                
           .                                                                    
                                                                                
       0100-EXIT.                                                               
           EXIT.                                                                

       0200-READ-EXTR.                                                          
                                                                                
           READ EXTR-IN AT END                                                  
                SET END-OF-EXTRS       TO TRUE                           
           END-READ                                                             
                                                                                
           IF NOT END-OF-EXTRS
              ADD 1                    TO EXTR-RECS-IN
           END-IF                                                               
                                                                                
           .                                                                    
                                                                                
       0200-EXIT.                                                               
           EXIT.                                                                
                                                                                
       0300-WRITE-EXTR.                                                         
                                                                                
           WRITE EXTR-RECORD-OUT       FROM EXTR-RECORD-IN
           ADD 1                       TO EXTR-RECS-OUT
                                                                                
           .                                                                    
                                                                                
       0300-EXIT.                                                               
           EXIT.                                                                
                                                                                
       0400-OPEN-FILES.                                                         
                                                                                
           OPEN INPUT EXTR-IN ERACCT                                         
               OUTPUT EXTR-OUT                                                  
                                                                                
           IF ERACCT-FILE-STATUS = '00'
               CONTINUE
           ELSE
               DISPLAY '*** ERROR OPENING ERACCT FILE ***'
               DISPLAY '*** STATUS CODE IS ' ERACCT-FILE-STATUS
               PERFORM ABEND-PGM
           END-IF

           .                                                                    
                                                                                
       0400-EXIT.                                                               
           EXIT.                                                                
                                                                                
       0500-CLOSE-FILES.                                                        
                                                                                
           CLOSE EXTR-IN EXTR-OUT ERACCT                                        
                                                                                
           .                                                                    
                                                                                
       0500-EXIT.                                                               
           EXIT.                                                                
                                                                                
       0600-INITIALIZE.                                                         
                                                                                
           PERFORM 0200-READ-EXTR      THRU 0200-EXIT
           PERFORM 1100-START-ERACCT   THRU 1100-EXIT
           PERFORM 1200-READ-ERACCT    THRU 1200-EXIT

           .                                                                    
                                                                                
       0600-EXIT.                                                               
           EXIT.                                                                

       1100-START-ERACCT.

           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD

           START ERACCT KEY IS NOT < AM-CONTROL-PRIMARY
           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERACCT, BAD START '
                    ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .

       1100-EXIT.
           EXIT.

       1200-READ-ERACCT.

           READ ERACCT NEXT RECORD

           IF (ERACCT-FILE-STATUS = '10' OR '23')
              OR (AM-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
              MOVE HIGH-VALUES         TO AM-CONTROL-A
              SET END-OF-ERACCT        TO TRUE
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY 'ERACCT, BAD READ NEXT '
                      ERACCT-FILE-STATUS
                 PERFORM ABEND-PGM
              ELSE
                 ADD 1                 TO WS-ERACCT-RECS-IN
              END-IF
           END-IF

           .

       1200-EXIT.
           EXIT.

                                                                                
       ABEND-PGM SECTION.
                                       COPY ELCABEND.

