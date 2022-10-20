       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.    FNB166.                                           
                                                                        
      ***************************************************************** 
      *                                                               * 
      *                  CID PREMIUM AND COMMISSION                   * 
      *                                                               * 
      ***************************************************************** 
031102******************************************************************
031102*                   C H A N G E   L O G
031102*
031102* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031102*-----------------------------------------------------------------
031102*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031102* EFFECTIVE    NUMBER
031102*-----------------------------------------------------------------
      * 021299                   DANA  TO PRODUCTION
031102* 031102    2002021300008  SMVA  REMOVE PLAN CODE TABLE DEPENDENCY
103002* 103002                   PEMA  ADD PROCESSING FOR DCC
031102******************************************************************
                                                                        
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT LOGIC-DETAIL-EXTRACT                                  
               ASSIGN TO SYS010                                         
               FILE STATUS IS SYS010-STATUS.                            
                                                                        
           SELECT FREEDOM-EXTRACT                                       
               ASSIGN TO SYS011.
PEMUNI*        ORGANIZATION IS LINE SEQUENTIAL.
                                                                        
           SELECT LOGIC-CONTROL-FILE                                    
               ASSIGN TO ELCNTL                                         
               ORGANIZATION IS INDEXED                                  
               ACCESS IS SEQUENTIAL                                     
               RECORD KEY IS CF-CONTROL-PRIMARY                         
               FILE STATUS IS ELCNTL-STATUS.                            
                                                                        
           SELECT LOGIC-ACCOUNT-MASTER                                  
               ASSIGN TO ERACCT                                         
               ORGANIZATION IS INDEXED                                  
               ACCESS IS DYNAMIC                                        
               RECORD KEY IS AM-CONTROL-PRIMARY                         
               FILE STATUS IS ERACCT-STATUS.                            
                                                                        
103002     SELECT DISK-DATE        ASSIGN TO SYS019.
                                                                        
           EJECT                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD  LOGIC-DETAIL-EXTRACT                                         
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           RECORD CONTAINS 510 CHARACTERS                               
           BLOCK CONTAINS 0 RECORDS.                                    
           COPY ECSEXT01.                                               
                                                                        
       FD  FREEDOM-EXTRACT                                              
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  EXTRACT-RECORD      PIC X(250).                              
                                                                        
       FD  LOGIC-CONTROL-FILE.                                          
           COPY ELCCNTL.                                                
                                                                        
       FD  LOGIC-ACCOUNT-MASTER.                                        
           COPY ERCACCT.                                                
                                                                        
103002 FD  DISK-DATE                                                    
103002     COPY ELCDTEFD.                                               
103002                                                                  
       EJECT                                                            
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
       01  FILLER.                                                      
103002     05  WS-ZERO                 PIC S9(3)  COMP-3  VALUE +0.
103002     05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.    
103002     05  PGM-SUB                 PIC S9(4) COMP VALUE +310.
103002     05  WS-RETURN-CODE          PIC S999  COMP-3 VALUE +0.
           05  DUMP                PIC X     VALUE ' '.                 
           05  FORCE-DUMP REDEFINES DUMP PIC S9 COMP-3.                 
           05  SYS010-STATUS       PIC XX    VALUE '00'.                
               88  EOF                       VALUE '10'.                
           05  DATE-SW             PIC X     VALUE ' '.                 
               88  VALID-DATE                VALUE 'V'.                 
           05  VALID-RECORD-SW     PIC X     VALUE '0'.                 
               88  VALID-RECORD              VALUE '1'.                 
           05  END-OF-SEARCH-SW    PIC X     VALUE '0'.                 
               88  END-OF-SEARCH             VALUE '1'.                 
           05  ELCNTL-STATUS       PIC XX    VALUE '00'.                
           05  ERACCT-STATUS       PIC XX    VALUE '00'.                
           05  WS-BEN-TYPE         PIC XX.                              
           05  WS-LF-PLAN.                                              
               10  WS-LF-PLAN1     PIC X.                               
               10  WS-LF-PLAN2     PIC XXX.                             
           05  WS-AH-PLAN.                                              
               10  WS-AH-PLAN1     PIC X.                               
               10  WS-AH-PLAN2     PIC XXX.                             
           05  WS-ZIP              PIC 9(5).                            
                                                                        
       01  FILLER              COMP-3.                                  
           05  SUB             PIC S9(3)         VALUE +0.              
           05  WRK             PIC S9(7)V99      VALUE +0.              
           05  COMM-WRK        PIC S9(9)V99      VALUE +0.              
                                                                        
       01  WS-EXTRACT-RECORD.                                           
           COPY FNC022.                                                 
                                                                        
       01  FILLER.                                                      
           05  BENEFIT-CODE-TABLE OCCURS 100 TIMES INDEXED BY BEN-INDEX.
               10  BEN-NUM       PIC XX    VALUE HIGH-VALUE.            
               10  BEN-ALPHA     PIC XXX   VALUE HIGH-VALUE.            
                                                                        
       01  FNX001-PARMS.                                                
           05  FNX001-DATA      PIC X(50).                              
           05  FNX001-CITY      PIC X(30).                              
           05  FNX001-STATE     PIC X(02).                              
           05  FNX001-ZIP       PIC X(09).                              

031102 01  SYSTEM-DATE.
031102     05  SYS-MO         PIC 9(2).
031102     05  SYS-DA         PIC 9(2).
031102     05  SYS-CCYY       PIC 9(4).

031102* FUNCTION-DATE COPYBOOK
031102                                      COPY ELCFUNDT.

031102* STATE EDIT TABLE
031102 COPY FNC018.

103002     COPY ELCDTECX.                                               
103002     COPY ELCDTEVR.                                               
                                                                        
       LINKAGE SECTION.                                                 
                                                                        
       01  PARM.                                                        
           05  PARM-LENGTH      PIC S9(4)   COMP.                       
           05  CYCLE-DATE       PIC X(8).                               
                                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       PROCEDURE DIVISION USING PARM.                                   
      *                                                                 
103002*************************************************************     
103002                                 COPY ELCDTERX.                   
103002*************************************************************     
           PERFORM 0000-HOUSEKEEPING THRU 0000-EXIT                     
                                                                        
           PERFORM 1000-PROCESS THRU 1000-EXIT UNTIL EOF
           CLOSE LOGIC-DETAIL-EXTRACT
               FREEDOM-EXTRACT
               LOGIC-ACCOUNT-MASTER                                                                        
031102     GOBACK.
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       1000-PROCESS.                                                    
      *                                                                 
           PERFORM 1100-GET-LOGIC-RECORD THRU 1100-EXIT                 
              WITH TEST AFTER UNTIL VALID-RECORD OR EOF
                                                                        
           IF EOF
               GO TO 1000-EXIT
031102     END-IF
                                                                        
           MOVE SPACES TO WS-EXTRACT-RECORD                             
                                                                        
103002     IF DTE-CLIENT = 'CID'
103002        MOVE 'CID LOGIC'         TO FX-SYSTEM                                
103002        MOVE 'CRLOGC'            TO FX-SOURCE-CODE                           
103002        MOVE '02'                TO FX-DIVISION                              
103002        MOVE 'S'                 TO FX-FY-REN
103002     ELSE
103002        IF DTE-CLIENT = 'DCC'
103002           MOVE 'LPAC LOGIC'     TO FX-SYSTEM                                
103002           MOVE 'LOGIC '         TO FX-SOURCE-CODE                           
103002           MOVE '11'             TO FX-DIVISION                              
103002           MOVE ' '              TO FX-FY-REN
103002        END-IF
103002     END-IF
103002                                                                  
103002     IF DTE-CLIENT = 'CID'
103002        PERFORM 1200-GET-PLAN    THRU 1200-EXIT
103002     ELSE
103002        IF DTE-CLIENT = 'DCC'
103002           MOVE DE-LF-TYPE       TO WS-LF-PLAN
103002           MOVE DE-AH-TYPE       TO WS-AH-PLAN
103002        END-IF   
103002     END-IF
                                                                        
           MOVE CYCLE-DATE  TO FX-POSTING-DATE                          

           MOVE DE-CERT     TO FX-POLICY-NO                             
           MOVE DE-STATE    TO FX-STATE                                 
           MOVE 'Y'         TO FX-LOC-CODE                              
                                                                        
           PERFORM 1300-GET-ADDR THRU 1300-EXIT                         
                                                                        
           MOVE DE-ACCT-PRIME  TO FX-AGENT-01                           
???        MOVE '6211'         TO FX-COST-CENTER                        
           MOVE ' '            TO FX-DISTR                              
           MOVE ' '            TO FX-SOURCE-ACCT                        
           MOVE ' '            TO FX-REFERENCE                          
                                                                        
           EVALUATE DE-TRANS                                            
              WHEN 'I' MOVE 'ISSUE PREMIUM   ' TO FX-DESCRIPTION        
              WHEN '8' MOVE 'ISSUE RC-L PREM ' TO FX-DESCRIPTION        
              WHEN 'C' MOVE 'CANCEL PREMIUM  ' TO FX-DESCRIPTION        
              WHEN '7' MOVE 'CANCEL RC-L PREM' TO FX-DESCRIPTION        
           END-EVALUATE                                                 
                                                                        
           IF (DE-TRANS = 'I' OR '8')                                   
             AND (DE-LF-PRM-ALT NOT = ZERO)                             
               ADD DE-LF-PRM-ALT TO DE-LF-PRM
031102     END-IF


           IF DE-LF-TYPE NOT = ZERO                                     
               MOVE WS-LF-PLAN TO FX-PLAN-CODE
031102*        CALL 'FNB160' USING WS-EXTRACT-RECORD
031102         PERFORM 1400-OTHER-FX-DATA   THRU 1400-EXIT
103002         IF DTE-CLIENT = 'CID'
                  PERFORM 2000-CID-LIFE-PREMIUM
                                       THRU 2000-EXIT
                  PERFORM 3000-CID-LIFE-COMMISSION
                                       THRU 3000-EXIT
               ELSE
                  PERFORM 2500-DCC-LIFE-PREMIUM
                                       THRU 2500-EXIT
                  PERFORM 3500-DCC-LIFE-COMMISSION
                                       THRU 3500-EXIT
               END-IF
           END-IF                                                       


           IF DE-AH-TYPE NOT = ZERO                                     
              MOVE WS-AH-PLAN          TO FX-PLAN-CODE
031102*       CALL 'FNB160' USING WS-EXTRACT-RECORD
031102        PERFORM 1400-OTHER-FX-DATA
                                       THRU 1400-EXIT
              IF DTE-CLIENT = 'CID'
                 PERFORM 4000-CID-AH-PREMIUM
                                       THRU 4000-EXIT
                 PERFORM 5000-CID-AH-COMMISSION
                                       THRU 5000-EXIT
              ELSE
                 PERFORM 4500-DCC-AH-PREMIUM
                                       THRU 4500-EXIT
                 PERFORM 5500-DCC-AH-COMMISSION
                                       THRU 5500-EXIT
              END-IF
           END-IF                                                       

           .

       1000-EXIT.                                                       
           EXIT.                                                        


       1100-GET-LOGIC-RECORD.                                           
      *                                                                 
           MOVE '0' TO VALID-RECORD-SW                                  
           READ LOGIC-DETAIL-EXTRACT                                    
               AT END GO TO 1100-EXIT.                                  
           IF DE-REIN NOT = SPACE                                       
               GO TO 1100-EXIT.                                         
           IF DE-TRANS = 'I' OR 'C' OR '7' OR '8'                       
               CONTINUE                                                 
           ELSE                                                         
               GO TO 1100-EXIT.                                         
           IF (DE-TRANS = 'I' OR '8')                                   
             AND (DE-ENTRY-STATUS = '3' OR '5' OR 'M')                  
               GO TO 1100-EXIT.                                         
                                                                        
           IF (DE-LF-TYPE = ZERO)  AND                                  
              (DE-AH-TYPE = ZERO)                                       
                 GO TO 1100-EXIT.                                       
                                                                        
           SET VALID-RECORD TO TRUE                                     
           .                                                            
       1100-EXIT.                                                       
           EXIT.                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       1200-GET-PLAN.                                                   
      *                                                                 
           MOVE SPACES TO WS-LF-PLAN                                    
           MOVE SPACES TO WS-AH-PLAN                                    
                                                                        
           IF DE-LF-TYPE NOT = ZERO                                     
              IF DE-IG = '1'                                            
                 MOVE 'I' TO WS-LF-PLAN1                                
              ELSE                                                      
                 MOVE 'G' TO WS-LF-PLAN1                                
              END-IF                                                    
              SET BEN-INDEX TO +1                                       
              SEARCH BENEFIT-CODE-TABLE                                 
                 WHEN BEN-NUM (BEN-INDEX) = DE-LF-TYPE                  
                   MOVE BEN-ALPHA (BEN-INDEX) TO WS-LF-PLAN2            
                 WHEN BEN-NUM (BEN-INDEX) = HIGH-VALUE                  
                   MOVE ZEROS TO WS-LF-PLAN2                            
                   DISPLAY 'INVALID LIFE BEN TYPE: '                    
                            DE-LF-TYPE '  ' DE-CONTROL                  
               END-SEARCH                                               
           END-IF                                                       
                                                                        
           IF DE-AH-TYPE NOT = ZERO                                     
              IF DE-IG = '1'                                            
                 MOVE 'I' TO WS-AH-PLAN1                                
              ELSE                                                      
                 MOVE 'G' TO WS-AH-PLAN1                                
              END-IF                                                    
              MOVE DE-AH-TYPE TO WS-AH-PLAN2                            
           END-IF                                                       

           .
       1200-EXIT.                                                       
           EXIT.                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       1300-GET-ADDR.                                                   
      *                                                                 
           MOVE DE-COMPANY-CD TO AM-COMPANY-CD                          
           MOVE DE-CNTRL1     TO AM-CNTRL-1                             
           MOVE LOW-VALUES    TO AM-EXPIRATION-DT                       
           START LOGIC-ACCOUNT-MASTER                                   
             KEY NOT LESS THAN AM-CONTROL-PRIMARY                       
           IF ERACCT-STATUS NOT = '00'                                  
              GO TO 1300-EXIT
031102     END-IF

           MOVE '0' TO END-OF-SEARCH-SW                                 
                                                                        
           PERFORM UNTIL END-OF-SEARCH                                  
              READ LOGIC-ACCOUNT-MASTER NEXT                            
              IF ERACCT-STATUS = '00'                                   
                 IF AM-COMPANY-CD = DE-COMPANY-CD                       
                    IF AM-CNTRL-1 = DE-CNTRL1                           
                       MOVE AM-CITY TO FX-CITY                          
                       MOVE AM-ZIP  TO FX-ZIP-CODE                      
                    ELSE                                                
                       SET END-OF-SEARCH TO TRUE                        
                    END-IF                                              
                 ELSE                                                   
                    SET END-OF-SEARCH TO TRUE                           
                 END-IF                                                 
              ELSE                                                      
                 SET END-OF-SEARCH TO TRUE                              
              END-IF                                                    
           END-PERFORM                                                  
                                                                        
           MOVE FX-CITY TO FNX001-DATA                                  
           CALL 'FNX001' USING FNX001-PARMS                             
           MOVE FNX001-CITY TO FX-CITY                                  

           .
       1300-EXIT.                                                       
           EXIT.                                                        
                                                                        

031102 1400-OTHER-FX-DATA.

031102**** PULLED FROM FNB160
031102     IF FX-STATE NOT= SPACES
031102         SEARCH ALL STATE-TABLE
031102            AT END DISPLAY 'INVALID STATE CODE: ' FX-STATE
031102            WHEN ST-STATE (ST-INDEX) = FX-STATE
031102            MOVE ST-ALT-STATE (ST-INDEX) TO FX-STATE
031102         END-SEARCH
031102     END-IF

031102     MOVE SYSTEM-DATE              TO FX-JOURNAL-DATE

031102**** THE '*' IN POSITION 250 ENSURES A 250 BYTE RECORD IS PASSED
031102**** TO FREEDOM - PREVENTS TRUNCATION OF BLANK FIELDS
031102     MOVE '*'                      TO WS-EXTRACT-RECORD(250:1)

031102     .
031102 1400-EXIT.
031102     EXIT.
           EJECT

      *                                                                 
       2000-CID-LIFE-PREMIUM.                                               
      *                                                                 
           MOVE '40' TO FX-TRAN-TYPE                                    

           IF DE-TRANS = 'I'                                            
              MOVE '01' TO FX-SUB-TYPE                                  
              MULTIPLY DE-LF-PRM BY -1 GIVING FX-AMOUNT                 
              WRITE EXTRACT-RECORD FROM WS-EXTRACT-RECORD               
           ELSE                                                         
               IF DE-TRANS = 'C'
                   MOVE '02' TO FX-SUB-TYPE
                   MOVE DE-LF-RFND TO FX-AMOUNT
                   WRITE EXTRACT-RECORD FROM WS-EXTRACT-RECORD
031102         END-IF
           END-IF

           .                                                            
       2000-EXIT.                                                       
           EXIT.                                                        
                                                                        
       2500-DCC-LIFE-PREMIUM.                                               
      *                                                                 
           MOVE '40'                   TO FX-TRAN-TYPE                                    
           MOVE +0 TO WRK, COMM-WRK                                     

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10               
             IF (DE-L-PC (SUB) NOT = ZERO)
110702          AND (DE-AGT-TYPE (SUB) NOT = 'O' AND 'P')
110702          IF DE-TRANS = 'I' OR '8'                                
110702             COMPUTE WRK ROUNDED = DE-LF-PRM  * DE-L-PC (SUB)     
110702          ELSE                                                    
110702             COMPUTE WRK ROUNDED = DE-LF-RFND * DE-L-PC (SUB)     
110702          END-IF
                ADD WRK TO COMM-WRK                                     
             END-IF                                                     
           END-PERFORM                                                  

           IF DE-TRANS = 'I'                                            
              MOVE '01'                TO FX-SUB-TYPE                                  
              COMPUTE COMM-WRK = DE-LF-PRM - COMM-WRK
              MULTIPLY COMM-WRK        BY -1 GIVING FX-AMOUNT                 
              WRITE EXTRACT-RECORD FROM WS-EXTRACT-RECORD               
           ELSE                                                         
              IF DE-TRANS = 'C'
                 MOVE '01'             TO FX-SUB-TYPE
                 COMPUTE COMM-WRK = DE-LF-RFND - COMM-WRK
                 MOVE COMM-WRK         TO FX-AMOUNT
                 WRITE EXTRACT-RECORD FROM WS-EXTRACT-RECORD
              END-IF
           END-IF

           .                                                            
       2500-EXIT.                                                       
           EXIT.                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       3000-CID-LIFE-COMMISSION.                                            
      *                                                                 
           MOVE '50' TO FX-TRAN-TYPE                                    
           IF DE-TRANS = 'I' OR '8'                                     
              MOVE '01' TO FX-SUB-TYPE                                  
           ELSE                                                         
              MOVE '02' TO FX-SUB-TYPE                                  
           END-IF                                                       
                                                                        
           MOVE +0 TO WRK, COMM-WRK                                     
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10               
             IF (DE-L-PC (SUB) NOT = ZERO)
110702          IF DE-TRANS = 'I' OR '8'                                
110702             COMPUTE WRK ROUNDED = DE-LF-PRM  * DE-L-PC (SUB)     
110702          ELSE                                                    
110702             COMPUTE WRK ROUNDED = DE-LF-RFND * DE-L-PC (SUB)     
110702          END-IF
                ADD WRK TO COMM-WRK                                     
             END-IF                                                     
           END-PERFORM                                                  
                                                                        
           IF COMM-WRK NOT = ZERO                                       
              IF DE-TRANS = 'I' OR '8'                                  
                 MOVE COMM-WRK TO FX-AMOUNT                             
                 WRITE EXTRACT-RECORD FROM WS-EXTRACT-RECORD            
              ELSE                                                      
                 MULTIPLY COMM-WRK BY -1 GIVING FX-AMOUNT               
                 WRITE EXTRACT-RECORD FROM WS-EXTRACT-RECORD            
              END-IF                                                    
           END-IF                                                       

           .
       3000-EXIT.                                                       
           EXIT.                                                        
                                                                        
       3500-DCC-LIFE-COMMISSION.                                            
      *                                                                 
           MOVE '50' TO FX-TRAN-TYPE                                    
           MOVE '01' TO FX-SUB-TYPE                                  
                                                                        
           MOVE +0 TO WRK, COMM-WRK                                     

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10               
             IF (DE-L-PC (SUB) NOT = ZERO)
110702          AND (DE-AGT-TYPE (SUB) = 'O' OR 'P')
110702          IF DE-TRANS = 'I' OR '8'                                
110702             COMPUTE WRK ROUNDED = DE-LF-PRM  * DE-L-PC (SUB)     
110702          ELSE                                                    
110702             COMPUTE WRK ROUNDED = DE-LF-RFND * DE-L-PC (SUB)     
110702          END-IF
                ADD WRK TO COMM-WRK                                     
             END-IF                                                     
           END-PERFORM                                                  
                                                                        
           IF COMM-WRK NOT = ZERO                                       
              IF DE-TRANS = 'I' OR '8'                                  
                 MOVE COMM-WRK TO FX-AMOUNT                             
                 WRITE EXTRACT-RECORD FROM WS-EXTRACT-RECORD            
              ELSE                                                      
                 MULTIPLY COMM-WRK BY -1 GIVING FX-AMOUNT               
                 WRITE EXTRACT-RECORD FROM WS-EXTRACT-RECORD            
              END-IF                                                    
           END-IF                                                       

           .
       3500-EXIT.                                                       
           EXIT.                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       4000-CID-AH-PREMIUM.                                                 
      *                                                                 
           MOVE '40' TO FX-TRAN-TYPE                                    
           IF DE-TRANS = 'I'                                            
              MOVE '01' TO FX-SUB-TYPE                                  
              MULTIPLY DE-AH-PRM BY -1 GIVING FX-AMOUNT                 
              WRITE EXTRACT-RECORD FROM WS-EXTRACT-RECORD               
           ELSE                                                         
               IF DE-TRANS = 'C'
                   MOVE '02' TO FX-SUB-TYPE
                   MOVE DE-AH-RFND TO FX-AMOUNT
                   WRITE EXTRACT-RECORD FROM WS-EXTRACT-RECORD
               END-IF
           END-IF

           .                                                            
       4000-EXIT.                                                       
           EXIT.                                                        
                                                                        
       4500-DCC-AH-PREMIUM.                                                 
      *                                                                 
           MOVE '40'                   TO FX-TRAN-TYPE                                    
           MOVE +0                     TO WRK, COMM-WRK                                     

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10               
             IF (DE-A-PC (SUB) NOT = ZERO)
                AND (DE-AGT-TYPE (SUB) NOT = 'O' AND 'P')
                IF DE-TRANS = 'I' OR '8'                                
                   COMPUTE WRK ROUNDED = DE-AH-PRM  * DE-A-PC (SUB)     
                ELSE                                                    
                   COMPUTE WRK ROUNDED = DE-AH-RFND * DE-A-PC (SUB)     
                END-IF                                                  
                ADD WRK TO COMM-WRK                                     
             END-IF                                                     
           END-PERFORM                                                  

           IF DE-TRANS = 'I'                                            
              MOVE '01'                TO FX-SUB-TYPE                                  
              COMPUTE COMM-WRK = DE-AH-PRM - COMM-WRK
              MULTIPLY COMM-WRK        BY -1 GIVING FX-AMOUNT                 
              WRITE EXTRACT-RECORD     FROM WS-EXTRACT-RECORD               
           ELSE                                                         
              IF DE-TRANS = 'C'
                 MOVE '01'             TO FX-SUB-TYPE
                 COMPUTE COMM-WRK = DE-AH-RFND - COMM-WRK
                 MOVE COMM-WRK         TO FX-AMOUNT
                 WRITE EXTRACT-RECORD  FROM WS-EXTRACT-RECORD
              END-IF
           END-IF

           .                                                            
       4500-EXIT.                                                       
           EXIT.                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       5000-CID-AH-COMMISSION.                                              
      *                                                                 
           MOVE '50' TO FX-TRAN-TYPE                                    
           IF DE-TRANS = 'I' OR '8'                                     
              MOVE '01' TO FX-SUB-TYPE                                  
           ELSE                                                         
              MOVE '02' TO FX-SUB-TYPE                                  
           END-IF                                                       
                                                                        
           MOVE +0 TO WRK, COMM-WRK                                     
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10               
             IF DE-A-PC (SUB) NOT = ZERO                                
                IF DE-TRANS = 'I' OR '8'                                
                   COMPUTE WRK ROUNDED = DE-AH-PRM  * DE-A-PC (SUB)     
                ELSE                                                    
                   COMPUTE WRK ROUNDED = DE-AH-RFND * DE-A-PC (SUB)     
                END-IF                                                  
                ADD WRK TO COMM-WRK                                     
             END-IF                                                     
           END-PERFORM                                                  
                                                                        
           IF COMM-WRK NOT = ZERO                                       
              IF DE-TRANS = 'I' OR '8'                                  
                 MOVE COMM-WRK TO FX-AMOUNT                             
                 WRITE EXTRACT-RECORD FROM WS-EXTRACT-RECORD            
              ELSE                                                      
                 MULTIPLY COMM-WRK BY -1 GIVING FX-AMOUNT               
                 WRITE EXTRACT-RECORD FROM WS-EXTRACT-RECORD            
              END-IF                                                    
           END-IF                                                       

           .
       5000-EXIT.                                                       
           EXIT.                                                        
                                                                        
       5500-DCC-AH-COMMISSION.                                              
      *                                                                 
           MOVE '50'                   TO FX-TRAN-TYPE                                    
           MOVE '01'                   TO FX-SUB-TYPE                                  
                                                                        
           MOVE +0                     TO WRK, COMM-WRK                                     

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10               
             IF (DE-A-PC (SUB) NOT = ZERO)
                AND (DE-AGT-TYPE (SUB) = 'O' OR 'P')
                IF DE-TRANS = 'I' OR '8'                                
                   COMPUTE WRK ROUNDED = DE-AH-PRM  * DE-A-PC (SUB)     
                ELSE                                                    
                   COMPUTE WRK ROUNDED = DE-AH-RFND * DE-A-PC (SUB)     
                END-IF                                                  
                ADD WRK TO COMM-WRK                                     
             END-IF                                                     
           END-PERFORM                                                  
                                                                        
           IF COMM-WRK NOT = ZERO                                       
              IF DE-TRANS = 'I' OR '8'                                  
                 MOVE COMM-WRK TO FX-AMOUNT                             
                 WRITE EXTRACT-RECORD FROM WS-EXTRACT-RECORD            
              ELSE                                                      
                 MULTIPLY COMM-WRK BY -1 GIVING FX-AMOUNT               
                 WRITE EXTRACT-RECORD FROM WS-EXTRACT-RECORD            
              END-IF                                                    
           END-IF                                                       

           .
       5500-EXIT.                                                       
           EXIT.                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       0000-HOUSEKEEPING.                                               
      *                                                                 
           CALL 'DATEEDIT' USING CYCLE-DATE,  DATE-SW                   
           IF NOT VALID-DATE                                            
               DISPLAY 'INVALID PARAMETER DATE: ' CYCLE-DATE            
               ADD +1 TO FORCE-DUMP
           END-IF


031102     MOVE FUNCTION CURRENT-DATE
031102                                 TO FUNCTION-DATE
031102     MOVE WS-FN-MO               TO SYS-MO
031102     MOVE WS-FN-DA               TO SYS-DA
031102     MOVE WS-FN-CCYR             TO SYS-CCYY


           PERFORM 0100-LOAD-BENEFIT-TABLE THRU 0100-EXIT

           OPEN INPUT LOGIC-ACCOUNT-MASTER                              
           IF ERACCT-STATUS = '00' OR '97'                              
               CONTINUE                                                 
           ELSE                                                         
               DISPLAY 'OPEN ERROR ' ERACCT-STATUS ' ON ERACCT'         
               ADD +1 TO FORCE-DUMP
           END-IF
                                                                        
           OPEN  INPUT LOGIC-DETAIL-EXTRACT                             
                OUTPUT FREEDOM-EXTRACT

           .
       0000-EXIT.                                                       
           EXIT.                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       0100-LOAD-BENEFIT-TABLE.                                         
      *                                                                 
           OPEN INPUT LOGIC-CONTROL-FILE                                
           IF ELCNTL-STATUS = '00' OR '97'                              
               MOVE '00' TO ELCNTL-STATUS                               
           ELSE                                                         
               DISPLAY 'OPEN ERROR ' ELCNTL-STATUS ' ON ELCNTL'         
               ADD +1 TO FORCE-DUMP
           END-IF
                                                                        
           SET BEN-INDEX TO +1
           READ LOGIC-CONTROL-FILE                                      
                                                                        
           PERFORM UNTIL ELCNTL-STATUS NOT = '00'                       
               IF (CF-COMPANY-ID = 'CID') AND (CF-RECORD-TYPE = '4')
                   PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
                       MOVE CF-BENEFIT-NUMERIC (SUB) TO
                                                   BEN-NUM (BEN-INDEX)
                       MOVE CF-BENEFIT-ALPHA (SUB)   TO
                                                   BEN-ALPHA (BEN-INDEX)
                       SET BEN-INDEX UP BY +1
                       IF BEN-INDEX > 100 DISPLAY
                           'TABLE OVERFLOW IN ROUTINE 0100-LOAD-BEN'
                           ADD +1 TO FORCE-DUMP
                       END-IF
                   END-PERFORM
               END-IF
               READ LOGIC-CONTROL-FILE
           END-PERFORM                                                  
                                                                        
           CLOSE LOGIC-CONTROL-FILE

           .                                                            
       0100-EXIT.                                                       
           EXIT.                                                        

103002 ABEND-PGM SECTION.                                               00026280
103002     DIVIDE WS-ZERO BY WS-ZERO GIVING WS-ZERO.                    00026290
103002 ABEND-EXIT.                                                      00026300
                                                                        
