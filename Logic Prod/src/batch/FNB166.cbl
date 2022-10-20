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
100404* 100404                   PEMA  ADD SPP PROCESSING
020305* 020305                   PEMA  ADD CLP STATE PROCESSING
092705* 092705  CR2005050300006  PEMA  ADD SPP LEASES
122205* 122205    2005033100001  PEMA  ADD PROCESSING FOR CSI
112906* 112906  CR2006111300003  PEMA  ADD PROCESSING FOR KY
101708* 101708    2008050500001  AJRA  ADD PROCESSING FOR CCC
101409* 101409  IR2009100200001  PEMA  ADD ROUNDED CLAUSE
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
021412* 021412   2011110200001   AJRA  ADD AHL CLM RESERVES
042512* 042512  IR2012042400001  AJRA  FORCE CYCLE DATE INTO FX DATES
032813* 032813  IR2013031500002  PEMA  CORRECT COMPANY ID
010914* 010914  IR2014010200011  PEMA  CORRECT AGENT TYPES
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
090214*090214   IR2014090200004  PEMA  DCC CREDIT UNION CORRECTIONS
121114* 121114  CR2014121100003  PEMA  INT CAR 7 COMM AS CLP
010716* 010716  CR2015082500001  PEMA  VPP CHANGES
011116* 011116  CR2015082400003  PEMA  ADD CARRIER 9 PROCESSING
042216* 042216  CR2016032400001  PEMA  MORE CARRIER 7 CHANGES
111616* 111616  CR2016092200001  PEMA  Remove CSI processing
042817* 042817  CR2017021400002  PEMA  Add benefit code to extract
092017* 092017  IR2017090600003  PEMA  Correct ref clp interface
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
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
                                                                        
101708     SELECT CCC-FREEDOM-EXTRACT  ASSIGN TO SYS013.
101708
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

101708 FD  CCC-FREEDOM-EXTRACT
101708     LABEL RECORDS ARE STANDARD
101708     RECORDING MODE IS F
101708     BLOCK CONTAINS 0 RECORDS.
101708 01  CCC-EXTRACT-RECORD          PIC X(250).
101708
       FD  LOGIC-CONTROL-FILE.                                          
           COPY ELCCNTL.                                                
                                                                        
       FD  LOGIC-ACCOUNT-MASTER.                                        
           COPY ERCACCT.                                                
                                                                        
103002 FD  DISK-DATE                                                    
103002     COPY ELCDTEFD.                                               
103002                                                                  
       EJECT                                                            
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
011410 77  WS-WORK-REF                 PIC S9(9)V99 COMP-3 VALUE +0.
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
           05  CNC-FACT        PIC S9(3)V9(7)    VALUE +0.
           05  SUB             PIC S9(3)         VALUE +0.              
           05  WRK             PIC S9(7)V99      VALUE +0.              
           05  COMM-WRK        PIC S9(9)V99      VALUE +0.              
                                                                        
       01  WS-EXTRACT-RECORD.                                           
           COPY FNC022.                                                 
                                                                        
       01  FILLER.                                                      
021412     05  BENEFIT-CODE-TABLE OCCURS 250 TIMES INDEXED BY BEN-INDEX.
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

011116     IF DTE-CLIENT = 'DCC' or 'VPP'
111616        CLOSE CCC-FREEDOM-EXTRACT
122205     END-IF

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
                                                                        
PEMTST*    DISPLAY ' DE CARRIER = ' DE-CARRIER
PEMTST*    MOVE '3'                    TO DE-CARRIER

122205     EVALUATE TRUE
062121        WHEN DTE-CLIENT = 'FNL'
062121           MOVE 'FNL LOGIC'      TO FX-SYSTEM
062121           MOVE '02'             TO FX-DIVISION
062121           MOVE 'CRLOGC'         TO FX-SOURCE-CODE
062121           MOVE 'S'              TO FX-FY-REN
021412        WHEN DTE-CLIENT = 'AHL'
021412           MOVE 'AHL LOGIC'      TO FX-SYSTEM
021412           MOVE '02'             TO FX-DIVISION
021412           MOVE 'CRLOGC'         TO FX-SOURCE-CODE
021412           MOVE 'S'              TO FX-FY-REN
122205        WHEN DTE-CLIENT = 'CID'
122205           MOVE 'CID LOGIC'      TO FX-SYSTEM
122205           MOVE '02'             TO FX-DIVISION
122205           MOVE 'CRLOGC'         TO FX-SOURCE-CODE
122205           MOVE 'S'              TO FX-FY-REN
011116        WHEN (DTE-CLIENT = 'DCC') AND
111616             (DE-CARRIER = '3' or '4' or '5' OR '6')
101708           MOVE 'CCCDCCLOGC'     TO FX-SYSTEM
101708           MOVE 'LOGIC '         TO FX-SOURCE-CODE
101708           MOVE ' '              TO FX-FY-REN
101708           MOVE '50'             TO FX-DIVISION
042216        WHEN (DTE-CLIENT = 'DCC') AND
042216             (DE-CARRIER = '7')
042216           MOVE 'CCCDCCLOGC'     TO FX-SYSTEM
042216           MOVE 'LOGIC '         TO FX-SOURCE-CODE
042216           MOVE ' '              TO FX-FY-REN
042216           MOVE '5C'             TO FX-DIVISION
011116        WHEN DTE-CLIENT = 'DCC' *> default - car 1,2&9
122205           MOVE 'LPAC LOGIC'     TO FX-SYSTEM
122205           MOVE 'LOGIC '         TO FX-SOURCE-CODE
122205           MOVE ' '              TO FX-FY-REN
122205           MOVE '11'             TO FX-DIVISION
010716        WHEN DTE-CLIENT = 'VPP'
010716           MOVE 'VPA LOGIC'      TO FX-SYSTEM
010716           MOVE '5V'             TO FX-DIVISION
010716           MOVE 'CRLOGC'         TO FX-SOURCE-CODE
010716           MOVE ' '              TO FX-FY-REN
122205     END-EVALUATE
103002                                                                  
062121     IF DTE-CLIENT = 'CID' OR 'AHL' or 'FNL'
103002        PERFORM 1200-GET-PLAN    THRU 1200-EXIT
103002     ELSE
010716        IF DTE-CLIENT = 'DCC' or 'VPP'
103002           MOVE DE-LF-TYPE       TO WS-LF-PLAN
103002           MOVE DE-AH-TYPE       TO WS-AH-PLAN
103002        END-IF   
103002     END-IF
                                                                        
           MOVE CYCLE-DATE  TO FX-POSTING-DATE                          

           MOVE DE-CERT     TO FX-POLICY-NO                             
           MOVE DE-STATE    TO FX-STATE                                 

010716     IF DTE-CLIENT = 'DCC' or 'VPP'
              IF DE-CLP-STATE = ZEROS OR SPACES OR LOW-VALUES
                 CONTINUE
              ELSE
                 IF DE-CLP-STATE NOT = DE-STATE
                    MOVE DE-CLP-STATE     TO FX-STATE
                 END-IF
              END-IF
020305     END-IF
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
                                                                        
090214     IF DE-LF-TYPE NOT = SPACES AND ZERO AND 'DD' AND 'CU'
              IF (DE-TRANS = 'I' OR '8')                                   
                 AND (DE-LF-PRM-ALT NOT = ZERO)                             
                 ADD DE-LF-PRM-ALT TO DE-LF-PRM
031102        END-IF
           END-IF

090214     IF (DE-LF-TYPE NOT = ZERO AND 'DD' AND 'CU')
              MOVE WS-LF-PLAN TO FX-PLAN-CODE
042817        move de-lf-type          to fx-distr
031102*       CALL 'FNB160' USING WS-EXTRACT-RECORD
031102        PERFORM 1400-OTHER-FX-DATA   THRU 1400-EXIT
010716        evaluate true
062121           when dte-client = 'FNL'
062121              PERFORM 2000-CID-LIFE-PREMIUM
062121                                 THRU 2000-EXIT
062121              PERFORM 3000-CID-LIFE-COMMISSION
062121                                 THRU 3000-EXIT
010716           when dte-client = 'CID'
010716              PERFORM 2000-CID-LIFE-PREMIUM
010716                                 THRU 2000-EXIT
010716              PERFORM 3000-CID-LIFE-COMMISSION
010716                                 THRU 3000-EXIT
010716           when dte-client = 'AHL'
010716              PERFORM 2000-CID-LIFE-PREMIUM
010716                                 THRU 2000-EXIT
010716              PERFORM 3000-CID-LIFE-COMMISSION
010716                                 THRU 3000-EXIT
010716              PERFORM 3100-AHL-LIFE-RESERVE
010716                                 THRU 3100-EXIT
010716           when dte-client = 'DCC'
010716              PERFORM 1060-FIND-LF-TYPE
010716                                 THRU 1060-EXIT
010716              PERFORM 2500-DCC-LIFE-PREMIUM
010716                                 THRU 2500-EXIT
010716              PERFORM 3500-DCC-LIFE-COMMISSION
010716                                 THRU 3500-EXIT
010716           when dte-client = 'VPP'
010716              PERFORM 1060-FIND-LF-TYPE
010716                                 THRU 1060-EXIT
010716              PERFORM 2500-DCC-LIFE-PREMIUM
010716                                 THRU 2500-EXIT
010716              PERFORM 3500-DCC-LIFE-COMMISSION
010716                                 THRU 3500-EXIT
010716        end-evaluate
           END-IF                                                       

           IF DE-AH-TYPE NOT = ZERO                                     
              MOVE WS-AH-PLAN          TO FX-PLAN-CODE
042817        move de-ah-type          to fx-distr
031102*       CALL 'FNB160' USING WS-EXTRACT-RECORD
031102        PERFORM 1400-OTHER-FX-DATA
                                       THRU 1400-EXIT
010716        evaluate true
062121           when dte-client = 'FNL'
062121              PERFORM 4000-CID-AH-PREMIUM
062121                                 THRU 4000-EXIT
062121              PERFORM 5000-CID-AH-COMMISSION
062121                                 THRU 5000-EXIT
062121
010716           when dte-client = 'CID'
010716              PERFORM 4000-CID-AH-PREMIUM
010716                                 THRU 4000-EXIT
010716              PERFORM 5000-CID-AH-COMMISSION
010716                                 THRU 5000-EXIT
010716
010716           when dte-client = 'AHL'
010716              PERFORM 4000-CID-AH-PREMIUM
010716                                 THRU 4000-EXIT
010716              PERFORM 5000-CID-AH-COMMISSION
010716                                 THRU 5000-EXIT
010716              PERFORM 5100-AHL-AH-RESERVE
010716                                 THRU 5100-EXIT
010716
010716           when dte-client = 'DCC'
010716              PERFORM 1050-FIND-AH-TYPE
010716                                 THRU 1050-EXIT
010716              PERFORM 4500-DCC-AH-PREMIUM
010716                                 THRU 4500-EXIT
010716              IF DE-CARRIER = '3' or '4' or '5' OR '6' or '7'
010716                 PERFORM 5260-CCC-AH-COMMISSION
010716                                 THRU 5260-EXIT
010716              END-IF
010716              PERFORM 5500-DCC-AH-COMMISSION
010716                                 THRU 5500-EXIT
010716
010716           when dte-client = 'VPP'
010716              PERFORM 1050-FIND-AH-TYPE
010716                                 THRU 1050-EXIT
010716              PERFORM 4500-DCC-AH-PREMIUM
010716                                 THRU 4500-EXIT
010716              PERFORM 5500-DCC-AH-COMMISSION
010716                                 THRU 5500-EXIT
010716        end-evaluate
           END-IF                                                       

           .
       1000-EXIT.                                                       
           EXIT.                                                        

       1050-FIND-AH-TYPE.

           PERFORM VARYING CLAS-INDEXA FROM CLAS-STARTA BY +1 UNTIL
              (CLAS-INDEXA > CLAS-MAXA)
              OR (DE-AH-TYPE = CLAS-I-BEN (CLAS-INDEXA))
           END-PERFORM
           
           IF DE-AH-TYPE NOT = CLAS-I-BEN (CLAS-INDEXA)
              DISPLAY ' INVALID AH TYPE ' DE-AH-TYPE
              PERFORM ABEND-PGM
           END-IF
              
           .
       1050-EXIT.
           EXIT.

011410 1060-FIND-LF-TYPE.
011410
011410     PERFORM VARYING CLAS-INDEXL FROM CLAS-STARTL BY +1 UNTIL
011410        (CLAS-INDEXL > CLAS-MAXL)
011410        OR (DE-LF-TYPE = CLAS-I-BEN (CLAS-INDEXL))
011410     END-PERFORM
011410     
011410     IF DE-LF-TYPE NOT = CLAS-I-BEN (CLAS-INDEXL)
011410        DISPLAY ' INVALID LF TYPE ' DE-LF-TYPE
011410        PERFORM ABEND-PGM
011410     END-IF
011410        
011410     .
011410 1060-EXIT.
011410     EXIT.

       1100-GET-LOGIC-RECORD.                                           
      *                                                                 
           MOVE '0' TO VALID-RECORD-SW                                  
           READ LOGIC-DETAIL-EXTRACT                                    
               AT END GO TO 1100-EXIT.                                  
           IF DE-REIN NOT = SPACE                                       
               GO TO 1100-EXIT.                                         
           IF DE-TRANS = 'I' OR 'C' OR '7' OR '8'                       
021412        OR 'Y'
               CONTINUE                                                 
           ELSE                                                         
               GO TO 1100-EXIT.                                         

           IF (DE-TRANS = 'I' OR '8')                                   
             AND (DE-ENTRY-STATUS = '3' OR '5' OR 'M')                  
               GO TO 1100-EXIT.                                         
                                                                        
           IF DE-ENTRY-STATUS = 'M'
              GO TO 1100-EXIT
           END-IF

090214     IF (DE-LF-TYPE = ZERO OR 'DD' or 'CU')  AND                                  
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
                                                                        
090214     IF DE-LF-TYPE NOT = ZERO AND 'DD' and 'CU'
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
051810                 MOVE SPACES  TO FX-CITY                          
051810                 STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
051810                    DELIMITED BY '  ' INTO FX-CITY
051810                 END-STRING
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

042512     IF DTE-CLIENT = 'AHL'
042512        MOVE CYCLE-DATE            TO FX-JOURNAL-DATE
042512     ELSE
031102     MOVE SYSTEM-DATE              TO FX-JOURNAL-DATE
042512     END-IF

031102**** THE '*' IN POSITION 250 ENSURES A 250 BYTE RECORD IS PASSED
031102**** TO FREEDOM - PREVENTS TRUNCATION OF BLANK FIELDS
031102     MOVE '*'                      TO WS-EXTRACT-RECORD(250:1)

031102     .
031102 1400-EXIT.
031102     EXIT.

       2000-CID-LIFE-PREMIUM.                                               

           MOVE '40'                   TO FX-TRAN-TYPE

           IF DE-TRANS = 'I'
              MOVE '01'                TO FX-SUB-TYPE
              MULTIPLY DE-LF-PRM BY -1 GIVING FX-AMOUNT
           ELSE
              IF DE-TRANS = 'C'
                 MOVE '02'             TO FX-SUB-TYPE
                 MOVE DE-LF-RFND       TO FX-AMOUNT
031102        END-IF
           END-IF

112906     IF DE-CARRIER = '8'
112906        MOVE '04'                TO FX-SUB-TYPE
112906     END-IF

           IF DE-TRANS = 'I' OR 'C'
              WRITE EXTRACT-RECORD     FROM WS-EXTRACT-RECORD
           END-IF

           .                                                            
       2000-EXIT.                                                       
           EXIT.                                                        
                                                                        
       2500-DCC-LIFE-PREMIUM.                                               
      *                                                                 
           MOVE '40'                   TO FX-TRAN-TYPE                                    
           MOVE +0 TO WRK, COMM-WRK                                     

011410     IF CLAS-I-BEN-CATEGORY (CLAS-INDEXL) = 'D'
011410        IF DE-TRANS = 'I'
011410           MOVE DE-REI-LFPRM     TO COMM-WRK
011410        ELSE
011410           IF DE-TRANS = 'C'
011410              COMPUTE CNC-FACT =
011410                 DE-LF-RFND / DE-LF-PRM
011410              COMPUTE COMM-WRK ROUNDED = DE-REI-LFPRM * CNC-FACT
011410           END-IF
011410        END-IF
011410     ELSE
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10               
             IF (DE-L-PC (SUB) NOT = ZERO)
110702          AND (DE-AGT-TYPE (SUB) = 'C' OR 'D')
      *         IF DE-L-PC (SUB) < ZEROS
      *            COMPUTE DE-L-PC (SUB) = DE-L-PC (SUB) * -1
      *         END-IF
110702*         IF DE-TRANS = 'I' OR '8'
110702          IF DE-TRANS = 'I'
110702             COMPUTE WRK ROUNDED =
032603                DE-LF-PRM  * DE-L-PC (SUB)     
032603          ELSE
      *            IF DE-TRANS = 'C' OR '7'
                   IF DE-TRANS = 'C'
032603                COMPUTE WRK ROUNDED =
                         DE-LF-RFND * DE-L-PC (SUB)
                   END-IF
                END-IF
                ADD WRK TO COMM-WRK                                     
             END-IF                                                     
           END-PERFORM
011410     END-IF

      *    IF DE-TRANS = 'I' OR '8'
           IF DE-TRANS = 'I'
              MOVE '01'                TO FX-SUB-TYPE                                  
011410        IF CLAS-I-BEN-CATEGORY (CLAS-INDEXL) = 'D'
011410           MULTIPLY COMM-WRK     BY -1 GIVING FX-AMOUNT
              ELSE
                 IF COMM-WRK < ZEROS
                    COMPUTE COMM-WRK = DE-LF-PRM + COMM-WRK
                    MOVE COMM-WRK         TO FX-AMOUNT
                 ELSE
                    COMPUTE COMM-WRK = DE-LF-PRM - COMM-WRK
                    MULTIPLY COMM-WRK     BY -1 GIVING FX-AMOUNT
                 END-IF
              END-IF
111616        IF (DE-CARRIER = '3' or '4' or '5' OR '6' or '7')
                  and (dte-client = 'DCC')
101708           WRITE CCC-EXTRACT-RECORD FROM WS-EXTRACT-RECORD
101708        ELSE
122205           WRITE EXTRACT-RECORD  FROM WS-EXTRACT-RECORD
122205        END-IF
           ELSE                                                         
      *       IF DE-TRANS = 'C' OR '7'
              IF DE-TRANS = 'C'
                 MOVE '01'             TO FX-SUB-TYPE
011410           IF CLAS-I-BEN-CATEGORY (CLAS-INDEXL) = 'D'
011410              MOVE COMM-WRK      TO FX-AMOUNT
                 ELSE
                    IF COMM-WRK < ZEROS
                       COMPUTE COMM-WRK = DE-LF-RFND + COMM-WRK
      *                MOVE COMM-WRK      TO FX-AMOUNT
                       MULTIPLY COMM-WRK BY -1 GIVING FX-AMOUNT
                    ELSE
                       COMPUTE COMM-WRK = DE-LF-RFND - COMM-WRK
                       MOVE COMM-WRK      TO FX-AMOUNT
                    END-IF
                 END-IF
111616           IF (DE-CARRIER = '3' or '4' or '5' OR '6' or '7')
                     and (dte-client = 'DCC')
101708              WRITE CCC-EXTRACT-RECORD FROM WS-EXTRACT-RECORD
101708           ELSE
122205              WRITE EXTRACT-RECORD
122205                                 FROM WS-EXTRACT-RECORD
122205           END-IF
              END-IF
           END-IF

           .                                                            
       2500-EXIT.                                                       
           EXIT.                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       3000-CID-LIFE-COMMISSION.                                            
      *                                                                 
           MOVE '50'                   TO FX-TRAN-TYPE
           IF DE-TRANS = 'I' OR '8'
              MOVE '01'                TO FX-SUB-TYPE
           ELSE
              MOVE '02'                TO FX-SUB-TYPE
           END-IF                                                       
                                                                        
112906     IF DE-CARRIER = '8'
112906        MOVE '04'                TO FX-SUB-TYPE
112906     END-IF

           MOVE +0                     TO WRK
                                          COMM-WRK                                     

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF (DE-L-PC (SUB) NOT = ZERO)
010914           and (de-agt-type (sub) = 'C' OR 'D' OR 'O' OR 'P')
110702           IF DE-TRANS = 'I' OR '8'
110702              COMPUTE WRK ROUNDED = DE-LF-PRM  * DE-L-PC (SUB)
110702           ELSE                                                    
110702              COMPUTE WRK ROUNDED = DE-LF-RFND * DE-L-PC (SUB)
110702           END-IF
                 ADD WRK               TO COMM-WRK
              END-IF                                                     
           END-PERFORM                                                  
                                                                        
           IF COMM-WRK NOT = ZERO
              IF DE-TRANS = 'I' OR '8'
                 MOVE COMM-WRK         TO FX-AMOUNT
                 WRITE EXTRACT-RECORD  FROM WS-EXTRACT-RECORD
              ELSE                                                      
                 MULTIPLY COMM-WRK BY -1 GIVING FX-AMOUNT
                 WRITE EXTRACT-RECORD  FROM WS-EXTRACT-RECORD
              END-IF                                                    
           END-IF                                                       

           .
       3000-EXIT.                                                       
           EXIT.                                                        

021412 3100-AHL-LIFE-RESERVE.                                            
021412 
021412     IF DE-LIFE-RSV
021412         CONTINUE
021412     ELSE
021412         GO TO 3100-EXIT
021412     END-IF
021412*                                                                 
021412     MOVE '30'                   TO FX-TRAN-TYPE
021412                                                                  
021412     IF DE-CARRIER = '8'
021412        MOVE '34'                TO FX-TRAN-TYPE
021412     END-IF
021412
021412     MOVE +0                     TO WRK
021412                                    COMM-WRK                                     
021412
021412*     IF DE-IBNR <> 0
021412*           MOVE 'AHLRS IBNR'      TO FX-SYSTEM
021412*           MOVE DE-IBNR           TO FX-AMOUNT
021412*           MOVE 'LF IBNR RESERVE' TO FX-DESCRIPTION
021412*           MOVE '18'              TO FX-SUB-TYPE
021412*           WRITE EXTRACT-RECORD  FROM WS-EXTRACT-RECORD
021412*     END-IF                                                       
021412
021412     IF DE-PAYCUR <> 0
021412           MOVE 'AHLRS PTC '      TO FX-SYSTEM
021412           MOVE DE-PAYCUR         TO FX-AMOUNT
021412           MOVE 'LF PAYCUR RESV ' TO FX-DESCRIPTION
021412           MOVE '19'              TO FX-SUB-TYPE
021412           WRITE EXTRACT-RECORD  FROM WS-EXTRACT-RECORD
021412     END-IF                                                       
021412
021412     IF DE-FUTRSV <> 0
021412           MOVE 'AHLRS CICS'      TO FX-SYSTEM
021412           MOVE DE-FUTRSV         TO FX-AMOUNT
021412           MOVE 'LF FUT RESERVE ' TO FX-DESCRIPTION
021412           MOVE '17'              TO FX-SUB-TYPE
021412           WRITE EXTRACT-RECORD  FROM WS-EXTRACT-RECORD
021412     END-IF                                                       
021412
021412     .
021412 3100-EXIT.                                                       
021412     EXIT.                                                        
021412

       3250-CSI-LIFE-COMMISSION.                                            

           MOVE '80' TO FX-TRAN-TYPE                                    
           MOVE '01' TO FX-SUB-TYPE                                  
                                                                        
           MOVE +0 TO WRK, COMM-WRK                                     

100404     PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10               
100404       IF DE-L-PC (SUB) NOT = ZERO
011116          IF DE-AGT-TYPE (SUB) = 'P'
100404             IF DE-TRANS = 'I' OR '8'                                
011410                IF CLAS-I-BEN-CATEGORY (CLAS-INDEXL) = 'D'
011410                   COMPUTE WRK ROUNDED =
011410                      DE-REI-LFPRM * DE-L-PC (SUB)
011410                ELSE
100404                  COMPUTE WRK ROUNDED = DE-LF-PRM  * DE-L-PC (SUB)
                      END-IF
                      MOVE +0          TO DE-L-PC (SUB)
100404             ELSE                                                    
011410                IF CLAS-I-BEN-CATEGORY (CLAS-INDEXL) = 'D'
011410                   COMPUTE CNC-FACT =
011410                      DE-LF-RFND / DE-LF-PRM
011410                   COMPUTE WS-WORK-REF ROUNDED =
011410                     DE-REI-LFPRM * CNC-FACT
011410                   COMPUTE WRK ROUNDED =
011410                      WS-WORK-REF * DE-L-PC (SUB)
011410                ELSE
100404                  COMPUTE WRK ROUNDED = DE-LF-RFND * DE-L-PC (SUB)
011410                END-IF
                      MOVE +0          TO DE-L-PC (SUB)
100404             END-IF
100404             ADD WRK             TO COMM-WRK                                     
100404          END-IF
100404       END-IF                                                     
100404     END-PERFORM                                                  

           IF COMM-WRK NOT = ZERO                                       
              IF DE-TRANS = 'I' OR '8'                                  
                 MOVE COMM-WRK         TO FX-AMOUNT
111616*          WRITE CSI-EXTRACT-RECORD
111616*                                FROM WS-EXTRACT-RECORD
              ELSE                                                      
                 MULTIPLY COMM-WRK BY -1 GIVING FX-AMOUNT               
111616*          WRITE CSI-EXTRACT-RECORD
111616*                                FROM WS-EXTRACT-RECORD
              END-IF                                                    
           END-IF                                                       

           .
       3250-EXIT.                                                       
           EXIT.                                                        

       3500-DCC-LIFE-COMMISSION.                                            

           MOVE '50'                   TO FX-TRAN-TYPE
           MOVE '01'                   TO FX-SUB-TYPE
                                                                        
           MOVE +0 TO WRK, COMM-WRK                                     

100404     PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10               
100404       IF DE-L-PC (SUB) NOT = ZERO
011116          IF DE-AGT-TYPE (SUB) = 'P'
100404             IF DE-TRANS = 'I' OR '8'                                
011410                IF CLAS-I-BEN-CATEGORY (CLAS-INDEXL) = 'D'
011410                   COMPUTE WRK ROUNDED =
011410                      DE-REI-LFPRM * DE-L-PC (SUB)
011410                ELSE
100404                   COMPUTE WRK ROUNDED =
                            DE-LF-PRM  * DE-L-PC (SUB)
011410                END-IF
100404             ELSE                                                    
011410                IF CLAS-I-BEN-CATEGORY (CLAS-INDEXL) = 'D'
011410                   COMPUTE CNC-FACT =
011410                      DE-LF-RFND / DE-LF-PRM
011410                   COMPUTE WS-WORK-REF ROUNDED =
011410                     DE-REI-LFPRM * CNC-FACT
011410                   COMPUTE WRK ROUNDED =
011410                      WS-WORK-REF * DE-L-PC (SUB)
011410                ELSE
100404                   COMPUTE WRK ROUNDED =
                            DE-LF-RFND * DE-L-PC (SUB)
011410                END-IF
100404             END-IF
100404             ADD WRK             TO COMM-WRK                                     
100404          ELSE
100404             IF DE-AGT-TYPE (SUB) = 'K'
100404                COMPUTE WRK = DE-L-PC (SUB) * +1000
100404                ADD WRK          TO COMM-WRK                                     
100404             END-IF
100404          END-IF
100404       END-IF                                                     
100404     END-PERFORM                                                  

           IF COMM-WRK NOT = ZERO                                       
              IF DE-TRANS = 'I' OR '8'                                  
                 MOVE COMM-WRK TO FX-AMOUNT                             
111616           IF (DE-CARRIER = '3' or '4' or '5' OR '6')
                     and (dte-client = 'DCC')
121114              WRITE CCC-EXTRACT-RECORD
121114                                 FROM WS-EXTRACT-RECORD
121114           ELSE
121114              IF DE-CARRIER = '7'
121114                 WRITE CCC-EXTRACT-RECORD
121114                                 FROM WS-EXTRACT-RECORD
121114                 MOVE '40'     TO FX-TRAN-TYPE
121114                 MULTIPLY COMM-WRK BY -1 GIVING FX-AMOUNT
121114                 WRITE CCC-EXTRACT-RECORD
121114                                 FROM WS-EXTRACT-RECORD
101708              ELSE
122205                 WRITE EXTRACT-RECORD
122205                                 FROM WS-EXTRACT-RECORD
                    end-if
122205           END-IF
              ELSE                                                      
                 MULTIPLY COMM-WRK BY -1 GIVING FX-AMOUNT               
111616           IF (DE-CARRIER = '3' or '4' or '5' OR '6')
                     and (dte-client = 'DCC')
121114              WRITE CCC-EXTRACT-RECORD
121114                                 FROM WS-EXTRACT-RECORD
121114           ELSE
121114              IF DE-CARRIER = '7'
121114                 WRITE CCC-EXTRACT-RECORD
121114                                 FROM WS-EXTRACT-RECORD
121114                 MOVE '40'     TO FX-TRAN-TYPE
121114                 move COMM-WRK to FX-AMOUNT
121114                 WRITE CCC-EXTRACT-RECORD
121114                                 FROM WS-EXTRACT-RECORD
101708              ELSE
122205                 WRITE EXTRACT-RECORD
122205                                 FROM WS-EXTRACT-RECORD
121114              end-if
101708           END-IF
              END-IF                                                    
           END-IF                                                       

           .
       3500-EXIT.                                                       
           EXIT.                                                        

       4000-CID-AH-PREMIUM.                                                 

           MOVE '40'                   TO FX-TRAN-TYPE

           IF DE-TRANS = 'I'
              MOVE '01'                TO FX-SUB-TYPE
              MULTIPLY DE-AH-PRM BY -1 GIVING FX-AMOUNT
           ELSE
              IF DE-TRANS = 'C'
                 MOVE '02'             TO FX-SUB-TYPE
                 MOVE DE-AH-RFND       TO FX-AMOUNT
              END-IF
           END-IF

112906     IF DE-CARRIER = '8'
112906        MOVE '04'                TO FX-SUB-TYPE
112906     END-IF

           IF DE-TRANS = 'I' OR 'C'
              WRITE EXTRACT-RECORD     FROM WS-EXTRACT-RECORD
           END-IF

           .                                                            
       4000-EXIT.                                                       
           EXIT.                                                        
                                                                        
       4500-DCC-AH-PREMIUM.                                                 
      *                                                                 
           MOVE '40'                   TO FX-TRAN-TYPE                                    
           MOVE +0                     TO WRK, COMM-WRK                                     

011410     EVALUATE TRUE
011410
011410      WHEN (CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L')
011410         AND (DE-TRANS = 'I')
011410         MULTIPLY DE-LF-PRM-ALT BY -1 GIVING FX-AMOUNT
011410         MOVE '01'             TO FX-SUB-TYPE
111616         IF (DE-CARRIER = '3' or '4' or '5' OR '6' or '7')
                  and (dte-client = 'DCC')
011410            WRITE CCC-EXTRACT-RECORD FROM WS-EXTRACT-RECORD
011410         ELSE
011410            WRITE EXTRACT-RECORD FROM WS-EXTRACT-RECORD
011410         END-IF
011410      WHEN (CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L')
011410         AND (DE-TRANS = 'C')
092017         IF (DE-REI-AHRFND NOT = ZEROS)
092017            and (de-ah-rfnd not = zeros)
                  MOVE DE-REI-AHRFND   TO FX-AMOUNT
               ELSE
011410            COMPUTE CNC-FACT = DE-AH-RFND / DE-AH-PRM
011410            COMPUTE FX-AMOUNT ROUNDED = DE-LF-PRM-ALT * CNC-FACT
               END-IF
011410         MOVE '01'          TO FX-SUB-TYPE
111616         IF (DE-CARRIER = '3' or '4' or '5' OR '6' or '7')
                  and (dte-client = 'DCC')
011410            WRITE CCC-EXTRACT-RECORD FROM 
011410                         WS-EXTRACT-RECORD
011410         ELSE
011410            WRITE EXTRACT-RECORD FROM WS-EXTRACT-RECORD
011410         END-IF
011410
090214      WHEN (CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'D' or 'C')
011410         AND (DE-TRANS = 'I')
011410         MULTIPLY DE-REI-AHPRM BY -1 GIVING FX-AMOUNT
011410         MOVE '01'             TO FX-SUB-TYPE
111616         IF (DE-CARRIER = '3' or '4' or '5' OR '6' or '7')
                  and (dte-client = 'DCC')
011410            WRITE CCC-EXTRACT-RECORD FROM WS-EXTRACT-RECORD
011410         ELSE
011410            WRITE EXTRACT-RECORD FROM WS-EXTRACT-RECORD
011410         END-IF
011410
090214      WHEN (CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'D' or 'C')
011410         AND (DE-TRANS = 'C')
011410         COMPUTE CNC-FACT = DE-AH-RFND / DE-AH-PRM
011410         COMPUTE FX-AMOUNT ROUNDED = DE-REI-AHPRM * CNC-FACT
011410         MOVE '01'          TO FX-SUB-TYPE
111616         IF (DE-CARRIER = '3' or '4' or '5' OR '6' or '7')
                  and (dte-client = 'DCC')
011410            WRITE CCC-EXTRACT-RECORD FROM 
011410                         WS-EXTRACT-RECORD
011410         ELSE
011410            WRITE EXTRACT-RECORD FROM WS-EXTRACT-RECORD
011410         END-IF
011410
011410      WHEN OTHER
011410         PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10               
011410           IF (DE-A-PC (SUB) NOT = ZERO)
011410              AND (DE-AGT-TYPE (SUB) = 'C' OR 'D')
011410              IF DE-TRANS = 'I'
011410                 COMPUTE WRK ROUNDED =
011410                   DE-AH-PRM  * DE-A-PC (SUB)
011410              ELSE
011410                 IF DE-TRANS = 'C'
011410                    COMPUTE WRK ROUNDED =
011410                     DE-AH-RFND * DE-A-PC (SUB)
011410                 END-IF
011410              END-IF                                                  
011410              ADD WRK TO COMM-WRK                                     
011410           END-IF                                                     
011410         END-PERFORM
011410
011410        IF DE-TRANS = 'I'
011410           COMPUTE WRK       = DE-AH-PRM - COMM-WRK
011410          MOVE '01'             TO FX-SUB-TYPE                                  
011410          MULTIPLY WRK       BY -1 GIVING FX-AMOUNT
111616          IF (DE-CARRIER = '3' or '4' or '5' OR '6' or '7')
                   and (dte-client = 'DCC')
011410             WRITE CCC-EXTRACT-RECORD FROM WS-EXTRACT-RECORD
011410          ELSE
011410             WRITE EXTRACT-RECORD
011410                                 FROM WS-EXTRACT-RECORD
011410          END-IF
011410        ELSE
011410           IF DE-TRANS = 'C'
011410              COMPUTE WRK        = DE-AH-RFND - COMM-WRK
011410              MOVE '01'          TO FX-SUB-TYPE                                  
011410              MOVE WRK           TO FX-AMOUNT
111616              IF (DE-CARRIER = '3' or '4' or '5' OR '6' or '7')
                       and (dte-client = 'DCC')
011410                 WRITE CCC-EXTRACT-RECORD
011410                                 FROM WS-EXTRACT-RECORD
011410              ELSE
011410                 WRITE EXTRACT-RECORD
011410                                 FROM WS-EXTRACT-RECORD
011410              END-IF
011410           END-IF
011410        END-IF
011410     END-EVALUATE

           .                                                            
       4500-EXIT.                                                       
           EXIT.                                                        

       5000-CID-AH-COMMISSION.

           MOVE '50'                   TO FX-TRAN-TYPE                                    
           IF DE-TRANS = 'I' OR '8'
              MOVE '01'                TO FX-SUB-TYPE
           ELSE
              MOVE '02'                TO FX-SUB-TYPE
           END-IF

112906     IF DE-CARRIER = '8'
112906        MOVE '04'                TO FX-SUB-TYPE
112906     END-IF

           MOVE +0                     TO WRK COMM-WRK

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10               
             IF DE-A-PC (SUB) NOT = ZERO                                
010914           and (de-agt-type (sub) = 'C' OR 'D' OR 'O' OR 'P')
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

021412 5100-AHL-AH-RESERVE.                                            
021412
021412     IF DE-AH-RSV
021412         CONTINUE
021412     ELSE
021412         GO TO 5100-EXIT
021412     END-IF
021412*                                                                 
021412     MOVE '30'                   TO FX-TRAN-TYPE
021412                                                                  
021412     IF DE-CARRIER = '8'
021412        MOVE '34'                TO FX-TRAN-TYPE
021412     END-IF
021412
021412     MOVE +0                     TO WRK
021412                                    COMM-WRK                                     
021412
021412     IF DE-IBNR <> 0
021412           MOVE 'AHLRS IBNR'      TO FX-SYSTEM
021412           MOVE DE-IBNR           TO FX-AMOUNT
021412           MOVE 'AH IBNR RESERVE' TO FX-DESCRIPTION
021412           MOVE '18'              TO FX-SUB-TYPE
021412           WRITE EXTRACT-RECORD  FROM WS-EXTRACT-RECORD
021412     END-IF                                                       
021412
021412     IF DE-PAYCUR <> 0
021412           MOVE 'AHLRS PTC '      TO FX-SYSTEM
021412           MOVE DE-PAYCUR         TO FX-AMOUNT
021412           MOVE 'AH PAYCUR RESV ' TO FX-DESCRIPTION
021412           MOVE '19'              TO FX-SUB-TYPE
021412           WRITE EXTRACT-RECORD  FROM WS-EXTRACT-RECORD
021412     END-IF                                                       
021412
021412     IF DE-FUTRSV <> 0
021412           MOVE 'AHLRS CICS'      TO FX-SYSTEM
021412           MOVE DE-FUTRSV         TO FX-AMOUNT
021412           MOVE 'AH FUT RESERVE ' TO FX-DESCRIPTION
021412           MOVE '17'              TO FX-SUB-TYPE
021412           WRITE EXTRACT-RECORD  FROM WS-EXTRACT-RECORD
021412     END-IF                                                       
021412
021412     .
021412 5100-EXIT.                                                       
021412     EXIT.                                                        
021412

       5250-CSI-AH-COMMISSION.                                              

           MOVE '80'                   TO FX-TRAN-TYPE                                    
           MOVE '01'                   TO FX-SUB-TYPE                                  
           MOVE +0                     TO WRK, COMM-WRK                                     

100404     PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10               
100404       IF DE-A-PC (SUB) NOT = ZERO
011116          IF DE-AGT-TYPE (SUB) = 'P'
100404             IF DE-TRANS = 'I' OR '8'                                
011410                IF CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'D'
011410                   COMPUTE WRK ROUNDED =
011410                      DE-REI-AHPRM * DE-A-PC (SUB)
011410                ELSE
100404                  COMPUTE WRK ROUNDED = DE-AH-PRM  * DE-A-PC (SUB)
011410                END-IF
                      MOVE +0          TO DE-A-PC (SUB)
100404             ELSE                                                    
011410                IF CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'D'
011410                   COMPUTE CNC-FACT = DE-AH-RFND / DE-AH-PRM
011410                   COMPUTE WS-WORK-REF ROUNDED = DE-REI-AHPRM *
011410                      CNC-FACT
011410                   COMPUTE WRK ROUNDED = WS-WORK-REF *
011410                     DE-A-PC (SUB)
011410                ELSE
100404                  COMPUTE WRK ROUNDED = DE-AH-RFND * DE-A-PC (SUB)
011410                END-IF
                      MOVE +0          TO DE-A-PC (SUB)
100404             END-IF
                   ADD WRK             TO COMM-WRK
                ELSE
                   IF DE-AGT-TYPE (SUB) = 'M'
                      IF DE-TRANS = 'I' OR '8'
011410                  IF CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'D'
011410                    COMPUTE WRK ROUNDED =
011410                      DE-REI-AHPRM * DE-A-PC (SUB)
011410                      MOVE ZEROS TO DE-A-PC (SUB)
011410                  ELSE
                           COMPUTE WRK ROUNDED = DE-LF-PRM-ALT *
                            DE-A-PC (SUB)
                           MOVE +0       TO DE-A-PC (SUB)
011410                  END-IF
                      ELSE
                         COMPUTE CNC-FACT = DE-AH-RFND / DE-AH-PRM
                         IF CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'D'
                            COMPUTE WRK ROUNDED = DE-REI-AHPRM
                               * CNC-FACT
                           MOVE +0       TO DE-A-PC (SUB)
                         ELSE
                           COMPUTE WRK ROUNDED = (DE-LF-PRM-ALT
                              * CNC-FACT) * DE-A-PC (SUB)
                           DISPLAY ' M CANCEL ' DE-CERT-NO ' '
                             DE-LF-PRM-ALT '  ' DE-A-PC (SUB) ' '
                             DE-AH-RFND '  ' DE-AH-PRM ' '
                              CNC-FACT
                           MOVE +0       TO DE-A-PC (SUB)
                         END-IF
                      END-IF
                      ADD WRK          TO COMM-WRK
                   END-IF
                end-if
100404       END-IF                                                     
100404     END-PERFORM                                                  

           IF COMM-WRK NOT = ZERO                                       
              IF DE-TRANS = 'I' OR '8'                                  
                 MOVE COMM-WRK TO FX-AMOUNT                             
111616*          WRITE CSI-EXTRACT-RECORD
111616*                                FROM WS-EXTRACT-RECORD
              ELSE                                                      
                 MULTIPLY COMM-WRK BY -1 GIVING FX-AMOUNT               
111616*          WRITE CSI-EXTRACT-RECORD
111616*                                FROM WS-EXTRACT-RECORD
              END-IF                                                    
           END-IF                                                       

           .
       5250-EXIT.                                                       
           EXIT.                                                        
                                                                        
101708 5260-CCC-AH-COMMISSION.                                              

101708     MOVE '80'                   TO FX-TRAN-TYPE                                    
101708     MOVE '01'                   TO FX-SUB-TYPE                                  
101708     MOVE +0                     TO WRK, COMM-WRK                                     
101708
101708     PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10               
101708        IF DE-AGT-TYPE (SUB) = 'M'
101708           IF DE-TRANS = 'I' OR '8'
101708              COMPUTE WRK ROUNDED = DE-LF-PRM-ALT *
101708                 DE-A-PC (SUB)
101708              MOVE +0       TO DE-A-PC (SUB)
101708           ELSE
101708              COMPUTE CNC-FACT = DE-AH-RFND / DE-AH-PRM
101708              COMPUTE WRK ROUNDED = (DE-LF-PRM-ALT
101708                 * CNC-FACT) * DE-A-PC (SUB)
101708              MOVE +0       TO DE-A-PC (SUB)
101708           END-IF
101708           ADD WRK          TO COMM-WRK
101708        END-IF
101708     END-PERFORM                                                  
101708
101708     IF COMM-WRK NOT = ZERO                                       
101708        IF DE-TRANS = 'I' OR '8'                                  
101708           MOVE COMM-WRK TO FX-AMOUNT                             
101708           WRITE CCC-EXTRACT-RECORD
101708                                 FROM WS-EXTRACT-RECORD
101708        ELSE                                                      
101708           MULTIPLY COMM-WRK BY -1 GIVING FX-AMOUNT               
101708           WRITE CCC-EXTRACT-RECORD
101708                                 FROM WS-EXTRACT-RECORD
101708        END-IF                                                    
101708     END-IF                                                       
101708
101708
101708     .
101708 5260-EXIT.                                                       
101708     EXIT.                                                        

       5500-DCC-AH-COMMISSION.                                              

           MOVE '50'                   TO FX-TRAN-TYPE                                    
           MOVE '01'                   TO FX-SUB-TYPE                                  
                                                                        
           MOVE +0                     TO WRK, COMM-WRK                                     
                                                                        
100404     PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10      
100404       IF DE-A-PC (SUB) NOT = ZERO
011116          IF DE-AGT-TYPE (SUB) = 'P'
100404             IF DE-TRANS = 'I' OR '8'
011410                IF CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'D'
011410                   COMPUTE WRK ROUNDED =
011410                      DE-REI-AHPRM * DE-A-PC (SUB)
011410                ELSE
100404                   COMPUTE WRK ROUNDED =
                           DE-AH-PRM  * DE-A-PC (SUB)
011410                END-IF
100404             ELSE                                                    
011410                IF CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'D'
011410                   COMPUTE CNC-FACT = DE-AH-RFND / DE-AH-PRM
011410                   COMPUTE WS-WORK-REF ROUNDED = DE-REI-AHPRM *
011410                      CNC-FACT
011410                   COMPUTE WRK ROUNDED = WS-WORK-REF *
011410                     DE-A-PC (SUB)
011410                ELSE
100404                   COMPUTE WRK ROUNDED =
                            DE-AH-RFND * DE-A-PC (SUB)
011410                END-IF
100404             END-IF
                   ADD WRK             TO COMM-WRK
100404          ELSE
011410            IF DE-AGT-TYPE (SUB) = 'M'
011410               IF CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'D'
011410                  IF DE-TRANS = 'I' OR '8'
011410                     COMPUTE WRK ROUNDED = DE-REI-AHPRM
011410                        * DE-A-PC (SUB)
011410                  ELSE
011410                     COMPUTE CNC-FACT = DE-AH-RFND / DE-AH-PRM
011410                     COMPUTE WS-WORK-REF ROUNDED = DE-REI-AHPRM
011410                       * CNC-FACT
011410                     COMPUTE WRK ROUNDED = WS-WORK-REF *
011410                       DE-A-PC (SUB)
011410                  END-IF
011410               END-IF
                  ELSE
100404              IF DE-AGT-TYPE (SUB) = 'K'
                       IF DE-TRANS = 'I' OR '8'
                          COMPUTE WRK ROUNDED = DE-A-PC (SUB) * +1000
                       ELSE
                          COMPUTE CNC-FACT = DE-AH-RFND / DE-AH-PRM
100404                    COMPUTE WRK ROUNDED = (DE-A-PC (SUB) * +1000)
                             * CNC-FACT
                       END-IF
                       ADD WRK          TO COMM-WRK
100404              END-IF
                  end-if
                END-IF
100404       END-IF                                                     
100404     END-PERFORM                                                  

           IF COMM-WRK NOT = ZERO                                       
              IF DE-TRANS = 'I' OR '8'                                  
                 MOVE COMM-WRK TO FX-AMOUNT                             
111616           IF (DE-CARRIER = '3' or '4' or '5' OR '6')
                    and (dte-client = 'DCC')
121114              WRITE CCC-EXTRACT-RECORD FROM WS-EXTRACT-RECORD
121114           ELSE
121114              IF DE-CARRIER = '7'
121114                 WRITE CCC-EXTRACT-RECORD FROM WS-EXTRACT-RECORD
121114                 MOVE '40'       TO FX-TRAN-TYPE
121114                 MULTIPLY COMM-WRK BY -1 GIVING FX-AMOUNT
121114                 WRITE CCC-EXTRACT-RECORD FROM WS-EXTRACT-RECORD
101708              ELSE
122205                 WRITE EXTRACT-RECORD
122205                                 FROM WS-EXTRACT-RECORD
101708              END-IF
122205           END-IF
              ELSE                                                      
                 MULTIPLY COMM-WRK BY -1 GIVING FX-AMOUNT               
111616           IF (DE-CARRIER = '3' or '4' or '5' OR '6')
                    and (dte-client = 'DCC')
121114              WRITE CCC-EXTRACT-RECORD FROM WS-EXTRACT-RECORD
121114           ELSE
121114              IF DE-CARRIER = '7'
121114                 WRITE CCC-EXTRACT-RECORD FROM WS-EXTRACT-RECORD
121114                 MOVE '40'       TO FX-TRAN-TYPE
121114                 move COMM-WRK   to FX-AMOUNT
121114                 WRITE CCC-EXTRACT-RECORD FROM WS-EXTRACT-RECORD
101708              ELSE
122205                 WRITE EXTRACT-RECORD
122205                                 FROM WS-EXTRACT-RECORD
                    end-if
122205           END-IF
              END-IF                                                    
           END-IF                                                       

           .
       5500-EXIT.                                                       
           EXIT.                                                        

       0000-HOUSEKEEPING.                                               

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

011116     IF DTE-CLIENT = 'DCC' OR 'VPP'
111616        OPEN OUTPUT CCC-FREEDOM-EXTRACT
122205     END-IF

           .
       0000-EXIT.                                                       
           EXIT.                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       0100-LOAD-BENEFIT-TABLE.                                         
      *                                                                 
           OPEN INPUT LOGIC-CONTROL-FILE                                
           IF ELCNTL-STATUS = '00' OR '97'
              MOVE '00'                TO ELCNTL-STATUS
           ELSE                                                         
              DISPLAY 'OPEN ERROR ' ELCNTL-STATUS ' ON ELCNTL'         
              ADD +1                   TO FORCE-DUMP
              perform abend-pgm
           END-IF

032813     move dte-client             to cf-company-id
032813     move '4'                    to cf-record-type
032813     move spaces                 to cf-access-cd-genl
032813     move +0                     to cf-sequence-no
032813
032813     start logic-control-file key >= cf-control-primary
032813
032813     IF ELCNTL-STATUS = '00'
032813        CONTINUE
032813     ELSE                                                         
032813        DISPLAY 'START ERR  ' ELCNTL-STATUS ' ON ELCNTL '
032813        cf-company-id ' ' cf-record-type ' ' cf-access-cd-genl
032813        ' ' cf-sequence-no
032813        ADD +1                   TO FORCE-DUMP
032813        perform abend-pgm
032813     END-IF
032813
032813     SET BEN-INDEX TO +1
           READ LOGIC-CONTROL-FILE next record
                                                                        
           PERFORM UNTIL ELCNTL-STATUS NOT = '00'                       
032813        IF CF-COMPANY-ID = dte-client
021412           AND CF-RECORD-TYPE = '4'
                 PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
                    MOVE CF-BENEFIT-NUMERIC (SUB)
                                       TO BEN-NUM (BEN-INDEX)
                    MOVE CF-BENEFIT-ALPHA (SUB)
                                       TO BEN-ALPHA (BEN-INDEX)
                    SET BEN-INDEX UP BY +1
021412              IF BEN-INDEX > 250 DISPLAY
                       'TABLE OVERFLOW IN ROUTINE 0100-LOAD-BEN'
                        ADD +1 TO FORCE-DUMP
                        perform abend-pgm
                    END-IF
                 END-PERFORM
              END-IF
              READ LOGIC-CONTROL-FILE next record
           END-PERFORM                                                  

           CLOSE LOGIC-CONTROL-FILE

           .                                                            
       0100-EXIT.                                                       
           EXIT.                                                        

103002 ABEND-PGM SECTION.                                               00026280
103002     DIVIDE WS-ZERO BY WS-ZERO GIVING WS-ZERO.                    00026290
103002 ABEND-EXIT.                                                      00026300
                                                                        
