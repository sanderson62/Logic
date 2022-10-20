       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.    FNB169.                                           
                                                                        
      ****************************************************************** 
      *                                                                * 
      *            FREEDOM INTERFACE FOR DCC CONTRACT FEES             *
      *                                                                * 
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 050704    2003080800002  SMVA  NEW PROGRAM FOR SECURE PAY
061405* 061405                   PEMA  ADD CLP STATE PROCESS FOR DCC
      ******************************************************************
                                                                        
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT DETEXTR              ASSIGN TO SYS010 
                                       FILE STATUS IS DETEXT-STATUS.                            
           SELECT FREEDOM-EXTRACT      ASSIGN TO SYS011
                                       ORGANIZATION IS LINE SEQUENTIAL.

122205     SELECT CSI-FREEDOM-EXTRACT  ASSIGN TO SYS012
122205                                 ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ERACCT               ASSIGN TO ERACCT
                                       ORGANIZATION IS INDEXED   
                                       ACCESS IS DYNAMIC  
                                       RECORD KEY IS AM-CONTROL-PRIMARY
                                       FILE STATUS IS ERACCT-STATUS. 
                                                                        
           SELECT DISK-DATE            ASSIGN TO SYS019.
                                                                        
           EJECT                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD  DETEXTR                                         
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

122205 FD  CSI-FREEDOM-EXTRACT                                              
122205     LABEL RECORDS ARE STANDARD                                   
122205     RECORDING MODE IS F                                          
122205     BLOCK CONTAINS 0 RECORDS.                                    
122205 01  CSI-EXTRACT-RECORD          PIC X(250).

                                                                        
       FD  ERACCT.                                        
           COPY ERCACCT.                                                
                                                                        
       FD  DISK-DATE                                                    
           COPY ELCDTEFD.                                               
                                                                        
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
       01  AGT-LEVEL                 COMP   PIC 9(04).  
       01  WS-CNC-FACT               COMP-3 PIC S9(03)V9(07) VALUE +0.
TEST   01  TEST-DISP-FX-AMT PIC ZZZZZZZZ9.99.
TEST   01  TEST-DISP-L-PC PIC ZZ.ZZZZZ.
TEST   01  TEST-DISP-A-PC PIC ZZ.ZZZZZ. 
       01  FILLER.                                                      
           05  WS-ZERO               COMP-3 PIC S9(03) VALUE +0.
           05  WS-ABEND-MESSAGE             PIC X(80)  VALUE SPACES.    
           05  PGM-SUB               COMP   PIC S9(04) VALUE +310.
           05  WS-RETURN-CODE        COMP-3 PIC S9(03) VALUE +0.

           05  DETEXT-STATUS                PIC X(02)  VALUE '00'.
               88  EOF-DETEXT                          VALUE '10'.

           05  DATE-SW                      PIC X(01)  VALUE ' '. 
               88  VALID-DATE                          VALUE 'V'.

           05  VALID-RECORD-SW              PIC X(01)  VALUE '0'.
               88  VALID-RECORD                        VALUE '1'.

           05  END-OF-SEARCH-SW             PIC X(01)  VALUE '0'.
               88  END-OF-SEARCH                       VALUE '1'.

           05  ERACCT-STATUS                PIC X(02)  VALUE '00'.

 
       01  WS-EXTRACT-RECORD.                                           
           COPY FNC022.                                                 
                                                                        
       01  FNX001-PARMS.                                                
           05  FNX001-DATA                  PIC X(50). 
           05  FNX001-CITY                  PIC X(30).
           05  FNX001-STATE                 PIC X(02).
           05  FNX001-ZIP                   PIC X(09).

       01  SYSTEM-DATE.
           05  SYS-MO                       PIC 9(02).
           05  SYS-DA                       PIC 9(02).
           05  SYS-CCYY                     PIC 9(04).

      ***** FUNCTION-DATE COPYBOOK
       COPY ELCFUNDT.

      ***** STATE EDIT TABLE
       COPY FNC018.

       COPY ELCDTECX.                                               
       COPY ELCDTEVR.                                               
                                                                        
       LINKAGE SECTION.                                                 
                                                                        
       01  PARM.                                                        
           05  PARM-LENGTH                  PIC S9(04) COMP. 
           05  CYCLE-DATE                   PIC X(08). 
                                                                        
                                                                        
                                                                        
           EJECT                                                        
                                                                        
       PROCEDURE DIVISION USING PARM.                                   
                                                                        
      *************************************************************     
       COPY ELCDTERX.                   
      *************************************************************     
           PERFORM 0000-HOUSEKEEPING       THRU 0000-EXIT                     
                                                                        
           PERFORM 1000-PROCESS            THRU 1000-EXIT 
               UNTIL EOF-DETEXT

           CLOSE DETEXTR
                 FREEDOM-EXTRACT
                 CSI-FREEDOM-EXTRACT
                 ERACCT                                                                        
           GOBACK.

                                                                        
       1000-PROCESS.                                                    

           PERFORM 1100-GET-LOGIC-RECORD   THRU 1100-EXIT                 
                WITH TEST AFTER
                    UNTIL VALID-RECORD OR EOF-DETEXT
                                                                        
           IF EOF-DETEXT
               GO TO 1000-EXIT
           END-IF
                                                                        
           IF DE-ADDL-CLP = +0
               GO TO 1000-EXIT
           END-IF

           MOVE SPACES                     TO WS-EXTRACT-RECORD                             

           MOVE 'LOGIC '                   TO FX-SOURCE-CODE 
           MOVE '11'                       TO FX-DIVISION 
           MOVE ' '                        TO FX-FY-REN

           MOVE CYCLE-DATE                 TO FX-POSTING-DATE
           MOVE DE-CERT                    TO FX-POLICY-NO

061405     IF DE-CLP-STATE = SPACES
061405        MOVE DE-STATE                TO DE-CLP-STATE
061405     END-IF
061405     MOVE DE-CLP-STATE               TO FX-STATE
      *    MOVE DE-STATE                   TO FX-STATE  
           MOVE 'Y'                        TO FX-LOC-CODE
                                                                        
           PERFORM 1300-GET-ADDR           THRU 1300-EXIT  
                                                                        
PEMTST*    DISPLAY ' CARRIER = ' DE-CARRIER
PEMTST*    MOVE '3'                    TO DE-CARRIER

122205     IF DE-CARRIER = '3' OR '4'
122205        MOVE 'CSIDCCFEES'        TO FX-SYSTEM
122205     ELSE
103002        MOVE 'LPAC LOGIC'        TO FX-SYSTEM
122205     END-IF

           MOVE DE-ACCT-PRIME              TO FX-AGENT-01 
           MOVE '6211'                     TO FX-COST-CENTER
           MOVE ' '                        TO FX-DISTR  
           MOVE ' '                        TO FX-SOURCE-ACCT
           MOVE ' '                        TO FX-REFERENCE 
                                                                        
           MOVE +1                         TO AGT-LEVEL


           PERFORM UNTIL AGT-LEVEL > +10

TEST  *    DISPLAY 'DE CERT ' DE-CERT
TEST  *    DISPLAY 'AGT LEVEL ' AGT-LEVEL
TEST  *    DISPLAY 'DE-AGT ' DE-AGT (AGT-LEVEL)
TEST  *    DISPLAY 'DE-AGT-TYPE ' DE-AGT-TYPE (AGT-LEVEL)
TEST  *    DISPLAY 'DE-TRAN ' DE-TRANS
TEST       MOVE DE-L-PC (AGT-LEVEL) TO TEST-DISP-L-PC
TEST  *    DISPLAY 'DE-L-PC ' TEST-DISP-L-PC
TEST       MOVE DE-A-PC (AGT-LEVEL) TO TEST-DISP-A-PC
TEST  *    DISPLAY 'DE-A-PC ' TEST-DISP-A-PC
 
               EVALUATE TRUE 
                   WHEN DE-TRANS = 'I'
                        AND DE-AGT-TYPE (AGT-LEVEL) = 'I' OR 'J'
                       MOVE 'ISSUE INCENTIVE'     TO FX-DESCRIPTION        
                       MOVE '01'                  TO FX-SUB-TYPE
                       COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                           * -1
                       PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                       MOVE '70'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF
                       MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                       MOVE '90'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF

                   WHEN DE-TRANS = 'I'
                        AND DE-AGT-TYPE (AGT-LEVEL) = 'L'
                       MOVE 'ISSUE LMBA FEE'      TO FX-DESCRIPTION        
                       MOVE '01'                  TO FX-SUB-TYPE
                       COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                           * -1
                       PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                       MOVE '75'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF
                       MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                       MOVE '95'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF

                   WHEN DE-TRANS = 'I'
                        AND DE-AGT-TYPE (AGT-LEVEL) = 'B'
                        AND DE-A-PC (AGT-LEVEL) NOT = +0
                       MOVE 'ISSUE CONTRACT FEE'  TO FX-DESCRIPTION        
                       MOVE '01'                  TO FX-SUB-TYPE
                       COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                           * -1
                       PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                       MOVE '72'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF
                       MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                       MOVE '92'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF

                   WHEN DE-TRANS = '8'
                        AND DE-AGT-TYPE (AGT-LEVEL) = 'I' OR 'J'
                       MOVE 'ISSUE RC-L INCENTIVE' 
                                                  TO FX-DESCRIPTION        
                       MOVE '01'                  TO FX-SUB-TYPE
                       COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                           * -1
                       PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                       MOVE '70'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF
                       MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                       MOVE '90'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF

                   WHEN DE-TRANS = '8'
                        AND DE-AGT-TYPE (AGT-LEVEL) = 'L'
                       MOVE 'ISSUE RC-L LMBA FEE' TO FX-DESCRIPTION        
                       MOVE '01'                  TO FX-SUB-TYPE
                       COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                           * -1
                       PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                       MOVE '75'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF
                       MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                       MOVE '95'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF

                   WHEN DE-TRANS = '8'
                        AND DE-AGT-TYPE (AGT-LEVEL) = 'B'
                        AND DE-A-PC (AGT-LEVEL) NOT = +0
                       MOVE 'ISSUE RC-L CONTRACT FEE'
                                                  TO FX-DESCRIPTION        
                       MOVE '01'                  TO FX-SUB-TYPE
                       COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                           * -1
                       PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                       MOVE '72'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF
                       MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                       MOVE '92'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF

                   WHEN DE-TRANS = 'C'
                        AND DE-AGT-TYPE (AGT-LEVEL) = 'I' OR 'J'
                       MOVE 'CANCEL INCENTIVE'    TO FX-DESCRIPTION        
                       MOVE '01'                  TO FX-SUB-TYPE
                       COMPUTE WS-CNC-FACT = DE-AH-RFND / DE-AH-PRM
                       COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                           * WS-CNC-FACT
                       PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                       MOVE '70'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF
                       MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                       MOVE '90'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF

                   WHEN DE-TRANS = 'C'
                        AND DE-AGT-TYPE (AGT-LEVEL) = 'L'
                       MOVE 'CANCEL LMBA FEE'     TO FX-DESCRIPTION        
                       MOVE '01'                  TO FX-SUB-TYPE
                       COMPUTE WS-CNC-FACT = DE-AH-RFND / DE-AH-PRM
                       COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                           * WS-CNC-FACT
                       PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                       MOVE '75'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF
                       MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                       MOVE '95'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF

                   WHEN DE-TRANS = 'C'
                        AND DE-AGT-TYPE (AGT-LEVEL) = 'B'
                        AND DE-A-PC (AGT-LEVEL) NOT = +0
                       MOVE 'CANCEL CONTRACT FEE' TO FX-DESCRIPTION        
                       MOVE '01'                  TO FX-SUB-TYPE
                       COMPUTE WS-CNC-FACT = DE-AH-RFND / DE-AH-PRM
                       COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                           * WS-CNC-FACT
                       PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                       MOVE '72'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF
                       MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                       MOVE '92'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF

                   WHEN DE-TRANS = '7'
                        AND DE-AGT-TYPE (AGT-LEVEL) = 'I' OR 'J'
                       MOVE 'CANCEL RC-L INCENTIVE'  
                                                  TO FX-DESCRIPTION        
                       MOVE '01'                  TO FX-SUB-TYPE
                       COMPUTE WS-CNC-FACT = DE-AH-RFND / DE-AH-PRM
                       COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                           * WS-CNC-FACT
                       PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                       MOVE '70'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF
                       MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                       MOVE '90'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF

                   WHEN DE-TRANS = '7'
                        AND DE-AGT-TYPE (AGT-LEVEL) = 'L'
                       MOVE 'CANCEL RC-L LMBA FEE' 
                                                  TO FX-DESCRIPTION        
                       MOVE '01'                  TO FX-SUB-TYPE
                       COMPUTE WS-CNC-FACT = DE-AH-RFND / DE-AH-PRM
                       COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                           * WS-CNC-FACT
                       PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                       MOVE '75'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF
                       MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                       MOVE '95'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF

                   WHEN DE-TRANS = '7'
                        AND DE-AGT-TYPE (AGT-LEVEL) = 'B'
                        AND DE-A-PC (AGT-LEVEL) NOT = +0
                       MOVE 'CANCEL RC-L CONTRACT FEE' 
                                                  TO FX-DESCRIPTION        
                       MOVE '01'                  TO FX-SUB-TYPE
                       COMPUTE WS-CNC-FACT = DE-AH-RFND / DE-AH-PRM
                       COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                           * WS-CNC-FACT
                       PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                       MOVE '72'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF
                       MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                       MOVE '92'                  TO FX-TRAN-TYPE     
                       IF DE-CARRIER = '3' OR '4'
                          WRITE CSI-EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       ELSE
                          WRITE EXTRACT-RECORD
                                       FROM WS-EXTRACT-RECORD
                       END-IF
               END-EVALUATE                                                 
TEST           MOVE FX-AMOUNT TO TEST-DISP-FX-AMT  
TEST  *        DISPLAY 'FX AMT ' TEST-DISP-FX-AMT
TEST  *        DISPLAY ' '
                                                                        
               ADD +1                      TO AGT-LEVEL
           END-PERFORM

           .
       1000-EXIT.                                                       
           EXIT.                                                        


       1100-GET-LOGIC-RECORD.                                           

           MOVE '0'                        TO VALID-RECORD-SW                                  
           READ DETEXTR                                    
               AT END GO TO 1100-EXIT
           END-READ
                                   
           IF DE-REIN NOT = SPACE                                       
               GO TO 1100-EXIT
           END-IF

           IF DE-TRANS = 'I' OR 'C' OR '7' OR '8'                       
               CONTINUE                                                 
           ELSE                                                         
               GO TO 1100-EXIT
           END-IF

           IF DE-ENTRY-STATUS = 'M'
              GO TO 1100-EXIT
           END-IF

           IF (DE-TRANS = 'I' OR '8')                                   
             AND (DE-ENTRY-STATUS = '3' OR '5')                  
               GO TO 1100-EXIT
           END-IF
                                                                        
           IF (DE-LF-TYPE = ZERO)  AND                                  
              (DE-AH-TYPE = ZERO)                                       
                 GO TO 1100-EXIT
           END-IF
                                                                        
           SET VALID-RECORD TO TRUE                                     

           .                                                            
       1100-EXIT.                                                       
           EXIT.                                                        
                                                                        
                                                                        
       1300-GET-ADDR.                                                   

           MOVE DE-COMPANY-CD              TO AM-COMPANY-CD
           MOVE DE-CNTRL1                  TO AM-CNTRL-1        
           MOVE LOW-VALUES                 TO AM-EXPIRATION-DT 

           START ERACCT    
             KEY NOT LESS THAN AM-CONTROL-PRIMARY

           IF ERACCT-STATUS NOT = '00' 
               DISPLAY 'ERROR ON ERACCT START ' ERACCT-STATUS
               GO TO 1300-EXIT
           END-IF

           MOVE '0' TO END-OF-SEARCH-SW                                 
                                                                        
           PERFORM UNTIL END-OF-SEARCH                                  
               READ ERACCT NEXT                            

               IF ERACCT-STATUS = '00'                                   
                  AND AM-COMPANY-CD = DE-COMPANY-CD                       
                  AND AM-CNTRL-1 = DE-CNTRL1                           
                   MOVE AM-CITY            TO FX-CITY                          
                   MOVE AM-ZIP             TO FX-ZIP-CODE                      
               ELSE                                                
                   DISPLAY 'ERACCT READ NEXT STATUS ' ERACCT-STATUS
                   SET END-OF-SEARCH       TO TRUE                        
               END-IF                                              
           END-PERFORM                                                  
                                                                        
           MOVE FX-CITY                    TO FNX001-DATA
           CALL 'FNX001' USING FNX001-PARMS                             
           MOVE FNX001-CITY                TO FX-CITY

           .
       1300-EXIT.                                                       
           EXIT.                                                        
                                                                        

       1400-OTHER-FX-DATA.

           IF FX-STATE NOT= SPACES
               SEARCH ALL STATE-TABLE
                   AT END DISPLAY 'INVALID STATE CODE: ' FX-STATE
                   WHEN ST-STATE (ST-INDEX) = FX-STATE
                       MOVE ST-ALT-STATE (ST-INDEX) TO FX-STATE
               END-SEARCH
           END-IF

           MOVE SYSTEM-DATE                TO FX-JOURNAL-DATE

      **** THE '*' IN POSITION 250 ENSURES A 250 BYTE RECORD IS PASSED
      **** TO FREEDOM - PREVENTS TRUNCATION OF BLANK FIELDS
           MOVE '*'                        TO WS-EXTRACT-RECORD(250:1)

           .
       1400-EXIT.
           EXIT.


       0000-HOUSEKEEPING.                                               

           CALL 'DATEEDIT' USING CYCLE-DATE,  DATE-SW                   
           IF NOT VALID-DATE                                            
               DISPLAY 'INVALID DATE PARAMETER: ' CYCLE-DATE            
               PERFORM ABEND-PGM           THRU ABEND-EXIT 
           END-IF

           MOVE FUNCTION CURRENT-DATE      TO FUNCTION-DATE
           MOVE WS-FN-MO                   TO SYS-MO
           MOVE WS-FN-DA                   TO SYS-DA
           MOVE WS-FN-CCYR                 TO SYS-CCYY

           OPEN INPUT  ERACCT                              
                       DETEXTR                             
                OUTPUT FREEDOM-EXTRACT CSI-FREEDOM-EXTRACT

           IF ERACCT-STATUS = '00' OR '97'                              
               CONTINUE                                                 
           ELSE                                                         
               DISPLAY 'OPEN ERROR ' ERACCT-STATUS ' ON ERACCT'         
           END-IF
                                                                        
           .
       0000-EXIT.                                                       
           EXIT.                                                        
                                                                        

       ABEND-PGM SECTION.                                   
           DIVIDE WS-ZERO BY WS-ZERO       GIVING WS-ZERO.   
       ABEND-EXIT.                                         
                                                                        
