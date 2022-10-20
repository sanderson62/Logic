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
021009* 021009  IR2009020900001  PEMA  ADD AGT TYPE J AS INCENTIVE
022009* 022009    2008050500001  AJRA  ADD PROCESSING FOR CCC
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
112911* 112911  CR2011083000003  PEMA  ADD SPPDDF TRUNCATED
080212* 080212  IR2012080100002  PEMA  CORRECT AGT TYPE N
082912* 082912  IR2012080700001  PEMA  CORRECT REF ADMIN FEE
080613* 080612  IR2013080500001  PEMA  CORRECT REF CSO ADMIN FEE
100413* 100413  IR2013100100001  PEMA  INCREASE PDEF OCCURS
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
073114* 073114  CR2014012300001  PEMA  ADD CU CARRIER 7 PROCESSING
010816* 010816  CR2015072000001  PEMA  ADD AGT TYP O TO FEES INTERFACE
042216* 042216  CR2016032400001  PEMA  MORE CARRIER 7 CHANGES
111616* 111616  CR2016092200001  PEMA  Remove CSI processing
032922* 032922  CR2021100800003  PEMA  Increase number of Prod Defs to 11
      ******************************************************************
                                                                        
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT DETEXTR              ASSIGN TO SYS010 
                                       FILE STATUS IS DETEXT-STATUS.                            
           SELECT FREEDOM-EXTRACT      ASSIGN TO SYS011
                                       ORGANIZATION IS LINE SEQUENTIAL.

022009     SELECT CCC-FREEDOM-EXTRACT  ASSIGN TO SYS013
022009                                 ORGANIZATION IS LINE SEQUENTIAL.
022009
           SELECT ERACCT               ASSIGN TO ERACCT
                                       ORGANIZATION IS INDEXED   
                                       ACCESS IS DYNAMIC  
                                       RECORD KEY IS AM-CONTROL-PRIMARY
                                       FILE STATUS IS ERACCT-STATUS. 
                                                                        
           SELECT ERPDEF           ASSIGN TO ERPDEF
                                   ACCESS IS DYNAMIC                    
                                   ORGANIZATION IS INDEXED              
                                   FILE STATUS IS ERPDEF-FILE-STATUS    
                                   RECORD KEY IS PD-CONTROL-PRIMARY.    

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
                                                                        
022009 FD  CCC-FREEDOM-EXTRACT                                              
022009     LABEL RECORDS ARE STANDARD                                   
022009     RECORDING MODE IS F                                          
022009     BLOCK CONTAINS 0 RECORDS.                                    
022009 01  CCC-EXTRACT-RECORD          PIC X(250).
022009
       FD  ERACCT.                                        
           COPY ERCACCT.                                                
                                                                        
       FD  ERPDEF.

                                   COPY ERCPDEF.

       FD  DISK-DATE                                                    
           COPY ELCDTEFD.                                               
                                                                        
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
       77  B-SUB                   PIC S9(4)   COMP.
100413 77  D1                      PIC S9(5) COMP-3 VALUE +0.
       77  P1                      PIC S999 COMP-3 VALUE +0.
       77  P2                      PIC S999 COMP-3 VALUE +0.
       77  CNC-WK                  PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-MONTH                PIC S999     COMP-3 VALUE +0.
       77  WS-HI-UEF               PIC S9V999   COMP-3 VALUE +0.
       77  WS-LO-UEF               PIC S9V999   COMP-3 VALUE +0.
       77  DD-IU-SW                    PIC X   VALUE ' '.
           88  DD-IU-PRESENT                 VALUE 'Y'.
       77  WS-FACT-CERT            PIC X(11) VALUE SPACES.
       77  A-OW                        PIC S9(7)V99 COMP-3 VALUE +0.
       77  W-FACTOR            PIC S9(09)V9(08) COMP-3 VALUE +0.
       77  WS-CLP-MO3              PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-COMM-MO3             PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-DDF-TERM             PIC S999 COMP-3 VALUE +0.

       01  WS-DCC-PRODUCT-CODE         PIC XXX  VALUE SPACES.
       01  AGT-LEVEL                 COMP   PIC 9(04).  
       01  WS-CNC-FACT               COMP-3 PIC S9(03)V9(07) VALUE +0.
011410 01  WS-LF-CNC-FACT            COMP-3 PIC S9(03)V9(07) VALUE +0.
011410 01  WS-WORK-AMT               COMP-3 PIC S9(7)V99 VALUE +0.
011410 01  WS-LF-WORK                  PIC S9(7)V99 VALUE +0 COMP-3.
011410 01  WS-AH-WORK                  PIC S9(7)V99 VALUE +0 COMP-3.
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
           05  ERPDEF-FILE-STATUS           PIC XX     VALUE '00'.

 
051414 01  max-pdef                    pic s9(5) comp-3 value +5000.
       01  DCC-DDF-WORK-AREA.
051414     05  F OCCURS 5000.
               10  DD-STATE                 PIC XX.
               10  DD-PRODUCT-CD            PIC XXX.
               10  DD-FILLER                PIC X(7).
               10  DD-BEN-TYPE              PIC X.
               10  DD-BEN-CODE              PIC XX.
               10  DD-PROD-EXP-DT           PIC XX.
               10  DD-1ST-YR-ALLOW          PIC S999V99 COMP-3.
032922         10  DD-PROD-DATA OCCURS 11.
                   15  DD-PROD-CODE         PIC X.
               10  DD-EARN-FACTORS.                                                  
                   15  F OCCURS 15.
                       20  F OCCURS 15.
                           25  DD-UEP-FACTOR    PIC S9V9(3)     COMP-3.
               10  DD-TRUNCATED             PIC X.

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
022009           CCC-FREEDOM-EXTRACT 
                 ERACCT
           GOBACK.

                                                                        
       1000-PROCESS.                                                    

           PERFORM 1100-GET-LOGIC-RECORD   THRU 1100-EXIT                 
                WITH TEST AFTER
                    UNTIL VALID-RECORD OR EOF-DETEXT
                                                                        
           IF EOF-DETEXT
               GO TO 1000-EXIT
           END-IF
                                                                        
011410*    IF DE-ADDL-CLP = +0
011410*        GO TO 1000-EXIT
011410*    END-IF

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

042216     evaluate true
111616        when de-carrier = '3' or '4' or '5' or '6'
042216           MOVE 'CCCDCCFEES'     TO FX-SYSTEM
042216           MOVE '50'             TO FX-DIVISION
042216        when de-carrier = '7'
042216           MOVE 'CCCDCCFEES'     TO FX-SYSTEM
042216           MOVE '5C'             TO FX-DIVISION
042216        when other
042216           MOVE 'LPAC LOGIC'     TO FX-SYSTEM
042216     end-evaluate

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
 
               EVALUATE DE-TRANS
                WHEN 'I'
                  EVALUATE TRUE
021009              WHEN DE-AGT-TYPE (AGT-LEVEL) = 'I' OR 'J'
                      MOVE 'ISSUE INCENTIVE'     TO FX-DESCRIPTION        
                      MOVE '01'                  TO FX-SUB-TYPE
                      COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                          * -1
                      PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                      MOVE '70'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
                      MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                      MOVE '90'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT

011410              WHEN DE-AGT-TYPE (AGT-LEVEL) = 'L' OR 'A'
                      MOVE 'ISSUE LMBA FEE'      TO FX-DESCRIPTION        
                      MOVE '01'                  TO FX-SUB-TYPE
011410                IF DE-AGT-TYPE (AGT-LEVEL) = 'L'
011410                   COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                          * -1
011410                ELSE
011410                   COMPUTE WS-LF-WORK ROUNDED = DE-REI-LFPRM *
011410                      DE-L-PC (AGT-LEVEL)
011410                   COMPUTE WS-AH-WORK ROUNDED = DE-REI-AHPRM *
011410                      DE-A-PC (AGT-LEVEL)
011410                   COMPUTE FX-AMOUNT ROUNDED = (WS-LF-WORK + 
011410                      WS-AH-WORK) * -1
011410                END-IF
                      PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                      MOVE '75'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
                      MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                      MOVE '95'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT

                    WHEN (DE-AGT-TYPE (AGT-LEVEL) = 'B')
                      AND (DE-A-PC (AGT-LEVEL) NOT = +0)
                      MOVE 'ISSUE CONTRACT FEE'  TO FX-DESCRIPTION        
                      MOVE '01'                  TO FX-SUB-TYPE
                      COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                          * -1
                      PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                      MOVE '72'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
                      MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                      MOVE '92'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT

                    WHEN (DE-AGT-TYPE (AGT-LEVEL) = 'N')
                      AND (DE-A-PC (AGT-LEVEL) NOT = +0)
                      MOVE 'ISSUE CSO ADMIN FEE'  TO FX-DESCRIPTION        
                      MOVE '01'                  TO FX-SUB-TYPE
                      COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                          * -1
                      PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                      MOVE '77'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
                      MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                      MOVE '97'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
052814              WHEN (DE-AGT-TYPE (AGT-LEVEL) = 'S')
052814                AND (DE-A-PC (AGT-LEVEL) NOT = +0)
052814                MOVE 'ISSUE CSO ADMIN FEE'  TO FX-DESCRIPTION        
052814                MOVE '01'                  TO FX-SUB-TYPE
052814                COMPUTE FX-AMOUNT rounded =
052814                   DE-A-PC (AGT-LEVEL) * de-ah-prm * -1
052814                PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
052814                MOVE '77'                  TO FX-TRAN-TYPE     
052814                PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
052814                MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
052814                MOVE '97'                  TO FX-TRAN-TYPE     
052814                PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
010816              WHEN (DE-AGT-TYPE (AGT-LEVEL) = 'O')
010816                AND (DE-A-PC (AGT-LEVEL) NOT = +0)
010816                MOVE 'ISSUE GA FEE       '  TO FX-DESCRIPTION        
010816                MOVE '01'                  TO FX-SUB-TYPE
010816                COMPUTE FX-AMOUNT =
010816                   DE-AH-PRM * DE-A-PC (AGT-LEVEL) * -1
010816                PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
010816                MOVE '76'                  TO FX-TRAN-TYPE     
010816                PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
010816                MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
010816                MOVE '96'                  TO FX-TRAN-TYPE     
010816                PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
                  END-EVALUATE
                WHEN '8'
                  EVALUATE TRUE
021009              WHEN DE-AGT-TYPE (AGT-LEVEL) = 'I' OR 'J'
                      MOVE 'ISSUE RC-L INCENTIVE' 
                                                 TO FX-DESCRIPTION        
                      MOVE '01'                  TO FX-SUB-TYPE
                      COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                          * -1
                      PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                      MOVE '70'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
                      MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                      MOVE '90'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT

011410              WHEN DE-AGT-TYPE (AGT-LEVEL) = 'L' OR 'A'
                      MOVE 'ISSUE RC-L LMBA FEE' TO FX-DESCRIPTION        
                      MOVE '01'                  TO FX-SUB-TYPE
011410                IF DE-AGT-TYPE (AGT-LEVEL) = 'L'
                         COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                          * -1
011410                ELSE
011410                   COMPUTE FX-AMOUNT ROUNDED = ((DE-REI-LFPRM
011410                      * DE-L-PC (AGT-LEVEL)) + (DE-REI-AHPRM
011410                      * DE-A-PC (AGT-LEVEL))) * -1
011410                END-IF
                      PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                      MOVE '75'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
                      MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                      MOVE '95'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT

                    WHEN (DE-AGT-TYPE (AGT-LEVEL) = 'B')
                      AND (DE-A-PC (AGT-LEVEL) NOT = +0)
                      MOVE 'ISSUE RC-L CONTRACT FEE'
                                                 TO FX-DESCRIPTION        
                      MOVE '01'                  TO FX-SUB-TYPE
                      COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                          * -1
                      PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                      MOVE '72'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
                      MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                      MOVE '92'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT

010816              WHEN (DE-AGT-TYPE (AGT-LEVEL) = 'O')
010816                AND (DE-A-PC (AGT-LEVEL) NOT = +0)
010816                MOVE 'ISSUE RC GA FEE    '  TO FX-DESCRIPTION        
010816                MOVE '01'                  TO FX-SUB-TYPE
010816                COMPUTE FX-AMOUNT =
010816                   DE-AH-PRM * DE-A-PC (AGT-LEVEL) * -1
010816                PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
010816                MOVE '76'                  TO FX-TRAN-TYPE     
010816                PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
010816                MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
010816                MOVE '96'                  TO FX-TRAN-TYPE     
010816                PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
                  END-EVALUATE
                WHEN 'C'
                  EVALUATE TRUE
                    WHEN DE-AGT-TYPE (AGT-LEVEL) = 'I'
                      MOVE 'CANCEL MGT FEE  '    TO FX-DESCRIPTION        
                      MOVE '01'                  TO FX-SUB-TYPE
                      COMPUTE A-OW = DE-A-PC (AGT-LEVEL) * 1000
                      PERFORM 2530-CALC-DDF-FEES THRU 2530-EXIT
                      move cnc-wk to fx-amount
                      PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                      MOVE '70'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
                      MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                      MOVE '90'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
021009              WHEN DE-AGT-TYPE (AGT-LEVEL) = 'J'
                      MOVE 'CANCEL SLS MKT  '    TO FX-DESCRIPTION        
                      MOVE '01'                  TO FX-SUB-TYPE
                      COMPUTE WS-CNC-FACT = DE-AH-RFND / DE-AH-PRM
                      IF DE-CANCEL-REASON = 'R'
                         MOVE +1      TO WS-CNC-FACT
                      END-IF
                      COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                          * WS-CNC-FACT
                      PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                      MOVE '70'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
                      MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                      MOVE '90'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT

011410              WHEN DE-AGT-TYPE (AGT-LEVEL) = 'L' OR 'A'
                      MOVE 'CANCEL LMBA FEE'     TO FX-DESCRIPTION        
                      MOVE '01'                  TO FX-SUB-TYPE
011410                IF DE-AGT-TYPE (AGT-LEVEL) = 'L'
011410                   IF DE-AH-PRM NOT = ZEROS
011410                      COMPUTE WS-CNC-FACT ROUNDED = DE-AH-RFND /
011410                         DE-AH-PRM
                            IF DE-CANCEL-REASON = 'R'
                               MOVE +1 TO WS-CNC-FACT
                            END-IF
011410                      COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL)
011410                         * 1000 * WS-CNC-FACT
011410                   END-IF
011410                ELSE
011410                   MOVE ZEROS   TO WS-WORK-AMT
011410                   IF DE-LF-PRM NOT = ZEROS
011410                      COMPUTE WS-CNC-FACT ROUNDED = DE-LF-RFND /
011410                         DE-LF-PRM
011410                      COMPUTE WS-WORK-AMT ROUNDED =
011410                        (DE-L-PC (AGT-LEVEL) * DE-REI-LFPRM)
011410                          * WS-CNC-FACT
011410                   END-IF
011410                   IF DE-AH-PRM NOT = ZEROS
011410                      COMPUTE WS-CNC-FACT ROUNDED = DE-AH-RFND /
011410                         DE-AH-PRM
011410                      COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT +
011410                     ((DE-A-PC (AGT-LEVEL) * DE-REI-AHPRM)
011410                        * WS-CNC-FACT)
011410                   END-IF
011410                   MOVE WS-WORK-AMT TO FX-AMOUNT
011410                END-IF
                      PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                      MOVE '75'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
                      MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                      MOVE '95'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT

                    WHEN (DE-AGT-TYPE (AGT-LEVEL) = 'B')
                      AND (DE-A-PC (AGT-LEVEL) NOT = +0)
                      MOVE 'CANCEL CONTRACT FEE' TO FX-DESCRIPTION        
                      MOVE '01'                  TO FX-SUB-TYPE
                      COMPUTE WS-CNC-FACT = DE-AH-RFND / DE-AH-PRM
                      COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                          * WS-CNC-FACT
                      PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                      MOVE '72'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
                      MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                      MOVE '92'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT

                    WHEN (DE-AGT-TYPE (AGT-LEVEL) = 'N')
                      AND (DE-A-PC (AGT-LEVEL) NOT = ZEROS)
                      MOVE 'CANCEL CSO ADMIN FEE' TO FX-DESCRIPTION        
                      MOVE '01'                  TO FX-SUB-TYPE

                      COMPUTE A-OW = DE-A-PC (AGT-LEVEL) * 1000
                      PERFORM 2530-CALC-DDF-FEES THRU 2530-EXIT

                      MOVE CNC-WK      TO FX-AMOUNT
                      PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                      MOVE '77'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
                      MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                      MOVE '97'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT

052814              WHEN (DE-AGT-TYPE (AGT-LEVEL) = 'S')
052814                AND (DE-A-PC (AGT-LEVEL) NOT = ZEROS)
052814                MOVE 'CANCEL CSO ADMIN FEE' TO FX-DESCRIPTION        
052814                MOVE '01'                  TO FX-SUB-TYPE
052814
052814                COMPUTE fx-amount =
052814                   DE-A-PC (AGT-LEVEL) * de-ah-rfnd
052814                PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
052814                MOVE '77'                  TO FX-TRAN-TYPE     
052814                PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
052814                MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
052814                MOVE '97'                  TO FX-TRAN-TYPE     
052814                PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT

010816              WHEN (DE-AGT-TYPE (AGT-LEVEL) = 'O')
010816                AND (DE-A-PC (AGT-LEVEL) NOT = +0)
010816                MOVE 'CANCEL GA FEE      '  TO FX-DESCRIPTION        
010816                MOVE '01'                  TO FX-SUB-TYPE
010816                COMPUTE FX-AMOUNT =
010816                   DE-AH-RFND * DE-A-PC (AGT-LEVEL)
010816                PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
010816                MOVE '76'                  TO FX-TRAN-TYPE     
010816                PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
010816                MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
010816                MOVE '96'                  TO FX-TRAN-TYPE     
010816                PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
                  END-EVALUATE
                WHEN '7'
                  EVALUATE TRUE
021009              WHEN DE-AGT-TYPE (AGT-LEVEL) = 'I' OR 'J'
                      MOVE 'CANCEL RC-L INCENTIVE'  
                                                 TO FX-DESCRIPTION        
                      MOVE '01'                  TO FX-SUB-TYPE
                      COMPUTE WS-CNC-FACT = DE-AH-RFND / DE-AH-PRM
                      COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                          * WS-CNC-FACT
                      PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                      MOVE '70'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
                      MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                      MOVE '90'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT

011410              WHEN DE-AGT-TYPE (AGT-LEVEL) = 'L' OR 'A'
                      MOVE 'CANCEL RC-L LMBA FEE' 
                                                 TO FX-DESCRIPTION        
                      MOVE '01'                  TO FX-SUB-TYPE
011410                IF DE-AH-PRM NOT = ZEROS
011410                   COMPUTE WS-CNC-FACT = DE-AH-RFND / DE-AH-PRM
011410                ELSE
011410                   MOVE ZEROS   TO WS-CNC-FACT
011410                END-IF
011410                IF DE-AGT-TYPE (AGT-LEVEL) = 'L'
011410                   COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000
011410                                    * WS-CNC-FACT
011410                ELSE
011410                   COMPUTE WS-WORK-AMT = (DE-A-PC (AGT-LEVEL) *
011410                      DE-REI-AHPRM)  * WS-CNC-FACT
011410                   IF DE-LF-PRM NOT = ZEROS
011410                      COMPUTE WS-LF-CNC-FACT = DE-LF-RFND /
011410                         DE-LF-PRM
011410                      COMPUTE FX-AMOUNT ROUNDED = WS-WORK-AMT +
011410                       ((DE-L-PC (AGT-LEVEL) * DE-REI-LFPRM)
011410                          * WS-LF-CNC-FACT)
011410                   END-IF
011410                END-IF
                      PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                      MOVE '75'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
                      MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                      MOVE '95'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT

                    WHEN (DE-AGT-TYPE (AGT-LEVEL) = 'B')
                      AND (DE-A-PC (AGT-LEVEL) NOT = +0)
                      MOVE 'CANCEL RC-L CONTRACT FEE' 
                                                 TO FX-DESCRIPTION        
                      MOVE '01'                  TO FX-SUB-TYPE
                      COMPUTE WS-CNC-FACT = DE-AH-RFND / DE-AH-PRM
                      COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                          * WS-CNC-FACT
                      PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
                      MOVE '72'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
                      MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
                      MOVE '92'                  TO FX-TRAN-TYPE     
                      PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT

010816              WHEN (DE-AGT-TYPE (AGT-LEVEL) = 'O')
010816                AND (DE-A-PC (AGT-LEVEL) NOT = +0)
010816                MOVE 'CANCEL RC GA FEE   '  TO FX-DESCRIPTION        
010816                MOVE '01'                  TO FX-SUB-TYPE
010816                COMPUTE FX-AMOUNT =
010816                   DE-AH-RFND * DE-A-PC (AGT-LEVEL)
010816                PERFORM 1400-OTHER-FX-DATA THRU 1400-EXIT
010816                MOVE '76'                  TO FX-TRAN-TYPE     
010816                PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
010816                MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
010816                MOVE '96'                  TO FX-TRAN-TYPE     
010816                PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
                  END-EVALUATE
               END-EVALUATE                                                 
TEST           MOVE FX-AMOUNT TO TEST-DISP-FX-AMT  
TEST  *        DISPLAY 'FX AMT ' TEST-DISP-FX-AMT
TEST  *        DISPLAY ' '
                                                                        
               ADD +1                      TO AGT-LEVEL
           END-PERFORM

           .
       1000-EXIT.                                                       
           EXIT.                                                        

       1010-WRITE-EXTRACT.

111616     IF DE-CARRIER = '3' or '4' or '5' OR '6' or '7'
              WRITE CCC-EXTRACT-RECORD FROM WS-EXTRACT-RECORD
           ELSE
              WRITE EXTRACT-RECORD     FROM WS-EXTRACT-RECORD
           END-IF

           .
       1010-EXIT.
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

           MOVE DE-COMPANY-CD          TO AM-COMPANY-CD
           MOVE DE-CNTRL1              TO AM-CNTRL-1        
           MOVE DE-EFF                 TO AM-EXPIRE-DT

           START ERACCT KEY >= AM-CONTROL-PRIMARY

           IF ERACCT-STATUS NOT = '00' 
               DISPLAY 'ERROR ON ERACCT START ' ERACCT-STATUS
               GO TO 1300-EXIT
           END-IF

           MOVE '0' TO END-OF-SEARCH-SW
           MOVE ' '                    TO WS-DCC-PRODUCT-CODE
                                                                        
           PERFORM UNTIL END-OF-SEARCH                                  
              READ ERACCT NEXT                            

              IF ERACCT-STATUS = '00'
                 IF (AM-COMPANY-CD = DE-COMPANY-CD)
                    AND (AM-CNTRL-1 = DE-CNTRL1)
051810              MOVE SPACES        TO FX-CITY                          
051810              STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
051810                DELIMITED BY '  ' INTO FX-CITY
051810              END-STRING
                    MOVE AM-ZIP        TO FX-ZIP-CODE
                    IF (DE-EFF >= AM-EFFECT-DT)
                       AND (DE-EFF < AM-EXPIRE-DT)
                       MOVE AM-DCC-PRODUCT-CODE
                                       TO WS-DCC-PRODUCT-CODE
                    END-IF
                 ELSE
                    SET END-OF-SEARCH  TO TRUE
                 END-IF
              ELSE                                                
                 DISPLAY 'ERACCT READ NEXT STATUS ' ERACCT-STATUS
                 SET END-OF-SEARCH     TO TRUE
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

       2520-REF-CSO-ADMIN-FEE.

           IF DE-CERT-NO = WS-FACT-CERT
              CONTINUE
           ELSE
              PERFORM 2550-GET-DDF-FACTORS THRU 2550-EXIT
           END-IF

           MOVE ZEROS                  TO CNC-WK

           EVALUATE TRUE
              WHEN (DE-DCC-DDF-REM-TRM3 = 0)
                 OR (DE-CANCEL-REASON = 'R')
                 DISPLAY ' REM TERM = 0 OR REPO'
                 MOVE 0                TO CNC-WK

082912        when de-ah-rfnd = de-ah-prm
082912           display ' full refund '
082912           move a-ow             to cnc-wk
              WHEN DE-DCC-DDF-REM-TRM3 = DE-AH-TERM
                 DISPLAY ' REM TERM  = ORIG TERM '
                 MOVE A-OW             TO CNC-WK

              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < 4
                 DISPLAY ' ORIG TERM - REM TRM < 4 '
                 COMPUTE W-FACTOR ROUNDED = A-OW -
                    (DD-1ST-YR-ALLOW (D1)
                    * WS-MONTH / 3) - (A-OW - DD-1ST-YR-ALLOW (D1))
                    * (1 - WS-HI-UEF) * WS-MONTH / 12
                 COMPUTE CNC-WK = W-FACTOR * 1

      **** (AdminFee-Yr1 AF)-((AdminFee- Yr1 AF)*(1 - UEF1)*mo/12)

              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < 13
                 DISPLAY ' ORIG TERM - REM TRM < 13 '
                 COMPUTE W-FACTOR ROUNDED = (a-ow -
                    DD-1ST-YR-ALLOW (D1))
                    - (a-ow - DD-1ST-YR-ALLOW (D1))
                    * (1 - WS-hi-UEF) * ws-MONTH / 12
                 COMPUTE CNC-WK = W-FACTOR * 1

      ****  (AdminFee-Yr1 AF)*{UEF1 - (UEF1 -UEF2)*mo/12}

              WHEN OTHER
                 DISPLAY ' OTHER '
                 COMPUTE W-FACTOR ROUNDED = (a-ow -
                    DD-1ST-YR-ALLOW (D1))
                    * (WS-HI-UEF - (WS-HI-UEF - WS-LO-UEF)
                    * ws-MONTH / 12)
                 COMPUTE CNC-WK = W-FACTOR * 1
           END-EVALUATE

           .
       2520-EXIT.
           EXIT.

       2530-CALC-DDF-FEES.

      *****   THIS PARA IS ONLY FOR COMM TYPES N AND I
      *****   DCC DDF REFUNDS ONLY

           IF DE-CERT-NO = WS-FACT-CERT
              CONTINUE
           ELSE
              PERFORM 2550-GET-DDF-FACTORS THRU 2550-EXIT
           END-IF

           COMPUTE WS-MONTH =
              FUNCTION REM(DE-AH-TERM - DE-DCC-DDF-REM-TRM3, 12)
           IF WS-MONTH = 0
              MOVE 12 TO WS-MONTH
           END-IF
           DISPLAY ' WS MONTH ' WS-MONTH

           EVALUATE TRUE
              WHEN DE-AGT-TYPE (AGT-LEVEL) = 'N'
                 PERFORM 2520-REF-CSO-ADMIN-FEE
                                       THRU 2520-EXIT
              WHEN (DE-AGT-TYPE (AGT-LEVEL) = 'I')
                 AND (DE-CANCEL-REASON = 'R')
                    MOVE A-OW TO CNC-WK
              WHEN (DE-AGT-TYPE (AGT-LEVEL) = 'I')
                 EVALUATE TRUE
                    WHEN ((DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < 4)
                       OR (NOT DD-IU-PRESENT)
                       COMPUTE WS-CNC-FACT = DE-REI-AHRFND /
                         DE-REI-AHPRM
                       COMPUTE CNC-WK ROUNDED = WS-CNC-FACT * A-OW
                     WHEN ((DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < 13)
                        AND (DD-IU-PRESENT)
                        COMPUTE WS-CLP-MO3 = DE-REI-AHPRM -
                           (DE-REI-AHPRM -
                           DE-IU-RATE-UP) * (WS-HI-UEF -
                           WS-LO-UEF) * 3 / 12
                        DISPLAY ' UECLPMO3 ' WS-CLP-MO3
                        COMPUTE WS-CNC-FACT = WS-CLP-MO3 / DE-REI-AHPRM
                        COMPUTE WS-COMM-MO3 ROUNDED = WS-CNC-FACT
                         * A-OW
                        DISPLAY ' UEMGTFO3 ' WS-COMM-MO3
                        COMPUTE CNC-WK = WS-COMM-MO3 - (WS-COMM-MO3 -
                          A-OW * WS-HI-UEF) * (WS-MONTH - 3) / 9
                    WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) > 12
                       COMPUTE CNC-WK = A-OW *
                          (WS-HI-UEF - (WS-HI-UEF - WS-LO-UEF) *
                          WS-MONTH / 12)
                 END-EVALUATE
                 DISPLAY ' CSO REF MGT FEE ' DE-CERT-NO ' ' CNC-WK
           END-EVALUATE

           .
       2530-EXIT.
           EXIT.

       2540-CALC-DDF-COMM.

           IF DE-CERT-NO = WS-FACT-CERT
              DISPLAY ' FACTORS ALREADY BUILT IN 2540 ' DE-CERT-NO
           ELSE
              PERFORM 2550-GET-DDF-FACTORS THRU 2550-EXIT
           END-IF

           COMPUTE WS-MONTH =
              FUNCTION REM(DE-AH-TERM - DE-DCC-DDF-REM-TRM3, 12)
           IF WS-MONTH = 0
              MOVE 12 TO WS-MONTH
           END-IF
           DISPLAY ' WS MONTH ' WS-MONTH

           EVALUATE TRUE
              WHEN DE-CANCEL-REASON = 'R'
                 MOVE A-OW             TO CNC-WK
              WHEN ((DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < 4)
                 OR (NOT DD-IU-PRESENT)
                    COMPUTE WS-CNC-FACT = DE-REI-AHRFND / DE-REI-AHPRM
                    COMPUTE CNC-WK = WS-CNC-FACT * A-OW
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < 13
                  COMPUTE WS-CLP-MO3 = DE-REI-AHPRM - (DE-REI-AHPRM -
                     DE-IU-RATE-UP) * (WS-HI-UEF -
                     WS-LO-UEF) * 3 / 12
                  DISPLAY ' UECLPMO3 ' WS-CLP-MO3
                  COMPUTE WS-CNC-FACT = WS-CLP-MO3 / DE-REI-AHPRM
                  COMPUTE WS-COMM-MO3 ROUNDED = WS-CNC-FACT * A-OW
                  DISPLAY ' UECOMMO3 ' WS-COMM-MO3
                  COMPUTE CNC-WK = WS-COMM-MO3 - (WS-COMM-MO3 -
                    A-OW * WS-HI-UEF) * (WS-MONTH - 3) / 9
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) > 12
                 COMPUTE CNC-WK = a-ow * (WS-HI-UEF - (WS-HI-UEF -
                    WS-LO-UEF) * ws-MONTH / 12)
           END-EVALUATE

           DISPLAY ' CSO REF comm    ' DE-CERT-NO ' ' CNC-WK

           .
       2540-EXIT.
           EXIT.

       2550-GET-DDF-FACTORS.

           MOVE ZEROS                  TO CNC-WK
                                          WS-LO-UEF WS-HI-UEF
           move ' '                    to dd-iu-sw
051414     if de-clp-state = spaces
051414        move de-state            to de-clp-state
           end-if

           PERFORM VARYING D1 FROM +1 BY +1 UNTIL
051414        ((DE-clp-STATE = DD-STATE (D1))
080212        and (WS-DCC-PRODUCT-CODE = dd-product-cd (d1))
080212*       AND (AM-DCC-PRODUCT-CODE = DD-PRODUCT-CD (D1))
              AND ('A'           = DD-BEN-TYPE   (D1))
              AND (DE-AH-TYPE    = DD-BEN-CODE   (D1))
              AND (DE-EFF        < DD-PROD-EXP-DT (D1)))
                              OR
100413        D1 > max-pdef
           END-PERFORM
100413     IF D1 > max-pdef
              DISPLAY ' COULD NOT FIND UEP FACTORS '
              GO TO 2550-EXIT
           END-IF

           PERFORM VARYING P1 FROM +1 BY +1 UNTIL
032922        (P1 > +11)
              OR (DD-PROD-CODE (D1 P1) = 'I')
           END-PERFORM
032922     IF P1 < +12
              display ' setting iu present to true '
              SET DD-IU-PRESENT        TO TRUE
           END-IF

           MOVE DE-AH-TERM             TO WS-DDF-TERM
           
           IF (DE-LOAN-TERM > WS-DDF-TERM)
              AND (DD-TRUNCATED (D1) = 'Y')
              MOVE DE-LOAN-TERM        TO WS-DDF-TERM
              DISPLAY ' FOUND TRUNCATED ' DE-CERT-NO
           END-IF

           EVALUATE TRUE
              WHEN WS-DDF-TERM > +168
                 MOVE 15               TO P1
              WHEN WS-DDF-TERM > +156
                 MOVE 14               TO P1
              WHEN WS-DDF-TERM > +144
                 MOVE 13               TO P1
              WHEN WS-DDF-TERM > +132
                 MOVE 12               TO P1
              WHEN WS-DDF-TERM > +120
                 MOVE 11               TO P1
              WHEN WS-DDF-TERM > +108
                 MOVE 10               TO P1
              WHEN WS-DDF-TERM > +96
                 MOVE 9                TO P1
              WHEN WS-DDF-TERM > +84
                 MOVE 8                TO P1
              WHEN WS-DDF-TERM > +72
                 MOVE 7                TO P1
              WHEN WS-DDF-TERM > +60
                 MOVE 6                TO P1
              WHEN WS-DDF-TERM > +48
                 MOVE 5                TO P1
              WHEN WS-DDF-TERM > +36
                 MOVE 4                TO P1
              WHEN WS-DDF-TERM > +24
                 MOVE 3                TO P1
              WHEN WS-DDF-TERM > +12
                 MOVE 2                TO P1
              WHEN OTHER
                 MOVE 1                TO P1
           END-EVALUATE

           EVALUATE TRUE
      *       WHEN ((DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +13)
      *          and (dd-iu-present)
      *          MOVE 2                TO P2
      *       WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +13
      *          MOVE 1                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +25
                 MOVE 2                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +37
                 MOVE 3                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +49
                 MOVE 4                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +61
                 MOVE 5                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +73
                 MOVE 6                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +85
                 MOVE 7                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +97
                 MOVE 8                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +109
                 MOVE 9                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +121
                 MOVE 10               TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +133
                 MOVE 11               TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +145
                 MOVE 12               TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +157
                 MOVE 13               TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +169
                 MOVE 14               TO P2
              WHEN OTHER
                 MOVE 15               TO P2
           END-EVALUATE

           MOVE DE-CERT-NO             TO WS-FACT-CERT
           MOVE DD-UEP-FACTOR (D1 P1 P2 + 1)
                                       TO WS-LO-UEF
           MOVE DD-UEP-FACTOR (D1 P1 P2)
                                       TO WS-HI-UEF

           .
       2550-EXIT.
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

           OPEN INPUT  ERACCT ERPDEF
                       DETEXTR                             
                OUTPUT FREEDOM-EXTRACT CCC-FREEDOM-EXTRACT

           IF ERACCT-STATUS = '00' OR '97'                              
               CONTINUE                                                 
           ELSE                                                         
               DISPLAY 'OPEN ERROR ' ERACCT-STATUS ' ON ERACCT'         
           END-IF

           IF ERPDEF-FILE-STATUS = '00' OR '97'                              
               CONTINUE                                                 
           ELSE                                                         
               DISPLAY 'OPEN ERROR ' ERPDEF-FILE-STATUS ' ON ERPDEF'
           END-IF

010816     IF DTE-CLIENT = 'DCC' or 'CAP'
              MOVE 0 TO B-SUB
              MOVE DTE-CLASIC-COMPANY-CD
                                       TO PD-CONTROL-PRIMARY
              START ERPDEF KEY >= PD-CONTROL-PRIMARY
              IF ERPDEF-FILE-STATUS = '00'
                 PERFORM UNTIL ERPDEF-FILE-STATUS NOT = '00'
                    READ ERPDEF NEXT RECORD
                    IF ERPDEF-FILE-STATUS = '00'
                       ADD 1 TO B-SUB
100413                 IF B-SUB > max-pdef
                          DISPLAY ' DDF UEP TABLE BLEW '
                          GO TO ABEND-PGM
                       END-IF
                       MOVE PD-STATE   TO DD-STATE (B-SUB)
                       MOVE PD-PRODUCT-CD
                                       TO DD-PRODUCT-CD (B-SUB)
                       MOVE PD-BEN-TYPE
                                       TO DD-BEN-TYPE (B-SUB)
                       MOVE PD-BEN-CODE
                                       TO DD-BEN-CODE (B-SUB)
                       MOVE PD-PROD-EXP-DT
                                       TO DD-PROD-EXP-DT (B-SUB)
                       MOVE PD-1ST-YR-ADMIN-ALLOW
                                       TO DD-1ST-YR-ALLOW (B-SUB)
                       PERFORM VARYING P2 FROM +1 BY +1 UNTIL
032922                    P2 > +11
                          MOVE PD-PROD-CODE (P2)
                                       TO DD-PROD-CODE (B-SUB P2)
                       END-PERFORM
                       MOVE PD-EARN-FACTORS
                                       TO DD-EARN-FACTORS (B-SUB)
080613                 move pd-truncated to dd-truncated (b-sub)
                    END-IF
                 END-PERFORM
              DISPLAY ' DCC PRODUCT DEF TABLE LOADED SUCCESSFULLY '
              END-IF
              CLOSE ERPDEF
           END-IF

           .
       0000-EXIT.                                                       
           EXIT.                                                        
                                                                        

       ABEND-PGM SECTION.                                   
           DIVIDE WS-ZERO BY WS-ZERO       GIVING WS-ZERO.   
       ABEND-EXIT.                                         
                                                                        
