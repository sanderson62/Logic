       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.    VPP169.                                           
                                                                        
      ****************************************************************** 
      *                                                                * 
      *            FREEDOM INTERFACE FOR VPP CONTRACT FEES             *
      *                                                                * 
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 080416                   PEMA  NEW PROGRAM FOR VPP G/L INTERFACE
040119* 040119  IR2019040100004  PEMA  Acct read beyond last date range      
      ******************************************************************
                                                                        
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT DETEXTR              ASSIGN TO SYS010 
                                       FILE STATUS IS DETEXT-STATUS.                            

           SELECT CERT-IN              ASSIGN TO SYS012.

           SELECT FREEDOM-EXTRACT      ASSIGN TO SYS011
                                       ORGANIZATION IS LINE SEQUENTIAL.

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
                                                                        
       FD  CERT-IN.                                                      
                                   COPY ECSCRT01.

       FD  FREEDOM-EXTRACT                                              
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  EXTRACT-RECORD      PIC X(250).                              

       FD  ERACCT.                                        
           COPY ERCACCT.                                                

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
       77  ws-bin-eff-date             pic xx value low-values.
       77  ws-bin-cancel-date          pic xx value low-values.
       77  ws-elapsed-months           pic s999 comp-3 value +0.
       77  a1                          pic s999 comp-3 value +0.
       77  ws-cancel-fee               pic s9(5)v99 comp-3 value +0.
       77  ws-customer-refund          pic s9(5)v99 comp-3 value +0.
       77  ws-ah-cnc-fact              pic s999v9(7) comp-3 value +0.
040119 77  ws-refund-short-due-ncb     pic s9(5)v9(5) comp-3 value +0.
       77  cert-in-recs                pic 9(7) value zeros.
       77  ws-cert-sw                  pic x value ' '.
           88  end-of-cert                value 'Y'.

040119 01  ws-save-account             pic x(4000) value spaces.
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

                                       copy ELCDATE.
                                       COPY ELCDTECX.                                               
                                       COPY ELCDTEVR.                                               

       LINKAGE SECTION.                                                 

       01  PARM.                                                        
           05  PARM-LENGTH                  PIC S9(04) COMP. 
           05  CYCLE-DATE                   PIC X(08). 
                                                                        
       PROCEDURE DIVISION USING PARM.                                   
                                                                        
      *************************************************************     
       COPY ELCDTERX.                   
      *************************************************************     
           PERFORM 0000-HOUSEKEEPING       THRU 0000-EXIT                     
                                                                        
           PERFORM 1000-PROCESS            THRU 1000-EXIT 
               UNTIL EOF-DETEXT

           CLOSE DETEXTR
                 FREEDOM-EXTRACT
                 ERACCT
                 CERT-IN
           GOBACK.

       1000-PROCESS.                                                    

           PERFORM 1100-GET-LOGIC-RECORD   THRU 1100-EXIT                 
                WITH TEST AFTER
                    UNTIL VALID-RECORD OR EOF-DETEXT
                                                                        
           IF EOF-DETEXT
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

           move 'VPP FEES'             to fx-system
           move '5V'                   to fx-division

           MOVE DE-ACCT-PRIME              TO FX-AGENT-01 
           MOVE '6211'                     TO FX-COST-CENTER
           MOVE ' '                        TO FX-DISTR  
           MOVE ' '                        TO FX-SOURCE-ACCT
           MOVE ' '                        TO FX-REFERENCE 
                                                                        
           MOVE +1                         TO AGT-LEVEL

           if (dte-client = 'VPP')
              if de-trans = 'C'
                 perform 2000-get-cancel-fees
                                       thru 2000-exit
              end-if
           end-if

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
                    WHEN (DE-AGT-TYPE (AGT-LEVEL) = 'I')
                       and (de-a-pc (agt-level) not = zero)
                      MOVE 'CANCEL MGT FEE  '    TO FX-DESCRIPTION        
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
021009              WHEN (DE-AGT-TYPE (AGT-LEVEL) = 'J')
                       and (de-a-pc (agt-level) not = zero)
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


                    WHEN (DE-AGT-TYPE (AGT-LEVEL) = 'N')
                      AND (DE-A-PC (AGT-LEVEL) NOT = ZEROS)
                      MOVE 'CANCEL CSO ADMIN FEE' TO FX-DESCRIPTION        
                      MOVE '01'                  TO FX-SUB-TYPE
                      COMPUTE WS-CNC-FACT = DE-AH-RFND / DE-AH-PRM
                      IF DE-CANCEL-REASON = 'R'
                         MOVE +1      TO WS-CNC-FACT
                      END-IF
                      COMPUTE FX-AMOUNT = DE-A-PC (AGT-LEVEL) * 1000 
                                          * WS-CNC-FACT
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

           WRITE EXTRACT-RECORD FROM WS-EXTRACT-RECORD

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
040119                 move account-master
040119                                 to ws-save-account
                    END-IF
                 ELSE
                    SET END-OF-SEARCH  TO TRUE
                 END-IF
              ELSE
                 DISPLAY 'ERACCT READ NEXT STATUS ' ERACCT-STATUS
                 display 'de key ' de-cntrl1 ' ' de-eff ' ' de-cert-no
                 SET END-OF-SEARCH     TO TRUE
              END-IF
040119        move ws-save-account     to account-master
           END-PERFORM

           MOVE FX-CITY                TO FNX001-DATA
           CALL 'FNX001' USING FNX001-PARMS                             
           MOVE FNX001-CITY            TO FX-CITY

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

       2000-get-cancel-fees.

           if (de-cancel-fee numeric)
              and (de-cancel-fee > zero)
              move 'VPA CANCEL FEE INCOME'
                                       to fx-description
              move '01'                to fx-sub-type
              compute fx-amount =
                 de-cancel-fee * -1
              PERFORM 1400-OTHER-FX-DATA
                                       THRU 1400-EXIT
              MOVE '73'                TO FX-TRAN-TYPE
              PERFORM 1010-WRITE-EXTRACT
                                       THRU 1010-EXIT
              MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
              MOVE '93'                  TO FX-TRAN-TYPE     
              PERFORM 1010-WRITE-EXTRACT THRU 1010-EXIT
           end-if

           perform 2010-match-to-cert  thru 2010-exit

           display ' made 2000 ' de-rein ' ' de-carrier ' ' de-state
              ' ' de-account ' ' de-eff ' ' de-cert-no ' '
              cr-lcom-ah (2) ' ' cr-lcom-ah (3) ' ' cr-lcom-ah (4)

           move +0                     to ws-refund-short-due-ncb
           move de-eff                 to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-eff-date
           else
              display ' invalid effective date - aborting '
                 de-state ' ' de-account ' ' de-cert-no ' '
                 de-eff ' ' dc-error-code
              perform abend-pgm
           end-if

           move de-lf-canc-dte         to dc-greg-date-cymd
           display ' canc dates ' de-lf-canc-dte ' ' de-ah-canc-dte
           if (de-ah-canc-dte not = zeros)
              and (de-ah-canc-dte > dc-greg-date-cymd)
              move de-ah-canc-dte      to dc-greg-date-cymd
           end-if
           move 'L'                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           if no-conversion-error
              move dc-bin-date-1 to ws-bin-cancel-date
           else
              display ' invalid cancel date - aborting '
                 de-state ' ' de-account ' ' de-cert-no ' '
                 dc-greg-date-cymd ' ' dc-error-code
              perform abend-pgm
           end-if

           move ws-bin-eff-date        to dc-bin-date-1
           move ws-bin-cancel-date     to dc-bin-date-2
           move '1'                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           if no-conversion-error
              display ' elapsed months/days b4 '
                 dc-elapsed-months ' ' dc-odd-days-over
              move dc-elapsed-months   to ws-elapsed-months
              if (dc-odd-days-over > +1)
                 and (dc-elapsed-months > +0)
                 add +1 to ws-elapsed-months
              end-if
              display ' elapsed mos ' de-cert-no ' '
                 ws-elapsed-months
           else
              display ' invalid dates - aborting ' de-state ' '
                 de-account ' ' de-cert-no ' ' de-eff ' '
                    de-lf-canc-dte ' ' de-ah-canc-dte ' '
                    dc-greg-date-cymd ' ' dc-error-code
              perform abend-pgm
           end-if

           display ' cancel fee ' de-cancel-fee ' ' de-ah-rfnd
           move de-cancel-fee          to ws-cancel-fee
           if de-ah-rfnd < ws-cancel-fee
              move de-ah-rfnd          to ws-cancel-fee
           end-if
           compute ws-customer-refund = de-ah-rfnd - ws-cancel-fee
           display ' cust ref ' ws-customer-refund
           if ws-customer-refund > 0
              compute ws-ah-cnc-fact rounded = de-ah-rfnd / de-ah-prm
              display ' can fact ' ws-ah-cnc-fact
              perform varying a1 from +1 by +1 until a1 > +10
                 if am-comm-chargeback (a1) not numeric
                    move zeros   to am-comm-chargeback (a1)
                 end-if
                 if (de-agt-type (a1) = 'J' or 'I')
                    and (am-comm-chargeback(a1) <> zeros)
                    and (ws-elapsed-months > am-comm-chargeback (a1))
                    display ' a1 comm ' a1 ' ' cr-lcom-ah (a1)
                    compute ws-refund-short-due-ncb rounded =
                       ws-refund-short-due-ncb +
                       ((cr-lcom-ah (a1) * +1000) * ws-ah-cnc-fact)
                    display ' ref short ' ws-refund-short-due-ncb
                 end-if
              end-perform
           end-if

           if ws-refund-short-due-ncb > zeros
              compute fx-amount =  ws-refund-short-due-ncb
              move 'VPA CANCEL FEE EXPENSE'
                                       to fx-description
              move '01'                to fx-sub-type
              PERFORM 1400-OTHER-FX-DATA
                                       THRU 1400-EXIT
              MOVE '74'                TO FX-TRAN-TYPE
              PERFORM 1010-WRITE-EXTRACT
                                       THRU 1010-EXIT
              MULTIPLY FX-AMOUNT BY -1   GIVING FX-AMOUNT
              MOVE '94'                TO FX-TRAN-TYPE     
              PERFORM 1010-WRITE-EXTRACT
                                       THRU 1010-EXIT
           end-if

           .
       2000-exit.
           exit.


       2010-match-to-cert.

           if de-control > cr-full-control
              perform 2020-read-cert   thru 2020-exit
              go to 2010-match-to-cert
           else
              if de-control < cr-full-control
                 display ' no cert for extract ' 
                 display '      extr = ' de-cntrl1 ' ' de-eff ' '
                    de-cert
                 display '      cert = ' cr-acct-control ' ' cr-dt ' '
                    cr-cert-no
                 perform abend-pgm
              end-if
           end-if

           .
       2010-exit.
           exit.

       2020-read-cert.

           read cert-in at end
              set end-of-cert to true
           end-read

           if not end-of-cert
              add 1 to cert-in-recs
           end-if

           .
       2020-exit.
           exit.

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

           OPEN INPUT
                   ERACCT
                   DETEXTR
                   CERT-IN
                OUTPUT
                   FREEDOM-EXTRACT

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

           perform 2020-read-cert      thru 2020-exit

           .
       0000-EXIT.                                                       
           EXIT.                                                        

       8500-DATE-CONVERT.              
                                       COPY ELCDCS.

       ABEND-PGM SECTION.                                   
           DIVIDE WS-ZERO BY WS-ZERO       GIVING WS-ZERO.   
       ABEND-EXIT.                                         
