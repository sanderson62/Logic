       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.  EL1283.                                                     
       AUTHOR.      CENTRAL STATES HEALTH AND LIFE                              
                    OMAHA, NEBRASKA.                                            
       DATE-COMPILED.                                                           
      *SECURITY.   *****************************************************        
      *            *                                                   *        
      *            *   THIS PROGRAM IS THE PROPERTY OF CENTRAL STATES  *        
      *            *   HEALTH AND LIFE                                 *        
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *        
      *            *   OF CSO       . IS EXPRESSLY PROHIBITED WITHOUT  *        
      *            *   THE PRIOR WRITTEN PERMISSION OF CENTRAL STATES. *        
      *            *                                                   *        
      *            *****************************************************        
      *REMARKS.                                                                 
      *          TRANSACTION - EXXE                                             
                                                                                
      *        PURGED CERT DISPLAY PROGRAM.                                     
                                                                                
      *    SCREENS     - EL128C - PURGED CERT DISPLAY                           
                                                                                
      *    ENTERED BY  - EL1282 - PURGED CERT LOOKUP                            
                                                                                
      *    EXIT TO     - CALLING PROGRAM                                        
                                                                                
      *    INPUT FILE  - ELPURG - PURGED CERT FILE                              
      *                  ELCNTL - CONTROL FILE                                  
                                                                                
      *    OUTPUT FILE - ELPURG - PURGED CERT FILE                              
                                                                                
      *    COMMAREA    - PASSED.  IF A CERTIFICATE IS SELECTED, THE             
      *                  CONTROL OF THAT CERTIFICATE IS PLACED IN THE           
      *                  APPROPRIATE FIELDS OF THE COMMAREA FOR                 
      *                  REFERENCE BY SUCCESSIVE PROGRAMS.  THE PROGRAM         
      *                  WORK AREA OF THE COMMAREA IS USED TO PASS THE          
      *                  RECORD KEY INFORMATION NEEDED BY EL1282 TO             
      *                  LOCATE THE CERTIFICATE.                                
                                                                                
                                                                                
      *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL1282.                
      *                  USE THE KEY TO THE CERTIFICATE MASTER PASSED           
      *                  IN THE COMMAREA TO DISPLAY THE CERTIFICATE AND         
      *                  RETURN WITH THE TRANSACTION OF THE CALLING             
      *                  PROGRAM.                                               
                                                                                
           EJECT                                                                
       ENVIRONMENT DIVISION.                                                    
                                                                                
       DATA DIVISION.                                                           
                                                                                
       WORKING-STORAGE SECTION.                                                 
       77  FILLER  PIC X(32)  VALUE '********************************'.         
       77  FILLER  PIC X(32)  VALUE '*   EL1283 WORKING STORAGE     *'.         
       77  FILLER  PIC X(32)  VALUE '************VMOD=2.001 ********'.          
                                                                                
                                           COPY ELCSCTM.                        
                                                                                
                                           COPY ELCSCRTY.                       
                                                                                
       01  WS-DATE-AREA.                                                        
           05  SAVE-DATE                   PIC X(8)     VALUE SPACES.           
           05  SAVE-BIN-DATE               PIC X(2)     VALUE SPACES.           
                                                                                
       01  FILLER                          COMP-3.                              
           05  WS-LF-ALT                   PIC S9(9)V99.                        
           05  WS-ERROR-COUNT              PIC S9(3)    VALUE ZERO.             
           05  TIME-IN                     PIC S9(7)    VALUE ZERO.             
           05  TIME-OUT                    REDEFINES                            
               TIME-IN                     PIC S9(3)V9(4).                      
                                                                                
           05  WS-ELAPSED-MONTHS           PIC S9(3)    VALUE ZERO.             
                                                                                
           05  WS-UPDATE-SW                PIC S9       VALUE ZERO.             
               88  NO-UPDATES-MADE                      VALUE ZERO.             
           05  WS-ST-REC-NOT-FOUND         PIC S9       VALUE ZERO.             
           05  WS-NOT-FOUND                PIC S9       VALUE ZERO.             
               88  BENEFIT-FOUND                        VALUE +1.               
           05  WS-READNEXT-SW              PIC S9       VALUE ZERO.             
                                                                                
           05  WS-COMPLETED-SUCCESSFUL     PIC S9       VALUE ZERO.             
             88  TRANSACTION-SUCCESSFUL                 VALUE +1.               
                                                                                
       01  FILLER   COMP SYNC.                                                  
           05  W-ONE                       PIC S9(4)    VALUE +1.               
           05  SC-ITEM                     PIC S9(4)    VALUE +0001.            
       01  WS-SAVE-CERT-CHANGE-REC         PIC X(300).                          
                                                                                
       01  FILLER.                                                              
           05  W-QID.                                                           
               10  W-QID-TERM              PIC X(04) VALUE SPACES.              
               10  FILLER                  PIC X(04) VALUE '128C'.              
           05  WS-CLBEN.                                                        
               10  WS-CLBEN2                       PIC ZZZ,ZZZ,ZZ9.99.          
               10  WS-CLBEN0 REDEFINES WS-CLBEN2   PIC ZZ,ZZZ,ZZZ,ZZ9.          
                                                                                
           05  WS-CALC-CD                  PIC X.                               
                                                                                
           05  WS-JOINT-INDICATOR          PIC X.                               
               88  WS-JOINT-COVERAGE       VALUE 'J'.                           
                                                                                
           05  WS-SAVE-CC-KEY              PIC X(37)    VALUE SPACES.           
                                                                                
           05  WS-BENEFIT-NO               PIC XX       VALUE ZERO.             
           05  WS-BENEFIT-DESCRIP          PIC X(10)    VALUE SPACES.           
           05  WS-KIND                     PIC X(3)     VALUE SPACES.           
           05  WS-SPACES                   PIC X        VALUE SPACES.           
                                                                                
           05  DEEDIT-FIELD                PIC X(15).                           
           05  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD  PIC S9(15).              
                                                                                
           05  WS-VAL-TABLE.                                                    
               10  WS-VAL-CD               PIC X.                               
               10  WS-VAL-TERM             PIC X.                               
               10  WS-VAL-BENE             PIC X.                               
                                                                                
           05  WS-MAPSET-NAME              PIC X(8)     VALUE 'EL1283S'.        
           05  WS-MAP-NAME                 PIC X(8)     VALUE 'EL128C'.         
           05  WS-MAP-NUMBER               PIC X(4)     VALUE '128C'.           
                                                                                
           05  THIS-PGM                    PIC X(8)     VALUE 'EL1283'.         
                                                                                
           05  WS-TRANS-ID                 PIC X(4)     VALUE 'EXXE'.           
           05  EL001                       PIC X(8)     VALUE 'EL001'.          
           05  EL004                       PIC X(8)     VALUE 'EL004'.          
           05  EL005                       PIC X(8)     VALUE 'EL005'.          
           05  EL010                       PIC X(8)     VALUE 'EL010'.          
           05  ELDATCV                     PIC X(8)     VALUE 'ELDATCV'.        
           05  ELRTRM                      PIC X(8)     VALUE 'ELRTRM'.         
           05  ELRAMT                      PIC X(8)     VALUE 'ELRAMT'.         
                                                                                
           05  WS-CONTROL-FILE-DSID        PIC X(8)     VALUE 'ELCNTL'.         
           05  WS-CERTIFICATE-MASTER-DSID  PIC X(8)     VALUE 'ELPURG'.         
           05  WS-CERT-MAINT-FILE-DSID     PIC X(8)     VALUE 'ERCRTC'.         
                                                                                
           05  WS-INDEX                    PIC S9(4)    VALUE ZERO              
                                           COMP                                 
                                           SYNC.                                
                                                                                
           05  WS-CURRENT-DATE             PIC XX      VALUE LOW-VALUES.        
           05  WS-JOURNAL-TYPE-ID          PIC XX      VALUE 'EL'.              
                                                                                
           05  WS-CAGEI                    PIC S99        VALUE ZERO.           
           05  WS-CJAGEI                   PIC S99        VALUE ZERO.           
           05  WS-APR                      PIC S999V9(4)  VALUE +0.             
           05  WS-CLM-DEDUCT               PIC S9(5)V99   VALUE +0.             
           05  WS-CAN-DEDUCT               PIC S9(5)V99   VALUE +0.             
           05  WS-LOAN-BAL                 PIC S9(7)V99   VALUE +0.             
           05  WS-LF-ORIG-TERM             PIC S9(3)      VALUE +0.             
           05  WS-AH-ORIG-TERM             PIC S9(3)      VALUE +0.             
           05  WS-LIVES                    PIC S9(7)      VALUE +0.             
           05  WS-BILLED                   PIC S9(7)      VALUE +0.             
           05  WS-LF-PREM                  PIC S9(7)V99   VALUE +0.             
           05  WS-LF-BENE                  PIC S9(9)V99   VALUE +0.             
           05  WS-AH-PREM                  PIC S9(7)V99   VALUE +0.             
           05  WS-AH-BENE                  PIC S9(7)V99   VALUE +0.             
           05  WS-SSNO                     PIC X(11).                           
           05  WS-SS-NO REDEFINES WS-SSNO  PIC 999B99B9999.                     
           05  WS-ACCOUNT.                                                      
               10  FILLER                  PIC X(4).                            
               10  WS-ACCT                 PIC X(6).                            
                                                                                
           05  WS-MEMBER-NO            PIC X(12).                               
           05  FILLER  REDEFINES  WS-MEMBER-NO.                                 
               10  WS-MEMBER-NO-1-8    PIC 9(8).                                
               10  FILLER              PIC X(4).                                
                                                                                
           05  WS-I-MICRO-NO           PIC S9(9)        COMP-3.                 
                                                                                
           05  WS-STATUS-DESC.                                                  
               10  FILLER              PIC X(7).                                
               10  WS-UW-STATUS        PIC X(5).                                
                                                                                
           EJECT                                                                
           05  ERROR-MESSAGES.                                                  
               10  ER-0008                 PIC X(4)    VALUE '0008'.            
               10  ER-0029                 PIC X(4)    VALUE '0029'.            
               10  ER-0033                 PIC X(4)    VALUE '0033'.            
               10  ER-0070                 PIC X(4)    VALUE '0070'.            
               10  ER-0142                 PIC X(4)    VALUE '0142'.            
               10  ER-0151                 PIC X(4)    VALUE '0151'.            
               10  ER-0588                 PIC X(4)    VALUE '0588'.            
               10  ER-0692                 PIC X(4)    VALUE '0692'.            
               10  ER-2152                 PIC X(4)    VALUE '2152'.            
               10  ER-2223                 PIC X(4)    VALUE '2223'.            
               10  ER-2351                 PIC X(4)    VALUE '2351'.            
               10  ER-2352                 PIC X(4)    VALUE '2352'.            
               10  ER-2354                 PIC X(4)    VALUE '2354'.            
               10  ER-2547                 PIC X(4)    VALUE '2547'.            
               10  ER-2848                 PIC X(4)    VALUE '2848'.            
               10  ER-7233                 PIC X(4)    VALUE '7233'.            
               10  ER-7234                 PIC X(4)    VALUE '7234'.            
               10  ER-7242                 PIC X(4)    VALUE '7242'.            
               10  ER-7248                 PIC X(4)    VALUE '7248'.            
                                                                                
           05  WS-CONTROL-FILE-KEY.                                             
               10  WS-CFK-COMPANY-ID       PIC X(3)     VALUE SPACES.           
               10  WS-CFK-RECORD-TYPE      PIC X        VALUE ZERO.             
      *          88  STATE-MASTER                       VALUE '3'.              
      *          88  LF-BENEFIT-MASTER                  VALUE '4'.              
      *          88  AH-BENEFIT-MASTER                  VALUE '5'.              
      *          88  CARRIER-MASTER                     VALUE '6'.              
               10  WS-CFK-ACCESS.                                               
                   15  WS-CFK-STATE        PIC XX       VALUE SPACES.           
                   15  WS-CFK-BENEFIT-NO                VALUE SPACES.           
                       20  FILLER          PIC X.                               
                       20  WS-CFK-CARRIER  PIC X.                               
               10  WS-CFK-SEQUENCE-NO      PIC S9(4)    VALUE ZERO COMP.        
                                                                                
           05  WS-CERTIFICATE-KEY.                                              
               10  WS-CK-COMPANY-CD        PIC X.                               
               10  WS-CK-CARRIER           PIC X.                               
               10  WS-CK-GROUPING          PIC X(6).                            
               10  WS-CK-STATE             PIC XX.                              
               10  WS-CK-ACCOUNT           PIC X(10).                           
               10  WS-CK-CERT-EFF-DT       PIC XX.                              
               10  WS-CK-CERT-NO.                                               
                   15  WS-CK-CERT-PRIME    PIC X(10).                           
                   15  WS-CK-CERT-SFX      PIC X.                               
                                                                                
           EJECT                                                                
                                           COPY ELCINTF.                        
           12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                           
               16  FILLER                  PIC X(7).                            
               16  PI-TO-EL677-KEY.                                             
                   20  PI-CHEK-COMP-CD     PIC X.                               
                   20  PI-CHEK-CARRIER     PIC X.                               
                   20  PI-CHEK-GROUPING    PIC X(6).                            
                   20  PI-CHEK-STATE       PIC XX.                              
                   20  PI-CHEK-ACCOUNT     PIC X(10).                           
                   20  PI-CHEK-EFF-DT      PIC XX.                              
                   20  PI-CHEK-CERT-NO     PIC X(10).                           
                   20  PI-CHEK-SFX         PIC X.                               
                   20  PI-CHEK-SEQUENCE    PIC S9(4)    COMP.                   
               16  FILLER                  PIC X(183).                          
               16  PI-PFKEY                PIC XXX.                             
                   88  PI-TO-EL1283-FROM-EL677        VALUE 'PF3'.              
                   88  PI-TO-EL677-FROM-EL1283        VALUE 'PF8'.              
               16  FILLER                  PIC X(86).                           
               16  PI-1ST-TIME-SW          PIC X.                               
               16  FILLER                  PIC XX.                              
               16  PI-PEND-SW              PIC X.                               
               16  FILLER                  PIC X(322).                          
                                                                                
           EJECT                                                                
                                           COPY ELCDATE.                        
           EJECT                                                                
                                           COPY EL1283S.                        
           EJECT                                                                
                                           COPY ELCJPFX.                        
                                           PIC X(450).                          
                                                                                
           EJECT                                                                
                                           COPY ELCCALC.                        
                                                                                
           EJECT                                                                
                                           COPY ELCEMIB.                        
                                                                                
           EJECT                                                                
                                           COPY ELCLOGOF.                       
                                                                                
           EJECT                                                                
                                           COPY ELCATTR.                        
                                                                                
           EJECT                                                                
                                           COPY ELCAID.                         
                                                                                
       01  FILLER REDEFINES DFHAID.                                             
           05  FILLER                      PIC X(8).                            
           05  PF-VALUES                   PIC X                                
               OCCURS 24 TIMES.                                                 
                                                                                
       LINKAGE SECTION.                                                         
                                                                                
       01  DFHCOMMAREA                     PIC X(1024).                         
                                                                                
           EJECT                                                                
                                           COPY ELCPURG.                        
                                                                                
           EJECT                                                                
                                           COPY ELCCNTL.                        
                                                                                
           EJECT                                                                
       PROCEDURE DIVISION.                                                      
                                                                                
           MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.             
           MOVE EIBDATE                TO DC-JULIAN-YYDDD.                      
           MOVE '5'                    TO DC-OPTION-CODE.                       
           PERFORM 8500-DATE-CONVERSION.                                        
           MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                           
           MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.                       
                                                                                
                                                                                
      *    NOTE *******************************************************         
      *         *                                                     *         
      *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *         
      *         *  FROM ANOTHER MODULE.                               *         
      *         *                                                     *         
      *         *******************************************************.        
                                                                                
           IF EIBCALEN NOT > ZERO                                               
               MOVE UNACCESS-MSG       TO  LOGOFF-MSG                           
               GO TO 8300-SEND-TEXT.                                            
                                                                                
           EXEC CICS HANDLE CONDITION                                           
               ERROR  (9990-ERROR)                                              
               QIDERR (0100-DISPLAY-CERTIFICATE)                                
           END-EXEC.                                                            
                                                                                
           MOVE EIBTRMID               TO  W-QID-TERM.                          
                                                                                
           IF PI-PROCESSOR-ID = 'LGXX'                                          
               NEXT SENTENCE                                                    
           ELSE                                                                 
               EXEC CICS READQ TS                                               
                   QUEUE   (PI-SECURITY-TEMP-STORE-ID)                          
                   INTO    (SECURITY-CONTROL)                                   
                   LENGTH  (SC-COMM-LENGTH)                                     
                   ITEM    (SC-ITEM)                                            
               END-EXEC                                                         
               MOVE SC-CREDIT-DISPLAY (33) TO  PI-DISPLAY-CAP                   
               MOVE SC-CREDIT-UPDATE  (33) TO  PI-MODIFY-CAP.                   
                                                                                
           IF PI-CALLING-PROGRAM = THIS-PGM                                     
               IF PI-1ST-TIME-SW = '1'                                          
                   MOVE ' '            TO  PI-1ST-TIME-SW                       
                   GO TO 0100-DISPLAY-CERTIFICATE.                              
                                                                                
           IF PI-CALLING-PROGRAM NOT = THIS-PGM                                 
               IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                           
                   MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6             
                   MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5             
                   MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4             
                   MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3             
                   MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2             
                   MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1             
                   MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM           
                   MOVE THIS-PGM             TO  PI-CALLING-PROGRAM             
                   PERFORM 3100-DELETE-PI-TS THRU 3100-EXIT                     
                 ELSE                                                           
                   MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM             
                   MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM           
                   MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1             
                   MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2             
                   MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3             
                   MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4             
                   MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5             
                   MOVE SPACES               TO  PI-SAVED-PROGRAM-6             
             ELSE                                                               
               GO TO 1000-EDIT-MAP.                                             
                                                                                
           EJECT                                                                
       0100-DISPLAY-CERTIFICATE.                                                
           IF NOT DISPLAY-CAP                                                   
               MOVE 'READ'             TO  SM-READ                              
               PERFORM 9995-SECURITY-VIOLATION                                  
               MOVE ER-0070            TO  EMI-ERROR                            
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                         
               GO TO 8100-SEND-INITIAL-MAP.                                     
                                                                                
           MOVE LOW-VALUES             TO  EL128CO.                             
                                                                                
           MOVE PI-COMPANY-CD          TO  WS-CK-COMPANY-CD.                    
           MOVE PI-CARRIER             TO  WS-CK-CARRIER.                       
           MOVE PI-GROUPING            TO  WS-CK-GROUPING.                      
           MOVE PI-STATE               TO  WS-CK-STATE.                         
           MOVE PI-ACCOUNT             TO  WS-CK-ACCOUNT.                       
           MOVE PI-CERT-NO             TO  WS-CK-CERT-NO.                       
           MOVE PI-CERT-EFF-DT         TO  WS-CK-CERT-EFF-DT.                   
                                                                                
           EXEC CICS HANDLE CONDITION                                           
               NOTFND (8880-NOT-FOUND)                                          
           END-EXEC.                                                            
                                                                                
           EXEC CICS READ                                                       
               DATASET (WS-CERTIFICATE-MASTER-DSID)                             
               RIDFLD  (WS-CERTIFICATE-KEY)                                     
               SET     (ADDRESS OF PURGE-CERT-MASTER)                           
           END-EXEC.                                                            
                                                                                
           MOVE PG-CERT-PRIME          TO  CCERTNOO.                            
           MOVE PG-CERT-SFX            TO  CCRTSFXO.                            
           MOVE PG-ACCOUNT             TO  CACCTNOO                             
                                           WS-ACCOUNT.                          
           MOVE PG-STATE               TO  CSTATEO.                             
           MOVE PG-CARRIER             TO  CCARIERO.                            
           MOVE PG-GROUPING            TO  CGROUPO.                             
                                                                                
           IF PG-CERT-EFF-DT  NOT = LOW-VALUES                                  
               MOVE SPACES             TO  DC-OPTION-CODE                       
               MOVE PG-CERT-EFF-DT     TO  DC-BIN-DATE-1                        
               PERFORM 8500-DATE-CONVERSION                                     
               MOVE DC-GREG-DATE-1-EDIT TO CEFDATEO.                            
                                                                                
           IF (PG-MEMB-STATE   NOT = PG-STATE) OR                               
              (PG-MEMB-ACCOUNT NOT = WS-ACCT)                                   
               MOVE AL-SANON           TO  CMEMCAPA                             
               MOVE AL-SANOF           TO  CMEMNOA                              
               MOVE PG-MEMBER-NO       TO  CMEMNOO.                             
                                                                                
           MOVE PG-INSURED-LAST-NAME   TO  CLNAMEO.                             
           MOVE PG-INSURED-FIRST-NAME  TO  CFNAMEO.                             
           MOVE PG-INSURED-INITIAL2    TO  CINITO.                              
           MOVE PG-INSURED-ISSUE-AGE   TO  CAGEO.                               
           MOVE PG-INSURED-JOINT-AGE   TO  CJAGEO.                              
      *    MOVE PG-BENEFICIARY         TO  CBNAMEO.                             
           MOVE PG-JT-LAST-NAME        TO  CJLNAMEO.                            
           MOVE PG-JT-FIRST-NAME       TO  CJFNAMEO.                            
           MOVE PG-JT-INITIAL          TO  CJINITO.                             
           MOVE PG-USER-FIELD          TO  CUSERCDO.                            
                                                                                
           IF (PG-SSN-STATE   NOT = PG-STATE)  OR                               
              (PG-SSN-ACCOUNT NOT = WS-ACCT)                                    
              MOVE PG-SOC-SEC-NO      TO  CSSNO                                 
           ELSE                                                                 
               MOVE SPACES             TO  CSSNO.                               
                                                                                
      *    MOVE PG-LOAN-NUMBER         TO  LOANNOO.                             
                                                                                
      *    MOVE PG-LOAN-BALANCE        TO  LOANBALO.                            
      *    MOVE PG-LOAN-OFFICER        TO  LNOFCO.                              
      *    MOVE PG-POLICY-FORM-NO      TO  CFORMNOO.                            
                                                                                
      *    IF PG-LIVES NUMERIC                                                  
      *        MOVE PG-LIVES           TO  CLIVESO                              
      *    ELSE                                                                 
      *        MOVE ZERO               TO  CLIVESO.                             
                                                                                
      *    IF PG-BILLED NUMERIC                                                 
      *        MOVE PG-BILLED          TO  CBILLEDO                             
      *    ELSE                                                                 
      *        MOVE ZERO               TO  CBILLEDO.                            
                                                                                
           MOVE EIBDATE                TO  DC-JULIAN-YYDDD.                     
           MOVE '5'                    TO  DC-OPTION-CODE.                      
           PERFORM 8500-DATE-CONVERSION.                                        
           MOVE DC-BIN-DATE-1          TO  DC-BIN-DATE-2                        
                                           WS-CURRENT-DATE.                     
           MOVE PG-CERT-EFF-DT         TO  DC-BIN-DATE-1.                       
           MOVE '1'                    TO  DC-OPTION-CODE.                      
           PERFORM 8500-DATE-CONVERSION.                                        
                                                                                
           MOVE DC-ELAPSED-MONTHS      TO  WS-ELAPSED-MONTHS.                   
                                                                                
           IF PG-LF-BENEFIT-CD  = '00'                                          
               GO TO 0200-AH-BENEFIT.                                           
                                                                                
           EJECT                                                                
                                                                                
           IF PG-LF-ALT-BENEFIT-AMT IS NOT NUMERIC                              
               MOVE +0                 TO  PG-LF-ALT-BENEFIT-AMT.               
                                                                                
           IF PG-LF-ALT-PREMIUM-AMT IS NOT NUMERIC                              
               MOVE +0                 TO  PG-LF-ALT-PREMIUM-AMT.               
                                                                                
           MOVE '4'                    TO  WS-CFK-RECORD-TYPE.                  
           MOVE PG-LF-BENEFIT-CD       TO  WS-BENEFIT-NO                        
                                           CLCDO.                               
           MOVE PI-LIFE-OVERRIDE-L2    TO  CLKINDO.                             
                                                                                
           PERFORM 8700-LOCATE-BENEFIT THRU 8700-EXIT.                          
                                                                                
           IF CF-SUMMARY-PROCESSING (WS-INDEX)                                  
               MOVE AL-UNNOF           TO CLTERMA.                              
                                                                                
           MOVE WS-BENEFIT-DESCRIP     TO  CLDESCO.                             
           MOVE CF-LF-COVERAGE-TYPE (WS-INDEX) TO CP-BENEFIT-TYPE.              
           MOVE CF-CO-EARNINGS-CALC (WS-INDEX) TO CP-EARNING-METHOD.            
           MOVE CF-SPECIAL-CALC-CD (WS-INDEX) TO CP-SPECIAL-CALC-CD.            
                                                                                
           MOVE PG-CERT-EFF-DT         TO  CP-CERT-EFF-DT.                      
           MOVE PG-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.                   
           MOVE WS-CURRENT-DATE        TO  CP-VALUATION-DT.                     
                                                                                
           MOVE WS-KIND                TO  CLEDESCO.                            
                                                                                
           COMPUTE WS-LF-ALT = PG-LF-BENEFIT-AMT                                
                             + PG-LF-ALT-BENEFIT-AMT.                           
                                                                                
           MOVE WS-LF-ALT              TO  CLBENO.                              
                                                                                
           MOVE PG-LF-ORIG-TERM        TO  CLTERMO                              
                                           CP-ORIGINAL-TERM.                    
           MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.               
           MOVE '4'                    TO  CP-REM-TERM-METHOD.                  
           MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.                       
                                                                                
      *** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***                     
                                                                                
           PERFORM 8400-READ-STATE-CNTL THRU 8400-EXIT.                         
                                                                                
           IF WS-ST-REC-NOT-FOUND = ZERO                                        
              MOVE CF-ST-FREE-LOOK-PERIOD  TO CP-FREE-LOOK                      
           ELSE                                                                 
              MOVE ER-2848             TO EMI-ERROR                             
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                          
              GO TO 8100-SEND-INITIAL-MAP.                                      
                                                                                
           PERFORM 9800-LINK-REM-TERM.                                          
           MOVE CP-REMAINING-TERM-3    TO  CLREMO.                              
                                                                                
           IF CP-REMAINING-TERM-3 > PG-LF-ORIG-TERM                             
              MOVE PG-LF-ORIG-TERM     TO  CLREMO.                              
                                                                                
           IF PG-LF-CURRENT-STATUS = '8'                                        
              IF PG-LF-CANCEL-DT NOT = LOW-VALUES                               
                  MOVE PG-LF-CANCEL-DT TO DC-BIN-DATE-1                         
                  MOVE SPACES          TO DC-OPTION-CODE                        
                  PERFORM 8500-DATE-CONVERSION                                  
                  IF NOT DATE-CONVERSION-ERROR                                  
                      MOVE DC-GREG-DATE-1-EDIT TO CLCANCLO.                     
                                                                                
           IF PG-LF-CURRENT-STATUS = '7'                                        
              IF PG-LF-DEATH-DT NOT = LOW-VALUES                                
                  MOVE PG-LF-DEATH-DT TO DC-BIN-DATE-1                          
                  MOVE SPACES          TO DC-OPTION-CODE                        
                  PERFORM 8500-DATE-CONVERSION                                  
                  IF NOT DATE-CONVERSION-ERROR                                  
                      MOVE DC-GREG-DATE-1-EDIT TO CLCANCLO.                     
                                                                                
           IF PG-LF-CURRENT-STATUS = '1' OR '4'                                 
              IF CP-REMAINING-TERM-3 = ZEROS                                    
                 MOVE 'EXPIRED'        TO WS-STATUS-DESC                        
                 MOVE AL-SABOF         TO CLSTATA                               
              ELSE                                                              
                 MOVE 'ACTIVE'         TO WS-STATUS-DESC.                       
                                                                                
           IF PG-LF-CURRENT-STATUS = '2'                                        
              MOVE 'PEND  '            TO WS-STATUS-DESC                        
              MOVE 'P'                 TO PI-PEND-SW.                           
                                                                                
           IF PG-LF-CURRENT-STATUS = '3'                                        
              MOVE 'RESTORE'           TO WS-STATUS-DESC.                       
                                                                                
           IF PG-LF-CURRENT-STATUS = '5'                                        
              MOVE 'REISSUE'           TO WS-STATUS-DESC.                       
                                                                                
           IF PG-LF-CURRENT-STATUS = '6'                                        
              MOVE 'LMP DIS'           TO WS-STATUS-DESC.                       
                                                                                
           IF PG-LF-CURRENT-STATUS = '7'                                        
              MOVE 'DEATH  '           TO WS-STATUS-DESC.                       
                                                                                
           IF PG-LF-CURRENT-STATUS = '8'                                        
              MOVE 'CANCEL '           TO WS-STATUS-DESC.                       
                                                                                
           IF PG-LF-CURRENT-STATUS = '9'                                        
              MOVE 'RE-ONLY'           TO WS-STATUS-DESC.                       
                                                                                
           IF PG-LF-CURRENT-STATUS = 'D'                                        
              MOVE 'DECLINE'           TO WS-STATUS-DESC.                       
                                                                                
           IF PG-LF-CURRENT-STATUS = 'V'                                        
              MOVE 'VOID'              TO WS-STATUS-DESC.                       
                                                                                
      *    IF PG-POLICY-UNDERWRITTEN  OR                                        
      *       PG-ENTRY-STATUS = 'U'                                             
      *        MOVE ' - UW'            TO WS-UW-STATUS                          
      *    ELSE                                                                 
      *        MOVE SPACES             TO WS-UW-STATUS.                         
                                                                                
           MOVE WS-STATUS-DESC         TO CLSTATO.                              
                                                                                
           COMPUTE WS-LF-ALT = PG-LF-PREMIUM-AMT                                
                             + PG-LF-ALT-PREMIUM-AMT.                           
                                                                                
           MOVE WS-LF-ALT              TO CLPREMO.                              
                                                                                
       0200-AH-BENEFIT.                                                         
           IF PG-AH-BENEFIT-CD = '00'                                           
               GO TO 0300-CONTINUE.                                             
                                                                                
           MOVE '5'                    TO  WS-CFK-RECORD-TYPE.                  
           MOVE PG-AH-BENEFIT-CD       TO  WS-BENEFIT-NO                        
                                           CACDO.                               
           MOVE PI-AH-OVERRIDE-L2      TO  CAKINDO.                             
                                                                                
           PERFORM 8700-LOCATE-BENEFIT THRU 8700-EXIT.                          
                                                                                
           IF  CF-SUMMARY-PROCESSING   (WS-INDEX)                               
               MOVE AL-UNNOF           TO CATERMA.                              
                                                                                
           MOVE 'A'                    TO CP-BENEFIT-TYPE.                      
           MOVE CF-CO-EARNINGS-CALC (WS-INDEX) TO CP-EARNING-METHOD.            
           MOVE CF-SPECIAL-CALC-CD (WS-INDEX) TO CP-SPECIAL-CALC-CD.            
           MOVE WS-BENEFIT-DESCRIP     TO  CADESCO.                             
           MOVE WS-KIND                TO  CAEDESCO.                            
           MOVE PG-CERT-EFF-DT         TO  CP-CERT-EFF-DT.                      
           MOVE PG-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.                   
           MOVE WS-CURRENT-DATE        TO  CP-VALUATION-DT.                     
           MOVE PG-AH-ORIG-TERM        TO  CATERMO                              
                                           CP-ORIGINAL-TERM.                    
           MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.               
           MOVE '4'                    TO  CP-REM-TERM-METHOD.                  
           MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.                       
                                                                                
      *** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***                     
                                                                                
           PERFORM 8400-READ-STATE-CNTL THRU 8400-EXIT.                         
                                                                                
           IF WS-ST-REC-NOT-FOUND = ZERO                                        
              MOVE CF-ST-FREE-LOOK-PERIOD  TO CP-FREE-LOOK                      
           ELSE                                                                 
              MOVE ER-2848             TO EMI-ERROR                             
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                          
              GO TO 8100-SEND-INITIAL-MAP.                                      
                                                                                
           PERFORM 9800-LINK-REM-TERM.                                          
           MOVE CP-REMAINING-TERM-3    TO  CAREMO.                              
                                                                                
           MOVE PG-AH-BENEFIT-AMT      TO  CABENO.                              
                                                                                
           IF PG-AH-CURRENT-STATUS = '8'                                        
              IF PG-AH-CANCEL-DT  NOT = LOW-VALUES                              
                  MOVE PG-AH-CANCEL-DT TO DC-BIN-DATE-1                         
                  MOVE SPACES          TO DC-OPTION-CODE                        
                  PERFORM 8500-DATE-CONVERSION                                  
                  IF NOT DATE-CONVERSION-ERROR                                  
                      MOVE DC-GREG-DATE-1-EDIT TO CACANCLO.                     
                                                                                
           IF PG-AH-CURRENT-STATUS = '6' OR '7'                                 
              IF PG-AH-SETTLEMENT-DT  NOT = LOW-VALUES                          
                  MOVE PG-AH-SETTLEMENT-DT TO DC-BIN-DATE-1                     
                  MOVE SPACES          TO DC-OPTION-CODE                        
                  PERFORM 8500-DATE-CONVERSION                                  
                  IF NOT DATE-CONVERSION-ERROR                                  
                      MOVE DC-GREG-DATE-1-EDIT TO CACANCLO.                     
                                                                                
           IF PG-AH-CURRENT-STATUS = '1' OR '4'                                 
              IF CP-REMAINING-TERM-3 = ZEROS                                    
                 MOVE 'EXPIRED'        TO WS-STATUS-DESC                        
                 MOVE AL-SABOF         TO CASTATA                               
              ELSE                                                              
                 MOVE 'ACTIVE'         TO WS-STATUS-DESC.                       
                                                                                
           IF PG-AH-CURRENT-STATUS = '2'                                        
              MOVE 'PEND  '            TO WS-STATUS-DESC                        
              MOVE 'P'                 TO PI-PEND-SW.                           
                                                                                
           IF PG-AH-CURRENT-STATUS = '3'                                        
              MOVE 'RESTORE'           TO WS-STATUS-DESC.                       
                                                                                
           IF PG-AH-CURRENT-STATUS = '5'                                        
              MOVE 'REISSUE'           TO WS-STATUS-DESC.                       
                                                                                
           IF PG-AH-CURRENT-STATUS = '6'                                        
              MOVE 'LMP DIS'           TO WS-STATUS-DESC.                       
                                                                                
           IF PG-AH-CURRENT-STATUS = '7'                                        
              MOVE 'DEATH  '           TO WS-STATUS-DESC.                       
                                                                                
           IF PG-AH-CURRENT-STATUS = '8'                                        
              MOVE 'CANCEL '           TO WS-STATUS-DESC.                       
                                                                                
           IF PG-AH-CURRENT-STATUS = '9'                                        
              MOVE 'RE-ONLY'           TO WS-STATUS-DESC.                       
                                                                                
           IF PG-AH-CURRENT-STATUS = 'D'                                        
              MOVE 'DECLINE'           TO WS-STATUS-DESC.                       
                                                                                
           IF PG-AH-CURRENT-STATUS = 'V'                                        
              MOVE 'VOID'              TO WS-STATUS-DESC.                       
                                                                                
           MOVE SPACES             TO WS-UW-STATUS.                             
                                                                                
           MOVE WS-STATUS-DESC         TO CASTATO.                              
                                                                                
           MOVE PG-AH-PREMIUM-AMT      TO CAPREMO.                              
                                                                                
       0300-CONTINUE.                                                           
      *    IF PG-NOTE-SW EQUAL ' '                                              
              MOVE '   '               TO  CNOTESO                              
      *    ELSE                                                                 
      *       MOVE 'YES'               TO  CNOTESO.                             
                                                                                
           MOVE PG-INSURED-SEX         TO  CSEXO.                               
           MOVE PG-LOAN-APR            TO  CAPRO.                               
           MOVE PG-IND-GRP-TYPE        TO  CINDGRPO.                            
      *    MOVE PG-PREMIUM-TYPE        TO  CPREMTPO.                            
                                                                                
      *    IF PG-SING-PRM                                                       
              MOVE '  '                TO CPTDESCO                              
      *       ELSE                                                              
      *       IF PG-O-B-COVERAGE                                                
      *          MOVE 'OB'             TO CPTDESCO                              
      *          ELSE                                                           
      *          IF PG-OPEN-END                                                 
      *             MOVE 'OE'          TO CPTDESCO.                             
                                                                                
      *    IF PG-CLAIM-DEDUCT-WITHHELD NOT NUMERIC                              
               MOVE ZEROS              TO CCLMDEDO                              
      *    ELSE                                                                 
      *        MOVE PG-CLAIM-DEDUCT-WITHHELD                                    
      *                                TO CCLMDEDO.                             
                                                                                
      *    IF PG-CANCEL-DEDUCT-WITHHELD NOT NUMERIC                             
               MOVE ZEROS              TO CCANDEDO                              
      *    ELSE                                                                 
      *        MOVE PG-CANCEL-DEDUCT-WITHHELD                                   
      *                                TO CCANDEDO.                             
                                                                                
                                                                                
           MOVE ' '                    TO  CCSRCDO.                             
                                                                                
           MOVE '                     '        TO CNOTE2O.                      
                                                                                
           MOVE AL-SANOF               TO  CMEMNOA   CLNAMEA   CINITA           
                                           CFNAMEA   CSEXA     CSSNA.           
           MOVE AL-SANOF               TO  CAGEA     CAPRA     CJAGEA           
                                           CCLMDEDA  CCANDEDA.                  
                                                                                
           IF PI-MAIL-YES                                                       
               MOVE AL-SANON               TO  PF6KEYA                          
           ELSE                                                                 
               MOVE AL-SADOF               TO  PF6KEYA.                         
                                                                                
           MOVE AL-SADOF           TO CNOTE1A.                                  
                                                                                
           IF (PG-LF-CURRENT-STATUS = '5' OR                                    
               PG-AH-CURRENT-STATUS = '5')                                      
                         AND                                                    
               PG-ENTRY-STATUS = '5'                                            
                  MOVE AL-SANOF        TO  CLCDA      CACDA                     
                                           CLTERMA    CATERMA                   
                                           CLPREMA    CAPREMA                   
                                           CLBENA     CABENA.                   
                                                                                
           GO TO 8100-SEND-INITIAL-MAP.                                         
                                                                                
           EJECT                                                                
       1000-EDIT-MAP.                                                           
           MOVE LOW-VALUES             TO EL128CI.                              
                                                                                
           IF EIBAID = DFHCLEAR                                                 
               GO TO 9400-CLEAR.                                                
                                                                                
           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                               
               MOVE ER-0008            TO EMI-ERROR                             
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                         
               GO TO 8200-SEND-DATAONLY.                                        
                                                                                
           EXEC CICS RECEIVE                                                    
               MAPSET (WS-MAPSET-NAME)                                          
               MAP    (WS-MAP-NAME)                                             
               INTO   (EL128CI)                                                 
           END-EXEC.                                                            
                                                                                
           IF CEMSG2L = 0                                                       
               GO TO 1100-CHECK-PFKEYS.                                         
                                                                                
           IF EIBAID NOT = DFHENTER                                             
               MOVE ER-0008            TO EMI-ERROR                             
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                         
               MOVE -1                 TO CEMSG2L                               
               GO TO 8200-SEND-DATAONLY.                                        
                                                                                
           IF (CEMSG2I NUMERIC) AND (CEMSG2I > 0 AND < 25)                      
               MOVE PF-VALUES (CEMSG2I)    TO  EIBAID                           
           ELSE                                                                 
               MOVE ER-0029                    TO  EMI-ERROR                    
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                         
               MOVE -1                 TO CEMSG2L                               
               GO TO 8200-SEND-DATAONLY.                                        
                                                                                
       1100-CHECK-PFKEYS.                                                       
                                                                                
           IF EIBAID = DFHPF23                                                  
               GO TO 9000-RETURN-CICS.                                          
                                                                                
           IF EIBAID = DFHPF24                                                  
               MOVE 'EL126 '           TO  THIS-PGM                             
               GO TO 9300-XCTL.                                                 
                                                                                
           IF PI-COMPANY-ID NOT = 'DMD'                                         
           IF PI-MAIL-YES                                                       
               IF EIBAID = DFHPF6                                               
                   IF PI-COMPANY-ID = 'AIG' OR 'AUK'                            
                       MOVE ER-0029    TO  EMI-ERROR                            
                       MOVE -1         TO  CEMSG2L                              
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
                       GO TO 8200-SEND-DATAONLY                                 
                   ELSE                                                         
                       MOVE 'EL1277'   TO  THIS-PGM                             
                       GO TO 9300-XCTL.                                         
                                                                                
                                                                                
           IF EIBAID = DFHPF9                                                   
              MOVE 'PF9'               TO PI-PFKEY                              
              PERFORM 3000-WRITE-PI-TS THRU 3000-EXIT                           
              MOVE '1'                 TO PI-PROGRAM-WORK-AREA                  
              MOVE 'EL6314'            TO THIS-PGM                              
              GO TO 9300-XCTL                                                   
           END-IF                                                               
                                                                                
           .                                                                    
       1110-CHECK-PF12.                                                         
           IF EIBAID = DFHPF12                                                  
              MOVE 'EL010 '            TO  THIS-PGM                             
              GO TO 9300-XCTL.                                                  
                                                                                
           IF NOT MODIFY-CAP                                                    
               MOVE 'UPDATE'           TO  SM-READ                              
               PERFORM 9995-SECURITY-VIOLATION                                  
               MOVE ER-0070            TO  EMI-ERROR                            
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                         
               GO TO 8100-SEND-INITIAL-MAP.                                     
                                                                                
           IF EIBAID NOT = DFHENTER                                             
               MOVE ER-0008            TO EMI-ERROR                             
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                         
               GO TO 8200-SEND-DATAONLY.                                        
                                                                                
           IF PI-COMPANY-ID NOT = 'DMD'                                         
               MOVE ZEROS             TO CBILLEDL.                              
                                                                                
                                                                                
                                                                                
       1000-TEST-ERROR.                                                         
                                                                                
           IF EMI-NO-ERRORS                                                     
               NEXT SENTENCE                                                    
           ELSE                                                                 
               GO TO 8200-SEND-DATAONLY.                                        
                                                                                
           IF NO-UPDATES-MADE                                                   
               GO TO 0100-DISPLAY-CERTIFICATE.                                  
                                                                                
      ******************************************************************        
      *       READ CONTROL FILE TO ACQUIRE CURRENT MONTH END DATE      *        
      ******************************************************************        
                                                                                
           MOVE LOW-VALUES             TO  WS-CONTROL-FILE-KEY.                 
           MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.                   
           MOVE '1'                    TO  WS-CFK-RECORD-TYPE.                  
           MOVE SPACES                 TO  WS-CFK-ACCESS.                       
           MOVE +0                     TO  WS-CFK-SEQUENCE-NO.                  
                                                                                
           EXEC CICS READ                                                       
               DATASET  (WS-CONTROL-FILE-DSID)                                  
               RIDFLD   (WS-CONTROL-FILE-KEY)                                   
               SET      (ADDRESS OF CONTROL-FILE)                               
           END-EXEC.                                                            
                                                                                
                                                                                
       1000-CONTINUE-EDITS.                                                     
                                                                                
                                                                                
           GO TO 0100-DISPLAY-CERTIFICATE.                                      
                                                                                
           EJECT                                                                
       3000-WRITE-PI-TS.                                                        
                                                                                
           EXEC CICS WRITEQ TS                                                  
               QUEUE  (W-QID)                                                   
               FROM   (PROGRAM-INTERFACE-BLOCK)                                 
               LENGTH (PI-COMM-LENGTH)                                          
               ITEM   (W-ONE)                                                   
           END-EXEC.                                                            
                                                                                
       3000-EXIT.                                                               
           EXIT.                                                                
           EJECT                                                                
       3100-RECOVER-PI-TS.                                                      
                                                                                
           EXEC CICS HANDLE CONDITION                                           
               QIDERR  (3100-QID-ERROR)                                         
           END-EXEC.                                                            
                                                                                
           EXEC CICS READQ TS                                                   
               QUEUE  (W-QID)                                                   
               LENGTH (PI-COMM-LENGTH)                                          
               INTO   (PROGRAM-INTERFACE-BLOCK)                                 
               ITEM   (W-ONE)                                                   
           END-EXEC.                                                            
                                                                                
       3100-DELETE-PI-TS.                                                       
                                                                                
           EXEC CICS DELETEQ TS                                                 
               QUEUE  (W-QID)                                                   
           END-EXEC.                                                            
                                                                                
           GO TO 3100-EXIT.                                                     
                                                                                
       3100-QID-ERROR.                                                          
                                                                                
           MOVE ER-0033                TO  EMI-ERROR.                           
           PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                          
                                                                                
       3100-EXIT.                                                               
           EXIT.                                                                
           EJECT                                                                
       8100-SEND-INITIAL-MAP.                                                   
           MOVE SAVE-DATE              TO  CDATEO.                              
           MOVE EIBTIME                TO  TIME-IN.                             
           MOVE TIME-OUT               TO  CTIMEO.                              
                                                                                
           IF EMI-ERROR  NOT = ZERO                                             
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                         
           ELSE                                                                 
               IF TRANSACTION-SUCCESSFUL                                        
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                        
                                                                                
           MOVE EMI-MESSAGE-AREA (1)    TO  CEMSG1O.                            
           MOVE PI-MEMBER-CAPTION       TO  CMEMCAPO.                           
                                                                                
           IF PI-COMPANY-ID = 'DMD'                                             
               MOVE AL-SANOF          TO DMDBILA                                
                                         CLNAMEA                                
                                         CFNAMEA                                
                                         CINITA                                 
                                         CAGEA                                  
                                         CSEXA                                  
                                         CSSNA                                  
                                         CJLNAMEA                               
                                         CJFNAMEA                               
                                         CJINITA                                
                                         CJAGEA                                 
                                         CBNAMEA                                
                                         LOANNOA                                
                                         LOANBALA                               
                                         LNOFCA                                 
                                         CAPRA                                  
      *                                  CFORMNOA                               
                                         CUSERCDA                               
                                         CINDGRPA                               
                                         CPREMTPA                               
                                         CLIVESA                                
                                         CCLMDEDA                               
                                         CCANDEDA                               
                                         CBILLEDA                               
                                         CMEMNOA                                
               MOVE -1                TO CEMSG2L                                
           ELSE                                                                 
               MOVE -1                TO CMEMNOL.                               
                                                                                
           EXEC CICS SEND                                                       
               FROM   (EL128CI)                                                 
               MAPSET (WS-MAPSET-NAME)                                          
               MAP    (WS-MAP-NAME)                                             
               CURSOR                                                           
               ERASE                                                            
           END-EXEC.                                                            
                                                                                
           GO TO 9100-RETURN-TRAN.                                              
                                                                                
       8100-EXIT.                                                               
           EXIT.                                                                
                                                                                
           EJECT                                                                
       8200-SEND-DATAONLY.                                                      
           MOVE SAVE-DATE              TO  CDATEO.                              
           MOVE EIBTIME                TO  TIME-IN.                             
           MOVE TIME-OUT               TO  CTIMEO.                              
                                                                                
           IF EMI-ERROR  NOT = ZERO                                             
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                        
                                                                                
           MOVE EMI-MESSAGE-AREA (1)   TO  CEMSG1O.                             
           MOVE PI-MEMBER-CAPTION      TO  CMEMCAPO.                            
                                                                                
           IF PI-COMPANY-ID = 'DMD'                                             
               MOVE AL-SANOF          TO DMDBILA                                
                                         CLNAMEA                                
                                         CFNAMEA                                
                                         CINITA                                 
                                         CAGEA                                  
                                         CSEXA                                  
                                         CSSNA                                  
                                         CJLNAMEA                               
                                         CJFNAMEA                               
                                         CJINITA                                
                                         CJAGEA                                 
                                         CBNAMEA                                
                                         LOANNOA                                
                                         LOANBALA                               
                                         LNOFCA                                 
                                         CAPRA                                  
      *                                  CFORMNOA                               
                                         CUSERCDA                               
                                         CINDGRPA                               
                                         CPREMTPA                               
                                         CLIVESA                                
                                         CCLMDEDA                               
                                         CCANDEDA                               
                                         CBILLEDA                               
                                         CMEMNOA                                
               MOVE -1                TO CEMSG2L.                               
                                                                                
           EXEC CICS SEND DATAONLY                                              
               FROM   (EL128CI)                                                 
               MAPSET (WS-MAPSET-NAME)                                          
               MAP    (WS-MAP-NAME)                                             
               CURSOR                                                           
           END-EXEC.                                                            
                                                                                
           GO TO 9100-RETURN-TRAN.                                              
                                                                                
       8200-EXIT.                                                               
           EXIT.                                                                
                                                                                
           EJECT                                                                
       8300-SEND-TEXT.                                                          
           EXEC CICS SEND TEXT                                                  
               FROM   (LOGOFF-TEXT)                                             
               LENGTH (LOGOFF-LENGTH)                                           
               ERASE                                                            
               FREEKB                                                           
           END-EXEC.                                                            
                                                                                
           EXEC CICS RETURN                                                     
           END-EXEC.                                                            
                                                                                
       8300-EXIT.                                                               
           EXIT.                                                                
           EJECT                                                                
                                                                                
       8400-READ-STATE-CNTL.                                                    
                                                                                
           MOVE ZERO                   TO  WS-ST-REC-NOT-FOUND.                 
           MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.                   
           MOVE '3'                    TO  WS-CFK-RECORD-TYPE.                  
           MOVE PI-STATE               TO  WS-CFK-ACCESS.                       
           MOVE +0                     TO  WS-CFK-SEQUENCE-NO.                  
                                                                                
           EXEC CICS HANDLE CONDITION                                           
               NOTFND (8410-STATE-REC-NOTFND)                                   
           END-EXEC.                                                            
                                                                                
           EXEC CICS READ                                                       
               DATASET  (WS-CONTROL-FILE-DSID)                                  
               RIDFLD   (WS-CONTROL-FILE-KEY)                                   
               SET      (ADDRESS OF CONTROL-FILE)                               
           END-EXEC.                                                            
                                                                                
           GO TO 8400-EXIT.                                                     
                                                                                
       8410-STATE-REC-NOTFND.                                                   
           MOVE +1                     TO WS-ST-REC-NOT-FOUND.                  
                                                                                
       8400-EXIT.                                                               
           EXIT.                                                                
           EJECT                                                                
                                                                                
       8500-DATE-CONVERSION.                                                    
           EXEC CICS LINK                                                       
               PROGRAM  (ELDATCV)                                               
               COMMAREA (DATE-CONVERSION-DATA)                                  
               LENGTH   (DC-COMM-LENGTH)                                        
           END-EXEC.                                                            
                                                                                
       8500-EXIT.                                                               
           EXIT.                                                                
                                                                                
       8600-DEEDIT.                                                             
           EXEC CICS BIF                                                        
                DEEDIT                                                          
                FIELD  (DEEDIT-FIELD)                                           
                LENGTH (15)                                                     
           END-EXEC.                                                            
                                                                                
       8600-EXIT.                                                               
           EXIT.                                                                
                                                                                
       8700-LOCATE-BENEFIT.                                                     
           EXEC CICS HANDLE CONDITION                                           
               NOTFND (8700-EXIT)                                               
           END-EXEC.                                                            
                                                                                
           MOVE SPACES                 TO  WS-KIND.                             
           MOVE ZERO                   TO  WS-NOT-FOUND.                        
                                                                                
           MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.                   
           MOVE SPACES                 TO  WS-CFK-ACCESS.                       
           MOVE WS-BENEFIT-NO          TO  WS-CFK-BENEFIT-NO.                   
                                                                                
           EXEC CICS READ                                                       
               DATASET (WS-CONTROL-FILE-DSID)                                   
               RIDFLD  (WS-CONTROL-FILE-KEY)                                    
               SET     (ADDRESS OF CONTROL-FILE)                                
               GTEQ                                                             
           END-EXEC.                                                            
                                                                                
           IF WS-CFK-COMPANY-ID  NOT = CF-COMPANY-ID  OR                        
              WS-CFK-RECORD-TYPE NOT = CF-RECORD-TYPE                           
                GO TO 8700-EXIT.                                                
                                                                                
           MOVE +1                     TO  WS-INDEX.                            
                                                                                
       8700-LOOKUP-BENEFIT.                                                     
           IF WS-BENEFIT-NO = CF-BENEFIT-CODE (WS-INDEX)                        
               MOVE CF-BENEFIT-ALPHA (WS-INDEX)   TO WS-KIND                    
               MOVE CF-SPECIAL-CALC-CD (WS-INDEX) TO WS-CALC-CD                 
               MOVE CF-BENEFIT-DESCRIP (WS-INDEX) TO WS-BENEFIT-DESCRIP         
               MOVE CF-JOINT-INDICATOR (WS-INDEX) TO WS-JOINT-INDICATOR         
               MOVE +1                            TO WS-NOT-FOUND               
               GO TO 8700-EXIT.                                                 
                                                                                
           IF CF-BENEFIT-CODE (WS-INDEX) NOT < CF-HI-BEN-IN-REC                 
               GO TO 8700-EXIT.                                                 
                                                                                
           IF WS-INDEX < +8                                                     
               ADD +1  TO  WS-INDEX                                             
               GO TO 8700-LOOKUP-BENEFIT.                                       
                                                                                
       8700-EXIT.                                                               
           EXIT.                                                                
                                                                                
       8880-NOT-FOUND.                                                          
           MOVE ER-0142                TO EMI-ERROR.                            
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                            
           GO TO 8100-SEND-INITIAL-MAP.                                         
                                                                                
       8880-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9000-RETURN-CICS.                                                        
           MOVE EL005                  TO  THIS-PGM.                            
           MOVE EIBAID                 TO  PI-ENTRY-CD-1.                       
           GO TO 9300-XCTL.                                                     
                                                                                
       9000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9100-RETURN-TRAN.                                                        
           MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.                    
           MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.                
                                                                                
           EXEC CICS RETURN                                                     
               COMMAREA (PROGRAM-INTERFACE-BLOCK)                               
               LENGTH   (PI-COMM-LENGTH)                                        
               TRANSID  (WS-TRANS-ID)                                           
           END-EXEC.                                                            
                                                                                
       9100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9300-XCTL.                                                               
           MOVE DFHENTER               TO  EIBAID.                              
                                                                                
           EXEC CICS XCTL                                                       
               PROGRAM  (THIS-PGM)                                              
               COMMAREA (PROGRAM-INTERFACE-BLOCK)                               
               LENGTH   (PI-COMM-LENGTH)                                        
           END-EXEC.                                                            
                                                                                
       9300-EXIT.                                                               
           EXIT.                                                                
                                                                                
           EJECT                                                                
       9400-CLEAR.                                                              
           MOVE PI-RETURN-TO-PROGRAM   TO  THIS-PGM.                            
           GO TO 9300-XCTL.                                                     
                                                                                
           EJECT                                                                
       9900-ERROR-FORMAT.                                                       
           ADD +1                      TO  WS-ERROR-COUNT.                      
                                                                                
           IF EMI-ERRORS-COMPLETE                                               
               MOVE ZERO               TO  EMI-ERROR                            
               GO TO 9900-EXIT.                                                 
                                                                                
           EXEC CICS LINK                                                       
               PROGRAM  (EL001)                                                 
               COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                         
               LENGTH   (EMI-COMM-LENGTH)                                       
           END-EXEC.                                                            
                                                                                
           MOVE ZERO                   TO  EMI-ERROR.                           
                                                                                
       9900-EXIT.                                                               
           EXIT.                                                                
                                                                                
           EJECT                                                                
                                                                                
       9800-LINK-REM-TERM.                                                      
           EXEC CICS LINK                                                       
               PROGRAM  (ELRTRM)                                                
               COMMAREA (CALCULATION-PASS-AREA)                                 
               LENGTH   (CP-COMM-LENGTH)                                        
           END-EXEC.                                                            
                                                                                
       9800-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9990-ERROR.                                                              
           MOVE DFHEIBLK               TO  EMI-LINE1.                           
                                                                                
           EXEC CICS LINK                                                       
               PROGRAM  (EL004)                                                 
               COMMAREA (EMI-LINE1)                                             
               LENGTH   (72)                                                    
           END-EXEC.                                                            
                                                                                
           IF CLAIM-SESSION                                                     
               GO TO 8100-SEND-INITIAL-MAP                                      
           ELSE                                                                 
               GO TO 8200-SEND-DATAONLY.                                        
                                                                                
       9990-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9995-SECURITY-VIOLATION.                                                 
                                   COPY ELCSCTP.                                
