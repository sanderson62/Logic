       IDENTIFICATION DIVISION.                                         
                                                                        
       PROGRAM-ID.                 VP6301.                              
      *                            VMOD=2.075                           
      *                                                                 
      *AUTHOR.     LOGIC,INC.                                           
      *            DALLAS, TEXAS.                                       
                                                                        
      *DATE-COMPILED.                                                   
                                                                        
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO.            *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF    CSO      IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************
                                                                        
      *REMARKS.                                                         
      *         TRANSACTION - VPA6 - NEW BUSINESS - DATA ENTRY (ISSUES).
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING  
      * 100703    2003080800002  PEMA  ADD SUPER GAP PROCESSING
      * 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
      * 081606  CR2006080800002  PEMA  ADD VIN NUMBER
      * 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
      * 072308  CR2008040800002  PEMA  ADD CRED BENE INFORMATION
      * 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
      * 030309  CR2009021700001  PEMA  ADD EDIT FOR BENE AND INS ADDR
      * 020210  IR2010011100002  PEMA  CORRECT ATTRB ON BCZIPCD
      * 030310  IR2010022400001  PEMA  CORRECT PF5 LOGIC, ADD APR PROC
      * 030310  CR2009031200002  PEMA  CHECK LOAN OFF EDIT SWITCH
      * 060211  CR2011051600002  PEMA  OPEN CP FIELD
      * 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
      * 050713  CR2008042200001  PEMA  ADD ZERO PCT APR PROCESSING
      * 111913  CR2008042200001  PEMA  ADDITIONAL 0 % APR CHANGES
      * 020514  IR2014012400001  PEMA  DARK OUT AH ALT BEN FOR CID&AHL
      * 042114  CR2014032000001  PEMA  rearrange dob,jntdob&ssn
      * 071714    2013100100002  PEMA  FIX CRIT PERIOD EDITS
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
062017* 062017  CR2015091000001  PEMA  ADD PROCESSING FOR TN REF INTEREST
      ******************************************************************
      
       ENVIRONMENT DIVISION.                                            
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
       77  FILLER  PIC X(32)  VALUE '********************************'. 
       77  FILLER  PIC X(32)  VALUE '*    VP6301 WORKING STORAGE    *'. 
       77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.075 *********'. 
       77  WS-BEG                      PIC S999 COMP-3 VALUE +0.
       77  WS-END                      PIC S999 COMP-3 VALUE +0.
                                                                        
           COPY ELCSCTM.                                                
                                                                        
           COPY ELCSCRTY.                                               
                                                                        
       01  WS-NUM-TABLE                PIC X(26)  VALUE
           '12345678012345070923456789'.
       01  FILLER REDEFINES WS-NUM-TABLE.
           05  WS-NUM OCCURS 26        PIC 9.
       01  V1                          PIC S999 COMP-3.
       01  V2                          PIC S999 COMP-3.
       01  V3                          PIC S999 COMP-3.
       01  WS-WORK-VIN                 PIC X(17)  VALUE SPACES.
       01  FILLER REDEFINES WS-WORK-VIN.
           05  WS-WORK-VIN-N OCCURS 17        PIC 9.
       01  WS-VIN-TOTAL                PIC S9(9)  VALUE +0.
       01  WS-VIN-FINAL                PIC S9(7)  VALUE +0.
       01  WS-VIN-REMAINDER            PIC S999   VALUE +0.
       01  WS-HEX-WORK.
           05  FILLER                  PIC X  VALUE LOW-VALUES.
           05  WS-HEX-BYTE             PIC X.
       01  WS-CHARCD REDEFINES WS-HEX-WORK PIC S9(4)  COMP.
       01  WS-CHARCD-A                 PIC S9(4)   COMP VALUE +65.
      
       01  WS-COMM-LENGTH          PIC S9(4) COMP VALUE +1900.          
                                                                        
       01  STANDARD-AREAS.                                              
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL              VALUE +00.
               88  RESP-ERROR               VALUE +01.
               88  RESP-NOTFND              VALUE +13.
               88  RESP-NOTOPEN             VALUE +19.
               88  RESP-ENDFILE             VALUE +20.
           12  GETMAIN-SPACE       PIC X       VALUE SPACE.             
           12  VP630B              PIC X(8)    VALUE 'VP630B'.          
           12  VP630C              PIC X(8)    VALUE 'VP630C'.          
           12  MAPSET-VP6301S      PIC X(8)    VALUE 'VP6301S'.         
           12  TRANS-EXA6          PIC X(4)    VALUE 'VPA6'.            
           12  THIS-PGM            PIC X(8)    VALUE 'VP6301'.          
           12  PGM-NAME            PIC X(8).                            
           12  TIME-IN             PIC S9(7).                           
           12  TIME-OUT-R  REDEFINES TIME-IN.                           
               16  FILLER          PIC X.                               
               16  TIME-OUT        PIC 99V99.                           
               16  FILLER          PIC XX.                              
           12  LINK-EL001          PIC X(8)    VALUE 'EL001'.           
           12  LINK-EL004          PIC X(8)    VALUE 'EL004'.           
           12  XCTL-EL005          PIC X(8)    VALUE 'EL005'.           
           12  XCTL-EL010          PIC X(8)    VALUE 'EL010'.           
           12  XCTL-EL626          PIC X(8)    VALUE 'EL626'.           
           12  XCTL-EL630          PIC X(8)    VALUE 'EL630'.           
           12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.         
           12  FILE-ID-ERPNDB      PIC X(8)    VALUE 'ERPNDB'.          
           12  FILE-ID-ERPNDM      PIC X(8)    VALUE 'ERPNDM'.          
           12  FILE-ID-ELCERT      PIC X(8)    VALUE 'ELCERT'.          
           12  FILE-ID-ELCNTL      PIC X(8)    VALUE 'ELCNTL'.          
           12  WS-CURRENT-DT       PIC X(8)    VALUE SPACES.            
           12  WS-CURRENT-BIN-DT   PIC XX      VALUE SPACES.            
           12  WS-TERM-IN-DAYS-SW  PIC X.                               
               88  WS-TERM-IN-DAYS-FOUND       VALUE 'Y'.               
                                                                        
           EJECT                                                        
       01  ERROR-MESSAGES.                                              
           12  ER-0000                 PIC X(4)  VALUE '0000'.          
           12  ER-0004                 PIC X(4)  VALUE '0004'.          
           12  ER-0008                 PIC X(4)  VALUE '0008'.          
           12  ER-0029                 PIC X(4)  VALUE '0029'.          
           12  ER-0070                 PIC X(4)  VALUE '0070'.          
           12  ER-0582                 PIC X(4)  VALUE '0582'.          
           12  ER-1923                 PIC X(4)  VALUE '1923'.          
           12  ER-2049                 PIC X(4)  VALUE '2049'.
           12  ER-2119                 PIC X(4)  VALUE '2119'.          
           12  ER-2212                 PIC X(4)  VALUE '2212'.          
           12  ER-2217                 PIC X(4)  VALUE '2217'.          
           12  ER-2218                 PIC X(4)  VALUE '2218'.          
           12  ER-2200                 PIC X(4)  VALUE '2200'.          
           12  ER-2209                 PIC X(4)  VALUE '2209'.
           12  ER-2220                 PIC X(4)  VALUE '2220'.          
           12  ER-2222                 PIC X(4)  VALUE '2222'.          
           12  ER-2223                 PIC X(4)  VALUE '2223'.          
           12  ER-2224                 PIC X(4)  VALUE '2224'.          
           12  ER-2226                 PIC X(4)  VALUE '2226'.          
           12  ER-2227                 PIC X(4)  VALUE '2227'.          
           12  ER-2228                 PIC X(4)  VALUE '2228'.          
           12  ER-2240                 PIC X(4)  VALUE '2240'.          
           12  ER-2241                 PIC X(4)  VALUE '2241'.          
           12  ER-2247                 PIC X(4)  VALUE '2247'.          
           12  ER-2423                 PIC X(4)  VALUE '2423'.          
           12  ER-2424                 PIC X(4)  VALUE '2424'.          
           12  ER-2425                 PIC X(4)  VALUE '2425'.          
           12  ER-2426                 PIC X(4)  VALUE '2426'.          
           12  ER-2427                 PIC X(4)  VALUE '2427'.          
           12  ER-2428                 PIC X(4)  VALUE '2428'.          
           12  ER-2431                 PIC X(4)  VALUE '2431'.          
           12  ER-2433                 PIC X(4)  VALUE '2433'.          
           12  ER-2437                 PIC X(4)  VALUE '2437'.          
           12  ER-2429                 PIC X(4)  VALUE '2429'.          
           12  ER-2442                 PIC X(4)  VALUE '2442'.          
           12  ER-2471                 PIC X(4)  VALUE '2471'.          
           12  ER-2526                 PIC X(4)  VALUE '2526'.          
           12  ER-2529                 PIC X(4)  VALUE '2529'.          
           12  ER-2531                 PIC X(4)  VALUE '2531'.          
           12  ER-2532                 PIC X(4)  VALUE '2532'.          
           12  ER-2541                 PIC X(4)  VALUE '2541'.          
           12  ER-2542                 PIC X(4)  VALUE '2542'.          
           12  ER-2589                 PIC X(4)  VALUE '2589'.          
           12  ER-2591                 PIC X(4)  VALUE '2591'.          
           12  ER-2592                 PIC X(4)  VALUE '2592'.          
           12  ER-2593                 PIC X(4)  VALUE '2593'.          
           12  ER-2594                 PIC X(4)  VALUE '2594'.          
           12  ER-2629                 PIC X(4)  VALUE '2629'.          
           12  ER-2630                 PIC X(4)  VALUE '2630'.          
           12  ER-2635                 PIC X(4)  VALUE '2635'.          
           12  ER-2636                 PIC X(4)  VALUE '2636'.          
           12  ER-2651                 PIC X(4)  VALUE '2651'.          
           12  ER-2670                 PIC X(4)  VALUE '2670'.          
           12  ER-2683                 PIC X(4)  VALUE '2683'.          
           12  ER-2700                 PIC X(4)  VALUE '2700'.          
           12  ER-2701                 PIC X(4)  VALUE '2701'.          
           12  ER-2702                 PIC X(4)  VALUE '2702'.          
           12  ER-2901                 PIC X(4)  VALUE '2901'.          
           12  ER-2963                 PIC X(4)  VALUE '2963'.
           12  ER-2964                 PIC X(4)  VALUE '2964'.
           12  ER-3166                 PIC X(4)  VALUE '3166'.
           12  ER-3825                 PIC X(4)  VALUE '3825'.
           12  ER-3826                 PIC X(4)  VALUE '3826'.
           12  ER-7400                 PIC X(4)  VALUE '7400'.          
           12  ER-7403                 PIC X(4)  VALUE '7403'.          
           12  ER-7404                 PIC X(4)  VALUE '7404'.          
           12  ER-7405                 PIC X(4)  VALUE '7405'.          
           12  ER-7423                 PIC X(4)  VALUE '7423'.          
           12  ER-7424                 PIC X(4)  VALUE '7424'.          
           12  ER-7530                 PIC X(4)  VALUE '7530'.          
           12  ER-7632                 PIC X(4)  VALUE '7632'.          
           12  ER-7630                 PIC X(4)  VALUE '7630'.          
           12  ER-7631                 PIC X(4)  VALUE '7631'.          
           12  ER-7633                 PIC X(4)  VALUE '7633'.          
           12  ER-7997                 PIC X(4)  VALUE '7997'.          
           12  ER-7998                 PIC X(4)  VALUE '7998'.
           12  ER-9841                 PIC X(4)  VALUE '9841'.
           12  ER-9999                 PIC X(4)  VALUE '9999'.          
                                                                        
           EJECT                                                        
                                                                        
       01  ACCESS-KEYS.                                                 
           12  ERPNDB-KEY.                                              
               16  ERPNDB-COMP-CD          PIC X     VALUE SPACE.       
               16  ERPNDB-ENTRY-BATCH      PIC X(6)  VALUE SPACES.      
               16  ERPNDB-BATCH-SEQ        PIC S9(4) VALUE +1 COMP.     
               16  ERPNDB-BATCH-CHG-SEQ    PIC S9(4) VALUE +0 COMP.     
                                                                        
           12  ERPNDB-RECORD-LENGTH        PIC S9(4) COMP VALUE +585.   
           12  ERPNDB-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +608.   
                                                                        
           12  ELCNTL-KEY.                                              
               16  ELCNTL-COMPANY-ID       PIC X(3)  VALUE SPACES.      
               16  ELCNTL-REC-TYPE         PIC X     VALUE SPACES.      
               16  ELCNTL-ACCESS.                                       
                   20  FILLER              PIC XX.                      
                   20  ELCNTL-HI-BEN       PIC XX.                      
               16  ELCNTL-SEQ              PIC S9(4) VALUE +0 COMP.     
                                                                        
           12  ELCNTL-RECORD-LENGTH        PIC S9(4) COMP VALUE +504.   
           12  ELCNTL-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +527.   
                                                                        
           12  ELCERT-KEY.                                              
               16  ELCERT-COMPANY-CD       PIC X.                       
               16  ELCERT-CARRIER          PIC X.                       
               16  ELCERT-GROUPING         PIC X(6).                    
               16  ELCERT-STATE            PIC XX.                      
               16  ELCERT-ACCOUNT          PIC X(10).                   
               16  ELCERT-CERT-EFF-DT      PIC XX.                      
               16  ELCERT-CERT-NO.                                      
                   20  ELCERT-CERT-PRIME   PIC X(10).                   
                   20  ELCERT-CERT-SFX     PIC X.                       

           12  ELCRTT-KEY.                                              
               16  ELCRTT-COMPANY-CD       PIC X.                       
               16  ELCRTT-CARRIER          PIC X.                       
               16  ELCRTT-GROUPING         PIC X(6).                    
               16  ELCRTT-STATE            PIC XX.                      
               16  ELCRTT-ACCOUNT          PIC X(10).                   
               16  ELCRTT-CERT-EFF-DT      PIC XX.                      
               16  ELCRTT-CERT-NO.                                      
                   20  ELCRTT-CERT-PRIME   PIC X(10).                   
                   20  ELCRTT-CERT-SFX     PIC X.
               16  ELCRTT-TRLR-TYPE        PIC X.

           12  ELCRTT-RECORD-LENGTH        PIC S9(4) COMP VALUE +552.
           12  ELCERT-RECORD-LENGTH        PIC S9(4) COMP VALUE +450.   
           12  ELCERT-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +473.   
                                                                        
           12  ERPNDM-KEY.                                              
               16  ERPNDM-COMP-CD          PIC X     VALUE SPACE.       
               16  ERPNDM-ENTRY-BATCH      PIC X(6)  VALUE SPACES.      
               16  ERPNDM-BATCH-SEQ        PIC S9(4) VALUE +1 COMP.     
               16  ERPNDM-BATCH-CHG-SEQ    PIC S9(4) VALUE +0 COMP.     
                                                                        
      *    12  ERPNDM-RECORD-LENGTH        PIC S9(4) COMP VALUE +250.
           12  ERPNDM-RECORD-LENGTH        PIC S9(4) COMP VALUE +374.
      *    12  ERPNDM-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +273.
           12  ERPNDM-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +397.
                                                                        
           EJECT                                                        
       01  WORK-AREA.                                                   
           12  DEEDIT-FIELD            PIC X(15).                       
           12  FILLER REDEFINES DEEDIT-FIELD.                           
               16  FILLER              PIC X(4).                        
               16  DEEDIT-FIELD-X11    PIC X(11).                       
           12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD PIC S9(15).       
           12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD PIC S9(13)V99.    
           12  DEEDIT-FIELD-V3 REDEFINES DEEDIT-FIELD PIC S9(12)V9(3).  
           12  DEEDIT-FIELD-V4 REDEFINES DEEDIT-FIELD PIC S9(11)V9(4).  
           12  DEEDIT-FIELD-V5 REDEFINES DEEDIT-FIELD PIC S9(10)V9(5).  
                                                                        
           12  WS-SUB                  PIC S9(4) VALUE +0  COMP.        
           12  WS-SUB1                 PIC S9(4) VALUE +0  COMP.        
           12  WS-SUB2                 PIC S9(4) VALUE +0  COMP.        
           12  WS-SUB3                 PIC S9(4) VALUE +0  COMP.        
           12  WS-ACCT-SUB             PIC S9(4) VALUE +0  COMP.        
           12  WS-COV-SUB              PIC S9(4) VALUE +0  COMP.        
           12  WS-EDIT-SUB             PIC S9(4) VALUE +0  COMP.        
           12  CENTURY-ADJ             PIC S9(08) VALUE +38400 COMP.    
           12  WS-WORK-BIN-RED         PIC S9(08) VALUE +0 COMP.        
           12  FILLER REDEFINES WS-WORK-BIN-RED.                        
               16  FILLER              PIC X(02).                       
               16  WS-WORK-BIN-DT      PIC X(02).                       
                                                                        
           12  WS-CALC-TERM            PIC S999V9(5) VALUE ZEROS.       
           12  WS-CALC-TERM-R REDEFINES WS-CALC-TERM.                   
               16  WS-CALC-TERM-WHOLE  PIC S999.                        
               16  WS-CALC-TERM-REMAIN PIC SV9(5).                      
                                                                        
           12  ERROR-SW                PIC X     VALUE SPACE.           
               88  NO-ERROR                VALUE SPACE.                 
               88  ERRORS                  VALUE 'Y'.                   
               88  WS-COVERAGE-PRESENT     VALUE 'Y'.                   
                                                                        
           12  WS-DATA-KEYED-SW        PIC X     VALUE SPACE.           
               88  WS-DATA-NOT-KEYED       VALUE SPACE.                 
               88  WS-DATA-KEYED           VALUE 'Y'.                   
                                                                        
           12  WS-EDITED-LF-CODE       PIC XX   VALUE SPACES.           
           12  WS-LF-ABBR-DESC         PIC XXX  VALUE SPACES.
           12  ws-lf-earnings-calc     pic x    value spaces.
                                                                        
           12  WS-EDITED-AH-CODE       PIC XX   VALUE ZEROS.            
           12  WS-AH-ABBR-DESC         PIC XXX  VALUE SPACES.           
                                                                        
           12  WS-BEN-CD               PIC XX   VALUE SPACES.           
                                                                        
           12  WS-ENTRY-CODE           PIC X     VALUE SPACE.           
               88  WS-ENTRY-CODE-VALID   VALUE ' ' 'E' 'R' 'P'          
                                               'M' 'D' 'V' 'U'.         
                                                                        
           12  WS-FORCE-CODE           PIC X     VALUE SPACE.           
               88  WS-FORCE-CODE-VALID   VALUE ' ' 'A' 'D'.             
                                                                        
           12  WS-ALL-NINES            PIC S9(7)V99 VALUE +9999999.99.  
                                                                        
           12  WS-MODE-CODE            PIC X     VALUE SPACE.           
               88 WS-MODE-CODE-VALID     VALUE ' ' 'M' 'W' 'S' 'B' 'T'. 
                                                                        
           12  WS-SKIP-CODE            PIC X     VALUE SPACE.           
               88 WS-SKIP-CODE-VALID     VALUE ' ' 'A' 'X' '0' THRU '9'.
                                                                        
           12  WS-KIND                 PIC XX    VALUE SPACE.           
               88 WS-KIND-LF             VALUE 'LF'.                    
               88 WS-KIND-AH             VALUE 'AH'.                    
               88 WS-KIND-PR             VALUE 'PR'.                    
               88 WS-KIND-UE             VALUE 'UE'.                    
               88 WS-KIND-DI             VALUE 'DI'.                    
               88 WS-KIND-MONTHLY        VALUE 'AH' 'UE'.               
                                                                        
           12  WS-JOURNAL-RECORD-LENGTH   PIC S9(4) COMP VALUE +0000.   
                                                                        
           12  WS-EDIT-CODE               PIC X(4)  VALUE SPACES.       
                                                                        
           12  WS-SAVE-INPUT-FIELDS.                                    
               16  WS-BAGE                 PIC 99       VALUE ZERO.     
               16  WS-BJNT-AGE             PIC 99       VALUE ZERO.     
               16  WS-BDAYS                PIC 999      VALUE ZERO.     
               16  WS-BLN-TERM             PIC 999      VALUE ZERO.     
               16  WS-BFREQ                PIC 99       VALUE ZERO.     
               16  WS-BPHONE               PIC 9(12)    VALUE  0 COMP-3.
               16  WS-BAPR                 PIC 99V9(4) VALUE ZEROS.
               16  FILLER REDEFINES WS-BAPR.
                   20  WS-APR-WHOLE-NUM    PIC 99.
                   20  WS-APR-DEC          PIC 9999.
               16  WS-BPMT                 PIC S9(6)V99 VALUE +0 COMP-3.
               16  WS-BPMTS                PIC S999     VALUE +0 COMP-3.
               16  WS-BLIVES               PIC 9(7)      COMP-3.        
                                                                        
               16  WS-B-COVERAGE.
                   20  WS-BTERM1            PIC 999       COMP-3.        
                   20  WS-BBEN1             PIC S9(10)V99 COMP-3.        
                   20  WS-BALT-BEN1     PIC S9(10)V99 COMP-3.            
                   20  WS-BPREM1        PIC S9(10)V99 COMP-3.            
                   20  WS-BALT-PREM1    PIC S9(7)V99 COMP-3.             
                   20  WS-BTERM2            PIC 999       COMP-3.        
                   20  WS-BCRIT-PERD2       PIC 99        COMP-3.        
                   20  WS-BBEN2             PIC S9(10)V99 COMP-3.        
                   20  WS-BPREM2        PIC S9(10)V99 COMP-3.            
                   20  WS-BALT-BEN2     PIC S9(10)V99 COMP-3.            
                   20  WS-BALT-PREM2    PIC S9(7)V99 COMP-3.             
                                                                        
               16  WS-C-FIELDS   OCCURS 4 TIMES.                        
                   20  WS-CLIVES      PIC 9(3)          COMP-3.         
                   20  WS-CREFUND1    PIC S9(7)V99      COMP-3.         
                   20  WS-CREFUND2    PIC S9(7)V99      COMP-3.         
                   20  WS-CAN-REA     PIC X.
                                                                        
           12  WS-CONVERTED-BIRTH      OCCURS 2 PIC XX.
           12  WS-CONVERTED-EFFDT      PIC XX    VALUE SPACES.          
           12  WS-CONVERTED-1ST-PMT-DT PIC XX    VALUE SPACES.          
           12  WS-CONVERTED-EXPIRDT      OCCURS 2 TIMES PIC XX.         
           12  WS-CONVERTED-CANCEL-DATES OCCURS 4 TIMES.                
               16  WS-CONVERTED-CANDT1 PIC XX.                          
               16  WS-CONVERTED-CANDT2 PIC XX.                          
           12  WS-CONVERTED-CAN-EFF-DATES OCCURS 4 TIMES.               
               16  WS-CONVERTED-CAN-EFF-DT PIC XX.                      
                                                                        
           12  WS-FIRST-NAME.                                           
               16  WS-1ST-INIT         PIC X.                           
               16  FILLER              PIC X(9).                        
                                                                        
           12  WS-INITIALS.                                             
               16  WS-INITIAL-1        PIC X.                           
               16  WS-INITIAL-2        PIC X.                           
                                                                        
           12  WS-ZIP-CODE.                                             
               16  WS-ZIP-1            PIC X.                           
                   88  WS-CANADIAN-ZIP    VALUE 'A' THRU 'Z'.           
               16  WS-ZIP-2-3          PIC XX.                          
               16  WS-ZIP-4            PIC X.                           
               16  WS-ZIP-5            PIC X.                           
               16  WS-ZIP-6            PIC X.                           
               16  FILLER              PIC X(4).                        
           12  WS-ZIP-AM-1  REDEFINES  WS-ZIP-CODE.                     
               16  WS-ZIP-AM-1-CODE    PIC X(5).                        
               16  WS-ZIP-AM-1-PLUS4   PIC X(4).                        
               16  FILLER              PIC X.                           
           12  WS-ZIP-AM-2  REDEFINES  WS-ZIP-CODE.                     
               16  WS-ZIP-AM-2-CODE    PIC X(5).                        
               16  WS-ZIP-AM-2-DASH    PIC X.                           
               16  WS-ZIP-AM-2-PLUS4   PIC X(4).                        
           12  WS-ZIP-CAN-1  REDEFINES  WS-ZIP-CODE.                    
               16  WS-ZIP-CAN-1-POST1  PIC XXX.                         
               16  WS-ZIP-CAN-1-POST2  PIC XXX.                         
               16  FILLER              PIC X(4).                        
           12  WS-ZIP-CAN-2  REDEFINES  WS-ZIP-CODE.                    
               16  WS-ZIP-CAN-2-POST1  PIC XXX.                         
               16  FILLER              PIC X.                           
               16  WS-ZIP-CAN-2-POST2  PIC XXX.                         
               16  FILLER              PIC XXX.                         
           12  WS-MEMBER-NO            PIC X(12).                       
           12  FILLER  REDEFINES  WS-MEMBER-NO.                         
               16  WS-MEMBER-NO-1-8    PIC 9(8).                        
               16  FILLER              PIC X(4).                        
           12  WS-I-MICRO-NO           PIC S9(9)        COMP-3.         
                                                                        
           EJECT                                                        
                                                                        
       01  WS-DATE-AREA.                                                
           12  WS-COMPARE-CURRENT-DT.                                   
               16  FILLER              PIC X(4)    VALUE SPACES.        
               16  WS-COMPARE-CURR-YR  PIC X(2)    VALUE SPACES.        
           12  WS-SAVE-BIRTH-DATE.                                      
               16  FILLER              PIC X(4)   VALUE SPACES.         
               16  WS-SAVE-BIRTH-YR    PIC X(2)   VALUE SPACES.         
                                                                        
       01  CLASIC-WARNING.                                              
           12  WARNING-LENGTH              PIC S9(4)  VALUE +124 COMP.  
           12  WARNING-TEXT.                                            
               16  FILLER                  PIC X(80)  VALUE             
                   'THIS DATA MAY HAVE PREVIOUSLY BEEN PROCESSED'.      
               16  FILLER                  PIC X(44)  VALUE             
                   'CONTACT PAUL @ CSO FOR FURTHER INFORMATION'.        
                                                                        
                                       COPY ELCDATE.
                                       COPY ELCLOGOF.
                                       COPY ELCATTR.
                                       COPY ELCEMIB.
                                       COPY ELCINTF.
                                       COPY ELC630PI.
                                       COPY ELCJPFX.

                                       PIC X(608).
                                                                        
                                       COPY ELCAID.
                                                                        
       01  FILLER    REDEFINES DFHAID.                                  
           12  FILLER              PIC X(8).                            
           12  PF-VALUES           PIC X       OCCURS 2.                
                                                                        
                                       COPY VP6301S.

       01  MAP-B REDEFINES VP630BI.                                     
           12  FILLER                      PIC X(42).                   
           12  DATA-AREA-B.                                             
               16  BSEQ-LEN                PIC S9(4)  COMP.             
               16  BSEQ-ATTRB              PIC X.                       
               16  BSEQ                    PIC 9(4).                    
               16  BMO-END-LEN             PIC S9(4)  COMP.             
               16  BMO-END-ATTRB           PIC X.                       
               16  BMO-END                 PIC X(8).                    
               16  BACCT-NM-LEN            PIC S9(4)  COMP.             
               16  BACCT-NM-ATTRB          PIC X.                       
               16  BACCT-NM                PIC X(30).                   
               16  BEFFDT-LEN              PIC S9(4)  COMP.             
               16  BEFFDT-ATTRB            PIC X.                       
               16  BEFFDT                  PIC X(6).                    
               16  BCERT-LEN               PIC S9(4)  COMP.             
               16  BCERT-ATTRB             PIC X.                       
               16  BCERT                   PIC X(10).                   
               16  BSFX-LEN                PIC S9(4)  COMP.             
               16  BSFX-ATTRB              PIC X.                       
               16  BSFX                    PIC X.                       
               16  BLN-OFFICER-LEN         PIC S9(4)  COMP.             
               16  BLN-OFFICER-ATTRB       PIC X.                       
               16  BLN-OFFICER             PIC X(5).                    
               16  B1ST-NAME-LEN           PIC S9(4)  COMP.             
               16  B1ST-NAME-ATTRB         PIC X.                       
               16  B1ST-NAME               PIC X(10).                   
               16  BINIT-LEN               PIC S9(4)  COMP.             
               16  BINIT-ATTRB             PIC X.                       
               16  BINIT                   PIC X.                       
               16  BLAST-NAME-LEN          PIC S9(4)  COMP.             
               16  BLAST-NAME-ATTRB        PIC X.                       
               16  BLAST-NAME              PIC X(15).                   
               16  BAGE-LEN                PIC S9(4)  COMP.             
               16  BAGE-ATTRB              PIC X.                       
               16  BAGE                    PIC 99.                      
               16  BSEX-LEN                PIC S9(4)  COMP.             
               16  BSEX-ATTRB              PIC X.                       
               16  BSEX                    PIC X.                       
               16  BJNT-1ST-NAME-LEN       PIC S9(4)   COMP.            
               16  BJNT-1ST-NAME-ATTRB     PIC X.                       
               16  BJNT-1ST-NAME           PIC X(10).                   
               16  BJNT-INIT-LEN           PIC S9(4)   COMP.            
               16  BJNT-INIT-ATTRB         PIC X.                       
               16  BJNT-INIT               PIC X.                       
               16  BJNT-LST-NAME-LEN       PIC S9(4)   COMP.            
               16  BJNT-LST-NAME-ATTRB     PIC X.                       
               16  BJNT-LST-NAME           PIC X(15).                   
               16  BJNT-AGE-LEN            PIC S9(4)   COMP.            
               16  BJNT-AGE-ATTRB          PIC X.                       
               16  BJNT-AGE                PIC 99.                      
               16  BADDRS1-LEN             PIC S9(4)  COMP.             
               16  BADDRS1-ATTRB           PIC X.                       
               16  BADDRS1                 PIC X(30).                   
               16  BADDRS2-LEN             PIC S9(4)  COMP.             
               16  BADDRS2-ATTRB           PIC X.                       
               16  BADDRS2                 PIC X(30).                   
               16  BCITY-LEN               PIC S9(4)  COMP.             
               16  BCITY-ATTRB             PIC X.                       
               16  BCITY                   PIC X(28).                   
               16  BSTATE-LEN              PIC S9(4)  COMP.             
               16  BSTATE-ATTRB            PIC X.                       
               16  BSTATE                  PIC XX.
               16  BZIPCDE-LEN             PIC S9(4)  COMP.             
               16  BZIPCDE-ATTRB           PIC X.                       
               16  BZIPCDE                 PIC X(10).                   
      
               16  BYEAR-LEN               PIC S9(4)  COMP.             
               16  BYEAR-ATTRB             PIC X.                       
               16  BYEAR                   PIC X(4).
               16  BMAKE-LEN               PIC S9(4)  COMP.             
               16  BMAKE-ATTRB             PIC X.                       
               16  BMAKE                   PIC X(20).
               16  BMODEL-LEN              PIC S9(4)  COMP.             
               16  BMODEL-ATTRB            PIC X.                       
               16  BMODEL                  PIC X(20).
               16  BFUTURE-LEN             PIC S9(4)  COMP.             
               16  BFUTURE-ATTRB           PIC X.                       
               16  BFUTURE                 PIC X(20).
               16  BVINHD-LEN              PIC S9(4)  COMP.
               16  BVINHD-ATTRB            PIC X.
               16  BVINNDI                 PIC X(5).
               16  BVIN-LEN                PIC S9(4)  COMP.
               16  BVIN-ATTRB              PIC X.
               16  BVIN-NOI                PIC X(17).
      
               16  BOMETER-LEN             PIC S9(4)  COMP.
               16  BOMETER-ATTRB           PIC X.
               16  BOMETER-in              PIC x(7).
               16  BOMETER-out REDEFINES BOMETER-in
                                           PIC 999,999.
               16  BBENEFICIARY-LEN        PIC S9(4)   COMP.            
               16  BBENEFICIARY-ATTRB      PIC X.                       
               16  BBENEFICIARY            PIC X(25).                   
               16  BCADDR1-LEN             PIC S9(4)  COMP.             
               16  BCADDR1-ATTRB           PIC X.                       
               16  BCADDR1                 PIC X(30).                   
               16  BCADDR2-LEN             PIC S9(4)  COMP.             
               16  BCADDR2-ATTRB           PIC X.                       
               16  BCADDR2                 PIC X(30).                   
               16  BCCITY-LEN              PIC S9(4)  COMP.             
               16  BCCITY-ATTRB            PIC X.                       
               16  BCCITY                  PIC X(28).                   
               16  BCSTATE-LEN             PIC S9(4)  COMP.             
               16  BCSTATE-ATTRB           PIC X.                       
               16  BCSTATE                 PIC XX.
               16  BCZIPCD-LEN             PIC S9(4)  COMP.             
               16  BCZIPCD-ATTRB           PIC X.                       
               16  BCZIPCD                 PIC X(10).                   
      
               16  BKIND1-LEN           PIC S9(4)  COMP.             
               16  BKIND1-ATTRB         PIC X.                       
               16  BKIND1               PIC XX.                      
               16  BTYPE1-LEN           PIC S9(4)  COMP.             
               16  BTYPE1-ATTRB         PIC X.                       
               16  BTYPE1               PIC X(3).                    
               16  BTERM1-LEN           PIC S9(4)  COMP.             
               16  BTERM1-ATTRB         PIC X.                       
               16  BTERM1I              PIC 999.                     
               16  BTERM1O REDEFINES                                 
                              BTERM1I   PIC ZZZ.                     
               16  BBENE1-LEN            PIC S9(4)  COMP.             
               16  BBENE1-ATTRB          PIC X.                       
               16  BBENE1I               PIC 9(10)V99.                
      *        16  BBENE1I               PIC 9(12).                   
               16  BBENE1O REDEFINES                                  
                                 BBENE1I PIC Z(9).99.                 
               16  BPREM1-LEN           PIC S9(4)  COMP.             
               16  BPREM1-ATTRB         PIC X.                       
               16  BPREM1I              PIC 9(9)V99.                 
      *        16  BPREM1I              PIC 9(11).                   
               16  BPREM1O REDEFINES                                 
                                BPREM1I PIC Z(7).99-.                
               16  BALT-BEN1-LEN        PIC S9(4)  COMP.             
               16  BALT-BEN1-ATTRB      PIC X.                       
               16  BALT-BEN1I           PIC 9(10)V99.                
      *        16  BALT-BEN1I           PIC 9(12).                   
               16  BALT-BEN1O REDEFINES                              
                                 BALT-BEN1I PIC Z(9).ZZ.             
               16  BALT-PREM1-LEN       PIC S9(4)  COMP.             
               16  BALT-PREM1-ATTRB     PIC X.                       
               16  BALT-PREM1I          PIC 9(7)V99.                 
      *        16  BALT-PREM1I          PIC 9(9).                    
               16  BALT-PREM1O REDEFINES                             
                                 BALT-PREM1I PIC Z(6).ZZ.            
      
               16  BKIND2-LEN           PIC S9(4)  COMP.             
               16  BKIND2-ATTRB         PIC X.                       
               16  BKIND2               PIC XX.                      
               16  BTYPE2-LEN           PIC S9(4)  COMP.             
               16  BTYPE2-ATTRB         PIC X.                       
               16  BTYPE2               PIC X(3).                    
               16  BTERM2-LEN           PIC S9(4)  COMP.             
               16  BTERM2-ATTRB         PIC X.                       
               16  BTERM2I              PIC 999.                     
               16  BTERM2O REDEFINES                                 
                              BTERM2I   PIC ZZZ.                     
               16  BBENE2-LEN            PIC S9(4)  COMP.             
               16  BBENE2-ATTRB          PIC X.                       
               16  BBENE2I               PIC 9(10)V99.                
      *        16  BBENE2I               PIC 9(12).                   
               16  BBENE2O REDEFINES                                  
                                 BBENE2I PIC Z(9).99.                 
               16  BPREM2-LEN           PIC S9(4)  COMP.             
               16  BPREM2-ATTRB         PIC X.                       
               16  BPREM2I              PIC 9(9)V99.                 
      *        16  BPREM2I              PIC 9(11).                   
               16  BPREM2O REDEFINES                                 
                                BPREM2I PIC Z(7).99-.                
               16  BCRIT-PERD2-LEN      PIC S9(4)  COMP.             
               16  BCRIT-PERD2-ATTRB    PIC X.                       
               16  BCRIT-PERD2I         PIC 99.                      
               16  BCRIT-PERD2O REDEFINES                            
                       BCRIT-PERD2I PIC ZZ.                      
               16  BALT-BEN2-LEN        PIC S9(4)  COMP.             
               16  BALT-BEN2-ATTRB      PIC X.                       
               16  BALT-BEN2I           PIC 9(10)V99.                
      *        16  BALT-BEN2I           PIC 9(12).                   
               16  BALT-BEN2O REDEFINES                              
                                 BALT-BEN2I PIC Z(9).ZZ.             
               16  BALT-PREM2-LEN       PIC S9(4)  COMP.             
               16  BALT-PREM2-ATTRB     PIC X.                       
               16  BALT-PREM2I          PIC 9(7)V99.                 
      *        16  BALT-PREM2I          PIC 9(9).                    
               16  BALT-PREM2O REDEFINES                             
                                 BALT-PREM2I PIC Z(6).ZZ.            
                                                                        
       01  MAP-C REDEFINES VP630BI.                                     
           12  FILLER                  PIC X(86).                       
           12  DATA-AREA-C             OCCURS 4 TIMES.                  
               16  CSEQ-LEN                PIC S9(4)  COMP.             
               16  CSEQ-ATTRB              PIC X.                       
               16  CSEQ                    PIC 9(4).                    
               16  CCERT-LEN               PIC S9(4)  COMP.             
               16  CCERT-ATTRB             PIC X.                       
               16  CCERT                   PIC X(10).                   
               16  CSFX-LEN                PIC S9(4)  COMP.             
               16  CSFX-ATTRB              PIC X.                       
               16  CSFX                    PIC X.                       
               16  CEFFDT-LEN              PIC S9(4)  COMP.             
               16  CEFFDT-ATTRB            PIC X.                       
               16  CEFFDT                  PIC 9(6).                    
               16  CLAST-NAME-LEN          PIC S9(4)  COMP.             
               16  CLAST-NAME-ATTRB        PIC X.                       
               16  CLAST-NAME              PIC X(15).                   
               16  CANCEL-INFO.                                         
                   20  CKIND1-LEN          PIC S9(4)  COMP.             
                   20  CKIND1-ATTRB        PIC X.                       
                   20  CKIND1              PIC XX.                      
                   20  CCANDT1-LEN         PIC S9(4)  COMP.             
                   20  CCANDT1-ATTRB       PIC X.                       
                   20  CCANDT1             PIC 9(6).                    
                   20  CREFUND1-LEN        PIC S9(4)  COMP.             
                   20  CREFUND1-ATTRB      PIC X.                       
                   20  CREFUND1I           PIC S9(9)V99.                
      *            20  CREFUND1I           PIC X(11).                   
                   20  CREFUND1O REDEFINES                              
                                 CREFUND1I PIC Z(7).99-.                
                   20  CMTHD1-LEN          PIC S9(4)  COMP.             
                   20  CMTHD1-ATTRB        PIC X.                       
                   20  CMTHD1              PIC X.                       
                   20  CKIND2-LEN          PIC S9(4)  COMP.             
                   20  CKIND2-ATTRB        PIC X.                       
                   20  CKIND2              PIC XX.                      
                   20  CCANDT2-LEN         PIC S9(4)  COMP.             
                   20  CCANDT2-ATTRB       PIC X.                       
                   20  CCANDT2             PIC 9(6).                    
                   20  CREFUND2-LEN        PIC S9(4)  COMP.             
                   20  CREFUND2-ATTRB      PIC X.                       
                   20  CREFUND2I           PIC S9(9)V99.                
      *            20  CREFUND2I           PIC X(11).                   
                   20  CREFUND2O REDEFINES                              
                                 CREFUND2I PIC Z(7).99-.                
                   20  CMTHD2-LEN          PIC S9(4)  COMP.             
                   20  CMTHD2-ATTRB        PIC X.                       
                   20  CMTHD2              PIC X.                       
                   20  CCHK-LEN            PIC S9(4)  COMP.             
                   20  CCHK-ATTRB          PIC X.                       
                   20  CCHK                PIC X.                       
                   20  CPAYEE-LEN          PIC S9(4)  COMP.             
                   20  CPAYEE-ATTRB        PIC X.                       
                   20  CPAYEE              PIC X(6).                    
                   20  CLIVES-LEN          PIC S9(4)  COMP.             
                   20  CLIVES-ATTRB        PIC X.                       
                   20  CLIVESI             PIC 999.                     
                   20  CLIVESO REDEFINES                                
                                CLIVESI    PIC ZZZ.                     
                   20  CCANREA-LEN         PIC S9(4)  COMP.             
                   20  CCANREA-ATTRB       PIC X.                       
                   20  CCANREA             PIC X.
           EJECT                                                        
       LINKAGE SECTION.                                                 
       01  DFHCOMMAREA             PIC X(1900).                         
                                                                        
           EJECT                                                        
      *01 PARMLIST .                                                    
      *    02  FILLER              PIC S9(8)   COMP.                    
      *    02  ERPNDB-POINTER      PIC S9(8)   COMP.                    
      *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                    
      *    02  ELCERT-POINTER      PIC S9(8)   COMP.                    
      *    02  ERPNDM-POINTER      PIC S9(8)   COMP.                    
                                                                        
                                       COPY ERCPNDB.
                                       COPY ELCCNTL.
                                       COPY ELCCERT.
                                       COPY ELCCRTT.
                                       COPY ERCPNDM.
                                                                        
       PROCEDURE DIVISION.                                              
                                                                        
           MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      
           MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.            
                                                                        
           MOVE +2                     TO EMI-NUMBER-OF-LINES.          
                                                                        
           IF EIBCALEN = 0                                              
               GO TO 8800-UNAUTHORIZED-ACCESS.                          
                                                                        
           MOVE EIBDATE                TO DC-JULIAN-YYDDD.              
           MOVE '5'                    TO DC-OPTION-CODE.               
           PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
           MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.            
           MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.                
           MOVE DC-GREG-DATE-1-MDY     TO WS-COMPARE-CURRENT-DT.        
                                                                        
           IF PI-CALLING-PROGRAM NOT = THIS-PGM                         
               IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   
                   MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      
                   MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      
                   MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      
                   MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      
                   MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      
                   MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      
                   MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    
                   MOVE THIS-PGM             TO PI-CALLING-PROGRAM      
               ELSE                                                     
                   MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      
                   MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    
                   MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      
                   MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      
                   MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      
                   MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      
                   MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      
                   MOVE SPACES               TO PI-SAVED-PROGRAM-6.     
                                                                        
           MOVE LOW-VALUES             TO VP630BI.                      
                                                                        
           IF EIBTRNID NOT = TRANS-EXA6                                 
               MOVE ZEROS              TO PI-LF-ISS-ENTERED             
                                          PI-LF-CAN-ENTERED             
                                          PI-AH-ISS-ENTERED             
                                          PI-AH-CAN-ENTERED             
                                          PI-ISS-CNT-ENTERED            
                                          PI-CAN-CNT-ENTERED            
               IF PI-MAINT-FUNC = 'N'                                   
                  MOVE +0              TO PI-LAST-SEQ-NO-ADDED          
                  MOVE +1              TO PI-NEXT-DISPLAY-SEQ-NO        
                  IF PI-MAP-NAME = VP630B                               
                     PERFORM 8550-SET-MAP-SEQ-NOS                       
                     GO TO 8100-SEND-INITIAL-MAP                        
                  ELSE                                                  
                     PERFORM 8550-SET-MAP-SEQ-NOS                       
                             VARYING WS-SUB2 FROM 1 BY 1                
                             UNTIL WS-SUB2   GREATER 4                  
                       GO TO 8100-SEND-INITIAL-MAP                      
               ELSE                                                     
                   GO TO 3000-CONTINUE-ENTRY.                           
                                                                        
           EXEC CICS HANDLE CONDITION                                   
               PGMIDERR  (9600-PGMID-ERROR)                             
               ERROR     (9990-ABEND)                                   
           END-EXEC.                                                    
                                                                        
           IF EIBAID = DFHCLEAR                                         
               MOVE SPACE TO PI-DISPLAY-SW                              
                             PI-BROWSE-SW                               
               GO TO 9400-CLEAR.                                        
                                                                        
           EJECT                                                        
                                                                        
       0200-RECEIVE.                                                    
           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       
               MOVE ER-0008            TO EMI-ERROR                     
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
               IF PI-MAP-NAME = VP630B                                  
                   MOVE -1             TO BPFENTRL                      
                   GO TO 8200-SEND-DATAONLY                             
               ELSE                                                     
                   MOVE -1             TO CPFENTRL                      
                   GO TO 8200-SEND-DATAONLY.                            
                                                                        
           EXEC CICS RECEIVE                                            
               MAP      (PI-MAP-NAME)                                   
               MAPSET   (MAPSET-VP6301S)                                
               INTO     (VP630BI)                                       
           END-EXEC.                                                    
                                                                        
           INSPECT VP630BI CONVERTING '_' TO ' '.                       
                                                                        
           IF PI-MAP-NAME = VP630B                                      
               IF BPFENTRL GREATER ZERO                                 
                   IF EIBAID NOT = DFHENTER                             
                       MOVE ER-0004    TO EMI-ERROR                     
                       MOVE AL-UNBOF   TO BPFENTRA                      
                       MOVE -1         TO BPFENTRL                      
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
                       GO TO 8200-SEND-DATAONLY                         
                   ELSE                                                 
                       IF BPFENTRI NUMERIC AND                          
                          BPFENTRI GREATER 0 AND LESS 23                
                           MOVE PF-VALUES (BPFENTRI) TO EIBAID          
                       ELSE                                             
                           MOVE ER-0029  TO EMI-ERROR                   
                           MOVE AL-UNBOF TO BPFENTRA                    
                           MOVE -1       TO BPFENTRL                    
                           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     
                           GO TO 8200-SEND-DATAONLY                     
               ELSE                                                     
                   NEXT SENTENCE                                        
           ELSE                                                         
           IF PI-MAP-NAME = VP630C                                      
               IF CPFENTRL GREATER ZERO                                 
                   IF EIBAID NOT = DFHENTER                             
                       MOVE ER-0004    TO EMI-ERROR                     
                       MOVE AL-UNBOF   TO CPFENTRA                      
                       MOVE -1         TO CPFENTRL                      
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
                       GO TO 8200-SEND-DATAONLY                         
                   ELSE                                                 
                       IF CPFENTRI NUMERIC AND                          
                          CPFENTRI GREATER 0 AND LESS 23                
                           MOVE PF-VALUES (CPFENTRI) TO EIBAID          
                       ELSE                                             
                           MOVE ER-0029  TO EMI-ERROR                   
                           MOVE AL-UNBOF TO BPFENTRA                    
                           MOVE -1       TO BPFENTRL                    
                           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     
                           GO TO 8200-SEND-DATAONLY.                    
                                                                        
           EJECT                                                        
      ******************************************************************
      *   PF KEY FUNCTIONS:                                            *
      *                                                                *
      *   PF1 = BROWSE FOWARD                                          *
      *   PF2 = BROWSE BACKWARD                                        *
      *   PF3 = ADD ISSUE RECORD                                       *
      *   PF4 = ADD CANCEL RECORD                                      *
      *   PF5 = RESET TABS (OPEN PROTECTED FIELDS)                     *
      *   PF6 = DELETE ENTRY                                           *
      ******************************************************************
                                                                        
       0300-CHECK-PFKEYS.                                               
           IF EIBAID = DFHPF12                                          
               GO TO 9500-PF12.                                         
                                                                        
           IF EIBAID NOT = DFHPF5                                       
              MOVE SPACE               TO PI-BROWSE-SW.                 
                                                                        
           IF EIBAID = DFHENTER                                         
               GO TO 1000-EDIT-MAPB.                                    
                                                                        
           IF EIBAID = DFHPF1                                           
               MOVE 'Y'                TO PI-BROWSE-SW                  
               GO TO 2000-BROWSE-FWD.                                   
                                                                        
           IF EIBAID = DFHPF2                                           
               MOVE 'Y'                TO PI-BROWSE-SW                  
               GO TO 2100-BROWSE-BKWD.                                  
                                                                        
           IF EIBAID = DFHPF3                                           
               MOVE SPACE              TO PI-DISPLAY-SW                 
               MOVE LOW-VALUES         TO VP630BI                       
               ADD +1                     PI-LAST-SEQ-NO-ADDED          
                     GIVING PI-NEXT-DISPLAY-SEQ-NO                      
               MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ          
               MOVE VP630B             TO PI-MAP-NAME                   
               PERFORM 8550-SET-MAP-SEQ-NOS                             
               GO TO 8100-SEND-INITIAL-MAP.                             
                                                                        
           IF EIBAID = DFHPF4                                           
               MOVE SPACE              TO PI-DISPLAY-SW                 
               MOVE LOW-VALUES         TO MAP-C                         
               ADD +1                     PI-LAST-SEQ-NO-ADDED          
                     GIVING PI-NEXT-DISPLAY-SEQ-NO                      
               MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ          
               MOVE VP630C             TO PI-MAP-NAME                   
               PERFORM 8550-SET-MAP-SEQ-NOS                             
                      VARYING WS-SUB2 FROM 1 BY 1                       
                      UNTIL WS-SUB2 GREATER +4                          
               GO TO 8100-SEND-INITIAL-MAP.                             
                                                                        
           IF EIBAID = DFHPF5                                           
              IF PI-BROWSE                                              
                 IF PI-MAP-NAME = VP630B                                
                    MOVE -1          TO BEFFDTL
                    GO TO 8200-SEND-DATAONLY                            
                 ELSE                                                   
                    MOVE -1          TO CCERT-LEN (1)                   
                    GO TO 8200-SEND-DATAONLY.                           
                                                                        
           IF EIBAID = DFHPF5                                           
              IF PI-MAP-NAME = VP630B                                   
                 PERFORM 0610-UNPROTECT-FIELDS THRU 0610-EXIT           
                 MOVE -1             TO BEFFDTL
                 GO TO 8200-SEND-DATAONLY                               
              ELSE                                                      
                 PERFORM 0710-UNPROTECT-FIELDS THRU 0710-EXIT           
                 ADD +1    PI-LAST-SEQ-NO-ADDED                         
                        GIVING PI-NEXT-DISPLAY-SEQ-NO                   
                MOVE -1             TO CCERT-LEN  (1)                   
                GO TO 8200-SEND-DATAONLY.                               
                                                                        
           IF EIBAID = DFHPF6                                           
               IF PI-LAST-FUNC-DISPLAY                                  
                   GO TO 6000-DELETE-PEND-BUS-RECORD                    
               ELSE                                                     
                   MOVE ER-2594        TO EMI-ERROR                     
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
                   IF PI-MAP-NAME = VP630B                              
                       MOVE -1         TO BPFENTRL                      
                       GO TO 8200-SEND-DATAONLY                         
                   ELSE                                                 
                       MOVE -1         TO CPFENTRL                      
                       GO TO 8200-SEND-DATAONLY.                        
                                                                        
           MOVE ER-0008 TO EMI-ERROR.                                   
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
                                                                        
           IF PI-MAP-NAME = VP630B                                      
               MOVE -1                 TO BPFENTRL                      
           ELSE                                                         
               MOVE -1                 TO CPFENTRL.                     
                                                                        
           GO TO 8200-SEND-DATAONLY.                                    
                                                                        
           EJECT                                                        
       0600-PROTECT-FIELDS.                                             
           IF PI-COMPANY-ID = 'MON'                                     
               MOVE AL-UANOF         TO BSFX-ATTRB                      
           ELSE                                                         
           IF PI-COMPANY-ID = 'PEM' OR                                  
                              'CGL' OR                                  
                              'TIH' OR                                  
                              'TII' OR                                  
                              'FGL' OR                                  
                              'OFL'                                     
              NEXT SENTENCE                                             
           ELSE                                                         
              IF NOT PI-ISS-SUFFIX-KEYED                                
                 MOVE AL-SANOF         TO BSFX-ATTRB.                   
                                                                        
           IF PI-PROCESSOR-ID = 'LGXX'                                  
              IF NOT PI-ISS-SUFFIX-KEYED                                
                 MOVE AL-SANOF         TO BSFX-ATTRB.                   

           IF PI-COMPANY-ID = 'DCC' or 'VPP' or 'CID'
              CONTINUE
           ELSE
              IF NOT PI-VIN-KEYED
                 MOVE AL-SANOF         TO BVIN-ATTRB
              END-IF
           END-IF

           IF PI-COMPANY-ID = 'CID' OR 'DCC' or 'AHL' or 'VPP'
              CONTINUE
           ELSE
              IF NOT PI-LN-OFFICER-KEYED                                   
                 MOVE AL-SANOF         TO BLN-OFFICER-ATTRB
              END-IF
           END-IF

           IF PI-COMPANY-ID = 'CRI' OR 'LGX'                            
               IF PI-BIRTHDT-KEYED  AND  NOT PI-AGE-KEYED               
                   MOVE AL-SANOF       TO BAGE-ATTRB.                   
                                                                        
           IF PI-COMPANY-ID = 'PEM' OR 'CGL' OR 'TIH' OR 'TII' OR
              'FGL' OR 'OFL'
              CONTINUE
           ELSE                                                         
              IF NOT PI-JNT-AGE-KEYED                                   
                 MOVE AL-SANOF         TO BJNT-AGE-ATTRB
              END-IF
           END-IF
      
           IF PI-COMPANY-ID = 'PEM' OR                                  
                              'CGL' OR                                  
                              'TIH' OR                                  
                              'TII' OR                                  
                              'FGL' OR                                  
                              'OFL'                                     
              NEXT SENTENCE                                             
           ELSE                                                         
              IF NOT PI-JNT-NAME-KEYED                                  
                 MOVE AL-SANOF           TO BJNT-INIT-ATTRB             
                                            BJNT-LST-NAME-ATTRB         
                                            BJNT-1ST-NAME-ATTRB.        
                                                                        
           IF PI-COMPANY-ID = 'TMS' OR 'CID' OR 'DCC' or 'AHL' or 'VPP'
              NEXT SENTENCE                                             
           ELSE                                                         
              IF NOT PI-BENEFICIARY-KEYED                               
                  MOVE AL-SANOF           TO BBENEFICIARY-ATTRB.        
                                                                        
           IF NOT PI-ALT-BEN-KEYED                                      
               MOVE AL-SANOF           TO BALT-BEN1-ATTRB
           END-IF
                                                                        
           IF NOT PI-ALT-PREM-KEYED                                     
               MOVE AL-SANOF           TO BALT-PREM1-ATTRB
           END-IF
      
           IF NOT PI-ALT-BEN-KEYED
               MOVE AL-SANOF           TO BALT-BEN2-ATTRB
           END-IF
      
           IF NOT PI-ALT-PREM-KEYED
               MOVE AL-SANOF           TO BALT-PREM2-ATTRB
           END-IF
      
           IF PI-PROCESSOR-ID = 'LGXX'                                  
              CONTINUE                                                  
           ELSE                                                         
              IF PI-COMPANY-ID = 'PEM' OR                               
                                 'CGL' OR                               
                                 'TIH' OR                               
                                 'TII' OR                               
                                 'FGL' OR                               
                                 'OFL'                                  
                 MOVE AL-SANOF            TO BADDRS2-ATTRB              
      *                                      BPHONE-ATTRB               
              ELSE
                 IF PI-COMPANY-ID ='CID' or 'AHL'
                    MOVE AL-SANOF         TO BADDRS2-ATTRB
                 END-IF
              END-IF
           END-IF
       
           IF PI-COMPANY-ID = 'TMS'                                     
      *        MOVE AL-UANOF              TO BSIG-ATTRB                 
               MOVE AL-UANOF              TO BJNT-AGE-ATTRB             
                                             BJNT-LST-NAME-ATTRB        
                                             BJNT-1ST-NAME-ATTRB        
                                             BJNT-INIT-ATTRB.
                                                                        
       0600-EXIT.                                                       
           EXIT.                                                        
                                                                        
           EJECT                                                        
       0610-UNPROTECT-FIELDS.                                           
           IF PI-COMPANY-ID = 'PEM' OR                                  
                              'CGL' OR                                  
                              'TIH' OR                                  
                              'TII' OR                                  
                              'FGL' OR                                  
                              'OFL'                                     
              NEXT SENTENCE                                             
           ELSE                                                         
              IF NOT PI-ISS-SUFFIX-KEYED                                
                 MOVE AL-UANOF         TO BSFX-ATTRB.                   
                                                                        
           IF PI-PROCESSOR-ID = 'LGXX'                                  
              IF NOT PI-ISS-SUFFIX-KEYED                                
                 MOVE AL-UANOF         TO BSFX-ATTRB.                   
                                                                        
           IF PI-COMPANY-ID = 'CID' OR 'DCC' or 'AHL' or 'VPP'
              CONTINUE
           ELSE
              IF NOT PI-LN-OFFICER-KEYED                                   
                 MOVE AL-UANOF         TO BLN-OFFICER-ATTRB
              END-IF
           END-IF
                                                                        
                                                                        
                                                                        
           IF PI-COMPANY-ID = 'FLA'                                     
              IF BAGE-LEN NOT GREATER THAN +0                            
                 MOVE AL-UANOF       TO BAGE-ATTRB.                     
                                                                        
           IF PI-COMPANY-ID = 'CRI' OR                                  
                              'LGX'                                     
              IF NOT PI-AGE-KEYED                                       
                 IF BAGE-LEN NOT GREATER THAN +0                        
                    MOVE AL-UANOF     TO BAGE-ATTRB.                    
                                                                        
           IF PI-COMPANY-ID = 'PEM' OR                                  
                              'CGL' OR                                  
                              'TIH' OR                                  
                              'TII' OR                                  
                              'FGL' OR                                  
                              'OFL'                                     
              NEXT SENTENCE                                             
           ELSE                                                         
              IF NOT PI-JNT-AGE-KEYED                                   
                 MOVE AL-UANOF           TO BJNT-AGE-ATTRB.             
                                                                        
           IF PI-COMPANY-ID = 'PEM' OR                                  
                              'CGL' OR                                  
                              'TIH' OR                                  
                              'TII' OR                                  
                              'FGL' OR                                  
                              'OFL'                                     
              NEXT SENTENCE                                             
           ELSE                                                         
              IF NOT PI-JNT-NAME-KEYED                                  
                 MOVE AL-UANOF           TO BJNT-INIT-ATTRB             
                                            BJNT-LST-NAME-ATTRB         
                                            BJNT-1ST-NAME-ATTRB.        
                                                                        
           IF PI-COMPANY-ID = 'TMS' OR 'CID' OR 'DCC' or 'AHL' or 'VPP'
              NEXT SENTENCE                                             
           ELSE                                                         
              IF NOT PI-BENEFICIARY-KEYED                               
                  MOVE AL-UANOF           TO BBENEFICIARY-ATTRB.        
                                                                        
           IF NOT PI-ALT-BEN-KEYED                                      
               MOVE AL-UANOF           TO BALT-BEN1-ATTRB
           END-IF
                                                                        
           IF NOT PI-ALT-PREM-KEYED                                     
               MOVE AL-UANOF           TO BALT-PREM1-ATTRB
           END-IF
      
           if pi-company-id = 'DCC' or 'VPP'
              IF NOT PI-ALT-BEN-KEYED
                 MOVE AL-UANOF         TO BALT-BEN2-ATTRB
              END-IF
              IF NOT PI-ALT-PREM-KEYED
                 MOVE AL-UANOF         TO BALT-PREM2-ATTRB
              END-IF
           end-if
      
           IF PI-PROCESSOR-ID = 'LGXX'                                  
              CONTINUE                                                  
           ELSE                                                         
              IF PI-COMPANY-ID = 'PEM' OR                               
                                 'CGL' OR                               
                                 'TIH' OR                               
                                 'TII' OR                               
                                 'FGL' OR                               
                                 'OFL'                                  
                 MOVE AL-UANOF            TO BADDRS2-ATTRB              
      *                                      BPHONE-ATTRB               
              ELSE
                 IF (PI-COMPANY-ID = 'CID' or 'AHL') AND
                    (PI-MAIL-YES)
                    MOVE AL-UANOF         TO BADDRS2-ATTRB
                 END-IF
              END-IF
           END-IF
                                                                        
           .
       0610-EXIT.                                                       
           EXIT.                                                        
                                                                        
           EJECT                                                        
                                                                        
       0700-PROTECT-FIELDS.                                             
                                                                        
           IF PI-COMPANY-ID = 'PEM' OR                                  
                              'CGL' OR                                  
                              'TIH' OR                                  
                              'TII' OR                                  
                              'FGL' OR                                  
                              'OFL'                                     
              NEXT SENTENCE                                             
           ELSE                                                         
              IF NOT PI-CAN-SUFFIX-KEYED                                
                 MOVE AL-SANOF         TO CSFX-ATTRB (1)                
                                          CSFX-ATTRB (2)                
                                          CSFX-ATTRB (3)                
                                          CSFX-ATTRB (4).               
                                                                        
           IF PI-PROCESSOR-ID = 'LGXX'                                  
              IF NOT PI-CAN-SUFFIX-KEYED                                
                 MOVE AL-SANOF         TO CSFX-ATTRB (1)                
                                          CSFX-ATTRB (2)                
                                          CSFX-ATTRB (3)                
                                          CSFX-ATTRB (4).               
                                                                        
           IF PI-COMPANY-ID = 'CSO' OR 'CID' or 'AHL'                   
              MOVE AL-SANOF            TO CLAST-NAME-ATTRB (1)          
                                          CLAST-NAME-ATTRB (2)          
                                          CLAST-NAME-ATTRB (3)          
                                          CLAST-NAME-ATTRB (4).         
                                                                        
           IF NOT PI-CAN-LIVES-KEYED                                    
              MOVE AL-SANOF            TO CLIVES-ATTRB (1)              
                                          CLIVES-ATTRB (2)              
                                          CLIVES-ATTRB (3)              
                                          CLIVES-ATTRB (4).             
                                                                        
           IF NOT PI-CAN-REA-KEYED
              MOVE AL-SANOF            TO CCANREA-ATTRB (1)
                                          CCANREA-ATTRB (2)
                                          CCANREA-ATTRB (3)
                                          CCANREA-ATTRB (4)
           END-IF
      
      *    IF PI-COMPANY-ID = 'HER'                                     
      *        NEXT SENTENCE                                            
      *    ELSE                                                         
      *        IF NOT PI-MICRO-NO-KEYED                                 
      *            MOVE AL-SANOF       TO CMICRO-NO-ATTRB (1)           
      *                                   CMICRO-NO-ATTRB (2)           
      *                                   CMICRO-NO-ATTRB (3)           
      *                                   CMICRO-NO-ATTRB (4).          
                                                                        
           IF NOT PI-PAYEE-KEYED                                        
               MOVE AL-SANOF           TO CPAYEE-ATTRB (1)              
                                          CPAYEE-ATTRB (2)              
                                          CPAYEE-ATTRB (3)              
                                          CPAYEE-ATTRB (4).             
           IF NOT PI-CHK-REQ-KEYED                                      
               MOVE AL-SANOF           TO CCHK-ATTRB   (1)              
                                          CCHK-ATTRB   (2)              
                                          CCHK-ATTRB   (3)              
                                          CCHK-ATTRB   (4).             
                                                                        
           IF NOT PI-REFUND-MTHD-KEYED                                  
               MOVE AL-SANOF           TO CMTHD1-ATTRB (1)              
                                          CMTHD2-ATTRB (1)              
                                          CMTHD1-ATTRB (2)              
                                          CMTHD2-ATTRB (2)              
                                          CMTHD1-ATTRB (3)              
                                          CMTHD2-ATTRB (3)              
                                          CMTHD1-ATTRB (4)              
                                          CMTHD2-ATTRB (4).             
                                                                        
       0700-EXIT.                                                       
           EXIT.                                                        
                                                                        
           EJECT                                                        
       0710-UNPROTECT-FIELDS.                                           
           IF PI-COMPANY-ID = 'PEM' OR                                  
                              'CGL' OR                                  
                              'TIH' OR                                  
                              'TII' OR                                  
                              'FGL' OR                                  
                              'OFL'                                     
              NEXT SENTENCE                                             
           ELSE                                                         
              IF NOT PI-CAN-SUFFIX-KEYED                                
               MOVE AL-UANOF           TO CSFX-ATTRB (1)                
                                          CSFX-ATTRB (2)                
                                          CSFX-ATTRB (3)                
                                          CSFX-ATTRB (4).               
                                                                        
           IF PI-PROCESSOR-ID = 'LGXX'                                  
              IF NOT PI-CAN-SUFFIX-KEYED                                
                 MOVE AL-UANOF         TO CSFX-ATTRB (1)                
                                          CSFX-ATTRB (2)                
                                          CSFX-ATTRB (3)                
                                          CSFX-ATTRB (4).               
                                                                        
           IF PI-COMPANY-ID = 'CSO' OR 'CID' or 'AHL'                   
              MOVE AL-UANOF            TO CLAST-NAME-ATTRB (1)          
                                          CLAST-NAME-ATTRB (2)          
                                          CLAST-NAME-ATTRB (3)          
                                          CLAST-NAME-ATTRB (4).         
                                                                        
           IF NOT PI-CAN-LIVES-KEYED                                    
               MOVE AL-UANOF           TO CLIVES-ATTRB (1)              
                                          CLIVES-ATTRB (2)              
                                          CLIVES-ATTRB (3)              
                                          CLIVES-ATTRB (4).             
                                                                        
           IF NOT PI-CAN-REA-KEYED
              MOVE AL-UANOF            TO CCANREA-ATTRB (1)
                                          CCANREA-ATTRB (2)
                                          CCANREA-ATTRB (3)
                                          CCANREA-ATTRB (4)
           END-IF
      
      *    IF PI-COMPANY-ID = 'HER'                                     
      *        NEXT SENTENCE                                            
      *    ELSE                                                         
      *        IF NOT PI-MICRO-NO-KEYED                                 
      *            MOVE AL-UNNOF       TO CMICRO-NO-ATTRB (1)           
      *                                   CMICRO-NO-ATTRB (2)           
      *                                   CMICRO-NO-ATTRB (3)           
      *                                   CMICRO-NO-ATTRB (4).          
                                                                        
           IF NOT PI-PAYEE-KEYED                                        
               MOVE AL-UANOF           TO CPAYEE-ATTRB (1)              
                                          CPAYEE-ATTRB (2)              
                                          CPAYEE-ATTRB (3)              
                                          CPAYEE-ATTRB (4).             
           IF NOT PI-CHK-REQ-KEYED                                      
               MOVE AL-UANOF           TO CCHK-ATTRB   (1)              
                                          CCHK-ATTRB   (2)              
                                          CCHK-ATTRB   (3)              
                                          CCHK-ATTRB   (4).             
                                                                        
           IF NOT PI-REFUND-MTHD-KEYED                                  
               MOVE AL-UANOF           TO CMTHD1-ATTRB (1)              
                                          CMTHD2-ATTRB (1)              
                                          CMTHD1-ATTRB (2)              
                                          CMTHD2-ATTRB (2)              
                                          CMTHD1-ATTRB (3)              
                                          CMTHD2-ATTRB (3)              
                                          CMTHD1-ATTRB (4)              
                                          CMTHD2-ATTRB (4).             
                                                                        
       0710-EXIT.                                                       
           EXIT.                                                        
           EJECT                                                        
       1000-EDIT-MAPB.                                                  
           IF PI-MAP-NAME NOT = VP630B                                  
               GO TO 1100-EDIT-MAPC.                                    
                                                                        
           IF PI-LAST-FUNC-DISPLAY                                      
             AND BSFX-LEN           = ZEROS                             
             AND B1ST-NAME-LEN      = ZEROS                             
             AND BLAST-NAME-LEN     = ZEROS                             
             AND BINIT-LEN          = ZEROS                             
             AND BJNT-1ST-NAME-LEN  = ZEROS                             
             AND BJNT-INIT-LEN      = ZEROS                             
             AND BJNT-LST-NAME-LEN  = ZEROS                             
             AND BSEX-LEN           = ZEROS                             
             AND BAGE-LEN           = ZEROS                             
             AND BTERM1-LEN         = ZEROS                             
             AND BTERM2-LEN         = ZEROS                             
             AND BTYPE1-LEN         = ZEROS                             
             AND BTYPE2-LEN         = ZEROS                             
             AND BBENE1-LEN         = ZEROS                             
             AND BBENE2-LEN         = ZEROS                             
             AND BALT-BEN1-LEN      = ZEROS                             
             AND BPREM1-LEN         = ZEROS                             
             AND BPREM2-LEN         = ZEROS                             
             AND BALT-PREM1-LEN     = ZEROS                             
             AND BVIN-LEN           = ZEROS
             AND BJNT-AGE-LEN       = ZEROS                             
             AND BBENEFICIARY-LEN   = ZEROS
             AND BCADDR1-LEN        = ZEROS
             AND BCADDR2-LEN        = ZEROS
             AND BCCITY-LEN         = ZEROS
             AND BCSTATE-LEN        = ZEROS
             AND BCZIPCD-LEN        = ZEROS
             AND BLN-OFFICER-LEN    = ZEROS                             
             AND BADDRS1-LEN        = ZEROS                             
             AND BADDRS2-LEN        = ZEROS                             
             AND BCITY-LEN          = ZEROS                             
             AND BSTATE-LEN         = ZEROS
             AND BZIPCDE-LEN        = ZEROS                             
             AND BAGE-LEN           = ZEROS
             and byear-len          = zeros
             and bmake-len          = zeros
             and bmodel-len         = zeros
             and bometer-len        = zeros
               MOVE SPACE              TO PI-DISPLAY-SW                 
               GO TO 1030-NOTHING-TO-EDIT.                              
                                                                        
       1010-EDIT-MAPB.                                                  
           IF BCERT-LEN             = ZEROS                             
             AND BLAST-NAME-LEN     = ZEROS                             
             AND BEFFDT-LEN         = ZEROS                             
             AND NOT PI-LAST-FUNC-DISPLAY                               
               GO TO 1030-NOTHING-TO-EDIT.                              
                                                                        
           MOVE AL-SABON               TO BSEQ-ATTRB.                   
                                                                        
           IF BCERT-LEN  GREATER ZEROS                                  
             AND PI-LAST-FUNC-DISPLAY                                   
               NEXT SENTENCE                                            
           ELSE                                                         
               IF BCERT-LEN  GREATER ZEROS                              
                   MOVE AL-UANON       TO BCERT-ATTRB                   
               ELSE                                                     
                   MOVE -1             TO BEFFDT-LEN
                   MOVE ER-2218        TO EMI-ERROR                     
                   MOVE AL-UABON       TO BCERT-ATTRB                   
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
                                                                        
           IF BSFX-LEN  NOT = ZEROS                                     
               MOVE 'Y'                TO PI-ISS-SUFFIX-KEYED-SW        
               MOVE AL-UANON           TO BSFX-ATTRB.                   
                                                                        
           IF BEFFDT-LEN  = ZEROS                                       
             AND PI-LAST-FUNC-DISPLAY                                   
               NEXT SENTENCE                                            
           ELSE                                                         
               IF BEFFDT-LEN   GREATER ZEROS                            
                   MOVE AL-UNNON           TO BEFFDT-ATTRB              
                   IF BEFFDT   NUMERIC                                  
                       MOVE 4              TO DC-OPTION-CODE            
                       MOVE BEFFDT    TO DC-GREG-DATE-1-MDY             
                       PERFORM 8500-DATE-CONVERT THRU 8500-EXIT         
                       MOVE DC-BIN-DATE-1  TO WS-CONVERTED-EFFDT        
                       IF NO-CONVERSION-ERROR                           
                           IF WS-CONVERTED-EFFDT NOT LESS               
                             PI-ACCT-LOW-EFF-DT  AND LESS               
                             PI-ACCT-HIGH-EXP-DT                        
                               PERFORM 1500-EDIT-ACCT-DT-RANGES THRU    
                                       1590-EXIT                        
                           ELSE                                         
                               MOVE 'Y' TO PI-FIN-RESP-ERROR-SW         
      *                        MOVE -1       TO BEFFDT-LEN              
      *                        MOVE ER-2589  TO EMI-ERROR               
      *                        MOVE AL-UNBON TO BEFFDT-ATTRB            
      *                        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT 
                       ELSE                                             
                           MOVE -1         TO BEFFDT-LEN                
                           MOVE ER-2226    TO EMI-ERROR                 
                           MOVE AL-UNBON   TO BEFFDT-ATTRB              
                           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     
                   ELSE                                                 
                       MOVE -1             TO BEFFDT-LEN                
                       MOVE ER-2223        TO EMI-ERROR                 
                       MOVE AL-UNBON       TO BEFFDT-ATTRB              
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
               ELSE                                                     
                   MOVE -1                 TO BEFFDT-LEN                
                   MOVE ER-2220            TO EMI-ERROR                 
                   MOVE AL-UNBON           TO BEFFDT-ATTRB              
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
                                                                        
      *    IF PI-COMPANY-ID = ('PEM' OR 'CRI')                          
      *      AND NOT PI-LAST-FUNC-DISPLAY                               
      *       IF BCERT      = BCERTV   AND                              
      *          BSFX       = BSFXV    AND                              
      *          BEFFDT     = BEFFDTV  AND                              
      *          BLAST-NAME = BLAST-NAMEV                               
      *          NEXT SENTENCE                                          
      *       ELSE                                                      
      *          MOVE -1               TO BCERT-LEN                     
      *          MOVE -1               TO BLAST-NAME-LEN                
      *          MOVE ER-3166          TO EMI-ERROR                     
      *          MOVE AL-UNBON         TO BCERT-ATTRB                   
      *                                   BCERTV-ATTRB                  
      *                                   BSFX-ATTRB                    
      *                                   BSFXV-ATTRB                   
      *                                   BEFFDT-ATTRB                  
      *                                   BEFFDTV-ATTRB                 
      *                                   BLAST-NAME-ATTRB              
      *                                   BLAST-NAMEV-ATTRB             
      *          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
                                                                        
           IF BLAST-NAME-LEN   GREATER ZEROS                            
               MOVE AL-UANON           TO BLAST-NAME-ATTRB.             
                                                                        
           IF B1ST-NAME-LEN    GREATER ZEROS                            
               MOVE AL-UANON           TO B1ST-NAME-ATTRB.              
                                                                        
           IF BINIT-LEN        GREATER ZEROS                            
               MOVE AL-UANON           TO BINIT-ATTRB.                  
                                                                        
           IF BSEX-LEN         GREATER ZEROS                            
               IF BSEX   = 'M' OR 'F'                                   
                   MOVE AL-UANON       TO BSEX-ATTRB                    
               ELSE                                                     
                   MOVE -1             TO BSEX-LEN                      
                   MOVE ER-2629        TO EMI-ERROR                     
                   MOVE AL-UABON       TO BSEX-ATTRB                    
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
                                                                        
           IF BAGE-LEN > 0
              MOVE 'Y'                 TO PI-AGE-KEYED-SW               
              IF BAGE NUMERIC                                           
                 MOVE BAGE             TO WS-BAGE                       
                 MOVE AL-UNNON         TO BAGE-ATTRB                    
              ELSE                                                      
                 MOVE -1             TO BAGE-LEN                        
                 MOVE ER-2223        TO EMI-ERROR                       
                 MOVE AL-UNBON       TO BAGE-ATTRB                      
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
                                                                        
           MOVE +0                     TO WS-SUB1
      
           .
       1020-EDIT-COVERAGES.                                             
           IF NOT MODIFY-CAP                                            
                MOVE 'UPDATE'       TO SM-READ                          
                PERFORM 9995-SECURITY-VIOLATION                         
                MOVE ER-0070        TO EMI-ERROR                        
                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                
                GO TO 8100-SEND-INITIAL-MAP.                            
                                                                        
           ADD +1                      TO WS-SUB1.                      
                                                                        
                                                                        
      
           IF BTYPE1-LEN       > ZEROS OR
              BTERM1-LEN       > ZEROS OR
              BBENE1-LEN       > ZEROS OR
              BPREM1-LEN       > ZEROS OR
              BALT-PREM1-LEN   > ZEROS OR
              BALT-BEN1-LEN    > ZEROS
              MOVE 'Y'                 TO WS-DATA-KEYED-SW
           ELSE
              GO TO 1020-EDIT-BENEFIT-2
           END-IF
      
           MOVE +1                     TO WS-SUB1
           IF NOT PI-LAST-FUNC-DISPLAY                                  
              IF BTYPE1-LEN  > ZEROS                     
                 MOVE AL-UANON         TO BTYPE1-ATTRB
                 PERFORM 1040-EDIT-INPUT-CODE
                                       THRU 1059-EXIT          
              END-IF                                                    
           ELSE                                                         
              IF BTYPE1-LEN > ZEROS                     
                 IF BTYPE1 = SPACES OR ZEROS                 
                    MOVE AL-UANON      TO BTYPE1-ATTRB
                 ELSE                                                   
                    MOVE AL-UANON      TO BTYPE1-ATTRB
                    PERFORM 1040-EDIT-INPUT-CODE
                                       THRU 1059-EXIT
                 END-IF
              END-IF
           END-IF
      
           IF PI-LAST-FUNC-DISPLAY                                      
              IF BTERM1-LEN  = ZEROS                           
                 CONTINUE
              ELSE
                 MOVE BTERM1I          TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT                                    
                 IF DEEDIT-FIELD-V0 NUMERIC                             
                    MOVE DEEDIT-FIELD-V0
                                       TO WS-BTERM1
                    IF WS-BTERM1 > ZERO                 
                       MOVE AL-UNNON   TO BTERM1-ATTRB
                    ELSE                                                
                       MOVE ER-2241    TO EMI-ERROR                
                       MOVE -1         TO BTERM1-LEN
                       MOVE AL-UNBOF   TO BTERM1-ATTRB
                       PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                    END-IF
                 ELSE                                                   
                    MOVE ER-2223       TO EMI-ERROR
                    MOVE -1            TO BTERM1-LEN
                    MOVE AL-UNBON      TO BTERM1-ATTRB
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 END-IF
              END-IF
           ELSE
              IF BTERM1-LEN > ZEROS              
                 MOVE BTERM1I          TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT                                   
                 IF DEEDIT-FIELD-V0      NUMERIC                       
                    IF DEEDIT-FIELD-V0 > ZERO
                       MOVE DEEDIT-FIELD-V0 TO WS-BTERM1
                       MOVE AL-UNNON        TO BTERM1-ATTRB
                    ELSE
                       MOVE ER-2241         TO EMI-ERROR               
                       MOVE -1              TO BTERM1-LEN
                       MOVE AL-UNBOF        TO BTERM1-ATTRB
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                    END-IF
                 ELSE
                    MOVE ER-2223             TO EMI-ERROR               
                    MOVE -1                  TO BTERM1-LEN
                    MOVE AL-UNBON            TO BTERM1-ATTRB
                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              ELSE                                                      
                 IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
                    MOVE ER-2240             TO EMI-ERROR               
                    MOVE -1                  TO BTERM1-LEN
                    MOVE AL-UNBOF            TO BTERM1-ATTRB
                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              END-IF
           END-IF
      
      
           IF BBENE1-LEN = ZEROS
              AND PI-LAST-FUNC-DISPLAY                                 
              CONTINUE
           ELSE                                                         
              IF BBENE1-LEN > ZEROS                       
                 MOVE AL-UNNON           TO BBENE1-ATTRB
                 EXEC CICS BIF DEEDIT                                   
                     FIELD  (BBENE1I)
                     LENGTH (12)                                        
                 END-EXEC                                               
                 IF BBENE1I NUMERIC                             
                    IF BBENE1I > ZEROS
                       MOVE BBENE1I TO WS-BBEN1
      *          MOVE BBENE1I           TO DEEDIT-FIELD
      *          PERFORM 8600-DEEDIT                                    
      *          IF DEEDIT-FIELD-V2  NUMERIC                            
      *             IF DEEDIT-FIELD-V2 > ZEROS                    
      *                MOVE DEEDIT-FIELD-V2 TO WS-BBEN1
                    ELSE                                                
                       MOVE ER-7632    TO EMI-ERROR                     
                       MOVE -1         TO BBENE1-LEN
                       MOVE AL-UNBOF   TO BBENE1-ATTRB
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                    END-IF
                 ELSE                                                   
                    MOVE ER-2223         TO EMI-ERROR                   
                    MOVE AL-UNBON        TO BBENE1-ATTRB
                    MOVE -1              TO BBENE1-LEN
                 END-IF
              ELSE                                                      
                 IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
                   MOVE ER-7632    TO EMI-ERROR                           
                   MOVE -1         TO BBENE1-LEN
                   MOVE AL-UNBOF   TO BBENE1-ATTRB
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              END-IF
           END-IF
      
      
           IF BPREM1-LEN = ZEROS
              AND PI-LAST-FUNC-DISPLAY                                  
              CONTINUE
           ELSE                                                         
              IF BPREM1-LEN > ZEROS                      
                 MOVE AL-UNNON           TO BPREM1-ATTRB
      *          MOVE BPREM1I            TO DEEDIT-FIELD                
      *          PERFORM 8600-DEEDIT                                    
      *          IF DEEDIT-FIELD-V2  NUMERIC                            
      *             IF DEEDIT-FIELD-V2 GREATER ZEROS                    
      *                MOVE DEEDIT-FIELD-V2 TO WS-BPREM1
                 EXEC CICS BIF DEEDIT                                   
                     FIELD  (BPREM1I)                          
                     LENGTH (11)                                        
                 END-EXEC                                               
                 IF BPREM1I NUMERIC                            
                    IF BPREM1I > ZEROS                   
                       MOVE BPREM1I TO WS-BPREM1
                    ELSE                                                
                       MOVE ER-7633    TO EMI-ERROR                     
                       MOVE -1         TO BPREM1-LEN
                       MOVE AL-UNBOF   TO BPREM1-ATTRB
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                    END-IF
                 ELSE                                                   
                    MOVE AL-UNBON   TO BPREM1-ATTRB
                    MOVE ER-2223         TO EMI-ERROR                   
                    MOVE -1              TO BPREM1-LEN
                 END-IF
              ELSE                                                      
                 IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
                    MOVE ER-7633    TO EMI-ERROR
                    MOVE -1         TO BPREM1-LEN
                    MOVE AL-UNBOF   TO BPREM1-ATTRB
                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              END-IF
           END-IF
      
           IF BALT-BEN1-LEN = ZEROS                            
              AND PI-LAST-FUNC-DISPLAY                                  
              CONTINUE
           ELSE                                                         
              IF BALT-BEN1-LEN > ZEROS                   
                 MOVE 'Y'              TO PI-ALT-BEN-KEYED-SW           
                 MOVE AL-UNNON         TO BALT-BEN1-ATTRB
      *          MOVE BALT-BEN1I       TO DEEDIT-FIELD            
      *          PERFORM 8600-DEEDIT                                    
      *          IF DEEDIT-FIELD-V2  NUMERIC                            
      *             MOVE DEEDIT-FIELD-V2 TO WS-BALT-BEN1
                 EXEC CICS BIF DEEDIT                                   
                     FIELD  (BALT-BEN1I)                       
                     LENGTH (12)                                        
                 END-EXEC                                               
                 IF BALT-BEN1I NUMERIC                         
                    MOVE BALT-BEN1I TO WS-BALT-BEN1
                 ELSE                                                   
                    MOVE ER-2223       TO EMI-ERROR                     
                    MOVE AL-UNBON      TO BALT-BEN1-ATTRB
                    MOVE -1            TO BALT-BEN1-LEN
                 END-IF
              END-IF
           END-IF
      
           IF BALT-PREM1-LEN = ZEROS                        
              AND PI-LAST-FUNC-DISPLAY                                  
              NEXT SENTENCE                                          
           ELSE                                                         
              IF BALT-PREM1-LEN > ZEROS                
                 MOVE 'Y'              TO PI-ALT-PREM-KEYED-SW          
                 MOVE AL-UNNON         TO BALT-PREM1-ATTRB
      *          MOVE BALT-PREM1I      TO DEEDIT-FIELD             
      *          PERFORM 8600-DEEDIT                                    
      *          IF DEEDIT-FIELD-V2  NUMERIC                            
      *             MOVE DEEDIT-FIELD-V2 TO WS-BALT-PREM1
                 EXEC CICS BIF DEEDIT                                   
                     FIELD  (BALT-PREM1I)                      
                     LENGTH (9)                                         
                 END-EXEC                                               
                 IF BALT-PREM1I NUMERIC                        
                    MOVE BALT-PREM1I TO WS-BALT-PREM1
                 ELSE                                                   
                    MOVE AL-UNBON      TO BALT-PREM1-ATTRB
                    MOVE ER-2223       TO EMI-ERROR
                    MOVE -1            TO BALT-PREM1-LEN
                 END-IF
              END-IF
           END-IF
      
           .
       1020-EDIT-BENEFIT-2.
      
           IF BTYPE2-LEN       > ZEROS OR
              BTERM2-LEN       > ZEROS OR
              BBENE2-LEN       > ZEROS OR
              BPREM2-LEN       > ZEROS OR
              BCRIT-PERD2-LEN  > ZEROS OR
              BALT-PREM2-LEN   > ZEROS OR
              BALT-BEN2-LEN    > ZEROS
              MOVE 'Y'                 TO WS-DATA-KEYED-SW
           ELSE
              GO TO 1025-CONT-EDIT
           END-IF
      
           MOVE +2                     TO WS-SUB1
           IF NOT PI-LAST-FUNC-DISPLAY                                  
              IF BTYPE2-LEN  > ZEROS                     
                 MOVE AL-UANON         TO BTYPE2-ATTRB
                 PERFORM 1040-EDIT-INPUT-CODE
                                       THRU 1059-EXIT          
              END-IF                                                    
           ELSE                                                         
              IF BTYPE2-LEN > ZEROS                     
                 IF BTYPE2 = SPACES OR ZEROS                 
                    MOVE AL-UANON      TO BTYPE2-ATTRB
                 ELSE                                                   
                    MOVE AL-UANON      TO BTYPE2-ATTRB
                    PERFORM 1040-EDIT-INPUT-CODE
                                       THRU 1059-EXIT
                 END-IF
              END-IF
           END-IF
      
           IF PI-LAST-FUNC-DISPLAY                                      
              IF BTERM2-LEN  = ZEROS                           
                 CONTINUE
              ELSE
                 MOVE BTERM2I          TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT                                    
                 IF DEEDIT-FIELD-V0 NUMERIC                             
                    MOVE DEEDIT-FIELD-V0
                                       TO WS-BTERM2
                    IF WS-BTERM2 > ZERO                 
                       MOVE AL-UNNON   TO BTERM2-ATTRB
                    ELSE                                                
                       MOVE ER-2241    TO EMI-ERROR                
                       MOVE -1         TO BTERM2-LEN
                       MOVE AL-UNBOF   TO BTERM2-ATTRB
                       PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                    END-IF
                 ELSE                                                   
                    MOVE ER-2223       TO EMI-ERROR
                    MOVE -1            TO BTERM2-LEN
                    MOVE AL-UNBON      TO BTERM2-ATTRB
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 END-IF
              END-IF
           ELSE
              IF BTERM2-LEN > ZEROS              
                 MOVE BTERM2I          TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT                                   
                 IF DEEDIT-FIELD-V0      NUMERIC                       
                    IF DEEDIT-FIELD-V0 > ZERO
                       MOVE DEEDIT-FIELD-V0 TO WS-BTERM2
                       MOVE AL-UNNON        TO BTERM2-ATTRB
                    ELSE
                       MOVE ER-2241         TO EMI-ERROR               
                       MOVE -1              TO BTERM2-LEN
                       MOVE AL-UNBOF        TO BTERM2-ATTRB
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                    END-IF
                 ELSE
                    MOVE ER-2223             TO EMI-ERROR               
                    MOVE -1                  TO BTERM2-LEN
                    MOVE AL-UNBON            TO BTERM2-ATTRB
                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              ELSE                                                      
                 IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
                    MOVE ER-2240             TO EMI-ERROR               
                    MOVE -1                  TO BTERM2-LEN
                    MOVE AL-UNBOF            TO BTERM2-ATTRB
                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              END-IF
           END-IF
      
           IF BBENE2-LEN = ZEROS
              AND PI-LAST-FUNC-DISPLAY                                 
              CONTINUE
           ELSE                                                         
              IF BBENE2-LEN > ZEROS                       
                 MOVE AL-UNNON           TO BBENE2-ATTRB
                 EXEC CICS BIF DEEDIT                                   
                     FIELD  (BBENE2I)                           
                     LENGTH (12)                                        
                 END-EXEC                                               
                 IF BBENE2I NUMERIC                             
                    IF BBENE2I > ZEROS
                       MOVE BBENE2I TO WS-BBEN2
      *          MOVE BBENE2I           TO DEEDIT-FIELD
      *          PERFORM 8600-DEEDIT                                    
      *          IF DEEDIT-FIELD-V2  NUMERIC                            
      *             IF DEEDIT-FIELD-V2 > ZEROS                    
      *                MOVE DEEDIT-FIELD-V2 TO WS-BBEN2
                    ELSE                                                
                       MOVE ER-7632    TO EMI-ERROR                     
                       MOVE -1         TO BBENE2-LEN
                       MOVE AL-UNBOF   TO BBENE2-ATTRB
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                    END-IF
                 ELSE                                                   
                    MOVE ER-2223         TO EMI-ERROR                   
                    MOVE AL-UNBON        TO BBENE2-ATTRB
                    MOVE -1              TO BBENE2-LEN
                 END-IF
              ELSE                                                      
                 IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
                   MOVE ER-7632    TO EMI-ERROR                           
                   MOVE -1         TO BBENE2-LEN
                   MOVE AL-UNBOF   TO BBENE2-ATTRB
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              END-IF
           END-IF
      
           IF BPREM2-LEN = ZEROS
              AND PI-LAST-FUNC-DISPLAY                                  
              CONTINUE
           ELSE                                                         
              IF BPREM2-LEN > ZEROS                      
                 MOVE AL-UNNON           TO BPREM2-ATTRB
      *          MOVE BPREM2I            TO DEEDIT-FIELD                
      *          PERFORM 8600-DEEDIT                                    
      *          IF DEEDIT-FIELD-V2  NUMERIC                            
      *             IF DEEDIT-FIELD-V2 GREATER ZEROS                    
      *                MOVE DEEDIT-FIELD-V2 TO WS-BPREM2
                 EXEC CICS BIF DEEDIT                                   
                     FIELD  (BPREM2I)                          
                     LENGTH (11)                                        
                 END-EXEC                                               
                 IF BPREM2I NUMERIC                            
                    IF BPREM2I > ZEROS                   
                       MOVE BPREM2I TO WS-BPREM2
                    ELSE                                                
                       MOVE ER-7633    TO EMI-ERROR                     
                       MOVE -1         TO BPREM2-LEN
                       MOVE AL-UNBOF   TO BPREM2-ATTRB
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                    END-IF
                 ELSE                                                   
                    MOVE AL-UNBON   TO BPREM2-ATTRB
                    MOVE ER-2223         TO EMI-ERROR                   
                    MOVE -1              TO BPREM2-LEN
                 END-IF
              ELSE                                                      
                 IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
                    MOVE ER-7633    TO EMI-ERROR
                    MOVE -1         TO BPREM2-LEN
                    MOVE AL-UNBOF   TO BPREM2-ATTRB
                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              END-IF
           END-IF
      
      *    IF BEXPIRE-LEN (WS-SUB1)   GREATER ZEROS                     
      *       MOVE 'Y'                 TO PI-EXPIRE-KEYED-SW            
      *        IF BEXPIRE (WS-SUB1)    NUMERIC                          
      *            MOVE AL-UNNON       TO BEXPIRE-ATTRB (WS-SUB1)       
      *            MOVE 4              TO DC-OPTION-CODE                
      *            MOVE BEXPIRE (WS-SUB1)   TO DC-GREG-DATE-1-MDY       
      *            PERFORM 8500-DATE-CONVERT THRU 8500-EXIT             
      *            MOVE DC-BIN-DATE-1  TO WS-CONVERTED-EXPIRDT (WS-SUB1)
      *            IF NO-CONVERSION-ERROR                               
      *                NEXT SENTENCE                                    
      *            ELSE                                                 
      *                MOVE -1         TO BEXPIRE-LEN   (WS-SUB1)       
      *                MOVE ER-2531    TO EMI-ERROR                     
      *                MOVE AL-UNBON   TO BEXPIRE-ATTRB (WS-SUB1)       
      *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
      *        ELSE                                                     
      *            MOVE -1             TO BEXPIRE-LEN   (WS-SUB1)       
      *            MOVE ER-2532        TO EMI-ERROR                     
      *            MOVE AL-UNBON       TO BEXPIRE-ATTRB (WS-SUB1)       
      *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
                                                                        
           IF BCRIT-PERD2-LEN > ZEROS                
              MOVE 'Y'                     TO PI-CRIT-PERD-KEYED-SW     
              MOVE BCRIT-PERD2I            TO DEEDIT-FIELD              
              PERFORM 8600-DEEDIT                                       
              IF DEEDIT-FIELD-V0 NUMERIC                                
                 MOVE DEEDIT-FIELD-V0      TO WS-BCRIT-PERD2
                 MOVE AL-UNNON             TO BCRIT-PERD2-ATTRB
              ELSE                                                     
                 MOVE -1                   TO BCRIT-PERD2-LEN
                 MOVE AL-UNBON             TO BCRIT-PERD2-ATTRB
                 MOVE ER-2223              TO EMI-ERROR                 
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              END-IF
           END-IF
                                                                        
           IF BALT-BEN2-LEN = ZEROS                            
              AND PI-LAST-FUNC-DISPLAY                                  
              CONTINUE
           ELSE                                                         
              IF BALT-BEN2-LEN > ZEROS                   
                 MOVE 'Y'              TO PI-ALT-BEN-KEYED-SW           
                 MOVE AL-UNNON         TO BALT-BEN2-ATTRB
      *          MOVE BALT-BEN2I       TO DEEDIT-FIELD            
      *          PERFORM 8600-DEEDIT                                    
      *          IF DEEDIT-FIELD-V2  NUMERIC                            
      *             MOVE DEEDIT-FIELD-V2 TO WS-BALT-BEN2
                 EXEC CICS BIF DEEDIT                                   
                     FIELD  (BALT-BEN2I)                       
                     LENGTH (12)                                        
                 END-EXEC                                               
                 IF BALT-BEN2I NUMERIC                         
                    MOVE BALT-BEN2I TO WS-BALT-BEN2
                 ELSE                                                   
                    MOVE ER-2223       TO EMI-ERROR                     
                    MOVE AL-UNBON      TO BALT-BEN2-ATTRB
                    MOVE -1            TO BALT-BEN2-LEN
                 END-IF
              END-IF
           END-IF
      
           IF BALT-PREM2-LEN = ZEROS                        
              AND PI-LAST-FUNC-DISPLAY                                  
              NEXT SENTENCE                                          
           ELSE                                                         
              IF BALT-PREM2-LEN > ZEROS                
                 MOVE 'Y'              TO PI-ALT-PREM-KEYED-SW          
                 MOVE AL-UNNON         TO BALT-PREM2-ATTRB
      *          MOVE BALT-PREM2I      TO DEEDIT-FIELD             
      *          PERFORM 8600-DEEDIT                                    
      *          IF DEEDIT-FIELD-V2  NUMERIC                            
      *             MOVE DEEDIT-FIELD-V2 TO WS-BALT-PREM2
                 EXEC CICS BIF DEEDIT                                   
                     FIELD  (BALT-PREM2I)                      
                     LENGTH (9)                                         
                 END-EXEC                                               
                 IF BALT-PREM2I NUMERIC                        
                    MOVE BALT-PREM2I TO WS-BALT-PREM2
                 ELSE                                                   
                    MOVE AL-UNBON      TO BALT-PREM2-ATTRB
                    MOVE ER-2223       TO EMI-ERROR
                    MOVE -1            TO BALT-PREM2-LEN
                 END-IF
              END-IF
           END-IF
      
      *    GO TO 1020-EDIT-COVERAGES.                                   
           .
       1025-CONT-EDIT.                                                  
      *    IF BLIVES-LEN               GREATER ZEROS                    
      *       MOVE 'Y'                   TO PI-ISS-LIVES-KEYED-SW       
      *       MOVE BLIVESI               TO DEEDIT-FIELD                
      *       PERFORM 8600-DEEDIT                                       
      *       IF DEEDIT-FIELD-V0 NUMERIC                                
      *          MOVE DEEDIT-FIELD-V0    TO WS-BLIVES                   
      *          MOVE AL-UNNON           TO BLIVES-ATTRB                
      *       ELSE                                                      
      *          MOVE -1                 TO BLIVES-LEN                  
      *          MOVE AL-UNBON           TO BLIVES-ATTRB                
      *          MOVE ER-2223            TO EMI-ERROR                   
      *          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
      
           IF (BSTATE-LEN > 0)
              AND (BSTATE NOT = '  ' AND '00')
              MOVE AL-UANON            TO BSTATE-ATTRB
              MOVE SPACES              TO ELCNTL-KEY
              MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
              MOVE '3'                 TO ELCNTL-REC-TYPE
              MOVE BSTATE              TO ELCNTL-ACCESS
              MOVE +0                  TO ELCNTL-SEQ
              EXEC CICS READ
                 DATASET   (FILE-ID-ELCNTL)
                 SET       (ADDRESS OF CONTROL-FILE)
                 RIDFLD    (ELCNTL-KEY)
                 RESP      (WS-RESPONSE)
              END-EXEC
              IF RESP-NORMAL
                 CONTINUE
              ELSE
                 MOVE ER-2963          TO EMI-ERROR
      *          MOVE -1               TO BSTATE-LEN
                 MOVE -1               TO BPFENTRL
                 MOVE AL-UABON         TO BSTATE-ATTRB
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 SUBTRACT +1 FROM EMI-FATAL-CTR
              END-IF
      *    ELSE
      *       MOVE ER-2209          TO EMI-ERROR
      *       MOVE -1               TO BSTATE-LEN
      *       MOVE AL-UABON         TO BSTATE-ATTRB
      *       PERFORM 9900-ERROR-FORMAT
      *                             THRU 9900-EXIT
      *       SUBTRACT +1 FROM EMI-FATAL-CTR
           END-IF
      
           IF BCSTATE-LEN > 0
              MOVE SPACES              TO ELCNTL-KEY
              MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
              MOVE '3'                 TO ELCNTL-REC-TYPE
              MOVE BCSTATE             TO ELCNTL-ACCESS
              MOVE +0                  TO ELCNTL-SEQ
              EXEC CICS READ
                 DATASET   (FILE-ID-ELCNTL)
                 SET       (ADDRESS OF CONTROL-FILE)
                 RIDFLD    (ELCNTL-KEY)
                 RESP      (WS-RESPONSE)
              END-EXEC
              IF RESP-NORMAL
                 CONTINUE
              ELSE
                 MOVE ER-2964          TO EMI-ERROR
      *          MOVE -1               TO BCSTATE-LEN
                 MOVE -1               TO BPFENTRL
                 MOVE AL-UABON         TO BCSTATE-ATTRB
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 SUBTRACT +1 FROM EMI-FATAL-CTR
              END-IF
           END-IF
      
           IF BJNT-1ST-NAME-LEN     GREATER ZEROS OR                    
              BJNT-INIT-LEN         GREATER ZEROS OR                    
              BJNT-LST-NAME-LEN     GREATER ZEROS                       
               MOVE 'Y'                TO PI-JNT-NAME-KEYED-SW          
               MOVE AL-UANON           TO BJNT-1ST-NAME-ATTRB           
                                          BJNT-INIT-ATTRB               
                                          BJNT-LST-NAME-ATTRB.          
                                                                        
           IF BJNT-AGE-LEN GREATER ZEROS                                
              MOVE 'Y'                 TO PI-JNT-AGE-KEYED-SW           
              IF BJNT-AGE NUMERIC                                       
                 MOVE BJNT-AGE         TO WS-BJNT-AGE                   
                 MOVE AL-UNNON         TO BJNT-AGE-ATTRB                
              ELSE                                                      
                 MOVE -1             TO BJNT-AGE-LEN                    
                 MOVE ER-2223        TO EMI-ERROR                       
                 MOVE AL-UNBON       TO BJNT-AGE-ATTRB                  
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
                                                                        
      *    IF BMICROFILM-NO-LEN  GREATER  ZEROS                         
      *        MOVE 'Y'                TO  PI-MICRO-NO-KEYED-SW         
      *        MOVE BMICROFILM-NOI     TO  DEEDIT-FIELD                 
      *        PERFORM 8600-DEEDIT                                      
      *        IF DEEDIT-FIELD-V0  NUMERIC                              
      *            MOVE DEEDIT-FIELD-V0                                 
      *                                TO  WS-I-MICRO-NO                
      *            MOVE AL-UNNON       TO  BMICROFILM-NO-ATTRB          
      *        ELSE                                                     
      *            MOVE -1             TO  BMICROFILM-NO-LEN            
      *            MOVE AL-UNBON       TO  BMICROFILM-NO-ATTRB          
      *            MOVE ER-2701        TO  EMI-ERROR                    
      *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
                                                                        
           IF BBENEFICIARY-LEN GREATER ZEROS                            
               MOVE 'Y'                TO PI-BENEFICIARY-KEYED-SW       
               MOVE AL-UANON           TO BBENEFICIARY-ATTRB.           
                                                                        
           IF BLN-OFFICER-LEN GREATER ZEROS                             
               MOVE 'Y'                TO PI-LN-OFFICER-KEYED-SW        
               MOVE AL-UANON           TO BLN-OFFICER-ATTRB.            
                                                                        
           .
       1027-CHECK-FUNCTION.                                             
                                                                        
           .
       1028-CHECK-MEMNO.                                                
                                                                        
      *    IF BMEM-NO-LEN GREATER ZEROS                                 
      *        MOVE 'Y'                TO PI-MEMBER-KEYED-SW            
      *        MOVE AL-UANON           TO BMEM-NO-ATTRB.                
                                                                        
      *    IF  BPHONE-LEN GREATER ZEROS                                 
      *        MOVE BPHONE             TO DEEDIT-FIELD                  
      *        PERFORM 8600-DEEDIT                                      
      *        MOVE DEEDIT-FIELD-V0    TO WS-BPHONE                     
      *        MOVE AL-UANON           TO BPHONE-ATTRB.                 
                                                                        
      *    IF BENTRYI = 'D' OR 'V'                                      
      *       GO TO 1029-CHECK-ERRORS.                                  
                                                                        
           IF NOT PI-LAST-FUNC-DISPLAY                                  
              AND WS-DATA-NOT-KEYED                                     
              MOVE ER-7400             TO EMI-ERROR                     
              MOVE -1                  TO BTYPE1-LEN
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
              MOVE 'Y'                 TO PI-ERROR-SW.                  
                                                                        
       1029-CHECK-ERRORS.                                               
                                                                        
           IF EMI-ERROR = ZEROS                                         
               MOVE 'Y'                TO PI-UPDATE-SW                  
               MOVE SPACE              TO PI-DISPLAY-SW                 
               PERFORM 4000-BUILD-ISSUE-RECORD THRU 4900-EXIT           
           ELSE                                                         
              IF (EMI-FATAL-CTR = ZEROS)
                 AND (EMI-FORCABLE-CTR = ZEROS)
                 MOVE 'Y'              TO PI-UPDATE-SW
                 MOVE SPACE            TO PI-DISPLAY-SW
                 PERFORM 4000-BUILD-ISSUE-RECORD THRU 4900-EXIT
                 MOVE 'Y'              TO PI-ERROR-SW
              ELSE
                 MOVE ZEROS              TO EMI-ERROR                     
                 MOVE 'Y'                TO PI-ERROR-SW
              END-IF
           END-IF
      
           .
       1030-NOTHING-TO-EDIT.                                            
           IF PI-DATA-ERRORS                                            
               MOVE AL-SABON           TO BSEQ-ATTRB                    
               GO TO 8200-SEND-DATAONLY.                                
                                                                        
           IF PI-SAV-BATCH-SEQ LESS PI-LAST-SEQ-NO-ADDED                
               SUBTRACT 1 FROM PI-SAV-BATCH-SEQ                         
               MOVE ER-0000        TO EMI-ERROR                         
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
               GO TO 2000-BROWSE-FWD.                                   
                                                                        
           MOVE LOW-VALUES     TO VP630BI.                              
           ADD +1                 PI-LAST-SEQ-NO-ADDED                  
                                  GIVING PI-NEXT-DISPLAY-SEQ-NO.        
           MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ.             
                                                                        
           PERFORM 8550-SET-MAP-SEQ-NOS.                                
                                                                        
           MOVE ER-0000        TO EMI-ERROR.                            
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           GO TO 8100-SEND-INITIAL-MAP.                                 
                                                                        
           EJECT                                                        
                                                                        
       1040-EDIT-INPUT-CODE.                                            
           IF WS-SUB1 = +2                                              
              GO TO 1050-EDIT-INPUT-AH-CODE.                            
                                                                        
           MOVE SPACES                 TO ELCNTL-ACCESS.                
           MOVE 'L'                    TO ELCNTL-REC-TYPE.              
           PERFORM 1070-ELCNTL-READ THRU 1079-EXIT.                     
                                                                        
           IF EMI-ERROR = 9999                                          
               GO TO 1048-NO-RECORD.                                    
                                                                        
           MOVE +1 TO WS-EDIT-SUB.                                      
                                                                        
       1041-SEARCH-LOOP.                                                
           IF CF-LIFE-CODE-OUT (WS-EDIT-SUB) = ZEROS                    
               GO TO 1047-NO-MATCH-FOUND.                               
                                                                        
           IF BTYPE1  = CF-LIFE-CODE-IN (WS-EDIT-SUB)                 
               MOVE CF-LIFE-CODE-OUT (WS-EDIT-SUB) TO WS-EDITED-LF-CODE 
               PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT          
               GO TO 1059-EXIT.                                         
                                                                        
           ADD 1   TO WS-EDIT-SUB.                                      
                                                                        
           IF WS-EDIT-SUB GREATER 120                                   
               GO TO 1047-NO-MATCH-FOUND.                               
                                                                        
           GO TO 1041-SEARCH-LOOP.                                      
                                                                        
       1047-NO-MATCH-FOUND.                                             
      *    MOVE ER-2424                TO EMI-ERROR.                    
      *    MOVE AL-UABON               TO BTYPE1-ATTRB
      *    MOVE -1                     TO BTYPE1-LEN
      *    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
      *    MOVE 'Y'                    TO ERROR-SW.                     
                                                                        
           MOVE BTYPE1                 TO WS-EDITED-LF-CODE.            
           PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT.             
                                                                        
           GO TO 1059-EXIT.                                             
                                                                        
       1048-NO-RECORD.                                                  
      *    MOVE ER-2423                TO EMI-ERROR.                    
      *    MOVE AL-UABON               TO BTYPE1-ATTRB
      *    MOVE -1                     TO BTYPE1-LEN
      *    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
      *    MOVE 'Y'                    TO ERROR-SW.                     
                                                                        
           MOVE BTYPE1                 TO WS-EDITED-LF-CODE.            
           PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT.             
                                                                        
           GO TO 1059-EXIT.                                             
                                                                        
       1050-EDIT-INPUT-AH-CODE.                                         
           MOVE SPACES                 TO ELCNTL-ACCESS.                
           MOVE 'A'                    TO ELCNTL-REC-TYPE.              
                                                                        
           PERFORM 1070-ELCNTL-READ THRU 1079-EXIT.                     
                                                                        
           IF EMI-ERROR = 9999                                          
               GO TO 1058-NO-RECORD.                                    
                                                                        
           MOVE +1 TO WS-EDIT-SUB.                                      
                                                                        
       1051-SEARCH-LOOP.                                                
           IF CF-AH-CODE-OUT (WS-EDIT-SUB) = ZEROS                      
               GO TO 1057-NO-MATCH-FOUND.                               
                                                                        
           IF BTYPE2    = CF-AH-CODE-IN (WS-EDIT-SUB)                   
               MOVE CF-AH-CODE-OUT (WS-EDIT-SUB) TO WS-EDITED-AH-CODE   
               PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT          
               GO TO 1059-EXIT.                                         
                                                                        
           ADD +1  TO WS-EDIT-SUB.                                      
                                                                        
           IF WS-EDIT-SUB GREATER +96                                   
               GO TO 1057-NO-MATCH-FOUND.                               
                                                                        
           GO TO 1051-SEARCH-LOOP.                                      
                                                                        
       1057-NO-MATCH-FOUND.                                             
      *    MOVE ER-2428                TO EMI-ERROR.                    
      *    MOVE AL-UABON               TO BTYPE2-ATTRB
      *    MOVE -1                     TO BTYPE2-LEN
      *    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
      *    MOVE 'Y'                    TO ERROR-SW.                     
                                                                        
           MOVE BTYPE2                 TO WS-EDITED-AH-CODE.            
           PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT.             
                                                                        
           GO TO 1059-EXIT.                                             
                                                                        
       1058-NO-RECORD.                                                  
      *    MOVE ER-2427                TO EMI-ERROR.                    
      *    MOVE AL-UABON               TO BTYPE2-ATTRB
      *    MOVE -1                     TO BTYPE2-LEN
      *    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
      *    MOVE 'Y'                    TO ERROR-SW.                     
                                                                        
           MOVE BTYPE2                 TO WS-EDITED-AH-CODE.            
           PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT.             
                                                                        
       1059-EXIT.                                                       
           EXIT.                                                        
                                                                        
           EJECT                                                        
                                                                        
       1060-BENEFIT-MASTER-READ.                                        
           MOVE SPACES                 TO ELCNTL-ACCESS.                
                                                                        
           IF ELCNTL-REC-TYPE = 'L'                                     
               MOVE WS-EDITED-LF-CODE  TO WS-BEN-CD                     
                                          ELCNTL-HI-BEN                 
               MOVE '4'                TO ELCNTL-REC-TYPE               
           ELSE                                                         
               MOVE WS-EDITED-AH-CODE  TO WS-BEN-CD                     
                                          ELCNTL-HI-BEN                 
               MOVE '5'                TO ELCNTL-REC-TYPE.              
                                                                        
           PERFORM 1070-ELCNTL-READ THRU 1079-EXIT.                     
                                                                        
           IF EMI-ERROR = 9999                                          
               GO TO 1062-NO-RECORD.                                    
                                                                        
           IF ELCNTL-COMPANY-ID NOT = CF-COMPANY-ID  OR                 
              ELCNTL-REC-TYPE   NOT = CF-RECORD-TYPE                    
                 GO TO 1062-NO-RECORD.                                  
                                                                        
           perform varying ws-sub from +1 by +1 until
              (ws-sub > +8)
              or (cf-benefit-code (ws-sub) = ws-ben-cd)
           end-perform
                                                                        
           IF WS-SUB NOT = +9                                           
               IF ELCNTL-REC-TYPE = '4'                                 
                   MOVE CF-BENEFIT-ALPHA (WS-SUB) TO WS-LF-ABBR-DESC    
                   move cf-co-earnings-calc (ws-sub)
                                       to ws-lf-earnings-calc
               ELSE                                                     
                   MOVE CF-BENEFIT-ALPHA (WS-SUB) TO WS-AH-ABBR-DESC    
           ELSE                                                         
               GO TO 1063-NO-MATCH-FOUND.                               
                                                                        
                                                                        
           IF  CF-TERM-IN-DAYS (WS-SUB)                                 
               MOVE 'Y'                TO WS-TERM-IN-DAYS-SW.           
                                                                        
           GO TO 1069-EXIT.                                             
                                                                        
       1062-NO-RECORD.                                                  
           MOVE ER-2426                TO EMI-ERROR.                    
                                                                        
           IF ELCNTL-REC-TYPE = '4'                                     
               MOVE AL-UABON           TO BTYPE1-ATTRB
               MOVE -1                 TO BTYPE1-LEN
           ELSE                                                         
               MOVE AL-UABON           TO BTYPE2-ATTRB
               MOVE -1                 TO BTYPE2-LEN
           END-IF
                                                                        
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE 'Y'                    TO ERROR-SW.                     
                                                                        
           GO TO 1069-EXIT.                                             
                                                                        
       1063-NO-MATCH-FOUND.                                             
           IF ELCNTL-REC-TYPE = '4'                                     
               MOVE ER-2425            TO EMI-ERROR                     
               MOVE AL-UABON           TO BTYPE1-ATTRB
               MOVE -1                 TO BTYPE1-LEN
           ELSE                                                         
               MOVE ER-2429            TO EMI-ERROR                     
               MOVE AL-UABON           TO BTYPE2-ATTRB
               MOVE -1                 TO BTYPE2-LEN
           END-IF
                                                                        
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE 'Y'                    TO ERROR-SW.                     
           GO TO 1069-EXIT.                                             
                                                                        
       1069-EXIT.                                                       
           EXIT.                                                        
                                                                        
           EJECT                                                        
                                                                        
       1070-ELCNTL-READ.                                                
           EXEC CICS HANDLE CONDITION                                   
               NOTFND  (1078-NO-RECORD)                                 
               ENDFILE (1078-NO-RECORD)                                 
           END-EXEC.                                                    
                                                                        
           IF ELCNTL-REC-TYPE = '1' OR '4' OR '5'                       
               EXEC CICS READ                                           
                   DATASET (FILE-ID-ELCNTL)                             
                   SET     (ADDRESS OF CONTROL-FILE)                    
                   RIDFLD  (ELCNTL-KEY)                                 
                   GTEQ                                                 
               END-EXEC                                                 
           ELSE                                                         
               EXEC CICS READ                                           
                   DATASET (FILE-ID-ELCNTL)                             
                   SET     (ADDRESS OF CONTROL-FILE)                    
                   RIDFLD  (ELCNTL-KEY)                                 
               END-EXEC.                                                
                                                                        
           GO TO 1079-EXIT.                                             
                                                                        
       1078-NO-RECORD.                                                  
           MOVE ER-9999                TO EMI-ERROR.                    
                                                                        
       1079-EXIT.                                                       
           EXIT.                                                        
                                                                        
           EJECT                                                        
                                                                        
      *1080-TERM-CONVERSION.                                            
      *    IF BMODE   = ' ' OR 'M'                                      
      *        MOVE WS-BPMTS      TO WS-CALC-TERM-WHOLE                 
      *        GO TO 1085-ROUND-TERM.                                   
                                                                        
      *    IF BMODE   = 'S'                                             
      *        COMPUTE WS-CALC-TERM = WS-BPMTS / 2                      
      *        GO TO 1085-ROUND-TERM.                                   
                                                                        
      *    IF BMODE   = 'W'                                             
      *        COMPUTE WS-CALC-TERM = WS-BPMTS / 4.33333                
      *        GO TO 1085-ROUND-TERM.                                   
                                                                        
      *    IF BMODE   = 'B'                                             
      *        COMPUTE WS-CALC-TERM = WS-BPMTS / 2.16667                
      *        GO TO 1085-ROUND-TERM.                                   
                                                                        
      *    IF BMODE   = 'T'                                             
      *        COMPUTE WS-CALC-TERM = WS-BPMTS / 1.08334.               
                                                                        
      *1085-ROUND-TERM.                                                 
      *    IF WS-CALC-TERM-REMAIN GREATER .00000                        
      *       ADD +1 TO WS-CALC-TERM.                                   
      *    MOVE ZEROS                  TO WS-CALC-TERM-REMAIN.          
                                                                        
      *    IF  BTYPE-LEN       (WS-SUB1)  GREATER ZEROS                 
      *        IF  WS-KIND-MONTHLY                                      
      *            IF BTERM-LEN    (WS-SUB1)  = ZEROS                   
      *               IF BPREM-LEN (WS-SUB1)  GREATER ZEROS             
      *                  IF  BPMT-LEN   GREATER ZEROS                   
      *                      NEXT SENTENCE                              
      *                  ELSE                                           
      *                      GO TO 1087-EDIT-TERM.                      
                                                                        
      *    IF PI-COMPANY-ID IS EQUAL TO 'HAN' OR 'JHL'                  
      *        IF BBEN-LEN (WS-SUB1) IS GREATER THAN ZEROS              
      *            GO TO 1087-EDIT-TERM.                                
                                                                        
      *    IF  BMODE   = 'M' OR ' '                                     
      *        MOVE WS-BPMT            TO  WS-BBEN    (WS-SUB1).        
                                                                        
      *    IF  BMODE   = 'W'                                            
      *        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 4.33333.   
                                                                        
      *    IF  BMODE   = 'S'                                            
      *        COMPUTE WS-BBEN (WS-SUB1)  ROUNDED = WS-BPMT * 2.        
                                                                        
      *    IF  BMODE   = 'B'                                            
      *        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 2.16667.   
                                                                        
      *    IF  BMODE   = 'T'                                            
      *        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 1.08334.   
                                                                        
      *    IF WS-SUB1 = +1                                              
      *        COMPUTE WS-BBEN  (WS-SUB1) = WS-BPMT * WS-BPMTS.         
                                                                        
      *    IF PI-COMPANY-ID = 'CRI'       OR  'LGX'                     
      *        IF WS-SUB1 = +1  AND                                     
      *           BBEN-LEN (WS-SUB1) GREATER ZEROS                      
      *             GO TO 1087-EDIT-TERM.                               
                                                                        
      *    MOVE  WS-BBEN (WS-SUB1)     TO BBENO       (WS-SUB1).        
      *    MOVE +12                    TO BBEN-LEN    (WS-SUB1).        
      *    MOVE AL-UNNON               TO BBEN-ATTRB  (WS-SUB1).        
                                                                        
      *1087-EDIT-TERM.                                                  
      *    IF BTERM-LEN (WS-SUB1)  = ZEROS                              
      *        MOVE WS-CALC-TERM-WHOLE   TO BTERMI      (WS-SUB1)       
      *                                     WS-BTERM    (WS-SUB1)       
      *        MOVE +3                   TO BTERM-LEN   (WS-SUB1)       
      *        GO TO 1089-EXIT.                                         
                                                                        
      *    MOVE BTERMI    (WS-SUB1)   TO DEEDIT-FIELD.                  
      *    PERFORM 8600-DEEDIT.                                         
      *    IF DEEDIT-FIELD-V0 NUMERIC                                   
      *       MOVE DEEDIT-FIELD-V0    TO WS-BTERM    (WS-SUB1)          
      *       IF WS-BTERM (WS-SUB1)  GREATER ZERO                       
      *          MOVE AL-UNNON        TO BTERM-ATTRB (WS-SUB1)          
      *       ELSE                                                      
      *          MOVE ER-2241         TO EMI-ERROR                      
      *          MOVE -1              TO BTERM-LEN   (WS-SUB1)          
      *          MOVE AL-UNBOF        TO BTERM-ATTRB (WS-SUB1)          
      *          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
      *          GO TO 1089-EXIT.                                       
                                                                        
      *    IF WS-BTERM (WS-SUB1)   NOT = WS-CALC-TERM-WHOLE             
      *        MOVE -1                   TO BTERM-LEN   (WS-SUB1)       
      *        MOVE ER-2593              TO EMI-ERROR                   
      *        MOVE AL-UNBON             TO BTERM-ATTRB (WS-SUB1)       
      *        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                
                                                                        
      *1089-EXIT.                                                       
      *    EXIT.                                                        
                                                                        
      *1090-CALCULATE-MONTHLY-TERM.                                     
      *    IF  BTYPE-LEN  (WS-SUB1)  GREATER ZEROS                      
      *        IF BTERM-LEN (WS-SUB1)  = ZEROS                          
      *           IF BPREM-LEN ( WS-SUB1) GREATER ZEROS                 
      *              IF  BPMT-LEN   GREATER ZEROS                       
      *                  NEXT SENTENCE                                  
      *              ELSE                                               
      *                  GO TO 1094-EXIT.                               
                                                                        
      *    COMPUTE  WS-CALC-TERM = WS-BDAYS / 31.                       
                                                                        
      *    IF WS-CALC-TERM-REMAIN GREATER .00000                        
      *       ADD +1 TO WS-CALC-TERM.                                   
                                                                        
      *    MOVE ZEROS                  TO  WS-CALC-TERM-REMAIN.         
                                                                        
      *    MOVE WS-CALC-TERM           TO  BTERMI    (WS-SUB1).         
      *    MOVE +3                     TO  BTERM-LEN (WS-SUB1).         
                                                                        
      *    IF  BMODE   = 'M'  OR  ' '                                   
      *        MOVE WS-BPMT            TO  WS-BBEN    (WS-SUB1).        
                                                                        
      *    IF  BMODE   = 'W'                                            
      *        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 4.33333.   
                                                                        
      *    IF  BMODE   = 'S'                                            
      *        COMPUTE WS-BBEN (WS-SUB1)  ROUNDED = WS-BPMT * 2.        
                                                                        
      *    IF  BMODE   = 'B'                                            
      *        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 2.16667.   
                                                                        
      *    IF  BMODE   = 'T'                                            
      *        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 1.08334.   
                                                                        
      *    IF WS-SUB1 = +1                                              
      *       COMPUTE WS-BBEN  (WS-SUB1) =                              
      *               WS-BBEN (WS-SUB1) * WS-CALC-TERM.                 
                                                                        
      *    MOVE  WS-BBEN (WS-SUB1)     TO BBENO       (WS-SUB1).        
      *    MOVE +12                    TO BBEN-LEN    (WS-SUB1).        
      *    MOVE AL-UNNON               TO BBEN-ATTRB  (WS-SUB1).        
                                                                        
      *1094-EXIT.                                                       
      *    EXIT.                                                        
      
       1095-CALC-AGE.                                                   
           MOVE WS-CONVERTED-BIRTH (1) TO DC-BIN-DATE-1
           MOVE WS-CURRENT-BIN-DT  TO DC-BIN-DATE-2.                    
           MOVE 1 TO DC-OPTION-CODE.                                    
           PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
           IF NO-CONVERSION-ERROR                                       
               COMPUTE WS-BAGE = DC-ELAPSED-MONTHS / 12                 
               MOVE WS-BAGE            TO BAGE                          
               MOVE +2                 TO BAGE-LEN                      
               MOVE AL-UNNON TO BAGE-ATTRB.                             
                                                                        
       1099-EXIT.                                                       
           EXIT.                                                        
                                                                        
           EJECT                                                        
                                                                        
       1100-EDIT-MAPC.                                                  
           MOVE +1                     TO WS-SUB2.                      
                                                                        
           IF PI-LAST-FUNC-DISPLAY                                      
              AND CLAST-NAME-LEN (1)   = ZEROS                          
              AND CLAST-NAME-LEN (2)   = ZEROS                          
              AND CLAST-NAME-LEN (3)   = ZEROS                          
              AND CLAST-NAME-LEN (4)   = ZEROS                          
              AND CCANDT1-LEN     (1) = ZEROS                           
              AND CCANDT2-LEN     (1) = ZEROS                           
              AND CCANDT1-LEN     (2) = ZEROS                           
              AND CCANDT2-LEN     (2) = ZEROS                           
              AND CCANDT1-LEN     (3) = ZEROS                           
              AND CCANDT2-LEN     (3) = ZEROS                           
              AND CCANDT1-LEN     (4) = ZEROS                           
              AND CCANDT2-LEN     (4) = ZEROS                           
              AND CMTHD1-LEN      (1) = ZEROS                           
              AND CMTHD2-LEN      (1) = ZEROS                           
              AND CMTHD1-LEN      (2) = ZEROS                           
              AND CMTHD2-LEN      (2) = ZEROS                           
              AND CMTHD1-LEN      (3) = ZEROS                           
              AND CMTHD2-LEN      (3) = ZEROS                           
              AND CMTHD1-LEN      (4) = ZEROS                           
              AND CMTHD2-LEN      (4) = ZEROS                           
              AND CREFUND1-LEN    (1) = ZEROS                           
              AND CREFUND2-LEN    (1) = ZEROS                           
              AND CREFUND1-LEN    (2) = ZEROS                           
              AND CREFUND2-LEN    (2) = ZEROS                           
              AND CREFUND1-LEN    (3) = ZEROS                           
              AND CREFUND2-LEN    (3) = ZEROS                           
              AND CREFUND1-LEN    (4) = ZEROS                           
              AND CREFUND2-LEN    (4) = ZEROS                           
              AND CLIVES-LEN     (1) = ZEROS                            
              AND CLIVES-LEN     (2) = ZEROS                            
              AND CLIVES-LEN     (3) = ZEROS                            
              AND CLIVES-LEN     (4) = ZEROS                            
              AND CCANREA-LEN    (1) = ZEROS                            
              AND CCANREA-LEN    (2) = ZEROS                            
              AND CCANREA-LEN    (3) = ZEROS                            
              AND CCANREA-LEN    (4) = ZEROS                            
              AND CPAYEE-LEN     (1) = ZEROS                            
              AND CPAYEE-LEN     (2) = ZEROS                            
              AND CPAYEE-LEN     (3) = ZEROS                            
              AND CPAYEE-LEN     (4) = ZEROS                            
              AND CCHK-LEN       (1) = ZEROS                            
              AND CCHK-LEN       (2) = ZEROS                            
              AND CCHK-LEN       (3) = ZEROS                            
              AND CCHK-LEN       (4) = ZEROS                            
               GO TO 1130-NOTHING-TO-EDIT.                              
                                                                        
       1110-EDIT-MAPC-LOOP.                                             
           IF CCERT-LEN       (WS-SUB2) = ZEROS                         
             AND CEFFDT-LEN   (WS-SUB2) = ZEROS                         
             AND CCANDT1-LEN  (WS-SUB2) = ZEROS                         
             AND CCANDT2-LEN  (WS-SUB2) = ZEROS                         
             AND CMTHD1-LEN   (WS-SUB2) = ZEROS                         
             AND CMTHD2-LEN   (WS-SUB2) = ZEROS                         
             AND CREFUND1-LEN (WS-SUB2) = ZEROS                         
             AND CREFUND2-LEN (WS-SUB2) = ZEROS                         
             AND NOT PI-LAST-FUNC-DISPLAY                               
               GO TO 1120-INCREMENT-OCCURRENCE.                         
                                                                        
           MOVE 'Y'                    TO WS-DATA-KEYED-SW.             
                                                                        
           MOVE AL-SABON               TO CSEQ-ATTRB (WS-SUB2).         
                                                                        
           IF CCERT-LEN (WS-SUB2) = ZEROS                               
             AND PI-LAST-FUNC-DISPLAY                                   
               NEXT SENTENCE                                            
           ELSE                                                         
               IF CCERT-LEN (WS-SUB2)  NOT = ZEROS                      
                   MOVE AL-UANON       TO CCERT-ATTRB (WS-SUB2)         
               ELSE                                                     
                   MOVE -1             TO CCERT-LEN   (WS-SUB2)         
                   MOVE ER-2218        TO EMI-ERROR                     
                   MOVE AL-UABON       TO CCERT-ATTRB (WS-SUB2)         
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
                                                                        
           IF CSFX-LEN (WS-SUB2)      GREATER ZEROS                     
               MOVE 'Y'                TO PI-CAN-SUFFIX-KEYED-SW        
               MOVE AL-UANON           TO CSFX-ATTRB (WS-SUB2).         
                                                                        
           IF CEFFDT-LEN (WS-SUB2) = ZEROS                              
             AND PI-LAST-FUNC-DISPLAY                                   
               NEXT SENTENCE                                            
           ELSE                                                         
               IF CEFFDT-LEN (WS-SUB2) GREATER ZEROS                    
                   MOVE AL-UNNON           TO CEFFDT-ATTRB (WS-SUB2)    
                   IF CEFFDT (WS-SUB2) NUMERIC                          
                       MOVE 4              TO DC-OPTION-CODE            
                       MOVE CEFFDT (WS-SUB2)  TO DC-GREG-DATE-1-MDY     
                       PERFORM 8500-DATE-CONVERT THRU 8500-EXIT         
                       MOVE DC-BIN-DATE-1  TO                           
                                   WS-CONVERTED-CAN-EFF-DT (WS-SUB2)    
                                   WS-CONVERTED-EFFDT                   
                       IF NO-CONVERSION-ERROR                           
                           IF WS-CONVERTED-CAN-EFF-DT (WS-SUB2)         
                             NOT LESS PI-ACCT-LOW-EFF-DT                
                             AND LESS PI-ACCT-HIGH-EXP-DT               
                               PERFORM 1500-EDIT-ACCT-DT-RANGES THRU    
                                       1590-EXIT                        
                           ELSE                                         
                               NEXT SENTENCE                            
      *                        MOVE -1       TO CEFFDT-LEN (WS-SUB2)    
      *                        MOVE ER-2589  TO EMI-ERROR               
      *                        MOVE AL-UNBON TO CEFFDT-ATTRB (WS-SUB2)  
      *                        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT 
                       ELSE                                             
                           MOVE -1         TO CEFFDT-LEN (WS-SUB2)      
                           MOVE ER-2226    TO EMI-ERROR                 
                           MOVE AL-UNBON   TO CEFFDT-ATTRB (WS-SUB2)    
                           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     
                   ELSE                                                 
                       MOVE -1             TO CEFFDT-LEN (WS-SUB2)      
                       MOVE ER-2223        TO EMI-ERROR                 
                       MOVE AL-UNBON       TO CEFFDT-ATTRB (WS-SUB2)    
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
               ELSE                                                     
                   MOVE -1                 TO CEFFDT-LEN (WS-SUB2)      
                   MOVE ER-2220            TO EMI-ERROR                 
                   MOVE AL-UNBON           TO CEFFDT-ATTRB (WS-SUB2)    
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
                                                                        
           IF CLAST-NAME-LEN (WS-SUB2) GREATER ZEROS                    
               MOVE AL-UANON           TO CLAST-NAME-ATTRB (WS-SUB2).   
                                                                        
           EJECT                                                        
                                                                        
       1115-EDIT-COVERAGES.                                             
           IF CCANDT1-LEN (WS-SUB2) = ZEROS                             
             AND PI-LAST-FUNC-DISPLAY                                   
               NEXT SENTENCE                                            
           ELSE                                                         
               IF CCANDT1-LEN (WS-SUB2) GREATER ZEROS                   
                   MOVE AL-UNNON       TO CCANDT1-ATTRB (WS-SUB2)       
                   IF PI-LAST-FUNC-DISPLAY AND                          
                      CCANDT1 (WS-SUB2) = SPACES                        
                       MOVE LOW-VALUES TO WS-CONVERTED-CANDT1 (WS-SUB2) 
                   ELSE                                                 
                      IF CCANDT1 (WS-SUB2) NUMERIC                      
                         MOVE 4              TO DC-OPTION-CODE          
                         MOVE CCANDT1 (WS-SUB2) TO                      
                                            DC-GREG-DATE-1-MDY          
                         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT       
                         MOVE DC-BIN-DATE-1  TO WS-CONVERTED-CANDT1     
                                                               (WS-SUB2)
                         IF NO-CONVERSION-ERROR                         
                            NEXT SENTENCE                               
                         ELSE                                           
                            MOVE -1       TO CCANDT1-LEN   (WS-SUB2)    
                            MOVE ER-2227  TO EMI-ERROR                  
                            MOVE AL-UNBON TO CCANDT1-ATTRB (WS-SUB2)    
                            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT    
                      ELSE                                              
                         MOVE -1         TO CCANDT1-LEN   (WS-SUB2)     
                         MOVE ER-2223    TO EMI-ERROR                   
                         MOVE AL-UNBON   TO CCANDT1-ATTRB (WS-SUB2)     
                         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT       
               ELSE                                                     
                   IF CREFUND1-LEN (WS-SUB2) GREATER ZEROS              
                      MOVE -1             TO CCANDT1-LEN   (WS-SUB2)    
                      MOVE ER-2222        TO EMI-ERROR                  
                      MOVE AL-UNBOF       TO CCANDT1-ATTRB (WS-SUB2)    
                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.         
                                                                        
           IF CREFUND1-LEN (WS-SUB2) = ZEROS                            
             AND PI-LAST-FUNC-DISPLAY                                   
               NEXT SENTENCE                                            
           ELSE                                                         
               IF CREFUND1-LEN (WS-SUB2) NOT = ZEROS                    
                  MOVE AL-UNNON       TO CREFUND1-ATTRB (WS-SUB2)       
      *           MOVE CREFUND1I (WS-SUB2) TO DEEDIT-FIELD-X11          
      *           PERFORM 8600-DEEDIT                                   
      *           MOVE DEEDIT-FIELD-V2  TO WS-CREFUND1 (WS-SUB2).       
                  EXEC CICS BIF DEEDIT                                  
                      FIELD  (CREFUND1I (WS-SUB2))                      
                      LENGTH (11)                                       
                  END-EXEC                                              
                  MOVE CREFUND1I (WS-SUB2) TO WS-CREFUND1 (WS-SUB2).    
                                                                        
      ******************************************************************
      *********** REFUND METHODS CORRESPOND TO EARNING METHODS *********
      *********** METHOD 'R' INDICATES A REPOSSESSION FOR 'FLC'*********
      ******************************************************************
           IF CMTHD1-LEN (WS-SUB2) = ZEROS                              
             AND PI-LAST-FUNC-DISPLAY                                   
               NEXT SENTENCE                                            
           ELSE                                                         
               IF CMTHD1-LEN (WS-SUB2) NOT = ZEROS                      
                  MOVE 'Y'            TO PI-REFUND-MTHD-KEYED-SW        
                  MOVE AL-UNNON       TO CMTHD1-ATTRB (WS-SUB2)         
                  IF CMTHD1 (WS-SUB2) EQUAL '1' OR '2' OR '3' OR '4'    
                                         OR '5' OR '6' OR '8' OR ' '    
      *                                  OR 'R'                         
                     NEXT SENTENCE                                      
                  ELSE                                                  
                      MOVE -1         TO CMTHD1-LEN   (WS-SUB2)         
                      MOVE ER-0582    TO EMI-ERROR                      
                      MOVE AL-UNBON   TO CMTHD1-ATTRB (WS-SUB2)         
                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.         
                                                                        
           MOVE SPACES                  TO WS-CONVERTED-CANDT2 (WS-SUB2)
           IF CCANDT2-LEN (WS-SUB2) = ZEROS                             
             AND PI-LAST-FUNC-DISPLAY                                   
               NEXT SENTENCE                                            
           ELSE                                                         
               IF CCANDT2-LEN (WS-SUB2) GREATER ZEROS                   
                   MOVE AL-UNNON       TO CCANDT2-ATTRB (WS-SUB2)       
                   IF PI-LAST-FUNC-DISPLAY AND                          
                      CCANDT2 (WS-SUB2) = SPACES                        
                       MOVE LOW-VALUES TO WS-CONVERTED-CANDT2 (WS-SUB2) 
                   ELSE                                                 
                      IF CCANDT2 (WS-SUB2) NUMERIC                      
                         MOVE 4              TO DC-OPTION-CODE          
                         MOVE CCANDT2 (WS-SUB2) TO                      
                                            DC-GREG-DATE-1-MDY          
                         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT       
                         MOVE DC-BIN-DATE-1  TO WS-CONVERTED-CANDT2     
                                                               (WS-SUB2)
                         IF NO-CONVERSION-ERROR                         
                            NEXT SENTENCE                               
                         ELSE                                           
                            MOVE -1       TO CCANDT2-LEN   (WS-SUB2)    
                            MOVE ER-2227  TO EMI-ERROR                  
                            MOVE AL-UNBON TO CCANDT2-ATTRB (WS-SUB2)    
                            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT    
                      ELSE                                              
                         MOVE -1         TO CCANDT2-LEN   (WS-SUB2)     
                         MOVE ER-2223    TO EMI-ERROR                   
                         MOVE AL-UNBON   TO CCANDT2-ATTRB (WS-SUB2)     
                         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.      
      ****     ELSE                                                     
      ****         IF CREFUND2-LEN (WS-SUB2) GREATER ZEROS              
      ****            MOVE -1             TO CCANDT2-LEN   (WS-SUB2)    
      ****            MOVE ER-2222        TO EMI-ERROR                  
      ****            MOVE AL-UNBOF       TO CCANDT2-ATTRB (WS-SUB2)    
      ****            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.         
                                                                        
           IF CREFUND2-LEN (WS-SUB2) = ZEROS                            
             AND PI-LAST-FUNC-DISPLAY                                   
               NEXT SENTENCE                                            
           ELSE                                                         
               IF CREFUND2-LEN (WS-SUB2) NOT = ZEROS                    
                  MOVE AL-UNNON       TO CREFUND2-ATTRB (WS-SUB2)       
      *           MOVE CREFUND2I (WS-SUB2) TO DEEDIT-FIELD-X11          
      *           PERFORM 8600-DEEDIT                                   
      *           MOVE DEEDIT-FIELD-V2  TO WS-CREFUND2 (WS-SUB2).       
                  EXEC CICS BIF DEEDIT                                  
                      FIELD  (CREFUND2I (WS-SUB2))                      
                      LENGTH (11)                                       
                  END-EXEC                                              
                  MOVE CREFUND2I (WS-SUB2) TO WS-CREFUND2 (WS-SUB2).    
                                                                        
           IF CMTHD2-LEN (WS-SUB2) = ZEROS                              
             AND PI-LAST-FUNC-DISPLAY                                   
               NEXT SENTENCE                                            
           ELSE                                                         
               IF CMTHD2-LEN (WS-SUB2) NOT = ZEROS                      
                  MOVE 'Y'            TO PI-REFUND-MTHD-KEYED-SW        
                  MOVE AL-UNNON       TO CMTHD2-ATTRB (WS-SUB2)         
                  IF CMTHD2 (WS-SUB2) EQUAL '1' OR '2' OR '3' OR '4'    
                                         OR '5' OR '6' OR '8' OR ' '    
      *                                  OR 'R'                         
                     NEXT SENTENCE                                      
                  ELSE                                                  
                      MOVE -1         TO CMTHD2-LEN   (WS-SUB2)         
                      MOVE ER-0582    TO EMI-ERROR                      
                      MOVE AL-UNBON   TO CMTHD2-ATTRB (WS-SUB2)         
                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.         
                                                                        
           IF CLIVES-LEN  (WS-SUB2)  GREATER ZEROS                      
               MOVE 'Y'                TO PI-CAN-LIVES-KEYED-SW         
               MOVE CLIVESI (WS-SUB2 ) TO DEEDIT-FIELD                  
               PERFORM 8600-DEEDIT                                      
               IF DEEDIT-FIELD-V0 NUMERIC                               
                   MOVE DEEDIT-FIELD-V0 TO WS-CLIVES   (WS-SUB2)        
                   MOVE AL-UNNON       TO CLIVES-ATTRB (WS-SUB2)        
               ELSE                                                     
                   MOVE -1             TO CLIVES-LEN   (WS-SUB2)        
                   MOVE AL-UNBON       TO CLIVES-ATTRB (WS-SUB2)        
                   MOVE ER-2223        TO EMI-ERROR                     
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
                                                                        
           IF CCANREA-LEN (WS-SUB2)  GREATER  ZEROS                   
               MOVE 'Y'                TO  PI-CAN-REA-KEYED-SW         
               IF CCANREA (WS-SUB2) = 'R' OR ' '
                  MOVE CCANREA (WS-SUB2)                                
                                       TO  WS-CAN-REA (WS-SUB2)        
                   MOVE AL-UANON       TO  CCANREA-ATTRB (WS-SUB2)    
               ELSE                                                     
                   MOVE -1             TO  CCANREA-LEN (WS-SUB2)      
                   MOVE AL-UABON       TO  CCANREA-ATTRB (WS-SUB2)    
                   MOVE ER-9841        TO  EMI-ERROR                    
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
                                                                        
           IF CPAYEE-LEN  (WS-SUB2)  GREATER ZEROS                      
              MOVE 'Y'                 TO PI-PAYEE-KEYED-SW.            
                                                                        
           IF CCHK-LEN    (WS-SUB2)  GREATER ZEROS                      
              MOVE 'Y'                 TO PI-CHK-REQ-KEYED-SW           
              IF  CCHK    (WS-SUB2)  = 'R' OR ' '                       
                  NEXT SENTENCE                                         
              ELSE                                                      
                  MOVE ER-7405         TO EMI-ERROR                     
                  MOVE -1              TO CCHK-LEN      (WS-SUB2)       
                  MOVE AL-UABON        TO CCHK-ATTRB    (WS-SUB2)       
                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
                                                                        
           IF PI-LAST-FUNC-DISPLAY                                      
              NEXT SENTENCE                                             
           ELSE                                                         
              IF CCANDT1-LEN (WS-SUB2) GREATER ZEROS OR                 
                 CCANDT2-LEN (WS-SUB2) GREATER ZEROS                    
                   NEXT SENTENCE                                        
                 ELSE                                                   
                   MOVE ER-2222          TO EMI-ERROR                   
                   MOVE -1               TO CCANDT1-LEN   (WS-SUB2)     
                   MOVE AL-UNBOF         TO CCANDT1-ATTRB (WS-SUB2)     
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
                                                                        
           IF PI-LAST-FUNC-DISPLAY                                      
              NEXT SENTENCE                                             
           ELSE                                                         
              IF WS-CONVERTED-CANDT1 (WS-SUB2) NOT = LOW-VALUES AND     
                 WS-CONVERTED-CANDT2 (WS-SUB2) = SPACES AND             
                 CREFUND2-LEN (WS-SUB2) GREATER ZEROS                   
                   MOVE WS-CONVERTED-CANDT1 (WS-SUB2) TO                
                        WS-CONVERTED-CANDT2 (WS-SUB2)                   
                   MOVE CCANDT1     (WS-SUB2) TO CCANDT2     (WS-SUB2)  
                   MOVE CCANDT1-LEN (WS-SUB2) TO CCANDT2-LEN (WS-SUB2). 
                                                                        
           IF  EMI-ERROR = ZEROS                                        
               MOVE 'Y'                TO PI-UPDATE-SW                  
           ELSE                                                         
               MOVE 'Y'                TO PI-ERROR-SW.                  
                                                                        
       1120-INCREMENT-OCCURRENCE.                                       
           ADD +1                      TO WS-SUB2.                      
                                                                        
           IF WS-SUB2 GREATER +4 OR PI-LAST-FUNC-DISPLAY                
               NEXT SENTENCE                                            
           ELSE                                                         
               GO TO 1110-EDIT-MAPC-LOOP.                               
                                                                        
           IF PI-DATA-ERRORS                                            
               MOVE AL-SABON           TO CSEQ-ATTRB (1) CSEQ-ATTRB (2) 
                                          CSEQ-ATTRB (3) CSEQ-ATTRB (4) 
              GO TO 8200-SEND-DATAONLY                                  
           ELSE                                                         
              PERFORM 5000-BUILD-CANCEL-RECORD THRU 5900-EXIT.          
                                                                        
       1130-NOTHING-TO-EDIT.                                            
           IF NOT PI-LAST-FUNC-DISPLAY                                  
              AND WS-DATA-NOT-KEYED                                     
                MOVE ER-7400             TO EMI-ERROR                   
                MOVE -1                  TO CCANDT1-LEN (1)             
                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                
                MOVE 'Y'                 TO PI-ERROR-SW                 
                GO TO 8200-SEND-DATAONLY.                               
                                                                        
           IF PI-SAV-BATCH-SEQ LESS PI-LAST-SEQ-NO-ADDED                
              SUBTRACT 1 FROM PI-SAV-BATCH-SEQ                          
              MOVE ER-0000            TO EMI-ERROR                      
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
              GO TO 2000-BROWSE-FWD.                                    
                                                                        
           MOVE LOW-VALUES     TO VP630BI.                              
                                                                        
           MOVE SPACE          TO PI-DISPLAY-SW.                        
                                                                        
           ADD +1                 PI-LAST-SEQ-NO-ADDED                  
                                  GIVING PI-NEXT-DISPLAY-SEQ-NO.        
                                                                        
           MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ.             
                                                                        
           PERFORM 8550-SET-MAP-SEQ-NOS                                 
                        VARYING WS-SUB2 FROM +1 BY +1                   
                        UNTIL WS-SUB2 GREATER +4.                       
                                                                        
           MOVE ER-0000            TO EMI-ERROR.                        
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           GO TO 8100-SEND-INITIAL-MAP.                                 
                                                                        
           EJECT                                                        
                                                                        
       1500-EDIT-ACCT-DT-RANGES.                                        
                                                                        
      ******************************************************************
      *                                                                *
      *         E D I T   A C C O U N T   D A T E   R A N G E S        *
      *                                                                *
      *                                                                *
      *    NOTE:  IT IS ONLY NECESSARY TO EDIT THE DATE RANGES         *
      *           FOR COMPANYS THAT USE THE ACCOUNTS RECEIVABLE        *
      *           SYSTEM.                                              *
      *                                                                *
      *    1.  DETERMINE THE DATE RANGE FOR THE EFFECTIVE DATE.        *
      *                                                                *
      *    2.  VERIFY THE ACCOUNT AGENT.  THE ACCOUNT AGENT SHOULD BE  *
      *        THE SAME FOR THE ENTIRE BATCH.  IF IT CHANGES, IT IS    *
      *        AN ERROR.                                               *
      *                                                                *
      *    3.  VERIFY THAT THE FINANCIAL RESPONSIBLITY.  THE FINANCIAL *
      *        RESPONSIBILITY SHOULD BE THE SAME FOR THE ENTIRE BATCH. *
      *        IF IT CHANGES, IT IS AN ERROR.                          *
      *                                                                *
      ******************************************************************
                                                                        
           IF PI-AR-PROCESSING                                          
              NEXT SENTENCE                                             
           ELSE                                                         
              GO TO 1590-EXIT.                                          
                                                                        
           MOVE +0                     TO WS-ACCT-SUB.                  
                                                                        
       1525-FIND-ACCT-DT-RANGE.                                         
                                                                        
           ADD  +1                     TO WS-ACCT-SUB.                  
                                                                        
           IF WS-ACCT-SUB GREATER +32                                   
              MOVE  ER-2119            TO EMI-ERROR                     
              IF PI-MAP-NAME = VP630B                                   
                 MOVE -1               TO BPFENTRL                      
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
                 GO TO 8200-SEND-DATAONLY                               
              ELSE                                                      
                 MOVE -1               TO CEFFDT-LEN (WS-SUB2)          
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
                 GO TO 1590-EXIT.                                       
                                                                        
           IF WS-CONVERTED-EFFDT NOT LESS PI-ACCT-EFF-DT                
                                                       (WS-ACCT-SUB)    
              IF WS-CONVERTED-EFFDT  LESS PI-ACCT-EXP-DT                
                                                       (WS-ACCT-SUB)    
                 NEXT SENTENCE                                          
              ELSE                                                      
                 GO TO 1525-FIND-ACCT-DT-RANGE.                         
                                                                        
           IF PI-ACCOUNT-AGENT = SPACES                                 
              MOVE PI-ACCT-AGENT  (WS-ACCT-SUB) TO PI-ACCOUNT-AGENT     
              MOVE PI-REMIT-AGENT (WS-ACCT-SUB) TO PI-FIN-RESP.         
                                                                        
           IF PI-ACCT-AGENT  (WS-ACCT-SUB) = PI-ACCOUNT-AGENT           
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE 'Y'                 TO PI-ACCT-AGENT-ERROR-SW.       
                                                                        
           IF PI-REMIT-AGENT (WS-ACCT-SUB) = PI-FIN-RESP                
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE 'Y'                 TO PI-FIN-RESP-ERROR-SW.         
                                                                        
       1590-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
                                                                        
       2000-BROWSE-FWD.                                                 
           MOVE LOW-VALUES             TO VP630BI.                      
                                                                        
           ADD +1                      TO PI-SAV-BATCH-SEQ.             
                                                                        
           EXEC CICS HANDLE CONDITION                                   
               NOTFND (2020-END-FILE)                                   
           END-EXEC.                                                    
                                                                        
           EXEC CICS READ                                               
               SET     (ADDRESS OF PENDING-BUSINESS)                    
               DATASET (FILE-ID-ERPNDB)                                 
               RIDFLD  (PI-SAV-ENDING-ERPNDB-KEY)                       
               GTEQ                                                     
           END-EXEC.                                                    
                                                                        
           IF PB-COMPANY-CD  = PI-SAV-COMP-CD  AND                      
              PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH                       
               NEXT SENTENCE                                            
           ELSE                                                         
               GO TO 2020-END-FILE.                                     
                                                                        
           IF PB-BATCH-TRAILER                                          
               GO TO 2020-END-FILE.                                     
                                                                        
           MOVE PB-BATCH-SEQ-NO        TO PI-SAV-BATCH-SEQ.             
                                                                        
           IF PB-ISSUE                                                  
               MOVE VP630B             TO PI-MAP-NAME                   
               MOVE AL-SANOF           TO BDELHDGA                      
               PERFORM 7000-FORMAT-ISSUE-SCREEN THRU 7090-EXIT          
           ELSE                                                         
               MOVE VP630C             TO PI-MAP-NAME                   
               MOVE AL-SANOF           TO CDELHDGA                      
               PERFORM 7100-FORMAT-CANCEL-SCREEN THRU 7190-EXIT.        
                                                                        
       2010-SEND-MAP.                                                   
           GO TO 8100-SEND-INITIAL-MAP.                                 
                                                                        
       2020-END-FILE.                                                   
           MOVE SPACE                  TO PI-DISPLAY-SW.                
                                                                        
           IF PI-MAP-NAME = VP630B                                      
               MOVE LOW-VALUES         TO VP630BI                       
               MOVE -1                 TO BPFENTRL                      
               ADD +1                     PI-LAST-SEQ-NO-ADDED          
                     GIVING PI-NEXT-DISPLAY-SEQ-NO                      
               PERFORM 8550-SET-MAP-SEQ-NOS                             
           ELSE                                                         
               MOVE LOW-VALUES         TO VP630BI                       
               MOVE -1                 TO CPFENTRL                      
               ADD +1                     PI-LAST-SEQ-NO-ADDED          
                     GIVING PI-NEXT-DISPLAY-SEQ-NO                      
               PERFORM 8550-SET-MAP-SEQ-NOS                             
                       VARYING WS-SUB2 FROM +1 BY +1                    
                       UNTIL WS-SUB2 GREATER +4.                        
                                                                        
           MOVE ER-2217                TO EMI-ERROR.                    
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           GO TO 2010-SEND-MAP.                                         
                                                                        
           EJECT                                                        
                                                                        
       2100-BROWSE-BKWD.                                                
           MOVE LOW-VALUES             TO VP630BI.                      
                                                                        
           SUBTRACT +1             FROM PI-SAV-BATCH-SEQ.               
                                                                        
           IF PI-SAV-BATCH-SEQ NOT GREATER +0                           
               GO TO 2120-END-FILE.                                     
                                                                        
           EXEC CICS HANDLE CONDITION                                   
               NOTFND (2100-BROWSE-BKWD)                                
           END-EXEC.                                                    
                                                                        
           EXEC CICS READ                                               
               SET     (ADDRESS OF PENDING-BUSINESS)                    
               DATASET (FILE-ID-ERPNDB)                                 
               RIDFLD  (PI-SAV-ENDING-ERPNDB-KEY)                       
           END-EXEC.                                                    
                                                                        
           IF PB-COMPANY-CD  = PI-SAV-COMP-CD  AND                      
              PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH                       
               NEXT SENTENCE                                            
           ELSE                                                         
               GO TO 2120-END-FILE.                                     
                                                                        
           IF PB-BATCH-TRAILER                                          
               GO TO 2120-END-FILE.                                     
                                                                        
           IF PB-ISSUE                                                  
               MOVE VP630B             TO PI-MAP-NAME                   
               MOVE AL-SANOF           TO BDELHDGA                      
               PERFORM 7000-FORMAT-ISSUE-SCREEN THRU 7090-EXIT          
           ELSE                                                         
               MOVE VP630C             TO PI-MAP-NAME                   
               MOVE AL-SANOF           TO CDELHDGA                      
               PERFORM 7100-FORMAT-CANCEL-SCREEN THRU 7190-EXIT.        
                                                                        
       2110-SEND-MAP.                                                   
           GO TO 8100-SEND-INITIAL-MAP.                                 
                                                                        
       2120-END-FILE.                                                   
           MOVE ER-2431                TO EMI-ERROR.                    
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE ER-0000                TO EMI-ERROR.                    
           MOVE ZEROS                  TO PI-SAV-BATCH-SEQ.             
           GO TO 2000-BROWSE-FWD.                                       
                                                                        
           EJECT                                                        
                                                                        
       3000-CONTINUE-ENTRY.                                             
           MOVE PI-SAV-ENDING-ERPNDB-KEY TO ERPNDB-KEY.                 
                                                                        
           EXEC CICS HANDLE CONDITION                                   
               NOTFND (3300-REC-NOT-FND)                                
           END-EXEC.                                                    
                                                                        
           EXEC CICS STARTBR                                            
               DATASET (FILE-ID-ERPNDB)                                 
               RIDFLD  (ERPNDB-KEY)                                     
               GTEQ                                                     
           END-EXEC.                                                    
                                                                        
       3100-READ-LOOP.                                                  
           EXEC CICS HANDLE CONDITION                                   
               ENDFILE (3200-END-BROWSE)                                
           END-EXEC.                                                    
                                                                        
           EXEC CICS READNEXT                                           
               SET     (ADDRESS OF PENDING-BUSINESS)                    
               DATASET (FILE-ID-ERPNDB)                                 
               RIDFLD  (ERPNDB-KEY)                                     
           END-EXEC.                                                    
                                                                        
           IF PB-COMPANY-CD  = PI-SAV-COMP-CD   AND                     
              PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH                       
               NEXT SENTENCE                                            
           ELSE                                                         
               GO TO 3200-END-BROWSE.                                   
                                                                        
           IF NOT PB-BATCH-TRAILER                                      
               GO TO 3110-NOT-BATCH-TRAILER.                            
                                                                        
       3105-PRIME-PI-COUNTS.                                            
           IF PI-LF-ISS-REMITTED = ZEROS                                
               MOVE PB-B-LF-ISS-PRM-REMITTED  TO PI-LF-ISS-REMITTED.    
                                                                        
           IF PI-AH-ISS-REMITTED = ZEROS                                
               MOVE PB-B-AH-ISS-PRM-REMITTED  TO PI-AH-ISS-REMITTED.    
                                                                        
           IF PI-ISS-CNT-REMITTED = ZEROS                               
               MOVE PB-B-ISSUE-CNT-REMITTED   TO PI-ISS-CNT-REMITTED.   
                                                                        
           IF PI-CAN-CNT-REMITTED = ZEROS                               
               MOVE PB-B-CANCEL-CNT-REMITTED  TO PI-CAN-CNT-REMITTED.   
                                                                        
           IF PI-LF-CAN-REMITTED = ZEROS                                
               MOVE PB-B-LF-CAN-PRM-REMITTED  TO PI-LF-CAN-REMITTED.    
                                                                        
           IF PI-AH-CAN-REMITTED = ZEROS                                
               MOVE PB-B-AH-CAN-PRM-REMITTED  TO PI-AH-CAN-REMITTED.    
                                                                        
           GO TO 3200-END-BROWSE.                                       
                                                                        
       3110-NOT-BATCH-TRAILER.                                          
           IF PB-ISSUE                                                  
               ADD PB-I-LF-PREMIUM-AMT     TO PI-LF-ISS-ENTERED         
               ADD PB-I-LF-ALT-PREMIUM-AMT TO PI-LF-ISS-ENTERED         
               ADD PB-I-AH-PREMIUM-AMT     TO PI-AH-ISS-ENTERED         
               ADD +1                      TO PI-ISS-CNT-ENTERED        
           ELSE                                                         
               ADD PB-C-LF-CANCEL-AMT      TO PI-LF-CAN-ENTERED         
               ADD PB-C-AH-CANCEL-AMT      TO PI-AH-CAN-ENTERED         
               ADD +1                      TO PI-CAN-CNT-ENTERED.       
                                                                        
           MOVE PB-BATCH-SEQ-NO            TO PI-LAST-SEQ-NO-ADDED      
                                              PI-SAV-BATCH-SEQ.         
                                                                        
           GO TO 3100-READ-LOOP.                                        
                                                                        
       3200-END-BROWSE.                                                 
           EXEC CICS ENDBR                                              
               DATASET (FILE-ID-ERPNDB)                                 
           END-EXEC.                                                    
                                                                        
           ADD +1                         PI-LAST-SEQ-NO-ADDED          
                                  GIVING PI-NEXT-DISPLAY-SEQ-NO.        
                                                                        
           IF PI-MAINT-FUNC = 'B'                                       
               MOVE ZEROS              TO PI-SAV-BATCH-SEQ              
               GO TO 2000-BROWSE-FWD                                    
           ELSE                                                         
               ADD +1                  TO PI-SAV-BATCH-SEQ.             
                                                                        
           IF PI-LF-ISS-REMITTED  = ZEROS  AND                          
              PI-AH-ISS-REMITTED  = ZEROS  AND                          
              PI-LF-CAN-REMITTED  = ZEROS  AND                          
              PI-AH-CAN-REMITTED  = ZEROS  AND                          
              PI-ISS-CNT-REMITTED = ZEROS  AND                          
              PI-CAN-CNT-REMITTED = ZEROS                               
               MOVE  VP630B            TO PI-MAP-NAME                   
           ELSE                                                         
               IF PI-LF-ISS-REMITTED  = ZEROS AND                       
                  PI-AH-ISS-REMITTED  = ZEROS AND                       
                  PI-ISS-CNT-REMITTED = ZEROS                           
                   MOVE  VP630C        TO PI-MAP-NAME                   
               ELSE                                                     
                   MOVE  VP630B        TO PI-MAP-NAME.                  
                                                                        
           IF PI-MAP-NAME = VP630B                                      
               PERFORM 8550-SET-MAP-SEQ-NOS                             
           ELSE                                                         
               PERFORM 8550-SET-MAP-SEQ-NOS                             
                       VARYING WS-SUB2 FROM +1 BY +1                    
                       UNTIL WS-SUB2 GREATER +4.                        
                                                                        
           GO TO 8100-SEND-INITIAL-MAP.                                 
                                                                        
       3300-REC-NOT-FND.                                                
           MOVE ER-2212                TO EMI-ERROR.                    
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           GO TO 8100-SEND-INITIAL-MAP.                                 
                                                                        
           EJECT                                                        
                                                                        
       4000-BUILD-ISSUE-RECORD.                                         
           IF BSEQ  GREATER PI-LAST-SEQ-NO-ADDED                        
               GO TO 4100-ADD-ISSUE-RECORD.                             
                                                                        
      ******************************************************************
      *   THE DATA ENTRY SYSTEM ALLOWS BROWSING OF THE CURRENT BUS.    *
      *   FILE. THE DATA ENTRY SYS. DOES NOT HAVE A MAINT. FUNCTION.   *
      *   THE PROGRAM ASSUMES THAT IF A MATCH ON THE READ FOR UPDATE   *
      *   IS SUCCESSFUL, THE RECORD HAS PREVIOUSLY BEEN DISPLAYED      *
      *   THRUOUGH A BROWSE.  CHANGES ARE APPLIED AND THE PB-RECORD IS *
      *   REWRITTEN, ELSE A NEW PENDING BUS. RECORD IS ADDED.          *
      ******************************************************************
                                                                        
           MOVE PI-COMPANY-CD          TO ERPNDB-COMP-CD.               
           MOVE PI-SAV-ENTRY-BATCH     TO ERPNDB-ENTRY-BATCH.           
           MOVE BSEQ                   TO ERPNDB-BATCH-SEQ.             
                                                                        
           EXEC CICS HANDLE CONDITION                                   
               NOTFND (4100-ADD-ISSUE-RECORD)                           
           END-EXEC.                                                    
                                                                        
           EXEC CICS READ                                               
               SET     (ADDRESS OF PENDING-BUSINESS)                    
               DATASET (FILE-ID-ERPNDB)                                 
               RIDFLD  (ERPNDB-KEY)                                     
               UPDATE                                                   
           END-EXEC.                                                    
                                                                        
           MOVE PB-CONTROL-PRIMARY     TO ERPNDM-KEY.                   
                                                                        
           MOVE 'B'                    TO JP-RECORD-TYPE                
           MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
           MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
           MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
           PERFORM 8400-LOG-JOURNAL-RECORD.                             
                                                                        
           MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.             
           MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         
           MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT.             
                                                                        
           IF BLAST-NAME-LEN GREATER ZEROS                              
               MOVE BLAST-NAME         TO PB-I-INSURED-LAST-NAME.       
                                                                        
           IF B1ST-NAME-LEN GREATER ZEROS                               
               MOVE B1ST-NAME          TO PB-I-INSURED-FIRST-NAME.      
                                                                        
           IF BINIT-LEN     GREATER ZEROS                               
               MOVE BINIT              TO PB-I-INSURED-MIDDLE-INIT.     
                                                                        
           IF BAGE-LEN      GREATER ZEROS                               
               MOVE WS-BAGE            TO PB-I-AGE.                     
                                                                        
           IF BJNT-AGE-LEN  GREATER ZEROS                               
               MOVE WS-BJNT-AGE        TO PB-I-JOINT-AGE.               
                                                                        
           IF BSEX-LEN      GREATER ZEROS                               
               MOVE BSEX               TO PB-I-INSURED-SEX.             
                                                                        
           IF BTERM1-LEN > ZEROS                             
              MOVE WS-BTERM1           TO PB-I-LF-TERM.                 
                                                                        
           IF BTERM2-LEN > ZEROS                             
              MOVE WS-BTERM2             TO PB-I-AH-TERM.                
      
      ******************************************************************
      *          IF BTYPE = ZEROS DELETE LIFE COVERAGE.                *
      *                                                                *
      *          IF BTYPE = SPACES ZERO OUT BENEFIT CODE.              *
      ******************************************************************
                                                                        
           IF BTYPE1-LEN > ZEROS                         
              IF BTYPE1           NOT = SPACES AND ZEROS                
                 MOVE BTYPE1            TO PB-I-LF-INPUT-CD             
                 MOVE WS-EDITED-LF-CODE TO PB-I-LIFE-BENEFIT-CD         
                 MOVE WS-LF-ABBR-DESC   TO PB-I-LF-ABBR                 
              ELSE                                                      
                 IF BTYPE1      = SPACES                                
                    MOVE SPACES         TO PB-I-LF-INPUT-CD             
                                           PB-I-LF-ABBR                 
                    MOVE ZEROS          TO PB-I-LIFE-BENEFIT-CD         
                 ELSE                                                   
                    SUBTRACT PB-I-LF-PREMIUM-AMT FROM PI-LF-ISS-ENTERED 
                    SUBTRACT PB-I-LF-ALT-PREMIUM-AMT FROM               
                             PI-LF-ISS-ENTERED                          
                    MOVE SPACES         TO PB-I-LF-INPUT-CD             
                                           PB-I-LF-ABBR                 
                    MOVE ZEROS          TO PB-I-LF-TERM                 
                                           PB-I-LF-BENEFIT-AMT          
                                           PB-I-LF-PREMIUM-AMT          
                                           PB-I-LF-BENEFIT-CD           
                                           PB-I-LF-PREM-CALC            
                                           PB-I-LF-ALT-BENEFIT-AMT      
                                           PB-I-LF-ALT-PREMIUM-AMT      
                                           PB-I-LF-CRIT-PER             
                    MOVE LOW-VALUES     TO PB-I-LF-EXPIRE-DT.           
                                                                        
           IF  BBENE1-LEN > ZEROS                         
               MOVE WS-BBEN1           TO PB-I-LF-BENEFIT-AMT.          
                                                                        
           IF  BALT-BEN1-LEN > ZEROS                      
               MOVE WS-BALT-BEN1          TO PB-I-LF-ALT-BENEFIT-AMT.   
                                                                        
           IF  BPREM1-LEN > ZEROS                         
               IF WS-BPREM1       = WS-ALL-NINES OR                     
                  WS-BPREM1       GREATER WS-ALL-NINES                  
                  SUBTRACT PB-I-LF-PREMIUM-AMT FROM PI-LF-ISS-ENTERED   
                  MOVE ZEROS           TO PB-I-LF-PREMIUM-AMT           
                  MOVE '?'             TO PB-I-LF-CALC-FLAG             
               ELSE                                                     
                  SUBTRACT PB-I-LF-PREMIUM-AMT FROM PI-LF-ISS-ENTERED   
                  MOVE WS-BPREM1       TO PB-I-LF-PREMIUM-AMT           
                  ADD  WS-BPREM1       TO PI-LF-ISS-ENTERED             
                  MOVE SPACE           TO PB-I-LF-CALC-FLAG.            
                                                                        
           IF  BALT-PREM1-LEN > ZEROS                      
               SUBTRACT PB-I-LF-ALT-PREMIUM-AMT FROM PI-LF-ISS-ENTERED  
               MOVE WS-BALT-PREM1      TO PB-I-LF-ALT-PREMIUM-AMT       
               ADD  WS-BALT-PREM1      TO PI-LF-ISS-ENTERED.            
                                                                        
           IF  BALT-PREM2-LEN > ZEROS
               MOVE WS-BALT-PREM2      TO PB-I-TOT-FEES
           END-IF
                                                                        
      ******************************************************************
      *          IF BTYPE = ZEROS DELETE A&H COVERAGE.                 *
      *                                                                *
      *          IF BTYPE = SPACES ZERO OUT BENEFIT CODE.              *
      ******************************************************************
                                                                        
           IF BTYPE2-LEN > ZEROS                         
              IF BTYPE2           NOT = SPACES AND ZEROS                
                 MOVE BTYPE2            TO PB-I-AH-INPUT-CD             
                 MOVE WS-EDITED-LF-CODE TO PB-I-AH-BENEFIT-CD           
                 MOVE WS-LF-ABBR-DESC   TO PB-I-AH-ABBR                 
              ELSE                                                      
                 IF BTYPE2      = SPACES                                
                    MOVE SPACES         TO PB-I-AH-INPUT-CD             
                                           PB-I-AH-ABBR                 
                    MOVE ZEROS          TO PB-I-AH-BENEFIT-CD           
                 ELSE                                                   
                    SUBTRACT PB-I-AH-PREMIUM-AMT FROM PI-AH-ISS-ENTERED 
                    MOVE SPACES         TO PB-I-AH-INPUT-CD             
                                           PB-I-AH-ABBR                 
                    MOVE ZEROS          TO PB-I-AH-TERM                 
                                           PB-I-AH-BENEFIT-AMT          
                                           PB-I-AH-PREMIUM-AMT          
                                           PB-I-AH-BENEFIT-CD           
                                           PB-I-AH-PREM-CALC            
                                           PB-I-AH-CRIT-PER
                                           PB-I-TOT-FEES
                                           PB-I-TOT-FEES-CALC
                    MOVE LOW-VALUES     TO PB-I-AH-EXPIRE-DT.           
                                                                        
           IF  BBENE2-LEN > ZEROS                         
               MOVE WS-BBEN2           TO PB-I-AH-BENEFIT-AMT.          
                                                                        
           IF  BPREM2-LEN > ZEROS                         
               IF WS-BPREM2       = WS-ALL-NINES OR                     
                  WS-BPREM2       GREATER WS-ALL-NINES                  
                  SUBTRACT PB-I-AH-PREMIUM-AMT FROM PI-AH-ISS-ENTERED   
                  MOVE ZEROS              TO PB-I-AH-PREMIUM-AMT        
                  MOVE '?'                TO PB-I-AH-CALC-FLAG          
               ELSE                                                     
                  SUBTRACT PB-I-AH-PREMIUM-AMT FROM PI-AH-ISS-ENTERED   
                  MOVE WS-BPREM2       TO PB-I-AH-PREMIUM-AMT           
                  ADD  WS-BPREM2       TO PI-AH-ISS-ENTERED             
                  MOVE SPACE           TO PB-I-AH-CALC-FLAG.            
                                                                        
           MOVE ZEROS                  TO PB-I-LF-CRIT-PER
                                                                        
           IF BCRIT-PERD2-LEN > ZEROS                   
              MOVE WS-BCRIT-PERD2       TO PB-I-AH-CRIT-PER.            

           IF BLN-OFFICER-LEN  GREATER ZEROS                            
               MOVE BLN-OFFICER        TO PB-I-LOAN-OFFICER.            
      
           IF PB-1ST-PMT-DT-PROCESSING                                  
              IF PB-I-1ST-PMT-DT = LOW-VALUES                           
                 MOVE '1'              TO PB-I-DATA-ENTRY-SW.           
                                                                        
           IF BVIN-LEN > ZEROS
              MOVE BVIN-NOI            TO PB-I-VIN
           END-IF

           IF BJNT-1ST-NAME-LEN   GREATER ZEROS                         
               MOVE BJNT-1ST-NAME      TO PB-I-JOINT-FIRST-NAME.        
                                                                        
           IF BJNT-INIT-LEN       GREATER ZEROS                         
               MOVE BJNT-INIT          TO PB-I-JOINT-MIDDLE-INIT.       
                                                                        
           IF BJNT-LST-NAME-LEN   GREATER ZEROS                         
               MOVE BJNT-LST-NAME      TO PB-I-JOINT-LAST-NAME.         
                                                                        
           IF BBENEFICIARY-LEN    GREATER ZEROS                         
               MOVE BBENEFICIARY       TO PB-I-BENEFICIARY-NAME.        
                                                                        
           MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.          
           MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.            
                                                                        
           IF PI-MAIL-YES                                               
              MOVE '1'                 TO PB-I-MAIL-ADDRS-SW.           
                                                                        
           MOVE 'C'                    TO JP-RECORD-TYPE.               
           MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
           MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
                                                                        
           move pb-control-by-account  to elcrtt-key

           EXEC CICS REWRITE                                            
               DATASET (FILE-ID-ERPNDB)                                 
               FROM    (PENDING-BUSINESS)                               
           END-EXEC.                                                    
                                                                        
           MOVE ERPNDB-KEY             TO PI-SAV-ENDING-ERPNDB-KEY.     
                                                                        
           PERFORM 8400-LOG-JOURNAL-RECORD.                             
                                                                        
           IF EIBAID = DFHENTER                                         
               MOVE PI-NEXT-DISPLAY-SEQ-NO TO BSEQ                      
               ADD +1                      TO PI-NEXT-DISPLAY-SEQ-NO    
               MOVE AL-SABON               TO BSEQ-ATTRB.               
                                                                        
           if (byear-len      <> zeros)
              or (bmake-len   <> zeros)
              or (bmodel-len  <> zeros)
              or (bometer-len <> zeros)
              move 'C'                 to elcrtt-trlr-type
              perform 4910-read-elcrtt-update
                                       thru 4910-exit
              if resp-normal
                 perform 4920-rewrite-elcrtt
                                       thru 4920-exit
              else
                 if resp-notfnd
                    perform 4930-write-elcrtt
                                       thru 4930-exit
                 end-if
              end-if
           end-if

           IF PI-MAIL-YES                                               
              NEXT SENTENCE                                             
           ELSE                                                         
              GO TO 4900-EXIT.                                          
                                                                        
           IF BLAST-NAME-LEN = ZEROS AND                                
              B1ST-NAME-LEN  = ZEROS AND                                
              BINIT-LEN      = ZEROS AND                                
              BADDRS1-LEN    = ZEROS AND                                
              BADDRS2-LEN    = ZEROS AND                                
              BCITY-LEN      = ZEROS AND
              BSTATE-LEN     = ZEROS AND
              BZIPCDE-LEN    = ZEROS AND                                
              BBENEFICIARY-LEN = ZEROS AND
              BCADDR1-LEN    = ZEROS AND
              BCADDR2-LEN    = ZEROS AND
              BCCITY-LEN     = ZEROS AND
              BCSTATE-LEN    = ZEROS AND
              BCZIPCD-LEN    = ZEROS
              GO TO 4900-EXIT.                                          
                                                                        
           EXEC CICS HANDLE CONDITION                                   
               NOTFND (4185-ADD-MAILING-RECORD)                         
           END-EXEC.                                                    
                                                                        
           EXEC CICS READ                                               
               SET     (ADDRESS OF PENDING-MAILING-DATA)                
               DATASET (FILE-ID-ERPNDM)                                 
               RIDFLD  (ERPNDM-KEY)                                     
               UPDATE                                                   
           END-EXEC.                                                    
                                                                        
           MOVE 'B'                    TO JP-RECORD-TYPE.               
           MOVE PENDING-MAILING-DATA   TO JP-RECORD-AREA.               
           MOVE ERPNDM-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
           MOVE FILE-ID-ERPNDM         TO JP-FILE-ID.                   
           PERFORM 8400-LOG-JOURNAL-RECORD.                             
                                                                        
           MOVE PI-PROCESSOR-ID        TO PM-LAST-MAINT-BY.             
           MOVE EIBTIME                TO PM-LAST-MAINT-HHMMSS.         
           MOVE WS-CURRENT-BIN-DT      TO PM-LAST-MAINT-DT.             
                                                                        
           IF BLAST-NAME-LEN      GREATER ZEROS                         
               MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.         
                                                                        
           IF B1ST-NAME-LEN       GREATER ZEROS                         
               MOVE B1ST-NAME          TO PM-INSURED-FIRST-NAME.        
                                                                        
           IF BINIT-LEN           GREATER ZEROS                         
               MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.       
                                                                        
           IF BAGE-LEN            GREATER ZEROS                         
               MOVE WS-BAGE            TO PM-INSURED-ISSUE-AGE.         
      
           IF BLAST-NAME-LEN      GREATER ZEROS                         
               MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.         
                                                                        
           IF BINIT-LEN           GREATER ZEROS                         
               MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.       
                                                                        
           IF BADDRS1-LEN         GREATER ZERO                          
               MOVE BADDRS1            TO PM-ADDRESS-LINE-1.            
                                                                        
           IF BADDRS2-LEN         GREATER ZERO                          
               MOVE BADDRS2            TO PM-ADDRESS-LINE-2.            
                                                                        
           IF BCITY-LEN > 0
              MOVE BCITY               TO PM-CITY
           END-IF
      
           IF BSTATE-LEN > 0
              MOVE BSTATE              TO PM-STATE
           END-IF
                                                                        
           IF BZIPCDE-LEN GREATER ZEROS                                 
              MOVE BZIPCDE             TO WS-ZIP-CODE                  
           ELSE                                                         
              GO TO 4010-CRED-BENE
           END-IF
      
           IF WS-CANADIAN-ZIP                                           
              IF WS-ZIP-4 = SPACE  OR  '-'                             
                 MOVE WS-ZIP-CAN-2-POST1   TO PM-CAN-POST1            
                 MOVE WS-ZIP-CAN-2-POST2   TO PM-CAN-POST2            
              ELSE                                                     
                 MOVE WS-ZIP-CAN-1-POST1   TO PM-CAN-POST1            
                 MOVE WS-ZIP-CAN-1-POST2   TO PM-CAN-POST2
              END-IF
           ELSE                                                         
              IF WS-ZIP-6 = SPACE  OR  '-'                             
                 MOVE WS-ZIP-AM-2-CODE     TO PM-ZIP-CODE             
                 MOVE WS-ZIP-AM-2-PLUS4    TO PM-ZIP-PLUS4            
              ELSE                                                     
                 MOVE WS-ZIP-AM-1-CODE     TO PM-ZIP-CODE             
                 MOVE WS-ZIP-AM-1-PLUS4    TO PM-ZIP-PLUS4
              END-IF
           END-IF
      
           .
       4010-CRED-BENE.
      
           IF BBENEFICIARY-LEN > ZEROS
              MOVE BBENEFICIARY        TO PM-CRED-BENE-NAME
           END-IF                      
           IF BCADDR1-LEN > ZEROS      
              MOVE BCADDR1             TO PM-CRED-BENE-ADDR
           END-IF                      
           IF BCADDR2-LEN > ZEROS      
              MOVE BCADDR2             TO PM-CRED-BENE-ADDR2
           END-IF                      
           IF BCCITY-LEN > ZEROS      
              MOVE BCCITY              TO PM-CRED-BENE-CITY
           END-IF
           IF BCSTATE-LEN > ZEROS      
              MOVE BCSTATE             TO PM-CRED-BENE-STATE
           END-IF
      
           IF BCZIPCD-LEN > ZEROS                                 
              MOVE BCZIPCD             TO WS-ZIP-CODE
           ELSE                                                         
              GO TO 4010-CONTINUE
           END-IF
      
           IF WS-CANADIAN-ZIP                                           
              IF WS-ZIP-4 = SPACE  OR  '-'                             
                 MOVE WS-ZIP-CAN-2-POST1   TO PM-CB-CAN-POST1
                 MOVE WS-ZIP-CAN-2-POST2   TO PM-CB-CAN-POST2
              ELSE                                                     
                 MOVE WS-ZIP-CAN-1-POST1   TO PM-CB-CAN-POST1
                 MOVE WS-ZIP-CAN-1-POST2   TO PM-CB-CAN-POST2
              END-IF
           ELSE                                                         
              IF WS-ZIP-6 = SPACE  OR  '-'                             
                 MOVE WS-ZIP-AM-2-CODE     TO PM-CB-ZIP-CODE
                 MOVE WS-ZIP-AM-2-PLUS4    TO PM-CB-ZIP-PLUS4
              ELSE                                                     
                 MOVE WS-ZIP-AM-1-CODE     TO PM-CB-ZIP-CODE
                 MOVE WS-ZIP-AM-1-PLUS4    TO PM-CB-ZIP-PLUS4
              END-IF
           END-IF
      
           .
       4010-CONTINUE.                                                   
                                                                        
           MOVE 'C'                    TO JP-RECORD-TYPE.               
           MOVE PENDING-MAILING-DATA   TO JP-RECORD-AREA.               
           MOVE ERPNDM-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
           MOVE FILE-ID-ERPNDM         TO JP-FILE-ID.                   
                                                                        
           EXEC CICS REWRITE                                            
               DATASET (FILE-ID-ERPNDM)                                 
               FROM    (PENDING-MAILING-DATA)                           
           END-EXEC.                                                    
                                                                        
           PERFORM 8400-LOG-JOURNAL-RECORD.                             
                                                                        
           GO TO 4900-EXIT.                                             
                                                                        
           EJECT                                                        
                                                                        
       4100-ADD-ISSUE-RECORD.                                           

           EXEC CICS GETMAIN
               SET     (ADDRESS OF CERTIFICATE-TRAILERS)
               LENGTH  (ELCRTT-RECORD-LENGTH)
               INITIMG (GETMAIN-SPACE)
           END-EXEC

           EXEC CICS GETMAIN                                            
               SET     (ADDRESS OF PENDING-BUSINESS)                    
               LENGTH  (ERPNDB-RECORD-LENGTH)                           
               INITIMG (GETMAIN-SPACE)                                  
           END-EXEC.                                                    

           MOVE 'PB'                   TO PB-RECORD-ID.                 
           MOVE PI-COMPANY-CD          TO PB-COMPANY-CD                 
                                          PB-COMPANY-CD-A1.             
           MOVE PI-COMPANY-ID          TO PB-COMPANY-ID.                
           MOVE PI-SAV-ENTRY-BATCH     TO PB-ENTRY-BATCH.               
           MOVE BSEQ                   TO PB-BATCH-SEQ-NO.              
                                                                        
           IF BSEQ   GREATER PI-LAST-SEQ-NO-ADDED                       
               MOVE BSEQ          TO PI-LAST-SEQ-NO-ADDED.              
                                                                        
           MOVE PI-SAV-CARRIER         TO PB-CARRIER.                   
           MOVE PI-SAV-GROUPING        TO PB-GROUPING.                  
           MOVE PI-SAV-STATE           TO PB-STATE.                     
           MOVE PI-SAV-ACCOUNT         TO PB-ACCOUNT.                   
           MOVE '1'                    TO PB-RECORD-TYPE.               
           MOVE BCERT                  TO PB-CERT-PRIME.                
           MOVE WS-CONVERTED-EFFDT     TO PB-CERT-EFF-DT.               
           MOVE ZEROS                  TO PB-BATCH-CHG-SEQ-NO           
                                          PB-ALT-CHG-SEQ-NO.            
                                                                        
           MOVE +0                     TO PB-NO-OF-ERRORS.              
                                                                        
           MOVE LOW-VALUES             TO PB-COMMON-ERRORS.             
                                                                        
           MOVE ZEROS                  TO PB-I-LOAN-TERM                
                                          PB-I-LF-POLICY-FEE            
                                          PB-I-LF-PREM-CALC             
                                          PB-I-LF-ALT-PREM-CALC         
                                          PB-I-LF-RATE                  
                                          PB-I-LF-ALT-RATE              
                                          PB-I-LF-REI-RATE              
                                          PB-I-LF-ALT-REI-RATE          
                                          PB-I-RATE-DEV-PCT-LF          
                                          PB-I-cancel-fee
                                          PB-I-AH-PREM-CALC             
                                          PB-I-TOT-FEES
                                          PB-I-TOT-FEES-CALC
                                          PB-I-AH-RATE                  
                                          PB-I-AH-REI-RATE              
                                          PB-I-AH-RATE-TRM              
                                          PB-I-RATE-DEV-PCT-AH          
                                          PB-I-BUSINESS-TYPE            
                                          PB-I-LIFE-COMMISSION          
                                          PB-I-JOINT-COMMISSION         
                                          PB-I-AH-COMMISSION            
                                          PB-I-CURR-SEQ                 
                                          PB-CHG-COUNT                  
                                          PB-LF-BILLED-AMTS             
                                          PB-AH-BILLED-AMTS             
                                          PB-CALC-TOLERANCE             
                                          PB-I-EXTENTION-DAYS           
      *                                   PB-I-MICROFILM-NO             
                                          PB-I-TERM-IN-DAYS             
                                          PB-I-STATE-TAX                
                                          PB-I-MUNI-TAX                 
                                          PB-I-NUM-BILLED
                                          pb-i-loan-apr.              
                                                                        
           MOVE LOW-VALUES             TO PB-CREDIT-ACCEPT-DT           
                                          PB-I-LF-EXPIRE-DT             
                                          PB-I-AH-EXPIRE-DT             
                                          PB-I-1ST-PMT-DT               
                                          PB-BILLED-DT                  
                                          PB-ACCT-EFF-DT                
                                          PB-ACCT-EXP-DT.               
                                                                        
                                                                        
           MOVE 'X'                    TO PB-FATAL-FLAG.                
                                                                        
           IF PI-MAIL-YES                                               
              MOVE '1'                 TO PB-I-MAIL-ADDRS-SW.           
                                                                        
           IF PI-NB-MONTH-END-DT NOT = SPACES                           
              MOVE PI-NB-MONTH-END-DT  TO PB-CREDIT-SELECT-DT           
             ELSE                                                       
              MOVE PI-CR-MONTH-END-DT     TO PB-CREDIT-SELECT-DT.       
                                                                        
           IF BSFX-LEN            GREATER ZEROS                         
               MOVE BSFX               TO PB-CERT-SFX.                  
                                                                        
           IF BLAST-NAME-LEN      GREATER ZEROS                         
               MOVE BLAST-NAME         TO PB-I-INSURED-LAST-NAME.       
                                                                        
           IF B1ST-NAME-LEN       GREATER ZEROS                         
               MOVE B1ST-NAME          TO PB-I-INSURED-FIRST-NAME.      
                                                                        
           IF BINIT-LEN           GREATER ZEROS                         
               MOVE BINIT              TO PB-I-INSURED-MIDDLE-INIT.     
                                                                        
           IF BAGE-LEN            GREATER ZEROS                         
               MOVE WS-BAGE            TO PB-I-AGE                      
           ELSE                                                         
               MOVE ZEROS              TO PB-I-AGE.                     
                                                                        
           IF BJNT-AGE-LEN        GREATER ZEROS                         
               MOVE WS-BJNT-AGE        TO PB-I-JOINT-AGE                
           ELSE                                                         
               MOVE ZEROS              TO PB-I-JOINT-AGE.               
                                                                        
               MOVE LOW-VALUES          TO PB-I-BIRTHDAY.               
      
              MOVE LOW-VALUES          TO PB-I-JOINT-BIRTHDAY
      
           IF BSEX-LEN            GREATER ZEROS                         
               MOVE BSEX               TO PB-I-INSURED-SEX.             
                                                                        
           IF BTERM1-LEN > ZEROS                         
              MOVE WS-BTERM1              TO PB-I-LF-TERM               
           ELSE                                                         
              MOVE ZEROS               TO PB-I-LF-TERM.                 
                                                                        
           IF BTERM2-LEN > ZEROS                         
              MOVE WS-BTERM2              TO PB-I-AH-TERM               
           ELSE                                                         
              MOVE ZEROS              TO PB-I-AH-TERM.                  
                                                                        
           MOVE ZEROS              TO PB-I-LOAN-TERM.               
                                                                        
               MOVE ZEROS              TO PB-I-PAY-FREQUENCY.           
                                                                        
                                                                        
                                                                        
               MOVE ZEROS              TO PB-I-NO-OF-PAYMENTS.          
                                                                        
               MOVE ZEROS              TO PB-I-PAYMENT-AMOUNT.          
                                                                        
                                                                        
           IF BTYPE1-LEN > ZEROS                         
              IF BTYPE1           NOT = ZEROS OR SPACES                 
                 MOVE BTYPE1            TO PB-I-LF-INPUT-CD             
                 MOVE WS-EDITED-LF-CODE TO PB-I-LIFE-BENEFIT-CD         
                 MOVE WS-LF-ABBR-DESC   TO PB-I-LF-ABBR                 
              ELSE                                                      
                 MOVE ZEROS             TO PB-I-LIFE-BENEFIT-CD         
           ELSE                                                         
                 MOVE ZEROS             TO PB-I-LIFE-BENEFIT-CD.        
                                                                        
           IF  BBENE1-LEN > ZEROS                         
               MOVE WS-BBEN1           TO PB-I-LF-BENEFIT-AMT           
           ELSE                                                         
               MOVE ZEROS              TO PB-I-LF-BENEFIT-AMT.          
                                                                        
           IF  BALT-BEN1-LEN > ZEROS                      
               MOVE WS-BALT-BEN1       TO PB-I-LF-ALT-BENEFIT-AMT       
           ELSE                                                         
               MOVE ZEROS              TO PB-I-LF-ALT-BENEFIT-AMT.      
                                                                        
           IF  BPREM1-LEN > ZEROS                         
               IF WS-BPREM1      = WS-ALL-NINES OR                      
                  WS-BPREM1      GREATER WS-ALL-NINES                   
                  MOVE ZEROS           TO PB-I-LF-PREMIUM-AMT           
                  MOVE '?'             TO PB-I-LF-CALC-FLAG             
               ELSE                                                     
                  ADD  WS-BPREM1       TO PI-LF-ISS-ENTERED             
                  MOVE WS-BPREM1       TO PB-I-LF-PREMIUM-AMT           
           ELSE                                                         
               MOVE ZEROS              TO PB-I-LF-PREMIUM-AMT.          
                                                                        
           IF  BALT-PREM1-LEN > ZEROS                      
               MOVE WS-BALT-PREM1      TO PB-I-LF-ALT-PREMIUM-AMT       
               ADD  WS-BALT-PREM1      TO PI-LF-ISS-ENTERED             
           ELSE                                                         
               MOVE ZEROS              TO PB-I-LF-ALT-PREMIUM-AMT.      
                                                                        
           IF  BALT-PREM2-LEN > ZEROS                      
               MOVE WS-BALT-PREM2      TO PB-I-TOT-FEES
           ELSE                                                         
               MOVE ZEROS              TO PB-I-TOT-FEES
           END-IF
                                                                        
           IF BTYPE2-LEN > ZEROS                         
              IF BTYPE2           NOT = ZEROS                           
                 MOVE BTYPE2            TO PB-I-AH-INPUT-CD             
                 MOVE WS-EDITED-AH-CODE TO PB-I-AH-BENEFIT-CD           
                 MOVE WS-AH-ABBR-DESC   TO PB-I-AH-ABBR                 
              ELSE                                                      
                 MOVE ZEROS             TO PB-I-AH-BENEFIT-CD           
           ELSE                                                         
                 MOVE ZEROS             TO PB-I-AH-BENEFIT-CD.          
                                                                        
           IF  BBENE2-LEN > ZEROS                      
               MOVE WS-BBEN2           TO PB-I-AH-BENEFIT-AMT           
           ELSE                                                         
               MOVE ZEROS              TO PB-I-AH-BENEFIT-AMT.          
                                                                        
           IF  BPREM2-LEN > ZEROS                         
               IF WS-BPREM2      = WS-ALL-NINES OR                      
                  WS-BPREM2      GREATER WS-ALL-NINES                   
                  MOVE ZEROS            TO PB-I-AH-PREMIUM-AMT          
                  MOVE '?'              TO PB-I-AH-CALC-FLAG            
               ELSE                                                     
                  ADD  WS-BPREM2        TO PI-AH-ISS-ENTERED            
                  MOVE WS-BPREM2        TO PB-I-AH-PREMIUM-AMT          
           ELSE                                                         
               MOVE ZEROS               TO PB-I-AH-PREMIUM-AMT.         
                                                                        
           IF PB-COMPANY-ID = 'NSL'                                     
           IF PB-I-AGE       GREATER 49   OR                            
              PB-I-JOINT-AGE GREATER 49                                 
               MOVE 'H'                TO PB-RECORD-BILL                
             ELSE                                                       
           IF PB-I-LF-BENEFIT-AMT GREATER +14999.99 OR                  
              PB-I-AH-BENEFIT-AMT GREATER +14999.99                     
                MOVE 'H'                TO PB-RECORD-BILL.              
                                                                        
      *    IF BCRIT-PERD-LEN      (1)   GREATER ZEROS                   
      *       MOVE WS-BCRIT-PERD  (1)   TO PB-I-LF-CRIT-PER             
      *    ELSE                                                         
              MOVE ZEROS                TO PB-I-LF-CRIT-PER.            
                                                                        
           IF BCRIT-PERD2-LEN > ZEROS                   
              MOVE WS-BCRIT-PERD2       TO PB-I-AH-CRIT-PER             
           ELSE                                                         
              MOVE ZEROS                TO PB-I-AH-CRIT-PER.            
                                                                        
      *    IF BIND-GRP-LEN        GREATER ZEROS                         
      *        MOVE BIND-GRP           TO PB-I-INDV-GRP-OVRD.           
                                                                        
      *    IF BRTCLS-LEN          GREATER ZEROS                         
      *        MOVE BRTCLS             TO PB-I-RATE-CLASS-OVRD.         
                                                                        
      *    IF BSIG-LEN            GREATER ZEROS                         
      *        MOVE BSIG               TO PB-I-SIG-SW.                  
                                                                        
                                                                        
                                                                        
                                                                        
           IF BLN-OFFICER-LEN     GREATER ZEROS                         
               MOVE BLN-OFFICER        TO PB-I-LOAN-OFFICER.            
                                                                        
           MOVE LOW-VALUES               TO PB-I-LF-EXPIRE-DT.       
                                                                        
           MOVE LOW-VALUES               TO PB-I-AH-EXPIRE-DT.       
                                                                        
                                                                        
              MOVE ZEROS               TO PB-I-TERM-IN-DAYS             
                                          PB-I-EXTENTION-DAYS.          
                                                                        
           IF PB-1ST-PMT-DT-PROCESSING                                  
              IF PB-I-1ST-PMT-DT = LOW-VALUES                           
                 MOVE '1'              TO PB-I-DATA-ENTRY-SW.           
                                                                        
           IF BVIN-LEN > ZEROS
              MOVE BVIN-NOI            TO PB-I-VIN
           ELSE
              MOVE SPACES              TO PB-I-VIN
           END-IF
      
           MOVE ZEROS               TO PB-I-LIVES.                   
                                                                        
           IF BJNT-1ST-NAME-LEN   GREATER ZEROS                         
               MOVE BJNT-1ST-NAME      TO PB-I-JOINT-FIRST-NAME.        
                                                                        
           IF BJNT-INIT-LEN       GREATER ZEROS                         
               MOVE BJNT-INIT          TO PB-I-JOINT-MIDDLE-INIT.       
                                                                        
           IF BJNT-LST-NAME-LEN   GREATER ZEROS                         
               MOVE BJNT-LST-NAME      TO PB-I-JOINT-LAST-NAME.         
                                                                        
           IF BBENEFICIARY-LEN    GREATER ZEROS                         
               MOVE BBENEFICIARY       TO PB-I-BENEFICIARY-NAME.        
                                                                        
           MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.          
           MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.            
                                                                        
       4175-WRITE-PB-RECORD.                                            
           MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY              
                                          PB-INPUT-BY.                  
           MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         
           MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT              
                                          PB-INPUT-DT.                  
                                                                        
           MOVE PB-CONTROL-PRIMARY     TO PB-CONTROL-BY-ORIG-BATCH.     
                                                                        
           MOVE PB-COMPANY-CD          TO PB-CSR-COMPANY-CD.            
           MOVE PI-CSR-ID              TO PB-CSR-ID.                    
           MOVE PB-ENTRY-BATCH         TO PB-CSR-ENTRY-BATCH.           
           MOVE PB-BATCH-SEQ-NO        TO PB-CSR-BATCH-SEQ-NO.          
           MOVE PB-BATCH-CHG-SEQ-NO    TO PB-CSR-BATCH-CHG-SEQ-NO.      
                                                                        
           MOVE 'A'                    TO JP-RECORD-TYPE.               
           MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
           MOVE PB-CONTROL-PRIMARY     TO PI-SAV-ENDING-ERPNDB-KEY      
                                          ERPNDM-KEY.                   
                                                                        
      *    MOVE PI-SAV-REFERENCE       TO PB-I-REFERENCE.               
           ADD +1                      TO PI-SAV-BATCH-SEQ.             
                                                                        
           move pb-control-by-account  to elcrtt-key
           EXEC CICS HANDLE CONDITION                                   
               DUPREC (4200-DUPLICATE-ALT-INDEX)                        
           END-EXEC.                                                    
                                                                        
           EXEC CICS WRITE                                              
               DATASET (FILE-ID-ERPNDB)                                 
               FROM    (PENDING-BUSINESS)                               
               RIDFLD  (PB-CONTROL-PRIMARY)                             
           END-EXEC.                                                    
                                                                        
           ADD +1                      TO PI-ISS-CNT-ENTERED.           
                                                                        
           MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
           MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
           PERFORM 8400-LOG-JOURNAL-RECORD.                             
                                                                        
           MOVE PI-NEXT-DISPLAY-SEQ-NO TO BSEQ.                         
           MOVE AL-SABON               TO BSEQ-ATTRB.                   
                                                                        
           ADD +1                      TO PI-NEXT-DISPLAY-SEQ-NO.       

           if (byear-len      <> zeros)
              or (bmake-len   <> zeros)
              or (bmodel-len  <> zeros)
              or (bometer-len <> zeros)
              move pb-control-by-account
                                       to elcrtt-key
              move 'C'                 to elcrtt-trlr-type
              perform 4910-read-elcrtt-update
                                       thru 4910-exit
              if resp-normal
                 perform 4920-rewrite-elcrtt
                                       thru 4920-exit
              else
                 if resp-notfnd
                    perform 4930-write-elcrtt
                                       thru 4930-exit
                 end-if
              end-if
           end-if



      ******************************************************************
      *    CHECK THE FIRST ISSUE RECORD IN EVERY NEW BATCH.  VERIFY    *
      *    THAT THE CERTIFICATE DOES NOT EXIST ON THE CERT. MASTER     *
      *    FILE.  IF IT DOES DISPLAY WARNING MESSAGE ON BLANK SCREEN.  *
      ******************************************************************
                                                                        
           IF  PI-MAINT-FUNC = 'N' NEXT SENTENCE                        
              ELSE                                                      
               GO TO 4185-ADD-MAILING-RECORD.                           
                                                                        
           IF  PI-ISSUE-ADDED                                           
               GO TO 4185-ADD-MAILING-RECORD.                           
                                                                        
           MOVE 'Y'                    TO  PI-ISSUE-ADDED-SW.           
                                                                        
           EXEC CICS HANDLE CONDITION                                   
               NOTFND (4185-ADD-MAILING-RECORD)                         
           END-EXEC.                                                    
                                                                        
           MOVE PB-CONTROL-BY-ACCOUNT  TO  ELCERT-KEY.                  
           MOVE PI-SAV-FC-CARRIER      TO  ELCERT-CARRIER.              
           MOVE PI-SAV-FC-GROUPING     TO  ELCERT-GROUPING.             
           MOVE PI-SAV-FC-STATE        TO  ELCERT-STATE.                
                                                                        
           EXEC CICS READ                                               
               SET     (ADDRESS OF CERTIFICATE-MASTER)                  
               DATASET (FILE-ID-ELCERT)                                 
               RIDFLD  (ELCERT-KEY)                                     
               LENGTH  (ELCERT-RECORD-LENGTH)                           
               UPDATE                                                   
           END-EXEC.                                                    
                                                                        
           IF  CERT-WAS-CREATED-FOR-CLAIM                               
               GO TO 4185-ADD-MAILING-RECORD.
      
           go to 4900-exit
           .
       4185-ADD-MAILING-RECORD.                                         
                                                                        
           IF  PI-MAIL-YES                                              
                                                                        
               IF  BADDRS1-LEN > ZERO                             
                       OR                                               
      
                   BBENEFICIARY-LEN > ZERO
                        OR
                   BADDRS2-LEN > ZERO                             
                       OR                                               
                   BCITY-LEN > ZERO
                       OR
                   BSTATE-LEN > ZERO
                       OR
                   BCADDR1-LEN > ZERO
                       OR
                   BCADDR2-LEN > ZERO
                       OR
                   BCCITY-LEN > ZERO
                       OR
                   BCSTATE-LEN > ZERO
                   NEXT SENTENCE                                        
                                                                        
               ELSE                                                     
                   GO TO 4900-EXIT                                      
                                                                        
           ELSE                                                         
               GO TO 4900-EXIT.                                         
                                                                        
           EXEC CICS GETMAIN                                            
               SET     (ADDRESS OF PENDING-MAILING-DATA)                
               LENGTH  (ERPNDM-RECORD-LENGTH)                           
               INITIMG (GETMAIN-SPACE)                                  
           END-EXEC.                                                    
                                                                        
           MOVE 'PM'                   TO PM-RECORD-ID.                 
           MOVE 'ER'                   TO PM-SOURCE-SYSTEM.             
                                                                        
           MOVE PI-PROCESSOR-ID        TO PM-LAST-MAINT-BY              
                                          PM-RECORD-ADDED-BY.           
           MOVE EIBTIME                TO PM-LAST-MAINT-HHMMSS.         
           MOVE WS-CURRENT-BIN-DT      TO PM-LAST-MAINT-DT              
                                          PM-RECORD-ADD-DT.             
                                                                        
           MOVE ERPNDM-KEY             TO PM-CONTROL-PRIMARY.           
                                                                        
           IF BLAST-NAME-LEN      GREATER ZEROS                         
               MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.         
                                                                        
           IF B1ST-NAME-LEN       GREATER ZEROS                         
               MOVE B1ST-NAME          TO PM-INSURED-FIRST-NAME.        
                                                                        
           IF BINIT-LEN           GREATER ZEROS                         
               MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.       
                                                                        
           IF BAGE-LEN            GREATER ZEROS                         
               MOVE WS-BAGE            TO PM-INSURED-ISSUE-AGE          
           ELSE                                                         
               MOVE ZEROS              TO PM-INSURED-ISSUE-AGE.         
                                                                        
              MOVE LOW-VALUES          TO PM-INSURED-BIRTH-DT.          
                                                                        
              MOVE LOW-VALUES          TO PM-JOINT-BIRTH-DT
      
      
           IF BLAST-NAME-LEN      GREATER ZEROS                         
               MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.         
                                                                        
           IF BINIT-LEN           GREATER ZEROS                         
               MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.       
                                                                        
           IF BADDRS1-LEN         GREATER ZERO                          
               MOVE BADDRS1            TO PM-ADDRESS-LINE-1.            
                                                                        
           IF BADDRS2-LEN         GREATER ZERO                          
               MOVE BADDRS2            TO PM-ADDRESS-LINE-2.            
                                                                        
           IF BCITY-LEN > 0
              MOVE BCITY               TO PM-CITY
           END-IF
      
           IF BSTATE-LEN > 0
              MOVE BSTATE              TO PM-STATE
           END-IF
                                                                        
           IF BZIPCDE-LEN GREATER ZEROS                                 
               MOVE BZIPCDE            TO  WS-ZIP-CODE                  
           ELSE                                                         
               GO TO 4188-CRED-BENE.                                    
                                                                        
           IF WS-CANADIAN-ZIP                                           
               IF WS-ZIP-4 = SPACE  OR  '-'                             
                   MOVE WS-ZIP-CAN-2-POST1   TO PM-CAN-POST1            
                   MOVE WS-ZIP-CAN-2-POST2   TO PM-CAN-POST2            
               ELSE                                                     
                   MOVE WS-ZIP-CAN-1-POST1   TO PM-CAN-POST1            
                   MOVE WS-ZIP-CAN-1-POST2   TO PM-CAN-POST2            
           ELSE                                                         
               IF WS-ZIP-6 = SPACE  OR  '-'                             
                   MOVE WS-ZIP-AM-2-CODE     TO PM-ZIP-CODE             
                   MOVE WS-ZIP-AM-2-PLUS4    TO PM-ZIP-PLUS4            
               ELSE                                                     
                   MOVE WS-ZIP-AM-1-CODE     TO PM-ZIP-CODE             
                   MOVE WS-ZIP-AM-1-PLUS4    TO PM-ZIP-PLUS4.           
                                                                        
           .
       4188-CRED-BENE.
      
      
           IF BBENEFICIARY-LEN > ZEROS
              MOVE BBENEFICIARY        TO PM-CRED-BENE-NAME
           END-IF                      
           IF BCADDR1-LEN > ZEROS      
              MOVE BCADDR1             TO PM-CRED-BENE-ADDR
           END-IF                      
           IF BCADDR2-LEN > ZEROS      
              MOVE BCADDR2             TO PM-CRED-BENE-ADDR2
           END-IF                      
           IF BCCITY-LEN > ZEROS      
              MOVE BCCITY              TO PM-CRED-BENE-CITY
           END-IF
           IF BCSTATE-LEN > ZEROS      
              MOVE BCSTATE             TO PM-CRED-BENE-STATE
           END-IF
      
           IF BCZIPCD-LEN > ZEROS                                 
              MOVE BCZIPCD             TO WS-ZIP-CODE
           ELSE                                                         
              GO TO 4188-CONTINUE
           END-IF
      
           IF WS-CANADIAN-ZIP                                           
              IF WS-ZIP-4 = SPACE  OR  '-'                             
                 MOVE WS-ZIP-CAN-2-POST1   TO PM-CB-CAN-POST1
                 MOVE WS-ZIP-CAN-2-POST2   TO PM-CB-CAN-POST2
              ELSE                                                     
                 MOVE WS-ZIP-CAN-1-POST1   TO PM-CB-CAN-POST1
                 MOVE WS-ZIP-CAN-1-POST2   TO PM-CB-CAN-POST2
              END-IF
           ELSE                                                         
              IF WS-ZIP-6 = SPACE  OR  '-'                             
                 MOVE WS-ZIP-AM-2-CODE     TO PM-CB-ZIP-CODE
                 MOVE WS-ZIP-AM-2-PLUS4    TO PM-CB-ZIP-PLUS4
              ELSE                                                     
                 MOVE WS-ZIP-AM-1-CODE     TO PM-CB-ZIP-CODE
                 MOVE WS-ZIP-AM-1-PLUS4    TO PM-CB-ZIP-PLUS4
              END-IF
           END-IF
      
           .
       4188-CONTINUE.                                                   
                                                                        
      *    IF BPHONE-LEN          GREATER ZERO                          
      *        MOVE WS-BPHONE          TO PM-PHONE-NO                   
      *    ELSE                                                         
               MOVE ZEROS              TO PM-PHONE-NO.                  
                                                                        
           MOVE 'A'                    TO JP-RECORD-TYPE.               
           MOVE PENDING-MAILING-DATA   TO JP-RECORD-AREA.               
           MOVE ERPNDM-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
           MOVE FILE-ID-ERPNDM         TO JP-FILE-ID.                   
                                                                        
           EXEC CICS WRITE                                              
               DATASET (FILE-ID-ERPNDM)                                 
               FROM    (PENDING-MAILING-DATA)                           
               RIDFLD  (PM-CONTROL-PRIMARY)                             
           END-EXEC.                                                    
                                                                        
           PERFORM 8400-LOG-JOURNAL-RECORD.                             
                                                                        
           MOVE LOW-VALUES             TO MAP-B.                        
                                                                        
           GO TO 4900-EXIT.                                             
                                                                        
       4200-DUPLICATE-ALT-INDEX.                                        
           MOVE ER-2247                TO EMI-ERROR.                    
           MOVE -1                     TO BCERT-LEN.                    
           MOVE AL-UABON               TO BCERT-ATTRB.                  
           MOVE AL-UNBON               TO BEFFDT-ATTRB.                 
           MOVE 'Y'                    TO PI-ERROR-SW.                  
                                                                        
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     
                                                                        
           IF BPREM1-LEN > ZEROS                               
               SUBTRACT WS-BPREM1                                       
                                       FROM PI-LF-ISS-ENTERED.          
                                                                        
           IF BALT-PREM1-LEN > ZEROS                           
               SUBTRACT WS-BALT-PREM1                                   
                                       FROM PI-LF-ISS-ENTERED.          
                                                                        
           IF BPREM2-LEN > ZEROS                               
               SUBTRACT WS-BPREM2                                       
                                       FROM PI-AH-ISS-ENTERED.          
                                                                        
           SUBTRACT +1                 FROM PI-LAST-SEQ-NO-ADDED        
                                            PI-SAV-BATCH-SEQ.           
                                                                        
       4900-EXIT.                                                       
           EXIT.                                                        

       4905-read-elcrtt.

           EXEC CICS READ
               SET     (ADDRESS OF certificate-trailers)
               DATASET ('ELCRTT')
               RIDFLD  (ELCRTT-KEY)
               resp    (ws-response)
           END-EXEC

           .
       4905-exit.
           exit.

       4910-read-elcrtt-update.

           EXEC CICS READ
               SET     (ADDRESS OF certificate-trailers)
               DATASET ('ELCRTT')
               RIDFLD  (ELCRTT-KEY)
               UPDATE
               resp    (ws-response)
           END-EXEC

           .
       4910-exit.
           exit.

       4920-rewrite-elcrtt.

           perform 4940-update-elcrtt  thru 4940-exit

           exec cics rewrite
              dataset     ('ELCRTT')
              from        (certificate-trailers)
              resp        (ws-response)
           end-exec

           .
       4920-exit.
           exit.

       4930-write-elcrtt.

           move 'CS'                   to certificate-trailers
           move elcrtt-key             to cs-control-primary
           move 'C'                    to cs-trailer-type
           perform 4940-update-elcrtt  thru 4940-exit

           EXEC CICS WRITE                                              
               DATASET ('ELCRTT')
               FROM    (CERTIFICATE-TRAILERS)
               RIDFLD  (CS-CONTROL-PRIMARY)
           END-EXEC

           .
       4930-exit.
           exit.

       4940-update-elcrtt.

           if byear-len <> zeros
              move byear               to cs-year
           end-if
           if bmake-len <> zeros
              move bmake               to cs-make
           end-if
           if bmodel-len <> zeros
              move bmodel              to cs-model
           end-if

           if bometer-len <> zeros
              MOVE Bometer-in          TO DEEDIT-FIELD
              PERFORM 8600-DEEDIT                                    
              IF DEEDIT-FIELD-V0 NUMERIC                             
                 MOVE DEEDIT-FIELD-V0  TO cs-vehicle-odometer
              end-if
           end-if

           .
       4940-exit.
           exit.

       5000-BUILD-CANCEL-RECORD.                                        
           MOVE +0                     TO WS-SUB2.                      
                                                                        
       5025-PROCESS-CANCEL.                                             
           ADD +1                      TO WS-SUB2.                      
                                                                        
           IF PI-LAST-FUNC-DISPLAY                                      
              IF WS-SUB2 GREATER +1                                     
                 GO TO 5900-EXIT.                                       
                                                                        
           IF WS-SUB2 GREATER +4                                        
              GO TO 5900-EXIT.                                          
                                                                        
           IF CCERT-LEN    (WS-SUB2) = ZEROS AND                        
              CEFFDT-LEN   (WS-SUB2) = ZEROS AND                        
              CCANDT1-LEN  (WS-SUB2) = ZEROS AND                        
              CCANDT2-LEN  (WS-SUB2) = ZEROS AND                        
              CMTHD1-LEN   (WS-SUB2) = ZEROS AND                        
              CMTHD2-LEN   (WS-SUB2) = ZEROS AND                        
              CREFUND1-LEN (WS-SUB2) = ZEROS AND                        
              CREFUND2-LEN (WS-SUB2) = ZEROS AND                        
              CLIVES-LEN   (WS-SUB2) = ZEROS AND                        
              CCHK-LEN     (WS-SUB2) = ZEROS                            
                 GO TO 5025-PROCESS-CANCEL.                             
                                                                        
           IF CSEQ (WS-SUB2) GREATER PI-LAST-SEQ-NO-ADDED               
               GO TO 5100-ADD-CANCEL-RECORD.                            
                                                                        
      ******************************************************************
      *   THE DATA ENTRY SYSTEM ALLOWS BROWSING OF THE CURRENT BUS.    *
      *   FILE. THE DATA ENTRY SYS. DOES NOT HAVE A MAINT. FUNCTION.   *
      *   THE PROGRAM ASSUMES THAT IF A MATCH ON THE READ FOR UPDATE   *
      *   IS SUCCESSFUL, THE RECORD HAS PREVIOUSLY BEEN DISPLAYED      *
      *   THROUGH A BROWSE.  CHANGES ARE APPLIED AND THE PB-RECORD IS  *
      *   REWRITTEN, ELSE A NEW PB-RECORD IS ADDED.                    *
      ******************************************************************
                                                                        
           MOVE PI-COMPANY-CD          TO ERPNDB-COMP-CD.               
           MOVE PI-SAV-ENTRY-BATCH     TO ERPNDB-ENTRY-BATCH.           
           MOVE CSEQ (WS-SUB2)         TO ERPNDB-BATCH-SEQ.             
                                                                        
           EXEC CICS HANDLE CONDITION                                   
               NOTFND (5100-ADD-CANCEL-RECORD)                          
           END-EXEC.                                                    
                                                                        
           EXEC CICS READ                                               
               SET     (ADDRESS OF PENDING-BUSINESS)                    
               DATASET (FILE-ID-ERPNDB)                                 
               RIDFLD  (ERPNDB-KEY)                                     
               UPDATE                                                   
           END-EXEC.                                                    
                                                                        
           MOVE 'B'                    TO JP-RECORD-TYPE                
           MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
           MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
           MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
                                                                        
           PERFORM 8400-LOG-JOURNAL-RECORD.                             
                                                                        
           MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.             
           MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         
           MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT.             
                                                                        
           IF CSFX-LEN  (WS-SUB2) GREATER ZEROS                         
              MOVE CSFX (WS-SUB2)      TO PB-CERT-SFX.                  
                                                                        
           IF CLAST-NAME-LEN (WS-SUB2) GREATER ZEROS                    
               MOVE CLAST-NAME (WS-SUB2)  TO PB-C-LAST-NAME.            
                                                                        
           IF CREFUND1-LEN   (WS-SUB2) GREATER ZEROS                    
               IF WS-CREFUND1 (WS-SUB2) = WS-ALL-NINES OR               
                  WS-CREFUND1 (WS-SUB2) GREATER WS-ALL-NINES            
                  SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED    
                  MOVE ZEROS            TO PB-C-LF-CANCEL-AMT           
                  MOVE '?'              TO PB-C-LF-CALC-REQ             
               ELSE                                                     
                  SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED    
                  ADD WS-CREFUND1  (WS-SUB2) TO PI-LF-CAN-ENTERED       
                  MOVE WS-CREFUND1 (WS-SUB2) TO PB-C-LF-CANCEL-AMT      
                  MOVE SPACE                 TO PB-C-LF-CALC-REQ.       
                                                                        
           IF CREFUND2-LEN   (WS-SUB2) GREATER ZEROS                    
               IF WS-CREFUND2 (WS-SUB2) = WS-ALL-NINES OR               
                  WS-CREFUND2 (WS-SUB2) GREATER WS-ALL-NINES            
                  SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED    
                  MOVE ZEROS            TO PB-C-AH-CANCEL-AMT           
                  MOVE '?'              TO PB-C-AH-CALC-REQ             
               ELSE                                                     
                  SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED    
                  ADD WS-CREFUND2  (WS-SUB2) TO PI-AH-CAN-ENTERED       
                  MOVE WS-CREFUND2 (WS-SUB2) TO PB-C-AH-CANCEL-AMT      
                  MOVE SPACE                 TO PB-C-AH-CALC-REQ.       
                                                                        
      ******************************************************************
      *      IF CANCEL DATE = SPACES (LOW-VALUES) DELETE COVERAGE.     *
      ******************************************************************
                                                                        
           IF CCANDT1-LEN (WS-SUB2) GREATER ZEROS                       
              MOVE WS-CONVERTED-CANDT1 (WS-SUB2) TO PB-C-LF-CANCEL-DT   
              IF   WS-CONVERTED-CANDT1 (WS-SUB2) = LOW-VALUES           
                   SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED   
                   MOVE ZEROS          TO PB-C-LF-REF-CALC              
                                          PB-C-LF-CANCEL-AMT.           
                                                                        
           IF CCANDT2-LEN (WS-SUB2) GREATER ZEROS                       
              MOVE WS-CONVERTED-CANDT2 (WS-SUB2) TO PB-C-AH-CANCEL-DT   
              IF   WS-CONVERTED-CANDT2 (WS-SUB2) = LOW-VALUES           
                   SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED   
                   MOVE ZEROS          TO PB-C-AH-REF-CALC              
                                          PB-C-AH-CANCEL-AMT.           
                                                                        
           IF CMTHD1-LEN (WS-SUB2) GREATER THAN +0                      
              MOVE CMTHD1 (WS-SUB2)    TO PB-C-LF-REFUND-OVERRIDE.      
                                                                        
           IF CMTHD2-LEN (WS-SUB2) GREATER THAN +0                      
              MOVE CMTHD2 (WS-SUB2)    TO PB-C-AH-REFUND-OVERRIDE.      
                                                                        
           IF CLIVES-LEN  (WS-SUB2) GREATER ZEROS                       
              MOVE WS-CLIVES (WS-SUB2) TO PB-C-LIVES.                   
                                                                        
           IF CCANREA-LEN (WS-SUB2) > ZEROS
              MOVE WS-CAN-REA (WS-SUB2) TO PB-C-CANCEL-REASON
           END-IF
      
      *    IF CMICRO-NO-LEN (WS-SUB2)  GREATER  ZEROS                   
      *        MOVE WS-MICRO-NO (WS-SUB2)                               
      *                                TO  PB-C-MICROFILM-NO.           
                                                                        
           IF CPAYEE-LEN  (WS-SUB2) GREATER ZEROS                       
              MOVE CPAYEE (WS-SUB2)    TO PB-C-PAYEE-CODE.              
                                                                        
           IF CCHK-LEN    (WS-SUB2) GREATER ZEROS                       
              MOVE CCHK   (WS-SUB2)    TO PB-C-REFUND-SW.               
                                                                        
           MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.          
           MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.            
                                                                        
           MOVE 'C'                    TO JP-RECORD-TYPE.               
           MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
           MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
           MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
                                                                        
           EXEC CICS REWRITE                                            
               DATASET (FILE-ID-ERPNDB)                                 
               FROM    (PENDING-BUSINESS)                               
           END-EXEC.                                                    
                                                                        
           MOVE ERPNDB-KEY             TO PI-SAV-ENDING-ERPNDB-KEY.     
                                                                        
           PERFORM 8400-LOG-JOURNAL-RECORD.                             
                                                                        
           MOVE LOW-VALUES             TO DATA-AREA-C (WS-SUB2).        
                                                                        
           IF EIBAID = DFHENTER                                         
               MOVE PI-NEXT-DISPLAY-SEQ-NO TO CSEQ (WS-SUB2)            
               ADD +1 TO PI-NEXT-DISPLAY-SEQ-NO                         
               MOVE AL-SABON               TO CSEQ-ATTRB (WS-SUB2).     
                                                                        
           GO TO 5900-EXIT.                                             
                                                                        
           EJECT                                                        
                                                                        
       5100-ADD-CANCEL-RECORD.                                          
           EXEC CICS GETMAIN                                            
               SET     (ADDRESS OF PENDING-BUSINESS)                    
               LENGTH  (ERPNDB-RECORD-LENGTH)                           
               INITIMG (GETMAIN-SPACE)                                  
           END-EXEC.                                                    
                                                                        
           MOVE 'PB'                   TO PB-RECORD-ID.                 
           MOVE PI-COMPANY-CD          TO PB-COMPANY-CD                 
                                          PB-COMPANY-CD-A1.             
           MOVE PI-COMPANY-ID          TO PB-COMPANY-ID.                
           MOVE PI-SAV-ENTRY-BATCH     TO PB-ENTRY-BATCH.               
           MOVE CSEQ (WS-SUB2)         TO PB-BATCH-SEQ-NO.              
                                                                        
           IF CSEQ (WS-SUB2) GREATER PI-LAST-SEQ-NO-ADDED               
              MOVE CSEQ (WS-SUB2)      TO PI-LAST-SEQ-NO-ADDED.         
                                                                        
           MOVE PI-SAV-CARRIER         TO PB-CARRIER.                   
           MOVE PI-SAV-GROUPING        TO PB-GROUPING.                  
           MOVE PI-SAV-STATE           TO PB-STATE.                     
           MOVE PI-SAV-ACCOUNT         TO PB-ACCOUNT.                   
           MOVE '2'                    TO PB-RECORD-TYPE.               
           MOVE CCERT (WS-SUB2)        TO PB-CERT-PRIME.                
           MOVE WS-CONVERTED-CAN-EFF-DT (WS-SUB2)                       
                                       TO PB-CERT-EFF-DT.               
                                                                        
      *    MOVE PI-SAV-REFERENCE       TO PB-C-REFERENCE.               
                                                                        
           MOVE ZEROS                  TO PB-BATCH-CHG-SEQ-NO           
                                          PB-ALT-CHG-SEQ-NO.            
                                                                        
           MOVE +0                     TO PB-NO-OF-ERRORS.              
                                                                        
           MOVE LOW-VALUES             TO PB-COMMON-ERRORS.             
                                                                        
           MOVE ZEROS                  TO PB-C-LF-REF-CALC              
                                          PB-C-AH-REF-CALC              
                                          PB-C-LF-RFND-CLP
                                          PB-C-AH-RFND-CLP
                                          PB-CI-INSURED-AGE             
                                          PB-CI-LF-TERM                 
                                          PB-CI-AH-TERM                 
                                          PB-CI-LF-BENEFIT-CD           
                                          PB-CI-LF-BENEFIT-AMT          
                                          PB-CI-LF-ALT-BENEFIT-AMT      
                                          PB-CI-LF-PREMIUM-AMT          
                                          PB-CI-LF-ALT-PREMIUM-AMT      
                                          PB-CI-AH-BENEFIT-CD           
                                          PB-CI-AH-BENEFIT-AMT          
                                          PB-CI-AH-PREMIUM-AMT          
                                          PB-CI-PAY-FREQUENCY           
                                          PB-CI-LOAN-APR                
                                          PB-CI-LOAN-TERM               
                                          PB-CI-LIFE-COMMISSION         
                                          PB-CI-AH-COMMISSION           
                                          PB-CI-CURR-SEQ                
                                          PB-CI-AH-CANCEL-AMT           
                                          PB-CI-LF-CANCEL-AMT           
                                          PB-CI-RATE-DEV-PCT-LF         
                                          PB-CI-RATE-DEV-PCT-AH         
                                          PB-CI-EXTENTION-DAYS          
                                          PB-CI-TERM-IN-DAYS            
                                          PB-CI-LIVES                   
                                          PB-CI-LF-CRIT-PER             
                                          PB-CI-AH-CRIT-PER             
                                          PB-C-LF-REM-TERM              
                                          PB-C-AH-REM-TERM              
                                          PB-CHG-COUNT                  
                                          PB-LF-BILLED-AMTS             
                                          PB-AH-BILLED-AMTS             
      *                                   PB-C-MICROFILM-NO             
062017                                    PB-C-INT-ON-REFS
                                          PB-CALC-TOLERANCE.            
                                                                        
           MOVE LOW-VALUES             TO PB-CI-AH-PAID-THRU-DT         
                                          PB-CI-AH-SETTLEMENT-DT        
                                          PB-CI-DEATH-DT                
                                          PB-CI-LF-PRIOR-CANCEL-DT      
                                          PB-CI-AH-PRIOR-CANCEL-DT      
                                          PB-CI-ENTRY-DT                
                                          PB-CI-LF-EXPIRE-DT            
                                          PB-CI-AH-EXPIRE-DT            
                                          PB-CI-LOAN-1ST-PMT-DT         
                                          PB-C-LF-CANCEL-DT             
                                          PB-C-AH-CANCEL-DT             
                                          PB-CREDIT-ACCEPT-DT           
                                          PB-BILLED-DT                  
                                          PB-ACCT-EFF-DT                
                                          PB-ACCT-EXP-DT.               
                                                                        
           IF PI-NB-MONTH-END-DT NOT = SPACES                           
              MOVE PI-NB-MONTH-END-DT  TO PB-CREDIT-SELECT-DT           
             ELSE                                                       
              MOVE PI-CR-MONTH-END-DT  TO PB-CREDIT-SELECT-DT.          
                                                                        
           MOVE 'X'                    TO PB-FATAL-FLAG.                
                                                                        
           IF CSFX-LEN  (WS-SUB2) GREATER ZEROS                         
              MOVE CSFX (WS-SUB2)      TO PB-CERT-SFX.                  
                                                                        
           IF CLAST-NAME-LEN (WS-SUB2) GREATER ZEROS                    
               MOVE CLAST-NAME (WS-SUB2)  TO PB-C-LAST-NAME.            
                                                                        
           IF CCANDT1-LEN (WS-SUB2) GREATER ZEROS                       
              MOVE WS-CONVERTED-CANDT1 (WS-SUB2) TO PB-C-LF-CANCEL-DT.  
                                                                        
           IF CCANDT2-LEN (WS-SUB2) GREATER ZEROS                       
              MOVE WS-CONVERTED-CANDT2 (WS-SUB2) TO PB-C-AH-CANCEL-DT.  
                                                                        
           IF CMTHD1-LEN (WS-SUB2) GREATER ZEROS                        
              MOVE CMTHD1 (WS-SUB2) TO PB-C-LF-REFUND-OVERRIDE.         
                                                                        
           IF CMTHD2-LEN (WS-SUB2) GREATER ZEROS                        
              MOVE CMTHD2 (WS-SUB2) TO PB-C-AH-REFUND-OVERRIDE.         
                                                                        
           IF CREFUND1-LEN   (WS-SUB2) GREATER ZEROS                    
               IF WS-CREFUND1 (WS-SUB2) = WS-ALL-NINES OR               
                  WS-CREFUND1 (WS-SUB2) GREATER WS-ALL-NINES            
                  MOVE ZEROS            TO PB-C-LF-CANCEL-AMT           
                  MOVE '?'              TO PB-C-LF-CALC-REQ             
               ELSE                                                     
                  ADD  WS-CREFUND1  (WS-SUB2) TO PI-LF-CAN-ENTERED      
                  MOVE WS-CREFUND1  (WS-SUB2) TO PB-C-LF-CANCEL-AMT     
           ELSE                                                         
               MOVE ZEROS            TO PB-C-LF-CANCEL-AMT.             
                                                                        
           IF CREFUND2-LEN   (WS-SUB2) GREATER ZEROS                    
               IF WS-CREFUND2 (WS-SUB2) = WS-ALL-NINES OR               
                  WS-CREFUND2 (WS-SUB2) GREATER WS-ALL-NINES            
                  MOVE ZEROS            TO PB-C-AH-CANCEL-AMT           
                  MOVE '?'              TO PB-C-AH-CALC-REQ             
               ELSE                                                     
                  ADD  WS-CREFUND2  (WS-SUB2) TO PI-AH-CAN-ENTERED      
                  MOVE WS-CREFUND2  (WS-SUB2) TO PB-C-AH-CANCEL-AMT     
           ELSE                                                         
               MOVE ZEROS              TO PB-C-AH-CANCEL-AMT.           
                                                                        
           IF CLIVES-LEN  (WS-SUB2) GREATER ZEROS                       
              MOVE WS-CLIVES (WS-SUB2) TO PB-C-LIVES                    
           ELSE                                                         
              MOVE ZEROS               TO PB-C-LIVES.                   
                                                                        
           IF CCANREA-LEN (WS-SUB2) > ZEROS
              MOVE WS-CAN-REA (WS-SUB2) TO PB-C-CANCEL-REASON
           END-IF
      
      *    IF CMICRO-NO-LEN (WS-SUB2)  GREATER  ZEROS                   
      *        MOVE WS-MICRO-NO (WS-SUB2)                               
      *                                TO  PB-C-MICROFILM-NO            
      *    ELSE                                                         
      *        MOVE ZEROS              TO  PB-C-MICROFILM-NO.           
                                                                        
           IF CPAYEE-LEN  (WS-SUB2) GREATER ZEROS                       
              MOVE CPAYEE (WS-SUB2)    TO PB-C-PAYEE-CODE.              
                                                                        
           IF CCHK-LEN    (WS-SUB2) GREATER ZEROS                       
              MOVE CCHK   (WS-SUB2)    TO PB-C-REFUND-SW.               
                                                                        
           IF PB-COMPANY-ID = 'LAP'  OR  'RMC'                          
               MOVE 'H'                TO PB-RECORD-BILL.               
                                                                        
           MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.          
           MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.            
                                                                        
           MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY              
                                          PB-INPUT-BY.                  
           MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         
           MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT              
                                          PB-INPUT-DT.                  
                                                                        
           MOVE PB-CONTROL-PRIMARY     TO PB-CONTROL-BY-ORIG-BATCH.     
                                                                        
           MOVE PB-COMPANY-CD          TO PB-CSR-COMPANY-CD.            
           MOVE PI-CSR-ID              TO PB-CSR-ID.                    
           MOVE PB-ENTRY-BATCH         TO PB-CSR-ENTRY-BATCH.           
           MOVE PB-BATCH-SEQ-NO        TO PB-CSR-BATCH-SEQ-NO.          
           MOVE PB-BATCH-CHG-SEQ-NO    TO PB-CSR-BATCH-CHG-SEQ-NO.      
                                                                        
           MOVE 'A'                    TO JP-RECORD-TYPE.               
           MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
           MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
           MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
           MOVE PB-CONTROL-PRIMARY     TO PI-SAV-ENDING-ERPNDB-KEY.     
           ADD +1                      TO PI-SAV-BATCH-SEQ.             
                                                                        
           EXEC CICS HANDLE CONDITION                                   
               DUPREC (5200-DUPLICATE-ALT-INDEX)                        
           END-EXEC.                                                    
                                                                        
           EXEC CICS WRITE                                              
               DATASET (FILE-ID-ERPNDB)                                 
               FROM    (PENDING-BUSINESS)                               
               RIDFLD  (PB-CONTROL-PRIMARY)                             
           END-EXEC.                                                    
                                                                        
           ADD +1                      TO PI-CAN-CNT-ENTERED.           
                                                                        
           PERFORM 8400-LOG-JOURNAL-RECORD.                             
                                                                        
           MOVE LOW-VALUES             TO DATA-AREA-C (WS-SUB2).        
           MOVE PI-NEXT-DISPLAY-SEQ-NO TO CSEQ        (WS-SUB2).        
           MOVE AL-SABON               TO CSEQ-ATTRB  (WS-SUB2).        
                                                                        
           ADD +1 TO                   PI-NEXT-DISPLAY-SEQ-NO.          
                                                                        
           GO TO 5025-PROCESS-CANCEL.                                   
                                                                        
       5200-DUPLICATE-ALT-INDEX.                                        
           MOVE ER-2247                TO EMI-ERROR.                    
           MOVE -1                     TO CCERT-LEN    (WS-SUB2).       
           MOVE AL-UABON               TO CCERT-ATTRB  (WS-SUB2).       
           MOVE AL-UNBON               TO CEFFDT-ATTRB (WS-SUB2).       
           MOVE 'Y'                    TO PI-ERROR-SW.                  
                                                                        
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
                                                                        
           IF CREFUND1-LEN (WS-SUB2) GREATER ZEROS                      
               SUBTRACT WS-CREFUND1 (WS-SUB2) FROM PI-LF-CAN-ENTERED.   
                                                                        
           IF CREFUND2-LEN (WS-SUB2) GREATER ZEROS                      
               SUBTRACT WS-CREFUND2 (WS-SUB2) FROM PI-AH-CAN-ENTERED.   
                                                                        
           GO TO 8200-SEND-DATAONLY.                                    
                                                                        
       5900-EXIT.                                                       
           EXIT.                                                        
                                                                        
           EJECT                                                        
                                                                        
       6000-DELETE-PEND-BUS-RECORD.                                     
           MOVE PI-COMPANY-CD          TO ERPNDB-COMP-CD.               
           MOVE PI-SAV-ENTRY-BATCH     TO ERPNDB-ENTRY-BATCH.           
                                                                        
           IF PI-MAP-NAME = VP630B                                      
               MOVE BSEQ               TO ERPNDB-BATCH-SEQ              
           ELSE                                                         
               MOVE CSEQ (1)           TO ERPNDB-BATCH-SEQ.             
                                                                        
           EXEC CICS HANDLE CONDITION                                   
               NOTFND (6990-REC-NOTFND)                                 
           END-EXEC.                                                    
                                                                        
           EXEC CICS READ                                               
               SET     (ADDRESS OF PENDING-BUSINESS)                    
               DATASET (FILE-ID-ERPNDB)                                 
               RIDFLD  (ERPNDB-KEY)                                     
               UPDATE                                                   
           END-EXEC.                                                    
                                                                        
      ******************************************************************
      *    PENDING BUSINESS RECORD CAN NOT BE DELETED THROUGH DATA     *
      *    ENTRY IF THE RECORD HAS BEEN EDITED.  IF THE RECORD HAS     *
      *    BEEN EDITED, THE CURRENT BUSINESS RECORD CAN ONLY BE DELETED*
      *    THROUGH REVIEW AND CORRECTION.                              *
      ******************************************************************
                                                                        
           IF  PB-ACCT-EFF-DT = LOW-VALUES NEXT SENTENCE                
              ELSE                                                      
               GO TO 6880-DELETE-ERROR.                                 
                                                                        
           IF PB-ISSUE                                                  
               SUBTRACT PB-I-LF-PREMIUM-AMT     FROM PI-LF-ISS-ENTERED  
               SUBTRACT PB-I-LF-ALT-PREMIUM-AMT FROM PI-LF-ISS-ENTERED  
               SUBTRACT PB-I-AH-PREMIUM-AMT     FROM PI-AH-ISS-ENTERED  
               SUBTRACT +1 FROM PI-ISS-CNT-ENTERED                      
           ELSE                                                         
               SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED       
               SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED       
               SUBTRACT +1 FROM PI-CAN-CNT-ENTERED.                     
                                                                        
       6300-DELETE-PB-RECORD.                                           
           MOVE 'D'                    TO JP-RECORD-TYPE.               
           MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
           MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     
           MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.                   
                                                                        
           EXEC CICS DELETE                                             
               DATASET (FILE-ID-ERPNDB)                                 
           END-EXEC.                                                    
                                                                        
           PERFORM 8400-LOG-JOURNAL-RECORD.                             
                                                                        
           MOVE 'Y'                    TO PI-UPDATE-SW.                 
           MOVE ER-0000                TO EMI-ERROR.                    
                                                                        
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
                                                                        
           ADD +1             PI-LAST-SEQ-NO-ADDED                      
                           GIVING PI-NEXT-DISPLAY-SEQ-NO.               
                                                                        
           IF PI-MAP-NAME = VP630B                                      
               MOVE LOW-VALUES         TO MAP-B                         
               PERFORM 8550-SET-MAP-SEQ-NOS                             
           ELSE                                                         
               MOVE SPACE              TO PI-DISPLAY-SW                 
               MOVE LOW-VALUES         TO MAP-C                         
               PERFORM 8550-SET-MAP-SEQ-NOS                             
                       VARYING WS-SUB2 FROM +1 BY +1                    
                       UNTIL WS-SUB2 GREATER +5.                        
                                                                        
           GO TO 8100-SEND-INITIAL-MAP.                                 
                                                                        
       6880-DELETE-ERROR.                                               
           EXEC CICS UNLOCK                                             
                DATASET (FILE-ID-ERPNDB)                                
           END-EXEC.                                                    
                                                                        
           MOVE ER-2901        TO EMI-ERROR.                            
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           IF PI-MAP-NAME = VP630B                                      
               MOVE -1                 TO BPFENTRL                      
           ELSE                                                         
               MOVE -1                 TO CPFENTRL.                     
                                                                        
           GO TO 8200-SEND-DATAONLY.                                    
                                                                        
       6990-REC-NOTFND.                                                 
           MOVE ER-2433                TO EMI-ERROR                     
                                                                        
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
                                                                        
           IF PI-MAP-NAME = VP630B                                      
               MOVE -1                 TO BPFENTRL                      
           ELSE                                                         
               MOVE -1                 TO CPFENTRL.                     
                                                                        
           GO TO 8200-SEND-DATAONLY.                                    
                                                                        
           EJECT                                                        
                                                                        
       7000-FORMAT-ISSUE-SCREEN.                                        
           MOVE 'Y'                        TO PI-DISPLAY-SW.            
           MOVE LOW-VALUES                 TO DATA-AREA-B.              
           MOVE -1                         TO BPFENTRL.                 
           MOVE PB-BATCH-SEQ-NO            TO BSEQ.                     
           MOVE AL-SABON                   TO BSEQ-ATTRB.               
           MOVE PB-CERT-PRIME              TO BCERT.                    
           MOVE AL-SANON                   TO BCERT-ATTRB.              
           MOVE PB-CERT-SFX                TO BSFX.                     
           MOVE AL-SANOF                   TO BSFX-ATTRB.               
           MOVE PB-CERT-EFF-DT             TO DC-BIN-DATE-1.            
           MOVE SPACE                      TO DC-OPTION-CODE.           
           PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
           MOVE DC-GREG-DATE-1-MDY         TO BEFFDT.                   
           MOVE AL-SANON                   TO BEFFDT-ATTRB.             
                                                                        
           MOVE PI-LIFE-OVERRIDE-L2        TO BKIND1
           MOVE PI-AH-OVERRIDE-L2          TO BKIND2
                                                                        
           IF PB-I-INSURED-LAST-NAME GREATER SPACES                     
              MOVE PB-I-INSURED-LAST-NAME  TO BLAST-NAME.               
                                                                        
           IF PB-I-INSURED-FIRST-NAME GREATER SPACES                    
              MOVE PB-I-INSURED-FIRST-NAME TO B1ST-NAME.                
                                                                        
           IF PB-I-INSURED-MIDDLE-INIT GREATER SPACES                   
              MOVE PB-I-INSURED-MIDDLE-INIT TO BINIT.                   
                                                                        
           IF PB-I-AGE GREATER ZEROS                                    
              MOVE PB-I-AGE                TO BAGE.                     
                                                                        
           IF PB-I-JOINT-AGE GREATER ZEROS                              
              MOVE PB-I-JOINT-AGE          TO BJNT-AGE.                 
                                                                        
      
           IF PB-I-INSURED-SEX GREATER SPACES                           
              MOVE PB-I-INSURED-SEX        TO BSEX.                     
                                                                        
           IF PB-I-LF-TERM GREATER ZEROS                                
              MOVE PB-I-LF-TERM            TO BTERM1O.               
                                                                        
           IF PB-I-AH-TERM GREATER ZEROS                                
              MOVE PB-I-AH-TERM            TO BTERM2O.               
      
           IF PB-I-LF-INPUT-CD GREATER SPACES                           
              MOVE PB-I-LF-INPUT-CD        TO BTYPE1.                
                                                                        
           IF PB-I-LF-BENEFIT-AMT GREATER ZEROS                         
              MOVE PB-I-LF-BENEFIT-AMT     TO BBENE1O.                
                                                                        
           IF PB-I-LF-ALT-BENEFIT-AMT GREATER ZEROS                     
              MOVE PB-I-LF-ALT-BENEFIT-AMT TO BALT-BEN1O.            
                                                                        
           IF PB-I-LF-PREMIUM-AMT GREATER ZEROS                         
              MOVE PB-I-LF-PREMIUM-AMT     TO BPREM1O.               
                                                                        
           IF PB-I-LF-ALT-PREMIUM-AMT GREATER ZEROS                     
              MOVE PB-I-LF-ALT-PREMIUM-AMT TO BALT-PREM1O.           
                                                                        
           IF PB-I-AH-INPUT-CD GREATER SPACES                           
              MOVE PB-I-AH-INPUT-CD        TO BTYPE2.                
                                                                        
           IF PB-I-AH-BENEFIT-AMT GREATER ZEROS                         
              MOVE PB-I-AH-BENEFIT-AMT     TO BBENE2O.                
                                                                        
           IF PB-I-AH-PREMIUM-AMT GREATER ZEROS                         
              MOVE PB-I-AH-PREMIUM-AMT     TO BPREM2O.               
                                                                        
                                                                        
           IF PB-I-AH-CRIT-PER GREATER ZEROS                            
              MOVE PB-I-AH-CRIT-PER           TO BCRIT-PERD2O.       
                                                                        
           IF PB-I-LOAN-OFFICER GREATER SPACES                          
              MOVE PB-I-LOAN-OFFICER       TO BLN-OFFICER.              
                                                                        
                                                                        
           IF PB-I-VIN > ZEROS
              MOVE PB-I-VIN            TO BVIN-NOI
           END-IF
      
                                                                        
           IF PB-I-JOINT-FIRST-NAME GREATER SPACES                      
              MOVE PB-I-JOINT-FIRST-NAME   TO BJNT-1ST-NAME.            
                                                                        
           IF PB-I-JOINT-MIDDLE-INIT GREATER SPACES                     
              MOVE PB-I-JOINT-MIDDLE-INIT  TO BJNT-INIT.                
                                                                        
           IF PB-I-JOINT-LAST-NAME GREATER SPACES                       
              MOVE PB-I-JOINT-LAST-NAME    TO BJNT-LST-NAME.            
                                                                        
           IF PB-I-BENEFICIARY-NAME GREATER SPACES                      
              MOVE PB-I-BENEFICIARY-NAME   TO BBENEFICIARY.             
                                                                        
           move pb-control-by-account  to elcrtt-key
           move 'C'                    to elcrtt-trlr-type
           perform 4905-read-elcrtt    thru 4905-exit
           if resp-normal
              if cs-year numeric
                 move cs-year          to byearo
              end-if
              move cs-make             to bmakeo
              move cs-model            to bmodelo
              if cs-vehicle-odometer numeric
                 move cs-vehicle-odometer
                                       to bometer-out
              end-if
           end-if

           MOVE PB-CONTROL-PRIMARY         TO ERPNDM-KEY.               
                                                                        
           EXEC CICS HANDLE CONDITION                                   
               NOTFND (7090-EXIT)                                       
           END-EXEC.                                                    
                                                                        
           EXEC CICS READ                                               
               SET     (ADDRESS OF PENDING-MAILING-DATA)                
               DATASET (FILE-ID-ERPNDM)                                 
               RIDFLD  (ERPNDM-KEY)                                     
               UPDATE                                                   
           END-EXEC.                                                    
                                                                        
           IF PM-ADDRESS-LINE-1 GREATER SPACES                          
              MOVE PM-ADDRESS-LINE-1       TO BADDRS1.                  
                                                                        
           IF PM-ADDRESS-LINE-2 GREATER SPACES                          
              MOVE PM-ADDRESS-LINE-2       TO BADDRS2.                  
                                                                        
           IF PM-CITY > SPACES                              
              MOVE PM-CITY                 TO BCITY
           END-IF
      
           IF PM-STATE > SPACES                              
              MOVE PM-STATE                TO BSTATE
           END-IF
                                                                        
           IF PM-ZIP            GREATER SPACES                          
               MOVE SPACES               TO WS-ZIP-CODE                 
               IF PM-CANADIAN-POST-CODE                                 
                   MOVE PM-CAN-POST1     TO WS-ZIP-CAN-2-POST1          
                   MOVE PM-CAN-POST2     TO WS-ZIP-CAN-2-POST2          
                   MOVE WS-ZIP-CODE      TO BZIPCDE                     
               ELSE                                                     
                   MOVE PM-ZIP-CODE      TO WS-ZIP-AM-2-CODE            
                   MOVE WS-ZIP-CODE      TO BZIPCDE                     
                   IF PM-ZIP-PLUS4 NOT = SPACES  AND  ZEROS             
                       MOVE '-'          TO WS-ZIP-AM-2-DASH            
                       MOVE PM-ZIP-PLUS4 TO WS-ZIP-AM-2-PLUS4           
                       MOVE WS-ZIP-CODE  TO BZIPCDE.                    
                                                                        
      *    IF PM-PHONE-NO NUMERIC                                       
      *       IF PM-PHONE-NO GREATER ZEROS                              
      *          MOVE PM-PHONE-NO          TO  BPHONE-NO                
      *          INSPECT BPHONE-NO CONVERTING ' ' TO '-'.               
      
      
           IF PM-CRED-BENE-ADDR > SPACES                          
              MOVE PM-CRED-BENE-ADDR       TO BCADDR1
           END-IF
                                                                        
           IF PM-CRED-BENE-ADDR2 > SPACES                          
              MOVE PM-CRED-BENE-ADDR2     TO BCADDR2
           END-IF
                                                                        
           IF PM-CRED-BENE-CITY > SPACES
              MOVE PM-CRED-BENE-CITY       TO BCCITY
           END-IF
           IF PM-CRED-BENE-STATE > SPACES
              MOVE PM-CRED-BENE-STATE      TO BCSTATE
           END-IF
                                                                        
           IF PM-CRED-BENE-ZIP > SPACES                          
              MOVE SPACES               TO WS-ZIP-CODE                 
              IF PM-CB-CANADIAN-POST-CODE                                 
                 MOVE PM-CB-CAN-POST1     TO WS-ZIP-CAN-2-POST1          
                 MOVE PM-CB-CAN-POST2     TO WS-ZIP-CAN-2-POST2          
                 MOVE WS-ZIP-CODE      TO BZIPCDE                     
              ELSE                                                     
                 MOVE PM-CB-ZIP-CODE      TO WS-ZIP-AM-2-CODE            
                 MOVE WS-ZIP-CODE      TO BCZIPCD                     
                 IF PM-CB-ZIP-PLUS4 NOT = SPACES  AND  ZEROS             
                    MOVE '-'          TO WS-ZIP-AM-2-DASH            
                    MOVE PM-CB-ZIP-PLUS4 TO WS-ZIP-AM-2-PLUS4           
                    MOVE WS-ZIP-CODE  TO BCZIPCD
                 END-IF
              END-IF
           END-IF

           .                                                            
       7090-EXIT.                                                       
           EXIT.                                                        
                                                                        
           EJECT                                                        
                                                                        
       7100-FORMAT-CANCEL-SCREEN.                                       
           MOVE 'Y'                    TO PI-DISPLAY-SW.                
                                                                        
           MOVE LOW-VALUES             TO DATA-AREA-C (2)               
                                          DATA-AREA-C (3).              
                                                                        
           MOVE -1                     TO CPFENTRL.                     
                                                                        
           MOVE PB-BATCH-SEQ-NO        TO CSEQ        (1).              
           MOVE AL-SABON               TO CSEQ-ATTRB  (1).              
           MOVE PB-CERT-PRIME          TO CCERT       (1).              
           MOVE AL-SANON               TO CCERT-ATTRB (1).              
           MOVE PB-CERT-SFX            TO CSFX        (1).              
           MOVE AL-SANON               TO CSFX-ATTRB  (1).              
                                                                        
           MOVE PB-CERT-EFF-DT         TO DC-BIN-DATE-1.                
           MOVE SPACE                  TO DC-OPTION-CODE.               
           PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
           MOVE DC-GREG-DATE-1-MDY     TO CEFFDT       (1).             
           MOVE AL-SANON               TO CEFFDT-ATTRB (1).             
           MOVE PB-C-LAST-NAME         TO CLAST-NAME   (1).             
                                                                        
           IF PB-C-LF-CANCEL-DT NOT = LOW-VALUES                        
              MOVE PB-C-LF-CANCEL-DT   TO DC-BIN-DATE-1                 
              MOVE SPACE               TO DC-OPTION-CODE                
              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  
              MOVE DC-GREG-DATE-1-MDY  TO CCANDT1 (1)                   
              MOVE AL-UANON            TO CCANDT1-ATTRB (1).            
                                                                        
           IF PB-C-AH-CANCEL-DT NOT = LOW-VALUES                        
              MOVE PB-C-AH-CANCEL-DT   TO DC-BIN-DATE-1                 
              MOVE SPACE               TO DC-OPTION-CODE                
              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  
              MOVE DC-GREG-DATE-1-MDY  TO CCANDT2 (1)                   
              MOVE AL-UANON            TO CCANDT2-ATTRB (1).            
                                                                        
           IF PB-C-LF-REFUND-OVERRIDE EQUAL SPACES                      
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE PB-C-LF-REFUND-OVERRIDE                              
                                       TO CMTHD1 (1)                    
              MOVE AL-UANON            TO CMTHD1-ATTRB (1).             
                                                                        
           IF PB-C-AH-REFUND-OVERRIDE EQUAL SPACES                      
              NEXT SENTENCE                                             
           ELSE                                                         
              MOVE PB-C-AH-REFUND-OVERRIDE                              
                                       TO CMTHD2 (1)                    
              MOVE AL-UANON            TO CMTHD2-ATTRB (1).             
                                                                        
           IF PB-C-LF-CANCEL-AMT NOT =  ZEROS                           
              MOVE PB-C-LF-CANCEL-AMT  TO CREFUND1O (1)                 
              MOVE AL-UNNON            TO CREFUND1-ATTRB (1).           
                                                                        
           IF PB-C-AH-CANCEL-AMT NOT =  ZEROS                           
              MOVE PB-C-AH-CANCEL-AMT  TO CREFUND2O      (1)            
              MOVE AL-UNNON            TO CREFUND2-ATTRB (1).           
                                                                        
           IF PB-C-LIVES GREATER ZEROS                                  
              MOVE PB-C-LIVES          TO CLIVESO      (1)              
              MOVE AL-UNNON            TO CLIVES-ATTRB (1).             
                                                                        
           IF PB-C-CANCEL-REASON GREATER SPACES
              MOVE PB-C-CANCEL-REASON  TO CCANREA (1)
              MOVE AL-UANON            TO CCANREA-ATTRB (1)
           END-IF
      
      *    IF PB-C-MICROFILM-NO  IS NUMERIC                             
      *        IF PB-C-MICROFILM-NO  NOT =  ZEROS                       
      *            MOVE PB-C-MICROFILM-NO                               
      *                                TO  CMICRO-NOO (1)               
      *            MOVE AL-UNNON       TO  CMICRO-NO-ATTRB (1).         
                                                                        
           IF PB-C-PAYEE-CODE GREATER SPACES                            
              MOVE PB-C-PAYEE-CODE     TO CPAYEE       (1)              
              MOVE AL-UANON            TO CPAYEE-ATTRB (1).             
                                                                        
           IF PB-C-REFUND-SW  GREATER SPACES                            
              MOVE PB-C-REFUND-SW      TO CCHK         (1)              
              MOVE AL-UANON            TO CCHK-ATTRB   (1).             
                                                                        
           IF PB-C-LAST-NAME GREATER SPACES                             
              MOVE PB-C-LAST-NAME      TO CLAST-NAME       (1)          
              MOVE AL-UANON            TO CLAST-NAME-ATTRB (1).         
                                                                        
           PERFORM 7180-PROTECT-FIELDS VARYING WS-SUB2 FROM +2 BY +1    
                                       UNTIL WS-SUB2 GREATER +4.        
                                                                        
           GO TO 7190-EXIT.                                             
                                                                        
       7180-PROTECT-FIELDS.                                             
           MOVE AL-SANOF               TO CCERT-ATTRB      (WS-SUB2)    
                                          CSFX-ATTRB       (WS-SUB2)    
                                          CEFFDT-ATTRB     (WS-SUB2)    
                                          CLAST-NAME-ATTRB (WS-SUB2)    
                                          CCANDT1-ATTRB    (WS-SUB2)    
                                          CCANDT2-ATTRB    (WS-SUB2)    
                                          CMTHD1-ATTRB     (WS-SUB2)    
                                          CMTHD2-ATTRB     (WS-SUB2)    
                                          CREFUND1-ATTRB   (WS-SUB2)    
                                          CREFUND2-ATTRB   (WS-SUB2)    
                                          CCHK-ATTRB       (WS-SUB2)    
                                          CPAYEE-ATTRB     (WS-SUB2)    
                                          CLIVES-ATTRB     (WS-SUB2)    
                                          CCANREA-ATTRB    (WS-SUB2).   
                                                                        
       7190-EXIT.                                                       
                                                                        
           EJECT                                                        
                                                                        
       8100-SEND-INITIAL-MAP.                                           
           IF PI-MAP-NAME = VP630B                                      
               NEXT SENTENCE                                            
           ELSE                                                         
               GO TO 8110-SEND-INITIAL-CANCEL-MAP.                      
                                                                        
      *    MOVE PI-MEMBER-CAPTION        TO BCAPTNO.                    
                                                                        
           IF EIBAID NOT = DFHPF1   AND                                 
              EIBAID NOT = DFHPF2   AND                                 
              EIBAID NOT = DFHPF5   AND                                 
              PI-MAINT-FUNC NOT = 'B'                                   
                PERFORM 0600-PROTECT-FIELDS THRU 0600-EXIT.             
                                                                        
           MOVE PI-SAV-ENTRY-BATCH     TO BBATCHO.                      
           MOVE PI-AM-NAME             TO BACCTNMO
           IF ((PI-AM-ADDR1 NOT = SPACES)
              OR (PI-AM-ADDR2 NOT = SPACES))
              IF BNFICRYO (1:5) = LOW-VALUES OR '_____'
                 MOVE PI-AM-NAME       TO BNFICRYO
                 MOVE AL-UANON         TO BBENEFICIARY-ATTRB
              END-IF
              IF BCADDR1 (1:5) = LOW-VALUES OR '_____'
                 MOVE PI-AM-ADDR2      TO BCADDR1
                 MOVE AL-UANON         TO BCADDR1-ATTRB
              END-IF
              IF BCADDR2 = SPACES OR LOW-VALUES
      *          MOVE PI-AM-ADDR2      TO BCADDR2
                 MOVE AL-UANON         TO BCADDR2-ATTRB
              END-IF
              IF BCCITY = SPACES OR LOW-VALUES
                 MOVE PI-AM-CITY       TO BCCITY
                 MOVE AL-UANON         TO BCCITY-ATTRB
              END-IF
              IF BCSTATE = SPACES OR LOW-VALUES
                 MOVE PI-AM-STATE      TO BCSTATE
                 MOVE AL-UANON         TO BCSTATE-ATTRB
              END-IF
              IF BCZIPCD = SPACES OR LOW-VALUES
                 MOVE PI-AM-ZIP        TO BCZIPCD
                 MOVE AL-UANON         TO BCZIPCD-ATTRB
              END-IF
           END-IF
      
           IF PI-AM-EDIT-LOAN-OFC = 'N'
              MOVE AL-SANOF            TO BLN-OFFICER-ATTRB
           END-IF
      
           IF PI-MAIL-YES                                               
              continue
           ELSE                                                         
              MOVE AL-SANOF            TO BADDRS1-ATTRB                 
                                          BADDRS2-ATTRB                 
                                          BCITY-ATTRB
                                          BSTATE-ATTRB
                                          BZIPCDE-ATTRB                 
      *                                   BZIP4-ATTRB                   
      *                                   BPHONE-ATTRB
           END-IF
      
           IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
              MOVE AL-SADOF            TO balt-ben2-attrb
                                          balt-prem2-attrb
           END-IF
      
           IF PI-COMPANY-ID NOT = 'DCC' and 'CID' and 'VPP'
              MOVE AL-SADOF            TO BVINHD-ATTRB
                                          BVIN-ATTRB
           END-IF
      
      *    IF PI-LAST-FUNC-DISPLAY                                      
      *          MOVE AL-SADOF            TO BCERTV-ATTRB               
      *                                      BSFXV-ATTRB                
      *                                      BEFFDTV-ATTRB              
      *                                      BLAST-NAMEV-ATTRB          
      *    ELSE                                                         
      *       IF PI-COMPANY-ID = ('PEM' OR 'CRI')                       
      *          MOVE AL-UANOF            TO BCERTV-ATTRB               
      *                                      BSFXV-ATTRB                
      *                                      BEFFDTV-ATTRB              
      *                                      BLAST-NAMEV-ATTRB          
      *       ELSE                                                      
      *          MOVE AL-SADOF            TO BCERTV-ATTRB               
      *                                      BSFXV-ATTRB                
      *                                      BEFFDTV-ATTRB              
      *                                      BLAST-NAMEV-ATTRB.         
                                                                        
           IF PI-LAST-FUNC-DISPLAY                                      
              NEXT SENTENCE                                             
           ELSE                                                         
              IF PI-COMPANY-ID = 'PEM' OR 'CRI'                         
                 MOVE AL-UADOF            TO BCERT-ATTRB                
                                             BSFX-ATTRB                 
                                             BEFFDT-ATTRB               
                                             BLAST-NAME-ATTRB           
              ELSE                                                      
                 MOVE AL-UANOF            TO BCERT-ATTRB                
                                             BEFFDT-ATTRB.              
                                                                        
           IF PI-NB-MONTH-END-DT NOT = SPACES                           
              MOVE PI-NB-MONTH-END-DT  TO DC-BIN-DATE-1                 
             ELSE                                                       
              MOVE PI-CR-MONTH-END-DT  TO DC-BIN-DATE-1.                
                                                                        
           MOVE SPACE                  TO DC-OPTION-CODE.               
           PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
           MOVE DC-GREG-DATE-1-EDIT    TO BMOENDO.                      
                                                                        
           MOVE WS-CURRENT-DT          TO BDATEO.                       
           MOVE EIBTIME                TO TIME-IN.                      
           MOVE TIME-OUT               TO BTIMEO.                       
                                                                        
           MOVE PI-LIFE-OVERRIDE-L2    TO BKIND1
           MOVE AL-SABOF               TO BKIND1-ATTRB
           MOVE PI-AH-OVERRIDE-L2      TO BKIND2
           MOVE AL-SABOF               TO BKIND2-ATTRB
                                                                        
           IF PI-DATA-ERRORS
              MOVE SPACE               TO PI-ERROR-SW
           ELSE
              IF EIBAID = DFHPF1 OR DFHPF2
                 CONTINUE
              ELSE
                 MOVE -1               TO BEFFDTL
               END-IF
           END-IF
      
           MOVE EMI-MESSAGE-AREA (1)   TO BERMSG1O.                     
           MOVE EMI-MESSAGE-AREA (2)   TO BERMSG2O.                     
                                                                        
           EXEC CICS SEND                                               
               MAP      (PI-MAP-NAME)                                   
               MAPSET   (MAPSET-VP6301S)                                
               FROM     (VP630BI)                                       
               ERASE                                                    
               CURSOR                                                   
           END-EXEC.                                                    
                                                                        
           GO TO 9100-RETURN-TRAN.                                      
                                                                        
           EJECT                                                        
       8110-SEND-INITIAL-CANCEL-MAP.                                    
           IF EIBAID NOT = DFHPF5  AND                                  
              EIBAID NOT = DFHPF1  AND                                  
              EIBAID NOT = DFHPF2  AND                                  
              PI-MAINT-FUNC NOT = 'B'                                   
                PERFORM 0700-PROTECT-FIELDS THRU 0700-EXIT.             
                                                                        
           MOVE PI-SAV-ENTRY-BATCH     TO CBATCHO.                      
           MOVE PI-AM-NAME             TO CACCTNMO.                     
                                                                        
           IF PI-NB-MONTH-END-DT NOT = SPACES                           
              MOVE PI-NB-MONTH-END-DT  TO DC-BIN-DATE-1                 
             ELSE                                                       
              MOVE PI-CR-MONTH-END-DT  TO DC-BIN-DATE-1.                
                                                                        
           MOVE SPACE                  TO DC-OPTION-CODE.               
           PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
           MOVE DC-GREG-DATE-1-EDIT    TO CMOENDO.                      
                                                                        
           MOVE WS-CURRENT-DT          TO CDATEO.                       
           MOVE EIBTIME                TO TIME-IN.                      
           MOVE TIME-OUT               TO CTIMEO.                       
                                                                        
           MOVE PI-LIFE-OVERRIDE-L2    TO CKIND1 (1)                    
                                          CKIND1 (2)                    
                                          CKIND1 (3)                    
                                          CKIND1 (4).                   
           MOVE PI-AH-OVERRIDE-L2      TO CKIND2 (1)                    
                                          CKIND2 (2)                    
                                          CKIND2 (3)                    
                                          CKIND2 (4).                   
           MOVE AL-SABOF               TO CKIND1-ATTRB (1)              
                                          CKIND1-ATTRB (2)              
                                          CKIND1-ATTRB (3)              
                                          CKIND1-ATTRB (4)              
                                          CKIND2-ATTRB (1)              
                                          CKIND2-ATTRB (2)              
                                          CKIND2-ATTRB (3)              
                                          CKIND2-ATTRB (4).             
                                                                        
           IF PI-DATA-ERRORS                                            
               MOVE SPACE              TO PI-ERROR-SW                   
           ELSE                                                         
               IF EIBAID = DFHPF1 OR DFHPF2                             
                   NEXT SENTENCE                                        
               ELSE                                                     
                   MOVE -1             TO CCERT1L.                      
                                                                        
           MOVE EMI-MESSAGE-AREA (1)   TO CERMSG1O.                     
           MOVE EMI-MESSAGE-AREA (2)   TO CERMSG2O.                     
                                                                        
           EXEC CICS SEND                                               
               MAP      (PI-MAP-NAME)                                   
               MAPSET   (MAPSET-VP6301S)                                
               FROM     (VP630BI)                                       
               ERASE                                                    
               CURSOR                                                   
           END-EXEC.                                                    
                                                                        
           GO TO 9100-RETURN-TRAN.                                      
                                                                        
           EJECT                                                        
                                                                        
       8200-SEND-DATAONLY.                                              
           MOVE SPACE              TO PI-ERROR-SW.                      
                                                                        
           IF PI-MAP-NAME = VP630B                                      
      *        MOVE PI-MEMBER-CAPTION      TO BCAPTNO                   
               MOVE WS-CURRENT-DT          TO BDATEO                    
               MOVE EIBTIME                TO TIME-IN                   
               MOVE TIME-OUT               TO BTIMEO                    
               MOVE EMI-MESSAGE-AREA (1)   TO BERMSG1O                  
               MOVE EMI-MESSAGE-AREA (2)   TO BERMSG2O                  
               EXEC CICS SEND                                           
                   MAP      (PI-MAP-NAME)                               
                   MAPSET   (MAPSET-VP6301S)                            
                   FROM     (VP630BI)                                   
                   DATAONLY                                             
                   CURSOR                                               
               END-EXEC                                                 
           ELSE                                                         
               MOVE WS-CURRENT-DT          TO CDATEO                    
               MOVE EIBTIME                TO TIME-IN                   
               MOVE TIME-OUT               TO CTIMEO                    
               MOVE EMI-MESSAGE-AREA (1)   TO CERMSG1O                  
               MOVE EMI-MESSAGE-AREA (2)   TO CERMSG2O                  
               EXEC CICS SEND                                           
                   MAP      (PI-MAP-NAME)                               
                   MAPSET   (MAPSET-VP6301S)                            
                   FROM     (VP630BI)                                   
                   DATAONLY                                             
                   CURSOR                                               
               END-EXEC.                                                
                                                                        
           GO TO 9100-RETURN-TRAN.                                      
                                                                        
           EJECT                                                        
                                                                        
       8300-SEND-TEXT.                                                  
           EXEC CICS SEND TEXT                                          
               FROM     (LOGOFF-TEXT)                                   
               LENGTH   (LOGOFF-LENGTH)                                 
               ERASE                                                    
               FREEKB                                                   
           END-EXEC.                                                    
                                                                        
           EXEC CICS RETURN                                             
           END-EXEC.                                                    
                                                                        
       8350-SEND-WARNING.                                               
           EXEC CICS SEND TEXT                                          
               FROM     (WARNING-TEXT)                                  
               LENGTH   (WARNING-LENGTH)                                
               ERASE                                                    
               FREEKB                                                   
           END-EXEC.                                                    
                                                                        
           GO TO 9100-RETURN-TRAN.                                      
                                                                        
       8400-LOG-JOURNAL-RECORD.                                         
           MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   
           MOVE THIS-PGM                TO JP-PROGRAM-ID.               
                                                                        
      *    EXEC CICS JOURNAL                                            
      *        JFILEID     (PI-JOURNAL-FILE-ID)                         
      *        JTYPEID     ('EL')                                       
      *        FROM        (JOURNAL-RECORD)                             
      *        LENGTH      (WS-JOURNAL-RECORD-LENGTH)                   
      *        END-EXEC.                                                
                                                                        
       8500-DATE-CONVERT.                                               
           EXEC CICS LINK                                               
               PROGRAM  (LINK-ELDATCV)                                  
               COMMAREA (DATE-CONVERSION-DATA)                          
               LENGTH   (DC-COMM-LENGTH)                                
           END-EXEC.                                                    
                                                                        
       8500-EXIT.                                                       
           EXIT.                                                        
                                                                        
           EJECT                                                        
                                                                        
       8550-SET-MAP-SEQ-NOS.                                            
           IF PI-MAP-NAME = VP630B                                      
               MOVE PI-NEXT-DISPLAY-SEQ-NO TO BSEQ                      
               MOVE AL-SABON               TO BSEQ-ATTRB                
           ELSE                                                         
               MOVE PI-NEXT-DISPLAY-SEQ-NO TO CSEQ (WS-SUB2)            
               MOVE AL-SABON               TO CSEQ-ATTRB (WS-SUB2).     
                                                                        
           ADD +1  TO PI-NEXT-DISPLAY-SEQ-NO.                           
                                                                        
       8555-EXIT.                                                       
           EXIT.                                                        
                                                                        
       8600-DEEDIT.                                                     
           EXEC CICS BIF DEEDIT                                         
               FIELD   (DEEDIT-FIELD)                                   
               LENGTH  (15)                                             
           END-EXEC.                                                    
                                                                        
       8600-EXIT.                                                       
           EXIT.                                                        
                                                                        
       8800-UNAUTHORIZED-ACCESS.                                        
           MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   
           GO TO 8300-SEND-TEXT.                                        
                                                                        
       9000-RETURN-CICS.                                                
           EXEC CICS RETURN                                             
           END-EXEC.                                                    
                                                                        
       9100-RETURN-TRAN.                                                
           MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             
                                                                        
           IF  PI-MAP-NAME = VP630B                                     
               MOVE '630B'             TO PI-CURRENT-SCREEN-NO.         
                                                                        
           IF  PI-MAP-NAME = VP630C                                     
               MOVE '630C'             TO PI-CURRENT-SCREEN-NO.         
                                                                        
           EXEC CICS RETURN                                             
               TRANSID    (TRANS-EXA6)                                  
               COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     
               LENGTH     (WS-COMM-LENGTH)                              
           END-EXEC.                                                    
                                                                        
       9300-XCTL.                                                       
           EXEC CICS XCTL                                               
               PROGRAM    (PGM-NAME)                                    
               COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     
               LENGTH     (WS-COMM-LENGTH)                              
           END-EXEC.                                                    
                                                                        
       9400-CLEAR.                                                      
           MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME                      
           GO TO 9300-XCTL.                                             
                                                                        
       9500-PF12.                                                       
           MOVE XCTL-EL010             TO PGM-NAME.                     
           GO TO 9300-XCTL.                                             
                                                                        
       9600-PGMID-ERROR.                                                
           EXEC CICS HANDLE CONDITION                                   
               PGMIDERR    (8300-SEND-TEXT)                             
           END-EXEC.                                                    
                                                                        
           MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           
           MOVE ' '                    TO PI-ENTRY-CD-1.                
           MOVE XCTL-EL005            TO PGM-NAME.                      
           MOVE PGM-NAME               TO LOGOFF-PGM.                   
           MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  
           GO TO 9300-XCTL.                                             
                                                                        
       9900-ERROR-FORMAT.                                               
           IF PI-MAP-NAME = VP630B                                      
              MOVE 2                   TO EMI-NUMBER-OF-LINES           
             ELSE                                                       
              MOVE 1                   TO EMI-NUMBER-OF-LINES.          
                                                                        
           IF NOT EMI-ERRORS-COMPLETE                                   
               MOVE LINK-EL001         TO PGM-NAME                      
               EXEC CICS LINK                                           
                   PROGRAM    (PGM-NAME)                                
                   COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           
                   LENGTH     (EMI-COMM-LENGTH)                         
               END-EXEC.                                                
                                                                        
       9900-EXIT.                                                       
           EXIT.                                                        
                                                                        
       9990-ABEND.                                                      
           MOVE LINK-EL004             TO PGM-NAME.                     
           MOVE DFHEIBLK               TO EMI-LINE1.                    
           EXEC CICS LINK                                               
               PROGRAM   (PGM-NAME)                                     
               COMMAREA  (EMI-LINE1)                                    
               LENGTH    (72)                                           
           END-EXEC.                                                    
                                                                        
           IF PI-MAP-NAME = VP630B                                      
               MOVE -1 TO BPFENTRL                                      
           ELSE                                                         
               MOVE -1 TO CPFENTRL.                                     
                                                                        
           GO TO 8200-SEND-DATAONLY.                                    
                                                                        
           GOBACK.                                                      
                                                                        
       9995-SECURITY-VIOLATION.                                         
           COPY ELCSCTP.                                                
                                                                        
       9995-EXIT.                                                       
           EXIT.                                                        
                                                                        
