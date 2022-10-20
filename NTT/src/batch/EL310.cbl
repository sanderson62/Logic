
00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL310 .                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 05/05/95 15:02:11.                 
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            
00008 *                            VMOD=2.085.                          
00009                                                                   
00010 *AUTHOR.     LOGIC, INC.                                          
00011 *            DALLAS, TEXAS.                                       
00012                                                                   
00013 *DATE-COMPILED.                                                   
00014                                                                   
00025 *REMARKS.                                                         
00026 *        THIS PROGRAM IS RUN EVERY NIGHT AFTER THE SHUTDOWN OF    
00027 *    CICS TO BRING THE CLAS-IC FILES CURRENT AND TO PRODUCE AN    
00028 *    EXTRACT TAPE USED AS INPUT FOR MOST OF THE CLAS-IC OFFLINE   
00029 *    REPORTING.
040902*
040902******************************************************************
040902*                   C H A N G E   L O G
040902*
040902* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
040902*-----------------------------------------------------------------
040902*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
040902* EFFECTIVE    NUMBER
040902*-----------------------------------------------------------------
040902* 040902    2001120300002  SMVA  ADD TOTAL PAID AMT TO AA EXTR
062602* 062602    2002030700006  PEMA  Add processing for priority 8
102902* 102902    2001061800003  PEMA  ADD DCC PROCESSING           
121902* 121902    2001061800003  SMVA  COMMENT OUT OBSOLETE CODE
122002* 122002                         ADD PROCESSING FOR NEW CLM TYP I
121703* 121703    2003080800002  SMVA  ADD PROCESSING FOR NEW CLM TYP G
051104* 051104    2004051200002  SMVA  ADD RPT CODE 2 TO BA EXTRACT
051804* 051804    2004051200002  SMVA  ADD RPT CODE 1 TO BA EXTRACT
022106* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST
061406* 061406  IR2006052600002  AJRA  EXCLUDE NON UWR NOTES FROM  
061406*                                45 DAY NO ACTIVITY REPORT
091808* 091808    2008022800002  AJRA  GET CHECK NUMBER FROM STATE CNTL FOR AK
      * 121608    2008100900001  PEMA  ADD REFORMATION PROCESSING
070909* 070909    2009060400001  AJRA  ALLOW OVERRIDE OF AUTOPAY END LETTER
092909* 092909  IR2009092500002  AJRA  USE PREV CYCLE DT FOR FOLLOW UP RPT
092909*                                REMOVE CLOSED CLAIMS FROM RESEND RPT
010510* 010510  CR2008100900001  PEMA  ADD RESCISSION AND REFORMATION STUFF
072810* 072810  CR2010010400004  PEMA  SUBTRACT PAID CLMS FROM PTC RESV.
051810* 051810 CR2010042900001   PEMA  SEPARATE CITY AND STATE
080510* 080510  CR2009122800001  AJRA  NAPERSOFT AUTO CLOSE ON FOLLOW UP
102810* 102810  CR2009122800001  AJRA  ADD STOP LETTER DATE
012511* 012511  IR2011012500001  AJRA  FIX AUTO CLOSE,STOP ARCHIVE VERIFY
032612* 032612  CR2011110200001  PEMA  AHL CHANGES
041712* 041712  IR2012041600003  AJRA  FIX BENE NAME ON AHL AUTO PAYMENTS
061013* 061013    2012113000002  PEMA  ADD SPECIAL STUFF FOR SPP DDF
080613* 080613  CR2013071700001  PEMA  ADJ RESV DUE TO CRIT PERIOD
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
072514* 072514  CR2013060600001  PEMA  FIX negative PTC clm reserve
111714* 111714  CR2014073000001  PEMA  DRAFTS TO CHECKS
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
081216* 081216  IR2016071900001  TANA  DEFAULT SURVEY TO Y FOR DCC
102516* 102516  CR2015102900002  PEMA  AUTOMATE AHL CLM CUT-OFF DATE
110916* 110916  IR2016091300002  TANA  ALLOW AHL TO AUTO-CLOSE
010918* 010918  IR2017120500003  TANA  SET ACH FLAG ON PMT TRLR
022718* 022718  CR2017100200004  TANA  SET HOLD & PAYS GREATER THAN 30
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
040819* 040819  IR2019030400004  TANA  FIX HOLD & PAY PTD UPDATE
050619* 051619  CR2019042600001  PEMA  USE USERID FOR 323 REPORTS NO SYST/AUTO
071019* 071019  IR2019070900002  PEMA  Fix last maint user SYST issue
072419* 072419  IR2019072300001  PEMA  Fix last maint user SYST issue again.
100919* 100919  IR2019092400001  TANA  Fix Month End Hold & Pay PTD Update
101019* 101019  IR2019100900001  PEMA  Fix last maint user SYST issue again
041320* 041320  CR2020030500002  PEMA  Distinguish between iss and canc notes.
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
032922* 032922  CR2021100800003  PEMA  Increase number of Prod Defs to 11
063022* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
040902******************************************************************
00030                                                                   
00031                                                                   
00032 *    INPUT FILES  - ERACCT - ACCOUNT MASTER                       
00033 *                   ELPGMS - PROGRAM OPTIONS SELECTED             
00034 *                   ELBENE - BENEFICIARY MASTER                   
00035 *                   ERNOTE - NOTES FILE (FOR DMD ONLY)            
083002********************** The MP files are no longer used
00036 *                   MPPLCY - CONVENIENCE POLICY MASTER            
00037 *                   MPPROD - CONVENIENCE PRODUCER MASTER          
00038 *                   MPPLAN - CONVENIENCE PRODUCER PLAN MASTER     
00039                                                                   
00040 *    I-O   FILES  - ELCNTL - CONTROL FILE                         
00041 *                   ELCERT - CERTIFICIATE MASTER                  
00042 *                   ELMSTR - CLAIM MASTER                         
00043 *                   ELTRLR - ACTIVITY TRAILERS                    
00044 *                   ELCHKQ - CHECK QUE                            
00045 *                   ELARCH - LETTER ARCHIVE                       
00046 *                   ELACTQ - ACTIVITY QUE                         
00047                                                                   
00048 *    OUTPUT FILES - ELEXTR - REPORTS EXTRACT                      
00049 *                   ELAINP - AIMS INTERFACE FILE                  
00050 *                   ELDMO  - (DMD) DMO FILE                       
00051 *                   ELSTAT - (DMD) STATISTICS FILE                
00052                                                                   
00053                                                                   
00054 *        EACH COMPANY'S OPTIONS FOR PROGRAM EL310 ARE READ DURING 
00055 *    THE EXECUTION OF THIS PROGRAM.  ONLY OPTIONS WITH A 'NONE'   
00056 *    FREQUENCY WILL BE RECOGNIZED.                                
00057                                                                   
00058 *        PROCESS = 1 EXTRACT B IS MONTH-TO-DATE                   
00059 *        PROCESS = 2 EXTRACT B IS YEAR-TO-DATE                    
00060                                                                   
00061 *        THE REPORT EXTRACT FILE, ELEXTR, IS WRITTEN FOR ALL      
00062 *    COMPANIES HAVING AN ONLINE CONTROL FILE. AN INTERNAL         
00063 *    SORT IS USED TO SEQUENCE THIS FILE AFTER CREATION.  THE      
00064 *    SELECTION CRITERIA USED FOR EACH OF THE EXTRACT TYPES ARE    
00065 *    AS FOLLOWS -                                                 
00066                                                                   
00067                                                                   
00068 *    - EXTRACT - A     RECORD - A     CLAIM RESERVES              
00069                                                                   
00070 *          EXTRACT FOR ALL OPEN CLAIMS                            
00071                                                                   
00072                                                                   
00073 *    - EXTRACT - A     RECORD - B     CLAIM PAYMENTS              
00074                                                                   
00075 *          ALL PAYMENTS AND VOIDS THAT HAVE NOT PREVIOUSLY        
00076 *          BEEN ACCEPTED BY THE CREDIT SYSTEM.                    
00077                                                                   
00078                                                                   
00079 *    - EXTRACT - B     RECORD - A     CLAIM CHECKS/PAYMENTS       
00080                                                                   
00081 *          ALL PAYMENTS WITH A CREDIT SELECT DATE EQUAL TO        
00082 *          THE CURRENT OR PRIOR MONTH-END.                        
00083                                                                   
00084                                                                   
00085 *    - EXTRACT - C     RECORD - A     CHECK USAGE                 
00086                                                                   
00087 *          CHECKS WRITTEN IN CURRENT AND PRIOR MONTH BASED ON     
00088 *          CHECK WRITTEN DATE IN CHECK QUE RECORD (ELCHKQ),       
00089 *          AND ALL OFFLINE PAYMENTS AND VOIDS SELECTED IN THE     
00090 *          CURRENT AND PRIOR MONTH.                               
00091                                                                   
00092                                                                   
00093 *    - EXTRACT - D     RECORD - A     AUTOMATIC PAYMENTS          
00094                                                                   
00095 *          CREATED FOR EACH AUTOMATIC PAYMENT GENERATED.          
00096                                                                   
00097                                                                   
00098 *    - EXTRACT - D     RECORD - B     CORRESPONDENCE TO RESEND    
00099                                                                   
00100 *          GENERATED FOR LETTERS SCHEDULED TO BE RESENT (AND NOT  
00101 *          YET RECEIVED). RESENT DATES SELECTED FOR CURRENT DATE  
00102 *          AND PLUS OR MINUS 2 DAYS.                              
00103 *          AS OF 07/11/85, EXTRACTS WILL NOT BE CREATED UNLESS    
00104 *          THE LETTER HAS NOT BEEN PRINTED WHETHER IT BE A RESEND 
00105 *          OR INITIAL LETTER.                                     
00106                                                                   
00107                                                                   
00108 *    - EXTRACT - D     RECORD - C     CLAIMS INACTIVE FOR 45 DAYS 
00109                                                                   
00110 *          SELECTED FOR CLAIMS WHOSE LAST MAINTENANCE DATE (IN    
00111 *          CLAIM MASTER) IS 45 DAYS PRIOR TO CURRENT DATE.        
00112                                                                   
00113                                                                   
00114 *    - EXTRACT - D     RECORD - D     AUTOMATIC CLOSINGS          
00115                                                                   
00116 *          THIS EXTRACT IS WRITTEN FOR EACH CLAIM CLOSED BY       
00117 *          AUTOMATIC PROCESSING.                                  
00118                                                                   
00119                                                                   
00120 *    - EXTRACT - D     RECORD - E     UNPROCESSED CHECKS          
00121                                                                   
00122 *          THERE IS TWO DIFFERENT CRITERIA FOR CREATING THIS      
00123 *          EXTRACT -                                              
00124                                                                   
00125 *          1. IF A PAYMENT HAS BEEN ISSUED ONLINE AND NOT         
00126 *             VOIDED OR RELEASED (PRINT TIME GIVEN), AN EXTRACT   
00127 *             IS CREATED.                                         
00128                                                                   
00129 *          2. IF A CHECK QUE RECORD (ELCHKQ) HAS BEEN CREATED,    
00130 *             BUT NEVER PRINTED (TIMES PRINTED = 0), AN           
00131 *             EXTRACT IS CREATED.                                 
00132                                                                   
00133                                                                   
00134 *    - EXTRACT - D     RECORD - F     UNRESOLVED ERRORS           
00135                                                                   
00136 *          CLAIMS THAT HAVE A COUNT IN EITHER THE FATAL OR        
00137 *          FORCIBLE ERROR FIELDS ARE SELECTED.                    
00138                                                                   
00139                                                                   
00140 *    - EXTRACT - D     RECORD - G     AUTOMATIC PROMPTS           
00141                                                                   
00142 *          PROMPTS ARE EXTRACTED BEGINNING WITH THE START         
00143 *          NOTIFICATION DATE AND WILL CONTINUE TO BE EXTRACTED    
00144 *          THRU THE SECOND DAY AFTER THE ACTION DATE.             
00145                                                                   
00146 *    - EXTRACT - D     RECORD - H     UNAPPROVED PAYMENTS         
00147                                                                   
00148 *          FOR CLIENTS WHO USE THE PAYMENT APPROVAL OPTION        
00149 *          IN THE COMPANY RECORD.   THESE EXTRACTS ARE CREATED    
00150 *          FOR EVERY PAYMENT GENERATED THAT HAS NOT BEEN APPROVED.
00151                                                                   
00152                                                                   
00153 *    - EXTRACT - D     RECORD - I     LETTERS ANSWERED TODAY      
00154                                                                   
00155 *          THESE EXTRACT ARE CREATED IF THE LETTER ANSWERED       
00156 *          DATE = TODAY'S DATE.                                   
00157                                                                   
00158 *    - EXTRACT - D     RECORD - J     LAG REPORT EXTRACTS         
00159                                                                   
00160 *          THESE EXTRACTS ARE CREATED FOR COMPANY-ID AIG AND AUK  
00161 *          FOR CLAIMS WITH NO ACTIVITY CODE SET AND NO ACTIVITY   
00162 *          FOR 60, 90 OR 120 DAYS.                                
00163                                                                   
00164 *    - EXTRACT - D     RECORD - K     LAPSE REPORT EXTRACTS       
00165                                                                   
00166 *          THESE EXTRACTS ARE CREATED FOR COMPANY-ID AIG AND AUK  
00167 *          FOR CLAIMS WITH AN ACTIVITY CODE OF 01 SET AND MEETS   
00168 *          THE REPORTING CRITERIA FOR ACTIVITY CODE 01.           
00169                                                                   
00170 *    - EXTRACT - D     RECORD - L     INCOMPLETE CERTIFICATE      
00171 *                                     EXTRACTS                    
00172                                                                   
00173 *          THESE EXTRACTS ARE CREATED FOR COMPANY-ID AIG AND AUK  
00174 *          FOR CLAIMS WITH A CERTIFICATE NUMBER EQUAL TO          
00175 *          "INCOMPLETE"                                           
00176                                                                   
00177 *    - EXTRACT - E     RECORD - A     CORRESPONDENCE REGISTER     
00178                                                                   
00179 *          EXTRACTS ARE CREATED IF ANY OF THE FOLLOWING DATES IN  
00180 *          THE CURRENT OR PREVIOUS MONTH                          
00181                                                                   
00182 *          1. ORIGINAL SEND DATE                                  
00183 *          2. ANSWER RECEIVED DATE                                
00184 *          3. RESEND DATE (ONLY IF NO ANSWER RECEIVED)            
00185 *          4. RE-SEND PRINTED DATE                                
00186 *          5. FOLLOW-UP DATE (ONLY IF NO ANSWER RECEIVED)         
00187                                                                   
00188 *    OUTPUT FROM THIS PROGRAM IS NOT ELIGIBLE FOR ONLINE STORAGE. 
00188 ***************************************************************** 
00188 *                                                                 
DAN01 *   CR# 2000021500004 - MODIFY CLAIM REPORTS FOR AUTO PAYS        
00188 *                                                                 
00188 ***************************************************************** 
00189                                                                   
00190  EJECT                                                            
00191                                                                   
00192  ENVIRONMENT DIVISION.                                            
00193                                                                   
00194  INPUT-OUTPUT SECTION.                                            
00195                                                                   
00196  FILE-CONTROL.                                                    
00197                                                                   
00198      SELECT REPORTS-EXTRACT-FILE                                  
00199          ASSIGN TO SYS010-UT-FBA1-S-SYS010.                       
00200                                                                   
00201      SELECT JOURNAL-LOG-FILE                                      
00202          ASSIGN TO SYS011-UT-FBA1-S-SYS011.                       
00203                                                                   
00204      SELECT ELSTAT-EXTRACT-FILE                                   
00205          ASSIGN TO SYS012-UT-FBA1-S-SYS012.                       
00206                                                                   
00207      SELECT SORT-WORK-FILE                                        
00208          ASSIGN TO SYS001-FBA1-S-SORTWK1.                         
00209                                                                   
00210      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   
00211                                                                   
00212      SELECT ELCNTL           ASSIGN TO SYS021-FBA1-ELCNTL         
00213                              ORGANIZATION IS INDEXED              
00214                              ACCESS IS DYNAMIC                    
00215                              RECORD KEY IS CF-CONTROL-PRIMARY     
00216                              FILE STATUS IS ELCNTL-FILE-STATUS.   
00217                                                                   
00218      SELECT ELCERT               COPY ELCCERTS.                   
00219                                                                   
00220      SELECT ELMSTR               COPY ELCMSTRS.                   
00221                                                                   
00222      SELECT ELTRLR           ASSIGN TO SYS024-FBA1-ELTRLR         
00223                              ORGANIZATION IS INDEXED              
00224                              ACCESS IS DYNAMIC                    
00225                              RECORD KEY IS AT-CONTROL-PRIMARY     
00226                              FILE STATUS IS ELTRLR-FILE-STATUS.   
00227                                                                   
00228                                                                   
00229      SELECT ERACCT     COPY ERCACCTS.                             
00230                                                                   
00231      SELECT ELCHKQ           ASSIGN TO SYS026-FBA1-ELCHKQ         
00232                              ORGANIZATION IS INDEXED              
00233                              ACCESS IS DYNAMIC                    
00234                              RECORD KEY IS CQ-CONTROL-PRIMARY     
00235                              FILE STATUS IS ELCHKQ-FILE-STATUS.   
00236                                                                   
00237      SELECT ELPGMS           ASSIGN TO SYS027-FBA1-ELPGMS         
00238                              ORGANIZATION IS INDEXED              
00239                              ACCESS IS DYNAMIC                    
00240                              RECORD KEY IS PS-CONTROL-PRIMARY     
00241                              FILE STATUS IS ELPGMS-FILE-STATUS.   
00242                                                                   
00243      SELECT ELARCH               COPY ELCARCHS.                   
00244                                                                   
00245      SELECT ELACTQ           ASSIGN TO SYS029-FBA1-ELACTQ         
00246                              ORGANIZATION IS INDEXED              
00247                              ACCESS IS DYNAMIC                    
00248                              RECORD KEY IS AQ-CONTROL-PRIMARY     
00249                              FILE STATUS IS ELACTQ-FILE-STATUS.   
00250                                                                   
CIDMOD     SELECT DLYACTV          ASSIGN TO SYS030-FBA1-DLYACTV        
CIDMOD                             ORGANIZATION IS INDEXED              
CIDMOD                             ACCESS IS DYNAMIC                    
CIDMOD                             RECORD KEY IS DA-KEY                 
CIDMOD                             FILE STATUS IS DLYACTV-FILE-STATUS.  
00264                                                                   
CIDMOD     SELECT DISPLAY-PRT      ASSIGN TO SYS053-UR-1403-S-SYS053.   
CIDMOD                                                                  
00251      SELECT ELAINP           ASSIGN TO SYS028-UT-3380-S-SYS028.   
00252                                                                   
00253      SELECT ELBENE           ASSIGN TO SYS034-FBA1-ELBENE         
00254                              ORGANIZATION IS INDEXED              
00255                              ACCESS IS DYNAMIC                    
00256                              RECORD KEY IS BE-CONTROL-PRIMARY     
00257                              FILE STATUS IS ELBENE-FILE-STATUS.   
00258                                                                   
00253      SELECT ERPDEF           ASSIGN TO ERPDEF
00254                              ORGANIZATION IS INDEXED
00255                              ACCESS IS DYNAMIC
00256                              RECORD KEY IS PC-CONTROL-PRIMARY
00257                              FILE STATUS IS ERPDEF-FILE-STATUS.
00258                                                                   
00259      SELECT ERNOTE           ASSIGN TO SYS035-FBA1-ERNOTE         
00260                              ORGANIZATION IS INDEXED              
00261                              ACCESS IS DYNAMIC                    
00262                              RECORD KEY IS CN-CONTROL-PRIMARY     
00263                              FILE STATUS IS ERNOTE-FILE-STATUS.   
00264                                                                   
00265      SELECT MPPROD           ASSIGN TO SYS032-FBA1-MPPROD         
00266                              ORGANIZATION IS INDEXED              
00267                              ACCESS IS DYNAMIC                    
00268                              RECORD KEY IS PD-CONTROL-PRIMARY     
00269                              FILE STATUS IS MPPROD-FILE-STATUS.   
00270                                                                   
00271      SELECT MPPLAN           ASSIGN TO SYS033-FBA1-MPPLAN         
00272                              ORGANIZATION IS INDEXED              
00273                              ACCESS IS DYNAMIC                    
00274                              RECORD KEY IS PP-CONTROL-PRIMARY     
00275                              FILE STATUS IS MPPLAN-FILE-STATUS.   

061013     SELECT ELCRTT           ASSIGN TO ELCRTT
061013                             ORGANIZATION IS INDEXED
061013                             ACCESS IS DYNAMIC
061013                             RECORD KEY IS CS-CONTROL-PRIMARY
061013                             FILE STATUS IS ELCRTT-FILE-STATUS.

00277      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   
00278                                                                   
00279  EJECT                                                            
00280  DATA DIVISION.                                                   
00281                                                                   
00282  FILE SECTION.                                                    
00283                                                                   
00284  FD  ELCNTL.                                                      
00285                                                                   
00286      COPY ELCCNTL.                                                
00287                                                                   
00288  EJECT                                                            
00289  FD  ELCERT.                                                      
00290                                                                   
00291      COPY ELCCERT.                                                
00292                                                                   
00293  EJECT                                                            
00294  FD  ELMSTR.                                                      
00295                                                                   
00296      COPY ELCMSTR.                                                
00297                                                                   
00298  EJECT                                                            
00299  FD  ELTRLR.                                                      
00300                                                                   
00301      COPY ELCTRLR.                                                
00302                                                                   
00303  EJECT                                                            
00304  FD  ERACCT.                                                      
00305                                                                   
00306      COPY ERCACCT.                                                
00307                                                                   
00308  EJECT                                                            
00309  FD  ELCHKQ.                                                      
00310                                                                   
00311      COPY ELCCHKQ.                                                
00312                                                                   
00313  EJECT                                                            
00314  FD  ELPGMS.                                                      
00315                                                                   
00316      COPY ELCPGMS.                                                
00317                                                                   
00318  EJECT                                                            
00319                                                                   
00320  FD  ELARCH.                                                      
00321                                                                   
00322      COPY ELCARCH.                                                
00323                                                                   
00324  EJECT                                                            
00325  FD  ELACTQ.                                                      
00326                                                                   
00327      COPY ELCACTQ.                                                
00328                                                                   
00329  EJECT                                                            
00330                                                                   
00331  FD  REPORTS-EXTRACT-FILE        COPY ELCEXTFD.                   
00332                                                                   
052804 01  REPORTS-EXTRACT-FILE-RECORD PIC X(319).                      
00334                                                                   
00335  FD  PRNTR                       COPY ELCPRTFD.                   
00336                                                                   
00337  EJECT                                                            
00338                                                                   
00339  FD  JOURNAL-LOG-FILE                                             
00340      RECORDING MODE IS V.                                         
00341                                                                   
00342      COPY ELCSLR.                                                 
00343                                                                   
00344  EJECT                                                            
00309  FD  DLYACTV                                                      
00310      RECORD CONTAINS 25 CHARACTERS                                
00311      LABEL RECORDS ARE STANDARD.                                  
00312                                  COPY ELCDAR.                     
00313                                                                   
00314                                  EJECT                            
CIDMOD FD  DISPLAY-PRT                                                  
CIDMOD     RECORDING MODE F                                             
CIDMOD     LABEL RECORDS ARE STANDARD                                   
CIDMOD     RECORD CONTAINS 133 CHARACTERS                               
CIDMOD     BLOCK CONTAINS 0 RECORDS                                     
CIDMOD     DATA RECORD IS DISPLAY-REC.                                  
CIDMOD                                                                  
CIDMOD 01  DISPLAY-REC.                                                 
CIDMOD     05  DISPLAY-CC                  PIC X.                       
CIDMOD     05  DISPLAY-INFO                PIC X(132).                  
CIDMOD                                                                  
CIDMOD                                 EJECT                            
00345  FD  ELAINP                                                       
00346      BLOCK CONTAINS 0 RECORDS
00347      RECORDING MODE F.                                            
00348                                                                   
00349  01  AIMS-INTERFACE-REC          PIC X(82).                       
00350                                                                   
00351  FD  ELSTAT-EXTRACT-FILE                                          
00352      RECORDING MODE F                                             
00353      LABEL RECORDS STANDARD                                       
00354      BLOCK CONTAINS 0 RECORDS
00355      RECORD CONTAINS  67 CHARACTERS.                              
00356                                                                   
00357      COPY ELCSTAT.                                                
00358  EJECT                                                            
00359  FD  ELBENE.                                                      
00360                                                                   
00361      COPY ELCBENE.                                                

       FD  ERPDEF.

                                       COPY ERCPDEF
            REPLACING LEADING ==PD== BY ==PC==.

00363 *************************************************                 
00364 *** NOTE : THE FOLLOWING FILE IS ACCESSED FOR ***                 
00365 *** CLIENT 'DMD' ONLY.  A SPECIAL COPYBOOK IS ***                 
00366 *** USED TO ACCESS THIS FILE                  ***                 
00367 *************************************************                 
00368  FD  ERNOTE.                                                      
00369                                                                   
00370      COPY ERCDMDNT.                                               
00371  EJECT                                                            
00372  FD  MPPROD.                                                      
00373                                                                   
00374      COPY MPCPROD.                                                
00375  EJECT                                                            
00376  FD  MPPLAN.                                                      
00377                                                                   
00378      COPY MPCPLAN.                                                

061013 FD  ELCRTT.
061013                                 COPY ELCCRTT.

00382  SD  SORT-WORK-FILE.                                              
00383                                                                   
00384      COPY ELCEXTR.                                                
00385                                                                   
00386  EJECT                                                            
00387                                                                   
00388  FD  DISK-DATE                                                    
00389      COPY ELCDTEFD.                                               
00390                                                                   
00391  EJECT                                                            
00392                                                                   
00393  WORKING-STORAGE SECTION.                                         
00394  77  FILLER  PIC X(32)   VALUE '********************************'.
00395  77  FILLER  PIC X(32)   VALUE '*     EL310  WORKING STORAGE   *'.
00396  77  FILLER  PIC X(32)   VALUE '******* VMOD=2.085 *************'.
00397                                                                   
CIDMOD 77  DIS-HEAD-SW             PIC X       VALUE 'Y'.               
CIDMOD 77  ERROR-COUNT             PIC 9(7)    VALUE ZEROS.             
CIDMOD 77  DIS-LINE-CNT            PIC 99      VALUE ZEROS.             
       77  WS-LETTER-ORIGIN        PIC X       VALUE SPACES.
       77  WS-PDEF-SAVE-KEY        PIC X(16)   VALUE SPACES.
032612 77  ws-term-tmp             pic s999  comp-3 value +0.
032612 77  ws-months-disabled      pic s9(5) comp-3 value +0.
061013 77  s1                      pic s999  comp-3 value +0.
061013 77  s2                      pic s999  comp-3 value +0.
102516 77  ws-bin-ahl-eom-dt           pic xx value low-values.
102516 77  ws-sql-code                 pic s9(7) value zeros.
102516 77  ws-dis-sql-code             pic -9999999 value zeros.
050619 77  ws-auto-pay-sw          pic x value ' '.
050619     88  looking-for-auto-pay-setup   value 'Y'.
050619 77  ws-corres-sw            pic x value ' '.
050619     88  looking-for-orig-letter      value 'Y'.
050619 77  ws-letter-sent          pic x(4) value spaces.
051619 77  ws-323g-letter          pic x(4) value spaces.
050619 77  ws-323g-sw              pic x value ' '.
050619     88  looking-for-323g-letter      value 'Y'.
050619 77  ws-323g-maint-user      pic x(4) value spaces.
071019 77  ws-stuff-ind            pic x value spaces.
071019     88  trlr-stuff-needed     value 'Y'.

CVTDEL*EXEC SQL
CVTDEL*   INCLUDE SQLDA
CVTDEL*END-EXEC
102516
102516 EXEC SQL
102516    INCLUDE SQLCA
102516 END-EXEC
102516
102516 01  P pointer.
102516 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
102516 01  var-ptr pointer.
102516 01  env-var-len                 pic 9(4)  binary.
102516 01  rc                          pic 9(9)  binary.
102516
102516 01  WS-KIXSYS.
102516     05  WS-KIX-FIL1             PIC X(10).
102516     05  WS-KIX-APPS             PIC X(10).
102516     05  WS-KIX-ENV              PIC X(10).
102516     05  WS-KIX-MYENV            PIC X(10).
102516     05  WS-KIX-SYS              PIC X(10).
102516
102516 EXEC SQL
102516    BEGIN DECLARE SECTION
102516 END-EXEC
102516
102516 01  sqlcmd                      pic x(1024).
102516 01  WS-MOE-DATE                 pic x(10).
102516 01  svr                         pic x(32).
102516 01  usr                         pic x(32).
102516 01  pass                        pic x(32).
102516 01  usr-pass                    pic x(64).
102516 01  ws-disp-code                pic s9(11).
102516
102516 01  PARM-DATES-RECORD.
102516     05  PD-AHL-EOM-DT           PIC X(8).
102516
102516 EXEC SQL
102516    END DECLARE SECTION
102516 END-EXEC

CIDMOD 01  DISPLAY-HD-1.                                                
CIDMOD     12  FILLER      PIC X     VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X(51) VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X(23) VALUE 'PROCESSING ERROR REPORT'.   
CIDMOD     12  FILLER      PIC X(51) VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X(07) VALUE ' EL-310'.                   
CIDMOD                                                                  
CIDMOD 01  DISPLAY-HD-2.                                                
CIDMOD     12  FILLER      PIC X     VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X     VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X(56) VALUE SPACES.                      
CIDMOD     12  DIS-DATE    PIC X(8)  VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X(67) VALUE SPACES.                      
CIDMOD                                                                  
CIDMOD 01  DISPLAY-LINE.                                                
CIDMOD     05  DISPLAY-LINE-05.                                         
CIDMOD        10  DIS-CC              PIC X.                            
CIDMOD        10  DIS-LINE-REASON     PIC X(32).                        
CIDMOD        10  DIS-LINE-REC        PIC X(100).                       
CIDMOD                                                                  
CIDMOD     05  DISPLAY-LINE-ALT  REDEFINES  DISPLAY-LINE-05.            
CIDMOD        10  DIS-CC-ALT          PIC X.                            
CIDMOD        10  DIS-FLD-1           PIC X(52).                        
CIDMOD        10  DIS-FLD-2           PIC X(80).                        
CIDMOD                                                                  
00388                                                                   
00398  01  FILLER                          COMP-3.                      
080613     05  ws-rem-bens                 pic s9(9)v99 value +0.
00399      05  WS-RESERVE-WORK             PIC S9(5)V99   VALUE +0.     
00400      05  WS-LINE-COUNT               PIC S999    VALUE +99.       
00401      05  WS-LINE-COUNT-MAX           PIC S999    VALUE +59.       
00402      05  WS-PAGE                     PIC S9(5)   VALUE ZERO.      
00403      05  WS-REPORT-SW                PIC S9      VALUE +1.        
00404      05  WS-PURGE-SW                 PIC S9      VALUE ZERO.      
00405      05  WS-ELCHKQ-SW                PIC S9      VALUE ZERO.      
00406      05  WS-DEBUG-RESERVES-SW        PIC S9      VALUE ZERO.      
00407      05  WS-VOID-PAYMENT-SW          PIC S9      VALUE ZERO.      
00408      05  WS-F-EXTRACT-SW             PIC S9      VALUE ZERO.      
00409      05  WS-RECORD-COUNT             PIC S9(9)   VALUE ZERO.      
00410      05  WS-RETURN-CODE              PIC S999    VALUE ZERO.      
00411      05  WS-ZERO                     PIC S9      VALUE ZERO.      
00412      05  WS-ACCOUNT-NOT-FOUND        PIC S9      VALUE ZERO.      
00413      05  WS-CERT-NOT-FOUND           PIC S9      VALUE ZERO.      
00414      05  WS-BENE-NOT-FOUND           PIC S9      VALUE ZERO.      
00415      05  WS-REWRITE-CERT             PIC S9      VALUE ZERO.      
00416      05  WS-PRODUCER-NOT-FOUND       PIC S9      VALUE ZERO.      
00417      05  WS-PLAN-NOT-FOUND           PIC S9      VALUE ZERO.      
00418          88  PLAN-NOT-FOUND                      VALUE +1.        
00419      05  WS-REWRITE-POLICY           PIC S9      VALUE ZERO.      
00420      05  WS-TRAILER-NOT-FOUND        PIC S9      VALUE ZERO.      
00421      05  WS-UPDATE-TRAILER           PIC S9      VALUE ZERO.      
00422      05  WS-CERTS-ADDED              PIC S9(7)   VALUE ZERO.      
00423      05  WS-CHECK-QUEUE-DELETED      PIC S9(7)   VALUE ZERO.      
00424      05  WS-CLAIMS-DELETED           PIC S9(7)   VALUE ZERO.      
00425      05  WS-CLAIMS-PURGED            PIC S9(7)   VALUE ZERO.      
00426      05  WS-TRAILERS-DELETED         PIC S9(7)   VALUE ZERO.      
00427      05  WS-LETTER-ARCHIVES-DELETED  PIC S9(7)   VALUE ZERO.      
00428      05  WS-TIME-WRITTEN             PIC S9(7)   VALUE ZERO.      
00429      05  WS-DATE-WRITTEN             PIC S9(5)   VALUE ZERO.      
00430      05  WS-WORK-TERM                PIC S999    VALUE ZERO.      
00431                                                                   
00432      05  WS-FORCE-COUNT              PIC S999    VALUE ZERO.      
00433                                                                   
00434      05  WS-GL-PTC                   PIC S9(7)V99 VALUE ZERO.     
00435      05  WS-GL-IBNR                  PIC S9(7)V99 VALUE ZERO.     
00436      05  WS-GL-FUTURE                PIC S9(7)V99 VALUE ZERO.     
00437                                                                   
00438      05  WS-AT-CHG-EXP               PIC S9(5)V99 VALUE ZERO.     
00439      05  WS-AT-NON-CHG-EXP           PIC S9(5)V99 VALUE ZERO.     
00440      05  WS-CHG-EXP                  PIC S9(5)V99 VALUE ZERO.     
00441      05  WS-NON-CHG-EXP              PIC S9(5)V99 VALUE ZERO.     
00442                                                                   
00443      05  WS-ISSUE-AGE                PIC S9(5)   VALUE ZERO.      
00444                                                                   
00445      05  WS-MONTHS-WORK              PIC S999    VALUE ZERO.      
00446                                                                   
00447      05  WS-MONTH-END                PIC S9      VALUE ZERO.      
00448          88  THIS-IS-NOT-MONTH-END               VALUE ZERO.      
00449          88  THIS-IS-MONTH-END                   VALUE +1.        
00450                                                                   
00451      05  WS-PURGED-STATUS            PIC S9      VALUE ZERO.      
00452          88  NOT-PURGED                          VALUE +0.        
00453          88  PURGED-CLAIM-LOOKUP                 VALUE +1.        
00454                                                                   
00455      05  WS-NO-BENEFIT               PIC S9      VALUE ZERO.      
00456          88  BENEFIT-FOUND                       VALUE ZERO.      
00457          88  BENEFIT-NOT-FOUND                   VALUE +1.        
00458                                                                   
00459      05  WS-CURRENT-TIME             PIC S9(7)   VALUE ZERO.      
00460                                                                   
00461      05  WS-RECORDS-RELEASED         PIC S9(7)   VALUE ZERO.      
00462      05  WS-RECORDS-RETURNED         PIC S9(7)   VALUE ZERO.      
00463                                                                   
00464      05  WS-CLAIM-COUNT              PIC S9(7)   VALUE ZERO.      
00465      05  WS-TRAILER-COUNT            PIC S9(7)   VALUE ZERO.      
00466      05  WS-EXTRACT-AA-COUNT         PIC S9(7)   VALUE ZERO.      
00467      05  WS-EXTRACT-AB-COUNT         PIC S9(7)   VALUE ZERO.      
00468      05  WS-EXTRACT-BA-COUNT         PIC S9(7)   VALUE ZERO.      
00469      05  WS-EXTRACT-CA-COUNT         PIC S9(7)   VALUE ZERO.      
00470      05  WS-EXTRACT-DA-COUNT         PIC S9(7)   VALUE ZERO.      
00471      05  WS-EXTRACT-DB-COUNT         PIC S9(7)   VALUE ZERO.      
00472      05  WS-EXTRACT-DC-COUNT         PIC S9(7)   VALUE ZERO.      
00473      05  WS-EXTRACT-DD-COUNT         PIC S9(7)   VALUE ZERO.      
00474      05  WS-EXTRACT-DE-COUNT         PIC S9(7)   VALUE ZERO.      
00475      05  WS-EXTRACT-DF-COUNT         PIC S9(7)   VALUE ZERO.      
00476      05  WS-EXTRACT-DG-COUNT         PIC S9(7)   VALUE ZERO.      
00477      05  WS-EXTRACT-DH-COUNT         PIC S9(7)   VALUE ZERO.      
00478      05  WS-EXTRACT-DI-COUNT         PIC S9(7)   VALUE ZERO.      
00479      05  WS-EXTRACT-DJ-COUNT         PIC S9(7)   VALUE ZERO.      
00480      05  WS-EXTRACT-DK-COUNT         PIC S9(7)   VALUE ZERO.      
00481      05  WS-EXTRACT-DL-COUNT         PIC S9(7)   VALUE ZERO.      
00482      05  WS-EXTRACT-EA-COUNT         PIC S9(7)   VALUE ZERO.      
00483      05  WS-EXTRACT-FA-COUNT         PIC S9(7)   VALUE ZERO.      
00484      05  WS-EXTRACT-FB-COUNT         PIC S9(7)   VALUE ZERO.      
00485      05  WS-EXTRACT-FC-COUNT         PIC S9(7)   VALUE ZERO.      
00486      05  WS-EXTRACT-GA-COUNT         PIC S9(7)   VALUE ZERO.      
00487      05  WS-PURGED-COUNT             PIC S9(7)   VALUE ZERO.      
00488                                                                   
00489      05  WS-TOTAL-CLAIM-COUNT        PIC S9(7)   VALUE ZERO.      
00490      05  WS-TOTAL-TRAILER-COUNT      PIC S9(7)   VALUE ZERO.      
00491      05  WS-TOTAL-AA-COUNT           PIC S9(7)   VALUE ZERO.      
00492      05  WS-TOTAL-AB-COUNT           PIC S9(7)   VALUE ZERO.      
00493      05  WS-TOTAL-BA-COUNT           PIC S9(7)   VALUE ZERO.      
00494      05  WS-TOTAL-CA-COUNT           PIC S9(7)   VALUE ZERO.      
00495      05  WS-TOTAL-DA-COUNT           PIC S9(7)   VALUE ZERO.      
00496      05  WS-TOTAL-DB-COUNT           PIC S9(7)   VALUE ZERO.      
00497      05  WS-TOTAL-DC-COUNT           PIC S9(7)   VALUE ZERO.      
00498      05  WS-TOTAL-DD-COUNT           PIC S9(7)   VALUE ZERO.      
00499      05  WS-TOTAL-DE-COUNT           PIC S9(7)   VALUE ZERO.      
00500      05  WS-TOTAL-DF-COUNT           PIC S9(7)   VALUE ZERO.      
00501      05  WS-TOTAL-DG-COUNT           PIC S9(7)   VALUE ZERO.      
00502      05  WS-TOTAL-DH-COUNT           PIC S9(7)   VALUE ZERO.      
00503      05  WS-TOTAL-DI-COUNT           PIC S9(7)   VALUE ZERO.      
00504      05  WS-TOTAL-DJ-COUNT           PIC S9(7)   VALUE ZERO.      
00505      05  WS-TOTAL-DK-COUNT           PIC S9(7)   VALUE ZERO.      
00506      05  WS-TOTAL-DL-COUNT           PIC S9(7)   VALUE ZERO.      
00507      05  WS-TOTAL-EA-COUNT           PIC S9(7)   VALUE ZERO.      
00508      05  WS-TOTAL-FA-COUNT           PIC S9(7)   VALUE ZERO.      
00509      05  WS-TOTAL-FB-COUNT           PIC S9(7)   VALUE ZERO.      
00510      05  WS-TOTAL-FC-COUNT           PIC S9(7)   VALUE ZERO.      
00511      05  WS-TOTAL-GA-COUNT           PIC S9(7)   VALUE ZERO.      
00512      05  WS-TOTAL-PURGED-COUNT       PIC S9(7)   VALUE ZERO.      
00513      05  WS-TOTAL-CLAIMS-DELETED     PIC S9(7)   VALUE ZERO.      
00514                                                                   
00515      05  WS-AUTO-PAY-AMT             PIC S9(5)V99 VALUE ZERO.     
00516                                                                   
00517      05  WS-CHECK-COUNTER            PIC S9(7)   VALUE ZERO.      
00518      05  WS-BEGIN-CHECK-COUNTER      PIC S9(7)   VALUE ZERO.      
00519                                                                   
00520      05  WS-NUMBER-OF-PERIODS        PIC S9(4)   VALUE ZERO.      
00521      05  WS-REMAINDER                PIC S9(4)   VALUE ZERO.      
00522      05  WS-DAYS-BEFORE-CLOSED       PIC S999    VALUE ZERO.      
00523      05  WS-DAYS-IN-PERIOD           PIC S9(4)   VALUE ZERO.      
00524                                                                   
00525      05  WS-ANNUAL-DAYS              PIC S999    VALUE ZERO.      
00526      05  WS-DAILY-RATE               PIC S999V99 VALUE ZERO.      
00527                                                                   
00528      05  WS-SAVE-PMT-AMT             PIC S9(7)V99 VALUE ZERO.     
00529      05  WS-CIDA-DISCOUNT            PIC S9V9(4) VALUE ZERO.      
00530  EJECT                                                            
00531                                                                   
00532  01  FILLER                          COMP SYNC.                   
00533      05  WS-INSURED-ADDR-SEQ-NO      PIC S9(4)   VALUE +0.        
00534      05  WS-PAYEE-ADDR-SEQ-NO        PIC S9(4)   VALUE +0.        
00535      05  AAR-SUB                     PIC S9(4)   VALUE +0.        
00536      05  AAR-SUB-1                   PIC S9(4)   VALUE +0.        
00537      05  PGM-SUB                     PIC S9(4)   VALUE +310.      
00538      05  WS-INDEX                    PIC S9(4)   VALUE ZERO.      
00539      05  WS-LENGTH                   REDEFINES                    
00540          WS-INDEX                    PIC S9(4).                   
00541      05  WS-ORIGINAL-START-ARCH-NO   PIC S9(8)   VALUE +0.        
00542      05  WS-NEW-START-ARCH-NO        PIC S9(8)   VALUE +0.        
00543      05  WS-ARCHIVE-COUNTER          PIC S9(8)   COMP VALUE +0.   
00544      05  WS-ORIG-ARCH-COUNTER        PIC S9(8)   COMP VALUE +0.   
00545                                                                   
00546  01  WS-DATES.                                                    
00547      12  WS-CURRENT-DATE-CYMD        PIC 9(8).                    
00548      12  WS-CERT-EFF-DATE-CYMD       PIC 9(8).                    
00549      12  WS-PMT-VOID-DATE-CYMD       PIC 9(8).                    
00550                                                                   
CIDMOD                              COPY ELCFUNDT.
CIDMOD
00551  01  FILLER.                                                      
00552      05  WS-AUTO-CASH                PIC X VALUE SPACES.    
070909     05  WS-AUTO-END-LETTER          PIC X(4) VALUE SPACES.      
00553      05  WS-AAR-LETTER               PIC X(4) VALUE SPACES.       
00554      05  WS-ACT-MONTHS               PIC S999 COMP-3 VALUE +0.    
00555      05  WS-ACT-REMAINDER            PIC S9(5) COMP-3 VALUE +0.   
00556      05  WS-AIG-CORR-OUT             PIC X VALUE 'N'.             
00557      05  WS-AIG-LAST-PARTIAL         PIC X VALUE 'N'.             
00558      05  WS-AIG-LAST-PMT-DT          PIC XX VALUE LOW-VALUES.     
00559      05  WS-AIG-LAST-PMT             PIC X VALUE 'Y'.             
00560      05  WS-AIG-LAST-PART-DT         PIC XX VALUE LOW-VALUES.     
00561      05  WS-AIG-LAST-THRU-EXP-DT     PIC XX VALUE LOW-VALUES.     
00562      05  WS-AIG-AUTO-START-DT        PIC XX VALUE LOW-VALUES.     
00563      05  WS-AIG-AUTO-END-DT          PIC XX VALUE LOW-VALUES.     
00564      05  WS-AIG-WORK-DT              PIC XX VALUE LOW-VALUES.     
00565      05  WS-AIG-PMT-INTERFACE-SW     PIC X       VALUE 'N'.       
00566      05  WS-DISPLAY-TIME             PIC 99B99B99.                
00567      05  WS-DEL-BIN1                 PIC 9999-.                   
00568                                                                   
00569      05  WS-PURGE-CLAIM-SW           PIC X       VALUE 'N'.       
080510     
080510     05  WS-DROP-CLAIM-SW            PIC X       VALUE 'N'.
080510         88 CLAIM-DROPPED                        VALUE 'Y'.
00570                                                                   
00571      05  WS-WEEKS                    PIC S9(5)  COMP-3 VALUE +0.  
00572      05  WS-DAYS-REMAINING           PIC S9     COMP-3 VALUE +0.  
00573      05  WS-WORK-DAYS                PIC S9(5)  COMP-3 VALUE +0.  
00574                                                                   
DAN01      05  WS-SCHEDULE-END-DT          PIC XX      VALUE LOW-VALUE. 
DAN01      05  WS-LAST-PMT-TYPE            PIC X       VALUE SPACE.     
DAN01      05  WS-INTERVAL-MONTHS          PIC S9(5)  COMP-3 VALUE +0.  
DAN01                                                                   
061406     05  WS-LAST-MAINT-DT            PIC XX      VALUE LOW-VALUE.
061406     05  WS-LAST-MAINT-USER          PIC X(4)    VALUE SPACES.
050619     05  ws-auto-pay-setup-by        pic x(4)    value spaces.
061406     05  WS-LAST-MAINT-TYPE          PIC X       VALUE SPACE.
061406     05  WS-TRAILER-BODY             PIC X(165)  VALUE SPACES.
061406     05  WS-REG-TRAILER  REDEFINES  WS-TRAILER-BODY.
061406         10  FILLER                  PIC X(159).
061406         10  WS-REG-TRAILER-MAINT-DT PIC XX.
061406         10  WS-REG-TRAILER-USER-ID  PIC X(4).
061406     05  WS-RESV-TRAILER REDEFINES  WS-TRAILER-BODY.
061406         10  FILLER                  PIC X(111).
061406         10  WS-RESV-TRAILER-MAINT-DT PIC XX.
061406         10  WS-RESV-TRAILER-USER-ID  PIC X(4).
061406         10  FILLER                   PIC X(48).
061406
00575      05 WS-DMD-DMO-ACTIVITY          PIC X       VALUE SPACE.     
00576         88  AUTO-CLOSE-ACTIVITY    VALUE 'Q'.                     
00577         88  FINAL-PAYMENT-ACTIVITY VALUE 'F'.                     
00578                                                                   
00579      05  W-DMD-CHECK-NO.                                          
00580         10  W-DMD-CHECK-NO-1         PIC X(4).                    
00581         10  W-DMD-CHECK-NO-2         PIC X(1).                    
00582         10  W-DMD-CHECK-NO-3         PIC X(2).                    
00583                                                                   
00584      05 WS-DMD-SAVE-TO-WRITE.                                     
00585         10  FILLER                   PIC X(6).                    
00586         10  WS-DMD-STAT-DATE         PIC X(6).                    
00587         10  FILLER                   PIC X(55).                   
00588                                                                   
00589      05 WS-DMD-STORE-STAT-RECORD     PIC X(67)   VALUE SPACES.    
00590      05 WS-DMD-PROCESSOR-ID          PIC X(4)    VALUE SPACES.    
00591      05 WS-DMD-MAINTENANCE-DT        PIC XX      VALUE LOW-VALUES.
00592      05 WS-DMD-MAINTENANCE-TYPE      PIC XX      VALUE SPACES.    
00593      05 WS-DMD-ELSTAT-SW             PIC X       VALUE SPACES.    
00594                                                                   
00595      05 WS-DMD-NOTE-FLAG             PIC X       VALUE SPACES.    
00596         88  WS-NOTE-RECORD-TO-WRITE VALUE 'Y'.                    
00597                                                                   
00598      05  WS-DMD-LAST-PMT-RECORDED-BY PIC X(4)    VALUE SPACES.    
00599      05  WS-WRITE-EXTRACT-D-RECORD-C PIC X       VALUE 'N'.       
00600                                                                   
00601      05  WS-DOING-RESERVES-SW        PIC X       VALUE SPACES.    
00602          88  WS-DOING-RESERVES                   VALUE 'Y'.       
00603                                                                   
00604      05  WS-CAUSE-CODE               PIC XX      VALUE SPACES.    
00605                                                                   
00606      05  WS-LAST-RECORD-TYPE         PIC X       VALUE SPACES.    
00607                                                                   
00608      05  X                           PIC X.                       
00609      05  ABEND-CODE                  PIC X(4).                    
00610      05  ABEND-OPTION                PIC X.                       
00611      05  OLC-REPORT-NAME             PIC X(8) VALUE 'EL310'.      
00612                                                                   
00613      05  WS-SAVE-PRINT-RECORD        PIC X(133)  VALUE SPACES.    
00614                                                                   
00615      05  WS-SAVE-SEARCH-ARCHIVE      PIC X(90).                   
00616      05  WS-SAVE-SEARCH-CONTROL      PIC X(8).                    
00617                                                                   
00618      05  WS-LAST-COMPANY-CD          PIC X VALUE LOW-VALUES.      
00619      05  WS-LAST-CARRIER             PIC X VALUE LOW-VALUES.      
00620                                                                   
00621      05  WS-CARRIER-CONTROL          PIC X       VALUE SPACES.    
00622                                                                   
00623      05  WS-CERT-PRIME.                                           
00624        07  WS-CERT-PRIME-N           PIC 9(10).                   
00625                                                                   
00626      05  WS-REM-TRM-CALC-OPTION      PIC X VALUE SPACE.           
00627                                                                   
00628      05  WS-DISPLAY-AMOUNT           PIC Z,ZZZ,ZZ9.99-.           
00629      05  WS-DISPLAY-COUNT            PIC Z,ZZZ,ZZ9-.              
00630                                                                   
00631      05  WS-DIAGNOSIS-DESCRIP        PIC X(60)   VALUE SPACES.    
00632                                                                   
00633      05  WS-ABEND-MESSAGE            PIC X(80)   VALUE SPACES.    
00634                                                                   
00635      05  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZERO.      
00636                                                                   
00637      05  ELCNTL-FILE-STATUS          PIC XX      VALUE ZERO.      
00638      05  ELCERT-FILE-STATUS          PIC XX      VALUE ZERO.      
00639      05  ELMSTR-FILE-STATUS          PIC XX      VALUE ZERO.      
00640      05  ELTRLR-FILE-STATUS          PIC XX      VALUE ZERO.      
00641      05  ERACCT-FILE-STATUS          PIC XX      VALUE ZERO.      
00642      05  ELCHKQ-FILE-STATUS          PIC XX      VALUE ZERO.      
00643      05  ELPGMS-FILE-STATUS          PIC XX      VALUE ZERO.      
00644      05  ELARCH-FILE-STATUS          PIC XX      VALUE ZERO.      
00645      05  ELACTQ-FILE-STATUS          PIC XX      VALUE ZERO.      
           05  ERPDEF-FILE-STATUS          PIC XX      VALUE ZERO.
00646      05  ELBENE-FILE-STATUS          PIC XX      VALUE ZERO.      
00647      05  ERNOTE-FILE-STATUS          PIC XX      VALUE ZERO.      
00648      05  MPPROD-FILE-STATUS          PIC XX      VALUE ZERO.      
00649      05  MPPLAN-FILE-STATUS          PIC XX      VALUE ZERO.      
061013     05  ELCRTT-FILE-STATUS          PIC XX      VALUE ZEROS.
CIDMOD     05  DLYACTV-FILE-STATUS         PIC XX      VALUE ZERO.
00650                                                                   
00651      05  WS-FILE-ERROR-MESSAGE.                                   
00652          10  FILLER                  PIC X(24)   VALUE            
00653              'ERROR OCCURRED OPENING -'.                          
00654          10  WS-FEM-FILE-NAME        PIC X(8).                    
00655                                                                   
00656 *GNB                                                              
00657      05  WS-ERACCT                   PIC 9(09).                   
00658 *GNB                                                              
00659      05  WS-123182                   PIC XX  VALUE  X'7C7F'.      
00660      05  WS-030185                   PIC XX  VALUE  X'7FC1'.      
00661      05  WS-033183                   PIC XX  VALUE  X'7CDF'.      
00662      05  WS-022984                   PIC XX  VALUE  X'7E3D'.      
00663      05  WS-010184                   PIC XX  VALUE  X'7E21'.      
00664      05  WS-123183                   PIC XX  VALUE  X'7DFF'.      
00665      05  WS-ACTUAL-DATE              PIC XX  VALUE LOW-VALUES.    
00666      05  WS-BIRTH-DATE               PIC XX  VALUE LOW-VALUES.    
00667      05  WS-PURGE-DATE               PIC XX  VALUE LOW-VALUES.    
00668      05  WS-CLOSE-DATE               PIC XX  VALUE LOW-VALUES.    
00669      05  WS-PROJECTED-CLOSE-DATE     PIC XX  VALUE LOW-VALUES.    
00670      05  WS-CHKQ-PURGE-DATE          PIC XX  VALUE LOW-VALUES.    
00671      05  WS-LETTER-PURGE-DATE        PIC XX  VALUE LOW-VALUES.    
00672      05  WS-PROMPT-PURGE-DATE        PIC XX  VALUE LOW-VALUES.    
00673      05  WS-PAID-FROM-DT             PIC XX  VALUE LOW-VALUES.    
00674      05  WS-ACTION-DATE              PIC XX  VALUE LOW-VALUES.    
00675      05  WS-PRIOR-MONTH              PIC XX  VALUE LOW-VALUES.    
00676      05  WS-CURRENT-DATE-PLUS-2      PIC XX  VALUE LOW-VALUES.    
00677      05  WS-CURRENT-DATE-PLUS-5      PIC XX  VALUE LOW-VALUES.    
00678      05  WS-CURRENT-DATE-MINUS-2     PIC XX  VALUE LOW-VALUES.    
00679      05  WS-CURRENT-DATE-MINUS-7     PIC XX  VALUE LOW-VALUES.    
00680      05  WS-CURRENT-DATE-MINUS-45    PIC XX  VALUE LOW-VALUES.    
00681      05  WS-CURRENT-DATE-MINUS-60    PIC XX  VALUE LOW-VALUES.    
00682      05  WS-CURRENT-DATE-MINUS-90    PIC XX  VALUE LOW-VALUES.    
00683      05  WS-CURRENT-DATE-PLUS-31     PIC XX  VALUE LOW-VALUES.    
00684      05  WS-GENERATE-AUTO-PAY-DT     PIC XX  VALUE LOW-VALUES.    
00685      05  WS-ONE-YEAR-AGO             PIC XX  VALUE LOW-VALUES.    
00686      05  WS-SIX-MONTHS-AGO           PIC XX  VALUE LOW-VALUES.    
00687                                                                   
00688      05  WS-SAVE-LAST-DT             PIC XX  VALUE LOW-VALUES.    
00689      05  WS-SAVE-PAID-DT             PIC XX  VALUE LOW-VALUES.    
00690                                                                   
00691      05  WS-LAST-PROCESS-DT          PIC XX  VALUE LOW-VALUES.    
00692      05  WS-EXPIRE-DT                PIC XX  VALUE LOW-VALUES.  
092909     05  WS-PREV-CYCLE-BIN           PIC XX  VALUE LOW-VALUES.
00693                                                                   
00694      05  WS-WORK-DATE1               PIC 9(6).                    
00695      05  WS-WORK-DATE1-X             REDEFINES                    
00696          WS-WORK-DATE1.                                           
00697          10  WS-WORK-MM1             PIC 99.                      
00698          10  WS-WORK-DD1             PIC 99.                      
00699          10  WS-WORK-YY1             PIC 99.                      
00700                                                                   
00701      05  WS-AUTO-WORK-DATE.                                       
00702          10  WS-AUTO-WORK-CCYY       PIC 9(4).                    
00703          10  WS-AUTO-WORK-MM         PIC 99.                      
00704          10  WS-AUTO-WORK-DD         PIC 99.                      
00705      05  WS-AUTO-WORK-DATE-N  REDEFINES                           
00706          WS-AUTO-WORK-DATE           PIC 9(8).                    
00707                                                                   
00708      05  WS-AUTO-PAY-DAY             PIC 99      VALUE ZEROS.     
00709                                                                   
00710      05  WS-WORK-DATE2               PIC 9(6).                    
00711      05  WS-WORK-DATE2-X             REDEFINES                    
00712          WS-WORK-DATE2.                                           
00713          10  WS-WORK-MM2             PIC 99.                      
00714          10  WS-WORK-DD2             PIC 99.                      
00715          10  WS-WORK-YY2             PIC 99.                      
00716                                                                   
00717      05  WS-MONTH-END-DATE           PIC XX  VALUE LOW-VALUES.    
00718                                                                   
00719      05  WS-EFFECTIVE-DATE.                                       
00720          10  WS-YR                   PIC  99.                     
00721          10  WS-MM                   PIC  99.                     
00722          10  WS-DD                   PIC  99.                     
00723                                                                   
00724      05  WS-COMPANY-ID               PIC XXX.                     
00725      05  WS-COMPANY-CODE             PIC X.                       
00726      05  WS-CERT-ACCESS-CONTROL      PIC X.                       
00727      05  WS-CR-REM-TERM-CALC         PIC X.                       
00728                                                                   
00729      05  WS-DMD-FILES-OPEN           PIC X.                       
00730        88  DMD-FILES-OPEN         VALUE 'Y'.                      
00731        88  NEED-TO-OPEN-DMD-FILES VALUE 'N'.                      
00732                                                                   
00733      05  WS-CHECK-NUMBERING-METHOD   PIC X.                       
00734        88  CHECK-NUMBERING-MANUAL                    VALUE '1'.   
00735        88  CHECK-NUMBERING-AUTO                      VALUE '2'.   
00736        88  CHECK-NUMBERING-CARRIER                   VALUE '3'.   
00737        88  CHECK-NUMBERING-AT-PRINT                  VALUE '4'.   
00738                                                                   
00739      05  WS-CHECK-NUMBER-AREA.                                    
00740          10  WS-CHECK-CARRIER        PIC X.                       
00741          10  WS-CHECK-NUMBER         PIC 9(6).                    
00742      05  WS-CHECK-NO.                                             
00743          10  WS-CHECK-2              PIC XX.                      `
00744          10  FILLER                  PIC X(5).                    
00745                                                 
091808     05  WS-AK-CHECK-NUMBER-AREA.                  
091808         10  WS-AK-CHECK-NUMBER      PIC 9(7).
091808*    05  WS-PAYEE-STATE          PIC X(2).
091808     05  WS-SUB                  PIC S9(3) COMP-3 VALUE +0.
091808     05  WS-BEG-SUB              PIC S9(3) COMP-3 VALUE +0.
091808     05  WS-END-SUB              PIC S9(3) COMP-3 VALUE +0.
091808     05  WS-STATE-LENGTH         PIC S9(3) COMP-3 VALUE +0.
091808     05  WS-PAYEE-STATE-FOUND    PIC X(01)        VALUE 'N'.
091808         88 PAYEE-STATE-FOUND                     VALUE 'Y'.
051810     05  WS-PAYEE-CITY-STATE.
051810         10  WS-PAYEE-CITY       PIC X(28).
051810         10  WS-PAYEE-STATE      PIC XX.
091808*    05  FILLER  REDEFINES WS-PAYEE-CITY-STATE.
091808*        10  WS-PAYEE-CITY-ST OCCURS 30 TIMES PIC X(1).
00746                                                                   
00747      05  WS-PMT-APPROVAL             PIC X       VALUE SPACES.    
00748        88  WS-PMT-APPROVAL-USED                  VALUE 'Y' 'G'.   
00749      05  WS-REM-TERM-METHOD          PIC X       VALUE ZERO.      
00750      05  WS-EARNING-METHOD           PIC X       VALUE ZERO.      
00751      05  WS-COVERAGE-TYPE            PIC X       VALUE SPACES.    
00752      05  WS-SPECIAL-CALC-CODE        PIC X       VALUE SPACES.    
00753                                                                   
00754      05  WS-AUTO-PAYEE-CODE.                                      
00755          10  WS-AUTO-PAYEE-CD        PIC X       VALUE SPACES.    
00756          10  WS-AUTO-PAYEE-SEQ       PIC X       VALUE SPACES.    
00757          10  WS-AUTO-PAYEE-SEQ-NUM REDEFINES                      
00758              WS-AUTO-PAYEE-SEQ       PIC 9.                       
00759                                                                   
00760      05  WS-PAYEE-CD                 PIC X.                       
00761      05  WS-PAYEE-NAME               PIC X(30).                   
00762                                                                   
00763      05  WS-PROGRAM-OPTIONS.                                      
00764          10  WS-FREQUENCY            PIC X(4).                    
00765          10  WS-PRINT-OPTION         PIC X.                       
00766          10  WS-FORMAT-OPTION        PIC X.                       
00767          10  WS-PROCESS-OPTION       PIC X.                       
00768          10  WS-TOTAL-OPTION         PIC X.                       
00769                                                                   
00770      05  WS-COMPANY-NAME.                                         
00771          10  WS-CN-CHAR              PIC X                        
00772              OCCURS 30 TIMES         INDEXED BY CN1.              
00773                                                                   
00774      05  WS-COMPANY-NAME2.                                        
00775          10  WS-CN2-CHAR             PIC X                        
00776              OCCURS 30 TIMES         INDEXED BY CN2.              
00777                                                                   
00778      05  WS-EXPENSE-CONTROLS.                                     
00779          10  WS-EXPENSE-METHOD       PIC X.                       
00780          10  WS-EXPENSE-PERCENT      PIC S999V99 VALUE ZERO       
00781                                      COMP-3.                      
00782          10  WS-EXPENSE-DOLLAR       PIC S999V99 VALUE ZERO       
00783                                      COMP-3.                      
00784                                                                   
00785      05  WS-RESERVE-CONTROLS.                                     
00786          10  WS-MANUAL-SW        PIC X.                           
00787          10  WS-FUTURE-SW        PIC X.                           
00788          10  WS-PTC-SW           PIC X.                           
00789          10  WS-IBNR-SW          PIC X.                           
00790          10  WS-PTC-LF-SW        PIC X.                           
00791          10  WS-CDT-ACCESS-METHOD PIC X.                          
00792          10  WS-PERCENT-OF-CDT   PIC S999V99   COMP-3.            
00793                                                                   
00794      05  WS-CLAIM-CALCULATION-METHOD PIC X       VALUE SPACES.    
00795                                                                   
00796      05  WS-LIFE-OVERRIDE-L1     PIC X      VALUE SPACES.         
00797      05  WS-LIFE-OVERRIDE-L2     PIC XX     VALUE SPACES.         
00798      05  WS-LIFE-OVERRIDE-L6     PIC X(6)   VALUE SPACES.         
00799      05  WS-LIFE-OVERRIDE-L12    PIC X(12)  VALUE SPACES.         
00800                                                                   
00801      05  WS-AH-OVERRIDE-L1       PIC X      VALUE SPACES.         
00802      05  WS-AH-OVERRIDE-L2       PIC XX     VALUE SPACES.         
00803      05  WS-AH-OVERRIDE-L6       PIC X(6)   VALUE SPACES.         
00804      05  WS-AH-OVERRIDE-L12      PIC X(12)  VALUE SPACES.         
00805                                                                   
00806      05  WS-PAID-THRU-TO         PIC X       VALUE SPACES.        
00807      05  WS-AMOUNT               PIC ZZZ,ZZZ,ZZ9.99-.             
00808      05  WS-COUNT                PIC ZZZ,ZZZ,ZZ9-.                
00809                                                                   
00810      05  WS-INITIALS.                                             
00811          10  WS-INITIAL1             PIC X.                       
00812          10  WS-INITIAL2             PIC X.                       
00813                                                                   
00814      05  WS-BIN-DATE-WORK-X.                                      
00815          10  WS-BIN-DATE-WORK        PIC S9(4)                    
00816                                      COMP.                        
00817                                                                   
00818      05  WS-BENEFIT-TABLE        OCCURS 2 TIMES                   
00819          INDEXED BY BENEFIT-INDEX1.                               
00820                                                                   
00821          10  WS-BENEFIT-MAX          PIC S9(9)                    
00822                                      COMP SYNC.                   
00823                                                                   
CIDMOD         10  WS-BENEFIT-TABLE-ENTRY   OCCURS 600 TIMES            
00825              INDEXED BY BENEFIT-INDEX2.                           
00826                                                                   
00827              15  WS-BENEFIT-CODE     PIC XX.                      
00828              15  WS-LF-COVERAGE-TYPE PIC X.                       
00829              15  WS-CO-REM-TERM-CALC PIC X.                       
00830              15  WS-CO-EARNINGS-CALC PIC X.                       
00831              15  WS-SPECIAL-CALC-CD  PIC X.                       
00832                                                                   
CIDMOD     05  WS-CSO-ACTION-TYPE      PIC X       VALUE SPACES.        
CIDMOD     05  DLYACTV-OUT             PIC 9(7)    VALUE ZERO.          
CIDMOD     05  DLYACTV-DUP             PIC 9(7)    VALUE ZERO.          
012511     05  WS-SAVE-TRAILER-CONTROL PIC X(22)   VALUE SPACES.
00805                                                                   
00833 ******************************************************************
00834 *             AUTOMATIC ACTIVITY AREA                            *
00835 ******************************************************************
00836      05  WS-AUTO-ACTIVITY-SW                PIC X       VALUE 'N'.
00837                                                                   
00838      05  WS-AUTO-ACTIVITY-REC.                                    
00839          10  WS-SYSTEM-DEFINED-ACTIVITY OCCURS 9 TIMES.           
00840              15  WS-SYS-ACTIVE-SW           PIC X.                
00841              15  WS-SYS-LETTER-ID           PIC X(4).             
00842              15  WS-SYS-RESEND-DAYS         PIC 999.              
00843              15  WS-SYS-FOLLOW-UP-DAYS      PIC 999.              
00844              15  WS-SYS-RESET-SW            PIC X.                
00845              15  WS-SYS-REPORT-DAYS         PIC 999.              
00846              15  WS-SYS-EACH-DAY-AFTER-SW   PIC X.                
00847                                                                   
00848          10  FILLER                         PIC X(50).            
00849                                                                   
00850          10  WS-USER-DEFINED-ACTIVITY  OCCURS 8 TIMES.            
00851              15  WS-USER-ACTIVE-SW          PIC X.                
00852              15  WS-USER-LETTER-ID          PIC X(4).             
00853              15  WS-USER-RESEND-DAYS        PIC 999.              
00854              15  WS-USER-FOLLOW-UP-DAYS     PIC 999.              
00855              15  WS-USER-RESET-SW           PIC X.                
00856              15  WS-USER-REPORT-DAYS        PIC 999.              
00857              15  WS-USER-EACH-DAY-AFTER-SW  PIC X.                
00858              15  WS-USER-ACTIVITY-DESC      PIC X(20).            
00859                                                                   
00860 ****************************************************************  
00861 *  DO NOT MODIFY EXCEPT FOR OPTIONAL RESERVE REASONS           *  
00862 ****************************************************************  
00863      05  WS-TABLE-COMMUNICATIONS.                                 
00864          10  FILLER                  PIC  X(6)      VALUE SPACES. 
00865          10  WS-TABLE-FILE-STATUS-IND                             
00866                                      PIC  X         VALUE '1'.    
00867              88  WS-OPEN-FILE                       VALUE '1'.    
00868              88  WS-CLOSE-FILE                      VALUE '2'.    
00869              88  WS-IGNORE-OPEN-CLOSE               VALUE ' '.    
00870          10  WS-CO-CALC-INTEREST     PIC S9V9(4)    COMP-3.       
00871          10  WS-CLAS-MAXS            PIC S999       COMP-3.       
00872          10  WS-STATE-TABLE.                                      
00873              15  WS-STATE-GRP        OCCURS 75 TIMES              
00874                                      INDEXED BY STATE-NDX.        
00875                  20  WS-STATE-CODE   PIC  XX.                     
00876                  20  FILLER          PIC  X.                      
00877                  20  WS-STATE-CALC-INTEREST                       
00878                                      PIC S9V9(4) COMP-3.          
00879 ****************************************************************  
00880 *  DO NOT MODIFY EXCEPT FOR OPTIONAL RESERVE REASONS           *  
00881 ****************************************************************  
00882                                                                   
00883      05  WS-RESERVE-OPTION-SWITCH    PIC  X.                      
00884          88  OPT-RESERVE-METHOD-AUTH              VALUE 'Y'.      
00885          88  OPT-RESERVE-METHOD-NOT-AUTH          VALUE ' '       
00886                                                         'N'.      
00887  EJECT                                                            
00888                                                                   
00889  01  DCT-COMMUNICATION-AREA.                                      
00890      12  DCT-PROCESS-TYPE         PIC  X.                         
00891      12  DCT-BILLING-BANK-ID      PIC  X(05).                     
00892      12  DCT-LOGIC-BENEFICIARY-ID PIC  X(10).                     
00893      12  DCT-CREDIT-CARD-NUMBER   PIC  X(16).                     
00894      12  DCT-PRODUCT-CODE         PIC  X(02).                     
00895      12  DCT-COLUMN-ID-REQUESTED  PIC  X(02).                     
00896      12  DCT-RETURN-CODE          PIC  X(02).                     
00897      12  DCT-MAIL-CODE            PIC  X(05).                     
00898      12  DCT-DISTRIBUTION-CODE    PIC  X(04).                     
00899      12  DCT-MSA-ACCT-NO          PIC  X(07).                     
00900                                                                   
00901  EJECT                                                            
00902      COPY ELCNWA.                                                 
00903                                                                   
00904  EJECT                                                            
00905      COPY ELCCALC.                                                
00906                                                                   
00907  EJECT                                                            
00908  01  DMO-COMMUNICATION-AREA.                                      
00909      12  DM-PROCESS-TYPE                 PIC  X.                  
00910      12  DM-RECORD-TYPE                  PIC  X(02).              
00911              88  DM-ISSUE-TRAN                VALUE 'CC'.         
00912              88  DM-CLAIM-STATUS-CHANGE       VALUE 'CS'.         
00913              88  DM-CLAIM-PAYMENT             VALUE 'DR'.         
00914      12  DM-DIST-CODE                    PIC  X(04).              
00915      12  DM-MAIL-CODE                    PIC  X(05).              
00916      12  DM-CREDIT-CARD-NUMBER           PIC  X(16).              
00917      12  DM-INSURED-NAME                 PIC  X(30).              
00918      12  DM-CLAIM-NO                     PIC  X(07).              
00919      12  DM-CLAIM-TYPE                   PIC  X(01).              
00920      12  DM-STATUS-DATA-AREA.                                     
00921          16  DM-CLAIM-STATUS             PIC  X(01).              
00922              88  DM-OPEN-NO-PAYMENTS              VALUE '1'.      
00923              88  DM-OPEN-WITH-PAYMENTS            VALUE '2'.      
00924              88  DM-CLOSED                        VALUE '3'.      
00925              88  DM-CLOSE-SETTLE-FINAL            VALUE '4'.      
00926              88  DM-DEFAULT                       VALUE '9'.      
00927          16  DM-STATUS-DATE              PIC  9(08).              
00928 ******YYYYMMDD                                                    
00929          16  DM-STAT-CHANGE-TYPE         PIC  X(01).              
00930              88  DM-MANUAL-CLOSE                  VALUE 'C'.      
00931              88  DM-CLAIM-DENIED                  VALUE 'D'.      
00932              88  DM-FINAL-PAYMENT                 VALUE 'F'.      
00933              88  DM-INITIAL-PAYMENT               VALUE 'I'.      
00934              88  DM-AUTO-CLOSE                    VALUE 'Q'.      
00935              88  DM-RE-OPENED                     VALUE 'R'.      
00936              88  DM-NEW-CLAIM-SETUP               VALUE 'S'.      
00937              88  DM-VOIDED-PAYMENT                VALUE 'V'.      
00938              88  DM-CLAIM-DELETED                 VALUE 'X'.      
00939          16  DM-STAT-CARRIER             PIC  X.                  
00940          16  FILLER                      PIC  X(32).              
00941      12  DM-RETURN-CODE                  PIC  X(02).              
00942                                                                   
00943  EJECT                                                            
00944      COPY ELCRSVCM.                                               
00945                                                                   
00946  EJECT                                                            
00947      COPY MPCPOLUP REPLACING WS-COMPANY-ID BY WS-MP-COMPANY-ID.   
00948                                                                   
00949  EJECT                                                            
00950      COPY MPCPLCY.                                                
00951                                                                   
00952  EJECT                                                            
00953                                                                   
00954  01  WS-HEADING1.                                                 
00955      05  FILLER                      PIC X(49)   VALUE '1'.       
00956      05  WS-H1-TITLE                 PIC X(70)   VALUE            
00957          'REPORT EXTRACT/NIGHTLY MAINTENANCE'.                    
00958      05  WS-H1-REPORT-NUMBER         PIC X(5)    VALUE 'EL310'.   
00959                                                                   
00960  01  WS-HEADING2.                                                 
00961      05  FILLER                      PIC X       VALUE SPACES.    
00962      05  WS-H2-COMPANY-ID            PIC XXX     VALUE 'XXX'.     
00963      05  FILLER                      PIC XXX     VALUE ' - '.     
00964      05  WS-H2-COMPANY-NAME          PIC X(30)   VALUE SPACES.    
00965      05  FILLER                      PIC X(19)   VALUE SPACES.    
00966      05  FILLER                      PIC X(63)   VALUE            
00967          'EXTRACT ERROR REGISTER'.                                
00968      05  FILLER                      PIC X(5)    VALUE 'PAGE'.    
00969      05  WS-H2-PAGE                  PIC ZZ,ZZ9.                  
00970      05  FILLER                      PIC XXX     VALUE SPACES.    
00971                                                                   
00972  01  WS-HEADING3.                                                 
00973      05  FILLER                      PIC X(58)   VALUE SPACES.    
00974      05  WS-H3-DATE                  PIC X(20)   VALUE SPACES.    
00975      05  FILLER                      PIC X(31)   VALUE SPACES.    
00976      05  WS-H3-RUN-TIME              PIC X(08)   VALUE SPACES.    
00977      05  FILLER                      PIC XX      VALUE SPACES.    
00978      05  WS-H3-RUN-DATE              PIC X(08)   VALUE SPACES.    
00979      05  FILLER                      PIC X(6)    VALUE SPACES.    
00980                                                                   
00981  01  WS-HEADING4.                                                 
00982      05  FILLER                      PIC X(133)  VALUE            
00983          '0          CLAIM      CERTIFICATE   EFFECTIVE'.         
00984                                                                   
00985  01  WS-HEADING5.                                                 
00986      05  FILLER                      PIC X(72)   VALUE            
00987          ' CARRIER   NUMBER       NUMBER        DATE         INSUR
00988 -        'ED'.                                                    
00989      05  FILLER                      PIC X(61)   VALUE            
00990          '  MESSAGE'.                                             
00991  EJECT                                                            
00992                                                                   
00993  01  WS-DETAIL1.                                                  
00994      05  FILLER                      PIC X(4).                    
00995      05  WS-D1-CARRIER               PIC X.                       
00996      05  FILLER                      PIC X(6).                    
00997      05  WS-D1-CLAIM-NO              PIC X(7).                    
00998      05  FILLER                      PIC X(6).                    
00999      05  WS-D1-CERT-NO               PIC X(11).                   
01000      05  FILLER                      PIC X(4).                    
01001      05  WS-D1-CERT-EFF-DT           PIC X(8).                    
01002      05  FILLER                      PIC XXX.                     
01003      05  WS-D1-INSURED-NAME          PIC X(20).                   
01004      05  FILLER                      PIC X(4).                    
01005      05  WS-D1-MESSAGE               PIC X(57).                   
01006                                                                   
01007      05  FILLER                      REDEFINES                    
01008          WS-D1-MESSAGE.                                           
01009          10  FILLER                  PIC X(35).                   
01010          10  WS-D1-BIRTH-DATE        PIC X(8).                    
01011          10  FILLER                  PIC X(14).                   
01012                                                                   
01013      05  FILLER                      REDEFINES                    
01014          WS-D1-MESSAGE.                                           
01015          10  FILLER                  PIC X(23).                   
01016          10  WS-D1-TRAILER-SEQ       PIC 9(4).                    
01017          10  FILLER                  PIC X(6).                    
01018          10  WS-D1-TRAILER-TYPE      PIC X.                       
01019          10  FILLER                  PIC X(23).                   
01020                                                                   
01021      05  FILLER                      REDEFINES                    
01022          WS-D1-MESSAGE.                                           
01023          10  FILLER                  PIC X(22).                   
01024          10  WS-D1-MSG               PIC X(20).                   
01025          10  FILLER                  PIC X(15).                   
01026                                                                   
01027      05  FILLER                      REDEFINES                    
01028          WS-D1-MESSAGE.                                           
01029          10  FILLER                  PIC X(32).                   
01030          10  WS-D1-SEQ               PIC 9(7).                    
01031          10  FILLER                  PIC XXX.                     
01032          10  WS-D1-QUE               PIC Z(7)9.                   
01033          10  FILLER                  PIC XXX.                     
01034          10  WS-D1-QUE-SEQ           PIC ZZZ9.                    
01035                                                                   
01036      05  FILLER                      REDEFINES                    
01037          WS-D1-MESSAGE.                                           
01038          10  FILLER                  PIC X(14).                   
01039          10  WS-D1-CARR-DESC         PIC X(5).                    
01040          10  WS-D1-CARR              PIC XX.                      
01041          10  WS-D1-CLAIM-DESC        PIC X(9).                    
01042          10  WS-D1-CLAIM             PIC X(8).                    
01043          10  WS-D1-CERT-DESC         PIC X(8).                    
01044          10  WS-D1-CERT              PIC X(11).                   
01045                                                                   
01046      05  FILLER                      REDEFINES                    
01047          WS-D1-MESSAGE.                                           
01048          10  FILLER                  PIC X(12).                   
01049          10  WS-D1-RETURN-CODE       PIC X.                       
01050          10  FILLER                  PIC X(44).                   
01051                                                                   
01052      05  FILLER                      REDEFINES                    
01053          WS-D1-MESSAGE.                                           
01054          10  FILLER                  PIC X(15).                   
01055          10  WS-D1-REM-TERM1         PIC ZZ9-.                    
01056          10  WS-D1-REM-TERM2         PIC ZZ9-.                    
01057          10  WS-D1-REM-TERM3         PIC ZZ9-.                    
01058          10  FILLER                  PIC X(30).                   
01059                                                                   
01060      05  FILLER                      REDEFINES                    
01061          WS-D1-MESSAGE.                                           
01062          10  FILLER                  PIC X(15).                   
01063          10  WS-D1-REM-AMT1          PIC ZZ,ZZ9.99-.              
01064          10  WS-D1-REM-AMT2          PIC ZZ,ZZ9.99-.              
01065          10  FILLER                  PIC X(22).                   
01066                                                                   
01067      05  FILLER                      REDEFINES                    
01068          WS-D1-MESSAGE.                                           
01069          10  FILLER                  PIC X(12).                   
01070          10  WS-D1-CDT-TABLE         PIC Z-.                      
01071          10  WS-D1-CDT-FACTOR        PIC ZZZZ9.9(6)-.             
01072          10  WS-D1-PTC-RESERVE       PIC ZZ,ZZ9.99-.              
01073          10  WS-D1-IBNR-RESERVE      PIC ZZ,ZZ9.99-.              
01074          10  WS-D1-FUTURE-RESERVE    PIC ZZ,ZZ9.99-.              
01075                                                                   
01076      05  FILLER                      REDEFINES                    
01077          WS-D1-MESSAGE.                                           
01078          10  FILLER                  PIC X(30).                   
01079          10  WS-D1-TRLR-SEQ          PIC 9(04).                   
01080          10  FILLER                  PIC X(23).                   
01081                                                                   
01082      05  FILLER                      PIC XX.                      
01083                                                                   
01084  01  WS-DETAIL2                      REDEFINES                    
01085      WS-DETAIL1.                                                  
01086      05  FILLER                      PIC X.                       
01087      05  WS-D2-DESC                  PIC X(30).                   
01088      05  WS-D2-COUNT                 PIC Z,ZZZ,ZZZ,ZZ9-.          
01089      05  FILLER                      PIC X(88).                   
01090                                                                   
01091  EJECT                                                            
01092      COPY ELCDATE.                                                
01093                                                                   
01094                                                                   
01095  EJECT                                                            
01096      COPY ELCJLFWS.                                               
01097                                                                   
01098  EJECT                                                            
01099      COPY ELCAIGGL.                                               
01100                                                                   
01101  EJECT                                                            
01102      COPY ELCAINP.                                                
01103                                                                   
01104  EJECT                                                            
01105                                                                   
01106      COPY ELCDTECX.                                               
01107      COPY ELCDTEVR.                                               
01108                                                                   
       01  sqlconnect-parms.
           05  p-sql-server            PIC X(30).
           05  p-sql-database          PIC X(30).
           05  p-connect-return-code   pic s9(5) comp-5.
           05  p-sql-return-message    pic x(256).

092909 LINKAGE SECTION.
092909
092909 01  PARM.
092909     05  PARM-LENGTH        COMP     PIC S9(04)    VALUE ZEROS.
092909     05  PARM-PREV-CYCLE-DT          PIC X(08)     VALUE SPACES.
092909     05  PARM-CYCLE-DT               PIC X(08)     VALUE SPACES.

102516 01  var                         pic x(30).

01110                                                                   
092909 PROCEDURE DIVISION USING PARM.                                              
01112                                                                   
01113 *************************************************************     
01114 *   THE DATE CARD ROUTINE IS USED TO PROVIDE A COMPANY ID         
01115 *   UPON WHICH CERTAIN PROCESSING OPTIONS WILL BE BASED.          
01116 *   PROCESSING DATES ARE STILL BASED UPON THE SYSTEM DATE         
01117 *   UNLESS OVERWRITTEN BY AN ACCEPT DATE.                         
01118 *************************************************************     
01119                                  COPY ELCDTERX.                   
01120 *************************************************************     
01121                                                                   
01122  EJECT                                                            
01123                                                                   
01124  0000-MAIN-LOGIC SECTION.                                         
01125                                                                   
01126      DISPLAY '******* START OF MESSAGES FROM EL310 *********'.    

102516     set P to address of KIXSYS
102516     CALL "getenv" using by value P returning var-ptr
102516     if var-ptr = null then
102516        display ' kixsys not set '
102516     else
102516        set address of var to var-ptr
102516        move 0 to env-var-len
102516        inspect var tallying env-var-len
102516          for characters before X'00' 
102516        unstring var (1:env-var-len) delimited by '/'
102516           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
102516              WS-KIX-SYS
102516        end-unstring
102516     end-if
102516
102516     display ' KIXSYS  ' ws-kix-myenv

01128      MOVE WS-CURRENT-DATE        TO  WS-H3-RUN-DATE.              
01129      MOVE WS-TIME-OF-DAY         TO  WS-H3-RUN-TIME.              
01130                                                                   
01131      MOVE 'N'                     TO WS-DMD-FILES-OPEN.           
01132                                                                   
CIDMOD     ACCEPT FUNCTION-DATE                                         
01134                                                                   
CIDMOD     MOVE 'Y'                    TO DC-FORCE-EL310-DATE-SW
CIDMOD     MOVE FUNCTION-DATE          TO DC-EL310-DATE
CIDMOD
01138      DISPLAY '     '.                                             
CIDMOD     DISPLAY ' ACCEPT-DATE - ' FUNCTION-DATE                      
01140      DISPLAY '     '.                                             
01141                                                                   
CIDMOD     MOVE WS-FN-DATE             TO  DC-GREG-DATE-CYMD
CIDMOD     MOVE 'L'                    TO  DC-OPTION-CODE
CIDMOD     MOVE ' '                    TO  DC-ERROR-CODE
CIDMOD
01148      PERFORM 8500-DATE-CONVERSION.                                
01149      IF DATE-CONVERSION-ERROR                                     
01150          DISPLAY 'DATE CONVERSION ERROR: ' DC-ERROR-CODE          
01151          MOVE '75' TO RETURN-CODE                                 
01152          GO TO ABEND-PGM.                                         
pemuni     open output display-prt

01154      PERFORM OPEN-FILES.                                          
01155                                                                   
01156      MOVE DTE-CLIENT             TO  WS-H2-COMPANY-ID.            
01157      MOVE COMPANY-NAME           TO  WS-H2-COMPANY-NAME.          
01158                                                                   
01159      MOVE DC-BIN-DATE-1          TO  BIN-RUN-DATE                 
01160                                      WS-ACTUAL-DATE.              
01161      MOVE ' '                    TO  DC-OPTION-CODE.              
01162      PERFORM 8500-DATE-CONVERSION.                                
01163      MOVE DC-GREG-DATE-CYMD      TO  WS-CURRENT-DATE-CYMD.        
01164                                                                   
020816     IF DTE-CLIENT = 'DMD' OR 'CID' OR 'DCC' or 'VPP'
01166          MOVE +57                TO WS-LINE-COUNT-MAX.            
01167                                                                   
01170                                                                   
01230  0000-CONTINUE-DATE-ROUTINE.                                      
01231                                                                   
CIDMOD     MOVE 'N'                    TO DC-FORCE-EL310-DATE-SW
CIDMOD
01232      MOVE BIN-RUN-DATE           TO  DC-BIN-DATE-1.               
01233      MOVE ' '                    TO  DC-OPTION-CODE.              
01234      PERFORM 8500-DATE-CONVERSION.                                
01235      MOVE DC-GREG-DATE-1-ALPHA   TO  WS-H3-DATE.                  
01236                                                                   
01237      MOVE SPACES                 TO  WS-DETAIL1.                  
01238      MOVE 'CURRENT DATE'         TO  WS-D1-MESSAGE.               
01239      MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-BIRTH-DATE.            
CIDMOD     DISPLAY ' '                                                  
CIDMOD     DISPLAY   WS-DETAIL1                                         
01240      MOVE WS-DETAIL1             TO  PRT.                         
01241      PERFORM WRITE-A-LINE.                                        
01242                                                                   
01243      MOVE SPACES                 TO  WS-DETAIL1.                  
01244      MOVE 'RUN DATE'             TO  WS-D1-MESSAGE.               
01245      MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-BIRTH-DATE.            
CIDMOD     DISPLAY ' '                                                  
CIDMOD     DISPLAY   WS-DETAIL1                                         
01246      MOVE WS-DETAIL1             TO  PRT.                         
01247      PERFORM WRITE-A-LINE.                                        
01248                                                                   
01249      MOVE BIN-RUN-DATE           TO  DC-BIN-DATE-1.               
01250      MOVE -2                     TO  DC-ELAPSED-MONTHS.           
01251      MOVE +1                     TO  DC-ELAPSED-DAYS.             
01252      MOVE '6'                    TO  DC-OPTION-CODE.              
01253      PERFORM 8500-DATE-CONVERSION.                                
01254      MOVE DC-BIN-DATE-2          TO  WS-PRIOR-MONTH.              
01255                                                                   
01256      MOVE SPACES                 TO  WS-DETAIL1.                  
01257      MOVE 'PRIOR MONTH BEGIN'    TO  WS-D1-MESSAGE.               
01258      MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-BIRTH-DATE.            
01259      MOVE WS-DETAIL1             TO  PRT.                         
01260      PERFORM WRITE-A-LINE.                                        
01261                                                                   
01262      MOVE BIN-RUN-DATE           TO  DC-BIN-DATE-1.               
01263      MOVE +2                     TO  DC-ELAPSED-DAYS.             
01264      MOVE ZERO                   TO  DC-ELAPSED-MONTHS.           
01265      MOVE '6'                    TO  DC-OPTION-CODE.              
01266      PERFORM 8500-DATE-CONVERSION.                                
01267      MOVE DC-BIN-DATE-2          TO  WS-CURRENT-DATE-PLUS-2.      
01268                                                                   
01269      MOVE SPACES                 TO  WS-DETAIL1.                  
01270      MOVE 'CURRENT DATE PLUS 2 DAYS'  TO  WS-D1-MESSAGE.          
01271      MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-BIRTH-DATE.            
01272      MOVE WS-DETAIL1             TO  PRT.                         
01273      PERFORM WRITE-A-LINE.                                        
01274                                                                   
01275      MOVE BIN-RUN-DATE           TO  DC-BIN-DATE-1.               
01276      MOVE +5                     TO  DC-ELAPSED-DAYS.             
01277      MOVE ZERO                   TO  DC-ELAPSED-MONTHS.           
01278      MOVE '6'                    TO  DC-OPTION-CODE.              
01279      PERFORM 8500-DATE-CONVERSION.                                
01280      MOVE DC-BIN-DATE-2          TO  WS-CURRENT-DATE-PLUS-5.      
01281                                                                   
01282      MOVE SPACES                 TO  WS-DETAIL1.                  
01283      MOVE 'CURRENT DATE PLUS 5 DAYS'  TO  WS-D1-MESSAGE.          
01284      MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-BIRTH-DATE.            
01285      MOVE WS-DETAIL1             TO  PRT.                         
01286      PERFORM WRITE-A-LINE.                                        
01287                                                                   
01288      MOVE BIN-RUN-DATE           TO  DC-BIN-DATE-1.               
01289      MOVE +31                    TO  DC-ELAPSED-DAYS.             
01290      MOVE ZERO                   TO  DC-ELAPSED-MONTHS.           
01291      MOVE '6'                    TO  DC-OPTION-CODE.              
01292      PERFORM 8500-DATE-CONVERSION.                                
01293      MOVE DC-BIN-DATE-2          TO  WS-CURRENT-DATE-PLUS-31.     
01294                                                                   
01295      MOVE SPACES                 TO  WS-DETAIL1.                  
01296      MOVE 'CURRENT DATE PLUS 31 DAYS'  TO  WS-D1-MESSAGE.         
01297      MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-BIRTH-DATE.            
01298      MOVE WS-DETAIL1             TO  PRT.                         
01299      PERFORM WRITE-A-LINE.                                        
01300                                                                   
01301      MOVE BIN-RUN-DATE           TO  DC-BIN-DATE-1.               
01302      MOVE -2                     TO  DC-ELAPSED-DAYS.             
01303      MOVE ZERO                   TO  DC-ELAPSED-MONTHS.           
01304      MOVE '6'                    TO  DC-OPTION-CODE.              
01305      PERFORM 8500-DATE-CONVERSION.                                
01306      MOVE DC-BIN-DATE-2          TO  WS-CURRENT-DATE-MINUS-2.     
01307                                                                   
01308      MOVE SPACES                 TO  WS-DETAIL1.                  
01309      MOVE 'CURRENT DATE MINUS 2 DAYS' TO  WS-D1-MESSAGE.          
01310      MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-BIRTH-DATE.            
01311      MOVE WS-DETAIL1             TO  PRT.                         
01312      PERFORM WRITE-A-LINE.                                        
01313                                                                   
01314      MOVE BIN-RUN-DATE           TO  DC-BIN-DATE-1.               
01315      MOVE -7                     TO  DC-ELAPSED-DAYS.             
01316      MOVE ZERO                   TO  DC-ELAPSED-MONTHS.           
01317      MOVE '6'                    TO  DC-OPTION-CODE.              
01318      PERFORM 8500-DATE-CONVERSION.                                
01319      MOVE DC-BIN-DATE-2          TO  WS-CURRENT-DATE-MINUS-7.     
01320                                                                   
01321      MOVE SPACES                 TO  WS-DETAIL1.                  
01322      MOVE 'CURRENT DATE MINUS 7 DAYS'  TO  WS-D1-MESSAGE.         
01323      MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-BIRTH-DATE.            
01324      MOVE WS-DETAIL1             TO  PRT.                         
01325      PERFORM WRITE-A-LINE.                                        
01326                                                                   
01327      MOVE BIN-RUN-DATE           TO  DC-BIN-DATE-1.               
01328      MOVE -45                    TO  DC-ELAPSED-DAYS.             
01329      MOVE ZERO                   TO  DC-ELAPSED-MONTHS.           
01330      MOVE '6'                    TO  DC-OPTION-CODE.              
01331      PERFORM 8500-DATE-CONVERSION.                                
01332      MOVE DC-BIN-DATE-2          TO  WS-CURRENT-DATE-MINUS-45.    
01333                                                                   
01334      MOVE SPACES                 TO  WS-DETAIL1.                  
01335      MOVE 'CURRENT DATE MINUS 45 DAYS'  TO  WS-D1-MESSAGE.        
01336      MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-BIRTH-DATE.            
01337      MOVE WS-DETAIL1             TO  PRT.                         
01338      PERFORM WRITE-A-LINE.                                        
01339                                                                   
01340      MOVE BIN-RUN-DATE           TO  DC-BIN-DATE-1.               
01341      MOVE -60                    TO  DC-ELAPSED-DAYS.             
01342      MOVE ZERO                   TO  DC-ELAPSED-MONTHS.           
01343      MOVE '6'                    TO  DC-OPTION-CODE.              
01344      PERFORM 8500-DATE-CONVERSION.                                
01345      MOVE DC-BIN-DATE-2          TO  WS-CURRENT-DATE-MINUS-60.    
01346                                                                   
01347      MOVE SPACES                 TO  WS-DETAIL1.                  
01348      MOVE 'CURRENT DATE MINUS 60 DAYS'  TO  WS-D1-MESSAGE.        
01349      MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-BIRTH-DATE.            
01350      MOVE WS-DETAIL1             TO  PRT.                         
01351      PERFORM WRITE-A-LINE.                                        
01352                                                                   
01353      MOVE BIN-RUN-DATE           TO  DC-BIN-DATE-1.               
01354      MOVE -90                    TO  DC-ELAPSED-DAYS.             
01355      MOVE ZERO                   TO  DC-ELAPSED-MONTHS.           
01356      MOVE '6'                    TO  DC-OPTION-CODE.              
01357      PERFORM 8500-DATE-CONVERSION.                                
01358      MOVE DC-BIN-DATE-2          TO  WS-CURRENT-DATE-MINUS-90.    
01359                                                                   
01360      MOVE SPACES                 TO  WS-DETAIL1.                  
01361      MOVE 'CURRENT DATE MINUS 90 DAYS'  TO  WS-D1-MESSAGE.        
01362      MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-BIRTH-DATE.            
01363      MOVE WS-DETAIL1             TO  PRT.                         
01364      PERFORM WRITE-A-LINE.                                        
01365                                                                   
01366      MOVE BIN-RUN-DATE           TO  DC-BIN-DATE-1.               
01367      MOVE ZERO                   TO  DC-ELAPSED-DAYS.             
01368      MOVE -12                    TO  DC-ELAPSED-MONTHS.           
01369      MOVE '6'                    TO  DC-OPTION-CODE.              
01370      PERFORM 8500-DATE-CONVERSION.                                
01371      MOVE DC-BIN-DATE-2          TO  WS-ONE-YEAR-AGO.             
01372                                                                   
01373      MOVE SPACES                 TO  WS-DETAIL1.                  
01374      MOVE 'ONE YEAR AGO'         TO  WS-D1-MESSAGE.               
01375      MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-BIRTH-DATE.            
01376      MOVE WS-DETAIL1             TO  PRT.                         
01377      PERFORM WRITE-A-LINE.                                        
01378                                                                   
01379      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      
01380                                                                   
01381      MOVE WS-TIME                TO  WS-CURRENT-TIME.             
01382                                                                   
01383      MOVE -1                     TO  DC-ELAPSED-MONTHS.           
01384      MOVE ZERO                   TO  DC-ELAPSED-DAYS.             
01385      MOVE '6'                    TO  DC-OPTION-CODE.              
01386      PERFORM 8500-DATE-CONVERSION.                                
01387      MOVE DC-BIN-DATE-2          TO  WS-PROMPT-PURGE-DATE.        
01388                                                                   
01389      MOVE SPACES                 TO  WS-DETAIL1.                  
01390      MOVE 'PROMPT PURGE DATE'    TO  WS-D1-MESSAGE.               
01391      MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-BIRTH-DATE.            
01392      MOVE WS-DETAIL1             TO  PRT.                         
01393      PERFORM WRITE-A-LINE.                                        
01394                                                                   
01395      MOVE BIN-RUN-DATE           TO  DC-BIN-DATE-1.               
01396      MOVE -3                     TO  DC-ELAPSED-MONTHS.           
01397      MOVE ZERO                   TO  DC-ELAPSED-DAYS.             
01398      MOVE '6'                    TO  DC-OPTION-CODE.              
01399      PERFORM 8500-DATE-CONVERSION.                                
01400      MOVE DC-BIN-DATE-2          TO  WS-CHKQ-PURGE-DATE.          
01401                                                                   
01402      MOVE SPACES                 TO  WS-DETAIL1.                  
01403      MOVE 'CHECK QUEUE PURGE DATE'  TO  WS-D1-MESSAGE.            
01404      MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-BIRTH-DATE.            
01405      MOVE WS-DETAIL1             TO  PRT.                         
01406      PERFORM WRITE-A-LINE.                                        
01407                                                                   
01408      MOVE BIN-RUN-DATE           TO  DC-BIN-DATE-1.               
01409      MOVE -3                     TO  DC-ELAPSED-MONTHS.           
01410      MOVE ZERO                   TO  DC-ELAPSED-DAYS.             
01411      MOVE '6'                    TO  DC-OPTION-CODE.              
01412      PERFORM 8500-DATE-CONVERSION.                                
01413      MOVE DC-BIN-DATE-2          TO  WS-LETTER-PURGE-DATE.        
01414                                                                   
01415      MOVE SPACES                 TO  WS-DETAIL1.                  
01416      MOVE 'LETTER PURGE DATE'    TO  WS-D1-MESSAGE.               
01417      MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-BIRTH-DATE.            
01418      MOVE WS-DETAIL1             TO  PRT.                         
01419      PERFORM WRITE-A-LINE.                                        
092909   
092909     MOVE PARM-PREV-CYCLE-DT     TO  DC-GREG-DATE-CYMD.
092909     MOVE 'L'                    TO  DC-OPTION-CODE.
092909     MOVE ' '                    TO  DC-ERROR-CODE.
092909     PERFORM 8500-DATE-CONVERSION.
092909     IF DATE-CONVERSION-ERROR
092909         DISPLAY 'PREV CYCLE DATE CONVERSION ERROR: ' 
092909                 DC-ERROR-CODE  '  USING CURR DT - 2'
092909         MOVE WS-CURRENT-DATE-MINUS-2 TO  WS-PREV-CYCLE-BIN
092909     ELSE
092909         MOVE DC-BIN-DATE-1      TO  WS-PREV-CYCLE-BIN
092909     END-IF.
092909
01420                                                                   
01421      MOVE +99                    TO  WS-LINE-COUNT.               
01422                                                                   
01423      SORT SORT-WORK-FILE                                          
01424          ON ASCENDING KEY EX-SORT-KEY-AREAS                       
01425              INPUT PROCEDURE 1000-SORT-INPUT-PROCEDURE            
01426              GIVING REPORTS-EXTRACT-FILE.                         
01427                                                                   
01428      IF SORT-RETURN GREATER THAN ZERO                             
01429          MOVE 'SORT FAILED'  TO  WS-ABEND-MESSAGE                 
01430          MOVE SORT-RETURN    TO  WS-RETURN-CODE                   
01431          GO TO ABEND-PGM.                                         
01432                                                                   
01433      PERFORM CLOSE-FILES.                                         
01434                                                                   
01435      IF  OPT-RESERVE-METHOD-AUTH                                  
01436          MOVE '2'                TO OP-TABLE-FILE-STATUS-IND      
01437          CALL 'ELRSVSPL' USING OPTIONAL-CALCULATION-PASS-AREA.    
01438                                                                   
01439      DISPLAY '******* END OF MESSAGES FROM EL310 ***********'.    
01440                                                                   
01441      GOBACK.                                                      
01442  EJECT                                                            
01443                                                                   
01444  1000-SORT-INPUT-PROCEDURE SECTION.                               
01445                                                                   
01446 *    NOTE ******************************************************* 
01447 *         *                                                     * 
01448 *         *      THE PROCESSING LOGIC OF THIS PROGRAM IS AS     * 
01449 *         *  FOLLOWS:                                           * 
01450 *         *                                                     * 
01451 *         *      1.  READ THE CONTROL FILE SEQUENTIALLY         * 
01452 *         *          PROCESSING EACH COMPANY RECORD.            * 
01453 *         *                                                     * 
01454 *         *      2.  SET THE OPTIONS FOR THIS COMPANY           * 
01455 *         *                                                     * 
01456 *         *      3.  PROCESS ALL OF THE CLAIMS FOR THIS         * 
01457 *         *          COMPANY                                    * 
01458 *         *                                                     * 
01459 *         *          A.  READ THE CLAIM RECORD                  * 
01460 *         *          B.  READ THE CERTIFICATE                   * 
01461 *         *          C.  READ THE ACCOUNT                       * 
01462 *         *          D.  READ ALL THE ACTIVITY TRAILERS FOR     * 
01463 *         *              THIS CLAIM                             * 
01464 *         *                                                     * 
01465 *         *      4.  GO TO STEP 1.                              * 
01466 *         *******************************************************.
01467                                                                   
01468      MOVE LOW-VALUES             TO  CF-CONTROL-PRIMARY.          
01469                                                                   
01470  EJECT                                                            
01471                                                                   
01472  1010-SIP.                                                        
01473                                                                   
01474      START ELCNTL                                                 
01475          KEY IS GREATER THAN CF-CONTROL-PRIMARY.                  
01476                                                                   
01477      IF ELCNTL-FILE-STATUS = '23'                                 
01478          GO TO 1990-SIP.                                          
01479                                                                   
01480      IF ELCNTL-FILE-STATUS NOT = '00'                             
01481          MOVE 'ERROR OCCURRED START INITIAL - ELCNTL'             
01482                                  TO  WS-ABEND-MESSAGE             
01483          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01484          GO TO ABEND-PGM.                                         
01485                                                                   
01486  1015-SIP.                                                        
01487                                                                   
01488      READ ELCNTL NEXT INTO WS-SAVE-CONTROL-FILE                   
01489                                                                   
01490      IF ELCNTL-FILE-STATUS = '10'                                 
01491          GO TO 1990-SIP.                                          
01492                                                                   
01493      IF ELCNTL-FILE-STATUS NOT = ZERO                             
01494          MOVE 'ERROR OCCURRED READNEXT - ELCNTL'                  
01495                                  TO  WS-ABEND-MESSAGE             
01496          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01497          GO TO ABEND-PGM.                                         
01498                                                                   
01499      IF CF-RECORD-TYPE NOT = '1'                                  
01500          GO TO 1015-SIP.                                          
01501                                                                   
01502      IF CO-HAS-CLAS-IC-CLAIM                                      
01503          NEXT SENTENCE                                            
01504        ELSE                                                       
01505          GO TO 1015-SIP.                                          
01506                                                                   
01507      MOVE CF-STARTING-ARCH-NO    TO WS-ORIGINAL-START-ARCH-NO.    
01508      MOVE +9999999               TO WS-NEW-START-ARCH-NO.         
01509                                                                   
01510      MOVE CF-LIFE-OVERRIDE-L1    TO WS-LIFE-OVERRIDE-L1.          
01511      MOVE CF-LIFE-OVERRIDE-L2    TO WS-LIFE-OVERRIDE-L2.          
01512      MOVE CF-LIFE-OVERRIDE-L6    TO WS-LIFE-OVERRIDE-L6.          
01513      MOVE CF-LIFE-OVERRIDE-L12   TO WS-LIFE-OVERRIDE-L12.         
01514                                                                   
01515      MOVE CF-AH-OVERRIDE-L1      TO WS-AH-OVERRIDE-L1.            
01516      MOVE CF-AH-OVERRIDE-L2      TO WS-AH-OVERRIDE-L2.            
01517      MOVE CF-AH-OVERRIDE-L6      TO WS-AH-OVERRIDE-L6.            
01518      MOVE CF-AH-OVERRIDE-L12     TO WS-AH-OVERRIDE-L12.           
01519                                                                   
01520      IF CF-VALID-REM-TRM-OPTION                                   
01521          MOVE CF-REM-TRM-CALC-OPTION                              
01522                                  TO WS-REM-TRM-CALC-OPTION        
01523      ELSE                                                         
01524          MOVE SPACE              TO WS-REM-TRM-CALC-OPTION.       
01525                                                                   
01526      MOVE CF-CLAIM-PAID-THRU-TO  TO WS-PAID-THRU-TO.              
01527      MOVE CF-CL-MAIL-TO-NAME     TO WS-COMPANY-NAME               
01528                                     WS-H2-COMPANY-NAME.           
01529      MOVE CF-COMPANY-ID          TO WS-COMPANY-ID                 
01530                                     WS-H2-COMPANY-ID.             
01531      MOVE CF-COMPANY-CD          TO WS-COMPANY-CODE.              
01532      MOVE LOW-VALUES             TO WS-LAST-CARRIER.              
01533                                                                   
01534      ACCEPT WS-TIME-OF-DAY  FROM  TIME.                           
01535                                                                   
01536      MOVE WS-TIME                TO  WS-DISPLAY-TIME              
01537      INSPECT WS-DISPLAY-TIME CONVERTING SPACES TO '.'             
01538                                                                   
01539      DISPLAY 'EL310 BEGAN PROCESSING OF ' WS-H2-COMPANY-NAME      
01540         ' AT ' WS-DISPLAY-TIME UPON CONSOLE.                      
01541      DISPLAY 'EL310 BEGAN PROCESSING OF ' WS-H2-COMPANY-NAME      
01542         ' AT ' WS-DISPLAY-TIME.                                   
01543                                                                   
01544      MOVE CF-CLAIM-CUTOFF-DATE TO DC-BIN-DATE-1.                  
01545      MOVE ' '                  TO DC-OPTION-CODE.                 
01546      PERFORM 8500-DATE-CONVERSION.                                
01547                                                                   
01548      IF (CF-CLAIM-CUTOFF-DATE = SPACES OR ZEROS OR HIGH-VALUES)   
01549                   OR                                              
01550         (DATE-CONVERSION-ERROR)                                   
01551             MOVE LOW-VALUES      TO CF-CLAIM-CUTOFF-DATE.         
01552                                                                   
CIDMOD 1016-DO-DMD-CID.                                                 
01607      IF (BIN-RUN-DATE NOT LESS THAN CF-CURRENT-MONTH-END)         
01608                             OR                                    
01609         ((CF-CLAIM-CUTOFF-DATE NOT EQUAL LOW-VALUES)     AND      
01610          (BIN-RUN-DATE EQUAL TO CF-CLAIM-CUTOFF-DATE))            
01611                             OR                                    
01612         ((CF-CLAIM-CUTOFF-DATE NOT EQUAL LOW-VALUES)  AND         
01613          (BIN-RUN-DATE NOT LESS THAN CF-CLAIM-CUTOFF-DATE))       
120903            DISPLAY 'THIS IS A MONTH END'
01614             MOVE '-'                     TO  WS-DETAIL1           
01615             MOVE 'THIS IS MONTH END'     TO  WS-D1-MESSAGE        
01616             MOVE WS-DETAIL1              TO  PRT                  
01617             PERFORM WRITE-A-LINE                                  
01618             MOVE +1                      TO  WS-MONTH-END         
01619          ELSE                                                     
01620             MOVE '-'                     TO  WS-DETAIL1           
01621             MOVE 'THIS IS NOT MONTH END' TO  WS-D1-MESSAGE        
01622             MOVE WS-DETAIL1              TO  PRT                  
01623             PERFORM WRITE-A-LINE                                  
01624             MOVE ZERO                    TO  WS-MONTH-END.        
01625                                                                   
01626  1017-CONTINUE.                                                   
01627      IF (CF-CLAIM-CUTOFF-DATE NOT EQUAL LOW-VALUES  AND           
01628          BIN-RUN-DATE NOT LESS THAN CF-CLAIM-CUTOFF-DATE)         
01629             MOVE ' '                       TO  WS-DETAIL1         
01630             MOVE 'CLAIMS WILL BE CUT OFF'  TO  WS-D1-MESSAGE      
01631             MOVE WS-DETAIL1                TO  PRT                
01632             PERFORM WRITE-A-LINE.                                 
01633                                                                   
01634      MOVE CF-CURRENT-MONTH-END   TO  DC-BIN-DATE-1.               
01635      MOVE -6                     TO  DC-ELAPSED-MONTHS.           
01636      MOVE ZERO                   TO  DC-ELAPSED-DAYS.             
01637      MOVE '6'                    TO  DC-OPTION-CODE.              
01638      PERFORM 8500-DATE-CONVERSION.                                
01639      MOVE DC-BIN-DATE-2          TO  WS-SIX-MONTHS-AGO.           
01640                                                                   
01641      MOVE SPACES                 TO  WS-DETAIL1.                  
01642      MOVE 'SIX MONTHS AGO'       TO  WS-D1-MESSAGE.               
01643      MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-BIRTH-DATE.            
01644      MOVE WS-DETAIL1             TO  PRT.                         
01645      PERFORM WRITE-A-LINE.                                        
01646                                                                   
01647      MOVE CF-PAYMENT-APPROVAL-SW     TO WS-PMT-APPROVAL.          
01648      MOVE CF-CERT-ACCESS-CONTROL     TO WS-CERT-ACCESS-CONTROL.   
01649      MOVE CF-CURRENT-MONTH-END       TO WS-MONTH-END-DATE.        
01650      MOVE CF-CR-REM-TERM-CALC        TO WS-CR-REM-TERM-CALC.      
01651      MOVE CF-CARRIER-CONTROL-LEVEL   TO WS-CARRIER-CONTROL.       
01652      MOVE CF-CO-CHECK-COUNTER        TO WS-CHECK-COUNTER          
01653                                         WS-BEGIN-CHECK-COUNTER.   
01654      MOVE CF-CO-ARCHIVE-COUNTER      TO WS-ARCHIVE-COUNTER        
01655                                         WS-ORIG-ARCH-COUNTER.     
01656      MOVE CF-CLAIMS-LAST-PROCESS-DT  TO WS-LAST-PROCESS-DT.       
01657                                                                   
01658      MOVE SPACES                 TO  WS-COMPANY-NAME2.            
01659      SET CN1 TO +30.                                              
01660                                                                   
01661      MOVE CF-CO-RESERVE-OPTION-SWITCH                             
01662                                  TO WS-RESERVE-OPTION-SWITCH.     
01663      DISPLAY 'CF-CO-RESERVE-OPTION-SWITCH - '                     
01664          CF-CO-RESERVE-OPTION-SWITCH.                             
01665                                                                   
01666      IF OPT-RESERVE-METHOD-NOT-AUTH                               
01667          GO TO 1020-SIP.                                          
01668                                                                   
01669      IF CF-CO-CIDA-TABLE-DISCOUNT-PCT GREATER THAN ZERO           
01670          MOVE CF-CO-CIDA-TABLE-DISCOUNT-PCT                       
01671                                  TO WS-CIDA-DISCOUNT              
01672      ELSE                                                         
01673          MOVE +1.0               TO WS-CIDA-DISCOUNT.             
01674                                                                   
01675      IF CF-CO-CALCULATION-INTEREST NUMERIC                        
01676               AND                                                 
01677         CF-CO-CALCULATION-INTEREST GREATER THAN ZERO              
01678            MOVE CF-CO-CALCULATION-INTEREST                        
01679                                  TO WS-CO-CALC-INTEREST           
01680         ELSE                                                      
01681            MOVE +.03             TO WS-CO-CALC-INTEREST.          
01682                                                                   
01683  1020-SIP.                                                        
01684                                                                   
01685      IF WS-CN-CHAR (CN1) = SPACES                                 
01686          IF CN1 GREATER THAN +1                                   
01687              SET CN1 DOWN BY +1                                   
01688              GO TO 1020-SIP                                       
01689            ELSE                                                   
01690              GO TO 1040-SIP.                                      
01691                                                                   
01692      SET WS-LENGTH TO CN1.                                        
01693                                                                   
01694      SUBTRACT WS-LENGTH FROM +30 GIVING WS-LENGTH.                
01695      DIVIDE +2 INTO WS-LENGTH ROUNDED.                            
01696                                                                   
01697      IF WS-LENGTH NOT GREATER THAN ZERO                           
01698          GO TO 1040-SIP.                                          
01699                                                                   
01700      SET CN2 TO CN1.                                              
01701      SET CN2 UP BY WS-LENGTH.                                     
01702                                                                   
01703  1030-SIP.                                                        
01704                                                                   
01705      MOVE WS-CN-CHAR (CN1) TO WS-CN2-CHAR (CN2).                  
01706                                                                   
01707      IF CN1 GREATER THAN +1                                       
01708          SET CN1                                                  
01709              CN2 DOWN BY +1                                       
01710          GO TO 1030-SIP.                                          
01711                                                                   
01712      MOVE WS-COMPANY-NAME2       TO  WS-COMPANY-NAME.             
01713                                                                   
01714  EJECT                                                            
01715                                                                   
01716  1040-SIP.                                                        
01717                                                                   
01718 *    NOTE ******************************************************* 
01719 *         *                                                     * 
01720 *         *      GET THE PROCESSING OPTIONS FOR THIS COMPANY    * 
01721 *         *                                                     * 
01722 *         *******************************************************.
01723                                                                   
01724                                                                   
01725      MOVE WS-COMPANY-CODE        TO  PS-CONTROL-PRIMARY.          
01726      MOVE 'EL310'                TO  PS-PROGRAM-NUMBER.           
01727                                                                   
01728      READ ELPGMS.                                                 
01729                                                                   
01730      IF ELPGMS-FILE-STATUS = '23'                                 
01731          MOVE SPACES             TO  WS-PROGRAM-OPTIONS           
01732          GO TO 1900-SIP.                                          
01733                                                                   
01734      IF ELPGMS-FILE-STATUS NOT = ZERO                             
01735          MOVE 'ERROR OCCURRED READ - ELPGMS'                      
01736                                  TO  WS-ABEND-MESSAGE             
01737          MOVE ELPGMS-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01738          GO TO ABEND-PGM.                                         
01739                                                                   
01740      MOVE PS-PROGRAM-OPTIONS (1) TO  WS-PROGRAM-OPTIONS.          
01741                                                                   
01742      INSPECT WS-PROCESS-OPTION CONVERTING SPACES TO '1'.          
01743                                                                   
01744      IF PS-FREQUENCY-CODE (1) NOT = 'NONE'                        
01745          GO TO 1900-SIP.                                          
01746                                                                   
01747      IF WS-CARRIER-CONTROL = SPACES                               
01748          GO TO 1050-SIP.                                          
01749                                                                   
01750      MOVE SPACES                 TO  WS-DETAIL1.                  
01751      MOVE 'USING CONTROL LEVEL CARRIER'                           
01752                                  TO  WS-D1-MESSAGE.               
01753      MOVE WS-CARRIER-CONTROL     TO  WS-D1-BIRTH-DATE.            
01754      MOVE WS-DETAIL1             TO  PRT.                         
01755      PERFORM WRITE-A-LINE.                                        
01756                                                                   
01757      MOVE SPACES                 TO  CF-ACCESS-OF-CARRIER.        
01758      MOVE '6'                    TO  CF-RECORD-TYPE.              
01759      MOVE WS-CARRIER-CONTROL     TO  CF-CARRIER-CNTL.             
01760                                                                   
01761      PERFORM 7300-READ-CONTROL-FILE.                              
01762                                                                   
01763      IF ELCNTL-FILE-STATUS = '23'                                 
01764          PERFORM 8600-DEFAULT-CARRIER.                            
01765                                                                   
01766      MOVE CF-DAYS-BEFORE-CLOSED  TO WS-DAYS-BEFORE-CLOSED.        
01767                                                                   
01768      MOVE BIN-RUN-DATE           TO  DC-BIN-DATE-1.               
01769      MULTIPLY CF-DAYS-BEFORE-CLOSED BY -1                         
01770                                  GIVING DC-ELAPSED-DAYS.          
01771      MOVE ZERO                   TO  DC-ELAPSED-MONTHS.           
01772      MOVE '6'                    TO  DC-OPTION-CODE.              
01773      PERFORM 8500-DATE-CONVERSION.                                
01774      MOVE DC-BIN-DATE-2          TO  WS-CLOSE-DATE.               
01775                                                                   
01776      MOVE BIN-RUN-DATE           TO  DC-BIN-DATE-1.               
01777      MULTIPLY CF-MONTHS-BEFORE-PURGED BY -1                       
01778                                  GIVING DC-ELAPSED-MONTHS.        
01779      MOVE ZERO                   TO  DC-ELAPSED-DAYS.             
01780      MOVE '6'                    TO  DC-OPTION-CODE.              
01781      PERFORM 8500-DATE-CONVERSION.                                
01782      MOVE DC-BIN-DATE-2          TO  WS-PURGE-DATE.               
01783                                                                   
01784      MOVE CF-RESERVE-CONTROLS    TO  WS-RESERVE-CONTROLS.         
01785      MOVE CF-EXPENSE-CONTROLS    TO  WS-EXPENSE-CONTROLS.         
01786      MOVE CF-CHECK-NO-METHOD     TO  WS-CHECK-NUMBERING-METHOD.   
01787                                                                   
01788      MOVE CF-CLAIM-CALC-METHOD  TO  WS-CLAIM-CALCULATION-METHOD.  
01789                                                                   
01790  EJECT                               
01791                                                                   
01792  1050-SIP.                                                        
01793 *    NOTE ******************************************************* 
01794 *         *                                                     * 
01795 *         *   BUILDS THE BENEFIT TABLE FOR THE COMPANY.         * 
01796 *         *                                                     * 
01797 *         *******************************************************.
01798                                                                   
01799      MOVE ZERO                   TO  WS-BENEFIT-MAX (1)           
01800                                      WS-BENEFIT-MAX (2).          
01801                                                                   
01802      MOVE SPACES               TO  CF-HI-BEN-IN-REC.              
01803      MOVE '4'                  TO  CF-RECORD-TYPE.                
01804                                                                   
01805      START ELCNTL                                                 
01806                   KEY NOT LESS THAN CF-CONTROL-PRIMARY.           
01807                                                                   
01808      IF ELCNTL-FILE-STATUS NOT = ZERO                             
01809          MOVE 'ERROR OCCURRED START - ELCNTL BENEFIT RECORD'      
01810                                  TO  WS-ABEND-MESSAGE             
01811          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01812          GO TO ABEND-PGM.                                         
01813                                                                   
01814      MOVE SPACES                 TO  WS-LAST-RECORD-TYPE.         
01815                                                                   
01816      MOVE LOW-VALUES             TO  WS-BENEFIT-TABLE (1)         
01817                                      WS-BENEFIT-TABLE (2).        
01818                                                                   
01819      SET BENEFIT-INDEX1                                           
01820          BENEFIT-INDEX2  TO  +1.                                  
01821                                                                   
01822  EJECT                                                            
01823                                                                   
01824  1060-SIP.                                                        
01825                                                                   
01826      READ ELCNTL NEXT INTO WS-SAVE-CONTROL-FILE.                  
01827                                                                   
01828      IF ELCNTL-FILE-STATUS = '10'                                 
01829          GO TO 1070-SIP.                                          
01830                                                                   
01831      IF ELCNTL-FILE-STATUS NOT = ZERO                             
01832          MOVE 'ERROR OCCURRED READNEXT - ELCNTL BENEFIT RECORD'   
01833                                  TO  WS-ABEND-MESSAGE             
01834          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01835          GO TO ABEND-PGM.                                         
01836                                                                   
01837      MOVE +1                     TO  WS-INDEX.                    
01838                                                                   
01839      IF WS-LAST-RECORD-TYPE = SPACES                              
01840          MOVE CF-RECORD-TYPE     TO  WS-LAST-RECORD-TYPE.         
01841                                                                   
01842      IF CF-RECORD-TYPE NOT = WS-LAST-RECORD-TYPE                  
01843          MOVE CF-RECORD-TYPE     TO  WS-LAST-RECORD-TYPE          
01844          SET BENEFIT-INDEX2 DOWN BY +1                            
01845          SET WS-BENEFIT-MAX (BENEFIT-INDEX1) TO BENEFIT-INDEX2    
01846          SET BENEFIT-INDEX2 TO +1                                 
01847          IF BENEFIT-INDEX1 LESS THAN +3                           
01848              SET BENEFIT-INDEX1 UP BY +1                          
01849            ELSE                                                   
01850              MOVE 'BENEFIT TABLE LIMIT EXCEEDED - 1'              
01851                                  TO  WS-ABEND-MESSAGE             
01852              GO TO ABEND-PGM.                                     
01853                                                                   
01854      IF CF-RECORD-TYPE = '4' OR '5'                               
01855          NEXT SENTENCE                                            
01856        ELSE                                                       
01857          GO TO 1072-SIP.                                          
01858                                                                   
01859  1070-SIP.                                                        
01860      MOVE CF-BENEFIT-CODE (WS-INDEX)                              
01861        TO WS-BENEFIT-CODE (BENEFIT-INDEX1, BENEFIT-INDEX2).       
01862                                                                   
01863      MOVE CF-LF-COVERAGE-TYPE (WS-INDEX)                          
01864        TO WS-LF-COVERAGE-TYPE (BENEFIT-INDEX1, BENEFIT-INDEX2).   
01865                                                                   
01866      MOVE CF-CO-REM-TERM-CALC (WS-INDEX)                          
01867        TO WS-CO-REM-TERM-CALC (BENEFIT-INDEX1, BENEFIT-INDEX2).   
01868                                                                   
01869      MOVE CF-CO-EARNINGS-CALC (WS-INDEX)                          
01870        TO WS-CO-EARNINGS-CALC (BENEFIT-INDEX1, BENEFIT-INDEX2).   
01871                                                                   
01872      MOVE CF-SPECIAL-CALC-CD  (WS-INDEX)                          
01873        TO WS-SPECIAL-CALC-CD  (BENEFIT-INDEX1, BENEFIT-INDEX2).   
01874                                                                   
CIDMOD     IF BENEFIT-INDEX2 LESS THAN +600                             
01876          SET BENEFIT-INDEX2 UP BY +1                              
01877        ELSE                                                       
01878          MOVE 'BENEFIT TABLE MAX LIMIT EXCEEDED - 2'              
01879                                  TO  WS-ABEND-MESSAGE             
01880          GO TO ABEND-PGM.                                         
01881                                                                   
01882      IF CF-BENEFIT-CODE (WS-INDEX) LESS THAN CF-HI-BEN-IN-REC     
01883        AND WS-INDEX LESS THAN +8                                  
01884          ADD +1  TO  WS-INDEX                                     
01885          GO TO 1070-SIP.                                          
01886                                                                   
01887      GO TO 1060-SIP.                                              
01888                                                                   
01889  EJECT                                                            
01890                                                                   
01891  1072-SIP.                                                        
01892 *    NOTE ******************************************************* 
01893 *         *                                                     * 
01894 *         *   BUILDS THE AUTOMATIC ACTIVITY FOR THE COMPANY.    * 
01895 *         *                                                     * 
01896 *         *******************************************************.
01897                                                                   
01898      MOVE SPACES                 TO  WS-AUTO-ACTIVITY-REC         
01899                                      CF-HI-BEN-IN-REC.            
01900                                                                   
01901      MOVE 'T'                    TO  CF-RECORD-TYPE.              
01902                                                                   
01903      START ELCNTL KEY IS NOT LESS THAN CF-CONTROL-PRIMARY         
01904                                                                   
01905      IF ELCNTL-FILE-STATUS NOT = ZERO                             
01906         MOVE 'ERROR OCCURRED START - ELCNTL AUTO ACT RECORD'      
01907                                  TO  WS-ABEND-MESSAGE             
01908          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01909          GO TO ABEND-PGM.                                         
01910                                                                   
01911  1075-SIP.                                                        
01912                                                                   
01913      READ ELCNTL NEXT INTO WS-SAVE-CONTROL-FILE.                  
01914                                                                   
01915      IF ELCNTL-FILE-STATUS = '10'                                 
01916          GO TO 1080-SIP.                                          
01917                                                                   
01918      IF ELCNTL-FILE-STATUS NOT = ZERO                             
01919          MOVE 'ERROR OCCURRED READNEXT - ELCNTL AUTO ACT RECORD'  
01920                                  TO  WS-ABEND-MESSAGE             
01921          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01922          GO TO ABEND-PGM.                                         
01923                                                                   
01924      IF CF-RECORD-TYPE EQUAL 'T'                                  
01925         MOVE 'Y'                 TO  WS-AUTO-ACTIVITY-SW          
01926      ELSE                                                         
01927         GO TO 1080-SIP.                                           
01928                                                                   
01929      MOVE +1                     TO AAR-SUB.                      
01930                                                                   
01931  1077-SIP.                                                        
01932                                                                   
01933      MOVE CF-SYS-ACTIVE-SW (AAR-SUB)                              
01934                                  TO WS-SYS-ACTIVE-SW (AAR-SUB).   
01935                                                                   
01936      MOVE CF-SYS-LETTER-ID (AAR-SUB)                              
01937                                  TO WS-SYS-LETTER-ID (AAR-SUB).   
01938                                                                   
01939      MOVE CF-SYS-RESEND-DAYS (AAR-SUB)                            
01940                                  TO WS-SYS-RESEND-DAYS (AAR-SUB). 
01941                                                                   
01942      MOVE CF-SYS-FOLLOW-UP-DAYS (AAR-SUB)                         
01943                               TO WS-SYS-FOLLOW-UP-DAYS (AAR-SUB). 
01944                                                                   
01945      MOVE CF-SYS-RESET-SW (AAR-SUB)                               
01946                                  TO WS-SYS-RESET-SW (AAR-SUB).    
01947                                                                   
01948      MOVE CF-SYS-REPORT-DAYS (AAR-SUB)                            
01949                                  TO WS-SYS-REPORT-DAYS (AAR-SUB). 
01950                                                                   
01951      MOVE CF-SYS-EACH-DAY-AFTER-SW (AAR-SUB)                      
01952                       TO WS-SYS-EACH-DAY-AFTER-SW (AAR-SUB).      
01953                                                                   
01954      IF AAR-SUB GREATER THAN +8                                   
01955         GO TO 1080-SIP.                                           
01956                                                                   
01957      IF CF-USER-RESEND-DAYS (AAR-SUB) IS NOT NUMERIC              
01958          MOVE ZEROS          TO  CF-USER-RESEND-DAYS (AAR-SUB).   
01959                                                                   
01960      IF CF-USER-FOLLOW-UP-DAYS (AAR-SUB) IS NOT NUMERIC           
01961          MOVE ZEROS          TO  CF-USER-FOLLOW-UP-DAYS (AAR-SUB).
01962                                                                   
01963      IF CF-USER-REPORT-DAYS (AAR-SUB) IS NOT NUMERIC              
01964          MOVE ZEROS          TO  CF-USER-REPORT-DAYS (AAR-SUB).   
01965                                                                   
01966      MOVE CF-USER-ACTIVE-SW (AAR-SUB)                             
01967                                  TO WS-USER-ACTIVE-SW (AAR-SUB).  
01968                                                                   
01969      MOVE CF-USER-LETTER-ID (AAR-SUB)                             
01970                                  TO WS-USER-LETTER-ID (AAR-SUB).  
01971                                                                   
01972      MOVE CF-USER-RESEND-DAYS (AAR-SUB)                           
01973                                TO WS-USER-RESEND-DAYS (AAR-SUB).  
01974                                                                   
01975      MOVE CF-USER-FOLLOW-UP-DAYS (AAR-SUB)                        
01976                              TO WS-USER-FOLLOW-UP-DAYS (AAR-SUB). 
01977                                                                   
01978      MOVE CF-USER-RESET-SW (AAR-SUB)                              
01979                                  TO WS-USER-RESET-SW (AAR-SUB).   
01980                                                                   
01981      MOVE CF-USER-REPORT-DAYS (AAR-SUB)                           
01982                                 TO WS-USER-REPORT-DAYS (AAR-SUB). 
01983                                                                   
01984      MOVE CF-USER-EACH-DAY-AFTER-SW (AAR-SUB)                     
01985                       TO WS-USER-EACH-DAY-AFTER-SW (AAR-SUB).     
01986                                                                   
01987      IF AAR-SUB LESS THAN +10                                     
01988         ADD +1 TO AAR-SUB                                         
01989         GO TO 1077-SIP.                                           
01990                                                                   
01991      EJECT                                                        
01992                                                                   
01993  1080-SIP.                                                        
01994 *    NOTE ******************************************************* 
01995 *         *                                                     * 
01996 *         *      LOAD THE STATE TABLE FOR THE COMPANY WHEN      * 
01997 *         *      THE COMPANY IS USING THE OPTIONAL RESERVE      * 
01998 *         *      CALCULATION METHOD.                            * 
01999 *         *                                                     * 
02000 *         *******************************************************.
02001                                                                   
02002      IF OPT-RESERVE-METHOD-NOT-AUTH                               
02003          GO TO 1100-SIP.                                          
02004                                                                   
02005      MOVE SPACES                 TO CF-ACCESS-CD-GENL.            
02006      MOVE '3'                    TO CF-RECORD-TYPE.               
02007                                                                   
02008      START ELCNTL KEY IS NOT LESS THAN CF-CONTROL-PRIMARY         
02009                                                                   
02010      IF ELCNTL-FILE-STATUS NOT = ZERO                             
02011          MOVE 'ERROR OCCURRED START - ELCNTL STATE RECORD'        
02012                                  TO WS-ABEND-MESSAGE              
02013          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          
02014          GO TO ABEND-PGM.                                         
02015                                                                   
02016      MOVE SPACES                 TO WS-LAST-RECORD-TYPE.          
02017                                                                   
02018      MOVE LOW-VALUES             TO WS-STATE-TABLE.               
02019                                                                   
02020      SET STATE-NDX               TO +1.                           
02021                                                                   
02022  EJECT                                                            
02023                                                                   
02024  1082-SIP.                                                        
02025                                                                   
02026      READ ELCNTL NEXT INTO WS-SAVE-CONTROL-FILE.                  
02027                                                                   
02028      IF ELCNTL-FILE-STATUS = '10'                                 
02029          GO TO 1090-CALL-TABLE-GEN.                               
02030                                                                   
02031      IF ELCNTL-FILE-STATUS NOT = ZERO                             
02032          MOVE 'ERROR OCCURRED READNEXT - ELCNTL STATE RECORD'     
02033                                  TO WS-ABEND-MESSAGE              
02034          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          
02035          GO TO ABEND-PGM.                                         
02036                                                                   
02037      MOVE +1                     TO WS-INDEX.                     
02038                                                                   
02039      IF CF-RECORD-TYPE NOT = '3'                                  
02040          GO TO 1090-CALL-TABLE-GEN.                               
02041                                                                   
02042  1088-SIP.                                                        
02043                                                                   
02044      MOVE CF-STATE-CODE          TO WS-STATE-CODE (STATE-NDX).    
02045                                                                   
02046      IF CF-ST-CALC-INTEREST NOT NUMERIC                           
02047              OR                                                   
02048         CF-ST-CALC-INTEREST NOT GREATER THAN ZEROS                
02049          MOVE WS-CO-CALC-INTEREST                                 
02050              TO WS-STATE-CALC-INTEREST (STATE-NDX)                
02051      ELSE                                                         
02052          MOVE CF-ST-CALC-INTEREST                                 
02053              TO WS-STATE-CALC-INTEREST (STATE-NDX).               
02054                                                                   
02055      IF STATE-NDX LESS THAN +75                                   
02056          SET STATE-NDX UP BY +1                                   
02057      ELSE                                                         
02058          MOVE 'STATE TABLE MAX LIMIT EXCEEDED'                    
02059                                  TO WS-ABEND-MESSAGE              
02060          MOVE '9999'             TO WS-RETURN-CODE                
02061          GO TO ABEND-PGM.                                         
02062                                                                   
02063      GO TO 1082-SIP.                                              
02064                                                                   
02065  1090-CALL-TABLE-GEN.                                             
02066                                                                   
02067      SET WS-CLAS-MAXS            TO STATE-NDX.                    
02068      SUBTRACT +1 FROM WS-CLAS-MAXS.                               
02069      CALL 'ELTBLGEN' USING WS-TABLE-COMMUNICATIONS.               
02070                                                                   
02071  EJECT                                                            
02072                                                                   
02073  1100-SIP.                                                        
02074 *    NOTE ******************************************************* 
02075 *         *                                                     * 
02076 *         *      POSITION THE CLAIM MASTER AT THE BEGINNING     * 
02077 *         *  OF THE COMPANY THEN PROCESS ALL OF THE CLAIMS.     * 
02078 *         *                                                     * 
02079 *         *******************************************************.
02080                                                                   
02081                                                                   
02082      MOVE LOW-VALUES             TO CL-CONTROL-PRIMARY.           
02083      MOVE WS-COMPANY-CODE        TO CL-COMPANY-CD.                
02084                                                                   
02085      START ELMSTR                                                 
02086          KEY EQUAL CL-COMPANY-CD.                                 
02087                                                                   
02088      IF ELMSTR-FILE-STATUS = '23'                                 
02089          GO TO 1900-SIP.                                          
02090                                                                   
02091      IF ELMSTR-FILE-STATUS NOT = ZERO                             
02092          MOVE 'ERROR OCCURRED START - ELMSTR'                     
02093                                  TO  WS-ABEND-MESSAGE             
02094          MOVE ELMSTR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
02095          GO TO ABEND-PGM.                                         
02096                                                                   
02097  EJECT                                                            
02098                                                                   
02099  1120-SIP.                                                        
02100                                                                   
121902*    IF WS-COMPANY-ID = 'DMD'                                     
121902*       IF WS-NOTE-RECORD-TO-WRITE                                
121902*            MOVE WS-DMD-SAVE-TO-WRITE TO                         
121902*                              PROCESSOR-STATISTICS               
121902*            WRITE PROCESSOR-STATISTICS                           
121902*            MOVE ' '       TO WS-DMD-NOTE-FLAG.                  
02107                                                                   
02108      READ ELMSTR NEXT INTO WS-SAVE-CLAIM-MASTER.                  
02109                                                                   
02110      IF ELMSTR-FILE-STATUS = '10'                                 
02111          GO TO 1900-SIP.                                          
02112                                                                   
02113      IF ELMSTR-FILE-STATUS NOT = ZERO                             
02114          MOVE 'ERROR OCCURRED READNEXT - ELMSTR'                  
02115                                  TO  WS-ABEND-MESSAGE             
02116          MOVE ELMSTR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
02117          GO TO ABEND-PGM.                                         
02118                                                                   
02119 **********************************************************        
02120 *** FOR DMD : BYPASS ALL PROCESSING FOR CLAIMS WITH    ***        
02121 ***  THE LAST CLOSE REASON OF 'C' OR 'E'.              ***        
02122 **********************************************************        
02123                                                                   
121902*    IF WS-COMPANY-ID = 'DMD'                                     
121902*        IF CLAIM-IS-CLOSED                                       
121902*           IF BENEFITS-CHANGED OR SETUP-ERRORS                   
121902*               GO TO 1120-SIP.                                   
02128                                                                   
02129      IF CL-COMPANY-CD NOT = WS-COMPANY-CODE                       
02130          GO TO 1900-SIP.                                          
02131                                                                   
02132      IF CL-HISTORY-ARCHIVE-DT = SPACES                            
02133          MOVE LOW-VALUES         TO  CL-HISTORY-ARCHIVE-DT.       
02134                                                                   
02135      MOVE 'N'                    TO  WS-AIG-CORR-OUT              
02136                                      WS-AIG-LAST-PARTIAL          
02137                                      WS-AIG-PMT-INTERFACE-SW.     
02138      MOVE 'Y'                    TO  WS-AIG-LAST-PMT.             
02139      MOVE LOW-VALUES             TO  WS-AIG-LAST-PMT-DT           
02140                                      WS-AIG-LAST-PART-DT          
02141                                      WS-AIG-LAST-THRU-EXP-DT      
02142                                      WS-AIG-AUTO-START-DT         
02143                                      WS-AIG-AUTO-END-DT           
02144                                      WS-AIG-WORK-DT.              
02145                                                                   
121902*    IF WS-COMPANY-ID = 'DMD'                                     
121902*        MOVE SPACES             TO  WS-DIAGNOSIS-DESCRIP         
080510     MOVE ZEROS                  TO  WS-INSURED-ADDR-SEQ-NO       
080510                                     WS-PAYEE-ADDR-SEQ-NO.        
02150                                                                   
02151      MOVE '0'                    TO  WS-DETAIL1.                  
02152                                                                   
02153      MOVE CL-CARRIER             TO  WS-D1-CARRIER.               
02154      MOVE CL-CLAIM-NO            TO  WS-D1-CLAIM-NO.              
02155      MOVE CL-CERT-NO             TO  WS-D1-CERT-NO.               
02156                                                                   
02157      MOVE CL-CERT-EFF-DT         TO  DC-BIN-DATE-1.               
02158      MOVE SPACES                 TO  DC-OPTION-CODE.              
02159      PERFORM 8500-DATE-CONVERSION.                                
02160      MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-CERT-EFF-DT.           
02161      MOVE DC-GREG-DATE-CYMD      TO  WS-CERT-EFF-DATE-CYMD.       
02162                                                                   
02163      PERFORM 5000-MOVE-NAME.                                      
02164                                                                   
02165      MOVE WS-NAME-WORK           TO  WS-D1-INSURED-NAME.          
02166                                                                   
02167      IF CL-ACTIVITY-CODE IS NOT NUMERIC                           
02168          MOVE ZEROS              TO  CL-ACTIVITY-CODE             
02169          MOVE 'ACTIVITY CODE NOT NUMERIC - FIXED'                 
02170                                  TO  WS-D1-MESSAGE                
02171          MOVE WS-DETAIL1         TO  PRT                          
02172          PERFORM WRITE-A-LINE                                     
02173          MOVE SPACES             TO  WS-DETAIL1.                  
02174                                                                   
02175      IF CL-LAPSE-REPORT-CODE IS NOT NUMERIC                       
02176          MOVE ZEROS              TO  CL-LAPSE-REPORT-CODE         
02177          MOVE 'LAPSE REPORT CODE FIXED'                           
02178                                  TO  WS-D1-MESSAGE                
02179          MOVE WS-DETAIL1         TO  PRT                          
02180          PERFORM WRITE-A-LINE                                     
02181          MOVE SPACES             TO  WS-DETAIL1.                  
02182                                                                   
02183      IF CL-LAST-MAINT-DT = LOW-VALUES                             
02184          MOVE CL-CARRIER         TO  WS-D1-CARRIER                
02185          MOVE CL-CLAIM-NO        TO  WS-D1-CLAIM-NO               
02186          MOVE CL-CERT-NO         TO  WS-D1-CERT-NO                
02187          MOVE WS-NAME-WORK       TO  WS-D1-INSURED-NAME           
02188          MOVE BIN-RUN-DATE       TO  CL-LAST-MAINT-DT             
02189          MOVE 'LAST MAINT DATE CORRECTED'                         
02190                                  TO  WS-D1-MESSAGE                
02191          MOVE WS-DETAIL1         TO  PRT                          
02192          PERFORM WRITE-A-LINE                                     
02193          MOVE SPACES             TO  WS-DETAIL1.                  
02194                                                                   
02195 ***************************************************************   
02196 ***            'DMD' ONLY  - PROJECT 6449                   ***   
02197 *** CREATES A STATISTIC RECORD FOR SETTING UP THIS CLAIM    ***   
02198 ***************************************************************   
121902*    IF WS-COMPANY-ID = 'DMD'                                     
121902*        IF CL-FILE-ESTABLISH-DT > WS-LAST-PROCESS-DT             
121902*            MOVE '14'               TO WS-DMD-MAINTENANCE-TYPE   
121902*            MOVE CL-FILE-ESTABLISH-DT                            
121902*                                    TO WS-DMD-MAINTENANCE-DT     
121902*            MOVE CL-FILE-ESTABLISHED-BY                          
121902*                                    TO WS-DMD-PROCESSOR-ID       
121902*            IF WS-DMD-PROCESSOR-ID = SPACES                      
121902*                MOVE CL-PROCESSOR-ID TO WS-DMD-PROCESSOR-ID      
121902*                PERFORM 9500-DMD-ACTIVITY-FILE THRU 9500-EXIT    
121902*             ELSE                                                
121902*                PERFORM 9500-DMD-ACTIVITY-FILE THRU 9500-EXIT.   
02211                                                                   
02212 **************************************************************    
02213 ***   WS-CARRIER-CONTROL IS ACQUIRED FROM THE COMPANY      ***    
02214 ***   MASTER RECORD (CF-CARRIER-CONTROL-LEVEL). IF A CLIENT***    
02215 ***   ENTERS A VALID CARRIER IN THIS FIELD, THIS CARRIER   ***    
02216 ***   AND IT'S INFORMATION WILL BE USED THROUGHOUT THE     ***    
02217 ***   CLIENT'S CLAIM PROCESSING.                           ***    
02218 **************************************************************    
02219                                                                   
121902*    IF WS-COMPANY-ID = 'FIA'                                     
121902*        NEXT SENTENCE                                            
121902*    ELSE                                                         
02223          IF WS-CARRIER-CONTROL NOT = SPACES                       
02224              GO TO 1130-SIP.                                      
02225                                                                   
02226      IF CL-CARRIER = WS-LAST-CARRIER                              
02227          GO TO 1130-SIP.                                          
02228                                                                   
02229      IF WS-LAST-CARRIER = LOW-VALUES                              
02230          GO TO 1125-SIP.                                          
02231                                                                   
121902*    IF WS-COMPANY-ID = 'AIG' OR 'AUK'                            
121902*        NEXT SENTENCE                                            
121902*    ELSE                                                         
02235         IF CHECK-NUMBERING-MANUAL   OR                            
02236            CHECK-NUMBERING-AT-PRINT                               
02237             GO TO 1125-SIP.                                       
02238                                                                   
02239      IF WS-CHECK-COUNTER = WS-BEGIN-CHECK-COUNTER                 
02240          GO TO 1125-SIP.                                          
02241                                                                   
02242      MOVE SPACES                 TO  CF-ACCESS-OF-CARRIER.        
02243      MOVE '6'                    TO  CF-RECORD-TYPE.              
02244      MOVE WS-LAST-CARRIER        TO  CF-CARRIER-CNTL.             
02245      MOVE ZERO                   TO  CF-SEQUENCE-NO.              
02246                                                                   
02247      PERFORM 7300-READ-CONTROL-FILE                               
02248                                                                   
02249      IF ELCNTL-FILE-STATUS NOT = ZERO                             
02250          MOVE 'ERROR OCCURRED READ FOR UPDATE CARRIER'            
02251                                  TO  WS-ABEND-MESSAGE             
02252          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
02253          GO TO ABEND-PGM.                                         
02254                                                                   
02255      MOVE WS-CHECK-COUNTER       TO  CF-CHECK-COUNTER.            
02256                                                                   
02257      PERFORM 7900-REWRITE-CONTROL-FILE.                           
02258                                                                   
02259  EJECT                                                            
02260 ************************************************************      
02261 ***  READS THE CARRIER MASTER :                          ***      
02262 ***  - REVIEWS THE 'DAYS BEFORE CLOSE' AND THE 'MONTHS   ***      
02263 ***     BEFORE PURGE'                                    ***      
02264 ***  - MOVES THE RESERVE AND EXPENSE CONTROLS TO WORKING ***      
02265 ***      STORAGE FOR LATER REFERENCE                     ***      
02266 ************************************************************      
02267                                                                   
02268  1125-SIP.                                                        
02269      MOVE SPACES                 TO  CF-ACCESS-OF-CARRIER.        
02270      MOVE '6'                    TO  CF-RECORD-TYPE.              
02271      MOVE CL-CARRIER             TO  CF-CARRIER-CNTL              
02272                                      WS-LAST-CARRIER.             
02273      MOVE ZERO                   TO  CF-SEQUENCE-NO.              
02274                                                                   
02275      PERFORM 7300-READ-CONTROL-FILE.                              
02276                                                                   
02277      IF ELCNTL-FILE-STATUS = '23'                                 
02278          PERFORM 8600-DEFAULT-CARRIER.                            
02279                                                                   
02280      MOVE CF-DAYS-BEFORE-CLOSED  TO WS-DAYS-BEFORE-CLOSED.        
02281                                                                   
121902*    IF WS-COMPANY-ID = 'FIA'                                     
121902*        MOVE CF-CHECK-NO-METHOD TO  WS-CHECK-NUMBERING-METHOD    
121902*        MOVE CF-CHECK-COUNTER   TO  WS-CHECK-COUNTER             
121902*                                    WS-BEGIN-CHECK-COUNTER       
121902*        GO TO 1130-SIP.                                          
02287                                                                   
02288      MOVE BIN-RUN-DATE           TO  DC-BIN-DATE-1.               
02289      MULTIPLY CF-DAYS-BEFORE-CLOSED BY -1                         
02290                                  GIVING DC-ELAPSED-DAYS.          
02291      MOVE ZERO                   TO  DC-ELAPSED-MONTHS.           
02292      MOVE '6'                    TO  DC-OPTION-CODE.              
02293      PERFORM 8500-DATE-CONVERSION.                                
02294      MOVE DC-BIN-DATE-2          TO  WS-CLOSE-DATE.               
02295                                                                   
02296      MOVE BIN-RUN-DATE           TO  DC-BIN-DATE-1.               
02297      MULTIPLY CF-MONTHS-BEFORE-PURGED BY -1                       
02298                                  GIVING DC-ELAPSED-MONTHS.        
02299      MOVE ZERO                   TO  DC-ELAPSED-DAYS.             
02300      MOVE '6'                    TO  DC-OPTION-CODE.              
02301      PERFORM 8500-DATE-CONVERSION.                                
02302      MOVE DC-BIN-DATE-2          TO  WS-PURGE-DATE.               
02303                                                                   
02304      MOVE CF-RESERVE-CONTROLS    TO  WS-RESERVE-CONTROLS.         
02305      MOVE CF-EXPENSE-CONTROLS    TO  WS-EXPENSE-CONTROLS.         
02306                                                                   
02307      MOVE CF-CHECK-NO-METHOD     TO  WS-CHECK-NUMBERING-METHOD.   
02308                                                                   
02309      MOVE CF-CHECK-COUNTER       TO  WS-CHECK-COUNTER             
02310                                      WS-BEGIN-CHECK-COUNTER.      
02311                                                                   
02312      MOVE CF-CLAIM-CALC-METHOD  TO  WS-CLAIM-CALCULATION-METHOD.  
02313                                                                   
02314  EJECT                                                            
02315                                                                   
02316  1130-SIP.                                                        
02317                                                                   
02318      IF CL-PURGED-DT NOT = LOW-VALUES AND SPACES                  
02319         ADD +1                    TO WS-PURGED-COUNT              
02320 *       GO TO 2195-DELETE-CLAIM.                                  
02321         MOVE +0                 TO WS-PURGED-STATUS               
02322         GO TO 1120-SIP.                                           
02323                                                                   
02324      MOVE +0                      TO WS-FORCE-COUNT.              
02325                                                                   
121902*    IF WS-COMPANY-ID = 'AIG' OR 'AUK'                            
121902*       GO TO 1135-SKIP-AUTO-CLOSE-RTN.                           
02328                                                                   
121902*    IF (WS-COMPANY-ID = 'FLA' OR 'FIA' OR 'KSM' OR               
121902*                        'CVL' OR 'FIM' OR 'DMD')                 
121902*             AND                                                 
121902*       CL-PRIORITY-CD = '9'                                      
121902*          GO TO 1135-SKIP-AUTO-CLOSE-RTN.                        
062602
020816     IF (WS-COMPANY-ID = 'CID' OR 'DCC' or 'VPP')
062602        AND (CL-PRIORITY-CD = '8')
062602           GO TO 1135-SKIP-AUTO-CLOSE-RTN
062602     END-IF
02334                                                                   
121902*    IF WS-COMPANY-ID = 'FIA'                                     
121902*        IF (CLAIM-IS-OPEN                                        
121902*          AND CL-FILE-ESTABLISH-DT LESS THAN WS-CLOSE-DATE       
121902*          AND CL-NEXT-AUTO-PAY-DT  LESS THAN WS-CLOSE-DATE       
121902*          AND CL-NEXT-RESEND-DT    LESS THAN WS-CLOSE-DATE       
121902*          AND CL-NEXT-FOLLOWUP-DT  LESS THAN WS-CLOSE-DATE       
121902*          AND CL-LAST-PMT-DT       LESS THAN WS-CLOSE-DATE       
121902*          AND BIN-RUN-DATE NOT EQUAL WS-CLOSE-DATE)              
121902*            IF CL-CLAIM-TYPE = WS-LIFE-OVERRIDE-L1               
121902*                PERFORM 2000-CLOSE-CLAIM                         
121902*            ELSE                                                 
121902*                IF CL-PAID-THRU-DT LESS THAN WS-CLOSE-DATE       
121902*                    PERFORM 2000-CLOSE-CLAIM.                    

110916*032612IF WS-COMPANY-ID = 'AHL'
110916*032612   CONTINUE
110916*032612ELSE
02352         IF (CLAIM-IS-OPEN                                         
02353            AND CL-LAST-MAINT-DT    < WS-CLOSE-DATE        
02354            AND CL-NEXT-AUTO-PAY-DT < WS-CLOSE-DATE        
02355            AND CL-NEXT-RESEND-DT   < WS-CLOSE-DATE        
02356            AND CL-NEXT-FOLLOWUP-DT < WS-CLOSE-DATE        
02357            AND CL-LAST-PMT-DT      < WS-CLOSE-DATE        
02358            AND BIN-RUN-DATE NOT = WS-CLOSE-DATE)
100518           IF CL-CLAIM-TYPE = WS-LIFE-OVERRIDE-L1 OR 'O'
02360               PERFORM 2000-CLOSE-CLAIM                         
02361            ELSE                                                 
02362               IF CL-PAID-THRU-DT LESS THAN WS-CLOSE-DATE       
02363                  PERFORM 2000-CLOSE-CLAIM
                    END-IF
                 END-IF
              END-IF
110916*    END-IF

           .
02365  1135-SKIP-AUTO-CLOSE-RTN.                                        
02366                                                                   
121902*    IF (WS-COMPANY-ID = 'AIG' OR 'AUK')                          
121902*        IF CLAIM-IS-CLOSED                                       
121902*            IF CL-ACTIVITY-CODE IS GREATER THAN 09               
121902*                GO TO 1140-SIP.                                  
02371                                                                   
121902*    IF (WS-COMPANY-ID = 'AIG' OR 'AUK')                          
121902*        IF CLAIM-IS-CLOSED                                       
121902*            IF (CL-NEXT-RESEND-DT = LOW-VALUES AND               
121902*                CL-NEXT-FOLLOWUP-DT = LOW-VALUES)                
121902*                NEXT SENTENCE                                    
121902*            ELSE  
121902*                GO TO 1140-SIP.                                  
02379                                                                   
02380 *    IF CLAIM-IS-CLOSED AND THIS-IS-NOT-MONTH-END                 
02381 *       IF CL-LAST-MAINT-DT LESS THAN WS-CURRENT-DATE-MINUS-90    
02382 *          GO TO 1300-SIP.                                        
02383                                                                   
02384      IF CLAIM-IS-CLOSED AND THIS-IS-NOT-MONTH-END                 
02385         IF (WS-IBNR-SW = '2' AND                                  
02386            CL-LAST-MAINT-DT LESS THAN WS-SIX-MONTHS-AGO)          
CIDMOD*        OR                                                       
CIDMOD*          (WS-IBNR-SW NOT = '2' AND                              
CIDMOD*          CL-LAST-MAINT-DT LESS THAN WS-CURRENT-DATE-MINUS-90)   
02390              GO TO 1300-SIP.                                      
02391                                                                   
02392  1140-SIP.                                                        
02393      ADD +1                      TO  WS-CLAIM-COUNT.              
02394                                                                   
02395      IF CL-CARRIER NOT = CL-CERT-CARRIER                          
02396          MOVE 'CLAIM CARRIER AND CERT CARRIER ARE NOT THE SAME'   
02397                                  TO  WS-D1-MESSAGE                
02398          MOVE WS-DETAIL1         TO  PRT                          
02399          PERFORM WRITE-A-LINE.                                    
02400                                                                   
121902*    IF CL-SYSTEM-IDENTIFIER = 'CV'                               
121902*        GO TO 1142-GET-POLICY-RECORD.                            
02403  EJECT                                                            
02404  1141-GET-CERT-RECORD.                                            
02405                                                                   
02406      PERFORM 2200-GET-CERTIFICATE                                 
02407                                                                   
02408      IF WS-CERT-NOT-FOUND NOT = ZERO                              
02409          MOVE 'CERTIFICATE NOT FOUND - CREATED'                   
02410                                  TO  WS-D1-MESSAGE                
02411          MOVE WS-DETAIL1         TO  PRT                          
02412          PERFORM WRITE-A-LINE                                     
02413          MOVE SPACES             TO  WS-DETAIL1                   
02414          PERFORM 9100-CREATE-CERTIFICATE.                         
02415                                                                   
121902*    IF (WS-COMPANY-ID = 'AIG' OR 'AUK')                          
121902*        IF CL-CLAIM-TYPE = 'A' OR 'I' OR 'G'
121902*            IF CM-AH-ORIG-TERM IS NOT NUMERIC                    
121902*                MOVE +0             TO  CM-AH-ORIG-TERM.         
02420                                                                   
121902*    IF (WS-COMPANY-ID = 'AIG' OR 'AUK')                          
121902*      IF CL-CLAIM-TYPE = 'A' OR 'I' OR 'G'
121902*        MOVE CM-LOAN-1ST-PMT-DT     TO  DC-BIN-DATE-1            
121902*        MOVE '6'                    TO  DC-OPTION-CODE           
121902*        MOVE +0                     TO  DC-ELAPSED-DAYS          
121902*        COMPUTE DC-ELAPSED-MONTHS = CM-AH-ORIG-TERM - +1         
121902*        PERFORM 8500-DATE-CONVERSION                             
121902*        IF NO-CONVERSION-ERROR                                   
121902*          MOVE DC-BIN-DATE-2        TO  WS-EXPIRE-DT  
121902*        ELSE                                                     
121902*          MOVE LOW-VALUES           TO  WS-EXPIRE-DT.            
02432                                                                   
02433      GO TO 1143-CHECK-NO-ACTIVITY.                                
02434                                                                   
02435  1142-GET-POLICY-RECORD.                                          
02436                                                                   
02437      PERFORM 2210-GET-POLICY-MASTER.                              
02438                                                                   
02439      IF WS-EMPLCY-RETURN-CODE NOT = '00'                          
02440          MOVE 'POLICY RECORD NOT FOUND ' TO  WS-D1-MESSAGE        
02441          MOVE WS-DETAIL1                 TO  PRT                  
02442          PERFORM WRITE-A-LINE                                     
02443          MOVE SPACES                     TO  WS-DETAIL1.          
02444                                                                   
121902*    IF WS-COMPANY-ID = 'CIG' OR 'CUK'                            
121902*        MOVE PM-POLICY-EFF-DT           TO  DC-BIN-DATE-1        
121902*        MOVE '6'                        TO  DC-OPTION-CODE       
121902*        MOVE PM-LOAN-TERM               TO  DC-ELAPSED-MONTHS    
121902*        MOVE +0                         TO  DC-ELAPSED-DAYS      
121902*        PERFORM 8500-DATE-CONVERSION                             
121902*        IF NO-CONVERSION-ERROR                                   
121902*            MOVE DC-BIN-DATE-2          TO  WS-EXPIRE-DT         
121902*        ELSE   
121902*            MOVE LOW-VALUES             TO  WS-EXPIRE-DT.        
02455                                                                   
02456  1143-CHECK-NO-ACTIVITY.                                          
02457                                                                   
121902*    IF WS-COMPANY-ID = 'DMD'                                     
121902*       IF CLAIM-IS-OPEN                                          
121902*       AND CL-LAST-PMT-DT > LOW-VALUES                           
121902*       AND CL-LAST-PMT-DT <  WS-CURRENT-DATE-MINUS-45            
121902*       AND CL-LAST-PMT-DT >= WS-CURRENT-DATE-MINUS-60            
121902*           MOVE 'Y'       TO WS-WRITE-EXTRACT-D-RECORD-C         
121902*       END-IF                                                    
121902*       GO TO 1145-CONTINUE                                       
121902*    END-IF.                                                      
02466                                                                   
061406***IF THE LAST MAINT DT ON THE CLAIM IS MORE THAN 45 DAYS OLD, USE IT
061406***OTHERWISE LOOK THROUGH THE TRAILERS FOR THE HIGHEST LAST MAINT DATE
061406***THAT IS NOT ON NOTE TRAILER



061406*    IF CLAIM-IS-OPEN
061406*        IF CL-LAST-MAINT-DT < WS-CURRENT-DATE-MINUS-45
061406*            MOVE CL-LAST-MAINT-DT TO WS-LAST-MAINT-DT
061406*            MOVE CL-LAST-MAINT-USER TO WS-LAST-MAINT-USER
061406*            MOVE CL-LAST-MAINT-TYPE TO WS-LAST-MAINT-TYPE
061406*        ELSE
061406*            PERFORM 4950-FIND-TRAILER-LAST-MAINT-DT
061406*            IF WS-LAST-MAINT-DT = LOW-VALUE
061406*                MOVE CL-LAST-MAINT-DT TO WS-LAST-MAINT-DT
061406*                MOVE CL-LAST-MAINT-USER TO WS-LAST-MAINT-USER
061406*                MOVE CL-LAST-MAINT-TYPE TO WS-LAST-MAINT-TYPE
061406*            END-IF
061406*        END-IF
061406*    END-IF.

050619     move spaces                 to ws-last-maint-user
050619                                    ws-last-maint-type
050619     move low-values             to ws-last-maint-dt
050619
050619     IF CLAIM-IS-OPEN
050619        IF CL-LAST-MAINT-DT < WS-CURRENT-DATE-MINUS-45
050619           MOVE CL-LAST-MAINT-DT TO WS-LAST-MAINT-DT
050619           MOVE CL-LAST-MAINT-USER
050619                                 TO WS-LAST-MAINT-USER
050619           MOVE CL-LAST-MAINT-TYPE
050619                                 TO WS-LAST-MAINT-TYPE
050619        end-if
050619     end-if

050619     IF CLAIM-IS-OPEN
072419        or ((cl-next-auto-pay-dt > WS-PREV-CYCLE-BIN)
072419        or (cl-next-resend-dt > WS-PREV-CYCLE-BIN)
101019        or (cl-next-followup-dt > WS-PREV-CYCLE-BIN)
101019        or (cl-last-maint-dt > WS-current-date-minus-45))
050619        IF (CL-LAST-MAINT-DT >= WS-CURRENT-DATE-MINUS-45)
050619           or (cl-last-maint-user = 'SYST' or 'AUTO')
050619           PERFORM 4950-FIND-TRAILER-LAST-MAINT-DT
050619           IF WS-LAST-MAINT-DT = LOW-VALUE
050619              MOVE CL-LAST-MAINT-DT
050619                                 TO WS-LAST-MAINT-DT
050619              MOVE CL-LAST-MAINT-USER
050619                                 TO WS-LAST-MAINT-USER
050619              MOVE CL-LAST-MAINT-TYPE
050619                                 TO WS-LAST-MAINT-TYPE
050619           END-IF
050619           if (ws-last-maint-user = 'AUTO' or 'E354')
050619              and (ws-last-maint-type = '1') *> assume auto pmt
050619              move ws-auto-pay-setup-by
050619                                 to ws-last-maint-user
050619           end-if
050619        end-if
050619     end-if

CIDMOD     IF CLAIM-IS-OPEN                                             
061406*CIDMOD        AND CL-LAST-MAINT-DT    < WS-CURRENT-DATE-MINUS-45
061406        AND WS-LAST-MAINT-DT    < WS-CURRENT-DATE-MINUS-45
CIDMOD        AND CL-NEXT-AUTO-PAY-DT < WS-CURRENT-DATE-MINUS-45
CIDMOD        AND CL-NEXT-RESEND-DT   < WS-CURRENT-DATE-MINUS-45
CIDMOD        AND CL-NEXT-FOLLOWUP-DT < WS-CURRENT-DATE-MINUS-45
CIDMOD        AND CL-LAST-PMT-DT      < WS-CURRENT-DATE-MINUS-45
100518        IF CL-CLAIM-TYPE = WS-LIFE-OVERRIDE-L1 OR 'O'
CIDMOD           PERFORM 3500-BUILD-EXTRACT-D-RECORD-C                  
CIDMOD        ELSE                                                      
CIDMOD           IF CL-PAID-THRU-DT < WS-CURRENT-DATE-MINUS-45
CIDMOD              PERFORM 3500-BUILD-EXTRACT-D-RECORD-C               
CIDMOD           END-IF
CIDMOD        END-IF
CIDMOD     END-IF.
CIDMOD
02479  1145-CONTINUE.                                                   
02480                                                                   
02481      MOVE CL-FORCEABLE-ERROR-CNT TO  WS-FORCE-COUNT.              
02482      MOVE ZERO                   TO  CL-FORCEABLE-ERROR-CNT       
02483                                      WS-AT-CHG-EXP                
02484                                      WS-AT-NON-CHG-EXP            
02485                                      WS-CHG-EXP                   
02486                                      WS-NON-CHG-EXP.              
02487                                                                   
121902*    IF CL-SYSTEM-IDENTIFIER NOT = 'CV'                           
02489          PERFORM 2600-GET-BENEFIT.                                 
               PERFORM 2300-GET-ACCOUNT.
121902*    ELSE                                                         
121902*        PERFORM 2350-GET-PRODUCER                                
121902*        IF WS-PRODUCER-NOT-FOUND = ZERO                          
121902*            PERFORM 2215-GET-PRODUCER-PLAN.                      
02495                                                                   
02496      IF CL-BENEFICIARY NOT = SPACES AND LOW-VALUES                
02497          PERFORM 2450-GET-BENEFICIARY THRU 2450-EXIT.             
02498                                                                   
02499 *    NOTE ******************************************************* 
02500 *         *      MAKE SURE THE PREMIUM TYPE IS CORRECT FOR      * 
02501 *         *  OUTSTANDING BALANCE POLICIES                       * 
02502 *         *******************************************************.
02503                                                                   
02504      IF WS-SPECIAL-CALC-CODE = 'O'                                
02505        AND CM-PREMIUM-TYPE NOT = '2'                              
02506          MOVE '2'                TO  CM-PREMIUM-TYPE              
02507                                      CL-CLAIM-PREM-TYPE           
02508          MOVE +1                 TO  WS-REWRITE-CERT              
02509          MOVE 'PREMIUM TYPE WRONG FOR OB'                         
02510                                  TO  WS-D1-MESSAGE                
02511          MOVE WS-DETAIL1         TO  PRT                          
02512          PERFORM WRITE-A-LINE                                     
02513          MOVE SPACES             TO  WS-DETAIL1.                  
02514                                                                   
02515  EJECT                                                            
02516                                                                   
02517 *    NOTE ******************************************************* 
02518 *         *      USE THE BIRTH DATE FROM THE CLAIM.             * 
02519 *         *  IF NOT AVAILABLE, CALCULATE FROM THE CERTIFICATE   * 
02520 *         *  INFORMATION.                                       * 
02521 *         *******************************************************.
02522                                                                   
02523      IF CL-INSURED-BIRTH-DT NOT = LOW-VALUES                      
02524          MOVE CL-INSURED-BIRTH-DT  TO  WS-BIRTH-DATE              
02525          GO TO 1200-SIP.                                          
02526                                                                   
121902*    IF CL-SYSTEM-IDENTIFIER = 'CV'                               
121902*        IF PM-INSURED-ISSUE-AGE IS NOT GREATER THAN ZERO         
121902*            MOVE +40                    TO  WS-ISSUE-AGE         
121902*        ELSE                                                     
121902*            MOVE PM-INSURED-ISSUE-AGE   TO  WS-ISSUE-AGE         
121902*    ELSE                                                         
02533          IF CM-INSURED-ISSUE-AGE NOT GREATER THAN ZERO            
02534              MOVE +40                    TO  WS-ISSUE-AGE         
02535          ELSE                                                     
02536              MOVE CM-INSURED-ISSUE-AGE   TO  WS-ISSUE-AGE.        
02537                                                                   
02538      MULTIPLY WS-ISSUE-AGE BY -12 GIVING DC-ELAPSED-MONTHS.       
02539      MOVE ZERO                   TO  DC-ELAPSED-DAYS.             
02540      MOVE CM-CERT-EFF-DT         TO  DC-BIN-DATE-1.               
02541      MOVE '6'                    TO  DC-OPTION-CODE.              
02542                                                                   
02543      PERFORM 8500-DATE-CONVERSION.                                
02544                                                                   
02545      MOVE DC-BIN-DATE-2          TO  WS-BIRTH-DATE.               
02546                                                                   
02547  EJECT                                                            
02548                                                                   
02549  1200-SIP.                                                        
121902*    IF CL-SYSTEM-IDENTIFIER NOT = 'CV'                           
02551        IF CM-INSURED-ISSUE-AGE = ZERO                             
02552          MOVE CL-INSURED-BIRTH-DT  TO  DC-BIN-DATE-1              
02553          MOVE CM-CERT-EFF-DT     TO  DC-BIN-DATE-2                
02554          MOVE '1'                TO  DC-OPTION-CODE               
02555          PERFORM 8500-DATE-CONVERSION                             
02556          IF DC-ERROR-CODE = SPACES                                
02557              DIVIDE DC-ELAPSED-MONTHS BY +12                      
02558                                  GIVING CM-INSURED-ISSUE-AGE      
02559              MOVE +1             TO  WS-REWRITE-CERT              
02560              MOVE 'ISSUE AGE CORRECTED' TO  WS-D1-MESSAGE         
02561              MOVE WS-DETAIL1     TO  PRT                          
02562              PERFORM WRITE-A-LINE                                 
02563              MOVE SPACES         TO  WS-DETAIL1.                  
02564                                                                   
02565      IF CL-AUTO-PAY-SEQ = ZERO                                    
02566          GO TO 1205-SIP.                                          
02567                                                                   
02568      IF (CLAIM-IS-CLOSED AND CL-AUTO-PAY-SEQ NOT EQUAL ZEROS)     
02569          MOVE ZEROS              TO CL-AUTO-PAY-SEQ               
02570          MOVE LOW-VALUES         TO CL-NEXT-AUTO-PAY-DT           
02571          GO TO 1205-SIP.                                          
02572                                                                   
02573      IF CL-NEXT-AUTO-PAY-DT NOT = LOW-VALUES                      
02574          GO TO 1205-SIP.                                          
02575                                                                   
02576      MOVE CL-CONTROL-PRIMARY     TO  AT-CONTROL-PRIMARY.          
02577      MOVE CL-AUTO-PAY-SEQ        TO  AT-SEQUENCE-NO.              
02578                                                                   
02579      PERFORM 8300-READ-TRAILER.                                   
02580                                                                   
02581      IF AT-TRAILER-TYPE NOT = '3'                                 
02582          MOVE 'AUTO PAY SEQ NOT AUTO PAY TRAILER'                 
02583                                  TO  WS-D1-MESSAGE                
02584          MOVE WS-DETAIL1         TO  PRT                          
02585          PERFORM WRITE-A-LINE                                     
02586          MOVE SPACES             TO  WS-DETAIL1                   
02587          GO TO 1205-SIP.                                          
02588                                                                   
02589      IF BIN-RUN-DATE NOT LESS THAN AT-SCHEDULE-START-DT           
02590        AND BIN-RUN-DATE NOT GREATER THAN AT-SCHEDULE-END-DT       
02591        AND AT-TERMINATED-DT = LOW-VALUES                          
02592          NEXT SENTENCE                                            
02593        ELSE                                                       
02594          GO TO 1205-SIP.                                          
02595                                                                   
02596      MOVE 'NEXT AUTO PAY DATE ZERO'  TO  WS-D1-MESSAGE.           
02597      MOVE WS-DETAIL1             TO  PRT.                         
02598      PERFORM WRITE-A-LINE.                                        
02599      MOVE SPACES                 TO  WS-DETAIL1.                  
02600                                                                   
02601      IF CL-PAID-THRU-DT = LOW-VALUES                              
02602          MOVE AT-SCHEDULE-START-DT  TO  CL-NEXT-AUTO-PAY-DT       
02603          GO TO 1205-SIP.                                          
02604                                                                   
02605      MOVE CL-PAID-THRU-DT        TO  DC-BIN-DATE-1.               
02606      MOVE AT-INTERVAL-MONTHS     TO  DC-ELAPSED-MONTHS.           
02607      MOVE ZERO                   TO  DC-ELAPSED-DAYS.             
02608      MOVE '6'                    TO  DC-OPTION-CODE.              
02609                                                                   
02610      PERFORM 8500-DATE-CONVERSION.                                
02611                                                                   
02612      IF DC-BIN-DATE-2 GREATER THAN AT-SCHEDULE-END-DT             
02613          MOVE AT-SCHEDULE-END-DT TO  CL-NEXT-AUTO-PAY-DT          
02614        ELSE                                                       
02615          MOVE DC-BIN-DATE-2      TO  CL-NEXT-AUTO-PAY-DT.         
02616                                                                   
02617  1205-SIP.                                                        
02618                                                                   
121902*    IF WS-COMPANY-ID = 'DMD'                                     
121902*        MOVE 'N'                TO  WS-DMD-ELSTAT-SW.            
02621                                                                   
02622      IF CL-NEXT-AUTO-PAY-DT NOT EQUAL LOW-VALUES                  
02623          PERFORM 2500-AUTO-PAYMENTS.                              
02624                                                                   
02625      MOVE SPACES                 TO  AT-CONTROL-PRIMARY.          
02626                                                                   
02627      MOVE CL-COMPANY-CD          TO  AT-COMPANY-CD.               
02628      MOVE CL-CARRIER             TO  AT-CARRIER.                  
02629      MOVE CL-CLAIM-NO            TO  AT-CLAIM-NO.                 
02630      MOVE CL-CERT-NO             TO  AT-CERT-NO.                  
02631      MOVE ZERO                   TO  AT-SEQUENCE-NO.              
02632                                                                   
02633      START ELTRLR                                                 
02634          KEY EQUAL AT-CONTROL-PRIMARY.                            
02635                                                                   
02636      IF ELTRLR-FILE-STATUS = '23'                                 
02637          MOVE 'HAS NO ZERO TRAILER - TRAILER CREATED'             
02638                                  TO  WS-D1-MESSAGE                
02639          MOVE WS-DETAIL1         TO  PRT                          
02640          PERFORM WRITE-A-LINE                                     
02641          PERFORM 9200-BUILD-ZERO-TRAILER                          
02642          GO TO 1211-SIP.                                          
02643                                                                   
02644      IF ELTRLR-FILE-STATUS NOT = ZERO                             
02645          MOVE 'ERROR OCCURRED START ZERO TRAILER - ELTRLR'        
02646                                  TO  WS-ABEND-MESSAGE             
02647          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
02648          GO TO ABEND-PGM.                                         
02649                                                                   
02650  EJECT                                                            
02651                                                                   
02652  1210-SIP.                                                        
02653                                                                   
02654      READ ELTRLR NEXT INTO WS-SAVE-ACTIVITY-TRAILERS.             
02655                                                                   
02656      IF ELTRLR-FILE-STATUS = '10'                                 
02657          GO TO 1300-SIP.                                          
02658                                                                   
02659      IF ELTRLR-FILE-STATUS NOT = ZERO                             
02660          MOVE 'ERROR OCCURRED READNEXT TRAILER RECORD - ELTRLR'   
02661                                  TO  WS-ABEND-MESSAGE             
02662          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
02663          GO TO ABEND-PGM.                                         
02664                                                                   
02665      IF CL-COMPANY-CD NOT = AT-COMPANY-CD                         
02666        OR CL-CARRIER  NOT = AT-CARRIER                            
02667        OR CL-CLAIM-NO NOT = AT-CLAIM-NO                           
02668        OR CL-CERT-NO  NOT = AT-CERT-NO                            
02669          GO TO 1300-SIP.                                          
02670                                                                   
02671  EJECT                                                            
02672                                                                   
02673  1211-SIP.                                                        
02674                                                                   
02675      ADD +1  TO  WS-TRAILER-COUNT.                                
02676                                                                   
02677 *    NOTE ******************************************************* 
02678 *         *                                                     * 
02679 *         *        PROCESS THE RESERVE / EXPENSE TRAILER        * 
02680 *         *                                                     * 
02681 *         *******************************************************.
02682                                                                   
02683      IF AT-TRAILER-TYPE NOT = '1'                                 
02684          GO TO 1215-SIP.                                          
02685                                                                   
02686      MOVE AT-ITD-PAID-EXPENSES       TO  WS-AT-NON-CHG-EXP.       
02687      MOVE AT-ITD-CHARGEABLE-EXPENSE  TO  WS-AT-CHG-EXP.           
02688                                                                   
02689      IF CLAIM-IS-CLOSED                                           
02690         IF (WS-IBNR-SW = '2') AND                                 
02691            (CL-LAST-MAINT-DT NOT LESS THAN WS-SIX-MONTHS-AGO)     
02692              NEXT SENTENCE                                        
02693           ELSE                                                    
02694              GO TO 1213-SIP.                                      
02695                                                                   
02696  1212-SIP.                                                        
02697                                                                   
121902*    IF WS-COMPANY-ID NOT = 'DMD'                                 
02699          PERFORM 3000-BUILD-EXTRACT-A-RECORD-A.                   
02700                                                                   
020816     IF WS-COMPANY-ID IS EQUAL TO 'CID' OR 'DCC' or 'VPP'
CIDMOD         PERFORM 4300-BUILD-EXTRACT-F-RECORD-C                    
CIDMOD     END-IF
CIDMOD                                                                  
02701      IF ELTRLR-FILE-STATUS NOT = '23'                             
02702          PERFORM 8100-REWRITE-TRAILER                             
02703          GO TO 1210-SIP.                                          
02704                                                                   
02705  1213-SIP.                                                        
02706                                                                   
02707      IF ELTRLR-FILE-STATUS NOT = '23'                             
02708         GO TO 1210-SIP.                                           
02709                                                                   
02710      PERFORM 8700-WRITE-TRAILER.                                  
02711                                                                   
02712      MOVE CL-COMPANY-CD          TO  AT-COMPANY-CD.               
02713      MOVE CL-CARRIER             TO  AT-CARRIER.                  
02714      MOVE CL-CLAIM-NO            TO  AT-CLAIM-NO.                 
02715      MOVE CL-CERT-NO             TO  AT-CERT-NO.                  
02716      MOVE ZERO                   TO  AT-SEQUENCE-NO.              
02717                                                                   
02718      START ELTRLR                                                 
02719          KEY IS GREATER THAN AT-CONTROL-PRIMARY.                  
02720                                                                   
02721      IF ELTRLR-FILE-STATUS = '23' OR '10'                         
02722          GO TO 1300-SIP.                                          
02723                                                                   
02724      IF ELTRLR-FILE-STATUS NOT = ZERO                             
02725          MOVE 'ERROR OCCURRED START GREATER THAN ZERO TRAILER - EL
02726 -             'TRLR'              TO  WS-ABEND-MESSAGE            
02727          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
02728          GO TO ABEND-PGM.                                         
02729                                                                   
02730      GO TO 1210-SIP.                                              
02731                                                                   
02732  EJECT                                                            
02733                                                                   
02734  1215-SIP.                                                        
02735 *    NOTE ******************************************************* 
02736 *         *                                                     * 
02737 *         *            PROCESS THE PAYMENT TRAILER              * 
02738 *         *                                                     * 
02739 *         *******************************************************.
02740                                                                   
02741      IF AT-TRAILER-TYPE = '2'                                     
02742          NEXT SENTENCE                                            
02743        ELSE                                                       
02744          GO TO 1220-SIP.                                          
02745                                                                   
121902*    IF WS-COMPANY-ID = 'DMD'                                     
121902*        IF AT-RECORDED-DT = CL-LAST-PMT-DT  AND                  
121902*           WS-WRITE-EXTRACT-D-RECORD-C = 'Y'                     
121902*              MOVE AT-RECORDED-BY TO WS-DMD-LAST-PMT-RECORDED-BY 
121902*              PERFORM 3500-BUILD-EXTRACT-D-RECORD-C.             
02751                                                                   
022718     IF AT-VOID-DT = LOW-VALUES
022718       AND NOT OFFLINE-PMT
022718       AND AT-TO-BE-WRITTEN-DT NOT GREATER THAN BIN-RUN-DATE
100919       AND (AT-CHECK-WRITTEN-DT > WS-PREV-CYCLE-BIN
100919       AND AT-CHECK-WRITTEN-DT <= BIN-RUN-DATE)
022718        IF AT-TO-BE-WRITTEN-DT > LOW-VALUES
022718           IF AT-PAID-THRU-DT > CL-PAID-THRU-DT
022718              MOVE AT-PAID-THRU-DT TO CL-PAID-THRU-DT
040819           END-IF
022718           ADD AT-AMOUNT-PAID TO CL-TOTAL-PAID-AMT
022718           ADD 1              TO CL-NO-OF-PMTS-MADE
022718        END-IF
022718     END-IF

02752      IF PAYMENT-NOT-QUEUED
02753        AND AT-VOID-DT = LOW-VALUES                                
02754        AND NOT OFFLINE-PMT                                        
02755        AND AT-CHECK-WRITTEN-DT = LOW-VALUES                       
02756        AND AT-TO-BE-WRITTEN-DT NOT GREATER THAN BIN-RUN-DATE
02757          PERFORM 2800-VERIFY-ACTIVITY-QUEUE                       
02758          IF (NOT WS-PMT-APPROVAL-USED) OR                         
02759             (WS-PMT-APPROVAL-USED AND                             
02760             AT-PAYMENT-APPROVAL-SW = 'A')                         
02761               PERFORM 3700-BUILD-EXTRACT-D-RECORD-E               
02762           ELSE                                                    
02763              IF WS-PMT-APPROVAL-USED AND                          
02764                 AT-PAYMENT-APPROVAL-SW = 'U'                      
02765                    PERFORM 3950-BUILD-EXTRACT-D-RECORD-H.         
02766                                                                   
121902*    IF WS-COMPANY-ID = 'AIG' OR 'AUK'                            
121902*       IF AT-PAYMENT-TYPE = '2'                                  
121902*          IF WS-AIG-LAST-THRU-EXP-DT = LOW-VALUES                
121902*             MOVE AT-CHECK-WRITTEN-DT                            
121902*                                TO  WS-AIG-LAST-THRU-EXP-DT.     
02772                                                                   
121902*    IF WS-COMPANY-ID = 'AIG' OR 'AUK'                            
121902*       IF AT-PAYMENT-TYPE = '1'                                  
121902*          IF WS-AIG-LAST-PART-DT = LOW-VALUES                    
121902*             MOVE AT-CHECK-WRITTEN-DT                            
121902*                                TO WS-AIG-LAST-PART-DT.          
02778                                                                   
121902*    IF WS-COMPANY-ID = 'AIG' OR 'AUK'                            
121902*       IF WS-AIG-LAST-PMT = 'Y'                                  
121902*          IF AT-PAYMENT-TYPE = '1'                               
121902*             MOVE 'Y'           TO WS-AIG-LAST-PARTIAL           
121902*             MOVE AT-CHECK-WRITTEN-DT                            
121902*                                TO WS-AIG-LAST-PMT-DT.           
02785                                                                   
121902*    IF (WS-COMPANY-ID = 'AIG' OR 'AUK')                          
121902*        IF AT-RECORDED-BY = 'L334'                               
121902*            NEXT SENTENCE                                        
121902*        ELSE                                                     
121902*            IF (AT-ASSOCIATES = 'A' OR 'I' OR 'N' OR 'M')        
121902*                NEXT SENTENCE                                    
121902*            ELSE                                                 
121902*                MOVE CL-ASSOCIATES  TO  AT-ASSOCIATES            
121902*                MOVE +1             TO  WS-UPDATE-TRAILER        
121902*                MOVE 'ASSOCIATES INDICATOR FIXED'                
121902*                                    TO  WS-D1-MESSAGE            
121902*                MOVE WS-DETAIL1     TO  PRT                      
121902*                PERFORM WRITE-A-LINE                             
121902*                MOVE SPACES         TO  WS-DETAIL1.              
02800                                                                   
02801      MOVE 'N'                    TO WS-AIG-LAST-PMT.              
02802                                                                   
02803      IF PAYMENT-WAS-FORCED                                        
02804        AND AT-CHECK-WRITTEN-DT GREATER WS-CURRENT-DATE-MINUS-7    
02805          ADD +1  TO  WS-FORCE-COUNT.                              
02806                                                                   
02807      IF OFFLINE-PMT                                               
02808         IF AT-CHECK-WRITTEN-DT = LOW-VALUES                       
02809            IF AT-RECORDED-DT = LOW-VALUES                         
02810                  NEXT SENTENCE                                    
02811            ELSE                                                   
022106             IF LIFE-INTEREST
022106                CONTINUE
022106             ELSE                                                 
02815                MOVE +1                 TO  WS-UPDATE-TRAILER      
02816                MOVE AT-RECORDED-DT     TO AT-CHECK-WRITTEN-DT     
02817                MOVE                                               
02818               'OFFLINE PAYMENT DATE WRITTEN NOT INPUT - CORRECTED'
02819                                       TO  WS-D1-MESSAGE           
02820                MOVE WS-DETAIL1         TO  PRT                    
02821                PERFORM WRITE-A-LINE                               
02822                MOVE SPACES             TO  WS-DETAIL1
                   END-IF
                 END-IF
              END-IF
           END-IF
02823                                                                   
02824      IF AT-PAYMENT-TYPE = '5'                                     
02825          ADD AT-AMOUNT-PAID      TO  WS-CHG-EXP.                  
02826                                                                   
02827      IF AT-PAYMENT-TYPE = '6'                                     
02828          ADD AT-AMOUNT-PAID      TO  WS-NON-CHG-EXP.              
02829                                                                   
02830      IF (AT-PMT-ACCEPT-DT = LOW-VALUES                            
02831         OR                                                        
02832          (AT-VOID-SELECT-DT NOT = LOW-VALUES AND                  
02833           AT-VOID-ACCEPT-DT = LOW-VALUES))                        
02834        AND                                                        
02835          (AT-CHECK-WRITTEN-DT NOT = LOW-VALUES AND                
02836           AT-CHECK-WRITTEN-DT NOT GREATER THAN BIN-RUN-DATE       
02837                   AND                                             
02838           AT-CHECK-WRITTEN-DT NOT LESS THAN WS-010184)            
121902*       OR                                                        
121902*          WS-COMPANY-ID = 'TAO')                                 
121902*            NEXT SENTENCE                                        
121902             CONTINUE
02842            ELSE                                                   
02843               PERFORM 2900-OLD-PAYMENTS THRU 2900-EXIT            
02844               GO TO 1217-SIP.                                     
02845                                                                   
02846      MOVE +0             TO  WS-VOID-PAYMENT-SW.                  
02847                                                                   
022106     IF AT-PAYMENT-TYPE NOT = 'I'
02848         PERFORM 3100-BUILD-EXTRACT-A-RECORD-B
022106     END-IF

           .                                                            
02850  1217-SIP.                                                        
02851                                                                   
02852      MOVE +0             TO  WS-VOID-PAYMENT-SW.                  
02853                                                                   
02854      IF ((AT-PMT-SELECT-DT = WS-MONTH-END-DATE) OR                
02855         (AT-VOID-SELECT-DT = WS-MONTH-END-DATE))                  
02856                        AND                                        
02857         ((AT-CHECK-WRITTEN-DT NOT = LOW-VALUES AND
02858          AT-CHECK-WRITTEN-DT NOT GREATER THAN BIN-RUN-DATE)
                          OR AT-PAYMENT-TYPE = 'I')
02859            PERFORM 3200-BUILD-EXTRACT-B-RECORD-A                  
02860            IF (OFFLINE-PMT)
022106              AND (AT-PAYMENT-TYPE NOT = 'I')
02861               MOVE +0             TO  WS-ELCHKQ-SW               
02862               PERFORM 3300-BUILD-EXTRACT-C-RECORD-A
                 END-IF
           END-IF
02863                                                                   
020816     IF (WS-COMPANY-ID IS EQUAL TO 'CID' OR 'DCC' or 'VPP')
022106        AND (AT-PAYMENT-TYPE NOT = 'I')
CIDMOD        PERFORM 4300-BUILD-EXTRACT-F-RECORD-C                     
CIDMOD     END-IF
CIDMOD                                                                  
020816     IF (WS-COMPANY-ID IS EQUAL TO 'CID' OR 'DCC' or 'VPP')
DAN01         AND (AT-PAYMENT-ORIGIN = '2')
DAN01         AND (AT-CHECK-WRITTEN-DT = BIN-RUN-DATE)
DAN01            PERFORM 3395-BUILD-EXTRACT-D-RECORD-A                  
DAN01      END-IF
DAN01                                                                   
02864      IF AT-CHECK-QUE-CONTROL = ZERO OR +99999999                  
02865          GO TO 1219-SIP.                                          
02866                                                                   
02867 * CHECK DMD IF A DMOFILE PAYMENT                                  
121902*    IF WS-COMPANY-ID = 'DMD'                                     
121902*       IF AT-CHECK-QUE-CONTROL  = +88888888  AND                 
121902*          AT-CHECK-QUE-SEQUENCE = +8888                          
121902*            GO TO 1219-SIP.                                      
02872                                                                   
02873      PERFORM 2400-GET-CHECK-QUE-RECORD.                           
02874                                                                   
02875      IF ELCHKQ-FILE-STATUS = '23'                                 
121902*        IF WS-COMPANY-ID = 'LGX'                                 
121902*            NEXT SENTENCE                                        
121902*        ELSE                                                     
02879              MOVE 'CHECK QUE NOT FOUND FOR PMT SEQ=9999 QUE=999999
02880 -             '99 SEQ=9999'              TO  WS-D1-MESSAGE        
02881              MOVE AT-SEQUENCE-NO         TO  WS-D1-SEQ            
02882              MOVE AT-CHECK-QUE-CONTROL   TO  WS-D1-QUE            
02883              MOVE AT-CHECK-QUE-SEQUENCE  TO WS-D1-QUE-SEQ         
02884              MOVE WS-DETAIL1             TO  PRT                  
02885              PERFORM WRITE-A-LINE                                 
02886              MOVE SPACES                 TO  WS-DETAIL1           
02887              GO TO 1219-SIP.                                      
02888                                                                   
02889      IF AT-CARRIER  = CQ-CARRIER  AND                             
02890         AT-CLAIM-NO = CQ-CLAIM-NO AND                             
02891         AT-CERT-NO  = CQ-CERT-NO                                  
02892          GO TO 1219-SIP.                                          
02893                                                                   
121902*    IF WS-COMPANY-ID = 'LGX'                                     
121902*        GO TO 1219-SIP.                                          
02896                                                                   
02897      MOVE 'CHECK QUE INCORRECT FOR PMT SEQ=9999 QUE=99999999 SEQ=9
02898 -         '999'                  TO  WS-D1-MESSAGE.               
02899      MOVE AT-SEQUENCE-NO         TO  WS-D1-SEQ.                   
02900      MOVE AT-CHECK-QUE-CONTROL   TO  WS-D1-QUE.                   
02901      MOVE AT-CHECK-QUE-SEQUENCE  TO  WS-D1-QUE-SEQ.               
02902      MOVE WS-DETAIL1             TO  PRT.                         
02903      PERFORM WRITE-A-LINE.                                        
02904      MOVE SPACES                 TO  WS-DETAIL1.                  
02905                                                                   
02906      MOVE 'CARR='                TO  WS-D1-CARR-DESC.             
02907      MOVE CQ-CARRIER             TO  WS-D1-CARR.                  
02908                                                                   
02909      MOVE 'CLAIM NO='            TO  WS-D1-CLAIM-DESC.            
02910      MOVE CQ-CLAIM-NO            TO  WS-D1-CLAIM.                 
02911                                                                   
02912      MOVE 'CERT-NO='             TO  WS-D1-CERT-DESC.             
02913      MOVE CQ-CERT-NO             TO  WS-D1-CERT.                  
02914                                                                   
02915      MOVE WS-DETAIL1             TO  PRT.                         
02916      PERFORM WRITE-A-LINE.                                        
02917      MOVE SPACES                 TO  WS-DETAIL1.                  
02918                                                                   
02919  1219-SIP.                                                        
02920                                                                   
02921 *         ******************************************************* 
02922 *         *   THE FOLLOWING CODE HAS BEEN ADDED TO CAUSE AIMS   * 
02923 *         * INTERFACE DATA TO BE CREATED.                       * 
02924 *         ******************************************************* 
02925                                                                   
121902*    IF WS-COMPANY-ID = 'AIG' OR 'AUK'                            
121902*        PERFORM 9400-AIMS-PAYMENT-INTERFACE THRU 9499-EXIT.      
02928                                                                   
02929      IF WS-UPDATE-TRAILER NOT = ZERO                              
02930          PERFORM 8100-REWRITE-TRAILER                             
02931          MOVE ZERO           TO  WS-UPDATE-TRAILER.               
02932                                                                   
121902*    IF WS-COMPANY-ID NOT = 'DMD'                                 
02934          GO TO 1210-SIP.                                          
02935                                                                   
02936 *  *******************************************************        
02937 *  *   THE FOLLOWING CODE HAS BEEN ADDED TO CREATE A     *        
02938 *  *   A STATISTICAL EXTRACT FOR DMD                     *        
02939 *  *******************************************************        
02940                                                                   
02941      IF AT-PAYMENT-TYPE = 'T'                                     
02942          GO TO 1210-SIP.                                          
02943                                                                   
02944      MOVE SPACES                 TO WS-DMD-MAINTENANCE-TYPE.      
02945                                                                   
02946 *   PARTIAL PAYMENT                                               
02947      IF AT-PAYMENT-TYPE = '1' OR '9'                              
02948          MOVE '02'            TO WS-DMD-MAINTENANCE-TYPE          
02949         ELSE                                                      
02950 *   FINAL PAYMENT                                                 
02951      IF AT-PAYMENT-TYPE = '2' OR '3'                              
02952          MOVE '01'            TO WS-DMD-MAINTENANCE-TYPE          
02953         ELSE                                                      
02954 *   ADDITIONAL PAYMENTS                                           
02955      IF AT-PAYMENT-TYPE = '4'                                     
02956          MOVE '03'            TO WS-DMD-MAINTENANCE-TYPE          
02957         ELSE                                                      
02958 *   OTHER PAYMENTS                                                
02959      IF AT-PAYMENT-TYPE = '5' OR '6'                              
02960          MOVE '04'            TO WS-DMD-MAINTENANCE-TYPE.         
02961                                                                   
02962 ********* CREATE 2 RECORDS FOR THOSE PAYMENTS WRITTEN AND *****   
02963 ********* VOIDED ON THE SAME DAY.                         *****   
02964      IF AT-VOID-DT NOT = LOW-VALUES AND SPACES                    
02965          IF AT-VOID-DT = AT-RECORDED-DT                           
02966              MOVE AT-RECORDED-DT     TO WS-DMD-MAINTENANCE-DT     
02967              MOVE AT-RECORDED-BY     TO WS-DMD-PROCESSOR-ID       
02968              IF (WS-DMD-MAINTENANCE-DT > WS-LAST-PROCESS-DT AND   
02969                  WS-DMD-MAINTENANCE-TYPE NOT = SPACES)            
02970                  PERFORM 9500-DMD-ACTIVITY-FILE THRU 9500-EXIT    
02971                  MOVE '05'        TO WS-DMD-MAINTENANCE-TYPE      
02972              END-IF                                               
02973          ELSE                                                     
02974              MOVE '05'            TO WS-DMD-MAINTENANCE-TYPE.     
02975                                                                   
02976      IF WS-DMD-MAINTENANCE-TYPE = '05'                            
02977          MOVE AT-VOID-DT         TO WS-DMD-MAINTENANCE-DT         
02978          MOVE AT-PAYMENT-LAST-UPDATED-BY                          
02979                                  TO WS-DMD-PROCESSOR-ID           
02980      ELSE                                                         
02981          MOVE AT-RECORDED-DT     TO WS-DMD-MAINTENANCE-DT         
02982          MOVE AT-RECORDED-BY     TO WS-DMD-PROCESSOR-ID.          
02983                                                                   
02984      IF (WS-DMD-MAINTENANCE-DT GREATER WS-LAST-PROCESS-DT  AND    
02985          WS-DMD-MAINTENANCE-TYPE NOT EQUAL SPACES)                
02986             PERFORM 9500-DMD-ACTIVITY-FILE THRU 9500-EXIT.        
02987                                                                   
02988      GO TO 1210-SIP.                                              
02989      EJECT                                                        
02990  1220-SIP.                                                        
02991 *    NOTE ******************************************************* 
02992 *         *                                                     * 
02993 *         *            PROCESS THE AUTO PAY TRAILER             * 
02994 *         *                                                     * 
02995 *         *******************************************************.
02996                                                                   
02997      IF AT-TRAILER-TYPE NOT = '3'                                 
02998          GO TO 1230-SIP.                                          
02999                                                                   
CIDMOD     IF WS-COMPANY-ID IS EQUAL TO 'CID' OR 'DMD'
020816                      OR 'DCC' or 'VPP'
CIDMOD         PERFORM 4300-BUILD-EXTRACT-F-RECORD-C                    
CIDMOD     END-IF
02376                                                                   
03000 ***************************************************************   
03001 ***  FOR 'DMD' ONLY  - PROJECT 6449                         ***   
03002 *** CREATES A STATISTIC RECORD FOR THE SET UP OF THIS AUTO  ***   
03003 *** PAYMENT FOR THIS PROCESSOR ID.                          ***   
03004 ***************************************************************   
121902*    IF WS-COMPANY-ID = 'DMD'                                     
121902*       IF AT-RECORDED-DT > WS-LAST-PROCESS-DT                    
121902*           MOVE '11'            TO  WS-DMD-MAINTENANCE-TYPE      
121902*           MOVE AT-RECORDED-DT  TO  WS-DMD-MAINTENANCE-DT        
121902*           MOVE AT-RECORDED-BY  TO  WS-DMD-PROCESSOR-ID          
121902*           PERFORM 9500-DMD-ACTIVITY-FILE THRU 9500-EXIT.        
03011                                                                   
03012 ***************************************************************** 
03013 ** CREATE AN ELSTAT RECORD FOR EACH AUTO-PAY TERMINATION          
03014 ** PROCESSED ON-LINE (MANUALLY).  WS-DMD-ELSTAT-SW IS SET TO      
03015 ** "Y" WHEN AN AUTOMATIC AUTO-PAY TERMINATION (TYPE: 12) RECORD   
03016 ** IS CREATED IN SECTION 2500-AUTO-PAYMENTS.  IF A RECORD WAS     
03017 ** CREATED THERE, THEN DO NOT CREATE ONE HERE.                    
03018 ***************************************************************** 
121902*    IF WS-COMPANY-ID = 'DMD'                                     
121902*      IF WS-DMD-ELSTAT-SW = 'N'                                  
121902*        IF AT-TERMINATED-DT NOT = LOW-VALUES                     
121902*          IF AT-TERMINATED-DT > WS-LAST-PROCESS-DT               
121902*              MOVE '16'            TO  WS-DMD-MAINTENANCE-TYPE   
121902*              MOVE AT-AUTO-PAY-LAST-MAINT-DT                     
121902*                                   TO  WS-DMD-MAINTENANCE-DT     
121902*              MOVE AT-AUTO-PAY-LAST-UPDATED-BY                   
121902*                                   TO  WS-DMD-PROCESSOR-ID       
121902*              PERFORM 9500-DMD-ACTIVITY-FILE THRU 9500-EXIT.     
03029                                                                   
03030      GO TO 1210-SIP.                                              
03031                                  EJECT                            
03032  1230-SIP.                                                        
03033 *    NOTE ******************************************************* 
03034 *         *                                                     * 
03035 *         *          PROCESS THE CORRESPONDENCE TRAILER         * 
03036 *         *                                                     * 
03037 *         *******************************************************.
03038                                                                   
03039      IF AT-TRAILER-TYPE NOT = '4'                                 
03040          GO TO 1240-SIP.                                          
03041                                                                   
092909     IF CLAIM-IS-OPEN AND AT-LETTER-SENT-DT < BIN-RUN-DATE
03042      IF (AT-AUTO-RE-SEND-DT NOT LESS THAN WS-CURRENT-DATE-MINUS-2 
03043        AND                                                        
03044         AT-AUTO-RE-SEND-DT NOT GREATER THAN WS-CURRENT-DATE-PLUS-2
03045        AND                                                        
03046         AT-LETTER-ANSWERED-DT = LOW-VALUES                        
102810       AND
102810        (AT-STOP-LETTER-DT = LOW-VALUES OR SPACES) 
03047        AND                                                        
03048         AT-RESEND-PRINT-DATE = LOW-VALUES)                        
03049        OR                                                         
03050         (AT-INITIAL-PRINT-DATE = LOW-VALUES AND                   
03051          AT-LETTER-ANSWERED-DT = LOW-VALUES AND                   
102810        (AT-STOP-LETTER-DT = LOW-VALUES OR SPACES) AND
03052          AT-LETTER-ARCHIVE-NO GREATER THAN +0)                    
03053             PERFORM 3400-BUILD-EXTRACT-D-RECORD-B.                
03054                                                                   
03055 *    IF AT-RECEIPT-FOLLOW-UP NOT GREATER THAN BIN-RUN-DATE        
03056 *      AND AT-RECEIPT-FOLLOW-UP NOT LESS WS-CURRENT-DATE-MINUS-2  
03057 *      AND AT-LETTER-ANSWERED-DT = LOW-VALUES                     
03058 *        PERFORM 3900-BUILD-EXTRACT-D-RECORD-G.                   
03059                                                                   
03055      IF (AT-RECEIPT-FOLLOW-UP <= BIN-RUN-DATE)
092909*03056         AND (AT-RECEIPT-FOLLOW-UP >= WS-CURRENT-DATE-MINUS-2)
092909        AND (AT-RECEIPT-FOLLOW-UP > WS-PREV-CYCLE-BIN)
03057         AND (AT-LETTER-ANSWERED-DT = LOW-VALUES)
121608        AND (AT-STD-LETTER-FORM (1:1) NOT = 'X')
102810        AND (AT-STOP-LETTER-DT = LOW-VALUES OR SPACES)
080510           IF AT-AUTO-CLOSE-IND = 'C'
080510             IF CLAIM-IS-OPEN
080510                MOVE 'Y' TO WS-DROP-CLAIM-SW
012511                MOVE AT-CONTROL-PRIMARY TO 
012511                              WS-SAVE-TRAILER-CONTROL
080510                PERFORM 2000-CLOSE-CLAIM
080510                MOVE 'N' TO WS-DROP-CLAIM-SW
012511                MOVE WS-SAVE-TRAILER-CONTROL TO 
012511                              AT-CONTROL-PRIMARY
012511                PERFORM 8300-READ-TRAILER
080510             END-IF
012511             GO TO 1210-SIP
080510           ELSE
080510              IF AT-AUTO-CLOSE-IND = 'B'
080510                IF CLAIM-IS-OPEN
080510                   MOVE 'Y' TO WS-DROP-CLAIM-SW
012511                   MOVE AT-CONTROL-PRIMARY TO 
012511                              WS-SAVE-TRAILER-CONTROL
080510                   PERFORM 3900-BUILD-EXTRACT-D-RECORD-G
080510                   PERFORM 2000-CLOSE-CLAIM
080510                   MOVE 'N' TO WS-DROP-CLAIM-SW
012511                   MOVE WS-SAVE-TRAILER-CONTROL TO 
012511                              AT-CONTROL-PRIMARY
012511                   PERFORM 8300-READ-TRAILER
080510                END-IF
012511                GO TO 1210-SIP
080510              ELSE
080510                  PERFORM 3900-BUILD-EXTRACT-D-RECORD-G
080510              END-IF
080510           END-IF
           END-IF
03059                                                                   
03060 ***************************************************************   
03061 ***            'DMD' ONLY  - PROJECT 6449                   ***   
03062 ***   CREATES A STATISTIC RECORD FOR LETTERS SENT           ***   
03063 ***************************************************************   
121902*    IF WS-COMPANY-ID = 'DMD'                                     
121902*        IF AT-LETTER-SENT-DT > WS-LAST-PROCESS-DT                
121902*            MOVE '09'               TO WS-DMD-MAINTENANCE-TYPE   
121902*            MOVE AT-LETTER-SENT-DT  TO WS-DMD-MAINTENANCE-DT     
121902*            MOVE AT-RECORDED-BY     TO WS-DMD-PROCESSOR-ID       
121902*            PERFORM 9500-DMD-ACTIVITY-FILE THRU 9500-EXIT        
121902*        ELSE                                                     
03071 ***************************************************************   
03072 ***   CREATES A STATISTIC RECORD FOR LETTERS ANSWERED       ***   
03073 ***************************************************************   
121902*           IF AT-CORR-SOL-UNSOL = 'U' AND                        
121902*              AT-RECORDED-DT > WS-LAST-PROCESS-DT                
121902*                MOVE '10'             TO WS-DMD-MAINTENANCE-TYPE 
121902*                MOVE AT-RECORDED-DT   TO WS-DMD-MAINTENANCE-DT   
121902*                MOVE AT-RECORDED-BY   TO WS-DMD-PROCESSOR-ID     
121902*                PERFORM 9500-DMD-ACTIVITY-FILE THRU 9500-EXIT.   
03080                                                                   
121902*    IF WS-COMPANY-ID = 'CSL'                                     
121902*        IF (AT-LETTER-ANSWERED-DT GREATER WS-PROMPT-PURGE-DATE   
121902*                               AND                               
121902*            AT-LETTER-ANSWERED-DT NOT GREATER WS-ACTUAL-DATE)    
121902*               MOVE AT-LETTER-ANSWERED-DT  TO  WS-ACTION-DATE    
121902*               PERFORM 3960-BUILD-EXTRACT-D-RECORD-I.            
03087                                                                   
121902*    IF WS-COMPANY-ID NOT = 'CSL'                                 
03089          IF AT-LETTER-ANSWERED-DT = WS-ACTUAL-DATE                
03090             MOVE AT-LETTER-ANSWERED-DT  TO  WS-ACTION-DATE        
03091             PERFORM 3960-BUILD-EXTRACT-D-RECORD-I.                
03092                                                                   
           IF AT-STD-LETTER-FORM (1:1) = 'X'
              IF (AT-RECEIPT-FOLLOW-UP <= BIN-RUN-DATE)
                 AND (AT-LETTER-ANSWERED-DT = LOW-VALUES)
102810           AND (AT-STOP-LETTER-DT = LOW-VALUES OR SPACES)
                 MOVE 'R'              TO WS-LETTER-ORIGIN
                 MOVE AT-RECEIPT-FOLLOW-UP
                                       TO WS-ACTION-DATE
                 PERFORM 4000-BUILD-EXTRACT-E-RECORD-A
                 MOVE SPACES           TO WS-LETTER-ORIGIN

                 IF CL-DENIAL-TYPE NOT = '4'
                    MOVE '4'           TO CL-DENIAL-TYPE
                 END-IF
              END-IF
           ELSE
              EVALUATE TRUE
                 WHEN (AT-LETTER-SENT-DT >= WS-PRIOR-MONTH)
                    AND (AT-LETTER-SENT-DT <= BIN-RUN-DATE)
                    MOVE AT-LETTER-SENT-DT
                                       TO WS-ACTION-DATE
                    PERFORM 4000-BUILD-EXTRACT-E-RECORD-A
                 WHEN (AT-LETTER-ANSWERED-DT >= WS-PRIOR-MONTH)
                    AND (AT-LETTER-ANSWERED-DT <= BIN-RUN-DATE)
                    MOVE AT-LETTER-ANSWERED-DT
                                       TO WS-ACTION-DATE
                    PERFORM 4000-BUILD-EXTRACT-E-RECORD-A
                 WHEN (AT-AUTO-RE-SEND-DT >= WS-PRIOR-MONTH)
                    AND (AT-AUTO-RE-SEND-DT <= BIN-RUN-DATE)
                    AND (AT-LETTER-ANSWERED-DT = LOW-VALUES)
102810              AND (AT-STOP-LETTER-DT = LOW-VALUES OR SPACES)
                    MOVE AT-AUTO-RE-SEND-DT
                                       TO WS-ACTION-DATE
                    PERFORM 4000-BUILD-EXTRACT-E-RECORD-A
                 WHEN (AT-RESEND-PRINT-DATE >= WS-PRIOR-MONTH)
                    AND (AT-RESEND-PRINT-DATE <= BIN-RUN-DATE)
                    MOVE AT-RESEND-PRINT-DATE
                                       TO WS-ACTION-DATE
                    PERFORM 4000-BUILD-EXTRACT-E-RECORD-A                
                 WHEN (AT-RECEIPT-FOLLOW-UP >= WS-PRIOR-MONTH)
                    AND (AT-RECEIPT-FOLLOW-UP <= BIN-RUN-DATE)
                    AND (AT-LETTER-ANSWERED-DT = LOW-VALUES)
102810              AND (AT-STOP-LETTER-DT = LOW-VALUES OR SPACES)
                    MOVE AT-RECEIPT-FOLLOW-UP
                                       TO WS-ACTION-DATE
                    PERFORM 4000-BUILD-EXTRACT-E-RECORD-A
              END-EVALUATE
           END-IF

03120 ***************************************************************   
03121 ***            'DMD' ONLY  - PROJECT 6637                   ***   
03122 ***   CREATES AN EX-EA RECORD WHEN A CORRESPONDENCE RECORD  ***   
03123 ***   HAS NO PRINT DATE AND THE VALUE IN BSR-CODE IS AN 'A' ***   
03124 ***************************************************************   
CIDMOD*    IF WS-COMPANY-ID = 'DMD'                                     
CIDMOD*       IF (AT-LETTER-SENT-DT = LOW-VALUES                        
CIDMOD*         AND AT-AUTOMATED-BSR)                                   
CIDMOD*           MOVE AT-RECORDED-DT         TO   WS-ACTION-DATE       
CIDMOD*           PERFORM 4000-BUILD-EXTRACT-E-RECORD-A.                
CIDMOD*                                                                 
012511*03131      IF AT-RECORDED-DT NOT LESS THAN WS-SIX-MONTHS-AGO            
012511*03132         IF AT-LETTER-ARCHIVE-NO GREATER THAN ZEROS                
012511*03133            PERFORM 2950-VERIFY-ELARCH-RECORD THRU 2969-EXIT.      
03134                                                                   
03135      IF AT-INITIAL-PRINT-DATE = LOW-VALUES AND                    
03136         AT-LETTER-SENT-DT NOT = LOW-VALUES AND                    
03137         AT-LETTER-ARCHIVE-NO < WS-NEW-START-ARCH-NO AND           
03138         AT-LETTER-ANSWERED-DT = LOW-VALUES AND                    
102810        (AT-STOP-LETTER-DT = LOW-VALUES OR SPACES) AND
03139         AT-LETTER-ARCHIVE-NO > ZEROS                              
03140 *         PERFORM 2950-VERIFY-ELARCH-RECORD THRU 2969-EXIT        
03141           MOVE AT-LETTER-ARCHIVE-NO TO WS-NEW-START-ARCH-NO.      
03142                                                                   
03143      IF AT-RESEND-PRINT-DATE = LOW-VALUES AND                     
03144         AT-AUTO-RE-SEND-DT NOT = LOW-VALUES AND                   
03145         AT-LETTER-ARCHIVE-NO < WS-NEW-START-ARCH-NO AND           
03146         AT-LETTER-ANSWERED-DT = LOW-VALUES AND                    
102810        (AT-STOP-LETTER-DT = LOW-VALUES OR SPACES) AND
03147         AT-LETTER-ARCHIVE-NO > ZEROS                              
03148 *         PERFORM 2950-VERIFY-ELARCH-RECORD THRU 2969-EXIT        
03149           MOVE AT-LETTER-ARCHIVE-NO TO WS-NEW-START-ARCH-NO.      
03150                                                                   
03151      IF WS-AIG-CORR-OUT = 'Y' OR                                  
03152         AT-LETTER-ANSWERED-DT NOT = LOW-VALUES                    
102810        OR (AT-STOP-LETTER-DT NOT = LOW-VALUES AND SPACES)
03153           GO TO 1235-SIP.                                         
03154                                                                   
03155      IF AT-RECEIPT-FOLLOW-UP NOT LESS THAN BIN-RUN-DATE           
03156         MOVE 'Y'                 TO WS-AIG-CORR-OUT               
03157         GO TO 1235-SIP.                                           
03158                                                                   
03159      IF ((AT-LETTER-SENT-DT NOT = LOW-VALUES) AND                 
03160         (AT-INITIAL-PRINT-DATE = LOW-VALUES))                     
03161                   OR                                              
03162         ((AT-AUTO-RE-SEND-DT NOT = LOW-VALUES) AND                
03163         (AT-RESEND-PRINT-DATE = LOW-VALUES))                      
03164             MOVE 'Y'                 TO WS-AIG-CORR-OUT.          
03165                                                                   
03166  1235-SIP.                                                        
03167                                                                   
020816     IF WS-COMPANY-ID IS EQUAL TO 'CID' OR 'DCC' or 'VPP'
CIDMOD         PERFORM 4300-BUILD-EXTRACT-F-RECORD-C                    
CIDMOD     END-IF
02505                                                                   
03168      GO TO 1210-SIP.                                              
03169                                                                   
03170  1240-SIP.                                                        
03171                                                                   
03172 *    NOTE ******************************************************* 
03173 *         *                                                     * 
03174 *         *          PROCESS THE ADDRESS TRAILER                * 
03175 *         *                                                     * 
03176 *         *******************************************************.
03177                                                                   
03178      IF AT-TRAILER-TYPE NOT = '5'                                 
03179          GO TO 1250-SIP.                                          
03180                                                                   
020816     IF WS-COMPANY-ID IS EQUAL TO 'CID' OR 'DCC' or 'VPP'
CIDMOD         PERFORM 4300-BUILD-EXTRACT-F-RECORD-C                    
CIDMOD     END-IF
02523                                                                   
121902*    IF WS-COMPANY-ID = 'DMD'                                     
080510       IF INSURED-ADDRESS                                        
080510          MOVE AT-SEQUENCE-NO     TO WS-INSURED-ADDR-SEQ-NO.     
03184                                                                   
03185      GO TO 1210-SIP.                                              
03186                                                                   
03187  1250-SIP.                                                        
03188                                                                   
03189 *    NOTE ******************************************************* 
03190 *         *                                                     * 
03191 *         *          PROCESS THE GENERAL INFO TRAILER           * 
03192 *         *                                                     * 
03193 *         *******************************************************.
03194                                                                   
03195      IF AT-TRAILER-TYPE NOT = '6'                                 
03196          GO TO 1260-SIP.                                          
03197                                                                   
020816     IF WS-COMPANY-ID IS EQUAL TO 'CID' OR 'DCC' or 'VPP'
CIDMOD         PERFORM 4300-BUILD-EXTRACT-F-RECORD-C                    
CIDMOD     END-IF
CIDMOD
020816     IF WS-COMPANY-ID = 'CID' OR 'DMD' OR 'DCC' or 'VPP'
03199          IF AT-SEQUENCE-NO = +90                                  
03200              MOVE AT-INFO-LINE-1     TO  WS-DIAGNOSIS-DESCRIP.    
03201                                                                   
03202 ***************************************************************   
03203 ***            'DMD' ONLY  - PROJECT 6449                   ***   
03204 ***   CREATES A STATISTIC RECORD FOR GENERAL INFO TRAILER   ***   
03205 ***************************************************************   
121902*    IF WS-COMPANY-ID = 'DMD'                                     
121902*        AND AT-RECORDED-DT > WS-LAST-PROCESS-DT                  
121902*        AND NOT AT-PAYMENT-NOTE                                  
121902*        AND NOT AT-CERT-CHANGE                                   
121902*        AND NOT AT-CONTINUED-NOTE                                
121902*        AND NOT AT-DIAGNOSIS-TRL                                 
121902*            MOVE AT-RECORDED-DT     TO WS-DMD-MAINTENANCE-DT     
121902*            MOVE AT-RECORDED-BY     TO WS-DMD-PROCESSOR-ID       
03214 ***    CALL IN                                              ***   
121902*            IF AT-INFO-TRAILER-TYPE = 'C'                        
121902*                IF AT-CALL-TYPE = 'I'                            
121902*                   MOVE '07'        TO WS-DMD-MAINTENANCE-TYPE   
121902*                   PERFORM 9500-DMD-ACTIVITY-FILE THRU 9500-EXIT 
03219 ***    CALL OUT                                             ***   
121902*                ELSE                                             
121902*                   MOVE '08'        TO WS-DMD-MAINTENANCE-TYPE   
121902*                   PERFORM 9500-DMD-ACTIVITY-FILE THRU 9500-EXIT 
03223 ***    GENERAL NOTES....                                    ***   
121902*            ELSE                                                 
121902*                MOVE '06'           TO WS-DMD-MAINTENANCE-TYPE   
121902*                PERFORM 9500-DMD-ACTIVITY-FILE THRU 9500-EXIT.   
03227                                                                   
03228      GO TO 1210-SIP.                                              
03229                                  EJECT                            
03230  1260-SIP.                                                        
03231 *    NOTE ******************************************************* 
03232 *         *                                                     * 
03233 *         *          PROCESS THE AUTO PROMPT TRAILER            * 
03234 *         *                                                     * 
03235 *         *******************************************************.
03236                                                                   
03237      IF AT-TRAILER-TYPE NOT = '7'                                 
03238          GO TO 1270-SIP.                                          
03239                                                                   
020816     IF WS-COMPANY-ID IS EQUAL TO 'CID' OR 'DCC' or 'VPP'
CIDMOD         PERFORM 4300-BUILD-EXTRACT-F-RECORD-C                    
CIDMOD     END-IF
02559                                                                   
121902*    IF WS-COMPANY-ID = 'DMD'                                     
121902*      IF AT-RECORDED-DT  >  WS-LAST-PROCESS-DT                   
121902*        MOVE '15'            TO  WS-DMD-MAINTENANCE-TYPE         
121902*        MOVE AT-RECORDED-DT  TO  WS-DMD-MAINTENANCE-DT           
121902*        MOVE AT-RECORDED-BY  TO  WS-DMD-PROCESSOR-ID             
121902*        PERFORM 9500-DMD-ACTIVITY-FILE THRU 9500-EXIT.           
03246                                                                   
03247      IF AT-PROMPT-START-DT GREATER THAN BIN-RUN-DATE              
03248          GO TO 1210-SIP.                                          
03249                                                                   
092909*03250      IF AT-PROMPT-END-DT LESS THAN WS-CURRENT-DATE-MINUS-2        
092909     IF AT-PROMPT-END-DT <= WS-PREV-CYCLE-BIN
03251          GO TO 1210-SIP.                                          
03252                                                                   
03253      PERFORM 3900-BUILD-EXTRACT-D-RECORD-G.                       
03254                                                                   
03255      GO TO 1210-SIP.                                              
03256                                  EJECT                            
03257  1270-SIP.                                                        
03258 *    NOTE ******************************************************* 
03259 *         *                                                     * 
03260 *         *          PROCESS THE DENIAL TRAILER                 * 
03261 *         *                                                     * 
03262 *         *******************************************************.
03263                                                                   
03264      IF AT-TRAILER-TYPE NOT = '8'                                 
03265          GO TO 1280-SIP.                                          
03266                                                                   
020816     IF WS-COMPANY-ID IS EQUAL TO 'CID' OR 'DCC' or 'VPP'
CIDMOD         PERFORM 4300-BUILD-EXTRACT-F-RECORD-C                    
CIDMOD     END-IF
02582                                                                   
03267 ***************************************************************   
03268 ***            'DMD' ONLY  - PROJECT 6449                   ***   
03269 ***   CREATES A STATISTIC RECORD FOR DENIALS                ***   
03270 ***************************************************************   
121902*    IF WS-COMPANY-ID = 'DMD'                                     
121902*       IF AT-RECORDED-DT > WS-LAST-PROCESS-DT                    
121902*            MOVE '13'               TO WS-DMD-MAINTENANCE-TYPE   
121902*            MOVE AT-RECORDED-DT     TO WS-DMD-MAINTENANCE-DT     
121902*            MOVE AT-RECORDED-BY     TO WS-DMD-PROCESSOR-ID       
121902*            PERFORM 9500-DMD-ACTIVITY-FILE THRU 9500-EXIT.       
03277                                                                   
03278      IF AT-RETRACTION-DT = SPACES                                 
03279          MOVE LOW-VALUES         TO  AT-RETRACTION-DT             
03280          PERFORM 8100-REWRITE-TRAILER.                            
03281                                                                   
03282      IF CLAIM-IS-CLOSED                                           
03283         IF CL-LAST-CLOSE-REASON = ' '                             
03284            MOVE 'CLAIM CLOSE REASON CORRECTED ON DENIALS'         
03285                              TO WS-D1-MESSAGE                     
03286            MOVE WS-DETAIL1   TO PRT                               
03287            PERFORM WRITE-A-LINE                                   
03288            MOVE SPACES       TO WS-DETAIL1                        
03289            MOVE '2'          TO CL-LAST-CLOSE-REASON.             
03290                                                                   
03291      GO TO 1210-SIP.                                              
03292                                                                   
03293  1280-SIP.                                                        
03294                                                                   
03295 *    NOTE ******************************************************* 
03296 *         *                                                     * 
03297 *         *        PROCESS THE INCURRED CHANGE TRAILER          * 
03298 *         *                                                     * 
03299 *         *******************************************************.
03300                                                                   
03301      IF AT-TRAILER-TYPE NOT = '9'                                 
03302          GO TO 1290-SIP.                                          
03303                                                                   
020816     IF WS-COMPANY-ID IS EQUAL TO 'CID' OR 'DCC' or 'VPP'
CIDMOD         PERFORM 4300-BUILD-EXTRACT-F-RECORD-C                    
CIDMOD     END-IF
02612                                                                   
03304      GO TO 1210-SIP.                                              
03305                                  EJECT                            
03306  1290-SIP.                                                        
03307 *    NOTE ******************************************************* 
03308 *         *                                                     * 
03309 *         *              PROCESS THE FORMS TRAILER              * 
03310 *         *                                                     * 
03311 *         *******************************************************.
03312                                                                   
03313      IF AT-TRAILER-TYPE NOT = 'A'                                 
03314          GO TO 1210-SIP.                                          
03315                                                                   
03316      IF CL-LAST-MAINT-DT LESS THAN AT-RECORDED-DT                 
03317          MOVE AT-RECORDED-DT         TO  CL-LAST-MAINT-DT         
03318          MOVE '2'                    TO  CL-LAST-MAINT-TYPE       
03319          MOVE AT-LAST-MAINT-HHMMSS   TO  CL-LAST-MAINT-HHMMSS     
03320          MOVE AT-RECORDED-BY         TO  CL-LAST-MAINT-USER       
03321          IF CLAIM-IS-CLOSED                                       
03322              MOVE 'O'                TO  CL-CLAIM-STATUS.         
03323                                                                   
03324      IF CL-NEXT-FOLLOWUP-DT LESS THAN AT-FORM-FOLLOW-UP-DT        
03325          MOVE AT-FORM-FOLLOW-UP-DT  TO  CL-NEXT-FOLLOWUP-DT.      
03326                                                                   
03327      IF CL-NEXT-RESEND-DT LESS THAN AT-FORM-RE-SEND-DT            
03328          MOVE AT-FORM-RE-SEND-DT  TO  CL-NEXT-RESEND-DT.          
03329                                                                   
03330      IF (AT-FORM-RE-SEND-DT NOT LESS THAN WS-CURRENT-DATE-MINUS-2 
03331        AND                                                        
03332         AT-FORM-RE-SEND-DT NOT GREATER THAN                       
03333                              WS-CURRENT-DATE-PLUS-2)              
03334        AND                                                        
03335         ((AT-FORM-SEND-ON-DT NOT = LOW-VALUES AND                 
03336          AT-FORM-ANSWERED-DT = LOW-VALUES)                        
03337             OR                                                    
03338         (AT-EMP-FORM-SEND-ON-DT NOT = LOW-VALUES AND              
03339          AT-EMP-FORM-ANSWERED-DT = LOW-VALUES)                    
03340             OR                                                    
03341         (AT-PHY-FORM-SEND-ON-DT NOT = LOW-VALUES AND              
03342          AT-PHY-FORM-ANSWERED-DT = LOW-VALUES))                   
03343              PERFORM 3400-BUILD-EXTRACT-D-RECORD-B.               
03344                                                                   
03345      IF AT-FORM-ANSWERED-DT = WS-ACTUAL-DATE                      
03346         MOVE AT-FORM-ANSWERED-DT  TO  WS-ACTION-DATE              
03347         PERFORM 3960-BUILD-EXTRACT-D-RECORD-I.                    
03348                                                                   
03349      IF AT-EMP-FORM-ANSWERED-DT = WS-ACTUAL-DATE                  
03350         MOVE AT-EMP-FORM-ANSWERED-DT  TO  WS-ACTION-DATE          
03351         PERFORM 3960-BUILD-EXTRACT-D-RECORD-I.                    
03352                                                                   
03353      IF AT-PHY-FORM-ANSWERED-DT = WS-ACTUAL-DATE                  
03354         MOVE AT-PHY-FORM-ANSWERED-DT  TO  WS-ACTION-DATE          
03355         PERFORM 3960-BUILD-EXTRACT-D-RECORD-I.                    
03356                                                                   
03357      IF (AT-FORM-SEND-ON-DT NOT LESS THAN WS-PRIOR-MONTH AND      
03358          AT-FORM-SEND-ON-DT NOT GREATER THAN BIN-RUN-DATE)        
03359              MOVE AT-FORM-SEND-ON-DT  TO  WS-ACTION-DATE          
03360              PERFORM 4000-BUILD-EXTRACT-E-RECORD-A                
03361              MOVE AT-EMP-FORM-SEND-ON-DT  TO  WS-ACTION-DATE      
03362              PERFORM 4000-BUILD-EXTRACT-E-RECORD-A                
03363              MOVE AT-PHY-FORM-SEND-ON-DT  TO  WS-ACTION-DATE      
03364              PERFORM 4000-BUILD-EXTRACT-E-RECORD-A                
03365        ELSE                                                       
03366      IF (AT-FORM-ANSWERED-DT NOT LESS THAN WS-PRIOR-MONTH AND     
03367          AT-FORM-ANSWERED-DT NOT GREATER THAN BIN-RUN-DATE)       
03368              OR                                                   
03369         (AT-EMP-FORM-ANSWERED-DT NOT LESS THAN WS-PRIOR-MONTH AND 
03370         AT-EMP-FORM-ANSWERED-DT NOT GREATER THAN BIN-RUN-DATE)    
03371              OR                                                   
03372         (AT-PHY-FORM-ANSWERED-DT NOT LESS THAN WS-PRIOR-MONTH AND 
03373         AT-PHY-FORM-ANSWERED-DT NOT GREATER THAN BIN-RUN-DATE)    
03374             MOVE AT-FORM-ANSWERED-DT  TO  WS-ACTION-DATE          
03375             PERFORM 4000-BUILD-EXTRACT-E-RECORD-A                 
03376             MOVE AT-EMP-FORM-ANSWERED-DT  TO  WS-ACTION-DATE      
03377             PERFORM 4000-BUILD-EXTRACT-E-RECORD-A                 
03378             MOVE AT-PHY-FORM-ANSWERED-DT  TO  WS-ACTION-DATE      
03379             PERFORM 4000-BUILD-EXTRACT-E-RECORD-A                 
03380        ELSE                                                       
03381      IF (AT-FORM-RE-SEND-DT NOT LESS THAN WS-PRIOR-MONTH AND      
03382          AT-FORM-RE-SEND-DT NOT GREATER THAN BIN-RUN-DATE)        
03383             AND                                                   
03384         ((AT-FORM-SEND-ON-DT NOT = LOW-VALUES AND                 
03385          AT-FORM-ANSWERED-DT = LOW-VALUES)                        
03386             OR                                                    
03387         (AT-EMP-FORM-SEND-ON-DT NOT = LOW-VALUES AND              
03388          AT-EMP-FORM-ANSWERED-DT = LOW-VALUES)                    
03389             OR                                                    
03390         (AT-PHY-FORM-SEND-ON-DT NOT = LOW-VALUES AND              
03391          AT-PHY-FORM-ANSWERED-DT = LOW-VALUES))                   
03392            MOVE AT-FORM-RE-SEND-DT  TO  WS-ACTION-DATE            
03393            PERFORM 4000-BUILD-EXTRACT-E-RECORD-A                  
03394        ELSE                                                       
03395      IF (AT-FORM-REPRINT-DT NOT LESS THAN WS-PRIOR-MONTH AND      
03396          AT-FORM-REPRINT-DT NOT GREATER THAN BIN-RUN-DATE)        
03397              MOVE AT-FORM-REPRINT-DT TO  WS-ACTION-DATE           
03398              PERFORM 4000-BUILD-EXTRACT-E-RECORD-A                
03399        ELSE                                                       
03400      IF (AT-FORM-FOLLOW-UP-DT NOT LESS THAN WS-PRIOR-MONTH AND    
03401          AT-FORM-FOLLOW-UP-DT NOT GREATER THAN BIN-RUN-DATE)      
03402            AND                                                    
03403         ((AT-FORM-SEND-ON-DT NOT = LOW-VALUES AND                 
03404          AT-FORM-ANSWERED-DT = LOW-VALUES)                        
03405             OR                                                    
03406         (AT-EMP-FORM-SEND-ON-DT NOT = LOW-VALUES AND              
03407          AT-EMP-FORM-ANSWERED-DT = LOW-VALUES)                    
03408             OR                                                    
03409         (AT-PHY-FORM-SEND-ON-DT NOT = LOW-VALUES AND              
03410          AT-PHY-FORM-ANSWERED-DT = LOW-VALUES))                   
03411            MOVE AT-FORM-FOLLOW-UP-DT  TO  WS-ACTION-DATE          
03412            PERFORM 4000-BUILD-EXTRACT-E-RECORD-A.                 
03413                                                                   
020816     IF WS-COMPANY-ID IS EQUAL TO 'CID' OR 'DCC' or 'VPP'
CIDMOD         PERFORM 4300-BUILD-EXTRACT-F-RECORD-C                    
CIDMOD     END-IF
02740                                                                   
03414      GO TO 1210-SIP.                                              
03415      EJECT                                                        
03416  1300-SIP.                                                        
03417                                                                   
020816     IF WS-COMPANY-ID IS EQUAL TO 'CID' OR 'DCC' or 'VPP'
CIDMOD         PERFORM 4100-BUILD-EXTRACT-F-RECORD-A                    
CIDMOD         PERFORM 4200-BUILD-EXTRACT-F-RECORD-B                    
CIDMOD     END-IF

           .
03421  1301-CONTINUE.                                                   
121902*    IF WS-COMPANY-ID NOT = 'AIG'                                 
03423         GO TO 1305-SIP-CHECK-AAR.                                 
03424                                                                   
03425 ***************** AIG CODE START ****************                 
121902*    IF CL-CERT-PRIME = 'INCOMPLETE'                              
121902*     IF CL-CLAIM-STATUS = 'O'                                    
121902*       PERFORM 3990-BUILD-EXTRACT-D-RECORD-L THRU 3999-EXIT.     
03429                                                                   
121902*    IF CL-ACTIVITY-CODE = ZEROS OR '02'                          
121902*       NEXT SENTENCE                                             
121902*    ELSE                                                         
121902*       GO TO 1305-SIP-CHECK-AAR.                                 
03434                                                                   
121902*    IF WS-AIG-CORR-OUT = 'N'     AND                             
121902*       CL-CLAIM-STATUS = 'O'     AND                             
121902*       WS-AIG-LAST-PARTIAL = 'Y' AND                             
121902*       CL-LAG-REPORT-CODE LESS THAN 3                            
121902*          NEXT SENTENCE                                          
121902*       ELSE                                                      
121902*          GO TO 1310-SIP.                                        
03442                                                                   
121902*    MOVE WS-AIG-LAST-PMT-DT     TO DC-BIN-DATE-1.                
121902*    MOVE BIN-RUN-DATE           TO DC-BIN-DATE-2.                
121902*    MOVE '1'                    TO DC-OPTION-CODE.               
121902*    PERFORM 8500-DATE-CONVERSION.                                
03447                                                                   
121902*    IF NO-CONVERSION-ERROR                                       
121902*      IF CL-CURRENT-STATE = '04'                                 
121902*        IF DC-ELAPSED-DAYS GREATER THAN +89                      
121902*          IF CL-LAG-REPORT-CODE = 1                              
121902*            MOVE 'LAG2'         TO  WS-AAR-LETTER                
121902*            PERFORM 2220-GEN-LETTER.                             
03454                                                                   
121902*    IF NO-CONVERSION-ERROR                                       
121902*       IF (DC-ELAPSED-DAYS GREATER THAN +59) AND                 
121902*          (CL-LAG-REPORT-CODE LESS THAN 1)                       
121902*            PERFORM 3970-BUILD-EXTRACT-D-RECORD-J THRU 3979-EXIT 
121902*            MOVE 'LAG1' TO WS-AAR-LETTER                         
121902*            PERFORM 2220-GEN-LETTER                              
121902*            ADD 1 TO CL-LAG-REPORT-CODE                          
121902*         ELSE                                                    
121902*       IF (DC-ELAPSED-DAYS GREATER THAN +89) AND                 
121902*          (CL-LAG-REPORT-CODE LESS THAN 2)                       
121902*            PERFORM 3970-BUILD-EXTRACT-D-RECORD-J THRU 3979-EXIT 
121902*            ADD 1 TO CL-LAG-REPORT-CODE                          
121902*         ELSE                                                    
121902*       IF (DC-ELAPSED-DAYS GREATER THAN +119) AND                
121902*          (CL-LAG-REPORT-CODE LESS THAN 3)                       
121902*            PERFORM 3970-BUILD-EXTRACT-D-RECORD-J THRU 3979-EXIT 
121902*            ADD 1 TO CL-LAG-REPORT-CODE.                         
121902*                                                                 
03473 *--------------------------------------------------------------*  
03474 *    CHECK IF CURRENT STATE EQUAL TO CALIFORNIA                *  
03475 *--------------------------------------------------------------*  
03476                                                                   
121902*    IF CL-CURRENT-STATE = '04'                                   
121902*        IF DC-ELAPSED-DAYS GREATER THAN +119                     
121902*            IF CL-LAG-REPORT-CODE = 3                            
121902*                MOVE 'LAG3'       TO  WS-AAR-LETTER              
121902*                MOVE ZEROS        TO  CL-LAG-REPORT-CODE         
121902*                PERFORM 2220-GEN-LETTER                          
121902*                PERFORM 2000-CLOSE-CLAIM                         
121902*                MOVE 07           TO  CL-ACTIVITY-CODE           
121902*                MOVE BIN-RUN-DATE TO  CL-ACTIVITY-MAINT-DT       
121902*                MOVE 'CLOS'       TO  CL-ACTIVITY-MAINT-TYPE.    
03487                                                                   
121902*    IF CL-CURRENT-STATE NOT = '04'                               
121902*        IF DC-ELAPSED-DAYS GREATER THAN +89                      
121902*            IF CL-LAG-REPORT-CODE = 2                            
121902*                MOVE 'LAG2'       TO  WS-AAR-LETTER              
121902*                MOVE ZEROS        TO  CL-LAG-REPORT-CODE         
121902*                PERFORM 2220-GEN-LETTER                          
121902*                PERFORM 2000-CLOSE-CLAIM                         
121902*                MOVE 07           TO  CL-ACTIVITY-CODE           
121902*                MOVE BIN-RUN-DATE TO  CL-ACTIVITY-MAINT-DT       
121902*                MOVE 'CLOS'       TO  CL-ACTIVITY-MAINT-TYPE.    
03498                                                                   
121902*    GO TO 1310-SIP.                                              
03500 ***************** AIG CODE END ****************                   
03501                                                                   
03502  1305-SIP-CHECK-AAR.                                              
03503                                                                   
03504      IF WS-AUTO-ACTIVITY-SW = 'N'                                 
03505          GO TO 1310-SIP.                                          
03506                                                                   
03507      IF CL-ACTIVITY-CODE = 00                                     
03508         GO TO 1310-SIP.                                           
03509                                                                   
03510      IF CL-ACTIVITY-CODE LESS THAN 18                             
03511         NEXT SENTENCE                                             
03512      ELSE                                                         
03513         GO TO 1310-SIP.                                           
03514                                                                   
03515      MOVE CL-ACTIVITY-CODE       TO AAR-SUB.                      
03516                                                                   
03517      IF AAR-SUB GREATER THAN 9                                    
03518          SUBTRACT 9 FROM AAR-SUB                                  
03519          IF WS-USER-REPORT-DAYS (AAR-SUB) = ZEROS                 
03520              GO TO 1310-SIP                                       
03521          ELSE                                                     
03522              DIVIDE WS-USER-REPORT-DAYS (AAR-SUB) BY +30          
03523                     GIVING WS-ACT-MONTHS                          
03524                     REMAINDER WS-ACT-REMAINDER                    
03525      ELSE                                                         
03526          IF WS-SYS-REPORT-DAYS (AAR-SUB) = ZEROS                  
03527             GO TO 1310-SIP                                        
03528          ELSE                                                     
03529              DIVIDE WS-SYS-REPORT-DAYS (AAR-SUB) BY +30           
03530                     GIVING WS-ACT-MONTHS                          
03531                     REMAINDER WS-ACT-REMAINDER.                   
03532                                                                   
121902*    IF (CL-ACTIVITY-CODE = 01 OR 08)                             
121902*        IF (WS-COMPANY-ID = 'AIG' OR 'AUK')                      
121902*            IF WS-AIG-CORR-OUT = 'Y'                             
121902*                GO TO 1310-SIP.                                  
03537                                                                   
03538      IF CL-ACTIVITY-CODE = 1                                      
03539         MOVE CL-FILE-ESTABLISH-DT    TO  WS-AIG-WORK-DT           
03540      ELSE                                                         
03541      IF CL-ACTIVITY-CODE = 2 OR 9                                 
03542         MOVE WS-AIG-LAST-PART-DT     TO  WS-AIG-WORK-DT           
03543      ELSE                                                         
03544      IF CL-ACTIVITY-CODE = 3                                      
03545         MOVE WS-AIG-LAST-THRU-EXP-DT TO  WS-AIG-WORK-DT           
03546      ELSE                                                         
03547      IF CL-ACTIVITY-CODE = 5                                      
03548         MOVE WS-AIG-AUTO-START-DT    TO  WS-AIG-WORK-DT           
03549      ELSE                                                         
03550      IF CL-ACTIVITY-CODE = 6                                      
03551         MOVE WS-AIG-AUTO-END-DT      TO  WS-AIG-WORK-DT           
03552      ELSE                                                         
03553      IF CL-ACTIVITY-CODE = 7                                      
03554         MOVE CL-LAST-CLOSE-DT        TO  WS-AIG-WORK-DT           
03555      ELSE                                                         
03556      IF CL-ACTIVITY-CODE = 8                                      
03557         MOVE CL-LAST-REOPEN-DT       TO  WS-AIG-WORK-DT           
03558      ELSE                                                         
03559         MOVE CL-ACTIVITY-MAINT-DT    TO  WS-AIG-WORK-DT.          
03560                                                                   
03561      IF WS-AIG-WORK-DT = LOW-VALUES                               
03562         GO TO 1310-SIP.                                           
03563                                                                   
03564      MOVE +0                     TO DC-ELAPSED-MONTHS             
03565                                     DC-ELAPSED-DAYS.              
03566      MOVE WS-AIG-WORK-DT         TO DC-BIN-DATE-1.                
03567      MOVE BIN-RUN-DATE           TO DC-BIN-DATE-2.                
03568      MOVE '1'                    TO DC-OPTION-CODE.               
03569      PERFORM 8500-DATE-CONVERSION.                                
03570                                                                   
03571      IF DATE-CONVERSION-ERROR                                     
03572         GO TO 1310-SIP.                                           
03573                                                                   
03574 ******************************************************************
03575 ***    THE FOLLOWING ROUTINE ELIMINATES 'WEEK-END' DAYS        ***
03576 ***    FROM THE ELAPSED-DAYS CALCULATION.                      ***
03577 ******************************************************************
03578                                                                   
121902*    IF WS-COMPANY-ID NOT = 'ACE'                                 
03580          GO TO 1306-SIP.                                          
03581                                                                   
03582 ***************** ACE CODE START **************                   
121902*    MOVE ZEROS                     TO WS-WEEKS                   
121902*                                      WS-DAYS-REMAINING          
121902*                                      WS-WORK-DAYS.              
121902*                                                                 
121902*    IF DC-ELAPSED-DAYS LESS THAN 7                               
121902*        MOVE DC-ELAPSED-DAYS       TO WS-DAYS-REMAINING          
121902*    ELSE                                                         
121902*        DIVIDE DC-ELAPSED-DAYS BY 7 GIVING    WS-WEEKS           
121902*                                    REMAINDER WS-DAYS-REMAINING  
121902*        COMPUTE WS-WORK-DAYS = WS-WEEKS * 5                      
121902*        IF WS-DAYS-REMAINING = ZERO                              
121902*            MOVE WS-WORK-DAYS      TO DC-ELAPSED-DAYS            
121902*            GO TO 1306-SIP.                                      
121902*                                                                 
121902*                                                                 
03598 * ADJUST ELAPSED-DAYS FOR WEEK-END DAYS                           
121902*                                                                 
03600 * SUNDAY                                                          
121902*    IF DC-DAY-OF-WEEK = 1                                        
121902*        IF WS-DAYS-REMAINING GREATER THAN 1                      
121902*            SUBTRACT 1 FROM WS-DAYS-REMAINING                    
121902*            ADD WS-DAYS-REMAINING TO WS-WORK-DAYS.               
121902*                                                                 
03606 * MONDAY                                                          
121902*    IF DC-DAY-OF-WEEK = 2                                        
121902*        IF WS-DAYS-REMAINING GREATER THAN 5                      
121902*            ADD 5 TO WS-WORK-DAYS                                
121902*        ELSE                                                     
121902*            ADD WS-DAYS-REMAINING TO WS-WORK-DAYS.               
121902*                                                                 
03613 * TUESDAY                                                         
121902*    IF DC-DAY-OF-WEEK = 3                                        
121902*        IF WS-DAYS-REMAINING GREATER THAN 4                      
121902*            ADD 4 TO WS-WORK-DAYS                                
121902*        ELSE                                                     
121902*            ADD WS-DAYS-REMAINING TO WS-WORK-DAYS.               
121902*                                                                 
03620 * WEDNESDAY                                                       
121902*    IF DC-DAY-OF-WEEK = 4                                        
121902*        IF WS-DAYS-REMAINING = 6                                 
121902*            ADD 4 TO WS-WORK-DAYS                                
121902*        ELSE                                                     
121902*            IF WS-DAYS-REMAINING GREATER THAN 3                  
121902*                ADD 3 TO WS-WORK-DAYS                            
121902*            ELSE                                                 
121902*                ADD WS-DAYS-REMAINING TO WS-WORK-DAYS.           
121902*                                                                 
03630 * THURSDAY                                                        
121902*    IF DC-DAY-OF-WEEK = 5                                        
121902*        IF WS-DAYS-REMAINING = 6                                 
121902*            ADD 4 TO WS-WORK-DAYS                                
121902*        ELSE                                                     
121902*            IF WS-DAYS-REMAINING = 5                             
121902*                ADD 3 TO WS-WORK-DAYS                            
121902*            ELSE                                                 
121902*                IF WS-DAYS-REMAINING GREATER THAN 2              
121902*                    ADD 2 TO WS-WORK-DAYS                        
121902*                ELSE                                             
121902*                    ADD WS-DAYS-REMAINING TO WS-WORK-DAYS.       
03642                                                                   
03643 * FRIDAY                                                          
121902*    IF DC-DAY-OF-WEEK = 6                                        
121902*        IF WS-DAYS-REMAINING = 6                                 
121902*            ADD 4 TO WS-WORK-DAYS                                
121902*        ELSE                                                     
121902*            IF WS-DAYS-REMAINING = 5                             
121902*                ADD 3 TO WS-WORK-DAYS                            
121902*            ELSE                                                 
121902*                IF WS-DAYS-REMAINING = 4                         
121902*                    ADD 2 TO WS-WORK-DAYS                        
121902*                ELSE                                             
121902*                    ADD 1 TO WS-WORK-DAYS.                       
03655                                                                   
03656 * SATURDAY                                                        
121902*    IF DC-DAY-OF-WEEK = 7                                        
121902*        IF WS-DAYS-REMAINING GREATER THAN 2                      
121902*            SUBTRACT 2 FROM WS-DAYS-REMAINING                    
121902*            ADD WS-DAYS-REMAINING TO WS-WORK-DAYS.               
03661                                                                   
121902*    MOVE WS-WORK-DAYS              TO DC-ELAPSED-DAYS.           
03663 ***************** ACE CODE END ****************                   
03664                                                                   
03665  1306-SIP.                                                        
03666                                                                   
03667      IF CL-ACTIVITY-CODE IS GREATER THAN 09                       
03668          GO TO 1308-SIP-CHECK-AAR.                                
03669                                                                   
03670      IF (WS-SYS-EACH-DAY-AFTER-SW (AAR-SUB) = 'Y') AND            
03671       (DC-ELAPSED-DAYS GREATER THAN WS-SYS-REPORT-DAYS (AAR-SUB)) 
03672           PERFORM 3980-BUILD-EXTRACT-D-RECORD-K THRU 3989-EXIT    
03673           GO TO 1310-SIP.                                         
03674                                                                   
03675      IF (WS-ACT-REMAINDER NOT = ZEROS)                            
03676         IF (DC-ELAPSED-DAYS NOT LESS THAN                         
03677                          WS-SYS-REPORT-DAYS (AAR-SUB))            
03678            IF CL-LAPSE-REPORT-CODE = ZEROS                        
03679               PERFORM 3980-BUILD-EXTRACT-D-RECORD-K THRU 3989-EXIT
03680               MOVE 1             TO CL-LAPSE-REPORT-CODE          
03681               GO TO 1310-SIP                                      
03682            ELSE                                                   
03683               GO TO 1310-SIP                                      
03684         ELSE                                                      
03685            GO TO 1310-SIP.                                        
03686                                                                   
03687      IF CL-LAPSE-REPORT-CODE NOT LESS THAN WS-ACT-MONTHS          
03688          GO TO 1310-SIP.                                          
03689                                                                   
03690      IF CL-LAPSE-REPORT-CODE = 0                                  
03691          IF DC-ELAPSED-DAYS GREATER THAN +29                      
03692              PERFORM 3980-BUILD-EXTRACT-D-RECORD-K THRU 3989-EXIT 
03693              MOVE 1              TO  CL-LAPSE-REPORT-CODE         
03694              GO TO 1310-SIP.                                      
03695                                                                   
03696      IF CL-LAPSE-REPORT-CODE = 1                                  
03697          IF DC-ELAPSED-DAYS GREATER THAN +59                      
03698              PERFORM 3980-BUILD-EXTRACT-D-RECORD-K THRU 3989-EXIT 
03699              ADD 1               TO  CL-LAPSE-REPORT-CODE         
03700              GO TO 1310-SIP.                                      
03701                                                                   
03702      IF CL-LAPSE-REPORT-CODE = 2                                  
03703          IF DC-ELAPSED-DAYS GREATER THAN +89                      
03704              PERFORM 3980-BUILD-EXTRACT-D-RECORD-K THRU 3989-EXIT 
03705              ADD 1               TO  CL-LAPSE-REPORT-CODE         
03706              GO TO 1310-SIP.                                      
03707                                                                   
03708      IF CL-LAPSE-REPORT-CODE = 3                                  
03709          IF DC-ELAPSED-DAYS GREATER THAN +119                     
03710              PERFORM 3980-BUILD-EXTRACT-D-RECORD-K THRU 3989-EXIT 
03711              ADD 1               TO  CL-LAPSE-REPORT-CODE         
03712              GO TO 1310-SIP.                                      
03713                                                                   
03714      IF CL-LAPSE-REPORT-CODE = 4                                  
03715          IF DC-ELAPSED-DAYS GREATER THAN +149                     
03716              PERFORM 3980-BUILD-EXTRACT-D-RECORD-K THRU 3989-EXIT 
03717              ADD 1               TO  CL-LAPSE-REPORT-CODE         
03718              GO TO 1310-SIP.                                      
03719                                                                   
03720      GO TO 1310-SIP.                                              
03721                                                                   
03722  1308-SIP-CHECK-AAR.                                              
03723                                                                   
03724      IF (WS-USER-EACH-DAY-AFTER-SW (AAR-SUB) = 'Y') AND           
03725       (DC-ELAPSED-DAYS GREATER THAN WS-USER-REPORT-DAYS (AAR-SUB))
03726         PERFORM 3980-BUILD-EXTRACT-D-RECORD-K THRU 3989-EXIT      
03727         GO TO 1310-SIP.                                           
03728                                                                   
03729      IF (WS-ACT-REMAINDER NOT = ZEROS)                            
03730         IF (DC-ELAPSED-DAYS NOT LESS THAN                         
03731                          WS-USER-REPORT-DAYS (AAR-SUB))           
03732            IF CL-LAPSE-REPORT-CODE = ZEROS                        
03733               PERFORM 3980-BUILD-EXTRACT-D-RECORD-K THRU 3989-EXIT
03734               MOVE 1             TO CL-LAPSE-REPORT-CODE          
03735               GO TO 1310-SIP                                      
03736            ELSE                                                   
03737               GO TO 1310-SIP                                      
03738         ELSE                                                      
03739            GO TO 1310-SIP.                                        
03740                                                                   
03741      IF CL-LAPSE-REPORT-CODE NOT LESS THAN WS-ACT-MONTHS          
03742          GO TO 1310-SIP.                                          
03743                                                                   
03744      IF CL-LAPSE-REPORT-CODE = 0                                  
03745          IF DC-ELAPSED-DAYS GREATER THAN +29                      
03746              MOVE 1              TO  CL-LAPSE-REPORT-CODE         
03747              PERFORM 3980-BUILD-EXTRACT-D-RECORD-K THRU 3989-EXIT 
03748              GO TO 1310-SIP.                                      
03749                                                                   
03750      IF CL-LAPSE-REPORT-CODE = 1                                  
03751          IF DC-ELAPSED-DAYS GREATER THAN +59                      
03752              PERFORM 3980-BUILD-EXTRACT-D-RECORD-K THRU 3989-EXIT 
03753              ADD 1               TO  CL-LAPSE-REPORT-CODE         
03754              GO TO 1310-SIP.                                      
03755                                                                   
03756      IF CL-LAPSE-REPORT-CODE = 2                                  
03757          IF DC-ELAPSED-DAYS GREATER THAN +89                      
03758              PERFORM 3980-BUILD-EXTRACT-D-RECORD-K THRU 3989-EXIT 
03759              ADD 1               TO  CL-LAPSE-REPORT-CODE         
03760              GO TO 1310-SIP.                                      
03761                                                                   
03762      IF CL-LAPSE-REPORT-CODE = 3                                  
03763          IF DC-ELAPSED-DAYS GREATER THAN +119                     
03764              PERFORM 3980-BUILD-EXTRACT-D-RECORD-K THRU 3989-EXIT 
03765              ADD 1               TO  CL-LAPSE-REPORT-CODE         
03766              GO TO 1310-SIP.                                      
03767                                                                   
03768      IF CL-LAPSE-REPORT-CODE = 4                                  
03769          IF DC-ELAPSED-DAYS GREATER THAN +149                     
03770              PERFORM 3980-BUILD-EXTRACT-D-RECORD-K THRU 3989-EXIT 
03771              ADD 1               TO  CL-LAPSE-REPORT-CODE         
03772              GO TO 1310-SIP.                                      
03773                                                                   
03774      EJECT                                                        
03775  1310-SIP.                                                        
03776                                                                   
03777 *************************************************************     
03778 *** IN AUGUST OF 1995, THE 'RETRIEVE' SYSTEM WAS CREATED. ***     
03779 *** CLAIMS NO LONGER WILL BE PURGED FROM EL310.           ***     
03780 *************************************************************     
03781                                                                   
03782      IF CLAIM-IS-CLOSED                                           
03783        AND THIS-IS-MONTH-END                                      
03784        AND CL-LAST-MAINT-DT    LESS THAN WS-PURGE-DATE            
03785        AND CL-NEXT-AUTO-PAY-DT LESS THAN WS-PURGE-DATE            
03786        AND CL-NEXT-RESEND-DT   LESS THAN WS-PURGE-DATE            
03787        AND CL-NEXT-FOLLOWUP-DT LESS THAN WS-PURGE-DATE            
03788        AND CL-LAST-PMT-DT      LESS THAN WS-PURGE-DATE            
03789        AND CL-PAID-THRU-DT     LESS THAN WS-PURGE-DATE            
03790        AND CL-HISTORY-ARCHIVE-DT NOT = LOW-VALUES                 
03791        AND WS-PURGE-DATE NOT = BIN-RUN-DATE                       
03792 *        PERFORM 2100-PURGE-CLAIM                                 
03793          GO TO 1120-SIP.                                          
03794                                                                   
03795      IF WS-FORCE-COUNT GREATER THAN ZERO OR                       
03796        CL-FATAL-ERROR-CNT GREATER THAN ZERO                       
03797          PERFORM 3800-BUILD-EXTRACT-D-RECORD-F.                   
03798                                                                   
03799      IF WS-AT-CHG-EXP     = WS-CHG-EXP   AND                      
03800         WS-AT-NON-CHG-EXP = WS-NON-CHG-EXP                        
03801           GO TO 1320-SIP.                                         
03802                                                                   
03803      MOVE CL-CONTROL-PRIMARY     TO  AT-CONTROL-PRIMARY.          
03804      MOVE ZERO                   TO  AT-SEQUENCE-NO.              
03805                                                                   
03806      PERFORM 8300-READ-TRAILER.                                   
03807                                                                   
03808      MOVE WS-CHG-EXP             TO  AT-ITD-CHARGEABLE-EXPENSE.   
03809      MOVE WS-NON-CHG-EXP         TO  AT-ITD-PAID-EXPENSES.        
03810                                                                   
03811      PERFORM 8100-REWRITE-TRAILER.                                
03812                                                                   
03813  1320-SIP.                                                        
03814                                                                   
03815 *         ******************************************************* 
03816 *         *   THE FOLLOWING CODE HAS BEEN ADDED TO CAUSE AIMS   * 
03817 *         * INTERFACE DATA TO BE CREATED.                       * 
03818 *         ******************************************************* 
03819                                                                   
121902*    IF WS-COMPANY-ID = 'AIG' OR 'AUK'                            
121902*        PERFORM 9300-AIMS-CL-MSTR-INTERFACE THRU 9349-EXIT.      
03822                                                                   
03823      PERFORM 8200-REWRITE-CLAIM.                                  
03824                                                                   
121902*    IF CL-SYSTEM-IDENTIFIER = 'CV'                               
121902*        IF WS-REWRITE-POLICY NOT = ZERO                          
121902*            PERFORM 8450-REWRITE-POLICY.                         
03828                                                                   
121902*    IF CL-SYSTEM-IDENTIFIER NOT = 'CV'                           
03830          IF WS-CERT-NOT-FOUND NOT = ZERO                          
03831              PERFORM 8800-WRITE-CERTIFICATE                       
03832          ELSE                                                     
03833              IF WS-REWRITE-CERT NOT = ZERO                        
03834                  PERFORM 8400-REWRITE-CERT.                       
03835                                                                   
03836      GO TO 1120-SIP.                                              
03837                                  EJECT                            
03838  1900-SIP.                                                        
03839 *    NOTE ******************************************************* 
03840 *         *                  COMPANY TOTALS                     * 
03841 *         *******************************************************.
03842                                                                   
03843      IF WS-EXTRACT-DA-COUNT GREATER THAN ZERO                     
03844        OR WS-EXTRACT-DB-COUNT GREATER THAN ZERO                   
03845        OR WS-EXTRACT-DC-COUNT GREATER THAN ZERO                   
03846        OR WS-EXTRACT-DD-COUNT GREATER THAN ZERO                   
03847        OR WS-EXTRACT-DE-COUNT GREATER THAN ZERO                   
03848        OR WS-EXTRACT-DF-COUNT GREATER THAN ZERO                   
03849        OR WS-EXTRACT-DG-COUNT GREATER THAN ZERO                   
03850        OR WS-EXTRACT-DH-COUNT GREATER THAN ZERO                   
03851        OR WS-EXTRACT-DI-COUNT GREATER THAN ZERO                   
03852        OR WS-EXTRACT-DJ-COUNT GREATER THAN ZERO                   
03853        OR WS-EXTRACT-DK-COUNT GREATER THAN ZERO                   
03854        OR WS-EXTRACT-DL-COUNT GREATER THAN ZERO                   
03855          NEXT SENTENCE                                            
03856        ELSE                                                       
03857          GO TO 1920-SIP.                                          
03858                                                                   
03859      MOVE SPACES                 TO  REPORTS-EXTRACT-RECORD.      
03860                                                                   
03861      MOVE 'EX'                   TO  EX-RECORD-ID.                
03862      MOVE '2'                    TO  EX-POSITIONING-CODE.         
03863      MOVE 'D'                    TO  EX-EXTRACT-CODE.             
03864      MOVE WS-COMPANY-CODE        TO  EX-COMPANY-CD.               
03865      MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID.               
03866                                                                   
03867      MOVE WS-COMPANY-NAME        TO  EX-DA-COMPANY-NAME.          
03868      MOVE WS-PRINT-OPTION        TO  EX-DA-PRINT-OPTION.          
03869      MOVE WS-FORMAT-OPTION       TO  EX-DA-FORMAT-OPTION.         
03870      MOVE WS-PROCESS-OPTION      TO  EX-DA-PROCESS-OPTION.        
03871      MOVE WS-TOTAL-OPTION        TO  EX-DA-TOTAL-OPTION.          
03872      MOVE WS-PAID-THRU-TO        TO  EX-DA-PAID-THRU-TO.          
03873                                                                   
03874      MOVE WS-H3-DATE             TO  EX-DA-ALPHA-DATE.            
03875                                                                   
03876      PERFORM 7000-RELEASE-RECORD.                                 
03877                                  EJECT                            
03878  1920-SIP.                                                        
03879                                                                   
121902*    IF WS-COMPANY-ID = 'AIG' OR 'AUK'                            
121902*        NEXT SENTENCE                                            
121902*    ELSE                                                         
03883         IF CHECK-NUMBERING-MANUAL   OR                            
03884            CHECK-NUMBERING-AT-PRINT                               
03885             GO TO 1925-SIP.                                       
03886                                                                   
03887      IF WS-CHECK-COUNTER = WS-BEGIN-CHECK-COUNTER                 
03888          GO TO 1925-SIP.                                          
03889                                                                   
03890      MOVE SPACES                 TO  CF-ACCESS-CD-GENL.           
03891      MOVE '1'                    TO  CF-RECORD-TYPE.              
03892      MOVE ZERO                   TO  CF-SEQUENCE-NO.              
03893                                                                   
121902     IF WS-CARRIER-CONTROL = SPACES                               
121902*       OR WS-COMPANY-ID = 'FIA'                                     
03896          MOVE '6'                TO  CF-RECORD-TYPE               
03897          MOVE WS-LAST-CARRIER    TO  CF-CARRIER-CNTL.             
03898                                                                   
03899      PERFORM 7300-READ-CONTROL-FILE.                              
03900                                                                   
03901      IF ELCNTL-FILE-STATUS NOT = ZERO                             
03902          MOVE 'ERROR OCCURRED READ FOR UPDATE CARRIER'            
03903                                  TO  WS-ABEND-MESSAGE             
03904          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
03905          GO TO ABEND-PGM.                                         
03906                                                                   
121902*    IF WS-COMPANY-ID = 'FIA'                                     
121902*        MOVE WS-CHECK-COUNTER       TO  CF-CHECK-COUNTER         
121902*    ELSE                                                         
03910          IF WS-CARRIER-CONTROL NOT = SPACES                       
03911              MOVE WS-CHECK-COUNTER   TO  CF-CO-CHECK-COUNTER      
03912          ELSE                                                     
03913              MOVE WS-CHECK-COUNTER   TO  CF-CHECK-COUNTER.        
03914                                                                   
03915      PERFORM 7900-REWRITE-CONTROL-FILE.                           
03916                                                                   
03917  EJECT                                                            
03918                                                                   
03919  1925-SIP.                                                        
03920                                                                   
03921 *     UPDATE CONTROL FILE WITH NEW STARTING ARCHIVE NUMBER AND/OR 
03922 *     CURRENT PROCESS DATE                                        
03923                                                                   
03924      MOVE SPACES                 TO  CF-ACCESS-CD-GENL.           
03925      MOVE '1'                    TO  CF-RECORD-TYPE.              
03926      MOVE ZERO                   TO  CF-SEQUENCE-NO.              
03927      PERFORM 7300-READ-CONTROL-FILE.                              
03928                                                                   
03929      IF ELCNTL-FILE-STATUS NOT = ZERO                             
101303         DISPLAY 'CF CONTROL PRIMARY  ' CF-CONTROL-PRIMARY 
03930          MOVE 'ERROR OCCURRED READ FOR UPDATE ARCH NO'            
03931                                  TO  WS-ABEND-MESSAGE             
03932          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
03933          GO TO ABEND-PGM.                                         
03934                                                                   
03935      MOVE WS-ACTUAL-DATE         TO  CF-CLAIMS-LAST-PROCESS-DT.   
03936                                                                   
03937      MOVE SPACES                 TO  WS-DETAIL1.                  
03938      MOVE 'CURRENT PROCESS DATE UPDATED'                          
03939                                  TO  WS-D1-MESSAGE.               
03940      MOVE WS-DETAIL1             TO  PRT.                         
03941      PERFORM WRITE-A-LINE.                                        
03942      MOVE SPACES                 TO  WS-DETAIL1.                  

102516     if ws-company-id <> 'AHL'
102516        go to  1925-continue-sip
102516     end-if
102516
102516     display ' About to check claim cut-off date ' ws-company-id
102516
102516     display ' about to connect to Logic '

      ****  The below code is for when the db has been
      ****  converted to sql server 2016
           evaluate ws-kix-myenv
              when 'cid1p'
                 move '//sdv-db01.cso.local:1433;'
                                       to p-sql-server
      *       when 'mdoff'
      *          move '//hov-tstdb01.cso.local:55330;'
      *                                to p-sql-server
              when other
                 move '//hov-tstdb01.cso.local:1433;'
                                       to p-sql-server
           end-evaluate

           move 'Logic'                to p-sql-database

           CALL 'SQLCONNECT' USING sqlconnect-parms
           display ' ret code ' p-connect-return-code
           move p-connect-return-code  to sqlcode
           move p-sql-return-message   to sqlerrmc

102516
102516     if sqlcode not = 0
102516        display "Error: cannot connect to Logic"
102516        move sqlcode             to ws-sql-code
102516        move ws-sql-code         to ws-dis-sql-code
102516        display ' sqlcode ' ws-dis-sql-code
102516        display sqlerrmc
102516        go to 1925-continue-sip
102516     end-if
102516
102516     exec sql
102516        select
102516           convert(char,pddate,112)
102516        into
102516           :PD-AHL-EOM-DT
102516        from
102516           ParmDates
102516        where
102516           pdtype = 'AHL MOE'
102516     end-exec
102516
102516     if sqlcode not = 0 and 1
102516        display "Error: cannot get ahl eom date "
102516        move sqlcode             to ws-sql-code
102516        move ws-sql-code         to ws-dis-sql-code
102516        display ' sqlcode ' ws-dis-sql-code
102516        display ' sql err mess    ' sqlerrmc
102516        go to 1925-disconnect
102516     end-if
102516
102516     display ' parmdates ahl eom dt ' pd-ahl-eom-dt
102516     move pd-ahl-eom-dt          to DC-GREG-DATE-CYMD-R
102516     move 'L'                    to dc-option-code
102516     PERFORM 8500-DATE-CONVERsion
102516     if no-conversion-error
102516        move dc-bin-date-1       to ws-bin-ahl-eom-dt
102516     else
102516        display ' found invalid ahl eom dt ' 
102516        go to 1925-disconnect
102516     end-if
102516
102516     if cf-claim-cutoff-date = low-values or spaces
102516        display ' cut off = low values '
102516        if ws-bin-ahl-eom-dt > bin-run-date
102516           move ws-bin-ahl-eom-dt
102516                                 to cf-claim-cutoff-date
102516           move spaces           to ws-detail1
102516           move 'About to update claim cut-off date '
102516                                 to ws-d1-message
102516           string
102516              pd-ahl-eom-dt (5:2) '/'
102516              pd-ahl-eom-dt (7:2) '/'
102516              pd-ahl-eom-dt (3:2)
102516              delimited by size into ws-d1-birth-date
102516           end-string
102516           move ws-detail1       to prt
102516           perform write-a-line
102516           display ' About to update cut-off dt  '
102516        else
102516           display ' must not have changed yet '
102516        end-if
102516     end-if
102516
102516     .
102516 1925-disconnect.
102516
102516     EXEC SQL
102516        DISCONNECT
102516     END-EXEC
102516
102516     if sqlcode not = 0
102516        display "Error: disconnect  "
102516        display ' sql return code ' sqlcode
102516        display ' sql err mess    ' sqlerrmc
102516     end-if
102516
102516     .
102516 1925-continue-sip.

03944      IF ((WS-ORIGINAL-START-ARCH-NO = WS-NEW-START-ARCH-NO) OR    
03945          (WS-NEW-START-ARCH-NO = +9999999))                       
03946                       AND                                         
03947         (WS-ORIG-ARCH-COUNTER = WS-ARCHIVE-COUNTER)               
03948            PERFORM 7900-REWRITE-CONTROL-FILE                      
03949            GO TO 1930-SIP.                                        
03950                                                                   
03951      IF WS-ORIGINAL-START-ARCH-NO = WS-NEW-START-ARCH-NO  OR      
03952         WS-NEW-START-ARCH-NO = +9999999                           
03953           NEXT SENTENCE                                           
03954        ELSE                                                       
03955           MOVE WS-NEW-START-ARCH-NO    TO CF-STARTING-ARCH-NO     
03956           MOVE 'STARTING ARCHIVE NUMBER UPDATED'                  
03957                                        TO  WS-D1-MESSAGE          
03958           MOVE WS-NEW-START-ARCH-NO    TO  WS-D1-SEQ              
03959           MOVE WS-ORIGINAL-START-ARCH-NO TO  WS-D1-QUE            
03960           MOVE WS-DETAIL1              TO  PRT                    
03961           PERFORM WRITE-A-LINE                                    
03962           MOVE SPACES                  TO  WS-DETAIL1.            
03963                                                                   
03964      IF WS-ARCHIVE-COUNTER NOT = CF-CO-ARCHIVE-COUNTER            
03965         MOVE WS-ARCHIVE-COUNTER      TO CF-CO-ARCHIVE-COUNTER.    
03966                                                                   
03967      PERFORM 7900-REWRITE-CONTROL-FILE.                           
03968                                  EJECT                            
03969  1930-SIP.                                                        
03970 *    NOTE ******************************************************* 
03971 *         *                                                     * 
03972 *         *      PROCESS THE INFORMATION FROM THE CHECK QUEUE   * 
03973 *         *                                                     * 
03974 *         *******************************************************.
03975                                                                   
03976      IF WS-FREQUENCY = 'NONE'                                     
03977          PERFORM 2700-PROCESS-CHECK-QUEUE                         
03978          IF THIS-IS-MONTH-END                                     
03979              MOVE SPACES         TO  CF-CONTROL-PRIMARY           
03980              MOVE WS-COMPANY-ID  TO  CF-COMPANY-ID                
03981              MOVE '1'            TO  CF-RECORD-TYPE               
03982              MOVE ZERO           TO  CF-SEQUENCE-NO               
03983              PERFORM 7300-READ-CONTROL-FILE                       
03984              IF ELCNTL-FILE-STATUS NOT = ZERO                     
03985                  MOVE 'COMPANY RECORD NOT FOUND - ELCNTL'         
03986                                  TO  WS-ABEND-MESSAGE             
03987                  MOVE ELCNTL-FILE-STATUS  TO  WS-ABEND-FILE-STATUS
03988                  GO TO ABEND-PGM                                  
03989                ELSE                                               
03990                  MOVE CF-CURRENT-MONTH-END  TO  DC-BIN-DATE-1     
03991                  MOVE +1             TO  DC-ELAPSED-MONTHS        
03992                  MOVE '1'            TO  DC-END-OF-MONTH          
03993                  MOVE ZERO           TO  DC-ELAPSED-DAYS          
03994                  MOVE '6'            TO  DC-OPTION-CODE           
03995                  PERFORM 8500-DATE-CONVERSION                     
03996                  MOVE DC-BIN-DATE-2  TO  CF-CURRENT-MONTH-END     
CIDMOD                 MOVE DC-BIN-DATE-2  TO  CF-CR-MONTH-END-DT       
03997                  MOVE LOW-VALUES     TO CF-CLAIM-CUTOFF-DATE      
03998 **               MOVE '0MONTH END DATE UPDATED - '                
CIDMOD                 MOVE '0CLAIM MONTH END DATE UPDATED - '          
03999                                  TO  WS-DETAIL2                   
04000                  MOVE WS-DETAIL2             TO  PRT              
04001                  PERFORM WRITE-A-LINE                             
CIDMOD                 MOVE '0CREDIT MONTH END DATE UPDATED - '         
CIDMOD                                 TO  WS-DETAIL2                   
CIDMOD                 MOVE WS-DETAIL2             TO  PRT              
CIDMOD                 PERFORM WRITE-A-LINE                             
04002                  PERFORM 7900-REWRITE-CONTROL-FILE.               
04003                                                                   
04004      MOVE +99                    TO  WS-LINE-COUNT.               
04005      MOVE ' *** RECORDS PROCESSED ***'  TO  WS-DETAIL2.           
04006      MOVE WS-DETAIL2             TO  PRT.                         
04007      PERFORM WRITE-A-LINE.                                        
04008                                                                   
04009      MOVE '0CLAIMS PROCESSED   -'  TO  WS-DETAIL2.                
04010      MOVE WS-CLAIM-COUNT         TO  WS-D2-COUNT.                 
04011      MOVE WS-DETAIL1             TO  PRT.                         
04012      PERFORM WRITE-A-LINE.                                        
04013                                                                   
04014      MOVE ' TRAILERS PROCESSED -'  TO  WS-DETAIL2.                
04015      MOVE WS-TRAILER-COUNT       TO  WS-D2-COUNT.                 
04016      MOVE WS-DETAIL1             TO  PRT.                         
04017      PERFORM WRITE-A-LINE.                                        
04018                                                                   
04019      MOVE '0CLAIM RECORDS DELETED'  TO  WS-DETAIL2.               
04020      MOVE WS-CLAIMS-DELETED      TO  WS-D2-COUNT.                 
04021      MOVE WS-DETAIL1             TO  PRT.                         
04022      PERFORM WRITE-A-LINE.                                        
04023                                                                   
04024      MOVE '0TOTAL PURGED CLAIMS' TO  WS-DETAIL2.                  
04025      MOVE WS-PURGED-COUNT        TO  WS-D2-COUNT.                 
04026      MOVE WS-DETAIL1             TO  PRT.                         
04027      PERFORM WRITE-A-LINE.                                        
04028                                                                   
04029      MOVE '0CLAIM RECORDS PURGED' TO  WS-DETAIL2.                 
04030      MOVE WS-CLAIMS-PURGED       TO  WS-D2-COUNT.                 
04031      MOVE WS-DETAIL1             TO  PRT.                         
04032      PERFORM WRITE-A-LINE.                                        
04033                                                                   
04034      MOVE ' TRAILER RECORDS DELETED'  TO  WS-DETAIL2.             
04035      MOVE WS-TRAILERS-DELETED    TO  WS-D2-COUNT.                 
04036      MOVE WS-DETAIL1             TO  PRT.                         
04037      PERFORM WRITE-A-LINE.                                        
04038                                                                   
04039      MOVE ' CERTIFICATES ADDED'  TO  WS-DETAIL2.                  
04040      MOVE WS-CERTS-ADDED         TO  WS-D2-COUNT.                 
04041      MOVE WS-DETAIL1             TO  PRT.                         
04042      PERFORM WRITE-A-LINE.                                        
04043                                                                   
04044      MOVE ' CHECK RECORDS DELETED  '  TO  WS-DETAIL2.             
04045      MOVE WS-CHECK-QUEUE-DELETED TO  WS-D2-COUNT.                 
04046      MOVE WS-DETAIL1             TO  PRT.                         
04047      PERFORM WRITE-A-LINE.                                        
04048                                                                   
04049      MOVE ' LETTER RECORDS DELETED '  TO  WS-DETAIL2.             
04050      MOVE WS-LETTER-ARCHIVES-DELETED  TO  WS-D2-COUNT.            
04051      MOVE WS-DETAIL1             TO  PRT.                         
04052      PERFORM WRITE-A-LINE.                                        
04053                                                                   
04054      MOVE '-*** RECORDS EXTRACTED ***'  TO  WS-DETAIL2.           
04055      MOVE WS-DETAIL2             TO  PRT.                         
04056      PERFORM WRITE-A-LINE.                                        
04057                                                                   
04058      MOVE '0EXTRACT A RECORD A -'  TO  WS-DETAIL2.                
04059      MOVE WS-EXTRACT-AA-COUNT    TO  WS-D2-COUNT.                 
04060      MOVE WS-DETAIL1             TO  PRT.                         
04061      PERFORM WRITE-A-LINE.                                        
04062                                                                   
04063      MOVE ' EXTRACT A RECORD B -'  TO  WS-DETAIL2.                
04064      MOVE WS-EXTRACT-AB-COUNT    TO  WS-D2-COUNT.                 
04065      MOVE WS-DETAIL2             TO  PRT.                         
04066      PERFORM WRITE-A-LINE.                                        
04067                                                                   
04068      MOVE '0EXTRACT B RECORD A -'  TO  WS-DETAIL2.                
04069      MOVE WS-EXTRACT-BA-COUNT    TO  WS-D2-COUNT.                 
04070      MOVE WS-DETAIL2             TO  PRT.                         
04071      PERFORM WRITE-A-LINE.                                        
04072                                                                   
04073      MOVE '0EXTRACT C RECORD A -'  TO  WS-DETAIL2.                
04074      MOVE WS-EXTRACT-CA-COUNT    TO  WS-D2-COUNT.                 
04075      MOVE WS-DETAIL2             TO  PRT.                         
04076      PERFORM WRITE-A-LINE.                                        
04077                                                                   
04078      MOVE '0EXTRACT D RECORD A -'  TO  WS-DETAIL2.                
04079      MOVE WS-EXTRACT-DA-COUNT    TO  WS-D2-COUNT.                 
04080      MOVE WS-DETAIL2             TO  PRT.                         
04081      PERFORM WRITE-A-LINE.                                        
04082                                                                   
04083      MOVE ' EXTRACT D RECORD B -'  TO  WS-DETAIL2.                
04084      MOVE WS-EXTRACT-DB-COUNT    TO  WS-D2-COUNT.                 
04085      MOVE WS-DETAIL2             TO  PRT.                         
04086      PERFORM WRITE-A-LINE.                                        
04087                                                                   
04088      MOVE ' EXTRACT D RECORD C -'  TO  WS-DETAIL2.                
04089      MOVE WS-EXTRACT-DC-COUNT    TO  WS-D2-COUNT.                 
04090      MOVE WS-DETAIL2             TO  PRT.                         
04091      PERFORM WRITE-A-LINE.                                        
04092                                                                   
04093      MOVE ' EXTRACT D RECORD D -'  TO  WS-DETAIL2.                
04094      MOVE WS-EXTRACT-DD-COUNT    TO  WS-D2-COUNT.                 
04095      MOVE WS-DETAIL2             TO  PRT.                         
04096      PERFORM WRITE-A-LINE.                                        
04097                                                                   
04098      MOVE ' EXTRACT D RECORD E -'  TO  WS-DETAIL2.                
04099      MOVE WS-EXTRACT-DE-COUNT    TO  WS-D2-COUNT.                 
04100      MOVE WS-DETAIL2             TO  PRT.                         
04101      PERFORM WRITE-A-LINE.                                        
04102                                                                   
04103      MOVE ' EXTRACT D RECORD F -'  TO  WS-DETAIL2.                
04104      MOVE WS-EXTRACT-DF-COUNT    TO  WS-D2-COUNT.                 
04105      MOVE WS-DETAIL2             TO  PRT.                         
04106      PERFORM WRITE-A-LINE.                                        
04107                                                                   
04108      MOVE ' EXTRACT D RECORD G -'  TO  WS-DETAIL2.                
04109      MOVE WS-EXTRACT-DG-COUNT    TO  WS-D2-COUNT.                 
04110      MOVE WS-DETAIL2             TO  PRT.                         
04111      PERFORM WRITE-A-LINE.                                        
04112                                                                   
04113      MOVE ' EXTRACT D RECORD H -'  TO  WS-DETAIL2.                
04114      MOVE WS-EXTRACT-DH-COUNT    TO  WS-D2-COUNT.                 
04115      MOVE WS-DETAIL2             TO  PRT.                         
04116      PERFORM WRITE-A-LINE.                                        
04117                                                                   
04118      MOVE ' EXTRACT D RECORD I -'  TO  WS-DETAIL2.                
04119      MOVE WS-EXTRACT-DI-COUNT    TO  WS-D2-COUNT.                 
04120      MOVE WS-DETAIL2             TO  PRT.                         
04121      PERFORM WRITE-A-LINE.                                        
04122                                                                   
04123      MOVE ' EXTRACT D RECORD J -'  TO  WS-DETAIL2.                
04124      MOVE WS-EXTRACT-DJ-COUNT    TO  WS-D2-COUNT.                 
04125      MOVE WS-DETAIL2             TO  PRT.                         
04126      PERFORM WRITE-A-LINE.                                        
04127                                                                   
04128      MOVE ' EXTRACT D RECORD K -'  TO  WS-DETAIL2.                
04129      MOVE WS-EXTRACT-DK-COUNT    TO  WS-D2-COUNT.                 
04130      MOVE WS-DETAIL2             TO  PRT.                         
04131      PERFORM WRITE-A-LINE.                                        
04132                                                                   
04133      MOVE ' EXTRACT D RECORD L -'  TO  WS-DETAIL2.                
04134      MOVE WS-EXTRACT-DL-COUNT    TO  WS-D2-COUNT.                 
04135      MOVE WS-DETAIL2             TO  PRT.                         
04136      PERFORM WRITE-A-LINE.                                        
04137                                                                   
04138      MOVE '0EXTRACT E RECORD A -'  TO  WS-DETAIL2.                
04139      MOVE WS-EXTRACT-EA-COUNT    TO  WS-D2-COUNT.                 
04140      MOVE WS-DETAIL2             TO  PRT.                         
04141      PERFORM WRITE-A-LINE.                                        
04142                                                                   
CIDMOD                                                                  
CIDMOD     DISPLAY 'DAILY ACTIVITY RECORDS WRITTEN      - '             
CIDMOD                                 DLYACTV-OUT.                     
CIDMOD     DISPLAY 'DAILY ACTIVITY RECORDS DUPRECS      - '             
CIDMOD                                 DLYACTV-DUP.                     
CIDMOD     MOVE ZEROS                  TO DLYACTV-OUT                   
CIDMOD                                    DLYACTV-DUP.                  
121902*    IF WS-COMPANY-ID = 'AUK' OR 'AUG'                            
121902*        MOVE ' EXTRACT G RECORD A -'                             
121902*                                  TO  WS-DETAIL2                 
121902*        MOVE WS-EXTRACT-GA-COUNT  TO  WS-D2-COUNT                
121902*        MOVE WS-DETAIL2           TO  PRT                        
121902*        PERFORM WRITE-A-LINE.                                    
04149                                                                   
04150      ADD WS-CLAIM-COUNT       TO  WS-TOTAL-CLAIM-COUNT.           
04151      ADD WS-TRAILER-COUNT     TO  WS-TOTAL-TRAILER-COUNT.         
04152      ADD WS-EXTRACT-AA-COUNT  TO  WS-TOTAL-AA-COUNT.              
04153      ADD WS-EXTRACT-AB-COUNT  TO  WS-TOTAL-AB-COUNT.              
04154      ADD WS-EXTRACT-BA-COUNT  TO  WS-TOTAL-BA-COUNT.              
04155      ADD WS-EXTRACT-CA-COUNT  TO  WS-TOTAL-CA-COUNT.              
04156      ADD WS-EXTRACT-DA-COUNT  TO  WS-TOTAL-DA-COUNT.              
04157      ADD WS-EXTRACT-DB-COUNT  TO  WS-TOTAL-DB-COUNT.              
04158      ADD WS-EXTRACT-DC-COUNT  TO  WS-TOTAL-DC-COUNT.              
04159      ADD WS-EXTRACT-DD-COUNT  TO  WS-TOTAL-DD-COUNT.              
04160      ADD WS-EXTRACT-DE-COUNT  TO  WS-TOTAL-DE-COUNT.              
04161      ADD WS-EXTRACT-DF-COUNT  TO  WS-TOTAL-DF-COUNT.              
04162      ADD WS-EXTRACT-DG-COUNT  TO  WS-TOTAL-DG-COUNT.              
04163      ADD WS-EXTRACT-DH-COUNT  TO  WS-TOTAL-DH-COUNT.              
04164      ADD WS-EXTRACT-DI-COUNT  TO  WS-TOTAL-DI-COUNT.              
04165      ADD WS-EXTRACT-DJ-COUNT  TO  WS-TOTAL-DJ-COUNT.              
04166      ADD WS-EXTRACT-DK-COUNT  TO  WS-TOTAL-DK-COUNT.              
04167      ADD WS-EXTRACT-DL-COUNT  TO  WS-TOTAL-DL-COUNT.              
04168      ADD WS-EXTRACT-EA-COUNT  TO  WS-TOTAL-EA-COUNT.              
04169      ADD WS-EXTRACT-GA-COUNT  TO  WS-TOTAL-GA-COUNT.              
04170      ADD WS-PURGED-COUNT      TO  WS-TOTAL-PURGED-COUNT.          
04171      ADD WS-CLAIMS-DELETED    TO  WS-TOTAL-CLAIMS-DELETED.        
04172                                                                   
04173      MOVE ZERO                   TO  WS-CLAIM-COUNT               
04174                                      WS-TRAILER-COUNT             
04175                                      WS-CHECK-QUEUE-DELETED       
04176                                      WS-CLAIMS-DELETED            
04177                                      WS-TRAILERS-DELETED          
04178                                      WS-CLAIMS-PURGED             
04179                                      WS-LETTER-ARCHIVES-DELETED   
04180                                      WS-EXTRACT-AA-COUNT          
04181                                      WS-EXTRACT-AB-COUNT          
04182                                      WS-EXTRACT-BA-COUNT          
04183                                      WS-EXTRACT-CA-COUNT          
04184                                      WS-EXTRACT-DA-COUNT          
04185                                      WS-EXTRACT-DB-COUNT          
04186                                      WS-EXTRACT-DC-COUNT          
04187                                      WS-EXTRACT-DD-COUNT          
04188                                      WS-EXTRACT-DE-COUNT          
04189                                      WS-EXTRACT-DF-COUNT          
04190                                      WS-EXTRACT-DG-COUNT          
04191                                      WS-EXTRACT-DH-COUNT          
04192                                      WS-EXTRACT-DI-COUNT          
04193                                      WS-EXTRACT-DJ-COUNT          
04194                                      WS-EXTRACT-DK-COUNT          
04195                                      WS-EXTRACT-DL-COUNT          
04196                                      WS-EXTRACT-EA-COUNT          
04197                                      WS-EXTRACT-GA-COUNT          
04198                                      WS-PURGED-COUNT              
04199                                      WS-CERTS-ADDED.              
04200                                                                   
04201      MOVE +99                    TO  WS-LINE-COUNT.               
04202                                                                   
04203      MOVE HIGH-VALUES            TO CF-RECORD-TYPE.               
04204      GO TO 1010-SIP.                                              
04205                                  EJECT                            
04206  1990-SIP.                                                        
04207 *    NOTE ******************************************************* 
04208 *         *                   FINAL TOTALS                      * 
04209 *         *******************************************************.
04210                                                                   
04211      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      
04212                                                                   
04213      MOVE WS-TIME                TO  WS-DISPLAY-TIME.             
04214      INSPECT WS-DISPLAY-TIME CONVERTING SPACES TO '.'             
04215                                                                   
04216      DISPLAY 'END OF PROCESSING FOR EL310'                        
04217              WS-DISPLAY-TIME UPON CONSOLE.                        
04218      DISPLAY 'END OF PROCESSING FOR EL310'                        
04219              WS-DISPLAY-TIME.                                     
04220                                                                   
04221      MOVE +99                      TO  WS-LINE-COUNT.             
04222      MOVE SPACES                   TO  WS-H2-COMPANY-ID.          
04223      MOVE '*** FINAL TOTALS ***'   TO  WS-H2-COMPANY-NAME.        
04224                                                                   
04225      MOVE '0CLAIMS PROCESSED   -'  TO  WS-DETAIL2.                
04226      MOVE WS-TOTAL-CLAIM-COUNT     TO  WS-D2-COUNT.               
04227      MOVE WS-DETAIL2               TO  PRT.                       
04228      PERFORM WRITE-A-LINE.                                        
04229                                                                   
04230      MOVE ' TRAILERS PROCESSED -'  TO  WS-DETAIL2.                
04231      MOVE WS-TOTAL-TRAILER-COUNT   TO  WS-D2-COUNT.               
04232      MOVE WS-DETAIL2               TO  PRT.                       
04233      PERFORM WRITE-A-LINE.                                        
04234                                                                   
04235      MOVE ' TOTAL CLAIMS DELETED'  TO  WS-DETAIL2.                
04236      MOVE WS-TOTAL-CLAIMS-DELETED  TO  WS-D2-COUNT.               
04237      MOVE WS-DETAIL2               TO  PRT.                       
04238      PERFORM WRITE-A-LINE.                                        
04239                                                                   
04240      MOVE ' TOTAL PURGED CLAIMS ' TO  WS-DETAIL2.                 
04241      MOVE WS-TOTAL-PURGED-COUNT   TO  WS-D2-COUNT.                
04242      MOVE WS-DETAIL2              TO  PRT.                        
04243      PERFORM WRITE-A-LINE.                                        
04244                                                                   
04245      MOVE '0EXTRACT A RECORD A -'  TO  WS-DETAIL2.                
04246      MOVE WS-TOTAL-AA-COUNT        TO  WS-D2-COUNT                
04247      MOVE WS-DETAIL2               TO  PRT.                       
04248      PERFORM WRITE-A-LINE.                                        
04249                                                                   
04250      MOVE ' EXTRACT A RECORD B -'  TO  WS-DETAIL2.                
04251      MOVE WS-TOTAL-AB-COUNT        TO  WS-D2-COUNT                
04252      MOVE WS-DETAIL2               TO  PRT.                       
04253      PERFORM WRITE-A-LINE.                                        
04254                                                                   
04255      MOVE '0EXTRACT B RECORD A -'  TO  WS-DETAIL2.                
04256      MOVE WS-TOTAL-BA-COUNT        TO  WS-D2-COUNT                
04257      MOVE WS-DETAIL2               TO  PRT.                       
04258      PERFORM WRITE-A-LINE.                                        
04259                                                                   
04260      MOVE '0EXTRACT C RECORD A -'  TO  WS-DETAIL2.                
04261      MOVE WS-TOTAL-CA-COUNT        TO  WS-D2-COUNT.               
04262      MOVE WS-DETAIL2               TO  PRT.                       
04263      PERFORM WRITE-A-LINE.                                        
04264                                                                   
04265      MOVE '0EXTRACT D RECORD A -'  TO  WS-DETAIL2.                
04266      MOVE WS-TOTAL-DA-COUNT        TO  WS-D2-COUNT.               
04267      MOVE WS-DETAIL2               TO  PRT.                       
04268      PERFORM WRITE-A-LINE.                                        
04269                                                                   
04270      MOVE ' EXTRACT D RECORD B -'  TO  WS-DETAIL2.                
04271      MOVE WS-TOTAL-DB-COUNT        TO  WS-D2-COUNT.               
04272      MOVE WS-DETAIL2               TO  PRT.                       
04273      PERFORM WRITE-A-LINE.                                        
04274                                                                   
04275      MOVE ' EXTRACT D RECORD C -'  TO  WS-DETAIL2.                
04276      MOVE WS-TOTAL-DC-COUNT        TO  WS-D2-COUNT.               
04277      MOVE WS-DETAIL2               TO  PRT.                       
04278      PERFORM WRITE-A-LINE.                                        
04279                                                                   
04280      MOVE ' EXTRACT D RECORD D -'  TO  WS-DETAIL2.                
04281      MOVE WS-TOTAL-DD-COUNT        TO  WS-D2-COUNT.               
04282      MOVE WS-DETAIL2               TO  PRT.                       
04283      PERFORM WRITE-A-LINE.                                        
04284                                                                   
04285      MOVE ' EXTRACT D RECORD E -'  TO  WS-DETAIL2.                
04286      MOVE WS-TOTAL-DE-COUNT        TO  WS-D2-COUNT.               
04287      MOVE WS-DETAIL2               TO  PRT.                       
04288      PERFORM WRITE-A-LINE.                                        
04289                                                                   
04290      MOVE ' EXTRACT D RECORD F -'  TO  WS-DETAIL2.                
04291      MOVE WS-TOTAL-DF-COUNT        TO  WS-D2-COUNT.               
04292      MOVE WS-DETAIL2               TO  PRT.                       
04293      PERFORM WRITE-A-LINE.                                        
04294                                                                   
04295      MOVE ' EXTRACT D RECORD G -'  TO  WS-DETAIL2.                
04296      MOVE WS-TOTAL-DG-COUNT        TO  WS-D2-COUNT.               
04297      MOVE WS-DETAIL2               TO  PRT.                       
04298      PERFORM WRITE-A-LINE.                                        
04299                                                                   
04300      MOVE ' EXTRACT D RECORD H -'  TO  WS-DETAIL2.                
04301      MOVE WS-TOTAL-DH-COUNT        TO  WS-D2-COUNT.               
04302      MOVE WS-DETAIL2               TO  PRT.                       
04303      PERFORM WRITE-A-LINE.                                        
04304                                                                   
04305      MOVE ' EXTRACT D RECORD I -'  TO  WS-DETAIL2.                
04306      MOVE WS-TOTAL-DI-COUNT        TO  WS-D2-COUNT.               
04307      MOVE WS-DETAIL2               TO  PRT.                       
04308      PERFORM WRITE-A-LINE.                                        
04309                                                                   
04310      MOVE ' EXTRACT D RECORD J -'  TO  WS-DETAIL2.                
04311      MOVE WS-TOTAL-DJ-COUNT        TO  WS-D2-COUNT.               
04312      MOVE WS-DETAIL2               TO  PRT.                       
04313      PERFORM WRITE-A-LINE.                                        
04314                                                                   
04315      MOVE ' EXTRACT D RECORD K -'  TO  WS-DETAIL2.                
04316      MOVE WS-TOTAL-DK-COUNT        TO  WS-D2-COUNT.               
04317      MOVE WS-DETAIL2               TO  PRT.                       
04318      PERFORM WRITE-A-LINE.                                        
04319                                                                   
04320      MOVE ' EXTRACT D RECORD L -'  TO  WS-DETAIL2.                
04321      MOVE WS-TOTAL-DL-COUNT        TO  WS-D2-COUNT.               
04322      MOVE WS-DETAIL2               TO  PRT.                       
04323      PERFORM WRITE-A-LINE.                                        
04324                                                                   
04325      MOVE '0EXTRACT E RECORD A -'  TO  WS-DETAIL2.                
04326      MOVE WS-TOTAL-EA-COUNT        TO  WS-D2-COUNT.               
04327      MOVE WS-DETAIL2               TO  PRT.                       
04328      PERFORM WRITE-A-LINE.                                        
04329                                                                   
121902*    IF WS-COMPANY-ID = 'AUK' OR 'AUG'                            
121902*        MOVE ' EXTRACT G RECORD A -'                             
121902*                                  TO  WS-DETAIL2                 
121902*        MOVE WS-TOTAL-GA-COUNT    TO  WS-D2-COUNT                
121902*        MOVE WS-DETAIL2           TO  PRT                        
121902*        PERFORM WRITE-A-LINE.                                    
04336                                                                   
04337  1999-EXIT.                                                       
04338      EXIT.                                                        
04339                                  EJECT                            
04340  2000-CLOSE-CLAIM SECTION.                                        

110916*032612IF WS-COMPANY-ID = 'AHL'
110916*032612   GO TO 2090-EXIT
110916*032612END-IF

04342      MOVE 'C'                    TO  CL-CLAIM-STATUS.             
04343                                                                   
04344      IF CL-AUTO-PAY-SEQ NOT EQUAL ZEROS                           
04345          MOVE ZERO               TO CL-AUTO-PAY-SEQ               
04346          MOVE LOW-VALUES         TO CL-NEXT-AUTO-PAY-DT.          
04347                                                                   
04348      MOVE '3'                    TO  CL-LAST-CLOSE-REASON         
04349                                      CL-LAST-MAINT-TYPE.          
04350      MOVE BIN-RUN-DATE           TO  CL-LAST-CLOSE-DT             
04351                                      CL-LAST-MAINT-DT.            
04352                                                                   
04353      MOVE WS-CURRENT-TIME        TO  CL-LAST-MAINT-HHMMSS.        
04354      MOVE 'AUTO'                 TO  CL-LAST-MAINT-USER.          
04355                                                                   
121902*    IF WS-COMPANY-ID = 'DMD'                                     
121902*        MOVE 'Q'                TO  WS-DMD-DMO-ACTIVITY          
121902*        PERFORM 9501-CREATE-DMO  THRU  9501-EXIT.                
04359                                                                   
04360      MOVE CL-CONTROL-PRIMARY     TO  AT-CONTROL-PRIMARY.          
04361      MOVE ZERO                   TO  AT-SEQUENCE-NO.              
04362                                                                   
04363      PERFORM 8300-READ-TRAILER.                                   
04364                                                                   
04365      IF ELTRLR-FILE-STATUS = '23'                                 
04366          PERFORM 9200-BUILD-ZERO-TRAILER                          
04367          PERFORM 8700-WRITE-TRAILER                               
04368          GO TO 2090-EXIT.                                         
04369                                                                   
04370      IF ELTRLR-FILE-STATUS NOT = ZERO                             
04371          MOVE 'ERROR OCCURRED READ ZERO TRAILER RECORD - ELTRLR'  
04372                                  TO  WS-ABEND-MESSAGE             
04373          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
04374          GO TO ABEND-PGM.                                         
04375                                                                   
04376      MOVE +1                     TO  WS-INDEX.                    
04377                                                                   
04378  2020-CLOSE-CLAIM.                                                
04379                                                                   
04380      IF AT-OPEN-CLOSE-TYPE (WS-INDEX) = SPACES                    
04381          GO TO 2030-CLOSE-CLAIM.                                  
04382                                                                   
04383      IF WS-INDEX LESS THAN +6                                     
04384          ADD +1                  TO  WS-INDEX                     
04385          GO TO 2020-CLOSE-CLAIM.                                  
04386                                                                   
04387      MOVE AT-OPEN-CLOSE-HISTORY (2) TO AT-OPEN-CLOSE-HISTORY (1). 
04388      MOVE AT-OPEN-CLOSE-HISTORY (3) TO AT-OPEN-CLOSE-HISTORY (2). 
04389      MOVE AT-OPEN-CLOSE-HISTORY (4) TO AT-OPEN-CLOSE-HISTORY (3). 
04390      MOVE AT-OPEN-CLOSE-HISTORY (5) TO AT-OPEN-CLOSE-HISTORY (4). 
04391      MOVE AT-OPEN-CLOSE-HISTORY (6) TO AT-OPEN-CLOSE-HISTORY (5). 
04392                                                                   
04393  2030-CLOSE-CLAIM.                                                
04394                                                                   
04395      MOVE BIN-RUN-DATE     TO  AT-OPEN-CLOSE-DATE (WS-INDEX).     
04396      MOVE 'C'              TO  AT-OPEN-CLOSE-TYPE (WS-INDEX).     
04397      MOVE 'AUTO '          TO  AT-OPEN-CLOSE-REASON (WS-INDEX).   
04398      MOVE WS-CURRENT-TIME  TO  AT-LAST-MAINT-HHMMSS.              
04399                                                                   
04400      PERFORM 3600-BUILD-EXTRACT-D-RECORD-D.                       
04401                                                                   
04402      PERFORM 8100-REWRITE-TRAILER.                                
04403                                                                   
04404  2040-CLOSE-CLAIM.                                                
04405                                                                   
020816     IF WS-COMPANY-ID = 'CID' OR 'DCC' or 'VPP'
CIDMOD         MOVE 'C'                   TO WS-CSO-ACTION-TYPE         
CIDMOD         PERFORM 9300-CSO-SPECIAL-PROCESSING THRU 9300-EXIT.      
CIDMOD                                                                  
121902*    IF WS-COMPANY-ID = 'AIG' OR 'AUK' OR 'CIG' OR 'CUK'          
121902*        GO TO 2090-EXIT.                                         
04408                                                                   
04409      IF WS-SYS-ACTIVE-SW (07) = 'N' OR ' '                        
04410          GO TO 2090-EXIT.                                         
04411                                                                   
04412      IF WS-SYS-LETTER-ID (07) = SPACES OR LOW-VALUES              
04413          GO TO 2090-EXIT.                                         
04414                                                                   
04415      PERFORM 2220-GEN-LETTER.                                     
04416                                                                   
04417      MOVE 07                         TO  CL-ACTIVITY-CODE.        
04418      MOVE 'CLOS'                     TO  CL-ACTIVITY-MAINT-TYPE.  
04419      MOVE BIN-RUN-DATE               TO  CL-ACTIVITY-MAINT-DT.    
04420                                                                   
04421  2090-EXIT.                                                       
04422      EXIT.                                                        
04423                                                                   
04424                                  EJECT                            
04425  2200-GET-CERTIFICATE SECTION.                                    
04426                                                                   
04427      MOVE ZERO                   TO  WS-CERT-NOT-FOUND            
04428                                      WS-REWRITE-CERT.             
04429                                                                   
04430      MOVE WS-COMPANY-CODE        TO  CM-COMPANY-CD.               
04431      MOVE CL-CERT-CARRIER        TO  CM-CARRIER.                  
04432      MOVE CL-CERT-GROUPING       TO  CM-GROUPING.                 
04433      MOVE CL-CERT-STATE          TO  CM-STATE.                    
04434      MOVE CL-CERT-ACCOUNT        TO  CM-ACCOUNT.                  
04435      MOVE CL-CERT-EFF-DT         TO  CM-CERT-EFF-DT.              
04436      MOVE CL-CERT-NO             TO  CM-CERT-NO.                  
04437                                                                   
04438      READ ELCERT INTO WS-SAVE-CERTIFICATE-MASTER.                 
04439                                                                   
04440      IF ELCERT-FILE-STATUS = '23'                                 
04441          MOVE +1                 TO  WS-CERT-NOT-FOUND            
04442          GO TO 2209-EXIT.                                         
04443                                                                   
04444      IF ELCERT-FILE-STATUS NOT = ZERO                             
04445          MOVE 'ERROR OCCURRED READ - ELCERT'                      
04446                                  TO  WS-ABEND-MESSAGE             
04447          MOVE ELCERT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
04448          GO TO ABEND-PGM.                                         
04449                                                                   
04450      IF CM-CLAIM-ATTACHED-COUNT NOT GREATER THAN ZERO             
04451          MOVE +1                 TO  CM-CLAIM-ATTACHED-COUNT      
04452                                      WS-REWRITE-CERT              
04453          IF CM-CLAIM-INTERFACE-SW = SPACES                        
04454              MOVE '1'            TO  CM-CLAIM-INTERFACE-SW.       
04455                                                                   
121902*    IF WS-COMPANY-ID = 'FIA'                                     
121902*      AND (CM-CARRIER = '2' OR '4')                              
121902*        IF CM-PREMIUM-TYPE NOT = '3'                             
121902*            MOVE '3'            TO  CM-PREMIUM-TYPE              
121902*            MOVE +1             TO  WS-REWRITE-CERT              
121902*            MOVE 'PREMIUM TYPE FOR OPEN END WRONG'               
121902*                                TO  WS-D1-MESSAGE                
121902*            MOVE WS-DETAIL1     TO  PRT                          
121902*            PERFORM WRITE-A-LINE.                                
04465                                                                   
121902*    IF WS-COMPANY-ID = 'FIA'                                     
121902*      AND (CM-CARRIER = '2' OR '4')                              
121902*        IF CM-AH-BENEFIT-AMT = ZEROS                             
121902*            MOVE 'BENEFIT AMT FOR OPEN END ZEROS'                
121902*                                TO  WS-D1-MESSAGE                
121902*            MOVE WS-DETAIL1     TO  PRT                          
121902*            PERFORM WRITE-A-LINE.                                
04473                                                                   
121902*    IF WS-COMPANY-ID = 'POS'                                     
121902*       IF ((CL-CLAIM-TYPE = WS-AH-OVERRIDE-L1 OR 'I' OR 'G')
121902*          AND (CM-AH-BENEFIT-CD = '49'))                             
121902*        OR                                                       
121902*         ((CL-CLAIM-TYPE = WS-LIFE-OVERRIDE-L1) AND              
121902*          (CM-LF-BENEFIT-CD = '49'))                             
121902*            IF CM-PREMIUM-TYPE = '2'                             
121902*                NEXT SENTENCE                                    
121902*              ELSE                                               
121902*                MOVE '2'        TO  CM-PREMIUM-TYPE              
121902*                MOVE +1         TO  WS-REWRITE-CERT.             
121902*                                                                 
04486  2209-EXIT.                                                       
04487      EXIT.                                                        
04488                                  EJECT                            
04489  2210-GET-POLICY-MASTER SECTION.                                  
04490                                                                   
121902*    IF CL-SYSTEM-IDENTIFIER NOT = 'CV'                           
04492      DISPLAY '*** CARRIER      *** ' CL-CARRIER.               
04493      DISPLAY '*** NON CV CLAIM *** ' CL-CLAIM-NO.              
04494      DISPLAY '*** COMPANY      *** ' WS-COMPANY-ID.           
04495      DISPLAY ' '.                                             
04496                                                                   
04497      MOVE ZERO                   TO  WS-REWRITE-POLICY.           
04498                                                                   
04499      MOVE WS-COMPANY-CODE        TO  PM-COMPANY-CD.               
04500      MOVE CL-CERT-CARRIER        TO  PM-CARRIER.                  
04501      MOVE CL-CERT-GROUPING       TO  PM-GROUPING.                 
04502      MOVE CL-CERT-STATE          TO  PM-STATE.                    
04503      MOVE CL-CERT-ACCOUNT        TO  PM-PRODUCER.                 
04504      MOVE CL-CERT-EFF-DT         TO  PM-POLICY-EFF-DT.            
04505      MOVE CL-CV-REFERENCE-NO     TO  PM-REFERENCE-NUMBER.         
04506      MOVE PM-CONTROL-PRIMARY     TO  WS-CONTROL-PRIMARY.          
04507                                                                   
04508      MOVE 'RR'                   TO  WS-EMPLCY-FUNCTION.          
04509                                                                   
04510      PERFORM 8350-CALL-EMPLCYX THRU 8350-EXIT.                    
04511                                                                   
04512      IF WS-EMPLCY-RETURN-CODE NOT = '00'                          
04513          MOVE 'ERROR OCCURRED READ - MPPLCY'                      
04514                                  TO  WS-ABEND-MESSAGE             
04515          MOVE WS-EMPLCY-RETURN-CODE TO  WS-ABEND-FILE-STATUS      
04516          GO TO ABEND-PGM.                                         
04517                                                                   
04518      IF PM-CLAIM-ATTACH-CNT NOT GREATER THAN ZERO                 
04519          MOVE +1                 TO  WS-CLAIM-ATTACH-CNT          
04520                                      WS-REWRITE-POLICY            
04521          IF PM-CLAIM-INTERFACE-SW = SPACES                        
04522              MOVE '1'            TO  WS-CLAIM-INTERFACE-SW.       
04523                                                                   
04524  2210-EXIT.                                                       
04525      EXIT.                                                        
04526                                  EJECT                            
04527  2215-GET-PRODUCER-PLAN   SECTION.                                
04528                                                                   
04529      MOVE ZERO                   TO  WS-PLAN-NOT-FOUND.           
04530                                                                   
04531      MOVE WS-COMPANY-CODE        TO  PP-COMPANY-CD.               
04532      MOVE PM-CARRIER             TO  PP-CARRIER.                  
04533      MOVE PM-GROUPING            TO  PP-GROUPING.                 
04534      MOVE PM-STATE               TO  PP-STATE.                    
04535      MOVE PM-PRODUCER            TO  PP-PRODUCER.                 
04536      MOVE PM-INS-PLAN-CD         TO  PP-PLAN-CODE.                
04537      MOVE PM-INS-PLAN-REVISION   TO  PP-PLAN-REVISION.            
04538                                                                   
04539      READ MPPLAN.                                                 
04540                                                                   
04541      IF MPPLAN-FILE-STATUS = '23'                                 
04542          MOVE +1                 TO  WS-PLAN-NOT-FOUND            
04543          GO TO 2215-EXIT.                                         
04544                                                                   
04545      IF MPPLAN-FILE-STATUS NOT = ZERO                             
04546          MOVE 'ERROR OCCURRED READ - MPPLAN'                      
04547                                  TO  WS-ABEND-MESSAGE             
04548          MOVE MPPLAN-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
04549          GO TO ABEND-PGM.                                         
04550                                                                   
04551  2215-EXIT.                                                       
04552      EXIT.                                                        
04553                                  EJECT                            
04554  2220-GEN-LETTER SECTION.                                         
04555                                                                   
04556 *    NOTE ******************************************************* 
04557 *         *      UPDATE THE ACTIVITY QUEUE RECORD FOR THE       * 
04558 *         *  CLOSED CLAIM LETTER IN THE AUTO ACTIVITY RECORD    * 
04559 *         *******************************************************.
04560                                                                   
04561      MOVE CL-CONTROL-PRIMARY     TO  AQ-CONTROL-PRIMARY.          
04562                                                                   
04563      READ ELACTQ INTO WS-SAVE-ACTIVITY-QUEUE.                     
04564                                                                   
04565      IF ELACTQ-FILE-STATUS = '23'                                 
04566          GO TO 2240-GEN-LETTER.                                   
04567                                                                   
04568      IF ELACTQ-FILE-STATUS NOT = ZERO                             
04569          MOVE 'ERROR OCCURRED READ - ELACTQ' TO  WS-ABEND-MESSAGE 
04570          MOVE ELACTQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
04571          GO TO ABEND-PGM.                                         
04572                                                                   
04573      GO TO 2260-GEN-LETTER.                                       
04574                                  EJECT                            
04575  2240-GEN-LETTER.                                                 
04576                                                                   
04577      MOVE 'AQ'                   TO  ACTIVITY-QUE.                
04578                                                                   
04579      MOVE CL-CONTROL-PRIMARY     TO  AQ-CONTROL-PRIMARY.          
04580      MOVE '1'                    TO  AQ-PENDING-LETTER-FLAG.      
04581      MOVE +0                     TO  AQ-PAYMENT-COUNTER           
04582                                      AQ-PMT-UNAPPROVED-COUNT.     
04583                                                                   
04584      MOVE SPACES                 TO  AQ-PENDING-STATUS-FLAG       
04585                                      AQ-PENDING-PAYMENT-FLAG.     
04586                                                                   
04587      MOVE LOW-VALUES             TO  AQ-RESEND-DATE               
04588                                      AQ-FOLLOWUP-DATE.            
04589                                                                   
04590      MOVE +3100                  TO  AQ-LAST-UPDATED-BY.          
04591                                                                   
04592      MOVE +20                    TO SLR-KEY-LENGTH.               
04593      MOVE +60                    TO SLR-RECORD-LENGTH.            
04594      MOVE AQ-CONTROL-PRIMARY     TO SLR-KEY.                      
04595      MOVE ACTIVITY-QUE           TO SLR-RECORD-IMAGE.             
04596      MOVE 'ELACTQ'               TO SLR-DSID.                     
04597      MOVE 'A'                    TO SLR-ACTION.                   
04598                                                                   
121902*    IF WS-COMPANY-ID = 'AIG' OR 'AUK'                            
121902*        MOVE WS-AAR-LETTER      TO AQ-AUTO-LETTER                
121902*        GO TO 2250-GEN-LETTER.                                   
04602                                                                   
04603      MOVE WS-SYS-LETTER-ID (07)  TO AQ-AUTO-LETTER.               
04604                                                                   
04605      MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1.                
04606      MOVE +0                     TO DC-ELAPSED-MONTHS.            
04607                                                                   
04608      IF WS-SYS-RESEND-DAYS (07) NOT = ZEROS                       
04609         MOVE WS-SYS-RESEND-DAYS (07)                              
04610                                  TO DC-ELAPSED-DAYS               
04611         MOVE '6'                 TO DC-OPTION-CODE                
04612         PERFORM 8500-DATE-CONVERSION                              
04613         IF NO-CONVERSION-ERROR                                    
04614            MOVE DC-BIN-DATE-2    TO AQ-RESEND-DATE.               
04615                                                                   
04616      IF WS-SYS-FOLLOW-UP-DAYS (07) NOT = ZEROS                    
04617         MOVE WS-SYS-FOLLOW-UP-DAYS (07)                           
04618                                  TO DC-ELAPSED-DAYS               
04619         MOVE '6'                 TO DC-OPTION-CODE                
04620         PERFORM 8500-DATE-CONVERSION                              
04621         IF NO-CONVERSION-ERROR                                    
04622            MOVE DC-BIN-DATE-2    TO AQ-FOLLOWUP-DATE.             
04623                                                                   
04624  2250-GEN-LETTER.                                                 
04625                                                                   
04626      WRITE ACTIVITY-QUE.                                          
04627                                                                   
04628      IF ELACTQ-FILE-STATUS = ZERO                                 
04629 *       PERFORM LOG-JOURNAL-RECORD                                
04630         GO TO 2290-EXIT.                                          
04631                                                                   
04632      MOVE 'ERROR OCCURRED WRITE - ELACTQ' TO  WS-ABEND-MESSAGE.   
04633      MOVE ELACTQ-FILE-STATUS     TO  WS-ABEND-FILE-STATUS.        
04634      GO TO ABEND-PGM.                                             
04635                                                                   
04636  2260-GEN-LETTER.                                                 
04637                                                                   
04638      MOVE '1'                    TO  AQ-PENDING-LETTER-FLAG.      
04639                                                                   
04640      MOVE +3100                  TO  AQ-LAST-UPDATED-BY.          
04641                                                                   
04642      MOVE +20                    TO SLR-KEY-LENGTH.               
04643      MOVE +60                    TO SLR-RECORD-LENGTH.            
04644      MOVE AQ-CONTROL-PRIMARY     TO SLR-KEY.                      
04645      MOVE ACTIVITY-QUE           TO SLR-RECORD-IMAGE.             
04646      MOVE 'ELACTQ'               TO SLR-DSID.                     
04647      MOVE 'C'                    TO SLR-ACTION.                   
04648                                                                   
121902*    IF WS-COMPANY-ID = 'AIG' OR 'AUK'                            
121902*        MOVE WS-AAR-LETTER      TO  AQ-AUTO-LETTER               
121902*        GO TO 2265-REWRITE-ELACTQ.                               
04652                                                                   
04653      MOVE WS-SYS-LETTER-ID (07)  TO AQ-AUTO-LETTER.               
04654                                                                   
04655      MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1.                
04656      MOVE +0                     TO DC-ELAPSED-MONTHS.            
04657                                                                   
04658      IF WS-SYS-RESEND-DAYS (07) NOT = ZEROS                       
04659         MOVE WS-SYS-RESEND-DAYS (07)                              
04660                                  TO DC-ELAPSED-DAYS               
04661         MOVE '6'                 TO DC-OPTION-CODE                
04662         PERFORM 8500-DATE-CONVERSION                              
04663         IF NO-CONVERSION-ERROR                                    
04664            MOVE DC-BIN-DATE-2    TO AQ-RESEND-DATE.               
04665                                                                   
04666      IF WS-SYS-FOLLOW-UP-DAYS (07) NOT = ZEROS                    
04667         MOVE WS-SYS-FOLLOW-UP-DAYS (07)                           
04668                                  TO DC-ELAPSED-DAYS               
04669         MOVE '6'                 TO DC-OPTION-CODE                
04670         PERFORM 8500-DATE-CONVERSION                              
04671         IF NO-CONVERSION-ERROR                                    
04672            MOVE DC-BIN-DATE-2    TO AQ-FOLLOWUP-DATE.             
04673                                                                   
04674  2265-REWRITE-ELACTQ.                                             
04675                                                                   
04676      REWRITE ACTIVITY-QUE                                         
04677                                                                   
04678      IF ELACTQ-FILE-STATUS NOT = ZERO                             
04679          MOVE 'ERROR OCCURRED REWRITE -ELACTQ' TO WS-ABEND-MESSAGE
04680          MOVE ELACTQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
04681          GO TO ABEND-PGM.                                         
04682                                                                   
04683 *    PERFORM LOG-JOURNAL-RECORD.                                  
04684                                                                   
04685  2290-EXIT.                                                       
04686      EXIT.                                                        
04687                                  EJECT                            
04688  2300-GET-ACCOUNT SECTION.                                        
04689 *    NOTE ******************************************************* 
04690 *         *      THIS SECTION GETS THE PROPER ACCOUNT MASTER    * 
04691 *         *  RECORD FOR EACH CLAIM TO BE PROCESSED.             * 
04692 *         *******************************************************.
04693                                                                   
04694      MOVE ZERO                   TO  WS-ACCOUNT-NOT-FOUND.        
04695                                                                   
04696      IF (AM-COMPANY-CD = WS-COMPANY-CD) AND                       
04697         (AM-CARRIER    = CM-CARRIER)    AND                       
04698         (AM-GROUPING   = CM-GROUPING)   AND                       
04699         (AM-STATE      = CM-STATE)      AND                       
04700         (AM-ACCOUNT    = CM-ACCOUNT)    AND                       
04701         (CM-CERT-EFF-DT NOT LESS THAN AM-EFFECTIVE-DT) AND        
04702         (CM-CERT-EFF-DT LESS THAN AM-EXPIRATION-DT)               
04703          GO TO 2349-EXIT.                                         
04704                                                                   
04705      MOVE WS-COMPANY-CODE        TO  AM-COMPANY-CD.               
04706      MOVE CL-CERT-CARRIER        TO  AM-CARRIER.                  
04707      MOVE CL-CERT-GROUPING       TO  AM-GROUPING.                 
04708      MOVE CL-CERT-STATE          TO  AM-STATE.                    
04709      MOVE CL-CERT-ACCOUNT        TO  AM-ACCOUNT.                  
04710      MOVE CL-CERT-EFF-DT         TO  AM-EXPIRATION-DT.            
04711 *GNB                                                              
CSOMOD*    MOVE AM-EXPIRATION-DT       TO WS-ERACCT.                    
CSOMOD*    DISPLAY 'AM-EXPIRATION-DT: ' WS-ERACCT.                      
04714                                                                   
04715      START ERACCT                                                 
04716          KEY IS NOT LESS THAN AM-CONTROL-PRIMARY                  
04717                                                                   
04718      IF ERACCT-FILE-STATUS = '23'                                 
04719          DISPLAY 'ERACCT STATUS 23'                               
04720          DISPLAY 'AM-ACCOUNT: ' AM-ACCOUNT                        
04721          GO TO 2320-GET-ACCOUNT.                                  
04722                                                                   
04723      IF ERACCT-FILE-STATUS NOT = ZERO                             
04724          MOVE 'ERROR OCCURRED START - ERACCT'                     
04725                                  TO  WS-ABEND-MESSAGE             
04726          MOVE ERACCT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
04727          GO TO ABEND-PGM.                                         
04728                                                                   
04729  2310-GET-ACCOUNT.                                                
04730                                                                   
04731      READ ERACCT NEXT.                                            
04732                                                                   
04733      IF ERACCT-FILE-STATUS = '10'                                 
04734          DISPLAY 'ERACCT STAUS 10'                                
04735          DISPLAY 'AM-ACCOUNT: ' AM-ACCOUNT                        
04736          GO TO 2320-GET-ACCOUNT.                                  
04737                                                                   
04738      IF ERACCT-FILE-STATUS NOT = ZERO                             
04739          MOVE 'ERROR OCCURRED READNEXT - ERACCT'                  
04740                                  TO  WS-ABEND-MESSAGE             
04741          MOVE ERACCT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
04742          GO TO ABEND-PGM.                                         
04743                                                                   
04744      IF AM-COMPANY-CD NOT = WS-COMPANY-CODE                       
04745        OR AM-CARRIER  NOT = CM-CARRIER                            
04746        OR AM-GROUPING NOT = CM-GROUPING                           
04747        OR AM-STATE    NOT = CM-STATE                              
04748        OR AM-ACCOUNT  NOT = CM-ACCOUNT                            
04749          DISPLAY 'CO., CARRIER, GROUPING, STATE, ACCOUNT, BAD'    
04750          GO TO 2320-GET-ACCOUNT.                                  
04751                                                                   
04752      IF CM-CERT-EFF-DT NOT LESS THAN AM-EFFECTIVE-DT              
04753        AND CM-CERT-EFF-DT LESS THAN AM-EXPIRATION-DT              
04754          GO TO 2349-EXIT.                                         
04755                                                                   
04756      GO TO 2310-GET-ACCOUNT.                                      
04757                                                                   
04758  2320-GET-ACCOUNT.                                                
04759                                                                   
04760      MOVE +1                 TO  WS-ACCOUNT-NOT-FOUND.            
04761                                                                   
04762      MOVE SPACES                 TO  AM-NAME                      
04763                                      AM-PERSON                    
04764                                      AM-ADDRS                     
04765                                      AM-CITY.                     
04766                                                                   
04767      MOVE ZERO                   TO  AM-ZIP                       
04768                                      AM-TEL-NO.                   
04769                                                                   
04770      MOVE 'ACCOUNT MASTER NOT FOUND ' TO WS-D1-MESSAGE.           
04771      MOVE WS-DETAIL1         TO  PRT.                             
04772      PERFORM WRITE-A-LINE.                                        
04773      MOVE SPACES             TO  WS-DETAIL1.                      
04774                                                                   
04775                                                                   
04776  2349-EXIT.                                                       
04777      EXIT.                                                        
04778                                  EJECT                            
04779  2350-GET-PRODUCER SECTION.                                       
04780 *    NOTE ******************************************************* 
04781 *         *      THIS SECTION GETS THE PROPER PRODUCER MASTER   * 
04782 *         *      RECORD FOR EACH CLAIM TO BE PROCESSED.         * 
04783 *         *******************************************************.
04784                                                                   
04785      MOVE ZERO                   TO  WS-PRODUCER-NOT-FOUND.       
04786                                                                   
04787      MOVE WS-COMPANY-CODE        TO  PD-COMPANY-CD.               
04788      MOVE CL-CERT-CARRIER        TO  PD-CARRIER.                  
04789      MOVE CL-CERT-GROUPING       TO  PD-GROUPING.                 
04790      MOVE CL-CERT-STATE          TO  PD-STATE.                    
04791      MOVE CL-CERT-ACCOUNT        TO  PD-PRODUCER.                 
04792      MOVE CL-CERT-EFF-DT         TO  PD-EXPIRE-DATE.              
04793                                                                   
04794      START MPPROD                                                 
04795          KEY IS NOT LESS THAN PD-CONTROL-PRIMARY.                 
04796                                                                   
04797      IF MPPROD-FILE-STATUS = '23'                                 
04798          GO TO 2370-GET-PRODUCER.                                 
04799                                                                   
04800      IF MPPROD-FILE-STATUS NOT = ZERO                             
04801          MOVE 'ERROR OCCURRED START - MPPROD'                     
04802                                  TO  WS-ABEND-MESSAGE             
04803          MOVE MPPROD-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
04804          GO TO ABEND-PGM.                                         
04805                                                                   
04806  2360-GET-PRODUCER.                                               
04807                                                                   
04808      READ MPPROD NEXT.                                            
04809                                                                   
04810      IF MPPROD-FILE-STATUS = '10'                                 
04811          GO TO 2370-GET-PRODUCER.                                 
04812                                                                   
04813      IF MPPROD-FILE-STATUS NOT = ZERO                             
04814          MOVE 'ERROR OCCURRED READNEXT - MPPROD'                  
04815                                  TO  WS-ABEND-MESSAGE             
04816          MOVE MPPROD-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
04817          GO TO ABEND-PGM.                                         
04818                                                                   
04819      IF PD-COMPANY-CD NOT = WS-COMPANY-CODE OR                    
04820         PD-CARRIER    NOT = PM-CARRIER      OR                    
04821         PD-GROUPING   NOT = PM-GROUPING     OR                    
04822         PD-STATE      NOT = PM-STATE        OR                    
04823         PD-PRODUCER   NOT = PM-PRODUCER                           
04824          GO TO 2360-GET-PRODUCER.                                 
04825                                                                   
04826      IF PM-POLICY-EFF-DT NOT LESS THAN PD-EFFECT-DTE   AND        
04827         PM-POLICY-EFF-DT LESS THAN PD-EXPIRE-DATE                 
04828          GO TO 2390-EXIT.                                         
04829                                                                   
04830      GO TO 2360-GET-PRODUCER.                                     
04831                                                                   
04832  2370-GET-PRODUCER.                                               
04833                                                                   
04834      MOVE +1                 TO  WS-PRODUCER-NOT-FOUND.           
04835                                                                   
04836      MOVE SPACES                 TO  PD-NAME                      
04837                                      PD-PERSON                    
04838                                      PD-ADDRS                     
04839                                      PD-CITY.                     
04840                                                                   
04841      MOVE ZERO                   TO  PD-ZIP                       
04842                                      PD-TEL-NO.                   
04843                                                                   
04844      MOVE 'PRODUCER MASTER NOT FOUND ' TO WS-D1-MESSAGE.          
04845      MOVE WS-DETAIL1         TO  PRT.                             
04846      PERFORM WRITE-A-LINE.                                        
04847      MOVE SPACES             TO  WS-DETAIL1.                      
04848                                                                   
04849  2390-EXIT.                                                       
04850      EXIT.                                                        
04851                                  EJECT                            
04852  2400-GET-CHECK-QUE-RECORD SECTION.                               
04853                                                                   
04854      MOVE WS-COMPANY-CODE        TO  CQ-COMPANY-CD.               
04855      MOVE AT-CHECK-QUE-CONTROL   TO  CQ-CONTROL-NUMBER.           
04856      MOVE AT-CHECK-QUE-SEQUENCE  TO  CQ-SEQUENCE-NUMBER.          
04857                                                                   
04858      READ ELCHKQ INTO WS-SAVE-CHECK-QUEUE.                        
04859                                                                   
04860      IF ELCHKQ-FILE-STATUS = '23'                                 
04861          GO TO 2400-EXIT.                                         
04862                                                                   
04863      IF ELCHKQ-FILE-STATUS NOT = ZERO                             
04864          MOVE 'ERROR OCCURRED READ - ELCHKQ'                      
04865                                  TO  WS-ABEND-MESSAGE             
04866          MOVE ELCHKQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
04867          GO TO ABEND-PGM.                                         
04868                                                                   
04869  2400-EXIT.                                                       
04870      EXIT.                                                        
04871                                                                   
04872 *2410-REWRITE-CHECK-QUE SECTION.                                  
04873 *                                                                 
04874 *    MOVE +3100                  TO  CQ-LAST-UPDATED-BY.          
04875 *                                                                 
04876 *    MOVE +7                     TO SLR-KEY-LENGTH.               
04877 *    MOVE +100                   TO SLR-RECORD-LENGTH.            
04878 *    MOVE CQ-CONTROL-PRIMARY     TO SLR-KEY.                      
04879 *    MOVE CHECK-QUE              TO SLR-RECORD-IMAGE.             
04880 *    MOVE 'ELCHKQ'               TO SLR-DSID.                     
04881 *    MOVE 'C'                    TO SLR-ACTION.                   
04882 *                                                                 
04883 *    REWRITE CHECK-QUE.                                           
04884 *                                                                 
04885 *    IF ELCHKQ-FILE-STATUS NOT = ZERO                             
04886 *        MOVE 'ERROR OCCURRED REWRITE - ELCHKQ'                   
04887 *                                TO  WS-ABEND-MESSAGE             
04888 *        MOVE ELCHKQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
04889 *        GO TO ABEND-PGM.                                         
04890 *                                                                 
04891 *    PERFORM LOG-JOURNAL-RECORD.                                  
04892 *                                                                 
04893 *2410-EXIT.                                                       
04894 *    EXIT.                                                        
04895 *                                EJECT                            
04896  2450-GET-BENEFICIARY  SECTION.                                   
04897                                                                   
04898      MOVE ZERO                   TO  WS-BENE-NOT-FOUND.           
04899      MOVE WS-COMPANY-CODE        TO  BE-COMPANY-CD.               
04900      MOVE 'B'                    TO  BE-RECORD-TYPE.              
04901      MOVE CL-BENEFICIARY         TO  BE-BENEFICIARY.              
04902                                                                   
04903      IF (BE-BENEFICIARY = CL-BENEFICIARY) AND                     
04904         (BE-COMPANY-CD = WS-COMPANY-CD)                           
04905          GO TO 2450-EXIT.                                         
04906                                                                   
04907      READ ELBENE.                                                 
04908                                                                   
04909      IF ELBENE-FILE-STATUS NOT = ZERO                             
04910          MOVE +1                 TO  WS-BENE-NOT-FOUND            
04911          GO TO 2450-EXIT.                                         
04912                                                                   
04913  2450-EXIT.                                                       
04914      EXIT.                                                        

061013 2460-update-elcrtt  SECTION.
061013
061013     move cm-control-primary     to cs-control-primary
061013     move 'B'                    to cs-trailer-type
061013     read elcrtt
061013     if elcrtt-file-status not = '00'
061013        display ' claim hist cert trailer not found '
061013           cs-control-primary (2:19) ' ' cs-cert-no
061013        go to 2460-hist-not-found
061013     end-if
061013
061013*    if cl-insured-claim = 'P'
061013*       move +1                  to s1
061013*    else
061013*       MOVE +2                  to s1
061013*    end-if
061013
061013*    move +1 to s1
061013*    evaluate cl-claim-type
061013*       when 'A'
061013*          move +1 to s2
061013*       when 'I'
061013*          move +2 to s2
061013*       when 'G'
061013*          move +3 to s2
061013*       when 'L'
061013*          move +4 to s2
061013*       when 'P'
061013*          move +5 to s2
061013*    end-evaluate
061013
051414     perform varying s1 from +1 by +1 until
051414        (s1 > +24)
051414        or (cl-claim-no = cs-claim-no (s1))
051414     end-perform
051414     if s1 < +25
051414        compute cs-total-paid (s1) =
051414           cs-total-paid (s1) + at-amount-paid
051414        compute cs-days-paid (s1) =
051414           cs-days-paid (s1) + at-days-in-period
051414     end-if

061013     rewrite certificate-trailers
061013     if elcrtt-file-status not = zeros
061013        display ' error-elcrtt-rewrite ' elcrtt-file-status ' '
061013           cs-cert-no
061013        go to 2460-exit
061013     end-if
061013
061013     MOVE 'claim hist updated'   TO WS-D1-MESSAGE
061013     MOVE WS-DETAIL1             TO PRT
061013     PERFORM WRITE-A-LINE
061013     MOVE SPACES                 TO WS-DETAIL1
061013     go to 2460-exit

           .
051414 2460-hist-not-found.
051414
051414     MOVE ' ERROR - NO CERT CLAIM HISTORY  '
051414                                 TO WS-D1-MESSAGE
051414     MOVE WS-DETAIL1             TO PRT
051414     PERFORM WRITE-A-LINE
051414     MOVE SPACES                 TO WS-DETAIL1

061013     .
061013 2460-exit.
061013     exit.

04916                                  EJECT                            
04917  2500-AUTO-PAYMENTS SECTION.                                      
04918 *    NOTE ******************************************************* 
04919 *         *      THIS SECTION DOES ALL OF THE AUTO PAYMENT      * 
04920 *         *  GENERATION AND REPORTING.                          * 
04921 *         *******************************************************.
04922                                                                   
04923      MOVE SPACES                 TO  AT-CONTROL-PRIMARY.          
04924                                                                   
04925      MOVE CL-COMPANY-CD          TO  AT-COMPANY-CD.               
04926      MOVE CL-CARRIER             TO  AT-CARRIER.                  
04927      MOVE CL-CLAIM-NO            TO  AT-CLAIM-NO.                 
04928      MOVE CL-CERT-NO             TO  AT-CERT-NO.                  
04929      MOVE CL-AUTO-PAY-SEQ        TO  AT-SEQUENCE-NO.              
04930                                                                   
04931      IF CL-AUTO-PAY-SEQ = +0                                      
04932          MOVE 'AUTO PAY TRAILER SEQ ZERO'  TO  WS-D1-MESSAGE      
04933          MOVE LOW-VALUES      TO  CL-NEXT-AUTO-PAY-DT             
04934          MOVE WS-DETAIL1      TO  PRT                             
04935          PERFORM WRITE-A-LINE                                     
04936          GO TO 2590-EXIT.                                         
04937                                                                   
04938      PERFORM 8300-READ-TRAILER.                                   
04939                                                                   
04940      IF WS-TRAILER-NOT-FOUND NOT = ZERO                           
04941          MOVE 'AUTO PAY TRAILER NOT FOUND'  TO  WS-D1-MESSAGE     
04942          MOVE WS-DETAIL1         TO  PRT                          
04943          PERFORM WRITE-A-LINE                                     
04944          GO TO 2590-EXIT.                                         
04945                                                                   
04946      MOVE AT-AUTO-CASH           TO WS-AUTO-CASH.                 
04947      MOVE AT-SCHEDULE-START-DT   TO WS-AIG-AUTO-START-DT.         
04948      MOVE AT-SCHEDULE-END-DT     TO WS-AIG-AUTO-END-DT.     
070909     MOVE AT-AUTO-END-LETTER     TO WS-AUTO-END-LETTER.      
04949                                                                   
04950 ***************************************************************** 
04951 *   IF THE ESTABLISHED TERMINATION DATE IS NOT EQUAL LOW-VALUES * 
04952 *     AND THIS DATE IS EQUAL TO OR LESS THAN TODAY - THIS IS NO * 
04953 *     LONGER ON THE AUTO PAY SCHEDULE -EXIT FROM THE ROUTINE    * 
04954 *****************************************************************.
04955                                                                   
04956      IF AT-TERMINATED-DT NOT = LOW-VALUES                         
04957        AND AT-TERMINATED-DT NOT GREATER THAN BIN-RUN-DATE         
04958          GO TO 2590-EXIT.                                         
04959                                                                   
04960      IF AT-AUTO-PAY-DAY NOT NUMERIC                               
04961         MOVE ZEROS               TO AT-AUTO-PAY-DAY.              
04962                                                                   
04963      IF AT-AUTO-PAY-DAY = ZEROS                                   
04964         MOVE CL-NEXT-AUTO-PAY-DT TO WS-GENERATE-AUTO-PAY-DT       
04965         GO TO 2510-AUTO-PAYMENTS                                  
04966      ELSE                                                         
04967         MOVE AT-AUTO-PAY-DAY     TO WS-AUTO-PAY-DAY.              
04968                                                                   
04969      MOVE CL-NEXT-AUTO-PAY-DT         TO DC-BIN-DATE-1.           
04970      MOVE ' '                         TO DC-OPTION-CODE.          
04971      PERFORM 8500-DATE-CONVERSION.                                
04972      MOVE DC-GREG-DATE-CYMD           TO WS-AUTO-WORK-DATE-N.     
04973                                                                   
04974      IF WS-AUTO-PAY-DAY GREATER THAN DC-DAYS-IN-MONTH             
04975         MOVE DC-DAYS-IN-MONTH         TO WS-AUTO-PAY-DAY.         
04976                                                                   
04977      IF WS-AUTO-PAY-DAY LESS THAN WS-AUTO-WORK-DD                 
04978         MOVE WS-AUTO-PAY-DAY          TO WS-AUTO-WORK-DD          
04979         MOVE WS-AUTO-WORK-DATE-N      TO DC-GREG-DATE-CYMD        
04980         MOVE 'L'                      TO DC-OPTION-CODE           
04981         PERFORM 8500-DATE-CONVERSION                              
04982         MOVE DC-BIN-DATE-1            TO WS-GENERATE-AUTO-PAY-DT  
04983         GO TO 2510-AUTO-PAYMENTS                                  
04984      ELSE                                                         
04985         IF WS-AUTO-PAY-DAY = WS-AUTO-WORK-DD                      
04986            MOVE CL-NEXT-AUTO-PAY-DT  TO WS-GENERATE-AUTO-PAY-DT   
04987            GO TO 2510-AUTO-PAYMENTS.                              
04988                                                                   
04989      IF WS-AUTO-WORK-MM = WS-AD-MM                                
04990          IF WS-AUTO-PAY-DAY IS GREATER THAN WS-AUTO-WORK-DD       
04991              SUBTRACT 1 FROM WS-AUTO-WORK-MM                      
04992          ELSE                                                     
04993              NEXT SENTENCE                                        
04994      ELSE                                                         
04995          SUBTRACT 1 FROM WS-AUTO-WORK-MM.                         
04996                                                                   
04997      IF WS-AUTO-WORK-MM = ZEROS                                   
04998         MOVE 12 TO WS-AUTO-WORK-MM                                
04999         SUBTRACT 1 FROM WS-AUTO-WORK-CCYY.                        
05000                                                                   
05001      MOVE WS-AUTO-WORK-DATE-N      TO DC-GREG-DATE-CYMD.          
05002      MOVE 'L'                      TO DC-OPTION-CODE.             
05003      PERFORM 8500-DATE-CONVERSION.                                
05004                                                                   
05005      IF WS-AUTO-PAY-DAY GREATER THAN DC-DAYS-IN-MONTH             
05006          MOVE DC-DAYS-IN-MONTH     TO WS-AUTO-PAY-DAY.            
05007                                                                   
05008      MOVE WS-AUTO-PAY-DAY          TO WS-AUTO-WORK-DD.            
05009      MOVE WS-AUTO-WORK-DATE-N      TO DC-GREG-DATE-CYMD.          
05010      MOVE 'L'                      TO DC-OPTION-CODE.             
05011      PERFORM 8500-DATE-CONVERSION.                                
05012      MOVE DC-BIN-DATE-1            TO WS-GENERATE-AUTO-PAY-DT.    
05013                                  EJECT                            
05014  2510-AUTO-PAYMENTS.                                              
05015                                                                   
DAN01      MOVE AT-SCHEDULE-END-DT  TO  WS-SCHEDULE-END-DT              
DAN01      MOVE AT-LAST-PMT-TYPE    TO  WS-LAST-PMT-TYPE                
DAN01      MOVE AT-INTERVAL-MONTHS  TO  WS-INTERVAL-MONTHS              
DAN01                                                                   
05016      IF WS-GENERATE-AUTO-PAY-DT NOT GREATER THAN                  
05017                WS-CURRENT-DATE-PLUS-5                             
05018         NEXT SENTENCE                                             
05019      ELSE                                                         
05020         GO TO 2590-EXIT.                                          
091808
091808 2510-10-DETERMINE-ADDR-STATE.
091808***NEED TO GET ADDRESS TO SEE IF IT IS AN ALASKA PAYMENT
091808     MOVE AT-AUTO-PAYEE-CD       TO  WS-AUTO-PAYEE-CODE.          
091808                                                                  
091808     IF WS-AUTO-PAYEE-SEQ-NUM NOT NUMERIC                         
091808        MOVE 1                     TO WS-AUTO-PAYEE-SEQ-NUM.      
091808                                                                  
091808     IF WS-AUTO-PAYEE-CD = 'I'                                    
091808        MOVE WS-AUTO-PAYEE-SEQ-NUM TO AT-SEQUENCE-NO              
091808        GO TO 2510-20-AUTO-PAYMENT-ADDR.                                 
091808                                                                  
091808     IF WS-AUTO-PAYEE-CD = 'O'                                    
091808        MOVE WS-AUTO-PAYEE-SEQ-NUM TO  AT-SEQUENCE-NO             
091808        ADD +50    TO AT-SEQUENCE-NO                              
091808        GO TO 2510-20-AUTO-PAYMENT-ADDR.
091808                                                                  
091808     IF WS-AUTO-PAYEE-CD = 'Q'                                    
091808        MOVE WS-AUTO-PAYEE-SEQ-NUM TO  AT-SEQUENCE-NO             
091808        ADD +60    TO AT-SEQUENCE-NO                              
091808        GO TO 2510-20-AUTO-PAYMENT-ADDR.                             
091808                                                                  
091808     IF WS-AUTO-PAYEE-CD = 'B'                                    
091808         IF CL-BENIF-ADDR-CNT NOT = +0                            
041712          AND WS-AUTO-PAYEE-SEQ-NUM > 0
091808             MOVE WS-AUTO-PAYEE-SEQ-NUM  TO  AT-SEQUENCE-NO       
091808             ADD +10                     TO  AT-SEQUENCE-NO       
091808             GO TO 2510-20-AUTO-PAYMENT-ADDR                          
091808         ELSE                                                     
091808             PERFORM 2450-GET-BENEFICIARY  THRU  2450-EXIT        
091808             IF WS-BENE-NOT-FOUND = ZERO                          
091808                 MOVE BE-MAIL-TO-NAME    TO  WS-PAYEE-NAME  
051810                 MOVE BE-CITY    TO WS-PAYEE-CITY
051810                 MOVE BE-STATE   TO WS-PAYEE-STATE
091808                 GO TO 2510-30-AUTO-PAYMENT-STATE                         
091808             ELSE                                   
091808                 DISPLAY 'BENE ADDR NOT FOUND FOR CLAIM ' 
091808                          AT-CLAIM-NO
091808                 MOVE CL-CERT-STATE      TO  WS-PAYEE-STATE
091808                 GO TO 2510-40-RELOAD-AUTO-PAYMENT.                        
091808                                                                  
091808     IF WS-AUTO-PAYEE-CD = 'A'                                    
091808         MOVE CL-CERT-STATE              TO  WS-PAYEE-STATE
091808         GO TO 2510-40-RELOAD-AUTO-PAYMENT.                        
091808                                                                  
091808 2510-20-AUTO-PAYMENT-ADDR.                                              
091808                                                                  
091808     PERFORM 8300-READ-TRAILER.                                   
091808                                                                  
091808     IF WS-TRAILER-NOT-FOUND NOT = ZERO   
091808         DISPLAY 'ADDR TRLR NOT FOUND FOR CLAIM ' AT-CLAIM-NO                        
091808         MOVE CL-CERT-STATE              TO  WS-PAYEE-STATE
091808         GO TO 2510-40-RELOAD-AUTO-PAYMENT.                        
091808                                                                  
091808     IF AT-TRAILER-TYPE NOT = '5'                                 
091808         DISPLAY 'ADDR TRLR NOT FOUND FOR CLAIM ' AT-CLAIM-NO                        
091808         MOVE CL-CERT-STATE              TO  WS-PAYEE-STATE
091808         GO TO 2510-40-RELOAD-AUTO-PAYMENT.                        
091808                                                                  
051810     MOVE AT-CITY                TO WS-PAYEE-CITY
051810     MOVE AT-STATE               TO WS-PAYEE-STATE

           .
091808 2510-30-AUTO-PAYMENT-STATE.
091808
051810     GO TO 2510-40-RELOAD-AUTO-PAYMENT
091808*    MOVE 'N' TO WS-PAYEE-STATE-FOUND.
091808*    MOVE 0 TO WS-BEG-SUB WS-END-SUB.
091808*    PERFORM VARYING WS-SUB FROM 30 BY -1
091808*       UNTIL WS-SUB = 0 OR PAYEE-STATE-FOUND
091808*      IF WS-END-SUB = 0  AND
091808*        (WS-PAYEE-CITY-ST (WS-SUB) EQUAL SPACES OR '.')
091808*         CONTINUE
091808*      ELSE
091808*         IF WS-END-SUB = 0
091808*             MOVE WS-SUB TO WS-END-SUB
091808*         ELSE
091808*             IF WS-PAYEE-CITY-ST (WS-SUB) = ' ' OR ','
091808*                 COMPUTE WS-BEG-SUB = WS-SUB + 1 
091808*                 MOVE 'Y' TO WS-PAYEE-STATE-FOUND
091808*             END-IF
091808*         END-IF
091808*      END-IF
091808*    END-PERFORM.
091808*
091808*    IF WS-BEG-SUB > 0
091808*       COMPUTE WS-STATE-LENGTH = (WS-END-SUB - WS-BEG-SUB) + 1
091808*       IF WS-STATE-LENGTH = 2
091808*          MOVE WS-PAYEE-CITY-STATE (WS-BEG-SUB:WS-STATE-LENGTH) 
091808*                 TO WS-PAYEE-STATE
091808*       ELSE
091808*          IF WS-PAYEE-CITY-STATE (WS-BEG-SUB:WS-STATE-LENGTH) =
091808*             'ALASKA'
091808*              MOVE 'AK' TO WS-PAYEE-STATE
091808*          ELSE
091808*              MOVE 'XX' TO WS-PAYEE-STATE
091808*          END-IF
091808*       END-IF
091808*    ELSE
091808*       MOVE 'XX' TO WS-PAYEE-STATE
091808*    END-IF.
091808     .
091808 2510-40-RELOAD-AUTO-PAYMENT.                                      
091808                                                                  
091808     MOVE SPACES                 TO  AT-CONTROL-PRIMARY.          
091808                                                                  
091808     MOVE CL-COMPANY-CD          TO  AT-COMPANY-CD.               
091808     MOVE CL-CARRIER             TO  AT-CARRIER.                  
091808     MOVE CL-CLAIM-NO            TO  AT-CLAIM-NO.                 
091808     MOVE CL-CERT-NO             TO  AT-CERT-NO.                  
091808     MOVE CL-AUTO-PAY-SEQ        TO  AT-SEQUENCE-NO.              
091808
091808     PERFORM 8300-READ-TRAILER.                                   
05021                                                                   
05022      MOVE SPACES                 TO  REPORTS-EXTRACT-RECORD.      
05023                                                                   
05024      MOVE 'EX'                   TO  EX-RECORD-ID.                
05025      MOVE '2'                    TO  EX-POSITIONING-CODE.         
05026      MOVE 'D'                    TO  EX-EXTRACT-CODE.             
05027      MOVE WS-COMPANY-CODE        TO  EX-COMPANY-CD.               
05028      MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID.               
05029      MOVE 'A'                    TO  EX-RECORD-TYPE.              
05030                                                                   
05031      MOVE CL-CARRIER             TO  EX-SF-CARRIER.               
05032      MOVE CL-CLAIM-NO            TO  EX-SF-CLAIM-NO.              
05033      MOVE CL-CERT-NO             TO  EX-SF-CERT-NO.               
05034                                                                   
121902*    IF CL-SYSTEM-IDENTIFIER = 'CV'                               
121902*        IF PM-CANCEL-DT NOT = LOW-VALUES                         
121902*            IF CL-NEXT-AUTO-PAY-DT GREATER THAN PM-CANCEL-DT     
121902*                MOVE '*'        TO  EX-DA-ERROR-FLAG (4)         
121902*                GO TO 2580-AUTO-PAYMENTS.                        
05040                                                                   
05041 ***************************************************************** 
05042 *   IS THE CERTIFICATE CANCELLED AND IS THE PAYMENT DATE        * 
05043 *     GREATER THAN THE AUTO PAYMENT SCHEDULED DATE ? EXIT FROM  * 
05044 *     THE ROUTINE                                               * 
05045 *****************************************************************.
05046                                                                   
121902*    IF CL-SYSTEM-IDENTIFIER NOT = 'CV'                           
05048          IF CM-AH-CANCEL-DT NOT = LOW-VALUES                      
05049              IF CL-NEXT-AUTO-PAY-DT GREATER THAN CM-AH-CANCEL-DT  
05050                  MOVE '*'        TO  EX-DA-ERROR-FLAG (4)         
05051                  GO TO 2580-AUTO-PAYMENTS.                        
05052                                                                   
05053      IF WS-GENERATE-AUTO-PAY-DT NOT GREATER THAN BIN-RUN-DATE     
05054          GO TO 2511-AUTO-PAYMENTS.                                
05055                                                                   
05056      MOVE WS-NAME-WORK           TO  EX-DA-INSURED-NAME.          
05057                                                                   
05058      MOVE WS-GENERATE-AUTO-PAY-DT TO  DC-BIN-DATE-2.              
05059      MOVE BIN-RUN-DATE           TO  DC-BIN-DATE-1.               
05060      MOVE '1'                    TO  DC-OPTION-CODE.              
05061      PERFORM 8500-DATE-CONVERSION.                                
05062      MOVE DC-ODD-DAYS-OVER       TO  EX-SF-DAYS-TO-PAYMENT.       
05063                                                                   
05064      MOVE ZERO                   TO  EX-DA-PAYMENT-AMOUNT         
05065                                      EX-DA-INTERVAL-MONTHS.       
05066                                                                   
05067      GO TO 2580-AUTO-PAYMENTS.                                    
05068                                                                   
05069                                                                   
05070  2511-AUTO-PAYMENTS.                                              
05071                                                                   
05072      MOVE CL-CLAIM-PREM-TYPE     TO  EX-DA-CLAIM-PREM-TYPE.       
05073                                                                   
05074      IF CL-NEXT-AUTO-PAY-DT NOT = AT-1ST-PAY-THRU-DT              
05075          GO TO 2511-CONT.                                         
05076                                                                   
05077 ***************************************************************** 
05078 *   THIS IS THE FIRST PAYMENT MADE ON THE AUTO PAYMENT SCHEDULE * 
05079 *****************************************************************.
05080                                                                   
05081      IF AT-FIRST-PMT-AMT NOT NUMERIC                              
05082          MOVE ZERO               TO AT-FIRST-PMT-AMT              
05083          MOVE 'FIRST PAYMENT AMOUNT NOT NUMERIC'                  
05084                                  TO WS-D1-MESSAGE                 
05085          MOVE WS-DETAIL1         TO PRT                           
05086          MOVE +99                TO WS-LINE-COUNT                 
05087          PERFORM WRITE-A-LINE.                                    
05088                                                                   
05089      MOVE AT-FIRST-PMT-AMT       TO EX-DA-PAYMENT-AMOUNT.         
05090      MOVE AT-DAYS-IN-1ST-PMT     TO WS-DAYS-IN-PERIOD.            
05091      MOVE AT-SCHEDULE-START-DT   TO WS-PAID-FROM-DT.              
05092                                                                   
05093      DIVIDE AT-FIRST-PMT-AMT BY AT-DAYS-IN-1ST-PMT                
05094                       GIVING WS-DAILY-RATE ROUNDED.               
05095                                                                   
121902*    IF WS-COMPANY-ID = 'DMD'                                     
121902*        MOVE CL-INCURRED-DT     TO DC-BIN-DATE-1                 
121902*        MOVE AT-SCHEDULE-START-DT                                
121902*                                TO DC-BIN-DATE-2                 
121902*        MOVE '1'                TO DC-OPTION-CODE                
121902*        PERFORM 8500-DATE-CONVERSION                             
121902*        COMPUTE CL-NO-OF-DAYS-PAID                               
121902*            = CL-NO-OF-DAYS-PAID                                 
121902*            + (30 * DC-ELAPSED-MONTHS)                           
121902*            + DC-ODD-DAYS-OVER                                   
121902*        GO TO 2513-AUTO-PAYMENTS                                 
121902*    ELSE                                                         
05108          ADD AT-DAYS-IN-1ST-PMT  TO  CL-NO-OF-DAYS-PAID.           
05109          GO TO 2513-AUTO-PAYMENTS.                                
05110                                                                   
05111  2511-CONT.                                                       
05112                                                                   
05113      MOVE ZERO                   TO  WS-DAILY-RATE.               
05114                                                                   
05115      MOVE AT-REGULAR-PMT-AMT     TO  EX-DA-PAYMENT-AMOUNT.        
05116      MOVE AT-DAYS-IN-REG-PMT     TO  WS-DAYS-IN-PERIOD.           
05117                                                                   
05118      MOVE CL-PAID-THRU-DT        TO  DC-BIN-DATE-1.               
05119      MOVE +1                     TO  DC-ELAPSED-DAYS.             
05120      MOVE ZERO                   TO  DC-ELAPSED-MONTHS.           
05121      MOVE '6'                    TO  DC-OPTION-CODE.              
05122      PERFORM 8500-DATE-CONVERSION.                                
05123      MOVE DC-BIN-DATE-2          TO  WS-PAID-FROM-DT.             
05124                                                                   
05125 ******************************************************************
05126 *                                                                *
05127 *   ***** NOTE *****                                             *
05128 *     IF THE AUTO PAY SCHEDULE WAS SET UP PRIOR TO 03-01-85      *
05129 *        IT IS POSSIBLE TO HAVE A FINAL PAYMENT OTHER THAN       *
05130 *        FULL MONTHS. THE FOLLOWING ROUTINE WAS ADDED TO         *
05131 *        FORCE A FULL MONTH CALC ON FINAL PAYMENTS, IF THE AUTO  *
05132 *        PAY SCHEDULE WAS RECORDED AFTER 03-01-85.               *
05133 *                                                                *
05134 *    IF (WS-CLAIM-CALCULATION-METHOD = ('4' OR '5'))             *
05135 *      OR ((CL-NEXT-AUTO-PAY-DT NOT LESS THAN AT-SCHEDULE-END-DT)*
05136 *      AND (AT-RECORDED-DT LESS THAN WS-030185))                 *
05137 *        NEXT SENTENCE                                           *
05138 *      ELSE                                                      *
05139 *        GO TO 2513-AUTO-PAYMENTS.                               *
05140 *                                                                *
05141 ******************************************************************
05142                                                                   
05143      MOVE CL-PAID-THRU-DT        TO  DC-BIN-DATE-1.               
05144                                                                   
05145      IF CL-NEXT-AUTO-PAY-DT GREATER THAN AT-SCHEDULE-END-DT       
05146          MOVE AT-SCHEDULE-END-DT TO  DC-BIN-DATE-2                
05147        ELSE                                                       
05148          MOVE CL-NEXT-AUTO-PAY-DT TO  DC-BIN-DATE-2.              
05149                                                                   
05150      MOVE '1'                    TO  DC-OPTION-CODE.              
05151      PERFORM 8500-DATE-CONVERSION.                                
05152                                                                   
05153      MOVE DC-ELAPSED-DAYS        TO  WS-DAYS-IN-PERIOD.           
05154                                                                   
121902*    IF WS-COMPANY-ID = 'FIA'                                     
121902*       NEXT SENTENCE                                             
121902*    ELSE                                                         
05158         GO TO 2513-AUTO-PAYMENTS.                                 

121902*** FIA CODE ****                                                     

121902*    IF WS-CLAIM-CALCULATION-METHOD = '4'                         
121902*        MOVE +360               TO  WS-ANNUAL-DAYS               
121902*      ELSE                                                       
121902*        MOVE +365               TO  WS-ANNUAL-DAYS.              
121902*                     
121902*    IF WS-COMPANY-ID = 'FIA'                                     
121902*      AND CL-CERT-ACCOUNT = '0000011043'                         
121902*        MOVE +13                TO  WS-MONTHS-WORK               
121902*      ELSE                                                       
121902*        MOVE +12                TO  WS-MONTHS-WORK.              
121902*                                                                 
121902*    IF CL-SYSTEM-IDENTIFIER = 'CV'                               
121902*        COMPUTE WS-DAILY-RATE ROUNDED =                          
121902*        (PM-INS-MONTH-BENEFIT * WS-MONTHS-WORK) / WS-ANNUAL-DAYS 
121902*    ELSE                                                         
121902*        COMPUTE WS-DAILY-RATE ROUNDED =                          
121902*           (CM-AH-BENEFIT-AMT * WS-MONTHS-WORK) / WS-ANNUAL-DAYS.
121902*                                                                 
121902*    MULTIPLY WS-DAYS-IN-PERIOD BY WS-DAILY-RATE                  
121902*                           GIVING EX-DA-PAYMENT-AMOUNT ROUNDED.   
121902*                                                                 
121902*    GO TO 2513-AUTO-PAYMENTS.                                    
05182                                                                   
05183  2513-AUTO-PAYMENTS.                                              
05184                                                                   
05185 ***************************************************************** 
05186 *   THIS IS THE LAST PAYMENT MADE ON THE AUTO PAYMENT SCHEDULE. * 
05187 *   THIS ROUTINE MOVES THE SCHEDULED END DATE TO THE TERMINATION* 
05188 *   DATE AND THUS EL310 WILL NOT PRODUCE A CHECK THE NEXT TIME  * 
05189 *   EL310 RUNS.                                                 * 
05190 *****************************************************************.
05191                                                                   
05192      IF (AT-AUTO-PAY-DAY IS GREATER THAN ZEROS AND                
05193          CL-NEXT-AUTO-PAY-DT = AT-SCHEDULE-END-DT)                
05194       OR                                                          
05195         (AT-SCHEDULE-END-DT NOT GREATER THAN BIN-RUN-DATE)        
05196          MOVE AT-LAST-PMT-TYPE   TO  EX-DA-PAYMENT-TYPE           
05197          INSPECT EX-DA-PAYMENT-TYPE CONVERTING 'PF' TO '12'       
05198          MOVE AT-SCHEDULE-END-DT TO  AT-TERMINATED-DT             
05199          MOVE ZERO               TO  CL-AUTO-PAY-SEQ              
05200        ELSE                                                       
05201          MOVE '1'                TO  EX-DA-PAYMENT-TYPE.          
05202                                                                   
05203      MOVE EX-DA-PAYMENT-AMOUNT   TO  WS-AUTO-PAY-AMT.             
05204                                                                   
05205      MOVE AT-INTERVAL-MONTHS     TO  EX-DA-INTERVAL-MONTHS.       
05206                                                                   
05207      MOVE AT-SCHEDULE-END-DT     TO  EX-DA-SCHEDULE-END-DT.       
05208      MOVE AT-LAST-PMT-TYPE       TO  EX-DA-LAST-PAYMENT-TYPE.     
05209                                                                   
121902*    IF (WS-COMPANY-ID = 'AIG' OR 'AUK')                          
121902*              OR                                                 
121902     IF WS-AUTO-CASH = 'N'                                       
05213           GO TO 2514-AUTO-PAYMENTS.                               
05214                                                                   
05215      IF CHECK-NUMBERING-AUTO                                      
05216        OR CHECK-NUMBERING-CARRIER                                 
05217          NEXT SENTENCE                                            
05218        ELSE                                                       
05219          GO TO 2515-AUTO-PAYMENTS.          
091808
111714*    IF WS-PAYEE-STATE = 'AK'
111714*       GO TO 2514-AK-AUTO-PAYMENT
111714*    END-IF.                      
05220                                                                   
05221      ADD +1                      TO  WS-CHECK-COUNTER.            
05222                                                                   
05223      IF WS-CHECK-COUNTER NOT LESS THAN +999999                    
05224          MOVE +1                 TO  WS-CHECK-COUNTER.            
05225                                                                   
05226      IF CHECK-NUMBERING-CARRIER                                   
05227          MOVE WS-LAST-CARRIER    TO  WS-CHECK-CARRIER             
05228        ELSE                                                       
05229          MOVE ZERO               TO  WS-CHECK-CARRIER.            
05230                                                                   
05231      MOVE WS-CHECK-COUNTER       TO  WS-CHECK-NUMBER.             
05232      MOVE WS-CHECK-NUMBER-AREA   TO  EX-DA-CHECK-NUMBER.          
05233      GO TO 2515-AUTO-PAYMENTS.                                    
05234                                                                   
05235  2514-AUTO-PAYMENTS.                                              
05236                                                                   
05237      IF WS-AUTO-CASH = 'N'                                        
05238          NEXT SENTENCE                                            
05239      ELSE                                                         
05240          GO TO 2515-AUTO-PAYMENTS.                                
05241                                                                   
05242      ADD +1                          TO  WS-CHECK-COUNTER.        
05243                                                                   
05244      IF WS-CHECK-COUNTER IS NOT LESS THAN +999999                 
05245          MOVE +1                     TO  WS-CHECK-COUNTER.        
05246                                                                   
05247      MOVE 'N'                        TO  WS-CHECK-CARRIER.        
05248      MOVE WS-CHECK-COUNTER           TO  WS-CHECK-NUMBER.         
05249      MOVE WS-CHECK-NUMBER-AREA       TO  EX-DA-CHECK-NUMBER.      
05250                 
091808     GO TO 2515-AUTO-PAYMENTS.                                                  
05251      EJECT  
091808
091808 2514-AK-AUTO-PAYMENT.
091808     MOVE WS-COMPANY-ID  TO CF-COMPANY-ID.
091808     MOVE SPACES         TO CF-ACCESS-OF-STATE.
091808     MOVE WS-PAYEE-STATE TO CF-STATE-CODE.     
091808     MOVE '3'            TO CF-RECORD-TYPE.    
091808     MOVE +0             TO CF-SEQUENCE-NO.
091808     PERFORM 7300-READ-CONTROL-FILE.
091808
091808     IF ELCNTL-FILE-STATUS = '23'
091808         GO TO 2515-AUTO-PAYMENTS
091808     END-IF.
091808
091808     IF CF-ST-CHECK-CNT-RESET
091808         MOVE +1             TO CF-ST-CHECK-COUNTER
091808     ELSE 
091808         ADD +1              TO CF-ST-CHECK-COUNTER
091808     END-IF.
091808
091808     MOVE CF-ST-CHECK-COUNTER TO WS-AK-CHECK-NUMBER.
091808     MOVE WS-AK-CHECK-NUMBER-AREA TO EX-DA-CHECK-NUMBER.
091808    
091808     PERFORM 7900-REWRITE-CONTROL-FILE.
091808                                                      
05252  2515-AUTO-PAYMENTS.                                              
05253                                                                   
05254 * UPDATE CLAIM RECORD                                             
05255                                                                   
05256      MOVE CL-LAST-PMT-DT         TO WS-SAVE-LAST-DT.              
05257      MOVE CL-PAID-THRU-DT        TO WS-SAVE-PAID-DT.              
05258                                                                   
05259      MOVE CL-LAST-PMT-AMT        TO WS-SAVE-PMT-AMT.              
05260                                                                   
121902*    IF DTE-CLIENT = 'DMD'                                        
121902*       MOVE BIN-RUN-DATE            TO  CL-LAST-PMT-DT           
121902*    ELSE                                                         
05264         MOVE WS-GENERATE-AUTO-PAY-DT TO  CL-LAST-PMT-DT.          
05265                                                                   
05266      MOVE CL-NEXT-AUTO-PAY-DT    TO  CL-PAID-THRU-DT              
05267                                      EX-DA-PAID-THRU-DT.          
05268      MOVE EX-DA-PAYMENT-AMOUNT   TO  CL-LAST-PMT-AMT.             
05269                                                                   
05270      ADD EX-DA-PAYMENT-AMOUNT  TO  CL-TOTAL-PAID-AMT.             
05271      ADD +1                          TO  CL-NO-OF-PMTS-MADE.      
05272                                                                   
05273      MOVE AT-1ST-PAY-THRU-DT         TO  DC-BIN-DATE-1.           
05274      MOVE SPACES                     TO  DC-OPTION-CODE.          
05275      PERFORM 8500-DATE-CONVERSION.                                
05276      MOVE DC-GREG-DATE-1-MDY         TO  WS-WORK-DATE1.           
05277                                                                   
05278      IF DC-DAYS-IN-MONTH = WS-WORK-DD1                            
05279         MOVE '1'                     TO  DC-END-OF-MONTH.         
05280                                                                   
05281      IF CL-NEXT-AUTO-PAY-DT LESS THAN AT-SCHEDULE-END-DT          
05282          MOVE CL-NEXT-AUTO-PAY-DT    TO  DC-BIN-DATE-1            
05283          MOVE AT-INTERVAL-MONTHS     TO  DC-ELAPSED-MONTHS        
05284          MOVE ZERO                   TO  DC-ELAPSED-DAYS          
05285          MOVE '6'                    TO  DC-OPTION-CODE           
05286          PERFORM 8500-DATE-CONVERSION                             
05287          MOVE DC-GREG-DATE-1-MDY     TO  WS-WORK-DATE2            
05288          IF WS-WORK-DD1 GREATER THAN DC-DAYS-IN-MONTH             
05289             MOVE DC-DAYS-IN-MONTH    TO WS-WORK-DD1               
05290          ELSE                                                     
05291             NEXT SENTENCE                                         
05292      ELSE                                                         
05293 ******************************************************************
05294 *** THIS ROUTINE ALSO TERMINATES THE AUTO PAYMENTS AND         ***
05295 *** UPDATES THE CLAIM RECORD WITH A ZERO ON THE AUTO PAY SEQ   ***
05296 *** SO THAT EL310 WILL NOT GO TO LOOK FOR THE AUTO PAY RECORD  ***
05297 *** THE NEXT TIME MONTH END RUNS.                              ***
05298 ******************************************************************
05299          MOVE LOW-VALUES             TO  CL-NEXT-AUTO-PAY-DT      
05300          MOVE AT-SCHEDULE-END-DT     TO  AT-TERMINATED-DT         
05301          MOVE ZEROS                  TO  CL-AUTO-PAY-SEQ          
05302 ***************************************************************   
05303 ***            'DMD' ONLY  - PROJECT 6449                    **   
05304 *** CREATES A STATISTIC RECORD WHEN TERMINATING AUTO PAYMENTS**   
05305 ***************************************************************   
121902*        IF WS-COMPANY-ID = 'DMD'                                 
121902*            MOVE 'Y'                TO WS-DMD-ELSTAT-SW          
121902*            MOVE '12'               TO WS-DMD-MAINTENANCE-TYPE   
121902*            MOVE BIN-RUN-DATE       TO WS-DMD-MAINTENANCE-DT     
121902*            MOVE AT-RECORDED-BY     TO WS-DMD-PROCESSOR-ID       
121902*            PERFORM 9500-DMD-ACTIVITY-FILE THRU 9500-EXIT        
121902*            GO TO 2517-AUTO-PAYMENTS                             
121902*        ELSE                                                     
05314              GO TO 2517-AUTO-PAYMENTS.                            
05315                                                                   
05316      IF WS-WORK-DD1 GREATER WS-WORK-DD2                           
05317          MOVE WS-WORK-DD1        TO  WS-WORK-DD2                  
05318          MOVE WS-WORK-DATE2      TO  DC-GREG-DATE-1-MDY           
05319          MOVE '4'                TO  DC-OPTION-CODE               
05320          PERFORM 8500-DATE-CONVERSION                             
05321          MOVE DC-BIN-DATE-1      TO  DC-BIN-DATE-2.               
05322                                                                   
05323      IF DC-BIN-DATE-2 LESS THAN AT-SCHEDULE-END-DT                
05324          MOVE DC-BIN-DATE-2      TO  CL-NEXT-AUTO-PAY-DT          
05325        ELSE                                                       
05326          MOVE AT-SCHEDULE-END-DT TO  CL-NEXT-AUTO-PAY-DT.         
05327                                                                   
05328      IF CL-PAID-THRU-DT = AT-SCHEDULE-END-DT                      
05329          MOVE LOW-VALUES         TO  CL-NEXT-AUTO-PAY-DT          
05330          MOVE AT-SCHEDULE-END-DT TO  AT-TERMINATED-DT             
05331          MOVE ZEROS              TO  CL-AUTO-PAY-SEQ.             
05332                                                                   
05333  2517-AUTO-PAYMENTS.                                              
05334                                                                   
05335      MOVE ' '                    TO DC-END-OF-MONTH.              
05336      MOVE AT-AUTO-PAYEE-CD       TO  WS-AUTO-PAYEE-CODE.          
05337                                                                   
05338      PERFORM 8100-REWRITE-TRAILER.                                
05339                                                                   
05340 ****** CREATES A DMO RECORD FOR AUTO PAYMENTS CONSIDERED TO BE    
05341 ****** FINAL PAYMENT                                              
121902*    IF WS-COMPANY-ID = 'DMD'                                     
121902*       IF AT-LAST-PMT-TYPE = 'F'                                 
121902*            MOVE 'F'          TO WS-DMD-DMO-ACTIVITY             
121902*            PERFORM 9501-CREATE-DMO THRU 9501-EXIT.              
05346                                                                   
05347      EJECT                                                        
05348 * GET ADDRESS                                                     
05349                                                                   
05350      IF WS-AUTO-PAYEE-SEQ-NUM NOT NUMERIC                         
05351         DISPLAY  AT-CONTROL-PRIMARY                               
05352         MOVE 1                     TO WS-AUTO-PAYEE-SEQ-NUM.      
05353                                                                   
05354      IF WS-AUTO-PAYEE-CD = 'I'                                    
05355         MOVE WS-AUTO-PAYEE-SEQ-NUM TO AT-SEQUENCE-NO              
05356         GO TO 2520-AUTO-PAYMENTS.                                 
05357                                                                   
05358      IF WS-AUTO-PAYEE-CD = 'O'                                    
05359         MOVE WS-AUTO-PAYEE-SEQ-NUM TO  AT-SEQUENCE-NO             
05360         ADD +50    TO AT-SEQUENCE-NO                              
05361         GO TO 2520-AUTO-PAYMENTS.                                 
05362                                                                   
05363      IF WS-AUTO-PAYEE-CD = 'Q'                                    
05364         MOVE WS-AUTO-PAYEE-SEQ-NUM TO  AT-SEQUENCE-NO             
05365         ADD +60    TO AT-SEQUENCE-NO                              
05366         GO TO 2520-AUTO-PAYMENTS.                                 
05367                                                                   
05368      IF WS-AUTO-PAYEE-CD = 'B'                                    
05369          IF CL-BENIF-ADDR-CNT NOT = +0                            
041712           AND WS-AUTO-PAYEE-SEQ-NUM > 0
05370              MOVE WS-AUTO-PAYEE-SEQ-NUM  TO  AT-SEQUENCE-NO       
05371              ADD +10                     TO  AT-SEQUENCE-NO       
05372              GO TO 2520-AUTO-PAYMENTS                             
05373          ELSE                                                     
05374              PERFORM 2450-GET-BENEFICIARY  THRU  2450-EXIT        
05375              IF WS-BENE-NOT-FOUND = ZERO                          
05376                  MOVE BE-MAIL-TO-NAME    TO  WS-PAYEE-NAME  
05377                  GO TO 2540-AUTO-PAYMENTS                         
05378              ELSE                                                 
05379                  MOVE '*'                TO  EX-DA-ERROR-FLAG (2) 
05380                  GO TO 2540-AUTO-PAYMENTS.                        
05381                                                                   
05382      IF WS-AUTO-PAYEE-CD = 'A'                                    
05383         IF CL-ACCOUNT-ADDR-CNT NOT = +0                           
05384            MOVE WS-AUTO-PAYEE-SEQ-NUM TO  AT-SEQUENCE-NO          
05385            ADD +20         TO  AT-SEQUENCE-NO                     
05386            GO TO 2520-AUTO-PAYMENTS                               
05387         ELSE                                                      
05388             NEXT SENTENCE                                         
05389      ELSE                                                         
05390          MOVE '*'                TO  EX-DA-ERROR-FLAG (1)         
05391          GO TO 2540-AUTO-PAYMENTS.                                
05392                                                                   
121902*    IF CL-SYSTEM-IDENTIFIER = 'CV'                               
121902*        IF WS-PRODUCER-NOT-FOUND = ZERO                          
121902*            MOVE PD-NAME        TO  WS-PAYEE-NAME                
121902*        ELSE                                                     
121902*            MOVE '*'            TO  EX-DA-ERROR-FLAG (3)         
121902*    ELSE                                                         
05399 *        PERFORM 2300-GET-ACCOUNT.                                 
05400          IF WS-ACCOUNT-NOT-FOUND = ZERO                           
05401              MOVE AM-NAME        TO  WS-PAYEE-NAME                
05402          ELSE                                                     
05403              MOVE '*'            TO  EX-DA-ERROR-FLAG (3).        
05404                                                                   
05405  2520-AUTO-PAYMENTS.                                              
05406                                                                   
05407      PERFORM 8300-READ-TRAILER.                                   
05408                                                                   
05409      IF WS-TRAILER-NOT-FOUND NOT = ZERO                           
05410          MOVE '*'                TO  EX-DA-ERROR-FLAG (2)         
05411          GO TO 2540-AUTO-PAYMENTS.                                
05412                                                                   
05413      IF AT-TRAILER-TYPE NOT = '5'                                 
05414          MOVE '*'                TO  EX-DA-ERROR-FLAG (2)         
05415          GO TO 2540-AUTO-PAYMENTS.                                
05416                                                                   
05417      MOVE AT-MAIL-TO-NAME        TO  WS-PAYEE-NAME.        
05418                                  EJECT                            
05419  2540-AUTO-PAYMENTS.                                              
05420                                                                   
05421 * CREATE PAYMENT TRAILER                                          
05422                                                                   
05423      SUBTRACT +1 FROM CL-TRAILER-SEQ-CNT.                         
05424      MOVE CL-TRAILER-SEQ-CNT     TO  AT-SEQUENCE-NO.              
05425                                                                   
05426      READ ELTRLR INTO WS-SAVE-ACTIVITY-TRAILERS.                  
05427                                                                   
05428      IF ELTRLR-FILE-STATUS = '23'                                 
05429         NEXT SENTENCE                                             
05430      ELSE                                                         
05431         DISPLAY SPACES                                            
05432         DISPLAY 'BAD PMT PNTR    ' AT-CONTROL-PRIMARY UPON CONSOLE
05433         DISPLAY 'BAD PMT PNTR    ' AT-CONTROL-PRIMARY             
05434         DISPLAY SPACES                                            
CIDMOD                                                                  
CIDMOD         ADD  +1  TO  ERROR-COUNT                                 
CIDMOD         MOVE 'NO ACTV TRLR - KEY IS = ' TO DIS-LINE-REASON       
CIDMOD         MOVE AT-CONTROL-PRIMARY         TO  DIS-LINE-REC         
CIDMOD         PERFORM 8600-DISPLAY-PRT THRU                            
CIDMOD               8600-DISPLAY-EXIT                                  
CIDMOD         MOVE SPACES TO DISPLAY-LINE                              
CIDMOD         PERFORM 8600-DISPLAY-PRT THRU                            
CIDMOD               8600-DISPLAY-EXIT                                  
CIDMOD                                                                  
05435         GO TO 2540-AUTO-PAYMENTS.                                 
05436                                                                   
05437 *************************************************************     
05438 ***     CLIENT CODED FOR 'DMD'  - PROJECT 6436            ***     
05439 ***  READS THE DCT TABLE TO DETERMINE WHETHER THIS PAYMENT***     
05440 ***  WILL BE CASH OR NON-CASH.                            ***     
05441 *************************************************************     
05442                                                                   
121902*    IF WS-COMPANY-ID = 'DMD'                                     
121902*        PERFORM 9502-GET-DCT THRU 9502-EXIT                      
121902*        IF DCT-RETURN-CODE NOT = 'OK' AND '01' AND '02'          
121902*            GO TO 2590-EXIT.                                     
05447                                                                   
05448      MOVE BIN-RUN-DATE           TO  AT-RECORDED-DT               
05449                                      AT-PAYMENT-LAST-MAINT-DT.    
05450      MOVE 'AUTO'                 TO  AT-RECORDED-BY.              
05451      MOVE WS-CURRENT-TIME        TO  AT-LAST-MAINT-HHMMSS.        
05452      MOVE '2'                    TO  AT-TRAILER-TYPE.             
05453                                                                   
05454      MOVE SPACES                 TO  AT-PAYMENT-TR.               
05455                                                                   
05456      MOVE EX-DA-PAYMENT-TYPE     TO  AT-PAYMENT-TYPE.             
05457      MOVE CL-CLAIM-TYPE          TO  AT-CLAIM-TYPE.               
05458      MOVE EX-DA-CLAIM-PREM-TYPE  TO  AT-CLAIM-PREM-TYPE.          
05459      MOVE EX-DA-PAYMENT-AMOUNT   TO  AT-AMOUNT-PAID.              
05460      MOVE EX-DA-CHECK-NUMBER     TO  AT-CHECK-NO.                 
05461      MOVE WS-PAID-FROM-DT        TO  AT-PAID-FROM-DT.             
05462      MOVE CL-PAID-THRU-DT        TO  AT-PAID-THRU-DT.             
05463      MOVE WS-DAYS-IN-PERIOD      TO  AT-DAYS-IN-PERIOD.           
05464      MOVE WS-AUTO-PAYEE-CODE     TO  AT-PAYEE-TYPE-CD.            
05465      MOVE WS-PAYEE-NAME          TO  AT-PAYEES-NAME.              
05466      MOVE '2'                    TO  AT-PAYMENT-ORIGIN.           
080510     MOVE 'Y'                    TO  AT-PRINT-EOB-WITH-CHECK.

010918     IF BE-ON-ACH
010918        MOVE 'Y' TO AT-ACH-PAYMENT
010918     ELSE
010918        MOVE SPACES TO AT-ACH-PAYMENT
010918     END-IF
05467                                                                   
081216     IF WS-COMPANY-ID = 'DCC'
081216        MOVE 'N' TO AT-PRINT-SURVEY
081216     END-IF.

121902*    IF WS-COMPANY-ID = 'AIG' OR 'AUK'
121902*        MOVE CL-ASSOCIATES  TO  AT-ASSOCIATES.                   
05470                                                                   
05471      IF WS-AUTO-CASH = 'N'                                        
05472          MOVE 'N'                TO  AT-CASH-PAYMENT              
05473      ELSE                                                         
05474          MOVE 'Y'                TO  AT-CASH-PAYMENT.             
05475                                                                   
05476      MOVE WS-SAVE-LAST-DT        TO  AT-PREV-LAST-PMT-DT.         
05477      MOVE WS-SAVE-PAID-DT        TO  AT-PREV-PAID-THRU-DT.        
05478      MOVE WS-SAVE-PMT-AMT        TO  AT-PREV-LAST-PMT-AMT.        
05479                                                                   
05480      MOVE LOW-VALUES             TO  AT-CHECK-WRITTEN-DT          
05481                                      AT-TO-BE-WRITTEN-DT          
05482                                      AT-VOID-DT                   
05483                                      AT-PMT-ACCEPT-DT             
05484                                      AT-PMT-SELECT-DT             
05485                                      AT-VOID-SELECT-DT            
05486                                      AT-VOID-ACCEPT-DT.           
05487                                                                   
05488      IF AT-CASH-PAYMENT = 'N'                                     
05489          MOVE WS-MONTH-END-DATE  TO  AT-PMT-SELECT-DT.            
05490                                                                   
05491      MOVE ZERO                   TO  AT-ADDL-RESERVE              
05492                                      AT-ELIMINATION-DAYS          
05493                                      AT-EXPENSE-PER-PMT.          
05494                                                                   
05495      MOVE WS-DAILY-RATE          TO  AT-DAILY-RATE.               
05496                                                                   
05497      IF CF-EXPENSE-METHOD = '2'                                   
05498        MOVE CF-EXPENSE-DOLLAR  TO  AT-EXPENSE-PER-PMT             
05499      ELSE                                                         
05500        IF CF-EXPENSE-METHOD = '3'                                 
05501          MULTIPLY AT-AMOUNT-PAID BY CF-EXPENSE-PERCENT            
05502                                  GIVING AT-EXPENSE-PER-PMT        
05503        ELSE                                                       
05504          IF CF-EXPENSE-METHOD = '4'                               
05505            MULTIPLY EX-DA-INTERVAL-MONTHS BY CF-EXPENSE-DOLLAR    
05506                                  GIVING AT-EXPENSE-PER-PMT.       
05507                                                                   
05508      MOVE ZERO                   TO  AT-CHECK-QUE-CONTROL         
05509                                      AT-CHECK-QUE-SEQUENCE        
05510                                      EX-SF-DAYS-TO-PAYMENT.       
05511                                                                   
05512      IF WS-PMT-APPROVAL-USED                                      
05513         MOVE 'A'                 TO AT-PAYMENT-APPROVAL-SW.       
05514                                  EJECT                            
05515 *    NOTE ******************************************************* 
05516 *         *      UPDATE THE ACTIVITY QUEUE RECORD FOR THE       * 
05517 *         *  PAYMENT THAT WAS GENERATED.                        * 
05518 *         *******************************************************.
05519                                                                   
05520      MOVE AT-CONTROL-PRIMARY     TO  AQ-CONTROL-PRIMARY.          
05521                                                                   
05522      READ ELACTQ INTO WS-SAVE-ACTIVITY-QUEUE.                     
05523                                                                   
05524      IF ELACTQ-FILE-STATUS = '23'                                 
05525          GO TO 2550-AUTO-PAYMENTS.                                
05526                                                                   
05527      IF ELACTQ-FILE-STATUS NOT = ZERO                             
05528          MOVE 'ERROR OCCURRED READ - ELACTQ' TO  WS-ABEND-MESSAGE 
05529          MOVE ELACTQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
05530          GO TO ABEND-PGM.                                         
05531                                                                   
05532      GO TO 2560-AUTO-PAYMENTS.                                    
05533                                  EJECT                            
05534  2550-AUTO-PAYMENTS.                                              
05535                                                                   
05536      MOVE 'AQ'                   TO  ACTIVITY-QUE.                
05537                                                                   
05538      MOVE AT-CONTROL-PRIMARY     TO  AQ-CONTROL-PRIMARY.          
05539      MOVE '1'                    TO  AQ-PENDING-PAYMENT-FLAG.     
05540      MOVE +1                     TO  AQ-PAYMENT-COUNTER.          
05541      MOVE ZERO                   TO  AQ-PMT-UNAPPROVED-COUNT.     
05542                                                                   
05543      MOVE SPACES                 TO  AQ-PENDING-STATUS-FLAG       
05544                                      AQ-PENDING-LETTER-FLAG       
05545                                      AQ-AUTO-LETTER.              
05546                                                                   
05547      MOVE LOW-VALUES             TO  AQ-RESEND-DATE               
05548                                      AQ-FOLLOWUP-DATE.            
05549                                                                   
05550      MOVE +3100                  TO  AQ-LAST-UPDATED-BY.          
05551                                                                   
05552      MOVE +20                    TO SLR-KEY-LENGTH.               
05553      MOVE +60                    TO SLR-RECORD-LENGTH.            
05554      MOVE AQ-CONTROL-PRIMARY     TO SLR-KEY.                      
05555      MOVE ACTIVITY-QUE           TO SLR-RECORD-IMAGE.             
05556      MOVE 'ELACTQ  '             TO SLR-DSID.                     
05557      MOVE 'A'                    TO SLR-ACTION.                   
05558                                                                   
05559      IF WS-AUTO-ACTIVITY-SW = 'N'                                 
05560         GO TO 2555-WRITE-ELACTQ.                                  
05561                                                                   
05562      IF AT-PAYMENT-TYPE = '1'                                     
05563         MOVE +2                  TO AAR-SUB                       
05564      ELSE                                                         
05565      IF AT-PAYMENT-TYPE = '2'                                     
05566         MOVE +6                  TO AAR-SUB                       
05567      ELSE                                                         
05568         GO TO 2555-WRITE-ELACTQ.                                  
05569                                                                   
05570      IF CL-NEXT-AUTO-PAY-DT = LOW-VALUES                          
05571          MOVE +6                 TO AAR-SUB.                      
05572                                                                   
121902*    IF WS-COMPANY-ID = 'HAN'                                     
121902*        MOVE +2                 TO AAR-SUB.                      
05575                                                                   
121902*    IF (WS-COMPANY-ID = 'AIG' OR 'AUK' OR 'CIG' OR 'CUK')        
121902*        IF AT-PAID-THRU-DT NOT LESS THAN WS-EXPIRE-DT            
121902*            MOVE +3             TO  AAR-SUB.                     
05579                                                                   
05580      IF (WS-SYS-ACTIVE-SW (AAR-SUB) = 'N' OR ' ')                 
05581          GO TO 2555-WRITE-ELACTQ.            
070909
070909     IF AAR-SUB = +6 AND WS-AUTO-END-LETTER = 'N   '
070909         GO TO 2555-WRITE-ELACTQ
070909     END-IF.                     
05582                                                                   
05583      IF (WS-SYS-LETTER-ID (AAR-SUB) = SPACES OR LOW-VALUES)       
05584          GO TO 2555-WRITE-ELACTQ.                                 
05585                                                                   
05586      MOVE '1'                    TO AQ-PENDING-LETTER-FLAG.
070909     IF AAR-SUB = +6  AND
070909        WS-AUTO-END-LETTER GREATER THAN SPACES
070909           MOVE WS-AUTO-END-LETTER TO AQ-AUTO-LETTER
070909     ELSE       
05587      MOVE WS-SYS-LETTER-ID (AAR-SUB)                              
05588                                  TO AQ-AUTO-LETTER.               
05589      MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1.                
05590      MOVE +0                     TO DC-ELAPSED-MONTHS.            
05591                                                                   
05592      IF WS-SYS-RESEND-DAYS (AAR-SUB) NOT = ZEROS                  
05593         MOVE WS-SYS-RESEND-DAYS (AAR-SUB)                         
05594                                  TO DC-ELAPSED-DAYS               
05595         MOVE '6'                 TO DC-OPTION-CODE                
05596         PERFORM 8500-DATE-CONVERSION                              
05597         IF NO-CONVERSION-ERROR                                    
05598            MOVE DC-BIN-DATE-2    TO AQ-RESEND-DATE.               
05599                                                                   
05600      IF WS-SYS-FOLLOW-UP-DAYS (AAR-SUB) NOT = ZEROS               
05601         MOVE WS-SYS-FOLLOW-UP-DAYS (AAR-SUB)                      
05602                                  TO DC-ELAPSED-DAYS               
05603         MOVE '6'                 TO DC-OPTION-CODE                
05604         PERFORM 8500-DATE-CONVERSION                              
05605         IF NO-CONVERSION-ERROR                                    
05606            MOVE DC-BIN-DATE-2    TO AQ-FOLLOWUP-DATE.             
05607                                                                   
05608  2555-WRITE-ELACTQ.                                               
05609                                                                   
05610      WRITE ACTIVITY-QUE.                                          
05611                                                                   
05612      IF ELACTQ-FILE-STATUS = ZERO                                 
05613 *       PERFORM LOG-JOURNAL-RECORD                                
05614         GO TO 2570-AUTO-PAYMENTS.                                 
05615                                                                   
05616      MOVE 'ERROR OCCURRED WRITE - ELACTQ' TO  WS-ABEND-MESSAGE    
05617      MOVE ELACTQ-FILE-STATUS     TO  WS-ABEND-FILE-STATUS         
05618      GO TO ABEND-PGM.                                             
05619                                                                   
05620  2560-AUTO-PAYMENTS.                                              
05621                                                                   
05622      ADD +1  TO  AQ-PAYMENT-COUNTER.                              
05623      MOVE '1'                    TO  AQ-PENDING-PAYMENT-FLAG.     
05624                                                                   
05625      IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC                       
05626         MOVE +0                  TO AQ-PMT-UNAPPROVED-COUNT.      
05627                                                                   
05628      MOVE +3100                  TO  AQ-LAST-UPDATED-BY.          
05629                                                                   
05630      MOVE +20                    TO SLR-KEY-LENGTH.               
05631      MOVE +60                    TO SLR-RECORD-LENGTH.            
05632      MOVE AQ-CONTROL-PRIMARY     TO SLR-KEY.                      
05633      MOVE ACTIVITY-QUE           TO SLR-RECORD-IMAGE.             
05634      MOVE 'ELACTQ  '             TO SLR-DSID.                     
05635      MOVE 'C'                    TO SLR-ACTION.                   
05636                                                                   
05637      IF WS-AUTO-ACTIVITY-SW = 'N'                                 
05638         GO TO 2565-REWRITE-ELACTQ.                                
05639                                                                   
05640      IF AT-PAYMENT-TYPE = '1'                                     
05641         MOVE +2                  TO AAR-SUB                       
05642      ELSE                                                         
05643      IF AT-PAYMENT-TYPE = '2'                                     
05644         MOVE +6                  TO AAR-SUB                       
05645      ELSE                                                         
05646         GO TO 2565-REWRITE-ELACTQ.                                
05647                                                                   
05648      IF CL-NEXT-AUTO-PAY-DT = LOW-VALUES                          
05649          MOVE +6                 TO AAR-SUB.                      
05650                                                                   
121902*    IF WS-COMPANY-ID = 'HAN'                                     
121902*        MOVE +2                 TO AAR-SUB.                      
05653                                                                   
121902*    IF (WS-COMPANY-ID = 'AIG' OR 'AUK' OR 'CIG' OR 'CUK')        
121902*        IF AT-PAID-THRU-DT NOT LESS THAN WS-EXPIRE-DT            
121902*            MOVE +3             TO  AAR-SUB.    
05657                                                                   
05658      IF (WS-SYS-ACTIVE-SW (AAR-SUB) = 'N' OR ' ')                 
05659          GO TO 2565-REWRITE-ELACTQ.                               
070909
070909     IF AAR-SUB = +6 AND WS-AUTO-END-LETTER = 'N   '
070909         GO TO 2565-REWRITE-ELACTQ
070909     END-IF.                 
05660                                                                   
05661      IF (WS-SYS-LETTER-ID (AAR-SUB) = SPACES OR LOW-VALUES)       
05662          GO TO 2565-REWRITE-ELACTQ.                               
05663                                                                   
05664      MOVE '1'                    TO AQ-PENDING-LETTER-FLAG.       
070909     IF AAR-SUB = +6  AND
070909        WS-AUTO-END-LETTER GREATER THAN SPACES
070909           MOVE WS-AUTO-END-LETTER TO AQ-AUTO-LETTER
070909     ELSE       
05665      MOVE WS-SYS-LETTER-ID (AAR-SUB)                              
05666                                  TO AQ-AUTO-LETTER.               
05667      MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1.                
05668      MOVE +0                     TO DC-ELAPSED-MONTHS.            
05669                                                                   
05670      IF WS-SYS-RESEND-DAYS (AAR-SUB) NOT = ZEROS                  
05671         MOVE WS-SYS-RESEND-DAYS (AAR-SUB)                         
05672                                  TO DC-ELAPSED-DAYS               
05673         MOVE '6'                 TO DC-OPTION-CODE                
05674         PERFORM 8500-DATE-CONVERSION                              
05675         IF NO-CONVERSION-ERROR                                    
05676            MOVE DC-BIN-DATE-2    TO AQ-RESEND-DATE.               
05677                                                                   
05678      IF WS-SYS-FOLLOW-UP-DAYS (AAR-SUB) NOT = ZEROS               
05679         MOVE WS-SYS-FOLLOW-UP-DAYS (AAR-SUB)                      
05680                                  TO DC-ELAPSED-DAYS               
05681         MOVE '6'                 TO DC-OPTION-CODE                
05682         PERFORM 8500-DATE-CONVERSION                              
05683         IF NO-CONVERSION-ERROR                                    
05684            MOVE DC-BIN-DATE-2    TO AQ-FOLLOWUP-DATE.             
05685                                                                   
05686  2565-REWRITE-ELACTQ.                                             
05687                                                                   
05688      REWRITE ACTIVITY-QUE.                                        
05689                                                                   
05690      IF ELACTQ-FILE-STATUS NOT = ZERO                             
05691          MOVE 'ERROR OCCURRED REWRITE -ELACTQ' TO WS-ABEND-MESSAGE
05692          MOVE ELACTQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
05693          GO TO ABEND-PGM.                                         
05694                                                                   
05695 *    PERFORM LOG-JOURNAL-RECORD.                                  
05696                                  EJECT                            
05697  2570-AUTO-PAYMENTS.                                              

061013     perform 2460-update-elcrtt  thru 2460-exit

05699      PERFORM 8700-WRITE-TRAILER.                                  
05700                                                                   
05701      IF WS-AUTO-ACTIVITY-SW = 'N'                                 
05702          GO TO 2575-AUTO-PAYMENTS.                                
05703                                                                   
05704       IF CL-ACTIVITY-CODE = 00                                    
05705          GO TO 2575-AUTO-PAYMENTS.                                
05706                                                                   
05707       MOVE CL-ACTIVITY-CODE          TO  AAR-SUB-1.               
05708                                                                   
05709       IF AAR-SUB-1 IS GREATER THAN 9                              
05710           SUBTRACT +9 FROM AAR-SUB-1                              
05711           IF (WS-USER-RESET-SW (AAR-SUB-1) NOT = 'N' AND ' ')     
05712               IF WS-SYS-ACTIVE-SW (AAR-SUB) = 'Y'                 
05713                   MOVE AAR-SUB       TO  CL-ACTIVITY-CODE         
05714                   MOVE BIN-RUN-DATE  TO  CL-ACTIVITY-MAINT-DT     
05715               ELSE                                                
05716                   NEXT SENTENCE                                   
05717           ELSE                                                    
05718               NEXT SENTENCE                                       
05719       ELSE                                                        
05720           IF (WS-SYS-RESET-SW (AAR-SUB-1) NOT = 'N' AND ' ')      
05721              IF WS-SYS-ACTIVE-SW (AAR-SUB) = 'Y'                  
05722                  MOVE AAR-SUB        TO  CL-ACTIVITY-CODE         
05723                  MOVE BIN-RUN-DATE   TO  CL-ACTIVITY-MAINT-DT.    
05724                                                                   
05725  2575-AUTO-PAYMENTS.                                              
05726                                                                   
05727      MOVE 'AUTO'                 TO  CL-LAST-MAINT-USER.          
05728      MOVE BIN-RUN-DATE           TO  CL-LAST-MAINT-DT.            
05729      MOVE WS-CURRENT-TIME        TO  CL-LAST-MAINT-HHMMSS.        
05730      MOVE '1'                    TO  CL-LAST-MAINT-TYPE.          
05731                                                                   
DAN01 ************************************************
020816*    IF WS-COMPANY-ID = 'CID' OR 'DCC' or 'VPP'
CIDMOD*       MOVE 'P'                 TO  WS-CSO-ACTION-TYPE           
CIDMOD*       PERFORM 9300-CSO-SPECIAL-PROCESSING
CIDMOD*                                THRU 9300-EXIT
CIDMOD*    END-IF
CIDMOD*                                                                 
05732 ******************************************************************
05733 ****   THE FOLLOWING STATEMENT PRODUCES A CONTINUING FORM IF THE  
05734 ****   LAST PAYMENT OF THE SCHEDULE WAS MADE AND IF THE LAST      
05735 ****   PAYMENT WAS A PARTIAL PAYMENT.                             
05736 ******************************************************************
05737                                                                   
121902*    IF WS-COMPANY-ID = 'LGX' OR 'CRI' OR 'LAP' OR 'RMC'          
121902*       IF CL-AUTO-PAY-SEQ = +0                                   
121902*          IF EX-DA-PAYMENT-TYPE = '1'                            
121902*             PERFORM 2582-BUILD-FORM-TRAILER                     
121902*             IF WS-COMPANY-ID NOT = 'LAP' AND 'RMC'              
121902*                 PERFORM 2587-BUILD-ARCHIVE-HEADER.              
05744                                                                   
05745      IF EX-DA-PAYMENT-TYPE = '2'                                  
DAN01***       PERFORM 7000-RELEASE-RECORD                              
DAN01***       ADD +1  TO  WS-EXTRACT-DA-COUNT                          
05748          IF CLAIM-IS-OPEN                                         
05749             PERFORM 2000-CLOSE-CLAIM                              
05750             GO TO 2590-EXIT                                       
05751          ELSE                                                     
05752             GO TO 2590-EXIT.                                      
05753                                                                   
05754  2580-AUTO-PAYMENTS.                                              
05755                                                                   
DAN01***   PERFORM 7000-RELEASE-RECORD.                                 
05757                                                                   
DAN01***   ADD +1                      TO  WS-EXTRACT-DA-COUNT.         
05759      GO TO 2590-EXIT.                                             
05760                                                                   
05761  2582-BUILD-FORM-TRAILER.                                         
05762                                                                   
05763      SUBTRACT +1  FROM CL-TRAILER-SEQ-CNT.                        
05764                                                                   
05765      MOVE SPACES                 TO ACTIVITY-TRAILERS.            
05766                                                                   
05767      MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY.           
05768      MOVE 'AT'                   TO AT-RECORD-ID.                 
05769      MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO.               
05770      MOVE 'A'                    TO AT-TRAILER-TYPE.              
05771      MOVE BIN-RUN-DATE           TO AT-RECORDED-DT                
05772                                     AT-FORM-LAST-MAINT-DT         
05773                                     AT-FORM-SEND-ON-DT.           
05774      MOVE 'EL310'                TO AT-RECORDED-BY                
05775                                     AT-FORM-LAST-UPDATED-BY.      
05776      MOVE WS-CURRENT-TIME        TO AT-LAST-MAINT-HHMMSS.         
05777      MOVE WS-CURRENT-DATE-PLUS-31 TO AT-FORM-FOLLOW-UP-DT.        
05778                                                                   
05779      MOVE LOW-VALUES             TO AT-FORM-ANSWERED-DT           
05780                                     AT-FORM-RE-SEND-DT            
05781                                     AT-EMP-FORM-ANSWERED-DT       
05782                                     AT-PHY-FORM-ANSWERED-DT       
05783                                     AT-EMP-FORM-SEND-ON-DT        
05784                                     AT-PHY-FORM-SEND-ON-DT        
05785                                     AT-FORM-PRINTED-DT            
05786                                     AT-FORM-REPRINT-DT            
05787                                     AT-FORM-REM-PRINT-DT.         
05788                                                                   
05789      MOVE '2'                    TO AT-FORM-TYPE.                 
05790                                                                   
05791      MOVE CL-INSURED-ADDR-CNT    TO AT-FORM-ADDR-SEQ-NO.          
05792      MOVE 'I'                    TO AT-FORM-ADDRESS.              
05793                                                                   
05794      IF CL-PROG-FORM-TYPE NOT = 'S'                               
05795         MOVE BIN-RUN-DATE        TO AT-PHY-FORM-SEND-ON-DT.       
05796                                                                   
05797      PERFORM 8700-WRITE-TRAILER.                                  
05798                                                                   
05799  2587-BUILD-ARCHIVE-HEADER.                                       
05800                                                                   
05801      ADD +1                      TO WS-ARCHIVE-COUNTER.           
05802      MOVE 'LA'                   TO LA-RECORD-ID.                 
05803      MOVE WS-ARCHIVE-COUNTER     TO LA-ARCHIVE-NO                 
05804                                     LA-ARCHIVE-NO-A1.             
05805      MOVE '4'                    TO LA-RECORD-TYPE                
05806                                     LA-RECORD-TYPE-A1.            
05807      MOVE +0                     TO LA-LINE-SEQ-NO                
05808                                     LA-LINE-SEQ-NO-A1.            
05809      MOVE WS-COMPANY-CODE        TO LA-COMPANY-CD                 
05810                                     LA-COMPANY-CD-A1.             
05811      MOVE CL-CARRIER             TO LA4-CARRIER.                  
05812      MOVE CL-CLAIM-NO            TO LA4-CLAIM-NO.                 
05813      MOVE CL-CERT-NO             TO LA4-CERT-NO.                  
05814      MOVE +0                     TO LA4-NO-OF-COPIES.             
05815      MOVE 'EL310'                TO LA4-PROCESSOR-CD.             
05816      MOVE BIN-RUN-DATE           TO LA4-CREATION-DT.              
05817      MOVE LOW-VALUES             TO LA4-RESEND-DATE               
05818                                     LA4-INITIAL-PRINT-DATE        
05819                                     LA4-RESEND-PRINT-DATE         
05820                                     LA4-FORM-REM-PRINT-DT.        
05821      MOVE CL-TRAILER-SEQ-CNT     TO LA4-FORM-TRLR-SEQ.            
05822      MOVE '2'                    TO LA4-FORM-TYPE.                
05823                                                                   
05824      WRITE LETTER-ARCHIVE.                                        
05825                                                                   
05826      IF ELARCH-FILE-STATUS NOT = '00'                             
05827         MOVE 'ERROR OCCURRED WRITE - ELARCH'                      
05828                           TO WS-ABEND-MESSAGE                     
05829         MOVE ELARCH-FILE-STATUS TO WS-ABEND-FILE-STATUS           
05830         GO TO ABEND-PGM.                                          
05831                                                                   
05832  2590-EXIT.                                                       
05833      EXIT.                                                        
05834                                  EJECT                            
05835  2600-GET-BENEFIT SECTION.                                        
05836 *    NOTE ******************************************************* 
05837 *         *      THIS SECTION SEARCHES THE BENEFIT TABLES TO    * 
05838 *         *  GET THE BENEFIT TABLE ENTRY FOR THE CLAIM BEING    * 
05839 *         *  PROCESSED.                                         * 
05840 *         *******************************************************.
05841                                                                   
05842      MOVE ZERO                   TO  WS-REM-TERM-METHOD           
05843                                      WS-EARNING-METHOD            
05844                                      WS-NO-BENEFIT.               
05845      MOVE SPACES                 TO  WS-COVERAGE-TYPE             
05846                                      WS-SPECIAL-CALC-CODE.        
05847                                                                   
052614     IF CL-CLAIM-TYPE = WS-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
022122         OR 'B' OR 'H'
05849         SET BENEFIT-INDEX1 TO +2                                  
05850      ELSE                                                         
05851         SET BENEFIT-INDEX1 TO +1.                                 
05852                                                                   
05853      SET BENEFIT-INDEX2 TO +1.                                    
05854                                                                   
05855  2620-GET-BENEFIT.                                                
05856                                                                   
052614     IF ((CL-CLAIM-TYPE = WS-AH-OVERRIDE-L1 OR 'I' OR 'G' 
022122          OR 'F' OR 'B' OR 'H') AND
05858          CM-AH-BENEFIT-CD =                                       
05859              WS-BENEFIT-CODE (BENEFIT-INDEX1, BENEFIT-INDEX2))    
05860                         OR                                        
100518        ((CL-CLAIM-TYPE = WS-LIFE-OVERRIDE-L1 OR 'O') AND
05862          CM-LF-BENEFIT-CD =                                       
05863              WS-BENEFIT-CODE (BENEFIT-INDEX1, BENEFIT-INDEX2))    
05864           MOVE WS-CO-REM-TERM-CALC (BENEFIT-INDEX1 BENEFIT-INDEX2)
05865                                  TO  WS-REM-TERM-METHOD           
05866           MOVE WS-CO-EARNINGS-CALC (BENEFIT-INDEX1 BENEFIT-INDEX2)
05867                                  TO  WS-EARNING-METHOD            
05868           MOVE WS-LF-COVERAGE-TYPE (BENEFIT-INDEX1 BENEFIT-INDEX2)
05869                                  TO  WS-COVERAGE-TYPE             
05870           MOVE WS-SPECIAL-CALC-CD  (BENEFIT-INDEX1 BENEFIT-INDEX2)
05871                                  TO  WS-SPECIAL-CALC-CODE         
05872           GO TO 2690-EXIT.                                        
05873                                                                   
05874      IF BENEFIT-INDEX2 LESS THAN WS-BENEFIT-MAX (BENEFIT-INDEX1)  
05875          SET BENEFIT-INDEX2 UP BY +1                              
05876          GO TO 2620-GET-BENEFIT.                                  
05877                                                                   
05878      MOVE +1                     TO  WS-NO-BENEFIT.               
05879                                                                   
05880      MOVE 'BENEFIT NOT FOUND'    TO  WS-D1-MESSAGE.               
05881      MOVE WS-DETAIL1             TO  PRT.                         
05882      PERFORM WRITE-A-LINE.                                        
05883      MOVE SPACES                 TO  WS-DETAIL1.                  
05884                                                                   
05885  2690-EXIT.                                                       
05886      EXIT.                                                        
05887                                  EJECT                            
05888  2700-PROCESS-CHECK-QUEUE SECTION.                                
05889 *    NOTE ******************************************************* 
05890 *         *      THIS SECTION PROCESSES THE CHECK QUEUE TO      * 
05891 *         *  PRODUCE THE CHECKS TO BE PRINTED REPORT AND PURGE  * 
05892 *         *  THE EXPIRED RECORDS FROM THE CHECK QUEUE.          * 
05893 *         *******************************************************.
05894                                                                   
05895      MOVE +1                     TO  WS-ELCHKQ-SW.                
05896                                                                   
05897      MOVE LOW-VALUES             TO  CQ-CONTROL-PRIMARY           
05898      MOVE WS-COMPANY-CODE        TO  CQ-COMPANY-CD.               
05899                                                                   
05900      START ELCHKQ                                                 
05901          KEY IS NOT LESS THAN CQ-CONTROL-PRIMARY                  
05902                                                                   
05903      IF ELCHKQ-FILE-STATUS = ('23' OR '10')                       
05904          GO TO 2790-EXIT.                                         
05905                                                                   
05906      IF ELCHKQ-FILE-STATUS NOT = ZERO                             
05907          MOVE 'ERROR OCCURRED START - ELCHKQ'                     
05908                                  TO  WS-ABEND-MESSAGE             
05909          MOVE ELCHKQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
05910          GO TO ABEND-PGM.                                         
05911                                                                   
05912  2720-PROCESS-CHECK-QUEUE.                                        
05913                                                                   
05914      READ ELCHKQ NEXT INTO WS-SAVE-CHECK-QUEUE.                   
05915                                                                   
05916      IF ELCHKQ-FILE-STATUS = '10'                                 
05917          GO TO 2790-EXIT.                                         
05918                                                                   
05919      IF ELCHKQ-FILE-STATUS NOT = ZERO                             
05920          MOVE 'ERROR OCCURRED READ NEXT - ELCHKQ'                 
05921                                  TO  WS-ABEND-MESSAGE             
05922          MOVE ELCHKQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
05923          GO TO ABEND-PGM.                                         
05924                                                                   
05925      IF CQ-COMPANY-CD NOT = WS-COMPANY-CODE                       
05926          GO TO 2790-EXIT.                                         
05927                                                                   
05928      MOVE +0             TO  WS-VOID-PAYMENT-SW.                  
05929                                                                   
05930      IF CQ-ENTRY-TYPE = ('A' OR 'X' OR 'S')                       
05931          PERFORM 3300-BUILD-EXTRACT-C-RECORD-A                    
05932          IF THIS-IS-MONTH-END                                     
05933            AND CQ-CHECK-WRITTEN-DT NOT = LOW-VALUES               
05934            AND CQ-CHECK-WRITTEN-DT LESS THAN WS-CHKQ-PURGE-DATE   
05935              PERFORM 7200-PURGE-CHECK-QUEUE                       
05936              GO TO 2720-PROCESS-CHECK-QUEUE                       
05937            ELSE                                                   
05938              GO TO 2720-PROCESS-CHECK-QUEUE.                      
05939                                                                   
05940 ******************************************************************
05941 ******     BYPASS CHECK QUE RECORDS THAT WERE NOT          *******
05942 ******     PROCESSED DURING THE CURRENT MONTH              *******
05943 ******************************************************************
05944                                                                   
05945      IF THIS-IS-NOT-MONTH-END                                     
05946         IF (CQ-CHECK-WRITTEN-DT NOT = LOW-VALUES)                 
05947           AND                                                     
05948            (CQ-CHECK-WRITTEN-DT NOT GREATER THAN WS-PRIOR-MONTH   
05949                 OR                                                
05950             CQ-CHECK-WRITTEN-DT GREATER THAN BIN-RUN-DATE)        
05951               GO TO 2720-PROCESS-CHECK-QUEUE.                     
05952                                                                   
05953      MOVE '0'                    TO  WS-DETAIL1.                  
05954                                                                   
05955      MOVE CQ-COMPANY-CD          TO  CL-COMPANY-CD.               
05956      MOVE CQ-CARRIER             TO  CL-CARRIER                   
05957                                      WS-D1-CARRIER.               
05958      MOVE CQ-CLAIM-NO            TO  CL-CLAIM-NO                  
05959                                      WS-D1-CLAIM-NO.              
05960      MOVE CQ-CERT-NO             TO  CL-CERT-NO                   
05961                                      WS-D1-CERT-NO.               
05962                                                                   
05963      READ ELMSTR INTO WS-SAVE-CLAIM-MASTER.                       
05964                                                                   
05965      IF ELMSTR-FILE-STATUS = '23'                                 
05966          MOVE 'CLAIM NOT FOUND - PROCESS CHECK QUEUE'             
05967                                  TO  WS-D1-MESSAGE                
05968          MOVE WS-DETAIL1         TO  PRT                          
05969          PERFORM WRITE-A-LINE                                     
05970          MOVE SPACES             TO  WS-DETAIL1                   
05971          GO TO 2720-PROCESS-CHECK-QUEUE.                          
05972                                                                   
05973      IF ELMSTR-FILE-STATUS NOT = ZERO                             
05974          MOVE 'ERROR OCCURRED READ - ELMSTR'                      
05975                                  TO  WS-ABEND-MESSAGE             
05976          MOVE ELMSTR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
05977          GO TO ABEND-PGM.                                         
05978                                                                   
05979      PERFORM 5000-MOVE-NAME.                                      
05980                                                                   
121902*    IF CL-SYSTEM-IDENTIFIER = 'CV'                               
121902*        PERFORM 2210-GET-POLICY-MASTER                           
121902*        IF WS-EMPLCY-RETURN-CODE NOT = '00'                      
121902*            MOVE 'POLICY RECORD NOT FOUND '                      
121902*                                TO  WS-D1-MESSAGE                
121902*            MOVE WS-DETAIL1     TO  PRT                          
121902*            PERFORM WRITE-A-LINE                                 
121902*        ELSE                                                     
121902*            NEXT SENTENCE                                        
121902*    ELSE                                                         
05991          PERFORM 2200-GET-CERTIFICATE.                             
05992          IF ELCERT-FILE-STATUS NOT = ZERO                         
05993              MOVE 'CERTIFICATE NOT FOUND - CREATED'               
05994                                  TO  WS-D1-MESSAGE                
05995              MOVE WS-DETAIL1     TO  PRT                          
05996              PERFORM WRITE-A-LINE                                 
05997              PERFORM 9100-CREATE-CERTIFICATE                      
05998              PERFORM 8800-WRITE-CERTIFICATE.                      
05999                                                                   
06000      MOVE CL-CONTROL-PRIMARY     TO  AT-CONTROL-PRIMARY.          
06001      MOVE CQ-PMT-TRLR-SEQUENCE   TO  AT-SEQUENCE-NO.              
06002                                                                   
06003      PERFORM 8300-READ-TRAILER.                                   
06004                                                                   
06005      IF ELTRLR-FILE-STATUS = '23'                                 
06006          MOVE 'PAYMENT TRAILER NOT FOUND - PROCESS CHECK QUEUE'   
06007                                  TO  WS-D1-MESSAGE                
06008          MOVE WS-DETAIL1         TO  PRT                          
06009          PERFORM WRITE-A-LINE                                     
06010          MOVE SPACES             TO  WS-DETAIL1                   
06011          GO TO 2720-PROCESS-CHECK-QUEUE.                          
06012                                                                   
06013      MOVE +2                     TO  WS-ELCHKQ-SW.                
06014                                                                   
06015      IF CQ-CHECK-WRITTEN-DT = SPACES OR LOW-VALUES                
06016          MOVE AT-CHECK-WRITTEN-DT  TO  CQ-CHECK-WRITTEN-DT.       
06017                                                                   
06018      IF AT-PMT-SELECT-DT = WS-MONTH-END-DATE                      
06019          PERFORM 3300-BUILD-EXTRACT-C-RECORD-A.                   
06020                                                                   
06021      IF CQ-TIMES-PRINTED = ZERO                                   
06022          PERFORM 3700-BUILD-EXTRACT-D-RECORD-E.                   
06023                                                                   
06024      IF THIS-IS-MONTH-END                                         
06025        AND CQ-CHECK-WRITTEN-DT NOT = LOW-VALUES                   
06026        AND CQ-CHECK-WRITTEN-DT LESS THAN WS-CHKQ-PURGE-DATE       
06027          PERFORM 7200-PURGE-CHECK-QUEUE                           
06028          MOVE +99999999          TO  AT-CHECK-QUE-CONTROL         
06029          MOVE ZERO               TO  AT-CHECK-QUE-SEQUENCE        
06030          PERFORM 8100-REWRITE-TRAILER                             
06031          GO TO 2720-PROCESS-CHECK-QUEUE.                          
06032                                                                   
06033      IF AT-CHECK-QUE-CONTROL NOT = +99999999                      
06034          IF AT-CHECK-QUE-CONTROL = CQ-CONTROL-NUMBER              
06035            AND AT-CHECK-QUE-SEQUENCE = CQ-SEQUENCE-NUMBER         
06036              NEXT SENTENCE                                        
06037            ELSE                                                   
06038              MOVE 'CORRECTING CHECK QUEUE POINTER'                
06039                                         TO  WS-D1-MESSAGE         
06040              MOVE AT-SEQUENCE-NO        TO  WS-D1-SEQ             
06041              MOVE AT-CHECK-QUE-CONTROL  TO  WS-D1-QUE             
06042              MOVE AT-CHECK-QUE-SEQUENCE TO  WS-D1-QUE-SEQ         
06043              MOVE WS-DETAIL1            TO  PRT                   
06044              PERFORM WRITE-A-LINE                                 
06045              MOVE SPACES                TO  WS-DETAIL1            
06046              MOVE CQ-CONTROL-NUMBER     TO  AT-CHECK-QUE-CONTROL  
06047              MOVE CQ-SEQUENCE-NUMBER    TO  AT-CHECK-QUE-SEQUENCE 
06048              PERFORM 8100-REWRITE-TRAILER                         
06049              GO TO 2720-PROCESS-CHECK-QUEUE.                      
06050                                                                   
06051      GO TO 2720-PROCESS-CHECK-QUEUE.                              
06052                                                                   
06053  2790-EXIT.                                                       
06054      EXIT.                                                        
06055                                  EJECT                            
06056  2800-VERIFY-ACTIVITY-QUEUE SECTION.                              
06057 *    NOTE ******************************************************* 
06058 *         *      THIS SECTION VERIFIES THAT THE PAYMENT THAT    * 
06059 *         *  HAS NOT BEEN RELEASED FOR PRINTING HAS AN ACTIVITY * 
06060 *         *  QUEUE RECORD AND THERE ARE PAYMENTS PENDING.       * 
06061 *         *******************************************************.
06062                                                                   
06063      MOVE CL-CONTROL-PRIMARY     TO  AQ-CONTROL-PRIMARY.          
06064                                                                   
06065      READ ELACTQ INTO WS-SAVE-ACTIVITY-QUEUE.                     
06066                                                                   
06067      IF ELACTQ-FILE-STATUS = '23'                                 
06068          GO TO 2830-VERIFY-ACTIVITY-QUEUE.                        
06069                                                                   
06070      IF ELACTQ-FILE-STATUS NOT = ZERO                             
06071          MOVE ELACTQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
06072          MOVE 'ERROR OCCURRED READ - ELACTQ' TO  WS-ABEND-MESSAGE 
06073          GO TO ABEND-PGM.                                         
06074                                                                   
06075      IF PENDING-PAYMENTS                                          
06076        AND AQ-PAYMENT-COUNTER GREATER THAN ZERO                   
06077          GO TO 2899-EXIT.                                         
06078                                                                   
06079      MOVE 'PAYMENT NOT RELEASED & PENDING PMT FLAG OFF - CORRECTED
06080 -         ' '                    TO  WS-D1-MESSAGE.               
06081      MOVE WS-DETAIL1             TO  PRT.                         
06082      PERFORM WRITE-A-LINE.                                        
06083      MOVE SPACES             TO  WS-DETAIL1.                      
06084                                                                   
06085      MOVE '1'                    TO  AQ-PENDING-PAYMENT-FLAG.     
06086                                                                   
06087      MOVE +1                     TO  AQ-PAYMENT-COUNTER.          
06088                                                                   
06089      MOVE +3100                  TO  AQ-LAST-UPDATED-BY.          
06090                                                                   
06091      MOVE +20                    TO SLR-KEY-LENGTH.               
06092      MOVE +60                    TO SLR-RECORD-LENGTH.            
06093      MOVE AQ-CONTROL-PRIMARY     TO SLR-KEY.                      
06094      MOVE ACTIVITY-QUE           TO SLR-RECORD-IMAGE.             
06095      MOVE 'ELACTQ  '             TO SLR-DSID.                     
06096      MOVE 'C'                    TO SLR-ACTION.                   
06097                                                                   
06098      REWRITE ACTIVITY-QUE.                                        
06099                                                                   
06100      IF ELACTQ-FILE-STATUS NOT = ZERO                             
06101          MOVE ELACTQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
06102          MOVE 'ERROR OCCURRED REWRITE -ELACTQ' TO WS-ABEND-MESSAGE
06103          GO TO ABEND-PGM.                                         
06104                                                                   
06105 *    PERFORM LOG-JOURNAL-RECORD.                                  
06106                                                                   
06107      GO TO 2899-EXIT.                                             
06108                                                                   
06109  2830-VERIFY-ACTIVITY-QUEUE.                                      
06110                                                                   
06111      MOVE 'PAYMENT NOT RELEASED & NO ACTIVITY QUE RECORD - CORRECT
06112 -         'ED'                   TO  WS-D1-MESSAGE.               
06113      MOVE WS-DETAIL1             TO  PRT.                         
06114      PERFORM WRITE-A-LINE.                                        
06115      MOVE SPACES                 TO  WS-DETAIL1                   
06116                                                                   
06117      MOVE 'AQ'                   TO  ACTIVITY-QUE.                
06118                                                                   
06119      MOVE CL-CONTROL-PRIMARY     TO  AQ-CONTROL-PRIMARY.          
06120                                                                   
06121      MOVE '1'                    TO  AQ-PENDING-PAYMENT-FLAG.     
06122      MOVE +1                     TO  AQ-PAYMENT-COUNTER.          
06123                                                                   
06124      MOVE +3100                  TO  AQ-LAST-UPDATED-BY.          
06125                                                                   
06126      MOVE +20                    TO SLR-KEY-LENGTH.               
06127      MOVE +60                    TO SLR-RECORD-LENGTH.            
06128      MOVE AQ-CONTROL-PRIMARY     TO SLR-KEY.                      
06129      MOVE ACTIVITY-QUE           TO SLR-RECORD-IMAGE.             
06130      MOVE 'ELACTQ  '             TO SLR-DSID.                     
06131      MOVE 'A'                    TO SLR-ACTION.                   
06132                                                                   
06133      WRITE ACTIVITY-QUE.                                          
06134                                                                   
06135      IF ELACTQ-FILE-STATUS NOT = ZERO                             
06136          MOVE ELACTQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
06137          MOVE 'ERROR OCCURRED WRITE - ELACTQ' TO WS-ABEND-MESSAGE 
06138          GO TO ABEND-PGM.                                         
06139                                                                   
06140 *    PERFORM LOG-JOURNAL-RECORD.                                  
06141                                                                   
06142  2899-EXIT.                                                       
06143      EXIT.                                                        
06144                                  EJECT                            
06145  2900-OLD-PAYMENTS SECTION.                                       
06146      IF (AT-PMT-ACCEPT-DT = LOW-VALUES AND                        
06147          AT-CHECK-WRITTEN-DT LESS THAN WS-010184 AND              
06148          AT-CHECK-WRITTEN-DT NOT = LOW-VALUES)                    
06149             MOVE WS-123183          TO AT-PMT-ACCEPT-DT           
06150             MOVE +1                 TO WS-UPDATE-TRAILER          
06151             MOVE 'PAYMENT NOT ACCEPTED, PRIOR TO 01/01/84'        
06152                                     TO  WS-D1-MESSAGE             
06153             MOVE WS-DETAIL1         TO  PRT                       
06154             PERFORM WRITE-A-LINE                                  
06155             MOVE SPACES             TO  WS-DETAIL1.               
06156                                                                   
06157      IF (AT-VOID-ACCEPT-DT = LOW-VALUES AND                       
06158         AT-VOID-DT LESS THAN WS-010184 AND                        
06159         AT-VOID-DT NOT = LOW-VALUES)                              
06160            MOVE WS-123183     TO AT-VOID-ACCEPT-DT                
06161            MOVE +1            TO WS-UPDATE-TRAILER                
06162            MOVE 'VOID NOT ACCEPTED, PRIOR TO 01/01/84'            
06163                               TO WS-D1-MESSAGE                    
06164            MOVE WS-DETAIL1    TO PRT                              
06165            PERFORM WRITE-A-LINE                                   
06166            MOVE SPACES TO WS-DETAIL1.                             
06167                                                                   
06168  2900-EXIT.                                                       
06169      EXIT.                                                        
06170                                  EJECT                            
06171  2950-VERIFY-ELARCH-RECORD SECTION.                               
06172                                                                   
06173      MOVE LOW-VALUES             TO  LA-CONTROL-PRIMARY.          
06174      MOVE WS-COMPANY-CODE        TO  LA-COMPANY-CD.               
06175      MOVE '1'                    TO  LA-RECORD-TYPE.              
06176      MOVE AT-LETTER-ARCHIVE-NO   TO  LA-ARCHIVE-NO.               
06177                                                                   
06178      START ELARCH                                                 
06179          KEY IS NOT LESS THAN LA-CONTROL-PRIMARY.                 
06180                                                                   
06181      IF ELARCH-FILE-STATUS = '23'                                 
06182         MOVE ' ELARCH REC NOT FOUND ' TO WS-D1-MESSAGE            
06183          GO TO 2955-PRINT-ERROR-DATA.                             
06184                                                                   
06185      IF ELARCH-FILE-STATUS NOT = ZERO                             
06186          MOVE 'ERROR OCCURRED START - ELARCH'                     
06187                                  TO  WS-ABEND-MESSAGE             
06188          MOVE ELARCH-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
06189          GO TO ABEND-PGM.                                         
06190                                                                   
06191      READ ELARCH NEXT RECORD.                                     
06192                                                                   
06193      IF ELARCH-FILE-STATUS = '10'                                 
06194         MOVE ' ELARCH REC NOT FOUND ' TO WS-D1-MESSAGE            
06195         GO TO 2955-PRINT-ERROR-DATA.                              
06196                                                                   
06197      IF ELARCH-FILE-STATUS NOT = ZERO                             
06198          MOVE 'ERROR OCCURRED READNEXT - ELARCH'                  
06199                                  TO  WS-ABEND-MESSAGE             
06200          MOVE ELARCH-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
06201          GO TO ABEND-PGM.                                         
06202                                                                   
06203      IF LA-COMPANY-CD NOT = WS-COMPANY-CODE                       
06204        OR AT-LETTER-ARCHIVE-NO NOT = LA-ARCHIVE-NO                
06205        OR LA-RECORD-TYPE NOT = '1'                                
06206           PERFORM 2970-SEARCH-LETTERS THRU 2999-EXIT              
06207           GO TO 2955-PRINT-ERROR-DATA.                            
06208                                                                   
06209      MOVE SPACES                 TO  WS-D1-MESSAGE.               
06210                                                                   
06211      IF (LA4-RESEND-DATE  NOT = AT-AUTO-RE-SEND-DT AND            
06212         LA4-RESEND-DATE   NOT = LOW-VALUES AND SPACES)            
06213          MOVE ' ARCH RSND DT NOT = TRLR, SEQ='                    
06214                                  TO  WS-D1-MESSAGE                
06215          MOVE AT-SEQUENCE-NO     TO  WS-D1-TRLR-SEQ               
06216          GO TO 2955-PRINT-ERROR-DATA.                             
06217                                                                   
06218      IF LA4-CREATION-DT NOT = AT-LETTER-SENT-DT                   
06219          MOVE 'ARCH CREAT DT NOT = TRLR, SEQ='                    
06220                                  TO  WS-D1-MESSAGE                
06221          MOVE AT-SEQUENCE-NO     TO  WS-D1-TRLR-SEQ               
06222          GO TO 2955-PRINT-ERROR-DATA.                             
06223                                                                   
06224      IF LA4-INITIAL-PRINT-DATE NOT = AT-INITIAL-PRINT-DATE        
06225          MOVE ' ARCH IPRT DT NOT = TRLR, SEQ='                    
06226                                  TO  WS-D1-MESSAGE                
06227          MOVE AT-SEQUENCE-NO     TO  WS-D1-TRLR-SEQ               
06228          GO TO 2955-PRINT-ERROR-DATA.                             
06229                                                                   
06230      IF LA4-RESEND-PRINT-DATE NOT = AT-RESEND-PRINT-DATE          
06231          MOVE ' ARCH RPRT DT NOT = TRLR, SEQ='                    
06232                                  TO  WS-D1-MESSAGE                
06233          MOVE AT-SEQUENCE-NO     TO  WS-D1-TRLR-SEQ               
06234          GO TO 2955-PRINT-ERROR-DATA.                             
06235                                                                   
06236      GO TO 2969-EXIT.                                             
06237                                                                   
06238  2955-PRINT-ERROR-DATA.                                           
06239      MOVE AT-CARRIER             TO WS-D1-CARRIER.                
06240      MOVE AT-CLAIM-NO            TO WS-D1-CLAIM-NO.               
06241      MOVE AT-CERT-NO             TO WS-D1-CERT-NO.                
06242                                                                   
06243      MOVE WS-DETAIL1             TO PRT.                          
06244      PERFORM WRITE-A-LINE.                                        
06245      MOVE SPACES                 TO WS-DETAIL1.                   
06246                                                                   
06247  2969-EXIT.                                                       
06248      EXIT.                                                        
06249                                  EJECT                            
06250  2970-SEARCH-LETTERS.                                             
06251                                                                   
06252      MOVE LOW-VALUES             TO  LA-CONTROL-PRIMARY.          
06253      MOVE '1'                    TO  LA-RECORD-TYPE.              
06254      MOVE AT-LETTER-ARCHIVE-NO   TO  LA-ARCHIVE-NO.               
06255                                                                   
06256      START ELARCH                                                 
06257          KEY IS NOT LESS THAN LA-CONTROL-PRIMARY.                 
06258                                                                   
06259      IF ELARCH-FILE-STATUS = '23'                                 
06260          MOVE ' ELARCH REC NOT FOUND ' TO WS-D1-MESSAGE           
06261          GO TO 2999-EXIT.                                         
06262                                                                   
06263      IF ELARCH-FILE-STATUS NOT = ZERO                             
06264          MOVE 'ERROR OCCURRED START - ELARCH'                     
06265                                  TO  WS-ABEND-MESSAGE             
06266          MOVE ELARCH-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
06267          GO TO ABEND-PGM.                                         
06268                                                                   
06269      READ ELARCH NEXT RECORD.                                     
06270                                                                   
06271      IF ELARCH-FILE-STATUS = '10'                                 
06272         MOVE ' ELARCH REC NOT FOUND ' TO WS-D1-MESSAGE            
06273         GO TO 2999-EXIT.                                          
06274                                                                   
06275      IF ELARCH-FILE-STATUS NOT = ZERO                             
06276          MOVE 'ERROR OCCURRED READNEXT - ELARCH'                  
06277                                  TO  WS-ABEND-MESSAGE             
06278          MOVE ELARCH-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
06279          GO TO ABEND-PGM.                                         
06280                                                                   
06281      IF (AT-LETTER-ARCHIVE-NO NOT = LA-ARCHIVE-NO)                
06282        OR (LA-RECORD-TYPE NOT = '1')                              
06283        OR (LA-CARRIER  NOT = AT-CARRIER)                          
06284        OR (LA-CLAIM-NO NOT = AT-CLAIM-NO)                         
06285        OR (LA-CERT-NO  NOT = AT-CERT-NO)                          
06286          MOVE ' ELARCH REC NOT FOUND ' TO WS-D1-MESSAGE           
06287          GO TO 2999-EXIT                                          
06288      ELSE                                                         
06289          MOVE 'LETTER ARCHIVE FIXED'   TO WS-D1-MESSAGE           
06290          GO TO 2793-DELETE-RECORD.                                
06291                                                                   
06292  2971-SEARCH-LOOP.                                                
06293                                                                   
06294      MOVE WS-SAVE-SEARCH-CONTROL    TO LA-CONTROL-PRIMARY.        
06295      ADD +1                         TO LA-LINE-SEQ-NO.            
06296                                                                   
06297      START ELARCH                                                 
06298          KEY IS NOT LESS THAN LA-CONTROL-PRIMARY.                 
06299                                                                   
06300      IF ELARCH-FILE-STATUS = '23'                                 
06301          GO TO 2999-EXIT.                                         
06302                                                                   
06303      IF ELARCH-FILE-STATUS NOT = ZERO                             
06304          MOVE 'ERROR OCCURRED START - ELARCH'                     
06305                                  TO  WS-ABEND-MESSAGE             
06306          MOVE ELARCH-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
06307          GO TO ABEND-PGM.                                         
06308                                                                   
06309      READ ELARCH NEXT RECORD.                                     
06310                                                                   
06311      IF ELARCH-FILE-STATUS = '10'                                 
06312         MOVE ' ELARCH REC NOT FOUND ' TO WS-D1-MESSAGE            
06313         GO TO 2999-EXIT.                                          
06314                                                                   
06315      IF ELARCH-FILE-STATUS NOT = ZERO                             
06316          MOVE 'ERROR OCCURRED READNEXT - ELARCH'                  
06317                                  TO  WS-ABEND-MESSAGE             
06318          MOVE ELARCH-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
06319          GO TO ABEND-PGM.                                         
06320                                                                   
06321      IF AT-LETTER-ARCHIVE-NO NOT = LA-ARCHIVE-NO                  
06322          GO TO 2999-EXIT.                                         
06323                                                                   
06324  2793-DELETE-RECORD.                                              
06325                                                                   
06326      MOVE LETTER-ARCHIVE               TO WS-SAVE-SEARCH-ARCHIVE. 
06327      MOVE LA-CONTROL-PRIMARY           TO WS-SAVE-SEARCH-CONTROL. 
06328                                                                   
06329      DELETE ELARCH RECORD.                                        
06330                                                                   
06331      IF ELARCH-FILE-STATUS NOT = ZERO                             
06332          MOVE ELARCH-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
06333          MOVE 'ERROR OCCURRED DELETE - ELARCH' TO WS-ABEND-MESSAGE
06334          GO TO ABEND-PGM.                                         
06335                                                                   
06336  2795-MODIFY-CODE.                                                
06337                                                                   
06338      MOVE WS-SAVE-SEARCH-ARCHIVE       TO LETTER-ARCHIVE.         
06339      MOVE AT-COMPANY-CD                TO LA-COMPANY-CD           
06340                                           LA-COMPANY-CD-A1.       
06341                                                                   
06342      WRITE LETTER-ARCHIVE.                                        
06343                                                                   
06344      IF ELARCH-FILE-STATUS NOT = '00'                             
06345         MOVE 'ERROR OCCURRED WRITE - ELARCH'                      
06346                           TO WS-ABEND-MESSAGE                     
06347         MOVE ELARCH-FILE-STATUS TO WS-ABEND-FILE-STATUS           
06348         GO TO ABEND-PGM.                                          
06349                                                                   
06350      GO TO 2971-SEARCH-LOOP.                                      
06351                                                                   
06352  2999-EXIT.                                                       
06353       EXIT.                                                       
06354                                  EJECT                            
06355  3000-BUILD-EXTRACT-A-RECORD-A SECTION.                           
06356 *    NOTE ******************************************************* 
06357 *         *             EXTRACT - A   RECORD - A                * 
06358 *         *                                                     * 
06359 *         *            EXTRACT FOR ALL OPEN CLAIMS              * 
06360 *         *******************************************************.
06361                                                                   
06362      PERFORM 2300-GET-ACCOUNT.                                    
06363                                                                   
06364      MOVE SPACES                 TO  REPORTS-EXTRACT-RECORD.      
06365                                                                   
06366      MOVE 'EX'                   TO  EX-RECORD-ID.                
06367      MOVE '1'                    TO  EX-POSITIONING-CODE.         
06368      MOVE 'A'                    TO  EX-EXTRACT-CODE.             
06369      MOVE WS-COMPANY-CODE        TO  EX-COMPANY-CD.               
06370      MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID                
06371                                      CP-COMPANY-ID                
06372                                      OP-COMPANY-ID.               
06373      MOVE 'A'                    TO  EX-RECORD-TYPE.              
06374                                                                   
06375      MOVE CL-CARRIER             TO  EX-SA-CARRIER.               
06376      MOVE CL-CLAIM-NO            TO  EX-SA-CLAIM-NO.              
06377      MOVE CL-CERT-NO             TO  EX-SA-CERT-NO.               
06378                                                                   
06379      MOVE CL-CERT-CARRIER        TO  EX-AA-CARRIER                
06380                                      CP-CARRIER.                  
06381      MOVE CL-CERT-STATE          TO  EX-AA-STATE.                 
06382      MOVE CL-CERT-ACCOUNT        TO  EX-AA-ACCOUNT.               
06383      MOVE CL-CERT-GROUPING       TO  EX-AA-GROUPING.              
06384      MOVE CL-CERT-EFF-DT         TO  EX-AA-CERT-EFF-DT.           
06385      MOVE CL-CLAIM-TYPE          TO  EX-AA-CLAIM-TYPE.            
06386                                                                   
052614     IF CL-CLAIM-TYPE = WS-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
022122        OR 'B' OR 'H'
06388          MOVE CM-AH-BENEFIT-CD   TO  EX-AA-BENEFIT-CODE           
06389      ELSE                                                         
06390          MOVE CM-LF-BENEFIT-CD   TO  EX-AA-BENEFIT-CODE.          
06391                                                                   
06392      MOVE CL-CLAIM-PREM-TYPE     TO  EX-AA-CLAIM-PREM-TYPE.       
06393                                                                   
06394      MOVE ZERO                   TO  EX-AA-REMAINING-TERM         
06395                                      EX-AA-REMAINING-BENEFIT      
06396                                      CP-RETURN-CODE               
06397                                      OP-RETURN-CODE.              
06398                                                                   
06399      MOVE WS-MONTH-END-DATE      TO  CP-VALUATION-DT              
06400                                      OP-VALUATION-DT.             
06401                                                                   
06402      MOVE CL-PAID-THRU-DT        TO  CP-PAID-THRU-DT              
06403                                      OP-PAID-THRU-DT.             
06404                                                                   
06405      IF CL-INSURED-BIRTH-DT NOT = LOW-VALUES                      
06406         MOVE CL-INSURED-BIRTH-DT TO CP-INSURED-BIRTH-DT           
06407      ELSE                                                         
06408         MOVE LOW-VALUES          TO CP-INSURED-BIRTH-DT.          
06409                                                                   
06410      MOVE CL-INCURRED-DT         TO  CP-INCURRED-DT               
06411                                      OP-INCURRED-DT.              
06412      MOVE CL-REPORTED-DT         TO  CP-REPORTED-DT               
06413                                      OP-REPORTED-DT.              
06414      MOVE SPACES                 TO  CP-ACCT-FLD-5.               
06415      MOVE WS-PERCENT-OF-CDT      TO  CP-CDT-PERCENT.              
06416                                                                   
06417      MOVE WS-CDT-ACCESS-METHOD   TO  CP-CDT-METHOD                
06418                                      OP-CIDA-METHOD.              
100518     IF CL-CLAIM-TYPE = 'O'
100518        MOVE 'L'                  TO CP-CLAIM-TYPE
100518                                     OP-CLAIM-TYPE
100518     ELSE
06419         MOVE CL-CLAIM-TYPE       TO  CP-CLAIM-TYPE
06420                                      OP-CLAIM-TYPE.
06421      MOVE CL-CLAIM-STATUS        TO  CP-CLAIM-STATUS              
06422                                      OP-CLAIM-STATUS.             
06423      MOVE CL-TOTAL-PAID-AMT      TO  CP-TOTAL-PAID.               
06424                                                                   
06425      MOVE WS-IBNR-SW             TO  CP-IBNR-RESERVE-SW.          
06426      MOVE '1'                    TO  CP-PROCESS-TYPE.             
06427                                                                   
121902*    IF CL-SYSTEM-IDENTIFIER = 'CV'                               
121902*        GO TO 3000-CONVENIENCE-RESERVES.                         
06430                                                                   
06431      IF CM-LF-ALT-BENEFIT-AMT IS NOT NUMERIC                      
06432          MOVE +0                 TO  CM-LF-ALT-BENEFIT-AMT.       
06433                                                                   
06434      IF CM-LF-PREMIUM-AMT IS NOT NUMERIC                          
06435          MOVE +0                 TO  CM-LF-PREMIUM-AMT.           
06436                                                                   
06437      IF CM-AH-PREMIUM-AMT IS NOT NUMERIC                          
06438          MOVE +0                 TO  CM-AH-PREMIUM-AMT.           
06439                                                                   
06440      MOVE WS-REM-TERM-METHOD     TO  CP-REM-TERM-METHOD.          
06441      MOVE WS-EARNING-METHOD      TO  CP-EARNING-METHOD.           
06442      MOVE WS-SPECIAL-CALC-CODE   TO  CP-SPECIAL-CALC-CD.          
06443                                                                   
100518     IF CL-CLAIM-TYPE = WS-LIFE-OVERRIDE-L1 OR 'O'
06445         MOVE CM-LF-ALT-BENEFIT-AMT TO CP-ALTERNATE-BENEFIT.       
06446                                                                   
06447      IF CP-REM-TERM-METHOD = (SPACES OR ZERO)                     
06448          MOVE WS-CR-REM-TERM-CALC  TO  CP-REM-TERM-METHOD.        
06449                                                                   
06450      IF CM-CERT-EFF-DT GREATER THAN CL-INCURRED-DT                
06451          MOVE 'INCURRED DATE LESS THAN EFFECTIVE DATE' TO         
06452                                   WS-D1-MESSAGE                   
06453          MOVE WS-DETAIL1          TO PRT                          
06454          PERFORM WRITE-A-LINE                                     
06455          MOVE SPACES              TO WS-DETAIL1                   
06456          GO TO 3099-EXIT.                                         
06457                                                                   
06458      MOVE CM-CERT-EFF-DT         TO  CP-CERT-EFF-DT               
06459                                      OP-CERT-EFF-DT.              
06460      MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.           
06461      MOVE CM-INSURED-ISSUE-AGE   TO  CP-ISSUE-AGE                 
06462                                      OP-ISSUE-AGE.                
06463                                                                   
06464      IF  AM-CRDT-MODIFICATION-PCT NUMERIC                         
06465              AND                                                  
06466          AM-CRDT-MODIFICATION-PCT NOT = +1                        
06467              AND                                                  
06468          AM-CRDT-MODIFICATION-PCT NOT = ZEROS                     
06469            MOVE AM-CRDT-MODIFICATION-PCT                          
06470                                  TO  OP-CIDA-MOD-PERCENT          
06471      ELSE                                                         
06472            MOVE WS-CIDA-DISCOUNT TO  OP-CIDA-MOD-PERCENT.         
06473                                                                   
052614     IF CL-CLAIM-TYPE = WS-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
022122        OR 'B' OR 'H'
06480         MOVE CM-ENTRY-STATUS     TO  EX-AA-CERT-STATUS            
06481         MOVE WS-AH-OVERRIDE-L1   TO  CP-BENEFIT-TYPE              
06482         MOVE CM-AH-ORIG-TERM     TO  CP-ORIGINAL-TERM             
06483                                      OP-ORIGINAL-TERM             
06484                                      EX-AA-ORIG-TERM              
06485         MOVE CM-AH-BENEFIT-AMT   TO  CP-ORIGINAL-BENEFIT          
06486                                      OP-ORIGINAL-BENEFIT          
06487                                      EX-AA-BENEFIT-AMT            
06488         MOVE CM-AH-PREMIUM-AMT   TO  CP-ORIGINAL-PREMIUM          
06489      ELSE                                                         
06490         MOVE CM-ENTRY-STATUS     TO  EX-AA-CERT-STATUS            
06491         MOVE CL-INCURRED-DT      TO  CP-VALUATION-DT              
06492                                      OP-VALUATION-DT              
06493         MOVE WS-COVERAGE-TYPE    TO  CP-BENEFIT-TYPE              
06494         MOVE CM-LF-ORIG-TERM     TO  CP-ORIGINAL-TERM             
06495                                      OP-ORIGINAL-TERM             
06496                                      EX-AA-ORIG-TERM              
06497         MOVE CM-LF-BENEFIT-AMT   TO  CP-ORIGINAL-BENEFIT          
06498                                      OP-ORIGINAL-BENEFIT          
06499                                      EX-AA-BENEFIT-AMT            
06500         MOVE CM-LF-PREMIUM-AMT   TO  CP-ORIGINAL-PREMIUM.         
06501                                                                   
06502      MOVE CM-IND-GRP-TYPE        TO EX-AA-IND-GRP.                
06503                                                                   
06504      IF CM-LOAN-APR IS NOT NUMERIC                                
06505          MOVE +0                 TO  CM-LOAN-APR.                 
06506                                                                   
06507      IF CM-LOAN-TERM IS NOT NUMERIC                               
06508          MOVE +0                 TO  CM-LOAN-TERM.                
06509                                                                   
06510      IF CM-LF-CRITICAL-PERIOD IS NOT NUMERIC                      
06511          MOVE +0                 TO  CM-LF-CRITICAL-PERIOD.       
06512                                                                   
06513      IF CM-AH-CRITICAL-PERIOD IS NOT NUMERIC                      
06514          MOVE +0                 TO  CM-AH-CRITICAL-PERIOD.       
06515                                                                   
06516      MOVE CM-LOAN-APR            TO  CP-LOAN-APR.                 
06517      MOVE CM-LOAN-TERM           TO  CP-LOAN-TERM.                
06518                                                                   
06519      IF  OPT-RESERVE-METHOD-AUTH                                  
06520          PERFORM 3092-MATCH-STATE-TABLE THRU 3092-EXIT.           
06521                                                                   
06522      IF CM-PAY-FREQUENCY NOT GREATER THAN ZERO                    
06523          MOVE +1                 TO  CM-PAY-FREQUENCY             
06524                                      WS-REWRITE-CERT.             
06525                                                                   
121902*    IF WS-COMPANY-ID = 'ACC' OR 'FDL'                            
121902*        IF WS-COVERAGE-TYPE = 'P'                                
121902*            GO TO 3050-BUILD-EXTRACT-A-RECORD-A.                 
06529                                                                   
06530      MOVE CM-PAY-FREQUENCY       TO  CP-PAY-FREQUENCY             
06531                                                                   
06532      MOVE WS-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.       
06533      GO TO 3000-GET-REMAINING-TERM.                               
06534                                                                   
06535  3000-CONVENIENCE-RESERVES.                                       
06536                                                                   
06537      IF WS-PLAN-NOT-FOUND = ZERO                                  
06538          MOVE PP-REFUND-CALC             TO  CP-EARNING-METHOD    
06539          IF PP-BENEFIT-IS-LEVEL                                   
06540              MOVE 'L'                    TO  CP-BENEFIT-TYPE      
06541          ELSE                                                     
06542              MOVE 'R'                    TO  CP-BENEFIT-TYPE.     
06543                                                                   
100518     IF CL-CLAIM-TYPE = WS-LIFE-OVERRIDE-L1 OR 'O'
06545          MOVE '2'                    TO  CP-REM-TERM-METHOD       
06546          MOVE PM-POLICY-EFF-DT       TO  CP-FIRST-PAY-DATE        
06547          MOVE PM-INS-TOTAL-BENEFIT   TO  CP-ORIGINAL-BENEFIT      
06548                                          OP-ORIGINAL-BENEFIT      
06549                                          EX-AA-BENEFIT-AMT        
06550          MOVE CL-INCURRED-DT         TO  CP-VALUATION-DT          
06551                                          OP-VALUATION-DT          
06552      ELSE                                                         
06553          MOVE '3'                    TO  CP-REM-TERM-METHOD       
06554          MOVE PM-LOAN-DT             TO  CP-FIRST-PAY-DATE        
06555          MOVE WS-AH-OVERRIDE-L1      TO  CP-BENEFIT-TYPE          
06556          MOVE PM-INS-MONTH-BENEFIT   TO  CP-ORIGINAL-BENEFIT      
06557                                          OP-ORIGINAL-BENEFIT      
06558                                          EX-AA-BENEFIT-AMT.       
06559                                                                   
06560      IF PM-POLICY-EFF-DT IS GREATER THAN CL-INCURRED-DT           
06561          MOVE 'INCURRED DATE LESS THAN EFFECTIVE DATE'            
06562                                  TO  WS-D1-MESSAGE                
06563          MOVE WS-DETAIL1         TO  PRT                          
06564          PERFORM WRITE-A-LINE                                     
06565          MOVE SPACES             TO  WS-DETAIL1                   
06566          GO TO 3099-EXIT.                                         
06567                                                                   
06568      MOVE '1'                    TO  CP-REM-TRM-CALC-OPTION.      
06569      MOVE 'A'                    TO  CP-SPECIAL-CALC-CD.          
06570      MOVE WS-CIDA-DISCOUNT       TO  OP-CIDA-MOD-PERCENT.         
06571                                                                   
06572      MOVE PM-INS-TYPE            TO  EX-AA-IND-GRP.               
06573      MOVE PM-LOAN-APR            TO  CP-LOAN-APR.                 
06574      MOVE PM-POLICY-EFF-DT       TO  CP-CERT-EFF-DT               
06575                                      OP-CERT-EFF-DT.              
06576      MOVE PM-INSURED-ISSUE-AGE   TO  CP-ISSUE-AGE                 
06577                                      OP-ISSUE-AGE.                
06578      MOVE PM-ENTRY-STATUS        TO  EX-AA-CERT-STATUS.           
06579      MOVE PM-LOAN-TERM           TO  CP-ORIGINAL-TERM             
06580                                      OP-ORIGINAL-TERM             
06581                                      CP-LOAN-TERM                 
06582                                      EX-AA-ORIG-TERM.             
06583                                                                   
06584      IF  OPT-RESERVE-METHOD-AUTH                                  
06585          PERFORM 3092-MATCH-STATE-TABLE THRU 3092-EXIT.           
06586                                                                   
06587  3000-GET-REMAINING-TERM.                                         
06588                                                                   
06589      MOVE ZERO                   TO  CP-REMAINING-TERM-1          
06590                                      CP-REMAINING-TERM-2          
06591                                      CP-REMAINING-TERM-3          
06592                                                                   
06593                                      CP-REMAINING-AMT             
06594                                      CP-REMAINING-AMT-PRV         
06595                                                                   
06596                                      CP-CDT-TABLE                 
06597                                      CP-CDT-FACTOR                
06598                                      CP-PTC-RESERVE               
06599                                      CP-IBNR-RESERVE              
06600                                      CP-FUTURE-RESERVE            
06601                                      CP-RESERVE-REMAINING-TERM    
06602                                                                   
06603                                      OP-RESERVE-FACTOR            
06604                                      OP-PTC-RESERVE               
06605                                      OP-FUTURE-RESERVE            
06606                                      OP-RESERVE-REMAINING-TERM    
06607                                      OP-MONTHLY-EQUIV-PERCENT.    
06608                                                                   
032612     IF CM-AH-CRITICAL-PERIOD NOT NUMERIC
032612        MOVE ZEROS               TO CM-AH-CRITICAL-PERIOD
032612     END-IF
032612
032612     if ws-company-id = 'AHL' or 'FNL'
032612        move cl-incurred-dt      to dc-bin-date-1
032612        move cp-valuation-dt     to dc-bin-date-2
032612        move '1'                 to dc-option-code
032612        PERFORM 8500-DATE-CONVERSION
032612        if no-conversion-error
032612           move dc-elapsed-months to ws-months-disabled
032612        else
032612           move zeros             to ws-months-disabled
032612        end-if
032612     end-if

06609      MOVE WS-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.       
06610      CALL 'ELRTRMX' USING CALCULATION-PASS-AREA.                  
06611                                                                   
06612      IF CP-RETURN-CODE NOT = ZERO                                 
06613          MOVE 'ERROR OCCURRED REMAINING TERM CALCULATIONS'        
06614                                      TO  WS-D1-MESSAGE            
06615          MOVE WS-DETAIL1             TO  PRT                      
06616          PERFORM WRITE-A-LINE                                     
06617          MOVE SPACES                 TO  WS-DETAIL1               
06618          MOVE 'RETURN CODE='         TO  WS-D1-MESSAGE            
06619          MOVE CP-RETURN-CODE         TO  WS-D1-RETURN-CODE        
06620          MOVE WS-DETAIL1             TO  PRT                      
06621          PERFORM WRITE-A-LINE                                     
06622          MOVE SPACES                 TO  WS-DETAIL1               
06623          MOVE 'REMAINING TERM='      TO  WS-D1-MESSAGE            
06624          MOVE CP-REMAINING-TERM-1    TO  WS-D1-REM-TERM1          
06625          MOVE CP-REMAINING-TERM-2    TO  WS-D1-REM-TERM2          
06626          MOVE CP-REMAINING-TERM-3    TO  WS-D1-REM-TERM3          
06627          MOVE WS-DETAIL1             TO  PRT                      
06628          PERFORM WRITE-A-LINE                                     
06629          MOVE SPACES                 TO  WS-DETAIL1               
06630      ELSE                                                       
06631          MOVE CP-REMAINING-TERM-3    TO  CP-REMAINING-TERM        
06632                                          OP-REMAINING-TERM
032612         if (ws-company-id = 'AHL' OR 'FNL')
052614            and (CL-CLAIM-TYPE = WS-AH-OVERRIDE-L1 OR 'I' OR 
022122                'G' OR 'F' OR 'B' OR 'H')
032612            AND (CM-AH-CRITICAL-PERIOD > ZEROS)
032612            AND (cp-REMAINING-TERM > (cm-ah-critical-period -
032612               WS-MONTHS-DISABLED))
032612            COMPUTE cp-REMAINING-TERM =
032612            (cm-ah-critical-period - WS-MONTHS-DISABLED)
032612            if cp-remaining-term < zeros
032612               move zeros        to cp-remaining-term
032612            end-if
032612            move cp-remaining-term
032612                                 to op-remaining-term
032612         end-if
032612     end-if
                  
06633                                                                   
CIDMOD     MOVE CM-RATE-CLASS          TO CP-CLASS-CODE

           MOVE ZEROS                  TO CP-R-MAX-TOT-BEN
                                          CP-R-MAX-MON-BEN

020816     IF (WS-COMPANY-ID = 'DCC' or 'VPP')
051414        AND (AM-DCC-PRODUCT-CODE <> spaces)
              CONTINUE
           ELSE
              GO TO 3000-GET-REMAINING-AMT
           END-IF

051414     IF CM-CLP-STATE = SPACES
051414        MOVE CM-STATE            to cm-clp-state
051414     end-if

           MOVE CL-COMPANY-CD          TO PC-CONTROL-PRIMARY
051414     MOVE CM-CLP-STATE           TO PC-STATE
           MOVE AM-DCC-PRODUCT-CODE    TO PC-PRODUCT-CD
100518     if (cl-claim-type = 'L' OR 'P' OR 'O')
051414        and (cm-lf-benefit-cd not = '00' and '  ' and 'DD')
051414        move 'L'                 to pc-ben-type
051414        move cm-lf-benefit-cd    to pc-ben-code
051414     else
051414        MOVE 'A'                 TO PC-BEN-TYPE
051414        MOVE CM-AH-BENEFIT-CD    TO PC-BEN-CODE
051414     end-if
           MOVE CM-CERT-EFF-DT         TO PC-PROD-EXP-DT
           MOVE PC-CONTROL-PRIMARY     TO WS-PDEF-SAVE-KEY

           START ERPDEF KEY > PC-CONTROL-PRIMARY
           IF ERPDEF-FILE-STATUS NOT = '00'
              DISPLAY ' ERPDEF START ' ERPDEF-FILE-STATUS
              GO TO 3000-GET-REMAINING-AMT
           END-IF
           READ ERPDEF NEXT RECORD
           IF (ERPDEF-FILE-STATUS NOT = '00')
              OR (WS-PDEF-SAVE-KEY NOT = PC-CONTROL-PRIMARY (1:16))
              DISPLAY ' ERPDEF READ  ' ERPDEF-FILE-STATUS
              DISPLAY ' SAVE KEY ' WS-PDEF-SAVE-KEY
              DISPLAY ' FILE KEY ' PC-CONTROL-PRIMARY (1:16)
              GO TO 3000-GET-REMAINING-AMT
           END-IF
           PERFORM VARYING WS-SUB FROM +1 BY +1 UNTIL
032922        (WS-SUB > +11)
              OR (PC-PROD-CODE (WS-SUB) = CL-CLAIM-TYPE)
           END-PERFORM
032922     IF WS-SUB < +12
              IF CL-CLAIM-TYPE = WS-AH-OVERRIDE-L1 OR 'I' OR 'G'
022122               OR 'F' OR 'B' OR 'H'
                 MOVE PC-MAX-AMT (WS-SUB) TO CP-R-MAX-MON-BEN
              ELSE
                 MOVE PC-MAX-AMT (WS-SUB) TO CP-R-MAX-TOT-BEN
              END-IF
           END-IF

           .
       3000-GET-REMAINING-AMT.

06634      IF CP-RETURN-CODE = ZERO                                     
06635          CALL 'ELRAMTX' USING CALCULATION-PASS-AREA               
06636          IF CP-RETURN-CODE NOT = ZERO                             
06637                 OR                                                
06638             CP-REMAINING-AMT NOT NUMERIC                          
06639               MOVE 'ERROR OCCURRED REMAINING AMOUNT CALCULATIONS' 
06640                                         TO  WS-D1-MESSAGE         
06641               MOVE WS-DETAIL1           TO  PRT                   
06642               PERFORM WRITE-A-LINE                                
06643               MOVE SPACES               TO  WS-DETAIL1            
06644               MOVE 'RETURN CODE='       TO  WS-D1-MESSAGE         
06645               MOVE CP-RETURN-CODE       TO  WS-D1-RETURN-CODE     
06646               MOVE WS-DETAIL1           TO  PRT                   
06647               PERFORM WRITE-A-LINE                                
06648               MOVE SPACES               TO  WS-DETAIL1            
06649               MOVE 'REMAINING AMT='     TO  WS-D1-MESSAGE         
06650               MOVE CP-REMAINING-AMT     TO  WS-D1-REM-AMT1        
06651               MOVE CP-REMAINING-AMT-PRV TO  WS-D1-REM-AMT2        
06652               MOVE WS-DETAIL1           TO  PRT                   
06653               PERFORM WRITE-A-LINE                                
06654               MOVE SPACES               TO  WS-DETAIL1            
06655            ELSE                                                   
06656               MOVE CP-REMAINING-AMT     TO  OP-REMAINING-AMT
032612              if ws-company-id = 'AHL' OR 'FNL'
100518                 IF (CL-CLAIM-TYPE = 'L' OR 'O')
032612                    AND CP-EARN-BY-R78
032612                    AND CM-LOAN-TERM > CM-LF-ORIG-TERM
032612                    DISPLAY ' ABOUT TO CHG REM AMT '
032612                      CL-CLAIM-NO ' ' CP-REMAINING-AMT
032612                    COMPUTE CP-REMAINING-AMT = (CM-LF-BENEFIT-AMT
032612                      / CM-LOAN-TERM) * ((CP-REMAINING-TERM +
032612                      (CM-LOAN-TERM - CM-LF-ORIG-TERM)))
032612                      DISPLAY '                   TO '
032612                         CP-REMAINING-AMT
032612                 END-IF
032612              END-IF
032612           END-IF
           END-IF       
                          
06657                                                                   
100518     IF (CL-CLAIM-TYPE = WS-LIFE-OVERRIDE-L1 OR 'O')
06659        AND CP-RETURN-CODE = ZERO                                  
06660          MOVE CP-REMAINING-AMT   TO  CM-LF-REMAINING-AMT          
06661          MOVE +1                 TO  WS-REWRITE-CERT.             
06662                                                                   
06663      IF OPT-RESERVE-METHOD-AUTH                                   
06664          GO TO 3000-USE-OPTIONAL-CALCULATOR.                      
06665                                                                   
121902*    IF CL-SYSTEM-IDENTIFIER NOT = 'CV'                           
06667 *        MOVE CM-AH-CRITICAL-PERIOD  TO  CP-CRITICAL-MONTHS.      
080613     if cl-critical-period not numeric
080613        move zeros               to cl-critical-period
080613     end-if
080613     move cl-critical-period     to cp-critical-months.
06668                                                                   
06669      MOVE ZERO                   TO  CP-RESERVE-REMAINING-TERM.   
06670                                                                   

032612     if ws-company-id = 'AHL' OR 'FNL'
032612        move low-values          to cp-expire-dt
100518        IF CL-CLAIM-TYPE = WS-LIFE-OVERRIDE-L1 OR 'O'
032612           MOVE cm-lf-loan-expire-dt
032612                                 to cp-expire-dt
032612        ELSE                                                         
032612           MOVE cm-ah-loan-expire-dt
032612                                 to cp-expire-dt
032612        end-if
032612     END-IF

06671      IF CP-RETURN-CODE = ZERO                                     
      *       display ' about to call elresvx ' cl-claim-no ' '
      *         cp-critical-months ' ' cp-remaining-term
      *         ' ' cp-remaining-amt

06672          CALL 'ELRESVX' USING CALCULATION-PASS-AREA               
06673          IF CP-RETURN-CODE NOT = ZERO                             
06674              MOVE 'ERROR OCCURRED RESERVE CALCULATIONS'           
06675                                        TO  WS-D1-MESSAGE          
06676              MOVE WS-DETAIL1           TO  PRT                    
06677              PERFORM WRITE-A-LINE                                 
06678              MOVE SPACES               TO  WS-DETAIL1             
06679              MOVE 'RETURN CODE='       TO  WS-D1-MESSAGE          
06680              MOVE CP-RETURN-CODE       TO  WS-D1-RETURN-CODE      
06681              MOVE WS-DETAIL1           TO  PRT                    
06682              PERFORM WRITE-A-LINE                                 
06683              MOVE SPACES               TO  WS-DETAIL1             
06684              MOVE 'RESERVES='          TO  WS-D1-MESSAGE          
06685              MOVE CP-CDT-TABLE         TO  WS-D1-CDT-TABLE        
06686              MOVE CP-CDT-FACTOR        TO  WS-D1-CDT-FACTOR       
06687              MOVE CP-PTC-RESERVE       TO  WS-D1-PTC-RESERVE      
06688              MOVE CP-IBNR-RESERVE      TO  WS-D1-IBNR-RESERVE     
06689              MOVE CP-FUTURE-RESERVE    TO  WS-D1-FUTURE-RESERVE   
06690              MOVE WS-DETAIL1           TO  PRT                    
06691              PERFORM WRITE-A-LINE                                 
06692              MOVE SPACES               TO  WS-DETAIL1.            
06693                                                                   
06694      IF CLAIM-IS-CLOSED                                           
06695          MOVE ZERO               TO  CP-FUTURE-RESERVE            
06696                                      CP-PTC-RESERVE.              

020816     IF (WS-COMPANY-ID = 'DCC' or 'VPP')
              AND (AM-DCC-PRODUCT-CODE = 'DDF')
              MOVE 'N'                 TO AT-IBNR-SW
           END-IF

100518     IF CL-CLAIM-TYPE = WS-LIFE-OVERRIDE-L1 OR 'O'
072810        IF (CL-TOTAL-PAID-AMT > +0)
072810           AND (CP-PTC-RESERVE > +0)
072810           COMPUTE CP-PTC-RESERVE = CP-PTC-RESERVE
072810              - CL-TOTAL-PAID-AMT
072810           IF CP-PTC-RESERVE < +0
072810              MOVE +0            TO CP-PTC-RESERVE
072810           END-IF
072810        END-IF
072810     END-IF

06697                                                                   
121902*    IF WS-COMPANY-ID = 'ITG'                                     
121902*        IF CL-CLAIM-TYPE = WS-AH-OVERRIDE-L1 OR 'I' OR 'G'
121902*            IF CP-CDT-TABLE = 1 OR 2 OR 3 OR 4                   
121902*                COMPUTE CP-FUTURE-RESERVE =                      
121902*                        CP-FUTURE-RESERVE * 1.20                 
121902*            ELSE                                                 
121902*                IF CP-CDT-TABLE = 5                              
121902*                    COMPUTE CP-FUTURE-RESERVE =                  
121902*                            CP-FUTURE-RESERVE * 1.05.            
06707                                                                   
052614     IF CL-CLAIM-TYPE = WS-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
022122        OR 'B' OR 'H'
121902*      IF (WS-COMPANY-ID NOT = 'NCL' AND 'CVL' AND 'CNL')         
06710         IF CP-FUTURE-RESERVE GREATER THAN CP-REMAINING-AMT        
06711             MOVE '*'             TO  EX-AA-FUTURE-RESERVE-FLAG    
06712             MOVE CP-REMAINING-AMT TO  CP-FUTURE-RESERVE.          

080613     if ws-company-id not = 'AHL' AND 'FNL'
080613        if cl-claim-type = ws-ah-override-l1 or 'I' or 'G'
022122               OR 'F' OR 'B' OR 'H'
080613           if cl-critical-period > zeros
080613              compute ws-rem-bens = (cl-critical-period -
080613                (cl-total-paid-amt / cp-original-benefit)) *
080613                cp-original-benefit
080613*             display ' rem bens ' ws-company-id ' ' cl-claim-no
080613*                ' ' ws-rem-bens
072514              if ws-rem-bens < zeros
072514                 move zeros      to ws-rem-bens
072514              end-if
080613              evaluate true
080613                 when cp-ptc-reserve > ws-rem-bens
080613                    move ws-rem-bens
080613                                 to cp-ptc-reserve
080613                    move zeros   to cp-future-reserve
080613*                   display ' adj ptc > rem ben ' cp-ptc-reserve
080613                    move '&'     to ex-aa-future-reserve-flag
080613                 when cp-future-reserve > ws-rem-bens
080613                    compute cp-future-reserve = ws-rem-bens -
080613                       cp-ptc-reserve
080613*                   display ' adj fut > rem ben '
080613*                             cp-future-reserve
080613                    move '&'     to ex-aa-future-reserve-flag
080613                 when (cp-future-reserve + cp-ptc-reserve) >
080613                    ws-rem-bens
080613                    compute cp-future-reserve = ws-rem-bens -
080613                       cp-ptc-reserve
080613*                   display ' adj tot > rem ben '
080613*                        cp-future-reserve
080613                    move '&'     to ex-aa-future-reserve-flag
080613              end-evaluate
080613           end-if
080613        end-if
080613     end-if

121902*    IF CL-CLAIM-TYPE = WS-AH-OVERRIDE-L1 OR 'I' OR 'G'
121902*      IF WS-COMPANY-ID  = 'MLI'                                  
121902*       IF CP-REMAINING-AMT GREATER THAN +0                       
121902*          COMPUTE WS-RESERVE-WORK =                              
121902*                  (CP-REMAINING-AMT - CP-PTC-RESERVE)            
121902*          IF WS-RESERVE-WORK LESS THAN +0                        
121902*             MOVE +0            TO CP-FUTURE-RESERVE             
121902*          ELSE                                                   
121902*             IF CP-FUTURE-RESERVE GREATER THAN WS-RESERVE-WORK   
121902*                MOVE '*'        TO EX-AA-FUTURE-RESERVE-FLAG     
121902*                MOVE WS-RESERVE-WORK TO CP-FUTURE-RESERVE.       
06725                                                                   
06726      MOVE CP-REMAINING-AMT       TO  EX-AA-REMAINING-BENEFIT.     
06727      MOVE CP-REMAINING-TERM      TO  EX-AA-REMAINING-TERM.        
06728                                                                   
06729      IF CP-INCURRED-AGE NOT NUMERIC                               
06730         MOVE +0                  TO CP-INCURRED-AGE.              
06731                                                                   
06732      MOVE CP-INCURRED-AGE        TO  EX-AA-INCURRED-AGE.          
06733                                                                   
06734      GO TO 3000-CONTINUE-2.                                       
06735                                                                   
06736  3000-USE-OPTIONAL-CALCULATOR.                                    
06737 ****  CALL OPTIONAL RESERVE CALCULATION MODULE  ****              
06738                                                                   
06739      IF CP-RETURN-CODE = ZERO                                     
06740          IF CLAIM-IS-OPEN                                         
06741              MOVE SPACES         TO  OP-PARM-DATA                 
06742              CALL 'ELRSVSPL' USING OPTIONAL-CALCULATION-PASS-AREA 
06743              IF OP-RETURN-CODE NOT = ZERO                         
06744                  MOVE 'ERROR OCCURRED OPT RSVS CALCULATIONS'      
06745                                  TO  WS-D1-MESSAGE                
06746                  MOVE WS-DETAIL1 TO  PRT                          
06747                  PERFORM WRITE-A-LINE                             
06748                  MOVE SPACES     TO  WS-DETAIL1                   
06749                  MOVE 'RETURN CODE='                              
06750                                  TO  WS-D1-MESSAGE                
06751                  MOVE OP-RETURN-CODE                              
06752                                  TO  WS-D1-RETURN-CODE            
06753                  MOVE WS-DETAIL1 TO  PRT                          
06754                  PERFORM WRITE-A-LINE                             
06755                  MOVE SPACES     TO  WS-DETAIL1                   
06756                  MOVE 'RESERVES='                                 
06757                                  TO  WS-D1-MESSAGE                
06758                  MOVE ZEROS      TO  WS-D1-CDT-TABLE              
06759                  MOVE OP-RESERVE-FACTOR                           
06760                                  TO  WS-D1-CDT-FACTOR             
06761                  MOVE OP-PTC-RESERVE                              
06762                                  TO  WS-D1-PTC-RESERVE            
06763                  MOVE ZEROS      TO  WS-D1-IBNR-RESERVE           
06764                  MOVE OP-FUTURE-RESERVE                           
06765                                  TO  WS-D1-FUTURE-RESERVE         
06766                  MOVE WS-DETAIL1 TO  PRT                          
06767                  PERFORM WRITE-A-LINE                             
06768                  MOVE SPACES     TO  WS-DETAIL1.                  
06769                                                                   
052614     IF CL-CLAIM-TYPE = WS-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
022122        OR 'B' OR 'H'
121902*      IF (WS-COMPANY-ID NOT = 'NCL' AND 'CVL' AND 'CNL')         
06772         IF OP-FUTURE-RESERVE GREATER THAN CP-REMAINING-AMT        
06773             MOVE '*'             TO  EX-AA-FUTURE-RSV-OPT-FLAG    
06774             MOVE CP-REMAINING-AMT TO  OP-FUTURE-RESERVE.          
06775                                                                   
121902*    IF CL-CLAIM-TYPE = WS-AH-OVERRIDE-L1 OR 'I' OR 'G'
121902*      IF WS-COMPANY-ID  = 'MLI'                                  
121902*       IF CP-REMAINING-AMT GREATER THAN +0                       
121902*          COMPUTE WS-RESERVE-WORK =                              
121902*                  (CP-REMAINING-AMT - OP-PTC-RESERVE)            
121902*          IF WS-RESERVE-WORK LESS THAN +0                        
121902*             MOVE +0            TO OP-FUTURE-RESERVE             
121902*          ELSE                                                   
121902*             IF OP-FUTURE-RESERVE GREATER THAN WS-RESERVE-WORK   
121902*                MOVE '*'        TO EX-AA-FUTURE-RSV-OPT-FLAG     
121902*                MOVE WS-RESERVE-WORK TO OP-FUTURE-RESERVE.       
06787                                                                   
06788      MOVE OP-REMAINING-AMT       TO  EX-AA-REMAINING-BENEFIT.     
06789      MOVE OP-REMAINING-TERM      TO  EX-AA-REMAINING-TERM.        
06790                                                                   
06791      IF  OP-INCURRED-AGE NOT NUMERIC                              
06792          MOVE +0                 TO  OP-INCURRED-AGE              
06793                                      EX-AA-INCURRED-AGE           
06794      ELSE                                                         
06795          MOVE OP-INCURRED-AGE    TO  EX-AA-INCURRED-AGE.          
06796                                                                   
06797  3000-CONTINUE-2.                                                 
06798                                                                   
06799      SUBTRACT WS-AUTO-PAY-AMT FROM AT-CURRENT-MANUAL-RESERVE      
06800                                                                   
06801      IF AT-CURRENT-MANUAL-RESERVE LESS THAN ZERO                  
06802        OR CLAIM-IS-CLOSED                                         
06803          MOVE ZERO               TO  AT-CURRENT-MANUAL-RESERVE.   
06804                                                                   
06805      IF (WS-MANUAL-SW = '1' OR 'Y') AND                           
06806         (AT-MANUAL-SW = '1' OR 'Y')                               
06807          NEXT SENTENCE                                            
06808      ELSE                                                         
06809          IF (WS-MANUAL-SW NOT = '1' AND 'Y')                      
06810              MOVE +0              TO  AT-CURRENT-MANUAL-RESERVE   
06811          ELSE                                                     
06812              IF (AT-MANUAL-SW NOT = '1' AND 'Y')                  
06813                  MOVE +0          TO  AT-CURRENT-MANUAL-RESERVE.  
06814                                                                   
06815                                                                   
06816      IF (WS-FUTURE-SW = '1' OR 'Y') AND                           
06817         (AT-FUTURE-SW = '1' OR 'Y')                               
06818          NEXT SENTENCE                                            
06819      ELSE                                                         
06820         IF (WS-FUTURE-SW NOT = '1' AND 'Y')                       
06821             MOVE +0              TO  CP-FUTURE-RESERVE            
06822                                      OP-FUTURE-RESERVE            
06823         ELSE                                                      
06824             IF (AT-FUTURE-SW NOT = '1' AND 'Y')                   
06825                 MOVE +0          TO  CP-FUTURE-RESERVE            
06826                                      OP-FUTURE-RESERVE.           
06827                                                                   
06828      IF (WS-PTC-SW = '1' OR 'Y') AND                              
06829         (AT-PTC-SW = '1' OR 'Y')                                  
06830          NEXT SENTENCE                                            
06831      ELSE                                                         
06832          IF (WS-PTC-SW NOT = '1' AND 'Y')                         
06833              MOVE +0             TO  CP-PTC-RESERVE               
06834                                      OP-PTC-RESERVE               
06835          ELSE                                                     
06836              IF (AT-PTC-SW NOT = '1' AND 'Y')                     
06837                  MOVE +0         TO  CP-PTC-RESERVE               
06838                                      OP-PTC-RESERVE.              
06839                                                                   
06840      IF (WS-IBNR-SW = '1' OR '2' OR 'Y') AND                      
06841         (AT-IBNR-SW = '1' OR '2' OR 'Y')                          
06842          NEXT SENTENCE                                            
06843      ELSE                                                         
06844          IF (WS-IBNR-SW NOT = '1' AND '2' AND 'Y')                
06845              MOVE +0             TO  CP-IBNR-RESERVE              
06846          ELSE                                                     
06847              IF (AT-IBNR-SW NOT = '1' AND '2' AND 'Y')            
06848                  MOVE +0         TO  CP-IBNR-RESERVE.             
06849                                                                   
06850      IF  OPT-RESERVE-METHOD-AUTH                                  
06851          MOVE OP-FUTURE-RESERVE  TO  AT-FUTURE-RESERVE            
06852                                      EX-AA-FUTURE-RSV-OPT         
06853          MOVE OP-PTC-RESERVE     TO  AT-PAY-CURRENT-RESERVE       
06854                                      EX-AA-PAY-CURRENT-RSV-OPT    
06855          MOVE ZEROS              TO  AT-IBNR-RESERVE              
06856                                      EX-AA-IBNR-RSV-OPT           
06857                                      EX-AA-FUTURE-RESERVE         
06858                                      EX-AA-PAY-CURRENT-RESERVE    
06859                                      EX-AA-IBNR-RESERVE           
06860          MOVE OP-RESERVE-FACTOR  TO  EX-AA-CIDA-FACTOR            
06861                                                                   
06862      ELSE                                                         
06863          MOVE CP-FUTURE-RESERVE  TO  AT-FUTURE-RESERVE            
06864                                      EX-AA-FUTURE-RESERVE         
06865          MOVE CP-PTC-RESERVE     TO  AT-PAY-CURRENT-RESERVE       
06866                                      EX-AA-PAY-CURRENT-RESERVE    
06867          MOVE CP-IBNR-RESERVE    TO  AT-IBNR-RESERVE              
06868                                      EX-AA-IBNR-RESERVE           
06869          MOVE CP-CDT-TABLE       TO  EX-AA-CDT-TABLE              
06870          MOVE CP-CDT-FACTOR      TO  EX-AA-CDT-FACTOR.            
06871                                                                   
06872  3050-BUILD-EXTRACT-A-RECORD-A.                                   
06873                                                                   
06874      IF CLAIM-IS-CLOSED                                           
06875        AND AT-CURRENT-MANUAL-RESERVE NOT GREATER THAN ZERO        
06876        AND AT-FUTURE-RESERVE         NOT GREATER THAN ZERO        
06877        AND AT-PAY-CURRENT-RESERVE    NOT GREATER THAN ZERO        
06878        AND AT-IBNR-RESERVE           NOT GREATER THAN ZERO        
06879          GO TO 3099-EXIT.                                         
06880                                                                   
06887      MOVE CM-INSURED-ISSUE-AGE   TO  EX-AA-INSURED-ISSUE-AGE.  
06888      MOVE AM-NAME                TO  EX-AA-ACCOUNT-NAME.       
06889      MOVE 'CR'                   TO  EX-AA-SYSTEM-IDENTIFIER.  
06890      MOVE CM-CLAIM-INTERFACE-SW  TO  EX-AA-CLAIM-INTERFACE-SW.
06891                                                                   
06892      MOVE WS-NAME-WORK           TO  EX-AA-INSURED-NAME.          
06893      MOVE WS-BIRTH-DATE          TO  EX-AA-INSURED-BIRTH-DT.      
06894      MOVE CL-INSURED-SEX-CD      TO  EX-AA-INSURED-SEX-CD.        
06895      MOVE CL-INCURRED-DT         TO  EX-AA-INCURRED-DT.           
06896      MOVE CL-REPORTED-DT         TO  EX-AA-REPORTED-DT.
06897                                                                   
06898      MOVE AT-CURRENT-MANUAL-RESERVE                               
06899                                  TO  EX-AA-MANUAL-RESERVE         
06900                                      EX-AA-MANUAL-RSV-OPT.        
06901      MOVE AT-ITD-PAID-EXPENSES   TO  EX-AA-NON-CHARGED-EXPENSES   
06902      MOVE AT-ITD-CHARGEABLE-EXPENSE TO EX-AA-CHARGED-EXPENSES     
06903      MOVE CL-PAID-THRU-DT        TO  EX-AA-PAID-THRU-DT
06904                                                                   
06905      IF BENEFIT-NOT-FOUND OR                                      
06906         PLAN-NOT-FOUND                                            
06907            MOVE ZEROS            TO  EX-AA-PAY-CURRENT-RESERVE    
06908                                      EX-AA-IBNR-RESERVE           
06909                                      EX-AA-FUTURE-RESERVE         
06910                                      EX-AA-MANUAL-RESERVE         
06911                                      EX-AA-PAY-CURRENT-RSV-OPT    
06912                                      EX-AA-IBNR-RSV-OPT           
06913                                      EX-AA-FUTURE-RSV-OPT         
06914                                      EX-AA-MANUAL-RSV-OPT.        
06915                                                                   
06916                                                                   
06918      IF CERT-PEND-ISSUE-ERROR                                 
06919          MOVE '2'                TO  EX-AA-CLAIM-INTERFACE-SW.    
06920                                                                   
06921      MOVE CL-PROCESSOR-ID        TO EX-AA-PROCESSOR-ID.           
06922                                                                   
06923      MOVE CL-LAST-PMT-DT         TO  EX-AA-LAST-PMT-DT.           
06924      MOVE CL-LAST-PMT-AMT        TO  EX-AA-LAST-PMT-AMT.          
06925      MOVE CL-LAST-MAINT-DT       TO  EX-AA-LAST-MAINT-DT.         
06926      MOVE CL-LAST-MAINT-TYPE     TO  EX-AA-MAINT-TYPE.            
06927      MOVE CL-SYSTEM-IDENTIFIER   TO  EX-AA-SYSTEM-IDENTIFIER.     
040902     MOVE CL-TOTAL-PAID-AMT      TO  EX-AA-TOTAL-PAID-AMT.

06929      MOVE CL-LAST-MAINT-DT       TO  WS-PROJECTED-CLOSE-DATE.     
06930                                                                   
06931      IF CL-NEXT-AUTO-PAY-DT GREATER THAN WS-PROJECTED-CLOSE-DATE  
06932          MOVE CL-NEXT-AUTO-PAY-DT                                 
06933                                  TO  WS-PROJECTED-CLOSE-DATE.     
06934                                                                   
06935      IF CL-NEXT-RESEND-DT GREATER THAN WS-PROJECTED-CLOSE-DATE    
06936          MOVE CL-NEXT-RESEND-DT  TO  WS-PROJECTED-CLOSE-DATE.     
06937                                                                   
06938      IF CL-NEXT-FOLLOWUP-DT GREATER THAN WS-PROJECTED-CLOSE-DATE  
06939          MOVE CL-NEXT-FOLLOWUP-DT                                 
06940                                  TO  WS-PROJECTED-CLOSE-DATE.     
06941                                                                   
06942      IF CL-LAST-PMT-DT GREATER THAN WS-PROJECTED-CLOSE-DATE       
06943          MOVE CL-LAST-PMT-DT     TO  WS-PROJECTED-CLOSE-DATE.     
06944                                                                   
052614     IF CL-CLAIM-TYPE = WS-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
022122        OR 'B' OR 'H'
06946          IF CL-PAID-THRU-DT GREATER THAN WS-PROJECTED-CLOSE-DATE  
06947              MOVE CL-PAID-THRU-DT                                 
06948                                  TO  WS-PROJECTED-CLOSE-DATE.     
06949                                                                   
06950      MOVE WS-PROJECTED-CLOSE-DATE TO DC-BIN-DATE-1.               
06951      MOVE WS-DAYS-BEFORE-CLOSED   TO  DC-ELAPSED-DAYS.             
06952      MOVE ZEROS                   TO  DC-ELAPSED-MONTHS.           
06953      MOVE '6'                     TO  DC-OPTION-CODE.              
06954      PERFORM 8500-DATE-CONVERSION.                                
06955      MOVE DC-BIN-DATE-2           TO  EX-AA-PROJECT-CLOSE-DT.      
06956                                                                   
06969      MOVE ZEROS                   TO  WS-GL-PTC                    
06970                                       WS-GL-IBNR                   
06971                                       WS-GL-FUTURE.                
06972                                                                   
06973      PERFORM 7000-RELEASE-RECORD.                                 
06974                                                                   
06975      ADD +1  TO  WS-EXTRACT-AA-COUNT.                             
06976                                                                   
121902*    IF  THIS-IS-MONTH-END                                        
121902*            AND                                                  
121902*        (WS-COMPANY-ID = 'AUK' OR 'AIG' OR 'CIG' OR 'CUK')       
121902*        PERFORM 4450-COMPLETE-EXTR-G-RECORD-A THRU 4450-EXIT.    
06981                                                                   
06982      IF WS-AUTO-PAY-AMT NOT = ZERO                                
06983          MOVE ZERO               TO  WS-AUTO-PAY-AMT.             
06984                                                                   
06985      MOVE BIN-RUN-DATE           TO  AT-LAST-COMPUTED-DT.         
06986                                                                   
06987      GO TO 3099-EXIT.                                             
06988                                                                   
06989  3092-MATCH-STATE-TABLE.                                          
06990                                                                   
06991      MOVE +0                     TO OP-CALC-INTEREST.             
06992                                                                   
06993      SET STATE-NDX               TO +1.                           
06994                                                                   
06995      SEARCH WS-STATE-GRP                                          
06996          VARYING STATE-NDX                                        
06997          WHEN                                                     
06998              WS-STATE-CODE (STATE-NDX) = CL-CERT-STATE            
06999              MOVE WS-STATE-CALC-INTEREST (STATE-NDX)              
07000                                  TO OP-CALC-INTEREST.             
07001                                                                   
07002      IF OP-CALC-INTEREST = +0                                     
07003          MOVE WS-CO-CALC-INTEREST TO OP-CALC-INTEREST.            
07004                                                                   
07005  3092-EXIT.                                                       
07006      EXIT.                                                        
07007                                                                   
07008  3099-EXIT.                                                       
07009      EXIT.                                                        
07010                                  EJECT                            
07011  3100-BUILD-EXTRACT-A-RECORD-B SECTION.                           
07012 *    NOTE ******************************************************* 
07013 *         *                      ELC310AB.                      * 
07014 *         *                                                     * 
07015 *         *             EXTRACT - A   RECORD - B                * 
07016 *         *                                                     * 
07017 *         *      EXTRACT FOR ALL PAYMENTS WITH CREDIT SELECT    * 
07018 *         *  DATES = THE CURRENT MONTH AND PRIOR MONTH          * 
07019 *         *******************************************************.
07020                                                                   
07021      IF WS-VOID-PAYMENT-SW = +1                                   
07022          MOVE ZERO               TO  WS-VOID-PAYMENT-SW           
07023          IF AT-PMT-ACCEPT-DT = LOW-VALUES                         
07024              NEXT SENTENCE                                        
07025            ELSE                                                   
07026              GO TO 3190-EXIT                                      
07027        ELSE                                                       
07028          IF AT-VOID-SELECT-DT NOT = LOW-VALUES                    
07029             AND AT-VOID-ACCEPT-DT = LOW-VALUES                    
07030              MOVE +1             TO  WS-VOID-PAYMENT-SW.          
07031                                                                   
07032      MOVE SPACES                 TO  REPORTS-EXTRACT-RECORD.      
07033                                                                   
07034      MOVE 'EX'                   TO  EX-RECORD-ID.                
07035      MOVE '1'                    TO  EX-POSITIONING-CODE.         
07036      MOVE 'A'                    TO  EX-EXTRACT-CODE.             
07037      MOVE WS-COMPANY-CODE        TO  EX-COMPANY-CD.               
07038      MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID.               
07039      MOVE 'B'                    TO  EX-RECORD-TYPE.              
07040                                                                   
07041      MOVE CL-CARRIER             TO  EX-SB-CARRIER.               
07042      MOVE CL-CLAIM-NO            TO  EX-AB-CLAIM-NO.              
07043      MOVE CL-CERT-NO             TO  EX-AB-CERT-NO.               
07044                                                                   
07045      MOVE CL-CERT-CARRIER        TO  EX-AB-CARRIER.               
07046      MOVE CL-CERT-STATE          TO  EX-AB-STATE.                 
07047      MOVE CL-CERT-ACCOUNT        TO  EX-AB-ACCOUNT.               
07048      MOVE CL-CERT-GROUPING       TO  EX-AB-GROUPING.              
07049      MOVE CL-CERT-EFF-DT         TO  EX-AB-CERT-EFF-DT.           
07050      MOVE AT-CLAIM-TYPE          TO  EX-AB-CLAIM-TYPE.            
07051      MOVE AT-SEQUENCE-NO         TO  EX-AB-TRAILER-SEQ-NO.        
07052      MOVE CL-CAUSE-CD            TO  EX-AB-CAUSE-CD.              
07053      MOVE CL-CLAIM-PREM-TYPE     TO  EX-AB-CLAIM-PREM-TYPE.       
07054                                                                   
07055      MOVE AT-PAYMENT-TYPE        TO  EX-AB-PAYMENT-TYPE.          
07056      MOVE AT-CHECK-NO            TO  EX-AB-CHECK-NO               
07057                                      EX-SB-CHECK-NO.              
07058      MOVE AT-PAYMENT-ORIGIN      TO  EX-AB-PAYMENT-ORIGIN.        
07059      MOVE AT-PAID-FROM-DT        TO  EX-AB-PAID-FROM-DT.          
07060                                                                   
07061      MOVE AT-VOID-SELECT-DT      TO  EX-AB-VOID-SELECT-DT.        
07062      MOVE AT-PMT-SELECT-DT       TO  EX-AB-PMT-SELECT-DT.         
07063                                                                   
07064      MOVE CL-REPORTED-DT         TO  EX-AB-REPORTED-DT.           
07065      MOVE CL-INCURRED-DT         TO  EX-AB-INCURRED-DT.           
07066                                                                   
07067      MOVE AT-CHECK-WRITTEN-DT    TO  EX-AB-CHECK-WRITTEN-DT.      
07068                                                                   
07069      IF WS-VOID-PAYMENT-SW = +1                                   
07070          MOVE AT-VOID-DT             TO  EX-AB-VOID-DT            
07071          MULTIPLY AT-AMOUNT-PAID BY -1 GIVING EX-AB-PAYMENT-AMOUNT
07072          MOVE AT-PREV-PAID-THRU-DT    TO  EX-AB-PAID-THRU-DT      
07073        ELSE                                                       
07074          MOVE AT-PAID-THRU-DT    TO  EX-AB-PAID-THRU-DT           
07075          MOVE LOW-VALUES         TO  EX-AB-VOID-DT                
07076          MOVE AT-AMOUNT-PAID     TO  EX-AB-PAYMENT-AMOUNT.        
07077                                                                   
07078      IF OFFLINE-PMT                                               
07079        AND EX-AB-CHECK-WRITTEN-DT = LOW-VALUES                    
07080          MOVE AT-RECORDED-DT    TO  EX-AB-CHECK-WRITTEN-DT.       
07081                                                                   
07082      MOVE WS-BIRTH-DATE          TO  DC-BIN-DATE-1.               
07083      MOVE CL-INCURRED-DT         TO  DC-BIN-DATE-2.               
07084      MOVE '1'                    TO  DC-OPTION-CODE.              
07085      PERFORM 8500-DATE-CONVERSION.                                
07086      DIVIDE DC-ELAPSED-MONTHS BY +12 GIVING EX-AB-INCURRED-AGE.   
07087                                                                   
07088      MOVE AT-DAYS-IN-PERIOD      TO  EX-AB-DAYS-IN-PERIOD.        
07089      MOVE AT-FORCE-CONTROL       TO  EX-AB-FORCE-CONTROL.         
07090      MOVE AT-ADDL-RESERVE        TO  EX-AB-ADDL-RESERVE.          
07091                                                                   
07092      MOVE CL-SYSTEM-IDENTIFIER   TO  EX-AB-SYSTEM-IDENTIFIER.     
07093                                                                   
121902*    IF CL-SYSTEM-IDENTIFIER = 'CV'                               
121902*        GO TO 3170-MOVE-POLICY-DATA.                             
07096                                                                   
07097  3160-MOVE-CERTIFICATE-DATA.                                      
07098                                                                   
07099      MOVE CM-CLAIM-INTERFACE-SW  TO  EX-AB-CLAIM-INTERFACE-SW.    
07100                                                                   
07101      IF CERT-PEND-ISSUE-ERROR                                     
07102         MOVE '2'                 TO  EX-AB-CLAIM-INTERFACE-SW.    
07103                                                                   
052614     IF CL-CLAIM-TYPE = WS-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
022122        OR 'B' OR 'H'
07105         MOVE CM-AH-BENEFIT-CD    TO  EX-AB-BENEFIT-CODE           
07106         MOVE CM-ENTRY-STATUS     TO  EX-AB-CERT-STATUS            
07107      ELSE                                                         
07108         MOVE CM-LF-BENEFIT-CD    TO  EX-AB-BENEFIT-CODE           
07109         MOVE CM-ENTRY-STATUS     TO  EX-AB-CERT-STATUS.           
07110                                                                   
07111      IF AT-PAYMENT-TYPE = ('4' OR '5' OR '6')                     
07112        OR AT-PMT-ACCEPT-DT NOT = LOW-VALUES                       
07113          GO TO 3180-BUILD-EXTRACT-A-RECORD-B.                     
07114                                                                   
07115      IF (CM-O-B-COVERAGE OR CM-OPEN-END)                          
07116        AND                                                        
121902        (AT-PREV-LAST-PMT-DT = LOW-VALUES)                       
121902*        OR (WS-COMPANY-ID = 'FIA' AND                               
121902*         WS-MONTH-END-DATE = WS-022984))                         
07120              MOVE '1'            TO  EX-AB-1ST-PAYMENT-SW.        
07121                                                                   
07122      GO TO 3180-BUILD-EXTRACT-A-RECORD-B.                         
07123                                                                   
07124  3170-MOVE-POLICY-DATA.                                           
07125                                                                   
07126      MOVE PM-CLAIM-INTERFACE-SW  TO  EX-AB-CLAIM-INTERFACE-SW.    
07127      MOVE PM-INS-PLAN-CD         TO  EX-AB-BENEFIT-CODE.          
07128      MOVE PM-ENTRY-STATUS        TO  EX-AB-CERT-STATUS.           
07129                                                                   
07130  3180-BUILD-EXTRACT-A-RECORD-B.                                   
07131                                                                   
07132      PERFORM 7000-RELEASE-RECORD.                                 
07133                                                                   
07134      ADD +1  TO  WS-EXTRACT-AB-COUNT.                             
07135                                                                   
07136      IF WS-VOID-PAYMENT-SW = +1                                   
07137          GO TO 3100-BUILD-EXTRACT-A-RECORD-B.                     
07138                                                                   
07139  3190-EXIT.                                                       
07140      EXIT.                                                        
07141                                  EJECT                            
07142  3200-BUILD-EXTRACT-B-RECORD-A SECTION.                           
07143 *    NOTE ******************************************************* 
07144 *         *             EXTRACT - B   RECORD - A                * 
07145 *         *                                                     * 
07146 *         *      EXTRACT FOR ALL PAYMENTS WITH CREDIT SELECT    * 
07147 *         *  DATES = THE CURRENT MONTH AND PRIOR MONTH          * 
07148 *         *******************************************************.
07149                                                                   
07150      IF WS-VOID-PAYMENT-SW = +1                                   
07151         MOVE ZERO                TO  WS-VOID-PAYMENT-SW           
07152      ELSE                                                         
07153         PERFORM 2300-GET-ACCOUNT                                  
07154         IF AT-VOID-SELECT-DT NOT =  LOW-VALUES                    
07155            MOVE +1               TO  WS-VOID-PAYMENT-SW.          
07156                                                                   
07157      MOVE SPACES                 TO  REPORTS-EXTRACT-RECORD.      
07158                                                                   
07159      MOVE 'EX'                   TO  EX-RECORD-ID.                
07160      MOVE '9'                    TO  EX-POSITIONING-CODE.         
07161      MOVE 'B'                    TO  EX-EXTRACT-CODE.             
07162      MOVE WS-COMPANY-CODE        TO  EX-COMPANY-CD.               
07163      MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID.               
07164      MOVE 'A'                    TO  EX-RECORD-TYPE.              
07165                                                                   
07166      MOVE CL-CARRIER             TO  EX-SB-CARRIER.               
07167      MOVE AT-CHECK-NO            TO  EX-SB-CHECK-NO.              
07168                                                                   
07169      MOVE CL-CERT-NO             TO  EX-BA-CERT-NO.               
07170      MOVE CL-CLAIM-NO            TO  EX-BA-CLAIM-NO.              
07171      MOVE CL-CERT-STATE          TO  EX-BA-STATE.                 
07172      MOVE CL-CERT-ACCOUNT        TO  EX-BA-ACCOUNT.               
07173                                                                   
07177      MOVE AM-NAME                TO  EX-BA-ACCOUNT-NAME.          
051104     MOVE AM-REPORT-CODE-2       TO  EX-BA-REPORT-CODE-2.
051804     MOVE AM-REPORT-CODE-1       TO  EX-BA-REPORT-CODE-1.

012607     MOVE AT-SEQUENCE-NO         TO  EX-BA-PMT-SEQ-NO

07179      MOVE CL-CERT-GROUPING       TO  EX-BA-GROUPING.              
07180      MOVE CL-CERT-EFF-DT         TO  EX-BA-CERT-EFF-DT.           
07181      MOVE AT-CLAIM-TYPE          TO  EX-BA-CLAIM-TYPE.            
07182      MOVE CL-CLAIM-PREM-TYPE     TO  EX-BA-CLAIM-PREM-TYPE.       
07183      MOVE AT-PAYMENT-TYPE        TO  EX-BA-PAYMENT-TYPE.          
07184      MOVE AT-PAID-FROM-DT        TO  EX-BA-PAID-FROM-DT.          
07185      MOVE AT-PAID-THRU-DT        TO  EX-BA-PAID-THRU-DT.          
07186      MOVE CL-INCURRED-DT         TO  EX-BA-INCURRED-DT.           
07187      MOVE CL-REPORTED-DT         TO  EX-BA-REPORTED-DT.           
07188      MOVE AT-CHECK-WRITTEN-DT    TO  EX-BA-CHECK-WRITTEN-DT.      
07189      MOVE AT-AMOUNT-PAID         TO  EX-BA-PAYMENT-AMOUNT.        
07190      MOVE CL-CAUSE-CD            TO  EX-BA-LOSS-CODE.             
07191                                                                   
07192      IF OFFLINE-PMT                                               
07193        AND EX-AB-CHECK-WRITTEN-DT = LOW-VALUES                    
07194          MOVE AT-RECORDED-DT    TO  EX-AB-CHECK-WRITTEN-DT.       
07195                                                                   
07196      MOVE WS-BIRTH-DATE          TO  DC-BIN-DATE-1.               
07197      MOVE BIN-RUN-DATE           TO  DC-BIN-DATE-2.               
07198      MOVE '1'                    TO  DC-OPTION-CODE.              
07199      PERFORM 8500-DATE-CONVERSION.                                
07200      DIVIDE DC-ELAPSED-MONTHS BY +12 GIVING EX-BA-INSURED-AGE.    
07201                                                                   
07202      MOVE WS-NAME-WORK           TO  EX-BA-INSURED-LAST-NAME.     
07203      MOVE CL-INSURED-SEX-CD      TO  EX-BA-INSURED-SEX-CD.        
07204                                                                   
07205      MOVE AT-DAYS-IN-PERIOD      TO  EX-BA-DAYS-IN-PERIOD.        
07206      MOVE AT-PAYEE-TYPE          TO  EX-BA-PAYEE-TYPE-CD.         
07207      MOVE AT-PAYEES-NAME         TO  EX-BA-PAYEES-NAME.           
07208                                                                   
07209      IF WS-VOID-PAYMENT-SW = +1                                   
07210         MOVE AT-VOID-DT          TO  EX-BA-VOID-DT                
07211      ELSE                                                         
07212         MOVE LOW-VALUES          TO  EX-BA-VOID-DT.               
07213                                                                   
07214      MOVE CL-CLAIM-STATUS        TO  EX-BA-CLAIM-STATUS.          
07215                                                                   
121902*    IF CL-SYSTEM-IDENTIFIER = 'CV'                               
121902*        MOVE PM-INS-PLAN-CD            TO  EX-BA-BENEFIT-CD      
121902*        MOVE PM-LOAN-TERM              TO  EX-BA-ORIG-TERM       
121902*        MOVE PM-TOTAL-PREM-RECVD       TO  EX-BA-PREMIUM-AMT     
121902*        MOVE PM-ENTRY-STATUS           TO  EX-BA-CERT-STATUS     
121902*        MOVE PM-SOC-SEC-NO             TO  EX-BA-SOC-SEC-NO      
121902*        IF AT-CLAIM-TYPE = WS-AH-OVERRIDE-L1 OR 'I' OR 'G'
121902*            MOVE PM-INS-MONTH-BENEFIT  TO  EX-BA-BENEFIT-AMT     
121902*            GO TO 3250-CONT-BUILD-BA-RECORD                      
121902*        ELSE                                                     
121902*            MOVE PM-INS-TOTAL-BENEFIT  TO  EX-BA-BENEFIT-AMT     
121902*            GO TO 3250-CONT-BUILD-BA-RECORD.                     
07228                                                                   
052614     IF AT-CLAIM-TYPE = WS-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
022122        OR 'B' OR 'H'
07230          MOVE CM-AH-BENEFIT-CD   TO  EX-BA-BENEFIT-CD             
07231          MOVE CM-AH-ORIG-TERM    TO  EX-BA-ORIG-TERM              
07232          MOVE CM-AH-BENEFIT-AMT  TO  EX-BA-BENEFIT-AMT            
07233          MOVE CM-AH-PREMIUM-AMT  TO  EX-BA-PREMIUM-AMT            
07234        ELSE                                                       
07235          MOVE CM-LF-BENEFIT-AMT  TO  EX-BA-BENEFIT-AMT            
07236          MOVE CM-LF-PREMIUM-AMT  TO  EX-BA-PREMIUM-AMT            
07237          MOVE CM-LF-BENEFIT-CD   TO  EX-BA-BENEFIT-CD             
07238          MOVE CM-LF-ORIG-TERM    TO  EX-BA-ORIG-TERM.             
07239                                                                   
07240      MOVE CM-ENTRY-STATUS        TO EX-BA-CERT-STATUS.            
07241      MOVE CM-RESIDENT-STATE      TO EX-BA-RESIDENT-STATE.         
07242                                                                   
022106*    IF WS-COMPANY-ID = 'DMD'
022406*       MOVE CL-SOC-SEC-NO       TO EX-BA-SOC-SEC-NO
022106*    ELSE
022106*       MOVE CM-SOC-SEC-NO       TO EX-BA-SOC-SEC-NO
022106*    END-IF
022106     MOVE CL-SOC-SEC-NO          TO EX-BA-SOC-SEC-NO
07247                                                                   
07248      MOVE CM-MEMBER-NO           TO  EX-BA-MEMBER-NUMBER.         
07249      MOVE CM-IND-GRP-TYPE        TO  EX-BA-IND-GRP-TYPE.          
07250                                                                   
07251  3250-CONT-BUILD-BA-RECORD.                                       
07252                                                                   
07253      MOVE AT-PAYMENT-ORIGIN      TO  EX-BA-PAYMENT-ORIGIN.        
07254                                                                   
07255      IF (AT-PMT-ACCEPT-DT = LOW-VALUES                            
07256         OR                                                        
07257          (AT-VOID-SELECT-DT NOT = LOW-VALUES AND                  
07258           AT-VOID-ACCEPT-DT = LOW-VALUES))                        
07259        AND                                                        
07260          (AT-CHECK-WRITTEN-DT NOT = LOW-VALUES AND                
07261           AT-CHECK-WRITTEN-DT NOT GREATER THAN BIN-RUN-DATE)      
07262              MOVE '*'            TO  EX-BA-RESERVE-FLAG.          
07263                                                                   
07264      MOVE AT-EXPENSE-TYPE        TO  EX-BA-EXPENSE-TYPE.          
07265      MOVE AT-EXPENSE-PER-PMT     TO  EX-BA-EXPENSE-PER-PMT.       
07266      MOVE AT-RECORDED-DT         TO  EX-BA-PMT-RECORDED-DT.       
07267      MOVE CL-BENEFICIARY         TO  EX-BA-BENEFICIARY.           
07268                                                                   
07269      MOVE CL-SYSTEM-IDENTIFIER   TO  EX-BA-SYSTEM-IDENTIFIER.     
07270      MOVE WS-LAST-PROCESS-DT     TO  EX-BA-LAST-PROCESS-DT.       
07271                                                                   
07272      IF (CL-LEGAL-STATE = SPACES OR LOW-VALUES)                   
07273          MOVE CL-CERT-STATE      TO  EX-BA-LEGAL-STATE            
07274      ELSE                                                         
07275          MOVE CL-LEGAL-STATE     TO  EX-BA-LEGAL-STATE.           
07276                                                                   
07277      IF WS-VOID-PAYMENT-SW = +1                                   
07278          MOVE AT-VOID-DT             TO  DC-BIN-DATE-1            
07279      ELSE                                                         
07280          MOVE AT-CHECK-WRITTEN-DT    TO  DC-BIN-DATE-1.           
07281                                                                   
07282      MOVE ' '                        TO  DC-OPTION-CODE.          
07283      PERFORM 8500-DATE-CONVERSION.                                
07284      MOVE DC-GREG-DATE-CYMD          TO  WS-PMT-VOID-DATE-CYMD.   
07285                                                                   
121902*    IF (WS-COMPANY-ID = 'AUK' OR 'AIG' OR 'CIG' OR 'CUK')        
121902*        MOVE ZEROS              TO  WS-GL-PTC                    
121902*                                    WS-GL-IBNR                   
121902*                                    WS-GL-FUTURE                 
121902*        MOVE SPACES             TO  WS-DOING-RESERVES-SW         
121902*        MOVE AT-ASSOCIATES      TO  EX-BA-ASSOCIATES             
121902*        PERFORM 4400-BUILD-EXTRACT-G-RECORD-A THRU 4400-EXIT.    
07293                                                                   
121902     IF WS-COMPANY-ID = 'DMD'
121902        PERFORM 3291-CONVERT-PAYEE-CODE
                                       THRU 3291-CONVERT-EXIT
121902         MOVE CL-CERT-NO (1:2)   TO EX-BA-STATE
121902         MOVE WS-DIAGNOSIS-DESCRIP (1:30)
121902                                 TO EX-BA-ACCOUNT-NAME
           END-IF        

121902     PERFORM 3291-CONVERT-PAYEE-CODE
                                       THRU 3291-CONVERT-EXIT
121902     MOVE CL-TOTAL-PAID-AMT      TO EX-BA-TOTAL-PAID-AMT      
121902     MOVE CM-LF-REMAINING-AMT    TO EX-BA-LF-REMAINING-AMT    
121902     MOVE WS-INSURED-ADDR-SEQ-NO TO EX-BA-INSURED-ADDR-SEQ    
121902     MOVE WS-PAYEE-ADDR-SEQ-NO   TO EX-BA-PAYEE-ADDR-SEQ      
121902     MOVE AT-RECORDED-BY         TO EX-BA-RECORDED-BY         
121902     MOVE CL-FILE-ESTABLISH-DT   TO EX-BA-FILE-ESTABLISHED-DT 
121902     MOVE CL-LAST-PMT-DT         TO EX-BA-LAST-PMT-DT  
121902     MOVE CL-LAST-REOPEN-DT      TO EX-BA-LAST-REOPEN-DT      
121902     MOVE CL-INSURED-BIRTH-DT    TO EX-BA-INSURED-BIRTH-DT
07308                                                                   
07309      PERFORM 7000-RELEASE-RECORD.                                 
07310                                                                   
07311      ADD +1                          TO  WS-EXTRACT-BA-COUNT.     
07312                                                                   
121902*    IF (WS-COMPANY-ID = 'AUK' OR 'AIG' OR 'CIG' OR 'CUK')        
121902*        PERFORM 4450-COMPLETE-EXTR-G-RECORD-A THRU 4450-EXIT.    
07315                                                                   
07316      IF WS-VOID-PAYMENT-SW = +1 AND                               
07317          AT-PMT-SELECT-DT = WS-MONTH-END-DATE                     
07318              GO TO 3200-BUILD-EXTRACT-B-RECORD-A.                 
07319                                                                   
07320  3290-EXIT.                                                       
07321      EXIT.                                                        
07322                                  EJECT                            
07323  3291-CONVERT-PAYEE-CODE.                                         
07324      MOVE AT-PAYEE-SEQ           TO WS-PAYEE-ADDR-SEQ-NO.         
07325      IF BENEFICIARY-PAID                                          
07326          MOVE ZEROS              TO WS-PAYEE-ADDR-SEQ-NO          
07327      ELSE                                                         
07328      IF ACCOUNT-PAID                                              
07329          COMPUTE WS-PAYEE-ADDR-SEQ-NO = WS-PAYEE-ADDR-SEQ-NO + 20 
07330      ELSE                                                         
07331      IF DOCTOR-PAID                                               
07332          COMPUTE WS-PAYEE-ADDR-SEQ-NO = WS-PAYEE-ADDR-SEQ-NO + 30 
07333      ELSE                                                         
07334      IF EMPLOYER-PAID                                             
07335          COMPUTE WS-PAYEE-ADDR-SEQ-NO = WS-PAYEE-ADDR-SEQ-NO + 40 
07336      ELSE                                                         
07337      IF OTHER-1-PAID                                              
07338          COMPUTE WS-PAYEE-ADDR-SEQ-NO = WS-PAYEE-ADDR-SEQ-NO + 50 
07339      ELSE                                                         
07340      IF OTHER-2-PAID                                              
07341          COMPUTE WS-PAYEE-ADDR-SEQ-NO = WS-PAYEE-ADDR-SEQ-NO + 60.
07342                                                                   
07343  3291-CONVERT-EXIT.                                               
07344      EXIT.                                                        
07345                                  EJECT                            
07346                                                                   
07347  3300-BUILD-EXTRACT-C-RECORD-A SECTION.                           
07348 *    NOTE ******************************************************* 
07349 *         *             EXTRACT - C   RECORD - A                * 
07350 *         *                                                     * 
07351 *         *      EXTRACT FOR ALL CHECKS WRITTEN IN CURRENT AND  * 
07352 *         *  PRIOR MONTH, BASED ON CHECK WRITTEN DATE.          * 
07353 *         *******************************************************.
07354                                                                   
07355      MOVE SPACES                 TO  REPORTS-EXTRACT-RECORD.      
07356                                                                   
07357      MOVE 'EX'                   TO  EX-RECORD-ID.                
07358      MOVE '9'                    TO  EX-POSITIONING-CODE.         
07359      MOVE 'C'                    TO  EX-EXTRACT-CODE.             
07360      MOVE WS-COMPANY-CODE        TO  EX-COMPANY-CD.               
07361      MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID.               
07362      MOVE 'A'                    TO  EX-RECORD-TYPE.              
07363                                                                   
07364      IF WS-ELCHKQ-SW = +1                                         
07365         MOVE CQ-CARRIER          TO EX-SB-CARRIER                 
07366         MOVE CQ-CLAIM-NO         TO EX-CA-CLAIM-NO                
07367         MOVE CQ-CERT-NO          TO EX-CA-CERT-NO                 
07368         MOVE CQ-CLAIM-TYPE       TO EX-CA-CLAIM-TYPE              
07369         MOVE SPACES              TO EX-CA-CLAIM-PREM-TYPE         
07370         MOVE SPACES              TO EX-CA-PAYEE-TYPE-CD           
07371      ELSE                                                         
07372         MOVE AT-PMT-SELECT-DT    TO EX-CA-PMT-SELECT-DT           
07373         MOVE AT-PAYEE-TYPE-CD    TO EX-CA-PAYEE-TYPE-CD           
07374         MOVE CL-CARRIER          TO EX-SB-CARRIER                 
07375         MOVE CL-CERT-NO          TO EX-CA-CERT-NO                 
07376         MOVE CL-CLAIM-NO         TO EX-CA-CLAIM-NO                
07377         MOVE AT-CLAIM-TYPE       TO EX-CA-CLAIM-TYPE              
07378         MOVE CL-CLAIM-PREM-TYPE  TO EX-CA-CLAIM-PREM-TYPE         
07379         MOVE CL-SYSTEM-IDENTIFIER TO EX-CA-SYSTEM-IDENTIFIER.     
07380                                                                   
07381      IF WS-VOID-PAYMENT-SW = +1                                   
07382         MOVE AT-VOID-DT          TO  EX-CA-VOID-DT                
07383         IF WS-ELCHKQ-SW NOT = +1                                  
07384            MOVE AT-VOID-SELECT-DT TO EX-CA-PMT-SELECT-DT.         
07385                                                                   
07386 *    NOTE ******************************************************* 
07387 *         *                                                     * 
07388 *         *      IF WS-ELCHKQ-SW = +1 THEN THIS WAS             * 
07389 *         *  PERFORMED FROM 2700-PROCESS-CHECK-QUEUE.           * 
07390 *         *                                                     * 
07391 *         *******************************************************.
07392                                                                   
07393      IF WS-ELCHKQ-SW = +1 OR +2                                   
07394          MOVE CQ-CHECK-NUMBER    TO  EX-SB-CHECK-NO               
07395          MOVE CQ-ENTRY-TYPE      TO  EX-CA-ENTRY-TYPE             
07396          MOVE CQ-CONTROL-NUMBER  TO  EX-CA-CONTROL-NUMBER         
07397          MOVE CQ-TIMES-PRINTED   TO  EX-CA-TIMES-PRINTED          
07398          MOVE CQ-PRINT-AT-HHMM   TO  EX-CA-PRINT-AT-HHMM          
07399          MOVE CQ-CHECK-WRITTEN-DT TO EX-CA-CHECK-WRITTEN-DATE     
07400          MOVE CQ-CHECK-AMOUNT    TO  EX-CA-PAYMENT-AMOUNT         
07401        ELSE                                                       
07402          MOVE AT-CHECK-NO        TO  EX-SB-CHECK-NO               
07403          MOVE 'Q'                TO  EX-CA-ENTRY-TYPE             
07404          MOVE ZERO               TO  EX-CA-CONTROL-NUMBER         
07405          MOVE +1                 TO  EX-CA-TIMES-PRINTED          
07406          MOVE ZERO               TO  EX-CA-PRINT-AT-HHMM          
07407          MOVE AT-CHECK-WRITTEN-DT TO EX-CA-CHECK-WRITTEN-DATE     
07408          MOVE AT-AMOUNT-PAID     TO  EX-CA-PAYMENT-AMOUNT.        
07409                                                                   
07410      IF WS-ELCHKQ-SW = +1 AND                                     
07411         CQ-ENTRY-TYPE = ('A' OR 'X')                              
07412          MOVE ZERO               TO  EX-CA-PAYMENT-AMOUNT         
07413        ELSE                                                       
07414          MOVE AT-PAYMENT-TYPE    TO  EX-CA-PAYMENT-TYPE           
07415          MOVE AT-RECORDED-BY     TO  EX-CA-CHECK-BY-USER          
07416          IF WS-VOID-PAYMENT-SW NOT = ZERO                         
07417              MULTIPLY -1 BY EX-CA-PAYMENT-AMOUNT.                 
07418                                                                   
07419      IF (WS-ELCHKQ-SW = +1) AND                                   
07420         (CQ-ENTRY-TYPE = ('A' OR 'S' OR 'X'))                     
07421         NEXT SENTENCE                                             
07422      ELSE                                                         
121902*        IF CL-SYSTEM-IDENTIFIER = 'CV'                           
121902*            MOVE PM-ENTRY-STATUS  TO  EX-CA-CERT-STATUS          
121902*        ELSE                                                     
07426              MOVE CM-ENTRY-STATUS  TO EX-CA-CERT-STATUS.          
07427                                                                   
07428      PERFORM 7000-RELEASE-RECORD.                                 
07429                                                                   
07430      ADD +1                        TO  WS-EXTRACT-CA-COUNT.       
07431                                                                   
07432      IF WS-ELCHKQ-SW = +1                                         
07433        AND CQ-ENTRY-TYPE = ('A' OR 'S' OR 'X')                    
07434          GO TO 3390-EXIT.                                         
07435                                                                   
07436      IF WS-VOID-PAYMENT-SW = +1                                   
07437          MOVE ZERO               TO  WS-VOID-PAYMENT-SW           
07438          GO TO 3390-EXIT.                                         
07439                                                                   
07440      IF WS-ELCHKQ-SW = +0 OR +2                                   
07441         IF AT-VOID-DT NOT = LOW-VALUES                            
07442             MOVE +1                 TO  WS-VOID-PAYMENT-SW        
07443             GO TO 3300-BUILD-EXTRACT-C-RECORD-A.                  
07444                                                                   
07445  3390-EXIT.                                                       
07446      EXIT.                                                        

DAN01                                  EJECT                            
DAN01  3395-BUILD-EXTRACT-D-RECORD-A SECTION.                           
DAN01 *    NOTE ******************************************************* 
DAN01 *         *             EXTRACT - D   RECORD - A                * 
DAN01 *         *                                                     * 
DAN01 *         *      AUTO PAYMENTS RELEASED  (CSO)                  * 
DAN01 *         *******************************************************.
DAN01                                                                   
DAN01      MOVE SPACES                 TO  REPORTS-EXTRACT-RECORD.      
DAN01                                                                   
DAN01      MOVE 'EX'                   TO  EX-RECORD-ID.                
DAN01      MOVE '2'                    TO  EX-POSITIONING-CODE.         
DAN01      MOVE 'D'                    TO  EX-EXTRACT-CODE.             
DAN01      MOVE WS-COMPANY-CODE        TO  EX-COMPANY-CD.               
DAN01      MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID.               
DAN01      MOVE 'A'                    TO  EX-RECORD-TYPE.              
DAN01      MOVE CL-CARRIER             TO  EX-SF-CARRIER.               
DAN01      MOVE CL-CLAIM-NO            TO  EX-SF-CLAIM-NO.              
DAN01      MOVE CL-CERT-NO             TO  EX-SF-CERT-NO.               
DAN01      MOVE +0                     TO  EX-SF-DAYS-TO-PAYMENT.       
DAN01      MOVE AM-NAME                TO  EX-DA-ACCT-NAME
DAN01      PERFORM 5000-MOVE-NAME                                       
DAN01      MOVE WS-NAME-WORK           TO  EX-DA-INSURED-NAME           
DAN01      MOVE AT-CLAIM-PREM-TYPE     TO  EX-DA-CLAIM-PREM-TYPE        
DAN01      MOVE AT-PAYMENT-TYPE        TO  EX-DA-PAYMENT-TYPE           
DAN01      MOVE AT-AMOUNT-PAID         TO  EX-DA-PAYMENT-AMOUNT         
DAN01      MOVE WS-INTERVAL-MONTHS     TO  EX-DA-INTERVAL-MONTHS        
DAN01      MOVE AT-PAID-THRU-DT        TO  EX-DA-PAID-THRU-DT           
DAN01      MOVE AT-CHECK-NO            TO  EX-DA-CHECK-NUMBER           
DAN01      MOVE WS-SCHEDULE-END-DT     TO  EX-DA-SCHEDULE-END-DT        
DAN01      MOVE WS-LAST-PMT-TYPE       TO  EX-DA-LAST-PAYMENT-TYPE      
DAN01                                                                   
DAN01      PERFORM 7000-RELEASE-RECORD.                                 
DAN01      ADD +1  TO  WS-EXTRACT-DA-COUNT.                             
DAN01  3395-EXIT.                                                       
DAN01      EXIT.                                                        

07447                                  EJECT                            
07448  3400-BUILD-EXTRACT-D-RECORD-B SECTION.                           
07449 *    NOTE ******************************************************* 
07450 *         *             EXTRACT - D   RECORD - B                * 
07451 *         *                                                     * 
07452 *         *      GENERATED FOR LETTERS SCHEDULED TO BE RESENT   * 
07453 *         *  (AND NOT YET RECEIVED).  RESEND DATES SELECTED FOR * 
07454 *         *  CURRENT DATE AND PLUS OR MINUS 2 DAYS.             * 
07455 *         *******************************************************.
07456                                                                   
07457      MOVE SPACES                 TO  REPORTS-EXTRACT-RECORD.      
07458                                                                   
07459      MOVE 'EX'                   TO  EX-RECORD-ID.                
07460      MOVE '2'                    TO  EX-POSITIONING-CODE.         
07461      MOVE 'D'                    TO  EX-EXTRACT-CODE.             
07462      MOVE WS-COMPANY-CODE        TO  EX-COMPANY-CD.               
07463      MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID.               
07464      MOVE 'B'                    TO  EX-RECORD-TYPE.              
07465                                                                   
07466      MOVE CL-CARRIER             TO  EX-SE-CARRIER.               
07467      MOVE CL-CLAIM-NO            TO  EX-SE-CLAIM-NO.              
07468      MOVE CL-CERT-NO             TO  EX-SE-CERT-NO.               
07469                                                                   
07470      MOVE CL-CLAIM-TYPE          TO  EX-DB-CLAIM-TYPE.            
07471                                                                   
122002     EVALUATE TRUE
122002     WHEN EX-DB-CLAIM-TYPE = WS-AH-OVERRIDE-L1                      
07473          MOVE WS-AH-OVERRIDE-L2   TO EX-DB-OVERRIDE-L2             

122002     WHEN EX-DB-CLAIM-TYPE = WS-LIFE-OVERRIDE-L1
07475          MOVE WS-LIFE-OVERRIDE-L2 TO EX-DB-OVERRIDE-L2

122002     WHEN EX-DB-CLAIM-TYPE = 'I'
122002         MOVE 'IU'                TO EX-DB-OVERRIDE-L2

122002     WHEN EX-DB-CLAIM-TYPE = 'G'
122002         MOVE 'GP'                TO EX-DB-OVERRIDE-L2
052614
052614     WHEN EX-DB-CLAIM-TYPE = 'F'
052614         MOVE 'FL'                TO EX-DB-OVERRIDE-L2
100518
100518     WHEN EX-DB-CLAIM-TYPE = 'O'
100518         MOVE 'OT'                TO EX-DB-OVERRIDE-L2
022122     WHEN EX-DB-CLAIM-TYPE = 'B'
022122         MOVE 'BR'                TO EX-DB-OVERRIDE-L2
022122
022122     WHEN EX-DB-CLAIM-TYPE = 'H'
022122         MOVE 'HS'                TO EX-DB-OVERRIDE-L2

122002     END-EVALUATE.
07476                                                                   
07477      IF AT-TRAILER-TYPE = '4'                                     
07478          MOVE AT-AUTO-RE-SEND-DT     TO  EX-SE-ACTION-DATE        
07479          MOVE AT-LETTER-ARCHIVE-NO   TO  EX-DB-LETTER-ARCHIVE-NO  
07480          MOVE AT-STD-LETTER-FORM     TO  EX-DB-STD-LETTER-FORM    
07481          MOVE AT-LETTER-SENT-DT      TO  EX-DB-LETTER-SENT-DT     
07482          MOVE AT-AUTO-RE-SEND-DT     TO  EX-DB-AUTO-RE-SEND-DT    
07483          MOVE AT-RECORDED-BY         TO  EX-DB-RECORDED-BY        
07484          MOVE AT-ADDRESEE-TYPE       TO  EX-DB-ADDRESEE-TYPE      
07485          MOVE AT-REASON-TEXT         TO  EX-DB-REASON             
07486          MOVE AT-INITIAL-PRINT-DATE  TO  EX-DB-INITIAL-PRINT-DT   
07487        ELSE                                                       
07488      IF AT-TRAILER-TYPE = 'A'                                     
07489          MOVE AT-FORM-RE-SEND-DT     TO  EX-SE-ACTION-DATE        
07490          MOVE -1                     TO  EX-DB-LETTER-ARCHIVE-NO  
07491          MOVE AT-FORM-TYPE           TO  EX-DB-STD-LETTER-FORM    
07492          MOVE AT-FORM-SEND-ON-DT     TO  EX-DB-LETTER-SENT-DT     
07493          MOVE AT-FORM-RE-SEND-DT     TO  EX-DB-AUTO-RE-SEND-DT    
07494          MOVE AT-RECORDED-BY         TO  EX-DB-RECORDED-BY        
07495          MOVE AT-FORM-ADDRESS        TO  EX-DB-ADDRESEE-TYPE      
07496          MOVE AT-INSTRUCT-LN-1       TO  EX-DB-REASON.            
07497                                                                   
07498      PERFORM 7000-RELEASE-RECORD.                                 
07499                                                                   
07500      ADD +1  TO  WS-EXTRACT-DB-COUNT.                             
07501                                                                   
07502  3490-EXIT.                                                       
07503      EXIT.                                                        
07504                                  EJECT                            
07505  3500-BUILD-EXTRACT-D-RECORD-C SECTION.                           
07506 *    NOTE ******************************************************* 
07507 *         *             EXTRACT - D   RECORD - C                * 
07508 *         *                                                     * 
07509 *         *      SELECTED FOR CLAIMS WHOSE LAST MAINTENANCE DATE* 
07510 *         *  IS 45 DAYS PRIOR TO CURRENT-DATE.                  * 
07511 *         *******************************************************.
07512                                                                   
07513      MOVE SPACES                 TO  REPORTS-EXTRACT-RECORD.      
07514                                                                   
07515      MOVE 'EX'                   TO  EX-RECORD-ID.                
07516      MOVE '2'                    TO  EX-POSITIONING-CODE.         
07517      MOVE 'D'                    TO  EX-EXTRACT-CODE.             
07518      MOVE WS-COMPANY-CODE        TO  EX-COMPANY-CD.               
07519      MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID.               
07520      MOVE 'C'                    TO  EX-RECORD-TYPE.              
07521                                                                   
121902*    IF WS-COMPANY-ID = 'DMD'                                     
121902*        MOVE CL-LAST-MAINT-USER TO  EX-SC2-PROCESSOR             
121902*        MOVE CL-CLAIM-NO        TO  EX-SC2-CLAIM-NO              
121902*        MOVE CL-CERT-NO         TO  EX-SC2-CERT-NO               
121902*        MOVE CL-CARRIER         TO  EX-SC2-CARRIER               
121902*    ELSE                                                         
061406*07528          MOVE CL-LAST-MAINT-DT   TO  EX-SE-ACTION-DATE.            
061406         MOVE WS-LAST-MAINT-DT   TO  EX-SE-ACTION-DATE.
07529          MOVE CL-CARRIER         TO  EX-SE-CARRIER.                
07530          MOVE CL-CLAIM-NO        TO  EX-SE-CLAIM-NO.               
07531          MOVE CL-CERT-NO         TO  EX-SE-CERT-NO.               
07532                                                                   
07533      MOVE CL-CLAIM-TYPE          TO  EX-DC-CLAIM-TYPE.            
07534                                                                   
122002     EVALUATE TRUE
122002     WHEN EX-DC-CLAIM-TYPE = WS-AH-OVERRIDE-L1                      
07536          MOVE WS-AH-OVERRIDE-L2   TO EX-DC-OVERRIDE-L2             

122002     WHEN EX-DC-CLAIM-TYPE = WS-LIFE-OVERRIDE-L1
07538          MOVE WS-LIFE-OVERRIDE-L2 TO EX-DC-OVERRIDE-L2

122002     WHEN EX-DC-CLAIM-TYPE = 'I'
122002         MOVE 'IU'                TO EX-DC-OVERRIDE-L2

122002     WHEN EX-DC-CLAIM-TYPE = 'G'
122002         MOVE 'GP'                TO EX-DC-OVERRIDE-L2
052614
052614     WHEN EX-DC-CLAIM-TYPE = 'F'
052614         MOVE 'FL'                TO EX-DC-OVERRIDE-L2
100518
100518     WHEN EX-DC-CLAIM-TYPE = 'O'
100518         MOVE 'OT'                TO EX-DC-OVERRIDE-L2

022122     WHEN EX-DC-CLAIM-TYPE = 'B'
022122         MOVE 'BR'                TO EX-DC-OVERRIDE-L2
022122
022122     WHEN EX-DC-CLAIM-TYPE = 'H'
022122         MOVE 'HS'                TO EX-DC-OVERRIDE-L2

122002     END-EVALUATE.
07539                                                                   
07540      MOVE CL-INCURRED-DT         TO  EX-DC-INCURRED-DT.           
07541      MOVE CL-FILE-ESTABLISH-DT   TO  EX-DC-FILE-ESTABLISH-DT.     
07542      MOVE CL-PAID-THRU-DT        TO  EX-DC-PAID-THRU-DT.          
061406*07543      MOVE CL-LAST-MAINT-DT       TO  EX-DC-LAST-MAINT-DT.         
061406*07544      MOVE CL-LAST-MAINT-USER     TO  EX-DC-RECORDED-BY.           
061406*07545      MOVE CL-LAST-MAINT-TYPE     TO  EX-DC-MAINT-TYPE.            
061406     MOVE WS-LAST-MAINT-DT       TO  EX-DC-LAST-MAINT-DT.
061406     MOVE WS-LAST-MAINT-USER     TO  EX-DC-RECORDED-BY.           
061406     MOVE WS-LAST-MAINT-TYPE     TO  EX-DC-MAINT-TYPE.            
07546                                                                   
121902*    IF WS-COMPANY-ID = 'DMD'                                     
121902*        MOVE 'N'                TO WS-WRITE-EXTRACT-D-RECORD-C   
121902*        MOVE CL-LAST-PMT-DT     TO EX-DC-LAST-MAINT-DT           
121902*        MOVE WS-DMD-LAST-PMT-RECORDED-BY                         
121902*                                TO EX-DC-RECORDED-BY             
121902*        MOVE '1'                TO EX-DC-MAINT-TYPE.             
07553                                                                   
07554      MOVE CL-INSURED-LAST-NAME   TO  EX-DC-INSURED-LAST-NAME.     
07555      MOVE CL-INSURED-1ST-NAME    TO  EX-DC-INSURED-FIRST-NAME.    
07556      MOVE CL-INSURED-MID-INIT    TO  EX-DC-INSURED-MID-INIT.      
07557      MOVE CL-CCN-A5              TO  EX-DC-CREDIT-CARD-NO.        
07558                                                                   
07559      PERFORM 7000-RELEASE-RECORD.                                 
07560                                                                   
07561      ADD +1  TO  WS-EXTRACT-DC-COUNT.                             
07562                                                                   
07563  3590-EXIT.                                                       
07564      EXIT.                                                        
07565                                  EJECT                            
07566  3600-BUILD-EXTRACT-D-RECORD-D SECTION.                           
07567 *    NOTE ******************************************************* 
07568 *         *                      ELC310DD.                      * 
07569 *         *                                                     * 
07570 *         *             EXTRACT - D   RECORD - D                * 
07571 *         *                                                     * 
07572 *         *      EXTRACT FOR ALL CLAIMS CLOSED BY AUTOMATIC     * 
07573 *         *  PROCESSING.                                        * 
07574 *         *******************************************************.
07575                                                                   
07576      MOVE SPACES                 TO  REPORTS-EXTRACT-RECORD.      
07577                                                                   
07578      MOVE 'EX'                   TO  EX-RECORD-ID.                
07579      MOVE '2'                    TO  EX-POSITIONING-CODE.         
07580      MOVE 'D'                    TO  EX-EXTRACT-CODE.             
07581      MOVE WS-COMPANY-CODE        TO  EX-COMPANY-CD.               
07582      MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID.               
07583      MOVE 'D'                    TO  EX-RECORD-TYPE.              
07584                                                                   
07585      MOVE CL-CARRIER             TO  EX-SA-CARRIER.               
07586      MOVE CL-CLAIM-NO            TO  EX-SA-CLAIM-NO.              
07587      MOVE CL-CERT-NO             TO  EX-SA-CERT-NO.               
07588                                                                   
07589      MOVE CL-CLAIM-TYPE          TO  EX-DD-CLAIM-TYPE.            
07590                                                                   
122002     EVALUATE TRUE
122002     WHEN EX-DD-CLAIM-TYPE = WS-AH-OVERRIDE-L1                      
07592          MOVE WS-AH-OVERRIDE-L2   TO EX-DD-OVERRIDE-L2             

122002     WHEN EX-DD-CLAIM-TYPE = WS-LIFE-OVERRIDE-L1
07594          MOVE WS-LIFE-OVERRIDE-L2 TO EX-DD-OVERRIDE-L2 
          
122002     WHEN EX-DD-CLAIM-TYPE = 'I'
122002         MOVE 'IU'                TO EX-DD-OVERRIDE-L2

122002     WHEN EX-DD-CLAIM-TYPE = 'G'
122002         MOVE 'GP'                TO EX-DD-OVERRIDE-L2
052614
052614     WHEN EX-DD-CLAIM-TYPE = 'F'
052614         MOVE 'FL'                TO EX-DD-OVERRIDE-L2
100518
100518     WHEN EX-DD-CLAIM-TYPE = 'O'
100518         MOVE 'OT'                TO EX-DD-OVERRIDE-L2

022122     WHEN EX-DD-CLAIM-TYPE = 'B'
022122         MOVE 'BR'                TO EX-DD-OVERRIDE-L2
022122     WHEN EX-DD-CLAIM-TYPE = 'H'
022122         MOVE 'HS'                TO EX-DD-OVERRIDE-L2

122002     END-EVALUATE.
07595                                                                   
07596      MOVE CL-INCURRED-DT         TO  EX-DD-INCURRED-DT.           
07597      MOVE CL-FILE-ESTABLISH-DT   TO  EX-DD-FILE-ESTABLISH-DT.     
07598      MOVE CL-PAID-THRU-DT        TO  EX-DD-PAID-THRU-DT.          
07599      MOVE CL-LAST-MAINT-DT       TO  EX-DD-LAST-MAINT-DT.         
07600      MOVE CL-LAST-MAINT-USER     TO  EX-DD-RECORDED-BY.           
07601      MOVE CL-LAST-MAINT-TYPE     TO  EX-DD-MAINT-TYPE.            
07602      MOVE CL-INSURED-LAST-NAME   TO  EX-DD-INSURED-LAST-NAME.     
07603      MOVE CL-INSURED-1ST-NAME    TO  EX-DD-INSURED-FIRST-NAME.    
07604      MOVE CL-CERT-ACCOUNT        TO  EX-DD-ACCOUNT.               
07605                                                                   
07606      IF WS-PURGE-SW = +1                                          
07607          MOVE '1'                TO  EX-DD-CLAIM-PURGED-SW        
07608          MOVE ZERO               TO  WS-PURGE-SW.                 
080510
080510     MOVE WS-DROP-CLAIM-SW       TO  EX-DD-DROP-CLAIM-SW.
07609                                                                   
07610      PERFORM 7000-RELEASE-RECORD.                                 
07611                                                                   
07612      ADD +1  TO  WS-EXTRACT-DD-COUNT.                             
07613                                                                   
07614  3690-EXIT.                                                       
07615      EXIT.                                                        
07616                                  EJECT                            
07617  3700-BUILD-EXTRACT-D-RECORD-E SECTION.                           
07618 *    NOTE ******************************************************* 
07619 *         *             EXTRACT - D   RECORD - E                * 
07620 *         *                                                     * 
07621 *         *      IF A CHECK QUE RECORD HAS BEEN CREATED BUT     * 
07622 *         *  NEVER PRINTED.                                     * 
07623 *         *******************************************************.
07624                                                                   
07625      MOVE SPACES                 TO  REPORTS-EXTRACT-RECORD.      
07626                                                                   
07627      MOVE 'EX'                   TO  EX-RECORD-ID.                
07628      MOVE '2'                    TO  EX-POSITIONING-CODE.         
07629      MOVE 'D'                    TO  EX-EXTRACT-CODE.             
07630      MOVE WS-COMPANY-CODE        TO  EX-COMPANY-CD.               
07631      MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID.               
07632      MOVE 'E'                    TO  EX-RECORD-TYPE.              
07633                                                                   
07634      MOVE CL-CARRIER             TO  EX-SA-CARRIER.               
07635      MOVE CL-CLAIM-NO            TO  EX-SA-CLAIM-NO.              
07636      MOVE CL-CERT-NO             TO  EX-SA-CERT-NO.               
07637      MOVE AT-CHECK-NO            TO  EX-DE-CHECK-NO.              
07638                                                                   
07639      MOVE CL-CERT-NO             TO  EX-DE-CERT-NO.               
07640      MOVE CL-CLAIM-NO            TO  EX-DE-CLAIM-NO.              
07641      MOVE CL-CLAIM-TYPE          TO  EX-DE-CLAIM-TYPE.            
07642                                                                   
07643      MOVE AT-CHECK-QUE-CONTROL   TO  EX-DE-CONTROL-NUMBER.        
07644      MOVE CL-CLAIM-PREM-TYPE     TO  EX-DE-CLAIM-PREM-TYPE.       
07645      MOVE AT-PAYMENT-TYPE        TO  EX-DE-PAYMENT-TYPE.          
07646      MOVE ZERO                   TO  EX-DE-PRINT-AT-HHMM.         
07647      MOVE AT-AMOUNT-PAID         TO  EX-DE-PAYMENT-AMOUNT.        
07648      MOVE AT-RECORDED-DT         TO  EX-DE-CHECK-WRITTEN-DATE.    
07649      MOVE AT-RECORDED-BY         TO  EX-DE-CHECK-BY-USER.         
07650                                                                   
07651      IF PAYMENT-NOT-QUEUED                                        
07652          MOVE ZERO               TO  EX-DE-TIMES-PRINTED          
07653        ELSE                                                       
07654          MOVE CQ-TIMES-PRINTED   TO  EX-DE-TIMES-PRINTED.         
07655                                                                   
07656      PERFORM 7000-RELEASE-RECORD.                                 
07657                                                                   
07658      ADD +1  TO  WS-EXTRACT-DE-COUNT.                             
07659                                                                   
07660  3790-EXIT.                                                       
07661      EXIT.                                                        
07662                                  EJECT                            
07663  3800-BUILD-EXTRACT-D-RECORD-F SECTION.                           
07664 *    NOTE ******************************************************* 
07665 *         *             EXTRACT - D   RECORD - F                * 
07666 *         *                                                     * 
07667 *         *      CLAIMS THAT HAVE A COUNT IN EITHER FATAL OR    * 
07668 *         *  FORCABLE ERRORS.                                   * 
07669 *         *******************************************************.
07670                                                                   
07671      MOVE SPACES                 TO  REPORTS-EXTRACT-RECORD.      
07672                                                                   
07673      MOVE 'EX'                   TO  EX-RECORD-ID.                
07674      MOVE '2'                    TO  EX-POSITIONING-CODE.         
07675      MOVE 'D'                    TO  EX-EXTRACT-CODE.             
07676      MOVE WS-COMPANY-CODE        TO  EX-COMPANY-CD.               
07677      MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID.               
07678      MOVE 'F'                    TO  EX-RECORD-TYPE.              
07679                                                                   
07680      MOVE CL-CARRIER             TO  EX-SA-CARRIER.               
07681      MOVE CL-CLAIM-NO            TO  EX-SA-CLAIM-NO.              
07682      MOVE CL-CERT-NO             TO  EX-SA-CERT-NO.               
07683                                                                   
07684      MOVE CL-CLAIM-TYPE          TO  EX-DF-CLAIM-TYPE.            
07685                                                                   
122002     EVALUATE TRUE
122002     WHEN EX-DF-CLAIM-TYPE = WS-AH-OVERRIDE-L1                      
07687          MOVE WS-AH-OVERRIDE-L2   TO EX-DF-OVERRIDE-L2             

122002     WHEN EX-DF-CLAIM-TYPE = WS-LIFE-OVERRIDE-L2
07689          MOVE WS-LIFE-OVERRIDE-L2 TO EX-DF-OVERRIDE-L2

122002     WHEN EX-DF-CLAIM-TYPE = 'I'
122002         MOVE 'IU'                TO EX-DF-OVERRIDE-L2

122002     WHEN EX-DF-CLAIM-TYPE = 'G'
122002         MOVE 'GP'                TO EX-DF-OVERRIDE-L2
052614
052614     WHEN EX-DF-CLAIM-TYPE = 'F'
052614         MOVE 'FL'                TO EX-DF-OVERRIDE-L2
100518
100518     WHEN EX-DF-CLAIM-TYPE = 'O'
100518         MOVE 'OT'                TO EX-DF-OVERRIDE-L2

022122     WHEN EX-DF-CLAIM-TYPE = 'B'
022122         MOVE 'BR'                TO EX-DF-OVERRIDE-L2
022122
022122     WHEN EX-DF-CLAIM-TYPE = 'H'
022122         MOVE 'HS'                TO EX-DF-OVERRIDE-L2

122002     END-EVALUATE.
07690                                                                   
07691      MOVE CL-INCURRED-DT         TO  EX-DF-INCURRED-DT.           
07692      MOVE CL-FILE-ESTABLISH-DT   TO  EX-DF-FILE-ESTABLISH-DT.     
07693      MOVE CL-PAID-THRU-DT        TO  EX-DF-PAID-THRU-DT.          
07694      MOVE CL-LAST-MAINT-DT       TO  EX-DF-LAST-MAINT-DT.         
07695      MOVE CL-LAST-MAINT-USER     TO  EX-DF-RECORDED-BY.           
07696      MOVE CL-FATAL-ERROR-CNT     TO  EX-DF-FATAL-ERROR-CNT.       
07697      MOVE WS-FORCE-COUNT         TO  EX-DF-FORCABLE-ERROR-CNT.    
07698      MOVE CL-LAST-MAINT-TYPE     TO  EX-DF-MAINT-TYPE.            
07699                                                                   
07700      PERFORM 7000-RELEASE-RECORD.                                 
07701                                                                   
07702      ADD +1  TO  WS-EXTRACT-DF-COUNT.                             
07703                                                                   
07704  3890-EXIT.                                                       
07705      EXIT.                                                        
07706                                  EJECT                            
07707  3900-BUILD-EXTRACT-D-RECORD-G SECTION.                           
07708 *    NOTE ******************************************************* 
07709 *         *             EXTRACT - D   RECORD - G                * 
07710 *         *                                                     * 
07711 *         *      PROMPTS ARE EXTRACTED BEGINNING WITH THE START * 
07712 *         *  NOTIFICATION DATE AND WILL CONTINUE THRU TWO DAYS  * 
07713 *         *  AFTER THE STOP NOTIFICATION DATE.                  * 
07714 *         *******************************************************.
07715                                                                   
07716      MOVE SPACES                 TO  REPORTS-EXTRACT-RECORD.      
07717                                                                   
07718      MOVE 'EX'                   TO  EX-RECORD-ID.                
07719      MOVE '2'                    TO  EX-POSITIONING-CODE.         
07720      MOVE 'D'                    TO  EX-EXTRACT-CODE.             
07721      MOVE WS-COMPANY-CODE        TO  EX-COMPANY-CD.               
07722      MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID.               
07723      MOVE 'G'                    TO  EX-RECORD-TYPE.              
07724                                                                   
07725      MOVE CL-CARRIER             TO  EX-SE3-CARRIER.              
07726      MOVE CL-CLAIM-NO            TO  EX-SE3-CLAIM-NO.             
07727      MOVE CL-CERT-NO             TO  EX-SE3-CERT-NO.              
07728      MOVE CL-PROCESSOR-ID        TO  EX-SE3-PROCESSOR.            
07729                                                                   
07730      MOVE CL-CLAIM-TYPE          TO  EX-DG-CLAIM-TYPE.            
07731                                                                   
122002     EVALUATE TRUE
122002     WHEN EX-DG-CLAIM-TYPE = WS-AH-OVERRIDE-L1                      
07733          MOVE WS-AH-OVERRIDE-L2   TO EX-DG-OVERRIDE-L2             

122002     WHEN EX-DG-CLAIM-TYPE = WS-LIFE-OVERRIDE-L1
07735          MOVE WS-LIFE-OVERRIDE-L2 TO EX-DG-OVERRIDE-L2

122002     WHEN EX-DG-CLAIM-TYPE = 'I'
122002         MOVE 'IU'                TO EX-DG-OVERRIDE-L2

122002     WHEN EX-DG-CLAIM-TYPE = 'G'
122002         MOVE 'GP'                TO EX-DG-OVERRIDE-L2
052614
052614     WHEN EX-DG-CLAIM-TYPE = 'F'
052614         MOVE 'FL'                TO EX-DG-OVERRIDE-L2
100518
100518     WHEN EX-DG-CLAIM-TYPE = 'O'
100518         MOVE 'OT'                TO EX-DG-OVERRIDE-L2

022122     WHEN EX-DG-CLAIM-TYPE = 'B'
022122         MOVE 'BR'                TO EX-DG-OVERRIDE-L2
022122
022122     WHEN EX-DG-CLAIM-TYPE = 'H'
022122         MOVE 'HS'                TO EX-DG-OVERRIDE-L2

122002     END-EVALUATE. 
07736                                                                   
07737      MOVE WS-NAME-WORK           TO  EX-DG-INSURED-LAST-NAME.     
07738      MOVE CL-FILE-LOCATION       TO  EX-DG-FILE-LOCATION.         
07739      MOVE CL-INCURRED-DT         TO  EX-DG-INCURRED-DT.           
07740      MOVE AT-RECORDED-BY         TO  EX-DG-RECORDED-BY.           

050619     if (ex-dg-recorded-by = 'SYST')
050619        and (ws-323g-maint-user <> spaces)
050619        move ws-323g-maint-user to ex-dg-recorded-by
050619     end-if
DAN01                                                                   
DAN01      IF AT-PAYMENT-ORIGIN = '2'                                   
DAN01         MOVE '0'  TO  EX-SE3-SORT-CODE                            
DAN01      ELSE                                                         
DAN01         MOVE '1'  TO  EX-SE3-SORT-CODE                            
DAN01      END-IF                                                       
07741                                                                   
050619     IF AT-TRAILER-TYPE = '7'  *> Auto prompt Trlr
07743          MOVE AT-PROMPT-END-DT   TO  EX-SE3-ACTION-DATE           
07744          MOVE AT-PROMPT-LINE-1   TO  EX-DG-TEXT-LINE-1            
07745          MOVE AT-PROMPT-LINE-2   TO  EX-DG-TEXT-LINE-2            
07746        ELSE                                                       
050619     IF AT-TRAILER-TYPE = '4'   *> Correspondence Trlr
07748          MOVE AT-RECEIPT-FOLLOW-UP  TO  EX-SE3-ACTION-DATE        
07749          MOVE 'LETTER SENT XX/XX/XX TO'  TO  EX-DG-TEXT-LINE-1    
07750          MOVE AT-LETTER-SENT-DT  TO  DC-BIN-DATE-1                
07751          MOVE SPACES             TO  DC-OPTION-CODE               
07752          PERFORM 8500-DATE-CONVERSION                             
07753          MOVE DC-GREG-DATE-1-EDIT  TO  EX-DG-LETTER-SENT-DT       
07754          MOVE AT-REASON-TEXT     TO  EX-DG-TEXT-LINE-2            
07755          MOVE AT-ADDRESSEE-NAME  TO  EX-DG-ADRESSEE-NAME          
07756          IF AT-ADDRESEE-TYPE = '1'                                
07757              MOVE 'INSURED   '   TO  EX-DG-ADRESSEE-TYPE          
07758            ELSE                                                   
07759          IF AT-ADDRESEE-TYPE = '2'                                
07760              MOVE 'BENEFICIARY'  TO  EX-DG-ADRESSEE-TYPE          
07761            ELSE                                                   
07762          IF AT-ADDRESEE-TYPE = '3'                                
07763              MOVE 'ACCOUNT    '  TO  EX-DG-ADRESSEE-TYPE          
07764            ELSE                                                   
07765          IF AT-ADDRESEE-TYPE = '4'                                
07766              MOVE 'PHYSICIAN  '  TO  EX-DG-ADRESSEE-TYPE          
07767            ELSE                                                   
07768          IF AT-ADDRESEE-TYPE = '5'                                
07769              MOVE 'EMPLOYER   '  TO  EX-DG-ADRESSEE-TYPE          
07770            ELSE                                                   
07771          IF AT-ADDRESEE-TYPE = '6'                                
07772              MOVE 'OTHER 1    '  TO  EX-DG-ADRESSEE-TYPE          
07773            ELSE                                                   
07774          IF AT-ADDRESEE-TYPE = '7'                                
07775              MOVE 'OTHER 2    '  TO  EX-DG-ADRESSEE-TYPE.         
07776                                                                   
121902*    IF WS-COMPANY-ID = 'UFL' OR 'WSL' OR 'UFR' OR 'WFL'          
121902*        MOVE CL-PROCESSOR-ID    TO  EX-SE2-PROCESSOR             
121902*        MOVE CL-CARRIER         TO  EX-SE2-CARRIER               
121902*        MOVE CL-CLAIM-NO        TO  EX-SE2-CLAIM-NO              
121902*        MOVE CL-CERT-NO         TO  EX-SE2-CERT-NO               
121902*        IF AT-TRAILER-TYPE = '7'                                 
121902*            MOVE AT-PROMPT-END-DT   TO  EX-SE2-ACTION-DATE       
121902*          ELSE                                                   
121902*            MOVE AT-RECEIPT-FOLLOW-UP  TO  EX-SE2-ACTION-DATE.   
080510
080510     MOVE WS-DROP-CLAIM-SW      TO EX-DG-DROP-CLAIM-SW.
07786                                                                   
07787      PERFORM 7000-RELEASE-RECORD.                                 
07788                                                                   
07789      ADD +1  TO  WS-EXTRACT-DG-COUNT.                             
07790                                                                   
07791  3949-EXIT.                                                       
07792      EXIT.                                                        
07793                                  EJECT                            
07794  3950-BUILD-EXTRACT-D-RECORD-H SECTION.                           
07795 *    NOTE ******************************************************* 
07796 *         *             EXTRACT - D   RECORD - H                * 
07797 *         *                                                     * 
07798 *         *      ALL PAYMENTS THAT HAVE NOT BEEN APPROVED       * 
07799 *         *******************************************************.
07800                                                                   
07801      MOVE SPACES                 TO  REPORTS-EXTRACT-RECORD.      
07802                                                                   
07803      MOVE 'EX'                   TO  EX-RECORD-ID.                
07804      MOVE '2'                    TO  EX-POSITIONING-CODE.         
07805      MOVE 'D'                    TO  EX-EXTRACT-CODE.             
07806      MOVE WS-COMPANY-CODE        TO  EX-COMPANY-CD.               
07807      MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID.               
07808      MOVE 'H'                    TO  EX-RECORD-TYPE.              
07809                                                                   
07810      MOVE CL-CARRIER             TO  EX-SA-CARRIER.               
07811      MOVE CL-CLAIM-NO            TO  EX-SA-CLAIM-NO.              
07812      MOVE CL-CERT-NO             TO  EX-SA-CERT-NO.               
07813                                                                   
07814      MOVE CL-CLAIM-TYPE          TO  EX-DH-CLAIM-TYPE.            
07815      MOVE AT-SEQUENCE-NO         TO  EX-DH-TRLR-SEQ-NO.           
07816      MOVE CL-CLAIM-PREM-TYPE     TO  EX-DH-CLAIM-PREM-TYPE.       
07817      MOVE AT-PAYMENT-TYPE        TO  EX-DH-PAYMENT-TYPE.          
07818      MOVE AT-AMOUNT-PAID         TO  EX-DH-PAYMENT-AMOUNT.        
07819      MOVE AT-RECORDED-DT         TO  EX-DH-CHECK-WRITTEN-DATE.    
07820      MOVE AT-RECORDED-BY         TO  EX-DH-CHECK-BY-USER.         
07821                                                                   
07822      PERFORM 7000-RELEASE-RECORD.                                 
07823                                                                   
07824      ADD +1  TO  WS-EXTRACT-DH-COUNT.                             
07825                                                                   
07826  3959-EXIT.                                                       
07827      EXIT.                                                        
07828                                  EJECT                            
07829  3960-BUILD-EXTRACT-D-RECORD-I SECTION.                           
07830 *    NOTE ******************************************************* 
07831 *         *             EXTRACT - D   RECORD - I                * 
07832 *         *                                                     * 
07833 *         *      EXTRACTS ARE CREATED IF THE LETTER ANSWERED    * 
07834 *         *  DATE = TODAY'S ACTUAL DATE IF IT IS                * 
07835 *         *  PAST 5:00 PM. OTHERWISE THE ANSWERED DATE WILL BE  * 
07836 *         *  COMPARED TO YESTERDAY'S DATE.                      * 
07837 *         *******************************************************.
07838                                                                   
07839      MOVE SPACES                 TO  REPORTS-EXTRACT-RECORD.      
07840                                                                   
07841      MOVE 'EX'                   TO  EX-RECORD-ID.                
07842      MOVE '2'                    TO  EX-POSITIONING-CODE.         
07843      MOVE 'D'                    TO  EX-EXTRACT-CODE.             
07844      MOVE WS-COMPANY-CODE        TO  EX-COMPANY-CD.               
07845      MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID.               
07846      MOVE 'I'                    TO  EX-RECORD-TYPE.              
07847                                                                   
07848      MOVE WS-ACTION-DATE         TO  EX-SE-ACTION-DATE.           
07849      MOVE CL-CARRIER             TO  EX-SE-CARRIER.               
07850      MOVE CL-CLAIM-NO            TO  EX-SE-CLAIM-NO.              
07851      MOVE CL-CERT-NO             TO  EX-SE-CERT-NO.               
07852                                                                   
07853      MOVE CL-CLAIM-TYPE          TO  EX-DI-CLAIM-TYPE.            
07854                                                                   
07855      IF AT-TRAILER-TYPE = '4'                                     
07856          MOVE AT-CORR-SOL-UNSOL      TO  EX-DI-CORR-SOL-UNSOL     
07857          MOVE AT-LETTER-ARCHIVE-NO   TO  EX-DI-LETTER-ARCHIVE-NO  
07858          MOVE AT-STD-LETTER-FORM     TO  EX-DI-STD-LETTER-FORM    
07859          MOVE AT-LETTER-SENT-DT      TO  EX-DI-LETTER-SENT-DT     
07860          MOVE AT-AUTO-RE-SEND-DT     TO  EX-DI-AUTO-RE-SEND-DT    
07861          MOVE AT-INITIAL-PRINT-DATE  TO  EX-DI-INITIAL-PRINT-DT   
07862          MOVE AT-RESEND-PRINT-DATE   TO  EX-DI-RESEND-PRINT-DT    
07863          MOVE AT-LETTER-ANSWERED-DT  TO  EX-DI-LETTER-ANSWERED-DT 
07864          MOVE AT-RECORDED-BY         TO  EX-DI-RECORDED-BY        
07865          MOVE WS-NAME-WORK           TO  EX-DI-INSURED-LAST-NAME  
07866          MOVE AT-ADDRESSEE-NAME      TO  EX-DI-ADDRESEE-NAME      
07867          MOVE AT-ADDRESEE-TYPE       TO  EX-DI-ADDRESEE-TYPE      
07868          MOVE AT-REASON-TEXT         TO  EX-DI-REASON             
07869        ELSE                                                       
07870      IF AT-TRAILER-TYPE = 'A'                                     
07871          MOVE -1                     TO  EX-DI-LETTER-ARCHIVE-NO  
07872          MOVE AT-FORM-TYPE           TO  EX-DI-STD-LETTER-FORM    
07873          MOVE AT-FORM-SEND-ON-DT     TO  EX-DI-LETTER-SENT-DT     
07874          MOVE AT-FORM-RE-SEND-DT     TO  EX-DI-AUTO-RE-SEND-DT    
07875          MOVE AT-FORM-PRINTED-DT     TO  EX-DI-INITIAL-PRINT-DT   
07876          MOVE AT-FORM-REPRINT-DT     TO  EX-DI-RESEND-PRINT-DT    
07877          MOVE AT-FORM-ANSWERED-DT    TO  EX-DI-LETTER-ANSWERED-DT 
07878          MOVE AT-RECORDED-BY         TO  EX-DI-RECORDED-BY        
07879          MOVE WS-NAME-WORK           TO  EX-DI-INSURED-LAST-NAME  
07880          MOVE AT-FORM-ADDRESS        TO  EX-DI-ADDRESEE-TYPE.     
07881                                                                   
07882      PERFORM 7000-RELEASE-RECORD.                                 
07883                                                                   
07884      ADD +1  TO  WS-EXTRACT-DI-COUNT.                             
07885                                                                   
07886  3969-EXIT.                                                       
07887      EXIT.                                                        
07888                                  EJECT                            
07889  3970-BUILD-EXTRACT-D-RECORD-J SECTION.                           
07890 *    NOTE ******************************************************* 
07891 *         *             EXTRACT - D   RECORD - J                * 
07892 *         *                                                     * 
07893 *         *      EXTRACTS ARE CREATED FOR AIG AND AUK ONLY,     * 
07894 *         *  IF THE CLAIM IS OPEN, AND HAS NO OUTSTANDING       * 
07895 *         *  CORRESPONDENCE, AND HAS NO ACTIVITY CODES ACTIVE   * 
07896 *         *  AND IS EITHER 60, 90 OR 120 DAYS FROM LAST PAYMENT * 
07897 *         *******************************************************.
07898                                                                   
07899      MOVE SPACES                 TO  REPORTS-EXTRACT-RECORD.      
07900                                                                   
07901      MOVE 'EX'                   TO  EX-RECORD-ID.                
07902      MOVE '2'                    TO  EX-POSITIONING-CODE.         
07903      MOVE 'D'                    TO  EX-EXTRACT-CODE.             
07904      MOVE WS-COMPANY-CODE        TO  EX-COMPANY-CD.               
07905      MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID.               
07906      MOVE 'J'                    TO  EX-RECORD-TYPE.              
07907                                                                   
07908      MOVE DC-ELAPSED-DAYS        TO  EX-SH-LAG-DAYS.              
07909      MOVE CL-CLAIM-NO            TO  EX-SH-CLAIM-NO.              
07910      MOVE CL-ACTIVITY-CODE       TO  EX-DJ-ACTIVITY-CODE.         
07911      MOVE CL-CARRIER             TO  EX-DJ-CARRIER.               
07912      MOVE CL-CERT-STATE          TO  EX-DJ-STATE.                 
07913      MOVE CL-CERT-NO             TO  EX-DJ-CERT-NO.               
07914      MOVE CL-INSURED-LAST-NAME   TO  EX-DJ-INSURED-NAME.          
07915      MOVE WS-AIG-LAST-PMT-DT     TO  EX-DJ-LAST-DATE.             
07916                                                                   
07917      PERFORM 7000-RELEASE-RECORD.                                 
07918                                                                   
07919      ADD +1  TO  WS-EXTRACT-DJ-COUNT.                             
07920                                                                   
07921  3979-EXIT.                                                       
07922      EXIT.                                                        
07923                                                                   
07924      EJECT                                                        
07925  3980-BUILD-EXTRACT-D-RECORD-K SECTION.                           
07926 *    NOTE ******************************************************* 
07927 *         *             EXTRACT - D   RECORD - K                * 
07928 *         *                                                     * 
07929 *         *      EXTRACTS ARE CREATED FOR AIG AND AUK ONLY,     * 
07930 *         *  IF THE CLAIM MEETS THE AUTO ACTIVITY CRITERIA      * 
07931 *         *  A DK RECORD WILL BE GENERATED.                     * 
07932 *         *******************************************************.
07933                                                                   
07934      MOVE SPACES                 TO  REPORTS-EXTRACT-RECORD.      
07935                                                                   
07936      MOVE 'EX'                   TO  EX-RECORD-ID.                
07937      MOVE '2'                    TO  EX-POSITIONING-CODE.         
07938      MOVE 'D'                    TO  EX-EXTRACT-CODE.             
07939      MOVE WS-COMPANY-CODE        TO  EX-COMPANY-CD.               
07940      MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID.               
07941      MOVE 'K'                    TO  EX-RECORD-TYPE.              
07942                                                                   
07943      MOVE CL-PROCESSOR-ID        TO  EX-SI-PROCESSOR.             
07944      MOVE CL-ACTIVITY-CODE       TO  EX-SI-ACTIVITY-CODE.         
07945                                                                   
121902*    IF WS-COMPANY-ID = 'DMD'                                     
121902*        COMPUTE EX-SI-DAYS-NINES-COMPLEMENT =                    
121902*                 9999 - EX-SI-DAYS                               
121902*        COMPUTE EX-SI-DAYS = (DC-ELAPSED-DAYS -                  
121902*                          WS-SYS-REPORT-DAYS (AAR-SUB))          
121902*    ELSE                                                         
07952          MOVE ZEROS              TO  EX-SI-DAYS-NINES-COMPLEMENT.  
07953          MOVE DC-ELAPSED-DAYS    TO  EX-SI-DAYS.                  
07954                                                                   
07955      MOVE CL-CARRIER             TO  EX-DK-CARRIER.               
07956      MOVE CL-CERT-STATE          TO  EX-DK-STATE.                 
07957      MOVE CL-CLAIM-NO            TO  EX-DK-CLAIM-NO.              
07958      MOVE CL-CERT-NO             TO  EX-DK-CERT-NO.               
07959      MOVE CL-INSURED-LAST-NAME   TO  EX-DK-INSURED-NAME.          
07960      MOVE WS-AIG-WORK-DT         TO  EX-DK-LAST-DATE.             
07961                                                                   
07962      PERFORM 7000-RELEASE-RECORD.                                 
07963                                                                   
07964      ADD +1                      TO  WS-EXTRACT-DK-COUNT.         
07965                                                                   
07966  3989-EXIT.                                                       
07967      EXIT.                                                        
07968                                                                   
07969      EJECT                                                        
07970  3990-BUILD-EXTRACT-D-RECORD-L SECTION.                           
07971 *    NOTE ******************************************************* 
07972 *         *             EXTRACT - D   RECORD - L                * 
07973 *         *                                                     * 
07974 *         *      EXTRACTS ARE CREATED FOR AIG AND AUK ONLY,     * 
07975 *         *  IF THE CLAIM HAS A CERTIFICATE NUMBER OF           * 
07976 *         *  "INCOMPLETE"                                       * 
07977 *         *******************************************************.
07978                                                                   
07979      MOVE SPACES                 TO  REPORTS-EXTRACT-RECORD.      
07980                                                                   
07981      MOVE 'EX'                   TO  EX-RECORD-ID.                
07982      MOVE '2'                    TO  EX-POSITIONING-CODE.         
07983      MOVE 'D'                    TO  EX-EXTRACT-CODE.             
07984      MOVE WS-COMPANY-CODE        TO  EX-COMPANY-CD.               
07985      MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID.               
07986      MOVE 'L'                    TO  EX-RECORD-TYPE.              
07987                                                                   
07988      MOVE CL-INSURED-LAST-NAME   TO  EX-SJ-LAST-NAME.             
07989      MOVE CL-CARRIER             TO  EX-DL-CARRIER.               
07990      MOVE CL-CLAIM-NO            TO  EX-DL-CLAIM-NO.              
07991      MOVE CL-INSURED-1ST-NAME    TO  EX-DL-FIRST-NAME.            
07992      MOVE CL-FILE-ESTABLISH-DT   TO  EX-DL-ESTABLISH-DT.          
07993      MOVE CL-PROCESSOR-ID        TO  EX-DL-PROCESSOR-ID.          
07994      MOVE CL-LAST-MAINT-DT       TO  EX-DL-LAST-MAINT-DT.         
07995      MOVE CL-LAST-MAINT-TYPE     TO  EX-DL-LAST-MAINT-TYPE.       
07996                                                                   
07997      PERFORM 7000-RELEASE-RECORD.                                 
07998                                                                   
07999      ADD +1  TO  WS-EXTRACT-DL-COUNT.                             
08000                                                                   
08001  3999-EXIT.                                                       
08002      EXIT.                                                        
08003                                                                   
08004      EJECT                                                        
08005  4000-BUILD-EXTRACT-E-RECORD-A SECTION.                           
08006 *    NOTE ******************************************************* 
08007 *         *             EXTRACT - E   RECORD - A                * 
08008 *         *                                                     * 
08009 *         *      EXTRACTS ARE CREATED IF ANY OF THE FOLLOWING   * 
08010 *         *  DATES ARE IN THE CURRENT MONTH OR PRIOR MONTH:     * 
08011 *         *                                                     * 
08012 *         *      1.  ORIGINAL SEND DATE                         * 
08013 *         *      2.  ANSWER RECEIVED DATE                       * 
08014 *         *      3.  RESEND DATE (ONLY IF NO ANSWER RECEIVED)   * 
08015 *         *      4.  RE-SEND PRINTED DATE                       * 
08016 *         *      5.  FOLLOW-UP DATE (ONLY IF NO ANSWER RECEIVED)* 
08017 *         *     FOR 'DMD' ONLY                                  * 
08018 *         *      6.  IF THE BSR-CODE IS AN 'A' AND THE LETTER   * 
08019 *         *          HAS NOT BEEN SENT.                         * 
08020 *         *******************************************************.
08021                                                                   
08022      IF WS-ACTION-DATE = LOW-VALUES                               
08023         GO TO 4090-EXIT.                                          
08024                                                                   
08025      MOVE SPACES                 TO  REPORTS-EXTRACT-RECORD.      
08026                                                                   
08027      MOVE 'EX'                   TO  EX-RECORD-ID.                
08028      MOVE '5'                    TO  EX-POSITIONING-CODE.         
08029      MOVE 'E'                    TO  EX-EXTRACT-CODE.             
08030      MOVE WS-COMPANY-CODE        TO  EX-COMPANY-CD.               
08031      MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID.               
08032      MOVE 'A'                    TO  EX-RECORD-TYPE.              
08033                                                                   
           IF WS-LETTER-ORIGIN = 'R'
              MOVE AT-LETTER-SENT-DT   TO DC-BIN-DATE-1
              MOVE WS-ACTUAL-DATE      TO DC-BIN-DATE-2
              MOVE '1'                 TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERSION
              IF NO-CONVERSION-ERROR
                 COMPUTE EX-SE4-NO-OF-DAYS = 9999 -
                    DC-ELAPSED-DAYS
              ELSE
                 DISPLAY ' DATE ERROR ' 
                 MOVE ZEROS            TO EX-SE4-NO-OF-DAYS
              END-IF
              MOVE CL-CARRIER          TO EX-SE4-CARRIER
              MOVE CL-CLAIM-NO         TO EX-SE4-CLAIM-NO
              MOVE CL-CERT-NO          TO EX-SE4-CERT-NO
           ELSE
08034         MOVE WS-ACTION-DATE      TO EX-SE-ACTION-DATE
08035         MOVE CL-CARRIER          TO EX-SE-CARRIER
08036         MOVE CL-CLAIM-NO         TO EX-SE-CLAIM-NO
08037         MOVE CL-CERT-NO          TO EX-SE-CERT-NO
           END-IF
08038                                                                   
08039      MOVE CL-CLAIM-TYPE          TO  EX-EA-CLAIM-TYPE.            
08040                                                                   
121902*    IF WS-COMPANY-ID = 'DMD'                                     
121902*        MOVE CL-INSURED-LAST-NAME   TO  EX-EA-INSURED-LAST-NAME  
121902*    ELSE                                                         
08044          MOVE WS-NAME-WORK           TO  EX-EA-INSURED-LAST-NAME. 
08045                                                                   
08046      IF AT-TRAILER-TYPE = '4'                                     
08047          MOVE AT-LETTER-ARCHIVE-NO   TO  EX-EA-LETTER-ARCHIVE-NO  
08048          MOVE AT-STD-LETTER-FORM     TO  EX-EA-STD-LETTER-FORM    
08049          MOVE AT-LETTER-SENT-DT      TO  EX-EA-LETTER-SENT-DT     
08050          MOVE AT-AUTO-RE-SEND-DT     TO  EX-EA-AUTO-RE-SEND-DT    
08051          MOVE AT-INITIAL-PRINT-DATE  TO  EX-EA-INITIAL-PRINT-DT   
08052          MOVE AT-RESEND-PRINT-DATE   TO  EX-EA-RESEND-PRINT-DT    
08053          MOVE AT-LETTER-ANSWERED-DT  TO  EX-EA-LETTER-ANSWERED-DT 
08054          MOVE AT-RECORDED-BY         TO  EX-EA-RECORDED-BY        
08055          MOVE AT-ADDRESSEE-NAME      TO  EX-EA-ADDRESEE-NAME      
08056          MOVE AT-ADDRESEE-TYPE       TO  EX-EA-ADDRESEE-TYPE      
08057          MOVE AT-REASON-TEXT         TO  EX-EA-REASON             
121608         MOVE WS-LETTER-ORIGIN       TO EX-EA-LETTER-ORIGIN
               MOVE AT-RECEIPT-FOLLOW-UP   TO EX-EA-RECEIPT-FOLLOW-UP
               MOVE AM-CSR-CODE            TO EX-EA-CSR
08058        ELSE                                                       
08059      IF AT-TRAILER-TYPE = 'A'                                     
08060          MOVE -1                     TO  EX-EA-LETTER-ARCHIVE-NO  
08061          MOVE AT-FORM-TYPE           TO  EX-EA-STD-LETTER-FORM    
08062          MOVE AT-FORM-SEND-ON-DT     TO  EX-EA-LETTER-SENT-DT     
08063          MOVE AT-FORM-RE-SEND-DT     TO  EX-EA-AUTO-RE-SEND-DT    
08064          MOVE AT-FORM-PRINTED-DT     TO  EX-EA-INITIAL-PRINT-DT   
08065          MOVE AT-FORM-REPRINT-DT     TO  EX-EA-RESEND-PRINT-DT    
08066          MOVE AT-FORM-ANSWERED-DT    TO  EX-EA-LETTER-ANSWERED-DT 
08067          MOVE AT-RECORDED-BY         TO  EX-EA-RECORDED-BY        
08068          MOVE AT-FORM-ADDRESS        TO  EX-EA-ADDRESEE-TYPE.     
08069                                                                   
CIDMOD*    IF WS-COMPANY-ID   = 'DMD' AND                               
CIDMOD*       AT-TRAILER-TYPE = '4'                                     
CIDMOD*        PERFORM 2450-GET-BENEFICIARY  THRU  2450-EXIT            
CIDMOD*        IF WS-BENE-NOT-FOUND = 0                                 
CIDMOD*           MOVE BE-BSR-FROM          TO EX-EA-FROM               
CIDMOD*           MOVE BE-BSR-DEPT          TO EX-EA-DEPT               
CIDMOD*           MOVE BE-BSR-PHONE-NUM     TO EX-EA-PHONE-NUM          
CIDMOD*           MOVE BE-BSR-FAX-NUM       TO EX-EA-FAX-NUM            
CIDMOD*           IF AT-AUTOMATED-BSR                                   
CIDMOD*             MOVE BE-OUTPUT-TYPE     TO EX-EA-OUTPUT-TYPE        
CIDMOD*           ELSE                                                  
CIDMOD*             MOVE SPACES             TO EX-EA-OUTPUT-TYPE        
CIDMOD*           END-IF                                                
CIDMOD*           MOVE AT-SEQUENCE-NO       TO EX-EA-CORR-TRLR-SEQ      
CIDMOD*           MOVE CL-INSURED-1ST-NAME  TO EX-EA-FIRST-NAME         
CIDMOD*           MOVE CL-INSURED-MID-INIT  TO EX-EA-FIRST-INIT         
CIDMOD*           MOVE CL-INCURRED-DT       TO EX-EA-INCURRED-DT        
CIDMOD*           MOVE CL-BENEFICIARY       TO EX-EA-BENEFICIARY        
CIDMOD*           MOVE CL-CCN               TO EX-EA-CREDIT-CARD-NUMBER.
CIDMOD*                                                                 
08090      PERFORM 7000-RELEASE-RECORD.                                 
08091                                                                   
08092      ADD +1                           TO WS-EXTRACT-EA-COUNT.     
08093                                                                   
08094  4090-EXIT.                                                       
08095      EXIT.                                                        
08096                                  EJECT                            
CIDMOD 4100-BUILD-EXTRACT-F-RECORD-A SECTION.                           
CIDMOD                                                                  
020816     IF WS-COMPANY-ID = 'CID' OR 'DCC' or 'VPP'
CIDMOD         NEXT SENTENCE                                            
CIDMOD       ELSE                                                       
CIDMOD         GO TO 4190-EXIT.                                         
CIDMOD                                                                  
CIDMOD     MOVE 'EX'                   TO  REPORTS-EXTRACT-RECORD       
CIDMOD                                                                  
CIDMOD     MOVE '9'                    TO  EX-POSITIONING-CODE          
CIDMOD     MOVE 'F'                    TO  EX-EXTRACT-CODE              
CIDMOD     MOVE WS-COMPANY-CD          TO  EX-COMPANY-CD                
CIDMOD     MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID                
CIDMOD     MOVE 'A'                    TO  EX-RECORD-TYPE               
CIDMOD                                                                  
CIDMOD     MOVE CL-CARRIER             TO  EX-SG-CARRIER                
CIDMOD     MOVE CL-CERT-STATE          TO  EX-SG-STATE                  
CIDMOD     MOVE CL-CERT-ACCOUNT        TO  EX-SG-ACCOUNT-NO             
CIDMOD     MOVE CL-CLAIM-NO            TO  EX-SG-CLAIM-NO               
CIDMOD     MOVE CL-CERT-NO             TO  EX-SG-CERT-NO                
CIDMOD     MOVE ZERO                   TO  EX-SG-TRAILER-SEQ-NO         
CIDMOD                                                                  
CIDMOD     MOVE WS-NAME-WORK           TO  EX-FA-INSURED-NAME           
CIDMOD     MOVE CL-INSURED-BIRTH-DT    TO  EX-FA-INSURED-BIRTH-DT       
CIDMOD     MOVE CL-INSURED-SEX-CD      TO  EX-FA-INSURED-SEX-CD         
CIDMOD     MOVE CL-INSURED-OCC-CD      TO  EX-FA-INSURED-OCC-CD         
CIDMOD     MOVE CL-SOC-SEC-NO          TO  EX-FA-SOC-SEC-NO             
CIDMOD     MOVE CL-PROCESSOR-ID        TO  EX-FA-PROCESSOR-ID           
CIDMOD                                                                  
CIDMOD     MOVE CL-CLAIM-STATUS        TO  EX-FA-CLAIM-STATUS.          
CIDMOD     MOVE CL-CLAIM-TYPE          TO  EX-FA-CLAIM-TYPE.            
CIDMOD     MOVE CL-CLAIM-PREM-TYPE     TO  EX-FA-CLAIM-PREM-TYPE.       
CIDMOD     MOVE CL-INCURRED-DT         TO  EX-FA-INCURRED-DT.           
CIDMOD     MOVE CL-REPORTED-DT         TO  EX-FA-REPORTED-DT.           
CIDMOD     MOVE CL-FILE-ESTABLISH-DT   TO  EX-FA-FILE-ESTABLISH-DT.     
CIDMOD     MOVE CL-EST-END-OF-DISAB-DT TO  EX-FA-EST-END-OF-DISAB-DT.   
CIDMOD     MOVE CL-LAST-PMT-DT         TO  EX-FA-LAST-PMT-DT.           
CIDMOD     MOVE CL-LAST-PMT-AMT        TO  EX-FA-LAST-PMT-AMT.          
CIDMOD     MOVE CL-PAID-THRU-DT        TO  EX-FA-PAID-THRU-DT.          
CIDMOD     MOVE CL-TOTAL-PAID-AMT      TO  EX-FA-TOTAL-PAID-AMT.        
CIDMOD     MOVE CL-NO-OF-PMTS-MADE     TO  EX-FA-NO-OF-PMTS-MADE.       
CIDMOD     MOVE CL-NO-OF-DAYS-PAID     TO  EX-FA-NO-OF-DAYS-PAID.       
CIDMOD     MOVE CL-PMT-CALC-METHOD     TO  EX-FA-PMT-CALC-METHOD.       
CIDMOD     MOVE CL-CAUSE-CD            TO  EX-FA-CAUSE-CD.              
CIDMOD     MOVE CL-LAST-REOPEN-DT      TO  EX-FA-LAST-REOPEN-DT.        
CIDMOD     MOVE CL-LAST-CLOSE-DT       TO  EX-FA-LAST-CLOSE-DT.         
CIDMOD     MOVE CL-LAST-CLOSE-REASON   TO  EX-FA-LAST-CLOSE-REASON.     
CIDMOD     MOVE CL-CERT-ORIGIN         TO  EX-FA-CERT-ORIGIN.           
CIDMOD     MOVE CL-PRIORITY-CD         TO  EX-FA-PRIORITY-CD.           
CIDMOD     MOVE CL-SUPV-ATTN-CD        TO  EX-FA-SUPV-ATTN-CD.          
CIDMOD     MOVE CL-PURGED-DT           TO  EX-FA-PURGED-DT.             
CIDMOD     MOVE CL-RESTORED-DT         TO  EX-FA-RESTORED-DT.           
CIDMOD     MOVE CL-NEXT-AUTO-PAY-DT    TO  EX-FA-NEXT-AUTO-PAY-DT.      
CIDMOD     MOVE CL-NEXT-RESEND-DT      TO  EX-FA-NEXT-RESEND-DT.        
CIDMOD     MOVE CL-NEXT-FOLLOWUP-DT    TO  EX-FA-NEXT-FOLLOWUP-DT.      
CIDMOD     MOVE CL-LAST-MAINT-DT       TO  EX-FA-LAST-MAINT-DT.         
CIDMOD     MOVE CL-LAST-MAINT-USER     TO  EX-FA-LAST-MAINT-USER.       
CIDMOD     MOVE CL-LAST-MAINT-HHMMSS   TO  EX-FA-LAST-MAINT-HHMMSS.     
CIDMOD     MOVE CL-LAST-MAINT-TYPE     TO  EX-FA-LAST-MAINT-TYPE.       
CIDMOD     MOVE CL-RELATED-CLAIM-NO    TO  EX-FA-RELATED-CLAIM-NO.      
CIDMOD     MOVE CL-HISTORY-ARCHIVE-DT  TO  EX-FA-HISTORY-ARCHIVE-DT.    
CIDMOD     MOVE CL-TRAILER-SEQ-CNT     TO  EX-FA-TRAILER-SEQ-CNT.       
CIDMOD     MOVE CL-LAST-INC-DT-CHANGE  TO  EX-FA-LAST-INC-DT-CHANGE.    
CIDMOD     MOVE CL-AUTO-PAY-SEQ        TO  EX-FA-AUTO-PAY-SEQ.          
CIDMOD     MOVE CL-ACCOUNT-ADDR-CNT    TO  EX-FA-ACCOUNT-ADDR-CNT.      
CIDMOD     MOVE CL-FILE-LOCATION       TO  EX-FA-FILE-LOCATION          
CIDMOD     MOVE CL-BENEFICIARY         TO  EX-FA-BENEFICIARY            
CIDMOD                                                                  
CIDMOD     MOVE AM-NAME                TO  EX-FA-ACCOUNT-NAME           
CIDMOD                                                                  
CIDMOD     IF WS-DIAGNOSIS-DESCRIP IS EQUAL TO SPACES                   
CIDMOD         NEXT SENTENCE                                            
CIDMOD     ELSE                                                         
CIDMOD         MOVE WS-DIAGNOSIS-DESCRIP                                
CIDMOD                                 TO  EX-FA-DIAGNOSIS-DESCRIP      
CIDMOD         MOVE SPACES             TO  WS-DIAGNOSIS-DESCRIP.        
CIDMOD                                                                  
CIDMOD     PERFORM 7000-RELEASE-RECORD                                  
CIDMOD                                                                  
CIDMOD     ADD +1  TO  WS-EXTRACT-FA-COUNT.                             
CIDMOD                                                                  
CIDMOD 4190-EXIT.                                                       
CIDMOD     EXIT.                                                        
CIDMOD                                 EJECT                            
CIDMOD 4200-BUILD-EXTRACT-F-RECORD-B SECTION.                           
CIDMOD                                                                  
020816     IF WS-COMPANY-ID = 'CID' OR 'DCC' or 'VPP'
CIDMOD         NEXT SENTENCE                                            
CIDMOD       ELSE                                                       
CIDMOD         GO TO 4290-EXIT.                                         
CIDMOD                                                                  
CIDMOD     MOVE 'EX'                   TO  REPORTS-EXTRACT-RECORD       
CIDMOD                                                                  
CIDMOD     MOVE '9'                    TO  EX-POSITIONING-CODE          
CIDMOD     MOVE 'F'                    TO  EX-EXTRACT-CODE              
CIDMOD     MOVE WS-COMPANY-CD          TO  EX-COMPANY-CD                
CIDMOD     MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID                
CIDMOD     MOVE 'B'                    TO  EX-RECORD-TYPE               
CIDMOD                                                                  
CIDMOD     MOVE CL-CARRIER             TO  EX-SG-CARRIER                
CIDMOD     MOVE CL-CERT-STATE          TO  EX-SG-STATE                  
CIDMOD     MOVE CL-CERT-ACCOUNT        TO  EX-SG-ACCOUNT-NO             
CIDMOD     MOVE CL-CLAIM-NO            TO  EX-SG-CLAIM-NO               
CIDMOD     MOVE CL-CERT-NO             TO  EX-SG-CERT-NO                
CIDMOD     MOVE ZERO                   TO  EX-SG-TRAILER-SEQ-NO         
CIDMOD                                                                  
CIDMOD     MOVE WS-NAME-WORK           TO  EX-FB-INSURED-NAME           
CIDMOD     MOVE CM-CERT-EFF-DT         TO  EX-FB-EFFECTIVE-DT           
CIDMOD     MOVE CM-SOC-SEC-NO          TO  EX-FB-SSN                    
CIDMOD     MOVE CM-MEMBER-NO           TO  EX-FB-MEMBER-NO              
CIDMOD     MOVE CM-INSURED-ISSUE-AGE   TO  EX-FB-INSURED-ISSUE-AGE      
CIDMOD     MOVE CM-INSURED-JOINT-AGE   TO  EX-FB-SPOUSE-ISSUE-AGE       
CIDMOD     MOVE CM-INSURED-SEX         TO  EX-FB-INSURED-SEX            
CIDMOD                                                                  
CIDMOD     MOVE CM-LF-BENEFIT-CD       TO  EX-FB-LF-BENEFIT-CD.         
CIDMOD     MOVE CM-LF-ORIG-TERM        TO  EX-FB-LF-ORIG-TERM.          
CIDMOD     MOVE CM-LF-BENEFIT-AMT      TO  EX-FB-LF-BENEFIT-AMT.        
CIDMOD     MOVE CM-LF-PREMIUM-AMT      TO  EX-FB-LF-PREMIUM-AMT.        
CIDMOD     MOVE CM-LF-REMAINING-AMT    TO  EX-FB-LF-REMAINING-AMT.      
CIDMOD     MOVE CM-LF-ITD-CANCEL-AMT   TO  EX-FB-LF-ITD-CANCEL-AMT.     
CIDMOD     MOVE CM-LF-ITD-DEATH-AMT    TO  EX-FB-LF-ITD-DEATH-AMT.      
CIDMOD     MOVE CM-LF-DEV-CODE         TO  EX-FB-LF-DEV-CODE.           
CIDMOD                                                                  
CIDMOD     MOVE CM-AH-BENEFIT-CD       TO  EX-FB-AH-BENEFIT-CD.         
CIDMOD     MOVE CM-AH-ORIG-TERM        TO  EX-FB-AH-ORIG-TERM.          
CIDMOD     MOVE CM-AH-BENEFIT-AMT      TO  EX-FB-AH-BENEFIT-AMT.        
CIDMOD     MOVE CM-AH-PREMIUM-AMT      TO  EX-FB-AH-PREMIUM-AMT.        
CIDMOD     MOVE CM-AH-ITD-CANCEL-AMT   TO  EX-FB-AH-ITD-CANCEL-AMT.     
CIDMOD     MOVE CM-AH-ITD-LUMP-PMT     TO  EX-FB-AH-ITD-LUMP-PMT.       
CIDMOD     MOVE CM-AH-DEV-CODE         TO  EX-FB-AH-DEV-CODE.           
CIDMOD                                                                  
CIDMOD     MOVE CM-LOAN-APR            TO  EX-FB-LOAN-APR.              
CIDMOD     MOVE CM-PAY-FREQUENCY       TO  EX-FB-PAY-FREQUENCY.         
CIDMOD     MOVE CM-LOAN-TERM           TO  EX-FB-LOAN-TERM.             
CIDMOD     MOVE CM-RATE-CLASS          TO  EX-FB-RATE-CLASS.            
CIDMOD     MOVE CM-POLICY-FORM-NO      TO  EX-FB-POLICY-FORM-NO.        
CIDMOD     MOVE CM-PREMIUM-TYPE        TO  EX-FB-PREMIUM-TYPE.          
CIDMOD     MOVE CM-IND-GRP-TYPE        TO  EX-FB-IND-GRP-TYPE.          
CIDMOD     MOVE CM-SKIP-CODE           TO  EX-FB-SKIP-CODE.             
CIDMOD     MOVE CM-PAYMENT-MODE        TO  EX-FB-PAYMENT-MODE.          
CIDMOD     MOVE CM-LOAN-NUMBER         TO  EX-FB-LOAN-NUMBER.           
CIDMOD     MOVE CM-LOAN-BALANCE        TO  EX-FB-LOAN-BALANCE.          
CIDMOD     MOVE CM-REIN-TABLE          TO  EX-FB-REIN-TABLE.            
CIDMOD     MOVE CM-SPECIAL-REIN-CODE   TO  EX-FB-SPECIAL-REIN-CODE.     
CIDMOD                                                                  
CIDMOD     MOVE CM-AH-CANCEL-DT        TO  EX-FB-AH-CANCEL-DT.          
CIDMOD     MOVE CM-LF-CANCEL-DT        TO  EX-FB-LF-CANCEL-DT.          
CIDMOD     MOVE CM-AH-SETTLEMENT-DT    TO  EX-FB-AH-SETTLEMENT-DT.      
CIDMOD     MOVE CM-LF-DEATH-DT         TO  EX-FB-LF-DEATH-DT.           
CIDMOD     MOVE CM-ENTRY-DT            TO  EX-FB-ENTRY-DT.              
CIDMOD     MOVE CM-LF-CURRENT-STATUS   TO  EX-FB-LF-CURRENT-STATUS.     
CIDMOD     MOVE CM-LF-STATUS-AT-DEATH  TO  EX-FB-LF-STATUS-AT-DEATH.    
CIDMOD     MOVE CM-LF-STATUS-AT-CANCEL TO  EX-FB-LF-STATUS-AT-CANCEL.   
CIDMOD     MOVE CM-LF-DEATH-EXIT-DT    TO  EX-FB-LF-DEATH-EXIT-DT.      
CIDMOD     MOVE CM-LF-CANCEL-EXIT-DT   TO  EX-FB-LF-CANCEL-EXIT-DT.     
CIDMOD     MOVE CM-AH-CURRENT-STATUS   TO  EX-FB-AH-CURRENT-STATUS.     
CIDMOD     MOVE CM-AH-PAID-THRU-DT     TO  EX-FB-AH-PAID-THRU-DT.       
CIDMOD     MOVE CM-AH-STATUS-AT-SETTLEMENT                              
CIDMOD                                 TO  EX-FB-AH-STATUS-AT-LUMP-SUM. 
CIDMOD     MOVE CM-AH-STATUS-AT-CANCEL TO  EX-FB-AH-STATUS-AT-CANCEL.   
CIDMOD     MOVE CM-AH-CANCEL-EXIT-DT   TO  EX-FB-AH-CANCEL-EXIT-DT.     
CIDMOD     MOVE CM-AH-SETTLEMENT-EXIT-DT                                
CIDMOD                                 TO  EX-FB-AH-SETTLEMENT-EXIT-DT. 
CIDMOD     MOVE CM-CLAIM-INTERFACE-SW  TO  EX-FB-CLAIM-INTERFACE-SW.    
CIDMOD     MOVE CM-CLAIM-ATTACHED-COUNT                                 
CIDMOD                                 TO  EX-FB-CLAIM-ATTACHED-COUNT.  
CIDMOD     MOVE CM-ENTRY-BATCH         TO  EX-FB-ENTRY-BATCH.           
CIDMOD     MOVE CM-LAST-MONTH-END      TO  EX-FB-LAST-MONTH-END.        
CIDMOD                                                                  
CIDMOD     MOVE CM-CREDIT-INTERFACE-SW-1 TO  EX-FB-CREDIT-INTERFACE-SW-1
CIDMOD     MOVE CM-CREDIT-INTERFACE-SW-2 TO  EX-FB-CREDIT-INTERFACE-SW-2
CIDMOD     MOVE CM-ACCOUNT-COMM-PCTS   TO  EX-FB-ACCOUNT-COMM-PCTS      
CIDMOD                                                                  
CIDMOD     PERFORM 7000-RELEASE-RECORD                                  
CIDMOD                                                                  
CIDMOD     ADD +1  TO  WS-EXTRACT-FB-COUNT.                             
CIDMOD                                                                  
CIDMOD 4290-EXIT.                                                       
CIDMOD     EXIT.                                                        
CIDMOD                                 EJECT                            
CIDMOD 4300-BUILD-EXTRACT-F-RECORD-C SECTION.                           
CIDMOD                                                                  
020816     IF WS-COMPANY-ID = 'CID' OR 'DCC' or 'VPP'
CIDMOD         NEXT SENTENCE                                            
CIDMOD       ELSE                                                       
CIDMOD         GO TO 4390-EXIT.                                         
CIDMOD                                                                  
CIDMOD     MOVE +1                     TO  WS-F-EXTRACT-SW              
CIDMOD                                                                  
CIDMOD     MOVE 'EX'                   TO  REPORTS-EXTRACT-RECORD       
CIDMOD                                                                  
CIDMOD     MOVE '9'                    TO  EX-POSITIONING-CODE          
CIDMOD     MOVE 'F'                    TO  EX-EXTRACT-CODE              
CIDMOD     MOVE WS-COMPANY-CD          TO  EX-COMPANY-CD                
CIDMOD     MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID                
CIDMOD     MOVE 'C'                    TO  EX-RECORD-TYPE               
CIDMOD                                                                  
CIDMOD     MOVE CL-CARRIER             TO  EX-SG-CARRIER                
CIDMOD     MOVE CL-CERT-STATE          TO  EX-SG-STATE                  
CIDMOD     MOVE CL-CERT-ACCOUNT        TO  EX-SG-ACCOUNT-NO             
CIDMOD     MOVE CL-CLAIM-NO            TO  EX-SG-CLAIM-NO               
CIDMOD     MOVE CL-CERT-NO             TO  EX-SG-CERT-NO                
CIDMOD     MOVE AT-SEQUENCE-NO         TO  EX-SG-TRAILER-SEQ-NO         
CIDMOD                                                                  
CIDMOD     MOVE WS-NAME-WORK           TO  EX-FC-INSURED-NAME           
CIDMOD     MOVE AT-TRAILER-TYPE        TO  EX-FC-TRAILER-TYPE           
CIDMOD     MOVE AT-RECORDED-DT         TO  EX-FC-RECORDED-DT            
CIDMOD     MOVE AT-RECORDED-BY         TO  EX-FC-RECORDED-BY            
CIDMOD     MOVE AT-LAST-MAINT-HHMMSS   TO  EX-FC-LAST-MAINT-HHMMSS      
CIDMOD                                                                  
CIDMOD     MOVE AT-TRAILER-BODY        TO  EX-FC-TRAILER-BODY           
CIDMOD                                                                  
CIDMOD     PERFORM 7000-RELEASE-RECORD                                  
CIDMOD                                                                  
CIDMOD     ADD +1  TO  WS-EXTRACT-FC-COUNT.                             
CIDMOD                                                                  
CIDMOD 4390-EXIT.                                                       
CIDMOD     EXIT.                                                        
CIDMOD                                 EJECT                            
08097  4400-BUILD-EXTRACT-G-RECORD-A SECTION.                           
08098 *    NOTE ******************************************************* 
08099 *         *             EXTRACT - G   RECORD - A                * 
08100 *         *                                                     * 
08101 *         *      EXTRACT FOR GENERAL LEDGER INTERFACE           * 
08102 *         *******************************************************.
08103                                                                   
08104      MOVE LOW-VALUES             TO  GENERAL-LEDGER-INTERFACE.    
08105      MOVE 'GL'                   TO  GL-RECORD-ID.                
08106      MOVE '2'                    TO  GL-SOURCE-CODE.              
08107      MOVE CL-CERT-KEY-DATA       TO  GL-ORIGINAL-KEYS.            
08108      MOVE CL-CURRENT-KEY-DATA    TO  GL-CURRENT-KEYS.             
08109                                                                   
08110      MOVE WS-COMPANY-ID          TO  GL-COMPANY.                  
08111                                                                   
08112      MOVE CL-CERT-NO             TO  GL-CERTIFICATE-NUMBER.       
08113      MOVE CL-CLAIM-NO            TO  GL-CLAIM-NUMBER.             
08114                                                                   
08115      MOVE WS-CERT-EFF-DATE-CYMD  TO  GL-EFFECT-DATE-YYYYMMDD.     
08116                                                                   
08117      IF WS-DOING-RESERVES                                         
08118          MOVE 'R'                TO  GL-PAY-TYPE                  
08119      ELSE                                                         
08120          IF WS-VOID-PAYMENT-SW = +1                               
08121              IF CHARGEABLE-EXPENSE                                
08122                      OR                                           
08123                  NON-CHARGEABLE-EXPENSE                           
08124                  MOVE 'X'        TO  GL-PAY-TYPE                  
08125              ELSE                                                 
08126                  MOVE 'V'        TO  GL-PAY-TYPE                  
08127          ELSE                                                     
08128              MOVE AT-CHECK-NO    TO  WS-CHECK-NO                  
08129              IF WS-CHECK-2 = 'CR'                                 
08130                  MOVE 'C'        TO  GL-PAY-TYPE                  
08131              ELSE                                                 
08132                  IF CHARGEABLE-EXPENSE                            
08133                          OR                                       
08134                      NON-CHARGEABLE-EXPENSE                       
08135                      MOVE 'E'    TO  GL-PAY-TYPE                  
08136                  ELSE                                             
08137                      MOVE 'P'    TO  GL-PAY-TYPE.                 
08138                                                                   
08139      IF GL-PAY-TYPE = 'R'                                         
08140          MOVE ZEROS              TO  GL-PAYMENT-AMOUNT            
08141          MOVE LOW-VALUE          TO  GL-CASH-CODE                 
08142          GO TO 4400-CONTINUE.                                     
08143                                                                   
08144      IF GL-PAY-TYPE = 'X' OR 'V'                                  
08145          COMPUTE GL-PAYMENT-AMOUNT = AT-AMOUNT-PAID * -1          
08146      ELSE                                                         
08147          MOVE AT-AMOUNT-PAID     TO  GL-PAYMENT-AMOUNT.           
08148                                                                   
08149      IF AT-CASH-PAYMENT = 'Y'                                     
08150          MOVE 'Y'                TO  GL-CASH-CODE                 
08151      ELSE                                                         
08152          MOVE 'N'                TO  GL-CASH-CODE.                
08153                                                                   
08154  4400-CONTINUE.                                                   
08155                                                                   
121902*    IF CL-SYSTEM-IDENTIFIER = 'CV'                               
121902*        MOVE PM-INS-PLAN-CD         TO  GL-BENEFIT-CODE          
121902*    ELSE                                                         
121902         IF  CL-CLAIM-TYPE = WS-AH-OVERRIDE-L1 OR 'I' OR 'G'
022122                OR 'F' OR 'B' OR 'H'
08160              MOVE CM-AH-BENEFIT-CD   TO  GL-BENEFIT-CODE          
08161          ELSE                                                     
08162              MOVE CM-LF-BENEFIT-CD   TO  GL-BENEFIT-CODE.         
08163                                                                   
121902*    IF CL-SYSTEM-IDENTIFIER = 'CV'                               
121902*        MOVE PM-LOAN-NUMBER     TO  GL-LOAN-NUMBER               
121902*    ELSE                                                         
08167          MOVE CM-MEMBER-NO       TO  GL-CURRENT-LN-NUM.            
08168          MOVE CM-LOAN-NUMBER     TO  GL-ORIGINAL-LN-NUM.          
08169                                                                   
08170      MOVE AT-ASSOCIATES          TO  GL-ASSOCIATES.               
08171      MOVE CL-LOAN-TYPE           TO  GL-LOAN-TYPE.                
08172                                                                   
121902*    IF CL-SYSTEM-IDENTIFIER = 'CV'                               
121902*        MOVE CL-CLAIM-TYPE              TO  GL-PRODUCT-CODE      
121902*    ELSE                                                         
08176          IF CM-CERT-SFX = 'R'                                     
08177              MOVE 'R'                    TO  GL-PRODUCT-CODE      
08178          ELSE                                                     
08179              IF CM-CERT-SFX = 'U'                                 
08180                  MOVE 'U'                TO  GL-PRODUCT-CODE      
08181              ELSE                                                 
08182                  IF CM-CERT-SFX = 'S'                             
08183                      IF AT-AIG-UNEMP-IND = 'U'                    
08184                          MOVE 'U'        TO  GL-PRODUCT-CODE      
08185                      ELSE                                         
08186                          MOVE 'A'        TO  GL-PRODUCT-CODE      
121902                     END-IF
08187                  ELSE                                             
08188                      MOVE CL-CLAIM-TYPE  TO  GL-PRODUCT-CODE
121902                 END-IF
121902             END-IF 
121902         END-IF. 
08189                                                                   
08190      MOVE WS-PMT-VOID-DATE-CYMD  TO  GL-ENTRY-DATE-YYYYMMDD.      
08191                                                                   
08192      MOVE CL-BENEFICIARY         TO  EX-BA-BENEFICIARY            
08193                                      GL-BENEFICIARY.              
08194      MOVE CL-LOAN-TYPE           TO  EX-BA-LOAN-TYPE              
08195                                      GL-LOAN-TYPE.                
08196      MOVE WS-GL-PTC              TO  GL-PAY-CURRENT-RESERVES.     
08197      MOVE WS-GL-IBNR             TO  GL-IBNR-RESERVES.            
08198      MOVE WS-GL-FUTURE           TO  GL-FUTURE-RESERVES.          
08199                                                                   
08200  4400-EXIT.                                                       
08201      EXIT.                                                        
08202                                  EJECT                            
08203  4450-COMPLETE-EXTR-G-RECORD-A SECTION.                           
08204                                                                   
08205      MOVE SPACES                 TO  REPORTS-EXTRACT-RECORD.      
08206                                                                   
08207      MOVE 'EX'                   TO  EX-RECORD-ID.                
08208      MOVE '9'                    TO  EX-POSITIONING-CODE.         
08209      MOVE 'G'                    TO  EX-EXTRACT-CODE.             
08210      MOVE WS-COMPANY-CODE        TO  EX-COMPANY-CD.               
08211      MOVE WS-COMPANY-ID          TO  EX-COMPANY-ID.               
08212      MOVE 'A'                    TO  EX-RECORD-TYPE.              
08213      MOVE GENERAL-LEDGER-INTERFACE                                
08214                                  TO  EX-GA-GENERAL-LEDGER-DATA.   
08215                                                                   
08216      PERFORM 7000-RELEASE-RECORD.                                 
08217                                                                   
08218      ADD +1                      TO  WS-EXTRACT-GA-COUNT.         
08219                                                                   
08220  4450-EXIT.                                                       
08221      EXIT.                                                        
061406
061406 4950-FIND-TRAILER-LAST-MAINT-DT  SECTION.
061406
061406     MOVE SPACES                 TO  AT-CONTROL-PRIMARY.          
061406     MOVE LOW-VALUES             TO  WS-LAST-MAINT-DT.
061406                                                                  
061406     MOVE CL-COMPANY-CD          TO  AT-COMPANY-CD.               
061406     MOVE CL-CARRIER             TO  AT-CARRIER.                  
061406     MOVE CL-CLAIM-NO            TO  AT-CLAIM-NO.                 
061406     MOVE CL-CERT-NO             TO  AT-CERT-NO.                  
061406     MOVE ZERO                   TO  AT-SEQUENCE-NO.              
061406                                                                  
061406     START ELTRLR                                                 
061406         KEY EQUAL AT-CONTROL-PRIMARY.                            
061406                                                                   
061406     IF ELTRLR-FILE-STATUS NOT EQUAL ZERO  
061406         MOVE CL-LAST-MAINT-DT   TO  WS-LAST-MAINT-DT
061406         MOVE CL-LAST-MAINT-USER TO  WS-LAST-MAINT-USER
061406         MOVE CL-LAST-MAINT-TYPE TO  WS-LAST-MAINT-TYPE
061406         GO TO 4959-EXIT
061406     END-IF.

050619     move spaces                 to ws-corres-sw
050619                                    ws-auto-pay-sw
050619                                    ws-letter-sent
050619                                    ws-323g-maint-user
050619                                    ws-323g-letter
050619                                    ws-323g-sw
071019                                    ws-stuff-ind
050619
071019** The below ws-las-maint-user and type will only be set if
071019**  the cl-last-maint stuff was older than 45 days ago.
050619     if (ws-last-maint-user = 'SYST')
050619        and (ws-last-maint-type = '2') *> Correspondence
050619*       display ' flag looking for letter ' at-claim-no
050619        set looking-for-orig-letter to true
050619     end-if
050619
050619     if (ws-last-maint-user = 'AUTO')
050619        and (ws-last-maint-type = '1') *> Payment
050619*       display ' flag looking for payment ' at-claim-no
050619        set looking-for-auto-pay-setup to true
050619     end-if

050619     MOVE SPACES                 TO ws-auto-pay-setup-by.
061406
061406 4951-TRLR-LOOP.
061406
061406     READ ELTRLR NEXT.             
061406                                                                  
061406     IF ELTRLR-FILE-STATUS NOT = ZERO  
061406         GO TO 4959-EXIT
061406     END-IF                                         
061406                                                                  
061406     IF CL-COMPANY-CD NOT = AT-COMPANY-CD                         
061406       OR CL-CARRIER  NOT = AT-CARRIER                            
061406       OR CL-CLAIM-NO NOT = AT-CLAIM-NO                           
061406       OR CL-CERT-NO  NOT = AT-CERT-NO                            
061406         GO TO 4959-EXIT                                          
061406     END-IF.
061406
061406     IF RESERVE-EXPENSE-TR OR AUTO-PROMPT-TR
061406         GO TO 4951-TRLR-LOOP
061406     END-IF.
061406
061406     IF GENERAL-INFO-TR
061406         IF AT-INFO-LINE-1 (1:4) = 'UNWR'  OR
061406            AT-INFO-LINE-1 (1:4) = 'UNWS'
061406             NEXT SENTENCE
061406         ELSE
061406             GO TO 4951-TRLR-LOOP
061406         END-IF
061406     END-IF.

071019     if ((looking-for-orig-letter)
071019           or
071019         (trlr-stuff-needed))
050619        and (correspondence-tr)
050619        and (at-recorded-by = 'SYST')
050619        and (ws-letter-sent = spaces)
050619        move at-std-letter-form  to ws-letter-sent
050619*       display ' established letter ' at-claim-no ' '
050619*          ws-letter-sent
050619     end-if
050619
071019     if ((looking-for-orig-letter)
071019           or
071019         (trlr-stuff-needed))
050619        and (correspondence-tr)
050619        and (at-recorded-by = 'SYST')
050619        and (ws-letter-sent = at-resend-letter-form)
050619        move at-std-letter-form to ws-letter-sent
050619*       display ' re establish letter ' at-claim-no ' '
050619*          ws-letter-sent
050619     end-if
050619
071019     if ((looking-for-orig-letter)
071019           or
071019         (trlr-stuff-needed))
050619        and (correspondence-tr)
050619        and (at-recorded-by <> 'SYST')
050619        and (ws-letter-sent = at-resend-letter-form)
050619        move at-std-letter-form to ws-letter-sent
050619        move at-recorded-by to ws-last-maint-user
071019        move spaces              to ws-corres-sw
071019                                    ws-stuff-ind
050619*       display ' re re establish letter ' at-claim-no ' '
050619*          ws-letter-sent
050619     end-if
050619
050619     if (looking-for-323g-letter)
050619        and (correspondence-tr)
050619        and (at-recorded-by = 'SYST')
050619        and (ws-323g-letter = at-resend-letter-form)
050619        move at-std-letter-form to ws-323g-letter
050619*       display ' re establish 323g letter ' at-claim-no ' '
050619*          ws-323g-letter
050619     end-if
050619
050619     if (looking-for-323g-letter)
050619        and (correspondence-tr)
050619        and (at-recorded-by <> 'SYST')
050619        and (ws-323g-letter = at-resend-letter-form)
050619        move at-std-letter-form to ws-323g-letter
050619        move at-recorded-by to ws-323g-maint-user
071019        move spaces              to ws-323g-sw
050619*       display ' re re establish letter ' at-claim-no ' '
050619*          ws-323g-letter
050619     end-if
050619
050619     if (looking-for-auto-pay-setup)
050619        and (auto-pay-tr)
050619        and (ws-auto-pay-setup-by = spaces)
050619*       display ' found auto pay ' at-claim-no ' ' at-recorded-by
050619        move at-recorded-by      to ws-auto-pay-setup-by
071019        move ' '                 to ws-stuff-ind
050619     end-if
061406            
061406     MOVE AT-TRAILER-BODY TO WS-TRAILER-BODY.
050619     IF WS-REG-TRAILER-USER-ID = 'E310'  or 'E354' *> lst maint by
061406         IF AT-RECORDED-DT > WS-LAST-MAINT-DT
061406             MOVE AT-RECORDED-DT TO WS-LAST-MAINT-DT
061406             MOVE AT-RECORDED-BY TO WS-LAST-MAINT-USER
061406             IF PAYMENT-TR OR AUTO-PAY-TR
061406                 MOVE '1' TO WS-LAST-MAINT-TYPE
050619                 set looking-for-auto-pay-setup to true
061406             ELSE 
061406                 IF CORRESPONDENCE-TR
061406                     MOVE '2' TO WS-LAST-MAINT-TYPE
061406                 ELSE 
061406                     IF INCURRED-CHG-TR
061406                         MOVE '5' TO WS-LAST-MAINT-TYPE
061406                     ELSE
061406                         MOVE '3' TO WS-LAST-MAINT-TYPE
061406                     END-IF
061406                 END-IF
061406             END-IF
061406         END-IF
061406     ELSE
061406         IF WS-REG-TRAILER-MAINT-DT > WS-LAST-MAINT-DT
061406             MOVE WS-REG-TRAILER-MAINT-DT TO WS-LAST-MAINT-DT
050619             MOVE WS-REG-TRAILER-USER-ID TO        *> lst maint by
061406                                  WS-LAST-MAINT-USER
061406             IF PAYMENT-TR OR AUTO-PAY-TR
061406                 MOVE '1' TO WS-LAST-MAINT-TYPE
061406             ELSE 
061406                 IF CORRESPONDENCE-TR
061406                     MOVE '2' TO WS-LAST-MAINT-TYPE
071019                     if ws-reg-trailer-user-id = 'SYST'
071019                        set looking-for-orig-letter to true
071019                     end-if
061406                 ELSE 
061406                     IF INCURRED-CHG-TR
061406                         MOVE '5' TO WS-LAST-MAINT-TYPE
061406                     ELSE
061406                         MOVE '3' TO WS-LAST-MAINT-TYPE
061406                     END-IF
061406                 END-IF
061406             END-IF
061406         END-IF
061406     END-IF.

050619     if (correspondence-tr)
050619        and (at-recorded-by = 'SYST')
050619        and (AT-RECEIPT-FOLLOW-UP <= BIN-RUN-DATE)
050619        AND (AT-RECEIPT-FOLLOW-UP > WS-PREV-CYCLE-BIN)
050619        move at-std-letter-form  to ws-323g-letter
050619        set looking-for-323g-letter to true
050619     end-if

061406     GO TO 4951-TRLR-LOOP.                              
061406
061406 4959-EXIT.
061406     EXIT.                                      
08222                                  EJECT                            
08223  5000-MOVE-NAME SECTION. COPY ELCMNS.                             
08224                                  EJECT                            
08225                                                                   
08226  7000-RELEASE-RECORD SECTION.                                     
08227 *    NOTE ******************************************************* 
08228 *         *      THIS SECTION RELEASES THE REPORTS EXTRACT      * 
08229 *         *  RECORDS TO THE SORT.                               * 
08230 *         *******************************************************.
08231                                                                   
08231                                                                   
08232      RELEASE REPORTS-EXTRACT-RECORD.                              
08233                                                                   
08234      IF SORT-RETURN NOT = ZERO                                    
08235          MOVE 'SORT FAILED - RELEASE'  TO  WS-ABEND-MESSAGE       
08236          MOVE SORT-RETURN        TO  WS-RETURN-CODE               
08237          GO TO ABEND-PGM.                                         
08238                                                                   
08239      ADD +1  TO  WS-RECORDS-RELEASED.                             
08240                                                                   
08241  7090-EXIT.                                                       
08242      EXIT.                                                        
08243                                  EJECT                            
08244 ********** THIS LOGIC CAN BE REMOVED                              
08245 *7100-PURGE-LETTER SECTION.                                       
08246                                                                   
08247 *    MOVE LOW-VALUES             TO  LA-CONTROL-PRIMARY.          
08248 *    MOVE WS-COMPANY-CODE        TO  LA-COMPANY-CD.               
08249 *    MOVE AT-LETTER-ARCHIVE-NO   TO  LA-ARCHIVE-NO.               
08250 *                                                                 
08251 *    START ELARCH                                                 
08252 *        KEY IS NOT LESS THAN LA-CONTROL-PRIMARY                  
08253 *                                                                 
08254 *    IF ELARCH-FILE-STATUS = '23'                                 
08255 *        GO TO 7190-EXIT.                                         
08256 *                                                                 
08257 *    IF ELARCH-FILE-STATUS NOT = ZERO                             
08258 *        MOVE 'ERROR OCCURRED START - ELARCH'                     
08259 *                                TO  WS-ABEND-MESSAGE             
08260 *        MOVE ELARCH-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
08261 *        GO TO ABEND-PGM.                                         
08262 *                                                                 
08263 *7110-PURGE-LETTER.                                               
08264 *                                                                 
08265 *    READ ELARCH NEXT INTO WS-SAVE-LETTER-ARCHIVE.                
08266 *                                                                 
08267 *    IF ELARCH-FILE-STATUS = '10'                                 
08268 *        GO TO 7190-EXIT.                                         
08269 *                                                                 
08270 *    IF ELARCH-FILE-STATUS NOT = ZERO                             
08271 *        MOVE 'ERROR OCCURRED READNEXT - ELARCH'                  
08272 *                                TO  WS-ABEND-MESSAGE             
08273 *        MOVE ELARCH-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
08274 *        GO TO ABEND-PGM.                                         
08275 *                                                                 
08276 *    IF LA-COMPANY-CD NOT = WS-COMPANY-CODE                       
08277 *      OR AT-LETTER-ARCHIVE-NO NOT = LA-ARCHIVE-NO                
08278 *        GO TO 7190-EXIT.                                         
08279 *                                                                 
08280 *    PERFORM 7800-DELETE-ARCHIVE-RECORD.                          
08281                                                                   
08282 *    GO TO 7110-PURGE-LETTER.                                     
08283                                                                   
08284 *7190-EXIT.                                                       
08285 *    EXIT.                                                        
08286                                  EJECT                            
08287  7200-PURGE-CHECK-QUEUE SECTION.                                  
08288                                                                   
08289      MOVE +7                     TO SLR-KEY-LENGTH.               
08290      MOVE +100                   TO SLR-RECORD-LENGTH.            
08291      MOVE CQ-CONTROL-PRIMARY     TO SLR-KEY.                      
08292      MOVE CHECK-QUE              TO SLR-RECORD-IMAGE.             
08293      MOVE 'ELCHKQ'               TO SLR-DSID.                     
08294      MOVE 'D'                    TO SLR-ACTION.                   
08295                                                                   
08296      DELETE ELCHKQ RECORD.                                        
08297                                                                   
08298      IF ELCHKQ-FILE-STATUS = '23'                                 
08299          GO TO 7290-EXIT.                                         
08300                                                                   
08301      IF ELCHKQ-FILE-STATUS NOT = ZERO                             
08302          MOVE 'ERROR OCCURRED DELETE - ELCHKQ'                    
08303                                  TO  WS-ABEND-MESSAGE             
08304          MOVE ELCHKQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
08305          GO TO ABEND-PGM.                                         
08306                                                                   
08307 *    PERFORM LOG-JOURNAL-RECORD.                                  
08308                                                                   
08309      ADD +1  TO  WS-CHECK-QUEUE-DELETED.                          
08310                                                                   
08311  7290-EXIT.                                                       
08312      EXIT.                                                        
08313                                  EJECT                            
08314  7300-READ-CONTROL-FILE SECTION.                                  
08315                                                                   
08316      READ ELCNTL INTO WS-SAVE-CONTROL-FILE.                       
08317                                                                   
08318      IF ELCNTL-FILE-STATUS = '23'                                 
08319          GO TO 7390-EXIT.                                         
08320                                                                   
08321      IF ELCNTL-FILE-STATUS NOT = ZERO                             
08322          MOVE 'ERROR OCCURRED READ - ELCNTL'                      
08323                                  TO  WS-ABEND-MESSAGE             
08324          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
08325          GO TO ABEND-PGM.                                         
08326                                                                   
08327  7390-EXIT.                                                       
08328      EXIT.                                                        
08329                                  EJECT                            
08330 **** THIS LOGIC CAN BE REMOVED DUE TO RETRIEVE PROJECT            
08331  7800-DELETE-ARCHIVE-RECORD SECTION.                              
08332                                                                   
08333 *    MOVE +8                     TO SLR-KEY-LENGTH.               
08334 *    MOVE +90                    TO SLR-RECORD-LENGTH.            
08335 *    MOVE LA-CONTROL-PRIMARY     TO SLR-KEY.                      
08336 *    MOVE LETTER-ARCHIVE         TO SLR-RECORD-IMAGE.             
08337 *    MOVE 'ELARCH'               TO SLR-DSID.                     
08338 *    MOVE 'D'                    TO SLR-ACTION.                   
08339                                                                   
08340 *    DELETE ELARCH RECORD                                         
08341 *                                                                 
08342 *    IF ELARCH-FILE-STATUS NOT = ZERO                             
08343 *        MOVE 'ERROR OCCURRED DELETE - ELARCH'                    
08344 *                                TO  WS-ABEND-MESSAGE             
08345 *        MOVE ELARCH-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
08346 *        GO TO ABEND-PGM.                                         
08347                                                                   
08348 *    PERFORM LOG-JOURNAL-RECORD.                                  
08349                                                                   
08350 *    ADD +1  TO  WS-LETTER-ARCHIVES-DELETED.                      
08351                                                                   
08352  7800-EXIT.                                                       
08353      EXIT.                                                        
08354                                  EJECT                            
08355  7900-REWRITE-CONTROL-FILE SECTION.                               
08356                                                                   
08357      MOVE +10                    TO SLR-KEY-LENGTH.               
08358      MOVE +750                   TO SLR-RECORD-LENGTH.            
08359      MOVE CF-CONTROL-PRIMARY     TO SLR-KEY.                      
08360      MOVE CONTROL-FILE           TO SLR-RECORD-IMAGE.             
08361      MOVE 'ELCNTL  '             TO SLR-DSID.                     
08362      MOVE 'C'                    TO SLR-ACTION.                   
08363                                                                   
08364      REWRITE CONTROL-FILE.                                        
08365                                                                   
08366      IF ELCNTL-FILE-STATUS NOT = ZERO                             
08367          MOVE 'ERROR OCCURRED REWRITE - ELCNTL'                   
08368                                  TO  WS-ABEND-MESSAGE             
08369          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
08370          GO TO ABEND-PGM.                                         
08371                                                                   
08372 *    PERFORM LOG-JOURNAL-RECORD.                                  
08373                                                                   
08374  7900-EXIT.                                                       
08375      EXIT.                                                        
08376  EJECT                                                            
08377 *** THIS LOGIC CAN BE DELETE DUE TO THE RETRIEVE PROJECT          
08378  8000-DELETE-TRAILER SECTION.                                     
08379 *    NOTE ******************************************************* 
08380 *         *      THIS SECTION DOES THE DELETES TO THE ACTIVITY  * 
08381 *         *    TRAILER FILE                                     * 
08382 *         *******************************************************.
08383                                                                   
08384 *    MOVE +22                    TO SLR-KEY-LENGTH.               
08385 *    MOVE +200                   TO SLR-RECORD-LENGTH.            
08386 *    MOVE AT-CONTROL-PRIMARY     TO SLR-KEY.                      
08387 *    MOVE ACTIVITY-TRAILERS      TO SLR-RECORD-IMAGE.             
08388 *    MOVE 'ELTRLR  '             TO SLR-DSID.                     
08389 *    MOVE 'D'                    TO SLR-ACTION.                   
08390 *                                                                 
08391 *    DELETE ELTRLR RECORD.                                        
08392                                                                   
08393 *    IF ELTRLR-FILE-STATUS NOT = ZERO                             
08394 *        MOVE 'ERROR OCCURRED DELETE - ELTRLR'                    
08395 *                                TO  WS-ABEND-MESSAGE             
08396 *        MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
08397 *        GO TO ABEND-PGM.                                         
08398                                                                   
08399 *    PERFORM LOG-JOURNAL-RECORD.                                  
08400                                                                   
08401 *    ADD +1                      TO  WS-TRAILERS-DELETED.         
08402                                                                   
08403  8000-EXIT.                                                       
08404      EXIT.                                                        
08405                                                                   
08406                                  EJECT                            
08407  8100-REWRITE-TRAILER SECTION.                                    
08408 *    NOTE ******************************************************* 
08409 *         *      THIS SECTION DOES THE REWRITES TO THE ACTIVITY * 
08410 *         *    TRAILER FILE                                     * 
08411 *         *******************************************************.
08412                                                                   
08413      IF AT-TRAILER-TYPE = '1'                                     
08414          MOVE 'E310'             TO  AT-RESERVES-LAST-UPDATED-BY  
08415          MOVE BIN-RUN-DATE       TO  AT-RESERVES-LAST-MAINT-DT    
08416        ELSE                                                       
08417      IF AT-TRAILER-TYPE = '2'                                     
08418          MOVE 'E310'             TO  AT-PAYMENT-LAST-UPDATED-BY   
08419          MOVE BIN-RUN-DATE       TO  AT-PAYMENT-LAST-MAINT-DT     
08420        ELSE                                                       
08421      IF AT-TRAILER-TYPE = '3'                                     
08422          MOVE 'E310'             TO  AT-AUTO-PAY-LAST-UPDATED-BY  
08423          MOVE BIN-RUN-DATE       TO  AT-AUTO-PAY-LAST-MAINT-DT    
08424        ELSE                                                       
08425      IF AT-TRAILER-TYPE = '4'                                     
08426          MOVE 'E310'             TO  AT-CORR-LAST-UPDATED-BY      
08427          MOVE BIN-RUN-DATE       TO  AT-CORR-LAST-MAINT-DT        
08428        ELSE                                                       
08429      IF AT-TRAILER-TYPE = '5'                                     
08430          MOVE 'E310'             TO  AT-ADDRESS-LAST-UPDATED-BY   
08431          MOVE BIN-RUN-DATE       TO  AT-ADDRESS-LAST-MAINT-DT     
08432        ELSE                                                       
08433      IF AT-TRAILER-TYPE = '6'                                     
08434          MOVE 'E310'             TO  AT-GEN-INFO-LAST-UPDATED-BY  
08435          MOVE BIN-RUN-DATE       TO  AT-GEN-INFO-LAST-MAINT-DT    
08436        ELSE                                                       
08437      IF AT-TRAILER-TYPE = '7'                                     
08438          MOVE 'E310'             TO  AT-PROMPT-LAST-UPDATED-BY    
08439          MOVE BIN-RUN-DATE       TO  AT-PROMPT-LAST-MAINT-DT      
08440        ELSE                                                       
08441      IF AT-TRAILER-TYPE = '8'                                     
08442          MOVE 'E310'             TO  AT-DENIAL-LAST-UPDATED-BY    
08443          MOVE BIN-RUN-DATE       TO  AT-DENIAL-LAST-MAINT-DT      
08444        ELSE                                                       
08445      IF AT-TRAILER-TYPE = '9'                                     
08446          MOVE 'E310'             TO  AT-INCURRED-LAST-UPDATED-BY  
08447        ELSE                                                       
08448      IF AT-TRAILER-TYPE = 'A'                                     
08449          MOVE 'E310'             TO  AT-FORM-LAST-UPDATED-BY      
08450          MOVE BIN-RUN-DATE       TO  AT-FORM-LAST-MAINT-DT.       
08451                                                                   
08452      MOVE WS-CURRENT-TIME        TO  AT-LAST-MAINT-HHMMSS.        
08453                                                                   
08454      MOVE +22                    TO SLR-KEY-LENGTH.               
08455      MOVE +200                   TO SLR-RECORD-LENGTH.            
08456      MOVE AT-CONTROL-PRIMARY     TO SLR-KEY.                      
08457      MOVE ACTIVITY-TRAILERS      TO SLR-RECORD-IMAGE.             
08458                                                                   
08459      MOVE 'ELTRLR'               TO SLR-DSID.                     
08460      MOVE 'C'                    TO SLR-ACTION.                   
08461                                                                   
08462      REWRITE ACTIVITY-TRAILERS                                    
08463                                                                   
08464      IF ELTRLR-FILE-STATUS NOT = ZERO                             
08465          MOVE 'ERROR OCCURRED REWRITE - ELTRLR'                   
08466                                  TO  WS-ABEND-MESSAGE             
08467          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
08468          GO TO ABEND-PGM.                                         
08469                                                                   
08470 *    PERFORM LOG-JOURNAL-RECORD.                                  
08471                                                                   
08472  8100-EXIT.                                                       
08473      EXIT.                                                        
08474                                  EJECT                            
08475  8200-REWRITE-CLAIM SECTION.                                      
08476 *    NOTE ******************************************************* 
08477 *         *    THIS SECTION DOES THE REWRITES TO THE CLAIM      * 
08478 *         *    MASTER FILE                                      * 
08479 *         *******************************************************.
08480                                                                   
08481      MOVE +20                    TO SLR-KEY-LENGTH.               
08482      MOVE +350                   TO SLR-RECORD-LENGTH.            
08483      MOVE CL-CONTROL-PRIMARY     TO SLR-KEY.                      
08484      MOVE CLAIM-MASTER           TO SLR-RECORD-IMAGE.             
08485                                                                   
08486      MOVE 'ELMSTR'               TO SLR-DSID.                     
08487      MOVE 'C'                    TO SLR-ACTION.
08488                                                                   
08489      REWRITE CLAIM-MASTER.                                        
08490                                                                   
08491      IF ELMSTR-FILE-STATUS NOT = ZERO                             
08492          MOVE 'ERROR OCCURRED REWRITE - ELMSTR'                   
08493                                  TO  WS-ABEND-MESSAGE             
08494          MOVE ELMSTR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
08495          GO TO ABEND-PGM.                                         
08496                                                                   
08497 *    PERFORM LOG-JOURNAL-RECORD.                                  
08498                                                                   
08499  8200-EXIT.                                                       
08500      EXIT.                                                        
08501                                  EJECT                            
08502 **** THIS LOGIC CAN BE DELETED DUE TO THE RETRIEVE PROJECT        
08503  8205-DELETE-CLAIM SECTION.                                       
08504 *    NOTE ******************************************************* 
08505 *         *    THIS SECTION DOES THE DELETES  TO THE CLAIM      * 
08506 *         *    MASTER FILE                                      * 
08507 *         *******************************************************.
08508                                                                   
08509 *    MOVE +20                    TO SLR-KEY-LENGTH.               
08510 *    MOVE +350                   TO SLR-RECORD-LENGTH.            
08511 *    MOVE CL-CONTROL-PRIMARY     TO SLR-KEY.                      
08512 *    MOVE CLAIM-MASTER           TO SLR-RECORD-IMAGE.             
08513                                                                   
08514 *    MOVE 'ELMSTR'               TO SLR-DSID.                     
08515 *    MOVE 'D'                    TO SLR-ACTION.                   
08516                                                                   
08517 *    DELETE ELMSTR RECORD.                                        
08518                                                                   
08519 *    IF ELMSTR-FILE-STATUS NOT = ZERO                             
08520 *        MOVE 'ERROR OCCURRED DELETE  - ELMSTR'                   
08521 *                                TO  WS-ABEND-MESSAGE             
08522 *        MOVE ELMSTR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
08523 *        GO TO ABEND-PGM.                                         
08524                                                                   
08525 *    PERFORM LOG-JOURNAL-RECORD.                                  
08526                                                                   
08527 *    MOVE 'CLAIM WILL BE DELETED'    TO  WS-D1-MESSAGE.           
08528 *    MOVE WS-DETAIL1                 TO  PRT.                     
08529 *    PERFORM WRITE-A-LINE.                                        
08530 *    MOVE SPACES                     TO  WS-DETAIL1.              
08531                                                                   
08532  8205-EXIT.                                                       
08533      EXIT.                                                        
08534                                  EJECT                            
08535  8300-READ-TRAILER SECTION.                                       
08536 *    NOTE ******************************************************* 
08537 *         *    THIS SECTION DOES THE READS TO THE ACTIVITY      * 
08538 *         *    TRAILER FILE.                                    * 
08539 *         *******************************************************.
08540                                                                   
08541      MOVE ZERO                   TO  WS-TRAILER-NOT-FOUND.        
08542                                                                   
08543      READ ELTRLR INTO WS-SAVE-ACTIVITY-TRAILERS                   
08544                                                                   
08545      IF ELTRLR-FILE-STATUS = '23'                                 
08546          MOVE +1                 TO  WS-TRAILER-NOT-FOUND         
08547          GO TO 8300-EXIT.                                         
08548                                                                   
08549      IF ELTRLR-FILE-STATUS NOT = ZERO                             
08550          MOVE 'ERROR OCCURRED READ - ELTRLR'                      
08551                                  TO  WS-ABEND-MESSAGE             
08552          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
08553          GO TO ABEND-PGM.                                         
08554                                                                   
08555  8300-EXIT.                                                       
08556      EXIT.                                                        
08557                                  EJECT                            
08558  8350-CALL-EMPLCYX   SECTION.                                     
08559 *    NOTE ******************************************************* 
08560 *         *    THIS SECTION DOES THE I-O TO THE POLICY MASTER   * 
08561 *         *    FILE.                                            * 
08562 *         *******************************************************.
08563                                                                   
08564      MOVE ZEROS                  TO  WS-EMPLCY-RETURN-CODE.       
08565                                                                   
08570                                                                   
08571  8350-EXIT.                                                       
08572      EXIT.                                                        
08573                                  EJECT                            
08574  8400-REWRITE-CERT SECTION.                                       
08575 *    NOTE ******************************************************* 
08576 *         *  THIS SECTION DOES THE REWRITES TO THE CERTIFICATE  * 
08577 *         *  MASTER FILE                                        * 
08578 *         *******************************************************.
08579                                                                   
08580      MOVE +33                    TO SLR-KEY-LENGTH.               
08581      MOVE +450                   TO SLR-RECORD-LENGTH.            
08582      MOVE CM-CONTROL-PRIMARY     TO SLR-KEY.                      
08583      MOVE CERTIFICATE-MASTER     TO SLR-RECORD-IMAGE.             
08584                                                                   
08585      MOVE 'ELCERT'               TO SLR-DSID.                     
08586      MOVE 'C'                    TO SLR-ACTION.                   
08587                                                                   
08588      REWRITE CERTIFICATE-MASTER.                                  
08589                                                                   
08590      IF ELCERT-FILE-STATUS NOT = ZERO                             
08591          MOVE 'ERROR OCCURRED REWRITE - ELCERT'                   
08592                                  TO  WS-ABEND-MESSAGE             
08593          MOVE ELCERT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
08594          GO TO ABEND-PGM.                                         
08595                                                                   
08596 *    PERFORM LOG-JOURNAL-RECORD.                                  
08597                                                                   
08598  8400-EXIT.                                                       
08599      EXIT.                                                        
08600                                  EJECT                            
08601  8450-REWRITE-POLICY   SECTION.                                   
08602                                                                   
08603      MOVE +42                    TO  SLR-KEY-LENGTH.              
08604      MOVE +1200                  TO  SLR-RECORD-LENGTH.           
08605      MOVE PM-CONTROL-PRIMARY     TO  SLR-KEY.                     
08606      MOVE POLICY-MASTER          TO  SLR-RECORD-IMAGE.            
08607      MOVE 'MPPLCY'               TO  SLR-DSID.                    
08608      MOVE 'C'                    TO  SLR-ACTION.                  
08609                                                                   
08610      MOVE LOW-VALUES             TO  WS-MPPLCY-AREA.              
08611      MOVE PM-COMPANY-CD          TO  WS-COMPANY-CD.               
08612      MOVE PM-CARRIER             TO  WS-CARRIER.                  
08613      MOVE PM-GROUPING            TO  WS-GROUPING.                 
08614      MOVE PM-STATE               TO  WS-STATE.                    
08615      MOVE PM-PRODUCER            TO  WS-PRODUCER.                 
08616      MOVE PM-POLICY-EFF-DT       TO  WS-POLICY-EFF-DT.            
08617      MOVE PM-REFERENCE-NUMBER    TO  WS-REFERENCE-NUMBER.         
08618                                                                   
08619      MOVE 'RW'                   TO  WS-EMPLCY-FUNCTION.          
08620      MOVE 'E310'                 TO  WS-LAST-CHANGE-PROCESSOR.    
08621      MOVE BIN-RUN-DATE           TO  WS-LAST-CHANGE-DT.           
08622      MOVE WS-CURRENT-TIME        TO  WS-LAST-CHANGE-TIME.         
08623                                                                   
08624      COMPUTE WS-CLAIM-ATTACH-CNT = PM-CLAIM-ATTACH-CNT - 1        
08625      IF WS-CLAIM-ATTACH-CNT IS NOT GREATER THAN +0                
08626          MOVE +0                 TO  WS-CLAIM-ATTACH-CNT          
08627          MOVE ' '                TO  WS-CLAIM-INTERFACE-SW.       
08628                                                                   
08629      PERFORM 8350-CALL-EMPLCYX THRU 8350-EXIT.                    
08630                                                                   
08631      IF WS-EMPLCY-RETURN-CODE NOT = '00'                          
08632          MOVE 'UNSUCCESSFUL UPDATE OF POLICY MASTER'              
08633                                  TO  WS-ABEND-MESSAGE             
08634          MOVE WS-EMPLCY-RETURN-CODE  TO  WS-ABEND-FILE-STATUS     
08635          GO TO ABEND-PGM.                                         
08636                                                                   
08637 *    PERFORM LOG-JOURNAL-RECORD.                                  
08638                                                                   
08639  8450-EXIT.                                                       
08640      EXIT.                                                        
08641                                  EJECT                            
08642  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       
08643                                  EJECT                            
08644  8600-DEFAULT-CARRIER SECTION.                                    
08645 *    NOTE ******************************************************* 
08646 *         *    THIS SECTION SETS THE CARRIER DEFAULT VALUES IF  * 
08647 *         *    THE CARRIER RECORD WAS NOT FOUND                 * 
08648 *         *******************************************************.
08649                                                                   
08650      MOVE SPACES                 TO  CF-CARRIER-MASTER-REC.       
08651                                                                   
08652      MOVE ZERO                   TO  CF-ZIP-CODE                  
08653                                      CF-PHONE-NO.                 
08654                                                                   
08655      MOVE '1'                    TO  CF-CLAIM-NO-METHOD.          
08656      MOVE ZERO                   TO  CF-CLAIM-COUNTER.            
08657                                                                   
08658      MOVE '1'                    TO  CF-CHECK-NO-CONTROL.         
08659      MOVE ZERO                   TO  CF-CHECK-COUNTER.            
08660                                                                   
08661      MOVE '1'                    TO  CF-EXPENSE-METHOD.           
08662      MOVE ZERO                   TO  CF-EXPENSE-PERCENT           
08663                                      CF-EXPENSE-DOLLAR.           
08664                                                                   
08665      MOVE '1'                    TO  CF-CDT-ACCESS-METHOD.        
08666      MOVE ZERO                   TO  CF-PERCENT-OF-CDT.           
08667                                                                   
08668      MOVE '1'                    TO  CF-CLAIM-CALC-METHOD         
08669      MOVE ZERO                   TO  CF-CALC-AMT-TOL              
08670                                      CF-MAX-REG-PMT               
08671                                      CF-MAX-REG-DAYS              
08672                                      CF-MAX-AUTO-PMT              
08673                                      CF-MAX-AUTO-MOS              
08674                                      CF-CALC-DAYS-TOL.            
08675                                                                   
08676      MOVE +60                    TO  CF-DAYS-BEFORE-CLOSED.       
08677      MOVE +18                    TO  CF-MONTHS-BEFORE-PURGED.     
08678                                                                   
08679      MOVE '111111'               TO  CF-RESERVE-CONTROLS.         
08680      MOVE ZERO                   TO  CF-PERCENT-OF-CDT.           
08681                                                                   
08682      MOVE 'CARRIER NOT FOUND ON CONTROL FILE - DEFAULTS USED'     
08683                                  TO  WS-D1-MESSAGE.               
08684                                                                   
08685      MOVE WS-DETAIL1             TO  PRT.                         
08686      PERFORM WRITE-A-LINE.                                        
08687                                                                   
08688  8600-EXIT.                                                       
08689      EXIT.                                                        
08690                                  EJECT                            
CIDMOD 8600-DIS-PRINT SECTION.                                          
CIDMOD                                                                  
CIDMOD 8600-DISPLAY-PRT.                                                
CIDMOD                                                                  
CIDMOD     IF  DIS-HEAD-SW =  'Y'                                       
CIDMOD       MOVE 'N' TO  DIS-HEAD-SW                                   
CIDMOD         PERFORM 8600-DISPLAY-HD THRU                             
CIDMOD             8600-HD-EXIT                                         
CIDMOD           GO TO 8600-DISPLAY-EXIT.                               
CIDMOD                                                                  
CIDMOD     IF  DIS-LINE-CNT GREATER THAN 59                             
CIDMOD         PERFORM 8600-DISPLAY-HD THRU                             
CIDMOD             8600-HD-EXIT.                                        
CIDMOD                                                                  
CIDMOD     MOVE   SPACES TO DIS-CC.                                     
CIDMOD     ADD +1 TO DIS-LINE-CNT.                                      
CIDMOD     WRITE  DISPLAY-REC FROM DISPLAY-LINE.                        
CIDMOD     MOVE   SPACES TO DISPLAY-REC.                                
CIDMOD     MOVE   SPACES TO DISPLAY-LINE.                               
CIDMOD                                                                  
CIDMOD                                                                  
CIDMOD 8600-DISPLAY-EXIT.                                               
CIDMOD     EXIT.                                                        
CIDMOD                                                                  
CIDMOD 8600-DIS-HEAD SECTION.                                           
CIDMOD                                                                  
CIDMOD 8600-DISPLAY-HD.                                                 
CIDMOD                                                                  
CIDMOD     MOVE '1' TO  DISPLAY-CC.                                     
CIDMOD     MOVE ZEROS TO DIS-LINE-CNT.                                  
CIDMOD     WRITE DISPLAY-REC FROM DISPLAY-HD-1.                         
CIDMOD     ADD  +1  TO DIS-LINE-CNT.                                    
CIDMOD     MOVE ' ' TO  DISPLAY-CC.                                     
CIDMOD     MOVE   SPACES TO DISPLAY-REC.                                
CIDMOD*    WRITE  DISPLAY-REC.                                          
CIDMOD     ADD +1 TO DIS-LINE-CNT.                                      
CIDMOD     WRITE  DISPLAY-REC FROM DISPLAY-HD-2.                        
CIDMOD     ADD +1 TO DIS-LINE-CNT.                                      
CIDMOD     MOVE   SPACES TO DISPLAY-REC.                                
CIDMOD     WRITE  DISPLAY-REC.                                          
CIDMOD                                                                  
CIDMOD                                                                  
CIDMOD                                                                  
CIDMOD 8600-HD-EXIT.                                                    
CIDMOD     EXIT.                                                        
CIDMOD                                 EJECT                            
08691  8700-WRITE-TRAILER SECTION.                                      
08692 *    NOTE ******************************************************* 
08693 *         *    THIS SECTION DOES THE WRITES TO THE ACTIVITY     * 
08694 *         *    TRAILER FILE                                     * 
08695 *         *******************************************************.
08696                                                                   
08697      IF AT-TRAILER-TYPE = '1'                                     
08698          MOVE 'E310'             TO  AT-RESERVES-LAST-UPDATED-BY  
08699          MOVE BIN-RUN-DATE       TO  AT-RESERVES-LAST-MAINT-DT    
08700        ELSE                                                       
08701      IF AT-TRAILER-TYPE = '2'                                     
08702          MOVE 'E310'             TO  AT-PAYMENT-LAST-UPDATED-BY   
08703          MOVE BIN-RUN-DATE       TO  AT-PAYMENT-LAST-MAINT-DT     
08704        ELSE                                                       
08705      IF AT-TRAILER-TYPE = '3'                                     
08706          MOVE 'E310'             TO  AT-AUTO-PAY-LAST-UPDATED-BY  
08707          MOVE BIN-RUN-DATE       TO  AT-AUTO-PAY-LAST-MAINT-DT    
08708        ELSE                                                       
08709      IF AT-TRAILER-TYPE = '4'                                     
08710          MOVE 'E310'             TO  AT-CORR-LAST-UPDATED-BY      
08711          MOVE BIN-RUN-DATE       TO  AT-CORR-LAST-MAINT-DT        
08712        ELSE                                                       
08713      IF AT-TRAILER-TYPE = '5'                                     
08714          MOVE 'E310'             TO  AT-ADDRESS-LAST-UPDATED-BY   
08715          MOVE BIN-RUN-DATE       TO  AT-ADDRESS-LAST-MAINT-DT     
08716        ELSE                                                       
08717      IF AT-TRAILER-TYPE = '6'                                     
08718          MOVE 'E310'             TO  AT-GEN-INFO-LAST-UPDATED-BY  
08719          MOVE BIN-RUN-DATE       TO  AT-GEN-INFO-LAST-MAINT-DT    
08720        ELSE                                                       
08721      IF AT-TRAILER-TYPE = '7'                                     
08722          MOVE 'E310'             TO  AT-PROMPT-LAST-UPDATED-BY    
08723          MOVE BIN-RUN-DATE       TO  AT-PROMPT-LAST-MAINT-DT      
08724        ELSE                                                       
08725      IF AT-TRAILER-TYPE = '8'                                     
08726          MOVE 'E310'             TO  AT-DENIAL-LAST-UPDATED-BY    
08727          MOVE BIN-RUN-DATE       TO  AT-DENIAL-LAST-MAINT-DT      
08728        ELSE                                                       
08729      IF AT-TRAILER-TYPE = '9'                                     
08730          MOVE 'E310'             TO  AT-INCURRED-LAST-UPDATED-BY  
08731        ELSE                                                       
08732      IF AT-TRAILER-TYPE = 'A'                                     
08733          MOVE 'E310'             TO  AT-FORM-LAST-UPDATED-BY      
08734          MOVE BIN-RUN-DATE       TO  AT-FORM-LAST-MAINT-DT.       
08735                                                                   
08736      MOVE WS-CURRENT-TIME        TO  AT-LAST-MAINT-HHMMSS.        
08737                                                                   
08738      MOVE +22                    TO SLR-KEY-LENGTH.               
08739      MOVE +200                   TO SLR-RECORD-LENGTH.            
08740      MOVE AT-CONTROL-PRIMARY     TO SLR-KEY.                      
08741      MOVE ACTIVITY-TRAILERS      TO SLR-RECORD-IMAGE.             
08742                                                                   
08743      MOVE 'ELTRLR'               TO SLR-DSID.                     
08744      MOVE 'A'                    TO SLR-ACTION.                   
08745                                                                   
08746      WRITE ACTIVITY-TRAILERS.                                     
08747                                                                   
08748      IF ELTRLR-FILE-STATUS NOT = ZERO                             
08749          MOVE 'ERROR OCCURRED WRITE - ELTRLR'                     
08750                                  TO  WS-ABEND-MESSAGE             
08751          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
08752          GO TO ABEND-PGM.                                         
08753                                                                   
08754 *    PERFORM LOG-JOURNAL-RECORD.                                  
08755                                                                   
08756  8700-EXIT.                                                       
08757      EXIT.                                                        
08758                                  EJECT                            
08759  8800-WRITE-CERTIFICATE SECTION.                                  
08760 *    NOTE ******************************************************* 
08761 *         *    THIS SECTION DOES THE WRITES TO THE CERTIFICATE  * 
08762 *         *    MASTER FILE                                      * 
08763 *         *******************************************************.
08764                                                                   
08765      MOVE +33                    TO SLR-KEY-LENGTH.               
08766      MOVE +450                   TO SLR-RECORD-LENGTH.            
08767      MOVE CM-CONTROL-PRIMARY     TO SLR-KEY.                      
08768      MOVE CERTIFICATE-MASTER     TO SLR-RECORD-IMAGE.             
08769                                                                   
08770      MOVE 'ELCERT'               TO SLR-DSID.                     
08771      MOVE 'A'                    TO SLR-ACTION.                   
08772                                                                   
08773      WRITE CERTIFICATE-MASTER.                                    
08774                                                                   
08775      ADD +1  TO  WS-CERTS-ADDED.                                  
08776                                                                   
08777      MOVE ZERO                   TO  WS-CERT-NOT-FOUND.           
08778                                                                   
08779      IF ELCERT-FILE-STATUS = '02'                                 
08780          MOVE ZERO               TO  ELCERT-FILE-STATUS.          
08781                                                                   
08782      IF ELCERT-FILE-STATUS NOT = ZERO                             
08783          MOVE 'ERROR OCCURRED WRITE - ELCERT'                     
08784                                  TO  WS-ABEND-MESSAGE             
08785          MOVE ELCERT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
08786          GO TO ABEND-PGM.                                         
08787                                                                   
08788 *    PERFORM LOG-JOURNAL-RECORD.                                  
08789                                                                   
08790  8800-EXIT.                                                       
08791      EXIT.                                                        
08792                                  EJECT                            
08793  9100-CREATE-CERTIFICATE SECTION.                                 
08794 *    NOTE ******************************************************* 
08795 *         *      THIS SECTION FORMATS THE CERTIFICATE RECORD    * 
08796 *         *  IF THE CERTIFICATE HAS BEEN DELETED FOR THIS CLAIM * 
08797 *         *******************************************************.
08798                                                                   
08799      MOVE SPACES                 TO CERTIFICATE-MASTER.           
08800                                                                   
08801      MOVE 'CM'                   TO CM-RECORD-ID.                 
08802                                                                   
08803      MOVE WS-COMPANY-CODE        TO CM-COMPANY-CD                 
08804                                     CM-COMPANY-CD-A1              
08805                                     CM-COMPANY-CD-A2              
08806                                     CM-COMPANY-CD-A4              
08807                                     CM-COMPANY-CD-A5.             
08808                                                                   
08809      MOVE CL-CERT-CARRIER        TO CM-CARRIER.                   
08810      MOVE CL-CERT-GROUPING       TO CM-GROUPING.                  
08811      MOVE CL-CERT-STATE          TO CM-STATE.                     
08812      MOVE CL-CERT-ACCOUNT        TO CM-ACCOUNT.                   
08813      MOVE CL-CERT-NO             TO CM-CERT-NO                    
08814                                     CM-CERT-NO-A4.                
08815      MOVE CL-CERT-EFF-DT         TO CM-CERT-EFF-DT.               
08816      MOVE CL-INSURED-LAST-NAME   TO CM-INSURED-LAST-NAME          
08817                                     CM-PART-LAST-NAME-A5.         
08818      MOVE CL-INSURED-1ST-NAME    TO  WS-INITIAL1                  
08819                                      CM-INSURED-FIRST-NAME.       
08820      MOVE CL-INSURED-MID-INIT    TO  WS-INITIAL2.                 
08821      MOVE WS-INITIALS            TO  CM-INSURED-INITIALS          
08822                                      CM-INSURED-INITIALS-A5.      
08823      MOVE CL-SOC-SEC-NO          TO CM-SOC-SEC-NO.                
08824      MOVE CM-STATE               TO CM-MEMB-STATE.                
08825      MOVE CM-ACCOUNT             TO CM-MEMB-ACCOUNT.              
08826                                                                   
08827      MOVE ZERO                   TO  CM-INSURED-ISSUE-AGE         
08828                                      CM-INSURED-JOINT-AGE.        
08829                                                                   
08830      MOVE CL-CLAIM-PREM-TYPE     TO  CM-PREMIUM-TYPE.             
08831      MOVE CL-INSURED-SEX-CD      TO  CM-INSURED-SEX.              
08832                                                                   
08833      MOVE '01'                   TO  CM-LF-BENEFIT-CD             
08834                                      CM-AH-BENEFIT-CD.            
08835      MOVE +1                     TO  CM-LF-ORIG-TERM              
08836                                      CM-AH-ORIG-TERM.             
08837      MOVE ZEROS                  TO  CM-LF-BENEFIT-AMT            
08838                                      CM-LF-PREMIUM-AMT            
08839                                      CM-LF-REMAINING-AMT          
08840                                      CM-LF-ITD-CANCEL-AMT         
08841                                      CM-LF-ITD-DEATH-AMT          
08842                                      CM-AH-BENEFIT-AMT            
08843                                      CM-AH-PREMIUM-AMT            
08844                                      CM-AH-ITD-CANCEL-AMT         
08845                                      CM-AH-ITD-LUMP-PMT           
08846                                      CM-LOAN-APR                  
08847                                      CM-PAY-FREQUENCY             
08848                                      CM-LOAN-BALANCE              
08849                                      CM-LF-DEV-CODE               
08850                                      CM-AH-DEV-CODE               
08851                                      CM-RATE-CLASS                
08852                                      CM-LF-CRITICAL-PERIOD        
08853                                      CM-LF-TERM-IN-DAYS           
08854                                      CM-LF-DEV-PCT                
08855                                      CM-LF-ALT-BENEFIT-AMT        
08856                                      CM-LF-ALT-PREMIUM-AMT        
08857                                      CM-LF-NSP-PREMIUM-AMT        
08858                                      CM-AH-CRITICAL-PERIOD        
08859                                      CM-AH-DEV-PCT                
08860                                      CM-AH-NSP-PREMIUM-AMT        
08861                                      CM-AH-ITD-AH-PMT             
08862                                      CM-LIVES                     
08863                                      CM-LOAN-TERM                 
08864                                      CM-LF-PREMIUM-RATE           
08865                                      CM-LF-ALT-PREMIUM-RATE       
08866                                      CM-AH-PREMIUM-RATE           
08867                                      CM-PMT-EXTENSION-DAYS        
08868                                      CM-LIFE-COMM-PCT             
08869                                      CM-AH-COMM-PCT.              
08870                                                                   
08871      MOVE LOW-VALUES             TO  CM-AH-CANCEL-DT              
08872                                      CM-LF-CANCEL-DT              
08873                                      CM-AH-SETTLEMENT-DT          
08874                                      CM-LF-DEATH-DT               
08875                                      CM-ENTRY-DT                  
08876                                      CM-LF-DEATH-EXIT-DT          
08877                                      CM-LF-CANCEL-EXIT-DT         
08878                                      CM-AH-SETTLEMENT-EXIT-DT     
08879                                      CM-AH-CANCEL-EXIT-DT         
08880                                      CM-AH-PAID-THRU-DT           
08881                                      CM-LF-LOAN-EXPIRE-DT         
08882                                      CM-AH-LOAN-EXPIRE-DT         
08883                                      CM-LOAN-1ST-PMT-DT           
08884                                      CM-LAST-MONTH-END            
08885                                      CM-LAST-ADD-ON-DT.           
08886                                                                   
08887      MOVE '4'                    TO  CM-LF-CURRENT-STATUS         
08888                                      CM-AH-CURRENT-STATUS         
08889                                      CM-ENTRY-STATUS.             
08890                                                                   
08891      MOVE '2'                    TO  CM-CLAIM-INTERFACE-SW.       
08892                                                                   
08893      MOVE +1                     TO  CM-CLAIM-ATTACHED-COUNT.     
08894                                                                   
08895                                                                   
08896  9199-EXIT.                                                       
08897      EXIT.                                                        
08898                                  EJECT                            
08899  9200-BUILD-ZERO-TRAILER SECTION.                                 
08900 *    NOTE ******************************************************* 
08901 *         *      THIS SECTION FORMATS THE ZERO TRAILER IF IT    * 
08902 *         *  HAS BEEN DELETED.                                  * 
08903 *         *******************************************************.
08904                                                                   
08905      MOVE SPACES                 TO ACTIVITY-TRAILERS.            
08906                                                                   
08907      MOVE 'AT'                   TO AT-RECORD-ID.                 
08908                                                                   
08909      MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY.           
08910      MOVE ZERO                   TO AT-SEQUENCE-NO.               
08911                                                                   
08912      MOVE '1'                    TO AT-TRAILER-TYPE.              
08913                                                                   
08914      MOVE BIN-RUN-DATE           TO AT-RECORDED-DT.               
08915                                                                   
08916      MOVE 'E310'                 TO AT-RECORDED-BY.               
08917                                                                   
08918      MOVE '1'                    TO AT-MANUAL-SW                  
08919                                     AT-FUTURE-SW                  
08920                                     AT-PTC-SW                     
08921                                     AT-IBNR-SW                    
08922                                     AT-PTC-LF-SW                  
08923                                     AT-CDT-ACCESS-METHOD.         
08924                                                                   
08925      MOVE ZERO                   TO  AT-PERCENT-OF-CDT            
08926                                      AT-FUTURE-RESERVE            
08927                                      AT-PAY-CURRENT-RESERVE       
08928                                      AT-IBNR-RESERVE              
08929                                                                   
08930      MOVE LOW-VALUES             TO  AT-LAST-COMPUTED-DT.         
08931                                                                   
08932      MOVE CL-TOTAL-PAID-AMT      TO  AT-CURRENT-MANUAL-RESERVE.   
08933                                                                   
08934      MOVE ZERO                   TO  AT-ITD-ADDITIONAL-RESERVE.   
08935                                                                   
08936      MOVE '1'                    TO  AT-EXPENSE-METHOD.           
08937                                                                   
08938      MOVE ZERO                   TO  AT-EXPENSE-PERCENT           
08939                                      AT-EXPENSE-DOLLAR            
08940                                      AT-ITD-PAID-EXPENSES         
08941                                      AT-ITD-CHARGEABLE-EXPENSE    
08942                                      AT-ITD-LIFE-REFUNDS          
08943                                      AT-ITD-AH-REFUNDS            
08944                                      AT-INITIAL-MANUAL-RESERVE.   
08945                                                                   
08946      MOVE WS-EXPENSE-CONTROLS    TO  AT-EXPENSE-CONTROLS.         
08947                                                                   
08948      MOVE LOW-VALUES             TO AT-OPEN-CLOSE-DATE (1)        
08949                                     AT-OPEN-CLOSE-DATE (2)        
08950                                     AT-OPEN-CLOSE-DATE (3)        
08951                                     AT-OPEN-CLOSE-DATE (4)        
08952                                     AT-OPEN-CLOSE-DATE (5)        
08953                                     AT-OPEN-CLOSE-DATE (6).       
08954                                                                   
08955      MOVE CL-FILE-ESTABLISH-DT   TO AT-OPEN-CLOSE-DATE (1).       
08956                                                                   
08957      MOVE 'O'                    TO AT-OPEN-CLOSE-TYPE (1).       
08958                                                                   
08959      MOVE 'E310'                 TO AT-OPEN-CLOSE-REASON (1).     
08960                                                                   
08961      IF CLAIM-IS-CLOSED                                           
08962          MOVE CL-LAST-CLOSE-DT   TO AT-OPEN-CLOSE-DATE (2)        
08963          MOVE 'C'                TO AT-OPEN-CLOSE-TYPE (2)        
08964          MOVE 'FINAL'            TO AT-OPEN-CLOSE-REASON (2).     
08965                                                                   
08966  9290-EXIT.                                                       
08967      EXIT.                                                        
08968      EJECT                                                        
CIDMOD 9300-CSO-SPECIAL-PROCESSING.                                     
CIDMOD     PERFORM 9310-CREATE-DLYACTV-RECORD THRU 9310-EXIT.           
CIDMOD 9300-EXIT.                                                       
CIDMOD     EXIT.                                                        
CIDMOD                                                                  
CIDMOD 9310-CREATE-DLYACTV-RECORD.                                      
CIDMOD     MOVE LOW-VALUES             TO  DAILY-ACTIVITY-RECORD.       
CIDMOD     MOVE CL-CONTROL-PRIMARY     TO  DA-KEY.                      
CIDMOD     MOVE ZEROS                  TO  DA-TRAILER-SEQ-NO.           
CIDMOD     IF WS-CSO-ACTION-TYPE EQUAL 'C'                              
CIDMOD       MOVE 'N'                  TO  DA-RECORD-TYPE               
CIDMOD     ELSE                                                         
CIDMOD       MOVE 'M'                  TO  DA-RECORD-TYPE.              
CIDMOD     WRITE DAILY-ACTIVITY-RECORD.                                 
CIDMOD     IF DLYACTV-FILE-STATUS EQUAL '22'                            
CIDMOD       ADD 1                      TO DLYACTV-DUP                  
CIDMOD       GO TO 9310-EXIT                                            
CIDMOD     ELSE                                                         
CIDMOD       NEXT SENTENCE.                                             
CIDMOD     IF DLYACTV-FILE-STATUS NOT EQUAL '00'                        
CIDMOD       DISPLAY 'ERROR OCCURRED ON DLYACTV WRITE'                  
CIDMOD       DISPLAY 'DLYACTV FILE STATUS = ' DLYACTV-FILE-STATUS       
CIDMOD                                                                  
CIDMOD       ADD  +1  TO  ERROR-COUNT                                   
CIDMOD         MOVE 'ERROR ON DLYACTV WRITE   ' TO DIS-LINE-REASON      
CIDMOD         MOVE SPACES                     TO  DIS-LINE-REC         
CIDMOD         PERFORM 8600-DISPLAY-PRT THRU                            
CIDMOD               8600-DISPLAY-EXIT                                  
CIDMOD         MOVE 'DLYACTV FILE STATUS  = ' TO DIS-LINE-REASON        
CIDMOD         MOVE DLYACTV-FILE-STATUS        TO DIS-LINE-REC          
CIDMOD         PERFORM 8600-DISPLAY-PRT THRU                            
CIDMOD               8600-DISPLAY-EXIT                                  
CIDMOD                                                                  
CIDMOD       GO TO 9310-EXIT                                            
CIDMOD     ELSE                                                         
CIDMOD       NEXT SENTENCE.                                             
CIDMOD     ADD 1                       TO DLYACTV-OUT.                  
CIDMOD                                                                  
CIDMOD 9310-EXIT.                                                       
CIDMOD     EXIT.                                                        
CIDMOD                                                                  
CIDMOD     EJECT                                                        
08969  9300-AIMS-CL-MSTR-INTERFACE   SECTION.                           
08970                                                                   
08971 *    NOTE ******************************************************* 
08972 *         *                                                     * 
08973 *         *    THE FOLLOWING SECTION OF CODE CREATES 'AIMS'     * 
08974 *         *  INTERFACE RECORDS FOR CLAIM MASTER RECORDS.        * 
08975 *         *                                                     * 
08976 *         ******************************************************* 
08977                                                                   
08978      IF CL-ASSOCIATES = 'I' OR 'M'                                
08979          NEXT SENTENCE                                            
08980        ELSE                                                       
08981          GO TO 9349-EXIT.                                         
08982                                                                   
08983      IF CL-FILE-ESTABLISH-DT GREATER THAN WS-LAST-PROCESS-DT      
08984          MOVE 'PA'               TO AIMS-INTERFACE-RECORD         
08985          MOVE 9999               TO AI-SEQUENCE-NBR               
08986          MOVE '1'                TO AI-OPEN-FLAG                  
08987          PERFORM 9350-CREATE-CL-INTERFACE-REC THRU 9399-EXIT.     
08988                                                                   
08989      IF CL-LAST-CLOSE-DT GREATER THAN WS-LAST-PROCESS-DT AND      
08990          WS-AIG-PMT-INTERFACE-SW = 'N'                            
08991          MOVE 'PA'               TO AIMS-INTERFACE-RECORD         
08992          MOVE ZERO               TO AI-SEQUENCE-NBR               
08993          MOVE '0'                TO AI-OPEN-FLAG                  
08994          PERFORM 9350-CREATE-CL-INTERFACE-REC THRU 9399-EXIT.     
08995                                                                   
08996      IF CL-LAST-REOPEN-DT GREATER THAN WS-LAST-PROCESS-DT         
08997          MOVE 'PA'               TO AIMS-INTERFACE-RECORD         
08998          MOVE 9998               TO AI-SEQUENCE-NBR               
08999          MOVE '2'                TO AI-OPEN-FLAG                  
09000          PERFORM 9350-CREATE-CL-INTERFACE-REC THRU 9399-EXIT.     
09001                                                                   
09002  9349-EXIT.                                                       
09003      EXIT.                                                        
09004                                                                   
09005                                                                   
09006  9350-CREATE-CL-INTERFACE-REC.                                    
09007                                                                   
09008 *    *************************************************************
09009 *    *  THE FOLLOWING SETS COMPANY IDENTIFICATION BY PUTTING A   *
09010 *    *  '1' IN THE HIGH-ORDER DIGIT OF THE SEQUENCE NUMBER,      *
09011 *    *  FOR 'AUK' RECORDS.                                       *
09012 *    *************************************************************
09013                                                                   
121902*    IF WS-COMPANY-ID = 'AUK'                                     
121902*        ADD 10000 TO AI-SEQUENCE-NBR.                            
09016                                                                   
09017      IF CL-CERT-PRIME IS NOT NUMERIC                              
09018          MOVE CL-CARRIER         TO WS-D1-CARRIER                 
09019          MOVE CL-CLAIM-NO        TO WS-D1-CLAIM-NO                
09020          MOVE CL-CERT-NO         TO WS-D1-CERT-NO                 
09021          PERFORM 5000-MOVE-NAME                                   
09022          MOVE WS-NAME-WORK       TO WS-D1-INSURED-NAME            
09023          MOVE 'INVALID CERT NUMBER - INTERFACE BYPASSED'          
09024                                  TO WS-D1-MESSAGE                 
09025          MOVE WS-DETAIL1         TO PRT                           
09026          PERFORM WRITE-A-LINE                                     
09027          MOVE SPACES             TO WS-DETAIL1                    
09028          GO TO 9399-EXIT.                                         
09029                                                                   
09030      MOVE CL-CERT-PRIME          TO WS-CERT-PRIME.                
09031      MOVE WS-CERT-PRIME-N        TO AI-POLICY-NBR.                
09032                                                                   
09033      MOVE SPACES                 TO DC-OPTION-CODE.               
09034                                                                   
09035      MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1.                
09036      PERFORM 8500-DATE-CONVERSION.                                
09037      IF NOT DATE-CONVERSION-ERROR                                 
09038          MOVE DC-GREG-DATE-1-MDY TO AI-LOSS-DATE                  
09039      ELSE                                                         
09040          MOVE ZEROS              TO AI-LOSS-DATE.                 
09041                                                                   
09042      MOVE CL-REPORTED-DT         TO DC-BIN-DATE-1.                
09043      PERFORM 8500-DATE-CONVERSION.                                
09044      IF NOT DATE-CONVERSION-ERROR                                 
09045          MOVE DC-GREG-DATE-1-MDY TO AI-CLAIM-ENTRY-DATE           
09046      ELSE                                                         
09047          MOVE ZEROS              TO AI-CLAIM-ENTRY-DATE.          
09048                                                                   
09049      MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1.                
09050      PERFORM 8500-DATE-CONVERSION.                                
09051      IF NOT DATE-CONVERSION-ERROR                                 
09052          MOVE DC-GREG-DATE-1-MDY TO AI-CLAIM-PAYMENT-DATE         
09053      ELSE                                                         
09054          MOVE ZEROS              TO AI-CLAIM-PAYMENT-DATE.        
09055                                                                   
09056      MOVE ZEROS                  TO AI-CLAIM-BEGIN-DATE           
09057                                     AI-CLAIM-END-DATE.            
09058                                                                   
09059      MOVE CL-INSURED-BIRTH-DT    TO DC-BIN-DATE-1.                
09060      PERFORM 8500-DATE-CONVERSION.                                
09061      IF NOT DATE-CONVERSION-ERROR                                 
09062          MOVE DC-GREG-DATE-1-MDY TO AI-INSURED-BIRTH-DATE         
09063      ELSE                                                         
09064          MOVE ZEROS              TO AI-INSURED-BIRTH-DATE.        
09065                                                                   
09066      MOVE ZERO                   TO AI-DRAFT-AMT.                 
09067      MOVE CL-CLAIM-NO            TO AI-CLAIM-NBR.                 
09068      MOVE CL-CARRIER             TO AI-CARRIER.                   
09069                                                                   
09070      IF CL-CERT-SFX = 'R' OR 'S' OR 'U'                           
09071          MOVE CL-CERT-SFX        TO AI-CLAIM-TYPE                 
09072        ELSE                                                       
09073          MOVE CL-CLAIM-TYPE      TO AI-CLAIM-TYPE.                
09074                                                                   
09075      MOVE '0'                    TO AI-EXCEPTION-FLAG             
09076                                     AI-NEGATIVE-AMT-FLAG.         
09077      MOVE '1'                    TO AI-CLASS-FLAG.                
09078                                                                   
09079      IF (AI-OPEN-FLAG = '1' OR '2')                               
09080        IF CL-CLAIM-TYPE = 'L'                                     
09081          MOVE '4'                TO AI-PAYMENT-TYPE-FLAG          
09082        ELSE                                                       
09083          IF CL-CERT-SFX = 'R' OR                                  
09084            AT-AIG-UNEMP-IND = 'U'                                 
09085            MOVE '6'              TO AI-PAYMENT-TYPE-FLAG          
09086          ELSE                                                     
09087            MOVE '1'              TO AI-PAYMENT-TYPE-FLAG          
09088      ELSE                                                         
09089        IF CL-CLAIM-TYPE = 'L'                                     
09090            MOVE '4'              TO AI-PAYMENT-TYPE-FLAG          
09091        ELSE                                                       
09092          IF CL-CERT-SFX = 'R' OR                                  
09093             AT-AIG-UNEMP-IND = 'U'                                
09094            MOVE '8'              TO AI-PAYMENT-TYPE-FLAG          
09095          ELSE                                                     
09096            MOVE '3'              TO AI-PAYMENT-TYPE-FLAG.         
09097                                                                   
09098      MOVE ZERO                   TO AI-IPARTN                     
09099                                     AI-LOAN-BRANCH                
09100                                     AI-IPACDB                     
09101                                     AI-IPAVDI.                    
09102                                                                   
09103      WRITE AIMS-INTERFACE-REC                                     
09104         FROM AIMS-INTERFACE-RECORD.                               
09105                                                                   
09106  9399-EXIT.                                                       
09107      EXIT.                                                        
09108      EJECT                                                        
09109  9400-AIMS-PAYMENT-INTERFACE.                                     
09110                                                                   
09111 *    NOTE ******************************************************* 
09112 *         *                                                     * 
09113 *         *    THE FOLLOWING SECTION OF CODE CREATES 'AIMS'     * 
09114 *         *  INTERFACE RECORDS FOR PAYMENT TRAILER RECORDS.     * 
09115 *         *                                                     * 
09116 *         ******************************************************* 
09117                                                                   
09118      IF AT-CHECK-WRITTEN-DT GREATER THAN WS-LAST-PROCESS-DT OR    
09119         AT-VOID-DT GREATER THAN WS-LAST-PROCESS-DT                
09120          NEXT SENTENCE                                            
09121        ELSE                                                       
09122          GO TO 9499-EXIT.                                         
09123                                                                   
09124      IF AT-CHECK-WRITTEN-DT = AT-VOID-DT                          
09125          GO TO 9499-EXIT.                                         
09126                                                                   
09127      IF CL-ASSOCIATES = 'I' OR 'M'                                
09128          NEXT SENTENCE                                            
09129        ELSE                                                       
09130          GO TO 9499-EXIT.                                         
09131                                                                   
09132      IF AT-PAYMENT-TYPE = '5' OR '6'                              
09133          GO TO 9499-EXIT.                                         
09134                                                                   
09135      IF AT-CERT-PRIME = 'INCOMPLETE'                              
09136          GO TO 9499-EXIT.                                         
09137                                                                   
09138 *    *************************************************************
09139 *    *  THE FOLLOWING IS FOR ACTION BY PROGRAM EL190; WHEN IT IS *
09140 *    *  NOT NECESSARY TO RETRANSMIT DELETED REJECT RECORDS.      *
09141 *    *************************************************************
09142                                                                   
09143      IF AT-PAYMENT-LAST-UPDATED-BY = 'E190'                       
09144          GO TO 9499-EXIT.                                         
09145                                                                   
09146      MOVE 'PA'                   TO AIMS-INTERFACE-RECORD.        
09147      MOVE AT-SEQUENCE-NO         TO AI-SEQUENCE-NBR.              
09148                                                                   
09149 *    *************************************************************
09150 *    *  THE FOLLOWING SETS COMPANY IDENTIFICATION BY PUTTING A   *
09151 *    *  '1' IN THE HIGH-ORDER DIGIT OF THE SEQUENCE NUMBER,      *
09152 *    *  FOR 'AUK' RECORDS.                                       *
09153 *    *************************************************************
09154                                                                   
121902*    IF WS-COMPANY-ID = 'AUK'                                     
121902*        ADD 10000 TO AI-SEQUENCE-NBR.                            
09157                                                                   
09158      IF CL-CERT-PRIME IS NOT NUMERIC                              
09159          MOVE CL-CARRIER         TO WS-D1-CARRIER                 
09160          MOVE CL-CLAIM-NO        TO WS-D1-CLAIM-NO                
09161          MOVE CL-CERT-NO         TO WS-D1-CERT-NO                 
09162          PERFORM 5000-MOVE-NAME                                   
09163          MOVE WS-NAME-WORK       TO WS-D1-INSURED-NAME            
09164          MOVE 'INVALID CERT NUMBER - INTERFACE BYPASSED'          
09165                                  TO WS-D1-MESSAGE                 
09166          MOVE WS-DETAIL1         TO PRT                           
09167          PERFORM WRITE-A-LINE                                     
09168          MOVE SPACES             TO WS-DETAIL1                    
09169          GO TO 9499-EXIT.                                         
09170                                                                   
09171      MOVE CL-CERT-PRIME          TO AI-POLICY-NBR.                
09172                                                                   
09173      MOVE SPACES                 TO DC-OPTION-CODE.               
09174                                                                   
09175      MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1.                
09176      PERFORM 8500-DATE-CONVERSION.                                
09177      IF NOT DATE-CONVERSION-ERROR                                 
09178          MOVE DC-GREG-DATE-1-MDY TO AI-LOSS-DATE                  
09179      ELSE                                                         
09180          MOVE ZEROS              TO AI-LOSS-DATE.                 
09181                                                                   
09182      MOVE CL-REPORTED-DT         TO DC-BIN-DATE-1.                
09183      PERFORM 8500-DATE-CONVERSION.                                
09184      IF NOT DATE-CONVERSION-ERROR                                 
09185          MOVE DC-GREG-DATE-1-MDY TO AI-CLAIM-ENTRY-DATE           
09186      ELSE                                                         
09187          MOVE ZEROS              TO AI-CLAIM-ENTRY-DATE.          
09188                                                                   
09189      IF AT-VOID-DT GREATER THAN WS-LAST-PROCESS-DT                
09190          MOVE AT-VOID-DT             TO DC-BIN-DATE-1             
09191        ELSE                                                       
09192          MOVE AT-CHECK-WRITTEN-DT    TO DC-BIN-DATE-1.            
09193                                                                   
09194      PERFORM 8500-DATE-CONVERSION.                                
09195      IF NOT DATE-CONVERSION-ERROR                                 
09196          MOVE DC-GREG-DATE-1-MDY TO AI-CLAIM-PAYMENT-DATE         
09197        ELSE                                                       
09198          MOVE ZEROS              TO AI-CLAIM-PAYMENT-DATE.        
09199                                                                   
09200      MOVE AT-PAID-FROM-DT        TO DC-BIN-DATE-1.                
09201      PERFORM 8500-DATE-CONVERSION.                                
09202      IF NOT DATE-CONVERSION-ERROR                                 
09203          MOVE DC-GREG-DATE-1-MDY TO AI-CLAIM-BEGIN-DATE           
09204      ELSE                                                         
09205          MOVE ZEROS              TO AI-CLAIM-BEGIN-DATE.          
09206                                                                   
09207      MOVE AT-PAID-THRU-DT        TO DC-BIN-DATE-1.                
09208      PERFORM 8500-DATE-CONVERSION.                                
09209      IF NOT DATE-CONVERSION-ERROR                                 
09210          MOVE DC-GREG-DATE-1-MDY TO AI-CLAIM-END-DATE             
09211      ELSE                                                         
09212          MOVE ZEROS              TO AI-CLAIM-END-DATE.            
09213                                                                   
09214      MOVE CL-INSURED-BIRTH-DT    TO DC-BIN-DATE-1.                
09215      PERFORM 8500-DATE-CONVERSION.                                
09216      IF NOT DATE-CONVERSION-ERROR                                 
09217          MOVE DC-GREG-DATE-1-MDY TO AI-INSURED-BIRTH-DATE         
09218      ELSE                                                         
09219          MOVE ZEROS              TO AI-INSURED-BIRTH-DATE.        
09220                                                                   
09221      MOVE AT-AMOUNT-PAID         TO AI-DRAFT-AMT.                 
09222      MOVE AT-CLAIM-NO            TO AI-CLAIM-NBR.                 
09223      MOVE AT-CARRIER             TO AI-CARRIER.                   
09224                                                                   
09225      IF CL-CERT-SFX = 'R' OR 'S' OR 'U'                           
09226          MOVE CL-CERT-SFX        TO AI-CLAIM-TYPE                 
09227        ELSE                                                       
09228          MOVE CL-CLAIM-TYPE      TO AI-CLAIM-TYPE.                
09229                                                                   
09230      IF PAYMENT-WAS-FORCED                                        
09231          MOVE '1'                TO AI-EXCEPTION-FLAG             
09232      ELSE                                                         
09233          MOVE '0'                TO AI-EXCEPTION-FLAG.            
09234                                                                   
09235      MOVE '0'                    TO AI-OPEN-FLAG.                 
09236 *                                                                 
09237 ******************************************************************
09238 *    IN THE FOLLOWING 'IF' STATEMENT, FOR A REGULAR PAYMENT, THE *
09239 *  AI-NEGATIVE-AMT-FLAG IS '0' FOR POSITIVE (OR ZERO) AMOUNTS,   *
09240 *  AND '1' FOR NEGATIVE AMOUNTS.  HOWEVER, IF THE TRANSACTION    *
09241 *  IS TO VOID A PREVIOUS PAYMENT, THE SIGN REVERSES ('1' IS      *
09242 *  POSITIVE (OR ZERO), AND '0' IS NEGATIVE).                     *
09243 ******************************************************************
09244 *                                                                 
09245      IF AT-VOID-DT GREATER THAN WS-LAST-PROCESS-DT                
09246          IF AT-AMOUNT-PAID < ZERO                                 
09247              MOVE '0'            TO AI-NEGATIVE-AMT-FLAG          
09248            ELSE                                                   
09249              MOVE '1'            TO AI-NEGATIVE-AMT-FLAG          
09250        ELSE                                                       
09251 **** IF AT-VOID-DT NOT = BIN-RUN-DATE   *** FOR READABILITY ***   
09252          IF AT-AMOUNT-PAID < ZERO                                 
09253              MOVE '1'            TO AI-NEGATIVE-AMT-FLAG          
09254            ELSE                                                   
09255              MOVE '0'            TO AI-NEGATIVE-AMT-FLAG.         
09256                                                                   
09257      IF AT-CASH-PAYMENT = 'Y'                                     
09258          MOVE '1'                TO AI-CLASS-FLAG                 
09259        ELSE                                                       
09260          MOVE '2'                TO AI-CLASS-FLAG.                
09261                                                                   
09262      IF AT-PAYMENT-TYPE = '1'                                     
09263          MOVE 'N'                TO WS-AIG-PMT-INTERFACE-SW       
09264          IF CL-CERT-SFX = 'R'                                     
09265              MOVE '6'            TO AI-PAYMENT-TYPE-FLAG          
09266            ELSE                                                   
09267              MOVE '1'            TO AI-PAYMENT-TYPE-FLAG.         
09268                                                                   
09269      IF AT-PAYMENT-TYPE = '2'                                     
09270          MOVE 'Y'                TO WS-AIG-PMT-INTERFACE-SW       
09271          IF CL-CERT-SFX = 'R'                                     
09272              MOVE '8'            TO AI-PAYMENT-TYPE-FLAG          
09273            ELSE                                                   
09274          IF AI-CLAIM-TYPE = 'L'                                   
09275              MOVE '4'            TO AI-PAYMENT-TYPE-FLAG          
09276            ELSE                                                   
09277              MOVE '3'            TO AI-PAYMENT-TYPE-FLAG.         
09278                                                                   
09279      IF AT-PAYMENT-TYPE = '3'                                     
09280          MOVE 'Y'                TO WS-AIG-PMT-INTERFACE-SW       
09281          IF CL-CERT-SFX = 'R'                                     
09282              MOVE '7'            TO AI-PAYMENT-TYPE-FLAG          
09283            ELSE                                                   
09284              MOVE '2'            TO AI-PAYMENT-TYPE-FLAG.         
09285                                                                   
09286      IF AT-PAYMENT-TYPE = '4'                                     
09287        IF CL-CLAIM-STATUS = 'O'                                   
09288          MOVE 'N'                TO WS-AIG-PMT-INTERFACE-SW       
09289          IF CL-CLAIM-TYPE = 'L'                                   
09290            MOVE '5'              TO AI-PAYMENT-TYPE-FLAG          
09291          ELSE                                                     
09292            IF CL-CERT-SFX = 'R'                                   
09293              MOVE '6'            TO AI-PAYMENT-TYPE-FLAG          
09294            ELSE                                                   
09295              MOVE '1'            TO AI-PAYMENT-TYPE-FLAG          
09296        ELSE                                                       
09297          MOVE 'Y'                TO WS-AIG-PMT-INTERFACE-SW       
09298          IF CL-CLAIM-TYPE = 'L'                                   
09299            MOVE '4'              TO AI-PAYMENT-TYPE-FLAG          
09300          ELSE                                                     
09301            IF CL-CERT-SFX = 'R'                                   
09302              MOVE '8'            TO AI-PAYMENT-TYPE-FLAG          
09303            ELSE                                                   
09304              MOVE '3'            TO AI-PAYMENT-TYPE-FLAG.         
09305                                                                   
09306      IF CL-CLAIM-TYPE = 'L'                                       
09307          NEXT SENTENCE                                            
09308      ELSE                                                         
09309          IF AT-VOID-DT IS GREATER THAN WS-LAST-PROCESS-DT         
022122             IF (CL-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F'
022122                 OR 'B' OR 'H') OR
09311                 (CL-CERT-SFX = 'U' OR 'S')                        
09312                  MOVE '1'        TO AI-PAYMENT-TYPE-FLAG          
09313              ELSE                                                 
09314                  IF CL-CERT-SFX = 'R'                             
09315                      MOVE '6'    TO AI-PAYMENT-TYPE-FLAG.         
09316                                                                   
09317 *    *************************************************************
09318 *    *  PAYMENTS ON CLOSED CLAIMS MUST REOPEN, THEN              *
09319 *    *  CLOSE AS FINAL.                                          *
09320 *    *************************************************************
09321                                                                   
09322      MOVE ZERO                   TO AI-IPARTN                     
09323                                     AI-LOAN-BRANCH                
09324                                     AI-IPACDB                     
09325                                     AI-IPAVDI.                    
09326                                                                   
09327      WRITE AIMS-INTERFACE-REC                                     
09328          FROM AIMS-INTERFACE-RECORD.                              
09329                                                                   
09330  9499-EXIT.                                                       
09331      EXIT.                                                        
09332      EJECT                                                        
09333  9500-DMD-ACTIVITY-FILE.                                          
09334      MOVE SPACES                 TO PROCESSOR-STATISTICS.         
09335      MOVE 'PS'                   TO SS-RECORD-ID.                 
09336      MOVE WS-DMD-PROCESSOR-ID    TO SS-PROCESSOR-ID.              
09337                                                                   
09338      MOVE WS-DMD-MAINTENANCE-DT  TO DC-BIN-DATE-1.                
09339      MOVE ' '                    TO DC-OPTION-CODE.               
09340      PERFORM 8500-DATE-CONVERSION.                                
09341      MOVE DC-GREG-DATE-1-YMD     TO SS-MAINTENANCE-DATE.          
09342                                                                   
09343      MOVE WS-DMD-MAINTENANCE-TYPE TO SS-MAINTENANCE-TYPE.         
09344      MOVE CL-COMPANY-CD          TO SS-COMPANY-CD.                
09345      MOVE CL-CARRIER             TO SS-CARRIER.                   
09346      MOVE CL-CLAIM-NO            TO SS-CLAIM-NO.                  
09347      MOVE CL-CERT-NO             TO SS-CERTIFICATE.               
09348                                                                   
09349 ***** INFORMATION RETRIEVED FROM CLAIM                            
09350      MOVE CL-CERT-GROUPING       TO SS-CERT-GROUPING.             
09351      MOVE CL-CERT-STATE          TO SS-CERT-STATE.                
09352      MOVE CL-CERT-ACCOUNT        TO SS-CERT-ACCOUNT.              
09353      MOVE CL-CLAIM-TYPE          TO SS-CLAIM-TYPE.                
09354      MOVE CL-CERT-EFF-DT         TO DC-BIN-DATE-1.                
09355      MOVE ' '                    TO DC-OPTION-CODE.               
09356      PERFORM 8500-DATE-CONVERSION.                                
09357      MOVE DC-GREG-DATE-1-YMD     TO SS-EFFECTIVE-DATE.            
09358                                                                   
09359 ***** INFORMATION RETRIEVED FROM CERT                             
09360      MOVE CM-PREMIUM-TYPE        TO SS-BENEFIT-TYPE.              
09361      IF CL-CLAIM-TYPE = 'L'                                       
09362          MOVE CM-LF-BENEFIT-CD   TO SS-BENEFIT-CODE               
09363          MOVE CM-LF-ORIG-TERM    TO SS-TERM                       
09364      ELSE                                                         
09365          MOVE CM-AH-BENEFIT-CD   TO SS-BENEFIT-CODE               
09366          MOVE CM-AH-ORIG-TERM    TO SS-TERM.                      
09367                                                                   
09368      IF SS-MAINTENANCE-TYPE = '06'                                
09369          IF WS-NOTE-RECORD-TO-WRITE                               
09370              IF SS-MAINTENANCE-DATE = WS-DMD-STAT-DATE            
09371                  GO TO 9500-EXIT                                  
09372              ELSE                                                 
09373                  MOVE PROCESSOR-STATISTICS    TO                  
09374                              WS-DMD-STORE-STAT-RECORD             
09375                  MOVE WS-DMD-SAVE-TO-WRITE TO                     
09376                                PROCESSOR-STATISTICS               
09377                  WRITE PROCESSOR-STATISTICS                       
09378                  MOVE WS-DMD-STORE-STAT-RECORD TO                 
09379                                WS-DMD-SAVE-TO-WRITE               
09380                  GO TO 9500-EXIT                                  
09381          ELSE                                                     
09382              MOVE 'Y'            TO WS-DMD-NOTE-FLAG              
09383              MOVE PROCESSOR-STATISTICS TO                         
09384                                WS-DMD-SAVE-TO-WRITE               
09385              GO TO 9500-EXIT.                                     
09386                                                                   
09387      WRITE PROCESSOR-STATISTICS.                                  
09388                                                                   
09389  9500-EXIT.                                                       
09390      EXIT.                                                        
09391      EJECT                                                        
09392  9501-CREATE-DMO.                                                 
09393                                                                   
09394      IF NEED-TO-OPEN-DMD-FILES                                    
09395          MOVE 'Y'                TO WS-DMD-FILES-OPEN             
09396          PERFORM 9501-OPEN-DLB6  THRU 9501-OPEN-DLB6-EXIT         
09397          PERFORM 9501-OPEN-DLB25 THRU 9501-OPEN-DLB25-EXIT.       
09398                                                                   
09399      MOVE SPACES                 TO DCT-COMMUNICATION-AREA.       
09400                                                                   
09401      MOVE CL-CERT-CARRIER        TO CN-CARRIER.                   
09402      MOVE CL-CERT-GROUPING       TO CN-GROUPING.                  
09403      MOVE CL-CERT-STATE          TO CN-STATE.                     
09404      MOVE CL-CERT-ACCOUNT        TO CN-ACCOUNT.                   
09405      MOVE CL-CERT-EFF-DT         TO CN-CERT-EFF-DT.               
09406      MOVE CL-COMPANY-CD          TO CN-COMPANY-CD.                
09407      MOVE CL-CERT-NO             TO CN-CERT-NO.                   
09408                                                                   
09409      READ ERNOTE.                                                 
09410                                                                   
09411      IF ERNOTE-FILE-STATUS = ZERO                                 
09412          MOVE CN-CSI-CC-BILL-BANK-ID TO DCT-BILLING-BANK-ID.      
09413                                                                   
09414      MOVE 'P'                    TO DCT-PROCESS-TYPE.             
09415      MOVE CL-BENEFICIARY         TO DCT-LOGIC-BENEFICIARY-ID.     
09416      MOVE CL-CCN                 TO DCT-CREDIT-CARD-NUMBER.       
09417                                                                   
09418      IF  CL-CERT-GROUPING (5:2) NOT GREATER THAN SPACES           
09419          MOVE 'CC'               TO DCT-PRODUCT-CODE              
09420      ELSE                                                         
09421          MOVE CL-CERT-GROUPING (5:2) TO DCT-PRODUCT-CODE.         
09422                                                                   
09423      MOVE '02'                   TO DCT-COLUMN-ID-REQUESTED.      
09424                                                                   
09425      CALL 'DLB006'  USING DCT-COMMUNICATION-AREA.                 
09426                                                                   
09427      IF DCT-RETURN-CODE = '01' OR '02'                            
09428          GO TO 9501-EXIT.                                         
09429                                                                   
09430      IF DCT-RETURN-CODE = '03'                                    
09431          MOVE 'BILLING BANK OR BENE MISSING, DLB006:RC=03'        
09432                                  TO WS-D1-MESSAGE                 
09433          MOVE WS-DETAIL1         TO PRT                           
09434          PERFORM WRITE-A-LINE                                     
09435          MOVE SPACES             TO WS-D1-MESSAGE                 
09436          GO TO 9501-EXIT.                                         
09437                                                                   
09438      IF DCT-RETURN-CODE = '04'                                    
09439          MOVE 'CREDT CARD NUMBER MISSING, DLB006:RC=04'           
09440                                  TO WS-D1-MESSAGE                 
09441          MOVE WS-DETAIL1         TO PRT                           
09442          PERFORM WRITE-A-LINE                                     
09443          MOVE SPACES             TO WS-D1-MESSAGE                 
09444          GO TO 9501-EXIT.                                         
09445                                                                   
09446      IF DCT-RETURN-CODE = '05'                                    
09447          MOVE 'PRODUCT CODE MISSING, DLB006:RC=05'                
09448                                  TO WS-D1-MESSAGE                 
09449          MOVE WS-DETAIL1         TO PRT                           
09450          PERFORM WRITE-A-LINE                                     
09451          MOVE SPACES             TO WS-D1-MESSAGE                 
09452          GO TO 9501-EXIT.                                         
09453                                                                   
09454      IF DCT-RETURN-CODE = '06' OR '07'                            
09455          MOVE 'INVALID COLUMN ID, DLB006:RC=06/07'                
09456                                  TO WS-D1-MESSAGE                 
09457          MOVE WS-DETAIL1         TO PRT                           
09458          PERFORM WRITE-A-LINE                                     
09459          MOVE SPACES             TO WS-D1-MESSAGE                 
09460          GO TO 9501-EXIT.                                         
09461                                                                   
09462      IF DCT-RETURN-CODE = '08'                                    
09463          MOVE 'CREDIT CARD PREFIX MISSING, DLB006:RC=08'          
09464                                  TO WS-D1-MESSAGE                 
09465          MOVE WS-DETAIL1         TO PRT                           
09466          PERFORM WRITE-A-LINE                                     
09467          MOVE SPACES             TO WS-D1-MESSAGE                 
09468          GO TO 9501-EXIT.                                         
09469                                                                   
09470      IF DCT-RETURN-CODE = 'Z1'                                    
09471          MOVE 'INVALID PROCESS TYPE, DLB006:RC=Z1'                
09472                                  TO WS-D1-MESSAGE                 
09473          MOVE WS-DETAIL1         TO PRT                           
09474          PERFORM WRITE-A-LINE                                     
09475          MOVE SPACES             TO WS-D1-MESSAGE                 
09476          GO TO ABEND-PGM.                                         
09477                                                                   
09478      IF DCT-RETURN-CODE = 'Z2' OR 'Z3'                            
09479          MOVE 'OPEN/CLOSE ERROR ON DCT TABLE, DLB006:RC=Z2/Z3'    
09480                                  TO WS-D1-MESSAGE                 
09481          MOVE WS-DETAIL1         TO PRT                           
09482          PERFORM WRITE-A-LINE                                     
09483          MOVE SPACES             TO WS-D1-MESSAGE                 
09484          GO TO ABEND-PGM.                                         
09485                                                                   
09486      IF DCT-RETURN-CODE = 'E1' OR 'E2'                            
09487          MOVE 'READ ERROR ON DCT TABLE, DLB006:RC=E1/E2'          
09488                                  TO WS-D1-MESSAGE                 
09489          MOVE WS-DETAIL1         TO PRT                           
09490          PERFORM WRITE-A-LINE                                     
09491          MOVE SPACES             TO WS-D1-MESSAGE                 
09492          GO TO ABEND-PGM.                                         
09493                                                                   
09494      IF DCT-RETURN-CODE NOT = 'OK'                                
09495          STRING 'UNKNOWN RETURN CODE, DLB006:RC='                 
09496              DELIMITED BY SIZE                                    
09497              DCT-RETURN-CODE DELIMITED BY SIZE                    
09498                                INTO WS-D1-MESSAGE                 
09499          MOVE WS-DETAIL1         TO PRT                           
09500          PERFORM WRITE-A-LINE                                     
09501          MOVE SPACES             TO WS-D1-MESSAGE                 
09502          GO TO ABEND-PGM.                                         
09503                                                                   
09504      MOVE SPACES                 TO DMO-COMMUNICATION-AREA.       
09505      MOVE 'CS'                   TO DM-RECORD-TYPE.               
09506      MOVE 'P'                    TO DM-PROCESS-TYPE.              
09507      MOVE DCT-DISTRIBUTION-CODE  TO DM-DIST-CODE.                 
09508      MOVE DCT-MAIL-CODE          TO DM-MAIL-CODE.                 
09509      MOVE CL-CCN                 TO DM-CREDIT-CARD-NUMBER.        
09510      MOVE CL-CLAIM-NO            TO DM-CLAIM-NO.                  
09511      MOVE CL-CERT-NO (4:1)       TO DM-CLAIM-TYPE.                
09512      MOVE WS-CURRENT-DATE-CYMD   TO DM-STATUS-DATE.               
09513                                                                   
09514      PERFORM 5000-MOVE-NAME.                                      
09515      MOVE WS-NAME-WORK           TO DM-INSURED-NAME.              
09516                                                                   
09517 ********** CHANGE TYPE = 'F' FOR FINAL PAYMENT,CLAIM STATUS = '4' 
09518 ********** CHANGE TYPE = 'Q' FOR AUTO CLOSING, CLAIM STATUS = '3' 
09519                                                                   
09520      MOVE CL-CARRIER                 TO DM-STAT-CARRIER.          
09521                                                                   
09522      IF AUTO-CLOSE-ACTIVITY                                       
09523          MOVE 'Q'                    TO DM-STAT-CHANGE-TYPE       
09524          MOVE '3'                    TO DM-CLAIM-STATUS           
09525      ELSE                                                         
09526      IF FINAL-PAYMENT-ACTIVITY                                    
09527          MOVE 'F'                    TO DM-STAT-CHANGE-TYPE       
09528          MOVE '4'                    TO DM-CLAIM-STATUS.          
09529                                                                   
09530      CALL 'DLB025'  USING DMO-COMMUNICATION-AREA.                 
09531                                                                   
09532      IF DM-RETURN-CODE = '01'                                     
09533          MOVE 'INVALID RECORD TYPE, DLB025:RC=02'                 
09534                                  TO WS-D1-MESSAGE                 
09535          MOVE WS-DETAIL1         TO PRT                           
09536          PERFORM WRITE-A-LINE                                     
09537          MOVE SPACES             TO WS-D1-MESSAGE                 
09538          GO TO 9501-EXIT.                                         
09539                                                                   
09540      IF DM-RETURN-CODE = '02'                                     
09541          MOVE 'DISTRIBUTION CODE MISSING, DLB025:RC=02'           
09542                                  TO WS-D1-MESSAGE                 
09543          MOVE WS-DETAIL1         TO PRT                           
09544          PERFORM WRITE-A-LINE                                     
09545          MOVE SPACES             TO WS-D1-MESSAGE                 
09546          GO TO 9501-EXIT.                                         
09547                                                                   
09548      IF DM-RETURN-CODE = '03'                                     
09549          MOVE 'MAIL CODE MISSING, DLB025:RC=03'                   
09550                                  TO WS-D1-MESSAGE                 
09551          MOVE WS-DETAIL1         TO PRT                           
09552          PERFORM WRITE-A-LINE                                     
09553          MOVE SPACES             TO WS-D1-MESSAGE                 
09554          GO TO 9501-EXIT.                                         
09555                                                                   
09556      IF DM-RETURN-CODE = '04'                                     
09557          MOVE 'INVALID CREDIT CARD NUMBER, DLB025:RC=04'          
09558                                  TO WS-D1-MESSAGE                 
09559          MOVE WS-DETAIL1         TO PRT                           
09560          PERFORM WRITE-A-LINE                                     
09561          MOVE SPACES             TO WS-D1-MESSAGE                 
09562          GO TO 9501-EXIT.                                         
09563                                                                   
09564      IF DM-RETURN-CODE = '05'                                     
09565          MOVE 'INSURED NAME MISSING, DLB025:RC=05'                
09566                                  TO WS-D1-MESSAGE                 
09567          MOVE WS-DETAIL1         TO PRT                           
09568          PERFORM WRITE-A-LINE                                     
09569          MOVE SPACES             TO WS-D1-MESSAGE                 
09570          GO TO 9501-EXIT.                                         
09571                                                                   
09572      IF DM-RETURN-CODE = '06'                                     
09573          MOVE 'CLAIM NUMBER MISSING, DLB025:RC=06'                
09574                                  TO WS-D1-MESSAGE                 
09575          MOVE WS-DETAIL1         TO PRT                           
09576          PERFORM WRITE-A-LINE                                     
09577          MOVE SPACES             TO WS-D1-MESSAGE                 
09578          GO TO 9501-EXIT.                                         
09579                                                                   
09580      IF DM-RETURN-CODE = '07'                                     
09581          MOVE 'CLAIM TYPE MISSING, DLB025:RC=07'                  
09582                                  TO WS-D1-MESSAGE                 
09583          MOVE WS-DETAIL1         TO PRT                           
09584          PERFORM WRITE-A-LINE                                     
09585          MOVE SPACES             TO WS-D1-MESSAGE                 
09586          GO TO 9501-EXIT.                                         
09587                                                                   
09588      IF DM-RETURN-CODE = '08'                                     
09589          MOVE 'INVALID CLAIM STATUS, DLB025:RC=08'                
09590                                  TO WS-D1-MESSAGE                 
09591          MOVE WS-DETAIL1         TO PRT                           
09592          PERFORM WRITE-A-LINE                                     
09593          MOVE SPACES             TO WS-D1-MESSAGE                 
09594          GO TO 9501-EXIT.                                         
09595                                                                   
09596      IF DM-RETURN-CODE = '09'                                     
09597          MOVE 'INVALID STATUS DATE, DLB025:RC=09'                 
09598                                  TO WS-D1-MESSAGE                 
09599          MOVE WS-DETAIL1         TO PRT                           
09600          PERFORM WRITE-A-LINE                                     
09601          MOVE SPACES             TO WS-D1-MESSAGE                 
09602          GO TO 9501-EXIT.                                         
09603                                                                   
09604      IF DM-RETURN-CODE = '10'                                     
09605          MOVE 'INVALID CHARGE TYPE, DLB025:RC=10'                 
09606                                  TO WS-D1-MESSAGE                 
09607          MOVE WS-DETAIL1         TO PRT                           
09608          PERFORM WRITE-A-LINE                                     
09609          MOVE SPACES             TO WS-D1-MESSAGE                 
09610          GO TO 9501-EXIT.                                         
09611                                                                   
09612      IF DM-RETURN-CODE = '11'                                     
09613          MOVE 'PAYMENT TYPE MISSING, DLB025:RC=11'                
09614                                  TO WS-D1-MESSAGE                 
09615          MOVE WS-DETAIL1         TO PRT                           
09616          PERFORM WRITE-A-LINE                                     
09617          MOVE SPACES             TO WS-D1-MESSAGE                 
09618          GO TO 9501-EXIT.                                         
09619                                                                   
09620      IF DM-RETURN-CODE = '12'                                     
09621          MOVE 'INVALID PAYMENT ACCOUNT, DLB025:RC=12'             
09622                                  TO WS-D1-MESSAGE                 
09623          MOVE WS-DETAIL1         TO PRT                           
09624          PERFORM WRITE-A-LINE                                     
09625          MOVE SPACES             TO WS-D1-MESSAGE                 
09626          GO TO 9501-EXIT.                                         
09627                                                                   
09628      IF DM-RETURN-CODE = '13'                                     
09629          MOVE 'INVALID PAYMENT DATE, DLB025:RC=13'                
09630                                  TO WS-D1-MESSAGE                 
09631          MOVE WS-DETAIL1         TO PRT                           
09632          PERFORM WRITE-A-LINE                                     
09633          MOVE SPACES             TO WS-D1-MESSAGE                 
09634          GO TO 9501-EXIT.                                         
09635                                                                   
09636      IF DM-RETURN-CODE = '14'                                     
09637          MOVE 'CERT NUMBER MISSING, DLB025:RC=14'                 
09638                                  TO WS-D1-MESSAGE                 
09639          MOVE WS-DETAIL1         TO PRT                           
09640          PERFORM WRITE-A-LINE                                     
09641          MOVE SPACES             TO WS-D1-MESSAGE                 
09642          GO TO 9501-EXIT.                                         
09643                                                                   
09644      IF DM-RETURN-CODE = '15'                                     
09645          MOVE 'INVALID TRAILER SEQUENCE #, DLB025:RC=15'          
09646                                  TO WS-D1-MESSAGE                 
09647          MOVE WS-DETAIL1         TO PRT                           
09648          PERFORM WRITE-A-LINE                                     
09649          MOVE SPACES             TO WS-D1-MESSAGE                 
09650          GO TO 9501-EXIT.                                         
09651                                                                   
09652      IF DM-RETURN-CODE = '16'                                     
09653          MOVE 'CARRIER MISSING, DLB025:RC=16'                     
09654                                  TO WS-D1-MESSAGE                 
09655          MOVE WS-DETAIL1         TO PRT                           
09656          PERFORM WRITE-A-LINE                                     
09657          MOVE SPACES             TO WS-D1-MESSAGE                 
09658          GO TO 9501-EXIT.                                         
09659                                                                   
09660      IF DM-RETURN-CODE = '17'                                     
09661          MOVE 'CARRIER MISSING, DLB025:RC=17'                     
09662                                  TO WS-D1-MESSAGE                 
09663          MOVE WS-DETAIL1         TO PRT                           
09664          PERFORM WRITE-A-LINE                                     
09665          MOVE SPACES             TO WS-D1-MESSAGE                 
09666          GO TO 9501-EXIT.                                         
09667                                                                   
09668      IF DM-RETURN-CODE = 'Z1'                                     
09669          MOVE 'INVALID PROCESS TYPE, DLB025:RC=Z1'                
09670                                  TO WS-D1-MESSAGE                 
09671          MOVE WS-DETAIL1         TO PRT                           
09672          PERFORM WRITE-A-LINE                                     
09673          MOVE SPACES             TO WS-D1-MESSAGE                 
09674          GO TO ABEND-PGM.                                         
09675                                                                   
09676      IF DM-RETURN-CODE = 'Z2' OR 'Z3'                             
09677          MOVE 'OPEN/CLOSE ERROR ON DMO FILE, DLB025:RC=Z2/Z3'     
09678                                  TO WS-D1-MESSAGE                 
09679          MOVE WS-DETAIL1         TO PRT                           
09680          PERFORM WRITE-A-LINE                                     
09681          MOVE SPACES             TO WS-D1-MESSAGE                 
09682          GO TO ABEND-PGM.                                         
09683                                                                   
09684      IF DM-RETURN-CODE = 'E1'                                     
09685          MOVE 'READ ERROR ON DMO FILE, DLB025:RC=E1'              
09686                                  TO WS-D1-MESSAGE                 
09687          MOVE WS-DETAIL1         TO PRT                           
09688          PERFORM WRITE-A-LINE                                     
09689          MOVE SPACES             TO WS-D1-MESSAGE                 
09690          GO TO ABEND-PGM.                                         
09691                                                                   
09692      IF DM-RETURN-CODE NOT = 'OK'                                 
09693          STRING 'UNKNOWN RETURN CODE, DLB025:RC='                 
09694              DELIMITED BY SIZE                                    
09695              DCT-RETURN-CODE DELIMITED BY SIZE                    
09696                                INTO WS-D1-MESSAGE                 
09697          MOVE WS-DETAIL1         TO PRT                           
09698          PERFORM WRITE-A-LINE                                     
09699          MOVE SPACES             TO WS-D1-MESSAGE                 
09700          GO TO ABEND-PGM.                                         
09701                                                                   
09702  9501-EXIT.                                                       
09703      EXIT.                                                        
09704                                                                   
09705  9501-OPEN-DLB6.                                                  
09706                                                                   
09707      MOVE SPACES                 TO DCT-COMMUNICATION-AREA.       
09708      MOVE 'O'                    TO DCT-PROCESS-TYPE.             
09709                                                                   
09710      CALL 'DLB006'  USING DCT-COMMUNICATION-AREA.                 
09711                                                                   
09712      IF  DCT-RETURN-CODE NOT = 'OK'                               
09713          MOVE 'DLB006'           TO  WS-FEM-FILE-NAME             
09714          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
09715          MOVE DCT-RETURN-CODE    TO  WS-ABEND-FILE-STATUS         
09716          GO TO ABEND-PGM.                                         
09717                                                                   
09718  9501-OPEN-DLB6-EXIT.                                             
09719      EXIT.                                                        
09720                                                                   
09721  9501-OPEN-DLB25.                                                 
09722                                                                   
09723      MOVE SPACES                 TO DMO-COMMUNICATION-AREA.       
09724      MOVE 'O'                    TO DM-PROCESS-TYPE.              
09725                                                                   
09726      CALL 'DLB025'  USING DMO-COMMUNICATION-AREA.                 
09727                                                                   
09728      IF  DM-RETURN-CODE NOT = 'OK'                                
09729          MOVE 'DLB025'           TO  WS-FEM-FILE-NAME             
09730          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
09731          MOVE DM-RETURN-CODE     TO  WS-ABEND-FILE-STATUS         
09732          GO TO ABEND-PGM.                                         
09733                                                                   
09734  9501-OPEN-DLB25-EXIT.                                            
09735      EXIT.                                                        
09736  EJECT                                                            
09737  9501-CLOSE-DLB6.                                                 
09738      MOVE SPACES                 TO DCT-COMMUNICATION-AREA.       
09739      MOVE 'C'                    TO DCT-PROCESS-TYPE.             
09740                                                                   
09741      CALL 'DLB006'  USING DCT-COMMUNICATION-AREA.                 
09742                                                                   
09743      IF  DCT-RETURN-CODE NOT = 'OK'                               
09744          MOVE 'DLB006'           TO  WS-FEM-FILE-NAME             
09745          MOVE 'ERROR OCCURRED CLOSING'                            
09746                                  TO  WS-ABEND-MESSAGE             
09747          MOVE DCT-RETURN-CODE    TO  WS-ABEND-FILE-STATUS         
09748          GO TO ABEND-PGM.                                         
09749                                                                   
09750  9501-CLOSE-DLB6-EXIT.                                            
09751      EXIT.                                                        
09752                                                                   
09753  9501-CLOSE-DLB25.                                                
09754      MOVE SPACES                 TO DMO-COMMUNICATION-AREA.       
09755      MOVE 'C'                    TO DM-PROCESS-TYPE.              
09756                                                                   
09757      CALL 'DLB025'  USING DMO-COMMUNICATION-AREA.                 
09758                                                                   
09759      IF  DM-RETURN-CODE NOT = 'OK'                                
09760          MOVE 'DLB025'           TO  WS-FEM-FILE-NAME             
09761          MOVE 'ERROR OCCURRED CLOSING'                            
09762                                  TO  WS-ABEND-MESSAGE             
09763          MOVE DM-RETURN-CODE     TO  WS-ABEND-FILE-STATUS         
09764          GO TO ABEND-PGM.                                         
09765                                                                   
09766  9501-CLOSE-DLB25-EXIT.                                           
09767      EXIT.                                                        
09768                                  EJECT                            
09769  9502-GET-DCT.                                                    
09770                                                                   
09771      IF NEED-TO-OPEN-DMD-FILES                                    
09772          MOVE 'Y'                TO WS-DMD-FILES-OPEN             
09773          PERFORM 9501-OPEN-DLB6  THRU 9501-OPEN-DLB6-EXIT         
09774          PERFORM 9501-OPEN-DLB25 THRU 9501-OPEN-DLB25-EXIT.       
09775                                                                   
09776 **** SET DEFAULT TO CASH.                                         
09777      MOVE 'Y'                    TO WS-AUTO-CASH.                 
09778                                                                   
09779      MOVE CL-CERT-CARRIER        TO CN-CARRIER.                   
09780      MOVE CL-CERT-GROUPING       TO CN-GROUPING.                  
09781      MOVE CL-CERT-STATE          TO CN-STATE.                     
09782      MOVE CL-CERT-ACCOUNT        TO CN-ACCOUNT.                   
09783      MOVE CL-CERT-EFF-DT         TO CN-CERT-EFF-DT.               
09784      MOVE CL-COMPANY-CD          TO CN-COMPANY-CD.                
09785      MOVE CL-CERT-NO             TO CN-CERT-NO.                   
09786                                                                   
09787      READ ERNOTE.                                                 
09788                                                                   
09789      IF ERNOTE-FILE-STATUS = '23'                                 
09790          GO TO 9502-NOTE-NOT-FOUND.                               
09791                                                                   
09792      IF ERNOTE-FILE-STATUS NOT = ZERO                             
09793          MOVE 'ERROR OCCURRED READ - ERNOTE '                     
09794                                TO WS-ABEND-MESSAGE                
09795          MOVE ERNOTE-FILE-STATUS TO WS-ABEND-FILE-STATUS          
09796          GO TO ABEND-PGM.                                         
09797                                                                   
09798      MOVE SPACES                 TO DCT-COMMUNICATION-AREA.       
09799      MOVE 'P'                    TO DCT-PROCESS-TYPE.             
09800      MOVE CL-BENEFICIARY         TO DCT-LOGIC-BENEFICIARY-ID.     
09801      MOVE CL-CCN                 TO DCT-CREDIT-CARD-NUMBER.       
09802                                                                   
09803      IF CL-CERT-GROUPING (5:2) NOT > SPACES                       
09804          MOVE 'CC'                   TO DCT-PRODUCT-CODE          
09805      ELSE                                                         
09806          MOVE CL-CERT-GROUPING (5:2) TO DCT-PRODUCT-CODE.         
09807                                                                   
09808      MOVE CN-CSI-CC-BILL-BANK-ID TO DCT-BILLING-BANK-ID.          
09809      MOVE '01'                   TO DCT-COLUMN-ID-REQUESTED.      
09810                                                                   
09811      CALL 'DLB006'  USING DCT-COMMUNICATION-AREA.                 
09812                                                                   
09813 ***** RECORD FOUND AND DISTRIBUTION CODE NOT EQUAL SPACES.        
09814 ***** IF THE CHECK IS NON-CASH OR 'TAPE', A NEW ALPHA CHECK       
09815 ***** NUMBER IS CREATED USING THE DISTRIBUTION CODE               
09816                                                                   
09817      IF DCT-RETURN-CODE = 'OK'                                    
09818          MOVE 'N'                   TO WS-AUTO-CASH               
09819          MOVE DCT-DISTRIBUTION-CODE TO W-DMD-CHECK-NO-1           
09820          MOVE CL-CERT-NO (4:1)      TO W-DMD-CHECK-NO-2           
09821          MOVE 'TP'                  TO W-DMD-CHECK-NO-3           
09822          MOVE W-DMD-CHECK-NO        TO AT-CHECK-NO                
09823                                        EX-DA-CHECK-NUMBER         
09824          GO TO 9502-EXIT.                                         
09825                                                                   
09826      IF DCT-RETURN-CODE = '01' OR '02'                            
09827          GO TO 9502-SET-TO-YES.                                   
09828                                                                   
09829      IF DCT-RETURN-CODE = '03'                                    
09830          MOVE 'INVALID BANK ID OR BENE ID - CASH/NON CASH UNKNOWN'
09831                                  TO WS-D1-MESSAGE                 
09832          MOVE WS-DETAIL1         TO PRT                           
09833          PERFORM WRITE-A-LINE                                     
09834          MOVE SPACES             TO WS-D1-MESSAGE                 
09835          GO TO 9502-EXIT.                                         
09836                                                                   
09837      IF DCT-RETURN-CODE = '04'                                    
09838          MOVE 'INVALID CREDIT CARD NUMBER -CASH/NON CASH UNKNOWN' 
09839                                  TO WS-D1-MESSAGE                 
09840          MOVE WS-DETAIL1         TO PRT                           
09841          PERFORM WRITE-A-LINE                                     
09842          MOVE SPACES             TO WS-D1-MESSAGE                 
09843          GO TO 9502-EXIT.                                         
09844                                                                   
09845      IF DCT-RETURN-CODE = '05'                                    
09846          MOVE 'INVALID PRODUCT CODE -CASH/NON CASH UNKNOWN'       
09847                                  TO WS-D1-MESSAGE                 
09848          MOVE WS-DETAIL1         TO PRT                           
09849          PERFORM WRITE-A-LINE                                     
09850          MOVE SPACES             TO WS-D1-MESSAGE                 
09851          GO TO 9502-EXIT.                                         
09852                                                                   
09853      IF DCT-RETURN-CODE = '06' OR '07'                            
09854          MOVE 'INVALID COLUMN ID - CASH/NON CASH UNKNOWN'         
09855                                  TO WS-D1-MESSAGE                 
09856          MOVE WS-DETAIL1         TO PRT                           
09857          PERFORM WRITE-A-LINE                                     
09858          MOVE SPACES             TO WS-D1-MESSAGE                 
09859          GO TO 9502-EXIT.                                         
09860                                                                   
09861      IF DCT-RETURN-CODE = '08'                                    
09862          MOVE 'CREDIT CARD PREFIX MISSING'                        
09863                                  TO WS-D1-MESSAGE                 
09864          MOVE WS-DETAIL1         TO PRT                           
09865          PERFORM WRITE-A-LINE                                     
09866          MOVE SPACES             TO WS-D1-MESSAGE                 
09867          GO TO 9502-EXIT.                                         
09868                                                                   
09869      IF DCT-RETURN-CODE = 'Z1'                                    
09870          MOVE 'INVALID PROCESS TYPE OR NOT FOUND'                 
09871                                  TO WS-D1-MESSAGE                 
09872          MOVE WS-DETAIL1         TO PRT                           
09873          PERFORM WRITE-A-LINE                                     
09874          MOVE SPACES             TO WS-D1-MESSAGE                 
09875          GO TO ABEND-PGM.                                         
09876                                                                   
09877      IF DCT-RETURN-CODE = 'Z2' OR 'Z3'                            
09878          MOVE 'ERROR OPENING/CLOSING DISTRIBUTION TABLES'         
09879                                  TO WS-D1-MESSAGE                 
09880          MOVE WS-DETAIL1         TO PRT                           
09881          PERFORM WRITE-A-LINE                                     
09882          MOVE SPACES             TO WS-D1-MESSAGE                 
09883          GO TO ABEND-PGM.                                         
09884                                                                   
09885      IF DCT-RETURN-CODE = 'E1' OR 'E2'                            
09886          MOVE 'ERROR READING DCT'                                 
09887                                  TO WS-D1-MESSAGE                 
09888          MOVE WS-DETAIL1         TO PRT                           
09889          PERFORM WRITE-A-LINE                                     
09890          MOVE SPACES             TO WS-D1-MESSAGE                 
09891          GO TO ABEND-PGM.                                         
09892                                                                   
09893      IF DCT-RETURN-CODE NOT = 'OK'                                
09894          MOVE 'UNKNOWN ERROR FROM DLB006'                         
09895                                  TO WS-D1-MESSAGE                 
09896          MOVE WS-DETAIL1         TO PRT                           
09897          PERFORM WRITE-A-LINE                                     
09898          MOVE SPACES             TO WS-D1-MESSAGE                 
09899          GO TO ABEND-PGM.                                         
09900                                                                   
09901      GO TO 9502-EXIT.                                             
09902                                                                   
09903  9502-NOTE-NOT-FOUND.                                             
09904                                                                   
09905      MOVE 'CERT NOTE NOT FOUND - '                                
09906                              TO WS-D1-MESSAGE.                    
09907      MOVE WS-DETAIL1         TO PRT.                              
09908      PERFORM WRITE-A-LINE.                                        
09909      MOVE SPACES             TO WS-D1-MESSAGE.                    
09910                                                                   
09911  9502-SET-TO-YES.                                                 
09912                                                                   
09913      MOVE 'Y'                TO AT-AUTO-CASH.                     
09914                                                                   
09915  9502-EXIT.                                                       
09916      EXIT.                                                        
09917                                                                   
09918 *LOG-JOURNAL-RECORD SECTION.     COPY ELCSLRP1.                   
09919                                  EJECT                            
09920  WRITE-A-LINE SECTION.           COPY ELCWAL.                     
09921                                  EJECT                            
09922  WRITE-HEADINGS SECTION.                                          
09923 *    NOTE ******************************************************* 
09924 *         *      THIS SECTION CONTROLS THE WRITING OF THE       * 
09925 *         *  HEADINGS.                                          * 
09926 *         *******************************************************.
09927                                                                   
09928  WHS-010.                                                         
09929                                                                   
09930      ADD +1                      TO  WS-PAGE.                     
09931      MOVE WS-PAGE                TO  WS-H2-PAGE.                  
09932                                                                   
09933      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        
09934                                                                   
09935      MOVE WS-HEADING1            TO  PRT.                         
09936      PERFORM WRITE-PRINTER.                                       
09937                                                                   
09938      MOVE WS-HEADING2            TO  PRT.                         
09939      PERFORM WRITE-PRINTER.                                       
09940                                                                   
09941      MOVE WS-HEADING3            TO  PRT.                         
09942      PERFORM WRITE-PRINTER.                                       
09943                                                                   
09944      MOVE WS-HEADING4            TO  PRT.                         
09945      PERFORM WRITE-PRINTER.                                       
09946                                                                   
09947      MOVE WS-HEADING5            TO  PRT.                         
09948      PERFORM WRITE-PRINTER.                                       
09949                                                                   
09950      MOVE +6                     TO  WS-LINE-COUNT.               
09951                                                                   
09952  WHS-020.                                                         
09953                                                                   
09954      MOVE WS-SAVE-PRINT-RECORD   TO  PRT.                         
09955      MOVE '-'                    TO  P-CTL.                       
09956                                                                   
09957  WHS-EXIT.                                                        
09958      EXIT.                                                        
09959                                                                   
09960                                  EJECT                            
09961  WRITE-PRINTER SECTION.                                           
09962                                                                   
09963                 COPY ELCWPS.                                      
09964                                                                   
09965      IF P-CTL = ' '                                               
09966         WRITE PRT AFTER ADVANCING 1 LINE                          
09967      ELSE                                                         
09968         IF P-CTL = '0'                                            
09969             WRITE PRT AFTER ADVANCING 2 LINE                      
09970         ELSE                                                      
09971             IF P-CTL = '-'                                        
09972                 WRITE PRT AFTER ADVANCING 3 LINE                  
09973             ELSE                                                  
09974                 WRITE PRT AFTER ADVANCING PAGE.                   
09975                                                                   
09976                                                                   
09977  WPS-EXIT.                                                        
09978      EXIT.                                                        
09979                                  EJECT                            
09980  OPEN-FILES SECTION.                                              
09981 *    NOTE ******************************************************* 
09982 *         *          THIS SECTION OPENS THE FILES.              * 
09983 *         *******************************************************.
09984                                                                   
09985  OFS-010.                                                         
09986                                                                   
09987      OPEN INPUT ERACCT                                            
09988                 ELPGMS                                            
09989                 ELBENE                                            
09990 *               ERNOTE
083002*               MPPROD                                            
083002*               MPPLAN                                            
09993           I-O   ELCERT                                            
09994                 ELCNTL                                            
09995                 ELMSTR                                            
09996                 ELTRLR                                            
09997                 ELCHKQ                                            
09998                 ELARCH                                            
09999                 ELACTQ                                            
                      ERPDEF
061013                ELCRTT
CIDMOD                DLYACTV                                           
10000           OUTPUT PRNTR                                            
10001                  ELAINP                                           
pemuni*                DISPLAY-PRT
10002                  JOURNAL-LOG-FILE.                                
10003                                                                   
10004      IF DTE-CLIENT = 'DMD'                                        
10005          OPEN OUTPUT ELSTAT-EXTRACT-FILE.                         
10006                                                                   
10007      MOVE LOW-VALUES             TO  WS-MPPLCY-AREA.              
10008      MOVE 'OO'                   TO  WS-EMPLCY-FUNCTION.          
10009      PERFORM 8350-CALL-EMPLCYX THRU 8350-EXIT.                    
10010                                                                   
10011      IF ELCNTL-FILE-STATUS  NOT = '00' AND '97'                   
10012          MOVE 'ELCNTL'           TO  WS-FEM-FILE-NAME             
10013          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
10014          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
10015          GO TO ABEND-PGM.                                         
10016                                                                   
10017      IF ELCERT-FILE-STATUS  NOT = '00' AND '97'                   
10018          MOVE 'ELCERT'           TO  WS-FEM-FILE-NAME             
10019          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
10020          MOVE ELCERT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
10021          GO TO ABEND-PGM.                                         
10022                                                                   
10023      IF ELMSTR-FILE-STATUS  NOT = '00' AND '97'                   
10024          MOVE 'ELMSTR'           TO  WS-FEM-FILE-NAME             
10025          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
10026          MOVE ELMSTR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
10027          GO TO ABEND-PGM.                                         
10028                                                                   
10029      IF ELTRLR-FILE-STATUS  NOT = '00' AND '97'                   
10030          MOVE 'ELTRLR'           TO  WS-FEM-FILE-NAME             
10031          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
10032          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
10033          GO TO ABEND-PGM.                                         
10034                                                                   
10035      IF ERACCT-FILE-STATUS  NOT = '00' AND '97'                   
10036          MOVE 'ERACCT'           TO  WS-FEM-FILE-NAME             
10037          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
10038          MOVE ERACCT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
10039          GO TO ABEND-PGM.                                         
10040                                                                   
10041      IF ELCHKQ-FILE-STATUS  NOT = '00' AND '97'                   
10042          MOVE 'ELCHKQ'           TO  WS-FEM-FILE-NAME             
10043          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
10044          MOVE ELCHKQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
10045          GO TO ABEND-PGM.                                         
10046                                                                   
10047      IF ELPGMS-FILE-STATUS  NOT = '00' AND '97'                   
10048          MOVE 'ELPGMS'           TO  WS-FEM-FILE-NAME             
10049          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
10050          MOVE ELPGMS-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
10051          GO TO ABEND-PGM.                                         
10052                                                                   
10053      IF ELARCH-FILE-STATUS  NOT = '00' AND '97'                   
10054          MOVE 'ELARCH'           TO  WS-FEM-FILE-NAME             
10055          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
10056          MOVE ELARCH-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
10057          GO TO ABEND-PGM.                                         
10058                                                                   
10059      IF ELACTQ-FILE-STATUS  NOT = '00' AND '97'                   
10060          MOVE 'ELACTQ'           TO  WS-FEM-FILE-NAME             
10061          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
10062          MOVE ELACTQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
10063          GO TO ABEND-PGM.                                         
10064                                                                   
           IF ERPDEF-FILE-STATUS  NOT = '00' AND '97'
              MOVE 'ERPDEF'            TO WS-FEM-FILE-NAME
              MOVE WS-FILE-ERROR-MESSAGE
                                       TO WS-ABEND-MESSAGE          
              MOVE ERPDEF-FILE-STATUS  TO WS-ABEND-FILE-STATUS         
              GO TO ABEND-PGM
           END-IF

10065      IF ELBENE-FILE-STATUS  NOT = '00' AND '97'                   
10066          MOVE 'ELBENE'           TO  WS-FEM-FILE-NAME             
10067          MOVE WS-FILE-ERROR-MESSAGE  TO  WS-ABEND-MESSAGE         
10068          MOVE ELBENE-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
10069          GO TO ABEND-PGM.                                         
10070                                                                   
10071      IF ERNOTE-FILE-STATUS  NOT = '00' AND '97'                   
10072          MOVE 'ERNOTE'           TO  WS-FEM-FILE-NAME             
10073          MOVE WS-FILE-ERROR-MESSAGE  TO  WS-ABEND-MESSAGE         
10074          MOVE ERNOTE-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
10075          GO TO ABEND-PGM.                                         

061013     IF ELCRTT-FILE-STATUS  NOT = '00' AND '97'                   
061013         MOVE 'ELCRTT'           TO  WS-FEM-FILE-NAME             
061013         MOVE WS-FILE-ERROR-MESSAGE  TO  WS-ABEND-MESSAGE         
061013         MOVE ELCRTT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
061013         GO TO ABEND-PGM.                                         

083002*    IF MPPROD-FILE-STATUS = '00' OR '97'                         
083002*        NEXT SENTENCE                                            
083002*    ELSE                                                         
083002*        MOVE 'MPPROD'           TO  WS-FEM-FILE-NAME             
083002*        MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
083002*        MOVE MPPROD-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
083002*        GO TO ABEND-PGM.                                         
10084                                                                   
083002*    IF MPPLAN-FILE-STATUS = '00' OR '97'                         
083002*        NEXT SENTENCE                                            
083002*    ELSE                                                         
083002*        MOVE 'MPPLAN'           TO  WS-FEM-FILE-NAME             
083002*        MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
083002*        MOVE MPPLAN-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
083002*        GO TO ABEND-PGM.                                         
10092                                                                   
083002*    IF WS-EMPLCY-RETURN-CODE NOT = '00' AND '97'                 
083002*        MOVE 'MPPLCY'           TO  WS-FEM-FILE-NAME             
083002*        MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
083002*        MOVE WS-EMPLCY-RETURN-CODE TO  WS-ABEND-FILE-STATUS      
083002*        GO TO ABEND-PGM.                                         
          .
10099  OFS-EXIT.                                                        
10100      EXIT.                                                        
10101                                  EJECT                            
10102  CLOSE-FILES SECTION.                                             
10103 *    NOTE ******************************************************* 
10104 *         *          THIS SECTION CLOSES THE FILES.             * 
10105 *         ******************************************************* 
10106                                                                   
10107  CFS-010.                                                         
10108                                                                   
CIDMOD         DISPLAY ' '.                                             
CIDMOD         DISPLAY '* * * * * * * * * * * * * * * * * *'.           
CIDMOD         DISPLAY ' DISPLAY ERROR COUNT = '  ERROR-COUNT.          
CIDMOD         DISPLAY '* * * * * * * * * * * * * * * * * *'.           
CIDMOD         DISPLAY ' '.                                             
CIDMOD                                                                  
CIDMOD         PERFORM 8600-DISPLAY-HD  THRU                            
CIDMOD               8600-HD-EXIT.                                      
CIDMOD                                                                  
CIDMOD         MOVE ' DISPLAY ERROR COUNT = '    TO DIS-LINE-REASON     
CIDMOD         MOVE ERROR-COUNT                TO  DIS-LINE-REC         
CIDMOD         PERFORM 8600-DISPLAY-PRT THRU                            
CIDMOD               8600-DISPLAY-EXIT.                                 
CIDMOD                                                                  
10109      CLOSE ELCNTL                                                 
10110            ERACCT                                                 
10111            ELPGMS                                                 
10112            ELCERT                                                 
10113            ELMSTR                                                 
10114            ELTRLR                                                 
10115            ELCHKQ                                                 
10116            ELARCH                                                 
10117            ELACTQ                                                 
                 ERPDEF
061013           ELCRTT
10118 *          ERNOTE                                                 
CIDMOD           DISPLAY-PRT
CIDMOD           DLYACTV
10119            PRNTR                                                  
10120            ELAINP                                                 
10121            ELBENE
083002*          MPPROD                                                 
083002*          MPPLAN                                                 
10124            JOURNAL-LOG-FILE.                                      
10125                                                                   
10126      IF DTE-CLIENT = 'DMD'                                        
10127          CLOSE ELSTAT-EXTRACT-FILE.                               
10128                                                                   
10129      MOVE LOW-VALUES             TO  WS-MPPLCY-AREA.              
10130      MOVE 'CC'                   TO  WS-EMPLCY-FUNCTION.          
10131      PERFORM 8350-CALL-EMPLCYX THRU 8350-EXIT.                    
10132                                                                   
10133      MOVE 'ERROR OCCURRED CLOSING-' TO  WS-FILE-ERROR-MESSAGE.    
10134                                                                   
10135      IF ERACCT-FILE-STATUS NOT = ZERO                             
10136          MOVE 'ERACCT  '         TO  WS-FEM-FILE-NAME             
10137          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
10138          MOVE ERACCT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
10139          GO TO ABEND-PGM.                                         
10140                                                                   
10141      IF ELACTQ-FILE-STATUS NOT = ZERO                             
10142          MOVE 'ELACTQ  '         TO  WS-FEM-FILE-NAME             
10143          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
10144          MOVE ELACTQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
10145          GO TO ABEND-PGM.                                         

061013     IF ELCRTT-FILE-STATUS NOT = ZERO                             
061013         MOVE 'ELCRTT  '         TO  WS-FEM-FILE-NAME             
061013         MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
061013         MOVE ELCRTT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
061013         GO TO ABEND-PGM.                                         

10147      IF ELCNTL-FILE-STATUS NOT = ZERO                             
10148          MOVE 'ELCNTL  '         TO  WS-FEM-FILE-NAME             
10149          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
10150          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
10151          GO TO ABEND-PGM.                                         
10152                                                                   
10153      IF ELCERT-FILE-STATUS NOT = ZERO                             
10154          MOVE 'ELCERT  '         TO  WS-FEM-FILE-NAME             
10155          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
10156          MOVE ELCERT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
10157          GO TO ABEND-PGM.                                         
10158                                                                   
10159      IF ELMSTR-FILE-STATUS NOT = ZERO                             
10160          MOVE 'ELMSTR  '         TO  WS-FEM-FILE-NAME             
10161          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
10162          MOVE ELMSTR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
10163          GO TO ABEND-PGM.                                         
10164                                                                   
10165      IF ELCHKQ-FILE-STATUS NOT = ZERO                             
10166          MOVE 'ELCHKQ  '         TO  WS-FEM-FILE-NAME             
10167          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
10168          MOVE ELCHKQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
10169          GO TO ABEND-PGM.                                         
10170                                                                   
10171      IF ELPGMS-FILE-STATUS NOT = ZERO                             
10172          MOVE 'ELPGMS  '         TO  WS-FEM-FILE-NAME             
10173          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
10174          MOVE ELPGMS-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
10175          GO TO ABEND-PGM.                                         
10176                                                                   
10177      IF ELARCH-FILE-STATUS NOT = ZERO                             
10178          MOVE 'ELARCH  '         TO  WS-FEM-FILE-NAME             
10179          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
10180          MOVE ELARCH-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
10181          GO TO ABEND-PGM.                                         
10182                                                                   
10183      IF ELTRLR-FILE-STATUS NOT = ZERO                             
10184          MOVE 'ELTRLR  '         TO  WS-FEM-FILE-NAME             
10185          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
10186          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
10187          GO TO ABEND-PGM.                                         
10188                                                                   
10189      IF ELBENE-FILE-STATUS NOT = ZERO                             
10190          MOVE 'ELBENE  '         TO  WS-FEM-FILE-NAME             
10191          MOVE WS-FILE-ERROR-MESSAGE  TO  WS-ABEND-MESSAGE         
10192          MOVE ELBENE-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
10193          GO TO ABEND-PGM.                                         
10194                                                                   
10195      IF ERNOTE-FILE-STATUS NOT = ZERO                             
10196          MOVE 'ERNOTE  '         TO  WS-FEM-FILE-NAME             
10197          MOVE WS-FILE-ERROR-MESSAGE  TO  WS-ABEND-MESSAGE         
10198          MOVE ERNOTE-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
10199          GO TO ABEND-PGM.                                         
10120
083002*    IF MPPROD-FILE-STATUS NOT = ZERO                             
083002*        MOVE 'MPPROD  '         TO  WS-FEM-FILE-NAME             
083002*        MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
083002*        MOVE MPPROD-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
083002*        GO TO ABEND-PGM.                                         
10206                                                                   
083002*    IF MPPLAN-FILE-STATUS NOT = ZERO                             
083002*        MOVE 'MPPLAN  '         TO  WS-FEM-FILE-NAME             
083002*        MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
083002*        MOVE MPPROD-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
083002*        GO TO ABEND-PGM.                                         
10212                                                                   
083002*    IF WS-EMPLCY-RETURN-CODE NOT = '00'                          
083002*        MOVE 'MPPLCY  '         TO  WS-FEM-FILE-NAME             
083002*        MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          
083002*        MOVE WS-EMPLCY-RETURN-CODE TO  WS-ABEND-FILE-STATUS      
083002*        GO TO ABEND-PGM.                                         
10218                                                                   
10219      IF DMD-FILES-OPEN                                            
10220          PERFORM 9501-CLOSE-DLB6  THRU 9501-CLOSE-DLB6-EXIT       
10221          PERFORM 9501-CLOSE-DLB25 THRU 9501-CLOSE-DLB25-EXIT.     
10222                                                                   
10223  CFS-EXIT.                                                        
10224      EXIT.                                                        
10225                                  EJECT                            
10226  ABEND-PGM SECTION.              COPY ELCABEND.                   
