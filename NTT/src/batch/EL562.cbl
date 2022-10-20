00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL562 .                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 08/15/94 08:50:13.                 
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            
00008 *                            VMOD=2.040.                          
00009                                                                   
00010 *AUTHOR.        LOGIC, INC.                                       
00011 *               DALLAS, TEXAS.                                    
00012                                                                   
00013 *DATE-COMPILED.                                                   
00014                                                                   
00015 *SECURITY.   *****************************************************
00016 *            *                                                   *
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00018 *            *                                                   *
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00022 *            *                                                   *
00023 *            *****************************************************
00024                                                                   
00025 *REMARKS.                                                         
00026 *        THIS PROGRAM PRINTS COMPENSATION STATEMENTS.             
00027 *                                                                 
00028 *    PROGRAM SWITCHES                                             
00029 *                                                                 
00030 *    1 - PRINT STATEMENTS WITH ACTIVITY OR BALANCE FORWARD        
00031 *        SEQUENCE - EFFECTIVE DATE WITHIN ACCOUNT                 
00032 *    2 - PRINT STATEMENTS WITH ACTIVITY ONLY                      
00033 *        SEQUENCE - EFFECTIVE DATE WITHIN ACCOUNT                 
00034 *    3 - PRINT NO COMPENSATION STATEMENTS                         
00035 *    4 - PRINT STATEMENTS WITH ACTIVITY OR BALANCE FORWARD        
00036 *        SEQUENCE - CERTIFICATE WITHIN ACCOUNT                    
00037 *    5 - PRINT STATEMENTS WITH ACTIVITY ONLY                      
00038 *        SEQUENCE - CERTIFICATE WITHIN ACCOUNT                    
00039 *    6 - PRINT STATEMENTS WITH ACTIVITY OR BALANCE FORWARD        
00040 *        SEQUENCE - NAME WITHIN ACCOUNT                           
00041 *    7 - PRINT STATEMENTS WITH ACTIVITY ONLY                      
00042 *        SEQUENCE - NAME WITHIN ACCOUNT                           
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PROCESSING        
060603* 060603                   PEMA  REMOVE CLAIM NAME FROM REPORT
030504* 030504 2004030300003     SMVA  MAKE SURE LIFE PREM PICKS UP FROM
030504*                                OVERWRITE TRANSACTIONS
120804* 120804                   PEMA  DONT INCLUDE RFNDS IN SPP BAL CLC
032905* 032905                   PEMA  MISC CHANGES
092705* 092705  CR2005050300006  PEMA  ADD SPP LEASES
091306* 091306  IR2006090500003  PEMA  SKIP TO NEW FORM ON TOTAL PAGE
082707* 082707    2007071200001  PEMA  ADD CHECK DATE TO PYAJ REC
090707* 090707  CR2006050800002  PEMA  ADD CODE FOR NEW AGENT TYPE
100307* 100307  CR2007080700001  PEMA  ADD CSR REPORTING CAPABILITY
020609* 020609                   PEMA  FIX DCC MONTHLY REFUNDS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
020310* 020310  CR2008100900004  PEMA  ADD REF4 EXTRACT PROCESSING
051810* 051810 CR2010042900001   PEMA  SPEPARATE CITY AND STATE
020211* 020211 CR2010051800001   PEMA  ADD CHECK DATE TO ECS063
020711* 020711  CR2010011400002  AJRA  PRINT BILLING NOTES ONE TIME
020811* 020811  CR2010011400002  AJRA  FIX ERROR MESSAGE PRINT
091911* 091911  IR2011090200003  PEMA  FIX LF PREM PROB WITH SPP
112711* 112711  IR2012010500001  PEMA  INCLUDE REF ON END BAL IF SPPDD
060712* 060712  IR2012060100002  PEMA  FIX BENCD SEARCH ON FATAL ERRS
082012* 082012  CR2012042700005  PEMA  ADD OVER 120 DAYS TO AGEING
070714* 070714  CR2013060600001  PEMA  AUTOMATE MONTH END BALANCING
092815* 092815  CR2015080300002  PEMA  ADD VIN TO ISSUES AND CANCELS
010416* 010416  CR2015082000001  PEMA  Check writer for endorsements
031416* 031416  CR2016030800002  PEMA  AH ONLY VIN PRINT PROBLEMS
062519* 062519  CR2019050800002  PEMA  CORRECT AGEING ISSUES
041320* 041320  CR2020030500002  PEMA  Issue, cancel billing notes
090821* 090821  IR2021083000001  PEMA  Increase size of chk amt
122002******************************************************************
00043  EJECT                                                            
00044  ENVIRONMENT DIVISION.                                            
00045  INPUT-OUTPUT SECTION.                                            
00046  FILE-CONTROL.                                                    
00047                                                                   
00048      SELECT SORT-WORK        ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.  
00049      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   
00050      SELECT COMM-MSTR-IN     ASSIGN TO SYS015-UT-FBA1-S-SYS015.   
00051      SELECT COMM-MSTR-OUT    ASSIGN TO SYS016-UT-FBA1-S-SYS016.   
00052      SELECT COMM-TRAN-IN     ASSIGN TO SYS017-UT-FBA1-S-SYS017.   
00053      SELECT SUMM-TRAN-OUT    ASSIGN TO SYS018-UT-FBA1-S-SYS018.   
00054      SELECT BILLING-DATA-FILE                                     
00055                              ASSIGN TO SYS010-UT-FBA1-S-SYS010.   
00056      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   
00057      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   
pemuni     SELECT ERPNDE           ASSIGN TO SYS022-FBA1-ERPNDE2
00059                              ORGANIZATION IS INDEXED              
00060                              ACCESS IS DYNAMIC                    
00061                              RECORD KEY IS PB-CONTROL-BY-ACCOUNT  
00062                              FILE STATUS IS ERPNDE-FILE-STATUS.   
00063      SELECT ERNOTE           ASSIGN TO SYS023-FBA1-ERNOTE         
00064                              ORGANIZATION IS INDEXED              
00065                              ACCESS IS DYNAMIC                    
00066                              RECORD KEY IS CN-CONTROL-PRIMARY     
00067                              FILE STATUS IS ERNOTE-FILE-STATUS.   
092815     SELECT ELCRTT           ASSIGN TO ELCRTT
092815                             ORGANIZATION IS INDEXED              
092815                             ACCESS IS DYNAMIC                    
092815                             RECORD KEY IS CS-CONTROL-PRIMARY     
092815                             FILE STATUS IS ELCRTT-FILE-STATUS.   
00068      SELECT ERCHEK           ASSIGN TO SYS024-FBA1-ERCHEK         
00069                              ORGANIZATION IS INDEXED              
00070                              ACCESS IS DYNAMIC                    
00071                              RECORD KEY IS CH-CONTROL-PRIMARY     
00072                              FILE STATUS IS ERCHEK-FILE-STATUS.   
00073      SELECT ELERRS           ASSIGN TO SYS027-FBA1-ELERRS         
00074                              ORGANIZATION IS INDEXED              
00075                              ACCESS IS DYNAMIC                    
00076                              RECORD KEY IS EM-CONTROL-PRIMARY     
00077                              FILE STATUS IS EM-STATUS.            
00078      SELECT ERACCT           ASSIGN TO SYS025-FBA1-ERACCT         
00079                              ORGANIZATION IS INDEXED              
00080                              ACCESS IS DYNAMIC                    
00081                              RECORD KEY IS AM-CONTROL-PRIMARY     
00082                              FILE STATUS IS ERACCT-FILE-STATUS.   
070714     SELECT  ERMEBL          ASSIGN SYS024-FBA1-ERMEBL      
070714                             ORGANIZATION INDEXED           
070714                             ACCESS DYNAMIC                 
070714                             RECORD KEY ME-CONTROL-PRIMARY  
070714                             FILE STATUS ERMEBL-FILE-STATUS.

00084  DATA DIVISION.                                                   
00085  FILE SECTION.                                                    
00086                                                                   
00087  SD  SORT-WORK.                                                   
00088  01  SORT-REC.                                                    
052504     12  SRT-KEY             PIC  X(69).                          
00090      12  SRT-CPTR            PIC  X(158).                         
00091                                                                   
00092  FD  PRNTR                                                        
00093      COPY ELCPRTFD.                                               
00094  EJECT                                                            
00095  FD  COMM-MSTR-IN                                                 
00096      COPY ECSCOIFD.                                               
00097  EJECT                                                            
00098  FD  COMM-MSTR-OUT                                                
00099      COPY ECSCOOFD.                                               
00100  EJECT                                                            
00101  FD  COMM-TRAN-IN                                                 
00102      COPY ECSCOMFD.                                               
00103                                                                   
00104  01  CP-RECORD-IN            PIC X(270).                          
00105  EJECT                                                            
00106  FD  SUMM-TRAN-OUT                                                
00107      BLOCK CONTAINS 0 RECORDS
00108      RECORDING MODE F.                                            
00109                                                                   
00110  01  CCM-WK.                                                      
00111      12  COMM-TRAN-WORK-REC.                                      
00112          16  CCW-ID          PIC  XX.                             
00113          16  CCW-CARR-GROUP  PIC  X(7).                           
00114          16  CCW-RESP-NO     PIC  X(10).                          
00115          16  CCW-ACCOUNT     PIC  X(10).                          
00116          16  CCW-AM-NO       PIC  X(10).                          
00117          16  CCW-TYPE        PIC  X.                              
00118              88  CCW-ACCTG                       VALUE '5'.       
00119              88  CCW-OVERWT                      VALUE '6'.       
00120              88  CCW-SUMMARY                     VALUE '7'.       
00121              88  CCW-PRE-BILLED                  VALUE '8'.       
00122          16  CCW-BAL-CTL     PIC  X.                              
00123          16  CCW-NAME        PIC  X(30).                          
00124          16  CCW-PREM        PIC S9(7)V99 COMP-3.                 
00125          16  CCW-COMM        PIC S9(7)V99 COMP-3.                 
00126          16  CCW-PMTS        PIC S9(7)V99 COMP-3.                 
00127          16  CCW-BEG-BAL     PIC S9(7)V99 COMP-3.                 
00128          16  CCW-END-BAL     PIC S9(7)V99 COMP-3.                 
00129          16  CCW-OV-L-PREM   PIC S9(7)V99 COMP-3.                 
00130          16  CCW-OV-A-PREM   PIC S9(7)V99 COMP-3.                 
00131          16  CCW-OV-LIFE     PIC S9(7)V99 COMP-3.                 
00132          16  CCW-OV-AH       PIC S9(7)V99 COMP-3.                 
011904         16  CCW-OV-AH-RFND  PIC S9(7)V99 COMP-3.
040504         16  CCW-DLR-INC     PIC S9(7)V99 COMP-3.
011410         16  CCW-LF-LMBA-FEE PIC S9(7)V99 COMP-3.
011410         16  CCW-AH-LMBA-FEE PIC S9(7)V99 COMP-3.
00133          16  CCW-OV-B-L-PREM PIC S9(7)V99 COMP-3.                 
00134          16  CCW-OV-B-A-PREM PIC S9(7)V99 COMP-3.                 
00135          16  CCW-OV-B-LIFE   PIC S9(7)V99 COMP-3.                 
00136          16  CCW-OV-B-AH     PIC S9(7)V99 COMP-3.
011410         16  CCW-OV-L-CNT    PIC S9(5)    COMP-3.
011410         16  CCW-OV-A-CNT    PIC S9(5)    COMP-3.

00138  FD  BILLING-DATA-FILE                                            
00139      BLOCK CONTAINS 0 RECORDS
00140      RECORDING MODE F.                                            
00141                                                                   
00142  01  BILLING-DATA-RECORD     PIC  X(284).                         

00144  FD  DISK-DATE                                                    
00145      COPY ELCDTEFD.                                               

00147  FD  FICH                                                         
00148      COPY ELCFCHFD.                                               

00150  FD  ERPNDE.                                                      
00151                                                                   
00152      COPY ERCPNDB.                                                

00154  FD  ERNOTE.                                                      
00155                                                                   
00156      COPY ERCNOTE.

092815 FD  ELCRTT.                                                      
092815                                                                  
092815                                 COPY ELCCRTT.

00158  FD  ERCHEK.                                                      
00159                                                                   
00160      COPY ERCCHEK.                                                

00162  FD  ELERRS.                                                      
00163                                                                   
00164      COPY ELCERRS.                                                
00165  EJECT                                                            
00166  FD  ERACCT.                                                      
00167                                                                   
00168      COPY ERCACCT.                                                
070714 FD  ERMEBL.
070714                                 COPY ERCMEBL.
00169  EJECT                                                            
00170  WORKING-STORAGE SECTION.                                         
00171  77  FILLER PIC X(32) VALUE '********************************'.   
00172  77  FILLER PIC X(32) VALUE '*  EL562 WORKING STORAGE       *'.   
00173  77  FILLER PIC X(32) VALUE '*********** VMOD=2.040 *********'.   
00174                                                                   
00175  77  STRT-LN                 PIC S99     COMP    VALUE +0.        
00176  77  END-LN                  PIC S99     COMP    VALUE +0.        
00177  77  SUB                     PIC S9(3)   COMP    VALUE +0.        
00177  77  FT-SUB                  PIC S9(3)   COMP    VALUE +0.        
00178  77  STMT-SW                 PIC S9      COMP-3  VALUE +0.        
00179  77  SUMM-SW                 PIC S9      COMP-3  VALUE +0.        
00180  77  PGM-SUB                 PIC S9(3)   COMP-3  VALUE +562.      
00181  77  LJ-NDX1                 PIC S9(3)   COMP-3  VALUE +0.        
00182  77  LJ-NDX2                 PIC S9(3)   COMP-3  VALUE +0.        
00183  77  SPACE-NP                PIC  X              VALUE '1'.       
00184  77  SPACE-1                 PIC  X              VALUE ' '.       
00185  77  SPACE-2                 PIC  X              VALUE '0'.       
00186  77  SPACE-3                 PIC  X              VALUE '-'.       
00187  77  X                       PIC  X              VALUE '1'.       
00188  77  CMI-END-OF-FILE-SW      PIC  X              VALUE 'N'.       
00189      88  CMI-END-OF-FILE                         VALUE 'Y'.       
00190  77  ACTIVITY-SWITCH         PIC  X              VALUE 'N'.       
00191      88  HAS-NO-ACTIVITY                         VALUE 'N'.       
00192  77  PREV-ACT-SWITCH         PIC  X              VALUE 'N'.       
00193      88  HAS-ACTIVITY                            VALUE 'Y'.       
00194  77  SUMMARY-PRT-SW          PIC  X              VALUE 'N'.       
00195      88  SUMMARY-PRT-NOT-NEEDED                  VALUE 'N'.       
00196      88  SUMMARY-PRT-NEEDED                      VALUE 'Y'.       
00197  77  PENDING-BUSINESS-SWITCH PIC  X              VALUE 'N'.       
00198      88  FROM-PENDING-BUSINESS                   VALUE 'Y'.       

       77  CISS-TOT-PRT-SW         PIC  X              VALUE 'N'.       
           88  CISS-TOT-NEEDED                         VALUE 'Y'.       
       77  CCAN-TOT-PRT-SW         PIC  X              VALUE 'N'.       
           88  CCAN-TOT-NEEDED                         VALUE 'Y'.       

00199  77  LIF-TOT-PRT-SW          PIC  X              VALUE 'N'.       
00200      88  LIF-TOT-NEEDED                          VALUE 'Y'.       
00201  77  AH-TOT-PRT-SW           PIC  X              VALUE 'N'.       
00202      88  AH-TOT-NEEDED                           VALUE 'Y'.       
00203  77  CLM-TOT-PRT-SW          PIC  X              VALUE 'N'.       
00204      88  CLM-TOT-NEEDED                          VALUE 'Y'.       
00205  77  HEADING-SWITCH          PIC  X              VALUE 'N'.       
00206      88  HEADING-NOT-PRINTED                     VALUE 'N'.       
00207  77  HEADING-SW              PIC  X              VALUE '0'.       
00208      88  ISSUE-HEADING                           VALUE '0'.       
00209      88  NOT-ISSUE-HEADING                       VALUE '1'.       
00210      88  SUMMARY-HEADING                         VALUE '2'.       
00211      88  PYADJ-HEADING                           VALUE '3'.       
00212  77  RECAP-PAGE-SWITCH       PIC  X              VALUE 'N'.       
00213      88  RECAP-PAGE                              VALUE 'Y'.       
00214  77  KSM-RECAP-PAGE-SWITCH       PIC  X          VALUE 'N'.       
00215      88  KSM-RECAP-PAGE                          VALUE 'Y'.       
00216  77  DUMMY-TOTALS-SWITCH     PIC  X              VALUE 'N'.       
00217      88  DUMMY-TOTALS                            VALUE 'Y'.       
00218  77  OB-SWITCH               PIC  X              VALUE 'N'.       
00219      88  OB-ON                                   VALUE 'Y'.       
00220  77  SRT-RECORD-SWITCH       PIC  X              VALUE 'N'.       
00221      88  DUMMY-RECORD-RELEASED                   VALUE 'Y'.       
00222  EJECT                                                            
052504 77  REC-013-FIRST-TIME-SW   PIC  X              VALUE 'Y'.
052504     88  REC-013-FIRST-TIME                      VALUE 'Y'.
052504 77  REC-017-FIRST-TIME-SW   PIC  X              VALUE 'Y'.
052504     88  REC-017-FIRST-TIME                      VALUE 'Y'.
00223  77  REC-02-FIRST-TIME-SW    PIC  X              VALUE 'Y'.       
00224      88  REC-02-FIRST-TIME                       VALUE 'Y'.       
00225  77  REC-03-FIRST-TIME-SW    PIC  X              VALUE 'Y'.       
00226      88  REC-03-FIRST-TIME                       VALUE 'Y'.       
00227  77  REC-04-FIRST-TIME-SW    PIC  X              VALUE 'Y'.       
00228      88  REC-04-FIRST-TIME                       VALUE 'Y'.       
00229  77  REC-05-FIRST-TIME-SW    PIC  X              VALUE 'Y'.       
00230      88  REC-05-FIRST-TIME                       VALUE 'Y'.       
00231  77  REC-06-FIRST-TIME-SW    PIC  X              VALUE 'Y'.       
00232      88  REC-06-FIRST-TIME                       VALUE 'Y'.       
00233  77  REC-07-FIRST-TIME-SW    PIC  X              VALUE 'Y'.       
00234      88  REC-07-FIRST-TIME                       VALUE 'Y'.       
00235  77  REC-08-FIRST-TIME-SW    PIC  X              VALUE 'Y'.       
00236      88  REC-08-FIRST-TIME                       VALUE 'Y'.       
00237  77  REC-09-FIRST-TIME-SW    PIC  X              VALUE 'Y'.       
00238      88  REC-09-FIRST-TIME                       VALUE 'Y'.       
00239  77  REC-10-FIRST-TIME-SW    PIC  X              VALUE 'Y'.       
00240      88  REC-10-FIRST-TIME                       VALUE 'Y'.       
00241  77  REC-11-FIRST-TIME-SW    PIC  X              VALUE 'Y'.       
00242      88  REC-11-FIRST-TIME                       VALUE 'Y'.       
00243  77  REC-13-FIRST-TIME-SW    PIC  X              VALUE 'Y'.       
00244      88  REC-13-FIRST-TIME                       VALUE 'Y'.       
00245  77  TRACE-SWITCH            PIC  X              VALUE 'N'.       
00246      88  TRACE-IS-ON                             VALUE 'Y'.       
00247  77  AH-ONLY-SWITCH          PIC  X              VALUE 'N'.       
00248      88  AH-ONLY                                 VALUE 'Y'.       
00249  77  LF-ONLY-SWITCH          PIC  X              VALUE 'N'.       
00250      88  LF-ONLY                                 VALUE 'Y'.       
00251  77  TIC-LINE-SWITCH         PIC  X              VALUE '0'.       
00252  77  TIC-TYPE-SWITCH         PIC  X              VALUE ' '.       
00253      88  TIC-ISSUE                               VALUE 'I'.       
00254      88  TIC-CANCEL                              VALUE 'C'.       
CIDMOD 77  WS-CONTROL-NAME         PIC X(30)   VALUE SPACES.
091306 77  WK1                     PIC S9(5)  COMP-3 VALUE +0.
091306 77  WK2                     PIC S9(5)  COMP-3 VALUE +0.
020210 77  WS-TOT-REF-COMM         PIC S9(7)V99 COMP-3 VALUE +0.
020210 77  WS-END-BAL              PIC S9(7)V99 COMP-3 VALUE +0.
020711 77  WS-CURR-BIN-DT          PIC XX      VALUE SPACES.
020711 77  WS-CURR-TIME            PIC S9(7)   VALUE ZEROS.
060712 77  ws-from-para-2310           pic x   value ' '.
060712     88  from-2310                  value 'y'.
090821 77  ws-formatted-amount     pic $$$,$$$.99 value zeros.
00255  EJECT                                                            
00256                                                                   
00257 ****                                                              
00258 **** KSM DATA TAPE WORK AREAS                                     
00259 ****                                                              
00260  01  KSM-TAPE-WORK-FIELDS.                                        
00261      12  SAV-KSM-DATE   COMP-3.                                   
00262          16  FILLER              PIC  999.                        
00263          16  SAV-KSM-CC          PIC  99.                         
00264          16  SAV-KSM-YY          PIC  99.                         
00265          16  SAV-KSM-MM          PIC  99.                         
00266          16  SAV-KSM-DD          PIC  99.                         
00267      12  WS-EFF-DATE             PIC  9(11)  COMP-3.              
00268      12  WS-EXP-DATE             PIC  9(11)  COMP-3.              
00269      12  SAV-KSM-CARR-GROUP.                                      
00270          16  SAV-KSM-CARRIER     PIC  X.                          
00271          16  SAV-KSM-GROUP       PIC  X(6).                       
00272      12  SAV-KSM-ACCOUNT         PIC  X(10).                      
00273      12  SAV-KSM-ACCT-NAME       PIC  X(30).                      
00274      12  SAV-KSM-ADDR-1          PIC  X(30).                      
00275      12  SAV-KSM-ADDR-2          PIC  X(30).                      
00276      12  SAV-KSM-CITY-STATE      PIC  X(30).                      
00277      12  SAV-KSM-ZIP             PIC  X(9).                       
00278      12  KSM-LO-CERT-DATE        PIC  9(11)    COMP-3.            
00279      12  KSM-HI-CERT-DATE        PIC  9(11)    COMP-3.            
00280      12  KSM-ACCT-STATUS         PIC  X.                          
00281      12  KSM-REI-TABLE           PIC  XXX.                        
00282      12  KSM-REPORT-CODE         PIC  X(10).                      
00283      12  KSM-ISS-LF-CNT          PIC  S9(7)    COMP-3.            
00284      12  KSM-ISS-AH-CNT          PIC  S9(7)    COMP-3.            
00285      12  KSM-CAN-LF-CNT          PIC  S9(7)    COMP-3.            
00286      12  KSM-CAN-AH-CNT          PIC  S9(7)    COMP-3.            
00287      12  KSM-CAN-PREM-91         PIC  S9(9)V99 COMP-3.            
00288                                                                   
00289  01  WS-BILLING-DETAIL-RECORD.                                    
00290      12  BDR-RECORD-TYPE         PIC X.                           
00291      12  BDR-CARR-GROUP.                                          
00292          16  BDR-CARRIER         PIC X.                           
00293          16  BDR-GROUPING        PIC X(6).                        
00294      12  BDR-ACCOUNT             PIC X(10).                       
00295      12  BDR-ACCT-NAME           PIC X(30).                       
00296      12  BDR-ADDR-1              PIC X(30).                       
00297      12  BDR-ADDR-2              PIC X(30).                       
00298      12  BDR-CITY-STATE          PIC X(30).                       
00299      12  BDR-ZIP                 PIC X(9).                        
00300      12  BDR-DATE  COMP-3.                                        
00301          16  FILLER              PIC 999.                         
00302          16  BDR-CC              PIC 99.                          
00303          16  BDR-YY              PIC 99.                          
00304          16  BDR-MM              PIC 99.                          
00305          16  BDR-DD              PIC 99.                          
00306      12  BDR-NAME.                                                
00307          16  BDR-LAST-NAME       PIC X(15).                       
00308          16  BDR-FIRST-NAME      PIC X(10).                       
00309          16  BDR-INITIAL         PIC X.                           
00310      12  BDR-CERT.                                                
00311          16  BDR-CERT-NO         PIC X(10).                       
00312          16  BDR-CERT-SFX        PIC X.                           
00313      12  BDR-EFFECT-DATE  COMP-3.                                 
00314          16  FILLER              PIC 999.                         
00315          16  BDR-EFF-CC          PIC 99.                          
00316          16  BDR-EFF-YY          PIC 99.                          
00317          16  BDR-EFF-MM          PIC 99.                          
00318          16  BDR-EFF-DD          PIC 99.                          
00319      12  BDR-AGE                 PIC 99.                          
00320      12  BDR-LIFE-BEN            PIC XX.                          
00321      12  BDR-LIFE-BEN-TYPE       PIC X(3).                        
00322      12  BDR-LIFE-TERM           PIC S999.                        
00323      12  BDR-FACE-AMOUNT         PIC S9(9)V99        COMP-3.      
00324      12  BDR-LIFE-PREMIUM        PIC S9(7)V99        COMP-3.      
00325      12  BDR-LIFE-COMMISSION     PIC S9(7)V99        COMP-3.      
00326      12  BDR-FACE-AMT-ALT        PIC S9(9)V99        COMP-3.      
00327      12  BDR-LIFE-PREM-ALT       PIC S9(7)V99        COMP-3.      
00328      12  BDR-LIFE-COMM-ALT       PIC S9(7)V99        COMP-3.      
00329      12  BDR-LF-CANCEL-DATE  COMP-3.                              
00330          16  FILLER              PIC 999.                         
00331          16  BDR-LF-CAN-CC       PIC 99.                          
00332          16  BDR-LF-CAN-YY       PIC 99.                          
00333          16  BDR-LF-CAN-MM       PIC 99.                          
00334          16  BDR-LF-CAN-DD       PIC 99.                          
00335      12  BDR-AH-BEN              PIC XX.                          
00336      12  BDR-AH-BEN-TYPE         PIC X(3).                        
00337      12  BDR-AH-TERM             PIC S999.                        
00338      12  BDR-AH-MO-BENEFIT       PIC S9(9)V99        COMP-3.      
00339      12  BDR-AH-PREMIUM          PIC S9(7)V99        COMP-3.      
00340      12  BDR-AH-COMMISSION       PIC S9(7)V99        COMP-3.      
00341      12  BDR-AH-CANCEL-DATE  COMP-3.                              
00342          16  FILLER              PIC 999.                         
00343          16  BDR-AH-CAN-CC       PIC 99.                          
00344          16  BDR-AH-CAN-YY       PIC 99.                          
00345          16  BDR-AH-CAN-MM       PIC 99.                          
00346          16  BDR-AH-CAN-DD       PIC 99.                          
00347      12  FILLER                  PIC X(10).                       
00348                                                                   
00349  01  WS-BILLING-SUMMARY-RECORD.                                   
00350      12  BSR-RECORD-TYPE         PIC X.                           
00351      12  BSR-CARR-GROUP.                                          
00352          16  BSR-CARRIER         PIC X.                           
00353          16  BSR-GROUPING        PIC X(6).                        
00354      12  BSR-ACCOUNT             PIC X(10).                       
00355      12  BSR-ACCT-NAME           PIC X(30).                       
00356      12  BSR-ADDR-1              PIC X(30).                       
00357      12  BSR-ADDR-2              PIC X(30).                       
00358      12  BSR-CITY-STATE          PIC X(30).                       
00359      12  BSR-ZIP                 PIC X(9).                        
00360      12  BSR-DATE  COMP-3.                                        
00361          16  FILLER              PIC 999.                         
00362          16  BSR-CC              PIC 99.                          
00363          16  BSR-YY              PIC 99.                          
00364          16  BSR-MM              PIC 99.                          
00365          16  BSR-DD              PIC 99.                          
00366      12  BSR-NET-LIFE-PREM       PIC S9(9)V99    COMP-3.          
00367      12  BSR-NET-AH-PREM         PIC S9(9)V99    COMP-3.          
00368      12  BSR-LIFE-COMM           PIC S9(9)V99    COMP-3.          
00369      12  BSR-AH-COMM             PIC S9(9)V99    COMP-3.          
00370      12  BSR-TOTAL-PREM          PIC S9(9)V99    COMP-3.          
00371      12  BSR-TOTAL-COMM          PIC S9(9)V99    COMP-3.          
00372      12  BSR-TOTAL-DUE           PIC S9(9)V99    COMP-3.          
00373      12  BSR-GROSS-LIFE-PREM     PIC S9(9)V99    COMP-3.          
00374      12  BSR-GROSS-AH-PREM       PIC S9(9)V99    COMP-3.          
00375      12  BSR-GROSS-LIFE-REFUNDS  PIC S9(9)V99    COMP-3.          
00376      12  BSR-GROSS-AH-REFUNDS    PIC S9(9)V99    COMP-3.          
00377      12  BSR-GROSS-WRITTEN       PIC S9(9)V99    COMP-3.          
00378      12  BSR-GROSS-REFUNDED      PIC S9(9)V99    COMP-3.          
00379      12  BSR-ISS-LF-CNT          PIC S9(7)       COMP-3.          
00380      12  BSR-ISS-AH-CNT          PIC S9(7)       COMP-3.          
00381      12  BSR-CAN-LF-CNT          PIC S9(7)       COMP-3.          
00382      12  BSR-CAN-AH-CNT          PIC S9(7)       COMP-3.          
00383      12  BSR-LO-CERT-DATE        PIC 9(11)       COMP-3.          
00384      12  BSR-HI-CERT-DATE        PIC 9(11)       COMP-3.          
00385      12  BSR-CAN-PREM-91         PIC S9(9)V99    COMP-3.          
00386      12  BSR-ACCT-STATUS         PIC X.                           
00387      12  BSR-REI-TABLE           PIC XXX.                         
00388      12  FILLER                  PIC X(5).                        
00389      12  BSR-REPORT-CODE         PIC X(10).                       
00390                                                                   
00391  01  KSM-TOTALS-AREA     COMP-3.                                  
00392      12  KSM-NET-LIFE-PREM       PIC S9(9)V99    VALUE ZEROS.     
00393      12  KSM-NET-AH-PREM         PIC S9(9)V99    VALUE ZEROS.     
00394      12  KSM-LIFE-COMM           PIC S9(9)V99    VALUE ZEROS.     
00395      12  KSM-AH-COMM             PIC S9(9)V99    VALUE ZEROS.     
00396      12  KSM-TOTAL-PREM          PIC S9(9)V99    VALUE ZEROS.     
00397      12  KSM-TOTAL-COMM          PIC S9(9)V99    VALUE ZEROS.     
00398      12  KSM-TOTAL-DUE           PIC S9(9)V99    VALUE ZEROS.     
00399      12  KSM-GROSS-LIFE-PREM     PIC S9(9)V99    VALUE ZEROS.     
00400      12  KSM-GROSS-AH-PREM       PIC S9(9)V99    VALUE ZEROS.     
00401      12  KSM-GROSS-LIFE-REFUNDS  PIC S9(9)V99    VALUE ZEROS.     
00402      12  KSM-GROSS-AH-REFUNDS    PIC S9(9)V99    VALUE ZEROS.     
00403      12  KSM-GROSS-WRITTEN       PIC S9(9)V99    VALUE ZEROS.     
00404      12  KSM-GROSS-REFUNDED      PIC S9(9)V99    VALUE ZEROS.     
00405      12  KSM-COUNT               PIC S9(7)       VALUE ZEROS.     
00406      12  KSM-DETAIL-COUNT        PIC S9(7)       VALUE ZEROS.     
00407  EJECT                                                            
070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
070714****                                                           ***
070714****   Month end balancing work area                           ***
070714****                                                           ***
070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***

070714 01  MONTH-END-DATA.                                          
070714     12  ME-START-DATE.                                       
070714         16  ME-START-MO     PIC  99.                         
070714         16  FILLER          PIC  X.                          
070714         16  ME-START-DA     PIC  99.                         
070714         16  FILLER          PIC  X.                          
070714         16  ME-START-YR     PIC  99.                         
070714     12  ME-CNDS-DATE        PIC  9(6).                       
070714     12  ME-CNDS-DATE-R  REDEFINES  ME-CNDS-DATE.             
070714         16  ME-CNDS-MO      PIC  99.                         
070714         16  ME-CNDS-DA      PIC  99.                         
070714         16  ME-CNDS-YR      PIC  99.                         
070714     12  ME-START-TIME       PIC  9(6).                       
070714     12  ME-UPDATE-FLAG      PIC  X          VALUE 'Y'.       
070714         88  ME-DO-UPDATE                    VALUE 'Y'.       
070714         88  ME-NO-UPDATE                    VALUE 'N'.       
070714     12  ERMEBL-FILE-STATUS  PIC  XX.                         
070714     12  MONTH-END-MOYR      PIC  9(4)                 COMP.  
070714     12  HLD-562-PREM-TOT         PIC S9(9)V99  COMP-3 VALUE +0.
070714     12  HLD-562-COMM-TOT         PIC S9(9)V99  COMP-3 VALUE +0.

00408  01  FILLER                      PIC X(24)       VALUE            
00409              '*** SORT WORK RECORD ***'.                          
00410  01  WS-SORT-REC.                                                 
00411      12  WS-SRT-KEY.                                              
00412          16  WS-SRT-KEY-A.                                        
00413              20  WS-SRT-CARR         PIC  X.                      
00414              20  WS-SRT-GROUP        PIC  X(6).                   
00415              20  WS-SRT-REMIT        PIC  X(10).                  
00416              20  WS-SRT-ACCT         PIC  X(10).                  
052504         16  WS-SRT-REC-TYPE         PIC  XXX.
pemuni             88  WS-HAS-ACTIVITY                 VALUE '%% '.
pemuni             88  WS-HAS-NO-ACTIVITY              VALUE '&& '.
00420              88  WS-COMP-REC                     VALUE '00 '.      
00421              88  WS-REMIT-REC                    VALUE '01 '.      
052504             88  WS-CASH-ISSUE                   VALUE '013'.
052504             88  WS-CASH-CANCEL                  VALUE '017'.
00422              88  WS-ISSUE                        VALUE '02 '.      
00423              88  WS-CANCEL                       VALUE '03 '.      
00424              88  WS-ISSUE-ERROR                  VALUE '04 '.      
00425              88  WS-ISSUE-HOLD                   VALUE '05 '.      
00426              88  WS-ISSUE-RETURN                 VALUE '06 '.      
00427              88  WS-ISSUE-VOIDED                 VALUE '07 '.      
00428              88  WS-CANCEL-ERROR                 VALUE '08 '.      
00429              88  WS-CANCEL-HOLD                  VALUE '09 '.      
00430              88  WS-CANCEL-RETURN                VALUE '10 '.      
00431              88  WS-PYMTS-ADJUST                 VALUE '11 '.      
00432              88  WS-SUMMARY                      VALUE '12 '.      
00433              88  WS-CLAIMS                       VALUE '13 '.      
00434              88  WS-DUMMY-RECORD                 VALUE '99 '.      
00435          16  WS-SRT-LOW.                                          
00436              20  WS-SRT-LNAME        PIC  X(15).                  
00437              20  WS-SRT-FNAME        PIC  X(10).                  
00438              20  WS-SRT-INITS        PIC  X.                      
00439          16  WS-SRT-LOW-DATE  REDEFINES WS-SRT-LOW.               
00440              20  FILLER              PIC  X(03).                  
00441              20  WS-SRT-LOW-DT       PIC  9(08).                  
00442              20  FILLER              PIC  X(15).                  
00443          16  WS-SRT-CERT             PIC  X(11).                  
00444          16  WS-SRT-REC-SFX          PIC  99.                     
00445      12  WS-SRT00-REC.                                            
00446          16  WS-SRT00-DATA           PIC  X(30).                  
00447          16  FILLER                  PIC  X(127).                 
00448      12  WS-SRT01-REC  REDEFINES  WS-SRT00-REC.                   
00449          16  WS-SRT01-CARR-GROUP     PIC  X(7).                   
00450          16  WS-SRT01-ACCT           PIC  X(10).                  
00451          16  WS-SRT01-NAME           PIC  X(30).                  
00452          16  WS-SRT01-RESP           PIC  X(10).                  
00453          16  WS-SRT01-REMIT          PIC  X(30).                  
CIDMOD*        16  FILLER                  PIC  X(70).                  
CIDMOD         16  WS-SRT01-CONTROL-NAME   PIC  X(30).
100307         16  WS-SRT01-CSR            PIC  X(4).
011410         16  WS-SRT01-SPPDD          PIC  X.
011410         16  FILLER                  PIC  X(35).                  
00455  EJECT                                                            
00456      12  WS-SRT013-REC  REDEFINES  WS-SRT00-REC.                   
00457          16  WS-SRT013-NAME.                                       
00458              20  WS-SRT013-LNAME      PIC  X(15).                  
00459              20  WS-SRT013-FNAME      PIC  X(10).                  
00460              20  WS-SRT013-INTL       PIC  X.                      
00461          16  WS-SRT013-CERT           PIC  X(11).                  
00462          16  WS-SRT013-EFF-DT         PIC  9(11)         COMP-3.   
00463          16  WS-SRT013-AGE            PIC  99.                     
00464          16  WS-SRT013-LF-TERM        PIC S9(3).                   
00465          16  WS-SRT013-LF-TYP         PIC  X(3).                   
00466          16  WS-SRT013-OB-ON          PIC  X.                      
00467          16  WS-SRT013-FACE           PIC S9(9)V99        COMP-3.  
00468          16  WS-SRT013-FACE-ALT       PIC S9(9)V99        COMP-3.  
00469          16  WS-SRT013-LF-PREM        PIC S9(7)V99        COMP-3.  
00470          16  WS-SRT013-LF-PREM-ALT    PIC S9(7)V99        COMP-3.  
00471          16  WS-SRT013-LF-PRM-PR      PIC S9(7)V99        COMP-3.  
00472          16  WS-SRT013-LF-PRM-PR-ALT  PIC S9(7)V99        COMP-3.  
00473          16  WS-SRT013-AH-TERM        PIC S9(3).                   
00474          16  WS-SRT013-AH-TYP         PIC  X(3).                   
00475          16  WS-SRT013-AH-BEN         PIC S9(7)V99        COMP-3.  
00476          16  WS-SRT013-AH-PREM        PIC S9(7)V99        COMP-3.  
00477          16  WS-SRT013-AH-PRM-PR      PIC S9(7)V99        COMP-3.  
00478          16  WS-SRT013-LF-COMM        PIC S9(7)V99        COMP-3.  
00479          16  WS-SRT013-LF-COMM-ALT    PIC S9(7)V99        COMP-3.  
00480          16  WS-SRT013-LF-COM-PCT     PIC SV9(5)          COMP-3.  
00481          16  WS-SRT013-AH-COMM        PIC S9(7)V99        COMP-3.  
00482          16  WS-SRT013-AH-COM-PCT     PIC SV9(5)          COMP-3.  
00483          16  WS-SRT013-RECALC-FLAG    PIC  X(01).                  
00484          16  WS-SRT013-PREM-OVERRIDE  PIC  X(01).                  
00485          16  WS-SRT013-RPTCDE         PIC  X(10).                  
011410         16  WS-SRT013-AH-BEN-CAT     PIC  X.
011410         16  WS-SRT013-LF-BEN-CAT     PIC  X.
011410         16  FILLER                  PIC  X.
00487          16  WS-SRT013-ACCT-STATUS    PIC  X.                      
00488          16  WS-SRT013-REI-TABLE      PIC  X(3).                   
00489          16  WS-SRT013-MEMBER-NO      PIC  X(12).                  
00491      12  WS-SRT017-REC  REDEFINES  WS-SRT00-REC.                   
00492          16  WS-SRT017-NAME.                                       
00493              20  WS-SRT017-LNAME      PIC  X(15).                  
00494              20  WS-SRT017-FNAME.                                  
00495                  24  WS-SRT017-FI     PIC  X.                      
00496                  24  FILLER          PIC  X(9).                   
00497              20  WS-SRT017-INTL       PIC  X.                      
00498          16  WS-SRT017-CERT           PIC  X(11).                  
00499          16  WS-SRT017-EFF-DT         PIC  9(11)          COMP-3.  
00500          16  WS-SRT017-LF-CAN-DT      PIC  9(11)          COMP-3.  
00501          16  WS-SRT017-LF-REF         PIC S9(7)V99        COMP-3.  
00502          16  WS-SRT017-LF-REF-REV     PIC S9(7)V99        COMP-3.  
00503          16  WS-SRT017-AH-CAN-DT      PIC 9(11)           COMP-3.  
00504          16  WS-SRT017-AH-REF         PIC S9(7)V99        COMP-3.  
00505          16  WS-SRT017-AH-REF-REV     PIC S9(7)V99        COMP-3.  
00506          16  WS-SRT017-TOT-REF        PIC S9(7)V99        COMP-3.  
00507          16  WS-SRT017-LF-COM-REF     PIC S9(7)V99        COMP-3.  
00508          16  WS-SRT017-AH-COM-REF     PIC S9(7)V99        COMP-3.  
00509          16  WS-SRT017-COM-TOTREF     PIC S9(7)V99        COMP-3.  
00510          16  WS-SRT017-RECALC-FLAG    PIC  X(01).                  
00511          16  WS-SRT017-REF-OVERRIDE   PIC  X(01).                  
00512          16  WS-SRT017-LF-COM-PCT     PIC  SV9(5)         COMP-3.  
00513          16  WS-SRT017-AH-COM-PCT     PIC  SV9(5)         COMP-3.  
00514          16  WS-SRT017-LF-TYP         PIC  XXX.                    
00515          16  WS-SRT017-AH-TYP         PIC  XXX.                    
00516          16  WS-SRT017-LF-TERM        PIC  S9(3)          COMP-3.  
00517          16  WS-SRT017-AH-TERM        PIC  S9(3)          COMP-3.  
00518          16  WS-SRT017-AGE            PIC  XX.                     
00519          16  WS-SRT017-LF-FACE        PIC  S9(7)V99       COMP-3.  
00520          16  WS-SRT017-AH-MO-BEN      PIC  S9(7)V99       COMP-3.  
011410         16  WS-SRT017-AH-BEN-CAT     PIC  X.
011410         16  FILLER                   PIC X.
011410         16  WS-SRT017-LF-BEN-CAT     PIC X.
011410         16  FILLER                  PIC  X(13).
00522          16  WS-SRT017-ACCT-STATUS    PIC  X.                      
00523          16  WS-SRT017-REI-TABLE      PIC  X(3).                   
00524          16  WS-SRT017-MEMBER-NO      PIC  X(12).                  
00456      12  WS-SRT02-REC  REDEFINES  WS-SRT00-REC.                   
00457          16  WS-SRT02-NAME.                                       
00458              20  WS-SRT02-LNAME      PIC  X(15).                  
00459              20  WS-SRT02-FNAME      PIC  X(10).                  
00460              20  WS-SRT02-INTL       PIC  X.                      
00461          16  WS-SRT02-CERT           PIC  X(11).                  
00462          16  WS-SRT02-EFF-DT         PIC  9(11)         COMP-3.   
00463          16  WS-SRT02-AGE            PIC  99.                     
00464          16  WS-SRT02-LF-TERM        PIC S9(3).                   
00465          16  WS-SRT02-LF-TYP         PIC  X(3).                   
00466          16  WS-SRT02-OB-ON          PIC  X.                      
00467          16  WS-SRT02-FACE           PIC S9(9)V99        COMP-3.  
00468          16  WS-SRT02-FACE-ALT       PIC S9(9)V99        COMP-3.  
00469          16  WS-SRT02-LF-PREM        PIC S9(7)V99        COMP-3.  
00470          16  WS-SRT02-LF-PREM-ALT    PIC S9(7)V99        COMP-3.  
00471          16  WS-SRT02-LF-PRM-PR      PIC S9(7)V99        COMP-3.  
00472          16  WS-SRT02-LF-PRM-PR-ALT  PIC S9(7)V99        COMP-3.  
00473          16  WS-SRT02-AH-TERM        PIC S9(3).                   
00474          16  WS-SRT02-AH-TYP         PIC  X(3).                   
00475          16  WS-SRT02-AH-BEN         PIC S9(7)V99        COMP-3.  
00476          16  WS-SRT02-AH-PREM        PIC S9(7)V99        COMP-3.  
00477          16  WS-SRT02-AH-PRM-PR      PIC S9(7)V99        COMP-3.  
00478          16  WS-SRT02-LF-COMM        PIC S9(7)V99        COMP-3.  
00479          16  WS-SRT02-LF-COMM-ALT    PIC S9(7)V99        COMP-3.  
00480          16  WS-SRT02-LF-COM-PCT     PIC SV9(5)          COMP-3.  
00481          16  WS-SRT02-AH-COMM        PIC S9(7)V99        COMP-3.  
00482          16  WS-SRT02-AH-COM-PCT     PIC SV9(5)          COMP-3.  
00483          16  WS-SRT02-RECALC-FLAG    PIC  X(01).                  
00484          16  WS-SRT02-PREM-OVERRIDE  PIC  X(01).                  
011410         16  WS-SRT02-LF-BEN-CAT     PIC  X.
011410         16  WS-SRT02-AH-BEN-CAT     PIC  X.
092815         16  ws-srt02-vin            pic  x(17).
092815         16  filler                  pic  x(10).
092815*        16  WS-SRT02-RPTCDE         PIC  X(10).                  
092815*        16  FILLER                  PIC  X.
092815*        16  WS-SRT02-ACCT-STATUS    PIC  X.                      
092815*        16  WS-SRT02-REI-TABLE      PIC  X(3).                   
092815*        16  WS-SRT02-MEMBER-NO      PIC  X(12).                  
00490  EJECT                                                            
00491      12  WS-SRT03-REC  REDEFINES  WS-SRT00-REC.                   
00492          16  WS-SRT03-NAME.                                       
00493              20  WS-SRT03-LNAME      PIC  X(15).                  
00494              20  WS-SRT03-FNAME.                                  
00495                  24  WS-SRT03-FI     PIC  X.                      
00496                  24  FILLER          PIC  X(9).                   
00497              20  WS-SRT03-INTL       PIC  X.                      
00498          16  WS-SRT03-CERT           PIC  X(11).                  
00499          16  WS-SRT03-EFF-DT         PIC  9(11)          COMP-3.  
00500          16  WS-SRT03-LF-CAN-DT      PIC  9(11)          COMP-3.  
00501          16  WS-SRT03-LF-REF         PIC S9(7)V99        COMP-3.  
00502          16  WS-SRT03-LF-REF-REV     PIC S9(7)V99        COMP-3.  
00503          16  WS-SRT03-AH-CAN-DT      PIC 9(11)           COMP-3.  
00504          16  WS-SRT03-AH-REF         PIC S9(7)V99        COMP-3.  
00505          16  WS-SRT03-AH-REF-REV     PIC S9(7)V99        COMP-3.  
00506          16  WS-SRT03-TOT-REF        PIC S9(7)V99        COMP-3.  
00507          16  WS-SRT03-LF-COM-REF     PIC S9(7)V99        COMP-3.  
00508          16  WS-SRT03-AH-COM-REF     PIC S9(7)V99        COMP-3.  
00509          16  WS-SRT03-COM-TOTREF     PIC S9(7)V99        COMP-3.  
00510          16  WS-SRT03-RECALC-FLAG    PIC  X(01).                  
00511          16  WS-SRT03-REF-OVERRIDE   PIC  X(01).                  
00512          16  WS-SRT03-LF-COM-PCT     PIC  SV9(5)         COMP-3.  
00513          16  WS-SRT03-AH-COM-PCT     PIC  SV9(5)         COMP-3.  
00514          16  WS-SRT03-LF-TYP         PIC  XXX.                    
00515          16  WS-SRT03-AH-TYP         PIC  XXX.                    
00516          16  WS-SRT03-LF-TERM        PIC  S9(3)          COMP-3.  
00517          16  WS-SRT03-AH-TERM        PIC  S9(3)          COMP-3.  
00518          16  WS-SRT03-AGE            PIC  XX.                     
00519          16  WS-SRT03-LF-FACE        PIC  S9(7)V99       COMP-3.  
00520          16  WS-SRT03-AH-MO-BEN      PIC  S9(7)V99       COMP-3.  
011410         16  WS-SRT03-AH-BEN-CAT     PIC  X.
020609         16  WS-SRT03-OB-ON          PIC  X.
011410         16  WS-SRT03-LF-BEN-CAT     PIC  X.
092815         16  ws-srt03-vin            pic  x(17).
092815         16  FILLER                  PIC  X(12).
092815*        16  WS-SRT03-ACCT-STATUS    PIC  X.                      
092815*        16  WS-SRT03-REI-TABLE      PIC  X(3).                   
092815*        16  WS-SRT03-MEMBER-NO      PIC  X(12).                  
00525  EJECT                                                            
00526      12  WS-SRT03A-REC  REDEFINES  WS-SRT00-REC.                  
00527          16  WS-SRT03A-PAYEE         PIC  X(30).                  
00528          16  WS-SRT03A-CHEK-DT       PIC  9(11)   COMP-3.         
00529          16  WS-SRT03A-CHEK-NUM      PIC  X(7).
010616         16  ws-srt03a-check-amt     pic s9(7)v99 comp-3.
010616         16  FILLER                  PIC  X(109).                 
00531      12  WS-SRT04-REC  REDEFINES  WS-SRT00-REC.                   
00532          16  WS-SRT04-ERR-SEV        PIC  X.                      
00533          16  WS-SRT04-ERR-TXT        PIC  X(65).                  
00534          16  FILLER                  PIC  X(91).                  
00535      12  WS-SRT05-REC  REDEFINES  WS-SRT00-REC.                   
00536          16  FILLER                  PIC  X(157).                 
00537      12  WS-SRT06-REC  REDEFINES  WS-SRT00-REC.                   
00538          16  FILLER                  PIC  X(157).                 
00539  EJECT                                                            
00540      12  WS-SRT07-REC  REDEFINES  WS-SRT00-REC.                   
00541          16  FILLER                  PIC  X(157).                 
00542      12  WS-SRT08-REC  REDEFINES  WS-SRT00-REC.                   
00543          16  FILLER                  PIC  X(157).                 
00544      12  WS-SRT09-REC  REDEFINES  WS-SRT00-REC.                   
00545          16  FILLER                  PIC  X(157).                 
00546      12  WS-SRT10-REC  REDEFINES  WS-SRT00-REC.                   
00547          16  WS-SRT10-DESC           PIC  X(30).                  
00548          16  WS-SRT10-PYMT           PIC S9(7)V99        COMP-3.  
00549          16  WS-SRT10-CHRG           PIC S9(7)V99        COMP-3.
082707         16  WS-SRT10-MAINT-DATE     PIC 9(6).
082707         16  FILLER                  PIC  X(111).                 
00551      12  WS-SRT11-REC  REDEFINES  WS-SRT00-REC.                   
00552          16  WS-SRT11-BEG-BAL        PIC S9(7)V99        COMP-3.  
00553          16  FILLER                  PIC  X(152).                 
00554      12  WS-SRT12-REC  REDEFINES  WS-SRT00-REC.                   
00555          16  WS-SRT12-NAME.                                       
00556              20  WS-SRT12-LNAME      PIC  X(15).                  
00557              20  WS-SRT12-FNAME      PIC  X(10).                  
00558              20  WS-SRT12-INTL       PIC  X.                      
00559          16  WS-SRT12-CERT           PIC  X(11).                  
00560          16  WS-SRT12-EFF-DT         PIC  9(11)          COMP-3.  
00561          16  WS-SRT12-LF-CLM         PIC S9(7)V99        COMP-3.  
00562          16  WS-SRT12-AH-CLM         PIC S9(7)V99        COMP-3.  
00563          16  FILLER                  PIC  X(92).                  
00564          16  WS-SRT12-MEMBER-NO      PIC  X(12).                  
00565      12  WS-SRT-SUPP-REC-TYP         PIC  X.                      
00566          88  WS-SUPP-CHECK                       VALUE 'C'.       
00567          88  WS-SUPP-ERROR                       VALUE 'E'.       
00568          88  WS-SUPP-NOTE                        VALUE 'N'.       
00569  EJECT                                                            
00570  01  MISC-WORKING-STORAGE.                                        
011410     12  PRV-SPP-CATEGORY           PIC X VALUE ' '.
00571      12  PRV-SRT-KEY.                                             
00572          16  PRV-SRT-KEY-A.                                       
00573              20  PRV-SRT-CARR-GROUP.                              
00574                  24  PRV-SRT-CARR    PIC  X.                      
00575                  24  PRV-SRT-GROUP   PIC  X(6).                   
00576              20  PRV-SRT-ACCT        PIC  X(10).                  
00577              20  PRV-SRT-REMIT       PIC  X(10).                  
00578          16  PRV-SRT-REC-TYPE        PIC  XX.                     
00579      12  HLD-SRT-KEY.                                             
00580          16  HLD-SRT-KEY-A.                                       
00581              20  HLD-SRT-CARR-GROUP.                              
00582                  24  HLD-SRT-CARR    PIC  X.                      
00583                  24  HLD-SRT-GROUP   PIC  X(6).                   
00584              20  HLD-SRT-ACCT        PIC  X(10).                  
00585              20  HLD-SRT-REMIT       PIC  X(10).                  
00586          16  HLD-SRT-REC-TYPE        PIC  XX.                     
00587      12  ABEND-CODE                  PIC  X(4)   VALUE ZERO.      
00588      12  WS-RETURN-CODE              PIC S9(3)   VALUE ZERO.      
00589      12  WS-ZERO                     PIC S9      VALUE ZERO.      
00590      12  WS-RUN-DT                   PIC  XX.                     
00591      12  WS-ABEND-MESSAGE            PIC  X(80)  VALUE SPACES.    
00592      12  WS-ABEND-FILE-STATUS        PIC  XX     VALUE ZERO.      
00593      12  ERPNDE-FILE-STATUS          PIC  XX     VALUE ZERO.      
00594      12  ERNOTE-FILE-STATUS          PIC  XX     VALUE ZERO.      
092815     12  ELCRTT-FILE-STATUS          PIC  XX     VALUE ZERO.
00595      12  ERCHEK-FILE-STATUS          PIC  XX     VALUE ZERO.      
00596      12  ERACCT-FILE-STATUS          PIC  XX     VALUE ZERO.      
00597      12  EM-STATUS.                                               
00598          16  EM-STAT-1               PIC  X.                      
00599          16  EM-STAT-2               PIC  X.                      
100307     12  SAV-CSR                     PIC  X(4).
011410     12  SAV-SPPDD                   PIC  X.
00600      12  SAV-CARR-GROUP.                                          
00601          16  SAV-CARR                PIC  X.                      
00602          16  SAV-GROUP               PIC  X(6).                   
00603      12  WS-ERR-CODE-X.                                           
00604          16  FILLER                  PIC  99.                     
00605          16  WS-ERROR-SUB            PIC  99.                     
00606      12  WS-ERR-CODE REDEFINES WS-ERR-CODE-X                      
00607                                      PIC  9(4).                   
00608      12  LIT-2600                    PIC  X(4)   VALUE '2600'.    
00609      12  LIT-2625                    PIC  X(4)   VALUE '2625'.    
00610      12  LIT-2725                    PIC  X(4)   VALUE '2725'.    
00611      12  LIT-2800                    PIC  X(4)   VALUE '2800'.    
00612      12  LIT-2825                    PIC  X(4)   VALUE '2825'.    
00613      12  LIT-2925                    PIC  X(4)   VALUE '2925'.    
052504     12  SAVE-WS-SORT-REC            PIC  X(227).
020811     12  SAVE-SRT-SFX                PIC  99     VALUE ZERO.
00615  EJECT                                                            
00616      12  SAV-CH-KEY.                                              
00617          16  SAV-CH-CONTROL-PRIMARY.                              
00618              20  SAV-CH-COMPANY-CD   PIC  X.                      
00619              20  SAV-CH-CARRIER      PIC  X.                      
00620              20  SAV-CH-GROUPING     PIC  X(6).                   
00621              20  SAV-CH-STATE        PIC  XX.                     
00622              20  SAV-CH-ACCOUNT      PIC  X(10).                  
00623              20  SAV-CH-CERT-EFF-DT  PIC  XX.                     
00624              20  SAV-CH-CERT-NO      PIC  X(11).                  
00625          16  SAV-CH-SEQUENCE-NO      PIC S9(4)   COMP.            
00626      12  TST-CH-KEY.                                              
00627          16  TST-CH-CONTROL-PRIMARY.                              
00628              20  TST-CH-COMPANY-CD   PIC  X.                      
00629              20  TST-CH-CARRIER      PIC  X.                      
00630              20  TST-CH-GROUPING     PIC  X(6).                   
00631              20  TST-CH-STATE        PIC  XX.                     
00632              20  TST-CH-ACCOUNT      PIC  X(10).                  
00633              20  TST-CH-CERT-EFF-DT  PIC  XX.                     
00634              20  TST-CH-CERT-NO      PIC  X(11).                  
00635          16  TST-CH-SEQUENCE-NO      PIC S9(4)   COMP.            
00636      12  SAV-PB-KEY.                                              
00637          16  SAV-PB-CONTROL-BY-ACCOUNT.                           
00638              20  SAV-PB-COMPANY-CD-A1    PIC  X.                  
00639              20  SAV-PB-CARRIER          PIC  X.                  
00640              20  SAV-PB-GROUPING         PIC  X(6).               
00641              20  SAV-PB-STATE            PIC  XX.                 
00642              20  SAV-PB-ACCOUNT          PIC  X(10).              
00643              20  SAV-PB-CERT-EFF-DT      PIC  XX.                 
00644              20  SAV-PB-CERT-NO          PIC  X(11).              
00645          16  SAV-PB-ALT-CHG-SEQ-NO       PIC S9(4)   COMP.        
00646          16  SAV-PB-RECORD-TYPE          PIC  X.                  
00647      12  TST-PB-KEY.                                              
00648          16  TST-PB-CONTROL-BY-ACCOUNT.                           
00649              20  TST-PB-COMPANY-CD-A1    PIC  X.                  
00650              20  TST-PB-CARRIER          PIC  X.                  
00651              20  TST-PB-GROUPING         PIC  X(6).               
00652              20  TST-PB-STATE            PIC  XX.                 
00653              20  TST-PB-ACCOUNT          PIC  X(10).              
00654              20  TST-PB-CERT-EFF-DT      PIC  XX.                 
00655              20  TST-PB-CERT-NO          PIC  X(11).              
00656          16  TST-PB-ALT-CHG-SEQ-NO       PIC S9(4)   COMP.        
00657          16  TST-PB-RECORD-TYPE          PIC  X.                  
00658      12  SAVE-COMPANY-NAME   PIC  X(30)  OCCURS 3 TIMES.          
00659 *  LEFT JUSTIFY COMPANY NAME.                                     
00660      12  LJ-NAME.                                                 
00661          16  LJ-CHAR         PIC  X      OCCURS 30 TIMES.         
00662  EJECT                                                            
00663      12  RMTX.                                                    
00664          16  RMT0            PIC  X(10).                          
00665          16  RMT1            PIC  X(30).                          
00666          16  RMT2            PIC  X(30).                          
00667          16  RMT3            PIC  X(30).                          
00668          16  RMT4            PIC  X(30).                          
00669          16  RMT5            PIC  X(30).                          
00670          16  RMT6.                                                
00671              20  FILLER      PIC  X(20).                          
00672              20  RMT-ZIP     PIC  X(10).                          
00673      12  BILX.                                                    
00674          16  BIL0            PIC  X(10).                          
00675          16  BIL1            PIC  X(30).                          
00676          16  BIL2            PIC  X(30).                          
00677          16  BIL3            PIC  X(30).                          
00678          16  BIL4            PIC  X(30).                          
00679          16  BIL5            PIC  X(30).                          
00680          16  BIL6            PIC  X(30).                          
00681      12  DUM-RMTX.                                                
00682          16  DUM-RMT0        PIC  X(10).                          
00683          16  DUM-RMT1        PIC  X(30)          VALUE SPACES.    
00684      12  DUM-BILX.                                                
00685          16  DUM-BIL0        PIC  X(10).                          
00686          16  DUM-BIL1        PIC  X(30)          VALUE            
00687                  'NEED MAILING NAME'.                             
00688          16  DUM-BIL2        PIC  X(30)          VALUE            
00689                  'NEED COMP. MASTER'.                             
00690          16  DUM-BIL3        PIC  X(30)          VALUE            
00691                  'NEED ADDRESS LINE 1'.                           
00692          16  DUM-BIL4        PIC  X(30)          VALUE            
00693                  'NEED ADDRESS LINE 2'.                           
00694          16  DUM-BIL5        PIC  X(30)          VALUE            
00695                  'NEED CITY, STATE ZIP'.                          
00696      12  REMIT-LEVELS.                                            
00697          16  RMT-LEVEL       PIC  X(190)     OCCURS 4 TIMES.      
00698      12  TELE-ZIP-LINE.                                           
00699          16  LINE5-TELE          PIC  X(12).                      
00700          16  FILLER              PIC  X(01).                      
00701          16  LINE5-ZIP-CODE.                                      
00702              20  LINE5-ZIP-FIVE  PIC  X(05).                      
00703              20  LINE5-ZIP-DASH  PIC  X(01).                      
00704              20  LINE5-ZIP-FOUR  PIC  X(04).                      
00705          16  LINE5-POSTAL-CODE  REDEFINES  LINE5-ZIP-CODE.        
00706              20  LINE5-POST-CODE1                                 
00707                                  PIC  X(03).                      
00708              20  FILLER          PIC  X(01).                      
00709              20  LINE5-POST-CODE2                                 
00710                                  PIC  X(03).                      
00711              20  FILLER          PIC  X(03).                      
00712          16  FILLER              PIC  X(07).                      
00713      12  WORK-ZIP-CODE.                                           
00714          16  WZC-PRIME       PIC  X(05).                          
00715          16  WZC-PLUS4       PIC  X(04).                          
00716      12  WORK-ZIP-CD  REDEFINES  WORK-ZIP-CODE.                   
00717          16  WZC-POS-1       PIC  X(01).                          
00718          16  FILLER          PIC  X(08).                          
00719      12  WORK-POSTAL-CD  REDEFINES  WORK-ZIP-CODE.                
00720          16  WZC-POST-CD1    PIC  X(03).                          
00721          16  WZC-POST-CD2    PIC  X(03).                          
00722          16  FILLER          PIC  X(03).                          
00723      12  COMP-3-AREA     COMP-3.                                  
00724          16  PGCTR           PIC S9(5)           VALUE +0.        
091306         16  TPGCTR          PIC S9(7)           VALUE +0.        
00725          16  LNCTR           PIC S9(3)           VALUE +0.        
00726          16  LNCTR-25-40     PIC S9(3)           VALUE +0.        
00727          16  LNCTR-34-49     PIC S9(3)           VALUE +0.        
00728          16  LNCTR-43-58     PIC S9(3)           VALUE +0.        
00729          16  VARY-LO         PIC S99V99          VALUE -00.00.    
00730          16  VARY-HI         PIC S99V99          VALUE +00.00.    
00731          16  WORK-PERC       PIC S99V9(3)        VALUE +0.        
00732          16  TOTAL-DUE       PIC S9(7)V99        VALUE +0.        
00733          16  NET-DUE         PIC S9(7)V99        VALUE +0.        
00734          16  TOT-NET-DUE     PIC S9(7)V99        VALUE +0.        
00735  EJECT                                                            
00736      12  CERT-TOTALS     COMP-3.                                  
00737          16  C-TT-FACE       PIC S9(7)V99.                        
00738          16  C-TT-FACE-ALT   PIC S9(7)V99.                        
00739          16  C-TT-AHBEN      PIC S9(7)V99.                        
00740          16  C-LF-PREM       PIC S9(7)V99.                        
00741          16  C-LF-PREM-ALT   PIC S9(7)V99.                        
00742          16  C-AH-PREM       PIC S9(7)V99.                        
00743          16  C-TT-PREM       PIC S9(7)V99.                        
00744          16  C-LF-REFD       PIC S9(7)V99.                        
00745          16  C-AH-REFD       PIC S9(7)V99.                        
00746          16  C-TT-REFD       PIC S9(7)V99.                        
00747          16  C-LF-COMM       PIC S9(7)V99.                        
00748          16  C-LF-COMM-ALT   PIC S9(7)V99.                        
00749          16  C-AH-COMM       PIC S9(7)V99.                        
00750          16  C-TT-COMM       PIC S9(7)V99.                        
00751          16  C-RR-PYMT       PIC S9(7)V99.
062519         16  c-rr-pymt-adj   pic s9(7)v99.
00752          16  C-CC-PYMT       PIC S9(7)V99.                        
00753          16  C-TT-PYMT       PIC S9(7)V99.                        
00754          16  C-LF-CLM        PIC S9(7)V99.                        
00755          16  C-AH-CLM        PIC S9(7)V99.                        
00756          16  C-TT-CLM        PIC S9(7)V99.                        
00757      12  STMT-TOTALS     COMP-3.                                  
00758          16  S-TT-FACE       PIC S9(7)V99.                        
00759          16  S-LF-PREM       PIC S9(7)V99.                        
00760          16  S-AH-PREM       PIC S9(7)V99.                        
00761          16  S-TT-PREM       PIC S9(7)V99.                        
00762          16  S-TT-PREM-BILL  PIC S9(7)V99.                        
00763          16  S-LF-REFD       PIC S9(7)V99.                        
00764          16  S-AH-REFD       PIC S9(7)V99.                        
00765          16  S-TT-REFD       PIC S9(7)V99.                        
00766          16  S-LF-COMM       PIC S9(7)V99.                        
00767          16  S-AH-COMM       PIC S9(7)V99.                        
00768          16  S-TT-COMM       PIC S9(7)V99.                        
00769          16  S-TT-COMM-BILL  PIC S9(7)V99.                        
00770          16  S-RR-PYMT       PIC S9(7)V99.
062519         16  s-rr-pymt-adj   pic s9(7)v99.
00771          16  S-CC-PYMT       PIC S9(7)V99.                        
00772          16  S-TT-PYMT       PIC S9(7)V99.                        
00773          16  S-TT-PYMT-BILL  PIC S9(7)V99.                        
00774          16  S-LF-CLM        PIC S9(7)V99.                        
00775          16  S-AH-CLM        PIC S9(7)V99.                        
00776          16  S-TT-CLM        PIC S9(7)V99.                        
00777          16  S-BEG-BAL       PIC S9(7)V99.                        
00778          16  S-WRT-OFF       PIC S9(7)V99.                        
00779          16  S-END-BAL       PIC S9(7)V99.                        
00780  EJECT                                                            
00781      12  FINAL-TOTALS    COMP-3.                                  
00782          16  T-BEG-BAL       PIC S9(7)V99        VALUE +0.        
00783          16  T-TOT-PRM       PIC S9(7)V99        VALUE +0.        
00784          16  T-TOT-REF       PIC S9(7)V99        VALUE +0.        
00785          16  T-TOT-COM       PIC S9(7)V99        VALUE +0.        
00786          16  T-ADJ-COM       PIC S9(7)V99        VALUE +0.        
00787          16  T-WRT-OFF       PIC S9(7)V99        VALUE +0.        
00788          16  T-TOT-PMT       PIC S9(7)V99        VALUE +0.        
00789          16  T-END-BAL       PIC S9(7)V99        VALUE +0.        
00790      12  NEW-CERT-TOTALS     COMP-3.                              
00791          16  NCT-L-ISS       PIC S9(5).                           
00792          16  NCT-A-ISS       PIC S9(5).                           
00793          16  NCT-L-CAN       PIC S9(5).                           
00794          16  NCT-A-CAN       PIC S9(5).                           
00795          16  NCT-DTH-CLM     PIC S9(5).                           
00796          16  NCT-DIS-CLM     PIC S9(5).                           
00797          16  NCT-L-PRM       PIC S9(7)V99.                        
00798          16  NCT-L-PRM-ALT   PIC S9(7)V99.                        
00799          16  NCT-A-PRM       PIC S9(7)V99.                        
00800          16  NCT-T-PRM       PIC S9(7)V99.                        
00801          16  NCT-L-COM       PIC S9(7)V99.                        
00802          16  NCT-L-COM-ALT   PIC S9(7)V99.                        
00803          16  NCT-A-COM       PIC S9(7)V99.                        
00804          16  NCT-T-COM       PIC S9(7)V99.                        
00805          16  NCT-L-REF       PIC S9(7)V99.                        
00806          16  NCT-A-REF       PIC S9(7)V99.                        
00807          16  NCT-T-REF       PIC S9(7)V99.                        
00808          16  NCT-L-RCM       PIC S9(7)V99.                        
00809          16  NCT-A-RCM       PIC S9(7)V99.                        
00810          16  NCT-T-RCM       PIC S9(7)V99.                        
00811          16  NCT-T-FAC       PIC S9(7)V99.                        
00812          16  NCT-T-FAC-ALT   PIC S9(7)V99.                        
00813          16  NCT-R-PMT       PIC S9(7)V99.                        
00814          16  NCT-C-CHG       PIC S9(7)V99.                        
00815          16  NCT-L-CLM       PIC S9(7)V99.                        
00816          16  NCT-A-CLM       PIC S9(7)V99.                        
00817          16  NCT-T-CLM       PIC S9(7)V99.                        
00818  EJECT                                                            
00819      12  NEW-STMT-TOTALS     COMP-3.                              
00820          16  NST-L-ISS       PIC S9(5).                           
00821          16  NST-A-ISS       PIC S9(5).                           
00822          16  NST-L-CAN       PIC S9(5).                           
00823          16  NST-A-CAN       PIC S9(5).                           
00824          16  NST-DTH-CLM     PIC S9(5).                           
00825          16  NST-DIS-CLM     PIC S9(5).                           
00826          16  NST-L-PRM       PIC S9(7)V99.                        
00827          16  NST-A-PRM       PIC S9(7)V99.                        
00828          16  NST-T-PRM       PIC S9(7)V99.                        
00829          16  NST-L-COM       PIC S9(7)V99.                        
00830          16  NST-A-COM       PIC S9(7)V99.                        
00831          16  NST-T-COM       PIC S9(7)V99.                        
00832          16  NST-L-REF       PIC S9(7)V99.                        
00833          16  NST-A-REF       PIC S9(7)V99.                        
00834          16  NST-T-REF       PIC S9(7)V99.                        
00835          16  NST-L-NET       PIC S9(7)V99.                        
00836          16  NST-A-NET       PIC S9(7)V99.                        
00837          16  NST-T-NET       PIC S9(7)V99.                        
00838          16  NST-L-RCM       PIC S9(7)V99.                        
00839          16  NST-A-RCM       PIC S9(7)V99.                        
00840          16  NST-T-RCM       PIC S9(7)V99.                        
00841          16  NST-T-FAC       PIC S9(7)V99.                        
00842          16  NST-R-PMT       PIC S9(7)V99.                        
00843          16  NST-C-CHG       PIC S9(7)V99.                        
00844          16  NST-L-CLM       PIC S9(7)V99.                        
00845          16  NST-A-CLM       PIC S9(7)V99.                        
00846          16  NST-T-CLM       PIC S9(7)V99.                        
00847          16  NST-B-BAL       PIC S9(7)V99.                        
00848          16  NST-W-OFF       PIC S9(7)V99.                        
00849          16  NST-E-BAL       PIC S9(7)V99.                        

00819      12  CASH-NEW-STMT-TOTALS     COMP-3.                              
00820          16  CNST-L-ISS       PIC S9(5).                           
00821          16  CNST-A-ISS       PIC S9(5).                           
00822          16  CNST-L-CAN       PIC S9(5).                           
00823          16  CNST-A-CAN       PIC S9(5).                           
00826          16  CNST-L-PRM       PIC S9(7)V99.                        
00827          16  CNST-A-PRM       PIC S9(7)V99.                        
00828          16  CNST-T-PRM       PIC S9(7)V99.                        
00829          16  CNST-L-COM       PIC S9(7)V99.                        
00830          16  CNST-A-COM       PIC S9(7)V99.                        
00831          16  CNST-T-COM       PIC S9(7)V99.                        
00832          16  CNST-L-REF       PIC S9(7)V99.                        
00833          16  CNST-A-REF       PIC S9(7)V99.                        
00834          16  CNST-T-REF       PIC S9(7)V99.                        
00838          16  CNST-L-RCM       PIC S9(7)V99.                        
00839          16  CNST-A-RCM       PIC S9(7)V99.                        
00840          16  CNST-T-RCM       PIC S9(7)V99.                        
00841          16  CNST-T-FAC       PIC S9(7)V99.                        
00850  EJECT                                                            
011410     12  NEW-FINAL-TOTALS    COMP-3 OCCURS 3.
00852          16  NFT-L-ISS       PIC S9(5).                           
00853          16  NFT-A-ISS       PIC S9(5).                           
00854          16  NFT-L-CAN       PIC S9(5).                           
00855          16  NFT-A-CAN       PIC S9(5).                           
00856          16  NFT-DTH-CLM     PIC S9(5).                           
00857          16  NFT-DIS-CLM     PIC S9(5).                           
00858          16  NFT-L-PRM       PIC S9(9)V99.                        
00859          16  NFT-A-PRM       PIC S9(9)V99.                        
00860          16  NFT-T-PRM       PIC S9(9)V99.                        
00861          16  NFT-L-COM       PIC S9(9)V99.                        
00862          16  NFT-A-COM       PIC S9(9)V99.                        
00863          16  NFT-T-COM       PIC S9(9)V99.                        
00864          16  NFT-L-REF       PIC S9(9)V99.                        
00865          16  NFT-A-REF       PIC S9(9)V99.                        
00866          16  NFT-T-REF       PIC S9(9)V99.                        
00867          16  NFT-L-NET       PIC S9(9)V99.                        
00868          16  NFT-A-NET       PIC S9(9)V99.                        
00869          16  NFT-T-NET       PIC S9(9)V99.                        
00870          16  NFT-L-RCM       PIC S9(9)V99.                        
00871          16  NFT-A-RCM       PIC S9(9)V99.                        
00872          16  NFT-T-RCM       PIC S9(9)V99.                        
00873          16  NFT-T-FAC       PIC S9(9)V99.                        
00874          16  NFT-R-PMT       PIC S9(9)V99.                        
00875          16  NFT-C-CHG       PIC S9(9)V99.                        
00876          16  NFT-L-CLM       PIC S9(9)V99.                        
00877          16  NFT-A-CLM       PIC S9(9)V99.                        
00878          16  NFT-T-CLM       PIC S9(9)V99.                        
00879          16  NFT-B-BAL       PIC S9(9)V99.                        
00880          16  NFT-W-OFF       PIC S9(9)V99.                        
00881          16  NFT-E-BAL       PIC S9(9)V99.                        
00882          16  NFT-ADJCO       PIC S9(9)V99.
00883      12  NEW-STMT-NP-TOTALS  COMP-3.                              
00884          16  NPS-L-PRM       PIC S9(9)V99   VALUE +0.             
00885          16  NPS-A-PRM       PIC S9(9)V99   VALUE +0.             
00886          16  NPS-T-PRM       PIC S9(9)V99   VALUE +0.             
00887          16  NPS-L-REF       PIC S9(9)V99   VALUE +0.             
00888          16  NPS-A-REF       PIC S9(9)V99   VALUE +0.             
00889          16  NPS-T-REF       PIC S9(9)V99   VALUE +0.             
00890          16  NPS-L-NET       PIC S9(9)V99   VALUE +0.             
00891          16  NPS-A-NET       PIC S9(9)V99   VALUE +0.             
00892          16  NPS-T-NET       PIC S9(9)V99   VALUE +0.             
00893          16  NPS-L-COM       PIC S9(9)V99   VALUE +0.             
00894          16  NPS-A-COM       PIC S9(9)V99   VALUE +0.             
00895          16  NPS-T-COM       PIC S9(9)V99   VALUE +0.             
011904         16  NPS-L-RCM       PIC S9(9)V99   VALUE +0.             
011904         16  NPS-A-RCM       PIC S9(9)V99   VALUE +0.             
011904         16  NPS-T-RCM       PIC S9(9)V99   VALUE +0.             
00896          16  NPS-TOT         PIC S9(9)V99   VALUE +0.             
00897      12  NEW-STMT-TOT-TOTALS  COMP-3.                             
00898          16  TOTS-L-PRM      PIC S9(9)V99   VALUE +0.             
00899          16  TOTS-A-PRM      PIC S9(9)V99   VALUE +0.             
00900          16  TOTS-T-PRM      PIC S9(9)V99   VALUE +0.             
00901          16  TOTS-L-REF      PIC S9(9)V99   VALUE +0.             
00902          16  TOTS-A-REF      PIC S9(9)V99   VALUE +0.             
00903          16  TOTS-T-REF      PIC S9(9)V99   VALUE +0.             
00904          16  TOTS-L-NET      PIC S9(9)V99   VALUE +0.             
00905          16  TOTS-A-NET      PIC S9(9)V99   VALUE +0.             
00906          16  TOTS-T-NET      PIC S9(9)V99   VALUE +0.             
00907          16  TOTS-L-COM      PIC S9(9)V99   VALUE +0.             
00908          16  TOTS-A-COM      PIC S9(9)V99   VALUE +0.             
00909          16  TOTS-T-COM      PIC S9(9)V99   VALUE +0.             
011904         16  TOTS-L-RCM      PIC S9(9)V99   VALUE +0.             
011904         16  TOTS-A-RCM      PIC S9(9)V99   VALUE +0.             
011904         16  TOTS-T-RCM      PIC S9(9)V99   VALUE +0.             
00910          16  TOTS-TOT        PIC S9(9)V99   VALUE +0.             
011410     12  NEW-FINAL-NP-TOTALS  COMP-3 OCCURS 3.
00912          16  NPF-L-PRM       PIC S9(9)V99.
00913          16  NPF-A-PRM       PIC S9(9)V99.
00914          16  NPF-T-PRM       PIC S9(9)V99.
00915          16  NPF-L-REF       PIC S9(9)V99.
00916          16  NPF-A-REF       PIC S9(9)V99.
00917          16  NPF-T-REF       PIC S9(9)V99.
00918          16  NPF-L-NET       PIC S9(9)V99.
00919          16  NPF-A-NET       PIC S9(9)V99.
00920          16  NPF-T-NET       PIC S9(9)V99.
00921          16  NPF-L-COM       PIC S9(9)V99.
00922          16  NPF-A-COM       PIC S9(9)V99.
00923          16  NPF-T-COM       PIC S9(9)V99.
011904         16  NPF-L-RCM       PIC S9(9)V99.
011904         16  NPF-A-RCM       PIC S9(9)V99.
011904         16  NPF-T-RCM       PIC S9(9)V99.
00924          16  NPF-TOT         PIC S9(9)V99.
011410     12  NEW-FINAL-TOT-TOTALS  COMP-3 OCCURS 3.
00926          16  TOTF-L-PRM      PIC S9(9)V99.
00927          16  TOTF-A-PRM      PIC S9(9)V99.
00928          16  TOTF-T-PRM      PIC S9(9)V99.
00929          16  TOTF-L-REF      PIC S9(9)V99.
00930          16  TOTF-A-REF      PIC S9(9)V99.
00931          16  TOTF-T-REF      PIC S9(9)V99.
00932          16  TOTF-L-NET      PIC S9(9)V99.
00933          16  TOTF-A-NET      PIC S9(9)V99.
00934          16  TOTF-T-NET      PIC S9(9)V99.
00935          16  TOTF-L-COM      PIC S9(9)V99.
00936          16  TOTF-A-COM      PIC S9(9)V99.
00937          16  TOTF-T-COM      PIC S9(9)V99.
011904         16  TOTF-L-RCM      PIC S9(9)V99.
011904         16  TOTF-A-RCM      PIC S9(9)V99.
011904         16  TOTF-T-RCM      PIC S9(9)V99.
00938          16  TOTF-TOT        PIC S9(9)V99.
00939      12  CUR-CP-SEQ.                                              
00940          16  CUR-CP-CTL-1.                                        
00941              20  CUR-CP-CARR-GROUP.                               
00942                  24  CUR-CP-CARR     PIC  X.                      
00943                  24  CUR-CP-GROUP    PIC  X(6).                   
00944              20  CUR-CP-RESP         PIC  X(10).                  
00945          16  CUR-CP-CTL-2.                                        
00946              20  CUR-CP-ACCT         PIC  X(10).                  
00947      12  PRE-CONTROL.                                             
00948          16  PRE-CTL-1.                                           
00949              20  PRE-CARR-GROUP.                                  
00950                  24  PRE-CARR        PIC  X.                      
00951                  24  PRE-GROUP       PIC  X(6).                   
00952              20  PRE-RESP            PIC  X(10).                  
00953          16  PRE-CTL-2.                                           
00954              20  PRE-ACCT            PIC  X(10).                  
00955      12  FIRST-CLAIM-SW              PIC  X.                      
00956          88  FIRST-CLAIM                         VALUE 'Y'.       
00957          88  ALREADY-PRINTED-TOTALS              VALUE 'N'.       
PEMTST     12  WS-EFF-DATE-NUM         PIC 9(11) VALUE ZEROS.
PEMTST     12  WS-EFF-DATE-R REDEFINES WS-EFF-DATE-NUM.
00959          16  FILLER              PIC  999.                        
00960          16  WS-EFF-CC           PIC  99.                         
00961          16  WS-EFF-YY           PIC  99.                         
00962          16  WS-EFF-MM           PIC  99.                         
00963          16  WS-EFF-DD           PIC  99.                         
PEMTST     12  WS-EXP-DATE-NUM         PIC 9(11) VALUE ZEROS.
PEMTST     12  WS-EXP-DATE-R REDEFINES WS-EXP-DATE-NUM.                 
00965          16  FILLER              PIC  999.                        
00966          16  WS-EXP-CC           PIC  99.                         
00967          16  WS-EXP-YY           PIC  99.                         
00968          16  WS-EXP-MM           PIC  99.                         
00969          16  WS-EXP-DD           PIC  99.                         
00970      12  WS-SRT02-EFF-DT-R       PIC 9(11).                       
00971      12  WS-SRT02-EFF-DT-RDEF  REDEFINES  WS-SRT02-EFF-DT-R.      
00972          16  FILLER              PIC  999.                        
00973          16  WS-SRT02-EFF-CC     PIC  99.                         
00974          16  WS-SRT02-EFF-YR     PIC  99.                         
00975          16  WS-SRT02-EFF-MO     PIC  99.                         
00976          16  WS-SRT02-EFF-DA     PIC  99.                         
00977      12  WS-SRT03-EFF-DT-R       PIC 9(11).                       
00978      12  WS-SRT03-EFF-DT-RDEF  REDEFINES  WS-SRT03-EFF-DT-R.      
00979          16  FILLER              PIC  999.                        
00980          16  WS-SRT03-EFF-CC     PIC  99.                         
00981          16  WS-SRT03-EFF-YR     PIC  99.                         
00982          16  WS-SRT03-EFF-MO     PIC  99.                         
00983          16  WS-SRT03-EFF-DA     PIC  99.                         
00984      12  WS-SRT03-LF-CAN-DT-R    PIC 9(11).                       
00985      12  WS-SRT03-LF-CAN-DT-RDEF  REDEFINES  WS-SRT03-LF-CAN-DT-R.
00986          16  FILLER              PIC  999.                        
00987          16  WS-SRT03-LF-CAN-CC  PIC  99.                         
00988          16  WS-SRT03-LF-CAN-YR  PIC  99.                         
00989          16  WS-SRT03-LF-CAN-MO  PIC  99.                         
00990          16  WS-SRT03-LF-CAN-DA  PIC  99.                         
00991      12  WS-SRT03-AH-CAN-DT-R    PIC  9(11).                      
00992      12  WS-SRT03-AH-CAN-DT-RDEF  REDEFINES  WS-SRT03-AH-CAN-DT-R.
00993          16  FILLER              PIC  999.                        
00994          16  WS-SRT03-AH-CAN-CC  PIC  99.                         
00995          16  WS-SRT03-AH-CAN-YR  PIC  99.                         
00996          16  WS-SRT03-AH-CAN-MO  PIC  99.                         
00997          16  WS-SRT03-AH-CAN-DA  PIC  99.                         
00998      12  WS-SRT12-EFF-DT-R       PIC  9(11).                      
00999      12  WS-SRT12-EFF-DT-RDEF  REDEFINES  WS-SRT12-EFF-DT-R.      
01000          16  FILLER              PIC  999.                        
01001          16  WS-SRT12-EFF-CC     PIC  99.                         
01002          16  WS-SRT12-EFF-YR     PIC  99.                         
01003          16  WS-SRT12-EFF-MO     PIC  99.                         
01004          16  WS-SRT12-EFF-DA     PIC  99.                         
01005      12  WS-SRT03A-CHEK-DT-R      PIC  9(11).                     
01006      12  WS-SRT03A-CHEK-DT-RDEF  REDEFINES  WS-SRT03A-CHEK-DT-R.  
01007          16  FILLER              PIC  999.                        
01008          16  WS-SRT03A-CK-CC     PIC  99.                         
01009          16  WS-SRT03A-CK-YR     PIC  99.                         
01010          16  WS-SRT03A-CK-MO     PIC  99.                         
01011          16  WS-SRT03A-CK-DA     PIC  99.                         
01012  01  COV-TABLE.                                                   
01013      05  LF-COV              PIC  X.                              
01014      05  ALT-COV             PIC  X.                              
01015      05  AH-COV              PIC  X.                              
01016  EJECT                                                            
01017  01  HD1.                                                         
01018      12  FILLER              PIC  X(51)          VALUE SPACES.    
011410     12  HD1-TITLE           PIC  X(35)          VALUE
011410             'STATEMENT FOR CREDIT INSURANCE'.
011410     12  FILLER              PIC  X(23)          VALUE SPACES.
100307     12  HD1-CSR             PIC  X(4)           VALUE SPACES.
100307     12  FILLER              PIC  X(6)           VALUE SPACES.
01021      12  FILLER              PIC  X(8)           VALUE 'EL562 '.  
01022                                                                   
01023  01  HD2.                                                         
01024      12  FILLER              PIC  X(51)          VALUE SPACES.    
01025      12  HD-CO               PIC  X(30).                          
01026      12  FILLER              PIC  X(38)          VALUE SPACES.    
01027      12  HD-RUN-DT           PIC  X(8)           VALUE SPACES.    
01028                                                                   
01029  01  HD3.                                                         
01030      12  FILLER              PIC  X(57)          VALUE SPACES.    
01031      12  HD-DT               PIC  X(18).                          
01032      12  FILLER              PIC  X(44)          VALUE SPACES.    
01033      12  FILLER              PIC  X(5)           VALUE 'PAGE'.    
01034      12  HD-PG               PIC  ZZ,ZZ9.                         
01035      12  FILLER              PIC  X(2)           VALUE SPACES.    
01036                                                                   
01037  01  HD4                     VALUE SPACES.                        
01038      12  HD-M1               PIC  X(14).                          
01039      12  HD-CARR-GROUP       PIC  X(7).                           
01040      12  HD-C-A              PIC  X.                              
01041      12  HD-ACCT             PIC  X(10).                          
01042      12  FILLER              PIC  XX.                             
01043      12  HD-NAME             PIC  X(30).                          
01044      12  FILLER              PIC  X(13).                          
01045      12  HD-M2               PIC  X(11).                          
01046      12  HD-M3               PIC  X(10).                          
01047      12  FILLER              PIC  XX.                             
01048      12  HD-REMIT            PIC  X(30).                          
01049      12  FILLER              PIC  XX.                             
01050                                                                   
01051  01  HD5                     PIC  X(132)         VALUE SPACES.    
01052                                                                   
01053  01  HD5A.                                                        
01054      12  FILLER              PIC  X(54)          VALUE SPACES.    
01055      12  FILLER              PIC  X(24)          VALUE            
01056              'N E W    B U S I N E S S'.                          
01057                                                                   
052504 01  HD5AA.                                                       
052504     12  FILLER              PIC  X(54)          VALUE SPACES.    
052504     12  FILLER              PIC  X(25)          VALUE            
052504             'C A S H   R E C E I V E D'.                         
052504                                                                  
052504 01  HD5AB.                                                       
052504     12  FILLER              PIC  X(54)          VALUE SPACES.    
052504     12  FILLER              PIC  X(25)          VALUE            
052504             'C A S H   R E T U R N E D'.                         
052504                                                                  
01058  01  HD5B.                                                        
01059      12  FILLER              PIC  X(46)          VALUE SPACES.    
01060      12  FILLER              PIC  X(41)          VALUE            
01061              'N E W    B U S I N E S S    (E R R O R S)'.         
01062  EJECT                                                            
01063  01  HD5C.                                                        
01064      12  FILLER              PIC  X(48)          VALUE SPACES.    
01065      12  FILLER              PIC  X(37)          VALUE            
01066              'N E W    B U S I N E S S    (H O L D)'.             
01067                                                                   
01068  01  HD5D.                                                        
01069      12  FILLER              PIC  X(34)          VALUE SPACES.    
01070      12  FILLER              PIC  X(45)          VALUE            
01071         'N E W    B U S I N E S S    (R E T U R N E D '.          
01072      12  FILLER              PIC  X(20)          VALUE            
01073         ' -  D E C L I N E D)'.                                   
01074                                                                   
01075  01  HD5E.                                                        
01076      12  FILLER              PIC  X(54)          VALUE SPACES.    
01077      12  FILLER              PIC  X(25)          VALUE            
01078              'C A N C E L L A T I O N S'.                         
01079                                                                   
01080  01  HD5F.                                                        
01081      12  FILLER              PIC  X(45)          VALUE SPACES.    
01082      12  FILLER              PIC  X(42)          VALUE            
01083              'C A N C E L L A T I O N S    (E R R O R S)'.        
01084                                                                   
01085  01  HD5G.                                                        
01086      12  FILLER              PIC  X(47)          VALUE SPACES.    
01087      12  FILLER              PIC  X(38)          VALUE            
01088              'C A N C E L L A T I O N S    (H O L D)'.            
01089                                                                   
01090  01  HD5H.                                                        
01091      12  FILLER              PIC  X(43)          VALUE SPACES.    
01092      12  FILLER              PIC  X(46)          VALUE            
01093              'C A N C E L L A T I O N S    (R E T U R N E D)'.    
01094                                                                   
01095  01  HD5I.                                                        
01096      12  FILLER              PIC  X(42)          VALUE SPACES.    
01097      12  FILLER              PIC  X(50)          VALUE            
01098              'P A Y M E N T S    A N D    A D J U S T M E N T S'. 
01099                                                                   
01100  01  HD5J.                                                        
01101      12  FILLER              PIC  X(60)          VALUE SPACES.    
01102      12  FILLER              PIC  X(13)          VALUE            
01103              'S U M M A R Y'.                                     
01104                                                                   
01105  01  HD5K.                                                        
01106      12  FILLER              PIC  X(52)          VALUE SPACES.    
01107      12  FILLER              PIC  X(28)          VALUE            
01108              'C L A I M    A C T I V I T Y'.                      
01109  01  HD5L.                                                        
01110      12  FILLER              PIC  X(34)          VALUE SPACES.    
01111      12  FILLER              PIC  X(45)          VALUE            
01112         'N E W    B U S I N E S S    (V O I D E D)    '.          
01113      12  FILLER              PIC  X(20)          VALUE            
01114         '                    '.                                   
01115                                                                   
01116  EJECT                                                            
01117  01  HD6                     PIC  X(132)         VALUE SPACES.    
01118                                                                   
01119 ****      HD6A     NEW BUSINESS LINE 1 STANDARD                   
01120  01  HD6A.                                                        
01121      12  FILLER              PIC  X(44)          VALUE            
01122              '                             CERTIFICATE    '.      
01123      12  FILLER              PIC  X(44)          VALUE            
01124              'EFF.           BEN.  FACE AMOUNT /   -------'.      
01125      12  FILLER              PIC  X(23)          VALUE            
01126              '--- PREMIUM ---------- '.                           
01127      12  HD6A-COMP           PIC  X(05)          VALUE            
01128              'COMP.'.                                             
01129                                                                   
01130 ****      HD6A-1   NEW BUSINESS LINE 1 WITH NET PREMIUM           
01131  01  HD6A-1.                                                      
01132      12  FILLER              PIC  X(44)          VALUE            
01133              '                    CERTIFICATE    EFF.     '.      
01134      12  FILLER              PIC  X(44)          VALUE            
01135              '      BEN.  FACE AMOUNT /  ---------- PREMIU'.      
01136      12  FILLER              PIC  X(44)          VALUE            
01137              'M ----------    COMPENSATION          NET   '.      
01138                                                                   
01139 ****      HD6B-1   CANCELLATION LINE 1 STANDARD                   
01140  01  HD6B-1.                                                      
01141      12  FILLER              PIC  X(44)          VALUE            
01142              '                   CERTIFICATE  EFFECTIVE CA'.      
01143      12  FILLER              PIC  X(13)          VALUE            
01144              'NCEL  ------ '.                                     
01145      12  HD6B-1-LF           PIC  X(6).                           
01146      12  FILLER              PIC  X(22)          VALUE            
01147              ' REFUND ------ ------ '.                            
01148      12  HD6B-1-AH           PIC  X(6).                           
           12  FILLER              PIC  X(20)          VALUE
                   '                    '.
01149 *    12  FILLER              PIC  X(41)          VALUE            
01150 *            ' REFUND ------ -- COMPENSATION REFUND ---'.         
           12  HD6B-COMP           PIC  X(5)          VALUE
                   'COMP.'.
01151                                                                   
01152 ****      HD6B-2   CANCELLATION LINE 1 WITH LIFE AND A/H ON       
01153 ****               SEPARATE LINES                                 
01154  01  HD6B-2.                                                      
01155      12  FILLER              PIC  X(44)          VALUE            
011410             '                             CERTIFICATE  EF'.      
01157      12  FILLER              PIC  X(44)          VALUE            
011410             'FECTIVE CANCEL          BEN          -------'.      
01159      12  FILLER              PIC  X(44)          VALUE            
01160              '---  REFUND  --------- COMP.             '.
01161                                                                   
01162 ****      HD6B-3   CANCELLATION LINE 1 WITH LIFE AND A/H ON       
01163 ****          SEPARATE LINES AND NET PREMIUM   CLIENT BWS ONLY
01164  01  HD6B-3.                                                      
01165      12  FILLER              PIC  X(44)          VALUE            
01166              '                   CERTIFICATE  EFFECTIVE CA'.      
01167      12  FILLER              PIC  X(44)          VALUE            
01168              'NCEL           BEN    ---------- REFUND ----'.      
01169      12  FILLER              PIC  X(44)          VALUE            
01170              '------    COMPENSATION                NET   '.      
01171                                                                   
01172  01  HD6C.                                                        
01173      12  FILLER              PIC  X(44)          VALUE            
01174              '                 DESCRIPTION                '.      
01175      12  FILLER              PIC  X(44)          VALUE            
01176              'RECEIPTS           CHARGES                  '.      
01177      12  FILLER              PIC  X(44)          VALUE SPACES.    
01178                                                                   
01179  01  HD6D.                                                        
01180      12  FILLER              PIC  X(44)          VALUE            
01181              '                              CERTIFICATE EF'.      
01182      12  FILLER              PIC  X(44)          VALUE            
01183              'FECTIVE                                     '.      
01184      12  FILLER              PIC  X(25)          VALUE SPACES.    
01185      12  HD6D-FILL           PIC  X(6)           VALUE SPACES.    
01186      12  FILLER              PIC  X(13)          VALUE SPACES.    
01187                                                                   
01188  01  HD7                     PIC  X(132)         VALUE SPACES.    
01189                                                                   
01190 ****      HD7A     NEW BUSINESS LINE 2 STANDARD                   
01191  01  HD7A.                                                        
01192      12  FILLER              PIC  X(44)          VALUE            
01193              '       NAME OF INSURED         NUMBER       '.      
01194      12  FILLER              PIC  X(14)          VALUE            
01195              'DATE  AGE TRM '.
032905     12  HD7A-BEN-HD         PIC  X(6)  VALUE ' TYPE '.

01194      12  FILLER              PIC  X(24)          VALUE            
01195              'MONTHLY BENEFIT     REPO'.      
01196      12  FILLER              PIC  X(11)          VALUE            
01197              'RTED       '.
01198      12  HD7A-PCT            PIC  X(16)          VALUE            
01199              'REVISED     PCT.'.                                              
01200      12  FILLER              PIC  X(16)          VALUE            
01201              '   COMPENSATION '.                                  
01202                                                                   
01203 ****      HD7A-1   NEW BUSINESS LINE 2 WITH NET PREMIUM           
01204  01  HD7A-1.                                                      
01205      12  FILLER              PIC  X(44)          VALUE            
01206              'NAME OF INSURED       NUMBER       DATE  AGE'.      
01207      12  FILLER              PIC  X(44)          VALUE            
01208              ' TRM  TYPE MONTHLY BENEFIT      REPORTED    '.      
01209      12  FILLER              PIC  X(44)          VALUE            
01210              '   REVISED    PCT.      AMOUNT        DUE   '.      
01211                                                                   
01212 ****      HD7B-1   CANCELLATION LINE 2 STANDARD                   
01213  01  HD7B-1.                                                      
01214      12  FILLER              PIC  X(44)          VALUE            
01215              ' NAME OF INSURED     NUMBER       DATE     D'.      
01216      12  FILLER              PIC  X(44)          VALUE            
01217              'ATE      REPORTED      REVISED       REPORTE'.      
           12  FILLER              PIC  X(11)          VALUE
                   '           '.
           12  HD7B-PCT            PIC  X(15)          VALUE
                   'REVISED    PCT.'.
           12  FILLER              PIC  X(18)          VALUE
                   '   COMPENSATION   '.


01218 *    12  FILLER              PIC  X(21)          VALUE            
01219 *            'D      REVISED       '.                             
01220      12  HD7B-LF             PIC  X(6).                           
01221 *    12  FILLER              PIC  X(8)           VALUE SPACES.    
01222      12  HD7B-AH             PIC  X(6).                           
01223 *    12  FILLER              PIC  X(3)           VALUE SPACES.    
01224                                                                   
01225 ****      HD7B-2   CANCELLATION LINE 2 WITH LIFE AND A/H ON       
01226 ****               SEPARATE LINES                                 
01227  01  HD7B-2.                                                      
01228      12  FILLER              PIC  X(44)          VALUE            
011410             '       NAME OF INSURED         NUMBER       '.      
01230      12  FILLER              PIC  X(44)          VALUE            
011410             'DATE     DATE           TYPE            REPO'.      
01232      12  FILLER              PIC  X(44)          VALUE            
01233              'RTED       REVISED    PCT.   COMPENSATION   '.
01234                                                                   
01235 ****      HD7B-3   CANCELLATION LINE 2 WITH LIFE AND A/H ON       
01236 ****               SEPARATE LINES AND NET PREMIUM                 
01237  01  HD7B-3.                                                      
01238      12  FILLER              PIC  X(44)          VALUE            
01239              ' NAME OF INSURED     NUMBER       DATE     D'.      
01240      12  FILLER              PIC  X(44)          VALUE            
01241              'ATE            TYPE        REPORTED       RE'.      
01242      12  FILLER              PIC  X(44)          VALUE            
01243              'VISED     PCT.      AMOUNT            DUE   '.      
01244                                                                   
01245  01  HD7D.                                                        
01246      12  FILLER              PIC  X(44)          VALUE            
01247 *            '       NAME OF INSURED          NUMBER      '.      
060603             '                                NUMBER      '.      
01248      12  FILLER              PIC  X(44)          VALUE            
01249              'DATE          TYPE OF CLAIM PAYMENT         '.      
01250      12  FILLER              PIC  X(25)          VALUE            
01251              ' CLAIM AMOUNT            '.                         
01252      12  HD7D-FILL           PIC  X(6)           VALUE SPACES.    
01253      12  FILLER              PIC  X(13)          VALUE SPACES.    
01254                                                                   
01255  01  HD8D.                                                        
01256      12  FILLER              PIC  X(47)          VALUE            
01257              '                                            '.      
01258      12  FILLER              PIC  X(44)          VALUE            
01259              '            PROCESSED     NON PROCESSED     '.      
01260      12  FILLER              PIC  X(42)          VALUE            
01261              '      TOTAL                               '.        
01262  01  HDR-DASH-LINE.                                               
01263      12  FILLER              PIC  X(44)          VALUE            
01264              '--------------------------------------------'.      
01265      12  FILLER              PIC  X(44)          VALUE            
01266              '--------------------------------------------'.      
01267      12  FILLER              PIC  X(44)          VALUE            
01268              '--------------------------------------------'.      
01269  EJECT                                                            
01270  01  P-REC.                                                       
01271      12  P-CCSW                  PIC  X.                          
01272      12  P-LINE.                                                  
01273          16  FILLER              PIC  X(132).                     
01274      12  P-LINE-1  REDEFINES  P-LINE.                             
01275          16  P-CP-NAME.                                           
01276              20  P-CP-LNAME      PIC  X(15).                      
01277              20  FILLER          PIC  X.                          
01278              20  P-CP-FNAME      PIC  X(10).                      
01279              20  FILLER          PIC  X.                          
01280              20  P-CP-INITIAL    PIC  X.                          
01281              20  FILLER          PIC  X.                          
01282          16  P-CP-NAME-RECALC  REDEFINES  P-CP-NAME.              
01283              20  P-CP-RECALC     PIC  X(03).                      
01284              20  P-CP-LNAME-R    PIC  X(15).                      
01285              20  FILLER          PIC  X.                          
01286              20  P-CP-FNAME-R    PIC  X(10).                      
01287          16  P-CP-MEMBER.                                         
01288              20  P-CP-CERT       PIC  X(11).                      
01289              20  FILLER          PIC  XX.                         
01290          16  P-CP-EFF-MO         PIC  XX.                         
01291          16  P-CP-EFF-MOD        PIC  X.                          
01292          16  P-CP-EFF-DA         PIC  XX.                         
01293          16  P-CP-EFF-DAD        PIC  X.                          
01294          16  P-CP-EFF-YR         PIC  XX.                         
01295          16  FILLER              PIC  X.                          
01296          16  P-CP-AGE            PIC  XX.                         
01297          16  FILLER              PIC  X.                          
01298          16  P-CP-TERM           PIC  ZZZ.                        
01299          16  FILLER              PIC  X.                          
01300          16  P-CP-BEN            PIC  XX.                         
01301          16  P-CP-BEN-DASH       PIC  X.                          
01302          16  P-CP-BEN-TYPE       PIC  X(3).                       
01303          16  P-CP-FACE           PIC  ZZZZ,ZZZ,ZZZ.ZZ-.           
01304          16  P-CP-FACE-DOL  REDEFINES                             
01305              P-CP-FACE           PIC  ZZZZZZZZZZZZ.ZZ-.           
01306          16  FILLER              PIC  X.                          
01307          16  P-CP-PREM           PIC  ZZ,ZZZ,ZZZ.ZZ-.             
01308          16  FILLER              PIC  X.                          
01309          16  P-CP-PREM-REV       PIC  ZZ,ZZZ,ZZZ.ZZ-.             
01310          16  P-CP-PREM-REV-R  REDEFINES  P-CP-PREM-REV.           
01311              20  FILLER          PIC  X(10).                      
01312              20  P-CP-PREM-REV-ZEROS                              
01313                                  PIC  X(03).                      
01314              20  FILLER          PIC  X(01).                      
01315          16  P-CP-COMM-PCT       PIC  ZZZ.ZZZ.                    
01316          16  FILLER              PIC  X.                          
01317          16  P-CP-COMM           PIC  ZZ,ZZZ,ZZZ.ZZ-.             
01318      12  P-LINE-1-NET  REDEFINES  P-LINE.                         
01319          16  P-CP-3-NAME.                                         
01320              20  P-CP-3-LNAME    PIC  X(15).                      
01321              20  FILLER          PIC  X.                          
01322              20  P-CP-3-INIT-1   PIC  X.                          
01323              20  FILLER          PIC  X.                          
01324              20  P-CP-3-INIT-2   PIC  X.                          
01325              20  FILLER          PIC  X.                          
01326          16  P-CP-3-NAME-RECALC REDEFINES P-CP-3-NAME.            
01327              20  P-CP-3-RECALC   PIC  X(03).                      
01328              20  P-CP-3-LNAME-R  PIC  X(15).                      
01329              20  FILLER          PIC  X.                          
01330              20  P-CP-3-FNAME-R  PIC  X.                          
01331          16  P-CP-3-MEMBER.                                       
01332              20  P-CP-3-CERT     PIC  X(11).                      
01333              20  FILLER          PIC  XX.                         
01334          16  P-CP-3-EFF-MO       PIC  XX.                         
01335          16  P-CP-3-EFF-MOD      PIC  X.                          
01336          16  P-CP-3-EFF-DA       PIC  XX.                         
01337          16  P-CP-3-EFF-DAD      PIC  X.                          
01338          16  P-CP-3-EFF-YR       PIC  XX.                         
01339          16  FILLER              PIC  X.                          
01340          16  P-CP-3-AGE          PIC  XX.                         
01341          16  FILLER              PIC  X.                          
01342          16  P-CP-3-TERM         PIC  ZZZ.                        
01343          16  FILLER              PIC  X.                          
01344          16  P-CP-3-BEN          PIC  XX.                         
01345          16  P-CP-3-BEN-DASH     PIC  X.                          
01346          16  P-CP-3-BEN-TYPE     PIC  X(3).                       
01347          16  P-CP-3-FACE         PIC  ZZZZ,ZZZ,ZZZ.ZZ-.           
01348          16  P-CP-3-FACE-DOL REDEFINES                            
01349              P-CP-3-FACE         PIC  ZZZZZZZZZZZZ.ZZ-.           
01350          16  P-CP-3-PREM         PIC  ZZ,ZZZ,ZZZ.ZZ-.             
01351          16  P-CP-3-PREM-REV     PIC  ZZ,ZZZ,ZZZ.ZZ-.             
01352          16  P-CP-3-PREM-REV-R REDEFINES P-CP-3-PREM-REV.         
01353              20  FILLER          PIC  X(10).                      
01354              20  P-CP-3-PREM-REV-ZEROS                            
01355                                  PIC  X(03).                      
01356              20  FILLER          PIC  X(01).                      
01357          16  P-CP-3-COMM-PCT     PIC  ZZZ.ZZZ.                    
01358          16  P-CP-3-COMM         PIC  Z,ZZZ,ZZZ.ZZ-.              
01359          16  P-CP-3-NET-AMT      PIC  Z,ZZZ,ZZZ.ZZ-.              

01361      12  P-LINE-2  REDEFINES  P-LINE.                             
01362          16  PC-CP-INSURED-INFO.                                  
01363              20  PC-CP-NAME.                                      
01364                  24  PC-CP-LNAME         PIC  X(15).              
01365                  24  FILLER              PIC  X.                  
011410                 24  PC-CP-FNAME         PIC  X(10).
                       24  FILLER              PIC  X.               
01367                  24  PC-CP-INITIAL       PIC  X.                  
01368                  24  FILLER              PIC  X.                  
01369              20  PC-CP-NAME-RECALC  REDEFINES  PC-CP-NAME.        
01370                  24  PC-CP-RECALC        PIC  XXX.                
01371                  24  PC-CP-LNAME-R       PIC  X(15).              
01372                  24  FILLER              PIC  X.
011410                 24  PC-CP-FNAME-R       PIC  X(10).                  
01373              20  PC-CP-MEMBER.                                    
01374                  24  PC-CP-CERT      PIC  X(11).                  
01375                  24  FILLER          PIC  XX.                     
01376              20  PC-CP-EFF-MO        PIC  XX.                     
01377              20  PC-CP-EFF-MOD       PIC  X.                      
01378              20  PC-CP-EFF-DA        PIC  XX.                     
01379              20  PC-CP-EFF-DAD       PIC  X.                      
01380              20  PC-CP-EFF-YR        PIC  XX.                     
01381              20  FILLER              PIC  X.                      
01382              20  PC-CP-CAN-MO        PIC  XX.                     
01383              20  PC-CP-CAN-MOD       PIC  X.                      
01384              20  PC-CP-CAN-DA        PIC  XX.                     
01385              20  PC-CP-CAN-DAD       PIC  X.                      
01386              20  PC-CP-CAN-YR        PIC  XX.                     
01387              20  FILLER              PIC  X.                      
01388          16  PC-CP-AMT-INFO.                                      
040504             20  PC-CP-LF-REF        PIC  Z,ZZZ,ZZ9.99-.
01390              20  FILLER              PIC  X.                      
01391              20  PC-CP-LF-REF-REV    PIC  Z,ZZZ,ZZZ.ZZ-.          
01392              20  PC-CP-LF-REF-REV-R  REDEFINES  PC-CP-LF-REF-REV. 
01393                  24  FILLER          PIC  X(09).                  
01394                  24  PC-CP-LF-REF-REV-ZEROS                       
01395                                      PIC  X(03).                  
01396                  24  FILLER          PIC  X(01).                  
01397              20  FILLER              PIC  X.                      
040504             20  PC-CP-AH-REF        PIC  Z,ZZZ,ZZ9.99-.
01399              20  FILLER              PIC  X.                      
040504             20  PC-CP-AH-REF-REV    PIC  Z,ZZZ,ZZ9.99-.
01401              20  PC-CP-AH-REF-REV-R  REDEFINES  PC-CP-AH-REF-REV. 
01402                  24  FILLER          PIC  X(09).                  
01403                  24  PC-CP-AH-REF-REV-ZEROS                       
01404                                      PIC  X(03).                  
01405                  24  FILLER          PIC  X(01).                  
040504             20  PC-CP-LCOM-REF      PIC  Z,ZZZ,ZZ9.99-.
01407              20  FILLER              PIC  X.                      
040504             20  PC-CP-ACOM-REF      PIC  Z,ZZZ,ZZ9.99-.
01409      12  P-LINE-2-2  REDEFINES  P-LINE.                           
011410         16  FILLER                  PIC  X(68).
01411          16  PC-CP-BEN               PIC  XX.                     
01412          16  PC-CP-BEN-DASH          PIC  X.                      
01413          16  PC-CP-BEN-TYPE          PIC  X(3).                   
011410         16  FILLER                  PIC  X(08).
040504         16  PC-CP-2-REF             PIC  Z,ZZZ,ZZ9.99-.
01416          16  FILLER                  PIC  XX.                     
040504         16  PC-CP-2-REF-REV         PIC  Z,ZZZ,ZZ9.99-.
01418          16  PC-CP-2-REF-REV-R  REDEFINES  PC-CP-2-REF-REV.       
01419              20  FILLER              PIC  X(09).                  
01420              20  PC-CP-2-REF-REV-ZEROS                            
01421                                      PIC  X(03).                  
01422              20  FILLER              PIC  X.                      
01423          16  PC-CP-2-COMM-PCT        PIC  ZZZ.ZZZ.                
01424          16  FILLER                  PIC  XX.                     
040504         16  PC-CP-2-COMM-AMT        PIC  Z,ZZZ,ZZ9.99-.
01426      12  P-LINE-2-3  REDEFINES  P-LINE.                           
011410         16  FILLER                  PIC  X(68).
01428          16  PC-CP-BEN-3             PIC  XX.                     
01429          16  PC-CP-BEN-DASH-3        PIC  X.                      
01430          16  PC-CP-BEN-TYPE-3        PIC  X(3).                   
01431          16  FILLER                  PIC  X(3).                   
040504         16  PC-CP-3-REF             PIC  Z,ZZZ,ZZ9.99-.
01433          16  FILLER                  PIC  XX.                     
040504         16  PC-CP-3-REF-REV         PIC  Z,ZZZ,ZZ9.99-.
01435          16  PC-CP-3-REF-REV-R  REDEFINES  PC-CP-3-REF-REV.       
01436              20  FILLER              PIC  X(09).                  
01437              20  PC-CP-3-REF-REV-ZEROS                            
01438                                      PIC  X(03).                  
01439              20  FILLER              PIC  X.                      
01440          16  PC-CP-3-COMM-PCT        PIC  ZZZ.ZZZ.                
01441          16  FILLER                  PIC  XX.                     
040504         16  PC-CP-3-COMM-AMT        PIC  Z,ZZZ,ZZ9.99-.
01443          16  FILLER                  PIC  XX.                     
040504         16  PC-CP-3-NET-AMT         PIC  Z,ZZZ,ZZ9.99-.
01445      12  P-LINE-3  REDEFINES  P-LINE.                             
01446          16  FILLER              PIC  X(35).                      
01447          16  P-LF-NOTE           PIC  X(10).                      
01448          16  FILLER  REDEFINES  P-LF-NOTE.                        
01449              20  FILLER          PIC  X(9).                       
01450              20  P-LF-SEVERITY   PIC  X.                          
01451          16  FILLER              PIC  X(3).                       
01452          16  P-L4-MESSAGE        PIC  X(84).                      
01453      12  P-LINE-4  REDEFINES  P-LINE.                             
01454          16  FILLER              PIC  X(10).                      
01455          16  P-PYMT-DESC         PIC  X(30).                      
01456          16  FILLER              PIC  X.                          
01457          16  P-R-PMT             PIC  Z,ZZZ,ZZZ.99-.              
01458          16  FILLER              PIC  X(6).                       
01459          16  P-C-CHG             PIC  Z,ZZZ,ZZZ.99-.              
01460          16  FILLER              PIC  X(7).
082707         16  P-ACCT-MAINT-DATE   PIC  X(8).                       
01461          16  P-ACCT-ENTRY        PIC  X(30).                      
082707         16  FILLER              PIC  X(14).                      
01463  EJECT                                                            
01464      12  P-LINE-5  REDEFINES  P-LINE.                             
01465          16  FILLER              PIC  X(15).                      
01466          16  T-DESC              PIC  X(25).                      
01467          16  FILLER              PIC  XXX.                        
01468          16  T-EXPL              PIC  X(11).                      
01469          16  T-AMT-P             PIC  ZZZ,ZZZ,ZZZ.99-.            
01470          16  T-AMTR-P  REDEFINES                                  
01471              T-AMT-P             PIC  X(15).                      
01472          16  FILLER              PIC  XX.                         
01473          16  T-AMT-N             PIC  ZZZ,ZZZ,ZZZ.99-.            
01474          16  T-AMTR-N  REDEFINES                                  
01475              T-AMT-N             PIC  X(15).                      
01476          16  FILLER              PIC  XX.                         
01477          16  T-AMT-T             PIC  ZZZ,ZZZ,ZZZ.99-.            
01478          16  T-AMTR-T  REDEFINES                                  
01479              T-AMT-T             PIC  X(15).                      
01480          16  FILLER              PIC  X(29).                      
01481      12  P-LINE-6  REDEFINES  P-LINE.                             
01482          16  FILLER              PIC  X(60).                      
01483          16  T-LC1               PIC  Z,ZZZ,ZZZ.99-.              
01484          16  FILLER              PIC  X(3).                       
01485          16  T-AP1               PIC  Z,ZZZ,ZZZ.99-.              
01486          16  FILLER              PIC  X(6).                       
01487          16  T-AC1               PIC  Z,ZZZ,ZZZ.99-.              
01488          16  T-TF1               PIC  ZZZZ,ZZZ.99-.               
01489          16  T-TP1               PIC  ZZZZ,ZZZ.99-.               
01490      12  P-LINE-7  REDEFINES  P-LINE.                             
010616         16  FILLER              PIC  X(14).                      
010616         16  p-check-line        pic  x(105).
010616*        16  P-CHK-PD-TO         PIC  X(23).                      
010616*        16  P-CHK-PAYEE         PIC  X(30).                      
010616*        16  P-BY-CK-NO          PIC  X(15).                      
010616*        16  P-CHECK-NUM         PIC  X(7).                       
010616*        16  P-CHECK-DATED       PIC  X(7).                       
010616*        16  P-CHECK-MO          PIC  XX.                         
010616*        16  P-CHECK-MOD         PIC  X.                          
010616*        16  P-CHECK-DA          PIC  XX.                         
010616*        16  P-CHECK-DAD         PIC  X.                          
010616*        16  P-CHECK-YR          PIC  XX.                         
010616*        16  FILLER              PIC  X(16).                      
01503      12  P-LINE-8  REDEFINES  P-LINE.                             
01504          16  FILLER              PIC  X(51).                      
01505          16  P-CLAIM-L-A         PIC  X(06).                      
01506          16  FILLER              PIC  X.                          
01507          16  P-CLAIM-MSG         PIC  X(28).                      
01508          16  FILLER              PIC  X(3).                       
01509          16  P-CLAIM-AMT         PIC  ZZZZ,ZZZ.ZZ-.               
01510          16  FILLER              PIC  X(9).                       
01511          16  P-CLAIM-MEMBER      PIC  X(12).                      
01512          16  FILLER              PIC  X(10).                      
01513      12  P-LINE-9  REDEFINES  P-LINE.                             
01514          16  FILLER              PIC  X(15).                      
01515          16  P-LINE-9-DESC       PIC  X(58).                      
01516          16  P-LINE-9-REMIT  REDEFINES  P-LINE-9-DESC.            
01517              20  P-LINE-9-PAYABLE                                 
01518                                  PIC  X(28).                      
01519              20  P-LINE-9-NAME                                    
01520                                  PIC  X(30).                      
01521          16  FILLER              PIC  X(59).                      
01522      12  P-LINE-10-KSM  REDEFINES  P-LINE.                        
01523          16  FILLER              PIC  X(49).                      
01524          16  P-KSM-DESC          PIC  X(30).                      
01525          16  FILLER              PIC  X(12).                      
01526          16  P-KSM-AMT           PIC  ZZZZ,ZZZ.99-.               
01527          16  FILLER              PIC  X(29).                      
01528      12  P-LINE-11-KSM  REDEFINES  P-LINE.                        
01529          16  FILLER              PIC  X(49).                      
01530          16  FILLER              PIC  X(30).                      
01531          16  FILLER              PIC  X(18).                      
01532          16  P-KSM-CNT           PIC  Z,ZZZ,ZZZ-.                 
01533          16  FILLER              PIC  X(25).                      
01534  EJECT                                                            
01535  01  SUMMARY-DESC-1              PIC  X(58)      VALUE            
01536      'VALID BUSINESS ON THIS STATEMENT IS REPRESENTED BY THE   '. 
01537  01  SUMMARY-DESC-2              PIC  X(58)      VALUE            
01538      'TOTAL OF THE PROCESSED COLUMN. THE NON-PROCESSED BUSINESS'. 
01539  01  SUMMARY-DESC-2              PIC  X(58)      VALUE            
01540      'WILL APPEAR ON SUBSEQUENT STATEMENTS.                    '. 
01541  01  KSM-SUMMARY-DESC-1          PIC  X(58)      VALUE            
01542      'WE APPRECIATE YOUR BUSINESS AND PROMPT PAYMENT            '.
01543  01  KSM-SUMMARY-DESC-2          PIC  X(58)      VALUE            
01544      'STATEMENT IS DUE UPON RECEIPT. PLEASE PAY AS BILLED.      '.
01545  01  KSM-SUMMARY-DESC-3          PIC  X(58)      VALUE            
01546      'CERTIFICATE TRANSACTIONS, INCLUDING REFUNDS NOT APPEARING '.
01547  01  KSM-SUMMARY-DESC-4          PIC  X(58)      VALUE            
01548      'ON THIS STATEMENT WILL APPEAR ON THE NEXT MONTHLY BILLING.'.
01549  01  KSM-SUMMARY-DESC-5          PIC  X(28)      VALUE            
01550      'MAKE CHECKS PAYABLE TO:     '.                              
01551  EJECT                                                            
01552  01  ISSUE-TOT-LINE.                                              
01553      12  FILLER              PIC  X(12)          VALUE            
01554              '**** TOTALS '.                                      
01555      12  LNB-L-DESC          PIC  X(06)          VALUE            
01556              '      '.                                            
01557      12  FILLER              PIC  X(03)          VALUE            
01558              ' = '.                                               
01559      12  LNB-LISS            PIC  ZZ,ZZZ-.                        
01560      12  FILLER              PIC  X(8)           VALUE            
01561              ' PREM = '.                                          
01562      12  LNB-LPRM            PIC  ZZ,ZZZ,ZZZ.ZZ-.                 
01563      12  FILLER              PIC  X(8)           VALUE            
01564              ' COMP = '.                                          
01565      12  LNB-LCOM            PIC  ZZ,ZZZ,ZZZ.ZZ-.                 
01566      12  LNB-A-DESC          PIC  X(6)           VALUE            
01567              '      '.                                            
01568      12  FILLER              PIC  X(3)           VALUE            
01569              ' = '.                                               
01570      12  LNB-AISS            PIC  ZZ,ZZZ-.                        
01571      12  FILLER              PIC  X(8)           VALUE            
01572              ' PREM = '.                                          
01573      12  LNB-APRM            PIC  ZZ,ZZZ,ZZZ.ZZ-.                 
01574      12  FILLER              PIC  X(8)           VALUE            
01575              ' COMP = '.                                          
01576      12  LNB-ACOM            PIC  ZZ,ZZZ,ZZZ.ZZ-.                 
01577                                                                   
01578  01  ISSUE-TOT-LIFE-NET.                                          
01579      12  FILLER              PIC  X(12)          VALUE            
01580              '**** TOTALS '.                                      
01581      12  LNB-L-DESC-NL       PIC  X(06)          VALUE            
01582              '      '.                                            
01583      12  FILLER              PIC  X(03)          VALUE            
01584              ' = '.                                               
01585      12  FILLER              PIC  X(03)          VALUE SPACES.    
01586      12  LNB-LISS-NL         PIC  ZZ,ZZZ-.                        
01587      12  FILLER              PIC  X(40)          VALUE SPACES.    
01588      12  LNB-LPRM-NL         PIC  ZZ,ZZZ,ZZZ.ZZ-.                 
01589      12  FILLER              PIC  X(20)          VALUE SPACES.    
01590      12  LNB-LCOM-NL         PIC  ZZ,ZZZ,ZZZ.ZZ-.                 
01591                                                                   
01592  01  ISSUE-TOT-AH-NET.                                            
01593      12  FILLER              PIC  X(12)          VALUE            
01594              '**** TOTALS '.                                      
01595      12  LNB-A-DESC-NA       PIC  X(06)          VALUE            
01596              '      '.                                            
01597      12  FILLER              PIC  X(03)          VALUE            
01598              ' = '.                                               
01599      12  FILLER              PIC  X(03)          VALUE SPACES.    
01600      12  LNB-AISS-NA         PIC  ZZ,ZZZ-.                        
01601      12  FILLER              PIC  X(40)          VALUE SPACES.    
01602      12  LNB-APRM-NA         PIC  ZZ,ZZZ,ZZZ.ZZ-.                 
01603      12  FILLER              PIC  X(20)          VALUE SPACES.    
01604      12  LNB-ACOM-NA         PIC  ZZ,ZZZ,ZZZ.ZZ-.                 
01605      12  LNB-NET-NA          PIC  Z,ZZZ,ZZZ.ZZ-.                  
01606                                                                   
01607  01  CANCEL-TOTAL-LINE.                                           
01608      12  FILLER              PIC  X(12)          VALUE            
01609              '**** TOTALS '.                                      
01610      12  ANB-L-DESC          PIC  X(06)          VALUE            
01611              '      '.                                            
01612      12  FILLER              PIC  X(03)          VALUE            
01613              ' = '.                                               
01614      12  ANB-LCAN            PIC  ZZ,ZZZ-.                        
01615      12  ANB-A-DESC          PIC  X(6)           VALUE            
01616              '      '.                                            
01617      12  FILLER              PIC  X(3)           VALUE            
01618              ' = '.                                               
01619      12  ANB-ACAN            PIC  ZZ,ZZZ-.                        
01620      12  FILLER              PIC  X(5)           VALUE SPACES.    
01621      12  ANB-LREF            PIC  ZZ,ZZZ,ZZZ.ZZ-.                 
01622      12  FILLER              PIC  X(14)          VALUE SPACES.    
01623      12  ANB-AREF            PIC  ZZ,ZZZ,ZZZ.ZZ-.                 
01624      12  FILLER              PIC  X(13)          VALUE SPACES.    
01625      12  ANB-LCOM            PIC  ZZ,ZZZ,ZZZ.ZZ-.                 
01626      12  ANB-ACOM            PIC  ZZ,ZZZ,ZZZ.ZZ-.                 
01627  01  CANCEL-TOTAL-LINE-LIFE-NET.                                  
01628      12  FILLER              PIC  X(12)          VALUE            
01629              '**** TOTALS '.                                      
01630      12  ANB-L-DESC-NL       PIC  X(06)          VALUE            
01631              '      '.                                            
01632      12  FILLER              PIC  X(03)          VALUE            
01633              ' = '.                                               
01634      12  ANB-LCAN-NL         PIC  ZZ,ZZZ-.                        
01635      12  FILLER              PIC  X(38)          VALUE SPACES.    
01636      12  ANB-LREF-NL         PIC  ZZ,ZZZ,ZZZ.ZZ-.                 
01637      12  FILLER              PIC  X(23)          VALUE SPACES.    
01638      12  ANB-LCOM-NL         PIC  ZZ,ZZZ,ZZZ.ZZ-.                 
01639                                                                   
01640  01  CANCEL-TOTAL-LINE-AH-NET.                                    
01641      12  FILLER              PIC  X(12)          VALUE            
01642              '**** TOTALS '.                                      
01643      12  ANB-A-DESC-NA       PIC  X(06)          VALUE            
01644              '      '.                                            
01645      12  FILLER              PIC  X(03)          VALUE            
01646              ' = '.                                               
01647      12  ANB-ACAN-NA         PIC  ZZ,ZZZ-.                        
01648      12  FILLER              PIC  X(38)          VALUE SPACES.    
01649      12  ANB-AREF-NA         PIC  ZZ,ZZZ,ZZZ.ZZ-.                 
01650      12  FILLER              PIC  X(23)          VALUE SPACES.    
01651      12  ANB-ACOM-NA         PIC  ZZ,ZZZ,ZZZ.ZZ-.                 
01652      12  FILLER              PIC  X(2)           VALUE SPACES.    
01653      12  ANB-NET-DUE         PIC  Z,ZZZ,ZZZ.ZZ-.                  
01654                                                                   
01655  01  CLAIM-TOTAL-LINE.                                            
01656      12  FILLER              PIC  X(28)          VALUE            
01657              'CLAIM PAYMENT TOTALS  '.                            
01658      12  CTL-DTH-CLM         PIC  ZZ,ZZZ-.                        
01659      12  FILLER              PIC  XX             VALUE SPACES.    
01660      12  CTL-L-DESC          PIC  X(06)          VALUE            
01661              '      '.                                            
01662      12  FILLER              PIC  X(08)          VALUE            
01663              '  =     '.                                          
01664      12  CTL-L-CLM           PIC  ZZ,ZZZ,ZZZ.ZZ-.                 
01665      12  FILLER              PIC  X(5)           VALUE SPACES.    
01666      12  CTL-DIS-CLM         PIC  ZZ,ZZZ-.                        
01667      12  FILLER              PIC  XX             VALUE SPACES.    
01668      12  CTL-A-DESC          PIC  X(06)          VALUE            
01669              '      '.                                            
01670      12  FILLER              PIC  X(06)          VALUE            
01671              '  =   '.                                            
01672      12  CTL-A-CLM           PIC  ZZ,ZZZ,ZZZ.ZZ-.                 
01673      12  FILLER              PIC  X(14)          VALUE            
01674              '      TOTAL = '.                                    
01675      12  CTL-T-CLM           PIC  ZZ,ZZZ,ZZZ.ZZ-.                 
01676  EJECT                                                            
01677  01  SPACE-COUNT                   PIC  S9       VALUE +0.        
01678  01  SHIFT-COUNT                   PIC  S9       VALUE +0.        
01679                                                                   
01680  01  SUMM-DESC.                                                   
01681      12  SUMM-DESC-POS-6.                                         
01682          16  SUMM-DESC-PT1-6       PIC  X(6).                     
01683          16  FILLER                PIC  X.                        
01684          16  SUMM-DESC-PT2-6       PIC  X(18).                    
01685      12  SUMM-DESC-POS-5 REDEFINES SUMM-DESC-POS-6.               
01686          16  SUMM-DESC-PT1-5       PIC  X(5).                     
01687          16  FILLER                PIC  X.                        
01688          16  SUMM-DESC-PT2-5       PIC  X(19).                    
01689      12  SUMM-DESC-POS-4 REDEFINES SUMM-DESC-POS-6.               
01690          16  SUMM-DESC-PT1-4       PIC  X(4).                     
01691          16  FILLER                PIC  X.                        
01692          16  SUMM-DESC-PT2-4       PIC  X(20).                    
01693      12  SUMM-DESC-POS-3 REDEFINES SUMM-DESC-POS-6.               
01694          16  SUMM-DESC-PT1-3       PIC  X(3).                     
01695          16  FILLER                PIC  X.                        
01696          16  SUMM-DESC-PT2-3       PIC  X(21).                    
01697      12  SUMM-DESC-POS-2 REDEFINES SUMM-DESC-POS-6.               
01698          16  SUMM-DESC-PT1-2       PIC  X(2).                     
01699          16  FILLER                PIC  X.                        
01700          16  SUMM-DESC-PT2-2       PIC  X(22).                    
01701      12  SUMM-DESC-POS-1 REDEFINES SUMM-DESC-POS-6.               
01702          16  SUMM-DESC-PT1-1       PIC  X(1).                     
01703          16  FILLER                PIC  X.                        
01704          16  SUMM-DESC-PT2-1       PIC  X(23).                    
01705                                                                   
01706  01  L-A-DESC.                                                    
01707      12  L-A-DESC-1                PIC  X.                        
01708      12  L-A-DESC-2                PIC  X.                        
01709      12  L-A-DESC-3                PIC  X.                        
01710      12  L-A-DESC-4                PIC  X.                        
01711      12  L-A-DESC-5                PIC  X.                        
01712      12  L-A-DESC-6                PIC  X.                        
01713                                                                   
01714  01  T-DESC-PRM-L                  PIC  X(25).                    
01715  01  T-DESC-REF-L                  PIC  X(25).                    
01716  01  T-DESC-NET-L                  PIC  X(25).                    
01717  01  T-DESC-COM-L                  PIC  X(25).                    
01718  01  T-DESC-PRM-A                  PIC  X(25).                    
01719  01  T-DESC-REF-A                  PIC  X(25).                    
01720  01  T-DESC-NET-A                  PIC  X(25).                    
01721  01  T-DESC-COM-A                  PIC  X(25).                    
01722                                                                   
01723  EJECT                                                            
122002                             COPY ECSCOM01.                      
01725  EJECT                                                            
01726                              COPY ELCBILVR.                       
01727  EJECT                                                            
01728                              COPY ERCCOMP.                        
01729  EJECT                                                            
01730                              COPY ELCDATE.                        
01731  EJECT                                                            
01732                              COPY ELCDTECX.                       
01733  EJECT                                                            
01734                              COPY ELCDTEVR.                       
01735  EJECT                                                            
01736                              COPY ELCERRWS.                       
01737  EJECT                                                            
020711
020711 LINKAGE SECTION.
020711
020711 01  PARM.
020711     05  PARM-LENGTH        COMP     PIC S9(04)    VALUE ZEROS.
020711     05  PARM-UPDATE-ERNOTE          PIC X(06)     VALUE SPACES.
020711
020711 PROCEDURE DIVISION USING PARM.
020711
020711     IF PARM-UPDATE-ERNOTE = 'UPDATE'
020711        DISPLAY 'TABLE ERNOTE WILL BE UPDATED'
020711     END-IF.
01739                                                                   
01740  0000-DATE-CARD-READ SECTION.                                     
01741                              COPY ELCDTERX.                       
01742                                                                   
070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
070714****                                                           ***
070714****   Set up the month-end auto balancing.                    ***
070714****                                                           ***
070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***

070714     MOVE WS-TIME                TO ME-START-TIME
070714     MOVE WS-CURRENT-DATE        TO ME-START-DATE
070714     MOVE ME-START-MO            TO ME-CNDS-MO
070714     MOVE ME-START-DA            TO ME-CNDS-DA
070714     MOVE ME-START-YR            TO ME-CNDS-YR

01743      IF DTE-PGM-OPT GREATER ZERO AND LESS 8                       
01744          NEXT SENTENCE                                            
01745      ELSE                                                         
01746          MOVE 1                  TO  DTE-PGM-OPT.                 
01747                                                                   
01748      IF DTE-CLIENT = 'TIC'                                        
01749          MOVE 'MEMBER'           TO  HD6D-FILL                    
01750          MOVE 'NUMBER'           TO  HD7D-FILL.                   

032905*    IF DTE-CLIENT = 'DCC' or 'CAP'
032905*       MOVE ' CODE '            TO HD7A-BEN-HD
032905*    END-IF

01751                                                                   
01752      IF DTE-CLIENT = 'KSM'                                        
01753          MOVE +40                TO  LNCTR-25-40                  
01754          MOVE +49                TO  LNCTR-34-49                  
01755          MOVE +58                TO  LNCTR-43-58                  
01756          MOVE SPACES             TO  HD6A-COMP                    
01757                                      HD7A-PCT                     
01758          MOVE RUN-CC             TO  SAV-KSM-CC                   
01759          MOVE RUN-YR             TO  SAV-KSM-YY                   
01760          MOVE RUN-MO             TO  SAV-KSM-MM                   
01761          MOVE RUN-DA             TO  SAV-KSM-DD                   
01762      ELSE                                                         
01763          MOVE +25                TO  LNCTR-25-40                  
01764          MOVE +34                TO  LNCTR-34-49                  
01765          MOVE +43                TO  LNCTR-43-58.                 
01766                                                                   
01767  EJECT                                                            
01768  0100-MAIN-LOGIC SECTION.                                         
01769      SORT SORT-WORK                                               
01770          ASCENDING KEY  SRT-KEY                                   
01771          INPUT PROCEDURE 0200-INPUT-ROUTINE   THRU 2999-EXIT      
01772          OUTPUT PROCEDURE 3000-OUTPUT-ROUTINE THRU 9099-EXIT.     
01773                                                                   
01774      IF SORT-RETURN NOT = ZERO                                    
01775          MOVE 'SORT FAILED'      TO  WS-ABEND-MESSAGE             
01776          MOVE SORT-RETURN        TO  WS-RETURN-CODE               
01777          GO TO ABEND-PGM.                                         
01778                                                                   
01779      GOBACK.                                                      
01780  EJECT                                                            
01781  0200-INPUT-ROUTINE SECTION.                                      
01782      OPEN INPUT  COMM-TRAN-IN   COMM-MSTR-IN   ERPNDE             
092815                 ELERRS         ERCHEK  ELCRTT
01784           OUTPUT SUMM-TRAN-OUT  COMM-MSTR-OUT  PRNTR.   
020711     IF PARM-UPDATE-ERNOTE = 'UPDATE'          
020711          OPEN I-O    ERNOTE
020711     ELSE
020711          OPEN INPUT ERNOTE
020711     END-IF.
01785                                                                   
01786      IF ERPNDE-FILE-STATUS  = '00' OR '97'                        
01787          NEXT SENTENCE                                            
01788      ELSE                                                         
01789          MOVE 'ERROR OCCURED OPEN  - ERPNDE'                      
01790                                   TO  WS-ABEND-MESSAGE            
01791          MOVE ERPNDE-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        
01792          GO TO ABEND-PGM.                                         
01793                                                                   
01794      IF EM-STATUS = '00' OR '97'                                  
01795          NEXT SENTENCE                                            
01796      ELSE                                                         
01797          MOVE EM-STATUS          TO  WS-ABEND-FILE-STATUS         
01798          MOVE 'ERROR OCCURED OPEN - ELERRS'                       
01799                                  TO  WS-ABEND-MESSAGE             
01800          GO TO ABEND-PGM.                                         
01801                                                                   
01802      IF ERNOTE-FILE-STATUS  = '00' OR '97'                        
01803          NEXT SENTENCE                                            
01804      ELSE                                                         
01805          MOVE 'ERROR OCCURED OPEN  - ERNOTE'                      
01806                                   TO  WS-ABEND-MESSAGE            
01807          MOVE ERNOTE-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        
01808          GO TO ABEND-PGM.                                         

092815     IF ELCRTT-FILE-STATUS  = '00' OR '97'
092815        CONTINUE
092815     ELSE
092815        MOVE 'ERROR OCCURED OPEN  - ELCRTT'
092815                                 TO WS-ABEND-MESSAGE
092815        MOVE ELCRTT-FILE-STATUS  TO WS-ABEND-FILE-STATUS
092815        GO TO ABEND-PGM
092815     END-IF

01810      IF ERCHEK-FILE-STATUS  = '00' OR '97'                        
01811          NEXT SENTENCE                                            
01812      ELSE                                                         
01813          MOVE 'ERROR OCCURED OPEN  - ERCHEK'                      
01814                                   TO  WS-ABEND-MESSAGE            
01815          MOVE ERCHEK-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        
01816          GO TO ABEND-PGM.                                         
01817                                                                   
CIDMOD*    IF DTE-CLIENT = 'KSM' OR 'CID'                               
01819 *       OPEN INPUT ERACCT                                         
01820 *       IF ERACCT-FILE-STATUS  NOT = '00' AND '97'                
01821 *          MOVE 'ERROR OCCURED OPEN  - ERACCT'                    
01822 *                                TO  WS-ABEND-MESSAGE             
01823 *          MOVE ERACCT-FILE-STATUS                                
01824 *                                TO  WS-ABEND-FILE-STATUS         
CIDMOD*          PERFORM ABEND-PGM                                      
CIDMOD*       END-IF
CIDMOD*    END-IF
01826 *            ELSE                                                 
01827 *                PERFORM 6010-CLEAR-BILLING-SUM-RECORD            
01828 *                        THRU  6099-EXIT                          
01829 *                PERFORM 6575-CLEAR-BILLING-DET-RECORD            
01830 *                        THRU  6599-EXIT                          
01831 *                OPEN OUTPUT BILLING-DATA-FILE.                   
01832                                                                   
01833      MOVE LOW-VALUES             TO  COMPENSATION-MASTER          
01834                                      COMP-IN-RECORD               
01835                                      COMP-OUT-RECORD              
01836                                      CUR-CP-SEQ                   
01837                                      PRV-SRT-KEY-A                
01838                                      PRE-CONTROL.                 
01839                                                                   
01840      MOVE COMPANY-NAME           TO  SAVE-COMPANY-NAME (1)        
01841                                      SAVE-COMPANY-NAME (2)        
01842                                      SAVE-COMPANY-NAME (3).       
01843  EJECT                                                            
01844      MOVE BIN-RUN-DATE           TO  WS-RUN-DT.                   
01845      MOVE ALPH-DATE              TO  HD-DT.                       
01846                                                                   
01847      ACCEPT WS-ACCEPT-DATE       FROM  DATE.                      
01848                                                                   
01849      MOVE WS-AD-YY               TO  WS-CD-YY.                    
01850      MOVE WS-AD-MM               TO  WS-CD-MM.                    
01851      MOVE WS-AD-DD               TO  WS-CD-DD.                    
01852      MOVE WS-CURRENT-DATE        TO  HD-RUN-DT.                   
020711
020711     MOVE WS-ACCEPT-DATE         TO  DC-GREG-DATE-1-YMD.
020711     MOVE '3'                    TO  DC-OPTION-CODE.
020711     PERFORM 1900-DATE-CONVERSION  THRU  1999-EXIT.          
020711     MOVE DC-BIN-DATE-1          TO  WS-CURR-BIN-DT.               
020711     ACCEPT WS-TIME-OF-DAY       FROM TIME.
020711     MOVE WS-TIME                TO  WS-CURR-TIME.
020711
01853      MOVE LIFE-OVERRIDE-L6       TO  HD6B-1-LF                    
01854                                      HD7B-LF                      
01855                                      LNB-L-DESC                   
01856                                      LNB-L-DESC-NL                
01857                                      ANB-L-DESC                   
01858                                      ANB-L-DESC-NL                
01859                                      CTL-L-DESC.                  
01860      MOVE AH-OVERRIDE-L6         TO  HD6B-1-AH                    
01861                                      HD7B-AH                      
01862                                      LNB-A-DESC                   
01863                                      LNB-A-DESC-NA                
01864                                      ANB-A-DESC                   
01865                                      ANB-A-DESC-NA                
01866                                      CTL-A-DESC.                  
01867                                                                   
01868      IF DTE-CLIENT = 'BWS'                                        
01869          MOVE HD6A-1             TO  HD6A                         
01870          MOVE HD7A-1             TO  HD7A                         
01871          MOVE HD6B-3             TO  HD6B-1                       
01872          MOVE HD7B-3             TO  HD7B-1                       
01873          MOVE HD7B-3             TO  HD7B-1                       
01874      ELSE                                                         
01875          IF DTE-FMT-OPT = '2'                                     
01876              MOVE HD6B-2         TO  HD6B-1                       
01877              MOVE HD7B-2         TO  HD7B-1.                      
01878                                                                   
01879      MOVE SPACES                 TO  RMTX.                        
01880      MOVE CNT-NAME (1)           TO  RMT1.                        
01881      MOVE CNT-NAME (2)           TO  RMT2.                        
01882      MOVE CNT-NAME (3)           TO  RMT3.                        
01883      MOVE CNT-NAME (4)           TO  RMT4.                        
01884      MOVE CNT-NAME (5)           TO  TELE-ZIP-LINE.               
01885      MOVE SPACES                 TO  RMT5.                        
01886      MOVE LINE5-ZIP-CODE         TO  RMT-ZIP.                     
01887                                                                   
01888      MOVE LIFE-OVERRIDE-L6       TO  L-A-DESC.                    
01889                                                                   
01890  0200-SHIFT-L.                                                    
01891      ADD +1 TO SHIFT-COUNT.                                       
01892                                                                   
01893      IF SHIFT-COUNT LESS THAN +6                                  
01894          IF L-A-DESC-1 = SPACE                                    
01895              MOVE L-A-DESC-2         TO  L-A-DESC-1               
01896              MOVE L-A-DESC-3         TO  L-A-DESC-2               
01897              MOVE L-A-DESC-4         TO  L-A-DESC-3               
01898              MOVE L-A-DESC-5         TO  L-A-DESC-4               
01899              MOVE L-A-DESC-6         TO  L-A-DESC-5               
01900              MOVE SPACE              TO  L-A-DESC-6               
01901              GO TO 0200-SHIFT-L.                                  
01902                                                                   
01903      IF L-A-DESC-6 EQUAL SPACE                                    
01904          ADD +1 TO SPACE-COUNT.                                   
01905                                                                   
01906      IF L-A-DESC-5 EQUAL SPACE                                    
01907          ADD +1 TO SPACE-COUNT.                                   
01908                                                                   
01909      IF L-A-DESC-4 EQUAL SPACE                                    
01910          ADD +1 TO SPACE-COUNT.                                   
01911                                                                   
01912      IF L-A-DESC-3 EQUAL SPACE                                    
01913          ADD +1 TO SPACE-COUNT.                                   
01914                                                                   
01915      IF L-A-DESC-2 EQUAL SPACE                                    
01916          ADD +1 TO SPACE-COUNT.                                   
01917                                                                   
01918      IF SPACE-COUNT = +0                                          
01919          MOVE L-A-DESC               TO  SUMM-DESC-PT1-6          
01920          MOVE 'PREMIUM'              TO  SUMM-DESC-PT2-6          
01921          MOVE SUMM-DESC              TO  T-DESC-PRM-L             
01922          MOVE 'REFUNDS'              TO  SUMM-DESC-PT2-6          
01923          MOVE SUMM-DESC              TO  T-DESC-REF-L             
01924          MOVE 'NET PREMIUMS'         TO  SUMM-DESC-PT2-6          
01925          MOVE SUMM-DESC              TO  T-DESC-NET-L             
01926          MOVE 'COMPENSATION'         TO  SUMM-DESC-PT2-6          
01927          MOVE SUMM-DESC              TO  T-DESC-COM-L.            
01928                                                                   
01929      IF SPACE-COUNT = +1                                          
01930          MOVE L-A-DESC               TO  SUMM-DESC-PT1-5          
01931          MOVE 'PREMIUM'              TO  SUMM-DESC-PT2-5          
01932          MOVE SUMM-DESC              TO  T-DESC-PRM-L             
01933          MOVE 'REFUNDS'              TO  SUMM-DESC-PT2-5          
01934          MOVE SUMM-DESC              TO  T-DESC-REF-L             
01935          MOVE 'NET PREMIUMS'         TO  SUMM-DESC-PT2-5          
01936          MOVE SUMM-DESC              TO  T-DESC-NET-L             
01937          MOVE 'COMPENSATION'         TO  SUMM-DESC-PT2-5          
01938          MOVE SUMM-DESC              TO  T-DESC-COM-L.            
01939                                                                   
01940      IF SPACE-COUNT = +2                                          
01941          MOVE L-A-DESC               TO  SUMM-DESC-PT1-4          
01942          MOVE 'PREMIUM'              TO  SUMM-DESC-PT2-4          
01943          MOVE SUMM-DESC              TO  T-DESC-PRM-L             
01944          MOVE 'REFUNDS'              TO  SUMM-DESC-PT2-4          
01945          MOVE SUMM-DESC              TO  T-DESC-REF-L             
01946          MOVE 'NET PREMIUMS'         TO  SUMM-DESC-PT2-4          
01947          MOVE SUMM-DESC              TO  T-DESC-NET-L             
01948          MOVE 'COMPENSATION'         TO  SUMM-DESC-PT2-4          
01949          MOVE SUMM-DESC              TO  T-DESC-COM-L.            
01950                                                                   
01951      IF SPACE-COUNT = +3                                          
01952          MOVE L-A-DESC               TO  SUMM-DESC-PT1-3          
01953          MOVE 'PREMIUM'              TO  SUMM-DESC-PT2-3          
01954          MOVE SUMM-DESC              TO  T-DESC-PRM-L             
01955          MOVE 'REFUNDS'              TO  SUMM-DESC-PT2-3          
01956          MOVE SUMM-DESC              TO  T-DESC-REF-L             
01957          MOVE 'NET PREMIUMS'         TO  SUMM-DESC-PT2-3          
01958          MOVE SUMM-DESC              TO  T-DESC-NET-L             
01959          MOVE 'COMPENSATION'         TO  SUMM-DESC-PT2-3          
01960          MOVE SUMM-DESC              TO  T-DESC-COM-L.            
01961                                                                   
01962      IF SPACE-COUNT = +4                                          
01963          MOVE L-A-DESC               TO  SUMM-DESC-PT1-2          
01964          MOVE 'PREMIUM'              TO  SUMM-DESC-PT2-2          
01965          MOVE SUMM-DESC              TO  T-DESC-PRM-L             
01966          MOVE 'REFUNDS'              TO  SUMM-DESC-PT2-2          
01967          MOVE SUMM-DESC              TO  T-DESC-REF-L             
01968          MOVE 'NET PREMIUMS'         TO  SUMM-DESC-PT2-2          
01969          MOVE SUMM-DESC              TO  T-DESC-NET-L             
01970          MOVE 'COMPENSATION'         TO  SUMM-DESC-PT2-2          
01971          MOVE SUMM-DESC              TO  T-DESC-COM-L.            
01972                                                                   
01973      IF SPACE-COUNT = +5                                          
01974          MOVE L-A-DESC               TO  SUMM-DESC-PT1-1          
01975          MOVE 'PREMIUM'              TO  SUMM-DESC-PT2-1          
01976          MOVE SUMM-DESC              TO  T-DESC-PRM-L             
01977          MOVE 'REFUNDS'              TO  SUMM-DESC-PT2-1          
01978          MOVE SUMM-DESC              TO  T-DESC-REF-L             
01979          MOVE 'NET PREMIUMS'         TO  SUMM-DESC-PT2-1          
01980          MOVE SUMM-DESC              TO  T-DESC-NET-L             
01981          MOVE 'COMPENSATION'         TO  SUMM-DESC-PT2-1          
01982          MOVE SUMM-DESC              TO  T-DESC-COM-L.            
01983                                                                   
01984      MOVE +0                     TO  SPACE-COUNT                  
01985                                      SHIFT-COUNT.                 
01986      MOVE SPACES                 TO  SUMM-DESC.                   
01987      MOVE AH-OVERRIDE-L6         TO  L-A-DESC.                    
01988                                                                   
01989  0200-SHIFT-A.                                                    
01990      ADD +1 TO SHIFT-COUNT.                                       
01991                                                                   
01992      IF SHIFT-COUNT LESS THAN +6                                  
01993          IF L-A-DESC-1 = SPACE                                    
01994              MOVE L-A-DESC-2         TO  L-A-DESC-1               
01995              MOVE L-A-DESC-3         TO  L-A-DESC-2               
01996              MOVE L-A-DESC-4         TO  L-A-DESC-3               
01997              MOVE L-A-DESC-5         TO  L-A-DESC-4               
01998              MOVE L-A-DESC-6         TO  L-A-DESC-5               
01999              MOVE SPACE              TO  L-A-DESC-6               
02000              GO TO 0200-SHIFT-A.                                  
02001                                                                   
02002      IF L-A-DESC-6 EQUAL SPACE                                    
02003          ADD +1 TO SPACE-COUNT.                                   
02004                                                                   
02005      IF L-A-DESC-5 EQUAL SPACE                                    
02006          ADD +1 TO SPACE-COUNT.                                   
02007                                                                   
02008      IF L-A-DESC-4 EQUAL SPACE                                    
02009          ADD +1 TO SPACE-COUNT.                                   
02010                                                                   
02011      IF L-A-DESC-3 EQUAL SPACE                                    
02012          ADD +1 TO SPACE-COUNT.                                   
02013                                                                   
02014      IF L-A-DESC-2 EQUAL SPACE                                    
02015          ADD +1 TO SPACE-COUNT.                                   
02016                                                                   
02017      IF SPACE-COUNT = +0                                          
02018          MOVE L-A-DESC               TO  SUMM-DESC-PT1-6          
02019          MOVE 'PREMIUM'              TO  SUMM-DESC-PT2-6          
02020          MOVE SUMM-DESC              TO  T-DESC-PRM-A             
02021          MOVE 'REFUNDS'              TO  SUMM-DESC-PT2-6          
02022          MOVE SUMM-DESC              TO  T-DESC-REF-A             
02023          MOVE 'NET PREMIUMS'         TO  SUMM-DESC-PT2-6          
02024          MOVE SUMM-DESC              TO  T-DESC-NET-A             
02025          MOVE 'COMPENSATION'         TO  SUMM-DESC-PT2-6          
02026          MOVE SUMM-DESC              TO  T-DESC-COM-A.            
02027                                                                   
02028      IF SPACE-COUNT = +1                                          
02029          MOVE L-A-DESC               TO  SUMM-DESC-PT1-5          
02030          MOVE 'PREMIUM'              TO  SUMM-DESC-PT2-5          
02031          MOVE SUMM-DESC              TO  T-DESC-PRM-A             
02032          MOVE 'REFUNDS'              TO  SUMM-DESC-PT2-5          
02033          MOVE SUMM-DESC              TO  T-DESC-REF-A             
02034          MOVE 'NET PREMIUMS'         TO  SUMM-DESC-PT2-5          
02035          MOVE SUMM-DESC              TO  T-DESC-NET-A             
02036          MOVE 'COMPENSATION'         TO  SUMM-DESC-PT2-5          
02037          MOVE SUMM-DESC              TO  T-DESC-COM-A.            
02038                                                                   
02039      IF SPACE-COUNT = +2                                          
02040          MOVE L-A-DESC               TO  SUMM-DESC-PT1-4          
02041          MOVE 'PREMIUM'              TO  SUMM-DESC-PT2-4          
02042          MOVE SUMM-DESC              TO  T-DESC-PRM-A             
02043          MOVE 'REFUNDS'              TO  SUMM-DESC-PT2-4          
02044          MOVE SUMM-DESC              TO  T-DESC-REF-A             
02045          MOVE 'NET PREMIUMS'         TO  SUMM-DESC-PT2-4          
02046          MOVE SUMM-DESC              TO  T-DESC-NET-A             
02047          MOVE 'COMPENSATION'         TO  SUMM-DESC-PT2-4          
02048          MOVE SUMM-DESC              TO  T-DESC-COM-A.            
02049                                                                   
02050      IF SPACE-COUNT = +3                                          
02051          MOVE L-A-DESC               TO  SUMM-DESC-PT1-3          
02052          MOVE 'PREMIUM'              TO  SUMM-DESC-PT2-3          
02053          MOVE SUMM-DESC              TO  T-DESC-PRM-A             
02054          MOVE 'REFUNDS'              TO  SUMM-DESC-PT2-3          
02055          MOVE SUMM-DESC              TO  T-DESC-REF-A             
02056          MOVE 'NET PREMIUMS'         TO  SUMM-DESC-PT2-3          
02057          MOVE SUMM-DESC              TO  T-DESC-NET-A             
02058          MOVE 'COMPENSATION'         TO  SUMM-DESC-PT2-3          
02059          MOVE SUMM-DESC              TO  T-DESC-COM-A.            
02060                                                                   
02061      IF SPACE-COUNT = +4                                          
02062          MOVE L-A-DESC               TO  SUMM-DESC-PT1-2          
02063          MOVE 'PREMIUM'              TO  SUMM-DESC-PT2-2          
02064          MOVE SUMM-DESC              TO  T-DESC-PRM-A             
02065          MOVE 'REFUNDS'              TO  SUMM-DESC-PT2-2          
02066          MOVE SUMM-DESC              TO  T-DESC-REF-A             
02067          MOVE 'NET PREMIUMS'         TO  SUMM-DESC-PT2-2          
02068          MOVE SUMM-DESC              TO  T-DESC-NET-A             
02069          MOVE 'COMPENSATION'         TO  SUMM-DESC-PT2-2          
02070          MOVE SUMM-DESC              TO  T-DESC-COM-A.            
02071                                                                   
02072      IF SPACE-COUNT = +5                                          
02073          MOVE L-A-DESC               TO  SUMM-DESC-PT1-1          
02074          MOVE 'PREMIUM'              TO  SUMM-DESC-PT2-1          
02075          MOVE SUMM-DESC              TO  T-DESC-PRM-A             
02076          MOVE 'REFUNDS'              TO  SUMM-DESC-PT2-1          
02077          MOVE SUMM-DESC              TO  T-DESC-REF-A             
02078          MOVE 'NET PREMIUMS'         TO  SUMM-DESC-PT2-1          
02079          MOVE SUMM-DESC              TO  T-DESC-NET-A             
02080          MOVE 'COMPENSATION'         TO  SUMM-DESC-PT2-1          
02081          MOVE SUMM-DESC              TO  T-DESC-COM-A.            
02082                                                                   
02083 *  SYSTEM REMIT TO                                                
02084      MOVE RMTX                   TO  RMT-LEVEL (1).               
02085                                                                   
02086 *  CARRIER REMIT TO                                               
02087      MOVE RMTX                   TO  RMT-LEVEL (2).               
02088                                                                   
02089 *  COMPANY REMIT TO                                               
02090      MOVE RMTX                   TO  RMT-LEVEL (3).               
02091                                                                   
02092 *  AGENT REMIT TO                                                 
02093      MOVE RMTX                   TO  RMT-LEVEL (4).               
02094                                                                   
02095      IF DTE-WRT-OFF NOT NUMERIC                                   
02096          MOVE ZEROES             TO  DTE-WRT-OFF  VARY-HI         
02097                                      VARY-LO                      
02098      ELSE                                                         
02099          MOVE DTE-WRT-OFF        TO  VARY-HI                      
02100          COMPUTE VARY-LO  =  VARY-HI  *  -1.                      
02101                                                                   
02102      PERFORM 1300-MSTR-CONTROL-RTN  THRU  1399-EXIT.              
02103      PERFORM 1500-RD-CP-RTN         THRU  1599-EXIT.              
02104  EJECT                                                            
02105  0300-PROCESS-RTN.                                                
02106      IF CUR-CP-SEQ LESS CO-CONTROL                                
02107          MOVE COMM-PREM-RECORD   TO  CCM-WK                       
02108          PERFORM 1600-FORM-CP-RTN THRU 1699-EXIT                  
02109          PERFORM 1500-RD-CP-RTN   THRU 1599-EXIT                  
02110          GO TO 0300-PROCESS-RTN.                                  
02111                                                                   
02112      IF CUR-CP-SEQ GREATER CO-CONTROL                             
02113          PERFORM 0700-BUILD-TOTAL-RTN  THRU 0799-EXIT             
02114          PERFORM 1200-UPDATE-MSTR-RTN  THRU 1299-EXIT             
02115          PERFORM 1300-MSTR-CONTROL-RTN THRU 1399-EXIT             
02116          GO TO 0300-PROCESS-RTN.                                  
02117                                                                   
02118      IF CUR-CP-SEQ = HIGH-VALUES                                  
02119          MOVE 'Y'                TO  PENDING-BUSINESS-SWITCH      
02120          PERFORM 2010-RELEASE-SORT-TWO                            
02121          GO TO 2300-READ-PENDING-BUS.                             
02122                                                                   
02123      IF DTE-PGM-OPT NOT = 3                                       
02124          MOVE +1  TO  STMT-SW.                                    
02125                                                                   
02126      PERFORM 0400-BUILD-DETAIL-RTN  THRU  0499-EXIT.              
02127      PERFORM 1500-RD-CP-RTN         THRU  1599-EXIT.              
02128                                                                   
02129      GO TO 0300-PROCESS-RTN.                                      
02130  EJECT                                                            
02131  0400-BUILD-DETAIL-RTN.                                           
02132      PERFORM 0900-CALC-DETAIL-RTN  THRU  0999-EXIT.               
02133                                                                   
02134      IF CP-BILLED                                                 
02135          GO TO 0499-EXIT.                                         
02136                                                                   
02137      MOVE RUN-MO                 TO  CO-ACT-MONTH.                
02138      MOVE RUN-DA                 TO  CO-ACT-DAY.                  
02139      MOVE RUN-YR                 TO  CO-ACT-YEAR.                 
02140                                                                   
02141      IF STMT-SW = +0                                              
02142          GO TO 0499-EXIT.                                         
02143                                                                   
02144      MOVE CP-CARRIER             TO  WS-SRT-CARR.                 
02145      MOVE CP-GROUPING            TO  WS-SRT-GROUP.                
02146      MOVE CP-ACCOUNT             TO  WS-SRT-ACCT.                 
02147      MOVE CP-REMIT               TO  WS-SRT-REMIT.                
02148      MOVE CP-EFF                 TO  WS-SRT-LOW-DT.               
02149                                                                   
02150      IF DTE-PGM-OPT = 1  OR  2                                    
02151          MOVE CP-EFF             TO  WS-SRT-LOW-DT                
02152      ELSE                                                         
02153          IF DTE-PGM-OPT = 4  OR  5                                
02154              MOVE CP-CERT        TO  WS-SRT-LOW                   
02155          ELSE                                                     
02156              IF DTE-PGM-OPT = 6  OR  7                            
02157                  MOVE CP-INSUREDS-NAME  TO  WS-SRT-LOW.           
02158                                                                   
02159      MOVE CP-CERT                TO  WS-SRT-CERT.                 
02160      MOVE ZERO                   TO  WS-SRT-REC-SFX.              
02161      MOVE ' '                    TO  WS-SRT-SUPP-REC-TYP.         
02162                                                                   
02163      IF CP-CLAIM                                                  
02164          GO TO 0480-BUILD-CLAIM.                                  
02165                                                                   
02166      IF CP-ACCTG                                                  
02167          GO TO 0470-BUILD-ACCTG.                                  
02168                                                                   
02169      IF CP-CANCEL OR                                              
02170         CP-RC-CANCEL                                              
02171          GO TO 0440-BUILD-CANCEL.                                 
02172  EJECT                                                            
02173  0410-BUILD-ISSUE.                                                
02174      MOVE SPACES                 TO  WS-SRT00-REC.                
02175      MOVE '02 '                  TO  WS-SRT-REC-TYPE.             
02176      MOVE 'Y'                    TO  ACTIVITY-SWITCH.             
02177      MOVE CP-INSUREDS-NAME       TO  WS-SRT02-NAME.               
02178      MOVE CP-CERT                TO  WS-SRT02-CERT.               
02179      MOVE CP-EFF                 TO  WS-SRT02-EFF-DT.             
02180      MOVE CP-AGE                 TO  WS-SRT02-AGE.                
02181      MOVE CP-LF-TERM             TO  WS-SRT02-LF-TERM.            
02182      MOVE OB-SWITCH              TO  WS-SRT02-OB-ON.              
122002     IF CP-MONTHLY-ISSUE
122002        MOVE 'M'                 TO  WS-SRT02-OB-ON
052504     ELSE
052504        IF (DTE-CLIENT = 'DCC' or 'CAP')
                 AND (CP-LF-TERM = 1
                     OR CP-AH-TERM = 1)
                 MOVE '013'            TO WS-SRT-REC-TYPE
              END-IF
122002     END-IF
02183      MOVE 'N'                    TO  OB-SWITCH.                   
02184      MOVE C-TT-FACE              TO  WS-SRT02-FACE.               
02185      MOVE C-TT-FACE-ALT          TO  WS-SRT02-FACE-ALT.           
02186      MOVE C-LF-PREM              TO  WS-SRT02-LF-PREM.            
02187      MOVE C-LF-PREM-ALT          TO  WS-SRT02-LF-PREM-ALT.        
02188      MOVE ZEROS                  TO  WS-SRT02-LF-PRM-PR           
02189                                      WS-SRT02-LF-PRM-PR-ALT.      
02190      MOVE C-LF-COMM              TO  WS-SRT02-LF-COMM.            
02191      MOVE C-LF-COMM-ALT          TO  WS-SRT02-LF-COMM-ALT         
02192      MOVE CP-LPC                 TO  WS-SRT02-LF-COM-PCT.         
02193      MOVE CP-AH-TERM             TO  WS-SRT02-AH-TERM.            
02194      MOVE C-TT-AHBEN             TO  WS-SRT02-AH-BEN.             
02195      MOVE C-AH-PREM              TO  WS-SRT02-AH-PREM.            
02196      MOVE ZEROS                  TO  WS-SRT02-AH-PRM-PR.          
02197      MOVE C-AH-COMM              TO  WS-SRT02-AH-COMM.            
02198      MOVE CP-APC                 TO  WS-SRT02-AH-COM-PCT.         
02199      MOVE CP-AH-TYPE             TO  CLAS-LOOK.                   
092815*    MOVE CP-MEMBER              TO  WS-SRT02-MEMBER-NO.          
02201      MOVE ' '                    TO  WS-SRT02-PREM-OVERRIDE.      

092815     move cp-company-cd          to cs-company-cd
092815     move cp-alt-record-control  to cs-control-primary (2:19)
092815     MOVE '3'                    TO DC-OPTION-CODE
092815     MOVE CP-ALT-EFF-CC          TO DC-ALPHA-CEN-N
092815     MOVE CP-ALT-EFF-YR          TO DC-YMD-YEAR
092815     MOVE CP-ALT-EFF-MO          TO DC-YMD-MONTH
092815     MOVE CP-ALT-EFF-DA          TO DC-YMD-DAY
092815     PERFORM 1900-DATE-CONVERSION THRU  1999-EXIT
092815     if not no-conversion-error
092815        display ' error converting date ' dc-greg-date-1-ymd
092815          ' ' cp-alt-cert-no
092815     end-if
092815     MOVE DC-BIN-DATE-1          TO CS-CERT-EFF-DT
092815     MOVE CP-ALT-CERT-NO         TO CS-CERT-NO
092815     move 'C'                    to cs-trailer-type
092815     move spaces                 to ws-srt02-vin
092815*    display ' about to read elcrtt 0410 '
092815     read elcrtt
092815     if elcrtt-file-status = '10' or '23' or '00'
092815        continue
092815     else
092815        display ' bad file status elcrtt ' elcrtt-file-status
092815        perform abend-pgm
092815     end-if
092815
092815     if elcrtt-file-status = '00'
092815        if cs-vin-number not = spaces and low-values
092815*          display ' found vin ' cp-alt-cert-no
092815           move cs-vin-number    to ws-srt02-vin
092815        end-if
092815     end-if

02203      IF CLAS-LOOK = '00'                                          
02204          MOVE SPACES             TO  WS-SRT02-AH-TYP              
02205          GO TO 0420-CHECK-LF.                                     
02206                                                                   
02207      PERFORM 0500-FIND-AH  THRU  0599-EXIT.                       
02208                                                                   
032905     IF SEC-PAY-CARRIER (CLAS-INDEXCN)
032905        MOVE CLAS-I-BEN (CLAS-INDEXA)
032905                                 TO WS-SRT02-AH-TYP
032905     ELSE
032905        MOVE CLAS-I-AB3 (CLAS-INDEXA)
032905                                 TO WS-SRT02-AH-TYP
032905     END-IF

100703     MOVE CLAS-I-BEN-CATEGORY (CLAS-INDEXA)
011410                                    TO  WS-SRT02-AH-BEN-CAT
02210                                                                   
02211      IF CLAS-I-BAL (CLAS-INDEXA) = 'B'                            
02212          MOVE 'OUTSTANDNG BAL'   TO  WS-SRT02-NAME.               
011410     IF WS-SRT02-AH-BEN-CAT = 'G' OR 'L'
040504        COMPUTE WS-SRT02-AH-PRM-PR = CP-AH-PRM
040504           - CP-AH-COM
040504*       MOVE CP-LF-PRM-ALT       TO WS-SRT02-AH-PRM-PR
100703     END-IF
           .

02213  EJECT                                                            
02214  0420-CHECK-LF.                                                   
02215      MOVE CP-LF-TYPE             TO  CLAS-LOOK.                   
02216                                                                   
091911     IF CLAS-LOOK = '00' OR '  ' OR 'DD'                                          
02218          MOVE SPACES             TO  WS-SRT02-LF-TYP              
02219          GO TO 0430-CHECK-RECALC.                                 
02220                                                                   
02221      PERFORM 0600-FIND-LF  THRU  0699-EXIT.                       
02222                                                                   
032905     IF SEC-PAY-CARRIER (CLAS-INDEXCN)
032905        MOVE CLAS-I-BEN (CLAS-INDEXL)
032905                                 TO WS-SRT02-LF-TYP
032905     ELSE
032905        MOVE CLAS-I-AB3 (CLAS-INDEXL)
032905                                 TO WS-SRT02-LF-TYP
032905     END-IF
02224                                                                   
02225      IF CLAS-I-BAL (CLAS-INDEXL) = 'B'                            
02226          MOVE 'OUTSTANDNG BAL'   TO  WS-SRT02-NAME.               
02227                                                                   
           MOVE CLAS-I-BEN-CATEGORY (CLAS-INDEXL)
011410                                 TO WS-SRT02-LF-BEN-CAT

           .
02228  0430-CHECK-RECALC.                                               
02229                                                                   
CIDMOD*    IF DTE-CLIENT = 'KSM' OR 'CID'                               
CIDMOD     IF DTE-CLIENT = 'KSM'                                        
02231          PERFORM 7000-ACCOUNT-MASTER-LOOK  THRU  7999-EXIT.       
02232                                                                   
02233      IF CP-RC-ISSUE OR                                            
02234         CP-RC-CANCEL                                              
02235          MOVE 'R'                TO  WS-SRT02-RECALC-FLAG         
02236          GO TO 0490-END-BUILD.                                    
02237                                                                   
02238      MOVE ' '                    TO  WS-SRT02-RECALC-FLAG.        
02239                                                                   
02240      PERFORM 1800-READ-NOTES THRU 1899-EXIT.                      
02241                                                                   
02242      IF CP-ALT-OVERRIDE-LIFE  OR                                  
02243         CP-ALT-OVERRIDE-AH    OR                                  
02244         CP-ALT-OVERRIDE-BOTH                                      
02245          PERFORM 2100-READ-PEND-BUS THRU 2199-EXIT.               

010616     PERFORM 2200-READ-CHECK-FILE THRU 2299-EXIT

02247      GO TO 0490-END-BUILD.                                        
02248                                                                   
02249  0440-BUILD-CANCEL.                                               
02250      MOVE SPACES                 TO  WS-SRT00-REC.                
02251      MOVE '03 '                  TO  WS-SRT-REC-TYPE.             
02252      MOVE 'Y'                    TO  ACTIVITY-SWITCH.             
02253      MOVE CP-INSUREDS-NAME       TO  WS-SRT03-NAME.               
02254      MOVE CP-CERT                TO  WS-SRT03-CERT.               
02255      MOVE CP-EFF                 TO  WS-SRT03-EFF-DT.             
02256      MOVE CP-AGE                 TO  WS-SRT03-AGE.                
02257      MOVE CP-LF-TERM             TO  WS-SRT03-LF-TERM.            
02258      MOVE CP-AH-TERM             TO  WS-SRT03-AH-TERM.            
02259      MOVE CP-LF-CNC              TO  WS-SRT03-LF-CAN-DT.          
02260      MOVE CP-AH-CNC              TO  WS-SRT03-AH-CAN-DT.          
02261      MOVE CP-AH-AMT              TO  WS-SRT03-AH-MO-BEN.          
092815*    MOVE CP-MEMBER              TO  WS-SRT03-MEMBER-NO.          
02263      MOVE ' '                    TO  WS-SRT03-REF-OVERRIDE
020609                                     WS-SRT03-OB-ON
020609     IF CP-MONTHLY-ISSUE
020609        MOVE 'M'                 TO  WS-SRT03-OB-ON
020609     ELSE
052504        IF (DTE-CLIENT = 'DCC' or 'CAP')
                 AND (CP-LF-TERM = 1
                   OR CP-AH-TERM = 1)
                 MOVE '017'               TO WS-SRT-REC-TYPE
              END-IF
020609     END-IF
02265      COMPUTE WS-SRT03-LF-FACE = CP-LF-AMT + CP-LF-AMT-ALT.        
02266                                                                   
02267      IF C-LF-REFD NOT = ZERO                                      
02268          COMPUTE WS-SRT03-LF-REF  =  C-LF-REFD  *  -1             
02269      ELSE                                                         
02270          MOVE C-LF-REFD          TO  WS-SRT03-LF-REF.             
02271                                                                   
02272      IF C-LF-COMM NOT = ZERO                                      
02273          COMPUTE WS-SRT03-LF-COM-REF  =  C-LF-COMM  *  -1         
02274      ELSE                                                         
02275          MOVE C-LF-COMM          TO  WS-SRT03-LF-COM-REF.         
02276                                                                   
02277      MOVE CP-LPC                 TO  WS-SRT03-LF-COM-PCT.         
02278                                                                   
02279      IF C-AH-REFD NOT = ZERO                                      
02280          COMPUTE WS-SRT03-AH-REF  =  C-AH-REFD  *  -1             
02281      ELSE                                                         
02282          MOVE C-AH-REFD          TO  WS-SRT03-AH-REF.             
02283                                                                   
02284      IF C-AH-COMM NOT = ZERO                                      
02285          COMPUTE WS-SRT03-AH-COM-REF  =  C-AH-COMM  *  -1         
02286      ELSE                                                         
02287          MOVE C-AH-COMM          TO  WS-SRT03-AH-COM-REF.         
02288                                                                   
02289      MOVE CP-APC                 TO  WS-SRT03-AH-COM-PCT.         
02290      MOVE ZEROS                  TO  WS-SRT03-LF-REF-REV          
02291                                      WS-SRT03-AH-REF-REV          
02292                                      WS-SRT03-TOT-REF             
02293                                      WS-SRT03-COM-TOTREF.         

092815     move cp-company-cd          to cs-company-cd
092815     move cp-alt-record-control  to cs-control-primary (2:19)
092815     MOVE '3'                    TO DC-OPTION-CODE
092815     MOVE CP-ALT-EFF-CC          TO DC-ALPHA-CEN-N
092815     MOVE CP-ALT-EFF-YR          TO DC-YMD-YEAR
092815     MOVE CP-ALT-EFF-MO          TO DC-YMD-MONTH
092815     MOVE CP-ALT-EFF-DA          TO DC-YMD-DAY
092815     PERFORM 1900-DATE-CONVERSION THRU  1999-EXIT
092815     if not no-conversion-error
092815        display ' error converting date ' dc-greg-date-1-ymd
092815          ' ' cp-alt-cert-no
092815     end-if
092815     MOVE DC-BIN-DATE-1          TO CS-CERT-EFF-DT
092815     MOVE CP-ALT-CERT-NO         TO CS-CERT-NO
092815     move 'C'                    to cs-trailer-type
092815
092815     move spaces                 to ws-srt03-vin
092815*    display ' about to read elcrtt 0440 '
092815     read elcrtt
092815     if elcrtt-file-status = '10' or '23' or '00'
092815        continue
092815     else
092815        display ' bad file status elcrtt ' elcrtt-file-status
092815        perform abend-pgm
092815     end-if
092815
092815     if elcrtt-file-status = '00'
092815        if cs-vin-number not = spaces and low-values
092815*          display ' found vin ' cp-alt-cert-no
092815           move cs-vin-number    to ws-srt03-vin
092815        end-if
092815     end-if

02294      MOVE CP-AH-TYPE             TO  CLAS-LOOK.                   
02295                                                                   
02296      IF CLAS-LOOK = '00'                                          
02297          GO TO 0450-CHECK-LF.                                     
02298                                                                   
02299      PERFORM 0500-FIND-AH  THRU  0599-EXIT.                       
02300                                                                   

032905     IF SEC-PAY-CARRIER (CLAS-INDEXCN)
032905        MOVE CLAS-I-BEN (CLAS-INDEXA)
032905                                 TO WS-SRT03-AH-TYP
032905     ELSE
032905        MOVE CLAS-I-AB3 (CLAS-INDEXA)
032905                                 TO WS-SRT03-AH-TYP
032905     END-IF

100703     MOVE CLAS-I-BEN-CATEGORY (CLAS-INDEXA)
                                       TO WS-SRT03-AH-BEN-CAT
                                       
02303                                                                   
02304      IF CLAS-I-BAL (CLAS-INDEXA) = 'B'                            
02305          MOVE 'OUTSTANDNG BAL'   TO  WS-SRT03-NAME.               
02306                                                                   
02307  0450-CHECK-LF.                                                   
02308      MOVE CP-LF-TYPE             TO  CLAS-LOOK.                   
02309                                                                   
091911     IF CLAS-LOOK = '00' OR '  ' OR 'DD'                                          
02311          GO TO 0460-CHECK-RECALC.                                 
02312                                                                   
02313      PERFORM 0600-FIND-LF  THRU  0699-EXIT.                       
02314                                                                   

032905     IF SEC-PAY-CARRIER (CLAS-INDEXCN)
032905        MOVE CLAS-I-BEN (CLAS-INDEXL)
032905                                 TO WS-SRT03-LF-TYP
032905     ELSE
032905        MOVE CLAS-I-AB3 (CLAS-INDEXL)
032905                                 TO WS-SRT03-LF-TYP
032905     END-IF

02317                                                                   
02318      IF CLAS-I-BAL (CLAS-INDEXL) = 'B'                            
02319          MOVE 'OUTSTANDNG BAL'   TO  WS-SRT03-NAME.               
02320                                                                   
011410     MOVE CLAS-I-BEN-CATEGORY (CLAS-INDEXL)
011410                                 TO WS-SRT03-LF-BEN-CAT

           .
02321  0460-CHECK-RECALC.                                               
02322                                                                   
CIDMOD*    IF DTE-CLIENT = 'KSM' OR 'CID'                               
CIDMOD     IF DTE-CLIENT = 'KSM'                                        
02324          PERFORM 7000-ACCOUNT-MASTER-LOOK  THRU  7999-EXIT.       
02325                                                                   
02326      IF CP-RC-ISSUE  OR                                           
02327         CP-RC-CANCEL                                              
02328          MOVE 'R'                TO  WS-SRT03-RECALC-FLAG         
02329          GO TO 0490-END-BUILD.                                    
02330                                                                   
02331      MOVE ' '                    TO  WS-SRT03-RECALC-FLAG.        
02332                                                                   
02333      PERFORM 1800-READ-NOTES THRU 1899-EXIT.                      
02334                                                                   
02335      IF CP-ALT-OVERRIDE-LIFE  OR                                  
02336         CP-ALT-OVERRIDE-AH    OR                                  
02337         CP-ALT-OVERRIDE-BOTH                                      
02338          PERFORM 2100-READ-PEND-BUS THRU 2199-EXIT.               
02339                                                                   
02340      IF CP-ALT-REFUND-CREATED                                     
02341          PERFORM 2200-READ-CHECK-FILE  THRU  2299-EXIT.           
02342                                                                   
02343      GO TO 0490-END-BUILD.                                        
02344  EJECT                                                            
02345  0470-BUILD-ACCTG.                                                
02346      MOVE '11 '                  TO  WS-SRT-REC-TYPE.             
02347      MOVE SPACES                 TO  WS-SRT-LOW                   
02348                                      WS-SRT-CERT.                 
02349      MOVE CP-AC-DESC             TO  WS-SRT10-DESC.               
02350      MOVE CP-AC-PMT              TO  WS-SRT10-PYMT.               
02351      MOVE CP-AC-CHG              TO  WS-SRT10-CHRG.
082707     MOVE CP-AC-MAINT-DATE       TO  WS-SRT10-MAINT-DATE
02352                                                                   
02353      GO TO 0490-END-BUILD.                                        
02354                                                                   
02355  0480-BUILD-CLAIM.                                                
02356      MOVE '13 '                  TO  WS-SRT-REC-TYPE.             
02357      MOVE CP-INSUREDS-NAME       TO  WS-SRT12-NAME.               
02358      MOVE CP-CERT                TO  WS-SRT12-CERT.               
02359      MOVE CP-EFF                 TO  WS-SRT12-EFF-DT.             
02360      MOVE CP-MEMBER              TO  WS-SRT12-MEMBER-NO.          
02361                                                                   
02362      IF C-LF-CLM = +0                                             
02363          MOVE ZEROS              TO  WS-SRT12-LF-CLM              
02364          MOVE C-AH-CLM           TO  WS-SRT12-AH-CLM              
02365      ELSE                                                         
02366          MOVE C-LF-CLM           TO  WS-SRT12-LF-CLM              
02367          MOVE ZEROS              TO  WS-SRT12-AH-CLM.             
02368                                                                   
02369  0490-END-BUILD.                                                  
02370      PERFORM 2000-RELEASE-SORT-REC  THRU  2099-EXIT.              
02371                                                                   
02372  0499-EXIT.                                                       
02373      EXIT.                                                        
02374  EJECT                                                            
02375  0500-FIND-AH.                                                    
02376      IF CLAS-STARTA = ZERO                                        
02377          DISPLAY 'CLAS-STARTA = ZERO'                             
02378          MOVE '0402'             TO  ABEND-CODE                   
02379          GO TO 9999-ABEND-PGM.                                    
02380                                                                   
02381      MOVE CLAS-STARTA            TO  CLAS-INDEXA.                 
02382                                                                   
02383  0510-AH-LOOP.                                                    
02384      IF CLAS-INDEXA GREATER CLAS-MAXA                             
02385         DISPLAY 'AH BENEFIT ' CLAS-LOOK ' NOT ON FILE'           
060712        if from-2310
060712           go to 0599-exit
060712        else
02386            MOVE '0402'             TO  ABEND-CODE                   
02387            GO TO 9999-ABEND-PGM
060712        end-if
060712     end-if
02388                                                                   
02389      IF CLAS-I-BEN (CLAS-INDEXA) NOT = CLAS-LOOK                  
02390          ADD +1                  TO  CLAS-INDEXA                  
02391          GO TO 0510-AH-LOOP.                                      
02392                                                                   
02393  0599-EXIT.                                                       
02394      EXIT.                                                        
02395  EJECT                                                            
02396  0600-FIND-LF.                                                    
02397      IF CLAS-STARTL = ZERO                                        
02398          DISPLAY 'CLAS-STARTL = ZERO'                             
02399          MOVE '0401'             TO  ABEND-CODE                   
02400          GO TO 9999-ABEND-PGM.                                    
02401                                                                   
02402      MOVE CLAS-STARTL            TO  CLAS-INDEXL.                 
02403                                                                   
02404  0610-LF-LOOP.                                                    
02405      IF CLAS-INDEXL GREATER CLAS-MAXL                             
02406         DISPLAY 'LIFE BENEFIT ' CLAS-LOOK ' NOT IN TABLE'        
060712        if from-2310
060712           go to 0699-exit
060712        else
02407            MOVE '0401'           TO  ABEND-CODE                   
02408            GO TO 9999-ABEND-PGM
060712        end-if
060712     end-if
02409                                                                   
02410      IF CLAS-I-BEN (CLAS-INDEXL) NOT = CLAS-LOOK                  
02411          ADD +1                  TO  CLAS-INDEXL                  
02412          GO TO 0610-LF-LOOP.                                      
02413                                                                   
02414  0699-EXIT.                                                       
02415      EXIT.                                                        
02416  EJECT                                                            
02417  0700-BUILD-TOTAL-RTN.                                            
02418      PERFORM 1000-CALC-TOTAL-RTN     THRU  1099-EXIT.             
02419      PERFORM 0800-BUILD-STMT-HD-RTN  THRU  0899-EXIT.             
02420                                                                   
02421  0710-BUILD-BAL-FWD.                                              
02422      MOVE CO-CARRIER             TO  WS-SRT-CARR.                 
02423      MOVE CO-GROUPING            TO  WS-SRT-GROUP.                
02424      MOVE CO-ACCOUNT             TO  WS-SRT-ACCT.                 
02425      MOVE CO-RESP-NO             TO  WS-SRT-REMIT.                
02426      MOVE '12 '                  TO  WS-SRT-REC-TYPE.             
02427      MOVE ZERO                   TO  WS-SRT-REC-SFX.              
02428      MOVE ' '                    TO  WS-SRT-SUPP-REC-TYP.         
02429      MOVE SPACES                 TO  WS-SRT-LOW                   
02430                                      WS-SRT-CERT.                 
02431      MOVE S-BEG-BAL              TO  WS-SRT11-BEG-BAL.            
02432                                                                   
02433      IF DTE-PGM-OPT = 1  OR  4  OR  6                             
02434          IF S-BEG-BAL NOT = ZERO                                  
02435              MOVE 'Y'            TO  ACTIVITY-SWITCH.             
02436                                                                   
02437                                                                   
02438      PERFORM 2000-RELEASE-SORT-REC  THRU  2099-EXIT.              
02439                                                                   
02440  0799-EXIT.                                                       
02441      EXIT.                                                        
02442  EJECT                                                            
02443  0800-BUILD-STMT-HD-RTN.                                          
02444      MOVE CO-CARRIER             TO  WS-SRT-CARR.                 
02445      MOVE CO-GROUPING            TO  WS-SRT-GROUP.                
02446      MOVE CO-ACCOUNT             TO  WS-SRT-ACCT.                 
02447      MOVE CO-RESP-NO             TO  WS-SRT-REMIT.                
02448      MOVE '00 '                  TO  WS-SRT-REC-TYPE.             
02449      MOVE ZERO                   TO  WS-SRT-REC-SFX.              
02450      MOVE SPACES                 TO  WS-SRT-LOW                   
02451                                      WS-SRT-CERT.                 
02452      MOVE ' '                    TO  WS-SRT-SUPP-REC-TYP.         
02453      MOVE SAVE-COMPANY-NAME (3)  TO  WS-SRT00-DATA.               
02454                                                                   
02455      PERFORM 2000-RELEASE-SORT-REC  THRU  2099-EXIT.              
02456                                                                   
02457      MOVE '01 '                  TO  WS-SRT-REC-TYPE.             
02458      MOVE ZERO                   TO  WS-SRT-REC-SFX.              
02459      MOVE SPACES                 TO  WS-SRT-LOW                   
02460                                      WS-SRT-CERT.                 
02461      MOVE CO-CARR-GROUP          TO  WS-SRT01-CARR-GROUP.         
02462      MOVE BIL0                   TO  WS-SRT01-ACCT.               
02463      MOVE BIL1                   TO  WS-SRT01-NAME.               
02464      MOVE RMT0                   TO  WS-SRT01-RESP.               
02465      MOVE RMT1                   TO  WS-SRT01-REMIT.              
PEMTST     MOVE CO-CONTROL-NAME        TO  WS-SRT01-CONTROL-NAME
100307     MOVE CO-CSR-CODE            TO  WS-SRT01-CSR

011410     IF CO-COMP-TYPE = '1'
011410        MOVE 'Y'                 TO  WS-SRT01-SPPDD
011410     ELSE
011410        MOVE 'N'                 TO  WS-SRT01-SPPDD
011410     END-IF

PEMTST*    MOVE WS-CONTROL-NAME        TO  WS-SRT01-CONTROL-NAME
02466                                                                   
02467      PERFORM 2000-RELEASE-SORT-REC  THRU  2099-EXIT.              
02468                                                                   
02469      MOVE 1                      TO  WS-SRT-REC-SFX.              
02470      MOVE SPACES                 TO  WS-SRT01-REC.                
02471      MOVE BIL2                   TO  WS-SRT01-NAME.               
02472      MOVE RMT2                   TO  WS-SRT01-REMIT.              
02473                                                                   
02474      PERFORM 2000-RELEASE-SORT-REC  THRU  2099-EXIT.              
02475                                                                   
02476      MOVE 2                      TO  WS-SRT-REC-SFX.              
02477      MOVE SPACES                 TO  WS-SRT01-REC.                
02478      MOVE BIL3                   TO  WS-SRT01-NAME.               
02479      MOVE RMT3                   TO  WS-SRT01-REMIT.              
02480                                                                   
02481      PERFORM 2000-RELEASE-SORT-REC  THRU  2099-EXIT.              
02482                                                                   
02483      MOVE 3                      TO  WS-SRT-REC-SFX.              
02484      MOVE SPACES                 TO  WS-SRT01-REC.                
02485      MOVE BIL4                   TO  WS-SRT01-NAME.               
02486      MOVE RMT4                   TO  WS-SRT01-REMIT.              
02487                                                                   
02488      PERFORM 2000-RELEASE-SORT-REC  THRU  2099-EXIT.              
02489  EJECT                                                            
02490  0810-CHK-LN5.                                                    
02491      IF BIL5 = SPACES   AND                                       
02492         RMT5 = SPACES                                             
02493          GO TO 0820-CHK-LN6.                                      
02494                                                                   
02495      MOVE 4                      TO  WS-SRT-REC-SFX.              
02496      MOVE SPACES                 TO  WS-SRT01-REC.                
02497      MOVE BIL5                   TO  WS-SRT01-NAME.               
02498      MOVE RMT5                   TO  WS-SRT01-REMIT.              
02499                                                                   
02500      PERFORM 2000-RELEASE-SORT-REC  THRU  2099-EXIT.              
02501                                                                   
02502  0820-CHK-LN6.                                                    
02503      IF BIL6 = SPACES  AND                                        
02504         RMT6 = SPACES                                             
02505          GO TO 0899-EXIT.                                         
02506                                                                   
02507      MOVE 5                      TO  WS-SRT-REC-SFX.              
02508      MOVE SPACES                 TO  WS-SRT01-REC.                
02509      MOVE BIL6                   TO  WS-SRT01-NAME.               
02510      MOVE RMT6                   TO  WS-SRT01-REMIT.              
02511                                                                   
02512      PERFORM 2000-RELEASE-SORT-REC  THRU  2099-EXIT.              
02513                                                                   
02514  0899-EXIT.                                                       
02515      EXIT.                                                        
02516  EJECT                                                            
02517  0900-CALC-DETAIL-RTN.                                            
02518      MOVE ZEROS                  TO  C-TT-FACE  C-TT-FACE-ALT     
02519                                      C-LF-PREM  C-LF-PREM-ALT     
02520                                      C-LF-COMM  C-LF-COMM-ALT     
02521                                      C-AH-PREM  C-TT-PREM         
02522                                      C-LF-REFD  C-AH-REFD         
02523                                      C-TT-REFD  C-AH-COMM         
02524                                      C-TT-COMM  C-RR-PYMT
062519                                     c-rr-pymt-adj
02525                                      C-CC-PYMT  C-TT-PYMT         
02526                                      C-LF-CLM   C-AH-CLM          
02527                                      C-TT-CLM   C-TT-AHBEN.       
02528                                                                   
02529      IF CP-CLAIM                                                  
02530          GO TO 0980-CALC-CLAIM.                                   
02531                                                                   
02532      IF CP-RC-ISSUE  OR                                           
02533         CP-ISSUE                                                  
122002*       OR CP-MONTHLY-ISSUE
02534          GO TO 0910-CALC-ISSUE.                                   
02535                                                                   
02536      IF CP-RC-CANCEL  OR                                          
02537         CP-CANCEL                                                 
02538          GO TO 0940-CALC-CANCEL.                                  
02539                                                                   
02540      IF CP-ACCTG                                                  
02541          GO TO 0970-CALC-ACCTG.                                   
02542                                                                   
           DISPLAY ' INVALID TRANS CP CONTROL ' CP-CONTROL
02543      MOVE '0301'                 TO  ABEND-CODE.                  
02544                                                                   
02545      GO TO 9999-ABEND-PGM.                                        
02546                                                                   
02547  0910-CALC-ISSUE.                                                 
02548                                                                   
02549      IF CP-BILLED                                                 
02550         IF CP-ISSUE                                              
100703           IF CP-LF-TYPE NOT = '00' AND '  '
091911                AND LOW-VALUES AND 'DD'
02551               ADD CP-LF-PRM      TO  S-TT-PREM-BILL
02552               ADD CP-LF-PRM-ALT  TO  S-TT-PREM-BILL
02554               ADD CP-LF-COM      TO  S-TT-COMM-BILL
02555               ADD CP-LF-COM-ALT  TO  S-TT-COMM-BILL
100703           END-IF
02553            ADD CP-AH-PRM         TO  S-TT-PREM-BILL
02556            ADD CP-AH-COM         TO  S-TT-COMM-BILL
02557            GO TO 0999-EXIT
02558         ELSE
100703           IF CP-LF-TYPE NOT = '00' AND '  ' AND LOW-VALUES
091911               AND 'DD'
02559               SUBTRACT CP-LF-PRM     FROM S-TT-PREM-BILL
02560               SUBTRACT CP-LF-PRM-ALT FROM S-TT-PREM-BILL
02562               SUBTRACT CP-LF-COM     FROM S-TT-COMM-BILL
02563               SUBTRACT CP-LF-COM-ALT FROM S-TT-COMM-BILL
100703           END-IF
02561            SUBTRACT CP-AH-PRM     FROM S-TT-PREM-BILL
02564            SUBTRACT CP-AH-COM     FROM S-TT-COMM-BILL
02565            GO TO 0999-EXIT
100703        END-IF
100703     END-IF
02566                                                                   
02567      MOVE CP-LF-AMT              TO  C-TT-FACE.                   
02568      MOVE CP-LF-AMT-ALT          TO  C-TT-FACE-ALT.               
02569      MOVE CP-AH-AMT              TO  C-TT-AHBEN.                  
02570      MOVE CP-AH-TYPE             TO  CLAS-LOOK.                   


032905     PERFORM VARYING CLAS-INDEXCN FROM +1 BY +1 UNTIL
032905        (CLAS-INDEXCN > CLAS-MAXCN)
032905        OR (CP-CARRIER  = CARRIER-SUB (CLAS-INDEXCN))
032905     END-PERFORM
032905     IF CLAS-INDEXCN > CLAS-MAXCN
032905        DISPLAY ' PROBLEM FINDING CARRIER ' CP-CARRIER
032905     END-IF


02571                                                                   
02572      IF CLAS-LOOK = '00'                                          
02573          GO TO 0920-CHECK-LF.                                     
02574                                                                   
02575      PERFORM 0500-FIND-AH  THRU  0599-EXIT.                       
02576                                                                   
02577      IF CLAS-I-BAL (CLAS-INDEXA) = 'B'  OR  'Z'                   
02578          MOVE 'Y'                TO  OB-SWITCH.                   
02579  EJECT                                                            
02580  0920-CHECK-LF.                                                   
02581      MOVE CP-LF-TYPE             TO  CLAS-LOOK.                   
02582                                                                   
091911     IF CLAS-LOOK = '00' OR '  ' OR 'DD'
02584          GO TO 0930-CONTINUE.                                     
02585                                                                   
02586      PERFORM 0600-FIND-LF  THRU  0699-EXIT.                       
02587                                                                   
02588      IF CLAS-I-BAL (CLAS-INDEXL) = 'B'  OR  'Z'                   
02589          MOVE 'Y'                TO  OB-SWITCH.                   
02590                                                                   
02591  0930-CONTINUE.                                                   
100703
100703     IF CP-LF-TYPE NOT = '00' AND SPACES AND LOW-VALUES
091911           AND 'DD'
100703        MOVE CP-LF-PRM           TO  C-LF-PREM
100703        MOVE CP-LF-PRM-ALT       TO  C-LF-PREM-ALT
100703     END-IF
100703
02594      MOVE CP-AH-PRM              TO  C-AH-PREM.                   
02595                                                                   
02596      COMPUTE C-TT-PREM  =  C-LF-PREM  +  C-LF-PREM-ALT            
02597                         +  C-AH-PREM.                             
02598                                                                   
100703     IF CP-LF-TYPE NOT = '00' AND SPACES AND LOW-VALUES
091911            AND 'DD'
02599         MOVE CP-LF-COM           TO  C-LF-COMM
02600         MOVE CP-LF-COM-ALT       TO  C-LF-COMM-ALT
100703     END-IF

02601      MOVE CP-AH-COM              TO  C-AH-COMM.                   
02602                                                                   
02603      COMPUTE C-TT-COMM  =  C-LF-COMM  +  C-LF-COMM-ALT            
02604                         +  C-AH-COMM.                             
02605                                                                   
02606      GO TO 0990-ROLL-DETAIL.                                      
02607                                                                   
02608  0940-CALC-CANCEL.                                                
02609      MOVE CP-AH-TYPE             TO  CLAS-LOOK.                   
02610                                                                   
02611      IF CLAS-LOOK = '00'                                          
02612          GO TO 0950-CHECK-LF.                                     
02613                                                                   
02614      PERFORM 0500-FIND-AH  THRU  0599-EXIT.                       
02615                                                                   
02616      IF CLAS-I-BAL (CLAS-INDEXA) = 'B'  OR  'Z'                   
02617          MOVE 'Y'                TO  OB-SWITCH.                   
02618                                                                   
02619  0950-CHECK-LF.                                                   
02620      MOVE CP-LF-TYPE             TO  CLAS-LOOK.                   
02621                                                                   
091911     IF CLAS-LOOK = '00' OR '  ' OR 'DD'
02623          GO TO 0960-CONTINUE.                                     
02624                                                                   
02625      PERFORM 0600-FIND-LF  THRU  0699-EXIT.                       
02626                                                                   
02627      IF CLAS-I-BAL (CLAS-INDEXL) = 'B'  OR  'Z'                   
02628          MOVE 'Y'                TO  OB-SWITCH.                   
02629  EJECT                                                            
02630  0960-CONTINUE.                                                   

100703     IF CP-LF-TYPE NOT = '00' AND SPACES AND LOW-VALUES
091911            AND 'DD'
100703        MOVE CP-LF-PRM           TO  C-LF-REFD
100703     END-IF
02632      MOVE CP-AH-PRM              TO  C-AH-REFD.                   
02633                                                                   
02634      COMPUTE C-TT-REFD  =  C-LF-REFD  +  C-AH-REFD.               
02635                                                                   
100703     IF CP-LF-TYPE NOT = '00' AND SPACES AND LOW-VALUES
091911           AND 'DD'
02636         MOVE CP-LF-COM           TO  C-LF-COMM
02637         MOVE CP-LF-COM-ALT       TO  C-LF-COMM-ALT
100703     END-IF

02638      MOVE CP-AH-COM              TO  C-AH-COMM.                   
02639                                                                   
02640      COMPUTE C-TT-COMM  =  C-LF-COMM  +  C-LF-COMM-ALT            
02641                         +  C-AH-COMM.                             
02642                                                                   
           IF CP-CANCEL
              COMPUTE WS-TOT-REF-COMM = WS-TOT-REF-COMM + C-TT-COMM
           END-IF

02643      GO TO 0990-ROLL-DETAIL.                                      
02644                                                                   
02645  0970-CALC-ACCTG.                                                 
02646                                                                   
02647      IF CP-BILLED                                                 
02648          ADD CP-AC-PMT TO S-TT-PYMT-BILL                          
02649          SUBTRACT CP-AC-CHG FROM S-TT-PYMT-BILL                   
02650          GO TO 0999-EXIT.                                         
02651                                                                   
02652      MOVE CP-AC-PMT              TO  C-RR-PYMT.                   
02653      MOVE CP-AC-CHG              TO  C-CC-PYMT.                   

062519     if (cp-ac-pmt <> zero)
062519        and (cp-ac-desc(14:10) = 'VOID REFCK' OR 'ST PY')
062519        compute c-rr-pymt-adj =
062519           c-rr-pymt-adj + cp-ac-pmt
062519     end-if
02654                                                                   
062519     if (cp-ac-chg <> zero)
062519        and (cp-ac-desc(14:7) = 'RE ITEM')
062519        compute c-rr-pymt-adj =
062519           c-rr-pymt-adj + cp-ac-chg
062519     end-if

02655      COMPUTE C-TT-PYMT  =  C-RR-PYMT  -  C-CC-PYMT.               
02656                                                                   
02657      GO TO 0990-ROLL-DETAIL.                                      
02658                                                                   
02659  0980-CALC-CLAIM.                                                 
02660      MOVE CP-CLM-LF-AMT          TO  C-LF-CLM.                    
02661      MOVE CP-CLM-AH-AMT          TO  C-AH-CLM.                    
02662                                                                   
02663      COMPUTE C-TT-CLM  =  C-LF-CLM  +  C-AH-CLM.                  
02664                                                                   
02665  0990-ROLL-DETAIL.                                                

122002     IF CP-MONTHLY-ISSUE
122002        GO TO 0999-EXIT
122002     END-IF

02666      ADD C-TT-FACE               TO  S-TT-FACE.                   
02667      ADD C-TT-FACE-ALT           TO  S-TT-FACE.                   
02668      ADD C-AH-PREM               TO  S-AH-PREM.                   
02669      ADD C-LF-PREM               TO  S-LF-PREM.                   
02670      ADD C-LF-PREM-ALT           TO  S-LF-PREM.                   
02671      ADD C-TT-PREM               TO  S-TT-PREM.                   

011410     IF (CP-AH-TYPE NOT = '00' AND '  ')
092705        AND (CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L')
012711        AND (CO-COMP-TYPE NOT = '1')
120804        CONTINUE
120804     ELSE
02672         ADD C-AH-REFD            TO  S-AH-REFD
02673         ADD C-LF-REFD            TO  S-LF-REFD
02674         ADD C-TT-REFD            TO  S-TT-REFD
120804     END-IF
           
02675      ADD C-LF-COMM               TO  S-LF-COMM.                   
02676      ADD C-LF-COMM-ALT           TO  S-LF-COMM.                   
02677      ADD C-AH-COMM               TO  S-AH-COMM.                   
02678      ADD C-TT-COMM               TO  S-TT-COMM.                   
02679      ADD C-RR-PYMT               TO  S-RR-PYMT.
062519     add c-rr-pymt-adj           to  s-rr-pymt-adj
02680      ADD C-CC-PYMT               TO  S-CC-PYMT.                   
02681      ADD C-TT-PYMT               TO  S-TT-PYMT.                   
02682      ADD C-LF-CLM                TO  S-LF-CLM.                    
02683      ADD C-AH-CLM                TO  S-AH-CLM.                    
02684      ADD C-TT-CLM                TO  S-TT-CLM.                    
02685                                                                   
02686  0999-EXIT.                                                       
02687      EXIT.                                                        
02688  EJECT                                                            
02689  1000-CALC-TOTAL-RTN.                                             
02690      MOVE CO-BAL-FWD             TO  S-BEG-BAL.                   
02691                                                                   
02692      COMPUTE S-END-BAL  =   S-BEG-BAL  +  S-TT-PREM  +  S-TT-REFD 
02693                         -   S-TT-PYMT  -  S-TT-COMM.              

02695      IF S-END-BAL NEGATIVE                                        
02696          IF S-END-BAL LESS VARY-LO                                
02697              GO TO 1010-CALC-TOTAL.                               
02698                                                                   
02699      IF S-END-BAL POSITIVE                                        
02700          IF S-END-BAL GREATER VARY-HI                             
02701              GO TO 1010-CALC-TOTAL.                               
02702                                                                   
02703      COMPUTE S-WRT-OFF  =  S-END-BAL  *  -1.                      
02704                                                                   
02705      ADD S-WRT-OFF               TO  S-END-BAL.                   
02706                                                                   
02707  1010-CALC-TOTAL.                                                 
02708      IF SUMM-SW = +1                                              
02709          PERFORM 1100-BUILD-SUMMARY-RTN  THRU  1199-EXIT.         
02710                                                                   
02711      ADD S-BEG-BAL               TO  T-BEG-BAL.                   
02712      ADD S-TT-PREM               TO  T-TOT-PRM.                   
02713      ADD S-TT-REFD               TO  T-TOT-REF.                   
02714      ADD S-TT-COMM               TO  T-TOT-COM.                   
02715      ADD S-TT-PYMT               TO  T-TOT-PMT.                   
02716      ADD S-WRT-OFF               TO  T-WRT-OFF.                   

02717      ADD S-END-BAL               TO  T-END-BAL.                   
02718      ADD S-TT-COMM               TO  T-ADJ-COM.                   
02719                                                                   
02720      SUBTRACT S-WRT-OFF          FROM  T-ADJ-COM.                 
02721                                                                   
02722  1099-EXIT.                                                       
02723      EXIT.                                                        
02724  EJECT                                                            
02725  1100-BUILD-SUMMARY-RTN.                                          
02726      MOVE SPACES                 TO  CCM-WK.                      
02727      MOVE '#'                    TO  CCW-ID.                      
02728      MOVE CO-CARR-GROUP          TO  CCW-CARR-GROUP.              
02729      MOVE CO-RESP-NO             TO  CCW-RESP-NO.                 
02730      MOVE LOW-VALUES             TO  CCW-ACCOUNT.                 
02731      MOVE CO-ACCOUNT             TO  CCW-AM-NO.                   
02732                                                                   
02733      IF S-TT-PREM-BILL = ZEROS AND                                
02734         S-TT-COMM-BILL = ZEROS AND                                
02735         S-TT-PYMT-BILL = ZEROS                                    
02736          GO TO 1100-REGULAR-CCW.                                  
02737                                                                   
02738      MOVE '8'                    TO  CCW-TYPE.                    
02739      MOVE S-TT-PREM-BILL         TO  CCW-PREM.                    
02740      MOVE S-TT-COMM-BILL         TO  CCW-COMM.                    
02741      MOVE S-TT-PYMT-BILL         TO  CCW-PMTS.                    
02742      MOVE ZEROS                  TO  CCW-BEG-BAL                  
02743                                      CCW-OV-L-PREM                
02744                                      CCW-OV-A-PREM                
02745                                      CCW-OV-LIFE                  
02746                                      CCW-OV-AH                    
011904                                     CCW-OV-AH-RFND 
040504                                     CCW-DLR-INC
011410                                     CCW-LF-LMBA-FEE
011410                                     CCW-AH-LMBA-FEE
02747                                      CCW-OV-B-L-PREM              
02748                                      CCW-OV-B-A-PREM              
02749                                      CCW-OV-B-LIFE                
02750                                      CCW-OV-B-AH
011410                                     CCW-OV-L-CNT
011410                                     CCW-OV-A-CNT.
02751                                                                   
02752      COMPUTE CCW-END-BAL = CCW-PREM - CCW-COMM - CCW-PMTS.        
02753                                                                   
02754      MOVE CO-BALANCE-CONTROL     TO  CCW-BAL-CTL.                 
02755      MOVE CO-ACCT-NAME           TO  CCW-NAME.                    
02756                                                                   
02757      IF CO-ACCT-NAME = SPACES                                     
02758          MOVE CO-MAIL-NAME       TO  CCW-NAME.                    
02759                                                                   
02760      PERFORM 1700-WRT-CP-RTN  THRU  1799-EXIT.                    
02761                                                                   
02762      IF S-TT-PREM = ZEROS AND                                     
02763         S-TT-REFD = ZEROS AND                                     
02764         S-TT-COMM = ZEROS AND                                     
02765         S-TT-PYMT = ZEROS AND                                     
02766         S-END-BAL = ZEROS                                         
02767          GO TO 1199-EXIT.                                         
02768                                                                   
02769  1100-REGULAR-CCW.                                                
02770      MOVE SPACES                 TO  CCM-WK.                      
02771      MOVE '#'                    TO  CCW-ID.                      
02772      MOVE CO-CARR-GROUP          TO  CCW-CARR-GROUP.              
02773      MOVE CO-RESP-NO             TO  CCW-RESP-NO.                 
02774      MOVE LOW-VALUES             TO  CCW-ACCOUNT.                 
02775      MOVE CO-ACCOUNT             TO  CCW-AM-NO.                   
02776      MOVE '7'                    TO  CCW-TYPE.                    
02777                                                                   
02778      COMPUTE CCW-PREM = S-TT-PREM + S-TT-REFD.                    
02779                                                                   
02780      MOVE S-TT-COMM              TO  CCW-COMM.                    
02781                                                                   
02782 * ADJUSTING COMMISSIONS BY WRT-OFF AMOUNT.                        
02783      SUBTRACT S-WRT-OFF          FROM  CCW-COMM.                  
02784                                                                   
02785      MOVE S-TT-PYMT              TO  CCW-PMTS.                    
02786      MOVE S-BEG-BAL              TO  CCW-BEG-BAL.                 
02787      MOVE S-END-BAL              TO  CCW-END-BAL.                 
02788      MOVE ZEROS                  TO  CCW-OV-L-PREM                
02789                                      CCW-OV-A-PREM                
02790                                      CCW-OV-LIFE                  
02791                                      CCW-OV-AH                    
011904                                     CCW-OV-AH-RFND
040504                                     CCW-DLR-INC
011410                                     CCW-LF-LMBA-FEE
011410                                     CCW-AH-LMBA-FEE
02792                                      CCW-OV-B-L-PREM              
02793                                      CCW-OV-B-A-PREM              
02794                                      CCW-OV-B-LIFE                
02795                                      CCW-OV-B-AH
011410                                     CCW-OV-L-CNT
011410                                     CCW-OV-A-CNT.
02796                                                                   
02797      MOVE CO-BALANCE-CONTROL     TO  CCW-BAL-CTL.                 
02798      MOVE CO-ACCT-NAME           TO  CCW-NAME.                    
02799                                                                   
02800      IF CO-ACCT-NAME = SPACES                                     
02801          MOVE CO-MAIL-NAME       TO  CCW-NAME.                    
02802                                                                   
02803      PERFORM 1700-WRT-CP-RTN  THRU  1799-EXIT.                    
02804                                                                   
02805  1199-EXIT.                                                       
02806      EXIT.                                                        
02807  EJECT                                                            
02808  1200-UPDATE-MSTR-RTN.

02809      ADD S-TT-COMM               TO  CO-YTD-COM.                  
02810                                                                   
02811      SUBTRACT S-WRT-OFF          FROM  CO-YTD-COM.                
02812                                                                   
02813      IF STMT-SW = +1                                              
02814          MOVE 'Y'                TO  CO-INTERNAL-CONTROL-2        
02815      ELSE                                                         
02816          MOVE 'N'                TO  CO-INTERNAL-CONTROL-2.       
02817                                                                   
02818      IF CO-CARRY-BALANCE                                          
02819          NEXT SENTENCE                                            
02820      ELSE                                                         
02821          GO TO 1299-EXIT.                                         
02822                                                                   
02823      MOVE S-TT-COMM              TO  CO-CUR-COM.                  
02824                                                                   
02825 * ADJUSTING COMMISSION BY WRT-OFF AMOUNT                          
02826      SUBTRACT S-WRT-OFF          FROM  CO-CUR-COM.                
02827                                                                   
02828      COMPUTE CO-CUR-CHG  =  S-TT-PREM  +  S-TT-REFD.              
02829                                                                   
02830      MOVE S-TT-PYMT              TO  CO-CUR-PMT.                  
02831      MOVE S-END-BAL              TO  CO-END-BAL.                  
02832                                                                   
020210     IF (CO-BILL-SW = 'B' OR 'C' OR 'E')
              AND (CO-TYPE = 'A')
020210        IF CO-END-BAL < ZEROS
                 COMPUTE WS-END-BAL = CO-CUR-CHG - CO-CUR-COM
                 IF (WS-END-BAL <= (CO-END-BAL + .10))
                    AND (WS-END-BAL >= (CO-END-BAL - .10))
                    CONTINUE
                 ELSE
020210              IF CO-END-BAL <= ((S-TT-REFD - WS-TOT-REF-COMM)
                        + .10)
020210                 AND (CO-END-BAL >= (S-TT-REFD - WS-TOT-REF-COMM)
020210                    - .10)
020210                 MOVE 'RF4'         TO CO-STMT-TYPE
                       DISPLAY ' SETTING REF4 STMT ' CO-CARRIER ' '
                          CO-RESP-NO ' ' CO-ACCOUNT ' ' S-TT-REFD ' '
                          WS-TOT-REF-COMM
                    END-IF
020210           END-IF
020210        END-IF
020210     END-IF

02833      COMPUTE CO-CUR  =  S-TT-PREM  +  S-TT-REFD                   
02834                      -  S-TT-COMM  -  S-WRT-OFF.                  
02835                                                                   
02836      MOVE S-LF-CLM               TO  CO-LF-CLM-AMT.               
02837      MOVE S-AH-CLM               TO  CO-AH-CLM-AMT.               
02838                                                                   
02839      IF CO-CUR-OVR-UNDR NOT NUMERIC                               
02840          MOVE ZEROS              TO  CO-CUR-OVR-UNDR.             
02841                                                                   
02842      IF CO-YTD-OVR-UNDR NOT NUMERIC                               
02843          MOVE ZEROS              TO  CO-YTD-OVR-UNDR.             
02844                                                                   
02845      ADD S-WRT-OFF               TO  CO-YTD-OVR-UNDR.             
02846                                                                   
02847      MOVE S-WRT-OFF              TO  CO-CUR-OVR-UNDR.             
02848                                                                   
02849 * ADJUSTING AGING AMOUNTS BY PAYMENT AMOUNTS                      

062519*    IF CO-CUR-PMT NEGATIVE                                       
062519*        SUBTRACT CO-CUR-PMT     FROM  CO-CUR                     
062519*    ELSE                                                         
062519*       if dte-client = 'AHL'
062519*          subtract co-cur-pmt from co-ov120
062519*       else
062519*          SUBTRACT CO-CUR-PMT   FROM  CO-OV90
062519*       end-if
062519*    end-if

062519     compute co-cur =
062519        co-cur - s-rr-pymt + s-cc-pymt

050919**  Apply payment to any balance first
050919
062519     if s-rr-pymt > zeros
062519        if dte-client = 'AHL'
062519           compute co-ov120 =
062519              co-ov120 - s-rr-pymt + s-rr-pymt-adj
062519        else
062519           compute co-ov90 =
062519              (co-ov90 - s-rr-pymt) + s-rr-pymt-adj
062519           compute co-cur =
062519              (co-cur + s-rr-pymt) - s-rr-pymt-adj
062519        end-if
062519     end-if
050919
02855      IF CO-CUR NEGATIVE                                           
082012        if dte-client = 'AHL'
082012           add co-cur            to co-ov120
082012        else
02856            ADD CO-CUR            TO  CO-OV90
082012        end-if
02857         MOVE +0                  TO  CO-CUR
           end-if

082012     if dte-client = 'AHL'
082012        if co-ov120 negative
082012           add co-ov120          to co-ov90
082012           move +0               to co-ov120
082012        end-if
082012     end-if

02859      IF CO-OV90 NEGATIVE                                          
02860          ADD CO-OV90             TO  CO-OV60                      
02861          MOVE +0                 TO  CO-OV90.                     
02862                                                                   
02863      IF CO-OV60 NEGATIVE                                          
02864          ADD CO-OV60             TO  CO-OV30                      
02865          MOVE +0                 TO  CO-OV60.                     
02866                                                                   
02867      IF CO-OV30 NEGATIVE                                          
02868          ADD CO-OV30             TO  CO-CUR                       
02869          MOVE +0                 TO  CO-OV30.                     
02870                                                                   
02871      COMPUTE TOTAL-DUE  =  CO-CUR   +  CO-OV30                    
02872                         +  CO-OV60  +  CO-OV90
082012                        +  co-ov120

02874      IF CO-END-BAL NOT = TOTAL-DUE  OR                            
02875         CO-END-BAL LESS ZERO                                      
02876          MOVE CO-END-BAL         TO  CO-CUR                       
02877          MOVE +0                 TO  CO-OV30  CO-OV60             
082012                                     CO-OV90  co-ov120
           end-if

           .
02880  1299-EXIT.                                                       
02881      EXIT.                                                        
02882  EJECT                                                            
02883  1300-MSTR-CONTROL-RTN.                                           
02884      MOVE COMPENSATION-MASTER    TO  COMP-OUT-RECORD.             
02885                                                                   
02886      IF COO-ID NOT = 'CO'                                         
02887          GO TO 1310-READ-MSTR.                                    
02888                                                                   
02889      WRITE COMP-OUT-RECORD.                                       
02890                                                                   
02891  1310-READ-MSTR.                                                  
02892      IF CMI-END-OF-FILE                                           
02893          MOVE COMP-IN-RECORD     TO  COMPENSATION-MASTER          
02894          GO TO 1399-EXIT.                                         
02895                                                                   
02896      READ COMM-MSTR-IN AT END                                     
02897          MOVE 'Y'                TO  CMI-END-OF-FILE-SW           
02898          MOVE HIGH-VALUE         TO  COMP-IN-RECORD               
02899                                      COMPENSATION-MASTER          
02900          GO TO 1399-EXIT.                                         
02901                                                                   
02902      MOVE COMP-IN-RECORD         TO  COMPENSATION-MASTER.         
02903                                                                   
02904      IF CO-CTL-1 = PRE-CTL-1                                      
02905          GO TO 1360-SET-NEW-CARR-COMP.                            
02906                                                                   
02907  1320-RESET-REMIT.                                                
02908      IF CO-CARRIER = PRE-CARR                                     
02909          GO TO 1340-SET-COMPANY.                                  
02910                                                                   
02911  1330-SET-CARRIER.                                                
02912      MOVE CO-CARRIER             TO  PRE-CARR.                    
02913      MOVE SAVE-COMPANY-NAME (1)  TO  SAVE-COMPANY-NAME (2)        
02914                                      SAVE-COMPANY-NAME (3).       
02915      MOVE RMT-LEVEL (1)          TO  RMT-LEVEL (2)                
02916                                      RMT-LEVEL (3)                
02917                                      RMT-LEVEL (4).               
02918                                                                   
02919  1340-SET-COMPANY.                                                
02920      IF CO-GROUPING = PRE-GROUP                                   
02921          GO TO 1350-SET-AGENT.                                    
02922                                                                   
02923      MOVE CO-GROUPING            TO  PRE-GROUP.                   
02924      MOVE SAVE-COMPANY-NAME (2)  TO  SAVE-COMPANY-NAME (3).       
02925      MOVE RMT-LEVEL (2)          TO  RMT-LEVEL (3)                
02926                                      RMT-LEVEL (4).               
02927  EJECT                                                            
02928  1350-SET-AGENT.                                                  
02929      IF CO-RESP-NO = PRE-RESP                                     
02930          GO TO 1360-SET-NEW-CARR-COMP.                            
02931                                                                   
02932      MOVE +0                     TO  SUMM-SW.                     
02933      MOVE CO-RESP-NO             TO  PRE-RESP.                    
02934      MOVE RMT-LEVEL (3)          TO  RMT-LEVEL (4).               
02935                                                                   
02936  1360-SET-NEW-CARR-COMP.                                          
02937      IF NOT CO-COMPANY-TYPE                                       
02938          GO TO 1370-SET-NEW-AGENT.                                
02939                                                                   
02940      MOVE CO-ACCT-NAME           TO  SAVE-COMPANY-NAME (2)        
02941                                      SAVE-COMPANY-NAME (3).       
02942                                                                   
02943      PERFORM 1400-FORM-RMT-RTN  THRU  1499-EXIT.                  
02944                                                                   
02945      MOVE RMTX                   TO  RMT-LEVEL (2)                
02946                                      RMT-LEVEL (3)                
02947                                      RMT-LEVEL (4).               
02948                                                                   
02949      GO TO 1300-MSTR-CONTROL-RTN.                                 
02950                                                                   
02951  1370-SET-NEW-AGENT.                                              

           IF CO-TYPE NOT = 'B' AND 'G'
              GO TO 1380-SET-REMIT
           END-IF

041105*    IF (NOT CO-GEN-AGENT-TYPE)
041105*       AND (CO-TYPE NOT = 'B')
02953 *        GO TO 1380-SET-REMIT.                                    
02954                                                                   
02955      MOVE +1                     TO  SUMM-SW.                     
02956                                                                   
02957      PERFORM 1400-FORM-RMT-RTN  THRU  1499-EXIT.                  
02958                                                                   
02959      MOVE CO-RESP-NO             TO  RMT0.                        
02960      MOVE RMTX                   TO  RMT-LEVEL (4).               
02961                                                                   
02962      GO TO 1300-MSTR-CONTROL-RTN.                                 
02963  EJECT                                                            
02964  1380-SET-REMIT.                                                  
02965      IF NOT CO-ACCOUNT-TYPE                                       
02966          MOVE '0301'             TO  ABEND-CODE                   
               DISPLAY ' NOT CO ACCT TYPE         ' CP-CONTROL
02967          GO TO 9999-ABEND-PGM.                                    
02968                                                                   
02969      PERFORM 1400-FORM-RMT-RTN  THRU  1499-EXIT.                  
02970                                                                   
02971      MOVE CO-ACCOUNT             TO  RMT0.                        
02972      MOVE RMTX                   TO  BILX.                        
02973      MOVE RMT-LEVEL (4)          TO  RMTX.                        
02974      MOVE +0                     TO  S-TT-FACE  S-LF-PREM         
02975                                      S-AH-PREM  S-TT-PREM         
02976                                      S-LF-REFD  S-AH-REFD         
02977                                      S-TT-REFD  S-LF-COMM         
02978                                      S-AH-COMM  S-TT-COMM
                                           WS-TOT-REF-COMM
02979                                      S-CC-PYMT  S-RR-PYMT
062519                                     s-rr-pymt-adj
02980                                      S-TT-PYMT  S-LF-CLM          
02981                                      S-AH-CLM   S-TT-CLM          
02982                                      S-BEG-BAL  S-WRT-OFF         
02983                                      S-END-BAL                    
02984                                      S-TT-PREM-BILL               
02985                                      S-TT-COMM-BILL               
02986                                      S-TT-PYMT-BILL               
091306                                     PGCTR TPGCTR STMT-SW
02988      MOVE +066                   TO  LNCTR.                       
02989                                                                   
02990      IF (DTE-PGM-OPT = 1  OR  4  OR  6)  AND                      
02991          CO-BAL-FWD NOT = ZERO                                    
02992          MOVE +1                 TO  STMT-SW.                     
02993                                                                   
02994      MOVE 'Y'                    TO  FIRST-CLAIM-SW.              
02995                                                                   
02996  1399-EXIT.                                                       
02997      EXIT.                                                        
02998  EJECT                                                            
02999  1400-FORM-RMT-RTN.                                               
03000      MOVE CO-ACCT-NAME           TO  LJ-NAME.                     
03001      MOVE +1                     TO  LJ-NDX1  LJ-NDX2.            
03002                                                                   
03003      IF LJ-CHAR (LJ-NDX2) NOT = SPACES OR                         
03004         LJ-NAME = SPACES                                          
03005          GO TO 1440-FORM-RMT.                                     
03006                                                                   
03007  1410-LEFT-JUSTIFY.                                               
03008      ADD +1                      TO  LJ-NDX2.                     
03009                                                                   
03010      IF LJ-CHAR (LJ-NDX2) = SPACE                                 
03011          GO TO 1410-LEFT-JUSTIFY.                                 
03012                                                                   
03013  1420-LOOP-2.                                                     
03014      MOVE LJ-CHAR (LJ-NDX2)      TO  LJ-CHAR (LJ-NDX1).           
03015      ADD +1                      TO  LJ-NDX2.                     
03016                                                                   
03017  1430-LOOP-3.                                                     
03018      ADD +1                      TO  LJ-NDX1.                     
03019                                                                   
03020      IF LJ-NDX1 GREATER +30                                       
03021          GO TO 1440-FORM-RMT.                                     
03022                                                                   
03023      IF LJ-NDX2 GREATER +30                                       
03024          MOVE SPACES             TO  LJ-CHAR (LJ-NDX1)            
03025          GO TO 1430-LOOP-3.                                       
03026                                                                   
03027      GO TO 1420-LOOP-2.                                           
03028                                                                   
03029  1440-FORM-RMT.                                                   
03030      MOVE SPACES                 TO  RMTX.                        
03031      MOVE CO-MAIL-NAME           TO  RMT1.                        
03032      MOVE LJ-NAME                TO  RMT2.                        
03033      MOVE CO-ADDR-1              TO  RMT3.                        
03034      MOVE CO-ADDR-2              TO  RMT4.                        
051810     MOVE SPACES                 TO  RMT5
051810     STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
051810        DELIMITED BY '  ' INTO RMT5
051810     END-STRING
03036      MOVE CO-ZIP                 TO  WORK-ZIP-CODE.               
03037      MOVE SPACES                 TO  LINE5-ZIP-CODE.              
03038                                                                   
03039      IF WZC-POS-1 NOT NUMERIC                                     
03040          MOVE WZC-POST-CD1       TO  LINE5-POST-CODE1             
03041          MOVE WZC-POST-CD2       TO  LINE5-POST-CODE2             
03042      ELSE                                                         
03043          MOVE WZC-PRIME          TO  LINE5-ZIP-FIVE               
03044          IF WZC-PLUS4 = '0000'  OR  SPACES                        
03045              MOVE SPACES         TO  LINE5-ZIP-DASH               
03046                                      LINE5-ZIP-FOUR               
03047          ELSE                                                     
03048              MOVE '-'            TO  LINE5-ZIP-DASH               
03049              MOVE WZC-PLUS4      TO  LINE5-ZIP-FOUR.              
03050                                                                   
03051      MOVE LINE5-ZIP-CODE         TO  RMT-ZIP.                     
03052                                                                   
03053      IF RMT1 = RMT2                                               
03054          MOVE SPACES             TO  RMT1.                        
03055                                                                   
03056      MOVE +0                     TO  LJ-NDX1.                     
03057                                                                   
03058  1450-CHECK-TWICE.                                                
03059      ADD +1                      TO  LJ-NDX1.                     
03060                                                                   
03061      IF RMT1 = SPACES                                             
03062          MOVE RMT2               TO  RMT1                         
03063          MOVE SPACES             TO  RMT2.                        
03064                                                                   
03065      IF RMT2 = SPACES                                             
03066          MOVE RMT3               TO  RMT2                         
03067          MOVE SPACES             TO  RMT3.                        
03068                                                                   
03069      IF RMT3 = SPACES                                             
03070          MOVE RMT4               TO  RMT3                         
03071          MOVE SPACES             TO  RMT4.                        
03072                                                                   
03073      IF RMT4 = SPACES                                             
03074          MOVE RMT5               TO  RMT4                         
03075          MOVE SPACES             TO  RMT5.                        
03076                                                                   
03077      IF RMT5 = SPACES                                             
03078          MOVE RMT6               TO  RMT5                         
03079          MOVE SPACES             TO  RMT6.                        
03080                                                                   
03081      IF LJ-NDX1 LESS +2                                           
03082          GO TO 1450-CHECK-TWICE.                                  
03083                                                                   
03084  1499-EXIT.                                                       
03085      EXIT.                                                        
03086  EJECT                                                            
03087  1500-RD-CP-RTN.                                                  
03088      READ COMM-TRAN-IN INTO CP-RECORD AT END                      
03089          MOVE HIGH-VALUES        TO  COMM-PREM-RECORD             
03090                                      CUR-CP-SEQ                   
03091          GO TO 1599-EXIT.                                         
03092                                                                   
03093      COPY ELCCOMM1.                                               
03094                                                                   
03095      MOVE CP-CARR-GROUP          TO  CUR-CP-CARR-GROUP.           
03096      MOVE CP-REMIT               TO  CUR-CP-RESP.                 
03097      MOVE CP-ACCOUNT             TO  CUR-CP-ACCT.                 
03098                                                                   
03099  1599-EXIT.                                                       
03100      EXIT.                                                        
03101  EJECT                                                            
03102  1600-FORM-CP-RTN.                                                
03103      MOVE '#'                    TO  CCM-WK.                      
03104      MOVE CP-CARR-GROUP          TO  CCW-CARR-GROUP.              
03105      MOVE CP-REMIT               TO  CCW-RESP-NO.                 
03106      MOVE CP-ACCOUNT             TO  CCW-ACCOUNT.                 
03107      MOVE CP-AM-NO               TO  CCW-AM-NO.                   
03108      MOVE +0                     TO  CCW-PREM                     
03109                                      CCW-COMM                     
03110                                      CCW-PMTS                     
03111                                      CCW-BEG-BAL                  
03112                                      CCW-END-BAL                  
03113                                      CCW-OV-L-PREM                
03114                                      CCW-OV-A-PREM                
03115                                      CCW-OV-LIFE                  
03116                                      CCW-OV-AH                    
011904                                     CCW-OV-AH-RFND
040504                                     CCW-DLR-INC
011410                                     CCW-LF-LMBA-FEE
011410                                     CCW-AH-LMBA-FEE
03117                                      CCW-OV-B-L-PREM              
03118                                      CCW-OV-B-A-PREM              
03119                                      CCW-OV-B-LIFE                
03120                                      CCW-OV-B-AH
011410                                     CCW-OV-L-CNT
011410                                     CCW-OV-A-CNT.
03121      MOVE CP-AC-DESC             TO  CCW-NAME.                    
03122      MOVE 'N'                    TO  CCW-BAL-CTL.                 
03123                                                                   
03124      IF CP-ACCTG                                                  
03125          GO TO 1610-ACCTG.                                        
03126                                                                   
03127      IF CP-OVERWT                                                 
03128          GO TO 1620-OVERWT.                                       
03129                                                                   
03130      IF CP-RC-OVERWT                                              
03131          GO TO 1630-RC-OVERWT.                                    
03132                                                                   
03133      GO TO 1699-EXIT.                                             
03134                                                                   
03135  1610-ACCTG.                                                      
03136      MOVE '5'                    TO  CCW-TYPE.                    
03137      MOVE LOW-VALUES             TO  CCW-AM-NO.                   
                                                                   
020211     MOVE SPACES                 TO CCW-NAME
020211     STRING CP-AC-DESC (1:11) CP-AC-DESC (14:10) ' '
020211        CP-AC-MAINT-DATE (3:2) '/' CP-AC-MAINT-DATE (5:2)
020211        '/' CP-AC-MAINT-DATE (1:2) DELIMITED BY SIZE
020211        INTO CCW-NAME
020211     END-STRING

03139      COMPUTE CCW-PMTS  =  CP-AC-PMT  -  CP-AC-CHG.                
03140                                                                   
03141      GO TO 1640-WRT.                                              
03142  EJECT                                                            
03143  1620-OVERWT.                                                     
03144      IF CP-OW-LF-PRM-BILLED NOT NUMERIC                           
03145         MOVE ZEROS               TO  CP-OW-LF-PRM-BILLED.         
03146                                                                   
03147      IF CP-OW-AH-PRM-BILLED NOT NUMERIC                           
03148         MOVE ZEROS               TO  CP-OW-AH-PRM-BILLED.         
03149                                                                   
122002     IF NOT CP-MONTHLY-ISSUE
100703        IF (CP-LF-TYPE NOT = '00' AND '  ' AND LOW-VALUES
091911               AND 'DD')
030504           OR (CP-OVERWT)
03150            MOVE CP-OW-LF-PRM        TO  CCW-OV-L-PREM
03151            ADD  CP-OW-LF-PRM-ALT    TO  CCW-OV-L-PREM
03153            MOVE CP-OW-LF-PRM-BILLED TO  CCW-OV-B-L-PREM
011410           MOVE CP-OW-LF-CNT        TO  CCW-OV-L-CNT
100703        END-IF
03152         MOVE CP-OW-AH-PRM        TO  CCW-OV-A-PREM
03154         MOVE CP-OW-AH-PRM-BILLED TO  CCW-OV-B-A-PREM
011410        MOVE CP-OW-AH-CNT        TO  CCW-OV-A-CNT
122002     END-IF
122002        .
03155                                                                   
03156  1630-RC-OVERWT.                                                  
03157      IF CP-OW-LF-COM-BILLED NOT NUMERIC                           
03158         MOVE ZEROS               TO  CP-OW-LF-COM-BILLED.         
03159                                                                   
03160      IF CP-OW-AH-COM-BILLED NOT NUMERIC                           
03161         MOVE ZEROS               TO  CP-OW-AH-COM-BILLED.         
03162                                                                   
03163      MOVE '6'                    TO  CCW-TYPE.                    
122002     IF NOT CP-MONTHLY-ISSUE
091911        IF CP-COM-TYPE = 'I' OR 'J' OR 'N'
011410           COMPUTE CCW-DLR-INC = CP-OW-AH-COM + CP-OW-LF-COM
040504        ELSE
                 IF CP-COM-TYPE = 'B' OR 'L' OR 'A'
011410              MOVE CP-OW-LF-COM  TO  CCW-LF-LMBA-FEE
011410              MOVE CP-OW-AH-COM  TO  CCW-AH-LMBA-FEE
                 ELSE
011410              MOVE CP-OW-LF-COM  TO CCW-OV-LIFE
011410              ADD CP-OW-LF-COM-ALT
011410                                 TO  CCW-OV-LIFE
011410              MOVE CP-OW-AH-COM  TO  CCW-OV-AH
                 END-IF
040504        END-IF
011904        MOVE CP-OW-AH-RFND       TO  CCW-OV-AH-RFND
03167         MOVE CP-OW-LF-COM-BILLED TO  CCW-OV-B-LIFE
03168         MOVE CP-OW-AH-COM-BILLED TO  CCW-OV-B-AH
122002     END-IF

122002        .
03169                                                                   
03170  1640-WRT.                                                        
03171      PERFORM 1700-WRT-CP-RTN  THRU  1799-EXIT.                    
03172                                                                   
03173  1699-EXIT.                                                       
03174      EXIT.                                                        
03175  EJECT                                                            
03176  1700-WRT-CP-RTN.                                                 
03177      WRITE CCM-WK.                                                
03178                                                                   
03179  1799-EXIT.                                                       
03180      EXIT.                                                        
03181  EJECT                                                            
03182  1800-READ-NOTES.                                                 
03183      MOVE WS-SORT-REC            TO  SAVE-WS-SORT-REC.            
03184      MOVE DTE-CLASIC-COMPANY-CD  TO  CN-COMPANY-CD.               
03185                                                                   
03186      IF FROM-PENDING-BUSINESS                                     
03187          MOVE PB-CARRIER         TO  CN-CARRIER                   
03188          MOVE PB-GROUPING        TO  CN-GROUPING                  
03189          MOVE PB-STATE           TO  CN-STATE                     
03190          MOVE PB-ACCOUNT         TO  CN-ACCOUNT                   
03191          MOVE PB-CERT-EFF-DT     TO  CN-CERT-EFF-DT               
03192          MOVE PB-CERT-NO         TO  CN-CERT-NO
041320         if pb-cancellation
041320            move '2'             to  cn-record-type
041320         else
041320            move '1'             to cn-record-type
041320         end-if
03193      ELSE                                                         
03194          MOVE CP-ALT-CARRIER     TO  CN-CARRIER                   
03195          MOVE CP-ALT-COMPANY     TO  CN-GROUPING                  
03196          MOVE CP-ALT-STATE       TO  CN-STATE                     
03197          MOVE CP-ALT-ACCOUNT     TO  CN-ACCOUNT                   
03198          MOVE '3'                TO  DC-OPTION-CODE               
03199          MOVE CP-ALT-EFF-CC      TO  DC-ALPHA-CEN-N               
03200          MOVE CP-ALT-EFF-YR      TO  DC-YMD-YEAR                  
03201          MOVE CP-ALT-EFF-MO      TO  DC-YMD-MONTH                 
03202          MOVE CP-ALT-EFF-DA      TO  DC-YMD-DAY                   
03203          PERFORM 1900-DATE-CONVERSION  THRU  1999-EXIT            
03204          MOVE DC-BIN-DATE-1      TO  CN-CERT-EFF-DT               
03205          MOVE CP-ALT-CERT-NO     TO  CN-CERT-NO
041320         if cp-rc-cancel or cp-cancel
041320            move '2'             to cn-record-type
041320         else
041320            move '1'             to cn-record-type
041320         end-if
041320     end-if
03206                                                                   
03207      READ ERNOTE.                                                 
03208                                                                   
03209      IF ERNOTE-FILE-STATUS = '23'                                 
03210          GO TO 1820-RESTORE-SORT-REC.                             
03211                                                                   
03212      IF ERNOTE-FILE-STATUS NOT = '00'                             
03213          MOVE 'ERROR OCCURED READ  - ERNOTE'                      
03214                                   TO  WS-ABEND-MESSAGE            
03215          MOVE ERNOTE-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        
03216          GO TO ABEND-PGM.                                         
03217                                                                   
03218      MOVE SPACES                 TO  WS-SRT02-REC.                
03219                                                                   
03220 *  THE FOLLOWING 'MON' CODE WILL CAUSE THE FIRST THREE CERT NOTES,
03221 *  IF ANY, TO BE PRINTED ON THE BILL.                             
03222      IF DTE-CLIENT = 'MON'                                        
03223          MOVE 1                         TO STRT-LN                
03224          IF CN-BILLING-END-LINE-NO NOT NUMERIC OR                 
03225             CN-BILLING-END-LINE-NO LESS 4                         
03226                MOVE 3     TO END-LN                               
03227                GO TO 1805-SKIP                                    
03228             ELSE                                                  
03229                MOVE CN-BILLING-END-LINE-NO TO END-LN              
03230                GO TO 1805-SKIP.                                   
03231                                                                   
03232      IF CN-BILLING-START-LINE-NO NUMERIC                          
03233          MOVE CN-BILLING-START-LINE-NO  TO  STRT-LN               
03234      ELSE                                                         
03235          MOVE ZERO                      TO  STRT-LN.              
03236                                                                   
03237      IF CN-BILLING-END-LINE-NO NUMERIC                            
03238          MOVE CN-BILLING-END-LINE-NO  TO  END-LN                  
03239      ELSE                                                         
03240          MOVE ZERO                    TO  END-LN.                 
03241                                                                   
03242  1805-SKIP.                                                       
03243      IF STRT-LN = ZERO AND                                        
03244         END-LN  = ZERO                                            
03245          GO TO 1820-RESTORE-SORT-REC.                             
03246  EJECT                                                            
03247  1810-BUILD-AND-RELEASE.                                          
03248      IF DTE-CLIENT = 'MON'                                        
03249        IF CN-LINE (STRT-LN) = SPACE                               
03250           GO TO 1815-ADD-TO-STRT-LN.                              
03251                                                                   
03252      ADD 1                       TO  WS-SRT-REC-SFX.              
03253      MOVE CN-LINE (STRT-LN)      TO  WS-SRT02-REC.                
03254      MOVE 'N'                    TO  WS-SRT-SUPP-REC-TYP.         
03255      PERFORM 2000-RELEASE-SORT-REC  THRU  2099-EXIT.              
03256                                                                   
03257  1815-ADD-TO-STRT-LN.                                             
03258      ADD +1                      TO  STRT-LN.                     
03259                                                                   
03260      IF STRT-LN NOT GREATER END-LN                                
03261          GO TO 1810-BUILD-AND-RELEASE.                            
020711
020711     IF PARM-UPDATE-ERNOTE NOT EQUAL 'UPDATE'
020711         GO TO 1820-RESTORE-SORT-REC
020711     END-IF.
020711
020711     IF WS-ISSUE OR WS-CANCEL OR WS-CASH-ISSUE OR WS-CASH-CANCEL
020711         NEXT SENTENCE
020711     ELSE
020711         GO TO 1820-RESTORE-SORT-REC
020711     END-IF.
020711
020711     MOVE ZERO                   TO CN-BILLING-START-LINE-NO
020711                                    CN-BILLING-END-LINE-NO.
020711     MOVE 'E562'                 TO CN-LAST-MAINT-USER.
020711     MOVE WS-CURR-BIN-DT         TO CN-LAST-MAINT-DT.
020711     MOVE WS-CURR-TIME           TO CN-LAST-MAINT-HHMMSS.
020711     REWRITE CERTIFICATE-NOTE.
020711     IF ERNOTE-FILE-STATUS NOT = '00'                             
020711         MOVE 'ERROR OCCURED REWRITE  - ERNOTE'                      
020711                                 TO  WS-ABEND-MESSAGE            
020711         MOVE ERNOTE-FILE-STATUS TO  WS-ABEND-FILE-STATUS        
020711         GO TO ABEND-PGM
020711     END-IF.                                         
03262                                                                   
03263  1820-RESTORE-SORT-REC.                                           
020811     MOVE WS-SRT-REC-SFX         TO  SAVE-SRT-SFX.
03264      MOVE SAVE-WS-SORT-REC       TO  WS-SORT-REC.                 
03265                                                                   
03266  1899-EXIT.                                                       
03267      EXIT.                                                        
03268  EJECT                                                            
03269  1900-DATE-CONVERSION.                                            
03270      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   
03271                                                                   
03272  1999-EXIT.                                                       
03273      EXIT.                                                        
03274  EJECT                                                            
03275  2000-RELEASE-SORT-REC.                                           
03276      IF DUMMY-RECORD-RELEASED                                     
03277          GO TO 2099-EXIT.                                         
03278                                                                   
03279      IF DTE-PGM-OPT = 3                                           
03280          MOVE '99 '              TO  WS-SRT-REC-TYPE              
03281          MOVE 'Y'                TO  SRT-RECORD-SWITCH.           
03282                                                                   
03283      IF PRV-SRT-KEY-A = LOW-VALUES                                
03284          MOVE WS-SRT-KEY-A       TO  PRV-SRT-KEY-A.               
03285                                                                   
03286      RELEASE SORT-REC  FROM  WS-SORT-REC.                         
03287                                                                   
03288      IF WS-SRT-KEY-A = PRV-SRT-KEY-A                              
03289          MOVE ACTIVITY-SWITCH    TO  PREV-ACT-SWITCH              
03290          GO TO 2099-EXIT.                                         
03291                                                                   
03292  2010-RELEASE-SORT-TWO.                                           
PEMUNI*    IF HAS-ACTIVITY
PEMUNI*        MOVE 'AA'               TO  PRV-SRT-REC-TYPE
PEMUNI*    ELSE
PEMUNI*        MOVE 'XX'               TO  PRV-SRT-REC-TYPE.
PEMUNI     IF HAS-ACTIVITY
PEMUNI         MOVE '%%'               TO  PRV-SRT-REC-TYPE
PEMUNI     ELSE
PEMUNI         MOVE '&&'               TO  PRV-SRT-REC-TYPE
PEMUNI     END-IF

03298      MOVE PRV-SRT-KEY            TO  SRT-KEY.                     
03299      MOVE SPACES                 TO  SRT-CPTR.                    
03300                                                                   
03301      RELEASE SORT-REC.                                            
03302                                                                   
03303  2020-CONTINUE.                                                   
03304      MOVE WS-SRT-KEY-A           TO  PRV-SRT-KEY-A.               
03305                                                                   
03306      IF WS-SRT-REC-TYPE GREATER '01 ' AND LESS '12 '                
03307          MOVE 'Y'                TO  ACTIVITY-SWITCH              
03308      ELSE                                                         
03309          MOVE 'N'                TO  ACTIVITY-SWITCH.             
03310                                                                   
03311  2099-EXIT.                                                       
03312      EXIT.                                                        
03313  EJECT                                                            
03314  2100-READ-PEND-BUS.                                              
03315      MOVE DTE-CLASIC-COMPANY-CD  TO  PB-COMPANY-CD-A1.            
03316      MOVE CP-ALT-CARRIER         TO  PB-CARRIER.                  
03317      MOVE CP-ALT-COMPANY         TO  PB-GROUPING.                 
03318      MOVE CP-ALT-STATE           TO  PB-STATE.                    
03319      MOVE CP-ACCOUNT             TO  PB-ACCOUNT.                  
03320      MOVE '3'                    TO  DC-OPTION-CODE.              
03321      MOVE CP-ALT-EFF-CC          TO  DC-ALPHA-CEN-N               
03322      MOVE CP-ALT-EFF-YR          TO  DC-YMD-YEAR                  
03323      MOVE CP-ALT-EFF-MO          TO  DC-YMD-MONTH                 
03324      MOVE CP-ALT-EFF-DA          TO  DC-YMD-DAY                   
03325                                                                   
03326      PERFORM 1900-DATE-CONVERSION  THRU  1999-EXIT.               
03327                                                                   
03328      MOVE DC-BIN-DATE-1          TO  PB-CERT-EFF-DT.              
03329      MOVE CP-ALT-CERT-NO         TO  PB-CERT-NO.                  
03330      MOVE ZEROS                  TO  PB-ALT-CHG-SEQ-NO.           
03331                                                                   
03332      IF CP-CANCEL                                                 
03333          MOVE '2'                TO  PB-RECORD-TYPE               
03334      ELSE                                                         
03335          MOVE '1'                TO  PB-RECORD-TYPE.              
03336                                                                   
03337      READ ERPNDE.                                                 
03338                                                                   
03339      IF ERPNDE-FILE-STATUS = '23'                                 
03340          GO TO 2199-EXIT.                                         
03341                                                                   
03342      IF ERPNDE-FILE-STATUS NOT = '00'                             
03343          MOVE 'ERROR OCCURED READ  - ERPNDE'                      
03344                                   TO  WS-ABEND-MESSAGE            
03345          MOVE ERPNDE-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        
03346          GO TO ABEND-PGM.                                         
03347                                                                   
03348      IF CP-CANCEL                                                 
03349          GO TO 2110-CHANGE-REFUNDS.                               
03350                                                                   
03351      IF CP-ALT-OVERRIDE-BOTH                                      
03352          MOVE WS-SRT02-LF-PREM   TO  WS-SRT02-LF-PRM-PR           
03353          MOVE PB-I-LF-PREMIUM-AMT                                 
03354                                  TO  WS-SRT02-LF-PREM             
03355          MOVE WS-SRT02-LF-PREM-ALT                                
03356                                  TO  WS-SRT02-LF-PRM-PR-ALT       
03357          MOVE PB-I-LF-ALT-PREMIUM-AMT                             
03358                                  TO  WS-SRT02-LF-PREM-ALT         
03359          MOVE WS-SRT02-AH-PREM   TO  WS-SRT02-AH-PRM-PR           
03360          MOVE PB-I-AH-PREMIUM-AMT                                 
03361                                  TO  WS-SRT02-AH-PREM             
03362          MOVE 'B'                TO  WS-SRT02-PREM-OVERRIDE       
03363          GO TO 2199-EXIT.                                         
03364  EJECT                                                            
03365      IF CP-ALT-OVERRIDE-LIFE                                      
03366          MOVE WS-SRT02-LF-PREM   TO  WS-SRT02-LF-PRM-PR           
03367          MOVE PB-I-LF-PREMIUM-AMT                                 
03368                                  TO  WS-SRT02-LF-PREM             
03369          MOVE WS-SRT02-LF-PREM-ALT                                
03370                                  TO  WS-SRT02-LF-PRM-PR-ALT       
03371          MOVE PB-I-LF-ALT-PREMIUM-AMT                             
03372                                  TO  WS-SRT02-LF-PREM-ALT         
03373          MOVE 'L'                TO  WS-SRT02-PREM-OVERRIDE       
03374          GO TO 2199-EXIT.                                         
03375                                                                   
03376      IF CP-ALT-OVERRIDE-AH                                        
03377          MOVE WS-SRT02-AH-PREM     TO  WS-SRT02-AH-PRM-PR         
03378          MOVE PB-I-AH-PREMIUM-AMT  TO  WS-SRT02-AH-PREM           
03379          MOVE 'A'                  TO  WS-SRT02-PREM-OVERRIDE     
03380          GO TO 2199-EXIT.                                         
03381                                                                   
03382  2110-CHANGE-REFUNDS.                                             
03383      IF CP-ALT-OVERRIDE-BOTH                                      
03384          MOVE WS-SRT03-LF-REF     TO  WS-SRT03-LF-REF-REV         
03385          MOVE PB-C-LF-CANCEL-AMT  TO  WS-SRT03-LF-REF             
03386          MOVE WS-SRT03-AH-REF     TO  WS-SRT03-AH-REF-REV         
03387          MOVE PB-C-AH-CANCEL-AMT  TO  WS-SRT03-AH-REF             
03388          MOVE 'B'                 TO  WS-SRT03-REF-OVERRIDE       
03389          GO TO 2199-EXIT.                                         
03390                                                                   
03391      IF CP-ALT-OVERRIDE-LIFE                                      
03392          MOVE WS-SRT03-LF-REF     TO  WS-SRT03-LF-REF-REV         
03393          MOVE PB-C-LF-CANCEL-AMT  TO  WS-SRT03-LF-REF             
03394          MOVE 'L'                 TO  WS-SRT03-REF-OVERRIDE       
03395          GO TO 2199-EXIT.                                         
03396                                                                   
03397      IF CP-ALT-OVERRIDE-AH                                        
03398          MOVE WS-SRT03-AH-REF     TO  WS-SRT03-AH-REF-REV         
03399          MOVE PB-C-AH-CANCEL-AMT  TO  WS-SRT03-AH-REF             
03400          MOVE 'A'                 TO  WS-SRT03-REF-OVERRIDE.      
03401                                                                   
03402  2199-EXIT.                                                       
03403      EXIT.                                                        
03404  EJECT                                                            
03405  2200-READ-CHECK-FILE.                                            
03406      MOVE WS-SORT-REC            TO  SAVE-WS-SORT-REC.            
03407      MOVE DTE-CLASIC-COMPANY-CD  TO  CH-COMPANY-CD.               
03408      MOVE CP-ALT-CARRIER         TO  CH-CARRIER.                  
03409      MOVE CP-ALT-COMPANY         TO  CH-GROUPING.                 
03410      MOVE CP-ALT-STATE           TO  CH-STATE.                    
03411      MOVE CP-ALT-ACCOUNT         TO  CH-ACCOUNT.                  
03412      MOVE '3'                    TO  DC-OPTION-CODE.              
03413      MOVE CP-ALT-EFF-CC          TO  DC-ALPHA-CEN-N               
03414      MOVE CP-ALT-EFF-YR          TO  DC-YMD-YEAR                  
03415      MOVE CP-ALT-EFF-MO          TO  DC-YMD-MONTH                 
03416      MOVE CP-ALT-EFF-DA          TO  DC-YMD-DAY                   
03417                                                                   
03418      PERFORM 1900-DATE-CONVERSION  THRU  1999-EXIT.               
03419                                                                   
03420      MOVE DC-BIN-DATE-1          TO  CH-CERT-EFF-DT.              
03421      MOVE CP-ALT-CERT-NO         TO  CH-CERT-NO.                  
03422      MOVE +1                     TO  CH-SEQUENCE-NO.              
03423      MOVE CH-CONTROL-PRIMARY     TO  SAV-CH-KEY.                  
03424                                                                   
03425      START ERCHEK  KEY NOT LESS THAN  CH-CONTROL-PRIMARY.         
03426                                                                   
03427      IF ERCHEK-FILE-STATUS = '23'                                 
03428          GO TO 2220-RESTORE-SORT.                                 
03429                                                                   
03430  2210-READ-NEXT.                                                  
03431      READ ERCHEK  NEXT RECORD.                                    
03432                                                                   
03433      IF ERCHEK-FILE-STATUS = '23' OR '10'                         
03434          GO TO 2220-RESTORE-SORT.                                 
03435                                                                   
03436      IF ERCHEK-FILE-STATUS NOT = '00'                             
03437          MOVE 'ERROR OCCURED READ  - ERCHEK'                      
03438                                   TO  WS-ABEND-MESSAGE            
03439          MOVE ERCHEK-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        
03440          GO TO ABEND-PGM.                                         
03441                                                                   
03442      MOVE CH-CONTROL-PRIMARY     TO  TST-CH-KEY.                  
03443                                                                   
03444      IF TST-CH-CONTROL-PRIMARY NOT = SAV-CH-CONTROL-PRIMARY       
03445          GO TO 2220-RESTORE-SORT.                                 
03446                                                                   
010616     if ((WS-SRT-REC-TYPE = '03 ')
010616        and (ch-check-origin-sw = 'R')
010616        and (ch-void-dt = low-values))
010616                    or
010616        ((WS-SRT-REC-TYPE = '02 ')
010616        and (ch-check-origin-sw = 'C')
010616        and (ch-void-dt = low-values))
010616        continue
010616     else
010616        go to 2210-read-next
010616     end-if 

03447      ADD 1                       TO  WS-SRT-REC-SFX.              
03448                                                                   
03449      MOVE SPACES                 TO  WS-SRT02-REC.                
03450      MOVE CH-PAYEE-NAME-1        TO  WS-SRT03A-PAYEE.
010616     move ch-amount-paid         to  ws-srt03a-check-amt
03451      MOVE ' '                    TO  DC-OPTION-CODE.              
03452      MOVE CH-CHECK-WRITTEN-DT    TO  DC-BIN-DATE-1.               
03453                                                                   
03454      PERFORM 1900-DATE-CONVERSION  THRU  1999-EXIT.               
03455                                                                   
03456      MOVE DC-GREG-DATE-CYMD      TO  WS-SRT03A-CHEK-DT.           
03457      MOVE CH-CHECK-NO            TO  WS-SRT03A-CHEK-NUM.          
03458      MOVE 'C'                    TO  WS-SRT-SUPP-REC-TYP.         
03459                                                                   
03460      PERFORM 2000-RELEASE-SORT-REC  THRU  2099-EXIT.              
03461                                                                   
03462      GO TO 2210-READ-NEXT.                                        
03463                                                                   
03464  2220-RESTORE-SORT.                                               
03465      MOVE SAVE-WS-SORT-REC       TO  WS-SORT-REC.                 
03466                                                                   
03467  2299-EXIT.                                                       
03468      EXIT.                                                        
03469  EJECT                                                            
03470  2300-READ-PENDING-BUS.                                           
03471      IF DTE-PGM-OPT = 3                                           
03472          GO TO 2999-EXIT.                                         
03473                                                                   
03474      MOVE LOW-VALUES             TO  PB-CONTROL-BY-ACCOUNT.       
03475      MOVE DTE-CLASIC-COMPANY-CD  TO  PB-COMPANY-CD-A1.            
03476      MOVE PB-CONTROL-BY-ACCOUNT  TO  SAV-PB-KEY.                  
03477                                                                   
03478      START ERPNDE  KEY NOT LESS THAN  PB-CONTROL-BY-ACCOUNT.      
03479                                                                   
03480      IF ERPNDE-FILE-STATUS = '23'                                 
03481          GO TO 2999-EXIT.                                         
03482                                                                   
03483      IF ERPNDE-FILE-STATUS NOT = ZERO                             
03484          MOVE 'ERROR OCCURED START - ERPNDE'                      
03485                                   TO  WS-ABEND-MESSAGE            
03486          MOVE ERPNDE-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        
03487          GO TO ABEND-PGM.                                         
03488                                                                   
03489  2310-READ-PENDING-NEXT.                                          
03490      READ ERPNDE  NEXT RECORD.                                    
03491                                                                   
03492      IF ERPNDE-FILE-STATUS = '23'  OR '10'                        
03493          GO TO 2999-EXIT.                                         
03494                                                                   
03495      IF ERPNDE-FILE-STATUS NOT = '00'                             
03496          MOVE 'ERROR OCCURED READ  - ERPNDE'                      
03497                                   TO  WS-ABEND-MESSAGE            
03498          MOVE ERPNDE-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        
03499          GO TO ABEND-PGM.                                         
03500                                                                   
03501      IF DTE-CLASIC-COMPANY-CD NOT = PB-COMPANY-CD                 
03502          GO TO 2999-EXIT.                                         
03503                                                                   
03504      IF PB-CREDIT-ACCEPT-DT = WS-RUN-DT OR LOW-VALUES             
03505             NEXT SENTENCE                                         
03506      ELSE                                                         
03507          GO TO 2310-READ-PENDING-NEXT.                            
03508                                                                   
03509      IF PB-BATCH-TRAILER                                          
03510          GO TO 2310-READ-PENDING-NEXT.                            
03511                                                                   
03512      IF PB-ALT-CHG-SEQ-NO NOT = ZEROS                             
03513          GO TO 2310-READ-PENDING-NEXT.                            
03514                                                                   
03515      IF PB-CANCELLATION                                           
03516          GO TO 2400-PROCESS-CANCELS.                              
03517                                                                   
03518      IF PB-RECORD-ON-HOLD  OR                                     
03519         PB-RECORD-RETURNED OR                                     
03520         PB-POLICY-IS-DECLINED OR                                  
03521         PB-POLICY-IS-VOIDED                                       
03522          GO TO 2320-HOLDS-N-RETURNS.                              
03523                                                                   
03524      IF PB-FATAL-ERRORS  OR                                       
03525         PB-UNFORCED-ERRORS                                        
03526          NEXT SENTENCE                                            
03527      ELSE                                                         
03528          GO TO 2310-READ-PENDING-NEXT.                            
03529                                                                   
03530      IF DTE-COMPENSATION-ACCESS  =  '1'  OR  '3'                  
03531          MOVE '0'                TO  WS-SRT-CARR                  
03532      ELSE                                                         
03533          MOVE PB-CARRIER         TO  WS-SRT-CARR.                 
03534                                                                   
03535      IF DTE-COMPENSATION-ACCESS  =  '2'  OR  '3'                  
03536          MOVE '000000'           TO  WS-SRT-GROUP                 
03537      ELSE                                                         
03538          MOVE PB-GROUPING        TO  WS-SRT-GROUP.                
03539                                                                   

032905     PERFORM VARYING CLAS-INDEXCN FROM +1 BY +1 UNTIL
032905        (CLAS-INDEXCN > CLAS-MAXCN)
032905        OR (PB-CARRIER  = CARRIER-SUB (CLAS-INDEXCN))
032905     END-PERFORM
032905     IF CLAS-INDEXCN > CLAS-MAXCN
032905        DISPLAY ' PROBLEM FINDING CARRIER ' PB-CARRIER
032905     END-IF

03540      MOVE PB-ACCOUNT             TO  WS-SRT-ACCT.                 
03541      MOVE PB-SV-REMIT-TO         TO  WS-SRT-REMIT.                
03542      MOVE '04 '                   TO  WS-SRT-REC-TYPE.             
03543      MOVE 'Y'                    TO  ACTIVITY-SWITCH.             
03544      MOVE ZERO                   TO  WS-SRT-REC-SFX.              
03545      MOVE PB-CERT-NO             TO  WS-SRT-CERT.                 
03546      MOVE ' '                    TO  DC-OPTION-CODE.              
03547      MOVE PB-CERT-EFF-DT         TO  DC-BIN-DATE-1.               
03548                                                                   
03549      PERFORM 1900-DATE-CONVERSION  THRU  1999-EXIT.               
03550                                                                   
03551      MOVE DC-GREG-DATE-CYMD       TO  WS-SRT-LOW-DT.              
03552                                                                   
03553      IF DTE-PGM-OPT = 1  OR  2                                    
03554          MOVE DC-GREG-DATE-CYMD  TO  WS-SRT-LOW-DT                
03555      ELSE                                                         
03556          IF DTE-PGM-OPT = 4  OR  5                                
03557              MOVE PB-CERT-NO      TO  WS-SRT-LOW                  
03558          ELSE                                                     
03559              IF DTE-PGM-OPT = 6  OR  7                            
03560                  MOVE PB-I-NAME   TO  WS-SRT-LOW.                 
03561                                                                   
060712     set from-2310 to true
091911     IF PB-I-LIFE-BENEFIT-CD NOT = '00' AND '  ' AND 'DD'
011410        MOVE PB-I-LIFE-BENEFIT-CD TO CLAS-LOOK
011410        PERFORM 0600-FIND-LF     THRU 0699-EXIT
060712        IF CLAS-INDEXL <= CLAS-MAXL                             
011410           MOVE CLAS-I-BEN-CATEGORY (CLAS-INDEXL)
011410                                 TO WS-SRT02-LF-BEN-CAT
060712        end-if
011410     END-IF
011410
011410     IF PB-I-AH-BENEFIT-CD NOT = '00' AND '  '
011410        MOVE PB-I-AH-BENEFIT-CD TO CLAS-LOOK
011410        PERFORM 0500-FIND-AH     THRU 0599-EXIT
060712        if clas-indexa <= clas-maxa
011410           MOVE CLAS-I-BEN-CATEGORY (CLAS-INDEXA)
011410                                 TO WS-SRT02-AH-BEN-CAT
060712        end-if
011410     END-IF

060712     move ' '                    to ws-from-para-2310
03562      MOVE ' '                    TO  WS-SRT-SUPP-REC-TYP.         
03563      MOVE PB-I-NAME              TO  WS-SRT02-NAME.               
03564      MOVE PB-CERT-NO             TO  WS-SRT02-CERT.               
03565      MOVE DC-GREG-DATE-CYMD      TO  WS-SRT02-EFF-DT.             
03566      MOVE PB-I-AGE               TO  WS-SRT02-AGE.                
03567      MOVE PB-I-LF-TERM           TO  WS-SRT02-LF-TERM.            
03568      MOVE PB-I-AH-TERM           TO  WS-SRT02-AH-TERM.            
03569      MOVE 'N'                    TO  WS-SRT02-OB-ON.              
03570      MOVE PB-I-LF-BENEFIT-AMT    TO  WS-SRT02-FACE.               
03571      MOVE PB-I-LF-ALT-BENEFIT-AMT                                 
03572                                  TO  WS-SRT02-FACE-ALT.           
03573                                                                   
03574      MOVE PB-I-LF-PREMIUM-AMT    TO  WS-SRT02-LF-PREM.            
03575      MOVE PB-I-LF-ALT-PREMIUM-AMT                                 
03576                                  TO  WS-SRT02-LF-PREM-ALT.        
03577                                                                   
03578      MOVE ZEROS                  TO  WS-SRT02-LF-COMM             
03579                                      WS-SRT02-LF-COMM-ALT         
03580                                      WS-SRT02-LF-COM-PCT.         
03581      MOVE PB-I-AH-BENEFIT-AMT    TO  WS-SRT02-AH-BEN.             
03582      MOVE PB-I-AH-PREMIUM-AMT    TO  WS-SRT02-AH-PREM.            
03583      MOVE ZEROS                  TO  WS-SRT02-AH-COMM             
03584                                      WS-SRT02-AH-COM-PCT          

032905     IF SEC-PAY-CARRIER (CLAS-INDEXCN)
032905        MOVE PB-I-AH-BENEFIT-CD  TO WS-SRT02-AH-TYP
032905        MOVE PB-I-LF-BENEFIT-CD  TO WS-SRT02-LF-TYP
032905     ELSE
032905        MOVE PB-I-AH-ABBR        TO  WS-SRT02-AH-TYP
032905        MOVE PB-I-LF-ABBR        TO  WS-SRT02-LF-TYP
032905     END-IF

03587      MOVE PB-I-LF-PREM-CALC      TO  WS-SRT02-LF-PRM-PR.          
03588      MOVE PB-I-LF-ALT-PREM-CALC  TO  WS-SRT02-LF-PRM-PR-ALT.      
03589      MOVE PB-I-AH-PREM-CALC      TO  WS-SRT02-AH-PRM-PR.          
092815*    MOVE PB-I-MEMBER-NO         TO  WS-SRT02-MEMBER-NO.          
           IF PB-I-POLICY-IS-MONTHLY
              MOVE 'M'                 TO  WS-SRT02-OB-ON
           END-IF
03591                                                                   
091911     IF WS-SRT02-LF-TYP = SPACES OR ZEROS OR 'DD'
03593          NEXT SENTENCE                                            
03594      ELSE                                                         
03595          IF PB-I-JOINT-COMMISSION = ZEROS                         
03596              COMPUTE WS-SRT02-LF-COMM =                           
03597                 (PB-I-LIFE-COMMISSION * PB-I-LF-PREMIUM-AMT)      
03598               + (PB-I-LIFE-COMMISSION * PB-I-LF-ALT-PREMIUM-AMT)  
03599              MOVE PB-I-LIFE-COMMISSION                            
03600                                   TO  WS-SRT02-LF-COM-PCT         
03601          ELSE                                                     
03602              COMPUTE WS-SRT02-LF-COMM =                           
03603                 (PB-I-JOINT-COMMISSION * PB-I-LF-PREMIUM-AMT)     
03604               + (PB-I-JOINT-COMMISSION * PB-I-LF-ALT-PREMIUM-AMT) 
03605              MOVE PB-I-JOINT-COMMISSION                           
03606                                   TO  WS-SRT02-LF-COM-PCT.        
03607                                                                   
03608      IF WS-SRT02-AH-TYP = SPACES OR ZEROS                         
03609          NEXT SENTENCE                                            
03610      ELSE                                                         
03611          COMPUTE WS-SRT02-AH-COMM =                               
03612              PB-I-AH-COMMISSION * PB-I-AH-PREMIUM-AMT             
03613              MOVE PB-I-AH-COMMISSION                              
03614                                   TO  WS-SRT02-AH-COM-PCT.        

092815     move pb-i-vin               to ws-srt02-vin

03616      PERFORM 1800-READ-NOTES        THRU 1899-EXIT.               
03617      PERFORM 2000-RELEASE-SORT-REC  THRU 2099-EXIT.               
03618      PERFORM 2500-FORMAT-ERRORS     THRU 2599-EXIT.               
03619                                                                   
03620      GO TO 2310-READ-PENDING-NEXT.                                
03621                                                                   
03622  2320-HOLDS-N-RETURNS.                                            
03623      IF DTE-COMPENSATION-ACCESS  =  '1'  OR  '3'                  
03624          MOVE '0'                TO  WS-SRT-CARR                  
03625      ELSE                                                         
03626          MOVE PB-CARRIER         TO  WS-SRT-CARR.                 
03627                                                                   
03628      IF DTE-COMPENSATION-ACCESS  =  '2'  OR  '3'                  
03629          MOVE '000000'           TO  WS-SRT-GROUP                 
03630      ELSE                                                         
03631          MOVE PB-GROUPING        TO  WS-SRT-GROUP.                
03632                                                                   
03633      MOVE PB-ACCOUNT             TO  WS-SRT-ACCT.                 
03634      MOVE PB-SV-REMIT-TO         TO  WS-SRT-REMIT.                
03635                                                                   
03636      IF PB-RECORD-ON-HOLD                                         
03637          MOVE '05 '               TO  WS-SRT-REC-TYPE              
03638      ELSE                                                         
03639          IF PB-RECORD-RETURNED OR                                 
03640             PB-POLICY-IS-DECLINED                                 
03641              MOVE '06 '          TO  WS-SRT-REC-TYPE              
03642          ELSE                                                     
03643              MOVE '07 '          TO  WS-SRT-REC-TYPE.             
03644                                                                   

032905     PERFORM VARYING CLAS-INDEXCN FROM +1 BY +1 UNTIL
032905        (CLAS-INDEXCN > CLAS-MAXCN)
032905        OR (PB-CARRIER  = CARRIER-SUB (CLAS-INDEXCN))
032905     END-PERFORM
032905     IF CLAS-INDEXCN > CLAS-MAXCN
032905        DISPLAY ' PROBLEM FINDING CARRIER ' PB-CARRIER
032905     END-IF

03645      MOVE 'Y'                    TO  ACTIVITY-SWITCH.             
03646      MOVE ZERO                   TO  WS-SRT-REC-SFX.              
03647      MOVE PB-CERT-NO             TO  WS-SRT-CERT.                 
03648      MOVE ' '                    TO  DC-OPTION-CODE.              
03649      MOVE PB-CERT-EFF-DT         TO  DC-BIN-DATE-1.               
03650                                                                   
03651      PERFORM 1900-DATE-CONVERSION  THRU  1999-EXIT.               
03652                                                                   
03653      MOVE DC-GREG-DATE-CYMD     TO  WS-SRT-LOW-DT.                
03654  EJECT                                                            
03655      IF DTE-PGM-OPT = 1  OR  2                                    
03656          MOVE DC-GREG-DATE-CYMD  TO  WS-SRT-LOW-DT                
03657      ELSE                                                         
03658          IF DTE-PGM-OPT = 4  OR  5                                
03659              MOVE PB-CERT-NO      TO  WS-SRT-LOW                  
03660          ELSE                                                     
03661              IF DTE-PGM-OPT = 6  OR  7                            
03662                  MOVE PB-I-NAME   TO  WS-SRT-LOW.                 
03663                                                                   
03664      MOVE ' '                    TO  WS-SRT-SUPP-REC-TYP.         
03665      MOVE PB-I-NAME              TO  WS-SRT02-NAME.               
03666      MOVE PB-CERT-NO             TO  WS-SRT02-CERT.               
03667      MOVE DC-GREG-DATE-CYMD      TO  WS-SRT02-EFF-DT.             
03668      MOVE PB-I-AGE               TO  WS-SRT02-AGE.                
03669      MOVE PB-I-LF-TERM           TO  WS-SRT02-LF-TERM.            
03670      MOVE PB-I-AH-TERM           TO  WS-SRT02-AH-TERM.            
03671      MOVE 'N'                    TO  WS-SRT02-OB-ON.              
03672      MOVE PB-I-LF-BENEFIT-AMT    TO  WS-SRT02-FACE.               
03673      MOVE PB-I-LF-ALT-BENEFIT-AMT                                 
03674                                  TO  WS-SRT02-FACE-ALT.           
03675      MOVE PB-I-LF-PREMIUM-AMT    TO  WS-SRT02-LF-PREM.            
03676      MOVE PB-I-LF-ALT-PREMIUM-AMT                                 
03677                                  TO  WS-SRT02-LF-PREM-ALT.        
03678      MOVE ZEROS                  TO  WS-SRT02-LF-COMM             
03679                                      WS-SRT02-LF-COMM-ALT         
03680                                      WS-SRT02-LF-COM-PCT.         
03681      MOVE PB-I-AH-BENEFIT-AMT    TO  WS-SRT02-AH-BEN.             
03682      MOVE PB-I-AH-PREMIUM-AMT    TO  WS-SRT02-AH-PREM.            
03683      MOVE ZEROS                  TO  WS-SRT02-AH-COMM             
03684                                      WS-SRT02-AH-COM-PCT.         

032905     IF SEC-PAY-CARRIER (CLAS-INDEXCN)
032905        MOVE PB-I-AH-BENEFIT-CD  TO WS-SRT02-AH-TYP
032905        MOVE PB-I-LF-BENEFIT-CD  TO WS-SRT02-LF-TYP
032905     ELSE
032905        MOVE PB-I-AH-ABBR        TO  WS-SRT02-AH-TYP
032905        MOVE PB-I-LF-ABBR        TO  WS-SRT02-LF-TYP
032905     END-IF

03687      MOVE PB-I-LF-PREM-CALC      TO  WS-SRT02-LF-PRM-PR.          
03688      MOVE PB-I-LF-ALT-PREM-CALC  TO  WS-SRT02-LF-PRM-PR-ALT.      
03689      MOVE PB-I-AH-PREM-CALC      TO  WS-SRT02-AH-PRM-PR.          
092815*    MOVE PB-I-MEMBER-NO         TO  WS-SRT02-MEMBER-NO.          
03691                                                                   
091911     IF WS-SRT02-LF-TYP = SPACES OR ZEROS OR 'DD'
03693          NEXT SENTENCE                                            
03694      ELSE                                                         
03695          IF PB-I-JOINT-COMMISSION = ZEROS                         
03696              COMPUTE WS-SRT02-LF-COMM =                           
03697                 (PB-I-LIFE-COMMISSION * PB-I-LF-PREMIUM-AMT)      
03698               + (PB-I-LIFE-COMMISSION * PB-I-LF-ALT-PREMIUM-AMT)  
03699              MOVE PB-I-LIFE-COMMISSION                            
03700                                   TO  WS-SRT02-LF-COM-PCT         
03701          ELSE                                                     
03702              COMPUTE WS-SRT02-LF-COMM =                           
03703                 (PB-I-JOINT-COMMISSION * PB-I-LF-PREMIUM-AMT)     
03704               + (PB-I-JOINT-COMMISSION * PB-I-LF-ALT-PREMIUM-AMT) 
03705              MOVE PB-I-JOINT-COMMISSION                           
03706                                   TO  WS-SRT02-LF-COM-PCT.        
03707                                                                   
03708      IF WS-SRT02-AH-TYP = SPACES OR ZEROS                         
03709          NEXT SENTENCE                                            
03710      ELSE                                                         
03711          COMPUTE WS-SRT02-AH-COMM =                               
03712              PB-I-AH-COMMISSION * PB-I-AH-PREMIUM-AMT             
03713              MOVE PB-I-AH-COMMISSION                              
03714                                   TO  WS-SRT02-AH-COM-PCT.        

092815     move pb-i-vin               to ws-srt02-vin

03716      PERFORM 1800-READ-NOTES        THRU 1899-EXIT.               
03717      PERFORM 2000-RELEASE-SORT-REC  THRU  2099-EXIT.              
03718      PERFORM 2500-FORMAT-ERRORS     THRU  2599-EXIT.              
03719                                                                   
03720      GO TO 2310-READ-PENDING-NEXT.                                
03721  EJECT                                                            
03722  2400-PROCESS-CANCELS.                                            
03723      IF PB-RECORD-ON-HOLD  OR                                     
03724         PB-RECORD-RETURNED                                        
03725          GO TO 2410-HOLDS-N-RETURNS.                              
03726                                                                   
03727      IF PB-FATAL-ERRORS  OR                                       
03728         PB-UNFORCED-ERRORS                                        
03729          NEXT SENTENCE                                            
03730      ELSE                                                         
03731          GO TO 2310-READ-PENDING-NEXT.                            
03732                                                                   
03733      IF DTE-COMPENSATION-ACCESS  =  '1'  OR  '3'                  
03734          MOVE '0'                TO  WS-SRT-CARR                  
03735      ELSE                                                         
03736          MOVE PB-CARRIER         TO  WS-SRT-CARR.                 
03737                                                                   
03738      IF DTE-COMPENSATION-ACCESS  =  '2'  OR  '3'                  
03739          MOVE '000000'           TO  WS-SRT-GROUP                 
03740      ELSE                                                         
03741          MOVE PB-GROUPING        TO  WS-SRT-GROUP.                
03742                                                                   
03743      MOVE PB-ACCOUNT             TO  WS-SRT-ACCT.                 
03744      MOVE PB-SV-REMIT-TO         TO  WS-SRT-REMIT.                
03745      MOVE '08 '                  TO  WS-SRT-REC-TYPE.             
03746      MOVE 'Y'                    TO  ACTIVITY-SWITCH.             
03747      MOVE ZERO                   TO  WS-SRT-REC-SFX.              
03748      MOVE PB-CERT-NO             TO  WS-SRT-CERT.                 
03749      MOVE ' '                    TO  DC-OPTION-CODE.              
03750      MOVE PB-CERT-EFF-DT         TO  DC-BIN-DATE-1.               
03751                                                                   
03752      PERFORM 1900-DATE-CONVERSION  THRU  1999-EXIT.               
03753                                                                   
03754      MOVE DC-GREG-DATE-CYMD     TO  WS-SRT-LOW-DT.                
03755                                                                   
03756      IF DTE-PGM-OPT = 1  OR  2                                    
03757          MOVE DC-GREG-DATE-CYMD  TO  WS-SRT-LOW-DT                
03758      ELSE                                                         
03759          IF DTE-PGM-OPT = 4  OR  5                                
03760              MOVE PB-CERT-NO      TO  WS-SRT-LOW                  
03761          ELSE                                                     
03762              IF DTE-PGM-OPT = 6  OR  7                            
03763                  MOVE PB-I-NAME   TO  WS-SRT-LOW.                 
03764                                                                   
03765      MOVE ' '                    TO  WS-SRT-SUPP-REC-TYP.         
03766      MOVE PB-CI-INSURED-NAME     TO  WS-SRT03-NAME.               
03767      MOVE PB-CERT-NO             TO  WS-SRT03-CERT.               
03768      MOVE DC-GREG-DATE-CYMD      TO  WS-SRT03-EFF-DT.             
03769      MOVE ' '                    TO  DC-OPTION-CODE.              
03770      MOVE PB-C-LF-CANCEL-DT      TO  DC-BIN-DATE-1.               
03771                                                                   
03772      PERFORM 1900-DATE-CONVERSION  THRU  1999-EXIT.               
03773                                                                   
03774      MOVE DC-GREG-DATE-CYMD      TO  WS-SRT03-LF-CAN-DT.          
03775      MOVE PB-C-LF-CANCEL-AMT     TO  WS-SRT03-LF-REF.             
03776      MOVE ZEROS                  TO  WS-SRT03-LF-COM-REF.         

032905     IF SEC-PAY-CARRIER (CLAS-INDEXCN)
032905        MOVE PB-CI-AH-BENEFIT-CD TO WS-SRT03-AH-TYP
032905        MOVE PB-CI-LF-BENEFIT-CD TO WS-SRT03-LF-TYP
032905     ELSE
032905        MOVE PB-CI-AH-ABBR       TO  WS-SRT03-AH-TYP
032905        MOVE PB-CI-LF-ABBR       TO  WS-SRT03-LF-TYP
032905     END-IF

03779      MOVE PB-C-AH-CANCEL-DT      TO  DC-BIN-DATE-1.               
03780                                                                   
03781      PERFORM 1900-DATE-CONVERSION  THRU  1999-EXIT.               

011410     IF PB-CI-LF-BENEFIT-CD NOT = '00' AND '  '
011410        MOVE PB-CI-LF-BENEFIT-CD TO  CLAS-LOOK
011410        PERFORM 0600-FIND-LF     THRU  0699-EXIT
011410        MOVE CLAS-I-BEN-CATEGORY (CLAS-INDEXL)
011410                                 TO WS-SRT03-LF-BEN-CAT
011410     END-IF

           IF PB-CI-AH-BENEFIT-CD NOT = '00' AND '  '
02570         MOVE PB-CI-AH-BENEFIT-CD TO  CLAS-LOOK
02575         PERFORM 0500-FIND-AH     THRU  0599-EXIT
011410        MOVE CLAS-I-BEN-CATEGORY (CLAS-INDEXA)
011410                                 TO WS-SRT03-AH-BEN-CAT
           END-IF

03783      MOVE DC-GREG-DATE-CYMD      TO  WS-SRT03-AH-CAN-DT.          
03784      MOVE PB-C-AH-CANCEL-AMT     TO  WS-SRT03-AH-REF.             
03785      MOVE PB-C-LF-REF-CALC       TO  WS-SRT03-LF-REF-REV.         

03787      MOVE ZEROS                  TO  WS-SRT03-AH-COM-REF          
03788                                      WS-SRT03-TOT-REF             
03789                                      WS-SRT03-COM-TOTREF.         

092705     IF CLAS-I-BEN-CATEGORY (CLAS-INDEXA) = 'G' OR 'L'
              COMPUTE WS-SRT03-AH-REF-REV = PB-CI-LF-ALT-PREMIUM-AMT
                 * (PB-C-AH-CANCEL-AMT / PB-CI-AH-PREMIUM-AMT)
              COMPUTE WS-SRT03-AH-COM-REF = 
                 PB-C-AH-CANCEL-AMT - WS-SRT03-AH-REF-REV
           ELSE
              MOVE PB-C-AH-REF-CALC    TO WS-SRT03-AH-REF-REV
03797         COMPUTE WS-SRT03-AH-COM-REF =                                
03798            PB-CI-AH-COMMISSION * PB-C-AH-CANCEL-AMT
           END-IF
           
03790      MOVE PB-CI-LIFE-COMMISSION  TO  WS-SRT03-LF-COM-PCT.         
03791      MOVE PB-CI-AH-COMMISSION    TO  WS-SRT03-AH-COM-PCT.         
092815*    MOVE PB-CI-MEMBER-NO        TO  WS-SRT03-MEMBER-NO.          
03793                                                                   
03794      COMPUTE WS-SRT03-LF-COM-REF =                                
03795          PB-CI-LIFE-COMMISSION * PB-C-LF-CANCEL-AMT.              

092815     move pb-control-by-account  to cs-control-primary
092815     move 'C'                    to cs-trailer-type
092815
092815     move spaces                 to ws-srt03-vin
092815*    display ' about to read elcrtt 0440 '
092815     read elcrtt
092815     if elcrtt-file-status = '10' or '23' or '00'
092815        continue
092815     else
092815        display ' bad file status elcrtt ' elcrtt-file-status
092815        perform abend-pgm
092815     end-if
092815
092815     if elcrtt-file-status = '00'
092815        if cs-vin-number not = spaces and low-values
092815*          display ' found vin ' cp-alt-cert-no
092815           move cs-vin-number    to ws-srt03-vin
092815        end-if
092815     end-if

03800      PERFORM 1800-READ-NOTES        THRU 1899-EXIT.               
03801      PERFORM 2000-RELEASE-SORT-REC  THRU  2099-EXIT.              
03802      PERFORM 2500-FORMAT-ERRORS     THRU  2599-EXIT.              
03803                                                                   
03804      GO TO 2310-READ-PENDING-NEXT.                                
03805                                                                   
03806  2410-HOLDS-N-RETURNS.                                            
03807      IF DTE-COMPENSATION-ACCESS  =  '1'  OR  '3'                  
03808          MOVE '0'                TO  WS-SRT-CARR                  
03809      ELSE                                                         
03810          MOVE PB-CARRIER         TO  WS-SRT-CARR.                 
03811                                                                   
03812      IF DTE-COMPENSATION-ACCESS  =  '2'  OR  '3'                  
03813          MOVE '000000'           TO  WS-SRT-GROUP                 
03814      ELSE                                                         
03815          MOVE PB-GROUPING        TO  WS-SRT-GROUP.                
03816                                                                   
03817      MOVE PB-ACCOUNT             TO  WS-SRT-ACCT.                 
03818      MOVE PB-SV-REMIT-TO         TO  WS-SRT-REMIT.                
03819                                                                   
03820      IF PB-RECORD-ON-HOLD                                         
03821          MOVE '09 '              TO  WS-SRT-REC-TYPE              
03822      ELSE                                                         
03823          MOVE '10 '              TO  WS-SRT-REC-TYPE.             
03824                                                                   
03825      MOVE 'Y'                    TO  ACTIVITY-SWITCH.             
03826      MOVE ZERO                   TO  WS-SRT-REC-SFX.              
03827      MOVE PB-CERT-NO             TO  WS-SRT-CERT.                 
03828      MOVE ' '                    TO  DC-OPTION-CODE.              
03829      MOVE PB-CERT-EFF-DT         TO  DC-BIN-DATE-1.               
03830                                                                   
03831      PERFORM 1900-DATE-CONVERSION  THRU  1999-EXIT.               
03832                                                                   
03833      MOVE DC-GREG-DATE-CYMD     TO  WS-SRT-LOW-DT                 
03834                                                                   
03835      IF DTE-PGM-OPT = 1  OR  2                                    
03836          MOVE DC-GREG-DATE-CYMD  TO  WS-SRT-LOW-DT                
03837      ELSE                                                         
03838          IF DTE-PGM-OPT = 4  OR  5                                
03839              MOVE PB-CERT-NO      TO  WS-SRT-LOW                  
03840          ELSE                                                     
03841              IF DTE-PGM-OPT = 6  OR  7                            
03842                  MOVE PB-I-NAME   TO  WS-SRT-LOW.                 
03843                                                                   
03844      MOVE ' '                    TO  WS-SRT-SUPP-REC-TYP.         
03845      MOVE PB-CI-INSURED-NAME     TO  WS-SRT03-NAME.               
03846      MOVE PB-CERT-NO             TO  WS-SRT03-CERT.               
03847      MOVE DC-GREG-DATE-CYMD      TO  WS-SRT03-EFF-DT.             
03848      MOVE ' '                    TO  DC-OPTION-CODE.              
03849      MOVE PB-C-LF-CANCEL-DT      TO  DC-BIN-DATE-1.               
03850                                                                   
03851      PERFORM 1900-DATE-CONVERSION  THRU  1999-EXIT.               
03852                                                                   
03853      MOVE DC-GREG-DATE-CYMD      TO  WS-SRT03-LF-CAN-DT.          
03854      MOVE PB-C-LF-CANCEL-AMT     TO  WS-SRT03-LF-REF.             

032905     IF SEC-PAY-CARRIER (CLAS-INDEXCN)
032905        MOVE PB-CI-AH-BENEFIT-CD TO WS-SRT03-AH-TYP
032905        MOVE PB-CI-LF-BENEFIT-CD TO WS-SRT03-LF-TYP
032905     ELSE
032905        MOVE PB-CI-AH-ABBR       TO  WS-SRT03-AH-TYP
032905        MOVE PB-CI-LF-ABBR       TO  WS-SRT03-LF-TYP
032905     END-IF

03857      MOVE ZEROS                  TO  WS-SRT03-LF-COM-REF.         
03858      MOVE PB-C-AH-CANCEL-DT      TO  DC-BIN-DATE-1.               
03859                                                                   
03860      PERFORM 1900-DATE-CONVERSION  THRU  1999-EXIT.               
03861                                                                   
03862      MOVE DC-GREG-DATE-CYMD      TO  WS-SRT03-AH-CAN-DT.          
03863      MOVE PB-C-AH-CANCEL-AMT     TO  WS-SRT03-AH-REF.             
03864      MOVE PB-C-LF-REF-CALC       TO  WS-SRT03-LF-REF-REV.         
03865      MOVE PB-C-AH-REF-CALC       TO  WS-SRT03-AH-REF-REV.         
03866      MOVE ZEROS                  TO  WS-SRT03-AH-COM-REF          
03867                                      WS-SRT03-TOT-REF             
03868                                      WS-SRT03-COM-TOTREF.         
03869      MOVE PB-CI-LIFE-COMMISSION  TO  WS-SRT03-LF-COM-PCT.         
03870      MOVE PB-CI-AH-COMMISSION    TO  WS-SRT03-AH-COM-PCT.         
092815*    MOVE PB-CI-MEMBER-NO        TO  WS-SRT03-MEMBER-NO.          
03872                                                                   
03873      COMPUTE WS-SRT03-LF-COM-REF =                                
03874          PB-CI-LIFE-COMMISSION * PB-C-LF-CANCEL-AMT.              
03875                                                                   
03876      COMPUTE WS-SRT03-AH-COM-REF =                                
03877          PB-CI-AH-COMMISSION * PB-C-AH-CANCEL-AMT.                
03878                                                                   
092815     move pb-control-by-account  to cs-control-primary
092815     move 'C'                    to cs-trailer-type
092815
092815     move spaces                 to ws-srt03-vin
092815*    display ' about to read elcrtt 0440 '
092815     read elcrtt
092815     if elcrtt-file-status = '10' or '23' or '00'
092815        continue
092815     else
092815        display ' bad file status elcrtt ' elcrtt-file-status
092815        perform abend-pgm
092815     end-if
092815
092815     if elcrtt-file-status = '00'
092815        if cs-vin-number not = spaces and low-values
092815*          display ' found vin ' cp-alt-cert-no
092815           move cs-vin-number    to ws-srt03-vin
092815        end-if
092815     end-if

03879      PERFORM 1800-READ-NOTES THRU 1899-EXIT.                      
03880                                                                   
03881      PERFORM 2000-RELEASE-SORT-REC  THRU  2099-EXIT.              
03882                                                                   
03883      PERFORM 2500-FORMAT-ERRORS  THRU  2599-EXIT.                 
03884                                                                   
03885      GO TO 2310-READ-PENDING-NEXT.                                
03886  EJECT                                                            
03887  2500-FORMAT-ERRORS.                                              
03888      IF PB-NO-OF-ERRORS = ZERO                                    
03889          GO TO 2599-EXIT.                                         
03890                                                                   
03891      MOVE +1                     TO  SUB.                         
020811     MOVE SAVE-SRT-SFX           TO  WS-SRT-REC-SFX.
03892                                                                   
03893  2510-ERR-LOOP.                                                   
03894      IF PB-NO-OF-ERRORS LESS SUB                                  
03895          GO TO 2599-EXIT.                                         
03896                                                                   
03897      MOVE PB-COMMON-ERROR (SUB) TO WS-ERR-CODE.                   
03898      PERFORM 2600-ERROR-FORMAT  THRU  2699-EXIT.                  
03899      MOVE EM-ERROR-SEVERITY  TO  WS-SRT04-ERR-SEV.                
03900      MOVE EM-ERROR-TEXT      TO  WS-SRT04-ERR-TXT.                
03901      ADD 1                   TO  WS-SRT-REC-SFX.                  
03902      MOVE 'E'                TO  WS-SRT-SUPP-REC-TYP.             
03903      IF WS-SRT04-ERR-SEV NOT = 'W'                                
03904          PERFORM 2000-RELEASE-SORT-REC  THRU  2099-EXIT.          
03905                                                                   
03906      ADD +1  TO  SUB.                                             
03907                                                                   
03908      IF SUB LESS 11                                               
03909          GO TO 2510-ERR-LOOP.                                     
03910                                                                   
03911  2599-EXIT.                                                       
03912       EXIT.                                                       
03913  EJECT                                                            
03914  2600-ERROR-FORMAT.                                               
03915      MOVE WS-ERR-CODE-X          TO  EM-MESSAGE-NUMBER.           
03916                                                                   
03917      READ ELERRS                                                  
03918          KEY IS EM-CONTROL-PRIMARY.                               
03919                                                                   
03920      IF EM-STATUS = '23'                                          
03921          MOVE '****ERROR NUMBER NOT FOUND'  TO  EM-ERROR-TEXT.    
03922                                                                   
03923      MOVE DTE-CLIENT TO E-CLIENT-ID.                              
03924                                                                   
03925                                  COPY ELCERRPD.                   
03926                                                                   
03927      IF EM-ERROR-TEXT-PREFIX = 'LLLLLL'                           
03928          MOVE LIFE-OVERRIDE-L6   TO EM-ERROR-TEXT-PREFIX.         
03929      IF EM-ERROR-TEXT-PREFIX = 'AAAAAA'                           
03930          MOVE AH-OVERRIDE-L6     TO EM-ERROR-TEXT-PREFIX.         
03931                                                                   
03932  2699-EXIT.                                                       
03933      EXIT.                                                        
03934                                                                   
03935  2999-EXIT.                                                       
03936      EXIT.                                                        
03937  EJECT                                                            
03938  3000-OUTPUT-ROUTINE SECTION.                                     
03939      MOVE ZEROS                  TO  NCT-L-ISS    NCT-A-ISS       
03940                                      NCT-L-CAN    NCT-A-CAN       
03941                                      NCT-DTH-CLM  NCT-DIS-CLM     
03942                                      NCT-T-FAC    NCT-T-FAC-ALT   
03943                                      NCT-L-PRM    NCT-L-PRM-ALT   
03944                                      NCT-L-COM    NCT-L-COM-ALT   
03945                                      NCT-A-PRM    NCT-T-PRM       
03946                                      NCT-A-COM    NCT-T-COM       
03947                                      NCT-L-REF    NCT-A-REF       
03948                                      NCT-T-REF    NCT-L-RCM       
03949                                      NCT-A-RCM    NCT-T-RCM       
03950                                      NCT-R-PMT    NCT-C-CHG       
03951                                      NCT-L-CLM    NCT-A-CLM       
03952                                      NCT-T-CLM                    
03953                                      NST-L-ISS    NST-A-ISS       
03954                                      NST-L-CAN    NST-A-CAN       
03955                                      NST-DTH-CLM  NST-DIS-CLM     
03956                                      NST-L-PRM    NST-A-PRM       
03957                                      NST-T-PRM    NST-L-COM       
03958                                      NST-A-COM    NST-T-COM       
03959                                      NST-L-REF    NST-A-REF       
03960                                      NST-T-REF    NST-L-RCM       
03961                                      NST-A-RCM    NST-T-RCM       
03962                                      NST-T-FAC    NST-R-PMT       
03963                                      NST-C-CHG    NST-L-CLM       
03964                                      NST-A-CLM    NST-T-CLM       
03965                                      NST-B-BAL    NST-W-OFF       
03966                                      NST-E-BAL                    
03967                                      NST-L-NET    NST-A-NET       
03968                                      NST-T-NET                    
                                           CNST-L-ISS    CNST-A-ISS
                                           CNST-L-CAN    CNST-A-CAN
                                           CNST-L-PRM    CNST-A-PRM
                                           CNST-T-PRM    CNST-L-COM
                                           CNST-A-COM    CNST-T-COM
                                           CNST-L-REF    CNST-A-REF
                                           CNST-T-REF    CNST-L-RCM
                                           CNST-A-RCM    CNST-T-RCM
                                           CNST-T-FAC
03969                                NFT-L-ISS (1)   NFT-A-ISS (1)
03970                                NFT-L-CAN (1)   NFT-A-CAN (1)
03971                                NFT-DTH-CLM (1) NFT-DIS-CLM (1)
03972                                NFT-L-PRM (1)   NFT-A-PRM (1)
03973                                NFT-T-PRM (1)   NFT-L-COM (1)
03974                                NFT-A-COM (1)   NFT-T-COM (1)
03975                                NFT-L-REF (1)   NFT-A-REF (1)
03976                                NFT-T-REF (1)   NFT-L-RCM (1)
03977                                NFT-L-NET (1)   NFT-A-NET (1)
03978                                NFT-T-NET (1)
03979                                NFT-A-RCM (1)   NFT-T-RCM (1)
03980                                NFT-T-FAC (1)   NFT-R-PMT (1)
03981                                NFT-C-CHG (1)   NFT-L-CLM (1)
03982                                NFT-A-CLM (1)   NFT-T-CLM (1)
03983                                NFT-B-BAL (1)   NFT-W-OFF (1)
03984                                NFT-E-BAL (1)   NFT-ADJCO (1)

           MOVE NEW-FINAL-TOTALS (1)   TO NEW-FINAL-TOTALS (2)
011410                                    NEW-FINAL-TOTALS (3)

           MOVE ZEROS                  TO  NPF-L-PRM (1)
                                           NPF-A-PRM (1)
                                           NPF-T-PRM (1)
                                           NPF-L-REF (1)
                                           NPF-A-REF (1)
                                           NPF-T-REF (1)
                                           NPF-L-NET (1)
                                           NPF-A-NET (1)
                                           NPF-T-NET (1)
                                           NPF-L-COM (1)
                                           NPF-A-COM (1)
                                           NPF-T-COM (1)
                                           NPF-L-RCM (1)
                                           NPF-A-RCM (1)
                                           NPF-T-RCM (1)
                                           NPF-TOT   (1)

           MOVE NEW-FINAL-NP-TOTALS (1) TO NEW-FINAL-NP-TOTALS (2)
011410                                     NEW-FINAL-NP-TOTALS (3)

           MOVE ZEROS                  TO  TOTF-L-PRM (1)
                                           TOTF-A-PRM (1)
                                           TOTF-T-PRM (1)
                                           TOTF-L-REF (1)
                                           TOTF-A-REF (1)
                                           TOTF-T-REF (1)
                                           TOTF-L-NET (1)
                                           TOTF-A-NET (1)
                                           TOTF-T-NET (1)
                                           TOTF-L-COM (1)
                                           TOTF-A-COM (1)
                                           TOTF-T-COM (1)
                                           TOTF-L-RCM (1)
                                           TOTF-A-RCM (1)
                                           TOTF-T-RCM (1)
                                           TOTF-TOT   (1)

           MOVE NEW-FINAL-TOT-TOTALS (1) TO NEW-FINAL-TOT-TOTALS (2)
011410                                      NEW-FINAL-TOT-TOTALS (3)

03986      MOVE LOW-VALUES             TO  PRV-SRT-KEY-A.               
03987      MOVE SPACES                 TO  SAVE-COMPANY-NAME (3)        
03988                                      SAV-CARR-GROUP
011410                                     SAV-CSR SAV-SPPDD
03989                                      BIL0  BIL1  BIL2  BIL3       
03990                                      BIL4  BIL5  BIL6             
03991                                      RMT0  RMT1  RMT2  RMT3       
03992                                      RMT4  RMT5  RMT6.            
03993  EJECT                                                            
03994  3010-RETURN-SORTED-RECS.                                         
03995      RETURN SORT-WORK  INTO  WS-SORT-REC  AT END                  
03996          GO TO 9000-E-O-J.                                        
03997                                                                   
03998      MOVE '0'                    TO  TIC-LINE-SWITCH.             
03999      MOVE ' '                    TO  TIC-TYPE-SWITCH.             
04000                                                                   
04001      IF WS-DUMMY-RECORD                                           
04002          GO TO 9000-E-O-J.                                        
04003                                                                   
04004      IF WS-HAS-ACTIVITY                                           
04005          MOVE 'Y'                TO  ACTIVITY-SWITCH              
04006          MOVE WS-SRT-KEY-A       TO  HLD-SRT-KEY-A                
04007          GO TO 3010-RETURN-SORTED-RECS.                           
04008                                                                   
04009      IF WS-HAS-NO-ACTIVITY                                        
04010          IF WS-SRT-KEY-A = HLD-SRT-KEY-A                          
04011              GO TO 3010-RETURN-SORTED-RECS                        
04012          ELSE                                                     
04013              MOVE 'N'            TO  ACTIVITY-SWITCH              
04014              GO TO 3010-RETURN-SORTED-RECS.                       
04015                                                                   
04016      IF HAS-NO-ACTIVITY                                           
04017          GO TO 3010-RETURN-SORTED-RECS.                           
04018                                                                   
04019      IF PRV-SRT-KEY-A = LOW-VALUES                                
04020          MOVE WS-SRT-KEY-A       TO  SAV-CARR-GROUP               
04021          MOVE WS-SRT-ACCT        TO  BIL0.                        
04022                                                                   
04023      IF WS-SRT-KEY-A NOT = PRV-SRT-KEY-A                          
04024          IF CLM-TOT-NEEDED                                        
04025              MOVE 'N'            TO  CLM-TOT-PRT-SW               
04026              PERFORM 3400-PRT-CLAIM-TOTALS  THRU  3499-XIT.       
04027                                                                   
04028      IF WS-SRT-KEY-A NOT = PRV-SRT-KEY-A                          
04029         MOVE WS-SRT-KEY-A        TO PRV-SRT-KEY-A                
04030         IF NOT WS-COMP-REC                                       
04031            IF SUMMARY-PRT-NOT-NEEDED                            
04032               MOVE 'Y'           TO SUMMARY-PRT-SW               
04033               PERFORM 3600-DUMMY-HEADING
                                       THRU 3699-EXIT      
04034            ELSE                                                 
04035               PERFORM 3700-DUMMY-TOTAL
                                       THRU 3799-EXIT        
04036               MOVE 'Y'           TO SUMMARY-PRT-SW               
04037               PERFORM 3600-DUMMY-HEADING
                                       THRU 3699-EXIT
                 END-IF
04038         ELSE                                                     
04039            IF SUMMARY-PRT-NEEDED                                
04040               PERFORM 3700-DUMMY-TOTAL
                                       THRU 3799-EXIT        
04041               MOVE 'N'           TO SUMMARY-PRT-SW
                 END-IF
              END-IF
              MOVE SPACES              TO PRV-SPP-CATEGORY
           END-IF

04043      IF WS-COMP-REC                                               
04044         MOVE +0                 TO  PGCTR                        
04045         MOVE LNCTR-43-58        TO  LNCTR                        
04046         MOVE 'N'                TO  HEADING-SWITCH               
04047         MOVE 'Y'                TO  REC-02-FIRST-TIME-SW         
052504                                    REC-013-FIRST-TIME-SW
052504                                    REC-017-FIRST-TIME-SW
04048                                     REC-03-FIRST-TIME-SW         
04049                                     REC-04-FIRST-TIME-SW         
04050                                     REC-05-FIRST-TIME-SW         
04051                                     REC-06-FIRST-TIME-SW         
04052                                     REC-07-FIRST-TIME-SW         
04053                                     REC-08-FIRST-TIME-SW         
04054                                     REC-09-FIRST-TIME-SW         
04055                                     REC-10-FIRST-TIME-SW         
04056                                     REC-11-FIRST-TIME-SW         
04057                                     REC-13-FIRST-TIME-SW         
04058         MOVE WS-SRT00-DATA      TO  SAVE-COMPANY-NAME (3)        
011904        PERFORM VARYING CLAS-INDEXCN FROM +1 BY +1 UNTIL
011904        (CLAS-INDEXCN > CLAS-MAXCN)
011904        OR (WS-SRT-CARR = CARRIER-SUB (CLAS-INDEXCN))
011904        END-PERFORM
011904        IF CLAS-INDEXCN > CLAS-MAXCN
011904           DISPLAY ' PROBLEM FINDING CARRIER ' WS-SRT-CARR
011904        END-IF
04059         GO TO 3010-RETURN-SORTED-RECS
011904     END-IF
04060                                                                   
04061      IF WS-REMIT-REC                                              
04062          IF WS-SRT-REC-SFX = 0                                    
04063              MOVE SPACES          TO  RMTX  BILX                  
04064              MOVE WS-SRT01-CARR-GROUP                             
04065                                   TO  SAV-CARR-GROUP              
04066                                       SAV-KSM-CARR-GROUP          
04067              MOVE WS-SRT01-ACCT   TO  BIL0                        
04068                                       SAV-KSM-ACCOUNT             
04069              MOVE WS-SRT01-NAME   TO  BIL1                        
04070                                       SAV-KSM-ACCT-NAME           
04071              MOVE WS-SRT01-RESP   TO  RMT0                        
04072              MOVE WS-SRT01-REMIT  TO  RMT1                        
CIDMOD             MOVE WS-SRT01-CONTROL-NAME
CIDMOD                                  TO  WS-CONTROL-NAME
100307             MOVE WS-SRT01-CSR    TO  SAV-CSR
                   MOVE WS-SRT01-SPPDD  TO  SAV-SPPDD
04073              GO TO 3010-RETURN-SORTED-RECS.                       
04074                                                                   
04075      IF WS-REMIT-REC                                              
04076          IF WS-SRT-REC-SFX = 1                                    
04077              MOVE WS-SRT01-NAME   TO  BIL2                        
04078                                       SAV-KSM-ADDR-1              
04079              MOVE WS-SRT01-REMIT  TO  RMT2                        
04080              GO TO 3010-RETURN-SORTED-RECS.                       
04081                                                                   
04082      IF WS-REMIT-REC                                              
04083          IF WS-SRT-REC-SFX = 2                                    
04084              MOVE WS-SRT01-NAME   TO  BIL3                        
04085                                       SAV-KSM-ADDR-2              
04086              MOVE WS-SRT01-REMIT  TO  RMT3                        
04087              GO TO 3010-RETURN-SORTED-RECS.                       
04088                                                                   
04089      IF WS-REMIT-REC                                              
04090          IF WS-SRT-REC-SFX = 3                                    
04091              MOVE WS-SRT01-NAME   TO  BIL4                        
04092                                       SAV-KSM-CITY-STATE          
04093              MOVE WS-SRT01-REMIT  TO  RMT4                        
04094              GO TO 3010-RETURN-SORTED-RECS.                       
04095                                                                   
04096      IF WS-REMIT-REC                                              
04097          IF WS-SRT-REC-SFX = 4                                    
04098              MOVE WS-SRT01-NAME   TO  BIL5                        
04099                                       SAV-KSM-ZIP                 
04100              MOVE WS-SRT01-REMIT  TO  RMT5                        
04101              GO TO 3010-RETURN-SORTED-RECS.                       
04102  EJECT                                                            
04103      IF WS-REMIT-REC                                              
04104          IF WS-SRT-REC-SFX = 5                                    
04105              MOVE WS-SRT01-NAME   TO  BIL6                        
04106              MOVE WS-SRT01-REMIT  TO  RMT6                        
04107              GO TO 3010-RETURN-SORTED-RECS.                       
04108                                                                   
04109      IF WS-CASH-ISSUE
04110         IF REC-013-FIRST-TIME                                     
04111            MOVE 'Y'              TO CISS-TOT-PRT-SW               
04112            MOVE 'N'              TO REC-013-FIRST-TIME-SW         
04113            MOVE '0'              TO HEADING-SW                   
04114            MOVE HD5AA            TO HD5                          
                 MOVE 'COMP.'          TO HD6A-COMP
                 MOVE 'REVISED     PCT.'
                                       TO HD7A-PCT
04115            MOVE HD6A             TO HD6                          
04116            MOVE HD7A             TO HD7                          
04117            PERFORM 3900-PRT-STMT-HD-RTN
                                       THRU 3999-EXIT
04118            PERFORM 3100-PRT-DETAIL-RTN
                                       THRU 3199-EXIT
04119            GO TO 3010-RETURN-SORTED-RECS
04120         ELSE
04121            PERFORM 3100-PRT-DETAIL-RTN
                                       THRU 3199-EXIT
04122            GO TO 3010-RETURN-SORTED-RECS
              END-IF
           END-IF

           IF WS-SRT-REC-TYPE > '013'
              IF CISS-TOT-NEEDED
                 MOVE 'N'              TO CISS-TOT-PRT-SW
                 PERFORM 3200-PRT-CASH-ISS-TOTALS
                                       THRU  3209-XIT
              END-IF
           END-IF
                 
052504     IF WS-CASH-CANCEL                                                 
04130         IF REC-017-FIRST-TIME                                     
04131            MOVE 'Y'              TO CCAN-TOT-PRT-SW                
04132            MOVE 'N'              TO REC-017-FIRST-TIME-SW         
04133            MOVE 'Y'              TO REC-013-FIRST-TIME-SW         
04134            MOVE '1'              TO HEADING-SW                   
04135            MOVE HD5AB            TO HD5                          
                 MOVE 'COMP.'          TO HD6B-COMP
                 MOVE 'REVISED    PCT.'
                                       TO HD7B-PCT
04136            MOVE HD6B-1           TO HD6                          
04137            MOVE HD7B-1           TO HD7                          
04138            IF LNCTR GREATER LNCTR-34-49                         
04139               PERFORM 3900-PRT-STMT-HD-RTN
                                       THRU 3999-EXIT    
04140               PERFORM 3100-PRT-DETAIL-RTN
                                       THRU 3199-EXIT     
04141               GO TO 3010-RETURN-SORTED-RECS                    
04142            ELSE                                                 
04143               PERFORM 3950-PRT-HD5
                                       THRU 3999-EXIT            
04144               PERFORM 3100-PRT-DETAIL-RTN
                                       THRU 3199-EXIT     
04145               GO TO 3010-RETURN-SORTED-RECS
                 END-IF
04146         ELSE                                                     
04147            PERFORM 3100-PRT-DETAIL-RTN
                                       THRU 3199-EXIT         
04148            GO TO 3010-RETURN-SORTED-RECS
              END-IF
           END-IF
04149                                                                   

           IF WS-SRT-REC-TYPE > '017'
              IF CCAN-TOT-NEEDED
                 MOVE 'N'              TO CCAN-TOT-PRT-SW
                 PERFORM 3300-PRT-CASH-CAN-TOTALS
                                       THRU  3309-XIT
              END-IF
           END-IF
                 
04109      IF WS-ISSUE
04110          IF REC-02-FIRST-TIME                                     
04111              MOVE 'Y'            TO  LIF-TOT-PRT-SW               
04112              MOVE 'N'            TO  REC-02-FIRST-TIME-SW         
04113              MOVE '0'            TO  HEADING-SW                   
04114              MOVE HD5A           TO  HD5                          
                   IF SEC-PAY-CARRIER (CLAS-INDEXCN)
                      IF WS-SRT02-AH-BEN-CAT = 'G' OR 'L'
                         MOVE SPACES      TO HD6A-COMP
                         MOVE ' MINIMUM FEE    '
                                       TO HD7A-PCT
032905                   MOVE ' CODE '    TO HD7A-BEN-HD
                      ELSE
                         MOVE 'COMP.'     TO HD6A-COMP
                         MOVE 'REVISED     PCT.'
                                       TO HD7A-PCT
032905                   MOVE ' TYPE '    TO HD7A-BEN-HD
                      END-IF
                      IF WS-SRT02-LF-BEN-CAT NOT = SPACES
                         MOVE WS-SRT02-LF-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      ELSE
                         MOVE WS-SRT02-AH-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      END-IF
                   END-IF
04115              MOVE HD6A           TO  HD6                          
04116              MOVE HD7A           TO  HD7                          
04117              PERFORM 3900-PRT-STMT-HD-RTN  THRU  3999-EXIT        
04118              PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT         
04119              GO TO 3010-RETURN-SORTED-RECS                        
04120          ELSE                                                     
04121              PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT         
04122              GO TO 3010-RETURN-SORTED-RECS.                       
04123                                                                   
04123                                                                   
04124      IF WS-CANCEL
04125         IF LIF-TOT-NEEDED
04126            MOVE 'N'              TO LIF-TOT-PRT-SW
04127            PERFORM 3200-PRT-LIFE-TOTALS
                                       THRU  3299-XIT
              END-IF
           END-IF
04128                                                                   

04129      IF WS-CANCEL                                                 
04130          IF REC-03-FIRST-TIME                                     
04131              MOVE 'Y'            TO  AH-TOT-PRT-SW                
04132              MOVE 'N'            TO  REC-03-FIRST-TIME-SW         
04133              MOVE 'Y'            TO  REC-02-FIRST-TIME-SW         
04134              MOVE '1'            TO  HEADING-SW                   
04135              MOVE HD5E           TO  HD5                          
                   IF SEC-PAY-CARRIER (CLAS-INDEXCN)
                      IF WS-SRT03-AH-BEN-CAT = 'G' OR 'L'
                         MOVE SPACES      TO HD6B-COMP
                         MOVE ' MINIMUM FEE   '
                                       TO HD7B-PCT
                      ELSE
                         MOVE 'COMP.'     TO HD6B-COMP
                         MOVE 'REVISED    PCT.'
                                       TO HD7B-PCT
                      END-IF
                      IF WS-SRT03-LF-BEN-CAT NOT = SPACES
                         MOVE WS-SRT03-LF-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      ELSE
                         MOVE WS-SRT03-AH-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      END-IF
                   END-IF
04136              MOVE HD6B-1         TO  HD6                          
04137              MOVE HD7B-1         TO  HD7                          
04138              IF LNCTR GREATER LNCTR-34-49                         
04139                  PERFORM 3900-PRT-STMT-HD-RTN  THRU  3999-EXIT    
04140                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04141                  GO TO 3010-RETURN-SORTED-RECS                    
04142              ELSE                                                 
04143                  PERFORM 3950-PRT-HD5  THRU  3999-EXIT            
04144                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04145                  GO TO 3010-RETURN-SORTED-RECS
                   END-IF
04146          ELSE                                                     
04147              PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT         
04148              GO TO 3010-RETURN-SORTED-RECS
               END-IF
           END-IF   
04149                                                                   
04150      IF WS-ISSUE-ERROR                                            
04151          IF LIF-TOT-NEEDED                                        
04152              MOVE 'N'            TO  LIF-TOT-PRT-SW               
04153              PERFORM 3200-PRT-LIFE-TOTALS  THRU  3299-XIT.        
04154  EJECT                                                            
04155      IF WS-ISSUE-ERROR                                            
04156          IF AH-TOT-NEEDED                                         
04157              MOVE 'N'            TO  AH-TOT-PRT-SW                
04158              PERFORM 3300-PRT-AH-TOTALS  THRU  3399-XIT.          
04159                                                                   
04160      IF WS-ISSUE-ERROR                                            
04161          IF REC-04-FIRST-TIME                                     
04162            AND  REC-02-FIRST-TIME                                 
04163              MOVE 'N'              TO  REC-04-FIRST-TIME-SW       
04164              MOVE 'Y'              TO  REC-03-FIRST-TIME-SW       
04165              MOVE '0'              TO  HEADING-SW                 
04166              MOVE HD5B             TO  HD5                        
                   IF SEC-PAY-CARRIER (CLAS-INDEXCN)
                      IF WS-SRT02-AH-BEN-CAT = 'G' OR 'L'
                         MOVE SPACES      TO HD6A-COMP
                         MOVE ' MINIMUM FEE    '
                                       TO HD7A-PCT
032905                   MOVE ' CODE '    TO HD7A-BEN-HD
                      ELSE
                         MOVE 'COMP.'     TO HD6A-COMP
                         MOVE 'REVISED     PCT.'
                                       TO HD7A-PCT
032905                   MOVE ' TYPE '    TO HD7A-BEN-HD
                      END-IF
                      IF WS-SRT02-LF-BEN-CAT NOT = SPACES
                         MOVE WS-SRT02-LF-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      ELSE
                         MOVE WS-SRT02-AH-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      END-IF
                   END-IF
04167              MOVE HD6A             TO  HD6                        
04168              MOVE HD7A             TO  HD7                        
04169              IF LNCTR GREATER LNCTR-34-49                         
04170                  PERFORM 3900-PRT-STMT-HD-RTN  THRU  3999-EXIT    
04171                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04172                  GO TO 3010-RETURN-SORTED-RECS                    
04173              ELSE                                                 
04174                  PERFORM 3950-PRT-HD5  THRU  3999-EXIT            
04175                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04176                  GO TO 3010-RETURN-SORTED-RECS
                   END-IF
04177          ELSE                                                     
04178              IF REC-04-FIRST-TIME                                 
04179                  MOVE 'N'          TO  REC-04-FIRST-TIME-SW       
04180                  MOVE 'Y'          TO  REC-03-FIRST-TIME-SW       
04181                  MOVE '0'          TO  HEADING-SW                 
04182                  MOVE HD5B         TO  HD5                        
                       IF SEC-PAY-CARRIER (CLAS-INDEXCN)
                          IF WS-SRT02-AH-BEN-CAT = 'G' OR 'L'
                             MOVE SPACES      TO HD6A-COMP
                             MOVE ' MINIMUM FEE    '
                                           TO HD7A-PCT
032905                       MOVE ' CODE '    TO HD7A-BEN-HD
                          ELSE
                             MOVE 'COMP.'     TO HD6A-COMP
                             MOVE 'REVISED     PCT.'
                                           TO HD7A-PCT
032905                       MOVE ' TYPE '    TO HD7A-BEN-HD
                          END-IF
                          IF WS-SRT02-LF-BEN-CAT NOT = SPACES
                             MOVE WS-SRT02-LF-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                          ELSE
                             MOVE WS-SRT02-AH-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                          END-IF
                       END-IF
04183                  MOVE HD6A         TO  HD6                        
04184                  MOVE HD7A         TO  HD7                        
04185                  IF LNCTR GREATER LNCTR-34-49                     
04186                      PERFORM 3900-PRT-STMT-HD-RTN                 
04187                          THRU  3999-EXIT                          
04188                      PERFORM 3100-PRT-DETAIL-RTN                  
04189                          THRU  3199-EXIT                          
04190                      GO TO 3010-RETURN-SORTED-RECS                
04191                  ELSE                                             
04192                      MOVE SPACE-1  TO  P-REC                      
04193                      PERFORM 8800-PRT-RTN  THRU  8899-EXIT        
04194                      PERFORM 3950-PRT-HD5                         
04195                      MOVE SPACE-1  TO  P-REC                      
04196                      PERFORM 8800-PRT-RTN  THRU  8899-EXIT        
04197                      PERFORM 3100-PRT-DETAIL-RTN                  
04198                          THRU  3199-EXIT                          
04199                      GO TO 3010-RETURN-SORTED-RECS
                       END-IF
04200              ELSE                                                 
04201                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04202                  GO TO 3010-RETURN-SORTED-RECS
                   END-IF
               END-IF
           END-IF
           .

04204      IF WS-ISSUE-HOLD                                             
04205          IF LIF-TOT-NEEDED                                        
04206              MOVE 'N'            TO  LIF-TOT-PRT-SW               
04207              PERFORM 3200-PRT-LIFE-TOTALS  THRU  3299-XIT.        
04208                                                                   
04209      IF WS-ISSUE-HOLD                                             
04210          IF AH-TOT-NEEDED                                         
04211              MOVE 'N'            TO  AH-TOT-PRT-SW                
04212              PERFORM 3300-PRT-AH-TOTALS  THRU  3399-XIT.          
04213                                                                   
04214      IF WS-ISSUE-HOLD                                             
04215          IF REC-05-FIRST-TIME  AND                                
04216             REC-04-FIRST-TIME  AND                                
04217             REC-02-FIRST-TIME                                     
04218              MOVE 'N'              TO  REC-05-FIRST-TIME-SW       
04219              MOVE 'Y'              TO  REC-03-FIRST-TIME-SW       
04220              MOVE '0'              TO  HEADING-SW                 
04221              MOVE HD5C             TO  HD5                        
                   IF SEC-PAY-CARRIER (CLAS-INDEXCN)
                      IF WS-SRT02-AH-BEN-CAT = 'G' OR 'L'
                         MOVE SPACES      TO HD6A-COMP
                         MOVE ' MINIMUM FEE    '
                                       TO HD7A-PCT
032905                   MOVE ' CODE '    TO HD7A-BEN-HD
                      ELSE
                         MOVE 'COMP.'     TO HD6A-COMP
                         MOVE 'REVISED     PCT.'
                                       TO HD7A-PCT
032905                   MOVE ' TYPE '    TO HD7A-BEN-HD
                      END-IF
                      IF WS-SRT02-LF-BEN-CAT NOT = SPACES
                         MOVE WS-SRT02-LF-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      ELSE
                         MOVE WS-SRT02-AH-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      END-IF
                   END-IF
04222              MOVE HD6A             TO  HD6                        
04223              MOVE HD7A             TO  HD7                        
04224              IF LNCTR GREATER LNCTR-34-49                         
04225                  PERFORM 3900-PRT-STMT-HD-RTN  THRU  3999-EXIT    
04226                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04227                  GO TO 3010-RETURN-SORTED-RECS                    
04228              ELSE                                                 
04229                  PERFORM 3950-PRT-HD5  THRU  3999-EXIT            
04230                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04231                  GO TO 3010-RETURN-SORTED-RECS                    
04232          ELSE                                                     
04233              IF REC-05-FIRST-TIME                                 
04234                  MOVE 'N'          TO  REC-05-FIRST-TIME-SW       
04235                  MOVE 'Y'          TO  REC-03-FIRST-TIME-SW       
04236                  MOVE '0'          TO  HEADING-SW                 
04237                  MOVE HD5C         TO  HD5                        
                     IF SEC-PAY-CARRIER (CLAS-INDEXCN)
                      IF WS-SRT02-AH-BEN-CAT = 'G' OR 'L'
                         MOVE SPACES      TO HD6A-COMP
                         MOVE ' MINIMUM FEE    '
                                       TO HD7A-PCT
032905                   MOVE ' CODE '    TO HD7A-BEN-HD
                      ELSE
                         MOVE 'COMP.'     TO HD6A-COMP
                         MOVE 'REVISED     PCT.'
                                       TO HD7A-PCT
032905                   MOVE ' TYPE '    TO HD7A-BEN-HD
                      END-IF
                      IF WS-SRT02-LF-BEN-CAT NOT = SPACES
                         MOVE WS-SRT02-LF-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      ELSE
                         MOVE WS-SRT02-AH-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      END-IF
                     END-IF
04238                  MOVE HD6A         TO  HD6                        
04239                  MOVE HD7A         TO  HD7                        
04240                  IF LNCTR  IS GREATER THAN  LNCTR-34-49           
04241                      PERFORM 3900-PRT-STMT-HD-RTN                 
04242                          THRU  3999-EXIT                          
04243                      PERFORM 3100-PRT-DETAIL-RTN                  
04244                          THRU  3199-EXIT                          
04245                      GO TO 3010-RETURN-SORTED-RECS                
04246                  ELSE                                             
04247                                                                   
04248                      MOVE SPACE-1  TO  P-REC                      
04249                      PERFORM 8800-PRT-RTN  THRU  8899-EXIT        
04250                      PERFORM 3950-PRT-HD5                         
04251                      MOVE SPACE-1  TO  P-REC                      
04252                      PERFORM 8800-PRT-RTN  THRU  8899-EXIT        
04253                      PERFORM 3100-PRT-DETAIL-RTN                  
04254                          THRU  3199-EXIT                          
04255                      GO TO 3010-RETURN-SORTED-RECS                
04256              ELSE                                                 
04257                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04258                  GO TO 3010-RETURN-SORTED-RECS.                   
04259                                                                   
04260      IF WS-ISSUE-RETURN                                           
04261          IF LIF-TOT-NEEDED                                        
04262              MOVE 'N'            TO  LIF-TOT-PRT-SW               
04263              PERFORM 3200-PRT-LIFE-TOTALS  THRU  3299-XIT.        
04264                                                                   
04265      IF WS-ISSUE-RETURN                                           
04266          IF AH-TOT-NEEDED                                         
04267              MOVE 'N'            TO  AH-TOT-PRT-SW                
04268              PERFORM 3300-PRT-AH-TOTALS  THRU  3399-XIT.          
04269                                                                   
04270      IF WS-ISSUE-RETURN                                           
04271          IF REC-06-FIRST-TIME  AND                                
04272             REC-05-FIRST-TIME  AND                                
04273             REC-04-FIRST-TIME  AND                                
04274             REC-02-FIRST-TIME                                     
04275              MOVE 'N'              TO  REC-06-FIRST-TIME-SW       
04276              MOVE 'Y'              TO  REC-03-FIRST-TIME-SW       
04277              MOVE '0'              TO  HEADING-SW                 
04278              MOVE HD5D             TO  HD5                        
                   IF SEC-PAY-CARRIER (CLAS-INDEXCN)
                      IF WS-SRT02-AH-BEN-CAT = 'G' OR 'L'
                         MOVE SPACES      TO HD6A-COMP
                         MOVE ' MINIMUM FEE    '
                                       TO HD7A-PCT
032905                   MOVE ' CODE '    TO HD7A-BEN-HD
                      ELSE
                         MOVE 'COMP.'     TO HD6A-COMP
                         MOVE 'REVISED     PCT.'
                                       TO HD7A-PCT
032905                   MOVE ' TYPE '    TO HD7A-BEN-HD
                      END-IF
                      IF WS-SRT02-LF-BEN-CAT NOT = SPACES
                         MOVE WS-SRT02-LF-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      ELSE
                         MOVE WS-SRT02-AH-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      END-IF
                   END-IF
04279              MOVE HD6A             TO  HD6                        
04280              MOVE HD7A             TO  HD7                        
04281              IF LNCTR GREATER LNCTR-34-49                         
04282                  PERFORM 3900-PRT-STMT-HD-RTN  THRU  3999-EXIT    
04283                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04284                  GO TO 3010-RETURN-SORTED-RECS                    
04285              ELSE                                                 
04286                  PERFORM 3950-PRT-HD5  THRU  3999-EXIT            
04287                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04288                  GO TO 3010-RETURN-SORTED-RECS                    
04289          ELSE                                                     
04290              IF REC-06-FIRST-TIME                                 
04291                  MOVE 'N'          TO  REC-06-FIRST-TIME-SW       
04292                  MOVE 'Y'          TO  REC-03-FIRST-TIME-SW       
04293                  MOVE '0'          TO  HEADING-SW                 
04294                  MOVE HD5D         TO  HD5                        
                    IF SEC-PAY-CARRIER (CLAS-INDEXCN)
                      IF WS-SRT02-AH-BEN-CAT = 'G' OR 'L'
                         MOVE SPACES      TO HD6A-COMP
                         MOVE ' MINIMUM FEE    '
                                       TO HD7A-PCT
032905                   MOVE ' CODE '    TO HD7A-BEN-HD
                      ELSE
                         MOVE 'COMP.'     TO HD6A-COMP
                         MOVE 'REVISED     PCT.'
                                       TO HD7A-PCT
032905                   MOVE ' TYPE '    TO HD7A-BEN-HD
                      END-IF
                      IF WS-SRT02-LF-BEN-CAT NOT = SPACES
                         MOVE WS-SRT02-LF-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      ELSE
                         MOVE WS-SRT02-AH-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      END-IF
                    END-IF
04295                  MOVE HD6A         TO  HD6                        
04296                  MOVE HD7A         TO  HD7                        
04297                  MOVE SPACE-1      TO  P-REC                      
04298                  IF LNCTR GREATER LNCTR-34-49                     
04299                      PERFORM 3900-PRT-STMT-HD-RTN                 
04300                          THRU  3999-EXIT                          
04301                      PERFORM 3100-PRT-DETAIL-RTN                  
04302                          THRU  3199-EXIT                          
04303                      GO TO 3010-RETURN-SORTED-RECS                
04304                  ELSE                                             
04305                      MOVE SPACE-1  TO  P-REC                      
04306                      PERFORM 8800-PRT-RTN  THRU  8899-EXIT        
04307                      PERFORM 3950-PRT-HD5                         
04308                      MOVE SPACE-1  TO  P-REC                      
04309                      PERFORM 8800-PRT-RTN  THRU  8899-EXIT        
04310                      PERFORM 3100-PRT-DETAIL-RTN                  
04311                          THRU  3199-EXIT                          
04312                      GO TO 3010-RETURN-SORTED-RECS                
04313              ELSE                                                 
04314                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04315                  GO TO 3010-RETURN-SORTED-RECS.                   
04316                                                                   
04317      IF WS-ISSUE-VOIDED                                           
04318          IF LIF-TOT-NEEDED                                        
04319              MOVE 'N'            TO  LIF-TOT-PRT-SW               
04320              PERFORM 3200-PRT-LIFE-TOTALS  THRU  3299-XIT.        
04321                                                                   
04322      IF WS-ISSUE-VOIDED                                           
04323          IF AH-TOT-NEEDED                                         
04324              MOVE 'N'            TO  AH-TOT-PRT-SW                
04325              PERFORM 3300-PRT-AH-TOTALS  THRU  3399-XIT.          
04326                                                                   
04327      IF WS-ISSUE-VOIDED                                           
04328          IF REC-07-FIRST-TIME  AND                                
04329             REC-06-FIRST-TIME  AND                                
04330             REC-05-FIRST-TIME  AND                                
04331             REC-04-FIRST-TIME  AND                                
04332             REC-02-FIRST-TIME                                     
04333              MOVE 'N'              TO  REC-07-FIRST-TIME-SW       
04334              MOVE 'Y'              TO  REC-03-FIRST-TIME-SW       
04335              MOVE '0'              TO  HEADING-SW                 
04336              MOVE HD5L             TO  HD5                        
                   IF SEC-PAY-CARRIER (CLAS-INDEXCN)
                      IF WS-SRT02-AH-BEN-CAT = 'G' OR 'L'
                         MOVE SPACES      TO HD6A-COMP
                         MOVE ' MINIMUM FEE    '
                                       TO HD7A-PCT
032905                   MOVE ' CODE '    TO HD7A-BEN-HD
                      ELSE
                         MOVE 'COMP.'     TO HD6A-COMP
                         MOVE 'REVISED     PCT.'
                                       TO HD7A-PCT
032905                   MOVE ' TYPE '    TO HD7A-BEN-HD
                      END-IF
                      IF WS-SRT02-LF-BEN-CAT NOT = SPACES
                         MOVE WS-SRT02-LF-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      ELSE
                         MOVE WS-SRT02-AH-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      END-IF
                   END-IF
04337              MOVE HD6A             TO  HD6                        
04338              MOVE HD7A             TO  HD7                        
04339              IF LNCTR GREATER LNCTR-34-49                         
04340                  PERFORM 3900-PRT-STMT-HD-RTN  THRU  3999-EXIT    
04341                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04342                  GO TO 3010-RETURN-SORTED-RECS                    
04343              ELSE                                                 
04344                  PERFORM 3950-PRT-HD5  THRU  3999-EXIT            
04345                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04346                  GO TO 3010-RETURN-SORTED-RECS                    
04347          ELSE                                                     
04348              IF REC-07-FIRST-TIME                                 
04349                  MOVE 'N'          TO  REC-07-FIRST-TIME-SW       
04350                  MOVE 'Y'          TO  REC-03-FIRST-TIME-SW       
04351                  MOVE '0'          TO  HEADING-SW                 
04352                  MOVE HD5L         TO  HD5                        
                    IF SEC-PAY-CARRIER (CLAS-INDEXCN)
                      IF WS-SRT02-AH-BEN-CAT = 'G' OR 'L'
                         MOVE SPACES      TO HD6A-COMP
                         MOVE ' MINIMUM FEE    '
                                       TO HD7A-PCT
032905                   MOVE ' CODE '    TO HD7A-BEN-HD
                      ELSE
                         MOVE 'COMP.'     TO HD6A-COMP
                         MOVE 'REVISED     PCT.'
                                       TO HD7A-PCT
032905                   MOVE ' TYPE '    TO HD7A-BEN-HD
                      END-IF
                      IF WS-SRT02-LF-BEN-CAT NOT = SPACES
                         MOVE WS-SRT02-LF-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      ELSE
                         MOVE WS-SRT02-AH-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      END-IF
                    END-IF
04353                  MOVE HD6A         TO  HD6                        
04354                  MOVE HD7A         TO  HD7                        
04355                  MOVE SPACE-1      TO  P-REC                      
04356                  IF LNCTR GREATER LNCTR-34-49                     
04357                      PERFORM 3900-PRT-STMT-HD-RTN                 
04358                          THRU  3999-EXIT                          
04359                      PERFORM 3100-PRT-DETAIL-RTN                  
04360                          THRU  3199-EXIT                          
04361                      GO TO 3010-RETURN-SORTED-RECS                
04362                  ELSE                                             
04363                      MOVE SPACE-1  TO  P-REC                      
04364                      PERFORM 8800-PRT-RTN  THRU  8899-EXIT        
04365                      PERFORM 3950-PRT-HD5                         
04366                      MOVE SPACE-1  TO  P-REC                      
04367                      PERFORM 8800-PRT-RTN  THRU  8899-EXIT        
04368                      PERFORM 3100-PRT-DETAIL-RTN                  
04369                          THRU  3199-EXIT                          
04370                      GO TO 3010-RETURN-SORTED-RECS                
04371              ELSE                                                 
04372                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04373                  GO TO 3010-RETURN-SORTED-RECS.                   
04374                                                                   
04375      IF WS-CANCEL-ERROR                                           
04376          IF LIF-TOT-NEEDED                                        
04377              MOVE 'N'            TO  LIF-TOT-PRT-SW               
04378              PERFORM 3200-PRT-LIFE-TOTALS  THRU  3299-XIT.        
04379                                                                   
04380      IF WS-CANCEL-ERROR                                           
04381          IF AH-TOT-NEEDED                                         
04382              MOVE 'N'            TO  AH-TOT-PRT-SW                
04383              PERFORM 3300-PRT-AH-TOTALS  THRU  3399-XIT.          
04384                                                                   
04385      IF WS-CANCEL-ERROR                                           
04386          IF REC-08-FIRST-TIME AND                                 
04387             REC-03-FIRST-TIME                                     
04388              MOVE 'N'              TO  REC-08-FIRST-TIME-SW       
04389              MOVE '1'              TO  HEADING-SW                 
04390              MOVE HD5F             TO  HD5                        
                   IF SEC-PAY-CARRIER (CLAS-INDEXCN)
                      IF WS-SRT03-AH-BEN-CAT = 'G' OR 'L'
                         MOVE SPACES      TO HD6B-COMP
                         MOVE ' MINIMUM FEE   '
                                       TO HD7B-PCT
                      ELSE
                         MOVE 'COMP.'     TO HD6B-COMP
                         MOVE 'REVISED    PCT.'
                                       TO HD7B-PCT
                      END-IF
                      IF WS-SRT03-LF-BEN-CAT NOT = SPACES
                         MOVE WS-SRT03-LF-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      ELSE
                         MOVE WS-SRT03-AH-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      END-IF
                   END-IF
04391              MOVE HD6B-1           TO  HD6                        
04392              MOVE HD7B-1           TO  HD7                        
04393              IF LNCTR GREATER LNCTR-34-49                         
04394                  PERFORM 3900-PRT-STMT-HD-RTN  THRU  3999-EXIT    
04395                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04396                  GO TO 3010-RETURN-SORTED-RECS                    
04397              ELSE                                                 
04398                                                                   
04399                  PERFORM 3950-PRT-HD5  THRU  3999-EXIT            
04400                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04401                  GO TO 3010-RETURN-SORTED-RECS                    
04402          ELSE                                                     
04403              IF REC-08-FIRST-TIME                                 
04404                  MOVE 'N'          TO  REC-08-FIRST-TIME-SW       
04405                  MOVE '1'          TO  HEADING-SW                 
04406                  MOVE HD5F         TO  HD5                        
                   IF SEC-PAY-CARRIER (CLAS-INDEXCN)
                      MOVE SPACES      TO HD6B-COMP
                      MOVE ' MINIMUM FEE   '
                                       TO HD7B-PCT
                   ELSE
                      MOVE 'COMP.'     TO HD6B-COMP
                      MOVE 'REVISED    PCT.'
                                       TO HD7B-PCT
                   END-IF
04407                  MOVE HD6B-1       TO  HD6                        
04408                  MOVE HD7B-1       TO  HD7                        
04409                  MOVE SPACE-1      TO  P-REC                      
04410                  IF LNCTR GREATER LNCTR-34-49                     
04411                      PERFORM 3900-PRT-STMT-HD-RTN                 
04412                          THRU  3999-EXIT                          
04413                      PERFORM 3100-PRT-DETAIL-RTN                  
04414                          THRU  3199-EXIT                          
04415                      GO TO 3010-RETURN-SORTED-RECS                
04416                  ELSE                                             
04417                      MOVE SPACE-1  TO  P-REC                      
04418                      PERFORM 8800-PRT-RTN  THRU  8899-EXIT        
04419                      PERFORM 3950-PRT-HD5                         
04420                      MOVE SPACE-1  TO  P-REC                      
04421                      PERFORM 8800-PRT-RTN  THRU  8899-EXIT        
04422                      PERFORM 3100-PRT-DETAIL-RTN                  
04423                          THRU  3199-EXIT                          
04424                      GO TO 3010-RETURN-SORTED-RECS                
04425              ELSE                                                 
04426                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04427                  GO TO 3010-RETURN-SORTED-RECS.                   
04428                                                                   
04429      IF WS-CANCEL-HOLD                                            
04430          IF LIF-TOT-NEEDED                                        
04431              MOVE 'N'            TO  LIF-TOT-PRT-SW               
04432              PERFORM 3200-PRT-LIFE-TOTALS  THRU  3299-XIT.        
04433                                                                   
04434      IF WS-CANCEL-HOLD                                            
04435          IF AH-TOT-NEEDED                                         
04436              MOVE 'N'            TO  AH-TOT-PRT-SW                
04437              PERFORM 3300-PRT-AH-TOTALS  THRU  3399-XIT.          
04438  EJECT                                                            
04439      IF WS-CANCEL-HOLD                                            
04440          IF REC-09-FIRST-TIME                                     
04441            AND  REC-08-FIRST-TIME                                 
04442              MOVE 'N'              TO  REC-09-FIRST-TIME-SW       
04443              MOVE '1'              TO  HEADING-SW                 
04444              MOVE HD5G             TO  HD5                        
                   IF SEC-PAY-CARRIER (CLAS-INDEXCN)
                      IF WS-SRT03-AH-BEN-CAT = 'G' OR 'L'
                         MOVE SPACES      TO HD6B-COMP
                         MOVE ' MINIMUM FEE   '
                                       TO HD7B-PCT
                      ELSE
                         MOVE 'COMP.'     TO HD6B-COMP
                         MOVE 'REVISED    PCT.'
                                       TO HD7B-PCT
                      END-IF
                      IF WS-SRT03-LF-BEN-CAT NOT = SPACES
                         MOVE WS-SRT03-LF-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      ELSE
                         MOVE WS-SRT03-AH-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      END-IF
                   END-IF
04445              MOVE HD6B-1           TO  HD6                        
04446              MOVE HD7B-1           TO  HD7                        
04447              IF LNCTR GREATER LNCTR-34-49                         
04448                  PERFORM 3900-PRT-STMT-HD-RTN  THRU  3999-EXIT    
04449                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04450                  GO TO 3010-RETURN-SORTED-RECS                    
04451              ELSE                                                 
04452                  PERFORM 3950-PRT-HD5  THRU  3999-EXIT            
04453                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04454                  GO TO 3010-RETURN-SORTED-RECS                    
04455          ELSE                                                     
04456              IF REC-09-FIRST-TIME                                 
04457                  MOVE 'N'          TO  REC-08-FIRST-TIME-SW       
04458                  MOVE '1'          TO  HEADING-SW                 
04459                  MOVE HD5G         TO  HD5                        
                    IF SEC-PAY-CARRIER (CLAS-INDEXCN)
                      IF WS-SRT03-AH-BEN-CAT = 'G' OR 'L'
                         MOVE SPACES      TO HD6B-COMP
                         MOVE ' MINIMUM FEE   '
                                       TO HD7B-PCT
                      ELSE
                         MOVE 'COMP.'     TO HD6B-COMP
                         MOVE 'REVISED    PCT.'
                                       TO HD7B-PCT
                      END-IF
                      IF WS-SRT03-LF-BEN-CAT NOT = SPACES
                         MOVE WS-SRT03-LF-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      ELSE
                         MOVE WS-SRT03-AH-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      END-IF
                    END-IF
04460                  MOVE HD6B-1       TO  HD6                        
04461                  MOVE HD7B-1       TO  HD7                        
04462                  MOVE SPACE-1      TO  P-REC                      
04463                  IF LNCTR GREATER LNCTR-34-49                     
04464                      PERFORM 3900-PRT-STMT-HD-RTN                 
04465                          THRU  3999-EXIT                          
04466                      PERFORM 3100-PRT-DETAIL-RTN                  
04467                          THRU  3199-EXIT                          
04468                      GO TO 3010-RETURN-SORTED-RECS                
04469                  ELSE                                             
04470                      MOVE SPACE-1  TO  P-REC                      
04471                      PERFORM 8800-PRT-RTN  THRU  8899-EXIT        
04472                      PERFORM 3950-PRT-HD5                         
04473                      MOVE SPACE-1  TO  P-REC                      
04474                      PERFORM 8800-PRT-RTN  THRU  8899-EXIT        
04475                      PERFORM 3100-PRT-DETAIL-RTN                  
04476                          THRU  3199-EXIT                          
04477                      GO TO 3010-RETURN-SORTED-RECS                
04478              ELSE                                                 
04479                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04480                  GO TO 3010-RETURN-SORTED-RECS.                   
04481                                                                   
04482      IF WS-CANCEL-RETURN                                          
04483          IF LIF-TOT-NEEDED                                        
04484              MOVE 'N'            TO  LIF-TOT-PRT-SW               
04485              PERFORM 3200-PRT-LIFE-TOTALS  THRU  3299-XIT.        
04486                                                                   
04487      IF WS-CANCEL-RETURN                                          
04488          IF AH-TOT-NEEDED                                         
04489              MOVE 'N'            TO  AH-TOT-PRT-SW                
04490              PERFORM 3300-PRT-AH-TOTALS  THRU  3399-XIT.          
04491  EJECT                                                            
04492      IF WS-CANCEL-RETURN                                          
04493          IF REC-10-FIRST-TIME  AND                                
04494             REC-09-FIRST-TIME  AND                                
04495             REC-08-FIRST-TIME                                     
04496              MOVE 'N'              TO  REC-10-FIRST-TIME-SW       
04497              MOVE '1'              TO  HEADING-SW                 
04498              MOVE HD5H             TO  HD5                        
                   IF SEC-PAY-CARRIER (CLAS-INDEXCN)
                      IF WS-SRT03-AH-BEN-CAT = 'G' OR 'L'
                         MOVE SPACES      TO HD6B-COMP
                         MOVE ' MINIMUM FEE   '
                                       TO HD7B-PCT
                      ELSE
                         MOVE 'COMP.'     TO HD6B-COMP
                         MOVE 'REVISED    PCT.'
                                       TO HD7B-PCT
                      END-IF
                      IF WS-SRT03-LF-BEN-CAT NOT = SPACES
                         MOVE WS-SRT03-LF-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      ELSE
                         MOVE WS-SRT03-AH-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      END-IF
                   END-IF
04499              MOVE HD6B-1           TO  HD6                        
04500              MOVE HD7B-1           TO  HD7                        
04501              IF LNCTR GREATER LNCTR-34-49                         
04502                  PERFORM 3900-PRT-STMT-HD-RTN  THRU  3999-EXIT    
04503                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04504                  GO TO 3010-RETURN-SORTED-RECS                    
04505              ELSE                                                 
04506                  PERFORM 3950-PRT-HD5  THRU  3999-EXIT            
04507                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04508                  GO TO 3010-RETURN-SORTED-RECS                    
04509          ELSE                                                     
04510              IF REC-10-FIRST-TIME                                 
04511                  MOVE 'N'          TO  REC-10-FIRST-TIME-SW       
04512                  MOVE '1'          TO  HEADING-SW                 
04513                  MOVE HD5H         TO  HD5                        
                    IF SEC-PAY-CARRIER (CLAS-INDEXCN)
                      IF WS-SRT03-AH-BEN-CAT = 'G' OR 'L'
                         MOVE SPACES      TO HD6B-COMP
                         MOVE ' MINIMUM FEE   '
                                       TO HD7B-PCT
                      ELSE
                         MOVE 'COMP.'     TO HD6B-COMP
                         MOVE 'REVISED    PCT.'
                                       TO HD7B-PCT
                      END-IF
                      IF WS-SRT03-LF-BEN-CAT NOT = SPACES
                         MOVE WS-SRT03-LF-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      ELSE
                         MOVE WS-SRT03-AH-BEN-CAT
                                       TO PRV-SPP-CATEGORY
                      END-IF
                    END-IF
04514                  MOVE HD6B-1       TO  HD6                        
04515                  MOVE HD7B-1       TO  HD7                        
04516                  MOVE SPACE-1      TO  P-REC                      
04517                  IF LNCTR GREATER LNCTR-34-49                     
04518                      PERFORM 3900-PRT-STMT-HD-RTN                 
04519                          THRU  3999-EXIT                          
04520                      PERFORM 3100-PRT-DETAIL-RTN                  
04521                          THRU  3199-EXIT                          
04522                      GO TO 3010-RETURN-SORTED-RECS                
04523                  ELSE                                             
04524                      MOVE SPACE-1  TO  P-REC                      
04525                      PERFORM 8800-PRT-RTN  THRU  8899-EXIT        
04526                      PERFORM 3950-PRT-HD5                         
04527                      MOVE SPACE-1  TO  P-REC                      
04528                      PERFORM 8800-PRT-RTN  THRU  8899-EXIT        
04529                      PERFORM 3100-PRT-DETAIL-RTN                  
04530                          THRU  3199-EXIT                          
04531                      GO TO 3010-RETURN-SORTED-RECS                
04532              ELSE                                                 
04533                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04534                  GO TO 3010-RETURN-SORTED-RECS.                   
04535                                                                   
04536      IF WS-PYMTS-ADJUST                                           
04537          IF LIF-TOT-NEEDED                                        
04538              MOVE 'N'            TO  LIF-TOT-PRT-SW               
04539              PERFORM 3200-PRT-LIFE-TOTALS  THRU  3299-XIT.        
04540  EJECT                                                            
04541      IF WS-PYMTS-ADJUST                                           
04542          IF AH-TOT-NEEDED                                         
04543              MOVE 'N'            TO  AH-TOT-PRT-SW                
04544              PERFORM 3300-PRT-AH-TOTALS  THRU  3399-XIT.          
04545                                                                   
04546      IF WS-PYMTS-ADJUST                                           
04547          IF REC-11-FIRST-TIME                                     
04548              MOVE 'N'            TO  REC-11-FIRST-TIME-SW         
04549              MOVE '3'            TO  HEADING-SW                   
04550              MOVE HD5I           TO  HD5                          
04551              MOVE HD6C           TO  HD6                          
04552              IF LNCTR GREATER LNCTR-34-49                         
04553                  PERFORM 3900-PRT-STMT-HD-RTN  THRU  3999-EXIT    
04554                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04555                  GO TO 3010-RETURN-SORTED-RECS                    
04556              ELSE                                                 
04557                  PERFORM 3950-PRT-HD5  THRU  3999-EXIT            
04558                  PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT     
04559                  GO TO 3010-RETURN-SORTED-RECS                    
04560          ELSE                                                     
04561              PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT         
04562              GO TO 3010-RETURN-SORTED-RECS.                       
04563                                                                   
04564      IF WS-SUMMARY                                                
04565          IF LIF-TOT-NEEDED                                        
04566              MOVE 'N'            TO  LIF-TOT-PRT-SW               
04567              PERFORM 3200-PRT-LIFE-TOTALS  THRU  3299-XIT.        
04568                                                                   
04569      IF WS-SUMMARY                                                
04570          IF AH-TOT-NEEDED                                         
04571              MOVE 'N'            TO  AH-TOT-PRT-SW                
04572              PERFORM 3300-PRT-AH-TOTALS  THRU  3399-XIT.          
04573                                                                   
04574      IF WS-SUMMARY                                                
04575          MOVE HD5J               TO  HD5                          
04576          MOVE HD8D               TO  HD6                          
04577          MOVE '2'                TO  HEADING-SW                   
04578          PERFORM 3900-PRT-STMT-HD-RTN  THRU  3999-EXIT            
04579          PERFORM 3800-PRT-TOTAL-RTN  THRU  3899-EXIT              
04580          GO TO 3010-RETURN-SORTED-RECS.                           
04581                                                                   
04582      IF WS-CLAIMS                                                 
04583          IF LIF-TOT-NEEDED                                        
04584              MOVE 'N'            TO  LIF-TOT-PRT-SW               
04585              PERFORM 3200-PRT-LIFE-TOTALS  THRU  3299-XIT.        
04586                                                                   
04587      IF WS-CLAIMS                                                 
04588          IF AH-TOT-NEEDED                                         
04589              MOVE 'N'            TO  AH-TOT-PRT-SW                
04590              PERFORM 3300-PRT-AH-TOTALS  THRU  3399-XIT.          
04591  EJECT                                                            
04592      IF WS-CLAIMS                                                 
04593          IF REC-13-FIRST-TIME                                     
04594              MOVE 'Y'            TO  CLM-TOT-PRT-SW               
04595              MOVE 'N'            TO  REC-13-FIRST-TIME-SW         
04596              MOVE '1'            TO  HEADING-SW                   
04597              MOVE HD5K           TO  HD5                          
04598              MOVE HD6D           TO  HD6                          
04599              MOVE HD7D           TO  HD7                          
04600              PERFORM 3900-PRT-STMT-HD-RTN  THRU  3999-EXIT        
04601              PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT         
04602              GO TO 3010-RETURN-SORTED-RECS                        
04603          ELSE                                                     
04604              PERFORM 3100-PRT-DETAIL-RTN  THRU  3199-EXIT         
04605              GO TO 3010-RETURN-SORTED-RECS.                       
04606  EJECT                                                            
04607  3100-PRT-DETAIL-RTN.                                             
04608      IF WS-SRT-REC-SFX = ZERO                                     
04609          IF WS-ISSUE         OR                                   
052504            WS-CASH-ISSUE    OR
052504            WS-CASH-CANCEL   OR
04610             WS-CANCEL        OR                                   
04611             WS-PYMTS-ADJUST  OR                                   
04612             WS-CLAIMS                                             
04613              PERFORM 4000-CALC-DETAIL-RTN  THRU  4099-EXIT        
04614          ELSE                                                     
04615              PERFORM 5000-CALC-NON-PRC  THRU  5099-EXIT.          
04616                                                                   
04617      IF LNCTR GREATER LNCTR-43-58                                 
04618          PERFORM 3900-PRT-STMT-HD-RTN  THRU  3999-EXIT.           
04619                                                                   
04620      IF WS-ISSUE-ERROR                                            
04621          MOVE 'I'                TO  TIC-TYPE-SWITCH              
04622          GO TO 3110-PRT-ISSUE-ERROR.                              
04623                                                                   
04624      IF WS-ISSUE-HOLD                                             
04625          MOVE 'I'                TO  TIC-TYPE-SWITCH              
04626          GO TO 3115-PRT-ISSUE-HOLD.                               
04627                                                                   
04628      IF WS-ISSUE-RETURN                                           
04629          MOVE 'I'                TO  TIC-TYPE-SWITCH              
04630          GO TO 3120-PRT-ISSUE-RETURN.                             
04631                                                                   
04632      IF WS-CANCEL                                                 
04633          MOVE 'C'                TO  TIC-TYPE-SWITCH              
04634          GO TO 3140-PRT-CANCEL.                                   
04635                                                                   
           IF WS-CASH-CANCEL                                                 
               MOVE 'C'                TO  TIC-TYPE-SWITCH              
               GO TO 3140-PRT-CANCEL.                                   
                                                                        
04636      IF WS-CANCEL-ERROR                                           
04637          MOVE 'C'                TO  TIC-TYPE-SWITCH              
04638          GO TO 3145-PRT-CANCEL-ERROR.                             
04639                                                                   
04640      IF WS-CANCEL-HOLD                                            
04641          MOVE 'C'                TO  TIC-TYPE-SWITCH              
04642          GO TO 3150-PRT-CANCEL-HOLD.                              
04643                                                                   
04644      IF WS-CANCEL-RETURN                                          
04645          MOVE 'C'                TO  TIC-TYPE-SWITCH              
04646          GO TO 3155-PRT-CANCEL-RETURN.                            
04647                                                                   
04648      IF WS-PYMTS-ADJUST                                           
04649          GO TO 3175-PRT-ACCTG.                                    
04650                                                                   
04651      IF WS-CLAIMS                                                 
04652          GO TO 3180-PRT-CLAIM.                                    
04653  EJECT                                                            
04654  3105-PRT-ISSUE.                                                  
04655      IF WS-SRT-REC-SFX GREATER ZERO                               
04656         PERFORM 3500-CHECK-TIC  THRU  3599-XIT                   
010616        if ws-supp-check
010616           move spaces           to p-check-line
010616           move ws-srt03a-check-amt
010616                                 to ws-formatted-amount
010616           move ws-srt03a-chek-dt
010616                                 to ws-srt03a-chek-dt-r
010616           if ws-srt03a-chek-num not = spaces and zeros
010616              string
010616                 'Premium Adjustment of ' delimited by size
010616                 ws-formatted-amount delimited by size
010616                 ' Paid to ' delimited by size
010616                 ws-srt03a-payee delimited by '   '
010616                 ' By Check No ' delimited by size
010616                 ws-srt03a-chek-num delimited by size
010616                 ' Dated ' delimited by size
010616                 ws-srt03a-ck-mo '-'
010616                 ws-srt03a-ck-da '-'
010616                 ws-srt03a-ck-yr
010616                    delimited by size into p-check-line
010616              end-string
010616           else
010616              string
010616                 'Premium Adjustment of '
010616                 ws-formatted-amount
010616                 ' Paid to '
010616                 ws-srt03a-payee
010616                    delimited by size into p-check-line
010616              end-string
010616           end-if
010616           MOVE SPACE-1          TO P-CCSW              
010616           GO TO 3190-PRT-DETAIL
010616        else  
04657            IF WS-SUPP-NOTE                                          
04658               MOVE '** NOTE **'  TO P-LF-NOTE                    
04659               MOVE WS-SRT02-REC  TO P-L4-MESSAGE                 
04660               MOVE SPACE-1       TO P-CCSW                       
04661               GO TO 3190-PRT-DETAIL                                
04662            ELSE                                                     
04663               MOVE WS-SRT02-REC  TO P-L4-MESSAGE                 
04664               MOVE SPACE-1       TO P-CCSW                       
04665               GO TO 3190-PRT-DETAIL
010616           end-if
010616        end-if
010616     end-if
04666                                                                   
04667      GO TO 3125-END-ISSUE.                                        
04668                                                                   
04669  3110-PRT-ISSUE-ERROR.                                            
04670      IF WS-SRT-REC-SFX GREATER ZERO                               
04671          PERFORM 3500-CHECK-TIC  THRU  3599-XIT                   
04672          IF WS-SUPP-NOTE                                          
04673              MOVE '** NOTE **'      TO  P-LF-NOTE                 
04674              MOVE WS-SRT02-REC      TO  P-L4-MESSAGE              
04675              MOVE SPACE-1           TO  P-CCSW                    
04676              GO TO 3190-PRT-DETAIL                                
04677          ELSE                                                     
04678              MOVE SPACES            TO  P-LF-SEVERITY             
04679              MOVE WS-SRT04-ERR-TXT  TO  P-L4-MESSAGE              
04680              MOVE SPACE-1           TO  P-CCSW                    
04681              GO TO 3190-PRT-DETAIL.                               
04682                                                                   
04683      GO TO 3125-END-ISSUE.                                        
04684                                                                   
04685  3115-PRT-ISSUE-HOLD.                                             
04686      IF WS-SRT-REC-SFX GREATER ZERO                               
04687          PERFORM 3500-CHECK-TIC  THRU  3599-XIT                   
04688          IF WS-SUPP-ERROR                                         
04689              MOVE SPACES            TO  P-LF-SEVERITY             
04690              MOVE WS-SRT04-ERR-TXT  TO  P-L4-MESSAGE              
04691              MOVE SPACE-1           TO  P-CCSW                    
04692              GO TO 3190-PRT-DETAIL                                
04693          ELSE                                                     
04694              IF WS-SUPP-NOTE                                      
04695                  MOVE '** NOTE **'  TO  P-LF-NOTE                 
04696                  MOVE WS-SRT02-REC  TO  P-L4-MESSAGE              
04697                  MOVE SPACE-1       TO  P-CCSW                    
04698                  GO TO 3190-PRT-DETAIL                            
04699              ELSE                                                 
04700                  MOVE WS-SRT02-REC  TO  P-L4-MESSAGE              
04701                  MOVE SPACE-1       TO  P-CCSW                    
04702                  GO TO 3190-PRT-DETAIL.                           
04703                                                                   
04704      GO TO 3125-END-ISSUE.                                        
04705  EJECT                                                            
04706  3120-PRT-ISSUE-RETURN.                                           
04707      IF WS-SRT-REC-SFX GREATER ZERO                               
04708         PERFORM 3500-CHECK-TIC  THRU  3599-XIT                   
010616        evaluate true
010616           when ws-supp-error
04710              MOVE SPACES            TO  P-LF-SEVERITY             
04711              MOVE WS-SRT04-ERR-TXT  TO  P-L4-MESSAGE              
04712              MOVE SPACE-1           TO  P-CCSW                    
04713              GO TO 3190-PRT-DETAIL                                
010616           when WS-SUPP-NOTE                                      
04716               MOVE '** NOTE **'  TO  P-LF-NOTE                 
04717               MOVE WS-SRT02-REC  TO  P-L4-MESSAGE              
04718               MOVE SPACE-1       TO  P-CCSW                    
04719               GO TO 3190-PRT-DETAIL                            
010616           when other
04721               MOVE WS-SRT02-REC  TO  P-L4-MESSAGE              
04722               MOVE SPACE-1       TO  P-CCSW                    
04723               GO TO 3190-PRT-DETAIL
010616        end-evaluate
010616     end-if

           .
04725  3125-END-ISSUE.                                                  
04726                                                                   
04727      MOVE 'NNN'                  TO  COV-TABLE.                   
04728      MOVE ZEROS                  TO  NET-DUE.                     
04729                                                                   
04730      IF WS-SRT02-LF-PREM NOT = ZEROS                              
04731          MOVE 'Y'                TO  LF-COV.                      
04732                                                                   
04733      IF WS-SRT02-LF-PREM-ALT NOT = ZEROS                          
04734          MOVE 'Y'                TO  ALT-COV.                     
04735                                                                   
04736      IF WS-SRT02-AH-PREM NOT = ZEROS                              
04737          MOVE 'Y'                TO  AH-COV.                      
04738                                                                   
04739      IF WS-SRT02-RECALC-FLAG  IS EQUAL TO  'R'                    
04740          IF DTE-CLIENT = 'BWS'                                    
04741              MOVE '(R)'          TO  P-CP-3-RECALC                
04742              MOVE WS-SRT02-LNAME                                  
04743                                  TO  P-CP-3-LNAME-R               
04744              MOVE WS-SRT02-FNAME                                  
04745                                  TO  P-CP-3-FNAME-R               
04746          ELSE                                                     
04747              MOVE '(R)'          TO  P-CP-RECALC                  
04748              MOVE WS-SRT02-LNAME TO  P-CP-LNAME-R                 
04749              MOVE WS-SRT02-FNAME TO  P-CP-FNAME-R                 
04750      ELSE                                                         
04751          IF DTE-CLIENT = 'BWS'                                    
04752              MOVE WS-SRT02-LNAME TO  P-CP-3-LNAME                 
04753              MOVE WS-SRT02-FNAME TO  P-CP-3-INIT-1                
04754              MOVE WS-SRT02-INTL  TO  P-CP-3-INIT-2                
04755          ELSE                                                     
04756              MOVE WS-SRT02-LNAME TO  P-CP-LNAME                   
04757              MOVE WS-SRT02-FNAME TO  P-CP-FNAME                   
04758              MOVE WS-SRT02-INTL  TO  P-CP-INITIAL.                
04759                                                                   
04760      IF DTE-CLIENT = 'BWS'                                        
04761          MOVE WS-SRT02-CERT      TO  P-CP-3-CERT                  
04762          MOVE WS-SRT02-EFF-DT    TO  WS-SRT02-EFF-DT-R            
04763          MOVE WS-SRT02-EFF-MO    TO  P-CP-3-EFF-MO                
04764          MOVE WS-SRT02-EFF-DA    TO  P-CP-3-EFF-DA                
04765          MOVE WS-SRT02-EFF-YR    TO  P-CP-3-EFF-YR                
04766          MOVE '-'                TO  P-CP-3-EFF-MOD P-CP-3-EFF-DAD
04767          MOVE WS-SRT02-AGE       TO  P-CP-3-AGE                   
04768      ELSE                                                         
04769          MOVE WS-SRT02-EFF-DT    TO  WS-SRT02-EFF-DT-R            
04770          MOVE WS-SRT02-CERT      TO  P-CP-CERT                    
04771          MOVE WS-SRT02-EFF-MO    TO  P-CP-EFF-MO                  
04772          MOVE WS-SRT02-EFF-DA    TO  P-CP-EFF-DA                  
04773          MOVE WS-SRT02-EFF-YR    TO  P-CP-EFF-YR                  
04774          MOVE '-'                TO  P-CP-EFF-MOD P-CP-EFF-DAD    
04775          MOVE WS-SRT02-AGE       TO  P-CP-AGE.                    
04776                                                                   
04777      IF WS-SRT02-LF-TERM = ZEROS  AND                             
04778         WS-SRT02-LF-PREM = ZEROS                                  
04779          MOVE 'Y'                TO  AH-ONLY-SWITCH               
04780          GO TO 3135-PRT-AH.                                       
04781                                                                   
04782      IF DTE-CLIENT = 'BWS'                                        
04783          MOVE WS-SRT02-LF-TERM   TO  P-CP-3-TERM                  
04784          MOVE LIFE-OVERRIDE-L2   TO  P-CP-3-BEN                   
04785          MOVE '-'                TO  P-CP-3-BEN-DASH              
04786          MOVE WS-SRT02-LF-TYP    TO  P-CP-3-BEN-TYPE              
04787      ELSE                                                         
04788          MOVE WS-SRT02-LF-TERM   TO  P-CP-TERM                    
04789          MOVE LIFE-OVERRIDE-L2   TO  P-CP-BEN                     
04790          MOVE '-'                TO  P-CP-BEN-DASH                
04791          MOVE WS-SRT02-LF-TYP    TO  P-CP-BEN-TYPE.               
04792                                                                   
04793      IF WS-SRT02-OB-ON = 'Y'                                      
04794          IF DTE-CLIENT = 'BWS'                                    
04795              MOVE WS-SRT02-FACE  TO  P-CP-3-FACE-DOL              
04796          ELSE                                                     
04797              MOVE WS-SRT02-FACE  TO  P-CP-FACE-DOL                
04798      ELSE                                                         
04799          IF DTE-CLIENT = 'BWS'                                    
04800              MOVE WS-SRT02-FACE  TO  P-CP-3-FACE                  
04801          ELSE                                                     
04802              MOVE WS-SRT02-FACE  TO  P-CP-FACE.                   
04803                                                                   
04804      IF DTE-CLIENT = 'BWS'                                        
04805          MOVE WS-SRT02-LF-PREM   TO  P-CP-3-PREM                  
04806      ELSE                                                         
04807          MOVE WS-SRT02-LF-PREM   TO  P-CP-PREM.                   
04808                                                                   
04809      IF WS-SRT02-LF-PRM-PR NOT = ZERO                             
04810          IF DTE-CLIENT = 'BWS'                                    
04811              MOVE WS-SRT02-LF-PRM-PR                              
04812                                   TO  P-CP-3-PREM-REV             
04813          ELSE                                                     
092705            IF WS-SRT02-AH-BEN-CAT = 'G' OR 'L'
100703               MOVE WS-SRT02-LF-PREM-ALT
100703                                 TO P-CP-PREM-REV
100703            ELSE
04814                MOVE WS-SRT02-LF-PRM-PR                              
04815                                   TO  P-CP-PREM-REV               
100703            END-IF
100703         END-IF
04816      ELSE                                                         
04817          IF WS-SRT02-PREM-OVERRIDE = 'B' OR 'L'                   
04818              IF DTE-CLIENT = 'BWS'                                
04819                  MOVE '.00'      TO  P-CP-3-PREM-REV-ZEROS        
04820              ELSE                                                 
04821                  MOVE '.00'      TO  P-CP-PREM-REV-ZEROS
100703             END-IF
               END-IF
            END-IF
04822                                                                   
04823      IF WS-SRT02-LF-COMM = ZERO                                   
04824          MOVE ZERO               TO  WORK-PERC                    
04825      ELSE                                                         
04826          COMPUTE WORK-PERC = WS-SRT02-LF-COM-PCT * +100.          
04827                                                                   
04828      IF DTE-CLIENT = 'BWS'                                        
04829          MOVE WORK-PERC          TO  P-CP-3-COMM-PCT              
04830          MOVE WS-SRT02-LF-COMM   TO  P-CP-3-COMM                  
04831      ELSE                                                         
04832          IF DTE-CLIENT = 'KSM'                                    
04833              MOVE WS-SRT02-LF-COMM                                
04834                                  TO  P-CP-COMM                    
04835          ELSE                                                     
04836              MOVE WORK-PERC      TO  P-CP-COMM-PCT                
04837              MOVE WS-SRT02-LF-COMM                                
04838                                  TO  P-CP-COMM.                   
04839                                                                   
04840      MOVE SPACE-2                TO  P-CCSW.                      
04841                                                                   
04842      PERFORM 3500-CHECK-TIC  THRU  3599-XIT.                      
04843                                                                   
04844      IF DTE-CLIENT = 'BWS'                                        
04845          IF WS-SRT02-LF-PRM-PR = ZEROS                            
04846              ADD WS-SRT02-LF-PREM TO NET-DUE                      
04847              SUBTRACT WS-SRT02-LF-COMM FROM NET-DUE               
04848              COMPUTE TOT-NET-DUE =                                
04849                  TOT-NET-DUE + WS-SRT02-LF-PREM - WS-SRT02-LF-COMM
04850              IF COV-TABLE = 'YNN'                                 
04851                  MOVE NET-DUE    TO  P-CP-3-NET-AMT               
04852              ELSE                                                 
04853                  NEXT SENTENCE                                    
04854          ELSE                                                     
04855              ADD WS-SRT02-LF-PRM-PR TO NET-DUE                    
04856              SUBTRACT WS-SRT02-LF-COMM FROM NET-DUE               
04857              COMPUTE TOT-NET-DUE =                                
04858                  TOT-NET-DUE + WS-SRT02-LF-PRM-PR                 
04859                              - WS-SRT02-LF-COMM                   
04860              IF COV-TABLE = 'YNN'                                 
04861                  MOVE NET-DUE    TO  P-CP-3-NET-AMT               
04862              ELSE                                                 
04863                  NEXT SENTENCE.                                   
04864                                                                   
04865      PERFORM 3190-PRT-DETAIL.                                     
04866                                                                   
04867  3130-PRT-LF.                                                     
04868      IF ALT-COV = 'N'                                             
04869          GO TO 3135-PRT-AH.                                       
04870                                                                   
04871      IF WS-SRT02-OB-ON = 'Y'                                      
04872          IF DTE-CLIENT = 'BWS'                                    
04873              MOVE WS-SRT02-FACE-ALT                               
04874                                  TO  P-CP-3-FACE-DOL              
04875          ELSE                                                     
04876              MOVE WS-SRT02-FACE-ALT                               
04877                                  TO  P-CP-FACE-DOL                
04878      ELSE                                                         
04879          IF DTE-CLIENT = 'BWS'                                    
04880              MOVE WS-SRT02-FACE-ALT                               
04881                                  TO  P-CP-3-FACE                  
04882          ELSE                                                     
04883              MOVE WS-SRT02-FACE-ALT                               
04884                                  TO  P-CP-FACE.                   
04885                                                                   
04886      IF DTE-CLIENT = 'BWS'                                        
04887          MOVE WS-SRT02-LF-PREM-ALT                                
04888                                  TO  P-CP-3-PREM                  
04889      ELSE                                                         
04890          MOVE WS-SRT02-LF-PREM-ALT                                
04891                                  TO  P-CP-PREM.                   
04892                                                                   
04893      IF WS-SRT02-LF-PRM-PR-ALT NOT = ZERO                         
04894          IF DTE-CLIENT = 'BWS'                                    
04895              MOVE WS-SRT02-LF-PRM-PR-ALT                          
04896                                  TO  P-CP-3-PREM-REV              
04897          ELSE                                                     
04898              MOVE WS-SRT02-LF-PRM-PR-ALT                          
04899                                  TO  P-CP-PREM-REV                
04900      ELSE                                                         
04901          IF WS-SRT02-PREM-OVERRIDE = 'B' OR 'L'                   
04902              IF DTE-CLIENT = 'BWS'                                
04903                  MOVE '.00'          TO  P-CP-3-PREM-REV-ZEROS    
04904              ELSE                                                 
04905                  MOVE '.00'          TO  P-CP-PREM-REV-ZEROS.     
04906                                                                   
04907      IF WS-SRT02-LF-COMM-ALT = ZERO                               
04908          MOVE ZERO               TO  WORK-PERC                    
04909      ELSE                                                         
04910          COMPUTE WORK-PERC = WS-SRT02-LF-COM-PCT * +100.          
04911                                                                   
04912      IF DTE-CLIENT = 'BWS'                                        
04913          MOVE WORK-PERC          TO  P-CP-3-COMM-PCT              
04914          MOVE WS-SRT02-LF-COMM-ALT                                
04915                                  TO  P-CP-3-COMM                  
04916      ELSE                                                         
04917          IF DTE-CLIENT = 'KSM'                                    
04918              MOVE WS-SRT02-LF-COMM-ALT                            
04919                                  TO  P-CP-COMM                    
04920          ELSE                                                     
04921              MOVE WORK-PERC      TO  P-CP-COMM-PCT                
04922              MOVE WS-SRT02-LF-COMM-ALT                            
04923                                  TO  P-CP-COMM.                   
04924                                                                   
04925      MOVE SPACE-1                TO  P-CCSW.                      
04926                                                                   
04927      PERFORM 3500-CHECK-TIC  THRU  3599-XIT.                      
04928                                                                   
04929      IF DTE-CLIENT = 'BWS'                                        
04930          ADD WS-SRT02-LF-PREM-ALT TO NET-DUE                      
04931          SUBTRACT WS-SRT02-LF-COMM-ALT FROM NET-DUE               
04932          COMPUTE TOT-NET-DUE =                                    
04933              TOT-NET-DUE  + WS-SRT02-LF-PREM-ALT                  
04934                           - WS-SRT02-LF-COMM-ALT                  
04935          IF COV-TABLE = 'YYN'                                     
04936              MOVE NET-DUE        TO  P-CP-3-NET-AMT.              
04937                                                                   
04938      PERFORM 3190-PRT-DETAIL.                                     
04939                                                                   
04940  3135-PRT-AH.                                                     
04941      IF WS-SRT02-AH-TERM = ZEROS  AND                             
04942         WS-SRT02-AH-PREM = ZEROS                                  
092815        IF WS-SRT02-VIN Not = spaces
092815           string
092815              '   VIN - '
092815              WS-SRT02-VIN
092815                 DELIMITED BY SIZE INTO P-LINE
092815           END-STRING
092815           PERFORM 3190-PRT-DETAIL
092815        END-IF
04943          IF DTE-CLIENT = 'TIC'                                    
04944              PERFORM 3500-CHECK-TIC  THRU  3599-XIT               
04945              MOVE SPACE-1        TO  P-CCSW                       
04946              PERFORM 3190-PRT-DETAIL                              
04947              MOVE 'Y'            TO  LF-ONLY-SWITCH               
04948              GO TO 3190-PRT-DETAIL                                
04949          ELSE                                                     
04950              MOVE 'Y'            TO  LF-ONLY-SWITCH               
04951              GO TO 3190-PRT-DETAIL.                               
04952                                                                   
04953      IF DTE-CLIENT = 'BWS'                                        
04954          MOVE WS-SRT02-AH-TERM   TO  P-CP-3-TERM                  
04955          MOVE AH-OVERRIDE-L2     TO  P-CP-3-BEN                   
04956          MOVE '-'                TO  P-CP-3-BEN-DASH              
04957          MOVE WS-SRT02-AH-TYP    TO  P-CP-3-BEN-TYPE              
04958      ELSE                                                         
04959          MOVE WS-SRT02-AH-TERM   TO  P-CP-TERM                    
04960          MOVE AH-OVERRIDE-L2     TO  P-CP-BEN                     
04961          MOVE '-'                TO  P-CP-BEN-DASH                
04962          MOVE WS-SRT02-AH-TYP    TO  P-CP-BEN-TYPE.               
04963                                                                   
04964      IF WS-SRT02-OB-ON = 'Y'                                      
04965          IF DTE-CLIENT = 'BWS'                                    
04966              MOVE WS-SRT02-AH-BEN                                 
04967                                  TO  P-CP-3-FACE-DOL              
04968          ELSE                                                     
04969              MOVE WS-SRT02-AH-BEN                                 
04970                                  TO  P-CP-FACE-DOL                
04971      ELSE                                                         
04972          IF DTE-CLIENT = 'BWS'                                    
04973              MOVE WS-SRT02-AH-BEN                                 
04974                                  TO  P-CP-3-FACE                  
04975          ELSE                                                     
04976              MOVE WS-SRT02-AH-BEN                                 
04977                                  TO  P-CP-FACE.                   
04978                                                                   
04979      IF DTE-CLIENT = 'BWS'                                        
04980          MOVE WS-SRT02-AH-PREM   TO  P-CP-3-PREM                  
04981      ELSE                                                         
04982          MOVE WS-SRT02-AH-PREM   TO  P-CP-PREM.                   
04983                                                                   
04984      IF WS-SRT02-AH-PRM-PR NOT = ZERO                             
04985          IF DTE-CLIENT = 'BWS'                                    
04986              MOVE WS-SRT02-AH-PRM-PR                              
04987                                  TO  P-CP-3-PREM-REV              
04988          ELSE                                                     
04989              MOVE WS-SRT02-AH-PRM-PR                              
04990                                  TO  P-CP-PREM-REV                
04991      ELSE                                                         
04992          IF WS-SRT02-PREM-OVERRIDE = 'B' OR 'A'                   
04993              IF DTE-CLIENT = 'BWS'                                
04994                  MOVE '.00'      TO  P-CP-3-PREM-REV-ZEROS        
04995              ELSE                                                 
04996                  MOVE '.00'      TO  P-CP-PREM-REV-ZEROS.         
04997                                                                   
04998      IF WS-SRT02-AH-COMM = ZERO                                   
04999          MOVE ZERO               TO  WORK-PERC                    
05000      ELSE                                                         
05001          COMPUTE WORK-PERC = WS-SRT02-AH-COM-PCT * +100.          
05002                                                                   
05003      IF DTE-CLIENT = 'BWS'                                        
05004          MOVE WORK-PERC          TO  P-CP-3-COMM-PCT              
05005          MOVE WS-SRT02-AH-COMM   TO  P-CP-3-COMM                  
05006      ELSE                                                         
05007          IF DTE-CLIENT = 'KSM'                                    
05008              MOVE WS-SRT02-AH-COMM                                
05009                                  TO  P-CP-COMM                    
05010          ELSE                                                     
05011              MOVE WORK-PERC      TO  P-CP-COMM-PCT                
05012              MOVE WS-SRT02-AH-COMM                                
05013                                  TO  P-CP-COMM.                   
05014                                                                   
05015      IF AH-ONLY                                                   
05016          IF DTE-CLIENT = 'TIC'                                    
05017              MOVE 'N'                TO  AH-ONLY-SWITCH           
05018              MOVE SPACE-2            TO  P-CCSW                   
05019              PERFORM 3500-CHECK-TIC  THRU  3599-XIT               
05020              PERFORM 3190-PRT-DETAIL                              
05021              MOVE SPACE-1            TO  P-CCSW                   
05022          ELSE                                                     
031416*            MOVE 'N'                TO  AH-ONLY-SWITCH           
05024              MOVE SPACE-2            TO  P-CCSW                   
05025      ELSE                                                         
05026          MOVE SPACE-1            TO  P-CCSW.                      
05027                                                                   
05028      PERFORM 3500-CHECK-TIC  THRU  3599-XIT.                      
05029                                                                   
05030      IF DTE-CLIENT = 'BWS'                                        
05031          IF WS-SRT02-AH-PRM-PR = ZEROS                            
05032              ADD WS-SRT02-AH-PREM TO NET-DUE                      
05033              SUBTRACT WS-SRT02-AH-COMM FROM NET-DUE               
05034              COMPUTE TOT-NET-DUE =                                
05035              TOT-NET-DUE + WS-SRT02-AH-PREM - WS-SRT02-AH-COMM    
05036              MOVE NET-DUE        TO  P-CP-3-NET-AMT               
05037          ELSE                                                     
05038              ADD WS-SRT02-AH-PRM-PR TO NET-DUE                    
05039              SUBTRACT WS-SRT02-AH-COMM FROM NET-DUE               
05040              COMPUTE TOT-NET-DUE =                                
05041              TOT-NET-DUE + WS-SRT02-AH-PRM-PR - WS-SRT02-AH-COMM  
05042              MOVE NET-DUE        TO  P-CP-3-NET-AMT.              
05043                                                                   
092815     IF NOT AH-ONLY
092815        IF WS-SRT02-VIN Not = spaces
092815           string
092815              '   VIN - '
092815              WS-SRT02-VIN
092815                 DELIMITED BY SIZE INTO P-LINE
092815           END-STRING
092815        END-IF
092815        GO TO 3190-PRT-DETAIL
092815     END-IF
092815
031416     MOVE 'N'                    TO AH-ONLY-SWITCH
092815     IF WS-SRT02-VIN Not = spaces
092815        PERFORM 3190-PRT-DETAIL
092815        string
092815           '   VIN - '
092815           WS-SRT02-VIN
092815              DELIMITED BY SIZE INTO P-LINE
092815        END-STRING
092815     END-IF

05044      GO TO 3190-PRT-DETAIL.                                       
05045  EJECT                                                            
05046  3140-PRT-CANCEL.                                                 
05047      IF WS-SRT-REC-SFX GREATER ZERO                               
05048         PERFORM 3500-CHECK-TIC  THRU  3599-XIT                   
010616        if ws-supp-check
010616           move spaces           to p-check-line
010616           move ws-srt03a-check-amt
010616                                 to ws-formatted-amount
010616           move ws-srt03a-chek-dt
010616                                 to ws-srt03a-chek-dt-r
010616           if ws-srt03a-chek-num not = spaces and zeros
010616              string
010616                 'Refund of ' delimited by size
010616                 ws-formatted-amount delimited by size
010616                 ' Paid to ' delimited by size
010616                 ws-srt03a-payee delimited by '   '
010616                 ' By Check No ' delimited by size
010616                 ws-srt03a-chek-num delimited by size
010616                 ' Dated ' delimited by size
010616                 ws-srt03a-ck-mo '-'
010616                 ws-srt03a-ck-da '-'
010616                 ws-srt03a-ck-yr
010616                    delimited by size into p-check-line
010616              end-string
010616           else
010616              string
010616                 'Refund of '
010616                 ws-formatted-amount
010616                 ' Paid to '
010616                 ws-srt03a-payee
010616                    delimited by size into p-check-line
010616              end-string
010616           end-if
                 MOVE SPACE-1          TO P-CCSW              
                 GO TO 3190-PRT-DETAIL
05063         ELSE                                                     
05064            IF WS-SUPP-NOTE                                      
05065               MOVE '** NOTE **'  TO P-LF-NOTE           
05066               MOVE WS-SRT02-REC  TO P-L4-MESSAGE        
05067               MOVE SPACE-1       TO P-CCSW              
05068               GO TO 3190-PRT-DETAIL                            
05069            ELSE                                                 
05070               MOVE WS-SRT02-REC  TO P-L4-MESSAGE        
05071               MOVE SPACE-1       TO P-CCSW              
05072               GO TO 3190-PRT-DETAIL
010616           end-if
010616        end-if
010616     end-if
05073                                                                   
05074      GO TO 3160-END-CANCEL.                                       
05075                                                                   
05076  3145-PRT-CANCEL-ERROR.                                           
05077      IF WS-SRT-REC-SFX GREATER ZERO                               
05078          PERFORM 3500-CHECK-TIC  THRU  3599-XIT                   
05079          IF WS-SUPP-NOTE                                          
05080              MOVE '** NOTE **'      TO  P-LF-NOTE                 
05081              MOVE WS-SRT02-REC      TO  P-L4-MESSAGE              
05082              MOVE SPACE-1           TO  P-CCSW                    
05083              GO TO 3190-PRT-DETAIL                                
05084          ELSE                                                     
05085              MOVE SPACES            TO  P-LF-SEVERITY             
05086              MOVE WS-SRT04-ERR-TXT  TO  P-L4-MESSAGE              
05087              MOVE SPACE-1           TO  P-CCSW                    
05088              GO TO 3190-PRT-DETAIL.                               
05089                                                                   
05090      GO TO 3160-END-CANCEL.                                       
05091  EJECT                                                            
05092  3150-PRT-CANCEL-HOLD.                                            
05093      IF WS-SRT-REC-SFX GREATER ZERO                               
05094          PERFORM 3500-CHECK-TIC  THRU  3599-XIT                   
05095          IF WS-SUPP-ERROR                                         
05096              MOVE SPACES            TO  P-LF-SEVERITY             
05097              MOVE WS-SRT04-ERR-TXT  TO  P-L4-MESSAGE              
05098              MOVE SPACE-1           TO  P-CCSW                    
05099              GO TO 3190-PRT-DETAIL                                
05100          ELSE                                                     
05101              IF WS-SUPP-NOTE                                      
05102                  MOVE '** NOTE **'  TO  P-LF-NOTE                 
05103                  MOVE WS-SRT02-REC  TO  P-L4-MESSAGE              
05104                  MOVE SPACE-1       TO  P-CCSW                    
05105                  GO TO 3190-PRT-DETAIL                            
05106              ELSE                                                 
05107                  MOVE WS-SRT02-REC  TO  P-L4-MESSAGE              
05108                  MOVE SPACE-1       TO  P-CCSW                    
05109                  GO TO 3190-PRT-DETAIL.                           
05110                                                                   
05111      GO TO 3160-END-CANCEL.                                       
05112                                                                   
05113  3155-PRT-CANCEL-RETURN.                                          
05114      IF WS-SRT-REC-SFX GREATER ZERO                               
05115          PERFORM 3500-CHECK-TIC  THRU  3599-XIT                   
05116          IF WS-SUPP-ERROR                                         
05117              MOVE SPACES            TO  P-LF-SEVERITY             
05118              MOVE WS-SRT04-ERR-TXT  TO  P-L4-MESSAGE              
05119              MOVE SPACE-1           TO  P-CCSW                    
05120              GO TO 3190-PRT-DETAIL                                
05121          ELSE                                                     
05122              IF WS-SUPP-NOTE                                      
05123                  MOVE '** NOTE **'  TO  P-LF-NOTE                 
05124                  MOVE WS-SRT02-REC  TO  P-L4-MESSAGE              
05125                  MOVE SPACE-1       TO  P-CCSW                    
05126                  GO TO 3190-PRT-DETAIL                            
05127              ELSE                                                 
05128                  MOVE WS-SRT02-REC  TO  P-L4-MESSAGE              
05129                  MOVE SPACE-1       TO  P-CCSW                    
05130                  GO TO 3190-PRT-DETAIL.                           
05131  EJECT                                                            
05132  3160-END-CANCEL.                                                 
05133                                                                   
05134      MOVE ZEROS                  TO  NET-DUE.                     
05135      MOVE 'N'                    TO  LF-ONLY-SWITCH.              
05136      MOVE 'N'                    TO  AH-ONLY-SWITCH.              
05137      MOVE 'NNN'                  TO  COV-TABLE.                   
05138                                                                   
05139      IF (WS-SRT03-AH-CAN-DT = ZEROS) AND                          
05140         (WS-SRT03-AH-REF = ZEROS)    AND                          
05141         (WS-SRT03-LF-CAN-DT = ZEROS) AND                          
05142         (WS-SRT03-LF-REF = ZEROS)                                 
05143          MOVE 'Y'                TO  LF-COV.                      
05144                                                                   
05145      IF WS-SRT03-LF-REF NOT = ZEROS                               
05146          MOVE 'Y'                TO  LF-COV.                      
05147                                                                   
05148      IF WS-SRT03-AH-REF NOT = ZEROS                               
05149          MOVE 'Y'                TO  AH-COV.                      

           IF DTE-CLIENT = 'DCC' or 'CAP'
              IF WS-SRT03-AH-CAN-DT NOT = ZEROS
                 MOVE 'Y'              TO  AH-COV
              END-IF
           END-IF
05150                                                                   
05151      IF (WS-SRT03-LF-CAN-DT NOT = ZEROS) AND                      
05152         (WS-SRT03-AH-CAN-DT NOT = ZEROS) AND                      
05153         (WS-SRT03-AH-CAN-DT NOT = WS-SRT03-LF-CAN-DT)             
05154          MOVE 'Y'                TO  LF-COV                       
05155                                      AH-COV.                      
05156                                                                   
05157      IF WS-SRT03-RECALC-FLAG  IS EQUAL TO  'R'                    
05158          MOVE '(R)'              TO  PC-CP-RECALC                 
05159          MOVE WS-SRT03-LNAME     TO  PC-CP-LNAME-R
090309         MOVE WS-SRT03-FNAME     TO  PC-CP-FNAME-R
05160      ELSE                                                         
05161 *        MOVE WS-SRT03-FI        TO  PC-CP-1ST-INIT               
090309         MOVE WS-SRT03-FNAME     TO  PC-CP-FNAME    
05162          MOVE WS-SRT03-INTL      TO  PC-CP-INITIAL                
05163          MOVE WS-SRT03-LNAME     TO  PC-CP-LNAME.                 
05164                                                                   
05165      MOVE WS-SRT03-CERT          TO  PC-CP-CERT.                  
05166      MOVE WS-SRT03-EFF-DT        TO  WS-SRT03-EFF-DT-R.           
05167      MOVE WS-SRT03-EFF-MO        TO  PC-CP-EFF-MO.                
05168      MOVE WS-SRT03-EFF-DA        TO  PC-CP-EFF-DA.                
05169      MOVE WS-SRT03-EFF-YR        TO  PC-CP-EFF-YR.                
05170      MOVE '-'                    TO  PC-CP-EFF-MOD                
05171                                      PC-CP-EFF-DAD.               
05172                                                                   
05173      IF COV-TABLE = 'NNY'                                         
05174          MOVE 'Y'                TO  AH-ONLY-SWITCH               
05175          GO TO 3165-PRT-AH.                                       
05176                                                                   
05177      IF WS-SRT03-LF-CAN-DT NOT EQUAL ZEROS                        
05178          MOVE WS-SRT03-LF-CAN-DT TO  WS-SRT03-LF-CAN-DT-R         
05179          MOVE WS-SRT03-LF-CAN-MO TO  PC-CP-CAN-MO                 
05180          MOVE WS-SRT03-LF-CAN-DA TO  PC-CP-CAN-DA                 
05181          MOVE WS-SRT03-LF-CAN-YR TO  PC-CP-CAN-YR                 
05182          MOVE '-'                TO  PC-CP-CAN-MOD                
05183                                      PC-CP-CAN-DAD.               
05184                                                                   
05185      IF DTE-FMT-OPT = '2' OR DTE-CLIENT = 'BWS'                   
05186          MOVE LIFE-OVERRIDE-L2   TO  PC-CP-BEN                    
05187          MOVE '-'                TO  PC-CP-BEN-DASH               
05188          MOVE WS-SRT03-LF-TYP    TO  PC-CP-BEN-TYPE               
05189          IF DTE-CLIENT = 'BWS'                                    
05190              MOVE WS-SRT03-LF-REF                                 
05191                                  TO  PC-CP-3-REF                  
05192          ELSE                                                     
05193              MOVE WS-SRT03-LF-REF                                 
05194                                  TO  PC-CP-2-REF                  
05195      ELSE                                                         
05196          MOVE WS-SRT03-LF-REF    TO  PC-CP-LF-REF.                
05197                                                                   
05198      IF WS-SRT03-LF-REF-REV NOT = ZERO                            
05199          IF DTE-CLIENT = 'BWS'                                    
05200              MOVE WS-SRT03-LF-REF-REV                             
05201                                    TO  PC-CP-3-REF-REV            
05202          ELSE                                                     
05203              IF DTE-FMT-OPT = '2'                                 
05204                  MOVE WS-SRT03-LF-REF-REV                         
05205                                    TO  PC-CP-2-REF-REV            
05206              ELSE                                                 
05207                  MOVE WS-SRT03-LF-REF-REV                         
05208                                    TO  PC-CP-LF-REF-REV           
05209      ELSE                                                         
05210          IF WS-SRT03-REF-OVERRIDE = 'B' OR 'L'                    
05211              IF DTE-CLIENT = 'BWS'                                
05212                  MOVE '.00'        TO  PC-CP-3-REF-REV-ZEROS      
05213              ELSE                                                 
05214                  IF DTE-FMT-OPT = '2'                             
05215                      MOVE '.00'    TO  PC-CP-2-REF-REV-ZEROS      
05216                  ELSE                                             
05217                      MOVE '.00'    TO  PC-CP-LF-REF-REV-ZEROS.    
05218                                                                   
05219      IF DTE-CLIENT = 'BWS'                                        
05220          MOVE WS-SRT03-LF-COM-REF                                 
05221                                  TO  PC-CP-3-COMM-AMT             
05222      ELSE                                                         
05223          IF DTE-FMT-OPT = '2'                                     
05224              MOVE WS-SRT03-LF-COM-REF                             
05225                                  TO  PC-CP-2-COMM-AMT             
05226          ELSE                                                     
05227              MOVE WS-SRT03-LF-COM-REF                             
05228                                  TO  PC-CP-LCOM-REF.              
05229                                                                   
05230      IF DTE-FMT-OPT = '2' OR DTE-CLIENT = 'BWS'                   
05231          IF WS-SRT03-LF-COM-REF = ZEROS                           
05232              MOVE ZEROS          TO  WORK-PERC                    
05233              IF DTE-CLIENT = 'BWS'                                
05234                  MOVE WORK-PERC  TO  PC-CP-3-COMM-PCT             
05235              ELSE                                                 
05236                  MOVE WORK-PERC  TO  PC-CP-2-COMM-PCT             
05237          ELSE                                                     
05238              COMPUTE WORK-PERC = WS-SRT03-LF-COM-PCT * +100       
05239              IF DTE-CLIENT = 'BWS'                                
05240                  MOVE WORK-PERC  TO  PC-CP-3-COMM-PCT             
05241              ELSE                                                 
05242                  MOVE WORK-PERC  TO  PC-CP-2-COMM-PCT.            
05243                                                                   
05244      MOVE SPACE-2                TO  P-CCSW.                      
05245                                                                   
05246      IF (DTE-FMT-OPT NOT = '2') AND                               
05247         (DTE-CLIENT NOT = 'BWS')                                  
05248          IF AH-COV = 'Y' AND                                      
05249             (WS-SRT03-AH-CAN-DT = WS-SRT03-LF-CAN-DT)             
05250              GO TO 3165-PRT-AH.                                   
05251                                                                   
05252      PERFORM 3500-CHECK-TIC  THRU  3599-XIT.                      
05253                                                                   
05254      IF DTE-CLIENT = 'BWS'                                        
05255          IF WS-SRT03-LF-REF-REV = ZEROS                           
05256              ADD WS-SRT03-LF-REF TO  NET-DUE                      
05257              SUBTRACT WS-SRT03-LF-COM-REF FROM NET-DUE            
05258              COMPUTE TOT-NET-DUE = TOT-NET-DUE                    
05259                          + WS-SRT03-LF-REF - WS-SRT03-LF-COM-REF  
05260              IF COV-TABLE = 'YNN'                                 
05261                  MOVE NET-DUE    TO  PC-CP-3-NET-AMT              
05262              ELSE                                                 
05263                  NEXT SENTENCE                                    
05264          ELSE                                                     
05265              ADD WS-SRT03-LF-REF-REV TO  NET-DUE                  
05266              SUBTRACT WS-SRT03-LF-COM-REF FROM NET-DUE            
05267              COMPUTE TOT-NET-DUE =                                
05268                    TOT-NET-DUE + WS-SRT03-LF-REF-REV              
05269                                - WS-SRT03-LF-COM-REF              
05270              IF COV-TABLE = 'YNN'                                 
05271                  MOVE NET-DUE    TO  PC-CP-3-NET-AMT              
05272              ELSE                                                 
05273                  NEXT SENTENCE.                                   
05274                                                                   
05275      PERFORM 3190-PRT-DETAIL.                                     
05276                                                                   
05277      IF COV-TABLE = 'YNN'                                         
05278          MOVE 'Y'                TO  LF-ONLY-SWITCH               
05279          GO TO 3190-PRT-DETAIL.                                   
05280                                                                   
05281  3165-PRT-AH.                                                     
05282      IF WS-SRT03-AH-CAN-DT = ZEROS                                
05283          IF DTE-CLIENT = 'TIC'                                    
05284              PERFORM 3500-CHECK-TIC  THRU  3599-XIT               
05285              PERFORM 3190-PRT-DETAIL                              
05286              MOVE 'Y'            TO  LF-ONLY-SWITCH               
05287              GO TO 3190-PRT-DETAIL                                
05288          ELSE                                                     
05289              MOVE 'Y'            TO  LF-ONLY-SWITCH               
05290              GO TO 3190-PRT-DETAIL.                               
05291                                                                   
052504*    IF WS-SRT03-AH-CAN-DT = WS-SRT03-LF-CAN-DT                   
052504*        GO TO 3170-PRT-AH.                                       
05294                                                                   
05295      MOVE WS-SRT03-AH-CAN-DT     TO  WS-SRT03-AH-CAN-DT-R.        
05296      MOVE WS-SRT03-AH-CAN-MO     TO  PC-CP-CAN-MO.                
05297      MOVE WS-SRT03-AH-CAN-DA     TO  PC-CP-CAN-DA.                
05298      MOVE WS-SRT03-AH-CAN-YR     TO  PC-CP-CAN-YR.                
05299      MOVE '-'                    TO  PC-CP-CAN-MOD                
05300                                      PC-CP-CAN-DAD.               
05301                                                                   
05302  3170-PRT-AH.                                                     
05303      IF DTE-FMT-OPT = '2' OR DTE-CLIENT = 'BWS'                   
05304          MOVE AH-OVERRIDE-L2     TO  PC-CP-BEN                    
05305          MOVE '-'                TO  PC-CP-BEN-DASH               
05306          MOVE WS-SRT03-AH-TYP    TO  PC-CP-BEN-TYPE               
05307          IF DTE-CLIENT = 'BWS'                                    
05308              MOVE WS-SRT03-AH-REF                                 
05309                                  TO  PC-CP-3-REF                  
05310          ELSE                                                     
05311             MOVE WS-SRT03-AH-REF                                 
05312                                  TO  PC-CP-2-REF                  
092705            IF WS-SRT03-AH-BEN-CAT = 'G' OR 'L'
100703               COMPUTE PC-CP-2-REF-REV = (WS-SRT03-AH-REF
100703                - WS-SRT03-AH-COM-REF)
100703               DISPLAY ' FOUND BEN CAT ' PC-CP-AH-REF-REV
100703            END-IF
100703         END-IF
05313      ELSE                                                         
05314          MOVE WS-SRT03-AH-REF    TO  PC-CP-AH-REF
100703     END-IF
05315                                                                   
05316      IF WS-SRT03-AH-REF-REV NOT = ZERO                            
05317          IF DTE-CLIENT = 'BWS'                                    
05318              MOVE WS-SRT03-AH-REF-REV                             
05319                                    TO  PC-CP-3-REF-REV            
05320          ELSE                                                     
05321              IF DTE-FMT-OPT = '2'                                 
05322                  MOVE WS-SRT03-AH-REF-REV                         
05323                                    TO  PC-CP-2-REF-REV            
05324              ELSE                                                 
05325                  MOVE WS-SRT03-AH-REF-REV                         
05326                                    TO  PC-CP-AH-REF-REV           
05327      ELSE                                                         
05328          IF WS-SRT03-REF-OVERRIDE = 'B' OR 'A'                    
05329              IF DTE-CLIENT = 'BWS'                                
05330                  MOVE '.00'      TO  PC-CP-3-REF-REV-ZEROS        
05331              ELSE                                                 
05332                  IF DTE-FMT-OPT = '2'                             
05333                      MOVE '.00'  TO  PC-CP-2-REF-REV-ZEROS        
05334                  ELSE                                             
05335                      MOVE '.00'  TO  PC-CP-AH-REF-REV-ZEROS.      
05336                                                                   
05337      IF DTE-CLIENT = 'BWS'                                        
05338          MOVE WS-SRT03-AH-COM-REF                                 
05339                                  TO  PC-CP-3-COMM-AMT             
05340      ELSE                                                         
05341          IF DTE-FMT-OPT = '2'                                     
05342              MOVE WS-SRT03-AH-COM-REF                             
05343                                  TO  PC-CP-2-COMM-AMT             
05344          ELSE                                                     
05345              MOVE WS-SRT03-AH-COM-REF                             
05346                                  TO  PC-CP-ACOM-REF.              
05347                                                                   
05348      IF DTE-FMT-OPT = '2'                                         
05349          IF WS-SRT03-AH-COM-REF = ZERO                            
05350              MOVE ZERO           TO  WORK-PERC                    
05351              MOVE WORK-PERC      TO  PC-CP-2-COMM-PCT             
05352          ELSE                                                     
05353              COMPUTE WORK-PERC = WS-SRT03-AH-COM-PCT * +100       
05354              MOVE WORK-PERC      TO PC-CP-2-COMM-PCT.             
05355                                                                   
05356      IF DTE-CLIENT = 'BWS'                                        
05357          IF WS-SRT03-AH-COM-REF = ZERO                            
05358              MOVE ZERO           TO  WORK-PERC                    
05359              MOVE WORK-PERC      TO  PC-CP-3-COMM-PCT             
05360          ELSE                                                     
05361              COMPUTE WORK-PERC = WS-SRT03-AH-COM-PCT * +100       
05362              MOVE WORK-PERC      TO PC-CP-3-COMM-PCT.             
05363                                                                   
05364      IF AH-ONLY                                                   
05365          IF DTE-CLIENT = 'TIC'                                    
05366              MOVE 'N'            TO  AH-ONLY-SWITCH               
05367              MOVE SPACE-2        TO  P-CCSW                       
05368              PERFORM 3500-CHECK-TIC  THRU  3599-XIT               
05369              PERFORM 3190-PRT-DETAIL                              
05370              MOVE SPACE-1        TO  P-CCSW                       
05371          ELSE                                                     
05372              MOVE 'N'            TO  AH-ONLY-SWITCH               
05373              MOVE SPACE-2        TO  P-CCSW                       
05374      ELSE                                                         
05375          IF WS-SRT03-AH-CAN-DT = WS-SRT03-LF-CAN-DT               
05376              IF DTE-CLIENT = 'TIC'                                
05377                  PERFORM 3500-CHECK-TIC  THRU  3599-XIT           
05378                  PERFORM 3190-PRT-DETAIL                          
05379                  MOVE SPACE-1    TO  P-CCSW                       
05380              ELSE                                                 
05381                  NEXT SENTENCE                                    
05382          ELSE                                                     
05383              MOVE SPACE-1        TO  P-CCSW.                      
05384                                                                   
05385      PERFORM 3500-CHECK-TIC  THRU  3599-XIT.                      
05386                                                                   
05387      IF DTE-CLIENT = 'BWS'                                        
05388          IF WS-SRT03-AH-REF-REV = ZEROS                           
05389              ADD WS-SRT03-AH-REF TO NET-DUE                       
05390              SUBTRACT WS-SRT03-AH-COM-REF FROM NET-DUE            
05391              COMPUTE TOT-NET-DUE =                                
05392                    TOT-NET-DUE + WS-SRT03-AH-REF                  
05393                                - WS-SRT03-AH-COM-REF              
05394              MOVE NET-DUE        TO  PC-CP-3-NET-AMT              
05395          ELSE                                                     
05396              ADD WS-SRT03-AH-REF-REV TO NET-DUE                   
05397              SUBTRACT WS-SRT03-AH-COM-REF FROM NET-DUE            
05398              COMPUTE TOT-NET-DUE =                                
05399                    TOT-NET-DUE + WS-SRT03-AH-REF-REV              
05400                                - WS-SRT03-AH-COM-REF              
05401              MOVE NET-DUE        TO  PC-CP-3-NET-AMT.             

092815     if lf-cov = 'Y' or alt-cov = 'Y'
092815        IF WS-SRT03-VIN Not = spaces
092815           string
092815              '   VIN - '
092815              WS-SRT03-VIN
092815                 DELIMITED BY SIZE INTO P-LINE
092815           END-STRING
092815        END-IF
092815        GO TO 3190-PRT-DETAIL
092815     end-if
092815
092815     IF WS-SRT03-VIN Not = spaces
092815        perform 3190-prt-detail
092815        string
092815           '   VIN - '
092815           WS-SRT03-VIN
092815              DELIMITED BY SIZE INTO P-LINE
092815        END-STRING
092815     END-IF

05403      GO TO 3190-PRT-DETAIL.                                       
05404                                                                   
05405  3175-PRT-ACCTG.                                                  
05406      MOVE WS-SRT10-DESC          TO  P-PYMT-DESC.                 
05407      MOVE SPACES                 TO  P-ACCT-ENTRY.                
05408      MOVE WS-SRT10-PYMT          TO  P-R-PMT.                     
05409      MOVE WS-SRT10-CHRG          TO  P-C-CHG.

082707     STRING WS-SRT10-MAINT-DATE (3:2) '/'
082707        WS-SRT10-MAINT-DATE (5:2) '/'
082707        WS-SRT10-MAINT-DATE (1:2) DELIMITED BY SIZE
082707        INTO P-ACCT-MAINT-DATE
082707     END-STRING

05410      MOVE SPACE-1                TO  P-CCSW.                      
05411                                                                   
05412      GO TO 3190-PRT-DETAIL.                                       
05413  EJECT                                                            
05414  3180-PRT-CLAIM.                                                  
05415      MOVE SPACE-1                TO  P-CCSW.                      
060603*    MOVE WS-SRT12-FNAME         TO  P-CP-FNAME.                  
060603*    MOVE WS-SRT12-INTL          TO  P-CP-INITIAL.                
060603*    MOVE WS-SRT12-LNAME         TO  P-CP-LNAME.                  
05419      MOVE WS-SRT12-CERT          TO  P-CP-CERT.                   
05420      MOVE WS-SRT12-EFF-DT        TO  WS-SRT12-EFF-DT-R.           
05421      MOVE WS-SRT12-EFF-MO        TO  P-CP-EFF-MO.                 
05422      MOVE WS-SRT12-EFF-DA        TO  P-CP-EFF-DA.                 
05423      MOVE WS-SRT12-EFF-YR        TO  P-CP-EFF-YR.                 
05424      MOVE '-'                    TO  P-CP-EFF-MOD  P-CP-EFF-DAD.  
05425                                                                   
05426      IF WS-SRT12-LF-CLM = ZERO                                    
05427          MOVE AH-OVERRIDE-L6     TO  P-CLAIM-L-A                  
05428          MOVE 'CLAIM PAID'       TO  P-CLAIM-MSG                  
05429          MOVE WS-SRT12-AH-CLM    TO  P-CLAIM-AMT                  
05430      ELSE                                                         
05431          MOVE LIFE-OVERRIDE-L6   TO  P-CLAIM-L-A                  
05432          MOVE 'CLAIM PAID'       TO  P-CLAIM-MSG                  
05433          MOVE WS-SRT12-LF-CLM    TO  P-CLAIM-AMT.                 
05434                                                                   
05435      IF DTE-CLIENT = 'TIC'                                        
05436          MOVE WS-SRT12-MEMBER-NO                                  
05437                                  TO  P-CLAIM-MEMBER.              
05438                                                                   
05439  3190-PRT-DETAIL.                                                 
05440                                                                   
05441      IF HEADING-NOT-PRINTED                                       
05442          PERFORM 3900-PRT-STMT-HD-RTN  THRU  3999-EXIT.           
05443                                                                   
05444      IF LF-ONLY                                                   
05445          MOVE SPACES             TO  P-REC                        
05446          MOVE 'N'                TO  LF-ONLY-SWITCH               
05447          GO TO 3195-END-DETAIL-PRINT.                             
05448                                                                   
05449      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
05450                                                                   
05451  3195-END-DETAIL-PRINT.                                           
05452                                                                   
05453      IF DTE-CLIENT = 'KSM'                                        
05454          IF WS-SRT-REC-SFX = ZERO                                 
05455              IF WS-ISSUE OR WS-CANCEL                             
05456                  PERFORM 6500-WRITE-BILLING-DET  THRU  6599-EXIT. 
05457                                                                   
05458  3199-EXIT.                                                       
05459      EXIT.                                                        
05460  EJECT                                                            
05461  3200-PRT-LIFE-TOTALS.                                            
05462      IF LNCTR GREATER LNCTR-43-58                                 
05463          PERFORM 3900-PRT-STMT-HD-RTN  THRU  3999-EXIT.           

05464      IF DTE-CLIENT = 'BWS'                                        
05465          MOVE NST-L-ISS          TO  LNB-LISS-NL                  
05466          MOVE NST-L-PRM          TO  LNB-LPRM-NL                  
011904*        MOVE NST-L-COM          TO  LNB-LCOM-NL                  
011904         COMPUTE LNB-LCOM-NL = NST-L-COM - NST-L-RCM
05468          MOVE ISSUE-TOT-LIFE-NET TO  P-LINE                       
05469          MOVE SPACE-2            TO  P-CCSW                       
05470          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    
05471          MOVE NST-A-ISS          TO  LNB-AISS-NA                  
05472          MOVE NST-A-PRM          TO  LNB-APRM-NA                  
011904*        MOVE NST-A-COM          TO  LNB-ACOM-NA                  
011904         COMPUTE LNB-ACOM-NA = NST-A-COM - NST-A-RCM
05474          MOVE TOT-NET-DUE        TO  LNB-NET-NA                   
05475          MOVE ISSUE-TOT-AH-NET   TO  P-LINE                       
05476          MOVE SPACE-1            TO  P-CCSW                       
05477          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    
05478      ELSE                                                         
05479          MOVE NST-L-ISS          TO  LNB-LISS                     
05480          MOVE NST-L-PRM          TO  LNB-LPRM                     
05481          MOVE NST-A-ISS          TO  LNB-AISS                     
05482          MOVE NST-A-PRM          TO  LNB-APRM                     
011904*        MOVE NST-L-COM          TO  LNB-LCOM                     
011904         COMPUTE LNB-LCOM = NST-L-COM - NST-L-RCM
011904*        MOVE NST-A-COM          TO  LNB-ACOM                     
011904         COMPUTE LNB-ACOM = NST-A-COM - NST-A-RCM 
05485          MOVE ISSUE-TOT-LINE     TO  P-LINE                       
05486          MOVE SPACE-2            TO  P-CCSW                       
05487          PERFORM 8800-PRT-RTN  THRU  8899-EXIT
           END-IF
05488                                                                   
05489      MOVE ZEROS                  TO  TOT-NET-DUE.                 
05490                                                                   
05491  3299-XIT.                                                        
05492      EXIT.                                                        

       3200-PRT-CASH-ISS-TOTALS.                                            

           IF LNCTR > LNCTR-43-58                                 
              PERFORM 3900-PRT-STMT-HD-RTN
                                       THRU 3999-EXIT
           END-IF

           MOVE CNST-L-ISS             TO LNB-LISS                     
           MOVE CNST-L-PRM             TO LNB-LPRM                     
           MOVE CNST-A-ISS             TO LNB-AISS                     
           MOVE CNST-A-PRM             TO LNB-APRM                     
           COMPUTE LNB-LCOM = CNST-L-COM - CNST-L-RCM
           COMPUTE LNB-ACOM = CNST-A-COM - CNST-A-RCM 
           MOVE ISSUE-TOT-LINE         TO P-LINE                       
           MOVE SPACE-2                TO P-CCSW                       
           PERFORM 8800-PRT-RTN        THRU 8899-EXIT
                                                                        
           MOVE ZEROS                  TO TOT-NET-DUE
           
           .                                                             
       3209-XIT.                                                        
           EXIT.                                                        

05494  3300-PRT-AH-TOTALS.                                              
05495      IF LNCTR GREATER LNCTR-43-58                                 
05496          PERFORM 3900-PRT-STMT-HD-RTN  THRU  3999-EXIT.           
05497                                                                   
05498      IF DTE-CLIENT = 'BWS'                                        
05499          MOVE NST-L-CAN          TO  ANB-LCAN-NL                  
05500          MOVE NST-L-REF          TO  ANB-LREF-NL                  
05501          MOVE NST-L-RCM          TO  ANB-LCOM-NL                  
05502          MOVE CANCEL-TOTAL-LINE-LIFE-NET                          
05503                                  TO  P-LINE                       
05504          MOVE SPACE-2            TO  P-CCSW                       
05505          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    
05506          MOVE NST-A-CAN          TO  ANB-ACAN-NA                  
05507          MOVE NST-A-REF          TO  ANB-AREF-NA                  
05508          MOVE NST-A-RCM          TO  ANB-ACOM-NA                  
05509          MOVE TOT-NET-DUE        TO  ANB-NET-DUE                  
05510          MOVE CANCEL-TOTAL-LINE-AH-NET                            
05511                                  TO  P-LINE                       
05512          MOVE SPACE-1            TO  P-CCSW                       
05513          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    
05514      ELSE                                                         
05515          MOVE NST-L-CAN          TO  ANB-LCAN                     
05516          MOVE NST-L-REF          TO  ANB-LREF                     
05517          MOVE NST-A-CAN          TO  ANB-ACAN                     
05518          MOVE NST-A-REF          TO  ANB-AREF                     
05519          MOVE NST-L-RCM          TO  ANB-LCOM                     
05520          MOVE NST-A-RCM          TO  ANB-ACOM                     
020210         COMPUTE WS-TOT-REF-COMM = NST-L-RCM + NST-A-RCM
05521          MOVE CANCEL-TOTAL-LINE  TO  P-LINE                       
05522          MOVE SPACE-2            TO  P-CCSW                       
05523          PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                   
05524                                                                   
05525      MOVE ZEROS                  TO  TOT-NET-DUE.                 
05526                                                                   
05527  3399-XIT.                                                        
05528      EXIT.                                                        

       3300-PRT-CASH-CAN-TOTALS.

           IF LNCTR > LNCTR-43-58                                 
              PERFORM 3900-PRT-STMT-HD-RTN
                                       THRU 3999-EXIT
           END-IF
                                                                        
           MOVE CNST-L-CAN             TO ANB-LCAN
           MOVE CNST-L-REF             TO ANB-LREF
           MOVE CNST-A-CAN             TO ANB-ACAN
           MOVE CNST-A-REF             TO ANB-AREF
           MOVE CNST-L-RCM             TO ANB-LCOM
           MOVE CNST-A-RCM             TO ANB-ACOM
           MOVE CANCEL-TOTAL-LINE      TO P-LINE
           MOVE SPACE-2                TO P-CCSW
           PERFORM 8800-PRT-RTN        THRU 8899-EXIT

           MOVE ZEROS                 TO  TOT-NET-DUE
           .
       3309-XIT.                                                        
           EXIT.                                                        
05529  EJECT                                                            
05530  3400-PRT-CLAIM-TOTALS.                                           
05531      MOVE NST-DTH-CLM            TO  CTL-DTH-CLM.                 
05532      MOVE NST-L-CLM              TO  CTL-L-CLM.                   
05533      MOVE NST-DIS-CLM            TO  CTL-DIS-CLM.                 
05534      MOVE NST-A-CLM              TO  CTL-A-CLM.                   
05535      MOVE NST-T-CLM              TO  CTL-T-CLM.                   
05536      MOVE CLAIM-TOTAL-LINE       TO  P-LINE.                      
05537      MOVE SPACE-2                TO  P-CCSW.                      
05538                                                                   
05539      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
05540                                                                   
05541  3499-XIT.                                                        
05542      EXIT.                                                        
05543  EJECT                                                            
05544  3500-CHECK-TIC.                                                  
05545      IF DTE-CLIENT NOT = 'TIC'                                    
05546          GO TO 3599-XIT.                                          
05547                                                                   
05548      IF TIC-LINE-SWITCH = '0'                                     
05549          MOVE '1'                TO  TIC-LINE-SWITCH              
05550          GO TO 3599-XIT.                                          
05551                                                                   
05552      IF TIC-CANCEL                                                
05553          GO TO 3520-TIC-CANCEL.                                   
05554                                                                   
05555  3510-TIC-ISSUE.                                                  
05556      IF TIC-LINE-SWITCH = '1'                                     
092815*        MOVE WS-SRT02-MEMBER-NO                                  
092815*                                TO  P-CP-MEMBER                  
05559          MOVE '2'                TO  TIC-LINE-SWITCH.             
05560                                                                   
05561      GO TO 3599-XIT.                                              
05562                                                                   
05563  3520-TIC-CANCEL.                                                 
05564      IF TIC-LINE-SWITCH = '1'                                     
092815*        MOVE WS-SRT03-MEMBER-NO                                  
092815*                                TO  PC-CP-MEMBER                 
05567          MOVE '2'                TO  TIC-LINE-SWITCH.             
05568                                                                   
05569  3599-XIT.                                                        
05570      EXIT.                                                        
05571  EJECT                                                            
05572  3600-DUMMY-HEADING.                                              
05573      MOVE +0                     TO  PGCTR.                       
05574      MOVE 'Y'                    TO  REC-02-FIRST-TIME-SW         
05575                                      REC-03-FIRST-TIME-SW         
05576                                      REC-04-FIRST-TIME-SW         
05577                                      REC-05-FIRST-TIME-SW         
05578                                      REC-06-FIRST-TIME-SW         
05579                                      REC-07-FIRST-TIME-SW         
05580                                      REC-08-FIRST-TIME-SW         
05581                                      REC-09-FIRST-TIME-SW         
05582                                      REC-10-FIRST-TIME-SW         
05583                                      REC-11-FIRST-TIME-SW         
05584                                      REC-13-FIRST-TIME-SW
                                           REC-013-FIRST-TIME-SW
                                           REC-017-FIRST-TIME-SW
05585      MOVE PRV-SRT-CARR-GROUP     TO  SAV-CARR-GROUP.              
05586      MOVE PRV-SRT-ACCT           TO  DUM-BIL0.                    
05587      MOVE PRV-SRT-REMIT          TO  DUM-RMT0.                    
05588      MOVE 'Y'                    TO  HEADING-SWITCH.              
05589                                                                   
05590  3610-DUMMY-HEAD.                                                 
05591      MOVE SAVE-COMPANY-NAME (3)  TO  HD-CO.
100307     MOVE SPACES                 TO  HD1-CSR
05592      MOVE HD1                    TO  P-LINE.                      
05593      MOVE SPACE-NP               TO  P-CCSW.                      
05594                                                                   
05595      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
05596                                                                   
05597      ADD +1                      TO  PGCTR
091306                                     TPGCTR
05598      MOVE PGCTR                  TO  HD-PG.                       
05599      MOVE HD2                    TO  P-LINE.                      
05600      MOVE SPACE-1                TO  P-CCSW.                      
05601                                                                   
05602      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
05603                                                                   
05604      MOVE HD3                    TO  P-LINE.                      
05605      MOVE SPACE-1                TO  P-CCSW.                      
05606                                                                   
05607      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
05608                                                                   
05609      MOVE 'ACCOUNT NO. - '       TO  HD-M1.                       
05610      MOVE SAV-CARR-GROUP         TO  HD-CARR-GROUP.               
05611      MOVE '-'                    TO  HD-C-A.                      
05612      MOVE DUM-BIL0               TO  HD-ACCT.                     
05613      MOVE DUM-BIL1               TO  HD-NAME.                     
05614      MOVE 'REMIT TO - '          TO  HD-M2.                       
05615                                                                   
05616      IF DUM-RMT0 = RMT0                                           
05617          MOVE RMT0               TO  HD-M3                        
05618          MOVE RMT1               TO  HD-REMIT                     
05619      ELSE                                                         
05620          MOVE DUM-RMT0           TO  HD-M3                        
05621          MOVE DUM-RMT1           TO  HD-REMIT.                    
05622                                                                   
05623      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
05624  EJECT                                                            
05625  3620-DUMMY-HEAD.                                                 
05626      MOVE DUM-BIL2               TO  HD-NAME.                     
05627                                                                   
05628      IF DUM-RMT0 = RMT0                                           
05629          MOVE RMT2               TO  HD-REMIT                     
05630      ELSE                                                         
05631          MOVE DUM-RMT1           TO  HD-REMIT.                    
05632                                                                   
05633      MOVE SPACE-1                TO  P-CCSW.                      
05634                                                                   
05635      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
05636                                                                   
05637      MOVE DUM-BIL3               TO  HD-NAME.                     
05638                                                                   
05639      IF DUM-RMT0 = RMT0                                           
05640          MOVE RMT3               TO  HD-REMIT                     
05641      ELSE                                                         
05642          MOVE DUM-RMT1           TO  HD-REMIT.                    
05643                                                                   
05644      MOVE SPACE-1                TO  P-CCSW.                      
05645                                                                   
05646      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
05647                                                                   
05648      MOVE DUM-BIL4               TO  HD-NAME.                     
05649                                                                   
05650      IF DUM-RMT0 = RMT0                                           
05651          MOVE RMT4               TO  HD-REMIT                     
05652      ELSE                                                         
05653          MOVE DUM-RMT1           TO  HD-REMIT.                    
05654                                                                   
05655      MOVE SPACE-1                TO  P-CCSW.                      
05656                                                                   
05657      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
05658                                                                   
05659      MOVE DUM-BIL5               TO  HD-NAME.                     
05660                                                                   
05661      IF DUM-RMT0 = RMT0                                           
05662          MOVE RMT5               TO  HD-REMIT                     
05663      ELSE                                                         
05664          MOVE DUM-RMT1           TO  HD-REMIT.                    
05665                                                                   
05666      MOVE SPACE-1                TO  P-CCSW.                      
05667                                                                   
05668      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
05669                                                                   
05670  3699-EXIT.                                                       
05671      EXIT.                                                        
05672  EJECT                                                            
05673  3700-DUMMY-TOTAL.                                                
05674      PERFORM 3610-DUMMY-HEAD.                                     
05675                                                                   
05676      MOVE HDR-DASH-LINE          TO  P-LINE.                      
05677      MOVE SPACE-2                TO  P-CCSW.                      
05678                                                                   
05679      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
05680                                                                   
05681      MOVE HD5J                   TO  P-LINE.                      
05682      MOVE SPACE-1                TO  P-CCSW.                      
05683                                                                   
05684      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
05685                                                                   
05686      MOVE HDR-DASH-LINE          TO  P-LINE.                      
05687      MOVE SPACE-1                TO  P-CCSW.                      
05688                                                                   
05689      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
05690                                                                   
05691      MOVE HD8D                   TO  P-LINE.                      
05692      MOVE SPACE-2                TO  P-CCSW.                      
05693                                                                   
05694      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
05695                                                                   
05696      MOVE 'Y'                    TO  DUMMY-TOTALS-SWITCH.         
05697      PERFORM 3800-PRT-TOTAL-RTN  THRU  3899-EXIT.                 
05698      MOVE 'N'                    TO  DUMMY-TOTALS-SWITCH.         
05699                                                                   
05700  3799-EXIT.                                                       
05701      EXIT.                                                        
05702  EJECT                                                            
05703  3800-PRT-TOTAL-RTN.                                              
05704      IF LNCTR GREATER LNCTR-25-40                                 
05705          PERFORM 3900-PRT-STMT-HD-RTN  THRU  3999-EXIT.           
05706                                                                   



      * if secure pay we need to add the issue commission here

           COMPUTE NST-L-ISS = NST-L-ISS + CNST-L-ISS
           COMPUTE NST-A-ISS = NST-A-ISS + CNST-A-ISS
           COMPUTE NST-L-PRM = NST-L-PRM + CNST-L-PRM
           COMPUTE NST-A-PRM = NST-A-PRM + CNST-A-PRM
           COMPUTE NST-L-COM = NST-L-COM + CNST-L-COM
           COMPUTE NST-A-COM = NST-A-COM + CNST-A-COM
           COMPUTE NST-L-RCM = NST-L-RCM + CNST-L-RCM
           COMPUTE NST-A-RCM = NST-A-RCM + CNST-A-RCM
           COMPUTE NST-L-CAN = NST-L-CAN + CNST-L-CAN
           COMPUTE NST-A-CAN = NST-A-CAN + CNST-A-CAN
           COMPUTE NST-L-REF = NST-L-REF + CNST-L-REF
           COMPUTE NST-A-REF = NST-A-REF + CNST-A-REF
           
           COMPUTE NST-T-PRM = NST-T-PRM + CNST-T-PRM
           COMPUTE NST-T-COM = NST-T-COM + CNST-T-COM
           COMPUTE NST-T-RCM = NST-T-RCM + CNST-T-RCM
           COMPUTE NST-T-REF = NST-T-REF + CNST-T-REF

           IF (SEC-PAY-CARRIER (CLAS-INDEXCN))
              AND (SAV-SPPDD NOT = 'Y')
      *       AND (PRV-SPP-CATEGORY NOT = 'D')
011904                                                                  
011904        MOVE 'TOTAL ISSUE FEES'  TO  T-DESC
011904        MOVE NST-T-PRM           TO  T-AMT-P
011904                                                                  
011904        COMPUTE NPS-T-PRM = NPS-L-PRM + NPS-A-PRM
011904        MOVE NPS-T-PRM           TO  T-AMT-N
011904                                                                  
011904        COMPUTE TOTS-T-PRM = NST-T-PRM + NPS-T-PRM
011904        MOVE TOTS-T-PRM          TO  T-AMT-T
011904        MOVE 'A'                 TO  T-EXPL
011904                                                                  
011904        MOVE SPACE-2             TO  P-CCSW
011904        PERFORM 8800-PRT-RTN     THRU 8899-EXIT
011904                                                                  
011904                                                                  
011904        MOVE 'TOTAL ISSUE INCOME'   TO  T-DESC
011904        MOVE NST-T-COM              TO  T-AMT-P
011904        COMPUTE NPS-T-COM = NPS-L-COM + NPS-A-COM
011904        MOVE NPS-T-COM              TO  T-AMT-N
011904        COMPUTE TOTS-T-COM = NST-T-COM + NPS-T-COM
011904        MOVE TOTS-T-COM             TO  T-AMT-T
011904        MOVE 'B'                    TO  T-EXPL
011904                                                                  
011904        MOVE SPACE-2             TO  P-CCSW
011904        PERFORM 8800-PRT-RTN  THRU  8899-EXIT


011904        MOVE 'TOTAL REFUND INCOME'  TO  T-DESC
011904        MOVE NST-T-RCM              TO  T-AMT-P
011904        COMPUTE NPS-T-RCM = NPS-L-RCM + NPS-A-RCM
011904        MOVE NPS-T-RCM              TO  T-AMT-N
011904        COMPUTE TOTS-T-RCM = NST-T-RCM + NPS-T-RCM
011904        MOVE TOTS-T-RCM             TO  T-AMT-T
011904        MOVE 'C'                    TO  T-EXPL
011904                                                                  
011904        MOVE SPACE-2             TO  P-CCSW
011904        PERFORM 8800-PRT-RTN  THRU  8899-EXIT

011904        COMPUTE NST-E-BAL  =  NST-T-PRM
011904           - NST-T-COM + NST-T-RCM
011904                                                                  
011904        MOVE 'AMOUNT DUE FOR THIS MONTH'  TO  T-DESC                 
011904        MOVE NST-E-BAL           TO  T-AMT-P                
011904        MOVE 'E=A-B+C'           TO  T-EXPL                 
011904        MOVE SPACE-2             TO  P-CCSW                 
011904                                                                  
011904        PERFORM 8800-PRT-RTN  THRU  8899-EXIT                        
011904                                                                  
011904     ELSE
              IF (SEC-PAY-CARRIER (CLAS-INDEXCN))
                 AND (SAV-SPPDD = 'Y')
      *          AND (PRV-SPP-CATEGORY = 'D')
                 MOVE 'LIFE FEES'      TO  T-DESC
              ELSE
011904           MOVE T-DESC-PRM-L     TO  T-DESC
              END-IF
011904        MOVE NST-L-PRM           TO  T-AMT-P
011904        MOVE NPS-L-PRM           TO  T-AMT-N
011904                                                                  
011904        COMPUTE TOTS-L-PRM = NST-L-PRM + NPS-L-PRM
011904        MOVE TOTS-L-PRM          TO  T-AMT-T
011904        MOVE SPACE-2             TO  P-CCSW
011904                                                                  
011904        PERFORM 8800-PRT-RTN  THRU  8899-EXIT
011904                                                                  
              IF (SEC-PAY-CARRIER (CLAS-INDEXCN))
                 AND (SAV-SPPDD = 'Y')
      *          AND (PRV-SPP-CATEGORY = 'D')
                 MOVE 'A&H FEES'       TO  T-DESC
              ELSE
011904           MOVE T-DESC-PRM-A     TO  T-DESC
              END-IF
011904        MOVE NST-A-PRM           TO  T-AMT-P
011904        MOVE NPS-A-PRM           TO  T-AMT-N
011904                                                                  
011904        COMPUTE TOTS-A-PRM = NST-A-PRM + NPS-A-PRM
011904        MOVE TOTS-A-PRM          TO  T-AMT-T
011904                                                                  
011904        PERFORM 8800-PRT-RTN  THRU  8899-EXIT
011904                                                                  
              IF (SEC-PAY-CARRIER (CLAS-INDEXCN))
                 AND (SAV-SPPDD = 'Y')
      *          AND (PRV-SPP-CATEGORY = 'D')
                 MOVE 'TOTAL FEES'     TO  T-DESC
              ELSE
011904           MOVE 'TOTAL PREMIUMS' TO  T-DESC
              END-IF
011904        MOVE NST-T-PRM           TO  T-AMT-P
011904                                                                  
011904        COMPUTE NPS-T-PRM = NPS-L-PRM + NPS-A-PRM
011904        MOVE NPS-T-PRM           TO  T-AMT-N
011904                                                                  
011904        COMPUTE TOTS-T-PRM = NST-T-PRM + NPS-T-PRM
011904        MOVE TOTS-T-PRM          TO  T-AMT-T
011904        MOVE 'A'                 TO  T-EXPL
011904                                                                  
011904        PERFORM 8800-PRT-RTN     THRU 8899-EXIT
011904                                                                  
011904        MOVE T-DESC-REF-L           TO  T-DESC
011904        MOVE NST-L-REF              TO  T-AMT-P
011904        MOVE NPS-L-REF              TO  T-AMT-N
011904                                                                  
011904        COMPUTE TOTS-L-REF = NST-L-REF + NPS-L-REF
011904        MOVE TOTS-L-REF             TO  T-AMT-T                      
011904        MOVE SPACE-2                TO  P-CCSW                       
011904                                                                  
011904        PERFORM 8800-PRT-RTN  THRU  8899-EXIT                        
011904                                                                  
011904        MOVE T-DESC-REF-A           TO  T-DESC                       
011904        MOVE NST-A-REF              TO  T-AMT-P                      
011904        MOVE NPS-A-REF              TO  T-AMT-N                      
011904                                                                  
011904        COMPUTE TOTS-A-REF = NST-A-REF + NPS-A-REF                   
011904        MOVE TOTS-A-REF             TO  T-AMT-T                      
011904                                                                  
011904        PERFORM 8800-PRT-RTN  THRU  8899-EXIT                        
011904                                                                  
011904        MOVE 'TOTAL REFUNDS '       TO  T-DESC                       
011904        MOVE NST-T-REF              TO  T-AMT-P                      
011904                                                                  
011904        COMPUTE NPS-T-REF = NPS-L-REF + NPS-A-REF                    
011904        MOVE NPS-T-REF              TO  T-AMT-N                      
011904                                                                  
011904        COMPUTE TOTS-T-REF = NST-T-REF + NPS-T-REF                   
011904        MOVE TOTS-T-REF             TO  T-AMT-T                      
011904        MOVE 'B'                    TO  T-EXPL                       
011904                                                                  
011904        PERFORM 8800-PRT-RTN  THRU  8899-EXIT                        

              IF (SEC-PAY-CARRIER (CLAS-INDEXCN))
                 AND (SAV-SPPDD = 'Y')
      *          AND (PRV-SPP-CATEGORY = 'D')
                 MOVE 'LIFE NET FEES'     TO  T-DESC
              ELSE
011904           MOVE T-DESC-NET-L        TO  T-DESC                       
              END-IF
011904        COMPUTE NST-L-NET  = NST-L-PRM - NST-L-REF                   
011904        COMPUTE NPS-L-NET  = NPS-L-PRM - NPS-L-REF                   
011904        MOVE NST-L-NET              TO  T-AMT-P                      
011904        MOVE NPS-L-NET              TO  T-AMT-N                      
011904                                                                  
011904        COMPUTE TOTS-L-NET = NST-L-NET + NPS-L-NET                   
011904        MOVE TOTS-L-NET             TO  T-AMT-T                      
011904        MOVE SPACE-2                TO  P-CCSW                       
011904                                                                  
011904        PERFORM 8800-PRT-RTN  THRU  8899-EXIT                        
011904                                                                  
              IF (SEC-PAY-CARRIER (CLAS-INDEXCN))
                 AND (SAV-SPPDD = 'Y')
      *          AND (PRV-SPP-CATEGORY = 'D')
                 MOVE 'A&H NET FEES'      TO  T-DESC
              ELSE
011904           MOVE T-DESC-NET-A        TO  T-DESC
              END-IF
011904        COMPUTE NST-A-NET  = NST-A-PRM - NST-A-REF                   
011904        COMPUTE NPS-A-NET  = NPS-A-PRM - NPS-A-REF                   
011904        MOVE NST-A-NET              TO  T-AMT-P                      
011904        MOVE NPS-A-NET              TO  T-AMT-N                      
011904                                                                  
011904        COMPUTE TOTS-A-NET = NST-A-NET + NPS-A-NET                   
011904        MOVE TOTS-A-NET             TO  T-AMT-T                      
011904                                                                  
011904        PERFORM 8800-PRT-RTN  THRU  8899-EXIT                        
011904                                                                  
              IF (SEC-PAY-CARRIER (CLAS-INDEXCN))
                 AND (SAV-SPPDD = 'Y')
      *          AND (PRV-SPP-CATEGORY = 'D')
                 MOVE 'TOTAL NET FEES'    TO  T-DESC
              ELSE
011904           MOVE 'TOTAL NET PREMIUMS' TO  T-DESC
              END-IF
011904        COMPUTE NST-T-NET  = NST-L-NET + NST-A-NET                   
011904        MOVE NST-T-NET              TO  T-AMT-P                      
011904                                                                  
011904        COMPUTE NPS-T-NET  = NPS-L-NET + NPS-A-NET                   
011904        MOVE NPS-T-NET              TO  T-AMT-N                      
011904                                                                  
011904        COMPUTE TOTS-T-NET = NST-T-NET + NPS-T-NET                   
011904        MOVE TOTS-T-NET             TO  T-AMT-T                      
011904        MOVE 'C=A-B'                TO  T-EXPL                       
011904                                                                  
011904        PERFORM 8800-PRT-RTN  THRU  8899-EXIT                        
011904                                                                  

011904        MOVE T-DESC-COM-L        TO  T-DESC                       
011904*       MOVE NST-L-COM           TO  T-AMT-P                      
011904        COMPUTE T-AMT-P = NST-L-COM - NST-L-RCM
011904        MOVE NPS-L-COM           TO  T-AMT-N                      
011904        COMPUTE T-AMT-N = NPS-L-COM - NPS-L-RCM
011904                                                                  
011904        COMPUTE TOTS-L-COM = NST-L-COM + NPS-L-COM                   
011904        COMPUTE TOTS-L-RCM = NST-L-RCM + NPS-L-RCM
011904        COMPUTE T-AMT-T = TOTS-L-COM - TOTS-L-RCM
011904        MOVE SPACE-2             TO  P-CCSW                       
011904                                                                  
011904        PERFORM 8800-PRT-RTN  THRU  8899-EXIT                        
011904                                                                  
011904        MOVE T-DESC-COM-A        TO  T-DESC                       
011904*       MOVE NST-A-COM           TO  T-AMT-P                      
011904        COMPUTE T-AMT-P = NST-A-COM - NST-A-RCM 
011904*       MOVE NPS-A-COM           TO  T-AMT-N                      
011904        COMPUTE T-AMT-N = NPS-A-COM - NPS-A-RCM
011904                                                                  
011904        COMPUTE TOTS-A-COM = NST-A-COM + NPS-A-COM                   
011904        COMPUTE TOTS-A-RCM = NST-A-RCM + NPS-A-RCM
011904*       MOVE TOTS-A-COM          TO  T-AMT-T                      
011904        COMPUTE T-AMT-T = TOTS-A-COM - TOTS-A-RCM
011904                                                                  
011904        PERFORM 8800-PRT-RTN     THRU  8899-EXIT                        
011904                                                                  
011904        MOVE 'TOTAL COMPENSATION'   TO  T-DESC                       
011904*       MOVE NST-T-COM           TO  T-AMT-P                      
011904        COMPUTE T-AMT-P = NST-T-COM - NST-T-RCM
011904        COMPUTE NPS-T-COM = NPS-L-COM + NPS-A-COM                    
011904        COMPUTE NPS-T-RCM = NPS-L-RCM + NPS-A-RCM
011904*       MOVE NPS-T-COM           TO  T-AMT-N                      
011904        COMPUTE T-AMT-N = NPS-T-COM - NPS-T-RCM
011904        COMPUTE TOTS-T-COM = NST-T-COM + NPS-T-COM                   
011904        COMPUTE TOTS-T-RCM = NST-T-RCM + NPS-T-RCM
011904*       MOVE TOTS-T-COM          TO  T-AMT-T                      
011904        COMPUTE T-AMT-T = TOTS-T-COM - TOTS-T-RCM
011904        MOVE 'D'                 TO  T-EXPL                       
011904                                                                  
011904        PERFORM 8800-PRT-RTN  THRU  8899-EXIT                        
011904                                                                  

011904        COMPUTE NST-E-BAL  =  NST-T-NET
011904           - (NST-T-COM - NST-T-RCM)
011904                                                                  
011904        MOVE 'AMOUNT DUE FOR THIS MONTH'  TO  T-DESC                 
011904        MOVE NST-E-BAL           TO  T-AMT-P                
011904        MOVE 'E=C-D'             TO  T-EXPL                 
011904        MOVE SPACE-2             TO  P-CCSW                 
011904                                                                  
011904        PERFORM 8800-PRT-RTN  THRU  8899-EXIT                        
011904                                                                  

011904     END-IF                                                             

05843      MOVE 'BALANCE BROUGHT FORWARD'  TO  T-DESC.                  
05844      MOVE 'F'                        TO  T-EXPL.                  
05845      MOVE SPACE-2                    TO  P-CCSW.                  
05846                                                                   
05847      IF DUMMY-TOTALS                                              
05848          MOVE NST-B-BAL              TO  T-AMT-P                  
05849      ELSE                                                         
05850          MOVE WS-SRT11-BEG-BAL       TO  T-AMT-P  NST-B-BAL.      
05851                                                                   
05852      COMPUTE NST-E-BAL  =  NST-E-BAL  +  NST-B-BAL                
05853                         -  NST-R-PMT  +  NST-C-CHG.               
05854                                                                   
05855      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
05856                                                                   
05857      MOVE 'AMOUNT PAID'          TO  T-DESC.                      
05858      MOVE NST-R-PMT              TO  T-AMT-P.                     
05859      MOVE 'G'                    TO  T-EXPL.                      
05860      MOVE SPACE-2                TO  P-CCSW.                      
05861                                                                   
05862      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
05863                                                                   
05864      MOVE 'AMOUNT CHARGED'       TO  T-DESC.                      
05865      MOVE NST-C-CHG              TO  T-AMT-P.                     
05866      MOVE 'H'                    TO  T-EXPL.                      
05867      MOVE SPACE-2                TO  P-CCSW.                      
05868                                                                   
05869      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
05870                                                                   
05871      IF NST-E-BAL NEGATIVE                                        
05872          IF NST-E-BAL LESS VARY-LO                                
05873              GO TO 3850-PRT-WRT-OFF.                              
05874                                                                   
05875      IF NST-E-BAL POSITIVE                                        
05876          IF NST-E-BAL GREATER VARY-HI                             
05877              GO TO 3850-PRT-WRT-OFF.                              
05878                                                                   
05879      COMPUTE NST-W-OFF = NST-E-BAL  *  -1.                        
05880                                                                   
05881      ADD NST-W-OFF               TO  NST-E-BAL.                   
05882                                                                   
05883  3850-PRT-WRT-OFF.                                                
05884      MOVE 'WRITE OFF ADJUSTMENT'  TO  T-DESC.                     
05885      MOVE NST-W-OFF               TO  T-AMT-P.                    
05886      MOVE 'I'                     TO  T-EXPL.                     
05887      MOVE SPACE-2                 TO  P-CCSW.                     
05888                                                                   
05889      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
05890                                                                   
05891  3860-PRT-TOTAL.                                                  
05892      MOVE ALL '-'                TO  T-AMTR-P.                    
05893      MOVE SPACE-2                TO  P-CCSW.                      
05894                                                                   
05895      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
05896                                                                   
05897      MOVE NST-E-BAL              TO  T-AMT-P.                     
05898                                                                   
05899      IF DTE-CLIENT = 'KSM'                                        
05900          MOVE 'TOTAL DUE     '   TO  T-DESC                       
05901      ELSE                                                         
05902         IF DTE-CLIENT = 'FLC'                                     
05903              MOVE 'BALANCE       '                                
05904                                  TO  T-DESC                       
05905         ELSE                                                      
05906             IF NST-E-BAL LESS +1.00 AND GREATER -1.00             
05907                 MOVE SPACES      TO  T-DESC                       
05908             ELSE                                                  
05909                 IF NST-E-BAL LESS ZERO                            
05910                     MOVE 'WE WILL REFUND'                         
05911                                  TO  T-DESC                       
05912                 ELSE                                              
05913                     MOVE 'PLEASE REMIT  '                         
05914                                  TO  T-DESC.                      
05915                                                                   
05916                                                                   
05917      MOVE 'J=E+F-G+H+I'          TO  T-EXPL.                      
05918      MOVE SPACE-2                TO  P-CCSW.                      
05919                                                                   
05920      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
05921                                                                   
05922      IF DTE-CLIENT = 'FLC'                                        
05923          GO TO 3870-ROLL-TOTALS.                                  
05924                                                                   
05925      IF DTE-CLIENT NOT = 'KSM'                                    
05926          IF NST-E-BAL GREATER +1.00                               
05927              MOVE '  PLEASE RETURN ONE COPY WITH REMITTANCE '     
05928                                  TO  P-LINE                       
05929              MOVE SPACE-2        TO  P-CCSW                       
05930              PERFORM 8800-PRT-RTN  THRU  8899-EXIT                
05931              GO TO 3870-ROLL-TOTALS                               
05932          ELSE                                                     
05933              GO TO 3870-ROLL-TOTALS.                              
05934                                                                   
05935      MOVE KSM-SUMMARY-DESC-1     TO  P-LINE-9-DESC.               
05936      MOVE SPACE-2                TO  P-CCSW.                      
05937      PERFORM  8800-PRT-RTN  THRU  8899-EXIT.                      
05938      MOVE KSM-SUMMARY-DESC-2     TO  P-LINE-9-DESC.               
05939      MOVE SPACE-2                TO  P-CCSW.                      
05940      PERFORM  8800-PRT-RTN  THRU  8899-EXIT.                      
05941      MOVE KSM-SUMMARY-DESC-3     TO  P-LINE-9-DESC.               
05942      PERFORM  8800-PRT-RTN  THRU  8899-EXIT.                      
05943      MOVE KSM-SUMMARY-DESC-4     TO  P-LINE-9-DESC.               
05944      PERFORM  8800-PRT-RTN  THRU  8899-EXIT.                      
05945      MOVE KSM-SUMMARY-DESC-5     TO  P-LINE-9-PAYABLE.            
05946      MOVE SAVE-COMPANY-NAME (3)  TO  P-LINE-9-NAME.               
05947      MOVE SPACE-2                TO  P-CCSW.                      
05948      PERFORM  8800-PRT-RTN  THRU  8899-EXIT.                      
05949                                                                   
05950      IF DTE-CLIENT = 'KSM'                                        
05951          PERFORM 6000-WRITE-BILLING-SUM  THRU  6099-EXIT.         
05952                                                                   
05953  3870-ROLL-TOTALS.                                                


           EVALUATE TRUE
              WHEN (SEC-PAY-CARRIER (CLAS-INDEXCN))
                 AND (SAV-SPPDD = 'Y')
      *          AND (PRV-SPP-CATEGORY = 'D')
                 MOVE +3               TO FT-SUB
              WHEN SEC-PAY-CARRIER (CLAS-INDEXCN)
                 MOVE +2               TO FT-SUB
              WHEN OTHER
                 MOVE +1               TO FT-SUB
           END-EVALUATE

      *    IF SEC-PAY-CARRIER (CLAS-INDEXCN)
      *       MOVE +2                  TO FT-SUB
      *    ELSE
      *       MOVE +1                  TO FT-SUB
      *    END-IF


05954      ADD NST-L-ISS               TO  NFT-L-ISS  (FT-SUB).
05955      ADD NST-A-ISS               TO  NFT-A-ISS  (FT-SUB).
05956      ADD NST-L-CAN               TO  NFT-L-CAN  (FT-SUB).
05957      ADD NST-A-CAN               TO  NFT-A-CAN  (FT-SUB).
05958      ADD NST-DTH-CLM             TO  NFT-DTH-CLM  (FT-SUB).
05959      ADD NST-DIS-CLM             TO  NFT-DIS-CLM  (FT-SUB).
05960      ADD NST-L-PRM               TO  NFT-L-PRM  (FT-SUB).
05961      ADD NST-A-PRM               TO  NFT-A-PRM  (FT-SUB).
05962      ADD NST-T-PRM               TO  NFT-T-PRM  (FT-SUB).
05963      ADD NST-L-COM               TO  NFT-L-COM  (FT-SUB).
05964      ADD NST-A-COM               TO  NFT-A-COM  (FT-SUB).
05965      ADD NST-T-COM               TO  NFT-T-COM  (FT-SUB).
011904     ADD NST-L-RCM               TO  NFT-L-RCM  (FT-SUB).
011904     ADD NST-A-RCM               TO  NFT-A-RCM  (FT-SUB).
011904     ADD NST-T-RCM               TO  NFT-T-RCM  (FT-SUB).
05966      ADD NST-L-REF               TO  NFT-L-REF  (FT-SUB).
05967      ADD NST-A-REF               TO  NFT-A-REF  (FT-SUB).
05968      ADD NST-T-REF               TO  NFT-T-REF  (FT-SUB).
05969      ADD NST-L-NET               TO  NFT-L-NET  (FT-SUB).
05970      ADD NST-A-NET               TO  NFT-A-NET  (FT-SUB).
05971      ADD NST-T-NET               TO  NFT-T-NET  (FT-SUB).
05972      ADD NST-T-FAC               TO  NFT-T-FAC  (FT-SUB).
05973      ADD NST-R-PMT               TO  NFT-R-PMT  (FT-SUB).
05974      ADD NST-C-CHG               TO  NFT-C-CHG  (FT-SUB).
05975      ADD NST-L-CLM               TO  NFT-L-CLM  (FT-SUB).
05976      ADD NST-A-CLM               TO  NFT-A-CLM  (FT-SUB).
05977      ADD NST-T-CLM               TO  NFT-T-CLM  (FT-SUB).
05978      ADD NST-B-BAL               TO  NFT-B-BAL  (FT-SUB).
05979      ADD NST-W-OFF               TO  NFT-W-OFF  (FT-SUB).
05980      ADD NPS-L-PRM               TO  NPF-L-PRM  (FT-SUB).
05981      ADD NPS-A-PRM               TO  NPF-A-PRM  (FT-SUB).
05982      ADD NPS-T-PRM               TO  NPF-T-PRM  (FT-SUB).
05983      ADD NPS-L-REF               TO  NPF-L-REF  (FT-SUB).
05984      ADD NPS-A-REF               TO  NPF-A-REF  (FT-SUB).
05985      ADD NPS-T-REF               TO  NPF-T-REF  (FT-SUB).
05986      ADD NPS-L-NET               TO  NPF-L-NET  (FT-SUB).
05987      ADD NPS-A-NET               TO  NPF-A-NET  (FT-SUB).
05988      ADD NPS-T-NET               TO  NPF-T-NET  (FT-SUB).
05989      ADD NPS-L-COM               TO  NPF-L-COM  (FT-SUB).
05990      ADD NPS-A-COM               TO  NPF-A-COM  (FT-SUB).
05991      ADD NPS-T-COM               TO  NPF-T-COM  (FT-SUB).
011904     ADD NPS-L-RCM               TO  NPF-L-RCM  (FT-SUB).
011904     ADD NPS-A-RCM               TO  NPF-A-RCM  (FT-SUB).
011904     ADD NPS-T-RCM               TO  NPF-T-RCM  (FT-SUB).
05992      ADD NST-E-BAL               TO  NFT-E-BAL  (FT-SUB).
05993      ADD TOTS-L-PRM              TO  TOTF-L-PRM  (FT-SUB).
05994      ADD TOTS-A-PRM              TO  TOTF-A-PRM  (FT-SUB).
05995      ADD TOTS-T-PRM              TO  TOTF-T-PRM  (FT-SUB).
05996      ADD TOTS-L-REF              TO  TOTF-L-REF  (FT-SUB).
05997      ADD TOTS-A-REF              TO  TOTF-A-REF  (FT-SUB).
05998      ADD TOTS-T-REF              TO  TOTF-T-REF  (FT-SUB).
05999      ADD TOTS-L-NET              TO  TOTF-L-NET  (FT-SUB).
06000      ADD TOTS-A-NET              TO  TOTF-A-NET  (FT-SUB).
06001      ADD TOTS-T-NET              TO  TOTF-T-NET  (FT-SUB).
06002      ADD TOTS-L-COM              TO  TOTF-L-COM  (FT-SUB).
06003      ADD TOTS-A-COM              TO  TOTF-A-COM  (FT-SUB).
06004      ADD TOTS-T-COM              TO  TOTF-T-COM  (FT-SUB).
011904     ADD TOTS-L-RCM              TO  TOTF-L-RCM  (FT-SUB).
011904     ADD TOTS-A-RCM              TO  TOTF-A-RCM  (FT-SUB).
011904     ADD TOTS-T-RCM              TO  TOTF-T-RCM  (FT-SUB).

06006      MOVE ZEROS                  TO  NST-L-ISS    NST-A-ISS       
06007                                      NST-L-CAN    NST-A-CAN       
06008                                      NST-DTH-CLM  NST-DIS-CLM     
06009                                      NST-L-PRM    NST-A-PRM       
06010                                      NST-T-PRM                    
06011                                      NST-L-COM    NST-A-COM       
06012                                      NST-T-COM                    
06013                                      NST-L-REF    NST-A-REF       
06014                                      NST-T-REF                    
06015                                      NST-L-NET    NST-A-NET       
06016                                      NST-T-NET                    
06017                                      NST-L-RCM    NST-A-RCM       
06018                                      NST-T-RCM                    
06019                                      NST-T-FAC    NST-R-PMT       
06020                                      NST-C-CHG    NST-L-CLM       
06021                                      NST-A-CLM    NST-T-CLM       
06022                                      NST-B-BAL    NST-W-OFF       
06023                                      NST-E-BAL                    
06024                                      NPS-L-PRM    NPS-A-PRM       
06025                                      NPS-T-PRM                    
06026                                      NPS-L-REF    NPS-A-REF       
06027                                      NPS-T-REF                    
06028                                      NPS-L-NET    NPS-A-NET       
06029                                      NPS-T-NET                    
06030                                      NPS-L-COM    NPS-A-COM       
06031                                      NPS-T-COM    NPS-L-RCM
011904                                     NPS-A-RCM    NPS-T-RCM.

06006      MOVE ZEROS                  TO  CNST-L-ISS    CNST-A-ISS       
06007                                      CNST-L-CAN    CNST-A-CAN       
06009                                      CNST-L-PRM    CNST-A-PRM       
06010                                      CNST-T-PRM                    
06011                                      CNST-L-COM    CNST-A-COM       
06012                                      CNST-T-COM                    
06013                                      CNST-L-REF    CNST-A-REF       
06014                                      CNST-T-REF                    
06017                                      CNST-L-RCM    CNST-A-RCM       
06018                                      CNST-T-RCM                    
06019                                      CNST-T-FAC
06032                                                                   
           .
06033  3899-EXIT.                                                       
06034      EXIT.                                                        
06035  EJECT                                                            
06036  3900-PRT-STMT-HD-RTN.                                            
06037      MOVE 'Y'                    TO  HEADING-SWITCH.              
06038                                                                   
06039      IF PGCTR = +0                                                
06040          GO TO 3910-PRT-FIRST-PAGE.                               
06041                                                                   
06042      MOVE SPACE-3                             TO  P-CCSW.         
06043      MOVE '          CONTINUED ON NEXT PAGE'  TO  P-LINE.         
06044                                                                   
06045      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06046                                                                   
06047  3910-PRT-FIRST-PAGE.                                             
06048      MOVE SAVE-COMPANY-NAME (3)  TO  HD-CO
100307     MOVE SAV-CSR                TO  HD1-CSR
06049      MOVE HD1                    TO  P-LINE.                      
06050      MOVE SPACE-NP               TO  P-CCSW.                      
06051                                                                   
06052      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06053                                                                   
06054      ADD +1                      TO  PGCTR
091306                                     TPGCTR
06055      MOVE PGCTR                  TO  HD-PG.                       
06056      MOVE HD2                    TO  P-LINE.                      
06057      MOVE SPACE-1                TO  P-CCSW.                      
06058                                                                   
06059      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06060                                                                   
06061      MOVE HD3                    TO  P-LINE.                      
06062      MOVE SPACE-1                TO  P-CCSW.                      
06063                                                                   
06064      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06065                                                                   
06066      IF RECAP-PAGE                                                
06067          GO TO 3960-PRT-HD6.                                      
06068                                                                   
06069      IF KSM-RECAP-PAGE                                            
06070          GO TO 3999-EXIT.                                         
06071                                                                   
06072  3920-PRT-SUB-HEADS.                                              
CIDMOD                                                                  
CIDMOD*    MOVE 'ACCOUNT NO. - '       TO  HD-M1.                       
CIDMOD*    MOVE SAV-CARR-GROUP         TO  HD-CARR-GROUP.               
CIDMOD*    MOVE '-'                    TO  HD-C-A.                      
CIDMOD*    MOVE BIL0                   TO  HD-ACCT.                     
CIDMOD*    MOVE BIL1                   TO  HD-NAME.                     
CIDMOD*    MOVE 'REMIT TO - '          TO  HD-M2.                       
CIDMOD*    MOVE RMT0                   TO  HD-M3.                       
CIDMOD*    MOVE RMT1                   TO  HD-REMIT.                    
CIDMOD*    MOVE HD4                    TO  P-LINE.                      
CIDMOD*                                                                 
CIDMOD*    PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
CIDMOD*                                                                 
CIDMOD*    IF WS-SUMMARY                                                
CIDMOD*        GO TO 3925-CONT-PRT-SUB-HEADS.                           
CIDMOD*                                                                 
CIDMOD*    IF PGCTR > +1                                          
CIDMOD*        GO TO 3950-PRT-HD5.                                      


06073      MOVE 'ACCOUNT NO. - '       TO  HD-M1.                       
06074      MOVE SAV-CARR-GROUP         TO  HD-CARR-GROUP.               
06075      MOVE '-'                    TO  HD-C-A.                      
06076      MOVE BIL0                   TO  HD-ACCT.                     
CIDMOD*    MOVE BIL1                   TO  HD-NAME.                     
06078      MOVE 'REMIT TO - '          TO  HD-M2.                       
06079      MOVE RMT0                   TO  HD-M3.                       
06080      MOVE RMT1                   TO  HD-REMIT.                    
CIDMOD     IF (PGCTR > +1) AND
CIDMOD        (NOT WS-SUMMARY)
              MOVE BIL1                TO HD-NAME
              MOVE HD4                 TO P-LINE
              PERFORM 8800-PRT-RTN     THRU 8899-EXIT
              GO TO 3950-PRT-HD5
           END-IF

06090      .                                                            
06091  3925-CONT-PRT-SUB-HEADS.                                         
CIDMOD
           IF WS-CONTROL-NAME NOT = SPACES
              MOVE WS-CONTROL-NAME     TO HD-NAME
           ELSE
              MOVE BIL1                TO HD-NAME
           END-IF

CIDMOD     MOVE HD4                 TO P-LINE
           PERFORM 8800-PRT-RTN     THRU 8899-EXIT
CIDMOD
06092      MOVE SPACES                 TO  HD-M1                        
06093                                      HD-CARR-GROUP                
06094                                      HD-C-A                       
06095                                      HD-ACCT                      
06096                                      HD-M2                        
06097                                      HD-M3.                       
06098                                                                   
CIDMOD     IF WS-CONTROL-NAME NOT = SPACES
CIDMOD        MOVE BIL1                TO  HD-NAME
CIDMOD        MOVE RMT2                TO  HD-REMIT
CIDMOD        MOVE SPACE-1             TO  P-CCSW
CIDMOD        MOVE HD4                 TO  P-LINE
CIDMOD        PERFORM 8800-PRT-RTN     THRU 8899-EXIT
CIDMOD        MOVE BIL2                TO  HD-NAME
CIDMOD        MOVE RMT3                TO  HD-REMIT
CIDMOD        MOVE SPACE-1             TO  P-CCSW
CIDMOD        MOVE HD4                 TO  P-LINE
CIDMOD        PERFORM 8800-PRT-RTN  THRU  8899-EXIT
CIDMOD        MOVE SPACES              TO  HD-M1
CIDMOD                                     HD-CARR-GROUP
CIDMOD                                     HD-C-A
CIDMOD                                     HD-ACCT
CIDMOD                                     HD-M2
CIDMOD                                     HD-M3
CIDMOD        MOVE BIL3                TO  HD-NAME
CIDMOD        MOVE RMT4                TO  HD-REMIT
CIDMOD        MOVE SPACE-1             TO  P-CCSW
CIDMOD        MOVE HD4                 TO  P-LINE
CIDMOD        PERFORM 8800-PRT-RTN     THRU  8899-EXIT
CIDMOD        MOVE SPACES              TO  HD-M1
CIDMOD                                     HD-CARR-GROUP
CIDMOD                                     HD-C-A
CIDMOD                                     HD-ACCT
CIDMOD                                     HD-M2
CIDMOD                                     HD-M3
CIDMOD        MOVE BIL4                TO  HD-NAME
CIDMOD        MOVE RMT5                TO  HD-REMIT
CIDMOD        MOVE SPACE-1             TO  P-CCSW
CIDMOD        MOVE HD4                 TO  P-LINE
CIDMOD        PERFORM 8800-PRT-RTN     THRU  8899-EXIT
CIDMOD        IF BIL5 = SPACES  AND
CIDMOD           RMT6 = SPACES
CIDMOD           CONTINUE
CIDMOD        ELSE
CIDMOD           MOVE SPACES           TO  HD-M1
CIDMOD                                     HD-CARR-GROUP
CIDMOD                                     HD-C-A
CIDMOD                                     HD-ACCT
CIDMOD                                     HD-M2
CIDMOD                                     HD-M3
CIDMOD           MOVE BIL5             TO  HD-NAME
CIDMOD           MOVE RMT6             TO  HD-REMIT
CIDMOD           MOVE SPACE-1          TO  P-CCSW
CIDMOD           MOVE HD4              TO  P-LINE
CIDMOD           PERFORM 8800-PRT-RTN  THRU  8899-EXIT
CIDMOD        END-IF
CIDMOD        IF BIL6 = SPACES
CIDMOD           CONTINUE
CIDMOD        ELSE
CIDMOD           MOVE SPACES           TO  HD-M1
CIDMOD                                     HD-CARR-GROUP
CIDMOD                                     HD-C-A
CIDMOD                                     HD-ACCT
CIDMOD                                     HD-M2
CIDMOD                                     HD-M3
CIDMOD           MOVE BIL6             TO  HD-NAME
CIDMOD           MOVE SPACES           TO  HD-REMIT
CIDMOD           MOVE SPACE-1          TO  P-CCSW
CIDMOD           MOVE HD4              TO  P-LINE
CIDMOD           PERFORM 8800-PRT-RTN  THRU  8899-EXIT
CIDMOD        END-IF
CIDMOD     ELSE
CIDMOD        MOVE BIL2                TO  HD-NAME
CIDMOD        MOVE RMT2                TO  HD-REMIT
CIDMOD        MOVE SPACE-1             TO  P-CCSW
CIDMOD        MOVE HD4                 TO  P-LINE
CIDMOD
CIDMOD        PERFORM 8800-PRT-RTN     THRU  8899-EXIT
CIDMOD
CIDMOD        MOVE SPACES              TO  HD-M1
CIDMOD                                     HD-CARR-GROUP
CIDMOD                                     HD-C-A
CIDMOD                                     HD-ACCT
CIDMOD                                     HD-M2
CIDMOD                                     HD-M3
CIDMOD        MOVE BIL3                TO  HD-NAME
CIDMOD        MOVE RMT3                TO  HD-REMIT
CIDMOD        MOVE SPACE-1             TO  P-CCSW
CIDMOD        MOVE HD4                 TO  P-LINE
CIDMOD
CIDMOD        PERFORM 8800-PRT-RTN     THRU  8899-EXIT
CIDMOD
CIDMOD        MOVE SPACES              TO  HD-M1
CIDMOD                                     HD-CARR-GROUP
CIDMOD                                     HD-C-A
CIDMOD                                     HD-ACCT
CIDMOD                                     HD-M2
CIDMOD                                     HD-M3
CIDMOD        MOVE BIL4                TO  HD-NAME
CIDMOD        MOVE RMT4                TO  HD-REMIT
CIDMOD        MOVE SPACE-1             TO  P-CCSW
CIDMOD        MOVE HD4                 TO  P-LINE
CIDMOD        PERFORM 8800-PRT-RTN     THRU  8899-EXIT
CIDMOD        IF BIL5 = SPACES  AND
CIDMOD           RMT5 = SPACES
CIDMOD           CONTINUE
CIDMOD        ELSE
CIDMOD           MOVE SPACES           TO  HD-M1
CIDMOD                                     HD-CARR-GROUP
CIDMOD                                     HD-C-A
CIDMOD                                     HD-ACCT
CIDMOD                                     HD-M2
CIDMOD                                     HD-M3
CIDMOD           MOVE BIL5             TO  HD-NAME
CIDMOD           MOVE RMT5             TO  HD-REMIT
CIDMOD           MOVE SPACE-1          TO  P-CCSW
CIDMOD           MOVE HD4              TO  P-LINE
CIDMOD           PERFORM 8800-PRT-RTN  THRU  8899-EXIT
CIDMOD        END-IF
CIDMOD        IF BIL6 = SPACES  AND
CIDMOD           RMT6 = SPACES
CIDMOD           CONTINUE
CIDMOD        ELSE
CIDMOD           MOVE SPACES           TO  HD-M1
CIDMOD                                     HD-CARR-GROUP
CIDMOD                                     HD-C-A
CIDMOD                                     HD-ACCT
CIDMOD                                     HD-M2
CIDMOD                                     HD-M3
CIDMOD           MOVE BIL6             TO  HD-NAME
CIDMOD           MOVE RMT6             TO  HD-REMIT
CIDMOD           MOVE SPACE-1          TO  P-CCSW
CIDMOD           MOVE HD4              TO  P-LINE
CIDMOD
CIDMOD           PERFORM 8800-PRT-RTN  THRU  8899-EXIT
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD     .
06168  3950-PRT-HD5.                                                    
06169      MOVE HDR-DASH-LINE          TO  P-LINE.                      
06170      MOVE SPACE-2                TO  P-CCSW.                      
06171                                                                   
06172      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06173                                                                   
06174      MOVE HD5                    TO  P-LINE.                      
06175      MOVE SPACE-1                TO  P-CCSW.                      
06176                                                                   
06177      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06178                                                                   
06179      MOVE HDR-DASH-LINE          TO  P-LINE.                      
06180      MOVE SPACE-1                TO  P-CCSW.                      
06181                                                                   
06182      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06183  EJECT                                                            
06184  3960-PRT-HD6.                                                    
06185                                                                   
06186      MOVE HD6                    TO  P-LINE.                      
06187      MOVE SPACE-2                TO  P-CCSW.                      
06188                                                                   
06189      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06190                                                                   
06191      IF PYADJ-HEADING OR SUMMARY-HEADING OR RECAP-PAGE            
06192          GO TO 3999-EXIT.                                         
06193                                                                   
06194      MOVE HD7                    TO  P-LINE.                      
06195      MOVE SPACE-1                TO  P-CCSW.                      
06196                                                                   
06197      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06198                                                                   
06199  3999-EXIT.                                                       
06200      EXIT.                                                        
06201  EJECT                                                            
06202  4000-CALC-DETAIL-RTN.                                            
06203      MOVE ZEROS                  TO  NCT-L-ISS    NCT-A-ISS       
06204                                      NCT-L-CAN    NCT-A-CAN       
06205                                      NCT-DTH-CLM  NCT-DIS-CLM     
06206                                      NCT-T-FAC    NCT-T-FAC-ALT   
06207                                      NCT-L-PRM    NCT-L-PRM-ALT   
06208                                      NCT-L-COM    NCT-L-COM-ALT   
06209                                      NCT-A-PRM    NCT-T-PRM       
06210                                      NCT-A-COM    NCT-T-COM       
06211                                      NCT-L-REF    NCT-A-REF       
06212                                      NCT-T-REF    NCT-L-RCM       
06213                                      NCT-A-RCM    NCT-T-RCM       
06214                                      NCT-R-PMT    NCT-C-CHG       
06215                                      NCT-L-CLM    NCT-A-CLM       
06216                                      NCT-T-CLM.                   
06217                                                                   
052504     IF WS-ISSUE OR WS-CASH-ISSUE                               
06219          GO TO 4010-CALC-ISSUE.                                   
06220                                                                   
06221      IF WS-CANCEL OR WS-CASH-CANCEL                               
06222          GO TO 4020-CALC-CANCEL.                                  
06223                                                                   
06224      IF WS-PYMTS-ADJUST                                           
06225          GO TO 4030-CALC-ACCTG.                                   
06226                                                                   
06227      IF WS-CLAIMS                                                 
06228          GO TO 4040-CALC-CLAIM.                                   
06229                                                                   
06230      GO TO 4099-EXIT.                                             
06231                                                                   
06232  4010-CALC-ISSUE.                                                 
06233      MOVE WS-SRT02-FACE          TO  NCT-T-FAC.                   
06234      MOVE WS-SRT02-FACE-ALT      TO  NCT-T-FAC-ALT.               
06235                                                                   
06236      IF WS-SRT02-LF-PREM NOT = ZERO                               
06237           ADD +1                 TO  NCT-L-ISS.                   
06238                                                                   
06239      IF WS-SRT02-LF-PRM-PR NOT = ZERO                             
06240          MOVE WS-SRT02-LF-PRM-PR  TO  NCT-L-PRM                   
06241      ELSE                                                         
06242          IF WS-SRT02-PREM-OVERRIDE = 'B' OR 'L'                   
06243              MOVE WS-SRT02-LF-PRM-PR                              
06244                                   TO  NCT-L-PRM                   
06245          ELSE                                                     
06246              MOVE WS-SRT02-LF-PREM                                
06247                                   TO  NCT-L-PRM.                  
06248                                                                   
06249      IF WS-SRT02-LF-PRM-PR-ALT NOT = ZERO                         
06250          MOVE WS-SRT02-LF-PRM-PR-ALT  TO  NCT-L-PRM-ALT           
06251      ELSE                                                         
06252          IF WS-SRT02-PREM-OVERRIDE = 'B' OR 'L'                   
06253              MOVE WS-SRT02-LF-PRM-PR-ALT                          
06254                                       TO  NCT-L-PRM-ALT           
06255          ELSE                                                     
06256              MOVE WS-SRT02-LF-PREM-ALT                            
06257                                       TO  NCT-L-PRM-ALT.          
06258                                                                   
06259      IF WS-SRT02-AH-PREM NOT = ZERO                               
06260           ADD +1                 TO  NCT-A-ISS.                   
06261                                                                   
06262      IF (WS-SRT02-AH-PRM-PR NOT = ZERO )                            
092705        AND (WS-SRT02-AH-BEN-CAT NOT = 'G' AND 'L')
06263          MOVE WS-SRT02-AH-PRM-PR  TO  NCT-A-PRM                   
06264      ELSE                                                         
06265          IF WS-SRT02-PREM-OVERRIDE = 'B' OR 'A'                   
06266              MOVE WS-SRT02-AH-PRM-PR                              
06267                                   TO  NCT-A-PRM                   
06268          ELSE                                                     
06269              MOVE WS-SRT02-AH-PREM                                
06270                                   TO  NCT-A-PRM.                  
06271                                                                   
06272      COMPUTE NCT-T-PRM  =  NCT-L-PRM  +  NCT-L-PRM-ALT            
06273                         +  NCT-A-PRM.                             
06274                                                                   
06275      MOVE WS-SRT02-LF-COMM       TO  NCT-L-COM.                   
06276      MOVE WS-SRT02-LF-COMM-ALT   TO  NCT-L-COM-ALT.               
06277      MOVE WS-SRT02-AH-COMM       TO  NCT-A-COM.                   
06278                                                                   
06279      COMPUTE NCT-T-COM  =  NCT-L-COM  +  NCT-L-COM-ALT            
06280                         +  NCT-A-COM.
06281                                                                   
06282      GO TO 4050-ROLL-DETAIL.                                      
06283                                                                   
06284  4020-CALC-CANCEL.                                                
06285      IF WS-SRT03-LF-REF NOT = ZERO                                
06286          ADD +1                  TO  NCT-L-CAN.                   
06287                                                                   
06288      IF WS-SRT03-LF-REF-REV NOT = ZERO                            
06289          MOVE WS-SRT03-LF-REF-REV  TO  NCT-L-REF                  
06290      ELSE                                                         
06291          IF WS-SRT03-REF-OVERRIDE = 'B' OR 'L'                    
06292              MOVE WS-SRT03-LF-REF-REV                             
06293                                    TO  NCT-L-REF                  
06294          ELSE                                                     
06295              MOVE WS-SRT03-LF-REF  TO  NCT-L-REF.                 
06296                                                                   
06297      IF WS-SRT03-AH-REF NOT = ZERO                                
06298          ADD +1                  TO  NCT-A-CAN.                   
06299                                                                   
06300      IF WS-SRT03-AH-REF-REV NOT = ZERO                            
06301          MOVE WS-SRT03-AH-REF-REV  TO  NCT-A-REF                  
06302      ELSE                                                         
06303          IF WS-SRT03-REF-OVERRIDE = 'B' OR 'A'                    
06304              MOVE WS-SRT03-AH-REF-REV                             
06305                                    TO  NCT-A-REF                  
06306          ELSE                                                     
06307              MOVE WS-SRT03-AH-REF  TO  NCT-A-REF.                 
06308                                                                   
06309      COMPUTE NCT-T-REF  =  NCT-L-REF  +  NCT-A-REF.               
06310                                                                   
06311      MOVE WS-SRT03-LF-COM-REF      TO  NCT-L-RCM.                 
06312      MOVE WS-SRT03-AH-COM-REF      TO  NCT-A-RCM.                 
06313                                                                   
06314      COMPUTE NCT-T-RCM  =  NCT-L-RCM  +  NCT-A-RCM.               
06315                                                                   
06316      GO TO 4050-ROLL-DETAIL.                                      
06317                                                                   
06318  4030-CALC-ACCTG.                                                 
06319      MOVE WS-SRT10-PYMT          TO  NCT-R-PMT.                   
06320      MOVE WS-SRT10-CHRG          TO  NCT-C-CHG.                   
06321                                                                   
06322      GO TO 4050-ROLL-DETAIL.                                      
06323                                                                   
06324  4040-CALC-CLAIM.                                                 
06325      MOVE WS-SRT12-LF-CLM        TO  NCT-L-CLM.                   
06326      MOVE WS-SRT12-AH-CLM        TO  NCT-A-CLM.                   
06327                                                                   
06328      COMPUTE NCT-T-CLM  =  NCT-L-CLM  +  NCT-A-CLM.               
06329                                                                   
06330      IF WS-SRT12-LF-CLM = ZERO                                    
06331          ADD +1                  TO  NCT-DIS-CLM                  
06332      ELSE                                                         
06333          ADD +1                  TO  NCT-DTH-CLM.                 
06334                                                                   
06335  4050-ROLL-DETAIL.                                                

020609     IF (WS-ISSUE)
020609        AND (WS-SRT02-OB-ON = 'M')
122002        GO TO 4099-EXIT 
020609     END-IF

020609     IF (WS-CANCEL)
020609        AND (WS-SRT03-OB-ON = 'M')
020609        GO TO 4099-EXIT 
020609     END-IF

           IF (WS-CASH-ISSUE)
              OR (WS-CASH-CANCEL)
06336      ADD NCT-L-ISS               TO  CNST-L-ISS
06337      ADD NCT-A-ISS               TO  CNST-A-ISS
06338      ADD NCT-L-CAN               TO  CNST-L-CAN
06339      ADD NCT-A-CAN               TO  CNST-A-CAN
06342      ADD NCT-L-PRM               TO  CNST-L-PRM
06343      ADD NCT-L-PRM-ALT           TO  CNST-L-PRM
06344      ADD NCT-A-PRM               TO  CNST-A-PRM
06345      ADD NCT-T-PRM               TO  CNST-T-PRM
06346      ADD NCT-L-COM               TO  CNST-L-COM
06347      ADD NCT-L-COM-ALT           TO  CNST-L-COM
06348      ADD NCT-A-COM               TO  CNST-A-COM
06349      ADD NCT-T-COM               TO  CNST-T-COM
06350      ADD NCT-L-REF               TO  CNST-L-REF
06351      ADD NCT-A-REF               TO  CNST-A-REF
06352      ADD NCT-T-REF               TO  CNST-T-REF
011904*    SUBTRACT NCT-L-RCM        FROM  CNST-L-COM
011904*    SUBTRACT NCT-A-RCM        FROM  CNST-A-COM
011904*    SUBTRACT NCT-T-RCM        FROM  CNST-T-COM
06356      ADD NCT-L-RCM               TO  CNST-L-RCM
06357      ADD NCT-A-RCM               TO  CNST-A-RCM
06358      ADD NCT-T-RCM               TO  CNST-T-RCM
06359      ADD NCT-T-FAC               TO  CNST-T-FAC
06360      ADD NCT-T-FAC-ALT           TO  CNST-T-FAC


           ELSE


06336      ADD NCT-L-ISS               TO  NST-L-ISS
06337      ADD NCT-A-ISS               TO  NST-A-ISS
06338      ADD NCT-L-CAN               TO  NST-L-CAN
06339      ADD NCT-A-CAN               TO  NST-A-CAN
06340      ADD NCT-DTH-CLM             TO  NST-DTH-CLM
06341      ADD NCT-DIS-CLM             TO  NST-DIS-CLM
06342      ADD NCT-L-PRM               TO  NST-L-PRM
06343      ADD NCT-L-PRM-ALT           TO  NST-L-PRM
06344      ADD NCT-A-PRM               TO  NST-A-PRM
06345      ADD NCT-T-PRM               TO  NST-T-PRM
06346      ADD NCT-L-COM               TO  NST-L-COM
06347      ADD NCT-L-COM-ALT           TO  NST-L-COM
06348      ADD NCT-A-COM               TO  NST-A-COM
06349      ADD NCT-T-COM               TO  NST-T-COM
06350      ADD NCT-L-REF               TO  NST-L-REF
06351      ADD NCT-A-REF               TO  NST-A-REF
06352      ADD NCT-T-REF               TO  NST-T-REF
011904*    SUBTRACT NCT-L-RCM        FROM  NST-L-COM
011904*    SUBTRACT NCT-A-RCM        FROM  NST-A-COM
011904*    SUBTRACT NCT-T-RCM        FROM  NST-T-COM
06356      ADD NCT-L-RCM               TO  NST-L-RCM
06357      ADD NCT-A-RCM               TO  NST-A-RCM
06358      ADD NCT-T-RCM               TO  NST-T-RCM
06359      ADD NCT-T-FAC               TO  NST-T-FAC
06360      ADD NCT-T-FAC-ALT           TO  NST-T-FAC
06361      ADD NCT-R-PMT               TO  NST-R-PMT
06362      ADD NCT-C-CHG               TO  NST-C-CHG
06363      ADD NCT-L-CLM               TO  NST-L-CLM
06364      ADD NCT-A-CLM               TO  NST-A-CLM
06365      ADD NCT-T-CLM               TO  NST-T-CLM




           END-IF

06367      IF DTE-CLIENT NOT = 'KSM'                                    
06368          GO TO 4099-EXIT.                                         
06369                                                                   
06370      IF WS-ISSUE OR WS-CANCEL                                     
06371          NEXT SENTENCE                                            
06372      ELSE                                                         
06373          GO TO 4099-EXIT.                                         
06374                                                                   
06375      IF WS-SRT02-EFF-DT LESS THAN KSM-LO-CERT-DATE                
06376          MOVE WS-SRT02-EFF-DT    TO  KSM-LO-CERT-DATE.            
06377                                                                   
06378      IF WS-SRT02-EFF-DT GREATER THAN KSM-HI-CERT-DATE             
06379          MOVE WS-SRT02-EFF-DT    TO  KSM-HI-CERT-DATE.            
06380                                                                   
06381      IF WS-ISSUE                                                  
091911         IF WS-SRT02-LF-TYP NOT = SPACES AND ZEROS AND 'DD'
06383              ADD +1 TO KSM-ISS-LF-CNT                             
06384              IF WS-SRT02-AH-TYP NOT = SPACES AND ZEROS            
06385                  ADD +1 TO KSM-ISS-AH-CNT                         
06386              ELSE                                                 
06387                  NEXT SENTENCE                                    
06388          ELSE                                                     
06389              IF WS-SRT02-AH-TYP NOT = SPACES AND ZEROS            
06390                  ADD +1 TO KSM-ISS-AH-CNT.                        
06391                                                                   
06392      IF WS-CANCEL                                                 
091911         IF WS-SRT03-LF-TYP NOT = SPACES AND '00' AND 'DD'
06394              ADD +1 TO KSM-CAN-LF-CNT                             
06395              IF WS-SRT03-AH-TYP NOT = SPACES                      
06396                  ADD +1 TO KSM-CAN-AH-CNT                         
06397              ELSE                                                 
06398                  NEXT SENTENCE                                    
06399          ELSE                                                     
06400              IF WS-SRT03-AH-TYP NOT = SPACES                      
06401                  ADD +1 TO KSM-CAN-AH-CNT.                        
06402                                                                   
06403      IF WS-SRT02-EFF-DT-R LESS THAN '19920101'                    
06404          IF WS-ISSUE                                              
06405              ADD WS-SRT02-LF-PREM TO KSM-CAN-PREM-91              
06406              ADD WS-SRT02-LF-PREM-ALT TO KSM-CAN-PREM-91          
06407              ADD WS-SRT02-AH-PREM TO KSM-CAN-PREM-91              
06408          ELSE                                                     
06409              SUBTRACT WS-SRT03-LF-REF FROM KSM-CAN-PREM-91        
06410              SUBTRACT WS-SRT03-AH-REF FROM KSM-CAN-PREM-91.       
06411                                                                   
092815*    MOVE WS-SRT02-ACCT-STATUS    TO  KSM-ACCT-STATUS.            
092815*    MOVE WS-SRT02-REI-TABLE      TO  KSM-REI-TABLE.              
092815*    MOVE WS-SRT02-RPTCDE         TO  KSM-REPORT-CODE.            
06415                                                                   
06416  4099-EXIT.                                                       
06417      EXIT.                                                        
06418  EJECT                                                            
06419  5000-CALC-NON-PRC.                                               
06420                                                                   
06421      IF (WS-ISSUE-VOIDED)
020204        OR (WS-SRT02-OB-ON = 'M')
06422          GO TO 5099-EXIT.                                         
06423                                                                   
06424      IF   WS-ISSUE-ERROR                                          
06425        OR WS-ISSUE-RETURN                                         
06426        OR WS-ISSUE-HOLD                                           
06427            ADD WS-SRT02-LF-PREM     TO NPS-L-PRM                  
06428            ADD WS-SRT02-LF-PREM-ALT TO NPS-L-PRM                  
06429            ADD WS-SRT02-LF-COMM     TO NPS-L-COM                  
06430            ADD WS-SRT02-LF-COMM-ALT TO NPS-L-COM                  
06431            ADD WS-SRT02-AH-PREM     TO NPS-A-PRM                  
06432            ADD WS-SRT02-AH-COMM     TO NPS-A-COM                  
06433      ELSE                                                         
06434            ADD WS-SRT03-LF-REF      TO NPS-L-REF                  
06435            ADD WS-SRT03-AH-REF      TO NPS-A-REF                  
011904*          SUBTRACT WS-SRT03-LF-COM-REF FROM NPS-L-COM            
011904*          SUBTRACT WS-SRT03-AH-COM-REF FROM NPS-A-COM.           
011904           ADD WS-SRT03-LF-COM-REF  TO NPS-L-RCM
011904           ADD WS-SRT03-AH-COM-REF  TO NPS-A-RCM
011904     END-IF
011904     .
06438                                                                   
06439  5099-EXIT.                                                       
06440      EXIT.                                                        
06441  EJECT                                                            
06442  6000-WRITE-BILLING-SUM.                                          
06443                                                                   
06444      MOVE 'S'                    TO  BSR-RECORD-TYPE.             
06445      MOVE SAV-KSM-CARR-GROUP     TO  BSR-CARR-GROUP.              
06446      MOVE SAV-KSM-ACCOUNT        TO  BSR-ACCOUNT.                 
06447      MOVE SAV-KSM-ACCT-NAME      TO  BSR-ACCT-NAME.               
06448      MOVE SAV-KSM-ADDR-1         TO  BSR-ADDR-1.                  
06449      MOVE SAV-KSM-ADDR-2         TO  BSR-ADDR-2.                  
06450      MOVE SAV-KSM-CITY-STATE     TO  BSR-CITY-STATE.              
06451      MOVE SAV-KSM-ZIP            TO  BSR-ZIP.                     
06452      MOVE SAV-KSM-DATE           TO  BSR-DATE.                    
06453                                                                   
06454      MOVE NST-L-NET              TO  BSR-NET-LIFE-PREM            
06455      MOVE NST-A-NET              TO  BSR-NET-AH-PREM              
011904*    MOVE NST-L-COM              TO  BSR-LIFE-COMM                
011904     COMPUTE BSR-LIFE-COMM = (NST-L-COM - NST-L-RCM)
011904*    MOVE NST-A-COM              TO  BSR-AH-COMM                  
011904     COMPUTE BSR-AH-COMM = NST-A-COM - NST-A-RCM
06458      MOVE NST-T-NET              TO  BSR-TOTAL-PREM               
011904*    MOVE NST-T-COM              TO  BSR-TOTAL-COMM               
011904     COMPUTE BSR-TOTAL-COMM = (NST-T-COM - NST-T-RCM)
06460      COMPUTE BSR-TOTAL-DUE = NST-T-NET -
011904        (NST-T-COM - NST-T-RCM)
06461      MOVE NST-L-PRM              TO  BSR-GROSS-LIFE-PREM          
06462      MOVE NST-A-PRM              TO  BSR-GROSS-AH-PREM            
06463      MOVE NST-L-REF              TO  BSR-GROSS-LIFE-REFUNDS       
06464      MOVE NST-A-REF              TO  BSR-GROSS-AH-REFUNDS         
06465      MOVE NST-T-PRM              TO  BSR-GROSS-WRITTEN            
06466      MOVE NST-T-REF              TO  BSR-GROSS-REFUNDED           
06467      MOVE KSM-ISS-LF-CNT         TO  BSR-ISS-LF-CNT               
06468      MOVE KSM-ISS-AH-CNT         TO  BSR-ISS-AH-CNT               
06469      MOVE KSM-CAN-LF-CNT         TO  BSR-CAN-LF-CNT               
06470      MOVE KSM-CAN-AH-CNT         TO  BSR-CAN-AH-CNT               
06471      MOVE KSM-CAN-PREM-91        TO  BSR-CAN-PREM-91              
06472      MOVE KSM-LO-CERT-DATE       TO  BSR-LO-CERT-DATE             
06473      MOVE KSM-HI-CERT-DATE       TO  BSR-HI-CERT-DATE             
06474      MOVE KSM-ACCT-STATUS        TO  BSR-ACCT-STATUS              
06475      MOVE KSM-REI-TABLE          TO  BSR-REI-TABLE.               
06476      MOVE KSM-REPORT-CODE        TO  BSR-REPORT-CODE.             
06477                                                                   
06478      IF ZERO = BSR-NET-LIFE-PREM      AND                         
06479                BSR-LIFE-COMM          AND                         
06480                BSR-NET-AH-PREM        AND                         
06481                BSR-AH-COMM            AND                         
06482                BSR-TOTAL-PREM         AND                         
06483                BSR-TOTAL-COMM         AND                         
06484                BSR-TOTAL-DUE          AND                         
06485                BSR-GROSS-LIFE-PREM    AND                         
06486                BSR-GROSS-AH-PREM      AND                         
06487                BSR-GROSS-LIFE-REFUNDS AND                         
06488                BSR-GROSS-AH-REFUNDS   AND                         
06489                BSR-GROSS-WRITTEN      AND                         
06490                BSR-GROSS-REFUNDED                                 
06491          GO TO 6010-CLEAR-BILLING-SUM-RECORD.                     
06492                                                                   
06493      IF BSR-CARRIER EQUAL '3'                                     
06494         GO TO 6010-CLEAR-BILLING-SUM-RECORD.                      
06495                                                                   
06496      COMPUTE KSM-NET-LIFE-PREM = KSM-NET-LIFE-PREM                
06497                                + BSR-NET-LIFE-PREM.               
06498      COMPUTE KSM-LIFE-COMM = KSM-LIFE-COMM + BSR-LIFE-COMM.       
06499      COMPUTE KSM-NET-AH-PREM = KSM-NET-AH-PREM + BSR-NET-AH-PREM. 
06500      COMPUTE KSM-AH-COMM = KSM-AH-COMM + BSR-AH-COMM.             
06501      COMPUTE KSM-TOTAL-PREM = KSM-TOTAL-PREM + BSR-TOTAL-PREM.    
06502      COMPUTE KSM-TOTAL-COMM = KSM-TOTAL-COMM + BSR-TOTAL-COMM.    
06503      COMPUTE KSM-TOTAL-DUE = KSM-TOTAL-DUE + BSR-TOTAL-DUE.       
06504      COMPUTE KSM-GROSS-LIFE-PREM = KSM-GROSS-LIFE-PREM            
06505                                  + BSR-GROSS-LIFE-PREM.           
06506      COMPUTE KSM-GROSS-AH-PREM = KSM-GROSS-AH-PREM                
06507                                + BSR-GROSS-AH-PREM.               
06508      COMPUTE KSM-GROSS-LIFE-REFUNDS = KSM-GROSS-LIFE-REFUNDS      
06509                                     + BSR-GROSS-LIFE-REFUNDS.     
06510      COMPUTE KSM-GROSS-AH-REFUNDS = KSM-GROSS-AH-REFUNDS          
06511                                   + BSR-GROSS-AH-REFUNDS.         
06512      COMPUTE KSM-GROSS-WRITTEN = KSM-GROSS-WRITTEN                
06513                                + BSR-GROSS-WRITTEN.               
06514      COMPUTE KSM-GROSS-REFUNDED = KSM-GROSS-REFUNDED              
06515                                 + BSR-GROSS-REFUNDED.             
06516                                                                   
06517      WRITE BILLING-DATA-RECORD  FROM  WS-BILLING-SUMMARY-RECORD.  
06518                                                                   
06519      ADD +1                      TO  KSM-COUNT.                   
06520                                                                   
06521  6010-CLEAR-BILLING-SUM-RECORD.                                   
06522      MOVE SPACES                 TO  WS-BILLING-SUMMARY-RECORD.   
06523      MOVE ZEROS                  TO  BSR-NET-LIFE-PREM            
06524                                      BSR-NET-AH-PREM              
06525                                      BSR-LIFE-COMM                
06526                                      BSR-AH-COMM                  
06527                                      BSR-TOTAL-PREM               
06528                                      BSR-TOTAL-COMM               
06529                                      BSR-TOTAL-DUE                
06530                                      BSR-GROSS-LIFE-PREM          
06531                                      BSR-GROSS-AH-PREM            
06532                                      BSR-GROSS-LIFE-REFUNDS       
06533                                      BSR-GROSS-AH-REFUNDS         
06534                                      BSR-GROSS-WRITTEN            
06535                                      BSR-GROSS-REFUNDED           
06536                                      BSR-ISS-LF-CNT               
06537                                      BSR-ISS-AH-CNT               
06538                                      BSR-CAN-LF-CNT               
06539                                      BSR-CAN-AH-CNT               
06540                                      BSR-CAN-PREM-91              
06541                                      KSM-ISS-LF-CNT               
06542                                      KSM-ISS-AH-CNT               
06543                                      KSM-CAN-LF-CNT               
06544                                      KSM-CAN-AH-CNT               
06545                                      KSM-CAN-PREM-91.             
06546                                                                   
06547       MOVE ZEROS                 TO  KSM-LO-CERT-DATE.            
06548       MOVE ZEROS                 TO  KSM-HI-CERT-DATE.            
06549                                                                   
06550  6099-EXIT.                                                       
06551      EXIT.                                                        
06552  EJECT                                                            
06553  6500-WRITE-BILLING-DET.                                          
06554                                                                   
06555      IF WS-ISSUE OR WS-CANCEL                                     
06556          NEXT SENTENCE                                            
06557      ELSE                                                         
06558          GO TO 6575-CLEAR-BILLING-DET-RECORD.                     
06559                                                                   
06560      IF SAV-KSM-CARRIER = '3'                                     
06561          GO TO 6575-CLEAR-BILLING-DET-RECORD.                     
06562                                                                   
06563      IF WS-CANCEL                                                 
06564          GO TO 6550-KSM-CANCEL.                                   
06565                                                                   
06566      MOVE WS-SRT02-LNAME         TO  BDR-NAME.                    
06567      MOVE WS-SRT02-FNAME         TO  BDR-FIRST-NAME.              
06568      MOVE WS-SRT02-INTL          TO  BDR-INITIAL.                 
06569      MOVE WS-SRT02-CERT          TO  BDR-CERT.                    
06570      MOVE WS-SRT02-AGE           TO  BDR-AGE.                     
06571      MOVE WS-SRT02-EFF-MO        TO  BDR-EFF-MM.                  
06572      MOVE WS-SRT02-EFF-DA        TO  BDR-EFF-DD.                  
06573      MOVE WS-SRT02-EFF-YR        TO  BDR-EFF-YY.                  
06574      MOVE WS-SRT02-EFF-CC        TO  BDR-EFF-CC.                  
06575                                                                   
06576      IF WS-SRT02-LF-TERM = ZEROS AND                              
06577         WS-SRT02-LF-PREM = ZEROS                                  
06578          MOVE ZEROS              TO  BDR-LIFE-TERM                
06579                                      BDR-FACE-AMOUNT              
06580                                      BDR-LIFE-PREMIUM             
06581                                      BDR-LIFE-COMMISSION          
06582                                      BDR-FACE-AMT-ALT             
06583                                      BDR-LIFE-PREM-ALT            
06584                                      BDR-LIFE-COMM-ALT            
06585      ELSE                                                         
06586          MOVE WS-SRT02-LF-TERM   TO  BDR-LIFE-TERM                
06587          MOVE LIFE-OVERRIDE-L2   TO  BDR-LIFE-BEN                 
06588          MOVE WS-SRT02-LF-TYP    TO  BDR-LIFE-BEN-TYPE            
06589          MOVE WS-SRT02-FACE      TO  BDR-FACE-AMOUNT              
06590          MOVE WS-SRT02-LF-PREM   TO  BDR-LIFE-PREMIUM             
06591          MOVE WS-SRT02-LF-COMM   TO  BDR-LIFE-COMMISSION          
06592          MOVE WS-SRT02-FACE-ALT  TO  BDR-FACE-AMT-ALT             
06593          MOVE WS-SRT02-LF-PREM-ALT                                
06594                                  TO  BDR-LIFE-PREM-ALT            
06595          MOVE WS-SRT02-LF-COMM-ALT                                
06596                                  TO  BDR-LIFE-COMM-ALT.           
06597                                                                   
06598      IF WS-SRT02-AH-TERM = ZEROS AND                              
06599         WS-SRT02-AH-PREM = ZEROS                                  
06600          MOVE SPACE              TO  BDR-AH-BEN                   
06601          MOVE ZEROS              TO  BDR-AH-TERM                  
06602                                      BDR-AH-MO-BENEFIT            
06603                                      BDR-AH-PREMIUM               
06604                                      BDR-AH-COMMISSION            
06605      ELSE                                                         
06606          MOVE WS-SRT02-AH-PREM   TO  BDR-AH-PREMIUM               
06607          MOVE WS-SRT02-AH-COMM   TO  BDR-AH-COMMISSION            
06608          MOVE WS-SRT02-AH-BEN    TO  BDR-AH-MO-BENEFIT            
06609          MOVE WS-SRT02-AH-TERM   TO  BDR-AH-TERM                  
06610          MOVE AH-OVERRIDE-L2     TO  BDR-AH-BEN                   
06611          MOVE WS-SRT02-AH-TYP    TO  BDR-AH-BEN-TYPE.             
06612                                                                   
06613      GO TO 6560-KSM-WRITE.                                        
06614                                                                   
06615  6550-KSM-CANCEL.                                                 
06616                                                                   
06617      MOVE WS-SRT03-LNAME         TO  BDR-NAME.                    
06618      MOVE WS-SRT03-FNAME         TO  BDR-FIRST-NAME.              
06619      MOVE WS-SRT03-INTL          TO  BDR-INITIAL.                 
06620      MOVE WS-SRT03-CERT          TO  BDR-CERT.                    
06621      MOVE WS-SRT03-AGE           TO  BDR-AGE.                     
06622      MOVE WS-SRT03-LF-TERM       TO  BDR-LIFE-TERM.               
06623      MOVE WS-SRT03-AH-TERM       TO  BDR-AH-TERM.                 
06624      MOVE WS-SRT03-LF-FACE       TO  BDR-FACE-AMOUNT.             
06625      MOVE WS-SRT03-AH-MO-BEN     TO  BDR-AH-MO-BENEFIT.           
06626      MOVE WS-SRT03-EFF-MO        TO  BDR-EFF-MM.                  
06627      MOVE WS-SRT03-EFF-DA        TO  BDR-EFF-DD.                  
06628      MOVE WS-SRT03-EFF-YR        TO  BDR-EFF-YY.                  
06629      MOVE WS-SRT03-EFF-CC        TO  BDR-EFF-CC.                  
06630      MOVE WS-SRT03-LF-CAN-MO     TO  BDR-LF-CAN-MM.               
06631      MOVE WS-SRT03-LF-CAN-DA     TO  BDR-LF-CAN-DD.               
06632      MOVE WS-SRT03-LF-CAN-YR     TO  BDR-LF-CAN-YY.               
06633      MOVE WS-SRT03-LF-CAN-CC     TO  BDR-LF-CAN-CC.               
06634      MOVE WS-SRT03-AH-CAN-MO     TO  BDR-AH-CAN-MM.               
06635      MOVE WS-SRT03-AH-CAN-DA     TO  BDR-AH-CAN-DD.               
06636      MOVE WS-SRT03-AH-CAN-YR     TO  BDR-AH-CAN-YY.               
06637      MOVE WS-SRT03-AH-CAN-CC     TO  BDR-AH-CAN-CC.               
06638                                                                   
06639      MOVE ZEROS                  TO  BDR-FACE-AMT-ALT             
06640                                      BDR-LIFE-PREM-ALT            
06641                                      BDR-LIFE-COMM-ALT.           
06642                                                                   
06643      IF WS-SRT03-LF-CAN-DT = ZEROS AND                            
06644         WS-SRT03-LF-REF = ZEROS                                   
06645          MOVE ZEROS              TO  BDR-LIFE-TERM                
06646                                      BDR-FACE-AMOUNT              
06647                                      BDR-LIFE-PREMIUM             
06648                                      BDR-LIFE-COMMISSION          
06649      ELSE                                                         
06650          MOVE LIFE-OVERRIDE-L2   TO  BDR-LIFE-BEN                 
06651          MOVE WS-SRT03-LF-TYP    TO  BDR-LIFE-BEN-TYPE            
06652          MOVE WS-SRT03-LF-REF    TO  BDR-LIFE-PREMIUM             
06653          MOVE WS-SRT03-LF-COM-REF                                 
06654                                  TO  BDR-LIFE-COMMISSION.         
06655                                                                   
06656      IF WS-SRT03-AH-CAN-DT = ZEROS AND                            
06657         WS-SRT03-AH-REF = ZEROS                                   
06658          MOVE ZEROS              TO  BDR-AH-TERM                  
06659                                      BDR-AH-MO-BENEFIT            
06660                                      BDR-AH-PREMIUM               
06661                                      BDR-AH-COMMISSION            
06662      ELSE                                                         
06663          MOVE AH-OVERRIDE-L2     TO  BDR-AH-BEN                   
06664          MOVE WS-SRT03-AH-TYP    TO  BDR-AH-BEN-TYPE              
06665          MOVE WS-SRT03-AH-REF    TO  BDR-AH-PREMIUM               
06666          MOVE WS-SRT03-AH-COM-REF                                 
06667                                  TO  BDR-AH-COMMISSION.           
06668  6560-KSM-WRITE.                                                  
06669                                                                   
06670      MOVE 'D'                    TO  BDR-RECORD-TYPE.             
06671      MOVE SAV-KSM-CARR-GROUP     TO  BDR-CARR-GROUP.              
06672      MOVE SAV-KSM-ACCOUNT        TO  BDR-ACCOUNT.                 
06673      MOVE SAV-KSM-ACCT-NAME      TO  BDR-ACCT-NAME.               
06674      MOVE SAV-KSM-ADDR-1         TO  BDR-ADDR-1.                  
06675      MOVE SAV-KSM-ADDR-2         TO  BDR-ADDR-2.                  
06676      MOVE SAV-KSM-CITY-STATE     TO  BDR-CITY-STATE.              
06677      MOVE SAV-KSM-ZIP            TO  BDR-ZIP.                     
06678      MOVE SAV-KSM-DATE           TO  BDR-DATE.                    
06679                                                                   
06680      WRITE BILLING-DATA-RECORD  FROM  WS-BILLING-DETAIL-RECORD.   
06681                                                                   
06682      ADD +1                      TO  KSM-DETAIL-COUNT.            
06683                                                                   
06684  6575-CLEAR-BILLING-DET-RECORD.                                   
06685      MOVE SPACES                 TO  WS-BILLING-DETAIL-RECORD.    
06686                                                                   
06687  6599-EXIT.                                                       
06688      EXIT.                                                        
06689  EJECT                                                            
06690                                                                   
06691  7000-ACCOUNT-MASTER-LOOK.                                        
06692      IF CP-ISSUE    OR CP-CANCEL OR                               
06693         CP-RC-ISSUE OR CP-RC-CANCEL                               
122002*       OR CP-MONTHLY-ISSUE
06694         CONTINUE                                                  
06695      ELSE                                                         
PEMTST        GO TO 7999-EXIT
PEMTST     END-IF                                                       
06697                                                                   
06698      IF CP-CARRIER   =  AM-CARRIER  AND                           
06699         CP-GROUPING  =  AM-GROUPING AND                           
06700         CP-STATE     =  AM-STATE    AND                           
06701         CP-ACCOUNT   =  AM-ACCOUNT                                
06702          NEXT SENTENCE                                            
06703      ELSE                                                         
06704          GO TO 7200-START.                                        
06705                                                                   
06706      IF CP-EFF LESS THAN WS-EFF-DATE                              
06707          GO TO 7200-START.                                        
06708                                                                   
06709      IF CP-EFF GREATER THAN WS-EXP-DATE                           
06710          GO TO 7400-READ-ACCT-NEXT.                               
06711                                                                   
06712      GO TO 7600-MOVE-ACCT-INFO.                                   
06713                                                                   
06714  7200-START.                                                      
06715                                                                   
06716      MOVE LOW-VALUES            TO AM-CONTROL-PRIMARY.            
06717                                                                   
06718      MOVE DTE-CLASIC-COMPANY-CD TO AM-COMPANY-CD.                 
06719      MOVE CP-CARRIER            TO AM-CARRIER.                    
06720      MOVE CP-GROUPING           TO AM-GROUPING.                   
06721      MOVE CP-STATE              TO AM-STATE.                      
06722      MOVE CP-ACCOUNT            TO AM-ACCOUNT.                    
06723                                                                   
06724      START ERACCT                                                 
06725             KEY NOT LESS AM-CONTROL-PRIMARY.                      
06726                                                                   
06727      IF ERACCT-FILE-STATUS NOT = ZERO                             
06728          MOVE 'ERROR OCCURED START - ERACCT'                      
06729                                   TO WS-ABEND-MESSAGE             
06730          MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS          
06731          GO TO ABEND-PGM.                                         
06732                                                                   
06733  7400-READ-ACCT-NEXT.                                             
06734      READ ERACCT NEXT RECORD.                                     
06735                                                                   
06736      IF ERACCT-FILE-STATUS = '10'                                 
06737          MOVE 'NO ACCT FOUND - EOF' TO WS-ABEND-MESSAGE           
06738          MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS          
06739          GO TO ABEND-PGM.                                         
06740                                                                   
06741      IF ERACCT-FILE-STATUS NOT = ZERO                             
06742          MOVE 'ERROR OCCURED READ - ERACCT'                       
06743                                   TO WS-ABEND-MESSAGE             
06744          MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS          
06745          GO TO ABEND-PGM.                                         
06746                                                                   
06747      IF AM-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 
06748          MOVE 'NO ACCT FOUND - CD' TO WS-ABEND-MESSAGE            
06749          MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS          
06750          GO TO ABEND-PGM.                                         
06751                                                                   
06752      IF CP-CARRIER    = AM-CARRIER  AND                           
06753         CP-GROUPING   = AM-GROUPING AND                           
06754         CP-STATE      = AM-STATE    AND                           
06755         CP-ACCOUNT    = AM-ACCOUNT                                
06756            NEXT SENTENCE                                          
06757          ELSE                                                     
06758          DISPLAY '** NO ACCT   CP-CAR= ' CP-CARRIER               
06759                              ' AM-CAR= ' AM-CARRIER               
06760                              ' CP-ACC= ' CP-ACCOUNT               
06761                              ' AM-ACC= ' AM-ACCOUNT               
06762          GO TO 7999-EXIT.                                         
06763                                                                   
PEMTST     MOVE ZEROS                 TO WS-EFF-DATE-NUM
PEMTST                                   WS-EXP-DATE-NUM
06764      MOVE AM-EFFECTIVE-DT       TO DC-BIN-DATE-1.                 
06765      MOVE ' '                   TO DC-OPTION-CODE.                
06766      PERFORM 1900-DATE-CONVERSION  THRU  1999-EXIT.               
06767      MOVE DC-YMD-YEAR           TO WS-EFF-YY.                     
06768      MOVE DC-YMD-MONTH          TO WS-EFF-MM.                     
06769      MOVE DC-YMD-DAY            TO WS-EFF-DD.                     
06770      MOVE DC-ALPHA-CEN-N        TO WS-EFF-CC.                     
06771      MOVE WS-EFF-DATE-NUM       TO WS-EFF-DATE.                   
06772                                                                   
06773      IF AM-EXPIRATION-DT = HIGH-VALUES                            
06774          MOVE 00099999999       TO WS-EXP-DATE-NUM                
06775                                    WS-EXP-DATE                    
06776       ELSE                                                        
06777          MOVE AM-EXPIRATION-DT  TO DC-BIN-DATE-1                  
06778          MOVE ' '               TO DC-OPTION-CODE                 
06779          PERFORM 1900-DATE-CONVERSION  THRU  1999-EXIT            
06780          MOVE DC-YMD-YEAR        TO WS-EXP-YY                     
06781          MOVE DC-YMD-MONTH       TO WS-EXP-MM                     
06782          MOVE DC-YMD-DAY         TO WS-EXP-DD                     
06783          MOVE DC-ALPHA-CEN-N     TO WS-EXP-CC                     
06784          MOVE WS-EXP-DATE-NUM    TO WS-EXP-DATE.                  
06785                                                                   
06786  7500-TEST-DATES.                                                 
06787      IF CP-EFF LESS THAN WS-EFF-DATE OR                           
06788         CP-EFF GREATER THAN WS-EXP-DATE                           
06789          GO TO 7400-READ-ACCT-NEXT.                               
06790                                                                   
06791  7600-MOVE-ACCT-INFO.                                             
CIDMOD*    MOVE AM-REI-TABLE          TO WS-SRT02-REI-TABLE.            
CIDMOD*    MOVE AM-STATUS             TO WS-SRT02-ACCT-STATUS.          
CIDMOD*    MOVE AM-REPORT-CODE-1      TO WS-SRT02-RPTCDE.               
PEMTST*    IF AM-CONTROL-NAME (1:5) NOT ALPHABETIC
PEMTST*       MOVE SPACES             TO AM-CONTROL-NAME
PEMTST*    END-IF
PEMTST*    MOVE AM-CONTROL-NAME       TO WS-CONTROL-NAME
PEMTST     .
06795                                                                   
06796  7999-EXIT.                                                       
06797       EXIT.                                                       
06798       EJECT                                                       
06799                                                                   
06800  8800-PRT-RTN.                                                    
06801      MOVE P-REC                  TO  PRT.                         
06802      MOVE P-CCSW                 TO  X.                           
06803                                                                   
06804      IF P-CCSW = SPACE-1                                          
06805          ADD +1                  TO  LNCTR                        
06806      ELSE                                                         
06807          IF P-CCSW = SPACE-2                                      
06808              ADD +2              TO  LNCTR                        
06809          ELSE                                                     
06810              IF P-CCSW = SPACE-3                                  
06811                  ADD +3          TO  LNCTR                        
06812              ELSE                                                 
06813                  MOVE +1         TO  LNCTR.                       
06814                                                                   
06815      MOVE SPACES                 TO  P-REC.                       
06816                                                                   
06817  8810-COPY-PRT-RTN.                                               
06818                              COPY ELCPRT2.                        
06819                                                                   
06820  8899-EXIT.                                                       
06821      EXIT.                                                        
06822  EJECT                                                            
06823  9000-E-O-J.                                                      

091306     IF DTE-CLIENT = 'DCC' or 'CAP'
091306        DIVIDE TPGCTR BY 2 GIVING WK1 REMAINDER WK2
091306        IF WK2 NOT = ZEROS
091306           MOVE SPACES           TO P-REC
091306           MOVE '1'              TO P-CCSW
091306           PERFORM 8800-PRT-RTN  THRU 8899-EXIT
091306        END-IF
091306     END-IF



06824      MOVE +0                     TO  PGCTR.                       
           MOVE +1                     TO FT-SUB
06825      MOVE '        OVERALL RECAP         '                        
06826                                  TO  HD1-TITLE.                   
06827      MOVE HD8D                   TO  HD6.                         
06828      MOVE 'Y'                    TO  RECAP-PAGE-SWITCH.           
06829                                                                   
06830      PERFORM 3910-PRT-FIRST-PAGE  THRU  3999-EXIT.                
06831                                                                   
06832      MOVE T-DESC-PRM-L           TO  T-DESC.                      
06833      MOVE NFT-L-PRM (FT-SUB)     TO  T-AMT-P.                     
06834      MOVE NPF-L-PRM (FT-SUB)     TO  T-AMT-N.                     
06835      MOVE TOTF-L-PRM (FT-SUB)    TO  T-AMT-T.                     
06836      MOVE SPACE-2                TO  P-CCSW.                      
06837                                                                   
06838      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06839                                                                   
06840      MOVE T-DESC-PRM-A           TO  T-DESC.                      
06841      MOVE NFT-A-PRM  (FT-SUB)    TO  T-AMT-P.                     
06842      MOVE NPF-A-PRM  (FT-SUB)    TO  T-AMT-N.                     
06843      MOVE TOTF-A-PRM (FT-SUB)    TO  T-AMT-T.                     
06844                                                                   
06845      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06846                                                                   
06847      MOVE 'TOTAL PREMIUMS'       TO  T-DESC.                      
06848      MOVE NFT-T-PRM  (FT-SUB)    TO  T-AMT-P.                     
06849      MOVE NPF-T-PRM  (FT-SUB)    TO  T-AMT-N.                     
06850      MOVE TOTF-T-PRM (FT-SUB)    TO  T-AMT-T.                     
06851      MOVE 'A'                    TO  T-EXPL.                      
06852                                                                   
06853      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06854                                                                   
06855      MOVE T-DESC-REF-L           TO  T-DESC.                      
06856      MOVE NFT-L-REF  (FT-SUB)    TO  T-AMT-P.                     
06857      MOVE NPF-L-REF  (FT-SUB)    TO  T-AMT-N.                     
06858      MOVE TOTF-L-REF (FT-SUB)    TO  T-AMT-T.                     
06859      MOVE SPACE-2                TO  P-CCSW.                      
06860                                                                   
06861      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06862                                                                   
06863      MOVE T-DESC-REF-A           TO  T-DESC.                      
06864      MOVE NFT-A-REF  (FT-SUB)    TO  T-AMT-P.                     
06865      MOVE NPF-A-REF  (FT-SUB)    TO  T-AMT-N.                     
06866      MOVE TOTF-A-REF (FT-SUB)    TO  T-AMT-T.                     
06867                                                                   
06868      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06869                                                                   
06870      MOVE 'TOTAL REFUNDS '       TO  T-DESC.                      
06871      MOVE NFT-T-REF  (FT-SUB)    TO  T-AMT-P.                     
06872      MOVE NPF-T-REF  (FT-SUB)    TO  T-AMT-N.                     
06873      MOVE TOTF-T-REF (FT-SUB)    TO  T-AMT-T.                     
06874      MOVE 'B'                    TO  T-EXPL.                      
06875                                                                   
06876      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06877                                                                   
06878      MOVE T-DESC-NET-L           TO  T-DESC.                      
06879      MOVE NFT-L-NET  (FT-SUB)    TO  T-AMT-P.                     
06880      MOVE NPF-L-NET  (FT-SUB)    TO  T-AMT-N.                     
06881      MOVE TOTF-L-NET (FT-SUB)    TO  T-AMT-T.                     
06882      MOVE SPACE-2                TO  P-CCSW.                      
06883                                                                   
06884      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06885                                                                   
06886      MOVE T-DESC-NET-A           TO  T-DESC.                      
06887      MOVE NFT-A-NET  (FT-SUB)    TO  T-AMT-P.                     
06888      MOVE NPF-A-NET  (FT-SUB)    TO  T-AMT-N.                     
06889      MOVE TOTF-A-NET (FT-SUB)    TO  T-AMT-T.                     
06890                                                                   
06891      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06892                                                                   
06893      MOVE 'TOTAL NET PREMIUMS'   TO  T-DESC.                      
06894      MOVE NFT-T-NET  (FT-SUB)    TO  T-AMT-P.                     
06895      MOVE NPF-T-NET  (FT-SUB)    TO  T-AMT-N.                     
06896      MOVE TOTF-T-NET (FT-SUB)    TO  T-AMT-T.                     
06897      MOVE 'C=A-B'                TO  T-EXPL.                      
06898                                                                   
06899      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06900                                                                   
070714     if me-do-update
070714        move nft-t-net (ft-sub)  to hld-562-prem-tot
070714        COMPUTE hld-562-comm-tot =
070714           NFT-T-COM (FT-SUB) - NFT-T-RCM (FT-SUB)
070714     end-if   

06901      MOVE T-DESC-COM-L           TO  T-DESC.                      
011904*    MOVE NFT-L-COM              TO  T-AMT-P.                     
011904     COMPUTE T-AMT-P = NFT-L-COM (FT-SUB)
              - NFT-L-RCM (FT-SUB)
011904*    MOVE NPF-L-COM              TO  T-AMT-N.                     
011904     COMPUTE T-AMT-N = NPF-L-COM (FT-SUB)
              - NPF-L-RCM (FT-SUB)
011904*    MOVE TOTF-L-COM             TO  T-AMT-T.                     
011904     COMPUTE T-AMT-T = TOTF-L-COM (FT-SUB)
              - TOTF-L-RCM (FT-SUB)
06905      MOVE SPACE-2                TO  P-CCSW.                      
06906                                                                   
06907      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06908                                                                   
06909      MOVE T-DESC-COM-A           TO  T-DESC.                      
011904*    MOVE NFT-A-COM              TO  T-AMT-P.                     
011904     COMPUTE T-AMT-P = NFT-A-COM (FT-SUB)
              - NFT-A-RCM (FT-SUB)
011904*    MOVE NPF-A-COM              TO  T-AMT-N.                     
011904     COMPUTE T-AMT-N = NPF-A-COM (FT-SUB)
              - NPF-A-RCM (FT-SUB)
011904*    MOVE TOTF-A-COM             TO  T-AMT-T.                     
011904     COMPUTE T-AMT-T = TOTF-A-COM (FT-SUB)
              - TOTF-A-RCM (FT-SUB)
06913                                                                   
06914      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06915                                                                   
06916      MOVE 'TOTAL COMPENSATION'   TO  T-DESC.                      
011904*    MOVE NFT-T-COM              TO  T-AMT-P.                     
011904     COMPUTE T-AMT-P = NFT-T-COM (FT-SUB)
              - NFT-T-RCM (FT-SUB)
011904*    MOVE NPF-T-COM              TO  T-AMT-N.                     
011904     COMPUTE T-AMT-N = NPF-T-COM (FT-SUB)
              - NPF-T-RCM (FT-SUB)
011904*    MOVE TOTF-T-COM             TO  T-AMT-T.                     
011904     COMPUTE T-AMT-T = TOTF-T-COM (FT-SUB)
              - TOTF-T-RCM (FT-SUB)
06920      MOVE 'D'                    TO  T-EXPL.                      
06921                                                                   
06922      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06923                                                                   
011904*    COMPUTE T-AMT-P  =  NFT-T-PRM  -  NFT-T-REF  -  NFT-T-COM.   
011904     COMPUTE T-AMT-P = NFT-T-PRM (FT-SUB)
              - NFT-T-REF (FT-SUB) -
011904        (NFT-T-COM (FT-SUB) - NFT-T-RCM (FT-SUB))
06925                                                                   
06926      MOVE 'AMOUNT DUE FOR THIS MONTH'  TO  T-DESC.                
06927      MOVE 'E=C-D'                      TO  T-EXPL.                
06928      MOVE SPACE-2                      TO  P-CCSW.                
06929                                                                   
06930      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06931                                                                   
06932      MOVE 'BALANCE BROUGHT FORWARD'  TO  T-DESC.                  
06933      MOVE NFT-B-BAL (FT-SUB)         TO  T-AMT-P.                 
06934      MOVE 'F'                        TO  T-EXPL.                  
06935      MOVE SPACE-2                    TO  P-CCSW.                  
06936                                                                   
06937      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06938                                                                   
06939      MOVE 'AMOUNT PAID'          TO  T-DESC.                      
06940      MOVE NFT-R-PMT  (FT-SUB)    TO  T-AMT-P.                     
06941      MOVE 'G'                    TO  T-EXPL.                      
06942      MOVE SPACE-2                TO  P-CCSW.                      
06943                                                                   
06944      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06945                                                                   
06946      MOVE 'AMOUNT CHARGED'       TO  T-DESC.                      
06947      MOVE NFT-C-CHG (FT-SUB)     TO  T-AMT-P.                     
06948      MOVE 'H'                    TO  T-EXPL.                      
06949      MOVE SPACE-2                TO  P-CCSW.                      
06950                                                                   
06951      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06952                                                                   
06953      MOVE 'WRITE OFF ADJUSTMENT'  TO  T-DESC.                     
06954      MOVE NFT-W-OFF (FT-SUB)      TO  T-AMT-P.                    
06955      MOVE 'I'                     TO  T-EXPL.                     
06956      MOVE SPACE-2                 TO  P-CCSW.                     
06957                                                                   
06958      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06959                                                                   
06960      MOVE ALL '-'                TO  T-AMTR-P.                    
06961      MOVE SPACE-2                TO  P-CCSW.                      
06962                                                                   
06963      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06964                                                                   
06965      MOVE 'BALANCE CARRIED FORWARD'  TO  T-DESC.                  
06966      MOVE NFT-E-BAL (FT-SUB)         TO  T-AMT-P.                 
06967      MOVE 'J=E+F-G+H+I'              TO  T-EXPL.                  
06968      MOVE SPACE-2                    TO  P-CCSW.                  
06969                                                                   
06970      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06971                                                                   
06972      IF T-TOT-COM NOT = T-ADJ-COM                                 
06973          MOVE 'ADJUSTED COMMISSION'  TO  T-DESC                   
06974          MOVE T-ADJ-COM              TO  T-AMT-P                  
06975          MOVE SPACE-3                TO  P-CCSW                   
06976          PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                   


120804     IF DTE-CLIENT NOT = 'DCC' and 'CAP'
120804        GO TO 9000-CONTINUE
120804     END-IF
           
091306     DIVIDE TPGCTR BY 2 GIVING WK1 REMAINDER WK2
091306     IF WK2 NOT = ZEROS
091306        MOVE SPACES              TO P-REC
091306        MOVE '1'                 TO P-CCSW
091306        PERFORM 8800-PRT-RTN     THRU 8899-EXIT
091306     END-IF

06824      MOVE +0                     TO  PGCTR.                       
           MOVE +2                     TO FT-SUB
06825      MOVE ' OVERALL RECAP (SECURE PAY PLUS) '                        
06826                                  TO  HD1-TITLE.                   
06827      MOVE HD8D                   TO  HD6.                         
06828      MOVE 'Y'                    TO  RECAP-PAGE-SWITCH.           
06829                                                                   
06830      PERFORM 3910-PRT-FIRST-PAGE  THRU  3999-EXIT.                
06846                                                                   
011904     MOVE 'TOTAL ISSUE FEES'     TO  T-DESC
06848      MOVE NFT-T-PRM  (FT-SUB)    TO  T-AMT-P.                     
06849      MOVE NPF-T-PRM  (FT-SUB)    TO  T-AMT-N.                     
06850      MOVE TOTF-T-PRM (FT-SUB)    TO  T-AMT-T.                     
06851      MOVE 'A'                    TO  T-EXPL.                      
011904     MOVE SPACE-2                TO  P-CCSW
06852                                                                   
06853      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06854                                                                   
06916      MOVE 'TOTAL ISSUE INCOME'   TO  T-DESC.                      
011904     MOVE NFT-T-COM  (FT-SUB)    TO  T-AMT-P.                     
011904     MOVE NPF-T-COM  (FT-SUB)    TO  T-AMT-N.                     
011904     MOVE TOTF-T-COM (FT-SUB)    TO  T-AMT-T.                     
06920      MOVE 'B'                    TO  T-EXPL.                      
011904     MOVE SPACE-2                TO  P-CCSW
06921                                                                   
06922      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06869                                                                   
06870      MOVE 'TOTAL REFUND INCOME ' TO  T-DESC.                      
06871      MOVE NFT-T-RCM  (FT-SUB)    TO  T-AMT-P.                     
06872      MOVE NPF-T-RCM  (FT-SUB)    TO  T-AMT-N.                     
06873      MOVE TOTF-T-RCM (FT-SUB)    TO  T-AMT-T.                     
06874      MOVE 'C'                    TO  T-EXPL.                      
011904     MOVE SPACE-2                TO  P-CCSW
06875                                                                   
06876      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06877                                                                   
06923                                                                   
011904*    COMPUTE T-AMT-P  =  NFT-T-PRM  -  NFT-T-REF  -  NFT-T-COM.   
011904     COMPUTE T-AMT-P = NFT-T-PRM (FT-SUB)
011904        - (NFT-T-COM (FT-SUB) - NFT-T-RCM (FT-SUB))
06925                                                                   
06926      MOVE 'AMOUNT DUE FOR THIS MONTH'  TO  T-DESC.                
06927      MOVE 'E=A-B+C'                      TO  T-EXPL.                
06928      MOVE SPACE-2                      TO  P-CCSW.                
06929                                                                   
06930      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       

06932      MOVE 'BALANCE BROUGHT FORWARD'  TO  T-DESC.                  
06933      MOVE NFT-B-BAL (FT-SUB)         TO  T-AMT-P.                 
06934      MOVE 'F'                        TO  T-EXPL.                  
06935      MOVE SPACE-2                    TO  P-CCSW.                  
06936                                                                   
06937      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06938                                                                   
06939      MOVE 'AMOUNT PAID'          TO  T-DESC.                      
06940      MOVE NFT-R-PMT  (FT-SUB)    TO  T-AMT-P.                     
06941      MOVE 'G'                    TO  T-EXPL.                      
06942      MOVE SPACE-2                TO  P-CCSW.                      
06943                                                                   
06944      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06945                                                                   
06946      MOVE 'AMOUNT CHARGED'       TO  T-DESC.                      
06947      MOVE NFT-C-CHG (FT-SUB)     TO  T-AMT-P.                     
06948      MOVE 'H'                    TO  T-EXPL.                      
06949      MOVE SPACE-2                TO  P-CCSW.                      
06950                                                                   
06951      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06952                                                                   
06953      MOVE 'WRITE OFF ADJUSTMENT'  TO  T-DESC.                     
06954      MOVE NFT-W-OFF (FT-SUB)      TO  T-AMT-P.                    
06955      MOVE 'I'                     TO  T-EXPL.                     
06956      MOVE SPACE-2                 TO  P-CCSW.                     
06957                                                                   
06958      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06959                                                                   
06960      MOVE ALL '-'                TO  T-AMTR-P.                    
06961      MOVE SPACE-2                TO  P-CCSW.                      
06962                                                                   
06963      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
06964                                                                   
06965      MOVE 'BALANCE CARRIED FORWARD'  TO  T-DESC.                  
06966      MOVE NFT-E-BAL (FT-SUB)         TO  T-AMT-P.                 
06967      MOVE 'J=E+F-G+H+I'              TO  T-EXPL.                  
06968      MOVE SPACE-2                    TO  P-CCSW.                  
06969                                                                   
06970      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       

120804     MOVE 'TOTAL NET   FEES'     TO  T-DESC
120804     COMPUTE T-AMT-P = NFT-T-PRM (FT-SUB) -
120804        NFT-T-REF (FT-SUB)
120804     MOVE ' NET FEES  '          TO  T-EXPL
120804     MOVE SPACE-2                TO  P-CCSW
120804
120804     PERFORM 8800-PRT-RTN  THRU  8899-EXIT

011410     MOVE +0                     TO  PGCTR.                       
011410     MOVE +3                     TO FT-SUB
011410
011410     MOVE ' OVERALL RECAP (DEALER DIRECT)   '
011410                                 TO  HD1-TITLE.                   
011410     MOVE HD8D                   TO  HD6.                         
011410     MOVE 'Y'                    TO  RECAP-PAGE-SWITCH.           
011410                                                                  
011410     PERFORM 3910-PRT-FIRST-PAGE  THRU  3999-EXIT.                
011410                                                                  
011410     MOVE 'TOTAL LIFE FEES'      TO  T-DESC.                      
011410     MOVE NFT-L-PRM (FT-SUB)     TO  T-AMT-P.                     
011410     MOVE NPF-L-PRM (FT-SUB)     TO  T-AMT-N.                     
011410     MOVE TOTF-L-PRM (FT-SUB)    TO  T-AMT-T.                     
011410     MOVE SPACE-2                TO  P-CCSW.                      
011410                                                                  
011410     PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
011410                                                                  
011410     MOVE 'TOTAL A&H FEES'       TO  T-DESC.                      
011410     MOVE NFT-A-PRM  (FT-SUB)    TO  T-AMT-P.                     
011410     MOVE NPF-A-PRM  (FT-SUB)    TO  T-AMT-N.                     
011410     MOVE TOTF-A-PRM (FT-SUB)    TO  T-AMT-T.                     
011410                                                                  
011410     PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
011410                                                                  
011410     MOVE 'TOTAL FEES'           TO  T-DESC.                      
011410     MOVE NFT-T-PRM  (FT-SUB)    TO  T-AMT-P.                     
011410     MOVE NPF-T-PRM  (FT-SUB)    TO  T-AMT-N.                     
011410     MOVE TOTF-T-PRM (FT-SUB)    TO  T-AMT-T.                     
011410     MOVE 'A'                    TO  T-EXPL.                      
011410                                                                  
011410     PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
011410                                                                  
011410     MOVE 'TOTAL LIFE REFUNDS'   TO  T-DESC.                      
011410     MOVE NFT-L-REF  (FT-SUB)    TO  T-AMT-P.                     
011410     MOVE NPF-L-REF  (FT-SUB)    TO  T-AMT-N.                     
011410     MOVE TOTF-L-REF (FT-SUB)    TO  T-AMT-T.                     
011410     MOVE SPACE-2                TO  P-CCSW.                      
011410                                                                  
011410     PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
011410                                                                  
011410     MOVE 'TOTAL A&H REFUNDS'    TO  T-DESC.                      
011410     MOVE NFT-A-REF  (FT-SUB)    TO  T-AMT-P.                     
011410     MOVE NPF-A-REF  (FT-SUB)    TO  T-AMT-N.                     
011410     MOVE TOTF-A-REF (FT-SUB)    TO  T-AMT-T.                     
011410                                                                  
011410     PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
011410                                                                  
011410     MOVE 'TOTAL REFUNDS '       TO  T-DESC.                      
011410     MOVE NFT-T-REF  (FT-SUB)    TO  T-AMT-P.                     
011410     MOVE NPF-T-REF  (FT-SUB)    TO  T-AMT-N.                     
011410     MOVE TOTF-T-REF (FT-SUB)    TO  T-AMT-T.                     
011410     MOVE 'B'                    TO  T-EXPL.                      
011410                                                                  
011410     PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
011410                                                                  
011410     MOVE 'TOTAL LIFE NET FEES'  TO  T-DESC.                      
011410     MOVE NFT-L-NET  (FT-SUB)    TO  T-AMT-P.                     
011410     MOVE NPF-L-NET  (FT-SUB)    TO  T-AMT-N.                     
011410     MOVE TOTF-L-NET (FT-SUB)    TO  T-AMT-T.                     
011410     MOVE SPACE-2                TO  P-CCSW.                      
011410                                                                  
011410     PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
011410                                                                  
011410     MOVE 'TOTAL A&H NET FEES'   TO  T-DESC.                      
011410     MOVE NFT-A-NET  (FT-SUB)    TO  T-AMT-P.                     
011410     MOVE NPF-A-NET  (FT-SUB)    TO  T-AMT-N.                     
011410     MOVE TOTF-A-NET (FT-SUB)    TO  T-AMT-T.                     
011410                                                                  
011410     PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
011410                                                                  
011410     MOVE 'TOTAL NET FEES'       TO  T-DESC.                      
011410     MOVE NFT-T-NET  (FT-SUB)    TO  T-AMT-P.                     
011410     MOVE NPF-T-NET  (FT-SUB)    TO  T-AMT-N.                     
011410     MOVE TOTF-T-NET (FT-SUB)    TO  T-AMT-T.                     
011410     MOVE 'C=A-B'                TO  T-EXPL.                      
011410                                                                  
011410     PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       

070714     if me-do-update
070714        compute hld-562-prem-tot =
070714           hld-562-prem-tot + nft-t-net (ft-sub)
070714        compute hld-562-comm-tot = hld-562-comm-tot +
070714           (NFT-T-COM (FT-SUB) - NFT-T-RCM (FT-SUB))
070714     end-if   

011410     MOVE T-DESC-COM-L           TO  T-DESC.                      
011410*    MOVE NFT-L-COM              TO  T-AMT-P.                     
011410     COMPUTE T-AMT-P = NFT-L-COM (FT-SUB)
011410        - NFT-L-RCM (FT-SUB)
011410*    MOVE NPF-L-COM              TO  T-AMT-N.                     
011410     COMPUTE T-AMT-N = NPF-L-COM (FT-SUB)
011410        - NPF-L-RCM (FT-SUB)
011410*    MOVE TOTF-L-COM             TO  T-AMT-T.                     
011410     COMPUTE T-AMT-T = TOTF-L-COM (FT-SUB)
011410        - TOTF-L-RCM (FT-SUB)
011410     MOVE SPACE-2                TO  P-CCSW.                      
011410                                                                  
011410     PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
011410                                                                  
011410     MOVE T-DESC-COM-A           TO  T-DESC.                      
011410*    MOVE NFT-A-COM              TO  T-AMT-P.                     
011410     COMPUTE T-AMT-P = NFT-A-COM (FT-SUB)
011410        - NFT-A-RCM (FT-SUB)
011410*    MOVE NPF-A-COM              TO  T-AMT-N.                     
011410     COMPUTE T-AMT-N = NPF-A-COM (FT-SUB)
011410        - NPF-A-RCM (FT-SUB)
011410*    MOVE TOTF-A-COM             TO  T-AMT-T.                     
011410     COMPUTE T-AMT-T = TOTF-A-COM (FT-SUB)
011410        - TOTF-A-RCM (FT-SUB)
011410                                                                  
011410     PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
011410                                                                  
011410     MOVE 'TOTAL COMPENSATION'   TO  T-DESC.                      
011410*    MOVE NFT-T-COM              TO  T-AMT-P.                     
011410     COMPUTE T-AMT-P = NFT-T-COM (FT-SUB)
011410        - NFT-T-RCM (FT-SUB)
011410*    MOVE NPF-T-COM              TO  T-AMT-N.                     
011410     COMPUTE T-AMT-N = NPF-T-COM (FT-SUB)
011410        - NPF-T-RCM (FT-SUB)
011410*    MOVE TOTF-T-COM             TO  T-AMT-T.                     
011410     COMPUTE T-AMT-T = TOTF-T-COM (FT-SUB)
011410        - TOTF-T-RCM (FT-SUB)
011410     MOVE 'D'                    TO  T-EXPL.                      
011410                                                                  
011410     PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
011410                                                                  
011410*    COMPUTE T-AMT-P  =  NFT-T-PRM  -  NFT-T-REF  -  NFT-T-COM.   
011410     COMPUTE T-AMT-P = NFT-T-PRM (FT-SUB)
011410        - NFT-T-REF (FT-SUB) -
011410        (NFT-T-COM (FT-SUB) - NFT-T-RCM (FT-SUB))
011410                                                                  
011410     MOVE 'AMOUNT DUE FOR THIS MONTH'  TO  T-DESC.                
011410     MOVE 'E=C-D'                      TO  T-EXPL.                
011410     MOVE SPACE-2                      TO  P-CCSW.                
011410                                                                  
011410     PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
011410                                                                  
011410     MOVE 'BALANCE BROUGHT FORWARD'  TO  T-DESC.                  
011410     MOVE NFT-B-BAL (FT-SUB)         TO  T-AMT-P.                 
011410     MOVE 'F'                        TO  T-EXPL.                  
011410     MOVE SPACE-2                    TO  P-CCSW.                  
011410                                                                  
011410     PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
011410                                                                  
011410     MOVE 'AMOUNT PAID'          TO  T-DESC.                      
011410     MOVE NFT-R-PMT  (FT-SUB)    TO  T-AMT-P.                     
011410     MOVE 'G'                    TO  T-EXPL.                      
011410     MOVE SPACE-2                TO  P-CCSW.                      
011410                                                                  
011410     PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
011410                                                                  
011410     MOVE 'AMOUNT CHARGED'       TO  T-DESC.                      
011410     MOVE NFT-C-CHG (FT-SUB)     TO  T-AMT-P.                     
011410     MOVE 'H'                    TO  T-EXPL.                      
011410     MOVE SPACE-2                TO  P-CCSW.                      
011410                                                                  
011410     PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
011410                                                                  
011410     MOVE 'WRITE OFF ADJUSTMENT'  TO  T-DESC.                     
011410     MOVE NFT-W-OFF (FT-SUB)      TO  T-AMT-P.                    
011410     MOVE 'I'                     TO  T-EXPL.                     
011410     MOVE SPACE-2                 TO  P-CCSW.                     
011410                                                                  
011410     PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
011410                                                                  
011410     MOVE ALL '-'                TO  T-AMTR-P.                    
011410     MOVE SPACE-2                TO  P-CCSW.                      
011410                                                                  
011410     PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
011410                                                                  
011410     MOVE 'BALANCE CARRIED FORWARD'  TO  T-DESC.                  
011410     MOVE NFT-E-BAL (FT-SUB)         TO  T-AMT-P.                 
011410     MOVE 'J=E+F-G+H+I'              TO  T-EXPL.                  
011410     MOVE SPACE-2                    TO  P-CCSW.                  
011410                                                                  
011410     PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       
011410                                                                  
011410*    IF T-TOT-COM NOT = T-ADJ-COM                                 
011410*        MOVE 'ADJUSTED COMMISSION'  TO  T-DESC                   
011410*        MOVE T-ADJ-COM              TO  T-AMT-P                  
011410*        MOVE SPACE-3                TO  P-CCSW                   
011410*        PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                   
011410
120804
120804     .
120804 9000-CONTINUE.
06977                                                                   
06978      IF DTE-CLIENT  =  'KSM'                                      
06979          CLOSE BILLING-DATA-FILE                                  
06980          MOVE 'Y'                TO  KSM-RECAP-PAGE-SWITCH        
06981          MOVE +0                 TO  PGCTR                        
06982          PERFORM 3910-PRT-FIRST-PAGE  THRU  3999-EXIT             
06983          MOVE 'TAPE DETAIL RECORDS        = '                     
06984                                  TO  P-KSM-DESC                   
06985          MOVE KSM-DETAIL-COUNT   TO  P-KSM-CNT                    
06986          MOVE SPACE-2            TO  P-CCSW                       
06987          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    
06988          MOVE 'TAPE SUMMARY RECORDS       = '                     
06989                                  TO  P-KSM-DESC                   
06990          MOVE KSM-COUNT          TO  P-KSM-CNT                    
06991          MOVE SPACE-2            TO  P-CCSW                       
06992          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    
06993          MOVE 'TOTAL LIFE WRITTEN PREMIUM = '                     
06994                                  TO  P-KSM-DESC                   
06995          MOVE KSM-GROSS-LIFE-PREM                                 
06996                                  TO  P-KSM-AMT                    
06997          MOVE SPACE-2            TO  P-CCSW                       
06998          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    
06999          MOVE 'TOTAL LIFE REFUNDS         = '                     
07000                                  TO  P-KSM-DESC                   
07001          MOVE KSM-GROSS-LIFE-REFUNDS                              
07002                                  TO  P-KSM-AMT                    
07003          MOVE SPACE-2            TO  P-CCSW                       
07004          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    
07005          MOVE 'TOTAL NET LIFE PREMIUM     = '                     
07006                                  TO  P-KSM-DESC                   
07007          MOVE KSM-NET-LIFE-PREM  TO  P-KSM-AMT                    
07008          MOVE SPACE-2            TO  P-CCSW                       
07009          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    
07010          MOVE 'TOTAL A&H WRITTEN PREMIUM  = '                     
07011                                  TO  P-KSM-DESC                   
07012          MOVE KSM-GROSS-AH-PREM  TO  P-KSM-AMT                    
07013          MOVE SPACE-2            TO  P-CCSW                       
07014          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    
07015          MOVE 'TOTAL A&H REFUNDS          = '                     
07016                                  TO  P-KSM-DESC                   
07017          MOVE KSM-GROSS-AH-REFUNDS                                
07018                                  TO  P-KSM-AMT                    
07019          MOVE SPACE-2            TO  P-CCSW                       
07020          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    
07021          MOVE 'TOTAL NET A&H  PREMIUM     = '                     
07022                                  TO  P-KSM-DESC                   
07023          MOVE KSM-NET-AH-PREM    TO  P-KSM-AMT                    
07024          MOVE SPACE-2            TO  P-CCSW                       
07025          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    
07026          MOVE 'TOTAL LIFE COMMISSION      = '                     
07027                                  TO  P-KSM-DESC                   
07028          MOVE KSM-LIFE-COMM      TO  P-KSM-AMT                    
07029          MOVE SPACE-2            TO  P-CCSW                       
07030          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    
07031          MOVE 'TOTAL A&H  COMMISSION      = '                     
07032                                  TO  P-KSM-DESC                   
07033          MOVE KSM-AH-COMM        TO  P-KSM-AMT                    
07034          MOVE SPACE-2            TO  P-CCSW                       
07035          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    
07036          MOVE 'TOTAL WRITTEN PREMIUM      = '                     
07037                                  TO  P-KSM-DESC                   
07038          MOVE KSM-GROSS-WRITTEN  TO  P-KSM-AMT                    
07039          MOVE SPACE-2            TO  P-CCSW                       
07040          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    
07041          MOVE 'TOTAL PREMIUM REFUNDED     = '                     
07042                                  TO  P-KSM-DESC                   
07043          MOVE KSM-GROSS-REFUNDED                                  
07044                                  TO  P-KSM-AMT                    
07045          MOVE SPACE-2            TO  P-CCSW                       
07046          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    
07047          MOVE 'TOTAL NET PREMIUM          = '                     
07048                                  TO  P-KSM-DESC                   
07049          MOVE KSM-TOTAL-PREM     TO  P-KSM-AMT                    
07050          MOVE SPACE-2            TO  P-CCSW                       
07051          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    
07052          MOVE 'TOTAL      COMMISSION      = '                     
07053                                  TO  P-KSM-DESC                   
07054          MOVE KSM-TOTAL-COMM     TO  P-KSM-AMT                    
07055          MOVE SPACE-2            TO  P-CCSW                       
07056          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    
07057          MOVE 'DUE THIS MONTH             = '                     
07058                                  TO  P-KSM-DESC                   
07059          MOVE KSM-TOTAL-DUE      TO  P-KSM-AMT                    
07060          MOVE SPACE-2            TO  P-CCSW                       
07061          PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                   
07062                                                                   
07063      CLOSE COMM-MSTR-IN   COMM-TRAN-IN   ERPNDE  ERNOTE           
07064            COMM-MSTR-OUT  SUMM-TRAN-OUT  ELERRS  ERCHEK           
092815           PRNTR ELCRTT.                                          
07066                                                                   
07067      IF ERPNDE-FILE-STATUS NOT = ZEROS                            
07068          MOVE 'ERROR OCCURED CLOSE - ERPNDE'                      
07069                                   TO   WS-ABEND-MESSAGE           
07070          MOVE ERPNDE-FILE-STATUS  TO   WS-ABEND-FILE-STATUS       
07071          GO TO ABEND-PGM.                                         
07072                                                                   
07073      IF EM-STAT-1 NOT = ZERO                                      
07074          MOVE EM-STATUS          TO   WS-ABEND-FILE-STATUS        
07075          MOVE 'ERROR OCCURED CLOSE - ELERRS'                      
07076                                  TO   WS-ABEND-MESSAGE            
07077          GO TO ABEND-PGM.                                         
07078                                                                   
07079      IF ERNOTE-FILE-STATUS NOT = ZEROS                            
07080          MOVE 'ERROR OCCURED CLOSE - ERNOTE'                      
07081                                   TO   WS-ABEND-MESSAGE           
07082          MOVE ERNOTE-FILE-STATUS  TO   WS-ABEND-FILE-STATUS       
07083          GO TO ABEND-PGM.                                         

092815     IF ELCRTT-FILE-STATUS NOT = ZEROS
092815        MOVE 'ERROR OCCURED CLOSE - ELCRTT'
092815                                 TO WS-ABEND-MESSAGE
092815        MOVE ELCRTT-FILE-STATUS  TO WS-ABEND-FILE-STATUS
092815        GO TO ABEND-PGM
092815     END-IF

07085      IF ERCHEK-FILE-STATUS NOT = ZEROS                            
07086          MOVE 'ERROR OCCURED CLOSE - ERCHEK'                      
07087                                   TO   WS-ABEND-MESSAGE           
07088          MOVE ERCHEK-FILE-STATUS  TO   WS-ABEND-FILE-STATUS       
07089          GO TO ABEND-PGM.                                         
CIDMOD*
CIDMOD*    IF DTE-CLIENT = 'CID'
CIDMOD*       CLOSE ERACCT
CIDMOD*       IF ERACCT-FILE-STATUS NOT = ZEROS                         
CIDMOD*           MOVE 'ERROR OCCURED CLOSE - ERACCT'                   
CIDMOD*                                    TO   WS-ABEND-MESSAGE        
CIDMOD*           MOVE ERACCT-FILE-STATUS  TO   WS-ABEND-FILE-STATUS    
CIDMOD*           GO TO ABEND-PGM.                                      
CIDMOD*                                                                 
070714     OPEN I-O ERMEBL.                                  
070714                                                       
070714     IF ERMEBL-FILE-STATUS <> ZERO and '97'
070714        MOVE 'N'                 TO ME-UPDATE-FLAG
070714        display ' me open ' ermebl-file-status
070714     end-if
070714     MOVE DTE-CLIENT             TO ME-COMPANY
070714                                                           
070714     COMPUTE MONTH-END-MOYR  =
070714        RUN-CCYY  *  12  +  RUN-MO
070714                                                           
070714     MOVE MONTH-END-MOYR         TO ME-MOYR
070714                                                           
070714     IF ME-DO-UPDATE                                       
070714        READ ERMEBL
070714        if ermebl-file-status <> '00'
070714           display ' me read ' ermebl-file-status
070714           MOVE 'N'              TO ME-UPDATE-FLAG    
070714           CLOSE ERMEBL
070714        end-if
070714     end-if

070714     IF ME-DO-UPDATE
070714        move hld-562-PREM-TOT    to me-562-PREM-TOT
070714        move hld-562-COMM-TOT    to me-562-COMM-TOT
070714        ACCEPT WS-TIME-OF-DAY FROM TIME
070714        REWRITE MONTH-END-BALANCES
070714        CLOSE ERMEBL
070714     end-if

           .
07091  9010-CLOSE-FICH.                                                 
07092                              COPY ELCPRTC.                        
07093                                                                   
07094  9099-EXIT.                                                       
07095      EXIT.                                                        
07096  EJECT                                                            
07097  9999-ABEND-PGM SECTION.                                          
07098      DISPLAY ABEND-CODE.                                          
07099      DISPLAY ABEND-CODE UPON CONSOLE.                             
07100                                                                   
07101  ABEND-PGM.                  COPY ELCABEND.                       
