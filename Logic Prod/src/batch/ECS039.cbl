00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                ECS039.                               
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 11/28/95 11:12:17.                 
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          
00008 *                           VMOD=2.014.                           
 0009                                                                   
00010 *AUTHOR.     LOGIC, INC.                                          
00011 *            DALLAS, TEXAS.                                       
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
00026 *        THIS PROGRAM CALCULATES A CLAIM RESERVE FOR DISABILITY   
00027 *        CLAIMS IT USES THE CLAIM HISTORY AS THE BASE FILE FOR    
00028 *        DETERMINING THE CLAIM RESERVE INFORMATION.               
00029 *                                                                 
00030 *        THIS PROGRAM ALSO GENERATES A DISK FILE TO ECS041 FOR    
00031 *        UPDATING THE EPEC WITH IBNR, PAID TO CURRENT, AND        
00032 *        CLAIM RESERVES.                                          
00033 *                                                                 
00034 *        THOSE CLIENTS USING THE OPTIONAL METHOD OF CALCULATING   
00035 *        CLAIMS RESERVES (OMCCR) AND THAT DON'T HAVE THE CLAIMS   
00036 *        SYSTEM, THIS PROGRAM SUPPLIES THE FUTURE AND PAID-TO-    
00037 *        CURRENT RESERVES BY CALLING THE OPTIONAL RESERVES        
00038 *        CALCULATOR, ELRSVSPL.  IBNR VALUES UNDER OMCCR ARE       
00039 *        PRODUCED BY ECS010 AND ARE CALCULATED AT THE ACCOUNT/    
00040 *        BENEFIT LEVEL.  THESE ARE READ INTO THE PROGRAM AND      
00041 *        SEPARATED FROM ALL OTHER TRANSACTIONS.                   
00042 *                                                                 
00043 *        UNDER OMCCR, ECS039 CREATES AN UNSORTED DETAIL RESERVES  
00044 *        FILE, USING THE RESERVES VALUES CALCULATED ABOVE.        
00045 *        SINCE THERE IS STILL A NEED TO UPDATE THE EPEC FILE      
00046 *        MOST OF THE STANDARD PROCESSING CONTINUED.  THE          
00047 *        REPORTS ARE KEPT WITH SLIGHT CHANGES TO REFLECT THE      
00048 *        DIFFERENT TABLES USED.  IBNR VALUES WILL APPEAR UNDER    
00049 *        THE CERT VALUE 'ACTBENIBNR' AND WILL BE USED TO CREATE   
00050 *        INDEPENDENT RECORDS FOR EPEC PROCESSING.                 
00051                                                                   
00052                                                                   
00053  ENVIRONMENT DIVISION.                                            
00054  CONFIGURATION SECTION.                                           
00055                                                                   
00056  INPUT-OUTPUT SECTION.                                            
00057  FILE-CONTROL.                                                    
00058                                                                   
00059      SELECT CLM-RSRV-SUMM   ASSIGN TO SYS002-UT-3380-S-SYS002.    
00060      SELECT CLAIM-HIST-IN   ASSIGN TO SYS018-UT-2400-S-SYS018.    
00061      SELECT CLM-RSRV-REPT   ASSIGN TO SYS008-UR-1403-S-SYS008.    
00062      SELECT DETAIL-EXTRACT-IN                                     
00063                             ASSIGN TO SYS021-UT-2400-S-SYS021.    
00064      SELECT DETAIL-RESERVES ASSIGN TO SYS022-UT-2400-S-SYS022.    
00065      SELECT DISK-DATE       ASSIGN TO SYS019-UT-3380-S-SYS019.    
00066      SELECT DISK-SORT       ASSIGN TO SYS001-UT-3380-S-SORTWK1.   
00067      SELECT FICH            ASSIGN TO SYS020-UT-2400-S-SYS020.    
00068      SELECT PEND-CLM-XTRACT ASSIGN TO SYS003-UT-3380-S-SYS003.    
00069      SELECT SORT-RESERVES   ASSIGN TO SYS001-UT-3380-S-SORTWK1.   
00070      SELECT SORTED-CLMS     ASSIGN TO SYS001-UT-3380-S-SORTWK1.   
00071      SELECT UNSRTD-HIST-IN  ASSIGN TO SYS001-UT-3380-S-SYS017.    
00072      SELECT UNSRTD-RESERVES ASSIGN TO SYS001-UT-3380-S-SYS016.    
00073  EJECT                                                            
00074  DATA DIVISION.                                                   
00075  FILE SECTION.                                                    
00076                                  EJECT                            
00077  FD  CLAIM-HIST-IN                                                
00078      BLOCK CONTAINS 0 RECORDS
00079      RECORDING MODE F.                                            
00080                                                                   
00081  01  IN-CLM-HIST.                                                 
00082      02  CH-DETAIL-FILE.                                          
00083      12  CH-RECORD-ID                      PIC XX.                
00084      12  CH-COMPANY-CD                     PIC X.                 
00085      12  CH-REIN                           PIC X.                 
00086                                                                   
00087      12  CH-CONTROL.                                              
00088          16  CH-CNTRL1.                                           
00089              20  CH-CARRIER                PIC X.                 
00090              20  CH-GROUPING.                                     
00091                  24  CH-GROUP-PREFIX       PIC XXX.               
00092                  24  CH-GROUP-PRIME        PIC XXX.               
00093              20  CH-STATE                  PIC XX.                
00094              20  CH-ACCOUNT.                                      
00095                  24  CH-ACCT-PREFIX        PIC X(4).              
00096                  24  CH-ACCT-PRIME         PIC X(6).              
00097          16  CH-CNTRL2.                                           
00098              20  CH-EFF                    PIC 9(11)  COMP-3.     
00099              20  CH-CERT.                                         
00100                  24 CH-CERT-NO.                                   
00101                      28  CH-CRT-PREFIX     PIC XXX.               
00102                      28  CH-CRT-PRIME      PIC X(7).              
00103                  24  CH-CRT-SUF            PIC X.                 
00104                                                                   
00105      12  CH-TRANS                          PIC X.                 
00106          88  CH-ISSUE                         VALUE 'I'.          
00107          88  CH-RC-ISSUE                      VALUE '8'.          
00108          88  CH-CANCEL                        VALUE 'C'.          
00109          88  CH-RC-CANCEL                     VALUE '7'.          
00110          88  CH-CLAIM                         VALUE 'X'.          
00111          88  CH-RESTORE                       VALUE 'R'.          
00112          88  CH-RESERVE                       VALUE 'Y'.          
00113          88  CH-RR-RC-ISS                     VALUE 'J'.          
00114          88  CH-RR-RC-CNC                     VALUE 'K'.          
00115          88  CH-RR-RC-CLM                     VALUE 'L'.          
00116                                                                   
00117      12  CH-REPORT-CODE-1                  PIC X(10).             
00118                                                                   
00119      12  CH-REPORT-CODE-2                  PIC X(10).             
00120                                                                   
00121      12  CH-NAME.                                                 
00122          16  CH-LNAME                      PIC X(15).             
00123          16  CH-FNAME.                                            
00124              20  CH-1ST-INIT-FNAME         PIC X.                 
00125              20  FILLER                    PIC X(9).              
00126          16  CH-INIT                       PIC X.                 
00127      12  CH-AGE                            PIC 99.                
00128      12  CH-SEX                            PIC X.                 
00129      12  CH-SOC-SEC-NO                     PIC X(11).             
00130                                                                   
00131      12  CH-LF-TYPE                        PIC XX.                
00132      12  CH-LF-TERM                        PIC 999        COMP-3. 
00133      12  CH-LF-BEN                         PIC S9(9)V99   COMP-3. 
00134      12  CH-LF-CNBEN                       PIC S9(9)V99   COMP-3. 
00135      12  CH-LF-PRM                         PIC S9(7)V99   COMP-3. 
00136      12  CH-LF-BEN-ALT                     PIC S9(9)V99   COMP-3. 
00137      12  CH-LF-CNBEN-ALT                   PIC S9(9)V99   COMP-3. 
00138      12  CH-LF-PRM-ALT                     PIC S9(7)V99   COMP-3. 
00139      12  CH-LF-RFND                        PIC S9(7)V99   COMP-3. 
00140                                                                   
00141      12  CH-LF-STAT-CDE                    PIC X.                 
00142      12  CH-LF-PREV-STAT                   PIC X.                 
00143      12  CH-LF-CANC-DTE                    PIC 9(11)      COMP-3. 
00144      12  CH-LF-CANC-EXIT-DT                PIC 9(11)      COMP-3. 
00145                                                                   
00146      12  CH-AH-TYPE                        PIC XX.                
00147      12  CH-AH-TERM                        PIC 999        COMP-3. 
00148      12  CH-AH-BEN                         PIC S9(7)V99   COMP-3. 
00149      12  CH-AH-PRM                         PIC S9(7)V99   COMP-3. 
00150      12  CH-AH-RFND                        PIC S9(7)V99   COMP-3. 
00151                                                                   
00152      12  CH-AH-STAT-CDE                    PIC X.                 
00153      12  CH-AH-PREV-STAT                   PIC X.                 
00154      12  CH-AH-CANC-DTE                    PIC 9(11)    COMP-3.   
00155      12  CH-AH-CANC-EXIT-DT                PIC 9(11)    COMP-3.   
00156                                                                   
00157      12  CH-LIVES                          PIC S999       COMP-3. 
00158      12  CH-CANCEL-CNT-ITD                 PIC S999       COMP-3. 
00159      12  CH-CANCEL-CNT-YTD                 PIC S999       COMP-3. 
00160                                                                   
00161      12  CH-APR                            PIC S999V9(4)  COMP-3. 
00162      12  CH-PMT-FREQ                       PIC 99.                
00163      12  CH-ACC-GPCD                       PIC XX.                
00164      12  CH-IG                             PIC X.                 
00165      12  CH-REMIT-TO                       PIC XX.                
00166      12  CH-MEMBER-NO                      PIC X(12).             
00167                                                                   
00168      12  FILLER                            PIC X(10).             
00169                                                                   
00170      12  CH-ENTRY-STATUS                   PIC X.                 
00171      12  CH-ENTRY-DTE                      PIC 9(11)    COMP-3.   
00172                                                                   
00173      12  CH-REI-AREA.                                             
00174          16  CH-REI-COMP.                                         
00175              20  CH-REINCO                 PIC XXX.               
00176              20  CH-REINCO-SUB             PIC XXX.               
00177          16  CH-REI-LFAMT                  PIC S9(9)V99   COMP-3. 
00178          16  CH-REI-LFPRM                  PIC S9(7)V99   COMP-3. 
00179          16  CH-REI-LFRFND                 PIC S9(7)V99   COMP-3. 
00180          16  CH-REI-AHAMT                  PIC S9(7)V99   COMP-3. 
00181          16  CH-REI-AHPRM                  PIC S9(7)V99   COMP-3. 
00182          16  CH-REI-AHRFND                 PIC S9(7)V99   COMP-3. 
00183          16  CH-REI-CNAMT                  PIC S9(9)V99   COMP-3. 
00184                                                                   
00185      12  FILLER                            PIC X(5).              
00186                                                                   
00187      12  CH-BILLED-AREA.                                          
00188          16  CH-GL-INTERFACE-CODES.                               
00189              20  CH-GL-CODES               PIC X  OCCURS 10 TIMES.
00190                                                                   
00191          16  CH-BILL-SW                    PIC X.                 
00192              88  CH-RECORD-ON-HOLD            VALUE 'H'.          
00193              88  CH-RECORD-RETURNED           VALUE 'R'.          
00194              88  CH-RECORD-ENDORSED           VALUE 'E'.          
00195              88  CH-OVERRIDE-LIFE             VALUE 'L'.          
00196              88  CH-OVERRIDE-AH               VALUE 'A'.          
00197              88  CH-OVERRIDE-BOTH             VALUE 'B'.          
00198                                                                   
00199          16  CH-REFUND-SW                  PIC X.                 
00200              88  CH-REFUND-CREATED            VALUE 'Y'.          
00201                                                                   
00202          16  CH-BILLED-LFPRM               PIC S9(7)V99   COMP-3. 
00203          16  CH-BILLED-LFRFND              PIC S9(7)V99   COMP-3. 
00204          16  CH-BILLED-AHPRM               PIC S9(7)V99   COMP-3. 
00205          16  CH-BILLED-AHRFND              PIC S9(7)V99   COMP-3. 
00206                                                                   
00207          16  CH-GA-BILL-STATUS             PIC X  OCCURS 5 TIMES. 
00208 *            88  CH-NO-PRE-BILL-GA            VALUE ' '.          
00209 *            88  CH-BILLED-GA                 VALUE 'B'.          
00210                                                                   
00211      12  CH-BILL-STATUS                    PIC X.                 
00212          88  CH-NO-PRE-BILL                   VALUE ' '.          
00213          88  CH-BILLED                        VALUE 'B'.          
00214          88  CH-REVERSE                       VALUE 'R'.          
00215                                                                   
00216      12  CH-REVERSE-REASONS.                                      
00217          16  CH-REVERSE-REASON-1           PIC X.                 
00218          16  CH-REVERSE-REASON-2           PIC X.                 
00219          16  CH-REVERSE-REASON-3           PIC X.                 
00220                                                                   
00221      12  CH-RECALC-CODE                    PIC X.                 
00222      12  CH-RECALC-TYPE                    PIC X.                 
00223                                                                   
00224      12  CH-CHARGEBACK-CODES.                                     
00225          16  CH-LF-COMM-CHARGEBACK         PIC X.                 
00226              88  CH-NO-LF-CHARGEBACK          VALUE 'N'.          
00227          16  CH-AH-COMM-CHARGEBACK         PIC X.                 
00228              88  CH-NO-AH-CHARGEBACK          VALUE 'N'.          
00229                                                                   
00230      12  FILLER                            PIC X(4).              
00231                                                                   
00232      12  CH-COMM-LEVELS.                                          
00233          16  CH-AGT-LEVELS     OCCURS  10  TIMES.                 
00234              20  CH-AGT.                                          
00235                  24  CH-AGT-PREFIX         PIC X(4).              
00236                  24  CH-AGT-PRIME          PIC X(6).              
00237              20  CH-AGT-TYPE               PIC X.                 
00238              20  CH-L-PC                   PIC SV9(5)     COMP-3. 
00239              20  CH-A-PC                   PIC SV9(5)     COMP-3. 
00240                                                                   
00241      12  CH-PROC-DT                        PIC 9(11)      COMP-3. 
00242 ****************************************************************  
00243 *******                 CLAIM EXTRACT                      *****  
00244 ****************************************************************  
00245      02 CH-CLAIM-EXTRACT     REDEFINES     CH-DETAIL-FILE.        
00246      12  FILLER                            PIC X(334).            
00247      12  CH-TYPE                           PIC X.                 
00248          88  CH-DTH                           VALUE '1'.          
00249          88  CH-AH                            VALUE '2'.          
00250          88  CH-OB-DTH                        VALUE '3'.          
00251          88  CH-OB-AH                         VALUE '4'.          
00252          88  CH-DEATH                         VALUES '1' '3'.     
00253          88  CH-DISABILITY                    VALUES '2' '4'.     
00254                                                                   
00255      12  CH-CLAIM-AMT                      PIC S9(9)V99   COMP-3. 
00256      12  CH-REI-CLAIM-AMT                  PIC S9(9)V99   COMP-3. 
00257                                                                   
00258      12  CH-INCUR                          PIC 9(11)    COMP-3.   
00259      12  CH-REPORTED.                                             
00260          16  CH-RPT-YR                     PIC 99.                
00261          16  CH-RPT-MO                     PIC 99.                
00262          16  CH-RPT-DA                     PIC 99.                
00263      12  CH-PAY                            PIC 9(11)    COMP-3.   
00264      12  CH-PAID-TO                        PIC 9(11)    COMP-3.   
00265                                                                   
00266      12  CH-CNUM                           PIC X(7).              
00267      12  CH-CHECK                          PIC X(7).              
00268      12  CH-PMT-TRAILER-SEQ                PIC S9(4)       COMP.  
00269      12  CH-DAYS-DISAB                     PIC 999.               
00270      12  CH-CLM-AGE                        PIC 99.                
00271      12  CH-PAY-CODE                       PIC X.                 
00272      12  CH-CLM-ERR                        PIC XX.                
00273                                                                   
00274      12  CH-ACC-NAME                       PIC X(30).             
00275      12  CH-ACC-EXP-DTE                    PIC 9(11)    COMP-3.   
00276      12  CH-ACC-EFF-DTE                    PIC 9(11)    COMP-3.   
00277      12  CH-CLM-CAUSE                      PIC XX.                
00278      12  CH-LOAN-OFFICER                   PIC XXX.               
00279                                                                   
00280      12  FILLER                            PIC X(62).             
00281                                                                   
00282      12  CH-CLM-PROC-DT                    PIC 9(11)    COMP-3.   
00283                                  EJECT                            
00284  FD  CLM-RSRV-SUMM                                                
00285      BLOCK CONTAINS 0 RECORDS
00286      RECORDING MODE F.                                            
00287                                                                   
00288  01  C-R-OUT.                                                     
00289      12  C-R-COMPANY-CD      PIC X.                               
00290      12  C-R-COMP.                                                
00291          16  C-R-CARR        PIC  X.                              
00292          16  C-R-CMP         PIC  X(6).                           
00293      12  C-R-ST              PIC  XX.                             
00294      12  C-R-ACCT            PIC  X(10).                          
00295      12  C-R-E-DTE           PIC  9(11)      COMP-3.              
00296      12  C-R-LAH             PIC  X.                              
00297      12  C-R-BEN             PIC  XX.                             
00298      12  C-R-REI-CMP         PIC  X(6).                           
00299      12  C-R-CARD-NO         PIC  X.                              
00300      12  C-R-RESV            PIC S9(9)V99    COMP-3.              
00301      12  C-R-REM-AMT         PIC S9(9)V99    COMP-3.              
00302      12  C-R-FUT-RESERVE     PIC S9(7)V99    COMP-3.              
00303      12  C-R-PTC-RESERVE     PIC S9(7)V99    COMP-3.              
00304      12  C-R-IBNR-RESERVE    PIC S9(7)V99    COMP-3.              
00305      12  C-R-CLM-ADJ-AMT     PIC S9(7)V99    COMP-3.              
00306      12  C-R-EXPENSES        PIC S9(7)V99    COMP-3.              
00307      12  C-R-PAYMENTS        PIC S9(7)V99    COMP-3.              
00308      12  C-R-OTH-COMMISSIONS PIC S9(7)V99    COMP-3.              
00309      12  C-R-REIN-PREM-ADJS  PIC S9(7)V99    COMP-3.              
00310      12  C-R-DATE            PIC  9(07)   COMP-3.                 
00311                              EJECT                                
00312  FD  CLM-RSRV-REPT                                                
00313                              COPY ELCPRTFD.                       
00314                              EJECT                                
00315  FD  DETAIL-EXTRACT-IN                                            
00316      BLOCK CONTAINS 0 RECORDS
00317      RECORDING MODE F.                                            
00318                                                                   
00319  01  DETAIL-EXT-IN-RCRD          PIC  X(510).                     
00320                                  EJECT                            
00321  FD  DETAIL-RESERVES                                              
00322      BLOCK CONTAINS 0 RECORDS
00323      RECORDING MODE F.                                            
00324                                                                   
00325  01  DETAIL-RESERVES-EXTRACT     PIC X(510).                      
00326                                  EJECT                            
00327  FD  DISK-DATE                                                    
00328                                  COPY ELCDTEFD.                   
00329                                  EJECT                            
00330                                                                   
00331  SD  DISK-SORT.                                                   
00332                                                                   
00333  01  SORT-WORK.                                                   
00334      12  SW-C                PIC  X(7).                           
00335      12  FILLER              PIC  X.                              
00336      12  SW-CC               PIC  X(11).                          
00337      12  FILLER              PIC  X(13).                          
00338      12  SW-B                PIC  X(10).                          
00339      12  FILLER              PIC  X(44).                          
00340      12  SW-A                PIC  XX.                             
00341      12  FILLER              PIC  X(28).                          
00342      12  SW-E                PIC  9(11)  COMP-3.                  
00343 *    12  SW-D                PIC  XX.                             
00344      12  FILLER              PIC  X(110).                         
00345      12  SW-0.                                                    
00346          16  SW0-CARR        PIC  X.                              
00347          16  SW0-CMP         PIC  X(6).                           
00348      12  SW-00               PIC  X(6).                           
00349      12  FILLER              PIC  XX.                             
00350      12  SW-BB               PIC  9(11)  COMP-3.                  
00351      12  FILLER              PIC  X(20).                          
00352                                                                   
00353  FD  FICH                                                         
00354                                  COPY ECSFICH.                    
00355                                  EJECT                            
00356  FD  PEND-CLM-XTRACT                                              
00357      BLOCK CONTAINS 0 RECORDS
00358      RECORDING MODE F.                                            
00359                                                                   
00360  01  XTR-OUT                 PIC  X(273).                         
00361                                  EJECT                            
00362  SD  SORT-RESERVES.                                               
00363                                                                   
00364  01  SORT-RSRV-REC.                                               
00365      12  FILLER              PIC  X(04).                          
00366      12  SORT-KEY-R1.                                             
00367          16  SKR1-CARRIER    PIC  X(01).                          
00368          16  SKR1-GROUPING   PIC  X(06).                          
00369          16  SKR1-STATE      PIC  X(02).                          
00370          16  SKR1-ACCOUNT    PIC  X(10).                          
00371          16  SKR1-EFF-DT     PIC  9(11)   COMP-3.                 
00372          16  SKR1-CERT       PIC  X(11).                          
00373      12  FILLER              PIC X(470).                          
00374                              EJECT                                
00375                                  EJECT                            
00376  SD  SORTED-CLMS.                                                 
00377                                                                   
00378  01  SORTED-CLMS-REC.                                             
00379      12  FILLER              PIC  X(04).                          
00380      12  SORT-KEY-2.                                              
00381 **********COLUMN 5-29****************                             
00382          16  SORT-CARRIER    PIC  X(01).                          
00383          16  SORT-GROUPING   PIC  X(06).                          
00384          16  SORT-STATE      PIC  X(02).                          
00385          16  SORT-ACCOUNT    PIC  X(10).                          
00386          16  SORT-EFF-DT     PIC  9(11)       COMP-3.             
00387 **********COLUMN 39-40  CERT ********                             
00388      12  SORT-KEY-4          PIC  X(11).                          
00389      12  FILLER              PIC X(197).                          
00390 **********COLUMN 238-243 COMP/REI****                             
00391      12  SORT-KEY-1          PIC  X(06).                          
00392      12  FILLER              PIC X(116).                          
00393 **********COLUMN 360-365 PAY ********                             
00394      12  SORT-KEY-5          PIC  9(11)  COMP-3.                  
00395 **********COLUMN 366-371 PAID TO ****                             
00396      12  SORT-KEY-6          PIC  9(11)  COMP-3.                  
00397 **********COLUMN 372-378 CNUM *******                             
00398      12  SORT-KEY-3          PIC  X(07).                          
00399      12  FILLER              PIC X(132).                          
00400                              EJECT                                
00401  FD  UNSRTD-HIST-IN                                               
00402      BLOCK CONTAINS 0 RECORDS
00403      RECORDING MODE F.                                            
00404                                                                   
00405  01  UNSRTD-REC-IN           PIC  X(510).                         
00406                              EJECT                                
00407  FD  UNSRTD-RESERVES                                              
00408      BLOCK CONTAINS 0 RECORDS
00409      RECORDING MODE F.                                            
00410                                                                   
00411  01  UNSRTD-RSRV-REC.                                             
00412      12  FILLER                            PIC X(334).            
00413                                                                   
00414      12  UR-RESERVE-TYPE                   PIC X.                 
00415          88  UR-LIFE-RSV                      VALUE '1' '3'.      
00416          88  UR-AH-RSV                        VALUE '2' '4'.      
00417                                                                   
00418      12  UR-IBNR                           PIC S9(7)V99   COMP-3. 
00419      12  UR-PAYCUR                         PIC S9(7)V99   COMP-3. 
00420      12  UR-FUTRSV                         PIC S9(7)V99   COMP-3. 
00421                                                                   
00422      12  UR-CLMNO                          PIC X(7).              
00423                                                                   
00424      12  UR-RSV-INCUR                      PIC 9(11)      COMP-3. 
00425      12  UR-RSV-REPORTED.                                         
00426          16  UR-RSV-RPT-YR                 PIC 99.                
00427          16  UR-RSV-RPT-MO                 PIC 99.                
00428          16  UR-RSV-RPT-DA                 PIC 99.                
00429      12  UR-RSV-PAYTO                      PIC 9(11)      COMP-3. 
00430                                                                   
00431      12  UR-REI-IBNR                       PIC S9(7)V99   COMP-3. 
00432      12  UR-REI-PAYCUR                     PIC S9(7)V99   COMP-3. 
00433      12  UR-REI-FUTRSV                     PIC S9(7)V99   COMP-3. 
00434                                                                   
00435      12  UR-RSV-ACC-NAME                   PIC X(30).             
00436      12  UR-ACC-EXP-DTE-RSV                PIC 9(11)      COMP-3. 
00437      12  UR-ACC-EFF-DTE-RSV                PIC 9(11)      COMP-3. 
00438                                                                   
00439      12  FILLER                            PIC X(72).             
00440                                                                   
00441      12  UR-RSV-PROC-DT                    PIC 9(11)  COMP-3.     
00442  EJECT                                                            
00443  WORKING-STORAGE SECTION.                                         
00444  77  FILLER  PIC X(32) VALUE '********************************'.  
00445  77  FILLER  PIC X(32) VALUE '     ECS039 WORKING STORAGE     '.  
00446  77  FILLER  PIC X(32) VALUE '*****VMOD=2.014*****************'.  
00447                                                                   
00448  77  PGM-SUB                 PIC S9(3)   COMP    VALUE +039.      
00449  77  CX                      PIC S9(3)   COMP.                    
00450  77  CY                      PIC S9(3)   COMP.                    
00451  77  CX-L                    PIC S9(3)   COMP.                    
00452  77  CX-H                    PIC S9(3)   COMP.                    
00453  77  CY-L                    PIC S9(3)   COMP.                    
00454  77  CY-H                    PIC S9(3)   COMP.                    
00455  77  X1                      PIC S9(4)   COMP.                    
00456  77  Y1                      PIC S9(4)   COMP.                    
00457  77  DISAB-AGE               PIC S9(3)           COMP-3.          
00458  77  Y-LOC                   PIC S9(3)           COMP-3.          
00459  77  LINER                   PIC S9(3)   COMP-3  VALUE +80.       
00460  77  EXPAND-FACTOR           PIC S9V99   COMP-3  VALUE +1.15.     
00461  77  CX-X                    PIC S9(3)V9         COMP-3.          
00462  77  PAGER                   PIC S9(5)   COMP-3  VALUE +1.        
00463  77  M60-COUNT               PIC S9(5)   COMP-3  VALUE +0.        
00464  77  LQTR-COUNT              PIC S9(5)   COMP-3  VALUE +0.        
00465  77  END-COUNT               PIC S9(5)   COMP-3  VALUE +0.        
00466  77  W-IBNR-COUNT            PIC S9(5)   COMP-3  VALUE +0.        
00467  77  TD2-COUNTS              PIC S9(5)   COMP-3  VALUE +0.        
00468  77  TD2-CNT-IBNR            PIC S9(5)   COMP-3  VALUE +0.        
00469  77  TD2-CNT-IBNR3           PIC S9(5)   COMP-3  VALUE +0.        
00470  77  C-TD2-COUNTS            PIC S9(5)   COMP-3  VALUE +0.        
00471  77  F-TD2-COUNTS            PIC S9(5)   COMP-3  VALUE +0.        
00472  77  RE-TOT                  PIC S9(5)   COMP-3  VALUE +0.        
00473  77  TOT-BEN                 PIC S9(5)V99        COMP-3.          
00474  77  CX-D                    PIC S9(3)V9(5)      COMP-3.          
00475  77  CY-D                    PIC S9(3)V9(5)      COMP-3.          
00476  77  CY-Y                    PIC S9(3)V9(5)      COMP-3.          
00477  77  RSRV-FACT               PIC S9(5)V9(6)      COMP-3.          
00478  77  FACT-A                  PIC S9(5)V9(6)      COMP-3.          
00479  77  FACT-B                  PIC S9(5)V9(6)      COMP-3.          
00480  77  INTER-RESULT            PIC S9(5)V9(6) COMP-3 VALUE +0.      
00481  77  A-TOT-BEN               PIC S9(9)V99 COMP-3 VALUE +0.        
00482  77  A-TOT-IBNR              PIC S9(9)V99 COMP-3 VALUE +0.        
00483  77  A-TOT-RSRV              PIC S9(9)V99 COMP-3 VALUE +0.        
00484  77  C-TOT-BEN               PIC S9(9)V99 COMP-3 VALUE +0.        
00485  77  C-TOT-IBNR              PIC S9(9)V99 COMP-3 VALUE +0.        
00486  77  C-TOT-RSRV              PIC S9(9)V99 COMP-3 VALUE +0.        
00487  77  F-TOT-BEN               PIC S9(9)V99 COMP-3 VALUE +0.        
00488  77  F-TOT-IBNR              PIC S9(9)V99 COMP-3 VALUE +0.        
00489  77  F-TOT-RSRV              PIC S9(9)V99 COMP-3 VALUE +0.        
00490  77  M60-RSRV                PIC S9(9)V99 COMP-3 VALUE +0.        
00491  77  LQTR-MOBEN              PIC S9(9)V99 COMP-3 VALUE +0.        
00492  77  TD2-MOBEN               PIC S9(9)V99 COMP-3 VALUE +0.        
00493  77  TD2-RSRV                PIC S9(9)V99 COMP-3 VALUE +0.        
00494  77  TD2-IBNR                PIC S9(9)V99 COMP-3 VALUE +0.        
00495  77  TD2-PTC                 PIC S9(9)V99 COMP-3 VALUE +0.        
00496  77  C-TD2-MOBEN             PIC S9(9)V99 COMP-3 VALUE +0.        
00497  77  C-TD2-RSRV              PIC S9(9)V99 COMP-3 VALUE +0.        
00498  77  C-TD2-IBNR              PIC S9(9)V99 COMP-3 VALUE +0.        
00499  77  C-TD2-PTC               PIC S9(9)V99 COMP-3 VALUE +0.        
00500  77  F-TD2-MOBEN             PIC S9(9)V99 COMP-3 VALUE +0.        
00501  77  F-TD2-RSRV              PIC S9(9)V99 COMP-3 VALUE +0.        
00502  77  F-TD2-IBNR              PIC S9(9)V99 COMP-3 VALUE +0.        
00503  77  F-TD2-PTC               PIC S9(9)V99 COMP-3 VALUE +0.        
00504  77  FD3-S-RSRV              PIC S9(9)V99 COMP-3 VALUE +0.        
00505  77  FD3-Y-RSRV              PIC S9(9)V99 COMP-3 VALUE +0.        
00506  77  CATCH-UP                PIC S9(9)V99 COMP-3 VALUE +0.        
00507  77  W-AH-DE-COUNT           PIC S9(11)    COMP-3 VALUE +0.       
00508  77  W-AHR-DE-COUNT          PIC S9(11)    COMP-3 VALUE +0.       
00509  77  W-AH-FUTURE-TOTALS      PIC S9(11)V99 COMP-3 VALUE +0.       
00510  77  W-AH-IBNR-COUNT         PIC S9(11)    COMP-3 VALUE +0.       
00511  77  W-AHR-IBNR-COUNT        PIC S9(11)    COMP-3 VALUE +0.       
00512  77  W-AH-IBNR-TOTALS        PIC S9(11)V99 COMP-3 VALUE +0.       
00513  77  W-AH-PTC-TOTALS         PIC S9(11)V99 COMP-3 VALUE +0.       
00514  77  W-AHR-FUTURE-TOTALS     PIC S9(11)V99 COMP-3 VALUE +0.       
00515  77  W-AHR-IBNR-TOTALS       PIC S9(11)V99 COMP-3 VALUE +0.       
00516  77  W-AHR-PTC-TOTALS        PIC S9(11)V99 COMP-3 VALUE +0.       
00517  77  W-LF-DE-COUNT           PIC S9(11)    COMP-3 VALUE +0.       
00518  77  W-LFR-DE-COUNT          PIC S9(11)    COMP-3 VALUE +0.       
00519  77  W-LF-FUTURE-TOTALS      PIC S9(11)V99 COMP-3 VALUE +0.       
00520  77  W-LF-IBNR-COUNT         PIC S9(11)    COMP-3 VALUE +0.       
00521  77  W-LFR-IBNR-COUNT        PIC S9(11)    COMP-3 VALUE +0.       
00522  77  W-LF-IBNR-TOTALS        PIC S9(11)V99 COMP-3 VALUE +0.       
00523  77  W-LF-PTC-TOTALS         PIC S9(11)V99 COMP-3 VALUE +0.       
00524  77  W-LFR-FUTURE-TOTALS     PIC S9(11)V99 COMP-3 VALUE +0.       
00525  77  W-LFR-IBNR-TOTALS       PIC S9(11)V99 COMP-3 VALUE +0.       
00526  77  W-LFR-PTC-TOTALS        PIC S9(11)V99 COMP-3 VALUE +0.       
00527  77  W-PENDING-RCRD-COUNT    PIC S9(11)    COMP-3 VALUE +0.       
00528  77  W-REJECTED-BY-RSRV      PIC S9(11)    COMP-3 VALUE +0.       
00529  77  W-RELEASED-RCRD-COUNT   PIC S9(11)    COMP-3 VALUE +0.       
00530  77  W-REJECTED-BY-DATE      PIC S9(11)    COMP-3 VALUE +0.       
00531  77  W-REJECTED-BY-STATUS    PIC S9(11)    COMP-3 VALUE +0.       
00532  77  W-REJECTED-BY-TYPE      PIC S9(11)    COMP-3 VALUE +0.       
00533  77  INTER-SW                PIC  X              VALUE SPACES.    
00534  77  X                       PIC  X              VALUE SPACES.    
00535  77  SET-ABORT               PIC  X              VALUE SPACES.    
00536  77  REL-CHECK               PIC  X              VALUE SPACES.    
00537  77  W-TBL                   PIC S99             VALUE ZERO.      
00538  77  SV-ACC-CO               PIC  X(4)           VALUE SPACES.    
00539  77  ACCT-CO                 PIC  X(4)           VALUE SPACES.    
00540  77  CHK-COMP                PIC  X(7)           VALUE SPACES.    
00541  77  LAS-CHK-COMP            PIC  X(7)           VALUE SPACES.    
00542  77  SV-EXP-DT               PIC  X(6)           VALUE SPACES.    
00543  77  SV-EFF-DT               PIC  X(6)           VALUE SPACES.    
00544  77  ACCT-NAME               PIC  X(30)          VALUE SPACES.    
00545                                                                   
00546  01  MISC-SWITCH-AREA.                                            
00547      12  FILLER                 PIC X(12)                         
00548                                 VALUE 'RETURN CODE:'.             
00549      12  WS-RETURN-CODE         PIC S9(4)   COMP.                 
00550      12  WS-ABEND-CODE          PIC  9(4).                        
00551      12  FILLER REDEFINES WS-ABEND-CODE.                          
00552          16  WS-AC-1-2.                                           
00553              20  WS-AC-1        PIC X(01).                        
00554              20  WS-AC-2        PIC X(01).                        
00555          16  WS-AC-3            PIC X(01).                        
00556          16  WS-AC-4            PIC X(01).                        
00557      12  WS-ABEND-MESSAGE.                                        
00558          16  FILLER             PIC X(13).                        
00559          16  WS-AM-CODE         PIC X(02).                        
00560          16  FILLER             PIC X(65).                        
00561      12  WS-ABEND-FILE-STATUS   PIC XX.                           
00562      12  WS-ZERO                PIC S9       COMP-3 VALUE +0.     
00563      12  INPUT-TRANSACTION-SWITCH    PIC  9.                      
00564          88  NO-CLAIMS                           VALUE 0.         
00565          88  NO-OPEN-CLAIMS                      VALUE 1.         
00566          88  HAVE-OPEN-CLAIMS                    VALUE 2.         
00567      12  WS-W-CRT-EFF.                                            
00568          16  FILLER          PIC  9(03).                          
00569          16  WCE-CCYY        PIC  9(04).                          
00570          16  WCE-CCYR REDEFINES WCE-CCYY.                         
00571              20  WCE-CC      PIC  99.                             
00572              20  WCE-YR      PIC  99.                             
00573          16  WCE-MO          PIC  99.                             
00574          16  WCE-DA          PIC  99.                             
00575      12  WS-W-CRT-EFF-N REDEFINES                                 
00576            WS-W-CRT-EFF      PIC 9(11).                           
00577      12  WS-W-RCVD.                                               
00578          16  FILLER          PIC  9(03).                          
00579          16  WRV-CCYY        PIC  9(04).                          
00580          16  WRV-CCYR REDEFINES WRV-CCYY.                         
00581              20  WRV-CC      PIC  99.                             
00582              20  WRV-YR      PIC  99.                             
00583          16  WRV-MO          PIC  99.                             
00584          16  WRV-DA          PIC  99.                             
00585      12  WS-W-RCVD-N REDEFINES                                    
00586             WS-W-RCVD        PIC 9(11).                           
00587      12  WS-W-INCUR.                                              
00588          16  FILLER          PIC  9(03).                          
00589          16  WIN-CCYY        PIC  9(04).                          
00590          16  WIN-CCYR REDEFINES WIN-CCYY.                         
00591              20  WIN-CC      PIC  99.                             
00592              20  WIN-YR      PIC  99.                             
00593          16  WIN-MO          PIC  99.                             
00594          16  WIN-DA          PIC  99.                             
00595      12  WS-W-INCUR-N REDEFINES                                   
00596             WS-W-INCUR       PIC  9(11).                          
00597      12  WS-W-ENTER.                                              
00598          16  FILLER          PIC  9(03).                          
00599          16  WEN-CCYY        PIC  9(04).                          
00600          16  WEN-CCYR REDEFINES WEN-CCYY.                         
00601              20  WEN-CC      PIC  99.                             
00602              20  WEN-YR      PIC  99.                             
00603          16  WEN-MO          PIC  99.                             
00604          16  WEN-DA          PIC  99.                             
00605      12  WS-W-ENTER-N REDEFINES                                   
00606             WS-W-ENTER       PIC  9(11).                          
00607      12  WS-W-PAY-TO.                                             
00608          16  FILLER          PIC  9(03).                          
00609          16  WPT-CCYY        PIC  9(04).                          
00610          16  WPT-CCYR REDEFINES WPT-CCYY.                         
00611              20  WPT-CC      PIC  99.                             
00612              20  WPT-YR      PIC  99.                             
00613          16  WPT-MO          PIC  99.                             
00614          16  WPT-DA          PIC  99.                             
00615      12  WS-W-PAY-TO-N REDEFINES                                  
00616             WS-W-PAY-TO      PIC  9(11).                          
00617      12  WS-W-LST-MAINT.                                          
00618          16  FILLER          PIC  9(03).                          
00619          16  WLM-CCYY        PIC  9(04).                          
00620          16  WLM-CCYR REDEFINES WLM-CCYY.                         
00621              20  WLM-CC      PIC  99.                             
00622              20  WLM-YR      PIC  99.                             
00623          16  WLM-MO          PIC  99.                             
00624          16  WLM-DA          PIC  99.                             
00625      12  WS-W-LST-MAINT-N REDEFINES                               
00626             WS-W-LST-MAINT   PIC  9(11).                          
00627      12  WS-W-1ST-PAY.                                            
00628          16  FILLER          PIC  9(03).                          
00629          16  W1P-CCYY        PIC  9(04).                          
00630          16  W1P-CCYR REDEFINES W1P-CCYY.                         
00631              20  W1P-CC      PIC  99.                             
00632              20  W1P-YR      PIC  99.                             
00633          16  W1P-MO          PIC  99.                             
00634          16  W1P-DA          PIC  99.                             
00635      12  WS-W-1ST-PAY-N REDEFINES                                 
00636             WS-W-1ST-PAY     PIC 9(11).                           
00637      12  WS-C-R-DATE.                                             
00638          16  FILLER          PIC 9.                               
00639          16  C-R-CC          PIC 99.                              
00640          16  C-R-YR          PIC 99.                              
00641          16  C-R-MO          PIC 99.                              
00642      12  WS-C-R-DATE-N REDEFINES                                  
00643             WS-C-R-DATE      PIC 9(07).                           
00644      12  WS-W-ACC-EXP.                                            
00645          16  FILLER          PIC 9(03).                           
00646          16  W-A-E-CC        PIC 99.                              
00647          16  W-A-E-YR        PIC 99.                              
00648          16  W-A-E-MO        PIC 99.                              
00649          16  W-A-E-DA        PIC 99.                              
00650      12  WS-W-ACC-EXP-N REDEFINES                                 
00651             WS-W-ACC-EXP     PIC 9(11).                           
00652  EJECT                                                            
00653  01  OTHER-STUFF.                                                 
00654      12  MISC-DTES.                                               
00655          16  VAL-DT          PIC S9(5)   COMP-3  VALUE +0.        
00656          16  SV-VAL          PIC S9(5)   COMP-3  VALUE +0.        
00657          16  VAL-RV3         PIC S9(5)   COMP-3  VALUE +0.        
00658          16  LMT-DT          PIC S9(5)   COMP-3  VALUE +0.        
00659          16  INC-DT          PIC S9(5)   COMP-3  VALUE +0.        
00660          16  RPT-DT          PIC S9(5)   COMP-3  VALUE +0.        
00661          16  EFF-DT          PIC S9(5)   COMP-3  VALUE +0.        
00662          16  EXP-DT          PIC S9(5)   COMP-3  VALUE +0.        
00663          16  WRK-DT          PIC S9(5)   COMP-3  VALUE +0.        
00664          16  ENT-DT          PIC S9(5)   COMP-3  VALUE +0.        
00665          16  PAYTO-DT        PIC S9(5)   COMP-3  VALUE +0.        
00666          16  PAY1-DT         PIC S9(5)   COMP-3  VALUE +0.        
00667      12  A-TERM.                                                  
00668          16  N-TRM           PIC  9(3).                           
00669      12  X-CNTRL.                                                 
00670          16  XC-COMP.                                             
00671              20  XC-CARR     PIC  X.                              
00672              20  XC-CMP      PIC  X(6).                           
00673          16  XC-ST           PIC  XX.                             
00674          16  XC-ACCT         PIC  X(10).                          
00675      12  CHIST-CNTRL.                                             
00676          16  CH-C0           PIC  X(6)           VALUE SPACES.    
00677          16  CH-C1           PIC  X(19)          VALUE SPACES.    
00678          16  CH-C2           PIC  X(7)           VALUE SPACES.    
00679          16  CH-C3           PIC  X(11)          VALUE SPACES.    
00680      12  LAS-CHIST           PIC  X(43)          VALUE SPACES.    
00681                                                                   
00682  01  W-OPTIONAL-WORK-FIELDS.                                      
00683      12  FILLER                  PIC  X(18)                       
00684                                  VALUE 'OPTIONAL WORK AREA'.      
00685      12  W-BEN-MAX               PIC S9(04) COMP   VALUE +0.      
00686      12  W-BEN-NDX               PIC S9(04) COMP   VALUE +0.      
00687      12  W-BEN-START             PIC S9(04) COMP   VALUE +0.      
00688      12  W-TIT-NDX               PIC S9(04) COMP   VALUE +0.      
00689      12  W-HDA-NDX               PIC S9(04) COMP   VALUE +0.      
00690      12  W-DETAIL-EXTRACT-CTR    PIC S9(09) COMP-3 VALUE +0.      
00691      12  W-DETAIL-RESERVES-CTR   PIC S9(09) COMP-3 VALUE +0.      
00692      12  W-BENEFIT               PIC  X(02)        VALUE SPACES.  
00693      12  W-VALUATION-DATE        PIC  9(11).                      
00694      12  W-VALUATION-BIN-DATE    PIC  X(02).                      
00695      12  W-CERT-EXP-DT           PIC  X(02).                      
00696  EJECT                                                            
00697      COPY ECSTAB64.                                               
00698  EJECT                                                            
00699  01  STORE-1ST-FOUR-TABLES.                                       
00700      12  S1-MONTHS       OCCURS 42.                               
00701          16  S1-M-AGE        PIC S9(5)   OCCURS 11.               
00702                                                                   
00703  01  STORE-5-THRU-18.                                             
00704    10  STR-TAB-5.                                                 
00705     11  FILLER               PIC  X(55).                          
00706     11  STR-TAB-6.                                                
00707      12  FILLER              PIC  X(55).                          
00708      12  STR-TAB-7.                                               
00709       13  FILLER             PIC  X(55).                          
00710       13  STR-TAB-8.                                              
00711        14  FILLER            PIC  X(55).                          
00712        14  STR-TAB-9.                                             
00713         15  FILLER           PIC  X(55).                          
00714         15  STR-TAB-10.                                           
00715          16  FILLER          PIC  X(55).                          
00716          16  STR-TAB-11.                                          
00717           17  FILLER         PIC  X(55).                          
00718           17  STR-TAB-12.                                         
00719            18  FILLER        PIC  X(55).                          
00720            18  STR-TAB-13.                                        
00721             19  FILLER       PIC  X(55).                          
00722             19  STR-TAB-14.                                       
00723              20  FILLER      PIC  X(55).                          
00724              20  STR-TAB-15.                                      
00725               21  FILLER     PIC  X(55).                          
00726               21  STR-TAB-16.                                     
00727                22  FILLER    PIC  X(55).                          
00728                22  STR-TAB-17.                                    
00729                 23  FILLER   PIC  X(55).                          
00730                 23  STR-TAB-18.                                   
00731                  24  FILLER  PIC  X(330).                         
00732                                                                   
00733  01  WRK-5-18-TABLES REDEFINES STORE-5-THRU-18.                   
00734      12  S5-YEARS        OCCURS 14.                               
00735          16 S5-Y-AGE         PIC S9(5)   OCCURS 11.               
00736      12  S5-AGES         OCCURS 5.                                
00737          16 S5-A-AGES        PIC S9(5)   OCCURS 11.               
00738  EJECT                                                            
00739  01  WRK-X-REC.                                                   
00740      12  WX-CNTRL.                                                
00741          16  W-CLMNO         PIC  X(7).                           
00742          16  W-CARR          PIC  X.                              
00743          16  W-CERT          PIC  X(11).                          
00744      12  FILLER              PIC  X(10).                          
00745      12  W-CALC-INTEREST     PIC S9(01)V9(04)    COMP-3.          
00746      12  W-ACCT              PIC  X(10).                          
00747      12  W-NAME              PIC  X(30).                          
00748      12  W-RSRV              PIC S9(7)V99        COMP-3.          
00749      12  W-TOTCLM            PIC S9(7)V99        COMP-3.          
00750      12  W-N-PYTS            PIC S9(3)           COMP-3.          
00751      12  W-CLM-TYP           PIC  X.                              
00752      12  W-CLM-STA           PIC  X.                              
00753      12  W-STATE             PIC  XX.                             
00754      12  W-AGE               PIC  99.                             
00755      12  W-CAUSE             PIC  X.                              
00756      12  W-OCCUP             PIC  X.                              
00757      12  W-CRT-EFF           PIC  9(11)  COMP-3.                  
00758      12  W-RCVD              PIC  9(11)  COMP-3.                  
00759      12  W-INCUR             PIC  9(11)  COMP-3.                  
00760      12  W-ENTER             PIC  9(11)  COMP-3.                  
00761      12  W-PAY-TO            PIC  9(11)  COMP-3.                  
00762      12  W-LST-MAINT         PIC  9(11)  COMP-3.                  
00763      12  W-PROC              PIC  X.                              
00764      12  W-COVTY             PIC  X.                              
00765      12  W-TERM              PIC  9(3).                           
00766      12  W-MO-BEN            PIC S9(7)V99        COMP-3.          
00767      12  W-SKIP              PIC  99.                             
00768      12  W-PYFR              PIC  XX.                             
00769      12  W-NOP               PIC  X(3).                           
00770      12  W-ERR-MES           PIC  XX     OCCURS 10.               
00771      12  W-REC-CTL           PIC  99.                             
00772      12  W-REC-CTL1          PIC  99.                             
00773      12  W-LN-NO             PIC  X(6).                           
00774      12  W-OE-CD             PIC  X.                              
00775      12  W-OE-STSW           PIC  X.                              
00776      12  W-OE-EDSW           PIC  X.                              
00777      12  W-EXP-DATE.                                              
00778          16  WEX-MO          PIC  99.                             
00779          16  WEX-DA          PIC  99.                             
00780          16  WEX-YR          PIC  99.                             
00781      12  W-CLMRESV           PIC S9(7)V99        COMP-3.          
00782      12  W-PERM-DIS          PIC  X.                              
00783      12  W-IBNR              PIC  X.                              
00784      12  W-ACC-NAME          PIC  X(30).                          
00785      12  W-1ST-PAY           PIC  9(11)          COMP-3.          
00786      12  W-RSRV-3            PIC S9(7)V99        COMP-3.          
00787      12  W-COMP.                                                  
00788          16  W-CARRIER       PIC  X.                              
00789          16  W-GROUPING      PIC  X(6).                           
00790      12  W-REI-COMP          PIC  X(6).                           
00791      12  W-CLM-BEN           PIC  XX.                             
00792      12  W-ACC-EXP           PIC  9(11)          COMP-3.          
00793      12  W-ACC-EFF           PIC  9(11)          COMP-3.          
00794      12  W-RSRV-FACT         PIC S9(3)V9(8)      COMP-3.          
00795      12  W-PAY-CURR          PIC S9(7)V99        COMP-3.          
00796      12  FILLER              PIC  X.                              
00797                                                                   
00798  01  LAS-CNTRL.                                                   
00799      12  LAS-CNTRLA.                                              
00800          16  LAS-REI-CMP     PIC  X(6)           VALUE SPACES.    
00801          16  LAS-COMP        PIC  X(7)           VALUE SPACES.    
00802          16  LAS-ST          PIC  XX             VALUE SPACES.    
00803          16  LAS-ACCT        PIC  X(10)          VALUE SPACES.    
00804      12  LAS-EXP             PIC  9(11)  COMP-3  VALUE 0.         
00805      12  LAS-EFF             PIC  9(11)  COMP-3  VALUE 0.         
00806                                                                   
00807  01  THS-CNTRL.                                                   
00808      12  THS-CNTRLA.                                              
00809          16  THS-REI-CMP     PIC  X(6)           VALUE SPACES.    
00810          16  THS-COMP        PIC  X(7)           VALUE SPACES.    
00811          16  THS-ST          PIC  XX             VALUE SPACES.    
00812          16  THS-ACCT        PIC  X(10)          VALUE SPACES.    
00813      12  THS-EXP             PIC  9(11)  COMP-3  VALUE 0.         
00814      12  THS-EFF             PIC  9(11)  COMP-3  VALUE 0.         
00815                                                                   
00816  01  ST-ACCUMS.                                                   
00817      12  ST-RSRV             PIC S9(9)V99        OCCURS 75.       
00818                                                                   
00819  01  YEAR-ACCUMS.                                                 
00820      12  YR-CCYY             PIC 9(04)         OCCURS 6.          
00821      12  YR-RSRV             PIC S9(9)V99        OCCURS 6.        
00822                                                                   
00823  01  ACCT-BRK-ACCUMS.                                             
00824      12  L-AH-ACCUMS     OCCURS 2.                                
00825          16  L-AH-BEN        PIC S9(7)V99 COMP-3      OCCURS 18.  
00826                                                                   
00827  01  W-TITLE.                                                     
00828      12  W-TIT-CHAR OCCURS 39 TIMES                               
00829                              PIC  X(01).                          
00830  EJECT                                                            
00831  01  HEAD-A.                                                      
00832      12  FILLER              PIC  X(43)          VALUE SPACES.    
00833      12  HA-REPORT-TITLE.                                         
00834          16  FILLER          PIC  X(12)          VALUE            
00835              'CREDIT LIFE '.                                      
00836          16  HEADA-RES-TYPE  PIC  X(12)          VALUE            
00837              'DISABILITY  '.                                      
00838          16  FILLER          PIC  X(15)          VALUE            
00839              ' CLAIM RESERVES'.                                   
00840      12  FILLER REDEFINES HA-REPORT-TITLE.                        
00841          16  HD-A-CHAR       OCCURS 39 TIMES                      
00842                              PIC  X(01).                          
00843      12  FILLER              PIC  X(38)          VALUE SPACES.    
00844      12  FILLER              PIC  X(6)           VALUE 'ECS039'.  
00845      12  HD-RPT-NO           PIC  X(01)          VALUE 'A'.       
00846  01  HEAD-2A.                                                     
00847      12  FILLER              PIC  X(41)          VALUE SPACES.    
00848      12  H2A-REPORT-TITLE    PIC  X(42)                           
00849          VALUE 'CREDIT LIFE ACCOUNT CLAIM RESERVES SUMMARY'.      
00850      12  FILLER              PIC  X(37)          VALUE SPACES.    
00851      12  FILLER              PIC  X(08)         VALUE 'ECS039B'.  
00852                                                                   
00853  01  HEAD-B.                                                      
00854      12  FILLER              PIC  X(47)          VALUE SPACES.    
00855      12  HB-NAME             PIC  X(30)          VALUE            
00856              '         COMPANY NAME'.                             
00857      12  FILLER              PIC  X(43)          VALUE SPACES.    
00858      12  HB-IPL              PIC  X(8)           VALUE            
00859              'IPL-DATE'.                                          
00860                                                                   
00861  01  HEAD-C.                                                      
00862      12  FILLER              PIC  X(53)          VALUE SPACES.    
00863      12  HC-DATE             PIC  X(18)          VALUE            
00864              'VALUATION DATE'.                                    
00865      12  FILLER              PIC  X(41)          VALUE SPACES.    
00866      12  FILLER              PIC  X(5)           VALUE 'PAGE '.   
00867      12  HC-PAGE             PIC  ZZ,ZZ9.                         
00868                                                                   
00869  01  HEAD-C1.                                                     
00870      12  FILLER              PIC  X(5)           VALUE SPACES.    
00871      12  FILLER              PIC  X(12)          VALUE            
00872              'REINSURANCE'.                                       
00873                                                                   
00874  01  HEAD-D.                                                      
00875      12  FILLER              PIC  X(103)         VALUE SPACES.    
00876      12  HD-FACTOR-SOURCE    PIC  X(9)           VALUE            
00877              '1964CDT3%'.                                         
00878                                                                   
00879  01  HEAD-E.                                                      
00880      12  FILLER              PIC  X(44)          VALUE            
00881              '                   CERT       CLAIM   EFF.  '.      
00882      12  FILLER              PIC  X(25)          VALUE            
00883              'ORIG ISS   EXPY  REM     '.                         
00884      12  HEADE-RES-TYPE      PIC  X(6)           VALUE            
00885              '      '.                                            
00886      12  FILLER              PIC  X(13)          VALUE            
00887              '   REPTD   MO'.                                     
00888      12  FILLER              PIC  X(07)          VALUE            
00889              'NTHLY  '.                                           
00890      12  HE-TABL-TITLE       PIC  X(05)          VALUE            
00891              'TABL '.                                             
00892      12  FILLER              PIC  X(03)          VALUE            
00893              'PM '.                                               
00894      12  HE-FACTOR-TITLE     PIC  X(09)          VALUE            
00895              'RSRV-FACT'.                                         
00896                                                                   
00897  01  HEAD-F.                                                      
00898      12  FILLER              PIC  X(44)          VALUE            
00899              '  ST  ACCOUNT     NUMBER      NUMBER  DATE  '.      
00900      12  FILLER              PIC  X(44)          VALUE            
00901              'TERM AGE   DATE  TERM   DATE/AGE  DATE    BE'.      
00902      12  FILLER              PIC  X(07)          VALUE            
00903              'NEFIT  '.                                           
00904      12  HF-TABL-TITLE       PIC  X(05)          VALUE            
00905              'USED '.                                             
00906      12  FILLER              PIC  X(03)          VALUE            
00907              'DS '.                                               
00908      12  HF-FACTOR-TITLE     PIC  X(13)          VALUE            
00909              'PER 100MI'.                                         
00910      12  FILLER              PIC  X(16)          VALUE            
00911              'RESERVE'.                                           
00912                                                                   
00913  01  HEAD-G.                                                      
00914      12  FILLER              PIC  X(5)    VALUE '     '.          
00915      12  HEADG-RES-TYPE      PIC  X(13)   VALUE '  DISABILITY '.  
00916      12  FILLER              PIC  X(26)   VALUE                   
00917              ' CLAIM RESERVES SUMMARIZED'.                        
00918      12  FILLER              PIC  X(44)   VALUE ' BY ACCOUNT'.    
00919                                                                   
00920  01  HEAD-H.                                                      
00921      12  FILLER              PIC  X(44)          VALUE            
00922              '                                      ACTIVE'.      
00923      12  FILLER              PIC  X(44)          VALUE            
00924              '       MONTHLY          CLAIM         CLM/RS'.      
00925      12  FILLER              PIC  X(44)          VALUE            
00926              'VR          IBNR     IBNR          PAY TO   '.      
00927                                                                   
00928  01  HEAD-I.                                                      
00929      12  FILLER              PIC  X(44)          VALUE            
00930              ' ST  ACCOUNT     ACCOUNT NAME         CLAIMS'.      
00931      12  FILLER              PIC  X(44)          VALUE            
00932              '       BENEFIT         RESERVE         100/M'.      
00933      12  FILLER              PIC  X(44)          VALUE            
00934              'I          RESERVE   RATIO         CURRENT  '.      
00935                                                                   
00936  01  HEAD-J.                                                      
00937      12  FILLER              PIC  X(8)           VALUE            
00938              '        '.                                          
00939      12  HEADJ-RES-TYPE      PIC  X(12)          VALUE            
00940              'DISABILITY  '.                                      
00941      12  FILLER              PIC  X(26)          VALUE            
00942              ' CLAIM RESERVES SUMMARIZED'.                        
00943      12  FILLER              PIC  X(42)          VALUE            
00944              '    BY YEAR INCURRED                      '.        
00945      12  FILLER              PIC  X(44)          VALUE            
00946              ' BY STATE'.                                         
00947                                                                   
00948  01  HEAD-K.                                                      
00949      12  FILLER              PIC  X(44)          VALUE            
00950              '                                            '.      
00951      12  FILLER              PIC  X(44)          VALUE            
00952              '                  CLAIM                     '.      
00953      12  FILLER              PIC  X(44)          VALUE            
00954              '           CLAIM'.                                  
00955                                                                   
00956  01  HEAD-L.                                                      
00957      12  FILLER              PIC  X(44)          VALUE            
00958              '                                            '.      
00959      12  FILLER              PIC  X(44)          VALUE            
00960              ' YEAR            RESERVE               STATE'.      
00961      12  FILLER              PIC  X(44)          VALUE            
00962              '          RESERVE'.                                 
00963                                                                   
00964  01  HEAD-M.                                                      
00965      12  FILLER              PIC  X(5)           VALUE            
00966              '     '.                                             
00967      12  HEADM-RES-TYPE      PIC  X(12)          VALUE            
00968              '  DISABILITY'.                                      
00969      12  FILLER              PIC  X(27)          VALUE            
00970              '  CLAIM RESERVE TOTALS FOR '.                       
00971      12  HM-CNT              PIC  ZZ,ZZ9.                         
00972      12  FILLER              PIC  X(38)          VALUE            
00973              ' CLAIMS WITH GREATER THAN 60 MONTHS OF'.            
00974      12  FILLER              PIC  X(23)          VALUE            
00975              ' REMAINING COVERAGE IS '.                           
00976      12  HM-RSRV             PIC  ZZZ,ZZZ,ZZ9.99-.                
00977                                                                   
00978  01  HEAD-N.                                                      
00979      12  FILLER              PIC  X(41)          VALUE            
00980              '        TOTAL MONTHLY BENEFIT INCOME FOR '.         
00981      12  HN-CNT              PIC  ZZ,ZZ9.                         
00982      12  FILLER              PIC  X(35)          VALUE            
00983              ' IBNR CASES DURING LAST QUARTER IS '.               
00984      12  HN-MOBEN            PIC  ZZZ,ZZZ,ZZ9.99-.                
00985  EJECT                                                            
00986  01  DATA-1.                                                      
00987      12  FILLER              PIC  XX             VALUE SPACES.    
00988      12  D1-ST               PIC  XX.                             
00989      12  FILLER              PIC  X              VALUE SPACES.    
00990      12  D1-ACCT             PIC  X(10).                          
00991      12  FILLER              PIC  X              VALUE SPACES.    
00992      12  D1-CERT             PIC  X(11).                          
00993      12  FILLER              PIC  XX             VALUE SPACES.    
00994      12  D1-CLM              PIC  X(7).                           
00995      12  FILLER              PIC  XX             VALUE SPACES.    
00996      12  D1-EFMO             PIC  Z9.                             
00997      12  FILLER              PIC  X              VALUE '/'.       
00998      12  D1-EFYR             PIC  99.                             
00999      12  FILLER              PIC  XX             VALUE SPACES.    
01000      12  D1-OTRM             PIC  ZZ9.                            
01001      12  FILLER              PIC  XX             VALUE SPACES.    
01002      12  D1-OAGE             PIC  Z9.                             
01003      12  FILLER              PIC  XX             VALUE SPACES.    
01004      12  D1-XMO              PIC  Z9.                             
01005      12  FILLER              PIC  X              VALUE '/'.       
01006      12  D1-XYR              PIC  99.                             
01007      12  FILLER              PIC  XX             VALUE SPACES.    
01008      12  D1-RTRM             PIC  ZZ9.                            
01009      12  FILLER              PIC  X(3)           VALUE SPACES.    
01010      12  D1-IMO              PIC  Z9.                             
01011      12  FILLER              PIC  X              VALUE '/'.       
01012      12  D1-IYR              PIC  99.                             
01013      12  FILLER              PIC  XX             VALUE SPACES.    
01014      12  D1-DAGE             PIC  Z9.                             
01015      12  FILLER              PIC  XX             VALUE SPACES.    
01016      12  D1-EMO              PIC  Z9.                             
01017      12  FILLER              PIC  X              VALUE '/'.       
01018      12  D1-EYR              PIC  99.                             
01019      12  D1-MOBEN            PIC  ZZZZZZ9.99-.                    
01020 *    12  FILLER              PIC  X              VALUE SPACES.    
01021      12  D1-TABL-GRP.                                             
01022          16  FILLER          PIC  X(02)          VALUE SPACES.    
01023          16  D1-TABL         PIC  Z9.                             
01024          16  FILLER          PIC  X              VALUE SPACES.    
01025      12  D1-CALC-INTEREST REDEFINES D1-TABL-GRP                   
01026                              PIC  .9999.                          
01027      12  FILLER              PIC  X              VALUE SPACES.    
01028      12  D1-PERM-DIS         PIC  XX.                             
01029 *    12  FILLER              PIC  X              VALUE SPACES.    
01030      12  D1-FACTOR           PIC  ZZZZZ9.999-.                    
01031      12  D1-FACTOR-OPT REDEFINES D1-FACTOR                        
01032                              PIC  ZZ9.999999-.                    
01033      12  D1-RESERVE          PIC  ZZZ,ZZ9.99-.                    
01034      12  D1-AST              PIC  X              VALUE SPACES.    
01035      12  FILLER              PIC  X              VALUE SPACES.    
01036      12  D1-IBNR             PIC  X(4).                           
01037  EJECT                                                            
01038  01  DATA-1A.                                                     
01039      12  FILLER              PIC  XX             VALUE SPACES.    
01040      12  D1A-ST              PIC  XX.                             
01041      12  FILLER              PIC  X              VALUE SPACES.    
01042      12  D1A-ACCT            PIC  X(10).                          
01043      12  FILLER              PIC  X              VALUE SPACES.    
01044      12  D1A-CERT            PIC  X(11).                          
01045      12  FILLER              PIC  X(11)          VALUE SPACES.    
01046      12  D1A-EFMO            PIC  Z9.                             
01047      12  FILLER              PIC  X              VALUE '/'.       
01048      12  D1A-EFYR            PIC  99.                             
01049      12  FILLER              PIC  X(70)          VALUE SPACES.    
01050      12  D1A-RESERVE         PIC  ZZZ,ZZ9.99-.                    
01051      12  D1A-AST             PIC  X              VALUE SPACES.    
01052      12  FILLER              PIC  X              VALUE SPACES.    
01053      12  D1A-IBNR            PIC  X(4).                           
01054                                                                   
01055  01  DATA-2.                                                      
01056      12  FILLER              PIC  X              VALUE SPACES.    
01057      12  D2-STATE            PIC  XX.                             
01058      12  FILLER              PIC  X              VALUE SPACES.    
01059      12  D2-COMP-FILL.                                            
01060          16  D2-ACC          PIC  X(10).                          
01061          16  FILLER          PIC  XX             VALUE SPACES.    
01062          16  D2-NAME         PIC  X(20).                          
01063      12  FILLER              PIC  X              VALUE SPACES.    
01064      12  D2-COUNTS           PIC  ZZZZZ.                          
01065      12  FILLER              PIC  X              VALUE SPACES.    
01066      12  D2-MOBEN            PIC  ZZZ,ZZZ,ZZ9.99-.                
01067      12  FILLER              PIC  X              VALUE SPACES.    
01068      12  D2-RSRV             PIC  ZZZ,ZZZ,ZZ9.99-.                
01069      12  D2-CLM100           PIC  ZZZZ,ZZZ,ZZ9.99-.               
01070      12  FILLER              PIC  X              VALUE SPACES.    
01071      12  D2-IBNR             PIC  ZZZ,ZZZ,ZZ9.99-.                
01072      12  FILLER              PIC  X              VALUE SPACES.    
01073      12  D2-RATIO            PIC  Z9.999.                         
01074      12  D2-RAT-SP REDEFINES                                      
01075          D2-RATIO            PIC  X(6).                           
01076      12  FILLER              PIC  XX             VALUE SPACES.    
01077      12  D2-PTC              PIC  ZZZ,ZZZ,ZZ9.99-.                
01078                                                                   
01079  01  DATA-3.                                                      
01080      12  D3-SPACE-1.                                              
01081          16  FILLER          PIC  X(44)         VALUE SPACES.     
01082          16  D3-YR           PIC  X(5).                           
01083          16  FILLER          PIC  X(5)          VALUE SPACES.     
01084          16  D3-Y-RSRV       PIC  ZZZ,ZZZ,ZZ9.99-.                
01085          16  FILLER          PIC  X(15)         VALUE SPACES.     
01086      12  D3-SPACE-2.                                              
01087          16  D3-ST           PIC  XX.                             
01088          16  FILLER          PIC  X(5)          VALUE SPACES.     
01089          16  D3-S-RSRV       PIC  ZZZ,ZZZ,ZZ9.99-.                
01090                                                                   
01091  01  D3-FILLS.                                                    
01092      12  D3-FYR.                                                  
01093          16  FILLER          PIC  X              VALUE SPACE.     
01094          16  D3-NCC          PIC  99.                             
01095          16  D3-NYR          PIC  99.                             
01096      12  D3-PRIOR            PIC  X(5)           VALUE 'PRIOR'.   
01097      12  D3-TOTAL            PIC  X(5)           VALUE 'TOTAL'.   
01098  EJECT                                                            
01099  01  TOTAL-1.                                                     
01100      12  FILLER              PIC  X(25)          VALUE SPACES.    
01101      12  T1-NAME             PIC  X(54)          VALUE SPACES.    
01102 *    12  FILLER              PIC  XX             VALUE SPACES.    
01103      12  T1-MOBEN            PIC  ZZZ,ZZZ,ZZ9.99-.                
01104      12  FILLER              PIC  X(15)          VALUE SPACES.    
01105      12  T1-RSRV             PIC  ZZZ,ZZZ,ZZ9.99-.                
01106                                                                   
01107  01  TOTAL-1A.                                                    
01108      12  FILLER              PIC  X(94)          VALUE SPACES.    
01109      12  FILLER              PIC  X(15)                           
01110          VALUE 'IBNR TOTALS: '.                                   
01111      12  T1A-IBNR            PIC  ZZZ,ZZZ,ZZ9.99-.                
01112                                                                   
01113  01  TOTAL-3.                                                     
01114      12  FILLER              PIC  X(20)          VALUE SPACES.    
01115      12  FILLER              PIC  X(34)          VALUE 'TOTAL'.   
01116      12  T3-Y-RSRV           PIC  ZZZ,ZZZ,ZZ9.99-.                
01117      12  FILLER              PIC  X(22)          VALUE SPACES.    
01118      12  T3-S-RSRV           PIC  ZZZ,ZZZ,ZZ9.99-.                
01119                                                                   
01120  01  T1-COMP-NAME.                                                
01121      12  T1-COMP-1ST         PIC  X(12)          VALUE SPACES.    
01122      12  T1-TITLE            PIC  X(13)          VALUE            
01123              'CARRIER/GRP'.                                       
01124      12  T1-COMP-NUM         PIC  X(07)          VALUE SPACES.    
01125                                                                   
01126  01  END-TOT-MES.                                                 
01127      12  FILLER              PIC  X(10)          VALUE            
01128              'TOTAL FOR '.                                        
01129      12  E-TOTCNT            PIC  ZZ,ZZ9.                         
01130      12  FILLER              PIC  X(6)           VALUE ' CASES'.  
01131                                                                   
01132  01  END-TOT-MES-OPT.                                             
01133      12  FILLER              PIC  X(10)          VALUE            
01134              'TOTAL FOR '.                                        
01135      12  E-TOTCNT-CLMS       PIC  ZZ,ZZ9.                         
01136      12  FILLER              PIC  X(12)                           
01137          VALUE ' CLAIMS AND '.                                    
01138      12  E-TOTCNT-IBNR       PIC  ZZ,ZZ9.                         
01139      12  FILLER              PIC  X(13)                           
01140          VALUE ' IBNR RECORDS'.                                   
01141  EJECT                                                            
01142      COPY ECSEXT01.                                               
01143  EJECT                                                            
01144      COPY ELCEXTVR.                                               
01145  EJECT                                                            
01146      COPY ELCCALC.                                                
01147  EJECT                                                            
01148  01  FILLER                      PIC  X(26)                       
01149                           VALUE 'OPTIONAL CALC AREA STARTS:'.     
01150      COPY ELCRSVCM.                                               
01151  EJECT                                                            
01152  01  FILLER                      PIC  X(22)                       
01153                           VALUE 'DATE CALC AREA STARTS:'.         
01154      COPY ELCDATE.                                                
01155  EJECT                                                            
01156      COPY ELCDTECX.                                               
01157  EJECT                                                            
01158      COPY ELCDTEVR.                                               
01159  EJECT                                                            
01160  PROCEDURE DIVISION.                                              
01161                                                                   
01162  0100-OPEN-EM.                                                    
01163                              COPY ELCDTERX.                       
01164                                                                   
01165      MOVE CLAS-STARTS     TO CLAS-INDEXS.                         
01166      MOVE WS-CURRENT-DATE TO HB-IPL.                              
01167      MOVE COMPANY-NAME    TO HB-NAME.                             
01168      MOVE ALPH-DATE       TO HC-DATE.                             
01169                                                                   
01170      MOVE AH-OVERRIDE-L6  TO HEADE-RES-TYPE.                      
01171      MOVE AH-OVERRIDE-L12 TO HEADG-RES-TYPE                       
01172                              HEADA-RES-TYPE                       
01173                              HEADM-RES-TYPE                       
01174                              HEADJ-RES-TYPE.                      
01175                                                                   
01176      PERFORM 0600-REARRANGE-TITLE THRU 0600-EXIT.                 
01177                                                                   
01178 *************  SET INTER SWITCH FOR AFL **************            
01179      IF DTE-CLIENT = 'AFL'                                        
01180          MOVE '1'  TO  INTER-SW.                                  
01181 ******************************************************            
01182                                                                   
01183      MOVE ZEROS  TO  INPUT-TRANSACTION-SWITCH.                    
01184                                                                   
01185      IF  DTE-OPT-RESERVE-METHOD-AUTH                              
01186          MOVE SPACES             TO HD-FACTOR-SOURCE              
01187          MOVE '1985 CIDA'        TO HE-FACTOR-TITLE               
01188          MOVE 'CALC'             TO HE-TABL-TITLE                 
01189          MOVE 'INTR'             TO HF-TABL-TITLE                 
01190          MOVE 'RSRV-FACT'        TO HF-FACTOR-TITLE               
01191          MOVE RUN-DATE           TO W-VALUATION-DATE              
01192          MOVE BIN-RUN-DATE       TO W-VALUATION-BIN-DATE.         
01193                                                                   
01194  0110-Z-ACCUMS.                                                   
01195      PERFORM 0670-Z-STATES                                        
01196          VARYING  CX  FROM  +1  BY  +1                            
01197              UNTIL  CX  =  +76.                                   
01198                                                                   
01199      PERFORM 0690-Z-YEARS  THRU  0700-EZ-YRS                      
01200          VARYING  CX  FROM  1  BY  1                              
01201              UNTIL  CX  =  7.                                     
01202                                                                   
01203      PERFORM 0710-Z-LAH                                           
01204          VARYING  CX  FROM  +1  BY  +1                            
01205              UNTIL  CX  GREATER THAN  +2                          
01206                  AFTER  CY  FROM  +1  BY  +1                      
01207                      UNTIL  CY  GREATER THAN  +18.                
01208                                                                   
01209      IF  DTE-OPT-RESERVE-METHOD-AUTH                              
01210          GO TO 0150-END-EXP.                                      
01211                                                                   
01212  0120-EXPAND-FACTORS.                                             
01213      IF DTE-CLIENT  NOT = 'LAP'                                   
01214          GO TO 0150-END-EXP.                                      
01215                                                                   
01216      MOVE '4'    TO  INTER-SW.                                    
01217      MOVE +1.15  TO  EXPAND-FACTOR.                               
01218      MOVE +0     TO  X1.                                          
01219                                                                   
01220  0130-EXP-TERM.                                                   
01221      ADD +1  TO  X1.                                              
01222                                                                   
01223      IF X1  GREATER THAN  +042                                    
01224          GO TO 0150-END-EXP.                                      
01225                                                                   
01226      MOVE +0  TO  Y1.                                             
01227                                                                   
01228  0140-EXP-FACT.                                                   
01229      ADD +1  TO  Y1.                                              
01230                                                                   
01231      IF Y1  GREATER THAN  +011                                    
01232          GO TO 0130-EXP-TERM.                                     
01233                                                                   
01234      IF T-1-FACTORS (X1 Y1)  NUMERIC                              
01235          MULTIPLY  EXPAND-FACTOR  BY  T-1-FACTORS (X1 Y1)         
01236              GIVING  T-1-FACTORS (X1 Y1)  ROUNDED.                
01237                                                                   
01238      IF T-2-FACTORS (X1 Y1)  NUMERIC                              
01239          MULTIPLY  EXPAND-FACTOR  BY  T-2-FACTORS (X1 Y1)         
01240              GIVING  T-2-FACTORS (X1 Y1)  ROUNDED.                
01241                                                                   
01242      IF T-3-FACTORS (X1 Y1)  NUMERIC                              
01243          MULTIPLY  EXPAND-FACTOR  BY  T-3-FACTORS (X1 Y1)         
01244              GIVING  T-3-FACTORS (X1 Y1)  ROUNDED.                
01245                                                                   
01246      IF T-4-FACTORS (X1 Y1)  NUMERIC                              
01247          MULTIPLY  EXPAND-FACTOR  BY  T-4-FACTORS (X1 Y1)         
01248              GIVING  T-4-FACTORS (X1 Y1)  ROUNDED.                
01249                                                                   
01250      GO TO 0140-EXP-FACT.                                         
01251                                                                   
01252  0150-END-EXP.                                                    
01253      MOVE +0  TO  X1  Y1.                                         
01254                                                                   
01255      OPEN  OUTPUT  CLM-RSRV-REPT.                                 
01256                                                                   
01257      IF  DTE-OPT-RESERVE-METHOD-UNAUTH                            
01258          GO TO 0170-GET-RESERVE-SECT.                             
01259                                                                   
01260      MOVE '1'                    TO OP-TABLE-FILE-STATUS-IND.     
01261      CALL 'ELRSVSPL' USING OPTIONAL-CALCULATION-PASS-AREA.        
01262                                                                   
01263      OPEN INPUT  DETAIL-EXTRACT-IN                                
01264           OUTPUT UNSRTD-RESERVES.                                 
01265                                                                   
01266      GO TO 0170-GET-RESERVE-SECT.                                 
01267  EJECT                                                            
01268  0170-GET-RESERVE-SECT SECTION.                                   
01269                                                                   
01270  0175-PRESORT-INPUT.                                              
01271                                                                   
01272      SORT SORTED-CLMS ON ASCENDING KEY SORT-KEY-1                 
01273                                        SORT-KEY-2                 
01274                                        SORT-KEY-3                 
01275                                        SORT-KEY-4                 
01276                                        SORT-KEY-5                 
01277                                        SORT-KEY-6                 
01278          USING UNSRTD-HIST-IN                                     
01279          GIVING CLAIM-HIST-IN.                                    
01280                                                                   
01281      IF  SORT-RETURN IS NOT EQUAL TO ZEROS                        
01282          MOVE SORT-RETURN        TO WS-RETURN-CODE                
01283          MOVE 'SORT ONE UNSUCCESSFUL - PROGRAM TERMINATED'        
01284                                  TO WS-ABEND-MESSAGE              
01285          GO TO ABEND-PGM.                                         
01286                                                                   
01287                                                                   
01288  0180-GET-RSRV-SORT.                                              
01289      SORT DISK-SORT  ON  ASCENDING KEY                            
01290          SW-00  SW-0  SW-A  SW-B  SW-BB  SW-C  SW-CC SW-E         
01291 *                                                                 
01292 *        REI-CMP  COMP  ST  ACCT  ACC-EXP  CLM-NO  PAY-TO-DTE     
01293 *                                                                 
01294          INPUT  PROCEDURE 0200-MAKE-EXTR-SECT    THRU 0299-EXIT   
01295          OUTPUT PROCEDURE 0300-PRINT-REPORT-SECT THRU 0940-EXIT.  
01296                                                                   
01297      IF SORT-RETURN  NOT = ZEROS                                  
01298         MOVE 0101      TO WS-RETURN-CODE                          
01299         MOVE ' ERROR IN SORT   ' TO WS-ABEND-MESSAGE              
01300         GO TO ABEND-PGM.                                          
01301                                                                   
01302      IF  DTE-OPT-RESERVE-METHOD-AUTH                              
01303          DISPLAY 'RECORDS RELEASED - ' W-RELEASED-RCRD-COUNT      
01304          DISPLAY 'REJECTED BY DATE - ' W-REJECTED-BY-DATE         
01305          DISPLAY 'REJECTED BY STATUS - ' W-REJECTED-BY-STATUS     
01306          DISPLAY 'REJECTED BY RSRV - ' W-REJECTED-BY-RSRV         
01307          DISPLAY 'REJECTED BY TYPE - ' W-REJECTED-BY-TYPE         
01308          MOVE ZEROS              TO W-REJECTED-BY-DATE            
01309                                     W-REJECTED-BY-RSRV            
01310                                     W-REJECTED-BY-TYPE.           
01311                                                                   
01312      PERFORM 0960-SECOND-REPORT-PRINT.                            
01313                                                                   
01314      IF  DTE-OPT-RESERVE-METHOD-AUTH                              
01315          DISPLAY 'PENDING RCRS READ - ' W-PENDING-RCRD-COUNT      
01316          DISPLAY 'REJECTED BY DATE - ' W-REJECTED-BY-DATE         
01317          DISPLAY 'REJECTED BY RSRV - ' W-REJECTED-BY-RSRV         
01318          DISPLAY 'REJECTED BY TYPE - ' W-REJECTED-BY-TYPE         
01319          SORT SORT-RESERVES ON ASCENDING KEY SORT-KEY-R1          
01320              USING UNSRTD-RESERVES                                
01321              GIVING DETAIL-RESERVES                               
01322          IF  SORT-RETURN IS NOT EQUAL ZEROS                       
01323              MOVE SORT-RETURN        TO WS-RETURN-CODE            
01324              MOVE 'RSRV SORT UNSUCCESSFUL - PROGRAM TERMINATED'   
01325                                      TO WS-ABEND-MESSAGE          
01326              GO TO ABEND-PGM.                                     
01327                                                                   
01328      GOBACK.                                                      
01329  EJECT                                                            
01330  0200-MAKE-EXTR-SECT      SECTION.                                
01331                                                                   
01332  0210-OPEN-INPUT-PROC.                                            
01333                                                                   
01334      OPEN  INPUT  CLAIM-HIST-IN.                                  
01335                                                                   
01336      COMPUTE VAL-DT  =  RUN-MO  +  (RUN-CCYY * +12).              
01337                                                                   
01338      MOVE VAL-DT  TO  VAL-RV3  SV-VAL.                            
01339                                                                   
01340      IF DTE-CLIENT  = 'AZT'                                       
01341          SUBTRACT  +1  FROM  VAL-RV3                              
01342      ELSE                                                         
01343          SUBTRACT  +3  FROM  VAL-RV3.                             
01344                                                                   
01345      IF  DTE-OPT-RESERVE-METHOD-AUTH                              
01346          PERFORM 2000-PROCESS-DETAIL-EXTRACT THRU 2000-EXIT.      
01347                                                                   
01348  0220-READ-CLM-HIST.                                              
01349                                                                   
01350 *    READ CLAIM-HIST-IN  INTO  DETAIL-EXTRACT  AT END             
01351      READ CLAIM-HIST-IN                                           
01352          AT END                                                   
01353              PERFORM 0230-REL-CHK THRU 0240-REL-CHK-X             
01354              GO TO 0290-EOJ-IP.                                   
01355                                                                   
01356      IF  CH-TRANS  NOT = 'X'                                      
01357          GO TO 0220-READ-CLM-HIST.                                
01358                                                                   
01359      IF  CH-PAY-CODE = 'V'                                        
01360          GO TO 0220-READ-CLM-HIST.                                
01361                                                                   
01362      IF  DTE-OPT-RESERVE-METHOD-AUTH                              
01363              AND                                                  
01364          CH-INCUR LESS THAN CH-EFF                                
01365          DISPLAY ' INCURRED DATE BEFORE CERT EFF DTE - '          
01366              CH-CONTROL ' INCUR - ' CH-INCUR '  EFF - ' CH-EFF    
01367          MOVE CH-EFF             TO CH-INCUR.                     
01368                                                                   
01369      IF  CH-CNUM = SPACES OR ZEROS                                
01370          MOVE CH-LNAME           TO CH-CNUM.                      
01371                                                                   
01372      IF INPUT-TRANSACTION-SWITCH  = '0'                           
01373          MOVE 1  TO  INPUT-TRANSACTION-SWITCH.                    
01374                                                                   
01375      MOVE CH-REI-COMP            TO CH-C0.                        
01376      MOVE CH-CNTRL1              TO CH-C1.                        
01377      MOVE CH-CNUM                TO CH-C2.                        
01378      MOVE CH-CERT                TO CH-C3.                        
01379                                                                   
01380      IF  LAS-CHIST EQUAL SPACES                                   
01381          MOVE IN-CLM-HIST        TO DETAIL-EXTRACT                
01382          GO TO 0260-BASIC-WRK-FILL                                
01383      ELSE                                                         
01384          IF  LAS-CHIST EQUAL CHIST-CNTRL                          
01385              MOVE IN-CLM-HIST    TO DETAIL-EXTRACT                
01386              GO TO 0270-SECOND-WRK-FILL.                          
01387                                                                   
01388  0230-REL-CHK.                                                    
01389                                                                   
01390      IF NO-CLAIMS                                                 
01391          GO TO 0235-MOVE-INPUT.                                   
01392                                                                   
01393      IF W-CLM-STA  =  'F'                                         
01394          GO TO 0235-MOVE-INPUT.                                   
01395                                                                   
01396      COMPUTE PAY1-DT  =  WPT-MO  +  (WPT-CCYY  *  +12).           
01397      COMPUTE ENT-DT   =  WEN-MO  +  (WEN-CCYY  *  +12).           
01398                                                                   
01399      IF PAY1-DT  = ZEROS                                          
01400          MOVE VAL-DT  TO  PAY1-DT.                                
01401                                                                   
01402      IF PAY1-DT  LESS THAN  VAL-RV3                               
01403         GO TO 0235-MOVE-INPUT.                                    
01404                                                                   
01405      IF ENT-DT  GREATER THAN  VAL-DT                              
01406          GO TO 0235-MOVE-INPUT.                                   
01407                                                                   
01408      IF  DTE-OPT-RESERVE-METHOD-AUTH                              
01409          PERFORM 2100-GET-OPT-RESERVES THRU 2100-EXIT             
01410          IF  W-RSRV EQUAL ZEROS                                   
01411              GO TO 0235-MOVE-INPUT.                               
01412                                                                   
01413      MOVE WRK-X-REC  TO  SORT-WORK.                               
01414                                                                   
01415      RELEASE SORT-WORK.                                           
01416                                                                   
01417      MOVE 2  TO  INPUT-TRANSACTION-SWITCH.                        
01418                                                                   
01419  0235-MOVE-INPUT.                                                 
01420                                                                   
01421      MOVE IN-CLM-HIST  TO DETAIL-EXTRACT.                         
01422                                                                   
01423  0240-REL-CHK-X.                                                  
01424      EXIT.                                                        
01425                                                                   
01426  EJECT                                                            
01427  0260-BASIC-WRK-FILL.                                             
01428      MOVE CHIST-CNTRL  TO  LAS-CHIST.                             
01429      MOVE SPACES       TO  WRK-X-REC  REL-CHECK.                  
01430      MOVE ZEROS        TO  W-RSRV      W-TOTCLM   W-N-PYTS        
01431                            W-SKIP      W-REC-CTL  W-REC-CTL1      
01432                            W-EXP-DATE  W-CLMRESV  W-RSRV-3        
01433                            W-PAY-CURR  W-RSRV-FACT.               
01434      MOVE DE-CNUM      TO  W-CLMNO.                               
01435      MOVE DE-CARRIER   TO  W-CARR.                                
01436      MOVE DE-CERT      TO  W-CERT.                                
01437      MOVE DE-ACCOUNT   TO  W-ACCT.                                
01438      MOVE DE-NAME      TO  W-NAME.                                
01439      MOVE DE-TYPE      TO  W-CLM-TYP.                             
01440                                                                   
01441      IF  DE-TYPE  = '1'  OR  '3'                                  
01442          MOVE DE-LF-TYPE TO W-CLM-BEN                             
01443          MOVE DE-LF-TERM TO W-TERM                                
01444      ELSE                                                         
01445          MOVE DE-AH-TERM TO W-TERM                                
01446          MOVE DE-AH-TYPE TO W-CLM-BEN.                            
01447                                                                   
01448      MOVE DE-STATE      TO  W-STATE.                              
01449      MOVE DE-AGE        TO  W-AGE.                                
01450      MOVE DE-EFF        TO  WS-W-CRT-EFF-N                        
01451                             W-CRT-EFF.                            
01452                                                                   
01453      IF DTE-CLIENT  = 'GTL'                                       
01454      AND DE-TYPE  = '4'                                           
01455          MOVE DE-INCUR  TO  WS-W-CRT-EFF-N                        
01456                             W-CRT-EFF.                            
01457                                                                   
01458      MOVE ZEROS         TO  WS-W-RCVD-N.                          
01459      MOVE '7'           TO  DC-OPTION-CODE.                       
01460      MOVE DE-RPT-YR     TO  DC-ALPHA-YEAR.                        
01461      PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT.                 
01462                                                                   
01463      IF ONLY-CENTURY                                              
01464         MOVE DC-ALPHA-CEN-N TO WRV-CC                             
01465         MOVE DE-RPT-YR      TO WRV-YR                             
01466         MOVE DE-RPT-MO      TO WRV-MO                             
01467         MOVE DE-RPT-DA      TO WRV-DA                             
01468         MOVE WS-W-RCVD-N    TO W-RCVD                             
01469      ELSE                                                         
01470         MOVE 'DE-REPORTED CONVERSION ERROR' TO WS-ABEND-MESSAGE   
01471         MOVE DC-ERROR-CODE TO WS-ABEND-FILE-STATUS                
01472         GO TO ABEND-PGM.                                          
01473                                                                   
01474      MOVE DE-INCUR      TO  WS-W-INCUR-N                          
01475                             W-INCUR.                              
01476      MOVE DE-PROC-DT    TO  WS-W-ENTER-N                          
01477                             W-ENTER.                              
01478                                                                   
01479      IF DE-AH-BEN  NOT NUMERIC                                    
01480         MOVE ZEROS  TO  DE-AH-BEN.                                
01481                                                                   
01482      IF DE-REIN NOT EQUAL SPACE                                   
01483         IF DE-REI-AHAMT NUMERIC                                   
01484             MOVE DE-REI-AHAMT  TO  W-MO-BEN                       
01485         ELSE                                                      
01486             MOVE DE-REI-CLAIM-AMT  TO  W-MO-BEN                   
01487      ELSE                                                         
01488          MOVE DE-AH-BEN  TO  W-MO-BEN.                            
01489                                                                   
01490      MOVE DE-PAY        TO  WS-W-1ST-PAY-N                        
01491                             W-1ST-PAY.                            
01492                                                                   
01493      MOVE DE-CARRIER TO W-CARRIER.                                
01494      MOVE DE-GROUPING TO W-GROUPING.                              
01495                                                                   
01496      IF DE-REIN  NOT = SPACE                                      
01497          MOVE DE-REI-COMP   TO  W-REI-COMP                        
01498      ELSE                                                         
01499          MOVE SPACES        TO  W-REI-COMP.                       
01500                                                                   
01501      IF DTE-CLIENT  = 'GTL' AND                                   
01502         DE-TYPE     = '4'                                         
01503          MOVE +6           TO  W-TERM                             
01504          MOVE DE-CLAIM-AMT TO  W-MO-BEN.                          
01505                                                                   
01506      MOVE ZEROS            TO  WS-W-PAY-TO-N                      
01507                                W-PAY-TO.                          
01508      MOVE DE-ACC-NAME      TO  W-ACC-NAME.                        
01509      MOVE DE-ACC-EXP-DTE   TO  W-ACC-EXP.                         
01510      MOVE DE-ACC-EFF-DTE   TO  W-ACC-EFF.                         
01511                                                                   
01512 ******************************************************************
01513      IF W-COMP = '2027304'     AND                                
01514         W-ACCT = '2730430000'  AND                                
01515         W-CERT = '0002619005 '                                    
01516          DISPLAY '     '                                          
01517          DISPLAY '     '                                          
01518          DISPLAY 'CLAIM CNTL: ' W-CLMNO '-' W-CARR '-' W-CERT ' ' 
01519              W-CRT-EFF ' AGE: ' W-AGE ' MO-BEN: ' W-MO-BEN        
01520              ' REINCO: ' W-REI-COMP                               
01521          DISPLAY '           REPORTED: ' W-RCVD                   
01522          DISPLAY '           INCURRED: ' W-INCUR                  
01523          DISPLAY '           ENTERED:  ' W-ENTER                  
01524          DISPLAY '           1ST PAID: ' W-1ST-PAY                
01525 ***      DISPLAY '           PAID TO:  ' W-PAY-TO                 
01526          DISPLAY '           IBNR RSV: ' W-IBNR                   
01527          DISPLAY '           PAY CURR: ' W-PAY-CURR               
01528          DISPLAY '           RESERVE3: ' W-RSRV-3.                
01529 ******************************************************************
01530                                                                   
01531  0270-SECOND-WRK-FILL.                                            
01532      MOVE DE-PROC-DT       TO  WS-W-LST-MAINT-N                   
01533                                W-LST-MAINT.                       
01534                                                                   
01535      IF DE-PAY-CODE  = 'F'                                        
01536          MOVE DE-PAY-CODE  TO  W-CLM-STA.                         
01537                                                                   
01538      IF DE-PAID-TO NOT = WS-W-PAY-TO-N                            
01539          MOVE DE-PAY-CODE  TO  W-CLM-STA                          
01540          MOVE DE-PAID-TO   TO  WS-W-PAY-TO-N                      
01541                                W-PAY-TO.                          
01542                                                                   
01543 ******************************************************************
01544      IF W-COMP = '2027304'     AND                                
01545         W-ACCT = '2730430000'  AND                                
01546         W-CERT = '0002619005 '                                    
01547          DISPLAY '           PAID TO:  ' W-PAY-TO                 
01548                  '   DE-PAID-TO: ' DE-PAID-TO                     
01549                  '  PAID: ' DE-PAY ' TYP: ' DE-PAY-CODE.          
01550 ******************************************************************
01551                                                                   
01552      GO TO 0220-READ-CLM-HIST.                                    
01553  EJECT                                                            
01554  0290-EOJ-IP.                                                     
01555                                                                   
01556      IF  DTE-OPT-RESERVE-METHOD-AUTH                              
01557          CLOSE CLAIM-HIST-IN                                      
01558                DETAIL-EXTRACT-IN                                  
01559                UNSRTD-RESERVES                                    
01560          DISPLAY 'NUMBER OF DETAIL EXTRACTS USED (IBNR) - '       
01561              W-DETAIL-EXTRACT-CTR                                 
01562          DISPLAY 'NUMBER OF DETAIL RESERVES CREATED - '           
01563              W-DETAIL-RESERVES-CTR                                
01564          DISPLAY '    A&H  - ' W-AH-DE-COUNT                      
01565                  ' A&H IBNR - ' W-AH-IBNR-COUNT                   
01566          DISPLAY '    LIFE - ' W-LF-DE-COUNT                      
01567                  ' LIFE - IBNR - ' W-LF-IBNR-COUNT                
01568          DISPLAY '    REIN A&H  - ' W-AHR-DE-COUNT                
01569                  ' REIN A&H IBNR - ' W-AHR-IBNR-COUNT             
01570          DISPLAY '    REIN LIFE - ' W-LFR-DE-COUNT                
01571                  ' REIN LIFE - IBNR - ' W-LFR-IBNR-COUNT          
01572          DISPLAY 'AH PTC TOTALS - ' W-AH-PTC-TOTALS               
01573          DISPLAY 'AH FUTURE RESERVES TOTALS - '                   
01574              W-AH-FUTURE-TOTALS                                   
01575          DISPLAY 'AH IBNR RESERVES TOTALS - '                     
01576              W-AH-IBNR-TOTALS                                     
01577          DISPLAY 'LF PTC TOTALS - ' W-LF-PTC-TOTALS               
01578          DISPLAY 'LF FUTURE RESERVES TOTALS - '                   
01579              W-LF-FUTURE-TOTALS                                   
01580          DISPLAY 'LF IBNR RESERVES TOTALS - '                     
01581              W-LF-IBNR-TOTALS                                     
01582          DISPLAY 'AH REIN PTC TOTALS - ' W-AHR-PTC-TOTALS         
01583          DISPLAY 'AH REIN FUTURE RESERVES TOTALS - '              
01584              W-AHR-FUTURE-TOTALS                                  
01585          DISPLAY 'AH REIN IBNR RESERVES TOTALS - '                
01586              W-AHR-IBNR-TOTALS                                    
01587          DISPLAY 'LF REIN PTC TOTALS - ' W-LFR-PTC-TOTALS         
01588          DISPLAY 'LF REIN FUTURE RESERVES TOTALS - '              
01589              W-LFR-FUTURE-TOTALS                                  
01590          DISPLAY 'LF REIN IBNR RESERVES TOTALS - '                
01591              W-LFR-IBNR-TOTALS                                    
01592          MOVE '2'                TO OP-TABLE-FILE-STATUS-IND      
01593          CALL 'ELRSVSPL' USING OPTIONAL-CALCULATION-PASS-AREA     
01594      ELSE                                                         
01595          CLOSE CLAIM-HIST-IN.                                     
01596                                                                   
01597  0299-EXIT.                                                       
01598      EXIT.                                                        
01599  EJECT                                                            
01600  0300-PRINT-REPORT-SECT   SECTION.                                
01601                                                                   
01602  0330-OPEN-RTN.                                                   
01603                                                                   
01604      DISPLAY '     '.                                             
01605      DISPLAY '     '.                                             
01606      DISPLAY '     '.                                             
01607      DISPLAY '**************************************************'.
01608      DISPLAY '*****                                        *****'.
01609      DISPLAY '*****     SORT OUTPUT PROCEDURE SECTION      *****'.
01610      DISPLAY '*****                                        *****'.
01611      DISPLAY '**************************************************'.
01612      DISPLAY '     '.                                             
01613      DISPLAY '     '.                                             
01614                                                                   
01615      OPEN  OUTPUT  PEND-CLM-XTRACT.                               
01616                                                                   
01617  0340-PRS-SECT.                                                   
01618      RETURN DISK-SORT  AT END                                     
01619          GO TO 0930-END-1ST-PRINT.                                
01620                                                                   
01621      MOVE SORT-WORK  TO  WRK-X-REC.                               
01622                                                                   
01623      PERFORM LOAD-WRK-X-REC-DATES.                                
01624                                                                   
01625      ADD +1                      TO W-RELEASED-RCRD-COUNT.        
01626                                                                   
01627      IF W-REI-COMP  NOT = SPACES                                  
01628          ADD +1           TO  RE-TOT                              
01629          MOVE W-REI-COMP  TO  CHK-COMP  LAS-REI-CMP               
01630      ELSE                                                         
01631          MOVE W-COMP  TO  CHK-COMP.                               
01632                                                                   
01633      IF LAS-ACCT  = SPACES                                        
01634          MOVE W-ACCT      TO  LAS-ACCT                            
01635          MOVE W-ACC-NAME  TO  ACCT-NAME.                          
01636                                                                   
01637      IF LAS-COMP  = SPACES                                        
01638          MOVE CHK-COMP  TO  LAS-COMP.                             
01639                                                                   
01640      IF LAS-COMP  NOT = CHK-COMP                                  
01641          PERFORM 0630-ACCT-TOTALS  THRU  0660-EXIT                
01642          MOVE CHK-COMP  TO  LAS-COMP                              
01643          MOVE W-ACCT    TO  LAS-ACCT.                             
01644                                                                   
01645      IF LAS-ACCT  NOT = W-ACCT                                    
01646          PERFORM 0630-ACCT-TOTALS  THRU  0640-E-ACC-TOTS.         
01647                                                                   
01648      MOVE W-ACCT      TO  LAS-ACCT.                               
01649      MOVE W-ACC-NAME  TO  ACCT-NAME.                              
01650                                                                   
01651  0350-CALC-RESERVE.                                               
01652                                                                   
01653      IF  DTE-OPT-RESERVE-METHOD-UNAUTH                            
01654          MOVE ZEROS              TO W-RSRV                        
01655                                     W-RSRV-3                      
01656          IF  DTE-CLIENT  = 'GTL'                                  
01657                  AND                                              
01658              W-CLM-TYP   = '4'                                    
01659              NEXT SENTENCE                                        
01660          ELSE                                                     
01661              IF  W-CLM-TYP  = '1'  OR  '3'  OR  '4'               
01662                  GO TO 0540-WRITE-XTRC-OUT                        
01663              ELSE                                                 
01664                  NEXT SENTENCE                                    
01665      ELSE                                                         
01666          IF  W-CLM-TYP  = '1'  OR  '3'  OR  '4'                   
01667              ADD +1            TO W-REJECTED-BY-TYPE              
01668              WRITE XTR-OUT FROM WRK-X-REC                         
01669              GO TO 0340-PRS-SECT.                                 
01670                                                                   
01671      MOVE ZEROS  TO  STORE-5-THRU-18.                             
01672                                                                   
01673      COMPUTE INC-DT   =  WIN-MO  +  (WIN-CCYY  *  +12).           
01674      COMPUTE RPT-DT   =  WRV-MO  +  (WRV-CCYY  *  +12).           
01675      COMPUTE EFF-DT   =  WCE-MO  +  (WCE-CCYY  *  +12).           
01676      COMPUTE ENT-DT   =  WEN-MO  +  (WEN-CCYY  *  +12).           
01677      COMPUTE LMT-DT   =  WLM-MO  +  (WLM-CCYY  *  +12).           
01678      COMPUTE PAY1-DT  =  W1P-MO  +  (W1P-CCYY  *  +12).           
01679                                                                   
01680      IF PAY1-DT  = ZERO                                           
01681          MOVE VAL-DT  TO  PAY1-DT.                                
01682                                                                   
01683      IF RPT-DT  = ZERO                                            
01684          MOVE PAY1-DT  TO  RPT-DT.                                
01685                                                                   
01686      COMPUTE EXP-DT   =  EFF-DT  +  W-TERM.                       
01687                                                                   
01688      IF  DTE-OPT-RESERVE-METHOD-AUTH                              
01689          IF  W-CERT EQUAL 'ACTBENIBNR'                            
01690              GO TO 0510-OPT-PRINT-START                           
01691          ELSE                                                     
01692              IF  EXP-DT NOT GREATER THAN VAL-DT                   
01693                  MOVE ZEROS     TO  W-RSRV                        
01694                  WRITE XTR-OUT FROM WRK-X-REC                     
01695                  ADD +1     TO W-REJECTED-BY-DATE                 
01696                  GO TO 0340-PRS-SECT                              
01697              ELSE                                                 
01698                  IF  W-CLM-STA = 'C' OR 'F'                       
01699                      MOVE ZEROS  TO W-RSRV                        
01700                      WRITE XTR-OUT FROM WRK-X-REC                 
01701                      ADD +1      TO W-REJECTED-BY-STATUS          
01702                      GO TO 0340-PRS-SECT                          
01703                  ELSE                                             
01704                      GO TO 0510-OPT-PRINT-START.                  
01705                                                                   
01706      IF EXP-DT  NOT GREATER THAN  VAL-DT                          
01707          MOVE ZEROS  TO  W-RSRV                                   
01708          GO TO 0530-TEST-3MO-AGO.                                 
01709                                                                   
01710      IF W-CLM-STA  = 'C'  OR  'F'                                 
01711          GO TO 0530-TEST-3MO-AGO.                                 
01712                                                                   
01713      SUBTRACT INC-DT  FROM  VAL-DT  GIVING  WRK-DT.               
01714                                                                   
01715  0360-FIND-THE-TABLE.                                             
01716      IF W-PERM-DIS  NOT =  SPACE                                  
01717          GO TO 0370-AT-LEAST-6.                                   
01718                                                                   
01719      MOVE +01  TO  W-TBL.                                         
01720                                                                   
01721      IF WRK-DT  LESS THAN  +1                                     
01722          GO TO 0380-USE-TABLE-1.                                  
01723                                                                   
01724      MOVE +02  TO  W-TBL.                                         
01725                                                                   
01726      IF WRK-DT  LESS THAN  +2                                     
01727          GO TO 0390-USE-TABLE-2.                                  
01728                                                                   
01729      MOVE +03  TO  W-TBL.                                         
01730                                                                   
01731      IF WRK-DT  LESS THAN  +6                                     
01732          GO TO 0400-USE-TABLE-3.                                  
01733                                                                   
01734      MOVE +04  TO  W-TBL.                                         
01735                                                                   
01736      IF WRK-DT  LESS THAN  +12                                    
01737          GO TO 0410-USE-TABLE-4.                                  
01738                                                                   
01739      MOVE +05  TO  W-TBL.                                         
01740                                                                   
01741      IF WRK-DT  LESS THAN  +24                                    
01742          GO TO 0770-USE-TABLE-5.                                  
01743                                                                   
01744  0370-AT-LEAST-6.                                                 
01745      MOVE +06  TO  W-TBL.                                         
01746                                                                   
01747      IF WRK-DT  LESS THAN  +36                                    
01748          GO TO 0780-USE-TABLE-6.                                  
01749                                                                   
01750      MOVE +07  TO  W-TBL.                                         
01751                                                                   
01752      IF WRK-DT  LESS THAN  +48                                    
01753          GO TO 0790-USE-TABLE-7.                                  
01754                                                                   
01755      MOVE +08  TO  W-TBL.                                         
01756                                                                   
01757      IF WRK-DT  LESS THAN  +60                                    
01758          GO TO 0800-USE-TABLE-8.                                  
01759                                                                   
01760      MOVE +09  TO  W-TBL.                                         
01761                                                                   
01762      IF WRK-DT  LESS THAN  +72                                    
01763          GO TO 0810-USE-TABLE-9.                                  
01764                                                                   
01765      MOVE +10  TO  W-TBL.                                         
01766                                                                   
01767      IF WRK-DT  LESS THAN  +84                                    
01768          GO TO 0820-USE-TABLE-10.                                 
01769                                                                   
01770      MOVE +11  TO  W-TBL.                                         
01771                                                                   
01772      IF WRK-DT  LESS THAN  +96                                    
01773          GO TO 0830-USE-TABLE-11.                                 
01774                                                                   
01775      MOVE +12  TO  W-TBL.                                         
01776                                                                   
01777      IF WRK-DT  LESS THAN  +108                                   
01778          GO TO 0840-USE-TABLE-12.                                 
01779                                                                   
01780      MOVE +13  TO  W-TBL.                                         
01781                                                                   
01782      IF WRK-DT  LESS THAN  +120                                   
01783          GO TO 0850-USE-TABLE-13.                                 
01784                                                                   
01785      MOVE +14  TO  W-TBL.                                         
01786                                                                   
01787      IF WRK-DT  LESS THAN  +132                                   
01788          GO TO 0860-USE-TABLE-14.                                 
01789                                                                   
01790      MOVE +15  TO  W-TBL.                                         
01791                                                                   
01792      IF WRK-DT  LESS THAN  +144                                   
01793          GO TO 0870-USE-TABLE-15.                                 
01794                                                                   
01795      MOVE +16  TO  W-TBL.                                         
01796                                                                   
01797      IF WRK-DT  LESS THAN  +156                                   
01798          GO TO 0880-USE-TABLE-16.                                 
01799                                                                   
01800      MOVE +17  TO  W-TBL.                                         
01801                                                                   
01802      IF WRK-DT  LESS THAN  +168                                   
01803          GO TO 0890-USE-TABLE-17.                                 
01804                                                                   
01805      MOVE +18  TO  W-TBL.                                         
01806                                                                   
01807      IF WRK-DT  LESS THAN  +180                                   
01808          GO TO 0900-USE-TABLE-18.                                 
01809                                                                   
01810      MOVE +19  TO  W-TBL.                                         
01811                                                                   
01812      GO TO 0920-USE-TABLE-19.                                     
01813                                                                   
01814  0380-USE-TABLE-1.                                                
01815      MOVE T-01-VALUES  TO  STORE-1ST-FOUR-TABLES.                 
01816                                                                   
01817      GO TO 0420-WORK-UNDER-12-MONTHS.                             
01818                                                                   
01819  0390-USE-TABLE-2.                                                
01820      MOVE T-02-VALUES  TO  STORE-1ST-FOUR-TABLES.                 
01821                                                                   
01822      GO TO 0420-WORK-UNDER-12-MONTHS.                             
01823                                                                   
01824  0400-USE-TABLE-3.                                                
01825      MOVE T-03-VALUES  TO  STORE-1ST-FOUR-TABLES.                 
01826                                                                   
01827      GO TO 0420-WORK-UNDER-12-MONTHS.                             
01828                                                                   
01829  0410-USE-TABLE-4.                                                
01830      MOVE T-04-VALUES  TO  STORE-1ST-FOUR-TABLES.                 
01831                                                                   
01832  0420-WORK-UNDER-12-MONTHS.                                       
01833      INSPECT STORE-1ST-FOUR-TABLES CONVERTING ' ' TO '0'.         
01834                                                                   
01835      PERFORM 0560-FIND-CX  THRU  0570-EXIT.                       
01836                                                                   
01837      COMPUTE CY  =  EXP-DT  -  VAL-DT.                            
01838                                                                   
01839      IF CY  LESS THAN  +1                                         
01840          MOVE +1  TO  CY.                                         
01841                                                                   
01842      IF CY  LESS THAN  +25                                        
01843          MOVE CY  TO  CY-Y                                        
01844          GO TO 0440-INTERPOLATE-MAYBE.                            
01845                                                                   
01846      IF CY  GREATER THAN  +180                                    
01847          GO TO 0430-AGE-LIMS-U12.                                 
01848                                                                   
01849      COMPUTE CY-Y  =  ((CY  -  +24)  /  +12)  +  +24.             
01850                                                                   
01851      GO TO 0440-INTERPOLATE-MAYBE.                                
01852                                                                   
01853  0430-AGE-LIMS-U12.                                               
01854      COMPUTE CY  =  W-AGE  +  ((EXP-DT  -  EFF-DT)  /  +12).      
01855                                                                   
01856      MOVE +38  TO  CY-Y.                                          
01857                                                                   
01858      IF CY  GREATER THAN  +50                                     
01859          MOVE +39  TO  CY-Y.                                      
01860                                                                   
01861      IF CY  GREATER THAN  +55                                     
01862          MOVE +40  TO  CY-Y.                                      
01863                                                                   
01864      IF CY  GREATER THAN  +60                                     
01865          MOVE +41  TO  CY-Y.                                      
01866                                                                   
01867      IF CY  GREATER THAN  +65                                     
01868          MOVE +42  TO  CY-Y.                                      
01869                                                                   
01870      IF CY-Y  =  +41                                              
01871      AND CX-X  GREATER THAN  +6                                   
01872          MOVE +6  TO  CX-X.                                       
01873                                                                   
01874      IF CY-Y  =  +40                                              
01875      AND CX-X  GREATER THAN  +5                                   
01876          MOVE +5  TO  CX-X.                                       
01877                                                                   
01878      IF CY-Y  =  +39                                              
01879      AND CX-X  GREATER THAN  +4                                   
01880          MOVE +4  TO  CX-X.                                       
01881                                                                   
01882      IF CY-Y  =  +38                                              
01883      AND CX-X  GREATER THAN  +3                                   
01884          MOVE +3  TO  CX-X.                                       
01885                                                                   
01886  0440-INTERPOLATE-MAYBE.                                          
01887      IF INTER-SW  = '1'                                           
01888          GO TO 0460-ROUND-HIGH.                                   
01889                                                                   
01890      IF INTER-SW  = '3'                                           
01891          GO TO 0490-INTER-FACTOR.                                 
01892                                                                   
01893  0450-JUST-ROUND.                                                 
01894      ADD +.5  CX-X  GIVING  CX.                                   
01895                                                                   
01896      IF INTER-SW  = '4'                                           
01897          GO TO 0470-ROUND-Y-HIGH.                                 
01898                                                                   
01899      IF INTER-SW  = '5'                                           
01900          MOVE CX  TO  CX-X                                        
01901          GO TO 0490-INTER-FACTOR.                                 
01902                                                                   
01903      ADD +.5  CY-Y  GIVING  CY.                                   
01904                                                                   
01905      GO TO 0480-CALC-ROUND-FACTOR.                                
01906                                                                   
01907  0460-ROUND-HIGH.                                                 
01908      ADD +.999  CX-X  GIVING  CX.                                 
01909                                                                   
01910  0470-ROUND-Y-HIGH.                                               
01911      ADD +.999  CY-Y  GIVING  CY.                                 
01912                                                                   
01913  0480-CALC-ROUND-FACTOR.                                          
01914      MOVE S1-M-AGE (CY  CX)  TO  RSRV-FACT.                       
01915                                                                   
01916      GO TO 0500-FILL-REC-ETC.                                     
01917                                                                   
01918  0490-INTER-FACTOR.                                               
01919      MOVE CX-X  TO  CX-L.                                         
01920      MOVE CY-Y  TO  CY-L.                                         
01921                                                                   
01922      ADD +.999  CX-X  GIVING  CX-H.                               
01923      ADD +.999  CY-Y  GIVING  CY-H.                               
01924                                                                   
01925      SUBTRACT CX-L  FROM  CX-X  GIVING  CX-D.                     
01926      SUBTRACT CY-L  FROM  CY-Y  GIVING  CY-D.                     
01927                                                                   
01928      COMPUTE FACT-A  =  ((S1-M-AGE (CY-L, CX-H)  -                
01929                          S1-M-AGE (CY-L, CX-L))  *                
01930                         CX-D)  +  S1-M-AGE (CY-L, CX-L).          
01931                                                                   
01932      COMPUTE FACT-B  =  ((S1-M-AGE (CY-H, CX-H)  -                
01933                          S1-M-AGE (CY-H, CX-L))  *                
01934                         CX-D)  +  S1-M-AGE (CY-H, CX-L).          
01935                                                                   
01936      COMPUTE RSRV-FACT  =  ((FACT-B  -  FACT-A)  *  CY-D)         
01937                         +  FACT-A.                                
01938                                                                   
01939  0500-FILL-REC-ETC.                                               
01940      EXIT.                                                        
01941  EJECT                                                            
01942  0510-NOW-FILL-PRT.                                               
01943      COMPUTE W-RSRV  =  (W-MO-BEN  /  +100)  *  RSRV-FACT.        
01944                                                                   
01945      IF DTE-CLIENT = 'ITG'                                        
01946          IF W-TBL = 1  OR  2  OR  3  OR  4                        
01947              COMPUTE W-RSRV = W-RSRV * 1.20                       
01948          ELSE                                                     
01949              IF W-TBL = 5                                         
01950                  COMPUTE W-RSRV = W-RSRV * 1.05.                  
01951                                                                   
01952      IF DTE-CLIENT = 'SAL'                                        
01953          COMPUTE W-RSRV = W-RSRV * (92 / 100).                    
01954                                                                   
01955  0510-OPT-PRINT-START.                                            
01956                                                                   
01957      IF W-RSRV  NEGATIVE                                          
01958          MOVE ZERO  TO  W-RSRV.                                   
01959                                                                   
01960      IF  W-CERT EQUAL 'ACTBENIBNR'                                
01961          GO TO 0515-ACTBENIBNR-PROCESS.                           
01962                                                                   
01963      COMPUTE TOT-BEN                                              
01964          = W-MO-BEN * (EXP-DT - VAL-DT).                          
01965                                                                   
01966      IF  W-RSRV GREATER THAN TOT-BEN                              
01967          MOVE '*'                TO D1-AST                        
01968          MOVE TOT-BEN            TO W-RSRV                        
01969      ELSE                                                         
01970          MOVE SPACES             TO D1-AST.                       
01971                                                                   
01972      ADD W-RSRV    TO  A-TOT-RSRV.                                
01973      ADD W-MO-BEN  TO  A-TOT-BEN.                                 
01974                                                                   
01975      IF  DTE-OPT-RESERVE-METHOD-UNAUTH                            
01976          MOVE SPACE              TO W-IBNR                        
01977          IF  INC-DT LESS THAN (VAL-DT - +2)                       
01978                  AND                                              
01979              RPT-DT GREATER THAN (VAL-DT - +3)                    
01980              MOVE 'X'            TO W-IBNR.                       
01981                                                                   
01982      MOVE W-STATE  TO  STATE-L.                                   
01983                                                                   
01984      IF  W-STATE NOT EQUAL STATE-SUB (CLAS-INDEXS)                
01985          PERFORM 0580-LOCATE-STATE-RTN THRU 0580-EXIT.            
01986                                                                   
01987      MOVE CLAS-INDEXS  TO  CX.                                    
01988                                                                   
01989      IF W-REI-COMP  = SPACES                                      
01990          ADD W-RSRV  TO  ST-RSRV (CX).                            
01991                                                                   
01992      COMPUTE CX = RUN-CCYY - WIN-CCYY + 1.                        
01993                                                                   
01994      IF  CX GREATER THAN  +5                                      
01995          MOVE +6                 TO CX.                           
01996                                                                   
01997      IF W-REI-COMP  = SPACES                                      
01998          ADD W-RSRV  TO  YR-RSRV (CX).                            
01999                                                                   
02000      IF ((EXP-DT - VAL-DT)  GREATER THAN  +59)                    
02001      AND W-REI-COMP  = SPACES                                     
02002          ADD +1      TO  M60-COUNT                                
02003          ADD W-RSRV  TO  M60-RSRV.                                
02004                                                                   
02005      IF W-IBNR  = 'X'                                             
02006      AND W-REI-COMP  = SPACES                                     
02007          ADD +1        TO  LQTR-COUNT                             
02008          ADD W-MO-BEN  TO  LQTR-MOBEN.                            
02009                                                                   
02010      GO TO 0520-PRINT-RSRV-DET-LINE.                              
02011                                                                   
02012  0515-ACTBENIBNR-PROCESS.                                         
02013                                                                   
02014      ADD W-RSRV                  TO A-TOT-IBNR.                   
02015                                                                   
02016  0520-PRINT-RSRV-DET-LINE.                                        
02017      IF LINER GREATER +60                                         
02018          PERFORM 0730-HEAD-RTN  THRU  0760-EXIT.                  
02019                                                                   
02020      MOVE W-STATE                TO D1-ST                         
02021                                     D1A-ST.                       
02022      MOVE W-ACCT                 TO D1-ACCT                       
02023                                     D1A-ACCT.                     
02024      MOVE W-CERT                 TO D1-CERT                       
02025                                     D1A-CERT.                     
02026      MOVE WCE-MO                 TO D1-EFMO                       
02027                                     D1A-EFMO.                     
02028      MOVE WCE-YR                 TO D1-EFYR                       
02029                                     D1A-EFYR.                     
02030                                                                   
02031      IF  W-CERT = 'ACTBENIBNR'                                    
02032          MOVE SPACES             TO D1-AST                        
02033          MOVE W-RSRV             TO D1A-RESERVE                   
02034          MOVE 'IBNR'             TO D1A-IBNR                      
02035          MOVE DATA-1A            TO PRT                           
02036          GO TO 0520-ACTBENIBNR-BYPASS.                            
02037                                                                   
02038      MOVE WRV-MO   TO  D1-EMO.                                    
02039      MOVE WRV-YR   TO  D1-EYR.                                    
02040      MOVE W-TERM   TO  D1-OTRM.                                   
02041      MOVE W-AGE    TO  D1-OAGE.                                   
02042      MOVE W-CLMNO  TO  D1-CLM.                                    
02043                                                                   
02044      COMPUTE D1-DAGE = W-AGE + ((INC-DT - EFF-DT) / +12).         
02045                                                                   
02046      MOVE WIN-MO                 TO D1-IMO                        
02047      MOVE WIN-YR                 TO D1-IYR                        
02048                                                                   
02049      COMPUTE CX  =  (EXP-DT)   /  +12.                            
02050                                                                   
02051      MOVE CX  TO  D1-XYR.                                         
02052                                                                   
02053      COMPUTE CY-Y  =  ((EXP-DT  /  +12)  -  CX)  *  +12.          
02054                                                                   
02055      IF CY-Y  =  ZERO                                             
02056          MOVE +12  TO  CY-Y                                       
02057          COMPUTE D1-XYR  =  (EXP-DT  -  +1)  /  +12.              
02058                                                                   
02059      ADD CY-Y  +.99  GIVING  D1-XMO.                              
02060                                                                   
02061      COMPUTE D1-RTRM  =  EXP-DT  -  VAL-DT.                       
02062                                                                   
02063      MOVE W-MO-BEN  TO  D1-MOBEN.                                 
02064                                                                   
02065      IF  DTE-OPT-RESERVE-METHOD-AUTH                              
02066          MOVE W-CALC-INTEREST    TO D1-CALC-INTEREST              
02067          MOVE W-RSRV-FACT        TO D1-FACTOR-OPT                 
02068      ELSE                                                         
02069          MOVE W-TBL              TO D1-TABL                       
02070          MOVE RSRV-FACT          TO D1-FACTOR.                    
02071                                                                   
02072      IF  W-PERM-DIS  NOT = SPACES AND ZEROS                       
02073          MOVE 'PD'               TO D1-PERM-DIS                   
02074      ELSE                                                         
02075          MOVE SPACES             TO D1-PERM-DIS.                  
02076                                                                   
02077      MOVE W-RSRV                 TO D1-RESERVE.                   
02078                                                                   
02079      IF  W-IBNR  = 'X'                                            
02080          MOVE 'IBNR'             TO D1-IBNR                       
02081      ELSE                                                         
02082          MOVE SPACE              TO D1-IBNR.                      
02083                                                                   
02084      MOVE DATA-1                 TO PRT.                          
02085                                                                   
02086  0520-ACTBENIBNR-BYPASS.                                          
02087                                                                   
02088      MOVE ' '  TO  X.                                             
02089                                                                   
02090      PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                       
02091                                                                   
02092      ADD +1  TO  LINER                                            
02093                                                                   
02094      IF  W-CERT EQUAL 'ACTBENIBNR'                                
02095          ADD +1                  TO W-IBNR-COUNT                  
02096      ELSE                                                         
02097          ADD +1                  TO END-COUNT.                    
02098                                                                   
02099      IF  DTE-OPT-RESERVE-METHOD-AUTH                              
02100          WRITE XTR-OUT FROM WRK-X-REC                             
02101          GO TO 0340-PRS-SECT.                                     
02102                                                                   
02103  0530-TEST-3MO-AGO.                                               
02104      IF ENT-DT  GREATER THAN  VAL-RV3                             
02105          GO TO 0540-WRITE-XTRC-OUT.                               
02106                                                                   
02107      IF EXP-DT  NOT GREATER THAN  VAL-RV3                         
02108          GO TO 0540-WRITE-XTRC-OUT.                               
02109                                                                   
02110      MOVE VAL-DT   TO  SV-VAL.                                    
02111      MOVE VAL-RV3  TO  VAL-DT.                                    
02112                                                                   
02113      SUBTRACT INC-DT  FROM  VAL-DT  GIVING  WRK-DT.               
02114                                                                   
02115      IF W-CLM-STA  =  'C'                                         
02116      AND  LMT-DT  NOT GREATER THAN  VAL-RV3                       
02117         GO TO 0540-WRITE-XTRC-OUT.                                
02118                                                                   
02119      PERFORM 0360-FIND-THE-TABLE  THRU  0500-FILL-REC-ETC.        
02120                                                                   
02121      COMPUTE W-RSRV-3  =  (W-MO-BEN  /  +100)  *  RSRV-FACT.      
02122      COMPUTE TOT-BEN   =  W-MO-BEN  *  (EXP-DT  -  VAL-DT).       
02123                                                                   
02124      IF DTE-CLIENT = 'ITG'                                        
02125          IF W-TBL = 1  OR  2  OR  3  OR  4                        
02126              COMPUTE W-RSRV-3 = W-RSRV-3 * 1.20                   
02127          ELSE                                                     
02128              IF W-TBL = 5                                         
02129                  COMPUTE W-RSRV-3 = W-RSRV-3 * 1.05.              
02130                                                                   
02131      IF DTE-CLIENT = 'SAL'                                        
02132          COMPUTE W-RSRV-3 = W-RSRV-3 * (92 / 100).                
02133                                                                   
02134      IF W-RSRV-3  GREATER THAN  TOT-BEN                           
02135          MOVE TOT-BEN  TO  W-RSRV-3.                              
02136                                                                   
02137  0540-WRITE-XTRC-OUT.                                             
02138      MOVE SV-VAL  TO  VAL-DT.                                     
02139                                                                   
02140      COMPUTE PAYTO-DT  =  WPT-MO  +  (WPT-CCYY  *  +12).          
02141                                                                   
02142      IF PAYTO-DT  =  ZERO                                         
02143          MOVE W-INCUR  TO  W-PAY-TO                               
02144                            WS-W-PAY-TO-N                          
02145          COMPUTE PAYTO-DT  =  WPT-MO  +  (WPT-CCYY  *  +12).      
02146                                                                   
02147      COMPUTE INTER-RESULT  =  WPT-DA  /  RUN-DA.                  
02148      COMPUTE INTER-RESULT  =  +1.0  -  INTER-RESULT.              
02149      COMPUTE INTER-RESULT  =  (VAL-DT  -  PAYTO-DT)               
02150                            + INTER-RESULT.                        
02151      COMPUTE CATCH-UP      =  W-MO-BEN  *  INTER-RESULT.          
02152                                                                   
02153      IF CATCH-UP  NEGATIVE                                        
02154          MOVE ZEROS  TO  CATCH-UP.                                
02155                                                                   
02156      MOVE CATCH-UP  TO  W-PAY-CURR.                               
02157                                                                   
02158 ******************************************************************
02159      IF W-COMP = '2027304'     AND                                
02160         W-ACCT = '2730430000'  AND                                
02161         W-CERT = '0002619005 '                                    
02162          DISPLAY '     '                                          
02163          DISPLAY '     '                                          
02164          DISPLAY 'CLAIM CNTL: ' W-CLMNO '-' W-CARR '-' W-CERT ' ' 
02165              W-CRT-EFF ' AGE: ' W-AGE ' MO-BEN: ' W-MO-BEN        
02166              ' REINCO: ' W-REI-COMP                               
02167          DISPLAY '           REPORTED: ' W-RCVD                   
02168          DISPLAY '           INCURRED: ' W-INCUR                  
02169          DISPLAY '           ENTERED:  ' W-ENTER                  
02170          DISPLAY '           1ST PAID: ' W-1ST-PAY                
02171          DISPLAY '           PAID TO:  ' W-PAY-TO                 
02172          DISPLAY '           IBNR RSV: ' W-IBNR                   
02173          DISPLAY '           PAY CURR: ' W-PAY-CURR               
02174          DISPLAY '           RESERVE3: ' W-RSRV-3.                
02175 ******************************************************************
02176                                                                   
02177      WRITE XTR-OUT  FROM  WRK-X-REC.                              
02178                                                                   
02179      GO TO 0340-PRS-SECT.                                         
02180                                                                   
02181  0560-FIND-CX.                                                    
02182      COMPUTE DISAB-AGE  =  W-AGE                                  
02183                         +  ((INC-DT  -  EFF-DT)  /  +12).         
02184                                                                   
02185      IF DISAB-AGE  LESS THAN  +22                                 
02186          MOVE +22  TO  DISAB-AGE.                                 
02187                                                                   
02188      IF DISAB-AGE  GREATER THAN  +72                              
02189          MOVE +72  TO  DISAB-AGE.                                 
02190                                                                   
02191      COMPUTE CX-X  =  (DISAB-AGE  -  +17)  /  +5.                 
02192                                                                   
02193  0570-EXIT.                                                       
02194      EXIT.                                                        
02195  EJECT                                                            
02196  0580-LOCATE-STATE-RTN.                                           
02197                                                                   
02198      IF  CLAS-STARTS EQUAL ZERO                                   
02199          GO TO 0580-EXIT.                                         
02200                                                                   
02201      MOVE CLAS-STARTS            TO CLAS-INDEXS.                  
02202                                                                   
02203  0580-LOOP-STATE.                                                 
02204                                                                   
02205      IF  STATE-L EQUAL STATE-SUB (CLAS-INDEXS)                    
02206          GO TO 0580-EXIT.                                         
02207                                                                   
02208      IF  CLAS-INDEXS NOT GREATER THAN CLAS-MAXS                   
02209          ADD +001                TO CLAS-INDEXS                   
02210          GO TO 0580-LOOP-STATE.                                   
02211                                                                   
02212  0580-EXIT.                                                       
02213      EXIT.                                                        
02214                                                                   
02215  0600-REARRANGE-TITLE.                                            
02216                                                                   
02217      MOVE +0                     TO W-TIT-NDX.                    
02218                                                                   
02219      PERFORM 0602-MOVE-CHARACTER THRU 0602-EXIT                   
02220              VARYING                                              
02221          W-HDA-NDX FROM 1 BY 1                                    
02222              UNTIL                                                
02223          W-HDA-NDX GREATER THAN 39.                               
02224                                                                   
02225      COMPUTE W-HDA-NDX = (39 - W-TIT-NDX) / 2.                    
02226      MOVE SPACES                 TO HA-REPORT-TITLE.              
02227                                                                   
02228      PERFORM 0604-MOVE-CHARACTER THRU 0604-EXIT                   
02229              VARYING                                              
02230          W-TIT-NDX FROM 1 BY 1                                    
02231              UNTIL                                                
02232          W-TIT-NDX GREATER THAN 39.                               
02233                                                                   
02234  0600-EXIT.                                                       
02235      EXIT.                                                        
02236                                                                   
02237  0602-MOVE-CHARACTER.                                             
02238                                                                   
02239      IF  HD-A-CHAR (W-HDA-NDX) EQUAL SPACES                       
02240              AND                                                  
02241          (W-TIT-NDX EQUAL ZEROS                                   
02242                  OR                                               
02243              W-TIT-CHAR (W-TIT-NDX) EQUAL SPACES)                 
02244          GO TO 0602-EXIT.                                         
02245                                                                   
02246      ADD +1                      TO W-TIT-NDX.                    
02247      MOVE HD-A-CHAR (W-HDA-NDX)  TO W-TIT-CHAR (W-TIT-NDX).       
02248                                                                   
02249  0602-EXIT.                                                       
02250      EXIT.                                                        
02251                                                                   
02252  0604-MOVE-CHARACTER.                                             
02253                                                                   
02254      ADD +1                      TO W-HDA-NDX.                    
02255      MOVE W-TIT-CHAR (W-TIT-NDX) TO HD-A-CHAR (W-HDA-NDX).        
02256                                                                   
02257  0604-EXIT.                                                       
02258      EXIT.                                                        
02259                                  EJECT                            
02260  0610-PRT-RTN.                                                    
02261                              COPY ELCPRT2.                        
02262                                                                   
02263  0620-EXIT.                                                       
02264      EXIT.                                                        
02265  EJECT                                                            
02266  0630-ACCT-TOTALS.                                                
02267                                                                   
02268      IF  DTE-OPT-RESERVE-METHOD-AUTH                              
02269          IF  A-TOT-RSRV  =  ZERO                                  
02270                  AND                                              
02271              A-TOT-IBNR  =  ZERO                                  
02272              MOVE ZEROS          TO A-TOT-IBNR                    
02273                                     A-TOT-RSRV                    
02274              GO TO 0640-E-ACC-TOTS                                
02275          ELSE                                                     
02276              NEXT SENTENCE                                        
02277      ELSE                                                         
02278          IF  A-TOT-RSRV  =  ZERO                                  
02279              MOVE ZEROS          TO A-TOT-RSRV                    
02280              GO TO 0640-E-ACC-TOTS.                               
02281                                                                   
02282      ADD A-TOT-BEN   TO  C-TOT-BEN                                
02283                          F-TOT-BEN.                               
02284      MOVE A-TOT-BEN  TO  T1-MOBEN.                                
02285                                                                   
02286      ADD A-TOT-RSRV  TO  C-TOT-RSRV                               
02287                          F-TOT-RSRV.                              
02288      ADD A-TOT-IBNR  TO  C-TOT-IBNR                               
02289                          F-TOT-IBNR.                              
02290      MOVE A-TOT-RSRV TO  T1-RSRV.                                 
02291                                                                   
02292      MOVE ACCT-NAME  TO  T1-NAME.                                 
02293      MOVE TOTAL-1    TO  PRT.                                     
02294      MOVE ' '        TO  X.                                       
02295      PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                       
02296                                                                   
02297      IF  DTE-OPT-RESERVE-METHOD-AUTH                              
02298          MOVE A-TOT-IBNR         TO T1A-IBNR                      
02299          MOVE TOTAL-1A           TO PRT                           
02300          MOVE ' '                TO X                             
02301          ADD +1                  TO LINER                         
02302          PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                   
02303                                                                   
02304      MOVE ZEROS      TO  A-TOT-BEN                                
02305                          A-TOT-IBNR                               
02306                          A-TOT-RSRV.                              
02307                                                                   
02308      MOVE SPACES  TO  PRT.                                        
02309                                                                   
02310      PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                       
02311                                                                   
02312      ADD +2  TO  LINER.                                           
02313                                                                   
02314  0640-E-ACC-TOTS.                                                 
02315      EXIT.                                                        
02316                                                                   
02317  0650-COMP-TOTALS.                                                
02318                                                                   
02319      IF  DTE-OPT-RESERVE-METHOD-AUTH                              
02320          IF  C-TOT-RSRV  =  ZERO                                  
02321                  AND                                              
02322              C-TOT-IBNR  =  ZERO                                  
02323              MOVE ZEROS          TO C-TOT-IBNR                    
02324                                     C-TOT-RSRV                    
02325              GO TO 0660-EXIT                                      
02326          ELSE                                                     
02327              NEXT SENTENCE                                        
02328      ELSE                                                         
02329          IF  C-TOT-RSRV  =  ZERO                                  
02330              MOVE ZEROS          TO C-TOT-RSRV                    
02331              GO TO 0660-EXIT.                                     
02332                                                                   
02333      MOVE C-TOT-BEN     TO  T1-MOBEN.                             
02334      MOVE C-TOT-RSRV    TO  T1-RSRV.                              
02335      MOVE LAS-COMP      TO  T1-COMP-NUM.                          
02336      MOVE T1-COMP-NAME  TO  T1-NAME.                              
02337      MOVE TOTAL-1       TO  PRT.                                  
02338      MOVE ' '           TO  X.                                    
02339      PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                       
02340                                                                   
02341      IF  DTE-OPT-RESERVE-METHOD-AUTH                              
02342          MOVE C-TOT-IBNR         TO T1A-IBNR                      
02343          MOVE TOTAL-1A           TO PRT                           
02344          MOVE ' '                TO X                             
02345          ADD +1                  TO LINER                         
02346          PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                   
02347                                                                   
02348      MOVE ZEROS                  TO C-TOT-BEN                     
02349                                     C-TOT-IBNR                    
02350                                     C-TOT-RSRV.                   
02351                                                                   
02352      MOVE SPACES  TO  PRT.                                        
02353                                                                   
02354      PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                       
02355                                                                   
02356      ADD +2  TO  LINER.                                           
02357                                                                   
02358      IF RE-TOT  NOT = +1                                          
02359          GO TO 0660-EXIT.                                         
02360                                                                   
02361      IF  DTE-OPT-RESERVE-METHOD-AUTH                              
02362          MOVE END-COUNT          TO E-TOTCNT-CLMS                 
02363          MOVE W-IBNR-COUNT       TO E-TOTCNT-IBNR                 
02364          MOVE END-TOT-MES-OPT    TO T1-NAME                       
02365      ELSE                                                         
02366          MOVE END-COUNT          TO E-TOTCNT                      
02367          MOVE END-TOT-MES        TO T1-NAME.                      
02368                                                                   
02369      MOVE F-TOT-BEN   TO  T1-MOBEN.                               
02370      MOVE F-TOT-RSRV  TO  T1-RSRV.                                
02371      MOVE TOTAL-1     TO  PRT.                                    
02372      MOVE ' '         TO  X.                                      
02373      PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                       
02374                                                                   
02375      IF  DTE-OPT-RESERVE-METHOD-AUTH                              
02376          MOVE F-TOT-IBNR         TO T1A-IBNR                      
02377          MOVE TOTAL-1A           TO PRT                           
02378          MOVE ' '                TO X                             
02379          ADD +1                  TO LINER                         
02380          PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                   
02381                                                                   
02382      MOVE ZEROS                  TO END-COUNT                     
02383                                     F-TOT-BEN                     
02384                                     F-TOT-IBNR                    
02385                                     F-TOT-RSRV                    
02386                                     W-IBNR-COUNT.                 
02387                                                                   
02388      MOVE +80            TO  LINER.                               
02389      MOVE 'REINSURANCE'  TO  T1-COMP-1ST.                         
02390      MOVE 'COMPANY'      TO  T1-TITLE.                            
02391                                                                   
02392      ADD +1  TO  RE-TOT.                                          
02393                                                                   
02394  0660-EXIT.                                                       
02395      EXIT.                                                        
02396                                                                   
02397  0670-Z-STATES.                                                   
02398      MOVE ZERO  TO  ST-RSRV (CX).                                 
02399                                                                   
02400  0680-EZ-STS.                                                     
02401      EXIT.                                                        
02402                                                                   
02403  0690-Z-YEARS.                                                    
02404      MOVE ZERO    TO  YR-RSRV (CX).                               
02405      MOVE RUN-CCYY TO  YR-CCYY(CX).                               
02406                                                                   
02407      IF CX  NOT = +1                                              
02408          ADD +1       TO    YR-CCYY(CX)                           
02409          SUBTRACT CX  FROM  YR-CCYY(CX).                          
02410                                                                   
02411  0700-EZ-YRS.                                                     
02412      EXIT.                                                        
02413                                                                   
02414  0710-Z-LAH.                                                      
02415      MOVE ZERO  TO  L-AH-BEN (CX  CY).                            
02416                                                                   
02417  0720-E-ZLAH.                                                     
02418      EXIT.                                                        
02419  EJECT                                                            
02420  0730-HEAD-RTN.                                                   
02421      MOVE PAGER   TO  HC-PAGE.                                    
02422      ADD +1       TO  PAGER.                                      
02423      MOVE +10     TO  LINER.                                      
02424      MOVE HEAD-A  TO  PRT.                                        
02425      MOVE '1'     TO  X.                                          
02426                                                                   
02427      PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                       
02428                                                                   
02429      MOVE HEAD-B  TO  PRT.                                        
02430      MOVE ' '     TO  X.                                          
02431                                                                   
02432      PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                       
02433                                                                   
02434      MOVE HEAD-C  TO  PRT.                                        
02435                                                                   
02436      PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                       
02437                                                                   
02438      IF LAS-REI-CMP  NOT =  SPACES                                
02439          MOVE HEAD-C1  TO  PRT                                    
02440          PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                   
02441                                                                   
02442  0740-EXIT.                                                       
02443      EXIT.                                                        
02444                                                                   
02445  0750-HEADS-PART-2.                                               
02446      MOVE HEAD-D  TO  PRT.                                        
02447      MOVE '0'     TO  X.                                          
02448                                                                   
02449      PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                       
02450                                                                   
02451      MOVE HEAD-E  TO  PRT.                                        
02452      MOVE ' '     TO  X.                                          
02453                                                                   
02454      PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                       
02455                                                                   
02456      MOVE HEAD-F  TO  PRT.                                        
02457                                                                   
02458      PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                       
02459                                                                   
02460      MOVE SPACES  TO  PRT.                                        
02461                                                                   
02462      PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                       
02463                                                                   
02464  0760-EXIT.                                                       
02465      EXIT.                                                        
02466  EJECT                                                            
02467  0770-USE-TABLE-5.                                                
02468      MOVE T-05-VALUES  TO  STR-TAB-5.                             
02469      MOVE ZERO         TO  Y-LOC.                                 
02470                                                                   
02471      GO TO 0910-DO-YEAR-TABLES.                                   
02472                                                                   
02473  0780-USE-TABLE-6.                                                
02474      MOVE T-06-VALUES  TO  STR-TAB-6.                             
02475      MOVE +1           TO  Y-LOC.                                 
02476                                                                   
02477      GO TO 0910-DO-YEAR-TABLES.                                   
02478                                                                   
02479  0790-USE-TABLE-7.                                                
02480      MOVE T-07-VALUES  TO  STR-TAB-7.                             
02481      MOVE +2           TO  Y-LOC.                                 
02482                                                                   
02483      GO TO 0910-DO-YEAR-TABLES.                                   
02484                                                                   
02485  0800-USE-TABLE-8.                                                
02486      MOVE T-08-VALUES  TO  STR-TAB-8.                             
02487      MOVE +3           TO  Y-LOC.                                 
02488                                                                   
02489      GO TO 0910-DO-YEAR-TABLES.                                   
02490                                                                   
02491  0810-USE-TABLE-9.                                                
02492      MOVE T-09-VALUES  TO  STR-TAB-9.                             
02493      MOVE +4           TO  Y-LOC.                                 
02494                                                                   
02495      GO TO 0910-DO-YEAR-TABLES.                                   
02496                                                                   
02497  0820-USE-TABLE-10.                                               
02498      MOVE T-10-VALUES  TO  STR-TAB-10.                            
02499      MOVE +5           TO  Y-LOC.                                 
02500                                                                   
02501      GO TO 0910-DO-YEAR-TABLES.                                   
02502                                                                   
02503  0830-USE-TABLE-11.                                               
02504      MOVE T-11-VALUES  TO  STR-TAB-11.                            
02505      MOVE +6           TO  Y-LOC.                                 
02506                                                                   
02507      GO TO 0910-DO-YEAR-TABLES.                                   
02508                                                                   
02509  0840-USE-TABLE-12.                                               
02510      MOVE T-12-VALUES  TO  STR-TAB-12.                            
02511      MOVE +7           TO  Y-LOC.                                 
02512                                                                   
02513      GO TO 0910-DO-YEAR-TABLES.                                   
02514                                                                   
02515  0850-USE-TABLE-13.                                               
02516      MOVE T-13-VALUES  TO  STR-TAB-13.                            
02517      MOVE +8           TO  Y-LOC.                                 
02518                                                                   
02519      GO TO 0910-DO-YEAR-TABLES.                                   
02520                                                                   
02521  0860-USE-TABLE-14.                                               
02522      MOVE T-14-VALUES  TO  STR-TAB-14.                            
02523      MOVE +9           TO  Y-LOC.                                 
02524                                                                   
02525      GO TO 0910-DO-YEAR-TABLES.                                   
02526                                                                   
02527  0870-USE-TABLE-15.                                               
02528      MOVE T-15-VALUES  TO  STR-TAB-15.                            
02529      MOVE +10          TO  Y-LOC.                                 
02530                                                                   
02531      GO TO 0910-DO-YEAR-TABLES.                                   
02532                                                                   
02533  0880-USE-TABLE-16.                                               
02534      MOVE T-16-VALUES  TO  STR-TAB-16.                            
02535      MOVE +11          TO  Y-LOC.                                 
02536                                                                   
02537      GO TO 0910-DO-YEAR-TABLES.                                   
02538                                                                   
02539  0890-USE-TABLE-17.                                               
02540      MOVE T-17-VALUES  TO  STR-TAB-17.                            
02541      MOVE +12          TO  Y-LOC.                                 
02542                                                                   
02543      GO TO 0910-DO-YEAR-TABLES.                                   
02544                                                                   
02545  0900-USE-TABLE-18.                                               
02546      MOVE T-18-VALUES  TO  STR-TAB-18.                            
02547      MOVE +13          TO  Y-LOC.                                 
02548                                                                   
02549  0910-DO-YEAR-TABLES.                                             
02550      PERFORM 0560-FIND-CX  THRU  0570-EXIT.                       
02551                                                                   
02552      INSPECT STORE-5-THRU-18 CONVERTING ' ' TO '0'.               
02553                                                                   
02554      COMPUTE CY-Y  =  (EXP-DT  -  VAL-DT)  /  +12.                
02555                                                                   
02556      IF CY-Y  LESS THAN  +1                                       
02557          MOVE +1  TO  CY-Y.                                       
02558                                                                   
02559      ADD Y-LOC             TO  CY-Y.                              
02560      MOVE STORE-5-THRU-18  TO  STORE-1ST-FOUR-TABLES.             
02561                                                                   
02562      IF CY-Y  LESS THAN  +15                                      
02563          GO TO 0440-INTERPOLATE-MAYBE.                            
02564                                                                   
02565      MOVE +15  TO  CY-Y.                                          
02566                                                                   
02567      COMPUTE CY  =  W-AGE  +  ((EXP-DT  -  EFF-DT)  /  +12).      
02568                                                                   
02569      IF CY  GREATER THAN  +50                                     
02570          MOVE +16  TO  CY-Y.                                      
02571                                                                   
02572      IF CY  GREATER THAN  +55                                     
02573          MOVE +17  TO  CY-Y.                                      
02574                                                                   
02575      IF CY  GREATER THAN  +60                                     
02576          MOVE +18  TO  CY-Y.                                      
02577                                                                   
02578      IF CY  GREATER THAN  +65                                     
02579          MOVE +19  TO  CY-Y.                                      
02580                                                                   
02581      IF CY-Y  = +18                                               
02582      AND CX-X  GREATER THAN  +6                                   
02583          MOVE +6  TO  CX-X.                                       
02584                                                                   
02585      IF CY-Y  = +17                                               
02586      AND CX-X  GREATER THAN  +5                                   
02587          MOVE +5  TO  CX-X.                                       
02588                                                                   
02589      IF CY-Y  = +16                                               
02590      AND CX-X  GREATER THAN  +4                                   
02591          MOVE +4  TO  CX-X.                                       
02592                                                                   
02593      IF CY-Y  = +15                                               
02594      AND CX-X  GREATER THAN  +3                                   
02595          MOVE +3  TO  CX-X.                                       
02596                                                                   
02597      GO TO 0440-INTERPOLATE-MAYBE.                                
02598                                                                   
02599  0920-USE-TABLE-19.                                               
02600 *   COMPUTE CURRENT AGE                                           
02601                                                                   
02602      COMPUTE CY  =  W-AGE  +  ((VAL-DT  -  EFF-DT)  /  +12).      
02603                                                                   
02604      IF CY  LESS THAN  +30                                        
02605          MOVE +30  TO  CY.                                        
02606                                                                   
02607 *   COMPUTE AGE AT EXPIRATION                                     
02608                                                                   
02609      COMPUTE CX  =  W-AGE  +  ((EXP-DT  -  EFF-DT)  /  +12).      
02610                                                                   
02611      MOVE +1  TO  CX-X.                                           
02612                                                                   
02613      IF CX  GREATER THAN  +50                                     
02614          MOVE +2  TO  CX-X.                                       
02615                                                                   
02616      IF CX  GREATER THAN  +55                                     
02617          MOVE +3  TO  CX-X.                                       
02618                                                                   
02619      IF CX  GREATER THAN  +60                                     
02620          MOVE +4  TO  CX-X.                                       
02621                                                                   
02622      IF CX  GREATER THAN  +65                                     
02623          MOVE +5  TO  CX-X.                                       
02624                                                                   
02625      IF CX-X  = +1                                                
02626      AND CY  GREATER THAN  +49                                    
02627          MOVE +49  TO  CY.                                        
02628                                                                   
02629      IF CX-X  = +2                                                
02630      AND CY  GREATER THAN  +54                                    
02631          MOVE +54  TO  CY.                                        
02632                                                                   
02633      IF CX-X  = +3                                                
02634      AND CY  GREATER THAN  +59                                    
02635          MOVE +59  TO  CY.                                        
02636                                                                   
02637      IF CX-X  = +4                                                
02638      AND CY  GREATER THAN  +64                                    
02639          MOVE +64  TO  CY.                                        
02640                                                                   
02641      COMPUTE CY  =  CY  -  29.                                    
02642                                                                   
02643      MOVE CX-X                   TO  CX.                          
02644      MOVE T-19-FACTORS (CY  CX)  TO  RSRV-FACT.                   
02645                                                                   
02646      GO TO 0500-FILL-REC-ETC.                                     
02647                                                                   
02648  0930-END-1ST-PRINT.                                              
02649      IF NO-CLAIMS                                                 
02650      OR NO-OPEN-CLAIMS                                            
02651          GO TO 0935-NO-CLAIMS.                                    
02652                                                                   
02653      MOVE +1  TO  RE-TOT.                                         
02654                                                                   
02655      PERFORM 0630-ACCT-TOTALS  THRU  0660-EXIT.                   
02656                                                                   
02657      MOVE +80  TO  LINER.                                         
02658                                                                   
02659      GO TO 0939-CLOSE-RTN.                                        
02660                                                                   
02661  0935-NO-CLAIMS.                                                  
02662      PERFORM 0730-HEAD-RTN  THRU  0740-EXIT.                      
02663                                                                   
02664      IF NO-CLAIMS                                                 
02665          MOVE 'NO CLAIMS INPUT TO ECS-039'       TO  P-DATA       
02666      ELSE                                                         
02667          MOVE 'NO OPEN CLAIMS INPUT TO ECS-039'  TO  P-DATA.      
02668                                                                   
02669      MOVE '0'  TO  X.                                             
02670                                                                   
02671      PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                       
02672                                                                   
02673  0939-CLOSE-RTN.                                                  
02674      CLOSE  PEND-CLM-XTRACT.                                      
02675                                                                   
02676  0940-EXIT.                                                       
02677      EXIT.                                                        
02678  EJECT                                                            
02679  0960-SECOND-REPORT-PRINT SECTION.                                
02680                                                                   
02681  0970-OPEN-XTRCT.                                                 
02682      OPEN  INPUT   PEND-CLM-XTRACT                                
02683            OUTPUT  CLM-RSRV-SUMM.                                 
02684                                                                   
02685      IF NO-CLAIMS                                                 
02686      OR NO-OPEN-CLAIMS                                            
02687          GO TO 1160-END-2ND-REPORTS.                              
02688                                                                   
02689      MOVE ZERO    TO  RE-TOT.                                     
02690      MOVE 'X'     TO  SET-ABORT.                                  
02691      MOVE SPACES  TO  LAS-CNTRL  T1-COMP-1ST  CHK-COMP.           
02692      MOVE 'CARRIER/GRP'          TO T1-TITLE.                     
02693      MOVE HEAD-2A TO  HEAD-A.                                     
02694 *    MOVE 'B'     TO  HD-RPT-NO.                                  
02695      MOVE +1      TO  PAGER.                                      
02696                                                                   
02697  0980-READ-XTRCT.                                                 
02698      READ PEND-CLM-XTRACT  AT END                                 
02699          GO TO 1080-REPORT-STYLE-2-END.                           
02700                                                                   
02701                                                                   
02702      ADD +1        TO  W-PENDING-RCRD-COUNT.                      
02703                                                                   
02704      MOVE ' '      TO  SET-ABORT.                                 
02705      MOVE XTR-OUT  TO  WRK-X-REC.                                 
02706                                                                   
02707      PERFORM LOAD-WRK-X-REC-DATES.                                
02708                                                                   
02709      IF  DTE-OPT-RESERVE-METHOD-AUTH                              
02710          IF  W-CLM-TYP EQUAL '1' OR '3' OR '4'                    
02711              ADD +1           TO W-REJECTED-BY-TYPE               
02712              PERFORM 1050-MAKE-CLM-SUMM THRU 1060-EXIT            
02713              GO TO 0980-READ-XTRCT                                
02714          ELSE                                                     
02715              IF  W-RSRV EQUAL ZEROS                               
02716                  ADD +1       TO W-REJECTED-BY-RSRV               
02717                  GO TO 0980-READ-XTRCT                            
02718              ELSE                                                 
02719                  PERFORM 1050-MAKE-CLM-SUMM THRU 1060-EXIT        
02720      ELSE                                                         
02721          IF  W-RSRV = ZEROS                                       
02722              GO TO 0980-READ-XTRCT                                
02723          ELSE                                                     
02724              PERFORM 1050-MAKE-CLM-SUMM  THRU  1060-EXIT.         
02725                                                                   
02726      MOVE W-ACCT      TO  THS-ACCT.                               
02727      MOVE W-STATE     TO  THS-ST.                                 
02728      MOVE W-COMP      TO  THS-COMP.                               
02729      MOVE W-REI-COMP  TO  THS-REI-CMP.                            
02730      MOVE W-ACC-EXP   TO  THS-EXP.                                
02731      MOVE W-ACC-EFF   TO  THS-EFF.                                
02732      MOVE W-COMP      TO  CHK-COMP.                               
02733                                                                   
02734      IF W-REI-COMP  NOT = SPACES                                  
02735          MOVE W-REI-COMP  TO  CHK-COMP                            
02736          ADD +1           TO  RE-TOT.                             
02737                                                                   
02738      IF LAS-CNTRL  = SPACES                                       
02739          MOVE THS-CNTRL   TO  LAS-CNTRL                           
02740          MOVE W-ACC-NAME  TO  ACCT-NAME                           
02741          MOVE CHK-COMP    TO  LAS-CHK-COMP.                       
02742                                                                   
02743      IF THS-CNTRLA  NOT = LAS-CNTRLA                              
02744          PERFORM 0990-ACCT-TOTS-2ND  THRU  1020-EXIT.             
02745                                                                   
02746      MOVE THS-CNTRL   TO  LAS-CNTRL.                              
02747      MOVE CHK-COMP    TO  LAS-CHK-COMP.                           
02748      MOVE W-ACC-NAME  TO  ACCT-NAME.                              
02749                                                                   
02750      IF  DTE-OPT-RESERVE-METHOD-AUTH                              
02751          DISPLAY 'ALT OPT METHOD'                                 
02752          IF  W-CERT EQUAL 'ACTBENIBNR'                            
02753              ADD W-RSRV          TO TD2-IBNR                      
02754              ADD +1              TO TD2-CNT-IBNR                  
02755          ELSE                                                     
02756              ADD W-MO-BEN        TO TD2-MOBEN                     
02757              ADD +1              TO TD2-COUNTS                    
02758              ADD W-RSRV          TO TD2-RSRV                      
02759      ELSE                                                         
02760          DISPLAY 'STANDARD'                                       
02761          ADD W-MO-BEN            TO TD2-MOBEN                     
02762          ADD +1                  TO TD2-COUNTS                    
02763          ADD W-RSRV              TO TD2-RSRV                      
02764          IF W-IBNR  NOT = SPACES                                  
02765              ADD W-RSRV          TO TD2-IBNR                      
02766              ADD +1              TO TD2-CNT-IBNR.                 
02767                                                                   
02768      ADD W-PAY-CURR  TO  TD2-PTC.                                 
02769                                                                   
02770      IF W-RSRV-3  NOT = ZERO                                      
02771          ADD +1  TO  TD2-CNT-IBNR3.                               
02772                                                                   
02773      GO TO 0980-READ-XTRCT.                                       
02774  EJECT                                                            
02775  0990-ACCT-TOTS-2ND.                                              
02776      MOVE SPACES    TO  D2-COMP-FILL.                             
02777      MOVE LAS-ST    TO  D2-STATE.                                 
02778      MOVE LAS-ACCT  TO  D2-ACC.                                   
02779                                                                   
02780  1000-E-ACC-FILL.                                                 
02781      MOVE ACCT-NAME   TO  D2-NAME.                                
02782      MOVE TD2-MOBEN   TO  D2-MOBEN.                               
02783      MOVE TD2-RSRV    TO  D2-RSRV.                                
02784      MOVE TD2-IBNR    TO  D2-IBNR.                                
02785      MOVE TD2-PTC     TO  D2-PTC.                                 
02786      MOVE TD2-COUNTS  TO  D2-COUNTS.                              
02787                                                                   
02788      COMPUTE INTER-RESULT  =  TD2-MOBEN  /  +100.                 
02789                                                                   
02790      IF INTER-RESULT  = ZERO                                      
02791          MOVE +1  TO  INTER-RESULT.                               
02792                                                                   
02793      COMPUTE D2-CLM100  =  TD2-RSRV  /  INTER-RESULT.             
02794                                                                   
02795      IF TD2-CNT-IBNR   NOT = ZERO AND                             
02796         TD2-CNT-IBNR3  NOT = ZERO                                 
02797          COMPUTE D2-RATIO  ROUNDED  =  TD2-CNT-IBNR               
02798                                     /  TD2-CNT-IBNR3              
02799      ELSE                                                         
02800          MOVE SPACES  TO  D2-RAT-SP.                              
02801                                                                   
02802      IF LINER  GREATER THAN  +60                                  
02803          PERFORM 0730-HEAD-RTN  THRU  0740-EXIT                   
02804          MOVE HEAD-G  TO  PRT                                     
02805          MOVE '0'     TO  X                                       
02806          PERFORM 0610-PRT-RTN  THRU  0620-EXIT                    
02807          MOVE HEAD-H  TO  PRT                                     
02808          MOVE ' '     TO  X                                       
02809          PERFORM 0610-PRT-RTN  THRU  0620-EXIT                    
02810          MOVE HEAD-I  TO  PRT                                     
02811          PERFORM 0610-PRT-RTN  THRU  0620-EXIT                    
02812          MOVE +10     TO  LINER.                                  
02813                                                                   
02814      MOVE DATA-2  TO  PRT.                                        
02815      MOVE '0'     TO  X.                                          
02816                                                                   
02817      PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                       
02818                                                                   
02819      ADD +2          TO  LINER.                                   
02820      ADD TD2-MOBEN   TO  F-TD2-MOBEN   C-TD2-MOBEN.               
02821      ADD TD2-RSRV    TO  F-TD2-RSRV    C-TD2-RSRV.                
02822      ADD TD2-IBNR    TO  F-TD2-IBNR    C-TD2-IBNR.                
02823      ADD TD2-PTC     TO  F-TD2-PTC     C-TD2-PTC.                 
02824      ADD TD2-COUNTS  TO  F-TD2-COUNTS  C-TD2-COUNTS.              
02825                                                                   
02826      MOVE ZEROS  TO  TD2-PTC.                                     
02827      MOVE ZEROS  TO  TD2-MOBEN     TD2-RSRV       TD2-IBNR.       
02828      MOVE ZEROS  TO  TD2-CNT-IBNR  TD2-CNT-IBNR3  TD2-COUNTS.     
02829                                                                   
02830  1010-ACC-TOT-2-COMP-CK.                                          
02831      IF THS-COMP     NOT = LAS-COMP   OR                          
02832         THS-REI-CMP  NOT = LAS-REI-CMP                            
02833          PERFORM 1030-COMP-TOTALS-2ND THRU 1040-EXIT.             
02834                                                                   
02835  1020-EXIT.                                                       
02836      EXIT.                                                        
02837  EJECT                                                            
02838  1030-COMP-TOTALS-2ND.                                            
02839      MOVE SPACES        TO  D2-STATE.                             
02840      MOVE SPACES        TO  D2-ACC.                               
02841      MOVE C-TD2-MOBEN   TO  D2-MOBEN.                             
02842      MOVE C-TD2-RSRV    TO  D2-RSRV.                              
02843      MOVE C-TD2-IBNR    TO  D2-IBNR.                              
02844      MOVE C-TD2-PTC     TO  D2-PTC.                               
02845      MOVE C-TD2-COUNTS  TO  D2-COUNTS.                            
02846                                                                   
02847      COMPUTE INTER-RESULT  =  C-TD2-MOBEN  /  +100.               
02848                                                                   
02849      IF INTER-RESULT  = ZERO                                      
02850          MOVE +1  TO  INTER-RESULT.                               
02851                                                                   
02852      COMPUTE D2-CLM100  =  C-TD2-RSRV  /  INTER-RESULT.           
02853                                                                   
02854      MOVE LAS-CHK-COMP  TO  T1-COMP-NUM.                          
02855      MOVE T1-COMP-NAME  TO  D2-COMP-FILL.                         
02856      MOVE DATA-2        TO  PRT.                                  
02857      MOVE '0'           TO  X.                                    
02858                                                                   
02859      PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                       
02860                                                                   
02861      ADD +2      TO  LINER.                                       
02862      MOVE ZEROS  TO  C-TD2-MOBEN  C-TD2-RSRV    C-TD2-IBNR        
02863                      C-TD2-PTC    C-TD2-COUNTS.                   
02864                                                                   
02865      IF RE-TOT  NOT = +1                                          
02866          GO TO 1040-EXIT.                                         
02867                                                                   
02868      PERFORM 1090-RS-2-END-A.                                     
02869                                                                   
02870  1040-EXIT.                                                       
02871      EXIT.                                                        
02872                                                                   
02873  1050-MAKE-CLM-SUMM.                                              
02874 *    IF W-RSRV  = ZEROS                                           
02875 *        GO TO 1060-EXIT.                                         
02876                                                                   
02877      MOVE SPACES   TO  C-R-OUT.                                   
02878      MOVE +0       TO  C-R-RESV              C-R-REM-AMT          
02879                        C-R-FUT-RESERVE       C-R-PTC-RESERVE      
02880                        C-R-IBNR-RESERVE      C-R-CLM-ADJ-AMT      
02881                        C-R-EXPENSES          C-R-PAYMENTS         
02882                        C-R-OTH-COMMISSIONS   C-R-REIN-PREM-ADJS.  
02883                                                                   
02884      MOVE DTE-CLASIC-COMPANY-CD TO C-R-COMPANY-CD.                
02885      MOVE W-COMP   TO  C-R-COMP.                                  
02886      MOVE W-STATE  TO  C-R-ST.                                    
02887      MOVE W-ACCT   TO  C-R-ACCT.                                  
02888      MOVE WS-W-CRT-EFF   TO C-R-E-DTE.                            
02889                                                                   
02890      IF W-CLM-TYP  = '1'  OR  '3'                                 
02891          MOVE LIFE-OVERRIDE-L1  TO  C-R-LAH                       
02892      ELSE                                                         
02893          MOVE AH-OVERRIDE-L1    TO  C-R-LAH.                      
02894                                                                   
02895      MOVE W-CLM-BEN   TO  C-R-BEN.                                
02896      MOVE W-REI-COMP  TO  C-R-REI-CMP.                            
02897      MOVE W-PAY-CURR  TO  C-R-PTC-RESERVE.                        
02898                                                                   
02899      IF W-IBNR  NOT = SPACES                                      
02900          MOVE W-RSRV  TO  C-R-IBNR-RESERVE.                       
02901                                                                   
02902      MOVE W-RSRV   TO  C-R-FUT-RESERVE.                           
02903                                                                   
02904      MOVE ZEROS    TO  WS-C-R-DATE.                               
02905      MOVE RUN-MO   TO  C-R-MO.                                    
02906      MOVE RUN-YR   TO  C-R-YR.                                    
02907      MOVE RUN-CC   TO  C-R-CC.                                    
02908      MOVE WS-C-R-DATE-N TO C-R-DATE.                              
02909                                                                   
02910      WRITE C-R-OUT.                                               
02911                                                                   
02912  1060-EXIT.                                                       
02913      EXIT.                                                        
02914                                                                   
02915  1080-REPORT-STYLE-2-END.                                         
02916      IF SET-ABORT  = 'X'                                          
02917          GO TO 1160-END-2ND-REPORTS.                              
02918                                                                   
02919      MOVE +3  TO  RE-TOT.                                         
02920                                                                   
02921      PERFORM 0990-ACCT-TOTS-2ND    THRU  1000-E-ACC-FILL.         
02922                                                                   
02923      PERFORM 1030-COMP-TOTALS-2ND  THRU  1040-EXIT.               
02924                                                                   
02925      MOVE SPACES  TO  LAS-ACCT.                                   
02926      MOVE SPACES  TO  LAS-ST.                                     
02927                                                                   
02928  1090-RS-2-END-A.                                                 
02929      MOVE SPACES                TO  D2-COMP-FILL.                 
02930      MOVE SPACES                TO  D2-STATE.                     
02931      MOVE 'TOTAL ALL COMPANYS'  TO  ACCT-NAME.                    
02932      MOVE +10                   TO  LINER.                        
02933      MOVE F-TD2-MOBEN           TO  TD2-MOBEN.                    
02934      MOVE F-TD2-RSRV            TO  TD2-RSRV.                     
02935      MOVE F-TD2-IBNR            TO  TD2-IBNR.                     
02936      MOVE F-TD2-PTC             TO  TD2-PTC.                      
02937      MOVE F-TD2-COUNTS          TO  TD2-COUNTS.                   
02938                                                                   
02939      PERFORM 1000-E-ACC-FILL.                                     
02940                                                                   
02941      MOVE ZEROS          TO  F-TD2-MOBEN  F-TD2-RSRV  F-TD2-IBNR  
02942                              F-TD2-PTC    F-TD2-COUNTS.           
02943      MOVE ZEROS          TO  C-TD2-MOBEN  C-TD2-RSRV  C-TD2-IBNR  
02944                              C-TD2-PTC    C-TD2-COUNTS.           
02945      MOVE +80            TO  LINER.                               
02946      MOVE 'REINSURANCE'  TO  T1-COMP-1ST.                         
02947      MOVE 'COMPANY'      TO  T1-TITLE.                            
02948      ADD +1              TO  RE-TOT.                              
02949                                                                   
02950  1100-RS-2-END-B.                                                 
02951      MOVE SPACES  TO  LAS-REI-CMP.                                
02952      MOVE 'C'     TO  HD-RPT-NO.                                  
02953      MOVE +1      TO  PAGER.                                      
02954                                                                   
02955      PERFORM 0730-HEAD-RTN  THRU  0740-EXIT.                      
02956                                                                   
02957      MOVE HEAD-J  TO  PRT.                                        
02958      MOVE '0'     TO  X.                                          
02959                                                                   
02960      PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                       
02961                                                                   
02962      MOVE HEAD-K  TO  PRT.                                        
02963      MOVE ' '     TO  X.                                          
02964                                                                   
02965      PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                       
02966                                                                   
02967      MOVE HEAD-L  TO  PRT.                                        
02968                                                                   
02969      PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                       
02970                                                                   
02971  1110-REPORT-STYLE-3.                                             
02972      MOVE +1  TO  CX  CY.                                         
02973                                                                   
02974  1120-PRINT-LOOP-3.                                               
02975      IF CX  GREATER THAN  +6                                      
02976          MOVE SPACES  TO  D3-SPACE-1                              
02977          GO TO 1130-DO-3-STATE.                                   
02978                                                                   
02979      IF CX  =  +6                                                 
02980          MOVE D3-PRIOR  TO  D3-YR.                                
02981                                                                   
02982      IF CX  LESS THAN  +6                                         
02983          MOVE YR-CCYY(CX)(1:2) TO D3-NCC                          
02984          MOVE YR-CCYY(CX)(3:2) TO D3-NYR                          
02985          MOVE D3-FYR           TO  D3-YR.                         
02986                                                                   
02987      ADD YR-RSRV (CX)   TO  FD3-Y-RSRV.                           
02988      MOVE YR-RSRV (CX)  TO  D3-Y-RSRV.                            
02989      ADD +1             TO  CX.                                   
02990                                                                   
02991  1130-DO-3-STATE.                                                 
02992      IF CY  GREATER THAN  +75                                     
02993          MOVE SPACES  TO  D3-SPACE-2                              
02994          GO TO 1140-E-3PRT-CK.                                    
02995                                                                   
02996      IF ST-RSRV (CY)  NOT = ZERO                                  
02997          MOVE STATE-SUB (CY)  TO  D3-ST                           
02998          MOVE ST-RSRV (CY)    TO  D3-S-RSRV                       
02999          ADD ST-RSRV (CY)     TO  FD3-S-RSRV                      
03000          ADD +1               TO  CY                              
03001          GO TO 1140-E-3PRT-CK.                                    
03002                                                                   
03003      ADD +1  TO  CY.                                              
03004                                                                   
03005      GO TO 1130-DO-3-STATE.                                       
03006                                                                   
03007  1140-E-3PRT-CK.                                                  
03008      IF D3-SPACE-1  = SPACES  AND                                 
03009         D3-SPACE-2  = SPACES                                      
03010          MOVE FD3-Y-RSRV  TO  T3-Y-RSRV                           
03011          MOVE FD3-S-RSRV  TO  T3-S-RSRV                           
03012          MOVE TOTAL-3     TO  PRT                                 
03013          MOVE '0'         TO  X                                   
03014          PERFORM 0610-PRT-RTN  THRU  0620-EXIT                    
03015          GO TO 1150-REPORT-STYLE-4.                               
03016                                                                   
03017      MOVE DATA-3  TO  PRT.                                        
03018      MOVE '0'     TO  X.                                          
03019                                                                   
03020      PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                       
03021                                                                   
03022      GO TO 1120-PRINT-LOOP-3.                                     
03023                                                                   
03024  1150-REPORT-STYLE-4.                                             
03025      PERFORM 0730-HEAD-RTN  THRU  0740-EXIT.                      
03026                                                                   
03027      MOVE M60-COUNT   TO  HM-CNT.                                 
03028      MOVE M60-RSRV    TO  HM-RSRV.                                
03029      MOVE LQTR-COUNT  TO  HN-CNT.                                 
03030      MOVE LQTR-MOBEN  TO  HN-MOBEN.                               
03031      MOVE HEAD-M      TO  PRT.                                    
03032      MOVE '0'         TO  X.                                      
03033                                                                   
03034      PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                       
03035                                                                   
03036      IF  DTE-OPT-RESERVE-METHOD-UNAUTH                            
03037          MOVE HEAD-N  TO  PRT                                     
03038          PERFORM 0610-PRT-RTN  THRU  0620-EXIT.                   
03039                                                                   
03040  1160-END-2ND-REPORTS.                                            
03041      CLOSE  CLM-RSRV-SUMM                                         
03042             PEND-CLM-XTRACT                                       
03043             CLM-RSRV-REPT.                                        
03044                                                                   
03045  1170-CLOSE-PRINT.                                                
03046                              COPY ELCPRTC.                        
03047                                                                   
03048                              EJECT                                
03049  2000-PROCESS-DETAIL-EXTRACT SECTION.                             
03050                                                                   
03051      READ DETAIL-EXTRACT-IN INTO DETAIL-EXTRACT                   
03052          AT END                                                   
03053              GO TO 2000-EXIT.                                     
03054                                                                   
03055      IF  DE-TRANS NOT = 'Y'                                       
03056          GO TO 2000-PROCESS-DETAIL-EXTRACT.                       
03057                                                                   
03058      IF  DE-CERT NOT EQUAL 'ACTBENIBNR'                           
03059          GO TO 2000-PROCESS-DETAIL-EXTRACT.                       
03060                                                                   
03061      MOVE SPACES                 TO WRK-X-REC.                    
03062      MOVE HIGH-VALUES            TO W-CLMNO.                      
03063      MOVE ZEROS                  TO W-AGE                         
03064                                     WS-W-ACC-EXP-N                
03065                                     W-ACC-EXP                     
03066                                     W-CALC-INTEREST               
03067                                     W-CLMRESV                     
03068                                     W-EXP-DATE                    
03069                                     W-MO-BEN                      
03070                                     W-N-PYTS                      
03071                                     W-PAY-CURR                    
03072                                     W-REC-CTL                     
03073                                     W-REC-CTL1                    
03074                                     W-RSRV                        
03075                                     W-RSRV-FACT                   
03076                                     W-RSRV-3                      
03077                                     W-SKIP                        
03078                                     W-TERM                        
03079                                     W-TOTCLM                      
03080                                     WS-W-CRT-EFF-N                
03081                                     W-CRT-EFF                     
03082                                     WS-W-ENTER-N                  
03083                                     W-ENTER                       
03084                                     WS-W-LST-MAINT-N              
03085                                     W-LST-MAINT                   
03086                                     WS-W-INCUR-N                  
03087                                     W-INCUR                       
03088                                     WS-W-RCVD-N                   
03089                                     W-RCVD                        
03090                                     WS-W-1ST-PAY-N                
03091                                     W-1ST-PAY                     
03092                                     W-PAY-TO                      
03093                                     WS-W-PAY-TO-N.                
03094      MOVE DE-CARRIER             TO W-CARR                        
03095                                     W-CARRIER.                    
03096      MOVE DE-GROUPING            TO W-GROUPING.                   
03097      MOVE DE-STATE               TO W-STATE.                      
03098      MOVE DE-CERT                TO W-CERT.                       
03099      MOVE '9999999'              TO W-CLMNO.                      
03100      MOVE DE-ACCOUNT             TO W-ACCT.                       
03101      MOVE DE-TYPE                TO W-CLM-TYP.                    
03102                                                                   
03103      IF  DE-TYPE  = '1'  OR  '3'                                  
03104          MOVE DE-LF-TYPE         TO W-CLM-BEN                     
03105      ELSE                                                         
03106          MOVE DE-AH-TYPE         TO W-CLM-BEN.                    
03107                                                                   
03108      MOVE DE-EFF                 TO WS-W-CRT-EFF-N                
03109                                     W-CRT-EFF.                    
03110      MOVE '99'                   TO WEX-MO                        
03111                                     WEX-DA                        
03112                                     WEX-YR.                       
03113      MOVE DE-RSV-PROC-DT         TO WS-W-LST-MAINT-N              
03114                                     W-LST-MAINT.                  
03115      MOVE DE-ACC-EXP-DTE-RSV     TO W-ACC-EXP.                    
03116      MOVE DE-ACC-EFF-DTE-RSV     TO W-ACC-EFF.                    
03117                                                                   
03118      IF  DE-REIN  NOT =  SPACE                                    
03119          MOVE DE-REI-COMP        TO W-REI-COMP                    
03120          MOVE DE-REI-IBNR        TO W-RSRV                        
03121          IF  DE-TYPE  = '2'  OR  '4'                              
03122              ADD DE-REI-IBNR     TO W-AHR-IBNR-TOTALS             
03123              ADD +1              TO W-AH-IBNR-COUNT               
03124          ELSE                                                     
03125              ADD DE-REI-IBNR     TO W-LFR-IBNR-TOTALS             
03126              ADD +1              TO W-LF-IBNR-COUNT               
03127      ELSE                                                         
03128          MOVE DE-IBNR            TO W-RSRV                        
03129          IF  DE-TYPE  = '2'  OR  '4'                              
03130              ADD DE-IBNR         TO W-AH-IBNR-TOTALS              
03131              ADD +1              TO W-AHR-IBNR-COUNT              
03132          ELSE                                                     
03133              ADD DE-IBNR         TO W-LF-IBNR-TOTALS              
03134              ADD +1              TO W-LFR-IBNR-COUNT.             
03135                                                                   
03136      MOVE '*'                    TO W-IBNR.                       
03137      MOVE DE-RSV-ACC-NAME        TO W-ACC-NAME.                   
03138                                                                   
03139      MOVE WRK-X-REC              TO SORT-WORK.                    
03140                                                                   
03141      RELEASE SORT-WORK.                                           
03142                                                                   
03143 *****COPY ELCEXTM2.                                               
03144                                                                   
03145      WRITE UNSRTD-RSRV-REC FROM DETAIL-EXTRACT.                   
03146                                                                   
03147      ADD +1                      TO W-DETAIL-EXTRACT-CTR          
03148                                     W-DETAIL-RESERVES-CTR.        
03149                                                                   
03150      GO TO 2000-PROCESS-DETAIL-EXTRACT.                           
03151                                                                   
03152  2000-EXIT.                                                       
03153      EXIT.                                                        
03154                                  EJECT                            
03155  2100-GET-OPT-RESERVES SECTION.                                   
03156                                                                   
03157      MOVE DTE-CLIENT             TO CP-COMPANY-ID                 
03158                                     OP-COMPANY-ID.                
03159      MOVE ZERO                   TO CP-RETURN-CODE                
03160                                     OP-RETURN-CODE.               
03161                                                                   
03162      IF  DE-STATE NOT EQUAL STATE-SUB (CLAS-INDEXS)               
03163          MOVE DE-STATE           TO STATE-L                       
03164          PERFORM 0580-LOCATE-STATE-RTN THRU 0580-EXIT.            
03165                                                                   
03166      IF  CLAS-INDEXS NOT GREATER THAN CLAS-MAXS                   
03167          MOVE STATE-CALC-INTEREST (CLAS-INDEXS)                   
03168                                  TO OP-CALC-INTEREST              
03169                                     W-CALC-INTEREST               
03170      ELSE                                                         
03171          MOVE COMPANY-CALC-INTEREST                               
03172                                  TO OP-CALC-INTEREST              
03173                                     W-CALC-INTEREST.              
03174                                                                   
03175      PERFORM 2200-FIND-BENEFIT-DATA THRU 2200-EXIT.               
03176                                                                   
03177      MOVE SPACES                 TO CP-REM-TERM-METHOD.           
03178                                                                   
03179      MOVE CLAS-I-EP (W-BEN-NDX)  TO CP-EARNING-METHOD.            
03180      MOVE CLAS-I-RL-AH (W-BEN-NDX)                                
03181                                  TO CP-BENEFIT-TYPE.              
03182      MOVE CLAS-I-BAL (W-BEN-NDX) TO CP-SPECIAL-CALC-CD.           
03183                                                                   
03184      MOVE DTE-REM-TRM-CALC-OPTION                                 
03185                                  TO CP-REM-TRM-CALC-OPTION.       
03186                                                                   
03187      MOVE CLAS-CO-REM-TERM-CALC (W-BEN-NDX)                       
03188                                  TO CP-REM-TERM-METHOD.           
03189                                                                   
03190      IF  CP-REM-TERM-METHOD EQUAL SPACES                          
03191              OR                                                   
03192          CP-REM-TERM-METHOD EQUAL LOW-VALUES                      
03193          MOVE DTE-REM-TRM        TO CP-REM-TERM-METHOD.           
03194                                                                   
03195      PERFORM 2110-CONVERT-CERT-EFF-DT THRU 2110-EXIT.             
03196                                                                   
03197      PERFORM 2120-CALCULATE-EXPIRE-DT THRU 2120-EXIT.             
03198                                                                   
03199      IF  W-CERT-EXP-DT LESS THAN W-VALUATION-BIN-DATE             
03200          GO TO 2100-EXIT.                                         
03201                                                                   
03202      MOVE LOW-VALUES             TO CP-FIRST-PAY-DATE             
03203                                     CP-INSURED-BIRTH-DT.          
03204                                                                   
03205 **** NOTE:  SINCE THE REMAINING TERM/ REMAINING AMOUNT            
03206 **** CALCULATORS WILL DEFAULT THIS VALUE TO CERT EFFECTIVE DATE   
03207 **** + 1, AND SINCE THIS VALUE IS NOT PRESENT IN THE CLMS         
03208 **** RECORD, LOW VALUES IS INSERTED TO FORCE THE DEFAULT.         
03209                                                                   
03210      PERFORM 2125-CONVERT-INCURRED-DT THRU 2125-EXIT.             
03211                                                                   
03212      PERFORM 2130-CONVERT-PAID-TO-DT THRU 2130-EXIT.              
03213                                                                   
03214      PERFORM 2140-CONVERT-REPORTED-DT THRU 2140-EXIT.             
03215                                                                   
03216      IF  DE-DISABILITY                                            
03217          MOVE W-VALUATION-BIN-DATE                                
03218                                  TO CP-VALUATION-DT               
03219                                     OP-VALUATION-DT               
03220          MOVE 'A'                TO CP-CLAIM-TYPE                 
03221                                     OP-CLAIM-TYPE                 
03222          MOVE DE-AH-TERM         TO CP-ORIGINAL-TERM              
03223          MOVE W-MO-BEN           TO CP-ORIGINAL-BENEFIT           
03224                                     OP-ORIGINAL-BENEFIT           
03225          IF  DE-REIN  NOT =  SPACE                                
03226              MOVE DE-REI-AHPRM   TO CP-ORIGINAL-PREMIUM           
03227          ELSE                                                     
03228              MOVE DE-AH-PRM      TO CP-ORIGINAL-PREMIUM           
03229      ELSE                                                         
03230         MOVE W-INCUR             TO DC-GREG-DATE-CYMD             
03231         MOVE 'L'                 TO DC-OPTION-CODE                
03232         PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT               
03233         IF  NO-CONVERSION-ERROR                                   
03234             MOVE DC-BIN-DATE-1      TO CP-VALUATION-DT            
03235                                        OP-VALUATION-DT            
03236         ELSE                                                      
03237            MOVE '**** INCURRED DATE INVALID ****'                 
03238                                  TO WS-ABEND-MESSAGE              
03239            MOVE '99'               TO WS-AC-1-2                   
03240            MOVE '4'                TO WS-AC-3                     
03241            MOVE DC-ERROR-CODE      TO WS-AC-4                     
03242            MOVE WS-ABEND-CODE      TO WS-RETURN-CODE              
03243            GO TO ABEND-PGM                                        
03244          END-IF                                                   
03245          MOVE 'L'                TO CP-CLAIM-TYPE                 
03246                                     OP-CLAIM-TYPE                 
03247          MOVE DE-LF-TERM         TO CP-ORIGINAL-TERM              
03248                                     OP-ORIGINAL-TERM              
03249          MOVE DE-LF-BEN          TO CP-ORIGINAL-BENEFIT           
03250                                     OP-ORIGINAL-BENEFIT           
03251          MOVE DE-LF-PRM          TO CP-ORIGINAL-PREMIUM           
03252          MOVE DE-LF-BEN-ALT      TO CP-ALTERNATE-BENEFIT          
03253          IF  DE-REIN  NOT =  SPACE                                
03254              MOVE DE-REI-LFAMT   TO CP-ORIGINAL-BENEFIT           
03255                                     OP-ORIGINAL-BENEFIT           
03256              MOVE DE-REI-LFPRM   TO CP-ORIGINAL-PREMIUM           
03257          ELSE                                                     
03258              MOVE DE-LF-BEN      TO CP-ORIGINAL-BENEFIT           
03259              MOVE DE-LF-PRM      TO CP-ORIGINAL-PREMIUM.          
03260                                                                   
03261 *    MOVE CLASIC-CLAIMS-EOM-DT   TO CP-VALUATION-DT               
03262 *                                   OP-VALUATION-DT.              
03263                                                                   
03264      MOVE SPACES                 TO CP-ACCT-FLD-5                 
03265      MOVE W-AGE                  TO CP-ISSUE-AGE                  
03266                                     OP-ISSUE-AGE.                 
03267      MOVE COMPANY-CIDA-DISCOUNT  TO OP-CIDA-MOD-PERCENT.          
03268      MOVE '1'                    TO CP-CDT-METHOD                 
03269                                     OP-CIDA-METHOD.               
03270      MOVE 'O'                    TO CP-CLAIM-STATUS               
03271                                     OP-CLAIM-STATUS.              
03272      MOVE DE-APR                 TO CP-LOAN-APR.                  
03273                                                                   
03274      IF DE-PMT-FREQ NOT GREATER THAN ZERO                         
03275          MOVE +1                 TO DE-PMT-FREQ.                  
03276                                                                   
03277      MOVE DE-PMT-FREQ            TO CP-PAY-FREQUENCY.             
03278                                                                   
03279      MOVE '1'                    TO CP-PROCESS-TYPE.              
03280                                                                   
03281      MOVE ZERO                   TO CP-REMAINING-TERM-1           
03282                                     CP-REMAINING-TERM-2           
03283                                     CP-REMAINING-TERM-3           
03284                                     CP-REMAINING-TERM             
03285                                     CP-REMAINING-AMT              
03286                                     CP-REMAINING-AMT-PRV          
03287                                     CP-RESERVE-REMAINING-TERM     
03288                                     OP-RESERVE-FACTOR             
03289                                     OP-REMAINING-AMT              
03290                                     OP-PTC-RESERVE                
03291                                     OP-FUTURE-RESERVE             
03292                                     OP-RESERVE-REMAINING-TERM     
03293                                     OP-MONTHLY-EQUIV-PERCENT.     
03294                                                                   
03295      CALL 'ELRTRMX' USING CALCULATION-PASS-AREA.                  
03296                                                                   
03297      IF  CP-RETURN-CODE NOT = ZERO                                
03298          MOVE 'ERROR OCCURED REMAINING TERM CALCULATIONS'         
03299                                  TO WS-ABEND-MESSAGE              
03300          MOVE '90'               TO WS-AC-1-2                     
03301          MOVE '0'                TO WS-AC-3                       
03302          MOVE CP-RETURN-CODE     TO WS-AC-4                       
03303          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                
03304          GO TO ABEND-PGM.                                         
03305                                                                   
03306      MOVE CP-REMAINING-TERM-3    TO CP-REMAINING-TERM             
03307                                     OP-REMAINING-TERM.            
03308                                                                   
03309      IF  CP-REMAINING-TERM NOT GREATER THAN ZEROS                 
03310          MOVE ZEROS              TO W-PAY-CURR                    
03311                                     W-RSRV                        
03312                                     W-RSRV-FACT                   
03313          GO TO 2100-EXIT.                                         
03314                                                                   
CIDMOD     MOVE DE-LOAN-CLASS          TO CP-CLASS-CODE
03315      CALL 'ELRAMTX' USING CALCULATION-PASS-AREA.                  
03316                                                                   
03317      IF  CP-RETURN-CODE NOT = ZERO                                
03318              OR                                                   
03319          CP-REMAINING-AMT NOT NUMERIC                             
03320          MOVE 'ERROR OCCURED REMAINING AMOUNT CALCULATIONS'       
03321                                  TO WS-ABEND-MESSAGE              
03322          MOVE '90'               TO WS-AC-1-2                     
03323          MOVE '1'                TO WS-AC-3                       
03324          MOVE CP-RETURN-CODE     TO WS-AC-4                       
03325          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                
03326          GO TO ABEND-PGM.                                         
03327                                                                   
03328      MOVE CP-REMAINING-AMT       TO OP-REMAINING-AMT.             
03329                                                                   
03330      MOVE SPACES                 TO OP-PARM-DATA.                 
03331                                                                   
03332      CALL 'ELRSVSPL' USING OPTIONAL-CALCULATION-PASS-AREA.        
03333                                                                   
03334      IF  OP-RETURN-CODE NOT = ZERO                                
03335          MOVE 'ERROR OCCURED OPT RSVS CALCULATIONS'               
03336                                  TO WS-ABEND-MESSAGE              
03337          MOVE '90'               TO WS-AC-1-2                     
03338          MOVE '2'                TO WS-AC-3                       
03339          MOVE OP-RETURN-CODE     TO WS-AC-4                       
03340          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                
03341          GO TO ABEND-PGM.                                         
03342                                                                   
03343      IF  OP-FUTURE-RESERVE GREATER THAN OP-REMAINING-AMT          
03344          MOVE OP-REMAINING-AMT   TO OP-FUTURE-RESERVE.            
03345                                                                   
03346      IF  OP-FUTURE-RESERVE GREATER THAN ZERO                      
03347          MOVE OP-FUTURE-RESERVE  TO W-RSRV                        
03348          MOVE OP-RESERVE-FACTOR  TO W-RSRV-FACT                   
03349      ELSE                                                         
03350          MOVE ZEROS              TO W-RSRV                        
03351                                     OP-FUTURE-RESERVE             
03352                                     OP-RESERVE-FACTOR.            
03353                                                                   
03354      IF  OP-PTC-RESERVE LESS THAN ZEROS                           
03355          MOVE ZEROS              TO OP-PTC-RESERVE                
03356                                     W-PAY-CURR                    
03357      ELSE                                                         
03358          MOVE OP-PTC-RESERVE     TO W-PAY-CURR.                   
03359                                                                   
03360      PERFORM 2180-COMPLETE-DETAIL-RESERVE THRU 2180-EXIT.         
03361                                                                   
03362  2100-EXIT.                                                       
03363      EXIT.                                                        
03364                                  EJECT                            
03365  2110-CONVERT-CERT-EFF-DT.                                        
03366                                                                   
03367      MOVE W-CRT-EFF              TO DC-GREG-DATE-CYMD.            
03368      MOVE 'L'                    TO DC-OPTION-CODE.               
03369      PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT.                 
03370                                                                   
03371      IF  NO-CONVERSION-ERROR                                      
03372          MOVE DC-BIN-DATE-1      TO CP-CERT-EFF-DT                
03373                                     OP-CERT-EFF-DT                
03374      ELSE                                                         
03375          MOVE '**** CERT EFFECTIVE DATE INVALID ****'             
03376                                  TO WS-ABEND-MESSAGE              
03377          MOVE '99'               TO WS-AC-1-2                     
03378          MOVE '1'                TO WS-AC-3                       
03379          MOVE DC-ERROR-CODE      TO WS-AC-4                       
03380          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                
03381          GO TO ABEND-PGM.                                         
03382                                                                   
03383  2110-EXIT.                                                       
03384      EXIT.                                                        
03385                                  EJECT                            
03386  2120-CALCULATE-EXPIRE-DT.                                        
03387                                                                   
03388      MOVE LOW-VALUES             TO W-CERT-EXP-DT.                
03389                                                                   
03390      IF  DE-DISABILITY                                            
03391          MOVE DE-AH-TERM         TO OP-ORIGINAL-TERM              
03392      ELSE                                                         
03393          MOVE DE-LF-TERM         TO OP-ORIGINAL-TERM.             
03394                                                                   
03395      MOVE OP-CERT-EFF-DT         TO DC-BIN-DATE-1.                
03396      MOVE OP-ORIGINAL-TERM       TO DC-ELAPSED-MONTHS.            
03397      MOVE ZERO                   TO DC-ELAPSED-DAYS.              
03398      MOVE '6'                    TO DC-OPTION-CODE.               
03399      PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT.                 
03400                                                                   
03401      IF  NO-CONVERSION-ERROR                                      
03402          MOVE DC-BIN-DATE-2      TO W-CERT-EXP-DT                 
03403      ELSE                                                         
03404          MOVE '**** EXPIRE DATE INVALID ****'                     
03405                                  TO WS-ABEND-MESSAGE              
03406          MOVE '99'               TO WS-AC-1-2                     
03407          MOVE '2'                TO WS-AC-3                       
03408          MOVE DC-ERROR-CODE      TO WS-AC-4                       
03409          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                
03410          GO TO ABEND-PGM.                                         
03411                                                                   
03412  2120-EXIT.                                                       
03413      EXIT.                                                        
03414                                  EJECT                            
03415  2125-CONVERT-INCURRED-DT.                                        
03416                                                                   
03417      MOVE W-INCUR                TO DC-GREG-DATE-CYMD             
03418      MOVE 'L'                    TO DC-OPTION-CODE.               
03419      PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT.                 
03420                                                                   
03421      IF  NO-CONVERSION-ERROR                                      
03422          MOVE DC-BIN-DATE-1      TO CP-INCURRED-DT                
03423                                     OP-INCURRED-DT                
03424      ELSE                                                         
03425          MOVE '**** INCURRED DATE INVALID ****'                   
03426                                  TO WS-ABEND-MESSAGE              
03427          MOVE '99'               TO WS-AC-1-2                     
03428          MOVE '3'                TO WS-AC-3                       
03429          MOVE DC-ERROR-CODE      TO WS-AC-4                       
03430          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                
03431          GO TO ABEND-PGM.                                         
03432                                                                   
03433  2125-EXIT.                                                       
03434      EXIT.                                                        
03435                                  EJECT                            
03436  2130-CONVERT-PAID-TO-DT.                                         
03437                                                                   
03438      IF  W-PAY-TO GREATER THAN ZEROS                              
03439          MOVE W-PAY-TO           TO DC-GREG-DATE-CYMD             
03440          MOVE 'L'                TO DC-OPTION-CODE                
03441          PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT              
03442          IF  NO-CONVERSION-ERROR                                  
03443              MOVE DC-BIN-DATE-1  TO CP-PAID-THRU-DT               
03444                                     OP-PAID-THRU-DT               
03445          ELSE                                                     
03446              MOVE '**** PAID TO DATE INVALID ****'                
03447                                  TO WS-ABEND-MESSAGE              
03448              MOVE '99'           TO WS-AC-1-2                     
03449              MOVE '4'            TO WS-AC-3                       
03450              MOVE DC-ERROR-CODE  TO WS-AC-4                       
03451              MOVE WS-ABEND-CODE  TO WS-RETURN-CODE                
03452              GO TO ABEND-PGM                                      
03453      ELSE                                                         
03454          MOVE OP-INCURRED-DT     TO CP-PAID-THRU-DT               
03455                                     OP-PAID-THRU-DT.              
03456                                                                   
03457  2130-EXIT.                                                       
03458      EXIT.                                                        
03459                                  EJECT                            
03460  2140-CONVERT-REPORTED-DT.                                        
03461                                                                   
03462      IF  W-RCVD GREATER THAN ZEROS                                
03463          MOVE W-RCVD             TO DC-GREG-DATE-CYMD             
03464          MOVE 'L'                TO DC-OPTION-CODE                
03465          PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT              
03466          IF  NO-CONVERSION-ERROR                                  
03467              MOVE DC-BIN-DATE-1  TO CP-REPORTED-DT                
03468                                     OP-REPORTED-DT                
03469          ELSE                                                     
03470              MOVE '**** RCVD/REPORTED DATE INVALID ****'          
03471                                  TO WS-ABEND-MESSAGE              
03472              MOVE '99'           TO WS-AC-1-2                     
03473              MOVE '5'            TO WS-AC-3                       
03474              MOVE DC-ERROR-CODE  TO WS-AC-4                       
03475              MOVE WS-ABEND-CODE  TO WS-RETURN-CODE                
03476              GO TO ABEND-PGM                                      
03477      ELSE                                                         
03478          MOVE CP-FIRST-PAY-DATE  TO CP-REPORTED-DT                
03479                                     OP-REPORTED-DT.               
03480                                                                   
03481  2140-EXIT.                                                       
03482      EXIT.                                                        
03483                                  EJECT                            
03484  2180-COMPLETE-DETAIL-RESERVE.                                    
03485                                                                   
03486      IF  DE-DISABILITY                                            
03487              AND                                                  
03488          (W-CLM-TYP = 'C' OR 'F')                                 
03489          MOVE ZEROS              TO OP-FUTURE-RESERVE.            
03490                                                                   
03491      IF  OP-FUTURE-RESERVE NOT GREATER THAN ZEROS                 
03492          GO TO 2180-EXIT.                                         
03493                                                                   
03494      MOVE 'Y'                    TO DE-TRANS.                     
03495      MOVE DETAIL-EXTRACT         TO UNSRTD-RSRV-REC.              
03496      MOVE ZEROS                  TO UR-IBNR                       
03497                                     UR-REI-IBNR.                  
03498                                                                   
03499      IF  DE-REIN EQUAL SPACE                                      
03500          MOVE OP-FUTURE-RESERVE  TO UR-FUTRSV                     
03501          MOVE OP-PTC-RESERVE     TO UR-PAYCUR                     
03502          MOVE ZEROS              TO UR-REI-FUTRSV                 
03503                                     UR-REI-PAYCUR                 
03504          IF  DE-DISABILITY                                        
03505              ADD OP-PTC-RESERVE  TO W-AH-PTC-TOTALS               
03506              ADD OP-FUTURE-RESERVE                                
03507                                  TO W-AH-FUTURE-TOTALS            
03508              ADD +1              TO W-AH-DE-COUNT                 
03509          ELSE                                                     
03510              ADD OP-PTC-RESERVE  TO W-LF-PTC-TOTALS               
03511              ADD OP-FUTURE-RESERVE                                
03512                                  TO W-LF-FUTURE-TOTALS            
03513              ADD +1              TO W-LF-DE-COUNT                 
03514      ELSE                                                         
03515          MOVE OP-FUTURE-RESERVE  TO UR-REI-FUTRSV                 
03516          MOVE OP-PTC-RESERVE     TO UR-REI-PAYCUR                 
03517          MOVE ZEROS              TO UR-FUTRSV                     
03518                                     UR-PAYCUR                     
03519          IF  DE-DISABILITY                                        
03520              ADD OP-PTC-RESERVE  TO W-AHR-PTC-TOTALS              
03521              ADD OP-FUTURE-RESERVE                                
03522                                  TO W-AHR-FUTURE-TOTALS           
03523              ADD +1              TO W-AHR-DE-COUNT                
03524          ELSE                                                     
03525              ADD +1              TO W-LFR-DE-COUNT                
03526              ADD OP-PTC-RESERVE  TO W-LFR-PTC-TOTALS              
03527              ADD OP-FUTURE-RESERVE                                
03528                                  TO W-LFR-FUTURE-TOTALS.          
03529                                                                   
03530      MOVE DE-REPORTED            TO UR-RSV-REPORTED.              
03531      MOVE DE-INCUR               TO UR-RSV-INCUR.                 
03532      MOVE DE-PAID-TO             TO UR-RSV-PAYTO.                 
03533      MOVE DE-CNUM                TO UR-CLMNO.                     
03534      MOVE DE-ACC-NAME            TO UR-RSV-ACC-NAME.              
03535      MOVE DE-ACC-EXP-DTE         TO UR-ACC-EXP-DTE-RSV.           
03536      MOVE DE-ACC-EFF-DTE         TO UR-ACC-EFF-DTE-RSV.           
03537      MOVE W-VALUATION-DATE       TO UR-RSV-PROC-DT.               
03538                                                                   
03539      WRITE UNSRTD-RSRV-REC.                                       
03540      ADD +1                      TO W-DETAIL-RESERVES-CTR.        
03541                                                                   
03542  2180-EXIT.                                                       
03543      EXIT.                                                        
03544                                  EJECT                            
03545  2200-FIND-BENEFIT-DATA.                                          
03546                                                                   
03547      IF  DE-DISABILITY                                            
03548          MOVE DE-AH-TYPE         TO W-BENEFIT                     
03549          MOVE CLAS-MAXA          TO W-BEN-MAX                     
03550          MOVE CLAS-STARTA        TO W-BEN-START                   
03551      ELSE                                                         
03552          MOVE DE-LF-TYPE         TO W-BENEFIT                     
03553          MOVE CLAS-MAXL          TO W-BEN-MAX                     
03554          MOVE CLAS-STARTL        TO W-BEN-START.                  
03555                                                                   
03556      PERFORM 2210-LOOP-BENEFIT THRU 2210-EXIT                     
03557              VARYING                                              
03558          W-BEN-NDX FROM W-BEN-START BY +1                         
03559              UNTIL                                                
03560          W-BEN-NDX GREATER THAN W-BEN-MAX                         
03561              OR                                                   
03562          CLAS-I-BEN (W-BEN-NDX) EQUAL W-BENEFIT.                  
03563                                                                   
03564      IF  W-BEN-NDX GREATER THAN W-BEN-MAX                         
03565          MOVE 'BENEFIT CODE XX NOT MATCHED '                      
03566                                  TO WS-ABEND-MESSAGE              
03567          MOVE W-BENEFIT          TO WS-AM-CODE                    
03568          MOVE '0100'             TO WS-RETURN-CODE                
03569          GO TO ABEND-PGM.                                         
03570                                                                   
03571  2200-EXIT.                                                       
03572      EXIT.                                                        
03573                                  EJECT                            
03574  2210-LOOP-BENEFIT.                                               
03575                                                                   
03576 ****DELIBERTLY LEFT BLANK.                                        
03577                                                                   
03578  2210-EXIT.                                                       
03579      EXIT.                                                        
03580                                  EJECT                            
03581  COPY ELCDCS.                                                     
03582                                                                   
03583                                                                   
03584  LOAD-WRK-X-REC-DATES.                                            
03585                                                                   
03586      MOVE W-CRT-EFF    TO  WS-W-CRT-EFF-N.                        
03587      MOVE W-RCVD       TO  WS-W-RCVD-N.                           
03588      MOVE W-INCUR      TO  WS-W-INCUR-N.                          
03589      MOVE W-ENTER      TO  WS-W-ENTER-N.                          
03590      MOVE W-PAY-TO     TO  WS-W-PAY-TO-N.                         
03591      MOVE W-LST-MAINT  TO  WS-W-LST-MAINT-N.                      
03592      MOVE W-1ST-PAY    TO  WS-W-1ST-PAY-N.                        
03593      MOVE W-ACC-EXP    TO  WS-W-ACC-EXP-N.                        
03594                                                                   
03595                                                                   
03596  ABEND-PGM SECTION.                                               
03597                              COPY ELCABEND.                       
03598                                                                   
