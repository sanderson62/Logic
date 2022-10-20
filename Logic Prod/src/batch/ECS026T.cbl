00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                ECS026.                               
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 05/13/94 09:27:38.                 
00007 *             PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE           
00008 *                           VMOD=2.021.                           
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
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *
00022 *            *                                                   *
00023 *            *****************************************************
00024                                                                   
00025                                                                   
00026 ******************************************************************
00027 *        THIS PROGRAM GENERATES THE EXPERIENCE ANALYSIS          *
00028 *        REPORT.                                                 *
00029 ******************************************************************
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING  
122002******************************************************************
00030                                                                   
00031  ENVIRONMENT DIVISION.                                            
00032  INPUT-OUTPUT SECTION.                                            
00033  FILE-CONTROL.                                                    
00034                                                                   
00035      SELECT  SORT-WORK       ASSIGN TO SYS001-UT-2314-S-SORTWK1.  
00036      SELECT  SORT-WORK-2     ASSIGN TO SYS001-UT-2314-S-SORTWK1.  
00037      SELECT  PRINTER         ASSIGN TO SYS008-UR-1403-S-SYS008.   
00038                                                                   
00039      SELECT  ACCT-IN         ASSIGN TO SYS021-FBA1-ERACCTT        
00040                              ORGANIZATION    INDEXED              
00041                              ACCESS          SEQUENTIAL           
00042                              RECORD KEY      AM-CONTROL-PRIMARY   
00043                              FILE STATUS     ERACCT-FILE-STATUS.  
00044                                                                   
00045      SELECT  ERRTBL-IN       ASSIGN TO SYS021-FBA1-ERRTBLT
00046                              ORGANIZATION    INDEXED              
00047                              ACCESS          DYNAMIC              
00048                              RECORD KEY      RE-CONTROL-PRIMARY   
00049                              FILE STATUS     ERRTBL-FILE-STATUS.  
00050                                                                   
00051      SELECT  CERT-IN         ASSIGN TO SYS011-UT-2400-S-SYS011.   
00052      SELECT  DISK-DATE       ASSIGN TO SYS019-UT-2314-S-SYS019.   
00053      SELECT  FICH            ASSIGN TO SYS020-UT-2400-S-SYS020.   
00054      SELECT  REIN-WORK       ASSIGN TO SYS030-UT-FBA1-S-SYS030.   
00055      SELECT  TEMP-WORK       ASSIGN TO SYS031-UT-FBA1-S-SYS031.   
00056  EJECT                                                            
00057  DATA DIVISION.                                                   
00058  FILE SECTION.                                                    
00059                                                                   
00060  SD  SORT-WORK.                                                   
00061                                                                   
00062  01  SORT-REC.                                                    
00063      12  SR-CONTROL          PIC X(20).                           
00064      12  FILLER              PIC X(1121).                         
00065                                                                   
00066  01  SORT-REC-B.                                                  
00067      12  FILLER              PIC X(20).                           
00068      12  SR-CONTROL-B        PIC X(10).                           
00069      12  FILLER              PIC X(1111).                         
00070                                                                   
00071  01  SORT-REC-C.                                                  
00072      12  FILLER              PIC X.                               
00073      12  SR-CONTROL-C1       PIC X(7).                            
00074      12  FILLER              PIC X(22).                           
00075      12  SR-CONTROL-C2       PIC X(10).                           
00076      12  FILLER              PIC X(1101).                         
00077                                                                   
00078  SD  SORT-WORK-2.                                                 
00079                                                                   
00080  01  SORT-REC-2.                                                  
00081      12  SR2-CONTROL         PIC X(4).                            
00082      12  FILLER              PIC X(120).                          
00083  EJECT                                                            
00084  FD  PRINTER                                                      
00085                              COPY ELCPRTFD.                       
00086  EJECT                                                            
00087  FD  ACCT-IN.                                                     
00088                              COPY ERCACCT.                        
00089  EJECT                                                            
00090  FD  ERRTBL-IN.
00091                              COPY ERCREIN.                        
00092  EJECT                                                            
00093  FD  CERT-IN                                                      
00094      RECORDING MODE F.                                            
00095                              COPY ECSCRT01.                       
00096  EJECT                                                            
00097  FD  REIN-WORK                                                    
00098      RECORDING MODE F                                             
pemuni     BLOCK CONTAINS 0 RECORDS.
00100                                                                   
00101  01  REIN-WORK-REC           PIC X(124).                          
00102                                                                   
00103  FD  TEMP-WORK                                                    
00104      RECORDING MODE F                                             
pemuni     BLOCK CONTAINS 0 RECORDS.
00106                                                                   
00107  01  TEMP-WORK-REC           PIC X(1141).                         
00108  EJECT                                                            
00109  FD  DISK-DATE                                                    
00110                              COPY ELCDTEFD.                       
00111                                                                   
00112  FD  FICH                                                         
00113                              COPY ELCFCHFD.                       
00114  EJECT                                                            
00115  WORKING-STORAGE SECTION.                                         
00116  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      
00117  77  FILLER  PIC X(32) VALUE '********************************'.  
00118  77  FILLER  PIC X(32) VALUE '     ECS026 WORKING STORAGE     '.  
00119  77  FILLER  PIC X(32) VALUE '******** VMOD=2.021 ************'.  
00120                                                                   
00121  77  PGM-SUB                 PIC S9(4)  COMP     VALUE +026.      
00122  77  A-SUB1                  PIC S9(4)  COMP     VALUE +0.        
00123  77  A-SUB2                  PIC S9(4)  COMP     VALUE +0.        
00124  77  P-SUB1                  PIC S9(4)  COMP     VALUE +0.        
00125  77  P-SUB2                  PIC S9(4)  COMP     VALUE +0.        
00126  77  SUB5                    PIC S9(4)  COMP     VALUE +0.        
00127  77  S1                      PIC S9(4)  COMP     VALUE +0.        
00128  77  CERT-END-SW             PIC X               VALUE 'N'.       
00129      88  END-OF-CERT                             VALUE 'Y'.       
00130  77  ACCT-END-SW             PIC X               VALUE 'N'.       
00131      88  END-OF-ACCT                             VALUE 'Y'.       
00132  77  EXTR-END-SW             PIC X               VALUE 'N'.       
00133      88  END-OF-EXTR                             VALUE 'Y'.       
00134  77  CERT-NEEDED-SW          PIC X               VALUE 'Y'.       
00135      88  CERT-NEEDED                             VALUE 'Y'.       
00136  77  ACCOUNT-NEEDED-SW       PIC X               VALUE 'Y'.       
00137      88  ACCOUNT-NEEDED                          VALUE 'Y'.       
00138  77  EXTR-NEEDED-SW          PIC X               VALUE 'Y'.       
00139      88  EXTR-NEEDED                             VALUE 'Y'.       
00140  77  CERT-ACCT-MATCH-SW      PIC X               VALUE 'N'.       
00141      88  NO-MATCH                                VALUE 'N'.       
00142  77  ACCOUNT-BREAK-SW        PIC X               VALUE 'N'.       
00143      88  ACCT-BREAK                              VALUE 'Y'.       
00144  77  ACCT-HAS-BIZ-SW         PIC X               VALUE 'N'.       
00145      88  NO-BIZ                                  VALUE 'N'.       
00146  77  FIRST-TIME-SW           PIC X               VALUE 'Y'.       
00147      88  FIRST-TIME                              VALUE 'Y'.       
00148  77  FIRST-READ-SW           PIC X               VALUE 'Y'.       
00149      88  FIRST-READ                              VALUE 'Y'.       
00150  77  WS-ERRTBL-OPEN-SW       PIC X               VALUE 'N'.       
00151      88  WS-ERRTBL-OPEN                          VALUE 'Y'.       
00152  77  X                       PIC X               VALUE SPACE.     
00153  77  ABEND-OPTION            PIC X               VALUE 'Y'.       
00154  77  CARRIER-L               PIC X.                               
00155  77  ABEND-CODE              PIC X(4)            VALUE SPACES.    
00156  77  ERACCT-FILE-STATUS      PIC XX              VALUE ZERO.      
00157  77  ERRTBL-FILE-STATUS      PIC XX              VALUE ZERO.      
00158  77  REIN-RT-SW              PIC X               VALUE 'X'.       
00159                                                                   
00160  01  WS.                                                          
00161      12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.      
00162      12  WS-ZERO                 PIC S9          VALUE ZERO.      
00163      12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.    
00164      12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.      
00165                                                                   
00166  01  WORK-ABEND-CODE.                                             
00167      12  WAC-1               PIC X.                               
00168      12  WAC-2               PIC X.                               
00169      12  WAC-3-4.                                                 
00170          16  WAC-3           PIC X.                               
00171          16  WAC-4           PIC X.                               
00172  EJECT                                                            
00173                              COPY ELCDATE.                        
00174  EJECT                                                            
00175                              COPY ELCCALC.                        
00176  EJECT                                                            
00177                              COPY ECSRITAB.                       
00178  EJECT                                                            
00179  01  MISC-WORK-AREAS.                                             
00180      12  WORK-ACC-NO         PIC X(10).                           
00181      12  LAST-SWR-CONTROL    PIC X(20)          VALUE LOW-VALUES. 
00182      12  LAST-SWR-CONTROL-R REDEFINES LAST-SWR-CONTROL.           
00183          16  LSC-REC-TYPE    PIC X.                               
00184          16  LSC-CARR        PIC X.                               
00185          16  LSC-GROUP       PIC X(6).                            
00186          16  LSC-STATE       PIC XX.                              
00187          16  LSC-ACCOUNT     PIC X(10).                           
00188 *            66  LSC-RPT-CODE  RENAMES  LSC-ACCOUNT.              
00189      12  SAVE-ACCT-NAME      PIC X(30).                           
00190      12  SAVE-REIN-NAME      PIC X(30).                           
00191      12  LAST-SWR2-CONTROL   PIC X(4)           VALUE LOW-VALUES. 
00192      12  LAST-SWR2-CONTROL-R  REDEFINES LAST-SWR2-CONTROL.        
00193          16  LSC2-REIN-COMP  PIC XXX.                             
00194          16  LSC2-CARR       PIC X.                               
00195      12  CLM-INCUR-REIN-DATE PIC 9(06)           VALUE ZEROS.     
00196      12  CLM-INCUR-REIN-DATE-R  REDEFINES CLM-INCUR-REIN-DATE.    
00197          16  CIRD-CCYY       PIC 9(04).                           
00198          16  CIRD-CCYR  REDEFINES CIRD-CCYY.                      
00199              20  CIRD-CC     PIC 99.                              
00200              20  CIRD-YR     PIC 99.                              
00201          16  CIRD-MO         PIC 99.                              
00202      12  INCURRED-DATE.                                           
00203          16  DTO-CC          PIC 99             VALUE ZEROS.      
00204          16  DTO-YR          PIC 99             VALUE ZEROS.      
00205          16  DTO-MO          PIC 99             VALUE ZEROS.      
00206      12  PROCESS-OPT-FROM-DT PIC 9(06)          VALUE ZEROS.      
00207      12  PROCESS-OPT-FROM-DT-R REDEFINES  PROCESS-OPT-FROM-DT.    
00208          16  POF-CCYY        PIC 9(04).                           
00209          16  POF-CCYR REDEFINES POF-CCYY.                         
00210              20  POF-CC      PIC 99.                              
00211              20  POF-YR      PIC 99.                              
00212          16  POF-MO          PIC 99.                              
00213      12  PROCESS-OPT-THRU-DT PIC 9(06)          VALUE ZEROS.      
00214      12  PROCESS-OPT-THRU-DT-R REDEFINES  PROCESS-OPT-THRU-DT.    
00215          16  POT-CCYY        PIC 9(04).                           
00216          16  POT-CCYR  REDEFINES POT-CCYY.                        
00217              20  POT-CC      PIC 99.                              
00218              20  POT-YR      PIC 99.                              
00219          16  POT-MO          PIC 99.                              
00220      12  CERT-ISSUE-DATE     PIC 9(06)          VALUE ZEROS.      
00221      12  CERT-ISSUE-DATE-R REDEFINES CERT-ISSUE-DATE.             
00222          16  CID-CCYY        PIC 9(04).                           
00223          16  CID-CCYR  REDEFINES CID-CCYY.                        
00224              20  CID-CC      PIC 99.                              
00225              20  CID-YR      PIC 99.                              
00226          16  CID-MO          PIC 99.                              
00227      12  EOM-RUN-DATE        PIC 9(11)          VALUE ZEROS.      
00228      12  EOM-RUN-DATE-R REDEFINES EOM-RUN-DATE.                   
00229          16  FILLER          PIC 999.                             
00230          16  EOM-CC          PIC 99.                              
00231          16  EOM-YR          PIC 99.                              
00232          16  EOM-MO          PIC 99.                              
00233          16  EOM-DA          PIC 99.                              
00234      12  BIN-EOM-DT          PIC XX             VALUE SPACE.      
00235      12  VALUATION-DT        PIC 9(11)          VALUE ZEROS.      
00236      12  VALUATION-DT-R REDEFINES VALUATION-DT.                   
00237          16  FILLER          PIC 999.                             
00238          16  VAL-CC          PIC 99.                              
00239          16  VAL-YR          PIC 99.                              
00240          16  VAL-MO          PIC 99.                              
00241          16  VAL-DA          PIC 99.                              
00242      12  BIN-VALUATION-DT    PIC XX.                              
00243      12  WK-BREAK-TYPE       PIC X.                               
00244          88  ACCOUNT-BREAK                      VALUE '1'.        
00245          88  STATE-BREAK                        VALUE '2'.        
00246          88  GROUP-BREAK                        VALUE '3'.        
00247          88  CARRIER-BREAK                      VALUE '4'.        
00248      12  WS-CR-BIN-DATE      PIC XX             VALUE SPACES.     
00249      12  CURR-CARR           PIC X              VALUE SPACES.     
00250      12  CURR-ACCT           PIC X(10)          VALUE SPACES.     
00251      12  CURR-STATE          PIC XX             VALUE SPACES.     
00252      12  CURR-GRP            PIC X(6)           VALUE SPACES.     
00253      12  CURR-REPORT-CODE-1  PIC X(10)          VALUE SPACES.     
00254      12  CURR-REPORT-CODE-2  PIC X(10)          VALUE SPACES.     
00255      12  CURR-REIN-COMP      PIC XXX            VALUE SPACES.     
00256      12  WS-PRINT-RPT-B      PIC X              VALUE 'N'.        
00257      12  WS-PRINT-RPT-C      PIC X              VALUE 'N'.        
00258      12  PREV-CARR           PIC X              VALUE SPACES.     
00259      12  PREV-ACCT           PIC X(10)          VALUE SPACES.     
00260      12  PREV-STATE          PIC XX             VALUE SPACES.     
00261      12  PREV-GRP            PIC X(6)           VALUE SPACES.     
00262      12  PREV-REPORT-CODE-1  PIC X(10)          VALUE SPACES.     
00263      12  PREV-REPORT-CODE-2  PIC X(10)          VALUE SPACES.     
00264      12  PREV-REIN-COMP      PIC XXX            VALUE SPACES.     
00265      12  PREV-REINS-TABLE    PIC XXX            VALUE SPACES.     
00266      12  WK-RPT-CODE         PIC X              VALUE SPACES.     
00267      12  WK-DTE-OPT          PIC X              VALUE SPACES.     
00268      12  TEMP-1              PIC S9(5)V99 COMP-3 VALUE +0.        
00269      12  TEMP-2              PIC S9(5)V99 COMP-3 VALUE +0.        
00270      12  TEMP-3              PIC S9(5)V99 COMP-3 VALUE +0.        
00271      12  TEMP-4              PIC S9V9(6)  COMP-3 VALUE +0.        
00272      12  TEMP-5              PIC S9V9(6)  COMP-3 VALUE +0.        
00273      12  TEMP-6              PIC S99V9(6) COMP-3 VALUE +0.        
00274      12  TEMP-7              PIC S9(9)V99 COMP-3 VALUE +0.        
00275      12  TEMP-8              PIC S9(9)V99 COMP-3 VALUE +0.        
00276      12  REM-TRM1            PIC S999V99  COMP-3.                 
00277      12  REM-TRM2            PIC S999V99  COMP-3.                 
00278      12  REM-TRM3            PIC S999V99  COMP-3.                 
00279      12  TEM-TRM1            PIC S999V99  COMP-3.                 
00280      12  ORIG-TERM           PIC S999V99  COMP-3.                 
00281      12  WS-RE-CNT           PIC S9999    COMP-3 VALUE +0.        
00282      12  STATUTORY-SWITCH    PIC X               VALUE SPACE.     
00283          88 STATUTORY-REQUIREMENT                VALUE '*'.       
00284                                                                   
00285  01  MISC-COMP-3-AREAS   COMP-3.                                  
00286      12  FLA-CLM-REIN-BASE   PIC S9(9)V99         VALUE +0.       
00287      12  ADJ-FLA-REIN-BASE   PIC S9(9)V9(4)       VALUE +0.       
00288      12  ROUND-UP-DOLLARS    PIC S9(9)            VALUE +0.       
00289      12  NET-PREMIUM         PIC S9(9)V99         VALUE +0.       
00290      12  UNEARNED-PREMIUM    PIC S9(5)V99         VALUE +0.       
00291      12  EARNED-PREMIUM      PIC S9(5)V99         VALUE +0.       
00292      12  PER-DAY-BENEFIT     PIC S9(5)V9(5)       VALUE +0.       
00293      12  AH-TOTAL-AMT        PIC S9(7)V99         VALUE +0.       
00294      12  LF-EPR-R78          PIC S9(7)V99         VALUE +0.       
00295      12  LF-EPR-PRO          PIC S9(7)V99         VALUE +0.       
00296      12  LF-EPR-ST           PIC S9(7)V99         VALUE +0.       
00297      12  AH-EPR-R78          PIC S9(7)V99         VALUE +0.       
00298      12  AH-EPR-PRO          PIC S9(7)V99         VALUE +0.       
00299      12  AH-EPR-ST           PIC S9(7)V99         VALUE +0.       
00300      12  SAVE-LF-EPR-R78     PIC S9(7)V99         VALUE +0.       
00301      12  SAVE-LF-EPR-PRO     PIC S9(7)V99         VALUE +0.       
00302      12  SAVE-AH-EPR-R78     PIC S9(7)V99         VALUE +0.       
00303      12  SAVE-AH-EPR-PRO     PIC S9(7)V99         VALUE +0.       
00304      12  WS-FAC              PIC S9V9999          VALUE +0.       
00305      12  WS-PRO              PIC S9V9999          VALUE +0.       
00306      12  WS-R78              PIC S9V9999          VALUE +0.       
00307      12  LF-R78              PIC S9V9999          VALUE +0.       
00308      12  AH-R78              PIC S9V9999          VALUE +0.       
00309      12  LF-PRO              PIC S9V9999          VALUE +0.       
00310      12  AH-PRO              PIC S9V9999          VALUE +0.       
00311      12  NO-DAYS-THIS-CERT   PIC S9(9)            VALUE +0.       
00312      12  WORK-AREA-FOR-ACCUMULATOR.                               
00313          16  WA-FACE-BEN     PIC S9(13)V99        VALUE +0.       
00314          16  WA-ISS-PRM      PIC S9(9)V99         VALUE +0.       
00315          16  WA-CNC-PRM      PIC S9(9)V99         VALUE +0.       
00316          16  WA-EARNED-PRM   PIC S9(9)V99         VALUE +0.       
00317          16  WA-CLAIM-AMT    PIC S9(9)V99         VALUE +0.       
00318          16  WA-CERT-COUNT   PIC S9(7)            VALUE +0.       
00319          16  WA-CLAIM-COUNT  PIC S9(9)            VALUE +0.       
00320          16  WA-NUMBER-DAYS  PIC S9(9)            VALUE +0.       
00321          16  WA-CRT-W-CLM    PIC S9(9)            VALUE +0.       
00322      12  REMAINING-TERM-WORK-AREAS.                               
00323          16  LF-REM-TRM      PIC S999V99          VALUE +0.       
00324          16  AH-REM-TRM      PIC S999V99          VALUE +0.       
00325  01  EARNED-PREM-AREAS.                                           
00326      12  WE-REIN-CO          PIC X(6).                            
00327      12  WE-LF-AH            PIC X.                               
00328      12  WE-BEN-TYPE         PIC XX.                              
00329      12  WE-LFAMT            PIC S9(9)V99    COMP-3.              
00330      12  WE-LFPRM            PIC S9(7)V99    COMP-3.              
00331      12  WE-LFAMT-ALT        PIC S9(9)V99    COMP-3.              
00332      12  WE-LFPRM-ALT        PIC S9(7)V99    COMP-3.              
00333      12  WE-LFRFND           PIC S9(7)V99    COMP-3.              
00334      12  WE-AHAMT            PIC S9(9)V99    COMP-3.              
00335      12  WE-AHPRM            PIC S9(7)V99    COMP-3.              
00336      12  WE-AHRFND           PIC S9(7)V99    COMP-3.              
00337  01  TEXAS-REG-WORK-AREAS   SYNC.                                 
00338      12  TEX-FACT-1          PIC S9(9)V99    COMP-3.              
00339      12  TEX-FACT-2          PIC S999        COMP-3.              
00340      12  TEX-FACT-3          PIC S999        COMP-3.              
00341      12  TEX-FACT-4          PIC S9(7)       COMP-3.              
00342      12  TEX-FACT-5          PIC S999        COMP-3.              
00343      12  TEX-FACT-6          PIC S999        COMP-3.              
00344      12  TEX-FACT-7          PIC S9(7)       COMP-3.              
00345      12  TEX-FACT-8          PIC S9V9(6)     COMP-3.              
00346      12  TEX-FACT-9          PIC S9(4)V9(11) COMP-3.              
00347                                                                   
00348  01  NET-PAY-INTERFACE.                                           
00349      12  NP-APR              PIC S999V9(4)   COMP-3.              
00350      12  NP-ORIG             PIC S999        COMP-3.              
00351      12  NP-REM              PIC S999        COMP-3.              
00352      12  NP-OPT              PIC X.                               
00353      12  NP-CAP              PIC S999        COMP-3.              
00354      12  NP-FACTOR           PIC S9(4)V9(9)  COMP-3.              
00355      12  NP-WORK1            PIC S9(9)V9(9)  COMP-3.              
00356      12  NP-WORK2            PIC S9(9)V9(9)  COMP-3.              
00357      12  NP-BENEFIT          PIC S9(9)V99    COMP-3.              
00358      12  NP-REMAINING        PIC S9(9)V99    COMP-3.              
00359      12  NP-AHPRM            PIC S9(7)V99    COMP-3.              
00360      12  NP-ACCOUNT          PIC X(10).                           
00361  EJECT                                                            
00362  01  ACCUMULATOR-TABLE   COMP-3.                                  
00363      12  AT-AMOUNTS-L-D    OCCURS 3 TIMES.                        
00364          16  AT-AMOUNTS    OCCURS 7 TIMES.                        
00365              20  AT-FACE-BEN     PIC S9(13)V99.                   
00366              20  AT-ISS-PRM      PIC S9(9)V99.                    
00367              20  AT-CNC-PRM      PIC S9(9)V99.                    
00368              20  AT-EARNED-PRM   PIC S9(9)V99.                    
00369              20  AT-CLAIM-AMT    PIC S9(9)V99.                    
00370              20  AT-CERT-COUNT   PIC S9(7).                       
00371              20  AT-CLAIM-COUNT  PIC S9(9).                       
00372              20  AT-NUMBER-DAYS  PIC S9(9).                       
00373              20  AT-CRT-W-CLM    PIC S9(9).                       
00374                                                                   
00375  01  ACCUMULATOR-TABLE-2 COMP-3.                                  
00376      12  AT2-AMOUNTS-L-D   OCCURS 3 TIMES.                        
00377          16  AT2-AMOUNTS   OCCURS 7 TIMES.                        
00378              20  AT2-FACE-BEN    PIC S9(13)V99.                   
00379              20  AT2-ISS-PRM     PIC S9(9)V99.                    
00380              20  AT2-CNC-PRM     PIC S9(9)V99.                    
00381              20  AT2-EARNED-PRM  PIC S9(9)V99.                    
00382              20  AT2-CLAIM-AMT   PIC S9(9)V99.                    
00383              20  AT2-CERT-COUNT  PIC S9(7).                       
00384              20  AT2-CLAIM-COUNT PIC S9(9).                       
00385              20  AT2-NUMBER-DAYS PIC S9(9).                       
00386              20  AT2-CRT-W-CLM   PIC S9(9).                       
00387                                                                   
00388  01  ACCUMULATOR-TABLE-3 COMP-3.                                  
00389      12  AT3-AMOUNTS-L-D   OCCURS 3 TIMES.                        
00390          16  AT3-AMOUNTS   OCCURS 7 TIMES.                        
00391              20  AT3-FACE-BEN    PIC S9(13)V99.                   
00392              20  AT3-ISS-PRM     PIC S9(9)V99.                    
00393              20  AT3-CNC-PRM     PIC S9(9)V99.                    
00394              20  AT3-EARNED-PRM  PIC S9(9)V99.                    
00395              20  AT3-CLAIM-AMT   PIC S9(9)V99.                    
00396              20  AT3-CERT-COUNT  PIC S9(7).                       
00397              20  AT3-CLAIM-COUNT PIC S9(9).                       
00398              20  AT3-NUMBER-DAYS PIC S9(9).                       
00399              20  AT3-CRT-W-CLM   PIC S9(9).                       
00400                                                                   
00401  01  ACCUMULATOR-TABLE-4 COMP-3.                                  
00402      12  AT4-AMOUNTS-L-D   OCCURS 3 TIMES.                        
00403          16  AT4-AMOUNTS   OCCURS 7 TIMES.                        
00404              20  AT4-FACE-BEN    PIC S9(13)V99.                   
00405              20  AT4-ISS-PRM     PIC S9(9)V99.                    
00406              20  AT4-CNC-PRM     PIC S9(9)V99.                    
00407              20  AT4-EARNED-PRM  PIC S9(9)V99.                    
00408              20  AT4-CLAIM-AMT   PIC S9(9)V99.                    
00409              20  AT4-CERT-COUNT  PIC S9(7).                       
00410              20  AT4-CLAIM-COUNT PIC S9(9).                       
00411              20  AT4-NUMBER-DAYS PIC S9(9).                       
00412              20  AT4-CRT-W-CLM   PIC S9(9).                       
00413                                                                   
00414  01  ACCUMULATOR-TABLE-5 COMP-3.                                  
00415      12  AT5-AMOUNTS-L-D   OCCURS 3 TIMES.                        
00416          16  AT5-AMOUNTS   OCCURS 7 TIMES.                        
00417              20  AT5-FACE-BEN    PIC S9(13)V99.                   
00418              20  AT5-ISS-PRM     PIC S9(9)V99.                    
00419              20  AT5-CNC-PRM     PIC S9(9)V99.                    
00420              20  AT5-EARNED-PRM  PIC S9(9)V99.                    
00421              20  AT5-CLAIM-AMT   PIC S9(9)V99.                    
00422              20  AT5-CERT-COUNT  PIC S9(7).                       
00423              20  AT5-CLAIM-COUNT PIC S9(9).                       
00424              20  AT5-NUMBER-DAYS PIC S9(9).                       
00425              20  AT5-CRT-W-CLM   PIC S9(9).                       
00426                                                                   
00427  01  PRINT-TABLE         COMP-3.                                  
00428      12  PT-AMOUNTS-L-D    OCCURS 4 TIMES.                        
00429          16  PT-AMOUNTS    OCCURS 8 TIMES.                        
00430              20  PT-FACE-BEN     PIC S9(13)V99.                   
00431              20  PT-CERT-COUNT   PIC S9(9).                       
00432              20  PT-CLAIM-COUNT  PIC S9(9).                       
00433              20  PT-NUMBER-DAYS  PIC S9(9).                       
00434              20  PT-GROSS-PRM    PIC S9(9)V99.                    
00435              20  PT-CNC-PRM      PIC S9(9)V99.                    
00436              20  PT-NET-PRM      PIC S9(9)V99.                    
00437              20  PT-EARNED-PRM   PIC S9(9)V99.                    
00438              20  PT-CLAIMS       PIC S9(9)V99.                    
00439              20  PT-EL-RATIO     PIC S9(5)V9(4).                  
00440              20  PT-PRCNT-PRM    PIC S9(5)V9(4).                  
00441              20  PT-PRCNT-CLAIM  PIC S9(5)V9(4).                  
00442              20  PT-PRCNT-CNC-G  PIC S9(5)V9(4).                  
00443              20  PT-AVG-LOAN     PIC S9(9)V99.                    
00444              20  PT-AVG-CLAIM    PIC S9(9)V99.                    
00445              20  PT-AVG-DAYS     PIC S9(9).                       
00446              20  PT-CRT-W-CLM    PIC S9(9).                       
00447                                                                   
00448  EJECT                                                            
00449  01  HEADING-1.                                                   
00450      12  FILLER              PIC X(49)           VALUE SPACES.    
00451      12  FILLER              PIC X(26)           VALUE            
00452              'EXPERIENCE ANALYSIS REPORT'.                        
00453      12  FILLER              PIC X(44)           VALUE SPACES.    
00454      12  FILLER              PIC X(6)            VALUE 'ECS026'.  
00455      12  H1-RPT-CODE         PIC X               VALUE SPACES.    
00456                                                                   
00457  01  HEADING-2.                                                   
00458      12  FILLER              PIC X(47)           VALUE SPACES.    
00459      12  H2-COMPANY          PIC X(30).                           
00460      12  FILLER              PIC X(42)           VALUE SPACES.    
00461      12  H2-CURRENT-DATE     PIC X(8).                            
00462                                                                   
00463  01  HEADING-3.                                                   
00464      12  FILLER              PIC X(53)           VALUE SPACES.    
00465      12  H3-RUN-DATE         PIC X(18).                           
00466                                                                   
00467  01  ACCOUNT-HEADING.                                             
00468      12  FILLER              PIC X(9)            VALUE 'ACCOUNT'. 
00469      12  AH-ACCT-NO          PIC X(10).                           
00470      12  FILLER              PIC XX              VALUE SPACES.    
00471      12  AH-ACCT-NAME        PIC X(30).                           
00472                                                                   
00473  01  FINAL-HEADING.                                               
00474      12  FILLER              PIC X(12)    VALUE 'FINAL TOTALS'.   
00475                                                                   
00476  01  CARRIER-HEADING.                                             
00477      12  FILLER              PIC X(9)            VALUE 'CARRIER'. 
00478      12  CH-CARR-NO          PIC X.                               
00479      12  FILLER              PIC XX              VALUE SPACES.    
00480      12  CH-CARR-NA          PIC X(40).                           
00481                                                                   
00482  01  STATE-HEADING.                                               
00483      12  FILLER              PIC X(7)            VALUE 'STATE'.   
00484      12  SH-STATE-NO         PIC XX.                              
00485      12  FILLER              PIC XX              VALUE SPACES.    
00486      12  SH-STATE-NAME       PIC X(20).                           
00487                                                                   
00488  01  GROUP-HEADING.                                               
00489      12  FILLER              PIC X(7)            VALUE 'GROUP'.   
00490      12  GH-GROUP-NO         PIC X(6).                            
00491                                                                   
00492  01  REPORT-CODE-1-HEADING.                                       
00493      12  RC1H-CAPTION        PIC X(12).                           
00494      12  RC1H-CODE           PIC X(10).                           
00495                                                                   
00496  01  REPORT-CODE-2-HEADING.                                       
00497      12  RC2H-CAPTION        PIC X(12).                           
00498      12  RC2H-CODE           PIC X(10).                           
00499                                                                   
00500  01  REINSURANCE-HEADING.                                         
00501      12  FILLER              PIC X(12)           VALUE            
00502              'REINS COMP'.                                        
00503      12  RH-COMP-NO          PIC X(6).                            
00504      12  FILLER              PIC XX              VALUE SPACES.    
00505      12  RH-COMP-NAME        PIC X(30).                           
00506                                                                   
00507  EJECT                                                            
00508  01  HEADING-7.                                                   
00509      12  H7-DESC.                                                 
00510          16 H7-DESC1-7       PIC X(7).                            
00511          16 H7-DESC8-20      PIC X(13).                           
00512      12  FILLER              PIC X(4)            VALUE SPACES.    
00513      12  FILLER              PIC X(8)            VALUE            
00514              '30-UNDER'.                                          
00515      12  FILLER              PIC X(8)            VALUE SPACES.    
00516      12  FILLER              PIC X(5)            VALUE '31-40'.   
00517      12  FILLER              PIC X(9)            VALUE SPACES.    
00518      12  FILLER              PIC X(5)            VALUE '41-45'.   
00519      12  FILLER              PIC X(9)            VALUE SPACES.    
00520      12  FILLER              PIC X(5)            VALUE '46-50'.   
00521      12  FILLER              PIC X(9)            VALUE SPACES.    
00522      12  FILLER              PIC X(5)            VALUE '51-55'.   
00523      12  FILLER              PIC X(9)            VALUE SPACES.    
00524      12  FILLER              PIC X(5)            VALUE '56-60'.   
00525      12  FILLER              PIC X(9)            VALUE SPACES.    
00526      12  FILLER              PIC X(5)            VALUE '61-UP'.   
00527      12  FILLER              PIC X(9)            VALUE SPACES.    
00528      12  FILLER              PIC X(5)            VALUE 'TOTAL'.   
00529                                                                   
00530  01  DETAIL-LINE.                                                 
00531      12  DL-DESC             PIC X(20).                           
00532      12  FILLER              PIC X(112).                          
00533                                                                   
00534  01  DETAIL-LINE-R REDEFINES DETAIL-LINE.                         
00535      12  FILLER              PIC X(20).                           
00536      12  DL-AMT              PIC ZZ,ZZZ,ZZZ.99-  OCCURS 8 TIMES.  
00537                                                                   
00538  01  DETAIL-LINE-RR REDEFINES DETAIL-LINE.                        
00539      12  FILLER              PIC X(20).                           
00540      12  DL-AMT1             PIC ZZZZZZZZZZZZ9-  OCCURS 8 TIMES.  
00541                                                                   
00542  01  DETAIL-LINE-RR REDEFINES DETAIL-LINE.                        
00543      12  FILLER              PIC X(20).                           
00544      12  DL-AMT2             PIC Z,ZZZ,ZZZ,ZZ9-  OCCURS 8 TIMES.  
00545  EJECT                                                            
00546  01  SORT-WORK-REC.                                               
00547      12  SWR-CONTROL.                                             
00548          16  SWR-REC-TYPE            PIC X.                       
00549          16  SWR-CARR                PIC X.                       
00550          16  SWR-GROUP               PIC X(6).                    
00551          16  SWR-STATE               PIC XX.                      
00552          16  SWR-ACCOUNT             PIC X(10).                   
00553 *            66  SWR-RPT-CODE RENAMES SWR-ACCOUNT.                
00554      12  SWR-REPORT-CODE-1           PIC X(10).                   
00555      12  SWR-REPORT-CODE-2           PIC X(10).                   
00556      12  SWR-ACCT-NAME               PIC X(30).                   
00557      12  SWR-TABLE       COMP-3.                                  
00558          16  SWR-AMOUNTS-L-D   OCCURS 3 TIMES.                    
00559              20  SWR-AMOUNTS   OCCURS 7 TIMES.                    
00560                  24  SWR-FACE-BEN    PIC S9(13)V99.               
00561                  24  SWR-ISS-PRM     PIC S9(9)V99.                
00562                  24  SWR-CNC-PRM     PIC S9(9)V99.                
00563                  24  SWR-EARNED-PRM  PIC S9(9)V99.                
00564                  24  SWR-CLAIM-AMT   PIC S9(9)V99.                
00565                  24  SWR-CERT-COUNT  PIC S9(7).                   
00566                  24  SWR-CLAIM-COUNT PIC S9(9).                   
00567                  24  SWR-NUMBER-DAYS PIC S9(9).                   
00568                  24  SWR-CRT-W-CLM   PIC S9(9).                   
00569                                                                   
00570  01  SORT-WORK-REC-2.                                             
00571      12  SWR2-CONTROL.                                            
00572          16  SWR2-REIN-COMP          PIC XXX.                     
00573          16  SWR2-CARR               PIC X.                       
00574      12  SWR2-DT                     PIC 9(11)      COMP-3.       
00575      12  SWR2-AGE                    PIC 99.                      
00576      12  SWR2-ENTRY-STATUS           PIC X.                       
00577      12  SWR2-LF-CURRENT-STATUS      PIC X.                       
00578      12  SWR2-AH-CURRENT-STATUS      PIC X.                       
00579      12  SWR2-LFTYP                  PIC XX.                      
00580      12  SWR2-LF-TERM                PIC S999       COMP-3.       
00581      12  SWR2-LF-REM-TRM             PIC S999       COMP-3.       
00582      12  SWR2-LFAMT                  PIC S9(9)V99   COMP-3.       
00583      12  SWR2-LFPRM                  PIC S9(7)V99   COMP-3.       
00584      12  SWR2-LFRFND                 PIC S9(7)V99   COMP-3.       
00585      12  SWR2-AHTYP                  PIC XX.                      
00586      12  SWR2-AH-TERM                PIC S999       COMP-3.       
00587      12  SWR2-AH-REM-TRM             PIC S999       COMP-3.       
00588      12  SWR2-AHAMT                  PIC S9(9)V99   COMP-3.       
00589      12  SWR2-AHPRM                  PIC S9(7)V99   COMP-3.       
00590      12  SWR2-AHRFND                 PIC S9(7)V99   COMP-3.       
00591      12  SWR2-NUM-DTH-CLM            PIC S999       COMP-3.       
00592      12  SWR2-DTHAMT                 PIC S9(9)V99   COMP-3.       
00593      12  SWR2-NUM-DIS-CLM            PIC S999       COMP-3.       
00594      12  SWR2-DISAMT                 PIC S9(9)V99   COMP-3.       
00595      12  SWR2-NAME                   PIC X(30).                   
00596      12  SWR2-REINS-TABLE            PIC XXX.                     
00597      12  SWR2-LF-EPR-PR              PIC S9(5)V99   COMP-3.       
00598      12  SWR2-LF-EPR-78              PIC S9(5)V99   COMP-3.       
00599      12  SWR2-AH-EPR-PR              PIC S9(5)V99   COMP-3.       
00600      12  SWR2-AH-EPR-78              PIC S9(5)V99   COMP-3.       
00601                                                                   
00602                              COPY ELCDTECX.                       
00603                                                                   
00604                              COPY ELCDTEVR.                       
00605                                                                   
00606                              COPY ELCCRTVR.                       
00607                                                                   
00608                              COPY ELCPSEVR.                       
00609  EJECT                                                            
00610  PROCEDURE DIVISION.                                              
00611                                                                   
00612  0000-READ-DATE-CARD.                                             
00613                              COPY ELCDTERX.                       
00614                                                                   
00615  0100-MAIN-LINE.                                                  
00616      OPEN OUTPUT PRINTER.                                         
00617                                                                   
00618      PERFORM 0120-INITIALIZATION  THRU 0199-EXIT.                 
00619                                                                   
00620      PERFORM 0200-MAIN-PROCESSING THRU 0299-EXIT.                 
00621                                                                   
00622      CLOSE PRINTER.                                               
00623                                                                   
00624  0110-CLOSE-FICH.                                                 
00625                              COPY ELCPRTC.                        
00626                                                                   
00627      GOBACK.                                                      
00628                                                                   
00629  0120-INITIALIZATION.                                             
00630      MOVE WS-CURRENT-DATE        TO H2-CURRENT-DATE.              
00631                                                                   
00632      MOVE COMPANY-NAME           TO H2-COMPANY.                   
00633      MOVE ALPH-DATE              TO H3-RUN-DATE.                  
00634                                                                   
00635      MOVE SPACES TO REIN-LEVELS-END.                              
00636      PERFORM CLEAR-REIN-HOLD THRU CLEAR-REIN-HOLD-X.              
00637                                                                   
00638      MOVE '1'                    TO WK-BREAK-TYPE.                
00639      PERFORM 6500-CLEAR-ACCUMULATOR-TABLE THRU 6599-EXIT.         
00640                                                                   
00641      MOVE '2'                    TO WK-BREAK-TYPE.                
00642      PERFORM 6500-CLEAR-ACCUMULATOR-TABLE THRU 6599-EXIT.         
00643                                                                   
00644      MOVE '3'                    TO WK-BREAK-TYPE.                
00645      PERFORM 6500-CLEAR-ACCUMULATOR-TABLE THRU 6599-EXIT.         
00646                                                                   
00647      MOVE '4'                    TO WK-BREAK-TYPE.                
00648      PERFORM 6500-CLEAR-ACCUMULATOR-TABLE THRU 6599-EXIT.         
00649                                                                   
00650      MOVE '5'                    TO WK-BREAK-TYPE.                
00651      PERFORM 6500-CLEAR-ACCUMULATOR-TABLE THRU 6599-EXIT.         
00652                                                                   
00653      PERFORM 6700-CLEAR-PRINT-TABLE THRU 6799-EXIT.               
00654                                                                   
00655      MOVE  '1'                   TO WK-BREAK-TYPE.                
00656                                                                   
00657      IF DTE-PRC-OPT NOT NUMERIC                                   
00658          MOVE 1 TO DTE-PRC-OPT.                                   
00659                                                                   
00660      IF DTE-TOT-OPT NOT NUMERIC                                   
00661          MOVE 1 TO DTE-TOT-OPT.                                   
00662                                                                   
00663      MOVE RUN-DATE              TO EOM-RUN-DATE.                  
00664      MOVE BIN-RUN-DATE          TO BIN-EOM-DT.                    
00665                                                                   
00666      MOVE RUN-CCYY              TO POT-CCYY.                      
00667      MOVE RUN-MO                TO POT-MO.                        
00668                                                                   
00669      IF DTE-PRC-OPT = 1 OR 2 OR 3 OR 4                            
00670          COMPUTE POF-CCYY = RUN-CCYY - DTE-PRC-OPT                
00671          COMPUTE POF-MO   = RUN-MO + 1                            
00672          IF POF-MO GREATER 12                                     
00673              MOVE 01 TO POF-MO                                    
00674              ADD 1 TO POF-CCYY.                                   
00675                                                                   
00676  0199-EXIT.                                                       
00677      EXIT.                                                        
00678  EJECT                                                            
00679  0200-MAIN-PROCESSING.                                            
00680      PERFORM  0300-CREATE-EXTRACTS  THRU  0399-EXIT.              
00681                                                                   
00682      CLOSE TEMP-WORK.                                             
00683      SORT SORT-WORK ON ASCENDING SR-CONTROL                       
00684                                  USING  TEMP-WORK                 
00685                                  GIVING TEMP-WORK.                
00686                                                                   
00687      IF SORT-RETURN NOT = ZEROS                                   
00688          MOVE  '0101'            TO  WS-RETURN-CODE               
00689          MOVE  'SORT ERROR'      TO  WS-ABEND-MESSAGE             
00690          GO TO ABEND-PGM.                                         
00691                                                                   
00692      OPEN INPUT TEMP-WORK.                                        
00693                                                                   
00694      MOVE 'A' TO WK-RPT-CODE.                                     
00695      PERFORM  3000-PRODUCE-REPORTS  THRU  3099-EXIT.              
00696                                                                   
00697      CLOSE TEMP-WORK.                                             
00698                                                                   
00699      IF WS-PRINT-RPT-B = 'Y'                                      
00700          SORT SORT-WORK ON ASCENDING SR-CONTROL-B                 
00701                                      USING  TEMP-WORK             
00702                                      GIVING TEMP-WORK             
00703          IF SORT-RETURN NOT = ZEROS                               
00704              MOVE '0101'         TO  WS-RETURN-CODE               
00705              MOVE 'SORT ERROR'   TO  WS-ABEND-MESSAGE             
00706              GO TO ABEND-PGM                                      
00707           ELSE                                                    
00708              OPEN INPUT TEMP-WORK                                 
00709              MOVE 'B'            TO WK-RPT-CODE                   
00710              MOVE 'Y'            TO FIRST-TIME-SW                 
00711              MOVE 'N'            TO EXTR-END-SW                   
00712              PERFORM 3000-PRODUCE-REPORTS THRU 3099-EXIT          
00713              CLOSE TEMP-WORK.                                     
00714                                                                   
00715      IF WS-PRINT-RPT-C = 'Y'                                      
00716          SORT SORT-WORK ON ASCENDING SR-CONTROL-C1                
00717                                      SR-CONTROL-C2                
00718                                      USING  TEMP-WORK             
00719                                      GIVING TEMP-WORK             
00720          IF SORT-RETURN NOT = ZEROS                               
00721              MOVE '0101'         TO  WS-RETURN-CODE               
00722              MOVE 'SORT ERROR'   TO  WS-ABEND-MESSAGE             
00723              GO TO ABEND-PGM                                      
00724            ELSE                                                   
00725              OPEN INPUT TEMP-WORK                                 
00726              MOVE 'C'            TO WK-RPT-CODE                   
00727              MOVE 'Y'            TO FIRST-TIME-SW                 
00728              MOVE 'N'            TO EXTR-END-SW                   
00729              PERFORM 3000-PRODUCE-REPORTS THRU 3099-EXIT          
00730              CLOSE TEMP-WORK.                                     
00731                                                                   
00732      MOVE 'D'                 TO WK-RPT-CODE.                     
00733      MOVE 'Y'                 TO FIRST-TIME-SW.                   
00734      MOVE 'N'                 TO EXTR-END-SW.                     
00735                                                                   
00736      IF DTE-TOT-OPT = 1 OR 4                                      
00737          SORT SORT-WORK-2 ON ASCENDING SR2-CONTROL                
00738                                        USING  REIN-WORK           
00739                                        GIVING REIN-WORK           
00740          IF SORT-RETURN NOT = ZEROS                               
00741              MOVE  '0102'            TO  WS-RETURN-CODE           
00742              MOVE  'SORT ERROR'      TO  WS-ABEND-MESSAGE         
00743              GO TO ABEND-PGM                                      
00744           ELSE                                                    
00745              OPEN INPUT REIN-WORK                                 
00746              PERFORM 5000-PRODUCE-D-REPORT THRU 5099-EXIT         
00747                                         UNTIL END-OF-EXTR         
00748              CLOSE REIN-WORK                                      
00749              IF WS-ERRTBL-OPEN                                    
00750                  CLOSE ERRTBL-IN
00751                  IF ERRTBL-FILE-STATUS  = '00'                    
00752                      NEXT SENTENCE                                
00753                  ELSE                                             
00754                      MOVE 'ERROR OCCURED CLOSE - ERRTBL'          
00755                                       TO  WS-ABEND-MESSAGE        
00756                      MOVE ERRTBL-FILE-STATUS                      
00757                                       TO  WS-ABEND-FILE-STATUS    
00758                      GO TO ABEND-PGM.                             
00759                                                                   
00760  0299-EXIT.                                                       
00761      EXIT.                                                        
00762  EJECT                                                            
00763  0300-CREATE-EXTRACTS.                                            
00764      OPEN INPUT ACCT-IN  CERT-IN                                  
00765          OUTPUT TEMP-WORK.                                        
00766                                                                   
00767      IF ERACCT-FILE-STATUS  = '00' OR '97'                        
00768          NEXT SENTENCE                                            
00769       ELSE                                                        
00770          MOVE 'ERROR OCCURED OPEN - ERACCTT' TO WS-ABEND-MESSAGE  
00771          MOVE ERACCT-FILE-STATUS         TO WS-ABEND-FILE-STATUS  
00772          GO TO ABEND-PGM.                                         
00773                                                                   
00774      IF DTE-TOT-OPT = 1 OR 4                                      
00775          OPEN INPUT ERRTBL-IN
00776               OUTPUT REIN-WORK                                    
00777          IF ERRTBL-FILE-STATUS  = '35'                            
00778              NEXT SENTENCE                                        
00779          ELSE                                                     
00780              IF ERRTBL-FILE-STATUS  = '00' OR '97'                
00781                  PERFORM REIN-CO-TABLE-BUILD THRU REIN-BUILD-EXIT 
00782                  MOVE 'Y'             TO  WS-ERRTBL-OPEN-SW       
00783              ELSE                                                 
00784                  MOVE 'ERROR OCCURED OPEN - ERRTBL'               
00785                                       TO  WS-ABEND-MESSAGE        
00786                  MOVE ERRTBL-FILE-STATUS                          
00787                                       TO  WS-ABEND-FILE-STATUS    
00788                  GO TO ABEND-PGM.                                 
00789                                                                   
00790      PERFORM 0400-PROCESS-CERTS THRU 0499-EXIT                    
00791                                 UNTIL END-OF-CERT.                
00792                                                                   
00793      CLOSE ACCT-IN  CERT-IN.                                      
00794                                                                   
00795      IF ERACCT-FILE-STATUS  = '00'                                
00796          NEXT SENTENCE                                            
00797       ELSE                                                        
00798          MOVE 'ERROR OCCURED CLOSE - ERACCTT'                     
00799                                  TO WS-ABEND-MESSAGE              
00800          MOVE ERACCT-FILE-STATUS                                  
00801                                  TO  WS-ABEND-FILE-STATUS         
00802          GO TO ABEND-PGM.                                         
00803                                                                   
00804      IF DTE-TOT-OPT = 1 OR 4                                      
00805          CLOSE          REIN-WORK.                                
00806                                                                   
00807  0399-EXIT.                                                       
00808      EXIT.                                                        
00809  EJECT                                                            
00810  0400-PROCESS-CERTS.                                              
00811      IF CERT-NEEDED                                               
00812          PERFORM 9000-READ-CERT-IN  THRU  9099-EXIT.              
00813                                                                   
00814      IF ACCOUNT-NEEDED                                            
00815          PERFORM 9200-READ-ACCT-IN  THRU  9299-EXIT.              
00816                                                                   
00817      PERFORM  0500-MATCH-CERT-ACCT  THRU  0599-EXIT.              
00818                                                                   
00819      IF NO-MATCH                                                  
00820          MOVE  '0302'            TO  WS-RETURN-CODE               
00821          MOVE  'NO ACCOUNT MATCH' TO  WS-ABEND-MESSAGE            
00822          GO TO ABEND-PGM.                                         
00823                                                                   
00824      IF ACCT-BREAK                                                
00825          PERFORM 1000-CREATE-EXTRACTS         THRU 1099-EXIT      
00826          PERFORM 6500-CLEAR-ACCUMULATOR-TABLE THRU 6599-EXIT      
00827          MOVE  'N'               TO  ACCT-HAS-BIZ-SW              
00828                                      CERT-NEEDED-SW               
00829          MOVE  'Y'               TO  ACCOUNT-NEEDED-SW            
00830          GO TO 0499-EXIT.                                         
00831                                                                   
00832      PERFORM  0700-ACCUMULATE-FROM-CERT  THRU  0799-EXIT.         
00833                                                                   
00834      IF (DTE-TOT-OPT = 1 OR 4) AND                                
00835         (CR-REIN-TABLE NOT = SPACES AND ZEROS AND LOW-VALUES)     
00836            PERFORM 2000-REINSURE-ROUTINE THRU 2099-EXIT.          
00837                                                                   
00838      MOVE  'Y'                   TO  ACCT-HAS-BIZ-SW              
00839                                      CERT-NEEDED-SW.              
00840      MOVE  'N'                   TO  ACCOUNT-NEEDED-SW.           
00841                                                                   
00842  0499-EXIT.                                                       
00843      EXIT.                                                        
00844  EJECT                                                            
00845  0500-MATCH-CERT-ACCT.                                            
00846      IF AM-CONTROL-A LESS CR-ACCT-CONTROL                         
00847          MOVE  'Y'               TO  ACCOUNT-BREAK-SW             
00848                                      CERT-ACCT-MATCH-SW           
00849          GO TO 0599-EXIT.                                         
00850                                                                   
00851      IF AM-CONTROL-A GREATER CR-ACCT-CONTROL                      
00852          MOVE  'N'               TO  CERT-ACCT-MATCH-SW           
00853          GO TO 0599-EXIT.                                         
00854                                                                   
00855      IF CR-DT LESS AM-EFFECT-DT                                   
00856          MOVE  'N'               TO  CERT-ACCT-MATCH-SW           
00857          GO TO 0599-EXIT.                                         
00858                                                                   
00859      IF CR-DT NOT LESS AM-EXPIRE-DT                               
00860          MOVE  'Y'               TO  ACCOUNT-BREAK-SW             
00861                                      CERT-ACCT-MATCH-SW           
00862          GO TO 0599-EXIT.                                         
00863                                                                   
00864      MOVE  'N'                   TO  ACCOUNT-BREAK-SW.            
00865      MOVE  'Y'                   TO  CERT-ACCT-MATCH-SW.          
00866                                                                   
00867  0599-EXIT.                                                       
00868      EXIT.                                                        
00869  EJECT                                                            
00870  0700-ACCUMULATE-FROM-CERT.                                       
00871      IF CR-AGE  GREATER  +60                                      
00872          MOVE  +7                TO  A-SUB2.                      
00873                                                                   
00874      IF CR-AGE  LESS  +61                                         
00875          MOVE  +6                TO  A-SUB2.                      
00876                                                                   
00877      IF CR-AGE  LESS  +56                                         
00878          MOVE  +5                TO  A-SUB2.                      
00879                                                                   
00880      IF CR-AGE  LESS  +51                                         
00881          MOVE  +4                TO  A-SUB2.                      
00882                                                                   
00883      IF CR-AGE  LESS  +46                                         
00884          MOVE  +3                TO  A-SUB2.                      
00885                                                                   
00886      IF CR-AGE  LESS  +41                                         
00887          MOVE  +2                TO  A-SUB2.                      
00888                                                                   
00889      IF CR-AGE  LESS  +31                                         
00890          MOVE  +1                TO  A-SUB2.                      
00891                                                                   
00892  EJECT                                                            
00893  0710-ACCUMULATE-LIFE.                                            
00894                                                                   
00895      IF CR-LFTYP = ZERO                                           
00896          GO TO 0750-ACCUMULATE-AH.                                
00897                                                                   
00898      IF DTE-PRC-OPT = 5                                           
00899          IF CR-LF-POLICY-IS-ACTIVE                                
00900               NEXT SENTENCE                                       
00901             ELSE                                                  
00902               GO TO 0750-ACCUMULATE-AH.                           
00903                                                                   
00904      MOVE CR-LFTYP               TO CLAS-LOOK.                    
00905      PERFORM  0900-FIND-LIFE-TYPE  THRU  0950-EXIT.               
00906                                                                   
00907      MOVE CR-LF-TERM             TO ORIG-TERM.                    
00908      MOVE RUN-DATE               TO VALUATION-DT.                 
00909      PERFORM 8800-REMAINING-TERM-ROUTINE THRU 8899-EXIT.          
00910      MOVE REM-TRM1               TO LF-REM-TRM.                   
00911                                                                   
00912      IF DTE-PRC-OPT = 5                                           
00913        IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')           
00914          IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'L'                  
00915              IF REM-TRM1 NOT GREATER +0                           
00916                  MOVE 'E'           TO CR-LF-CURRENT-STATUS       
00917                  GO TO 0750-ACCUMULATE-AH                         
00918              ELSE                                                 
00919                  NEXT SENTENCE                                    
00920          ELSE                                                     
00921              IF (REM-TRM1 + 1) NOT GREATER +0                     
00922                  MOVE 'E'           TO CR-LF-CURRENT-STATUS       
00923                  GO TO 0750-ACCUMULATE-AH                         
00924              ELSE                                                 
00925                  NEXT SENTENCE                                    
00926      ELSE                                                         
00927          IF REM-TRM1 NOT GREATER +0  OR                           
00928             CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'B'                  
00929              MOVE 'E'               TO CR-LF-CURRENT-STATUS       
00930              GO TO 0750-ACCUMULATE-AH.                            
00931                                                                   
00932      IF CLAS-I-JOINT (CLAS-INDEXL) = 'J'                          
00933          MOVE  +2                TO  A-SUB1                       
00934      ELSE                                                         
00935          MOVE  +1                TO  A-SUB1.                      
00936                                                                   
00937      MOVE AT-AMOUNTS (A-SUB1  A-SUB2)                             
00938                                  TO  WORK-AREA-FOR-ACCUMULATOR.   
00939                                                                   
00940      IF CR-ENTRY-STATUS = '9'                                     
00941          GO TO 0715-ADD-LIFE-EARN.                                
00942                                                                   
122002     IF CR-ENTRY-STATUS NOT = '5' AND 'M'                        
00944          ADD CR-LFAMT            TO  WA-FACE-BEN                  
00945          ADD CR-LFAMT-ALT        TO  WA-FACE-BEN                  
00946          ADD CR-LFPRM            TO  WA-ISS-PRM                   
00947          ADD CR-LFPRM-ALT        TO  WA-ISS-PRM.                  
00948                                                                   
00949      ADD CR-LFRFND               TO  WA-CNC-PRM.                  
00950      ADD CR-DTHAMT               TO  WA-CLAIM-AMT.                
00951      ADD CR-DTHEXP               TO  WA-CLAIM-AMT.                
00952      ADD +1                      TO  WA-CERT-COUNT.               
00953      ADD CR-NUM-DTH-CLM          TO  WA-CLAIM-COUNT.              
00954                                                                   
00955      MOVE  ZERO                  TO  WA-NUMBER-DAYS.              
00956                                                                   
00957      IF CR-DTHAMT NOT = ZERO                                      
00958          ADD  +1                 TO  WA-CRT-W-CLM.                
00959                                                                   
00960  0715-ADD-LIFE-EARN.                                              
00961                                                                   
00962      MOVE ZEROS TO EARNED-PREMIUM LF-EPR-R78                      
00963                                   LF-EPR-PRO                      
00964                                   LF-EPR-ST                       
00965                              SAVE-LF-EPR-R78                      
00966                              SAVE-LF-EPR-PRO.                     
00967                                                                   
00968      PERFORM 8200-EP-BUMP-RTN  THRU  8299-EXIT.                   
00969      PERFORM 8400-EP-DO-LF     THRU  8499-EXIT.                   
00970                                                                   
00971      MOVE LF-EPR-R78      TO SAVE-LF-EPR-R78.                     
00972      IF CLAS-I-EP (CLAS-INDEXL) = 'B'                             
00973          MOVE LF-EPR-ST   TO SAVE-LF-EPR-R78.                     
00974                                                                   
00975      MOVE LF-EPR-PRO      TO SAVE-LF-EPR-PRO.                     
00976                                                                   
00977      IF CLAS-I-EP (CLAS-INDEXL) = 'U'                             
00978          MOVE 'R' TO CLAS-I-EP (CLAS-INDEXL).                     
00979                                                                   
00980      IF DTE-CLIENT = 'FIA'                                        
00981        IF CR-CARRIER = 'A' OR 'B'                                 
00982          COMPUTE EARNED-PREMIUM = (LF-EPR-PRO + LF-EPR-R78) / +2  
00983          GO TO 0715-CONT.                                         
00984                                                                   
00985      IF CLAS-I-EP (CLAS-INDEXL) = '1'                             
00986          COMPUTE EARNED-PREMIUM = (LF-EPR-PRO * +.6667)           
00987                                 + (LF-EPR-R78 * +.3333)           
00988          GO TO 0715-CONT.                                         
00989                                                                   
00990      IF CLAS-I-EP (CLAS-INDEXL) = 'M'                             
00991          COMPUTE EARNED-PREMIUM = (LF-EPR-PRO + LF-EPR-R78) / +2  
00992          GO TO 0715-CONT.                                         
00993                                                                   
00994      IF CLAS-I-EP (CLAS-INDEXL) = 'P' OR                          
00995         AM-STATE   = 'WY'             OR                          
00996         DTE-CLIENT = 'SLC'                                        
00997           MOVE LF-EPR-PRO TO EARNED-PREMIUM                       
00998           GO TO 0715-CONT.                                        
00999                                                                   
01000      IF CLAS-I-EP (CLAS-INDEXL) = 'B'                             
01001           MOVE LF-EPR-ST  TO EARNED-PREMIUM                       
01002           GO TO 0715-CONT.                                        
01003                                                                   
01004      IF CLAS-I-EP (CLAS-INDEXL) = 'R' OR 'T' OR 'N' OR 'A'        
01005          MOVE LF-EPR-R78 TO EARNED-PREMIUM.                       
01006                                                                   
01007  0715-CONT.                                                       
01008                                                                   
01009      IF CR-ENTRY-STATUS NOT = '9'                                 
01010          ADD EARNED-PREMIUM     TO  WA-EARNED-PRM.                
01011                                                                   
01012      MOVE WORK-AREA-FOR-ACCUMULATOR                               
01013                                 TO AT-AMOUNTS (A-SUB1  A-SUB2).   
01014                                                                   
01015      MOVE ZEROS TO EARNED-PREMIUM.                                
01016                                                                   
01017  EJECT                                                            
01018  0750-ACCUMULATE-AH.                                              
01019                                                                   
01020      IF CR-AHTYP = ZERO                                           
01021          GO TO 0799-EXIT.                                         
01022                                                                   
01023      IF DTE-PRC-OPT = 5                                           
01024          IF CR-AH-POLICY-IS-ACTIVE                                
01025               NEXT SENTENCE                                       
01026             ELSE                                                  
01027               GO TO 0799-EXIT.                                    
01028                                                                   
01029      MOVE CR-AHTYP             TO CLAS-LOOK.                      
01030      PERFORM  0960-FIND-AH-TYPE  THRU  0990-EXIT.                 
01031                                                                   
01032      MOVE CR-AH-TERM           TO ORIG-TERM.                      
01033      MOVE RUN-DATE             TO VALUATION-DT.                   
01034      PERFORM 8800-REMAINING-TERM-ROUTINE THRU 8899-EXIT.          
01035      MOVE REM-TRM1             TO AH-REM-TRM.                     
01036                                                                   
01037      IF DTE-PRC-OPT = 5                                           
01038        IF REM-TRM1 NOT GREATER +0                                 
01039            MOVE 'E'              TO CR-AH-CURRENT-STATUS          
01040            GO TO 0799-EXIT.                                       
01041                                                                   
01042      MOVE +3                     TO  A-SUB1.                      
01043      MOVE AT-AMOUNTS (A-SUB1  A-SUB2)                             
01044                                  TO  WORK-AREA-FOR-ACCUMULATOR.   
01045                                                                   
01046      IF CR-ENTRY-STATUS = '9'                                     
01047          GO TO 0755-ADD-AH-EARN.                                  
01048                                                                   
122002     IF CR-ENTRY-STATUS NOT = '5' AND 'M'                       
01050          COMPUTE AH-TOTAL-AMT = CR-AHAMT * CR-AH-TERM             
01051          ADD AH-TOTAL-AMT        TO  WA-FACE-BEN                  
01052          ADD CR-AHPRM            TO  WA-ISS-PRM.                  
01053                                                                   
01054      ADD CR-AHRFND               TO  WA-CNC-PRM.                  
01055      ADD CR-DISAMT               TO  WA-CLAIM-AMT.                
01056      ADD CR-DISEXP               TO  WA-CLAIM-AMT.                
01057      ADD +1                      TO  WA-CERT-COUNT.               
01058      ADD CR-NUM-DIS-CLM          TO  WA-CLAIM-COUNT.              
01059                                                                   
01060      IF CR-DISAMT NOT  = ZEROS AND                                
01061         CR-AHAMT  NOT  = ZEROS                                    
01062          COMPUTE PER-DAY-BENEFIT ROUNDED = CR-AHAMT / 30          
01063          COMPUTE NO-DAYS-THIS-CERT ROUNDED = CR-DISAMT            
01064                                              /  PER-DAY-BENEFIT   
01065          ADD +1                 TO  WA-CRT-W-CLM                  
01066          ADD NO-DAYS-THIS-CERT  TO  WA-NUMBER-DAYS.               
01067                                                                   
01068  0755-ADD-AH-EARN.                                                
01069                                                                   
01070      MOVE ZEROS TO EARNED-PREMIUM AH-EPR-R78                      
01071                                   AH-EPR-PRO                      
01072                                   AH-EPR-ST                       
01073                              SAVE-AH-EPR-R78                      
01074                              SAVE-AH-EPR-PRO.                     
01075                                                                   
01076      PERFORM 8200-EP-BUMP-RTN THRU 8299-EXIT.                     
01077      PERFORM 8600-EP-DO-AH    THRU 8699-EXIT.                     
01078                                                                   
01079      MOVE AH-EPR-R78  TO SAVE-AH-EPR-R78.                         
01080      MOVE AH-EPR-PRO  TO SAVE-AH-EPR-PRO.                         
01081                                                                   
01082      IF (DTE-CLIENT = 'FIA' AND CR-CARRIER = 'A' OR 'B')  OR      
01083         (DTE-CLIENT = 'MLI')                                      
01084              COMPUTE EARNED-PREMIUM =                             
01085                  (AH-EPR-PRO + AH-EPR-R78) / +2                   
01086              GO TO 0755-CONT.                                     
01087                                                                   
01088      IF DTE-CLIENT = 'GIC'                                        
01089          IF CLAS-I-BAL (CLAS-INDEXA) NOT = 'B'                    
01090              COMPUTE EARNED-PREMIUM =                             
01091                   (AH-EPR-PRO * +.80) + (AH-EPR-R78 * +.20)       
01092              GO TO 0755-CONT.                                     
01093                                                                   
01094      IF CLAS-I-EP (CLAS-INDEXA) = '1' OR                          
01095        (DTE-CLIENT = 'ITY' AND CR-CARRIER = '5' OR '7')           
01096           COMPUTE EARNED-PREMIUM =                                
01097                (AH-EPR-PRO * +.6667) + (AH-EPR-R78 * +.3333)      
01098           GO TO 0755-CONT.                                        
01099                                                                   
01100      IF CLAS-I-EP (CLAS-INDEXA) = 'M'                             
01101          COMPUTE EARNED-PREMIUM =                                 
01102              (AH-EPR-PRO + AH-EPR-R78) / +2                       
01103          GO TO 0755-CONT.                                         
01104                                                                   
01105      IF CLAS-I-EP (CLAS-INDEXA) = 'P'  OR                         
01106         AM-STATE   = 'WY'     OR                                  
01107         DTE-CLIENT = 'SLC'    OR                                  
01108        (DTE-CLIENT = 'ITY' AND CR-CARRIER = '2')                  
01109           MOVE AH-EPR-PRO TO EARNED-PREMIUM                       
01110           GO TO 0755-CONT.                                        
01111                                                                   
01112      IF CLAS-I-EP (CLAS-INDEXA) = 'R'  OR  'A'                    
01113           IF (DTE-CLIENT = 'POS' AND CR-CARRIER = '2')  OR        
01114              (DTE-CLIENT = 'FIM')                                 
01115               COMPUTE EARNED-PREMIUM =                            
01116                   (AH-EPR-PRO + AH-EPR-R78) / +2                  
01117             ELSE                                                  
01118               MOVE AH-EPR-R78 TO EARNED-PREMIUM.                  
01119                                                                   
01120  0755-CONT.                                                       
01121                                                                   
01122      IF CR-ENTRY-STATUS NOT = '9'                                 
01123          ADD EARNED-PREMIUM     TO  WA-EARNED-PRM.                
01124                                                                   
01125      MOVE WORK-AREA-FOR-ACCUMULATOR                               
01126                           TO AT-AMOUNTS (A-SUB1  A-SUB2).         
01127                                                                   
01128      MOVE ZEROS TO EARNED-PREMIUM.                                
01129                                                                   
01130  0799-EXIT.                                                       
01131      EXIT.                                                        
01132  EJECT                                                            
01133  0800-DATE-CONVERT-ROUTINE.                                       
01134      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   
01135                                                                   
01136  0899-EXIT.                                                       
01137      EXIT.                                                        
01138  EJECT                                                            
01139  0900-FIND-LIFE-TYPE.                                             
01140      IF CLAS-I-BEN (CLAS-INDEXL)  = CLAS-LOOK                     
01141          GO TO 0950-EXIT.                                         
01142                                                                   
01143      MOVE  CLAS-STARTL           TO  CLAS-INDEXL.                 
01144                                                                   
01145  0910-FIND-LIFE-LOOP.                                             
01146      IF CLAS-INDEXL  GREATER CLAS-MAXL                            
01147          GO TO 0940-FIND-LIFE-ABEND.                              
01148                                                                   
01149      IF CLAS-I-BEN (CLAS-INDEXL) = CLAS-LOOK                      
01150          GO TO 0950-EXIT.                                         
01151                                                                   
01152      ADD +1                     TO  CLAS-INDEXL.                  
01153                                                                   
01154      GO TO 0910-FIND-LIFE-LOOP.                                   
01155                                                                   
01156  0940-FIND-LIFE-ABEND.                                            
01157      MOVE '0401'                 TO  WS-RETURN-CODE.              
01158      MOVE 'INVALID BENEFIT TYPE' TO  WS-ABEND-MESSAGE.            
01159                                                                   
01160      GO TO ABEND-PGM.                                             
01161                                                                   
01162  0950-EXIT.                                                       
01163      EXIT.                                                        
01164                                                                   
01165  0960-FIND-AH-TYPE.                                               
01166      IF CLAS-I-BEN (CLAS-INDEXA)  = CLAS-LOOK                     
01167          GO TO 0990-EXIT.                                         
01168                                                                   
01169      MOVE  CLAS-STARTA           TO  CLAS-INDEXA.                 
01170                                                                   
01171  0970-FIND-AH-LOOP.                                               
01172      IF CLAS-INDEXA  GREATER CLAS-MAXA                            
01173          GO TO 0980-FIND-AH-ABEND.                                
01174                                                                   
01175      IF CLAS-I-BEN (CLAS-INDEXA) = CLAS-LOOK                      
01176          GO TO 0990-EXIT.                                         
01177                                                                   
01178      ADD +1                     TO  CLAS-INDEXA.                  
01179                                                                   
01180      GO TO 0970-FIND-AH-LOOP.                                     
01181                                                                   
01182  0980-FIND-AH-ABEND.                                              
01183      MOVE '0401'                 TO  WS-RETURN-CODE.              
01184      MOVE 'INVALID BENEFIT TYPE' TO  WS-ABEND-MESSAGE.            
01185                                                                   
01186      GO TO ABEND-PGM.                                             
01187                                                                   
01188  0990-EXIT.                                                       
01189      EXIT.                                                        
01190  EJECT                                                            
01191  1000-CREATE-EXTRACTS.                                            
01192      IF NO-BIZ                                                    
01193          GO TO 1099-EXIT.                                         
01194                                                                   
01195      MOVE SPACES                 TO  SORT-WORK-REC.               
01196      MOVE 'A'                    TO  SWR-REC-TYPE.                
01197      MOVE AM-CARRIER             TO  SWR-CARR.                    
01198      MOVE AM-GROUPING            TO  SWR-GROUP.                   
01199      MOVE AM-STATE               TO  SWR-STATE.                   
01200      MOVE AM-ACCOUNT             TO  SWR-ACCOUNT.                 
01201      MOVE AM-NAME                TO  SWR-ACCT-NAME.               
01202      MOVE AM-REPORT-CODE-1       TO  SWR-REPORT-CODE-1.           
01203      MOVE AM-REPORT-CODE-2       TO  SWR-REPORT-CODE-2.           
01204      MOVE ACCUMULATOR-TABLE      TO  SWR-TABLE.                   
01205                                                                   
01206      IF DTE-TOT-OPT = 3 OR 4                                      
01207          MOVE SPACES TO SWR-ACCOUNT  SWR-ACCT-NAME.               
01208                                                                   
01209      IF AM-REPORT-CODE-1 NOT = SPACES                             
01210          MOVE 'Y' TO WS-PRINT-RPT-B.                              
01211                                                                   
01212      IF AM-REPORT-CODE-2 NOT = SPACES                             
01213          MOVE 'Y' TO WS-PRINT-RPT-C.                              
01214                                                                   
01215      WRITE TEMP-WORK-REC FROM SORT-WORK-REC.                      
01216                                                                   
01217  1099-EXIT.                                                       
01218      EXIT.                                                        
01219  EJECT                                                            
01220  2000-REINSURE-ROUTINE.                                           
01221                                                                   
01222      IF DTE-PRC-OPT = 5                                           
01223          IF NOT CR-LF-POLICY-IS-ACTIVE                            
01224              MOVE ZEROS          TO CR-LFTYP.                     
01225      IF DTE-PRC-OPT = 5                                           
01226          IF NOT CR-AH-POLICY-IS-ACTIVE                            
01227              MOVE ZEROS          TO CR-AHTYP.                     
01228                                                                   
01229      IF CR-LFTYP = ZEROS  AND                                     
01230         CR-AHTYP = ZEROS                                          
01231          GO TO 2099-EXIT.                                         
01232                                                                   
01233      MOVE SPACES                 TO  REIN-LEVELS-END.             
01234                                                                   
01235      PERFORM CLEAR-REIN-HOLD THRU CLEAR-REIN-HOLD-X.              
01236                                                                   
01237      MOVE DTE-CLASIC-COMPANY-CD  TO  REIN-SRCH-COMP-CD.           
01238      MOVE 'A'                    TO  REIN-SRCH-CODE.              
01239      MOVE CR-REIN-TABLE          TO  REIN-SRCH SWR2-REINS-TABLE.  
01240                                                                   
01241      IF REIN-SRCH NOT = SAVE-REIN-SRCH                            
01242          MOVE REIN-SRCH          TO  SAVE-REIN-SRCH               
01243          MOVE REIN-SEARCH        TO  RE-CONTROL-PRIMARY           
01244          READ ERRTBL-IN
01245              IF ERRTBL-FILE-STATUS = '23'                         
01246                  DISPLAY 'INVALID REINSURANCE TABLE CODE - '      
01247                            REIN-SRCH ' ' CR-FULL-CONTROL          
01248                  MOVE ERRTBL-FILE-STATUS TO WS-ABEND-FILE-STATUS  
01249                  GO TO ABEND-PGM                                  
01250              ELSE                                                 
01251                  IF ERRTBL-FILE-STATUS NOT = '00'                 
01252                      MOVE 'ERROR OCCURED READ - ERRTBL'           
01253                                          TO WS-ABEND-MESSAGE      
01254                      MOVE ERRTBL-FILE-STATUS                      
01255                                          TO WS-ABEND-FILE-STATUS  
01256                      GO TO ABEND-PGM.                             
01257                                                                   
01258                                                                   
01259      IF RE-REMAINING (1) NOT = 'I'                                
01260          GO TO 2010-REINSURE-ROUTINE-GET-CALC.                    
01261                                                                   
01262      MOVE SPACE                        TO REIN-CANCEL-LF-SW       
01263                                           REIN-CANCEL-AH-SW.      
01264      MOVE ZEROS                        TO REIN-EARN-LF-TERM       
01265                                           REIN-LCNC-EARN-TERM     
01266                                           REIN-LF-CLM-MONTHS      
01267                                           REIN-EARN-AH-TERM       
01268                                           REIN-ACNC-EARN-TERM     
01269                                           REIN-AH-PRIOR-CLMS-PAID 
01270                                           REIN-AH-CLM-MONTHS      
01271                                           REIN-AH-CLM-MTH-1       
01272                                           REIN-AH-CLM-MTH-2       
01273                                           REIN-AH-CLM-MTH-3       
01274                                           REIN-AH-CLM-MTH-4       
01275                                           REIN-AH-CLM-MTH-5.      
01276                                                                   
01277      MOVE CR-DT                        TO DC-GREG-DATE-CYMD.      
01278      MOVE 'L'                          TO DC-OPTION-CODE.         
01279      PERFORM 0800-DATE-CONVERT-ROUTINE THRU 0899-EXIT.            
01280      MOVE DC-BIN-DATE-1                TO WS-CR-BIN-DATE.         
01281                                                                   
01282      IF CR-LFTYP = ZEROS  OR  SPACES                              
01283          GO TO 2003-CONTINUE.                                     
01284                                                                   
01285      MOVE EOM-RUN-DATE                 TO VALUATION-DT.           
01286      MOVE BIN-EOM-DT                   TO BIN-VALUATION-DT.       
01287                                                                   
01288      IF CR-LF-CANCEL-EXIT-DATE NOT = ZEROS                        
01289        IF CR-LF-CANCEL-EXIT-DATE  LESS THAN VALUATION-DT          
01290            MOVE CR-LF-CANCEL-EXIT-DATE TO VALUATION-DT.           
01291                                                                   
01292      IF CR-LF-CLAIM-EXIT-DATE NOT = ZEROS                         
01293        IF CR-LF-CLAIM-EXIT-DATE  LESS THAN VALUATION-DT           
01294            MOVE CR-LF-CLAIM-EXIT-DATE  TO VALUATION-DT.           
01295                                                                   
01296      MOVE VALUATION-DT                 TO DC-GREG-DATE-CYMD.      
01297      PERFORM 2009-CALC-ELAPSED-MONTHS THRU 2009-CALC-EXIT.        
01298      IF DC-ELAPSED-MONTHS GREATER THAN CR-LF-TERM                 
01299          MOVE CR-LF-TERM               TO REIN-EARN-LF-TERM       
01300      ELSE                                                         
01301          MOVE DC-ELAPSED-MONTHS        TO REIN-EARN-LF-TERM.      
01302                                                                   
01303      IF CR-LF-CANC-DT NOT = ZEROS                                 
01304          MOVE CR-LF-CANC-DT            TO DC-GREG-DATE-CYMD       
01305          PERFORM 2009-CALC-ELAPSED-MONTHS THRU 2009-CALC-EXIT     
01306          MOVE DC-ELAPSED-MONTHS        TO REIN-LCNC-EARN-TERM     
01307          IF CR-DT = CR-LF-CANC-DT                                 
01308              MOVE '*'                  TO REIN-CANCEL-LF-SW.      
01309                                                                   
01310      IF CR-DTH-DT NOT = ZEROS                                     
01311          MOVE CR-DTH-DT                TO DC-GREG-DATE-CYMD       
01312          PERFORM 2009-CALC-ELAPSED-MONTHS THRU 2009-CALC-EXIT     
01313          MOVE DC-ELAPSED-MONTHS        TO REIN-LF-CLM-MONTHS.     
01314                                                                   
01315                                                                   
01316  2003-CONTINUE.                                                   
01317                                                                   
01318      IF CR-AHTYP = ZEROS  OR  SPACES                              
01319         GO TO 2010-REINSURE-ROUTINE-GET-CALC.                     
01320                                                                   
01321      MOVE EOM-RUN-DATE                 TO VALUATION-DT.           
01322      MOVE BIN-EOM-DT                   TO BIN-VALUATION-DT.       
01323                                                                   
01324      IF CR-AH-CANCEL-EXIT-DATE NOT = ZEROS                        
01325        IF CR-AH-CANCEL-EXIT-DATE  LESS THAN VALUATION-DT          
01326            MOVE CR-AH-CANCEL-EXIT-DATE TO VALUATION-DT.           
01327                                                                   
01328      IF CR-AH-SETTLEMENT-EXIT-DATE NOT = ZEROS                    
01329        IF CR-AH-SETTLEMENT-EXIT-DATE  LESS THAN VALUATION-DT      
01330            MOVE CR-AH-SETTLEMENT-EXIT-DATE TO VALUATION-DT.       
01331                                                                   
01332      MOVE VALUATION-DT                 TO DC-GREG-DATE-CYMD.      
01333      PERFORM 2009-CALC-ELAPSED-MONTHS THRU 2009-CALC-EXIT.        
01334      IF DC-ELAPSED-MONTHS GREATER THAN CR-AH-TERM                 
01335          MOVE CR-AH-TERM               TO REIN-EARN-AH-TERM       
01336      ELSE                                                         
01337          MOVE DC-ELAPSED-MONTHS        TO REIN-EARN-AH-TERM.      
01338                                                                   
01339      IF CR-AH-CANC-DT NOT = ZEROS                                 
01340          MOVE CR-AH-CANC-DT            TO DC-GREG-DATE-CYMD       
01341          PERFORM 2009-CALC-ELAPSED-MONTHS THRU 2009-CALC-EXIT     
01342          MOVE DC-ELAPSED-MONTHS        TO REIN-ACNC-EARN-TERM     
01343          IF CR-DT = CR-AH-CANC-DT                                 
01344              MOVE '*'                  TO REIN-CANCEL-AH-SW.      
01345                                                                   
01346      IF CR-DIS-INCUR-DT (1) NOT = ZEROS                           
01347          MOVE CR-DIS-INCUR-DT(1)       TO DC-GREG-DATE-CYMD       
01348          PERFORM 2009-CALC-ELAPSED-MONTHS THRU 2009-CALC-EXIT     
01349          MOVE DC-ELAPSED-MONTHS        TO REIN-AH-CLM-MTH-1.      
01350                                                                   
01351      IF CR-DIS-INCUR-DT (2) NOT = ZEROS                           
01352          MOVE CR-DIS-INCUR-DT (2)      TO DC-GREG-DATE-CYMD       
01353          PERFORM 2009-CALC-ELAPSED-MONTHS THRU 2009-CALC-EXIT     
01354          MOVE DC-ELAPSED-MONTHS        TO REIN-AH-CLM-MTH-2.      
01355                                                                   
01356      IF CR-DIS-INCUR-DT (3) NOT = ZEROS                           
01357          MOVE CR-DIS-INCUR-DT (3)      TO DC-GREG-DATE-CYMD       
01358          PERFORM 2009-CALC-ELAPSED-MONTHS THRU 2009-CALC-EXIT     
01359          MOVE DC-ELAPSED-MONTHS        TO REIN-AH-CLM-MTH-3.      
01360                                                                   
01361      IF CR-DIS-INCUR-DT (4) NOT = ZEROS                           
01362          MOVE CR-DIS-INCUR-DT (4)      TO DC-GREG-DATE-CYMD       
01363          PERFORM 2009-CALC-ELAPSED-MONTHS THRU 2009-CALC-EXIT     
01364          MOVE DC-ELAPSED-MONTHS        TO REIN-AH-CLM-MTH-4.      
01365                                                                   
01366      IF CR-DIS-INCUR-DT (5) NOT = ZEROS                           
01367          MOVE CR-DIS-INCUR-DT (5)      TO DC-GREG-DATE-CYMD       
01368          PERFORM 2009-CALC-ELAPSED-MONTHS THRU 2009-CALC-EXIT     
01369          MOVE DC-ELAPSED-MONTHS        TO REIN-AH-CLM-MTH-5.      
01370                                                                   
01371                                                                   
01372      GO TO 2010-REINSURE-ROUTINE-GET-CALC.                        
01373                                                                   
01374                                                                   
01375  2009-CALC-ELAPSED-MONTHS.                                        
01376                                                                   
01377      MOVE 'L'                          TO DC-OPTION-CODE.         
01378      PERFORM 0800-DATE-CONVERT-ROUTINE THRU 0899-EXIT.            
01379      MOVE DC-BIN-DATE-1                TO DC-BIN-DATE-2.          
01380      MOVE WS-CR-BIN-DATE               TO DC-BIN-DATE-1.          
01381      MOVE '1'                          TO DC-OPTION-CODE.         
01382      MOVE ' '                          TO DC-CENTURY-ADJUSTMENT.  
01383      MOVE ZEROS                        TO DC-ELAPSED-MONTHS       
01384                                           DC-ODD-DAYS-OVER        
01385                                           DC-ELAPSED-DAYS.        
01386      PERFORM 0800-DATE-CONVERT-ROUTINE THRU 0899-EXIT.            
01387                                                                   
01388  2009-CALC-EXIT.                                                  
01389      EXIT.                                                        
01390                                                                   
01391                                                                   
01392  2010-REINSURE-ROUTINE-GET-CALC.                                  
01393                              COPY ECSRTPFM.                       
01394                                                                   
01395  2020-REINSURE-ROUTINE-SKIP-1.                                    
01396      MOVE +1 TO SUB1.                                             
01397                                                                   
01398  2030-REINSURE-ROUTINE-BLD-WORK.                                  
01399      IF REIN-COMP (SUB1) = SPACES                                 
01400          GO TO 2099-EXIT.                                         
01401                                                                   
01402      IF REIN-REM-SW (SUB1) = 'Z'                                  
01403          ADD +1 TO SUB1                                           
01404          GO TO 2030-REINSURE-ROUTINE-BLD-WORK.                    
01405                                                                   
01406      IF REIN-LF-AH-FLGS (SUB1) = SPACES                           
01407          ADD +1 TO SUB1                                           
01408          GO TO 2030-REINSURE-ROUTINE-BLD-WORK.                    
01409                                                                   
01410      IF CR-LFRFND = ZEROS  AND                                    
01411         CR-AHRFND = ZEROS                                         
01412          NEXT SENTENCE                                            
01413      ELSE                                                         
01414          PERFORM REINSURE-CALC-CANCEL THRU REINSURE-CALC-CANCEL-X.
01415                                                                   
01416      MOVE CR-DTHAMT TO RW-LFCLMWK.                                
01417      ADD  CR-DTHEXP TO RW-LFCLMWK.                                
01418      MOVE CR-DISAMT TO RW-AHCLMWK.                                
01419      ADD  CR-DISEXP TO RW-AHCLMWK.                                
01420                                                                   
01421      IF RW-LFCLMWK = ZEROS  AND                                   
01422         RW-AHCLMWK = ZEROS                                        
01423          NEXT SENTENCE                                            
01424      ELSE                                                         
01425          PERFORM REINSURE-CALC-CLAIM  THRU REINSURE-CALC-CLAIM-X. 
01426                                                                   
01427      MOVE REIN-COMP (SUB1)       TO  SWR2-REIN-COMP.              
01428      MOVE REIN-WORK-FLDS (SUB1)  TO  RWF-FIELDS.                  
01429      MOVE CR-CARRIER             TO  SWR2-CARR.                   
01430      MOVE CR-DT                  TO  SWR2-DT.                     
01431      MOVE CR-AGE                 TO  SWR2-AGE.                    
01432      MOVE CR-ENTRY-STATUS        TO  SWR2-ENTRY-STATUS.           
01433      MOVE CR-LF-CURRENT-STATUS   TO  SWR2-LF-CURRENT-STATUS.      
01434      MOVE CR-AH-CURRENT-STATUS   TO  SWR2-AH-CURRENT-STATUS.      
01435                                                                   
122002     IF CR-ENTRY-STATUS = '5' OR 'M'                              
01437          MOVE ZEROS              TO RWF-LFAMT    RWF-AHAMT        
01438                                     RWF-LFPRM    RWF-AHPRM.       
01439                                                                   
01440      MOVE ZEROS    TO SWR2-LFTYP SWR2-LF-TERM SWR2-LF-REM-TRM     
01441                       SWR2-LFAMT SWR2-LFPRM   SWR2-LFRFND         
01442                       SWR2-NUM-DTH-CLM        SWR2-DTHAMT         
01443                       SWR2-LF-EPR-78          SWR2-LF-EPR-PR      
01444                       SWR2-AHTYP SWR2-AH-TERM SWR2-AH-REM-TRM     
01445                       SWR2-AHAMT SWR2-AHPRM   SWR2-AHRFND         
01446                       SWR2-NUM-DIS-CLM        SWR2-DISAMT         
01447                       SWR2-AH-EPR-78          SWR2-AH-EPR-PR.     
01448                                                                   
01449      IF REIN-LF-FLG (SUB1) = 'X'  AND                             
01450         CR-LFTYP NOT = ZERO                                       
01451          MOVE CR-LFTYP           TO  SWR2-LFTYP                   
01452          MOVE CR-LF-TERM         TO  SWR2-LF-TERM                 
01453          MOVE LF-REM-TRM         TO  SWR2-LF-REM-TRM              
01454          MOVE RWF-LFAMT          TO  SWR2-LFAMT                   
01455          MOVE RWF-LFPRM          TO  SWR2-LFPRM                   
01456          MOVE RWF-LFRFND         TO  SWR2-LFRFND                  
01457          MOVE CR-NUM-DTH-CLM     TO  SWR2-NUM-DTH-CLM             
01458          MOVE RWF-LFCLM          TO  SWR2-DTHAMT                  
01459          IF REIN-REM-SW (SUB1) = 'I'                              
01460              COMPUTE SWR2-LF-EPR-78 = RWF-LFPRM - RWF-LFRFND      
01461              MOVE SWR2-LF-EPR-78 TO  SWR2-LF-EPR-PR               
01462          ELSE                                                     
01463              IF CR-LFPRM NOT = ZEROS                              
01464                  COMPUTE SWR2-LF-EPR-78 =                         
01465                         (RWF-LFPRM * SAVE-LF-EPR-R78) / CR-LFPRM  
01466                  COMPUTE SWR2-LF-EPR-PR =                         
01467                         (RWF-LFPRM * SAVE-LF-EPR-PRO) / CR-LFPRM. 
01468                                                                   
01469      IF REIN-AH-FLG (SUB1) = 'X'  AND                             
01470         CR-AHTYP NOT = ZERO                                       
01471          MOVE CR-AHTYP           TO  SWR2-AHTYP                   
01472          MOVE CR-AH-TERM         TO  SWR2-AH-TERM                 
01473          MOVE AH-REM-TRM         TO  SWR2-AH-REM-TRM              
01474          MOVE RWF-AHAMT          TO  SWR2-AHAMT                   
01475          MOVE RWF-AHPRM          TO  SWR2-AHPRM                   
01476          MOVE RWF-AHRFND         TO  SWR2-AHRFND                  
01477          MOVE CR-NUM-DIS-CLM     TO  SWR2-NUM-DIS-CLM             
01478          MOVE RWF-AHCLM          TO  SWR2-DISAMT                  
01479          IF REIN-REM-SW (SUB1) = 'I'                              
01480              COMPUTE SWR2-AH-EPR-78 = RWF-AHPRM - RWF-AHRFND      
01481              MOVE SWR2-AH-EPR-78 TO  SWR2-AH-EPR-PR               
01482          ELSE                                                     
01483              IF CR-AHPRM NOT = ZEROS                              
01484                  COMPUTE SWR2-AH-EPR-78 =                         
01485                         (RWF-AHPRM * SAVE-AH-EPR-R78) / CR-AHPRM  
01486                  COMPUTE SWR2-AH-EPR-PR =                         
01487                         (RWF-AHPRM * SAVE-AH-EPR-PRO) / CR-AHPRM. 
01488                                                                   
01489                                                                   
01490      WRITE REIN-WORK-REC  FROM  SORT-WORK-REC-2.                  
01491                                                                   
01492      ADD +1 TO SUB1.                                              
01493                                                                   
01494      GO TO 2030-REINSURE-ROUTINE-BLD-WORK.                        
01495                                                                   
01496  2099-EXIT.                                                       
01497      EXIT.                                                        
01498                                                                   
01499  EJECT                                                            
01500  2900-REINSURANCE-ROUTINES.                                       
01501                              COPY ECSRIRTN.                       
01502                                                                   
01503  EJECT                                                            
01504  3000-PRODUCE-REPORTS.                                            
01505      PERFORM 3200-PROCESS-EXTRACTS THRU 3299-EXIT                 
01506                                    UNTIL END-OF-EXTR.             
01507                                                                   
01508  3099-EXIT.                                                       
01509      EXIT.                                                        
01510  EJECT                                                            
01511  3200-PROCESS-EXTRACTS.                                           
01512      IF EXTR-NEEDED                                               
01513          PERFORM 9300-READ-SORTED-FILE THRU 9399-EXIT.            
01514                                                                   
01515      IF WK-RPT-CODE = 'B' AND SWR-REPORT-CODE-1 = SPACES          
01516          GO TO 3299-EXIT.                                         
01517                                                                   
01518      IF WK-RPT-CODE = 'C' AND SWR-REPORT-CODE-2 = SPACES          
01519          GO TO 3299-EXIT.                                         
01520                                                                   
01521      IF WK-RPT-CODE = 'A'                                         
01522          MOVE  SWR-CARR          TO  CURR-CARR                    
01523          MOVE  SWR-GROUP         TO  CURR-GRP                     
01524          MOVE  SWR-STATE         TO  CURR-STATE                   
01525          MOVE  SWR-ACCOUNT       TO  CURR-ACCT                    
01526      ELSE                                                         
01527      IF WK-RPT-CODE = 'B'                                         
01528          MOVE  SWR-REPORT-CODE-1 TO CURR-REPORT-CODE-1            
01529      ELSE                                                         
01530      IF WK-RPT-CODE = 'C'                                         
01531          MOVE  SWR-CARR          TO  CURR-CARR                    
01532          MOVE  SWR-GROUP         TO  CURR-GRP                     
01533          MOVE  SWR-REPORT-CODE-2 TO  CURR-REPORT-CODE-2.          
01534                                                                   
01535      IF FIRST-TIME                                                
01536          MOVE  'N'               TO  FIRST-TIME-SW                
01537          MOVE  SWR-ACCT-NAME     TO  SAVE-ACCT-NAME               
01538          MOVE  SWR-CARR          TO  PREV-CARR CURR-CARR          
01539          MOVE  SWR-GROUP         TO  PREV-GRP CURR-GRP            
01540          MOVE  SWR-STATE         TO  PREV-STATE CURR-STATE        
01541          MOVE  SWR-ACCOUNT       TO  PREV-ACCT CURR-ACCT          
01542          MOVE  SWR-REPORT-CODE-1 TO  PREV-REPORT-CODE-1           
01543                                      CURR-REPORT-CODE-1           
01544          MOVE  SWR-REPORT-CODE-2 TO  PREV-REPORT-CODE-2           
01545                                      CURR-REPORT-CODE-2.          
01546                                                                   
01547      IF WK-RPT-CODE NOT = 'A'                                     
01548          GO TO 3220-CHECK-REPORT-B.                               
01549                                                                   
01550      IF SWR-CONTROL = HIGH-VALUES                                 
01551          PERFORM 3400-ACCT-BREAK  THRU  3499-EXIT                 
01552          PERFORM 3500-ST-BREAK    THRU  3599-EXIT                 
01553          PERFORM 3600-GROUP-BREAK THRU  3699-EXIT                 
01554          PERFORM 3700-CARR-BREAK  THRU  3749-EXIT                 
01555          PERFORM 3750-FINAL-BREAK THRU  3799-EXIT                 
01556          GO TO 3250-BUILD-CONTROL.                                
01557                                                                   
01558      IF PREV-CARR NOT = CURR-CARR                                 
01559          PERFORM 3400-ACCT-BREAK  THRU  3499-EXIT                 
01560          PERFORM 3500-ST-BREAK    THRU  3599-EXIT                 
01561          PERFORM 3600-GROUP-BREAK THRU  3699-EXIT                 
01562          PERFORM 3700-CARR-BREAK  THRU  3749-EXIT                 
01563          GO TO 3250-BUILD-CONTROL.                                
01564                                                                   
01565      IF PREV-GRP NOT = CURR-GRP                                   
01566          PERFORM 3400-ACCT-BREAK  THRU  3499-EXIT                 
01567          PERFORM 3500-ST-BREAK    THRU  3599-EXIT                 
01568          PERFORM 3600-GROUP-BREAK THRU  3699-EXIT                 
01569          GO TO 3250-BUILD-CONTROL.                                
01570                                                                   
01571      IF PREV-STATE NOT = CURR-STATE                               
01572          PERFORM 3400-ACCT-BREAK  THRU  3499-EXIT                 
01573          PERFORM 3500-ST-BREAK    THRU  3599-EXIT                 
01574          GO TO 3250-BUILD-CONTROL.                                
01575                                                                   
01576      IF DTE-TOT-OPT = 1 OR 2                                      
01577        IF PREV-ACCT NOT = CURR-ACCT                               
01578            PERFORM 3400-ACCT-BREAK  THRU  3499-EXIT.              
01579                                                                   
01580  3220-CHECK-REPORT-B.                                             
01581      IF WK-RPT-CODE = 'B'                                         
01582          IF PREV-REPORT-CODE-1 NOT = CURR-REPORT-CODE-1           
01583              PERFORM 3300-REPORT-BREAK THRU 3399-EXIT.            
01584                                                                   
01585      IF WK-RPT-CODE = 'C'                                         
01586          IF PREV-REPORT-CODE-2 NOT = CURR-REPORT-CODE-2 OR        
01587             PREV-CARR          NOT = CURR-CARR          OR        
01588             PREV-GRP           NOT = CURR-GRP                     
01589              PERFORM 3300-REPORT-BREAK THRU 3399-EXIT.            
01590                                                                   
01591  3250-BUILD-CONTROL.                                              
01592      IF SWR-CONTROL = HIGH-VALUES                                 
01593          GO TO 3299-EXIT.                                         
01594                                                                   
01595      PERFORM  3800-ACCUMULATE-FOR-CONTROL  THRU  3899-EXIT.       
01596                                                                   
01597      MOVE  'Y'                   TO EXTR-NEEDED-SW.               
01598      MOVE CURR-CARR              TO PREV-CARR.                    
01599      MOVE CURR-STATE             TO PREV-STATE.                   
01600      MOVE CURR-GRP               TO PREV-GRP.                     
01601      MOVE CURR-ACCT              TO PREV-ACCT.                    
01602      MOVE SWR-ACCT-NAME          TO SAVE-ACCT-NAME.               
01603      MOVE CURR-REPORT-CODE-1     TO PREV-REPORT-CODE-1.           
01604      MOVE CURR-REPORT-CODE-2     TO PREV-REPORT-CODE-2.           
01605                                                                   
01606  3299-EXIT.                                                       
01607      EXIT.                                                        
01608  EJECT                                                            
01609  3300-REPORT-BREAK.                                               
01610      MOVE '1' TO WK-BREAK-TYPE.                                   
01611                                                                   
01612      IF WK-RPT-CODE = 'D'                                         
01613          PERFORM 3900-ACCUMULATE-FOR-BREAK THRU 3999-EXIT.        
01614                                                                   
01615      PERFORM 4000-CREATE-PRINT-TABLE      THRU 4099-EXIT.         
01616      PERFORM 4500-PRINT-A-PAGE            THRU 4599-EXIT.         
01617      PERFORM 6500-CLEAR-ACCUMULATOR-TABLE THRU 6599-EXIT.         
01618      PERFORM 6700-CLEAR-PRINT-TABLE       THRU 6799-EXIT.         
01619                                                                   
01620  3399-EXIT.                                                       
01621       EXIT.                                                       
01622  EJECT                                                            
01623  3400-ACCT-BREAK.                                                 
01624      MOVE '1' TO WK-BREAK-TYPE.                                   
01625      PERFORM 3900-ACCUMULATE-FOR-BREAK THRU 3999-EXIT.            
01626                                                                   
01627      IF DTE-TOT-OPT = 1 OR 2                                      
01628          PERFORM 4000-CREATE-PRINT-TABLE THRU 4099-EXIT           
01629          PERFORM 4500-PRINT-A-PAGE       THRU 4599-EXIT.          
01630                                                                   
01631      PERFORM 6500-CLEAR-ACCUMULATOR-TABLE THRU 6599-EXIT.         
01632      PERFORM 6700-CLEAR-PRINT-TABLE       THRU 6799-EXIT.         
01633                                                                   
01634  3499-EXIT.                                                       
01635       EXIT.                                                       
01636  EJECT                                                            
01637  3500-ST-BREAK.                                                   
01638      MOVE '2' TO WK-BREAK-TYPE.                                   
01639      PERFORM 3900-ACCUMULATE-FOR-BREAK    THRU 3999-EXIT.         
01640      PERFORM 4000-CREATE-PRINT-TABLE      THRU 4099-EXIT.         
01641      PERFORM 4500-PRINT-A-PAGE            THRU 4599-EXIT.         
01642      PERFORM 6500-CLEAR-ACCUMULATOR-TABLE THRU 6599-EXIT.         
01643      PERFORM 6700-CLEAR-PRINT-TABLE       THRU 6799-EXIT.         
01644                                                                   
01645  3599-EXIT.                                                       
01646       EXIT.                                                       
01647  EJECT                                                            
01648  3600-GROUP-BREAK.                                                
01649      MOVE '3' TO WK-BREAK-TYPE.                                   
01650      PERFORM 3900-ACCUMULATE-FOR-BREAK    THRU 3999-EXIT.         
01651      PERFORM 4000-CREATE-PRINT-TABLE      THRU 4099-EXIT.         
01652      PERFORM 4500-PRINT-A-PAGE            THRU 4599-EXIT.         
01653      PERFORM 6500-CLEAR-ACCUMULATOR-TABLE THRU 6599-EXIT.         
01654      PERFORM 6700-CLEAR-PRINT-TABLE       THRU 6799-EXIT.         
01655                                                                   
01656  3699-EXIT.                                                       
01657       EXIT.                                                       
01658  EJECT                                                            
01659  3700-CARR-BREAK.                                                 
01660      MOVE '4' TO WK-BREAK-TYPE.                                   
01661      PERFORM 3900-ACCUMULATE-FOR-BREAK    THRU 3999-EXIT.         
01662      PERFORM 4000-CREATE-PRINT-TABLE      THRU 4099-EXIT.         
01663      PERFORM 4500-PRINT-A-PAGE            THRU 4599-EXIT.         
01664      PERFORM 6500-CLEAR-ACCUMULATOR-TABLE THRU 6599-EXIT.         
01665      PERFORM 6700-CLEAR-PRINT-TABLE       THRU 6799-EXIT.         
01666                                                                   
01667  3749-EXIT.                                                       
01668       EXIT.                                                       
01669  EJECT                                                            
01670  3750-FINAL-BREAK.                                                
01671      MOVE '5' TO WK-BREAK-TYPE.                                   
01672      PERFORM 4000-CREATE-PRINT-TABLE      THRU 4099-EXIT.         
01673      PERFORM 4500-PRINT-A-PAGE            THRU 4599-EXIT.         
01674      PERFORM 6500-CLEAR-ACCUMULATOR-TABLE THRU 6599-EXIT.         
01675      PERFORM 6700-CLEAR-PRINT-TABLE       THRU 6799-EXIT.         
01676                                                                   
01677  3799-EXIT.                                                       
01678       EXIT.                                                       
01679  EJECT                                                            
01680  3800-ACCUMULATE-FOR-CONTROL.                                     
01681      PERFORM  3830-ACCUM-LOOP  THRU  3839-EXIT                    
01682          VARYING  A-SUB1  FROM  1  BY  1                          
01683            UNTIL  A-SUB1  GREATER  3                              
01684          AFTER  A-SUB2  FROM  1  BY  1                            
01685          UNTIL  A-SUB2  GREATER  7.                               
01686                                                                   
01687      GO TO 3899-EXIT.                                             
01688                                                                   
01689  3830-ACCUM-LOOP.                                                 
01690      MOVE  AT-AMOUNTS (A-SUB1  A-SUB2)                            
01691                                  TO  WORK-AREA-FOR-ACCUMULATOR.   
01692                                                                   
01693      ADD SWR-FACE-BEN    (A-SUB1  A-SUB2)  TO WA-FACE-BEN.        
01694      ADD SWR-ISS-PRM     (A-SUB1  A-SUB2)  TO WA-ISS-PRM.         
01695      ADD SWR-CNC-PRM     (A-SUB1  A-SUB2)  TO WA-CNC-PRM.         
01696      ADD SWR-EARNED-PRM  (A-SUB1  A-SUB2)  TO WA-EARNED-PRM.      
01697      ADD SWR-CLAIM-AMT   (A-SUB1  A-SUB2)  TO WA-CLAIM-AMT.       
01698      ADD SWR-CERT-COUNT  (A-SUB1  A-SUB2)  TO WA-CERT-COUNT.      
01699      ADD SWR-CLAIM-COUNT (A-SUB1  A-SUB2)  TO WA-CLAIM-COUNT.     
01700      ADD SWR-NUMBER-DAYS (A-SUB1  A-SUB2)  TO WA-NUMBER-DAYS.     
01701      ADD SWR-CRT-W-CLM   (A-SUB1  A-SUB2)  TO WA-CRT-W-CLM.       
01702                                                                   
01703      MOVE  WORK-AREA-FOR-ACCUMULATOR                              
01704                                  TO  AT-AMOUNTS (A-SUB1  A-SUB2). 
01705                                                                   
01706  3839-EXIT.                                                       
01707      EXIT.                                                        
01708                                                                   
01709  3899-EXIT.                                                       
01710      EXIT.                                                        
01711  EJECT                                                            
01712  3900-ACCUMULATE-FOR-BREAK.                                       
01713      PERFORM  3930-ACCUM-LOOP  THRU  3939-EXIT                    
01714          VARYING  A-SUB1  FROM  1  BY  1                          
01715            UNTIL  A-SUB1  GREATER  3                              
01716          AFTER  A-SUB2  FROM  1  BY  1                            
01717           UNTIL A-SUB2  GREATER  7.                               
01718                                                                   
01719      GO TO 3999-EXIT.                                             
01720                                                                   
01721  3930-ACCUM-LOOP.                                                 
01722      IF ACCOUNT-BREAK                                             
01723          ADD AT-FACE-BEN (A-SUB1 A-SUB2)                          
01724                                TO  AT2-FACE-BEN (A-SUB1 A-SUB2)   
01725          ADD AT-ISS-PRM (A-SUB1 A-SUB2)                           
01726                                TO  AT2-ISS-PRM (A-SUB1 A-SUB2)    
01727          ADD AT-CNC-PRM (A-SUB1 A-SUB2)                           
01728                                TO  AT2-CNC-PRM (A-SUB1 A-SUB2)    
01729          ADD AT-EARNED-PRM (A-SUB1 A-SUB2)                        
01730                                TO  AT2-EARNED-PRM (A-SUB1 A-SUB2) 
01731          ADD AT-CLAIM-AMT (A-SUB1 A-SUB2)                         
01732                                TO  AT2-CLAIM-AMT (A-SUB1 A-SUB2)  
01733          ADD AT-CERT-COUNT (A-SUB1 A-SUB2)                        
01734                                TO  AT2-CERT-COUNT (A-SUB1 A-SUB2) 
01735          ADD AT-CLAIM-COUNT (A-SUB1 A-SUB2)                       
01736                                TO  AT2-CLAIM-COUNT (A-SUB1 A-SUB2)
01737          ADD AT-NUMBER-DAYS (A-SUB1 A-SUB2)                       
01738                                TO  AT2-NUMBER-DAYS (A-SUB1 A-SUB2)
01739          ADD AT-CRT-W-CLM (A-SUB1 A-SUB2)                         
01740                                TO  AT2-CRT-W-CLM (A-SUB1 A-SUB2)  
01741      ELSE                                                         
01742      IF STATE-BREAK                                               
01743          ADD AT2-FACE-BEN (A-SUB1 A-SUB2)                         
01744                                TO  AT3-FACE-BEN (A-SUB1 A-SUB2)   
01745          ADD AT2-ISS-PRM (A-SUB1 A-SUB2)                          
01746                                TO  AT3-ISS-PRM (A-SUB1 A-SUB2)    
01747          ADD AT2-CNC-PRM (A-SUB1 A-SUB2)                          
01748                                TO  AT3-CNC-PRM (A-SUB1 A-SUB2)    
01749          ADD AT2-EARNED-PRM (A-SUB1 A-SUB2)                       
01750                                TO  AT3-EARNED-PRM (A-SUB1 A-SUB2) 
01751          ADD AT2-CLAIM-AMT (A-SUB1 A-SUB2)                        
01752                                TO  AT3-CLAIM-AMT (A-SUB1 A-SUB2)  
01753          ADD AT2-CERT-COUNT (A-SUB1 A-SUB2)                       
01754                                TO  AT3-CERT-COUNT (A-SUB1 A-SUB2) 
01755          ADD AT2-CLAIM-COUNT (A-SUB1 A-SUB2)                      
01756                                TO  AT3-CLAIM-COUNT (A-SUB1 A-SUB2)
01757          ADD AT2-NUMBER-DAYS (A-SUB1 A-SUB2)                      
01758                                TO  AT3-NUMBER-DAYS (A-SUB1 A-SUB2)
01759          ADD AT2-CRT-W-CLM (A-SUB1 A-SUB2)                        
01760                                TO  AT3-CRT-W-CLM (A-SUB1 A-SUB2)  
01761      ELSE                                                         
01762      IF GROUP-BREAK                                               
01763          ADD AT3-FACE-BEN (A-SUB1 A-SUB2)                         
01764                                TO  AT4-FACE-BEN (A-SUB1 A-SUB2)   
01765          ADD AT3-ISS-PRM (A-SUB1 A-SUB2)                          
01766                                TO  AT4-ISS-PRM (A-SUB1 A-SUB2)    
01767          ADD AT3-CNC-PRM (A-SUB1 A-SUB2)                          
01768                                TO  AT4-CNC-PRM (A-SUB1 A-SUB2)    
01769          ADD AT3-EARNED-PRM (A-SUB1 A-SUB2)                       
01770                                TO  AT4-EARNED-PRM (A-SUB1 A-SUB2) 
01771          ADD AT3-CLAIM-AMT (A-SUB1 A-SUB2)                        
01772                                TO  AT4-CLAIM-AMT (A-SUB1 A-SUB2)  
01773          ADD AT3-CERT-COUNT (A-SUB1 A-SUB2)                       
01774                                TO  AT4-CERT-COUNT (A-SUB1 A-SUB2) 
01775          ADD AT3-CLAIM-COUNT (A-SUB1 A-SUB2)                      
01776                                TO  AT4-CLAIM-COUNT (A-SUB1 A-SUB2)
01777          ADD AT3-NUMBER-DAYS (A-SUB1 A-SUB2)                      
01778                                TO  AT4-NUMBER-DAYS (A-SUB1 A-SUB2)
01779          ADD AT3-CRT-W-CLM (A-SUB1 A-SUB2)                        
01780                                TO  AT4-CRT-W-CLM (A-SUB1 A-SUB2)  
01781      ELSE                                                         
01782      IF CARRIER-BREAK                                             
01783          ADD AT4-FACE-BEN (A-SUB1 A-SUB2)                         
01784                                TO  AT5-FACE-BEN (A-SUB1 A-SUB2)   
01785          ADD AT4-ISS-PRM (A-SUB1 A-SUB2)                          
01786                                TO  AT5-ISS-PRM (A-SUB1 A-SUB2)    
01787          ADD AT4-CNC-PRM (A-SUB1 A-SUB2)                          
01788                                TO  AT5-CNC-PRM (A-SUB1 A-SUB2)    
01789          ADD AT4-EARNED-PRM (A-SUB1 A-SUB2)                       
01790                                TO  AT5-EARNED-PRM (A-SUB1 A-SUB2) 
01791          ADD AT4-CLAIM-AMT (A-SUB1 A-SUB2)                        
01792                                TO  AT5-CLAIM-AMT (A-SUB1 A-SUB2)  
01793          ADD AT4-CERT-COUNT (A-SUB1 A-SUB2)                       
01794                                TO  AT5-CERT-COUNT (A-SUB1 A-SUB2) 
01795          ADD AT4-CLAIM-COUNT (A-SUB1 A-SUB2)                      
01796                                TO  AT5-CLAIM-COUNT (A-SUB1 A-SUB2)
01797          ADD AT4-NUMBER-DAYS (A-SUB1 A-SUB2)                      
01798                                TO  AT5-NUMBER-DAYS (A-SUB1 A-SUB2)
01799          ADD AT4-CRT-W-CLM (A-SUB1 A-SUB2)                        
01800                                TO  AT5-CRT-W-CLM (A-SUB1 A-SUB2). 
01801                                                                   
01802  3939-EXIT.                                                       
01803      EXIT.                                                        
01804                                                                   
01805  3999-EXIT.                                                       
01806      EXIT.                                                        
01807  EJECT                                                            
01808  4000-CREATE-PRINT-TABLE.                                         
01809      PERFORM  4020-CALC-PRINT-TOTALS  THRU  4029-EXIT             
01810          VARYING  A-SUB1  FROM  1  BY  1                          
01811            UNTIL  A-SUB1  GREATER  3                              
01812          AFTER  A-SUB2  FROM  1  BY  1                            
01813           UNTIL A-SUB2  GREATER  7.                               
01814                                                                   
01815      PERFORM  4050-CALC-PRINT-PRCNTS  THRU  4059-EXIT             
01816          VARYING  P-SUB1  FROM  1  BY  1                          
01817            UNTIL  P-SUB1  GREATER  4                              
01818          AFTER  P-SUB2  FROM  1  BY  1                            
01819           UNTIL P-SUB2  GREATER  8.                               
01820                                                                   
01821      PERFORM  4080-CALC-PRINT-AVERAGES  THRU  4089-EXIT           
01822          VARYING  P-SUB1  FROM  1  BY  1                          
01823            UNTIL  P-SUB1  GREATER  3                              
01824          AFTER  P-SUB2  FROM  1  BY  1                            
01825           UNTIL P-SUB2  GREATER  8.                               
01826                                                                   
01827      GO TO 4099-EXIT.                                             
01828  EJECT                                                            
01829  4020-CALC-PRINT-TOTALS.                                          
01830      MOVE  A-SUB1                TO  P-SUB1.                      
01831      MOVE  A-SUB2                TO  P-SUB2.                      
01832                                                                   
01833      IF ACCOUNT-BREAK                                             
01834          MOVE AT-AMOUNTS (A-SUB1 A-SUB2)                          
01835                                  TO  WORK-AREA-FOR-ACCUMULATOR    
01836      ELSE                                                         
01837      IF STATE-BREAK                                               
01838          MOVE AT2-AMOUNTS (A-SUB1 A-SUB2)                         
01839                                  TO  WORK-AREA-FOR-ACCUMULATOR    
01840      ELSE                                                         
01841      IF GROUP-BREAK                                               
01842          MOVE AT3-AMOUNTS (A-SUB1 A-SUB2)                         
01843                                  TO  WORK-AREA-FOR-ACCUMULATOR    
01844      ELSE                                                         
01845      IF CARRIER-BREAK                                             
01846          MOVE AT4-AMOUNTS (A-SUB1 A-SUB2)                         
01847                                  TO  WORK-AREA-FOR-ACCUMULATOR    
01848      ELSE                                                         
01849          MOVE AT5-AMOUNTS (A-SUB1 A-SUB2)                         
01850                                  TO  WORK-AREA-FOR-ACCUMULATOR.   
01851                                                                   
01852      ADD  WA-ISS-PRM         TO  PT-GROSS-PRM (P-SUB1  P-SUB2)    
01853                                  PT-GROSS-PRM (P-SUB1  8)         
01854                                  PT-GROSS-PRM (4  P-SUB2)         
01855                                  PT-GROSS-PRM (4  8).             
01856                                                                   
01857      ADD  WA-CNC-PRM         TO  PT-CNC-PRM (P-SUB1  P-SUB2)      
01858                                  PT-CNC-PRM (P-SUB1  8)           
01859                                  PT-CNC-PRM (4  P-SUB2)           
01860                                  PT-CNC-PRM (4  8).               
01861                                                                   
01862      ADD  WA-EARNED-PRM      TO  PT-EARNED-PRM (P-SUB1  P-SUB2)   
01863                                  PT-EARNED-PRM (P-SUB1  8)        
01864                                  PT-EARNED-PRM (4  P-SUB2)        
01865                                  PT-EARNED-PRM (4  8).            
01866                                                                   
01867      ADD  WA-CLAIM-AMT       TO  PT-CLAIMS (P-SUB1  P-SUB2)       
01868                                  PT-CLAIMS (P-SUB1  8)            
01869                                  PT-CLAIMS (4  P-SUB2)            
01870                                  PT-CLAIMS (4  8).                
01871                                                                   
01872      COMPUTE  NET-PREMIUM  = WA-ISS-PRM - WA-CNC-PRM.             
01873                                                                   
01874      ADD  NET-PREMIUM        TO  PT-NET-PRM (P-SUB1  P-SUB2)      
01875                                  PT-NET-PRM (P-SUB1  8)           
01876                                  PT-NET-PRM (4  P-SUB2)           
01877                                  PT-NET-PRM (4  8).               
01878                                                                   
01879      ADD  WA-FACE-BEN        TO  PT-FACE-BEN (P-SUB1  P-SUB2)     
01880                                  PT-FACE-BEN (P-SUB1  8).         
01881                                                                   
01882      ADD  WA-CERT-COUNT      TO  PT-CERT-COUNT (P-SUB1  P-SUB2)   
01883                                  PT-CERT-COUNT (P-SUB1  8).       
01884                                                                   
01885      ADD  WA-CLAIM-COUNT     TO  PT-CLAIM-COUNT (P-SUB1  P-SUB2)  
01886                                  PT-CLAIM-COUNT (P-SUB1  8).      
01887                                                                   
01888      ADD  WA-NUMBER-DAYS     TO  PT-NUMBER-DAYS (P-SUB1  P-SUB2)  
01889                                  PT-NUMBER-DAYS (P-SUB1  8).      
01890                                                                   
01891      ADD  WA-CRT-W-CLM       TO  PT-CRT-W-CLM (P-SUB1  P-SUB2)    
01892                                  PT-CRT-W-CLM (P-SUB1  8).        
01893                                                                   
01894  4029-EXIT.                                                       
01895      EXIT.                                                        
01896  EJECT                                                            
01897  4050-CALC-PRINT-PRCNTS.                                          
01898      IF  PT-EARNED-PRM (P-SUB1  P-SUB2)  = +0                     
01899          MOVE  +0.0          TO  PT-EL-RATIO (P-SUB1  P-SUB2)     
01900      ELSE                                                         
01901          COMPUTE  PT-EL-RATIO (P-SUB1  P-SUB2)  ROUNDED  =        
01902                                 PT-CLAIMS (P-SUB1  P-SUB2)  /     
01903                                 PT-EARNED-PRM (P-SUB1  P-SUB2).   
01904                                                                   
01905      IF  PT-GROSS-PRM (P-SUB1  8)  = +0                           
01906          MOVE  +0            TO  PT-PRCNT-PRM (P-SUB1  P-SUB2)    
01907      ELSE                                                         
01908          COMPUTE  PT-PRCNT-PRM (P-SUB1  P-SUB2)  ROUNDED  =       
01909                                PT-GROSS-PRM (P-SUB1  P-SUB2)  /   
01910                                PT-GROSS-PRM (P-SUB1  8).          
01911                                                                   
01912      IF  PT-CLAIMS (P-SUB1  8)  = +0                              
01913          MOVE  +0            TO  PT-PRCNT-CLAIM (P-SUB1  P-SUB2)  
01914      ELSE                                                         
01915          COMPUTE  PT-PRCNT-CLAIM (P-SUB1  P-SUB2)  ROUNDED  =     
01916                                PT-CLAIMS (P-SUB1  P-SUB2)  /      
01917                                PT-CLAIMS (P-SUB1  8).             
01918                                                                   
01919      IF  PT-GROSS-PRM (P-SUB1  P-SUB2)  = +0                      
01920          MOVE  +0            TO  PT-PRCNT-CNC-G (P-SUB1  P-SUB2)  
01921      ELSE                                                         
01922          COMPUTE  PT-PRCNT-CNC-G (P-SUB1  P-SUB2)  ROUNDED  =     
01923                                PT-CNC-PRM   (P-SUB1  P-SUB2)  /   
01924                                PT-GROSS-PRM (P-SUB1  P-SUB2).     
01925                                                                   
01926  4059-EXIT.                                                       
01927      EXIT.                                                        
01928  EJECT                                                            
01929  4080-CALC-PRINT-AVERAGES.                                        
01930      IF  PT-CERT-COUNT (P-SUB1  P-SUB2)  = +0                     
01931          MOVE  +0            TO  PT-AVG-LOAN (P-SUB1  P-SUB2)     
01932      ELSE                                                         
01933          COMPUTE  PT-AVG-LOAN (P-SUB1  P-SUB2)  ROUNDED  =        
01934                               PT-FACE-BEN (P-SUB1  P-SUB2)  /     
01935                               PT-CERT-COUNT (P-SUB1  P-SUB2).     
01936                                                                   
01937      IF  PT-CLAIM-COUNT (P-SUB1  P-SUB2)  = +0                    
01938          MOVE  +0            TO  PT-AVG-CLAIM (P-SUB1  P-SUB2)    
01939      ELSE                                                         
01940          COMPUTE  PT-AVG-CLAIM (P-SUB1  P-SUB2)  ROUNDED  =       
01941                               PT-CLAIMS (P-SUB1  P-SUB2)  /       
01942                               PT-CLAIM-COUNT (P-SUB1  P-SUB2).    
01943                                                                   
01944      IF  PT-CRT-W-CLM (P-SUB1  P-SUB2)  = +0                      
01945          MOVE  +0            TO  PT-AVG-DAYS (P-SUB1  P-SUB2)     
01946      ELSE                                                         
01947          COMPUTE  PT-AVG-DAYS (P-SUB1  P-SUB2)  ROUNDED  =        
01948                               PT-NUMBER-DAYS (P-SUB1  P-SUB2)  /  
01949                               PT-CRT-W-CLM (P-SUB1  P-SUB2).      
01950                                                                   
01951  4089-EXIT.                                                       
01952      EXIT.                                                        
01953                                                                   
01954  4099-EXIT.                                                       
01955      EXIT.                                                        
01956  EJECT                                                            
01957  4500-PRINT-A-PAGE.                                               
01958      PERFORM  7000-HEADING-ROUTINE  THRU  7099-EXIT.              
01959                                                                   
01960      PERFORM  4510-PRINT-REPORT-SECTIONS  THRU  4514-EXIT         
01961          VARYING  P-SUB1  FROM  1  BY  1                          
01962            UNTIL  P-SUB1  GREATER  4.                             
01963                                                                   
01964      GO TO 4599-EXIT.                                             
01965                                                                   
01966  4510-PRINT-REPORT-SECTIONS.                                      
01967      IF P-SUB1 = 1                                                
01968          MOVE  'SINGLE'          TO  H7-DESC1-7                   
01969          MOVE LIFE-OVERRIDE-L12  TO  H7-DESC8-20.                 
01970                                                                   
01971      IF P-SUB1 = 2                                                
01972          MOVE  'JOINT'           TO  H7-DESC1-7                   
01973          MOVE LIFE-OVERRIDE-L12  TO  H7-DESC8-20.                 
01974                                                                   
01975      IF P-SUB1 = 3                                                
01976          MOVE  AH-OVERRIDE-L12   TO  H7-DESC.                     
01977                                                                   
01978      IF P-SUB1 = 4                                                
01979          MOVE  'COMBINED TOTALS'  TO  H7-DESC.                    
01980                                                                   
01981      MOVE  '0'                   TO  X.                           
01982      MOVE  HEADING-7             TO  P-DATA.                      
01983      PERFORM  7700-PRINT-RTN  THRU  7799-EXIT.                    
01984                                                                   
01985      MOVE  'GROSS PREMIUM'       TO  DL-DESC.                     
01986                                                                   
01987      PERFORM  4515-LOAD-GROSS-PRM  THRU  4519-EXIT                
01988          VARYING  P-SUB2  FROM  1  BY  1                          
01989            UNTIL  P-SUB2  GREATER 8.                              
01990                                                                   
01991      MOVE  DETAIL-LINE           TO  P-DATA.                      
01992      PERFORM  7700-PRINT-RTN  THRU  7799-EXIT.                    
01993                                                                   
01994      MOVE  ' '                   TO  X.                           
01995      MOVE  'REFUNDS'             TO  DL-DESC.                     
01996                                                                   
01997      PERFORM  4520-LOAD-CNC-PRM  THRU  4524-EXIT                  
01998          VARYING  P-SUB2  FROM  1  BY  1                          
01999            UNTIL  P-SUB2  GREATER  8.                             
02000                                                                   
02001      MOVE  DETAIL-LINE           TO  P-DATA.                      
02002      PERFORM  7700-PRINT-RTN  THRU  7799-EXIT.                    
02003                                                                   
02004      MOVE  'NET PREMIUM'         TO  DL-DESC.                     
02005                                                                   
02006      PERFORM  4525-LOAD-NET-PRM  THRU  4529-EXIT                  
02007          VARYING  P-SUB2  FROM  1  BY  1                          
02008            UNTIL  P-SUB2  GREATER 8.                              
02009                                                                   
02010      MOVE  DETAIL-LINE           TO  P-DATA.                      
02011      PERFORM  7700-PRINT-RTN  THRU  7799-EXIT.                    
02012                                                                   
02013      MOVE  'EARNED PREMIUM'      TO  DL-DESC.                     
02014                                                                   
02015      PERFORM  4530-LOAD-EARNED-PRM  THRU  4534-EXIT               
02016          VARYING  P-SUB2  FROM  1  BY  1                          
02017            UNTIL  P-SUB2  GREATER 8.                              
02018                                                                   
02019      MOVE  DETAIL-LINE           TO  P-DATA.                      
02020      PERFORM  7700-PRINT-RTN  THRU  7799-EXIT.                    
02021                                                                   
02022      MOVE  'CLAIMS PAID'         TO  DL-DESC.                     
02023                                                                   
02024      PERFORM  4535-LOAD-CLAIMS  THRU  4539-EXIT                   
02025          VARYING  P-SUB2  FROM  1  BY  1                          
02026            UNTIL  P-SUB2  GREATER 8.                              
02027                                                                   
02028      MOVE  DETAIL-LINE           TO  P-DATA.                      
02029      PERFORM  7700-PRINT-RTN  THRU  7799-EXIT.                    
02030                                                                   
02031      MOVE  'EARN/LOSS RATIO'     TO  DL-DESC.                     
02032                                                                   
02033      PERFORM  4540-LOAD-EL-RATIO  THRU  4544-EXIT                 
02034          VARYING  P-SUB2  FROM  1  BY  1                          
02035            UNTIL  P-SUB2  GREATER 8.                              
02036                                                                   
02037      MOVE  DETAIL-LINE           TO  P-DATA.                      
02038      PERFORM  7700-PRINT-RTN  THRU  7799-EXIT.                    
02039                                                                   
02040      MOVE  '% PREMIUM BY AGE'    TO  DL-DESC.                     
02041                                                                   
02042      PERFORM  4545-LOAD-PRCNT-PRM  THRU  4549-EXIT                
02043          VARYING  P-SUB2  FROM  1  BY  1                          
02044            UNTIL  P-SUB2  GREATER 8.                              
02045                                                                   
02046      MOVE  DETAIL-LINE           TO  P-DATA.                      
02047      PERFORM  7700-PRINT-RTN  THRU  7799-EXIT.                    
02048                                                                   
02049      MOVE  '% CLAIMS BY AGE'     TO  DL-DESC.                     
02050                                                                   
02051      PERFORM  4550-LOAD-PRCNT-CLAIM  THRU  4554-EXIT              
02052          VARYING  P-SUB2  FROM  1  BY  1                          
02053            UNTIL  P-SUB2  GREATER 8.                              
02054                                                                   
02055      MOVE  DETAIL-LINE           TO  P-DATA.                      
02056      PERFORM  7700-PRINT-RTN  THRU  7799-EXIT.                    
02057                                                                   
02058      MOVE  '% REFUNDS TO GROSS'  TO  DL-DESC.                     
02059                                                                   
02060      PERFORM  4555-LOAD-PRCNT-CNC-G  THRU  4559-EXIT              
02061          VARYING  P-SUB2  FROM  1  BY  1                          
02062            UNTIL  P-SUB2  GREATER 8.                              
02063                                                                   
02064      MOVE  DETAIL-LINE           TO  P-DATA.                      
02065      PERFORM  7700-PRINT-RTN  THRU  7799-EXIT.                    
02066                                                                   
02067      IF P-SUB1 = 4                                                
02068          GO TO 4514-EXIT.                                         
02069                                                                   
02070      IF P-SUB1 = 1                                                
02071          MOVE  'AVG INITIAL EXPOSURE'  TO  DL-DESC                
02072          PERFORM  4560-LOAD-AVG-LOAN  THRU  4564-EXIT             
02073              VARYING  P-SUB2  FROM  1  BY  1                      
02074                UNTIL  P-SUB2  GREATER 8                           
02075          MOVE  DETAIL-LINE             TO  P-DATA                 
02076          PERFORM  7700-PRINT-RTN  THRU  7799-EXIT.                
02077                                                                   
02078      IF P-SUB1 = 1 OR 2                                           
02079          MOVE  'AVG. CLAIM PAID'  TO  DL-DESC                     
02080          PERFORM  4565-LOAD-AVG-CLAIM  THRU  4569-EXIT            
02081              VARYING  P-SUB2  FROM  1  BY  1                      
02082                UNTIL  P-SUB2  GREATER 8                           
02083          MOVE  DETAIL-LINE        TO  P-DATA                      
02084          PERFORM  7700-PRINT-RTN  THRU  7799-EXIT.                
02085                                                                   
02086      IF P-SUB1 = 3                                                
02087          MOVE  'AVG. DAYS INDEMNITY'  TO  DL-DESC                 
02088          PERFORM  4570-LOAD-AVG-DAYS  THRU  4574-EXIT             
02089              VARYING  P-SUB2  FROM  1  BY  1                      
02090                UNTIL  P-SUB2  GREATER 8                           
02091          MOVE  DETAIL-LINE            TO  P-DATA                  
02092          PERFORM  7700-PRINT-RTN  THRU  7799-EXIT.                
02093                                                                   
02094  4514-EXIT.                                                       
02095      EXIT.                                                        
02096  EJECT                                                            
02097  4515-LOAD-GROSS-PRM.                                             
02098      COMPUTE  ROUND-UP-DOLLARS  ROUNDED  =                        
02099               PT-GROSS-PRM (P-SUB1  P-SUB2)  *  1.                
02100                                                                   
02101      MOVE  ROUND-UP-DOLLARS      TO  DL-AMT2 (P-SUB2).            
02102                                                                   
02103  4519-EXIT.                                                       
02104      EXIT.                                                        
02105                                                                   
02106  4520-LOAD-CNC-PRM.                                               
02107      COMPUTE  PT-CNC-PRM (P-SUB1 P-SUB2)  =                       
02108               PT-CNC-PRM (P-SUB1 P-SUB2)  *  -1.                  
02109                                                                   
02110      COMPUTE  ROUND-UP-DOLLARS  ROUNDED  =                        
02111               PT-CNC-PRM (P-SUB1  P-SUB2)  *  1.                  
02112                                                                   
02113      MOVE  ROUND-UP-DOLLARS      TO  DL-AMT2 (P-SUB2).            
02114                                                                   
02115  4524-EXIT.                                                       
02116      EXIT.                                                        
02117                                                                   
02118  4525-LOAD-NET-PRM.                                               
02119      COMPUTE  ROUND-UP-DOLLARS  ROUNDED  =                        
02120               PT-NET-PRM (P-SUB1  P-SUB2)  *  1.                  
02121                                                                   
02122      MOVE  ROUND-UP-DOLLARS      TO  DL-AMT2 (P-SUB2).            
02123                                                                   
02124  4529-EXIT.                                                       
02125      EXIT.                                                        
02126                                                                   
02127  4530-LOAD-EARNED-PRM.                                            
02128      COMPUTE  ROUND-UP-DOLLARS  ROUNDED  =                        
02129               PT-EARNED-PRM (P-SUB1  P-SUB2)  *  1.               
02130                                                                   
02131      MOVE  ROUND-UP-DOLLARS      TO  DL-AMT2 (P-SUB2).            
02132                                                                   
02133  4534-EXIT.                                                       
02134      EXIT.                                                        
02135                                                                   
02136  4535-LOAD-CLAIMS.                                                
02137      COMPUTE  ROUND-UP-DOLLARS  ROUNDED  =                        
02138               PT-CLAIMS (P-SUB1  P-SUB2)  *  1.                   
02139                                                                   
02140      MOVE  ROUND-UP-DOLLARS      TO  DL-AMT2 (P-SUB2).            
02141                                                                   
02142  4539-EXIT.                                                       
02143      EXIT.                                                        
02144                                                                   
02145  4540-LOAD-EL-RATIO.                                              
02146      COMPUTE  PT-EL-RATIO (P-SUB1  P-SUB2)  =                     
02147               PT-EL-RATIO (P-SUB1  P-SUB2)  *  100.               
02148                                                                   
02149      MOVE  PT-EL-RATIO (P-SUB1  P-SUB2)                           
02150                                  TO  DL-AMT (P-SUB2).             
02151                                                                   
02152  4544-EXIT.                                                       
02153      EXIT.                                                        
02154  EJECT                                                            
02155  4545-LOAD-PRCNT-PRM.                                             
02156      COMPUTE  PT-PRCNT-PRM (P-SUB1  P-SUB2)  =                    
02157               PT-PRCNT-PRM (P-SUB1  P-SUB2)  *  100.              
02158                                                                   
02159      MOVE  PT-PRCNT-PRM (P-SUB1  P-SUB2)                          
02160                                  TO  DL-AMT (P-SUB2).             
02161                                                                   
02162  4549-EXIT.                                                       
02163      EXIT.                                                        
02164                                                                   
02165  4550-LOAD-PRCNT-CLAIM.                                           
02166      COMPUTE  PT-PRCNT-CLAIM (P-SUB1  P-SUB2)  =                  
02167               PT-PRCNT-CLAIM (P-SUB1  P-SUB2)  *  100.            
02168                                                                   
02169      MOVE  PT-PRCNT-CLAIM (P-SUB1  P-SUB2)                        
02170                                  TO  DL-AMT (P-SUB2).             
02171                                                                   
02172  4554-EXIT.                                                       
02173      EXIT.                                                        
02174                                                                   
02175  4555-LOAD-PRCNT-CNC-G.                                           
02176      COMPUTE  PT-PRCNT-CNC-G (P-SUB1  P-SUB2)  =                  
02177               PT-PRCNT-CNC-G (P-SUB1  P-SUB2)  *  100.            
02178                                                                   
02179      MOVE  PT-PRCNT-CNC-G (P-SUB1  P-SUB2)                        
02180                                  TO  DL-AMT (P-SUB2).             
02181                                                                   
02182  4559-EXIT.                                                       
02183      EXIT.                                                        
02184                                                                   
02185  4560-LOAD-AVG-LOAN.                                              
02186      COMPUTE  ROUND-UP-DOLLARS ROUNDED  =                         
02187               PT-AVG-LOAN (P-SUB1  P-SUB2)  *  1.                 
02188                                                                   
02189      MOVE  ROUND-UP-DOLLARS      TO  DL-AMT2 (P-SUB2).            
02190                                                                   
02191  4564-EXIT.                                                       
02192      EXIT.                                                        
02193                                                                   
02194  4565-LOAD-AVG-CLAIM.                                             
02195      COMPUTE  ROUND-UP-DOLLARS  ROUNDED  =                        
02196               PT-AVG-CLAIM (P-SUB1  P-SUB2)  *  1.                
02197                                                                   
02198      MOVE  ROUND-UP-DOLLARS      TO  DL-AMT2 (P-SUB2).            
02199                                                                   
02200  4569-EXIT.                                                       
02201      EXIT.                                                        
02202  EJECT                                                            
02203  4570-LOAD-AVG-DAYS.                                              
02204      MOVE  PT-AVG-DAYS (P-SUB1  P-SUB2)                           
02205                                  TO  DL-AMT1 (P-SUB2).            
02206                                                                   
02207  4574-EXIT.                                                       
02208      EXIT.                                                        
02209                                                                   
02210  4599-EXIT.                                                       
02211      EXIT.                                                        
02212  EJECT                                                            
02213  5000-PRODUCE-D-REPORT.                                           
02214      PERFORM 9400-READ-REIN-FILE THRU 9499-EXIT.                  
02215                                                                   
02216      MOVE SWR2-REIN-COMP  TO CURR-REIN-COMP.                      
02217      MOVE SWR2-CARR       TO CURR-CARR.                           
02218                                                                   
02219      IF FIRST-TIME                                                
02220          MOVE CURR-REIN-COMP   TO PREV-REIN-COMP                  
02221          MOVE CURR-CARR        TO PREV-CARR                       
02222          MOVE SWR2-REINS-TABLE TO PREV-REINS-TABLE                
02223          MOVE 'N'              TO FIRST-TIME-SW.                  
02224                                                                   
02225      IF PREV-REIN-COMP NOT = CURR-REIN-COMP                       
02226          PERFORM 5300-GET-REINS-NAME THRU 5399-EXIT               
02227          PERFORM 3300-REPORT-BREAK   THRU 3399-EXIT               
02228          PERFORM 3500-ST-BREAK       THRU 3599-EXIT               
02229      ELSE                                                         
02230      IF PREV-CARR NOT = CURR-CARR                                 
02231          PERFORM 5300-GET-REINS-NAME THRU 5399-EXIT               
02232          PERFORM 3300-REPORT-BREAK   THRU 3399-EXIT.              
02233                                                                   
02234      IF END-OF-EXTR                                               
02235          GO TO 5099-EXIT.                                         
02236                                                                   
02237      PERFORM 5100-ACCUMULATE-FROM-REIN THRU 5199-EXIT.            
02238                                                                   
02239      MOVE CURR-REIN-COMP   TO PREV-REIN-COMP.                     
02240      MOVE CURR-CARR        TO PREV-CARR.                          
02241      MOVE SWR2-REINS-TABLE TO PREV-REINS-TABLE.                   
02242                                                                   
02243  5099-EXIT.                                                       
02244       EXIT.                                                       
02245  EJECT                                                            
02246                                                                   
02247  5100-ACCUMULATE-FROM-REIN.                                       
02248      IF SWR2-AGE GREATER +60                                      
02249          MOVE  +7                TO  A-SUB2.                      
02250                                                                   
02251      IF SWR2-AGE LESS +61                                         
02252          MOVE  +6                TO  A-SUB2.                      
02253                                                                   
02254      IF SWR2-AGE LESS +56                                         
02255          MOVE  +5                TO  A-SUB2.                      
02256                                                                   
02257      IF SWR2-AGE LESS +51                                         
02258          MOVE  +4                TO  A-SUB2.                      
02259                                                                   
02260      IF SWR2-AGE LESS +46                                         
02261          MOVE  +3                TO  A-SUB2.                      
02262                                                                   
02263      IF SWR2-AGE LESS +41                                         
02264          MOVE  +2                TO  A-SUB2.                      
02265                                                                   
02266      IF SWR2-AGE LESS +31                                         
02267          MOVE  +1                TO  A-SUB2.                      
02268                                                                   
02269  EJECT                                                            
02270  5110-ACCUMULATE-LIFE.                                            
02271      IF SWR2-LFTYP = ZERO                                         
02272          GO TO 5150-ACCUMULATE-AH.                                
02273                                                                   
02274      IF DTE-PRC-OPT = 5                                           
02275          IF SWR2-LF-CURRENT-STATUS = '1' OR '2' OR '3' OR         
122002                                     '4' OR '5' OR '9' OR 'M'     
02277          NEXT SENTENCE                                            
02278         ELSE                                                      
02279          GO TO 5150-ACCUMULATE-AH.                                
02280                                                                   
02281      MOVE  SWR2-LFTYP            TO  CLAS-LOOK.                   
02282                                                                   
02283      PERFORM  0900-FIND-LIFE-TYPE  THRU  0950-EXIT.               
02284                                                                   
02285      IF CLAS-I-JOINT (CLAS-INDEXL) = 'J'                          
02286          MOVE  +2                TO  A-SUB1                       
02287      ELSE                                                         
02288          MOVE  +1                TO  A-SUB1.                      
02289                                                                   
02290      MOVE  AT-AMOUNTS (A-SUB1  A-SUB2)                            
02291                                  TO  WORK-AREA-FOR-ACCUMULATOR.   
02292                                                                   
122002     IF SWR2-ENTRY-STATUS = '5' OR 'M'                            
02294          MOVE  +0                TO  SWR2-LFAMT                   
02295                                      SWR2-LFPRM.                  
02296                                                                   
02297      ADD  SWR2-LFAMT             TO  WA-FACE-BEN.                 
02298      ADD  SWR2-LFPRM             TO  WA-ISS-PRM.                  
02299      ADD  SWR2-LFRFND            TO  WA-CNC-PRM.                  
02300      ADD  SWR2-DTHAMT            TO  WA-CLAIM-AMT.                
02301      ADD  +1                     TO  WA-CERT-COUNT.               
02302      ADD  SWR2-NUM-DTH-CLM       TO  WA-CLAIM-COUNT.              
02303                                                                   
02304      MOVE  ZERO                  TO  WA-NUMBER-DAYS.              
02305                                                                   
02306      IF SWR2-DTHAMT NOT = ZERO                                    
02307          ADD  +1                 TO  WA-CRT-W-CLM.                
02308                                                                   
02309  5115-ADD-LIFE-EARN.                                              
02310      MOVE ZEROS           TO  EARNED-PREMIUM.                     
02311      MOVE SWR2-LF-EPR-78  TO  LF-EPR-R78.                         
02312      MOVE SWR2-LF-EPR-PR  TO  LF-EPR-PRO.                         
02313                                                                   
02314      IF DTE-CLIENT = 'FIM'                                        
02315          PERFORM 5500-CHECK-PCT  THRU  5599-EXIT.                 
02316                                                                   
02317      IF DTE-CLIENT = 'FFL'                                        
02318          MOVE +.005 TO FAC-1.                                     
02319                                                                   
02320      IF DTE-CLIENT = 'FIM'                                        
02321          MOVE +.015 TO FAC-1.                                     
02322                                                                   
02323      MOVE FAC-1 TO WS-FAC.                                        
02324                                                                   
02325      IF DTE-CLIENT = 'FIM'                                        
02326          MOVE FAC-3          TO WS-FAC                            
02327          MOVE LF-PRO         TO WS-PRO                            
02328          MOVE LF-R78         TO WS-R78.                           
02329                                                                   
02330      IF DTE-CLIENT = 'FIM'                                        
02331          IF WS-PRO = ZERO  AND  WS-R78 = ZERO                     
02332              IF CLAS-I-EP  (CLAS-INDEXL) = 'P'  OR                
02333                 STATE-ABBR (CLAS-INDEXS) = 'WY'                   
02334                  MOVE +1.00      TO WS-PRO                        
02335              ELSE                                                 
02336                  MOVE +1.00      TO WS-R78.                       
02337                                                                   
02338      IF CLAS-I-EP (CLAS-INDEXL) = 'U'                             
02339          MOVE 'R' TO CLAS-I-EP (CLAS-INDEXL).                     
02340                                                                   
02341      IF DTE-CLIENT = 'FIA'  AND                                   
02342         SWR2-CARR = 'A' OR 'B'                                    
02343          COMPUTE EARNED-PREMIUM = (LF-EPR-PRO + LF-EPR-R78) / +2  
02344          GO TO 5115-CONT.                                         
02345                                                                   
02346      IF DTE-CLIENT = 'FIM'                                        
02347          COMPUTE EARNED-PREMIUM =                                 
02348               (LF-EPR-PRO * WS-PRO) + (LF-EPR-R78 * WS-R78)       
02349          GO TO 5115-CONT.                                         
02350                                                                   
02351      IF DTE-CLIENT = 'GIC'                                        
02352          COMPUTE EARNED-PREMIUM =                                 
02353                   (LF-EPR-PRO * +.80) + (LF-EPR-R78 * +.20)       
02354          GO TO 5115-CONT.                                         
02355                                                                   
02356      IF CLAS-I-EP (CLAS-INDEXL) = '1'                             
02357          COMPUTE EARNED-PREMIUM =                                 
02358                  (LF-EPR-PRO * +.6667) + (LF-EPR-R78 * +.3333)    
02359          GO TO 5115-CONT.                                         
02360                                                                   
02361      IF CLAS-I-EP (CLAS-INDEXL) = 'M'                             
02362          COMPUTE EARNED-PREMIUM =                                 
02363                  (LF-EPR-PRO + LF-EPR-R78) / +2                   
02364          GO TO 5115-CONT.                                         
02365                                                                   
02366      IF CLAS-I-EP  (CLAS-INDEXL) = 'P'   OR                       
02367         STATE-ABBR (CLAS-INDEXS) = 'WY'  OR                       
02368         DTE-CLIENT = 'SLC'                                        
02369           MOVE LF-EPR-PRO TO EARNED-PREMIUM                       
02370           GO TO 5115-CONT.                                        
02371                                                                   
02372      IF CLAS-I-EP (CLAS-INDEXL) = 'R' OR 'T' OR 'N' OR 'A' OR 'B' 
02373          MOVE LF-EPR-R78 TO EARNED-PREMIUM.                       
02374                                                                   
02375  5115-CONT.                                                       
02376      ADD EARNED-PREMIUM          TO WA-EARNED-PRM.                
02377                                                                   
02378      MOVE WORK-AREA-FOR-ACCUMULATOR                               
02379                                  TO AT-AMOUNTS (A-SUB1  A-SUB2).  
02380  EJECT                                                            
02381  5150-ACCUMULATE-AH.                                              
02382      IF SWR2-AHTYP = ZERO                                         
02383          GO TO 5199-EXIT.                                         
02384                                                                   
02385      IF DTE-PRC-OPT = 5                                           
02386          IF SWR2-AH-CURRENT-STATUS = '1' OR '2' OR '3' OR         
02387                                      '4' OR '5' OR '9' OR 'M'     
02388            NEXT SENTENCE                                          
02389          ELSE                                                     
02390            GO TO 5199-EXIT.                                       
02391                                                                   
02392      MOVE  +3                    TO  A-SUB1.                      
02393      MOVE  AT-AMOUNTS (A-SUB1  A-SUB2)                            
02394                                  TO  WORK-AREA-FOR-ACCUMULATOR.   
02395                                                                   
122002     IF SWR2-ENTRY-STATUS = '5' OR 'M'                            
02397          MOVE +0                 TO  SWR2-AHAMT                   
02398                                      SWR2-AHPRM.                  
02399                                                                   
02400      COMPUTE AH-TOTAL-AMT = SWR2-AHAMT * SWR2-AH-TERM.            
02401                                                                   
02402      ADD  AH-TOTAL-AMT           TO  WA-FACE-BEN.                 
02403      ADD  SWR2-AHPRM             TO  WA-ISS-PRM.                  
02404      ADD  SWR2-AHRFND            TO  WA-CNC-PRM.                  
02405      ADD  SWR2-DISAMT            TO  WA-CLAIM-AMT.                
02406      ADD  +1                     TO  WA-CERT-COUNT.               
02407      ADD  SWR2-NUM-DIS-CLM       TO  WA-CLAIM-COUNT.              
02408                                                                   
02409      IF SWR2-DISAMT NOT = ZEROS AND                               
02410         SWR2-AHAMT  NOT = ZEROS                                   
02411          COMPUTE PER-DAY-BENEFIT   ROUNDED = SWR2-AHAMT / 30      
02412          COMPUTE NO-DAYS-THIS-CERT ROUNDED = SWR2-DISAMT          
02413                                              / PER-DAY-BENEFIT    
02414          ADD  +1                 TO  WA-CRT-W-CLM                 
02415          ADD  NO-DAYS-THIS-CERT  TO  WA-NUMBER-DAYS.              
02416                                                                   
02417  5155-ADD-AH-EARN.                                                
02418      MOVE ZEROS           TO  EARNED-PREMIUM.                     
02419      MOVE SWR2-AH-EPR-78  TO  AH-EPR-R78.                         
02420      MOVE SWR2-AH-EPR-PR  TO  AH-EPR-PRO.                         
02421                                                                   
02422      IF DTE-CLIENT = 'FIM'                                        
02423          PERFORM 5500-CHECK-PCT  THRU  5599-EXIT.                 
02424                                                                   
02425      IF DTE-CLIENT = 'FFL'                                        
02426          MOVE ZEROS TO FAC-2.                                     
02427                                                                   
02428      IF DTE-CLIENT = 'FIM'                                        
02429          MOVE +.025 TO FAC-2.                                     
02430                                                                   
02431      MOVE FAC-2 TO WS-FAC.                                        
02432                                                                   
02433      IF DTE-CLIENT = 'FIM'                                        
02434          MOVE FAC-4          TO WS-FAC                            
02435          MOVE AH-PRO         TO WS-PRO                            
02436          MOVE AH-R78         TO WS-R78.                           
02437                                                                   
02438      IF DTE-CLIENT = 'FIM'                                        
02439          IF WS-PRO = ZERO  AND  WS-R78 = ZERO                     
02440             IF CLAS-I-EP  (CLAS-INDEXA) = 'P'  OR                 
02441                STATE-ABBR (CLAS-INDEXS) = 'WY'                    
02442                  MOVE +1.00      TO WS-PRO                        
02443              ELSE                                                 
02444                  MOVE +1.00      TO WS-R78.                       
02445                                                                   
02446      IF DTE-CLIENT = 'FIA'  AND                                   
02447         SWR2-CARR = 'A' OR 'B'                                    
02448           COMPUTE EARNED-PREMIUM = (AH-EPR-PRO + AH-EPR-R78) / +2 
02449           GO TO 5155-CONT.                                        
02450                                                                   
02451      IF DTE-CLIENT = 'FIM'                                        
02452          COMPUTE EARNED-PREMIUM =                                 
02453               (AH-EPR-PRO * WS-PRO) + (AH-EPR-R78 * WS-R78)       
02454          GO TO 5155-CONT.                                         
02455                                                                   
02456      IF DTE-CLIENT = 'GIC'                                        
02457          IF CLAS-I-BAL (CLAS-INDEXA) NOT = 'B'                    
02458              COMPUTE EARNED-PREMIUM =                             
02459                   (AH-EPR-PRO * +.80) + (AH-EPR-R78 * +.20)       
02460              GO TO 5155-CONT.                                     
02461                                                                   
02462      IF CLAS-I-EP (CLAS-INDEXA) = '1'  OR                         
02463         (DTE-CLIENT = 'ITY' AND (SWR2-CARR = '5' OR '7'))         
02464              COMPUTE EARNED-PREMIUM =                             
02465             (AH-EPR-PRO * +.6667) + (AH-EPR-R78 * +.3333)         
02466              GO TO 5155-CONT.                                     
02467                                                                   
02468      IF CLAS-I-EP  (CLAS-INDEXA) = 'M'                            
02469           COMPUTE EARNED-PREMIUM =                                
02470                   (AH-EPR-PRO + AH-EPR-R78) / +2                  
02471           GO TO 5155-CONT.                                        
02472                                                                   
02473      IF CLAS-I-EP  (CLAS-INDEXA) = 'P'   OR                       
02474         STATE-ABBR (CLAS-INDEXS) = 'WY'  OR                       
02475         DTE-CLIENT = 'SLC'               OR                       
02476        (DTE-CLIENT = 'ITY' AND SWR2-CARR = '2')                   
02477           MOVE AH-EPR-PRO TO EARNED-PREMIUM                       
02478           GO TO 5155-CONT.                                        
02479                                                                   
02480      IF CLAS-I-EP (CLAS-INDEXA) = 'R'  OR  'A'                    
02481         IF (DTE-CLIENT = 'POS' AND SWR2-CARR = '2')   OR          
02482            (DTE-CLIENT = 'FIM')                                   
02483               COMPUTE EARNED-PREMIUM =                            
02484                   ((AH-EPR-PRO + AH-EPR-R78) / +2)                
02485           ELSE                                                    
02486              MOVE AH-EPR-R78 TO EARNED-PREMIUM.                   
02487                                                                   
02488  5155-CONT.                                                       
02489      ADD  EARNED-PREMIUM         TO  WA-EARNED-PRM.               
02490                                                                   
02491      MOVE  WORK-AREA-FOR-ACCUMULATOR                              
02492                              TO  AT-AMOUNTS (A-SUB1  A-SUB2).     
02493                                                                   
02494  5199-EXIT.                                                       
02495      EXIT.                                                        
02496  EJECT                                                            
02497                                                                   
02498  5300-GET-REINS-NAME.                                             
02499       MOVE 0 TO CO-SUB.                                           
02500                                                                   
02501  5300-GET-NAME-LOOP.                                              
02502       ADD +1 TO CO-SUB.                                           
02503       IF PREV-REIN-COMP = RC2-CO-PRIME (CO-SUB)                   
02504           MOVE RC2-CO-NAME (CO-SUB) TO SAVE-REIN-NAME             
02505           GO TO 5399-EXIT                                         
02506       ELSE                                                        
02507       IF RC2-CO-PRIME (CO-SUB) = SPACES                           
02508           MOVE SPACES TO SAVE-REIN-NAME                           
02509           GO TO 5399-EXIT                                         
02510       ELSE                                                        
02511           GO TO 5300-GET-NAME-LOOP.                               
02512                                                                   
02513  5399-EXIT.                                                       
02514      EXIT.                                                        
02515  EJECT                                                            
02516  5500-CHECK-PCT.                                                  
02517      IF FIRST-READ                                                
02518          MOVE ZEROS            TO CO-SUB                          
02519                                   LF-PRO LF-R78 AH-PRO AH-R78     
02520          MOVE 'N'              TO FIRST-READ-SW                   
02521          GO TO 5500-LOOP.                                         
02522                                                                   
02523      IF SWR2-REIN-COMP = PREV-REIN-COMP                           
02524          GO TO 5599-EXIT.                                         
02525                                                                   
02526  5500-LOOP.                                                       
02527       ADD +1 TO CO-SUB.                                           
02528       IF SWR2-REIN-COMP = RC2-CO-PRIME (CO-SUB)                   
02529           MOVE RC2-LF-R78-PCT (CO-SUB) TO LF-R78                  
02530           MOVE RC2-LF-PRO-PCT (CO-SUB) TO LF-PRO                  
02531           MOVE RC2-AH-R78-PCT (CO-SUB) TO AH-R78                  
02532           MOVE RC2-AH-PRO-PCT (CO-SUB) TO AH-PRO                  
02533           GO TO 5599-EXIT                                         
02534       ELSE                                                        
02535       IF RC2-CO-PRIME (CO-SUB) = SPACES                           
02536           GO TO 5599-EXIT                                         
02537       ELSE                                                        
02538           GO TO 5500-LOOP.                                        
02539                                                                   
02540  5599-EXIT.                                                       
02541      EXIT.                                                        
02542  EJECT                                                            
02543  6500-CLEAR-ACCUMULATOR-TABLE.                                    
02544      PERFORM  6530-CLEAR-LOOP  THRU  6539-EXIT                    
02545          VARYING  A-SUB2  FROM  1  BY  1                          
02546            UNTIL  A-SUB2  GREATER  7.                             
02547                                                                   
02548      IF ACCOUNT-BREAK                                             
02549          MOVE AT-AMOUNTS-L-D (1) TO  AT-AMOUNTS-L-D (2)           
02550                                      AT-AMOUNTS-L-D (3)           
02551      ELSE                                                         
02552      IF STATE-BREAK                                               
02553          MOVE AT2-AMOUNTS-L-D (1) TO  AT2-AMOUNTS-L-D (2)         
02554                                       AT2-AMOUNTS-L-D (3)         
02555      ELSE                                                         
02556      IF GROUP-BREAK                                               
02557          MOVE AT3-AMOUNTS-L-D (1) TO  AT3-AMOUNTS-L-D (2)         
02558                                       AT3-AMOUNTS-L-D (3)         
02559      ELSE                                                         
02560      IF CARRIER-BREAK                                             
02561          MOVE AT4-AMOUNTS-L-D (1) TO  AT4-AMOUNTS-L-D (2)         
02562                                       AT4-AMOUNTS-L-D (3)         
02563      ELSE                                                         
02564          MOVE AT5-AMOUNTS-L-D (1) TO  AT5-AMOUNTS-L-D (2)         
02565                                       AT5-AMOUNTS-L-D (3).        
02566                                                                   
02567      GO TO 6599-EXIT.                                             
02568                                                                   
02569  6530-CLEAR-LOOP.                                                 
02570      IF ACCOUNT-BREAK                                             
02571          MOVE +0                 TO  AT-FACE-BEN    (1 A-SUB2)    
02572                                      AT-ISS-PRM     (1 A-SUB2)    
02573                                      AT-CNC-PRM     (1 A-SUB2)    
02574                                      AT-EARNED-PRM  (1 A-SUB2)    
02575                                      AT-CLAIM-AMT   (1 A-SUB2)    
02576                                      AT-CERT-COUNT  (1 A-SUB2)    
02577                                      AT-CLAIM-COUNT (1 A-SUB2)    
02578                                      AT-NUMBER-DAYS (1 A-SUB2)    
02579                                      AT-CRT-W-CLM   (1 A-SUB2)    
02580      ELSE                                                         
02581      IF STATE-BREAK                                               
02582          MOVE +0                 TO  AT2-FACE-BEN    (1 A-SUB2)   
02583                                      AT2-ISS-PRM     (1 A-SUB2)   
02584                                      AT2-CNC-PRM     (1 A-SUB2)   
02585                                      AT2-EARNED-PRM  (1 A-SUB2)   
02586                                      AT2-CLAIM-AMT   (1 A-SUB2)   
02587                                      AT2-CERT-COUNT  (1 A-SUB2)   
02588                                      AT2-CLAIM-COUNT (1 A-SUB2)   
02589                                      AT2-NUMBER-DAYS (1 A-SUB2)   
02590                                      AT2-CRT-W-CLM   (1 A-SUB2)   
02591      ELSE                                                         
02592      IF GROUP-BREAK                                               
02593          MOVE +0                 TO  AT3-FACE-BEN    (1 A-SUB2)   
02594                                      AT3-ISS-PRM     (1 A-SUB2)   
02595                                      AT3-CNC-PRM     (1 A-SUB2)   
02596                                      AT3-EARNED-PRM  (1 A-SUB2)   
02597                                      AT3-CLAIM-AMT   (1 A-SUB2)   
02598                                      AT3-CERT-COUNT  (1 A-SUB2)   
02599                                      AT3-CLAIM-COUNT (1 A-SUB2)   
02600                                      AT3-NUMBER-DAYS (1 A-SUB2)   
02601                                      AT3-CRT-W-CLM   (1 A-SUB2)   
02602      ELSE                                                         
02603      IF CARRIER-BREAK                                             
02604          MOVE +0                 TO  AT4-FACE-BEN    (1 A-SUB2)   
02605                                      AT4-ISS-PRM     (1 A-SUB2)   
02606                                      AT4-CNC-PRM     (1 A-SUB2)   
02607                                      AT4-EARNED-PRM  (1 A-SUB2)   
02608                                      AT4-CLAIM-AMT   (1 A-SUB2)   
02609                                      AT4-CERT-COUNT  (1 A-SUB2)   
02610                                      AT4-CLAIM-COUNT (1 A-SUB2)   
02611                                      AT4-NUMBER-DAYS (1 A-SUB2)   
02612                                      AT4-CRT-W-CLM   (1 A-SUB2)   
02613      ELSE                                                         
02614          MOVE +0                 TO  AT5-FACE-BEN    (1 A-SUB2)   
02615                                      AT5-ISS-PRM     (1 A-SUB2)   
02616                                      AT5-CNC-PRM     (1 A-SUB2)   
02617                                      AT5-EARNED-PRM  (1 A-SUB2)   
02618                                      AT5-CLAIM-AMT   (1 A-SUB2)   
02619                                      AT5-CERT-COUNT  (1 A-SUB2)   
02620                                      AT5-CLAIM-COUNT (1 A-SUB2)   
02621                                      AT5-NUMBER-DAYS (1 A-SUB2)   
02622                                      AT5-CRT-W-CLM   (1 A-SUB2).  
02623                                                                   
02624  6539-EXIT.                                                       
02625      EXIT.                                                        
02626                                                                   
02627  6599-EXIT.                                                       
02628      EXIT.                                                        
02629  EJECT                                                            
02630  6700-CLEAR-PRINT-TABLE.                                          
02631      PERFORM  6730-CLEAR-LOOP  THRU  6739-EXIT                    
02632          VARYING  P-SUB2  FROM  1  BY  1                          
02633            UNTIL  P-SUB2  GREATER  8.                             
02634                                                                   
02635      MOVE  PT-AMOUNTS-L-D (1)    TO  PT-AMOUNTS-L-D (2).          
02636      MOVE  PT-AMOUNTS-L-D (1)    TO  PT-AMOUNTS-L-D (3).          
02637      MOVE  PT-AMOUNTS-L-D (1)    TO  PT-AMOUNTS-L-D (4).          
02638                                                                   
02639      GO TO 6799-EXIT.                                             
02640                                                                   
02641  6730-CLEAR-LOOP.                                                 
02642      MOVE  +0                    TO  PT-FACE-BEN    (1 P-SUB2)    
02643                                      PT-CERT-COUNT  (1 P-SUB2)    
02644                                      PT-CLAIM-COUNT (1 P-SUB2)    
02645                                      PT-NUMBER-DAYS (1 P-SUB2)    
02646                                      PT-GROSS-PRM   (1 P-SUB2)    
02647                                      PT-CNC-PRM     (1 P-SUB2)    
02648                                      PT-NET-PRM     (1 P-SUB2)    
02649                                      PT-EARNED-PRM  (1 P-SUB2)    
02650                                      PT-CLAIMS      (1 P-SUB2)    
02651                                      PT-EL-RATIO    (1 P-SUB2)    
02652                                      PT-PRCNT-PRM   (1 P-SUB2)    
02653                                      PT-PRCNT-CLAIM (1 P-SUB2)    
02654                                      PT-PRCNT-CNC-G (1 P-SUB2)    
02655                                      PT-AVG-LOAN    (1 P-SUB2)    
02656                                      PT-AVG-CLAIM   (1 P-SUB2)    
02657                                      PT-AVG-DAYS    (1 P-SUB2)    
02658                                      PT-CRT-W-CLM   (1 P-SUB2).   
02659                                                                   
02660  6739-EXIT.                                                       
02661      EXIT.                                                        
02662                                                                   
02663  6799-EXIT.                                                       
02664      EXIT.                                                        
02665                                                                   
02666  EJECT                                                            
02667  7000-HEADING-ROUTINE.                                            
02668      MOVE  WK-RPT-CODE           TO  H1-RPT-CODE.                 
02669      MOVE  '1'                   TO  X.                           
02670      MOVE  HEADING-1             TO  P-DATA.                      
02671      PERFORM  7700-PRINT-RTN  THRU  7799-EXIT.                    
02672                                                                   
02673      MOVE  ' '                   TO  X.                           
02674      MOVE  HEADING-2             TO  P-DATA.                      
02675      PERFORM  7700-PRINT-RTN  THRU  7799-EXIT.                    
02676                                                                   
02677      MOVE  HEADING-3             TO  P-DATA.                      
02678      PERFORM  7700-PRINT-RTN  THRU  7799-EXIT.                    
02679                                                                   
02680      MOVE  SPACES                TO  P-DATA.                      
02681      PERFORM  7700-PRINT-RTN  THRU  7799-EXIT.                    
02682                                                                   
02683      IF WK-RPT-CODE NOT = 'A'                                     
02684          GO TO 7020-CHECK-REPORT-B.                               
02685                                                                   
02686      IF WK-BREAK-TYPE = '5'                                       
02687          MOVE FINAL-HEADING  TO P-DATA                            
02688          PERFORM 7700-PRINT-RTN THRU 7799-EXIT                    
02689          MOVE SPACES       TO P-DATA                              
02690          PERFORM 7700-PRINT-RTN THRU 7799-EXIT 3 TIMES            
02691          GO TO 7099-EXIT.                                         
02692                                                                   
02693      MOVE PREV-CARR        TO CH-CARR-NO                          
02694                               CARRIER-L.                          
02695      PERFORM 7800-LOAD-CARRIER-NAME THRU 7899-EXIT.               
02696                                                                   
02697      MOVE CARRIER-HEADING  TO P-DATA.                             
02698      PERFORM 7700-PRINT-RTN THRU 7799-EXIT.                       
02699                                                                   
02700      IF WK-BREAK-TYPE = '4'                                       
02701          MOVE SPACES TO P-DATA                                    
02702          PERFORM 7700-PRINT-RTN THRU 7799-EXIT 3 TIMES            
02703      ELSE                                                         
02704      IF WK-BREAK-TYPE = '3'                                       
02705          MOVE PREV-GRP      TO GH-GROUP-NO                        
02706          MOVE GROUP-HEADING TO P-DATA                             
02707          PERFORM 7700-PRINT-RTN THRU 7799-EXIT                    
02708          MOVE SPACES        TO P-DATA                             
02709          PERFORM 7700-PRINT-RTN THRU 7799-EXIT 2 TIMES            
02710      ELSE                                                         
02711      IF WK-BREAK-TYPE = '2'                                       
02712          MOVE PREV-GRP      TO GH-GROUP-NO                        
02713          MOVE GROUP-HEADING TO P-DATA                             
02714          PERFORM 7700-PRINT-RTN THRU 7799-EXIT                    
02715          MOVE PREV-STATE    TO SH-STATE-NO  STATE-L               
02716          PERFORM 7900-LOAD-STATE-NAME THRU 7999-EXIT              
02717          MOVE STATE-HEADING TO P-DATA                             
02718          PERFORM 7700-PRINT-RTN THRU 7799-EXIT                    
02719          MOVE SPACES        TO P-DATA                             
02720          PERFORM 7700-PRINT-RTN THRU 7799-EXIT                    
02721      ELSE                                                         
02722          MOVE PREV-GRP        TO GH-GROUP-NO                      
02723          MOVE GROUP-HEADING   TO P-DATA                           
02724          PERFORM 7700-PRINT-RTN THRU 7799-EXIT                    
02725          MOVE PREV-STATE      TO SH-STATE-NO  STATE-L             
02726          PERFORM 7900-LOAD-STATE-NAME THRU 7999-EXIT              
02727          MOVE STATE-HEADING   TO P-DATA                           
02728          PERFORM 7700-PRINT-RTN THRU 7799-EXIT                    
02729          MOVE PREV-ACCT       TO AH-ACCT-NO                       
02730          MOVE SAVE-ACCT-NAME  TO AH-ACCT-NAME                     
02731          MOVE ACCOUNT-HEADING TO P-DATA                           
02732          PERFORM 7700-PRINT-RTN THRU 7799-EXIT.                   
02733                                                                   
02734  7020-CHECK-REPORT-B.                                             
02735      IF WK-RPT-CODE  = 'B'                                        
02736          MOVE  CLAS-REPORT-CD1-CAPTION TO RC1H-CAPTION            
02737          MOVE  PREV-REPORT-CODE-1      TO RC1H-CODE               
02738          MOVE  REPORT-CODE-1-HEADING   TO  P-DATA                 
02739          PERFORM  7700-PRINT-RTN  THRU  7799-EXIT                 
02740          MOVE  SPACES                  TO  P-DATA                 
02741          PERFORM  7700-PRINT-RTN  THRU  7799-EXIT 3 TIMES.        
02742                                                                   
02743      IF WK-RPT-CODE   = 'C'                                       
02744          MOVE  PREV-CARR         TO  CH-CARR-NO  CARRIER-L        
02745          PERFORM  7800-LOAD-CARRIER-NAME  THRU  7899-EXIT         
02746          MOVE  CARRIER-HEADING   TO  P-DATA                       
02747          PERFORM  7700-PRINT-RTN  THRU  7799-EXIT                 
02748          MOVE PREV-GRP           TO GH-GROUP-NO                   
02749          MOVE GROUP-HEADING      TO P-DATA                        
02750          PERFORM 7700-PRINT-RTN THRU 7799-EXIT                    
02751          MOVE  CLAS-REPORT-CD2-CAPTION TO RC2H-CAPTION            
02752          MOVE  PREV-REPORT-CODE-2      TO RC2H-CODE               
02753          MOVE  REPORT-CODE-2-HEADING   TO  P-DATA                 
02754          PERFORM  7700-PRINT-RTN  THRU  7799-EXIT                 
02755          MOVE  SPACES            TO  P-DATA                       
02756          PERFORM  7700-PRINT-RTN  THRU  7799-EXIT.                
02757  EJECT                                                            
02758      IF WK-RPT-CODE   = 'D'                                       
02759          MOVE PREV-REIN-COMP      TO RH-COMP-NO                   
02760          MOVE SAVE-REIN-NAME      TO RH-COMP-NAME                 
02761          MOVE REINSURANCE-HEADING TO P-DATA                       
02762          PERFORM  7700-PRINT-RTN  THRU  7799-EXIT                 
02763        IF WK-BREAK-TYPE = '1'                                     
02764          MOVE  PREV-CARR         TO  CH-CARR-NO  CARRIER-L        
02765          PERFORM  7800-LOAD-CARRIER-NAME  THRU  7899-EXIT         
02766          MOVE  CARRIER-HEADING   TO  P-DATA                       
02767          PERFORM  7700-PRINT-RTN  THRU  7799-EXIT                 
02768          MOVE SPACES             TO P-DATA                        
02769          PERFORM  7700-PRINT-RTN  THRU  7799-EXIT 2 TIMES         
02770          IF WK-BREAK-TYPE = '2'                                   
02771            MOVE SPACES             TO P-DATA                      
02772            PERFORM  7700-PRINT-RTN  THRU  7799-EXIT 3 TIMES.      
02773                                                                   
02774  7099-EXIT.                                                       
02775      EXIT.                                                        
02776  EJECT                                                            
02777  7700-PRINT-RTN.                                                  
02778                              COPY ELCPRT2.                        
02779                                                                   
02780  7799-EXIT.                                                       
02781      EXIT.                                                        
02782  EJECT                                                            
02783  7800-LOAD-CARRIER-NAME.                                          
02784      MOVE  ALL  '$'              TO  CH-CARR-NA.                  
02785                                                                   
02786      IF CARRIER-SUB (1)  = SPACE                                  
02787          GO TO 7899-EXIT.                                         
02788                                                                   
02789      MOVE  +0                    TO  S1.                          
02790                                                                   
02791  7810-LOAD-CARRIER-LOOP.                                          
02792      ADD  +1                     TO  S1.                          
02793                                                                   
02794      IF S1 GREATER  CLAS-MAXCN                                    
02795          GO TO 7899-EXIT.                                         
02796                                                                   
02797      IF CARRIER-L  = CARRIER-SUB (S1)                             
02798          MOVE  CARRIER-PIC (S1)  TO  CH-CARR-NA                   
02799          GO TO 7899-EXIT.                                         
02800                                                                   
02801      GO TO 7810-LOAD-CARRIER-LOOP.                                
02802                                                                   
02803  7899-EXIT.                                                       
02804      EXIT.                                                        
02805                                                                   
02806  7900-LOAD-STATE-NAME.                                            
02807      MOVE  ALL  '$'              TO  SH-STATE-NAME.               
02808      MOVE  ZERO                  TO  CLAS-INDEXS.                 
02809                                                                   
02810  7920-LOAD-STATE-LOOP.                                            
02811      ADD  1                      TO  CLAS-INDEXS.                 
02812                                                                   
02813      IF CLAS-INDEXS  GREATER  CLAS-MAXS                           
02814          GO TO 7999-EXIT.                                         
02815                                                                   
02816      IF STATE-L  NOT = STATE-SUB (CLAS-INDEXS)                    
02817          GO TO 7920-LOAD-STATE-LOOP.                              
02818                                                                   
02819      MOVE  STATE-PIC (CLAS-INDEXS)  TO  SH-STATE-NAME.            
02820                                                                   
02821  7999-EXIT.                                                       
02822      EXIT.                                                        
02823  EJECT                                                            
02824  8000-RELEASE-SORT-WORK.                                          
02825      MOVE  SORT-WORK-REC         TO  SORT-REC.                    
02826                                                                   
02827      RELEASE  SORT-REC.                                           
02828                                                                   
02829  8099-EXIT.                                                       
02830      EXIT.                                                        
02831                                                                   
02832 ******************************************************************
02833 ***    E A R N E D   P R E M I U M   C A L C U L A T I O N S   ***
02834 ******************************************************************
02835                                                                   
02836  8200-EP-BUMP-RTN.                                                
02837      MOVE CR-LFAMT       TO WE-LFAMT.                             
02838      MOVE CR-LFPRM       TO WE-LFPRM.                             
02839                                                                   
02840      IF CR-LFTYP NOT = ZEROS                                      
02841          IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')         
02842              MOVE CR-LFAMT-ALT        TO WE-LFAMT-ALT             
02843              MOVE CR-LFPRM-ALT        TO WE-LFPRM-ALT             
02844          ELSE                                                     
02845              MOVE ZEROS               TO WE-LFAMT-ALT             
02846                                          WE-LFPRM-ALT.            
02847                                                                   
02848      MOVE CR-LFRFND      TO WE-LFRFND.                            
02849                                                                   
02850      MOVE CR-AHPRM       TO WE-AHPRM.                             
02851      MOVE CR-AHAMT       TO WE-AHAMT.                             
02852      MOVE CR-AHRFND      TO WE-AHRFND.                            
02853                                                                   
02854      IF CR-POLICY-IS-RESTORE  OR                                  
02855         CR-POLICY-IS-REISSUE                                      
122002        OR CR-POLICY-IS-MONTHLY
02856          MOVE ZEROS      TO WE-LFAMT  WE-LFAMT-ALT                
02857                             WE-LFPRM  WE-LFPRM-ALT                
02858                             WE-AHAMT  WE-AHPRM.                   
02859                                                                   
02860  8299-EXIT.                                                       
02861      EXIT.                                                        
02862                                                                   
02863  8350-EP-LF-REINSURE.                                             
02864      MOVE SWR2-LFPRM     TO WE-LFPRM.                             
02865      MOVE SWR2-LFAMT     TO WE-LFAMT.                             
02866      MOVE +0             TO WE-LFAMT-ALT                          
02867                             WE-LFPRM-ALT.                         
02868      MOVE SWR2-LFRFND    TO WE-LFRFND.                            
02869                                                                   
122002     IF SWR2-LF-CURRENT-STATUS = '3' OR '5' OR 'M'                
02871          MOVE ZEROS      TO WE-LFPRM                              
02872                             WE-AHPRM.                             
02873                                                                   
02874  8359-EXIT.                                                       
02875      EXIT.                                                        
02876                                                                   
02877  8360-EP-AH-REINSURE.                                             
02878      MOVE SWR2-AHPRM     TO WE-AHPRM.                             
02879      MOVE SWR2-AHAMT     TO WE-AHAMT                              
02880      MOVE SWR2-AHRFND    TO WE-AHRFND.                            
02881                                                                   
02882      IF SWR2-AH-CURRENT-STATUS = '3' OR '5' OR 'M'                
02883          MOVE ZEROS      TO WE-LFPRM                              
02884                             WE-AHPRM.                             
02885                                                                   
02886  8369-EXIT.                                                       
02887      EXIT.                                                        
02888  EJECT                                                            
02889 ******************************************************************
02890 ***  C A L C U L A T E   L I F E   E A R N E D   P R E M I U M ***
02891 ******************************************************************
02892                                                                   
02893  8400-EP-DO-LF.                                                   
02894      MOVE +0                         TO   LF-EPR-R78              
02895                                           LF-EPR-PRO              
02896                                           LF-EPR-ST.              
02897                                                                   
02898      MOVE LIFE-OVERRIDE-L1       TO WE-LF-AH.                     
02899                                                                   
02900      IF CR-LF-STATUS-AT-DEATH NOT = ' '                           
02901          COMPUTE LF-EPR-R78 = WE-LFPRM + WE-LFPRM-ALT             
02902          MOVE LF-EPR-R78 TO LF-EPR-PRO LF-EPR-ST.                 
02903                                                                   
122002     IF (CR-LF-STATUS-AT-CANCEL = ' ') AND
122002         (CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'Z')
02906              GO TO 8430-EP-DO-LF-EP.                              
02907                                                                   
02908      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'Z'                      
02909          GO TO 8430-EP-DO-LF-EP.                                  
02910                                                                   
02911      COMPUTE LF-EPR-R78 = (WE-LFPRM + WE-LFPRM-ALT) - WE-LFRFND.  
02912      MOVE LF-EPR-R78 TO LF-EPR-PRO LF-EPR-ST.                     
02913                                                                   
02914      GO TO 8499-EXIT.                                             
02915                                                                   
02916  8430-EP-DO-LF-EP.                                                
02917      MOVE SPACE                  TO STATUTORY-SWITCH.             
02918                                                                   
122002     IF (CR-LF-STATUS-AT-DEATH NOT = ' ')  AND
122002        (CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'Z')
02921          GO TO 8499-EXIT.                                         
02922                                                                   
02923      IF REM-TRM1 GREATER CR-LF-TERM                               
02924          MOVE +0 TO LF-EPR-R78 LF-EPR-PRO LF-EPR-ST               
02925          GO TO 8499-EXIT.                                         
02926                                                                   
02927      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'Z'                      
02928          GO TO 8480-EP-DO-LF-EP-SUM.                              
02929                                                                   
02930      IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')             
02931          IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'L'                  
02932              IF REM-TRM1 NOT GREATER +0                           
02933                  COMPUTE LF-EPR-R78 = WE-LFPRM + WE-LFPRM-ALT     
02934                  MOVE LF-EPR-R78 TO LF-EPR-PRO LF-EPR-ST          
02935                  GO TO 8499-EXIT                                  
02936              ELSE                                                 
02937                  NEXT SENTENCE                                    
02938          ELSE                                                     
02939              IF (REM-TRM1 + 1) NOT GREATER +0                     
02940                  COMPUTE LF-EPR-R78 = WE-LFPRM + WE-LFPRM-ALT     
02941                  MOVE LF-EPR-R78 TO LF-EPR-PRO LF-EPR-ST          
02942                  GO TO 8499-EXIT                                  
02943              ELSE                                                 
02944                  NEXT SENTENCE                                    
02945      ELSE                                                         
02946          IF REM-TRM1 NOT GREATER +0  OR                           
02947             CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'B'                  
02948              MOVE WE-LFPRM TO LF-EPR-R78 LF-EPR-PRO LF-EPR-ST     
02949              GO TO 8499-EXIT.                                     
02950                                                                   
02951      IF CLAS-I-EP (CLAS-INDEXL) = 'T'                             
02952          GO TO 8440-EP-DO-LF-EP-TEX.                              
02953                                                                   
02954      IF CLAS-I-EP (CLAS-INDEXL) = 'N'                             
02955          GO TO 8450-EP-DO-LF-EP-NP.                               
02956                                                                   
CIDMOD*    IF STATE-ABBR (CLAS-INDEXS) = 'OH'                           
CIDMOD     IF (STATE-ABBR (CLAS-INDEXS) = 'OH') AND                     
CIDMOD        (CR-RATING-CLASS NOT = 'L ')
02958          IF CR-APR     GREATER ZERO   AND                         
02959             CR-LF-TERM GREATER 060    AND                         
02960             CR-DT      GREATER 19831031                           
02961              MOVE '*'             TO STATUTORY-SWITCH             
02962              GO TO 8450-EP-DO-LF-EP-NP.                           
02963                                                                   
02964      IF STATE-ABBR (CLAS-INDEXS) = 'MT'                           
02965          IF CR-APR     GREATER ZERO  AND                          
02966             CR-LF-TERM GREATER 061   AND                          
02967             CR-DT      GREATER 19830318                           
02968              MOVE '*'             TO STATUTORY-SWITCH             
02969              GO TO 8450-EP-DO-LF-EP-NP.                           
02970                                                                   
02971      IF STATE-ABBR (CLAS-INDEXS) = 'UT'                           
02972          IF CR-APR     GREATER ZERO     AND                       
02973             CR-LF-TERM GREATER 062      AND                       
02974             CR-DT      GREATER 19810831 AND                       
02975             CR-DT      LESS    19830901                           
02976              MOVE '*'             TO STATUTORY-SWITCH             
02977              GO TO 8450-EP-DO-LF-EP-NP.                           
02978                                                                   
02979      IF STATE-ABBR (CLAS-INDEXS) = 'RI'                           
02980          IF CR-APR     GREATER ZERO  AND                          
02981             CR-LF-TERM GREATER 060   AND                          
02982             CR-DT      GREATER 19831231                           
02983              MOVE '*'             TO STATUTORY-SWITCH             
02984              GO TO 8450-EP-DO-LF-EP-NP.                           
02985                                                                   
02986      GO TO 8460-EP-DO-LF-EP-R78.                                  
02987                                                                   
02988  8440-EP-DO-LF-EP-TEX.                                            
02989      COMPUTE TEX-FACT-4 = (CR-LF-TERM  * CR-LF-TERM)              
02990                         + (CR-PMT-FREQ * CR-LF-TERM).             
02991                                                                   
02992      DIVIDE REM-TRM1 BY CR-PMT-FREQ                               
02993          GIVING TEX-FACT-5   REMAINDER TEX-FACT-6.                
02994                                                                   
02995      COMPUTE TEX-FACT-5 = TEX-FACT-5 * CR-PMT-FREQ.               
02996                                                                   
02997      COMPUTE TEX-FACT-7 = (TEX-FACT-5 * TEX-FACT-5) +             
02998                           (TEX-FACT-5 * CR-PMT-FREQ) +            
02999                           (2 * (TEX-FACT-6 *                      
03000                           (TEX-FACT-5 + CR-PMT-FREQ))).           
03001                                                                   
03002      COMPUTE TEX-FACT-8 ROUNDED = TEX-FACT-7 / TEX-FACT-4.        
03003                                                                   
03004      COMPUTE LF-EPR-R78 ROUNDED =                                 
03005                     WE-LFPRM - (WE-LFPRM * TEX-FACT-8).           
03006                                                                   
03007      GO TO 8460-EP-DO-LF-EP-R78.                                  
03008                                                                   
03009  8450-EP-DO-LF-EP-NP.                                             
03010      IF WE-LFPRM = ZERO                                           
03011          GO TO 8460-EP-DO-LF-EP-R78.                              
03012                                                                   
03013      MOVE CR-APR                 TO NP-APR.                       
03014      MOVE CR-LF-TERM             TO NP-ORIG  NP-CAP.              
03015      MOVE REM-TRM1               TO NP-REM.                       
03016                                                                   
03017      MOVE CLAS-I-CALC-TYPE (CLAS-INDEXL) TO NP-OPT.               
03018                                                                   
03019      IF NP-OPT = 'T' OR 'U' OR 'V'                                
03020          MOVE CR-LOAN-TERM       TO NP-ORIG.                      
03021                                                                   
03022      MOVE 'R'                    TO NP-OPT.                       
03023                                                                   
03024      CALL 'ECSNETRM'                                              
03025          USING NP-APR NP-ORIG NP-REM NP-OPT NP-CAP NP-FACTOR.     
03026                                                                   
03027      COMPUTE LF-EPR-R78 ROUNDED =                                 
03028                      WE-LFPRM - (NP-FACTOR * WE-LFPRM).           
03029                                                                   
03030      IF STATUTORY-REQUIREMENT                                     
03031          MOVE LF-EPR-R78             TO LF-EPR-ST.                
03032                                                                   
03033  8460-EP-DO-LF-EP-R78.                                            
03034      COMPUTE TEMP-1 = CR-LF-TERM * (CR-LF-TERM + 1).              
03035      COMPUTE TEMP-2 = REM-TRM1   * (REM-TRM1 + 1).                
03036                                                                   
03037      IF DTE-R78 = '1'                                             
03038          COMPUTE TEMP-2 = REM-TRM1 * REM-TRM1.                    
03039                                                                   
03040      COMPUTE TEMP-4 ROUNDED = TEMP-2 / TEMP-1.                    
03041                                                                   
03042      IF REM-TRM1 GREATER +0                                       
03043          COMPUTE TEMP-7 ROUNDED =                                 
03044              WE-LFPRM - (WE-LFPRM * TEMP-4)                       
03045      ELSE                                                         
03046          MOVE WE-LFPRM                TO TEMP-7.                  
03047                                                                   
03048      IF (CLAS-I-EP (CLAS-INDEXL) NOT = 'B' AND 'K' AND 'L')       
03049          MOVE ZEROS                  TO TEMP-8                    
03050          GO TO 8465-END-LF-EP-R78.                                
03051                                                                   
03052      MOVE TEMP-7                     TO LF-EPR-ST.                
03053                                                                   
03054      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L'                  
03055          COMPUTE TEMP-1 = (CR-LF-TERM + 1) * (CR-LF-TERM + 2)     
03056          COMPUTE TEMP-2 = (REM-TRM1 + 1) * (REM-TRM1 + 2)         
03057          IF DTE-R78 = '1'                                         
03058              COMPUTE TEMP-2 = (REM-TRM1 + 1) * (REM-TRM1 + 1).    
03059                                                                   
03060      COMPUTE TEMP-4 ROUNDED = TEMP-2 / TEMP-1.                    
03061                                                                   
03062      COMPUTE TEMP-8 ROUNDED =                                     
03063                WE-LFPRM-ALT - (WE-LFPRM-ALT * TEMP-4).            
03064                                                                   
03065  8465-END-LF-EP-R78.                                              
03066      IF LF-EPR-R78 = ZEROS                                        
03067          COMPUTE LF-EPR-R78 = TEMP-7 + TEMP-8.                    
03068                                                                   
03069  8470-EP-DO-LF-EP-PRO.                                            
03070      IF DTE-CLIENT = 'MIC' OR 'MCC'                               
03071          COMPUTE TEMP-5 ROUNDED = (REM-TRM1 - .5) / CR-LF-TERM    
03072      ELSE                                                         
03073          COMPUTE TEMP-5 ROUNDED = REM-TRM1 / CR-LF-TERM.          
03074                                                                   
03075      IF REM-TRM1 GREATER +0                                       
03076          COMPUTE TEMP-7 ROUNDED = WE-LFPRM - (WE-LFPRM * TEMP-5)  
03077      ELSE                                                         
03078          MOVE WE-LFPRM                  TO TEMP-7.                
03079                                                                   
03080      IF (CLAS-I-EP (CLAS-INDEXL) NOT = 'B' AND 'K' AND 'L')       
03081          MOVE ZEROS                     TO TEMP-8                 
03082          GO TO 8475-END-LF-EP-PRO.                                
03083                                                                   
03084      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L'                  
03085          IF DTE-CLIENT = 'MIC' OR 'MCC'                           
03086              COMPUTE TEMP-5 ROUNDED =                             
03087                            (REM-TRM1 + .5) / (CR-LF-TERM + 1)     
03088          ELSE                                                     
03089              COMPUTE TEMP-5 ROUNDED =                             
03090                            (REM-TRM1 + 1) / (CR-LF-TERM + 1).     
03091                                                                   
03092      COMPUTE TEMP-8 ROUNDED =                                     
03093                    WE-LFPRM-ALT - (WE-LFPRM-ALT * TEMP-5).        
03094                                                                   
03095      COMPUTE LF-EPR-ST = LF-EPR-ST + TEMP-8.                      
03096                                                                   
03097  8475-END-LF-EP-PRO.                                              
03098      COMPUTE LF-EPR-PRO = TEMP-7 + TEMP-8.                        
03099                                                                   
03100      GO TO 8485-EP-DO-LF-STATUTORY.                               
03101                                                                   
03102  8480-EP-DO-LF-EP-SUM.                                            
03103      IF REM-TRM1 NOT GREATER +0                                   
03104          COMPUTE LF-EPR-R78 =                                     
03105                          (WE-LFPRM + WE-LFPRM-ALT) - WE-LFRFND    
03106          MOVE LF-EPR-R78 TO LF-EPR-PRO LF-EPR-ST                  
03107          GO TO 8499-EXIT.                                         
03108                                                                   
03109      IF WE-LFPRM + WE-LFPRM-ALT EQUAL ZEROS                       
03110         MOVE +.01 TO WE-LFPRM.                                    
03111                                                                   
03112      COMPUTE TEMP-7 ROUNDED = WE-LFPRM - (WE-LFRFND *             
03113                    (WE-LFPRM / (WE-LFPRM + WE-LFPRM-ALT))).       
03114                                                                   
03115      IF TEMP-7 NOT GREATER ZERO                                   
03116          IF (CLAS-I-EP (CLAS-INDEXL) NOT = 'B' AND 'K' AND 'L')   
03117              MOVE ZEROS   TO LF-EPR-R78 LF-EPR-PRO LF-EPR-ST      
03118              GO TO 8499-EXIT                                      
03119          ELSE                                                     
03120              MOVE ZEROS             TO TEMP-7.                    
03121                                                                   
03122      COMPUTE TEMP-1 = CR-LF-TERM * (CR-LF-TERM + 1).              
03123      COMPUTE TEMP-2 = REM-TRM1 * (REM-TRM1 + 1).                  
03124      IF DTE-R78 = '1'                                             
03125          COMPUTE TEMP-2 = REM-TRM1 * REM-TRM1.                    
03126                                                                   
03127      COMPUTE TEMP-4 ROUNDED = TEMP-2 / TEMP-1.                    
03128      COMPUTE TEMP-5 ROUNDED = REM-TRM1 / CR-LF-TERM.              
03129                                                                   
03130      COMPUTE LF-EPR-R78 ROUNDED = TEMP-7 - (TEMP-7 * TEMP-4).     
03131                                                                   
03132      COMPUTE LF-EPR-PRO ROUNDED = TEMP-7 - (TEMP-7 * TEMP-5).     
03133                                                                   
03134      IF (CLAS-I-EP (CLAS-INDEXL) NOT = 'B' AND 'K' AND 'L')       
03135          GO TO 8485-EP-DO-LF-STATUTORY.                           
03136                                                                   
03137      MOVE LF-EPR-R78               TO LF-EPR-ST.                  
03138                                                                   
03139      COMPUTE TEMP-8 ROUNDED = WE-LFPRM-ALT - (WE-LFRFND *         
03140                    (WE-LFPRM-ALT / (WE-LFPRM + WE-LFPRM-ALT))).   
03141                                                                   
03142      IF TEMP-8 NOT GREATER ZERO                                   
03143          GO TO 8485-EP-DO-LF-STATUTORY.                           
03144                                                                   
03145      COMPUTE LF-EPR-R78 ROUNDED = LF-EPR-R78 +                    
03146                               (TEMP-8 - (TEMP-8 * TEMP-4)).       
03147                                                                   
03148      COMPUTE LF-EPR-PRO ROUNDED = LF-EPR-PRO +                    
03149                               (TEMP-8 - (TEMP-8 * TEMP-5)).       
03150                                                                   
03151      COMPUTE LF-EPR-ST ROUNDED = LF-EPR-ST +                      
03152                               (TEMP-8 - (TEMP-8 * TEMP-5)).       
03153                                                                   
03154  8485-EP-DO-LF-STATUTORY.                                         
03155      IF LF-EPR-ST NOT = ZEROS                                     
03156          GO TO 8499-EXIT.                                         
03157                                                                   
03158      IF CLAS-I-RL-AH (CLAS-INDEXL) = 'R'                          
03159          MOVE LF-EPR-R78             TO LF-EPR-ST                 
03160      ELSE                                                         
03161          MOVE LF-EPR-PRO             TO LF-EPR-ST.                
03162                                                                   
03163      IF STATE-ABBR (CLAS-INDEXS) = 'AL'                           
03164          MOVE LF-EPR-R78             TO LF-EPR-ST.                
03165                                                                   
03166      IF STATE-ABBR (CLAS-INDEXS) = 'WY'                           
03167          MOVE LF-EPR-PRO             TO LF-EPR-ST.                
03168                                                                   
03169  8499-EXIT.                                                       
03170      EXIT.                                                        
03171                                                                   
03172  EJECT                                                            
03173 ******************************************************************
03174 ***   C A L C U L A T E   A & H   E A R N E D   P R E M I U M  ***
03175 ******************************************************************
03176                                                                   
03177  8600-EP-DO-AH.                                                   
03178      MOVE +0                         TO   AH-EPR-R78              
03179                                           AH-EPR-PRO              
03180                                           AH-EPR-ST.              
03181                                                                   
03182      MOVE AH-OVERRIDE-L1       TO WE-LF-AH.                       
03183      MOVE CR-AHTYP             TO WE-BEN-TYPE.                    
03184                                                                   
03185      IF CR-AH-STATUS-AT-SETTLEMENT NOT = ' '                      
03186          MOVE WE-AHPRM TO AH-EPR-R78 AH-EPR-PRO AH-EPR-ST.        
03187                                                                   
03188      IF CR-AH-STATUS-AT-CANCEL = ' '                              
03189          GO TO 8630-EP-DO-AH-EP.                                  
03190                                                                   
03191      COMPUTE AH-EPR-R78 = WE-AHPRM - WE-AHRFND.                   
03192      MOVE AH-EPR-R78 TO AH-EPR-PRO AH-EPR-ST.                     
03193                                                                   
03194      GO TO 8699-EXIT.                                             
03195                                                                   
03196  8630-EP-DO-AH-EP.                                                
122002     IF (CR-AH-STATUS-AT-SETTLEMENT NOT = ' ') AND
122002        (CLAS-I-CALC-TYPE (CLAS-INDEXA) NOT = 'Z')
03199          GO TO 8699-EXIT.                                         
03200                                                                   
03201      IF REM-TRM1 GREATER CR-AH-TERM                               
03202          MOVE +0 TO AH-EPR-R78 AH-EPR-PRO AH-EPR-ST               
03203          GO TO 8699-EXIT.                                         
03204                                                                   
03205      IF CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'Z'                      
03206          COMPUTE WE-AHPRM = WE-AHPRM - WE-AHRFND.                 
03207                                                                   
03208      IF REM-TRM1 NOT GREATER +0                                   
03209          MOVE WE-AHPRM TO AH-EPR-R78 AH-EPR-PRO AH-EPR-ST         
03210          GO TO 8699-EXIT.                                         
03211                                                                   
03212      IF CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'B'                      
03213          MOVE WE-AHPRM TO AH-EPR-R78 AH-EPR-PRO AH-EPR-ST         
03214          GO TO 8699-EXIT.                                         
03215                                                                   
03216      COMPUTE TEMP-1 = CR-AH-TERM * (CR-AH-TERM + 1).              
03217      COMPUTE TEMP-2 = REM-TRM1 * (REM-TRM1 + 1).                  
03218      IF DTE-R78 = '1'                                             
03219          COMPUTE TEMP-2 = REM-TRM1 * REM-TRM1.                    
03220      COMPUTE TEMP-4 ROUNDED = TEMP-2 / TEMP-1.                    
03221                                                                   
03222      IF DTE-CLIENT = 'MIC' OR 'MCC'                               
03223          COMPUTE TEMP-5 ROUNDED = (REM-TRM1 - .5) / CR-AH-TERM    
03224      ELSE                                                         
03225          COMPUTE TEMP-5 ROUNDED = REM-TRM1 / CR-AH-TERM.          
03226                                                                   
03227      COMPUTE AH-EPR-R78 ROUNDED = WE-AHPRM * TEMP-4.              
03228      COMPUTE AH-EPR-R78 = WE-AHPRM - AH-EPR-R78.                  
03229                                                                   
03230      COMPUTE AH-EPR-PRO ROUNDED = WE-AHPRM * TEMP-5.              
03231      COMPUTE AH-EPR-PRO = WE-AHPRM - AH-EPR-PRO.                  
03232                                                                   
03233  8640-EP-DO-AH-STATUTORY.                                         
03234      MOVE AH-EPR-R78                 TO AH-EPR-ST.                
03235                                                                   
03236  8645-EP-AH-STAT-LOOP.                                            
03237      IF (STATE-ABBR (CLAS-INDEXS) = 'OH'  AND                     
03238          CR-DT GREATER 19831031)  OR                              
03239         (STATE-ABBR (CLAS-INDEXS) = 'OK'  AND                     
03240          CR-DT GREATER 19820629)  OR                              
03241         (STATE-ABBR (CLAS-INDEXS) = 'TX'  AND                     
03242          CR-DT GREATER 19830831)                                  
03243            COMPUTE AH-EPR-ST = (AH-EPR-R78 + AH-EPR-PRO) / 2.     
03244                                                                   
03245      IF DTE-CLIENT = 'CSO'  OR  'CID'                             
03246         IF STATE-ABBR (CLAS-INDEXS) = 'CO'                        
03247           IF CR-DT LESS THAN 19970101                             
03248               MOVE AH-EPR-R78        TO AH-EPR-ST                 
03249           ELSE                                                    
03250               COMPUTE AH-EPR-ST = (AH-EPR-R78 + AH-EPR-PRO) / 2.  
03251                                                                   
03252  8699-EXIT.                                                       
03253      EXIT.                                                        
03254                                                                   
03255  8800-REMAINING-TERM-ROUTINE.                                     
03256      MOVE CR-DT                      TO DC-GREG-DATE-CYMD.        
03257      MOVE 'L'                        TO DC-OPTION-CODE.           
03258      PERFORM 0800-DATE-CONVERT-ROUTINE THRU 0899-EXIT.            
03259      MOVE DC-BIN-DATE-1              TO CP-CERT-EFF-DT.           
03260                                                                   
03261      IF CR-LOAN-1ST-PMT-DT NOT NUMERIC                            
03262          MOVE ZEROS                  TO CR-LOAN-1ST-PMT-DT.       
03263                                                                   
03264      IF CR-LOAN-1ST-PMT-DT NOT = ZEROS                            
03265          MOVE CR-LOAN-1ST-PMT-DT     TO DC-GREG-DATE-1-YMD        
03266          MOVE ZEROS                  TO DC-ELAPSED-MONTHS         
03267                                         DC-ELAPSED-DAYS           
03268          MOVE '3'                    TO DC-OPTION-CODE            
03269          PERFORM 0800-DATE-CONVERT-ROUTINE THRU 0899-EXIT         
03270          IF NO-CONVERSION-ERROR                                   
03271              MOVE DC-BIN-DATE-1      TO CP-FIRST-PAY-DATE         
03272          ELSE                                                     
03273              MOVE ZEROS              TO CR-LOAN-1ST-PMT-DT.       
03274                                                                   
03275      IF CR-LOAN-1ST-PMT-DT = ZEROS                                
03276          MOVE CP-CERT-EFF-DT         TO DC-BIN-DATE-1             
03277          MOVE +1                     TO DC-ELAPSED-MONTHS         
03278          MOVE ZEROS                  TO DC-ELAPSED-DAYS           
03279          MOVE '6'                    TO DC-OPTION-CODE            
03280          PERFORM 0800-DATE-CONVERT-ROUTINE THRU 0899-EXIT         
03281          MOVE DC-BIN-DATE-1          TO CP-FIRST-PAY-DATE         
03282          MOVE DC-GREG-DATE-1-YMD     TO CR-LOAN-1ST-PMT-DT.       
03283                                                                   
03284      MOVE VALUATION-DT               TO DC-GREG-DATE-CYMD.        
03285      MOVE 'L'                        TO DC-OPTION-CODE.           
03286      PERFORM 0800-DATE-CONVERT-ROUTINE THRU 0899-EXIT.            
03287      MOVE DC-BIN-DATE-1              TO CP-VALUATION-DT.          
03288                                                                   
03289      MOVE SPACES                     TO CP-ACCT-FLD-5.            
03290      MOVE DTE-CLIENT                 TO CP-COMPANY-ID.            
03291      MOVE ORIG-TERM                  TO CP-ORIGINAL-TERM.         
03292      MOVE DTE-REM-TRM                TO CP-REM-TERM-METHOD.       
03293      MOVE DTE-REM-TRM-CALC-OPTION    TO CP-REM-TRM-CALC-OPTION.   
03294      MOVE '3'                        TO CP-PROCESS-TYPE.          
03295      MOVE DTE-CLASIC-COMPANY-CD      TO CP-COMPANY-CD.            
03296                                                                   
03297      IF DTE-CLIENT = 'FIM'                                        
03298          MOVE '5'                    TO CP-REM-TERM-METHOD.       
03299                                                                   
03300      IF DTE-CLIENT NOT = 'FLI' AND 'FLU'                          
03301          GO TO 8850-GET-REMAINING-TERM.                           
03302                                                                   
03303      IF WE-LF-AH = AH-OVERRIDE-L1                                 
03304          MOVE '2'                    TO CP-REM-TERM-METHOD        
03305          GO TO 8850-GET-REMAINING-TERM.                           
03306                                                                   
03307      IF CR-LFTYP = '01' OR '03'                                   
03308          MOVE '4'                    TO CP-REM-TERM-METHOD.       
03309                                                                   
03310      IF CR-LFTYP = '02' OR '04'                                   
03311          MOVE '2'                    TO CP-REM-TERM-METHOD.       
03312                                                                   
03313  8850-GET-REMAINING-TERM.                                         
03314      CALL 'ELRTRMX' USING CALCULATION-PASS-AREA.                  
03315                                                                   
03316      MOVE CP-REMAINING-TERM-1        TO REM-TRM1.                 
03317      MOVE CP-REMAINING-TERM-2        TO REM-TRM2.                 
03318      MOVE CP-REMAINING-TERM-3        TO REM-TRM3.                 
03319      MOVE CP-ODD-DAYS                TO TEM-TRM1.                 
03320                                                                   
03321  8899-EXIT.                                                       
03322      EXIT.                                                        
03323  EJECT                                                            
03324  9000-READ-CERT-IN.                                               
03325                                                                   
03326      READ CERT-IN  AT END                                         
03327          MOVE  'Y'               TO  CERT-END-SW                  
03328          MOVE  HIGH-VALUES       TO  CERTIFICATE-RECORD           
03329          GO TO 9099-EXIT.                                         
03330                                                                   
03331      IF CR-POLICY-IS-DECLINED OR                                  
03332         CR-POLICY-IS-VOID                                         
03333              GO TO 9000-READ-CERT-IN.                             
03334                                                                   
03335      COPY ELCCRTM1.                                               
03336                                                                   
03337      MOVE CR-CCYY TO CID-CCYY.                                    
03338      MOVE CR-MO   TO CID-MO.                                      
03339                                                                   
03340      IF DTE-PRC-OPT = 1 OR 2 OR 3 OR 4                            
03341          IF CERT-ISSUE-DATE LESS PROCESS-OPT-FROM-DT OR           
03342                          GREATER PROCESS-OPT-THRU-DT              
03343              GO TO 9000-READ-CERT-IN.                             
03344                                                                   
03345  9099-EXIT.                                                       
03346      EXIT.                                                        
03347                                                                   
03348  9200-READ-ACCT-IN.                                               
03349      READ ACCT-IN.                                                
03350                                                                   
03351      IF ERACCT-FILE-STATUS = '10'                                 
03352          MOVE  'Y'               TO  ACCT-END-SW                  
03353          MOVE  HIGH-VALUES       TO  ACCOUNT-MASTER               
03354          GO TO 9299-EXIT.                                         
03355                                                                   
03356      IF ERACCT-FILE-STATUS NOT = ZERO                             
03357          MOVE 'ERROR OCCURED READ - ERACCTT'  TO  WS-ABEND-MESSAGE
03358          MOVE ERACCT-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        
03359          GO TO ABEND-PGM.                                         
03360                                                                   
03361  9299-EXIT.                                                       
03362      EXIT.                                                        
03363                                                                   
03364  9300-READ-SORTED-FILE.                                           
03365      READ TEMP-WORK INTO SORT-WORK-REC   AT END                   
03366          MOVE  'Y'               TO  EXTR-END-SW                  
03367          MOVE  HIGH-VALUES       TO  SORT-WORK-REC.               
03368                                                                   
03369  9399-EXIT.                                                       
03370      EXIT.                                                        
03371                                                                   
03372  9400-READ-REIN-FILE.                                             
03373      READ REIN-WORK INTO SORT-WORK-REC-2  AT END                  
03374          MOVE  'Y'               TO  EXTR-END-SW                  
03375          MOVE  HIGH-VALUES       TO  SORT-WORK-REC-2.             
03376                                                                   
03377  9499-EXIT.                                                       
03378      EXIT.                                                        
03379                                                                   
03380  ABEND-PGM SECTION.                                               
03381                                  COPY ELCABEND.                   
