00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
SUNPSD PROGRAM-ID.                 SUN050.                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 05/16/94 13:28:34.                 
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          
00008 *                            VMOD=2.072.                          
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
00025 *REMARKS.                                                         
00026 *        CREATES ALPHA/GAAP FILES.                                
00027 *                                                                 
00028 *    DESCRIPTION OF PROGRAM OPTIONS.                              
00029 *                                                                 
00030 *    1  -  CREATE ALPHA EXTRACT ONLY.                             
00031 *    2  -  CREATE GAAP EXTRACT ONLY.                              
00032 *    3  -  CREATE BOTH ALPHA AND GAAP EXTRACTS.                   
00033                                                                   
00034  EJECT                                                            
00035  ENVIRONMENT DIVISION.                                            
00036  CONFIGURATION SECTION.                                           
00037  INPUT-OUTPUT SECTION.                                            
00038  FILE-CONTROL.                                                    
00039                                                                   
00040      SELECT CERT-MASTER      ASSIGN TO SYS011-UT-2400-S-SYS011.   
00041                                                                   
00042      SELECT ERACCTT          ASSIGN TO SYS015-FBA1-ERACCTT        
00043                              ACCESS IS SEQUENTIAL                 
00044                              ORGANIZATION IS INDEXED              
00045                              FILE STATUS IS ERACCTT-FILE-STATUS   
00046                              RECORD KEY IS AM-CONTROL-PRIMARY.    
00047                                                                   
00048      SELECT ERRTBL-IN        ASSIGN TO SYS014-FBA1-ERRTBLT
00049                              ACCESS IS DYNAMIC                    
00050                              ORGANIZATION IS INDEXED              
00051                              FILE STATUS IS ERRTBL-FILE-STATUS    
00052                              RECORD KEY IS RE-CONTROL-PRIMARY.    
00053                                                                   
00054      SELECT ERMEBL           ASSIGN SYS024-FBA1-ERMEBL            
00055                              ACCESS DYNAMIC                       
00056                              ORGANIZATION INDEXED                 
00057                              FILE STATUS ERMEBL-FILE-STATUS       
00058                              RECORD KEY ME-CONTROL-PRIMARY.       
00059                                                                   
00060      SELECT GAAP-EXTRACT     ASSIGN TO SYS012-UT-2400-S-SYS012.   
00061                                                                   
00062      SELECT ALPHA-EXTRACT    ASSIGN TO SYS013-UT-2400-S-SYS013.   
00063                                                                   
00064      SELECT PRINTER          ASSIGN TO SYS008-UR-1403-S-SYS008.   
00065                                                                   
00066      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   
00067                                                                   
00068      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   
00069  EJECT                                                            
00070  DATA DIVISION.                                                   
00071  FILE SECTION.                                                    
00072                                                                   
00073  FD  CERT-MASTER                                                  
00074      BLOCK CONTAINS 0 RECORDS
00075      RECORDING MODE F.                                            
00076                                                                   
00077      COPY ECSCRT01.                                               
00078  EJECT                                                            
00079  FD  ERACCTT.                                                     
00080                                                                   
00081      COPY ERCACCT.                                                
00082  EJECT                                                            
00083  FD  ERRTBL-IN.                                                      
00084                                                                   
00085      COPY ERCREIN.                                                
00086  EJECT                                                            
00087  FD  ERMEBL.                                                      
00088                                                                   
00089      COPY ERCMEBL.                                                
00090  EJECT                                                            
00091  FD  GAAP-EXTRACT                                                 
00092      COPY ECSGAPFD.                                               
00093                                                                   
00094  FD  ALPHA-EXTRACT                                                
00095                                                                   
00096      COPY ECSAEXFD.                                               
00097  EJECT                                                            
00098  FD  DISK-DATE                                                    
00099                                                                   
00100      COPY ELCDTEFD.                                               
00101  EJECT                                                            
00102  FD  PRINTER                                                      
00103                                                                   
00104      COPY ELCPRTFD.                                               
00105                                                                   
00106  FD  FICH                                                         
00107                                                                   
00108      COPY ELCFCHFD.                                               
00109                                                                   
00110  EJECT                                                            
00111  WORKING-STORAGE SECTION.                                         
00112  77  FILLER  PIC X(32) VALUE '********************************'.  
00113  77  FILLER  PIC X(32) VALUE '     ECS050 WORKING STORAGE     '.  
00114  77  FILLER  PIC X(32) VALUE '********** VMOD=2.072 **********'.  
00115                                                                   
00116  77  S1                      PIC S9(4)         COMP   VALUE +00.  
00117  77  SRL                     PIC S9(4)         COMP   VALUE +00.  
00118  77  SAVE-REMAINING-AMT      PIC S9(9)V99      COMP-3.            
00119  77  W-PASS-CTR              PIC S9(4)         COMP   VALUE +00.  
00120  77  WS-CEDE-FACT            PIC S9V9(4)       COMP   VALUE +0.   
00121                                                                   
SUNPSD 01 READCRT-CTR PIC 9(09) COMP-3 VALUE 0.
SUNPSD 01 DIFFTME-CTR PIC 9(09) COMP-3 VALUE 0.
SUNPSD 01 ELRATEX-CTR PIC 9(09) COMP-3 VALUE 0.
SUNPSD 01 ELRAMTX-CTR PIC 9(09) COMP-3 VALUE 0.
SUNPSD 01 ELRTRMX-CTR PIC 9(09) COMP-3 VALUE 0.
SUNPSD 01 ELUPRMX-CTR PIC 9(09) COMP-3 VALUE 0.
SUNPSD 01 ELDATCX-CTR PIC 9(09) COMP-3 VALUE 0.
SUNPSD 01 ALLCALL-CTR PIC 9(09) COMP-3 VALUE 0.
SUNPSD 01 SUNBTIME.
SUNPSD    05  SUNBHH   PIC 9(02).
SUNPSD    05  SUNBMN   PIC 9(02).
SUNPSD    05  SUNBSE   PIC 9(02).
SUNPSD    05  SUNBHS   PIC 9(02).
SUNPSD 01 SUNETIME.
SUNPSD    05  SUNEHH   PIC 9(02).
SUNPSD    05  SUNEMN   PIC 9(02).
SUNPSD    05  SUNESE   PIC 9(02).
SUNPSD    05  SUNEHS   PIC 9(02).
SUNPSD 01 SUNRTN  PIC X(08).                    
00122  01  MONTH-END-DATA.                                              
00123      12  ME-START-DATE.                                           
00124          16  ME-START-MO         PIC 99.                          
00125          16  FILLER              PIC X.                           
00126          16  ME-START-DA         PIC 99.                          
00127          16  FILLER              PIC X.                           
00128          16  ME-START-YR         PIC 99.                          
00129      12  ME-CNDS-DATE            PIC 9(6).                        
00130      12  ME-CNDS-DATE-R REDEFINES ME-CNDS-DATE.                   
00131          16  ME-CNDS-MO          PIC 99.                          
00132          16  ME-CNDS-DA          PIC 99.                          
00133          16  ME-CNDS-YR          PIC 99.                          
00134      12  ME-START-TIME           PIC 9(6).                        
00135      12  ME-UPDATE-FLAG          PIC X VALUE 'Y'.                 
00136          88  ME-DO-UPDATE        VALUE 'Y'.                       
00137          88  ME-NO-UPDATE        VALUE 'N'.                       
00138      12  ERMEBL-FILE-STATUS      PIC XX.                          
00139      12  MONTH-END-MOYR          PIC 9999 COMP.                   
00140                                                                   
00141  01  ERACCTT-FILE-STATUS         PIC XX       VALUE ZEROS.        
00142  01  ERRTBL-FILE-STATUS          PIC XX       VALUE ZEROS.        
00143                                                                   
00144  01  WS-ABEND-DATA.                                               
00145      12  WS-RETURN-CODE          PIC S9(4)    VALUE ZERO   COMP.  
00146      12  WS-ABEND-MESSAGE        PIC X(80)    VALUE SPACES.       
00147      12  WS-ABEND-FILE-STATUS    PIC XX       VALUE ZERO.         
00148      12  WS-ZERO                 PIC S9       VALUE ZERO   COMP-3.
00149                                                                   
00150      12  WS-ABEND-CODE           PIC 9(4).                        
00151      12  WORK-ABEND-CODE  REDEFINES  WS-ABEND-CODE.               
00152          16  W-ABEND-CODE-1      PIC XX.                          
00153          16  W-ABEND-CODE-2      PIC XX.                          
00154                                                                   
00155  01  WORK-CERT-NO.                                                
00156      12  WK-CERT-PREFIX      PIC X.                               
00157      12  FILLER              PIC X(10).                           
00158                                                                   
00159  01  WORK-REGION.                                                 
00160      12  WK-REGION           PIC XXX.                             
00161          88  A-CAPTIVE-ACCOUNT  VALUE '500' THRU '599'            
00162                                       '610' THRU '699'.           
00163      12  FILLER              PIC X(7).                            
00164                                                                   
00165  01  AH-RES-STATE-TABLE.                                          
00166      12  FILLER              PIC X(42)       VALUE                
00167              'AKALAZCADEGAIDILKSKYLAMDMIMTNVOHORSCSDWIWY'.        
00168      12  FILLER              PIC XX          VALUE HIGH-VALUES.   
00169                                                                   
00170  01  AH-RES-STATE-TAB REDEFINES AH-RES-STATE-TABLE.               
00171      12  AH-RES-STATE-ENT    PIC XX      OCCURS 22 TIMES.         
00172                                                                   
00173  01  TEXAS-REG-WORK-AREAS.                                        
00174      12  TEX-FACT-1          PIC S9(9)V9(2)  COMP-3.              
00175      12  TEX-FACT-2          PIC S9(3)       COMP-3.              
00176      12  TEX-FACT-3          PIC S9(3)       COMP-3.              
00177      12  TEX-FACT-4          PIC S9(7)       COMP-3.              
00178      12  TEX-FACT-5          PIC S9(3)       COMP-3.              
00179      12  TEX-FACT-6          PIC S9(3)       COMP-3.              
00180      12  TEX-FACT-7          PIC S9(7)       COMP-3.              
00181      12  TEX-FACT-8          PIC S9V9(6)     COMP-3.              
00182      12  TEX-FACT-9          PIC S9(4)V9(11) COMP-3.              
00183                                                                   
00184  01  NET-PAY-INTERFACE.                                           
00185      12  NP-APR              PIC S9(3)V9(4)  COMP-3.              
00186      12  NP-ORIG             PIC S9(3)       COMP-3.              
00187      12  NP-REM              PIC S9(3)       COMP-3.              
00188      12  NP-OPT              PIC X.                               
00189      12  NP-CAP              PIC S9(3)       COMP-3.              
00190      12  NP-FACTOR           PIC S9(4)V9(9)  COMP-3.              
00191      12  NP-WORK1            PIC S9(9)V9(6)  COMP-3.              
00192      12  NP-BENEFIT          PIC S9(9)V99    COMP-3.              
00193      12  NP-REMAINING        PIC S9(9)V99    COMP-3.              
00194      12  NP-AHPRM            PIC S9(7)V99    COMP-3.              
00195      12  NP-ACCOUNT          PIC X(10).                           
00196                                                                   
00197  01  WS-OU-TOTALS.                                                
00198      12  TOT-LF-OVER59       PIC S9(11)V99   COMP-3  VALUE +0.    
00199      12  TOT-LF-UNDER60      PIC S9(11)V99   COMP-3  VALUE +0.    
00200      12  TOT-AH-OVER59       PIC S9(11)V99   COMP-3  VALUE +0.    
00201      12  TOT-AH-UNDER60      PIC S9(11)V99   COMP-3  VALUE +0.    
00202      12  F-TOT-LF-OVER59     PIC S9(11)V99   COMP-3  VALUE +0.    
00203      12  F-TOT-LF-UNDER60    PIC S9(11)V99   COMP-3  VALUE +0.    
00204      12  F-TOT-AH-OVER59     PIC S9(11)V99   COMP-3  VALUE +0.    
00205      12  F-TOT-AH-UNDER60    PIC S9(11)V99   COMP-3  VALUE +0.    
00206                                                                   
00207  01  MISC.                                                        
CIDMOD     12  MONTHS-DIFF            PIC S9(5) VALUE +0 COMP-3.
CIDMOD     12  WS-GR-LFCOM            PIC S9(7)V99 VALUE +0 COMP-3.
CIDMOD     12  WS-GR-LFCOM-ALT        PIC S9(7)V99 VALUE +0 COMP-3.
CIDMOD     12  WS-GR-AHCOM            PIC S9(7)V99 VALUE +0 COMP-3.
00208      12  WS-NCL-OB-CERT-NO.                                       
00209          16  WS-OB-1-2          PIC X(02).                        
00210          16  WS-OB-3-4          PIC X(02).                        
00211          16  FILLER             PIC X(07) VALUE 'OBUNRN '.        
00212                                                                   
00213      12  WS-NCL-OB-DATES-TBL OCCURS 3.                            
00214          16  WS-NCL-OB-DATES    PIC 9(08).                        
00215 **** 1ST CURRENT RUN DATE                                         
00216 **** 2ND LAST MONTH END DATE                                      
00217 **** 3RD MONTH B/4 LAST MONTH END DATE                            
00218          16  WS-NCL-OB-DATES-R  REDEFINES WS-NCL-OB-DATES.        
00219              20  WS-OB-CCYY      PIC 9(04).                       
00220              20  WS-OB-CCYR  REDEFINES  WS-OB-CCYY.               
00221                  24  WS-OB-CC    PIC 9(02).                       
00222                  24  WS-OB-YR    PIC 9(02).                       
00223              20  WS-OB-MO        PIC 9(02).                       
00224              20  WS-OB-DA        PIC 9(02).                       
00225      12  WS-OB-DATE-1-SWITCH     PIC X     VALUE SPACES.          
00226          88  OB-DATE-1-USED         VALUE '*'.                    
00227      12  WS-OB-DATE-2-SWITCH     PIC X     VALUE SPACES.          
00228          88  OB-DATE-2-USED         VALUE '*'.                    
00229      12  WS-OB-DATE-3-SWITCH     PIC X     VALUE SPACES.          
00230          88  OB-DATE-3-USED         VALUE '*'.                    
00231      12  WS-HOLD-AM-CONTROL      PIC X(19) VALUE LOW-VALUES.      
00232      12  WS-HOLD-CERT            PIC X(1056)     VALUE SPACES.    
00233      12  WS-NCL-OB-DATA OCCURS 3.                                 
00234          16  WS-NCL-LIFE-OB OCCURS 20.                            
00235              20  WS-LIFE-OB-CODE PIC X(02).                       
00236              20  WS-LIFE-PREM    PIC S9(07)V99   COMP-3.          
00237              20  WS-LIFE-BEN     PIC S9(09)V99   COMP-3.          
00238          16  WS-NCL-AH-OB OCCURS 20.                              
00239              20  WS-AH-OB-CODE   PIC X(02).                       
00240              20  WS-AH-PREM      PIC S9(07)V99   COMP-3.          
00241              20  WS-AH-BEN       PIC S9(07)V99   COMP-3.          
00242      12  LINER               PIC S999        COMP-3 VALUE +80.    
00243      12  SAVE-YR-GP          PIC S9(4)       COMP.                
00244      12  WS-SUM-LF-FACTOR    PIC S99V9(7)    COMP-3 VALUE +0.     
00245      12  WS-SUM-AH-FACTOR    PIC S99V9(7)    COMP-3 VALUE +0.     
00246      12  SA                  PIC S999        COMP.                
00247      12  SB                  PIC S999        COMP.                
00248      12  PGM-SUB             PIC S999        COMP    VALUE +050.  
00249      12  SUB                 PIC S9999       COMP    VALUE +0.    
00250      12  SUBA                PIC S9999       COMP    VALUE +0.    
CIDMOD     12  SUBB                PIC S9999       COMP    VALUE +0.    
00251      12  ABEND-CODE          PIC X(4)                VALUE ZEROS. 
00252      12  ABEND-OPTION        PIC X                   VALUE 'Y'.   
00253      12  X                   PIC X                   VALUE ' '.   
00254      12  AH-EARN-METHOD      PIC X                   VALUE SPACE. 
00255      12  AGE-DIFFERENCE      PIC S99                 VALUE ZEROS. 
00256      12  LF-BAL-REMTERM      PIC S999V99     COMP-3  VALUE ZEROS. 
00257      12  LF-REM-TRM1         PIC S999V99     COMP-3  VALUE ZEROS. 
00258      12  LF-REM-TRM2         PIC S999V99     COMP-3  VALUE ZEROS. 
00259      12  AH-REM-TRM1         PIC S999V99     COMP-3  VALUE ZEROS. 
00260      12  AH-REM-TRM2         PIC S999V99     COMP-3  VALUE ZEROS. 
00261      12  SV-LF-BAL-REMTERM   PIC S999V99     COMP-3  VALUE ZEROS. 
00262      12  SV-LF-REM-TRM1      PIC S999V99     COMP-3  VALUE ZEROS. 
00263      12  SV-LF-REM-TRM2      PIC S999V99     COMP-3  VALUE ZEROS. 
00264      12  SV-AH-REM-TRM1      PIC S999V99     COMP-3  VALUE ZEROS. 
00265      12  SV-AH-REM-TRM2      PIC S999V99     COMP-3  VALUE ZEROS. 
00266      12  OLDEST-LF-REMTERM   PIC S999        COMP-3  VALUE ZEROS. 
00267      12  LF-OVER-100         PIC  9(5)               VALUE ZEROS. 
00268      12  OLDEST-AH-REMTERM   PIC S999        COMP-3  VALUE ZEROS. 
00269      12  INTERMED            PIC S9(9)V9(6)  COMP-3.              
00270      12  WS-FACT             PIC S9V9(6)     COMP-3.              
00271      12  WS-LINE             PIC S9(3)       COMP-3  VALUE +99.   
00272      12  WS-PAGE             PIC S9(5)       COMP-3  VALUE ZERO.  
00273      12  SAVE-GAAP-REC       PIC X(365).                          
00274      12  SAVE-ALPHA-REC      PIC X(300).                          
00275      12  SAVE-MORT-TYP       PIC X.                               
00276      12  BGN-DT              PIC X(6).                            
00277      12  ERN-START-DATE      PIC 9(8).                            
00278      12  ERN-START-DT  REDEFINES ERN-START-DATE.                  
00279          16  ESD-CC          PIC 99.                              
00280          16  ESD-YR          PIC 99.                              
00281          16  ESD-MO          PIC 99.                              
00282          16  ESD-DA          PIC 99.                              
00283      12  VALUATION-DATE      PIC 9(08).                           
00284      12  VALUATION-DATE-N REDEFINES VALUATION-DATE.               
00285          16  VAL-CC          PIC 99.                              
00286          16  VAL-YR          PIC 99.                              
00287          16  VAL-MO          PIC 99.                              
00288          16  VAL-DA          PIC 99.                              
00289      12  END-MO              PIC 99.                              
00290      12  END-DA              PIC 99.                              
00291      12  END-YR              PIC 99.                              
00292      12  WS-BIN-CR-DT        PIC XX.                              
00293      12  WS-BIN-VAL-DT       PIC XX.                              
00294      12  HLD-TRM             PIC S9(3)V99    COMP-3.              
00295      12  TEMP-1              PIC S9(5)V99    COMP-3.              
00296      12  TEMP-2              PIC S9(5)V99    COMP-3.              
00297      12  TEMP-3              PIC S9(2)V9(9)  COMP-3.              
00298      12  TEMP-4              PIC S9(9)V99    COMP-3.              
00299      12  REIN-RT-SW          PIC X               VALUE SPACES.    
00300      12  DTO-CC              PIC 99              VALUE ZEROS.     
00301      12  DTO-YR              PIC 99              VALUE ZEROS.     
00302      12  DTO-MO              PIC 99              VALUE ZEROS.     
00303      12  FLA-CLM-REIN-BASE   PIC S9(5)V99    COMP-3.              
00304      12  ADJ-FLA-REIN-BASE   PIC S9(5)V9(4)  COMP-3.              
00305      12  CLM-INCUR-REIN-DATE.                                     
00306          16  CIRD-CCYY.                                           
00307              20  CIRD-CC     PIC 99.                              
00308              20  CIRD-YR     PIC 99.                              
00309          16  CIRD-MO         PIC 99.                              
00310      12  REIN-AGENT.                                              
00311          16  FILLER          PIC X(4).                            
00312          16  RA-RCO          PIC X(6).                            
00313      12  DOMICILE-STATE      PIC XX              VALUE SPACES.    
00314      12  CUR-CALC            PIC S9(5)    COMP-3.                 
00315      12  ROH-LF-CALC         PIC S9(5)    COMP-3.                 
00316      12  ROH-AH-CALC         PIC S9(5)    COMP-3.                 
00317      12  PRIOR-CARRIER       PIC X               VALUE SPACES.    
00318          88  PRINTING-FINAL-TOTALS    VALUE '*'.                  
00319      12  O-B-SWITCH          PIC X               VALUE SPACES.    
00320          88  O-B-CERTIFICATE          VALUE '*'.                  
00321      12  SUMMARY-SWITCH      PIC X               VALUE SPACES.    
00322          88  SUMMARY-CERTIFICATE      VALUE '*'.                  
00323      12  O-B-LF-CNT-SW       PIC X               VALUE SPACES.    
00324          88  EXCLUDE-O-B-LF           VALUE '*'.                  
00325      12  O-B-AH-CNT-SW       PIC X               VALUE SPACES.    
00326          88  EXCLUDE-O-B-AH           VALUE '*'.                  
00327      12  LF-JOINT-SWITCH     PIC X               VALUE SPACES.    
00328          88  JOINT-LF-COVERAGE        VALUE 'J'.                  
00329      12  AH-JOINT-SWITCH     PIC X               VALUE SPACES.    
00330          88  JOINT-AH-COVERAGE        VALUE 'J'.                  
00331      12  REIN-ALPHA-SW       PIC X               VALUE SPACES.    
00332 **   12  WS-CR-BIN-DATE      PIC XX   VALUE SPACES.               
00333 **   12  WS-CR-CCYY          PIC 9(04) VALUE ZERO.                
00334 **   12  WS-CR-CCYR REDEFINES WS-CR-CCYY.                         
00335 **       16  WS-CR-CC        PIC 99.                              
00336 **       16  WS-CR-YY        PIC 99.                              
00337 **   12  WS-GR-CCYY          PIC 9(04) VALUE ZERO.                
00338 **   12  WS-GR-CCYR REDEFINES WS-GR-CCYY.                         
00339 **       16  WS-GR-CC        PIC 99.                              
00340 **       16  WS-GR-YY        PIC 99.                              
00341  EJECT                                                            
00342      COPY ELCDATE.                                                
00343  EJECT                                                            
00344      COPY ELCCALC.                                                
00345  EJECT                                                            
00346      COPY ELCREINV.                                               
00347  EJECT                                                            
00348      COPY ECSAEX01.                                               
00349      COPY ELCAEXVR.                                               
00350                                                                   
00351  01  INITIALIZED-ALPHA-RECORD PIC X(300).                         
00352  EJECT                                                            
00353      COPY ECSGAP01.                                               
00354      COPY ELCGAPVR.                                               
00355                                                                   
00356  01  INITIALIZED-GAAP-RECORD PIC X(365).                          
00357                                                                   
00358  01  GAAP-ALT-PRM-COM.                                            
00359      12  GR-LFCOM-ALT        PIC S9(7)V99    COMP-3.              
00360      12  LF-PRM              PIC S9(7)V99    COMP-3.              
00361      12  LF-PRM-ALT          PIC S9(7)V99    COMP-3.              
00362      12  GR-LFSRV-ALT        PIC S9(7)V99    COMP-3.              
00363  EJECT                                                            
00364                              COPY ECSRITAB.                       
00365                              COPY ELCPSEVR.                       
00366                                                                   
00367                                                                   
00368  01  REIN-ONLY-HOLDERS.                                           
00369      12  R-O-H           OCCURS 6.                                
00370          16  ROH-CNTRL       PIC X(35).                           
00371          16  ROH-ENTRY       PIC X.                               
00372          16  ROH-LF-STATUS   PIC X.                               
00373          16  ROH-AH-STATUS   PIC X.                               
00374  EJECT                                                            
00375  01  PRINT-LINES.                                                 
00376      12  HDR-1.                                                   
00377          16  FILLER          PIC X(45)         VALUE SPACES.      
00378          16  FILLER          PIC X(33)         VALUE              
00379              '       GAAP EXTRACT TOTALS       '.                 
00380          16  FILLER          PIC X(42)         VALUE SPACES.      
00381          16  FILLER          PIC X(8)          VALUE 'ECS050  '.  
00382      12  HDR-1A.                                                  
00383          16  FILLER          PIC X(45)         VALUE SPACES.      
00384          16  FILLER          PIC X(33)         VALUE              
00385              '        EXCEPTION LISTING        '.                 
00386          16  FILLER          PIC X(42)         VALUE SPACES.      
00387          16  FILLER          PIC X(8)          VALUE 'ECS050  '.  
00388      12  HDR-1B.                                                  
00389          16  FILLER          PIC X(45)         VALUE SPACES.      
00390          16  FILLER          PIC X(33)         VALUE              
00391              'COVERAGES EXCLUDED FROM VALUATION'.                 
00392          16  FILLER          PIC X(42)         VALUE SPACES.      
00393          16  FILLER          PIC X(8)          VALUE 'ECS050  '.  
00394      12  HDR-2.                                                   
00395          16  FILLER          PIC X(47)         VALUE SPACES.      
00396          16  H2-COMP         PIC X(30).                           
00397          16  FILLER          PIC X(43)         VALUE SPACES.      
00398          16  H2-DATE         PIC X(8).                            
00399      12  HDR-3.                                                   
00400          16  FILLER          PIC X(53)         VALUE SPACES.      
00401          16  H3-DATE         PIC X(18).                           
00402          16  FILLER          PIC X(49)         VALUE SPACES.      
00403          16  FILLER          PIC X(5)          VALUE 'PAGE '.     
00404          16  H3-PAGE         PIC ZZ,ZZ9.                          
00405      12  HDR-4A.                                                  
00406          16  FILLER          PIC X(62)      VALUE SPACES.         
00407          16  FILLER          PIC X(9)       VALUE '*------- '.    
00408          16  HD4A-DESC-1     PIC X(6).                            
00409          16  FILLER          PIC X(29)         VALUE              
00410              ' EXCLUSIONS ------* *------- '.                     
00411          16  HD4A-DESC-2     PIC X(6).                            
00412          16  FILLER          PIC X(20)         VALUE              
00413              ' EXCLUSIONS -------*'.                              
00414      12  HDR-4B.                                                  
00415          16  FILLER          PIC X(44)         VALUE              
00416              ' CARRIER  GROUPING  STATE    ACCOUNT    CERT'.      
00417          16  FILLER          PIC X(44)         VALUE              
00418              '. NO.   EFF DATE    EXIT DTE    DATE      RE'.      
00419          16  FILLER          PIC X(44)         VALUE              
00420              'ASON       EXIT DTE    DATE      REASON     '.      
00421      12  HDR-4C.                                                  
00422          16  FILLER          PIC X(44)         VALUE              
00423              ' CARRIER  GROUPING  STATE    ACCOUNT    CERT'.      
00424          16  FILLER          PIC X(44)         VALUE              
00425              '. NO.   EFF DATE  **************************'.      
00426          16  FILLER          PIC X(44)         VALUE              
00427              '***  WARNING  *****************************>'.      
00428      12  HDR-5.                                                   
00429          16  FILLER          PIC X(45)         VALUE              
00430              '                                             '.     
00431          16  FILLER          PIC X(44)         VALUE              
00432              ' GROSS          REINSURANCE                 '.      
00433      12  HDR-6.                                                   
00434          16  FILLER          PIC X             VALUE SPACES.      
00435          16  FILLER          PIC X(9)          VALUE 'CARRIER'.   
00436          16  HD6-CARR        PIC X             VALUE SPACE.       
00437      12  HDR-7.                                                   
00438          16  HD7-COV         PIC X(27)         VALUE SPACES.      
00439          16  FILLER          PIC X(26)         VALUE SPACES.      
00440          16  HD7-DESC-1      PIC X(12)         VALUE SPACES.      
00441          16  FILLER          PIC X(30)         VALUE SPACES.      
00442          16  HD7-DESC-2      PIC X(12)         VALUE SPACES.      
00443          16  FILLER          PIC X(16)         VALUE SPACES.      
00444      12  HDR-7A.                                                  
00445          16  FILLER          PIC X(47)         VALUE SPACES.      
00446          16  FILLER          PIC X(12)         VALUE 'COUNTS'.    
00447          16  FILLER          PIC X(03)         VALUE SPACES.      
00448          16  FILLER          PIC X(16)         VALUE              
00449                                             'ORIGINAL PREMIUM'.   
00450          16  FILLER          PIC X(12)         VALUE SPACES.      
00451          16  FILLER          PIC X(12)         VALUE 'COUNTS'.    
00452          16  FILLER          PIC X(02)         VALUE SPACES.      
00453          16  FILLER          PIC X(16)         VALUE              
00454                                             'ORIGINAL PREMIUM'.   
00455      12  HDR-7B.                                                  
00456          16  HD7B-COV        PIC X(27)         VALUE SPACES.      
00457          16  FILLER          PIC X(16)         VALUE SPACES.      
00458          16  HD7B-DESC-1     PIC X(12)         VALUE SPACES.      
00459          16  FILLER          PIC X(07)         VALUE SPACES.      
00460          16  HD7B-DESC-2     PIC X(12)         VALUE SPACES.      
00461          16  FILLER          PIC X(16)         VALUE SPACES.      
00462      12  DTL-1.                                                   
00463          16  FILLER.                                              
00464              20  FILLER          PIC X(4).                        
00465              20  D1-CARRIER      PIC X.                           
00466              20  FILLER          PIC X(6).                        
00467              20  D1-GROUPING     PIC X(6).                        
00468              20  FILLER          PIC X(5).                        
00469              20  D1-STATE        PIC XX.                          
00470              20  FILLER          PIC XXX.                         
00471              20  D1-ACCT         PIC X(10).                       
00472              20  FILLER          PIC XX.                          
00473              20  D1-CERT         PIC X(11).                       
00474              20  FILLER          PIC XX.                          
00475              20  D1-EFF-MO       PIC 99.                          
00476              20  D1-SLASH-1      PIC X.                           
00477              20  D1-EFF-DA       PIC 99.                          
00478              20  D1-SLASH-2      PIC X.                           
00479              20  D1-EFF-YR       PIC 99.                          
00480              20  FILLER          PIC X(4).                        
00481          16  D1-REIN-MESSAGE.                                     
00482              20  D1L-EXIT-DATE.                                   
00483                  24  D1L-EXIT-MO      PIC 99.                     
00484                  24  D1L-SLASH-1      PIC X.                      
00485                  24  D1L-EXIT-DA      PIC 99.                     
00486                  24  D1L-SLASH-2      PIC X.                      
00487                  24  D1L-EXIT-YR      PIC 99.                     
00488              20  FILLER               PIC XX.                     
00489              20  D1L-REAS-DATE.                                   
00490                  24  D1L-REAS-MO      PIC 99.                     
00491                  24  D1L-SLASH-3      PIC X.                      
00492                  24  D1L-REAS-DA      PIC 99.                     
00493                  24  D1L-SLASH-4      PIC X.                      
00494                  24  D1L-REAS-YR      PIC 99.                     
00495              20  FILLER               PIC XX.                     
00496              20  D1L-REASON           PIC X(11).                  
00497              20  FILLER               PIC X(4).                   
00498              20  D1A-EXIT-DATE.                                   
00499                  24  D1A-EXIT-MO      PIC 99.                     
00500                  24  D1A-SLASH-1      PIC X.                      
00501                  24  D1A-EXIT-DA      PIC 99.                     
00502                  24  D1A-SLASH-2      PIC X.                      
00503                  24  D1A-EXIT-YR      PIC 99.                     
00504              20  FILLER               PIC XX.                     
00505              20  D1A-REAS-DATE.                                   
00506                  24  D1A-REAS-MO      PIC 99.                     
00507                  24  D1A-SLASH-3      PIC X.                      
00508                  24  D1A-REAS-DA      PIC 99.                     
00509                  24  D1A-SLASH-4      PIC X.                      
00510                  24  D1A-REAS-YR      PIC 99.                     
00511              20  FILLER               PIC XX.                     
00512              20  D1A-REASON           PIC X(11).                  
00513              20  FILLER               PIC XX.                     
00514                                                                   
00515      12  TOT-0.                                                   
00516          16  FILLER          PIC X               VALUE SPACES.    
00517          16  T0-MSG          PIC X(36).                           
00518      12  TOT-1.                                                   
00519          16  FILLER          PIC X               VALUE SPACES.    
00520          16  T1-MSG          PIC X(36).                           
00521          16  FILLER          PIC X(6)            VALUE SPACES.    
00522          16  T1-COUNT1       PIC ZZZ,ZZZ,ZZ9-.                    
00523          16  FILLER          PIC X(6)            VALUE SPACES.    
00524          16  T1-COUNT2       PIC ZZZ,ZZZ,ZZ9-.                    
00525      12  TOT-2.                                                   
00526          16  FILLER          PIC X               VALUE SPACES.    
00527          16  T2-MSG          PIC X(36).                           
00528          16  T2-GROSS        PIC ZZ,ZZZ,ZZZ,ZZZ.99-.              
00529          16  FILLER          PIC XX              VALUE SPACES.    
00530          16  T2-REIN         PIC ZZ,ZZZ,ZZZ,ZZZ.99-.              
00531                                                                   
00532      12  TOT-3.                                                   
00533          16  FILLER          PIC X               VALUE SPACES.    
00534          16  T3-MSG          PIC X(36).                           
00535          16  FILLER          PIC X(6)            VALUE SPACES.    
00536          16  T3-GROSS        PIC ZZZ,ZZZ,ZZ9-.                    
00537          16  FILLER          PIC X(6)            VALUE SPACES.    
00538          16  T3-REIN         PIC ZZZ,ZZZ,ZZ9-.                    
00539                                                                   
00540      12  TOT-4.                                                   
00541          16  FILLER          PIC X               VALUE SPACES.    
00542          16  T4-MSG          PIC X(36).                           
00543          16  FILLER          PIC X(6)            VALUE SPACES.    
00544          16  T4-COUNT1       PIC ZZZ,ZZZ,ZZ9-.                    
00545          16  FILLER          PIC X(6)            VALUE SPACES.    
00546          16  T4-PREM-LF      PIC ZZ,ZZZ,ZZZ,ZZZ.99-.              
00547          16  FILLER          PIC X(6)            VALUE SPACES.    
00548          16  T4-COUNT2       PIC ZZZ,ZZZ,ZZ9-.                    
00549          16  FILLER          PIC X(6)            VALUE SPACES.    
00550          16  T4-PREM-AH      PIC ZZ,ZZZ,ZZZ,ZZZ.99-.              
00551                                                                   
00552      12  TOT-MSG-1           PIC X(36)           VALUE            
00553              'CERTIFICATES ON FILE............... '.              
00554      12  TOT-MSG-1A          PIC X(36)           VALUE            
00555              '     (RE-ISSUED CERTS)............. '.              
00556      12  TOT-MSG-1B          PIC X(36)           VALUE            
00557              '     (REINSURANCE ONLY CERTS)...... '.              
00558      12  TOT-MSG-1C          PIC X(36)           VALUE            
00559              '     (VOIDED CERTS)................ '.              
00560      12  TOT-MSG-1D          PIC X(36)           VALUE            
00561              '     (DECLINED CERTS).............. '.              
00562      12  TOT-MSG-2           PIC X(36)           VALUE            
00563              '   SINGLE PREMIUM CERTIFICATES..... '.              
00564      12  TOT-MSG-3           PIC X(36)           VALUE            
00565              '   OUTSTANDING BALANCE CERTIFICATES '.              
00566      12  TOT-MSG-4           PIC X(36)           VALUE            
00567              '   SUMMARY CERTIFICATES............ '.              
00568                                                                   
00569      12  TOT-MSG-5A          PIC X(36)           VALUE            
00570              '   INITIAL   COVERAGES............. '.              
00571      12  TOT-MSG-5           PIC X(36)           VALUE            
00572              '   CANCELLED COVERAGES............. '.              
00573      12  TOT-MSG-6           PIC X(36)           VALUE            
00574              '   CLAIMED   COVERAGES............. '.              
00575      12  TOT-MSG-7           PIC X(36)           VALUE            
00576              '   EXPIRED   COVERAGES............. '.              
00577      12  TOT-MSG-8           PIC X(36)           VALUE            
00578              '   FUTURE    COVERAGES............. '.              
00579      12  TOT-MSG-9-OB        PIC X(36)           VALUE            
00580              '   ACTIVE OB COVERAGES............. '.              
00581                                                                   
00582      12  TOT-MSG-9           PIC X(36)           VALUE            
00583              '   ACTIVE    COVERAGES............. '.              
00584                                                                   
00585      12  TOT-MSG-10          PIC X(36)           VALUE            
00586              'ALPHA EXTRACT RECORDS.............. '.              
00587      12  TOT-MSG-10J         PIC X(36)           VALUE            
00588              'ALPHA JOINT EXTRACT RECORDS........ '.              
00589      12  TOT-MSG-11          PIC X(36)           VALUE            
00590              'INITIAL   FACE AMOUNT.............. '.              
00591      12  TOT-MSG-12          PIC X(36)           VALUE            
00592              'REMAINING FACE AMOUNT.............. '.              
00593      12  TOT-MSG-13          PIC X(36)           VALUE            
00594              'REMAINING MONTHLY BENEFITS......... '.              
00595      12  TOT-MSG-14          PIC X(36)           VALUE            
00596              'FUTURES - FACE AMOUNT.............. '.              
00597      12  TOT-MSG-15          PIC X(36)           VALUE            
00598              'MONTHLY DECREASE................... '.              
00599      12  TOT-MSG-16          PIC X(36)           VALUE            
00600              'GAAP EXTRACT RECORDS............... '.              
00601                                                                   
00602      12  TOT-MSG-17          PIC X(36)           VALUE            
00603              '   INDIVIDUAL CERTS UNDER 60 MONTHS '.              
00604      12  TOT-MSG-18          PIC X(36)           VALUE            
00605              '   INDIVIDUAL CERTS OVER  59 MONTHS '.              
00606  EJECT                                                            
00607                                                                   
00608  01  TOT-ACCUMS.                                                  
00609      12  TOT-READ            PIC S9(9)     COMP-3  VALUE ZERO.    
00610      12  TOT-REIS            PIC S9(9)     COMP-3  VALUE ZERO.    
00611      12  TOT-DECLINED        PIC S9(9)     COMP-3  VALUE ZERO.    
00612      12  TOT-VOIDED          PIC S9(9)     COMP-3  VALUE ZERO.    
00613      12  TOT-REIO            PIC S9(9)     COMP-3  VALUE ZERO.    
00614      12  TOT-SNGLPRM         PIC S9(9)     COMP-3  VALUE ZERO.    
00615      12  TOT-OB              PIC S9(9)     COMP-3  VALUE ZERO.    
00616      12  TOT-SUM             PIC S9(9)     COMP-3  VALUE ZERO.    
00617      12  TOT-LF              PIC S9(9)     COMP-3  VALUE ZERO.    
00618      12  TOT-AH              PIC S9(9)     COMP-3  VALUE ZERO.    
00619      12  TOT-CANC-LF         PIC S9(9)     COMP-3  VALUE ZERO.    
00620      12  TOT-CANC-AH         PIC S9(9)     COMP-3  VALUE ZERO.    
00621      12  TOT-DTH             PIC S9(9)     COMP-3  VALUE ZERO.    
00622      12  TOT-DIS             PIC S9(9)     COMP-3  VALUE ZERO.    
00623      12  TOT-EXP-LF          PIC S9(9)     COMP-3  VALUE ZERO.    
00624      12  TOT-EXP-AH          PIC S9(9)     COMP-3  VALUE ZERO.    
00625      12  TOT-FUTR-LF         PIC S9(9)     COMP-3  VALUE ZERO.    
00626      12  TOT-FUTR-AH         PIC S9(9)     COMP-3  VALUE ZERO.    
00627      12  TOT-ACTIVE-LF       PIC S9(9)     COMP-3  VALUE ZERO.    
00628      12  TOT-ACTIVE-LF-OB    PIC S9(9)     COMP-3  VALUE ZERO.    
00629      12  TOT-ACTIVE-AH       PIC S9(9)     COMP-3  VALUE ZERO.    
00630      12  TOT-ACTIVE-AH-OB    PIC S9(9)     COMP-3  VALUE ZERO.    
00631      12  TOT-ALPHA           PIC S9(9)     COMP-3  VALUE ZERO.    
00632      12  TOT-ALPHA-JOINT     PIC S9(9)     COMP-3  VALUE ZERO.    
00633      12  TOT-GAAP            PIC S9(9)     COMP-3  VALUE ZERO.    
00634      12  REI-GAAP            PIC S9(9)     COMP-3  VALUE ZERO.    
00635      12  TOT-INIT-FACE       PIC S9(11)V99 COMP-3  VALUE ZERO.    
00636      12  REI-INIT-FACE       PIC S9(11)V99 COMP-3  VALUE ZERO.    
00637      12  TOT-REM-FACE        PIC S9(11)V99 COMP-3  VALUE ZERO.    
00638      12  REI-REM-FACE        PIC S9(11)V99 COMP-3  VALUE ZERO.    
00639      12  TOT-REM-AH          PIC S9(11)V99 COMP-3  VALUE ZERO.    
00640      12  REI-REM-AH          PIC S9(11)V99 COMP-3  VALUE ZERO.    
00641      12  TOT-FUTR-FACE       PIC S9(11)V99 COMP-3  VALUE ZERO.    
00642      12  REI-FUTR-FACE       PIC S9(11)V99 COMP-3  VALUE ZERO.    
00643      12  TOT-MO-DEC          PIC S9(11)V99 COMP-3  VALUE ZERO.    
00644      12  REI-MO-DEC          PIC S9(11)V99 COMP-3  VALUE ZERO.    
00645      12  TOT-LF-AMT          PIC S9(11)V99 COMP-3  VALUE ZERO.    
00646      12  TOT-AH-AMT          PIC S9(11)V99 COMP-3  VALUE ZERO.    
00647      12  TOT-CANC-LF-AMT     PIC S9(11)V99 COMP-3  VALUE ZERO.    
00648      12  TOT-CANC-AH-AMT     PIC S9(11)V99 COMP-3  VALUE ZERO.    
00649      12  TOT-DTH-AMT         PIC S9(11)V99 COMP-3  VALUE ZERO.    
00650      12  TOT-DIS-AMT         PIC S9(11)V99 COMP-3  VALUE ZERO.    
00651      12  TOT-EXP-LF-AMT      PIC S9(11)V99 COMP-3  VALUE ZERO.    
00652      12  TOT-EXP-AH-AMT      PIC S9(11)V99 COMP-3  VALUE ZERO.    
00653      12  TOT-FUTR-LF-AMT     PIC S9(11)V99 COMP-3  VALUE ZERO.    
00654      12  TOT-FUTR-AH-AMT     PIC S9(11)V99 COMP-3  VALUE ZERO.    
00655      12  TOT-ACTIVE-LF-AMT   PIC S9(11)V99 COMP-3  VALUE ZERO.    
00656      12  TOT-ACTIVE-AH-AMT   PIC S9(11)V99 COMP-3  VALUE ZERO.    
00657      12  TOT-ACT-LF-AMT-OB   PIC S9(11)V99 COMP-3  VALUE ZERO.    
00658      12  TOT-ACT-AH-AMT-OB   PIC S9(11)V99 COMP-3  VALUE ZERO.    
00659                                                                   
00660  01  ZERO-ACCUMS             PIC X(298).                          
00661                                                                   
00662  01  FIN-TOT-ACCUMS.                                              
00663      12  F-TOT-READ          PIC S9(9)     COMP-3  VALUE ZERO.    
00664      12  F-TOT-REIS          PIC S9(9)     COMP-3  VALUE ZERO.    
00665      12  F-TOT-DECLINED      PIC S9(9)     COMP-3  VALUE ZERO.    
00666      12  F-TOT-VOIDED        PIC S9(9)     COMP-3  VALUE ZERO.    
00667      12  F-TOT-REIO          PIC S9(9)     COMP-3  VALUE ZERO.    
00668      12  F-TOT-SNGLPRM       PIC S9(9)     COMP-3  VALUE ZERO.    
00669      12  F-TOT-OB            PIC S9(9)     COMP-3  VALUE ZERO.    
00670      12  F-TOT-SUM           PIC S9(9)     COMP-3  VALUE ZERO.    
00671      12  F-TOT-LF            PIC S9(9)     COMP-3  VALUE ZERO.    
00672      12  F-TOT-AH            PIC S9(9)     COMP-3  VALUE ZERO.    
00673      12  F-TOT-CANC-LF       PIC S9(9)     COMP-3  VALUE ZERO.    
00674      12  F-TOT-CANC-AH       PIC S9(9)     COMP-3  VALUE ZERO.    
00675      12  F-TOT-DTH           PIC S9(9)     COMP-3  VALUE ZERO.    
00676      12  F-TOT-DIS           PIC S9(9)     COMP-3  VALUE ZERO.    
00677      12  F-TOT-EXP-LF        PIC S9(9)     COMP-3  VALUE ZERO.    
00678      12  F-TOT-EXP-AH        PIC S9(9)     COMP-3  VALUE ZERO.    
00679      12  F-TOT-FUTR-LF       PIC S9(9)     COMP-3  VALUE ZERO.    
00680      12  F-TOT-FUTR-AH       PIC S9(9)     COMP-3  VALUE ZERO.    
00681      12  F-TOT-ACTIVE-LF     PIC S9(9)     COMP-3  VALUE ZERO.    
00682      12  F-TOT-ACTIVE-LF-OB  PIC S9(9)     COMP-3  VALUE ZERO.    
00683      12  F-TOT-ACTIVE-AH     PIC S9(9)     COMP-3  VALUE ZERO.    
00684      12  F-TOT-ACTIVE-AH-OB  PIC S9(9)     COMP-3  VALUE ZERO.    
00685      12  F-TOT-ALPHA         PIC S9(9)     COMP-3  VALUE ZERO.    
00686      12  F-TOT-ALPHA-JOINT   PIC S9(9)     COMP-3  VALUE ZERO.    
00687      12  F-TOT-GAAP          PIC S9(9)     COMP-3  VALUE ZERO.    
00688      12  F-REI-GAAP          PIC S9(9)     COMP-3  VALUE ZERO.    
00689      12  F-TOT-INIT-FACE     PIC S9(11)V99 COMP-3  VALUE ZERO.    
00690      12  F-REI-INIT-FACE     PIC S9(11)V99 COMP-3  VALUE ZERO.    
00691      12  F-TOT-REM-FACE      PIC S9(11)V99 COMP-3  VALUE ZERO.    
00692      12  F-REI-REM-FACE      PIC S9(11)V99 COMP-3  VALUE ZERO.    
00693      12  F-TOT-REM-AH        PIC S9(11)V99 COMP-3  VALUE ZERO.    
00694      12  F-REI-REM-AH        PIC S9(11)V99 COMP-3  VALUE ZERO.    
00695      12  F-TOT-FUTR-FACE     PIC S9(11)V99 COMP-3  VALUE ZERO.    
00696      12  F-REI-FUTR-FACE     PIC S9(11)V99 COMP-3  VALUE ZERO.    
00697      12  F-TOT-MO-DEC        PIC S9(11)V99 COMP-3  VALUE ZERO.    
00698      12  F-REI-MO-DEC        PIC S9(11)V99 COMP-3  VALUE ZERO.    
00699      12  F-TOT-LF-AMT        PIC S9(11)V99 COMP-3  VALUE ZERO.    
00700      12  F-TOT-AH-AMT        PIC S9(11)V99 COMP-3  VALUE ZERO.    
00701      12  F-TOT-CANC-LF-AMT   PIC S9(11)V99 COMP-3  VALUE ZERO.    
00702      12  F-TOT-CANC-AH-AMT   PIC S9(11)V99 COMP-3  VALUE ZERO.    
00703      12  F-TOT-DTH-AMT       PIC S9(11)V99 COMP-3  VALUE ZERO.    
00704      12  F-TOT-DIS-AMT       PIC S9(11)V99 COMP-3  VALUE ZERO.    
00705      12  F-TOT-EXP-LF-AMT    PIC S9(11)V99 COMP-3  VALUE ZERO.    
00706      12  F-TOT-EXP-AH-AMT    PIC S9(11)V99 COMP-3  VALUE ZERO.    
00707      12  F-TOT-FUTR-LF-AMT   PIC S9(11)V99 COMP-3  VALUE ZERO.    
00708      12  F-TOT-FUTR-AH-AMT   PIC S9(11)V99 COMP-3  VALUE ZERO.    
00709      12  F-TOT-ACTIVE-LF-AMT PIC S9(11)V99 COMP-3  VALUE ZERO.    
00710      12  F-TOT-ACTIVE-AH-AMT PIC S9(11)V99 COMP-3  VALUE ZERO.    
00711      12  F-TOT-ACT-LF-AMT-OB PIC S9(11)V99 COMP-3  VALUE ZERO.    
00712      12  F-TOT-ACT-AH-AMT-OB PIC S9(11)V99 COMP-3  VALUE ZERO.    
00713                                                                   
00714                                                                   
00715  01  WS-EXCEP-REPT-LINE.                                          
00716      03  FILLER                  PIC X(4).                        
00717      03  EXP-CARRIER             PIC X.                           
00718      03  FILLER                  PIC X(6).                        
00719      03  EXP-GROUPING            PIC X(6).                        
00720      03  FILLER                  PIC X(5).                        
00721      03  EXP-STATE               PIC XX.                          
00722      03  FILLER                  PIC XXX.                         
00723      03  EXP-ACCT                PIC X(10).                       
00724      03  FILLER                  PIC XX.                          
00725      03  EXP-CERT                PIC X(11).                       
00726      03  FILLER                  PIC XX.                          
00727      03  EXP-EFF-MO              PIC 99.                          
00728      03  EXP-SLASH-1             PIC X.                           
00729      03  EXP-EFF-DA              PIC 99.                          
00730      03  EXP-SLASH-2             PIC X.                           
00731      03  EXP-EFF-YR              PIC 99.                          
00732      03  FILLER                  PIC X(2).                        
00733      03  EXP-MESSAGE             PIC X(66).                       
00734                                                                   
CIDMOD 01  WS-LEVELS-CHARGEBACK-SWITCHES.                               
CIDMOD     12  WS-CHARGEBACK-LEVELS.                                    
CIDMOD         16  WS-CHARGEBACK-SW    PIC X(01) OCCURS 10 TIMES.       
CIDMOD
00735  01  EXCEPTION-MESSAGES.                                          
00736      03  EXC-1.                                                   
00737          05  FILLER              PIC X(17)                        
00738                         VALUE 'EARNING METHOD IS'.                
00739          05  FILLER              PIC X(24)                        
00740                         VALUE ' NET PAY AND CERT CONTAI'.         
00741          05  FILLER              PIC X(25)                        
00742                         VALUE 'NS A ZERO APR.           '.        
00743                                                                   
00744      03  EXC-2.                                                   
00745          05  FILLER              PIC X(17)                        
00746                         VALUE 'EARN METHOD NET P'.                
00747          05  FILLER              PIC X(24)                        
00748                         VALUE 'AY TRUNCATED-LOAN TERM L'.         
00749          05  FILLER              PIC X(25)                        
00750                         VALUE 'ESS THAN INSURANCE TERM  '.        
00751                                                                   
00752      03  EXC-3.                                                   
00753          05  FILLER              PIC X(17)                        
00754                         VALUE 'ERROR OCCURED IN '.                
00755          05  FILLER              PIC X(24)                        
00756                         VALUE 'CALCULATING UNEARNED PR'.          
00757          05  FILLER              PIC X(25)                        
00758                         VALUE 'EMIUM.                   '.        
00759                                                                   
00760      COPY ELCDTECX.                                               
00761                                                                   
00762      COPY ELCDTEVR.                                               
00763                                                                   
00764      COPY ELCCRTVR.                                               
00765                                                                   
00766      COPY ELCACCTV.                                               
00767                                                                   
00768  EJECT                                                            
00769  PROCEDURE DIVISION.                                              
00770                                                                   
00771  0000-GET-START-TIME.                                             
00772                                                                   
00773  0003-LOAD-DATE-CARD.                                             
00774                              COPY ELCDTERX.                       
00775                                                                   
00776      MOVE WS-TIME                TO ME-START-TIME.                
00777                                                                   
00778  0100-INITIALIZATION-ROUTINE.                                     
00779                                                                   
00780      OPEN INPUT  CERT-MASTER                                      
00781                  ERACCTT                                          
00782           I-O    ERMEBL                                           
00783           OUTPUT PRINTER.                                         
00784                                                                   
00785      IF ERACCTT-FILE-STATUS  = '00' OR '97'                       
00786          NEXT SENTENCE                                            
00787        ELSE                                                       
00788          MOVE '11'                   TO W-ABEND-CODE-1            
00789          MOVE ERACCTT-FILE-STATUS    TO W-ABEND-CODE-2            
00790          MOVE WS-ABEND-CODE          TO WS-RETURN-CODE            
00791          MOVE 'OPEN ERROR - ERACCTT' TO WS-ABEND-MESSAGE          
00792          GO TO ABEND-PGM.                                         
00793                                                                   
00794      IF ERMEBL-FILE-STATUS  = '00' OR '97'                        
00795          NEXT SENTENCE                                            
00796        ELSE                                                       
00797          MOVE 'N' TO ME-UPDATE-FLAG.                              
00798                                                                   
00799      IF DTE-PGM-OPT = '1' OR '3'                                  
00800          OPEN OUTPUT ALPHA-EXTRACT.                               
00801                                                                   
00802      IF DTE-PGM-OPT = '2' OR '3'                                  
00803          OPEN OUTPUT GAAP-EXTRACT.                                
00804                                                                   
00805      MOVE 'R'                    TO CP-RATE-FILE.                 
00806      MOVE 'O'                    TO CP-IO-FUNCTION.               
00807      PERFORM 3090-CALL-RATING-ROUTINE THRU 3099-EXIT.             
00808      IF IO-ERROR                                                  
00809          MOVE 0302               TO WS-RETURN-CODE                
00810          MOVE 'ERROR OCCURED OPENING - ELRATE'                    
00811                                  TO WS-ABEND-MESSAGE              
00812          GO TO ABEND-PGM.                                         
00813                                                                   
00814      MOVE WS-CURRENT-DATE        TO H2-DATE                       
00815                                     ME-START-DATE.                
00816      MOVE ME-START-MO            TO ME-CNDS-MO.                   
00817      MOVE ME-START-DA            TO ME-CNDS-DA.                   
00818      MOVE ME-START-YR            TO ME-CNDS-YR.                   
00819      MOVE COMPANY-NAME           TO H2-COMP.                      
00820      MOVE ALPH-DATE              TO H3-DATE.                      
00821      MOVE LIFE-OVERRIDE-L6       TO HD4A-DESC-1.                  
00822      MOVE AH-OVERRIDE-L6         TO HD4A-DESC-2.                  
00823                                                                   
00824      COMPUTE CUR-CALC = (RUN-CCYY * +12) + RUN-MO.                
00825      MOVE CUR-CALC               TO MONTH-END-MOYR.               
00826                                                                   
00827      MOVE HIGH-VALUES            TO REIN-HOLD-AREAS.              
00828      MOVE SPACES                 TO REIN-ONLY-HOLDERS             
00829                                     REIN-LEVELS-END.              
00830                                                                   
00831      MOVE RUN-DATE               TO VALUATION-DATE.               
00832      MOVE BIN-RUN-DATE           TO WS-BIN-VAL-DT.                
00833                                                                   
00834      IF DTE-CLIENT NOT EQUAL 'NCL'                                
00835         GO TO 0109-CONTINUE-INIT.                                 
00836                                                                   
00837      MOVE RUN-DATE               TO WS-NCL-OB-DATES(1)            
00838                                                                   
00839      MOVE +0                     TO DC-ELAPSED-MONTHS             
00840                                     DC-ELAPSED-DAYS.              
00841      MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1.                
00842      MOVE +0                     TO DC-ELAPSED-DAYS.              
00843      MOVE -1                     TO DC-ELAPSED-MONTHS.            
00844      MOVE '6'                    TO DC-OPTION-CODE.               
00845      PERFORM 0400-DATE-CONVERSION-ROUTINE THRU 0409-EXIT.         
00846      MOVE DC-GREG-DATE-CYMD      TO WS-NCL-OB-DATES (2).          
00847      MOVE +0                     TO DC-ELAPSED-DAYS.              
00848      MOVE -2                     TO DC-ELAPSED-MONTHS.            
00849      MOVE '6'                    TO DC-OPTION-CODE.               
00850      PERFORM 0400-DATE-CONVERSION-ROUTINE THRU 0409-EXIT.         
00851      MOVE DC-GREG-DATE-CYMD      TO WS-NCL-OB-DATES (3).          
00852      MOVE +0                     TO DC-ELAPSED-MONTHS             
00853                                     DC-ELAPSED-DAYS.              
00854                                                                   
00855      DISPLAY '     '.                                             
00856      DISPLAY ' CURRENT MONTH DATE ' WS-NCL-OB-DATES (1).          
00857      DISPLAY ' CURRENT DATE - 1   ' WS-NCL-OB-DATES (2).          
00858      DISPLAY ' CURRENT DATE - 2   ' WS-NCL-OB-DATES (3).          
00859      DISPLAY '     '.                                             
00860                                                                   
00861      PERFORM 0102-CLEAR-OB THRU 0102-OB-EXIT                      
00862          VARYING SUB FROM +1 BY +1 UNTIL SUB GREATER THAN +20.    
00863                                                                   
00864      MOVE +1                     TO SUB.                          
00865                                                                   
00866      PERFORM 0106-LOAD-LIFE THRU 0106-EXIT                        
00867          VARYING CLAS-INDEXL FROM CLAS-STARTL BY +1               
00868            UNTIL CLAS-INDEXL GREATER THAN CLAS-MAXL.              
00869                                                                   
00870      MOVE +1                     TO SUB.                          
00871                                                                   
00872      PERFORM 0107-LOAD-AH THRU 0107-EXIT                          
00873          VARYING CLAS-INDEXA FROM CLAS-STARTA BY +1               
00874            UNTIL CLAS-INDEXA GREATER THAN CLAS-MAXA.              
00875                                                                   
00876      GO TO 0109-CONTINUE-INIT.                                    
00877                                                                   
00878  0102-CLEAR-OB.                                                   
00879                                                                   
00880       MOVE ZEROS                 TO WS-LIFE-OB-CODE (1 SUB)       
00881                                     WS-LIFE-OB-CODE (2 SUB)       
00882                                     WS-LIFE-OB-CODE (3 SUB)       
00883                                     WS-AH-OB-CODE (1 SUB)         
00884                                     WS-AH-OB-CODE (2 SUB)         
00885                                     WS-AH-OB-CODE (3 SUB).        
00886       MOVE +0                    TO WS-LIFE-PREM (1 SUB)          
00887                                     WS-LIFE-PREM (2 SUB)          
00888                                     WS-LIFE-PREM (3 SUB)          
00889                                     WS-AH-PREM (1 SUB)            
00890                                     WS-AH-PREM (2 SUB)            
00891                                     WS-AH-PREM (3 SUB)            
00892                                     WS-LIFE-BEN (1 SUB)           
00893                                     WS-LIFE-BEN (2 SUB)           
00894                                     WS-LIFE-BEN (3 SUB)           
00895                                     WS-AH-BEN (1 SUB)             
00896                                     WS-AH-BEN (2 SUB)             
00897                                     WS-AH-BEN (3 SUB).            
00898                                                                   
00899  0102-OB-EXIT.                                                    
00900      EXIT.                                                        
00901                                                                   
00902  0103-CLEAR-OB.                                                   
00903                                                                   
00904       MOVE +0                    TO WS-LIFE-PREM (1 SUB)          
00905                                     WS-LIFE-PREM (2 SUB)          
00906                                     WS-LIFE-PREM (3 SUB)          
00907                                     WS-AH-PREM (1 SUB)            
00908                                     WS-AH-PREM (2 SUB)            
00909                                     WS-AH-PREM (3 SUB)            
00910                                     WS-LIFE-BEN (1 SUB)           
00911                                     WS-LIFE-BEN (2 SUB)           
00912                                     WS-LIFE-BEN (3 SUB)           
00913                                     WS-AH-BEN (1 SUB)             
00914                                     WS-AH-BEN (2 SUB)             
00915                                     WS-AH-BEN (3 SUB).            
00916                                                                   
00917  0103-OB-EXIT.                                                    
00918      EXIT.                                                        
00919                                                                   
00920  0106-LOAD-LIFE.                                                  
00921                                                                   
00922      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) EQUAL 'B'                  
00923         MOVE CLAS-I-BEN (CLAS-INDEXL)                             
00924                                  TO WS-LIFE-OB-CODE (1 SUB)       
00925                                     WS-LIFE-OB-CODE (2 SUB)       
00926                                     WS-LIFE-OB-CODE (3 SUB)       
00927         ADD +1 TO SUB.                                            
00928                                                                   
00929  0106-EXIT.                                                       
00930      EXIT.                                                        
00931                                                                   
00932  0107-LOAD-AH.                                                    
00933                                                                   
00934      IF CLAS-I-CALC-TYPE (CLAS-INDEXA) EQUAL 'B'                  
00935         MOVE CLAS-I-BEN (CLAS-INDEXA)                             
00936                                  TO WS-AH-OB-CODE (1 SUB)         
00937                                     WS-AH-OB-CODE (2 SUB)         
00938                                     WS-AH-OB-CODE (3 SUB)         
00939         ADD +1 TO SUB.                                            
00940                                                                   
00941  0107-EXIT.                                                       
00942      EXIT.                                                        
00943                                                                   
00944  0109-CONTINUE-INIT.                                              
00945                                                                   
00946                                                                   
00947      PERFORM CLEAR-REIN-HOLD THRU CLEAR-REIN-HOLD-X.              
00948      MOVE TOT-ACCUMS             TO ZERO-ACCUMS.                  
00949                                                                   
00950      MOVE SPACES                 TO ALPHA-RECORD.                 
00951      MOVE 'AX'                   TO AX-RECORD-ID.                 
00952      MOVE ZEROS                  TO AX-AGE   AX-APR  AX-PMT-FREQ  
00953                                     AX-LF-TYP        AX-AH-TYP    
00954                                     AX-LF-TERM       AX-AH-TERM   
00955                                     AX-LF-REMTERM    AX-AH-REMTERM
00956                                     AX-LF-AMT        AX-AH-AMT    
00957                                     AX-LF-REMAMT     AX-AH-REMAMT 
00958                                     AX-LF-PRM        AX-AH-PRM    
00959                                     AX-LF-REFUND     AX-AH-REFUND 
00960                                     AX-LF-CLAIM-PMTS AX-DT        
00961                                     AX-AH-CLAIM-PMTS AX-LF-CNCL   
00962                                     AX-CAN-MICROFILM-NO AX-DEATH  
00963                                     AX-ISS-MICROFILM-NO AX-LF-EXIT
00964                                     AX-LF-AMT-ALT    AX-LF-EXPIRES
00965                                     AX-LF-REMAMT-ALT AX-AH-CNCL   
00966                                     AX-LF-PRM-ALT    AX-LUMP-SUM  
00967                                     AX-AH-EXIT       AX-AH-EXPIRES
00968                                     AX-ENTRY.                     
00969                                                                   
00970      MOVE ALPHA-RECORD           TO INITIALIZED-ALPHA-RECORD.     
00971                                                                   
00972      MOVE SPACES            TO GAAP-RECORD.                       
00973      MOVE 'GR'              TO GR-RECORD-ID.                      
00974      MOVE ZEROS             TO GR-LFTYP          GR-AHTYP         
00975                                GR-APR            GR-PMT-FREQ      
00976                                GR-LOAN-TERM      GR-AGE           
00977                                GR-ACC-EXPIRES                     
00978                                GR-LF-TERM        GR-AH-TERM       
00979                                GR-LF-REMTERM     GR-AH-REMTERM    
00980                                GR-LF-UP-REMTERM  GR-AH-UP-REMTERM 
00981                                GR-LFBEN          GR-AHBEN         
00982                                GR-LFPRM          GR-AHPRM         
00983                                GR-LFCOM          GR-AHCOM         
00984                                GR-LFEXP          GR-AHEXP         
00985                                GR-LFTAX          GR-AHTAX         
00986                                GRP-LFPRM         GRP-AHPRM        
00987                                GRP-LFCOM         GRP-AHCOM        
00988                                GRP-LFEXP         GRP-AHEXP        
00989                                GRP-LFTAX         GRP-AHTAX        
00990                                GRR-LFPRM         GRR-AHPRM        
00991                                GRR-LFCOM         GRR-AHCOM        
00992                                GRR-LFEXP         GRR-AHEXP        
00993                                GRR-LFTAX         GRR-AHTAX        
00994                                GRD-LFPRM         GRD-AHPRM        
00995                                GRD-LFCOM         GRD-AHCOM        
00996                                GRS-LFPRM         GRS-AHPRM        
00997                                GRS-LFCOM         GRS-AHCOM        
00998                                GR-LFSRV          GR-AHSRV         
00999                                GR-MORT-AGE       GR-REM-AMT       
01000                                GR-MO-DEC         GR-MORT-FACT     
01001                                GR-RESV           GR-ALT-RESV      
01002                                GR-CNT            GR-CNT-LF        
01003                                GR-CNT-AH         GR-AH-REM-BEN    
01004                                GR-DIR-CERT-LFBEN GR-EFF           
01005                                GR-ENT-DT         GR-LF-EXPIRE-DATE
01006                                GR-AH-EXPIRE-DATE.                 
01007                                                                   
01008      MOVE GAAP-RECORD       TO INITIALIZED-GAAP-RECORD.           
01009                                                                   
01010      PERFORM 0590-READ-AM-MSTR THRU 0598-EXIT.                    
01011                                                                   
01012  0110-READ-CERT-MASTER.                                           
01013                                                                   
01014      READ CERT-MASTER                                             
01015               AT END  GO TO 9000-END-THE-JOB.                     
01016                                                                   
SUNPSD     ADD 1 TO READCRT-CTR.
01017      IF DTE-CLIENT EQUAL 'HAN'                                    
01018         IF (CR-GROUPING EQUAL '097713' OR '010184')               
01019                            OR                                     
01020            ((CR-CARRIER EQUAL '2') AND                            
01021             (CR-DT GREATER THAN  19930630 ))                      
01022            GO TO 0110-READ-CERT-MASTER.                           
01023                                                                   
01024      IF DTE-CLIENT = 'NCL'                                        
01025         IF CR-ACCT-CONTROL NOT = WS-HOLD-AM-CONTROL  AND          
01026            WS-HOLD-AM-CONTROL NOT = LOW-VALUES                    
01027             MOVE CERTIFICATE-RECORD  TO WS-HOLD-CERT              
01028             PERFORM 0395-CHECK-OUT-OB THRU 0395-EXIT              
01029             MOVE WS-HOLD-CERT        TO CERTIFICATE-RECORD        
01030             PERFORM 0103-CLEAR-OB THRU 0103-OB-EXIT               
01031             VARYING SUB FROM +1 BY +1 UNTIL SUB GREATER THAN +20. 
01032                                                                   
01033      COPY ELCCRTM1.                                               
01034                                                                   
01035      MOVE CR-ACCT-CONTROL     TO WS-HOLD-AM-CONTROL.              
01036                                                                   
01037      IF CR-CARRIER NOT = PRIOR-CARRIER                            
01038          PERFORM 8000-CARRIER-BREAK-ROUTINE THRU 8999-EXIT.       
01039                                                                   
01040      IF DTE-CLIENT EQUAL 'FIM'                                    
01041         IF CR-REIN-TABLE EQUAL '38K'                              
01042            MOVE SPACES TO CR-REIN-TABLE.                          
01043                                                                   
01044      MOVE SPACES                 TO O-B-SWITCH                    
01045                                     SUMMARY-SWITCH                
01046                                     GR-REIN.                      
01047                                                                   
01048      ADD +1                      TO TOT-READ.                     
01049                                                                   
01050      IF CR-ENTRY-STATUS = '5'                                     
01051          ADD +1                  TO TOT-REIS.                     
01052                                                                   
01053      IF CR-ENTRY-STATUS = 'D'                                     
01054          ADD +1                  TO TOT-DECLINED.                 
01055                                                                   
01056      IF CR-ENTRY-STATUS = 'V'                                     
01057          ADD +1                  TO TOT-VOIDED.                   
01058                                                                   
01059      IF CR-ENTRY-STATUS = '9'                                     
01060          ADD +1                  TO TOT-REIO.                     
01061                                                                   
01062      PERFORM 7000-REIN-ONLY-STATUS THRU 7599-EXIT.                
01063                                                                   
01064                                                                   
01065      PERFORM 0500-MATCH-TO-ACC     THRU 0599-EXIT.                
01066                                                                   
01067                                                                   
01068      MOVE CLAS-STARTL TO CLAS-INDEXL.                             
01069      MOVE CLAS-STARTA TO CLAS-INDEXA.                             
01070      MOVE CLAS-STARTS TO CLAS-INDEXS.                             
01071                                                                   
01072      MOVE SPACES TO LF-JOINT-SWITCH, AH-JOINT-SWITCH.             
01073                                                                   
01074  0120-STATE-LOOKUP.                                               
01075      IF CR-STATE NOT = STATE-SUB (CLAS-INDEXS)                    
01076          IF CLAS-INDEXS NOT = CLAS-MAXS                           
01077              ADD +1 TO CLAS-INDEXS                                
01078              GO TO 0120-STATE-LOOKUP.                             
01079                                                                   
01080      IF CR-LFTYP = ZERO                                           
01081          MOVE ZERO TO CLAS-INDEXL                                 
01082          GO TO 0140-FIND-AH.                                      
01083                                                                   
01084  0130-FIND-LIFE-LOOP.                                             
01085      IF CLAS-INDEXL GREATER CLAS-MAXL OR CLAS-STARTL = ZERO       
01086          DISPLAY 'LIFE BENEFIT ' CR-LFTYP ' NOT IN TABLE'         
CIDMOD         DISPLAY 'RETURN CODE - 0401'                             
01087          MOVE 0401 TO WS-RETURN-CODE                              
CIDMOD         GO TO 0130-LIFE-LOOP-EXIT.                               
CIDMOD***      GO TO ABEND-PGM.                                         
01089                                                                   
01090      IF CR-LFTYP NOT = CLAS-I-BEN (CLAS-INDEXL)                   
01091          ADD +1 TO CLAS-INDEXL                                    
01092          GO TO 0130-FIND-LIFE-LOOP.                               
01093                                                                   
01094      MOVE CLAS-I-JOINT (CLAS-INDEXL) TO LF-JOINT-SWITCH.          
01095                                                                   
CIDMOD 0130-LIFE-LOOP-EXIT.                                             
CIDMOD     EXIT.                                                        
CIDMOD                                                                  
01096  0140-FIND-AH.                                                    
01097      IF CR-AHTYP = ZERO                                           
01098          MOVE ZERO TO CLAS-INDEXA                                 
01099          GO TO 0160-CALC-REM-TERM.                                
01100                                                                   
01101  0150-FIND-AH-LOOP.                                               
01102      IF CLAS-INDEXA GREATER CLAS-MAXA OR CLAS-STARTA = ZEROS      
01103          DISPLAY 'A&H BENEFIT ' CR-AHTYP ' NOT IN TABLE'          
01104          MOVE 0402 TO WS-RETURN-CODE                              
01105          GO TO ABEND-PGM.                                         
01106                                                                   
01107      IF CR-AHTYP NOT = CLAS-I-BEN (CLAS-INDEXA)                   
01108          ADD +1 TO CLAS-INDEXA                                    
01109          GO TO 0150-FIND-AH-LOOP.                                 
01110                                                                   
01111      MOVE CLAS-I-JOINT (CLAS-INDEXA) TO AH-JOINT-SWITCH.          
01112                                                                   
01113      IF AH-EARN-METHOD = SPACE                                    
01114          MOVE CLAS-I-EP (CLAS-INDEXA) TO AH-EARN-METHOD.          
01115                                                                   
01116  0160-CALC-REM-TERM.                                              
01117                                                                   
01118      MOVE CR-DT                      TO DC-GREG-DATE-CYMD.        
01119      MOVE 'L'                        TO DC-OPTION-CODE.           
01120      PERFORM 0400-DATE-CONVERSION-ROUTINE THRU 0409-EXIT          
01121      MOVE DC-BIN-DATE-1              TO CP-CERT-EFF-DT            
01122                                         WS-BIN-CR-DT.             
01123                                                                   
01124      IF CR-LOAN-1ST-PMT-DT NOT NUMERIC                            
01125          MOVE ZEROS                  TO CR-LOAN-1ST-PMT-DT.       
01126                                                                   
01127      MOVE LOW-VALUES             TO     CP-FIRST-PAY-DATE.        
01128                                                                   
01129      IF CR-LOAN-1ST-PMT-DT NOT = ZEROS                            
01130          MOVE CR-LOAN-1ST-PMT-DT     TO DC-GREG-DATE-1-YMD        
01131          MOVE ZEROS                  TO DC-ELAPSED-MONTHS         
01132                                         DC-ELAPSED-DAYS           
01133          MOVE '3'                    TO DC-OPTION-CODE            
01134          PERFORM 0400-DATE-CONVERSION-ROUTINE THRU 0409-EXIT      
01135          IF NO-CONVERSION-ERROR                                   
01136              MOVE DC-BIN-DATE-1      TO CP-FIRST-PAY-DATE         
01137          ELSE                                                     
01138              MOVE ZEROS              TO CR-LOAN-1ST-PMT-DT.       
01139                                                                   
01140      IF CP-FIRST-PAY-DATE  LESS THAN  CP-CERT-EFF-DT              
01141          MOVE ZEROS  TO  CR-LOAN-1ST-PMT-DT.                      
01142                                                                   
01143      IF CR-LOAN-1ST-PMT-DT = ZEROS                                
01144          MOVE CP-CERT-EFF-DT         TO DC-BIN-DATE-1             
01145          MOVE +1                     TO DC-ELAPSED-MONTHS         
01146          MOVE ZEROS                  TO DC-ELAPSED-DAYS           
01147          MOVE '6'                    TO DC-OPTION-CODE            
01148          PERFORM 0400-DATE-CONVERSION-ROUTINE THRU 0409-EXIT      
01149          MOVE DC-BIN-DATE-2          TO CP-FIRST-PAY-DATE         
01150          MOVE DC-GREG-DATE-1-YMD     TO CR-LOAN-1ST-PMT-DT.       
01151                                                                   
01152      IF GR-REIN NOT = 'R'                                         
01153          MOVE BIN-RUN-DATE           TO CP-VALUATION-DT           
01154      ELSE                                                         
01155          MOVE ERN-START-DATE         TO DC-GREG-DATE-CYMD         
01156          MOVE 'L'                    TO DC-OPTION-CODE            
01157          PERFORM 0400-DATE-CONVERSION-ROUTINE THRU 0409-EXIT      
01158          MOVE DC-BIN-DATE-1          TO CP-VALUATION-DT.          
01159                                                                   
CIDMOD     MOVE WS-BIN-CR-DT           TO DC-BIN-DATE-1                 
CIDMOD     MOVE WS-BIN-VAL-DT          TO DC-BIN-DATE-2                 
CIDMOD     MOVE '1'                    TO DC-OPTION-CODE                
CIDMOD     MOVE ' '                    TO DC-CENTURY-ADJUSTMENT         
CIDMOD     MOVE ZEROS                  TO DC-ELAPSED-MONTHS             
CIDMOD                                    DC-ODD-DAYS-OVER              
CIDMOD                                    DC-ELAPSED-DAYS               
CIDMOD                                    MONTHS-DIFF
CIDMOD     PERFORM 0400-DATE-CONVERSION-ROUTINE
CIDMOD                                 THRU 0409-EXIT
CIDMOD                                                                  
CIDMOD     IF NO-CONVERSION-ERROR
CIDMOD        MOVE DC-ELAPSED-MONTHS   TO MONTHS-DIFF
CIDMOD        IF DC-ODD-DAYS-OVER > ZERO
CIDMOD           ADD +1                TO MONTHS-DIFF
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD
01160      MOVE STATE-SUB (CLAS-INDEXS)    TO CP-STATE.                 
01161      MOVE STATE-ABBR (CLAS-INDEXS)   TO CP-STATE-STD-ABBRV.       
01162      MOVE '3'                        TO CP-PROCESS-TYPE.          
01163      MOVE DTE-CLASIC-COMPANY-CD      TO CP-COMPANY-CD.            
01164      MOVE DTE-CLIENT                 TO CP-COMPANY-ID.            
01165      MOVE SPACES                     TO CP-ACCT-FLD-5.            
01166      MOVE DTE-REM-TRM                TO CP-REM-TERM-METHOD.       
01167                                                                   
01168      IF DTE-CLIENT = 'FIM'                                        
01169          MOVE '5'                    TO CP-REM-TERM-METHOD.       
01170                                                                   
01171      IF DTE-CLIENT = 'CSL'                                        
01172          IF CR-CARRIER = '2'                                      
01173              IF CR-LFTYP = '07'  OR  '08'                         
01174                  MOVE '4'            TO CP-REM-TERM-METHOD.       
01175                                                                   
01176      IF DTE-CLIENT = 'POS'                                        
01177          IF CR-CARRIER = '1'                                      
01178              MOVE '1'                TO CP-REM-TERM-METHOD        
01179          ELSE                                                     
01180              MOVE '2'                TO CP-REM-TERM-METHOD.       
01181                                                                   
01182                                                                   
01183      IF CR-POLICY-IS-VOID OR CR-POLICY-IS-DECLINED                
01184          MOVE ZEROS                  TO LF-REM-TRM1               
01185                                         LF-REM-TRM2               
01186                                         AH-REM-TRM1               
01187                                         AH-REM-TRM2               
01188          GO TO 0179-CALC-REM-TERM-EXIT.                           
01189                                                                   
01190      IF CR-LFTYP = ZEROS                                          
01191          MOVE ZEROS                  TO LF-REM-TRM1               
01192                                         LF-REM-TRM2               
01193          GO TO 0170-CALC-REM-TERM-AH.                             
01194                                                                   
01195      IF (CR-ENTRY-STATUS NOT = ('9' AND '5') AND                  
01196         CR-IND-GRP = '1' AND                                      
01197         CR-ENTRY-CCYY = RUN-CCYY AND                              
01198         GR-REIN NOT = 'R')                                        
01199          IF CR-LF-TERM LESS THAN +60                              
01200              COMPUTE TOT-LF-UNDER60 = TOT-LF-UNDER60 +            
01201                                       CR-LFPRM + CR-LFPRM-ALT     
01202          ELSE                                                     
01203              COMPUTE TOT-LF-OVER59 = TOT-LF-OVER59 +              
01204                                       CR-LFPRM + CR-LFPRM-ALT.    
01205                                                                   
01206      IF (CR-ENTRY-STATUS NOT = '9' AND                            
01207         CR-IND-GRP = '1' AND                                      
01208         CR-LF-CEX-CCYY = RUN-CCYY AND                             
01209         GR-REIN NOT = 'R')                                        
01210          IF CR-LF-TERM LESS THAN +60                              
01211              SUBTRACT CR-LFRFND FROM TOT-LF-UNDER60               
01212          ELSE                                                     
01213              SUBTRACT CR-LFRFND FROM TOT-LF-OVER59.               
01214                                                                   
01215      MOVE CLAS-I-RL-AH (CLAS-INDEXL) TO CP-BENEFIT-TYPE.          
01216      MOVE CLAS-I-BAL (CLAS-INDEXL)   TO CP-SPECIAL-CALC-CD.       
01217      MOVE CR-LF-TERM                 TO CP-ORIGINAL-TERM          
01218                                         CP-LOAN-TERM.             
01219                                                                   
01220      IF ((CLAS-I-EP (CLAS-INDEXL) EQUAL 'B' OR 'K' OR 'L') AND    
01221         CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT EQUAL 'L')             
01222          ADD +1                      TO CP-ORIGINAL-TERM          
01223                                         CP-LOAN-TERM.             
01224                                                                   
01225 *    IF CP-TRUNCATED-LIFE                                         
01226 *        MOVE CR-LOAN-TERM           TO CP-ORIGINAL-TERM.         
01227      IF CP-TERM-IS-DAYS                                           
01228          MOVE CR-LF-TERM-IN-DAYS     TO CP-TERM-OR-EXT-DAYS       
01229      ELSE                                                         
01230          MOVE ZEROS                  TO CP-TERM-OR-EXT-DAYS.      
01231                                                                   
01232      MOVE DTE-REM-TRM-CALC-OPTION    TO CP-REM-TRM-CALC-OPTION.   
01233                                                                   
01234      PERFORM 0410-GET-REMAINING-TERM THRU 0419-REM-TERM-EXIT.     
01235                                                                   
01236      IF ((CLAS-I-EP (CLAS-INDEXL) EQUAL 'B' OR 'K' OR 'L') AND    
01237             CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT EQUAL 'L')         
01238          MOVE CP-REMAINING-TERM-2        TO LF-BAL-REMTERM        
01239          COMPUTE CP-REMAINING-TERM-1 = CP-REMAINING-TERM-1 - 1    
01240          COMPUTE CP-REMAINING-TERM-2 = CP-REMAINING-TERM-2 - 1.   
01241                                                                   
01242      IF CP-REMAINING-TERM-1 NEGATIVE                              
01243          MOVE ZEROS                  TO CP-REMAINING-TERM-1.      
01244                                                                   
01245      IF CP-REMAINING-TERM-2 NEGATIVE                              
01246          MOVE ZEROS                  TO CP-REMAINING-TERM-2.      
01247                                                                   
01248      MOVE CP-REMAINING-TERM-1        TO LF-REM-TRM1.              
01249      MOVE CP-REMAINING-TERM-2        TO LF-REM-TRM2.              
01250                                                                   
01251  0170-CALC-REM-TERM-AH.                                           
01252                                                                   
01253      IF CR-AHTYP = ZEROS                                          
01254          MOVE ZEROS                  TO AH-REM-TRM1               
01255                                         AH-REM-TRM2               
01256          GO TO 0179-CALC-REM-TERM-EXIT.                           
01257                                                                   
01258      IF (CR-ENTRY-STATUS NOT = ('9' AND '5') AND                  
01259         CR-IND-GRP = '1' AND                                      
01260         CR-ENTRY-CCYY = RUN-CCYY AND                              
01261         GR-REIN NOT = 'R')                                        
01262          IF CR-AH-TERM LESS THAN +60                              
01263              ADD CR-AHPRM            TO TOT-AH-UNDER60            
01264          ELSE                                                     
01265              ADD CR-AHPRM            TO TOT-AH-OVER59.            
01266                                                                   
01267      IF (CR-ENTRY-STATUS NOT = '9' AND                            
01268         CR-IND-GRP = '1' AND                                      
01269         CR-AH-CEX-CCYY = RUN-CCYY AND                             
01270         GR-REIN NOT = 'R')                                        
01271          IF CR-AH-TERM LESS THAN +60                              
01272              SUBTRACT CR-AHRFND FROM TOT-AH-UNDER60               
01273          ELSE                                                     
01274              SUBTRACT CR-AHRFND FROM TOT-AH-OVER59.               
01275                                                                   
01276      IF CR-LFTYP NOT = ZEROS                                      
01277          IF CR-AH-TERM = CR-LF-TERM                               
01278              MOVE CP-REMAINING-TERM-1    TO AH-REM-TRM1           
01279              MOVE CP-REMAINING-TERM-2    TO AH-REM-TRM2           
01280              GO TO 0179-CALC-REM-TERM-EXIT.                       
01281                                                                   
01282      MOVE CLAS-I-RL-AH (CLAS-INDEXA) TO CP-BENEFIT-TYPE.          
01283      MOVE CLAS-I-BAL (CLAS-INDEXA)   TO CP-SPECIAL-CALC-CD.       
01284      MOVE CR-AH-TERM                 TO CP-ORIGINAL-TERM          
01285                                         CP-LOAN-TERM.             
01286      MOVE ZEROS                      TO CP-TERM-OR-EXT-DAYS.      
01287                                                                   
01288      MOVE DTE-REM-TRM-CALC-OPTION    TO CP-REM-TRM-CALC-OPTION.   
01289                                                                   
01290      PERFORM 0410-GET-REMAINING-TERM THRU 0419-REM-TERM-EXIT.     
01291                                                                   
01292      MOVE CP-REMAINING-TERM-1        TO AH-REM-TRM1.              
01293      MOVE CP-REMAINING-TERM-2        TO AH-REM-TRM2.              
01294                                                                   
01295  0179-CALC-REM-TERM-EXIT.                                         
01296      EXIT.                                                        
01297                                                                   
01298  EJECT                                                            
01299  0200-SET-UP-ALPHA-GAAP.                                          
01300                                                                   
01301      MOVE INITIALIZED-ALPHA-RECORD   TO ALPHA-RECORD.             
01302      PERFORM LOAD-ALPHA-WS-DATES.                                 
01303                                                                   
01304      MOVE INITIALIZED-GAAP-RECORD    TO GAAP-RECORD.              
01305      PERFORM LOAD-GAAP-WS-DATES.                                  
01306                                                                   
01307      MOVE DTE-CLASIC-COMPANY-CD      TO AX-COMPANY-CD             
01308                                         GR-COMPANY-CD.            
01309      MOVE CR-FULL-CONTROL            TO AX-CONTROL                
01310                                         GR-CONTROL.               
01311                                                                   
01312      MOVE CR-DT                      TO GR-EFF                    
01313                                         WS-GR-EFF-N               
01314                                         AX-DT                     
01315                                         WS-AX-DT-N.               
01316                                                                   
01317      MOVE CR-APR                     TO AX-APR                    
01318                                         GR-APR.                   
01319      MOVE CR-IND-GRP                 TO AX-IND-GRP                
01320                                         GR-IG.                    
01321      MOVE CR-PMT-FREQ                TO AX-PMT-FREQ               
01322                                         GR-PMT-FREQ.              
01323                                                                   
01324      MOVE CR-ENTRY-STATUS            TO AX-ENTRY-STATUS           
01325                                         GR-CERT-STATUS.           
01326      MOVE CR-ENTRY-DATE              TO AX-ENTRY                  
01327                                         WS-AX-ENTRY-N             
01328                                         GR-ENT-DT                 
01329                                         WS-GR-ENT-DT-N.           
01330                                                                   
01331      MOVE CR-NAME                    TO AX-NAME.                  
01332      MOVE CR-AGE                     TO AX-AGE.                   
01333      MOVE CR-SEX                     TO AX-SEX                    
01334                                         GR-SEX-CODE.              
01335                                                                   
01336      MOVE CR-REIN-SPEC               TO AX-SPEC-REIN.             
01337      MOVE CR-REIN-TABLE              TO AX-REIN-TABLE.            
01338                                                                   
01339      IF DTE-CLIENT = 'NCL'                                        
01340          MOVE AM-CSR-CODE            TO AX-MEM-NO                 
01341      ELSE                                                         
01342          MOVE CR-MEMBER-NO           TO AX-MEM-NO.                
01343                                                                   
01344      MOVE CR-SOC-SEC                 TO AX-SOC-NO.                
01345      MOVE CR-CAN-MICROFILM-NO        TO AX-CAN-MICROFILM-NO       
01346      MOVE CR-ISS-MICROFILM-NO        TO AX-ISS-MICROFILM-NO       
01347                                                                   
01348      MOVE 'P'                        TO GR-REIN.                  
01349                                                                   
01350      IF CR-LOAN-TERM NUMERIC                                      
01351          MOVE CR-LOAN-TERM           TO GR-LOAN-TERM              
01352      ELSE                                                         
01353          MOVE ZEROS                  TO GR-LOAN-TERM.             
01354      MOVE CR-AGE                     TO GR-AGE                    
01355                                         GR-MORT-AGE.              
01356      MOVE AM-EXPIRE-DT               TO GR-ACC-EXPIRES.           
01357                                                                   
01358      MOVE AM-REPORT-CODE-2           TO GR-REGION.                
01359      MOVE AM-POOL-PRIME              TO GR-POOL-PRIME.            
01360                                                                   
01361      MOVE CR-UNDERWRITING-CODE       TO GR-UNDERWRITING-CODE.     
01362                                                                   
01363      IF CR-LFTYP = ZERO                                           
01364           GO TO 0220-ALPHA-GAAP-A-H-DATA.                         
01365                                                                   
01366      IF DTE-CLIENT = 'DAC'                                        
01367          IF CLAS-I-JOINT (CLAS-INDEXL) = 'J'  AND                 
01368             CR-AGE LESS THAN CR-JOINT-AGE                         
01369              IF CR-SEX = 'M'                                      
01370                  COMPUTE GR-AGE = CR-JOINT-AGE - +6               
01371                  MOVE GR-AGE         TO GR-MORT-AGE               
01372              ELSE                                                 
01373                  COMPUTE GR-AGE = CR-JOINT-AGE + +6               
01374                  MOVE GR-AGE         TO GR-MORT-AGE.              
01375                                                                   
01376  0208-CSL-JOINT-EQUAL-AGE.                                        
01377                                                                   
01378      IF DTE-CLIENT = 'CSL'  AND                                   
01379         CLAS-I-JOINT (CLAS-INDEXL) = 'J'  AND                     
01380         CR-JOINT-AGE NOT = ZEROS                                  
01381          NEXT SENTENCE                                            
01382      ELSE                                                         
01383          GO TO 0210-ALPHA-GAAP-LIFE-DATA.                         
01384                                                                   
01385      IF CR-AGE NOT LESS THAN CR-JOINT-AGE                         
01386          COMPUTE AGE-DIFFERENCE = CR-AGE - CR-JOINT-AGE           
01387      ELSE                                                         
01388          COMPUTE AGE-DIFFERENCE = CR-JOINT-AGE - CR-AGE.          
01389                                                                   
01390      IF AGE-DIFFERENCE LESS THAN 2    MOVE 0 TO AGE-DIFFERENCE    
01391      ELSE                                                         
01392      IF AGE-DIFFERENCE LESS THAN 4    MOVE 1 TO AGE-DIFFERENCE    
01393      ELSE                                                         
01394      IF AGE-DIFFERENCE LESS THAN 6    MOVE 2 TO AGE-DIFFERENCE    
01395      ELSE                                                         
01396      IF AGE-DIFFERENCE LESS THAN 9    MOVE 3 TO AGE-DIFFERENCE    
01397      ELSE                                                         
01398      IF AGE-DIFFERENCE LESS THAN 12   MOVE 4 TO AGE-DIFFERENCE    
01399      ELSE                                                         
01400      IF AGE-DIFFERENCE LESS THAN 17   MOVE 5 TO AGE-DIFFERENCE    
01401      ELSE                                                         
01402      IF AGE-DIFFERENCE LESS THAN 23   MOVE 6 TO AGE-DIFFERENCE    
01403      ELSE                                                         
01404          MOVE 7 TO AGE-DIFFERENCE.                                
01405                                                                   
01406      IF CR-AGE NOT LESS THAN CR-JOINT-AGE                         
01407          COMPUTE GR-AGE = CR-AGE - AGE-DIFFERENCE                 
01408      ELSE                                                         
01409          COMPUTE GR-AGE = CR-JOINT-AGE - AGE-DIFFERENCE.          
01410                                                                   
01411      MOVE GR-AGE                     TO GR-MORT-AGE.              
01412                                                                   
01413  0210-ALPHA-GAAP-LIFE-DATA.                                       
01414                                                                   
01415      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'B'                      
01416          MOVE '*'                    TO O-B-SWITCH.               
01417      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'Z'                      
01418          MOVE 'Z'                    TO GR-SUMMARY-FLAG           
01419          ADD CR-SUM-CAN-CNT-ITD      TO TOT-CANC-LF               
01420          ADD CR-LFPRM                TO TOT-CANC-LF-AMT           
01421          ADD CR-LFPRM-ALT            TO TOT-CANC-LF-AMT           
01422          MOVE '*'                    TO SUMMARY-SWITCH.           
01423                                                                   
01424      IF CR-ENTRY-STATUS NOT = ('9' AND '5' AND 'D' AND 'V')       
01425         COMPUTE TOT-LF-AMT  = TOT-LF-AMT  +                       
01426                                       CR-LFPRM + CR-LFPRM-ALT     
01427         IF SUMMARY-CERTIFICATE                                    
01428            ADD CR-LIVES            TO TOT-LF                      
01429         ELSE                                                      
01430            ADD +1                  TO TOT-LF.                     
01431                                                                   
01432      MOVE CR-LFTYP                   TO AX-LF-TYP                 
01433                                         GR-LFTYP.                 
01434      MOVE CR-LF-TERM                 TO AX-LF-TERM                
01435                                         GR-LF-TERM.               
01436      MOVE LF-REM-TRM1                TO GR-LF-UP-REMTERM.         
01437      MOVE LF-REM-TRM2                TO AX-LF-REMTERM             
01438                                         GR-LF-REMTERM.            
01439      MOVE CR-LFAMT                   TO AX-LF-AMT.                
01440      ADD CR-LFAMT                    TO GR-LFBEN.                 
01441      MOVE CR-LFPRM                   TO AX-LF-PRM.                
01442      ADD CR-LFPRM                    TO GR-LFPRM.                 
01443                                                                   
01444      IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')             
01445          MOVE CR-LFAMT-ALT           TO AX-LF-AMT-ALT             
01446          ADD CR-LFAMT-ALT            TO GR-LFBEN                  
01447          MOVE CR-LFPRM-ALT           TO AX-LF-PRM-ALT             
01448          ADD CR-LFPRM-ALT            TO GR-LFPRM.                 
01449                                                                   
01450      MOVE GR-LFBEN                   TO GR-DIR-CERT-LFBEN.        
01451                                                                   
01452      MOVE CR-LFRFND                  TO AX-LF-REFUND.             
01453      MOVE CR-DTHAMT                  TO AX-LF-CLAIM-PMTS.         
01454                                                                   
01455      MOVE CR-LF-CURRENT-STATUS       TO AX-LF-STATUS.             
01456                                                                   
01457      MOVE CR-LF-EXPIRE-DATE          TO AX-LF-EXPIRES             
01458                                         WS-AX-LF-EXPIRES-N        
01459                                         GR-LF-EXPIRE-DATE         
01460                                         WS-GR-LF-EXPIRE-DATE-N.   
01461                                                                   
01462      IF CR-POLICY-IS-VOID OR CR-POLICY-IS-DECLINED                
01463          GO TO 0220-ALPHA-GAAP-A-H-DATA.                          
01464                                                                   
01465      IF (AX-LF-STATUS = '7' AND                                   
01466          CR-ENTRY-STATUS NOT EQUAL '5' AND '9')                   
01467          ADD +1                      TO TOT-DTH                   
01468          ADD  CR-LFPRM               TO TOT-DTH-AMT               
01469          ADD  CR-LFPRM-ALT           TO TOT-DTH-AMT.              
01470                                                                   
01471      IF AX-LF-STATUS = '7'                                        
01472          MOVE CR-DTH-DT              TO AX-DEATH                  
01473                                         WS-AX-DEATH-N             
01474          MOVE CR-LF-CLAIM-EXIT-DATE  TO AX-LF-EXIT                
01475                                         WS-AX-LF-EXIT-N           
01476            GO TO 0220-ALPHA-GAAP-A-H-DATA.                        
01477                                                                   
01478      IF  CR-POLICY-IS-REISSUE OR CR-POLICY-IS-REIN-ONLY OR        
01479          SUMMARY-CERTIFICATE                                      
01480              NEXT SENTENCE                                        
01481      ELSE                                                         
01482          IF (AX-LF-STATUS = '6'  OR  '8' )                        
01483              ADD +1                  TO TOT-CANC-LF               
01484              ADD  CR-LFPRM           TO TOT-CANC-LF-AMT           
01485              ADD  CR-LFPRM-ALT       TO TOT-CANC-LF-AMT.          
01486                                                                   
01487      IF AX-LF-STATUS = '6'  OR  '8'                               
01488          MOVE CR-LF-CANC-DT          TO AX-LF-CNCL                
01489                                         WS-AX-LF-CNCL-N           
01490          MOVE CR-LF-CANCEL-EXIT-DATE TO AX-LF-EXIT                
01491                                         WS-AX-LF-EXIT-N           
01492            GO TO 0220-ALPHA-GAAP-A-H-DATA.                        
01493                                                                   
01494      IF CR-ENTRY-STATUS EQUAL '5' OR '9'                          
01495          NEXT SENTENCE                                            
01496      ELSE                                                         
01497      IF ((CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')  AND       
01498          CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L'  AND            
01499          LF-BAL-REMTERM NOT GREATER THAN ZERO)                    
01500                        OR                                         
01501         ((CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')  AND       
01502          CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'L'  AND                
01503          AX-LF-REMTERM NOT GREATER THAN ZERO)                     
01504                        OR                                         
01505         ((CLAS-I-EP (CLAS-INDEXL) NOT = 'B' AND 'K' AND 'L') AND  
01506          AX-LF-REMTERM NOT GREATER ZERO)                          
01507          ADD +1                      TO TOT-EXP-LF                
01508          ADD CR-LFPRM                TO TOT-EXP-LF-AMT            
01509          ADD CR-LFPRM-ALT            TO TOT-EXP-LF-AMT            
01510          IF O-B-CERTIFICATE                                       
01511             MOVE '*'                 TO O-B-LF-CNT-SW.            
01512                                                                   
01513      IF ((CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')   AND      
01514          CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L'  AND            
01515          LF-BAL-REMTERM NOT GREATER THAN ZERO)                    
01516                        OR                                         
01517         ((CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')  AND       
01518          CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'L'  AND                
01519          AX-LF-REMTERM NOT GREATER THAN ZERO)                     
01520                        OR                                         
01521         ((CLAS-I-EP (CLAS-INDEXL) NOT = 'B' AND 'K' AND 'L') AND  
01522          AX-LF-REMTERM NOT GREATER ZERO AND                       
01523          NOT O-B-CERTIFICATE)                                     
01524          MOVE ZERO                   TO AX-LF-REMTERM             
01525          MOVE 'E'                    TO AX-LF-STATUS              
01526            GO TO 0220-ALPHA-GAAP-A-H-DATA.                        
01527                                                                   
01528      IF (AX-LF-REMTERM GREATER AX-LF-TERM                         
01529         AND CR-ENTRY-STATUS NOT EQUAL '5' AND '9')                
01530          ADD +1                      TO TOT-FUTR-LF               
01531          ADD CR-LFPRM                TO TOT-FUTR-LF-AMT           
01532          ADD CR-LFPRM-ALT            TO TOT-FUTR-LF-AMT.          
01533                                                                   
01534      IF AX-LF-REMTERM GREATER AX-LF-TERM                          
01535          MOVE AX-LF-TERM             TO AX-LF-REMTERM             
01536                                         LF-REM-TRM1               
01537                                         LF-REM-TRM2               
01538          MOVE 'F'                    TO AX-LF-STATUS              
01539                                         GR-FLAG                   
01540            GO TO 0220-ALPHA-GAAP-A-H-DATA.                        
01541                                                                   
01542      IF (DTE-CLIENT EQUAL 'NCL') AND                              
01543         (O-B-CERTIFICATE)                                         
01544         NEXT SENTENCE                                             
01545      ELSE                                                         
01546         GO TO 0215-CONTINUE-ALPHA.                                
01547                                                                   
01548      IF (CR-CCYY LESS THAN WS-OB-CCYY (3)) OR                     
01549         (CR-CCYY EQUAL WS-OB-CCYY (3) AND                         
01550         CR-MO LESS THAN WS-OB-MO (3))                             
01551         GO TO 0215-CONTINUE-ALPHA.                                
01552                                                                   
01553      PERFORM VARYING SUB FROM +1 BY +1                            
01554        UNTIL CR-LFTYP EQUAL WS-LIFE-OB-CODE (1 SUB) OR            
01555        SUB GREATER THAN +20                                       
01556      END-PERFORM.                                                 
01557                                                                   
01558      IF SUB GREATER THAN +20                                      
01559         GO TO 0215-CONTINUE-ALPHA.                                
01560                                                                   
01561      IF (CR-CCYY EQUAL WS-OB-CCYY (1)) AND                        
01562         (CR-MO EQUAL WS-OB-MO (1))                                
01563         MOVE '*'          TO WS-OB-DATE-1-SWITCH                  
01564         ADD CR-LFPRM TO WS-LIFE-PREM (1 SUB)                      
01565         ADD CR-LFPRM-ALT TO WS-LIFE-PREM (1 SUB)                  
01566         ADD CR-LFAMT TO WS-LIFE-BEN (1 SUB)                       
01567         ADD CR-LFAMT-ALT TO WS-LIFE-BEN (1 SUB)                   
01568      ELSE                                                         
01569      IF (CR-CCYY EQUAL WS-OB-CCYY (2)) AND                        
01570         (CR-MO EQUAL WS-OB-MO (2))                                
01571         MOVE '*'          TO WS-OB-DATE-2-SWITCH                  
01572         ADD CR-LFPRM TO WS-LIFE-PREM (2 SUB)                      
01573         ADD CR-LFPRM-ALT TO WS-LIFE-PREM (2 SUB)                  
01574         ADD CR-LFAMT TO WS-LIFE-BEN (2 SUB)                       
01575         ADD CR-LFAMT-ALT TO WS-LIFE-BEN (2 SUB)                   
01576      ELSE                                                         
01577      IF (CR-CCYY EQUAL WS-OB-CCYY (3)) AND                        
01578         (CR-MO EQUAL WS-OB-MO (3))                                
01579         MOVE '*'          TO WS-OB-DATE-3-SWITCH                  
01580         ADD CR-LFPRM TO WS-LIFE-PREM (3 SUB)                      
01581         ADD CR-LFPRM-ALT TO WS-LIFE-PREM (3 SUB)                  
01582         ADD CR-LFAMT TO WS-LIFE-BEN (3 SUB)                       
01583         ADD CR-LFAMT-ALT TO WS-LIFE-BEN (3 SUB).                  
01584                                                                   
01585  0215-CONTINUE-ALPHA.                                             
01586                                                                   
01587      IF CR-ENTRY-STATUS EQUAL '5' OR '9'                          
01588         NEXT SENTENCE                                             
01589      ELSE                                                         
01590         IF O-B-CERTIFICATE                                        
01591             IF EXCLUDE-O-B-LF                                     
01592                 MOVE SPACES      TO O-B-LF-CNT-SW                 
01593             ELSE                                                  
01594                 ADD CR-LFPRM     TO TOT-ACT-LF-AMT-OB             
01595                 ADD CR-LFPRM-ALT TO TOT-ACT-LF-AMT-OB             
01596                 ADD +1           TO TOT-ACTIVE-LF-OB              
01597         ELSE                                                      
01598             ADD CR-LFPRM         TO TOT-ACTIVE-LF-AMT             
01599             ADD CR-LFPRM-ALT     TO TOT-ACTIVE-LF-AMT             
01600             IF SUMMARY-CERTIFICATE                                
01601                 COMPUTE TOT-ACTIVE-LF = TOT-ACTIVE-LF +           
01602                                    CR-LIVES - CR-SUM-CAN-CNT-ITD  
01603             ELSE                                                  
01604                 ADD +1           TO TOT-ACTIVE-LF.                
01605                                                                   
01606      IF CR-ENTRY-STATUS = '5'  OR                                 
01607         O-B-CERTIFICATE                                           
01608          GO TO 0220-ALPHA-GAAP-A-H-DATA.                          
01609                                                                   
01610      IF SUMMARY-CERTIFICATE                                       
01611          COMPUTE GR-CNT-LF = CR-LIVES - CR-SUM-CAN-CNT-ITD.       
01612                                                                   
01613      IF NOT SUMMARY-CERTIFICATE                                   
01614          MOVE +1                         TO GR-CNT-LF.            
01615                                                                   
01616  0220-ALPHA-GAAP-A-H-DATA.                                        
01617                                                                   
01618      IF CR-AHTYP = ZERO                                           
01619          GO TO 0230-CHECK-INFORCE-STATUS.                         
01620                                                                   
01621      IF CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'B'                      
01622          MOVE '*'                    TO O-B-SWITCH.               
01623      IF CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'Z'                      
01624          MOVE 'Z'                    TO GR-SUMMARY-FLAG           
01625          ADD CR-SUM-CAN-CNT-ITD      TO TOT-CANC-AH               
01626          ADD CR-AHPRM                TO TOT-CANC-AH-AMT           
01627          MOVE '*'                    TO SUMMARY-SWITCH.           
01628                                                                   
01629      IF CR-ENTRY-STATUS NOT = ('9' AND '5' AND 'D' AND 'V')       
01630          ADD CR-AHPRM                TO TOT-AH-AMT                
01631          IF SUMMARY-CERTIFICATE                                   
01632            ADD CR-LIVES              TO TOT-AH                    
01633          ELSE                                                     
01634            ADD +1                    TO TOT-AH.                   
01635                                                                   
01636      MOVE CR-AHTYP                   TO AX-AH-TYP                 
01637                                         GR-AHTYP.                 
01638      MOVE CR-AH-TERM                 TO AX-AH-TERM                
01639                                         GR-AH-TERM.               
01640      MOVE AH-REM-TRM1                TO GR-AH-UP-REMTERM.         
01641      MOVE AH-REM-TRM2                TO AX-AH-REMTERM             
01642                                         GR-AH-REMTERM.            
01643      MOVE CR-AHAMT                   TO AX-AH-AMT                 
01644                                         GR-AHBEN.                 
01645                                                                   
01646      MOVE CR-AHPRM                   TO AX-AH-PRM                 
01647                                         GR-AHPRM.                 
01648      MOVE CR-AHRFND                  TO AX-AH-REFUND.             
01649      MOVE CR-DISAMT                  TO AX-AH-CLAIM-PMTS.         
01650                                                                   
01651      MOVE CR-AH-CURRENT-STATUS       TO AX-AH-STATUS.             
01652                                                                   
01653      MOVE CR-AH-EXPIRE-DATE          TO AX-AH-EXPIRES             
01654                                         WS-AX-AH-EXPIRES-N        
01655                                         GR-AH-EXPIRE-DATE         
01656                                         WS-GR-AH-EXPIRE-DATE-N.   
01657                                                                   
01658      IF CR-DIS-PAY-DT NOT = ZEROS                                 
01659          MOVE CR-DIS-PAY-DT          TO AX-LUMP-SUM               
01660                                         WS-AX-LUMP-SUM-N          
01661          MOVE '6'                    TO AX-AH-PRE-PLST.           
01662                                                                   
01663      IF CR-POLICY-IS-VOID OR CR-POLICY-IS-DECLINED                
01664          GO TO 0230-CHECK-INFORCE-STATUS.                         
01665                                                                   
01666      IF (AX-AH-STATUS = '6'                                       
01667          AND CR-ENTRY-STATUS NOT EQUAL '5' AND '9')               
01668          ADD +1                          TO TOT-DIS               
01669          ADD CR-AHPRM                    TO TOT-DIS-AMT.          
01670                                                                   
01671      IF AX-AH-STATUS = '6'                                        
01672          MOVE CR-DIS-DT                  TO AX-LUMP-SUM           
01673                                             WS-AX-LUMP-SUM-N      
01674          MOVE CR-AH-SETTLEMENT-EXIT-DATE TO AX-AH-EXIT            
01675                                             WS-AX-AH-EXIT-N       
01676          GO TO 0230-CHECK-INFORCE-STATUS.                         
01677                                                                   
01678      IF  CR-POLICY-IS-REISSUE OR CR-POLICY-IS-REIN-ONLY OR        
01679          SUMMARY-CERTIFICATE                                      
01680              NEXT SENTENCE                                        
01681      ELSE                                                         
01682          IF AX-AH-STATUS = '7' OR '8'                             
01683              ADD +1                      TO TOT-CANC-AH           
01684              ADD CR-AHPRM                TO TOT-CANC-AH-AMT.      
01685                                                                   
01686      IF AX-AH-STATUS = '7'  OR  '8'                               
01687          MOVE CR-AH-CANC-DT          TO AX-AH-CNCL                
01688                                         WS-AX-AH-CNCL-N           
01689          MOVE CR-AH-CANCEL-EXIT-DATE TO AX-AH-EXIT                
01690                                         WS-AX-AH-EXIT-N           
01691          GO TO 0230-CHECK-INFORCE-STATUS.                         
01692                                                                   
01693      IF (AX-AH-REMTERM NOT GREATER ZERO)                          
01694          AND (CR-ENTRY-STATUS NOT EQUAL '5' AND '9')              
01695          ADD +1                      TO TOT-EXP-AH                
01696          ADD CR-AHPRM                TO TOT-EXP-AH-AMT            
01697          IF O-B-CERTIFICATE                                       
01698              MOVE '*' TO O-B-AH-CNT-SW.                           
01699                                                                   
01700      IF (AX-AH-REMTERM NOT GREATER ZERO) AND                      
01701         (NOT O-B-CERTIFICATE)                                     
01702          MOVE ZERO                   TO AX-AH-REMTERM             
01703          MOVE 'E'                    TO AX-AH-STATUS              
01704          GO TO 0230-CHECK-INFORCE-STATUS.                         
01705                                                                   
01706      IF (AX-AH-REMTERM GREATER AX-AH-TERM)                        
01707          AND (CR-ENTRY-STATUS NOT EQUAL '5' AND '9')              
01708          ADD +1                      TO TOT-FUTR-AH               
01709          ADD CR-AHPRM                TO TOT-FUTR-AH-AMT.          
01710                                                                   
01711      IF AX-AH-REMTERM GREATER AX-AH-TERM                          
01712          MOVE AX-AH-TERM             TO AX-AH-REMTERM             
01713                                         AH-REM-TRM1               
01714                                         AH-REM-TRM2               
01715          MOVE 'F'                    TO AX-AH-STATUS              
01716                                         GR-FLAG                   
01717          GO TO 0230-CHECK-INFORCE-STATUS.                         
01718                                                                   
01719                                                                   
01720      IF (DTE-CLIENT EQUAL 'NCL') AND                              
01721         (O-B-CERTIFICATE)                                         
01722         NEXT SENTENCE                                             
01723      ELSE                                                         
01724         GO TO 0225-CONTINUE-ALPHA.                                
01725                                                                   
01726      IF (CR-CCYY LESS THAN WS-OB-CCYY (3)) OR                     
01727         (CR-CCYY EQUAL WS-OB-CCYY (3) AND                         
01728         CR-MO LESS THAN WS-OB-MO (3))                             
01729         GO TO 0225-CONTINUE-ALPHA.                                
01730                                                                   
01731      PERFORM VARYING SUB FROM +1 BY +1                            
01732        UNTIL CR-AHTYP EQUAL WS-AH-OB-CODE (1 SUB) OR              
01733        SUB GREATER THAN +20                                       
01734      END-PERFORM.                                                 
01735                                                                   
01736      IF SUB GREATER THAN +20                                      
01737         GO TO 0225-CONTINUE-ALPHA.                                
01738                                                                   
01739      IF (CR-CCYY EQUAL WS-OB-CCYY (1)) AND                        
01740         (CR-MO EQUAL WS-OB-MO (1))                                
01741         MOVE '*'          TO WS-OB-DATE-1-SWITCH                  
01742         ADD CR-AHPRM TO WS-AH-PREM (1 SUB)                        
01743         ADD CR-AHAMT TO WS-AH-BEN (1 SUB)                         
01744      ELSE                                                         
01745      IF (CR-CCYY EQUAL WS-OB-CCYY (2)) AND                        
01746         (CR-MO EQUAL WS-OB-MO (2))                                
01747         MOVE '*'          TO WS-OB-DATE-2-SWITCH                  
01748         ADD CR-AHPRM TO WS-AH-PREM (2 SUB)                        
01749         ADD CR-AHAMT TO WS-AH-BEN (2 SUB)                         
01750      ELSE                                                         
01751      IF (CR-CCYY EQUAL WS-OB-CCYY (3)) AND                        
01752         (CR-MO EQUAL WS-OB-MO (3))                                
01753         MOVE '*'          TO WS-OB-DATE-3-SWITCH                  
01754         ADD CR-AHPRM TO WS-AH-PREM (3 SUB)                        
01755         ADD CR-AHAMT TO WS-AH-BEN (3 SUB).                        
01756                                                                   
01757  0225-CONTINUE-ALPHA.                                             
01758                                                                   
01759      IF CR-ENTRY-STATUS EQUAL '5' OR '9'                          
01760         NEXT SENTENCE                                             
01761      ELSE                                                         
01762         IF O-B-CERTIFICATE                                        
01763             IF EXCLUDE-O-B-AH                                     
01764                 MOVE SPACES      TO O-B-AH-CNT-SW                 
01765             ELSE                                                  
01766                 ADD CR-AHPRM     TO TOT-ACT-AH-AMT-OB             
01767                 ADD +1           TO TOT-ACTIVE-AH-OB              
01768         ELSE                                                      
01769             ADD CR-AHPRM         TO TOT-ACTIVE-AH-AMT             
01770             IF SUMMARY-CERTIFICATE                                
01771                 COMPUTE TOT-ACTIVE-AH = TOT-ACTIVE-AH +           
01772                                    CR-LIVES - CR-SUM-CAN-CNT-ITD  
01773             ELSE                                                  
01774                 ADD +1           TO TOT-ACTIVE-AH.                
01775                                                                   
01776                                                                   
01777      IF CR-ENTRY-STATUS = '5'  OR                                 
01778         O-B-CERTIFICATE                                           
01779          GO TO 0230-CHECK-INFORCE-STATUS.                         
01780                                                                   
01781      IF SUMMARY-CERTIFICATE                                       
01782          COMPUTE GR-CNT-AH = CR-LIVES - CR-SUM-CAN-CNT-ITD.       
01783                                                                   
01784      IF NOT SUMMARY-CERTIFICATE                                   
01785          MOVE +1                     TO GR-CNT-AH.                
01786                                                                   
01787  0230-CHECK-INFORCE-STATUS.                                       
01788                                                                   
01789      IF O-B-CERTIFICATE                                           
01790          ADD +1                      TO TOT-OB                    
01791          MOVE ZERO                   TO AX-LF-REMTERM             
01792                                         AX-AH-REMTERM             
01793          MOVE 'OUTSTANDING BAL'      TO AX-NAME                   
01794          PERFORM 0600-WRITE-ALPHA-EXTRACT THRU 0699-EXIT          
01795          GO TO 0110-READ-CERT-MASTER                              
01796      ELSE                                                         
01797          IF SUMMARY-CERTIFICATE                                   
01798              ADD +1                  TO TOT-SUM                   
01799              MOVE 'SUMMARY CERT.'    TO AX-NAME                   
01800          ELSE                                                     
01801              ADD +1                  TO TOT-SNGLPRM.              
01802                                                                   
01803      IF (AX-ENTRY-STATUS = '5'  OR 'D' OR 'V') OR                 
01804         ((AX-LF-STATUS = ' '  OR  'E'  OR  '6'  OR  '7'  OR  '8') 
01805                 AND                                               
01806          (AX-AH-STATUS = ' '  OR  'E'  OR  '6'  OR  '7'  OR  '8'))
01807          PERFORM 6000-PRINT-EXCLUSIONS THRU 6999-PRT-EXCLUSIONS-X 
01808          PERFORM 0600-WRITE-ALPHA-EXTRACT THRU 0699-EXIT          
01809          GO TO 0110-READ-CERT-MASTER.                             
01810                                                                   
01811      IF AX-LF-STATUS = 'E' OR '6' OR '7' OR '8'                   
01812          MOVE ZEROS                  TO  GR-LFTYP                 
01813                                          GR-LFBEN                 
01814                                          GR-LFPRM                 
01815                                          GR-LF-TERM               
01816                                          GR-LF-REMTERM            
01817                                          GR-LF-UP-REMTERM.        
01818      IF AX-AH-STATUS = 'E' OR '6' OR '7' OR '8'                   
01819          MOVE ZEROS                  TO  GR-AHTYP                 
01820                                          GR-AHBEN                 
01821                                          GR-AHPRM                 
01822                                          GR-AH-TERM               
01823                                          GR-AH-REMTERM            
01824                                          GR-AH-UP-REMTERM.        
01825                                                                   
01826  0240-SUMMARY-ADJUSTMENTS.                                        
01827                                                                   
01828      IF SUMMARY-CERTIFICATE                                       
01829          COMPUTE GR-CNT = CR-LIVES - CR-SUM-CAN-CNT-ITD           
01830      ELSE                                                         
01831          MOVE +1                     TO GR-CNT                    
01832          GO TO 0260-CALCULATE-PREMIUM-RESERVE.                    
01833                                                                   
01834      IF CR-LFTYP = ZERO                                           
01835          GO TO 0250-A-H-SUMMARY-ADJ.                              
01836                                                                   
01837      MOVE +1                         TO WS-SUM-LF-FACTOR.         
01838                                                                   
01839      IF CR-LFPRM NOT GREATER CR-LFRFND                            
01840          MOVE ZERO                   TO GR-LFBEN                  
01841          MOVE ZERO                   TO GR-LFPRM                  
01842          MOVE +0                     TO WS-SUM-LF-FACTOR          
01843      ELSE                                                         
01844          IF CR-LFRFND = ZERO                                      
01845              MOVE CR-LFAMT           TO GR-LFBEN                  
01846              MOVE CR-LFPRM           TO GR-LFPRM                  
01847          ELSE                                                     
01848              COMPUTE TEMP-3 ROUNDED = CR-LFRFND / CR-LFPRM        
01849              COMPUTE GR-LFBEN ROUNDED =                           
01850                               CR-LFAMT - (CR-LFAMT * TEMP-3)      
01851              COMPUTE WS-SUM-LF-FACTOR = GR-LFBEN / CR-LFAMT       
01852              COMPUTE GR-LFPRM ROUNDED =                           
01853                               CR-LFPRM - CR-LFRFND.               
01854                                                                   
01855      MOVE GR-LFPRM                   TO CR-LFPRM.                 
01856                                                                   
01857  0250-A-H-SUMMARY-ADJ.                                            
01858                                                                   
01859      IF CR-AHTYP = ZERO                                           
01860          GO TO 0260-CALCULATE-PREMIUM-RESERVE.                    
01861                                                                   
01862      MOVE +1                         TO WS-SUM-AH-FACTOR.         
01863                                                                   
01864      IF CR-AHPRM NOT GREATER CR-AHRFND                            
01865          MOVE ZERO                   TO GR-AHBEN                  
01866          MOVE ZERO                   TO GR-AHPRM                  
01867          MOVE +0                     TO WS-SUM-AH-FACTOR          
01868      ELSE                                                         
01869          IF CR-AHRFND = ZERO                                      
01870              MOVE CR-AHAMT           TO GR-AHBEN                  
01871              MOVE CR-AHPRM           TO GR-AHPRM                  
01872          ELSE                                                     
01873              COMPUTE TEMP-3 ROUNDED = CR-AHRFND / CR-AHPRM        
01874              COMPUTE GR-AHBEN ROUNDED =                           
01875                               CR-AHAMT - (CR-AHAMT * TEMP-3)      
01876              COMPUTE WS-SUM-AH-FACTOR = GR-AHBEN / CR-AHAMT       
01877              COMPUTE GR-AHPRM ROUNDED =                           
01878                               CR-AHPRM - CR-AHRFND.               
01879                                                                   
01880      MOVE GR-AHPRM                   TO CR-AHPRM.                 
01881                                                                   
01882  0260-CALCULATE-PREMIUM-RESERVE.                                  
01883                                                                   
01884      MOVE CR-LFPRM                   TO LF-PRM.                   
01885      MOVE CR-LFPRM-ALT               TO LF-PRM-ALT.               
01886      MOVE SPACE                      TO CP-REIN-FLAG.             
01887                                                                   
01888      PERFORM 1000-CALCULATE-UNEARNED THRU 1099-CALC-UNEARNED-X.   
01889                                                                   
01890      IF (STATE-ABBR (CLAS-INDEXS) EQUAL 'VA')  AND                
01891         (CR-AH-TERM GREATER THAN +61) AND                         
01892         (CR-DT  GREATER  THAN  19921231 )                         
01893           MOVE 'A' TO AH-EARN-METHOD.                             
01894                                                                   
01895      IF (GR-AHTYP NOT = ZERO)       AND                           
01896         (CLAS-I-CALC-TYPE (CLAS-INDEXA) NOT = 'B' AND 'Z') AND    
01897         (AH-EARN-METHOD = 'A' OR 'C')                             
01898          PERFORM 3000-RE-RATE-AH-ROUTINE THRU 3099-EXIT           
01899          IF CP-ERROR-RATE-NOT-FOUND  OR                           
01900             CP-ERROR-RATE-IS-ZERO  OR                             
01901             CP-ERROR-IN-DATES                                     
01902              NEXT SENTENCE                                        
01903          ELSE                                                     
01904              COMPUTE GRS-AHPRM ROUNDED =                          
01905                                  ((GR-AH-UP-REMTERM * GR-AHBEN)   
01906                                  / +100) * CP-PREMIUM-RATE        
01907              COMPUTE GRD-AHPRM ROUNDED =                          
01908                                  ((GR-AH-UP-REMTERM * GR-AHBEN)   
01909                                  / +100) * CP-PREMIUM-RATE.       
01910                                                                   
01911                                                                   
01912  0300-ESTABLISH-MORT-BASIS.                                       
01913                                                                   
01914      MOVE CR-MORT                    TO GR-MORT-CODE.             
01915                                                                   
01916      IF CLAS-MORT-OVRD NOT = SPACES                               
01917          MOVE CLAS-MORT-OVRD         TO GR-MORT-CODE.             
01918                                                                   
01919      MOVE '0'                        TO GR-MORT-TYP.              
01920                                                                   
01921      IF (DTE-CLIENT = 'POS') AND                                  
01922         (GR-CARRIER EQUAL '3' OR '5')                             
01923         MOVE 'G250'              TO GR-MORT-CODE.                 
01924                                                                   
01925      IF DTE-CLIENT = 'BOA'                                        
01926         MOVE 'I350'              TO GR-MORT-CODE.                 
01927                                                                   
01928      IF DTE-CLIENT = 'CVL'                                        
01929         MOVE 'I350'              TO GR-MORT-CODE.                 
01930                                                                   
01931      IF (DTE-CLIENT = 'FLA')  AND                                 
01932         (GR-CARRIER EQUAL '0')                                    
01933          IF (GR-STATE = '36')  AND                                
01934             (GR-EFF GREATER THAN 19901231)                        
01935              MOVE 'S350'         TO GR-MORT-CODE                  
01936          ELSE                                                     
01937          IF (GR-STATE = '09')  AND                                
01938             (GR-EFF GREATER THAN 19900930)                        
01939              MOVE 'S350'         TO GR-MORT-CODE.                 
01940                                                                   
01941      IF DTE-CLIENT = 'CRI'                                        
01942         MOVE 'G350'              TO GR-MORT-CODE.                 
01943                                                                   
01944      IF DTE-CLIENT = 'MIL'                                        
01945         MOVE 'G350'              TO GR-MORT-CODE.                 
01946                                                                   
CIDMOD     IF DTE-CLIENT = 'CID'
CIDMOD        NEXT SENTENCE
CIDMOD      ELSE
CIDMOD        GO TO 0300-CID-MORT-DONE.                                 
CIDMOD
CIDMOD     IF GR-YR = 94
CIDMOD        MOVE 'L990' TO GR-MORT-CODE
CIDMOD        GO TO 0300-CID-MORT-DONE.                                 
CIDMOD
CIDMOD      IF GR-YR = 95 OR 96
CIDMOD        MOVE 'L960' TO GR-MORT-CODE
CIDMOD        GO TO 0300-CID-MORT-DONE.                                 
CIDMOD
CIDMOD      IF GR-YR = 97
CIDMOD        MOVE 'L680' TO GR-MORT-CODE
CIDMOD        GO TO 0300-CID-MORT-DONE.                                 
CIDMOD
CIDMOD 0300-CID-MORT-DONE.                                              
CIDMOD
01947      IF DTE-CLIENT = 'NCB'                                        
01948        IF GR-CARRIER = 'T'                                        
01949            MOVE 'F520'               TO GR-MORT-CODE              
01950        ELSE                                                       
01951          IF (GR-CARRIER = 'G' OR '2' OR '3' OR '4')  AND          
01952              GR-CCYY LESS THAN 1994                               
01953              MOVE 'A300'             TO GR-MORT-CODE              
01954          ELSE                                                     
01955              IF GR-CCYY GREATER 1993                              
01956                  MOVE 'J520'         TO GR-MORT-CODE              
01957              ELSE                                                 
01958                  IF GR-CCYY GREATER 1986                          
01959                      MOVE 'L650'     TO GR-MORT-CODE              
01960                  ELSE                                             
01961                      IF GR-CCYY GREATER THAN 1983                 
01962                          MOVE 'L720' TO GR-MORT-CODE.             
01963                                                                   
01964      IF DTE-CLIENT = 'POS' AND CR-CARRIER = '2'                   
01965          IF GR-CCYY GREATER 1976 AND GR-GROUPING = '000030'       
01966              MOVE '2350'             TO GR-MORT-CODE.             
01967                                                                   
01968      IF DTE-CLIENT = 'IST'                                        
01969         IF CR-LF-TERM GREATER 59                                  
01970             MOVE 'D550'              TO GR-MORT-CODE              
01971         ELSE                                                      
01972             MOVE 'F550'              TO GR-MORT-CODE.             
01973                                                                   
01974      IF DTE-CLIENT = 'FIM'                                        
01975          IF CR-SEX-FEMALE                                         
01976              MOVE 'F550'             TO GR-MORT-CODE              
01977          ELSE                                                     
01978              MOVE 'D550'             TO GR-MORT-CODE.             
01979                                                                   
01980      IF DTE-CLIENT = 'FIM'                                        
01981          IF GR-CARRIER = 'H'                                      
01982              MOVE 'D350'             TO GR-MORT-CODE.             
01983                                                                   
01984      IF DTE-CLIENT = 'SNR' AND                                    
01985         GR-CARRIER = '1' AND                                      
01986         CR-SEX = 'F'                                              
01987          MOVE '9350'                 TO GR-MORT-CODE.             
01988                                                                   
01989      IF DTE-CLIENT = 'SBG' AND                                    
01990         GR-EFF GREATER THAN 19820615                              
01991           MOVE 'F350'                TO GR-MORT-CODE.             
01992                                                                   
01993      IF DTE-CLIENT = 'HAN' OR 'JHL'                               
01994        IF GR-CCYR = 1994  OR  1995                                
01995            IF CR-SEX-FEMALE                                       
01996                MOVE 'R550'           TO GR-MORT-CODE              
01997            ELSE                                                   
01998                MOVE 'S550'           TO GR-MORT-CODE              
01999        ELSE                                                       
02000            IF CR-SEX-FEMALE                                       
02001                MOVE 'R600'           TO GR-MORT-CODE              
02002            ELSE                                                   
02003                MOVE 'S600'           TO GR-MORT-CODE.             
02004                                                                   
02005      IF DTE-CLIENT = 'NCL'                                        
02006          MOVE AM-REPORT-CODE-2       TO WORK-REGION               
02007          MOVE 'N450'                 TO GR-MORT-CODE              
02008          IF NOT A-CAPTIVE-ACCOUNT                                 
02009              IF GR-STATE = 'AK' OR 'AR' OR 'CO' OR 'DC' OR 'GA'   
02010                         OR 'IA' OR 'ID' OR 'IL' OR 'IN' OR 'KY'   
02011                         OR 'LA' OR 'MA' OR 'MD' OR 'ME' OR 'MI'   
02012                         OR 'MN' OR 'MT' OR 'NE' OR 'NH' OR 'NM'   
02013                         OR 'NV' OR 'SC' OR 'SD' OR 'UT' OR 'VT'   
02014                         OR 'WV' OR 'MS'                           
02015                         OR 'AL' OR 'AZ' OR 'CA' OR 'CT' OR 'FL'   
02016                         OR 'MO' OR 'ND' OR 'RI' OR 'WI' OR 'WY'   
02017                  MOVE 'W450'         TO GR-MORT-CODE.             
02018                                                                   
02019      IF DTE-ALT-MORT-CODE NOT = SPACES                            
02020          MOVE DTE-ALT-MORT-CODE      TO GR-ALT-MORT-CODE.         
02021                                                                   
02022      MOVE ZEROS                      TO GR-ALT-RESV.              
02023                                                                   
02024      IF DTE-CLIENT = 'GLC'                                        
02025          IF GR-CARRIER = '2'                                      
02026              MOVE 48                 TO GR-AGE  GR-MORT-AGE.      
02027                                                                   
02028      IF GR-LFTYP NOT = ZERO                                       
02029          PERFORM 2000-CALC-REM-AMT THRU 2999-CALC-REM-AMT-X.      
02030                                                                   
02031      IF DTE-CLIENT = 'CSL'                                        
02032         IF GR-LFTYP NOT = ZEROS  AND                              
02033            AM-MAX-TOT-BEN NOT = ZEROS                             
02034            IF GR-REM-AMT GREATER THAN AM-MAX-TOT-BEN              
02035                MOVE AM-MAX-TOT-BEN   TO GR-REM-AMT.               
02036                                                                   
02037      IF GR-AHTYP NOT = ZERO                                       
02038          COMPUTE GR-AH-REM-BEN ROUNDED = GR-AHBEN * GR-AH-REMTERM.
02039                                                                   
02040      IF DTE-CLIENT = 'CSL'                                        
02041         IF GR-AHTYP NOT = ZEROS  AND                              
02042            AM-MAX-MON-BEN NOT = ZEROS                             
02043            IF GR-AHBEN GREATER THAN AM-MAX-MON-BEN                
02044                COMPUTE GR-AH-REM-BEN ROUNDED =                    
02045                                 AM-MAX-MON-BEN * GR-AH-REMTERM.   
02046                                                                   
02047      MOVE GR-REM-AMT                 TO AX-LF-REMAMT.             
02048      MOVE GR-AH-REM-BEN              TO AX-AH-REMAMT.             
02049                                                                   
02050      PERFORM 0600-WRITE-ALPHA-EXTRACT THRU 0699-EXIT.             
02051                                                                   
02052      IF CR-ENTRY-STATUS = '9'                                     
02053          GO TO 0390-GENERATE-GAAP-RECORDS.                        
02054                                                                   
02055      ADD GR-LFBEN                    TO TOT-INIT-FACE.            
02056      ADD GR-REM-AMT                  TO TOT-REM-FACE.             
02057      ADD GR-AH-REM-BEN               TO TOT-REM-AH.               
02058      IF GR-FLAG = 'F'                                             
02059          ADD GR-LFBEN                TO TOT-FUTR-FACE.            
02060      ADD GR-MO-DEC                   TO TOT-MO-DEC.               
02061                                                                   
02062  0390-GENERATE-GAAP-RECORDS.                                      
02063                                                                   
02064      IF DTE-PGM-OPT = '1'                                         
02065          GO TO 0110-READ-CERT-MASTER.                             
02066                                                                   
02067      MOVE GAAP-RECORD                TO SAVE-GAAP-REC.            
02068      MOVE GR-REM-AMT                 TO SAVE-REMAINING-AMT.       
02069                                                                   
02070      IF CR-ENTRY-STATUS NOT = '9'                                 
02071          ADD +1                      TO TOT-GAAP                  
02072          PERFORM 0700-WRITE-GAAP-RECORD THRU 0799-WRITE-GAAP-X.   
02073                                                                   
02074      PERFORM 4000-REINSURE-ROUTINE THRU 4099-REINSURE-ROUTINE-X.  
02075                                                                   
02076      GO TO 0110-READ-CERT-MASTER.                                 
02077                                                                   
02078  EJECT                                                            
02079  0395-CHECK-OUT-OB.                                               
02080                                                                   
02081      MOVE SPACES                 TO O-B-SWITCH                    
02082                                     SUMMARY-SWITCH                
02083                                     GR-REIN.                      
02084                                                                   
02085      IF OB-DATE-1-USED  OR                                        
02086         OB-DATE-2-USED  OR                                        
02087         OB-DATE-3-USED                                            
02088          PERFORM 0397-BUILD-LF-GAAP THRU 0397-EXIT                
02089          VARYING SUB FROM +1 BY +1 UNTIL SUB GREATER THAN +20     
02090          PERFORM 0398-BUILD-AH-GAAP THRU 0398-EXIT                
02091          VARYING SUB FROM +1 BY +1 UNTIL SUB GREATER THAN +20.    
02092                                                                   
02093      MOVE SPACES                 TO WS-OB-DATE-1-SWITCH           
02094                                     WS-OB-DATE-2-SWITCH           
02095                                     WS-OB-DATE-3-SWITCH.          
02096                                                                   
02097  0395-EXIT.                                                       
02098      EXIT.                                                        
02099  0397-BUILD-LF-GAAP.                                              
02100                                                                   
02101 *    DISPLAY '*** BUILD GAAP ***'                                 
02102 *    DISPLAY '*** SUB *** ' SUB                                   
02103 *    DISPLAY '*LF PREM*1 ' WS-LIFE-PREM (1 SUB)                   
02104 *    DISPLAY '*LF PREM*2 ' WS-LIFE-PREM (2 SUB)                   
02105 *    DISPLAY '*LF PREM*3 ' WS-LIFE-PREM (3 SUB)                   
02106                                                                   
02107      IF OB-DATE-1-USED                                            
02108          IF WS-LIFE-PREM (1 SUB) GREATER THAN +0                  
02109              MOVE +1             TO SUBA                          
02110              GO TO 0397-BUILD-LF-CONTINUED                        
02111          ELSE                                                     
02112              GO TO 0397-EXIT.                                     
02113                                                                   
02114      IF OB-DATE-2-USED                                            
02115          IF WS-LIFE-PREM (2 SUB) GREATER THAN +0                  
02116              MOVE +2             TO SUBA                          
02117              GO TO 0397-BUILD-LF-CONTINUED                        
02118          ELSE                                                     
02119              GO TO 0397-EXIT.                                     
02120                                                                   
02121      IF OB-DATE-3-USED                                            
02122          IF WS-LIFE-PREM (3 SUB) GREATER THAN +0                  
02123              MOVE +3             TO SUBA                          
02124              GO TO 0397-BUILD-LF-CONTINUED                        
02125          ELSE                                                     
02126              GO TO 0397-EXIT.                                     
02127                                                                   
02128  0397-BUILD-LF-CONTINUED.                                         
02129                                                                   
02130      MOVE INITIALIZED-GAAP-RECORD TO GAAP-RECORD.                 
02131      PERFORM LOAD-GAAP-WS-DATES.                                  
02132                                                                   
02133      MOVE 'P'                    TO GR-REIN.                      
02134                                                                   
02135      MOVE DTE-CLASIC-COMPANY-CD  TO GR-COMPANY-CD.                
02136      MOVE AM-CONTROL-A           TO GR-CONTROL.                   
02137                                                                   
02138      MOVE AM-REPORT-CODE-2       TO GR-REGION.                    
02139      MOVE AM-POOL-PRIME          TO GR-POOL-PRIME.                
02140                                                                   
02141      MOVE WS-NCL-OB-DATES (SUBA) TO WS-GR-EFF-N.                  
02142                                                                   
02143      MOVE 01                     TO GR-DA                         
02144      MOVE WS-GR-EFF-N            TO GR-EFF.                       
02145                                                                   
02146      MOVE 'LF'                   TO WS-OB-1-2.                    
02147      MOVE WS-LIFE-OB-CODE (SUBA SUB)                              
02148                                  TO WS-OB-3-4.                    
02149      MOVE WS-NCL-OB-CERT-NO      TO GR-CERT-NO.                   
02150                                                                   
02151      MOVE SPACES                 TO GR-REIN-COMP.                 
02152                                                                   
02153      MOVE +42                    TO GR-AGE                        
02154                                     GR-MORT-AGE.                  
02155                                                                   
02156      MOVE +1                     TO GR-LF-TERM                    
02157                                     GR-LF-REMTERM                 
02158                                     GR-LF-UP-REMTERM.             
02159                                                                   
02160      MOVE 'ZER0'                 TO GR-MORT-CODE.                 
02161      MOVE +1                     TO GR-CNT GR-CNT-LF.             
02162      MOVE WS-NCL-OB-DATES (SUBA) TO GR-ENT-DT                     
02163                                     WS-GR-ENT-DT-N                
02164                                     GR-LF-EXPIRE-DATE             
02165                                     WS-GR-LF-EXPIRE-DATE-N.       
02166                                                                   
02167      MOVE WS-LIFE-PREM (SUBA SUB)                                 
02168                                  TO GR-LFPRM.                     
02169      MOVE WS-LIFE-BEN (SUBA SUB) TO GR-LFBEN.                     
02170      MOVE WS-LIFE-OB-CODE (SUBA SUB)                              
02171                                  TO GR-LFTYP.                     
02172      COMPUTE GRR-LFPRM EQUAL GR-LFPRM * .5.                       
02173      MOVE GRR-LFPRM              TO GRP-LFPRM                     
02174                                     GRS-LFPRM                     
02175                                     GRD-LFPRM.                    
02176                                                                   
02177      COMPUTE GR-REM-AMT EQUAL GR-LFBEN * .5.                      
02178                                                                   
02179      ADD GR-LFBEN                    TO TOT-INIT-FACE.            
02180      ADD GR-REM-AMT                  TO TOT-REM-FACE.             
02181                                                                   
02182      MOVE '*' TO GR-OB-IND.                                       
02183                                                                   
02184      MOVE GAAP-RECORD            TO SAVE-GAAP-REC.                
02185      MOVE GR-REM-AMT             TO SAVE-REMAINING-AMT.           
02186      ADD +1                      TO TOT-GAAP.                     
02187      PERFORM 0700-WRITE-GAAP-RECORD THRU 0799-WRITE-GAAP-X.       
02188                                                                   
02189      IF AM-REI-TABLE EQUAL SPACES OR ZEROS                        
02190         GO TO 0397-EXIT.                                          
02191                                                                   
02192      MOVE GR-CONTROL             TO CR-FULL-CONTROL.              
02193      MOVE GR-LFTYP               TO CR-LFTYP.                     
02194      MOVE +1                     TO CR-LF-TERM.                   
02195      MOVE GR-LFBEN               TO CR-LFAMT.                     
02196      MOVE GR-LFPRM               TO CR-LFPRM CR-LFPRM-CALC.       
02197      MOVE ZEROS                  TO CR-LFAMT-ALT CR-LFPRM-ALT     
02198                                     CR-AHTYP CR-AHAMT             
02199                                     CR-AHPRM CR-AHPRM-CALC.       
02200      MOVE AM-IG                  TO CR-IND-GRP.                   
02201      MOVE +42                    TO CR-AGE.                       
02202      MOVE AM-REI-TABLE           TO CR-REIN-TABLE.                
02203                                                                   
02204      MOVE '*'                    TO O-B-SWITCH.                   
02205                                                                   
02206      MOVE CLAS-STARTL            TO CLAS-INDEXL.                  
CIDMOD     PERFORM 0130-FIND-LIFE-LOOP THRU 0130-LIFE-LOOP-EXIT.        
CIDMOD***  PERFORM 0130-FIND-LIFE-LOOP.                                 
02208                                                                   
02209      PERFORM 4000-REINSURE-ROUTINE THRU 4099-REINSURE-ROUTINE-X.  
02210                                                                   
02211  0397-EXIT.                                                       
02212      EXIT.                                                        
02213  0398-BUILD-AH-GAAP.                                              
02214                                                                   
02215      IF OB-DATE-1-USED                                            
02216          IF WS-AH-PREM (1 SUB) GREATER THAN +0                    
02217              MOVE +1             TO SUBA                          
02218              GO TO 0398-BUILD-AH-CONTINUED                        
02219          ELSE                                                     
02220              GO TO 0398-EXIT.                                     
02221                                                                   
02222      IF OB-DATE-2-USED                                            
02223          IF WS-AH-PREM (2 SUB) GREATER THAN +0                    
02224              MOVE +2             TO SUBA                          
02225              GO TO 0398-BUILD-AH-CONTINUED                        
02226          ELSE                                                     
02227              GO TO 0398-EXIT.                                     
02228                                                                   
02229      IF OB-DATE-3-USED                                            
02230          IF WS-AH-PREM (3 SUB) GREATER THAN +0                    
02231              MOVE +3             TO SUBA                          
02232              GO TO 0398-BUILD-AH-CONTINUED                        
02233          ELSE                                                     
02234              GO TO 0398-EXIT.                                     
02235                                                                   
02236  0398-BUILD-AH-CONTINUED.                                         
02237                                                                   
02238      MOVE INITIALIZED-GAAP-RECORD TO GAAP-RECORD.                 
02239      PERFORM LOAD-GAAP-WS-DATES.                                  
02240                                                                   
02241      MOVE 'P'                    TO GR-REIN.                      
02242                                                                   
02243      MOVE DTE-CLASIC-COMPANY-CD  TO GR-COMPANY-CD.                
02244      MOVE AM-CONTROL-A           TO GR-CONTROL.                   
02245                                                                   
02246      MOVE AM-REPORT-CODE-2       TO GR-REGION.                    
02247      MOVE AM-POOL-PRIME          TO GR-POOL-PRIME.                
02248                                                                   
02249      MOVE WS-NCL-OB-DATES (SUBA) TO WS-GR-EFF-N.                  
02250                                                                   
02251      MOVE 01                     TO GR-DA                         
02252      MOVE WS-GR-EFF-N            TO GR-EFF.                       
02253                                                                   
02254      MOVE 'AH'                   TO WS-OB-1-2.                    
02255      MOVE WS-AH-OB-CODE (SUBA SUB)                                
02256                                  TO WS-OB-3-4.                    
02257      MOVE WS-NCL-OB-CERT-NO      TO GR-CERT-NO.                   
02258                                                                   
02259      MOVE SPACES                 TO GR-REIN-COMP.                 
02260                                                                   
02261      MOVE +42                    TO GR-AGE                        
02262                                     GR-MORT-AGE.                  
02263                                                                   
02264      MOVE +1                     TO GR-AH-TERM                    
02265                                     GR-AH-REMTERM                 
02266                                     GR-AH-UP-REMTERM.             
02267                                                                   
02268      MOVE 'ZER0'                 TO GR-MORT-CODE.                 
02269      MOVE +1                     TO GR-CNT GR-CNT-AH.             
02270      MOVE WS-NCL-OB-DATES (SUBA) TO GR-ENT-DT                     
02271                                     WS-GR-ENT-DT-N                
02272                                     GR-AH-EXPIRE-DATE             
02273                                     WS-GR-AH-EXPIRE-DATE-N.       
02274                                                                   
02275      MOVE WS-AH-PREM (SUBA SUB)  TO GR-AHPRM.                     
02276                                                                   
02277      MOVE WS-AH-BEN (SUBA SUB)   TO GR-AHBEN.                     
02278      MOVE WS-AH-OB-CODE (SUBA SUB)                                
02279                                  TO GR-AHTYP.                     
02280      COMPUTE GRR-AHPRM EQUAL GR-AHPRM * .5.                       
02281      MOVE GRR-AHPRM              TO GRP-AHPRM                     
02282                                     GRS-AHPRM                     
02283                                     GRD-AHPRM.                    
02284                                                                   
02285      COMPUTE GR-AH-REM-BEN EQUAL GR-AHBEN * .5.                   
02286                                                                   
02287      ADD GR-AH-REM-BEN               TO TOT-REM-AH.               
02288                                                                   
02289      MOVE '*' TO GR-OB-IND.                                       
02290                                                                   
02291      MOVE GAAP-RECORD            TO SAVE-GAAP-REC.                
02292      ADD +1                      TO TOT-GAAP.                     
02293      PERFORM 0700-WRITE-GAAP-RECORD THRU 0799-WRITE-GAAP-X.       
02294                                                                   
02295      IF AM-REI-TABLE EQUAL SPACES OR ZEROS                        
02296         GO TO 0398-EXIT.                                          
02297                                                                   
02298      MOVE GR-CONTROL             TO CR-FULL-CONTROL.              
02299      MOVE GR-AHTYP               TO CR-AHTYP.                     
02300      MOVE +1                     TO CR-AH-TERM.                   
02301      MOVE GR-AHBEN               TO CR-AHAMT.                     
02302      MOVE GR-AHPRM               TO CR-AHPRM CR-AHPRM-CALC.       
02303      MOVE ZEROS                  TO CR-LFTYP CR-LFAMT             
02304                                     CR-LFAMT-ALT                  
02305                                     CR-LFPRM CR-LFPRM-CALC.       
02306      MOVE AM-IG                  TO CR-IND-GRP.                   
02307      MOVE +42                    TO CR-AGE.                       
02308      MOVE AM-REI-TABLE           TO CR-REIN-TABLE.                
02309                                                                   
02310      MOVE '*'                    TO O-B-SWITCH.                   
02311                                                                   
02312      MOVE CLAS-STARTA            TO CLAS-INDEXA.                  
02313      PERFORM 0150-FIND-AH-LOOP.                                   
02314                                                                   
02315      PERFORM 4000-REINSURE-ROUTINE THRU 4099-REINSURE-ROUTINE-X.  
02316                                                                   
02317  0398-EXIT.                                                       
02318      EXIT.                                                        
02319                                                                   
02320  0400-DATE-CONVERSION-ROUTINE.                                    
02321                                                                   
SUNPSD     COPY SUNDCS.                                                 
SUNPSD*****COPY ELCDCS.                                                 
02323                                                                   
02324  0409-EXIT.                                                       
02325      EXIT.                                                        
02326                                                                   
02327                                                                   
02328  0410-GET-REMAINING-TERM.                                         
02329                                                                   
SUNPSD     ADD 1 TO ELRTRMX-CTR ALLCALL-CTR.
SUNPSD     CALL 'SUNTRMX' USING CALCULATION-PASS-AREA. 
SUNPSD*****CALL 'ELRTRMX' USING CALCULATION-PASS-AREA.                  
02331                                                                   
02332      IF DTE-CLIENT = 'NCL'                                        
02333          MOVE CP-REMAINING-TERM-1    TO CP-REMAINING-TERM-2.      
02334                                                                   
02335  0419-REM-TERM-EXIT.                                              
02336      EXIT.                                                        
02337                                                                   
02338                                                                   
02339  0500-MATCH-TO-ACC.                                               
02340                                                                   
02341      MOVE SPACE TO AH-EARN-METHOD.                                
02342                                                                   
02343      IF AM-CONTROL-A LESS CR-ACCT-CONTROL                         
02344          PERFORM 0590-READ-AM-MSTR THRU 0598-EXIT                 
02345          GO TO 0500-MATCH-TO-ACC.                                 
02346                                                                   
02347      IF AM-CONTROL-A GREATER CR-ACCT-CONTROL                      
02348          GO TO 0550-AM-MSTR-ERROR.                                
02349                                                                   
02350      IF CR-DT NOT LESS AM-EXPIRE-DT                               
02351          PERFORM 0590-READ-AM-MSTR THRU 0598-EXIT                 
02352          GO TO 0500-MATCH-TO-ACC.                                 
02353                                                                   
02354      IF CR-DT LESS AM-EFFECT-DT                                   
02355          GO TO 0550-AM-MSTR-ERROR.                                
02356                                                                   
PEMMOD*    IF AM-EARN-METHOD-A NOT = SPACES                             
PEMMOD*        MOVE AM-EARN-METHOD-A TO AH-EARN-METHOD.                 
02359                                                                   
CIDMOD     MOVE  'YYYYYYYYYY'           TO  WS-CHARGEBACK-LEVELS        
CIDMOD                                                                  
CIDMOD     PERFORM VARYING SUBB FROM +1 BY +1 UNTIL
CIDMOD        SUBB > +10
CIDMOD        IF AM-COMM-CHARGEBACK (SUBB) NOT NUMERIC
CIDMOD           MOVE ZEROS TO AM-COMM-CHARGEBACK (SUBB)
CIDMOD        END-IF
CIDMOD        IF AM-COMM-CHARGEBACK (SUBB) = ZEROS
CIDMOD           CONTINUE
CIDMOD        ELSE
CIDMOD           IF AM-COMM-CHARGEBACK (SUBB) = '99'
CIDMOD              MOVE 'N' TO WS-CHARGEBACK-SW (SUBB)
CIDMOD           END-IF
CIDMOD        END-IF
CIDMOD     END-PERFORM
CIDMOD
02360      GO TO 0599-EXIT.                                             
02361                                                                   
02362  0550-AM-MSTR-ERROR.                                              
02363                                                                   
02364      DISPLAY 'NO ACCOUNT MASTER ' CR-FULL-CONTROL.                
02365      DISPLAY 'NO ACCOUNT MASTER ' CR-FULL-CONTROL UPON CONSOLE.   
02366                                                                   
02367      IF DTE-CLIENT = 'ELT' OR 'FMK' OR 'XXX' OR 'PLI' OR          
02368                      'ADL' OR 'DEF' OR 'GIC' OR 'FIA'             
02369          GO TO 0599-EXIT.                                         
02370                                                                   
02371      MOVE 0302 TO WS-RETURN-CODE                                  
02372      GO TO ABEND-PGM.                                             
02373                                                                   
02374  0590-READ-AM-MSTR.                                               
02375                                                                   
02376      READ ERACCTT                                                 
02377          IF ERACCTT-FILE-STATUS = '10'                            
02378          MOVE HIGH-VALUES TO AM-MSTR-CNTRL                        
02379          GO TO 0598-EXIT.                                         
02380                                                                   
02381      IF ERACCTT-FILE-STATUS NOT = '00'                            
02382          MOVE ERACCTT-FILE-STATUS TO W-ABEND-CODE-2               
02383          MOVE WS-ABEND-CODE TO WS-RETURN-CODE                     
02384          MOVE 'READ ERROR - ERACCTT' TO WS-ABEND-MESSAGE          
02385            GO TO ABEND-PGM.                                       
02386                                                                   
02387 *****COPY ELCACCTI.                                               
02388                                                                   
PEMMOD*    IF AM-REI-LF-TAX NOT NUMERIC                                 
PEMMOD*        MOVE ZEROS                  TO AM-REI-LF-TAX.            
PEMMOD*    IF AM-REI-AH-TAX NOT NUMERIC                                 
PEMMOD*        MOVE ZEROS                  TO AM-REI-AH-TAX.            
02393      IF AM-MAX-TOT-BEN NOT NUMERIC                                
02394          MOVE ZEROS                  TO AM-MAX-TOT-BEN.           
02395      IF AM-MAX-MON-BEN NOT NUMERIC                                
02396          MOVE ZEROS                  TO AM-MAX-MON-BEN.           
02397                                                                   
02398  0598-EXIT.                                                       
02399      EXIT.                                                        
02400                                                                   
02401  0599-EXIT.                                                       
02402      EXIT.                                                        
02403                                                                   
02404                                                                   
02405  0600-WRITE-ALPHA-EXTRACT.                                        
02406                                                                   
02407      IF DTE-PGM-OPT = ('1' OR '3')                                
02408          NEXT SENTENCE                                            
02409      ELSE                                                         
02410          GO TO 0699-EXIT.                                         
02411                                                                   
02412      IF REIN-ALPHA-SW EQUAL 'R'                                   
02413          MOVE SPACES                 TO REIN-ALPHA-SW             
02414          MOVE 'R'                    TO AX-ALPHA-TYPE-CODE        
02415          MOVE CR-NAME                TO AX-NAME                   
02416          MOVE CR-AGE                 TO AX-AGE                    
02417          GO TO 0610-CONTINUE.                                     
02418                                                                   
02419      IF SUMMARY-CERTIFICATE                                       
02420          MOVE 'S'                    TO AX-ALPHA-TYPE-CODE        
02421      ELSE                                                         
02422          IF O-B-CERTIFICATE                                       
02423              MOVE 'O'                TO AX-ALPHA-TYPE-CODE        
02424      ELSE                                                         
02425              MOVE 'I'                TO AX-ALPHA-TYPE-CODE.       
02426                                                                   
02427  0610-CONTINUE.                                                   
02428                                                                   
02429      MOVE ALPHA-RECORD TO SAVE-ALPHA-REC.                         
02430                                                                   
02431      WRITE ALPHA-EXTRACT-RECORD FROM ALPHA-RECORD.                
02432                                                                   
02433      IF AX-REIN-ALPHA                                             
02434          NEXT SENTENCE                                            
02435      ELSE                                                         
02436          ADD +1                      TO TOT-ALPHA.                
02437                                                                   
02438      IF SUMMARY-CERTIFICATE                                       
02439          GO TO 0699-EXIT.                                         
02440                                                                   
02441      IF O-B-CERTIFICATE                                           
02442          GO TO 0699-EXIT.                                         
02443                                                                   
02444      IF CR-JOINT-NAME EQUAL SPACES OR LOW-VALUES                  
02445          GO TO 0699-EXIT.                                         
02446                                                                   
02447      IF JOINT-LF-COVERAGE OR JOINT-AH-COVERAGE                    
02448          NEXT SENTENCE                                            
02449      ELSE                                                         
02450          GO TO 0699-EXIT.                                         
02451                                                                   
02452      MOVE CR-JOINT-NAME              TO AX-NAME.                  
02453      MOVE CR-JOINT-AGE               TO AX-AGE.                   
02454                                                                   
02455      MOVE SPACES                     TO AX-SEX                    
02456                                         AX-SOC-NO.                
02457                                                                   
02458      IF NOT JOINT-LF-COVERAGE                                     
02459          MOVE SPACES                 TO AX-LF-TYP                 
02460          MOVE ZEROS                  TO AX-LF-TERM                
02461                                         AX-LF-REMTERM             
02462                                         AX-LF-AMT                 
02463                                         AX-LF-REMAMT              
02464                                         AX-LF-AMT-ALT             
02465                                         AX-LF-REMAMT-ALT          
02466                                         AX-LF-PRM                 
02467                                         AX-LF-PRM-ALT             
02468                                         AX-LF-REFUND              
02469                                         AX-LF-CLAIM-PMTS.         
02470                                                                   
02471      IF NOT JOINT-AH-COVERAGE                                     
02472          MOVE SPACES                 TO AX-AH-TYP                 
02473          MOVE ZEROS                  TO AX-AH-TERM                
02474                                         AX-AH-REMTERM             
02475                                         AX-AH-AMT                 
02476                                         AX-AH-REMAMT              
02477                                         AX-AH-PRM                 
02478                                         AX-AH-REFUND              
02479                                         AX-AH-CLAIM-PMTS.         
02480                                                                   
02481      IF AX-REIN-ALPHA                                             
02482          MOVE '9'                    TO AX-ALPHA-TYPE-CODE        
02483      ELSE                                                         
02484          MOVE 'J'                    TO AX-ALPHA-TYPE-CODE.       
02485                                                                   
02486      WRITE ALPHA-EXTRACT-RECORD FROM ALPHA-RECORD.                
02487                                                                   
02488      ADD +1                          TO TOT-ALPHA-JOINT.          
02489                                                                   
02490  0699-EXIT.                                                       
02491      EXIT.                                                        
02492                                                                   
02493                                                                   
02494  0700-WRITE-GAAP-RECORD.                                          
02495                                                                   
02496      IF GR-LFTYP = ZERO  AND  GR-AHTYP = ZERO                     
02497          GO TO 0799-WRITE-GAAP-X.                                 
02498                                                                   
02499 ******************************************************************
02500      IF GR-REIN = 'P'                                             
02501        IF GR-LFTYP NOT = ZEROS                                    
02502            IF GR-LF-REMTERM GREATER THAN OLDEST-LF-REMTERM        
02503                MOVE GR-LF-REMTERM   TO OLDEST-LF-REMTERM.         
02504                                                                   
02505      IF GR-REIN = 'P'                                             
02506        IF GR-LFTYP NOT = ZEROS                                    
02507            IF GR-LF-REMTERM GREATER THAN +100                     
02508                ADD 1 TO LF-OVER-100.                              
02509                                                                   
02510      IF GR-REIN = 'P'                                             
02511        IF GR-AHTYP NOT = ZEROS                                    
02512            IF GR-AH-REMTERM GREATER THAN OLDEST-AH-REMTERM        
02513                MOVE GR-AH-REMTERM   TO OLDEST-AH-REMTERM.         
02514 ******************************************************************
02515                                                                   
02516      IF DTE-PGM-OPT = ('2' OR '3')                                
02517          WRITE GAAP-EXTRACT-RECORD FROM GAAP-RECORD.              
02518                                                                   
02519  0799-WRITE-GAAP-X.                                               
02520      EXIT.                                                        
02521                                                                   
02522  EJECT                                                            
02523  0900-CALCULATE-COMMISSIONS.                                      
02524                                                                   
02525      MOVE +1                          TO SA.                      
02526      MOVE ZEROS                       TO GR-LFCOM                 
02527                                          GR-LFCOM-ALT             
02528                                          GR-AHCOM                 
CIDMOD                                         WS-GR-LFCOM
CIDMOD                                         WS-GR-LFCOM-ALT
CIDMOD                                         WS-GR-AHCOM
02529                                                                   
02530      MOVE ZEROS                       TO GR-LFSRV                 
02531                                          GR-LFSRV-ALT             
02532                                          GR-AHSRV.                
02533                                                                   
02534  0905-CALC-COMM-LOOP.                                             
02535                                                                   
02536      IF DTE-CLIENT = 'POS'                                        
02537          COMPUTE GR-LFCOM ROUNDED =                               
02538                        GR-LFCOM + (LF-PRM * CR-LCOM-L (SA))       
02539          COMPUTE GR-LFCOM-ALT ROUNDED =                           
02540                    GR-LFCOM-ALT + (LF-PRM-ALT * CR-LCOM-L (SA))   
02541          COMPUTE GR-AHCOM ROUNDED =                               
02542                        GR-AHCOM + (GR-AHPRM * CR-LCOM-AH (SA))    
02543          GO TO 0910-CALC-COMM-SKIP.                               
02544                                                                   
02545      IF GR-REIN = 'P' AND                                         
02546         CR-AGT-TYPE (SA) = ('C' OR 'D' OR 'O' OR 'P')             
02547      OR                                                           
02548         GR-REIN = 'R' AND                                         
02549         CR-AGT-TYPE (SA) = ('R' OR 'D' OR 'T' OR 'P')             
02550          COMPUTE GR-LFCOM =                                       
02551              GR-LFCOM + (LF-PRM * CR-LCOM-L (SA))                 
02552          COMPUTE GR-LFCOM-ALT =                                   
02553              GR-LFCOM-ALT + (LF-PRM-ALT * CR-LCOM-L (SA))         
02554          COMPUTE GR-AHCOM =                                       
02555              GR-AHCOM + (GR-AHPRM * CR-LCOM-AH (SA))              
CIDMOD         IF (WS-CHARGEBACK-SW (SA) = 'N') OR
CIDMOD            ((AM-COMM-CHARGEBACK (SA) NOT = ZEROS) AND
CIDMOD            (MONTHS-DIFF > AM-COMM-CHARGEBACK (SA)))
CIDMOD            CONTINUE
CIDMOD         ELSE
CIDMOD            COMPUTE WS-GR-LFCOM =                                 
CIDMOD                WS-GR-LFCOM + (LF-PRM * CR-LCOM-L (SA))           
CIDMOD            COMPUTE WS-GR-LFCOM-ALT =                             
CIDMOD                WS-GR-LFCOM-ALT + (LF-PRM-ALT * CR-LCOM-L (SA))   
CIDMOD            COMPUTE WS-GR-AHCOM =                                 
CIDMOD                WS-GR-AHCOM + (GR-AHPRM * CR-LCOM-AH (SA))        
CIDMOD         END-IF
CIDMOD     END-IF
02556                                                                   
02557      IF GR-REIN = 'P' AND                                         
02558         CR-AGT-TYPE (SA) = ('F' OR 'S' OR 'G' OR 'B')             
02559      OR                                                           
02560         GR-REIN = 'R' AND                                         
02561         CR-AGT-TYPE (SA) = ('U' OR 'S' OR 'V' OR 'B')             
02562          COMPUTE GR-LFSRV =                                       
02563              GR-LFSRV + (LF-PRM * CR-LCOM-L (SA))                 
02564          COMPUTE GR-LFSRV-ALT =                                   
02565              GR-LFSRV-ALT + (LF-PRM-ALT * CR-LCOM-L (SA))         
02566          COMPUTE GR-AHSRV =                                       
02567              GR-AHSRV + (GR-AHPRM * CR-LCOM-AH (SA)).             
02568                                                                   
02569      IF GR-REIN NOT = 'R' OR                                      
02570         CR-AGT-TYPE (SA) NOT = 'W'                                
02571          GO TO 0910-CALC-COMM-SKIP.                               
02572                                                                   
02573      MOVE CR-COM-AGT (SA) TO REIN-AGENT.                          
02574                                                                   
02575      IF RA-RCO NOT = GR-REIN-COMP                                 
02576          GO TO 0910-CALC-COMM-SKIP.                               
02577                                                                   
02578      COMPUTE GR-LFCOM =                                           
02579         GR-LFCOM + (LF-PRM * CR-LCOM-L (SA)).                     
02580      COMPUTE GR-LFCOM-ALT =                                       
02581         GR-LFCOM-ALT + (LF-PRM-ALT * CR-LCOM-L (SA)).             
02582      COMPUTE GR-AHCOM =                                           
02583         GR-AHCOM + (GR-AHPRM * CR-LCOM-AH (SA)).                  
02584                                                                   
02585  0910-CALC-COMM-SKIP.                                             
02586                                                                   
02587      ADD +1 TO SA.                                                
02588                                                                   
02589      IF SA LESS +11                                               
02590          GO TO 0905-CALC-COMM-LOOP.                               
02591                                                                   
02592      COMPUTE GR-LFCOM = GR-LFCOM + GR-LFCOM-ALT.                  
02593      COMPUTE GR-LFSRV = GR-LFSRV + GR-LFSRV-ALT.                  
02594                                                                   
02595  0999-CALC-COMM-X.                                                
02596      EXIT.                                                        
02597  EJECT                                                            
02598 ***************************************************************** 
02599 *  ECS050 WAS MODIFIED IN MAY,1989 TO 'CALL' A CENTRAL UNEARNED   
02600 *   PREMIUM MODULE.  IF ANY MODIFICATIONS TO UNEARNED PREMIUM     
02601 *   ARE NECESSARY, THEY SHOULD BE MADE IN 'ELUPRMX' TO STANDARDIZ 
02602 *   AND CENTRALIZE ALL UNEARNED PREMIUM CALCULATIONS.             
02603 ***************************************************************** 
02604  1000-CALCULATE-UNEARNED.                                         
02605                                                                   
02606      IF (DTE-CLIENT EQUAL 'NCL') AND                              
02607         (O-B-CERTIFICATE)                                         
02608         COMPUTE GRR-LFPRM EQUAL GR-LFPRM * .5                     
02609         MOVE GRR-LFPRM           TO GRP-LFPRM                     
02610                                     GRS-LFPRM                     
02611                                     GRD-LFPRM                     
02612         COMPUTE GRR-AHPRM EQUAL GR-AHPRM * .5                     
02613         MOVE GRR-AHPRM           TO GRP-AHPRM                     
02614                                     GRS-AHPRM                     
02615                                     GRD-AHPRM                     
02616         GO TO 1099-CALC-UNEARNED-X.                               
02617                                                                   
02618      PERFORM 0900-CALCULATE-COMMISSIONS THRU 0999-CALC-COMM-X.    
02619                                                                   
PEMMOD     IF CR-LF-ISS-PREM-TAX NUMERIC
PEMMOD        COMPUTE GR-LFTAX ROUNDED =
PEMMOD           GR-LFPRM * CR-LF-ISS-PREM-TAX
PEMMOD     END-IF
PEMMOD     IF CR-AH-ISS-PREM-TAX NUMERIC
PEMMOD        COMPUTE GR-AHTAX ROUNDED =
PEMMOD           GR-AHPRM * CR-AH-ISS-PREM-TAX
PEMMOD     END-IF
PEMMOD*    COMPUTE GR-LFTAX ROUNDED = GR-LFPRM * AM-REI-LF-TAX.         
PEMMOD*    COMPUTE GR-AHTAX ROUNDED = GR-AHPRM * AM-REI-AH-TAX.         
02622                                                                   
02623 ** IF THE REMAINING TERM IS GREATER THAN THE LIFE TERM, AN ERROR  
02624 ** HAS OCCURRED                                                   
02625                                                                   
02626      IF GR-FLAG = 'F'                                             
02627        MOVE GR-LFPRM              TO GRP-LFPRM                    
02628                                      GRR-LFPRM                    
02629                                      GRS-LFPRM                    
02630                                      GRD-LFPRM                    
02631        MOVE GR-LFCOM              TO GRP-LFCOM                    
02632                                      GRR-LFCOM                    
02633                                      GRS-LFCOM                    
02634                                      GRD-LFCOM                    
02635        MOVE GR-LFTAX              TO GRP-LFTAX                    
02636                                      GRR-LFTAX                    
02637        MOVE GR-AHPRM              TO GRP-AHPRM                    
02638                                      GRR-AHPRM                    
02639                                      GRS-AHPRM                    
02640                                      GRD-AHPRM                    
02641        MOVE GR-AHCOM              TO GRP-AHCOM                    
02642                                      GRR-AHCOM                    
02643                                      GRS-AHCOM                    
02644                                      GRD-AHCOM                    
02645        MOVE GR-AHTAX              TO GRP-AHTAX                    
02646                                      GRR-AHTAX                    
02647          GO TO 1099-CALC-UNEARNED-X.                              
02648                                                                   
02649                                                                   
02650  EJECT                                                            
02651  1001-CALCULATE-UNEARNED-LIFE.                                    
02652                                                                   
02653      IF GR-LFTYP = ZEROS                                          
02654          GO TO 1010-CALCULATE-UNEARNED-AH.                        
02655                                                                   
02656      IF GR-REIN = 'R'                                             
02657          IF RWF-LF-RUNOFF-SW = 'N'  AND                           
02658             VALUATION-DATE NOT LESS THAN RWF-EP-STOP-DATE         
02659              MOVE ZEROS              TO GR-LFTYP GR-LFBEN GR-LFPRM
02660              GO TO 1010-CALCULATE-UNEARNED-AH.                    
02661                                                                   
02662 *    MOVE GR-LFBEN                   TO CP-ORIGINAL-BENEFIT       
02663 *                                       CP-RATING-BENEFIT-AMT.    
02664      MOVE LF-PRM                     TO CP-ORIGINAL-PREMIUM.      
02665      MOVE GR-LF-TERM                 TO CP-ORIGINAL-TERM.         
02666      MOVE LF-REM-TRM1                TO CP-REMAINING-TERM.        
02667      MOVE CLAS-I-EP (CLAS-INDEXL)    TO CP-EARNING-METHOD.        
02668      MOVE LF-PRM-ALT                 TO CP-ALTERNATE-PREMIUM.     
02669      MOVE CLAS-I-RL-AH (CLAS-INDEXL) TO CP-BENEFIT-TYPE.          
02670      MOVE CLAS-I-CALC-TYPE (CLAS-INDEXL)                          
02671                                      TO CP-SPECIAL-CALC-CD.       
02672      MOVE CR-LFTYP                   TO CP-BENEFIT-CD.            
02673      MOVE LIFE-OVERRIDE-L1           TO CP-LIFE-OVERRIDE-CODE.    
02674                                                                   
02675      PERFORM 3100-CALC-UNEARNED-PREMIUM THRU 3199-EXIT.           
02676                                                                   
02677      IF CP-ERROR-OCCURED                                          
02678         DISPLAY CP-RETURN-CODE ' ERROR OCCUR '                    
02679         MOVE EXC-3                   TO EXP-MESSAGE               
02680         PERFORM 7600-EXCEP-REPT      THRU 7699-EXIT               
02681         GO TO 1009-EXIT.                                          
02682                                                                   
CIDMOD     IF GR-LFTYP = 'QD'                                           
CIDMOD        MOVE +15.0               TO CP-LOAN-APR                   
CIDMOD     END-IF                                                       
CIDMOD
02683      IF (CP-EARN-AS-NET-PAY  AND                                  
02684         CP-LOAN-APR = ZERO)                                       
02685          MOVE EXC-1                  TO EXP-MESSAGE               
02686          PERFORM 7600-EXCEP-REPT     THRU 7699-EXIT.              
02687                                                                   
02688      IF (CP-EARN-AS-NET-PAY  AND                                  
02689         CP-TRUNCATED-LIFE  AND                                    
02690         CP-LOAN-TERM LESS THAN CP-ORIGINAL-TERM)                  
02691          MOVE EXC-2                  TO EXP-MESSAGE               
02692          PERFORM 7600-EXCEP-REPT     THRU 7699-EXIT.              
02693                                                                   
02694                                                                   
02695      MOVE CP-R78-U-PRM               TO GRR-LFPRM.                
02696      MOVE CP-PRORATA-U-PRM           TO GRP-LFPRM.                
02697      MOVE CP-STATE-U-PRM             TO GRS-LFPRM.                
02698      MOVE CP-DOMICILE-U-PRM          TO GRD-LFPRM.                
02699                                                                   
02700      IF DTE-CLIENT = 'NCL'                                        
02701         IF CLAS-I-RL-AH (CLAS-INDEXL) = 'L'                       
02702            MOVE GRP-LFPRM        TO GRD-LFPRM                     
02703         ELSE                                                      
02704            IF GR-EFF GREATER THAN 19941231                        
02705               IF CP-EARN-AS-NET-PAY                               
02706                  MOVE GRS-LFPRM  TO GRD-LFPRM                     
02707               ELSE                                                
02708               IF CP-EARN-AS-REG-BALLOON                           
02709                  MOVE GRS-LFPRM  TO GRD-LFPRM                     
02710               ELSE                                                
02711                  MOVE GRR-LFPRM  TO GRD-LFPRM                     
02712            ELSE                                                   
02713               MOVE GRR-LFPRM     TO GRD-LFPRM.                    
02714                                                                   
CIDMOD     COMPUTE CP-ORIGINAL-PREMIUM = WS-GR-LFCOM - WS-GR-LFCOM-ALT. 
CIDMOD     MOVE WS-GR-LFCOM-ALT            TO CP-ALTERNATE-PREMIUM.     
LOGIC      MOVE CLAS-I-EP (CLAS-INDEXL)    TO CP-EARNING-METHOD.        
02717                                                                   
02718      PERFORM 3100-CALC-UNEARNED-PREMIUM THRU 3199-EXIT.           
02719                                                                   
02720      IF CP-ERROR-OCCURED                                          
02721         GO TO 1009-EXIT.                                          
02722                                                                   
02723      MOVE CP-R78-U-PRM               TO GRR-LFCOM.                
02724      MOVE CP-PRORATA-U-PRM           TO GRP-LFCOM.                
02725      MOVE CP-STATE-U-PRM             TO GRS-LFCOM.                
02726      MOVE CP-DOMICILE-U-PRM          TO GRD-LFCOM.                
02727                                                                   
CIDMOD     IF GR-LFTYP = 'QD'                                           
CIDMOD        MOVE +15.0               TO CP-LOAN-APR                   
CIDMOD     END-IF                                                       
CIDMOD
CIDMOD*****************************************************************
CIDMOD*
CIDMOD* THE FOLLOWING 7 LINES OF CODE WERE COMMENTED OUT IN THE LOGIC
CIDMOD* BASE SYSTEM CODE. CSO WAS USING THEM IN CURRENT PRODUCTION, SO
CIDMOD* THE ASTRISK WAS REMOVED FROM EACH LINE.
CIDMOD*
CIDMOD* (PUT BACK IN FOR TESTING - 11/16/98)
CIDMOD*
CIDMOD*****************************************************************
CIDMOD
LOGIC *    IF (CP-EARN-AS-NET-PAY                                       
LOGIC *       AND GR-LFPRM GREATER THAN ZERO)                           
LOGIC *       COMPUTE NP-WORK1 = GRR-LFPRM / GR-LFPRM                   
LOGIC *       COMPUTE GRS-LFCOM ROUNDED = GR-LFCOM * NP-WORK1           
LOGIC *       MOVE GRS-LFCOM                 TO GRD-LFCOM               
LOGIC *       IF CP-LOAN-APR EQUAL ZERO                                 
LOGIC *          MOVE ZERO                   TO GRS-LFPRM.              
CIDMOD
CIDMOD*****************************************************************
CIDMOD
02735                                                                   
02736      IF DTE-CLIENT = 'NCL'                                        
02737         IF CLAS-I-RL-AH (CLAS-INDEXL) = 'L'                       
02738            MOVE GRP-LFCOM        TO GRD-LFCOM                     
02739         ELSE                                                      
02740            IF GR-EFF GREATER THAN 19941231                        
02741               IF CP-EARN-AS-NET-PAY                               
02742                  MOVE GRS-LFCOM  TO GRD-LFCOM                     
02743               ELSE                                                
02744               IF CP-EARN-AS-REG-BALLOON                           
02745                  MOVE GRS-LFCOM  TO GRD-LFCOM                     
02746               ELSE                                                
02747                  MOVE GRR-LFCOM  TO GRD-LFCOM                     
02748            ELSE                                                   
02749               MOVE GRR-LFCOM     TO GRD-LFCOM.                    
02750                                                                   
02751      IF CLAS-I-EP (CLAS-INDEXL) = 'T'                             
02752         MOVE GR-LFTAX            TO CP-ORIGINAL-PREMIUM           
02753         PERFORM 3100-CALC-UNEARNED-PREMIUM                        
02753                                  THRU 3199-EXIT                   
02754         IF CP-ERROR-OCCURED                                       
02755            GO TO 1009-EXIT                                        
02756         ELSE                                                      
02757            MOVE CP-R78-U-PRM     TO GRR-LFTAX                     
02758                                     GRP-LFTAX                     
PEMMOD        END-IF
02759      ELSE                                                         
02760         COMPUTE GRP-LFTAX ROUNDED =                               
PEMMOD*          GRP-LFPRM * AM-REI-LF-TAX                              
PEMMOD           GRP-LFPRM * CR-LF-ISS-PREM-TAX                         
02762         COMPUTE GRR-LFTAX ROUNDED =                               
PEMMOD*          GRR-LFPRM * AM-REI-LF-TAX.                             
PEMMOD           GRR-LFPRM * CR-LF-ISS-PREM-TAX                         
PEMMOD     END-IF
02764      .                                                            
02765  1009-EXIT.                                                       
02766      EXIT.                                                        
02767                                                                   
02768  EJECT                                                            
02769  1010-CALCULATE-UNEARNED-AH.                                      
02770                                                                   
02771      IF GR-AHTYP = ZEROS                                          
02772          GO TO 1099-CALC-UNEARNED-X.                              
02773                                                                   
02774      IF GR-REIN = 'R'                                             
02775          IF RWF-AH-RUNOFF-SW = 'N'  AND                           
02776             VALUATION-DATE NOT LESS THAN RWF-EP-STOP-DATE         
02777              MOVE ZEROS              TO GR-AHTYP GR-AHBEN GR-AHPRM
02778              GO TO 1099-CALC-UNEARNED-X.                          
02779                                                                   
02780      MOVE GR-AH-TERM                 TO CP-ORIGINAL-TERM.         
02781      MOVE GR-AHPRM                   TO CP-ORIGINAL-PREMIUM.      
02782      MOVE GR-AHBEN                   TO CP-ORIGINAL-BENEFIT       
02783                                         CP-RATING-BENEFIT-AMT.    
02784      MOVE AH-REM-TRM1                TO CP-REMAINING-TERM.        
02785                                                                   
02786      IF AH-EARN-METHOD EQUAL SPACES                               
02787          MOVE CLAS-I-EP (CLAS-INDEXA)    TO CP-EARNING-METHOD     
02788      ELSE                                                         
02789          MOVE AH-EARN-METHOD             TO CP-EARNING-METHOD.    
02790                                                                   
02791      MOVE CLAS-I-RL-AH (CLAS-INDEXA) TO CP-BENEFIT-TYPE.          
02792      MOVE ZEROS                      TO CP-ALTERNATE-PREMIUM.     
02793      MOVE CLAS-I-CALC-TYPE (CLAS-INDEXA)                          
02794                                      TO CP-SPECIAL-CALC-CD.       
02795      MOVE AM-AH-DEVIATION            TO CP-DEVIATION-CODE.        
02796      MOVE CR-AHTYP                   TO CP-BENEFIT-CD.            
02797      MOVE AH-OVERRIDE-L1             TO CP-AH-OVERRIDE-CODE.      
02798      MOVE AM-AH-DEVIATION-PCT        TO CP-RATE-DEV-PCT.          
02799      MOVE CR-PMT-EXTENSION-DAYS      TO CP-TERM-OR-EXT-DAYS.      
02800                                                                   
02801                                                                   
02802      PERFORM 3100-CALC-UNEARNED-PREMIUM THRU 3199-EXIT.           
02803                                                                   
02804      IF CP-ERROR-OCCURED                                          
02805          GO TO 1099-CALC-UNEARNED-X.                              
02806                                                                   
02807      MOVE CP-R78-U-PRM               TO GRR-AHPRM.                
02808      MOVE CP-PRORATA-U-PRM           TO GRP-AHPRM.                
02809      MOVE CP-STATE-U-PRM             TO GRS-AHPRM.                
02810      MOVE CP-DOMICILE-U-PRM          TO GRD-AHPRM.                
02811                                                                   
02812      IF DTE-CLIENT = 'NCL'                                        
02813          MOVE GRP-AHPRM              TO GRD-AHPRM                 
02814          IF GR-STATE = 'NM'  OR  'WI'                             
02815              COMPUTE GRS-AHPRM = ((GRR-AHPRM + GRP-AHPRM) / 2)    
02816          ELSE                                                     
02817              MOVE GRR-AHPRM          TO GRS-AHPRM.                
02818                                                                   
02819      IF DTE-CLIENT = 'NCL'                                        
02820         IF GR-EFF GREATER THAN 19941231                           
02821            COMPUTE GRD-AHPRM = ((GRR-AHPRM + GRP-AHPRM) / 2).     
02822                                                                   
CIDMOD     MOVE WS-GR-AHCOM                TO CP-ORIGINAL-PREMIUM.      
02824      MOVE ZEROS                      TO CP-ALTERNATE-PREMIUM.     
02825                                                                   
02826      PERFORM 3100-CALC-UNEARNED-PREMIUM THRU 3199-EXIT.           
02827                                                                   
02828                                                                   
02829      IF CP-ERROR-OCCURED                                          
02830          GO TO 1099-CALC-UNEARNED-X.                              
02831                                                                   
02832      MOVE CP-R78-U-PRM               TO GRR-AHCOM.                
02833      MOVE CP-PRORATA-U-PRM           TO GRP-AHCOM.                
02834      MOVE CP-STATE-U-PRM             TO GRS-AHCOM.                
02835      MOVE CP-DOMICILE-U-PRM          TO GRD-AHCOM.                
02836                                                                   
02837      IF DTE-CLIENT = 'NCL'                                        
02838          MOVE GRP-AHCOM              TO GRD-AHCOM                 
02839          IF GR-STATE = 'NM'  OR  'WI'                             
02840              COMPUTE GRS-AHCOM = ((GRR-AHCOM + GRP-AHCOM) / 2)    
02841          ELSE                                                     
02842              MOVE GRR-AHCOM          TO GRS-AHCOM.                
02843                                                                   
02844      IF DTE-CLIENT = 'NCL'                                        
02845         IF GR-EFF GREATER THAN 19941231                           
02846            COMPUTE GRD-AHCOM = ((GRR-AHCOM + GRP-AHCOM) / 2).     
02847                                                                   
02848      COMPUTE GRP-AHTAX ROUNDED =                                  
PEMMOD*         GRP-AHPRM * AM-REI-AH-TAX.                              
PEMMOD          GRP-AHPRM * CR-AH-ISS-PREM-TAX                          
02850                                                                   
02851      COMPUTE GRR-AHTAX ROUNDED =                                  
PEMMOD*         GRR-AHPRM * AM-REI-AH-TAX.                              
PEMMOD          GRR-AHPRM * CR-AH-ISS-PREM-TAX                          
02853      .                                                            
02854  1099-CALC-UNEARNED-X.                                            
02855      EXIT.                                                        
02856  EJECT                                                            
02857  EJECT                                                            
02858  2000-CALC-REM-AMT.                                               
02859                                                                   
02860      IF (DTE-CLIENT = 'ABL') AND (GR-CARRIER NOT EQUAL '2')       
02861          IF (CR-SEX = 'F') AND                                    
02862             (GR-MORT-AGE GREATER +22) AND                         
02863             (CR-MORT = 'E550' OR 'F550')                          
02864                 COMPUTE GR-AGE = GR-AGE - +6.                     
02865                                                                   
02866      IF (DTE-CLIENT EQUAL 'NCL') AND                              
02867         (O-B-CERTIFICATE)                                         
02868         COMPUTE GR-REM-AMT EQUAL GR-LFBEN * .5                    
02869         GO TO 2999-CALC-REM-AMT-X.                                
02870                                                                   
02871 ****************************************************************  
02872 *  THIS LOGIC ALLOWS THE CLIENT TO CONTROL THE AGE CALCULATION *  
02873 *  ROUTINE THROUGH A COMPANY LEVEL CONTROL.  OPTIONS ALLOW THE *  
02874 *  CLIENT TO 1) USE THE AGE METHOD ASSIGNED TO THE TABLE WHEN  *  
02875 *  WHEN IT WAS GENERATED, 2) USE AGE-LAST, 3) USE AGE-NEAR.    *  
02876 ****************************************************************  
02877                                                                   
02878      IF  DTE-USE-ALL-AGE-LAST                                     
02879          COMPUTE GR-MORT-AGE =                                    
02880              GR-AGE + ((GR-LF-TERM - GR-LF-REMTERM) / +12)        
02881                                                                   
02882      ELSE                                                         
02883          IF  DTE-USE-ALL-AGE-NEAR                                 
02884              COMPUTE GR-MORT-AGE ROUNDED =                        
02885                  GR-AGE + ((GR-LF-TERM - GR-LF-REMTERM) / +12)    
02886                                                                   
02887          ELSE                                                     
02888              IF  DTE-USE-TABLE-ASSIGNED-METHOD                    
02889                  SET CLAS-MORT-NDX TO +1                          
02890                  SEARCH CLAS-MORT-FLD                             
02891                      VARYING CLAS-MORT-NDX                        
02892                                                                   
02893                      AT END                                       
02894 ******THIS IS A DEFAULT TO AGE LAST FOR A TABLE MISSING****       
02895 ******ECS080 SHOULD REPORT THE MISSING TABLE           ****       
02896                          COMPUTE GR-MORT-AGE = GR-AGE +           
02897                              ((GR-LF-TERM - GR-LF-REMTERM) / +12) 
02898                                                                   
02899                      WHEN                                         
02900                          CLAS-MORT-CODE (CLAS-MORT-NDX)           
02901                              EQUAL GR-MORT-CODE                   
02902                          PERFORM 2040-DETERMINE-AGE-METHOD        
02903                              THRU 2040-EXIT.                      
02904                                                                   
02905      ADD +1                      TO W-PASS-CTR.                   
02906                                                                   
02907      IF DTE-CLIENT = 'DAC' OR 'BPI'                               
02908          IF CR-SEX = 'F'                                          
02909              COMPUTE GR-MORT-AGE = GR-MORT-AGE - +6.              
02910                                                                   
02911      IF CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P'                   
02912          MOVE GR-LFBEN TO GR-REM-AMT                              
02913          GO TO 2999-CALC-REM-AMT-X.                               
02914                                                                   
CIDMOD     IF GR-LFTYP = 'QD'                                           
CIDMOD*       MOVE +15.0               TO CP-LOAN-APR  (PAUL CHANGE)    
CIDMOD        MOVE +15.0               TO GR-APR                        
CIDMOD     END-IF                                                       
CIDMOD
02915      IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')  AND        
02916         GR-REIN = 'P'                                             
02917          COMPUTE INTERMED ROUNDED = CR-LFAMT / GR-LF-TERM         
02918      ELSE                                                         
02919          COMPUTE INTERMED ROUNDED = GR-LFBEN / GR-LF-TERM.        
02920                                                                   
02921      IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')  AND        
02922         GR-REIN = 'R'                                             
02923          COMPUTE INTERMED ROUNDED EQUAL                           
02924              (CR-LFAMT * WS-CEDE-FACT) / GR-LF-TERM.              
02925                                                                   
02926      MOVE INTERMED                    TO GR-MO-DEC.               
02927                                                                   
02928      IF GR-LF-REMTERM = GR-LF-TERM                                
02929          IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L') AND     
02930             (GR-REIN = 'R')                                       
02931              NEXT SENTENCE                                        
02932          ELSE                                                     
02933              MOVE GR-LFBEN TO GR-REM-AMT                          
02934              GO TO 2999-CALC-REM-AMT-X.                           
02935                                                                   
02936 *    IF GR-SUMMARY-REC  OR                                        
02937 *       (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')             
02938 *        GO TO 2010-ORDINARY-REM.                                 
02939                                                                   
02940      IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')             
02941          GO TO 2010-ORDINARY-REM.                                 
02942                                                                   
02943      IF CLAS-I-EP (CLAS-INDEXL) = 'T'                             
02944          GO TO 2020-CALC-TEXAS-REM.                               
02945                                                                   
02946      IF DTE-CLIENT = 'NCL'                                        
02947          IF GR-EFF LESS THAN 19910101                             
02948              GO TO 2010-ORDINARY-REM.                             
02949                                                                   
CIDMOD     IF GR-LFTYP = 'QD'                                           
CIDMOD*       MOVE +15.0               TO CP-LOAN-APR  (PAUL CHANGE)    
CIDMOD        MOVE +15.0               TO GR-APR                        
CIDMOD        MOVE 'N'           TO CLAS-I-CALC-TYPE (CLAS-INDEXL)
CIDMOD        GO TO 2030-CALC-NET-PAY-REM                               
CIDMOD     END-IF                                                       
CIDMOD
02950      IF CLAS-I-EP (CLAS-INDEXL) = 'N'                             
02951          GO TO 2030-CALC-NET-PAY-REM.                             
02952                                                                   
CIDMOD*    IF STATE-ABBR (CLAS-INDEXS) = 'OH'                           
CIDMOD     IF (STATE-ABBR (CLAS-INDEXS) = 'OH') AND                     
CIDMOD        (CR-RATING-CLASS NOT = 'L ')
02954          IF (GR-LF-TERM GREATER THAN +60) AND                     
02955             (GR-EFF GREATER THAN 19831031) AND                    
02956             (GR-APR GREATER THAN ZERO)                            
02957              GO TO 2030-CALC-NET-PAY-REM.                         
02958                                                                   
02959      IF STATE-ABBR (CLAS-INDEXS) = 'MT'                           
02960          IF (GR-LF-TERM GREATER THAN +61) AND                     
02961             (GR-EFF GREATER THAN 19830318) AND                    
02962             (GR-APR GREATER THAN ZERO)                            
02963              GO TO 2030-CALC-NET-PAY-REM.                         
02964                                                                   
02965      IF STATE-ABBR (CLAS-INDEXS) = 'UT'                           
02966          IF (GR-LF-TERM GREATER THAN +62) AND                     
02967             (GR-EFF GREATER THAN 19810831) AND                    
02968             (GR-EFF LESS THAN 19830901) AND                       
02969             (GR-APR GREATER THAN ZERO)                            
02970              GO TO 2030-CALC-NET-PAY-REM.                         
02971                                                                   
02972      IF STATE-ABBR (CLAS-INDEXS) = 'RI'                           
02973          IF (GR-LF-TERM GREATER THAN +60) AND                     
02974             (GR-EFF GREATER THAN 19831231) AND                    
02975             (GR-APR GREATER THAN ZERO)                            
02976              GO TO 2030-CALC-NET-PAY-REM.                         
02977                                                                   
02978  2010-ORDINARY-REM.                                               
02979                                                                   
02980      IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')  AND        
02981         GR-REIN = 'P'                                             
02982          COMPUTE GR-REM-AMT ROUNDED =                             
02983                         (INTERMED * GR-LF-REMTERM) + CR-LFAMT-ALT 
02984      ELSE                                                         
02985          COMPUTE GR-REM-AMT ROUNDED = INTERMED * GR-LF-REMTERM.   
02986                                                                   
02987      IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')  AND        
02988         GR-REIN = 'R'                                             
02989          COMPUTE GR-REM-AMT ROUNDED EQUAL                         
02990             (INTERMED * GR-LF-REMTERM) +                          
02991             (CR-LFAMT-ALT * WS-CEDE-FACT).                        
02992                                                                   
02993      GO TO 2999-CALC-REM-AMT-X.                                   
02994                                                                   
02995  2020-CALC-TEXAS-REM.                                             
02996      DIVIDE GR-LFBEN BY GR-LF-TERM                                
02997          GIVING TEX-FACT-1.                                       
02998      DIVIDE LF-REM-TRM2 BY GR-PMT-FREQ                            
02999          GIVING TEX-FACT-2                                        
03000          REMAINDER TEX-FACT-3.                                    
03001                                                                   
03002      IF TEX-FACT-3 NOT = ZERO                                     
03003          ADD +1 TO TEX-FACT-2.                                    
03004                                                                   
03005      IF (TEX-FACT-2 * GR-PMT-FREQ) = GR-LF-TERM                   
03006          MOVE GR-LFBEN TO GR-REM-AMT                              
03007      ELSE                                                         
03008          COMPUTE GR-REM-AMT ROUNDED =                             
03009              (TEX-FACT-1 * (TEX-FACT-2 * GR-PMT-FREQ)).           
03010                                                                   
03011      GO TO 2999-CALC-REM-AMT-X.                                   
03012                                                                   
03013  2030-CALC-NET-PAY-REM.                                           
03014                                                                   
03015      MOVE GR-EFF                     TO  DC-GREG-DATE-CYMD        
03016      MOVE 'L'                        TO  DC-OPTION-CODE.          
03017      PERFORM 0400-DATE-CONVERSION-ROUTINE THRU 0409-EXIT.         
03018      MOVE DC-BIN-DATE-1              TO  CP-CERT-EFF-DT.          
03019                                                                   
03020      IF CR-LOAN-1ST-PMT-DT IS NOT EQUAL TO ZEROS                  
03021          MOVE CR-LOAN-1ST-PMT-DT     TO  DC-GREG-DATE-1-YMD       
03022          MOVE '3'                    TO  DC-OPTION-CODE           
03023          PERFORM 0400-DATE-CONVERSION-ROUTINE THRU 0409-EXIT      
03024          MOVE DC-BIN-DATE-1          TO  CP-FIRST-PAY-DATE        
03025      ELSE                                                         
03026          MOVE CP-CERT-EFF-DT         TO  DC-BIN-DATE-1            
03027          MOVE +1                     TO  DC-ELAPSED-MONTHS        
03028          MOVE +0                     TO  DC-ELAPSED-DAYS          
03029          MOVE '6'                    TO  DC-OPTION-CODE           
03030          PERFORM 0400-DATE-CONVERSION-ROUTINE THRU 0409-EXIT      
03031          MOVE DC-BIN-DATE-2          TO  CP-FIRST-PAY-DATE.       
03032                                                                   
03033      MOVE GR-LFBEN                   TO  CP-ORIGINAL-BENEFIT.     
03034      MOVE GR-APR                     TO  CP-LOAN-APR.             
03035      MOVE GR-LF-TERM                 TO  CP-ORIGINAL-TERM.        
03036      MOVE GR-LOAN-TERM               TO  CP-LOAN-TERM.            
03037      MOVE LF-REM-TRM2                TO  CP-REMAINING-TERM.       
03038      MOVE CLAS-I-CALC-TYPE (CLAS-INDEXL) TO CP-SPECIAL-CALC-CD.   
03039      MOVE CLAS-I-RL-AH (CLAS-INDEXL) TO  CP-BENEFIT-TYPE.         
03040      MOVE CLAS-I-EP (CLAS-INDEXL)    TO  CP-EARNING-METHOD.       
03041                                                                   
SUNPSD     ADD 1 TO ELRAMTX-CTR ALLCALL-CTR.
SUNPSD     CALL 'SUNAMTX' USING CALCULATION-PASS-AREA.  
SUNPSD*****CALL 'ELRAMTX' USING CALCULATION-PASS-AREA.                  
03043                                                                   
03044      MOVE CP-REMAINING-AMT           TO  GR-REM-AMT.              
03045                                                                   
03046      GO TO 2999-CALC-REM-AMT-X.                                   
03047                                                                   
03048  2040-DETERMINE-AGE-METHOD.                                       
03049                                                                   
03050      IF DTE-CLIENT EQUAL 'ACM'                                    
03051              COMPUTE GR-MORT-AGE EQUAL                            
03052                  GR-AGE + ((GR-LF-TERM - GR-LF-REMTERM) / +12)    
03053      ELSE                                                         
03054      IF  CLAS-AGE-METHOD (CLAS-MORT-NDX) EQUAL 'AL'               
03055          COMPUTE GR-MORT-AGE =                                    
03056              GR-AGE + ((GR-LF-TERM - GR-LF-REMTERM) / +12)        
03057                                                                   
03058      ELSE                                                         
03059          IF  CLAS-AGE-METHOD (CLAS-MORT-NDX) EQUAL 'AN'           
03060              COMPUTE GR-MORT-AGE ROUNDED =                        
03061                  GR-AGE + ((GR-LF-TERM - GR-LF-REMTERM) / +12).   
03062                                                                   
03063                                                                   
03064  2040-EXIT.                                                       
03065      EXIT.                                                        
03066                                                                   
03067  2999-CALC-REM-AMT-X.                                             
03068      EXIT.                                                        
03069  EJECT                                                            
03070  3000-RE-RATE-AH-ROUTINE.                                         
03071                                                                   
03072      MOVE CR-DT                      TO DC-GREG-DATE-CYMD.        
03073      MOVE 'L'                        TO DC-OPTION-CODE.           
03074      PERFORM 0400-DATE-CONVERSION-ROUTINE THRU 0409-EXIT          
03075      MOVE DC-BIN-DATE-1              TO CP-CERT-EFF-DT.           
03076      MOVE CLAS-I-RL-AH (CLAS-INDEXA) TO CP-BENEFIT-TYPE.          
03077      MOVE SPACES                     TO CP-ACCT-FLD-5.            
03078      MOVE DTE-CLIENT                 TO CP-COMPANY-ID.            
03079      MOVE STATE-SUB (CLAS-INDEXS)    TO CP-STATE.                 
03080      MOVE STATE-ABBR (CLAS-INDEXS)   TO CP-STATE-STD-ABBRV.       
03081      MOVE CR-AGE                     TO CP-ISSUE-AGE.             
03082      MOVE GR-AH-UP-REMTERM           TO CP-ORIGINAL-TERM.         
03083      MOVE CR-AHAMT                   TO CP-ORIGINAL-BENEFIT       
03084                                         CP-RATING-BENEFIT-AMT.    
03085      IF CP-STATE-STD-ABBRV = 'OR'                                 
03086          COMPUTE CP-RATING-BENEFIT-AMT =                          
03087                                   CR-AHAMT * GR-AH-UP-REMTERM.    
03088      MOVE CR-APR                     TO CP-LOAN-APR.              
03089      MOVE CR-PMT-FREQ                TO CP-PAY-FREQUENCY.         
03090      MOVE AH-EARN-METHOD             TO CP-EARNING-METHOD.        
03091      MOVE '3'                        TO CP-PROCESS-TYPE.          
03092      MOVE CLAS-I-BAL (CLAS-INDEXA)   TO CP-SPECIAL-CALC-CD.       
03093      MOVE CR-LOAN-TERM               TO CP-LOAN-TERM.             
03094      IF CR-RATING-CLASS NOT = SPACE AND ZERO                      
03095          MOVE CR-RATING-CLASS        TO CP-CLASS-CODE             
03096      ELSE                                                         
03097          MOVE AM-CAL-TABLE           TO CP-CLASS-CODE.            
03098      MOVE AM-AH-DEVIATION            TO CP-DEVIATION-CODE.        
03099      MOVE CR-AHTYP                   TO CP-BENEFIT-CD.            
03100      MOVE DTE-CLASIC-COMPANY-CD      TO CP-COMPANY-CD.            
03101      MOVE CR-PMT-EXTENSION-DAYS      TO CP-TERM-OR-EXT-DAYS.      
03102      MOVE AH-OVERRIDE-L1             TO CP-AH-OVERRIDE-CODE.      
03103      MOVE AM-AH-DEVIATION-PCT        TO CP-RATE-DEV-PCT.          
03104                                                                   
03105                                                                   
03106  3090-CALL-RATING-ROUTINE.                                        
03107                                                                   
SUNPSD     ADD 1 TO ELRATEX-CTR ALLCALL-CTR.
SUNPSD     CALL 'SUNATEX' USING CALCULATION-PASS-AREA.   
SUNPSD*****CALL 'ELRATEX' USING CALCULATION-PASS-AREA.                  
03109                                                                   
03110  3099-EXIT.                                                       
03111      EXIT.                                                        
03112  3100-CALC-UNEARNED-PREMIUM.                                      
03113                                                                   
03114      MOVE CR-DT                      TO DC-GREG-DATE-CYMD         
03115      MOVE 'L'                        TO DC-OPTION-CODE.           
03116      PERFORM 0400-DATE-CONVERSION-ROUTINE THRU 0409-EXIT          
03117      MOVE DC-BIN-DATE-1              TO CP-CERT-EFF-DT.           
03118                                                                   
03119      MOVE BIN-RUN-DATE               TO CP-VALUATION-DT.          
03120                                                                   
03121      MOVE DTE-R78                    TO CP-R78-OPTION.            
03122      MOVE SPACES                     TO CP-ACCT-FLD-5.            
03123      MOVE DTE-CLIENT                 TO CP-COMPANY-ID.            
03124      MOVE CR-AGE                     TO CP-ISSUE-AGE.             
03125      MOVE CR-APR                     TO CP-LOAN-APR.              
03126      MOVE CR-PMT-FREQ                TO CP-PAY-FREQUENCY.         
03127      MOVE CR-LOAN-TERM               TO CP-LOAN-TERM.             
03128      MOVE DOMICILE-STATE             TO CP-DOMICILE-STATE.        
03129      MOVE GR-CARRIER                 TO CP-CARRIER.               
03130                                                                   
03131      IF CR-RATING-CLASS NOT = SPACE AND ZERO                      
03132          MOVE CR-RATING-CLASS        TO CP-CLASS-CODE             
03133      ELSE                                                         
03134          MOVE AM-CAL-TABLE           TO CP-CLASS-CODE.            
03135      MOVE STATE-SUB (CLAS-INDEXS)    TO CP-STATE.                 
03136      MOVE STATE-ABBR (CLAS-INDEXS)   TO CP-STATE-STD-ABBRV.       
03137      MOVE DTE-CLASIC-COMPANY-CD      TO CP-COMPANY-CD.            
03138      MOVE CR-PMT-EXTENSION-DAYS      TO CP-TERM-OR-EXT-DAYS.      
03139                                                                   
03140                                                                   
03141  3190-CALL-UNEARNED-ROUTINE.                                      
03142                                                                   
SUNPSD     ADD 1 TO ELUPRMX-CTR ALLCALL-CTR.
SUNPSD     CALL 'SUNPRMX' USING CALCULATION-PASS-AREA.    
SUNPSD*****CALL 'ELUPRMX' USING CALCULATION-PASS-AREA.                  
03144                                                                   
03145  3199-EXIT.                                                       
03146      EXIT.                                                        
03147  EJECT                                                            
03148  4000-REINSURE-ROUTINE.                                           
03149      PERFORM CLEAR-REIN-HOLD THRU CLEAR-REIN-HOLD-X.              
03150                                                                   
03151      IF CR-REIN-TABLE = SPACES OR ZEROS                           
03152          GO TO 4020-REINSURE-ROUTINE-SKIP-1.                      
03153                                                                   
03154      MOVE CR-REIN-TABLE TO REIN-SRCH.                             
03155                                                                   
03156      PERFORM 4800-RR-READ-REIN THRU 4899-RR-READ-REIN-X.          
03157                                                                   
03158      IF RE-REMAINING (1) NOT = 'I'                                
03159          GO TO 4010-REINSURE-ROUTINE-GET-CALC.                    
03160                                                                   
03161      MOVE ZEROS                     TO REIN-EARN-LF-TERM          
03162                                        REIN-EARN-AH-TERM.         
03163                                                                   
03164      MOVE WS-BIN-CR-DT              TO DC-BIN-DATE-1.             
03165      MOVE WS-BIN-VAL-DT             TO DC-BIN-DATE-2.             
03166      MOVE '1'                       TO DC-OPTION-CODE.            
03167      MOVE ' '                       TO DC-CENTURY-ADJUSTMENT.     
03168      MOVE ZEROS                     TO DC-ELAPSED-MONTHS          
03169                                        DC-ODD-DAYS-OVER           
03170                                        DC-ELAPSED-DAYS.           
03171      PERFORM 0400-DATE-CONVERSION-ROUTINE THRU 0409-EXIT.         
03172                                                                   
03173      IF CR-LFTYP NOT = ZEROS  AND  SPACES                         
03174          IF DC-ELAPSED-MONTHS GREATER THAN CR-LF-TERM             
03175              MOVE CR-LF-TERM        TO REIN-EARN-LF-TERM          
03176          ELSE                                                     
03177              MOVE DC-ELAPSED-MONTHS TO REIN-EARN-LF-TERM.         
03178                                                                   
03179      IF CR-AHTYP NOT = ZEROS  AND  SPACES                         
03180          IF DC-ELAPSED-MONTHS GREATER THAN CR-AH-TERM             
03181              MOVE CR-AH-TERM        TO REIN-EARN-AH-TERM          
03182          ELSE                                                     
03183              MOVE DC-ELAPSED-MONTHS TO REIN-EARN-AH-TERM.         
03184                                                                   
03185                                                                   
03186  4010-REINSURE-ROUTINE-GET-CALC.                                  
03187                                                                   
03188                              COPY ECSRTPFM.                       
03189                                                                   
03190  4020-REINSURE-ROUTINE-SKIP-1.                                    
03191      MOVE +1 TO SUB1.                                             
03192                                                                   
03193  4030-REINSURE-ROUTINE-BLD-GAAP.                                  
03194      IF REIN-COMP (SUB1) = SPACES                                 
03195          GO TO 4099-REINSURE-ROUTINE-X.                           
03196                                                                   
03197      IF REIN-REM-SW (SUB1) = 'Z'                                  
03198          ADD +1 TO SUB1                                           
03199          GO TO 4030-REINSURE-ROUTINE-BLD-GAAP.                    
03200                                                                   
03201      IF REIN-LF-AH-FLGS (SUB1) = SPACES                           
03202          ADD +1 TO SUB1                                           
03203          GO TO 4030-REINSURE-ROUTINE-BLD-GAAP.                    
03204                                                                   
03205      PERFORM REIN-FIND-CO-LOOP THRU FIND-REI-CO-EXIT              
03206          VARYING CO-SUB FROM +1 BY +1 UNTIL                       
03207            (RCT-REIN-CO (CO-SUB) = REIN-COMP (SUB1))   OR         
03208            (CO-SUB GREATER THAN REIN-CO-TABLE-ENT-CNT) OR         
03209            (RCT-REIN-CO (CO-SUB) = SPACES).                       
03210                                                                   
03211      IF (CO-SUB GREATER THAN REIN-CO-TABLE-ENT-CNT)  OR           
03212         (RCT-REIN-CO (CO-SUB) = SPACES)                           
03213          MOVE ZEROS          TO CO-SUB.                           
03214                                                                   
03215      MOVE ZEROS              TO ERN-START-DATE.                   
03216                                                                   
03217      IF CO-SUB NOT = ZEROS                                        
03218        IF RCT-EARNING-START-DT (CO-SUB) NOT = ZEROS               
03219          MOVE RCT-EARNING-START-DT (CO-SUB)                       
03220                              TO DC-GREG-DATE-CYMD                 
03221          MOVE 'L'            TO DC-OPTION-CODE                    
03222          PERFORM 0400-DATE-CONVERSION-ROUTINE THRU 0409-EXIT      
03223          MOVE +0             TO DC-ELAPSED-MONTHS                 
03224          MOVE -1             TO DC-ELAPSED-DAYS                   
03225          MOVE '6'            TO DC-OPTION-CODE                    
03226          PERFORM 0400-DATE-CONVERSION-ROUTINE THRU 0409-EXIT      
03227          MOVE +0             TO DC-ELAPSED-MONTHS                 
03228                                 DC-ELAPSED-DAYS                   
03229          MOVE DC-BIN-DATE-2  TO DC-BIN-DATE-1                     
03230          MOVE ' '            TO DC-OPTION-CODE                    
03231          PERFORM 0400-DATE-CONVERSION-ROUTINE THRU 0409-EXIT      
03232          IF NO-CONVERSION-ERROR                                   
03233              MOVE DC-GREG-DATE-CYMD  TO ERN-START-DATE            
03234          ELSE                                                     
03235              DISPLAY 'ERROR OCCURED CONVERTING EARNING START '    
03236                 'DATE FOR REIN. CO. ' GR-REIN-COMP                
03237                 '  START DATE: ' RCT-EARNING-START-DT (CO-SUB)    
03238                 '  IN TABLE: ' CR-REIN-TABLE                      
03239              MOVE 'ERROR OCCURED CONVERTING EARNING START DATE '  
03240                                  TO WS-ABEND-MESSAGE              
03241              MOVE DC-ERROR-CODE  TO WS-ABEND-FILE-STATUS          
03242              MOVE 0501           TO WS-ABEND-CODE                 
03243              GO TO ABEND-PGM.                                     
03244                                                                   
03245      MOVE SAVE-GAAP-REC      TO GAAP-RECORD.                      
03246      PERFORM LOAD-GAAP-WS-DATES.                                  
03247      MOVE 'R'                TO GR-REIN.                          
03248      MOVE REIN-COMP (SUB1)   TO GR-REIN-COMP.                     
03249      MOVE REIN-WORK-FLDS (SUB1) TO RWF-FIELDS.                    
03250      MOVE RE-COMP-INFO (SUB1) TO WT-COMP-INFO.                    
03251                                                                   
03252      IF REIN-LF-FLG (SUB1) = 'X'  AND                             
03253         GR-LFTYP NOT = ZERO                                       
03254          MOVE RWF-LFAMT TO GR-LFBEN                               
03255          MOVE RWF-LFPRM TO GR-LFPRM                               
03256          IF GR-LFBEN = ZEROS AND                                  
03257             GR-LFPRM = ZEROS                                      
03258               MOVE ZEROS TO GR-LFTYP                              
03259          ELSE                                                     
03260               NEXT SENTENCE                                       
03261      ELSE                                                         
03262          MOVE ZERO TO GR-LFTYP  GR-LFBEN  GR-LFPRM.               
03263                                                                   
03264      IF DTE-CLIENT = 'MIC'                                        
03265          IF GR-LFTYP NOT = ZERO                                   
03266              COMPUTE GR-LFBEN = CR-LFAMT + CR-LFAMT-ALT.          
03267                                                                   
03268      IF REIN-AH-FLG (SUB1) = 'X'  AND                             
03269         GR-AHTYP NOT = ZERO                                       
03270          MOVE RWF-AHAMT TO GR-AHBEN                               
03271          MOVE RWF-AHPRM TO GR-AHPRM                               
03272          IF GR-AHBEN = ZEROS AND                                  
03273             GR-AHPRM = ZEROS                                      
03274               MOVE ZEROS TO GR-AHTYP                              
03275          ELSE                                                     
03276               NEXT SENTENCE                                       
03277      ELSE                                                         
03278          MOVE ZERO TO GR-AHTYP  GR-AHBEN  GR-AHPRM.               
03279                                                                   
03280      IF DTE-CLIENT = 'MIC'                                        
03281          IF GR-AHTYP NOT = ZERO                                   
03282              MOVE CR-AHAMT  TO GR-AHBEN.                          
03283                                                                   
03284      IF GR-SUMMARY-REC                                            
03285          COMPUTE GR-AHBEN = GR-AHBEN * WS-SUM-AH-FACTOR           
03286          COMPUTE GR-LFBEN = GR-LFBEN * WS-SUM-LF-FACTOR.          
03287                                                                   
03288      MOVE ZEROS                     TO GRP-LFPRM  GRP-AHPRM       
03289                                        GRP-LFCOM  GRP-AHCOM       
03290                                        GRR-LFPRM  GRR-AHPRM       
03291                                        GRR-LFCOM  GRR-AHCOM       
03292                                        GRS-LFPRM  GRS-AHPRM       
03293                                        GRS-LFCOM  GRS-AHCOM       
03294                                        GRD-LFPRM  GRD-AHPRM       
03295                                        GRD-LFCOM  GRD-AHCOM.      
03296                                                                   
03297      MOVE GR-LFPRM                  TO LF-PRM.                    
03298      MOVE ZEROS                     TO LF-PRM-ALT.                
03299                                                                   
03300      MOVE 'R'                        TO CP-REIN-FLAG.             
03301                                                                   
03302      MOVE LF-REM-TRM1              TO SV-LF-REM-TRM1.             
03303      MOVE LF-REM-TRM2              TO SV-LF-REM-TRM2.             
03304      MOVE LF-BAL-REMTERM           TO SV-LF-BAL-REMTERM.          
03305      MOVE AH-REM-TRM1              TO SV-AH-REM-TRM1.             
03306      MOVE AH-REM-TRM2              TO SV-AH-REM-TRM2.             
03307                                                                   
03308      IF ERN-START-DATE NOT GREATER THAN VALUATION-DATE            
03309          GO TO 4050-CALCULATE-REIN-UNEARNED.                      
03310                                                                   
03311      PERFORM 0160-CALC-REM-TERM THRU 0179-CALC-REM-TERM-EXIT.     
03312                                                                   
03313      IF GR-LFTYP NOT = ZEROS                                      
03314        IF ((CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')  AND     
03315            CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L'  AND          
03316            LF-BAL-REMTERM NOT GREATER THAN ZERO)                  
03317                            OR                                     
03318           ((CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')  AND     
03319            CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'L'  AND              
03320            LF-REM-TRM2 NOT GREATER THAN ZERO)                     
03321                            OR                                     
03322           ((CLAS-I-EP (CLAS-INDEXL) NOT = 'B' AND 'K' AND 'L') AND
03323            LF-REM-TRM2 NOT GREATER ZERO AND                       
03324            NOT O-B-CERTIFICATE)                                   
03325            MOVE ZERO                     TO LF-REM-TRM2.          
03326                                                                   
03327      IF LF-REM-TRM2 NOT GREATER THAN ZERO                         
03328          MOVE ZEROS                TO GR-LFTYP GR-LFBEN GR-LFPRM  
03329      ELSE                                                         
03330          MOVE LF-REM-TRM1          TO GR-LF-UP-REMTERM            
03331          MOVE LF-REM-TRM2          TO GR-LF-REMTERM.              
03332                                                                   
03333      IF AH-REM-TRM2 NOT GREATER THAN ZERO                         
03334          MOVE ZEROS                TO GR-AHTYP GR-AHBEN GR-AHPRM  
03335      ELSE                                                         
03336          MOVE AH-REM-TRM1          TO GR-AH-UP-REMTERM            
03337          MOVE AH-REM-TRM2          TO GR-AH-REMTERM.              
03338                                                                   
03339      IF GR-LFTYP = ZEROS  AND                                     
03340         GR-AHTYP = ZEROS                                          
03341          GO TO 4095-REINSURE-ROUTINE-FINISH.                      
03342                                                                   
03343  4050-CALCULATE-REIN-UNEARNED.                                    
03344                                                                   
03345      IF REIN-REM-SW (SUB1) = 'I'                                  
03346          GO TO 4080-REINSURE-RISK-PREMIUMS.                       
03347                                                                   
03348      IF GR-LFTYP NOT EQUAL ZERO                                   
03349         IF (CLAS-I-EP (CLAS-INDEXL) EQUAL 'B' OR 'K' OR 'L')      
03350            COMPUTE WS-CEDE-FACT EQUAL                             
03351                    GR-LFBEN / (CR-LFAMT + CR-LFAMT-ALT)           
03352            COMPUTE LF-PRM ROUNDED EQUAL                           
03353                          (CR-LFPRM * WS-CEDE-FACT)                
03354            COMPUTE LF-PRM-ALT ROUNDED EQUAL                       
03355                          (CR-LFPRM-ALT * WS-CEDE-FACT).           
03356                                                                   
03357      PERFORM 1000-CALCULATE-UNEARNED THRU 1099-CALC-UNEARNED-X.   
03358                                                                   
03359      IF GR-LFTYP = ZEROS  AND                                     
03360         GR-AHTYP = ZEROS                                          
03361          GO TO 4095-REINSURE-ROUTINE-FINISH.                      
03362                                                                   
03363      IF (STATE-ABBR (CLAS-INDEXS) EQUAL 'VA')  AND                
03364         (CR-AH-TERM GREATER THAN +61) AND                         
03365         (CR-DT  GREATER  THAN 19921231)                           
03366           MOVE 'A' TO AH-EARN-METHOD.                             
03367                                                                   
03368      IF (GR-AHTYP NOT = ZERO)  AND                                
03369         (CLAS-I-CALC-TYPE (CLAS-INDEXA) NOT = 'B' AND 'Z')  AND   
03370         (AH-EARN-METHOD = 'A' OR 'C') AND                         
03371         (GR-AHPRM NOT = ZERO)                                     
03372          PERFORM 4500-REIN-AH-RATE-LOOKUP                         
03373              THRU 4599-E-REIN-AH-RATE-LOOKUP                      
03374          IF CP-ERROR-RATE-NOT-FOUND  OR                           
03375             CP-ERROR-RATE-IS-ZERO  OR                             
03376             CP-ERROR-IN-DATES                                     
03377              NEXT SENTENCE                                        
03378          ELSE                                                     
03379              COMPUTE GRS-AHPRM ROUNDED =                          
03380                                  ((GR-AH-UP-REMTERM * GR-AHBEN)   
03381                                  / +100) * CP-PREMIUM-RATE        
03382              COMPUTE GRD-AHPRM ROUNDED =                          
03383                                  ((GR-AH-UP-REMTERM * GR-AHBEN)   
03384                                  / +100) * CP-PREMIUM-RATE.       
03385                                                                   
03386      MOVE ZEROS TO GR-REM-AMT.                                    
03387      MOVE ZEROS TO GR-MO-DEC.                                     
03388                                                                   
03389      IF GR-LFTYP NOT EQUAL ZERO                                   
03390          PERFORM 2000-CALC-REM-AMT THRU 2999-CALC-REM-AMT-X.      
03391                                                                   
03392      IF WT-REMAINING = 'L'                                        
03393          IF SAVE-REMAINING-AMT GREATER THAN GR-LFBEN              
03394              MOVE GR-LFBEN TO GR-REM-AMT                          
03395              MOVE GR-LF-TERM TO GR-LF-REMTERM                     
03396          ELSE                                                     
03397              MOVE SAVE-REMAINING-AMT TO GR-REM-AMT.               
03398                                                                   
03399      MOVE ZEROS                          TO GR-AH-REM-BEN.        
03400                                                                   
03401      IF GR-AHTYP NOT = ZERO                                       
03402          COMPUTE GR-AH-REM-BEN ROUNDED = GR-AHBEN * GR-AH-REMTERM 
03403          IF DTE-CLIENT = 'NCL'  AND                               
03404             O-B-CERTIFICATE                                       
03405              COMPUTE GR-AH-REM-BEN = GR-AHBEN * .5.               
03406                                                                   
03407      IF DTE-CLIENT NOT = 'MIC'  AND  'MCC'                        
03408          GO TO 4090-REINSURE-ROUTINE-CONT.                        
03409                                                                   
03410      IF CO-SUB = ZEROS                                            
03411          GO TO 4090-REINSURE-ROUTINE-CONT.                        
03412                                                                   
03413      IF GR-AHTYP NOT = ZEROS  AND                                 
03414         RCT-AH-CLM-MAX (CO-SUB) NOT = ZEROS                       
03415          IF GR-AH-REM-BEN GREATER THAN RCT-AH-CLM-MAX (CO-SUB)    
03416              MOVE RCT-AH-CLM-MAX (CO-SUB) TO GR-AH-REM-BEN.       
03417                                                                   
03418      IF GR-LFTYP = ZEROS  OR                                      
03419         RCT-LF-CLM-MAX (CO-SUB) = ZEROS                           
03420          GO TO 4090-REINSURE-ROUTINE-CONT.                        
03421                                                                   
03422      IF RCT-LF-CLM-MAX (CO-SUB) NOT LESS THAN GR-LFBEN            
03423          GO TO 4090-REINSURE-ROUTINE-CONT.                        
03424                                                                   
03425      MOVE RCT-LF-CLM-MAX (CO-SUB)   TO GR-LFBEN.                  
03426                                                                   
03427      IF GR-REM-AMT GREATER THAN RCT-LF-CLM-MAX (CO-SUB)           
03428          MOVE RCT-LF-CLM-MAX (CO-SUB) TO GR-REM-AMT               
03429          COMPUTE GR-MO-DEC ROUNDED =                              
03430                        (RCT-LF-CLM-MAX (CO-SUB) / GR-LF-REMTERM). 
03431                                                                   
03432      GO TO 4090-REINSURE-ROUTINE-CONT.                            
03433                                                                   
03434                                                                   
03435  4080-REINSURE-RISK-PREMIUMS.                                     
03436                                                                   
03437      IF GR-LFTYP = ZERO                                           
03438          MOVE ZEROS                   TO GR-REM-AMT               
03439                                          GR-MO-DEC                
03440          GO TO 4085-CONTINUE.                                     
03441                                                                   
03442      COMPUTE GR-MO-DEC = RS-LIFE-BEN / CR-LF-TERM.                
03443                                                                   
03444      COMPUTE GR-REM-AMT = GR-LFBEN -                              
03445                          (GR-MO-DEC * REIN-EARN-LF-TERM).         
03446                                                                   
03447      IF GR-REM-AMT NEGATIVE                                       
03448          MOVE ZEROS                   TO GR-LFTYP                 
03449                                          GR-MO-DEC                
03450                                          GR-REM-AMT               
03451          GO TO 4085-CONTINUE.                                     
03452                                                                   
03453       COMPUTE GR-LF-REMTERM = GR-REM-AMT / GR-MO-DEC.             
03454       MOVE GR-LF-REMTERM              TO GR-LF-UP-REMTERM.        
03455                                                                   
03456  4085-CONTINUE.                                                   
03457                                                                   
03458      IF GR-AHTYP = ZERO                                           
03459          MOVE ZEROS                   TO GR-AH-REM-BEN            
03460          GO TO 4090-REINSURE-ROUTINE-CONT.                        
03461                                                                   
03462      COMPUTE GR-AH-REM-BEN = (GR-AHBEN * CR-AH-TERM) -            
03463                              (RS-AH-BEN * REIN-EARN-AH-TERM).     
03464                                                                   
03465      IF GR-AH-REM-BEN NEGATIVE                                    
03466          MOVE ZEROS                   TO GR-AHTYP                 
03467                                          GR-AH-REM-BEN            
03468          GO TO 4090-REINSURE-ROUTINE-CONT.                        
03469                                                                   
03470       COMPUTE GR-AH-REMTERM = GR-AH-REM-BEN / RS-AH-BEN.          
03471       MOVE GR-AH-REMTERM              TO GR-AH-UP-REMTERM.        
03472                                                                   
03473                                                                   
03474  4090-REINSURE-ROUTINE-CONT.                                      
03475                                                                   
03476      ADD GR-LFBEN TO REI-INIT-FACE.                               
03477      ADD GR-REM-AMT TO REI-REM-FACE.                              
03478      ADD GR-AH-REM-BEN TO REI-REM-AH.                             
03479      IF GR-FLAG = 'F'                                             
03480          ADD GR-LFBEN TO REI-FUTR-FACE.                           
03481      ADD GR-MO-DEC TO REI-MO-DEC.                                 
03482                                                                   
03483  4095-REINSURE-ROUTINE-FINISH.                                    
03484                                                                   
03485      IF GR-LFTYP = ZERO  AND  GR-AHTYP = ZERO                     
03486          GO TO 4097-CONTINUE.                                     
03487                                                                   
03488      IF DTE-CLIENT EQUAL 'CVL'                                    
03489          ADD +1 TO REI-GAAP                                       
03490          PERFORM 0700-WRITE-GAAP-RECORD THRU 0799-WRITE-GAAP-X    
03491          PERFORM 4100-FORMAT-REIN-ALPHA THRU 4199-EXIT            
03492          PERFORM 0600-WRITE-ALPHA-EXTRACT THRU 0699-EXIT          
03493      ELSE                                                         
03494          ADD +1 TO REI-GAAP                                       
03495          PERFORM 0700-WRITE-GAAP-RECORD THRU 0799-WRITE-GAAP-X.   
03496                                                                   
03497  4097-CONTINUE.                                                   
03498                                                                   
03499      MOVE SV-LF-REM-TRM1           TO LF-REM-TRM1.                
03500      MOVE SV-LF-REM-TRM2           TO LF-REM-TRM2.                
03501      MOVE SV-LF-BAL-REMTERM        TO LF-BAL-REMTERM.             
03502      MOVE SV-AH-REM-TRM1           TO AH-REM-TRM1.                
03503      MOVE SV-AH-REM-TRM2           TO AH-REM-TRM2.                
03504                                                                   
03505      ADD +1 TO SUB1.                                              
03506                                                                   
03507      GO TO 4030-REINSURE-ROUTINE-BLD-GAAP.                        
03508                                                                   
03509  4099-REINSURE-ROUTINE-X.                                         
03510      EXIT.                                                        
03511  EJECT                                                            
03512  4100-FORMAT-REIN-ALPHA.                                          
03513                                                                   
03514      MOVE 'R'                    TO  REIN-ALPHA-SW.               
03515                                                                   
03516      MOVE GR-REIN-COMP           TO  AX-REIN-COMP.                
03517                                                                   
03518      MOVE GR-LFBEN               TO  AX-LF-AMT.                   
03519      MOVE GR-LFPRM               TO  AX-LF-PRM.                   
03520      MOVE GR-LFTYP               TO  AX-LF-TYP.                   
03521      MOVE GR-LF-REMTERM          TO  AX-LF-REMTERM.               
03522      MOVE GR-REM-AMT             TO  AX-LF-REMAMT.                
03523                                                                   
03524      MOVE GR-AHBEN               TO  AX-AH-AMT.                   
03525      MOVE GR-AHPRM               TO  AX-AH-PRM.                   
03526      MOVE GR-AHTYP               TO  AX-AH-TYP.                   
03527      MOVE GR-AH-REMTERM          TO  AX-AH-REMTERM.               
03528      MOVE GR-AH-REM-BEN          TO  AX-AH-REMAMT.                
03529                                                                   
03530  4199-EXIT.                                                       
03531      EXIT.                                                        
03532  EJECT                                                            
03533  4500-REIN-AH-RATE-LOOKUP.                                        
03534                                                                   
03535      PERFORM 3000-RE-RATE-AH-ROUTINE.                             
03536                                                                   
03537      IF DTE-CLIENT = 'ELT'                                        
03538          IF CR-CARRIER = 'E' OR '1' OR '2' OR '4' OR '6' OR '7'   
03539              MOVE SPACE             TO CP-CLASS-CODE              
03540                                        CP-STATE-STD-ABBRV         
03541              MOVE ZERO              TO CP-DEVIATION-CODE          
03542              IF CR-REIN-SPEC = 'R'                                
03543                  MOVE '71'          TO CP-STATE                   
03544              ELSE                                                 
03545                  MOVE '70'          TO CP-STATE.                  
03546                                                                   
03547      PERFORM 3090-CALL-RATING-ROUTINE THRU 3099-EXIT.             
03548                                                                   
03549  4599-E-REIN-AH-RATE-LOOKUP.                                      
03550      EXIT.                                                        
03551  EJECT                                                            
03552  4800-RR-READ-REIN.                                               
03553      IF REIN-OPEN-SW = ' '                                        
03554          MOVE 'X'                   TO REIN-OPEN-SW               
03555          MOVE DTE-CLASIC-COMPANY-CD TO REIN-SRCH-COMP-CD          
03556          MOVE 'A'                   TO REIN-SRCH-CODE             
03557          OPEN INPUT ERRTBL-IN                                        
03558              IF ERRTBL-FILE-STATUS NOT = '00'  AND  '97'          
03559                  MOVE '21'               TO W-ABEND-CODE-1        
03560                  MOVE ERRTBL-FILE-STATUS TO W-ABEND-CODE-2        
03561                  MOVE WS-ABEND-CODE      TO WS-RETURN-CODE        
03562                  MOVE 'OPEN ERROR - ERRTBL' TO WS-ABEND-MESSAGE   
03563                  GO TO ABEND-PGM                                  
03564              ELSE                                                 
03565                  PERFORM REIN-CO-TABLE-BUILD THRU REIN-BUILD-EXIT.
03566                                                                   
03567      IF REIN-SRCH NOT = SAVE-REIN-SRCH                            
03568          MOVE REIN-SRCH TO SAVE-REIN-SRCH                         
03569          MOVE REIN-SEARCH TO RE-CONTROL-PRIMARY                   
03570          READ ERRTBL-IN                                              
03571              IF ERRTBL-FILE-STATUS = '23'                         
03572                  DISPLAY 'INVALID REINSURANCE TABLE CODE - '      
03573                            REIN-SRCH ' ' CR-FULL-CONTROL          
03574                  MOVE '24'               TO W-ABEND-CODE-1        
03575                  MOVE ERRTBL-FILE-STATUS TO W-ABEND-CODE-2        
03576                  MOVE WS-ABEND-CODE      TO WS-RETURN-CODE        
CIDMOD                 GO TO 4020-REINSURE-ROUTINE-SKIP-1
CIDMOD****             GO TO ABEND-PGM                                  
03578              ELSE                                                 
03579                  IF ERRTBL-FILE-STATUS NOT = '00'                 
03580                      MOVE '24'               TO W-ABEND-CODE-1    
03581                      MOVE ERRTBL-FILE-STATUS TO W-ABEND-CODE-2    
03582                      MOVE WS-ABEND-CODE      TO WS-RETURN-CODE    
03583                      GO TO ABEND-PGM.                             
03584                                                                   
03585      IF RE-CODE NOT = 'A'                                         
03586         PERFORM REIN-DATE-LOAD.                                   
03587                                                                   
03588  4899-RR-READ-REIN-X.                                             
03589      EXIT.                                                        
03590  EJECT                                                            
03591  4900-REINSURANCE-ROUTINES.                                       
03592                              COPY ECSRIRTN.                       
03593  EJECT                                                            
03594  5000-PRINT-ROUTINE.                                              
03595                              COPY ELCPRT2.                        
03596                                                                   
03597  5899-PRINT-X.                                                    
03598      EXIT.                                                        
03599                                                                   
03600  5900-HEADING-ROUTINE.                                            
03601      MOVE HDR-1B          TO PRT.                                 
03602      MOVE '1'             TO X.                                   
03603                                                                   
03604      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
03605                                                                   
03606      MOVE HDR-2           TO PRT.                                 
03607      MOVE ' '             TO X.                                   
03608                                                                   
03609      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
03610                                                                   
03611      ADD +1               TO WS-PAGE.                             
03612      MOVE WS-PAGE         TO H3-PAGE.                             
03613      MOVE HDR-3           TO PRT.                                 
03614                                                                   
03615      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
03616                                                                   
03617      MOVE HDR-4A TO PRT.                                          
03618      MOVE '0' TO X.                                               
03619                                                                   
03620      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
03621                                                                   
03622      MOVE HDR-4B TO PRT.                                          
03623      MOVE ' ' TO X.                                               
03624                                                                   
03625      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
03626                                                                   
03627      MOVE SPACES TO PRT.                                          
03628                                                                   
03629      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
03630                                                                   
03631      MOVE ZEROS                  TO WS-LINE.                      
03632                                                                   
03633  5999-HEADINGS-X.                                                 
03634      EXIT.                                                        
03635  EJECT                                                            
03636  6000-PRINT-EXCLUSIONS.                                           
03637                                                                   
03638      IF DTE-CLIENT NOT = 'FMK'                                    
03639          GO TO 6999-PRT-EXCLUSIONS-X.                             
03640                                                                   
03641      IF CR-ENTRY-STATUS = '5' OR 'D' OR 'V'                       
03642          GO TO 6999-PRT-EXCLUSIONS-X.                             
03643                                                                   
03644      IF WS-LINE GREATER +50                                       
03645          PERFORM 5900-HEADING-ROUTINE THRU 5999-HEADINGS-X.       
03646                                                                   
03647      MOVE SPACES               TO DTL-1.                          
03648                                                                   
03649      MOVE CR-CARRIER           TO D1-CARRIER.                     
03650      MOVE CR-GROUPING          TO D1-GROUPING.                    
03651      MOVE CR-STATE             TO D1-STATE.                       
03652      MOVE CR-ACCOUNT           TO D1-ACCT.                        
03653      MOVE CR-CERT-NO           TO D1-CERT.                        
03654      MOVE CR-MO                TO D1-EFF-MO.                      
03655      MOVE CR-DA                TO D1-EFF-DA.                      
03656      MOVE CR-YR                TO D1-EFF-YR.                      
03657      MOVE '/'                  TO D1-SLASH-1                      
03658                                   D1-SLASH-2.                     
03659      MOVE '/' TO D1-SLASH-1  D1-SLASH-2.                          
03660      MOVE '00/00/00'           TO D1L-EXIT-DATE                   
03661                                   D1A-EXIT-DATE.                  
03662                                                                   
03663      IF CR-LFTYP = ZEROS                                          
03664          GO TO 6500-A-H-EXCLUSIONS.                               
03665                                                                   
03666      IF CR-LF-CURRENT-STATUS = '7'                                
03667          MOVE CR-LF-DEX-MO TO D1L-EXIT-MO                         
03668          MOVE CR-LF-DEX-DA TO D1L-EXIT-DA                         
03669          MOVE CR-LF-DEX-YR TO D1L-EXIT-YR                         
03670          MOVE '/' TO D1L-SLASH-1  D1L-SLASH-2                     
03671          MOVE CR-DTH-MO TO D1L-REAS-MO                            
03672          MOVE CR-DTH-DA TO D1L-REAS-DA                            
03673          MOVE CR-DTH-YR TO D1L-REAS-YR                            
03674          MOVE '/' TO D1L-SLASH-3  D1L-SLASH-4                     
03675          MOVE 'DEATH CLAIM' TO D1L-REASON                         
03676            GO TO 6500-A-H-EXCLUSIONS.                             
03677                                                                   
03678      IF CR-LF-CURRENT-STATUS = '8'                                
03679          MOVE CR-LF-CEX-MO TO D1L-EXIT-MO                         
03680          MOVE CR-LF-CEX-DA TO D1L-EXIT-DA                         
03681          MOVE CR-LF-CEX-YR TO D1L-EXIT-YR                         
03682          MOVE '/' TO D1L-SLASH-1  D1L-SLASH-2                     
03683          MOVE CR-LF-CNC-MO TO D1L-REAS-MO                         
03684          MOVE CR-LF-CNC-DA TO D1L-REAS-DA                         
03685          MOVE CR-LF-CNC-YR TO D1L-REAS-YR                         
03686          MOVE '/' TO D1L-SLASH-3  D1L-SLASH-4                     
03687          MOVE 'CANCELLED  ' TO D1L-REASON                         
03688            GO TO 6500-A-H-EXCLUSIONS.                             
03689                                                                   
03690      IF LF-REM-TRM2 NOT GREATER ZERO                              
03691          COMPUTE D1L-EXIT-YR =                                    
03692             ((CR-YR * 12) + CR-MO + CR-LF-TERM - 1) / 12          
03693          COMPUTE D1L-EXIT-MO =                                    
03694           ((CR-YR * 12) + CR-MO + CR-LF-TERM) -                   
03695                                        (D1L-EXIT-YR * 12)         
03696          MOVE CR-DA TO D1L-EXIT-DA                                
03697          MOVE '/' TO D1L-SLASH-1  D1L-SLASH-2                     
03698          MOVE CR-YR TO D1L-REAS-YR                                
03699          MOVE CR-MO TO D1L-REAS-MO                                
03700          MOVE CR-DA TO D1L-REAS-DA                                
03701          MOVE '/' TO D1L-SLASH-3  D1L-SLASH-4                     
03702          MOVE 'EXPIRED    ' TO D1L-REASON.                        
03703                                                                   
03704  6500-A-H-EXCLUSIONS.                                             
03705                                                                   
03706      IF CR-AHTYP = ZEROS                                          
03707          GO TO 6900-PRINT-EXCLUSION-DETAIL.                       
03708                                                                   
03709      IF CR-AH-CURRENT-STATUS = '6'                                
03710          MOVE CR-AH-DEX-MO TO D1A-EXIT-MO                         
03711          MOVE CR-AH-DEX-DA TO D1A-EXIT-DA                         
03712          MOVE CR-AH-DEX-YR TO D1A-EXIT-YR                         
03713          MOVE '/' TO D1A-SLASH-1  D1A-SLASH-2                     
03714          MOVE CR-DIS-MO TO D1A-REAS-MO                            
03715          MOVE CR-DIS-DA TO D1A-REAS-DA                            
03716          MOVE CR-DIS-YR TO D1A-REAS-YR                            
03717          MOVE '/' TO D1A-SLASH-3  D1A-SLASH-4                     
03718          MOVE 'LUMP DISAB ' TO D1A-REASON                         
03719            GO TO 6900-PRINT-EXCLUSION-DETAIL.                     
03720                                                                   
03721      IF CR-AH-CURRENT-STATUS = '8'                                
03722          MOVE CR-AH-CEX-MO TO D1A-EXIT-MO                         
03723          MOVE CR-AH-CEX-DA TO D1A-EXIT-DA                         
03724          MOVE CR-AH-CEX-YR TO D1A-EXIT-YR                         
03725          MOVE '/' TO D1A-SLASH-1  D1A-SLASH-2                     
03726          MOVE CR-AH-CNC-MO TO D1A-REAS-MO                         
03727          MOVE CR-AH-CNC-DA TO D1A-REAS-DA                         
03728          MOVE CR-AH-CNC-YR TO D1A-REAS-YR                         
03729          MOVE '/' TO D1A-SLASH-3  D1A-SLASH-4                     
03730          MOVE 'CANCELLED  ' TO D1A-REASON                         
03731            GO TO 6900-PRINT-EXCLUSION-DETAIL.                     
03732                                                                   
03733      IF AH-REM-TRM2 NOT GREATER ZERO                              
03734          COMPUTE D1A-EXIT-YR =                                    
03735             ((CR-YR * 12) + CR-MO + CR-AH-TERM - 1) / 12          
03736          COMPUTE D1A-EXIT-MO =                                    
03737           ((CR-YR * 12) + CR-MO + CR-AH-TERM) - (D1A-EXIT-YR * 12)
03738          MOVE CR-DA TO D1A-EXIT-DA                                
03739          MOVE '/' TO D1A-SLASH-1  D1A-SLASH-2                     
03740          MOVE CR-YR TO D1A-REAS-YR                                
03741          MOVE CR-MO TO D1A-REAS-MO                                
03742          MOVE CR-DA TO D1A-REAS-DA                                
03743          MOVE '/' TO D1A-SLASH-3  D1A-SLASH-4                     
03744          MOVE 'EXPIRED    ' TO D1A-REASON.                        
03745                                                                   
03746  6900-PRINT-EXCLUSION-DETAIL.                                     
03747                                                                   
03748      IF (D1L-EXIT-YR = RUN-YR  AND  D1L-EXIT-MO = RUN-MO)  OR     
03749         (D1A-EXIT-YR = RUN-YR  AND  D1A-EXIT-MO = RUN-MO)         
03750          NEXT SENTENCE                                            
03751      ELSE                                                         
03752          GO TO 6999-PRT-EXCLUSIONS-X.                             
03753                                                                   
03754      IF D1L-EXIT-DATE = '00/00/00'                                
03755          MOVE SPACES TO D1L-EXIT-DATE.                            
03756      IF D1A-EXIT-DATE = '00/00/00'                                
03757          MOVE SPACES TO D1A-EXIT-DATE.                            
03758                                                                   
03759      MOVE DTL-1 TO PRT.                                           
03760      MOVE ' ' TO X.                                               
03761                                                                   
03762      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
03763                                                                   
03764      ADD +1 TO WS-LINE.                                           
03765                                                                   
03766  6999-PRT-EXCLUSIONS-X.                                           
03767      EXIT.                                                        
03768  EJECT                                                            
03769  7000-REIN-ONLY-STATUS.                                           
03770 ***  THIS ROUTINE CHECKS FOR THE EXISTENCE OF A 'REAL' CERT THAT  
03771 ***  A REINSURANCE-ONLY CERT CAN MATCH UP WITH.  THE ASSUMPTION   
03772 ***  IS MADE THAT THE REINSURANCE-ONLY CERT WILL HAVE THE SAME    
03773 ***  CONTROL AS THE 'REAL' CERT EXCEPT FOR THE SUFFIX.  ALSO IT   
03774 ***  WILL CHECK THE STATUS OF THE REINSURANCE-ONLY CERT TO SEE    
03775 ***  THAT IT AGREES WITH THE 'REAL' CERT.                         
03776                                                                   
03777 ***  DECLINED CERTIFICATES OR VOIDED CERTIFICATES SHOULD NOT      
03778 ***  GO THROUGH ANY USELESS REINSURANCE PROCESSING LOGIC.         
03779                                                                   
03780      IF CR-ENTRY-STATUS EQUAL 'D' OR 'V'                          
03781          GO TO 7599-EXIT.                                         
03782                                                                   
03783      IF DTE-CLIENT = 'CSL'                                        
03784          IF CR-REIN-TABLE = 'RRR'                                 
03785              GO TO 7599-EXIT.                                     
03786                                                                   
03787      IF DTE-CLIENT = 'FLI' OR 'FLU'                               
03788          MOVE CR-CERT-NO         TO WORK-CERT-NO                  
03789          IF WK-CERT-PREFIX = 'R'                                  
03790              GO TO 7599-EXIT.                                     
03791                                                                   
03792      MOVE CR-FULL-CONTROL        TO ROH-CNTRL (1).                
03793      MOVE CR-ENTRY-STATUS        TO ROH-ENTRY (1).                
03794                                                                   
03795      IF CR-LFTYP = ZEROS                                          
03796          MOVE SPACE              TO ROH-LF-STATUS (1)             
03797          MOVE ZEROS              TO ROH-LF-CALC                   
03798      ELSE                                                         
03799          MOVE CR-LF-CURRENT-STATUS TO ROH-LF-STATUS (1)           
03800          IF ROH-LF-STATUS (1) NOT = '7' AND NOT = '8'             
03801              MOVE '1'             TO ROH-LF-STATUS (1).           
03802                                                                   
03803      IF CR-AHTYP = ZEROS                                          
03804          MOVE SPACE              TO ROH-AH-STATUS (1)             
03805          MOVE ZEROS              TO ROH-AH-CALC                   
03806      ELSE                                                         
03807          MOVE CR-AH-CURRENT-STATUS TO ROH-AH-STATUS (1)           
03808          IF ROH-AH-STATUS (1) NOT = '6' AND NOT = '8'             
03809              MOVE '1'             TO ROH-AH-STATUS (1).           
03810                                                                   
03811      IF CR-ENTRY-STATUS NOT = 'R' AND NOT = '9'                   
03812          GO TO 7500-SHIFT-ROH.                                    
03813                                                                   
03814      IF ROH-CNTRL (1) NOT = ROH-CNTRL (2) AND                     
03815                       NOT = ROH-CNTRL (3) AND                     
03816                       NOT = ROH-CNTRL (4) AND                     
03817                       NOT = ROH-CNTRL (5) AND                     
03818                       NOT = ROH-CNTRL (6)                         
03819          GO TO 7400-ROH-ERROR.                                    
03820                                                                   
03821      MOVE +2 TO SB.                                               
03822                                                                   
03823  7100-ROH-LOOP.                                                   
03824      IF ROH-CNTRL (1) = ROH-CNTRL (SB)                            
03825          GO TO 7300-CK-ROH-STATS.                                 
03826                                                                   
03827  7200-ROH-INCRE.                                                  
03828      ADD +1 TO SB.                                                
03829                                                                   
03830      IF SB LESS +7                                                
03831          GO TO 7100-ROH-LOOP.                                     
03832                                                                   
03833      GO TO 7400-ROH-ERROR.                                        
03834                                                                   
03835  7300-CK-ROH-STATS.                                               
03836      IF ROH-ENTRY (SB) = 'R' OR '9'                               
03837          GO TO 7200-ROH-INCRE.                                    
03838                                                                   
03839      IF ROH-LF-STATUS (1) NOT = SPACE                             
03840          IF ROH-LF-STATUS (1) NOT = ROH-LF-STATUS (SB)            
03841              GO TO 7400-ROH-ERROR.                                
03842                                                                   
03843      IF ROH-AH-STATUS (1) NOT = SPACE                             
03844          IF ROH-AH-STATUS (1) NOT = ROH-AH-STATUS (SB)            
03845              GO TO 7400-ROH-ERROR.                                
03846                                                                   
03847      GO TO 7500-SHIFT-ROH.                                        
03848                                                                   
03849  7400-ROH-ERROR.                                                  
03850                                                                   
03851      IF CR-LFTYP NOT = ZEROS                                      
03852          COMPUTE ROH-LF-CALC = (CR-CCYY * 12) + CR-MO             
03853                                               + CR-LF-TERM.       
03854      IF CR-AHTYP NOT = ZEROS                                      
03855          COMPUTE ROH-AH-CALC = (CR-CCYY * 12) + CR-MO             
03856                                               + CR-AH-TERM.       
03857                                                                   
03858      IF ROH-LF-CALC NOT GREATER CUR-CALC  AND                     
03859         ROH-AH-CALC NOT GREATER CUR-CALC                          
03860          GO TO 7500-SHIFT-ROH.                                    
03861                                                                   
03862      MOVE SPACES                 TO DTL-1.                        
03863      MOVE CR-CARRIER             TO D1-CARRIER.                   
03864      MOVE CR-GROUPING            TO D1-GROUPING.                  
03865      MOVE CR-STATE               TO D1-STATE.                     
03866      MOVE CR-ACCOUNT             TO D1-ACCT.                      
03867      MOVE CR-CERT-NO             TO D1-CERT.                      
03868      MOVE CR-MO                  TO D1-EFF-MO.                    
03869      MOVE CR-DA                  TO D1-EFF-DA.                    
03870      MOVE CR-YR                  TO D1-EFF-YR.                    
03871      MOVE '/'                    TO D1-SLASH-1   D1-SLASH-2.      
03872      MOVE 'POSSIBLE REINSURANCE ONLY STATUS ERROR'                
03873                                  TO D1-REIN-MESSAGE.              
03874                                                                   
03875      IF WS-LINE GREATER +50                                       
03876          PERFORM 5900-HEADING-ROUTINE THRU 5999-HEADINGS-X.       
03877                                                                   
03878      MOVE DTL-1                  TO PRT.                          
03879      MOVE ' '                    TO X.                            
03880                                                                   
03881      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
03882                                                                   
03883      ADD +1                      TO WS-LINE.                      
03884                                                                   
03885  7500-SHIFT-ROH.                                                  
03886                                                                   
03887      MOVE R-O-H (5)              TO R-O-H (6).                    
03888      MOVE R-O-H (4)              TO R-O-H (5).                    
03889      MOVE R-O-H (3)              TO R-O-H (4).                    
03890      MOVE R-O-H (2)              TO R-O-H (3).                    
03891      MOVE R-O-H (1)              TO R-O-H (2).                    
03892                                                                   
03893  7599-EXIT.                                                       
03894      EXIT.                                                        
03895  7600-EXCEP-REPT.                                                 
03896      IF LINER GREATER THAN +60                                    
03897          MOVE HDR-1A             TO PRT                           
03898          MOVE '1'                TO X                             
03899          PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X             
03900          MOVE HDR-2              TO PRT                           
03901          MOVE ' '                TO X                             
03902          PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X             
03903          ADD +1                  TO WS-PAGE                       
03904          MOVE WS-PAGE            TO H3-PAGE                       
03905          MOVE HDR-3              TO PRT                           
03906          PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X             
03907          MOVE HDR-4C             TO PRT                           
03908          MOVE '0'                TO X                             
03909          PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X             
03910          MOVE SPACES TO PRT                                       
03911          PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X             
03912          MOVE +8                 TO LINER.                        
03913                                                                   
03914      MOVE ZEROS                TO WS-LINE.                        
03915      MOVE CR-CARRIER           TO EXP-CARRIER.                    
03916      MOVE CR-GROUPING          TO EXP-GROUPING.                   
03917      MOVE CR-STATE             TO EXP-STATE.                      
03918      MOVE CR-ACCOUNT           TO EXP-ACCT.                       
03919      MOVE CR-CERT-NO           TO EXP-CERT.                       
03920      MOVE CR-MO                TO EXP-EFF-MO.                     
03921      MOVE CR-DA                TO EXP-EFF-DA.                     
03922      MOVE CR-YR                TO EXP-EFF-YR.                     
03923      MOVE '/'                  TO EXP-SLASH-1                     
03924                                   EXP-SLASH-2.                    
03925                                                                   
03926      MOVE WS-EXCEP-REPT-LINE   TO PRT.                            
03927      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
03928      MOVE SPACES               TO   WS-EXCEP-REPT-LINE.           
03929      ADD +2                    TO LINER.                          
03930                                                                   
03931  7699-EXIT.                                                       
03932      EXIT.                                                        
03933                                                                   
03934  EJECT                                                            
03935  8000-CARRIER-BREAK-ROUTINE.                                      
03936                                                                   
03937      IF PRIOR-CARRIER = SPACES                                    
03938          GO TO 8900-INITIALIZE-TOTALS.                            
03939                                                                   
03940      MOVE HDR-1                  TO PRT.                          
03941      MOVE '1'                    TO X.                            
03942                                                                   
03943      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
03944                                                                   
03945      MOVE HDR-2                  TO PRT.                          
03946      MOVE ' '                    TO X.                            
03947                                                                   
03948      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
03949                                                                   
03950      ADD +1                      TO WS-PAGE.                      
03951      MOVE WS-PAGE                TO H3-PAGE.                      
03952      MOVE HDR-3                  TO PRT.                          
03953                                                                   
03954      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
03955                                                                   
03956      MOVE SPACES                 TO PRT.                          
03957      MOVE '-'                    TO X.                            
03958                                                                   
03959      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
03960                                                                   
03961      IF PRINTING-FINAL-TOTALS                                     
03962          MOVE SPACES             TO HDR-6                         
03963      ELSE                                                         
03964          MOVE PRIOR-CARRIER      TO HD6-CARR.                     
03965      MOVE HDR-6 TO PRT.                                           
03966      MOVE ' ' TO X.                                               
03967                                                                   
03968      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
03969                                                                   
03970      MOVE SPACES                 TO TOT-1.                        
03971                                                                   
03972      MOVE TOT-MSG-1              TO T1-MSG.                       
03973      MOVE TOT-READ               TO T1-COUNT1.                    
03974                                                                   
03975      MOVE TOT-1                  TO PRT.                          
03976      MOVE '0'                    TO X.                            
03977                                                                   
03978      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
03979                                                                   
03980      MOVE TOT-MSG-1A             TO T1-MSG.                       
03981      MOVE TOT-REIS               TO T1-COUNT1.                    
03982                                                                   
03983      MOVE TOT-1                  TO PRT.                          
03984      MOVE ' '                    TO X.                            
03985                                                                   
03986      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
03987                                                                   
03988      MOVE TOT-MSG-1B             TO T1-MSG.                       
03989      MOVE TOT-REIO               TO T1-COUNT1.                    
03990                                                                   
03991      MOVE TOT-1                  TO PRT.                          
03992      MOVE ' '                    TO X.                            
03993                                                                   
03994      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
03995                                                                   
03996      MOVE TOT-MSG-1C             TO T1-MSG.                       
03997      MOVE TOT-VOIDED             TO T1-COUNT1.                    
03998                                                                   
03999      MOVE TOT-1                  TO PRT.                          
04000      MOVE ' '                    TO X.                            
04001                                                                   
04002      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04003                                                                   
04004      MOVE TOT-MSG-1D             TO T1-MSG.                       
04005      MOVE TOT-DECLINED           TO T1-COUNT1.                    
04006                                                                   
04007      MOVE TOT-1                  TO PRT.                          
04008      MOVE ' '                    TO X.                            
04009                                                                   
04010      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04011                                                                   
04012      MOVE TOT-MSG-2              TO T1-MSG.                       
04013      MOVE TOT-SNGLPRM            TO T1-COUNT1.                    
04014                                                                   
04015      MOVE TOT-1                  TO PRT.                          
04016      MOVE ' '                    TO X.                            
04017                                                                   
04018      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04019                                                                   
04020      MOVE TOT-MSG-3              TO T1-MSG.                       
04021      MOVE TOT-OB                 TO T1-COUNT1.                    
04022                                                                   
04023      MOVE TOT-1                  TO PRT.                          
04024                                                                   
04025      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04026                                                                   
04027      MOVE TOT-MSG-4              TO T1-MSG.                       
04028      MOVE TOT-SUM                TO T1-COUNT1.                    
04029                                                                   
04030      MOVE TOT-1                  TO PRT.                          
04031                                                                   
04032      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04033                                                                   
04034      MOVE SPACES                 TO PRT.                          
04035      MOVE '0'                    TO X.                            
04036                                                                   
04037      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04038                                                                   
04039      MOVE ' COVERAGE STATISTICS - '  TO HD7-COV.                  
04040      MOVE LIFE-OVERRIDE-L12          TO HD7-DESC-1.               
04041      MOVE   AH-OVERRIDE-L12          TO HD7-DESC-2.               
04042                                                                   
04043      MOVE HDR-7                  TO PRT.                          
04044      MOVE '-'                    TO X.                            
04045                                                                   
04046      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04047                                                                   
04048      MOVE HDR-7A                 TO PRT.                          
04049      MOVE ' '                    TO X.                            
04050                                                                   
04051      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04052                                                                   
04053      MOVE TOT-MSG-5A             TO T4-MSG.                       
04054      MOVE TOT-LF                 TO T4-COUNT1.                    
04055      MOVE TOT-LF-AMT             TO T4-PREM-LF.                   
04056      MOVE TOT-AH                 TO T4-COUNT2.                    
04057      MOVE TOT-AH-AMT             TO T4-PREM-AH.                   
04058                                                                   
04059      MOVE TOT-4                  TO PRT.                          
04060      MOVE '0'                    TO X.                            
04061                                                                   
04062      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04063                                                                   
04064      MOVE TOT-MSG-5              TO T4-MSG.                       
04065      MOVE TOT-CANC-LF            TO T4-COUNT1.                    
04066      MOVE TOT-CANC-LF-AMT        TO T4-PREM-LF.                   
04067      MOVE TOT-CANC-AH            TO T4-COUNT2.                    
04068      MOVE TOT-CANC-AH-AMT        TO T4-PREM-AH.                   
04069                                                                   
04070      MOVE TOT-4                  TO PRT.                          
04071      MOVE '0'                    TO X.                            
04072                                                                   
04073      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04074                                                                   
04075      MOVE TOT-MSG-6              TO T4-MSG.                       
04076      MOVE TOT-DTH                TO T4-COUNT1.                    
04077      MOVE TOT-DTH-AMT            TO T4-PREM-LF.                   
04078      MOVE TOT-DIS                TO T4-COUNT2.                    
04079      MOVE TOT-DIS-AMT            TO T4-PREM-AH.                   
04080                                                                   
04081      MOVE TOT-4                  TO PRT.                          
04082      MOVE ' '                    TO X.                            
04083                                                                   
04084      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04085                                                                   
04086      MOVE TOT-MSG-7              TO T4-MSG.                       
04087      MOVE TOT-EXP-LF             TO T4-COUNT1.                    
04088      MOVE TOT-EXP-LF-AMT         TO T4-PREM-LF.                   
04089      MOVE TOT-EXP-AH             TO T4-COUNT2.                    
04090      MOVE TOT-EXP-AH-AMT         TO T4-PREM-AH.                   
04091                                                                   
04092      MOVE TOT-4                  TO PRT.                          
04093                                                                   
04094      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04095                                                                   
04096      MOVE TOT-MSG-8              TO T4-MSG.                       
04097      MOVE TOT-FUTR-LF            TO T4-COUNT1.                    
04098      MOVE TOT-FUTR-LF-AMT        TO T4-PREM-LF.                   
04099      MOVE TOT-FUTR-AH            TO T4-COUNT2.                    
04100      MOVE TOT-FUTR-AH-AMT        TO T4-PREM-AH.                   
04101                                                                   
04102      MOVE TOT-4                  TO PRT.                          
04103                                                                   
04104      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04105                                                                   
04106      MOVE TOT-MSG-9-OB           TO T4-MSG.                       
04107      MOVE TOT-ACTIVE-LF-OB       TO T4-COUNT1.                    
04108      MOVE TOT-ACT-LF-AMT-OB      TO T4-PREM-LF.                   
04109      MOVE TOT-ACTIVE-AH-OB       TO T4-COUNT2.                    
04110      MOVE TOT-ACT-AH-AMT-OB      TO T4-PREM-AH.                   
04111                                                                   
04112      MOVE TOT-4                  TO PRT.                          
04113                                                                   
04114      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04115                                                                   
04116      MOVE TOT-MSG-9              TO T4-MSG.                       
04117      MOVE TOT-ACTIVE-LF          TO T4-COUNT1.                    
04118      MOVE TOT-ACTIVE-LF-AMT      TO T4-PREM-LF.                   
04119      MOVE TOT-ACTIVE-AH          TO T4-COUNT2.                    
04120      MOVE TOT-ACTIVE-AH-AMT      TO T4-PREM-AH.                   
04121                                                                   
04122      MOVE TOT-4                  TO PRT.                          
04123                                                                   
04124      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04125                                                                   
04126      MOVE SPACES                 TO TOT-1.                        
04127      MOVE TOT-MSG-10             TO T1-MSG.                       
04128      MOVE TOT-ALPHA              TO T1-COUNT1.                    
04129                                                                   
04130      MOVE TOT-1                  TO PRT.                          
04131      MOVE '-'                    TO X.                            
04132                                                                   
04133      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04134                                                                   
04135      MOVE SPACES                 TO TOT-1.                        
04136      MOVE TOT-MSG-10J            TO T1-MSG.                       
04137      MOVE TOT-ALPHA-JOINT        TO T1-COUNT1.                    
04138                                                                   
04139      MOVE TOT-1                  TO PRT.                          
04140      MOVE ' '                    TO X.                            
04141                                                                   
04142      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04143                                                                   
04144      MOVE SPACES                 TO PRT.                          
04145      MOVE '0'                    TO X.                            
04146                                                                   
04147      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04148                                                                   
04149      MOVE HDR-5                  TO PRT.                          
04150                                                                   
04151      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04152                                                                   
04153      MOVE TOT-MSG-11             TO T2-MSG.                       
04154      MOVE TOT-INIT-FACE          TO T2-GROSS.                     
04155      MOVE REI-INIT-FACE          TO T2-REIN.                      
04156                                                                   
04157      MOVE TOT-2                  TO PRT.                          
04158      MOVE '0'                    TO X.                            
04159                                                                   
04160      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04161                                                                   
04162      MOVE TOT-MSG-12             TO T2-MSG.                       
04163      MOVE TOT-REM-FACE           TO T2-GROSS.                     
04164      MOVE REI-REM-FACE           TO T2-REIN.                      
04165                                                                   
04166      MOVE TOT-2                  TO PRT.                          
04167      MOVE ' '                    TO X.                            
04168                                                                   
04169      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04170                                                                   
04171      MOVE TOT-MSG-13             TO T2-MSG.                       
04172      MOVE TOT-REM-AH             TO T2-GROSS.                     
04173      MOVE REI-REM-AH             TO T2-REIN.                      
04174                                                                   
04175      MOVE TOT-2                  TO PRT.                          
04176                                                                   
04177      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04178                                                                   
04179      MOVE TOT-MSG-14             TO T2-MSG.                       
04180      MOVE TOT-FUTR-FACE          TO T2-GROSS.                     
04181      MOVE REI-FUTR-FACE          TO T2-REIN.                      
04182                                                                   
04183      MOVE TOT-2                  TO PRT.                          
04184                                                                   
04185      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04186                                                                   
04187      MOVE TOT-MSG-15             TO T2-MSG.                       
04188      MOVE TOT-MO-DEC             TO T2-GROSS.                     
04189      MOVE REI-MO-DEC             TO T2-REIN.                      
04190                                                                   
04191      MOVE TOT-2                  TO PRT.                          
04192                                                                   
04193      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04194                                                                   
04195      MOVE TOT-MSG-16             TO T3-MSG.                       
04196      MOVE TOT-GAAP               TO T3-GROSS.                     
04197      MOVE REI-GAAP               TO T3-REIN.                      
04198                                                                   
04199      MOVE TOT-3                  TO PRT.                          
04200      MOVE '-'                    TO X.                            
04201                                                                   
04202      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04203                                                                   
04204      MOVE SPACES                 TO PRT.                          
04205      MOVE '0'                    TO X.                            
04206                                                                   
04207      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04208                                                                   
04209      MOVE ' YEAR TO DATE NET PREMIUM - '  TO HD7B-COV.            
04210      MOVE LIFE-OVERRIDE-L12      TO HD7B-DESC-1.                  
04211      MOVE   AH-OVERRIDE-L12      TO HD7B-DESC-2.                  
04212                                                                   
04213      MOVE HDR-7B                 TO PRT.                          
04214      MOVE '-'                    TO X.                            
04215                                                                   
04216      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04217                                                                   
04218      MOVE TOT-MSG-17             TO T2-MSG.                       
04219      MOVE TOT-LF-UNDER60         TO T2-GROSS.                     
04220      MOVE TOT-AH-UNDER60         TO T2-REIN.                      
04221                                                                   
04222      MOVE TOT-2                  TO PRT.                          
04223      MOVE '0'                    TO X.                            
04224                                                                   
04225      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04226                                                                   
04227      MOVE TOT-MSG-18             TO T2-MSG.                       
04228      MOVE TOT-LF-OVER59          TO T2-GROSS.                     
04229      MOVE TOT-AH-OVER59          TO T2-REIN.                      
04230                                                                   
04231      MOVE TOT-2                  TO PRT.                          
04232      MOVE ' '                    TO X.                            
04233                                                                   
04234      PERFORM 5000-PRINT-ROUTINE THRU 5899-PRINT-X.                
04235                                                                   
04236  8800-ADD-TO-FINAL-TOTALS.                                        
04237                                                                   
04238      MOVE +80                    TO WS-LINE                       
04239                                     LINER.                        
04240      ADD TOT-LF-AMT              TO F-TOT-LF-AMT.                 
04241      ADD TOT-AH-AMT              TO F-TOT-AH-AMT.                 
04242      ADD TOT-CANC-LF-AMT         TO F-TOT-CANC-LF-AMT.            
04243      ADD TOT-CANC-AH-AMT         TO F-TOT-CANC-AH-AMT.            
04244      ADD TOT-DTH-AMT             TO F-TOT-DTH-AMT.                
04245      ADD TOT-DIS-AMT             TO F-TOT-DIS-AMT.                
04246      ADD TOT-EXP-LF-AMT          TO F-TOT-EXP-LF-AMT.             
04247      ADD TOT-EXP-AH-AMT          TO F-TOT-EXP-AH-AMT.             
04248      ADD TOT-FUTR-LF-AMT         TO F-TOT-FUTR-LF-AMT.            
04249      ADD TOT-FUTR-AH-AMT         TO F-TOT-FUTR-AH-AMT.            
04250      ADD TOT-ACTIVE-LF-AMT       TO F-TOT-ACTIVE-LF-AMT.          
04251      ADD TOT-ACTIVE-AH-AMT       TO F-TOT-ACTIVE-AH-AMT.          
04252      ADD TOT-ACT-LF-AMT-OB       TO F-TOT-ACT-LF-AMT-OB.          
04253      ADD TOT-ACT-AH-AMT-OB       TO F-TOT-ACT-AH-AMT-OB.          
04254                                                                   
04255      ADD TOT-READ                TO F-TOT-READ.                   
04256      ADD TOT-REIS                TO F-TOT-REIS.                   
04257      ADD TOT-VOIDED              TO F-TOT-VOIDED.                 
04258      ADD TOT-DECLINED            TO F-TOT-DECLINED.               
04259      ADD TOT-REIO                TO F-TOT-REIO.                   
04260      ADD TOT-SNGLPRM             TO F-TOT-SNGLPRM.                
04261      ADD TOT-OB                  TO F-TOT-OB.                     
04262      ADD TOT-SUM                 TO F-TOT-SUM.                    
04263      ADD TOT-LF                  TO F-TOT-LF.                     
04264      ADD TOT-AH                  TO F-TOT-AH.                     
04265      ADD TOT-CANC-LF             TO F-TOT-CANC-LF.                
04266      ADD TOT-CANC-AH             TO F-TOT-CANC-AH.                
04267      ADD TOT-DTH                 TO F-TOT-DTH.                    
04268      ADD TOT-DIS                 TO F-TOT-DIS.                    
04269      ADD TOT-EXP-LF              TO F-TOT-EXP-LF.                 
04270      ADD TOT-EXP-AH              TO F-TOT-EXP-AH.                 
04271      ADD TOT-FUTR-LF             TO F-TOT-FUTR-LF.                
04272      ADD TOT-FUTR-AH             TO F-TOT-FUTR-AH.                
04273      ADD TOT-ACTIVE-LF           TO F-TOT-ACTIVE-LF.              
04274      ADD TOT-ACTIVE-AH           TO F-TOT-ACTIVE-AH.              
04275      ADD TOT-ACTIVE-LF-OB        TO F-TOT-ACTIVE-LF-OB.           
04276      ADD TOT-ACTIVE-AH-OB        TO F-TOT-ACTIVE-AH-OB.           
04277      ADD TOT-ALPHA               TO F-TOT-ALPHA.                  
04278      ADD TOT-ALPHA-JOINT         TO F-TOT-ALPHA-JOINT.            
04279      ADD TOT-GAAP                TO F-TOT-GAAP.                   
04280      ADD REI-GAAP                TO F-REI-GAAP.                   
04281      ADD TOT-INIT-FACE           TO F-TOT-INIT-FACE.              
04282      ADD REI-INIT-FACE           TO F-REI-INIT-FACE.              
04283      ADD TOT-REM-FACE            TO F-TOT-REM-FACE.               
04284      ADD REI-REM-FACE            TO F-REI-REM-FACE.               
04285      ADD TOT-REM-AH              TO F-TOT-REM-AH.                 
04286      ADD REI-REM-AH              TO F-REI-REM-AH.                 
04287      ADD TOT-FUTR-FACE           TO F-TOT-FUTR-FACE.              
04288      ADD REI-FUTR-FACE           TO F-REI-FUTR-FACE.              
04289      ADD TOT-MO-DEC              TO F-TOT-MO-DEC.                 
04290      ADD REI-MO-DEC              TO F-REI-MO-DEC.                 
04291                                                                   
04292      ADD TOT-LF-OVER59           TO F-TOT-LF-OVER59.              
04293      ADD TOT-LF-UNDER60          TO F-TOT-LF-UNDER60.             
04294      ADD TOT-AH-OVER59           TO F-TOT-AH-OVER59.              
04295      ADD TOT-AH-UNDER60          TO F-TOT-AH-UNDER60.             
04296                                                                   
04297  8900-INITIALIZE-TOTALS.                                          
04298                                                                   
04299      MOVE ZEROS                  TO TOT-LF-OVER59  TOT-AH-OVER59  
04300                                     TOT-LF-UNDER60 TOT-AH-UNDER60.
04301      MOVE ZERO-ACCUMS            TO TOT-ACCUMS.                   
04302      MOVE CR-CARRIER             TO PRIOR-CARRIER.                
04303                                                                   
04304  8910-SET-CARRIER.                                                
04305      IF CARRIER-SUB (1) = SPACE                                   
04306          MOVE SPACES             TO DOMICILE-STATE                
04307          GO TO 8999-EXIT.                                         
04308                                                                   
04309      MOVE +0                     TO S1.                           
04310  8915-S-C-LOOP.                                                   
04311                                                                   
04312      ADD +1                      TO S1.                           
04313                                                                   
04314      IF S1 GREATER THAN CLAS-MAXCN                                
04315          MOVE SPACES             TO DOMICILE-STATE                
04316          GO TO 8999-EXIT.                                         
04317                                                                   
04318      IF PRIOR-CARRIER = CARRIER-SUB (S1)                          
04319          MOVE CARRIER-DOM-ST (S1) TO DOMICILE-STATE               
04320          GO TO 8999-EXIT.                                         
04321                                                                   
04322      GO TO 8915-S-C-LOOP.                                         
04323                                                                   
04324  8999-EXIT.                                                       
04325      EXIT.                                                        
04326  EJECT                                                            
04327  ABEND-PGM.                                                       
04328                              COPY ELCABEND.                       
04329  EJECT                                                            
04330  9000-END-THE-JOB.                                                
04331                                                                   
04332      IF DTE-CLIENT EQUAL 'NCL'                                    
04333            PERFORM 0395-CHECK-OUT-OB THRU 0395-EXIT.              
04334                                                                   
04335      MOVE CR-ACCT-CONTROL     TO WS-HOLD-AM-CONTROL.              
04336                                                                   
04337      PERFORM 8000-CARRIER-BREAK-ROUTINE THRU 8999-EXIT.           
04338                                                                   
04339      MOVE FIN-TOT-ACCUMS         TO TOT-ACCUMS.                   
04340      MOVE F-TOT-LF-OVER59        TO TOT-LF-OVER59.                
04341      MOVE F-TOT-LF-UNDER60       TO TOT-LF-UNDER60.               
04342      MOVE F-TOT-AH-OVER59        TO TOT-AH-OVER59.                
04343      MOVE F-TOT-AH-UNDER60       TO TOT-AH-UNDER60.               
04344                                                                   
04345      MOVE '*'                    TO PRIOR-CARRIER.                
04346                                                                   
04347      PERFORM 8000-CARRIER-BREAK-ROUTINE.                          
04348                                                                   
04349  LOAD-GAAP-WS-DATES.                                              
04350      COPY ELCGAPM1.                                               
04351                                                                   
04352 *UNLOAD-GAAP-WS-DATES.                                            
04353 *****COPY ELCGAPM2.                                               
04354                                                                   
04355  REIN-DATE-LOAD.                                                  
04356      COPY ELCRENM1.                                               
04357                                                                   
04358  LOAD-ALPHA-WS-DATES.                                             
04359      COPY ELCAEXM1.                                               
04360                                                                   
04361 *UNLOAD-ALPHA-WS-DATES.                                           
04362 *****COPY ELCAEXM2.                                               
04363                                                                   
04364 *LOAD-CERT-WS-DATES.                                              
04365 *****COPY ELCCRTM1.                                               
04366                                                                   
04367 *UNLOAD-CERT-WS-DATES.                                            
04368 *****COPY ELCCRTM2.                                               
04369                                                                   
04370  9900-CLOSE-FILES.                                                
04371                                                                   
04372      DISPLAY '     '.                                             
04373      DISPLAY '     '.                                             
04374      DISPLAY '     '.                                             
04375      DISPLAY 'GREATEST REMAINING TERM-LIFE: ' OLDEST-LF-REMTERM.  
04376      DISPLAY '     '.                                             
04377      DISPLAY '    REMAINING TERMS OVER 100: ' LF-OVER-100.        
04378      DISPLAY '     '.                                             
04379      DISPLAY '     '.                                             
04380      DISPLAY 'GREATEST REMAINING TERM- A&H: ' OLDEST-AH-REMTERM.  
04381      DISPLAY '     '.                                             
04382      DISPLAY '     '.                                             
04383                                                                   
04384      CLOSE ERACCTT                                                
04385            CERT-MASTER                                            
04386            PRINTER.                                               
04387                                                                   
04388      IF DTE-PGM-OPT = ('1' OR '3')                                
04389          CLOSE ALPHA-EXTRACT.                                     
04390                                                                   
04391      IF DTE-PGM-OPT = ('2' OR '3')                                
04392          CLOSE GAAP-EXTRACT.                                      
04393                                                                   
04394      IF REIN-OPEN-SW = 'X'                                        
04395          CLOSE ERRTBL-IN                                             
04396          IF ERRTBL-FILE-STATUS NOT = '00'                         
04397              MOVE '22'               TO W-ABEND-CODE-1            
04398              MOVE ERRTBL-FILE-STATUS TO W-ABEND-CODE-2            
04399              MOVE WS-ABEND-CODE      TO WS-RETURN-CODE            
04400              GO TO ABEND-PGM.                                     
04401                                                                   
04402      MOVE 'C'                    TO CP-IO-FUNCTION.               
04403      PERFORM 3090-CALL-RATING-ROUTINE THRU 3099-EXIT.             
04404      IF IO-ERROR                                                  
04405          MOVE 0302               TO WS-RETURN-CODE                
04406          MOVE 'ERROR OCCURED CLOSING - ELRATE'                    
04407                                  TO WS-ABEND-MESSAGE              
04408          GO TO ABEND-PGM.                                         
04409                                                                   
04410  9990-CLOSE-FICH.                                                 
04411                              COPY ELCPRTC.                        
04412                                                                   
04413  9999-END-OF-JOB.                                                 
04414                                                                   
04415      IF ME-DO-UPDATE                                              
04416          MOVE DTE-CLIENT         TO ME-COMPANY                    
04417          MOVE MONTH-END-MOYR     TO ME-MOYR                       
04418          READ ERMEBL                                              
04419          IF ERMEBL-FILE-STATUS NOT = '00'                         
04420              MOVE 'N'            TO ME-UPDATE-FLAG                
04421              CLOSE ERMEBL.                                        
04422                                                                   
04423      IF ME-DO-UPDATE                                              
04424          MOVE F-TOT-READ         TO ME-050-CERT-IN                
04425                                     ME-050-CERT-OUT               
04426          MOVE ME-START-TIME      TO ME-050-START                  
04427          MOVE ME-CNDS-DATE       TO ME-050-RUN-DT                 
04428          ACCEPT WS-TIME-OF-DAY   FROM TIME                        
04429          MOVE WS-TIME            TO ME-050-END                    
04430          ADD 1                   TO ME-050-RUN-CT                 
04431          REWRITE MONTH-END-BALANCES                               
04432          CLOSE ERMEBL                                             
04433          DISPLAY 'MONTH-END BALANCES POSTED'                      
04434      ELSE                                                         
04435          DISPLAY 'MONTH-END BALANCES NOT POSTED'.                 
SUNPSD     DISPLAY "SUNSPD READCRT-CTR="   READCRT-CTR.
SUNPSD     DISPLAY "SUNPSD ELRATEX-CTR="   ELRATEX-CTR.
SUNPSD     DISPLAY "SUNPSD ELRAMTX-CTR="   ELRAMTX-CTR.
SUNPSD     DISPLAY "SUNPSD ELRTRMX-CTR="   ELRTRMX-CTR.
SUNPSD     DISPLAY "SUNPSD ELUPRMX-CTR="   ELUPRMX-CTR.
SUNPSD     DISPLAY "SUNPSD ELDATCX-CTR="   ELDATCX-CTR.
SUNPSD     DISPLAY "SUNPSD DIFFTME-CTR="   DIFFTME-CTR.
SUNPSD     DISPLAY "SUNPSD --------------------------".
SUNPSD     DISPLAY "SUNPSD ALLCALL-CTR="   ALLCALL-CTR.    
04436                                                                   
04437      GOBACK.                                                      
