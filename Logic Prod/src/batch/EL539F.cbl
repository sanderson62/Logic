00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL539F.
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 02/12/96 16:38:04.                 
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            
00008 *                            VMOD=2.015.                          
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
00026 *        GENERATES, SORTS AND PRINTS PENETRATION REPORT.          
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING  
022804* 022804                   SMVA  HANDLE FILE STATUS 23 ON ERLOFC        
052104* 052104   2004050500004   PEMA  USE CURRENT ACCT DT RANGE
092905* 092905 CR2005080300007   PEMA  ADD DISCLAIMER TO ACCOUNT SUMMARY
110105* 110105   2005071200004   PEMA  INCREASE SIZE OF LOAN OFFICER
011606* 011606 IR2006120700003   PEMA  CORRECT PAGE BREAK FOR DISCLAIMER
122002******************************************************************
00027  EJECT                                                            
00028  ENVIRONMENT DIVISION.                                            
00029  CONFIGURATION SECTION.                                           
00030  INPUT-OUTPUT SECTION.                                            
00031  FILE-CONTROL.                                                    
00032                                                                   
00033      SELECT SORT-WORK    ASSIGN SYS001-UT-FBA1-S-SORTWK1.         
00034                                                                   
00035      SELECT PRINT-FILE   ASSIGN SYS008-UR-1403-S-SYS008.          
00036                                                                   
00037      SELECT EXTRACT-INTERFACE-FILE                                
00038                          ASSIGN SYS010-UT-2400-S-SYS010.          
00039                                                                   
00040      SELECT DISK-DATE    ASSIGN SYS019-UT-FBA1-SYS019.            
00041                                                                   
00042      SELECT FICH         ASSIGN SYS020-UT-2400-S-SYS020.          
00043                                                                   
00044      SELECT ELREPT       ASSIGN SYS021-FBA1-ELREPT                
00045                          ORGANIZATION IS INDEXED                  
00046                          ACCESS IS DYNAMIC                        
00047                          RECORD KEY IS RF-CONTROL-PRIMARY         
00048                          FILE STATUS IS DTE-VSAM-FLAGS.           
00049                                                                   
00050      SELECT ERACCT       ASSIGN SYS022-FBA1-ERACCT                
00051                          ORGANIZATION IS INDEXED                  
00052                          ACCESS IS DYNAMIC                        
00053                          RECORD KEY IS AM-CONTROL-PRIMARY         
00054                          FILE STATUS IS AM-FILE-STATUS.           
00055                                                                   
00056      SELECT ERLOFC       ASSIGN SYS023-FBA1-ERLOFC                
00057                          ORGANIZATION IS INDEXED                  
00058                          ACCESS IS DYNAMIC                        
00059                          RECORD KEY IS LO-CONTROL-PRIMARY         
00060                          FILE STATUS IS LO-STATUS.                
00061                                                                   
00062      SELECT ERPNDB       ASSIGN SYS024-FBA1-ERPNDB2               
00063                          ORGANIZATION IS INDEXED                  
00064                          ACCESS IS DYNAMIC                        
00065                          RECORD KEY IS PB-CONTROL-BY-ACCT         
00066                          FILE STATUS IS PB-STATUS.                
00067  EJECT                                                            
00068  DATA DIVISION.                                                   
00069  FILE SECTION.                                                    
00070                                                                   
00071  SD  SORT-WORK.                                                   
00072                                                                   
00073  01  SORT-RECORD.                                                 
00074      12  SORT-CTL.                                                
00075          16  SC-ACCT-INFO.                                        
00076              20  SC-CO       PIC  X(01).                          
00077              20  SC-CARR     PIC  X(01).                          
00078              20  SC-GRP      PIC  X(06).                          
00079              20  SC-ST       PIC  X(02).                          
00080              20  SC-ACCT     PIC  X(10).                          
110105         16  SC-LNOFF        PIC  X(05).
00082          16  SC-EFFDT        PIC  X(06).                          
00083          16  SC-CERT         PIC  X(11).                          
00084      12  SORT-DTA            PIC  X(82).
00085  EJECT                                                            
00086  FD  PRINT-FILE                                                   
00087                          COPY ELCPRTFD.                           
00088  EJECT                                                            
00089  FD  EXTRACT-INTERFACE-FILE                                       
00090                          COPY ERCEXTFD.                           
00091                                                                   
00092  01  EXTRACT-INTERFACE-FILE-RECORD   PIC  X(629).                 
00093  EJECT                                                            
00094  FD  DISK-DATE                                                    
00095                          COPY ELCDTEFD.                           
00096  EJECT                                                            
00097  FD  FICH                                                         
00098                          COPY ELCFCHFD.                           
00099  EJECT                                                            
00100  FD  ELREPT                                                       
00101                          COPY ELCRPTFD.                           
00102                                                                   
00103      COPY ELCREPT.                                                
00104  EJECT                                                            
00105  FD  ERACCT.                                                      
00106                                                                   
00107      COPY ERCACCT.                                                
00108  EJECT                                                            
00109  FD  ERLOFC.                                                      
00110                                                                   
00111      COPY ERCLOFC.                                                
00112  EJECT                                                            
00113  FD  ERPNDB.                                                      
00114                                                                   
00115  01  PB-RECORD.                                                   
00116      12  FILLER              PIC  X(13).                          
00117      12  PB-CONTROL-BY-ACCT.                                      
00118          16  PB-COMP-CD      PIC  X(01).                          
00119          16  FILLER          PIC  X(35).                          
00120      12  FILLER              PIC  X(536).                         
00121  EJECT                                                            
00122  WORKING-STORAGE SECTION.                                         
00123  77  LCP-ASA                       PIC X.                         
00124  77  FILLER  PIC  X(32) VALUE '********************************'. 
00125  77  FILLER  PIC  X(32) VALUE '*   EL539F WORKING-STORAGE     *'. 
00126  77  FILLER  PIC  X(32) VALUE '***********VMOD=2.015 **********'. 
00127                                                                   
00128  77  PGM-SUB                 PIC S9(03)  COMP    VALUE +539.      
PEMMOD 77  WK1                     PIC S999    COMP-3  VALUE +0.
PEMMOD 77  WK2                     PIC S999    COMP-3  VALUE +0.
00129  77  WS-RETURN-CODE          PIC S9(04)  COMP    VALUE +0.        
00130  77  TBLX                    PIC S9(04)  COMP.                    
00131  77  AX                      PIC S9(04)  COMP.                    
00132  77  WS-ZERO                 PIC S9(01)  COMP-3  VALUE +0.        
00133  77  LNCT                    PIC S9(02)  COMP-3  VALUE +80.       
00134  77  PGCT                    PIC S9(05)  COMP-3  VALUE +0.        
00135  77  X                       PIC  X(01).                          
00136  77  LO-OPT1                 PIC  X(01).                          
00137  77  LO-OPT2                 PIC  X(01).                          
00138  77  SKIP-THIS-ACCT          PIC  X(01)          VALUE 'N'.       
00139  77  SKIP-THIS-LOAN-OFCR     PIC  X(01)          VALUE 'N'.       
00140  77  HAVE-DATA               PIC  X(01)          VALUE 'N'.       
00141  77  LO-UPD                  PIC  X(01).                          
00142  77  HAVE-HIGH-VALUE         PIC  X(01)          VALUE 'N'.       
00143  77  PRNTG-SUMM              PIC  X(01)          VALUE 'N'.       
00144  77  PRNTG-DTL               PIC  X(01)          VALUE 'N'.       
00145  77  ANY-COMP                PIC  X(01).                          
00146  77  WS-ABEND-FILE-STATUS    PIC  X(02)          VALUE ZERO.      
00147  77  OLC-REPORT-NAME         PIC  X(06)          VALUE 'EL539'.   
00148  77  SAVE-ACCOUNT            PIC  X(10).                          
00149  77  LST-ACCT                PIC  X(20)          VALUE LOW-VALUE. 
00150  77  KEY-20                  PIC  X(20).                          
110105 77  LST-OFFCR               PIC  X(25)          VALUE LOW-VALUE. 
110105 77  KEY-23                  PIC  X(25).                          
110105 77  HLD-CTL                 PIC  X(25)          VALUE LOW-VALUE. 
00154  77  WS-ABEND-MESSAGE        PIC  X(80)          VALUE SPACES.    
PEMMOD 77  WS-WORK-AMT             PIC S9(9)V99 COMP-3 VALUE +0.
PEMMOD 77  WS-FIX-CNT              PIC 9(05) VALUE ZEROS.
00155  EJECT                                                            
052104 01  WS-HOLD-ERACCT          PIC X(2000) VALUE SPACES.
00156  01  MISC-WS.                                                     
PEMMOD     12  WS-BANK             PIC  X(10) VALUE SPACES.
00157      12  PB-STATUS.                                               
00158          16  PB-STAT-1       PIC  X(01).                          
00159          16  PB-STAT-2       PIC  X(01).                          
00160      12  AM-FILE-STATUS.                                          
00161          16  AM-STAT-1       PIC  X(01).                          
00162          16  AM-STAT-2       PIC  X(01).                          
00163      12  LO-STATUS.                                               
00164          16  LO-STAT-1       PIC  X(01).                          
00165          16  LO-STAT-2       PIC  X(01).                          
00166      12  CTLKEY.                                                  
00167          16  CPGCTL.                                              
00168              20  CTLCO       PIC  X(01).                          
00169              20  CTLCARR     PIC  X(01).                          
00170              20  CTLGRP      PIC  X(06).                          
00171              20  CTLST       PIC  X(02).                          
00172              20  CTLACCT     PIC  X(10).                          
00173          16  FILLER.                                              
110105             20  CTLOFCR     PIC  X(05).
00175      12  PRVKEY.                                                  
00176          16  PPGCTL          PIC  X(20)      VALUE LOW-VALUES.    
110105         16  PLO             PIC  X(05)      VALUE LOW-VALUES.    
00178      12  BUILD-LO-KEY.                                            
00179          16  BLK-COMPANY     PIC  X(01).                          
00180          16  BLK-CARRIER     PIC  X(01).                          
00181          16  BLK-GROUPING    PIC  X(06).                          
00182          16  BLK-STATE       PIC  X(02).                          
00183          16  BLK-ACCT        PIC  X(10).                          
00184      12  TOT-COMM            PIC S9(11)V99  COMP-3.               
00185      12  HOLD-PROCESS-MO-YR.                                      
00186          16  HOLD-PROCESS-CC PIC  XX.                             
00187          16  HOLD-PROCESS-YR PIC  XX.                             
00188          16  HOLD-PROCESS-MO PIC  XX.                             
00189      12  WS-PEND-YRMODA      PIC 9(11).                           
00190      12  WS-PEND-YRMODA-R REDEFINES WS-PEND-YRMODA.               
00191          16  FILLER          PIC  9(03).                          
00192          16  PEND-CC         PIC  9(02).                          
00193          16  PEND-YR         PIC  9(02).                          
00194          16  PEND-MO         PIC  9(02).                          
00195          16  PEND-DA         PIC  9(02).                          
00196  EJECT                                                            
00197  01  CONAMREC.                                                    
00198      12  CONA-LOVAL          PIC  X(20).                          
00199      12  CONA-NAME           PIC  X(30).                          
00200      12  CONA-ANDT           PIC  X(18).                          
00201                                                                   
00202  01  ACCT-REC.                                                    
00203      12  ACCT-INFO.                                               
00204          16  ACCT-CO         PIC  X(01).                          
00205          16  ACCT-CARR       PIC  X(01).                          
00206          16  ACCT-GRP        PIC  X(06).                          
00207          16  ACCT-ST         PIC  X(02).                          
00208          16  ACCT-NUM        PIC  X(10).                          
110105     12  ACCT-LOVAL          PIC  X(05).
00210      12  ACCT-NAME           PIC  X(30).                          
00211      12  ACCT-PERSON         PIC  X(20).                          
00212      12  ACCT-ADDR1          PIC  X(20).                          
00213      12  ACCT-CTY-ST         PIC  X(20).                          
00214      12  ACCT-ZIP.                                                
00215          16  ACCT-ZIP-PRIME  PIC  X(05).                          
00216          16  ACCT-ZIP-PLUS4  PIC  X(04).                          
00217      12  ACCT-CANADIAN-POSTAL-CODE REDEFINES ACCT-ZIP.            
00218          16  ACCT-CAN-POSTAL-CODE-1.                              
00219              20  FILLER      PIC  X(01).                          
00220                  88 ACCT-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.   
00221              20  FILLER      PIC  X(02).                          
00222          16  ACCT-CAN-POSTAL-CODE-2                               
00223                              PIC  X(03).                          
00224          16  ACCT-CAN-FILLER PIC  X(03).                          
00225                                                                   
00226  01  LN-REC.                                                      
00227      12  LN-CO               PIC  X(01).                          
00228      12  LN-CARR             PIC  X(01).                          
00229      12  LN-GRP              PIC  X(06).                          
00230      12  LN-ST               PIC  X(02).                          
00231      12  LN-ACCT             PIC  X(10).                          
110105     12  LN-OFFCODE          PIC  X(05).
00233      12  LN-LOVAL            PIC  X(06).                          
00234      12  LN-OFFCR            PIC  X(30).                          
00235      12  LN-OPT1             PIC  X(01).                          
00236      12  LN-OPT2             PIC  X(01).                          
00237                                                                   
00238  01  PEND-REC.                                                    
00239      12  PEND-CO             PIC  X(01).                          
00240      12  PEND-CARR           PIC  X(01).                          
00241      12  PEND-GRP            PIC  X(06).                          
00242      12  PEND-ST             PIC  X(02).                          
00243      12  PEND-ACCT           PIC  X(10).                          
110105     12  PEND-OFFICER        PIC  X(05).                          
00245      12  PEND-YRMODA         PIC  9(11)  COMP-3.                  
00246      12  PEND-CERT-PRIME     PIC  X(10).                          
00247      12  PEND-CERT-SFX       PIC  X(01).                          
00248      12  PEND-NAME           PIC  X(15).                          
00249      12  PEND-INIT           PIC  X(02).                          
00250      12  PEND-AGE            PIC  9(02).                          
00251      12  PEND-LF-TYP         PIC  X(03).                          
00252      12  PEND-LF-PRM         PIC S9(07)V99   COMP-3.              
00253      12  PEND-LF-COM         PIC S9(07)V99   COMP-3.              
00254      12  PEND-LF-AMT         PIC S9(09)V99   COMP-3.              
00255      12  PEND-AH-TYP         PIC  X(03).                          
00256      12  PEND-AH-PRM         PIC S9(07)V99   COMP-3.              
00257      12  PEND-AH-AMT         PIC S9(09)V99   COMP-3.              
00258      12  PEND-AH-COM         PIC S9(07)V99   COMP-3.              
00259      12  PEND-LF-TERM        PIC S9(03)      COMP-3.              
00260      12  PEND-AH-TERM        PIC S9(03)      COMP-3.              
00261  EJECT                                                            
00262  01  HD-1.                                                        
00263      12  FILLER              PIC  X(46)          VALUE SPACES.    
00264      12  FILLER              PIC  X(31)          VALUE            
00265              'LOAN PRODUCTION AND PENETRATION'.                   
00266      12  FILLER              PIC  X(42)          VALUE SPACES.    
00267      12  FILLER              PIC  X(06)          VALUE 'EL539'.   
00268                                                                   
00269  01  HD-2.                                                        
00270      12  FILLER              PIC  X(48)          VALUE SPACES.    
00271      12  HD2-CO              PIC  X(30).                          
00272      12  FILLER              PIC  X(41)          VALUE SPACES.    
00273      12  HD2-RUN             PIC  X(08).                          
00274                                                                   
00275  01  HD-3.                                                        
00276      12  FILLER              PIC  X(54)          VALUE SPACES.    
00277      12  HD3-DATE            PIC  X(18).                          
00278      12  FILLER              PIC  X(47)          VALUE SPACES.    
00279      12  FILLER              PIC  X(05)          VALUE 'PAGE '.   
00280      12  HD3-PG              PIC ZZZ,ZZ9.                         
00281                                                                   
00282  01  HD-4.                                                        
00283      12  FILLER              PIC  X(24)          VALUE            
00284              'CARR  GROUP ST  ACCOUNT'.                           
00285                                                                   
00286  01  HD-5.                                                        
00287      12  FILLER              PIC  X(02)          VALUE SPACE.     
00288      12  H5-CARR             PIC  X(01).                          
00289      12  FILLER              PIC  X(02)          VALUE SPACE.     
00290      12  H5-GRP              PIC  X(06).                          
00291      12  FILLER              PIC  X(01)          VALUE SPACE.     
00292      12  H5-ST               PIC  X(02).                          
00293      12  FILLER              PIC  X(01)          VALUE SPACE.     
00294      12  H5-ACCT             PIC  X(10).                          
00295                                                                   
00296  01  HD-6.                                                        
00297      12  H6-NA               PIC  X(30).                          
00298      12  FILLER              PIC  X(27)          VALUE SPACES.    
00299      12  H6-DS               PIC  X(19).                          
00300                                                                   
00301  01  HD-7.                                                        
00302      12  H7-NA               PIC  X(30).                          
00303      12  FILLER              PIC  X(27)          VALUE SPACES.    
00304      12  FILLER              PIC  X(19)          VALUE            
00305              '-------------------'.                               
00306                                                                   
00307  01  HD-8.                                                        
00308      12  H8-NA               PIC  X(30).                          
00309                                                                   
00310  01  HD-9.                                                        
00311      12  H9-NA               PIC  X(30).                          
00312      12  H9-ZIP.                                                  
00313          16  H9-ZIP-PRIME    PIC  X(05).                          
00314          16  H9-DASH         PIC  X(01).                          
00315          16  H9-ZIP-PLUS4    PIC  X(04).                          
00316      12  H9-CANADIAN-POSTAL-CODE REDEFINES H9-ZIP.                
00317          16  H9-CAN-POSTAL-CODE-1                                 
00318                              PIC  X(03).                          
00319          16  H9-DASH-CAN     PIC  X(01).                          
00320          16  H9-CAN-POSTAL-CODE-2                                 
00321                              PIC  X(03).                          
00322          16  H9-CAN-FILLER   PIC  X(03).                          
00323  EJECT                                                            
00324  01  HD-10.                                                       
110105     12  H10-LOF             PIC  X(05).
00326      12  H10-DSH             PIC  X(01).                          
00327      12  H10-LONAME          PIC  X(20).                          
00328                                                                   
00329  01  HD-11.                                                       
00330      12  FILLER              PIC  X(44)          VALUE            
00331              'CERTIFICATE  EFFECTIVE     INSURED          '.      
00332      12  FILLER              PIC  X(15)          VALUE            
00333              '-------------  '.                                   
00334      12  H11-LF              PIC  X(06).                          
00335      12  FILLER              PIC  X(33)          VALUE            
00336              '  --------------  -------------  '.                 
00337      12  H11-AH              PIC  X(06).                          
00338      12  FILLER              PIC  X(17)          VALUE            
00339              '  -------------- '.                                 
00340                                                                   
00341  01  HD-12.                                                       
00342      12  FILLER              PIC  X(44)          VALUE            
00343              'NUMBER          DATE        NAME       AGE T'.      
00344      12  FILLER              PIC  X(44)          VALUE            
PEMMOD             'ERM TYPE   PREM/REFUND       LIFE BEN  TERM '.      
00346      12  FILLER              PIC  X(34)          VALUE            
PEMMOD             'TYPE  PREM/REFUND        A&H BEN  '.                
00348      12  HD9-VAR             PIC  X(10).                          
00349  EJECT                                                            
00350  01  DT-1.                                                        
00351      12  D1-CERT-PRIME       PIC  X(10).                          
00352      12  FILLER              PIC  X(01).                          
00353      12  D1-CERT-SFX         PIC  X(01).                          
00354      12  FILLER              PIC  X(01).                          
00355      12  D1-EFF.                                                  
00356          16  D1-EFMO         PIC  9(02).                          
00357          16  D1-SL1          PIC  X(01).                          
00358          16  D1-EFDA         PIC  9(02).                          
00359          16  D1-SL2          PIC  X(01).                          
00360          16  D1-EFYR         PIC  9(02).                          
00361      12  FILLER              PIC  X(01).                          
00362      12  D1-NAME             PIC  X(14).                          
00363      12  FILLER              PIC  X(01).                          
00364      12  D1-INIT             PIC  X(01).                          
00365      12  FILLER              PIC  X(01).                          
00366      12  D1-AGE              PIC  9(02).                          
00367      12  FILLER              PIC  X(02).                          
00368      12  D1-LF-TERM          PIC ZZ9-            BLANK WHEN ZERO. 
00369      12  FILLER              PIC  X(01).                          
00370      12  D1-LTYP             PIC  X(03).                          
00371      12  FILLER              PIC  X(01).                          
00372      12  D1-LPRM             PIC ZZ,ZZZ,ZZZ.99-  BLANK WHEN ZERO. 
00373      12  FILLER              PIC  X(01).                          
00374      12  D1-LBEN             PIC ZZZ,ZZZ,ZZZ.99- BLANK WHEN ZERO. 
00375      12  FILLER              PIC  X(01).                          
00376      12  D1-AH-TERM          PIC ZZ9-            BLANK WHEN ZERO. 
00377      12  FILLER              PIC  X(01).                          
00378      12  D1-HTYP             PIC  X(03).                          
00379      12  FILLER              PIC  X(01).                          
00380      12  D1-HPRM             PIC Z,ZZZ,ZZZ.99-   BLANK WHEN ZERO. 
PEMTST*    12  FILLER              PIC  X(01).
00382      12  D1-HBEN             PIC ZZZ,ZZZ,ZZZ.99- BLANK WHEN ZERO. 
00383      12  D1-COMM             PIC ZZZZ,ZZZ.99-    BLANK WHEN ZERO. 
00384                                                                   
00385  01  SUM-HD1.                                                     
00386      12  FILLER              PIC  X(44)          VALUE            
00387              '                        ----- LOANS ----- --'.      
00388      12  FILLER              PIC  X(18)          VALUE            
00389              '----------------- '.                                
00390      12  SUM-HD1-LF          PIC  X(06).                          
00391      12  FILLER              PIC  X(37)          VALUE            
00392              ' ----------------- ------------------'.             
00393      12  SUM-HD1-AH          PIC  X(06).                          
00394      12  FILLER              PIC  X(21)          VALUE            
00395              '  ------------------ '.                             
00396                                                                   
00397  01  SUM-HD2.                                                     
00398      12  FILLER              PIC X(44)           VALUE            
00399              'CODE  NAME              COUNT     AMOUNT  CO'.      
00400      12  FILLER              PIC X(44)           VALUE            
00401              'UNT   PCT       BENEFIT   PCT     PREM/REF C'.      
00402      12  FILLER              PIC X(44)           VALUE            
00403              'OUNT   PCT       BENEFIT   PCT     PREM/REF '.      

092905 01  DISC-LINE-1.
           12  FILLER                  PIC X(6)       VALUE SPACES.
           12  FILLER                  PIC X(126)     VALUE
             'Just a reminder, if you are using the reports to pay compe
      -      'nsation to those people listed, they must be licensed '.

092905 01  DISC-LINE-2.
           12  FILLER                  PIC XX         VALUE SPACES.
           12  FILLER                  PIC X(130)     VALUE
             'and appointed with CSO if applicable in your state. To det
      -      'ermine who is licensed and appointed with CSO, please '.

092905 01  DISC-LINE-3.
           12  FILLER                  PIC XX         VALUE SPACES.
           12  FILLER                  PIC X(130)     VALUE
             'contact our Licensing Department at 1-800-826-6587.'.




00404  EJECT                                                            
00405  01  SUM-DT.                                                      
110105     12  SD-CODE             PIC  X(05).                          
00407      12  SD-DASH             PIC  X(01).                          
00408      12  SD-NAME             PIC  X(20).                          
00409      12  SD-LNCT             PIC ZZZZZ-.                          
00410      12  SD-LNAMT            PIC ZZZ,ZZZ,ZZZ-.                    
00411      12  SD-LICT             PIC ZZZZZ-.                          
00412      12  SD-LCCT             PIC ZZZ.Z-.                          
00413      12  SD-LBEN             PIC ZZ,ZZZ,ZZZ.ZZ-.                  
00414      12  SD-BPCT             PIC ZZZ.Z-.                          
022306     12  SD-LPRM             PIC ZZZZ,ZZZ.ZZ-.                   
00416      12  SD-DICT             PIC ZZZZZ-.                          
00417      12  SD-DCPC             PIC ZZZ.Z-.                          
00418      12  SD-DBEN             PIC ZZ,ZZZ,ZZZ.ZZ-.                  
00419      12  SD-DBPC             PIC ZZZ.Z-.                          
022306     12  SD-DPRM             PIC ZZZZ,ZZZ.ZZ-.                   
00421                                                                   
00422  01  FILLER                  PIC  X(16)          VALUE            
00423          'TABLE BEGIN HERE'.                                      
00424  01  TBL.                                                         
00425      12  TBL-CTL.                                                 
00426          16  TBL-CO          PIC  X(01).                          
00427          16  TBL-CARR        PIC  X(01).                          
00428          16  TBL-GRP         PIC  X(06).                          
00429          16  TBL-ST          PIC  X(02).                          
00430          16  TBL-ACCT        PIC  X(10).                          
00431      12  TBL-NAME            PIC  X(20).                          
00432      12  TBL-MAIL1           PIC  X(20).                          
00433      12  TBL-MAIL2           PIC  X(20).                          
00434      12  TBL-MAIL3           PIC  X(20).                          
00435      12  TBL-ZIP.                                                 
00436          16  TBL-ZIP-PRIME   PIC  9(05).                          
00437          16  TBL-ZIP-PLUS4   PIC  9(04).                          
00438      12  TBL-CANADIAN-POSTAL-CODE REDEFINES TBL-ZIP.              
00439          16  TBL-CAN-POSTAL-CODE-1                                
00440                              PIC  X(03).                          
00441          16  TBL-CAN-POSTAL-CODE-2                                
00442                              PIC  X(03).                          
00443          16  TBL-CAN-FILLER PIC   X(03).                          
00444      12  TBL-ENT         OCCURS  201  TIMES.                      
110105         16  TBL-CODE        PIC  X(05).                          
PEMMOD         16  TBL-LO-NAME     PIC  X(30).
00446          16  TBL-LICT        PIC S9(05)      COMP-3.              
00447          16  TBL-LBEN        PIC S9(09)V99   COMP-3.              
00448          16  TBL-LPRM        PIC S9(07)V99   COMP-3.              
00449          16  TBL-LCCT        PIC S9(05)      COMP-3.              
00450          16  TBL-LREF        PIC S9(07)V99   COMP-3.              
00451          16  TBL-LCOM        PIC S9(07)V99   COMP-3.              
00452          16  TBL-LOCT        PIC S9(5)       COMP-3.              
00453          16  TBL-LVOL        PIC S9(9)V99    COMP-3.              
00454          16  TBL-DICT        PIC S9(05)      COMP-3.              
00455          16  TBL-DBEN        PIC S9(09)V99   COMP-3.              
00456          16  TBL-DPRM        PIC S9(07)V99   COMP-3.              
00457          16  TBL-DCCT        PIC S9(05)      COMP-3.              
00458          16  TBL-DREF        PIC S9(07)V99   COMP-3.              
00459          16  TBL-DCOM        PIC S9(07)V99   COMP-3.              
PEMMOD     12  FIN-ENT.
PEMMOD         16  FIN-LICT        PIC S9(07)     VALUE +0 COMP-3.
PEMMOD         16  FIN-LBEN        PIC S9(11)V99  VALUE +0 COMP-3.
PEMMOD         16  FIN-LPRM        PIC S9(09)V99  VALUE +0 COMP-3.
PEMMOD         16  FIN-LCCT        PIC S9(07)     VALUE +0 COMP-3.
PEMMOD         16  FIN-LREF        PIC S9(09)V99  VALUE +0 COMP-3.
PEMMOD         16  FIN-LCOM        PIC S9(09)V99  VALUE +0 COMP-3.
PEMMOD         16  FIN-LOCT        PIC S9(7)      VALUE +0 COMP-3.
PEMMOD         16  FIN-LVOL        PIC S9(11)V99  VALUE +0 COMP-3.
PEMMOD         16  FIN-DICT        PIC S9(07)     VALUE +0 COMP-3.
PEMMOD         16  FIN-DBEN        PIC S9(11)V99  VALUE +0 COMP-3.
PEMMOD         16  FIN-DPRM        PIC S9(09)V99  VALUE +0 COMP-3.
PEMMOD         16  FIN-DCCT        PIC S9(07)     VALUE +0 COMP-3.
PEMMOD         16  FIN-DREF        PIC S9(09)V99  VALUE +0 COMP-3.
PEMMOD         16  FIN-DCOM        PIC S9(09)V99  VALUE +0 COMP-3.
00460  EJECT                                                            
00461      COPY ERCEXTR.                                                
00462  EJECT                                                            
00463      COPY ERCPNDB.                                                
00464  EJECT                                                            
00465      COPY ELCDATE.                                                
00466  EJECT                                                            
00467      COPY ELCDTECX.                                               
00468  EJECT                                                            
00469      COPY ELCDTEVR.                                               
00470  EJECT                                                            
00471  PROCEDURE DIVISION.                                              
00472                                                                   
00473  0000-GET-DATE.                                                   
PEMMOD
PEMMOD     ACCEPT      WS-BANK
00474                              COPY ELCDTERX.                       
00475  EJECT                                                            
00476  0100-PUT-CO.                                                     
00477      MOVE LIFE-OVERRIDE-L6       TO  H11-LF                       
00478                                      SUM-HD1-LF.                  
00479      MOVE AH-OVERRIDE-L6         TO  H11-AH                       
00480                                      SUM-HD1-AH.                  
00481      MOVE LOW-VALUES             TO  CONA-LOVAL.                  
00482      MOVE COMPANY-NAME           TO  CONA-NAME.                   
00483      MOVE ALPH-DATE              TO  CONA-ANDT.                   
00484                                                                   
00485      IF DTE-PGM-OPT  IS NOT EQUAL TO  2                           
00486          OPEN INPUT ERPNDB                                        
00487      ELSE                                                         
00488          OPEN INPUT EXTRACT-INTERFACE-FILE.                       
00489                                                                   
00490      OPEN INPUT ERACCT                                            
00491           I-O   ERLOFC.                                           
00492                                                                   
00493      IF DTE-PGM-OPT  IS NOT EQUAL TO  2                           
00494          IF PB-STATUS  IS EQUAL TO  '00'  OR  '97'                
00495              NEXT SENTENCE                                        
00496          ELSE                                                     
00497              MOVE PB-STATUS      TO  WS-ABEND-FILE-STATUS         
00498              MOVE 'ERROR ON OPEN - ERPNDB'                        
00499                                  TO  WS-ABEND-MESSAGE             
00500              GO TO ABEND-PGM.                                     
00501                                                                   
00502      IF AM-FILE-STATUS  IS EQUAL TO  '00'  OR  '97'               
00503          NEXT SENTENCE                                            
00504      ELSE                                                         
00505          MOVE AM-FILE-STATUS     TO  WS-ABEND-FILE-STATUS         
00506          MOVE 'ERROR ON OPEN - ERACCT'                            
00507                                  TO  WS-ABEND-MESSAGE             
00508          GO TO ABEND-PGM.                                         
00509                                                                   
00510      IF LO-STATUS  IS EQUAL TO  '00'  OR  '97'                    
00511          NEXT SENTENCE                                            
00512      ELSE                                                         
00513          MOVE LO-STATUS          TO  WS-ABEND-FILE-STATUS         
00514          MOVE 'ERROR ON OPEN - ERLOFC'                            
00515                                  TO  WS-ABEND-MESSAGE             
00516          GO TO ABEND-PGM.                                         
00517                                                                   
00518      IF DTE-PRT-OPT  IS EQUAL TO  'P'  OR  'B'  OR  'T'           
00519          OPEN OUTPUT PRINT-FILE.                                  
00520                                                                   
00521  0200-SORT-RECS SECTION.                                          
00522      SORT SORT-WORK  ON ASCENDING KEY  SORT-CTL                   
00523          INPUT PROCEDURE 0300-INPUT-SECT  THRU  1099-EXIT         
00524          OUTPUT PROCEDURE 1100-PUT-REPORTS  THRU  1199-EXIT.      
00525                                                                   
00526      IF SORT-RETURN  IS NOT EQUAL TO  ZEROS                       
00527          MOVE '0101'             TO  WS-RETURN-CODE               
00528          MOVE 'BAD SORT RETURN CODE '                             
00529                                  TO  WS-ABEND-MESSAGE             
00530          GO TO ABEND-PGM.                                         
00531                                                                   
PEMMOD     DISPLAY '  FIX COUNTS  ' WS-FIX-CNT
PEMMOD
00532      GO TO 9000-EOJ.                                              
00533  EJECT                                                            
00534  0300-INPUT-SECT SECTION.                                         
00535      MOVE CONAMREC               TO  SORT-RECORD.                 
00536                                                                   
00537  0310-RELEASE.                                                    
PEMMOD     MOVE SPACES                 TO  SC-ST
PEMMOD
00538      RELEASE SORT-RECORD.                                         
00539                                                                   
00540  0319-EXIT.                                                       
00541      EXIT.                                                        
00542                                                                   
00543  0400-READ-INPUT.                                                 
00544      IF DTE-PGM-OPT  IS EQUAL TO  2                               
00545          GO TO 0410-ISSUES-CANCELS.                               
00546                                                                   
00547      MOVE LOW-VALUE              TO  PB-CONTROL-BY-ACCOUNT.       
00548      MOVE DTE-CLASIC-COMPANY-CD  TO  PB-COMP-CD.                  
00549                                                                   
00550      MOVE PB-CONTROL-BY-ACCOUNT  TO  PB-CONTROL-BY-ACCT.          
00551                                                                   
00552      START ERPNDB  KEY  GREATER  THAN  PB-CONTROL-BY-ACCT.        
00553                                                                   
00554      IF PB-STATUS  IS EQUAL TO  '23'                              
00555          GO TO 0800-END-INPUT.                                    
00556                                                                   
00557      IF PB-STAT-1  IS NOT EQUAL TO  '0'                           
00558          MOVE PB-STATUS          TO  WS-ABEND-FILE-STATUS         
00559          MOVE 'ERROR OCCURRED START - ERPNDB'                     
00560                                  TO  WS-ABEND-MESSAGE             
00561          GO TO ABEND-PGM.                                         
00562                                                                   
00563      GO TO 0420-ISSUES-CANCELS.                                   
00564                                                                   
00565  0410-ISSUES-CANCELS.                                             
00566      READ EXTRACT-INTERFACE-FILE  INTO  EXTRACT-INTERFACE-RECORD  
00567          AT END                                                   
00568              GO TO 0800-END-INPUT.                                
00569                                                                   
00570      IF EX-EXTRACT-CODE  IS GREATER THAN  'A'                     
00571          GO TO 0800-END-INPUT.                                    
00572                                                                   
00573      IF EX-RECORD-TYPE  IS GREATER THAN  'E'                      
00574          GO TO 0800-END-INPUT.                                    
00575                                                                   
00576      IF EX-COMPANY-CD  IS LESS THAN  DTE-CLASIC-COMPANY-CD        
00577          GO TO 0410-ISSUES-CANCELS.                               
00578                                                                   
00579      IF EX-COMPANY-CD  IS GREATER THAN  DTE-CLASIC-COMPANY-CD     
00580          GO TO 0800-END-INPUT.                                    
00581                                                                   
00582      IF EX-RECORD-TYPE  IS NOT EQUAL TO  'A'                      
00583          GO TO 0410-ISSUES-CANCELS.                               
00584                                                                   
00585      MOVE EX-DATA-AREAS          TO  PENDING-BUSINESS.            
00586                                                                   
00587      GO TO 0430-PICKUP.                                           
00588                                                                   
00589  0420-ISSUES-CANCELS.                                             
00590      IF DTE-PGM-OPT  IS EQUAL TO  2                               
00591          GO TO 0410-ISSUES-CANCELS.                               
00592                                                                   
00593      READ ERPNDB  NEXT RECORD  INTO  PENDING-BUSINESS.            
00594                                                                   
00595      IF PB-STAT-1  IS EQUAL TO  '1'                               
00596          GO TO 0800-END-INPUT.                                    
00597                                                                   
00598      IF PB-STAT-1  IS NOT EQUAL TO  '0'                           
00599          MOVE PB-STATUS          TO  WS-ABEND-FILE-STATUS         
00600          MOVE 'ERROR OCCURRED READ - ERPNDB'                      
00601                                  TO  WS-ABEND-MESSAGE             
00602          GO TO ABEND-PGM.                                         
00603                                                                   
00604  0430-PICKUP.                                                     
00605      IF PB-COMPANY-CD  IS NOT EQUAL TO  DTE-CLASIC-COMPANY-CD     
00606          GO TO 0800-END-INPUT.                                    
00607                                                                   
00608      IF PB-ISSUE                                                  
00609        AND CLASIC-CREATED-CERT                                    
00610          GO TO 0420-ISSUES-CANCELS.                               
00611                                                                   
00612      IF PB-ISSUE                                                  
00613        OR PB-CANCELLATION                                         
00614          NEXT SENTENCE                                            
00615      ELSE                                                         
00616          GO TO 0420-ISSUES-CANCELS.                               
00617                                                                   
00618      IF PB-REIN-ONLY-CERT                                         
00619        OR PB-REISSUED-CERT                                        
122002       OR PB-MONTHLY-CERT
00620        OR PB-PREM-ACCTNG-ONLY                                     
00621        OR PB-POLICY-IS-DECLINED                                   
00622        OR PB-POLICY-IS-VOIDED                                     
00623          GO TO 0420-ISSUES-CANCELS.                               
00624                                                                   
00625      IF PB-UNFORCED-ERRORS                                        
00626        OR PB-FATAL-ERRORS                                         
00627        OR PB-RECORD-ON-HOLD                                       
00628        OR PB-RECORD-RETURNED                                      
00629          GO TO 0420-ISSUES-CANCELS.                               
00630                                                                   
PEMFIX     IF DTE-CLIENT = 'CID'
PEMFIX        IF PB-ISSUE
PEMFIX           IF PB-ACCOUNT = '0000456000'
PEMFIX              IF PB-CERT-NO = '0000030564 '
PEMFIX                 ADD 1           TO WS-FIX-CNT
PEMFIX                 GO TO 0420-ISSUES-CANCELS
PEMFIX              END-IF
PEMFIX           END-IF
PEMFIX*          IF PB-ACCOUNT = '0000852100'
PEMFIX*             IF PB-CERT-NO = '0000347297 '
PEMFIX*                ADD 1           TO WS-FIX-CNT
PEMFIX*                GO TO 0420-ISSUES-CANCELS
PEMFIX*             END-IF
PEMFIX*          END-IF
PEMFIX        END-IF
PEMFIX     END-IF

00631 *    IF DTE-CLIENT  IS EQUAL TO  'MON' OR 'CID'                   
00632 *        IF PB-ISSUE                                              
00633 *          AND (PB-I-LOAN-OFFICER  IS EQUAL TO  SPACES            
00634 *          OR PB-I-LOAN-OFFICER  IS EQUAL TO  LOW-VALUES)         
00635 *            GO TO 0420-ISSUES-CANCELS.                           
00636 *                                                                 
00637 *    IF DTE-CLIENT  IS EQUAL TO  'MON' OR 'CID'                   
00638 *        IF PB-CANCELLATION                                       
00639 *          AND (PB-CI-LOAN-OFFICER  IS EQUAL TO  SPACES           
00640 *          OR PB-CI-LOAN-OFFICER  IS EQUAL TO  LOW-VALUES)        
00641 *            GO TO 0420-ISSUES-CANCELS.                           
00642                                                                   
00643      IF DTE-CLIENT = 'UCL'                                        
00644          IF (PB-SV-CARRIER = '6') OR                              
00645             (PB-SV-CARRIER = '2' AND PB-ACCOUNT = '0000000651')   
00646              NEXT SENTENCE                                        
00647            ELSE                                                   
00648              GO TO 0420-ISSUES-CANCELS.                           
00649                                                                   
PEMFIX*    IF DTE-CLIENT = 'CID'
PEMFIX*       IF PB-ISSUE
PEMFIX*          IF PB-ACCOUNT = '0001110300'
PEMFIX*             IF PB-CERT-NO = '0000090183 '
PEMFIX*                MOVE '10GL' TO PB-I-LOAN-OFFICER
PEMFIX*                ADD 1 TO WS-FIX-CNT
PEMFIX*             END-IF
PEMFIX*             IF PB-CERT-NO = '0000090205 '
PEMFIX*                MOVE '12SL' TO PB-I-LOAN-OFFICER
PEMFIX*                ADD 1 TO WS-FIX-CNT
PEMFIX*             END-IF
PEMFIX*             IF PB-CERT-NO = '0000090286 ' OR '0000090317 '
PEMFIX*                MOVE '10SB' TO PB-I-LOAN-OFFICER
PEMFIX*                ADD 1 TO WS-FIX-CNT
PEMFIX*             END-IF
PEMFIX*             IF PB-CERT-NO = '0000090292 ' OR '0000090506 '
PEMFIX*                MOVE '01SF' TO PB-I-LOAN-OFFICER
PEMFIX*                ADD 1 TO WS-FIX-CNT
PEMFIX*             END-IF
PEMFIX*             IF PB-CERT-NO = '0000090460 ' OR '0000090511 '
PEMFIX*                MOVE '01JA' TO PB-I-LOAN-OFFICER
PEMFIX*                ADD 1 TO WS-FIX-CNT
PEMFIX*             END-IF
PEMFIX*          END-IF
PEMFIX*       END-IF
PEMFIX*    END-IF
PEMFIX 
PEMFIX*    IF DTE-CLIENT = 'CID'
PEMFIX*       IF PB-CANCELLATION
PEMFIX*          IF PB-ACCOUNT = '0000946300'
PEMFIX*             IF PB-CERT-NO = '0000006253 '
PEMFIX*                MOVE '001' TO PB-CI-LOAN-OFFICER
PEMFIX*                ADD 1 TO WS-FIX-CNT
PEMFIX*             END-IF
PEMFIX*          END-IF
PEMFIX*       END-IF
PEMFIX*    END-IF
PEMFIX 
00650      MOVE PB-CONTROL-BY-ACCOUNT  TO  PEND-REC.                    
00651                                                                   
00652      IF DTE-COMP-VG  IS EQUAL TO  ' '                             
00653          MOVE PB-SV-CARRIER      TO  PEND-CARR                    
00654          MOVE PB-SV-GROUPING     TO  PEND-GRP.                    
00655                                                                   
00656      IF DTE-COMP-VG  IS EQUAL TO  '2'                             
00657          MOVE PB-SV-GROUPING     TO  PEND-GRP.                    
00658                                                                   
00659      IF DTE-COMP-VG  IS EQUAL TO  '3'                             
00660          MOVE PB-SV-CARRIER      TO  PEND-CARR                    
00661          MOVE PB-SV-GROUPING     TO  PEND-GRP                     
00662          MOVE PB-SV-STATE        TO  PEND-ST.                     
00663                                                                   
00664      IF DTE-COMP-VG  IS EQUAL TO  '4'                             
00665          MOVE PB-SV-GROUPING     TO  PEND-GRP                     
00666          MOVE PB-SV-STATE        TO  PEND-ST.                     
00667                                                                   
00668      MOVE PEND-REC               TO  KEY-20.                      
00669                                                                   
00670      IF (SKIP-THIS-ACCT  = 'Y') AND                               
00671         (KEY-20  = LST-ACCT)                                      
00672          GO TO 0420-ISSUES-CANCELS.                               
00673                                                                   
00674      IF KEY-20  IS NOT EQUAL TO  LST-ACCT                         
00675          PERFORM 0700-GET-ACCT  THRU  0799-EXIT.                  
00676                                                                   
00677      IF SKIP-THIS-ACCT  IS EQUAL TO  'Y'                          
00678          GO TO 0420-ISSUES-CANCELS.                               
00679                                                                   
00680      MOVE PB-CERT-EFF-DT         TO  DC-BIN-DATE-1.               
00681      MOVE SPACE                  TO  DC-OPTION-CODE.              
00682                                                                   
00683      PERFORM 0500-DATE-RTN  THRU  0599-EXIT.                      
00684                                                                   
00685      MOVE DC-GREG-DATE-CYMD      TO  PEND-YRMODA.                 
00686      MOVE PB-CERT-PRIME          TO  PEND-CERT-PRIME.             
00687      MOVE PB-CERT-SFX            TO  PEND-CERT-SFX.               
00688                                                                   
00689      IF NOT PB-ISSUE                                              
00690          GO TO 0440-CHK-CANCEL.                                   
00691                                                                   
110105     IF PB-I-LOAN-OFFICER = SPACES
110105         MOVE '99999'            TO PB-I-LOAN-OFFICER
110105     END-IF
00694                                                                   
051906     IF PB-I-LOAN-OFFICER (5:1) = LOW-VALUES
051906        MOVE SPACES TO PB-I-LOAN-OFFICER (5:1)
051906     END-IF
051906     IF PB-I-LOAN-OFFICER (4:1) = LOW-VALUES
051906        MOVE SPACES TO PB-I-LOAN-OFFICER (4:1)
051906     END-IF
110105     MOVE PB-I-LOAN-OFFICER      TO PEND-OFFICER.                
00696      MOVE PEND-REC               TO  KEY-23.                      
00697                                                                   
00698      IF KEY-23  IS NOT EQUAL TO  LST-OFFCR                        
00699          PERFORM 0600-GET-LOAN-OFFICER  THRU  0699-EXIT.          
00700                                                                   
00701      IF SKIP-THIS-LOAN-OFCR EQUAL 'Y'                             
00702          GO TO 0420-ISSUES-CANCELS.                               
00703                                                                   
00704      MOVE PB-I-INSURED-LAST-NAME TO  PEND-NAME.                   
00705      MOVE PB-I-INSURED-1ST-INIT  TO  PEND-INIT.                   
00706      MOVE PB-I-AGE               TO  PEND-AGE.                    
00707      MOVE PB-I-LF-ABBR           TO  PEND-LF-TYP.                 
00708      MOVE PB-I-LF-PREMIUM-AMT    TO  PEND-LF-PRM.                 
00709                                                                   
00710      IF PB-I-LF-ALT-PREMIUM-AMT  IS GREATER THAN  ZEROS           
00711          COMPUTE PEND-LF-PRM = PEND-LF-PRM                        
00712                              + PB-I-LF-ALT-PREMIUM-AMT.           
00713                                                                   
00714      MOVE PB-I-LF-BENEFIT-AMT    TO  PEND-LF-AMT.                 
00715                                                                   
00716      IF PB-I-LF-ALT-BENEFIT-AMT  IS GREATER THAN  ZEROS           
00717          COMPUTE PEND-LF-AMT = PEND-LF-AMT                        
00718                              + PB-I-LF-ALT-BENEFIT-AMT.           
00719                                                                   
00720      MOVE PB-I-AH-ABBR           TO  PEND-AH-TYP.                 
00721      MOVE PB-I-AH-PREMIUM-AMT    TO  PEND-AH-PRM.                 
00722                                                                   
00723      IF DTE-CLIENT = 'UCL'                                        
00724          NEXT SENTENCE                                            
00725       ELSE                                                        
00726          GO TO 0435-CONTINUE.                                     
00727                                                                   
00728      IF PB-OVERRIDE-LIFE OR PB-OVERRIDE-BOTH                      
00729          COMPUTE PEND-LF-PRM = PB-I-LF-PREM-CALC                  
00730                              + PB-I-LF-ALT-PREM-CALC.             
00731                                                                   
00732      IF PB-OVERRIDE-AH OR PB-OVERRIDE-BOTH                        
00733          MOVE PB-I-AH-PREM-CALC  TO PEND-AH-PRM.                  
00734                                                                   
00735  0435-CONTINUE.                                                   
PEMMOD*    MULTIPLY PB-I-AH-BENEFIT-AMT  BY  PB-I-AH-TERM               
PEMMOD*        GIVING  PEND-AH-AMT.                                     
PEMMOD     MOVE PB-I-AH-BENEFIT-AMT    TO  PEND-AH-AMT
00738                                                                   
00739      MOVE PB-I-LF-TERM           TO  PEND-LF-TERM.                
00740      MOVE PB-I-AH-TERM           TO  PEND-AH-TERM.                
00741                                                                   
00742      COMPUTE PEND-LF-COM ROUNDED =                                
00743          PEND-LF-PRM * PB-I-LIFE-COMMISSION.                      
00744      COMPUTE PEND-AH-COM ROUNDED =                                
00745          PEND-AH-PRM * PB-I-AH-COMMISSION.                        
00746                                                                   
00747      MOVE PEND-REC               TO  SORT-RECORD.                 
00748                                                                   
00749      PERFORM 0310-RELEASE  THRU  0319-EXIT.                       
00750                                                                   
00751      GO TO 0420-ISSUES-CANCELS.                                   
00752  EJECT                                                            
00753  0440-CHK-CANCEL.                                                 
00754      IF NOT PB-CANCELLATION                                       
00755          GO TO 0420-ISSUES-CANCELS.                               
00756                                                                   
110105     IF PB-CI-LOAN-OFFICER = SPACES
110105         MOVE '99999'            TO PB-CI-LOAN-OFFICER
110105     END-IF
00759                                                                   
051906     IF PB-CI-LOAN-OFFICER (5:1) = LOW-VALUES
051906        MOVE SPACES TO PB-CI-LOAN-OFFICER (5:1)
051906     END-IF
051906     IF PB-CI-LOAN-OFFICER (4:1) = LOW-VALUES
051906        MOVE SPACES TO PB-CI-LOAN-OFFICER (4:1)
051906     END-IF
110105     MOVE PB-CI-LOAN-OFFICER     TO PEND-OFFICER.
00761      MOVE PEND-REC               TO  KEY-23.                      
00762                                                                   
00763      IF KEY-23  IS NOT EQUAL TO  LST-OFFCR                        
00764          PERFORM 0600-GET-LOAN-OFFICER  THRU  0699-EXIT.          
00765                                                                   
00766      IF SKIP-THIS-LOAN-OFCR EQUAL 'Y'                             
00767          GO TO 0420-ISSUES-CANCELS.                               
00768                                                                   
00769      MOVE PB-CI-LAST-NAME        TO  PEND-NAME.                   
00770      MOVE PB-CI-INITIALS         TO  PEND-INIT.                   
00771      MOVE PB-CI-INSURED-AGE      TO  PEND-AGE.                    
00772      MOVE PB-CI-LF-ABBR          TO  PEND-LF-TYP.                 
00773      MOVE PB-CI-AH-ABBR          TO  PEND-AH-TYP.                 
00774      MOVE PB-CI-LF-TERM          TO  PEND-LF-TERM.                
00775      MOVE PB-CI-AH-TERM          TO  PEND-AH-TERM.                
00776                                                                   
00777      MULTIPLY PB-C-LF-CANCEL-AMT  BY  -1  GIVING  PEND-LF-PRM.    
00778      MULTIPLY PB-C-AH-CANCEL-AMT  BY  -1  GIVING  PEND-AH-PRM.    
00779                                                                   
00780      IF DTE-CLIENT = 'UCL'                                        
00781          NEXT SENTENCE                                            
00782       ELSE                                                        
00783          GO TO 0535-CONTINUE.                                     
00784                                                                   
00785      IF PB-OVERRIDE-LIFE OR PB-OVERRIDE-BOTH                      
00786          MULTIPLY PB-C-LF-REF-CALC BY -1 GIVING PEND-LF-PRM.      
00787                                                                   
00788      IF PB-OVERRIDE-AH OR PB-OVERRIDE-BOTH                        
00789          MULTIPLY PB-C-AH-REF-CALC BY -1 GIVING PEND-AH-PRM.      
00790                                                                   
00791  0535-CONTINUE.                                                   
00792      MOVE ZERO                   TO  PEND-LF-AMT  PEND-AH-AMT.    
00793                                                                   
00794      COMPUTE PEND-LF-COM ROUNDED = PB-CI-LIFE-COMMISSION          
00795                                  * PEND-LF-PRM.                   
00796      COMPUTE PEND-AH-COM ROUNDED = PB-CI-AH-COMMISSION            
00797                                  * PEND-AH-PRM.                   
00798                                                                   
00799      MOVE PEND-REC               TO  SORT-RECORD.                 
00800                                                                   
00801      PERFORM 0310-RELEASE  THRU  0319-EXIT.                       
00802                                                                   
00803      GO TO 0420-ISSUES-CANCELS.                                   
00804  EJECT                                                            
00805  0500-DATE-RTN.                                                   
00806      CALL 'ELDATCX'  USING  DATE-CONVERSION-DATA.                 
00807                                                                   
00808      IF DC-ERROR-CODE  IS NOT EQUAL TO  SPACE                     
00809          MOVE ZERO               TO  DC-CONVERSION-DATES.         
00810                                                                   
00811  0599-EXIT.                                                       
00812      EXIT.                                                        
00813                                                                   
00814  0600-GET-LOAN-OFFICER.                                           
00815      MOVE PEND-REC               TO  LST-OFFCR.                   
00816      MOVE SPACES                 TO  LO-CONTROL-PRIMARY.          
00817      MOVE PB-COMPANY-CD-A1       TO  LO-COMPANY-CD.               
00818      MOVE PB-CARRIER             TO  LO-CARRIER.                  
00819      MOVE PB-GROUPING            TO  LO-GROUPING.                 
00820      MOVE PB-STATE               TO  LO-STATE.                    
00821      MOVE PB-ACCOUNT             TO  LO-ACCOUNT.                  
00822      MOVE PEND-OFFICER           TO  LO-OFFICER-CODE.             
00823                                                                   
00824      READ ERLOFC.                                                 
00825                                                                   
00826      IF LO-STAT-1  IS EQUAL TO  '1'                               
00827          GO TO 0620-INV-LO.                                       
00828                                                                   
00829      IF LO-STATUS  IS EQUAL TO  '23'                              
00830          GO TO 0620-INV-LO.                                       
00831                                                                   
00832      IF LO-COMPANY-CD  IS NOT EQUAL TO  PB-COMPANY-CD-A1          
00833          GO TO 0620-INV-LO.                                       
00834                                                                   
00835      IF LO-STATUS  IS NOT EQUAL TO  ZERO                          
00836          MOVE LO-STATUS          TO  WS-ABEND-FILE-STATUS         
00837          MOVE 'ERROR OCCURRED READ - EROFCR'                      
00838                                  TO  WS-ABEND-MESSAGE             
00839          GO TO ABEND-PGM.                                         
00840                                                                   
00841      IF LO-CONTROL-PRIMARY  IS GREATER THAN  KEY-23               
00842          GO TO 0620-INV-LO.                                       
00843                                                                   
00844      MOVE LO-OFFICER-NAME        TO  LN-OFFCR.                    
00845      MOVE LO-OPT1                TO  LN-OPT1.                     
00846      MOVE LO-OPT2                TO  LN-OPT2.                     
00847      MOVE LO-CONTROL-PRIMARY     TO  LST-OFFCR.                   
00848                                                                   
00849  0610-PUT-LO.                                                     
00850      MOVE LO-COMPANY-CD          TO  LN-CO.                       
00851      MOVE PEND-CARR              TO  LN-CARR.                     
00852      MOVE PEND-GRP               TO  LN-GRP.                      
00853      MOVE PEND-ST                TO  LN-ST.                       
00854      MOVE LO-ACCOUNT             TO  LN-ACCT.                     
00855      MOVE LO-OFFICER-CODE        TO  LN-OFFCODE.                  
00856      MOVE LOW-VALUES             TO  LN-LOVAL.                    
00857      MOVE LO-OFFICER-NAME        TO  LN-OFFCR.                    
00858      MOVE LO-COMP-CONTROL        TO  LN-OPT1.                     
00859      MOVE LO-DETAIL-CONTROL      TO  LN-OPT2.                     
00860      MOVE LN-REC                 TO  SORT-RECORD.                 
00861                                                                   
00862      PERFORM 0310-RELEASE  THRU  0319-EXIT.                       
00863                                                                   
00864      MOVE 'N'                    TO  SKIP-THIS-LOAN-OFCR.         
00865                                                                   
00866      GO TO 0699-EXIT.                                             
00867                                                                   
00868  0620-INV-LO.                                                     
00869      MOVE HIGH-VALUES            TO  PEND-OFFICER  LST-OFFCR.     
00870                                                                   
00871      IF DTE-FMT-OPT EQUAL '2'                                     
00872          MOVE 'Y'                TO  SKIP-THIS-LOAN-OFCR          
00873          GO TO 0699-EXIT.                                         
00874                                                                   
00875      IF HAVE-HIGH-VALUE  IS EQUAL TO  'Y'                         
00876          GO TO 0699-EXIT.                                         
00877                                                                   
00878      MOVE PEND-CO                TO  LN-CO.                       
00879      MOVE PEND-CARR              TO  LN-CARR.                     
00880      MOVE PEND-GRP               TO  LN-GRP.                      
00881      MOVE PEND-ST                TO  LN-ST.                       
00882      MOVE PB-ACCOUNT             TO  LN-ACCT.                     
00883      MOVE HIGH-VALUES            TO  LN-OFFCODE.                  
00884      MOVE LOW-VALUE              TO  LN-LOVAL.                    
00885      MOVE 'UNASSIGNED'           TO  LN-OFFCR.                    
00886      MOVE 'Y'                    TO  LN-OPT1.                     
00887      MOVE 'D'                    TO  LN-OPT2.                     
00888      MOVE 'Y'                    TO  HAVE-HIGH-VALUE.             
00889      MOVE LN-REC                 TO  SORT-RECORD.                 
00890      MOVE LN-REC                 TO  LST-OFFCR.                   
00891                                                                   
00892      PERFORM 0310-RELEASE  THRU  0319-EXIT.                       
00893                                                                   
00894  0699-EXIT.                                                       
00895      EXIT.                                                        
00896  EJECT                                                            
00897  0700-GET-ACCT.                                                   
00898      MOVE SPACE                  TO  AM-CONTROL-PRIMARY.          
00899      MOVE 'N'                    TO  HAVE-HIGH-VALUE.             
00900      MOVE LOW-VALUES             TO  LST-OFFCR  HLD-CTL.          
00901      MOVE PEND-CO                TO  AM-COMPANY-CD.               
00902      MOVE PEND-CARR              TO  AM-CARRIER.                  
00903      MOVE PEND-GRP               TO  AM-GROUPING.                 
00904      MOVE PEND-ST                TO  AM-STATE.                    
00905      MOVE PB-ACCOUNT             TO  AM-ACCOUNT.                  
PEMMOD     MOVE PB-CERT-EFF-DT         TO  AM-EXPIRATION-DT
00906                                                                   
00907      START ERACCT  KEY  NOT LESS THAN  AM-CONTROL-PRIMARY.        
00908                                                                   
00909      IF AM-FILE-STATUS  IS EQUAL TO  '23'                         
00910          GO TO 0720-INVALID-ACCOUNT.                              
00911                                                                   
00912      IF AM-STAT-1  IS NOT EQUAL TO  '0'                           
00913          MOVE AM-FILE-STATUS     TO  WS-ABEND-FILE-STATUS         
00914          MOVE 'ERROR OCCURRED START - ERACCT'                     
00915                                  TO  WS-ABEND-MESSAGE             
00916          GO TO ABEND-PGM.                                         
00917                                                                   
00918  0710-NEXT-ACCT.                                                  
00919      READ ERACCT  NEXT RECORD.                                    
00920                                                                   
00921      IF AM-STAT-1  IS EQUAL TO  '1'                               
00922          GO TO 0720-INVALID-ACCOUNT.                              
00923                                                                   
00924      IF AM-COMPANY-CD  IS NOT EQUAL TO  PB-COMPANY-CD-A1          
00925          GO TO 0720-INVALID-ACCOUNT.                              
00926                                                                   
00927      IF AM-STAT-1  IS NOT EQUAL TO  '0'                           
00928          MOVE AM-FILE-STATUS     TO  WS-ABEND-FILE-STATUS         
00929          MOVE 'ERROR OCCURRED READ - ERACCT'                      
00930                                  TO  WS-ABEND-MESSAGE             
00931          GO TO ABEND-PGM.                                         
00932                                                                   
00933      IF PB-ACCOUNT  IS EQUAL TO  AM-ACCOUNT                       
00934          GO TO 0740-VALID-ACCOUNT.                                
00935                                                                   
00936  0720-INVALID-ACCOUNT.                                            
00937      MOVE 'Y'                    TO  SKIP-THIS-ACCT.              
00938      MOVE KEY-20                 TO  LST-ACCT.                    
00939                                                                   
00940      GO TO 0799-EXIT.                                             
00941                                                                   
00942  0730-PUT-ACCOUNT.                                                
00943      MOVE AM-COMPANY-CD          TO  ACCT-CO.                     
00944      MOVE AM-CARRIER             TO  ACCT-CARR.                   
00945      MOVE AM-GROUPING            TO  ACCT-GRP.                    
00946      MOVE AM-STATE               TO  ACCT-ST.                     
00947      MOVE AM-ACCOUNT             TO  ACCT-NUM.                    
00948      MOVE LOW-VALUES             TO  ACCT-LOVAL.                  
00949      MOVE ACCT-REC               TO  SORT-RECORD.                 
00950                                                                   
PEMMOD     IF (AM-EDIT-LOAN-OFC NOT = 'Y')                              
PEMMOD                  OR
PEMMOD        ((WS-BANK NOT = SPACES) AND
PEMMOD         (WS-BANK NOT = AM-USER-SELECT-1))
00952          MOVE 'Y'                TO  SKIP-THIS-ACCT               
PEMMOD         MOVE KEY-20             TO  LST-ACCT                     
00953          GO TO 0799-EXIT                                          
00954      ELSE                                                         
00955          MOVE 'N'                TO  SKIP-THIS-ACCT.              
00956                                                                   
00957      PERFORM 0310-RELEASE  THRU  0319-EXIT.                       
00958                                                                   
00959      MOVE KEY-20                 TO  LST-ACCT.                    
00960      MOVE 'N'                    TO  HAVE-HIGH-VALUE.             
00961                                                                   
00962      GO TO 0799-EXIT.                                             
00963                                                                   
00964  0740-VALID-ACCOUNT.                                              

052104     IF AM-EXPIRATION-DT NOT = HIGH-VALUES
052104        PERFORM UNTIL
052104          (AM-FILE-STATUS   NOT = '00')
052104          OR (AM-COMPANY-CD NOT = PB-COMPANY-CD-A1)
052104          OR (AM-ACCOUNT    NOT = PB-ACCOUNT)
052104          MOVE ACCOUNT-MASTER      TO WS-HOLD-ERACCT
052104          READ ERACCT  NEXT RECORD
052104        END-PERFORM
052104        MOVE WS-HOLD-ERACCT      TO ACCOUNT-MASTER
052104     END-IF
                      
00965      MOVE 'N'                    TO  SKIP-THIS-ACCT.              
00966      MOVE AM-NAME                TO  ACCT-NAME.                   
00967      MOVE AM-PERSON              TO  ACCT-PERSON.                 
00968      MOVE AM-ADDRS               TO  ACCT-ADDR1.                  
00969      MOVE AM-CITY                TO  ACCT-CTY-ST.                 
00970                                                                   
00971      IF  AM-CANADIAN-POST-CODE                                    
00972          MOVE AM-CAN-POSTAL-1    TO ACCT-CAN-POSTAL-CODE-1        
00973          MOVE AM-CAN-POSTAL-2    TO ACCT-CAN-POSTAL-CODE-2        
00974          MOVE SPACES             TO ACCT-CAN-FILLER               
00975                                                                   
00976      ELSE                                                         
00977          MOVE AM-ZIP-PRIME       TO ACCT-ZIP-PRIME                
00978                                                                   
00979          IF  AM-ZIP-PLUS4 = SPACES OR ZEROS                       
00980              MOVE SPACES         TO ACCT-ZIP-PLUS4                
00981                                                                   
00982          ELSE                                                     
00983              MOVE AM-ZIP-PLUS4   TO ACCT-ZIP-PLUS4.               
00984                                                                   
00985      GO TO 0730-PUT-ACCOUNT.                                      
00986                                                                   
00987  0799-EXIT.                                                       
00988      EXIT.                                                        
00989                                                                   
00990  0800-END-INPUT.                                                  
00991                                                                   
00992  0810-GET-LOAN-OFFICERS.                                          
00993      MOVE SPACES                 TO  SAVE-ACCOUNT                 
00994                                      LO-CONTROL-PRIMARY.          
00995      MOVE DTE-CLASIC-COMPANY-CD  TO  LO-COMPANY-CD.               
00996                                                                   
00997      START ERLOFC  KEY  NOT LESS THAN  LO-CONTROL-PRIMARY.        
00998                                                                   
022804     EVALUATE TRUE
022804     WHEN LO-STATUS = '00'
022804         CONTINUE

022804     WHEN LO-STATUS = '23'
022804         GO TO 1099-EXIT

010704     WHEN OTHER
01000          MOVE LO-STATUS          TO  WS-ABEND-FILE-STATUS         
01001          MOVE 'ERROR OCCURRED START - EROFCR'                     
01002                                  TO  WS-ABEND-MESSAGE             
01003          GO TO ABEND-PGM
022804     END-EVALUATE.
01004                                                                   
01005  0820-GET-NEXT-OFFICER.                                           
01006      READ ERLOFC  NEXT RECORD.                                    
01007                                                                   
01008      IF DTE-FMT-OPT EQUAL '2'                                     
01009          GO TO 1099-EXIT.                                         
01010                                                                   
01011      IF LO-STATUS  IS EQUAL TO  '23'  OR  '10'                    
01012          GO TO 1099-EXIT.                                         
01013                                                                   
01014      IF LO-STATUS  IS NOT EQUAL TO  ZERO                          
01015          MOVE LO-STATUS          TO  WS-ABEND-FILE-STATUS         
01016          MOVE 'ERROR OCCURRED READ - EROFCR'                      
01017                                  TO  WS-ABEND-MESSAGE             
01018          GO TO ABEND-PGM.                                         
01019                                                                   
01020      IF LO-COMPANY-CD  IS NOT EQUAL TO  DTE-CLASIC-COMPANY-CD     
01021          GO TO 1099-EXIT.                                         
01022                                                                   
01023      IF LO-LOAN-COUNT (RUN-MO) IS GREATER THAN  ZERO              
01024        OR LO-LOAN-VOLUME (RUN-MO) IS GREATER THAN  ZERO           
01025          NEXT SENTENCE                                            
01026      ELSE                                                         
01027          GO TO 0820-GET-NEXT-OFFICER.                             
01028                                                                   
01029      MOVE LO-COMPANY-CD          TO  LN-CO.                       
01030      MOVE LO-CARRIER             TO  LN-CARR.                     
01031      MOVE LO-GROUPING            TO  LN-GRP.                      
01032      MOVE LO-STATE               TO  LN-ST.                       
01033      MOVE LO-ACCOUNT             TO  LN-ACCT.                     
01034                                                                   
01035      IF DTE-COMP-VG  IS EQUAL TO  ' '                             
01036          MOVE LO-SV-CARRIER      TO  LN-CARR                      
01037          MOVE LO-SV-GROUPING     TO  LN-GRP.                      
01038                                                                   
01039      IF DTE-COMP-VG  IS EQUAL TO  '2'                             
01040          MOVE LO-SV-GROUPING     TO  LN-GRP.                      
01041                                                                   
01042      IF DTE-COMP-VG  IS EQUAL TO  '3'                             
01043          MOVE LO-SV-CARRIER      TO  LN-CARR                      
01044          MOVE LO-SV-GROUPING     TO  LN-GRP                       
01045          MOVE LO-SV-STATE        TO  LN-ST.                       
01046                                                                   
01047      IF DTE-COMP-VG  IS EQUAL TO  '4'                             
01048          MOVE LO-SV-GROUPING     TO  LN-GRP                       
01049          MOVE LO-SV-STATE        TO  LN-ST.                       
01050                                                                   
01051      MOVE LO-OFFICER-CODE        TO  LN-OFFCODE.                  
01052      MOVE LOW-VALUES             TO  LN-LOVAL.                    
01053      MOVE LO-OFFICER-NAME        TO  LN-OFFCR.                    
01054      MOVE LO-COMP-CONTROL        TO  LN-OPT1.                     
01055      MOVE LO-DETAIL-CONTROL      TO  LN-OPT2.                     
01056                                                                   
01057      IF LO-ACCOUNT  IS EQUAL TO  SAVE-ACCOUNT                     
01058          GO TO 0830-RELEASE-LOAN-OFFICER.                         
01059                                                                   
01060      MOVE LO-ACCOUNT             TO  SAVE-ACCOUNT.                
01061                                                                   
01062      PERFORM 0900-GET-ACCOUNT  THRU  0999-EXIT.                   
01063                                                                   
01064  0830-RELEASE-LOAN-OFFICER.                                       
01065      IF SKIP-THIS-ACCT  IS EQUAL TO  'Y'                          
01066          GO TO 0820-GET-NEXT-OFFICER.                             
01067                                                                   
01068      MOVE LN-REC                 TO  SORT-RECORD.                 
01069                                                                   
01070      PERFORM 0310-RELEASE  THRU  0319-EXIT.                       
01071                                                                   
01072      GO TO 0820-GET-NEXT-OFFICER.                                 
01073  EJECT                                                            
01074  0900-GET-ACCOUNT.                                                
01075      MOVE LN-CO                  TO  AM-COMPANY-CD.               
01076      MOVE LN-CARR                TO  AM-CARRIER.                  
01077      MOVE LN-GRP                 TO  AM-GROUPING.                 
01078      MOVE LN-ST                  TO  AM-STATE.                    
01079      MOVE LN-ACCT                TO  AM-ACCOUNT.                  
052104     MOVE BIN-RUN-DATE           TO  AM-EXPIRATION-DT
01080                                                                   
01081                                                                   
01082      START ERACCT  KEY  NOT LESS THAN  AM-CONTROL-PRIMARY.        
01083                                                                   
01084      IF AM-FILE-STATUS  IS EQUAL TO  '23'                         
01085          GO TO 0920-INVALID-ACCOUNT.                              
01086                                                                   
01087      IF AM-STAT-1  IS NOT EQUAL TO  '0'                           
01088          MOVE AM-FILE-STATUS     TO  WS-ABEND-FILE-STATUS         
01089          MOVE 'ERROR OCCURRED START - ERACCT'                     
01090                                  TO  WS-ABEND-MESSAGE             
01091          GO TO ABEND-PGM.                                         
01092                                                                   
01093  0910-NEXT-ACCT.                                                  
01094      READ ERACCT  NEXT RECORD.                                    
01095                                                                   
01096      IF AM-STAT-1  IS EQUAL TO  '1'                               
01097          GO TO 0920-INVALID-ACCOUNT.                              
01098                                                                   
01099      IF AM-STAT-1  IS NOT EQUAL TO  '0'                           
01100          MOVE AM-FILE-STATUS     TO  WS-ABEND-FILE-STATUS         
01101          MOVE 'ERROR OCCURRED READ - ERACCT'                      
01102                                  TO  WS-ABEND-MESSAGE             
01103          GO TO ABEND-PGM.                                         
01104                                                                   
01105      IF AM-COMPANY-CD  IS NOT EQUAL TO  DTE-CLASIC-COMPANY-CD     
01106          GO TO 0920-INVALID-ACCOUNT.                              
01107                                                                   
01108      IF AM-ACCOUNT  IS EQUAL TO  SAVE-ACCOUNT                     
01109          GO TO 0930-VALID-ACCOUNT.                                
01110                                                                   
01111  0920-INVALID-ACCOUNT.                                            
01112      MOVE 'Y'                    TO  SKIP-THIS-ACCT.              
01113                                                                   
01114      GO TO 0999-EXIT.                                             
01115                                                                   
01116  0930-VALID-ACCOUNT.                                              

      *    IF AM-EXPIRATION-DT NOT = HIGH-VALUES
      *       PERFORM UNTIL
      *         (AM-FILE-STATUS   NOT = '00')
      *         OR (AM-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
      *         OR (AM-ACCOUNT    NOT = LN-ACCT)
      *         MOVE ACCOUNT-MASTER      TO WS-HOLD-ERACCT
      *         READ ERACCT  NEXT RECORD
      *       END-PERFORM
      *       MOVE WS-HOLD-ERACCT      TO ACCOUNT-MASTER
      *    END-IF

01117      MOVE AM-COMPANY-CD          TO  ACCT-CO.                     
01118      MOVE AM-CARRIER             TO  ACCT-CARR.                   
01119      MOVE AM-GROUPING            TO  ACCT-GRP.                    
01120      MOVE AM-STATE               TO  ACCT-ST.                     
01121      MOVE AM-ACCOUNT             TO  ACCT-NUM.                    
01122      MOVE LOW-VALUES             TO  ACCT-LOVAL.                  
01123      MOVE AM-NAME                TO  ACCT-NAME.                   
01124      MOVE AM-PERSON              TO  ACCT-PERSON.                 
01125      MOVE AM-ADDRS               TO  ACCT-ADDR1.                  
01126      MOVE AM-CITY                TO  ACCT-CTY-ST.                 
01127                                                                   
01128      IF  AM-CANADIAN-POST-CODE                                    
01129          MOVE AM-CAN-POSTAL-1    TO ACCT-CAN-POSTAL-CODE-1        
01130          MOVE AM-CAN-POSTAL-2    TO ACCT-CAN-POSTAL-CODE-2        
01131          MOVE SPACES             TO ACCT-CAN-FILLER               
01132                                                                   
01133      ELSE                                                         
01134          MOVE AM-ZIP-PRIME       TO ACCT-ZIP-PRIME                
01135                                                                   
01136          IF  AM-ZIP-PLUS4 = SPACES OR ZEROS                       
01137              MOVE SPACES         TO ACCT-ZIP-PLUS4                
01138                                                                   
01139          ELSE                                                     
01140              MOVE AM-ZIP-PLUS4   TO ACCT-ZIP-PLUS4.               
01141                                                                   
01142      MOVE ACCT-REC               TO  SORT-RECORD.                 
01143                                                                   
01144      IF AM-EDIT-LOAN-OFC EQUAL 'N'                                
01145          MOVE 'Y'                TO  SKIP-THIS-ACCT               
01146          GO TO 0999-EXIT                                          
01147      ELSE                                                         
01148          MOVE 'N'                TO  SKIP-THIS-ACCT.              
01149                                                                   
01150      PERFORM 0310-RELEASE  THRU  0319-EXIT.                       
01151                                                                   
01152  0999-EXIT.                                                       
01153      EXIT.                                                        
01154                                                                   
01155  1099-EXIT.                                                       
01156      EXIT.                                                        
01157  EJECT                                                            
01158  1100-PUT-REPORTS SECTION.                                        
01159      MOVE LOW-VALUES             TO  CONAMREC  ACCT-REC           
01160                                      LN-REC    PEND-REC.          
01161                                                                   
01162      RETURN SORT-WORK                                             
01163          AT END                                                   
01164              GO TO 1150-EOF.                                      
01165                                                                   
01166      MOVE SORT-RECORD            TO  CONAMREC.                    
01167      MOVE CONA-NAME              TO  HD2-CO.                      
01168      MOVE CONA-ANDT              TO  HD3-DATE.                    
01169      MOVE WS-CURRENT-DATE        TO  HD2-RUN.                     
01170                                                                   
01171      PERFORM 1400-INITIALIZE-TABLE  THRU  1499-EXIT               
01172          VARYING  TBLX  FROM  1  BY  1                            
01173              UNTIL  TBLX  IS EQUAL TO  202.                       
01174                                                                   
01175      MOVE LOW-VALUES             TO  LN-OFFCODE.                  
01176                                                                   
01177  1110-PRINT-LOOP.                                                 
01178      RETURN SORT-WORK                                             
01179          AT END                                                   
01180              GO TO 1150-EOF.                                      
01181                                                                   
01182      MOVE SC-CO                  TO  CTLCO.                       
01183      MOVE SC-CARR                TO  CTLCARR.                     
01184      MOVE SC-GRP                 TO  CTLGRP.                      
01185      MOVE SC-ST                  TO  CTLST.                       
01186      MOVE SC-ACCT                TO  CTLACCT.                     
01187      MOVE SC-LNOFF               TO  CTLOFCR.                     
01188                                                                   
01189      IF PRVKEY  IS EQUAL TO  LOW-VALUE                            
01190          MOVE CTLKEY             TO  PRVKEY.                      
01191                                                                   
01192      IF CPGCTL  IS NOT EQUAL TO  PPGCTL                           
01193          PERFORM 2000-MINORS.                                     
01194                                                                   
01195      IF SC-LNOFF  IS EQUAL TO  LOW-VALUE                          
01196          GO TO 1130-SET-ACCOUNT.                                  
01197                                                                   
01198      IF SC-EFFDT  IS EQUAL TO  LOW-VALUE                          
01199         GO TO 1140-SET-LOAN-OFFICER.                              
01200                                                                   
01201      MOVE 'Y'                    TO  HAVE-DATA.                   
01202      MOVE SORT-RECORD            TO  PEND-REC.                    
01203                                                                   
01204      IF LN-OPT2  IS EQUAL TO  'S'                                 
01205          GO TO 1120-ADD-ONLY.                                     
01206                                                                   
01207 *    IF LNCT  IS GREATER THAN  45                                 
PEMMOD     IF LNCT  IS GREATER THAN  43                                 
01208          PERFORM 1700-PAGE-HEADING  THRU  1799-EXIT.              
01209                                                                   
01210      MOVE 'Y'                    TO  PRNTG-DTL.                   
01211      MOVE SPACE                  TO  DT-1.                        
01212      MOVE PEND-CERT-PRIME        TO  D1-CERT-PRIME.               
01213      MOVE PEND-CERT-SFX          TO  D1-CERT-SFX.                 
01214      MOVE PEND-YRMODA            TO  WS-PEND-YRMODA.              
01215      MOVE PEND-MO                TO  D1-EFMO.                     
01216      MOVE PEND-DA                TO  D1-EFDA.                     
01217      MOVE PEND-YR                TO  D1-EFYR.                     
01218      MOVE '/'                    TO  D1-SL1  D1-SL2.              
01219      MOVE PEND-NAME              TO  D1-NAME.                     
01220      MOVE PEND-INIT              TO  D1-INIT.                     
01221      MOVE PEND-AGE               TO  D1-AGE.                      
01222      MOVE PEND-LF-TERM           TO  D1-LF-TERM.                  
01223      MOVE PEND-AH-TERM           TO  D1-AH-TERM.                  
01224      MOVE PEND-LF-TYP            TO  D1-LTYP.                     
01225      MOVE PEND-LF-PRM            TO  D1-LPRM.                     
01226      MOVE PEND-LF-AMT            TO  D1-LBEN.                     
01227      MOVE PEND-AH-TYP            TO  D1-HTYP.                     
01228      MOVE PEND-AH-PRM            TO  D1-HPRM.                     
01229      MOVE PEND-AH-AMT            TO  D1-HBEN.                     
01230                                                                   
01231      IF LN-OPT1  IS EQUAL TO  'Y'                                 
01232          ADD PEND-LF-COM  PEND-AH-COM  GIVING  D1-COMM.           
01233                                                                   
01234      IF LNCT  IS EQUAL TO  3                                      
01235          MOVE ZERO               TO  P-CTL                        
01236      ELSE                                                         
01237          MOVE SPACE              TO  P-CTL.                       
01238                                                                   
01239      MOVE DT-1                   TO  P-DATA.                      
01240                                                                   
01241      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    
01242                                                                   
01243      ADD 1                       TO  LNCT.                        
01244                                                                   
01245  1120-ADD-ONLY.                                                   
01246      ADD PEND-LF-AMT             TO  TBL-LBEN (TBLX)              
PEMMOD                                     FIN-LBEN
01247      ADD PEND-AH-AMT             TO  TBL-DBEN (TBLX)              
PEMMOD                                     FIN-DBEN
01248      ADD PEND-LF-COM             TO  TBL-LCOM (TBLX)              
PEMMOD                                     FIN-LCOM
01249      ADD PEND-AH-COM             TO  TBL-DCOM (TBLX)              
PEMMOD                                     FIN-DCOM
01250                                                                   
01251      IF PEND-LF-PRM  IS GREATER THAN  ZERO                        
01252          ADD 1                   TO  TBL-LICT (TBLX)              
PEMMOD                                     FIN-LICT
01253          ADD PEND-LF-PRM         TO  TBL-LPRM (TBLX)              
PEMMOD                                     FIN-LPRM
01254      ELSE                                                         
01255          IF PEND-LF-PRM  IS LESS THAN  ZERO                       
01256              ADD 1               TO  TBL-LCCT (TBLX)              
PEMMOD                                     FIN-LCCT
01257              ADD PEND-LF-PRM     TO  TBL-LREF (TBLX)              
PEMMOD                                     FIN-LREF.
01258                                                                   
01259      IF PEND-AH-PRM  IS GREATER THAN  ZERO                        
01260          ADD 1                   TO  TBL-DICT (TBLX)              
PEMMOD                                     FIN-DICT
01261          ADD PEND-AH-PRM         TO  TBL-DPRM (TBLX)              
PEMMOD                                     FIN-DPRM
01262      ELSE                                                         
01263          IF PEND-AH-PRM  IS LESS THAN  ZERO                       
01264              ADD 1               TO  TBL-DCCT (TBLX)              
PEMMOD                                     FIN-DCCT
01265              ADD PEND-AH-PRM     TO  TBL-DREF (TBLX)              
PEMMOD                                     FIN-DREF.
01266                                                                   
01267      GO TO 1110-PRINT-LOOP.                                       
01268                                                                   
01269  1130-SET-ACCOUNT.                                                
01270      IF SC-ACCT-INFO  IS EQUAL TO  ACCT-INFO                      
01271          GO TO 1110-PRINT-LOOP.                                   
01272                                                                   
01273      IF PRNTG-DTL  IS EQUAL TO  'Y'                               
01274          PERFORM 1300-DETAIL-TOTALS  THRU  1399-EXIT.             
01275                                                                   
01276      MOVE SORT-RECORD            TO  ACCT-REC.                    
01277      MOVE ACCT-CARR              TO  H5-CARR.                     
01278      MOVE ACCT-GRP               TO  H5-GRP.                      
01279      MOVE ACCT-ST                TO  H5-ST.                       
01280      MOVE ACCT-NUM               TO  H5-ACCT.                     
01281      MOVE ACCT-NAME              TO  H6-NA.                       
01282      MOVE ACCT-PERSON            TO  H7-NA.                       
01283      MOVE ACCT-ADDR1             TO  H8-NA.                       
01284      MOVE ACCT-CTY-ST            TO  H9-NA.                       
01285                                                                   
01286      IF  ACCT-CANADIAN-POST-CODE                                  
01287          MOVE ACCT-CAN-POSTAL-CODE-1                              
01288                                  TO H9-CAN-POSTAL-CODE-1          
01289          MOVE ACCT-CAN-POSTAL-CODE-2                              
01290                                  TO H9-CAN-POSTAL-CODE-2          
01291          MOVE SPACES             TO H9-DASH-CAN                   
01292                                     H9-CAN-FILLER                 
01293                                                                   
01294      ELSE                                                         
01295          MOVE ACCT-ZIP-PRIME     TO H9-ZIP-PRIME                  
01296          MOVE SPACES             TO H9-DASH                       
01297                                                                   
01298          IF  ACCT-ZIP-PLUS4 = SPACES OR ZEROS                     
01299              MOVE SPACES         TO H9-ZIP-PLUS4                  
01300                                                                   
01301          ELSE                                                     
01302              MOVE ACCT-ZIP-PLUS4 TO H9-ZIP-PLUS4.                 
01303                                                                   
01304      MOVE 'N'                    TO  HAVE-DATA.                   
01305      MOVE LOW-VALUES             TO  LN-OFFCODE.                  
01306      MOVE ZERO                   TO  TBLX.                        
01307      MOVE 80                     TO  LNCT.                        
01308      MOVE ACCT-CO                TO  TBL-CO.                      
01309      MOVE ACCT-CARR              TO  TBL-CARR.                    
01310      MOVE ACCT-GRP               TO  TBL-GRP.                     
01311      MOVE ACCT-ST                TO  TBL-ST.                      
01312      MOVE ACCT-NUM               TO  TBL-ACCT.                    
01313      MOVE ACCT-NAME              TO  TBL-NAME.                    
01314      MOVE ACCT-PERSON            TO  TBL-MAIL1.                   
01315      MOVE ACCT-ADDR1             TO  TBL-MAIL2.                   
01316      MOVE ACCT-CTY-ST            TO  TBL-MAIL3.                   
01317      MOVE ACCT-ZIP               TO  TBL-ZIP.                     
01318                                                                   
01319      GO TO 1110-PRINT-LOOP.                                       
01320                                                                   
01321  1140-SET-LOAN-OFFICER.                                           
01322      IF SC-LNOFF  IS EQUAL TO  LN-OFFCODE                         
01323          GO TO 1110-PRINT-LOOP.                                   
01324                                                                   
01325      IF PRNTG-DTL  IS EQUAL TO  'Y'                               
01326          PERFORM 1300-DETAIL-TOTALS  THRU  1399-EXIT.             
01327                                                                   
01328      MOVE SORT-RECORD            TO  LN-REC.                      
01329                                                                   
01330      IF LN-OFFCODE  IS EQUAL TO  HIGH-VALUES                      
01331          MOVE SPACE              TO  H10-LOF                      
01332      ELSE                                                         
01333          MOVE LN-OFFCODE         TO  H10-LOF.                     
01334                                                                   
01335      MOVE '-'                    TO  H10-DSH.                     
01336      MOVE LN-OFFCR               TO  H10-LONAME.                  
01337                                                                   
01338      ADD 1                       TO  TBLX.                        
01339                                                                   
01340      IF TBLX  IS GREATER THAN  200                                
01341          DISPLAY ' TOO MANY ENTRIES FOR TABLE ' SORT-CTL ' ' TBLX 
01342          MOVE 1                  TO  TBLX                         
01343          GO TO ABEND-PGM.                                         
01344                                                                   
01345      MOVE LN-OFFCODE             TO  TBL-CODE (TBLX).             
PEMMOD     MOVE LN-OFFCR               TO  TBL-LO-NAME (TBLX).
01346      MOVE 80                     TO  LNCT.                        
01347                                                                   
01348      GO TO 1110-PRINT-LOOP.                                       
01349                                                                   
01350  1150-EOF.                                                        
01351      PERFORM 2000-MINORS.                                         
01352                                                                   
PEMMOD     PERFORM 3000-FINAL-TOTALS   THRU 3000-EXIT
PEMMOD
01353      IF DTE-PGM-OPT  IS EQUAL TO  2                               
01354          CLOSE EXTRACT-INTERFACE-FILE                             
01355      ELSE                                                         
01356          CLOSE ERPNDB.                                            
01357                                                                   
01358      CLOSE ERLOFC  ERACCT.                                        
01359                                                                   
01360      IF DTE-PRT-OPT  IS EQUAL TO  'P'  OR  'B'  OR  'T'           
01361          CLOSE PRINT-FILE.                                        
01362                                                                   
01363  1199-EXIT.                                                       
01364      EXIT.                                                        
01365  EJECT                                                            
01366  1200-END-SORT-SECTION.                                           
01367                                                                   
01368  1300-DETAIL-TOTALS.                                              
PEMMOD
PEMMOD*    IF LNCT  IS GREATER THAN  34                                 
PEMMOD     IF LNCT  IS GREATER THAN  39
PEMMOD        PERFORM 1700-PAGE-HEADING                                 
PEMMOD                                 THRU  1799-EXIT                  
PEMMOD     END-IF
PEMMOD
01369      MOVE 'N'                    TO  PRNTG-DTL.                   
01370      MOVE SPACE                  TO  DT-1.                        
01371      MOVE 'TOTALS'               TO  D1-EFF.                      
01372      MOVE TBL-LBEN (TBLX)        TO  D1-LBEN.                     
01373      MOVE TBL-DBEN (TBLX)        TO  D1-HBEN.                     
01374                                                                   
01375      ADD TBL-LPRM (TBLX)  TBL-LREF (TBLX)  GIVING  D1-LPRM.       
01376      ADD TBL-DPRM (TBLX)  TBL-DREF (TBLX)  GIVING  D1-HPRM.       
01377                                                                   
01378      IF LN-OPT1  IS EQUAL TO  'Y'                                 
01379          ADD TBL-LCOM (TBLX)  TBL-DCOM (TBLX)  GIVING  D1-COMM.   
01380                                                                   
01381      MOVE DT-1                   TO  P-DATA.                      
01382      MOVE ZERO                   TO  P-CTL.                       
01383                                                                   
01384      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    
01385                                                                   
PEMMOD     MOVE '     TOTAL ISSUED PREMIUM ' TO DT-1
PEMMOD     COMPUTE WS-WORK-AMT =
PEMMOD        TBL-LPRM (TBLX) + TBL-DPRM (TBLX)
PEMMOD     MOVE WS-WORK-AMT           TO D1-LBEN
PEMMOD     MOVE DT-1                  TO P-DATA
PEMMOD     MOVE '0'                   TO P-CTL
PEMMOD     PERFORM 1600-PRINT-LINE THRU 1699-EXIT
PEMMOD
PEMMOD     MOVE '     TOTAL CANCELLED PREMIUM ' TO DT-1
PEMMOD     COMPUTE WS-WORK-AMT =
PEMMOD        TBL-LREF (TBLX) + TBL-DREF (TBLX)
PEMMOD     MOVE WS-WORK-AMT           TO D1-LBEN
PEMMOD     MOVE DT-1                  TO P-DATA
PEMMOD     MOVE '0'                   TO P-CTL
PEMMOD     PERFORM 1600-PRINT-LINE THRU 1699-EXIT
PEMMOD
PEMMOD     MOVE '     NET PREMIUM FOR LOAN OFFICER ' TO DT-1
PEMMOD     COMPUTE WS-WORK-AMT =
PEMMOD        TBL-LPRM (TBLX) + TBL-DPRM (TBLX) +
PEMMOD        TBL-LREF (TBLX) + TBL-DREF (TBLX)
PEMMOD     MOVE WS-WORK-AMT           TO D1-LBEN
PEMMOD     MOVE DT-1                  TO P-DATA
PEMMOD     MOVE '0'                   TO P-CTL
PEMMOD     PERFORM 1600-PRINT-LINE THRU 1699-EXIT
PEMMOD
PEMMOD     .
01386  1399-EXIT.                                                       
01387      EXIT.                                                        
01388                                                                   
01389  1400-INITIALIZE-TABLE.                                           
01390      MOVE LOW-VALUE              TO  TBL-CODE (TBLX).             
PEMMOD     MOVE SPACES                 TO  TBL-LO-NAME (TBLX)
01391      MOVE ZEROS                  TO  TBL-LICT (TBLX)              
01392                                      TBL-LBEN (TBLX)              
01393                                      TBL-LPRM (TBLX)              
01394                                      TBL-LCCT (TBLX)              
01395                                      TBL-LREF (TBLX)              
01396                                      TBL-LCOM (TBLX)              
01397                                      TBL-LOCT (TBLX)              
01398                                      TBL-LVOL (TBLX)              
01399                                      TBL-DICT (TBLX)              
01400                                      TBL-DBEN (TBLX)              
01401                                      TBL-DPRM (TBLX)              
01402                                      TBL-DCCT (TBLX)              
01403                                      TBL-DREF (TBLX)              
01404                                      TBL-DCOM (TBLX).             
01405                                                                   
01406  1499-EXIT.                                                       
01407      EXIT.                                                        
01408                                                                   
01409  1500-RE-INITIALIZE.                                              
01410      ADD TBL-LICT (TBLX)         TO  TBL-LICT (201).              
01411      ADD TBL-LBEN (TBLX)         TO  TBL-LBEN (201).              
01412      ADD TBL-LPRM (TBLX)         TO  TBL-LPRM (201).              
01413      ADD TBL-LCCT (TBLX)         TO  TBL-LCCT (201).              
01414      ADD TBL-LREF (TBLX)         TO  TBL-LREF (201).              
01415      ADD TBL-LCOM (TBLX)         TO  TBL-LCOM (201).              
01416      ADD TBL-DICT (TBLX)         TO  TBL-DICT (201).              
01417      ADD TBL-DBEN (TBLX)         TO  TBL-DBEN (201).              
01418      ADD TBL-DPRM (TBLX)         TO  TBL-DPRM (201).              
01419      ADD TBL-DCCT (TBLX)         TO  TBL-DCCT (201).              
01420      ADD TBL-DREF (TBLX)         TO  TBL-DREF (201).              
01421      ADD TBL-DCOM (TBLX)         TO  TBL-DCOM (201).              
01422                                                                   
01423  1599-EXIT.                                                       
01424      EXIT.                                                        
01425  EJECT                                                            
01426  1600-PRINT-LINE SECTION.                                         
01427      MOVE P-CTL                  TO  X.                           
01428                                                                   
01429      COPY ELCPRT2X.                                               
01430                                                                   
01431  1699-EXIT.                                                       
01432      EXIT.                                                        
01433  EJECT                                                            
01434  1700-PAGE-HEADING SECTION.                                       
01435      ADD 1                       TO  PGCT.                        
01436                                                                   
01437      MOVE ZERO                   TO  LNCT.                        
01438      MOVE PGCT                   TO  HD3-PG.                      
01439      MOVE '1'                    TO  P-CTL.                       
01440      MOVE HD-1                   TO  P-DATA.                      
01441                                                                   
01442      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    
01443                                                                   
01444      MOVE SPACE                  TO  P-CTL.                       
01445      MOVE HD-2                   TO  P-DATA.                      
01446                                                                   
01447      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    
01448                                                                   
01449      MOVE SPACE                  TO  P-CTL.                       
01450      MOVE HD-3                   TO  P-DATA.                      
01451                                                                   
01452      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    
01453                                                                   
01454      MOVE ZERO                   TO  P-CTL.                       
01455      MOVE HD-4                   TO  P-DATA.                      
01456                                                                   
01457      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    
01458                                                                   
01459      MOVE HD-5                   TO  P-DATA.                      
01460      MOVE ZERO                   TO  P-CTL.                       
01461                                                                   
01462      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    
01463                                                                   
01464      IF PRNTG-SUMM  IS EQUAL TO  'Y'                              
01465         MOVE 'PRODUCTION SUMMARY '                                
01466                                  TO  H6-DS                        
01467      ELSE                                                         
01464         IF PRNTG-SUMM  IS EQUAL TO  'F'                           
01465            MOVE '    FINAL TOTALS   '                             
01466                                  TO  H6-DS                        
01467         ELSE                                                      
01468            MOVE 'PRODUCTION DETAIL  '                             
01469                                  TO  H6-DS                        
PEMMOD        END-IF
PEMMOD     END-IF
01470                                                                   
01471      MOVE HD-6                   TO  P-DATA.                      
01472      MOVE ZERO                   TO  P-CTL.                       
01473                                                                   
01474      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    

01475                                                                   
01476      MOVE HD-7                   TO  P-DATA.                      
01477      MOVE SPACE                  TO  P-CTL.                       
01478                                                                   
01479      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    
01480                                                                   
01481      MOVE HD-8                   TO  P-DATA.                      
01482      MOVE SPACE                  TO  P-CTL.                       
01483                                                                   
01484      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    
01485                                                                   
01486      MOVE HD-9                   TO  P-DATA.                      
01487      MOVE ' '                    TO  P-CTL.                       
01488                                                                   
01489      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    
01490                                                                   
011506     ADD +12                     TO LNCT

01491      IF PRNTG-SUMM  IS EQUAL TO  'Y'                              
01492          GO TO 1710-BY-SUM1AND2.                                  
01493                                                                   
01494      MOVE ZERO                   TO  P-CTL.                       
01495      MOVE HD-10                  TO  P-DATA.                      
01496                                                                   
01497      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    
01498                                                                   
01499      IF LN-OPT1  IS EQUAL TO  'Y'                                 
01500          MOVE 'COMMISSION'       TO  HD9-VAR                      
01501      ELSE                                                         
01502          MOVE SPACE              TO  HD9-VAR.                     
01503                                                                   
01504      MOVE ZERO                   TO  P-CTL.                       
01505      MOVE HD-11                  TO  P-DATA.                      
01506                                                                   
01507      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    
01508                                                                   
01509      MOVE HD-12                  TO  P-DATA.                      
01510      MOVE SPACE                  TO  P-CTL.                       
01511                                                                   
01512      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    
01513                                                                   
01514      ADD 5                       TO  LNCT.                        
01515                                                                   
01516      GO TO 1799-EXIT.                                             
01517                                                                   
01518  1710-BY-SUM1AND2.                                                
01519      MOVE SUM-HD1                TO  P-DATA.                      
01520      MOVE ZERO                   TO  P-CTL.                       
01521                                                                   
01522      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    
01523                                                                   
01524      MOVE SUM-HD2                TO  P-DATA.                      
01525      MOVE SPACE                  TO  P-CTL.                       
01526                                                                   
01527      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    
01528                                                                   
01529      ADD 3                       TO  LNCT.                        
01530                                                                   
01531  1799-EXIT.                                                       
01532      EXIT.                                                        
01533  EJECT                                                            
01534  2000-MINORS SECTION.                                             
01535      IF PRNTG-DTL  IS EQUAL TO  'Y'                               
01536          PERFORM 1300-DETAIL-TOTALS  THRU  1399-EXIT.             
01537                                                                   
01538      IF HAVE-DATA  IS EQUAL TO  'N'                               
01539          MOVE CTLKEY             TO  PRVKEY                       
01540          GO TO 2199-MINORS-EX.                                    
01541                                                                   
01542      MOVE 80                     TO  LNCT.                        
01543      MOVE 'Y'                    TO  PRNTG-SUMM.                  
01544      MOVE 'N'                    TO  ANY-COMP.                    
01545                                                                   
01546      PERFORM 2100-MINOR-LOOP  THRU  2180-NON-UPD-LO               
01547          VARYING  AX  FROM  1  BY  1                              
01548              UNTIL  AX  IS GREATER THAN  TBLX.                    
01549                                                                   
01550      PERFORM 1500-RE-INITIALIZE  THRU  1599-EXIT                  
01551          VARYING  TBLX  FROM  1  BY  1                            
01552              UNTIL  TBLX  IS EQUAL TO  AX.                        
01553                                                                   
01554      PERFORM 1400-INITIALIZE-TABLE  THRU  1499-EXIT               
01555          VARYING  TBLX  FROM  1  BY  1                            
01556          UNTIL  TBLX  IS EQUAL TO  AX.                            
01557                                                                   
01558      MOVE CTLKEY                 TO  PRVKEY.                      
01559      MOVE 201                    TO  AX.                          
01560      MOVE HIGH-VALUE             TO  TBL-CODE (201).              
01561                                                                   
01562      PERFORM 2100-MINOR-LOOP  THRU  2180-NON-UPD-LO.              
01563                                                                   
011606     IF LNCT > 50
011606        PERFORM 1700-PAGE-HEADING
011606                                 THRU 1799-EXIT
011606     END-IF

PEMMOD     MOVE '     TOTAL ISSUED PREMIUM ' TO SUM-DT
PEMMOD     COMPUTE WS-WORK-AMT =
PEMMOD        TBL-LPRM (AX) + TBL-DPRM (AX)
PEMMOD     MOVE WS-WORK-AMT           TO SD-LBEN
PEMMOD     MOVE SUM-DT                TO P-DATA
PEMMOD     MOVE '0'                   TO P-CTL
PEMMOD     PERFORM 1600-PRINT-LINE THRU 1699-EXIT
PEMMOD
PEMMOD     MOVE '     TOTAL CANCELLED PREMIUM ' TO SUM-DT
PEMMOD     COMPUTE WS-WORK-AMT =
PEMMOD        TBL-LREF (AX) + TBL-DREF (AX)
PEMMOD     MOVE WS-WORK-AMT           TO SD-LBEN
PEMMOD     MOVE SUM-DT                TO P-DATA
PEMMOD     MOVE '0'                   TO P-CTL
PEMMOD     PERFORM 1600-PRINT-LINE THRU 1699-EXIT
PEMMOD
PEMMOD     MOVE '     NET PREMIUM FOR ACCOUNT      ' TO SUM-DT
PEMMOD     COMPUTE WS-WORK-AMT =
PEMMOD        TBL-LPRM (AX) + TBL-DPRM (AX) +
PEMMOD        TBL-LREF (AX) + TBL-DREF (AX)
PEMMOD     MOVE WS-WORK-AMT           TO SD-LBEN
PEMMOD     MOVE SUM-DT                TO P-DATA
PEMMOD     MOVE '0'                   TO P-CTL
PEMMOD     PERFORM 1600-PRINT-LINE THRU 1699-EXIT

092905     MOVE DISC-LINE-1            TO P-DATA
092905     MOVE '0'                    TO P-CTL
092905     PERFORM 1600-PRINT-LINE     THRU 1699-EXIT

092905     MOVE DISC-LINE-2            TO P-DATA
092905     MOVE ' '                    TO P-CTL
092905     PERFORM 1600-PRINT-LINE     THRU 1699-EXIT

092905     MOVE DISC-LINE-3            TO P-DATA
092905     MOVE ' '                    TO P-CTL
092905     PERFORM 1600-PRINT-LINE     THRU 1699-EXIT

PEMMOD
01564      MOVE 201                    TO  TBLX.                        
01565                                                                   
01566      PERFORM 1400-INITIALIZE-TABLE  THRU  1499-EXIT.              
01567                                                                   
01568      MOVE 'N'                    TO  PRNTG-SUMM.                  
01569                                                                   
PEMMOD     DIVIDE PGCT BY +2
PEMMOD          GIVING WK1
PEMMOD          REMAINDER WK2
PEMMOD     MOVE +0                     TO PGCT
PEMMOD
PEMMOD     IF WK2  NOT = +0
PEMMOD        MOVE SPACES              TO PRT
PEMMOD        MOVE '1'                 TO P-CTL
PEMMOD        PERFORM 1600-PRINT-LINE  THRU 1699-EXIT
PEMMOD     END-IF
PEMMOD
01570      GO TO 2199-MINORS-EX.                                        
01571                                                                   
01572  2100-MINOR-LOOP.                                                 
01573      MOVE TBL-CTL                TO  BUILD-LO-KEY.                
01574                                                                   
01575      IF DTE-COMP-VG  IS EQUAL TO  1                               
01576          GO TO 2105-LO-KEY-OK.                                    
01577                                                                   
01578      IF DTE-COMP-VG  IS EQUAL TO  3                               
01579          MOVE SPACES             TO  BLK-CARRIER                  
01580                                      BLK-GROUPING                 
01581                                      BLK-STATE                    
01582          GO TO 2105-LO-KEY-OK.                                    
01583                                                                   
01584      IF DTE-COMP-VG  IS EQUAL TO  2                               
01585          MOVE SPACES             TO  BLK-GROUPING                 
01586          GO TO 2105-LO-KEY-OK.                                    
01587                                                                   
01588      IF DTE-COMP-VG  IS EQUAL TO  4                               
01589          MOVE SPACES             TO  BLK-GROUPING                 
01590                                      BLK-STATE                    
01591          GO TO 2105-LO-KEY-OK.                                    
01592                                                                   
01593      IF DTE-COMP-VG  IS EQUAL TO  ' '                             
01594          MOVE SPACES             TO  BLK-CARRIER                  
01595                                      BLK-GROUPING                 
01596          GO TO 2105-LO-KEY-OK.                                    
01597                                                                   
01598  2105-LO-KEY-OK.                                                  
01599      MOVE BUILD-LO-KEY           TO  LO-CONTROL-PRIMARY.          
PEMMOD*    MOVE 'Y'                    TO  LO-UPD.                      
PEMMOD     MOVE 'N'                    TO  LO-UPD.                      
01601                                                                   
01602      IF TBL-CODE (AX)  IS EQUAL TO  HIGH-VALUE                    
01603          MOVE 'N'                TO  LO-UPD                       
01604          GO TO 2110-BY-GETLO.                                     
01605                                                                   
01606      MOVE TBL-CODE (AX)          TO  LO-OFFICER-CODE.             
01607                                                                   
01608      READ ERLOFC  INVALID KEY                                     
01609          MOVE 'N'                TO  LO-UPD.                      
01610                                                                   
01611  2110-BY-GETLO.                                                   
PEMTST*    IF LNCT  IS GREATER THAN  40                                 
PEMTST     IF LNCT  IS GREATER THAN  36                                 
01613          PERFORM 1700-PAGE-HEADING  THRU  1799-EXIT.              
01614                                                                   
01615      MOVE SPACE                  TO  SUM-DT.                      
01616      MOVE TBL-CODE (AX)          TO  SD-CODE.                     

           IF SD-CODE = LOW-VALUES OR HIGH-VALUES
              MOVE SPACES              TO SD-CODE
           END-IF
           
01617      MOVE '-'                    TO  SD-DASH.                     
PEMMOD     MOVE TBL-LO-NAME (AX)       TO  SD-NAME.
01618                                                                   
01619 *    IF LO-UPD  IS EQUAL TO  'N'                                  
01620 *        MOVE 'UNASSIGNED'       TO  SD-NAME                      
01621 *    ELSE                                                         
01622 *        MOVE LO-OFFICER-NAME    TO  SD-NAME.                     
01623                                                                   
01623                                                                   
01624      IF AX = 201                                                  
01625          MOVE 'TOTALS'           TO  SUM-DT
01625 *        MOVE 'TOTALS'           TO  SD-NAME                      
01626          IF DTE-CLIENT = 'UCL'                                    
01627              NEXT SENTENCE                                        
01628            ELSE                                                   
01629              GO TO 2110-CONTINUE                                  
01630        ELSE                                                       
01631          GO TO 2110-CONTINUE.                                     
01632                                                                   
01633      MOVE TBL-LOCT (AX)          TO  SD-LNCT.                     
01634      MOVE TBL-LVOL (AX)          TO  SD-LNAMT.                    
01635      COMPUTE SD-BPCT ROUNDED = TBL-LBEN (AX) * 100                
01636                              / TBL-LVOL (AX)                      
01637          ON SIZE ERROR                                            
01638              MOVE ZERO           TO  SD-BPCT.                     
01639                                                                   
01640      COMPUTE SD-LCCT ROUNDED = TBL-LICT (AX) * 100                
01641                              / TBL-LOCT (AX)                      
01642          ON SIZE ERROR                                            
01643              MOVE ZERO           TO  SD-LCCT.                     
01644                                                                   
01645      COMPUTE SD-DBPC ROUNDED = TBL-DBEN (AX) * 100                
01646                              / TBL-LVOL (AX)                      
01647          ON SIZE ERROR                                            
01648              MOVE ZERO           TO  SD-BPCT.                     
01649                                                                   
01650      COMPUTE SD-DCPC ROUNDED = TBL-DICT (AX) * 100                
01651                              / TBL-LOCT (AX)                      
01652          ON SIZE ERROR                                            
01653              MOVE ZERO           TO  SD-DCPC.                     
01654                                                                   
01655  2110-CONTINUE.                                                   
01656      IF LO-UPD  IS EQUAL TO  'N'                                  
01657          NEXT SENTENCE                                            
01658      ELSE                                                         
01659          MOVE LO-LOAN-COUNT (RUN-MO)                              
01660                                  TO  SD-LNCT                      
01661          MOVE LO-LOAN-VOLUME (RUN-MO)                             
01662                                  TO  SD-LNAMT.                    
01663                                                                   
01664      MOVE TBL-LICT (AX)          TO  SD-LICT.                     
01665      MOVE TBL-LBEN (AX)          TO  SD-LBEN.                     
01666      MOVE TBL-LPRM (AX)          TO  SD-LPRM.                     
01667      MOVE TBL-DICT (AX)          TO  SD-DICT.                     
01668      MOVE TBL-DBEN (AX)          TO  SD-DBEN.                     
01669      MOVE TBL-DPRM (AX)          TO  SD-DPRM.                     
01670                                                                   
01671      IF LO-UPD  IS EQUAL TO  'N'                                  
01672          GO TO 2130-BYCOMP.                                       
01673                                                                   
01674      COMPUTE SD-LCCT ROUNDED = TBL-LICT (AX) * 100                
01675                              / LO-LOAN-COUNT (RUN-MO)             
01676          ON SIZE ERROR                                            
01677              MOVE ZERO           TO  SD-LCCT.                     
01678                                                                   
01679      COMPUTE SD-BPCT ROUNDED = TBL-LBEN (AX) * 100                
01680                              / LO-LOAN-VOLUME (RUN-MO)            
01681          ON SIZE ERROR                                            
01682              MOVE ZERO           TO  SD-BPCT.                     
01683                                                                   
01684      COMPUTE SD-DCPC ROUNDED = TBL-DICT (AX) * 100                
01685                              / LO-LOAN-COUNT (RUN-MO)             
01686          ON SIZE ERROR                                            
01687              MOVE ZERO           TO  SD-DCPC.                     
01688                                                                   
01689      COMPUTE SD-DBPC ROUNDED = TBL-DBEN (AX) * 100                
01690                              / LO-LOAN-VOLUME (RUN-MO)            
01691          ON SIZE ERROR                                            
01692              MOVE ZERO           TO  SD-BPCT.                     
01693                                                                   
01694  2130-BYCOMP.                                                     
01695      MOVE SUM-DT                 TO  P-DATA.                      
01696      MOVE '0'                    TO  P-CTL.                       
01697                                                                   
01698      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    
01699                                                                   
01700      ADD 2                       TO  LNCT.                        
01701                                                                   
01702      IF TBL-LCCT (AX)  IS EQUAL TO  ZERO                          
01703        AND TBL-LREF (AX) IS EQUAL TO  ZERO                        
01704        AND TBL-DCCT (AX) IS EQUAL TO  ZERO                        
01705        AND TBL-DREF (AX) IS EQUAL TO  ZERO                        
01706          GO TO 2150-BY-REFUNDS.                                   
01707                                                                   
01708      MOVE SPACE                  TO  SUM-DT.                      
01709      MOVE 'CANCELLATIONS'        TO  SD-NAME.                     
01710      MOVE TBL-LCCT (AX)          TO  SD-LICT.                     
01711      MOVE TBL-LREF (AX)          TO  SD-LPRM.                     
01712      MOVE TBL-DCCT (AX)          TO  SD-DICT.                     
01713      MOVE TBL-DREF (AX)          TO  SD-DPRM.                     
01714      MOVE SUM-DT                 TO  P-DATA.                      
01715      MOVE SPACE                  TO  P-CTL.                       
01716                                                                   
01717      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    
01718                                                                   
01719      ADD 1                       TO  LNCT.                        
01720                                                                   
01721  2150-BY-REFUNDS.                                                 
01722      IF AX  IS EQUAL TO  201                                      
01723        AND ANY-COMP  IS EQUAL TO  'Y'                             
01724          NEXT SENTENCE                                            
01725      ELSE                                                         
01726          IF LO-UPD  IS EQUAL TO  'Y'                              
01727            AND LO-SUPPRESS-COMP                                   
01728              GO TO 2170-BY-COMP.                                  
01729                                                                   
01730      IF AX  IS EQUAL TO  201                                      
01731        AND ANY-COMP  IS EQUAL TO  'N'                             
01732          GO TO 2170-BY-COMP.                                      
01733                                                                   
01734      MOVE SPACE                  TO  SUM-DT.                      
01735      MOVE 'COMPENSATION'         TO  SD-NAME.                     
01736      MOVE TBL-LCOM (AX)          TO  SD-LPRM.                     
01737      MOVE TBL-DCOM (AX)          TO  SD-DPRM.                     
01738      MOVE SUM-DT                 TO  P-DATA.                      
01739      MOVE SPACE                  TO  P-CTL.                       
01740                                                                   
01741      PERFORM 1600-PRINT-LINE  THRU  1699-EXIT.                    
01742                                                                   
01743      ADD 1                       TO  LNCT.                        
01744                                                                   
01745      MOVE 'Y'                    TO  ANY-COMP.                    
01746                                                                   
01747  2170-BY-COMP.                                                    
01748      IF LO-UPD  IS EQUAL TO  'N'                                  
01749        OR TBL-CODE (AX)  IS EQUAL TO  HIGH-VALUE                  
01750          GO TO 2180-NON-UPD-LO.                                   
01751                                                                   
01752      MOVE TBL-LICT (AX)          TO  LO-LF-COUNT (RUN-MO).        
01753                                                                   
01754      ADD .5  TBL-LPRM (AX)  GIVING  LO-LF-PREM (RUN-MO).          
01755      ADD .5  TBL-LBEN (AX)  GIVING  LO-LF-BENEFIT (RUN-MO).       
01756                                                                   
01757      MOVE TBL-DICT (AX)          TO  LO-AH-COUNT (RUN-MO).        
01758                                                                   
01759      ADD .5  TBL-DPRM (AX)  GIVING  LO-AH-PREM (RUN-MO).          
01760      ADD .5  TBL-DBEN (AX)  GIVING  LO-AH-BENEFIT (RUN-MO).       
01761                                                                   
01762      IF DTE-CLIENT = 'UCL'                                        
01763          ADD TBL-LCOM (AX) TBL-DCOM (AX)  GIVING TOT-COMM         
01764          MOVE EP-MO              TO  HOLD-PROCESS-MO              
01765          MOVE EP-YR              TO  HOLD-PROCESS-YR              
01766          MOVE EP-CC              TO  HOLD-PROCESS-CC              
01767          MOVE TOT-COMM           TO  LO-TOTAL-COMMISSION          
01768          MOVE HOLD-PROCESS-MO-YR TO  LO-PROCESS-MO-YR.            
01769                                                                   
01770      REWRITE LOAN-OFFICER-MASTER.                                 
01771                                                                   
01772  2180-NON-UPD-LO.                                                 
01773      EXIT.                                                        
01774                                                                   
01775  2199-MINORS-EX.                                                  
01776      EXIT.                                                        
PEMMOD 3000-FINAL-TOTALS SECTION.
PEMMOD
PEMMOD     MOVE SPACES                 TO HD-5
PEMMOD                                    HD-7
PEMMOD                                    HD-8
PEMMOD                                    HD-9
PEMMOD                                    HD-10
PEMMOD                                    H6-NA
PEMMOD
PEMMOD     MOVE 'F'                    TO  PRNTG-SUMM                   
PEMMOD     PERFORM 1700-PAGE-HEADING   THRU  1799-EXIT                  
PEMMOD
01369      MOVE 'N'                    TO  PRNTG-DTL                    
01370      MOVE SPACE                  TO  DT-1                         
01371      MOVE 'GRAND '               TO  D1-EFF                       
01372      MOVE FIN-LBEN               TO  D1-LBEN                      
01373      MOVE FIN-DBEN               TO  D1-HBEN                      
01374                                                                   
01375      ADD FIN-LPRM  FIN-LREF   GIVING  D1-LPRM                     
01376      ADD FIN-DPRM  FIN-DREF   GIVING  D1-HPRM                     
01377                                                                   
01378      IF LN-OPT1  IS EQUAL TO  'Y'                                 
01379         ADD FIN-LCOM  FIN-DCOM  GIVING  D1-COMM                   
PEMMOD     END-IF
01380                                                                   
01381      MOVE DT-1                   TO  P-DATA                       
01382      MOVE ZERO                   TO  P-CTL                        
01383                                                                   
01384      PERFORM 1600-PRINT-LINE     THRU  1699-EXIT                  
01385                                                                   
PEMMOD     MOVE ' GRAND TOT ISSUED PREMIUM ' TO DT-1
PEMMOD     COMPUTE WS-WORK-AMT =
PEMMOD        FIN-LPRM  + FIN-DPRM
PEMMOD     MOVE WS-WORK-AMT            TO D1-LBEN
PEMMOD     MOVE DT-1                   TO P-DATA
PEMMOD     MOVE '0'                    TO P-CTL
PEMMOD     PERFORM 1600-PRINT-LINE     THRU 1699-EXIT
PEMMOD
PEMMOD     MOVE ' GRAND TOT CANCELLED PREMIUM ' TO DT-1
PEMMOD     COMPUTE WS-WORK-AMT =
PEMMOD        FIN-LREF  + FIN-DREF
PEMMOD     MOVE WS-WORK-AMT            TO D1-LBEN
PEMMOD     MOVE DT-1                   TO P-DATA
PEMMOD     MOVE '0'                    TO P-CTL
PEMMOD     PERFORM 1600-PRINT-LINE     THRU 1699-EXIT
PEMMOD
PEMMOD     MOVE ' GRAND TOT NET PREMIUM          ' TO DT-1
PEMMOD     COMPUTE WS-WORK-AMT =
PEMMOD        FIN-LPRM  + FIN-DPRM  +
PEMMOD        FIN-LREF  + FIN-DREF
PEMMOD     MOVE WS-WORK-AMT            TO D1-LBEN
PEMMOD     MOVE DT-1                   TO P-DATA
PEMMOD     MOVE '0'                    TO P-CTL
PEMMOD     PERFORM 1600-PRINT-LINE THRU 1699-EXIT
PEMMOD
PEMMOD     .
PEMMOD 3000-EXIT.
PEMMOD     EXIT.
PEMMOD
01777  EJECT                                                            
01778  ABEND-PGM SECTION.                                               
01779      COPY ELCABEND.                                               
01780                                                                   
01781  9000-EOJ SECTION.                                                
01782      COPY ELCPRTCX.                                               
01783                                                                   
01784      GOBACK.                                                      
