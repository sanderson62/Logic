00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                ECS082.                               
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 09/09/94 17:02:00.                 
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          
00008 *                          VMOD=2.016.                            
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
00024 *                                                                 
00025 *REMARKS.                                                         
00026 *        PRINT UNEARNED PREMIUM AND COMMISSION ANALYSIS.          
00027 *        PROGRAM SWITCHES - GAAP TOTALS BY -                      
00028 *            1 - ACCOUNT                                          
00029 *                STATE                                            
00030 *                COMPANY                                          
00031 *                CARRIER                                          
00032 *                FINAL TOTALS                                     
00033 *                OVERALL REINSURANCE TOTALS                       
00034 *            2 - STATE                                            
00035 *                COMPANY                                          
00036 *                CARRIER                                          
00037 *                FINAL TOTALS                                     
00038 *                OVERALL REINSURANCE TOTALS                       
00039 *            3 - COMPANY                                          
00040 *                CARRIER                                          
00041 *                FINAL TOTALS                                     
00042 *                OVERALL REINSURANCE TOTALS                       
00043 *            4 - CARRIER                                          
00044 *                FINAL TOTALS                                     
00045 *                OVERALL REINSURANCE TOTALS                       
00046 *            5 - FINAL TOTALS                                     
00047 *                OVERALL REINSURANCE TOTALS                       
00048 *            6 - OVERALL REINSURANCE TOTALS                       
00049  EJECT                                                            
00050 *       --- ECS082 ---                                            
00051 *       FORMAT OPTIONS ARE AS FOLLOWS                             
00052 *            1 - ALL INFORCE COVERAGES                            
00053 *            2 - INFORCE COVERAGES OVER 120 MONTHS                
00054 *            3 - INFORCE COVERAGES UNDER 121 MONTHS               
00055 *                                                                 
00056 *       --- ECS082 ---                                            
00057 *       TOTAL OPTIONS ARE AS FOLLOWS                              
00058 *            1 - NO TOTALS FOR EITHER REPORT CODES                
00059 *            2 - PRINT TOTALS BY REPORT CODE 1                    
00060 *            3 - PRINT TOTALS BY REPORT CODE 2                    
00061 *            4 - PRINT TOTALS BY REPORT CODES 1 AND 2             
00062 *                                                                 
00063 *     CUR-REC-TYPE                                                
00064 *          -  ...... PRC OPT TOTALS OVERALL...........ECS082A     
00065 *          1  ...... REPORT CODE 1 OVERALL............ECS082B     
00066 *          2  ...... REPORT CODE 2 WITHIN CARRIER.....ECS082C     
00067 *          3  .(MON) CARRIER WITHIN REPORT CODE 1.....ECS082D     
00068 *          4  ...... REIN COMPANY OVERALL.............ECS082R     
00069 *          5  .(MON) REIN COMPANY WITHIN REPT CD 1....ECS082S     
092602******************************************************************
092602*                   C H A N G E   L O G
092602*
092602* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
092602*-----------------------------------------------------------------
092602*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
092602* EFFECTIVE    NUMBER
092602*-----------------------------------------------------------------
092602* 092602    2002091900008  PEMA  INCREASE NUMBER OF MAXIMUM
092602*                                  BENEFIT CODES FROM 300 TO 900
042203* 042203  DON'T USE THIS PROGRAM IT IS A SPECIAL FOR JMDA ONLY
092602******************************************************************
00070                                                                   
00071  EJECT                                                            
00072  ENVIRONMENT DIVISION.                                            
00073  CONFIGURATION SECTION.                                           
00074  INPUT-OUTPUT SECTION.                                            
00075  FILE-CONTROL.                                                    
00076                                                                   
00077      SELECT SORTFL    ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.         
00078                                                                   
00079      SELECT PRNTR     ASSIGN TO SYS008-UR-1403-S-SYS008.          
00080      SELECT GAAP-EXTR ASSIGN TO SYS011-UT-2400-S-SYS011.          
00081                                                                   
00082      SELECT RTBL-FILE ASSIGN TO SYS014-3380-ERRTBLT
00083                              ACCESS       DYNAMIC                 
00084                              ORGANIZATION INDEXED                 
00085                              FILE STATUS  REIN-FILE-STATUS        
00086                              RECORD KEY   RE-CONTROL-PRIMARY.     
00087                                                                   
00088      SELECT ACCT-MSTR        ASSIGN TO SYS015-3380-ERACCTT        
00089                              ACCESS       SEQUENTIAL              
00090                              ORGANIZATION INDEXED                 
00091                              FILE STATUS  AM-FILE-STATUS          
00092                              RECORD KEY   AM-CONTROL-PRIMARY.     
00093                                                                   
00094      SELECT DISK-DATE ASSIGN TO SYS019-UT-FBA1-S-SYS019.          
00095      SELECT FICH      ASSIGN TO SYS020-UT-2400-S-SYS020.          
00096                                                                   
00097  EJECT                                                            
00098  DATA DIVISION.                                                   
00099  FILE SECTION.                                                    
00100                                                                   
00101  SD  SORTFL                                                       
00102                                    .                              
00103  01  SRT-REC.                                                     
00104      12  S-PARM                  PIC X(46).                       
00105      12  FILLER                  PIC X(158).                      
00106                                                                   
00107  FD  PRNTR                                                        
00108                              COPY ELCPRTFD.                       
00109  EJECT                                                            
00110  FD  GAAP-EXTR                                                    
00111                              COPY ECSGAPFD.                       
00112                                                                   
00113                              COPY ECSGAP01.                       
00114  EJECT                                                            
00115  FD  RTBL-FILE                                                    
00116                              COPY ECSRTFDD.                       
00117                                                                   
00118                              COPY ERCREIN.                        
00119  EJECT                                                            
00120  FD  ACCT-MSTR.                                                   
00121                              COPY ERCACCT.                        
00122  EJECT                                                            
00123  FD  DISK-DATE                                                    
00124                              COPY ELCDTEFD.                       
00125  EJECT                                                            
00126  FD  FICH                                                         
00127                              COPY ECSFICH.                        
00128  EJECT                                                            
00129  WORKING-STORAGE SECTION.                                         
00130  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      
00131  77  LCP-ASA                       PIC X.                         
00132  77  FILLER  PIC X(32) VALUE '********************************'.  
00133  77  FILLER  PIC X(32) VALUE '     ECS082 WORKING STORAGE     '.  
00134  77  FILLER  PIC X(32) VALUE '*********** VMOD=2.016 *********'.  
00135                                                                   
00136  77  AH-SW                       PIC S9     COMP-3.               
00137  77  LIFE-SW                     PIC S9     COMP-3.               
00138  77  FST-SW                      PIC S9     COMP-3.               
00139  77  HD-SW                       PIC S9     COMP-3   VALUE +0.    
00140  77  TAB-SW                      PIC S9     COMP-3   VALUE +0.    
00141  77  ZERO-SW                     PIC S9     COMP-3   VALUE +1.    
00142  77  PASS-SW                     PIC S9     COMP-3   VALUE +1.    
00143  77  MAX-MORT-LF-TYPES           PIC S999   COMP-3   VALUE +100.  
00144  77  X1                          PIC S999   COMP-3.               
00145  77  X2                          PIC S999   COMP-3.               
00146  77  M1                          PIC S9(4)  COMP-3.               
00147  77  M2                          PIC S999   COMP-3.               
00148  77  LNCTR                       PIC S999   COMP-3.               
092602 77  MAX-BEN                     PIC S999   COMP-3   VALUE +900.
00150  77  PGCTR                       PIC S9(5)  COMP-3.               
00151  77  X                           PIC X      VALUE SPACE.          
00152  77  HEAD-SW1                    PIC X      VALUE SPACE.          
00153  77  HEAD-SW                     PIC X      VALUE SPACE.          
00154      88  USE-DATE-LINE                      VALUE '1'.            
00155  77  REIN-FILE-STATUS            PIC XX     VALUE '00'.           
00156  77  AM-FILE-STATUS              PIC XX     VALUE '00'.           
00157  77  PRE-NDX                     PIC 999    VALUE ZEROS.          
00158  77  PRE-TAB                     PIC X(4)   VALUE LOW-VALUE.      
00159  77  W-TOTAL-RECORDS-READ        PIC S9(11) COMP-3    VALUE +0.   
00160  77  W-TOTAL-P-RECORDS           PIC S9(11) COMP-3    VALUE +0.   
00161  77  W-TOTAL-R-RECORDS           PIC S9(11) COMP-3    VALUE +0.   
00162  77  ACQ-PCT                     PIC S9V9999  COMP-3  VALUE +0.02.
00163  77  ACQ-WRITTEN                 PIC S9(9)V99 COMP-3  VALUE +0.   
00164  77  ACQ-P78                     PIC S9(9)V99 COMP-3  VALUE +0.   
00165  77  ACQ-PRATA                   PIC S9(9)V99 COMP-3  VALUE +0.   
00166  77  ACQ-DOMICILE                PIC S9(9)V99 COMP-3  VALUE +0.   
00167                                                                   
00168  EJECT                                                            
00169  01  WS.                                                          
00170      12  WS-SAVE-CARR-NAME   PIC X(30)   VALUE SPACES.            
00171      12  WS-SAVE-ST-NAME     PIC X(20)   VALUE SPACES.            
00172      12  WS-SAVE-ACCT-NAME   PIC X(30)   VALUE SPACES.            
00173      12  WS-SAVE-REIN-BY     PIC X(30)   VALUE SPACES.            
00174      12  WS-SAVE-CEDE-FROM   PIC X(30)   VALUE SPACES.            
00175      12  WS-RETURN-CODE      PIC S9(4)   VALUE +0         COMP.   
00176                                                                   
00177      12  WS-ABEND-MESSAGE    PIC X(80).                           
00178      12  WS-ABEND-MESSAGE-BEN REDEFINES WS-ABEND-MESSAGE.         
00179          16  FILLER          PIC X(9).                            
00180          16  WS-MESS-BEN     PIC X(6).                            
00181          16  FILLER          PIC X(20).                           
00182          16  WS-DISPLAY      PIC X(45).                           
00183                                                                   
00184      12  WS-ABEND-FILE-STATUS PIC X(2)   VALUE ZEROS.             
00185      12  WS-ZERO             PIC S9      VALUE +0 COMP-3.         
00186      12  PGM-SUB             PIC S999 COMP       VALUE +082.      
00187      12  FIRST-TIME-SW       PIC X               VALUE 'Y'.       
00188          88  FIRST-TIME                          VALUE 'Y'.       
00189      12  WS-ACCT-SEQ.                                             
00190          16  WS-ACCT-COMP-CD PIC X.                               
00191          16  WS-ACCT-KEY.                                         
00192              20  FILLER      PIC X(19).                           
00193              20  WS-ACCT-EXP PIC 9(11)   COMP-3.                  
00194      12  WS-REPORT-CODE-1    PIC X(10).                           
00195                                                                   
00196  01  REIN-TABLE-WORK-AREAS.                                       
00197      12  LAST-REIN-SEARCH    PIC X(8)            VALUE LOW-VALUE. 
00198      12  RTBL-OPEN-SW        PIC X               VALUE SPACE.     
00199                                                                   
00200  01  W-HD-WORK-AREA.                                              
00201      12  W-ZEROS                 PIC S9(04) COMP VALUE +0.        
00202      12  W-WL-NDX                PIC S9(04) COMP.                 
00203      12  W-WA2-NDX               PIC S9(04) COMP.                 
00204      12  W-WORK-LINE.                                             
00205          16  FILLER              PIC X(10).                       
00206          16  W-WORK-AREA-2.                                       
00207              20  W-WA2-CHAR OCCURS 86 TIMES                       
00208                                  PIC X(01).                       
00209      12  W-WORKING-LINE.                                          
00210          16  W-WL-CHAR OCCURS 86 TIMES                            
00211                                  PIC X(01).                       
00212  EJECT                                                            
00213  01  HD1.                                                         
00214      12  FILLER              PIC X(21)           VALUE SPACES.    
00215      12  HD1-FORMAT          PIC X(21)           VALUE SPACES.    
00216      12  FILLER              PIC X(40)           VALUE            
00217              'UNEARNED PREMIUM AND COMMISSION ANALYSIS'.          
00218      12  FILLER              PIC X(37)           VALUE SPACES.    
00219      12  FILLER              PIC X(6)            VALUE 'ECS082'.  
00220      12  HD1A                PIC X               VALUE 'A'.       
00221                                                                   
00222  01  HD2.                                                         
00223      12  FILLER              PIC X(47)           VALUE SPACES.    
00224      12  HD-CO               PIC X(30).                           
00225      12  FILLER              PIC X(42)           VALUE SPACES.    
00226      12  HD-RD               PIC X(8).                            
00227                                                                   
00228  01  HD3.                                                         
00229      12  FILLER              PIC X(53)           VALUE SPACES.    
00230                                                                   
00231      12  HD-DT               PIC X(18).                           
00232      12  FILLER              PIC X(48)           VALUE SPACES.    
00233      12  FILLER              PIC X(5)            VALUE 'PAGE '.   
00234      12  HD-PAGE             PIC ZZ,ZZ9.                          
00235                                                                   
00236  01  HDA.                                                         
00237      12  HDA-TOTAL-FOR       PIC X(10)           VALUE SPACES.    
00238      12  HDA-HEAD            PIC X(11)           VALUE 'CARRIER'. 
00239      12  HDA-RPT-CD1.                                             
00240          16  HDA-CARRIER     PIC X(01)           VALUE SPACES.    
00241          16  FILLER          PIC X               VALUE SPACES.    
00242          16  HDA-AST1        PIC X               VALUE '('.       
00243          16  HDA-CARR-NAME   PIC X(30)           VALUE SPACES.    
00244          16  HDA-AST2        PIC X               VALUE ')'.       
00245                                                                   
00246  01  HDB.                                                         
00247      12  HDB-TOTAL-FOR       PIC X(10)           VALUE SPACES.    
00248      12  HDB-HEAD            PIC X(11)           VALUE 'GROUP'.   
00249      12  HDB-GROUP           PIC X(10)           VALUE SPACES.    
00250                                                                   
00251  01  HDC.                                                         
00252      12  HDC-TOTAL-FOR       PIC X(10)           VALUE SPACES.    
00253      12  HDC-HEAD            PIC X(11)           VALUE 'STATE'.   
00254      12  HDC-RPT-CD2.                                             
00255          16  HDC-STATE       PIC X(02)           VALUE SPACES.    
00256          16  FILLER          PIC X               VALUE SPACES.    
00257          16  HDC-AST1        PIC X               VALUE '('.       
00258          16  HDC-ST-NAME     PIC X(30)           VALUE SPACES.    
00259          16  HDC-AST2        PIC X               VALUE ')'.       
00260                                                                   
00261  01  HDD.                                                         
00262      12  HDD-TOTAL-FOR       PIC X(10)           VALUE SPACES.    
00263      12  HDD-HEAD            PIC X(11)           VALUE 'ACCOUNT'. 
00264      12  HDD-ACCOUNT         PIC X(10)           VALUE SPACES.    
00265      12  FILLER              PIC X               VALUE SPACES.    
00266      12  HDD-AST1            PIC X               VALUE '('.       
00267      12  HDD-ACCT-NAME       PIC X(30)           VALUE SPACES.    
00268      12  HDD-AST2            PIC X               VALUE ')'.       
00269                                                                   
00270  01  HDE.                                                         
00271      12  HDE-TOTAL-FOR       PIC X(10)           VALUE SPACES.    
00272      12  HDE-HEAD            PIC X(11)           VALUE 'REIN SUB'.
00273      12  HDE-REIN-SUB        PIC X(10)           VALUE SPACES.    
00274                                                                   
00275  01  HDF.                                                         
00276      12  HDF-TOTAL-FOR       PIC X(10)           VALUE SPACES.    
00277      12  HDF-HEAD            PIC X(11)           VALUE            
00278          'REIN COMP'.                                             
00279      12  HDF-REIN-COMP       PIC X(10)           VALUE SPACES.    
00280                                                                   
00281  01  HDG.                                                         
00282      12  FILLER              PIC X(10)           VALUE SPACES.    
00283      12  HDG-HEAD            PIC X(14)           VALUE            
00284          'REINSURED BY'.                                          
00285      12  HDG-REIN-BY         PIC X(30)           VALUE SPACES.    
00286                                                                   
00287  01  HDH.                                                         
00288      12  FILLER              PIC X(10)           VALUE SPACES.    
00289      12  HDH-HEAD            PIC X(14)           VALUE            
00290          'CEDED FROM'.                                            
00291      12  HDH-CEDED-FROM      PIC X(30)           VALUE SPACES.    
00292                                                                   
00293  01  HD4.                                                         
00294      12 FILLER               PIC X(44)           VALUE            
00295             '                             * * * * * * * *'.       
00296      12 FILLER               PIC X(44)           VALUE            
00297             ' * *    P R E M I U M    * * * * * * * * * *'.       
00298      12 FILLER               PIC X(44)           VALUE            
00299             '                                            '.       
00300                                                                   
00301  01  HD5.                                                         
00302      12 HD-DESC              PIC X(15).                           
00303      12 FILLER               PIC X(29)           VALUE            
00304                            'IN FORCE                  UNE'.       
00305      12 FILLER               PIC X(44)           VALUE            
00306             'ARNED     UNEARNED     DOMICILE      STATE  '.       
00307      12 FILLER               PIC X(26)           VALUE            
00308             '    MORTALITY    ALTERNATE'.                         
00309      12 HD5-VALUE            PIC X(18)           VALUE            
00310             '        REMAINING '.                                 
00311                                                                   
00312  01  HD6.                                                         
00313      12 FILLER               PIC X(44)           VALUE            
00314             '                COUNT        WRITTEN      RU'.       
00315      12 FILLER               PIC X(44)           VALUE            
00316             'LE 78     PRO-RATA    STATUTORY    STATUTORY'.       
00317      12 FILLER               PIC X(44)           VALUE            
00318             '     RESERVE      RESERVE           AMOUNT  '.       
00319                                                                   
00320  01  HD7.                                                         
00321      12  FILLER                  PIC X(44)  VALUE                 
00322         '                        *-------------  C O '.           
00323      12  FILLER                  PIC X(44)  VALUE                 
00324         'M M I S S I O N  -------------* *-----------'.           
00325      12  FILLER                  PIC X(44)  VALUE                 
00326         '-------  T A X E S  ------------------*     '.           
00327                                                                   
00328  01  HD8.                                                         
00329      12  FILLER                  PIC X(44)  VALUE                 
00330         '                                PAID      RU'.           
00331      12  FILLER                  PIC X(44)  VALUE                 
00332         'LE 78     PRO-RATA     DOMICILE         PAID'.           
00333      12  FILLER                  PIC X(44)  VALUE                 
00334         '      RULE 78     PRO-RATA     DOMICILE     '.           
00335  01  HD9.                                                         
00336      12  FILLER                  PIC X(44)  VALUE                 
00337         '                        *-------  A C Q U I '.           
00338      12  FILLER                  PIC X(44)  VALUE                 
00339         'S I T I O N   C O S T  -------*             '.           
00340      12  FILLER                  PIC X(44)  VALUE SPACES.         
00341                                                                   
00342  01  HD10.                                                        
00343      12  FILLER                  PIC X(44)  VALUE                 
00344         '                                PAID      RU'.           
00345      12  FILLER                  PIC X(44)  VALUE                 
00346         'LE 78     PRO-RATA     DOMICILE             '.           
00347      12  FILLER                  PIC X(44)  VALUE SPACES.         
00348                                                                   
00349  01  HD11.                                                        
00350      12  FILLER                  PIC X(44)  VALUE                 
00351         '                        *------------  S E R'.           
00352      12  FILLER                  PIC X(44)  VALUE                 
00353         ' V I C E  F E E  -------------*             '.           
00354      12  FILLER                  PIC X(44)  VALUE                 
00355         '                                            '.           
00356                                                                   
00357  EJECT                                                            
00358  01  MORTALITY-HDS.                                               
00359      12  HD-4M.                                                   
00360          16  FILLER          PIC X(44)           VALUE            
00361                  '  MORTALITY                      BENEFIT    '.  
00362          16  FILLER          PIC X(44)           VALUE            
00363                  '        ------ INDIVIDUAL -------          -'.  
00364          16  FILLER          PIC X(44)           VALUE            
00365                  '-------- GROUP ---------             TOTAL  '.  
00366      12  HD-5M.                                                   
00367          16  FILLER          PIC X(44)           VALUE            
00368                  '    TABLE                         TYPE      '.  
00369          16  FILLER          PIC X(44)           VALUE            
00370                  '        UNDER 121        OVER 120          U'.  
00371          16  FILLER          PIC X(44)           VALUE            
00372                  'NDER 121        OVER 120            RESERVE '.  
00373                                                                   
00374  01  SUB-HEADINGS.                                                
00375                                                                   
00376      12  HEADLINE.                                                
00377          16  FILLER          PIC X(10)           VALUE            
00378                  'FOR DATES '.                                    
00379          16  EFFEC-MO        PIC 99.                              
00380          16  FILLER          PIC X               VALUE '/'.       
00381          16  EFFEC-DA        PIC 99.                              
00382          16  FILLER          PIC X               VALUE '/'.       
00383          16  EFFEC-YR        PIC 99.                              
00384          16  FILLER          PIC X(6)            VALUE ' THRU '.  
00385          16  EXPIRED-MO      PIC 99.                              
00386          16  FILLER          PIC X               VALUE '/'.       
00387          16  EXPIRED-DA      PIC 99.                              
00388          16  FILLER          PIC X               VALUE '/'.       
00389          16  EXPIRED-YR      PIC 99.                              
00390  EJECT                                                            
00391  01  MISC-WS.                                                     
00392      12  WORK-REC.                                                
00393          16  W-SEQ.                                               
00394              20  W-REC-TYPE      PIC X.                           
00395              20  W-RCO-RPT1.                                      
00396                22  W-RCO         PIC X(3).                        
00397                22  W-RPT-CD1     PIC X(10).                       
00398              20  FILLER REDEFINES W-RCO-RPT1.                     
00399                22  W1-RPT-CD1    PIC X(10).                       
00400                22  W1-RCO        PIC X(3).                        
00401              20  W-CARR          PIC X.                           
00402              20  W-CO            PIC X(6).                        
00403              20  W-RPT-CD2       PIC X(10).                       
00404              20  W-ST            PIC XX.                          
00405              20  W-ACCT          PIC X(10).                       
00406              20  W-RCOSUB        PIC X(3).                        
00407          16  W-NDX               PIC S999.                        
00408          16  W-DUP               PIC S9  COMP-3.                  
00409          16  W-TAB               PIC X(4).                        
00410          16  W-AMTS.                                              
00411              20  W-COUNT         PIC S9(7)     COMP-3.            
00412              20  W-WRITTEN       PIC S9(9)V99  COMP-3.            
00413              20  W-P78           PIC S9(9)V99  COMP-3.            
00414              20  W-PRATA         PIC S9(9)V99  COMP-3.            
00415              20  W-DOMICILE      PIC S9(9)V99  COMP-3.            
00416              20  W-STATE         PIC S9(9)V99  COMP-3.            
00417              20  W-RESERV        PIC S9(9)V99  COMP-3.            
00418              20  W-ALTRSV        PIC S9(9)V99  COMP-3.            
00419              20  W-REMAIN        PIC S9(13)V99 COMP-3.            
00420              20  W-PAID          PIC S9(9)V99  COMP-3.            
00421              20  W-C78           PIC S9(9)V99  COMP-3.            
00422              20  W-CRATA         PIC S9(9)V99  COMP-3.            
00423              20  W-CDOMI         PIC S9(9)V99  COMP-3.            
00424              20  W-TAX           PIC S9(9)V99  COMP-3.            
00425              20  W-T78           PIC S9(9)V99  COMP-3.            
00426              20  W-TRATA         PIC S9(9)V99  COMP-3.            
00427              20  W-TDOMI         PIC S9(9)V99  COMP-3.            
00428              20  W-SFPAID        PIC S9(9)V99  COMP-3.            
00429              20  W-SF78          PIC S9(9)V99  COMP-3.            
00430              20  W-SFRATA        PIC S9(9)V99  COMP-3.            
00431              20  W-SFDOMI        PIC S9(9)V99  COMP-3.            
00432          16  W-MORT-AMTS.                                         
00433              20  W-IUNDR         PIC S9(9)V99  COMP-3.            
00434              20  W-IOVER         PIC S9(9)V99  COMP-3.            
00435              20  W-GUNDR         PIC S9(9)V99  COMP-3.            
00436              20  W-GOVER         PIC S9(9)V99  COMP-3.            
00437      12  ACCT-SEQ.                                                
00438          16  ACCT-SEQA           PIC X(20)       VALUE LOW-VALUE. 
00439          16  ACCT-SEQB           PIC 9(11)  COMP-3.               
00440      12  SAVE-LPOINTERS.                                          
00441          16  SAVE-LPOINTER       PIC XXX         VALUE LOW-VALUE. 
00442          16  SAVE-LX2            PIC S999 COMP-3.                 
00443      12  SAVE-APOINTERS.                                          
00444          16  SAVE-APOINTER       PIC XXX         VALUE LOW-VALUE. 
00445          16  SAVE-AX2            PIC S999.                        
00446      12  CUR-SEQ.                                                 
00447          16  CUR-REC-TYPE    PIC X.                               
00448          16  CUR-RCO-RPT1.                                        
00449            20  CUR-RCO       PIC X(3).                            
00450            20  CUR-RPT-CD1   PIC X(10).                           
00451          16  FILLER REDEFINES CUR-RCO-RPT1.                       
00452            20  CUR1-RPT-CD1  PIC X(10).                           
00453            20  CUR1-RCO      PIC X(3).                            
00454          16  CUR-CARR        PIC X.                               
00455          16  CUR-CO          PIC X(6).                            
00456          16  CUR-RPT-CD2     PIC X(10).                           
00457          16  CUR-ST          PIC XX.                              
00458          16  CUR-ACCT        PIC X(10).                           
00459          16  CUR-RCOSUB      PIC X(3).                            
00460          16  CUR-DATE        PIC 9(11)   COMP-3.                  
00461      12  WX-SEQ.                                                  
00462          16  WX-REC-TYPE     PIC X.                               
00463          16  WX-RCO-RPT1.                                         
00464            20  WX-RCO        PIC X(3).                            
00465            20  WX-RPT-CD1    PIC X(10).                           
00466          16  FILLER REDEFINES WX-RCO-RPT1.                        
00467            20  WX1-RPT-CD1   PIC X(10).                           
00468            20  WX1-RCO       PIC X(3).                            
00469          16  WX-CARR         PIC X.                               
00470          16  WX-CO           PIC X(6).                            
00471          16  WX-RPT-CD2      PIC X(10).                           
00472          16  WX-ST           PIC XX.                              
00473          16  WX-ACCT         PIC X(10).                           
00474          16  WX-RCOSUB       PIC X(3).                            
00475          16  WX-DATE         PIC 9(11)   COMP-3.                  
00476      12  X-POINTERS.                                              
092602         16  X-MATCH     OCCURS 900 TIMES.
00478              20  X-POINTER.                                       
00479                  24  X-BEN       PIC XX.                          
00480                  24  X-TYP       PIC 9.                           
00481              20  X-DESC          PIC X(10).                       
00482      12  WX-POINTERS.                                             
00483          16  WX-POINTER.                                          
00484              20  WX-BEN          PIC XX.                          
00485              20  WX-TYP          PIC 9.                           
00486      12  PX-POINTER.                                              
00487          16  PX-BEN              PIC XXX.                         
00488          16  PX-DESC.                                             
00489              20  FILLER          PIC X(6).                        
00490              20  PX-BEN-DESC     PIC X(4).                        
00491                                                                   
00492  EJECT                                                            
00493  01  X-TOTALS.                                                    
00494      12  X-TOTS.                                                  
092602         16  X-LEVEL             OCCURS 900 TIMES.
00496              20  X-AMTS              PIC X(126).                  
00497  01  X-M-TOTALS.                                                  
00498 **   12  XM-TAB                  OCCURS 40 TIMES.                 
CIDMOD     12  XM-TAB                  OCCURS 60 TIMES.                 
00499          16  XM-BEN              OCCURS 100 TIMES.                
00500              20  XM-AMTS             PIC X(24).                   
00501                                                                   
00502  01  X-INDIVIDUAL-TOTALS.                                         
00503      12  X-DETL.                                                  
00504          16  X-COUNT                 PIC S9(7)     COMP-3.        
00505          16  X-WRITTEN               PIC S9(9)V99  COMP-3.        
00506          16  X-P78                   PIC S9(9)V99  COMP-3.        
00507          16  X-PRATA                 PIC S9(9)V99  COMP-3.        
00508          16  X-DOMICILE              PIC S9(9)V99  COMP-3.        
00509          16  X-STATE                 PIC S9(9)V99  COMP-3.        
00510          16  X-RESERV                PIC S9(9)V99  COMP-3.        
00511          16  X-ALTRSV                PIC S9(9)V99  COMP-3.        
00512          16  X-REMAIN                PIC S9(13)V99 COMP-3.        
00513          16  X-PAID                  PIC S9(9)V99  COMP-3.        
00514          16  X-C78                   PIC S9(9)V99  COMP-3.        
00515          16  X-CRATA                 PIC S9(9)V99  COMP-3.        
00516          16  X-CDOMI                 PIC S9(9)V99  COMP-3.        
00517          16  X-TAX                   PIC S9(9)V99  COMP-3.        
00518          16  X-T78                   PIC S9(9)V99  COMP-3.        
00519          16  X-TRATA                 PIC S9(9)V99  COMP-3.        
00520          16  X-TDOMI                 PIC S9(9)V99  COMP-3.        
00521          16  X-SFPAID                PIC S9(9)V99  COMP-3.        
00522          16  X-SF78                  PIC S9(9)V99  COMP-3.        
00523          16  X-SFRATA                PIC S9(9)V99  COMP-3.        
00524          16  X-SFDOMI                PIC S9(9)V99  COMP-3.        
00525      12  XM-DETL.                                                 
00526          16  X-IUNDR                 PIC S9(9)V99  COMP-3.        
00527          16  X-IOVER                 PIC S9(9)V99  COMP-3.        
00528          16  X-GUNDR                 PIC S9(9)V99  COMP-3.        
00529          16  X-GOVER                 PIC S9(9)V99  COMP-3.        
00530                                                                   
00531      12  R-DETL.                                                  
00532          16  R-COUNT                 PIC S9(7)     COMP-3.        
00533          16  R-WRITTEN               PIC S9(9)V99  COMP-3.        
00534          16  R-P78                   PIC S9(9)V99  COMP-3.        
00535          16  R-PRATA                 PIC S9(9)V99  COMP-3.        
00536          16  R-DOMICILE              PIC S9(9)V99  COMP-3.        
00537          16  R-STATE                 PIC S9(9)V99  COMP-3.        
00538          16  R-RESERV                PIC S9(9)V99  COMP-3.        
00539          16  R-ALTRSV                PIC S9(9)V99  COMP-3.        
00540          16  R-REMAIN                PIC S9(13)V99 COMP-3.        
00541          16  R-PAID                  PIC S9(9)V99  COMP-3.        
00542          16  R-C78                   PIC S9(9)V99  COMP-3.        
00543          16  R-CRATA                 PIC S9(9)V99  COMP-3.        
00544          16  R-CDOMI                 PIC S9(9)V99  COMP-3.        
00545          16  R-TAX                   PIC S9(9)V99  COMP-3.        
00546          16  R-T78                   PIC S9(9)V99  COMP-3.        
00547          16  R-TRATA                 PIC S9(9)V99  COMP-3.        
00548          16  R-TDOMI                 PIC S9(9)V99  COMP-3.        
00549          16  R-SFPAID                PIC S9(9)V99  COMP-3.        
00550          16  R-SF78                  PIC S9(9)V99  COMP-3.        
00551          16  R-SFRATA                PIC S9(9)V99  COMP-3.        
00552          16  R-SFDOMI                PIC S9(9)V99  COMP-3.        
00553      12  RM-DETL.                                                 
00554          16  R-IUNDR                 PIC S9(9)V99  COMP-3.        
00555          16  R-IOVER                 PIC S9(9)V99  COMP-3.        
00556          16  R-GUNDR                 PIC S9(9)V99  COMP-3.        
00557          16  R-GOVER                 PIC S9(9)V99  COMP-3.        
00558                                                                   
00559  01  REIN-SUB-TOTALS.                                             
00560      12  REIN-SUB-TOTS.                                           
092602         16  REIN-SUB-LEVEL          OCCURS 900 TIMES.
00562              20  REIN-SUB-AMTS           PIC X(126).              
00563  01  REIN-SUB-M-TOTALS.                                           
00564 **   12  REIN-SUB-M-TOTS             OCCURS 40 TIMES.             
CIDMOD     12  REIN-SUB-M-TOTS             OCCURS 60 TIMES.             
00565          16  REIN-SUB-M-LEVEL        OCCURS 100 TIMES.            
00566              20  REIN-SUB-M-AMTS         PIC X(24).               
00567                                                                   
00568  01  ACCT-TOTALS.                                                 
092602     12  FILLER                      PIC X(113400).
CIDMOD*    12  FILLER                      PIC X(34650).                
00570  01  ACCT-M-TOTALS.                                               
00571      12  FILLER                      PIC X(24000).                
00572      12  FILLER                      PIC X(24000).                
00573      12  FILLER                      PIC X(24000).                
00574      12  FILLER                      PIC X(24000).                
CIDMOD     12  FILLER                      PIC X(24000).                
CIDMOD     12  FILLER                      PIC X(24000).                
00575  01  ST-TOTALS.                                                   
092602     12  FILLER                      PIC X(113400).
CIDMOD*    12  FILLER                      PIC X(34650).                
00577  01  ST-M-TOTALS.                                                 
00578      12  FILLER                      PIC X(24000).                
00579      12  FILLER                      PIC X(24000).                
00580      12  FILLER                      PIC X(24000).                
00581      12  FILLER                      PIC X(24000).                
CIDMOD     12  FILLER                      PIC X(24000).                
CIDMOD     12  FILLER                      PIC X(24000).                
00582  01  RPTCD2-TOTALS.                                               
092602     12  FILLER                      PIC X(113400).
CIDMOD*    12  FILLER                      PIC X(34650).                
00584  01  RPTCD2-M-TOTALS.                                             
00585      12  FILLER                      PIC X(24000).                
00586      12  FILLER                      PIC X(24000).                
00587      12  FILLER                      PIC X(24000).                
00588      12  FILLER                      PIC X(24000).                
CIDMOD     12  FILLER                      PIC X(24000).                
CIDMOD     12  FILLER                      PIC X(24000).                
00589  01  CO-TOTALS.                                                   
092602     12  FILLER                      PIC X(113400).
CIDMOD*    12  FILLER                      PIC X(34650).                
00591  01  CO-M-TOTALS.                                                 
00592      12  FILLER                      PIC X(24000).                
00593      12  FILLER                      PIC X(24000).                
00594      12  FILLER                      PIC X(24000).                
00595      12  FILLER                      PIC X(24000).                
CIDMOD     12  FILLER                      PIC X(24000).                
CIDMOD     12  FILLER                      PIC X(24000).                
00596  01  CARR-TOTALS.                                                 
092602     12  FILLER                      PIC X(113400).
CIDMOD*    12  FILLER                      PIC X(34650).                
00598  01  CARR-M-TOTALS.                                               
00599      12  FILLER                      PIC X(24000).                
00600      12  FILLER                      PIC X(24000).                
00601      12  FILLER                      PIC X(24000).                
00602      12  FILLER                      PIC X(24000).                
CIDMOD     12  FILLER                      PIC X(24000).                
CIDMOD     12  FILLER                      PIC X(24000).                
00603  01  RPTCD1-TOTALS.                                               
092602     12  FILLER                      PIC X(113400).
CIDMOD*    12  FILLER                      PIC X(34650).                
00605  01  RPTCD1-M-TOTALS.                                             
00606      12  FILLER                      PIC X(24000).                
00607      12  FILLER                      PIC X(24000).                
00608      12  FILLER                      PIC X(24000).                
00609      12  FILLER                      PIC X(24000).                
CIDMOD     12  FILLER                      PIC X(24000).                
CIDMOD     12  FILLER                      PIC X(24000).                
00610  01  REIN-TOTALS.                                                 
092602     12  FILLER                      PIC X(113400).
CIDMOD*    12  FILLER                      PIC X(34650).                
00612  01  REIN-M-TOTALS.                                               
00613      12  FILLER                      PIC X(24000).                
00614      12  FILLER                      PIC X(24000).                
00615      12  FILLER                      PIC X(24000).                
00616      12  FILLER                      PIC X(24000).                
CIDMOD     12  FILLER                      PIC X(24000).                
CIDMOD     12  FILLER                      PIC X(24000).                
00617  01  FINAL-TOTALS.                                                
092602     12  FILLER                      PIC X(113400).
CIDMOD*    12  FILLER                      PIC X(34650).                
00619  01  FINL-M-TOTALS.                                               
00620      12  FILLER                      PIC X(24000).                
00621      12  FILLER                      PIC X(24000).                
00622      12  FILLER                      PIC X(24000).                
00623      12  FILLER                      PIC X(24000).                
CIDMOD     12  FILLER                      PIC X(24000).                
CIDMOD     12  FILLER                      PIC X(24000).                
00624  EJECT                                                            
00625  01  CALC-CTRS.                                                   
00626      12  SUB-TOTALS.                                              
00627          16  S-COUNT         PIC S9(7)     COMP-3.                
00628          16  S-WRITTEN       PIC S9(9)V99  COMP-3.                
00629          16  S-P78           PIC S9(9)V99  COMP-3.                
00630          16  S-PRATA         PIC S9(9)V99  COMP-3.                
00631          16  S-DOMICILE      PIC S9(9)V99  COMP-3.                
00632          16  S-STATE         PIC S9(9)V99  COMP-3.                
00633          16  S-RESERV        PIC S9(9)V99  COMP-3.                
00634          16  S-ALTRSV        PIC S9(9)V99  COMP-3.                
00635          16  S-REMAIN        PIC S9(13)V99 COMP-3.                
00636          16  S-PAID          PIC S9(9)V99  COMP-3.                
00637          16  S-C78           PIC S9(9)V99  COMP-3.                
00638          16  S-CRATA         PIC S9(9)V99  COMP-3.                
00639          16  S-CDOMI         PIC S9(9)V99  COMP-3.                
00640          16  S-TAX           PIC S9(9)V99  COMP-3.                
00641          16  S-T78           PIC S9(9)V99  COMP-3.                
00642          16  S-TRATA         PIC S9(9)V99  COMP-3.                
00643          16  S-TDOMI         PIC S9(9)V99  COMP-3.                
00644          16  S-SFPAID        PIC S9(9)V99  COMP-3.                
00645          16  S-SF78          PIC S9(9)V99  COMP-3.                
00646          16  S-SFRATA        PIC S9(9)V99  COMP-3.                
00647          16  S-SFDOMI        PIC S9(9)V99  COMP-3.                
00648      12  M-SUB-TOTALS.                                            
00649          16  S-IUNDR         PIC S9(9)V99  COMP-3.                
00650          16  S-IOVER         PIC S9(9)V99  COMP-3.                
00651          16  S-GUNDR         PIC S9(9)V99  COMP-3.                
00652          16  S-GOVER         PIC S9(9)V99  COMP-3.                
00653          16  S-TOTAL         PIC S9(9)V99  COMP-3.                
00654      12  TOTALS.                                                  
00655          16  T-COUNT         PIC S9(7)     COMP-3.                
00656          16  T-WRITTEN       PIC S9(9)V99  COMP-3.                
00657          16  T-P78           PIC S9(9)V99  COMP-3.                
00658          16  T-PRATA         PIC S9(9)V99  COMP-3.                
00659          16  T-DOMICILE      PIC S9(9)V99  COMP-3.                
00660          16  T-STATE         PIC S9(9)V99  COMP-3.                
00661          16  T-RESERV        PIC S9(9)V99  COMP-3.                
00662          16  T-ALTRSV        PIC S9(9)V99  COMP-3.                
00663          16  T-REMAIN        PIC S9(13)V99 COMP-3.                
00664          16  T-PAID          PIC S9(9)V99  COMP-3.                
00665          16  T-C78           PIC S9(9)V99  COMP-3.                
00666          16  T-CRATA         PIC S9(9)V99  COMP-3.                
00667          16  T-CDOMI         PIC S9(9)V99  COMP-3.                
00668          16  T-TAX           PIC S9(9)V99  COMP-3.                
00669          16  T-T78           PIC S9(9)V99  COMP-3.                
00670          16  T-TRATA         PIC S9(9)V99  COMP-3.                
00671          16  T-TDOMI         PIC S9(9)V99  COMP-3.                
00672          16  T-SFPAID        PIC S9(9)V99  COMP-3.                
00673          16  T-SF78          PIC S9(9)V99  COMP-3.                
00674          16  T-SFRATA        PIC S9(9)V99  COMP-3.                
00675          16  T-SFDOMI        PIC S9(9)V99  COMP-3.                
00676      12  M-TOTALS.                                                
00677          16  T-IUNDR         PIC S9(9)V99  COMP-3.                
00678          16  T-IOVER         PIC S9(9)V99  COMP-3.                
00679          16  T-GUNDR         PIC S9(9)V99  COMP-3.                
00680          16  T-GOVER         PIC S9(9)V99  COMP-3.                
00681          16  T-TOTAL         PIC S9(9)V99  COMP-3.                
00682      12  X-TOTAL             PIC S9(9)V99  COMP-3.                
00683  EJECT                                                            
00684  01  SPECIAL-TOTALS.                                              
00685      12  CERT-WORK.                                               
00686          16  X-CALC-REMAIN       PIC S9(15)V99   COMP-3.          
00687          16  X-AH-EXP            PIC S9(9)V99    COMP-3.          
00688          16  X-AVG-REM-TERM      PIC S9(9)V99    COMP-3.          
00689          16  X-AVG-TOT-TERM      PIC S9(9)V99    COMP-3.          
00690      12  DATE-WORK.                                               
00691          16  DATE-AGE-REMAIN     PIC S9(15)V99   COMP-3.          
00692          16  DATE-AH-EXP         PIC S9(9)V99    COMP-3.          
00693          16  DATE-AVG-REM-TERM   PIC S9(9)V99    COMP-3.          
00694          16  DATE-AVG-TOT-TERM   PIC S9(9)V99    COMP-3.          
00695      12  ACCT-WORK.                                               
00696          16  ACCT-AGE-REMAIN     PIC S9(15)V99   COMP-3.          
00697          16  ACCT-AH-EXP         PIC S9(9)V99    COMP-3.          
00698          16  ACCT-AVG-REM-TERM   PIC S9(9)V99    COMP-3.          
00699          16  ACCT-AVG-TOT-TERM   PIC S9(9)V99    COMP-3.          
00700      12  STATE-WORK.                                              
00701          16  ST-AGE-REMAIN       PIC S9(15)V99   COMP-3.          
00702          16  ST-AH-EXP           PIC S9(9)V99    COMP-3.          
00703          16  ST-AVG-REM-TERM     PIC S9(9)V99    COMP-3.          
00704          16  ST-AVG-TOT-TERM     PIC S9(9)V99    COMP-3.          
00705      12  COMPANY-WORK.                                            
00706          16  CO-AGE-REMAIN       PIC S9(15)V99   COMP-3.          
00707          16  CO-AH-EXP           PIC S9(9)V99    COMP-3.          
00708          16  CO-AVG-REM-TERM     PIC S9(9)V99    COMP-3.          
00709          16  CO-AVG-TOT-TERM     PIC S9(9)V99    COMP-3.          
00710      12  CARRIER-WORK.                                            
00711          16  CARR-AGE-REMAIN     PIC S9(15)V99   COMP-3.          
00712          16  CARR-AH-EXP         PIC S9(9)V99    COMP-3.          
00713          16  CARR-AVG-REM-TERM   PIC S9(9)V99    COMP-3.          
00714          16  CARR-AVG-TOT-TERM   PIC S9(9)V99    COMP-3.          
00715      12  FINAL-WORK.                                              
00716          16  FINAL-AGE-REMAIN    PIC S9(15)V99   COMP-3.          
00717          16  FINAL-AH-EXP        PIC S9(9)V99    COMP-3.          
00718          16  FINAL-AVG-REM-TERM  PIC S9(9)V99    COMP-3.          
00719          16  FINAL-AVG-TOT-TERM  PIC S9(9)V99    COMP-3.          
00720      12  REIN-SUB-WORK.                                           
00721          16  REIN-SUB-AGE-REMAIN    PIC S9(15)V99   COMP-3.       
00722          16  REIN-SUB-AH-EXP        PIC S9(9)V99    COMP-3.       
00723          16  REIN-SUB-AVG-REM-TERM  PIC S9(9)V99    COMP-3.       
00724          16  REIN-SUB-AVG-TOT-TERM  PIC S9(9)V99    COMP-3.       
00725      12  REIN-WORK.                                               
00726          16  REIN-AGE-REMAIN    PIC S9(15)V99   COMP-3.           
00727          16  REIN-AH-EXP        PIC S9(9)V99    COMP-3.           
00728          16  REIN-AVG-REM-TERM  PIC S9(9)V99    COMP-3.           
00729          16  REIN-AVG-TOT-TERM  PIC S9(9)V99    COMP-3.           
00730      12  RPTCD1-WORK.                                             
00731          16  RPTCD1-AGE-REMAIN    PIC S9(15)V99   COMP-3.         
00732          16  RPTCD1-AH-EXP        PIC S9(9)V99    COMP-3.         
00733          16  RPTCD1-AVG-REM-TERM  PIC S9(9)V99    COMP-3.         
00734          16  RPTCD1-AVG-TOT-TERM  PIC S9(9)V99    COMP-3.         
00735      12  RPTCD2-WORK.                                             
00736          16  RPTCD2-AGE-REMAIN    PIC S9(15)V99   COMP-3.         
00737          16  RPTCD2-AH-EXP        PIC S9(9)V99    COMP-3.         
00738          16  RPTCD2-AVG-REM-TERM  PIC S9(9)V99    COMP-3.         
00739          16  RPTCD2-AVG-TOT-TERM  PIC S9(9)V99    COMP-3.         
00740      12  OTHR-WORK.                                               
00741          16  X-WT-REMAIN         PIC S9(5)V99  COMP-3  VALUE +0.  
00742          16  X-COMP-AVG-TERM     PIC S9(5)V99  COMP-3  VALUE +0.  
00743          16  X-COMP-TOT-TERM     PIC S9(5)V99  COMP-3  VALUE +0.  
00744      12  X-DUPS.                                                  
00745          16  X-DUP           PIC S9(7) COMP-3.                    
00746      12  AC-DUPS.                                                 
00747          16  AC-DUP          PIC S9(7) COMP-3.                    
00748      12  ST-DUPS.                                                 
00749          16  ST-DUP          PIC S9(7) COMP-3.                    
00750      12  CO-DUPS.                                                 
00751          16  CO-DUP          PIC S9(7) COMP-3.                    
00752      12  CARR-DUPS.                                               
00753          16  CARR-DUP        PIC S9(7) COMP-3.                    
00754      12  FINL-DUPS.                                               
00755          16  FINL-DUP        PIC S9(7) COMP-3.                    
00756      12  REIN-DUPS.                                               
00757          16  REIN-DUP        PIC S9(7) COMP-3.                    
00758      12  REIN-SUB-DUPS.                                           
00759          16  REIN-SUB-DUP    PIC S9(7) COMP-3.                    
00760      12  RPTCD1-DUPS.                                             
00761          16  RPTCD1-DUP      PIC S9(7) COMP-3.                    
00762      12  RPTCD2-DUPS.                                             
00763          16  RPTCD2-DUP      PIC S9(7) COMP-3.                    
00764      12  PRT-SPECIAL.                                             
00765          16  FILLER          PIC X(6)            VALUE            
00766                  'TOTAL '.                                        
00767          16  P-AH-DESC       PIC X(2)            VALUE            
00768                  '  '.                                            
00769          16  FILLER          PIC X(19)           VALUE            
00770                  '  MONTHLY EXPOSURE '.                           
00771          16  P-AH-EXP        PIC ZZZ,ZZZ,ZZZ.ZZ.                  
00772          16  FILLER          PIC XXX             VALUE SPACES.    
00773          16  FILLER          PIC X(36)           VALUE            
00774                  '  AGE WEIGHTED BY REMAINING BENEFIT '.          
00775          16  P-WT-REMAIN     PIC ZZZZ9.99.                        
00776          16  FILLER          PIC XXX             VALUE SPACES.    
00777          16  FILLER          PIC X(25)           VALUE            
00778                  '  AVERAGE REMAINING TERM '.                     
00779          16  P-AVG-REM-TERM  PIC ZZZZ9.99.                        
00780      12  PRT-SPECIAL-2.                                           
00781          16  FILLER          PIC X(91)           VALUE SPACES.    
00782          16  FILLER          PIC X(25)           VALUE            
00783                  '  AVERAGE ORIGINAL TERM  '.                     
00784          16  P-AVG-TOT-TERM  PIC ZZZZ9.99.                        
00785  EJECT                                                            
00786  01  P-REC.                                                       
00787      12  P-CCSW                  PIC X.                           
00788      12  P-LN.                                                    
00789          16  P-BEN               PIC X(3).                        
00790          16  P-DESC              PIC X(10).                       
00791          16  P-COUNT             PIC ZZ,ZZZ,ZZZ-.                 
00792          16  P-DETAIL-1.                                          
00793              20  P-WRITTEN       PIC ZZZZ,ZZZ,ZZZ-.               
00794              20  P-P78           PIC ZZZZ,ZZZ,ZZZ-.               
00795              20  P-PRATA         PIC ZZZZ,ZZZ,ZZZ-.               
00796              20  P-PDOMICILE     PIC ZZZZ,ZZZ,ZZZ-.               
00797              20  P-PSTATE        PIC ZZZZ,ZZZ,ZZZ-.               
00798              20  P-RESERV        PIC ZZZZ,ZZZ,ZZZ-.               
00799              20  P-ALTRSV        PIC ZZZZ,ZZZ,ZZZ-.               
00800              20  P-REMAIN        PIC ZZZZ,ZZZ,ZZZ,ZZZ-.           
00801          16  P-DETAIL-2 REDEFINES P-DETAIL-1.                     
00802              20  P-PAID          PIC ZZZZ,ZZZ,ZZZ-.               
00803              20  P-C78           PIC ZZZZ,ZZZ,ZZZ-.               
00804              20  P-CRATA         PIC ZZZZ,ZZZ,ZZZ-.               
00805              20  P-CDOMI         PIC ZZZZ,ZZZ,ZZZ-.               
00806              20  P-TAX           PIC ZZZZ,ZZZ,ZZZ-.               
00807              20  P-T78           PIC ZZZZ,ZZZ,ZZZ-.               
00808              20  P-TRATA         PIC ZZZZ,ZZZ,ZZZ-.               
00809              20  P-TDOMI         PIC ZZZZ,ZZZ,ZZZ-.               
00810              20  FILLER          PIC X(4).                        
00811          16  P-DETAIL-3 REDEFINES P-DETAIL-1.                     
00812              20  P-SFPAID        PIC ZZZZ,ZZZ,ZZZ-.               
00813              20  P-SF78          PIC ZZZZ,ZZZ,ZZZ-.               
00814              20  P-SFRATA        PIC ZZZZ,ZZZ,ZZZ-.               
00815              20  P-SFDOMI        PIC ZZZZ,ZZZ,ZZZ-.               
00816              20  FILLER          PIC X(56).                       
00817      12  P-LN-M REDEFINES P-LN.                                   
00818          16  PM-TABLE            PIC X(5).                        
00819          16  PM-TDESC            PIC X(27).                       
00820          16  PM-BEN              PIC XXX.                         
00821          16  PM-BDESC            PIC X(11).                       
00822          16  PM-IUNDR            PIC ZZZ,ZZZ,ZZZ,ZZZ-.            
00823          16  PM-IOVER            PIC ZZZ,ZZZ,ZZZ,ZZZ-.            
00824          16  PM-GUNDR            PIC ZZ,ZZZ,ZZZ,ZZZ,ZZZ-.         
00825          16  PM-GOVER            PIC ZZZ,ZZZ,ZZZ,ZZZ-.            
00826          16  PM-TOTAL            PIC ZZ,ZZZ,ZZZ,ZZZ,ZZZ-.         
00827                                                                   
00828                              COPY ELCDTECX.                       
00829                                                                   
00830                              COPY ELCDTEVR.                       
00831                                                                   
00832                              COPY ELCACCTV.                       
00833  EJECT                                                            
00834  PROCEDURE DIVISION.                                              
00835                                                                   
00836  0000-SET-START SECTION 49.                                       
00837                              COPY ELCDTERX SUPPRESS.              
00838                                                                   
           MOVE 2                      TO DTE-PGM-OPT
00839      MOVE WS-CURRENT-DATE TO  HD-RD.                              
00840      MOVE COMPANY-NAME    TO  HD-CO.                              
00841      MOVE ALPH-DATE       TO  HD-DT.                              
00842                                                                   
00843      IF DTE-FMT-OPT = 2                                           
00844         MOVE '     OVER 120 MONTHS '                              
00845                                  TO HD1-FORMAT.                   
00846                                                                   
00847      IF DTE-FMT-OPT = 3                                           
00848         MOVE '    UNDER 121 MONTHS '                              
00849                                  TO HD1-FORMAT.                   
00850                                                                   
00851      MOVE AH-OVERRIDE-L2          TO P-AH-DESC.                   
00852                                                                   
00853      PERFORM 3000-ZERO-RTN THRU 3099-EXIT.                        
00854      MOVE X-M-TOTALS          TO REIN-SUB-M-TOTALS.               
00855                                                                   
00856  0200-SORT-RTN SECTION.                                           
00857      SORT SORTFL ON ASCENDING KEY S-PARM                          
00858          INPUT  PROCEDURE 0300-INPUT-RTN   THRU  0799-EXIT        
00859          OUTPUT PROCEDURE 0900-OUTPUT-RTN  THRU  1099-EXIT.       
00860                                                                   
00861      IF SORT-RETURN NOT = ZEROS                                   
00862         MOVE ' SORT ABORTED ' TO WS-ABEND-MESSAGE                 
00863         MOVE 0101             TO WS-RETURN-CODE                   
00864         GO TO ABEND-PGM.                                          
00865                                                                   
00866      GO TO 9999-EOJ-RTN.                                          
00867  EJECT                                                            
00868  0300-INPUT-RTN SECTION 45.                                       
00869      OPEN INPUT  ACCT-MSTR                                        
00870                  GAAP-EXTR                                        
00871           OUTPUT PRNTR.                                           
00872                                                                   
00873      IF AM-FILE-STATUS  = '00' OR '97'                            
00874          NEXT SENTENCE                                            
00875        ELSE                                                       
00876          MOVE ' ACCT OPEN ERROR'    TO WS-ABEND-MESSAGE           
00877          MOVE AM-FILE-STATUS        TO WS-ABEND-FILE-STATUS       
00878          GO TO ABEND-PGM.                                         
00879                                                                   
00880      MOVE SPACES     TO  P-REC.                                   
00881      MOVE LOW-VALUE  TO  CUR-SEQ                                  
00882                          ACCT-SEQ                                 
00883                          WS-ACCT-SEQ                              
00884                          WX-SEQ.                                  
00885      MOVE +0         TO  FST-SW                                   
00886                          PGCTR.                                   
00887      MOVE +66        TO  LNCTR.                                   
00888      MOVE +1         TO  PASS-SW.                                 
00889                                                                   
00890      PERFORM 6000-LOAD-TABLES  THRU  6099-EXIT.                   
00891                                                                   
00892  0310-READ-RTN.                                                   
00893      READ GAAP-EXTR AT END                                        
00894          GO TO 0750-END-INPUT.                                    
00895                                                                   
00896      IF DTE-FMT-OPT = 2                                           
00897         IF GR-LF-TERM LESS THAN +121                              
00898            MOVE ZEROS TO GR-LFTYP.                                
00899                                                                   
00900      IF DTE-FMT-OPT = 2                                           
00901         IF GR-AH-TERM LESS THAN +121                              
00902            MOVE ZEROS TO GR-AHTYP.                                
00903                                                                   
00904      IF DTE-FMT-OPT = 3                                           
00905         IF GR-LF-TERM GREATER THAN +120                           
00906            MOVE ZEROS TO GR-LFTYP.                                
00907                                                                   
00908      IF DTE-FMT-OPT = 3                                           
00909         IF GR-AH-TERM GREATER THAN +120                           
00910            MOVE ZEROS TO GR-AHTYP.                                
00911                                                                   
00912      IF (GR-LFTYP EQUAL ZEROS) AND                                
00913         (GR-AHTYP EQUAL ZEROS)                                    
00914         GO TO 0310-READ-RTN.                                      
00915                                                                   
00916      IF GR-LFSRV NOT NUMERIC                                      
00917          MOVE ZEROS       TO GR-LFSRV.                            
00918      IF GR-AHSRV NOT NUMERIC                                      
00919          MOVE ZEROS       TO GR-AHSRV.                            
00920                                                                   
00921      ADD +1               TO W-TOTAL-RECORDS-READ.                
00922                                                                   
00923      IF GR-REIN = 'P'                                             
00924          ADD +1           TO W-TOTAL-P-RECORDS                    
00925      ELSE                                                         
00926          ADD +1           TO W-TOTAL-R-RECORDS.                   
00927                                                                   
00928      IF GR-REIN = ('P' OR 'R')                                    
00929          NEXT SENTENCE                                            
00930      ELSE                                                         
00931         MOVE ' INVALID GAAP RECORD ' TO WS-ABEND-MESSAGE          
00932         MOVE 0301                    TO WS-RETURN-CODE            
00933         GO TO ABEND-PGM.                                          
00934                                                                   
00935      MOVE GR-CARRIER      TO  WX-CARR.                            
00936      MOVE GR-GROUPING     TO  WX-CO.                              
00937      MOVE GR-STATE        TO  WX-ST.                              
00938      MOVE GR-ACCOUNT      TO  WX-ACCT.                            
00939                                                                   
00940      MOVE GR-ACC-EXPIRES  TO  WX-DATE.                            
00941                                                                   
00942      MOVE DTE-CLASIC-COMPANY-CD TO WS-ACCT-COMP-CD.               
00943      MOVE GR-CONTROL            TO WS-ACCT-KEY.                   
00944      MOVE GR-ACC-EXPIRES        TO WS-ACCT-EXP.                   
00945                                                                   
00946      IF WX-SEQ NOT = CUR-SEQ                                      
00947          PERFORM 1200-BREAK-RTN  THRU  1399-EXIT.                 
00948                                                                   
00949      MOVE +1  TO  X2.                                             
00950                                                                   
00951      IF WS-ACCT-SEQ = ACCT-SEQ                                    
00952          GO TO 0320-FIND-LIFE.                                    
00953                                                                   
00954  0311-FIND-ACCT-NM.                                               
00955      IF ACCT-SEQ GREATER THAN WS-ACCT-SEQ                         
00956          MOVE 'INVALID ACCOUNT'  TO  WS-SAVE-ACCT-NAME            
00957          GO TO 0320-FIND-LIFE.                                    
00958                                                                   
00959      IF ACCT-SEQ LESS THAN WS-ACCT-SEQ                            
00960          GO TO 0315-READ-ACCT.                                    
00961                                                                   
00962      MOVE AM-NAME       TO  WS-SAVE-ACCT-NAME.                    
00963      MOVE AM-EFF-MO     TO  EFFEC-MO.                             
00964      MOVE AM-EFF-DA     TO  EFFEC-DA.                             
00965      MOVE AM-EFF-YR     TO  EFFEC-YR.                             
00966      MOVE AM-EXP-MO     TO  EXPIRED-MO.                           
00967      MOVE AM-EXP-DA     TO  EXPIRED-DA.                           
00968      MOVE AM-EXP-YR     TO  EXPIRED-YR.                           
00969                                                                   
00970      IF DTE-CLIENT = 'NCL'                                        
00971          MOVE SPACES             TO AM-REPORT-CODE-1              
00972          IF AM-RETRO-POOL NOT = ZEROS  AND  SPACES                
00973            IF AM-RETRO-POOL = 'SINGLE'                            
00974              MOVE AM-ACCOUNT     TO AM-REPORT-CODE-1              
00975            ELSE                                                   
00976              MOVE AM-RETRO-POOL  TO AM-REPORT-CODE-1.             
00977                                                                   
00978      GO TO 0320-FIND-LIFE.                                        
00979                                                                   
00980  0315-READ-ACCT.                                                  
00981                                                                   
00982      READ ACCT-MSTR NEXT.                                         
00983                                                                   
00984      IF AM-FILE-STATUS = '10'                                     
00985         MOVE HIGH-VALUE  TO  ACCT-SEQ                             
00986         GO TO 0311-FIND-ACCT-NM.                                  
00987                                                                   
00988      IF AM-FILE-STATUS NOT = '00'                                 
00989         MOVE ' ACCT READ ERROR'    TO WS-ABEND-MESSAGE            
00990         MOVE AM-FILE-STATUS        TO WS-ABEND-FILE-STATUS        
00991         GO TO ABEND-PGM.                                          
00992                                                                   
00993      COPY ELCACCTI.                                               
00994                                                                   
00995      MOVE AM-CONTROL-PRIMARY TO  ACCT-SEQ.                        
00996                                                                   
00997      GO TO 0311-FIND-ACCT-NM.                                     
00998  EJECT                                                            
00999  0320-FIND-LIFE.                                                  
01000      IF GR-LFTYP = ZEROS                                          
01001          GO TO 0410-FIND-AH.                                      
01002                                                                   
01003      IF GR-SUMMARY-REC                                            
01004         MOVE GR-CNT-LF TO  X-COUNT                                
01005       ELSE                                                        
01006         MOVE +1        TO  X-COUNT.                               
01007                                                                   
01008      MOVE GR-LFPRM     TO  X-WRITTEN.                             
01009      MOVE GRR-LFPRM    TO  X-P78.                                 
01010      MOVE GRP-LFPRM    TO  X-PRATA.                               
01011      MOVE GRD-LFPRM    TO  X-DOMICILE.                            
01012      MOVE GRS-LFPRM    TO  X-STATE.                               
01013      MOVE GR-ALT-RESV  TO  X-ALTRSV.                              
01014      MOVE GR-RESV      TO  X-RESERV.                              
01015 *    MOVE GR-REM-AMT   TO  X-REMAIN.                              
           MOVE GR-LFBEN     TO  X-REMAIN
01016                                                                   
01017      MOVE GR-LFCOM     TO  X-PAID.                                
01018      MOVE GRR-LFCOM    TO  X-C78.                                 
01019      MOVE GRP-LFCOM    TO  X-CRATA.                               
01020      MOVE GRD-LFCOM    TO  X-CDOMI.                               
01021      MOVE GR-LFTAX     TO  X-TAX.                                 
01022      MOVE GRR-LFTAX    TO  X-T78.                                 
01023      MOVE GRP-LFTAX    TO  X-TRATA.                               
PEMMOD*    IF GR-LFTAX = ZEROS                                          
PEMMOD*        MOVE ZEROS    TO  X-TDOMI                                
PEMMOD*    ELSE                                                         
PEMMOD*        COMPUTE X-TDOMI ROUNDED = GRD-LFPRM * AM-REI-LF-TAX.     
01028                                                                   
01029 ****** NOTE : FIELDS WERE NOT ADDED TO THE GAAP COPYBOOK TO       
01030 ****** HOUSE ALL VALUES OF SERVICE FEES.  THEY ARE CALCULATED     
01031 ****** USING THE SAME PERCENT AS THEIR COUNTER PART COMMISSION    
01032 ****** FIELDS.                                                    
01033                                                                   
01034      MOVE GR-LFSRV     TO X-SFPAID.                               
01035      IF GR-LFPRM EQUAL ZEROS                                      
01036          MOVE ZEROS    TO X-SF78                                  
01037                           X-SFRATA                                
01038                           X-SFDOMI                                
01039      ELSE                                                         
01040          COMPUTE X-SF78 ROUNDED =                                 
01041                             (GRR-LFPRM / GR-LFPRM) * GR-LFSRV     
01042          COMPUTE X-SFRATA ROUNDED =                               
01043                             (GRP-LFPRM / GR-LFPRM) * GR-LFSRV     
01044          COMPUTE X-SFDOMI ROUNDED =                               
01045                             (GRD-LFPRM / GR-LFPRM) * GR-LFSRV.    
01046                                                                   
01047      MOVE CLAS-STARTM  TO  M1.                                    
01048                                                                   
01049  0330-FIND-MORT.                                                  
01050      IF GR-MORT-CODE = CLAS-MORT-CODE (M1) OR                     
01051         CLAS-MORT-CODE (M1) = SPACES                              
01052          NEXT SENTENCE                                            
01053      ELSE                                                         
01054          ADD +1  TO  M1                                           
01055          GO TO 0330-FIND-MORT.                                    
01056                                                                   
01057      MOVE +0  TO  X-IUNDR  X-IOVER  X-GUNDR  X-GOVER.             
01058                                                                   
01059      IF GR-IG = '1'                                               
01060          GO TO 0350-LIFE-IND.                                     
01061                                                                   
01062  0340-LIFE-GRP.                                                   
01063      IF GR-LF-TERM LESS THAN +121                                 
01064          MOVE X-RESERV TO  X-GUNDR                                
01065      ELSE                                                         
01066          MOVE X-RESERV TO  X-GOVER.                               
01067                                                                   
01068      GO TO 0360-CONT-LIFE.                                        
01069                                                                   
01070  0350-LIFE-IND.                                                   
01071      IF GR-LF-TERM LESS THAN +121                                 
01072          MOVE X-RESERV TO  X-IUNDR                                
01073      ELSE                                                         
01074          MOVE X-RESERV TO  X-IOVER.                               
01075                                                                   
01076  0360-CONT-LIFE.                                                  
01077      MOVE GR-LFTYP  TO  WX-BEN.                                   
01078      MOVE +1        TO  WX-TYP.                                   
01079                                                                   
01080      IF WX-POINTER = SAVE-LPOINTER                                
01081          MOVE SAVE-LX2  TO  X2.                                   
01082                                                                   
01083      IF GR-REIN = 'R'                                             
01084          GO TO 0370-LOOP-LIFE.                                    
01085                                                                   
01086      IF GR-SUMMARY-REC                                            
01087          COMPUTE DATE-AVG-REM-TERM = (GR-LF-REMTERM * GR-CNT-LF)  
01088                                      + DATE-AVG-REM-TERM          
01089          COMPUTE DATE-AVG-TOT-TERM = (GR-LF-TERM * GR-CNT-LF)     
01090                                      + DATE-AVG-TOT-TERM          
01091       ELSE                                                        
01092          COMPUTE DATE-AVG-REM-TERM =                              
01093                           DATE-AVG-REM-TERM + GR-LF-REMTERM       
01094          COMPUTE DATE-AVG-TOT-TERM =                              
01095                           DATE-AVG-TOT-TERM + GR-LF-TERM.         
01096                                                                   
01097      COMPUTE X-CALC-REMAIN ROUNDED = (GR-REM-AMT * GR-AGE).       
01098                                                                   
01099      ADD X-CALC-REMAIN           TO DATE-AGE-REMAIN.              
01100                                                                   
01101  0370-LOOP-LIFE.                                                  
01102                                                                   
01103      IF X2 GREATER THAN MAX-BEN                                   
01104             OR                                                    
01105         X-POINTER (X2) EQUAL LOW-VALUES                           
01106         MOVE ' INVALID        BENEFIT TYPE' TO WS-ABEND-MESSAGE   
01107         MOVE LIFE-OVERRIDE-L6 TO WS-MESS-BEN                      
01108         MOVE WX-POINTER       TO WS-DISPLAY                       
01109         MOVE 0401             TO WS-RETURN-CODE                   
01110         GO TO ABEND-PGM.                                          
01111                                                                   
01112      IF WX-POINTER NOT = X-POINTER (X2)                           
01113          ADD +1  TO  X2                                           
01114          GO TO 0370-LOOP-LIFE.                                    
01115                                                                   
01116      MOVE WX-POINTER      TO  SAVE-LPOINTER.                      
01117      MOVE X2              TO  SAVE-LX2.                           
01118                                                                   
01119      IF GR-REIN = 'R'                                             
01120         GO TO 0380-END-LIFE.                                      
01121                                                                   
01122      MOVE X-AMTS (X2)     TO  R-DETL.                             
01123                                                                   
01124      ADD X-COUNT    TO  R-COUNT.                                  
01125      ADD X-WRITTEN  TO  R-WRITTEN.                                
01126      ADD X-P78      TO  R-P78.                                    
01127      ADD X-PRATA    TO  R-PRATA.                                  
01128      ADD X-DOMICILE TO  R-DOMICILE.                               
01129      ADD X-STATE    TO  R-STATE.                                  
01130      ADD X-RESERV   TO  R-RESERV.                                 
01131      ADD X-ALTRSV   TO  R-ALTRSV.                                 
01132      ADD X-REMAIN   TO  R-REMAIN.                                 
01133      ADD X-PAID     TO  R-PAID.                                   
01134      ADD X-C78      TO  R-C78.                                    
01135      ADD X-CRATA    TO  R-CRATA.                                  
01136      ADD X-CDOMI    TO  R-CDOMI.                                  
01137      ADD X-TAX      TO  R-TAX.                                    
01138      ADD X-T78      TO  R-T78.                                    
01139      ADD X-TRATA    TO  R-TRATA.                                  
01140      ADD X-TDOMI    TO  R-TDOMI.                                  
01141      ADD X-SFPAID   TO  R-SFPAID.                                 
01142      ADD X-SF78     TO  R-SF78.                                   
01143      ADD X-SFRATA   TO  R-SFRATA.                                 
01144      ADD X-SFDOMI   TO  R-SFDOMI.                                 
01145                                                                   
01146      MOVE R-DETL  TO  X-AMTS (X2).                                
01147                                                                   
01148      IF X2 GREATER THAN MAX-MORT-LF-TYPES                         
01149          MOVE MAX-MORT-LF-TYPES  TO  M2                           
01150      ELSE                                                         
01151          MOVE X2                 TO  M2.                          
01152                                                                   
01153      MOVE XM-AMTS (M1 M2)  TO  RM-DETL.                           
01154                                                                   
01155      ADD X-IUNDR  TO  R-IUNDR.                                    
01156      ADD X-IOVER  TO  R-IOVER.                                    
01157      ADD X-GUNDR  TO  R-GUNDR.                                    
01158      ADD X-GOVER  TO  R-GOVER.                                    
01159                                                                   
01160      MOVE RM-DETL  TO  XM-AMTS (M1 M2).                           
01161                                                                   
01162  0380-END-LIFE.                                                   
01163      MOVE +0 TO W-DUP.                                            
01164                                                                   
01165      IF GR-AHTYP NOT = ZEROS                                      
01166         IF NOT GR-SUMMARY-REC                                     
01167            MOVE +1 TO  W-DUP                                      
01168            IF GR-REIN = 'P'                                       
01169               ADD +1  TO  X-DUP.                                  
01170                                                                   
01171      IF GR-REIN = 'R'                                             
01172          GO TO 0400-HAVE-LIFE-REIN.                               
01173                                                                   
01174      IF (DTE-TOT-OPT = '2' OR '4') AND                            
01175         (AM-REPORT-CODE-1 NOT = SPACES AND ZEROS AND LOW-VALUES)  
01176         MOVE LOW-VALUES       TO W-SEQ                            
01177         MOVE '1'              TO W-REC-TYPE                       
01178         MOVE AM-REPORT-CODE-1 TO W-RPT-CD1                        
01179         MOVE GR-MORT-CODE     TO W-TAB                            
01180         MOVE X2               TO W-NDX                            
01181         MOVE X-DETL           TO W-AMTS                           
01182         MOVE XM-DETL          TO W-MORT-AMTS                      
01183         MOVE WORK-REC         TO SRT-REC                          
01184         RELEASE SRT-REC.                                          
01185                                                                   
01186 ********** CARR WITHIN REPT CODE 1 ***********                    
01187      IF DTE-CLIENT = 'MON'                                        
01188         MOVE '3'              TO W-REC-TYPE                       
01189         MOVE GR-CARRIER       TO W-CARR                           
01190         MOVE GR-GROUPING      TO W-CO                             
01191         MOVE GR-STATE         TO W-ST                             
01192         MOVE WORK-REC         TO SRT-REC                          
01193         RELEASE SRT-REC.                                          
01194                                                                   
01195 ********** STATE WITHIN CARRIER     ***********                   
01196                                                                   
01197      IF DTE-CLIENT EQUAL 'NCB'                                    
01198         MOVE LOW-VALUES       TO W-SEQ                            
01199         MOVE '2'              TO W-REC-TYPE                       
01200         MOVE GR-STATE         TO W-RPT-CD2                        
01201         MOVE GR-CARRIER       TO W-CARR                           
01202         MOVE ZEROS            TO W-CO                             
01203         MOVE GR-MORT-CODE     TO W-TAB                            
01204         MOVE X2               TO W-NDX                            
01205         MOVE X-DETL           TO W-AMTS                           
01206         MOVE XM-DETL          TO W-MORT-AMTS                      
01207         MOVE WORK-REC         TO SRT-REC                          
01208         RELEASE SRT-REC                                           
01209      ELSE                                                         
01210      IF (DTE-TOT-OPT = '3' OR '4') AND                            
01211         (AM-REPORT-CODE-2 NOT = SPACES AND ZEROS AND LOW-VALUES)  
01212         MOVE LOW-VALUES       TO W-SEQ                            
01213         MOVE '2'              TO W-REC-TYPE                       
01214         MOVE AM-REPORT-CODE-2 TO W-RPT-CD2                        
01215         MOVE GR-CARRIER       TO W-CARR                           
01216         MOVE GR-GROUPING      TO W-CO                             
01217         MOVE GR-MORT-CODE     TO W-TAB                            
01218         MOVE X2               TO W-NDX                            
01219         MOVE X-DETL           TO W-AMTS                           
01220         MOVE XM-DETL          TO W-MORT-AMTS                      
01221         MOVE WORK-REC         TO SRT-REC                          
01222         RELEASE SRT-REC.                                          
01223                                                                   
01224      GO TO 0410-FIND-AH.                                          
01225                                                                   
01226  0400-HAVE-LIFE-REIN.                                             
01227      MOVE LOW-VALUES          TO W-SEQ.                           
01228      MOVE '4'                 TO W-REC-TYPE.                      
01229      MOVE GR-REINCO           TO W-RCO.                           
01230      MOVE GR-REINCO-SUB       TO W-RCOSUB.                        
01231      MOVE GR-CARRIER          TO W-CARR.                          
01232      MOVE GR-GROUPING         TO W-CO.                            
01233      MOVE GR-STATE            TO W-ST.                            
01234      MOVE GR-ACCOUNT          TO W-ACCT.                          
01235      MOVE GR-MORT-CODE        TO W-TAB.                           
01236      MOVE X2                  TO W-NDX.                           
01237      MOVE X-DETL              TO W-AMTS.                          
01238      MOVE XM-DETL             TO W-MORT-AMTS.                     
01239      MOVE WORK-REC            TO SRT-REC.                         
01240      RELEASE SRT-REC.                                             
01241                                                                   
01242 ********** REIN WITHIN REPT CODE 1 ***********                    
01243      IF DTE-CLIENT = 'MON'                                        
01244       IF DTE-TOT-OPT = '2' OR '4'                                 
01245        IF AM-REPORT-CODE-1 NOT = SPACES AND ZEROS AND LOW-VALUES  
01246          MOVE LOW-VALUES          TO W-SEQ                        
01247          MOVE '5'                 TO W-REC-TYPE                   
01248          MOVE AM-REPORT-CODE-1    TO W1-RPT-CD1                   
01249          MOVE GR-REINCO           TO W1-RCO                       
01250          MOVE GR-REINCO-SUB       TO W-RCOSUB                     
01251          MOVE GR-CARRIER          TO W-CARR                       
01252          MOVE GR-GROUPING         TO W-CO                         
01253          MOVE GR-STATE            TO W-ST                         
01254          MOVE GR-ACCOUNT          TO W-ACCT                       
01255          MOVE GR-MORT-CODE        TO W-TAB                        
01256          MOVE X2                  TO W-NDX                        
01257          MOVE X-DETL              TO W-AMTS                       
01258          MOVE XM-DETL             TO W-MORT-AMTS                  
01259          MOVE WORK-REC            TO SRT-REC                      
01260          RELEASE SRT-REC.                                         
01261                                                                   
01262  0410-FIND-AH.                                                    
01263      IF GR-AHTYP = ZEROS                                          
01264          GO TO 0310-READ-RTN.                                     
01265                                                                   
01266      IF GR-SUMMARY-REC                                            
01267         MOVE GR-CNT-AH  TO  X-COUNT                               
01268       ELSE                                                        
01269         MOVE +1         TO  X-COUNT.                              
01270                                                                   
01271      MOVE GR-AHPRM      TO  X-WRITTEN.                            
01272      MOVE GRR-AHPRM     TO  X-P78.                                
01273      MOVE GRP-AHPRM     TO  X-PRATA.                              
01274      MOVE GRD-AHPRM     TO  X-DOMICILE.                           
01275      MOVE GRS-AHPRM     TO  X-STATE.                              
01276      MOVE GR-AHCOM      TO  X-PAID.                               
01277      MOVE GRR-AHCOM     TO  X-C78.                                
01278      MOVE GRP-AHCOM     TO  X-CRATA.                              
01279      MOVE GRD-AHCOM     TO  X-CDOMI.                              
01280      MOVE GR-AHTAX      TO  X-TAX.                                
01281      MOVE GRR-AHTAX     TO  X-T78.                                
01282      MOVE GRP-AHTAX     TO  X-TRATA.                              
PEMMOD*    IF GR-AHTAX = ZEROS                                          
PEMMOD*        MOVE ZEROS     TO  X-TDOMI                               
PEMMOD*    ELSE                                                         
PEMMOD*        COMPUTE X-TDOMI ROUNDED = GRD-AHPRM * AM-REI-AH-TAX.     
01287                                                                   
01288 ****** NOTE : FIELDS WERE NOT ADDED TO THE GAAP COPYBOOK TO       
01289 ****** HOUSE ALL VALUES OF SERVICE FEES.  THEY ARE CALCULATED     
01290 ****** USING THE SAME PERCENT AS THEIR COUNTER PART COMMISSION    
01291 ****** FIELDS.                                                    
01292                                                                   
01293      MOVE GR-AHSRV     TO X-SFPAID.                               
01294      IF GR-AHPRM EQUAL ZEROS                                      
01295          MOVE ZEROS    TO X-SF78                                  
01296                           X-SFRATA                                
01297                           X-SFDOMI                                
01298      ELSE                                                         
01299          COMPUTE X-SF78 ROUNDED =                                 
01300                             (GRR-AHPRM / GR-AHPRM) * GR-AHSRV     
01301          COMPUTE X-SFRATA ROUNDED =                               
01302                             (GRP-AHPRM / GR-AHPRM) * GR-AHSRV     
01303          COMPUTE X-SFDOMI ROUNDED =                               
01304                             (GRD-AHPRM / GR-AHPRM) * GR-AHSRV.    
01305                                                                   
01306 *    COMPUTE X-REMAIN ROUNDED = GR-AHBEN * GR-AH-REMTERM.         
           COMPUTE X-REMAIN ROUNDED = GR-AHBEN * GR-AH-TERM
01307                                                                   
01308      MOVE +0        TO  X-RESERV                                  
01309                         X-ALTRSV.                                 
01310      MOVE GR-AHTYP  TO  WX-BEN.                                   
01311      MOVE +2        TO  WX-TYP.                                   
01312                                                                   
01313      IF WX-POINTER = SAVE-APOINTER                                
01314          MOVE SAVE-AX2  TO  X2.                                   
01315                                                                   
01316      IF GR-REIN = 'R'                                             
01317          GO TO 0420-LOOP-AH.                                      
01318                                                                   
01319      IF GR-SUMMARY-REC                                            
01320          COMPUTE DATE-AVG-REM-TERM = DATE-AVG-REM-TERM +          
01321                            (GR-AH-REMTERM * GR-CNT-AH)            
01322          COMPUTE DATE-AVG-TOT-TERM = DATE-AVG-TOT-TERM +          
01323                            (GR-AH-TERM * GR-CNT-AH)               
01324      ELSE                                                         
01325          IF GR-LFTYP = ZEROS                                      
01326              COMPUTE DATE-AVG-REM-TERM = DATE-AVG-REM-TERM        
01327                                        + GR-AH-REMTERM            
01328              COMPUTE DATE-AVG-TOT-TERM = DATE-AVG-TOT-TERM        
01329                                        + GR-AH-TERM.              
01330                                                                   
01331      ADD GR-AHBEN  TO  DATE-AH-EXP.                               
01332                                                                   
01333  0420-LOOP-AH.                                                    
01334      IF X2 GREATER THAN MAX-BEN                                   
01335             OR                                                    
01336         X-POINTER (X2) EQUAL LOW-VALUES                           
01337         MOVE ' INVALID BENEFIT TYPE' TO WS-ABEND-MESSAGE          
01338         MOVE AH-OVERRIDE-L6 TO WS-MESS-BEN                        
01339         MOVE WX-POINTER     TO WS-DISPLAY                         
01340         MOVE 0402           TO WS-RETURN-CODE                     
01341         GO TO ABEND-PGM.                                          
01342                                                                   
01343      IF WX-POINTER NOT = X-POINTER (X2)                           
01344          ADD +1  TO  X2                                           
01345          GO TO 0420-LOOP-AH.                                      
01346                                                                   
01347      MOVE WX-POINTER      TO  SAVE-APOINTER.                      
01348      MOVE X2              TO  SAVE-AX2.                           
01349                                                                   
01350      IF GR-REIN = 'R'                                             
01351         GO TO 0440-HAVE-AH-REIN.                                  
01352                                                                   
01353      MOVE X-AMTS (X2)     TO  R-DETL.                             
01354                                                                   
01355      ADD X-COUNT    TO  R-COUNT.                                  
01356      ADD X-WRITTEN  TO  R-WRITTEN.                                
01357      ADD X-P78      TO  R-P78.                                    
01358      ADD X-PRATA    TO  R-PRATA.                                  
01359      ADD X-DOMICILE TO  R-DOMICILE.                               
01360      ADD X-STATE    TO  R-STATE.                                  
01361      ADD X-RESERV   TO  R-RESERV.                                 
01362      ADD X-ALTRSV   TO  R-ALTRSV.                                 
01363      ADD X-REMAIN   TO  R-REMAIN.                                 
01364      ADD X-PAID     TO  R-PAID.                                   
01365      ADD X-C78      TO  R-C78.                                    
01366      ADD X-CRATA    TO  R-CRATA.                                  
01367      ADD X-CDOMI    TO  R-CDOMI.                                  
01368      ADD X-TAX      TO  R-TAX.                                    
01369      ADD X-T78      TO  R-T78.                                    
01370      ADD X-TRATA    TO  R-TRATA.                                  
01371      ADD X-TDOMI    TO  R-TDOMI.                                  
01372                                                                   
01373      ADD X-SFPAID   TO  R-SFPAID                                  
01374      ADD X-SF78     TO  R-SF78                                    
01375      ADD X-SFRATA   TO  R-SFRATA.                                 
01376      ADD X-SFDOMI   TO  R-SFDOMI.                                 
01377                                                                   
01378      MOVE R-DETL  TO  X-AMTS (X2).                                
01379                                                                   
01380      MOVE +0   TO W-DUP.                                          
01381                                                                   
01382      IF (DTE-TOT-OPT = '2' OR '4') AND                            
01383         (AM-REPORT-CODE-1 NOT = SPACES AND ZEROS AND LOW-VALUES)  
01384         MOVE LOW-VALUES       TO W-SEQ                            
01385         MOVE '1'              TO W-REC-TYPE                       
01386         MOVE AM-REPORT-CODE-1 TO W-RPT-CD1                        
01387         MOVE GR-MORT-CODE     TO W-TAB                            
01388         MOVE X2               TO W-NDX                            
01389         MOVE X-DETL           TO W-AMTS                           
01390         MOVE +0               TO W-IUNDR   W-GUNDR                
01391                                  W-IOVER   W-GOVER                
01392         MOVE WORK-REC         TO SRT-REC                          
01393         RELEASE SRT-REC.                                          
01394                                                                   
01395 ********** CARR WITHIN REPT CODE 1 ***********                    
01396      IF DTE-CLIENT = 'MON'                                        
01397         MOVE '3'              TO W-REC-TYPE                       
01398         MOVE GR-CARRIER       TO W-CARR                           
01399         MOVE GR-GROUPING      TO W-CO                             
01400         MOVE GR-STATE         TO W-ST                             
01401         MOVE WORK-REC         TO SRT-REC                          
01402         RELEASE SRT-REC.                                          
01403                                                                   
01404 ********** STATE WITHIN CARRIER    ***********                    
01405                                                                   
01406      IF DTE-CLIENT EQUAL 'NCB'                                    
01407         MOVE LOW-VALUES       TO W-SEQ                            
01408         MOVE '2'              TO W-REC-TYPE                       
01409         MOVE GR-STATE         TO W-RPT-CD2                        
01410         MOVE GR-CARRIER       TO W-CARR                           
01411         MOVE ZEROS            TO W-CO                             
01412         MOVE GR-MORT-CODE     TO W-TAB                            
01413         MOVE X2               TO W-NDX                            
01414         MOVE X-DETL           TO W-AMTS                           
01415         MOVE +0               TO W-IUNDR   W-GUNDR                
01416                                  W-IOVER   W-GOVER                
01417         MOVE WORK-REC         TO SRT-REC                          
01418         RELEASE SRT-REC                                           
01419      ELSE                                                         
01420      IF (DTE-TOT-OPT = '3' OR '4') AND                            
01421         (AM-REPORT-CODE-2 NOT = SPACES AND ZEROS AND LOW-VALUES)  
01422         MOVE LOW-VALUES       TO W-SEQ                            
01423         MOVE '2'              TO W-REC-TYPE                       
01424         MOVE AM-REPORT-CODE-2 TO W-RPT-CD2                        
01425         MOVE GR-CARRIER       TO W-CARR                           
01426         MOVE GR-GROUPING      TO W-CO                             
01427         MOVE GR-MORT-CODE     TO W-TAB                            
01428         MOVE X2               TO W-NDX                            
01429         MOVE X-DETL           TO W-AMTS                           
01430         MOVE +0               TO W-IUNDR   W-GUNDR                
01431                                  W-IOVER   W-GOVER                
01432         MOVE WORK-REC         TO SRT-REC                          
01433         RELEASE SRT-REC.                                          
01434                                                                   
01435      GO TO 0310-READ-RTN.                                         
01436                                                                   
01437  0440-HAVE-AH-REIN.                                               
01438      MOVE +0                  TO W-DUP.                           
01439      MOVE LOW-VALUES          TO W-SEQ.                           
01440      MOVE '4'                 TO W-REC-TYPE.                      
01441      MOVE GR-REINCO           TO W-RCO.                           
01442      MOVE GR-REINCO-SUB       TO W-RCOSUB.                        
01443      MOVE GR-CARRIER          TO W-CARR.                          
01444      MOVE GR-GROUPING         TO W-CO.                            
01445      MOVE GR-STATE            TO W-ST.                            
01446      MOVE GR-ACCOUNT          TO W-ACCT.                          
01447      MOVE SPACES              TO W-TAB.                           
01448      MOVE X2                  TO W-NDX.                           
01449      MOVE X-DETL              TO W-AMTS.                          
01450      MOVE +0                  TO W-IUNDR   W-GUNDR                
01451                                  W-IOVER   W-GOVER.               
01452      MOVE WORK-REC            TO SRT-REC.                         
01453      RELEASE SRT-REC.                                             
01454                                                                   
01455 ********** REIN WITHIN REPT CODE 1 ***********                    
01456      IF DTE-CLIENT = 'MON'                                        
01457       IF DTE-TOT-OPT = '2' OR '4'                                 
01458        IF AM-REPORT-CODE-1 NOT = SPACES AND ZEROS AND LOW-VALUES  
01459          MOVE +0                  TO W-DUP                        
01460          MOVE LOW-VALUES          TO W-SEQ                        
01461          MOVE '5'                 TO W-REC-TYPE                   
01462          MOVE AM-REPORT-CODE-1    TO W1-RPT-CD1                   
01463          MOVE GR-REINCO           TO W1-RCO                       
01464          MOVE GR-REINCO-SUB       TO W-RCOSUB                     
01465          MOVE GR-CARRIER          TO W-CARR                       
01466          MOVE GR-GROUPING         TO W-CO                         
01467          MOVE GR-STATE            TO W-ST                         
01468          MOVE GR-ACCOUNT          TO W-ACCT                       
01469          MOVE SPACES              TO W-TAB                        
01470          MOVE X2                  TO W-NDX                        
01471          MOVE X-DETL              TO W-AMTS                       
01472          MOVE +0                  TO W-IUNDR W-GUNDR              
01473                                      W-IOVER W-GOVER              
01474          MOVE WORK-REC            TO SRT-REC                      
01475          RELEASE SRT-REC.                                         
01476                                                                   
01477      GO TO 0310-READ-RTN.                                         
01478                                                                   
01479  EJECT                                                            
01480  0750-END-INPUT.                                                  
01481      CLOSE ACCT-MSTR                                              
01482            GAAP-EXTR.                                             
01483                                                                   
01484      DISPLAY '     '.                                             
01485      DISPLAY '     '.                                             
01486      DISPLAY 'TOTAL RECORDS READ - ' W-TOTAL-RECORDS-READ.        
01487      DISPLAY 'TOTAL P RCRDS READ - ' W-TOTAL-P-RECORDS.           
01488      DISPLAY 'TOTAL R RCRDS READ - ' W-TOTAL-R-RECORDS.           
01489                                                                   
01490      IF AM-FILE-STATUS NOT = '00'                                 
01491         MOVE ' CLOSE ERROR ON ACCT' TO WS-ABEND-MESSAGE           
01492         MOVE AM-FILE-STATUS         TO WS-ABEND-FILE-STATUS       
01493         GO TO ABEND-PGM.                                          
01494                                                                   
01495  0799-EXIT.                                                       
01496      EXIT.                                                        
01497  EJECT                                                            
01498  0900-OUTPUT-RTN SECTION 47.                                      
01499                                                                   
01500      MOVE +66  TO  LNCTR.                                         
01501 *****MOVE SPACES  TO CUR-SEQ.                                     
01502                                                                   
01503  0910-RETURN-LOOP.                                                
01504      RETURN SORTFL AT END                                         
01505          GO TO 1030-END-OUTPUT.                                   
01506                                                                   
01507      MOVE SRT-REC TO WORK-REC.                                    
01508                                                                   
01509      MOVE W-SEQ  TO  WX-SEQ.                                      
01510      MOVE ZEROS  TO  WX-DATE.                                     
01511                                                                   
01512      IF WX-SEQ NOT = CUR-SEQ                                      
01513          PERFORM 1200-BREAK-RTN  THRU  1399-EXIT                  
01514          GO TO 0980-NEW-NDX.                                      
01515                                                                   
01516      IF W-NDX = PRE-NDX                                           
01517          GO TO 0990-SAME-NDX.                                     
01518                                                                   
01519 *    IF W-NDX = 99                                                
01520 *        ADD W-COUNT  TO  X-DUP                                   
01521 *        GO TO 0910-RETURN-LOOP.                                  
01522                                                                   
01523  EJECT                                                            
01524  0980-NEW-NDX.                                                    
01525      MOVE W-NDX           TO  X2                                  
01526                               PRE-NDX.                            
01527                                                                   
01528  0981-READ-RTBL-FILE.                                             
01529      IF CUR-REC-TYPE NOT = '4' AND '5'                            
01530         GO TO 0990-SAME-NDX.                                      
01531                                                                   
01532      IF CUR-REC-TYPE = '5'                                        
01533          MOVE CUR1-RCO         TO RE-COMP-PRIME                   
01534        ELSE                                                       
01535          MOVE CUR-RCO          TO RE-COMP-PRIME.                  
01536                                                                   
01537      MOVE CUR-RCOSUB       TO RE-COMP-SUB.                        
01538                                                                   
01539      IF RTBL-OPEN-SW = 'X'                                        
01540          GO TO 0982-READ-RTBL-FILE-GO.                            
01541                                                                   
01542      MOVE 'X'  TO  RTBL-OPEN-SW.                                  
01543                                                                   
01544      OPEN INPUT RTBL-FILE.                                        
01545                                                                   
01546      IF REIN-FILE-STATUS  = '00' OR '97'                          
01547          NEXT SENTENCE                                            
01548        ELSE                                                       
01549          MOVE ' OPEN ERROR ON REIN' TO WS-ABEND-MESSAGE           
01550          MOVE REIN-FILE-STATUS      TO WS-ABEND-FILE-STATUS       
01551          GO TO ABEND-PGM.                                         
01552                                                                   
01553  0982-READ-RTBL-FILE-GO.                                          
01554      IF FIRST-TIME                                                
01555          MOVE 'N'                 TO  FIRST-TIME-SW               
01556          MOVE DTE-CLASIC-COMPANY-CD TO RE-COMPANY-CD              
01557          MOVE 'B'                 TO  RE-CODE.                    
01558                                                                   
01559      IF RE-CONTROL-PRIMARY = LAST-REIN-SEARCH                     
01560          GO TO 0990-SAME-NDX.                                     
01561                                                                   
01562      MOVE RE-CONTROL-PRIMARY      TO  LAST-REIN-SEARCH.           
01563                                                                   
01564      READ RTBL-FILE.                                              
01565                                                                   
01566      IF REIN-FILE-STATUS = '00'                                   
01567          NEXT SENTENCE                                            
01568      ELSE                                                         
01569          IF REIN-FILE-STATUS = '23'                               
01570              MOVE 'UNKNOWN'   TO  WS-SAVE-REIN-BY                 
01571                                   WS-SAVE-CEDE-FROM               
01572              GO TO 0990-SAME-NDX                                  
01573          ELSE                                                     
01574             MOVE ' READ ERROR ON REIN' TO WS-ABEND-MESSAGE        
01575             MOVE REIN-FILE-STATUS      TO WS-ABEND-FILE-STATUS    
01576             GO TO ABEND-PGM.                                      
01577                                                                   
01578      MOVE RE-NAME       TO  WS-SAVE-REIN-BY.                      
01579      MOVE RE-CEDE-NAME  TO  WS-SAVE-CEDE-FROM.                    
01580                                                                   
01581  0990-SAME-NDX.                                                   
01582      MOVE X-AMTS (X2)     TO  R-DETL.                             
01583      ADD W-COUNT    TO  R-COUNT.                                  
01584      ADD W-WRITTEN  TO  R-WRITTEN.                                
01585      ADD W-P78      TO  R-P78.                                    
01586      ADD W-PRATA    TO  R-PRATA.                                  
01587      ADD W-DOMICILE TO  R-DOMICILE.                               
01588      ADD W-STATE    TO  R-STATE.                                  
01589      ADD W-RESERV   TO  R-RESERV.                                 
01590      ADD W-ALTRSV   TO  R-ALTRSV.                                 
01591      ADD W-REMAIN   TO  R-REMAIN.                                 
01592      ADD W-PAID     TO  R-PAID.                                   
01593      ADD W-C78      TO  R-C78.                                    
01594      ADD W-CRATA    TO  R-CRATA.                                  
01595      ADD W-CDOMI    TO  R-CDOMI.                                  
01596      ADD W-TAX      TO  R-TAX.                                    
01597      ADD W-T78      TO  R-T78.                                    
01598      ADD W-TRATA    TO  R-TRATA.                                  
01599      ADD W-TDOMI    TO  R-TDOMI.                                  
01600      ADD W-DUP      TO  X-DUP.                                    
01601                                                                   
01602      ADD W-SFPAID   TO  R-SFPAID.                                 
01603      ADD W-SF78     TO  R-SF78.                                   
01604      ADD W-SFRATA   TO  R-SFRATA.                                 
01605      ADD W-SFDOMI   TO  R-SFDOMI.                                 
01606                                                                   
01607      MOVE R-DETL TO X-AMTS (X2).                                  
01608                                                                   
01609      IF W-IUNDR = +0 AND                                          
01610         W-IOVER = +0 AND                                          
01611         W-GUNDR = +0 AND                                          
01612         W-GOVER = +0                                              
01613          GO TO 0910-RETURN-LOOP.                                  
01614                                                                   
01615      IF X2    = M2 AND                                            
01616         W-TAB = PRE-TAB                                           
01617          GO TO 1020-SAME-TAB.                                     
01618                                                                   
01619      IF X2 GREATER THAN MAX-MORT-LF-TYPES AND                     
01620         M2 = MAX-MORT-LF-TYPES AND                                
01621         PRE-TAB = W-TAB                                           
01622          GO TO 1020-SAME-TAB.                                     
01623                                                                   
01624      IF PRE-TAB = LOW-VALUE                                       
01625          GO TO 1000-NEW-TAB.                                      
01626                                                                   
01627  EJECT                                                            
01628  1000-NEW-TAB.                                                    
01629      IF X2 GREATER THAN MAX-MORT-LF-TYPES                         
01630          MOVE MAX-MORT-LF-TYPES  TO  M2                           
01631      ELSE                                                         
01632          MOVE X2                 TO  M2.                          
01633                                                                   
01634      MOVE W-TAB  TO  PRE-TAB.                                     
01635      MOVE +1     TO  M1.                                          
01636                                                                   
01637  1010-LOOP-M1.                                                    
01638      IF CLAS-MORT-CODE (M1) = W-TAB OR SPACES                     
01639          NEXT SENTENCE                                            
01640      ELSE                                                         
01641          ADD +1  TO  M1                                           
01642          GO TO 1010-LOOP-M1.                                      
01643                                                                   
01644  1020-SAME-TAB.                                                   
01645      MOVE XM-AMTS (M1 M2)  TO  RM-DETL.                           
01646      ADD W-IUNDR  TO  R-IUNDR.                                    
01647      ADD W-IOVER  TO  R-IOVER.                                    
01648      ADD W-GUNDR  TO  R-GUNDR.                                    
01649      ADD W-GOVER  TO  R-GOVER.                                    
01650                                                                   
01651      MOVE RM-DETL  TO  XM-AMTS (M1 M2).                           
01652                                                                   
01653      GO TO 0910-RETURN-LOOP.                                      
01654                                                                   
01655  1030-END-OUTPUT.                                                 
01656      MOVE HIGH-VALUE  TO  WX-SEQ.                                 
01657      MOVE +2          TO  FST-SW.                                 
01658                                                                   
01659      PERFORM 1200-BREAK-RTN  THRU  1399-EXIT.                     
01660                                                                   
01661  1099-EXIT.                                                       
01662      EXIT.                                                        
01663  EJECT                                                            
01664  1100-CO-RESIDENT SECTION 30.                                     
01665                                                                   
01666  1200-BREAK-RTN.                                                  
01667      IF FST-SW = +0                                               
01668          MOVE +1  TO  FST-SW                                      
01669          GO TO 1280-INTL-FINAL.                                   
01670                                                                   
01671  1210-DATE-BREAK.                                                 
01672                                                                   
01673      MOVE SPACES            TO HDA                                
01674                                HDB                                
01675                                HDC                                
01676                                HDD                                
01677                                HDE                                
01678                                HDF                                
01679                                HDG                                
01680                                HDH.                               
01681                                                                   
01682      MOVE 'CARRIER'         TO HDA-HEAD.                          
01683      MOVE CUR-CARR          TO HDA-CARRIER.                       
01684      MOVE WS-SAVE-CARR-NAME TO HDA-CARR-NAME.                     
01685                                                                   
01686      MOVE 'GROUP'           TO HDB-HEAD.                          
01687      MOVE CUR-CO            TO HDB-GROUP.                         
01688                                                                   
01689      MOVE 'STATE'           TO HDC-HEAD.                          
01690      MOVE CUR-ST            TO HDC-STATE.                         
01691      MOVE WS-SAVE-ST-NAME   TO HDC-ST-NAME.                       
01692                                                                   
01693      MOVE 'ACCOUNT'         TO HDD-HEAD.                          
01694      MOVE CUR-ACCT          TO HDD-ACCOUNT.                       
01695      MOVE WS-SAVE-ACCT-NAME TO HDD-ACCT-NAME.                     
01696                                                                   
01697      MOVE '('               TO HDA-AST1                           
01698                                HDC-AST1                           
01699                                HDD-AST1.                          
01700      MOVE ')'               TO HDA-AST2                           
01701                                HDC-AST2                           
01702                                HDD-AST2.                          
01703                                                                   
01704      IF CUR-REC-TYPE = '1'                                        
01705         MOVE CUR-RPT-CD1              TO HDA-RPT-CD1              
01706         MOVE CLAS-REPORT-CD1-CAPTION  TO HDA-HEAD.                
01707                                                                   
01708      IF CUR-REC-TYPE = '2'                                        
01709         MOVE CUR-RPT-CD2         TO HDC-RPT-CD2                   
01710         IF DTE-CLIENT EQUAL 'NCB'                                 
01711            MOVE 'STATE'          TO HDC-HEAD                      
01712            MOVE SPACES           TO HDB-HEAD                      
01713                                     HDB-GROUP                     
01714         ELSE                                                      
01715            MOVE CLAS-REPORT-CD2-CAPTION TO HDC-HEAD.              
01716                                                                   
01717      IF CUR-REC-TYPE = '3'                                        
01718         MOVE CUR-RPT-CD1             TO HDA-CARR-NAME.            
01719                                                                   
01720      IF CUR-REC-TYPE = '4'                                        
01721         MOVE SPACES            TO HDD-ACCT-NAME                   
01722                                   HDD-AST1                        
01723                                   HDD-AST2                        
01724         MOVE 'REIN SUB'        TO HDE-HEAD                        
01725         MOVE CUR-RCOSUB        TO HDE-REIN-SUB                    
01726         MOVE 'REIN COMP'       TO HDF-HEAD                        
01727         MOVE CUR-RCO           TO HDF-REIN-COMP                   
01728         MOVE ' REINSURED BY '  TO HDG-HEAD                        
01729         MOVE WS-SAVE-REIN-BY   TO HDG-REIN-BY                     
01730         MOVE 'CEDED FROM  '    TO HDH-HEAD                        
01731         MOVE WS-SAVE-CEDE-FROM TO HDH-CEDED-FROM.                 
01732                                                                   
01733      IF CUR-REC-TYPE = '5'                                        
01734         MOVE CUR1-RPT-CD1      TO HDA-CARR-NAME                   
01735         MOVE SPACES            TO HDD-ACCT-NAME                   
01736                                   HDD-AST1                        
01737                                   HDD-AST2                        
01738         MOVE 'REIN SUB'        TO HDE-HEAD                        
01739         MOVE CUR-RCOSUB        TO HDE-REIN-SUB                    
01740         MOVE 'REIN COMP'       TO HDF-HEAD                        
01741         MOVE CUR1-RCO          TO HDF-REIN-COMP                   
01742         MOVE 'REINSURED BY'    TO HDG-HEAD                        
01743         MOVE WS-SAVE-REIN-BY   TO HDG-REIN-BY                     
01744         MOVE 'CEDED FROM'      TO HDH-HEAD                        
01745         MOVE WS-SAVE-CEDE-FROM TO HDH-CEDED-FROM.                 
01746                                                                   
01747      PERFORM 7100-REMOVE-SPACES THRU 7100-EXIT.                   
01748                                                                   
01749      PERFORM 5000-ROLL-RTN  THRU  5099-EXIT.                      
01750                                                                   
01751      ADD DATE-AGE-REMAIN   TO  REIN-SUB-AGE-REMAIN.               
01752      ADD DATE-AH-EXP       TO  REIN-SUB-AH-EXP.                   
01753      ADD DATE-AVG-REM-TERM TO  REIN-SUB-AVG-REM-TERM.             
01754      ADD DATE-AVG-TOT-TERM TO  REIN-SUB-AVG-TOT-TERM.             
01755                                                                   
01756      MOVE DATE-WORK  TO  CERT-WORK.                               
01757                                                                   
01758      ADD X-DUP       TO  REIN-SUB-DUP.                            
01759                                                                   
01760      MOVE 'X'        TO  HEAD-SW1.                                
01761                                                                   
01762      IF (USE-DATE-LINE) AND                                       
01763         (CUR-REC-TYPE NOT = '1' AND '2' AND '3')                  
01764          PERFORM 2000-BLD-RTN  THRU  2099-EXIT.                   
01765                                                                   
01766      MOVE ' '      TO  HEAD-SW1.                                  
01767      MOVE WX-DATE  TO  CUR-DATE.                                  
01768                                                                   
01769      IF CUR-SEQ = WX-SEQ                                          
01770          GO TO 1340-INTL-DATE.                                    
01771  EJECT                                                            
01772  1215-REIN-SUB-BREAK.                                             
01773                                                                   
01774      MOVE REIN-SUB-TOTALS    TO  X-TOTALS                         
01775                                                                   
01776      MOVE REIN-SUB-M-TOTALS  TO  X-M-TOTALS.                      
01777                                                                   
01778      MOVE ACCT-TOTALS        TO  REIN-SUB-TOTALS.                 
01779      MOVE ACCT-M-TOTALS      TO  REIN-SUB-M-TOTALS.               
01780                                                                   
01781      MOVE 'TOTAL FOR'        TO  HDE-TOTAL-FOR.                   
01782      MOVE '       IN'        TO  HDD-TOTAL-FOR                    
01783                                  HDC-TOTAL-FOR                    
01784                                  HDB-TOTAL-FOR                    
01785                                  HDA-TOTAL-FOR.                   
01786                                                                   
01787      IF  HDF-REIN-COMP GREATER THAN SPACES                        
01788          MOVE '       IN'    TO  HDF-TOTAL-FOR.                   
01789                                                                   
01790      PERFORM 5000-ROLL-RTN  THRU  5099-EXIT.                      
01791                                                                   
01792      MOVE REIN-SUB-TOTALS      TO  ACCT-TOTALS.                   
01793      MOVE REIN-SUB-M-TOTALS    TO  ACCT-M-TOTALS.                 
01794                                                                   
01795      ADD REIN-SUB-AGE-REMAIN   TO  ACCT-AGE-REMAIN.               
01796      ADD REIN-SUB-AH-EXP       TO  ACCT-AH-EXP.                   
01797      ADD REIN-SUB-AVG-REM-TERM TO  ACCT-AVG-REM-TERM.             
01798      ADD REIN-SUB-AVG-TOT-TERM TO  ACCT-AVG-TOT-TERM.             
01799                                                                   
01800      MOVE REIN-SUB-WORK  TO  CERT-WORK.                           
01801                                                                   
01802      ADD REIN-SUB-DUP    TO  AC-DUP.                              
01803                                                                   
01804      MOVE REIN-SUB-DUPS  TO  X-DUPS.                              
01805                                                                   
01806      IF (DTE-PGM-OPT NOT GREATER 1) AND                           
01807          (CUR-REC-TYPE = '4' OR '5')                              
01808          PERFORM 2000-BLD-RTN  THRU  2099-EXIT.                   
01809                                                                   
01810      MOVE WX-RCOSUB TO  CUR-RCOSUB.                               
01811                                                                   
01812      IF CUR-SEQ = WX-SEQ                                          
01813          GO TO 1335-INTL-REINSUB.                                 
01814  EJECT                                                            
01815  1220-ACCOUNT-BREAK.                                              
01816                                                                   
01817      MOVE ACCT-TOTALS    TO  X-TOTALS.                            
01818      MOVE ACCT-M-TOTALS  TO  X-M-TOTALS.                          
01819                                                                   
01820      MOVE ST-TOTALS      TO  REIN-SUB-TOTALS.                     
01821      MOVE ST-M-TOTALS    TO  REIN-SUB-M-TOTALS.                   
01822                                                                   
01823      MOVE 'TOTAL FOR'        TO  HDD-TOTAL-FOR.                   
01824      MOVE '       IN'        TO  HDC-TOTAL-FOR                    
01825                                  HDB-TOTAL-FOR                    
01826                                  HDA-TOTAL-FOR.                   
01827      MOVE SPACES             TO  HDE.                             
01828                                                                   
01829      IF  HDF-REIN-COMP GREATER THAN SPACES                        
01830          MOVE '       IN'    TO  HDF-TOTAL-FOR.                   
01831                                                                   
01832      PERFORM 5000-ROLL-RTN  THRU  5099-EXIT.                      
01833                                                                   
01834      MOVE REIN-SUB-TOTALS    TO  ST-TOTALS.                       
01835      MOVE REIN-SUB-M-TOTALS  TO  ST-M-TOTALS.                     
01836                                                                   
01837      ADD ACCT-AGE-REMAIN   TO  ST-AGE-REMAIN.                     
01838      ADD ACCT-AH-EXP       TO  ST-AH-EXP.                         
01839      ADD ACCT-AVG-REM-TERM TO  ST-AVG-REM-TERM.                   
01840      ADD ACCT-AVG-TOT-TERM TO  ST-AVG-TOT-TERM.                   
01841                                                                   
01842      MOVE ACCT-WORK  TO  CERT-WORK.                               
01843                                                                   
01844      ADD AC-DUP      TO  ST-DUP.                                  
01845                                                                   
01846      MOVE AC-DUPS    TO  X-DUPS.                                  
01847                                                                   
01848      IF (DTE-PGM-OPT NOT GREATER 1) AND                           
01849         (CUR-REC-TYPE NOT = '1' AND '2' AND '3')                  
01850          PERFORM 2000-BLD-RTN  THRU  2099-EXIT.                   
01851                                                                   
01852      MOVE WX-ACCT  TO  CUR-ACCT.                                  
01853                                                                   
01854      IF CUR-SEQ = WX-SEQ                                          
01855          GO TO 1330-INTL-ACCOUNT.                                 
01856  EJECT                                                            
01857  1230-STATE-BREAK.                                                
01858                                                                   
01859      MOVE ST-TOTALS        TO  X-TOTALS                           
01860      MOVE ST-M-TOTALS      TO  X-M-TOTALS.                        
01861                                                                   
01862      MOVE RPTCD2-TOTALS    TO  REIN-SUB-TOTALS.                   
01863      MOVE RPTCD2-M-TOTALS  TO  REIN-SUB-M-TOTALS.                 
01864                                                                   
01865      MOVE 'TOTAL FOR'        TO  HDC-TOTAL-FOR.                   
01866      MOVE '       IN'        TO  HDB-TOTAL-FOR                    
01867                                  HDA-TOTAL-FOR.                   
01868      MOVE SPACES             TO  HDE                              
01869                                  HDD.                             
01870                                                                   
01871      IF  HDF-REIN-COMP GREATER THAN SPACES                        
01872          MOVE '       IN'    TO  HDF-TOTAL-FOR.                   
01873                                                                   
01874      PERFORM 5000-ROLL-RTN  THRU  5099-EXIT.                      
01875                                                                   
01876      MOVE REIN-SUB-TOTALS     TO  RPTCD2-TOTALS.                  
01877      MOVE REIN-SUB-M-TOTALS   TO  RPTCD2-M-TOTALS.                
01878                                                                   
01879      ADD ST-AGE-REMAIN   TO  RPTCD2-AGE-REMAIN.                   
01880      ADD ST-AH-EXP       TO  RPTCD2-AH-EXP.                       
01881      ADD ST-AVG-REM-TERM TO  RPTCD2-AVG-REM-TERM.                 
01882      ADD ST-AVG-TOT-TERM TO  RPTCD2-AVG-TOT-TERM.                 
01883                                                                   
01884      MOVE STATE-WORK  TO  CERT-WORK.                              
01885                                                                   
01886      ADD ST-DUP       TO  RPTCD2-DUP.                             
01887                                                                   
01888      MOVE ST-DUPS     TO  X-DUPS.                                 
01889                                                                   
01890      IF (DTE-PGM-OPT NOT GREATER 2) AND                           
01891         (CUR-REC-TYPE NOT = '1' AND '3')                          
01892          PERFORM 2000-BLD-RTN  THRU  2099-EXIT.                   
01893                                                                   
01894      MOVE WX-ST  TO  CUR-ST.                                      
01895                                                                   
01896      IF CUR-SEQ = WX-SEQ                                          
01897          GO TO 1310-INTL-STATE.                                   
01898  EJECT                                                            
01899  1235-RPTCD2-BREAK.                                               
01900                                                                   
01901      MOVE RPTCD2-TOTALS      TO  X-TOTALS                         
01902      MOVE RPTCD2-M-TOTALS    TO  X-M-TOTALS.                      
01903                                                                   
01904      MOVE CO-TOTALS          TO  REIN-SUB-TOTALS.                 
01905      MOVE CO-M-TOTALS        TO  REIN-SUB-M-TOTALS.               
01906                                                                   
01907      MOVE 'TOTAL FOR'        TO  HDB-TOTAL-FOR.                   
01908      MOVE '       IN'        TO  HDA-TOTAL-FOR.                   
01909      MOVE SPACES             TO  HDE                              
01910                                  HDD                              
01911                                  HDC.                             
01912                                                                   
01913      IF  HDF-REIN-COMP GREATER THAN SPACES                        
01914          MOVE '       IN'    TO  HDF-TOTAL-FOR.                   
01915                                                                   
01916      PERFORM 5000-ROLL-RTN  THRU  5099-EXIT.                      
01917                                                                   
01918      MOVE REIN-SUB-TOTALS    TO  CO-TOTALS.                       
01919      MOVE REIN-SUB-M-TOTALS  TO  CO-M-TOTALS.                     
01920                                                                   
01921      ADD RPTCD2-AGE-REMAIN   TO  CO-AGE-REMAIN.                   
01922      ADD RPTCD2-AH-EXP       TO  CO-AH-EXP.                       
01923      ADD RPTCD2-AVG-REM-TERM TO  CO-AVG-REM-TERM.                 
01924      ADD RPTCD2-AVG-TOT-TERM TO  CO-AVG-TOT-TERM.                 
01925                                                                   
01926      MOVE RPTCD2-WORK  TO  CERT-WORK.                             
01927                                                                   
01928      ADD RPTCD2-DUP    TO  CO-DUP.                                
01929                                                                   
01930      MOVE RPTCD2-DUPS  TO  X-DUPS.                                
01931                                                                   
01932      IF (DTE-PGM-OPT NOT GREATER 2) AND                           
01933         (CUR-REC-TYPE = '3')                                      
01934          PERFORM 2000-BLD-RTN  THRU  2099-EXIT.                   
01935                                                                   
01936      MOVE WX-RPT-CD2  TO  CUR-RPT-CD2.                            
01937                                                                   
01938      IF CUR-SEQ = WX-SEQ                                          
01939          GO TO 1305-INTL-RPTCD2.                                  
01940  EJECT                                                            
01941  1240-COMPANY-BREAK.                                              
01942                                                                   
01943      MOVE CO-TOTALS      TO  X-TOTALS.                            
01944      MOVE CO-M-TOTALS    TO  X-M-TOTALS.                          
01945                                                                   
01946      MOVE CARR-TOTALS    TO  REIN-SUB-TOTALS.                     
01947      MOVE CARR-M-TOTALS  TO  REIN-SUB-M-TOTALS.                   
01948                                                                   
01949      MOVE 'TOTAL FOR'        TO  HDB-TOTAL-FOR.                   
01950      MOVE '       IN'        TO  HDA-TOTAL-FOR.                   
01951      MOVE SPACES             TO  HDE                              
01952                                  HDD                              
01953                                  HDC.                             
01954                                                                   
01955      IF  HDF-REIN-COMP GREATER THAN SPACES                        
01956          MOVE '       IN'    TO  HDF-TOTAL-FOR.                   
01957                                                                   
01958      PERFORM 5000-ROLL-RTN  THRU  5099-EXIT.                      
01959                                                                   
01960      MOVE REIN-SUB-TOTALS    TO  CARR-TOTALS.                     
01961      MOVE REIN-SUB-M-TOTALS  TO  CARR-M-TOTALS.                   
01962                                                                   
01963      ADD CO-AGE-REMAIN   TO  CARR-AGE-REMAIN.                     
01964      ADD CO-AH-EXP       TO  CARR-AH-EXP.                         
01965      ADD CO-AVG-REM-TERM TO  CARR-AVG-REM-TERM.                   
01966      ADD CO-AVG-TOT-TERM TO  CARR-AVG-TOT-TERM.                   
01967                                                                   
01968      MOVE COMPANY-WORK   TO  CERT-WORK.                           
01969                                                                   
01970      ADD CO-DUP          TO  CARR-DUP.                            
01971                                                                   
01972      MOVE CO-DUPS        TO  X-DUPS.                              
01973                                                                   
01974      IF (DTE-PGM-OPT NOT GREATER 3) AND                           
01975         (CUR-REC-TYPE NOT = '1')                                  
01976          PERFORM 2000-BLD-RTN  THRU  2099-EXIT.                   
01977                                                                   
01978      MOVE WX-CO      TO CUR-CO.                                   
01979                                                                   
01980      IF WX-SEQ = CUR-SEQ                                          
01981          GO TO 1300-INTL-COMPANY.                                 
01982  EJECT                                                            
01983  1250-CARRIER-BREAK.                                              
01984                                                                   
01985      MOVE CARR-TOTALS     TO  X-TOTALS                            
01986      MOVE CARR-M-TOTALS   TO  X-M-TOTALS.                         
01987                                                                   
01988      MOVE RPTCD1-TOTALS   TO  REIN-SUB-TOTALS.                    
01989      MOVE RPTCD1-M-TOTALS TO  REIN-SUB-M-TOTALS.                  
01990                                                                   
01991      MOVE 'TOTAL FOR'        TO  HDA-TOTAL-FOR.                   
01992      MOVE SPACES             TO  HDE                              
01993                                  HDD                              
01994                                  HDC                              
01995                                  HDB.                             
01996                                                                   
01997      IF  HDF-REIN-COMP GREATER THAN SPACES                        
01998          MOVE '       IN'    TO  HDF-TOTAL-FOR.                   
01999                                                                   
02000      PERFORM 5000-ROLL-RTN  THRU  5099-EXIT.                      
02001                                                                   
02002      MOVE REIN-SUB-TOTALS    TO  RPTCD1-TOTALS.                   
02003      MOVE REIN-SUB-M-TOTALS  TO  RPTCD1-M-TOTALS.                 
02004                                                                   
02005      ADD CARR-AGE-REMAIN   TO  RPTCD1-AGE-REMAIN.                 
02006      ADD CARR-AH-EXP       TO  RPTCD1-AH-EXP.                     
02007      ADD CARR-AVG-REM-TERM TO  RPTCD1-AVG-REM-TERM.               
02008      ADD CARR-AVG-TOT-TERM TO  RPTCD1-AVG-TOT-TERM.               
02009                                                                   
02010      MOVE CARRIER-WORK  TO  CERT-WORK.                            
02011                                                                   
02012      ADD CARR-DUP       TO  RPTCD1-DUP.                           
02013                                                                   
02014      MOVE CARR-DUPS     TO  X-DUPS.                               
02015                                                                   
02016      IF (DTE-PGM-OPT NOT GREATER 4) AND                           
02017         (CUR-REC-TYPE NOT = '1')                                  
02018          PERFORM 2000-BLD-RTN  THRU  2099-EXIT.                   
02019                                                                   
02020      MOVE WX-CARR  TO  CUR-CARR.                                  
02021                                                                   
02022      IF CUR-SEQ = WX-SEQ                                          
02023          GO TO 1290-INTL-CARRIER.                                 
02024  EJECT                                                            
02025  1251-RPTCD1-BREAK.                                               
02026                                                                   
02027      MOVE RPTCD1-TOTALS      TO  X-TOTALS                         
02028      MOVE RPTCD1-M-TOTALS    TO  X-M-TOTALS.                      
02029                                                                   
02030      MOVE REIN-TOTALS        TO  REIN-SUB-TOTALS.                 
02031      MOVE REIN-M-TOTALS      TO  REIN-SUB-M-TOTALS.               
02032                                                                   
02033      MOVE 'TOTAL FOR'        TO  HDA-TOTAL-FOR.                   
02034      MOVE SPACES             TO  HDE                              
02035                                  HDD                              
02036                                  HDC                              
02037                                  HDB.                             
02038                                                                   
02039      IF  HDF-REIN-COMP GREATER THAN SPACES                        
02040          MOVE '       IN'    TO  HDF-TOTAL-FOR.                   
02041                                                                   
02042      PERFORM 5000-ROLL-RTN  THRU  5099-EXIT.                      
02043                                                                   
02044      MOVE REIN-SUB-TOTALS        TO  REIN-TOTALS.                 
02045      MOVE REIN-SUB-M-TOTALS      TO  REIN-M-TOTALS.               
02046                                                                   
02047      ADD RPTCD1-AGE-REMAIN   TO  REIN-AGE-REMAIN.                 
02048      ADD RPTCD1-AH-EXP       TO  REIN-AH-EXP.                     
02049      ADD RPTCD1-AVG-REM-TERM TO  REIN-AVG-REM-TERM.               
02050      ADD RPTCD1-AVG-TOT-TERM TO  REIN-AVG-TOT-TERM.               
02051                                                                   
02052      MOVE RPTCD1-WORK  TO  CERT-WORK.                             
02053                                                                   
02054      ADD RPTCD1-DUP       TO  REIN-DUP.                           
02055                                                                   
02056      MOVE RPTCD1-DUPS     TO  X-DUPS.                             
02057                                                                   
02058      IF (DTE-PGM-OPT NOT GREATER 4) AND                           
02059         (CUR-REC-TYPE = '1' OR '3')                               
02060          PERFORM 2000-BLD-RTN  THRU  2099-EXIT.                   
02061                                                                   
02062      IF CUR-REC-TYPE = '5'                                        
02063          MOVE WX1-RPT-CD1 TO  CUR1-RPT-CD1                        
02064        ELSE                                                       
02065          MOVE WX-RPT-CD1  TO  CUR-RPT-CD1.                        
02066                                                                   
02067      IF CUR-SEQ = WX-SEQ                                          
02068          GO TO 1288-INTL-RPTCD1.                                  
02069  EJECT                                                            
02070  1257-REIN-BREAK.                                                 
02071                                                                   
02072      MOVE REIN-TOTALS    TO  X-TOTALS                             
02073      MOVE REIN-M-TOTALS  TO  X-M-TOTALS.                          
02074                                                                   
02075      MOVE FINAL-TOTALS   TO  REIN-SUB-TOTALS.                     
02076      MOVE FINL-M-TOTALS  TO  REIN-SUB-M-TOTALS.                   
02077                                                                   
02078      MOVE SPACES             TO  HDE                              
02079                                  HDD                              
02080                                  HDC                              
02081                                  HDB                              
02082                                  HDA.                             
02083                                                                   
02084      IF HDF-REIN-COMP GREATER THAN SPACES                         
02085          MOVE 'TOTAL FOR'    TO  HDF-TOTAL-FOR                    
02086      ELSE                                                         
02087          MOVE SPACES         TO  HDH                              
02088                                  HDG                              
02089                                  HDF.                             
02090                                                                   
02091      PERFORM 5000-ROLL-RTN  THRU  5099-EXIT.                      
02092                                                                   
02093      MOVE REIN-SUB-TOTALS    TO  FINAL-TOTALS.                    
02094      MOVE REIN-SUB-M-TOTALS  TO  FINL-M-TOTALS.                   
02095                                                                   
02096      ADD REIN-AGE-REMAIN   TO  FINAL-AGE-REMAIN.                  
02097      ADD REIN-AH-EXP       TO  FINAL-AH-EXP.                      
02098      ADD REIN-AVG-REM-TERM TO  FINAL-AVG-REM-TERM.                
02099      ADD REIN-AVG-TOT-TERM TO  FINAL-AVG-TOT-TERM.                
02100                                                                   
02101      MOVE REIN-WORK  TO  CERT-WORK.                               
02102                                                                   
02103      ADD REIN-DUP    TO  FINL-DUP.                                
02104                                                                   
02105      MOVE REIN-DUPS  TO  X-DUPS.                                  
02106                                                                   
02107      IF (DTE-PGM-OPT NOT GREATER 5) AND                           
02108         (CUR-REC-TYPE = '4' OR '5')                               
02109          PERFORM 2000-BLD-RTN  THRU  2099-EXIT.                   
02110                                                                   
02111      IF CUR-REC-TYPE = '5'                                        
02112          MOVE WX1-RCO  TO  CUR1-RCO                               
02113        ELSE                                                       
02114          MOVE WX-RCO   TO  CUR-RCO.                               
02115                                                                   
02116      IF CUR-SEQ = WX-SEQ                                          
02117          GO TO 1283-INTL-REIN.                                    
02118  EJECT                                                            
02119  1260-FINAL-BREAK.                                                
02120                                                                   
02121      MOVE SPACES             TO HDF                               
02122                                 HDG                               
02123                                 HDH.                              
02124                                                                   
02125      MOVE ' FINAL TOTALS'    TO HDD.                              
02126                                                                   
02127      MOVE FINAL-TOTALS   TO  X-TOTALS.                            
02128      MOVE FINL-M-TOTALS  TO  X-M-TOTALS.                          
02129      MOVE FINAL-WORK     TO  CERT-WORK.                           
02130      MOVE FINL-DUPS      TO  X-DUPS.                              
02131                                                                   
02132      IF DTE-PGM-OPT NOT GREATER 5                                 
02133          PERFORM 2000-BLD-RTN  THRU  2099-EXIT.                   
02134                                                                   
02135      IF DTE-PGM-OPT  = 6  AND                                     
02136         CUR-REC-TYPE = '4' OR '5'                                 
02137          PERFORM 2000-BLD-RTN  THRU  2099-EXIT.                   
02138                                                                   
02139      MOVE WX-REC-TYPE TO CUR-REC-TYPE.                            
02140                                                                   
02141      IF CUR-REC-TYPE = '1'                                        
02142          MOVE 'B'         TO  HD1A.                               
02143                                                                   
02144      IF CUR-REC-TYPE = '2'                                        
02145          MOVE 'C'         TO  HD1A.                               
02146                                                                   
02147      IF CUR-REC-TYPE = '3'                                        
02148          MOVE 'D'         TO  HD1A.                               
02149                                                                   
02150      IF CUR-REC-TYPE = '4'                                        
02151          MOVE +2          TO  PASS-SW                             
02152          MOVE 'R'         TO  HD1A.                               
02153                                                                   
02154      IF CUR-REC-TYPE = '5'                                        
02155          MOVE +2          TO  PASS-SW                             
02156          MOVE 'S'         TO  HD1A.                               
02157                                                                   
02158  EJECT                                                            
02159  1280-INTL-FINAL.                                                 
02160      IF FST-SW = +2                                               
02161          GO TO 1399-EXIT.                                         
02162                                                                   
02163      MOVE WX-REC-TYPE TO CUR-REC-TYPE.                            
02164                                                                   
02165      PERFORM 3000-ZERO-RTN  THRU  3099-EXIT.                      
02166                                                                   
02167      MOVE X-TOTALS    TO  FINAL-TOTALS.                           
02168      MOVE X-M-TOTALS  TO  FINL-M-TOTALS.                          
02169      MOVE +0          TO  FINAL-AGE-REMAIN                        
02170                           FINAL-AVG-REM-TERM                      
02171                           FINAL-AVG-TOT-TERM                      
02172                           FINAL-AH-EXP                            
02173                           FINL-DUP.                               
02174                                                                   
02175  1283-INTL-REIN.                                                  
02176      MOVE WX-RCO    TO  CUR-RCO.                                  
02177                                                                   
02178      IF ZERO-SW NOT = +0                                          
02179          PERFORM 3000-ZERO-RTN  THRU  3099-EXIT.                  
02180                                                                   
02181      MOVE X-TOTALS     TO  REIN-TOTALS.                           
02182      MOVE X-M-TOTALS   TO  REIN-M-TOTALS.                         
02183      MOVE +0           TO  REIN-AGE-REMAIN                        
02184                            REIN-AVG-REM-TERM                      
02185                            REIN-AVG-TOT-TERM                      
02186                            REIN-AH-EXP                            
02187                            REIN-DUP.                              
02188                                                                   
02189  1288-INTL-RPTCD1.                                                
02190      MOVE WX-RPT-CD1 TO  CUR-RPT-CD1.                             
02191                                                                   
02192      IF ZERO-SW NOT = +0                                          
02193          PERFORM 3000-ZERO-RTN  THRU  3099-EXIT.                  
02194                                                                   
02195      MOVE X-TOTALS    TO  RPTCD1-TOTALS.                          
02196      MOVE X-M-TOTALS  TO  RPTCD1-M-TOTALS.                        
02197      MOVE +0          TO  RPTCD1-AGE-REMAIN                       
02198                           RPTCD1-AVG-REM-TERM                     
02199                           RPTCD1-AVG-TOT-TERM                     
02200                           RPTCD1-AH-EXP                           
02201                           RPTCD1-DUP.                             
02202                                                                   
02203  1290-INTL-CARRIER.                                               
02204      MOVE WX-CARR  TO  CUR-CARR.                                  
02205                                                                   
02206      IF ZERO-SW NOT = +0                                          
02207          PERFORM 3000-ZERO-RTN  THRU  3099-EXIT.                  
02208                                                                   
02209      MOVE X-TOTALS    TO  CARR-TOTALS.                            
02210      MOVE X-M-TOTALS  TO  CARR-M-TOTALS.                          
02211      MOVE +0          TO  CARR-AGE-REMAIN                         
02212                           CARR-AVG-REM-TERM                       
02213                           CARR-AVG-TOT-TERM                       
02214                           CARR-AH-EXP                             
02215                           CARR-DUP.                               
02216                                                                   
02217      MOVE CLAS-STARTCN       TO  CLAS-INDEXCN.                    
02218      MOVE 'INVALID CARRIER'  TO  WS-SAVE-CARR-NAME.               
02219                                                                   
02220  1295-FIND-CARR-DESC.                                             
02221      IF CLAS-INDEXCN GREATER CLAS-MAXCN                           
02222          GO TO 1300-INTL-COMPANY.                                 
02223                                                                   
02224      IF CARRIER-SUB (CLAS-INDEXCN) NOT = CUR-CARR                 
02225          ADD +1  TO  CLAS-INDEXCN                                 
02226          GO TO 1295-FIND-CARR-DESC.                               
02227                                                                   
02228      MOVE CARRIER-PIC (CLAS-INDEXCN)  TO  WS-SAVE-CARR-NAME.      
02229                                                                   
02230  1300-INTL-COMPANY.                                               
02231      MOVE WX-CO  TO  CUR-CO.                                      
02232                                                                   
02233      IF ZERO-SW NOT = +0                                          
02234          PERFORM 3000-ZERO-RTN  THRU  3099-EXIT.                  
02235                                                                   
02236      MOVE X-TOTALS    TO  CO-TOTALS.                              
02237      MOVE X-M-TOTALS  TO  CO-M-TOTALS.                            
02238      MOVE +0          TO  CO-AGE-REMAIN                           
02239                           CO-AVG-REM-TERM                         
02240                           CO-AVG-TOT-TERM                         
02241                           CO-AH-EXP                               
02242                           CO-DUP.                                 
02243                                                                   
02244  1305-INTL-RPTCD2.                                                
02245      MOVE WX-RPT-CD2 TO  CUR-RPT-CD2.                             
02246                                                                   
02247      IF ZERO-SW NOT = +0                                          
02248          PERFORM 3000-ZERO-RTN  THRU  3099-EXIT.                  
02249                                                                   
02250      MOVE X-TOTALS    TO  RPTCD2-TOTALS.                          
02251      MOVE X-M-TOTALS  TO  RPTCD2-M-TOTALS.                        
02252      MOVE +0          TO  RPTCD2-AGE-REMAIN                       
02253                           RPTCD2-AVG-REM-TERM                     
02254                           RPTCD2-AVG-TOT-TERM                     
02255                           RPTCD2-AH-EXP                           
02256                           RPTCD2-DUP.                             
02257                                                                   
02258  1310-INTL-STATE.                                                 
02259      MOVE WX-ST  TO  CUR-ST                                       
02260                      STATE-L.                                     
02261                                                                   
02262      IF ZERO-SW NOT = +0                                          
02263          PERFORM 3000-ZERO-RTN  THRU  3099-EXIT.                  
02264                                                                   
02265      MOVE X-TOTALS    TO  ST-TOTALS.                              
02266      MOVE X-M-TOTALS  TO  ST-M-TOTALS.                            
02267      MOVE +0          TO  ST-AGE-REMAIN                           
02268                           ST-AVG-REM-TERM                         
02269                           ST-AVG-TOT-TERM                         
02270                           ST-AH-EXP                               
02271                           ST-DUP.                                 
02272                                                                   
02273      MOVE CLAS-STARTS      TO  CLAS-INDEXS.                       
02274      MOVE 'INVALID STATE'  TO  WS-SAVE-ST-NAME.                   
02275                                                                   
02276  1320-FIND-ST-DESC.                                               
02277      IF CLAS-INDEXS GREATER CLAS-MAXS                             
02278          GO TO 1330-INTL-ACCOUNT.                                 
02279                                                                   
02280      IF STATE-SUB (CLAS-INDEXS) NOT = STATE-L                     
02281          ADD +1  TO  CLAS-INDEXS                                  
02282          GO TO 1320-FIND-ST-DESC.                                 
02283                                                                   
02284      MOVE STATE-PIC (CLAS-INDEXS)  TO  WS-SAVE-ST-NAME.           
02285                                                                   
02286  1330-INTL-ACCOUNT.                                               
02287      MOVE WX-ACCT  TO  CUR-ACCT.                                  
02288                                                                   
02289      IF ZERO-SW NOT = +0                                          
02290          PERFORM 3000-ZERO-RTN  THRU  3099-EXIT.                  
02291                                                                   
02292      MOVE X-TOTALS    TO  ACCT-TOTALS.                            
02293      MOVE X-M-TOTALS  TO  ACCT-M-TOTALS.                          
02294      MOVE +0          TO  ACCT-AGE-REMAIN                         
02295                           ACCT-AH-EXP                             
02296                           ACCT-AVG-REM-TERM                       
02297                           ACCT-AVG-TOT-TERM                       
02298                           AC-DUP.                                 
02299                                                                   
02300  1335-INTL-REINSUB.                                               
02301      MOVE WX-RCOSUB TO  CUR-RCOSUB.                               
02302                                                                   
02303      IF ZERO-SW NOT = +0                                          
02304          PERFORM 3000-ZERO-RTN  THRU  3099-EXIT.                  
02305                                                                   
02306      MOVE X-TOTALS    TO  REIN-SUB-TOTALS.                        
02307      MOVE X-M-TOTALS  TO  REIN-SUB-M-TOTALS.                      
02308      MOVE +0          TO  REIN-SUB-AGE-REMAIN                     
02309                           REIN-SUB-AVG-REM-TERM                   
02310                           REIN-SUB-AVG-TOT-TERM                   
02311                           REIN-SUB-AH-EXP                         
02312                           REIN-SUB-DUP.                           
02313                                                                   
02314  1340-INTL-DATE.                                                  
02315      MOVE WX-DATE  TO  CUR-DATE.                                  
02316                                                                   
02317      IF ZERO-SW NOT = +0                                          
02318          PERFORM 3000-ZERO-RTN  THRU  3099-EXIT.                  
02319                                                                   
02320      MOVE +0         TO  DATE-AGE-REMAIN                          
02321                          DATE-AH-EXP                              
02322                          DATE-AVG-REM-TERM                        
02323                          DATE-AVG-TOT-TERM                        
02324                          X-DUP.                                   
02325      MOVE +1         TO  ZERO-SW.                                 
02326      MOVE +66        TO  LNCTR.                                   
02327      MOVE LOW-VALUE  TO  PRE-TAB.                                 
02328                                                                   
02329  1399-EXIT.                                                       
02330      EXIT.                                                        
02331  EJECT                                                            
02332  2000-BLD-RTN.                                                    
02333      PERFORM 7000-HD-RTN  THRU  7099-EXIT.                        
02334                                                                   
02335      MOVE +0  TO  X-WT-REMAIN.                                    
02336                                                                   
02337      IF CUR-REC-TYPE NOT = '4' AND '5'                            
02338          MOVE 'GROSS'        TO  HD-DESC                          
02339      ELSE                                                         
02340          MOVE 'REINSURANCE'  TO  HD-DESC.                         
02341                                                                   
02342      MOVE '-'                TO  P-CCSW.                          
02343                                                                   
02344      PERFORM 2100-PRT-EXTRACT  THRU  2199-EXIT.                   
02345                                                                   
02346      PERFORM 2200-PRT-EXTRACT  THRU  2299-EXIT.                   
02347      PERFORM 2800-PRT-EXTRACT  THRU  2899-EXIT.                   
02348                                                                   
02349      IF DTE-CLIENT = 'NCL'                                        
02350          PERFORM 2600-PRT-EXTRACT  THRU  2699-EXIT.               
02351                                                                   
02352      IF X-WT-REMAIN = +0 AND                                      
02353         X-AH-EXP    = +0                                          
02354          GO TO 2090-BLD-MORT.                                     
02355                                                                   
02356      MOVE X-AH-EXP         TO  P-AH-EXP.                          
02357      MOVE X-WT-REMAIN      TO  P-WT-REMAIN.                       
02358      MOVE X-COMP-AVG-TERM  TO  P-AVG-REM-TERM.                    
02359      MOVE PRT-SPECIAL      TO  P-LN.                              
02360      MOVE '-'              TO  P-CCSW.                            
02361      PERFORM 8000-PRT-RTN  THRU  8099-EXIT.                       
02362                                                                   
02363      MOVE X-COMP-TOT-TERM  TO  P-AVG-TOT-TERM.                    
02364      MOVE PRT-SPECIAL-2    TO  P-LN.                              
02365      MOVE ' '              TO  P-CCSW.                            
02366      PERFORM 8000-PRT-RTN  THRU  8099-EXIT.                       
02367                                                                   
02368  2090-BLD-MORT.                                                   
02369      PERFORM 2300-PRT-MORT-RTN  THRU  2399-EXIT.                  
02370                                                                   
02371  2099-EXIT.                                                       
02372      EXIT.                                                        
02373  EJECT                                                            
02374  2100-PRT-EXTRACT.                                                
02375      MOVE +0  TO  LIFE-SW   AH-SW       HD-SW                     
02376                   S-COUNT   S-WRITTEN   S-P78       S-PRATA       
02377                   S-REMAIN  S-RESERV    S-STATE     S-DOMICILE    
02378                   S-ALTRSV.                                       
02379      MOVE +1  TO  X2.                                             
02380                                                                   
02381  2110-LOOP-LIFE.                                                  
02382      IF X2 GREATER MAX-BEN                                        
02383          GO TO 2120-OUT-LIFE.                                     
02384                                                                   
02385      IF X-TYP (X2) NOT = 1                                        
02386          GO TO 2120-OUT-LIFE.                                     
02387                                                                   
02388      MOVE X-AMTS (X2)  TO  X-DETL.                                
02389                                                                   
02390      IF X-COUNT = +0                                              
02391          ADD +1  TO  X2                                           
02392          GO TO 2110-LOOP-LIFE.                                    
02393                                                                   
CIDMOD*    IF LNCTR GREATER +58                                         
CIDMOD     IF LNCTR GREATER +50                                         
02395          MOVE +0  TO  HD-SW                                       
02396          PERFORM 7000-HD-RTN  THRU  7099-EXIT.                    
02397                                                                   
02398      IF HD-SW = +0                                                
02399          MOVE +1       TO  LIFE-SW  HD-SW                         
02400          MOVE HD4      TO  P-LN                                   
02401          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
02402          MOVE HD5      TO  P-LN                                   
02403          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
02404          MOVE HD6      TO  P-LN                                   
02405          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
02406          MOVE '0'      TO  P-CCSW.                                
02407                                                                   
02408      MOVE X-BEN (X2)   TO  PX-BEN.                                
02409      MOVE X-DESC (X2)  TO  PX-DESC.                               
02410                                                                   
02411      ADD X-COUNT    TO  S-COUNT.                                  
02412      ADD X-WRITTEN  TO  S-WRITTEN.                                
02413      ADD X-P78      TO  S-P78.                                    
02414      ADD X-PRATA    TO  S-PRATA.                                  
02415      ADD X-DOMICILE TO  S-DOMICILE.                               
02416      ADD X-STATE    TO  S-STATE.                                  
02417      ADD X-RESERV   TO  S-RESERV.                                 
02418      ADD X-ALTRSV   TO  S-ALTRSV.                                 
02419      ADD X-REMAIN   TO  S-REMAIN.                                 
02420                                                                   
02421      PERFORM 2400-PRT-LINE  THRU  2499-EXIT.                      
02422                                                                   
02423      ADD +1  TO  X2.                                              
02424                                                                   
02425      GO TO 2110-LOOP-LIFE.                                        
02426  EJECT                                                            
02427  2120-OUT-LIFE.                                                   
02428      MOVE SUB-TOTALS  TO  TOTALS.                                 
02429                                                                   
02430      IF LIFE-SW = 0                                               
02431          GO TO 2140-LOOP-AH.                                      
02432                                                                   
02433      MOVE 'TOTAL'       TO  PX-DESC.                              
02434      MOVE LIFE-OVERRIDE-L2 TO PX-BEN-DESC.                        
02435      MOVE SPACES        TO  PX-BEN.                               
02436      MOVE SUB-TOTALS    TO  X-DETL.                               
02437                                                                   
02438      PERFORM 2400-PRT-LINE  THRU  2499-EXIT.                      
02439                                                                   
02440      MOVE '0'      TO  P-CCSW.                                    
02441                                                                   
02442      IF CUR-REC-TYPE = '4' OR '5'                                 
02443          GO TO 2130-ZERO-SUBTOTAL.                                
02444                                                                   
02445      MOVE ZEROS  TO  X-WT-REMAIN.                                 
02446                                                                   
02447      IF S-REMAIN NOT = ZEROS                                      
02448          COMPUTE X-WT-REMAIN ROUNDED = X-CALC-REMAIN / S-REMAIN.  
02449                                                                   
02450  2130-ZERO-SUBTOTAL.                                              
02451      MOVE +0  TO  S-COUNT     S-WRITTEN  S-P78     S-PRATA        
02452                   S-DOMICILE  S-STATE    S-RESERV  S-ALTRSV       
02453                   S-REMAIN.                                       
02454  EJECT                                                            
02455  2140-LOOP-AH.                                                    
02456      IF X2 GREATER MAX-BEN                                        
02457          GO TO 2150-OUT-AH.                                       
02458                                                                   
02459      IF X-TYP (X2) NOT = 2                                        
02460          GO TO 2140-LOOP-AH.                                      
02461                                                                   
02462      MOVE X-AMTS (X2)  TO  X-DETL.                                
02463                                                                   
02464      IF X-COUNT = +0                                              
02465          ADD +1  TO  X2                                           
02466          GO TO 2140-LOOP-AH.                                      
02467                                                                   
CIDMOD*    IF LNCTR GREATER +58                                         
CIDMOD     IF LNCTR GREATER +50                                         
02469          MOVE +0  TO  HD-SW                                       
02470          PERFORM 7000-HD-RTN  THRU  7099-EXIT.                    
02471                                                                   
02472      IF HD-SW  = +0                                               
02473          MOVE +1       TO  HD-SW                                  
02474          MOVE HD4      TO  P-LN                                   
02475          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
02476          MOVE HD5      TO  P-LN                                   
02477          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
02478          MOVE HD6      TO  P-LN                                   
02479          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
02480          MOVE '0'      TO  P-CCSW.                                
02481                                                                   
02482      MOVE +1           TO  AH-SW.                                 
02483      MOVE X-BEN (X2)   TO  PX-BEN.                                
02484      MOVE X-DESC (X2)  TO  PX-DESC.                               
02485                                                                   
02486      ADD X-COUNT    TO  S-COUNT.                                  
02487      ADD X-WRITTEN  TO  S-WRITTEN.                                
02488      ADD X-P78      TO  S-P78.                                    
02489      ADD X-PRATA    TO  S-PRATA.                                  
02490      ADD X-DOMICILE TO  S-DOMICILE.                               
02491      ADD X-STATE    TO  S-STATE.                                  
02492      ADD X-RESERV   TO  S-RESERV.                                 
02493      ADD X-ALTRSV   TO  S-ALTRSV.                                 
02494      ADD X-REMAIN   TO  S-REMAIN.                                 
02495                                                                   
02496      PERFORM 2400-PRT-LINE  THRU  2499-EXIT.                      
02497                                                                   
02498      ADD +1  TO  X2.                                              
02499                                                                   
02500      GO TO 2140-LOOP-AH.                                          
02501  EJECT                                                            
02502  2150-OUT-AH.                                                     
02503      IF AH-SW = +0                                                
02504          GO TO 2160-PRT-TOTALS.                                   
02505                                                                   
02506      MOVE 'TOTAL'      TO  PX-DESC.                               
02507      MOVE AH-OVERRIDE-L2 TO PX-BEN-DESC.                          
02508      MOVE SPACES       TO  PX-BEN.                                
02509      MOVE SUB-TOTALS   TO  X-DETL.                                
02510                                                                   
02511      PERFORM 2400-PRT-LINE  THRU  2499-EXIT.                      
02512                                                                   
02513      MOVE '0'      TO  P-CCSW.                                    
02514                                                                   
02515      ADD S-COUNT    TO  T-COUNT.                                  
02516      ADD S-WRITTEN  TO  T-WRITTEN.                                
02517      ADD S-P78      TO  T-P78.                                    
02518      ADD S-PRATA    TO  T-PRATA.                                  
02519      ADD S-DOMICILE TO  T-DOMICILE.                               
02520      ADD S-STATE    TO  T-STATE.                                  
02521      ADD S-RESERV   TO  T-RESERV.                                 
02522      ADD S-ALTRSV   TO  T-ALTRSV.                                 
02523      ADD S-REMAIN   TO  T-REMAIN.                                 
02524                                                                   
02525  2160-PRT-TOTALS.                                                 
02526 *    IF LIFE-SW = 0                                               
02527 *        GO TO 2190-PRE-EXIT.                                     
02528                                                                   
02529      MOVE TOTALS  TO  X-DETL.                                     
02530                                                                   
02531      SUBTRACT X-DUP       FROM  X-COUNT.                          
02532                                                                   
02533      MOVE '    TOTAL'  TO  PX-DESC.                               
02534      MOVE SPACES       TO  PX-BEN.                                
02535                                                                   
02536      PERFORM 2400-PRT-LINE  THRU  2499-EXIT.                      
02537                                                                   
02538  2190-PRE-EXIT.                                                   
02539      MOVE ZERO         TO X-COMP-AVG-TERM.                        
02540      IF X-COUNT NOT = ZERO                                        
02541          COMPUTE X-COMP-AVG-TERM = X-AVG-REM-TERM / X-COUNT.      
02542                                                                   
02543      MOVE ZERO         TO X-COMP-TOT-TERM.                        
02544      IF X-COUNT NOT = ZERO                                        
02545          COMPUTE X-COMP-TOT-TERM = X-AVG-TOT-TERM / X-COUNT.      
02546                                                                   
02547  2199-EXIT.                                                       
02548      EXIT.                                                        
02549  EJECT                                                            
02550  2200-PRT-EXTRACT.                                                
02551      MOVE +0  TO  LIFE-SW   AH-SW                                 
02552                   S-PAID    S-C78    S-CRATA    S-CDOMI           
02553                   S-TAX     S-T78    S-TRATA    S-TDOMI.          
02554      MOVE +1  TO  X2.                                             
02555                                                                   
02556  2210-LOOP-LIFE.                                                  
02557      IF X2 GREATER MAX-BEN                                        
02558          GO TO 2220-OUT-LIFE.                                     
02559                                                                   
02560      IF X-TYP (X2) NOT = 1                                        
02561          GO TO 2220-OUT-LIFE.                                     
02562                                                                   
02563      MOVE X-AMTS (X2)  TO  X-DETL.                                
02564                                                                   
02565      IF X-COUNT = +0                                              
02566          ADD +1  TO  X2                                           
02567          GO TO 2210-LOOP-LIFE.                                    
02568                                                                   
02569      IF LIFE-SW = +0                                              
02570          MOVE +1       TO  LIFE-SW                                
02571          MOVE HD7      TO  P-LN                                   
02572          MOVE '-'      TO  P-CCSW                                 
02573          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
02574          MOVE HD8      TO  P-LN                                   
02575          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
02576          MOVE '0'      TO  P-CCSW.                                
02577                                                                   
02578      MOVE X-BEN (X2)   TO  PX-BEN.                                
02579      MOVE X-DESC (X2)  TO  PX-DESC.                               
02580                                                                   
02581      ADD X-PAID     TO  S-PAID.                                   
02582      ADD X-C78      TO  S-C78.                                    
02583      ADD X-CRATA    TO  S-CRATA.                                  
02584      ADD X-CDOMI    TO  S-CDOMI.                                  
02585      ADD X-TAX      TO  S-TAX.                                    
02586      ADD X-T78      TO  S-T78.                                    
02587      ADD X-TRATA    TO  S-TRATA.                                  
02588      ADD X-TDOMI    TO  S-TDOMI.                                  
02589                                                                   
02590      PERFORM 2500-PRT-LINE  THRU  2599-EXIT.                      
02591                                                                   
02592      ADD +1  TO  X2.                                              
02593                                                                   
02594      GO TO 2210-LOOP-LIFE.                                        
02595  EJECT                                                            
02596  2220-OUT-LIFE.                                                   
02597      MOVE SUB-TOTALS  TO  TOTALS.                                 
02598                                                                   
02599      IF LIFE-SW = 0                                               
02600          GO TO 2240-LOOP-AH.                                      
02601                                                                   
02602      MOVE 'TOTAL'       TO  PX-DESC.                              
02603      MOVE LIFE-OVERRIDE-L2 TO PX-BEN-DESC.                        
02604      MOVE SPACES        TO  PX-BEN.                               
02605      MOVE SUB-TOTALS    TO  X-DETL.                               
02606                                                                   
02607      PERFORM 2500-PRT-LINE  THRU  2599-EXIT.                      
02608                                                                   
02609      MOVE '0'      TO  P-CCSW.                                    
02610                                                                   
02611  2230-ZERO-SUBTOTAL.                                              
02612      MOVE +0  TO  S-PAID    S-C78    S-CRATA    S-CDOMI           
02613                   S-TAX     S-T78    S-TRATA    S-TDOMI.          
02614  EJECT                                                            
02615  2240-LOOP-AH.                                                    
02616      IF X2 GREATER MAX-BEN                                        
02617          GO TO 2250-OUT-AH.                                       
02618                                                                   
02619      IF X-TYP (X2) NOT = 2                                        
02620          GO TO 2240-LOOP-AH.                                      
02621                                                                   
02622      MOVE X-AMTS (X2)  TO  X-DETL.                                
02623                                                                   
02624      IF X-COUNT = +0                                              
02625          ADD +1  TO  X2                                           
02626          GO TO 2240-LOOP-AH.                                      
02627                                                                   
02628      IF AH-SW   = +0 AND                                          
02629         LIFE-SW = +0                                              
02630          MOVE HD7      TO  P-LN                                   
02631          MOVE '-'      TO  P-CCSW                                 
02632          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
02633          MOVE HD8      TO  P-LN                                   
02634          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
02635          MOVE '0'      TO  P-CCSW.                                
02636                                                                   
02637      MOVE +1           TO  AH-SW.                                 
02638      MOVE X-BEN (X2)   TO  PX-BEN.                                
02639      MOVE X-DESC (X2)  TO  PX-DESC.                               
02640                                                                   
02641      ADD X-PAID     TO  S-PAID.                                   
02642      ADD X-C78      TO  S-C78.                                    
02643      ADD X-CRATA    TO  S-CRATA.                                  
02644      ADD X-CDOMI    TO  S-CDOMI.                                  
02645      ADD X-TAX      TO  S-TAX.                                    
02646      ADD X-T78      TO  S-T78.                                    
02647      ADD X-TRATA    TO  S-TRATA.                                  
02648      ADD X-TDOMI    TO  S-TDOMI.                                  
02649                                                                   
02650      PERFORM 2500-PRT-LINE  THRU  2599-EXIT.                      
02651                                                                   
02652      ADD +1  TO  X2.                                              
02653                                                                   
02654      GO TO 2240-LOOP-AH.                                          
02655  EJECT                                                            
02656  2250-OUT-AH.                                                     
02657      IF AH-SW = +0                                                
02658          GO TO 2299-EXIT.                                         
02659                                                                   
02660      MOVE 'TOTAL'      TO  PX-DESC.                               
02661      MOVE AH-OVERRIDE-L2 TO PX-BEN-DESC.                          
02662      MOVE SPACES       TO  PX-BEN.                                
02663      MOVE SUB-TOTALS   TO  X-DETL.                                
02664                                                                   
02665      PERFORM 2500-PRT-LINE  THRU  2599-EXIT.                      
02666                                                                   
02667      MOVE '0'      TO  P-CCSW.                                    
02668                                                                   
02669      ADD S-PAID     TO  T-PAID.                                   
02670      ADD S-C78      TO  T-C78.                                    
02671      ADD S-CRATA    TO  T-CRATA.                                  
02672      ADD S-CDOMI    TO  T-CDOMI.                                  
02673      ADD S-TAX      TO  T-TAX.                                    
02674      ADD S-T78      TO  T-T78.                                    
02675      ADD S-TRATA    TO  T-TRATA.                                  
02676      ADD S-TDOMI    TO  T-TDOMI.                                  
02677                                                                   
02678  2260-PRT-TOTALS.                                                 
02679      IF LIFE-SW = 0                                               
02680          GO TO 2299-EXIT.                                         
02681                                                                   
02682      MOVE TOTALS  TO  X-DETL.                                     
02683                                                                   
02684      SUBTRACT X-DUP       FROM  X-COUNT.                          
02685                                                                   
02686      MOVE '    TOTAL'  TO  PX-DESC.                               
02687      MOVE SPACES       TO  PX-BEN.                                
02688                                                                   
02689      PERFORM 2500-PRT-LINE  THRU  2599-EXIT.                      
02690                                                                   
02691  2299-EXIT.                                                       
02692      EXIT.                                                        
02693  EJECT                                                            
02694  2300-PRT-MORT-RTN.                                               
02695      MOVE +0  TO  M1  HD-SW  T-IUNDR  T-IOVER  T-GUNDR  T-GOVER   
02696                              T-TOTAL.                             
02697                                                                   
02698  2310-PRT-M1.                                                     
02699      ADD +1  TO  M1.                                              
02700                                                                   
02701      IF M1 GREATER CLAS-MAXM                                      
02702          GO TO 2340-PRT-MORT-TOTAL.                               
02703                                                                   
02704      MOVE +0  TO  M2  TAB-SW  S-IUNDR  S-IOVER  S-GUNDR           
02705                               S-GOVER  S-TOTAL.                   
02706  EJECT                                                            
02707  2320-PRT-M2.                                                     
02708      ADD +1  TO  M2.                                              
02709                                                                   
02710      IF M2 GREATER MAX-MORT-LF-TYPES                              
02711          GO TO 2330-PRT-MORT-SUB.                                 
02712                                                                   
02713      MOVE XM-AMTS (M1 M2)  TO  XM-DETL.                           
02714                                                                   
02715      COMPUTE X-TOTAL = X-IUNDR + X-IOVER + X-GUNDR + X-GOVER.     
02716                                                                   
02717      IF X-TOTAL = +0                                              
02718          GO TO 2320-PRT-M2.                                       
02719                                                                   
CIDMOD*    IF LNCTR GREATER +58                                         
CIDMOD     IF LNCTR GREATER +50                                         
02721          MOVE +0  TO  HD-SW  TAB-SW                               
02722          PERFORM 7000-HD-RTN  THRU  7099-EXIT.                    
02723                                                                   
02724      IF HD-SW = +0                                                
02725          MOVE +1       TO  HD-SW                                  
02726          MOVE HD-4M    TO  P-LN                                   
02727          MOVE '-'      TO  P-CCSW                                 
02728          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
02729          MOVE HD-5M    TO  P-LN                                   
02730          PERFORM 8000-PRT-RTN  THRU  8099-EXIT.                   
02731                                                                   
02732      IF TAB-SW = +0                                               
02733          MOVE +1                   TO  TAB-SW                     
02734          MOVE '0'                  TO  P-CCSW                     
02735          MOVE CLAS-MORT-CODE (M1)  TO  PM-TABLE                   
02736          MOVE CLAS-MORT-DESC (M1)  TO  PM-TDESC.                  
02737                                                                   
02738      IF M2 = MAX-MORT-LF-TYPES                                    
02739          MOVE 'OTHERS'     TO  PM-BDESC                           
02740      ELSE                                                         
02741          MOVE X-BEN (M2)   TO  PM-BEN                             
02742          MOVE X-DESC (M2)  TO  PM-BDESC.                          
02743                                                                   
02744      MOVE X-IUNDR  TO  PM-IUNDR.                                  
02745      MOVE X-IOVER  TO  PM-IOVER.                                  
02746      MOVE X-GUNDR  TO  PM-GUNDR.                                  
02747      MOVE X-GOVER  TO  PM-GOVER.                                  
02748      MOVE X-TOTAL  TO  PM-TOTAL.                                  
02749                                                                   
02750      PERFORM 8000-PRT-RTN  THRU  8099-EXIT.                       
02751                                                                   
02752      ADD X-IUNDR  TO  S-IUNDR.                                    
02753      ADD X-IOVER  TO  S-IOVER.                                    
02754      ADD X-GUNDR  TO  S-GUNDR.                                    
02755      ADD X-GOVER  TO  S-GOVER.                                    
02756      ADD X-TOTAL  TO  S-TOTAL.                                    
02757                                                                   
02758      GO TO 2320-PRT-M2.                                           
02759  EJECT                                                            
02760  2330-PRT-MORT-SUB.                                               
02761      IF S-TOTAL = +0                                              
02762          GO TO 2310-PRT-M1.                                       
02763                                                                   
02764      MOVE 'TOTAL'  TO  PM-BDESC.                                  
02765      MOVE S-IUNDR  TO  PM-IUNDR.                                  
02766      MOVE S-IOVER  TO  PM-IOVER.                                  
02767      MOVE S-GUNDR  TO  PM-GUNDR.                                  
02768      MOVE S-GOVER  TO  PM-GOVER.                                  
02769      MOVE S-TOTAL  TO  PM-TOTAL.                                  
02770                                                                   
02771      PERFORM 8000-PRT-RTN  THRU  8099-EXIT.                       
02772                                                                   
02773      ADD S-IUNDR  TO  T-IUNDR.                                    
02774      ADD S-IOVER  TO  T-IOVER.                                    
02775      ADD S-GUNDR  TO  T-GUNDR.                                    
02776      ADD S-GOVER  TO  T-GOVER.                                    
02777      ADD S-TOTAL  TO  T-TOTAL.                                    
02778                                                                   
02779      GO TO 2310-PRT-M1.                                           
02780                                                                   
02781  2340-PRT-MORT-TOTAL.                                             
02782                                                                   
02783      IF T-TOTAL = +0                                              
02784          GO TO 2399-EXIT.                                         
02785                                                                   
02786      MOVE '0'      TO  P-CCSW.                                    
02787      MOVE 'TOTAL'  TO  PM-TDESC.                                  
02788      MOVE T-IUNDR  TO  PM-IUNDR.                                  
02789      MOVE T-IOVER  TO  PM-IOVER.                                  
02790      MOVE T-GUNDR  TO  PM-GUNDR.                                  
02791      MOVE T-GOVER  TO  PM-GOVER.                                  
02792      MOVE T-TOTAL  TO  PM-TOTAL.                                  
02793                                                                   
02794      PERFORM 8000-PRT-RTN  THRU  8099-EXIT.                       
02795                                                                   
02796  2399-EXIT.                                                       
02797      EXIT.                                                        
02798  EJECT                                                            
02799  2400-PRT-LINE.                                                   
CIDMOD*    IF LNCTR GREATER +58                                         
CIDMOD     IF LNCTR GREATER +50                                         
02801          PERFORM 7000-HD-RTN  THRU  7099-EXIT                     
02802          MOVE HD4      TO  P-LN                                   
02803          MOVE '-'      TO  P-CCSW                                 
02804          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
02805          MOVE HD5      TO  P-LN                                   
02806          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
02807          MOVE HD6      TO  P-LN                                   
02808          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
02809          MOVE '0'      TO  P-CCSW.                                
02810                                                                   
02811      MOVE PX-BEN     TO  P-BEN.                                   
02812      MOVE PX-DESC    TO  P-DESC.                                  
02813      MOVE X-COUNT    TO  P-COUNT.                                 
02814      MOVE X-WRITTEN  TO  P-WRITTEN.                               
02815      MOVE X-P78      TO  P-P78.                                   
02816      MOVE X-PRATA    TO  P-PRATA.                                 
02817      MOVE X-DOMICILE TO  P-PDOMICILE.                             
02818      MOVE X-STATE    TO  P-PSTATE.                                
02819      MOVE X-RESERV   TO  P-RESERV.                                
02820      MOVE X-ALTRSV   TO  P-ALTRSV.                                
02821      MOVE X-REMAIN   TO  P-REMAIN.                                
02822                                                                   
02823      PERFORM 8000-PRT-RTN  THRU  8099-EXIT.                       
02824                                                                   
02825  2499-EXIT.                                                       
02826      EXIT.                                                        
02827  EJECT                                                            
02828  2500-PRT-LINE.                                                   
CIDMOD*    IF LNCTR GREATER +58                                         
CIDMOD     IF LNCTR GREATER +50                                         
02830          PERFORM 7000-HD-RTN  THRU  7099-EXIT                     
02831          MOVE HD7      TO  P-LN                                   
02832          MOVE '-'      TO  P-CCSW                                 
02833          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
02834          MOVE HD8      TO  P-LN                                   
02835          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
02836          MOVE '0'      TO  P-CCSW.                                
02837                                                                   
02838      MOVE PX-BEN     TO  P-BEN.                                   
02839      MOVE PX-DESC    TO  P-DESC.                                  
02840      MOVE X-PAID     TO  P-PAID.                                  
02841      MOVE X-C78      TO  P-C78.                                   
02842      MOVE X-CRATA    TO  P-CRATA.                                 
02843      MOVE X-CDOMI    TO  P-CDOMI.                                 
02844      MOVE X-TAX      TO  P-TAX.                                   
02845      MOVE X-T78      TO  P-T78.                                   
02846      MOVE X-TRATA    TO  P-TRATA.                                 
02847      MOVE X-TDOMI    TO  P-TDOMI.                                 
02848                                                                   
02849      PERFORM 8000-PRT-RTN  THRU  8099-EXIT.                       
02850                                                                   
02851  2599-EXIT.                                                       
02852      EXIT.                                                        
02853  EJECT                                                            
02854  2600-PRT-EXTRACT.                                                
02855      MOVE +0  TO  LIFE-SW    AH-SW                                
02856                   S-WRITTEN  S-P78  S-PRATA  S-DOMICILE.          
02857      MOVE +1  TO  X2.                                             
02858                                                                   
02859  2610-LOOP-LIFE.                                                  
02860      IF X2 GREATER MAX-BEN                                        
02861          GO TO 2620-OUT-LIFE.                                     
02862                                                                   
02863      IF X-TYP (X2) NOT = 1                                        
02864          GO TO 2620-OUT-LIFE.                                     
02865                                                                   
02866      MOVE X-AMTS (X2)  TO  X-DETL.                                
02867                                                                   
02868      IF X-COUNT = +0                                              
02869          ADD +1  TO  X2                                           
02870          GO TO 2610-LOOP-LIFE.                                    
02871                                                                   
02872      IF LIFE-SW = +0                                              
02873          MOVE +1       TO  LIFE-SW                                
02874          MOVE HD9      TO  P-LN                                   
02875          MOVE '-'      TO  P-CCSW                                 
02876          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
02877          MOVE HD10     TO  P-LN                                   
02878          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
02879          MOVE '0'      TO  P-CCSW.                                
02880                                                                   
02881      MOVE X-BEN (X2)   TO  PX-BEN.                                
02882      MOVE X-DESC (X2)  TO  PX-DESC.                               
02883                                                                   
02884      COMPUTE ACQ-WRITTEN   ROUNDED = X-WRITTEN  * ACQ-PCT.        
02885      COMPUTE ACQ-P78       ROUNDED = X-P78      * ACQ-PCT.        
02886      COMPUTE ACQ-PRATA     ROUNDED = X-PRATA    * ACQ-PCT.        
02887      COMPUTE ACQ-DOMICILE  ROUNDED = X-DOMICILE * ACQ-PCT.        
02888                                                                   
02889      ADD  ACQ-WRITTEN    TO S-WRITTEN.                            
02890      ADD  ACQ-P78        TO S-P78.                                
02891      ADD  ACQ-PRATA      TO S-PRATA.                              
02892      ADD  ACQ-DOMICILE   TO S-DOMICILE.                           
02893                                                                   
02894      PERFORM 2700-PRT-LINE  THRU  2799-EXIT.                      
02895                                                                   
02896      ADD +1  TO  X2.                                              
02897                                                                   
02898      GO TO 2610-LOOP-LIFE.                                        
02899  EJECT                                                            
02900  2620-OUT-LIFE.                                                   
02901      MOVE SUB-TOTALS  TO  TOTALS.                                 
02902                                                                   
02903      IF LIFE-SW = 0                                               
02904          GO TO 2640-LOOP-AH.                                      
02905                                                                   
02906      MOVE 'TOTAL'       TO  PX-DESC.                              
02907      MOVE LIFE-OVERRIDE-L2 TO PX-BEN-DESC.                        
02908      MOVE SPACES        TO  PX-BEN.                               
02909      MOVE SUB-TOTALS    TO  X-DETL.                               
02910                                                                   
02911      MOVE X-WRITTEN     TO ACQ-WRITTEN.                           
02912      MOVE X-P78         TO ACQ-P78.                               
02913      MOVE X-PRATA       TO ACQ-PRATA.                             
02914      MOVE X-DOMICILE    TO ACQ-DOMICILE.                          
02915                                                                   
02916      PERFORM 2700-PRT-LINE  THRU  2799-EXIT.                      
02917                                                                   
02918      MOVE '0'      TO  P-CCSW.                                    
02919                                                                   
02920  2630-ZERO-SUBTOTAL.                                              
02921      MOVE +0  TO  S-WRITTEN  S-P78  S-PRATA  S-DOMICILE.          
02922  EJECT                                                            
02923  2640-LOOP-AH.                                                    
02924      IF X2 GREATER MAX-BEN                                        
02925          GO TO 2650-OUT-AH.                                       
02926                                                                   
02927      IF X-TYP (X2) NOT = 2                                        
02928          GO TO 2640-LOOP-AH.                                      
02929                                                                   
02930      MOVE X-AMTS (X2)  TO  X-DETL.                                
02931                                                                   
02932      IF X-COUNT = +0                                              
02933          ADD +1  TO  X2                                           
02934          GO TO 2640-LOOP-AH.                                      
02935                                                                   
02936      IF AH-SW   = +0 AND                                          
02937         LIFE-SW = +0                                              
02938          MOVE HD9      TO  P-LN                                   
02939          MOVE '-'      TO  P-CCSW                                 
02940          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
02941          MOVE HD10     TO  P-LN                                   
02942          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
02943          MOVE '0'      TO  P-CCSW.                                
02944                                                                   
02945      MOVE +1           TO  AH-SW.                                 
02946      MOVE X-BEN (X2)   TO  PX-BEN.                                
02947      MOVE X-DESC (X2)  TO  PX-DESC.                               
02948                                                                   
02949      COMPUTE ACQ-WRITTEN   ROUNDED = X-WRITTEN  * ACQ-PCT.        
02950      COMPUTE ACQ-P78       ROUNDED = X-P78      * ACQ-PCT.        
02951      COMPUTE ACQ-PRATA     ROUNDED = X-PRATA    * ACQ-PCT.        
02952      COMPUTE ACQ-DOMICILE  ROUNDED = X-DOMICILE * ACQ-PCT.        
02953                                                                   
02954      ADD  ACQ-WRITTEN    TO S-WRITTEN.                            
02955      ADD  ACQ-P78        TO S-P78.                                
02956      ADD  ACQ-PRATA      TO S-PRATA.                              
02957      ADD  ACQ-DOMICILE   TO S-DOMICILE.                           
02958                                                                   
02959      PERFORM 2700-PRT-LINE  THRU  2799-EXIT.                      
02960                                                                   
02961      ADD +1  TO  X2.                                              
02962                                                                   
02963      GO TO 2640-LOOP-AH.                                          
02964  EJECT                                                            
02965  2650-OUT-AH.                                                     
02966      IF AH-SW = +0                                                
02967          GO TO 2699-EXIT.                                         
02968                                                                   
02969      MOVE 'TOTAL'      TO  PX-DESC.                               
02970      MOVE AH-OVERRIDE-L2 TO PX-BEN-DESC.                          
02971      MOVE SPACES       TO  PX-BEN.                                
02972      MOVE SUB-TOTALS   TO  X-DETL.                                
02973                                                                   
02974      MOVE X-WRITTEN     TO ACQ-WRITTEN.                           
02975      MOVE X-P78         TO ACQ-P78.                               
02976      MOVE X-PRATA       TO ACQ-PRATA.                             
02977      MOVE X-DOMICILE    TO ACQ-DOMICILE.                          
02978                                                                   
02979      PERFORM 2700-PRT-LINE  THRU  2799-EXIT.                      
02980                                                                   
02981      MOVE '0'      TO  P-CCSW.                                    
02982                                                                   
02983      ADD S-WRITTEN  TO  T-WRITTEN.                                
02984      ADD S-P78      TO  T-P78.                                    
02985      ADD S-PRATA    TO  T-PRATA.                                  
02986      ADD S-DOMICILE TO  T-DOMICILE.                               
02987                                                                   
02988  2660-PRT-TOTALS.                                                 
02989      IF LIFE-SW = 0                                               
02990          GO TO 2699-EXIT.                                         
02991                                                                   
02992      MOVE TOTALS        TO X-DETL.                                
02993                                                                   
02994      MOVE X-WRITTEN     TO ACQ-WRITTEN.                           
02995      MOVE X-P78         TO ACQ-P78.                               
02996      MOVE X-PRATA       TO ACQ-PRATA.                             
02997      MOVE X-DOMICILE    TO ACQ-DOMICILE.                          
02998                                                                   
02999 *?*  SUBTRACT X-DUP       FROM  X-COUNT.                          
03000                                                                   
03001      MOVE '    TOTAL'  TO  PX-DESC.                               
03002      MOVE SPACES       TO  PX-BEN.                                
03003                                                                   
03004      PERFORM 2700-PRT-LINE  THRU  2799-EXIT.                      
03005                                                                   
03006  2699-EXIT.                                                       
03007      EXIT.                                                        
03008  EJECT                                                            
03009  2700-PRT-LINE.                                                   
CIDMOD*    IF LNCTR GREATER +58                                         
CIDMOD     IF LNCTR GREATER +50                                         
03011          PERFORM 7000-HD-RTN  THRU  7099-EXIT                     
03012          MOVE HD9      TO  P-LN                                   
03013          MOVE '-'      TO  P-CCSW                                 
03014          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
03015          MOVE HD10     TO  P-LN                                   
03016          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
03017          MOVE '0'      TO  P-CCSW.                                
03018                                                                   
03019      MOVE PX-BEN     TO  P-BEN.                                   
03020      MOVE PX-DESC    TO  P-DESC.                                  
03021                                                                   
03022      MOVE ACQ-WRITTEN            TO P-PAID.                       
03023      MOVE ACQ-P78                TO P-C78.                        
03024      MOVE ACQ-PRATA              TO P-CRATA.                      
03025      MOVE ACQ-DOMICILE           TO P-CDOMI.                      
03026                                                                   
03027      MOVE ZERO                   TO P-TAX.                        
03028      MOVE ZERO                   TO P-T78.                        
03029      MOVE ZERO                   TO P-TRATA.                      
03030      MOVE ZERO                   TO P-TDOMI.                      
03031                                                                   
03032      PERFORM 8000-PRT-RTN  THRU  8099-EXIT.                       
03033                                                                   
03034  2799-EXIT.                                                       
03035      EXIT.                                                        
03036  EJECT                                                            
03037  2800-PRT-EXTRACT.                                                
03038      MOVE +0                TO  LIFE-SW                           
03039                                 AH-SW.                            
03040      MOVE +0                TO  S-SFPAID                          
03041                                 S-SF78                            
03042                                 S-SFRATA                          
03043                                 S-SFDOMI.                         
03044                                                                   
03045      MOVE +1                TO  X2.                               
03046                                                                   
03047  2810-LOOP-LIFE.                                                  
03048      IF X2 GREATER MAX-BEN                                        
03049          GO TO 2820-OUT-LIFE.                                     
03050                                                                   
03051      IF X-TYP (X2) NOT = 1                                        
03052          GO TO 2820-OUT-LIFE.                                     
03053                                                                   
03054      MOVE X-AMTS (X2)       TO  X-DETL.                           
03055                                                                   
03056      IF (X-COUNT = +0 OR                                          
03057          X-SFPAID EQUAL ZERO)                                     
03058          ADD +1             TO  X2                                
03059          GO TO 2810-LOOP-LIFE.                                    
03060                                                                   
03061      IF LIFE-SW = +0                                              
03062          MOVE +1            TO  LIFE-SW                           
03063          MOVE HD11          TO  P-LN                              
03064          MOVE '-'           TO  P-CCSW                            
03065          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
03066          MOVE HD10          TO  P-LN                              
03067          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
03068          MOVE '0'           TO  P-CCSW.                           
03069                                                                   
03070      MOVE X-BEN (X2)        TO  PX-BEN.                           
03071      MOVE X-DESC (X2)       TO  PX-DESC.                          
03072                                                                   
03073      ADD X-SFPAID           TO  S-SFPAID.                         
03074      ADD X-SF78             TO  S-SF78.                           
03075      ADD X-SFRATA           TO  S-SFRATA.                         
03076      ADD X-SFDOMI           TO  S-SFDOMI.                         
03077                                                                   
03078      PERFORM 2900-PRT-LINE  THRU  2999-EXIT.                      
03079                                                                   
03080      ADD +1                 TO  X2.                               
03081                                                                   
03082      GO TO 2810-LOOP-LIFE.                                        
03083  EJECT                                                            
03084  2820-OUT-LIFE.                                                   
03085      MOVE SUB-TOTALS        TO  TOTALS.                           
03086                                                                   
03087      IF LIFE-SW = 0                                               
03088          GO TO 2840-LOOP-AH.                                      
03089                                                                   
03090      MOVE 'TOTAL'           TO  PX-DESC.                          
03091      MOVE LIFE-OVERRIDE-L2  TO  PX-BEN-DESC.                      
03092      MOVE SPACES            TO  PX-BEN.                           
03093      MOVE SUB-TOTALS        TO  X-DETL.                           
03094                                                                   
03095      PERFORM 2900-PRT-LINE  THRU  2999-EXIT.                      
03096                                                                   
03097      MOVE '0'               TO  P-CCSW.                           
03098                                                                   
03099  2830-ZERO-SUBTOTAL.                                              
03100      MOVE +0                TO  S-SFPAID                          
03101                                 S-SF78                            
03102                                 S-SFRATA                          
03103                                 S-SFDOMI.                         
03104                                                                   
03105  EJECT                                                            
03106  2840-LOOP-AH.                                                    
03107      IF X2 GREATER MAX-BEN                                        
03108          GO TO 2850-OUT-AH.                                       
03109                                                                   
03110      IF X-TYP (X2) NOT = 2                                        
03111          ADD +1             TO  X2                                
03112          GO TO 2840-LOOP-AH.                                      
03113                                                                   
03114      MOVE X-AMTS (X2)       TO  X-DETL.                           
03115                                                                   
03116      IF (X-COUNT = +0 OR                                          
03117          X-SFPAID EQUAL ZERO)                                     
03118          ADD +1             TO  X2                                
03119          GO TO 2840-LOOP-AH.                                      
03120                                                                   
03121      IF AH-SW   = +0 AND                                          
03122         LIFE-SW = +0                                              
03123          MOVE HD11          TO  P-LN                              
03124          MOVE '-'           TO  P-CCSW                            
03125          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
03126          MOVE HD10          TO  P-LN                              
03127          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
03128          MOVE '0'           TO  P-CCSW.                           
03129                                                                   
03130      MOVE +1                TO  AH-SW.                            
03131      MOVE X-BEN (X2)        TO  PX-BEN.                           
03132      MOVE X-DESC (X2)       TO  PX-DESC.                          
03133                                                                   
03134      ADD X-SFPAID           TO  S-SFPAID.                         
03135      ADD X-SF78             TO  S-SF78.                           
03136      ADD X-SFRATA           TO  S-SFRATA.                         
03137      ADD X-SFDOMI           TO  S-SFDOMI.                         
03138                                                                   
03139      PERFORM 2900-PRT-LINE  THRU  2999-EXIT.                      
03140                                                                   
03141      ADD +1                 TO  X2.                               
03142                                                                   
03143      GO TO 2840-LOOP-AH.                                          
03144  EJECT                                                            
03145  2850-OUT-AH.                                                     
03146      IF AH-SW = +0                                                
03147          GO TO 2899-EXIT.                                         
03148                                                                   
03149      MOVE 'TOTAL'          TO  PX-DESC.                           
03150      MOVE AH-OVERRIDE-L2   TO  PX-BEN-DESC.                       
03151      MOVE SPACES           TO  PX-BEN.                            
03152      MOVE SUB-TOTALS       TO  X-DETL.                            
03153                                                                   
03154      PERFORM 2900-PRT-LINE  THRU  2999-EXIT.                      
03155                                                                   
03156      MOVE '0'              TO  P-CCSW.                            
03157                                                                   
03158      ADD S-SFPAID          TO  T-SFPAID.                          
03159      ADD S-SF78            TO  T-SF78.                            
03160      ADD S-SFRATA          TO  T-SFRATA.                          
03161      ADD S-SFDOMI          TO  T-SFDOMI.                          
03162                                                                   
03163  2860-PRT-TOTALS.                                                 
03164      IF LIFE-SW = 0                                               
03165          GO TO 2899-EXIT.                                         
03166                                                                   
03167      MOVE TOTALS           TO  X-DETL.                            
03168                                                                   
03169      SUBTRACT X-DUP       FROM  X-COUNT.                          
03170                                                                   
03171      MOVE '    TOTAL'      TO  PX-DESC.                           
03172      MOVE SPACES           TO  PX-BEN.                            
03173                                                                   
03174      PERFORM 2900-PRT-LINE  THRU  2999-EXIT.                      
03175                                                                   
03176  2899-EXIT.                                                       
03177      EXIT.                                                        
03178  EJECT                                                            
03179  2900-PRT-LINE.                                                   
CIDMOD*    IF LNCTR GREATER +58                                         
CIDMOD     IF LNCTR GREATER +50                                         
03181          PERFORM 7000-HD-RTN  THRU  7099-EXIT                     
03182          MOVE HD11     TO  P-LN                                   
03183          MOVE '-'      TO  P-CCSW                                 
03184          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
03185          MOVE HD10     TO  P-LN                                   
03186          PERFORM 8000-PRT-RTN  THRU  8099-EXIT                    
03187          MOVE '0'      TO  P-CCSW.                                
03188                                                                   
03189      MOVE PX-BEN     TO  P-BEN.                                   
03190      MOVE PX-DESC    TO  P-DESC.                                  
03191      MOVE X-SFPAID   TO  P-SFPAID.                                
03192      MOVE X-SF78     TO  P-SF78                                   
03193      MOVE X-SFRATA   TO  P-SFRATA.                                
03194      MOVE X-SFDOMI   TO  P-SFDOMI.                                
03195                                                                   
03196      PERFORM 8000-PRT-RTN  THRU  8099-EXIT.                       
03197                                                                   
03198  2999-EXIT.                                                       
03199      EXIT.                                                        
03200  EJECT                                                            
03201  EJECT                                                            
03202  3000-ZERO-RTN.                                                   
03203      MOVE +0  TO  X-COUNT     X-WRITTEN   X-P78       X-PRATA     
03204                   X-DOMICILE  X-STATE     X-RESERV    X-ALTRSV    
03205                   X-REMAIN    X-PAID      X-C78       X-CRATA     
03206                   X-CDOMI     X-TAX       X-T78       X-TRATA     
03207                   X-TDOMI     X-SFPAID    X-SF78      X-SFRATA    
03208                   X-SFDOMI    X-IUNDR     X-IOVER     X-GUNDR     
03209                   X-GOVER     ZERO-SW.                            
03210                                                                   
03211      MOVE +0  TO  X2.                                             
03212                                                                   
03213  3020-ZERO-X2.                                                    
03214      ADD +1  TO  X2.                                              
03215                                                                   
03216      IF X2 GREATER MAX-BEN                                        
03217          GO TO 3030-ZERO-MORT.                                    
03218                                                                   
03219      MOVE X-DETL  TO  X-AMTS (X2).                                
03220                                                                   
03221      GO TO 3020-ZERO-X2.                                          
03222                                                                   
03223  3030-ZERO-MORT.                                                  
03224      MOVE +0  TO  M1.                                             
03225                                                                   
03226  3040-ZERO-M1.                                                    
03227      ADD +1  TO  M1.                                              
03228                                                                   
03229 **   IF M1 GREATER +40                                            
CIDMOD     IF M1 GREATER +60                                            
03230          GO TO 3099-EXIT.                                         
03231                                                                   
03232      MOVE +0  TO  M2.                                             
03233                                                                   
03234  3050-ZERO-M2.                                                    
03235      ADD +1  TO  M2.                                              
03236                                                                   
03237      IF M2 GREATER MAX-MORT-LF-TYPES                              
03238          GO TO 3040-ZERO-M1.                                      
03239                                                                   
03240      MOVE XM-DETL TO XM-AMTS (M1 M2).                             
03241                                                                   
03242      GO TO 3050-ZERO-M2.                                          
03243                                                                   
03244  3099-EXIT.                                                       
03245      EXIT.                                                        
03246  EJECT                                                            
03247  5000-ROLL-RTN.                                                   
03248      MOVE +0  TO  X2.                                             
03249                                                                   
03250  5020-ROLL-X2.                                                    
03251      ADD +1  TO  X2.                                              
03252                                                                   
03253      IF X2 GREATER MAX-BEN                                        
03254          GO TO 5030-ROLL-MORT.                                    
03255                                                                   
03256      MOVE X-AMTS (X2)         TO  X-DETL.                         
03257      MOVE REIN-SUB-AMTS (X2)  TO  R-DETL.                         
03258                                                                   
03259      ADD X-COUNT    TO  R-COUNT.                                  
03260      ADD X-WRITTEN  TO  R-WRITTEN.                                
03261      ADD X-P78      TO  R-P78.                                    
03262      ADD X-PRATA    TO  R-PRATA.                                  
03263      ADD X-DOMICILE TO  R-DOMICILE.                               
03264      ADD X-STATE    TO  R-STATE.                                  
03265      ADD X-RESERV   TO  R-RESERV.                                 
03266      ADD X-ALTRSV   TO  R-ALTRSV.                                 
03267      ADD X-REMAIN   TO  R-REMAIN.                                 
03268      ADD X-PAID     TO  R-PAID.                                   
03269      ADD X-C78      TO  R-C78.                                    
03270      ADD X-CRATA    TO  R-CRATA.                                  
03271      ADD X-CDOMI    TO  R-CDOMI.                                  
03272      ADD X-TAX      TO  R-TAX.                                    
03273      ADD X-T78      TO  R-T78.                                    
03274      ADD X-TRATA    TO  R-TRATA.                                  
03275      ADD X-TDOMI    TO  R-TDOMI.                                  
03276                                                                   
03277      ADD X-SFPAID   TO  R-SFPAID.                                 
03278      ADD X-SF78     TO  R-SF78.                                   
03279      ADD X-SFRATA   TO  R-SFRATA.                                 
03280      ADD X-SFDOMI   TO  R-SFDOMI.                                 
03281                                                                   
03282      MOVE R-DETL  TO  REIN-SUB-AMTS (X2).                         
03283                                                                   
03284      GO TO 5020-ROLL-X2.                                          
03285  EJECT                                                            
03286  5030-ROLL-MORT.                                                  
03287      MOVE +0  TO  M1.                                             
03288                                                                   
03289  5040-ROLL-M1.                                                    
03290      ADD +1  TO  M1.                                              
03291                                                                   
03292 **   IF M1 GREATER +40                                            
CIDMOD     IF M1 GREATER +60                                            
03293          GO TO 5099-EXIT.                                         
03294                                                                   
03295      MOVE +0  TO  M2.                                             
03296                                                                   
03297  5050-ROLL-M2.                                                    
03298      ADD +1  TO  M2.                                              
03299                                                                   
03300      IF M2 GREATER MAX-MORT-LF-TYPES                              
03301          GO TO 5040-ROLL-M1.                                      
03302                                                                   
03303      MOVE XM-AMTS (M1 M2)          TO  XM-DETL.                   
03304      MOVE REIN-SUB-M-AMTS (M1 M2)  TO  RM-DETL.                   
03305                                                                   
03306      ADD X-IUNDR  TO  R-IUNDR.                                    
03307      ADD X-IOVER  TO  R-IOVER.                                    
03308      ADD X-GUNDR  TO  R-GUNDR.                                    
03309      ADD X-GOVER  TO  R-GOVER.                                    
03310                                                                   
03311      MOVE RM-DETL  TO  REIN-SUB-M-AMTS (M1 M2).                   
03312                                                                   
03313      GO TO 5050-ROLL-M2.                                          
03314                                                                   
03315  5099-EXIT.                                                       
03316      EXIT.                                                        
03317  EJECT                                                            
03318  6000-LOAD-TABLES.                                                
03319      MOVE +0           TO  X2.                                    
03320      MOVE CLAS-STARTL  TO  CLAS-INDEXL.                           
03321      MOVE CLAS-STARTA  TO  CLAS-INDEXA.                           
03322      MOVE HIGH-VALUE   TO  X-POINTERS.                            
03323                                                                   
03324      IF CLAS-MAXL = ZEROS                                         
03325          GO TO 6020-FORMAT-AH-RTN.                                
03326                                                                   
03327  6010-FORMAT-LIFE.                                                
03328      IF CLAS-INDEXL GREATER CLAS-MAXL                             
03329          GO TO 6020-FORMAT-AH-RTN.                                
03330                                                                   
03331      ADD +1  TO  X2.                                              
03332                                                                   
03333      MOVE CLAS-I-BEN (CLAS-INDEXL)   TO  X-BEN (X2).              
03334      MOVE 1                          TO  X-TYP (X2).              
03335      MOVE CLAS-I-AB10 (CLAS-INDEXL)  TO  X-DESC (X2).             
03336                                                                   
03337      ADD +1  TO  CLAS-INDEXL.                                     
03338                                                                   
03339      GO TO 6010-FORMAT-LIFE.                                      
03340                                                                   
03341  6020-FORMAT-AH-RTN.                                              
03342      IF CLAS-MAXA = ZEROS                                         
03343          GO TO 6040-FORMAT-SET.                                   
03344                                                                   
03345  6030-FORMAT-AH.                                                  
03346      IF CLAS-INDEXA GREATER CLAS-MAXA                             
03347          GO TO 6040-FORMAT-SET.                                   
03348                                                                   
03349      ADD +1  TO  X2.                                              
03350                                                                   
03351      MOVE CLAS-I-BEN (CLAS-INDEXA)   TO  X-BEN (X2).              
03352      MOVE 2                          TO  X-TYP (X2).              
03353      MOVE CLAS-I-AB10 (CLAS-INDEXA)  TO  X-DESC (X2).             
03354                                                                   
03355      ADD +1  TO  CLAS-INDEXA.                                     
03356                                                                   
03357      GO TO 6030-FORMAT-AH.                                        
03358                                                                   
03359  6040-FORMAT-SET.                                                 
03360      MOVE X2  TO  MAX-BEN.                                        
03361                                                                   
03362      ADD +1  TO  CLAS-MAXM.                                       
03363                                                                   
03364 **   IF CLAS-MAXM GREATER +40                                     
03365 **       MOVE +40  TO  CLAS-MAXM.                                 
CIDMOD     IF CLAS-MAXM GREATER +60                                     
CIDMOD         MOVE +60  TO  CLAS-MAXM.                                 
03366                                                                   
03367      MOVE CLAS-MAXM  TO  M1.                                      
03368      MOVE SPACES     TO  CLAS-MORT-CODE (M1).                     
03369      MOVE 'OTHERS'   TO  CLAS-MORT-DESC (M1).                     
03370                                                                   
03371  6099-EXIT.                                                       
03372      EXIT.                                                        
03373  EJECT                                                            
03374  7000-HD-RTN.                                                     
03375      ADD +1  TO  PGCTR.                                           
03376                                                                   
03377      MOVE PGCTR    TO  HD-PAGE.                                   
03378      MOVE HD1      TO  P-LN.                                      
03379      MOVE '1'      TO  P-CCSW.                                    
03380                                                                   
03381      PERFORM 8000-PRT-RTN  THRU  8099-EXIT.                       
03382                                                                   
03383      MOVE HD2  TO  P-LN.                                          
03384                                                                   
03385      PERFORM 8000-PRT-RTN  THRU  8099-EXIT.                       
03386                                                                   
03387      MOVE HD3  TO  P-LN.                                          
03388                                                                   
03389      PERFORM 8000-PRT-RTN  THRU  8099-EXIT.                       
03390                                                                   
03391      MOVE '0'      TO  P-CCSW.                                    
03392                                                                   
03393      IF HDE NOT = SPACES                                          
03394         MOVE HDE TO P-LN                                          
03395         PERFORM 8000-PRT-RTN THRU 8099-EXIT.                      
03396                                                                   
03397      IF HDD NOT = SPACES                                          
03398         MOVE HDD TO P-LN                                          
03399         PERFORM 8000-PRT-RTN THRU 8099-EXIT.                      
03400                                                                   
03401      IF HDC NOT = SPACES                                          
03402         MOVE HDC TO P-LN                                          
03403         PERFORM 8000-PRT-RTN THRU 8099-EXIT.                      
03404                                                                   
03405      IF HDB NOT = SPACES                                          
03406         MOVE HDB TO P-LN                                          
03407         PERFORM 8000-PRT-RTN THRU 8099-EXIT.                      
03408                                                                   
03409      IF HDA NOT = SPACES                                          
03410         MOVE HDA TO P-LN                                          
03411         PERFORM 8000-PRT-RTN THRU 8099-EXIT.                      
03412                                                                   
03413      IF HDF NOT = SPACES                                          
03414         MOVE HDF TO P-LN                                          
03415         PERFORM 8000-PRT-RTN THRU 8099-EXIT.                      
03416                                                                   
03417      IF HDG NOT = SPACES                                          
03418         MOVE HDG TO P-LN                                          
03419         PERFORM 8000-PRT-RTN THRU 8099-EXIT.                      
03420                                                                   
03421      IF HDH NOT = SPACES                                          
03422         MOVE HDH TO P-LN                                          
03423         PERFORM 8000-PRT-RTN THRU 8099-EXIT.                      
03424                                                                   
03425      IF PASS-SW NOT = +2                                          
03426          IF USE-DATE-LINE AND HEAD-SW1 = 'X'                      
03427              MOVE HEADLINE  TO  P-LN                              
03428              MOVE '0'       TO  P-CCSW                            
03429              PERFORM 8000-PRT-RTN  THRU  8099-EXIT.               
03430                                                                   
03431  7099-EXIT.                                                       
03432      EXIT.                                                        
03433  EJECT                                                            
03434  7100-REMOVE-SPACES.                                              
03435                                                                   
03436      IF HDE NOT = SPACES                                          
03437         MOVE HDE                 TO W-WORK-LINE                   
03438         PERFORM 7110-PROCESS-LINE THRU 7110-EXIT                  
03439         MOVE W-WORK-LINE         TO HDE.                          
03440                                                                   
03441      IF HDD NOT = SPACES                                          
03442         MOVE HDD                 TO W-WORK-LINE                   
03443         PERFORM 7110-PROCESS-LINE THRU 7110-EXIT                  
03444         MOVE W-WORK-LINE         TO HDD.                          
03445                                                                   
03446      IF HDC NOT = SPACES                                          
03447         MOVE HDC                 TO W-WORK-LINE                   
03448         PERFORM 7110-PROCESS-LINE THRU 7110-EXIT                  
03449         MOVE W-WORK-LINE         TO HDC.                          
03450                                                                   
03451      IF HDB NOT = SPACES                                          
03452         MOVE HDB                 TO W-WORK-LINE                   
03453         PERFORM 7110-PROCESS-LINE THRU 7110-EXIT                  
03454         MOVE W-WORK-LINE         TO HDB.                          
03455                                                                   
03456      IF HDA NOT = SPACES                                          
03457         MOVE HDA                 TO W-WORK-LINE                   
03458         PERFORM 7110-PROCESS-LINE THRU 7110-EXIT                  
03459         MOVE W-WORK-LINE         TO HDA.                          
03460                                                                   
03461      IF HDF NOT = SPACES                                          
03462         MOVE HDF                 TO W-WORK-LINE                   
03463         PERFORM 7110-PROCESS-LINE THRU 7110-EXIT                  
03464         MOVE W-WORK-LINE         TO HDF.                          
03465                                                                   
03466      IF HDG NOT = SPACES                                          
03467         MOVE HDG                 TO W-WORK-LINE                   
03468         PERFORM 7110-PROCESS-LINE THRU 7110-EXIT                  
03469         MOVE W-WORK-LINE         TO HDG.                          
03470                                                                   
03471      IF HDH NOT = SPACES                                          
03472         MOVE HDH                 TO W-WORK-LINE                   
03473         PERFORM 7110-PROCESS-LINE THRU 7110-EXIT                  
03474         MOVE W-WORK-LINE         TO HDH.                          
03475                                                                   
03476  7100-EXIT.                                                       
03477      EXIT.                                                        
03478                                  EJECT                            
03479  7110-PROCESS-LINE.                                               
03480                                                                   
03481      MOVE W-WORK-AREA-2          TO W-WORKING-LINE.               
03482      MOVE SPACES                 TO W-WORK-AREA-2.                
03483      MOVE ZEROS                  TO W-WA2-NDX.                    
03484                                                                   
03485      PERFORM 7120-TRANSFER-DATA THRU 7120-EXIT                    
03486              VARYING                                              
03487          W-WL-NDX FROM 1 BY 1                                     
03488              UNTIL                                                
03489          W-WL-NDX GREATER THAN +86.                               
03490                                                                   
03491  7110-EXIT.                                                       
03492      EXIT.                                                        
03493                                                                   
03494  7120-TRANSFER-DATA.                                              
03495                                                                   
03496      IF  W-WL-CHAR (W-WL-NDX) EQUAL SPACES                        
03497              AND                                                  
03498          W-WA2-CHAR (W-WA2-NDX) EQUAL SPACES                      
03499          NEXT SENTENCE                                            
03500                                                                   
03501      ELSE                                                         
03502          IF  W-WL-CHAR (W-WL-NDX) EQUAL ')'                       
03503                  AND                                              
03504              W-WA2-CHAR (W-WA2-NDX) EQUAL SPACES                  
03505              MOVE W-WL-CHAR (W-WL-NDX)                            
03506                                  TO W-WA2-CHAR (W-WA2-NDX)        
03507                                                                   
03508          ELSE                                                     
03509              ADD +1              TO W-WA2-NDX                     
03510              MOVE W-WL-CHAR (W-WL-NDX)                            
03511                                  TO W-WA2-CHAR (W-WA2-NDX).       
03512                                                                   
03513  7120-EXIT.                                                       
03514      EXIT.                                                        
03515  EJECT                                                            
03516  8000-PRT-RTN.                                                    
03517      MOVE P-CCSW   TO  X P-CTL.                                   
03518      MOVE P-LN     TO  P-DATA.                                    
03519      MOVE ' '      TO  P-REC.                                     
03520                                                                   
03521      IF X = ' '                                                   
03522          ADD +1  TO  LNCTR                                        
03523      ELSE                                                         
03524          IF X = '0'                                               
03525              ADD +2  TO  LNCTR                                    
03526          ELSE                                                     
03527              IF X = '-'                                           
03528                  ADD +3  TO  LNCTR                                
03529              ELSE                                                 
03530                  MOVE +1  TO  LNCTR.                              
03531                                                                   
03532  8010-PRT-COPY-RTN.                                               
03533                              COPY ELCPRT2.                        
03534                                                                   
03535  8099-EXIT.                                                       
03536      EXIT.                                                        
03537  EJECT                                                            
03538  9000-END-OF-JOB SECTION 48.                                      
03539                                                                   
03540  9999-EOJ-RTN.                                                    
03541                              COPY ELCPRTC.                        
03542                                                                   
03543      CLOSE PRNTR.                                                 
03544                                                                   
03545      IF RTBL-OPEN-SW = 'X'                                        
03546          CLOSE RTBL-FILE.                                         
03547                                                                   
03548      IF REIN-FILE-STATUS NOT = '00'                               
03549         MOVE ' CLOSE ERROR ON REIN' TO WS-ABEND-MESSAGE           
03550         MOVE REIN-FILE-STATUS       TO WS-ABEND-FILE-STATUS       
03551         GO TO ABEND-PGM.                                          
03552                                                                   
03553      GOBACK.                                                      
03554                                                                   
03555  ABEND-PGM.                                                       
03556                            COPY ELCABEND SUPPRESS.                
