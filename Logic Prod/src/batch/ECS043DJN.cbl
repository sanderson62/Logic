00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                ECS043.                               
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 05/13/94 16:43:46.                 
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          
00008 *                           VMOD=2.021.                           
00009 *AUTHOR.     LOGIC, INC.                                          
00010 *            DALLAS, TEXAS.                                       
00011                                                                   
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *
00021 *            *                                                   *
00022 *            *****************************************************
00023                                                                   
00024 *REMARKS.    THIS PROGRAM WILL READ THE EP-EC FILE AND PRINT      
00025 *            EARNED REVIEW STATEMENTS.                            
00026                                                                   
00027 *            AN OPTIONAL CARD INPUT MAY BE USED TO SPECIFY A      
00028 *        FORMAT OTHER THAN YTD AND ITD.                           
00029                                                                   
00030 *        FORMAT OF INPUT IS -                                     
00031 *         CC 1-2    CARD I.D. 'RF'                                
00032 *            3-5    CLIENT-ID                                     
00033 *            6      LEFT SIDE FORMAT  (RIGHT SIDE ALWAYS ITD)     
00034                                                                   
00035 *                  BLANK  USE DATE IN CC 7-14 AS BEGINNING DATE   
00036 *                      1  YTD                                     
00037 *                      2  QTD                                     
00038 *                      3  LAST 3 MOS.                             
00039 *                      4  LAST 6 MOS.                             
00040 *                      5  LAST 12 MOS.                            
00041 *                      6  LAST MONTH                              
00042                                                                   
00043 *            7-14   BEGINNING DATE  (CCYYMMDD)                    
00044                                                                   
060402******************************************************************
060402*                   C H A N G E   L O G
060402*
060402* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
060402*-----------------------------------------------------------------
060402*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
060402* EFFECTIVE    NUMBER
060402*-----------------------------------------------------------------
060402* 060402    2002060300014  SMVA  MOVE PAGE # FROM BOTTOM TO TOP
061302* 061302    2002061200004  SMVA  REMOVE ECS043B HARDCOPY - PRTN043
060402******************************************************************
00045                                                                   
00046                                                                   
00047  ENVIRONMENT DIVISION.                                            
00048  INPUT-OUTPUT SECTION.                                            
00049  FILE-CONTROL.                                                    
00050                                                                   
00051      SELECT  SORT-WORK       ASSIGN TO SYS001-DA-FBA1-S-SORTWK1.  
00052      SELECT  PRNTR           ASSIGN TO SYS008-UR-1403-S-SYS008.   
00053      SELECT  EPEC-FILE       ASSIGN TO SYS010-UT-2400-S-SYS010.   
00054      SELECT  ACC-MSTR        ASSIGN TO ERACCTT
00055                              ACCESS IS SEQUENTIAL                 
00056                              ORGANIZATION IS INDEXED              
00057                              FILE STATUS IS ERACCTT-FILE-STATUS   
00058                              RECORD KEY IS AM-CONTROL-PRIMARY.    
00059      SELECT  ERPLAN-IN       ASSIGN TO ERPLAN
00060                              ORGANIZATION IS INDEXED              
00061                              ACCESS IS DYNAMIC                    
00062                              FILE STATUS IS ERPLAN-FILE-STATUS    
00063                              RECORD KEY IS PL-CONTROL-PRIMARY.    
00064      SELECT  DISK-DATE       ASSIGN TO SYS019-UT-FBA1-S-SYS019.   
00065      SELECT  FICH            ASSIGN TO SYS020-UT-2400-S-SYS020.   
00066      SELECT  CONTROL-CARD    ASSIGN TO SYS006-UR-2540R-S-SYS006.  
00067                                                                   
00068  EJECT                                                            
00069  DATA DIVISION.                                                   
00070  FILE SECTION.                                                    
00071                                                                   
00072  SD  SORT-WORK.                                                   
00073                                                                   
00074  01  SW-REC-OUT.                                                  
00075      12  SW-RPT-TYPE-ID-OUT  PIC X.                               
00076      12  SW-CTL-OUT          PIC X(35).                           
00077      12  FILLER              PIC X(1090).                         
00078  EJECT                                                            
00079  FD  CONTROL-CARD                                                 
00080      BLOCK CONTAINS 0 RECORDS
00081      RECORDING MODE F.                                            
00082                                                                   
00083  01  CARD-IN-AREA            PIC X(80).                           
00084                                                                   
00085  FD  PRNTR                                                        
00086                              COPY ELCPRTFD.                       
00087                                                                   
00088  EJECT                                                            
00089  FD  EPEC-FILE                                                    
00090                              COPY ECSEPCFD.                       
00091                              COPY ECSEPC01.                       
00092                                                                   
00093  EJECT                                                            
00094  FD  ACC-MSTR.                                                    
00095                              COPY ERCACCT.                        
00096                                                                   
00097  EJECT                                                            
00098  FD  ERPLAN-IN.
00099                              COPY ERCPLAN.                        
00100                                                                   
00101  EJECT                                                            
00102  FD  DISK-DATE                                                    
00103                              COPY ELCDTEFD.                       
00104                                                                   
00105  EJECT                                                            
00106  FD  FICH                                                         
00107                              COPY ELCFCHFD.                       
00108                                                                   
00109  EJECT                                                            
00110  WORKING-STORAGE SECTION.                                         
00111                                                                   
00112  77  FILLER  PIC X(32) VALUE '********************************'.  
00113  77  FILLER  PIC X(32) VALUE '     ECS043 WORKING STORAGE     '.  
00114  77  FILLER  PIC X(32) VALUE '***********VMOD=2.020 **********'.  
00115                                                                   
00116  77  NO-OF-RECORDS-RELEASED  PIC S9(9)      COMP-3  VALUE +0.     
00117  77  NDX                     PIC S999            COMP.            
00118  77  SA                      PIC S999            COMP.            
00119  77  SB                      PIC S999            COMP.            
00120  77  SC                      PIC S999            COMP.            
00121  77  SE                      PIC S999            COMP.            
00122  77  SF                      PIC S999            COMP.            
00123  77  SG                      PIC S999            COMP.            
00124  77  SH                      PIC S999            COMP.            
00125  77  CL                      PIC S999            COMP.            
00126  77  AL                      PIC S999            COMP.            
00127  77  AN                      PIC S999            COMP.            
00128  77  WS-SEQ-NBR              PIC S9              COMP   VALUE +0. 
00129  77  PAGE-CNT                PIC S9(5)           COMP-3 VALUE +0. 
00130  77  LINE-CNT                PIC S999            COMP-3 VALUE +99.
00131  77  PGM-SUB                 PIC S999            COMP-3 VALUE +43.
00132  77  X                       PIC X.                               
00133  77  BREAK-1-SWITCH          PIC X       VALUE SPACES.            
00134  77  WS-EP-CODE              PIC X.                               
00135  77  WS-OB-CODE              PIC X.                               
00136  77  SAVE-DA-UP              PIC S9(9)V99.                        
00137  77  SAVE-DA-UC              PIC S9(9)V99.                        
00138  77  SAVE-DA-GA-UC           PIC S9(9)V99.                        
00139  77  SAVE-DA-PCT-E-PREM      PIC S9(9)V99.                        
00140  77  SAVE-DA-PCT-UP          PIC S9(9)V99.                        
00141  77  SAVE-BEG-DATE           PIC 9(11)     VALUE ZERO.            
00142  77  WS-RETRO-FLAG           PIC X.                               
00143                                                                   
CIDMOD 01  SPACE-LINE                PIC X(132)  VALUE SPACES.          
CIDMOD                                                                  
00144  01  WS-ABEND-STORAGE.                                            
00145      12  WS-RETURN-CODE        PIC S9(4)   VALUE ZERO COMP.       
00146      12  WS-ABEND-MESSAGE      PIC X(80)   VALUE SPACES.          
00147      12  WS-ABEND-FILE-STATUS  PIC XX      VALUE ZERO.            
00148      12  WS-ZERO               PIC S9      VALUE ZERO COMP-3.     
00149                                                                   
00150      12  WS-ABEND-CODE         PIC 9(4).                          
00151      12  WORK-ABEND-CODE       REDEFINES     WS-ABEND-CODE.       
00152          16  W-ABEND-CODE-1    PIC XX.                            
00153          16  W-ABEND-CODE-2    PIC XX.                            
00154      12  ERACCTT-FILE-STATUS   PIC XX.                            
00155      12  ERPLAN-FILE-STATUS    PIC XX.                            
00156                                                                   
00157  01  WORK-MSG.                                                    
00158      05  FILLER              PIC X    VALUE SPACE.                
00159      05  W-MM                PIC XX.                              
00160      05  FILLER              PIC X    VALUE '/'.                  
00161      05  W-DD                PIC XX.                              
00162      05  FILLER              PIC X    VALUE '/'.                  
00163      05  W-CCYY              PIC X(4).                            
00164      05  FILLER        PIC X(9)  VALUE ' TO DATE '.               
00165                                                                   
00166  01  FORMAT-CONTROL-CARD.                                         
00167      05  FC-ID               PIC XX.                              
00168      05  FC-CLIENT           PIC XXX.                             
00169      05  FC-CODE             PIC X.                               
00170      05  FC-DATE             PIC 9(8).                            
00171      05  FC-DATE-R  REDEFINES  FC-DATE.                           
00172          10  FC-CCYY         PIC 9(04).                           
00173          10  FC-CCYR REDEFINES FC-CCYY.                           
00174              15  FC-CC       PIC 99.                              
00175              15  FC-YY       PIC 99.                              
00176          10  FC-MM           PIC 99.                              
00177          10  FC-DD           PIC 99.                              
00178      05  FILLER              PIC X(66).                           
00179                                                                   
00180  EJECT                                                            
00181  01  SW-REC.                                                      
00182      12  SW-RPT-TYPE-ID      PIC X.                               
00183          88  ACCOUNT-RPT        VALUE 'A'.                        
00184          88  RETRO-GROUP-RPT    VALUE 'B'.                        
00185          88  GA-ACCOUNT-RPT     VALUE 'C'.                        
00186      12  SW-CTL.                                                  
00187          16  SW-GA           PIC X(10).                           
00188          16  SW-CARR         PIC X.                               
00189          16  SW-COMP         PIC XXXXXX.                          
00190          16  SW-STATE        PIC XX.                              
00191          16  SW-ACCT         PIC X(10).                           
00192          16  SW-EXP-DT       PIC 9(11)  COMP-3.                   
00193      12  SW-EFF-DT           PIC 9(11)  COMP-3.                   
00194      12  SW-NAME             PIC X(30).                           
00195      12  SW-ADDRS            PIC X(20).                           
00196      12  SW-CITY             PIC X(20).                           
00197      12  SW-ZIP.                                                  
00198          16  SW-ZIP-PRIME.                                        
00199              20  FILLER      PIC X(01).                           
00200                  88  SW-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.    
00201              20  FILLER      PIC X(04).                           
00202          16  SW-ZIP-PLUS4    PIC X(04).                           
00203      12  SW-CANADIAN-POSTAL-CODES REDEFINES SW-ZIP.               
00204          16  SW-CAN-POST-CODE-1                                   
00205                              PIC X(03).                           
00206          16  SW-CAN-POST-CODE-2                                   
00207                              PIC X(03).                           
00208          16  FILLER          PIC X(03).                           
00209      12  SW-LRET             PIC S9V9(4)     COMP-3.              
00210      12  SW-ARET             PIC S9V9(4)     COMP-3.              
00211      12  SW-RET-Y-N          PIC X.                               
00212      12  SW-RET-TYP          PIC X.                               
00213      12  SW-GA-FLAG          PIC X.                               
00214      12  SW-ACCUMS           PIC X(972).                          
00215      12  SW-BEG-DATE         PIC 9(11)       COMP-3.              
00216      12  SW-RETRO-LIMIT      PIC S9(7)       COMP-3.              
00217      12  SW-PREM-P-E         PIC X.                               
00218      12  SW-CLMS-P-I         PIC X.                               
00219      12  FILLER              PIC X(12).                           
00220                                                                   
00221  EJECT                                                            
00222  01  WS-ACCT-CONTROL.                                             
00223      12  WS-MSTR-CNTRL       PIC X(25)   VALUE LOW-VALUES.        
00224      12  WS-EFFECT           PIC 9(11)  COMP-3  VALUE ZEROS.      
00225                                                                   
00226  01  WS-SAVE-CONTROL.                                             
00227      12  WS-SAVE-CARR        PIC X.                               
00228      12  WS-SAVE-COMP        PIC XXXXXX.                          
00229      12  WS-SAVE-STATE       PIC XX.                              
00230      12  WS-SAVE-ACCT        PIC X(10).                           
00231      12  WS-SAVE-EXP-DATE    PIC 9(11)  COMP-3.                   
00232      12  WS-SAVE-EFF-DATE    PIC 9(11)  COMP-3.                   
00233                                                                   
00234  01  WS-SAVE-CTL.                                                 
00235      12  WS-SAVE-GA          PIC X(10).                           
00236      12  WS-SAVE-CAR         PIC X.                               
00237      12  WS-SAVE-CMP         PIC XXXXXX.                          
00238      12  WS-SAVE-ST          PIC XX.                              
00239      12  WS-SAVE-ACC         PIC X(10).                           
00240      12  WS-SAVE-EXP         PIC 9(11)  COMP-3.                   
00241                                                                   
00242  01  WS-PLAN-CONTROL.                                             
00243      12  WS-PL-COMPANY-CD    PIC X          VALUE SPACE.          
00244      12  WS-PL-CONTROL-A.                                         
00245          16  WS-PL-CARRIER       PIC X      VALUE SPACE.          
00246          16  WS-PL-GROUPING      PIC X(6)   VALUE SPACES.         
00247          16  WS-PL-STATE         PIC XX     VALUE SPACES.         
00248          16  WS-PL-ACCOUNT       PIC X(10)  VALUE SPACES.         
00249      12  WS-PL-BENEFIT-TYPE  PIC X          VALUE SPACE.          
00250      12  WS-PL-BENEFIT-CODE  PIC XX         VALUE SPACE.          
00251      12  WS-PL-REVISION-NO   PIC X(3)       VALUE SPACES.         
00252                                                                   
00253  EJECT                                                            
00254  01  MISC.                                                        
00255      12  RETENT-FACT         PIC S9V9(5)   VALUE +0.              
00256      12  ERP-PERCENT         PIC S9V9(5)   VALUE +0.              
00257      12  EXP-FACTOR          PIC S9V9(5)   VALUE +0.              
00258      12  LIFE-RETENT         PIC S9V9(5)     COMP-3  VALUE +0.    
00259      12  AH-RETENT           PIC S9V9(5)     COMP-3  VALUE +0.    
00260      12  SAVE-RETRO-LIMIT    PIC S9(7)       COMP-3.              
00261      12  ANNUALIZED-PREM     PIC S9(9)V99    COMP-3.              
00262      12  WS-MIN-LOSS         PIC S9(5)V99    COMP-3.              
00263      12  HEAD-SW             PIC X         VALUE SPACE.           
00264      12  NBR-OF-MTHS         PIC 99        VALUE 12.              
00265      12  SAVE-GA-FLAG        PIC X.                               
00266      12  SAVE-RET-Y-N        PIC X.                               
00267      12  SAVE-RET-TYP        PIC X.                               
00268      12  SAVE-PREM-P-E       PIC X.                               
00269      12  SAVE-CLMS-P-I       PIC X.                               
00270      12  WS-EXCLUSION-SWITCH PIC X             VALUE SPACE.       
00271          88  EXCLUDING-BENEFIT  VALUE '*'.                        
00272                                                                   
00273  01  BEG-DATE                PIC 9(11).                           
00274  01  BEG-DATE-R REDEFINES BEG-DATE.                               
00275      12 FILLER               PIC 999.                             
00276      12  BD-CCYY             PIC 9(04).                           
00277      12  BD-CCYR REDEFINES BD-CCYY.                               
00278          16  BD-CC           PIC 99.                              
00279          16  BD-YR           PIC 99.                              
00280      12  BD-MO               PIC 99.                              
00281      12  BD-DA               PIC 99.                              
00282                                                                   
00283  01  RUN-DT                  PIC 9(11).                           
00284  01  RUN-DT-R REDEFINES RUN-DT.                                   
00285      12  FILLER              PIC 999.                             
00286      12  RD-CCYY             PIC 9(04).                           
00287      12  RD-CCYR REDEFINES RD-CCYY.                               
00288          16  RD-CC           PIC 99.                              
00289          16  RD-YR           PIC 99.                              
00290      12  RD-MO               PIC 99.                              
00291      12  RD-DA               PIC 99.                              
00292                                                                   
00293  01  WORK-DATE           PIC 9(11).                               
00294  01  WORK-DATE-R  REDEFINES  WORK-DATE.                           
00295      12  FILLER          PIC 999.                                 
00296      12  WK-CCYY         PIC 9(04).                               
00297      12  WK-CCYR REDEFINES WK-CCYY.                               
00298          16  WK-CC       PIC 99.                                  
00299          16  WK-YR       PIC 99.                                  
00300      12  WK-MO           PIC 99.                                  
00301      12  WK-DA           PIC 99.                                  
00302                                                                   
00303  01  ZONE-GROUP.                                                  
00304      03  ZON                 PIC XXX       VALUE 'ZON'.           
00305      03  Z-G                 PIC XXX.                             
00306                                                                   
00307  EJECT                                                            
00308 *                                                                 
00309 * THE DETAIL AND AGENTS ACCUMULATORS ARE USED AS FOLLOWS          
00310 *                                                                 
00311 *    OCCURRENCE 1 = LIFE BEGINNING DATA                           
00312 *    OCCURRENCE 2 = A&H BEGINNING DATA                            
00313 *    OCCURRENCE 3 = LIFE CURRENT DATA                             
00314 *    OCCURRENCE 4 = A&H CURRENT DATA                              
00315 *                                                                 
00316  01  DETAIL-ACCUMULATORS     COMP-3.                              
00317      12  DETAIL-ACCUMS   OCCURS 4 TIMES.                          
00318          16  DA-ISP          PIC S9(9)V99.                        
PEMMOD         16  DA-TAX          PIC S9(9)V99.                        
00319          16  DA-CNP          PIC S9(9)V99.                        
00320          16  DA-UP           PIC S9(9)V99.                        
00321          16  DA-CLM          PIC S9(9)V99.                        
00322          16  DA-EXP          PIC S9(9)V99.                        
00323          16  DA-CPS          PIC S9(9)V99.                        
00324          16  DA-OTC          PIC S9(9)V99.                        
00325          16  DA-LRV          PIC S9(9)V99.                        
00326          16  DA-CAJ          PIC S9(9)V99.                        
00327          16  DA-ISC          PIC S9(9)V99.                        
00328          16  DA-CNC          PIC S9(9)V99.                        
00329          16  DA-UC           PIC S9(9)V99.                        
00330          16  DA-ISC-ADDL     PIC S9(9)V99.                        
00331          16  DA-CNC-ADDL     PIC S9(9)V99.                        
00332          16  DA-UC-ADDL      PIC S9(9)V99.                        
00333          16  DA-GA-ISC       PIC S9(9)V99.                        
00334          16  DA-GA-CNC       PIC S9(9)V99.                        
00335          16  DA-GA-UC        PIC S9(9)V99.                        
00336          16  DA-PCT-E-PREM   PIC S9(9)V99.                        
00337                                                                   
00338  01  TEMP-GA-ACCUMS.                                              
00339      12  GA-LEVELS.                                               
00340          16  GA-LEVELS OCCURS 10 TIMES.                           
00341              20  XDA-GA-NO       PIC X(10).                       
00342              20  XDA-GA-TYPE     PIC X.                           
00343              20  ACCUM-LEVELS OCCURS 4 TIMES.                     
00344                  24  XDA-GA-ISC      PIC S9(9)V99    COMP-3.      
00345                  24  XDA-GA-CNC      PIC S9(9)V99    COMP-3.      
00346                  24  XDA-GA-UC       PIC S9(9)V99    COMP-3.      
00347                                                                   
00348  01  EC-REC-MAX                  PIC S99 COMP-3 VALUE +10.        
00349  EJECT                                                            
00350 *                                                                 
00351 * THE PRINT ACCUMULATORS ARE USED AS FOLLOWS                      
00352 *                                                                 
00353 *    A SET CONSISTS OF 6 ACCUMULATORS                             
00354 *                                                                 
00355 *        OCCURRENCE 1 = LIFE TOTAL PERIOD                         
00356 *        OCCURRENCE 2 = A&H TOTAL FOR PERIOD                      
00357 *        OCCURRENCE 3 = COMBINED TOTAL FOR PERIOD                 
00358 *        OCCURRENCE 4 = LIFE TOTAL FOR INCEPTION-TO-DATE          
00359 *        OCCURRENCE 5 = A&H TOTAL FOR INCEPTION-TO-DATE           
00360 *        OCCURRENCE 6 = COMBINED TOTAL FOR INCEPTION-TO-DATE      
00361 *                                                                 
00362 *    THERE ARE 6 SETS                                             
00363 *                                                                 
00364 *        SET 1 = LEVEL 1 (DATE RANGE TOTALS)                      
00365 *        SET 2 = LEVEL 2 (ACCOUNT TOTALS)                         
00366 *        SET 3 = LEVEL 3 (STATE TOTALS)                           
00367 *        SET 4 = LEVEL 4 (COMPANY TOTALS)                         
00368 *        SET 5 = LEVEL 5 (CARRIER TOTALS)                         
00369 *        SET 6 = LEVEL 6 (FINAL TOTALS)                           
00370 *                                                                 
00371  01  PRINT-ACCUMULATORS      COMP-3.                              
00372      12  PRINT-ACCUMS    OCCURS 36.                               
00373          16  PA-NW           PIC S9(9)V99.                        
00374          16  PA-BR           PIC S9(9)V99.                        
00375          16  PA-ER           PIC S9(9)V99.                        
00376          16  PA-EP           PIC S9(9)V99.                        
00377          16  PA-PE           PIC S9(9)V99.                        
00378          16  PA-PL           PIC S9(9)V99.                        
00379          16  PA-BLR          PIC S9(9)V99.                        
00380          16  PA-ELR          PIC S9(9)V99.                        
00381          16  PA-TL           PIC S9(9)V99.                        
00382          16  PA-RC           PIC S9(9)V99.                        
00383          16  PA-OC           PIC S9(9)V99.                        
00384          16  PA-TC           PIC S9(9)V99.                        
00385          16  PA-CP           PIC S9(9)V99.                        
00386          16  PA-EX           PIC S9(9)V99.                        
00387          16  PA-TE           PIC S9(9)V99.                        
00388          16  PA-BA           PIC S9(9)V99.                        
00389          16  PA-NTE          PIC S9(9)V99.                        
00390          16  PA-NBA          PIC S9(9)V99.                        
00391          16  PA-EC           PIC S9(9)V99.                        
00392          16  PA-GARC         PIC S9(9)V99.                        
00393          16  PA-GAEC         PIC S9(9)V99.                        
00394          16  PA-GACU         PIC S9(9)V99.                        
00395          16  PA-CU           PIC S9(9)V99.                        
00396          16  PA-GATC         PIC S9(9)V99.                        
00397          16  PA-GAGT         PIC S9(9)V99.                        
00398          16  PA-GABA         PIC S9(9)V99.                        
00399          16  PA-GATE         PIC S9(9)V99.                        
00400  01  PRINT-ACCUMULATORS-R    REDEFINES PRINT-ACCUMULATORS  COMP-3.
00401      12  PRINT-ACCUMS-R  OCCURS 6.                                
00402          16  PRINT-ACCUMS-SL OCCURS 6.                            
00403              20  PRINT-ACCUMS-L OCCURS 27.                        
00404                  24  PA-AMT  PIC S9(9)V99.                        
00405  EJECT                                                            
00406                                                                   
00407 *                                                                 
00408 * THE FOLLOWING ACCUMULATORS ARE FOR THE PERCENTS                 
00409 *    TO BE PRINTED ON THE RETRO                                   
00410 *                                                                 
00411  01  PERCENT-ACCUMULATORS    COMP-3.                              
00412      12  PER-ACCUMS      OCCURS 2.                                
00413        14  PER-ACCUMS-X   OCCURS 27.                              
00414          16  PT-PRCNT        PIC S9(4)V9(4).                      
00415                                                                   
00416  01  DESC-TABLE.                                                  
00417      12  FILLER              PIC X(33)   VALUE                    
00418              '1. NET WRITTEN PREMIUM      00101'.                 
00419      12  FILLER              PIC X(33)   VALUE                    
00420              '2. PLUS BEGINNING RESERVE   00202'.                 
00421      12  FILLER              PIC X(33)   VALUE                    
00422              '3. LESS ENDING RESERVE      00303'.                 
00423      12  FILLER              PIC X(33)   VALUE                    
00424              '4. EARNED PREMIUM           00404'.                 
00425      12  FILLER              PIC X(33)   VALUE                    
00426              '5. PERCENT OF EARNED PREMIUM00505'.                 
00427      12  FILLER              PIC X(33)   VALUE                    
00428              '6. EXPENSES                 -0000'.                 
00429      12  FILLER              PIC X(33)   VALUE                    
00430              ' A. LOSSES                  00000'.                 
00431      12  FILLER              PIC X(33)   VALUE                    
00432              '  1. PAID                    0606'.                 
00433      12  FILLER              PIC X(33)   VALUE                    
00434              '  2. LESS BEG. LOSS RESERVE  0707'.                 
00435      12  FILLER              PIC X(33)   VALUE                    
00436              '  3. PLUS END. LOSS RESERVE  0808'.                 
00437      12  FILLER              PIC X(33)   VALUE                    
00438              '  4. TOTAL                   0909'.                 
00439      12  FILLER              PIC X(33)   VALUE                    
00440              ' B. COMMISSIONS             00000'.                 
00441      12  FILLER              PIC X(33)   VALUE                    
00442              '  1. RETAINED                2323'.                 
00443      12  FILLER              PIC X(33)   VALUE                    
00444              '  2. OTHER COMMISSION PAID   1111'.                 
00445      12  FILLER              PIC X(33)   VALUE                    
00446              '  3. OVERWRITE                 22'.                 
00447      12  FILLER              PIC X(33)   VALUE                    
00448              '  3. TOTAL                   12  '.                 
00449      12  FILLER              PIC X(33)   VALUE                    
00450              '  4. TOTAL                     24'.                 
00451      12  WS-EX-LINE          PIC X(33)   VALUE                    
00452              ' C. OTHER EXPENSE           01414'.                 
00453      12  FILLER              PIC X(33)   VALUE                    
00454              '7. TOTAL EXPENSE            01727'.                 
00455      12  WS-BAL-LINE-1       PIC X(33)   VALUE                    
00456              '8. BALANCE AT END OF PERIOD 01826'.                 
00457      12  WS-BAL-LINE-2       PIC X(33)   VALUE                    
00458              '   (LINE 5 MINUS LINE 7)     0000'.                 
00459      12  FILLER              PIC X(33)   VALUE                    
00460              '9. ACCOUNT BALANCES         0  18'.                 
00461      12  FILLER              PIC X(33)   VALUE                    
00462              '9. EXPERIENCE REFUND PRV PD 013  '.                 
00463      12  FILLER              PIC X(33)   VALUE                    
00464              '10. EXPERIENCE REFUND PRV PD0  13'.                 
00465      12  FILLER              PIC X(33)   VALUE                    
00466              '10. NET BALANCE OR LOSS     -16  '.                 
00467      12  FILLER              PIC X(33)   VALUE                    
00468              '11. NET BALANCE OR LOSS     -  25'.                 
00469      12  FILLER              PIC X(33)   VALUE HIGH-VALUES.       
00470  01  DESC-TABLE-R    REDEFINES DESC-TABLE.                        
00471      12  DT-TB-ENT   OCCURS 27.                                   
00472          16  DT-ENTRY    PIC X(28).                               
00473          16  DT-CTL-CHAR PIC X.                                   
00474          16  DT-SUBSCR   OCCURS 2.                                
00475              20  DT-SS-R.                                         
00476                  24  DT-SS   PIC 99.                              
00477                                                                   
00478  01  DESC-TABLE-DESCRIPTIONS.                                     
00479      12  EXPENSE-LINE        PIC X(33)   VALUE                    
00480              ' C. TAXES AND OTHER EXPENSES01414'.                 
00481      12  BALANCE-LINE-1      PIC X(33)   VALUE                    
00482              '8. BALANCE AT END OF PERIOD 01826'.                 
00483      12  BALANCE-LINE-2      PIC X(33)   VALUE                    
00484              '   (LINE 5 MINUS LINE 7)     0000'.                 
00485  EJECT                                                            
00486                                                                   
00487  01  HEAD-1.                                                      
00488      12  H1-NAME         PIC X(30)   VALUE SPACES.                
00489      12  FILLER          PIC X(21)   VALUE SPACES.                
00490      12  FILLER          PIC X(20)   VALUE 'CREDIT INSURANCE EAR'.
00491      12  FILLER          PIC X(10)   VALUE 'NED REVIEW'.          
00492      12  FILLER          PIC X(38)   VALUE SPACES.                
00493      12  FILLER          PIC X(6)    VALUE 'ECS043'.              
00494      12  H1-REPORT-SUF   PIC X       VALUE SPACES.                
00495                                                                   
00496  01  HEAD-2.                                                      
00497      12  H2-ADDR         PIC X(30)   VALUE SPACES.                
00498      12  FILLER          PIC X(21)   VALUE SPACES.                
00499      12  HD-COMP         PIC X(30)   VALUE SPACES.                
00500      12  FILLER          PIC X(38)   VALUE SPACES.                
00501      12  HD-RD           PIC X(8)    VALUE SPACES.                
00502                                                                   
00503  01  HEAD-3.                                                      
00504      12  H3-ADDR         PIC X(20)   VALUE SPACES.                
00505      12  FILLER          PIC X       VALUE SPACES.                
00506      12  H3-ZIP.                                                  
00507          16  H3-ZIP-PRIME                                         
00508                          PIC X(5).                                
00509          16  H3-DASH     PIC X.                                   
00510          16  H3-ZIP-PLUS4                                         
00511                          PIC X(4).                                
00512      12  H3-CANADIAN-POSTAL-CODE REDEFINES H3-ZIP.                
00513          16  H3-CAN-POSTAL-CODE-1                                 
00514                          PIC X(3).                                
00515          16  H3-DASH-CAN PIC X.                                   
00516          16  H3-CAN-POSTAL-CODE-2                                 
00517                          PIC X(3).                                
00518          16  H3-CAN-FILLER                                        
00519                          PIC X(3).                                
00520      12  FILLER          PIC X(26)   VALUE SPACES.                
00521      12  HD-AD           PIC X(18)   VALUE SPACES.                
060402     12  FILLER          PIC X(44)   VALUE SPACES.
060402     12  FILLER          PIC X(05)   VALUE
060402         'PAGE '.
060402     12  H3-PAGE         PIC ZZZZ9.

00524  01  HEAD-4.                                                      
00525      12  FILLER          PIC X(20)   VALUE 'COMP    ST ACCOUNT  '.
00526      12  H4-FT           PIC X(20)   VALUE '       FROM    TO'.   
00527      12  FILLER          PIC X(5)    VALUE SPACES.                
00528      12  H4-GROUP        PIC X(6)    VALUE SPACES.                
00529                                                                   
00530  01  HEAD-5.                                                      
00531      12  H5-CARR         PIC X       VALUE SPACES.                
00532      12  H5-COMP         PIC X(6)    VALUE SPACES.                
00533      12  FILLER          PIC X       VALUE SPACES.                
00534      12  H5-ST           PIC XX      VALUE SPACES.                
00535      12  FILLER          PIC X       VALUE SPACES.                
00536      12  H5-ACCT         PIC X(10)   VALUE SPACES.                
00537      12  FILLER          PIC X       VALUE SPACES.                
00538      12  H5-FMO          PIC XX      VALUE SPACES.                
00539      12  H5-FS1          PIC X       VALUE '-'.                   
00540      12  H5-FDA          PIC XX      VALUE SPACES.                
00541      12  H5-FS2          PIC X       VALUE '-'.                   
00542      12  H5-FYR          PIC XX      VALUE SPACES.                
00543      12  FILLER          PIC X       VALUE SPACES.                
00544      12  H5-TMO          PIC XX      VALUE SPACES.                
00545      12  H5-TS1          PIC X       VALUE '-'.                   
00546      12  H5-TDA          PIC XX      VALUE SPACES.                
00547      12  H5-TS2          PIC X       VALUE '-'.                   
00548      12  H5-TYR          PIC XX      VALUE SPACES.                
00549      12  FILLER          PIC X(4)    VALUE SPACES.                
00550      12  H5-GROUP        PIC X(10)   VALUE SPACES.                
00551      12  H5-MESSAGE      PIC X(40)   VALUE SPACES.                
CIDMOD     12  END-CHARACTER   PIC X       VALUE ' '.                   
00552                                                                   
00553  01  HEAD-6.                                                      
00554      12  FILLER          PIC X(15)   VALUE '***************'.     
00555      12  H6-YTD-HD.                                               
00556          14  FILLER      PIC X(8)    VALUE '*  LAST '.            
00557          14  H6-MTHS     PIC Z9.                                  
00558          14  FILLER      PIC X(10)   VALUE ' MONTHS  *'.          
00559      12  H6-YTD-HD-R  REDEFINES H6-YTD-HD.                        
00560          16  H6R-FILLER  PIC X.                                   
00561          16  H6R-MO      PIC XX.                                  
00562          16  H6R-DASH1   PIC X.                                   
00563          16  H6R-DA      PIC XX.                                  
00564          16  H6R-DASH2   PIC X.                                   
00565          16  H6R-CCYY    PIC X(4).                                
00566          16  H6R-MSG     PIC X(9).                                
00567      12  FILLER          PIC X(15)   VALUE '***************'.     
00568      12  FILLER          PIC X(29)   VALUE SPACES.                
00569      12  FILLER          PIC X(16)   VALUE '****************'.    
00570      12  FILLER          PIC X(21)  VALUE '  INCEPTION TO DATE  '.
00571      12  FILLER          PIC X(16)   VALUE '****************'.    
00572                                                                   
00573  01  HEAD-7.                                                      
00574      12  HEAD-7-PLUG-1   PIC X(12)   VALUE SPACES.                
00575      12  FILLER          PIC X(1)    VALUE SPACES.                
00576      12  HEAD-7-PLUG-2   PIC X(12)   VALUE SPACES.                
00577      12  FILLER          PIC X(15)   VALUE '         TOTAL '.     
00578      12  FILLER          PIC X(20)   VALUE '   PERCENT          '.
00579      12  FILLER          PIC X(20)   VALUE '                    '.
00580      12  HEAD-7-PLUG-3   PIC X(12)   VALUE SPACES.                
00581      12  FILLER          PIC X       VALUE SPACES.                
00582      12  HEAD-7-PLUG-4   PIC X(12)   VALUE SPACES.                
00583      12  FILLER          PIC X(15)   VALUE '           TOTA'.     
00584      12  FILLER          PIC X(12)   VALUE 'L    PERCENT'.        
00585                                                                   
00586  01  HEAD-8-STATE.                                                
00587      12  FILLER          PIC X(8)    VALUE '  STATE '.            
00588      12  H8-ST           PIC XX      VALUE SPACES.                
00589      12  FILLER          PIC X(7)    VALUE ' TOTALS'.             
00590                                                                   
00591  01  HEAD-8-COMP.                                                 
00592      12  FILLER          PIC X(10)   VALUE '  COMPANY '.          
00593      12  H8-COMP         PIC X(6)    VALUE SPACES.                
00594      12  FILLER          PIC X(7)    VALUE ' TOTALS'.             
00595                                                                   
00596  01  HEAD-8-CARR.                                                 
00597      12  FILLER          PIC X(10)   VALUE '  CARRIER '.          
00598      12  H8-CARR         PIC X       VALUE SPACES.                
00599      12  FILLER          PIC X(7)    VALUE ' TOTALS'.             
00600                                                                   
00601  01  HEAD-8-GROUP.                                                
00602      12  H-8-G           PIC X(8)    VALUE '  GROUP '.            
00603      12  H8-GROUP        PIC X(6)    VALUE SPACES.                
00604      12  FILLER          PIC X(7)    VALUE ' TOTALS'.             
00605                                                                   
00606  01  HEAD-8-GA.                                                   
00607      12  FILLER          PIC X(8)    VALUE '   G.A. '.            
00608      12  H8-GA           PIC X(10)   VALUE SPACES.                
00609      12  FILLER          PIC X(7)    VALUE ' TOTALS'.             
00610                                                                   
00611  01  HEAD-8-FINAL.                                                
00612      12  FILLER          PIC X(20)   VALUE '  FINAL       TOTALS'.
00613                                                                   
060402*01  HEAD-9.
060402*    12  FILLER          PIC X(127)  VALUE SPACES.
060402*    12  H9-PAGE         PIC ZZZZ9.
00617                                                                   
00618  01  DETAIL-LINE.                                                 
00619      12  DL-YLF          PIC ZZ,ZZZ,ZZZ.99-.                      
00620      12  DL-YAH          PIC ZZ,ZZZ,ZZZ.99-.                      
00621      12  DL-YT           PIC ZZ,ZZZ,ZZZ.99-.                      
00622      12  DL-YPT          PIC ZZZZ.99-    BLANK WHEN ZERO.         
00623      12  FILLER          PIC X.                                   
00624      12  DL-DESC         PIC X(28).                               
00625      12  DL-ILF          PIC ZZZ,ZZZ,ZZZ.99-.                     
00626      12  DL-IAH          PIC ZZZ,ZZZ,ZZZ.99-.                     
00627      12  DL-IT           PIC ZZZ,ZZZ,ZZZ.99-.                     
00628      12  DL-IPT          PIC ZZZZ.99-    BLANK WHEN ZERO.         
00629                                                                   
00630                                     COPY ELCDATE.                 
00631                                                                   
00632                                     COPY ELCDTECX.                
00633                                                                   
00634                                     COPY ELCDTEVR.                
00635                                                                   
00636                                     COPY ELCACCTV.                
00637                                                                   
00638                                     COPY ELCEPCVR.                
00639                                                                   
00640  EJECT                                                            
00641  PROCEDURE DIVISION.                                              
00642  0100-READ-DATE-CARD.                                             
00643                              COPY ELCDTERX SUPPRESS.              
00644                                                                   
00645      MOVE LIFE-OVERRIDE-L12     TO HEAD-7-PLUG-1                  
00646                                    HEAD-7-PLUG-3.                 
00647      MOVE AH-OVERRIDE-L12       TO HEAD-7-PLUG-2                  
00648                                    HEAD-7-PLUG-4.                 
00649      MOVE WS-CURRENT-DATE TO HD-RD.                               
00650      MOVE ALPH-DATE       TO HD-AD.                               
00651      MOVE COMPANY-NAME    TO HD-COMP.                             
00652                                                                   
CIDMOD     MOVE LOW-VALUES      TO END-CHARACTER.                       
CIDMOD                                                                  
00653      MOVE RUN-DATE        TO RUN-DT                               
00654                              BEG-DATE.                            
00655                                                                   
00656      OPEN INPUT CONTROL-CARD.                                     
00657                                                                   
00658      READ CONTROL-CARD INTO FORMAT-CONTROL-CARD                   
00659           AT END   MOVE '1' TO FC-CODE                            
00660                    GO TO 0130-BUILD-SELECT-DATES.                 
00661                                                                   
00662  0110-READ-CARD-LOOP.                                             
00663      IF FC-ID = 'RF'                                              
00664          IF FC-CLIENT = DTE-CLIENT                                
00665              GO TO 0120-FLUSH-REMAINING-CARDS.                    
00666                                                                   
00667      READ CONTROL-CARD INTO FORMAT-CONTROL-CARD                   
00668           AT END   MOVE '1' TO FC-CODE                            
00669                    GO TO 0130-BUILD-SELECT-DATES.                 
00670                                                                   
00671      GO TO 0110-READ-CARD-LOOP.                                   
00672                                                                   
00673  0120-FLUSH-REMAINING-CARDS.                                      
00674      READ CONTROL-CARD                                            
00675             AT END GO TO 0130-BUILD-SELECT-DATES.                 
00676                                                                   
00677      GO TO 0120-FLUSH-REMAINING-CARDS.                            
00678                                                                   
00679  0130-BUILD-SELECT-DATES.                                         
00680      IF DTE-PGM-OPT = '5' OR '7'                                  
00681         MOVE 'RF'       TO FC-ID                                  
00682         MOVE DTE-CLIENT TO FC-CLIENT                              
00683         MOVE '5'        TO FC-CODE                                
00684         MOVE RUN-DATE   TO FC-DATE.                               
00685                                                                   
00686      IF FC-CODE = ' '                                             
00687        IF FC-DATE NOT NUMERIC  OR                                 
00688           FC-DATE = ZEROS                                         
00689           MOVE 'INVALID BEGIN DATE  ' TO WS-ABEND-MESSAGE         
00690           GO TO ABEND-PGM.                                        
00691                                                                   
00692      IF FC-CODE = ' '                                             
00693          MOVE FC-DATE   TO BEG-DATE                               
00694          MOVE FC-MM     TO W-MM                                   
00695          MOVE FC-DD     TO W-DD                                   
00696          MOVE FC-CCYY   TO W-CCYY                                 
00697          MOVE WORK-MSG  TO H6-YTD-HD                              
00698          GO TO 0140-SORT-CONTROL.                                 
00699                                                                   
00700      IF DTE-PGM-OPT = '5'                                         
00701          MOVE FC-DATE   TO BEG-DATE.                              
00702                                                                   
00703      IF FC-CODE = ('1' OR '5')                                    
00704          SUBTRACT 1 FROM BD-CCYY                                  
00705          IF FC-CODE = '1'                                         
00706               MOVE 31   TO BD-DA                                  
00707               MOVE 12   TO BD-MO                                  
00708               MOVE '*   YEAR TO DATE   *' TO H6-YTD-HD            
00709               GO TO 0140-SORT-CONTROL                             
00710          ELSE                                                     
00711               MOVE '*  LAST 12 MONTHS  *' TO H6-YTD-HD            
00712               MOVE BEG-DATE TO SAVE-BEG-DATE                      
00713               GO TO 0140-SORT-CONTROL.                            
00714                                                                   
00715      IF FC-CODE = '2'                                             
00716          MOVE '* QUARTER TO DATE  *' TO H6-YTD-HD                 
00717          IF RD-MO LESS 3                                          
00718              MOVE 12    TO BD-MO                                  
00719              MOVE 31    TO BD-DA                                  
00720              SUBTRACT 1 FROM BD-CCYY                              
00721              GO TO 0140-SORT-CONTROL                              
00722          ELSE                                                     
00723              IF RD-MO LESS 6                                      
00724                  MOVE 03 TO BD-MO                                 
00725                  MOVE 31 TO BD-DA                                 
00726                  GO TO 0140-SORT-CONTROL                          
00727              ELSE                                                 
00728                  IF BD-MO LESS 9                                  
00729                      MOVE 06 TO BD-MO                             
00730                      MOVE 30 TO BD-DA                             
00731                      GO TO 0140-SORT-CONTROL                      
00732                  ELSE                                             
00733                      MOVE 09 TO BD-MO                             
00734                      MOVE 30 TO BD-DA                             
00735                      GO TO 0140-SORT-CONTROL.                     
00736                                                                   
00737      MOVE BIN-RUN-DATE          TO DC-BIN-DATE-1.                 
00738      MOVE '6'                   TO DC-OPTION-CODE.                
00739                                                                   
00740      EVALUATE FC-CODE                                             
00741         WHEN '3'                                                  
00742            MOVE '*   LAST 3 MONTHS  *' TO H6-YTD-HD               
00743            MOVE  -3 TO DC-ELAPSED-MONTHS                          
00744         WHEN '4'                                                  
00745            MOVE '*   LAST 6 MONTHS  *' TO H6-YTD-HD               
00746            MOVE  -6 TO DC-ELAPSED-MONTHS                          
00747         WHEN '6'                                                  
00748            MOVE '*    LAST MONTH    *' TO H6-YTD-HD               
00749            MOVE  -1 TO DC-ELAPSED-MONTHS                          
00750      END-EVALUATE.                                                
00751                                                                   
00752      MOVE ZEROS                 TO DC-ELAPSED-DAYS.               
00753      MOVE '1'                   TO DC-END-OF-MONTH.               
00754      PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT.                 
00755      MOVE DC-GREG-DATE-CYMD     TO BEG-DATE.                      
00756                                                                   
00757                                                                   
00758  EJECT                                                            
00759  0140-SORT-CONTROL.                                               
00760      SORT SORT-WORK ON                                            
00761          ASCENDING  SW-RPT-TYPE-ID-OUT                            
00762                     SW-CTL-OUT                                    
00763          INPUT PROCEDURE 0150-BUILD-EXTRACTS                      
00764          OUTPUT PROCEDURE                                         
00765              0745-OUTPUT-PROCEDURE THRU 1090-CLOSE-EXIT.          
00766                                                                   
00767      IF SORT-RETURN NOT = ZERO AND 4                              
00768           MOVE SORT-RETURN            TO WS-RETURN-CODE           
00769           MOVE 'UNSUCCESSFUL SORT RC' TO WS-ABEND-MESSAGE         
00770           GO TO ABEND-PGM.                                        
00771                                                                   
00772      GOBACK.                                                      
00773                                                                   
00774      EJECT                                                        
00775  0150-BUILD-EXTRACTS SECTION.                                     
00776                                                                   
00777  0160-BEGIN-PROCESSING.                                           
00778      OPEN INPUT                                                   
00779              ACC-MSTR                                             
00780              ERPLAN-IN
00781              EPEC-FILE                                            
00782          OUTPUT                                                   
00783              PRNTR.                                               
00784                                                                   
00785      IF ERACCTT-FILE-STATUS  = '00' OR '97'                       
00786          NEXT SENTENCE                                            
00787        ELSE                                                       
00788          MOVE '11'                TO W-ABEND-CODE-1               
00789          MOVE ERACCTT-FILE-STATUS TO W-ABEND-CODE-2               
00790          MOVE WS-ABEND-CODE       TO WS-RETURN-CODE               
00791          MOVE 'OPEN ERROR - ERACCTT' TO WS-ABEND-MESSAGE          
00792          GO TO ABEND-PGM.                                         
00793                                                                   
00794      IF ERPLAN-FILE-STATUS  = '00' OR '97'                        
00795          NEXT SENTENCE                                            
00796        ELSE                                                       
00797          MOVE '12'                TO W-ABEND-CODE-1               
00798          MOVE ERPLAN-FILE-STATUS  TO W-ABEND-CODE-2               
00799          MOVE WS-ABEND-CODE       TO WS-RETURN-CODE               
00800          MOVE 'OPEN ERROR - ERPLAN ' TO WS-ABEND-MESSAGE          
00801          GO TO ABEND-PGM.                                         
00802                                                                   
00803      PERFORM 0550-ZERO-DA  VARYING SA FROM 1 BY 1                 
00804                            UNTIL SA GREATER 4.                    
00805      PERFORM 0560-ZERO-PA  VARYING SB FROM 1 BY 1                 
00806                            UNTIL SB GREATER 36.                   
00807      PERFORM 0570-ZERO-XDA VARYING CL FROM 1 BY 1                 
00808                            UNTIL CL GREATER 10.                   
00809                                                                   
00810      PERFORM 0240-READ-EXTRACT THRU 0240-EXIT.                    
00811      PERFORM 0260-READ-ACCT    THRU 0260-EXIT.                    
00812                                                                   
00813      MOVE EP-CONTROL TO WS-SAVE-CONTROL.                          
00814                                                                   
00815      GO TO 0200-CHECK-ACCT-MSTR.                                  
00816                                                                   
00817  EJECT                                                            
00818  0170-CHECK-CONTROL.                                              
00819                                                                   
00820      PERFORM 0250-CHECK-PLAN  THRU 0250-EXIT.                     
00821                                                                   
00822      IF EXCLUDING-BENEFIT                                         
00823         GO TO 0190-BYPASS-ACCUM.                                  
00824                                                                   
00825      IF DTE-PGM-OPT = '5'                                         
00826         AND AM-AN-MO NOT = RUN-MO                                 
00827         GO TO 0190-BYPASS-ACCUM.                                  
00828                                                                   
00829      IF AM-RET-Y-N = 'L'                                          
00830          IF EP-RCD-TYPE NOT = LIFE-OVERRIDE-L1                    
00831              GO TO 0190-BYPASS-ACCUM.                             
00832                                                                   
00833      IF AM-RET-Y-N = 'D'                                          
00834          IF EP-RCD-TYPE NOT = AH-OVERRIDE-L1                      
00835              GO TO 0190-BYPASS-ACCUM.                             
00836                                                                   
00837      IF EP-RECORD-ID = 'EP'                                       
00838          IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                        
00839              ADD EP-RETRO-PAYMENTS TO DA-CPS (3)                  
00840              ADD EP-RETRO-EXPENSES TO DA-EXP (3)                  
00841              ADD EP-RETRO-OTH-COMM TO DA-OTC (3)                  
00842          ELSE                                                     
00843              ADD EP-RETRO-PAYMENTS TO DA-CPS (4)                  
00844              ADD EP-RETRO-EXPENSES TO DA-EXP (4)                  
00845              ADD EP-RETRO-OTH-COMM TO DA-OTC (4).                 
00846                                                                   
00847      IF EP-RECORD-ID = 'EP'                                       
00848          IF EP-RUN-DTE NOT GREATER BEG-DATE                       
00849              IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                    
00850                  ADD EP-RETRO-PAYMENTS TO DA-CPS (1)              
00851                  ADD EP-RETRO-EXPENSES TO DA-EXP (1)              
00852                  ADD EP-RETRO-OTH-COMM TO DA-OTC (1)              
00853              ELSE                                                 
00854                  ADD EP-RETRO-PAYMENTS TO DA-CPS (2)              
00855                  ADD EP-RETRO-EXPENSES TO DA-EXP (2)              
00856                  ADD EP-RETRO-OTH-COMM TO DA-OTC (2).             
00857                                                                   
00858      IF EP-PURGE = 'P'                                            
00859          IF EP-RUN-DTE GREATER BEG-DATE                           
00860              MOVE 'Q' TO EP-PURGE                                 
00861              GO TO 0180-TEST-ACCUM                                
00862          ELSE                                                     
00863              GO TO 0180-TEST-ACCUM.                               
00864                                                                   
00865      IF EP-RUN-CCYY = RD-CCYY  AND                                
00866         EP-RUN-MO = RD-MO                                         
00867          MOVE '2' TO EP-PURGE                                     
00868          GO TO 0180-TEST-ACCUM.                                   
00869                                                                   
00870      IF EP-RUN-CCYY = BD-CCYY  AND                                
00871         EP-RUN-MO = BD-MO                                         
00872          MOVE '1' TO EP-PURGE                                     
00873          GO TO 0180-TEST-ACCUM.                                   
00874                                                                   
00875      GO TO 0190-BYPASS-ACCUM.                                     
00876                                                                   
00877  EJECT                                                            
00878  0180-TEST-ACCUM.                                                 
00879      IF EP-RECORD-ID = 'EP'                                       
00880          IF EP-PURGE = ('1' OR 'P')                               
00881              IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                    
00882                  MOVE +1 TO SA                                    
00883                  MOVE LIFE-RETENT   TO RETENT-FACT                
00884                  PERFORM 0640-EP-ADD THRU 0640-EXIT               
00885              ELSE                                                 
00886                  MOVE +2 TO SA                                    
00887                  MOVE   AH-RETENT   TO RETENT-FACT                
00888                  PERFORM 0640-EP-ADD THRU 0640-EXIT.              
00889                                                                   
00890      IF EP-RECORD-ID = 'EP'                                       
00891          IF EP-PURGE = ('2' OR 'P' OR 'Q')                        
00892              IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                    
00893                  MOVE +3 TO SA                                    
00894                  MOVE LIFE-RETENT   TO RETENT-FACT                
00895                  PERFORM 0640-EP-ADD THRU 0640-EXIT               
00896              ELSE                                                 
00897                  MOVE +4 TO SA                                    
00898                  MOVE   AH-RETENT   TO RETENT-FACT                
00899                  PERFORM 0640-EP-ADD THRU 0640-EXIT.              
00900                                                                   
00901      IF EP-RECORD-ID = 'EC'                                       
00902          IF EP-PURGE = ('1' OR 'P')                               
00903              IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                    
00904                  MOVE +1 TO SA                                    
00905                  PERFORM 0690-EC-ADD THRU 0729-EXIT               
00906              ELSE                                                 
00907                  MOVE +2 TO SA                                    
00908                  PERFORM 0690-EC-ADD THRU 0729-EXIT.              
00909                                                                   
00910      IF EP-RECORD-ID = 'EC'                                       
00911          IF EP-PURGE = ('2' OR 'P' OR 'Q')                        
00912              IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                    
00913                  MOVE +3 TO SA                                    
00914                  PERFORM 0690-EC-ADD THRU 0729-EXIT               
00915              ELSE                                                 
00916                  MOVE +4 TO SA                                    
00917                  PERFORM 0690-EC-ADD THRU 0729-EXIT.              
00918                                                                   
00919  EJECT                                                            
00920  0190-BYPASS-ACCUM.                                               
00921      PERFORM 0240-READ-EXTRACT THRU 0240-EXIT.                    
00922                                                                   
00923      IF EP-CONTROL = WS-SAVE-CONTROL                              
00924          GO TO 0170-CHECK-CONTROL.                                
00925                                                                   
00926      PERFORM 0280-CALCULATE-RETRO THRU 0330-EXIT.                 
00927                                                                   
00928      IF EP-CONTROL = HIGH-VALUES                                  
00929          GO TO 0220-BUILD-EXTRACTS-EXIT.                          
00930                                                                   
00931      MOVE EP-CONTROL TO WS-SAVE-CONTROL.                          
00932                                                                   
00933  EJECT                                                            
00934  0200-CHECK-ACCT-MSTR.                                            
00935      IF WS-ACCT-CONTROL LESS WS-SAVE-CONTROL                      
00936          IF AM-GPCD = 98                                          
00937              PERFORM 0360-BUILD-DUMMY-GA                          
00938              PERFORM 0260-READ-ACCT THRU 0260-EXIT                
00939              GO TO 0200-CHECK-ACCT-MSTR                           
00940          ELSE                                                     
00941              PERFORM 0260-READ-ACCT THRU 0260-EXIT                
00942              GO TO 0200-CHECK-ACCT-MSTR.                          
00943                                                                   
00944  0210-NO-ACCT-MSTR.                                               
00945      IF WS-ACCT-CONTROL GREATER WS-SAVE-CONTROL                   
00946          PERFORM 0270-NO-ACCT-MSG                                 
00947          PERFORM 0240-READ-EXTRACT THRU 0240-EXIT                 
00948          IF EP-CONTROL = HIGH-VALUES                              
00949              GO TO 0220-BUILD-EXTRACTS-EXIT                       
00950          ELSE                                                     
00951              MOVE EP-CONTROL TO WS-SAVE-CONTROL                   
00952              GO TO 0200-CHECK-ACCT-MSTR.                          
00953                                                                   
00954      IF DTE-PGM-OPT = '7'                                         
00955          PERFORM 0215-COMPUTE-BEG-DATE THRU 0215-EXIT.            
00956                                                                   
00957      GO TO 0170-CHECK-CONTROL.                                    
00958                                                                   
00959  EJECT                                                            
00960  0215-COMPUTE-BEG-DATE.                                           
00961      IF (AM-ANNIVERSARY-DATE NOT NUMERIC) OR                      
00962         (AM-AN-MO LESS 01 OR GREATER 12)                          
00963             MOVE SAVE-BEG-DATE TO BEG-DATE                        
00964             GO TO 0215-EXIT.                                      
00965                                                                   
00966      MOVE RD-CCYY  TO BD-CCYY.                                    
00967      MOVE AM-AN-MO TO BD-MO.                                      
00968                                                                   
00969      IF BD-MO NOT LESS RD-MO                                      
00970          SUBTRACT 1 FROM BD-CCYY.                                 
00971                                                                   
00972      IF BD-MO = 04 OR 06 OR 09 OR 11                              
00973          MOVE 30 TO BD-DA                                         
00974      ELSE                                                         
00975          MOVE 31 TO BD-DA.                                        
00976                                                                   
00977      IF BD-MO = 02                                                
00978          MOVE BD-CCYY     TO HOLD-CEN-1-CCYY                      
00979          MOVE 'H'         TO DC-OPTION-CODE                       
00980          PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT              
00981          IF ONLY-LEAP-YEAR                                        
00982              MOVE HOLD-CEN-1-DA TO BD-DA                          
00983          ELSE                                                     
00984              MOVE 28            TO BD-DA.                         
00985                                                                   
00986  0215-EXIT.                                                       
00987      EXIT.                                                        
00988                                                                   
00989  0220-BUILD-EXTRACTS-EXIT.                                        
00990      CLOSE                                                        
00991          ACC-MSTR                                                 
00992          ERPLAN-IN
00993          EPEC-FILE.                                               
00994                                                                   
00995  EJECT                                                            
00996 ****************************************************************  
00997 *                                                              *  
00998 ****************************************************************  
00999                                                                   
01000  0230-PERFORMED-ROUTINES SECTION.                                 
01001                                                                   
01002  0240-READ-EXTRACT.                                               
01003      READ EPEC-FILE AT END                                        
01004           MOVE HIGH-VALUES TO EP-CONTROL                          
01005           GO TO 0240-EXIT.                                        
01006                                                                   
01007      IF EP-RECORD-ID NOT = 'EP' AND 'EC'                          
01008          GO TO 0240-READ-EXTRACT.                                 
01009                                                                   
01010      IF EP-REIN = 'R'                                             
01011          GO TO 0240-READ-EXTRACT.                                 
01012                                                                   
01013      IF EP-RUN-DTE GREATER RUN-DT                                 
01014          GO TO 0240-READ-EXTRACT.                                 
01015                                                                   
01016      COPY ELCEPCM1.                                               
01017                                                                   
01018  0240-EXIT.                                                       
01019      EXIT.                                                        
01020  EJECT                                                            
01021                                                                   
01022  0250-CHECK-PLAN.                                                 
01023                                                                   
01024      PERFORM 0650-EP-EARN-TYPE THRU 0680-EXIT.                    
01025                                                                   
01026 *    IF AM-RET-Y-N = ' '   OR  'N'                                
01027 *        MOVE +1.0 TO LIFE-RETENT AH-RETENT                       
01028 *        GO TO 0250-EXIT.                                         
01029                                                                   
01030                                                                   
01031      IF EP-COMPANY-CD  EQUAL  WS-PL-COMPANY-CD   AND              
01032         EP-CNTRL-1     EQUAL  WS-PL-CONTROL-A    AND              
01033         EP-RCD-TYPE    EQUAL  WS-PL-BENEFIT-TYPE AND              
01034         EP-BEN-CODE    EQUAL  WS-PL-BENEFIT-CODE                  
01035             GO TO 0250-EXIT.                                      
01036                                                                   
01037      MOVE EP-COMPANY-CD       TO WS-PL-COMPANY-CD.                
01038      MOVE EP-CNTRL-1          TO WS-PL-CONTROL-A.                 
01039      MOVE EP-RCD-TYPE         TO WS-PL-BENEFIT-TYPE.              
01040      MOVE EP-BEN-CODE         TO WS-PL-BENEFIT-CODE.              
01041                                                                   
01042      MOVE SPACE               TO WS-EXCLUSION-SWITCH.             
01043                                                                   
01044      MOVE +1                  TO NDX.                             
01045                                                                   
01046  0250-PLAN-LOOP.                                                  
01047                                                                   
01048      IF NDX GREATER THAN +20                                      
01049         MOVE +1                 TO NDX                            
01050         GO TO 025A-EDIT-FOR-SPECIAL.                              
01051                                                                   
01052      IF AM-BENEFIT-CODE (NDX)  EQUAL EP-BEN-CODE AND              
01053         AM-BENEFIT-TYPE (NDX)  EQUAL EP-RCD-TYPE                  
01054           MOVE AM-BENEFIT-REVISION (NDX) TO WS-PL-REVISION-NO     
01055           MOVE WS-PLAN-CONTROL     TO PL-CONTROL-PRIMARY          
01056           GO TO 0250-READ-PLAN                                    
01057      ELSE                                                         
01058         ADD +1                TO NDX                              
01059         GO TO 0250-PLAN-LOOP.                                     
01060                                                                   
01061  0250-READ-PLAN.                                                  
01062                                                                   
01063      IF AM-BENEFIT-RETRO-Y-N (NDX) = 'N'                          
01064          MOVE '*'              TO WS-EXCLUSION-SWITCH             
01065          GO TO 0250-EXIT.                                         
01066                                                                   
01067      READ ERPLAN-IN.
01068                                                                   
01069      IF ERPLAN-FILE-STATUS  EQUAL '00'                            
01070          NEXT SENTENCE                                            
01071      ELSE                                                         
01072         IF ERPLAN-FILE-STATUS  EQUAL '23'                         
01073             GO TO 0250-PLAN-NOT-FOUND                             
01074         ELSE                                                      
01075             MOVE ERPLAN-FILE-STATUS  TO WS-ABEND-FILE-STATUS      
01076             MOVE 'READ ERROR - ERPLAN ' TO WS-ABEND-MESSAGE       
01077             DISPLAY '**BAD PLAN FILE READ'                        
01078             DISPLAY '**ERPLAN-FILE-STATUS= ' ERPLAN-FILE-STATUS   
01079             GO TO ABEND-PGM.                                      
01080                                                                   
01081      IF EP-COMPANY-CD  EQUAL  PL-COMPANY-CD   AND                 
01082         EP-CNTRL-1     EQUAL  PL-CONTROL-A    AND                 
01083         EP-RCD-TYPE    EQUAL  PL-BENEFIT-TYPE AND                 
01084         AM-BENEFIT-CODE (NDX)  EQUAL  PL-BENEFIT-CODE             
01085            NEXT SENTENCE                                          
01086      ELSE                                                         
01087            GO TO 0250-PLAN-NOT-FOUND.                             
01088                                                                   
01089      IF AM-RET-Y-N = ' '   OR  'N'                                
01090          MOVE +1.0 TO LIFE-RETENT AH-RETENT                       
01091          GO TO 0250-EXIT.                                         
01092                                                                   
01093      IF PL-RETRO-RET EQUAL ZEROS                                  
01094          GO TO 0250-PLAN-NOT-FOUND.                               
01095                                                                   
01096      IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                            
01097          COMPUTE LIFE-RETENT = 1 - PL-RETRO-RET                   
01098          GO TO 0250-EXIT                                          
01099      ELSE                                                         
01100          COMPUTE AH-RETENT   = 1 - PL-RETRO-RET                   
01101          GO TO 0250-EXIT.                                         
01102                                                                   
01103  025A-EDIT-FOR-SPECIAL.                                           
01104                                                                   
01105      IF (AM-BENEFIT-CODE (NDX) EQUAL '99')                        
01106       AND                                                         
01107         (AM-BENEFIT-TYPE (NDX) EQUAL EP-RCD-TYPE)                 
01108           MOVE AM-BENEFIT-REVISION (NDX) TO WS-PL-REVISION-NO     
01109           MOVE AM-BENEFIT-CODE (NDX)     TO WS-PL-BENEFIT-CODE    
01110           MOVE WS-PLAN-CONTROL           TO PL-CONTROL-PRIMARY    
01111           GO TO 0250-READ-PLAN.                                   
01112                                                                   
01113      IF (AM-BENEFIT-CODE (NDX) EQUAL '91')                        
01114       AND                                                         
01115         (AM-BENEFIT-TYPE (NDX) EQUAL LIFE-OVERRIDE-L1)            
01116         AND                                                       
01117           AM-BENEFIT-TYPE (NDX) EQUAL EP-RCD-TYPE                 
01118           IF CLAS-CO-BEN-I-G-CD (CLAS-INDEXL) EQUAL 'I'           
01119               AND CLAS-I-RL-AH (CLAS-INDEXL) EQUAL 'R'            
01120           MOVE AM-BENEFIT-REVISION (NDX) TO WS-PL-REVISION-NO     
01121           MOVE AM-BENEFIT-CODE (NDX)     TO WS-PL-BENEFIT-CODE    
01122           MOVE WS-PLAN-CONTROL           TO PL-CONTROL-PRIMARY    
01123           GO TO 0250-READ-PLAN.                                   
01124                                                                   
01125      IF (AM-BENEFIT-CODE (NDX) EQUAL '91')                        
01126       AND                                                         
01127         (AM-BENEFIT-TYPE (NDX) EQUAL AH-OVERRIDE-L1)              
01128       AND                                                         
01129         (AM-BENEFIT-TYPE (NDX) EQUAL EP-RCD-TYPE)                 
01130          IF CLAS-CO-BEN-I-G-CD (CLAS-INDEXL) EQUAL 'I'            
01131             MOVE AM-BENEFIT-REVISION (NDX) TO WS-PL-REVISION-NO   
01132             MOVE AM-BENEFIT-CODE (NDX)   TO WS-PL-BENEFIT-CODE    
01133             MOVE WS-PLAN-CONTROL         TO PL-CONTROL-PRIMARY    
01134             GO TO 0250-READ-PLAN.                                 
01135                                                                   
01136      IF (AM-BENEFIT-CODE (NDX) EQUAL '92')                        
01137       AND                                                         
01138         (AM-BENEFIT-TYPE (NDX) EQUAL LIFE-OVERRIDE-L1)            
01139       AND                                                         
01140         (AM-BENEFIT-TYPE (NDX) EQUAL EP-RCD-TYPE)                 
01141          IF (CLAS-CO-BEN-I-G-CD (CLAS-INDEXL) EQUAL 'G')          
01142            AND (CLAS-I-RL-AH (CLAS-INDEXL) EQUAL 'R')             
01143              MOVE AM-BENEFIT-REVISION (NDX) TO WS-PL-REVISION-NO  
01144              MOVE AM-BENEFIT-CODE (NDX)   TO WS-PL-BENEFIT-CODE   
01145              MOVE WS-PLAN-CONTROL         TO PL-CONTROL-PRIMARY   
01146              GO TO 0250-READ-PLAN.                                
01147                                                                   
01148      IF (AM-BENEFIT-CODE (NDX) EQUAL '92')                        
01149       AND                                                         
01150         (AM-BENEFIT-TYPE (NDX) EQUAL AH-OVERRIDE-L1)              
01151       AND                                                         
01152         (AM-BENEFIT-TYPE (NDX) EQUAL EP-RCD-TYPE)                 
01153          IF (CLAS-CO-BEN-I-G-CD (CLAS-INDEXL) EQUAL 'G')          
01154              MOVE AM-BENEFIT-REVISION (NDX) TO WS-PL-REVISION-NO  
01155              MOVE AM-BENEFIT-CODE (NDX)   TO WS-PL-BENEFIT-CODE   
01156              MOVE WS-PLAN-CONTROL         TO PL-CONTROL-PRIMARY   
01157              GO TO 0250-READ-PLAN.                                
01158                                                                   
01159      IF (AM-BENEFIT-CODE (NDX) EQUAL '93')                        
01160       AND                                                         
01161         (AM-BENEFIT-TYPE (NDX) EQUAL LIFE-OVERRIDE-L1)            
01162       AND                                                         
01163         (AM-BENEFIT-TYPE (NDX) EQUAL EP-RCD-TYPE)                 
01164          IF (CLAS-CO-BEN-I-G-CD (CLAS-INDEXL) EQUAL 'I')          
01165            AND (CLAS-I-RL-AH (CLAS-INDEXL) EQUAL 'P' OR 'L')      
01166              MOVE AM-BENEFIT-REVISION (NDX) TO WS-PL-REVISION-NO  
01167              MOVE AM-BENEFIT-CODE (NDX)   TO WS-PL-BENEFIT-CODE   
01168              MOVE WS-PLAN-CONTROL         TO PL-CONTROL-PRIMARY   
01169              GO TO 0250-READ-PLAN.                                
01170                                                                   
01171      IF (AM-BENEFIT-CODE (NDX) EQUAL '94')                        
01172       AND                                                         
01173         (AM-BENEFIT-TYPE (NDX) EQUAL LIFE-OVERRIDE-L1)            
01174       AND                                                         
01175         (AM-BENEFIT-TYPE (NDX) EQUAL EP-RCD-TYPE)                 
01176          IF (CLAS-CO-BEN-I-G-CD (CLAS-INDEXL) EQUAL 'G')          
01177            AND (CLAS-I-RL-AH (CLAS-INDEXL) EQUAL 'P' OR 'L')      
01178              MOVE AM-BENEFIT-REVISION (NDX) TO WS-PL-REVISION-NO  
01179              MOVE AM-BENEFIT-CODE (NDX)   TO WS-PL-BENEFIT-CODE   
01180              MOVE WS-PLAN-CONTROL         TO PL-CONTROL-PRIMARY   
01181              GO TO 0250-READ-PLAN.                                
01182                                                                   
01183      IF (AM-BENEFIT-CODE (NDX) EQUAL '98')                        
01184       AND                                                         
01185         (AM-BENEFIT-TYPE (NDX) EQUAL EP-RCD-TYPE)                 
01186              MOVE AM-BENEFIT-REVISION (NDX) TO WS-PL-REVISION-NO  
01187              MOVE AM-BENEFIT-CODE (NDX)   TO WS-PL-BENEFIT-CODE   
01188              MOVE WS-PLAN-CONTROL         TO PL-CONTROL-PRIMARY   
01189              GO TO 0250-READ-PLAN.                                
01190                                                                   
01191      IF NDX NOT LESS THAN +20                                     
01192        GO TO 0250-PLAN-NOT-FOUND                                  
01193      ELSE                                                         
01194        ADD  +1                 TO NDX                             
01195        GO TO 025A-EDIT-FOR-SPECIAL.                               
01196                                                                   
01197                                                                   
01198  0250-PLAN-NOT-FOUND.                                             
01199      IF AM-RET-Y-N = ' '   OR  'N'                                
01200          MOVE +1.0 TO LIFE-RETENT AH-RETENT                       
01201          GO TO 0250-EXIT                                          
01202      ELSE                                                         
01203          COMPUTE LIFE-RETENT = 1 - AM-LF-RET                      
01204          COMPUTE AH-RETENT   = 1 - AM-AH-RET                      
01205          GO TO 0250-EXIT.                                         
01206                                                                   
01207  0250-EXIT.                                                       
01208      EXIT.                                                        
01209                                                                   
01210  EJECT                                                            
01211  0260-READ-ACCT.                                                  
01212      READ ACC-MSTR                                                
01213              AT END MOVE HIGH-VALUES TO WS-ACCT-CONTROL           
01214                     GO TO 0260-EXIT.                              
01215                                                                   
01216      COPY ELCACCTI.                                               
01217                                                                   
01218      MOVE AM-MSTR-CNTRL TO WS-MSTR-CNTRL.                         
01219      MOVE AM-EFFECT-DT  TO WS-EFFECT.                             
01220                                                                   
01221                                                                   
01222      IF AM-RET-ST-TAX-USE = 'Y' OR 'P' OR 'E'                     
01223          MOVE EXPENSE-LINE TO WS-EX-LINE.                         
01224                                                                   
01225      IF DTE-PGM-OPT  = '5' AND                                    
01226         AM-AN-MO NOT = RUN-MO                                     
01227          MOVE SPACES TO AM-RETRO-POOL                             
01228          MOVE +0.0   TO AM-LF-RET AM-AH-RET                       
01229          MOVE 'N'    TO AM-RET-Y-N.                               
01230                                                                   
01231      IF DTE-CLIENT = 'GIC'                                        
01232          MOVE 'P' TO AM-RET-P-E.                                  
01233      IF AM-RETRO-QUALIFY-LIMIT NOT NUMERIC  OR                    
01234         AM-RETRO-QUALIFY-LIMIT = ZEROS                            
01235          MOVE -9999999         TO AM-RETRO-QUALIFY-LIMIT.         
01236                                                                   
01237      IF AM-RET-P-E NOT = 'P'  AND  'E'                            
01238          MOVE 'E'              TO AM-RET-P-E.                     
01239      IF AM-RETRO-PREM-P-E NOT = 'P'  AND  'E'                     
01240          MOVE AM-RET-P-E       TO AM-RETRO-PREM-P-E.              
01241      IF AM-RETRO-CLMS-P-I NOT = 'P'  AND  'I'                     
01242          MOVE 'I'              TO AM-RETRO-CLMS-P-I.              
01243      IF AM-RETRO-RET-METHOD-LF NOT = 'P'  AND  'S'                
01244          MOVE 'P'              TO AM-RETRO-RET-METHOD-LF.         
01245      IF AM-RETRO-RET-BASIS-LF NOT = 'P'  AND  'E'                 
01246          MOVE 'P'              TO AM-RETRO-RET-BASIS-LF.          
01247      IF AM-RETRO-RET-METHOD-AH NOT = 'P'  AND  'S'  AND  'L'      
01248          MOVE 'P'              TO AM-RETRO-RET-METHOD-AH.         
01249      IF AM-RETRO-RET-BASIS-AH NOT = 'P'  AND  'E'                 
01250          MOVE 'P'              TO AM-RETRO-RET-BASIS-AH.          
01251                                                                   
01252      IF AM-RETRO-USE-LIFE-METHOD                                  
01253          MOVE AM-RETRO-RET-BRACKET-LF TO AM-RETRO-RET-BRACKET-AH. 
01254                                                                   
01255  0260-EXIT.                                                       
01256      EXIT.                                                        
01257                                                                   
01258  EJECT                                                            
01259  0270-NO-ACCT-MSG.                                                
01260      IF LINE-CNT GREATER +55                                      
01261          MOVE ZERO   TO LINE-CNT                                  
01262          MOVE '1'    TO X                                         
01263          MOVE HEAD-1 TO P-DATA                                    
01264          PERFORM 0610-PRT-RTN THRU 0630-EXIT                      
01265          MOVE ' '    TO X                                         
01266          MOVE HEAD-2 TO P-DATA                                    
01267          PERFORM 0610-PRT-RTN THRU 0630-EXIT
060402         ADD +1               TO PAGE-CNT
060402         MOVE PAGE-CNT        TO H3-PAGE
01268          MOVE ' '    TO X                                         
01269          MOVE HEAD-3 TO P-DATA                                    
01270          PERFORM 0610-PRT-RTN THRU 0630-EXIT                      
01271          MOVE ' '    TO X                                         
01272          MOVE HEAD-4 TO P-DATA                                    
01273          PERFORM 0610-PRT-RTN THRU 0630-EXIT                      
01274          MOVE ' '    TO X                                         
01275          MOVE SPACES TO P-DATA                                    
01276          PERFORM 0610-PRT-RTN THRU 0630-EXIT.                     
01277                                                                   
01278      MOVE WS-SAVE-CARR     TO H5-CARR.                            
01279      MOVE WS-SAVE-COMP     TO H5-COMP.                            
01280      MOVE WS-SAVE-STATE    TO H5-ST.                              
01281      MOVE WS-SAVE-ACCT     TO H5-ACCT.                            
01282      MOVE WS-SAVE-EXP-DATE TO WORK-DATE.                          
01283      MOVE WK-YR            TO H5-TYR.                             
01284      MOVE WK-MO            TO H5-TMO.                             
01285      MOVE WK-DA            TO H5-TDA.                             
01286      MOVE WS-SAVE-EFF-DATE TO WORK-DATE.                          
01287      MOVE WK-YR            TO H5-FYR.                             
01288      MOVE WK-MO            TO H5-FMO.                             
01289      MOVE WK-DA            TO H5-FDA.                             
01290      MOVE ' NO MATCHING ACCOUNT MASTER' TO H5-MESSAGE.            
01291      MOVE ' '              TO X.                                  
01292      MOVE HEAD-5           TO P-DATA.                             
01293      PERFORM 0610-PRT-RTN THRU 0630-EXIT.                         
01294                                                                   
01295  EJECT                                                            
01296  0280-CALCULATE-RETRO.                                            
01297 ******************************************************************
01298 *              SPECIAL LOSS-RESERVE CALCULATION FOR OLI           
01299                                                                   
01300      IF DTE-CLIENT NOT = 'POS'                                    
01301          GO TO 0290-SKIP-CC.                                      
01302                                                                   
01303      IF AM-CARRIER NOT = '1'                                      
01304          GO TO 0290-SKIP-CC.                                      
01305                                                                   
01306      IF AM-FLD-5 NOT = 'CC'                                       
01307          GO TO 0290-SKIP-CC.                                      
01308                                                                   
01309      COMPUTE DA-LRV (1) = (+.40 * ((DA-ISP (1) - DA-CNP (1))      
01310                      - DA-UP (1))) - DA-CLM (1).                  
01311      COMPUTE DA-LRV (2) = (+.40 * ((DA-ISP (2) - DA-CNP (2))      
01312                      - DA-UP (2))) - DA-CLM (2).                  
01313      COMPUTE DA-LRV (3) = (+.40 * ((DA-ISP (3) - DA-CNP (3))      
01314                      - DA-UP (3))) - DA-CLM (3).                  
01315      COMPUTE DA-LRV (4) = (+.40 * ((DA-ISP (4) - DA-CNP (4))      
01316                      - DA-UP (4))) - DA-CLM (4).                  
01317                                                                   
01318      IF DA-LRV (1) LESS ZERO                                      
01319          ADD DA-LRV (1) TO DA-LRV (2)                             
01320          MOVE ZEROS TO DA-LRV (1)                                 
01321          IF DA-LRV (2) LESS ZERO                                  
01322              MOVE ZEROS TO DA-LRV (2).                            
01323                                                                   
01324      IF DA-LRV (2) LESS ZERO                                      
01325          ADD DA-LRV (2) TO DA-LRV (1)                             
01326          MOVE ZEROS TO DA-LRV (2)                                 
01327          IF DA-LRV (1) LESS ZERO                                  
01328              MOVE ZEROS TO DA-LRV (1).                            
01329                                                                   
01330      IF DA-LRV (3) LESS ZERO                                      
01331          ADD DA-LRV (3) TO DA-LRV (4)                             
01332          MOVE ZEROS TO DA-LRV (3)                                 
01333          IF DA-LRV (4) LESS ZERO                                  
01334              MOVE ZEROS TO DA-LRV (4).                            
01335                                                                   
01336      IF DA-LRV (4) LESS ZERO                                      
01337          ADD DA-LRV (4) TO DA-LRV (3)                             
01338          MOVE ZEROS TO DA-LRV (4)                                 
01339          IF DA-LRV (3) LESS ZERO                                  
01340              MOVE ZEROS TO DA-LRV (3).                            
01341                                                                   
01342  EJECT                                                            
01343 ******************************************************************
01344 **       END OF SPECIAL OLI CODING                                
01345 ******************************************************************
01346                                                                   
01347  0290-SKIP-CC.                                                    
01348                                                                   
01349  EJECT                                                            
01350  0300-CALC-RETRO-A.                                               
01351                                                                   
01352      MOVE +1 TO SA.                                               
01353      MOVE +3 TO SB.                                               
01354      MOVE +1 TO SC.                                               
01355      MOVE +3 TO SF.                                               
01356      PERFORM 0380-COMPUTE-YTD THRU 0390-EXIT.                     
01357      MOVE +2 TO SA.                                               
01358      MOVE +4 TO SB.                                               
01359      MOVE +2 TO SC.                                               
01360      PERFORM 0380-COMPUTE-YTD THRU 0390-EXIT.                     
01361      MOVE +3 TO SB.                                               
01362      MOVE +4 TO SC.                                               
01363      MOVE +6 TO SF.                                               
01364      PERFORM 0400-COMPUTE-ITD THRU 0410-EXIT.                     
01365      MOVE +4 TO SB.                                               
01366      MOVE +5 TO SC.                                               
01367      PERFORM 0400-COMPUTE-ITD THRU 0410-EXIT.                     
01368      MOVE SPACES       TO SW-REC.                                 
01369      MOVE 'A'          TO SW-RPT-TYPE-ID.                         
01370      MOVE AM-NAME      TO SW-NAME.                                
01371      MOVE AM-ADDRS     TO SW-ADDRS.                               
01372      MOVE AM-CITY      TO SW-CITY.                                
01373      MOVE AM-ZIP       TO SW-ZIP.                                 
01374      MOVE AM-EXPIRE-DT TO SW-EXP-DT.                              
01375      MOVE AM-EFFECT-DT TO SW-EFF-DT.                              
01376                                                                   
01377      IF PA-EP (4) NOT EQUAL ZEROS                                 
01378         COMPUTE LIFE-RETENT ROUNDED =                             
01379             PA-PE (4) / PA-EP (4)                                 
01380      ELSE                                                         
01381         MOVE +0.0      TO LIFE-RETENT.                            
01382                                                                   
01383      IF PA-EP (5) NOT EQUAL ZEROS                                 
01384         COMPUTE AH-RETENT  ROUNDED =                              
01385             PA-PE (5) / PA-EP (5)                                 
01386      ELSE                                                         
01387         MOVE +0.0      TO AH-RETENT.                              
01388                                                                   
01389      MOVE LIFE-RETENT  TO SW-LRET.                                
01390      MOVE AH-RETENT    TO SW-ARET.                                
01391      MOVE AM-RET-Y-N   TO SW-RET-Y-N.                             
01392      MOVE AM-RET-P-E   TO SW-RET-TYP.                             
01393      MOVE AM-RETRO-PREM-P-E TO SW-PREM-P-E.                       
01394      MOVE AM-RETRO-CLMS-P-I TO SW-CLMS-P-I.                       
01395      MOVE AM-RETRO-QUALIFY-LIMIT   TO SW-RETRO-LIMIT.             
01396      MOVE PRINT-ACCUMS-R (1) TO SW-ACCUMS.                        
01397      MOVE AM-CARRIER   TO SW-CARR.                                
01398      MOVE AM-GROUPING  TO SW-COMP.                                
01399      MOVE AM-STATE     TO SW-STATE.                               
01400      MOVE AM-ACCOUNT   TO SW-ACCT.                                
01401      MOVE BEG-DATE     TO SW-BEG-DATE.                            
01402                                                                   
01403  0310-EXIT.                                                       
01404      EXIT.                                                        
01405                                                                   
01406  EJECT                                                            
01407  0320-CONT-RETRO-A.                                               
01408      IF AM-RET-Y-N = 'Y' OR 'I' OR 'G' OR 'L' OR 'D'              
01409          RELEASE SW-REC-OUT FROM SW-REC                           
01410          ADD +1 TO NO-OF-RECORDS-RELEASED.                        
01411                                                                   
01412      IF AM-RETRO-POOL NOT = SPACES AND NOT = ZEROS                
01413          MOVE AM-RETRO-POOL TO SW-GA                              
01414          MOVE 'B'           TO SW-RPT-TYPE-ID                     
01415          IF DTE-CLIENT NOT = 'VSL'                                
01416              RELEASE SW-REC-OUT FROM SW-REC                       
01417              ADD +1 TO NO-OF-RECORDS-RELEASED                     
01418          ELSE                                                     
01419              IF AM-RET-Y-N = 'Y' OR 'I' OR 'G' OR 'L' OR 'D'      
01420                  RELEASE SW-REC-OUT FROM SW-REC                   
01421                  ADD +1 TO NO-OF-RECORDS-RELEASED.                
01422                                                                   
01423      IF DTE-CLIENT = 'TAO'                                        
01424          MOVE AM-USER-FIELDS TO Z-G                               
01425          MOVE ZONE-GROUP     TO SW-GA                             
01426          MOVE 'B'            TO SW-RPT-TYPE-ID                    
01427          RELEASE SW-REC-OUT FROM SW-REC                           
01428          ADD +1             TO NO-OF-RECORDS-RELEASED.            
01429                                                                   
01430      MOVE ZEROS TO DA-ISC-ADDL (1) DA-CNC-ADDL (1) DA-UC-ADDL (1) 
01431                    DA-ISC-ADDL (2) DA-CNC-ADDL (2) DA-UC-ADDL (2) 
01432                    DA-ISC-ADDL (3) DA-CNC-ADDL (3) DA-UC-ADDL (3) 
01433                    DA-ISC-ADDL (4) DA-CNC-ADDL (4) DA-UC-ADDL (4).
01434                                                                   
01435      PERFORM 0340-GA-EXT-LOOP THRU 0350-EXIT                      
01436          VARYING CL FROM 1 BY 1 UNTIL CL GREATER EC-REC-MAX.      
01437                                                                   
01438      IF AM-GPCD = 98                                              
01439          MOVE AM-ACCOUNT  TO SW-GA                                
01440          MOVE 'X'         TO SW-GA-FLAG                           
01441          MOVE HIGH-VALUES TO SW-CARR SW-COMP SW-STATE             
01442          MOVE 'C'         TO SW-RPT-TYPE-ID                       
01443          RELEASE SW-REC-OUT FROM SW-REC                           
01444          ADD +1 TO NO-OF-RECORDS-RELEASED.                        
01445                                                                   
01446      PERFORM 0550-ZERO-DA VARYING SA FROM 1 BY 1                  
01447                           UNTIL SA GREATER 4.                     
01448      PERFORM 0560-ZERO-PA VARYING SB FROM 1 BY 1                  
01449                           UNTIL SB GREATER 6.                     
01450      PERFORM 0570-ZERO-XDA VARYING CL FROM 1 BY 1                 
01451                           UNTIL CL GREATER 10.                    
01452                                                                   
01453  0330-EXIT.                                                       
01454      EXIT.                                                        
01455                                                                   
01456  EJECT                                                            
01457  0340-GA-EXT-LOOP.                                                
01458      IF XDA-GA-TYPE (CL) = 'O' OR 'P' OR 'G' OR 'B'               
01459          NEXT SENTENCE                                            
01460       ELSE                                                        
01461          GO TO 0350-EXIT.                                         
01462                                                                   
01463      MOVE XDA-GA-ISC (CL, 1) TO DA-GA-ISC (1).                    
01464      MOVE XDA-GA-ISC (CL, 2) TO DA-GA-ISC (2).                    
01465      MOVE XDA-GA-ISC (CL, 3) TO DA-GA-ISC (3).                    
01466      MOVE XDA-GA-ISC (CL, 4) TO DA-GA-ISC (4).                    
01467      MOVE XDA-GA-CNC (CL, 1) TO DA-GA-CNC (1).                    
01468      MOVE XDA-GA-CNC (CL, 2) TO DA-GA-CNC (2).                    
01469      MOVE XDA-GA-CNC (CL, 3) TO DA-GA-CNC (3).                    
01470      MOVE XDA-GA-CNC (CL, 4) TO DA-GA-CNC (4).                    
01471      MOVE XDA-GA-UC  (CL, 1) TO DA-GA-UC  (1).                    
01472      MOVE XDA-GA-UC  (CL, 2) TO DA-GA-UC  (2).                    
01473      MOVE XDA-GA-UC  (CL, 3) TO DA-GA-UC  (3).                    
01474      MOVE XDA-GA-UC  (CL, 4) TO DA-GA-UC  (4).                    
01475                                                                   
01476      MOVE 3 TO SB.                                                
01477      PERFORM 0560-ZERO-PA.                                        
01478      MOVE 6 TO SB.                                                
01479      PERFORM 0560-ZERO-PA.                                        
01480      PERFORM 0300-CALC-RETRO-A THRU 0310-EXIT.                    
01481      MOVE XDA-GA-NO (CL)         TO SW-GA.                        
01482      MOVE 'C'                    TO SW-RPT-TYPE-ID.               
01483      RELEASE SW-REC-OUT FROM SW-REC.                              
01484      ADD +1 TO NO-OF-RECORDS-RELEASED.                            
01485                                                                   
01486  0350-EXIT.                                                       
01487      EXIT.                                                        
01488                                                                   
01489  EJECT                                                            
01490  0360-BUILD-DUMMY-GA.                                             
01491                                                                   
01492      MOVE SPACES       TO SW-REC.                                 
01493      MOVE -9999999     TO SW-RETRO-LIMIT.                         
01494      MOVE 'X'          TO SW-GA-FLAG.                             
01495      MOVE 'C'          TO SW-RPT-TYPE-ID.                         
01496      MOVE AM-NAME      TO SW-NAME.                                
01497      MOVE AM-ADDRS     TO SW-ADDRS.                               
01498      MOVE AM-CITY      TO SW-CITY.                                
01499      MOVE AM-ZIP       TO SW-ZIP.                                 
01500      MOVE AM-EXPIRE-DT TO SW-EXP-DT.                              
01501      MOVE AM-EFFECT-DT TO SW-EFF-DT.                              
01502                                                                   
01503      IF AM-RET-Y-N = ' '   OR  'N'                                
01504          MOVE +1.0 TO SW-LRET SW-ARET                             
01505      ELSE                                                         
01506          COMPUTE SW-LRET = 1 - AM-LF-RET                          
01507          COMPUTE SW-ARET = 1 - AM-AH-RET.                         
01508                                                                   
01509      MOVE AM-RET-P-E         TO SW-RET-TYP.                       
01510      MOVE PRINT-ACCUMS-R (6) TO SW-ACCUMS.                        
01511      MOVE HIGH-VALUES        TO SW-CARR SW-COMP SW-STATE.         
01512      MOVE AM-ACCOUNT         TO SW-ACCT SW-GA.                    
01513      RELEASE SW-REC-OUT FROM SW-REC.                              
01514      ADD +1 TO NO-OF-RECORDS-RELEASED.                            
01515                                                                   
01516  0370-EXIT.                                                       
01517      EXIT.                                                        
01518                                                                   
01519  EJECT                                                            
01520  0380-COMPUTE-YTD.                                                
01521      COMPUTE PA-NW (SC) = (DA-ISP (SB) - DA-CNP (SB)) -           
01522                           (DA-ISP (SA) - DA-CNP (SA)).            
01523                                                                   
01524      IF AM-RETRO-PREM-P-E = 'E'                                   
01525          MOVE DA-UP (SA) TO PA-BR (SC)                            
01526          MOVE DA-UP (SB) TO PA-ER (SC).                           
01527                                                                   
01528      COMPUTE PA-EP (SC) = PA-NW (SC) + PA-BR (SC) - PA-ER (SC).   
01529                                                                   
01530      COMPUTE PA-PE (SC) = DA-PCT-E-PREM (SB) - DA-PCT-E-PREM (SA).
01531                                                                   
01532      COMPUTE PA-PL (SC) = DA-CLM (SB) - DA-CLM (SA).              
01533                                                                   
01534      IF AM-RETRO-CLMS-P-I = 'I'                                   
01535          MOVE DA-LRV (SA) TO PA-BLR (SC)                          
01536          MOVE DA-LRV (SB) TO PA-ELR (SC).                         
01537                                                                   
01538      IF DTE-CLIENT = 'ABL'                                        
01539          COMPUTE PA-BLR (SC) =   DA-LRV (SA) +                    
01540          ((((DA-ISP (SB) - DA-ISP (SA)) -                         
01541             (DA-CNP (SB) - DA-CNP (SA))) - DA-UP (SA)) * .25)     
01542          COMPUTE PA-ELR (SC) =   DA-LRV (SB) + (PA-EP (SC) * .25).
01543                                                                   
01544      IF DTE-CLIENT = 'GIC'                                        
01545          IF SC = +1                                               
01546              COMPUTE PA-BLR (SC) =  PA-BR (SC) * .00              
01547              COMPUTE PA-ELR (SC) =  PA-ER (SC) * .00              
01548          ELSE                                                     
01549              COMPUTE PA-BLR (SC) =  PA-BR (SC) * .12              
01550              COMPUTE PA-ELR (SC) =  PA-ER (SC) * .12.             
01551                                                                   
01552      IF AM-RET-MIN-LOSS-L NOT NUMERIC                             
01553          MOVE +0 TO AM-RET-MIN-LOSS-L.                            
01554                                                                   
01555      IF AM-RET-MIN-LOSS-A NOT NUMERIC                             
01556          MOVE +0 TO AM-RET-MIN-LOSS-A.                            
01557                                                                   
01558      IF SC = +1                                                   
01559          COMPUTE WS-MIN-LOSS ROUNDED =                            
01560             (PA-EP (SC) * AM-RET-MIN-LOSS-L) - PA-PL (SC)         
01561                                               + PA-BLR (SC)       
01562         ELSE                                                      
01563          COMPUTE WS-MIN-LOSS ROUNDED =                            
01564             (PA-EP (SC) * AM-RET-MIN-LOSS-A) - PA-PL (SC)         
01565                                               + PA-BLR (SC).      
01566      IF SC = +1                                                   
01567          IF AM-RET-MIN-LOSS-L = ZERO                              
01568             MOVE ZERO TO WS-MIN-LOSS.                             
01569                                                                   
01570      IF SC = +2                                                   
01571          IF AM-RET-MIN-LOSS-A = ZERO                              
01572             MOVE ZERO TO WS-MIN-LOSS.                             
01573                                                                   
01574      IF WS-MIN-LOSS GREATER PA-ELR (SC)                           
01575          MOVE WS-MIN-LOSS TO PA-ELR (SC).                         
01576                                                                   
01577      COMPUTE PA-TL (SC) = PA-PL (SC) - PA-BLR (SC) + PA-ELR (SC). 
01578                                                                   
01579      COMPUTE PA-RC (SC) = ((DA-ISC (SB) - DA-CNC (SB)) -          
01580                            (DA-ISC (SA) - DA-CNC (SA))) +         
01581                           ((DA-ISC-ADDL (SB) - DA-CNC-ADDL (SB)) -
01582                            (DA-ISC-ADDL (SA) - DA-CNC-ADDL (SA))).
01583                                                                   
01584      COMPUTE PA-EC (SC) = PA-RC (SC) + (DA-UC (SA) - DA-UC (SB))  
01585                          + (DA-UC-ADDL (SA) - DA-UC-ADDL (SB)).   
01586                                                                   
01587      IF AM-RET-P-E = 'P'                                          
01588          MOVE PA-RC (SC) TO PA-CU (SC)                            
01589       ELSE                                                        
01590          MOVE PA-EC (SC) TO PA-CU (SC).                           
01591                                                                   
01592      COMPUTE PA-OC (SC) = DA-OTC (SB) - DA-OTC (SA).              
01593      COMPUTE PA-TC (SC) = PA-CU (SC) + PA-OC (SC).                
01594      COMPUTE PA-CP (SC) = DA-CPS (SB) - DA-CPS (SA).              
01595                                                                   
01596      IF DTE-CLIENT = 'FLA'                                        
01597          COMPUTE PA-EX (SC) = PA-NW (SC) * .11                    
01598       ELSE                                                        
01599          COMPUTE PA-EX (SC) = DA-EXP (SB) - DA-EXP (SA).          
01600                                                                   
01601      IF AM-RET-ST-TAX-USE = 'Y' OR 'P' OR 'E'                     
01602          IF (AM-RET-P-E = 'P') OR (AM-RET-ST-TAX-USE = 'P')       
01603               IF SC = 1                                           
01604                  COMPUTE PA-EX (SC) =  PA-EX (SC) +               
PEMMOD*                          (PA-NW (SC) * AM-REI-LF-TAX)           
PEMMOD                    (DA-TAX (SB) - DA-TAX (SA))                   
01606                ELSE                                               
01607                  COMPUTE PA-EX (SC) =  PA-EX (SC) +               
PEMMOD*                          (PA-NW (SC) * AM-REI-AH-TAX)           
PEMMOD                    (DA-TAX (SB) - DA-TAX (SA))                   
01609           ELSE                                                    
01610                  IF SC = 1                                        
01611                     COMPUTE PA-EX (SC) =  PA-EX (SC) +            
PEMMOD*                             (PA-EP (SC) * AM-REI-LF-TAX)        
PEMMOD                       (PA-EP (SC) / PA-NW (SC)) *
PEMMOD                       (DA-TAX (SB) - DA-TAX (SA))
01613                   ELSE                                            
01612                     COMPUTE PA-EX (SC) =  PA-EX (SC) +            
PEMMOD*                             (PA-EP (SC) * AM-REI-AH-TAX).       
PEMMOD                       (PA-EP (SC) / PA-NW (SC)) *
PEMMOD                       (DA-TAX (SB) - DA-TAX (SA))
01616                                                                   
01617      COMPUTE PA-TE (SC) = PA-TL (SC) + PA-TC (SC)                 
01618                           + PA-CP (SC) + PA-EX (SC).              
01619      COMPUTE PA-BA (SC) = PA-PE (SC) - PA-TE (SC).                
01620      COMPUTE PA-NTE (SC) = PA-TL (SC) + PA-TC (SC) + PA-EX (SC).  
01621      COMPUTE PA-NBA (SC) = PA-PE (SC) - PA-NTE (SC).              
01622      COMPUTE PA-GARC (SC) = (DA-GA-ISC (SB) - DA-GA-CNC (SB)) -   
01623                    (DA-GA-ISC (SA) - DA-GA-CNC (SA)).             
01624      COMPUTE PA-GAEC (SC) = PA-GARC (SC) + DA-GA-UC (SA) -        
01625                                         DA-GA-UC (SB).            
01626      PERFORM 0520-ADD-PA-LEVELS.                                  
01627                                                                   
01628  0390-EXIT.                                                       
01629      EXIT.                                                        
01630                                                                   
01631  EJECT                                                            
01632  0400-COMPUTE-ITD.                                                
01633      COMPUTE PA-NW (SC) = DA-ISP (SB) - DA-CNP (SB).              
01634                                                                   
01635      IF AM-RETRO-PREM-P-E = 'E'                                   
01636          MOVE DA-UP (SB)       TO PA-ER (SC).                     
01637                                                                   
01638      COMPUTE PA-EP (SC) = PA-NW (SC) - PA-ER (SC).                
01639                                                                   
01640      MOVE DA-PCT-E-PREM (SB)   TO PA-PE (SC).                     
01641                                                                   
01642      MOVE DA-CLM (SB)          TO PA-PL (SC).                     
01643                                                                   
01644      IF AM-RETRO-CLMS-P-I = 'I'                                   
01645          MOVE DA-LRV (SB)      TO PA-ELR (SC).                    
01646                                                                   
01647      IF DTE-CLIENT = 'ABL'                                        
01648          COMPUTE PA-ELR (SC) = DA-LRV (SB) + (PA-EP (SC) * .25).  
01649                                                                   
01650      IF DTE-CLIENT = 'GIC'                                        
01651          IF SC = +4                                               
01652              COMPUTE PA-ELR (SC) =  PA-ER (SC) * .00              
01653          ELSE                                                     
01654              COMPUTE PA-ELR (SC) =  PA-ER (SC) * .12.             
01655                                                                   
01656      IF AM-RET-MIN-LOSS-L NOT NUMERIC                             
01657          MOVE +0 TO AM-RET-MIN-LOSS-L.                            
01658                                                                   
01659      IF AM-RET-MIN-LOSS-A NOT NUMERIC                             
01660          MOVE +0 TO AM-RET-MIN-LOSS-A.                            
01661                                                                   
01662      IF SC = +4                                                   
01663          COMPUTE WS-MIN-LOSS ROUNDED =                            
01664             (PA-EP (SC) * AM-RET-MIN-LOSS-L) - PA-PL (SC)         
01665      ELSE                                                         
01666          COMPUTE WS-MIN-LOSS ROUNDED =                            
01667             (PA-EP (SC) * AM-RET-MIN-LOSS-A) - PA-PL (SC).        
01668                                                                   
01669      IF SC = +4                                                   
01670          IF AM-RET-MIN-LOSS-L = ZERO                              
01671             MOVE ZERO TO WS-MIN-LOSS.                             
01672                                                                   
01673      IF SC = +5                                                   
01674          IF AM-RET-MIN-LOSS-A = ZERO                              
01675             MOVE ZERO TO WS-MIN-LOSS.                             
01676                                                                   
01677      IF WS-MIN-LOSS GREATER PA-ELR (SC)                           
01678          MOVE WS-MIN-LOSS TO PA-ELR (SC).                         
01679                                                                   
01680      COMPUTE PA-TL (SC) = PA-PL (SC) + PA-ELR (SC).               
01681                                                                   
01682                                                                   
01683      COMPUTE PA-RC (SC) = (DA-ISC (SB) - DA-CNC (SB)) +           
01684                           (DA-ISC-ADDL (SB) - DA-CNC-ADDL (SB)).  
01685      COMPUTE PA-EC (SC) =                                         
01686                   PA-RC (SC) - DA-UC (SB) - DA-UC-ADDL (SB).      
01687                                                                   
01688      IF AM-RET-P-E = 'P'                                          
01689          MOVE PA-RC (SC) TO PA-CU (SC)                            
01690      ELSE                                                         
01691          MOVE PA-EC (SC) TO PA-CU (SC).                           
01692                                                                   
01693      MOVE DA-OTC (SB) TO PA-OC (SC).                              
01694      COMPUTE PA-TC (SC) = PA-CU (SC) + PA-OC (SC).                
01695      MOVE DA-CPS (SB) TO PA-CP (SC).                              
01696                                                                   
01697      IF DTE-CLIENT = 'FLA'                                        
01698         COMPUTE PA-EX (SC) = PA-NW (SC) * .11                     
01699      ELSE                                                         
01700         MOVE DA-EXP (SB) TO PA-EX (SC)                            
PEMMOD     END-IF
01701                                                                   
PEMMOD*    MOVE 'P' TO AM-RET-ST-TAX-USE
PEMMOD*    MOVE 'P' TO AM-RET-P-E
01702      IF AM-RET-ST-TAX-USE = 'Y' OR 'P' OR 'E'                     
01703         IF (AM-RET-P-E = 'P') OR (AM-RET-ST-TAX-USE = 'P')        
01704            IF SC = 4                                              
01705               COMPUTE PA-EX (SC) =  PA-EX (SC) +                  
PEMMOD*                (PA-NW (SC) * AM-REI-LF-TAX)                     
PEMMOD                 DA-TAX (SB)                                      
01707            ELSE                                                   
01708               COMPUTE PA-EX (SC) =  PA-EX (SC) +                  
PEMMOD*                (PA-NW (SC) * AM-REI-AH-TAX)                     
PEMMOD                 DA-TAX (SB)                                      
PEMMOD           END-IF                                                 
01710         ELSE                                                      
01711            IF SC = 4                                              
01712               COMPUTE PA-EX (SC) =  PA-EX (SC) +                  
PEMMOD*                (PA-EP (SC) * AM-REI-LF-TAX)                     
PEMMOD                 ((PA-EP (SC) / PA-NW (SC)) * DA-TAX (SB))        
01714            ELSE                                                   
01715               COMPUTE PA-EX (SC) =  PA-EX (SC) +                  
PEMMOD*                (PA-EP (SC) * AM-REI-AH-TAX).                    
PEMMOD                 ((PA-EP (SC) / PA-NW (SC)) * DA-TAX (SB))        
PEMMOD           END-IF                                                 
PEMMOD        END-IF                                                    
PEMMOD     END-IF                                                       
01717                                                                   
01718      COMPUTE PA-TE (SC) = PA-TL (SC) + PA-TC (SC)                 
01719                         + PA-CP (SC) + PA-EX (SC).                
01720      COMPUTE PA-BA (SC) = PA-PE (SC) - PA-TE (SC).                
01721      COMPUTE PA-NTE (SC) = PA-TL (SC) + PA-TC (SC) + PA-EX (SC).  
01722      COMPUTE PA-NBA (SC) = PA-PE (SC) - PA-NTE (SC).              
01723      COMPUTE PA-GARC (SC) = DA-GA-ISC (SB) - DA-GA-CNC (SB).      
01724      COMPUTE PA-GAEC (SC) = PA-GARC (SC) - DA-GA-UC (SB).         
01725                                                                   
01726      PERFORM 0520-ADD-PA-LEVELS.                                  
01727                                                                   
01728  0410-EXIT.                                                       
01729      EXIT.                                                        
01730                                                                   
01731  EJECT                                                            
01732  0420-CALCULATE-GA.                                               
01733      COMPUTE PA-PE (SC) ROUNDED = PA-EP (SC) * RETENT-FACT.       
01734                                                                   
01735      IF SAVE-RET-TYP = 'E'                                        
01736          MOVE PA-GAEC (SC) TO PA-GACU (SC)                        
01737      ELSE                                                         
01738          MOVE PA-GARC (SC) TO PA-GACU (SC).                       
01739                                                                   
01740      COMPUTE PA-GATC (SC) = PA-TC (SC) + PA-GACU (SC).            
01741      COMPUTE PA-GATE (SC) = PA-NTE (SC) + PA-GACU (SC).           
01742      COMPUTE PA-GABA (SC) = PA-PE (SC) - PA-GATE (SC).            
01743      COMPUTE PA-GAGT (SC) = PA-GABA (SC) - PA-CP (SC)             
01744                                       - PA-NBA (SC).              
01745      ADD PA-GACU (SC) TO PA-GACU (SF).                            
01746      ADD PA-GATC (SC) TO PA-GATC (SF).                            
01747      ADD PA-GATE (SC) TO PA-GATE (SF).                            
01748      ADD PA-GABA (SC) TO PA-GABA (SF).                            
01749      ADD PA-GAGT (SC) TO PA-GAGT (SF).                            
01750                                                                   
01751  0430-EXIT.                                                       
01752                                                                   
01753  EJECT                                                            
01754  0440-GENERATE-RETRO.                                             
01755      IF DTE-PGM-OPT = '1'                                         
01756          AND H1-REPORT-SUF NOT = 'A'                              
01757      OR DTE-PGM-OPT = '2'                                         
01758          AND H1-REPORT-SUF NOT = 'B'                              
01759      OR DTE-PGM-OPT = '3'                                         
01760          AND H1-REPORT-SUF NOT = 'C'                              
01761              GO TO 0470-EXIT.                                     
01762                                                                   
01763      IF DTE-PGM-OPT = ('5' OR '6' OR '7') AND                     
01764         H1-REPORT-SUF NOT = ('A' AND 'B')                         
01765           GO TO 0470-EXIT.                                        
01766                                                                   
01767      PERFORM 0580-ZERO-PCNT VARYING SB FROM 1 BY 1                
01768                             UNTIL SB GREATER 2                    
01769        AFTER SC FROM 1 BY 1 UNTIL SC GREATER 27.                  
01770                                                                   
01771      MOVE +1 TO SE.                                               
01772      PERFORM 0480-COMPUTE-PERCENTS THRU 0490-EXIT.                
01773      MOVE +2 TO SE.                                               
01774      ADD +3 TO SF.                                                
01775                                                                   
01776      PERFORM 0480-COMPUTE-PERCENTS THRU 0490-EXIT.                
01777      PERFORM 0590-HEADING-ROUTINE  THRU 0600-EXIT.                
01778                                                                   
01779      IF HEAD-SW = '1' OR '2'                                      
01780          IF SAVE-RET-TYP = 'E'                                    
01781              MOVE '  1. EARNED '         TO DT-ENTRY (13)         
01782          ELSE                                                     
01783              MOVE '  1. RETAINED '       TO DT-ENTRY (13)         
01784      ELSE                                                         
01785          MOVE '  1. ACCOUNT COMMISSION ' TO DT-ENTRY (13).        
01786                                                                   
01787      MOVE +1 TO SB.                                               
01788                                                                   
01789  0450-PRINT-LOOP.                                                 
01790      IF DT-SS-R (SB SH) = SPACES                                  
01791          GO TO 0460-CHECK-NEXT-LINE.                              
01792                                                                   
01793      IF DT-SS-R (SB SH) = '00'                                    
01794          MOVE DT-CTL-CHAR (SB) TO X                               
01795          MOVE DT-ENTRY (SB)    TO DL-DESC                         
01796          MOVE DETAIL-LINE      TO P-DATA                          
01797          PERFORM 0610-PRT-RTN THRU 0630-EXIT                      
01798          GO TO 0460-CHECK-NEXT-LINE.                              
01799                                                                   
01800      MOVE DT-CTL-CHAR (SB) TO X.                                  
01801      MOVE DT-SS (SB SH)    TO SA.                                 
01802                                                                   
01803      PERFORM 0500-PRINT-DETAIL THRU 0510-EXIT.                    
01804                                                                   
01805  0460-CHECK-NEXT-LINE.                                            
01806      ADD +1 TO SB.                                                
01807      IF DT-CTL-CHAR (SB) NOT = HIGH-VALUES                        
01808          GO TO 0450-PRINT-LOOP.                                   
01809                                                                   
01810      MOVE '-'    TO X.                                            
01811      MOVE SPACES TO P-DATA.                                       
01812      PERFORM 0610-PRT-RTN THRU 0630-EXIT.                         
060402*    IF (DTE-FMT-OPT  =  2)        AND
060402*       (BREAK-1-SWITCH NOT = ' ')   AND
060402*       (H1-REPORT-SUF = 'A')
060402*       NEXT SENTENCE
060402*    ELSE
060402*       ADD 1 TO PAGE-CNT.
060402*    MOVE PAGE-CNT TO H9-PAGE.
060402*    MOVE HEAD-9   TO P-DATA.
060402*    PERFORM 0610-PRT-RTN THRU 0630-EXIT.
01822                                                                   
01823  0470-EXIT.                                                       
01824      EXIT.                                                        
01825  EJECT                                                            
01826                                                                   
01827  0480-COMPUTE-PERCENTS.                                           
01828      IF PA-EP (SF) = ZERO                                         
01829          GO TO 0490-EXIT.                                         
01830                                                                   
01831      COMPUTE PT-PRCNT (SE 5) ROUNDED =                            
01832          PA-PE (SF) / PA-EP (SF).                                 
01833      COMPUTE PT-PRCNT (SE 9) ROUNDED =                            
01834          PA-TL (SF) / PA-EP (SF).                                 
01835      COMPUTE PT-PRCNT (SE 12) ROUNDED =                           
01836          PA-TC (SF) / PA-EP (SF).                                 
01837                                                                   
01838      IF DTE-CLIENT = 'TAO'  AND  PA-NW (SF) NOT = ZERO            
01839          COMPUTE PT-PRCNT (SE 12) ROUNDED =                       
01840              PA-TC (SF) / PA-NW (SF).                             
01841                                                                   
01842      COMPUTE PT-PRCNT (SE 13) ROUNDED =                           
01843          PA-CP (SF) / PA-EP (SF).                                 
01844      COMPUTE PT-PRCNT (SE 14) ROUNDED =                           
01845          PA-EX (SF) / PA-EP (SF).                                 
01846      COMPUTE PT-PRCNT (SE 16) ROUNDED =                           
01847          PA-BA (SF) / PA-EP (SF).                                 
01848      COMPUTE PT-PRCNT (SE 17) ROUNDED =                           
01849          PA-NTE (SF) / PA-EP (SF).                                
01850      COMPUTE PT-PRCNT (SE 18) ROUNDED =                           
01851          PA-NBA (SF) / PA-EP (SF).                                
01852      COMPUTE PT-PRCNT (SE 24) ROUNDED =                           
01853          PA-GATC (SF) / PA-EP (SF).                               
01854      COMPUTE PT-PRCNT (SE 25) ROUNDED =                           
01855          PA-GAGT (SF) / PA-EP (SF).                               
01856      COMPUTE PT-PRCNT (SE 26) ROUNDED =                           
01857          PA-GABA (SF) / PA-EP (SF).                               
01858      COMPUTE PT-PRCNT (SE 27) ROUNDED =                           
01859          PA-GATE (SF) / PA-EP (SF).                               
01860                                                                   
01861      COMPUTE PT-PRCNT (SE 5)  =  PT-PRCNT (SE 5)  * +100.         
01862      COMPUTE PT-PRCNT (SE 9)  =  PT-PRCNT (SE 9)  * +100.         
01863      COMPUTE PT-PRCNT (SE 12) =  PT-PRCNT (SE 12) * +100.         
01864      COMPUTE PT-PRCNT (SE 13) =  PT-PRCNT (SE 13) * +100.         
01865      COMPUTE PT-PRCNT (SE 14) =  PT-PRCNT (SE 14) * +100.         
01866      COMPUTE PT-PRCNT (SE 16) =  PT-PRCNT (SE 16) * +100.         
01867      COMPUTE PT-PRCNT (SE 17) =  PT-PRCNT (SE 17) * +100.         
01868      COMPUTE PT-PRCNT (SE 18) =  PT-PRCNT (SE 18) * +100.         
01869      COMPUTE PT-PRCNT (SE 24) =  PT-PRCNT (SE 24) * +100.         
01870      COMPUTE PT-PRCNT (SE 25) =  PT-PRCNT (SE 25) * +100.         
01871      COMPUTE PT-PRCNT (SE 26) =  PT-PRCNT (SE 26) * +100.         
01872      COMPUTE PT-PRCNT (SE 27) =  PT-PRCNT (SE 27) * +100.         
01873                                                                   
01874  0490-EXIT.                                                       
01875      EXIT.                                                        
01876                                                                   
01877  EJECT                                                            
01878  0500-PRINT-DETAIL.                                               
01879      MOVE PA-AMT (SG 1 SA) TO DL-YLF.                             
01880      MOVE PA-AMT (SG 2 SA) TO DL-YAH.                             
01881      MOVE PA-AMT (SG 3 SA) TO DL-YT.                              
01882      MOVE PT-PRCNT (1 SA)  TO DL-YPT.                             
01883      MOVE DT-ENTRY (SB)    TO DL-DESC.                            
01884      MOVE PA-AMT (SG 4 SA) TO DL-ILF.                             
01885      MOVE PA-AMT (SG 5 SA) TO DL-IAH.                             
01886      MOVE PA-AMT (SG 6 SA) TO DL-IT.                              
01887      MOVE PT-PRCNT (2 SA)  TO DL-IPT.                             
01888      MOVE DETAIL-LINE      TO P-DATA.                             
01889      PERFORM 0610-PRT-RTN THRU 0630-EXIT.                         
01890      MOVE SPACES           TO DETAIL-LINE  P-DATA.                
01891                                                                   
01892  0510-EXIT.                                                       
01893      EXIT.                                                        
01894                                                                   
01895  EJECT                                                            
01896  0520-ADD-PA-LEVELS.                                              
01897      ADD PA-NW   (SC) TO PA-NW   (SF).                            
01898      ADD PA-BR   (SC) TO PA-BR   (SF).                            
01899      ADD PA-ER   (SC) TO PA-ER   (SF).                            
01900      ADD PA-EP   (SC) TO PA-EP   (SF).                            
01901      ADD PA-PE   (SC) TO PA-PE   (SF).                            
01902      ADD PA-PL   (SC) TO PA-PL   (SF).                            
01903      ADD PA-BLR  (SC) TO PA-BLR  (SF).                            
01904      ADD PA-ELR  (SC) TO PA-ELR  (SF).                            
01905      ADD PA-TL   (SC) TO PA-TL   (SF).                            
01906      ADD PA-RC   (SC) TO PA-RC   (SF).                            
01907      ADD PA-OC   (SC) TO PA-OC   (SF).                            
01908      ADD PA-TC   (SC) TO PA-TC   (SF).                            
01909      ADD PA-CP   (SC) TO PA-CP   (SF).                            
01910      ADD PA-EX   (SC) TO PA-EX   (SF).                            
01911      ADD PA-TE   (SC) TO PA-TE   (SF).                            
01912      ADD PA-BA   (SC) TO PA-BA   (SF).                            
01913      ADD PA-NTE  (SC) TO PA-NTE  (SF).                            
01914      ADD PA-NBA  (SC) TO PA-NBA  (SF).                            
01915      ADD PA-EC   (SC) TO PA-EC   (SF).                            
01916      ADD PA-GARC (SC) TO PA-GARC (SF).                            
01917      ADD PA-GAEC (SC) TO PA-GAEC (SF).                            
01918      ADD PA-GACU (SC) TO PA-GACU (SF).                            
01919      ADD PA-CU   (SC) TO PA-CU   (SF).                            
01920      ADD PA-GATC (SC) TO PA-GATC (SF).                            
01921      ADD PA-GAGT (SC) TO PA-GAGT (SF).                            
01922      ADD PA-GABA (SC) TO PA-GABA (SF).                            
01923      ADD PA-GATE (SC) TO PA-GATE (SF).                            
01924                                                                   
01925  0530-ADD-UP-LEVELS.                                              
01926      PERFORM 0520-ADD-PA-LEVELS.                                  
01927      ADD +1 TO SC                                                 
01928                SF.                                                
01929                                                                   
01930  0540-EXIT.                                                       
01931      EXIT.                                                        
01932                                                                   
01933  EJECT                                                            
01934  0550-ZERO-DA.                                                    
01935      MOVE +0 TO DA-ISP (SA)  DA-CNP (SA)  DA-UP (SA)   DA-UC (SA) 
PEMMOD                DA-TAX (SA)
01936                 DA-CLM (SA)  DA-EXP (SA)  DA-CPS (SA)  DA-OTC (SA)
01937                 DA-LRV (SA)  DA-CAJ (SA)  DA-ISC (SA)  DA-CNC (SA)
01938                 DA-ISC-ADDL (SA) DA-CNC-ADDL (SA) DA-UC-ADDL (SA) 
01939                 DA-GA-ISC (SA)   DA-GA-CNC (SA)   DA-GA-UC (SA)   
01940                 DA-PCT-E-PREM (SA).                               
01941                                                                   
01942  0560-ZERO-PA.                                                    
01943      MOVE +0 TO PA-NW (SB)  PA-BR (SB) PA-ER (SB)  PA-EP (SB)     
01944                 PA-PE (SB)  PA-PL (SB) PA-BLR (SB) PA-ELR (SB)    
01945                 PA-TL (SB)  PA-RC (SB) PA-OC (SB)  PA-TC (SB)     
01946                 PA-CP (SB)  PA-EX (SB) PA-TE (SB)  PA-BA (SB)     
01947                 PA-NTE (SB) PA-NBA (SB)   PA-EC (SB)              
01948                 PA-GARC (SB)  PA-GAEC (SB)  PA-GACU (SB)          
01949                 PA-GATC (SB)  PA-GAGT (SB)  PA-GABA (SB)          
01950                 PA-GATE (SB)  PA-CU (SB).                         
01951                                                                   
01952  0570-ZERO-XDA.                                                   
01953                                                                   
01954      PERFORM 0575-ZERO-XDA VARYING SA FROM 1 BY 1                 
01955                            UNTIL SA GREATER THAN 4.               
01956                                                                   
01957  0575-ZERO-XDA.                                                   
01958                                                                   
01959      MOVE SPACES                 TO  XDA-GA-NO (CL)               
01960                                      XDA-GA-TYPE (CL).            
01961      MOVE ZEROS                  TO  XDA-GA-ISC (CL SA)           
01962                                      XDA-GA-CNC (CL SA)           
01963                                      XDA-GA-UC  (CL SA).          
01964                                                                   
01965  0580-ZERO-PCNT.                                                  
01966      MOVE +0 TO PT-PRCNT (SB SC).                                 
01967                                                                   
01968  EJECT                                                            
01969  0590-HEADING-ROUTINE.                                            
01970      IF DTE-PGM-OPT = '1'                                         
01971          AND H1-REPORT-SUF NOT = 'A'                              
01972      OR DTE-PGM-OPT = '2'                                         
01973          AND H1-REPORT-SUF NOT = 'B'                              
01974      OR DTE-PGM-OPT = '3'                                         
01975          AND H1-REPORT-SUF NOT = 'C'                              
01976              GO TO 0600-EXIT.                                     
01977                                                                   
01978      IF DTE-PGM-OPT = ('5' OR '6' OR '7') AND                     
01979         H1-REPORT-SUF NOT = ('A' AND 'B')                         
01980         GO TO 0600-EXIT.                                          
01981                                                                   
01982      MOVE '1'    TO X.                                            
01983      MOVE HEAD-1 TO P-DATA.                                       
01984      PERFORM 0610-PRT-RTN THRU 0630-EXIT.                         
01985      MOVE ' '    TO X.                                            
01986      MOVE HEAD-2 TO P-DATA.                                       
01987      PERFORM 0610-PRT-RTN THRU 0630-EXIT.
060402     ADD +1               TO PAGE-CNT.
060402     MOVE PAGE-CNT        TO H3-PAGE.
01988      MOVE HEAD-3 TO P-DATA.                                       
01989      PERFORM 0610-PRT-RTN THRU 0630-EXIT.                         
01990                                                                   
01991      IF HEAD-SW = '1' OR '2'                                      
01992          MOVE '0'    TO X                                         
01993          MOVE HEAD-4 TO P-DATA                                    
01994          PERFORM 0610-PRT-RTN THRU 0630-EXIT                      
01995          MOVE ' '    TO X                                         
01996          MOVE HEAD-5 TO P-DATA                                    
01997          PERFORM 0610-PRT-RTN THRU 0630-EXIT.                     
01998                                                                   
01999      MOVE '0' TO X.                                               
02000      IF DTE-PGM-OPT = '7'                                         
02001          IF HEAD-SW NOT = '1' AND '2'                             
02002              MOVE '* CONTRACT TO DATE *' TO H6-YTD-HD             
02003          ELSE                                                     
02004              MOVE BD-MO     TO H6R-MO                             
02005              MOVE BD-DA     TO H6R-DA                             
02006              MOVE BD-CCYY   TO H6R-CCYY                           
02007              MOVE '-'       TO H6R-DASH1 H6R-DASH2                
02008              MOVE ' TO DATE '       TO H6R-MSG                    
02009              MOVE ' '       TO H6R-FILLER.                        
02010                                                                   
02011      MOVE HEAD-6 TO P-DATA.                                       
02012      PERFORM 0610-PRT-RTN THRU 0630-EXIT.                         
02013      MOVE ' '    TO X.                                            
02014      MOVE HEAD-7 TO P-DATA.                                       
02015      PERFORM 0610-PRT-RTN THRU 0630-EXIT.                         
02016      MOVE '0'    TO X.                                            
02017                                                                   
02018  0600-EXIT.                                                       
02019      EXIT.                                                        
02020  EJECT                                                            
02021                                                                   
02022  0610-PRT-RTN.                                                    
02023 ****  THE FOLLOWING CLIENT CODING SUPPRESSES DATE BREAKS ON '43A'.
02024                                                                   
02025      IF (DTE-FMT-OPT  =  2)        AND                            
02026         (BREAK-1-SWITCH NOT = ' ')   AND                          
02027         (H1-REPORT-SUF = 'A')                                     
02028              MOVE SPACES TO P-DATA  DETAIL-LINE                   
02029              GO TO 0630-EXIT.                                     
02030                                                                   
02031  0620-PRT-REST.                                                   
CIDMOD*                            COPY ELCPRT2.                        
CIDMOD                             COPY PRTN043.                        
02033      MOVE SPACES TO P-DATA  DETAIL-LINE.                          
02034                                                                   
02035  0630-EXIT.                                                       
02036      EXIT.                                                        
02037                                                                   
02038  EJECT                                                            
02039  0640-EP-ADD.                                                     
02040      ADD EP-ISS-PRM TO DA-ISP (SA).                               
PEMMOD     ADD EP-PRM-TAX TO DA-TAX (SA).                               
02041      ADD EP-CNC-PRM TO DA-CNP (SA).                               
02042      ADD EP-CLM-AMT TO DA-CLM (SA).                               
02043      ADD EP-CLM-DU TO DA-LRV (SA).                                
02044                                                                   
02045      IF DTE-CLIENT NOT = 'ABL'                                    
02046          ADD EP-CLM-IBNR TO DA-LRV (SA).                          
02047                                                                   
02048      ADD EP-LOSS-RESV TO DA-LRV (SA).                             
02049      ADD EP-CLAIM-ADJ TO DA-CAJ (SA).                             
02050                                                                   
02051      IF DTE-CLIENT = 'ABL'                                        
02052          ADD EP-CLAIM-ADJ TO DA-CLM (SA).                         
02053                                                                   
02054 *    PERFORM 0650-EP-EARN-TYPE THRU 0680-EXIT.                    
02055                                                                   
02056      MOVE DA-UP (SA) TO SAVE-DA-UP.                               
02057      MOVE 0                    TO SAVE-DA-PCT-UP.                 
02058                                                                   
02059      IF WS-EP-CODE = 'R' OR 'N' OR 'U' OR 'A' OR 'T'              
02060          COMPUTE SAVE-DA-PCT-UP =                                 
02061                        ((EP-ISS-PRM - EP-CNC-PRM) - EP-PRM-78)    
02062          COMPUTE DA-UP (SA) = DA-UP (SA) +                        
02063                        ((EP-ISS-PRM - EP-CNC-PRM) - EP-PRM-78).   
02064                                                                   
02065      IF WS-EP-CODE = 'P'                                          
02066          COMPUTE SAVE-DA-PCT-UP =                                 
02067                        ((EP-ISS-PRM - EP-CNC-PRM) - EP-PRM-PR)    
02068          COMPUTE DA-UP (SA) = DA-UP (SA) +                        
02069                        ((EP-ISS-PRM - EP-CNC-PRM) - EP-PRM-PR).   
02070                                                                   
02071      IF (WS-EP-CODE = 'B' OR 'K' OR 'L')                          
02072          COMPUTE SAVE-DA-PCT-UP =                                 
02073                        ((EP-ISS-PRM - EP-CNC-PRM) - EP-PRM-ST)    
02074          COMPUTE DA-UP (SA) = DA-UP (SA) +                        
02075                        ((EP-ISS-PRM - EP-CNC-PRM) - EP-PRM-ST).   
02076                                                                   
02077      IF WS-EP-CODE = 'M'                                          
02078          COMPUTE SAVE-DA-PCT-UP =                                 
02079              ((EP-ISS-PRM - EP-CNC-PRM) -                         
02080                ((EP-PRM-PR + EP-PRM-78) * .5))                    
02081          COMPUTE DA-UP (SA) = DA-UP (SA) +                        
02082              ((EP-ISS-PRM - EP-CNC-PRM) -                         
02083                ((EP-PRM-PR + EP-PRM-78) * .5)).                   
02084                                                                   
02085      IF WS-EP-CODE = '1'                                          
02086          COMPUTE SAVE-DA-PCT-UP =                                 
02087              ((EP-ISS-PRM - EP-CNC-PRM) -                         
02088               ((EP-PRM-PR * +.6667) + (EP-PRM-78 * +.3333)))      
02089          COMPUTE DA-UP (SA) = DA-UP (SA) +                        
02090              ((EP-ISS-PRM - EP-CNC-PRM) -                         
02091               ((EP-PRM-PR * +.6667) + (EP-PRM-78 * +.3333))).     
02092                                                                   
02093      IF WS-EP-CODE = '2'                                          
02094          COMPUTE SAVE-DA-PCT-UP =                                 
02095              ((EP-ISS-PRM - EP-CNC-PRM) -                         
02096               ((EP-PRM-PR * +.80) + (EP-PRM-78 * +.20)))          
02097          COMPUTE DA-UP (SA) = DA-UP (SA) +                        
02098              ((EP-ISS-PRM - EP-CNC-PRM) -                         
02099               ((EP-PRM-PR * +.80) + (EP-PRM-78 * +.20))).         
02100                                                                   
02101      IF AM-RETRO-PREM-P-E = 'E'                                   
02102          COMPUTE SAVE-DA-PCT-E-PREM  ROUNDED  =                   
02103              (EP-ISS-PRM - EP-CNC-PRM - SAVE-DA-PCT-UP)           
02104                                                  * RETENT-FACT    
02105      ELSE                                                         
02106          COMPUTE SAVE-DA-PCT-E-PREM  ROUNDED  =                   
02107                        (EP-ISS-PRM - EP-CNC-PRM) * RETENT-FACT.   
02108                                                                   
02109      ADD SAVE-DA-PCT-E-PREM      TO DA-PCT-E-PREM (SA).           
02110                                                                   
02111  0640-EXIT.                                                       
02112      EXIT.                                                        
02113  EJECT                                                            
02114                                                                   
02115  0650-EP-EARN-TYPE.                                               
02116      MOVE CLAS-STARTS TO CLAS-INDEXS.                             
02117                                                                   
02118  0652-ST-LOOK-UP.                                                 
02119      IF AM-STATE = STATE-SUB (CLAS-INDEXS)                        
02120          NEXT SENTENCE                                            
02121       ELSE                                                        
02122           ADD +1 TO CLAS-INDEXS                                   
02123           IF CLAS-INDEXS NOT GREATER CLAS-MAXS                    
02124               GO TO 0652-ST-LOOK-UP                               
02125            ELSE                                                   
02126              DISPLAY ' INVALID STATE CODE -' AM-STATE             
02127              MOVE    ' INVALID STATE CODE -' TO WS-ABEND-MESSAGE  
02128              MOVE 0402      TO WS-RETURN-CODE                     
02129              GO TO ABEND-PGM.                                     
02130                                                                   
02131      IF EP-RCD-TYPE = AH-OVERRIDE-L1                              
02132          MOVE CLAS-STARTA TO CLAS-INDEXA                          
02133          GO TO 0670-EP-EARN-AH.                                   
02134                                                                   
02135      MOVE CLAS-STARTL TO CLAS-INDEXL.                             
02136                                                                   
02137  0660-EP-EARN-TYPE-LOOP.                                          
02138      IF (CLAS-INDEXL GREATER CLAS-MAXL) OR (CLAS-INDEXL = ZERO)   
02139          DISPLAY ' INVALID LIFE BENEFIT TYPE -' EP-BEN-CODE       
02140          MOVE ' INVALID LIFE BENEFIT TYPE -' TO WS-ABEND-MESSAGE  
02141          MOVE 0401            TO WS-RETURN-CODE                   
02142          GO TO ABEND-PGM.                                         
02143                                                                   
02144      IF EP-BEN-CODE NOT = CLAS-I-BEN (CLAS-INDEXL)                
02145          ADD +1 TO CLAS-INDEXL                                    
02146          GO TO 0660-EP-EARN-TYPE-LOOP.                            
02147                                                                   
02148      MOVE CLAS-I-EP (CLAS-INDEXL) TO WS-EP-CODE.                  
02149                                                                   
02150      IF STATE-ABBR (CLAS-INDEXS) = 'WY'                           
02151           MOVE 'P' TO WS-EP-CODE.                                 
02152                                                                   
02153      IF DTE-CLIENT = 'ITG'                                        
02154           MOVE 'P' TO WS-EP-CODE.                                 
02155                                                                   
02156      IF CLAS-I-RL-AH (CLAS-INDEXL) = 'R'                          
02157        AND (AM-RET-EARN-R NOT = SPACE AND ZERO)                   
02158            MOVE AM-RET-EARN-R TO WS-EP-CODE.                      
02159      IF (CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P')                 
02160        AND (AM-RET-EARN-L NOT = SPACE AND ZERO)                   
02161            MOVE AM-RET-EARN-L TO WS-EP-CODE.                      
02162                                                                   
02163      MOVE CLAS-I-BAL (CLAS-INDEXL) TO WS-OB-CODE.                 
02164                                                                   
02165      IF CLAS-I-RL-AH (CLAS-INDEXL) = 'R'                          
02166        AND (SA = 1 OR 2)                                          
02167        AND (AM-RET-BEG-EARN-R NOT = SPACE AND ZERO)               
02168            MOVE AM-RET-BEG-EARN-R TO WS-EP-CODE.                  
02169                                                                   
02170      IF (CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P')                 
02171        AND (SA = 1 OR 2)                                          
02172        AND (AM-RET-BEG-EARN-L NOT = SPACE AND ZERO)               
02173            MOVE AM-RET-BEG-EARN-L TO WS-EP-CODE.                  
02174                                                                   
02175      GO TO 0680-EXIT.                                             
02176  EJECT                                                            
02177                                                                   
02178  0670-EP-EARN-AH.                                                 
02179      IF (CLAS-INDEXA GREATER CLAS-MAXA) OR (CLAS-INDEXA = ZERO)   
02180          DISPLAY ' INVALID A&H BENEFIT TYPE -' EP-BEN-CODE        
02181          MOVE    ' INVALID A&H BENEFIT TYPE -' TO WS-ABEND-MESSAGE
02182          MOVE 0402         TO WS-RETURN-CODE                      
02183          GO TO ABEND-PGM.                                         
02184                                                                   
02185      IF EP-BEN-CODE NOT = CLAS-I-BEN (CLAS-INDEXA)                
02186          ADD +1 TO CLAS-INDEXA                                    
02187          GO TO 0670-EP-EARN-AH.                                   
02188                                                                   
02189      MOVE CLAS-I-EP (CLAS-INDEXA) TO WS-EP-CODE.                  
02190                                                                   
02191      IF STATE-ABBR (CLAS-INDEXS) = 'WY'                           
02192           MOVE 'P' TO WS-EP-CODE.                                 
02193                                                                   
02194      IF DTE-CLIENT = 'ITG'                                        
02195           MOVE 'P' TO WS-EP-CODE.                                 
02196                                                                   
02197      IF CLAS-I-RL-AH (CLAS-INDEXA) = 'A'                          
02198        AND (AM-RET-EARN-A NOT = SPACE AND ZERO)                   
02199            MOVE AM-RET-EARN-A TO WS-EP-CODE.                      
02200                                                                   
02201      IF CLAS-I-RL-AH (CLAS-INDEXA) = 'A'                          
02202        AND (SA = 1 OR 2)                                          
02203        AND (AM-RET-BEG-EARN-A NOT = SPACE AND ZERO)               
02204            MOVE AM-RET-BEG-EARN-A TO WS-EP-CODE.                  
02205                                                                   
02206      IF DTE-CLIENT = 'ITY'                                        
02207          IF EP-CARRIER = '5' OR '7'                               
02208               MOVE '1' TO WS-EP-CODE.                             
02209                                                                   
02210      IF DTE-CLIENT = 'GIC'                                        
02211          IF CLAS-I-BAL (CLAS-INDEXA) NOT = 'B'                    
02212               MOVE '2' TO WS-EP-CODE.                             
02213                                                                   
02214      IF DTE-CLIENT = 'AN1' OR 'TFS' OR 'NIS' OR                   
02215                      'UW1' OR 'DDB' OR 'GSL'                      
02216          MOVE 'M'                  TO WS-EP-CODE.                 
02217                                                                   
02218      MOVE CLAS-I-BAL (CLAS-INDEXA) TO WS-OB-CODE.                 
02219                                                                   
02220  0680-EXIT.                                                       
02221      EXIT.                                                        
02222  EJECT                                                            
02223                                                                   
02224  0690-EC-ADD.                                                     
02225      PERFORM 0700-ACCT-EC-ADD  THRU 0710-EXIT                     
02226          VARYING CL FROM 1 BY 1 UNTIL CL GREATER THAN +5.         
02227                                                                   
02228      GO TO 0729-EXIT.                                             
02229  EJECT                                                            
02230                                                                   
02231  0700-ACCT-EC-ADD.                                                
02232 ******************************************************************
02233 *         CL - AGENT LEVEL WITHIN EC RECORD                       
02234 *         AL - AGENT LEVEL WITHIN GA TABLE                        
02235 *         SA - BEGINNING / CURRENT   LIFE/AH INDICATOR            
02236 ******************************************************************
02237                                                                   
02238      IF EC-AGT-NO (CL) = ZEROS                                    
02239          GO TO 0710-EXIT.                                         
02240                                                                   
02241      IF EC-SEQ-NBR GREATER THAN 1                                 
02242          MOVE EC-SEQ-NBR         TO  WS-SEQ-NBR                   
02243          SUBTRACT +1 FROM WS-SEQ-NBR                              
02244          COMPUTE AL = WS-SEQ-NBR * 5.                             
02245                                                                   
02246      PERFORM 0725-CHECK-AM-RETRO  THRU  0725-EXIT.                
02247                                                                   
02248      IF WS-RETRO-FLAG = 'N'                                       
02249         GO TO 0710-EXIT.                                          
02250                                                                   
02251      MOVE +1                     TO  AL.                          
02252                                                                   
02253  0700-LOOP.                                                       
02254                                                                   
02255      IF EC-AGT-NO (CL) = XDA-GA-NO (AL)                           
02256          GO TO 0705-ACCUM.                                        
02257                                                                   
02258      IF XDA-GA-NO (AL) = SPACES                                   
02259          MOVE EC-AGT-NO (CL)     TO  XDA-GA-NO (AL)               
02260          MOVE EC-AGT-TYPE (CL)   TO  XDA-GA-TYPE (AL)             
02261          GO TO 0705-ACCUM.                                        
02262                                                                   
02263      ADD +1 TO AL.                                                
02264                                                                   
02265      IF AL GREATER THAN EC-REC-MAX                                
02266          MOVE 'NO OF EC RECORDS EXCEED TABLE LIMIT'               
02267                                  TO  WS-ABEND-MESSAGE             
02268          GO TO ABEND-PGM.                                         
02269                                                                   
02270      GO TO 0700-LOOP.                                             
02271                                                                   
02272  0705-ACCUM.                                                      
02273                                                                   
02274      IF EC-AGT-TYPE (CL) = 'C' OR 'D'                             
02275          ADD EC-ISS-COMM (CL) TO DA-ISC (SA)                      
02276          ADD EC-CNC-COMM (CL) TO DA-CNC (SA).                     
02277                                                                   
02278      IF EC-AGT-TYPE (CL) = 'O' OR 'P'                             
02279          ADD EC-ISS-COMM (CL) TO XDA-GA-ISC (AL SA)               
02280          ADD EC-CNC-COMM (CL) TO XDA-GA-CNC (AL SA)               
02281        IF AM-RETRO-LV-INDIC (AL) = 'A'                            
02282            ADD EC-ISS-COMM (CL) TO DA-ISC-ADDL (SA)               
02283            ADD EC-CNC-COMM (CL) TO DA-CNC-ADDL (SA).              
02284                                                                   
02285 *    MOVE DA-UC (SA) TO SAVE-DA-UC.                               
02286 *    MOVE XDA-GA-UC (SA AL) TO SAVE-DA-GA-UC.                     
02287                                                                   
02288      IF WS-EP-CODE = 'R' OR 'N' OR 'U' OR 'A' OR 'T'              
02289         IF EC-AGT-TYPE (CL) = 'C' OR 'D'                          
02290          COMPUTE DA-UC (SA) = DA-UC (SA) +                        
02291              ((EC-ISS-COMM (CL) - EC-CNC-COMM (CL)) -             
02292               EC-COMM-78 (CL))                                    
02293         ELSE                                                      
02294          IF EC-AGT-TYPE (CL) = 'O' OR 'P'                         
02295           COMPUTE XDA-GA-UC (AL SA) = XDA-GA-UC (AL SA) +         
02296              ((EC-ISS-COMM (CL) - EC-CNC-COMM (CL)) -             
02297               EC-COMM-78 (CL))                                    
02298             IF AM-RETRO-LV-INDIC (AL) = 'A'                       
02299                COMPUTE DA-UC-ADDL (SA) = DA-UC-ADDL (SA) +        
02300                   ((EC-ISS-COMM (CL) - EC-CNC-COMM (CL)) -        
02301                     EC-COMM-78 (CL)).                             
02302                                                                   
02303      IF WS-EP-CODE = 'P'                                          
02304        IF EC-AGT-TYPE (CL) = 'C' OR 'D'                           
02305          COMPUTE DA-UC (SA) = DA-UC (SA) +                        
02306              ((EC-ISS-COMM (CL) - EC-CNC-COMM (CL)) -             
02307               EC-COMM-PR (CL))                                    
02308        ELSE                                                       
02309         IF EC-AGT-TYPE (CL) = 'O' OR 'P'                          
02310          COMPUTE XDA-GA-UC (AL SA) = XDA-GA-UC (AL SA) +          
02311              ((EC-ISS-COMM (CL) - EC-CNC-COMM (CL)) -             
02312               EC-COMM-PR (CL))                                    
02313             IF AM-RETRO-LV-INDIC (AL) = 'A'                       
02314                COMPUTE DA-UC-ADDL (SA) = DA-UC-ADDL (SA) +        
02315                 ((EC-ISS-COMM (CL) - EC-CNC-COMM (CL)) -          
02316                   EC-COMM-PR (CL)).                               
02317                                                                   
02318      IF (WS-EP-CODE = 'B' OR 'K' OR 'L')                          
02319        IF EC-AGT-TYPE (CL) = 'C' OR 'D'                           
02320          COMPUTE DA-UC (SA) = DA-UC (SA) +                        
02321              ((EC-ISS-COMM (CL) - EC-CNC-COMM (CL)) -             
02322               EC-COMM-ST (CL))                                    
02323        ELSE                                                       
02324         IF EC-AGT-TYPE (CL) = 'O' OR 'P'                          
02325          COMPUTE XDA-GA-UC (AL SA) = XDA-GA-UC (AL SA) +          
02326              ((EC-ISS-COMM (CL) - EC-CNC-COMM (CL)) -             
02327               EC-COMM-ST (CL))                                    
02328             IF AM-RETRO-LV-INDIC (AL) = 'A'                       
02329                COMPUTE DA-UC-ADDL (SA) = DA-UC-ADDL (SA) +        
02330                 ((EC-ISS-COMM (CL) - EC-CNC-COMM (CL)) -          
02331                   EC-COMM-ST (CL)).                               
02332                                                                   
02333      IF WS-EP-CODE = 'M'                                          
02334          IF EC-AGT-TYPE (CL) = 'C' OR 'D'                         
02335                  COMPUTE DA-UC (SA) = DA-UC (SA) +                
02336                     ((EC-ISS-COMM (CL) - EC-CNC-COMM (CL)) -      
02337                       ((EC-COMM-PR (CL) + EC-COMM-78 (CL)) * .5)) 
02338           ELSE                                                    
02339            IF EC-AGT-TYPE (CL) = 'O' OR 'P'                       
02340                  COMPUTE XDA-GA-UC (AL SA) = XDA-GA-UC (AL SA) +  
02341                     ((EC-ISS-COMM (CL) - EC-CNC-COMM (CL)) -      
02342                       ((EC-COMM-PR (CL) + EC-COMM-78 (CL)) * .5)) 
02343                IF AM-RETRO-LV-INDIC (AL) = 'A'                    
02344                    COMPUTE DA-UC-ADDL (SA) = DA-UC-ADDL (SA) +    
02345                     ((EC-ISS-COMM (CL) - EC-CNC-COMM (CL)) -      
02346                       ((EC-COMM-PR (CL) + EC-COMM-78 (CL)) * .5)).
02347                                                                   
02348      IF WS-EP-CODE = '1'                                          
02349          IF EC-AGT-TYPE (CL) = 'C' OR 'D'                         
02350                  COMPUTE DA-UC (SA) = DA-UC (SA) +                
02351                       ((EC-ISS-COMM (CL) - EC-CNC-COMM (CL)) -    
02352                       ((EC-COMM-PR (CL) * +.6667) +               
02353                        (EC-COMM-78 (CL) * +.3333)))               
02354          ELSE                                                     
02355              IF EC-AGT-TYPE (CL) = 'O' OR 'P'                     
02356                  COMPUTE XDA-GA-UC (AL SA) = XDA-GA-UC (AL SA) +  
02357                       ((EC-ISS-COMM (CL) - EC-CNC-COMM (CL)) -    
02358                       ((EC-COMM-PR (CL) * +.6667) +               
02359                        (EC-COMM-78 (CL) * +.3333)))               
02360                  IF AM-RETRO-LV-INDIC (AL) = 'A'                  
02361                      COMPUTE DA-UC-ADDL (SA) = DA-UC-ADDL (SA) +  
02362                       ((EC-ISS-COMM (CL) - EC-CNC-COMM (CL)) -    
02363                       ((EC-COMM-PR (CL) * +.6667) +               
02364                        (EC-COMM-78 (CL) * +.3333))).              
02365                                                                   
02366      IF WS-EP-CODE = '2'                                          
02367          IF EC-AGT-TYPE (CL) = 'C' OR 'D'                         
02368                  COMPUTE DA-UC (SA) = DA-UC (SA) +                
02369                       ((EC-ISS-COMM (CL) - EC-CNC-COMM (CL)) -    
02370                       ((EC-COMM-PR (CL) * +.80) +                 
02371                        (EC-COMM-78 (CL) * +.20)))                 
02372          ELSE                                                     
02373              IF EC-AGT-TYPE (CL) = 'O' OR 'P'                     
02374                  COMPUTE XDA-GA-UC (AL SA) = XDA-GA-UC (AL SA) +  
02375                       ((EC-ISS-COMM (CL) - EC-CNC-COMM (CL)) -    
02376                       ((EC-COMM-PR (CL) * +.80) +                 
02377                        (EC-COMM-78 (CL) * +.20)))                 
02378                  IF AM-RETRO-LV-INDIC (AL) = 'A'                  
02379                      COMPUTE DA-UC-ADDL (SA) = DA-UC-ADDL (SA) +  
02380                       ((EC-ISS-COMM (CL) - EC-CNC-COMM (CL)) -    
02381                       ((EC-COMM-PR (CL) * +.80) +                 
02382                        (EC-COMM-78 (CL) * +.20))).                
02383                                                                   
02384  0710-EXIT.                                                       
02385      EXIT.                                                        
02386                                                                   
02387  0725-CHECK-AM-RETRO.                                             
02388                                                                   
02389      MOVE SPACE                  TO  WS-RETRO-FLAG.               
02390                                                                   
02391      PERFORM 0725-CHECK-LOOP VARYING AN FROM 1 BY 1               
02392                              UNTIL AN GREATER THAN 10.            
02393  0725-CHECK-LOOP.                                                 
02394                                                                   
02395      IF EC-AGT-NO (CL) = AM-AGT (AN)                              
02396          MOVE AM-RETRO-LV-INDIC (AN)                              
02397                                  TO  WS-RETRO-FLAG                
02398          MOVE +11                TO  AN.                          
02399                                                                   
02400  0725-EXIT.                                                       
02401      EXIT.                                                        
02402                                                                   
02403  0729-EXIT.                                                       
02404      EXIT.                                                        
02405  EJECT                                                            
02406                                                                   
02407  0730-RETURN-SORTED-FILE.                                         
02408                                                                   
02409      IF SW-CTL NOT = HIGH-VALUES                                  
02410          RETURN SORT-WORK  INTO  SW-REC                           
02411                     AT END MOVE HIGH-VALUES TO SW-RPT-TYPE-ID     
02412                                                SW-CTL.            
02413  0730-EXIT.                                                       
02414      EXIT.                                                        
02415                                                                   
02416  0740-MOVE-SW-REC.                                                
02417      IF SW-RPT-TYPE-ID = 'C'                                      
02418          IF DTE-CLIENT = 'LBL'                                    
02419              MOVE 'E'            TO SW-RET-TYP.                   
02420                                                                   
02421      MOVE SW-NAME                TO H1-NAME.                      
02422      MOVE SW-ADDRS               TO H2-ADDR.                      
02423      MOVE SW-CITY                TO H3-ADDR.                      
02424                                                                   
02425      IF  SW-CANADIAN-POST-CODE                                    
02426          MOVE SW-CAN-POST-CODE-1 TO H3-CAN-POSTAL-CODE-1          
02427          MOVE SW-CAN-POST-CODE-2 TO H3-CAN-POSTAL-CODE-2          
02428          MOVE SPACES             TO H3-DASH-CAN                   
02429                                     H3-CAN-FILLER                 
02430                                                                   
02431      ELSE                                                         
02432          MOVE SW-ZIP-PRIME       TO H3-ZIP-PRIME                  
02433                                                                   
02434          IF  SW-ZIP-PLUS4 = SPACES OR ZEROS                       
02435              MOVE SPACES         TO H3-ZIP-PLUS4 H3-DASH          
02436                                                                   
02437          ELSE                                                     
02438              MOVE SW-ZIP-PLUS4   TO H3-ZIP-PLUS4                  
02439              MOVE '-'            TO H3-DASH.                      
02440                                                                   
02441      MOVE SW-CARR                TO H5-CARR.                      
02442      MOVE SW-COMP                TO H5-COMP.                      
02443      MOVE SW-STATE               TO H5-ST.                        
02444      MOVE SW-ACCT                TO H5-ACCT.                      
02445      MOVE '-'                    TO H5-FS1 H5-FS2 H5-TS1 H5-TS2.  
02446      MOVE SW-EFF-DT              TO WORK-DATE.                    
02447      MOVE WK-MO                  TO H5-FMO.                       
02448      MOVE WK-DA                  TO H5-FDA.                       
02449      MOVE WK-YR                  TO H5-FYR.                       
02450      MOVE SW-EXP-DT              TO WORK-DATE.                    
02451      MOVE WK-MO                  TO H5-TMO.                       
02452      MOVE WK-DA                  TO H5-TDA.                       
02453      MOVE WK-YR                  TO H5-TYR.                       
02454      MOVE SW-ACCUMS              TO PRINT-ACCUMS-R (1).           
02455      MOVE SW-CTL                 TO WS-SAVE-CTL.                  
02456      MOVE SW-GA-FLAG             TO SAVE-GA-FLAG.                 
02457      MOVE SW-RET-Y-N             TO SAVE-RET-Y-N.                 
02458      MOVE SW-RET-TYP             TO SAVE-RET-TYP.                 
02459      MOVE SW-LRET                TO LIFE-RETENT.                  
02460      MOVE SW-ARET                TO AH-RETENT.                    
02461      MOVE SW-RETRO-LIMIT         TO SAVE-RETRO-LIMIT.             
02462      MOVE SW-PREM-P-E            TO SAVE-PREM-P-E.                
02463      MOVE SW-CLMS-P-I            TO SAVE-CLMS-P-I.                
02464      MOVE SW-BEG-DATE            TO BEG-DATE.                     
02465                                                                   
02466      IF SW-RPT-TYPE-ID = 'B'                                      
02467          MOVE 'GROUP'            TO H4-GROUP                      
02468          MOVE SW-GA              TO H5-GROUP.                     
02469                                                                   
02470      MOVE SW-GA                  TO ZONE-GROUP.                   
02471                                                                   
02472      IF SW-RPT-TYPE-ID = 'B' AND ZON = 'ZON'                      
02473          MOVE '  ZONE'           TO H4-GROUP                      
02474          INSPECT H5-GROUP CONVERTING 'ZON' TO '   '.              
02475                                                                   
02476      IF SW-RPT-TYPE-ID = 'C'                                      
02477          MOVE ' G.A.'            TO H4-GROUP                      
02478          MOVE SW-GA              TO H5-GROUP.                     
02479                                                                   
02480      PERFORM 0730-RETURN-SORTED-FILE THRU 0730-EXIT.              
02481      EJECT                                                        
02482  0745-OUTPUT-PROCEDURE SECTION.                                   
02483      IF NO-OF-RECORDS-RELEASED = +0                               
02484         GO TO 1080-E-O-J.                                         
02485                                                                   
02486  0750-PRINT-REPORT SECTION.                                       
02487      PERFORM 0730-RETURN-SORTED-FILE THRU 0730-EXIT.              
02488                                                                   
02489      IF SW-RPT-TYPE-ID = 'B'                                      
02490          GO TO 0900-PRINT-GROUP-TOTALS.                           
02491                                                                   
02492      IF SW-RPT-TYPE-ID = 'C'                                      
02493          GO TO 0990-PRINT-GA-TOTALS.                              
02494                                                                   
02495      MOVE 'A'    TO H1-REPORT-SUF.                                
02496      PERFORM 0740-MOVE-SW-REC.                                    
02497      MOVE SPACES TO H5-MESSAGE.                                   
02498                                                                   
02499 *    IF SW-CARR = WS-SAVE-CAR                                     
02500 *        AND SW-COMP = WS-SAVE-CMP                                
02501 *        AND SW-STATE = WS-SAVE-ST                                
02502 *        AND SW-ACCT = WS-SAVE-ACC                                
02503 *            GO TO 0780-BREAK-1                                   
02504 *    ELSE                                                         
02505 *        GO TO 0800-BREAK-2.                                      
02506                                                                   
02507  0760-CHECK-BREAK.                                                
02508      IF SW-CARR NOT = WS-SAVE-CAR                                 
02509          GO TO 0860-BREAK-5.                                      
02510      IF SW-COMP NOT = WS-SAVE-CMP                                 
02511          GO TO 0840-BREAK-4.                                      
02512      IF SW-STATE NOT = WS-SAVE-ST                                 
02513          GO TO 0820-BREAK-3.                                      
02514      IF SW-ACCT NOT = WS-SAVE-ACC                                 
02515          GO TO 0800-BREAK-2.                                      
02516                                                                   
02517      GO TO 0780-BREAK-1.                                          
02518                                                                   
02519  EJECT                                                            
02520  0770-BREAK-RETURN.                                               
02521      PERFORM 0740-MOVE-SW-REC.                                    
02522                                                                   
02523      IF SW-CTL = HIGH-VALUES                                      
02524          PERFORM 0880-BREAK-6                                     
02525          GO TO 1080-E-O-J                                         
02526      ELSE                                                         
02527          IF SW-RPT-TYPE-ID = 'B'                                  
02528              PERFORM 0880-BREAK-6                                 
02529              GO TO 0900-PRINT-GROUP-TOTALS                        
02530          ELSE                                                     
02531              IF SW-RPT-TYPE-ID = 'C'                              
02532                  PERFORM 0880-BREAK-6                             
02533                  GO TO 0990-PRINT-GA-TOTALS                       
02534              ELSE                                                 
02535                  GO TO 0760-CHECK-BREAK.                          
02536                                                                   
02537  0780-BREAK-1.                                                    
02538      MOVE 'X'            TO BREAK-1-SWITCH.                       
02539      MOVE '1'            TO HEAD-SW.                              
02540      MOVE 'FROM      TO' TO H4-FT.                                
02541      MOVE +1             TO SG.                                   
02542      MOVE +3             TO SF.                                   
02543      MOVE +1             TO SH.                                   
02544                                                                   
02545      IF DTE-CLIENT NOT = 'ITG'                                    
02546          PERFORM 0440-GENERATE-RETRO THRU 0470-EXIT.              
02547                                                                   
02548      MOVE +1 TO SC.                                               
02549      MOVE +7 TO SF.                                               
02550      PERFORM 0530-ADD-UP-LEVELS 6 TIMES.                          
02551      PERFORM 0560-ZERO-PA VARYING SB FROM 1 BY 1                  
02552                           UNTIL SB GREATER 6.                     
02553                                                                   
02554  0790-BREAD-1-EXIT.                                               
02555      GO TO 0770-BREAK-RETURN.                                     
02556  EJECT                                                            
02557                                                                   
02558  0800-BREAK-2.                                                    
02559      IF BREAK-1-SWITCH NOT = ' '                                  
02560          PERFORM 0780-BREAK-1                                     
02561      ELSE                                                         
02562          MOVE +1 TO SC                                            
02563          MOVE +7 TO SF                                            
02564          PERFORM 0530-ADD-UP-LEVELS 6 TIMES                       
02565          PERFORM 0560-ZERO-PA VARYING SB FROM 1 BY 1              
02566                               UNTIL SB GREATER 6.                 
02567                                                                   
02568      MOVE SPACES TO BREAK-1-SWITCH.                               
02569      MOVE '2'    TO HEAD-SW.                                      
02570                                                                   
02571      MOVE SPACES TO H4-FT  H5-FMO  H5-FS1  H5-FDA  H5-FS2  H5-FYR 
02572                     H5-TMO  H5-TS1  H5-TDA  H5-TS2  H5-TYR.       
02573                                                                   
02574      IF FC-CODE = ' '  OR  '5'                                    
02575          MOVE PA-NW (9)             TO ANNUALIZED-PREM.           
02576      IF FC-CODE = '1'                                             
02577          COMPUTE ANNUALIZED-PREM = (PA-NW (9) / RD-MO) * 12.      
02578      IF FC-CODE = '2'                                             
02579          IF RD-MO = 01 OR 04 OR 07 OR 10                          
02580              COMPUTE ANNUALIZED-PREM =  PA-NW (9) * 12            
02581          ELSE                                                     
02582              IF RD-MO = 02 OR 05 OR 08 OR 11                      
02583                  COMPUTE ANNUALIZED-PREM =  PA-NW (9) * 6         
02584              ELSE                                                 
02585                  COMPUTE ANNUALIZED-PREM =  PA-NW (9) * 4.        
02586      IF FC-CODE = '3'                                             
02587          COMPUTE ANNUALIZED-PREM =  PA-NW (9) * 4.                
02588      IF FC-CODE = '4'                                             
02589          COMPUTE ANNUALIZED-PREM =  PA-NW (9) * 2.                
02590      IF FC-CODE = '6'                                             
02591          COMPUTE ANNUALIZED-PREM =  PA-NW (9) * 12.               
02592                                                                   
02593      IF ANNUALIZED-PREM GREATER THAN SAVE-RETRO-LIMIT             
02594          MOVE +2   TO SG                                          
02595          MOVE +9   TO SF                                          
02596          MOVE +1   TO SH                                          
02597          PERFORM 0440-GENERATE-RETRO THRU 0470-EXIT               
02598          MOVE +7   TO SC                                          
02599          MOVE +13  TO SF                                          
02600          PERFORM 0530-ADD-UP-LEVELS 6 TIMES.                      
02601                                                                   
02602      PERFORM 0560-ZERO-PA VARYING SB FROM 7 BY 1                  
02603                           UNTIL SB GREATER 12.                    
02604                                                                   
02605  0810-BREAK-2-EXIT.                                               
02606      GO TO 0770-BREAK-RETURN.                                     
02607                                                                   
02608  0820-BREAK-3.                                                    
02609      PERFORM 0800-BREAK-2.                                        
02610      MOVE '3'           TO HEAD-SW.                               
02611      MOVE WS-SAVE-ST    TO H8-ST.                                 
02612      MOVE HEAD-8-STATE  TO H2-ADDR.                               
02613      MOVE SPACES        TO H1-NAME  H3-ADDR  H3-ZIP.              
02614      MOVE +3            TO SG.                                    
02615      MOVE +15           TO SF.                                    
02616      PERFORM 0440-GENERATE-RETRO THRU 0470-EXIT.                  
02617      MOVE +13           TO SC.                                    
02618      MOVE +19           TO SF.                                    
02619      PERFORM 0530-ADD-UP-LEVELS 6 TIMES.                          
02620      PERFORM 0560-ZERO-PA VARYING SB FROM 13 BY 1                 
02621                           UNTIL SB GREATER 18.                    
02622                                                                   
02623      MOVE BALANCE-LINE-1 TO WS-BAL-LINE-1.                        
02624      MOVE BALANCE-LINE-2 TO WS-BAL-LINE-2.                        
02625                                                                   
02626  0830-BREAK-3-EXIT.                                               
02627      GO TO 0770-BREAK-RETURN.                                     
02628                                                                   
02629  EJECT                                                            
02630  0840-BREAK-4.                                                    
02631      PERFORM 0820-BREAK-3.                                        
02632      MOVE '4'         TO HEAD-SW.                                 
02633      MOVE WS-SAVE-CMP TO H8-COMP.                                 
02634      MOVE HEAD-8-COMP TO H2-ADDR.                                 
02635      MOVE +4          TO SG.                                      
02636      MOVE +21         TO SF.                                      
02637      PERFORM 0440-GENERATE-RETRO THRU 0470-EXIT.                  
02638      MOVE +19         TO SC.                                      
02639      MOVE +25         TO SF.                                      
02640      PERFORM 0530-ADD-UP-LEVELS 6 TIMES.                          
02641      PERFORM 0560-ZERO-PA VARYING SB FROM 19 BY 1                 
02642                           UNTIL SB GREATER 24.                    
02643                                                                   
02644  0850-BREAK-4-EXIT.                                               
02645      GO TO 0770-BREAK-RETURN.                                     
02646                                                                   
02647  0860-BREAK-5.                                                    
02648      PERFORM 0840-BREAK-4.                                        
02649      MOVE '5'         TO HEAD-SW.                                 
02650      MOVE WS-SAVE-CAR TO H8-CARR.                                 
02651      MOVE HEAD-8-CARR TO H2-ADDR.                                 
02652      MOVE +5          TO SG.                                      
02653      MOVE +27         TO SF.                                      
02654      PERFORM 0440-GENERATE-RETRO THRU 0470-EXIT.                  
02655      MOVE +25         TO SC.                                      
02656      MOVE +31         TO SF.                                      
02657      PERFORM 0530-ADD-UP-LEVELS 6 TIMES.                          
02658      PERFORM 0560-ZERO-PA VARYING SB FROM 25 BY 1                 
02659                           UNTIL SB GREATER 30.                    
02660                                                                   
02661  0870-BREAK-5-EXIT.                                               
02662      GO TO 0770-BREAK-RETURN.                                     
02663  EJECT                                                            
02664                                                                   
02665  0880-BREAK-6.                                                    
02666      PERFORM 0860-BREAK-5.                                        
02667      MOVE '6'          TO HEAD-SW.                                
02668      MOVE HEAD-8-FINAL TO H2-ADDR.                                
02669      MOVE +6           TO SG.                                     
02670      MOVE +33          TO SF.                                     
02671      PERFORM 0440-GENERATE-RETRO THRU 0470-EXIT.                  
02672                                                                   
02673  0890-BREAK-6-EXIT.                                               
02674      EXIT.                                                        
02675  EJECT                                                            
02676                                                                   
02677  0900-PRINT-GROUP-TOTALS.                                         
02678      PERFORM 0560-ZERO-PA VARYING SB FROM 1 BY 1                  
02679                           UNTIL SB GREATER 36.                    
02680      MOVE 'B' TO H1-REPORT-SUF.                                   
02681      PERFORM 0740-MOVE-SW-REC.                                    
02682                                                                   
02683  0910-CHECK-GROUP-BREAK.                                          
02684      IF SW-GA NOT = WS-SAVE-GA                                    
02685          GO TO 0970-BREAK-GROUP-3.                                
02686                                                                   
02687      IF SW-CARR  NOT = WS-SAVE-CAR OR                             
02688         SW-COMP  NOT = WS-SAVE-CMP OR                             
02689         SW-STATE NOT = WS-SAVE-ST  OR                             
02690         SW-ACCT  NOT = WS-SAVE-ACC                                
02691            GO TO 0950-BREAK-GROUP-2.                              
02692                                                                   
02693      GO TO 0930-BREAK-GROUP-1.                                    
02694                                                                   
02695  0920-BREAK-GROUP-RETURN.                                         
02696      PERFORM 0740-MOVE-SW-REC.                                    
02697                                                                   
02698      IF SW-CTL = HIGH-VALUES                                      
02699          PERFORM 0970-BREAK-GROUP-3                               
02700          GO TO 1080-E-O-J                                         
02701      ELSE                                                         
02702          IF SW-RPT-TYPE-ID = 'C'                                  
02703              PERFORM 0970-BREAK-GROUP-3                           
02704              GO TO 0990-PRINT-GA-TOTALS                           
02705          ELSE                                                     
02706              GO TO 0910-CHECK-GROUP-BREAK.                        
02707  EJECT                                                            
02708                                                                   
02709  0930-BREAK-GROUP-1.                                              
02710      MOVE +1 TO SC.                                               
02711      MOVE +7 TO SF.                                               
02712      PERFORM 0530-ADD-UP-LEVELS 6 TIMES.                          
02713      PERFORM 0560-ZERO-PA VARYING SB FROM 1 BY 1                  
02714                           UNTIL SB GREATER 6.                     
02715                                                                   
02716  0940-BREAK-GROUP-1-EXIT.                                         
02717      GO TO 0920-BREAK-GROUP-RETURN.                               
02718                                                                   
02719  0950-BREAK-GROUP-2.                                              
02720      PERFORM 0930-BREAK-GROUP-1.                                  
02721      MOVE '2'    TO HEAD-SW.                                      
02722      MOVE SPACES TO H4-FT  H5-FMO  H5-FS1  H5-FDA  H5-FS2  H5-FYR 
02723                     H5-TMO H5-TS1  H5-TDA  H5-TS2  H5-TYR.        
02724      MOVE +2     TO SG.                                           
02725      MOVE +9     TO SF.                                           
02726      MOVE +1     TO SH.                                           
02727      PERFORM 0440-GENERATE-RETRO THRU 0470-EXIT.                  
02728      MOVE +7     TO SC.                                           
02729      MOVE +13    TO SF.                                           
02730      PERFORM 0530-ADD-UP-LEVELS 6 TIMES.                          
02731      PERFORM 0560-ZERO-PA VARYING SB FROM 7 BY 1                  
02732                           UNTIL SB GREATER 12.                    
02733                                                                   
02734  0960-BREAK-GROUP-2-EXIT.                                         
02735      GO TO 0920-BREAK-GROUP-RETURN.                               
02736                                                                   
02737  0970-BREAK-GROUP-3.                                              
02738      PERFORM 0950-BREAK-GROUP-2.                                  
02739      MOVE '3'        TO HEAD-SW.                                  
02740      MOVE WS-SAVE-GA TO H8-GROUP  ZONE-GROUP.                     
02741      MOVE '  GROUP'  TO H-8-G.                                    
02742                                                                   
02743      IF ZON = 'ZON'                                               
02744          INSPECT H8-GROUP CONVERTING 'ZON' TO '   '               
02745          MOVE '  ZONE' TO H-8-G.                                  
CIDMOD*                                                                 
CIDMOD*  SKIP 1 ADDITIONAL PAGE TO PRINT GROUP TOTALS ON A              
CIDMOD*  SEPARATE PAGE BY ITSELF.                                       
CIDMOD*                                                                 
CIDMOD     MOVE '1'              TO X.                                  
CIDMOD     MOVE SPACE-LINE       TO P-DATA.                             
CIDMOD     PERFORM 0610-PRT-RTN THRU 0630-EXIT.                         
02746                                                                   
02747      MOVE HEAD-8-GROUP TO H2-ADDR.                                
02748      MOVE SPACES       TO H1-NAME  H3-ADDR  H3-ZIP.               
02749      MOVE +3           TO SG.                                     
02750      MOVE +15          TO SF.                                     
02751      PERFORM 0440-GENERATE-RETRO THRU 0470-EXIT.                  
CIDMOD*                                                                 
CIDMOD*  SKIP 1 ADDITIONAL PAGE TO PRINT GROUP TOTALS ON A              
CIDMOD*  SEPARATE PAGE BY ITSELF.                                       
CIDMOD*                                                                 
CIDMOD     MOVE '1'              TO X.                                  
CIDMOD     MOVE SPACE-LINE       TO P-DATA.                             
CIDMOD     PERFORM 0610-PRT-RTN THRU 0630-EXIT.                         
CIDMOD                                                                  
02752      MOVE +13          TO SC.                                     
02753      MOVE +19          TO SF.                                     
02754      PERFORM 0530-ADD-UP-LEVELS 6 TIMES.                          
02755      PERFORM 0560-ZERO-PA VARYING SB FROM 13 BY 1                 
02756                           UNTIL SB GREATER 18.                    
02757                                                                   
02758  0980-BREAK-GROUP-3-EXIT.                                         
02759      GO TO 0920-BREAK-GROUP-RETURN.                               
02760  EJECT                                                            
02761  0990-PRINT-GA-TOTALS.                                            
02762      PERFORM 0560-ZERO-PA VARYING SB FROM 1 BY 1                  
02763                           UNTIL SB GREATER 36.                    
02764      MOVE 'C' TO H1-REPORT-SUF.                                   
02765      PERFORM 0740-MOVE-SW-REC.                                    
02766                                                                   
02767  1000-CHECK-GA-BREAK.                                             
02768      IF SW-GA NOT = WS-SAVE-GA                                    
02769          GO TO 1060-BREAK-GA-3.                                   
02770                                                                   
02771      IF SW-CARR  NOT = WS-SAVE-CAR  OR                            
02772         SW-COMP  NOT = WS-SAVE-CMP  OR                            
02773         SW-STATE NOT = WS-SAVE-ST   OR                            
02774         SW-ACCT  NOT = WS-SAVE-ACC                                
02775            GO TO 1040-BREAK-GA-2.                                 
02776                                                                   
02777      GO TO 1020-BREAK-GA-1.                                       
02778                                                                   
02779  1010-BREAK-GA-RETURN.                                            
02780      PERFORM 0740-MOVE-SW-REC.                                    
02781                                                                   
02782      IF SW-CTL = HIGH-VALUES                                      
02783          PERFORM 1060-BREAK-GA-3                                  
02784          GO TO 1080-E-O-J.                                        
02785                                                                   
02786      GO TO 1000-CHECK-GA-BREAK.                                   
02787                                                                   
02788  1020-BREAK-GA-1.                                                 
02789      MOVE +1 TO SC.                                               
02790      MOVE +7 TO SF.                                               
02791      PERFORM 0530-ADD-UP-LEVELS 6 TIMES.                          
02792      PERFORM 0560-ZERO-PA VARYING SB FROM 1 BY 1                  
02793                           UNTIL SB GREATER 6.                     
02794                                                                   
02795  1030-BREAK-GA-1-EXIT.                                            
02796      GO TO 1010-BREAK-GA-RETURN.                                  
02797                                                                   
02798  1040-BREAK-GA-2.                                                 
02799      PERFORM 1020-BREAK-GA-1.                                     
02800                                                                   
02801      IF LIFE-RETENT = ZERO                                        
02802          MOVE ZEROS TO PA-NBA (7) PA-NBA (10).                    
02803                                                                   
02804      IF AH-RETENT = ZERO                                          
02805          MOVE ZEROS TO PA-NBA (8) PA-NBA (11).                    
02806                                                                   
02807      ADD PA-NBA (7) PA-NBA (8) GIVING PA-NBA (9).                 
02808      ADD PA-NBA (10) PA-NBA (11) GIVING PA-NBA (12).              
02809                                                                   
02810      MOVE '2' TO HEAD-SW.                                         
02811      MOVE SPACES TO H4-FT  H5-FMO  H5-FS1  H5-FDA  H5-FS2  H5-FYR 
02812                     H5-TMO H5-TS1  H5-TDA  H5-TS2  H5-TYR.        
02813                                                                   
02814      MOVE +2 TO SG.                                               
02815      MOVE +9 TO SF.                                               
02816      MOVE +1 TO SH.                                               
02817      PERFORM 0440-GENERATE-RETRO THRU 0470-EXIT.                  
02818                                                                   
02819      IF PA-NBA (7) LESS +0                                        
02820          ADD PA-NBA (7) TO PA-NBA (8)                             
02821          MOVE +0 TO PA-NBA (7)                                    
02822          IF PA-NBA (8) LESS +0                                    
02823              MOVE +0 TO PA-NBA (8).                               
02824                                                                   
02825      IF PA-NBA (8) LESS +0                                        
02826          ADD PA-NBA (8) TO PA-NBA (7)                             
02827          MOVE +0 TO PA-NBA (8)                                    
02828          IF PA-NBA (7) LESS +0                                    
02829              MOVE +0 TO PA-NBA (7).                               
02830                                                                   
02831      COMPUTE PA-NBA (9) = PA-NBA (7) + PA-NBA (8).                
02832                                                                   
02833      IF SAVE-RET-Y-N = SPACE  OR  'N'                             
02834          MOVE ZEROS  TO  PA-NBA (7)  PA-NBA (8)  PA-NBA (9).      
02835                                                                   
02836      IF PA-NBA (10) LESS +0                                       
02837          ADD PA-NBA (10) TO PA-NBA (11)                           
02838          MOVE +0 TO PA-NBA (10)                                   
02839          IF PA-NBA (11) LESS +0                                   
02840              MOVE +0 TO PA-NBA (11).                              
02841                                                                   
02842      IF PA-NBA (11) LESS +0                                       
02843          ADD PA-NBA (11) TO PA-NBA (10)                           
02844          MOVE +0 TO PA-NBA (11)                                   
02845          IF PA-NBA (10) LESS +0                                   
02846              MOVE +0 TO PA-NBA (10).                              
02847                                                                   
02848      COMPUTE PA-NBA (12) = PA-NBA (10) + PA-NBA (11).             
02849                                                                   
02850      IF SAVE-RET-Y-N = SPACE  OR  'N'                             
02851          MOVE ZEROS  TO  PA-NBA (10)  PA-NBA (11)  PA-NBA (12).   
02852                                                                   
02853      MOVE +7  TO SC.                                              
02854      MOVE +13 TO SF.                                              
02855      PERFORM 0530-ADD-UP-LEVELS 6 TIMES.                          
02856      PERFORM 0560-ZERO-PA VARYING SB FROM 7 BY 1                  
02857                           UNTIL SB GREATER 12.                    
02858                                                                   
02859  1050-BREAK-GA-2-EXIT.                                            
02860      GO TO 1010-BREAK-GA-RETURN.                                  
02861  EJECT                                                            
02862                                                                   
02863  1060-BREAK-GA-3.                                                 
02864      PERFORM 1040-BREAK-GA-2.                                     
02865      MOVE '3'        TO HEAD-SW.                                  
02866      MOVE WS-SAVE-GA TO H8-GA.                                    
02867      MOVE HEAD-8-GA  TO H2-ADDR.                                  
02868      MOVE SPACES     TO H1-NAME  H3-ADDR  H3-ZIP.                 
02869 *    MOVE +1.0       TO LIFE-RETENT AH-RETENT.                    
02870                                                                   
02871      IF DTE-CLIENT = 'GIC'                                        
02872          MOVE 'P' TO SAVE-RET-TYP                                 
02873      ELSE                                                         
02874          MOVE 'E' TO SAVE-RET-TYP.                                
02875                                                                   
02876      MOVE +3    TO SG.                                            
02877      MOVE +15   TO SF.                                            
02878      MOVE +2    TO SH.                                            
02879      MOVE LIFE-RETENT TO RETENT-FACT.                             
02880      MOVE +13   TO SC.                                            
02881      MOVE +15   TO SF.                                            
02882      PERFORM 0420-CALCULATE-GA THRU 0430-EXIT.                    
02883      MOVE AH-RETENT TO RETENT-FACT.                               
02884      MOVE +14   TO SC.                                            
02885      PERFORM 0420-CALCULATE-GA THRU 0430-EXIT.                    
02886      MOVE LIFE-RETENT TO RETENT-FACT.                             
02887      MOVE +16   TO SC.                                            
02888      MOVE +18   TO SF.                                            
02889      PERFORM 0420-CALCULATE-GA THRU 0430-EXIT.                    
02890      MOVE AH-RETENT TO RETENT-FACT.                               
02891      MOVE +17 TO SC.                                              
02892      PERFORM 0420-CALCULATE-GA THRU 0430-EXIT.                    
02893                                                                   
02894      ADD PA-PE (13) PA-PE (14) GIVING PA-PE (15).                 
02895      ADD PA-PE (16) PA-PE (17) GIVING PA-PE (18).                 
02896                                                                   
02897      MOVE +15 TO SF.                                              
02898                                                                   
02899      PERFORM 0440-GENERATE-RETRO THRU 0470-EXIT.                  
02900      PERFORM 0560-ZERO-PA VARYING SB FROM 13 BY 1                 
02901                           UNTIL SB GREATER 18.                    
02902                                                                   
02903  1070-BREAK-GA-3-EXIT.                                            
02904      GO TO 1010-BREAK-GA-RETURN.                                  
02905  EJECT                                                            
02906  1080-E-O-J.                                                      
02907      CLOSE                                                        
02908          PRNTR.                                                   
02909                                                                   
02910  1090-CLOSE-FICH.                                                 
02911                              COPY ELCPRTC.                        
02912                                                                   
02913  1090-CLOSE-EXIT.                                                 
02914      EXIT.                                                        
02915                                                                   
02916  COPY ELCDCS.                                                     
02917                                                                   
02918  ABEND-PGM SECTION.                                               
02919                              COPY ELCABEND SUPPRESS.              
