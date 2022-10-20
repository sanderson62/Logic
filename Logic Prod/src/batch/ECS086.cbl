00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                ECS086.                               
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 02/09/96 07:52:22.                 
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          
00008 *                           VMOD=2.008                            
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
00026 *        PRINT MORTALITY RESERVES IN DETAIL.                      
00027 *        PROGRAM OPTION 1 = DETAIL                                
00028 *                       2 = CELL                                  
00029 *                       3 = SUMMARY                               
00030                                                                   
020113******************************************************************
020113*                   C H A N G E   L O G
020113*
020113* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
020113*-----------------------------------------------------------------
020113*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
020113* EFFECTIVE    NUMBER
020113*-----------------------------------------------------------------
020113* 020113  IR2013020100001  PEMA  INCREASE # OF MORT TABLES
110821* 110821  IR2021102600001  PEMA  Handle no input properly
020113******************************************************************
00031  ENVIRONMENT DIVISION.                                            
00032  INPUT-OUTPUT SECTION.                                            
00033  FILE-CONTROL.                                                    
00034                                                                   
00035      SELECT SORT-WORK ASSIGN TO SYS001-DA-FBA1-S-SORTWK1.         
00036      SELECT PRNTR     ASSIGN TO SYS008-UR-1403-S-SYS008.          
00037      SELECT RESV-ANS  ASSIGN TO SYS012-UT-2400-S-SYS012.          
00038      SELECT DISK-DATE ASSIGN TO SYS019-UT-FBA1-S-SYS019.          
00039      SELECT FICH      ASSIGN TO SYS020-UT-2400-S-SYS020.          
00040  EJECT                                                            
00041  DATA DIVISION.                                                   
00042  FILE SECTION.                                                    
00043                                                                   
00044  SD  SORT-WORK.                                                   
00045                              COPY ECSGAP01.                       
00046  EJECT                                                            
00047  FD  PRNTR                                                        
00048                              COPY ELCPRTFD.                       
00049  EJECT                                                            
00050  FD  RESV-ANS                                                     
00051                              COPY ECSGAPFD.                       
00052  EJECT                                                            
00053  FD  DISK-DATE                                                    
00054                              COPY ELCDTEFD.                       
00055  EJECT                                                            
00056  FD  FICH                                                         
00057                              COPY ECSFICH.                        
00058  EJECT                                                            
00059  WORKING-STORAGE SECTION.                                         
00060  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      
00061  77  FILLER  PIC X(32) VALUE '********************************'.  
00062  77  FILLER  PIC X(32) VALUE '     ECS086 WORKING-STORAGE     '.  
00063  77  FILLER  PIC X(32)   VALUE '*********** VMOD=2.008**********'.
00064                                                                   
00065  77  LINE-CT                 PIC S99     COMP    VALUE +85.       
CIDMOD 77  WS-50                   PIC S99     COMP    VALUE +50.       
00066  77  WS-56                   PIC S99     COMP    VALUE +56.       
00067  77  WS-76                   PIC S99     COMP    VALUE +76.       
00068  77  X1                      PIC S999    COMP    VALUE +0.        
00069  77  CA                      PIC S999    COMP    VALUE +0.        
00070  77  CB                      PIC S999    COMP    VALUE +0.        
00071  77  SAV-CA                  PIC S999    COMP    VALUE +0.        
00072  77  SAV-CB                  PIC S999    COMP    VALUE +0.        
00073  77  PGM-SUB                 PIC S999    COMP    VALUE +086.      
00074  77  PAGE-CT                 PIC S9(5)   COMP-3  VALUE +0.        
LGC191 77  SELECT-CT               PIC S9999   COMP-3  VALUE +4000.     
00075  77  X                       PIC X               VALUE SPACE.     
00076  77  REIN-SW                 PIC X               VALUE 'X'.       
00077  77  DD-SW                   PIC X               VALUE 'X'.       
00078  77  EOJ-SW                  PIC X               VALUE SPACES.    
00079  77  R-SW                    PIC 99              VALUE 01.        
110821 77  ws-release-cnt          pic s9(9)  comp-3   value +0.
00080  EJECT                                                            
00081  01  HD1.                                                         
00082      12  FILLER              PIC X(42)           VALUE SPACES.    
00083      12  FILLER              PIC X(39)           VALUE            
00084              'MORTALITY AND UNEARNED PREMIUM RESERVES'.           
00085      12  FILLER              PIC X(38)           VALUE SPACES.    
00086      12  FILLER              PIC X(7)            VALUE 'ECS086 '. 
00087                                                                   
00088  01  HD2.                                                         
00089      12  FILLER              PIC X(47)           VALUE SPACES.    
00090      12  HD-COMP             PIC X(30).                           
00091      12  FILLER              PIC X(42)           VALUE SPACES.    
00092      12  HD-RDT              PIC X(8).                            
00093                                                                   
00094  01  HD2A.                                                        
00095      12  FILLER              PIC X(37)           VALUE SPACES.    
00096      12  FILLER              PIC X(25)           VALUE            
00097              'REDUCING RESERVE FACTORS '.                         
00098      12  FILLER              PIC X(25)           VALUE            
00099              'PER $100 MONTHLY DECREASE'.                         
00100                                                                   
00101  01  HD2AA.                                                       
00102      12  FILLER              PIC X(44)           VALUE SPACES.    
00103      12  FILLER              PIC X(21)           VALUE            
00104              'LEVEL RESERVE FACTORS'.                             
00105      12  FILLER              PIC X(22)           VALUE            
00106              ' PER $1000 FACE       '.                            
00107                                                                   
00108  01  HD2AAA.                                                      
00109      12  FILLER              PIC X(37)           VALUE SPACES.    
00110      12  FILLER              PIC X(24)           VALUE            
00111              'NET PAY RESERVE FACTORS '.                          
00112      12  FILLER              PIC X(26)           VALUE            
00113              'USE COMMUTATION FUNCTIONS '.                        
00114                                                                   
00115  01  HD3.                                                         
00116      12  HD-REIN             PIC X(13)           VALUE SPACES.    
00117      12  FILLER              PIC X(40)           VALUE SPACES.    
00118      12  HD-DT               PIC X(18).                           
00119      12  FILLER              PIC X(48)           VALUE SPACES.    
00120      12  FILLER              PIC X(5)            VALUE 'PAGE '.   
00121      12  HD-PG               PIC ZZ,ZZ9.                          
00122                                                                   
00123  01  HD3AA.                                                       
00124      12  FILLER              PIC X(12)           VALUE            
00125              ' GROUPING - '.                                      
00126      12  HD3-CO              PIC X(6).                            
00127      12  FILLER              PIC X(12)           VALUE            
00128              '    STATE - '.                                      
00129      12  HD3-ST              PIC XX.                              
00130      12  FILLER              PIC X               VALUE SPACES.    
00131      12  HD3-STN             PIC X(20).                           
00132      12  FILLER              PIC X(9)            VALUE SPACES.    
00133      12  FILLER              PIC X(18)           VALUE            
00134              'MORTALITY TABLE - '.                                
00135      12  HD3-MRT             PIC X(26)           VALUE SPACES.    
00136      12  FILLER              PIC X(10)           VALUE            
00137              '  LIVES - '.                                        
00138      12  HD3-LVS             PIC X(10)           VALUE SPACES.    
00139                                                                   
00140  01  HD3AAA.                                                      
00141      12  FILLER              PIC X(11)           VALUE            
00142              ' CARRIER - '.                                       
00143      12  HD3-CAR             PIC X.                               
00144      12  FILLER              PIC X(120)          VALUE SPACES.    
00145                                                                   
00146  01  HD4A.                                                        
00147      12  FILLER              PIC X(62)           VALUE SPACES.    
00148      12  FILLER              PIC X(16)           VALUE            
00149              'INTEREST RATE - '.                                  
00150      12  HD-INT              PIC 9.9.                             
00151      12  FILLER              PIC XX              VALUE ' %'.      
00152                                                                   
00153  01  HD5A.                                                        
00154      12  FILLER              PIC X(71)           VALUE SPACES.    
00155      12  FILLER              PIC X(7)            VALUE 'TYPE -'.  
00156      12  HD-IG               PIC X(15).                           
00157                                                                   
00158  01  HD6A.                                                        
00159      12  FILLER              PIC X(48)           VALUE            
00160              '  ACCOUNT     CERT.      EFF.   ISS VAL ORIG REM'.  
00161      12  FILLER              PIC X(35)           VALUE            
00162              '  APR OR     C   INITIAL  REMAINING'.               
00163      12  FILLER              PIC X(49)           VALUE            
00164              '  MONTHLY    MORT.      ALT.    ORIG.  STATE STAT'. 
00165                                                                   
00166  01  HD7A.                                                        
00167      12  FILLER              PIC X(48)           VALUE            
00168              '  NUMBER     NUMBER      DATE   AGE AGE TERM TRM'.  
00169      12  FILLER              PIC X(35)           VALUE            
00170              ' PMT FREQ    T   AMOUNT    AMOUNT  '.               
00171      12  FILLER              PIC X(49)           VALUE            
00172              '  DECREASE  RESERVE   RESERVE  PREMIUM  UNEARNED '. 
00173                                                                   
00174  01  H6-CELL.                                                     
00175      12  H6-CO-ST.                                                
00176          16  H6-CL           PIC X(42).                           
00177          16  FILLER          PIC X(8).                            
00178      12  FILLER              PIC X(82).                           
00179                                                                   
00180  01  H7-CELL.                                                     
00181      12  H7-CO-ST.                                                
00182          16  H7-CL           PIC X(42).                           
00183          16  FILLER          PIC X(8).                            
00184      12  FILLER              PIC X(82).                           
00185  EJECT                                                            
00186  01  CNTRL-FIELDS.                                                
00187      12  WS-RETURN-CODE      PIC S9(4)   COMP.                    
00188      12  WS-ABEND-MESSAGE    PIC X(80).                           
00189      12  WS-ABEND-FILE-STATUS PIC XX.                             
00190      12  WS-ZERO             PIC S9      COMP-3  VALUE +0.        
00191      12  LAS-CAR             PIC X               VALUE SPACES.    
00192      12  LAS-COMP            PIC X(6)            VALUE SPACES.    
00193      12  LAS-ST              PIC XX              VALUE SPACES.    
00194      12  LAS-TAB             PIC X(4)            VALUE SPACES.    
00195      12  LAS-INT-R           PIC XX.                              
00196      12  LAS-INT REDEFINES                                        
00197          LAS-INT-R           PIC 9V9.                             
00198      12  LAS-GRP             PIC X               VALUE SPACES.    
00199      12  LAS-TYPE            PIC XX              VALUE ZEROS.     
00200      12  LAS-AGE             PIC 99              VALUE ZERO.      
00201      12  LAS-RTRM            PIC S999            VALUE ZERO.      
00202      12  LAS-FAC             PIC 9(5)V9(4).                       
00203      12  LAS-CODE            PIC X(4)            VALUE SPACES.    
00204                                                                   
00205  01  MISC-AREAS.                                                  
00206      12  WK-INT.                                                  
00207          16  WK-INT2         PIC 99.                              
00208          16  FILLER          PIC 9               VALUE ZERO.      
00209      12  SAVE-ZERO           PIC S9(7)V99 COMP-3 VALUE +0.        
00210      12  TEMPRT              PIC X.                               
00211      12  SUB-A               PIC  S999    COMP-3 VALUE +0.        
00212      12  NET-SW              PIC X               VALUE SPACE.     
00213  EJECT                                                            
00214  01  LN-1.                                                        
00215      12  L1-ACCT             PIC X(10).                           
00216      12  FILLER              PIC X.                               
00217      12  L1-CERT             PIC X(11).                           
00218      12  FILLER              PIC X.                               
00219      12  L1-MO               PIC XX.                              
00220      12  L1-MOD              PIC X.                               
00221      12  L1-DA               PIC XX.                              
00222      12  L1-DAD              PIC X.                               
00223      12  L1-YR               PIC XX.                              
00224      12  FILLER              PIC XX.                              
00225      12  L1-IAGE             PIC Z9.                              
00226      12  FILLER              PIC XX.                              
00227      12  L1-VAGE             PIC Z9.                              
00228      12  FILLER              PIC X.                               
00229      12  L1-TRM              PIC ZZ9.                             
00230      12  FILLER              PIC XX.                              
00231      12  L1-RTRM             PIC ZZ9.                             
00232      12  FILLER              PIC X.                               
00233      12  L1-APR              PIC ZZ9.999.                         
00234      12  L1-PMF REDEFINES L1-APR.                                 
00235          16  L1-PMS          PIC X(5).                            
00236          16  L1-PM           PIC ZZ.                              
00237      12  FILLER              PIC XXX.                             
00238      12  L1-CNT              PIC ZZ9.                             
00239      12  FILLER              PIC X(7).                            
00240      12  L1-RAMT             PIC ZZZ,ZZZ,ZZZ.99- BLANK WHEN ZERO. 
00241      12  FILLER              PIC X(7).                            
00242      12  L1-RESV             PIC Z,ZZZ,ZZZ.99-   BLANK WHEN ZERO. 
00243      12  FILLER              PIC X(5).                            
00244      12  L1-O-PREM           PIC Z,ZZZ,ZZZ.99-   BLANK WHEN ZERO. 
00245      12  FILLER              PIC X(10).                           
00246                                                                   
00247  01  LN-2.                                                        
00248      12  FILLER              PIC X(59).                           
00249      12  L2-AMT              PIC ZZZ,ZZZ,ZZZ.99- BLANK WHEN ZERO. 
00250      12  FILLER              PIC X(5).                            
00251      12  L2-DEX              PIC ZZZ,ZZZ,ZZZ.99- BLANK WHEN ZERO. 
00252      12  FILLER              PIC X(5).                            
00253      12  L2-ALT-RESV         PIC Z,ZZZ,ZZZ.99-   BLANK WHEN ZERO. 
00254      12  FILLER              PIC X(7).                            
00255      12  L2-ST-STAT          PIC Z,ZZZ,ZZZ.99-   BLANK WHEN ZERO. 
00256                                                                   
00257  01  LN-3.                                                        
00258      12  L3-TOT-DES      PIC X(54).                               
00259      12  L3-CNT          PIC ZZZZ,ZZ9.                            
00260      12  FILLER          PIC X(4).                                
00261      12  L3-RAMT         PIC ZZ,ZZZ,ZZZ,ZZZ.99- BLANK WHEN ZERO.  
00262      12  FILLER          PIC X(5).                                
00263      12  L3-RESV         PIC ZZZ,ZZZ,ZZZ.99- BLANK WHEN ZERO.     
00264      12  FILLER          PIC X(3).                                
00265      12  L3-PREM         PIC ZZZ,ZZZ,ZZZ.99- BLANK WHEN ZERO.     
00266      12  FILLER          PIC X(10).                               
00267                                                                   
00268  01  LN-4.                                                        
00269      12  FILLER          PIC X(76).                               
00270      12  L4-DEC          PIC ZZ,ZZZ,ZZZ,ZZZ.99- BLANK WHEN ZERO.  
00271      12  FILLER          PIC X(3).                                
00272      12  L4-ALT-RESV     PIC ZZZ,ZZZ,ZZZ.99- BLANK WHEN ZERO.     
00273      12  FILLER          PIC X(5).                                
00274      12  L4-ST-STAT      PIC ZZZ,ZZZ,ZZZ.99- BLANK WHEN ZERO.     
00275                                                                   
00276  01  LN-3-FRONTS.                                                 
00277      12  L3-TERM-DESC.                                            
00278          16  FILLER          PIC X(6)            VALUE            
00279                  '*TERM '.                                        
00280          16  L3-RTRM         PIC ZZ9.                             
00281          16  FILLER          PIC X(7)            VALUE ' TOTAL'.  
00282          16  FILLER          PIC X(10)           VALUE            
00283                  '(FACTOR = '.                                    
00284          16  L3-FAC          PIC Z(5).9(4).                       
00285          16  FILLER          PIC X               VALUE ')'.       
00286      12  L3-AGE-DESC.                                             
00287          16  FILLER          PIC X(7)            VALUE            
00288                  '  *AGE '.                                       
00289          16  L3-AGE          PIC Z9.                              
00290          16  FILLER          PIC X(6)            VALUE            
00291                  ' TOTAL'.                                        
00292      12  L3-TYPE-DESC.                                            
00293          16  FILLER          PIC X(5)            VALUE '    *'.   
00294          16  FILLER          PIC X(5)            VALUE 'PLAN '.   
00295          16  L3-PLN          PIC XX.                              
00296          16  FILLER          PIC X(7)            VALUE ' TOTAL '. 
00297          16  L3-TYP          PIC X(10)           VALUE SPACES.    
00298      12  L3-FRONT-1.                                              
00299          16  FILLER          PIC X(7)            VALUE '      *'. 
00300          16  L3-GRP-DES      PIC X(42)           VALUE SPACES.    
00301      12  L3-INT-DESC.                                             
00302          16  FILLER          PIC X(23)           VALUE            
00303                  '        *INTEREST RATE '.                       
00304          16  L3-INT-DEST     PIC 9.9.                             
00305          16  FILLER          PIC X(17)           VALUE '% TOTAL'. 
00306      12  L3-TAB-DESC.                                             
00307          16  FILLER          PIC X(15)           VALUE            
00308                  '        *TABLE '.                               
00309          16  L3-TBL          PIC X(3).                            
00310          16  FILLER          PIC X               VALUE SPACE.     
00311          16  L3-TAB          PIC X(28).                           
00312      12  L3-ST-LINE.                                              
00313          16  FILLER          PIC X(17)           VALUE            
00314                  '          *STATE '.                             
00315          16  L3-ST-NO        PIC XX.                              
00316          16  FILLER          PIC X(28)           VALUE ' TOTAL'.  
00317      12  L3-CO-LINE.                                              
00318          16  FILLER          PIC X(22)           VALUE            
00319                  '            *GROUPING'.                         
00320          16  L3-COMP-NO      PIC X(6).                            
00321          16  FILLER          PIC X(21)           VALUE ' TOTAL'.  
00322      12  L3-CAR-LINE.                                             
00323          16  FILLER          PIC X(24)           VALUE            
00324                  '              *CARRIER  '.                      
00325          16  L3-CA-NO        PIC X.                               
00326          16  FILLER          PIC X(6)            VALUE ' TOTAL'.  
00327      12  L3-FINAL-LINE.                                           
00328          16  FILLER          PIC X(49)           VALUE            
00329                  '                * FINAL REPORT TOTALS'.         
00330                                                                   
00331  01  TOTAL-AREAS         COMP-3    SYNC.                          
00332      12  TOT-RAMT        OCCURS 22.                               
00333          16  T-RAMT          PIC S9(11)V99.                       
00334      12  TOT-MDEC        OCCURS 22.                               
00335          16  T-MDEC          PIC S9(11)V99.                       
00336      12  TOT-MRES        OCCURS 22.                               
00337          16  T-MRES          PIC S9(9)V9(6).                      
00338      12  TOT-ALT-MRES    OCCURS 22.                               
00339          16  T-ALT-MRES      PIC S9(9)V9(6).                      
00340      12  TOT-UNERN       OCCURS 22.                               
00341          16  T-UNERN         PIC S9(9)V99.                        
00342      12  TOT-CNT         OCCURS 22.                               
00343          16  T-CNT           PIC S9(7).                           
00344      12  TOT-PREM        OCCURS 22.                               
00345          16  T-PREM          PIC S9(9)V99.                        
00346                                                                   
00347                              COPY ELCDTECX.                       
00348                                                                   
00349                              COPY ELCDTEVR.                       
00350                                                                   
00351                              COPY ELCGAPVR.                       
00352  EJECT                                                            
00353  PROCEDURE DIVISION.                                              
00354                                                                   
00355  0000-SET-START SECTION.                                          
00356                              COPY ELCDTERX.                       
00357                                                                   
00358      IF DTE-CLIENT = 'MON'                                        
00359           MOVE WS-76      TO WS-56.                               
00360                                                                   
00361      MOVE ZEROS           TO LAS-INT-R.                           
00362      MOVE COMPANY-NAME    TO HD-COMP.                             
00363      MOVE WS-CURRENT-DATE TO HD-RDT.                              
00364      MOVE ALPH-DATE       TO HD-DT.                               
00365      MOVE SPACES          TO H6-CELL H7-CELL                      
00366                              LN-1 LN-2 LN-3 LN-4.                 
00367      MOVE HD6A            TO H6-CELL.                             
00368      MOVE HD7A            TO H7-CELL.                             
00369                                                                   
00370      PERFORM 0050-INITIALIZE-TOTALS                               
00371              VARYING                                              
00372          CA FROM +1 BY +1                                         
00373              UNTIL                                                
00374          CA GREATER THAN +22.                                     
00375                                                                   
00376      PERFORM 3200-LOAD-MORT THRU 3200-LOAD-MORT-XIT.              
00377                                                                   
00378      IF DTE-PGM-OPT = '2'                                         
00379          MOVE SPACES TO H6-CL  H7-CL.                             
00380                                                                   
00381      IF DTE-PGM-OPT = '3'                                         
00382          MOVE SPACES TO H6-CO-ST  H7-CO-ST.                       
00383                                                                   
00384      MOVE H6-CELL     TO HD6A.                                    
00385      MOVE H7-CELL     TO HD7A.                                    
00386      MOVE DTE-PGM-OPT TO R-SW.                                    
00387                                                                   
LGC191     IF DTE-PGM-OPT = '4'                                         
LGC191         MOVE '1' TO R-SW                                         
LGC191     END-IF.                                                      
LGC191                                                                  
00388      SORT SORT-WORK ON ASCENDING GR-REIN                          
00389                                  GR-CARRIER                       
00390                                  GR-GROUPING                      
00391                                  GR-STATE                         
00392                                  GR-MORT-CODE                     
00393                                  GR-IG                            
00394                                  GR-LFTYP                         
00395                                  GR-MORT-AGE                      
00396                                  GR-LF-REMTERM                    
00397                                  GR-CERT-NO                       
00398          INPUT PROCEDURE 0100-SELECT-RECORDS THRU 0199-XIT        
00399          OUTPUT PROCEDURE 0200-PRINT-REPORT THRU 2299-XIT.        
00400                                                                   
00401      IF SORT-RETURN NOT = ZEROS                                   
00402         MOVE 0101   TO WS-RETURN-CODE                             
00403         MOVE ' ERROR IN SORT ' TO WS-ABEND-MESSAGE                
00404         GO TO ABEND-PGM.                                          
00405                                                                   
00406      GO TO 9999-STOP-RUN.                                         
00407  EJECT                                                            
00408  0050-INITIALIZE-TOTALS SECTION.                                  
00409                                                                   
00410      MOVE ZEROS                  TO T-RAMT (CA)                   
00411                                     T-MDEC (CA)                   
00412                                     T-MRES (CA)                   
00413                                     T-ALT-MRES (CA)               
00414                                     T-UNERN (CA)                  
00415                                     T-CNT (CA)                    
00416                                     T-PREM (CA).                  
00417                                                                   
00418  EJECT                                                            
00419  0100-SELECT-RECORDS SECTION.                                     
00420      OPEN INPUT  RESV-ANS                                         
00421           OUTPUT PRNTR.                                           
00422                                                                   
00423  0110-READ-NEXT-GAAP-RECORD.                                      
00424      READ RESV-ANS AT END                                         
00425          GO TO 0120-END-SELECT-RECORDS.                           
00426                                                                   
00427      MOVE GAAP-EXTRACT-RECORD TO GAAP-RECORD.                     
00428                                                                   
00429      IF GR-LFTYP = ZERO                                           
00430          GO TO 0110-READ-NEXT-GAAP-RECORD.                        
00431                                                                   
CIDMOD     IF DTE-PGM-OPT = '4'                                         
CIDMOD        IF GR-REIN = 'R'                                          
CIDMOD           GO TO 0110-READ-NEXT-GAAP-RECORD                       
CIDMOD        END-IF                                                    
CIDMOD     END-IF                                                       
CIDMOD                                                                  
00432      COPY ELCGAPM1.                                               
00433                                                                   
00434      MOVE GR-LFTYP TO CLAS-LOOK.                                  
00435                                                                   
00436      PERFORM 3100-FIND-LIFE-TYPE THRU 3199-FIND-LIFE-TYPE-X.      
00437                                                                   
00438      IF CLAS-I-BAL (CLAS-INDEXL) = 'B'                            
00439          GO TO 0110-READ-NEXT-GAAP-RECORD.                        
00440                                                                   
00441      IF GR-FLAG NOT = SPACE                                       
00442          GO TO 0110-READ-NEXT-GAAP-RECORD.                        
00443                                                                   
00444      IF GR-REIN = 'R'                                             
00445          MOVE GR-REIN-COMP TO GR-GROUPING.                        
00446                                                                   
00447      IF DTE-CLIENT = 'MON'                                        
00448        IF DTE-PGM-OPT = '3'                                       
00449          MOVE ZERO         TO GR-STATE.                           
00450                                                                   
LGC191     IF DTE-PGM-OPT = '4'                                         
LGC191         IF SELECT-CT  >  3999                                    
LGC191             MOVE 1 TO SELECT-CT                                  
LGC191             GO TO 0110-CONTINUE                                  
LGC191         ELSE                                                     
LGC191             ADD 1 TO SELECT-CT                                   
LGC191             GO TO 0110-READ-NEXT-GAAP-RECORD                     
LGC191         END-IF                                                   
LGC191     END-IF.                                                      
LGC191                                                                  
LGC191 0110-CONTINUE.                                                   
LGC191                                                                  
00451      RELEASE GAAP-RECORD.                                         
110821     add +1 to ws-release-cnt
00452                                                                   
00453      GO TO 0110-READ-NEXT-GAAP-RECORD.                            
00454                                                                   
00455  0120-END-SELECT-RECORDS.                                         
00456      CLOSE RESV-ANS.                                              
00457                                                                   
00458  0199-XIT.                                                        
00459      EXIT.                                                        
00460  EJECT                                                            
00461  0200-PRINT-REPORT SECTION.                                       

110821     if ws-release-cnt = zeros
110821        perform 0400-HEADINGS
110821        move 'Nothing to Process '
110821                                 to p-data
110821        perform 1700-prxx        thru 1799-PRXX-XIT
110821        GO TO 9999-STOP-RUN
110821     end-if

00462      PERFORM 1200-READ-EXTRACTS THRU 1299-READ-XIT.               
00463                                                                   
00464      PERFORM 1300-CLEAR-ACCUMS THRU 1399-CLEAR-XIT.               
00465                                                                   
00466      PERFORM 1500-SETUP-CONTROL THRU 1599-SETUP-XIT.              
00467                                                                   
00468      GO TO 0220-LAST-CHECK.                                       
00469                                                                   
00470  0210-READ-LOOP.                                                  
00471      PERFORM 1200-READ-EXTRACTS THRU 1299-READ-XIT.               
00472                                                                   
00473  0220-LAST-CHECK.                                                 
00474      GO TO 0300-DETAIL-REPORT                                     
00475            0310-CELL-REPORT                                       
00476            0320-SUMMARY-REPORT                                    
00477                DEPENDING ON R-SW.                                 
00478                                                                   
00479  0300-DETAIL-REPORT.                                              
00480      PERFORM 0500-DETAIL-CTL THRU 0599-DETAIL-XIT.                
                                                                        
CIDMOD     IF LINE-CT GREATER THAN WS-50                                
00482 *    IF LINE-CT GREATER THAN WS-56                                
00483          PERFORM 0400-HEADINGS THRU 0470-DETAIL-HEAD.             
00484                                                                   
00485      PERFORM 1800-PRINT-DETAIL THRU 1899-PR-DET-XIT.              
00486                                                                   
00487      MOVE 1 TO CA.                                                
00488                                                                   
00489      PERFORM 0800-ACCUM-TOTALS THRU 0899-ACCUM-XIT.               
00490                                                                   
00491      IF GR-LF-TERM GREATER THAN +120                              
00492          MOVE 12 TO CA                                            
00493          PERFORM 0800-ACCUM-TOTALS THRU 0899-ACCUM-XIT.           
00494                                                                   
00495      GO TO 0210-READ-LOOP.                                        
00496                                                                   
00497  0310-CELL-REPORT.                                                
00498      PERFORM 0500-DETAIL-CTL THRU 0599-DETAIL-XIT.                
00499                                                                   
CIDMOD     IF LINE-CT GREATER THAN WS-50                                
00500 *    IF LINE-CT GREATER THAN WS-56                                
00501          PERFORM 0400-HEADINGS THRU 0470-DETAIL-HEAD.             
00502                                                                   
00503      IF EOJ-SW = 'X'                                              
00504          GO TO 2210-END-OF-JOB.                                   
00505                                                                   
00506      MOVE 1 TO CA.                                                
00507                                                                   
00508      PERFORM 0800-ACCUM-TOTALS THRU 0899-ACCUM-XIT.               
00509                                                                   
00510      IF GR-LF-TERM GREATER THAN +120                              
00511          MOVE 12 TO CA                                            
00512          PERFORM 0800-ACCUM-TOTALS THRU 0899-ACCUM-XIT.           
00513                                                                   
00514      GO TO 0210-READ-LOOP.                                        
00515                                                                   
00516  0320-SUMMARY-REPORT.                                             
00517      PERFORM 0600-SUMMARY-CTL THRU 0699-SUMM-XIT.                 
                                                                        
CIDMOD     IF LINE-CT GREATER THAN WS-50                                
00519 *    IF LINE-CT GREATER THAN WS-56                                
00520          PERFORM 0400-HEADINGS THRU 0470-DETAIL-HEAD.             
00521                                                                   
00522      IF EOJ-SW = 'X'                                              
00523          GO TO 2210-END-OF-JOB.                                   
00524                                                                   
00525      MOVE 3 TO CA.                                                
00526                                                                   
00527      PERFORM 0800-ACCUM-TOTALS THRU 0899-ACCUM-XIT.               
00528                                                                   
00529      IF GR-LF-TERM GREATER THAN +120                              
00530          MOVE 14 TO CA                                            
00531          PERFORM 0800-ACCUM-TOTALS THRU 0899-ACCUM-XIT.           
00532                                                                   
00533      GO TO 0210-READ-LOOP.                                        
00534                                                                   
00535  0330-COMPANY-REPORT.                                             
00536      PERFORM 0700-COMP-CTL THRU 0799-COMP-EXIT.                   
00537                                                                   
CIDMOD     IF LINE-CT GREATER THAN WS-50                                
00538 *    IF LINE-CT GREATER THAN WS-56                                
00539          PERFORM 0400-HEADINGS                                    
00540          PERFORM 0470-DETAIL-HEAD THRU 0499-HEAD-XIT.             
00541                                                                   
00542      IF EOJ-SW = 'X'                                              
00543          GO TO 2210-END-OF-JOB.                                   
00544                                                                   
00545      MOVE 8 TO CA.                                                
00546                                                                   
00547      PERFORM 0800-ACCUM-TOTALS THRU 0899-ACCUM-XIT.               
00548                                                                   
00549      GO TO 0210-READ-LOOP.                                        
00550                                                                   
00551  0400-HEADINGS.                                                   
00552      ADD +1       TO PAGE-CT.                                     
00553      MOVE PAGE-CT TO HD-PG.                                       
00554      MOVE '1'     TO P-CTL.                                       
00555      MOVE HD1     TO P-DATA.                                      
00556      PERFORM 1700-PRXX THRU 1799-PRXX-XIT.                        
00557                                                                   
00558      MOVE ' ' TO P-CTL.                                           
00559      MOVE HD2 TO P-DATA.                                          
00560      PERFORM 1700-PRXX THRU 1799-PRXX-XIT.                        
00561                                                                   
00562      MOVE ' ' TO P-CTL.                                           
00563      MOVE HD3 TO P-DATA.                                          
00564      PERFORM 1700-PRXX THRU 1799-PRXX-XIT.                        
00565                                                                   
00566      MOVE ' ' TO P-CTL.                                           
00567                                                                   
00568  0410-HEAD-X1.                                                    
00569      MOVE LAS-TYPE TO CLAS-LOOK.                                  
00570                                                                   
00571      PERFORM 3100-FIND-LIFE-TYPE THRU 3199-FIND-LIFE-TYPE-X.      
00572                                                                   
00573      IF CLAS-I-RL-AH (CLAS-INDEXL) = 'R'                          
00574          MOVE HD2A TO P-DATA                                      
00575      ELSE                                                         
00576          MOVE HD2AA TO P-DATA.                                    
00577                                                                   
00578      MOVE SPACE TO NET-SW.                                        
00579                                                                   
00580      IF CLAS-I-EP (CLAS-INDEXL) = 'N'                             
00581          MOVE 'N' TO NET-SW                                       
00582          MOVE HD2AAA TO P-DATA.                                   
00583                                                                   
00584      PERFORM 1700-PRXX THRU 1799-PRXX-XIT.                        
00585                                                                   
00586  0420-CARR-HEAD.                                                  
00587      MOVE LAS-CAR TO  HD3-CAR                                     
00588      MOVE '0'     TO P-CTL.                                       
00589      MOVE HD3AAA  TO P-DATA.                                      
00590      PERFORM 1700-PRXX THRU 1799-PRXX-XIT.                        
00591                                                                   
00592  0430-COMP-HEAD.                                                  
00593      MOVE LAS-COMP        TO HD3-CO                               
00594      MOVE LAS-ST          TO HD3-ST                               
00595      MOVE LAS-ST          TO STATE-L.                             
00596      MOVE CLAS-STARTS     TO CLAS-INDEXS.                         
00597      MOVE 'INVALID STATE' TO HD3-STN.                             
00598                                                                   
00599  0440-COMP-HEAD-STATE.                                            
00600      IF CLAS-INDEXS GREATER THAN CLAS-MAXS                        
00601          GO TO 0450-TAB-HEAD.                                     
00602                                                                   
00603      IF STATE-SUB (CLAS-INDEXS) NOT = STATE-L                     
00604          ADD 1 TO CLAS-INDEXS                                     
00605          GO TO 0440-COMP-HEAD-STATE.                              
00606                                                                   
00607      MOVE STATE-PIC (CLAS-INDEXS) TO HD3-STN.                     
00608                                                                   
00609  0450-TAB-HEAD.                                                   
00610      PERFORM 2100-FIND-MORT-TABLE THRU 2199-FIND-MORT-TABLE-X.    
00611                                                                   
00612      MOVE CLAS-MORT-DESC (CLAS-INDEXM) TO HD3-MRT.                
00613                                                                   
00614      IF DTE-CLIENT = 'MON'                                        
00615        IF CLAS-MORT-CODE (CLAS-INDEXM) = 'Z000'                   
00616            MOVE 'ZERO RESERVE TABLE'   TO HD3-MRT.                
00617                                                                   
00618      MOVE LAS-TYPE                     TO CLAS-LOOK.              
00619                                                                   
00620      PERFORM 3100-FIND-LIFE-TYPE THRU 3199-FIND-LIFE-TYPE-X.      
00621                                                                   
00622      IF CLAS-I-JOINT (CLAS-INDEXL) = 'J'                          
00623          MOVE 'JOINT'  TO HD3-LVS                                 
00624      ELSE                                                         
00625          MOVE 'SINGLE' TO HD3-LVS.                                
00626                                                                   
00627      MOVE HD3AA TO P-DATA.                                        
00628      MOVE  ' '  TO P-CTL.                                         
00629      PERFORM 1700-PRXX THRU 1799-PRXX-XIT.                        
00630                                                                   
00631  0460-TYPE-HEAD.                                                  
00632      IF LAS-GRP = '1'                                             
00633          MOVE 'INDIVIDUAL' TO HD-IG                               
00634      ELSE                                                         
00635          MOVE 'GROUP'      TO HD-IG.                              
00636                                                                   
00637      MOVE ' '  TO P-CTL.                                          
00638      MOVE HD5A TO P-DATA.                                         
00639      PERFORM 1700-PRXX THRU 1799-PRXX-XIT.                        
00640                                                                   
00641  0470-DETAIL-HEAD.                                                
00642      MOVE HD6A TO P-DATA.                                         
00643      MOVE '0'  TO P-CTL.                                          
00644      PERFORM 1700-PRXX THRU 1799-PRXX-XIT.                        
00645                                                                   
00646      MOVE ' '  TO P-CTL.                                          
00647      MOVE HD7A TO P-DATA.                                         
00648      PERFORM 1700-PRXX THRU 1799-PRXX-XIT.                        
00649                                                                   
00650      MOVE SPACES TO P-DATA.                                       
00651      MOVE ' '    TO P-CTL.                                        
00652      PERFORM 1700-PRXX THRU 1799-PRXX-XIT.                        
00653                                                                   
00654  0499-HEAD-XIT.                                                   
00655      EXIT.                                                        
00656                                                                   
00657  0500-DETAIL-CTL.                                                 
00658      IF LAS-CAR NOT = GR-CARRIER                                  
00659          PERFORM 0900-RTRM-BREAK THRU 1199-CAR-BREAK-X            
00660          PERFORM 0400-HEADINGS THRU 0470-DETAIL-HEAD              
00661          GO TO 0599-DETAIL-XIT.                                   
00662                                                                   
00663      IF LAS-COMP NOT = GR-GROUPING                                
00664          PERFORM 0900-RTRM-BREAK THRU 1110-COMP-BREAK             
00665          PERFORM 0400-HEADINGS THRU 0470-DETAIL-HEAD              
00666          GO TO 0599-DETAIL-XIT.                                   
00667                                                                   
00668      IF LAS-ST NOT = GR-STATE                                     
00669          PERFORM 0900-RTRM-BREAK THRU 1100-STATE-BREAK            
00670          PERFORM 0400-HEADINGS THRU 0470-DETAIL-HEAD              
00671          GO TO 0599-DETAIL-XIT.                                   
00672                                                                   
00673      IF LAS-TAB NOT = GR-MORT-CODE                                
00674          PERFORM 0900-RTRM-BREAK THRU 1099-TAB-BREAK-XIT          
00675          PERFORM 0400-HEADINGS THRU 0470-DETAIL-HEAD              
00676          GO TO 0599-DETAIL-XIT.                                   
00677                                                                   
00678      IF LAS-GRP NOT = GR-IG                                       
00679          PERFORM 0900-RTRM-BREAK THRU 1000-GRP-BREAK              
00680          PERFORM 0400-HEADINGS THRU 0470-DETAIL-HEAD              
00681          GO TO 0599-DETAIL-XIT.                                   
00682                                                                   
00683      IF LAS-TYPE NOT = GR-LFTYP                                   
00684          PERFORM 0900-RTRM-BREAK THRU 0999-TYPE-BREAK-X           
00685          PERFORM 0400-HEADINGS THRU 0470-DETAIL-HEAD              
00686          GO TO 0599-DETAIL-XIT.                                   
00687                                                                   
00688      IF LAS-AGE NOT = GR-MORT-AGE                                 
00689          PERFORM 0900-RTRM-BREAK THRU 0910-AGE-BREAK              
00690          GO TO 0599-DETAIL-XIT.                                   
00691                                                                   
00692      IF LAS-RTRM NOT = GR-LF-REMTERM                              
00693          PERFORM 0900-RTRM-BREAK.                                 
00694                                                                   
00695  0599-DETAIL-XIT.                                                 
00696      EXIT.                                                        
00697                                                                   
00698  0600-SUMMARY-CTL.                                                
00699      IF LAS-CAR NOT = GR-CARRIER                                  
00700          PERFORM 0920-TYPE-BREAK THRU 1199-CAR-BREAK-X            
00701          GO TO 0699-SUMM-XIT.                                     
00702                                                                   
00703      IF LAS-COMP NOT = GR-GROUPING                                
00704          PERFORM 0920-TYPE-BREAK THRU 1110-COMP-BREAK             
00705          GO TO 0699-SUMM-XIT.                                     
00706                                                                   
00707      IF LAS-ST NOT = GR-STATE                                     
00708          PERFORM 0920-TYPE-BREAK THRU 1100-STATE-BREAK            
00709          GO TO 0699-SUMM-XIT.                                     
00710                                                                   
00711      IF LAS-TAB NOT = GR-MORT-CODE                                
00712          PERFORM 0920-TYPE-BREAK THRU 1099-TAB-BREAK-XIT          
00713          GO TO 0699-SUMM-XIT.                                     
00714                                                                   
00715      IF LAS-GRP NOT = GR-IG                                       
00716          PERFORM 0920-TYPE-BREAK THRU 1000-GRP-BREAK              
00717          GO TO 0699-SUMM-XIT.                                     
00718                                                                   
00719      IF LAS-TYPE NOT = GR-LFTYP                                   
00720          PERFORM 0920-TYPE-BREAK THRU 0999-TYPE-BREAK-X           
00721          GO TO 0699-SUMM-XIT.                                     
00722                                                                   
00723  0699-SUMM-XIT.                                                   
00724      EXIT.                                                        
00725                                                                   
00726  0700-COMP-CTL.                                                   
00727      IF LAS-COMP NOT = GR-GROUPING                                
00728          PERFORM 1110-COMP-BREAK.                                 
00729                                                                   
00730      MOVE GR-GROUPING TO LAS-COMP.                                
00731                                                                   
00732  0799-COMP-EXIT.                                                  
00733      EXIT.                                                        
00734                                                                   
00735  0800-ACCUM-TOTALS.                                               
00736      ADD GR-LFPRM    TO T-PREM     (CA).                          
00737      ADD GR-CNT-LF   TO T-CNT      (CA).                          
00738      ADD GRS-LFPRM   TO T-UNERN    (CA).                          
00739      ADD GR-REM-AMT  TO T-RAMT     (CA)                           
00740      ADD GR-MO-DEC   TO T-MDEC     (CA)                           
00741      ADD GR-RESV     TO T-MRES     (CA)                           
00742      ADD GR-ALT-RESV TO T-ALT-MRES (CA).                          
00743                                                                   
00744  0899-ACCUM-XIT.                                                  
00745      EXIT.                                                        
00746                                                                   
00747  0900-RTRM-BREAK.                                                 
00748      MOVE 1        TO CA.                                         
00749      MOVE 2        TO CB.                                         
00750      MOVE LAS-RTRM TO L3-RTRM.                                    
00751                                                                   
00752      IF NET-SW = 'N'                                              
00753         IF T-RAMT (1) GREATER THAN ZERO                           
00754             COMPUTE LAS-FAC ROUNDED = T-MRES (1)                  
00755                                  / (T-RAMT (1) / 1000).           
00756                                                                   
00757      MOVE LAS-FAC      TO L3-FAC.                                 
00758      MOVE L3-TERM-DESC TO L3-TOT-DES.                             
00759      MOVE ' '          TO P-CTL.                                  
00760                                                                   
00761      PERFORM 1600-TOTALS-TO-PRINT THRU 1699-TOTALS-XIT.           
00762                                                                   
00763      MOVE GR-LF-REMTERM TO LAS-RTRM.                              
00764      MOVE GR-MORT-FACT TO LAS-FAC.                                
00765                                                                   
00766  0910-AGE-BREAK.                                                  
00767      MOVE 2           TO CA.                                      
00768      MOVE 3           TO CB.                                      
00769      MOVE LAS-AGE     TO L3-AGE.                                  
00770      MOVE L3-AGE-DESC TO L3-TOT-DES.                              
00771      MOVE ' '         TO P-CTL.                                   
00772                                                                   
00773      PERFORM 1600-TOTALS-TO-PRINT THRU 1699-TOTALS-XIT.           
00774                                                                   
00775      MOVE SPACES TO P-DATA.                                       
00776      MOVE ' '    TO P-CTL.                                        
00777      PERFORM 1700-PRXX THRU 1799-PRXX-XIT.                        
00778                                                                   
00779      MOVE GR-MORT-AGE TO LAS-AGE.                                 
00780                                                                   
00781  0920-TYPE-BREAK.                                                 
00782      MOVE 3        TO CA.                                         
00783      MOVE 4        TO CB.                                         
00784      MOVE LAS-TYPE TO CLAS-LOOK.                                  
00785                                                                   
00786      PERFORM 3100-FIND-LIFE-TYPE THRU 3199-FIND-LIFE-TYPE-X.      
00787                                                                   
00788      MOVE CLAS-I-AB10 (CLAS-INDEXL) TO L3-TYP.                    
00789      MOVE LAS-TYPE                  TO L3-PLN.                    
00790      MOVE L3-TYPE-DESC              TO L3-TOT-DES.                
00791      MOVE ' '                       TO P-CTL.                     
00792                                                                   
00793      PERFORM 1600-TOTALS-TO-PRINT THRU 1699-TOTALS-XIT.           
00794                                                                   
00795      IF EOJ-SW NOT = 'X'                                          
00796          MOVE GR-LFTYP TO LAS-TYPE.                               
00797                                                                   
00798  0999-TYPE-BREAK-X.                                               
00799      EXIT.                                                        
00800                                                                   
00801  1000-GRP-BREAK.                                                  
                                                                        
CIDMOD     IF LINE-CT GREATER THAN WS-50                                
00802 *    IF LINE-CT GREATER THAN WS-56                                
00803          PERFORM 0400-HEADINGS THRU 0499-HEAD-XIT.                
00804                                                                   
00805      MOVE 4 TO CA.                                                
00806      MOVE 5 TO CB.                                                
00807                                                                   
00808      IF LAS-GRP = '1'                                             
00809          MOVE 'TYPE - INDIVIDUAL TOTAL' TO L3-GRP-DES             
00810      ELSE                                                         
00811          MOVE 'TYPE - GROUP TOTAL'      TO L3-GRP-DES.            
00812                                                                   
00813      MOVE L3-FRONT-1 TO L3-TOT-DES.                               
00814      MOVE '0'        TO P-CTL.                                    
00815                                                                   
00816      PERFORM 1600-TOTALS-TO-PRINT THRU 1699-TOTALS-XIT.           
00817                                                                   
00818      MOVE GR-IG TO LAS-GRP.                                       
00819                                                                   
00820  1010-TAB-BREAK.                                                  
00821      MOVE 5 TO CA.                                                
00822      MOVE 6 TO CB.                                                
00823                                                                   
00824      PERFORM 2100-FIND-MORT-TABLE THRU 2199-FIND-MORT-TABLE-X.    
00825                                                                   
00826      MOVE CLAS-MORT-DESC (CLAS-INDEXM) TO L3-TAB.                 
00827                                                                   
00828      IF DTE-CLIENT = 'MON'                                        
00829        IF CLAS-MORT-CODE (CLAS-INDEXM) = 'Z000'                   
00830            MOVE 'ZERO RESERVE TABLE'   TO L3-TAB.                 
00831                                                                   
00832      MOVE LAS-CODE                     TO L3-TBL.                 
00833      MOVE L3-TAB-DESC                  TO L3-TOT-DES.             
00834      MOVE '0'                          TO P-CTL.                  
00835                                                                   
00836      PERFORM 1600-TOTALS-TO-PRINT THRU 1699-TOTALS-XIT.           
00837                                                                   
00838      MOVE GR-MORT-CODE TO LAS-TAB.                                
00839      MOVE GR-MORT-CODE TO LAS-CODE.                               
00840                                                                   
00841  1099-TAB-BREAK-XIT.                                              
00842      EXIT.                                                        
00843                                                                   
00844  1100-STATE-BREAK.                                                
00845      MOVE 6          TO CA.                                       
00846      MOVE 7          TO CB.                                       
00847      MOVE LAS-ST     TO L3-ST-NO.                                 
00848      MOVE L3-ST-LINE TO L3-TOT-DES.                               
00849      MOVE '-'        TO P-CTL.                                    
00850                                                                   
00851      PERFORM 1600-TOTALS-TO-PRINT THRU 1699-TOTALS-XIT.           
00852                                                                   
00853      MOVE SPACES TO L3-TOT-DES.                                   
00854      MOVE SPACES TO PRT.                                          
00855      PERFORM 1700-PRXX THRU 1799-PRXX-XIT.                        
00856                                                                   
00857      MOVE GR-STATE TO LAS-ST.                                     
00858                                                                   
00859  1110-COMP-BREAK.                                                 
00860      MOVE 7          TO CA.                                       
00861      MOVE 8          TO CB.                                       
00862      MOVE LAS-COMP   TO L3-COMP-NO.                               
00863      MOVE L3-CO-LINE TO L3-TOT-DES.                               
00864      MOVE '0'        TO P-CTL.                                    
00865                                                                   
00866      PERFORM 1600-TOTALS-TO-PRINT THRU 1699-TOTALS-XIT.           
00867                                                                   
00868      MOVE GR-GROUPING TO LAS-COMP.                                
00869      MOVE SPACES   TO L3-TOT-DES.                                 
00870      MOVE SPACES   TO PRT.                                        
00871      PERFORM 1700-PRXX THRU 1799-PRXX-XIT.                        
00872                                                                   
00873  1120-CAR-BREAK.                                                  
00874      MOVE 8           TO CA.                                      
00875      MOVE 9           TO CB.                                      
00876      MOVE LAS-CAR     TO L3-CA-NO.                                
00877      MOVE L3-CAR-LINE TO L3-TOT-DES.                              
00878      MOVE '0'         TO P-CTL.                                   
00879                                                                   
00880      PERFORM 1600-TOTALS-TO-PRINT THRU 1699-TOTALS-XIT.           
00881                                                                   
00882      MOVE GR-CARRIER  TO LAS-CAR.                                 
00883      MOVE SPACES TO L3-TOT-DES.                                   
00884                                                                   
00885      IF EOJ-SW = 'X'                                              
00886          GO TO 2210-END-OF-JOB.                                   
00887                                                                   
00888      IF LAS-CAR GREATER THAN L3-CA-NO                             
00889          PERFORM 0400-HEADINGS THRU 0499-HEAD-XIT.                
00890                                                                   
00891  1199-CAR-BREAK-X.                                                
00892      EXIT.                                                        
00893                                                                   
00894  1200-READ-EXTRACTS.                                              
00895      RETURN SORT-WORK AT END                                      
00896          GO TO 2200-E-O-J-RTN.                                    
00897                                                                   
00898      IF GR-REIN = 'R' AND REIN-SW = 'X'                           
00899          PERFORM 1900-REIN-SUB THRU 1999-REIN-XIT.                
00900                                                                   
00901      MOVE ' ' TO DD-SW.                                           
00902                                                                   
00903      COPY ELCGAPM1.                                               
00904                                                                   
00905  1299-READ-XIT.                                                   
00906      EXIT.                                                        
00907                                                                   
00908  1300-CLEAR-ACCUMS.                                               
00909      MOVE 1 TO CA.                                                
00910                                                                   
00911      PERFORM 1400-ZERO-AREAS 22 TIMES.                            
00912                                                                   
00913  1399-CLEAR-XIT.                                                  
00914      EXIT.                                                        
00915                                                                   
00916  1400-ZERO-AREAS.                                                 
00917      MOVE ZERO TO T-RAMT      (CA)                                
00918                   T-MDEC      (CA)                                
00919                   T-MRES      (CA)                                
00920                   T-ALT-MRES  (CA)                                
00921                   T-UNERN     (CA)                                
00922                   T-CNT       (CA)                                
00923                   T-PREM      (CA).                               
00924      ADD 1     TO CA.                                             
00925                                                                   
00926  1499-ZERO-XIT.                                                   
00927      EXIT.                                                        
00928                                                                   
00929  1500-SETUP-CONTROL.                                              
00930      MOVE GR-CARRIER   TO LAS-CAR.                                
00931      MOVE GR-GROUPING  TO LAS-COMP.                               
00932      MOVE GR-STATE     TO LAS-ST.                                 
00933      MOVE GR-MORT-CODE TO LAS-TAB.                                
00934      MOVE GR-MORT-INT  TO LAS-INT-R.                              
00935      MOVE GR-IG        TO LAS-GRP.                                
00936      MOVE GR-LFTYP     TO LAS-TYPE.                               
00937      MOVE GR-MORT-AGE  TO LAS-AGE.                                
00938      MOVE GR-LF-REMTERM       TO LAS-RTRM.                        
00939      MOVE GR-MORT-FACT TO LAS-FAC.                                
00940      MOVE GR-MORT-CODE TO LAS-CODE.                               
00941                                                                   
00942  1599-SETUP-XIT.                                                  
00943      EXIT.                                                        
00944                                                                   
00945  1600-TOTALS-TO-PRINT.                                            
00946      MOVE T-CNT  (CA)     TO L3-CNT.                              
00947      MOVE T-RAMT (CA)     TO L3-RAMT.                             
00948      MOVE T-MRES (CA)     TO L3-RESV.                             
00949      MOVE T-PREM (CA)     TO L3-PREM.                             
00950      MOVE LN-3            TO P-DATA.                              
00951                                                                   
00952      PERFORM 1700-PRXX THRU 1799-PRXX-XIT.                        
00953                                                                   
00954      MOVE T-MDEC     (CA) TO L4-DEC.                              
00955      MOVE T-ALT-MRES (CA) TO L4-ALT-RESV.                         
00956      MOVE T-UNERN    (CA) TO L4-ST-STAT.                          
00957      MOVE LN-4            TO P-DATA.                              
00958                                                                   
00959      PERFORM 1700-PRXX THRU 1799-PRXX-XIT.                        
00960                                                                   
00961      ADD T-RAMT     (CA) TO T-RAMT     (CB).                      
00962      ADD T-MDEC     (CA) TO T-MDEC     (CB).                      
00963      ADD T-MRES     (CA) TO T-MRES     (CB).                      
00964      ADD T-ALT-MRES (CA) TO T-ALT-MRES (CB).                      
00965      ADD T-UNERN    (CA) TO T-UNERN    (CB).                      
00966      ADD T-CNT      (CA) TO T-CNT      (CB).                      
00967      ADD T-PREM     (CA) TO T-PREM     (CB).                      
00968                                                                   
00969      IF GR-LF-TERM GREATER THAN +120                              
00970          MOVE CA           TO SAV-CA                              
00971          MOVE CB           TO SAV-CB                              
00972          ADD 11            TO CA                                  
00973          ADD 11            TO CB                                  
00974          ADD T-RAMT     (CA) TO T-RAMT     (CB)                   
00975          ADD T-MDEC     (CA) TO T-MDEC     (CB)                   
00976          ADD T-MRES     (CA) TO T-MRES     (CB)                   
00977          ADD T-ALT-MRES (CA) TO T-ALT-MRES (CB)                   
00978          ADD T-UNERN    (CA) TO T-UNERN    (CB)                   
00979          ADD T-CNT      (CA) TO T-CNT      (CB)                   
00980          ADD T-PREM     (CA) TO T-PREM     (CB)                   
00981          MOVE SAVE-ZERO      TO T-RAMT     (CA)                   
00982                                 T-MDEC     (CA)                   
00983                                 T-MRES     (CA)                   
00984                                 T-ALT-MRES (CA)                   
00985                                 T-UNERN    (CA)                   
00986                                 T-CNT      (CA)                   
00987                                 T-PREM     (CA)                   
00988          MOVE SAV-CA         TO CA                                
00989          MOVE SAV-CB         TO CB.                               
00990                                                                   
00991      MOVE SAVE-ZERO TO T-RAMT     (CA)                            
00992                        T-MDEC     (CA)                            
00993                        T-MRES     (CA)                            
00994                        T-ALT-MRES (CA)                            
00995                        T-UNERN    (CA)                            
00996                        T-CNT      (CA)                            
00997                        T-PREM     (CA).                           
00998                                                                   
00999  1699-TOTALS-XIT.                                                 
01000      EXIT.                                                        
01001                                                                   
01002  1700-PRXX.                                                       
01003      MOVE P-CTL TO X.                                             
01004                                                                   
01005      IF X = ' '                                                   
01006          ADD +1 TO LINE-CT                                        
01007      ELSE                                                         
01008          IF X = '0'                                               
01009              ADD +2 TO LINE-CT                                    
01010          ELSE                                                     
01011              IF X = '-'                                           
01012                  ADD +3 TO LINE-CT                                
01013              ELSE                                                 
01014                  MOVE +1 TO LINE-CT.                              
01015                                                                   
01016  1710-PRXX-GO.                                                    
01017                              COPY ELCPRT2.                        
01018                                                                   
01019      MOVE SPACES TO P-DATA.                                       
01020                                                                   
01021  1799-PRXX-XIT.                                                   
01022          EXIT.                                                    
01023                                                                   
01024  1800-PRINT-DETAIL.                                               
01025      MOVE GR-ACCOUNT  TO L1-ACCT.                                 
01026      MOVE GR-CERT     TO L1-CERT.                                 
01027      MOVE GR-MO       TO L1-MO.                                   
01028      MOVE GR-DA       TO L1-DA.                                   
01029      MOVE GR-YR       TO L1-YR.                                   
01030      MOVE '-'         TO L1-MOD  L1-DAD.                          
01031      MOVE GR-AGE      TO L1-IAGE.                                 
01032      MOVE GR-LF-TERM  TO L1-TRM.                                  
01033      MOVE GR-LFBEN    TO L2-AMT.                                  
01034      MOVE GR-MORT-AGE TO L1-VAGE.                                 
01035      MOVE GR-LF-REMTERM      TO L1-RTRM.                          
01036                                                                   
01037      IF GR-APR = ZEROS                                            
01038          MOVE SPACES      TO L1-PMS                               
01039          MOVE GR-PMT-FREQ TO L1-PM                                
01040      ELSE                                                         
01041          MOVE GR-APR      TO L1-APR.                              
01042                                                                   
01043      MOVE GR-CNT-LF   TO L1-CNT.                                  
01044      MOVE GR-REM-AMT  TO L1-RAMT.                                 
01045      MOVE GR-MO-DEC   TO L2-DEX.                                  
01046      MOVE GR-RESV     TO L1-RESV.                                 
01047      MOVE GR-ALT-RESV TO L2-ALT-RESV.                             
01048      MOVE GR-LFPRM    TO L1-O-PREM.                               
01049      MOVE GRS-LFPRM   TO L2-ST-STAT.                              
01050                                                                   
01051      MOVE ' '         TO P-CTL.                                   
01052      MOVE LN-1        TO P-DATA.                                  
01053                                                                   
01054      PERFORM 1700-PRXX THRU 1799-PRXX-XIT.                        
01055                                                                   
01056      MOVE ' '         TO P-CTL.                                   
01057      MOVE LN-2        TO P-DATA.                                  
01058                                                                   
01059      PERFORM 1700-PRXX THRU 1799-PRXX-XIT.                        
01060                                                                   
01061  1899-PR-DET-XIT.                                                 
01062      EXIT.                                                        
01063                                                                   
01064  1900-REIN-SUB.                                                   
01065      IF DD-SW = 'X'                                               
01066          GO TO 1910-R-S-SET.                                      
01067                                                                   
01068      PERFORM 0900-RTRM-BREAK THRU 1199-CAR-BREAK-X.               
01069                                                                   
01070      PERFORM 2210-END-OF-JOB.                                     
01071                                                                   
01072  1910-R-S-SET.                                                    
01073      PERFORM 1300-CLEAR-ACCUMS THRU 1399-CLEAR-XIT.               
01074                                                                   
01075      PERFORM 1500-SETUP-CONTROL THRU 1599-SETUP-XIT.              
01076                                                                   
01077      MOVE '(REINSURANCE)' TO HD-REIN.                             
01078      MOVE +80             TO LINE-CT.                             
01079      MOVE SPACES          TO REIN-SW.                             
01080      MOVE ' '             TO DD-SW.                               
01081                                                                   
01082  1999-REIN-XIT.                                                   
01083      EXIT.                                                        
01084                                                                   
01085  2100-FIND-MORT-TABLE.                                            
01086      IF CLAS-MAXM = ZEROS                                         
01087          MOVE +1 TO CLAS-STARTM.                                  
01088                                                                   
01089      MOVE CLAS-STARTM TO CLAS-INDEXM.                             
01090                                                                   
01091  2110-FIND-MORT-TABLE-LOOP.                                       
01092      IF DTE-CLIENT = 'MON'                                        
01093        IF CLAS-MORT-CODE (CLAS-INDEXM) = 'Z000'                   
01094            GO TO 2199-FIND-MORT-TABLE-X.                          
01095                                                                   
01096      IF CLAS-INDEXM GREATER THAN CLAS-MAXM                        
01097          MOVE 0301 TO WS-RETURN-CODE                              
01098          MOVE ' INVALIDE LIFE TYPE ' TO WS-ABEND-MESSAGE          
01099          GO TO ABEND-PGM.                                         
01100                                                                   
01101      IF LAS-CODE = CLAS-MORT-CODE (CLAS-INDEXM)                   
01102          OR CLAS-MORT-CODE (CLAS-INDEXM) EQUAL SPACES             
01103              NEXT SENTENCE                                        
01104      ELSE                                                         
01105          ADD +1             TO CLAS-INDEXM                        
01106          GO TO 2110-FIND-MORT-TABLE-LOOP.                         
01107                                                                   
01108  2199-FIND-MORT-TABLE-X.                                          
01109      EXIT.                                                        
01110                                                                   
01111  2200-E-O-J-RTN.                                                  
01112      MOVE 'X'         TO EOJ-SW.                                  
01113      MOVE HIGH-VALUES TO GR-CARRIER GR-GROUPING  GR-STATE.        
01114 *    MOVE 99          TO GR-LFTYP.                                
01115                                                                   
01116      GO TO 0220-LAST-CHECK.                                       
01117                                                                   
01118  2210-END-OF-JOB.                                                 
01119      PERFORM 0400-HEADINGS.                                       
01120                                                                   
01121      PERFORM 0470-DETAIL-HEAD THRU 0499-HEAD-XIT.                 
01122                                                                   
01123      MOVE L3-FINAL-LINE TO L3-TOT-DES.                            
01124      MOVE 09            TO CA.                                    
01125      MOVE 10            TO CB.                                    
01126      MOVE '0'           TO P-CTL.                                 
01127                                                                   
01128      PERFORM 1600-TOTALS-TO-PRINT THRU 1699-TOTALS-XIT.           
01129                                                                   
01130  2220-END-EXIT.                                                   
01131      MOVE '1'                        TO P-CTL.                    
01132      MOVE ' END OF JOB    ECS-086  ' TO P-DATA.                   
01133      PERFORM 1700-PRXX THRU 1799-PRXX-XIT.                        
01134                                                                   
01135  2230-CLOSE-FICH.                                                 
01136                              COPY ELCPRTC.                        
01137                                                                   
01138      CLOSE PRNTR.                                                 
01139                                                                   
01140  2240-CLOSE-FICH-XIT.                                             
01141      EXIT.                                                        
01142                                                                   
01143  2299-XIT.                                                        
01144      EXIT.                                                        
01145                                                                   
01146  3000-COMMON-ROUTINE SECTION.                                     
01147                                                                   
01148  3100-FIND-LIFE-TYPE.                                             
01149      IF CLAS-MAXL = ZEROS                                         
01150          MOVE +1 TO CLAS-STARTL.                                  
01151                                                                   
01152      MOVE CLAS-STARTL TO CLAS-INDEXL.                             
01153                                                                   
01154  3110-FIND-LIFE-TYPE-LOOP.                                        
01155      IF CLAS-INDEXL GREATER THAN CLAS-MAXL                        
01156          MOVE ' INVALID BENEFIT CODE ' TO WS-ABEND-MESSAGE        
01157          MOVE 0401     TO WS-RETURN-CODE                          
01158          GO TO ABEND-PGM.                                         
01159                                                                   
01160      IF CLAS-I-BEN (CLAS-INDEXL) NOT = CLAS-LOOK                  
01161          ADD +1 TO CLAS-INDEXL                                    
01162          GO TO 3110-FIND-LIFE-TYPE-LOOP.                          
01163                                                                   
01164  3199-FIND-LIFE-TYPE-X.                                           
01165      EXIT.                                                        
01166                                                                   
01167  3200-LOAD-MORT.                                                  
01168      IF CLAS-MAXM = ZEROS                                         
01169          MOVE +1 TO CLAS-STARTM.                                  
01170                                                                   
01171      ADD +1         TO CLAS-MAXM.                                 
01172                                                                   
020113     IF CLAS-MAXM GREATER THAN +150                               
020113         MOVE +150  TO CLAS-MAXM.                                 
01175                                                                   
01176      MOVE CLAS-MAXM TO X1.                                        
01177      MOVE SPACES    TO CLAS-MORT-CODE (X1).                       
01178      MOVE 'OTHERS'  TO CLAS-MORT-DESC (X1).                       
01179                                                                   
01180  3200-LOAD-MORT-XIT.                                              
01181      EXIT.                                                        
01182  9000-STOP-RUN-RTN SECTION.                                       
01183                                                                   
01184  9999-STOP-RUN.                                                   
01185      GOBACK.                                                      
01186                                                                   
01187  ABEND-PGM.                                                       
01188                     COPY ELCABEND.                                
01189                                                                   
