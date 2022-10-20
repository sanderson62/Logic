00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL675 .                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 02/14/96 07:28:08.                 
00007 *                            VMOD=2.005                           
00008 *                                                                 
00008 *                                                                 
00009 *AUTHOR.     LOGIC INC.                                           
00010 *            DALLAS, TEXAS.                                       
00011                                                                   
00012 *DATE-COMPILED.                                                   
00013 *SECURITY.   *****************************************************
00014 *            *                                                   *
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00016 *            *                                                   *
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00020 *            *                                                   *
00021 *            *****************************************************
00022                                                                   
00023 *REMARKS.     TRANS - EXE9 - COMPENSATION MASTER PRINT            
00024 *             STARTED FROM EL671                                  
00025                                                                   
00026      EJECT                                                        
00027  ENVIRONMENT DIVISION.                                            
00028  DATA DIVISION.                                                   
00029  WORKING-STORAGE SECTION.                                         
00030  01  LCP-TIME-OF-DAY-XX.                                          
00031      05  LCP-TIME-OF-DAY-68        PIC 9(6).                      
00032      05  FILLER                    PIC 99.                        
00033  01  LCP-CICS-TIME                 PIC 9(15).                     
00034  77  FILLER  PIC X(32)  VALUE '********************************'. 
00035  77  FILLER  PIC X(32)  VALUE '*    EL675 WORKING STORAGE     *'. 
00036  77  FILLER  PIC X(32)  VALUE '************ V/M 2.005 *********'. 
00037                                                                   
00038  77  CLEN        PIC S9(4)  COMP    VALUE 1024.                   
00039  77  CTR         PIC S99    COMP    VALUE +0.                     
00040                                                                   
00041  01  WS-DATE-AREA.                                                
00042      03  SAVE-DATE               PIC X(8)    VALUE SPACES.        
00043      03  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.        
00044      03  SAVE-DATE-ALPHA         PIC X(18)   VALUE SPACES.        
00045                                                                   
00046  01  WORK-AREAS.                                                  
00047      03  WS-REPORT-ID            PIC X(6)    VALUE SPACES.        
00048      03  PGM-NAME                PIC X(8)    VALUE SPACES.        
00049      03  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     
00050      03  EMI-LINE1               PIC X(72).                       
00051      03  WS-NEXT-TRAN            PIC X(4).                        
00052      03  WS-TERMINAL-ID.                                          
00053          05  WS-TERM-PREFIX      PIC XX.                          
00054          05  FILLER              PIC XX.                          
00055      03  PRT-CNT                 PIC S9(3)   VALUE +4   COMP-3.   
00056      03  WS-LINE-NUMBER          PIC S9(7)   VALUE ZERO COMP-3.   
00057      03  WS-PAGE                 PIC S9(5)   VALUE ZERO COMP-3.   
00058      03  WS-REPORT-SW            PIC S9      VALUE ZERO COMP-3.   
00059      03  WS-PRINT-SW             PIC S9      VALUE ZERO COMP-3.   
00060      03  REPT-FILE-ID            PIC X(8)    VALUE 'ELREPT'.      
00061      03  COMP-FILE-ID            PIC X(8).                        
00062      03  COMP-FILE-ID-CR         PIC X(8)    VALUE 'ERCOMP'.      
00063      03  COMP-FILE-ID-CV         PIC X(8)    VALUE 'MPCOMP'.      
00064      03  CNTL-ID                 PIC X(8)    VALUE 'ELCNTL'.      
00065      03  SA                      PIC S999    COMP.                
00066      03  B-REC-FLAG              PIC X       VALUE SPACES.        
00067      03  WS-DATE.                                                 
00068          05  WS-YR               PIC XX.                          
00069          05  WS-MO               PIC XX.                          
00070          05  WS-DA               PIC XX.                          
00071      03  ABEND-AREA              PIC X(72).                       
00072                                                                   
00073  01  ACCESS-KEYS.                                                 
00074      03  ELCNTL-KEY.                                              
00075          05  CNTL-COMP-ID        PIC X(3).                        
00076          05  CNTL-REC-TYPE       PIC X.                           
00077          05  CNTL-ACCESS         PIC X(4).                        
00078          05  CNTL-SEQ-NO         PIC S9(4)    COMP.               
00079                                                                   
00080                           COPY ELCREPT.                           
00081      EJECT                                                        
00082                           COPY ELCDATE.                           
00083      EJECT                                                        
00084                                                                   
00085  01  MISC-WORKING-STORAGE.                                        
00086      03  WK-DATE.                                                 
00087          05  WK-MO           PIC 99.                              
00088          05  WK-DA           PIC 99.                              
00089          05  WK-YR           PIC 99.                              
00090      03  DASH-LINE           PIC X(80) VALUE ALL '-'.             
00091      03  WORK-AMT            PIC S9(9)V99  COMP-3 VALUE +0.       
00092      03  FINAL-TOTALS    COMP-3.                                  
00093          05  F-BAL           PIC S9(9)V99  VALUE +0.              
00094          05  F-ADJ           PIC S9(9)V99  VALUE +0.              
00095          05  F-YTD           PIC S9(9)V99  VALUE +0.              
00096                                                                   
00097  01  HDR-LINES.                                                   
00098      03  HDR-1.                                                   
00099          05  FILLER          PIC X(21)   VALUE SPACES.            
00100          05  FILLER          PIC X(33)   VALUE                    
00101                  'COMPENSATION MASTER FILE LISTING'.              
00102          05  FILLER          PIC X(13)   VALUE SPACES.            
00103          05  FILLER          PIC X(8)    VALUE 'EL - 675'.        
00104                                                                   
00105      03  HDR-2.                                                   
00106          05  FILLER          PIC X(29)   VALUE SPACES.            
00107          05  H2-COMP         PIC X(30)   VALUE 'LOGIC, INC.'.     
00108          05  FILLER          PIC X(9)    VALUE SPACES.            
00109          05  H2-DATE         PIC X(8).                            
00110                                                                   
00111      03  HDR-3.                                                   
00112          05  FILLER          PIC X(27)   VALUE SPACES.            
00113          05  H3-DATE.                                             
00114            07  H3-DATE1      PIC X(8)    VALUE SPACES.            
00115            07  H3-THRU       PIC X(6)    VALUE SPACES.            
00116            07  H3-DATE2      PIC X(8)    VALUE SPACES.            
00117          05  FILLER          PIC X(16)   VALUE SPACES.            
00118          05  FILLER          PIC X(5)    VALUE 'PAGE '.           
00119          05  H3-PAGE         PIC ZZ,ZZ9.                          
00120                                                                   
00121      03  HDR-4.                                                   
00122          05  FILLER          PIC X(19)           VALUE            
00123                  'CARRIER GROUPING - '.                           
00124          05  HD-CARR         PIC X               VALUE SPACES.    
00125          05  HD-GROUP        PIC X(6)            VALUE SPACES.    
00126          05  FILLER          PIC XXX             VALUE ' - '.     
00127          05  HD-NAME         PIC X(30)           VALUE SPACES.    
00128                                                                   
00129      03  HDR-5.                                                   
00130          05  FILLER          PIC X(44)           VALUE            
00131                  ' FINAN.        ACCT.  AGT CAR MAILING'.         
00132          05  FILLER          PIC X(30)           VALUE            
00133                  '          ACCOUNT'.                             
00134                                                                   
00135      03  HDR-5A.                                                  
00136          05  FILLER          PIC X(50)           VALUE            
00137            '  LAST      CURRENT    YEAR TO DATE  YEAR TO DATE '.  
00138                                                                   
00139      03  HDR-6.                                                   
00140          05  FILLER          PIC X(44)           VALUE            
00141                  ' RESPON        NUMBER TYP BAL ADDRESS'.         
00142          05  FILLER          PIC X(30)           VALUE            
00143                  '          NAME     '.                           
00144                                                                   
00145      03  HDR-6A.                                                  
00146          05  FILLER          PIC X(50)           VALUE            
00147            '  MAINT     BALANCE    ADJUSTMENTS   COMPENSATION '.  
00148                                                                   
00149  EJECT                                                            
00150  01  P-REC.                                                       
00151      03  P-LINE.                                                  
00152          05  P-FILL.                                              
00153              07  P-RESP          PIC X(10).                       
00154              07  FILLER          PIC X.                           
00155              07  P-ACCT          PIC X(10).                       
00156              07  FILLER          PIC XX.                          
00157              07  P-TYPE          PIC X.                           
00158              07  FILLER          PIC XXX.                         
00159              07  P-CARY          PIC X.                           
00160              07  FILLER          PIC XX.                          
00161          05  P-ADDR              PIC X(30).                       
00162          05  FILLER              PIC X.                           
00163          05  P-NAME.                                              
00164              07  P-TELFONE-SOC-SEC.                               
00165                  09  P-AREA      PIC XXX.                         
00166                  09  P-DSH1      PIC X.                           
00167                  09  P-PREF      PIC XXX.                         
00168                  09  P-DSH2      PIC X.                           
00169                  09  P-FONE      PIC X(4).                        
00170                  09  P-BLK       PIC X(3).                        
00171                  09  P-SOC       PIC X(11).                       
00172              07  P-ZIP-R REDEFINES P-TELFONE-SOC-SEC.             
00173                  09  P-ZIP       PIC X(9).                        
00174                  09  FILLER      PIC X(17).                       
00175                                                                   
00176      03  P-LINE-2 REDEFINES P-LINE.                               
00177          05  FILLER              PIC X(18).                       
00178          05  P-CAR-GROUP         PIC X(7).                        
00179          05  FILLER              PIC X.                           
00180          05  P-DESC              PIC X(30).                       
00181          05  FILLER              PIC X(31).                       
00182                                                                   
00183      03  P-LINE2.                                                 
00184          05  P-MO                PIC XX.                          
00185          05  P-DSH3              PIC X.                           
00186          05  P-DA                PIC XX.                          
00187          05  P-DSH4              PIC X.                           
00188          05  P-YR                PIC XX.                          
00189          05  FILLER              PIC X.                           
00190          05  P-BAL               PIC Z,ZZZ,ZZZ.ZZ-.               
00191          05  P-ADJ               PIC ZZ,ZZZ,ZZZ.ZZ-.              
00192          05  P-YTD               PIC ZZ,ZZZ,ZZZ.ZZ-.              
00193          05  FILLER              PIC X(9).                        
00194      03  P-LINE2-2 REDEFINES P-LINE2.                             
00195          05  P-CTR               PIC ZZZ,ZZZ,ZZZ-.                
00196          05  FILLER              PIC X.                           
00197          05  P-BAL-X             PIC ZZZ,ZZZ,ZZZ.ZZ-.             
00198          05  FILLER              PIC X(13).                       
00199          05  P-YTD-X             PIC ZZZ,ZZZ,ZZZ.ZZ-.             
00200          05  FILLER              PIC XXX.                         
00201      03  P-LINE2-3 REDEFINES P-LINE2.                             
00202          05  FILLER              PIC X(20).                       
00203          05  P-ADJ-X             PIC ZZZ,ZZZ,ZZZ.ZZ-.             
00204          05  FILLER              PIC X(24).                       
00205      EJECT                                                        
00206                                      COPY ELCAID.                 
00207  01  PF-AID REDEFINES DFHAID.                                     
00208      05  FILLER                      PIC X(8).                    
00209      05  PF-VALUES  OCCURS 24        PIC X.                       
00210      EJECT                                                        
00211                                      COPY ELCINTF.                
00212      12  PI-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.             
00213          16  PI-LO-DATE          PIC XX.                          
00214          16  PI-HI-DATE          PIC XX.                          
00215                                                                   
00216          16  PI-ERCOMP-KEY.                                       
00217              20  PI-CMP-COMPANY-CD  PIC X.                        
00218              20  PI-CMP-CODE        PIC X.                        
00219              20  PI-CMP-TABLE       PIC X(3).                     
00220                                                                   
00221          16  PI-SAVE-ERCOMP-KEY     PIC X(5).                     
00222                                                                   
00223          16  PI-BROWSE-SW           PIC X.                        
00224              88  BROWSE-STARTED              VALUE 'Y'.           
00225          16  PI-ERCOMP-EOF-SW       PIC X.                        
00226              88  ERCOMP-EOF                  VALUE 'Y'.           
00227          16  PI-EXCESS-SW           PIC X.                        
00228              88  EXCESS-LEVEL-EXISTS         VALUE 'X'.           
00229          16  PI-COMPANY-ADD-SW      PIC X.                        
00230              88  COMPANY-RECORD-ADDED        VALUE 'Y'.           
00231                                                                   
00232          16  PI-SUB                 PIC S99.                      
00233          16  PI-LAST-LEVEL          PIC S99.                      
00234          16  FILLER                 PIC X(618).                   
00235                                                                   
00236      EJECT                                                        
00237  LINKAGE SECTION.                                                 
00238  01  DFHCOMMAREA                     PIC X(1024).                  
00239 *01 PARM-LIST .                                                   
00240 *    02  FILLER              PIC S9(8)   COMP.                    
00241 *    02  ERCOMP-POINTER      PIC S9(8)   COMP.                    
00242 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                    
00243      EJECT                                                        
00244                           COPY ERCCOMP.                           
00245                                                                   
00246                           COPY ELCCNTL SUPPRESS.                  
00247      EJECT                                                        
00248                                                                   
00249  PROCEDURE DIVISION.                                              
00250                                                                   
00251      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               
00252      MOVE '5'                   TO DC-OPTION-CODE.                
00253      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
00254      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    
00255      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                
00256      MOVE DC-GREG-DATE-1-ALPHA  TO  SAVE-DATE-ALPHA.              
00257                                                                   
00258      MOVE DFHCOMMAREA           TO  PROGRAM-INTERFACE-BLOCK.      
00259      MOVE 'EL675'               TO  WS-REPORT-ID.                 
00260                                                                   
00261  1000-START.                                                      
00262      EXEC CICS  HANDLE CONDITION                                  
00263             ERROR    (8800-ABEND)                                 
00264             PGMIDERR (8900-PGMIDERR)                              
00265      END-EXEC.                                                    
00266                                                                   
00267  2000-RECEIVE.                                                    
00268      EXEC CICS RETRIEVE                                           
00269          INTO   (PROGRAM-INTERFACE-BLOCK)                         
00270          LENGTH (CLEN)                                            
00271      END-EXEC.                                                    
00272                                                                   
00273  2000-CHECK-IN-PROGRESS.                                          
00274                                                                   
00275      IF  MORTGAGE-SESSION                                         
00276          MOVE COMP-FILE-ID-CV    TO COMP-FILE-ID                  
00277                                                                   
00278      ELSE                                                         
00279          MOVE COMP-FILE-ID-CR    TO COMP-FILE-ID.                 
00280                                                                   
00281      EXEC CICS  HANDLE CONDITION                                  
00282             NOTFND   (2000-WRITE-INITIAL-TRAILER)                 
00283      END-EXEC.                                                    
00284                                                                   
00285      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               
00286      MOVE 'RF'                   TO  RF-RECORD-ID.                
00287      MOVE '2'                    TO  RF-RECORD-TYPE.              
00288      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                
00289      MOVE ZEROS                  TO  RF-LINE-NUMBER.              
00290                                                                   
00291      EXEC CICS READ                                               
00292          DATASET (REPT-FILE-ID)                                   
00293          INTO    (REPORT-SAVE-FILE)                               
00294          RIDFLD  (RF-CONTROL-PRIMARY)                             
00295      END-EXEC.                                                    
00296                                                                   
00297 ********IF RECORD FOUND, THEN ANOTHER REPORT HAS ALREADY          
00298 ********BEEN STARTED.  IF PREVIOUS REPORT ABENDED AND DIDN'T      
00299 ********COMPLETE, THEN OPERATOR MUST PURGE REPORT AND CREATE      
00300 ********A NEW ONE.                                                
00301      GO TO 9999-RETURN-CICS.                                      
00302                                                                   
00303  2000-WRITE-INITIAL-TRAILER.                                      
00304      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               
00305      MOVE 'RF'                   TO  RF-RECORD-ID.                
00306      MOVE '2'                    TO  RF-RECORD-TYPE.              
00307      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                
00308      MOVE ZEROS                  TO  RF-LINE-NUMBER.              
00309                                                                   
00310      MOVE SPACES                 TO  RF-TRAILER-RECORD.           
00311      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                     
00312      END-EXEC                                                     
00313      EXEC CICS FORMATTIME                                         
00314                ABSTIME(LCP-CICS-TIME)                             
00315                TIME(LCP-TIME-OF-DAY-XX)                           
00316      END-EXEC                                                     
00317      MOVE  LCP-TIME-OF-DAY-68 TO RF-PRINT-HH-MM-SS.               
00318      MOVE 'STARTED'              TO  RF-CURRENT-DATE.             
00319                                                                   
00320      EXEC CICS WRITE                                              
00321          DATASET (REPT-FILE-ID)                                   
00322          FROM    (REPORT-SAVE-FILE)                               
00323          RIDFLD  (RF-CONTROL-PRIMARY)                             
00324      END-EXEC.                                                    
00325                                                                   
00326  2100-DELETE-REC.                                                 
00327      MOVE 1 TO RF-LINE-NUMBER.                                    
00328      EXEC CICS  HANDLE CONDITION                                  
00329             NOTFND   (2300-DELETE-REC)                            
00330      END-EXEC.                                                    
00331                                                                   
00332  2200-DELETE-1.                                                   
00333      MOVE PI-COMPANY-CD TO RF-COMPANY-CD.                         
00334      MOVE 'RF'          TO RF-RECORD-ID.                          
00335      MOVE '1'           TO RF-RECORD-TYPE.                        
00336      MOVE WS-REPORT-ID  TO RF-REPORT-ID.                          
00337                                                                   
00338      EXEC CICS DELETE                                             
00339          DATASET   (REPT-FILE-ID)                                 
00340          RIDFLD    (RF-CONTROL-PRIMARY)                           
00341          KEYLENGTH (11)                                           
00342      END-EXEC.                                                    
00343                                                                   
00344      ADD 1 TO RF-LINE-NUMBER.                                     
00345      GO TO 2200-DELETE-1.                                         
00346                                                                   
00347  2300-DELETE-REC.                                                 
00348      EXEC CICS  HANDLE CONDITION                                  
00349             NOTFND   (3000-START-BROWSE)                          
00350      END-EXEC.                                                    
00351                                                                   
00352  2400-DELETE-2.                                                   
00353      MOVE PI-COMPANY-CD TO RF-COMPANY-CD.                         
00354      MOVE 'RF'          TO RF-RECORD-ID.                          
00355      MOVE '2'           TO RF-RECORD-TYPE.                        
00356      MOVE WS-REPORT-ID  TO RF-REPORT-ID.                          
00357                                                                   
00358      EXEC CICS DELETE                                             
00359          DATASET   (REPT-FILE-ID)                                 
00360          RIDFLD    (RF-CONTROL-PRIMARY)                           
00361          KEYLENGTH (11)                                           
00362      END-EXEC.                                                    
00363                                                                   
00364      ADD 1 TO RF-LINE-NUMBER.                                     
00365      GO TO 2400-DELETE-2.                                         
00366                                                                   
00367  3000-START-BROWSE.                                               
00368      IF PI-PROCESSOR-ID = 'CBL '                                  
00369         GO TO 5500-LABEL-ONLY-ROUTINE.                            
00370                                                                   
00371      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 
00372      MOVE SPACES                 TO CNTL-ACCESS.                  
00373      MOVE '1'                    TO CNTL-REC-TYPE.                
00374      MOVE +0                     TO CNTL-SEQ-NO.                  
00375                                                                   
00376      PERFORM 9000-READ-CONTROL THRU 9000-EXIT.                    
00377                                                                   
00378      MOVE CF-CL-MAIL-TO-NAME     TO H2-COMP.                      
00379      MOVE SAVE-DATE              TO H2-DATE.                      
00380      MOVE ' THRU '               TO H3-THRU.                      
00381                                                                   
00382      MOVE SPACE                  TO DC-OPTION-CODE.               
00383      MOVE PI-LO-DATE             TO DC-BIN-DATE-1.                
00384      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
00385      MOVE DC-GREG-DATE-1-EDIT    TO H3-DATE1.                     
00386                                                                   
00387      IF PI-LO-DATE = LOW-VALUES                                   
00388         MOVE 'INCEPT.'           TO H3-DATE1.                     
00389                                                                   
00390      MOVE SPACE                  TO DC-OPTION-CODE.               
00391      MOVE PI-HI-DATE             TO DC-BIN-DATE-1.                
00392      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
00393      MOVE DC-GREG-DATE-1-EDIT    TO H3-DATE2.                     
00394                                                                   
00395      IF PI-HI-DATE = HIGH-VALUES                                  
00396         MOVE 'CURRENT'        TO H3-DATE2.                        
00397                                                                   
00398      EXEC CICS  HANDLE CONDITION                                  
00399             NOTFND   (9999-RETURN-CICS)                           
00400      END-EXEC.                                                    
00401                                                                   
00402      MOVE LOW-VALUES             TO PI-ERCOMP-KEY.                
00403      MOVE PI-COMPANY-CD          TO PI-CMP-COMPANY-CD.            
00404                                                                   
00405      EXEC CICS STARTBR                                            
00406           DATASET  (COMP-FILE-ID)                                 
00407           RIDFLD   (PI-ERCOMP-KEY)                                
00408      END-EXEC.                                                    
00409                                                                   
00410      EXEC CICS  HANDLE CONDITION                                  
00411             ENDFILE  (4500-ENDBROWSE)                             
00412      END-EXEC.                                                    
00413                                                                   
00414  4000-READNEXT.                                                   
00415      EXEC CICS READNEXT                                           
00416           DATASET  (COMP-FILE-ID)                                 
00417           SET      (ADDRESS OF COMPENSATION-MASTER)               
00418           RIDFLD   (PI-ERCOMP-KEY)                                
00419      END-EXEC.                                                    
00420                                                                   
00421      IF PI-COMPANY-CD = CO-COMPANY-CD                             
00422         GO TO 5000-PRINT-IT.                                      
00423                                                                   
00424  4500-ENDBROWSE.                                                  
00425      MOVE '* TOTALS' TO P-LINE2.                                  
00426      MOVE F-BAL      TO P-BAL.                                    
00427      MOVE F-ADJ      TO P-ADJ.                                    
00428      MOVE F-YTD      TO P-YTD.                                    
00429      MOVE '-'        TO RF-CTL-CHAR-133.                          
00430      MOVE P-LINE2    TO RF-DATA-133.                              
00431      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        
00432      MOVE '1'        TO RF-CTL-CHAR-133.                          
00433      MOVE SPACES     TO RF-DATA-133.                              
00434      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        
00435                                                                   
00436      EXEC CICS ENDBR                                              
00437           DATASET  (COMP-FILE-ID)                                 
00438      END-EXEC.                                                    
00439                                                                   
00440  4600-DELETE-INITIAL-2.                                           
00441      MOVE PI-COMPANY-CD    TO RF-COMPANY-CD.                      
00442      MOVE 'RF'             TO RF-RECORD-ID.                       
00443      MOVE '2'              TO RF-RECORD-TYPE.                     
00444      MOVE WS-REPORT-ID     TO RF-REPORT-ID.                       
00445      MOVE ZEROS            TO RF-LINE-NUMBER.                     
00446                                                                   
00447      EXEC CICS DELETE                                             
00448          DATASET   (REPT-FILE-ID)                                 
00449          RIDFLD    (RF-CONTROL-PRIMARY)                           
00450          KEYLENGTH (11)                                           
00451      END-EXEC.                                                    
00452                                                                   
00453      GO TO 8999-WRITE-TRAILER.                                    
00454                                                                   
00455  5000-PRINT-IT.                                                   
00456      IF CO-LAST-MAINT-DT LESS    PI-LO-DATE OR                    
00457         CO-LAST-MAINT-DT GREATER PI-HI-DATE                       
00458         GO TO 4000-READNEXT.                                      
00459                                                                   
00460      MOVE SPACES TO P-LINE.                                       
00461                                                                   
00462      IF PRT-CNT = 4                                               
00463              PERFORM 6500-HDR-RTN THRU 6500-H-R-X.                
00464                                                                   
00465      ADD 1          TO PRT-CNT.                                   
00466      MOVE '0'       TO RF-CTL-CHAR-133.                           
00467      MOVE HDR-5     TO RF-DATA-133.                               
00468      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        
00469      MOVE ' '       TO RF-CTL-CHAR-133.                           
00470      MOVE HDR-6     TO RF-DATA-133.                               
00471      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        
00472                                                                   
00473      IF CO-RESP-NO NOT = LOW-VALUE                                
00474          MOVE CO-RESP-NO TO P-RESP.                               
00475                                                                   
00476      IF CO-ACCOUNT NOT = LOW-VALUE                                
00477          MOVE CO-ACCOUNT TO P-ACCT.                               
00478                                                                   
00479      IF CO-YTD-OVR-UNDR NOT NUMERIC                               
00480          MOVE ZEROS TO CO-YTD-OVR-UNDR.                           
00481                                                                   
00482      MOVE CO-TYPE            TO P-TYPE.                           
00483      MOVE CO-BALANCE-CONTROL TO P-CARY.                           
00484      MOVE CO-MAIL-NAME       TO P-ADDR.                           
00485      MOVE CO-ACCT-NAME       TO P-NAME.                           
00486                                                                   
00487      MOVE '0'                TO RF-CTL-CHAR-133.                  
00488      MOVE P-LINE             TO RF-DATA-133.                      
00489      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        
00490                                                                   
00491      MOVE SPACES             TO P-FILL P-BLK.                     
00492      MOVE CO-ADDR-1          TO P-ADDR.                           
00493      MOVE CO-AREA-CODE       TO P-AREA.                           
00494      MOVE CO-PREFIX          TO P-PREF.                           
00495      MOVE CO-PHONE           TO P-FONE.                           
00496      MOVE '-'                TO P-DSH1 P-DSH2.                    
00497      MOVE CO-SOC-SEC         TO P-SOC.                            
00498      MOVE ' '                TO RF-CTL-CHAR-133.                  
00499      MOVE P-LINE             TO RF-DATA-133.                      
00500      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        
00501      MOVE SPACES        TO P-NAME.                                
00502                                                                   
00503      IF CO-ADDR-2 NOT = SPACES                                    
00504          MOVE CO-ADDR-2 TO P-ADDR                                 
00505          MOVE ' '        TO RF-CTL-CHAR-133                       
00506          MOVE P-LINE     TO RF-DATA-133                           
00507          PERFORM 7000-PRT-LINE THRU 7000-EXIT.                    
00508                                                                   
00509 *    MOVE CO-ADDR-3      TO P-ADDR.                               
           STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
              DELIMITED BY '  ' INTO P-ADDR
           END-STRING
00510      MOVE CO-ZIP         TO P-ZIP.                                
00511      MOVE ' '            TO RF-CTL-CHAR-133.                      
00512      MOVE P-LINE         TO RF-DATA-133.                          
00513      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        
00514                                                                   
00515      MOVE '0'            TO RF-CTL-CHAR-133.                      
00516      MOVE HDR-5A         TO RF-DATA-133.                          
00517      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        
00518      MOVE ' '            TO RF-CTL-CHAR-133.                      
00519      MOVE HDR-6A         TO RF-DATA-133.                          
00520      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        
00521      MOVE SPACES         TO P-LINE2.                              
00522                                                                   
00523      MOVE CO-LAST-MAINT-DT   TO DC-BIN-DATE-1.                    
00524      MOVE SPACE              TO DC-OPTION-CODE.                   
00525      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
00526      MOVE DC-GREG-DATE-1-MDY TO WK-DATE.                          
00527      MOVE WK-MO              TO P-MO.                             
00528      MOVE WK-DA              TO P-DA.                             
00529      MOVE WK-YR              TO P-YR.                             
00530                                                                   
00531      MOVE CO-END-BAL         TO P-BAL.                            
00532      MOVE CO-YTD-OVR-UNDR    TO P-ADJ.                            
00533      COMPUTE WORK-AMT = CO-YTD-OV + CO-YTD-COM.                   
00534      MOVE WORK-AMT            TO P-YTD.                           
00535      MOVE '-'                 TO P-DSH3  P-DSH4.                  
00536      MOVE ' '                 TO RF-CTL-CHAR-133.                 
00537      MOVE P-LINE2             TO RF-DATA-133.                     
00538      MOVE SPACES              TO P-LINE2.                         
00539      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        
00540                                                                   
00541      IF PRT-CNT NOT = 4                                           
00542          MOVE '0'             TO RF-CTL-CHAR-133                  
00543          MOVE DASH-LINE       TO RF-DATA-133                      
00544          PERFORM 7000-PRT-LINE THRU 7000-EXIT.                    
00545                                                                   
00546      ADD CO-END-BAL      TO F-BAL.                                
00547      ADD CO-YTD-OVR-UNDR TO F-ADJ.                                
00548      ADD CO-YTD-OV       TO F-YTD.                                
00549      ADD CO-YTD-COM      TO F-YTD.                                
00550                                                                   
00551      GO TO 4000-READNEXT.                                         
00552      EJECT                                                        
00553  5500-LABEL-ONLY-ROUTINE.                                         
00554      EXEC CICS  HANDLE CONDITION                                  
00555             NOTFND   (9999-RETURN-CICS)                           
00556      END-EXEC.                                                    
00557                                                                   
00558      MOVE LOW-VALUES    TO PI-ERCOMP-KEY.                         
00559      MOVE PI-COMPANY-CD TO PI-CMP-COMPANY-CD.                     
00560                                                                   
00561      EXEC CICS STARTBR                                            
00562           DATASET  (COMP-FILE-ID)                                 
00563           RIDFLD   (PI-ERCOMP-KEY)                                
00564      END-EXEC.                                                    
00565                                                                   
00566      EXEC CICS  HANDLE CONDITION                                  
00567             ENDFILE  (5520-ENDBROWSE)                             
00568      END-EXEC.                                                    
00569                                                                   
00570  5510-READNEXT.                                                   
00571      EXEC CICS READNEXT                                           
00572           DATASET  (COMP-FILE-ID)                                 
00573           SET      (ADDRESS OF COMPENSATION-MASTER)               
00574           RIDFLD   (PI-ERCOMP-KEY)                                
00575      END-EXEC.                                                    
00576                                                                   
00577      IF PI-COMPANY-CD NOT = CO-COMPANY-CD                         
00578         GO TO 5520-ENDBROWSE.                                     
00579                                                                   
00580      MOVE CO-MAIL-NAME       TO RF-DATA-133.                      
00581      MOVE '0'                 TO RF-CTL-CHAR-133.                 
00582      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        
00583      MOVE CO-ACCT-NAME        TO RF-DATA-133.                     
00584      MOVE ' '                 TO RF-CTL-CHAR-133.                 
00585      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        
00586      MOVE CO-ADDR-1          TO RF-DATA-133.                      
00587      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        
00588      MOVE CO-ADDR-2          TO RF-DATA-133.                      
00589      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        
00590 *    MOVE CO-ADDR-3          TO RF-DATA-133.                      
           STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
              DELIMITED BY '  ' INTO RF-DATA-133
           END-STRING
00591      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        
00592      MOVE CO-ZIP             TO RF-DATA-133.                      
00593      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        
00594                                                                   
00595      GO TO 5510-READNEXT.                                         
00596                                                                   
00597  5520-ENDBROWSE.                                                  
00598      EXEC CICS ENDBR                                              
00599           DATASET  (COMP-FILE-ID)                                 
00600      END-EXEC.                                                    
00601                                                                   
00602      PERFORM 8999-WRITE-TRAILER.                                  
00603      GO TO 9999-RETURN-CICS.                                      
00604                                                                   
00605  EJECT                                                            
00606  6500-HDR-RTN.                                                    
00607      ADD +1              TO WS-PAGE.                              
00608      MOVE WS-PAGE        TO H3-PAGE.                              
00609      MOVE '1'            TO RF-CTL-CHAR-133.                      
00610      MOVE HDR-1          TO RF-DATA-133.                          
00611      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        
00612      MOVE ' '            TO RF-CTL-CHAR-133.                      
00613      MOVE HDR-2          TO RF-DATA-133.                          
00614      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        
00615      MOVE ' '            TO RF-CTL-CHAR-133.                      
00616      MOVE HDR-3          TO RF-DATA-133.                          
00617      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        
00618      MOVE CO-CARRIER     TO HD-CARR.                              
00619      MOVE CO-GROUPING    TO HD-GROUP.                             
00620      MOVE CO-ACCT-NAME   TO HD-NAME.                              
00621      MOVE '0'            TO RF-CTL-CHAR-133.                      
00622      MOVE HDR-4          TO RF-DATA-133.                          
00623      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                        
00624      MOVE +0             TO PRT-CNT.                              
00625                                                                   
00626  6500-H-R-X.                                                      
00627      EXIT.                                                        
00628                                                                   
00629  7000-PRT-LINE.                                                   
00630      MOVE PI-COMPANY-CD  TO RF-COMPANY-CD.                        
00631      MOVE 'RF'           TO RF-RECORD-ID.                         
00632      MOVE '1'            TO RF-RECORD-TYPE.                       
00633      MOVE WS-REPORT-ID   TO RF-REPORT-ID.                         
00634      ADD 1               TO WS-LINE-NUMBER.                       
00635      MOVE WS-LINE-NUMBER TO RF-LINE-NUMBER.                       
00636                                                                   
00637      EXEC CICS WRITE                                              
00638          DATASET (REPT-FILE-ID)                                   
00639          FROM    (REPORT-SAVE-FILE)                               
00640          RIDFLD  (RF-CONTROL-PRIMARY)                             
00641      END-EXEC.                                                    
00642                                                                   
00643  7000-EXIT.                                                       
00644       EXIT.                                                       
00645                                                                   
00646  8500-DATE-CONVERT.                                               
00647      MOVE LINK-ELDATCV           TO PGM-NAME.                     
00648                                                                   
00649      EXEC CICS LINK                                               
00650          PROGRAM    (PGM-NAME)                                    
00651          COMMAREA   (DATE-CONVERSION-DATA)                        
00652          LENGTH     (DC-COMM-LENGTH)                              
00653      END-EXEC.                                                    
00654                                                                   
00655  8500-EXIT.                                                       
00656      EXIT.                                                        
00657                                                                   
00658  8800-ABEND.                                                      
00659      MOVE DFHEIBLK TO EMI-LINE1.                                  
00660                                                                   
00661      EXEC CICS LINK                                               
00662          PROGRAM   ('EL004')                                      
00663          COMMAREA  (EMI-LINE1)                                    
00664          LENGTH    (72)                                           
00665      END-EXEC.                                                    
00666                                                                   
00667      GO TO 9999-RETURN-CICS.                                      
00668                                                                   
00669  8900-PGMIDERR.                                                   
00670      GO TO 9999-RETURN-CICS.                                      
00671                                                                   
00672  8999-WRITE-TRAILER.                                              
00673      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               
00674      MOVE 'RF'                   TO  RF-RECORD-ID.                
00675      MOVE '2'                    TO  RF-RECORD-TYPE.              
00676      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                
00677      ADD +1                      TO  WS-LINE-NUMBER.              
00678      MOVE WS-LINE-NUMBER         TO  RF-LINE-NUMBER.              
00679                                                                   
00680      MOVE SPACES                 TO  RF-TRAILER-RECORD.           
00681      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                     
00682      END-EXEC                                                     
00683      EXEC CICS FORMATTIME                                         
00684                ABSTIME(LCP-CICS-TIME)                             
00685                TIME(LCP-TIME-OF-DAY-XX)                           
00686      END-EXEC                                                     
00687      MOVE  LCP-TIME-OF-DAY-68 TO RF-PRINT-HH-MM-SS.               
00688      MOVE SAVE-DATE              TO  RF-CURRENT-DATE.             
00689                                                                   
00690      EXEC CICS WRITE                                              
00691          DATASET (REPT-FILE-ID)                                   
00692          FROM    (REPORT-SAVE-FILE)                               
00693          RIDFLD  (RF-CONTROL-PRIMARY)                             
00694      END-EXEC.                                                    
00695                                                                   
00696  9000-READ-CONTROL.                                               
00697      EXEC CICS READ                                               
00698          DATASET     (CNTL-ID)                                    
00699          SET         (ADDRESS OF CONTROL-FILE)                    
00700          RIDFLD      (ELCNTL-KEY)                                 
00701      END-EXEC.                                                    
00702                                                                   
00703  9000-EXIT.                                                       
00704       EXIT.                                                       
00705                                                                   
00706  9999-RETURN-CICS.                                                
00707      EXEC CICS  RETURN                                            
00708      END-EXEC.                                                    
00709                                                                   
00710  9999-EXIT.                                                       
00711       EXIT.                                                       
00712                                                                   
