00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 ECS150.                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 01/01/97 16:27:55.                 
00007 *             PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE           
00008 *                            VMOD=2.029.                          
00009 *AUTHOR         LOGIC, INC.                                       
00010 *               DALLAS, TEXAS.                                    
00011 *DATE-COMPILED.                                                   
00012                                                                   
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
00023 *REMARKS.                                                         
00024 *        THIS PROGRAM PRODUCES THE EXTRACT RECORDS                
00025 *        NEEDED TO COMPLETE THE LIFE AND A/H ANNUAL REPORT.       
00026 *        DETAIL IS GENERATED IF REQUESTED.                        
00027  EJECT                                                            
00028  ENVIRONMENT DIVISION.                                            
00029  CONFIGURATION SECTION.                                           
00030  INPUT-OUTPUT SECTION.                                            
00031  FILE-CONTROL.                                                    
00032                                                                   
00033      SELECT SORT-WORK        ASSIGN TO SYS001-UT-2314-S-SORTWK1.  
00034      SELECT PRINT-FILE       ASSIGN TO SYS008-UR-1403-S-SYS008.   
00035      SELECT CERT-FILE        ASSIGN TO SYS011-UT-2400-S-SYS011.   
00036      SELECT REINS-EXTRACT-2  ASSIGN TO SYS012-UT-2400-S-SYS012.   
pemuni     SELECT ERRTBL-IN        ASSIGN TO ERRTBLT
00038                              ORGANIZATION IS INDEXED              
00039                              ACCESS IS DYNAMIC                    
00040                              FILE STATUS IS ERRTBL-FILE-STATUS    
00041                              RECORD KEY IS RE-CONTROL-PRIMARY.    
00042      SELECT ERACCTT          ASSIGN TO SYS010-FBA1-ERACCTT        
00043                              ORGANIZATION IS INDEXED              
00044                              ACCESS IS SEQUENTIAL                 
00045                              FILE STATUS IS ACCT-FILE-STATUS      
00046                              RECORD KEY IS AM-CONTROL-PRIMARY.    
00047      SELECT REINS-EXTRACT-1  ASSIGN TO SYS018-UT-2400-S-SYS018.   
00048      SELECT DISK-DATE        ASSIGN TO SYS019-UT-2314-S-SYS019.   
00049      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   
00050      SELECT CLAS150-A-EXTRACT ASSIGN TO SYS021-UT-2400-S-SYS021.  
00051      SELECT CLAS150-B-EXTRACT ASSIGN TO SYS022-UT-2400-S-SYS022.  
00052      SELECT CLAS150-C-EXTRACT ASSIGN TO SYS023-UT-2400-S-SYS023.  
00053  EJECT                                                            
00054  DATA DIVISION.                                                   
00055  FILE SECTION.                                                    
00056                                                                   
00057  SD  SORT-WORK.                                                   
00058 *    RECORD CONTAINS 210 CHARACTERS                               
00059                                                                   
00060  01  SORT-REC.                                                    
00061      12  SORT-KEY            PIC X(42).                           
00062      12  FILLER              PIC X(168).                          
00063  EJECT                                                            
00064  FD  PRINT-FILE                                                   
00065                              COPY ELCPRTFD.                       
00066  EJECT                                                            
00067  FD  CERT-FILE                                                    
00068                              COPY ECSCRIFD.                       
00069  EJECT                                                            
00070  FD  REINS-EXTRACT-2                                              
00071      BLOCK CONTAINS 0 RECORDS
00072      RECORDING MODE IS F.                                         
00073 *    RECORD CONTAINS 210 CHARACTERS                               
00074                                                                   
00075  01  RC-REC-2.                                                    
00076      12  RC-CONTRAL-2        PIC  X(210).                         
00077  EJECT                                                            
00078  FD  ERRTBL-IN
00079                              COPY ECSRTFDD.                       
00080                                                                   
00081                              COPY ERCREIN.                        
00082  EJECT                                                            
00083  FD  ERACCTT.                                                     
00084                              COPY ERCACCT.                        
00085  EJECT                                                            
00086  FD  REINS-EXTRACT-1                                              
00087      BLOCK CONTAINS 0 RECORDS
00088      RECORDING MODE IS F.                                         
00089 *    RECORD CONTAINS 210 CHARACTERS                               
00090                                                                   
00091  01  RC-REC-1.                                                    
00092      12  RC-CONTRAL-1        PIC  X(210).                         
00093                                                                   
00094  EJECT                                                            
00095  FD  DISK-DATE                                                    
00096                              COPY ELCDTEFD.                       
00097  EJECT                                                            
00098  FD  FICH                                                         
00099      RECORDING MODE IS F.                                         
00100                                                                   
00101  01  FICH-REC                PIC X(133).                          
00102                                                                   
00103  EJECT                                                            
00104  FD  CLAS150-A-EXTRACT                                            
00105      BLOCK CONTAINS 0 RECORDS
00106      RECORDING MODE IS F.                                         
00107 *    RECORD CONTAINS 2701 CHARACTERS                              
00108 *                                                                 
00109 *  THIS EXTRACT FILE CONTAINS THE RECORDS                         
00110 *  NEEDED TO PRINT THE CERTIFICATE ACTIVITY AND EXHIBIT           
00111 *  REPORT FOR THE 1ST INSURANCE COMPANY                           
00112 *                                                                 
00113 *          ** RECORD TYPE AND SORT KEY DESCRIPTION **             
00114 ******** SORT KEY ***************   ******** USAGE *********      
00115 *TYPE*CARR*COMP*STATE*ACCT*STATE*                                 
00116 * 01 * X  *  X *  X  * X  * LV  * - ACCT TOTAL-1ST INS.           
00117 * 02 * X  *  X *  X  * X  * LV  * - ACCT TOTAL-REINS              
00118 * 03 * X  *  X *  X  * HV * LV  * - ST TOT-1ST INS                
00119 * 04 * X  *  X *  X  * HV * LV  * - ST TOT-REINS                  
00120 * 05 * X  *  X *  HV * HV * LV  * - ST TOT-1ST,ACCUM FOR CO TOTAL 
00121 * 06 * X  *  X *  HV * HV * LV  * - ST TOT-REINS,ACCUM FOR CO TOT 
00122 * 07 * X  * HV *  X  * HV * LV  * - ST TOT-1ST,ACCUM FOR ST/CARR  
00123 * 08 * X  * HV *  X  * HV * LV  * - ST TOT-REIN,ACCUM FOR ST/CARR 
00124 * 09 * X  * HV *  HV * HV * LV  * - ST TOT-1ST,ACCUM FOR CARR TOT 
00125 * 10 * X  * HV *  HV * HV * LV  * - ST TOT-REIN,ACCUM FOR CARR TOT
00126 * 11 * HV * HV *  HV * HV * LV  * - ST TOT-1ST,ACCUM FOR GRND TOT 
00127 * 12 * HV * HV *  HV * HV * LV  * - ST TOT-REIN,ACCUM FOR GRND TOT
00128 * 13 * HV * HV *  HV * HV *  X  * - ST TOT-1ST,ACCUM FOR ST TOTAL 
00129 * 14 * HV * HV *  HV * HV *  X  * - ST TOT-REIN,ACCUM FOR ST TOTAL
00130 *                                                                 
00131 * X-INDICATES FIELD INITIALIZED WITH ACTUAL VALUE                 
00132 * LV - INDICATES FIELD WILL CONTAIN LOW-VALUE                     
00133 * HV - INDICATES FIELD WILL CONTAIN HIGH-VALUE                    
00134 *                                                                 
00135 ***NOTE - FIELD ER-REINS-CO IS SET TO LOW-VALUE FOR ALL RECORDS   
00136 *         IN THIS FILE SINCE IT IS NOT USED FOR THIS REPORT       
00137                                                                   
00138  01  CLAS150-EXTRACT-RECORD-A    PIC X(2701).                     
00139  EJECT                                                            
00140  FD  CLAS150-B-EXTRACT                                            
00141      BLOCK CONTAINS 0 RECORDS
00142      RECORDING MODE IS F.                                         
00143 *    RECORD CONTAINS 2701 CHARACTERS                              
00144 *                                                                 
00145 *  THIS FILE CONTAINS THE RECORDS NEEDED TO                       
00146 *  PRINT THE CERTIFICATE ACTIVITY AND EXHIBIT                     
00147 *  REPORT FOR THE REINSURANCE COMPANY WITH THE                    
00148 *  EXCEPTION OF OVER STATE TOTALS.                                
00149 *                                                                 
00150 *          ** RECORD TYPE AND SORT KEY DESCRIPTION **             
00151 ******* SORT KEY *********                                        
00152 *RE-CO*CARR*COMP*STATE*ACCT*STATE*TYPE*                           
00153 *  X  * X  *  X *  X  * X  * LV  * 15 *   ACCT TOTAL              
00154 *  X  * X  *  X *  X  * LV * LV  * 16 *   STATE TOTAL             
00155 *  X  * X  *  X *  LV * LV * LV  * 17 *   COMPANY TOTAL           
00156 *  X  * X  * LV *  LV * LV * LV  * 18 *   CARRIER TOTAL           
00157 *  X  * LV * LV *  LV * LV * LV  * 19 *   REINS CO TOTAL          
00158 *     *    *    *     *    *     *    *                           
00159 * X= INITIALIZED TO ACTUAL VALUE       * TOTAL RECORD TYPE 19     
00160 * LV = INITIALIZED TO LOW-VALUE          FOR GRAND TOTALS         
00161                                                                   
00162  01  CLAS150-EXTRACT-RECORD-B    PIC X(2701).                     
00163  EJECT                                                            
00164  FD  CLAS150-C-EXTRACT                                            
00165      BLOCK CONTAINS 0 RECORDS
00166      RECORDING MODE IS F.                                         
00167 *    RECORD CONTAINS 2701 CHARACTERS                              
00168 *                                                                 
00169 *    THIS FILE CONTAINS THE RECORDS NEEDED TO PRINT               
00170 *    THE REINSURANCE COMPANY - CERTIFICATE AND EXHIBIT            
00171 *    REPORT - AT THE STATE LEVEL.                                 
00172 *                                                                 
00173 *    ALL RECORDS IN THIS FILE ARE TYPE 20                         
00174 *    ALL CONTROL FIELDS IN THE SORT KEY ARE SET TO                
00175 *     LOW-VALUE WITH THE EXCEPTION OF THE STATE AND TYPE          
00176 *     FIELDS.                                                     
00177 *                                                                 
00178 *                                                                 
00179                                                                   
00180  01  CLAS150-EXTRACT-RECORD-C    PIC X(2701).                     
00181  EJECT                                                            
00182  WORKING-STORAGE SECTION.                                         
00183  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      
00184  77  FILLER  PIC X(32) VALUE '********************************'.  
00185  77  FILLER  PIC X(32) VALUE '     ECS150 WORKING STORAGE     '.  
00186  77  FILLER  PIC X(32) VALUE '******* VMOD=2.029 *************'.  
00187                                                                   
00188                                                                   
00189  77  LINE-CNT                PIC S9(3)      COMP-3 VALUE +56.     
00190  77  FACTOR-C                PIC S9(3)      COMP-3 VALUE +0.      
00191  77  MINUS-CNT               PIC S9(3)      COMP-3 VALUE +0.      
00192  77  PAGE-CNT                PIC S9(5)      COMP-3 VALUE +0.      
00193  77  EXPIRED-MONTHS          PIC S9(5)      COMP-3 VALUE +0.      
00194  77  UNEXPIRED-MONTHS        PIC S9(3)V99   COMP-3 VALUE +0.      
00195  77  HOLD-PREV-UNEXP         PIC S9(5)      COMP-3 VALUE +0.      
00196  77  HOLD-PREV-EXP           PIC S9(5)      COMP-3 VALUE +0.      
00197  77  HOLD-END-UNEXP          PIC S9(5)      COMP-3 VALUE +0.      
00198  77  HOLD-END-EXP            PIC S9(5)      COMP-3 VALUE +0.      
00199  77  TERM-WORK               PIC S9(5)      COMP-3 VALUE +0.      
00200  77  REINS-REC-CNT           PIC S9(7)      COMP-3 VALUE +0.      
00201  77  EXB-CNT                 PIC S9(7)      COMP-3 VALUE ZERO.    
00202  77  FACTOR-A                PIC S9(4)V9(11) COMP-3 VALUE +0.     
00203  77  FACTOR-B                PIC S9(4)V9(11) COMP-3 VALUE +0.     
00204  77  WS-TOTAL-FACE-AMT       PIC S9(7)V99   COMP-3 VALUE +0.      
00205  77  UNEARNED-AMT-LF         PIC S9(7)V99   COMP-3 VALUE +0.      
00206  77  UNEARNED-AMT-AH         PIC S9(7)V99   COMP-3 VALUE +0.      
00207  77  SUMMARY-AMT-LF          PIC S9(9)V99   COMP-3 VALUE +0.      
00208  77  SUMMARY-AMT-AH          PIC S9(9)V99   COMP-3 VALUE +0.      
00209  77  MINUS-AMT               PIC S9(7)V99   COMP-3 VALUE +0.      
00210  77  TEMP-RESULT             PIC S9(9)V99   COMP-3 VALUE +0.      
00211  77  SAVE-BGN-INFORCE        PIC S9(9)V99   COMP-3 VALUE +0.      
00212  77  SAVE-END-INFORCE        PIC S9(9)V99   COMP-3 VALUE +0.      
00213  77  TEMP-RESULT-L           PIC S9(9)V9(6) COMP-3 VALUE +0.      
00214  77  SAVE-REM-TERM           PIC S9(3)V99   COMP-3 VALUE +0.      
00215  77  X                       PIC X                 VALUE SPACE.   
00216  77  ERRTBL-FILE-STATUS      PIC XX              VALUE '00'.      
00217  77  ACCT-FILE-STATUS        PIC XX              VALUE '00'.      
00218                                                                   
00219  EJECT                                                            
00220  01  WORK-AREAS.                                                  
           12  sub3                       pic s9(5) value +0 comp-3.
00221      12  WS-ZERO                    PIC S9     VALUE +0 COMP-3.
00222      12  WS-RETURN-CODE             PIC S9(4)  VALUE +0 COMP-3.   
00223      12  WS-ABEND-MESSAGE           PIC X(80)  VALUE SPACES.      
00224      12  WS-ABEND-FILE-STATUS       PIC XX     VALUE ZERO.        
00225      12  INTERIM-YR                 PIC 99     VALUE 0.           
00226      12  WS-931231                  PIC XX     VALUE X'8CFF'.     
00227      12  WS-CR-DIS-CCYY             PIC 9(04)  VALUE 0.           
00228      12  WS-CR-CCYY                 PIC 9(04)  VALUE 0.           
00229                                                                   
00230  01  FILLER PIC X(16)  VALUE 'CONTROL SWITCHES'.                  
00231  01  PROGRAM-CNTRL-SWITCHES.                                      
00232      12  PASS-TYPE           PIC 9               VALUE 0.         
00233          88  FIRST-INS-PASS                      VALUE 0.         
00234          88  REINSURANCE-PASS                    VALUE 1.         
00235      12  INFORCE-CNT-SW      PIC 9               VALUE 0.         
00236      12  RECORD-TYPE-SW      PIC 9               VALUE 0.         
00237          88  FIRST-INS-REC                       VALUE 0.         
00238          88  REINSURANCE-REC                     VALUE 1.         
00239      12  EFFECTIVE-CONTROL   PIC 9               VALUE 0.         
00240          88  EFFECTIVE-LAST-PER                  VALUE 1.         
00241          88  EFFECTIVE-THIS-PER                  VALUE 2.         
00242          88  EFFECTIVE-BOTH-PER                  VALUE 3.         
00243      12  CERT-STATUS         PIC 9               VALUE 0.         
00244          88  CERT-ACTIVE                         VALUE 0.         
00245          88  CERT-CANCELLED                      VALUE 1.         
00246          88  CERT-CLAIM                          VALUE 2.         
00247          88  LUMP-SUM                            VALUE 3.         
00248          88  CLAIM-OR-CANCEL                     VALUE 1 2 3.     
00249      12  REINSURANCE-FOUND   PIC 9               VALUE 0.         
00250          88  NO-REINSURANCE                      VALUE 0.         
00251      12  OB-SW               PIC 9               VALUE 0.         
00252          88  OB-COVERAGE                         VALUE 1.         
00253      12  SUMMARY-SW          PIC 9               VALUE 0.         
00254          88  SUMMARY-REC                         VALUE 1.         
00255      12  COUNT-CNTRL         PIC 9               VALUE 0.         
00256          88  ALREADY-COUNTED                     VALUE 1.         
00257      12  NEW-IN-EFFECT       PIC 9               VALUE 0.         
00258          88  ENTERED-THIS-PERIOD                 VALUE 1.         
00259      12  OLD-IN-EFFECT       PIC 9               VALUE 0.         
00260          88  ENTERED-PREV-PERIOD                 VALUE 1.         
00261      12  PRINT-CNTRL         PIC 9               VALUE 0.         
00262          88  FIRST-DETAIL-DONE                   VALUE 1.         
00263      12  INS-TYPE-SW         PIC 9               VALUE 0.         
00264          88  INDIVIDUAL-INS                      VALUE 1.         
00265          88  GROUP-INS                           VALUE 2.         
00266      12  LIFE-INS-SW         PIC 9               VALUE 0.         
00267          88  LEVEL-LIFE                          VALUE 1.         
00268          88  REDUCING-LIFE                       VALUE 2.         
00269      12  CANCEL-SW           PIC 9               VALUE 0.         
00270          88  CURRENT-CANCEL-ONLY                 VALUE 1.         
00271      12  CLAIM-SW            PIC 9               VALUE 0.         
00272          88  CURRENT-CLAIM-ONLY                  VALUE 1.         
00273      12  REINS-FILE-SW       PIC 9               VALUE 0.         
00274          88  REINS-FILE-CLOSED                   VALUE 0.         
00275          88  REINS-FILE-OPEN                     VALUE 1.         
00276      12  TEXAS-REG-SW        PIC 9               VALUE 0.         
00277          88  TEXAS-CERT                          VALUE 1.         
00278      12  NET-PAY-SW          PIC 9               VALUE 0.         
00279          88  NET-PAY-CERT                        VALUE 1.         
00280  01  FILLER PIC X(16)  VALUE 'PRINT   SWITCHES'.                  
00281  01  PRINT-SWITCHES.                                              
00282      12  LF-REFUND-SW        PIC 9               VALUE 0.         
00283          88 PRT-LF-REFUND                        VALUE 1.         
00284      12  AH-REFUND-SW        PIC 9               VALUE 0.         
00285          88 PRT-AH-REFUND                        VALUE 1.         
00286      12  LF-ISSUE            PIC 9               VALUE 0.         
00287          88 PRT-LF-ISSUE                         VALUE 1.         
00288      12  LF-CANC             PIC 9               VALUE 0.         
00289          88 PRT-LF-CANC                          VALUE 1.         
00290      12  LF-DEATH            PIC 9               VALUE 0.         
00291          88 PRT-LF-DEATH                         VALUE 1.         
00292      12  LF-EXPIRY           PIC 9               VALUE 0.         
00293          88 PRT-LF-EXPIRY                        VALUE 1.         
00294      12  LF-DECR             PIC 9               VALUE 0.         
00295          88 PRT-LF-DECR                          VALUE 1.         
00296      12  LF-INCR             PIC 9               VALUE 0.         
00297          88 PRT-LF-INCR                          VALUE 1.         
00298      12  LF-LUMP             PIC 9               VALUE 0.         
00299          88 PRT-LF-LUMP                          VALUE 1.         
00300      12  AH-ISSUE            PIC 9               VALUE 0.         
00301          88 PRT-AH-ISSUE                         VALUE 1.         
00302      12  AH-CANC             PIC 9               VALUE 0.         
00303          88 PRT-AH-CANC                          VALUE 1.         
00304      12  AH-CLAIM            PIC 9               VALUE 0.         
00305          88 PRT-AH-CLAIM                         VALUE 1.         
00306  01  PRINT-AMOUNTS.                                               
00307      12  PRT-ISSUE-AMT-LF    PIC S9(11)V99 COMP-3   VALUE ZERO.   
00308      12  PRT-CANC-AMT-LF     PIC S9(11)V99 COMP-3   VALUE ZERO.   
00309      12  PRT-DEATH-AMT-LF    PIC S9(11)V99 COMP-3   VALUE ZERO.   
00310      12  PRT-EXPIRY-AMT-LF   PIC S9(11)V99 COMP-3   VALUE ZERO.   
00311      12  PRT-DECR-AMT-LF     PIC S9(11)V99 COMP-3   VALUE ZERO.   
00312      12  PRT-INCR-AMT-LF     PIC S9(11)V99 COMP-3   VALUE ZERO.   
00313      12  PRT-ISSUE-AMT-AH    PIC S9(11)V99 COMP-3   VALUE ZERO.   
00314      12  PRT-CANC-AMT-AH     PIC S9(11)V99 COMP-3   VALUE ZERO.   
00315      12  PRT-CLAIM-AMT-AH    PIC S9(11)V99 COMP-3   VALUE ZERO.   
00316  EJECT                                                            
00317  01  RC-REC.                                                      
00318      12  RC-CONTROL.                                              
00319          16  RC-COMP            PIC X(6).                         
00320          16  RC-CMCTL.                                            
00321              20  RC-CNTRL-1     PIC X(19).                        
00322              20  RC-CNTRL-2     PIC X(17).                        
00323      12  RC-ENTRY-STATUS        PIC X.                            
00324      12  RC-LFTYP               PIC XX.                           
00325      12  RC-LF-TERM             PIC S9(3)       COMP-3.           
00326      12  RC-LFAMT               PIC S9(9)V99    COMP-3.           
00327      12  RC-LFPRM               PIC S9(7)V99    COMP-3.           
00328      12  RC-LFRFND              PIC S9(7)V99    COMP-3.           
00329      12  RC-LF-CANCEL-STATUS    PIC X.                            
00330      12  RC-LF-CLAIM-STATUS     PIC X.                            
00331      12  RC-LF-CURRENT-STATUS   PIC X.                            
00332      12  RC-AHTYP               PIC XX.                           
00333      12  RC-AH-TERM             PIC S9(3)       COMP-3.           
00334      12  RC-AHAMT               PIC S9(7)V99    COMP-3.           
00335      12  RC-AHPRM               PIC S9(7)V99    COMP-3.           
00336      12  RC-AHRFND              PIC S9(7)V99    COMP-3.           
00337      12  RC-AH-CANCEL-STATUS    PIC X.                            
00338      12  RC-AH-CLAIM-STATUS     PIC X.                            
00339      12  RC-AH-CURRENT-STATUS   PIC X.                            
00340      12  RC-DTHAMT              PIC S9(9)V99    COMP-3.           
00341      12  RC-DISAMT              PIC S9(9)V99    COMP-3.           
00342      12  RC-IND-GRP             PIC X.                            
00343      12  RC-LF-CANC-DT          PIC 9(11)       COMP-3.           
00344      12  RC-AH-CANC-DT          PIC 9(11)       COMP-3.           
00345      12  RC-DIS-DT              PIC 9(11)       COMP-3.           
00346      12  RC-ENT-DT              PIC 9(11)       COMP-3.           
00347      12  RC-LF-CANC-EXIT        PIC 9(11)       COMP-3.           
00348      12  RC-AH-CANC-EXIT        PIC 9(11)       COMP-3.           
00349      12  RC-LF-CLAIM-EXIT       PIC 9(11)       COMP-3.           
00350      12  RC-AH-CLAIM-EXIT       PIC 9(11)       COMP-3.           
00351      12  RC-PMT-FREQ            PIC 9(2).                         
00352      12  RC-APR                 PIC S9(3)V9(4)  COMP-3.           
00353      12  RC-LOAN-TERM           PIC S9(3)       COMP-3.           
00354      12  RC-I-LIVES             PIC S9(3)       COMP-3.           
00355      12  RC-C-LIVES-YTD         PIC S9(3)       COMP-3.           
00356      12  RC-C-LIVES-ITD         PIC S9(3)       COMP-3.           
00357      12  RC-LFRFND-YTD          PIC S9(7)V99    COMP-3.           
00358      12  RC-AHRFND-YTD          PIC S9(7)V99    COMP-3.           
00359      12  RC-LF-TERM-IN-DAYS     PIC S9(5)       COMP-3.           
00360      12  RC-LF-EXPIRE-DT        PIC 9(11)       COMP-3.           
00361      12  RC-AH-EXPIRE-DT        PIC 9(11)       COMP-3.           
00362      12  RC-LOAN-1ST-PMT-DT     PIC 9(6).                         
00363      12  RC-ORIG-LFAMT          PIC S9(9)V99    COMP-3.           
00364      12  RC-LFAMT-ALT           PIC S9(9)V99    COMP-3.           
00365      12  RC-CEDE-FACT           PIC S9(1)V9(4)  COMP-3.           
00366      12  FILLER                 PIC X.                            
00367  EJECT                                                            
00368  01  PROCESSING-CONTROL-DATES.                                    
00369      12  PERIOD-START        PIC XX.                              
00370      12  PERIOD-START-9      PIC S9(5)    COMP-3.                 
00371      12  PERIOD-END          PIC XX.                              
00372      12  PERIOD-END-9        PIC S9(5)    COMP-3.                 
00373      12  CERT-EFFECT-DT      PIC XX.                              
00374      12  FIRST-PAY-DT        PIC XX.                              
00375      12  CERT-EXPIRE-DATE    PIC XX.                              
PEMMOD     12  CERT-EXPIRE-DATE-O  PIC XX.
00376      12  CANCEL-EXIT-DATE    PIC XX.
00377      12  CLAIM-EXIT-DATE     PIC XX.                              
00378      12  VALUATION-DATE      PIC S9(5)    COMP-3.                 
00379      12  CERT-ENTRY-DATE     PIC XX.                              
00380      12  LOW-EXIT-DATE       PIC XX.                              
00381      12  SUM-CANCEL-DATE     PIC XX.                              
00382  EJECT                                                            
00383  01  SAVE-CONTROL-BREAKS                         VALUE SPACES.    
00384      12  LAST-REINS-CO       PIC X(6).                            
00385      12  SAVE-CNTRL-GRP.                                          
00386          16  LAST-CARRIER    PIC X.                               
00387          16  LAST-GROUPING   PIC X(6).                            
00388          16  LAST-STATE      PIC XX.                              
00389          16  LAST-ACCOUNT    PIC X(10).                           
00390                                                                   
00391  01  SAVE-EXP-DT.                                                 
00392      12  SE-YR             PIC 99.                                
00393      12  SE-MO             PIC 99.                                
00394      12  SE-DA             PIC 99.                                
00395                                                                   
00396  01  REMAINING-TERM-WORK-FIELDS.                                  
00397      12  BGN-DT.                                                  
00398          16  BGN-YR          PIC 99.                              
00399          16  BGN-MO          PIC 99.                              
00400          16  BGN-DA          PIC 99.                              
00401      12  END-DT.                                                  
00402          16  END-YR          PIC 99.                              
00403          16  END-MO          PIC 99.                              
00404          16  END-DA          PIC 99.                              
00405      12  TEMP-1              PIC S9(9)V9(7)  COMP-3 VALUE +0.     
00406      12  TEMP-2              PIC S9(9)V9(7)  COMP-3 VALUE +0.     
00407      12  TEMP-3              PIC S9(9)V9(7)  COMP-3 VALUE +0.     
00408      12  TEMP-4              PIC S9V9(8)     COMP-3 VALUE +0.     
00409      12  TEMP-5              PIC S9V9(8)     COMP-3 VALUE +0.     
00410      12  LF-REM-TRM          PIC S9(3)V99    COMP-3 VALUE +0.     
00411      12  AH-REM-TRM          PIC S9(3)V99    COMP-3 VALUE +0.     
00412      12  LF-BAL-REMTRM       PIC S9(3)V99    COMP-3 VALUE +0.     
00413      12  REM-TRM1            PIC S9(3)V99    COMP-3 VALUE +0.     
00414      12  REM-TRM2            PIC S9(3)V99    COMP-3 VALUE +0.     
00415      12  TEM-TRM1            PIC S9(3)V99    COMP-3 VALUE +0.     
00416      12  HLD-TRM             PIC S9(3)V99    COMP-3 VALUE +0.     
00417      12  RUN-DATE-BIN        PIC XX.                              
00418      12  EP-DT-BIN           PIC XX.                              
00419      12  WS-CR-BIN-DATE      PIC XX.                              
00420                                                                   
00421  01  TEXAS-REG-WORK-AREAS.                                        
00422      12  TEX-FACT-1          PIC S9(7)V99    COMP-3.              
00423      12  TEX-FACT-2          PIC S9(3)       COMP-3.              
00424      12  TEX-FACT-3          PIC S9(3)       COMP-3.              
00425      12  TEX-FACT-4          PIC S9(7)       COMP-3.              
00426      12  TEX-FACT-5          PIC S9(3)       COMP-3.              
00427      12  TEX-FACT-6          PIC S9(3)       COMP-3.              
00428      12  TEX-FACT-7          PIC S9(7)       COMP-3.              
00429      12  TEX-FACT-8          PIC S9V9(6)     COMP-3.              
00430      12  TEX-FACT-9          PIC S9(4)V9(11) COMP-3.              
00431                                                                   
00432  01  NET-PAY-INTERFACE.                                           
00433      12  NP-APR              PIC S9(3)V9(4)  COMP-3.              
00434      12  NP-ORIG             PIC S9(3)       COMP-3.              
00435      12  NP-REM              PIC S9(3)       COMP-3.              
00436      12  NP251-ORIG          PIC S9(3)V99    COMP-3.              
00437      12  NP251-REM           PIC S9(3)V99    COMP-3.              
00438      12  NP-SUB              PIC S9(3)       COMP-3.              
00439      12  NP-EFF-DT           PIC X(06).                           
00440      12  NP-OPT              PIC X.                               
00441      12  NP-CAP              PIC S9(3)       COMP-3.              
00442      12  NP-FACTOR           PIC S9(4)V9(9)  COMP-3.              
00443      12  NP-WORK1            PIC S9(6)V9(9)  COMP-3.              
00444      12  NP-WORK2            PIC S9(6)V9(9)  COMP-3.              
00445      12  NP-BENEFIT          PIC S9(9)V99    COMP-3.              
00446      12  NP-REMAINING        PIC S9(9)V99    COMP-3.              
00447      12  NP-AHPRM            PIC S9(7)V99    COMP-3.              
00448      12  NP-ACCOUNT          PIC X(10).                           
00449  EJECT                                                            
00450 ** OTHER WORK FIELDS                                              
00451  01  SAVE-REINS-CERT         PIC X(210).                          
00452                                                                   
00453  01  COUNT-WORK              PIC S9(3)   COMP-3  VALUE +0.        
00454                                                                   
00455  01  COUNT-SAVE              PIC S9(3)   COMP-3  VALUE +0.        
00456  01  INFORCE-DOLLARS         PIC S9(9)V99 COMP-3 VALUE +0.        
00457  01  DOLLAR-AMT              PIC S9(9)V99 COMP-3 VALUE +0.        
00458  01  DECREASING-DOLLAR       PIC S9(9)V99 COMP-3 VALUE +0.        
00459  01  WS-PR-UN-PREM           PIC S9(7)V99 COMP-3 VALUE +0.        
00460  01  WS-PR-UN-PREM-ALT       PIC S9(7)V99 COMP-3 VALUE +0.        
00461  01  WS-R78-UN-PREM          PIC S9(7)V99 COMP-3 VALUE +0.        
00462  01  WS-R78-UN-PREM-ALT      PIC S9(7)V99 COMP-3 VALUE +0.        
00463  01  WS-STATE-UN-PREM        PIC S9(7)V99 COMP-3 VALUE +0.        
00464  01  AH-EARN-METHOD          PIC X(01)           VALUE SPACE.     
00465 *                                                                 
00466 *                                                                 
00467 * LEAVE THE ABOVE 2 LINE IN TO PREVENT LOSING THE LINE ABOVE      
00480  EJECT                                                            
00481  01  ACTIVITY-TOTAL-TABLE.                                        
00482      12  CONTROL-LEVELS OCCURS 5 TIMES.                           
00483 *          **ON PASS FOR 1ST INSURANCE COMPANY                    
00484 *            1=  STATE TOTALS   - 1ST INSURANCE                   
00485 *            2=  ACCOUNT TOTALS - 1ST INSURANCE                   
00486 *            3=  STATE TOTALS   - REINSURANCE                     
00487 *            4=  ACCOUNT TOTALS - REINSURANCE                     
00488 *            5=  NOT USED                                         
00489 *          **ON PASS FOR REINSURANCE COMPANY                      
00490 *            1=  REINSURANCE COMPANY TOTALS                       
00491 *            2=  CARRIER TOTALS                                   
00492 *            3=  COMPANY TOTALS                                   
00493 *            4=  STATE TOTALS                                     
00494 *            5=  ACCOUNT TOTALS                                   
00495 *                                                                 
00496          16  TERM-GROUPS OCCURS 3 TIMES.                          
00497 *            1=  0-60 MONTHS                                      
00498 *            2=  61 - 120 MONTHS                                  
00499 *            3=  OVER 120 MONTHS                                  
00500 *                                                                 
00501              20  INSURANCE-TYPES OCCURS 10 TIMES.                 
00502 *                **  1-5 ARE GROUP                                
00503 *                    6-10 ARE INDIVIDUAL                          
00504 *                 1,6 =  LIFE(RED)                                
00505 *                 2,7 =  LIFE(LEV)                                
00506 *                 3,8 =  LIFE(OB)                                 
00507 *                 4,9 =  AH                                       
00508 *                 5,10=  AH(OB)                                   
00509                  24  ISSUE-CNT   PIC S9(13)    COMP-3.            
00510                  24  ISSUE-AMT   PIC S9(11)V99 COMP-3.            
00511                  24  ISSUE-PREM  PIC S9(11)V99 COMP-3.            
00512                  24  CANCEL-CNT  PIC S9(13)    COMP-3.            
00513                  24  CANCEL-AMT  PIC S9(11)V99 COMP-3.            
00514                  24  CANCEL-PREM PIC S9(11)V99 COMP-3.            
00515                  24  CLAIM-CNT   PIC S9(13)    COMP-3.            
00516                  24  CLAIM-AMT   PIC S9(11)V99 COMP-3.            
00517                  24  EARN-PREM   PIC S9(11)V99 COMP-3.            
00518  EJECT                                                            
00519  01  EXHIBIT-TOTALS-TABLE.                                        
00520      12  E-CONTROL-LEVELS OCCURS 5 TIMES.                         
00521 *            * DEFINITION OF LEVELS SAME AS ACTIVITY-TOTAL-TABLE  
00522          16  E-TERM-GROUPS OCCURS 3 TIMES.                        
00523 *            * DEFINITION OF LEVELS SAME AS ACTIVITY-TOTAL-TABLE  
00524              20  EXHIBIT-GROUPS OCCURS 9 TIMES.                   
00525 *                1=  INFORCE END OF PREVIOUS PERIOD               
00526 *                2=  ISSUED DURING PERIOD                         
00527 *                3=  TOTAL INFORCE (LEV 1 + LEV 2)                
00528 *                4=  DEATHS IN PERIOD                             
00529 *                5=  EXPIRED                                      
00530 *                6=  CANCELLED                                    
00531 *                7=  DECREASES                                    
00532 *                8=  TOTAL DEDUCTIONS (LEV 4+5+6+7)               
00533 *                9=  TOTAL INFORCE AT END OF PERIOD (LEV 3-8)     
00534 *                                                                 
00535                  24  GROUP-CNT   PIC S9(13)    COMP-3.            
00536                  24  GROUP-AMT   PIC S9(11)V99 COMP-3.            
00537                  24  IND-CNT     PIC S9(13)    COMP-3.            
00538                  24  IND-AMT     PIC S9(11)V99 COMP-3.            
00539                                                                   
00540  01  AH-PAID-TABLE.                                               
00541      12  A-CONTROL-LEVELS OCCURS 5 TIMES.                         
00542 *            * DEFINITION OF LEVELS SAME AS ACTIVITY-TOTAL-TABLE. 
00543          16  AH-PAID-THIS-YR     PIC S9(11)V99 COMP-3.            
00544          16  AH-PAID-LAST-YR     PIC S9(11)V99 COMP-3.            
00545  EJECT                                                            
00546  01  TABLE-SUBSCRIPTS    COMP.                                    
00547 *                                                                 
00548 * CONTROL-LEVELS                                                  
00549      12  CL                  PIC S9(4).                           
00550 * TERM-GROUPS                                                     
00551      12  TG                  PIC S9(4).                           
00552 * DETAIL (EXHIBIT GROUPS)                                         
00553      12  EG                  PIC S9(4).                           
00554 * DETAIL (INSURANCE TYPES)                                        
00555      12  IY                  PIC S9(4).                           
00556 * CONTROL-LEVELS + 1 (USED TO ROLL TOTALS)                        
00557      12  CLX                 PIC S9(4).                           
00558 * LAST-ADD-LEVEL.                                                 
00559      12  LA                  PIC S9(4).                           
00560                                                                   
00561      12  SAV-LF-TERM-CODE    PIC S9(4).                           
00562                                                                   
00563  01  DATE-EDIT-AREAS.                                             
00564      12  FORMAT-DATE.                                             
00565          16  XMM             PIC 99.                              
00566          16  FILLER          PIC X               VALUE '/'.       
00567          16  XDD             PIC 99.                              
00568          16  FILLER          PIC X               VALUE '/'.       
00569          16  XYY             PIC 99.                              
00570      12  EDITED-DATE REDEFINES                                    
00571          FORMAT-DATE         PIC X(8).                            
00572      12  RAW-DATE.                                                
00573          16  XCC             PIC 99.                              
00574          16  XYY             PIC 99.                              
00575          16  XMM             PIC 99.                              
00576          16  XDD             PIC 99.                              
00577      12  DATE-WORK REDEFINES                                      
00578          RAW-DATE            PIC 9(8).                            
00579  EJECT                                                            
00580  01  HEAD-LINE-1.                                                 
00581      12  FILLER              PIC X(12)           VALUE            
00582              '  CARRIER - '.                                      
00583      12  HL-CARRIER          PIC X               VALUE SPACES.    
00584      12  FILLER              PIC X(34)           VALUE SPACES.    
00585      12  HL-CO-NAME          PIC X(30)           VALUE SPACES.    
00586      12  FILLER              PIC X(42)           VALUE SPACES.    
00587      12  FILLER              PIC X(8)            VALUE 'ECS150'.  
00588                                                                   
00589  01  HEAD-LINE-2.                                                 
00590      12  FILLER              PIC X(12)           VALUE            
00591              ' GROUPING - '.                                      
00592      12  HL-COMPANY          PIC X(6)            VALUE SPACES.    
00593      12  FILLER              PIC X(23)           VALUE SPACES.    
00594      12  FILLER              PIC X(49)           VALUE            
00595              'CERTIFICATE ACTIVITY - DETAIL AND SUMMARY TOTALS'.  
00596      12  FILLER              PIC X(29)           VALUE SPACES.    
00597      12  HL-DATE             PIC X(8).                            
00598                                                                   
00599  01  HEAD-LINE-3.                                                 
00600      12  FILLER              PIC X(12)           VALUE            
00601              '  STATE   - '.                                      
00602      12  HL-STATE            PIC X(20)           VALUE SPACES.    
00603      12  FILLER              PIC X(17)           VALUE SPACES.    
00604      12  FILLER              PIC X(11)           VALUE            
00605              'FOR PERIOD '.                                       
00606      12  HL-PER-START        PIC X(8).                            
00607      12  FILLER              PIC X(6)            VALUE ' THRU '.  
00608      12  HL-PER-END          PIC X(8).                            
00609      12  FILLER              PIC X(37)           VALUE SPACES.    
00610      12  FILLER              PIC X(5)            VALUE 'PAGE '.   
00611      12  HL-PAGENO           PIC ZZ,ZZ9.                          
00612                                                                   
00613  01  HEAD-LINE-4.                                                 
00614      12  FILLER              PIC X(12)           VALUE            
00615              '  ACCOUNT - '.                                      
00616      12  HL-ACCOUNT          PIC X(11)           VALUE SPACES.    
00617      12  FILLER              PIC X(109)          VALUE SPACES.    
00618                                                                   
00619  01  HEAD-LINE-5.                                                 
00620      12  FILLER              PIC X(25)           VALUE            
00621              '  REINSURANCE COMPANY - '.                          
00622      12  HL-REIN-CO          PIC X(6)            VALUE SPACES.    
00623      12  FILLER              PIC X(101)          VALUE SPACES.    
00624                                                                   
00625  01  HEAD-LINE-6.                                                 
00626      12  FILLER              PIC X(7)    VALUE '  *****'.         
00627      12  HL-MESSAGE          PIC X(21)   VALUE SPACES.            
00628      12  FILLER              PIC X(5)    VALUE '*****'.           
00629      12  FILLER              PIC X(99)   VALUE SPACES.            
00630                                                                   
00631  01  HEAD-LINE-7A.                                                
00632      12  FILLER              PIC X(44)           VALUE            
00633              '          CERTIFICATE     ISSUE    TERM     '.      
00634      12  FILLER              PIC X(44)           VALUE            
00635              '       PLAN       ORIGINAL        PREMIUM AM'.      
00636      12  FILLER              PIC X(44)           VALUE            
00637              'OUNTS                                       '.      
00638                                                                   
00639  01  HEAD-LINE-7B.                                                
00640      12  FILLER              PIC X(44)           VALUE            
00641              '            NUMBER        DATE     AH  LF   '.      
00642      12  FILLER              PIC X(44)           VALUE            
00643              'TYPE   CODE        AMOUNT          LIFE     '.      
00644      12  FILLER              PIC X(44)           VALUE            
00645              ' AH        ACTIVITY      AMOUNT             '.      
00646                                                                   
00647  01  HEAD-LINE-8A.                                                
00648      12  FILLER              PIC X(44)           VALUE            
00649              ' CLASSIFICATION         I S S U E D         '.      
00650      12  FILLER              PIC X(44)           VALUE            
00651              '                  C A N C E L L E D         '.      
00652      12  FILLER              PIC X(44)           VALUE            
00653              '          C L A I M S            E A R N E D'.      
00654                                                                   
00655  01  HEAD-LINE-8B.                                                
00656      12  FILLER              PIC X(44)           VALUE            
00657              '*************** ****************************'.      
00658      12  FILLER              PIC X(44)           VALUE            
00659              '********    ********************************'.      
00660      12  FILLER              PIC X(44)           VALUE            
00661              '****   *********************    ************'.      
00662                                                                   
00663  01  HEAD-LINE-8C.                                                
00664      12  FILLER              PIC X(44)           VALUE            
00665             ' TYPE    TERM   -COUNT- ----AMOUNT----  --PR'.       
00666      12  FILLER              PIC X(44)           VALUE            
00667             'EMIUM---    -COUNT- ----AMOUNT----  --PREMIU'.       
00668      12  FILLER              PIC X(44)           VALUE            
00669             'M---   -COUNT- ---AMOUNT----    ---AMOUNT---'.       
00670  EJECT                                                            
00671  01  DETAIL-LINE-1.                                               
00672      12  FILLER              PIC X(8)            VALUE SPACES.    
00673      12  DL-CERT-NO          PIC X(10).                           
00674      12  DL-CERT-SUFX        PIC X.                               
00675      12  FILLER              PIC X(5)            VALUE SPACES.    
00676      12  DL-ISSUE-DT         PIC X(8).                            
00677      12  FILLER              PIC XX              VALUE SPACES.    
00678      12  DL-AH-TERM          PIC ZZ9.                             
00679      12  FILLER              PIC X               VALUE SPACES.    
00680      12  DL-LF-TERM          PIC ZZ9.                             
00681      12  FILLER              PIC X(4)            VALUE SPACES.    
00682      12  DL-TYPE             PIC XXX.                             
00683      12  FILLER              PIC X(4)            VALUE SPACES.    
00684      12  DL-PLAN             PIC XXX.                             
00685      12  FILLER              PIC X(2)            VALUE SPACES.    
00686      12  DL-ORIG-AMT         PIC ZZZ,ZZZ,ZZ9.99.                  
00687      12  FILLER              PIC X(5)            VALUE SPACES.    
00688      12  DL-LF-PREM          PIC ZZ,ZZ9.99.                       
00689      12  DL-AH-PREM          PIC ZZ,ZZ9.99.                       
00690      12  FILLER              PIC X(7)            VALUE SPACES.    
00691      12  DL-ACTIVITY         PIC X(5).                            
00692      12  DL-RSRV-AMT         PIC ZZZ,ZZZ,ZZ9.99-.                 
00693      12  FILLER              PIC X(14)           VALUE SPACES.    
00694                                                                   
00695  01  DETAIL-LINE-2.                                               
00696      12  DL-DESCRIP          PIC X(16)           VALUE SPACES.    
00697      12  DL-ISS-CNT          PIC ZZZ,ZZ9.                         
00698      12  DL-ISS-AMT          PIC ZZZ,ZZZ,ZZ9.99-.                 
00699      12  DL-ISS-PRM          PIC ZZ,ZZZ,ZZ9.99-.                  
00700      12  FILLER              PIC XX              VALUE SPACES.    
00701      12  DL-CAN-CNT          PIC ZZZ,ZZ9.                         
00702      12  FILLER              PIC X               VALUE SPACES.    
00703      12  DL-CAN-AMT          PIC ZZZ,ZZZ,ZZ9.99.                  
00704      12  FILLER              PIC X               VALUE SPACES.    
00705      12  DL-CAN-PRM          PIC ZZ,ZZZ,ZZ9.99.                   
00706      12  FILLER              PIC XXX             VALUE SPACES.    
00707      12  DL-CLM-CNT          PIC ZZZ,ZZ9.                         
00708      12  FILLER              PIC X               VALUE SPACES.    
00709      12  DL-CLM-AMT          PIC ZZ,ZZZ,ZZ9.99.                   
00710      12  FILLER              PIC XX              VALUE SPACES.    
00711      12  DL-EARN-AMT         PIC ZZ,ZZZ,ZZZ.99-.                  
00712  EJECT                                                            
00713  01  ABEND-CD.                                                    
00714      12  ABEND-CODE.                                              
00715          16  ABEND-FILE-ID       PIC XX VALUE ZEROS.              
00716          16  ABEND-CODE-1 REDEFINES ABEND-FILE-ID                 
00717                                  PIC XX.                          
00718          16  ABEND-REASON        PIC XX VALUE ZEROS.              
00719          16  ABEND-CODE-2 REDEFINES ABEND-REASON                  
00720                                  PIC XX.                          
00721      12  ABEND-OPTION    PIC X          VALUE 'Y'.                
00722      12  PGM-SUB         PIC S999 COMP  VALUE +150.               
00723      12  R-EX-DT             PIC X(6).                            
00724                                                                   
00725  01  KB-REIN.                                                     
00726      12  CLM-INCUR-REIN-DATE PIC 9(6).                            
00727      12  CLM-INCUR-REIN-DATE-R REDEFINES CLM-INCUR-REIN-DATE.     
00728          16  CIRD-CCYY.                                           
00729              20  CIRD-CC     PIC 99.                              
00730              20  CIRD-YR     PIC 99.                              
00731          16  CIRD-MO         PIC 99.                              
00732      12  DTO-CC              PIC 99.                              
00733      12  DTO-YR              PIC 99.                              
00734      12  DTO-MO              PIC 99.                              
00735      12  FLA-CLM-REIN-BASE   PIC S9(5)V99.                        
00736      12  ADJ-FLA-REIN-BASE   PIC S9(5)V99.                        
00737      12  REIN-RT-SW          PIC X               VALUE 'X'.       
00738  EJECT                                                            
00739                              COPY ECSRITAB.
00740                              COPY ELCPSEVR.                       
00741  EJECT                                                            
00742  01  CLAS150-EXTRACT-RECORD.                                      
00743      12  ER-SORT-KEY.                                             
00744          16  ER-CONTROL.                                          
00745              20  ER-REINS-CO             PIC X(06).               
00746              20  ER-CARRIER              PIC X(01).               
00747              20  ER-COMPANY              PIC X(06).               
00748              20  ER-STATE                PIC X(02).               
00749              20  ER-ACCOUNT              PIC X(10).               
00750          16  ER-ST-SEQ                   PIC X(02).               
00751          16  ER-REC-TYPE                 PIC 99.                  
00752      12  ER-PERIOD-START                 PIC 9(11)  COMP-3.       
00753      12  ER-PERIOD-END                   PIC 9(11)  COMP-3.       
00754      12  ER-ACTIVITY-TOTALS.                                      
00755          16  ER-TERM-GROUP OCCURS 3 TIMES.                        
00756 *                1= 0-60 MONTHS                                   
00757 *                2= 61-120 MONTHS                                 
00758 *                3= 121 AND OVER                                  
00759              20  ER-TYPE-GROUPS OCCURS 10 TIMES.                  
00760 *                1= LIFE(R-GP)       6= LIFE(R-IND)               
00761 *                2= LIFE(L-GP)       7= LIFE(L-IND)               
00762 *                3= LIFE(OB-GP)      8= LIFE(OB-IND)              
00763 *                4= AH(GP)           9= AH(IND)                   
00764 *                5= AH(OB-GP)       10= AH(OB-IND)                
00765                  24  ER-DETAIL.                                   
00766                      28  ER-ISSUE-CNT    PIC S9(13)    COMP-3.    
00767                      28  ER-ISSUE-AMT    PIC S9(11)V99 COMP-3.    
00768                      28  ER-ISSUE-PREM   PIC S9(11)V99 COMP-3.    
00769                      28  ER-CANCEL-CNT   PIC S9(13)    COMP-3.    
00770                      28  ER-CANCEL-AMT   PIC S9(11)V99 COMP-3.    
00771                      28  ER-CANCEL-PREM  PIC S9(11)V99 COMP-3.    
00772                      28  ER-CLAIM-CNT    PIC S9(13)    COMP-3.    
00773                      28  ER-CLAIM-AMT    PIC S9(11)V99 COMP-3.    
00774                      28  ER-EARN-PREM    PIC S9(11)V99 COMP-3.    
00775      12  ER-EXHIBIT-TOTALS.                                       
00776          16  ER-TERM-GROUP-E OCCURS 3 TIMES.                      
00777 *                1= 0-60 MONTHS                                   
00778 *                2= 61-120 MONTHS                                 
00779 *                3= 121 AND OVER                                  
00780              20  ER-LINE-DETAIL OCCURS 9 TIMES.                   
00781 *                1= INFORCE PREVIOUS PER  6= CANCELED IN PERIOD   
00782 *                2= ISSUED IN PERIOD      7= DECREASES IN PERIOD  
00783 *                3= TOTAL INFORCE-START   8= TOTAL DEC IN PERIOD  
00784 *                4= DEATHS IN PERIOD      9= TOTAL INFORCE AT END 
00785 *                5= EXPIRED IN PERIOD                             
00786                  24  ER-DETAIL-E.                                 
00787                      28  ER-GROUP-CNT    PIC S9(13)    COMP-3.    
00788                      28  ER-GROUP-AMT    PIC S9(11)V99 COMP-3.    
00789                      28  ER-IND-CNT      PIC S9(13)    COMP-3.    
00790                      28  ER-IND-AMT      PIC S9(11)V99 COMP-3.    
00791                                                                   
00792      12  ER-AH-PD-THIS                   PIC S9(11)V99 COMP-3.    
00793      12  ER-AH-PD-LAST                   PIC S9(11)V99 COMP-3.    
00794  EJECT                                                            
00795                              COPY ECSCRT01.                       
00796  EJECT                                                            
00797                              COPY ELCDATE.                        
00798  EJECT                                                            
00799                              COPY ELCCALC.                        
00800  EJECT                                                            
00801                              COPY ELCDTECX.                       
00802  EJECT                                                            
00803                              COPY ELCDTEVR.                       
00804  EJECT                                                            
00805                              COPY ELCCRTVR.                       
00806  EJECT                                                            
00807                              COPY ELCACCTV.                       
00808  EJECT                                                            
00809                              COPY ELCREINV.                       
00810  EJECT                                                            
00811  PROCEDURE DIVISION.                                              
00812 ******************************************************************
00813 *                                                                *
00814 *                *** SECTION DESCRIPTIONS ***                    *
00815 *  * SECTION NO. *                                               *
00816 *                                                                *
00817 *      1000      CLAS DATE LOAD, DATE EDITS, WORK AREA INIT      *
00818 *                                                                *
00819 *      2000      PERFORM LOOP FOR EXTRACT-A                      *
00820 *                                                                *
00821 *      3000      PERFORM LOOP FOR EXTRACT-B                      *
00822 *                                                                *
00823 *      4000      COMPUTATIONS FOR-RESERVES, CLAIMS, ISSUES,      *
00824 *                                 CANCELS, INCREASES/DECREASES   *
00825 *                                                                *
00826 *      5000      PRINT ROUTINES                                  *
00827 *                                                                *
00828 *      6000      REINSURANCE CALCULATIONS                        *
00829 *                                                                *
00830 *      7000      LEVEL BREAK PROCESSING                          *
00831 *                                                                *
00832 *      8000      MISCELANEOUS ROUTINES                           *
00833 *                                                                *
00834 *      9990      PROGRAM EXIT                                    *
00835 ******************************************************************
00836  EJECT                                                            
00837  0100-ININITIALIZATION       SECTION 49.                          
00838                                                                   
00839  0110-CLAS-DATA-LOAD.                                             
00840                              COPY ELCDTERX.                       
00841                                                                   
00842  0120-PERIOD-EDIT.                                                
00843 **   MOVE 1 TO DTE-PGM-OPT.                                       
00844      IF EP-CCYY GREATER THAN RUN-CCYY OR                          
00845         EP-MO GREATER THAN RUN-MO                                 
00846          GO TO 0130-CLEAR-WORK.                                   
00847                                                                   
00848      IF EP-DT = RUN-DATE                                          
00849          SUBTRACT +1 FROM RUN-CCYY                                
00850          MOVE WS-RUN-DATE-N TO RUN-DATE                           
00851          GO TO 0130-CLEAR-WORK.                                   
00852                                                                   
00853      DISPLAY 'PERIOD START IS AFTER PERIOD END-CHECK CLAS CARD'.  
00854      MOVE '0998'                 TO ABEND-CODE.                   
00855      GO TO ABEND-PGM.                                             
00856                                                                   
00857  0130-CLEAR-WORK.                                                 
00858      MOVE HIGH-VALUES            TO REIN-HOLD-AREAS.              
00859      MOVE SPACES                 TO REIN-LEVELS-END.              
00860      PERFORM 8100-CLEAR-TABLES THRU 8100-EXIT                     
00861          VARYING CL FROM +1 BY +1                                 
00862              UNTIL CL GREATER THAN +5.                            
00863                                                                   
00864  0140-OPEN-FILES.                                                 
00865      OPEN INPUT ERACCTT  CERT-FILE                                
00866           OUTPUT PRINT-FILE  CLAS150-A-EXTRACT.                   
00867      MOVE LOW-VALUES             TO ACCOUNT-MASTER.               
00868                                                                   
00869      COPY ELCACCTI.                                               
00870                                                                   
00871      IF ACCT-FILE-STATUS NOT = '00' AND '97'                      
00872          MOVE ACCT-FILE-STATUS   TO WS-ABEND-FILE-STATUS          
00873          MOVE ' ERROR ACCOUNT MASTER OPEN '                       
00874                                  TO WS-ABEND-MESSAGE              
00875          PERFORM ABEND-PGM.                                       
00876                                                                   
00877      MOVE 'R'                        TO  CP-RATE-FILE.            
00878      MOVE 'O'                        TO  CP-IO-FUNCTION.          
00879      PERFORM 8455-CALL-RATING-ROUTINE THRU 8499-EXIT.             
00880      IF IO-ERROR                                                  
00881          MOVE 0302                   TO  WS-RETURN-CODE           
00882          MOVE 'ERROR OCCURED OPENING - ELRATE'                    
00883                                      TO  WS-ABEND-MESSAGE         
00884          GO TO ABEND-PGM.                                         
00885                                                                   
00886  0150-SET-DATES.                                                  
00887                                                                   
00888      MOVE RUN-DATE                   TO DC-GREG-DATE-CYMD.        
00889      MOVE 'L'                        TO DC-OPTION-CODE.           
00890      PERFORM 8500-DATE-CONVERT-ROUTINE THRU 8599-DATE-CONVERT-X.  
00891      MOVE DC-BIN-DATE-1              TO RUN-DATE-BIN PERIOD-START.
00892                                                                   
00893      MOVE EP-DT                      TO DC-GREG-DATE-CYMD.        
00894      MOVE 'L'                        TO DC-OPTION-CODE.           
00895      PERFORM 8500-DATE-CONVERT-ROUTINE THRU 8599-DATE-CONVERT-X.  
00896      MOVE DC-BIN-DATE-1              TO PERIOD-END  EP-DT-BIN.    
00897                                                                   
00898      COMPUTE PERIOD-START-9 = (RUN-CCYY * 12) + RUN-MO.           
00899      COMPUTE PERIOD-END-9 = (EP-CCYY * 12) + EP-MO.               
00900                                                                   
00901                                                                   
00902                                                                   
00903      MOVE COMPANY-NAME           TO HL-CO-NAME.                   
00904      MOVE WS-CURRENT-DATE        TO HL-DATE.                      
00905      MOVE RUN-DATE               TO DATE-WORK.                    
00906      MOVE CORRESPONDING RAW-DATE TO FORMAT-DATE.                  
00907      MOVE FORMAT-DATE            TO HL-PER-START.                 
00908      MOVE EP-DT                  TO DATE-WORK.                    
00909      MOVE CORRESPONDING RAW-DATE TO FORMAT-DATE.                  
00910      MOVE FORMAT-DATE            TO HL-PER-END.                   
00911                                                                   
00912  EJECT                                                            
00913  1000-MAINLINE-CONTROL       SECTION 05.                          
00914                                                                   
00915  1100-CREATE-EXTRACT-A.                                           
00916                                                                   
00917      PERFORM 2000-EXTRACT-A THRU 2000-EXIT                        
00918          UNTIL CR-ACCT-CONTROL = HIGH-VALUES.                     
00919      CLOSE ERACCTT CERT-FILE  CLAS150-A-EXTRACT.                  
00920                                                                   
00921      IF REINS-FILE-OPEN                                           
00922          CLOSE REINS-EXTRACT-1.                                   
00923                                                                   
00924      IF REIN-OPEN-SW NOT = SPACE                                  
00925          MOVE SPACE              TO REIN-OPEN-SW                  
00926          CLOSE ERRTBL-IN
00927          IF ERRTBL-FILE-STATUS NOT = '00'                         
00928              MOVE '22'           TO ABEND-FILE-ID                 
00929              MOVE ERRTBL-FILE-STATUS                              
00930                                  TO ABEND-REASON                  
00931              GO TO ABEND-PGM.                                     
00932                                                                   
00933      IF NO-REINSURANCE                                            
00934          GO TO 9990-PROGRAM-END.                                  
00935                                                                   
00936  1200-CREATE-EXTRACT-BC.                                          
00937      SORT SORT-WORK  ASCENDING KEY SORT-KEY                       
00938          USING REINS-EXTRACT-1                                    
00939              GIVING REINS-EXTRACT-2.                              
00940  S1200-EXIT.                                                      
00941      EXIT.                                                        
00942                                                                   
00943  1200-CONTINUE.                                                   
00944      OPEN INPUT  REINS-EXTRACT-2                                  
00945           OUTPUT CLAS150-B-EXTRACT  CLAS150-C-EXTRACT.            
00946      MOVE SPACES                 TO SAVE-CONTROL-BREAKS.          
00947      MOVE LOW-VALUES             TO CERTIFICATE-RECORD.           
00948      COPY ELCCRTM1.                                               
00949      MOVE +1                     TO PASS-TYPE.                    
00950      PERFORM 8100-CLEAR-TABLES THRU 8100-EXIT                     
00951          VARYING CL FROM +1 BY +1                                 
00952              UNTIL CL GREATER +5.                                 
00953      PERFORM 3000-EXTRACT-BC THRU 3000-EXIT                       
00954          UNTIL RC-CONTROL = HIGH-VALUES.                          
00955      CLOSE CLAS150-B-EXTRACT  CLAS150-C-EXTRACT  REINS-EXTRACT-2. 
00956      GO TO 9990-PROGRAM-END.                                      
00957  EJECT                                                            
00958                                                                   
00959  2000-EXTRACT-A          SECTION 10.                              
00960  2100-READ-CERT-FILE.                                             
00961      READ CERT-FILE INTO CERTIFICATE-RECORD AT END                
00962          MOVE HIGH-VALUES        TO CR-ACCT-CONTROL.              
00963                                                                   
00964      COPY ELCCRTM1.                                               
00965                                                                   
00966      IF CR-AH-CANC-DT NOT = ZEROS                                 
00967         IF CR-AH-CANCEL-EXIT-DATE = ZEROS                         
00968             MOVE CR-AH-CANC-DT TO CR-AH-CANCEL-EXIT-DATE          
00969                                   WS-CR-AH-CANCEL-EXIT-DATE.      
00970                                                                   
00971      IF CR-LF-CANC-DT NOT = ZEROS                                 
00972         IF CR-LF-CANCEL-EXIT-DATE = ZEROS                         
00973             MOVE CR-LF-CANC-DT TO CR-LF-CANCEL-EXIT-DATE          
00974                                   WS-CR-LF-CANCEL-EXIT-DATE.      
00975                                                                   
00976      IF CR-ACCT-CONTROL = HIGH-VALUES                             
00977          GO TO 2210-CONTINUE.                                     
00978                                                                   
00979      IF CR-POLICY-IS-DECLINED OR                                  
00980         CR-POLICY-IS-VOID                                         
00981             DISPLAY 'CERT BYPASSED - VOID OR DECLINED '           
00982                                                 CR-FULL-CONTROL   
00983             GO TO 2100-READ-CERT-FILE.                            
00984                                                                   
PEMMOD     IF (cr-carrier = '9')
PEMMOD        and (cr-state = 'NE')
PEMMOD        and (cr-account = '0000015860')
PEMMOD        and (cr-cert-no = '1098173790 ')
PEMMOD            DISPLAY 'CERT BYPASSED - the big one '
PEMMOD                                                CR-FULL-CONTROL
PEMMOD            GO TO 2100-READ-CERT-FILE.
00984
00985      IF CR-LFTYP NOT = '00'                                       
00986          IF CR-LF-TERM = 0                                        
00987              IF CR-AHTYP = '00'                                   
00988                  DISPLAY 'CERT BYPASSED DUE TO INVALID TERM '     
00989                                                  CR-FULL-CONTROL  
00990                  GO TO 2100-READ-CERT-FILE.                       
00991                                                                   
00992      IF CR-AHTYP NOT EQUAL '00'                                   
00993          IF CR-AH-TERM = 0                                        
00994              IF CR-LFTYP = '00'                                   
00995                  DISPLAY 'CERT BYPASSED DUE TO INVALID TERM '     
00996                                                  CR-FULL-CONTROL  
00997                  GO TO 2100-READ-CERT-FILE.                       
00998                                                                   
00999  2210-CONTINUE.                                                   
01000                                                                   
01001      IF CR-LFTYP NOT EQUAL '00'                                   
01002          IF CR-LF-TERM = 0                                        
01003              DISPLAY 'INVALID TERM ON LIFE CVG ' CR-FULL-CONTROL  
01004              MOVE '00'             TO  CR-LFTYP.                  
01005                                                                   
01006      IF CR-AHTYP NOT EQUAL '00'                                   
01007          IF CR-AH-TERM = 0                                        
01008              DISPLAY 'INVALID TERM ON AH CVG ' CR-FULL-CONTROL    
01009              MOVE '00'             TO  CR-AHTYP.                  
01010                                                                   
01011      IF SAVE-CONTROL-BREAKS = SPACES                              
01012          MOVE CR-CARRIER         TO LAST-CARRIER                  
01013          MOVE CR-GROUPING        TO LAST-GROUPING                 
01014          MOVE CR-STATE           TO LAST-STATE                    
01015          MOVE CR-ACCOUNT         TO LAST-ACCOUNT.                 
01016                                                                   
01017  2200-MATCH-ACCT-TO-CERT.                                         
01018                                                                   
01019      IF CR-ACCT-CONTROL EQUAL HIGH-VALUES                         
01020          GO TO 2300-LEVEL-BREAK-CHECK.                            
01021                                                                   
01022      IF AM-CONTROL-A LESS THAN CR-ACCT-CONTROL                    
01023          PERFORM 2210-ACCT-MAST-READ THRU 2210-EXIT               
01024          GO TO 2200-MATCH-ACCT-TO-CERT.                           
01025                                                                   
01026      IF AM-CONTROL-A GREATER THAN CR-ACCT-CONTROL                 
01027          GO TO 2205-ACCT-CERT-MISMATCH.                           
01028                                                                   
01029      IF CR-DT  NOT LESS THAN AM-EXPIRE-DT                         
01030          PERFORM 2210-ACCT-MAST-READ THRU 2210-EXIT               
01031          GO TO 2200-MATCH-ACCT-TO-CERT.                           
01032                                                                   
01033      IF CR-DT NOT LESS THAN AM-EFFECT-DT                          
01034          GO TO 2300-LEVEL-BREAK-CHECK.                            
01035                                                                   
01036  2205-ACCT-CERT-MISMATCH.                                         
01037      DISPLAY 'NO MATCHING ACCOUNT MASTER FOR CONTROL '            
01038               CR-ACCT-CONTROL.                                    
01039                                                                   
01040      IF DTE-CLIENT EQUAL 'FIA'                                    
CIDMOD                      OR 'CID'
01041         GO TO 2100-READ-CERT-FILE                                 
01042      ELSE                                                         
01043         MOVE '0302'              TO ABEND-CODE                    
01044         GO TO ABEND-PGM.                                          
01045                                                                   
01046  2210-ACCT-MAST-READ.                                             
01047      READ ERACCTT AT END                                          
01048          MOVE HIGH-VALUES        TO ACCOUNT-MASTER.               
01049                                                                   
01050      COPY ELCACCTI.                                               
01051                                                                   
01052  2210-EXIT.                                                       
01053      EXIT.                                                        
01054                                                                   
01055  2300-LEVEL-BREAK-CHECK.                                          
01056                                                                   
01057      IF (LAST-CARRIER NOT = CR-CARRIER) OR                        
01058         (LAST-GROUPING NOT = CR-GROUPING)                         
01059          PERFORM 7400-STATE-BREAK THRU 7400-EXIT                  
01060          PERFORM 7500-ACCOUNT-BREAK THRU 7500-EXIT                
01061          MOVE CR-ACCT-CONTROL   TO SAVE-CNTRL-GRP                 
01062          GO TO 2400-VALIDATE-DATA.                                
01063                                                                   
01064      IF LAST-STATE NOT EQUAL CR-STATE                             
01065          PERFORM 7400-STATE-BREAK THRU 7400-EXIT                  
01066          MOVE CR-ACCT-CONTROL    TO SAVE-CNTRL-GRP                
01067          GO TO 2400-VALIDATE-DATA.                                
01068                                                                   
01069      IF LAST-ACCOUNT NOT EQUAL CR-ACCOUNT                         
01070          PERFORM 7500-ACCOUNT-BREAK THRU 7500-EXIT                
01071          MOVE CR-ACCT-CONTROL    TO SAVE-CNTRL-GRP.               
01072                                                                   
01073  2400-VALIDATE-DATA.                                              
01074      IF SAVE-CNTRL-GRP = HIGH-VALUES                              
01075          GO TO 2000-EXIT.                                         
01076                                                                   
PEMMOD*    IF AM-EARN-METHOD-A NOT EQUAL SPACES AND ZEROS               
PEMMOD*        MOVE AM-EARN-METHOD-A   TO AH-EARN-METHOD                
PEMMOD*    ELSE                                                         
PEMMOD     MOVE SPACES                 TO AH-EARN-METHOD                
01081                                                                   
01082      IF CR-IND-GRP NOT = '1' AND '2'                              
01083          MOVE AM-IG              TO CR-IND-GRP.                   
01084      IF CR-LFAMT-ALT NOT NUMERIC                                  
01085          MOVE ZEROS              TO CR-LFAMT-ALT.                 
01086      IF CR-LFPRM-ALT NOT NUMERIC                                  
01087          MOVE ZEROS              TO CR-LFPRM-ALT.                 
01088      IF CR-LFPRM-CALC NOT NUMERIC                                 
01089          MOVE ZEROES             TO CR-LFPRM-CALC.                
01090      IF CR-AHPRM-CALC NOT NUMERIC                                 
01091          MOVE ZEROES             TO CR-AHPRM-CALC.                
01092      IF CR-LFRFND-CALC NOT NUMERIC                                
01093          MOVE ZEROES             TO CR-LFRFND-CALC.               
01094      IF CR-AHRFND-CALC NOT NUMERIC                                
01095          MOVE ZEROES             TO CR-AHRFND-CALC.               
01096      IF CR-APR NOT NUMERIC                                        
01097          MOVE ZEROS              TO CR-APR.                       
01098      IF CR-AGE NOT NUMERIC                                        
01099          MOVE ZEROS              TO CR-AGE.                       
01100      IF CR-LIVES NOT NUMERIC                                      
01101          MOVE ZERO               TO CR-LIVES.                     
01102      IF CR-SUM-CAN-CNT-ITD NOT NUMERIC                            
01103          MOVE ZERO               TO CR-SUM-CAN-CNT-ITD.           
01104      IF CR-SUM-CAN-CNT-YTD NOT NUMERIC                            
01105          MOVE ZERO               TO CR-SUM-CAN-CNT-YTD.           
01106      IF CR-DTHEXP-YTD NOT NUMERIC                                 
01107          MOVE ZERO               TO CR-DTHEXP-YTD.                
01108      IF CR-DTHAMT-YTD NOT NUMERIC                                 
01109          MOVE ZERO               TO CR-DTHAMT-YTD.                
01110      IF CR-DISAMT-YTD NOT NUMERIC                                 
01111          MOVE ZERO               TO CR-DISAMT-YTD.                
01112      IF CR-DISEXP-YTD NOT NUMERIC                                 
01113          MOVE ZERO               TO CR-DISEXP-YTD.                
01114                                                                   
01115      ADD CR-DTHEXP-YTD           TO CR-DTHAMT-YTD.                
01116      ADD CR-DISEXP-YTD           TO CR-DISAMT-YTD.                
01117                                                                   
01118      MOVE CR-DIS-CC              TO CR-DIS-INC-CC (1).            
01119      MOVE CR-DIS-YR              TO CR-DIS-INC-YR (1).            
01120      MOVE CR-DIS-MO              TO CR-DIS-INC-MO (1).            
01121      MOVE CR-DIS-DA              TO CR-DIS-INC-DA (1).            
01122      MOVE WS-CR-DIS-INCUR-DT(1)  TO CR-DIS-INCUR-DT (1).          
01123      MOVE CR-DISAMT-YTD          TO CR-INCUR-DISAMT (1).          
01124                                                                   
01125      MOVE ZEROS                  TO CR-INCUR-DISEXP (1)           
01126                                     CR-DIS-INCUR-DT (2)           
01127                                     WS-CR-DIS-INCUR-DT(2)         
01128                                     CR-DIS-INCUR-DT (3)           
01129                                     WS-CR-DIS-INCUR-DT(3)         
01130                                     CR-DIS-INCUR-DT (4)           
01131                                     WS-CR-DIS-INCUR-DT(4)         
01132                                     CR-DIS-INCUR-DT (5)           
01133                                     WS-CR-DIS-INCUR-DT(5).        
01134                                                                   
01135  2500-CALC-1ST-INS.                                               
01136      MOVE 0                      TO RECORD-TYPE-SW.               
01137      PERFORM 4000-ACTIVITY-TOTALING THRU 4000-EXIT.               
01138                                                                   
01139  2600-CALC-REINSURANCE.                                           
01140      IF CR-REIN-TABLE = SPACES OR ZEROES                          
01141          GO TO 2000-EXIT.                                         
01142                                                                   
01143      IF REINS-FILE-CLOSED                                         
01144          OPEN OUTPUT REINS-EXTRACT-1                              
01145          MOVE 1                  TO REINS-FILE-SW.                
01146                                                                   
01147      PERFORM 6000-REINSURANCE-CALC THRU 6000-EXIT.                
01148      PERFORM 2700-REINS-EXTRACT THRU 2700-EXIT                    
01149          VARYING SUB1 FROM +1 BY +1                               
01150              UNTIL REIN-COMP (SUB1) = SPACES.                     
01151      GO TO 2000-EXIT.                                             
01152                                                                   
01153  2700-REINS-EXTRACT.                                              
01154      IF REIN-LF-AH-FLGS (SUB1) = SPACES                           
01155          GO TO 2700-EXIT.                                         
01156                                                                   
01157      IF REIN-REM-SW (SUB1) = 'Z'                                  
01158          GO TO 2700-EXIT.                                         
PEMMOD     perform varying sub3 from +1 by +1 until
              (sub3 > +1500) or
              (rein-comp (sub1) = rc2-rein-co (sub3))
           end-perform
           if sub3 > +1500
              go to 2700-exit
           else
              if rc2-ecs152-rpt (sub3) = 'Y'
                 continue
              else
                 go to 2700-exit
              end-if
           end-if
PEMMOD
01159                                                                   
01160      MOVE REIN-WORK-FLDS (SUB1)  TO RWF-FIELDS.                   
01161      MOVE SAVE-REINS-CERT        TO RC-REC.                       
01162      MOVE REIN-COMP (SUB1)       TO RC-COMP.                      
01163      MOVE RC-AHTYP               TO CR-AHTYP.                     
01164      MOVE RC-LFTYP               TO CR-LFTYP.                     
01165      MOVE RC-PMT-FREQ            TO CR-PMT-FREQ.                  
01166      MOVE RC-I-LIVES             TO CR-LIVES.                     
01167      MOVE RC-C-LIVES-ITD         TO CR-SUM-CAN-CNT-ITD.           
01168      MOVE RC-C-LIVES-YTD         TO CR-SUM-CAN-CNT-YTD.           
01169      MOVE RC-AHRFND-YTD          TO CR-AHRFND-CALC.               
01170      MOVE RC-LFRFND-YTD          TO CR-LFRFND-CALC.               
01171      MOVE RC-LF-EXPIRE-DT        TO CR-LF-EXPIRE-DATE             
01172                                     WS-CR-LF-EXPIRE-DATE-N.       
01173      MOVE RC-AH-EXPIRE-DT        TO CR-AH-EXPIRE-DATE             
01174                                     WS-CR-AH-EXPIRE-DATE-N.       
01175      MOVE ZEROES                 TO RC-LFAMT   CR-LFAMT           
01176                                     RC-LFPRM   CR-LFPRM           
pemmod                                    cr-lfprm-alt                  
01177                                     RC-AHAMT   CR-AHAMT           
01178                                     RC-AHPRM   CR-AHPRM           
01179                                     RC-LFRFND  CR-LFRFND          
01180                                     RC-AHRFND  CR-AHRFND          
01181                                     RC-DTHAMT  CR-DTHAMT-YTD      
01182                                     RC-DISAMT  CR-DISAMT-YTD      
01183                                     RC-CEDE-FACT.                 
01184                                                                   
01185      IF REIN-LF-FLG (SUB1) = 'X'                                  
01186          MOVE RWF-LFAMT          TO RC-LFAMT   CR-LFAMT           
01187          MOVE RWF-LFPRM          TO RC-LFPRM   CR-LFPRM           
01188          MOVE RWF-LFRFND         TO RC-LFRFND  CR-LFRFND          
01189          MOVE RWF-LFCLM          TO RC-DTHAMT  CR-DTHAMT-YTD      
01190      ELSE                                                         
01191          MOVE ZERO               TO RC-LFTYP  CR-LFTYP.           
01192                                                                   
01193      COMPUTE WS-TOTAL-FACE-AMT = RC-ORIG-LFAMT + RC-LFAMT-ALT.    
01194                                                                   
01195      IF (REIN-LF-FLG (SUB1) = 'X')  AND                           
01196         (CLAS-I-EP (CLAS-INDEXL) = 'B')                           
01197          COMPUTE RC-CEDE-FACT = CR-LFAMT / WS-TOTAL-FACE-AMT.     
01198                                                                   
01199      IF REIN-AH-FLG (SUB1) = 'X'                                  
01200          MOVE RWF-AHAMT          TO RC-AHAMT   CR-AHAMT           
01201          MOVE RWF-AHPRM          TO RC-AHPRM   CR-AHPRM           
01202          MOVE RWF-AHRFND         TO RC-AHRFND  CR-AHRFND          
01203          MOVE RWF-AHCLM          TO RC-DISAMT  CR-DISAMT-YTD      
01204      ELSE                                                         
01205          MOVE ZERO               TO RC-AHTYP  CR-AHTYP.           
01206                                                                   
01207      ADD +1 TO REINS-REC-CNT.                                     
01208      MOVE 1                      TO REINSURANCE-FOUND.            
PEMMOD     WRITE RC-REC-1 FROM RC-REC.
01210      .
01211  2710-EVALUATE-REINS.                                             
01212      MOVE 1                      TO RECORD-TYPE-SW.               
01213      PERFORM 4000-ACTIVITY-TOTALING THRU 4000-EXIT.               
01214                                                                   
01215  2700-EXIT.                                                       
01216      EXIT.                                                        
01217                                                                   
01218  2000-EXIT.                                                       
01219      EXIT.                                                        
01220  EJECT                                                            
01221  3000-EXTRACT-BC     SECTION 15.                                  
01222                                                                   
01223  3100-READ-REINS-EXTRACT.                                         
01224      READ REINS-EXTRACT-2 INTO RC-REC AT END                      
01225          MOVE HIGH-VALUES TO RC-CONTROL                           
01226          GO TO 2150-CONTROL-BREAKS.                               
01227      MOVE SPACES                 TO CERTIFICATE-RECORD.           
01228      MOVE RC-CMCTL               TO CR-FULL-CONTROL.              
01229      MOVE RC-ENTRY-STATUS        TO CR-ENTRY-STATUS.              
01230      MOVE RC-LFTYP               TO CR-LFTYP.                     
01231      MOVE RC-LF-TERM             TO CR-LF-TERM.                   
01232      MOVE RC-LF-TERM-IN-DAYS     TO CR-LF-TERM-IN-DAYS.           
01233      MOVE RC-LF-CANCEL-STATUS    TO CR-LF-STATUS-AT-CANCEL.       
01234      MOVE RC-LF-CLAIM-STATUS     TO CR-LF-STATUS-AT-DEATH.        
01235      MOVE RC-LF-CURRENT-STATUS   TO CR-LF-CURRENT-STATUS.         
01236      MOVE RC-AHTYP               TO CR-AHTYP.                     
01237      MOVE RC-AH-TERM             TO CR-AH-TERM.                   
01238      MOVE RC-AH-CANCEL-STATUS    TO CR-AH-STATUS-AT-CANCEL.       
01239      MOVE RC-AH-CLAIM-STATUS     TO CR-AH-STATUS-AT-SETTLEMENT.   
01240      MOVE RC-AH-CURRENT-STATUS   TO CR-AH-CURRENT-STATUS.         
01241      MOVE RC-AHAMT               TO CR-AHAMT.                     
01242      MOVE RC-LFAMT               TO CR-LFAMT.                     
01243      MOVE RC-LFAMT-ALT           TO CR-LFAMT-ALT.                 
01244      MOVE RC-LFPRM               TO CR-LFPRM.                     
01245      MOVE RC-AHPRM               TO CR-AHPRM.                     
01246      MOVE RC-LFRFND              TO CR-LFRFND.                    
01247      MOVE RC-AHRFND              TO CR-AHRFND.                    
01248      MOVE RC-DTHAMT              TO CR-DTHAMT-YTD.                
01249      MOVE RC-DISAMT              TO CR-DISAMT-YTD.                
01250      MOVE RC-IND-GRP             TO CR-IND-GRP.                   
01251      MOVE RC-LF-CANCEL-STATUS    TO CR-LF-STATUS-AT-CANCEL.       
01252      MOVE RC-LF-CLAIM-STATUS     TO CR-LF-STATUS-AT-DEATH.        
01253      MOVE RC-AH-CANCEL-STATUS    TO CR-AH-STATUS-AT-CANCEL.       
01254      MOVE RC-AH-CLAIM-STATUS     TO CR-AH-STATUS-AT-SETTLEMENT.   
01255      MOVE RC-LF-CANC-DT          TO CR-LF-CANC-DT.                
01256      MOVE RC-AH-CANC-DT          TO CR-AH-CANC-DT.                
01257      MOVE RC-DIS-DT              TO CR-DIS-DT.                    
01258      MOVE RC-ENT-DT              TO CR-ENTRY-DATE.                
01259      MOVE RC-LF-CANC-EXIT        TO CR-LF-CANCEL-EXIT-DATE.       
01260      MOVE RC-AH-CANC-EXIT        TO CR-AH-CANCEL-EXIT-DATE.       
01261      MOVE RC-LF-CLAIM-EXIT       TO CR-LF-CLAIM-EXIT-DATE.        
01262      MOVE RC-AH-CLAIM-EXIT       TO CR-AH-SETTLEMENT-EXIT-DATE.   
01263      MOVE RC-PMT-FREQ            TO CR-PMT-FREQ.                  
01264      MOVE RC-APR                 TO CR-APR.                       
01265      MOVE RC-LOAN-TERM           TO CR-LOAN-TERM.                 
01266      MOVE RC-I-LIVES             TO CR-LIVES.                     
01267      MOVE RC-C-LIVES-ITD         TO CR-SUM-CAN-CNT-ITD.           
01268      MOVE RC-C-LIVES-YTD         TO CR-SUM-CAN-CNT-YTD.           
01269      MOVE RC-AHRFND-YTD          TO CR-AHRFND-CALC.               
01270      MOVE RC-LFRFND-YTD          TO CR-LFRFND-CALC.               
01271      MOVE RC-LF-EXPIRE-DT        TO CR-LF-EXPIRE-DATE.            
01272      MOVE RC-AH-EXPIRE-DT        TO CR-AH-EXPIRE-DATE.            
01273      MOVE RC-LOAN-1ST-PMT-DT     TO CR-LOAN-1ST-PMT-DT.           
01274                                                                   
01275      COPY ELCCRTM1.                                               
01276                                                                   
01277  2150-CONTROL-BREAKS.                                             
01278      IF SAVE-CONTROL-BREAKS = SPACES                              
01279          MOVE CR-ACCT-CONTROL    TO SAVE-CNTRL-GRP                
01280          MOVE RC-COMP            TO LAST-REINS-CO.                
01281                                                                   
01282      IF LAST-REINS-CO NOT = RC-COMP                               
01283          PERFORM 7100-REINS-CO-BREAK THRU 7100-EXIT               
01284          GO TO 3200-CALC-FOR-REINSURANCE.                         
01285                                                                   
01286      IF LAST-CARRIER NOT = CR-CARRIER                             
01287          PERFORM 7200-CARRIER-BREAK THRU 7200-EXIT                
01288          GO TO 3200-CALC-FOR-REINSURANCE.                         
01289                                                                   
01290      IF LAST-GROUPING NOT = CR-GROUPING                           
01291          PERFORM 7300-COMPANY-BREAK THRU 7300-EXIT                
01292          GO TO 3200-CALC-FOR-REINSURANCE.                         
01293                                                                   
01294      IF LAST-STATE NOT = CR-STATE                                 
01295          PERFORM 7400-STATE-BREAK THRU 7400-EXIT                  
01296          GO TO 3200-CALC-FOR-REINSURANCE.                         
01297                                                                   
01298      IF LAST-ACCOUNT NOT = CR-ACCOUNT                             
01299          PERFORM 7500-ACCOUNT-BREAK THRU 7500-EXIT.               
01300                                                                   
01301  3200-CALC-FOR-REINSURANCE.                                       
01302      IF RC-CONTROL = HIGH-VALUES                                  
01303          GO TO 3000-EXIT.                                         
01304                                                                   
01305      MOVE CR-ACCT-CONTROL        TO SAVE-CNTRL-GRP.               
01306      MOVE RC-COMP                TO LAST-REINS-CO.                
01307      PERFORM 4000-ACTIVITY-TOTALING THRU 4000-EXIT.               
01308                                                                   
01309  3000-EXIT.                                                       
01310      EXIT.                                                        
01311  EJECT                                                            
01312  4000-ACTIVITY-TOTALING  SECTION 25.                              
01313                                                                   
01314  4100-ELIM-FUTURES.                                               
01315      MOVE  CR-ENTRY-DATE     TO  DC-GREG-DATE-CYMD.               
01316      MOVE 'L'                TO  DC-OPTION-CODE.                  
01317      PERFORM 8500-DATE-CONVERT-ROUTINE THRU 8599-DATE-CONVERT-X.  
01318      IF NO-CONVERSION-ERROR                                       
01319          MOVE DC-BIN-DATE-1  TO  CERT-ENTRY-DATE                  
01320      ELSE                                                         
01321          MOVE CR-DT          TO  DC-GREG-DATE-CYMD                
01322          MOVE 'L'            TO  DC-OPTION-CODE                   
01323          PERFORM 8500-DATE-CONVERT-ROUTINE THRU                   
01324                                       8599-DATE-CONVERT-X         
01325          IF NO-CONVERSION-ERROR                                   
01326              MOVE DC-BIN-DATE-1  TO  CERT-ENTRY-DATE.             
01327                                                                   
01328      IF CERT-ENTRY-DATE GREATER THAN PERIOD-END                   
01329          GO TO 4000-EXIT.                                         
01330                                                                   
01331      IF CR-ENTRY-STATUS = '9'                                     
01332          IF REINSURANCE-PASS OR REINSURANCE-REC                   
01333              NEXT SENTENCE                                        
01334          ELSE                                                     
01335              GO TO 4000-EXIT.                                     
01336                                                                   
01337  4110-FIND-CLAS-L-TYPE.                                           
01338      IF CR-LFTYP = ZERO                                           
01339          GO TO 4120-FIND-CLAS-A-TYPE.                             
01340                                                                   
01341      MOVE CR-LFTYP               TO CLAS-LOOK.                    
01342      MOVE CLAS-STARTL            TO CLAS-INDEXL.                  
01343                                                                   
01344  4115-LIFE-LOOKUP.                                                
01345      IF CLAS-INDEXL IS GREATER THAN CLAS-MAXL                     
01346          DISPLAY 'INVALID LIFE TYPE' CLAS-LOOK                    
01347          MOVE '0401'             TO ABEND-CODE                    
01348          GO TO ABEND-PGM.                                         
01349                                                                   
01350      IF CLAS-I-BEN (CLAS-INDEXL) NOT = CLAS-LOOK                  
01351          ADD +1 TO CLAS-INDEXL                                    
01352          GO TO 4115-LIFE-LOOKUP.                                  
01353                                                                   
01354  4120-FIND-CLAS-A-TYPE.                                           
01355      IF CR-AHTYP = ZERO                                           
01356          GO TO 4130-FIND-CLAS-STATE-ABBR.                         
01357      MOVE CR-AHTYP               TO CLAS-LOOK.                    
01358      MOVE CLAS-STARTA            TO CLAS-INDEXA.                  
01359                                                                   
01360  4125-AH-LOOKUP.                                                  
01361      IF CLAS-INDEXA IS GREATER THAN CLAS-MAXA                     
01362          DISPLAY 'INVALID AH TYPE' CLAS-LOOK                      
01363          MOVE '0402'             TO ABEND-CODE                    
01364          GO TO ABEND-PGM.                                         
01365                                                                   
01366      IF CLAS-I-BEN (CLAS-INDEXA) NOT = CLAS-LOOK                  
01367          ADD +1 TO CLAS-INDEXA                                    
01368          GO TO 4125-AH-LOOKUP.                                    
01369                                                                   
01370      MOVE CLAS-I-EP (CLAS-INDEXA) TO AH-EARN-METHOD.              
01371                                                                   
PEMMOD*    IF AM-EARN-METHOD-A NOT EQUAL SPACES AND ZEROS               
PEMMOD*        MOVE AM-EARN-METHOD-A TO AH-EARN-METHOD.                 
01374                                                                   
01375  4130-FIND-CLAS-STATE-ABBR.                                       
01376      MOVE CLAS-STARTS TO CLAS-INDEXS.                             
01377                                                                   
01378  4135-STATE-LOOK-UP.                                              
01379      IF CLAS-INDEXS IS GREATER THAN CLAS-MAXS                     
01380          DISPLAY ' INVALID STATE CODE -' CR-STATE                 
01381          MOVE '0301'             TO ABEND-CODE                    
01382          GO TO ABEND-PGM.                                         
01383                                                                   
01384      IF STATE-SUB (CLAS-INDEXS) NOT = CR-STATE                    
01385          ADD +1 TO CLAS-INDEXS                                    
01386          GO TO 4135-STATE-LOOK-UP.                                
01387                                                                   
01388      IF (CR-LFTYP NOT = ZERO AND                                  
01389          CLAS-I-BAL (CLAS-INDEXL) = 'B')                          
01390          OR                                                       
01391         (CR-AHTYP NOT = ZERO AND                                  
01392          CLAS-I-BAL (CLAS-INDEXA) = 'B')                          
01393           MOVE 1                 TO OB-SW                         
01394      ELSE                                                         
01395           MOVE 0                 TO OB-SW.                        
01396                                                                   
01397      IF (CR-LFTYP NOT = ZERO AND                                  
01398          CLAS-I-BAL (CLAS-INDEXL) = 'Z')                          
01399          OR                                                       
01400         (CR-AHTYP NOT = ZERO AND                                  
01401          CLAS-I-BAL (CLAS-INDEXA) = 'Z')                          
01402           MOVE 1                 TO SUMMARY-SW                    
01403      ELSE                                                         
01404           MOVE 0                 TO SUMMARY-SW.                   
01405                                                                   
01406      IF CERT-ENTRY-DATE GREATER THAN PERIOD-START                 
01407          MOVE 2                  TO EFFECTIVE-CONTROL             
01408      ELSE                                                         
01409          MOVE 3                  TO EFFECTIVE-CONTROL.            
01410                                                                   
01411      MOVE ZEROS                  TO PRINT-SWITCHES.               
01412      MOVE ZEROS                  TO PRT-ISSUE-AMT-LF              
01413                                     PRT-CANC-AMT-LF               
01414                                     PRT-DEATH-AMT-LF              
01415                                     PRT-EXPIRY-AMT-LF             
01416                                     PRT-DECR-AMT-LF               
01417                                     PRT-INCR-AMT-LF               
01418                                     PRT-ISSUE-AMT-LF              
01419                                     PRT-CANC-AMT-LF               
01420                                     PRT-CLAIM-AMT-AH.             
01421      MOVE 1                      TO COUNT-WORK.                   
01422                                                                   
01423      MOVE CR-DT                      TO DC-GREG-DATE-CYMD.        
01424      MOVE 'L'                        TO DC-OPTION-CODE.           
01425      PERFORM 8500-DATE-CONVERT-ROUTINE THRU 8599-DATE-CONVERT-X.  
01426      MOVE DC-BIN-DATE-1              TO CERT-EFFECT-DT.           
01427                                                                   
01428      IF REINSURANCE-PASS OR REINSURANCE-REC                       
01429          MOVE CR-LOAN-1ST-PMT-DT TO DC-GREG-DATE-1-YMD            
01430          MOVE ZEROS                  TO DC-ELAPSED-MONTHS         
01431                                         DC-ELAPSED-DAYS           
01432          MOVE '3' TO DC-OPTION-CODE                               
01433          PERFORM 8500-DATE-CONVERT-ROUTINE THRU                   
01434                  8599-DATE-CONVERT-X                              
01435          MOVE DC-BIN-DATE-1 TO FIRST-PAY-DT                       
01436          GO TO 4137-SKIP-CALC.                                    
01437                                                                   
01438      IF CR-LOAN-1ST-PMT-DT NOT NUMERIC                            
01439          MOVE ZEROS                  TO CR-LOAN-1ST-PMT-DT.       
01440                                                                   
01441      MOVE LOW-VALUES             TO     FIRST-PAY-DT.             
01442                                                                   
01443      IF CR-LOAN-1ST-PMT-DT NOT = ZEROS                            
01444          MOVE CR-LOAN-1ST-PMT-DT     TO DC-GREG-DATE-1-YMD        
01445          MOVE ZEROS                  TO DC-ELAPSED-MONTHS         
01446                                         DC-ELAPSED-DAYS           
01447          MOVE '3'                    TO DC-OPTION-CODE            
01448          PERFORM 8500-DATE-CONVERT-ROUTINE THRU                   
01449                  8599-DATE-CONVERT-X                              
01450          IF NO-CONVERSION-ERROR                                   
01451              MOVE DC-BIN-DATE-1      TO FIRST-PAY-DT              
01452          ELSE                                                     
01453              MOVE ZEROS              TO CR-LOAN-1ST-PMT-DT.       
01454                                                                   
01455      IF FIRST-PAY-DT       LESS THAN  CERT-EFFECT-DT              
01456          MOVE ZEROS  TO  CR-LOAN-1ST-PMT-DT.                      
01457                                                                   
01458      IF CR-LOAN-1ST-PMT-DT = ZEROS                                
01459          MOVE CERT-EFFECT-DT         TO DC-BIN-DATE-1             
01460          MOVE +1                     TO DC-ELAPSED-MONTHS         
01461          MOVE ZEROS                  TO DC-ELAPSED-DAYS           
01462          MOVE '6'                    TO DC-OPTION-CODE            
01463          PERFORM 8500-DATE-CONVERT-ROUTINE THRU                   
01464                  8599-DATE-CONVERT-X                              
01465          MOVE DC-BIN-DATE-2          TO FIRST-PAY-DT              
01466          MOVE DC-GREG-DATE-1-YMD     TO CR-LOAN-1ST-PMT-DT.       
01467                                                                   
01468  4137-SKIP-CALC.                                                  
01469                                                                   
01470      IF SUMMARY-REC                                               
01471          COMPUTE COUNT-WORK = CR-LIVES - CR-SUM-CAN-CNT-ITD.      
01472      IF OB-COVERAGE                                               
01473          MOVE ZERO               TO COUNT-WORK.                   
01474                                                                   
01475      IF CR-ENTRY-STATUS = '3' OR '5'                              
01476          MOVE ZERO               TO COUNT-WORK.                   
01477                                                                   
01478      MOVE ZEROES                 TO DOLLAR-AMT                    
01479                                     DECREASING-DOLLAR             
01480                                     TEXAS-REG-SW                  
01481                                     NET-PAY-SW                    
01482                                     NEW-IN-EFFECT                 
01483                                     OLD-IN-EFFECT                 
01484                                     SAVE-BGN-INFORCE              
01485                                     SAVE-END-INFORCE              
01486                                     INFORCE-CNT-SW                
01487                                     CANCEL-SW                     
01488                                     CLAIM-SW                      
01489                                     PRINT-CNTRL.                  
01490                                                                   
01491      IF DTE-CLIENT EQUAL 'HAN'                                    
01492         MOVE '2' TO CR-IND-GRP.                                   
01493                                                                   
01494      IF CR-IND-GRP = '2'                                          
01495          MOVE 2                  TO INS-TYPE-SW                   
01496      ELSE                                                         
01497          MOVE 1                  TO INS-TYPE-SW.                  
01498                                                                   
01499      IF REINSURANCE-PASS                                          
01500          MOVE +5                 TO CL                            
01501      ELSE                                                         
01502          IF FIRST-INS-REC                                         
01503              MOVE +2             TO CL                            
01504          ELSE                                                     
01505              MOVE +4             TO CL.                           
01506                                                                   
01507      IF CR-LFTYP = ZERO                                           
01508          GO TO 4900-PROCESS-DISABILITY.                           
01509                                                                   
01510      IF CR-LF-CURRENT-STATUS = '4'                                
01511          IF CR-LF-CANCEL-EXIT-DATE IS GREATER THAN ZEROS          
01512              MOVE '8'            TO  CR-LF-CURRENT-STATUS.        
01513                                                                   
01514      IF CR-LF-CURRENT-STATUS = '6' OR '7' OR '8'                  
01515          NEXT SENTENCE                                            
01516      ELSE                                                         
01517          IF (CR-DTHAMT-YTD IS GREATER THAN +0) OR                 
01518             (CR-DTHAMT-YTD IS LESS THAN +0)                       
01519              MOVE '7'            TO  CR-LF-CURRENT-STATUS.        
01520                                                                   
01521      IF CR-LF-CURRENT-STATUS NOT = 6 AND 7 AND 8                  
01522          MOVE 0                  TO CERT-STATUS.                  
01523      IF CR-LF-CURRENT-STATUS = 7                                  
01524          MOVE 2                  TO CERT-STATUS.                  
01525      IF CR-LF-CURRENT-STATUS = 8                                  
01526          MOVE 1                  TO CERT-STATUS.                  
01527      IF CR-LF-CURRENT-STATUS = 6                                  
01528          MOVE 3                  TO CERT-STATUS.                  
01529                                                                   
01530      MOVE CERT-EFFECT-DT             TO CP-CERT-EFF-DT.           
01531      MOVE FIRST-PAY-DT               TO CP-FIRST-PAY-DATE.        
01532      MOVE SPACES                     TO CP-ACCT-FLD-5.            
01533      MOVE DTE-CLIENT                 TO CP-COMPANY-ID.            
01534      MOVE CR-LF-TERM                 TO CP-ORIGINAL-TERM.         
01535      MOVE DTE-REM-TRM                TO CP-REM-TERM-METHOD.       
01536      MOVE '3'                        TO CP-PROCESS-TYPE.          
01537      MOVE DTE-CLASIC-COMPANY-CD      TO CP-COMPANY-CD.            
01538      MOVE DTE-REM-TRM-CALC-OPTION    TO CP-REM-TRM-CALC-OPTION.   
01539                                                                   
01540      MOVE CLAS-I-RL-AH (CLAS-INDEXL) TO CP-BENEFIT-TYPE.          
01541      MOVE CLAS-I-BAL (CLAS-INDEXL)   TO CP-SPECIAL-CALC-CD.       
01542      MOVE CR-LF-TERM                 TO CP-ORIGINAL-TERM          
01543                                         CP-LOAN-TERM.             
01544                                                                   
01545      IF CLAS-I-EP (CLAS-INDEXL) = 'B' AND                         
01546         CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L'                  
01547          ADD +1                      TO CP-ORIGINAL-TERM          
01548                                         CP-LOAN-TERM.             
01549                                                                   
01550      IF CP-TERM-IS-DAYS                                           
01551          MOVE CR-LF-TERM-IN-DAYS     TO CP-TERM-OR-EXT-DAYS       
01552      ELSE                                                         
01553          MOVE ZEROS                  TO CP-TERM-OR-EXT-DAYS.      
01554                                                                   
01555      MOVE RUN-DATE-BIN       TO CP-VALUATION-DT.                  
01556                                                                   
01557      PERFORM 4585-GET-REMAINING-TERM THRU 4585-EXIT.              
01558                                                                   
01559      IF CLAS-I-EP (CLAS-INDEXL) = 'B' AND                         
01560         CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L'                  
01561          COMPUTE CP-REMAINING-TERM-1 = CP-REMAINING-TERM-1 - 1    
01562          COMPUTE CP-REMAINING-TERM-2 = CP-REMAINING-TERM-2 - 1.   
01563                                                                   
01564      MOVE CP-REMAINING-TERM-2  TO  SAVE-REM-TERM.                 
01565                                                                   
01566      MOVE EP-DT-BIN          TO CP-VALUATION-DT.                  
01567                                                                   
01568      PERFORM 4585-GET-REMAINING-TERM THRU 4585-EXIT.              
01569                                                                   
01570      MOVE ZEROS TO LF-BAL-REMTRM.                                 
01571      IF CLAS-I-EP (CLAS-INDEXL) = 'B' AND                         
01572         CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L'                  
01573          MOVE CP-REMAINING-TERM-2 TO LF-BAL-REMTRM                
01574          COMPUTE CP-REMAINING-TERM-1 = CP-REMAINING-TERM-1 - 1    
01575          COMPUTE CP-REMAINING-TERM-2 = CP-REMAINING-TERM-2 - 1.   
01576                                                                   
01577      IF CP-REMAINING-TERM-2 GREATER THAN CR-LF-TERM               
01578          MOVE CR-LF-TERM TO CP-REMAINING-TERM-2                   
01579                             CP-REMAINING-TERM-1.                  
01580                                                                   
01581      MOVE CP-REMAINING-TERM-2 TO LF-REM-TRM.                      

01584      MOVE CR-LF-EXPIRE-DATE TO DC-GREG-DATE-CYMD
01585      MOVE 'L' TO DC-OPTION-CODE
01586      PERFORM 8500-DATE-CONVERT-ROUTINE THRU
01587              8599-DATE-CONVERT-X
01588      MOVE DC-BIN-DATE-1 TO CERT-EXPIRE-DATE-O
01582                                                                   
01583      IF REINSURANCE-PASS OR REINSURANCE-REC                       
01584          MOVE CR-LF-EXPIRE-DATE TO DC-GREG-DATE-CYMD              
01585          MOVE 'L' TO DC-OPTION-CODE                               
01586          PERFORM 8500-DATE-CONVERT-ROUTINE THRU                   
01587                  8599-DATE-CONVERT-X                              
01588          MOVE DC-BIN-DATE-1 TO CERT-EXPIRE-DATE                   
01589          GO TO 4150-SKIP-CALC.                                    
01590                                                                   
01591      MOVE FIRST-PAY-DT            TO DC-BIN-DATE-1.               
01592                                                                   
01593      IF (CLAS-I-EP (CLAS-INDEXL) = 'B') AND                       
01594         (CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L')                
01595          MOVE CR-LF-TERM          TO DC-ELAPSED-MONTHS            
01596      ELSE                                                         
01597          COMPUTE DC-ELAPSED-MONTHS = CR-LF-TERM - +1.             
01598                                                                   
01599      MOVE '6'                     TO DC-OPTION-CODE.              
01600      PERFORM 8500-DATE-CONVERT-ROUTINE THRU                       
01601              8599-DATE-CONVERT-X.                                 
01602      IF NO-CONVERSION-ERROR                                       
01603          MOVE DC-BIN-DATE-2        TO CERT-EXPIRE-DATE            
01604      ELSE                                                         
01605          MOVE LOW-VALUES           TO CERT-EXPIRE-DATE.           
01606                                                                   
01607      IF (CERT-EXPIRE-DATE = LOW-VALUES)
01608         OR (DTE-REM-TRM-CALC-OPTION = '1' OR '2')
01609         MOVE CERT-EFFECT-DT          TO DC-BIN-DATE-1             
01610         MOVE CR-LF-TERM              TO DC-ELAPSED-MONTHS         
01611         MOVE ZEROS                   TO DC-ELAPSED-DAYS           
01612         MOVE '6'                     TO DC-OPTION-CODE            
01613         PERFORM 8500-DATE-CONVERT-ROUTINE THRU                    
01614                 8599-DATE-CONVERT-X                               
01615         IF NO-CONVERSION-ERROR                                    
01616            MOVE DC-BIN-DATE-2        TO CERT-EXPIRE-DATE          
01617         ELSE                                                      
01618            MOVE LOW-VALUES           TO CERT-EXPIRE-DATE.         
01619                                                                   
01620 *    IF CLAS-I-EP (CLAS-INDEXL) = 'B' AND                         
01621 *       CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L'                  
01622 *       MOVE CERT-EFFECT-DT          TO DC-BIN-DATE-1             
01623 *       COMPUTE DC-ELAPSED-MONTHS = CR-LF-TERM + 1                
01624 *       MOVE ZEROS                   TO DC-ELAPSED-DAYS           
01625 *       MOVE '6'                     TO DC-OPTION-CODE            
01626 *       PERFORM 8500-DATE-CONVERT-ROUTINE THRU                    
01627 *               8599-DATE-CONVERT-X                               
01628 *       IF NO-CONVERSION-ERROR                                    
01629 *          MOVE DC-BIN-DATE-2        TO CERT-EXPIRE-DATE          
01630 *       ELSE                                                      
01631 *          MOVE LOW-VALUES           TO CERT-EXPIRE-DATE.         
01632                                                                   
01633      IF CP-REMAINING-TERM-2  GREATER THAN ZEROS                   
01634          IF CERT-EXPIRE-DATE LESS THAN PERIOD-END                 
PEMMOD            MOVE CERT-EXPIRE-DATE-O
                                    TO CERT-EXPIRE-DATE.
PEMMOD*            MOVE PERIOD-END  TO  CERT-EXPIRE-DATE.
01636                                                                   
01637      MOVE CERT-EXPIRE-DATE TO DC-BIN-DATE-1.                      
01638      MOVE ' '                TO  DC-OPTION-CODE.                  
01639      PERFORM 8500-DATE-CONVERT-ROUTINE THRU 8599-DATE-CONVERT-X.  
01640      MOVE DC-GREG-DATE-CYMD  TO WS-CR-LF-EXPIRE-DATE-N            
01641                                 CR-LF-EXPIRE-DATE.                
01642                                                                   
01643  4150-SKIP-CALC.                                                  
01644                                                                   
01645      MOVE LOW-VALUES             TO LOW-EXIT-DATE.                
01646      MOVE CR-LF-DEX-CC           TO DC-ALPHA-CEN-N.               
01647      MOVE CR-LF-DEX-YR           TO DC-YMD-YEAR.                  
01648      MOVE CR-LF-DEX-MO           TO DC-YMD-MONTH.                 
01649      MOVE CR-LF-DEX-DA           TO DC-YMD-DAY.                   
01650      MOVE '3'                TO  DC-OPTION-CODE.                  
01651      PERFORM 8500-DATE-CONVERT-ROUTINE THRU 8599-DATE-CONVERT-X.  
01652      MOVE DC-BIN-DATE-1  TO  CLAIM-EXIT-DATE.                     
01653                                                                   
01654      MOVE CR-LF-CANCEL-EXIT-DATE  TO DC-GREG-DATE-CYMD.           
01655      MOVE 'L'                TO  DC-OPTION-CODE.                  
01656      PERFORM 8500-DATE-CONVERT-ROUTINE THRU 8599-DATE-CONVERT-X.  
01657      MOVE DC-BIN-DATE-1  TO  CANCEL-EXIT-DATE.                    
01658                                                                   
01659      MOVE CR-LF-CANC-DT      TO DC-GREG-DATE-CYMD.                
01660      MOVE 'L'                TO  DC-OPTION-CODE.                  
01661      PERFORM 8500-DATE-CONVERT-ROUTINE THRU 8599-DATE-CONVERT-X.  
01662      MOVE DC-BIN-DATE-1  TO  SUM-CANCEL-DATE.                     
01663                                                                   
01664      IF CLAIM-EXIT-DATE = SPACES OR ZEROS                         
01665          MOVE LOW-VALUES TO CLAIM-EXIT-DATE.                      
01666                                                                   
01667      IF CANCEL-EXIT-DATE = SPACES OR ZEROS                        
01668          MOVE LOW-VALUES TO CANCEL-EXIT-DATE.                     
01669                                                                   
01670      IF CLAIM-EXIT-DATE = LOW-VALUES AND                          
01671         CANCEL-EXIT-DATE = LOW-VALUES                             
01672          GO TO 4300-SET-SUBSCRIPTS.                               
01673                                                                   
01674      IF CLAIM-EXIT-DATE = LOW-VALUES                              
01675          MOVE CANCEL-EXIT-DATE   TO LOW-EXIT-DATE                 
01676          GO TO 4300-SET-SUBSCRIPTS.                               
01677                                                                   
01678      IF CANCEL-EXIT-DATE = LOW-VALUES                             
01679          MOVE CLAIM-EXIT-DATE    TO LOW-EXIT-DATE                 
01680          GO TO 4300-SET-SUBSCRIPTS.                               
01681                                                                   
01682      IF CLAIM-EXIT-DATE LESS THAN CANCEL-EXIT-DATE                
01683          MOVE CLAIM-EXIT-DATE    TO LOW-EXIT-DATE                 
01684      ELSE                                                         
01685          MOVE CANCEL-EXIT-DATE   TO LOW-EXIT-DATE.                
01686                                                                   
01687  4300-SET-SUBSCRIPTS.                                             
01688      IF (CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P')                 
01689          MOVE 1                  TO LIFE-INS-SW                   
01690      ELSE                                                         
01691          MOVE 2                  TO LIFE-INS-SW.                  
01692                                                                   
01693      IF CR-LF-TERM LESS THAN +121                                 
01694          MOVE +2                 TO TG                            
01695      ELSE                                                         
01696          MOVE +3                 TO TG.                           
01697                                                                   
01698      IF CR-LF-TERM LESS THAN +61                                  
01699          MOVE +1                 TO TG.                           
01700      MOVE TG                     TO SAV-LF-TERM-CODE.             
01701                                                                   
01702  4410-INFORCE-PRIOR-PERIOD.                                       
01703      IF NOT EFFECTIVE-BOTH-PER                                    
01704          GO TO 4420-INFORCE-THIS-PERIOD.                          
01705                                                                   
01706      IF CLAIM-EXIT-DATE NOT = LOW-VALUES  AND                     
01707         CANCEL-EXIT-DATE NOT = LOW-VALUES                         
01708          IF CLAIM-EXIT-DATE NOT GREATER THAN PERIOD-START AND     
01709             CANCEL-EXIT-DATE GREATER THAN PERIOD-START            
01710              MOVE 1              TO CANCEL-SW                     
01711              MOVE 0              TO COUNT-WORK                    
01712              GO TO 4700-COMPUTE-CANCEL                            
01713          ELSE                                                     
01714              IF CANCEL-EXIT-DATE NOT GREATER THAN PERIOD-START    
01715              AND                                                  
01716              CLAIM-EXIT-DATE GREATER THAN PERIOD-START            
01717               MOVE 1             TO CLAIM-SW                      
01718               MOVE 0             TO COUNT-WORK                    
01719               GO TO 4750-COMPUTE-CLAIM.                           
01720                                                                   
01721      IF CERT-CLAIM  AND                                           
01722         LOW-EXIT-DATE = LOW-VALUES                                
01723          GO TO 4415-CONTINUE.                                     
01724                                                                   
01725      IF CLAIM-OR-CANCEL AND                                       
01726         LOW-EXIT-DATE NOT GREATER THAN PERIOD-START AND           
01727             CR-DTHAMT-YTD NOT = ZERO                              
01728                 GO TO 4750-COMPUTE-CLAIM.                         
01729                                                                   
01730      IF CLAIM-OR-CANCEL AND                                       
01731         LOW-EXIT-DATE NOT GREATER THAN PERIOD-START               
01732          GO TO 4900-PROCESS-DISABILITY.                           
01733                                                                   
01734  4415-CONTINUE.                                                   
01735                                                                   
01736      IF (CERT-EXPIRE-DATE NOT GREATER THAN PERIOD-START) AND      
01737         NOT CLAIM-OR-CANCEL                                       
01738             GO TO 4900-PROCESS-DISABILITY.                        
01739                                                                   
01740      MOVE 1                      TO OLD-IN-EFFECT.                
01741                                                                   
01742      IF CERT-EXPIRE-DATE NOT GREATER THAN PERIOD-START            
01743          MOVE 0                  TO COUNT-WORK                    
01744          GO TO 4420-INFORCE-THIS-PERIOD.                          
01745                                                                   
01746      IF (CR-LFTYP = ZERO) OR                                      
01747         (CR-ENTRY-STATUS = '5') OR                                
01748         (OB-COVERAGE)                                             
01749            MOVE ZERO             TO INFORCE-DOLLARS               
01750            GO TO 4420-INFORCE-THIS-PERIOD.                        
01751                                                                   
01752      IF NOT SUMMARY-REC                                           
01753         NEXT SENTENCE                                             
01754       ELSE                                                        
01755          IF (SUM-CANCEL-DATE GREATER PERIOD-START) OR             
01756             (CR-LFRFND = ZERO)                                    
01757              MOVE CR-LIVES       TO COUNT-WORK                    
01758              MOVE CR-LFAMT       TO INFORCE-DOLLARS               
01759            ELSE                                                   
01760              IF CR-LFPRM NOT GREATER CR-LFRFND                    
01761                 MOVE ZERO        TO INFORCE-DOLLARS               
01762                                     CR-LFPRM                      
01763                                     CR-LFRFND                     
01764               ELSE                                                
01765                 COMPUTE TEMP-RESULT-L ROUNDED =                   
01766                                      CR-LFRFND / CR-LFPRM         
01767                 COMPUTE INFORCE-DOLLARS ROUNDED = CR-LFAMT -      
01768                                     (CR-LFAMT * TEMP-RESULT-L).   
01769                                                                   
01770      PERFORM 8300-STARTING-INFORCE THRU 8300-EXIT.                
01771                                                                   
01772                                                                   
01773      IF DTE-CLIENT = 'HER'                                        
01774          MOVE CR-LFAMT TO INFORCE-DOLLARS                         
01775          IF CLAS-I-EP (CLAS-INDEXL) = 'B'                         
01776              ADD CR-LFAMT-ALT TO INFORCE-DOLLARS.                 
01777                                                                   
01778      MOVE +1                     TO EG.                           
01779      PERFORM 8400-ADD-TO-EXHIBIT THRU 8400-EXIT.                  
01780                                                                   
01781      MOVE INFORCE-DOLLARS        TO SAVE-BGN-INFORCE.             
01782                                                                   
01783  4420-INFORCE-THIS-PERIOD.                                        
01784                                                                   
01785      IF CERT-CLAIM  AND                                           
01786         LOW-EXIT-DATE = LOW-VALUES                                
01787          NEXT SENTENCE                                            
01788      ELSE                                                         
01789         IF CLAIM-OR-CANCEL AND                                    
01790            LOW-EXIT-DATE NOT GREATER THAN PERIOD-END              
01791             GO TO 4500-COMPUTE-UNEARNED.                          
01792                                                                   
01793      IF CERT-EXPIRE-DATE NOT GREATER THAN PERIOD-END              
01794          GO TO 4500-COMPUTE-UNEARNED.                             
01795                                                                   
01796      MOVE 1                      TO NEW-IN-EFFECT.                
01797                                                                   
01798      IF NOT REINSURANCE-PASS                                      
01799          ADD +1 TO EXB-CNT.                                       
01800                                                                   
01801      IF (CR-LFTYP = ZERO) OR                                      
01802         (CR-ENTRY-STATUS = '5') OR                                
01803         (OB-COVERAGE)                                             
01804            MOVE ZERO             TO INFORCE-DOLLARS               
01805            GO TO 4500-COMPUTE-UNEARNED.                           
01806                                                                   
01807      IF SUMMARY-REC                                               
01808          COMPUTE COUNT-WORK = CR-LIVES - CR-SUM-CAN-CNT-ITD       
01809          IF CR-LFPRM NOT GREATER CR-LFRFND                        
01810             MOVE ZERO            TO INFORCE-DOLLARS               
01811                                     CR-LFPRM                      
01812                                     CR-LFRFND                     
01813           ELSE                                                    
01814            IF CR-LFRFND = ZERO                                    
01815              MOVE CR-LFAMT       TO INFORCE-DOLLARS               
01816            ELSE                                                   
01817                 COMPUTE TEMP-RESULT-L ROUNDED =                   
01818                                      CR-LFRFND / CR-LFPRM         
01819                 COMPUTE INFORCE-DOLLARS ROUNDED = CR-LFAMT -      
01820                                     (CR-LFAMT * TEMP-RESULT-L).   
01821                                                                   
01822      PERFORM 8350-ENDING-INFORCE THRU 8300-EXIT.                  
01823                                                                   
01824      IF DTE-CLIENT = 'HER'                                        
01825          MOVE CR-LFAMT TO INFORCE-DOLLARS                         
01826          IF CLAS-I-EP (CLAS-INDEXL) = 'B'                         
01827              ADD CR-LFAMT-ALT TO INFORCE-DOLLARS.                 
01828                                                                   
01829      IF GROUP-INS                                                 
01830          ADD COUNT-WORK TO GROUP-CNT (CL  TG  9)                  
01831          ADD INFORCE-DOLLARS TO GROUP-AMT (CL  TG  9)             
01832      ELSE                                                         
01833          ADD COUNT-WORK TO IND-CNT (CL  TG  9)                    
01834          ADD INFORCE-DOLLARS TO IND-AMT (CL  TG  9).              
01835                                                                   
01836      MOVE 1                      TO INFORCE-CNT-SW.               
01837      MOVE INFORCE-DOLLARS        TO DECREASING-DOLLAR             
01838                                     SAVE-END-INFORCE.             
01839                                                                   
01840  4500-COMPUTE-UNEARNED.                                           
01841      IF CR-ENTRY-STATUS = ('3' OR '5')                            
01842          GO TO 4600-COMPUTE-ISSUES.                               
01843                                                                   
01844      IF OB-COVERAGE                                               
01845          GO TO 4600-COMPUTE-ISSUES.                               
01846                                                                   
01847      MOVE CR-DT                      TO DC-GREG-DATE-CYMD.        
01848      MOVE 'L'                        TO DC-OPTION-CODE.           
01849      PERFORM 8500-DATE-CONVERT-ROUTINE THRU 8599-DATE-CONVERT-X.  
01850      MOVE DC-BIN-DATE-1              TO CP-CERT-EFF-DT.           
01851                                                                   
01852      IF CR-LOAN-1ST-PMT-DT NOT NUMERIC                            
01853          MOVE ZEROS                  TO CR-LOAN-1ST-PMT-DT.       
01854                                                                   
01855      MOVE LOW-VALUES  TO  CP-FIRST-PAY-DATE.                      
01856                                                                   
01857      IF CR-LOAN-1ST-PMT-DT NOT = ZEROS                            
01858          MOVE CR-LOAN-1ST-PMT-DT     TO DC-GREG-DATE-1-YMD        
01859          MOVE ZEROS                  TO DC-ELAPSED-MONTHS         
01860                                         DC-ELAPSED-DAYS           
01861          MOVE '3'                    TO DC-OPTION-CODE            
01862          PERFORM 8500-DATE-CONVERT-ROUTINE THRU                   
01863                  8599-DATE-CONVERT-X                              
01864          IF NO-CONVERSION-ERROR                                   
01865              MOVE DC-BIN-DATE-1      TO CP-FIRST-PAY-DATE         
01866          ELSE                                                     
01867              MOVE ZEROS              TO CR-LOAN-1ST-PMT-DT.       
01868                                                                   
01869      IF CP-FIRST-PAY-DATE LESS THAN  CP-CERT-EFF-DT               
01870          MOVE ZEROS  TO  CR-LOAN-1ST-PMT-DT.                      
01871                                                                   
01872      IF CR-LOAN-1ST-PMT-DT = ZEROS                                
01873          MOVE CP-CERT-EFF-DT         TO DC-BIN-DATE-1             
01874          MOVE +1                     TO DC-ELAPSED-MONTHS         
01875          MOVE ZEROS                  TO DC-ELAPSED-DAYS           
01876          MOVE '6'                    TO DC-OPTION-CODE            
01877          PERFORM 8500-DATE-CONVERT-ROUTINE THRU                   
01878                  8599-DATE-CONVERT-X                              
01879          MOVE DC-BIN-DATE-2          TO CP-FIRST-PAY-DATE         
01880          MOVE DC-GREG-DATE-1-YMD     TO CR-LOAN-1ST-PMT-DT.       
01881                                                                   
01882      IF ENTERED-PREV-PERIOD                                       
01883          MOVE RUN-DATE-BIN       TO CP-VALUATION-DT               
01884          MOVE +1                 TO FACTOR-C                      
01885          PERFORM 4550-UNEARNED-CALC THRU 4550-EXIT                
01886          MOVE UNEXPIRED-MONTHS   TO HOLD-PREV-UNEXP               
01887          MOVE EXPIRED-MONTHS     TO HOLD-PREV-EXP.                
01888                                                                   
01889      IF ENTERED-PREV-PERIOD                                       
01890          IF CERT-CANCELLED                                        
01891              IF CANCEL-EXIT-DATE IS NOT GREATER THAN PERIOD-END   
01892                  GO TO 4700-COMPUTE-CANCEL.                       
01893                                                                   
01894      IF ENTERED-THIS-PERIOD                                       
01895          IF CERT-CANCELLED                                        
01896              IF CANCEL-EXIT-DATE IS NOT GREATER THAN PERIOD-END   
01897                  GO TO 4600-COMPUTE-ISSUES.                       
01898                                                                   
01899      IF ENTERED-THIS-PERIOD                                       
01900          MOVE EP-DT-BIN          TO CP-VALUATION-DT               
01901          MOVE -1                 TO FACTOR-C                      
01902          PERFORM 4550-UNEARNED-CALC THRU 4550-EXIT                
01903          MOVE UNEXPIRED-MONTHS   TO HOLD-END-UNEXP                
01904          MOVE EXPIRED-MONTHS     TO HOLD-END-EXP.                 
01905                                                                   
01906      GO TO 4600-COMPUTE-ISSUES.                                   
01907                                                                   
01908  4550-UNEARNED-CALC.                                              
01909                                                                   
01910      MOVE SPACES                     TO CP-ACCT-FLD-5.            
01911      MOVE DTE-CLIENT                 TO CP-COMPANY-ID.            
01912      MOVE CR-LF-TERM                 TO CP-ORIGINAL-TERM.         
01913      MOVE DTE-REM-TRM                TO CP-REM-TERM-METHOD.       
01914      MOVE '3'                        TO CP-PROCESS-TYPE.          
01915      MOVE DTE-CLASIC-COMPANY-CD      TO CP-COMPANY-CD.            
01916      MOVE DTE-REM-TRM-CALC-OPTION    TO CP-REM-TRM-CALC-OPTION.   
01917      MOVE CLAS-I-RL-AH (CLAS-INDEXL) TO CP-BENEFIT-TYPE.          
01918      MOVE CLAS-I-BAL   (CLAS-INDEXL) TO CP-SPECIAL-CALC-CD.       
01919                                                                   
01920      IF (CLAS-I-EP (CLAS-INDEXL) = 'B') AND                       
01921         (CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L')                
01922          ADD +1                      TO CP-ORIGINAL-TERM          
01923                                         CP-LOAN-TERM.             
01924                                                                   
01925      IF CP-TERM-IS-DAYS                                           
01926          MOVE CR-LF-TERM-IN-DAYS     TO CP-TERM-OR-EXT-DAYS       
01927      ELSE                                                         
01928          MOVE ZEROS                  TO CP-TERM-OR-EXT-DAYS.      
01929                                                                   
01930      PERFORM 4585-GET-REMAINING-TERM THRU 4585-EXIT.              
01931                                                                   
01932      IF NO-CP-ERROR                                               
01933          NEXT SENTENCE                                            
01934      ELSE                                                         
01935          MOVE +0                     TO CP-REMAINING-TERM-1       
01936                                         CP-REMAINING-TERM-2.      
01937                                                                   
01938      IF (CLAS-I-EP (CLAS-INDEXL) = 'B') AND                       
01939         (CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L')                
01940          COMPUTE CP-REMAINING-TERM-1 = (CP-REMAINING-TERM-1 - +1).
01941                                                                   
01942      IF CP-REMAINING-TERM-1 IS NEGATIVE                           
01943          MOVE ZEROS                  TO CP-REMAINING-TERM-1.      
01944      IF CP-REMAINING-TERM-2 IS NEGATIVE                           
01945          MOVE ZEROS                  TO CP-REMAINING-TERM-2.      
01946                                                                   
01947      IF CP-REMAINING-TERM-1 = ZEROS                               
01948          IF (CLAS-I-EP (CLAS-INDEXL) = 'B') AND                   
01949             (CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L') AND        
01950             (CP-REMAINING-TERM-2 IS GREATER THAN ZEROS)           
01951              NEXT SENTENCE                                        
01952          ELSE                                                     
01953              GO TO 4550-EXIT.                                     
01954                                                                   
01955      MOVE CP-REMAINING-TERM-1        TO UNEXPIRED-MONTHS.         
01956      COMPUTE EXPIRED-MONTHS = CR-LF-TERM - UNEXPIRED-MONTHS.      
01957                                                                   
01958      IF CR-LFPRM NOT NUMERIC                                      
01959         MOVE +0 TO CR-LFPRM.                                      
01960                                                                   
01961      IF SUMMARY-REC                                               
01962          IF CR-LFRFND = +0                                        
01963              MOVE CR-LFPRM       TO CP-ORIGINAL-PREMIUM           
01964          ELSE                                                     
01965              COMPUTE CP-ORIGINAL-PREMIUM ROUNDED =                
01966                      CR-LFPRM - CR-LFRFND                         
01967      ELSE                                                         
01968          MOVE CR-LFPRM           TO CP-ORIGINAL-PREMIUM.          
01969                                                                   
01970      IF CR-LFPRM-ALT NOT NUMERIC                                  
01971         MOVE +0 TO CR-LFPRM-ALT.                                  
01972      MOVE CR-LFPRM-ALT           TO CP-ALTERNATE-PREMIUM          
01973      IF CR-LF-TERM NOT NUMERIC                                    
01974         MOVE +0 TO CR-LF-TERM.                                    
01975      MOVE CR-LF-TERM             TO CP-ORIGINAL-TERM              
01976      MOVE UNEXPIRED-MONTHS       TO CP-REMAINING-TERM             
01977      MOVE CLAS-I-EP (CLAS-INDEXL)                                 
01978                                  TO CP-EARNING-METHOD             
01979      MOVE CLAS-I-RL-AH (CLAS-INDEXL)                              
01980                                  TO CP-BENEFIT-TYPE               
01981      MOVE CLAS-I-CALC-TYPE (CLAS-INDEXL)                          
01982                                  TO CP-SPECIAL-CALC-CD            
01983      MOVE CR-LFTYP               TO CP-BENEFIT-CD                 
01984      MOVE LIFE-OVERRIDE-L1       TO CP-LIFE-OVERRIDE-CODE         
01985      PERFORM 4945-CALC-UNEARNED-PREMIUM THRU 4945-AH-EXIT.        
01986                                                                   
01987      IF CP-ERROR-OCCURED                                          
01988         GO TO 4575-ERROR-LF.                                      
01989                                                                   
01990      MOVE CP-STATE-U-PRM         TO UNEARNED-AMT-LF               
01991                                                                   
01992      IF CLAS-I-EP (CLAS-INDEXL) = 'T'                             
01993          MOVE '1'                TO  TEXAS-REG-SW.                
01994                                                                   
01995      IF CLAS-I-EP (CLAS-INDEXL) = 'N'                             
01996          MOVE '1'                TO  NET-PAY-SW.                  
01997                                                                   
01998  4552-LIFE-EARNINGS-CONT.                                         
01999                                                                   
02000      COMPUTE UNEARNED-AMT-LF = UNEARNED-AMT-LF * FACTOR-C.        
02001                                                                   
02002      IF LEVEL-LIFE                                                
02003          MOVE +2                 TO IY                            
02004      ELSE                                                         
02005          MOVE +1                 TO IY.                           
02006                                                                   
02007      IF INDIVIDUAL-INS                                            
02008          ADD +5 TO IY.                                            
02009                                                                   
02010      ADD UNEARNED-AMT-LF TO EARN-PREM (CL  TG  IY).               
02011                                                                   
02012  4550-EXIT.                                                       
02013      EXIT.                                                        
02014                                                                   
02015  4575-ERROR-LF.                                                   
02016                                                                   
02017      DISPLAY 'ERROR OCCURED IN LF UNEARNED ROUTINE'               
02018      DISPLAY 'CP RETURN CODE ...' CP-RETURN-CODE.                 
02019      DISPLAY 'FULL CONTROL   ...' CR-FULL-CONTROL                 
02020      GO TO ABEND-PGM.                                             
02021                                                                   
02022  4585-GET-REMAINING-TERM.                                         
02023                                                                   
02024      CALL 'ELRTRMX' USING CALCULATION-PASS-AREA.                  
02025                                                                   
02026      IF DTE-CLIENT = 'NCL'                                        
02027          MOVE CP-REMAINING-TERM-1 TO CP-REMAINING-TERM-2.         
02028                                                                   
02029  4585-EXIT.                                                       
02030      EXIT.                                                        
02031                                                                   
02032  4600-COMPUTE-ISSUES.                                             
02033      IF EFFECTIVE-BOTH-PER                                        
02034          GO TO 4700-COMPUTE-CANCEL.                               
02035                                                                   
02036      IF CR-ENTRY-STATUS = '3' OR '5'                              
02037          GO TO 4700-COMPUTE-CANCEL.                               
02038                                                                   
02039      MOVE '1'                    TO LF-ISSUE.                     
02040      MOVE CR-LFAMT               TO PRT-ISSUE-AMT-LF.             
02041      MOVE CR-LFAMT               TO TEMP-RESULT.                  
02042                                                                   
02043                                                                   
02044      IF LEVEL-LIFE                                                
02045          MOVE +2                 TO IY                            
02046      ELSE                                                         
02047          MOVE +1                 TO IY.                           
02048                                                                   
02049      IF OB-COVERAGE                                               
02050          MOVE +3                 TO IY.                           
02051                                                                   
02052  4610-TOT-LF-ISSUES.                                              
02053      MOVE TEMP-RESULT            TO INFORCE-DOLLARS.              
02054                                                                   
02055      MOVE +2                     TO EG.                           
02056      IF SUMMARY-REC                                               
02057          MOVE CR-LIVES           TO COUNT-WORK.                   
02058      PERFORM 8400-ADD-TO-EXHIBIT THRU 8400-EXIT.                  
02059                                                                   
02060      MOVE 1                      TO INFORCE-CNT-SW.               
02061      ADD INFORCE-DOLLARS TO SAVE-BGN-INFORCE.                     
02062                                                                   
02063      IF INDIVIDUAL-INS                                            
02064          ADD +5 TO IY.                                            
02065                                                                   
02066      IF CR-LFPRM-ALT NOT NUMERIC                                  
02067          MOVE ZEROS              TO  CR-LFPRM-ALT.                
02068                                                                   
02069      IF CLAS-I-EP (CLAS-INDEXL) = 'B'                             
02070          ADD CR-LFPRM            TO  EARN-PREM  (CL  TG  IY)      
02071                                      ISSUE-PREM (CL  TG  IY)      
02072          ADD CR-LFPRM-ALT        TO  EARN-PREM  (CL  TG  IY)      
02073                                      ISSUE-PREM (CL  TG  IY)      
02074      ELSE                                                         
02075          ADD CR-LFPRM            TO  EARN-PREM  (CL  TG  IY)      
02076                                      ISSUE-PREM (CL  TG  IY).     
02077                                                                   
02078      ADD TEMP-RESULT TO ISSUE-AMT (CL  TG  IY).                   
02079      ADD COUNT-WORK TO ISSUE-CNT (CL  TG  IY).                    
02080                                                                   
02081  4700-COMPUTE-CANCEL.                                             
02082 *    NOTE ******************************************************* 
02083 *         *  EFFECTIVE AS OF VMOD 6.30 THE FOLLOWING TWO        * 
02084 *         *  LINES OF CODE ARE DELETED. IF PRESENT, YOUR CANCEL * 
02085 *         *  COUNT AND CLAIM COUNT WILL BE TOO LOW BECAUSE ONLY * 
02086 *         *  POLICIES ISSUED IN CURRENT YEAR WILL BE COUNTED.   * 
02087 *         *  THE AMOUNTS ARE CORRECT AND WILL BE UNAFFECTED.    * 
02088 *         *******************************************************.
02089 *    IF INFORCE-CNT-SW NOT = 1                                    
02090 *        MOVE 0 TO COUNT-WORK.                                    
02091 ******************************************************************
02092      IF SUMMARY-REC AND                                           
02093         (SUM-CANCEL-DATE NOT GREATER PERIOD-END) AND              
02094         (SUM-CANCEL-DATE GREATER PERIOD-START) AND                
02095         (CR-SUM-CAN-CNT-YTD NOT = ZERO)                           
02096          NEXT SENTENCE                                            
02097        ELSE                                                       
02098         IF LUMP-SUM AND                                           
02099            (CLAIM-EXIT-DATE NOT GREATER PERIOD-END) AND           
02100            (CLAIM-EXIT-DATE NOT LESS PERIOD-START)                
02101             NEXT SENTENCE                                         
02102          ELSE                                                     
02103             IF (CANCEL-EXIT-DATE = ZERO OR LOW-VALUES) OR         
02104                CANCEL-EXIT-DATE GREATER THAN PERIOD-END           
02105                 GO TO 4750-COMPUTE-CLAIM.                         
02106                                                                   
PEMMOD     SUBTRACT CR-CCYY FROM CR-LF-CNC-CCYY GIVING INTERIM-YR.
PEMMOD*    SUBTRACT CR-YR FROM CR-LF-CNC-YR GIVING INTERIM-YR.
02108      IF INTERIM-YR < 0                                            
02109         ADD 100 TO INTERIM-YR                                     
02110      END-IF.                                                      
02111      COMPUTE TERM-WORK = (INTERIM-YR * 12)                        
02112                         + (CR-LF-CNC-MO - CR-MO).                 
02113      IF CR-LF-CNC-DA LESS THAN CR-DA                              
02114          SUBTRACT 1 FROM TERM-WORK.                               
02115                                                                   
02116      IF LUMP-SUM                                                  
02117          MOVE '1'                TO LF-LUMP                       
02118      ELSE                                                         
02119          MOVE '1'                TO LF-CANC.                      
02120                                                                   
02121      MULTIPLY CR-LFRFND BY -1 GIVING UNEARNED-AMT-LF.             
02122      MOVE '1'                    TO LF-CANC.                      
02123      MOVE CR-LFRFND              TO PRT-CANC-AMT-LF.              
02124                                                                   
02125      IF LEVEL-LIFE                                                
02126          MOVE +2                 TO IY                            
02127      ELSE                                                         
02128          MOVE +1                 TO IY.                           
02129                                                                   
02130      IF OB-COVERAGE                                               
02131          MOVE +3                 TO IY.                           
02132                                                                   
02133      IF INDIVIDUAL-INS                                            
02134          ADD +5 TO IY.                                            
02135                                                                   
02136      ADD UNEARNED-AMT-LF TO EARN-PREM (CL  TG  IY).               
02137                                                                   
02138      IF SUMMARY-REC                                               
02139          GO TO 4703-SUM-CANC.                                     
02140                                                                   
02141      IF LEVEL-LIFE                                                
02142          MOVE CR-LFAMT           TO DOLLAR-AMT                    
02143          GO TO 4705-LF-CANCEL-GO.                                 
02144                                                                   
02145      IF OB-COVERAGE                                               
02146          MOVE CR-LFAMT           TO DOLLAR-AMT                    
02147          GO TO 4705-LF-CANCEL-GO.                                 
02148                                                                   
02149      IF TERM-WORK NOT POSITIVE                                    
02150          MOVE CR-LFAMT           TO DOLLAR-AMT                    
02151          GO TO 4705-LF-CANCEL-GO.                                 
02152                                                                   
02153      IF TEXAS-CERT                                                
02154          GO TO 4701-TEX-CANC.                                     
02155                                                                   
02156      IF NET-PAY-CERT                                              
02157          GO TO 4702-NET-PAY-CANC.                                 
02158                                                                   
02159  4700-REG-CANCEL.                                                 
02160      COMPUTE DOLLAR-AMT = CR-LFAMT - ((TERM-WORK / CR-LF-TERM)    
02161                              * CR-LFAMT).                         
02162      GO TO 4705-LF-CANCEL-GO.                                     
02163                                                                   
02164  4701-TEX-CANC.                                                   
02165      DIVIDE CR-LFAMT BY CR-LF-TERM                                
02166          GIVING TEX-FACT-1.                                       
02167      DIVIDE TERM-WORK BY CR-PMT-FREQ                              
02168          GIVING TEX-FACT-2                                        
02169              REMAINDER TEX-FACT-3.                                
02170      COMPUTE DOLLAR-AMT =                                         
02171          (TEX-FACT-1 * (TEX-FACT-2 * CR-PMT-FREQ)).               
02172                                                                   
02173      GO TO 4705-LF-CANCEL-GO.                                     
02174                                                                   
02175  4702-NET-PAY-CANC.                                               
02176                                                                   
02177      MOVE CR-DT                  TO  DC-GREG-DATE-CYMD.           
02178      MOVE 'L'                    TO  DC-OPTION-CODE.              
02179      PERFORM 8500-DATE-CONVERT-ROUTINE THRU 8599-DATE-CONVERT-X.  
02180      MOVE DC-BIN-DATE-1          TO  CP-CERT-EFF-DT.              
02181                                                                   
02182      IF CR-LOAN-1ST-PMT-DT NOT = ZEROS                            
02183          MOVE CR-LOAN-1ST-PMT-DT TO  DC-GREG-DATE-1-YMD           
02184          MOVE '3'                TO  DC-OPTION-CODE               
02185          PERFORM 8500-DATE-CONVERT-ROUTINE THRU                   
02186              8599-DATE-CONVERT-X                                  
02187          MOVE DC-BIN-DATE-1      TO  CP-FIRST-PAY-DATE            
02188      ELSE                                                         
02189          MOVE CP-CERT-EFF-DT     TO  DC-BIN-DATE-1                
02190          MOVE +1                 TO  DC-ELAPSED-MONTHS            
02191          MOVE +0                 TO  DC-ELAPSED-DAYS              
02192          MOVE '6'                TO  DC-OPTION-CODE               
02193          PERFORM 8500-DATE-CONVERT-ROUTINE THRU                   
02194              8599-DATE-CONVERT-X                                  
02195          MOVE DC-BIN-DATE-2      TO  CP-FIRST-PAY-DATE.           
02196                                                                   
02197      MOVE CR-LFAMT               TO  CP-ORIGINAL-BENEFIT.         
02198      MOVE CR-APR                 TO  CP-LOAN-APR.                 
02199      MOVE CR-LF-TERM             TO  CP-ORIGINAL-TERM.            
02200      MOVE CR-LOAN-TERM           TO  CP-LOAN-TERM.                
02201      COMPUTE CP-REMAINING-TERM = CR-LF-TERM - TERM-WORK.          
02202      MOVE STATE-ABBR       (CLAS-INDEXS) TO  CP-STATE-STD-ABBRV.  
02203      MOVE CLAS-I-CALC-TYPE (CLAS-INDEXL) TO  CP-SPECIAL-CALC-CD.  
02204      MOVE CLAS-I-RL-AH     (CLAS-INDEXL) TO  CP-BENEFIT-TYPE.     
02205      MOVE CLAS-I-EP        (CLAS-INDEXL) TO  CP-EARNING-METHOD.   
02206                                                                   
02207      CALL 'ELRAMTX' USING CALCULATION-PASS-AREA.                  
02208                                                                   
02209      IF SUMMARY-REC                                               
02210          COMPUTE DOLLAR-AMT ROUNDED = CP-REMAMT-FACTOR *          
02211                                       (TEMP-1 / +1000)            
02212          GO TO 4705-LF-CANCEL-GO.                                 
02213                                                                   
02214      IF CP-STATE-STD-ABBRV = 'NC'                                 
02215                AND                                                
CIDMOD*      DTE-CLIENT NOT = 'WDS'                                     
CIDMOD       (DTE-CLIENT NOT = 'WDS' AND 'CID')                         
02217           IF CP-CERT-EFF-DT GREATER THAN WS-931231                
02218              MOVE CP-REMAINING-AMT                                
02219                                   TO  DOLLAR-AMT                  
02220              GO TO 4705-LF-CANCEL-GO.                             
02221                                                                   
02222       COMPUTE DOLLAR-AMT ROUNDED = CP-REMAMT-FACTOR *             
02223                                       (CR-LFAMT / +1000).         
02224 *    IF SUMMARY-REC                                               
02225 *          COMPUTE DOLLAR-AMT ROUNDED =                           
02226 *                          CP-REMAMT-FACTOR * (TEMP-1 / +1000)    
02227 *      ELSE                                                       
02228 *          COMPUTE DOLLAR-AMT ROUNDED =                           
02229 *                          CP-REMAMT-FACTOR * (CR-LFAMT / +1000). 
02230                                                                   
02231      GO TO 4705-LF-CANCEL-GO.                                     
02232                                                                   
02233  4703-SUM-CANC.                                                   
02234      IF CR-LFPRM NOT GREATER CR-LFRFND                            
02235         MOVE ZERO                TO DOLLAR-AMT                    
02236         GO TO 4705-LF-CANCEL-GO.                                  
02237                                                                   
02238      IF CR-LFRFND = ZERO                                          
02239         MOVE CR-LFAMT            TO TEMP-1                        
02240       ELSE                                                        
02241                 COMPUTE TEMP-RESULT-L ROUNDED =                   
02242                                      CR-LFRFND / CR-LFPRM         
02243                 COMPUTE TEMP-1 ROUNDED = CR-LFAMT -               
02244                                     (CR-LFAMT * TEMP-RESULT-L).   
02245                                                                   
02246      IF NET-PAY-CERT                                              
02247          GO TO 4702-NET-PAY-CANC.                                 
02248                                                                   
02249      IF LEVEL-LIFE                                                
02250          MOVE TEMP-1             TO DOLLAR-AMT                    
02251       ELSE                                                        
02252          COMPUTE DOLLAR-AMT = TEMP-1 - ((TERM-WORK / CR-LF-TERM)  
02253                                                         * TEMP-1).
02254                                                                   
02255  4705-LF-CANCEL-GO.                                               
02256      MOVE DOLLAR-AMT             TO DECREASING-DOLLAR.            
02257                                                                   
02258      IF DOLLAR-AMT NEGATIVE                                       
02259          MOVE ZERO               TO DOLLAR-AMT.                   
02260                                                                   
02261  4710-TOT-LF-CANCEL.                                              
02262      MOVE +6                     TO EG.                           
02263      MOVE DOLLAR-AMT             TO INFORCE-DOLLARS.              
02264      MOVE COUNT-WORK             TO COUNT-SAVE.                   
02265                                                                   
02266      IF (CLAIM-EXIT-DATE NOT = ZERO AND LOW-VALUES) AND           
02267         (CLAIM-EXIT-DATE NOT GREATER PERIOD-END) AND              
02268         (CLAIM-EXIT-DATE GREATER PERIOD-START) AND                
02269         (NOT LUMP-SUM)                                            
02270          MOVE ZERO               TO COUNT-WORK                    
02271          MOVE ZERO               TO DOLLAR-AMT INFORCE-DOLLARS.   
02272                                                                   
02273      IF SUMMARY-REC                                               
02274          MOVE CR-SUM-CAN-CNT-YTD TO COUNT-WORK.                   
02275                                                                   
02276      IF NOT CURRENT-CANCEL-ONLY                                   
02277          PERFORM 8400-ADD-TO-EXHIBIT THRU 8400-EXIT               
02278          SUBTRACT INFORCE-DOLLARS FROM SAVE-BGN-INFORCE.          
02279                                                                   
02280      IF LEVEL-LIFE                                                
02281          MOVE +2                 TO IY                            
02282      ELSE                                                         
02283          MOVE +1                 TO IY.                           
02284                                                                   
02285      IF OB-COVERAGE                                               
02286          MOVE +3                 TO IY.                           
02287                                                                   
02288      IF INDIVIDUAL-INS                                            
02289          ADD +5 TO IY.                                            
02290                                                                   
02291      IF CR-LF-STATUS-AT-CANCEL NOT = SPACES                       
02292          ADD COUNT-WORK TO CANCEL-CNT (CL  TG  IY).               
02293                                                                   
02294      ADD CR-LFRFND TO CANCEL-PREM (CL  TG  IY).                   
02295      IF NOT CURRENT-CANCEL-ONLY                                   
02296         ADD DOLLAR-AMT TO CANCEL-AMT (CL  TG  IY).                
02297                                                                   
02298      MOVE COUNT-SAVE             TO COUNT-WORK.                   
02299                                                                   
02300  4730-TOT-AH-CANCEL.                                              
02301      IF LUMP-SUM                                                  
02302          GO TO 4900-PROCESS-DISABILITY.                           
02303                                                                   
02304      IF (CANCEL-EXIT-DATE NOT = ZERO AND LOW-VALUES) AND          
02305         (CANCEL-EXIT-DATE NOT GREATER PERIOD-END) AND             
02306         (CANCEL-EXIT-DATE GREATER PERIOD-START)                   
02307           NEXT SENTENCE                                           
02308        ELSE                                                       
02309          GO TO 4750-COMPUTE-CLAIM.                                
02310                                                                   
02311      IF OB-COVERAGE                                               
02312          MOVE +5                 TO IY                            
02313      ELSE                                                         
02314          MOVE +4                 TO IY.                           
02315                                                                   
02316      IF INDIVIDUAL-INS                                            
02317          ADD +5 TO IY.                                            
02318                                                                   
02319      IF SUMMARY-REC                                               
02320          MOVE CR-SUM-CAN-CNT-YTD TO COUNT-WORK.                   
02321                                                                   
02322                                                                   
02323                                                                   
02324      IF CURRENT-CANCEL-ONLY                                       
02325          GO TO 4900-PROCESS-DISABILITY.                           
02326      IF CLAIM-EXIT-DATE NOT = LOW-VALUES                          
02327          GO TO 4750-COMPUTE-CLAIM.                                
02328      IF CR-DTHAMT-YTD NOT = ZERO                                  
02329          GO TO 4750-COMPUTE-CLAIM.                                
02330      IF CERT-ENTRY-DATE GREATER THAN PERIOD-START                 
02331          GO TO 4820-COMPUTE-DECREASE.                             
02332      IF CERT-EXPIRE-DATE NOT GREATER THAN PERIOD-START            
02333          GO TO 4800-COMPUTE-INCREASE.                             
02334                                                                   
02335      GO TO 4820-COMPUTE-DECREASE.                                 
02336                                                                   
02337  4750-COMPUTE-CLAIM.                                              
02338      IF CR-LFTYP = ZERO                                           
02339          GO TO 4820-COMPUTE-DECREASE.                             
02340                                                                   
02341      IF LUMP-SUM                                                  
02342          GO TO 4820-COMPUTE-DECREASE.                             
02343                                                                   
02344      IF CLAIM-EXIT-DATE GREATER THAN PERIOD-END                   
02345          GO TO 4770-COMPUTE-EXPIRE.                               
02346                                                                   
02347      IF (CLAIM-EXIT-DATE = ZERO OR LOW-VALUES OR SPACES) AND      
02348          CR-DTHAMT-YTD = ZERO                                     
02349              GO TO 4770-COMPUTE-EXPIRE.                           
02350                                                                   
02351      MOVE CR-DTHAMT-YTD          TO DECREASING-DOLLAR.            
02352      MOVE CR-DTHAMT-YTD          TO PRT-DEATH-AMT-LF.             
02353      MOVE '1'                    TO LF-DEATH.                     
02354                                                                   
02355      IF LEVEL-LIFE                                                
02356          MOVE +2                 TO IY                            
02357      ELSE                                                         
02358          MOVE +1                 TO IY.                           
02359                                                                   
02360      IF OB-COVERAGE                                               
02361          MOVE +3                 TO IY.                           
02362                                                                   
02363      IF INDIVIDUAL-INS                                            
02364          ADD +5 TO IY.                                            
02365                                                                   
02366      ADD COUNT-WORK TO CLAIM-CNT (CL  TG  IY).                    
02367      ADD CR-DTHAMT-YTD TO CLAIM-AMT (CL TG IY).                   
02368      MOVE CR-DTHAMT-YTD TO INFORCE-DOLLARS.                       
02369                                                                   
02370      IF OB-COVERAGE                                               
02371          MOVE +5                 TO IY                            
02372      ELSE                                                         
02373          MOVE +4                 TO IY.                           
02374                                                                   
02375      IF INDIVIDUAL-INS                                            
02376          ADD +5 TO IY.                                            
02377                                                                   
02378      MOVE +4                     TO EG.                           
02379      PERFORM 8400-ADD-TO-EXHIBIT THRU 8400-EXIT                   
02380                                                                   
02381      IF CURRENT-CLAIM-ONLY                                        
02382          GO TO 4900-PROCESS-DISABILITY.                           
02383                                                                   
02384      IF OB-COVERAGE                                               
02385          GO TO 4770-COMPUTE-EXPIRE.                               
02386                                                                   
02387      SUBTRACT INFORCE-DOLLARS FROM SAVE-BGN-INFORCE.              
02388                                                                   
02389      IF CERT-EXPIRE-DATE NOT GREATER THAN PERIOD-START AND        
02390         NOT LEVEL-LIFE                                            
02391          GO TO 4800-COMPUTE-INCREASE.                             
02392                                                                   
02393      IF NOT LEVEL-LIFE                                            
02394          GO TO 4820-COMPUTE-DECREASE.                             
02395                                                                   
02396      COMPUTE DOLLAR-AMT = CR-LFAMT - CR-DTHAMT-YTD.               
02397      MOVE '1'                    TO LF-DECR.                      
02398      MOVE DOLLAR-AMT             TO PRT-DECR-AMT-LF.              
02399                                                                   
02400      IF GROUP-INS                                                 
02401          ADD DOLLAR-AMT TO GROUP-AMT (CL  TG  7)                  
02402          ADD DOLLAR-AMT TO GROUP-AMT (CL  TG  8)                  
02403      ELSE                                                         
02404          ADD DOLLAR-AMT TO IND-AMT (CL  TG  7)                    
02405          ADD DOLLAR-AMT TO IND-AMT (CL  TG  8).                   
02406      SUBTRACT DOLLAR-AMT FROM SAVE-BGN-INFORCE.                   
02407                                                                   
02408      IF CERT-EXPIRE-DATE GREATER THAN PERIOD-START                
02409          GO TO 4900-PROCESS-DISABILITY                            
02410      ELSE                                                         
02411          MOVE CR-LFAMT           TO DECREASING-DOLLAR             
02412          GO TO 4800-COMPUTE-INCREASE.                             
02413                                                                   
02414  4770-COMPUTE-EXPIRE.                                             
02415      IF CR-ENTRY-STATUS = '3' OR '5'                              
02416          GO TO 4900-PROCESS-DISABILITY.                           
02417                                                                   
02418      IF CERT-EXPIRE-DATE GREATER THAN PERIOD-END                  
02419          GO TO 4820-COMPUTE-DECREASE.                             
02420                                                                   
02421      IF CR-LFTYP = ZERO                                           
02422          GO TO 4900-PROCESS-DISABILITY.                           
02423                                                                   
02424      IF (CLAS-I-EP (CLAS-INDEXL) = 'B'  AND                       
02425          CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L'  AND            
02426          LF-BAL-REMTRM NOT GREATER THAN ZERO)                     
02427                        OR                                         
02428         (CLAS-I-EP (CLAS-INDEXL) = 'B'  AND                       
02429          CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'L'  AND                
02430          LF-REM-TRM NOT GREATER THAN ZERO)                        
02431                        OR                                         
02432         (CLAS-I-EP (CLAS-INDEXL) NOT = 'B'  AND                   
02433          LF-REM-TRM NOT GREATER ZERO)                             
02434            NEXT SENTENCE                                          
02435          ELSE                                                     
02436          GO TO 4900-PROCESS-DISABILITY.                           
02437                                                                   
02438      IF SUMMARY-REC                                               
02439         COMPUTE COUNT-WORK = CR-LIVES - CR-SUM-CAN-CNT-ITD        
02440         IF CR-LFPRM NOT GREATER CR-LFRFND                         
02441            MOVE ZERO TO INFORCE-DOLLARS                           
02442          ELSE                                                     
02443           IF CR-LFRFND = ZERO                                     
02444              MOVE CR-LFAMT TO INFORCE-DOLLARS                     
02445            ELSE                                                   
02446                 COMPUTE TEMP-RESULT-L ROUNDED =                   
02447                                      CR-LFRFND / CR-LFPRM         
02448                 COMPUTE DOLLAR-AMT ROUNDED = CR-LFAMT -           
02449                                     (CR-LFAMT * TEMP-RESULT-L).   
02450                                                                   
02451      IF EFFECTIVE-BOTH-PER                                        
02452          PERFORM 8300-STARTING-INFORCE THRU 8300-EXIT             
02453          MOVE INFORCE-DOLLARS    TO DOLLAR-AMT                    
02454      ELSE                                                         
02455          MOVE CR-LFAMT           TO DOLLAR-AMT.                   
02456                                                                   
02457      IF OB-COVERAGE AND CR-LFTYP NOT = ZERO                       
02458          MOVE CR-LFAMT           TO DOLLAR-AMT.                   
02459                                                                   
02460      MOVE '1'                    TO LF-EXPIRY.                    
02461      MOVE DOLLAR-AMT             TO PRT-EXPIRY-AMT-LF.            
02462      MOVE DOLLAR-AMT             TO INFORCE-DOLLARS.              
02463      MOVE +5                     TO EG.                           
02464      PERFORM 8400-ADD-TO-EXHIBIT THRU 8400-EXIT.                  
02465                                                                   
02466      IF NOT OB-COVERAGE                                           
02467          SUBTRACT INFORCE-DOLLARS FROM SAVE-BGN-INFORCE.          
02468                                                                   
02469      GO TO 4900-PROCESS-DISABILITY.                               
02470                                                                   
02471  4800-COMPUTE-INCREASE.                                           
02472      IF CR-ENTRY-STATUS = '3' OR '5'                              
02473          GO TO 4900-PROCESS-DISABILITY.                           
02474                                                                   
02475      IF NOT EFFECTIVE-BOTH-PER                                    
02476          GO TO 4900-PROCESS-DISABILITY.                           
02477                                                                   
02478      IF CR-LFTYP = ZERO                                           
02479          GO TO 4900-PROCESS-DISABILITY.                           
02480                                                                   
02481      IF OB-COVERAGE OR SUMMARY-REC                                
02482          GO TO 4900-PROCESS-DISABILITY.                           
02483                                                                   
02484      COMPUTE MINUS-AMT = DECREASING-DOLLAR * -1.                  
02485      MOVE '1'                    TO LF-INCR.                      
02486      MOVE MINUS-AMT              TO PRT-INCR-AMT-LF.              
02487                                                                   
02488      IF GROUP-INS                                                 
02489          ADD MINUS-AMT TO GROUP-AMT (CL  TG  7)                   
02490          ADD MINUS-AMT TO GROUP-AMT (CL  TG  8)                   
02491      ELSE                                                         
02492          ADD MINUS-AMT TO IND-AMT (CL  TG  7)                     
02493          ADD MINUS-AMT TO IND-AMT (CL  TG  8).                    
02494      ADD DECREASING-DOLLAR TO SAVE-BGN-INFORCE.                   
02495                                                                   
02496      GO TO 4900-PROCESS-DISABILITY.                               
02497                                                                   
02498  4820-COMPUTE-DECREASE.                                           
02499      IF CR-ENTRY-STATUS = '3' OR '5'                              
02500          GO TO 4900-PROCESS-DISABILITY.                           
02501                                                                   
02502      IF CR-LFTYP = ZERO                                           
02503          GO TO 4900-PROCESS-DISABILITY.                           
02504                                                                   
02505      IF LEVEL-LIFE                                                
02506          GO TO 4900-PROCESS-DISABILITY.                           
02507                                                                   
02508      IF OB-COVERAGE OR SUMMARY-REC                                
02509          GO TO 4900-PROCESS-DISABILITY.                           
02510                                                                   
02511      IF NOT EFFECTIVE-BOTH-PER                                    
02512          SUBTRACT DECREASING-DOLLAR FROM CR-LFAMT                 
02513              GIVING DOLLAR-AMT.                                   

02514      IF EFFECTIVE-BOTH-PER
02515          PERFORM 8300-STARTING-INFORCE THRU 8300-EXIT             
02516          MOVE INFORCE-DOLLARS    TO DOLLAR-AMT                    
02517          SUBTRACT DECREASING-DOLLAR FROM DOLLAR-AMT.              
02518                                                                   
02519      MOVE '1'                    TO LF-DECR.                      
02520      MOVE DOLLAR-AMT             TO PRT-DECR-AMT-LF.              
02521                                                                   
02522      IF GROUP-INS                                                 
02523          ADD DOLLAR-AMT TO GROUP-AMT (CL  TG  7)                  
02524          ADD DOLLAR-AMT TO GROUP-AMT (CL  TG  8)                  
02525      ELSE                                                         
02526          ADD DOLLAR-AMT TO IND-AMT (CL  TG  7)                    
02527          ADD DOLLAR-AMT TO IND-AMT (CL  TG  8).                   
02528      SUBTRACT DOLLAR-AMT FROM SAVE-BGN-INFORCE.                   
02529                                                                   
02530  4900-PROCESS-DISABILITY.                                         
02531                                                                   
02532      IF CR-AHTYP = ZERO                                           
02533          GO TO 4999-PRINT.                                        
02534                                                                   
02535      MOVE ZERO                   TO CANCEL-SW.                    
02536      MOVE ZERO                   TO CLAIM-SW.                     
02537                                                                   
02538      IF CR-AH-CURRENT-STATUS NOT = 6 AND 7 AND 8                  
02539          MOVE 0                  TO CERT-STATUS.                  
02540      IF CR-AH-CURRENT-STATUS = 7                                  
02541          MOVE 2                  TO CERT-STATUS.                  
02542      IF CR-AH-CURRENT-STATUS = 8                                  
02543          MOVE 1                  TO CERT-STATUS.                  
02544      IF CR-AH-CURRENT-STATUS = 6                                  
02545          MOVE 3                  TO CERT-STATUS.                  
02546                                                                   
02547      MOVE CERT-EFFECT-DT             TO CP-CERT-EFF-DT.           
02548      MOVE FIRST-PAY-DT               TO CP-FIRST-PAY-DATE.        
02549      MOVE CR-AH-TERM                 TO CP-ORIGINAL-TERM.         
02550      MOVE CLAS-I-RL-AH (CLAS-INDEXL) TO CP-BENEFIT-TYPE.          
02551      MOVE CLAS-I-BAL (CLAS-INDEXL)   TO CP-SPECIAL-CALC-CD.       
02552      MOVE '3'                        TO CP-PROCESS-TYPE.          
02553      MOVE DTE-CLASIC-COMPANY-CD      TO CP-COMPANY-CD.            
02554      MOVE DTE-REM-TRM-CALC-OPTION    TO CP-REM-TRM-CALC-OPTION.   
02555      MOVE ZEROS TO  CP-TERM-OR-EXT-DAYS.                          
02556                                                                   
02557  AH-REM-TERM.                                                     
02558                                                                   
02559      PERFORM 4585-GET-REMAINING-TERM THRU 4585-EXIT.              
02560                                                                   
02561      MOVE CP-REMAINING-TERM-2 TO AH-REM-TRM.                      
02562                                                                   
02563      IF REINSURANCE-PASS OR REINSURANCE-REC                       
02564          MOVE CR-AH-EXPIRE-DATE TO DC-GREG-DATE-CYMD              
02565          MOVE 'L' TO DC-OPTION-CODE                               
02566          PERFORM 8500-DATE-CONVERT-ROUTINE THRU                   
02567                  8599-DATE-CONVERT-X                              
02568          MOVE DC-BIN-DATE-1 TO CERT-EXPIRE-DATE                   
02569          GO TO 4910-SKIP-CALC.                                    
02570                                                                   
02571      MOVE FIRST-PAY-DT            TO DC-BIN-DATE-1.               
02572      COMPUTE DC-ELAPSED-MONTHS = CR-AH-TERM - +1.                 
02573      MOVE '6'                     TO DC-OPTION-CODE.              
02574      PERFORM 8500-DATE-CONVERT-ROUTINE THRU                       
02575                  8599-DATE-CONVERT-X.                             
02576      IF NO-CONVERSION-ERROR                                       
02577          MOVE DC-BIN-DATE-2        TO CERT-EXPIRE-DATE            
02578      ELSE                                                         
02579          MOVE LOW-VALUES           TO CERT-EXPIRE-DATE.           
02580                                                                   
02581      IF (CERT-EXPIRE-DATE = LOW-VALUES)     OR                    
02582          (DTE-REM-TRM-CALC-OPTION = '1' OR '2')                   
02583         MOVE CERT-EFFECT-DT          TO DC-BIN-DATE-1             
02584         MOVE CR-AH-TERM              TO DC-ELAPSED-MONTHS         
02585         MOVE ZEROS                   TO DC-ELAPSED-DAYS           
02586         MOVE '6'                     TO DC-OPTION-CODE            
02587         PERFORM 8500-DATE-CONVERT-ROUTINE THRU                    
02588                 8599-DATE-CONVERT-X                               
02589         IF NO-CONVERSION-ERROR                                    
02590            MOVE DC-BIN-DATE-2        TO CERT-EXPIRE-DATE          
02591         ELSE                                                      
02592            MOVE LOW-VALUES           TO CERT-EXPIRE-DATE.         
02593                                                                   
02594      IF CP-REMAINING-TERM-2 NOT GREATER THAN ZEROS                
02595          IF CERT-EXPIRE-DATE GREATER THAN PERIOD-END              
02596              MOVE PERIOD-END  TO  CERT-EXPIRE-DATE.               
02597                                                                   
02598      MOVE CERT-EXPIRE-DATE TO DC-BIN-DATE-1.                      
02599      MOVE ' '                TO  DC-OPTION-CODE.                  
02600      PERFORM 8500-DATE-CONVERT-ROUTINE THRU 8599-DATE-CONVERT-X.  
02601      MOVE DC-GREG-DATE-CYMD TO WS-CR-AH-EXPIRE-DATE               
02602                                CR-AH-EXPIRE-DATE.                 
02603                                                                   
02604  4910-SKIP-CALC.                                                  
02605                                                                   
02606      MOVE LOW-VALUES             TO LOW-EXIT-DATE.                
02607      MOVE CR-AH-SETTLEMENT-EXIT-DATE TO DC-GREG-DATE-CYMD.        
02608      MOVE 'L'                TO  DC-OPTION-CODE.                  
02609      PERFORM 8500-DATE-CONVERT-ROUTINE THRU 8599-DATE-CONVERT-X.  
02610      MOVE DC-BIN-DATE-1  TO  CLAIM-EXIT-DATE.                     
02611                                                                   
02612      MOVE CR-AH-CANCEL-EXIT-DATE  TO DC-GREG-DATE-CYMD.           
02613      MOVE 'L'                TO  DC-OPTION-CODE.                  
02614      PERFORM 8500-DATE-CONVERT-ROUTINE THRU 8599-DATE-CONVERT-X.  
02615      MOVE DC-BIN-DATE-1  TO  CANCEL-EXIT-DATE.                    
02616                                                                   
02617      MOVE CR-AH-CANC-DT      TO DC-GREG-DATE-CYMD.                
02618      MOVE 'L'                TO  DC-OPTION-CODE.                  
02619      PERFORM 8500-DATE-CONVERT-ROUTINE THRU 8599-DATE-CONVERT-X.  
02620      MOVE DC-BIN-DATE-1  TO  SUM-CANCEL-DATE.                     
02621                                                                   
02622      MOVE LOW-VALUES             TO LOW-EXIT-DATE.                
02623                                                                   
02624      IF CLAIM-EXIT-DATE = SPACES OR ZEROS                         
02625          MOVE LOW-VALUES TO CLAIM-EXIT-DATE.                      
02626                                                                   
02627      IF CANCEL-EXIT-DATE = SPACES OR ZEROS                        
02628          MOVE LOW-VALUES TO CANCEL-EXIT-DATE.                     
02629                                                                   
02630      IF CLAIM-EXIT-DATE = LOW-VALUES AND                          
02631         CANCEL-EXIT-DATE = LOW-VALUES                             
02632          GO TO 4920-SET-SUBSCRIPTS.                               
02633                                                                   
02634      IF CLAIM-EXIT-DATE = LOW-VALUES                              
02635          MOVE CANCEL-EXIT-DATE   TO LOW-EXIT-DATE                 
02636          GO TO 4920-SET-SUBSCRIPTS.                               
02637                                                                   
02638      IF CANCEL-EXIT-DATE = LOW-VALUES                             
02639          MOVE CLAIM-EXIT-DATE    TO LOW-EXIT-DATE                 
02640          GO TO 4920-SET-SUBSCRIPTS.                               
02641                                                                   
02642      IF CLAIM-EXIT-DATE LESS THAN CANCEL-EXIT-DATE                
02643          MOVE CLAIM-EXIT-DATE    TO LOW-EXIT-DATE                 
02644      ELSE                                                         
02645          MOVE CANCEL-EXIT-DATE   TO LOW-EXIT-DATE.                
02646                                                                   
02647  4920-SET-SUBSCRIPTS.                                             
02648                                                                   
02649      IF CR-AH-TERM LESS THAN +121                                 
02650          MOVE +2                 TO TG                            
02651      ELSE                                                         
02652          MOVE +3                 TO TG.                           
02653                                                                   
02654      IF CR-AH-TERM LESS THAN +61                                  
02655          MOVE +1                 TO TG.                           
02656                                                                   
02657      IF OB-COVERAGE                                               
02658          MOVE +5                 TO IY                            
02659      ELSE                                                         
02660          MOVE +4                 TO IY.                           
02661                                                                   
02662      IF INDIVIDUAL-INS                                            
02663          ADD +5 TO IY.                                            
02664                                                                   
02665      IF NOT EFFECTIVE-BOTH-PER                                    
02666          GO TO 4925-CHECK-THIS-PER-ITEM.                          
02667                                                                   
02668      IF CLAIM-EXIT-DATE NOT = LOW-VALUES AND                      
02669         CANCEL-EXIT-DATE NOT = LOW-VALUES                         
02670          IF CLAIM-EXIT-DATE NOT GREATER THAN PERIOD-START AND     
02671             CANCEL-EXIT-DATE GREATER THAN PERIOD-START            
02672              MOVE 1              TO CANCEL-SW                     
02673              MOVE 0              TO COUNT-WORK                    
02674              GO TO 4970-CHECK-CANCEL                              
02675          ELSE                                                     
02676              IF CANCEL-EXIT-DATE NOT GREATER THAN PERIOD-START    
02677              AND                                                  
02678              CLAIM-EXIT-DATE GREATER THAN PERIOD-START            
02679               MOVE 1             TO CLAIM-SW                      
02680               MOVE 0             TO COUNT-WORK                    
02681               GO TO 4980-CHECK-CLAIM.                             
02682                                                                   
02683      IF CLAIM-OR-CANCEL AND                                       
02684         LOW-EXIT-DATE NOT GREATER THAN PERIOD-START AND           
02685             CR-DISAMT-YTD NOT = ZERO                              
02686                 GO TO 4980-CHECK-CLAIM.                           
02687                                                                   
02688      IF CLAIM-OR-CANCEL AND                                       
02689         LOW-EXIT-DATE NOT GREATER THAN PERIOD-START               
02690          GO TO 4999-PRINT.                                        
02691                                                                   
02692      IF (CERT-EXPIRE-DATE NOT GREATER THAN PERIOD-START) AND      
02693         NOT CLAIM-OR-CANCEL                                       
02694             GO TO 4980-CHECK-CLAIM.                               
02695                                                                   
02696      MOVE '1'                    TO OLD-IN-EFFECT.                
02697                                                                   
02698  4925-CHECK-THIS-PER-ITEM.                                        
02699                                                                   
02700      IF CLAIM-OR-CANCEL AND                                       
02701         LOW-EXIT-DATE NOT GREATER THAN PERIOD-END                 
02702          GO TO 4930-COMPUTE-UNEARNED.                             
02703                                                                   
02704      IF CERT-EXPIRE-DATE NOT GREATER THAN PERIOD-END              
02705          GO TO 4930-COMPUTE-UNEARNED.                             
02706                                                                   
02707      MOVE 1                      TO NEW-IN-EFFECT.                
02708                                                                   
02709  4930-COMPUTE-UNEARNED.                                           
02710      IF CR-ENTRY-STATUS = ('3' OR '5')                            
02711          GO TO 4950-CHECK-AH-ISSUE.                               
02712                                                                   
02713      IF SUMMARY-REC                                               
02714          IF CR-AHPRM IS NOT GREATER THAN CR-AHRFND                
02715              MOVE ZERO           TO  CR-AHPRM                     
02716                                      CR-AHRFND.                   
02717      IF OB-COVERAGE                                               
02718          GO TO 4950-CHECK-AH-ISSUE.                               
02719                                                                   
02720      MOVE CR-DT                      TO DC-GREG-DATE-CYMD.        
02721      MOVE 'L'                        TO DC-OPTION-CODE.           
02722      PERFORM 8500-DATE-CONVERT-ROUTINE THRU 8599-DATE-CONVERT-X.  
02723      MOVE DC-BIN-DATE-1              TO CP-CERT-EFF-DT.           
02724                                                                   
02725      IF CR-LOAN-1ST-PMT-DT NOT NUMERIC                            
02726          MOVE ZEROS                  TO CR-LOAN-1ST-PMT-DT.       
02727                                                                   
02728      IF CR-LOAN-1ST-PMT-DT NOT = ZEROS                            
02729          MOVE CR-LOAN-1ST-PMT-DT     TO DC-GREG-DATE-1-YMD        
02730          MOVE ZEROS                  TO DC-ELAPSED-MONTHS         
02731                                         DC-ELAPSED-DAYS           
02732          MOVE '3'                    TO DC-OPTION-CODE            
02733          PERFORM 8500-DATE-CONVERT-ROUTINE THRU                   
02734                  8599-DATE-CONVERT-X                              
02735          IF NO-CONVERSION-ERROR                                   
02736              MOVE DC-BIN-DATE-1      TO CP-FIRST-PAY-DATE         
02737          ELSE                                                     
02738              MOVE ZEROS              TO CR-LOAN-1ST-PMT-DT.       
02739                                                                   
02740      IF CR-LOAN-1ST-PMT-DT = ZEROS                                
02741          MOVE CP-CERT-EFF-DT         TO DC-BIN-DATE-1             
02742          MOVE +1                     TO DC-ELAPSED-MONTHS         
02743          MOVE ZEROS                  TO DC-ELAPSED-DAYS           
02744          MOVE '6'                    TO DC-OPTION-CODE            
02745          PERFORM 8500-DATE-CONVERT-ROUTINE THRU                   
02746                  8599-DATE-CONVERT-X                              
02747          MOVE DC-BIN-DATE-1          TO CP-FIRST-PAY-DATE.        
02748                                                                   
02749      IF ENTERED-PREV-PERIOD                                       
02750          MOVE RUN-DATE-BIN       TO CP-VALUATION-DT               
02751          MOVE +1                 TO FACTOR-C                      
02752          PERFORM 4940-UNEARNED-CALC THRU 4940-EXIT.               
02753                                                                   
02754      IF ENTERED-PREV-PERIOD                                       
02755          IF CERT-CANCELLED                                        
02756              IF CANCEL-EXIT-DATE IS NOT GREATER THAN PERIOD-END   
02757                  GO TO 4970-CHECK-CANCEL.                         
02758                                                                   
02759      IF ENTERED-THIS-PERIOD                                       
02760          IF CERT-CANCELLED                                        
02761              IF CANCEL-EXIT-DATE IS NOT GREATER THAN PERIOD-END   
02762                  GO TO 4950-CHECK-AH-ISSUE.                       
02763                                                                   
02764      IF ENTERED-THIS-PERIOD                                       
02765          MOVE EP-DT-BIN          TO CP-VALUATION-DT               
02766          MOVE -1                 TO FACTOR-C                      
02767          PERFORM 4940-UNEARNED-CALC THRU 4940-EXIT.               
02768                                                                   
02769      GO TO 4950-CHECK-AH-ISSUE.                                   
02770                                                                   
02771  4940-UNEARNED-CALC.                                              
02772                                                                   
02773      MOVE SPACES                     TO CP-ACCT-FLD-5.            
02774      MOVE DTE-CLIENT                 TO CP-COMPANY-ID.            
02775      MOVE CR-AH-TERM                 TO CP-ORIGINAL-TERM.         
02776      MOVE DTE-REM-TRM                TO CP-REM-TERM-METHOD.       
02777      MOVE '3'                        TO CP-PROCESS-TYPE.          
02778      MOVE DTE-CLASIC-COMPANY-CD      TO CP-COMPANY-CD.            
02779      MOVE DTE-REM-TRM-CALC-OPTION    TO CP-REM-TRM-CALC-OPTION.   
02780                                                                   
02781  4940-GET-REMAINING-TERM.                                         
02782                                                                   
02783      PERFORM 4585-GET-REMAINING-TERM THRU 4585-EXIT.              
02784                                                                   
02785      MOVE CP-REMAINING-TERM-1        TO UNEXPIRED-MONTHS.         
02786      COMPUTE EXPIRED-MONTHS = CR-AH-TERM - UNEXPIRED-MONTHS.      
02787                                                                   
02788  4940-TERM-OK.                                                    
02789      IF UNEXPIRED-MONTHS NOT GREATER ZERO                         
02790          GO TO 4940-EXIT.                                         
02791                                                                   
02792      IF SUMMARY-REC                                               
02793          IF CR-AHRFND = +0                                        
02794              MOVE CR-AHPRM       TO CP-ORIGINAL-PREMIUM           
02795          ELSE                                                     
02796              COMPUTE CP-ORIGINAL-PREMIUM ROUNDED =                
02797                  CR-AHPRM - CR-AHRFND                             
02798      ELSE                                                         
02799          MOVE CR-AHPRM           TO CP-ORIGINAL-PREMIUM.          
02800                                                                   
02801      MOVE CR-AH-TERM             TO CP-ORIGINAL-TERM              
02802      MOVE CR-AHAMT               TO CP-ORIGINAL-BENEFIT           
02803                                     CP-RATING-BENEFIT-AMT.        
02804      MOVE UNEXPIRED-MONTHS       TO CP-REMAINING-TERM             
02805                                                                   
02806      IF AH-EARN-METHOD = SPACES                                   
02807          MOVE CLAS-I-EP (CLAS-INDEXA) TO  CP-EARNING-METHOD       
02808      ELSE                                                         
02809          MOVE AH-EARN-METHOD          TO  CP-EARNING-METHOD.      
02810                                                                   
02811      MOVE CLAS-I-RL-AH (CLAS-INDEXA)                              
02812                                  TO CP-BENEFIT-TYPE               
02813      MOVE ZEROS                  TO CP-ALTERNATE-PREMIUM          
02814      MOVE CLAS-I-CALC-TYPE (CLAS-INDEXA)                          
02815                                  TO CP-SPECIAL-CALC-CD            
02816      MOVE AM-AH-DEVIATION        TO CP-DEVIATION-CODE             
02817      MOVE CR-AHTYP               TO CP-BENEFIT-CD                 
02818      MOVE AH-OVERRIDE-L1         TO CP-AH-OVERRIDE-CODE           
02819                                                                   
02820      IF CR-PMT-EXTENSION-DAYS NOT NUMERIC                         
02821         MOVE ZEROS               TO CP-TERM-OR-EXT-DAYS           
02822      ELSE                                                         
02823         MOVE CR-PMT-EXTENSION-DAYS                                
02824                                  TO CP-TERM-OR-EXT-DAYS.          
02825                                                                   
02826      PERFORM 4945-CALC-UNEARNED-PREMIUM THRU 4945-AH-EXIT.        
02827                                                                   
02828      IF CP-ERROR-OCCURED                                          
02829         GO TO 4945-ERROR-AH.                                      
02830                                                                   
02831      MOVE CP-STATE-U-PRM    TO UNEARNED-AMT-AH.                   
02832                                                                   
02833      IF (STATE-ABBR (CLAS-INDEXS) = 'VA')      AND                
02834         (CR-AH-TERM GREATER THAN +61)  AND                        
02835         (CR-DT GREATER THAN  19921231)                            
02836           MOVE  'A'  TO  AH-EARN-METHOD.                          
02837                                                                   
02838      IF (CLAS-I-CALC-TYPE (CLAS-INDEXA) NOT = 'B' AND 'Z') AND    
02839         (AH-EARN-METHOD = 'A' OR 'C')                             
02840          PERFORM 8450-RE-RATE-ROUTINE THRU 8499-EXIT              
02841          IF (CP-ERROR-RATE-NOT-FOUND OR                           
02842              CP-ERROR-RATE-IS-ZERO OR                             
02843              CP-ERROR-IN-DATES)                                   
02844              NEXT SENTENCE                                        
02845          ELSE                                                     
02846              COMPUTE UNEARNED-AMT-AH ROUNDED =                    
02847               ((UNEXPIRED-MONTHS * CR-AHAMT) / +100) *            
02848                 CP-PREMIUM-RATE.                                  
02849                                                                   
02850      COMPUTE UNEARNED-AMT-AH ROUNDED =                            
02851                            UNEARNED-AMT-AH * FACTOR-C.            
02852                                                                   
02853      IF GROUP-INS                                                 
02854          ADD UNEARNED-AMT-AH TO EARN-PREM (CL  TG  4)             
02855      ELSE                                                         
02856          ADD UNEARNED-AMT-AH TO EARN-PREM (CL  TG  9).            
02857  4940-EXIT.                                                       
02858      EXIT.                                                        
02859                                                                   
02860  4945-CALC-UNEARNED-PREMIUM.                                      
02861                                                                   
02862      MOVE CR-DT TO DC-GREG-DATE-CYMD.                             
02863      MOVE 'L' TO DC-OPTION-CODE.                                  
02864      PERFORM 8500-DATE-CONVERT-ROUTINE THRU                       
02865                   8599-DATE-CONVERT-X.                            
02866      MOVE DC-BIN-DATE-1 TO CP-CERT-EFF-DT                         
02867      MOVE DTE-R78 TO CP-R78-OPTION                                
02868      MOVE SPACES TO CP-ACCT-FLD-5                                 
02869      MOVE DTE-CLIENT TO CP-COMPANY-ID                             
02870      IF CR-AGE NOT NUMERIC                                        
02871         MOVE +0 TO CR-AGE.                                        
02872      MOVE CR-AGE TO CP-ISSUE-AGE                                  
02873      IF CR-APR NOT NUMERIC                                        
02874         MOVE +0 TO CR-APR.                                        
02875      MOVE CR-APR TO CP-LOAN-APR                                   
02876                                                                   
02877      IF CR-PMT-FREQ IS NOT NUMERIC                                
02878          MOVE ZEROS              TO  CP-PAY-FREQUENCY             
02879      ELSE                                                         
02880          MOVE CR-PMT-FREQ        TO  CP-PAY-FREQUENCY.            
02881                                                                   
02882      IF CR-LOAN-TERM NOT NUMERIC                                  
02883         MOVE +0 TO CR-LOAN-TERM.                                  
02884      MOVE CR-LOAN-TERM TO CP-LOAN-TERM                            
02885      MOVE SPACES         TO CP-DOMICILE-STATE                     
02886      MOVE CR-CARRIER TO CP-CARRIER                                
02887      IF CR-RATING-CLASS NOT = SPACES AND ZERO                     
02888         MOVE CR-RATING-CLASS TO CP-CLASS-CODE                     
02889      ELSE                                                         
02890         MOVE AM-CAL-TABLE TO CP-CLASS-CODE.                       
02891                                                                   
02892      MOVE STATE-SUB (CLAS-INDEXS) TO CP-STATE                     
02893      MOVE STATE-ABBR (CLAS-INDEXS) TO CP-STATE-STD-ABBRV          
02894      MOVE DTE-CLASIC-COMPANY-CD TO CP-COMPANY-CD                  
02895                                                                   
02896      CALL 'ELUPRMX'  USING CALCULATION-PASS-AREA.                 
02897                                                                   
02898  4945-AH-EXIT.                                                    
02899      EXIT.                                                        
02900                                                                   
02901      EJECT                                                        
02902  4945-ERROR-AH.                                                   
02903                                                                   
02904      DISPLAY 'ERROR OCCURED IN AH UNEARNED ROUTINE'               
02905      DISPLAY 'CP RETURN CODE ...' CP-RETURN-CODE.                 
02906      GO TO ABEND-PGM.                                             
02907                                                                   
02908  4950-CHECK-AH-ISSUE.                                             
02909                                                                   
02910      IF EFFECTIVE-BOTH-PER                                        
02911          GO TO 4970-CHECK-CANCEL.                                 
02912                                                                   
02913      IF CR-ENTRY-STATUS = '3' OR '5'                              
02914          GO TO 4970-CHECK-CANCEL.                                 
02915                                                                   
02916      IF SUMMARY-REC                                               
02917          MOVE CR-LIVES           TO COUNT-WORK.                   
02918                                                                   
02919      MOVE '1'                    TO AH-ISSUE.                     
02920      ADD COUNT-WORK TO ISSUE-CNT (CL  TG  IY).                    
02921      ADD CR-AHPRM TO EARN-PREM (CL  TG  IY).                      
02922      ADD CR-AHPRM TO ISSUE-PREM (CL  TG  IY).                     
02923                                                                   
02924  4970-CHECK-CANCEL.                                               
02925      IF SUMMARY-REC AND                                           
02926         (SUM-CANCEL-DATE NOT GREATER PERIOD-END) AND              
02927         (SUM-CANCEL-DATE GREATER PERIOD-START) AND                
02928         (CR-SUM-CAN-CNT-YTD NOT = ZERO)                           
02929          NEXT SENTENCE                                            
02930        ELSE                                                       
02931         IF LUMP-SUM AND                                           
02932            (CLAIM-EXIT-DATE NOT GREATER PERIOD-END) AND           
02933            (CLAIM-EXIT-DATE NOT LESS PERIOD-START)                
02934             NEXT SENTENCE                                         
02935          ELSE                                                     
02936             IF (CANCEL-EXIT-DATE = ZERO OR LOW-VALUES) OR         
02937                CANCEL-EXIT-DATE GREATER THAN PERIOD-END           
02938                 GO TO 4980-CHECK-CLAIM.                           
02939                                                                   
02940      MOVE '1'                TO AH-CANC.                          
02941                                                                   
02942      MULTIPLY CR-AHRFND BY -1 GIVING UNEARNED-AMT-AH.             
02943      MOVE '1'                    TO AH-CANC.                      
02944      MOVE CR-AHRFND              TO PRT-CANC-AMT-AH.              
02945      ADD UNEARNED-AMT-AH TO EARN-PREM (CL  TG  IY).               
02946                                                                   
02947      IF CR-AH-STATUS-AT-CANCEL NOT = SPACES                       
02948          ADD COUNT-WORK TO     CANCEL-CNT (CL  TG  IY).           
02949                                                                   
02950      ADD CR-AHRFND TO     CANCEL-PREM (CL  TG  IY).               
02951                                                                   
02952  4980-CHECK-CLAIM.                                                
02953      IF CR-DISAMT-YTD = ZERO                                      
02954          GO TO 4999-PRINT.                                        
02955                                                                   
02956      MOVE '1'                    TO AH-CLAIM.                     
02957      MOVE CR-DISAMT-YTD          TO PRT-CLAIM-AMT-AH.             
02958                                                                   
02959      MOVE CR-DIS-CC              TO WS-CR-DIS-CCYY(1:2).          
02960      MOVE CR-DIS-YR              TO WS-CR-DIS-CCYY(3:2).          
02961      ADD COUNT-WORK    TO CLAIM-CNT (CL TG IY).                   
02962      ADD CR-DISAMT-YTD TO CLAIM-AMT (CL TG IY).                   
02963      COMPUTE TERM-WORK = (WS-CR-DIS-CCYY * 12) + CR-DIS-MO.       
02964                                                                   
02965      IF TERM-WORK LESS THAN PERIOD-START-9                        
02966          ADD CR-DISAMT-YTD TO AH-PAID-LAST-YR (CL)                
02967      ELSE                                                         
02968          ADD CR-DISAMT-YTD TO AH-PAID-THIS-YR (CL).               
02969                                                                   
02970      MULTIPLY CR-AHRFND BY -1 GIVING UNEARNED-AMT-AH.             
02971                                                                   
02972  4999-PRINT.                                                      
02973                                                                   
02974      IF PRT-LF-ISSUE OR                                           
02975         PRT-AH-ISSUE                                              
02976          MOVE 'ISSUE'            TO DL-ACTIVITY                   
02977          MOVE PRT-ISSUE-AMT-LF   TO DL-RSRV-AMT                   
02978          PERFORM 5100-PRINT-DETAIL THRU 5100-EXIT.                
02979                                                                   
02980      IF PRT-LF-DECR                                               
02981          MOVE 'DECR '            TO DL-ACTIVITY                   
02982          MOVE PRT-DECR-AMT-LF    TO DL-RSRV-AMT                   
02983          PERFORM 5100-PRINT-DETAIL THRU 5100-EXIT.                
02984                                                                   
02985      IF PRT-LF-CANC AND                                           
02986         PRT-AH-CANC                                               
02987              MOVE '1'                TO LF-REFUND-SW              
02988              MOVE 'CANC '            TO DL-ACTIVITY               
02989              MOVE '1'                TO AH-REFUND-SW              
02990              ADD PRT-CANC-AMT-AH     TO PRT-CANC-AMT-LF           
02991              MOVE PRT-CANC-AMT-LF    TO DL-RSRV-AMT               
02992              PERFORM 5100-PRINT-DETAIL THRU 5100-EXIT             
02993              GO TO 4999-NEXT-PRINT.                               
02994                                                                   
02995      IF PRT-LF-CANC                                               
02996              MOVE '1'                TO LF-REFUND-SW              
02997              MOVE 'CANC '            TO DL-ACTIVITY               
02998              MOVE PRT-CANC-AMT-LF    TO DL-RSRV-AMT               
02999              PERFORM 5100-PRINT-DETAIL THRU 5100-EXIT.            
03000                                                                   
03001      IF PRT-AH-CANC                                               
03002              MOVE '1'                TO AH-REFUND-SW              
03003              MOVE 'CANC '            TO DL-ACTIVITY               
03004              MOVE PRT-CANC-AMT-AH    TO DL-RSRV-AMT               
03005              PERFORM 5100-PRINT-DETAIL THRU 5100-EXIT.            
03006                                                                   
03007  4999-NEXT-PRINT.                                                 
03008      IF PRT-LF-INCR                                               
03009          MOVE 'INCR '            TO DL-ACTIVITY                   
03010          MOVE PRT-INCR-AMT-LF    TO DL-RSRV-AMT                   
03011          PERFORM 5100-PRINT-DETAIL THRU 5100-EXIT.                
03012                                                                   
03013      IF PRT-LF-EXPIRY                                             
03014          MOVE 'EXP  '            TO DL-ACTIVITY                   
03015          MOVE PRT-EXPIRY-AMT-LF   TO DL-RSRV-AMT                  
03016          PERFORM 5100-PRINT-DETAIL THRU 5100-EXIT.                
03017                                                                   
03018      IF PRT-LF-DEATH                                              
03019          MOVE 'DEATH'            TO DL-ACTIVITY                   
03020          MOVE PRT-DEATH-AMT-LF   TO DL-RSRV-AMT                   
03021          PERFORM 5100-PRINT-DETAIL THRU 5100-EXIT.                
03022                                                                   
03023      IF PRT-LF-LUMP                                               
03024          MOVE 'LUMP '            TO DL-ACTIVITY                   
03025          MOVE PRT-CANC-AMT-LF    TO DL-RSRV-AMT                   
03026          PERFORM 5100-PRINT-DETAIL THRU 5100-EXIT.                
03027                                                                   
03028      IF PRT-AH-CLAIM                                              
03029          MOVE 'DISAB'            TO DL-ACTIVITY                   
03030          MOVE PRT-CLAIM-AMT-AH   TO DL-RSRV-AMT                   
03031          PERFORM 5100-PRINT-DETAIL THRU 5100-EXIT.                
03032                                                                   
03033  4999-ADVN-TOTALS.                                                
03034                                                                   
03035      IF CR-LFTYP = ZERO                                           
03036          GO TO 4000-EXIT.                                         
03037                                                                   
03038      MOVE SAV-LF-TERM-CODE       TO TG.                           
03039                                                                   
03040      COMPUTE GROUP-CNT (CL TG 3) =                                
03041                             GROUP-CNT (CL TG 1)                   
03042                           + GROUP-CNT (CL TG 2).                  
03043      COMPUTE GROUP-AMT (CL TG 3) =                                
03044                             GROUP-AMT (CL TG 1)                   
03045                           + GROUP-AMT (CL TG 2).                  
03046      COMPUTE GROUP-AMT (CL TG 8) =                                
03047                             GROUP-AMT (CL TG 3)                   
03048                           - GROUP-AMT (CL TG 9).                  
03049      COMPUTE GROUP-AMT (CL TG 7) =                                
03050                             GROUP-AMT (CL TG 8)                   
03051                           - GROUP-AMT (CL TG 4)                   
03052                           - GROUP-AMT (CL TG 5)                   
03053                           - GROUP-AMT (CL TG 6).                  
03054                                                                   
03055      COMPUTE GROUP-CNT (CL TG 5) =                                
03056                             GROUP-CNT (CL TG 3)                   
03057                           - GROUP-CNT (CL TG 9)                   
03058                           - GROUP-CNT (CL TG 4)                   
03059                           - GROUP-CNT (CL TG 6).                  
03060                                                                   
03061      IF GROUP-CNT (CL TG 5) GREATER THAN ZEROS                    
03062          COMPUTE GROUP-AMT (CL TG 5) =                            
03063                             GROUP-AMT (CL TG 3)                   
03064                           - GROUP-AMT (CL TG 9)                   
03065                           - GROUP-AMT (CL TG 4)                   
03066                           - GROUP-AMT (CL TG 6)                   
03067                           - GROUP-AMT (CL TG 7).                  
03068                                                                   
03069      IF GROUP-CNT (CL TG 5) LESS THAN ZERO                        
03070          MOVE ZEROS TO GROUP-CNT (CL TG 5).                       
03071                                                                   
03072      COMPUTE GROUP-CNT (CL TG 8) =                                
03073                             GROUP-CNT (CL TG 4)                   
03074                           + GROUP-CNT (CL TG 5)                   
03075                           + GROUP-CNT (CL TG 6).                  
03076      COMPUTE IND-CNT (CL TG 3) =                                  
03077                             IND-CNT (CL TG 1)                     
03078                           + IND-CNT (CL TG 2).                    
03079      COMPUTE IND-AMT (CL TG 3) =                                  
03080                             IND-AMT (CL TG 1)                     
03081                           + IND-AMT (CL TG 2).                    
03082      COMPUTE IND-AMT (CL TG 8) =                                  
03083                             IND-AMT (CL TG 3)                     
03084                           - IND-AMT (CL TG 9).                    
03085      COMPUTE IND-AMT (CL TG 7) =                                  
03086                             IND-AMT (CL TG 8)                     
03087                           - IND-AMT (CL TG 4)                     
03088                           - IND-AMT (CL TG 5)                     
03089                           - IND-AMT (CL TG 6).                    
03090                                                                   
03091      COMPUTE IND-CNT (CL TG 5) =                                  
03092                             IND-CNT (CL TG 3)                     
03093                           - IND-CNT (CL TG 9)                     
03094                           - IND-CNT (CL TG 4)                     
03095                           - IND-CNT (CL TG 6).                    
03096      IF IND-CNT (CL TG 5) GREATER THAN ZEROS                      
03097          COMPUTE IND-AMT (CL TG 5) =                              
03098                             IND-AMT (CL TG 3)                     
03099                           - IND-AMT (CL TG 9)                     
03100                           - IND-AMT (CL TG 4)                     
03101                           - IND-AMT (CL TG 6)                     
03102                           - IND-AMT (CL TG 7).                    
03103                                                                   
03104      IF IND-CNT (CL TG 5) LESS THAN ZERO                          
03105          MOVE ZEROS TO IND-CNT (CL TG 5).                         
03106                                                                   
03107      COMPUTE IND-CNT (CL TG 8) =                                  
03108                             IND-CNT (CL TG 4)                     
03109                           + IND-CNT (CL TG 5)                     
03110                           + IND-CNT (CL TG 6).                    
03111  4000-EXIT.                                                       
03112      EXIT.                                                        
03113                                                                   
03114  EJECT                                                            
03115  5000-PRINT-ROUTINES     SECTION 30.                              
03116                                                                   
03117  5100-PRINT-DETAIL.                                               
03118      IF DTE-PGM-OPT NOT = 1                                       
03119          GO TO 5100-EXIT.                                         
03120                                                                   
03121      IF FIRST-INS-PASS AND REINSURANCE-REC                        
03122          GO TO 5100-EXIT.                                         
03123                                                                   
03124      IF LINE-CNT GREATER THAN +53                                 
03125          PERFORM 5500-PRINT-HEADING THRU 5500-EXIT.               
03126                                                                   
03127      IF FIRST-DETAIL-DONE                                         
03128          MOVE DETAIL-LINE-1      TO P-DATA                        
03129          MOVE SPACE              TO X                             
03130          PERFORM 5900-PRINT                                       
03131          MOVE SPACES             TO DETAIL-LINE-1                 
03132          ADD +1 TO LINE-CNT                                       
03133          GO TO 5100-EXIT.                                         
03134                                                                   
03135      MOVE CR-CERT                TO DL-CERT-NO.                   
03136      MOVE CR-CERT-SFX            TO DL-CERT-SUFX.                 
03137      MOVE CR-DA                  TO XDD OF FORMAT-DATE.           
03138      MOVE CR-MO                  TO XMM OF FORMAT-DATE.           
03139      MOVE CR-YR                  TO XYY OF FORMAT-DATE.           
03140      MOVE EDITED-DATE            TO DL-ISSUE-DT.                  
03141      MOVE CR-AH-TERM             TO DL-AH-TERM.                   
03142      MOVE CR-LF-TERM             TO DL-LF-TERM.                   
03143                                                                   
03144      IF CR-IND-GRP = '2'                                          
03145          MOVE 'GP '              TO DL-TYPE                       
03146      ELSE                                                         
03147          MOVE 'IND'              TO DL-TYPE.                      
03148                                                                   
03149      MOVE SPACE                  TO DL-PLAN.                      
03150      MOVE CLAS-I-AB3 (CLAS-INDEXL)                                
03151                                  TO DL-PLAN.                      
03152      MOVE CR-LFAMT               TO DL-ORIG-AMT.                  
03153                                                                   
03154      IF OB-COVERAGE OR SUMMARY-REC                                
03155           MOVE CR-LFAMT          TO DL-ORIG-AMT.                  
03156                                                                   
03157      IF PRT-LF-REFUND                                             
03158          MOVE CR-LFRFND          TO DL-LF-PREM                    
03159      ELSE                                                         
03160          MOVE CR-LFPRM           TO DL-LF-PREM.                   
03161                                                                   
03162      IF PRT-AH-REFUND                                             
03163          MOVE CR-AHRFND          TO DL-AH-PREM                    
03164      ELSE                                                         
03165          MOVE CR-AHPRM           TO DL-AH-PREM.                   
03166                                                                   
03167      MOVE SPACE                  TO X.                            
03168      MOVE DETAIL-LINE-1          TO P-DATA.                       
03169      PERFORM 5900-PRINT.                                          
03170      MOVE SPACES                 TO DETAIL-LINE-1.                
03171      MOVE 1                      TO PRINT-CNTRL.                  
03172      ADD +1 TO LINE-CNT.                                          
03173                                                                   
03174  5100-EXIT.                                                       
03175      EXIT.                                                        
03176                                                                   
03177  EJECT                                                            
03178  5200-PRINT-SUMMARY-TOTALS    SECTION 40.                         
03179      IF DTE-PGM-OPT NOT = 1                                       
03180          PERFORM 5500-PRINT-HEADING THRU 5500-EXIT                
03181      ELSE                                                         
03182          IF LINE-CNT NOT LESS THAN +44                            
03183              PERFORM 5500-PRINT-HEADING THRU 5500-EXIT.           
03184                                                                   
03185      ADD 60 TO LINE-CNT.                                          
03186      MOVE '    SUMMARY TOTALS   ' TO HL-MESSAGE.                  
03187      MOVE HEAD-LINE-6            TO P-DATA.                       
03188      MOVE '0'                    TO X.                            
03189      PERFORM 5900-PRINT.                                          
03190                                                                   
03191      MOVE SPACE                  TO X.                            
03192      MOVE HEAD-LINE-8A           TO P-DATA.                       
03193      PERFORM 5900-PRINT.                                          
03194                                                                   
03195      MOVE HEAD-LINE-8B           TO P-DATA.                       
03196      PERFORM 5900-PRINT.                                          
03197      MOVE HEAD-LINE-8C           TO P-DATA.                       
03198      PERFORM 5900-PRINT.                                          
03199                                                                   
03200      IF REINSURANCE-PASS                                          
03201          MOVE +5                 TO CL                            
03202      ELSE                                                         
03203          MOVE +2                 TO CL.                           
03204                                                                   
03205      PERFORM 5300-ADD-TERMS THRU 5300-EXIT                        
03206          VARYING TG FROM +1 BY +1                                 
03207              UNTIL TG GREATER THAN +3.                            
03208                                                                   
03209      MOVE 'GROUP   0-60    '     TO DL-DESCRIP.                   
03210      MOVE 1                      TO TG.                           
03211      MOVE 5                      TO IY.                           
03212      PERFORM 5240-BUILD-SUMMARY.                                  
03213                                                                   
03214      MOVE 'GROUP   61-120  '     TO DL-DESCRIP.                   
03215      MOVE 2                      TO TG.                           
03216      PERFORM 5240-BUILD-SUMMARY.                                  
03217                                                                   
03218      MOVE 'GROUP   GTR 120 '     TO DL-DESCRIP.                   
03219      MOVE 3                      TO TG                            
03220      PERFORM 5240-BUILD-SUMMARY.                                  
03221                                                                   
03222      MOVE 1                      TO TG.                           
03223      MOVE 10                     TO IY.                           
03224      MOVE 'INDV    0-60    '     TO DL-DESCRIP.                   
03225      PERFORM 5240-BUILD-SUMMARY                                   
03226                                                                   
03227      MOVE 2                      TO TG.                           
03228      MOVE 'INDV    61-120  '     TO DL-DESCRIP.                   
03229      PERFORM 5240-BUILD-SUMMARY.                                  
03230                                                                   
03231      MOVE 3                      TO TG.                           
03232      MOVE 'INDV    GTR 120 '     TO DL-DESCRIP.                   
03233      PERFORM 5240-BUILD-SUMMARY.                                  
03234                                                                   
03235  5200-EXIT.                                                       
03236      EXIT.                                                        
03237                                                                   
03238  5240-BUILD-SUMMARY.                                              
03239      MOVE ISSUE-CNT   (CL  TG  IY) TO DL-ISS-CNT.                 
03240      MOVE ISSUE-AMT   (CL  TG  IY) TO DL-ISS-AMT.                 
03241      MOVE ISSUE-PREM  (CL  TG  IY) TO DL-ISS-PRM.                 
03242      MOVE CANCEL-CNT  (CL  TG  IY) TO DL-CAN-CNT.                 
03243      MOVE CANCEL-AMT  (CL  TG  IY) TO DL-CAN-AMT.                 
03244      MOVE CANCEL-PREM (CL  TG  IY) TO DL-CAN-PRM.                 
03245      MOVE CLAIM-CNT   (CL  TG  IY) TO DL-CLM-CNT.                 
03246      MOVE CLAIM-AMT   (CL  TG  IY) TO DL-CLM-AMT.                 
03247      MOVE EARN-PREM   (CL  TG  IY) TO DL-EARN-AMT.                
03248      MOVE DETAIL-LINE-2            TO P-DATA.                     
03249      PERFORM 5900-PRINT.                                          
03250                                                                   
03251  5300-ADD-TERMS.                                                  
03252      MOVE +5                     TO LA.                           
03253                                                                   
03254      PERFORM 5350-ADD-GRPS THRU 5350-EXIT                         
03255          VARYING IY FROM 1 BY 1                                   
03256              UNTIL IY GREATER THAN +4.                            
03257                                                                   
03258      MOVE +10 TO LA.                                              
03259                                                                   
03260      PERFORM 5350-ADD-GRPS THRU 5350-EXIT                         
03261          VARYING IY FROM 6 BY 1                                   
03262              UNTIL IY GREATER THAN +9.                            
03263                                                                   
03264  5300-EXIT.                                                       
03265      EXIT.                                                        
03266                                                                   
03267  5350-ADD-GRPS.                                                   
03268      ADD CORRESPONDING INSURANCE-TYPES (CL  TG  IY) TO            
03269                        INSURANCE-TYPES (CL  TG  LA).              
03270                                                                   
03271  5350-EXIT.                                                       
03272      EXIT.                                                        
03273                                                                   
03274  EJECT                                                            
03275  5500-PRINT-HEADING      SECTION 30.                              
03276      MOVE 0                      TO LINE-CNT.                     
03277      ADD +1 TO PAGE-CNT.                                          
03278      MOVE LAST-CARRIER           TO HL-CARRIER.                   
03279      MOVE LAST-GROUPING          TO HL-COMPANY.                   
03280      MOVE PAGE-CNT               TO HL-PAGENO.                    
03281      MOVE LAST-ACCOUNT           TO HL-ACCOUNT.                   
03282      MOVE LAST-REINS-CO          TO HL-REIN-CO.                   
03283      MOVE LAST-STATE             TO STATE-L.                      
03284                                                                   
03285  5510-STATE-LOOKUP.                                               
03286                              COPY ECSSTLOK.                       
03287                                                                   
03288      IF STATE-L = SPACE                                           
03289          MOVE SPACE              TO HL-STATE                      
03290      ELSE                                                         
03291          MOVE STATE-PIC (CLAS-INDEXS) TO HL-STATE.                
03292                                                                   
03293      MOVE '1'                    TO X.                            
03294      MOVE HEAD-LINE-1            TO P-DATA.                       
03295      PERFORM 5900-PRINT.                                          
03296                                                                   
03297      MOVE SPACE                  TO X.                            
03298      MOVE HEAD-LINE-2            TO P-DATA.                       
03299      PERFORM 5900-PRINT.                                          
03300                                                                   
03301      MOVE HEAD-LINE-3            TO P-DATA.                       
03302      PERFORM 5900-PRINT.                                          
03303                                                                   
03304      MOVE HEAD-LINE-4            TO P-DATA.                       
03305      PERFORM 5900-PRINT.                                          
03306                                                                   
03307      IF REINSURANCE-PASS                                          
03308          MOVE HEAD-LINE-5        TO P-DATA                        
03309          PERFORM 5900-PRINT.                                      
03310                                                                   
03311      IF DTE-PGM-OPT NOT = 1                                       
03312          MOVE ' NO DETAIL GENERATED ' TO HL-MESSAGE               
03313          MOVE HEAD-LINE-6        TO P-DATA                        
03314          PERFORM 5900-PRINT                                       
03315      ELSE                                                         
03316          MOVE '0'                TO X                             
03317          MOVE HEAD-LINE-7A       TO P-DATA                        
03318          PERFORM 5900-PRINT                                       
03319          MOVE SPACE              TO X                             
03320          MOVE HEAD-LINE-7B       TO P-DATA                        
03321          PERFORM 5900-PRINT.                                      
03322                                                                   
03323  5500-EXIT.                                                       
03324      EXIT.                                                        
03325                                                                   
03326  5900-PRINT.                                                      
03327                              COPY ELCPRT2.                        
03328                                                                   
03329  5900-EXIT.                                                       
03330      EXIT.                                                        
03331                                                                   
03332  EJECT                                                            
03333  6000-REINSURANCE-CALC   SECTION 35.                              
03334      IF CR-REIN-TABLE = SPACES OR ZEROS                           
03335          GO TO 6000-EXIT.                                         
03336                                                                   
03337      PERFORM CLEAR-REIN-HOLD THRU CLEAR-REIN-HOLD-X.              
03338      MOVE CR-REIN-TABLE          TO REIN-SRCH.                    
03339      PERFORM RR-READ-REIN THRU RR-READ-REIN-X.                    
03340                                                                   
03341  REINSURE-ROUTINE-GET-CALC.                                       
03342                              COPY ECSRTPFM.                       
03343                                                                   
03344  REINSURE-ROUTINE-SKIP-1.                                         
03345      PERFORM REINSURE-CANCEL THRU REINSURE-CANCEL-X.              
03346      PERFORM REINSURE-CLAIM THRU REINSURE-CLAIM-X.                
03347      MOVE SPACES                 TO RC-REC.                       
03348      MOVE CR-FULL-CONTROL        TO RC-CMCTL.                     
03349      MOVE CR-LFAMT               TO RC-ORIG-LFAMT.                
03350      MOVE CR-LFAMT-ALT           TO RC-LFAMT-ALT.                 
03351      MOVE CR-LF-TERM             TO RC-LF-TERM.                   
03352      MOVE CR-LF-TERM-IN-DAYS     TO RC-LF-TERM-IN-DAYS.           
03353      MOVE CR-AH-TERM             TO RC-AH-TERM.                   
03354      MOVE CR-ENTRY-STATUS        TO RC-ENTRY-STATUS.              
03355      MOVE CR-LF-CURRENT-STATUS   TO RC-LF-CURRENT-STATUS.         
03356      MOVE CR-AH-CURRENT-STATUS   TO RC-AH-CURRENT-STATUS.         
03357      MOVE CR-LFTYP               TO RC-LFTYP.                     
03358      MOVE CR-AHTYP               TO RC-AHTYP.                     
03359      MOVE CR-IND-GRP             TO RC-IND-GRP.                   
03360      MOVE CR-LF-CANC-DT          TO RC-LF-CANC-DT.                
03361      MOVE CR-AH-CANC-DT          TO RC-AH-CANC-DT.                
03362      MOVE CR-DIS-DT              TO RC-DIS-DT.                    
03363      MOVE CR-ENTRY-DATE          TO RC-ENT-DT.                    
pemmod     MOVE CR-LF-status-at-cancel TO RC-LF-cancel-status           
pemmod     MOVE CR-ah-status-at-cancel TO RC-ah-cancel-status           
03364      MOVE CR-LF-CANCEL-EXIT-DATE TO RC-LF-CANC-EXIT.              
03365      MOVE CR-AH-CANCEL-EXIT-DATE TO RC-AH-CANC-EXIT.              
03366      MOVE CR-LF-CLAIM-EXIT-DATE  TO RC-LF-CLAIM-EXIT.             
03367      MOVE CR-AH-SETTLEMENT-EXIT-DATE                              
03368                                  TO RC-AH-CLAIM-EXIT.             
03369      MOVE CR-PMT-FREQ            TO RC-PMT-FREQ.                  
03370      MOVE CR-APR                 TO RC-APR.                       
03371      MOVE CR-LOAN-TERM           TO RC-LOAN-TERM.                 
03372      MOVE CR-IND-GRP             TO RC-IND-GRP.                   
03373      MOVE CR-LIVES               TO RC-I-LIVES.                   
03374      MOVE CR-SUM-CAN-CNT-YTD     TO RC-C-LIVES-YTD.               
03375      MOVE CR-SUM-CAN-CNT-ITD     TO RC-C-LIVES-ITD.               
03376      MOVE CR-LFRFND-CALC         TO RC-LFRFND-YTD.                
03377      MOVE CR-AHRFND-CALC         TO RC-AHRFND-YTD.                
03378      MOVE CR-LF-EXPIRE-DATE      TO RC-LF-EXPIRE-DT.              
03379      MOVE CR-AH-EXPIRE-DATE      TO RC-AH-EXPIRE-DT.              
03380      MOVE CR-LOAN-1ST-PMT-DT     TO RC-LOAN-1ST-PMT-DT.           
03381      MOVE RC-REC                 TO SAVE-REINS-CERT.              
03382      GO TO 6000-EXIT.                                             
03383                                                                   
03384  RR-READ-REIN.                                                    
03385      IF REIN-OPEN-SW = ' '                                        
03386          MOVE LOW-VALUES         TO RE-KEY                        
03387          MOVE 'X'                TO REIN-OPEN-SW                  
03388          MOVE 'A'                TO REIN-SRCH-CODE                
03389          OPEN INPUT ERRTBL-IN
03390          IF ERRTBL-FILE-STATUS = '00' OR '97'                     
03391              PERFORM REIN-CO-TABLE-BUILD THRU REIN-BUILD-EXIT     
03392           ELSE                                                    
03393              MOVE '21'           TO ABEND-FILE-ID                 
03394              MOVE ERRTBL-FILE-STATUS                              
03395                                  TO ABEND-REASON                  
03396              GO TO ABEND-PGM.                                     
03397                                                                   
03398      IF REIN-SRCH NOT = SAVE-REIN-SRCH                            
03399          MOVE LOW-VALUES         TO RE-KEY                        
03400          MOVE DTE-CLASIC-COMPANY-CD                               
03401                                  TO RE-COMPANY-CD                 
03402          MOVE REIN-SRCH          TO SAVE-REIN-SRCH                
03403                                     RE-TABLE                      
03404          MOVE REIN-SRCH-CODE     TO RE-CODE                       
03405          READ ERRTBL-IN
03406          IF ERRTBL-FILE-STATUS NOT = '00'                         
03407              DISPLAY 'INVALID REINSURANCE TABLE CODE - '          
03408                  RE-TABLE ' ' CR-ACCT-CONTROL                     
03409              MOVE '24'            TO ABEND-FILE-ID                
03410              MOVE ERRTBL-FILE-STATUS                              
03411                                  TO ABEND-REASON                  
03412              GO TO ABEND-PGM.                                     
03413                                                                   
03414      IF RE-CODE NOT = 'A'                                         
03415         PERFORM REIN-DATE-LOAD.                                   
03416                                                                   
03417  RR-READ-REIN-X.                                                  
03418      EXIT.                                                        
03419                                                                   
03420  REINSURE-CANCEL.                                                 
03421      IF REIN-COMP (1) = SPACES                                    
03422          GO TO REINSURE-CANCEL-X.                                 
03423                                                                   
03424      IF CR-LFRFND = ZERO AND                                      
03425         CR-AHRFND = ZERO                                          
03426          GO TO REINSURE-CANCEL-X.                                 
03427                                                                   
03428      PERFORM REINSURE-CALC-CANCEL                                 
03429              THRU REINSURE-CALC-CANCEL-X                          
03430                  VARYING SUB1 FROM +1 BY +1                       
03431                      UNTIL REIN-COMP (SUB1) = SPACES.             
03432                                                                   
03433  REINSURE-CANCEL-X.                                               
03434      EXIT.                                                        
03435                                                                   
03436  REINSURE-CLAIM.                                                  
03437      IF REIN-COMP (1) = SPACES                                    
03438          GO TO REINSURE-CLAIM-X.                                  
03439                                                                   
03440      MOVE CR-DTHAMT-YTD          TO RW-LFCLMWK.                   
03441      MOVE CR-DISAMT-YTD          TO RW-AHCLMWK.                   
03442      MOVE +0                     TO RW-LFCLM                      
03443                                     RW-AHCLM.                     
03444                                                                   
03445      IF RW-LFCLMWK = ZERO AND                                     
03446         RW-AHCLMWK = ZERO                                         
03447          GO TO REINSURE-CLAIM-X.                                  
03448                                                                   
03449      PERFORM REINSURE-CALC-CLAIM                                  
03450              THRU REINSURE-CALC-CLAIM-X                           
03451                  VARYING SUB1 FROM +1 BY +1                       
03452                      UNTIL REIN-COMP (SUB1) = SPACES.             
03453                                                                   
03454  REINSURE-CLAIM-X.                                                
03455      EXIT.                                                        
03456  EJECT                                                            
03457                                                                   
03458  REINSURANCE-ROUTINES.                                            
03459                              COPY ECSRIRTNP.
03460  EJECT                                                            
03461                                                                   
03462  REM-TRM-RTN.                                                     
03463      SUBTRACT BGN-YR FROM END-YR GIVING REM-TRM1.                 
03464                                                                   
03465      IF REM-TRM1 < 0                                              
03466        ADD 100 TO REM-TRM1.                                       
03467                                                                   
03468      MULTIPLY +12 BY REM-TRM1.                                    
03469      SUBTRACT BGN-MO FROM END-MO GIVING TEM-TRM1.                 
03470      ADD TEM-TRM1 TO REM-TRM1.                                    
03471                                                                   
03472      IF DTE-REM-TRM = '2'                                         
03473          GO TO REM-TRM-2.                                         
03474                                                                   
03475      SUBTRACT BGN-DA FROM END-DA GIVING TEM-TRM1.                 
03476                                                                   
03477      IF TEM-TRM1 = ZERO                                           
03478          GO TO REM-TRM-1.                                         
03479                                                                   
03480      IF TEM-TRM1 IS LESS THAN -15                                 
03481          SUBTRACT +1 FROM REM-TRM1                                
03482          GO TO REM-TRM-1.                                         
03483                                                                   
03484      IF TEM-TRM1 IS GREATER THAN +15                              
03485          ADD +1 TO REM-TRM1                                       
03486          GO TO REM-TRM-0.                                         
03487                                                                   
03488      IF TEM-TRM1 IS GREATER THAN ZERO                             
03489          GO TO REM-TRM-1.                                         
03490                                                                   
03491  REM-TRM-0.                                                       
03492      SUBTRACT REM-TRM1 FROM HLD-TRM GIVING REM-TRM1.              
03493      ADD +1   REM-TRM1 GIVING REM-TRM2.                           
03494      GO TO REM-TRMX.                                              
03495                                                                   
03496  REM-TRM-1.                                                       
03497      SUBTRACT REM-TRM1 FROM HLD-TRM GIVING REM-TRM1.              
03498      MOVE REM-TRM1               TO REM-TRM2.                     
03499      GO TO REM-TRMX.                                              
03500                                                                   
03501  REM-TRM-2.                                                       
03502      SUBTRACT REM-TRM1 FROM HLD-TRM GIVING REM-TRM2.              
03503      MOVE REM-TRM2               TO REM-TRM1.                     
03504      IF REM-TRM2 NOT = +0                                         
03505          COMPUTE REM-TRM1 = REM-TRM2 - +.5.                       
03506                                                                   
03507  REM-TRMX.                                                        
03508      EXIT.                                                        
03509                                                                   
03510  6000-EXIT.                                                       
03511      EXIT.                                                        
03512                                                                   
03513  EJECT                                                            
03514  7000-LEVEL-BREAK-PROCESSING SECTION 40.                          
03515                                                                   
03516  7100-REINS-CO-BREAK.                                             
03517      PERFORM 7200-CARRIER-BREAK THRU 7200-EXIT.                   
03518      MOVE 1                      TO CL.                           
03519      PERFORM 8200-RECORD-SETUP THRU 8200-EXIT.                    
03520      MOVE 19                     TO ER-REC-TYPE.                  
03521      MOVE LAST-REINS-CO          TO ER-REINS-CO.                  
03522      WRITE CLAS150-EXTRACT-RECORD-B FROM CLAS150-EXTRACT-RECORD.  
03523      PERFORM 8100-CLEAR-TABLES THRU 8100-EXIT.                    
03524                                                                   
03525  7100-EXIT.                                                       
03526      EXIT.                                                        
03527                                                                   
03528  7200-CARRIER-BREAK.                                              
03529      PERFORM 7300-COMPANY-BREAK THRU 7300-EXIT.                   
03530      MOVE +2                     TO CL.                           
03531      PERFORM 7900-ROLL-TOTALS THRU 7900-EXIT.                     
03532      PERFORM 8200-RECORD-SETUP THRU 8200-EXIT.                    
03533      MOVE 18                     TO ER-REC-TYPE                   
03534      MOVE LAST-REINS-CO          TO ER-REINS-CO.                  
03535      MOVE LAST-CARRIER           TO ER-CARRIER.                   
03536      WRITE CLAS150-EXTRACT-RECORD-B FROM CLAS150-EXTRACT-RECORD.  
03537      PERFORM 8100-CLEAR-TABLES THRU 8100-EXIT.                    
03538                                                                   
03539  7200-EXIT.                                                       
03540      EXIT.                                                        
03541                                                                   
03542  7300-COMPANY-BREAK.                                              
03543      PERFORM 7400-STATE-BREAK THRU 7400-EXIT.                     
03544      MOVE +3                     TO CL.                           
03545      PERFORM 7900-ROLL-TOTALS THRU 7900-EXIT.                     
03546      PERFORM 8200-RECORD-SETUP THRU 8200-EXIT.                    
03547      MOVE 17                     TO ER-REC-TYPE.                  
03548      MOVE LAST-REINS-CO          TO ER-REINS-CO.                  
03549      MOVE LAST-CARRIER           TO ER-CARRIER.                   
03550      MOVE LAST-GROUPING          TO ER-COMPANY.                   
03551      WRITE CLAS150-EXTRACT-RECORD-B FROM CLAS150-EXTRACT-RECORD.  
03552      PERFORM 8100-CLEAR-TABLES THRU 8100-EXIT.                    
03553                                                                   
03554  7300-EXIT.                                                       
03555      EXIT.                                                        
03556                                                                   
03557  7400-STATE-BREAK.                                                
03558      PERFORM 7500-ACCOUNT-BREAK THRU 7500-EXIT.                   
03559                                                                   
03560      IF FIRST-INS-PASS                                            
03561          GO TO 7450-STATE-BREAK-2.                                
03562                                                                   
03563      MOVE +4                     TO CL.                           
03564      PERFORM 7900-ROLL-TOTALS THRU 7900-EXIT.                     
03565      PERFORM 8200-RECORD-SETUP THRU 8200-EXIT.                    
03566      MOVE 16                     TO ER-REC-TYPE.                  
03567      MOVE LAST-REINS-CO          TO ER-REINS-CO.                  
03568      MOVE LAST-GROUPING          TO ER-COMPANY.                   
03569      MOVE LAST-CARRIER           TO ER-CARRIER.                   
03570      MOVE LAST-STATE             TO ER-STATE.                     
03571      WRITE CLAS150-EXTRACT-RECORD-B FROM CLAS150-EXTRACT-RECORD.  
03572      PERFORM 8100-CLEAR-TABLES THRU 8100-EXIT.                    
03573      MOVE LOW-VALUES             TO ER-SORT-KEY.                  
03574      MOVE 20                     TO ER-REC-TYPE.                  
03575      MOVE LAST-STATE             TO ER-STATE                      
03576      WRITE CLAS150-EXTRACT-RECORD-C FROM CLAS150-EXTRACT-RECORD.  
03577      GO TO 7400-EXIT.                                             
03578                                                                   
03579  7450-STATE-BREAK-2.                                              
03580      MOVE +1                     TO CL.                           
03581      PERFORM 8200-RECORD-SETUP THRU 8200-EXIT.                    
03582      MOVE 11                     TO ER-REC-TYPE.                  
03583      PERFORM 7460-WRITE-ST-EXTRACT THRU 7460-EXIT.                
03584                                                                   
03585      MOVE 09                     TO ER-REC-TYPE.                  
03586      MOVE LAST-CARRIER           TO ER-CARRIER.                   
03587      PERFORM 7460-WRITE-ST-EXTRACT THRU 7460-EXIT.                
03588                                                                   
03589      MOVE 07                     TO ER-REC-TYPE.                  
03590      MOVE LAST-STATE             TO ER-STATE.                     
03591      PERFORM 7460-WRITE-ST-EXTRACT THRU 7460-EXIT.                
03592                                                                   
03593      MOVE 05                     TO ER-REC-TYPE.                  
03594      MOVE HIGH-VALUE             TO ER-STATE.                     
03595      MOVE LAST-GROUPING          TO ER-COMPANY.                   
03596      PERFORM 7460-WRITE-ST-EXTRACT THRU 7460-EXIT.                
03597                                                                   
03598      MOVE LAST-STATE             TO ER-STATE.                     
03599      MOVE 03                     TO ER-REC-TYPE.                  
03600      PERFORM 7460-WRITE-ST-EXTRACT THRU 7460-EXIT.                
03601                                                                   
03602      MOVE HIGH-VALUES            TO ER-CONTROL.                   
03603      MOVE LAST-STATE             TO ER-ST-SEQ.                    
03604      MOVE 13 TO                  ER-REC-TYPE.                     
03605      PERFORM 7460-WRITE-ST-EXTRACT THRU 7460-EXIT.                
03606                                                                   
03607      PERFORM 8100-CLEAR-TABLES THRU 8100-EXIT.                    
03608      MOVE +3                     TO CL.                           
03609      PERFORM 8200-RECORD-SETUP THRU 8200-EXIT.                    
03610      MOVE 12                     TO ER-REC-TYPE.                  
03611      PERFORM 7460-WRITE-ST-EXTRACT THRU 7460-EXIT.                
03612                                                                   
03613      MOVE 10                     TO ER-REC-TYPE.                  
03614      MOVE LAST-CARRIER           TO ER-CARRIER                    
03615      PERFORM 7460-WRITE-ST-EXTRACT THRU 7460-EXIT.                
03616                                                                   
03617      MOVE 08                     TO ER-REC-TYPE.                  
03618      MOVE LAST-STATE             TO ER-STATE.                     
03619      PERFORM 7460-WRITE-ST-EXTRACT THRU 7460-EXIT.                
03620                                                                   
03621      MOVE 06                     TO ER-REC-TYPE.                  
03622      MOVE HIGH-VALUE             TO ER-STATE.                     
03623      MOVE LAST-GROUPING          TO ER-COMPANY.                   
03624      PERFORM 7460-WRITE-ST-EXTRACT THRU 7460-EXIT.                
03625                                                                   
03626      MOVE 04                     TO ER-REC-TYPE.                  
03627      MOVE LAST-STATE             TO ER-STATE.                     
03628      PERFORM 7460-WRITE-ST-EXTRACT THRU 7460-EXIT.                
03629                                                                   
03630      MOVE HIGH-VALUES            TO ER-CONTROL                    
03631      MOVE LAST-STATE             TO ER-ST-SEQ.                    
03632      MOVE 14                     TO ER-REC-TYPE.                  
03633      PERFORM 7460-WRITE-ST-EXTRACT THRU 7460-EXIT.                
03634                                                                   
03635      PERFORM 8100-CLEAR-TABLES THRU 8100-EXIT.                    
03636                                                                   
03637  7400-EXIT.                                                       
03638      EXIT.                                                        
03639                                                                   
03640  7460-WRITE-ST-EXTRACT.                                           
03641      WRITE CLAS150-EXTRACT-RECORD-A FROM CLAS150-EXTRACT-RECORD.  
03642                                                                   
03643  7460-EXIT.                                                       
03644      EXIT.                                                        
03645                                                                   
03646  7500-ACCOUNT-BREAK.                                              
03647      COMPUTE TERM-WORK = PERIOD-END-9 - PERIOD-START-9.           
03648                                                                   
03649      PERFORM 7505-ADJ-OB-TOTS THRU 7505-EXIT                      
03650          VARYING TG FROM 1 BY 1                                   
03651              UNTIL TG GREATER +3.                                 
03652                                                                   
03653      GO TO 7510-ACCT-REIN-BREAK.                                  
03654                                                                   
03655  7505-ADJ-OB-TOTS.                                                
03656      IF REINSURANCE-PASS                                          
03657          MOVE +5                 TO CL                            
03658          PERFORM 7507-OB-DIVIDE THRU 7507-EXIT                    
03659      ELSE                                                         
03660          MOVE +2                 TO CL                            
03661          PERFORM 7507-OB-DIVIDE THRU 7507-EXIT                    
03662          MOVE +4                 TO CL                            
03663          PERFORM 7507-OB-DIVIDE THRU 7507-EXIT.                   
03664                                                                   
03665  7505-EXIT.                                                       
03666      EXIT.                                                        
03667                                                                   
03668  7507-OB-DIVIDE.                                                  
03669 *    IF ISSUE-AMT (CL  TG  3) NOT = 0                             
03670 *        DIVIDE 12 INTO ISSUE-AMT (CL  TG  3)                     
03671 *        MOVE +1                 TO ISSUE-CNT (CL  TG  3).        
03672 *                                                                 
03673 *    IF ISSUE-AMT (CL  TG  8) NOT = 0                             
03674 *        DIVIDE 12 INTO ISSUE-AMT (CL  TG  8)                     
03675 *        MOVE +1                 TO ISSUE-CNT (CL  TG  8).        
03676 *                                                                 
03677 *    IF ISSUE-AMT (CL  TG  5) NOT = 0                             
03678 *        DIVIDE 12 INTO ISSUE-AMT (CL  TG  5)                     
03679 *        MOVE +1                 TO ISSUE-CNT (CL  TG  5).        
03680 *                                                                 
03681 *    IF ISSUE-AMT (CL  TG  10) NOT = 0                            
03682 *        DIVIDE 12 INTO ISSUE-AMT (CL  TG  10)                    
03683 *        MOVE +1                 TO ISSUE-CNT (CL  TG  10).       
03684                                                                   
03685  7507-EXIT.                                                       
03686      EXIT.                                                        
03687                                                                   
03688  7510-ACCT-REIN-BREAK.                                            
03689      IF FIRST-INS-PASS                                            
03690          GO TO 7550-ACCNT-BREAK-2.                                
03691                                                                   
03692      MOVE +5                     TO CL.                           
03693      PERFORM 7900-ROLL-TOTALS THRU 7900-EXIT.                     
03694      PERFORM 8200-RECORD-SETUP THRU 8200-EXIT.                    
03695      MOVE SAVE-CONTROL-BREAKS    TO ER-CONTROL.                   
03696      MOVE 15                     TO ER-REC-TYPE.                  
03697      WRITE CLAS150-EXTRACT-RECORD-B FROM CLAS150-EXTRACT-RECORD.  
03698                                                                   
03699      IF DTE-PGM-OPT = 1                                           
03700          PERFORM 5200-PRINT-SUMMARY-TOTALS THRU 5200-EXIT.        
03701                                                                   
03702      PERFORM 8100-CLEAR-TABLES THRU 8100-EXIT.                    
03703      GO TO 7500-EXIT.                                             
03704                                                                   
03705  7550-ACCNT-BREAK-2.                                              
03706      MOVE +2                     TO CL.                           
03707      PERFORM 7900-ROLL-TOTALS THRU 7900-EXIT.                     
03708      PERFORM 8200-RECORD-SETUP THRU 8200-EXIT.                    
03709      MOVE SAVE-CONTROL-BREAKS    TO ER-CONTROL.                   
03710      MOVE 01                     TO ER-REC-TYPE.                  
03711      WRITE CLAS150-EXTRACT-RECORD-A FROM CLAS150-EXTRACT-RECORD.  
03712                                                                   
03713      MOVE +4                     TO CL.                           
03714      PERFORM 7900-ROLL-TOTALS THRU 7900-EXIT.                     
03715      PERFORM 8200-RECORD-SETUP THRU 8200-EXIT.                    
03716      MOVE SAVE-CONTROL-BREAKS    TO ER-CONTROL.                   
03717      MOVE 02                     TO ER-REC-TYPE.                  
03718      WRITE CLAS150-EXTRACT-RECORD-A FROM CLAS150-EXTRACT-RECORD.  
03719                                                                   
03720      IF DTE-PGM-OPT = 1                                           
03721          PERFORM 5200-PRINT-SUMMARY-TOTALS THRU 5200-EXIT.        
03722      MOVE +4                     TO CL.                           
03723      PERFORM 8100-CLEAR-TABLES THRU 8100-EXIT.                    
03724      MOVE +2                     TO CL.                           
03725      PERFORM 8100-CLEAR-TABLES THRU 8100-EXIT.                    
03726                                                                   
03727  7500-EXIT.                                                       
03728      EXIT.                                                        
03729                                                                   
03730  7900-ROLL-TOTALS.                                                
03731      COMPUTE CLX = CL - 1.                                        
03732      ADD CORRESPONDING A-CONTROL-LEVELS (CL) TO                   
03733                        A-CONTROL-LEVELS (CLX).                    
03734      PERFORM 7910-TERM-LOOP THRU 7910-EXIT                        
03735          VARYING TG FROM +1 BY +1                                 
03736              UNTIL TG GREATER +3.                                 
03737                                                                   
03738  7900-EXIT.                                                       
03739      EXIT.                                                        
03740                                                                   
03741  7910-TERM-LOOP.                                                  
03742      PERFORM 7920-ROLL-ACTIVITY THRU 7920-EXIT                    
03743          VARYING IY FROM +1 BY +1                                 
03744              UNTIL IY GREATER +10.                                
03745      PERFORM 7930-ROLL-EXHIBIT THRU 7930-EXIT                     
03746          VARYING EG FROM +1 BY +1                                 
03747              UNTIL EG GREATER +9.                                 
03748                                                                   
03749  7910-EXIT.                                                       
03750      EXIT.                                                        
03751                                                                   
03752  7920-ROLL-ACTIVITY.                                              
03753      ADD CORRESPONDING INSURANCE-TYPES (CL  TG  IY) TO            
03754                        INSURANCE-TYPES (CLX  TG  IY).             
03755                                                                   
03756  7920-EXIT.                                                       
03757      EXIT.                                                        
03758                                                                   
03759  7930-ROLL-EXHIBIT.                                               
03760      ADD CORRESPONDING EXHIBIT-GROUPS (CL  TG  EG) TO             
03761                        EXHIBIT-GROUPS (CLX  TG  EG).              
03762                                                                   
03763  7930-EXIT.                                                       
03764      EXIT.                                                        
03765  EJECT                                                            
03766  8000-MISCELANEOUS-ROUTINES  SECTION 32.                          
03767                                                                   
03768  8100-CLEAR-TABLES.                                               
03769      MOVE ZERO                   TO AH-PAID-THIS-YR (CL)          
03770                                     AH-PAID-LAST-YR (CL).         
03771                                                                   
03772      PERFORM 8110-ZERO-TERM-GROUPS THRU 8110-EXIT                 
03773          VARYING TG FROM +1 BY +1                                 
03774              UNTIL TG GREATER THAN 3.                             
03775                                                                   
03776  8100-EXIT.                                                       
03777      EXIT.                                                        
03778                                                                   
03779  8110-ZERO-TERM-GROUPS.                                           
03780      PERFORM 8120-ZERO-ACTIVITY THRU 8120-EXIT                    
03781          VARYING IY FROM +1 BY +1                                 
03782              UNTIL IY GREATER +10.                                
03783                                                                   
03784      PERFORM 8130-ZERO-EXHIBIT THRU 8130-EXIT                     
03785          VARYING EG FROM +1 BY +1                                 
03786              UNTIL EG GREATER +9.                                 
03787                                                                   
03788  8110-EXIT.                                                       
03789      EXIT.                                                        
03790                                                                   
03791  8120-ZERO-ACTIVITY.                                              
03792      MOVE ZERO                  TO ISSUE-CNT   (CL  TG  IY)       
03793                                    ISSUE-AMT   (CL  TG  IY)       
03794                                    ISSUE-PREM  (CL  TG  IY)       
03795                                    CANCEL-CNT  (CL  TG  IY)       
03796                                    CANCEL-PREM (CL  TG  IY)       
03797                                    CANCEL-AMT  (CL  TG  IY)       
03798                                    CLAIM-CNT   (CL  TG  IY)       
03799                                    CLAIM-AMT   (CL  TG  IY)       
03800                                    EARN-PREM   (CL  TG  IY).      
03801                                                                   
03802  8120-EXIT.                                                       
03803      EXIT.                                                        
03804                                                                   
03805  8130-ZERO-EXHIBIT.                                               
03806      MOVE ZERO                  TO GROUP-CNT (CL  TG  EG)         
03807                                    GROUP-AMT (CL  TG  EG)         
03808                                    IND-CNT   (CL  TG  EG)         
03809                                    IND-AMT   (CL  TG  EG).        
03810                                                                   
03811  8130-EXIT.                                                       
03812      EXIT.                                                        
03813                                                                   
03814  8200-RECORD-SETUP.                                               
03815      MOVE LOW-VALUES             TO ER-SORT-KEY.                  
03816      MOVE RUN-DATE               TO ER-PERIOD-START.              
03817      MOVE EP-DT                  TO ER-PERIOD-END.                
03818      MOVE CONTROL-LEVELS (CL)    TO ER-ACTIVITY-TOTALS.           
03819      MOVE E-CONTROL-LEVELS (CL)  TO ER-EXHIBIT-TOTALS.            
03820      MOVE AH-PAID-THIS-YR (CL)   TO ER-AH-PD-THIS.                
03821      MOVE AH-PAID-LAST-YR (CL)   TO ER-AH-PD-LAST.                
03822      MOVE HIGH-VALUES            TO ER-CONTROL.                   
03823                                                                   
03824  8200-EXIT.                                                       
03825      EXIT.                                                        
03826                                                                   
03827  8300-STARTING-INFORCE.                                           
03828                                                                   
03829      IF CR-LFAMT-ALT NOT NUMERIC                                  
03830          MOVE ZEROS TO CR-LFAMT-ALT.                              
03831                                                                   
03832      IF CR-ENTRY-STATUS = '3' OR '5'                              
03833          MOVE ZERO               TO INFORCE-DOLLARS               
03834          GO TO 8300-EXIT.                                         
03835                                                                   
03836      IF SUMMARY-REC                                               
03837          NEXT SENTENCE                                            
03838        ELSE                                                       
03839           MOVE CR-LFAMT          TO INFORCE-DOLLARS.              
03840                                                                   
03841      IF CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P'                   
03842          GO TO 8390-OB-ADJ.                                       
03843                                                                   
03844      MOVE CR-CC                  TO WS-CR-CCYY (1:2).             
03845      MOVE CR-YR                  TO WS-CR-CCYY (3:2).             
03846                                                                   
03847      COMPUTE VALUATION-DATE = CR-LF-TERM -                        
03848        (((RUN-CCYY * 12) + RUN-MO) -                              
03849                             ((WS-CR-CCYY * 12) + CR-MO)).         
03850                                                                   
03851      MOVE SAVE-REM-TERM              TO UNEXPIRED-MONTHS.         
03852      COMPUTE EXPIRED-MONTHS = CR-LF-TERM - UNEXPIRED-MONTHS.      
03853                                                                   
03854      IF SAVE-REM-TERM  = CR-LF-TERM                               
03855          MOVE CR-LFAMT  TO  INFORCE-DOLLARS                       
03856          IF CLAS-I-EP (CLAS-INDEXL) = 'B'                         
03857              ADD CR-LFAMT-ALT      TO INFORCE-DOLLARS             
03858              GO TO 8300-EXIT                                      
03859          ELSE                                                     
03860              GO TO 8300-EXIT.                                     
03861                                                                   
03862      IF UNEXPIRED-MONTHS NOT GREATER ZERO                         
03863          IF LF-BAL-REMTRM NOT GREATER THAN ZERO                   
03864              MOVE ZEROS TO INFORCE-DOLLARS                        
03865              GO TO 8300-EXIT.                                     
03866                                                                   
03867      IF SUMMARY-REC                                               
03868          COMPUTE TEMP-RESULT-L ROUNDED =                          
03869                          INFORCE-DOLLARS / CR-LF-TERM             
03870       ELSE                                                        
03871          COMPUTE TEMP-RESULT-L ROUNDED =                          
03872                          CR-LFAMT / CR-LF-TERM.                   
03873                                                                   
03874      IF REINSURANCE-PASS OR REINSURANCE-REC                       
03875         IF CLAS-I-EP (CLAS-INDEXL) = 'B'                          
03876             COMPUTE TEMP-RESULT-L ROUNDED  =                      
03877             (RC-ORIG-LFAMT * RC-CEDE-FACT) / CR-LF-TERM.          
03878                                                                   
03879      IF CLAS-I-EP (CLAS-INDEXL) = 'T'                             
03880          GO TO 8310-TEXAS-INFORCE.                                
03881                                                                   
03882      IF CLAS-I-EP (CLAS-INDEXL) = 'N'                             
03883          GO TO 8320-NET-PAY-INFORCE.                              
03884                                                                   
03885      IF DTE-CLIENT EQUAL 'FLU'                                    
03886          GO TO 8325-FLU-INFORCE.                                  
03887                                                                   
CIDMOD     IF CR-LFTYP = 'QD'                                           
CIDMOD        MOVE +15.0               TO CR-APR                        
CIDMOD        GO TO 8320-NET-PAY-INFORCE                                
CIDMOD     END-IF                                                       
03893                                                                   
CIDMOD*    IF STATE-ABBR (CLAS-INDEXS) = 'OH'                           
CIDMOD     IF (STATE-ABBR (CLAS-INDEXS) = 'OH') AND                     
CIDMOD        (CR-RATING-CLASS NOT = 'L ')
03889          IF (CR-DT GREATER THAN 19831031) AND                     
03890             (CR-LF-TERM GREATER THAN +60)  AND                    
03891             (CR-APR GREATER THAN ZERO)                            
03892              GO TO 8320-NET-PAY-INFORCE.                          
03893                                                                   
03894      IF STATE-ABBR (CLAS-INDEXS) = 'MT'                           
03895          IF (CR-DT GREATER THAN 19830318) AND                     
03896             (CR-LF-TERM GREATER THAN +61)  AND                    
03897             (CR-APR GREATER THAN ZERO)                            
03898              GO TO 8320-NET-PAY-INFORCE.                          
03899                                                                   
03900      IF STATE-ABBR (CLAS-INDEXS) = 'UT'                           
03901          IF (CR-DT GREATER THAN 19810831) AND                     
03902             (CR-DT LESS THAN 19830901) AND                        
03903             (CR-LF-TERM GREATER THAN +62)  AND                    
03904             (CR-APR GREATER THAN ZERO)                            
03905              GO TO 8320-NET-PAY-INFORCE.                          
03906                                                                   
03907      IF STATE-ABBR (CLAS-INDEXS) = 'RI'                           
03908          IF (CR-DT GREATER THAN 19831231) AND                     
03909             (CR-LF-TERM GREATER THAN +60)  AND                    
03910             (CR-APR GREATER THAN ZERO)                            
03911              GO TO 8320-NET-PAY-INFORCE.                          
03912                                                                   
03913  8305-REG-INFORCE.                                                
03914                                                                   
03915      COMPUTE INFORCE-DOLLARS ROUNDED =                            
03916          (TEMP-RESULT-L * UNEXPIRED-MONTHS) + CR-LFAMT-ALT.       
03917                                                                   
03918      IF REINSURANCE-PASS OR REINSURANCE-REC                       
03919         IF CLAS-I-EP (CLAS-INDEXL) = 'B'                          
03920          COMPUTE INFORCE-DOLLARS ROUNDED =                        
03921           (TEMP-RESULT-L * UNEXPIRED-MONTHS) +                    
03922           (CR-LFAMT-ALT * RC-CEDE-FACT).                          
03923                                                                   
03924      GO TO 8390-OB-ADJ.                                           
03925                                                                   
03926  8310-TEXAS-INFORCE.                                              
03927      DIVIDE CR-LFAMT  BY  CR-LF-TERM                              
03928          GIVING TEX-FACT-1.                                       
03929                                                                   
03930      DIVIDE UNEXPIRED-MONTHS  BY  CR-PMT-FREQ                     
03931          GIVING TEX-FACT-2                                        
03932          REMAINDER TEX-FACT-3.                                    
03933                                                                   
03934      IF TEX-FACT-3 NOT ZERO                                       
03935          ADD +1 TO TEX-FACT-2.                                    
03936                                                                   
03937      IF (TEX-FACT-2 * CR-PMT-FREQ) = CR-LF-TERM                   
03938          MOVE CR-LFAMT  TO  INFORCE-DOLLARS                       
03939      ELSE                                                         
03940          COMPUTE INFORCE-DOLLARS ROUNDED =                        
03941              (TEX-FACT-1 * (TEX-FACT-2 * CR-PMT-FREQ)).           
03942                                                                   
03943      GO TO 8390-OB-ADJ.                                           
03944                                                                   
03945  8320-NET-PAY-INFORCE.                                            
03946                                                                   
03947      MOVE CR-DT                  TO  DC-GREG-DATE-CYMD.           
03948      MOVE 'L'                    TO  DC-OPTION-CODE.              
03949      PERFORM 8500-DATE-CONVERT-ROUTINE THRU                       
03950              8599-DATE-CONVERT-X                                  
03951                                                                   
03952      MOVE DC-BIN-DATE-1          TO  CP-CERT-EFF-DT.              
03953                                                                   
03954      IF CR-LOAN-1ST-PMT-DT NOT = ZEROS                            
03955          MOVE CR-LOAN-1ST-PMT-DT TO  DC-GREG-DATE-1-YMD           
03956          MOVE '3'                TO  DC-OPTION-CODE               
03957          PERFORM 8500-DATE-CONVERT-ROUTINE THRU                   
03958                  8599-DATE-CONVERT-X                              
03959          MOVE DC-BIN-DATE-1      TO  CP-FIRST-PAY-DATE            
03960      ELSE                                                         
03961          MOVE CP-CERT-EFF-DT     TO  DC-BIN-DATE-1                
03962          MOVE +1                 TO  DC-ELAPSED-MONTHS            
03963          MOVE +0                 TO  DC-ELAPSED-DAYS              
03964          MOVE '6'                TO  DC-OPTION-CODE               
03965          PERFORM 8500-DATE-CONVERT-ROUTINE THRU                   
03966              8599-DATE-CONVERT-X                                  
03967          MOVE DC-BIN-DATE-2      TO  CP-FIRST-PAY-DATE.           
03968                                                                   
03969      MOVE CR-LFAMT               TO  CP-ORIGINAL-BENEFIT.         
03970      MOVE CR-APR                 TO  CP-LOAN-APR.                 
03971      MOVE CR-LF-TERM             TO  CP-ORIGINAL-TERM.            
03972      MOVE CR-LOAN-TERM           TO  CP-LOAN-TERM.                
03973      MOVE SAVE-REM-TERM          TO  CP-REMAINING-TERM.           
03974      MOVE STATE-ABBR       (CLAS-INDEXS) TO  CP-STATE-STD-ABBRV.  
03975      MOVE CLAS-I-CALC-TYPE (CLAS-INDEXL) TO  CP-SPECIAL-CALC-CD.  
03976      MOVE CLAS-I-RL-AH     (CLAS-INDEXL) TO  CP-BENEFIT-TYPE.     
03977      MOVE CLAS-I-EP        (CLAS-INDEXL) TO  CP-EARNING-METHOD.   
03978                                                                   
03979      CALL 'ELRAMTX' USING CALCULATION-PASS-AREA.                  
03980                                                                   
03981      IF SUMMARY-REC                                               
03982          COMPUTE INFORCE-DOLLARS ROUNDED = CP-REMAMT-FACTOR *     
03983                                       (INFORCE-DOLLARS / +1000)   
03984          GO TO 8390-OB-ADJ.                                       
03985                                                                   
03986      IF CP-STATE-STD-ABBRV = 'NC'                                 
03987                AND                                                
CIDMOD*      DTE-CLIENT NOT = 'WDS'                                     
CIDMOD       (DTE-CLIENT NOT = 'WDS' AND 'CID')                         
03989          IF CP-CERT-EFF-DT GREATER THAN WS-931231                 
03990              MOVE CP-REMAINING-AMT TO  INFORCE-DOLLARS            
03991              GO TO 8390-OB-ADJ.                                   
03992                                                                   
03993       COMPUTE INFORCE-DOLLARS ROUNDED = CP-REMAMT-FACTOR *        
03994                                       (CR-LFAMT / +1000).         
03995                                                                   
03996      GO TO 8390-OB-ADJ.                                           
03997                                                                   
03998  8325-FLU-INFORCE.                                                
03999      MOVE CR-APR                 TO NP-APR.                       
04000      MOVE CR-LF-TERM             TO NP-ORIG  NP-CAP.              
04001      COMPUTE NP251-REM = CR-LF-TERM - TERM-WORK.                  
04002      MOVE '1'                    TO NP-OPT.                       
04003      MOVE CR-AHPRM               TO NP-AHPRM.                     
04004      MOVE CR-ACCOUNT             TO NP-ACCOUNT.                   
04005      MOVE CR-DT                  TO NP-EFF-DT.                    
04006      MOVE CR-LFAMT               TO NP-BENEFIT.                   
04007 ***  CALL 'IBKE251B'                                              
04008 ***      USING NP-ACCOUNT NP-EFF-DT NP-BENEFIT NP-AHPRM NP251-REM 
04009 ***            NP251-ORIG NP-SUB NP-REMAINING.                    
04010      MOVE NP-REMAINING           TO INFORCE-DOLLARS.              
04011      GO TO 8390-OB-ADJ.                                           
04012                                                                   
04013  8350-ENDING-INFORCE.                                             
04014      IF CR-LFAMT-ALT NOT NUMERIC                                  
04015          MOVE ZEROS TO CR-LFAMT-ALT.                              
04016                                                                   
04017      IF CR-ENTRY-STATUS = '3' OR '5'                              
04018          MOVE ZERO               TO INFORCE-DOLLARS               
04019          GO TO 8300-EXIT.                                         
04020                                                                   
04021      IF SUMMARY-REC                                               
04022          NEXT SENTENCE                                            
04023        ELSE                                                       
04024           MOVE CR-LFAMT          TO INFORCE-DOLLARS.              
04025                                                                   
04026      IF CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P'                   
04027          IF CLAS-I-EP (CLAS-INDEXL) = 'B'                         
04028              ADD CR-LFAMT-ALT    TO INFORCE-DOLLARS               
04029              GO TO 8390-OB-ADJ                                    
04030          ELSE                                                     
04031              GO TO 8390-OB-ADJ.                                   
04032                                                                   
04033      MOVE CP-REMAINING-TERM-2        TO UNEXPIRED-MONTHS.         
04034      COMPUTE EXPIRED-MONTHS = CR-LF-TERM - UNEXPIRED-MONTHS.      
04035                                                                   
04036      IF CP-REMAINING-TERM-2 = CR-LF-TERM                          
04037          MOVE CR-LFAMT  TO  INFORCE-DOLLARS                       
04038          IF CLAS-I-EP (CLAS-INDEXL) = 'B'                         
04039             IF REINSURANCE-PASS OR REINSURANCE-REC                
04040                 NEXT SENTENCE                                     
04041             ELSE                                                  
04042                 ADD CR-LFAMT-ALT    TO INFORCE-DOLLARS            
04043                 GO TO 8390-OB-ADJ                                 
04044          ELSE                                                     
04045              GO TO 8300-EXIT.                                     
04046                                                                   
04047      IF UNEXPIRED-MONTHS NOT GREATER ZERO                         
04048          IF LF-BAL-REMTRM NOT GREATER THAN ZERO                   
04049              MOVE ZEROS TO INFORCE-DOLLARS                        
04050              GO TO 8300-EXIT.                                     
04051                                                                   
04052      MOVE CR-CC                  TO WS-CR-CCYY (1:2).             
04053      MOVE CR-YR                  TO WS-CR-CCYY (3:2).             
04054      COMPUTE VALUATION-DATE = CR-LF-TERM -                        
04055         (((EP-CCYY * 12) + EP-MO)                                 
04056                     - ((WS-CR-CCYY * 12) + CR-MO)).               
04057                                                                   
04058      IF SUMMARY-REC                                               
04059          COMPUTE TEMP-RESULT-L ROUNDED =                          
04060                            INFORCE-DOLLARS / CR-LF-TERM           
04061       ELSE                                                        
04062          COMPUTE TEMP-RESULT-L ROUNDED =                          
04063                            CR-LFAMT / CR-LF-TERM.                 
04064                                                                   
04065      IF REINSURANCE-PASS OR REINSURANCE-REC                       
04066         IF CLAS-I-EP (CLAS-INDEXL) = 'B'                          
04067          COMPUTE TEMP-RESULT-L ROUNDED =                          
04068           (RC-ORIG-LFAMT * RC-CEDE-FACT) / CR-LF-TERM.            
04069                                                                   
04070      IF CLAS-I-EP (CLAS-INDEXL) = 'B'                             
04071          GO TO 8355-NORMAL-ENDING.                                
04072                                                                   
04073      IF CLAS-I-EP (CLAS-INDEXL) = 'T'                             
04074          GO TO 8360-TEXAS-ENDING.                                 
04075                                                                   
04076      IF DTE-CLIENT = 'NCL'                                        
04077        IF CR-DT LESS 19910101                                     
04078          GO TO 8355-NORMAL-ENDING.                                
04079                                                                   
04080      IF CLAS-I-EP (CLAS-INDEXL) = 'N'                             
04081          GO TO 8370-NET-PAY-ENDING.                               
04082                                                                   
04083      IF DTE-CLIENT = 'FLU'                                        
04084          GO TO 8375-FLU-ENDING.                                   
04085                                                                   
CIDMOD     IF CR-LFTYP = 'QD'                                           
CIDMOD        MOVE +15.0               TO CR-APR                        
CIDMOD        GO TO 8370-NET-PAY-ENDING                                 
CIDMOD     END-IF                                                       
CIDMOD                                                                  
CIDMOD*    IF STATE-ABBR (CLAS-INDEXS) = 'OH'                           
CIDMOD     IF (STATE-ABBR (CLAS-INDEXS) = 'OH') AND                     
CIDMOD        (CR-RATING-CLASS NOT = 'L ')
04087          IF (CR-DT GREATER THAN 19831031) AND                     
04088             (CR-LF-TERM GREATER THAN +60) AND                     
04089             (CR-APR GREATER THAN ZERO)                            
04090              GO TO 8370-NET-PAY-ENDING.                           
04091                                                                   
04092      IF STATE-ABBR (CLAS-INDEXS) = 'MT'                           
04093          IF (CR-DT GREATER THAN 19830318) AND                     
04094             (CR-LF-TERM GREATER THAN +61) AND                     
04095             (CR-APR GREATER THAN ZERO)                            
04096              GO TO 8370-NET-PAY-ENDING.                           
04097                                                                   
04098      IF STATE-ABBR (CLAS-INDEXS) = 'UT'                           
04099          IF (CR-DT GREATER THAN 19810831 ) AND                    
04100             (CR-DT LESS THAN 19830901 ) AND                       
04101             (CR-LF-TERM GREATER THAN +62) AND                     
04102             (CR-APR GREATER THAN ZERO)                            
04103              GO TO 8370-NET-PAY-ENDING.                           
04104                                                                   
04105      IF STATE-ABBR (CLAS-INDEXS) = 'RI'                           
04106          IF (CR-DT GREATER THAN 19831231) AND                     
04107             (CR-LF-TERM GREATER THAN +60) AND                     
04108             (CR-APR GREATER THAN ZERO)                            
04109              GO TO 8370-NET-PAY-ENDING.                           
04110                                                                   
04111  8355-NORMAL-ENDING.                                              
04112                                                                   
04113      COMPUTE INFORCE-DOLLARS ROUNDED =                            
04114         (TEMP-RESULT-L * UNEXPIRED-MONTHS) + CR-LFAMT-ALT.        
04115                                                                   
04116      IF REINSURANCE-PASS OR REINSURANCE-REC                       
04117          IF CLAS-I-EP (CLAS-INDEXL) = 'B'                         
04118           COMPUTE INFORCE-DOLLARS ROUNDED =                       
04119           (TEMP-RESULT-L * UNEXPIRED-MONTHS) +                    
04120           (CR-LFAMT-ALT * RC-CEDE-FACT).                          
04121                                                                   
04122      GO TO 8390-OB-ADJ.                                           
04123                                                                   
04124  8360-TEXAS-ENDING.                                               
04125      DIVIDE CR-LFAMT  BY  CR-LF-TERM                              
04126          GIVING TEX-FACT-1.                                       
04127      DIVIDE UNEXPIRED-MONTHS  BY  CR-PMT-FREQ                     
04128          GIVING TEX-FACT-2                                        
04129          REMAINDER TEX-FACT-3.                                    
04130      IF TEX-FACT-3 NOT ZERO                                       
04131          ADD +1 TO TEX-FACT-2.                                    
04132      IF (TEX-FACT-2 * CR-PMT-FREQ) = CR-LF-TERM                   
04133          MOVE CR-LFAMT  TO  INFORCE-DOLLARS                       
04134      ELSE                                                         
04135          COMPUTE INFORCE-DOLLARS ROUNDED =                        
04136              (TEX-FACT-1 * (TEX-FACT-2 * CR-PMT-FREQ)).           
04137                                                                   
04138 ****************************************************************  
04139 *    DIVIDE VALUATION-DATE BY CR-PMT-FREQ                         
04140 *        GIVING TEX-FACT-2                                        
04141 *            REMAINDER TEX-FACT-3.                                
04142 *    IF TEX-FACT-3 NOT = ZERO                                     
04143 *        ADD +1 TO TEX-FACT-2.                                    
04144 *    COMPUTE INFORCE-DOLLARS =                                    
04145 *         (TEMP-RESULT-L * (TEX-FACT-2 * CR-PMT-FREQ)).           
04146                                                                   
04147      GO TO 8390-OB-ADJ.                                           
04148                                                                   
04149  8370-NET-PAY-ENDING.                                             
04150                                                                   
04151      MOVE CR-DT                  TO  DC-GREG-DATE-CYMD.           
04152      MOVE 'L'                    TO  DC-OPTION-CODE.              
04153      PERFORM 8500-DATE-CONVERT-ROUTINE THRU                       
04154              8599-DATE-CONVERT-X.                                 
04155      MOVE DC-BIN-DATE-1          TO  CP-CERT-EFF-DT.              
04156                                                                   
04157      IF CR-LOAN-1ST-PMT-DT NOT = ZEROS                            
04158          MOVE CR-LOAN-1ST-PMT-DT TO  DC-GREG-DATE-1-YMD           
04159          MOVE '3'                TO  DC-OPTION-CODE               
04160          PERFORM 8500-DATE-CONVERT-ROUTINE THRU                   
04161                  8599-DATE-CONVERT-X                              
04162          MOVE DC-BIN-DATE-1      TO  CP-FIRST-PAY-DATE            
04163      ELSE                                                         
04164          MOVE CP-CERT-EFF-DT     TO  DC-BIN-DATE-1                
04165          MOVE +1                 TO  DC-ELAPSED-MONTHS            
04166          MOVE +0                 TO  DC-ELAPSED-DAYS              
04167          MOVE '6'                TO  DC-OPTION-CODE               
04168          PERFORM 8500-DATE-CONVERT-ROUTINE THRU                   
04169                  8599-DATE-CONVERT-X                              
04170          MOVE DC-BIN-DATE-2      TO  CP-FIRST-PAY-DATE.           
04171                                                                   
04172      MOVE CR-LFAMT               TO  CP-ORIGINAL-BENEFIT.         
04173      MOVE CR-APR                 TO  CP-LOAN-APR.                 
04174      MOVE CR-LF-TERM             TO  CP-ORIGINAL-TERM.            
04175      MOVE CR-LOAN-TERM           TO  CP-LOAN-TERM.                
04176      MOVE CP-REMAINING-TERM-2    TO  CP-REMAINING-TERM.           
04177      MOVE STATE-ABBR       (CLAS-INDEXS) TO  CP-STATE-STD-ABBRV.  
04178      MOVE CLAS-I-CALC-TYPE (CLAS-INDEXL) TO  CP-SPECIAL-CALC-CD.  
04179      MOVE CLAS-I-RL-AH     (CLAS-INDEXL) TO  CP-BENEFIT-TYPE.     
04180      MOVE CLAS-I-EP        (CLAS-INDEXL) TO  CP-EARNING-METHOD.   
04181                                                                   
04182      CALL 'ELRAMTX' USING CALCULATION-PASS-AREA.                  
04183                                                                   
04184      IF SUMMARY-REC                                               
04185          COMPUTE INFORCE-DOLLARS ROUNDED = CP-REMAMT-FACTOR *     
04186                                       (INFORCE-DOLLARS / +1000)   
04187          GO TO 8390-OB-ADJ.                                       
04188                                                                   
04189      IF CP-STATE-STD-ABBRV = 'NC'                                 
04190                AND                                                
CIDMOD*      DTE-CLIENT NOT = 'WDS'                                     
CIDMOD       (DTE-CLIENT NOT = 'WDS' AND 'CID')                         
04192          IF CP-CERT-EFF-DT GREATER THAN WS-931231                 
04193              MOVE CP-REMAINING-AMT TO INFORCE-DOLLARS             
04194              GO TO 8390-OB-ADJ.                                   
04195                                                                   
04196       COMPUTE INFORCE-DOLLARS ROUNDED = CP-REMAMT-FACTOR *        
04197                                       (CR-LFAMT / +1000).         
04198                                                                   
04199      GO TO 8390-OB-ADJ.                                           
04200                                                                   
04201  8375-FLU-ENDING.                                                 
04202      MOVE CR-APR                 TO NP-APR.                       
04203      MOVE CR-LF-TERM             TO NP-ORIG  NP-CAP.              
04204      COMPUTE NP251-REM = CR-LF-TERM - TERM-WORK.                  
04205      MOVE '1'                    TO NP-OPT.                       
04206      MOVE CR-AHPRM               TO NP-AHPRM.                     
04207      MOVE CR-ACCOUNT             TO NP-ACCOUNT.                   
04208      MOVE CR-DT                  TO NP-EFF-DT.                    
04209      MOVE CR-LFAMT               TO NP-BENEFIT.                   
04210 ***  CALL 'IBKE251B'                                              
04211 ***      USING NP-ACCOUNT NP-EFF-DT NP-BENEFIT NP-AHPRM NP251-REM 
04212 ***            NP251-ORIG NP-SUB NP-REMAINING.                    
04213      MOVE NP-REMAINING           TO INFORCE-DOLLARS.              
04214      GO TO 8390-OB-ADJ.                                           
04215                                                                   
04216  8390-OB-ADJ.                                                     
04217      IF CLAS-I-BAL (CLAS-INDEXL) = 'B'                            
04218          MOVE ZERO               TO INFORCE-DOLLARS.              
04219                                                                   
04220  8300-EXIT.                                                       
04221      EXIT.                                                        
04222                                                                   
04223  8400-ADD-TO-EXHIBIT.                                             
04224      IF GROUP-INS                                                 
04225          ADD COUNT-WORK      TO GROUP-CNT (CL  TG  EG)            
04226          ADD INFORCE-DOLLARS TO GROUP-AMT (CL  TG  EG)            
04227      ELSE                                                         
04228          ADD COUNT-WORK      TO IND-CNT (CL  TG  EG)              
04229          ADD INFORCE-DOLLARS TO IND-AMT (CL  TG  EG).             
04230                                                                   
04231  8400-EXIT.                                                       
04232      EXIT.                                                        
04233                                                                   
04234  EJECT                                                            
04235  8450-RE-RATE-ROUTINE.                                            
04236                                                                   
04237      MOVE CR-DT                  TO  DC-GREG-DATE-CYMD.           
04238      MOVE 'L'                    TO  DC-OPTION-CODE.              
04239      PERFORM 8500-DATE-CONVERT-ROUTINE THRU 8599-DATE-CONVERT-X.  
04240      MOVE DC-BIN-DATE-1          TO  CP-CERT-EFF-DT.              
04241      MOVE CLAS-I-RL-AH (CLAS-INDEXA) TO  CP-BENEFIT-TYPE.         
04242      MOVE SPACES                 TO  CP-ACCT-FLD-5.               
04243      MOVE DTE-CLIENT             TO  CP-COMPANY-ID.               
04244      MOVE STATE-SUB (CLAS-INDEXS)    TO  CP-STATE.                
04245      MOVE STATE-ABBR (CLAS-INDEXS)   TO  CP-STATE-STD-ABBRV.      
04246                                                                   
04247      IF CR-AGE IS NOT NUMERIC                                     
04248          MOVE ZEROS              TO  CP-ISSUE-AGE                 
04249      ELSE                                                         
04250          MOVE CR-AGE             TO  CP-ISSUE-AGE.                
04251                                                                   
04252      MOVE CP-REMAINING-TERM-1    TO  CP-ORIGINAL-TERM.            
04253      MOVE CR-AHAMT               TO  CP-ORIGINAL-BENEFIT          
04254                                      CP-RATING-BENEFIT-AMT.       
04255                                                                   
04256      IF CP-STATE-STD-ABBRV = 'OR'                                 
04257          COMPUTE CP-RATING-BENEFIT-AMT =                          
04258                                 CR-AHAMT * CP-REMAINING-TERM-1.   
04259                                                                   
04260      MOVE CR-APR                 TO  CP-LOAN-APR.                 
04261                                                                   
04262      IF CR-PMT-FREQ IS NOT NUMERIC                                
04263          MOVE ZEROS              TO  CP-PAY-FREQUENCY             
04264      ELSE                                                         
04265          MOVE CR-PMT-FREQ        TO  CP-PAY-FREQUENCY.            
04266                                                                   
04267      IF AH-EARN-METHOD = SPACES                                   
04268          MOVE CLAS-I-EP (CLAS-INDEXA) TO  CP-EARNING-METHOD       
04269      ELSE                                                         
04270          MOVE AH-EARN-METHOD          TO  CP-EARNING-METHOD.      
04271                                                                   
04272      IF CR-RATING-CLASS NOT = SPACE AND ZERO                      
04273          MOVE CR-RATING-CLASS    TO  CP-CLASS-CODE                
04274      ELSE                                                         
04275          MOVE AM-CAL-TABLE       TO  CP-CLASS-CODE.               
04276                                                                   
04277      MOVE '3'                    TO  CP-PROCESS-TYPE.             
04278      MOVE CLAS-I-BAL (CLAS-INDEXA)   TO  CP-SPECIAL-CALC-CD.      
04279      MOVE CR-LOAN-TERM           TO  CP-LOAN-TERM.                
04280      MOVE AM-AH-DEVIATION        TO  CP-DEVIATION-CODE.           
04281      MOVE CR-AHTYP               TO  CP-BENEFIT-CD.               
04282      MOVE DTE-CLASIC-COMPANY-CD  TO  CP-COMPANY-CD.               
04283                                                                   
04284      IF CR-PMT-EXTENSION-DAYS IS NOT NUMERIC                      
04285          MOVE ZEROS              TO  CP-TERM-OR-EXT-DAYS          
04286      ELSE                                                         
04287          MOVE CR-PMT-EXTENSION-DAYS  TO  CP-TERM-OR-EXT-DAYS.     
04288                                                                   
04289      MOVE AH-OVERRIDE-L1         TO  CP-AH-OVERRIDE-CODE.         
04290                                                                   
04291      IF AM-AH-DEVIATION-PCT IS NOT NUMERIC                        
04292          MOVE ZEROS              TO  CP-RATE-DEV-PCT              
04293      ELSE                                                         
04294          MOVE AM-AH-DEVIATION-PCT TO CP-RATE-DEV-PCT.             
04295                                                                   
04296  8455-CALL-RATING-ROUTINE.                                        
04297                                                                   
04298      CALL 'ELRATEX' USING CALCULATION-PASS-AREA.                  
04299                                                                   
04300  8499-EXIT.                                                       
04301      EXIT.                                                        
04302                                                                   
04303  EJECT                                                            
04304  8500-DATE-CONVERT-ROUTINE.                                       
04305                                                                   
04306      COPY ELCDCS.                                                 
04307                                                                   
04308  8599-DATE-CONVERT-X.                                             
04309      EXIT.                                                        
04310                                                                   
04311  REIN-DATE-LOAD.                                                  
04312      MOVE RE-CLM-INCURRED-LIM    TO  WS-RE-CLM-INCURRED-LIM-N.    
04313      MOVE RE-EARNING-START-DT    TO  WS-RE-EARNING-START-DT-N.    
04314      MOVE RE-EARNING-STOP-DT     TO  WS-RE-EARNING-STOP-DT-N.     
04315                                                                   
04316  9990-PROGRAM-END            SECTION 45.                          
04317                                                                   
04318  9999-CLOSE-FICH.                                                 
04319                              COPY ELCPRTC.                        
04320                                                                   
04321      CLOSE PRINT-FILE.                                            
04322                                                                   
04323      IF REIN-OPEN-SW EQUAL 'X'                                    
04324             MOVE SPACE           TO REIN-OPEN-SW                  
04325             CLOSE ERRTBL-IN
04326             IF ERRTBL-FILE-STATUS = '00'                          
04327                NEXT SENTENCE                                      
04328             ELSE                                                  
04329                MOVE '22'         TO ABEND-FILE-ID                 
04330                MOVE ERRTBL-FILE-STATUS                            
04331                                  TO ABEND-REASON                  
04332                GO TO ABEND-PGM.                                   
04333 *                                                                 
04334 *                                                                 
04335 * LEAVE THE ABOVE 2 LINES IN TO PREVENT LOSING THE LINE ABOVE     
04344                                                                   
04345      MOVE ZEROS  TO RETURN-CODE.
04345      GOBACK.                                                      
04346                                                                   
04347  9999-EXIT.                                                       
04348      EXIT.                                                        
04349                                                                   
04350  ABEND-PGM.                                                       
04351                                  COPY ELCABEND.                   
