00001  ID DIVISION.                                                     
00002                                                                   
00003  PROGRAM-ID.                 EL630.                               
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 04/19/94 08:59:00.                 
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          
00008 *                            VMOD=2.046.                          
00009                                                                   
00010 *AUTHOR.     LOGIC,INC.                                           
00011 *            DALLAS, TEXAS.                                       
00012                                                                   
00013 *DATE-COMPILED.                                                   
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
00024 *REMARKS.    TRANSACTION - EXA5 - NEW BUSINESS - DATA ENTRY.
103001*
103001******************************************************************
103001*                   C H A N G E   L O G
103001*
103001* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
103001*-----------------------------------------------------------------
103001*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
103001* EFFECTIVE    NUMBER
103001*-----------------------------------------------------------------
103001* 103001    2001100100006  SMVA  CHECK FOR ? MARKS ANYWHERE IN
103001*                              BATCH NUMBER
072308* 072308  CR2008040800002  PEMA  ADD CRED BENE INFORMATION
111109* 111109  CR2008100900003  AJRA  ADD NEW CERT NOTE TABLE 
030310* 030310  CR2009031200002  PEMA  OPEN LOAN OFFICER ON 6301
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
041320* 041320  CR2020030500002  PEMA  Distinguish between iss and canc notes.
103001******************************************************************
00025                                                                   
00026  ENVIRONMENT DIVISION.                                            
00027                                                                   
00028      EJECT                                                        
00029  DATA DIVISION.                                                   
00030  WORKING-STORAGE SECTION.                                         
00031                                                                   
00032  77  FILLER  PIC X(32)  VALUE '********************************'. 
00033  77  FILLER  PIC X(32)  VALUE '*    EL630 WORKING STORAGE     *'. 
00034  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.046 *********'. 
00035                                                                   
00036      COPY ELCSCTM.                                                
00037      COPY ELCSCRTY.                                               
00038                                                                   
00039      EJECT                                                        
00040                                                                   
00041  01  WS-COMM-LENGTH              PIC S9(4) COMP VALUE +1900.      
00042  01  STANDARD-AREAS.                                              
00043      12  SC-ITEM                 PIC S9(4) COMP VALUE +1.         
00044      12  GETMAIN-SPACE           PIC X       VALUE SPACE.         
00045      12  MAP-NAME                PIC X(8)    VALUE 'EL630A'.      
00046      12  MAPSET-NAME             PIC X(8)    VALUE 'EL630S '.     
00047      12  SCREEN-NUMBER           PIC X(4)    VALUE '630A'.        
00048      12  TRANS-ID                PIC X(4)    VALUE 'EXA5'.        
00049      12  EDIT-TRANS              PIC X(4)    VALUE 'EXEB'.        
00050      12  PASS-AREA-LEN           PIC S9(4)   COMP VALUE +16.      
00051      12  THIS-PGM                PIC X(8)    VALUE 'EL630'.       
00052      12  PGM-NAME                PIC X(8)    VALUE SPACES.        
00053      12  TIME-IN                 PIC S9(7)   VALUE ZEROS.         
00054      12  TIME-OUT-R  REDEFINES TIME-IN.                           
00055          16  FILLER              PIC X.                           
00056          16  TIME-OUT            PIC 99V99.                       
00057          16  FILLER              PIC X(2).                        
00058      12  XCTL-005                PIC X(8)    VALUE 'EL005'.       
00059      12  XCTL-010                PIC X(8)    VALUE 'EL010'.       
00060      12  XCTL-626                PIC X(8)    VALUE 'EL626'.       
00061      12  XCTL-6301               PIC X(8)    VALUE 'EL6301'.      
00062      12  XCTL-6302               PIC X(8)    VALUE 'EL6302'.      
00063      12  XCTL-633                PIC X(8)    VALUE 'EL633'.       
00064      12  XCTL-633DMD             PIC X(8)    VALUE 'EL633DMD'.    
00065      12  XCTL-635                PIC X(8)    VALUE 'EL635'.       
00066      12  LINK-001                PIC X(8)    VALUE 'EL001'.       
00067      12  LINK-004                PIC X(8)    VALUE 'EL004'.       
00068      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     
00069      12  ELCNTL-FILE-ID          PIC X(8)    VALUE 'ELCNTL'.      
00070      12  ERPNDB-FILE-ID          PIC X(8)    VALUE 'ERPNDB'.      
00071      12  ERACCT-FILE-ID          PIC X(8)    VALUE 'ERACCT'.      
00072      12  ERACCT2-FILE-ID         PIC X(8)    VALUE 'ERACCT2'.     
00073      12  ELCERT-FILE-ID          PIC X(8)    VALUE 'ELCERT'.      
00074      12  ERNOTE-FILE-ID          PIC X(8)    VALUE 'ERNOTE'.      
111109     12  ERCNOT-FILE-ID          PIC X(8)    VALUE 'ERCNOT'.
00075      12  ERMAIL-FILE-ID          PIC X(8)    VALUE 'ERMAIL'.      
00076      12  ERCOMP-FILE-ID          PIC X(8)    VALUE 'ERCOMP'.      
00077      12  ERPNDM-FILE-ID          PIC X(8)    VALUE 'ERPNDM'.      
00078      12  ERRQST-FILE-ID          PIC X(8)    VALUE 'ERRQST'.      
00079      12  WS-CURRENT-BIN-DT       PIC XX      VALUE SPACES.        
00080      12  WS-CURRENT-DT           PIC X(8)    VALUE SPACES.        
00081      12  WS-CURRENT-YMD          PIC 9(6)    VALUE ZEROS.         
00082      12  WS-SYNC-CNTR            PIC S9(3)   VALUE +0 COMP-3.     
00083      12  WS-SUB1                 PIC S9(4)   VALUE +0 COMP.       
00084      12  WS-SUB2                 PIC S9(4)   VALUE +0 COMP.
103001     12  PIC6-SUB                PIC S9(04)  VALUE +0 COMP.
00085      12  WS-AR-REPORTING         PIC X       VALUE SPACE.         
00086          88  AR-NET-REPORT               VALUE 'N'.               
00087          88  AR-GROSS-REPORT             VALUE 'G'.               
00088      12  WS-COMP-MASTER-ERROR-SW PIC X       VALUE SPACE.         
00089          88  WS-COMP-MASTER-ERROR        VALUE 'Y'.               
00090                                                                   
00091      12  WS-UPDATE-REFERENCE-SW  PIC X       VALUE SPACE.         
00092          88  WS-UPDATE-REFERENCE         VALUE 'Y'.               
00093
103001     12  WS-SWITCH               PIC X(01)   VALUE SPACE.
103001         88  QUESTION-MARK-FOUND             VALUE 'Y'.
103001
00094      12  WS-RECORD-FOUND-SW      PIC X       VALUE SPACE.         
00095          88  RECORD-FOUND                VALUE 'Y'.               
00096          88  RECORD-NOT-FOUND            VALUE 'N'.               
00097                                                                   
00098      12  WS-ON1-SW1              PIC X(01)   VALUE 'Y'.           
00099      12  WS-ON1-SW2              PIC X(01)   VALUE 'Y'.           
00100      12  WS-ON1-SW3              PIC X(01)   VALUE 'Y'.           
00101      12  WS-ON1-SW4              PIC X(01)   VALUE 'Y'.           
00102      12  WS-ON1-SW5              PIC X(01)   VALUE 'Y'.           
00103      12  WS-ON1-SW6              PIC X(01)   VALUE 'Y'.           
00104      12  WS-ON1-SW7              PIC X(01)   VALUE 'Y'.           
00105                                                                   
00106      12  WS-ERPNDB-RECORD        PIC X(585)  VALUE SPACES.        
00107                                                                   
00108  01  BATCH-TO-PROCESS.                                            
00109      05  EDIT-COMPANY-CD         PIC X       VALUE LOW-VALUES.    
00110      05  EDIT-BATCH              PIC X(6)    VALUE SPACES.        
00111      05  EDIT-COMPANY-ID         PIC XXX     VALUE SPACES.        
00112      05  EDIT-RESTART-BATCH      PIC X(6)    VALUE SPACES.        
00113                                                                   
00114      EJECT                                                        
00115  01  ERROR-MESSAGES.                                              
00116      12  ER-0008                 PIC X(4)    VALUE '0008'.        
00117      12  ER-0023                 PIC X(4)    VALUE '0023'.        
00118      12  ER-0029                 PIC X(4)    VALUE '0029'.        
00119      12  ER-0070                 PIC X(4)    VALUE '0070'.        
00120      12  ER-0194                 PIC X(4)    VALUE '0194'.        
00121      12  ER-0195                 PIC X(4)    VALUE '0195'.        
00122      12  ER-0196                 PIC X(4)    VALUE '0196'.        
00123      12  ER-0197                 PIC X(4)    VALUE '0197'.        
00124      12  ER-0340                 PIC X(4)    VALUE '0340'.        
00125      12  ER-0587                 PIC X(4)    VALUE '0587'.        
00126      12  ER-0905                 PIC X(4)    VALUE '0905'.        
00127      12  ER-2119                 PIC X(4)    VALUE '2119'.        
00128      12  ER-2126                 PIC X(4)    VALUE '2126'.        
00129      12  ER-2132                 PIC X(4)    VALUE '2132'.        
00130      12  ER-2201                 PIC X(4)    VALUE '2201'.        
00131      12  ER-2208                 PIC X(4)    VALUE '2208'.        
00132      12  ER-2209                 PIC X(4)    VALUE '2209'.        
00133      12  ER-2210                 PIC X(4)    VALUE '2210'.        
00134      12  ER-2211                 PIC X(4)    VALUE '2211'.        
00135      12  ER-2212                 PIC X(4)    VALUE '2212'.        
00136      12  ER-2213                 PIC X(4)    VALUE '2213'.        
00137      12  ER-2214                 PIC X(4)    VALUE '2214'.        
00138      12  ER-2215                 PIC X(4)    VALUE '2215'.        
00139      12  ER-2216                 PIC X(4)    VALUE '2216'.        
00140      12  ER-2229                 PIC X(4)    VALUE '2229'.        
00141      12  ER-2242                 PIC X(4)    VALUE '2242'.        
00142      12  ER-2248                 PIC X(4)    VALUE '2248'.        
00143      12  ER-2370                 PIC X(4)    VALUE '2370'.        
00144      12  ER-2371                 PIC X(4)    VALUE '2371'.        
00145      12  ER-2402                 PIC X(4)    VALUE '2402'.        
00146      12  ER-2422                 PIC X(4)    VALUE '2422'.        
00147      12  ER-2800                 PIC X(4)    VALUE '2800'.        
00148      12  ER-2880                 PIC X(4)    VALUE '2880'.        
00149      12  ER-2990                 PIC X(4)    VALUE '2990'.        
00150                                                                   
00151      EJECT                                                        
00152  01  ACCESS-KEYS.                                                 
00153      12  ELCNTL-KEY.                                              
00154          16  CNTL-COMP-ID        PIC X(3)  VALUE SPACES.          
00155          16  CNTL-REC-TYPE       PIC X     VALUE SPACES.          
00156          16  CNTL-ACCESS.                                         
00157              20  CNTL-STATE      PIC XX    VALUE SPACES.          
00158              20  FILLER          PIC X     VALUE SPACES.          
00159              20  CNTL-CARRIER    PIC X     VALUE SPACES.          
00160          16  CNTL-SEQ            PIC S9(4) VALUE +0 COMP.         
00161                                                                   
00162      12  ERPNDB-KEY.                                              
00163          16  PNDB-COMP-CD        PIC X     VALUE SPACE.           
00164          16  PNDB-ENTRY-BATCH    PIC X(6)  VALUE SPACES.          
00165          16  PNDB-BATCH-SEQ      PIC S9(4) VALUE +0 COMP.         
00166          16  PNDB-BATCH-CHG-SEQ  PIC S9(4) VALUE +0 COMP.         
00167                                                                   
00168      12  ERACCT-KEY.                                              
00169          16  ERACCT-COMP-KEY.                                     
00170              20  ACCT-CO         PIC X     VALUE SPACES.          
00171              20  ACCT-CARRIER    PIC X     VALUE SPACES.          
00172              20  ACCT-GROUPING   PIC X(6)  VALUE SPACES.          
00173              20  ACCT-STATE      PIC XX    VALUE SPACES.          
00174              20  ACCT-ACCOUNT    PIC X(10) VALUE SPACES.          
00175          16  ACCT-EXP-DATE       PIC XX    VALUE SPACES.          
00176          16  FILLER              PIC X(4)  VALUE LOW-VALUES.      
00177      12  ERACCT-SAVE-KEY         PIC X(20) VALUE SPACES.          
00178                                                                   
00179      12  ELCERT-KEY.                                              
00180          16  CERT-COMPANY-CD     PIC X     VALUE SPACES.          
00181          16  CERT-CARRIER        PIC X     VALUE SPACES.          
00182          16  CERT-GROUPING       PIC X(6)  VALUE SPACES.          
00183          16  CERT-STATE          PIC XX    VALUE SPACES.          
00184          16  FILLER              PIC X(23) VALUE SPACES.          
00185                                                                   
041320     12  ernote-generic-key-len  pic s9(4) comp value +33.
041320     12  ernote-key.
041320         16  ernote-company-cd   pic x     value spaces.
041320         16  ernote-carrier      pic x     value spaces.
041320         16  ernote-grouping     pic x(6)  value spaces.
041320         16  ernote-state        pic xx    value spaces.
041320         16  ernote-account      pic x(10) value spaces.
041320         16  ernote-cert-eff-dt  pic xx    value spaces.
041320         16  ernote-cert-no      pic x(11) value spaces.
041320         16  ernote-rec-type     pic x     value spaces.

00186      12  ERCOMP-KEY.                                              
00187          16  ERCOMP-COMP-CD      PIC X     VALUE SPACE.           
00188          16  ERCOMP-CARRIER      PIC X     VALUE SPACES.          
00189          16  ERCOMP-GROUPING     PIC X(6)  VALUE SPACES.          
00190          16  ERCOMP-FIN-RESP     PIC X(10) VALUE SPACES.          
00191          16  ERCOMP-ACCT         PIC X(10) VALUE SPACES.          
00192          16  ERCOMP-RECORD-TYPE  PIC X     VALUE SPACES.          
00193                                                                   
00194      12  ERRQST-KEY.                                              
00195          16  ERRQST-COMP-CD      PIC X     VALUE SPACE.           
00196          16  ERRQST-ENTRY-BATCH  PIC X(6)  VALUE SPACES.          
111109
111109     12  ERCNOT-KEY. 
111109         16  ERCNOT-PART-KEY.
111109             20  ERCNOT-COMPANY-CD   PIC X        VALUE SPACE.    
111109             20  ERCNOT-CARRIER      PIC X        VALUE SPACES.   
111109             20  ERCNOT-GROUPING     PIC X(6)     VALUE SPACES.   
111109             20  ERCNOT-STATE        PIC XX       VALUE SPACES.   
111109             20  ERCNOT-ACCOUNT      PIC X(10)    VALUE SPACES.   
111109             20  ERCNOT-CERT-EFF-DT  PIC XX       VALUE SPACES.   
111109             20  ERCNOT-CERT-NO.                                  
111109                 25  ERCNOT-CERT-PRIME PIC X(10)  VALUE SPACES.   
111109                 25  ERCNOT-CERT-SFX PIC X        VALUE SPACES.   
111109         16  ERCNOT-REC-TYPE         PIC X        VALUE SPACES.
111109         16  ERCNOT-SEQUENCE         PIC S9(4) COMP VALUE +0.
111109                                                                  
00197                                                                   
00198  01  FILLER.                                                      
041320     12  W-RESPONSE              PIC S9(8)   COMP.
041320         88  RESP-NORMAL               VALUE +00.
041320         88  RESP-NOTFND               VALUE +13.
041320         88  RESP-DUPREC               VALUE +14.
041320         88  RESP-DUPKEY               VALUE +15.
041320         88  RESP-NOTOPEN              VALUE +19.
041320         88  RESP-ENDFILE              VALUE +20.
041320         88  resp-lengtherr            value +22.
00199      12  WS-DEEDIT-FIELD         PIC S9(8)V99 VALUE ZEROS.        
00200      12  WS-DT-DEEDIT-FIELD REDEFINES                             
00201          WS-DEEDIT-FIELD         PIC X(10).                       
00202      12  WS-DEEDIT-FIELD-DATE  REDEFINES  WS-DEEDIT-FIELD.        
00203          16   FILLER                           PIC X(04).         
00204          16   WS-DEEDIT-FIELD-DATE-OUT         PIC X(06).         
00205      12  QUESTION-MARKS          PIC X(6)      VALUE '??????'.
103001     12  QUESTION-MARK           PIC X(01)     VALUE '?'.
00206      12  WS-BATCH-NO             PIC 9(6)      VALUE ZEROS.       
00207      12  WS-OBAL                 PIC S9(8)V99  VALUE ZEROS.       
00208      12  WS-OCNT                 PIC S9(5)     VALUE ZEROS.       
00209      12  WS-DELETE-CNT           PIC S9(5)     VALUE ZEROS.       
00210      12  WS-PF1-SW               PIC X         VALUE SPACES.      
00211          88  WS-PF1                            VALUE 'Y'.         
00212                                                                   
00213      12  WS-SAV-PNDB-KEY.                                         
00214          16  WS-SAV-COMP-CD        PIC X       VALUE LOW-VALUES.  
00215          16  WS-SAV-ENTRY-BATCH    PIC X(6)       VALUE SPACES.   
00216          16  WS-SAV-BATCH-SEQ      PIC S9(4) COMP VALUE ZEROS.    
00217          16  WS-SAV-BATCH-CHG-SEQ  PIC S9(4) COMP VALUE ZEROS.    
00218                                                                   
00219      12  DATE-TEST-AREA          PIC 9(6)         VALUE ZEROS.    
00220      12  DATE-TEST-AREA-R  REDEFINES DATE-TEST-AREA.              
00221          16  DATE-TEST-MM        PIC 99.                          
00222          16  DATE-TEST-DD        PIC 99.                          
00223          16  DATE-TEST-YY        PIC 99.                          
00224      12  DIVIDE-RESULT           PIC 99           VALUE ZEROS.    
00225      12  DIVIDE-REMAINDER        PIC 9            VALUE 0.        
00226      12  WS-CERT-NOTE-SW         PIC X            VALUE ' '.      
00227          88 CERT-NOTES-ARE-PRESENT   VALUE 'Y'.                   
00228      12  WS-CERT-ADDRESS-SW      PIC X            VALUE ' '.      
00229          88 CERT-ADDRESS-PRESENT     VALUE 'Y'.                   
00230      12  WS-PRM-HEADER.                                           
00231          16  WS-PRM-OVERRIDE     PIC XX           VALUE SPACES.   
00232          16  FILLER              PIC X(8)  VALUE '-PREMIUM'.      
00233      12  WS-REFUND-HEADER.                                        
00234          16  WS-REFUND-OVERRIDE  PIC XX    VALUE SPACES.          
00235          16  FILLER              PIC X(7)  VALUE '-REFUND'.       
00236                                                                   
00237      EJECT                                                        
00238                                                                   
00239      COPY ELCDATE.                                                
00240                                                                   
00241      EJECT                                                        
00242      COPY ELCLOGOF.                                               
00243                                                                   
00244      EJECT                                                        
00245      COPY ELCATTR.                                                
00246                                                                   
00247      EJECT                                                        
00248      COPY ELCEMIB.                                                
00249                                                                   
00250      EJECT                                                        
00251      COPY ELCINTF.                                                
00252      COPY ELC630PI.                                               
00253           16  FILLER              PIC X(390).                     
00254                                                                   
00255      EJECT                                                        
00256      COPY ELCAID.                                                 
00257  01  FILLER    REDEFINES DFHAID.                                  
00258      12  FILLER              PIC X(8).                            
00259      12  PF-VALUES           PIC X       OCCURS 2.                
00260      12  FILLER              PIC X(25).                           
00261                                                                   
00262      EJECT                                                        
00263      COPY EL630S.                                                 
00264                                                                   
00265      EJECT                                                        
00266  LINKAGE SECTION.                                                 
00267  01  DFHCOMMAREA             PIC X(1900).                         
00268                                                                   
00269      EJECT                                                        
00270                                                                   
00271      COPY ELCCNTL.                                                
00272      EJECT                                                        
00273      COPY ERCPNDB.                                                
00274      EJECT                                                        
00275      COPY ERCACCT.                                                
00276      EJECT                                                        
00277      COPY ELCCERT.                                                
00278      EJECT                                                        
00279      COPY ERCNOTE.                                                
00280      EJECT                                                        
00281      COPY ERCMAIL.                                                
00282      EJECT                                                        
00283      COPY ERCPNDM.                                                
00284      EJECT                                                        
00285      COPY ERCCOMP.                                                
00286      EJECT                                                        
00287      COPY ERCRQST.                                                
00288      EJECT                                                        
111109     COPY ERCCNOT.
00289                                                                   
00290  PROCEDURE DIVISION.                                              
00291                                                                   
00292      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      
00293      MOVE 2                      TO EMI-NUMBER-OF-LINES.          
00294      IF EIBCALEN = 0                                              
00295          GO TO 8800-UNAUTHORIZED-ACCESS.                          
00296                                                                   
00297      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              
00298      MOVE '5'                    TO DC-OPTION-CODE.               
00299      PERFORM 9700-DATE-LINK.                                      
00300                                                                   
00301      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.            
00302      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.                
00303      MOVE DC-GREG-DATE-1-YMD     TO WS-CURRENT-YMD.               
00304                                                                   
00305      MOVE PI-CALLING-PROGRAM TO PI-SAVE-CALLING-PGM.              
00306      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         
00307          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   
00308              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      
00309              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      
00310              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      
00311              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      
00312              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      
00313              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      
00314              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    
00315              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      
00316              MOVE SPACES               TO PI-PROGRAM-WORK-AREA    
00317                                           PI-MISC                 
00318          ELSE                                                     
00319              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      
00320              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    
00321              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      
00322              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      
00323              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      
00324              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      
00325              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      
00326              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     
00327                                                                   
00328      MOVE LOW-VALUES             TO EL630AI.                      
00329                                                                   
00330      IF PI-SAVE-CALLING-PGM = XCTL-6302                           
00331          GO TO 4000-SHOW-TOTALS.                                  
00332                                                                   
020816     IF PI-SAVE-CALLING-PGM = XCTL-6301 or 'VP6301'
00334          MOVE 'N'                TO PI-EL630-FIRST-TIME-SW        
00335          IF (PI-CAN-CNT-REMITTED GREATER ZERO OR                  
00336              PI-ISS-CNT-REMITTED GREATER ZERO)                    
00337                  GO TO 4000-SHOW-TOTALS                           
00338          ELSE                                                     
00339          IF PI-ISS-CNT-ENTERED = ZEROS  AND                       
00340             PI-CAN-CNT-ENTERED = ZEROS                            
00341              MOVE 'Y'            TO PI-VERIFY-DELETE-SW           
00342              GO TO 3000-DELETE-ENTERED-BATCH                      
00343          ELSE                                                     
00344              GO TO 4000-SHOW-TOTALS.                              
00345                                                                   
00346      IF EIBTRNID NOT = TRANS-ID                                   
00347          MOVE SPACE              TO PI-BROWSE-SW                  
00348          MOVE ZEROS              TO PI-LAST-SEQ-NO-ADDED          
00349          MOVE SPACES             TO PI-CR-CONTROL-IN-PROGRESS     

00350          MOVE QUESTION-MARKS     TO BATCHI
00351          MOVE AL-UANON           TO BATCHA                        
00352          GO TO 8100-SEND-INITIAL-MAP.                             
00353                                                                   
00354      EXEC CICS HANDLE CONDITION                                   
00355          PGMIDERR  (9600-PGMID-ERROR)                             
00356          ERROR     (9990-ABEND)                                   
00357      END-EXEC.                                                    
00358                                                                   
00359      IF EIBAID = DFHCLEAR                                         
00360         MOVE SPACES              TO PI-ACCT-AGENT-PROCESSED-SW    
00361         GO TO 9400-CLEAR.                                         
00362                                                                   
00363      IF PI-PROCESSOR-ID = 'LGXX'                                  
00364          GO TO 0200-RECEIVE.                                      
00365                                                                   
00366      EXEC CICS READQ TS                                           
00367          QUEUE  (PI-SECURITY-TEMP-STORE-ID)                       
00368          INTO   (SECURITY-CONTROL)                                
00369          LENGTH (SC-COMM-LENGTH)                                  
00370          ITEM   (SC-ITEM)                                         
00371      END-EXEC.                                                    
00372                                                                   
00373      MOVE SC-CREDIT-DISPLAY (11)  TO PI-DISPLAY-CAP.              
00374      MOVE SC-CREDIT-UPDATE  (11)  TO PI-MODIFY-CAP.               
00375                                                                   
00376      IF NOT DISPLAY-CAP                                           
00377          MOVE 'READ'          TO SM-READ                          
00378          PERFORM 9995-SECURITY-VIOLATION                          
00379                               THRU 9995-EXIT                      
00380          MOVE ER-0070         TO  EMI-ERROR                       
00381          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00382          GO TO 8100-SEND-INITIAL-MAP.                             
00383                                                                   
00384      EJECT                                                        
00385  0200-RECEIVE.                                                    
00386      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       
00387          MOVE ER-0008            TO EMI-ERROR                     
00388          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00389          MOVE -1                 TO MAINTL                        
00390          GO TO 8200-SEND-DATAONLY.                                
00391                                                                   
00392      EXEC CICS RECEIVE                                            
00393          MAP      (MAP-NAME)                                      
00394          MAPSET   (MAPSET-NAME)                                   
00395          INTO     (EL630AI)                                       
00396      END-EXEC.                                                    
00397                                                                   
00398      IF PFENTERL = 0                                              
00399          GO TO 0300-CHECK-PFKEYS.                                 
00400                                                                   
00401      IF PFENTERI NUMERIC                                          
00402         IF PFENTERI GREATER 0 AND LESS 25                         
00403            MOVE PF-VALUES (PFENTERI) TO EIBAID                    
00404         ELSE                                                      
00405            MOVE ER-0029            TO EMI-ERROR                   
00406            GO TO 0320-INPUT-ERROR.                                
00407                                                                   
00408  0300-CHECK-PFKEYS.                                               
00409      IF EIBAID = DFHENTER                                         
00410         IF PI-CLEAR-ERROR                                         
00411            MOVE -1                 TO PFENTERL                    
00412            MOVE ER-2213            TO EMI-ERROR                   
00413            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
00414            GO TO 4000-SHOW-TOTALS.                                
00415                                                                   
00416      IF EIBAID = DFHPF23                                          
00417          GO TO 8810-PF23.                                         
00418      IF EIBAID = DFHPF24                                          
00419          GO TO 9200-RETURN-MAIN-MENU.                             
00420      IF EIBAID = DFHPF12                                          
00421          GO TO 9500-PF12.                                         
00422                                                                   
00423      IF EIBAID = DFHPF1                                           
00424         IF PI-DATA-UPDATED                                        
00425            MOVE 'Y'            TO WS-PF1-SW                       
00426            MOVE SPACE          TO PI-NB-MONTH-END-DT              
00427                                   PI-CLEAR-ERROR-SW               
00428            PERFORM 2000-WRITE-BATCH-TOTAL-REC THRU 2990-EXIT      
00429            MOVE SPACES TO BATCH-TO-PROCESS                        
00430            MOVE PI-COMPANY-ID  TO EDIT-COMPANY-ID                 
00431            MOVE PI-COMPANY-CD  TO EDIT-COMPANY-CD                 
00432            MOVE PI-SAV-ENTRY-BATCH TO EDIT-BATCH                  
00433            EXEC CICS START                                        
00434                 TRANSID       (EDIT-TRANS)                        
00435                 FROM          (BATCH-TO-PROCESS)                  
00436                 LENGTH        (PASS-AREA-LEN)                     
00437            END-EXEC                                               
00438            MOVE PI-SAV-REFERENCE TO PI-SAV-PYAJ-REFERENCE         
00439            MOVE SPACES         TO PI-UPDATE-SW                    
00440                                   PI-KEYED-SWITCHES               
00441                                   PI-AGE-KEYED-SW                 
00442                                   PI-BIRTHDT-KEYED-SW             
00443                                   PI-ACCT-AGENT-PROCESSED-SW      
00444                                   PI-SAV-REFERENCE                
00445            MOVE ZEROS          TO PI-ISS-CNT-ENTERED              
00446                                   PI-CAN-CNT-ENTERED              
00447            MOVE LOW-VALUES     TO EL630AO                         
00448            MOVE SPACE          TO PI-BROWSE-SW                    
00449            MOVE QUESTION-MARKS TO BATCHI                          
00450            MOVE AL-UANON       TO BATCHA                          
00451            MOVE -1             TO MAINTL                          
00452            IF EMI-NO-ERRORS                                       
00453                MOVE ZEROS          TO EMI-ERROR                   
00454                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT           
00455                GO TO 8100-SEND-INITIAL-MAP                        
00456            ELSE                                                   
00457                GO TO 8100-SEND-INITIAL-MAP                        
00458         ELSE                                                      
00459            IF  MAINTI = 'N'                                       
00460                MOVE -1             TO MAINTL                      
00461                MOVE ER-2211        TO EMI-ERROR                   
00462                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT           
00463                GO TO 8200-SEND-DATAONLY.                          
00464                                                                   
00465      IF EIBAID = DFHPF1                                           
00466         MOVE SPACE          TO PI-NB-MONTH-END-DT                 
00467                                PI-CLEAR-ERROR-SW                  
00468         MOVE SPACES TO BATCH-TO-PROCESS                           
00469         MOVE PI-COMPANY-ID  TO EDIT-COMPANY-ID                    
00470         MOVE PI-COMPANY-CD  TO EDIT-COMPANY-CD                    
00471         MOVE PI-SAV-ENTRY-BATCH TO EDIT-BATCH                     
00472         IF EDIT-BATCH GREATER SPACES                              
00473              EXEC CICS START                                      
00474                 TRANSID       (EDIT-TRANS)                        
00475                 FROM          (BATCH-TO-PROCESS)                  
00476                 LENGTH        (PASS-AREA-LEN)                     
00477              END-EXEC                                             
00478              MOVE SPACES         TO PI-UPDATE-SW                  
00479                                     PI-KEYED-SWITCHES             
00480                                     PI-AGE-KEYED-SW               
00481                                     PI-BIRTHDT-KEYED-SW           
00482                                     PI-ACCT-AGENT-PROCESSED-SW    
00483                                     PI-SAV-REFERENCE              
00484              MOVE ZEROS          TO PI-ISS-CNT-ENTERED            
00485                                     PI-CAN-CNT-ENTERED            
00486                                     EMI-ERROR                     
00487              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
00488              MOVE LOW-VALUES     TO EL630AO                       
00489              MOVE SPACE          TO PI-BROWSE-SW                  
00490              MOVE QUESTION-MARKS TO BATCHI                        
00491              MOVE AL-UANON       TO BATCHA                        
00492              MOVE -1             TO MAINTL                        
00493              GO TO 8100-SEND-INITIAL-MAP                          
00494         ELSE                                                      
00495              MOVE -1             TO MAINTL                        
00496              MOVE ER-2211        TO EMI-ERROR                     
00497              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
00498              GO TO 8200-SEND-DATAONLY.                            
00499                                                                   
00500      IF EIBAID = DFHPF3                                           
00501          IF NOT PI-DATA-UPDATED                                   
00502              MOVE SPACE          TO PI-EL630-FIRST-TIME-SW        
00503              IF PI-AR-PROCESSING                                  
00504                 MOVE XCTL-635       TO PGM-NAME                   
00505                 GO TO 9300-XCTL                                   
00506              ELSE                                                 
00507                 IF PI-COMPANY-ID = 'DMD'                          
00508                    MOVE XCTL-633DMD TO PGM-NAME                   
00509                    GO TO 9300-XCTL                                
00510                 ELSE                                              
00511                    MOVE XCTL-633    TO PGM-NAME                   
00512                    GO TO 9300-XCTL                                
00513          ELSE                                                     
00514              MOVE -1             TO BATCHL                        
00515              MOVE AL-UABON       TO BATCHA                        
00516              MOVE ER-2213        TO EMI-ERROR                     
00517              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
00518              GO TO 8200-SEND-DATAONLY.                            
00519                                                                   
00520      IF EIBAID = DFHPF4                                           
00521          IF NOT PI-EL630-FIRST-TIME                               
00522              MOVE XCTL-6302      TO PGM-NAME                      
00523              GO TO 9300-XCTL                                      
00524          ELSE                                                     
00525              PERFORM 0400-PRIME-BATCH-TOTALS THRU 0490-EXIT       
00526              IF EMI-ERROR = ZEROS                                 
00527                  MOVE 'N'        TO PI-EL630-FIRST-TIME-SW        
00528                  MOVE XCTL-6302  TO PGM-NAME                      
00529                  GO TO 9300-XCTL                                  
00530              ELSE                                                 
00531                  GO TO 8200-SEND-DATAONLY.                        
00532                                                                   
00533      IF EIBAID = DFHPF5                                           
00534              IF PI-EL630-FIRST-TIME                               
00535                  MOVE -1         TO MAINTL                        
00536                  GO TO 8200-SEND-DATAONLY                         
00537              ELSE                                                 
00538              IF PI-ISS-CNT-ENTERED = ZEROS  AND                   
00539                 PI-CAN-CNT-ENTERED = ZEROS                        
00540                  MOVE LOW-VALUES TO     EL630AO                   
00541                  MOVE QUESTION-MARKS TO BATCHI                    
00542                  MOVE AL-UANON   TO     BATCHA                    
00543                  MOVE -1         TO     MAINTL                    
00544                  GO TO 8100-SEND-INITIAL-MAP                      
00545              ELSE                                                 
00546                  MOVE -1         TO BATCHL                        
00547                  MOVE AL-UABON   TO BATCHA                        
00548                  MOVE ER-2213    TO EMI-ERROR                     
00549                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
00550                  GO TO 4000-SHOW-TOTALS.                          
00551                                                                   
00552      IF EIBAID = DFHPF2                                           
00553         MOVE  AL-UANON           TO  BATCHA                       
00554         MOVE  BATCHI             TO PI-SAV-ENTRY-BATCH            
00555         GO TO 3000-DELETE-ENTERED-BATCH.                          
00556                                                                   
00557      IF  MAINTI = 'S'                                             
00558          MOVE  MAINTI            TO  PI-MAINT-FUNC                
00559          PERFORM 0400-PRIME-BATCH-TOTALS  THRU  0490-EXIT         
00560              IF EMI-ERROR = ZEROS                                 
00561                  MOVE 'N'        TO PI-EL630-FIRST-TIME-SW        
00562                  GO TO 4000-SHOW-TOTALS                           
00563              ELSE                                                 
00564                  GO TO 8200-SEND-DATAONLY.                        
00565                                                                   
00566      IF EIBAID = DFHENTER                                         
00567          GO TO 0330-EDIT-DATA.                                    
00568                                                                   
00569  0320-INPUT-ERROR.                                                
00570      MOVE ER-0029                TO EMI-ERROR.                    
00571      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00572      MOVE AL-UNBON               TO PFENTERA.                     
00573      IF PFENTERL = 0                                              
00574          MOVE -1                 TO MAINTL                        
00575      ELSE                                                         
00576          MOVE -1                 TO PFENTERL.                     
00577                                                                   
00578      GO TO 8200-SEND-DATAONLY.                                    
00579                                                                   
00580      EJECT                                                        
00581  0330-EDIT-DATA.                                                  
00582      IF MODIFY-CAP                                                
00583          NEXT SENTENCE                                            
00584        ELSE                                                       
00585          IF MAINTI NOT = 'S'                                      
00586            MOVE 'UPDATE'       TO SM-READ                         
00587            PERFORM 9995-SECURITY-VIOLATION                        
00588                               THRU 9995-EXIT                      
00589            MOVE ER-0070        TO EMI-ERROR                       
00590            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
00591            GO TO 8100-SEND-INITIAL-MAP.                           
00592                                                                   
00593      MOVE SPACE                  TO  PI-NB-MONTH-END-DT.          
00594                                                                   
00595      PERFORM 1250-READ-COMPANY-REC THRU 1250-EXIT.                
00596                                                                   
00597      IF MAINTI = 'N' OR 'C' OR 'B' OR 'K'                         
00598          NEXT SENTENCE                                            
00599      ELSE                                                         
00600          MOVE ER-0023            TO EMI-ERROR                     
00601          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00602          MOVE -1                 TO MAINTL                        
00603          MOVE AL-UABON           TO MAINTA                        
00604          GO TO 8200-SEND-DATAONLY.                                
00605                                                                   
00606      IF PI-CARRIER-SECURITY GREATER SPACES                        
00607          IF CARRIERL GREATER ZEROS                                
00608            IF PI-CARRIER-SECURITY = CARRIERI                      
00609               MOVE AL-UANON       TO CARRIERA                     
00610             ELSE                                                  
00611               MOVE -1             TO CARRIERL                     
00612               MOVE AL-UABON       TO CARRIERA                     
00613               MOVE ER-2370       TO EMI-ERROR                     
00614               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           
00615                                                                   
00616      IF PI-ACCOUNT-SECURITY GREATER SPACES                        
00617          IF ACCOUNTL GREATER ZEROS                                
00618              IF PI-ACCOUNT-SECURITY = ACCOUNTI                    
00619                  MOVE AL-UANON   TO ACCOUNTA                      
00620                ELSE                                               
00621                  MOVE -1         TO ACCOUNTL                      
00622                  MOVE AL-UABON   TO ACCOUNTA                      
00623                  MOVE ER-2371   TO EMI-ERROR                      
00624                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        
00625                                                                   
00626      IF  NOT EMI-NO-ERRORS                                        
00627          GO TO 8200-SEND-DATAONLY.                                
00628                                                                   
00629      MOVE MAINTI                 TO PI-MAINT-FUNC.                
00630      MOVE AL-UANON               TO MAINTA.                       
00631                                                                   
00632      MOVE PI-COMPANY-CD          TO PI-SAV-COMP-CD.               
00633      MOVE ZEROS                  TO PI-SAV-BATCH-SEQ              
00634                                     PI-SAV-BATCH-CHG-SEQ.         
00635      MOVE SPACES                 TO PI-SAV-CERT-EFF-DT            
00636                                     PI-SAV-CERT-NO.               
00637                                                                   
00638      IF BATCHL = ZEROS                                            
00639          MOVE -1                 TO BATCHL                        
00640          MOVE AL-UNBON           TO BATCHA                        
00641          MOVE ER-2201            TO EMI-ERROR                     
00642          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00643      ELSE                                                         
00644          IF BATCHI = QUESTION-MARKS OR SPACES                     
00645              IF MAINTI = 'N'                                      
00646                  PERFORM 1300-GET-BATCH-NO THRU 1300-EXIT         
00647              ELSE                                                 
00648                  MOVE -1         TO BATCHL                        
00649                  MOVE AL-UNBON   TO BATCHA                        
00650                  MOVE ER-2201    TO EMI-ERROR                     
00651                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
00652          ELSE
103001             MOVE SPACE TO WS-SWITCH
103001             PERFORM 0335-FAT-FINGER-EDIT VARYING PIC6-SUB
103001                 FROM +1 BY +1
103001                 UNTIL PIC6-SUB > +6 OR QUESTION-MARK-FOUND.
103001
103001     IF QUESTION-MARK-FOUND
103001         IF MAINTI = 'N'
103001             PERFORM 1300-GET-BATCH-NO THRU 1300-EXIT
103001         ELSE
103001             MOVE -1         TO BATCHL
103001             MOVE AL-UNBON   TO BATCHA
103001             MOVE ER-2201    TO EMI-ERROR
103001             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
103001     ELSE
00653          IF BATCHI = PI-SAV-ENTRY-BATCH
00654              MOVE AL-UANON   TO BATCHA
00655          ELSE
00656              MOVE BATCHI TO PI-SAV-ENTRY-BATCH
00657              IF MAINTI = 'C' OR 'B'
00658                  MOVE AL-UANON TO BATCHA
00659                  MOVE ZEROS      TO ELFISSL  ELFCANL
00660                                     EAHISSL  EAHCANL
00661                                     EISSCNTL ECANCNTL
00662                                     PI-LF-ISS-REMITTED            
00663                                     PI-LF-CAN-REMITTED            
00664                                     PI-AH-ISS-REMITTED            
00665                                     PI-AH-CAN-REMITTED            
00666                                     PI-ISS-CNT-REMITTED           
00667                                     PI-CAN-CNT-REMITTED.          
00668                                                                   
00669      IF REFL GREATER ZEROS                                        
00670         MOVE REFI                TO PI-SAV-REFERENCE.             
00671                                                                   
00672      IF MAINTI = 'C' OR 'B'                                       
00673          GO TO 0340-CHECK-TOTALS.                                 
00674                                                                   
00675      IF CARRIERL GREATER ZEROS                                    
00676          MOVE AL-UANON           TO CARRIERA                      
00677          PERFORM 1000-VERIFY-CARRIER-ID THRU 1000-EXIT            
00678      ELSE                                                         
00679          IF NOT ST-ACCNT-CNTL AND NOT ACCNT-CNTL                  
00680              MOVE -1             TO CARRIERL                      
00681              MOVE AL-UABON       TO CARRIERA                      
00682              MOVE ER-0194        TO EMI-ERROR                     
00683              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
00684                                                                   
00685      IF GROUPL GREATER ZEROS                                      
00686          MOVE AL-UANON           TO GROUPA                        
00687      ELSE                                                         
00688          IF CARR-GROUP-ST-ACCNT-CNTL                              
00689              MOVE -1 TO          GROUPL                           
00690              MOVE AL-UABON       TO GROUPA                        
00691              MOVE ER-0195        TO EMI-ERROR                     
00692              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
00693                                                                   
00694      IF STATEL GREATER ZEROS                                      
00695          MOVE AL-UANON           TO STATEA                        
00696          PERFORM 1100-VERIFY-STATE-ID THRU 1100-EXIT              
00697      ELSE                                                         
00698          IF NOT ACCNT-CNTL AND NOT CARR-ACCNT-CNTL                
00699              MOVE -1             TO STATEL                        
00700              MOVE AL-UABON       TO STATEA                        
00701              MOVE ER-0196        TO EMI-ERROR                     
00702              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
00703                                                                   
00704      IF ACCOUNTL GREATER ZEROS                                    
00705          MOVE AL-UANON           TO ACCOUNTA                      
00706          PERFORM 1200-VERIFY-ACCOUNT THRU 1200-EXIT               
00707      ELSE                                                         
00708          MOVE -1 TO ACCOUNTL                                      
00709          MOVE AL-UABON           TO ACCOUNTA                      
00710          MOVE ER-0197            TO EMI-ERROR                     
00711          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
103001
103001 0335-FAT-FINGER-EDIT.
103001
103001     IF BATCHI (PIC6-SUB:1) = QUESTION-MARK
103001         SET QUESTION-MARK-FOUND TO TRUE
103001     END-IF.
00712                                                                   
00713  0340-CHECK-TOTALS.                                               
00714 ******************************************************************
00715 *        IF MAINTENANCE FUNCTION = 'K' (COPY BATCH)              *
00716 *           GO TO CHECK ERRORS.                                  *
00717 ******************************************************************
00718                                                                   
00719      IF  MAINTI = 'K'                                             
00720          GO TO 0340-ERROR-CHECK.                                  
00721                                                                   
00722 *    ********************************************************     
00723 *    *         THE FOLLOWING FIELDS ARE NOT REQUIRED        *     
00724 *    *         IF ENTERED THE ONLY REQUIREMENT IS NUMERIC   *     
00725 *    ********************************************************     
00726                                                                   
00727      IF ELFISSL GREATER ZEROS                                     
00728          MOVE AL-UNNON           TO ELFISSA                       
00729          EXEC CICS BIF DEEDIT                                     
00730              FIELD   (ELFISSI)                                    
00731              LENGTH  (12)                                         
00732          END-EXEC                                                 
00733          MOVE ELFISSI            TO PI-LF-ISS-REMITTED            
00734      ELSE                                                         
00735          MOVE ZEROS              TO PI-LF-ISS-REMITTED.           
00736                                                                   
00737      IF ELFCANL GREATER ZEROS                                     
00738          MOVE AL-UNNON           TO ELFCANA                       
00739          EXEC CICS BIF DEEDIT                                     
00740              FIELD   (ELFCANI)                                    
00741              LENGTH  (10)                                         
00742          END-EXEC                                                 
00743          MOVE ELFCANI            TO PI-LF-CAN-REMITTED            
00744      ELSE                                                         
00745          MOVE ZEROS              TO PI-LF-CAN-REMITTED.           
00746                                                                   
00747      IF EAHISSL GREATER ZEROS                                     
00748          MOVE AL-UNNON           TO EAHISSA                       
00749          EXEC CICS BIF DEEDIT                                     
00750              FIELD   (EAHISSI)                                    
00751              LENGTH  (12)                                         
00752          END-EXEC                                                 
00753          MOVE EAHISSI            TO PI-AH-ISS-REMITTED            
00754      ELSE                                                         
00755          MOVE ZEROS              TO PI-AH-ISS-REMITTED.           
00756                                                                   
00757      IF EAHCANL GREATER ZEROS                                     
00758          MOVE AL-UNNON           TO EAHCANA                       
00759          EXEC CICS BIF DEEDIT                                     
00760              FIELD   (EAHCANI)                                    
00761              LENGTH  (10)                                         
00762          END-EXEC                                                 
00763          MOVE EAHCANI            TO PI-AH-CAN-REMITTED            
00764      ELSE                                                         
00765          MOVE ZEROS              TO PI-AH-CAN-REMITTED.           
00766                                                                   
00767      IF EISSCNTL GREATER ZEROS                                    
00768          MOVE AL-UNNON           TO EISSCNTA                      
00769          EXEC CICS BIF DEEDIT                                     
00770              FIELD (EISSCNTI)                                     
00771              LENGTH(6)                                            
00772          END-EXEC                                                 
00773          MOVE EISSCNTI           TO PI-ISS-CNT-REMITTED           
00774      ELSE                                                         
00775          MOVE ZEROS              TO PI-ISS-CNT-REMITTED.          
00776                                                                   
00777      IF ECANCNTL GREATER ZEROS                                    
00778          MOVE AL-UNNON           TO ECANCNTA                      
00779          EXEC CICS BIF DEEDIT                                     
00780              FIELD  (ECANCNTI)                                    
00781              LENGTH (6)                                           
00782          END-EXEC                                                 
00783          MOVE ECANCNTI           TO PI-CAN-CNT-REMITTED           
00784      ELSE                                                         
00785          MOVE ZEROS              TO PI-CAN-CNT-REMITTED.          
00786                                                                   
00787      IF MNTHNDTL GREATER ZEROS                                    
00788         NEXT SENTENCE                                             
00789        ELSE                                                       
00790         GO TO 0340-RECEIVED-DATE.                                 
00791                                                                   
00792      MOVE MNTHNDTI               TO WS-DT-DEEDIT-FIELD.           
00793      PERFORM 8600-DEEDIT THRU 8600-EXIT.                          
00794                                                                   
00795      MOVE WS-DEEDIT-FIELD-DATE-OUT                                
00796                                  TO DC-GREG-DATE-1-MDY.           
00797      MOVE '4'          TO DC-OPTION-CODE.                         
00798      PERFORM 9700-DATE-LINK.                                      
00799                                                                   
00800      IF DATE-CONVERSION-ERROR                                     
00801         GO TO 0340-MNTH-DAY-ERROR.                                
00802                                                                   
00803      MOVE DC-GREG-DATE-1-EDIT    TO MNTHNDTO.                     
00804      MOVE DC-BIN-DATE-1          TO PI-NB-MONTH-END-DT.           
00805      MOVE DC-GREG-DATE-1-MDY     TO DATE-TEST-AREA.               
00806                                                                   
00807      IF DATE-TEST-DD = DC-DAYS-IN-MONTH OR 01                     
00808         CONTINUE                                                  
00809      ELSE                                                         
00810         GO TO 0340-MNTH-DAY-ERROR.                                
00811                                                                   
00812  0340-RECEIVED-DATE.                                              
00813                                                                   
00814      IF RECEVDTL GREATER ZEROS                                    
00815         NEXT SENTENCE                                             
00816        ELSE                                                       
00817         MOVE WS-CURRENT-DT       TO RECEVDTI.                     
00818                                                                   
00819      MOVE RECEVDTI               TO WS-DT-DEEDIT-FIELD.           
00820      PERFORM 8600-DEEDIT THRU 8600-EXIT.                          
00821                                                                   
00822      MOVE WS-DEEDIT-FIELD-DATE-OUT                                
00823                                  TO DC-GREG-DATE-1-MDY.           
00824      MOVE '4'          TO DC-OPTION-CODE.                         
00825      PERFORM 9700-DATE-LINK.                                      
00826                                                                   
00827      IF DATE-CONVERSION-ERROR                                     
00828         GO TO 0340-RECEV-DAY-ERROR.                               
00829                                                                   
00830      MOVE DC-GREG-DATE-1-EDIT    TO RECEVDTO.                     
00831      MOVE DC-BIN-DATE-1          TO PI-RECEIVED-DT.               
00832                                                                   
00833      GO TO 0340-ERROR-CHECK.                                      
00834                                                                   
00835  0340-MNTH-DAY-ERROR.                                             
00836                                                                   
00837      MOVE -1       TO MNTHNDTL.                                   
00838      MOVE AL-UABON TO MNTHNDTA.                                   
00839      MOVE ER-0587  TO EMI-ERROR.                                  
00840      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00841      GO TO 0340-ERROR-CHECK.                                      
00842                                                                   
00843  0340-RECEV-DAY-ERROR.                                            
00844                                                                   
00845      MOVE -1       TO RECEVDTL.                                   
00846      MOVE AL-UABON TO RECEVDTA.                                   
00847      MOVE ER-0905  TO EMI-ERROR.                                  
00848      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00849                                                                   
00850  0340-ERROR-CHECK.                                                
00851                                                                   
00852      IF NOT EMI-NO-ERRORS                                         
00853          GO TO 8200-SEND-DATAONLY.                                
00854                                                                   
00855      IF PI-NB-MONTH-END-DT EQUAL LOW-VALUES OR SPACES             
00856         GO TO 0345-END-DATE-CHECK.                                
00857                                                                   
00858      IF PI-NB-MONTH-END-DT LESS THAN PI-CR-MONTH-END-DT           
00859         MOVE PI-NB-MONTH-END-DT  TO DC-BIN-DATE-1                 
00860         MOVE PI-CR-MONTH-END-DT  TO DC-BIN-DATE-2                 
00861      ELSE                                                         
00862      IF PI-CR-MONTH-END-DT LESS THAN PI-NB-MONTH-END-DT           
00863         MOVE PI-NB-MONTH-END-DT  TO DC-BIN-DATE-2                 
00864         MOVE PI-CR-MONTH-END-DT  TO DC-BIN-DATE-1                 
00865      ELSE                                                         
00866         GO TO 0345-END-DATE-CHECK.                                
00867                                                                   
00868      MOVE  '1'                   TO  DC-OPTION-CODE.              
00869      PERFORM  9700-DATE-LINK.                                     
00870                                                                   
00871      IF (DATE-CONVERSION-ERROR)                                   
00872                    OR                                             
00873         ((DC-ELAPSED-MONTHS GREATER THAN +1) AND                  
00874          (PI-COMPANY-ID NOT EQUAL 'DMD'))                         
00875         MOVE -1       TO MNTHNDTL                                 
00876         MOVE AL-UABON TO MNTHNDTA                                 
00877         MOVE ER-0587  TO EMI-ERROR                                
00878         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 
00879                                                                   
00880  0345-END-DATE-CHECK.                                             
00881                                                                   
00882      IF NOT EMI-NO-ERRORS                                         
00883          GO TO 8200-SEND-DATAONLY.                                
00884                                                                   
00885      IF PI-DELETE-IS-OK                                           
00886          MOVE SPACE              TO PI-VERIFY-DELETE-SW.          
00887                                                                   
00888      IF MAINTI = 'N'                                              
00889        AND PI-DATA-UPDATED                                        
00890          MOVE 'C'                TO PI-MAINT-FUNC                 
00891          GO TO 0360-XCTL-EL6301.                                  
00892                                                                   
00893      EXEC CICS HANDLE CONDITION                                   
00894          NOTFND  (0350-NO-RECORDS)                                
00895          ENDFILE (0350-NO-RECORDS)                                
00896      END-EXEC.                                                    
00897                                                                   
00898      EXEC CICS READ                                               
00899          SET     (ADDRESS OF PENDING-BUSINESS)                    
00900          DATASET (ERPNDB-FILE-ID)                                 
00901          RIDFLD  (PI-SAV-ENDING-ERPNDB-KEY)                       
00902          GTEQ                                                     
00903      END-EXEC.                                                    
00904                                                                   
00905      IF PI-SAV-COMP-CD     = PB-COMPANY-CD  AND                   
00906         PI-SAV-ENTRY-BATCH = PB-ENTRY-BATCH                       
00907          IF MAINTI = 'N'                                          
00908              MOVE ER-2229        TO EMI-ERROR                     
00909              MOVE -1             TO MAINTL                        
00910              MOVE AL-UABON       TO MAINTA                        
00911              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
00912              GO TO 8200-SEND-DATAONLY                             
00913          ELSE                                                     
00914              NEXT SENTENCE                                        
00915      ELSE                                                         
00916          GO TO 0350-NO-RECORDS.                                   
00917                                                                   
00918      IF PI-CARRIER-SECURITY GREATER SPACES                        
00919         IF PB-CARRIER = PI-CARRIER-SECURITY                       
00920            NEXT SENTENCE                                          
00921          ELSE                                                     
00922            GO TO 0350-NO-RECORDS.                                 
00923                                                                   
00924      IF PI-ACCOUNT-SECURITY GREATER SPACES                        
00925         IF PB-ACCOUNT = PI-ACCOUNT-SECURITY                       
00926            NEXT SENTENCE                                          
00927          ELSE                                                     
00928            GO TO 0350-NO-RECORDS.                                 
00929                                                                   
00930      IF PB-BILLED-DT NOT = LOW-VALUES                             
00931          MOVE ER-2402            TO EMI-ERROR                     
00932          MOVE -1                 TO BATCHL                        
00933          MOVE AL-UABON           TO BATCHA                        
00934          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00935          GO TO 8200-SEND-DATAONLY.                                
00936                                                                   
00937 ******************************************************************
00938 *        IF MAINTENANCE FUNCTION = 'K'                           *
00939 *           REWRITE ENTIRE BATCH WITH NEW ACCOUNT INFORMATION.   *
00940 ******************************************************************
00941                                                                   
00942      IF  PI-SAV-COMP-CD = PB-COMPANY-CD                           
00943          IF  PI-SAV-ENTRY-BATCH = PB-ENTRY-BATCH                  
00944              IF  MAINTI = 'K'                                     
00945                  GO TO 6000-REWRITE-ENTIRE-BATCH.                 
00946                                                                   
00947      IF PI-SAV-COMP-CD     = PB-COMPANY-CD   AND                  
00948         PI-SAV-ENTRY-BATCH = PB-ENTRY-BATCH  AND                  
00949         MAINTI NOT = 'N'                                          
00950          MOVE PB-CARRIER         TO PI-SAV-CARRIER                
00951                                     ACCT-CARRIER                  
00952          MOVE PB-GROUPING        TO PI-SAV-GROUPING               
00953                                     ACCT-GROUPING                 
00954          MOVE PB-STATE           TO PI-SAV-STATE                  
00955                                     ACCT-STATE                    
00956          MOVE PB-ACCOUNT         TO PI-SAV-ACCOUNT                
00957                                     ACCT-ACCOUNT                  
00958          PERFORM 1200-READ-ACCOUNT THRU 1200-EXIT                 
00959          PERFORM 2000-WRITE-BATCH-TOTAL-REC THRU 2990-EXIT        
00960          MOVE 'EL630B'           TO PI-MAP-NAME.                  
00961          GO TO 0360-XCTL-EL6301.                                  
00962                                                                   
00963  0350-NO-RECORDS.                                                 
00964      IF MAINTI = 'N'                                              
00965          MOVE SPACE              TO PI-ISSUE-ADDED-SW             
00966          MOVE ZEROS              TO PI-LF-ISS-ENTERED             
00967                                     PI-LF-CAN-ENTERED             
00968                                     PI-AH-ISS-ENTERED             
00969                                     PI-AH-CAN-ENTERED             
00970                                     PI-ISS-CNT-ENTERED            
00971                                     PI-CAN-CNT-ENTERED            
00972          PERFORM 2000-WRITE-BATCH-TOTAL-REC THRU 2990-EXIT        
00973          IF PI-LF-ISS-REMITTED  = ZEROS AND                       
00974             PI-AH-ISS-REMITTED  = ZEROS AND                       
00975             PI-LF-CAN-REMITTED  = ZEROS AND                       
00976             PI-AH-CAN-REMITTED  = ZEROS AND                       
00977             PI-ISS-CNT-REMITTED = ZEROS AND                       
00978             PI-CAN-CNT-REMITTED = ZEROS                           
00979              MOVE 'EL630B'     TO PI-MAP-NAME                     
00980              GO TO 0360-XCTL-EL6301                               
00981          ELSE                                                     
00982              IF PI-LF-ISS-REMITTED  = ZEROS AND                   
00983                 PI-AH-ISS-REMITTED  = ZEROS AND                   
00984                 PI-ISS-CNT-REMITTED = ZEROS                       
00985                  MOVE 'EL630C'     TO PI-MAP-NAME                 
00986                  GO TO 0360-XCTL-EL6301                           
00987              ELSE                                                 
00988                  MOVE 'EL630B'     TO PI-MAP-NAME                 
00989                  GO TO 0360-XCTL-EL6301                           
00990      ELSE                                                         
00991          MOVE ER-2212            TO EMI-ERROR                     
00992          MOVE -1                 TO BATCHL                        
00993          MOVE AL-UABON           TO BATCHA                        
00994          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00995          GO TO 8200-SEND-DATAONLY.                                
00996                                                                   
00997  0360-XCTL-EL6301.                                                
00998      IF PI-MAINT-FUNC = 'N'                                       
00999          MOVE SPACES             TO PI-KEYED-SWITCHES             
01000                                     PI-AGE-KEYED-SW               
01001                                     PI-BIRTHDT-KEYED-SW.          
01002                                                                   
020816     if pi-company-id = 'VPP'
020816        move 'VP6301'            to pgm-name
020816        move 'VP630B'            to PI-MAP-NAME
020816     else
              MOVE XCTL-6301           TO PGM-NAME
020816     end-if
01004                                                                   
01005      GO TO 9300-XCTL.                                             
01006      EJECT                                                        
01007  0400-PRIME-BATCH-TOTALS.                                         
01008      EXEC CICS HANDLE CONDITION                                   
01009          NOTFND (0480-NO-BATCH-TRAILER)                           
01010      END-EXEC.                                                    
01011                                                                   
01012      MOVE PI-COMPANY-CD          TO PNDB-COMP-CD.                 
01013      MOVE 9999                   TO PNDB-BATCH-SEQ.               
01014      MOVE BATCHI                 TO PNDB-ENTRY-BATCH.             
01015                                                                   
01016      EXEC CICS READ                                               
01017          DATASET (ERPNDB-FILE-ID)                                 
01018          SET     (ADDRESS OF PENDING-BUSINESS)                    
01019          RIDFLD  (ERPNDB-KEY)                                     
01020      END-EXEC.                                                    
01021                                                                   
01022      IF PB-BILLED-DT NOT = LOW-VALUES                             
01023          MOVE ER-2402            TO EMI-ERROR                     
01024          MOVE -1                 TO BATCHL                        
01025          MOVE AL-UABON           TO BATCHA                        
01026          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
01027          GO TO 0490-EXIT.                                         
01028                                                                   
01029      MOVE SPACES                   TO PI-ACCT-AGENT-PROCESSED-SW. 
01030      MOVE PI-COMPANY-CD            TO PI-SAV-COMP-CD.             
01031      MOVE PB-ENTRY-BATCH           TO PI-SAV-ENTRY-BATCH.         
01032                                                                   
01033      MOVE PB-REFERENCE             TO PI-SAV-REFERENCE.           
01034      MOVE PB-CSR-ID                TO PI-CSR-ID.                  
01035                                                                   
01036      MOVE PB-CARRIER               TO PI-SAV-CARRIER.             
01037      MOVE PB-GROUPING              TO PI-SAV-GROUPING.            
01038      MOVE PB-STATE                 TO PI-SAV-STATE.               
01039      MOVE PB-ACCOUNT               TO PI-SAV-ACCOUNT.             
01040      MOVE PB-B-LF-ISS-PRM-REMITTED TO PI-LF-ISS-REMITTED.         
01041      MOVE PB-B-LF-ISS-PRM-ENTERED  TO PI-LF-ISS-ENTERED.          
01042      MOVE PB-B-AH-ISS-PRM-REMITTED TO PI-AH-ISS-REMITTED.         
01043      MOVE PB-B-AH-ISS-PRM-ENTERED  TO PI-AH-ISS-ENTERED.          
01044      MOVE PB-B-LF-CAN-PRM-REMITTED TO PI-LF-CAN-REMITTED.         
01045      MOVE PB-B-LF-CAN-PRM-ENTERED  TO PI-LF-CAN-ENTERED.          
01046      MOVE PB-B-AH-CAN-PRM-REMITTED TO PI-AH-CAN-REMITTED.         
01047      MOVE PB-B-AH-CAN-PRM-ENTERED  TO PI-AH-CAN-ENTERED.          
01048      MOVE PB-B-ISSUE-CNT-REMITTED  TO PI-ISS-CNT-REMITTED.        
01049      MOVE PB-B-ISSUE-CNT-ENTERED   TO PI-ISS-CNT-ENTERED.         
01050      MOVE PB-B-CANCEL-CNT-REMITTED TO PI-CAN-CNT-REMITTED.        
01051      MOVE PB-B-CANCEL-CNT-ENTERED  TO PI-CAN-CNT-ENTERED.         
01052                                                                   
01053 ******************************************************************
01054 *    IF THE MONTH-END-DATE IN THE TRAILER RECORD (BATCH HEADER)  *
01055 *       IS NOT EQUAL TO THE ACCOUNTS MONTH-END-DATE PRIME THE    *
01056 *       TRAILER'S MONTH-END-DATE.                                *
01057 ******************************************************************
01058                                                                   
01059      MOVE SPACE                    TO PI-NB-MONTH-END-DT.         
01060      IF  PB-CREDIT-SELECT-DT = PI-CR-MONTH-END-DT                 
01061          NEXT SENTENCE                                            
01062        ELSE                                                       
01063          MOVE PB-CREDIT-SELECT-DT  TO  PI-NB-MONTH-END-DT.        
01064                                                                   
01065      IF  PB-B-RECEIVED-DT = SPACES OR LOW-VALUES                  
01066          MOVE PB-INPUT-DT          TO PI-RECEIVED-DT              
01067      ELSE                                                         
01068          MOVE PB-B-RECEIVED-DT     TO PI-RECEIVED-DT.             
01069                                                                   
01070      GO TO 0490-EXIT.                                             
01071                                                                   
01072  0480-NO-BATCH-TRAILER.                                           
01073      MOVE ER-2212                TO EMI-ERROR.                    
01074      MOVE -1                     TO BATCHL.                       
01075      MOVE AL-UABON               TO BATCHA.                       
01076      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01077                                                                   
01078  0490-EXIT.                                                       
01079      EXIT.                                                        
01080      EJECT                                                        
01081  1000-VERIFY-CARRIER-ID.                                          
01082      MOVE SPACES                 TO ELCNTL-KEY.                   
01083      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 
01084      MOVE '6'                    TO CNTL-REC-TYPE.                
01085      MOVE CARRIERI               TO CNTL-CARRIER                  
01086      MOVE +0                     TO CNTL-SEQ.                     
01087                                                                   
01088      EXEC CICS HANDLE CONDITION                                   
01089          NOTFND   (1000-NO-CARRIER)                               
01090      END-EXEC.                                                    
01091                                                                   
01092      EXEC CICS READ                                               
01093          DATASET   (ELCNTL-FILE-ID)                               
01094          SET       (ADDRESS OF CONTROL-FILE)                      
01095          RIDFLD    (ELCNTL-KEY)                                   
01096      END-EXEC.                                                    
01097                                                                   
01098      GO TO 1000-EXIT.                                             
01099                                                                   
01100  1000-NO-CARRIER.                                                 
01101      MOVE ER-2208                TO EMI-ERROR                     
01102      MOVE -1                     TO CARRIERL                      
01103      MOVE AL-UABON               TO CARRIERA                      
01104      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01105                                                                   
01106  1000-EXIT.                                                       
01107      EXIT.                                                        
01108      EJECT                                                        
01109  1100-VERIFY-STATE-ID.                                            
01110      MOVE SPACES                 TO ELCNTL-KEY.                   
01111      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 
01112      MOVE '3'                    TO CNTL-REC-TYPE.                
01113      MOVE STATEI                 TO CNTL-STATE.                   
01114      MOVE +0                     TO CNTL-SEQ.                     
01115                                                                   
01116      EXEC CICS HANDLE CONDITION                                   
01117          NOTFND   (1100-NO-STATE)                                 
01118      END-EXEC.                                                    
01119                                                                   
01120      EXEC CICS READ                                               
01121          DATASET   (ELCNTL-FILE-ID)                               
01122          SET       (ADDRESS OF CONTROL-FILE)                      
01123          RIDFLD    (ELCNTL-KEY)                                   
01124      END-EXEC.                                                    
01125                                                                   
01126      GO TO 1100-EXIT.                                             
01127                                                                   
01128  1100-NO-STATE.                                                   
01129      MOVE ER-2209                TO EMI-ERROR.                    
01130      MOVE -1                     TO STATEL.                       
01131      MOVE AL-UABON               TO STATEA.                       
01132      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01133      GO TO 8200-SEND-DATAONLY.                                    
01134                                                                   
01135  1100-EXIT.                                                       
01136      EXIT.                                                        
01137      EJECT                                                        
01138                                                                   
01139  1200-VERIFY-ACCOUNT.                                             
01140      IF CARRIERL GREATER ZEROS                                    
01141          MOVE CARRIERI           TO ACCT-CARRIER                  
01142      ELSE                                                         
01143          MOVE SPACES             TO ACCT-CARRIER.                 
01144                                                                   
01145      IF GROUPL GREATER ZEROS                                      
01146          MOVE GROUPI             TO ACCT-GROUPING                 
01147      ELSE                                                         
01148          MOVE SPACES             TO ACCT-GROUPING.                
01149                                                                   
01150      IF STATEL GREATER ZEROS                                      
01151          MOVE STATEI             TO ACCT-STATE                    
01152      ELSE                                                         
01153          MOVE SPACES             TO ACCT-STATE.                   
01154                                                                   
01155      MOVE ACCOUNTI               TO ACCT-ACCOUNT.                 
01156                                                                   
01157  1200-READ-ACCOUNT.                                               
01158      MOVE PI-COMPANY-CD          TO ACCT-CO.                      
01159      MOVE LOW-VALUES             TO ACCT-EXP-DATE                 
01160                                     PI-ACCT-LOW-EFF-DT            
01161                                     PI-ACCT-HIGH-EXP-DT.          
01162                                                                   
01163      EXEC CICS HANDLE CONDITION                                   
01164          NOTFND   (1200-ACCOUNT-INVALID)                          
01165          ENDFILE  (1200-ACCOUNT-INVALID)                          
01166      END-EXEC.                                                    
01167                                                                   
01168      EXEC CICS STARTBR                                            
01169          DATASET (ERACCT2-FILE-ID)                                
01170          RIDFLD  (ERACCT-KEY)                                     
01171      END-EXEC.                                                    
01172                                                                   
01173      MOVE ERACCT-KEY             TO ERACCT-SAVE-KEY.              
01174                                                                   
01175  1200-READ-LOOP.                                                  
01176      EXEC CICS READNEXT                                           
01177          DATASET   (ERACCT2-FILE-ID)                              
01178          SET       (ADDRESS OF ACCOUNT-MASTER)                    
01179          RIDFLD    (ERACCT-KEY)                                   
01180      END-EXEC.                                                    
01181                                                                   
01182      IF ERACCT-COMP-KEY NOT = ERACCT-SAVE-KEY                     
01183          GO TO 1200-ACCOUNT-INVALID.                              
01184                                                                   
01185      IF WS-ON1-SW1 IS EQUAL TO 'Y'                                
01186          MOVE 'N'                TO WS-ON1-SW1                    
01187          MOVE +0                 TO WS-SUB1                       
01188          MOVE AM-CARRIER         TO PI-SAV-FC-CARRIER             
01189          MOVE AM-GROUPING        TO PI-SAV-FC-GROUPING            
01190          MOVE AM-STATE           TO PI-SAV-FC-STATE               
01191          MOVE AM-VG-CARRIER      TO PI-SAV-CARRIER                
01192          MOVE AM-VG-GROUPING     TO PI-SAV-GROUPING               
01193          MOVE AM-VG-STATE        TO PI-SAV-STATE                  
01194          MOVE AM-VG-ACCOUNT      TO PI-SAV-ACCOUNT                
01195          MOVE AM-EFFECTIVE-DT    TO PI-ACCT-LOW-EFF-DT.           
01196                                                                   
01197      MOVE AM-EXPIRATION-DT       TO PI-ACCT-HIGH-EXP-DT.          
01198      MOVE AM-NAME                TO PI-AM-NAME.                   
030310     IF AM-EDIT-LOAN-OFC = 'Y'
030310        MOVE 'Y'                 TO PI-AM-EDIT-LOAN-OFC
030310     ELSE
030310        MOVE 'N'                 TO PI-AM-EDIT-LOAN-OFC
030310     END-IF
072308     IF AM-GPCD = 02 OR 03 OR 04 OR 05
072308        MOVE AM-PERSON           TO PI-AM-ADDR1
072308        MOVE AM-ADDRS            TO PI-AM-ADDR2
072308*       MOVE AM-CITY             TO PI-AM-CITYST
              MOVE AM-ADDR-CITY        TO PI-AM-CITY
              MOVE AM-ADDR-STATE       TO PI-AM-STATE
072308        MOVE AM-ZIP              TO PI-AM-ZIP
           ELSE
              MOVE SPACES TO PI-AM-ADDR1 PI-AM-ADDR2
                            PI-AM-CITYST PI-AM-ZIP
072308     END-IF
01199                                                                   
01200      MOVE AM-CARRIER             TO PI-COMP-CARRIER.              
01201      MOVE AM-GROUPING            TO PI-COMP-GROUPING.             
01202      MOVE AM-CSR-CODE            TO PI-CSR-ID.                    
01203                                                                   
01204      IF PI-AR-PROCESSING                                          
01205         MOVE 'Y'                 TO WS-RECORD-FOUND-SW            
01206         PERFORM 1260-FIND-ACCT-AGENT THRU 1260-EXIT               
01207         IF RECORD-NOT-FOUND                                       
01208            GO TO 8200-SEND-DATAONLY.                              
01209                                                                   
01210      GO TO 1200-READ-LOOP.                                        
01211                                                                   
01212  1200-ACCOUNT-INVALID.                                            
01213      IF PI-ACCT-LOW-EFF-DT NOT = LOW-VALUES                       
01214         MOVE 'Y'                 TO PI-ACCT-AGENT-PROCESSED-SW    
01215         GO TO 1200-EXIT.                                          
01216                                                                   
01217      IF CARR-GROUP-ST-ACCNT-CNTL                                  
01218          MOVE -1                     TO CARRIERL                  
01219          MOVE AL-UABON               TO CARRIERA                  
01220                                         GROUPA                    
01221                                         STATEA                    
01222                                         ACCOUNTA                  
01223      ELSE                                                         
01224          IF ST-ACCNT-CNTL                                         
01225              MOVE -1                 TO STATEL                    
01226              MOVE AL-UABON           TO STATEA                    
01227                                         ACCOUNTA                  
01228          ELSE                                                     
01229              IF CARR-ST-ACCNT-CNTL                                
01230                  MOVE -1             TO CARRIERL                  
01231                  MOVE AL-UABON       TO CARRIERA                  
01232                                         STATEA                    
01233                                         ACCOUNTA                  
01234              ELSE                                                 
01235                  IF ACCNT-CNTL                                    
01236                      MOVE -1         TO ACCOUNTL                  
01237                      MOVE AL-UABON   TO ACCOUNTA                  
01238                  ELSE                                             
01239                      MOVE -1         TO CARRIERL                  
01240                      MOVE AL-UABON   TO CARRIERA                  
01241                                         ACCOUNTA.                 
01242                                                                   
01243      MOVE ER-2210                TO EMI-ERROR.                    
01244      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01245                                                                   
01246  1200-EXIT.                                                       
01247      EXIT.                                                        
01248      EJECT                                                        
01249  1250-READ-COMPANY-REC.                                           
01250      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 
01251      MOVE '1'                    TO CNTL-REC-TYPE.                
01252      MOVE SPACES                 TO CNTL-ACCESS.                  
01253      MOVE +0                     TO CNTL-SEQ.                     
01254                                                                   
01255      EXEC CICS HANDLE CONDITION                                   
01256          NOTFND   (1250-NO-COMPANY-REC)                           
01257      END-EXEC.                                                    
01258                                                                   
01259      EXEC CICS READ                                               
01260          DATASET   (ELCNTL-FILE-ID)                               
01261          SET       (ADDRESS OF CONTROL-FILE)                      
01262          RIDFLD    (ELCNTL-KEY)                                   
01263      END-EXEC.                                                    
01264                                                                   
01265      MOVE CF-CREDIT-EDIT-CONTROLS TO PI-CREDIT-EDIT-CONTROLS.     
01266      GO TO 1250-EXIT.                                             
01267                                                                   
01268  1250-NO-COMPANY-REC.                                             
01269      MOVE ER-2248                TO EMI-ERROR.                    
01270      MOVE -1                     TO BATCHL.                       
01271      MOVE AL-UABON               TO CARRIERA                      
01272      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01273      GO TO 8200-SEND-DATAONLY.                                    
01274                                                                   
01275  1250-EXIT.                                                       
01276      EXIT.                                                        
01277      EJECT                                                        
01278                                                                   
01279  1260-FIND-ACCT-AGENT.                                            
01280                                                                   
01281 ******************************************************************
01282 *                                                                *
01283 *            F I N D   A C C O U N T   A G E N T                 *
01284 *                                                                *
01285 *    NOTE:  IT IS NECESSARY TO LOAD EVERY DATE RANGE FOR         *
01286 *           EACH ACCOUNT AGENT / FINANCIAL RESPONSIBILITY        *
01287 *           INTO THE PROGRAM INTERFACE AREA.  THE ACCOUNT        *
01288 *           DATE RANGES ARE CHECKED DURING DATA ENTRY IN         *
01289 *           EL6301.                                              *
01290 *                                                                *
01291 ******************************************************************
01292                                                                   
01293      IF PI-ACCT-AGENT-PROCESSED                                   
01294         GO TO 1260-EXIT.                                          
01295                                                                   
01296      IF WS-ON1-SW2 IS EQUAL TO 'Y'                                
01297         MOVE 'N'                 TO WS-ON1-SW2                    
01298      ELSE                                                         
01299         MOVE +0                  TO WS-SUB2                       
01300         GO TO 1260-CONT-FIND-ACCT-AGENT.                          
01301                                                                   
01302      MOVE +0                     TO WS-SUB2.                      
01303                                                                   
01304  1260-INITIALIZE-DATE-RANGES.                                     
01305      ADD +1                      TO WS-SUB2.                      
01306                                                                   
01307      IF WS-SUB2 GREATER +32                                       
01308         GO TO 1260-INITIALIZE-FINISHED.                           
01309                                                                   
01310      MOVE SPACES                 TO PI-ACCT-EFF-DT (WS-SUB2)      
01311                                     PI-ACCT-EXP-DT (WS-SUB2)      
01312                                     PI-REMIT-AGENT (WS-SUB2)      
01313                                     PI-ACCT-AGENT  (WS-SUB2).     
01314                                                                   
01315      GO TO 1260-INITIALIZE-DATE-RANGES.                           
01316                                                                   
01317  1260-INITIALIZE-FINISHED.                                        
01318      MOVE SPACES                 TO PI-ACCOUNT-AGENT              
01319                                     PI-FIN-RESP.                  
01320                                                                   
01321      MOVE 'N'                    TO PI-ACCT-AGENT-ERROR-SW        
01322                                     PI-FIN-RESP-ERROR-SW.         
01323                                                                   
01324      MOVE +0                     TO WS-SUB2.                      
01325                                                                   
01326  1260-CONT-FIND-ACCT-AGENT.                                       
01327      ADD  +1                     TO WS-SUB2.                      
01328                                                                   
01329      IF WS-SUB2 GREATER +10                                       
01330         MOVE  SPACES             TO PI-ACCT-AGENT (WS-SUB1)       
01331         GO TO 1260-EXIT.                                          
01332                                                                   
052814     IF AM-COM-TYP (WS-SUB2) = 'C' OR 'D' OR 'F'
01334         NEXT SENTENCE                                             
01335      ELSE                                                         
01336         GO TO 1260-CONT-FIND-ACCT-AGENT.                          
01337                                                                   
01338      IF WS-SUB1 = +0                                              
01339         MOVE +1                   TO WS-SUB1                      
01340                                      PI-SUB                       
01341         MOVE AM-EFFECTIVE-DT      TO PI-ACCT-EFF-DT (WS-SUB1)     
01342         MOVE AM-EXPIRATION-DT     TO PI-ACCT-EXP-DT (WS-SUB1)     
01343         MOVE AM-AGT (WS-SUB2)     TO PI-ACCT-AGENT  (WS-SUB1)     
01344         MOVE AM-AGT (AM-REMIT-TO) TO PI-REMIT-AGENT (WS-SUB1)     
01345         GO TO 1260-EXIT.                                          
01346                                                                   
01347      IF AM-AGT (WS-SUB2)     = PI-ACCT-AGENT  (WS-SUB1) AND       
01348         AM-AGT (AM-REMIT-TO) = PI-REMIT-AGENT (WS-SUB1)           
01349           MOVE AM-EXPIRATION-DT  TO PI-ACCT-EXP-DT (WS-SUB1)      
01350         ELSE                                                      
01351           ADD +1                      TO WS-SUB1                  
01352           IF WS-SUB1 GREATER +32                                  
01353               MOVE  ER-2119             TO EMI-ERROR              
01354               MOVE -1                   TO MAINTL                 
01355               PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT           
01356               MOVE 'N'                  TO WS-RECORD-FOUND-SW     
01357               GO TO 1260-EXIT                                     
01358           ELSE                                                    
01359            MOVE WS-SUB1              TO PI-SUB                    
01360            MOVE AM-EFFECTIVE-DT      TO PI-ACCT-EFF-DT (WS-SUB1)  
01361            MOVE AM-EXPIRATION-DT     TO PI-ACCT-EXP-DT (WS-SUB1)  
01362            MOVE AM-AGT (WS-SUB2)     TO PI-ACCT-AGENT  (WS-SUB1)  
01363            MOVE AM-AGT (AM-REMIT-TO) TO PI-REMIT-AGENT (WS-SUB1). 
01364                                                                   
01365  1260-EXIT.                                                       
01366       EXIT.                                                       
01367                                                                   
01368      EJECT                                                        
01369                                                                   
01370  1300-GET-BATCH-NO.                                               
01371      EXEC CICS HANDLE CONDITION                                   
01372          NOTFND   (1300-NO-COMPANY-REC)                           
01373      END-EXEC.                                                    
01374                                                                   
01375      EXEC CICS READ                                               
01376          DATASET   (ELCNTL-FILE-ID)                               
01377          SET       (ADDRESS OF CONTROL-FILE)                      
01378          RIDFLD    (ELCNTL-KEY)                                   
01379          UPDATE                                                   
01380      END-EXEC.                                                    
01381                                                                   
01382  1300-ASSIGN-NUMBER-LOOP.                                         
01383      IF CF-LAST-BATCH-RESET                                       
01384          MOVE ZEROS              TO CF-LAST-BATCH-NO              
01385          ADD +1                  TO CF-LAST-BATCH-NO              
01386      ELSE                                                         
01387          ADD +1                  TO CF-LAST-BATCH-NO.             
01388                                                                   
01389      MOVE CF-LAST-BATCH-NO       TO WS-BATCH-NO.                  
01390      MOVE WS-BATCH-NO            TO PI-SAV-ENTRY-BATCH.           
01391                                                                   
01392  1300-REWRITE-COMPANY-REC.                                        
01393      MOVE WS-BATCH-NO            TO BATCHI.                       
01394      MOVE AL-UANON               TO BATCHA.                       
01395                                                                   
01396      EXEC CICS REWRITE                                            
01397          DATASET (ELCNTL-FILE-ID)                                 
01398          FROM    (CONTROL-FILE)                                   
01399      END-EXEC.                                                    
01400                                                                   
01401      EXEC CICS SYNCPOINT                                          
01402      END-EXEC.                                                    
01403                                                                   
01404  1300-CHECK-FOR-DUP-BATCH.                                        
01405      EXEC CICS HANDLE CONDITION                                   
01406          NOTFND  (1300-EXIT)                                      
01407      END-EXEC.                                                    
01408                                                                   
01409      EXEC CICS READ                                               
01410          SET     (ADDRESS OF PENDING-BUSINESS)                    
01411          DATASET (ERPNDB-FILE-ID)                                 
01412          RIDFLD  (PI-SAV-ENDING-ERPNDB-KEY)                       
01413          GTEQ                                                     
01414      END-EXEC.                                                    
01415                                                                   
01416      IF PI-SAV-COMP-CD NOT = PB-COMPANY-CD                        
01417          GO TO 1300-EXIT.                                         
01418                                                                   
01419      IF PI-SAV-ENTRY-BATCH = PB-ENTRY-BATCH                       
01420          MOVE LOW-VALUES         TO EL630AO                       
01421          MOVE QUESTION-MARKS     TO BATCHO                        
01422          MOVE AL-UABON           TO BATCHA                        
01423          MOVE -1                 TO BATCHL                        
01424          MOVE ER-2229            TO EMI-ERROR                     
01425          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
01426          GO TO 8100-SEND-INITIAL-MAP.                             
01427                                                                   
01428      GO TO 1300-EXIT.                                             
01429                                                                   
01430  1300-NO-COMPANY-REC.                                             
01431      MOVE ER-2248                TO EMI-ERROR.                    
01432      MOVE -1                     TO BATCHL.                       
01433      MOVE AL-UABON               TO CARRIERA                      
01434      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01435      GO TO 8200-SEND-DATAONLY.                                    
01436                                                                   
01437  1300-EXIT.                                                       
01438      EXIT.                                                        
01439      EJECT                                                        
01440                                                                   
01441  1400-VERIFY-COMP-MASTER.                                         
01442                                                                   
01443 ******************************************************************
01444 *                                                                 
01445 *      V E R I F Y    C O M P E N S A T I O N    M A S T E R      
01446 *                                                                 
01447 *       READ COMPENSATION MASTER AND SAVE SUMMARY CODE FOR        
01448 *       A.R. PROCESSING.                                          
01449 *                                                                 
01450 ******************************************************************
01451                                                                   
01452      EXEC CICS HANDLE CONDITION                                   
01453          NOTFND   (1470-NO-COMP-MSTR)                             
01454      END-EXEC.                                                    
01455                                                                   
01456      MOVE SPACE                  TO WS-COMP-MASTER-ERROR-SW.      
01457      MOVE PI-COMPANY-CD          TO ERCOMP-COMP-CD.               
01458                                                                   
01459      IF PI-ZERO-CARRIER OR                                        
01460         PI-ZERO-CAR-GROUP                                         
01461         MOVE ZEROS               TO ERCOMP-CARRIER                
01462      ELSE                                                         
01463         MOVE PI-COMP-CARRIER     TO ERCOMP-CARRIER.               
01464                                                                   
01465      IF PI-ZERO-GROUPING OR                                       
01466         PI-ZERO-CAR-GROUP                                         
01467         MOVE ZEROS               TO ERCOMP-GROUPING               
01468      ELSE                                                         
01469         MOVE PI-COMP-GROUPING    TO ERCOMP-GROUPING.              
01470                                                                   
01471      MOVE PI-FIN-RESP            TO ERCOMP-FIN-RESP.              
01472      MOVE PI-ACCOUNT-AGENT       TO ERCOMP-ACCT.                  
01473      MOVE 'A'                    TO ERCOMP-RECORD-TYPE.           
01474                                                                   
01475      EXEC CICS READ                                               
01476          DATASET   (ERCOMP-FILE-ID)                               
01477          SET       (ADDRESS OF COMPENSATION-MASTER)               
01478          RIDFLD    (ERCOMP-KEY)                                   
01479      END-EXEC.                                                    
01480                                                                   
01481      MOVE CO-AR-SUMMARY-CODE     TO PI-SUMMARY-CODE.              
01482                                                                   
01483      IF REFL GREATER ZEROS                                        
01484          GO TO 1490-EXIT.                                         
01485                                                                   
01486      IF PI-SAV-REFERENCE GREATER SPACES                           
01487          GO TO 1490-EXIT.                                         
01488                                                                   
01489      IF CO-AR-NET-REPORT                                          
01490         MOVE 'Y'                 TO WS-UPDATE-REFERENCE-SW        
01491         MOVE WS-CURRENT-YMD      TO PI-SAV-REFERENCE              
01492      ELSE                                                         
01493         MOVE SPACES              TO PI-SAV-REFERENCE.             
01494                                                                   
01495      GO TO 1490-EXIT.                                             
01496                                                                   
01497  1470-NO-COMP-MSTR.                                               
01498      MOVE ER-2800                TO EMI-ERROR.                    
01499      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01500      MOVE 'Y'                    TO WS-COMP-MASTER-ERROR-SW.      
01501                                                                   
01502  1490-EXIT.                                                       
01503      EXIT.                                                        
01504                                                                   
01505      EJECT                                                        
01506                                                                   
01507 ******************************************************************
01508 *                                                                *
01509 *           W R I T E   R E Q U E S T   R E C O R D              *
01510 *                                                                *
01511 ******************************************************************
01512                                                                   
01513  1900-WRITE-REQUEST-RECORD.                                       
01514      EXEC CICS GETMAIN                                            
01515           SET       (ADDRESS OF AR-REQUEST-RECORD)                
01516           LENGTH    (200)                                         
01517           INITIMG   (GETMAIN-SPACE)                               
01518      END-EXEC.                                                    
01519                                                                   
01520      MOVE 'RQ'                   TO RQ-RECORD-ID.                 
01521                                                                   
01522      MOVE PI-SAV-ENTRY-BATCH     TO RQ-ENTRY-BATCH                
01523                                     RQ-BATCH-A1                   
01524                                     RQ-BATCH-A2                   
01525                                     RQ-BATCH-A3                   
01526                                     RQ-BATCH-A4.                  
01527                                                                   
01528      MOVE PI-SAV-REFERENCE       TO RQ-REFERENCE-A1               
01529                                     RQ-REFERENCE-A2               
01530                                     RQ-REFERENCE-A4.              
01531                                                                   
01532      MOVE PI-COMPANY-CD          TO RQ-COMPANY-CD                 
01533                                     RQ-COMPANY-CD-A1              
01534                                     RQ-COMPANY-CD-A2              
01535                                     RQ-COMPANY-CD-A3              
01536                                     RQ-COMPANY-CD-A4.             
01537                                                                   
01538      MOVE PI-SAV-FC-CARRIER      TO RQ-CARRIER-A1                 
01539                                     RQ-CARRIER-A2                 
01540                                     RQ-CARRIER-A3                 
01541                                     PI-CR-CARRIER.                
01542                                                                   
01543      MOVE PI-SAV-FC-GROUPING     TO RQ-GROUPING-A1                
01544                                     RQ-GROUPING-A2                
01545                                     RQ-GROUPING-A3                
01546                                     PI-CR-GROUPING.               
01547                                                                   
01548      IF PI-ZERO-CARRIER                                           
01549         MOVE ZERO                TO RQ-CARRIER-A2                 
01550                                     PI-CR-CARRIER.                
01551                                                                   
01552      IF PI-ZERO-GROUPING                                          
01553         MOVE ZEROS               TO RQ-GROUPING-A2                
01554                                     PI-CR-GROUPING.               
01555                                                                   
01556      IF PI-ZERO-CAR-GROUP                                         
01557         MOVE ZEROS               TO RQ-CARRIER-A2                 
01558                                     RQ-GROUPING-A2                
01559                                     PI-CR-CARRIER                 
01560                                     PI-CR-GROUPING.               
01561                                                                   
01562      MOVE PI-SAV-FC-STATE        TO RQ-STATE-A1                   
01563                                     PI-CR-STATE.                  
01564                                                                   
01565      MOVE PI-SAV-ACCOUNT         TO RQ-ACCOUNT-A1                 
01566                                     RQ-ACCOUNT-A4                 
01567                                     PI-CR-ACCOUNT.                
01568                                                                   
01569                                                                   
01570      MOVE PI-ACCOUNT-AGENT       TO RQ-ACCT-AGENT-A2              
01571                                     RQ-ACCT-AGENT-A3.             
01572                                                                   
01573      MOVE WS-CURRENT-BIN-DT      TO RQ-ENTRY-DT.                  
01574                                                                   
01575      IF  PI-NB-MONTH-END-DT GREATER SPACES                        
01576          MOVE PI-NB-MONTH-END-DT TO RQ-MO-END-DT                  
01577      ELSE                                                         
01578          MOVE PI-CR-MONTH-END-DT TO RQ-MO-END-DT.                 
01579                                                                   
01580      MOVE LOW-VALUES             TO RQ-REQUEST-DT                 
01581                                     RQ-STMT-DT                    
01582                                     RQ-REVERSAL-DT                
01583                                     RQ-CREDIT-SELECT-DT           
01584                                     RQ-CREDIT-ACCEPT-DT.          
01585                                                                   
01586      IF PI-FIN-RESP-ERROR                                         
01587         MOVE 'E'                 TO RQ-STATUS                     
01588         MOVE 'UNKNOWN'           TO RQ-FIN-RESP-A2                
01589                                     PI-CR-FIN-RESP                
01590      ELSE                                                         
01591         MOVE PI-FIN-RESP         TO RQ-FIN-RESP-A2                
01592                                     PI-CR-FIN-RESP.               
01593                                                                   
01594      IF PI-ACCT-AGENT-ERROR                                       
01595         MOVE 'E'                 TO RQ-STATUS                     
01596         MOVE 'UNKNOWN'           TO RQ-ACCT-AGENT-A2              
01597                                     RQ-ACCT-AGENT-A3.             
01598                                                                   
01599      IF WS-COMP-MASTER-ERROR                                      
01600         MOVE 'E'                 TO RQ-STATUS                     
01601         MOVE 'UNKNOWN'           TO PI-CR-FIN-RESP.               
01602                                                                   
01603      MOVE RQ-CONTROL-PRIMARY     TO ERRQST-KEY.                   
01604      MOVE LOW-VALUES             TO RQ-STMT-DT                    
01605                                     RQ-REQUEST-DT                 
01606                                     RQ-REVERSAL-DT.               
01607                                                                   
01608      MOVE CO-AR-SUMMARY-CODE     TO RQ-SUMMARY-CODE.              
01609                                                                   
01610      IF  PI-NB-MONTH-END-DT GREATER SPACES                        
01611          MOVE PI-NB-MONTH-END-DT TO PB-CREDIT-SELECT-DT           
01612      ELSE                                                         
01613          MOVE PI-CR-MONTH-END-DT TO PB-CREDIT-SELECT-DT.          
01614                                                                   
01615      EXEC CICS HANDLE CONDITION                                   
01616          DUPREC (1990-EXIT)                                       
01617      END-EXEC.                                                    
01618                                                                   
01619      EXEC CICS WRITE                                              
01620           DATASET    (ERRQST-FILE-ID)                             
01621           FROM       (AR-REQUEST-RECORD)                          
01622           RIDFLD     (ERRQST-KEY)                                 
01623      END-EXEC.                                                    
01624                                                                   
01625  1990-EXIT.                                                       
01626      EXIT.                                                        
01627                                                                   
01628      EJECT                                                        
01629  2000-WRITE-BATCH-TOTAL-REC.                                      
01630                                                                   
01631      MOVE PI-COMPANY-CD          TO PNDB-COMP-CD.                 
01632      MOVE PI-SAV-ENTRY-BATCH     TO PNDB-ENTRY-BATCH.             
01633      MOVE 9999                   TO PNDB-BATCH-SEQ.               
01634                                                                   
01635      EXEC CICS HANDLE CONDITION                                   
01636          NOTFND (2100-ADD-BATCH-TOTAL-REC)                        
01637      END-EXEC.                                                    
01638                                                                   
01639      EXEC CICS READ                                               
01640          SET    (ADDRESS OF PENDING-BUSINESS)                     
01641          DATASET(ERPNDB-FILE-ID)                                  
01642          RIDFLD (ERPNDB-KEY)                                      
01643          UPDATE                                                   
01644      END-EXEC.                                                    
01645                                                                   
01646 ******************************************************************
01647 *    IF THE MONTH-END-DATE IN THE TRAILER RECORD (BATCH HEADER)  *
01648 *       IS NOT EQUAL TO THE ACCOUNTS MONTH-END-DATE AND THE      *
01649 *       OPERATOR HAS NOT ENTERED A NEW MONTH-END-DATE USE THE    *
01650 *       MONTH-END-DATE IN TRAILER RECORD FOR ALL DETAIL RECORDS. *
01651 ******************************************************************
01652                                                                   
01653      IF  PI-NB-MONTH-END-DT = SPACES                              
01654          IF  PB-CREDIT-SELECT-DT = PI-CR-MONTH-END-DT             
01655              NEXT SENTENCE                                        
01656          ELSE                                                     
01657              MOVE PB-CREDIT-SELECT-DT  TO  PI-NB-MONTH-END-DT.    
01658                                                                   
01659      IF PI-AR-PROCESSING                                          
01660         IF REFL = ZEROS                                           
01661            IF PB-REFERENCE GREATER SPACES                         
01662               MOVE PB-REFERENCE  TO PI-SAV-REFERENCE              
01663            ELSE                                                   
01664               NEXT SENTENCE                                       
01665         ELSE                                                      
01666            IF PI-SAV-REFERENCE NOT = PB-REFERENCE                 
01667               MOVE 'Y'              TO WS-UPDATE-REFERENCE-SW     
01668                                        WS-RECORD-FOUND-SW         
01669               PERFORM 7100-REWRITE-REQUEST-REC THRU 7190-EXIT     
01670               IF RECORD-NOT-FOUND                                 
01671                   GO TO 8200-SEND-DATAONLY                        
01672               ELSE                                                
01673                   MOVE PI-SAV-REFERENCE TO PB-REFERENCE.          
01674                                                                   
01675      IF ELFISSL NOT = ZEROS                                       
01676          MOVE PI-LF-ISS-REMITTED  TO PB-B-LF-ISS-PRM-REMITTED.    
01677      IF EAHISSL NOT = ZEROS                                       
01678          MOVE PI-AH-ISS-REMITTED  TO PB-B-AH-ISS-PRM-REMITTED.    
01679      IF EISSCNTL NOT = ZEROS                                      
01680          MOVE PI-ISS-CNT-REMITTED TO PB-B-ISSUE-CNT-REMITTED.     
01681      IF ECANCNTL NOT = ZEROS                                      
01682          MOVE PI-CAN-CNT-REMITTED TO PB-B-CANCEL-CNT-REMITTED.    
01683      IF ELFCANL NOT = ZEROS                                       
01684          MOVE PI-LF-CAN-REMITTED  TO PB-B-LF-CAN-PRM-REMITTED.    
01685      IF EAHCANL NOT = ZEROS                                       
01686          MOVE PI-AH-CAN-REMITTED  TO PB-B-AH-CAN-PRM-REMITTED.    
01687                                                                   
01688      IF WS-PF1                                                    
01689          MOVE PI-LF-ISS-ENTERED   TO PB-B-LF-ISS-PRM-ENTERED      
01690          MOVE PI-AH-ISS-ENTERED   TO PB-B-AH-ISS-PRM-ENTERED      
01691          MOVE PI-ISS-CNT-ENTERED  TO PB-B-ISSUE-CNT-ENTERED       
01692          MOVE PI-CAN-CNT-ENTERED  TO PB-B-CANCEL-CNT-ENTERED      
01693          MOVE PI-LF-CAN-ENTERED   TO PB-B-LF-CAN-PRM-ENTERED      
01694          MOVE PI-AH-CAN-ENTERED   TO PB-B-AH-CAN-PRM-ENTERED      
01695          MOVE PI-LAST-SEQ-NO-ADDED TO PB-B-HIGHEST-SEQ-NO.        
01696                                                                   
01697      MOVE PI-RECEIVED-DT         TO PB-B-RECEIVED-DT.             
01698                                                                   
01699      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.             
01700      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         
01701      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT.             
01702                                                                   
01703      IF PI-AR-PROCESSING                                          
01704         IF PI-DATA-UPDATED                                        
01705            PERFORM 1400-VERIFY-COMP-MASTER   THRU 1490-EXIT       
01706            PERFORM 1900-WRITE-REQUEST-RECORD THRU 1990-EXIT       
01707            IF PI-MAINT-FUNC NOT = 'K'                             
01708               MOVE PI-SAV-REFERENCE TO PB-REFERENCE.              
01709                                                                   
01710      EXEC CICS REWRITE                                            
01711          DATASET(ERPNDB-FILE-ID)                                  
01712          FROM   (PENDING-BUSINESS)                                
01713      END-EXEC.                                                    
01714                                                                   
01715      IF  PI-AR-PROCESSING                                         
01716          IF WS-UPDATE-REFERENCE                                   
01717             PERFORM 5000-UPDATE-ENTIRE-BATCH THRU 5900-EXIT       
01718             GO TO 2990-EXIT.                                      
01719                                                                   
01720      IF PI-NB-MONTH-END-DT = PB-CREDIT-SELECT-DT                  
01721         GO TO 2990-EXIT.                                          
01722                                                                   
01723      IF PI-NB-MONTH-END-DT GREATER SPACES                         
01724          PERFORM 5000-UPDATE-ENTIRE-BATCH THRU 5900-EXIT          
01725              IF PI-AR-PROCESSING                                  
01726                  MOVE 'Y'          TO WS-RECORD-FOUND-SW          
01727                  PERFORM 7200-REWRITE-REQUEST THRU 7299-EXIT      
01728                  IF RECORD-NOT-FOUND                              
01729                      GO TO 8200-SEND-DATAONLY.                    
01730                                                                   
01731      GO TO 2990-EXIT.                                             
01732                                                                   
01733      EJECT                                                        
01734                                                                   
01735  2100-ADD-BATCH-TOTAL-REC.                                        
01736      EXEC CICS GETMAIN                                            
01737          SET    (ADDRESS OF PENDING-BUSINESS)                     
01738          LENGTH (585)                                             
01739          INITIMG(GETMAIN-SPACE)                                   
01740      END-EXEC.                                                    
01741                                                                   
01742      MOVE 'PB'                   TO PB-RECORD-ID.                 
01743      MOVE PI-COMPANY-CD          TO PB-COMPANY-CD                 
01744                                     PB-COMPANY-CD-A1.             
01745      MOVE PI-COMPANY-ID          TO PB-COMPANY-ID.                
01746      MOVE PI-SAV-ENTRY-BATCH     TO PB-ENTRY-BATCH                
01747                                     PB-CERT-NO.                   
01748      MOVE 9999                   TO PB-BATCH-SEQ-NO.              
01749      MOVE HIGH-VALUES            TO PB-CERT-EFF-DT.               
01750      MOVE ZEROS                  TO PB-BATCH-CHG-SEQ-NO           
01751                                     PB-ALT-CHG-SEQ-NO.            
01752                                                                   
01753      IF REFL GREATER THAN +0                                      
01754         MOVE REFI                TO PB-REFERENCE                  
01755                                     PI-SAV-REFERENCE.             
01756                                                                   
01757      IF PI-LF-ISS-REMITTED     NOT NUMERIC                        
01758         MOVE +0                  TO PI-LF-ISS-REMITTED.           
01759                                                                   
01760      IF PI-LF-ISS-ENTERED      NOT NUMERIC                        
01761         MOVE +0                  TO PI-LF-ISS-ENTERED.            
01762                                                                   
01763      IF PI-AH-ISS-REMITTED     NOT NUMERIC                        
01764         MOVE +0                  TO PI-AH-ISS-REMITTED.           
01765                                                                   
01766      IF PI-AH-ISS-ENTERED      NOT NUMERIC                        
01767         MOVE +0                  TO PI-AH-ISS-ENTERED.            
01768                                                                   
01769      IF PI-ISS-CNT-REMITTED    NOT NUMERIC                        
01770         MOVE +0                  TO PI-ISS-CNT-REMITTED.          
01771                                                                   
01772      IF PI-ISS-CNT-ENTERED     NOT NUMERIC                        
01773         MOVE +0                  TO PI-ISS-CNT-ENTERED.           
01774                                                                   
01775      IF PI-CAN-CNT-REMITTED    NOT NUMERIC                        
01776         MOVE +0                  TO PI-CAN-CNT-REMITTED.          
01777                                                                   
01778      IF PI-CAN-CNT-ENTERED     NOT NUMERIC                        
01779         MOVE +0                  TO PI-CAN-CNT-ENTERED.           
01780                                                                   
01781      IF PI-LF-CAN-REMITTED     NOT NUMERIC                        
01782         MOVE +0                  TO PI-LF-CAN-REMITTED.           
01783                                                                   
01784      IF PI-LF-CAN-ENTERED      NOT NUMERIC                        
01785         MOVE +0                  TO PI-LF-CAN-ENTERED.            
01786                                                                   
01787      IF PI-LF-CAN-REMITTED     NOT NUMERIC                        
01788         MOVE +0                  TO PI-LF-CAN-REMITTED.           
01789                                                                   
01790      IF PI-AH-CAN-REMITTED     NOT NUMERIC                        
01791         MOVE +0                  TO PI-AH-CAN-REMITTED.           
01792                                                                   
01793      IF PI-AH-CAN-ENTERED      NOT NUMERIC                        
01794         MOVE +0                  TO PI-AH-CAN-ENTERED.            
01795                                                                   
01796                                                                   
01797      MOVE PI-SAV-CARRIER         TO PB-CARRIER.                   
01798      MOVE PI-SAV-GROUPING        TO PB-GROUPING.                  
01799      MOVE PI-SAV-STATE           TO PB-STATE.                     
01800      MOVE PI-SAV-ACCOUNT         TO PB-ACCOUNT.                   
01801      MOVE '9'                    TO PB-RECORD-TYPE.               
01802      MOVE PI-LF-ISS-REMITTED     TO PB-B-LF-ISS-PRM-REMITTED.     
01803      MOVE PI-LF-ISS-ENTERED      TO PB-B-LF-ISS-PRM-ENTERED.      
01804      MOVE PI-AH-ISS-REMITTED     TO PB-B-AH-ISS-PRM-REMITTED.     
01805      MOVE PI-AH-ISS-ENTERED      TO PB-B-AH-ISS-PRM-ENTERED.      
01806      MOVE PI-ISS-CNT-REMITTED    TO PB-B-ISSUE-CNT-REMITTED.      
01807      MOVE PI-ISS-CNT-ENTERED     TO PB-B-ISSUE-CNT-ENTERED.       
01808      MOVE PI-CAN-CNT-REMITTED    TO PB-B-CANCEL-CNT-REMITTED.     
01809      MOVE PI-CAN-CNT-ENTERED     TO PB-B-CANCEL-CNT-ENTERED.      
01810      MOVE PI-LF-CAN-REMITTED     TO PB-B-LF-CAN-PRM-REMITTED.     
01811      MOVE PI-LF-CAN-ENTERED      TO PB-B-LF-CAN-PRM-ENTERED.      
01812      MOVE PI-AH-CAN-REMITTED     TO PB-B-AH-CAN-PRM-REMITTED.     
01813      MOVE PI-AH-CAN-ENTERED      TO PB-B-AH-CAN-PRM-ENTERED.      
01814      MOVE ZEROS                  TO PB-B-LF-ISS-PRM-COMPUTED      
01815                                     PB-B-LF-CAN-PRM-COMPUTED      
01816                                     PB-B-AH-ISS-PRM-COMPUTED      
01817                                     PB-B-AH-CAN-PRM-COMPUTED      
01818                                     PB-LF-BILLED-AMTS             
01819                                     PB-AH-BILLED-AMTS             
01820                                     PB-CHG-COUNT                  
01821                                     PB-CALC-TOLERANCE.            
01822      MOVE LOW-VALUES             TO PB-CREDIT-ACCEPT-DT           
01823                                     PB-BILLED-DT                  
01824                                     PB-ACCT-EFF-DT                
01825                                     PB-ACCT-EXP-DT.               
01826                                                                   
01827      IF  PI-NB-MONTH-END-DT GREATER SPACES                        
01828          MOVE PI-NB-MONTH-END-DT TO PB-CREDIT-SELECT-DT           
01829         ELSE                                                      
01830          MOVE PI-CR-MONTH-END-DT TO PB-CREDIT-SELECT-DT.          
01831                                                                   
01832      MOVE PI-LAST-SEQ-NO-ADDED   TO PB-B-HIGHEST-SEQ-NO.          
01833                                                                   
01834      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY              
01835                                     PB-INPUT-BY.                  
01836      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         
01837      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT              
01838                                     PB-INPUT-DT.                  
01839                                                                   
01840      MOVE PB-CONTROL-PRIMARY     TO PB-CONTROL-BY-ORIG-BATCH.     
01841                                                                   
01842      MOVE PB-COMPANY-CD          TO PB-CSR-COMPANY-CD.            
01843      MOVE PB-ENTRY-BATCH         TO PB-CSR-ENTRY-BATCH.           
01844      MOVE PI-CSR-ID              TO PB-CSR-ID.                    
01845      MOVE PB-BATCH-SEQ-NO        TO PB-CSR-BATCH-SEQ-NO.          
01846      MOVE PB-BATCH-CHG-SEQ-NO    TO PB-CSR-BATCH-CHG-SEQ-NO.      
01847                                                                   
01848      MOVE PI-RECEIVED-DT         TO PB-B-RECEIVED-DT.             
01849                                                                   
01850      MOVE PI-AM-NAME             TO  PB-ACCOUNT-NAME.             
01851      MOVE PI-SAV-FC-CARRIER      TO  PB-SV-CARRIER.               
01852      MOVE PI-SAV-FC-GROUPING     TO  PB-SV-GROUPING.              
01853      MOVE PI-SAV-FC-STATE        TO  PB-SV-STATE.                 
01854                                                                   
01855      MOVE PB-CONTROL-PRIMARY     TO  ERPNDB-KEY.                  
01856                                                                   
01857      EXEC CICS WRITE                                              
01858          DATASET(ERPNDB-FILE-ID)                                  
01859          FROM   (PENDING-BUSINESS)                                
01860          RIDFLD (PB-CONTROL-PRIMARY)                              
01861      END-EXEC.                                                    
01862                                                                   
01863  2990-EXIT.                                                       
01864      EXIT.                                                        
01865                                                                   
01866      EJECT                                                        
01867  3000-DELETE-ENTERED-BATCH.                                       
01868      MOVE SPACE          TO PI-NB-MONTH-END-DT                    
01869                             PI-SAV-REFERENCE.                     
01870                                                                   
01871      IF NOT PI-DELETE-IS-OK                                       
01872          MOVE 'Y'                TO PI-VERIFY-DELETE-SW           
01873          GO TO 3300-FIRST-PF2.                                    
01874                                                                   
01875      MOVE SPACE                  TO PI-ACCT-AGENT-PROCESSED-SW    
01876                                     PI-VERIFY-DELETE-SW.          
01877      MOVE PI-COMPANY-CD          TO PNDB-COMP-CD.                 
01878      MOVE PI-SAV-ENTRY-BATCH     TO PNDB-ENTRY-BATCH.             
01879      MOVE 1                      TO PNDB-BATCH-SEQ.               
01880                                                                   
01881      EXEC CICS SYNCPOINT                                          
01882      END-EXEC.                                                    
01883                                                                   
01884      EXEC CICS HANDLE CONDITION                                   
01885          NOTFND(3200-NO-RECORDS)                                  
01886      END-EXEC.                                                    
01887                                                                   
01888      EXEC CICS STARTBR                                            
01889          DATASET(ERPNDB-FILE-ID)                                  
01890          RIDFLD(ERPNDB-KEY)                                       
01891      END-EXEC.                                                    
01892                                                                   
01893  3000-GET-NEXT-RECORD.                                            
01894      EXEC CICS HANDLE CONDITION                                   
01895          NOTFND(3120-END-ROUTINE)                                 
01896      END-EXEC.                                                    
01897                                                                   
01898      ADD +1                      TO  WS-SYNC-CNTR.                
01899                                                                   
01900      IF WS-SYNC-CNTR GREATER +100                                 
01901          MOVE +0                 TO  WS-SYNC-CNTR                 
01902          EXEC  CICS SYNCPOINT                                     
01903          END-EXEC.                                                
01904                                                                   
01905      IF WS-ON1-SW3 IS EQUAL TO 'Y'                                
01906          MOVE 'N'                TO  WS-ON1-SW3                   
01907          GO TO 3005-READ-NEXT-RECORD.                             
01908                                                                   
01909      EXEC CICS STARTBR                                            
01910          DATASET(ERPNDB-FILE-ID)                                  
01911          RIDFLD (ERPNDB-KEY)                                      
01912      END-EXEC.                                                    
01913                                                                   
01914      EXEC CICS HANDLE CONDITION                                   
01915          NOTFND (3100-STOP-BROWSE)                                
01916          ENDFILE(3100-STOP-BROWSE)                                
01917      END-EXEC.                                                    
01918                                                                   
01919  3005-READ-NEXT-RECORD.                                           
01920      EXEC CICS READNEXT                                           
01921          SET    (ADDRESS OF PENDING-BUSINESS)                     
01922          DATASET(ERPNDB-FILE-ID)                                  
01923          RIDFLD (ERPNDB-KEY)                                      
01924      END-EXEC.                                                    
01925                                                                   
01926      IF PB-COMPANY-CD  = PI-SAV-COMP-CD AND                       
01927         PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH                       
01928          NEXT SENTENCE                                            
01929      ELSE                                                         
01930          GO TO 3100-STOP-BROWSE.                                  
01931                                                                   
01932      IF PB-BILLED-DT NOT = LOW-VALUES                             
01933          EXEC CICS SYNCPOINT ROLLBACK                             
01934          END-EXEC                                                 
01935          MOVE ER-2402            TO EMI-ERROR                     
01936          MOVE -1                 TO BATCHL                        
01937          MOVE AL-UABON           TO BATCHA                        
01938          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
01939          GO TO 8200-SEND-DATAONLY.                                
01940                                                                   
01941 ******************************************************************
01942 *                DELETE PENDING ADDRESS RECORD                   *
01943 ******************************************************************
01944                                                                   
01945      IF PB-I-MAIL-ADDRS-PRESENT                                   
01946         NEXT SENTENCE                                             
01947        ELSE                                                       
01948         GO TO 3006-DELETE-CERTIFICATE.                            
01949                                                                   
01950      EXEC CICS HANDLE CONDITION                                   
01951           NOTOPEN  (3006-DELETE-CERTIFICATE)                      
01952           NOTFND   (3006-DELETE-CERTIFICATE)                      
01953      END-EXEC.                                                    
01954                                                                   
01955      EXEC CICS READ                                               
01956          EQUAL                                                    
01957          DATASET   (ERPNDM-FILE-ID)                               
01958          SET       (ADDRESS OF PENDING-MAILING-DATA)              
01959          RIDFLD    (ERPNDB-KEY)                                   
01960          UPDATE                                                   
01961      END-EXEC.                                                    
01962                                                                   
01963      EXEC CICS DELETE                                             
01964          DATASET   (ERPNDM-FILE-ID)                               
01965      END-EXEC.                                                    
01966                                                                   
01967  3006-DELETE-CERTIFICATE.                                         
01968      IF PB-BATCH-TRAILER                                          
01969         GO TO 3020-CONTINUE-DELETE.                               
01970                                                                   
01971      EXEC CICS HANDLE CONDITION                                   
01972           NOTFND  (3020-CONTINUE-DELETE)                          
01973      END-EXEC.                                                    
01974                                                                   
01975      MOVE PB-CONTROL-BY-ACCOUNT  TO ELCERT-KEY.                   
01976      MOVE PB-SV-CARRIER          TO CERT-CARRIER.                 
01977      MOVE PB-SV-GROUPING         TO CERT-GROUPING.                
01978      MOVE PB-SV-STATE            TO CERT-STATE.                   
01979      EXEC CICS READ                                               
01980          SET     (ADDRESS OF CERTIFICATE-MASTER)                  
01981          DATASET (ELCERT-FILE-ID)                                 
01982          RIDFLD  (ELCERT-KEY)                                     
01983          UPDATE                                                   
01984      END-EXEC.                                                    
01985                                                                   
01986      IF INSURED-ADDR-PRESENT                                      
01987         MOVE 'Y'                  TO WS-CERT-ADDRESS-SW           
01988      ELSE                                                         
01989         MOVE ' '                  TO WS-CERT-ADDRESS-SW.          
01990                                                                   
01991      IF  CERT-NOTES-ARE-NOT-PRESENT                               
01992          MOVE ' '                TO WS-CERT-NOTE-SW               
01993      ELSE                                                         
01994          MOVE 'Y'                TO WS-CERT-NOTE-SW.              
01995                                                                   
01996      IF PB-CANCELLATION                                           
01997         IF PB-CI-LF-PRIOR-CANCEL-DT = LOW-VALUES                  
01998            MOVE SPACE                 TO CM-CREDIT-INTERFACE-SW-2 
01999            MOVE ZERO                  TO CM-LF-ITD-CANCEL-AMT     
02000            MOVE LOW-VALUES            TO CM-LF-CANCEL-EXIT-DT     
02001                                          CM-LF-CANCEL-DT          
02002            MOVE SPACE                 TO CM-LF-EXIT-BATCH         
02003         ELSE                                                      
02004            MOVE SPACE                 TO CM-CREDIT-INTERFACE-SW-2 
02005            MOVE PB-CI-LF-CANCEL-AMT   TO CM-LF-ITD-CANCEL-AMT     
02006            MOVE PB-CI-LF-PRIOR-CANCEL-DT TO CM-LF-CANCEL-DT       
02007            MOVE PB-CI-LIVES              TO CM-LIVES.             
02008                                                                   
02009      IF PB-CANCELLATION                                           
02010         IF PB-CI-LF-PRIOR-CANCEL-DT = LOW-VALUES                  
02011            IF CM-LF-STATUS-AT-CANCEL NOT EQUAL SPACE              
02012                MOVE CM-LF-STATUS-AT-CANCEL                        
02013                                         TO CM-LF-CURRENT-STATUS   
02014                MOVE SPACE                                         
02015                                         TO CM-LF-STATUS-AT-CANCEL 
02016             ELSE                                                  
02017                MOVE PB-CI-LF-POLICY-STATUS                        
02018                                         TO CM-LF-CURRENT-STATUS   
02019         ELSE                                                      
02020             MOVE PB-CI-LF-POLICY-STATUS TO CM-LF-CURRENT-STATUS.  
02021                                                                   
02022      IF PB-CANCELLATION                                           
02023         IF PB-CI-AH-PRIOR-CANCEL-DT = LOW-VALUES                  
02024            MOVE SPACE                 TO CM-CREDIT-INTERFACE-SW-2 
02025            MOVE ZERO                  TO CM-AH-ITD-CANCEL-AMT     
02026            MOVE LOW-VALUES            TO CM-AH-CANCEL-EXIT-DT     
02027                                          CM-AH-CANCEL-DT          
02028            MOVE SPACE                 TO CM-AH-EXIT-BATCH         
02029         ELSE                                                      
02030            MOVE SPACE                 TO CM-CREDIT-INTERFACE-SW-2 
02031            MOVE PB-CI-AH-CANCEL-AMT   TO CM-AH-ITD-CANCEL-AMT     
02032            MOVE PB-CI-AH-PRIOR-CANCEL-DT TO CM-AH-CANCEL-DT       
02033            MOVE PB-CI-LIVES           TO CM-LIVES.                
02034                                                                   
02035      IF PB-CANCELLATION                                           
02036         IF PB-CI-AH-PRIOR-CANCEL-DT = LOW-VALUES                  
02037            IF CM-AH-STATUS-AT-CANCEL NOT EQUAL SPACE              
02038                MOVE CM-AH-STATUS-AT-CANCEL                        
02039                                         TO CM-AH-CURRENT-STATUS   
02040                MOVE SPACE                                         
02041                                         TO CM-AH-STATUS-AT-CANCEL 
02042                GO TO 3010-REWRITE-CERT                            
02043             ELSE                                                  
02044                MOVE PB-CI-AH-POLICY-STATUS                        
02045                                         TO CM-AH-CURRENT-STATUS   
02046                GO TO 3010-REWRITE-CERT                            
02047         ELSE                                                      
02048             MOVE PB-CI-AH-POLICY-STATUS TO CM-AH-CURRENT-STATUS   
02049             GO TO 3010-REWRITE-CERT.                              
02050                                                                   
02051 ******************************************************************
02052 ******************************************************************
02053 ******* THIS CODE SHOULD BYPASS SETTING THE CLAIM-INTERFACE-SW  **
02054 ******* IF THE CERTIFICATE HAS ALREADY GONE THROUGH A MONTH END **
02055 ******************************************************************
02056 ******************************************************************
02057                                                                   
02058      IF CERT-ADDED-BATCH AND                                      
02059         (NOT NO-CLAIM-ATTACHED                                    
02060          OR                                                       
02061          CM-CLAIM-ATTACHED-COUNT GREATER THAN +0)                 
02062         GO TO 3010-REWRITE-CERT.                                  
02063                                                                   
02064 ******************************************************************
02065 ******************************************************************
02066                                                                   
02067      IF CERT-ADDED-BATCH  AND                                     
02068         (NO-CLAIM-ATTACHED OR CM-CLAIM-ATTACHED-COUNT = ZEROS)    
02069         GO TO 3010-REWRITE-CERT.                                  
02070                                                                   
02071      IF NO-CLAIM-ATTACHED                                         
02072          NEXT SENTENCE                                            
02073        ELSE                                                       
02074          MOVE SPACE                  TO CM-CREDIT-INTERFACE-SW-1  
02075          MOVE '2'                    TO CM-CLAIM-INTERFACE-SW     
02076          GO TO 3010-REWRITE-CERT.                                 
02077                                                                   
02078         EXEC CICS DELETE                                          
02079              DATASET    (ELCERT-FILE-ID)                          
02080         END-EXEC.                                                 
02081                                                                   
02082 ******************************************************************
02083 *       DELETE CERTIFICATE NOTES FOR ALL DELETED CERTIFICATES    *
02084 ******************************************************************
02085                                                                   
02086      IF CERT-NOTES-ARE-PRESENT                                    
02087         NEXT SENTENCE                                             
02088      ELSE                                                         
02089         GO TO 3007-DELETE-MAILING-DATA.                           
02090                                                                   
111109     EXEC CICS HANDLE CONDITION                                   
111109          NOTFND  (3006-DELETE-ERNOTE)                           
111109     END-EXEC.                                                    
111109                         
111109     MOVE ELCERT-KEY            TO ERCNOT-KEY.
111109     MOVE '0'                   TO ERCNOT-REC-TYPE.
111109     MOVE 0                     TO ERCNOT-SEQUENCE.
111109  
111109 3006-DELETE-ERCNOT-LOOP.
111109
111109     EXEC CICS READ                                               
111109         GTEQ                                                    
111109         DATASET   (ERCNOT-FILE-ID)                               
111109         SET       (ADDRESS OF CERT-NOTE-FILE)                  
111109         RIDFLD    (ERCNOT-KEY)                                   
111109     END-EXEC.                                                    
111109                             
111109     MOVE CZ-CONTROL-PRIMARY      TO ERCNOT-KEY
111109     IF ERCNOT-PART-KEY NOT EQUAL ELCERT-KEY
111109         GO TO 3006-DELETE-ERNOTE
111109     END-IF.
111109                                                                  
111109     EXEC CICS DELETE                                             
111109         DATASET   (ERCNOT-FILE-ID) 
111109         RIDFLD    (ERCNOT-KEY)                              
111109         END-EXEC.                                                
111109                                                                  
111109     GO TO 3006-DELETE-ERCNOT-LOOP.
111109
111109 3006-DELETE-ERNOTE.
111109
041320     move elcert-key             to ernote-key
041320     move '1'                    to ernote-rec-type
041320
041320     exec cics delete
041320        dataset    (ernote-file-id)
041320        keylength  (ernote-generic-key-len)
041320        ridfld     (ernote-key(1:33))
041320        generic
041320        resp       (w-response)
041320     end-exec
           .
02107 ******************************************************************
02108 *     DELETE MAILING ADDRESS RECORDS FOR DELETED CERTIFICATES    *
02109 ******************************************************************
02110                                                                   
02111  3007-DELETE-MAILING-DATA.                                        
02112      IF CERT-ADDRESS-PRESENT                                      
02113         NEXT SENTENCE                                             
02114      ELSE                                                         
02115         GO TO 3020-CONTINUE-DELETE.                               
02116                                                                   
02117      EXEC CICS HANDLE CONDITION                                   
02118           NOTOPEN  (3020-CONTINUE-DELETE)                         
02119           NOTFND   (3020-CONTINUE-DELETE)                         
02120      END-EXEC.                                                    
02121                                                                   
02122      EXEC CICS READ                                               
02123          EQUAL                                                    
02124          DATASET   (ERMAIL-FILE-ID)                               
02125          SET       (ADDRESS OF MAILING-DATA)                      
02126          RIDFLD    (ELCERT-KEY)                                   
02127          UPDATE                                                   
02128      END-EXEC.                                                    
02129                                                                   
02130      EXEC CICS DELETE                                             
02131          DATASET   (ERMAIL-FILE-ID)                               
02132      END-EXEC.                                                    
02133                                                                   
02134      GO TO 3020-CONTINUE-DELETE.                                  
02135                                                                   
02136  3010-REWRITE-CERT.                                               
02137                                                                   
02138      EXEC CICS REWRITE                                            
02139          DATASET    (ELCERT-FILE-ID)                              
02140          FROM       (CERTIFICATE-MASTER)                          
02141      END-EXEC.                                                    
02142                                                                   
02143  3020-CONTINUE-DELETE.                                            
02144                                                                   
02145      EXEC CICS ENDBR                                              
02146          DATASET(ERPNDB-FILE-ID)                                  
02147      END-EXEC.                                                    
02148                                                                   
02149      EXEC CICS DELETE                                             
02150          DATASET(ERPNDB-FILE-ID)                                  
02151          RIDFLD (ERPNDB-KEY)                                      
02152      END-EXEC.                                                    
02153                                                                   
02154      ADD 1   TO WS-DELETE-CNT.                                    
02155      GO TO 3000-GET-NEXT-RECORD.                                  
02156                                                                   
02157  3100-STOP-BROWSE.                                                
02158      EXEC CICS ENDBR                                              
02159          DATASET(ERPNDB-FILE-ID)                                  
02160      END-EXEC.                                                    
02161                                                                   
02162  3120-END-ROUTINE.                                                
02163      MOVE SPACE                  TO PI-UPDATE-SW.                 
02164                                                                   
02165      IF WS-DELETE-CNT NOT GREATER ZERO                            
02166          GO TO 3200-NO-RECORDS.                                   
02167                                                                   
02168      IF PI-AR-PROCESSING                                          
02169         MOVE 'Y'                 TO WS-RECORD-FOUND-SW            
02170         PERFORM 7000-VERIFY-REQUEST-REC THRU 7090-EXIT            
02171         IF RECORD-NOT-FOUND                                       
02172            GO TO 8200-SEND-DATAONLY.                              
02173                                                                   
020816     IF PI-SAVE-CALLING-PGM NOT = XCTL-6301 and 'VP6301'
02175          MOVE ZEROS              TO EMI-ERROR                     
02176          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                
02177                                                                   
02178      MOVE SPACE                  TO PI-BROWSE-SW.                 
02179      MOVE LOW-VALUES             TO EL630AO.                      
02180      MOVE QUESTION-MARKS         TO BATCHI.                       
02181      MOVE AL-UANON               TO BATCHA.                       
02182      MOVE -1                     TO MAINTL.                       
02183      GO TO 8100-SEND-INITIAL-MAP.                                 
02184                                                                   
02185  3200-NO-RECORDS.                                                 
02186      MOVE ER-2242                TO EMI-ERROR.                    
02187      MOVE -1 TO BATCHL                                            
02188      MOVE AL-UABON               TO BATCHA.                       
02189      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02190      GO TO 8200-SEND-DATAONLY.                                    
02191                                                                   
02192  3300-FIRST-PF2.                                                  
02193      MOVE SPACE                  TO PI-CLEAR-ERROR-SW.            
02194      MOVE ER-2422                TO EMI-ERROR.                    
02195      MOVE -1                     TO PFENTERL.                     
02196      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02197      GO TO 8100-SEND-INITIAL-MAP.                                 
02198      EJECT                                                        
02199                                                                   
02200  4000-SHOW-TOTALS.                                                
02201                                                                   
02202      MOVE LOW-VALUES             TO EL630AI.                      
02203      MOVE PI-SAV-ENTRY-BATCH     TO BATCHO.                       
02204      MOVE PI-SAV-REFERENCE       TO REFO.                         
02205                                                                   
02206      IF PI-DATA-UPDATED                                           
02207         MOVE 'C'                TO PI-MAINT-FUNC.                 
02208                                                                   
02209      MOVE PI-MAINT-FUNC          TO MAINTO.                       
02210                                                                   
02211      IF PI-SAV-CARRIER NOT = SPACES                               
02212          MOVE PI-SAV-CARRIER     TO CARRIERO.                     
02213                                                                   
02214      IF PI-SAV-GROUPING NOT = SPACES                              
02215          MOVE PI-SAV-GROUPING    TO GROUPO.                       
02216                                                                   
02217      IF PI-SAV-STATE NOT = SPACES                                 
02218          MOVE PI-SAV-STATE       TO STATEO.                       
02219                                                                   
02220      MOVE PI-SAV-ACCOUNT         TO ACCOUNTO.                     
02221                                                                   
02222      IF PI-DATA-UPDATED                                           
02223          MOVE AL-SANON           TO BATCHA                        
02224                                     MAINTA                        
02225                                     CARRIERA                      
02226                                     GROUPA                        
02227                                     STATEA                        
02228                                     ACCOUNTA                      
02229      ELSE                                                         
02230          MOVE AL-UANON           TO BATCHA                        
02231                                     MAINTA                        
02232                                     CARRIERA                      
02233                                     GROUPA                        
02234                                     STATEA                        
02235                                     ACCOUNTA.                     
02236                                                                   
02237      MOVE PI-LF-ISS-ENTERED      TO ALFISSO.                      
02238      IF PI-LF-ISS-REMITTED NOT = ZEROS                            
02239          MOVE PI-LF-ISS-REMITTED TO ELFISSO                       
02240          MOVE AL-UNNON           TO ELFISSA                       
02241          COMPUTE WS-OBAL = PI-LF-ISS-REMITTED - PI-LF-ISS-ENTERED 
02242          IF WS-OBAL NOT = ZEROS                                   
02243              MOVE WS-OBAL        TO OLFISSO.                      
02244                                                                   
02245      MOVE PI-LF-CAN-ENTERED      TO ALFCANO.                      
02246      IF PI-LF-CAN-REMITTED NOT = ZEROS                            
02247          MOVE PI-LF-CAN-REMITTED TO ELFCANO                       
02248          MOVE AL-UNNON           TO ELFCANA                       
02249          COMPUTE WS-OBAL = PI-LF-CAN-REMITTED - PI-LF-CAN-ENTERED 
02250          IF WS-OBAL NOT = ZEROS                                   
02251              MOVE WS-OBAL        TO OLFCANO.                      
02252                                                                   
02253      MOVE PI-AH-ISS-ENTERED      TO AAHISSO.                      
02254      IF PI-AH-ISS-REMITTED NOT = ZEROS                            
02255          MOVE PI-AH-ISS-REMITTED TO EAHISSO                       
02256          MOVE AL-UNNON           TO EAHISSA                       
02257          COMPUTE WS-OBAL = PI-AH-ISS-REMITTED - PI-AH-ISS-ENTERED 
02258          IF WS-OBAL NOT = ZEROS                                   
02259              MOVE WS-OBAL        TO OAHISSO.                      
02260                                                                   
02261      MOVE PI-AH-CAN-ENTERED      TO AAHCANO.                      
02262      IF PI-AH-CAN-REMITTED NOT = ZEROS                            
02263          MOVE PI-AH-CAN-REMITTED TO EAHCANO                       
02264          MOVE AL-UNNON           TO EAHCANA                       
02265          COMPUTE WS-OBAL = PI-AH-CAN-REMITTED - PI-AH-CAN-ENTERED 
02266          IF WS-OBAL NOT = ZEROS                                   
02267              MOVE WS-OBAL        TO OAHCANO.                      
02268                                                                   
02269      MOVE PI-ISS-CNT-ENTERED     TO AISSCNTO.                     
02270      IF PI-ISS-CNT-REMITTED NOT = ZEROS                           
02271          MOVE PI-ISS-CNT-REMITTED TO EISSCNTO                     
02272          MOVE AL-UNNON            TO EISSCNTA                     
02273          COMPUTE WS-OCNT = PI-ISS-CNT-REMITTED -                  
02274                                 PI-ISS-CNT-ENTERED                
02275          IF WS-OCNT NOT = ZEROS                                   
02276              MOVE WS-OCNT        TO OISSCNTO.                     
02277                                                                   
02278      MOVE PI-CAN-CNT-ENTERED     TO ACANCNTO.                     
02279      IF PI-CAN-CNT-REMITTED NOT = ZEROS                           
02280          MOVE PI-CAN-CNT-REMITTED TO ECANCNTO                     
02281          MOVE AL-UNNON            TO ECANCNTA                     
02282          COMPUTE WS-OCNT = PI-CAN-CNT-REMITTED -                  
02283                                 PI-CAN-CNT-ENTERED                
02284          IF WS-OCNT NOT = ZEROS                                   
02285              MOVE WS-OCNT        TO OCANCNTO.                     
02286                                                                   
02287      IF  PI-NB-MONTH-END-DT GREATER SPACES                        
02288          MOVE PI-NB-MONTH-END-DT TO DC-BIN-DATE-1                 
02289        ELSE                                                       
02290          MOVE PI-CR-MONTH-END-DT TO DC-BIN-DATE-1.                
02291                                                                   
02292      MOVE  SPACE                 TO  DC-OPTION-CODE.              
02293      PERFORM  9700-DATE-LINK.                                     
02294                                                                   
02295      IF  DATE-CONVERSION-ERROR                                    
02296          MOVE LOW-VALUES TO MNTHNDTO                              
02297          GO TO 0490-EXIT.                                         
02298                                                                   
02299      MOVE DC-GREG-DATE-1-EDIT    TO  MNTHNDTO.                    
02300      MOVE AL-UANON               TO  MNTHNDTA.                    
02301                                                                   
02302      IF  PI-NB-MONTH-END-DT GREATER SPACES                        
02303          MOVE PI-NB-MONTH-END-DT TO DC-BIN-DATE-1                 
02304        ELSE                                                       
02305          MOVE PI-CR-MONTH-END-DT TO DC-BIN-DATE-1.                
02306                                                                   
02307      MOVE  SPACE                 TO  DC-OPTION-CODE.              
02308      PERFORM  9700-DATE-LINK.                                     
02309                                                                   
02310      IF  DATE-CONVERSION-ERROR                                    
02311          MOVE LOW-VALUES TO MNTHNDTO                              
02312      ELSE                                                         
02313          MOVE DC-GREG-DATE-1-EDIT    TO  MNTHNDTO                 
02314          MOVE AL-UANON               TO  MNTHNDTA.                
02315                                                                   
02316      MOVE PI-RECEIVED-DT         TO DC-BIN-DATE-1.                
02317                                                                   
02318      MOVE  SPACE                 TO  DC-OPTION-CODE.              
02319      PERFORM  9700-DATE-LINK.                                     
02320                                                                   
02321      IF  DATE-CONVERSION-ERROR                                    
02322          MOVE LOW-VALUES TO RECEVDTO                              
02323      ELSE                                                         
02324          MOVE DC-GREG-DATE-1-EDIT    TO  RECEVDTO                 
02325          MOVE AL-UANON               TO  RECEVDTA.                
02326                                                                   
02327      GO TO 8100-SEND-INITIAL-MAP.                                 
02328      EJECT                                                        
02329                                                                   
02330 ***************************************************************** 
02331 *            U P D A T E   E N T I R E   B A T C H              * 
02332 *  THIS SECTION UPDATES AN ENTIRE BATCH WITH A NEW MONTH END    * 
02333 *  DATE.                                                        * 
02334 ***************************************************************** 
02335                                                                   
02336  5000-UPDATE-ENTIRE-BATCH.                                        
02337                                                                   
02338      MOVE ZEROS                  TO  PNDB-BATCH-SEQ               
02339                                      PNDB-BATCH-CHG-SEQ.          
02340                                                                   
02341      EXEC CICS HANDLE CONDITION                                   
02342          NOTFND(5900-EXIT)                                        
02343      END-EXEC.                                                    
02344                                                                   
02345      EXEC CICS STARTBR                                            
02346          DATASET(ERPNDB-FILE-ID)                                  
02347          RIDFLD (ERPNDB-KEY)                                      
02348      END-EXEC.                                                    
02349                                                                   
02350  5010-GET-NEXT-RECORD.                                            
02351      EXEC CICS HANDLE CONDITION                                   
02352          NOTFND (5100-STOP-BROWSE)                                
02353          ENDFILE(5100-STOP-BROWSE)                                
02354      END-EXEC.                                                    
02355                                                                   
02356  5020-READ-NEXT-RECORD.                                           
02357      EXEC CICS READNEXT                                           
02358          SET    (ADDRESS OF PENDING-BUSINESS)                     
02359          DATASET(ERPNDB-FILE-ID)                                  
02360          RIDFLD (ERPNDB-KEY)                                      
02361      END-EXEC.                                                    
02362                                                                   
02363      IF WS-ON1-SW4 IS EQUAL TO 'Y'                                
02364          MOVE 'N'                    TO  WS-ON1-SW4               
02365          MOVE PB-CONTROL-PRIMARY     TO  WS-SAV-PNDB-KEY          
02366          GO TO 5030-READ-FOR-UPDATE.                              
02367                                                                   
02368      IF ERPNDB-KEY = WS-SAV-PNDB-KEY                              
02369         GO TO 5020-READ-NEXT-RECORD.                              
02370                                                                   
02371      IF PB-COMPANY-CD  = WS-SAV-COMP-CD  AND                      
02372         PB-ENTRY-BATCH = WS-SAV-ENTRY-BATCH                       
02373          NEXT SENTENCE                                            
02374      ELSE                                                         
02375          GO TO 5100-STOP-BROWSE.                                  
02376                                                                   
02377      IF PB-ISSUE                                                  
02378         IF PB-I-REFERENCE NOT = PI-SAV-REFERENCE                  
02379            GO TO 5030-READ-FOR-UPDATE.                            
02380                                                                   
02381      IF PB-CANCELLATION                                           
02382         IF PB-C-REFERENCE NOT = PI-SAV-REFERENCE                  
02383            GO TO 5030-READ-FOR-UPDATE.                            
02384                                                                   
02385      IF PB-CREDIT-SELECT-DT = PI-NB-MONTH-END-DT                  
02386          GO TO 5010-GET-NEXT-RECORD.                              
02387                                                                   
02388  5030-READ-FOR-UPDATE.                                            
02389      EXEC CICS ENDBR                                              
02390          DATASET(ERPNDB-FILE-ID)                                  
02391      END-EXEC.                                                    
02392                                                                   
02393      EXEC CICS READ                                               
02394          SET     (ADDRESS OF PENDING-BUSINESS)                    
02395          DATASET (ERPNDB-FILE-ID)                                 
02396          RIDFLD  (ERPNDB-KEY)                                     
02397          UPDATE                                                   
02398      END-EXEC.                                                    
02399                                                                   
02400      MOVE ERPNDB-KEY                  TO WS-SAV-PNDB-KEY.         
02401                                                                   
02402      IF PI-NB-MONTH-END-DT GREATER THAN SPACES                    
02403         MOVE  PI-NB-MONTH-END-DT TO PB-CREDIT-SELECT-DT.          
02404                                                                   
02405      IF PB-ISSUE                                                  
02406         IF PB-I-REFERENCE NOT = PI-SAV-REFERENCE                  
02407            MOVE PI-SAV-REFERENCE TO PB-I-REFERENCE.               
02408                                                                   
02409      IF PB-CANCELLATION                                           
02410         IF PB-C-REFERENCE NOT = PI-SAV-REFERENCE                  
02411            MOVE PI-SAV-REFERENCE TO PB-C-REFERENCE.               
02412                                                                   
02413  5050-REWRITE-PENDING-BUS.                                        
02414       EXEC CICS REWRITE                                           
02415           DATASET    (ERPNDB-FILE-ID)                             
02416           FROM       (PENDING-BUSINESS)                           
02417       END-EXEC.                                                   
02418                                                                   
02419      EXEC CICS STARTBR                                            
02420          DATASET(ERPNDB-FILE-ID)                                  
02421          RIDFLD (ERPNDB-KEY)                                      
02422      END-EXEC.                                                    
02423                                                                   
02424      GO TO 5010-GET-NEXT-RECORD.                                  
02425                                                                   
02426  5100-STOP-BROWSE.                                                
02427      EXEC CICS ENDBR                                              
02428          DATASET(ERPNDB-FILE-ID)                                  
02429      END-EXEC.                                                    
02430                                                                   
02431  5900-EXIT.                                                       
02432       EXIT.                                                       
02433                                                                   
02434      EJECT                                                        
02435                                                                   
02436 ******************************************************************
02437 *          R E W R I T E   E N T I R E   B A T C H              * 
02438 *  THIS SECTION REWRITES AN ENTIRE BATCH WITH NEW ACCOUNT INFO. * 
02439 *  1.  DELETE THE PENDING BUSINESS RECORD                       * 
02440 *  2.  IF THE PENDING RECORD IS AN ISSUE, AND THE CERTIFICATE   * 
02441 *      HAS NOT BEEN ADDED TO BATCH OR A CLAIM IS NOT ATTACHED,  * 
02442 *      DELETE THE CERTIFICATE,CERTIFICATE NOTES, AND MAILING    * 
02443 *      RECORDS                                                  * 
02444 *  3.  IF THE PENDING RECORD HAS BEEN ADDED TO BATCH OR A CLAIM * 
02445 *      IS ATTACHED MOVE SPACE TO CM-CREDIT-INTERFACE-SW-1 AND   * 
02446 *      REWRITE THE CERTIFICATE.                                 * 
02447 *  4.  IF THE PENDING RECORD IS CANCELLATION, BACKOUT THE       * 
02448 *      CANCELLATION INFORMATION IN THE CERTIFICATE RECORD       * 
02449 *      AND REWRITE THE CERTIFICATE.                             * 
02450 *  5.  WRITE A NEW PENDING BUSINESS RECORD WITH THE NEW ACCOUNT * 
02451 *      INFORMATION.                                             * 
02452 *  6.  AFTER EACH DETAIL RECORD IS PROCESSED REWRITE THE BATCH  * 
02453 *      RECORD WITH THE NEW ACCOUNT INFORMATION.                 * 
02454 *  7.  ONCE THE BATCH IS REWRITTEN FORCE THE OPERATOR TO        * 
02455 *      RE-EDITED THE BATCH.                                     * 
02456 *  NOTE:  THE PENDING BUSINESS RECORDS ARE DELETED AND ADDED    * 
02457 *         TO RESET THE ALTERNATE INDEX.                         * 
02458 ******************************************************************
02459  6000-REWRITE-ENTIRE-BATCH.                                       
02460      IF  NOT EMI-NO-ERRORS                                        
02461          GO TO 8200-SEND-DATAONLY.                                
02462                                                                   
02463      PERFORM 0400-PRIME-BATCH-TOTALS  THRU  0490-EXIT.            
02464                                                                   
02465      IF  EMI-ERROR = ZEROS                                        
02466          MOVE 'N'                TO  PI-EL630-FIRST-TIME-SW       
02467        ELSE                                                       
02468          GO TO 8200-SEND-DATAONLY.                                
02469                                                                   
02470      MOVE PI-SAV-COMP-CD         TO  PNDB-COMP-CD.                
02471      MOVE PI-SAV-ENTRY-BATCH     TO  PNDB-ENTRY-BATCH.            
02472                                                                   
02473      MOVE ZEROS                  TO  PNDB-BATCH-SEQ               
02474                                      PNDB-BATCH-CHG-SEQ.          
02475                                                                   
02476      EXEC CICS GETMAIN                                            
02477          SET    (ADDRESS OF PENDING-BUSINESS)                     
02478          LENGTH (585)                                             
02479          INITIMG(GETMAIN-SPACE)                                   
02480      END-EXEC.                                                    
02481                                                                   
02482      EXEC CICS HANDLE CONDITION                                   
02483          NOTFND(6500-NO-RECORDS)                                  
02484      END-EXEC.                                                    
02485                                                                   
02486  6010-GET-NEXT-RECORD.                                            
02487      IF WS-ON1-SW5 IS EQUAL TO 'Y'                                
02488          MOVE 'N'                TO  WS-ON1-SW5                   
02489          GO TO 6015-START-BROWSE.                                 
02490                                                                   
02491      EXEC CICS HANDLE CONDITION                                   
02492          NOTFND (6400-STOP-BROWSE)                                
02493          ENDFILE(6400-STOP-BROWSE)                                
02494      END-EXEC.                                                    
02495                                                                   
02496  6015-START-BROWSE.                                               
02497      EXEC CICS STARTBR                                            
02498          DATASET(ERPNDB-FILE-ID)                                  
02499          RIDFLD (ERPNDB-KEY)                                      
02500      END-EXEC.                                                    
02501                                                                   
02502  6020-READ-NEXT-RECORD.                                           
02503      EXEC CICS READNEXT                                           
02504          INTO   (PENDING-BUSINESS)                                
02505          DATASET(ERPNDB-FILE-ID)                                  
02506          RIDFLD (ERPNDB-KEY)                                      
02507      END-EXEC.                                                    
02508                                                                   
02509      IF WS-ON1-SW6 IS EQUAL TO 'Y'                                
02510          MOVE 'N'                    TO WS-ON1-SW6                
02511          MOVE PB-CONTROL-PRIMARY     TO WS-SAV-PNDB-KEY           
02512          GO TO 6030-READ-FOR-UPDATE.                              
02513                                                                   
02514      IF ERPNDB-KEY = WS-SAV-PNDB-KEY                              
02515          GO TO 6020-READ-NEXT-RECORD.                             
02516                                                                   
02517      IF PB-COMPANY-CD  = WS-SAV-COMP-CD  AND                      
02518         PB-ENTRY-BATCH = WS-SAV-ENTRY-BATCH                       
02519          NEXT SENTENCE                                            
02520      ELSE                                                         
02521          GO TO 6400-STOP-BROWSE.                                  
02522                                                                   
02523  6030-READ-FOR-UPDATE.                                            
02524      EXEC CICS ENDBR                                              
02525          DATASET(ERPNDB-FILE-ID)                                  
02526      END-EXEC.                                                    
02527                                                                   
02528      EXEC CICS READ                                               
02529          INTO (PENDING-BUSINESS)                                  
02530          DATASET (ERPNDB-FILE-ID)                                 
02531          RIDFLD  (ERPNDB-KEY)                                     
02532          UPDATE                                                   
02533      END-EXEC.                                                    
02534                                                                   
02535      MOVE ERPNDB-KEY                  TO WS-SAV-PNDB-KEY.         
02536                                                                   
02537      MOVE PENDING-BUSINESS       TO WS-ERPNDB-RECORD.             
02538                                                                   
02539  6050-DELETE-PENDING-BUS.                                         
02540      EXEC CICS DELETE                                             
02541          DATASET    (ERPNDB-FILE-ID)                              
02542      END-EXEC.                                                    
02543                                                                   
02544      MOVE WS-ERPNDB-RECORD       TO PENDING-BUSINESS.             
02545                                                                   
02546      IF  PB-BATCH-TRAILER                                         
02547          GO TO 6300-WRITE-PNDB-RECORD.                            
02548                                                                   
02549      MOVE PB-CONTROL-BY-ACCOUNT  TO ELCERT-KEY.                   
02550      IF  PB-SV-CARRIER GREATER SPACES                             
02551          MOVE PB-SV-CARRIER          TO CERT-CARRIER.             
02552      IF  PB-SV-GROUPING GREATER SPACES                            
02553          MOVE PB-SV-GROUPING         TO CERT-GROUPING.            
02554      IF  PB-SV-STATE GREATER SPACES                               
02555          MOVE PB-SV-STATE            TO CERT-STATE.               
02556                                                                   
02557      IF PB-ISSUE                                                  
02558          NEXT SENTENCE                                            
02559        ELSE                                                       
02560          GO TO 6200-PROCESS-CANCELS.                              
02561                                                                   
02562      EXEC CICS HANDLE CONDITION                                   
02563           NOTFND  (6300-WRITE-PNDB-RECORD)                        
02564      END-EXEC.                                                    
02565                                                                   
02566      EXEC CICS READ                                               
02567          SET     (ADDRESS OF CERTIFICATE-MASTER)                  
02568          DATASET (ELCERT-FILE-ID)                                 
02569          RIDFLD  (ELCERT-KEY)                                     
02570          UPDATE                                                   
02571      END-EXEC.                                                    
02572                                                                   
02573      IF INSURED-ADDR-PRESENT                                      
02574         MOVE 'Y'                  TO WS-CERT-ADDRESS-SW           
02575      ELSE                                                         
02576         MOVE ' '                  TO WS-CERT-ADDRESS-SW.          
02577                                                                   
02578      IF  CERT-NOTES-ARE-NOT-PRESENT                               
02579          MOVE ' '                TO WS-CERT-NOTE-SW               
02580      ELSE                                                         
02581          MOVE 'Y'                TO WS-CERT-NOTE-SW.              
02582                                                                   
02583      IF  CERT-ADDED-BATCH                                         
02584          EXEC CICS UNLOCK                                         
02585               DATASET (ELCERT-FILE-ID)                            
02586          END-EXEC                                                 
02587          GO TO 6300-WRITE-PNDB-RECORD.                            
02588                                                                   
02589      IF  CM-CLAIM-ATTACHED-COUNT GREATER +0                       
02590          NEXT SENTENCE                                            
02591         ELSE                                                      
02592          GO TO 6060-DELETE-CERTIFICATE.                           
02593                                                                   
02594      MOVE SPACE                 TO  CM-CREDIT-INTERFACE-SW-1.     
02595      MOVE '2'                   TO  CM-CLAIM-INTERFACE-SW.        
02596                                                                   
02597      EXEC CICS REWRITE                                            
02598           DATASET    (ELCERT-FILE-ID)                             
02599           FROM       (CERTIFICATE-MASTER)                         
02600      END-EXEC.                                                    
02601                                                                   
02602      GO TO 6300-WRITE-PNDB-RECORD.                                
02603                                                                   
02604  6060-DELETE-CERTIFICATE.                                         
02605                                                                   
02606      EXEC CICS DELETE                                             
02607           DATASET    (ELCERT-FILE-ID)                             
02608      END-EXEC.                                                    
02609                                                                   
02610 ******************************************************************
02611 *       DELETE CERTIFICATE NOTES FOR ALL DELETED CERTIFICATES    *
02612 ******************************************************************
02613                                                                   
02614      IF CERT-NOTES-ARE-PRESENT                                    
02615         NEXT SENTENCE                                             
02616      ELSE                                                         
02617         GO TO 6070-DELETE-MAILING.                                
02618                                                                   
02619      EXEC CICS HANDLE CONDITION                                   
02620           NOTFND  (6070-DELETE-MAILING)                           
02621      END-EXEC.                                                    
02622                                                                   
02623      EXEC CICS READ                                               
02624          EQUAL                                                    
02625          DATASET   (ERNOTE-FILE-ID)                               
02626          SET       (ADDRESS OF CERTIFICATE-NOTE)                  
02627          RIDFLD    (ELCERT-KEY)                                   
02628          UPDATE                                                   
02629      END-EXEC.                                                    
02630                                                                   
02631      EXEC CICS DELETE                                             
02632          DATASET   (ERNOTE-FILE-ID)                               
02633      END-EXEC.                                                    
02634                                                                   
02635 ******************************************************************
02636 *     DELETE MAILING ADDRESS RECORDS FOR DELETED CERTIFICATES    *
02637 ******************************************************************
02638                                                                   
02639  6070-DELETE-MAILING.                                             
02640      IF CERT-ADDRESS-PRESENT                                      
02641         NEXT SENTENCE                                             
02642      ELSE                                                         
02643         GO TO 6300-WRITE-PNDB-RECORD.                             
02644                                                                   
02645      EXEC CICS HANDLE CONDITION                                   
02646           NOTOPEN (6300-WRITE-PNDB-RECORD)                        
02647           NOTFND  (6300-WRITE-PNDB-RECORD)                        
02648      END-EXEC.                                                    
02649                                                                   
02650      EXEC CICS READ                                               
02651          EQUAL                                                    
02652          DATASET   (ERMAIL-FILE-ID)                               
02653          SET       (ADDRESS OF MAILING-DATA)                      
02654          RIDFLD    (ELCERT-KEY)                                   
02655          UPDATE                                                   
02656      END-EXEC.                                                    
02657                                                                   
02658      EXEC CICS DELETE                                             
02659          DATASET   (ERMAIL-FILE-ID)                               
02660      END-EXEC.                                                    
02661                                                                   
02662      GO TO 6300-WRITE-PNDB-RECORD.                                
02663                                                                   
02664  6200-PROCESS-CANCELS.                                            
02665      EXEC CICS HANDLE CONDITION                                   
02666           NOTFND  (6300-WRITE-PNDB-RECORD)                        
02667      END-EXEC.                                                    
02668                                                                   
02669      EXEC CICS READ                                               
02670          SET     (ADDRESS OF CERTIFICATE-MASTER)                  
02671          DATASET (ELCERT-FILE-ID)                                 
02672          RIDFLD  (ELCERT-KEY)                                     
02673          UPDATE                                                   
02674      END-EXEC.                                                    
02675                                                                   
02676      IF PB-CANCELLATION                                           
02677         IF PB-CI-LF-PRIOR-CANCEL-DT = LOW-VALUES                  
02678            MOVE SPACE                  TO CM-CREDIT-INTERFACE-SW-2
02679            MOVE ZERO                   TO CM-LF-ITD-CANCEL-AMT    
02680            MOVE LOW-VALUES             TO CM-LF-CANCEL-EXIT-DT    
02681                                           CM-LF-CANCEL-DT         
02682         ELSE                                                      
02683            MOVE SPACE                  TO CM-CREDIT-INTERFACE-SW-2
02684            MOVE PB-CI-LF-CANCEL-AMT    TO CM-LF-ITD-CANCEL-AMT    
02685            MOVE PB-CI-LF-PRIOR-CANCEL-DT TO CM-LF-CANCEL-DT       
02686            MOVE PB-CI-LIVES            TO CM-LIVES.               
02687                                                                   
02688      IF PB-CANCELLATION                                           
02689         IF PB-CI-LF-PRIOR-CANCEL-DT = LOW-VALUES                  
02690            IF CM-LF-STATUS-AT-CANCEL NOT EQUAL SPACE              
02691                MOVE CM-LF-STATUS-AT-CANCEL                        
02692                                         TO CM-LF-CURRENT-STATUS   
02693                MOVE SPACE                                         
02694                                         TO CM-LF-STATUS-AT-CANCEL 
02695             ELSE                                                  
02696                MOVE PB-CI-LF-POLICY-STATUS                        
02697                                         TO CM-LF-CURRENT-STATUS   
02698         ELSE                                                      
02699             MOVE PB-CI-LF-POLICY-STATUS TO CM-LF-CURRENT-STATUS.  
02700                                                                   
02701      IF PB-CANCELLATION                                           
02702         IF PB-CI-AH-PRIOR-CANCEL-DT = LOW-VALUES                  
02703            MOVE SPACE                  TO CM-CREDIT-INTERFACE-SW-2
02704            MOVE ZERO                   TO CM-AH-ITD-CANCEL-AMT    
02705            MOVE LOW-VALUES             TO CM-AH-CANCEL-EXIT-DT    
02706                                           CM-AH-CANCEL-DT         
02707         ELSE                                                      
02708            MOVE SPACE                  TO CM-CREDIT-INTERFACE-SW-2
02709            MOVE PB-CI-AH-CANCEL-AMT    TO CM-AH-ITD-CANCEL-AMT    
02710            MOVE PB-CI-AH-PRIOR-CANCEL-DT TO CM-AH-CANCEL-DT       
02711            MOVE PB-CI-LIVES            TO CM-LIVES.               
02712                                                                   
02713      IF PB-CANCELLATION                                           
02714         IF PB-CI-AH-PRIOR-CANCEL-DT = LOW-VALUES                  
02715            IF CM-AH-STATUS-AT-CANCEL NOT EQUAL SPACE              
02716                MOVE CM-AH-STATUS-AT-CANCEL                        
02717                                         TO CM-AH-CURRENT-STATUS   
02718                MOVE SPACE                                         
02719                                         TO CM-AH-STATUS-AT-CANCEL 
02720             ELSE                                                  
02721                MOVE PB-CI-AH-POLICY-STATUS                        
02722                                         TO CM-AH-CURRENT-STATUS   
02723         ELSE                                                      
02724             MOVE PB-CI-AH-POLICY-STATUS TO CM-AH-CURRENT-STATUS.  
02725                                                                   
02726  6255-REWRITE-CERT.                                               
02727                                                                   
02728      EXEC CICS REWRITE                                            
02729          DATASET    (ELCERT-FILE-ID)                              
02730          FROM       (CERTIFICATE-MASTER)                          
02731      END-EXEC.                                                    
02732                                                                   
02733  6300-WRITE-PNDB-RECORD.                                          
02734                                                                   
02735      MOVE 'X'                    TO  PB-FATAL-FLAG.               
02736                                                                   
02737      MOVE SPACES                 TO  PB-I-RATE-DEVIATION-LF       
02738                                      PB-I-RATE-DEVIATION-AH       
02739                                      PB-REIN-CD                   
02740                                      PB-SV-CARRIER                
02741                                      PB-SV-GROUPING               
02742                                      PB-SV-STATE.                 
02743                                                                   
02744      IF PB-BATCH-TRAILER                                          
02745          MOVE PI-AM-NAME         TO  PB-ACCOUNT-NAME              
02746          MOVE PI-SAV-FC-CARRIER  TO  PB-SV-CARRIER                
02747          MOVE PI-SAV-FC-GROUPING TO  PB-SV-GROUPING               
02748          MOVE PI-SAV-FC-STATE    TO  PB-SV-STATE.                 
02749                                                                   
02750      MOVE LOW-VALUES             TO  PB-ACCT-EFF-DT               
02751                                      PB-ACCT-EXP-DT.              
02752                                                                   
02753      MOVE ZEROS                  TO  PB-CALC-TOLERANCE.           
02754                                                                   
02755      IF  CARRIERL GREATER ZEROS                                   
02756          MOVE CARRIERI           TO PB-CARRIER                    
02757         ELSE                                                      
02758          MOVE SPACES             TO PB-CARRIER.                   
02759                                                                   
02760      IF  GROUPL   GREATER ZEROS                                   
02761          MOVE GROUPI             TO PB-GROUPING                   
02762         ELSE                                                      
02763          MOVE SPACES             TO PB-GROUPING.                  
02764                                                                   
02765      IF  STATEL   GREATER ZEROS                                   
02766          MOVE STATEI             TO PB-STATE                      
02767         ELSE                                                      
02768          MOVE SPACES             TO PB-STATE.                     
02769                                                                   
02770      IF  ACCOUNTL GREATER ZEROS                                   
02771          MOVE ACCOUNTI           TO PB-ACCOUNT                    
02772         ELSE                                                      
02773          MOVE SPACES             TO PB-ACCOUNT.                   
02774                                                                   
02775      IF WS-ON1-SW7 IS EQUAL TO 'Y'                                
02776          MOVE 'N'                TO WS-ON1-SW7                    
02777          MOVE PB-GROUPING        TO PI-SAV-GROUPING               
02778          MOVE PB-STATE           TO PI-SAV-STATE                  
02779          MOVE PB-ACCOUNT         TO PI-SAV-ACCOUNT.               
02780                                                                   
02781      EXEC CICS WRITE                                              
02782          DATASET(ERPNDB-FILE-ID)                                  
02783          FROM   (PENDING-BUSINESS)                                
02784          RIDFLD (PB-CONTROL-PRIMARY)                              
02785      END-EXEC.                                                    
02786                                                                   
02787      GO TO 6010-GET-NEXT-RECORD.                                  
02788                                                                   
02789  6400-STOP-BROWSE.                                                
02790      EXEC CICS ENDBR                                              
02791          DATASET(ERPNDB-FILE-ID)                                  
02792      END-EXEC.                                                    
02793                                                                   
02794      MOVE 'Y'                    TO  PI-UPDATE-SW.                
02795                                                                   
02796      IF PI-AR-PROCESSING                                          
02797         PERFORM 6600-REWRITE-REQUEST THRU 6690-EXIT.              
02798                                                                   
02799      MOVE ER-2990                TO EMI-ERROR.                    
02800      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  
02801      MOVE -1                     TO MAINTL.                       
02802      MOVE AL-SANON               TO MAINTA.                       
02803      MOVE PI-SAV-REFERENCE       TO REFO.                         
02804      GO TO 8200-SEND-DATAONLY.                                    
02805                                                                   
02806  6500-NO-RECORDS.                                                 
02807      MOVE ER-2242                TO EMI-ERROR.                    
02808      MOVE -1                     TO BATCHL.                       
02809      MOVE AL-UABON               TO BATCHA.                       
02810      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
02811      GO TO 8200-SEND-DATAONLY.                                    
02812                                                                   
02813      EJECT                                                        
02814                                                                   
02815 ******************************************************************
02816 *                                                                *
02817 *          R E W R I T E   R E Q U E S T   R E C O R D           *
02818 *                                                                *
02819 ******************************************************************
02820                                                                   
02821  6600-REWRITE-REQUEST.                                            
02822                                                                   
02823      EXEC CICS HANDLE CONDITION                                   
02824          NOTFND (6690-EXIT)                                       
02825      END-EXEC.                                                    
02826                                                                   
02827      MOVE PI-COMPANY-CD          TO  ERRQST-COMP-CD.              
02828      MOVE PI-SAV-ENTRY-BATCH     TO  ERRQST-ENTRY-BATCH.          
02829                                                                   
02830      EXEC CICS READ                                               
02831           DATASET    (ERRQST-FILE-ID)                             
02832           SET        (ADDRESS OF AR-REQUEST-RECORD)               
02833           RIDFLD     (ERRQST-KEY)                                 
02834           UPDATE                                                  
02835      END-EXEC.                                                    
02836                                                                   
02837      IF  CARRIERL GREATER ZEROS                                   
02838          MOVE CARRIERI           TO RQ-CARRIER-A1                 
02839                                     RQ-CARRIER-A2                 
02840                                     RQ-CARRIER-A2.                
02841                                                                   
02842      IF  GROUPL   GREATER ZEROS                                   
02843          MOVE GROUPI             TO RQ-GROUPING-A1                
02844                                     RQ-GROUPING-A2                
02845                                     RQ-GROUPING-A3.               
02846                                                                   
02847      IF  STATEL   GREATER ZEROS                                   
02848          MOVE STATEI             TO RQ-STATE-A1.                  
02849                                                                   
02850      IF  ACCOUNTL GREATER ZEROS                                   
02851          MOVE ACCOUNTI           TO RQ-ACCOUNT-A1                 
02852                                     RQ-ACCOUNT-A4.                
02853                                                                   
02854      IF PI-ZERO-CARRIER                                           
02855         MOVE ZERO                TO RQ-CARRIER-A2.                
02856                                                                   
02857      IF PI-ZERO-GROUPING                                          
02858         MOVE ZEROS               TO RQ-GROUPING-A2.               
02859                                                                   
02860      IF PI-ZERO-CAR-GROUP                                         
02861         MOVE ZEROS               TO RQ-CARRIER-A2                 
02862                                     RQ-GROUPING-A2.               
02863                                                                   
02864      MOVE PI-REMIT-AGENT (PI-SUB) TO RQ-FIN-RESP-A2               
02865                                      PI-FIN-RESP.                 
02866                                                                   
02867      MOVE PI-ACCT-AGENT  (PI-SUB) TO RQ-ACCT-AGENT-A2             
02868                                      PI-ACCOUNT-AGENT.            
02869                                                                   
02870      EXEC CICS REWRITE                                            
02871           DATASET    (ERRQST-FILE-ID)                             
02872           FROM       (AR-REQUEST-RECORD)                          
02873      END-EXEC.                                                    
02874                                                                   
02875  6690-EXIT.                                                       
02876      EXIT.                                                        
02877      EJECT                                                        
02878                                                                   
02879 ******************************************************************
02880 *                                                                *
02881 *           V E R I F Y   R E Q U E S T   R E C O R D            *
02882 *                                                                *
02883 ******************************************************************
02884                                                                   
02885  7000-VERIFY-REQUEST-REC.                                         
02886      EXEC CICS HANDLE CONDITION                                   
02887          NOTFND(7090-EXIT)                                        
02888      END-EXEC.                                                    
02889                                                                   
02890      MOVE PI-COMPANY-CD          TO ERRQST-COMP-CD.               
02891      MOVE PI-SAV-ENTRY-BATCH     TO ERRQST-ENTRY-BATCH.           
02892                                                                   
02893      EXEC CICS READ                                               
02894          SET    (ADDRESS OF AR-REQUEST-RECORD)                    
02895          DATASET(ERRQST-FILE-ID)                                  
02896          RIDFLD (ERRQST-KEY)                                      
02897      END-EXEC.                                                    
02898                                                                   
02899      IF RQ-STMT-DT NOT = LOW-VALUES                               
02900         MOVE ER-2126        TO EMI-ERROR                          
02901         MOVE -1             TO BATCHL                             
02902         MOVE AL-UABON       TO BATCHA                             
02903         PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT                 
02904         MOVE 'N'            TO WS-RECORD-FOUND-SW                 
02905         GO TO 7090-EXIT.                                          
02906                                                                   
02907 ******************************************************************
02908 *                                                                *
02909 *   IF THE BATCH IS BEING DELETED (PF2), DELETE REQUEST RECORD.  *
02910 *                                                                *
02911 ******************************************************************
02912                                                                   
02913      IF EIBAID = DFHPF2                                           
02914         NEXT SENTENCE                                             
02915      ELSE                                                         
02916         GO TO 7090-EXIT.                                          
02917                                                                   
02918      EXEC CICS READ                                               
02919          SET    (ADDRESS OF AR-REQUEST-RECORD)                    
02920          DATASET(ERRQST-FILE-ID)                                  
02921          RIDFLD (ERRQST-KEY)                                      
02922          UPDATE                                                   
02923      END-EXEC.                                                    
02924                                                                   
02925      EXEC CICS DELETE                                             
02926          DATASET(ERRQST-FILE-ID)                                  
02927      END-EXEC.                                                    
02928                                                                   
02929  7090-EXIT.                                                       
02930       EXIT.                                                       
02931                                                                   
02932       EJECT                                                       
02933                                                                   
02934  7100-REWRITE-REQUEST-REC.                                        
02935                                                                   
02936      EXEC CICS HANDLE CONDITION                                   
02937          NOTFND(7180-NO-REQUEST-REC)                              
02938      END-EXEC.                                                    
02939                                                                   
02940                                                                   
02941      MOVE PI-COMPANY-CD          TO ERRQST-COMP-CD.               
02942      MOVE PI-SAV-ENTRY-BATCH     TO ERRQST-ENTRY-BATCH.           
02943                                                                   
02944      EXEC CICS READ                                               
02945          SET    (ADDRESS OF AR-REQUEST-RECORD)                    
02946          DATASET(ERRQST-FILE-ID)                                  
02947          RIDFLD (ERRQST-KEY)                                      
02948          UPDATE                                                   
02949      END-EXEC.                                                    
02950                                                                   
02951      IF RQ-STMT-DT NOT = LOW-VALUES                               
02952         MOVE ER-2126        TO EMI-ERROR                          
02953         MOVE -1             TO BATCHL                             
02954         MOVE AL-UABON       TO BATCHA                             
02955         PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT                 
02956         MOVE 'N'            TO WS-RECORD-FOUND-SW                 
02957         GO TO 7190-EXIT.                                          
02958                                                                   
02959      MOVE PI-SAV-REFERENCE  TO RQ-REFERENCE-A1                    
02960                                RQ-REFERENCE-A2                    
02961                                RQ-REFERENCE-A4.                   
02962                                                                   
02963      EXEC CICS REWRITE                                            
02964          DATASET(ERRQST-FILE-ID)                                  
02965          FROM   (AR-REQUEST-RECORD)                               
02966      END-EXEC.                                                    
02967                                                                   
02968      GO TO 7190-EXIT.                                             
02969                                                                   
02970  7180-NO-REQUEST-REC.                                             
02971      MOVE ER-2132        TO EMI-ERROR.                            
02972      MOVE -1             TO BATCHL.                               
02973      MOVE AL-UABON       TO BATCHA.                               
02974      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                   
02975      MOVE 'N'            TO WS-RECORD-FOUND-SW.                   
02976      GO TO 7190-EXIT.                                             
02977                                                                   
02978  7190-EXIT.                                                       
02979      EXIT.                                                        
02980                                                                   
02981      EJECT                                                        
02982                                                                   
02983 ******************************************************************
02984 *                                                                 
02985 *     7200-REWRITE IS USED TO REWRITE THE REQUEST FILE WHEN       
02986 *     THE EOM DATE IS CHANGED.                                    
02987 *                                                                 
02988 ******************************************************************
02989                                                                   
02990  7200-REWRITE-REQUEST.                                            
02991      EXEC CICS HANDLE CONDITION                                   
02992          NOTFND(7280-NO-REQUEST-REC)                              
02993      END-EXEC.                                                    
02994                                                                   
02995      MOVE PI-COMPANY-CD          TO ERRQST-COMP-CD.               
02996      MOVE PI-SAV-ENTRY-BATCH     TO ERRQST-ENTRY-BATCH.           
02997                                                                   
02998      EXEC CICS READ                                               
02999          SET    (ADDRESS OF AR-REQUEST-RECORD)                    
03000          DATASET(ERRQST-FILE-ID)                                  
03001          RIDFLD (ERRQST-KEY)                                      
03002          UPDATE                                                   
03003      END-EXEC.                                                    
03004                                                                   
03005      IF RQ-STMT-DT NOT = LOW-VALUES                               
03006         MOVE ER-2126        TO EMI-ERROR                          
03007         MOVE -1             TO BATCHL                             
03008         MOVE AL-UABON       TO BATCHA                             
03009         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
03010         MOVE 'N'            TO WS-RECORD-FOUND-SW                 
03011         GO TO 7299-EXIT.                                          
03012                                                                   
03013                                                                   
03014      IF PI-NB-MONTH-END-DT NOT = RQ-MO-END-DT                     
03015         MOVE PI-NB-MONTH-END-DT  TO  RQ-MO-END-DT.                
03016                                                                   
03017      IF PI-SAV-REFERENCE NOT = RQ-REFERENCE-A1                    
03018         MOVE PI-SAV-REFERENCE    TO RQ-REFERENCE-A1               
03019                                     RQ-REFERENCE-A2               
03020                                     RQ-REFERENCE-A4.              
03021                                                                   
03022      EXEC CICS REWRITE                                            
03023          DATASET(ERRQST-FILE-ID)                                  
03024          FROM   (AR-REQUEST-RECORD)                               
03025      END-EXEC.                                                    
03026                                                                   
03027      GO TO 7299-EXIT.                                             
03028                                                                   
03029  7280-NO-REQUEST-REC.                                             
03030      MOVE ER-2132        TO EMI-ERROR.                            
03031      MOVE -1             TO BATCHL.                               
03032      MOVE AL-UABON       TO BATCHA.                               
03033      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
03034      MOVE 'N'            TO WS-RECORD-FOUND-SW.                   
03035                                                                   
03036  7299-EXIT.                                                       
03037      EXIT.                                                        
03038                                                                   
03039      EJECT                                                        
03040                                                                   
03041  8100-SEND-INITIAL-MAP.                                           
03042      MOVE WS-CURRENT-DT          TO DATEO.                        
03043      MOVE EIBTIME                TO TIME-IN.                      
03044      MOVE TIME-OUT               TO TIMEO.                        
03045      MOVE -1                     TO MAINTL.                       
03046      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     
03047      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                     
03048                                                                   
03049      MOVE PI-LIFE-OVERRIDE-L2    TO WS-PRM-OVERRIDE               
03050                                     WS-REFUND-OVERRIDE.           
03051      MOVE WS-PRM-HEADER          TO LFPHDGO.                      
03052      MOVE WS-REFUND-HEADER       TO LFRHDGO.                      
03053                                                                   
03054      MOVE PI-AH-OVERRIDE-L2      TO WS-PRM-OVERRIDE               
03055                                     WS-REFUND-OVERRIDE.           
03056      MOVE WS-PRM-HEADER          TO AHPHDGO.                      
03057      MOVE WS-REFUND-HEADER       TO AHRHDGO.                      
03058                                                                   
03059      IF NOT PI-AR-PROCESSING                                      
03060         MOVE AL-SADOF            TO REFHDGA                       
03061                                     REFA.                         
03062                                                                   
03063      MOVE AL-SANOF               TO PF5A.                         
03064                                                                   
03065      IF CARR-GROUP-ST-ACCNT-CNTL                                  
03066          NEXT SENTENCE                                            
03067      ELSE                                                         
03068          IF ST-ACCNT-CNTL                                         
03069              MOVE AL-SADOF       TO CARRHDGA                      
03070                                     GRPHDGA                       
03071              MOVE AL-SANOF       TO CARRIERA                      
03072                                     GROUPA                        
03073          ELSE                                                     
03074              IF CARR-ST-ACCNT-CNTL                                
03075                  MOVE AL-SADOF   TO GRPHDGA                       
03076                  MOVE AL-SANOF   TO GROUPA                        
03077              ELSE                                                 
03078                  IF ACCNT-CNTL                                    
03079                      MOVE AL-SADOF TO CARRHDGA                    
03080                                       GRPHDGA                     
03081                                       STHDGA                      
03082                      MOVE AL-SANOF TO CARRIERA                    
03083                                       GROUPA                      
03084                                       STATEA                      
03085                  ELSE                                             
03086                      IF CARR-ACCNT-CNTL                           
03087                          MOVE AL-SADOF TO GRPHDGA                 
03088                                           STHDGA                  
03089                          MOVE AL-SANOF TO GROUPA                  
03090                                           STATEA.                 
03091      EXEC CICS SEND                                               
03092          MAP      (MAP-NAME)                                      
03093          MAPSET   (MAPSET-NAME)                                   
03094          FROM     (EL630AO)
03095          ERASE                                                    
03096          CURSOR                                                   
03097      END-EXEC.                                                    
03098                                                                   
03099      GO TO 9100-RETURN-TRAN.                                      
03100      EJECT                                                        
03101                                                                   
03102  8200-SEND-DATAONLY.                                              
03103      MOVE WS-CURRENT-DT          TO DATEO.                        
03104      MOVE EIBTIME                TO TIME-IN.                      
03105      MOVE TIME-OUT               TO TIMEO.                        
03106      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     
03107      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                     
03108      EXEC CICS SEND                                               
03109          MAP      (MAP-NAME)                                      
03110          MAPSET   (MAPSET-NAME)                                   
03111          FROM     (EL630AO)                                       
03112          DATAONLY                                                 
03113          ERASEAUP                                                 
03114          CURSOR                                                   
03115      END-EXEC.                                                    
03116                                                                   
03117      GO TO 9100-RETURN-TRAN.                                      
03118                                                                   
03119  8300-SEND-TEXT.                                                  
03120      EXEC CICS SEND TEXT                                          
03121          FROM     (LOGOFF-TEXT)                                   
03122          LENGTH   (LOGOFF-LENGTH)                                 
03123          ERASE                                                    
03124          FREEKB                                                   
03125      END-EXEC.                                                    
03126                                                                   
03127      EXEC CICS RETURN                                             
03128      END-EXEC.                                                    
03129      EJECT                                                        
03130  8600-DEEDIT.                                                     
03131      EXEC CICS BIF DEEDIT                                         
03132          FIELD (WS-DEEDIT-FIELD)                                  
03133          LENGTH(10)                                               
03134       END-EXEC.                                                   
03135                                                                   
03136  8600-EXIT.                                                       
03137      EXIT.                                                        
03138      EJECT                                                        
03139  8800-UNAUTHORIZED-ACCESS.                                        
03140      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   
03141      GO TO 8300-SEND-TEXT.                                        
03142                                                                   
03143  8810-PF23.                                                       
03144      IF NOT PI-DATA-UPDATED                                       
03145          NEXT SENTENCE                                            
03146      ELSE                                                         
03147          MOVE -1                 TO PFENTERL                      
03148          MOVE ER-2213            TO EMI-ERROR                     
03149          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
03150          GO TO 4000-SHOW-TOTALS.                                  
03151                                                                   
03152      MOVE EIBAID                 TO PI-ENTRY-CD-1.                
03153      MOVE XCTL-005               TO PGM-NAME.                     
03154      GO TO 9300-XCTL.                                             
03155                                                                   
03156  9100-RETURN-TRAN.                                                
03157      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             
03158      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.         
03159      EXEC CICS RETURN                                             
03160          TRANSID    (TRANS-ID)                                    
03161          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     
03162          LENGTH     (WS-COMM-LENGTH)                              
03163      END-EXEC.                                                    
03164                                                                   
03165      MOVE ZEROS  TO RETURN-CODE.
03165      GOBACK.                                                      
03166                                                                   
03167  9200-RETURN-MAIN-MENU.                                           
03168      IF NOT PI-DATA-UPDATED                                       
03169          NEXT SENTENCE                                            
03170      ELSE                                                         
03171          MOVE -1                 TO PFENTERL                      
03172          MOVE ER-2213            TO EMI-ERROR                     
03173          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
03174          GO TO 4000-SHOW-TOTALS.                                  
03175                                                                   
03176      MOVE XCTL-626               TO PGM-NAME.                     
03177      GO TO 9300-XCTL.                                             
03178                                                                   
03179  9300-XCTL.                                                       
03180      EXEC CICS XCTL                                               
03181          PROGRAM    (PGM-NAME)                                    
03182          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     
03183          LENGTH     (WS-COMM-LENGTH)                              
03184      END-EXEC.                                                    
03185      MOVE ZEROS  TO RETURN-CODE.
03185      GOBACK.                                                      
03186                                                                   
03187  9400-CLEAR.                                                      
03188      IF NOT PI-DATA-UPDATED                                       
03189          NEXT SENTENCE                                            
03190      ELSE                                                         
03191          MOVE 'Y'                TO PI-CLEAR-ERROR-SW             
03192          MOVE -1                 TO PFENTERL                      
03193          MOVE ER-2213            TO EMI-ERROR                     
03194          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
03195          GO TO 4000-SHOW-TOTALS.                                  
03196                                                                   
03197      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     
03198      GO TO 9300-XCTL.                                             
03199                                                                   
03200  9500-PF12.                                                       
03201      MOVE XCTL-010               TO PGM-NAME.                     
03202      GO TO 9300-XCTL.                                             
03203                                                                   
03204  9600-PGMID-ERROR.                                                
03205      EXEC CICS HANDLE CONDITION                                   
03206          PGMIDERR    (8300-SEND-TEXT)                             
03207      END-EXEC.                                                    
03208                                                                   
03209      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           
03210      MOVE ' '                    TO PI-ENTRY-CD-1.                
03211      MOVE XCTL-005               TO PGM-NAME.                     
03212      MOVE PGM-NAME               TO LOGOFF-PGM.                   
03213      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  
03214      GO TO 9300-XCTL.                                             
03215                                                                   
03216  9700-DATE-LINK.                                                  
03217      MOVE LINK-ELDATCV           TO PGM-NAME.                     
03218      EXEC CICS LINK                                               
03219          PROGRAM    (PGM-NAME)                                    
03220          COMMAREA   (DATE-CONVERSION-DATA)                        
03221          LENGTH     (DC-COMM-LENGTH)                              
03222      END-EXEC.                                                    
03223                                                                   
03224  9900-ERROR-FORMAT.                                               
03225      IF NOT EMI-ERRORS-COMPLETE                                   
03226          MOVE LINK-001           TO PGM-NAME                      
03227          EXEC CICS LINK                                           
03228              PROGRAM    (PGM-NAME)                                
03229              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           
03230              LENGTH     (EMI-COMM-LENGTH)                         
03231          END-EXEC.                                                
03232                                                                   
03233  9900-EXIT.                                                       
03234      EXIT.                                                        
03235                                                                   
03236  9990-ABEND.                                                      
03237      MOVE LINK-004               TO PGM-NAME.                     
03238      MOVE DFHEIBLK               TO EMI-LINE1.                    
03239      EXEC CICS LINK                                               
03240          PROGRAM   (PGM-NAME)                                     
03241          COMMAREA  (EMI-LINE1)                                    
03242          LENGTH    (72)                                           
03243      END-EXEC.                                                    
03244                                                                   
03245      MOVE -1                     TO PFENTERL.                     
03246      GO TO 8200-SEND-DATAONLY.                                    
03247                                                                   
03248  9995-SECURITY-VIOLATION.                                         
03249                              COPY ELCSCTP.                        
03250                                                                   
03251  9995-EXIT.                                                       
03252      EXIT.                                                        
03253                                                                   
