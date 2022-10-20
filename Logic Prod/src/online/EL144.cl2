00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL144 .                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 04/19/95 13:02:02.                 
00007 *                            VMOD=2.007                           
00008 *                                                                 
00008 *                                                                 
00009 *AUTHOR.    LOGIC, INC.                                           
00010 *           DALLAS, TEXAS.                                        
00011                                                                   
00012 *DATE-COMPILED.                                                   
00013                                                                   
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023                                                                   
00024 *REMARKS.                                                         
00025 *        THIS PROGRAM DISPLAYS ALL PENDING ACTIVITY               
00026 *    FROM THE ACTIVITY-QUE FILE ALONG WITH THE TYPE OF ACTIVITY.  
00027 *                                                                 
00028 *    SCREENS     - EL144A - PENDING ACTIVITY REVIEW               
00029 *                  EL144B - ACTIVITY MAINTENANCE                  
00030 *    ENTERED BY  - EL126  - PROCESSING MENU                       
00031 *                                                                 
00032 *    EXIT TO     - EL126  - RESULT OF CLEAR                       
00033 *                                                                 
00034 *    INPUT FILES - ELACTQ - ACTIVITY-QUE FILE                     
00035 *                                                                 
00036 *    OUTPUT FILES - ELACTQ                                        
00037 *                                                                 
00038 *    COMMAREA    - PASSED.                                        
00039 *                                                                 
00040 *                                                                 
00041 *    NARRATIVE   - ALL ACTIVITY QUE RECORDS ARE READ , THE TYPE   
00042 *                  OF ACTION IS DETERMINED, THE SCREEN IS BUILT   
00043 *                  THEN SENT.                                     
00044                                                                   
00045      EJECT                                                        
00046  ENVIRONMENT DIVISION.                                            
00047                                                                   
00048  DATA DIVISION.                                                   
00049                                                                   
00050  WORKING-STORAGE SECTION.                                         
00051                                                                   
00052  77  FILLER  PIC X(32)  VALUE '********************************'. 
00053  77  FILLER  PIC X(32)  VALUE '*   EL144  WORKING STORAGE     *'. 
00054  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.007 *********'. 
00055                                                                   
00056      COPY ELCSCTM.                                                
00057                                                                   
00058      COPY ELCSCRTY.                                               
00059                                                                   
00060  01  WS-DATE-AREA.                                                
00061      12  SAVE-DATE           PIC X(8)    VALUE SPACES.            
00062      12  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            
00063                                                                   
00064  01  FILLER                          COMP-3.                      
00065      12  WS-READNEXT-SW              PIC S9          VALUE ZERO.  
00066                                                                   
00067      12  TIME-IN                     PIC S9(7)       VALUE ZERO.  
00068      12  TIME-OUT                    REDEFINES                    
00069          TIME-IN                     PIC S9(3)V9(4).              
00070                                                                   
00071  01  FILLER.                                                      
00072      12  WS-DEEDIT-FIELD             PIC X(8) VALUE SPACES.       
00073      12  WS-DEEDIT-FIELD-V0 REDEFINES WS-DEEDIT-FIELD             
00074                                      PIC S9(8).                   
00075      12  WS-DEEDIT-LENGTH            PIC S9(4) VALUE +8 COMP.     
00076      12  GETMAIN-SPACE               PIC X     VALUE SPACE.       
00077      12  WS-ELACTQ-LENGTH            PIC S9(04) COMP VALUE +60.   
00078      12  WS-PMT-COUNT                PIC S9(01) COMP-3 VALUE +0.  
00079      12  WS-UNA-PMT-COUNT            PIC S9(01) COMP-3 VALUE +0.  
00080      12  WS-BROWSE-SW                PIC X(01) VALUE SPACES.      
00081      12  SC-ITEM                     PIC S9(04) COMP VALUE +1.    
00082      12  WS-AQ-CONTROL-PRIMARY.                                   
00083          16  WS-AQ-COMPANY-CD        PIC X(01).                   
00084          16  WS-AQ-CARRIER           PIC X(01).                   
00085          16  WS-AQ-CLAIM-NO          PIC X(07).                   
00086          16  WS-AQ-CERT-PRIME        PIC X(10).                   
00087          16  WS-AQ-CERT-SFX          PIC X(01).                   
00088      12  WS-ELACTQ-DSID              PIC X(08)   VALUE 'ELACTQ'.  
00089      12  WS-ELMSTR-DSID              PIC X(08)   VALUE 'ELMSTR'.  
00090      12  WS-MAPSET-NAME              PIC X(8)    VALUE 'EL144S'.  
00091      12  WS-MAP-NAME                 PIC X(8)    VALUE 'EL144A'.  
00092                                                                   
00093      12  FILLER                      REDEFINES                    
00094          WS-MAP-NAME.                                             
00095          16  FILLER                  PIC X(02).                   
00096          16  WS-MAP-NUMBER           PIC X(04).                   
00097          16  FILLER                  PIC X(02).                   
00098                                                                   
00099      12  THIS-PGM                    PIC X(8)      VALUE 'EL144'. 
00100      12  XCTL-PGM                    PIC X(8).                    
00101                                                                   
00102      12  WS-TRANS-ID                 PIC X(4)        VALUE 'EX54'.
00103                                                                   
00104      12  ER-0000                     PIC 9(4)        VALUE 0000.  
00105      12  ER-0004                     PIC 9(4)        VALUE 0004.  
00106      12  ER-0005                     PIC 9(4)        VALUE 0005.  
00107      12  ER-0008                     PIC 9(4)        VALUE 0008.  
00108      12  ER-0029                     PIC 9(4)        VALUE 0029.  
00109      12  ER-0048                     PIC 9(4)        VALUE 0048.  
00110      12  ER-0070                     PIC 9(4)        VALUE 0070.  
00111      12  ER-0284                     PIC 9(4)        VALUE 0284.  
00112      12  ER-0295                     PIC 9(4)        VALUE 0295.  
00113      12  ER-0296                     PIC 9(4)        VALUE 0296.  
00114      12  ER-0312                     PIC 9(4)        VALUE 0312.  
00115      12  ER-0313                     PIC 9(4)        VALUE 0313.  
00116      12  ER-0676                     PIC 9(4)        VALUE 0676.  
00117      12  ER-0677                     PIC 9(4)        VALUE 0677.  
00118      12  ER-0678                     PIC 9(4)        VALUE 0678.  
00119      12  ER-0679                     PIC 9(4)        VALUE 0679.  
00120      12  ER-0980                     PIC 9(4)        VALUE 0980.  
00121                                                                   
00122      EJECT                                                        
00123      COPY ELCINTF.                                                
00124                                                                   
00125      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                   
00126          16  PI-TOP-KEY       PIC X(20).                          
00127          16  PI-BOT-KEY       PIC X(20).                          
00128          16  PI-MAP-NAME      PIC X(06).                          
00129          16  FILLER           PIC X(586).                         
00130          16  PI-EIBAID-LAST   PIC X(8).                           
00131      EJECT                                                        
00132      COPY EL144S.                                                 
00133                                                                   
00134  01  FILLER                          REDEFINES                    
00135      EL144AI.                                                     
00136                                                                   
00137      12  FILLER                      PIC X(62).                   
00138                                                                   
00139      12  EL144A-SCREEN.                                           
00140          16  EL144A-LINES                OCCURS 16 TIMES          
00141                                          INDEXED BY EL144A-INDEX  
00142                                                     EL144A-INDEXB.
00143                                                                   
00144              20  EL144A-CLAIM-LENGTH     PIC S9(4)                
00145                                          COMP.                    
00146              20  EL144A-CLAIM-ATTRB      PIC X.                   
00147              20  EL144A-CLAIM            PIC X(7).                
00148                                                                   
00149              20  EL144A-CARRIER-LENGTH   PIC S9(4)                
00150                                      COMP.                        
00151              20  EL144A-CARRIER-ATTRB    PIC X.                   
00152              20  EL144A-CARRIER          PIC X.                   
00153                                                                   
00154              20  EL144A-CERT-NO-LENGTH   PIC S9(4)                
00155                                          COMP.                    
00156              20  EL144A-CERT-NO-ATTRB    PIC X.                   
00157              20  EL144A-CERT-NO          PIC X(11).               
00158                                                                   
00159              20  EL144A-PMT-LENGTH       PIC S9(4)                
00160                                          COMP.                    
00161              20  EL144A-PMT-ATTRB        PIC X.                   
00162              20  EL144A-PMT              PIC X(3).                
00163                                                                   
00164              20  EL144A-PCNT-LENGTH      PIC S9(4)                
00165                                          COMP.                    
00166              20  EL144A-PCNT-ATTRB       PIC X.                   
00167              20  EL144A-PCNT             PIC 9(01).               
00168                                                                   
00169              20  EL144A-PUCNT-LENGTH     PIC S9(4)                
00170                                          COMP.                    
00171              20  EL144A-PUCNT-ATTRB      PIC X.                   
00172              20  EL144A-PUCNT            PIC 9(01).               
00173                                                                   
00174              20  EL144A-STATUS-LENGTH    PIC S9(4)                
00175                                          COMP.                    
00176              20  EL144A-STATUS-ATTRB     PIC X.                   
00177              20  EL144A-STATUS           PIC X(06).               
00178              20  EL144A-LETR-LENGTH      PIC S9(4)                
00179                                          COMP.                    
00180              20  EL144A-LETR-ATTRB       PIC X.                   
00181              20  EL144A-LETR             PIC X(04).               
00182              20  EL144A-REST-LENGTH      PIC S9(4)                
00183                                          COMP.                    
00184              20  EL144A-REST-ATTRB       PIC X.                   
00185              20  EL144A-REST             PIC X.                   
00186                                                                   
00187                                                                   
00188      EJECT                                                        
00189      COPY ELCEMIB.                                                
00190                                                                   
00191      EJECT                                                        
00192      COPY ELCDATE.                                                
00193                                                                   
00194      EJECT                                                        
00195      COPY ELCLOGOF.                                               
00196                                                                   
00197      EJECT                                                        
00198      COPY ELCATTR.                                                
00199                                                                   
00200      EJECT                                                        
00201      COPY ELCAID.                                                 
00202                                                                   
00203  01  FILLER                      REDEFINES                        
00204      DFHAID.                                                      
00205                                                                   
00206      12  FILLER                      PIC X(8).                    
00207                                                                   
00208      12  PF-VALUES                   PIC X                        
00209          OCCURS 24 TIMES.                                         
00210      EJECT                                                        
00211  LINKAGE SECTION.                                                 
00212                                                                   
00213  01  DFHCOMMAREA                     PIC X(1024).                 
00214                                                                   
00215 *01 DFHBLLDS                         COMP SYNC.                   
00216 *    12  BLLCBAR                     PIC S9(9).                   
00217 *    12  ELACTQ-BLL                  PIC S9(9).                   
00218 *    12  ELMSTR-BLL                  PIC S9(9).                   
00219      EJECT                                                        
00220      COPY ELCACTQ.                                                
00221                                                                   
00222      EJECT                                                        
00223      COPY ELCMSTR.                                                
00224                                                                   
00225      EJECT                                                        
00226  PROCEDURE DIVISION.                                              
00227                                                                   
00228      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               
00229      MOVE '5'                   TO DC-OPTION-CODE.                
00230      PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT                  
00231      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    
00232      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                
00233                                                                   
00234      MOVE DFHCOMMAREA           TO  PROGRAM-INTERFACE-BLOCK.      
00235                                                                   
00236 *    NOTE ******************************************************* 
00237 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * 
00238 *         *  FROM ANOTHER MODULE.                               * 
00239 *         *******************************************************.
00240                                                                   
00241      IF EIBCALEN NOT GREATER THAN ZERO                            
00242          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   
00243          GO TO 8300-SEND-TEXT.                                    
00244                                                                   
00245      EXEC CICS HANDLE CONDITION                                   
00246          PGMIDERR (9600-PGMIDERR)                                 
00247          ERROR    (9990-ERROR)                                    
00248      END-EXEC.                                                    
00249                                                                   
00250      EJECT                                                        
00251  0010-MAIN-LOGIC.                                                 
00252                                                                   
00253      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         
00254         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                    
00255            MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6       
00256            MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5       
00257            MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4       
00258            MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3       
00259            MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2       
00260            MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1       
00261            MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM     
00262            MOVE THIS-PGM             TO  PI-CALLING-PROGRAM       
00263         ELSE                                                      
00264            MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM       
00265            MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM     
00266            MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1       
00267            MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2       
00268            MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3       
00269            MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4       
00270            MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5       
00271            MOVE SPACES               TO  PI-SAVED-PROGRAM-6       
00272      ELSE                                                         
00273         GO TO 0020-MAIN-LOGIC.                                    
00274                                                                   
00275  0015-MAIN-LOGIC.                                                 
00276 *    NOTE ******************************************************* 
00277 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      * 
00278 *         *  INTERFACE BLOCK FOR THIS MODULE.                   * 
00279 *         *******************************************************.
00280                                                                   
00281      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA         
00282                                                                   
00283      MOVE LOW-VALUES             TO  PI-TOP-KEY                   
00284                                      PI-BOT-KEY                   
00285      MOVE 'EL144A'               TO  PI-MAP-NAME                  
00286                                                                   
00287      IF EIBTRNID NOT EQUAL WS-TRANS-ID                            
00288         MOVE LOW-VALUES         TO  EL144AI                       
00289         GO TO 8100-SEND-INITIAL-MAP.                              
00290                                                                   
00291      EJECT                                                        
00292  0020-MAIN-LOGIC.                                                 
00293 *    NOTE ******************************************************* 
00294 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- * 
00295 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    * 
00296 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         * 
00297 *         *******************************************************.
00298                                                                   
00299      IF EIBAID EQUAL DFHCLEAR                                     
00300         IF PI-MAP-NAME = 'EL144B' OR 'EL144C'                     
00301            MOVE PI-TOP-KEY TO PI-BOT-KEY                          
00302                               WS-AQ-CONTROL-PRIMARY               
00303            MOVE 'EL144A' TO PI-MAP-NAME                           
00304            GO TO 1010-BYPASS-PRIME-KEY.                           
00305                                                                   
00306      IF EIBAID EQUAL DFHCLEAR                                     
00307          GO TO 9400-CLEAR.                                        
00308                                                                   
00309      IF EIBAID EQUAL DFHPA1 OR DFHPA2 OR DFHPA3                   
00310          MOVE LOW-VALUES         TO  EL144AI                      
00311          MOVE ER-0008            TO  EMI-ERROR                    
00312          GO TO 8200-SEND-DATAONLY.                                
00313                                                                   
00314      IF PI-MAP-NAME EQUAL 'EL144A'                                
00315         EXEC CICS RECEIVE                                         
00316              INTO   (EL144AI)                                     
00317              MAPSET (WS-MAPSET-NAME)                              
00318              MAP    (WS-MAP-NAME)                                 
00319         END-EXEC                                                  
00320      ELSE                                                         
00321      IF PI-MAP-NAME = 'EL144B'                                    
00322         EXEC CICS RECEIVE                                         
00323              INTO   (EL144AI)                                     
00324              MAPSET (WS-MAPSET-NAME)                              
00325              MAP    ('EL144B')                                    
00326         END-EXEC                                                  
00327         GO TO 5000-PROCESS-EL144B                                 
00328      ELSE                                                         
00329         EXEC CICS RECEIVE                                         
00330              INTO   (EL144AI)                                     
00331              MAPSET (WS-MAPSET-NAME)                              
00332              MAP    ('EL144C')                                    
00333         END-EXEC                                                  
00334         GO TO 6000-PROCESS-EL144C.                                
00335                                                                   
00336      IF PFKEYL GREATER ZERO                                       
00337         IF EIBAID NOT = DFHENTER                                  
00338            MOVE ER-0004        TO  EMI-ERROR                      
00339            MOVE AL-UNBOF       TO  PFKEYA                         
00340            MOVE -1             TO  PFKEYL                         
00341            GO TO 8200-SEND-DATAONLY                               
00342          ELSE                                                     
00343             IF PFKEYO IS NUMERIC                                  
00344               AND PFKEYO IS GREATER THAN ZERO                     
00345               AND PFKEYO IS LESS THAN '25'                        
00346                 MOVE PF-VALUES (PFKEYI) TO  EIBAID                
00347             ELSE                                                  
00348                MOVE ER-0029        TO  EMI-ERROR                  
00349                MOVE AL-UNBOF       TO  PFKEYA                     
00350                MOVE -1             TO  PFKEYL                     
00351                GO TO 8200-SEND-DATAONLY.                          
00352                                                                   
00353      IF EIBAID EQUAL DFHPF12                                      
00354         MOVE 'EL010'            TO  XCTL-PGM                      
00355         GO TO 9300-XCTL.                                          
00356                                                                   
00357      IF EIBAID EQUAL DFHPF23                                      
00358         GO TO 9000-RETURN-CICS.                                   
00359                                                                   
00360      IF EIBAID EQUAL DFHPF24                                      
00361         MOVE 'EL126'            TO  XCTL-PGM                      
00362         GO TO 9300-XCTL.                                          
00363                                                                   
00364      IF PI-PROCESSOR-ID EQUAL 'LGXX'                              
00365         NEXT SENTENCE                                             
00366      ELSE                                                         
00367         EXEC CICS READQ TS                                        
00368              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  
00369              INTO    (SECURITY-CONTROL)                           
00370              LENGTH  (SC-COMM-LENGTH)                             
00371              ITEM    (SC-ITEM)                                    
00372         END-EXEC                                                  
00373         MOVE SC-CLAIMS-DISPLAY (22)  TO  PI-DISPLAY-CAP           
00374         MOVE SC-CLAIMS-UPDATE  (22)  TO  PI-MODIFY-CAP            
00375         IF NOT DISPLAY-CAP                                        
00376            MOVE 'READ'              TO  SM-READ                   
00377            PERFORM 9995-SECURITY-VIOLATION                        
00378            MOVE ER-0070             TO  EMI-ERROR                 
00379            GO TO 8100-SEND-INITIAL-MAP.                           
00380                                                                   
00381      IF EIBAID = DFHENTER OR DFHPF1 OR DFHPF2 OR DFHPF3 OR        
00382                              DFHPF4                               
00383         NEXT SENTENCE                                             
00384      ELSE                                                         
00385         MOVE ER-0008            TO  EMI-ERROR                     
00386         MOVE -1                 TO  PFKEYL                        
00387         GO TO 8200-SEND-DATAONLY.                                 
00388                                                                   
00389      IF EIBAID EQUAL DFHPF3                                       
PEMMOD        IF PI-PROCESSOR-ID NOT EQUAL 'LGXX' AND 'PEMA'            
00391            MOVE ER-0008         TO  EMI-ERROR                     
00392            MOVE -1              TO  PFKEYL                        
00393            GO TO 8200-SEND-DATAONLY.                              
00394                                                                   
00395      IF EIBAID EQUAL DFHPF2                                       
00396         GO TO 2000-START-BROWSE-BWD.                              
00397                                                                   
00398      IF EIBAID EQUAL DFHPF3                                       
00399         MOVE 'EL144B' TO PI-MAP-NAME                              
00400         GO TO 8100-SEND-INITIAL-MAP.                              
00401                                                                   
00402      IF EIBAID = DFHPF4                                           
00403         MOVE EIBAID   TO PI-EIBAID-LAST                           
00404         MOVE LOW-VALUES TO EL144CI                                
00405         MOVE 'EL144C' TO PI-MAP-NAME                              
00406         GO TO 8100-SEND-INITIAL-MAP.                              
00407                                                                   
00408      EJECT                                                        
00409  1000-START-BROWSE-FWD.                                           
00410                                                                   
00411      MOVE LOW-VALUES       TO PI-TOP-KEY.                         
00412      MOVE PI-BOT-KEY       TO WS-AQ-CONTROL-PRIMARY.              
00413                                                                   
00414      MOVE PI-COMPANY-CD    TO WS-AQ-COMPANY-CD.                   
00415                                                                   
00416      IF CARRL  EQUAL +0 AND                                       
00417         CLAIML EQUAL +0 AND                                       
00418         CERTL  EQUAL +0 AND                                       
00419         SFXL   EQUAL +0                                           
00420         GO TO 1010-BYPASS-PRIME-KEY.                              
00421                                                                   
00422      MOVE LOW-VALUES       TO WS-AQ-CONTROL-PRIMARY.              
00423      MOVE PI-COMPANY-CD    TO WS-AQ-COMPANY-CD.                   
00424                                                                   
00425      IF CARRL  GREATER THAN +0                                    
00426         MOVE CARRI         TO WS-AQ-CARRIER.                      
00427                                                                   
00428      IF CLAIML GREATER THAN +0                                    
00429         MOVE CLAIMI        TO WS-AQ-CLAIM-NO.                     
00430                                                                   
00431      IF CERTL  GREATER THAN +0                                    
00432         MOVE CERTI         TO WS-AQ-CERT-PRIME.                   
00433                                                                   
00434      IF SFXL   GREATER THAN +0                                    
00435         MOVE SFXI          TO WS-AQ-CERT-SFX.                     
00436                                                                   
00437  1010-BYPASS-PRIME-KEY.                                           
00438                                                                   
00439      MOVE LOW-VALUES       TO EL144AI.                            
00440                                                                   
00441      EXEC CICS HANDLE CONDITION                                   
00442          ENDFILE  (1800-END-OF-FILE)                              
00443          NOTFND   (1800-END-OF-FILE)                              
00444      END-EXEC.                                                    
00445                                                                   
00446      EXEC CICS STARTBR                                            
00447          DATASET (WS-ELACTQ-DSID)                                 
00448          RIDFLD  (WS-AQ-CONTROL-PRIMARY)                          
00449          GTEQ                                                     
00450      END-EXEC.                                                    
00451                                                                   
00452      MOVE 'Y' TO WS-BROWSE-SW.                                    
00453      SET EL144A-INDEX TO +1.                                      
00454                                                                   
00455  1100-READNEXT.                                                   
00456                                                                   
00457      EXEC CICS READNEXT                                           
00458          DATASET (WS-ELACTQ-DSID)                                 
00459          RIDFLD  (WS-AQ-CONTROL-PRIMARY)                          
00460          SET     (ADDRESS OF ACTIVITY-QUE)                        
00461      END-EXEC.                                                    
00462                                                                   
00463      IF WS-AQ-COMPANY-CD NOT EQUAL PI-COMPANY-CD                  
00464          GO TO 1800-END-OF-FILE.                                  
00465                                                                   
00466      IF WS-AQ-CONTROL-PRIMARY EQUAL PI-BOT-KEY                    
00467         GO TO 1100-READNEXT.                                      
00468                                                                   
00469      IF NOT PI-NO-CARRIER-SECURITY                                
00470          IF WS-AQ-CARRIER NOT = PI-CARRIER-SECURITY               
00471             GO TO 1100-READNEXT.                                  
00472                                                                   
00473      IF PI-TOP-KEY EQUAL LOW-VALUES                               
00474         MOVE WS-AQ-CONTROL-PRIMARY TO PI-TOP-KEY.                 
00475                                                                   
00476      MOVE WS-AQ-CONTROL-PRIMARY  TO  PI-BOT-KEY.                  
00477                                                                   
00478      MOVE AQ-CLAIM-NO    TO  EL144A-CLAIM   (EL144A-INDEX).       
00479      MOVE AQ-CARRIER     TO  EL144A-CARRIER (EL144A-INDEX).       
00480      MOVE AQ-CERT-NO     TO  EL144A-CERT-NO (EL144A-INDEX).       
00481                                                                   
00482      IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC                       
00483         MOVE +0 TO AQ-PMT-UNAPPROVED-COUNT.                       
00484                                                                   
00485      IF PENDING-PAYMENTS                                          
00486         MOVE 'YES'       TO  EL144A-PMT (EL144A-INDEX)            
00487         MOVE AQ-PAYMENT-COUNTER                                   
00488                          TO  EL144A-PCNT (EL144A-INDEX)           
00489         MOVE AQ-PMT-UNAPPROVED-COUNT                              
00490                          TO  EL144A-PUCNT (EL144A-INDEX).         
00491                                                                   
00492      IF PENDING-FULL-PRINT                                        
00493         MOVE 'YES(F)'       TO EL144A-STATUS (EL144A-INDEX)       
00494      ELSE                                                         
00495      IF PENDING-PART-PRINT                                        
00496         MOVE 'YES(P)'       TO EL144A-STATUS (EL144A-INDEX).      
00497                                                                   
00498      IF PENDING-LETTERS                                           
00499         MOVE AQ-AUTO-LETTER TO EL144A-LETR (EL144A-INDEX).        
00500                                                                   
00501      MOVE AQ-PENDING-CLAIM-RESTORE                                
00502                             TO EL144A-REST (EL144A-INDEX).        
00503                                                                   
00504      IF EL144A-INDEX LESS THAN +16                                
00505         SET EL144A-INDEX UP BY +1                                 
00506         GO TO 1100-READNEXT.                                      
00507                                                                   
00508      GO TO 1900-END-BROWSE.                                       
00509                                                                   
00510  1800-END-OF-FILE.                                                
00511      MOVE ER-0313                TO  EMI-ERROR.                   
00512                                                                   
00513  1900-END-BROWSE.                                                 
00514                                                                   
00515      IF WS-BROWSE-SW EQUAL 'Y'                                    
00516         EXEC CICS ENDBR                                           
00517              DATASET (WS-ELACTQ-DSID)                             
00518         END-EXEC.                                                 
00519                                                                   
00520      MOVE -1                     TO  PFKEYL.                      
00521                                                                   
00522      GO TO 8100-SEND-INITIAL-MAP.                                 
00523                                                                   
00524      EJECT                                                        
00525  2000-START-BROWSE-BWD.                                           
00526                                                                   
00527      MOVE LOW-VALUES       TO PI-BOT-KEY.                         
00528      MOVE PI-TOP-KEY       TO WS-AQ-CONTROL-PRIMARY.              
00529                                                                   
00530      MOVE PI-COMPANY-CD    TO WS-AQ-COMPANY-CD.                   
00531                                                                   
00532      IF CARRL  EQUAL +0 AND                                       
00533         CLAIML EQUAL +0 AND                                       
00534         CERTL  EQUAL +0 AND                                       
00535         SFXL   EQUAL +0                                           
00536         GO TO 2010-BYPASS-PRIME-KEY.                              
00537                                                                   
00538      MOVE LOW-VALUES       TO WS-AQ-CONTROL-PRIMARY.              
00539      MOVE PI-COMPANY-CD    TO WS-AQ-COMPANY-CD.                   
00540                                                                   
00541      IF CARRL  GREATER THAN +0                                    
00542         MOVE CARRI         TO WS-AQ-CARRIER.                      
00543                                                                   
00544      IF CLAIML GREATER THAN +0                                    
00545         MOVE CLAIMI        TO WS-AQ-CLAIM-NO.                     
00546                                                                   
00547      IF CERTL  GREATER THAN +0                                    
00548         MOVE CERTI         TO WS-AQ-CERT-PRIME.                   
00549                                                                   
00550      IF SFXL   GREATER THAN +0                                    
00551         MOVE SFXI          TO WS-AQ-CERT-SFX.                     
00552                                                                   
00553  2010-BYPASS-PRIME-KEY.                                           
00554                                                                   
00555      MOVE LOW-VALUES       TO EL144AI.                            
00556                                                                   
00557      EXEC CICS HANDLE CONDITION                                   
00558          ENDFILE  (2800-END-OF-FILE)                              
00559          NOTFND   (2800-END-OF-FILE)                              
00560      END-EXEC.                                                    
00561                                                                   
00562      EXEC CICS STARTBR                                            
00563          DATASET (WS-ELACTQ-DSID)                                 
00564          RIDFLD  (WS-AQ-CONTROL-PRIMARY)                          
00565          GTEQ                                                     
00566      END-EXEC.                                                    
00567                                                                   
00568      MOVE 'Y' TO WS-BROWSE-SW.                                    
00569                                                                   
00570      EXEC CICS READNEXT                                           
00571          DATASET (WS-ELACTQ-DSID)                                 
00572          RIDFLD  (WS-AQ-CONTROL-PRIMARY)                          
00573          SET     (ADDRESS OF ACTIVITY-QUE)                        
00574      END-EXEC.                                                    
00575                                                                   
00576      IF PI-TOP-KEY EQUAL LOW-VALUES                               
00577         MOVE WS-AQ-CONTROL-PRIMARY TO PI-TOP-KEY.                 
00578                                                                   
00579      SET EL144A-INDEX TO +16.                                     
00580                                                                   
00581  2100-READPREV.                                                   
00582                                                                   
00583      EXEC CICS READPREV                                           
00584          DATASET (WS-ELACTQ-DSID)                                 
00585          RIDFLD  (WS-AQ-CONTROL-PRIMARY)                          
00586          SET     (ADDRESS OF ACTIVITY-QUE)                        
00587      END-EXEC.                                                    
00588                                                                   
00589      IF WS-AQ-COMPANY-CD NOT = PI-COMPANY-CD                      
00590          GO TO 2800-END-OF-FILE.                                  
00591                                                                   
00592      IF NOT PI-NO-CARRIER-SECURITY                                
00593         IF WS-AQ-CARRIER NOT = PI-CARRIER-SECURITY                
00594            GO TO 2100-READPREV.                                   
00595                                                                   
00596      IF WS-AQ-CONTROL-PRIMARY EQUAL PI-TOP-KEY                    
00597         GO TO 2100-READPREV.                                      
00598                                                                   
00599      IF PI-BOT-KEY EQUAL LOW-VALUES                               
00600         MOVE WS-AQ-CONTROL-PRIMARY TO PI-BOT-KEY.                 
00601                                                                   
00602      MOVE WS-AQ-CONTROL-PRIMARY  TO  PI-TOP-KEY.                  
00603                                                                   
00604      MOVE AQ-CLAIM-NO    TO  EL144A-CLAIM   (EL144A-INDEX).       
00605      MOVE AQ-CARRIER     TO  EL144A-CARRIER (EL144A-INDEX).       
00606      MOVE AQ-CERT-NO     TO  EL144A-CERT-NO (EL144A-INDEX).       
00607                                                                   
00608      IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC                       
00609         MOVE +0 TO AQ-PMT-UNAPPROVED-COUNT.                       
00610                                                                   
00611      IF PENDING-PAYMENTS                                          
00612         MOVE 'YES'       TO  EL144A-PMT (EL144A-INDEX)            
00613         MOVE AQ-PAYMENT-COUNTER                                   
00614                          TO  EL144A-PCNT (EL144A-INDEX)           
00615         MOVE AQ-PMT-UNAPPROVED-COUNT                              
00616                          TO EL144A-PUCNT (EL144A-INDEX).          
00617                                                                   
00618      IF PENDING-FULL-PRINT                                        
00619         MOVE 'YES(F)'       TO EL144A-STATUS (EL144A-INDEX)       
00620      ELSE                                                         
00621      IF PENDING-PART-PRINT                                        
00622         MOVE 'YES(P)'       TO EL144A-STATUS (EL144A-INDEX).      
00623                                                                   
00624      IF PENDING-LETTERS                                           
00625         MOVE AQ-AUTO-LETTER TO EL144A-LETR (EL144A-INDEX).        
00626                                                                   
00627      MOVE AQ-PENDING-CLAIM-RESTORE                                
00628                             TO EL144A-REST (EL144A-INDEX).        
00629                                                                   
00630      IF EL144A-INDEX GREATER THAN +1                              
00631         SET EL144A-INDEX DOWN BY +1                               
00632         GO TO 2100-READPREV.                                      
00633                                                                   
00634      GO TO 2900-END-BROWSE.                                       
00635                                                                   
00636  2800-END-OF-FILE.                                                
00637                                                                   
00638      PERFORM 3000-BUMP-SCREEN THRU 3999-EXIT.                     
00639                                                                   
00640      MOVE ER-0313                TO  EMI-ERROR.                   
00641                                                                   
00642  2900-END-BROWSE.                                                 
00643                                                                   
00644      IF WS-BROWSE-SW EQUAL 'Y'                                    
00645         EXEC CICS ENDBR                                           
00646              DATASET (WS-ELACTQ-DSID)                             
00647         END-EXEC.                                                 
00648                                                                   
00649      MOVE -1                     TO  PFKEYL.                      
00650                                                                   
00651      GO TO 8100-SEND-INITIAL-MAP.                                 
00652                                                                   
00653      EJECT                                                        
00654  3000-BUMP-SCREEN.                                                
00655                                                                   
00656      IF EL144A-INDEX EQUAL +16                                    
00657         GO TO 3999-EXIT.                                          
00658                                                                   
00659      PERFORM 4000-REARRANGE-SCREEN THRU 4099-EXIT                 
00660         UNTIL EL144A-LINES (1) NOT EQUAL LOW-VALUES.              
00661                                                                   
00662  3999-EXIT.                                                       
00663      EXIT.                                                        
00664                                                                   
00665  4000-REARRANGE-SCREEN.                                           
00666                                                                   
00667      IF EL144A-LINES (1) EQUAL LOW-VALUES                         
00668         MOVE EL144A-LINES (02) TO EL144A-LINES (01)               
00669         MOVE EL144A-LINES (03) TO EL144A-LINES (02)               
00670         MOVE EL144A-LINES (04) TO EL144A-LINES (03)               
00671         MOVE EL144A-LINES (05) TO EL144A-LINES (04)               
00672         MOVE EL144A-LINES (06) TO EL144A-LINES (05)               
00673         MOVE EL144A-LINES (07) TO EL144A-LINES (06)               
00674         MOVE EL144A-LINES (08) TO EL144A-LINES (07)               
00675         MOVE EL144A-LINES (09) TO EL144A-LINES (08)               
00676         MOVE EL144A-LINES (10) TO EL144A-LINES (09)               
00677         MOVE EL144A-LINES (11) TO EL144A-LINES (10)               
00678         MOVE EL144A-LINES (12) TO EL144A-LINES (11)               
00679         MOVE EL144A-LINES (13) TO EL144A-LINES (12)               
00680         MOVE EL144A-LINES (14) TO EL144A-LINES (13)               
00681         MOVE EL144A-LINES (15) TO EL144A-LINES (14)               
00682         MOVE EL144A-LINES (16) TO EL144A-LINES (15)               
00683         MOVE LOW-VALUES        TO EL144A-LINES (16).              
00684                                                                   
00685  4099-EXIT.                                                       
00686      EXIT.                                                        
00687                                                                   
00688      EJECT                                                        
00689  5000-PROCESS-EL144B.                                             
00690                                                                   
00691      IF EIBAID EQUAL DFHPF12                                      
00692         MOVE 'EL010'            TO  XCTL-PGM                      
00693         GO TO 9300-XCTL.                                          
00694                                                                   
00695      IF EIBAID EQUAL DFHPF23                                      
00696         GO TO 9000-RETURN-CICS.                                   
00697                                                                   
00698      IF EIBAID EQUAL DFHPF24                                      
00699         MOVE 'EL126'            TO  XCTL-PGM                      
00700         GO TO 9300-XCTL.                                          
00701                                                                   
00702      IF EIBAID EQUAL DFHENTER                                     
00703         NEXT SENTENCE                                             
00704      ELSE                                                         
00705         MOVE ER-0008            TO  EMI-ERROR                     
00706         MOVE -1                 TO  CARRL                         
00707         GO TO 8200-SEND-DATAONLY.                                 
00708                                                                   
00709      IF BCARRL  EQUAL +0 AND                                      
00710         BCLAIML EQUAL +0 AND                                      
00711         BCERTL  EQUAL +0 AND                                      
00712         BSFXL   EQUAL +0                                          
00713           MOVE -1           TO BCARRL                             
00714           GO TO 8200-SEND-DATAONLY.                               
00715                                                                   
00716      IF BCARRL  EQUAL +0 OR                                       
00717         BCLAIML EQUAL +0 OR                                       
00718         BCERTL  EQUAL +0                                          
00719           MOVE ER-0005      TO EMI-ERROR                          
00720           MOVE -1           TO BCARRL                             
00721           MOVE AL-UNBON     TO BCARRA                             
00722           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               
00723                                                                   
00724      IF BSFXI = LOW-VALUES                                        
00725         MOVE SPACES TO BSFXI.                                     
00726                                                                   
00727      IF BTYPEI EQUAL 'P' OR 'L' OR 'S' OR 'F'                     
00728         NEXT SENTENCE                                             
00729      ELSE                                                         
00730         MOVE ER-0676      TO EMI-ERROR                            
00731         MOVE -1           TO BTYPEL                               
00732         MOVE AL-UNBON     TO BTYPEA                               
00733         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 
00734                                                                   
00735      IF BTYPEI EQUAL 'P'                                          
00736         IF BPCNTI NOT NUMERIC OR                                  
00737            BPCNTI EQUAL '0'                                       
00738              MOVE ER-0677      TO EMI-ERROR                       
00739              MOVE -1           TO BPCNTL                          
00740              MOVE AL-UNBON     TO BPCNTA                          
00741              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             
00742         ELSE                                                      
00743              MOVE BPCNTI     TO WS-PMT-COUNT.                     
00744                                                                   
00745      IF BTYPEI EQUAL 'P'                                          
00746         IF BPUCNTI NOT NUMERIC OR                                 
00747            BPUCNTI EQUAL '0'                                      
00748              MOVE +0          TO WS-UNA-PMT-COUNT                 
00749         ELSE                                                      
00750              MOVE BPUCNTI     TO WS-UNA-PMT-COUNT.                
00751                                                                   
00752      IF BTYPEI EQUAL 'P'                                          
00753         IF WS-UNA-PMT-COUNT GREATER THAN +0                       
00754            IF WS-UNA-PMT-COUNT NOT EQUAL WS-PMT-COUNT             
00755               MOVE ER-0678      TO EMI-ERROR                      
00756               MOVE -1           TO BTYPEL                         
00757               MOVE AL-UNBON     TO BTYPEA                         
00758               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           
00759                                                                   
00760      IF NOT EMI-NO-ERRORS                                         
00761         GO TO 8200-SEND-DATAONLY.                                 
00762                                                                   
00763      MOVE PI-COMPANY-CD  TO WS-AQ-COMPANY-CD.                     
00764      MOVE BCARRI         TO WS-AQ-CARRIER.                        
00765      MOVE BCLAIMI        TO WS-AQ-CLAIM-NO.                       
00766      MOVE BCERTI         TO WS-AQ-CERT-PRIME.                     
00767      MOVE BSFXI          TO WS-AQ-CERT-SFX.                       
00768                                                                   
00769      EXEC CICS HANDLE CONDITION                                   
00770           NOTFND    (5050-CLAIM-NOT-FOUND)                        
00771      END-EXEC.                                                    
00772                                                                   
00773      EXEC CICS READ                                               
00774           DATASET    (WS-ELMSTR-DSID)                             
00775           RIDFLD     (WS-AQ-CONTROL-PRIMARY)                      
00776           SET        (ADDRESS OF CLAIM-MASTER)                    
00777      END-EXEC.                                                    
00778                                                                   
00779      EXEC CICS HANDLE CONDITION                                   
00780           NOTFND    (5040-ADD-ELACTQ)                             
00781      END-EXEC.                                                    
00782                                                                   
00783      EXEC CICS READ                                               
00784          DATASET (WS-ELACTQ-DSID)                                 
00785          RIDFLD  (WS-AQ-CONTROL-PRIMARY)                          
00786          SET     (ADDRESS OF ACTIVITY-QUE)                        
00787      END-EXEC.                                                    
00788                                                                   
00789      MOVE ER-0679      TO EMI-ERROR.                              
00790      MOVE -1           TO BCARRL                                  
00791      MOVE AL-UNBON     TO BCARRA                                  
00792                           BCLAIMA                                 
00793                           BCERTA                                  
00794                           BSFXA.                                  
00795                                                                   
00796      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00797      GO TO 8200-SEND-DATAONLY.                                    
00798                                                                   
00799  5040-ADD-ELACTQ.                                                 
00800                                                                   
00801      EXEC CICS GETMAIN                                            
00802           LENGTH     (WS-ELACTQ-LENGTH)                           
00803           INITIMG    (GETMAIN-SPACE)                              
00804           SET        (ADDRESS OF ACTIVITY-QUE)                    
00805      END-EXEC.                                                    
00806                                                                   
00807      MOVE 'AQ'            TO AQ-RECORD-ID                         
00808      MOVE PI-COMPANY-CD   TO AQ-COMPANY-CD                        
00809      MOVE BCARRI          TO AQ-CARRIER                           
00810      MOVE BCLAIMI         TO AQ-CLAIM-NO                          
00811      MOVE BCERTI          TO AQ-CERT-PRIME                        
00812      MOVE BSFXI           TO AQ-CERT-SFX                          
00813      MOVE LOW-VALUES      TO AQ-RESEND-DATE                       
00814                              AQ-FOLLOWUP-DATE.                    
00815                                                                   
00816      MOVE +0              TO AQ-PAYMENT-COUNTER                   
00817                              AQ-PMT-UNAPPROVED-COUNT              
00818                              AQ-LAST-UPDATED-BY.                  
00819                                                                   
00820      IF BTYPEI EQUAL 'P'                                          
00821         MOVE '1'      TO AQ-PENDING-PAYMENT-FLAG                  
00822      ELSE                                                         
00823      IF BTYPEI EQUAL 'S'                                          
00824         MOVE '2'      TO AQ-PENDING-STATUS-FLAG                   
00825      ELSE                                                         
00826      IF BTYPEI EQUAL 'F'                                          
00827         MOVE '1'      TO AQ-PENDING-STATUS-FLAG.                  
00828                                                                   
00829      IF BTYPEI EQUAL 'P'                                          
00830         MOVE WS-PMT-COUNT        TO AQ-PAYMENT-COUNTER            
00831         MOVE WS-UNA-PMT-COUNT    TO AQ-PMT-UNAPPROVED-COUNT.      
00832                                                                   
00833      IF (BTYPEI EQUAL 'L') AND                                    
00834         (BLETRI NOT EQUAL LOW-VALUES AND SPACES)                  
00835         NEXT SENTENCE                                             
00836      ELSE                                                         
00837         GO TO 5045-CONTINUE.                                      
00838                                                                   
00839      MOVE '1'                    TO AQ-PENDING-LETTER-FLAG        
00840      MOVE BLETRI                 TO AQ-AUTO-LETTER.               
00841                                                                   
00842      IF BSENDL NOT GREATER ZERO                                   
00843         MOVE ER-0295             TO  EMI-ERROR                    
00844         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
00845         MOVE -1                  TO  BSENDL                       
00846         MOVE AL-UNBON            TO  BSENDA                       
00847      ELSE                                                         
00848         MOVE BSENDI              TO  WS-DEEDIT-FIELD              
00849         PERFORM 8600-DEEDIT THRU 8600-EXIT                        
00850         MOVE WS-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY           
00851         MOVE '4'                 TO  DC-OPTION-CODE               
00852         PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT               
00853         IF DC-ERROR-CODE NOT = SPACE                              
00854            MOVE ER-0295          TO  EMI-ERROR                    
00855            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
00856            MOVE -1               TO  BSENDL                       
00857            MOVE AL-UNBON         TO  BSENDA                       
00858         ELSE                                                      
00859            MOVE DC-BIN-DATE-1    TO  AQ-RESEND-DATE               
00860            MOVE AL-UNNON         TO  BSENDA                       
00861            MOVE DC-GREG-DATE-1-EDIT                               
00862                                  TO  BSENDO.                      
00863                                                                   
00864      IF BFOLLOWL NOT GREATER ZERO                                 
00865         MOVE ER-0296             TO  EMI-ERROR                    
00866         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
00867         MOVE -1                  TO  BFOLLOWL                     
00868         MOVE AL-UNBON            TO  BFOLLOWA                     
00869      ELSE                                                         
00870         MOVE BFOLLOWI            TO  WS-DEEDIT-FIELD              
00871         PERFORM 8600-DEEDIT THRU 8600-EXIT                        
00872         MOVE WS-DEEDIT-FIELD-V0  TO  DC-GREG-DATE-1-MDY           
00873         MOVE '4'                 TO  DC-OPTION-CODE               
00874         PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT               
00875         IF DC-ERROR-CODE NOT = SPACE                              
00876            MOVE ER-0296          TO  EMI-ERROR                    
00877            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
00878            MOVE -1               TO  BFOLLOWL                     
00879            MOVE AL-UNBON         TO  BFOLLOWA                     
00880         ELSE                                                      
00881            MOVE DC-BIN-DATE-1    TO  AQ-FOLLOWUP-DATE             
00882            MOVE AL-UNNON         TO  BFOLLOWA                     
00883            MOVE DC-GREG-DATE-1-EDIT                               
00884                                  TO  BFOLLOWO.                    
00885                                                                   
00886  5045-CONTINUE.                                                   
00887                                                                   
00888      IF (NO-PENDING-ACTIVITY)                                     
00889               OR                                                  
00890         (AQ-PENDING-PAYMENT-FLAG EQUAL '1') AND                   
00891         (AQ-PAYMENT-COUNTER EQUAL +0)                             
00892               OR                                                  
00893         (AQ-PENDING-LETTER-FLAG EQUAL '1') AND                    
00894         (AQ-AUTO-LETTER EQUAL SPACES)                             
00895            MOVE ER-0048             TO EMI-ERROR                  
00896            MOVE -1                  TO BCARRL                     
00897            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              
00898                                                                   
00899      IF NOT EMI-NO-ERRORS                                         
00900         GO TO 8200-SEND-DATAONLY.                                 
00901                                                                   
00902      MOVE +144 TO AQ-LAST-UPDATED-BY.                             
00903                                                                   
00904      EXEC CICS WRITE                                              
00905           DATASET     (WS-ELACTQ-DSID)                            
00906           RIDFLD      (WS-AQ-CONTROL-PRIMARY)                     
00907           FROM        (ACTIVITY-QUE)                              
00908      END-EXEC.                                                    
00909                                                                   
00910      MOVE ER-0000      TO EMI-ERROR                               
00911      MOVE -1           TO BCARRL                                  
00912      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00913      GO TO 8200-SEND-DATAONLY.                                    
00914                                                                   
00915  5050-CLAIM-NOT-FOUND.                                            
00916                                                                   
00917      MOVE ER-0284      TO EMI-ERROR.                              
00918      MOVE -1           TO BCARRL.                                 
00919      MOVE AL-UNBON     TO BCARRA                                  
00920                           BCLAIMA                                 
00921                           BCERTA                                  
00922                           BSFXA.                                  
00923                                                                   
00924      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00925      GO TO 8200-SEND-DATAONLY.                                    
00926                                                                   
00927      EJECT                                                        
00928  6000-PROCESS-EL144C.                                             
00929      IF EIBAID = DFHPF12                                          
00930         MOVE 'EL010'            TO  XCTL-PGM                      
00931         GO TO 9300-XCTL.                                          
00932                                                                   
00933      IF EIBAID = DFHPF23                                          
00934         GO TO 9000-RETURN-CICS.                                   
00935                                                                   
00936      IF EIBAID = DFHPF24                                          
00937         MOVE 'EL126'            TO  XCTL-PGM                      
00938         GO TO 9300-XCTL.                                          
00939                                                                   
00940      IF EIBAID = DFHENTER                                         
00941         NEXT SENTENCE                                             
00942      ELSE                                                         
00943         MOVE ER-0008            TO  EMI-ERROR                     
00944         MOVE -1                 TO  CCARRL                        
00945         GO TO 8200-SEND-DATAONLY.                                 
00946                                                                   
00947      IF CCARRL  = +0 AND                                          
00948         CCLAIML = +0 AND                                          
00949         CCERTL  = +0 AND                                          
00950         CSFXL   = +0                                              
00951           MOVE -1           TO CCARRL                             
00952           GO TO 8200-SEND-DATAONLY.                               
00953                                                                   
00954      IF CCARRL  = +0 OR                                           
00955         CCLAIML = +0 OR                                           
00956         CCERTL  = +0                                              
00957           MOVE ER-0005      TO EMI-ERROR                          
00958           MOVE -1           TO CCARRL                             
00959           MOVE AL-UNBON     TO CCARRA                             
00960           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               
00961                                                                   
00962      IF CSFXI = LOW-VALUES                                        
00963         MOVE SPACES TO CSFXI.                                     
00964                                                                   
00965      IF (CRTYPL = +0)                                             
00966              OR                                                   
00967         (CRTYPI NOT = 'C' AND 'L' AND 'B')                        
00968           MOVE ER-0980      TO EMI-ERROR                          
00969           MOVE -1           TO CRTYPL                             
00970           MOVE AL-UNBON     TO CRTYPA                             
00971           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               
00972                                                                   
00973      IF NOT EMI-NO-ERRORS                                         
00974         GO TO 8200-SEND-DATAONLY.                                 
00975                                                                   
00976      MOVE PI-COMPANY-CD  TO WS-AQ-COMPANY-CD.                     
00977      MOVE CCARRI         TO WS-AQ-CARRIER.                        
00978      MOVE CCLAIMI        TO WS-AQ-CLAIM-NO.                       
00979      MOVE CCERTI         TO WS-AQ-CERT-PRIME.                     
00980      MOVE CSFXI          TO WS-AQ-CERT-SFX.                       
00981                                                                   
00982      EXEC CICS HANDLE CONDITION                                   
00983           NOTFND    (6040-ADD-ELACTQ)                             
00984      END-EXEC.                                                    
00985                                                                   
00986      EXEC CICS READ                                               
00987          DATASET (WS-ELACTQ-DSID)                                 
00988          RIDFLD  (WS-AQ-CONTROL-PRIMARY)                          
00989          SET     (ADDRESS OF ACTIVITY-QUE)                        
00990      END-EXEC.                                                    
00991                                                                   
00992      MOVE ER-0679      TO EMI-ERROR.                              
00993      MOVE -1           TO CCARRL.                                 
00994      MOVE AL-UNBON     TO CCARRA                                  
00995                           CCLAIMA                                 
00996                           CCERTA                                  
00997                           CSFXA.                                  
00998                                                                   
00999      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01000      GO TO 8200-SEND-DATAONLY.                                    
01001                                                                   
01002  6040-ADD-ELACTQ.                                                 
01003      EXEC CICS GETMAIN                                            
01004           LENGTH     (WS-ELACTQ-LENGTH)                           
01005           INITIMG    (GETMAIN-SPACE)                              
01006           SET        (ADDRESS OF ACTIVITY-QUE)                    
01007      END-EXEC.                                                    
01008                                                                   
01009      MOVE 'AQ'            TO AQ-RECORD-ID.                        
01010      MOVE PI-COMPANY-CD   TO AQ-COMPANY-CD.                       
01011      MOVE CCARRI          TO AQ-CARRIER.                          
01012      MOVE CCLAIMI         TO AQ-CLAIM-NO.                         
01013      MOVE CCERTI          TO AQ-CERT-PRIME.                       
01014      MOVE CSFXI           TO AQ-CERT-SFX.                         
01015      MOVE LOW-VALUES      TO AQ-RESEND-DATE                       
01016                              AQ-FOLLOWUP-DATE.                    
01017                                                                   
01018      MOVE +0              TO AQ-PAYMENT-COUNTER                   
01019                              AQ-PMT-UNAPPROVED-COUNT              
01020                              AQ-LAST-UPDATED-BY.                  
01021                                                                   
01022      MOVE CRTYPI          TO AQ-PENDING-CLAIM-RESTORE.            
01023                                                                   
01024  6045-CONTINUE.                                                   
01025      IF NOT EMI-NO-ERRORS                                         
01026         GO TO 8200-SEND-DATAONLY.                                 
01027                                                                   
01028      MOVE +144 TO AQ-LAST-UPDATED-BY.                             
01029                                                                   
01030      EXEC CICS WRITE                                              
01031           DATASET     (WS-ELACTQ-DSID)                            
01032           RIDFLD      (WS-AQ-CONTROL-PRIMARY)                     
01033           FROM        (ACTIVITY-QUE)                              
01034      END-EXEC.                                                    
01035                                                                   
01036      MOVE ER-0000      TO EMI-ERROR.                              
01037      MOVE -1           TO CCARRL.                                 
01038      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01039      GO TO 8200-SEND-DATAONLY.                                    
01040                                                                   
01041  EJECT                                                            
01042  8100-SEND-INITIAL-MAP.                                           
01043                                                                   
01044      IF EMI-ERROR NOT = ZERO                                      
01045         PERFORM 9900-ERROR-FORMAT.                                
01046                                                                   
PEMMOD     IF PI-PROCESSOR-ID NOT EQUAL 'LGXX' AND 'PEMA'               
01048         IF PI-MAP-NAME EQUAL 'EL144A'                             
01049            MOVE AL-SADOF       TO  PFKEY3A.                       
01050                                                                   
01051      IF PI-MAP-NAME EQUAL 'EL144A'                                
01052         MOVE EIBTIME                TO TIME-IN                    
01053         MOVE SAVE-DATE              TO DATEO                      
01054         MOVE TIME-OUT               TO TIMEO                      
01055         MOVE -1                     TO PFKEYL                     
01056         MOVE EMI-MESSAGE-AREA (1)   TO MSG1O                      
01057         EXEC CICS SEND                                            
01058              FROM   (EL144AI)                                     
01059              MAPSET (WS-MAPSET-NAME)                              
01060              MAP    (WS-MAP-NAME)                                 
01061              CURSOR                                               
01062              ERASE                                                
01063         END-EXEC                                                  
01064      ELSE                                                         
01065      IF PI-MAP-NAME = 'EL144B'                                    
01066         MOVE EIBTIME                TO TIME-IN                    
01067         MOVE SAVE-DATE              TO BDATEO                     
01068         MOVE TIME-OUT               TO BTIMEO                     
01069         MOVE -1                     TO BCARRL                     
01070         MOVE EMI-MESSAGE-AREA (1)   TO BMSG1O                     
01071         EXEC CICS SEND                                            
01072              FROM   (EL144AI)                                     
01073              MAPSET (WS-MAPSET-NAME)                              
01074              MAP    ('EL144B')                                    
01075              CURSOR                                               
01076              ERASE                                                
01077         END-EXEC                                                  
01078      ELSE                                                         
01079         MOVE EIBTIME                TO TIME-IN                    
01080         MOVE SAVE-DATE              TO CDATEO                     
01081         MOVE TIME-OUT               TO CTIMEO                     
01082         MOVE -1                     TO CCARRL                     
01083         MOVE EMI-MESSAGE-AREA (1)   TO CMSG1O                     
01084         EXEC CICS SEND                                            
01085              FROM   (EL144AI)                                     
01086              MAPSET (WS-MAPSET-NAME)                              
01087              MAP    ('EL144C')                                    
01088              CURSOR                                               
01089              ERASE                                                
01090         END-EXEC.                                                 
01091                                                                   
01092      GO TO 9100-RETURN-TRAN.                                      
01093                                                                   
01094  8200-SEND-DATAONLY.                                              
01095                                                                   
01096      IF EMI-ERROR NOT = ZERO                                      
01097          PERFORM 9900-ERROR-FORMAT.                               
01098                                                                   
PEMMOD     IF PI-PROCESSOR-ID NOT EQUAL 'LGXX' AND 'PEMA'               
01100         IF PI-MAP-NAME EQUAL 'EL144A'                             
01101            MOVE AL-SADOF       TO  PFKEY3A.                       
01102                                                                   
01103      IF PI-MAP-NAME EQUAL 'EL144A'                                
01104         MOVE EIBTIME                TO TIME-IN                    
01105         MOVE SAVE-DATE              TO DATEO                      
01106         MOVE TIME-OUT               TO TIMEO                      
01107         MOVE -1                     TO PFKEYL                     
01108         MOVE EMI-MESSAGE-AREA (1)   TO MSG1O                      
01109         EXEC CICS SEND DATAONLY                                   
01110             FROM   (EL144AI)                                      
01111             MAPSET (WS-MAPSET-NAME)                               
01112             MAP    (WS-MAP-NAME)                                  
01113             CURSOR                                                
01114         END-EXEC                                                  
01115      ELSE                                                         
01116      IF PI-MAP-NAME = 'EL144B'                                    
01117         MOVE EIBTIME                TO TIME-IN                    
01118         MOVE SAVE-DATE              TO BDATEO                     
01119         MOVE TIME-OUT               TO BTIMEO                     
01120         MOVE EMI-MESSAGE-AREA (1)   TO BMSG1O                     
01121         EXEC CICS SEND DATAONLY                                   
01122             FROM   (EL144AI)                                      
01123             MAPSET (WS-MAPSET-NAME)                               
01124             MAP    ('EL144B')                                     
01125             CURSOR                                                
01126         END-EXEC                                                  
01127      ELSE                                                         
01128         MOVE EIBTIME                TO TIME-IN                    
01129         MOVE SAVE-DATE              TO CDATEO                     
01130         MOVE TIME-OUT               TO CTIMEO                     
01131         MOVE EMI-MESSAGE-AREA (1)   TO CMSG1O                     
01132         EXEC CICS SEND DATAONLY                                   
01133             FROM   (EL144AI)                                      
01134             MAPSET (WS-MAPSET-NAME)                               
01135             MAP    ('EL144C')                                     
01136             CURSOR                                                
01137         END-EXEC.                                                 
01138                                                                   
01139      GO TO 9100-RETURN-TRAN.                                      
01140                                                                   
01141      EJECT                                                        
01142  8300-SEND-TEXT.                                                  
01143                                                                   
01144      EXEC CICS SEND TEXT                                          
01145          FROM   (LOGOFF-TEXT)                                     
01146          LENGTH (LOGOFF-LENGTH)                                   
01147          ERASE  FREEKB                                            
01148      END-EXEC.                                                    
01149                                                                   
01150      EXEC CICS RETURN                                             
01151      END-EXEC.                                                    
01152                                                                   
01153      EJECT                                                        
01154  8500-DATE-CONVERSION.                                            
01155                                                                   
01156      EXEC CICS LINK                                               
01157          PROGRAM  ('ELDATCV')                                     
01158          COMMAREA (DATE-CONVERSION-DATA)                          
01159          LENGTH   (DC-COMM-LENGTH)                                
01160      END-EXEC.                                                    
01161                                                                   
01162  8500-EXIT.                                                       
01163      EXIT.                                                        
01164                                                                   
01165      EJECT                                                        
01166  8600-DEEDIT.                                                     
01167      EXEC CICS BIF DEEDIT                                         
01168          FIELD  (WS-DEEDIT-FIELD)                                 
01169          LENGTH (WS-DEEDIT-LENGTH)                                
01170      END-EXEC.                                                    
01171                                                                   
01172  8600-EXIT.                                                       
01173      EXIT.                                                        
01174                                                                   
01175      EJECT                                                        
01176  9000-RETURN-CICS.                                                
01177                                                                   
01178      MOVE 'EL005'                TO  XCTL-PGM.                    
01179      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               
01180      GO TO 9300-XCTL.                                             
01181                                                                   
01182  9100-RETURN-TRAN.                                                
01183                                                                   
01184      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            
01185      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.        
01186                                                                   
01187      EXEC CICS RETURN                                             
01188          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       
01189          LENGTH   (PI-COMM-LENGTH)                                
01190          TRANSID  (WS-TRANS-ID)                                   
01191      END-EXEC.                                                    
01192                                                                   
01193  9100-EXIT.                                                       
01194      EXIT.                                                        
01195                                                                   
01196  9300-XCTL.                                                       
01197                                                                   
01198      MOVE DFHENTER               TO  EIBAID.                      
01199                                                                   
01200      EXEC CICS XCTL                                               
01201          PROGRAM  (XCTL-PGM)                                      
01202          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       
01203          LENGTH   (PI-COMM-LENGTH)                                
01204      END-EXEC.                                                    
01205                                                                   
01206  9300-EXIT.                                                       
01207      EXIT.                                                        
01208                                                                   
01209      EJECT                                                        
01210  9400-CLEAR.                                                      
01211                                                                   
01212      MOVE PI-RETURN-TO-PROGRAM  TO  XCTL-PGM.                     
01213      GO TO 9300-XCTL.                                             
01214                                                                   
01215  9600-PGMIDERR.                                                   
01216                                                                   
01217      EXEC CICS HANDLE CONDITION                                   
01218          PGMIDERR (8300-SEND-TEXT)                                
01219      END-EXEC.                                                    
01220                                                                   
01221      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM           
01222                                      LOGOFF-PGM.                  
01223                                                                   
01224      MOVE 'EL005'                TO  XCTL-PGM.                    
01225      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 
01226      MOVE SPACES                 TO  PI-ENTRY-CD-1.               
01227      GO TO 9300-XCTL.                                             
01228                                                                   
01229  9900-ERROR-FORMAT.                                               
01230                                                                   
01231      EXEC CICS LINK                                               
01232          PROGRAM  ('EL001')                                       
01233          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 
01234          LENGTH   (EMI-COMM-LENGTH)                               
01235      END-EXEC.                                                    
01236                                                                   
01237  9900-EXIT.                                                       
01238      EXIT.                                                        
01239                                                                   
01240  9990-ERROR.                                                      
01241                                                                   
01242      MOVE DFHEIBLK TO EMI-LINE1.                                  
01243      EXEC CICS LINK                                               
01244          PROGRAM  ('EL004')                                       
01245          COMMAREA (EMI-LINE1)                                     
01246          LENGTH   (72)                                            
01247      END-EXEC.                                                    
01248                                                                   
01249      GO TO 8200-SEND-DATAONLY.                                    
01250                                                                   
01251      EJECT                                                        
01252  9995-SECURITY-VIOLATION.                                         
01253      COPY ELCSCTP.                                                
01254                                                                   
01255  9995-EXIT.                                                       
01256      EXIT.                                                        
01257                                                                   
01258  9999-LAST-PARAGRAPH SECTION.                                     
01259                                                                   
01260      GOBACK.                                                      
01261                                                                   
