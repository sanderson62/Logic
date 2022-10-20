00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL347 .                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 06/07/95 16:14:10.                 
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            
00008 *                            VMOD=2.007.                          
00009                                                                   
00010 *AUTHOR.     LOGIC, INC.                                          
00011 *            DALLAS, TEXAS.                                       
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
00026 *    THIS PROGRAM WILL BUILD THE ONLINE RETRIEVAL FILE (ELRETR).  
00027 *                                                                 
00028 *    THE PURPOSE IS TO DETERMINE WHICH CLAIMS ARE CLOSED AND      
00029 *    ELIGIBLE FOR PURGING TO THE ONLINE RETRIEVAL FILE.           
00030                                                                   
00031 *    INPUT FILES  - ELCNTL - CONTROL FILE                         
00032                                                                   
00033 *    I/O FILES    - ELRETR - RETRIEVE FILE                        
00034 *                 - ELMSTR - CLAIM MASTER                         
00035 *                                                                 
00036                                                                   
00037      EJECT                                                        
00038  ENVIRONMENT DIVISION.                                            
00039                                                                   
00040  INPUT-OUTPUT SECTION.                                            
00041                                                                   
00042  FILE-CONTROL.                                                    
00043                                                                   
00044      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   
00045                                                                   
00046      SELECT ELCNTL           ASSIGN TO SYS021-FBA1-ELCNTL         
00047                                  ORGANIZATION  INDEXED            
00048                                  ACCESS        DYNAMIC            
00049                                  RECORD KEY    CF-CONTROL-PRIMARY 
00050                                  FILE STATUS   ELCNTL-FILE-STATUS.
00051                                                                   
00052      SELECT ELRETR           ASSIGN TO SYS024-FBA1-ELRETR         
00053                                  ORGANIZATION  INDEXED            
00054                                  ACCESS        DYNAMIC            
00055                                  RECORD KEY    RL-CONTROL-PRIMARY 
00056                                  FILE STATUS   ELRETR-FILE-STATUS.
00057                                                                   
00058      SELECT ELMSTR           ASSIGN TO SYS023-FBA1-ELMSTR         
00059                                  ORGANIZATION  INDEXED            
00060                                  ACCESS        DYNAMIC            
00061                                  RECORD KEY    CLMS-KEY           
00062                                  FILE STATUS   ELMSTR-FILE-STATUS.
00063                                                                   
00064      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   
00065                                                                   
00066      EJECT                                                        
00067  DATA DIVISION.                                                   
00068                                                                   
00069  FILE SECTION.                                                    
00070                                                                   
00071  FD  ELCNTL.                                                      
00072      COPY ELCCNTL.                                                
00073                                                                   
00074      EJECT                                                        
00075                                                                   
00076  FD  ELMSTR.                                                      
00077  01  CLMS-MSTR.                                                   
00078      12  FILLER                  PIC XX.                          
00079      12  CLMS-KEY.                                                
00080          16  COMP-CD             PIC X.                           
00081          16  FILLER              PIC X(19).                       
00082      12  FILLER                  PIC X(328).                      
00083                                                                   
00084      EJECT                                                        
00085                                                                   
00086  FD  ELRETR.                                                      
00087      COPY ELCRETR.                                                
00088                                                                   
00089      EJECT                                                        
00090                                                                   
00091  FD  PRNTR                                                        
00092      BLOCK CONTAINS 0 RECORDS
00093      RECORDING MODE F.                                            
00094                                                                   
00095  01  PRT.                                                         
00096      12  P-CTL                   PIC  X.                          
00097      12  P-DATA                  PIC  X(132).                     
00098                                                                   
00099      EJECT                                                        
00100                                                                   
00101  FD  DISK-DATE                                                    
00102      COPY ELCDTEFD.                                               
00103                                                                   
00104      EJECT                                                        
00105                                                                   
00106  WORKING-STORAGE SECTION.                                         
00107  77  FILLER  PIC X(32)   VALUE '********************************'.
00108  77  FILLER  PIC X(32)   VALUE '*     EL347  WORKING STORAGE   *'.
00109  77  FILLER  PIC X(32)   VALUE '******** VMOD=2.007 ************'.
00110                                                                   
00111  77  DSP1    PIC Z9.                                              
00112  77  DSP2    PIC Z9.                                              
00113                                                                   
00114  01  WS.                                                          
00115      12  WS-RECS-INPUT           PIC 9(8)        VALUE ZERO.      
00116      12  WS-RECS-BUILT           PIC 9(8)        VALUE ZERO.      
00117      12  WS-TOTAL-RECS-INPUT     PIC 9(8)        VALUE ZERO.      
00118      12  WS-TOTAL-RECS-BUILT     PIC 9(8)        VALUE ZERO.      
00119      12  X                       PIC X.                           
00120      12  LN-CNT                  PIC S999 COMP-3 VALUE +99.       
00121      12  LN-MAX                  PIC S999 COMP-3 VALUE +56.       
00122      12  WS-PAGE                 PIC 9(5) COMP-3 VALUE ZERO.      
00123      12  PGM-SUB                 PIC S999 COMP   VALUE +347.      
00124      12  NDX                     PIC S99     VALUE ZERO.          
00125      12  WS-BUILD-RETRIEVE-AFTER-MONTHS                           
00126                                  PIC 99      VALUE ZERO.          
00127      12  WS-COMPANY-CD           PIC X.                           
00128      12  WS-COMPANY-ID           PIC XXX.                         
00129      12  WS-PROGRAM-OPTIONS.                                      
00130          16  WS-FREQUENCY        PIC X(4).                        
00131          16  WS-PRINT-OPTION     PIC X.                           
00132          16  WS-FORMAT-OPTION    PIC X.                           
00133          16  WS-PROCESS-OPTION   PIC X.                           
00134          16  WS-TOTAL-OPTION     PIC X.                           
00135                                                                   
00136      12  WS-DISPLAY-TIME         PIC 99B99B99.                    
00137      12  WS-LAST-CARRIER         PIC X       VALUE SPACES.        
00138      12  WS-CARRIER              PIC X       VALUE SPACES.        
00139      12  WS-LAST-CLAIM-NO        PIC X(7)    VALUE SPACES.        
00140      12  WS-CURRENT-BIN-DT       PIC XX.                          
00141      12  WS-TIME-OF-DAY.                                          
00142          16  WS-TIME             PIC 9(6).                        
00143          16  WS-HUN-SEC          PIC 99.                          
00144      12  WS-CURRENT-DATE.                                         
00145          16  WS-CURR-YR          PIC XX.                          
00146          16  WS-CURR-MO          PIC XX.                          
00147          16  WS-CURR-DA          PIC XX.                          
00148      12  WS-CURRENT-DATE-EDIT.                                    
00149          16  WS-CEDT-MO          PIC XX.                          
00150          16  FILLER              PIC X       VALUE '/'.           
00151          16  WS-CEDT-DA          PIC XX.                          
00152          16  FILLER              PIC X       VALUE '/'.           
00153          16  WS-CEDT-YR          PIC XX.                          
00154                                                                   
00155      12  WS-SKIP-ASSOC-RECS      PIC X       VALUE SPACES.        
00156          88  SKIP-ASSOC-RECS  VALUE 'X'.                          
00157                                                                   
00158      12  WS-ABEND-MESSAGE        PIC X(80)   VALUE SPACES.        
00159      12  WS-ABEND-FILE-STATUS    PIC XX      VALUE ZERO.          
00160      12  WS-RETURN-CODE          PIC S9(4)   VALUE ZERO COMP.     
00161      12  WS-ZERO                 PIC S9      VALUE ZERO COMP-3.   
00162                                                                   
00163      12  ELRETR-FILE-STATUS      PIC XX      VALUE ZERO.          
00164      12  ELCNTL-FILE-STATUS      PIC XX      VALUE ZERO.          
00165      12  ELMSTR-FILE-STATUS      PIC XX      VALUE ZERO.          
00166                                                                   
00167      12  WS-ASSOC-RECS.                                           
00168          16  WS-ASSOC-REC        PIC X(350) OCCURS 30.            
00169                                                                   
00170      12  WS-SAVE-ELMSTR          PIC X(350).                      
00171      12  WS-SAVE-ELCNTL          PIC X(750).                      
00172                                                                   
00173      12  A-NDX                   PIC 9(4) VALUE ZERO.             
00174                                                                   
00175  EJECT                                                            
00176  01  HDG-1.                                                       
00177      12  FILLER      PIC X       VALUE '1'.                       
00178      12  FILLER      PIC X(52)   VALUE SPACES.                    
00179      12  FILLER      PIC X(73)   VALUE 'RETRIEVE FILE BUILD'.     
00180      12  FILLER      PIC X(8)    VALUE 'EL347'.                   
00181                                                                   
00182  01  HDG-2.                                                       
00183      12  FILLER      PIC X       VALUE SPACE.                     
00184      12  FILLER      PIC X(47)   VALUE SPACES.                    
00185      12  HD-CLIENT   PIC X(30).                                   
00186      12  FILLER      PIC X(47)   VALUE SPACES.                    
00187      12  HD-RUN      PIC X(8).                                    
00188                                                                   
00189  01  HDG-3.                                                       
00190      12  FILLER      PIC X       VALUE SPACE.                     
00191      12  FILLER      PIC X(52)   VALUE SPACES.                    
00192      12  HD-DATE     PIC X(18).                                   
00193      12  FILLER      PIC X(52)   VALUE SPACES.                    
00194      12  FILLER      PIC X(5)    VALUE 'PAGE'.                    
00195      12  HD-PAGE     PIC Z(5).                                    
00196                                                                   
00197  01  HDG-4.                                                       
00198      12  FILLER      PIC X       VALUE '0'.                       
00199      12  FILLER      PIC X(35)                                    
00200                      VALUE ' CARR   CLAIM     CERT NO.'.          
00201      12  FILLER      PIC X(21)                                    
00202                      VALUE 'ASSOC. CLAIMS'.                       
00203      12  FILLER      PIC X(76)                                    
00204                      VALUE ' CLOSED  AFTER      PURGED'.          
00205                                                                   
00206  01  DETAIL-LINE.                                                 
00207      12  FILLER      PIC XXX     VALUE SPACES.                    
00208      12  DL-CARR     PIC X.                                       
00209      12  FILLER      PIC X(4)    VALUE SPACES.                    
00210      12  DL-CLAIM    PIC X(7).                                    
00211      12  FILLER      PIC XXX     VALUE SPACES.                    
00212      12  DL-CERT     PIC X(11).                                   
00213      12  FILLER      PIC X(10)   VALUE SPACES.                    
00214      12  DL-ASS.                                                  
00215          16  DL-SEQU  PIC Z9.                                     
00216          16  DL-OF    PIC X(4)    VALUE ' OF'.                    
00217          16  DL-TOTAL PIC Z9.                                     
00218      12  FILLER      PIC X(10)   VALUE SPACES.                    
00219      12  DLC-DATE    PIC 99B99B99.                                
00220      12  FILLER      PIC XX      VALUE SPACES.                    
00221      12  DL-MOS      PIC ZZZ.                                     
00222      12  FILLER      PIC X(6)    VALUE SPACES.                    
00223      12  DLP-DATE    PIC 99B99B99.                                
00224                                                                   
00225  01  GRAND-TOTALS.                                                
00226      12  FILLER      PIC X       VALUE '0'.                       
00227      12  FILLER      PIC X(34)                                    
00228                      VALUE '**** GRAND TOTALS ****'.              
00229                                                                   
00230  01  TOTAL-LINE1.                                                 
00231      12  FILLER      PIC X       VALUE '0'.                       
00232      12  FILLER      PIC X(34)                                    
00233                      VALUE '**** CLAIM MASTER INPUT'.             
00234      12  TL-RECS1    PIC ZZ,ZZZ,ZZ9.                              
00235                                                                   
00236  01  TOTAL-LINE2.                                                 
00237      12  FILLER      PIC X       VALUE '0'.                       
00238      12  FILLER      PIC X(34)                                    
00239                      VALUE '**** CLAIM RETRIEVE RECORDS BUILT'.   
00240      12  TL-RECS2    PIC ZZ,ZZZ,ZZ9.                              
00241                                                                   
00242      EJECT                                                        
00243      COPY ELCDATE.                                                
00244                                                                   
00245      EJECT                                                        
00246      COPY ELCMSTR.                                                
00247                                                                   
00248      EJECT                                                        
00249  PROCEDURE DIVISION.                                              
00250                                                                   
00251  0000-MAIN-LOGIC SECTION.                                         
00252      PERFORM OPEN-FILES.                                          
00253                                                                   
00254      PERFORM 0500-PROCESS-CLIENT.                                 
00255                                                                   
00256      PERFORM 8600-GRAND-TOTALS.                                   
00257                                                                   
00258      PERFORM CLOSE-FILES.                                         
00259                                                                   
00260      GOBACK.                                                      
00261                                                                   
00262  EJECT                                                            
00263                                                                   
00264  OPEN-FILES SECTION.                                              
00265      OPEN INPUT ELCNTL                                            
00266           I-O   ELRETR                                            
00267                 ELMSTR                                            
00268          OUTPUT PRNTR.                                            
00269                                                                   
00270      IF ELRETR-FILE-STATUS  NOT = '00' AND '97'                   
00271          MOVE 'ERROR OCCURRED OPEN - ELRETR'                      
00272                                  TO WS-ABEND-MESSAGE              
00273          MOVE ELRETR-FILE-STATUS TO WS-ABEND-FILE-STATUS          
00274          GO TO ABEND-PGM.                                         
00275                                                                   
00276      IF ELCNTL-FILE-STATUS  NOT = '00' AND '97'                   
00277          MOVE 'ERROR OCCURRED OPEN - ELCNTL'                      
00278                                  TO WS-ABEND-MESSAGE              
00279          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          
00280          GO TO ABEND-PGM.                                         
00281                                                                   
00282      IF ELMSTR-FILE-STATUS  NOT = '00' AND '97'                   
00283          MOVE 'ERROR OCCURRED OPEN - ELMSTR'                      
00284                                  TO WS-ABEND-MESSAGE              
00285          MOVE ELMSTR-FILE-STATUS TO WS-ABEND-FILE-STATUS          
00286          GO TO ABEND-PGM.                                         
00287                                                                   
CIDMOD*    ACCEPT WS-CURRENT-DATE.                                      
CIDMOD     ACCEPT WS-CURRENT-DATE FROM DATE.                            
00289                                                                   
00290      MOVE WS-CURR-MO             TO WS-CEDT-MO.                   
00291      MOVE WS-CURR-DA             TO WS-CEDT-DA.                   
00292      MOVE WS-CURR-YR             TO WS-CEDT-YR.                   
00293                                                                   
00294      MOVE WS-CURRENT-DATE        TO DC-GREG-DATE-1-YMD.           
00295      MOVE '3'                    TO DC-OPTION-CODE.               
00296      PERFORM 8000-DATE-CONVERT.                                   
00297      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.            
00298                                                                   
00299      MOVE WS-CURRENT-DATE-EDIT   TO HD-RUN.                       
00300                                                                   
00301  EJECT                                                            
00302  0500-PROCESS-CLIENT   SECTION.                                   
00303      MOVE LOW-VALUES             TO  CF-CONTROL-PRIMARY.          
00304                                                                   
00305  0500-START-ELCNTL.                                               
00306      START ELCNTL                                                 
00307          KEY GREATER CF-CONTROL-PRIMARY.                          
00308                                                                   
00309      IF ELCNTL-FILE-STATUS NOT = '00'                             
00310          MOVE 'ERROR OCCURRED START INITIAL - ELCNTL'             
00311                                  TO  WS-ABEND-MESSAGE             
00312          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00313          GO TO ABEND-PGM.                                         
00314                                                                   
00315  0500-READNEXT-ELCNTL.                                            
00316      READ ELCNTL NEXT.                                            
00317                                                                   
00318      IF ELCNTL-FILE-STATUS = '10'                                 
00319          GO TO E-O-F.                                             
00320                                                                   
00321      IF ELCNTL-FILE-STATUS NOT = ZERO                             
00322          MOVE 'ERROR OCCURRED READNEXT - ELCNTL'                  
00323                                  TO  WS-ABEND-MESSAGE             
00324          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00325          GO TO ABEND-PGM.                                         
00326                                                                   
00327      IF NOT CF-COMPANY-MASTER                                     
00328          GO TO 0500-READNEXT-ELCNTL.                              
00329                                                                   
00330      ACCEPT WS-TIME-OF-DAY  FROM  TIME.                           
00331                                                                   
00332      MOVE WS-TIME                TO WS-DISPLAY-TIME.              
00333      INSPECT WS-DISPLAY-TIME CONVERTING SPACES TO '.'             
00334                                                                   
00335      IF CF-COMPANY-ID = 'NCX' OR 'BAL' OR 'LII'                   
00336          GO TO 0500-READNEXT-ELCNTL.                              
00337                                                                   
00338      IF CO-HAS-CLAS-IC-CLAIM                                      
00339          NEXT SENTENCE                                            
00340        ELSE                                                       
00341          GO TO 0500-READNEXT-ELCNTL.                              
00342                                                                   
00343      DISPLAY 'EL347 BEGAN PROCESSING OF ' CF-COMPANY-ID           
00344         ' AT ' WS-DISPLAY-TIME.                                   
00345                                                                   
00346      MOVE CF-CL-MAIL-TO-NAME     TO HD-CLIENT.                    
00347                                                                   
00348      MOVE CF-COMPANY-ID          TO WS-COMPANY-ID.                
00349      MOVE CF-COMPANY-CD          TO WS-COMPANY-CD.                
00350                                                                   
00351      MOVE LOW-VALUES             TO CLMS-KEY.                     
00352      MOVE WS-COMPANY-CD          TO COMP-CD.                      
00353                                                                   
00354      START ELMSTR                                                 
00355          KEY EQUAL COMP-CD.                                       
00356                                                                   
00357      IF ELMSTR-FILE-STATUS = '23'                                 
00358          GO TO 0500-START-ELCNTL.                                 
00359                                                                   
00360      IF ELMSTR-FILE-STATUS NOT = ZERO                             
00361          MOVE 'ERROR OCCURRED START - ELMSTR'                     
00362                                  TO WS-ABEND-MESSAGE              
00363          MOVE ELMSTR-FILE-STATUS TO WS-ABEND-FILE-STATUS          
00364          GO TO ABEND-PGM.                                         
00365                                                                   
00366  1000-READNEXT-ELMSTR.                                            
00367      READ ELMSTR NEXT RECORD                                      
00368                  INTO CLAIM-MASTER.                               
00369                                                                   
00370      IF ELMSTR-FILE-STATUS = '10'                                 
00371          GO TO E-O-F.                                             
00372                                                                   
00373      IF ELMSTR-FILE-STATUS NOT = ZERO                             
00374          MOVE 'ERROR OCCURRED READNEXT - ELMSTR'                  
00375                                  TO WS-ABEND-MESSAGE              
00376          MOVE ELMSTR-FILE-STATUS TO WS-ABEND-FILE-STATUS          
00377          GO TO ABEND-PGM.                                         
00378                                                                   
00379      IF CL-COMPANY-CD NOT = WS-COMPANY-CD                         
00380          GO TO E-O-F.                                             
00381                                                                   
00382      ADD 1 TO WS-RECS-INPUT                                       
00383               WS-TOTAL-RECS-INPUT.                                
00384                                                                   
00385      IF CL-CARRIER = '9'        AND                               
00386         CL-CLAIM-NO = '9999999' AND                               
00387         CL-CERT-NO  = '99999999999'                               
00388          GO TO 1500-SKIP-REC.                                     
00389                                                                   
00390      IF CL-PURGED-DT NOT = SPACES AND LOW-VALUES AND ZEROS        
00391          MOVE CL-PURGED-DT           TO DC-BIN-DATE-1             
00392          MOVE ' '                    TO DC-OPTION-CODE            
00393          PERFORM 8000-DATE-CONVERT                                
00394          MOVE DC-GREG-DATE-1-MDY     TO DLP-DATE                  
00395          GO TO 1200-RETRIEVE-OUT.                                 
00396                                                                   
00397 *    DISPLAY CL-CARRIER ' ' CL-CLAIM-NO '  IN'.                   
00398                                                                   
00399      IF CL-CLAIM-STATUS NOT = 'C'                                 
00400          GO TO 1500-SKIP-REC.                                     
00401                                                                   
00402      IF CF-COMPANY-ID = 'DMD'                                     
00403         IF CL-ACTIVITY-CODE = 11                                  
00404 *          DISPLAY '* DMD - ACTIVITY CODE 11'                     
00405            GO TO 1500-SKIP-REC.                                   
00406                                                                   
00407      IF SKIP-ASSOC-RECS                                           
00408         IF CL-CLAIM-NO = WS-LAST-CLAIM-NO                         
00409 *           DISPLAY '** SKIPPED / ASSOC'                          
00410             MOVE ZERO             TO A-NDX                        
00411             GO TO 1000-READNEXT-ELMSTR.                           
00412                                                                   
00413      MOVE SPACES                 TO WS-SKIP-ASSOC-RECS.           
00414                                                                   
00415      MOVE CONTROL-FILE           TO WS-SAVE-ELCNTL.               
00416                                                                   
00417      IF CF-CARRIER-CONTROL-LEVEL = SPACES                         
00418          MOVE CL-CARRIER               TO WS-CARRIER              
00419        ELSE                                                       
00420          MOVE CF-CARRIER-CONTROL-LEVEL TO WS-CARRIER.             
00421                                                                   
00422      PERFORM 4000-GET-CARRIER-RECORD.                             
00423                                                                   
00424      MOVE WS-SAVE-ELCNTL         TO CONTROL-FILE.                 
00425                                                                   
00426      IF ELCNTL-FILE-STATUS = '23'                                 
00427 *********   NO CARRIER RECORD                                     
00428          GO TO 1000-READNEXT-ELMSTR.                              
00429                                                                   
00430      IF WS-BUILD-RETRIEVE-AFTER-MONTHS = ZERO OR 99               
00431          GO TO 1500-SKIP-REC.                                     
00432                                                                   
00433      IF CF-COMPANY-ID = 'DMD'                                     
00434          MOVE CL-LAST-MAINT-DT   TO DC-BIN-DATE-1                 
00435        ELSE                                                       
00436          MOVE CL-LAST-CLOSE-DT   TO DC-BIN-DATE-1.                
00437                                                                   
00438      MOVE WS-CURRENT-BIN-DT      TO DC-BIN-DATE-2.                
00439      MOVE '1'                    TO DC-OPTION-CODE.               
00440      PERFORM 8000-DATE-CONVERT.                                   
00441                                                                   
00442      IF DC-ELAPSED-MONTHS LESS WS-BUILD-RETRIEVE-AFTER-MONTHS     
00443 *        DISPLAY 'MONTHS AFTER CLOSE= '  DC-ELAPSED-MONTHS        
00444 *             ' BUILD-RETRIEVE-AFTER= '                           
00445 *                            WS-BUILD-RETRIEVE-AFTER-MONTHS       
00446 *        DISPLAY ' '                                              
00447          GO TO 1500-SKIP-REC.                                     
00448                                                                   
00449      IF CF-COMPANY-ID NOT = 'DMD'                                 
00450         IF CL-RESTORED-DT NOT = SPACES AND LOW-VALUES             
00451             MOVE CL-LAST-MAINT-DT   TO DC-BIN-DATE-1              
00452             MOVE WS-CURRENT-BIN-DT  TO DC-BIN-DATE-2              
00453             MOVE '1'                TO DC-OPTION-CODE             
00454             PERFORM 8000-DATE-CONVERT                             
00455             IF DC-ELAPSED-DAYS LESS 30                            
00456 *              DISPLAY 'RESTORED AND HAD ACTIVITY WITHIN 30 DAYS' 
00457 *              DISPLAY ' '                                        
00458                GO TO 1500-SKIP-REC.                               
00459                                                                   
00460      IF CL-ASSOC-CERT-TOTAL NUMERIC                               
00461      IF CL-ASSOC-CERT-TOTAL GREATER 1                             
00462         IF A-NDX GREATER ZERO                                     
00463            IF CL-CLAIM-NO = WS-LAST-CLAIM-NO                      
00464               ADD 1 TO A-NDX                                      
00465               MOVE CLAIM-MASTER     TO WS-ASSOC-REC (A-NDX)       
00466               GO TO 1000-READNEXT-ELMSTR                          
00467             ELSE                                                  
00468               MOVE ZERO             TO NDX                        
00469               PERFORM 2500-BUILD-ASSOC-RETR A-NDX TIMES           
00470               INITIALIZE WS-ASSOC-RECS                            
00471               MOVE 1                TO A-NDX                      
00472               MOVE CLAIM-MASTER     TO WS-ASSOC-REC (A-NDX)       
00473               MOVE CL-CLAIM-NO      TO WS-LAST-CLAIM-NO           
00474               GO TO 1000-READNEXT-ELMSTR                          
00475          ELSE                                                     
00476            MOVE 1                TO A-NDX                         
00477            MOVE CLAIM-MASTER     TO WS-ASSOC-REC (A-NDX)          
00478            MOVE CL-CLAIM-NO      TO WS-LAST-CLAIM-NO              
00479            GO TO 1000-READNEXT-ELMSTR.                            
00480                                                                   
00481      IF A-NDX GREATER ZERO                                        
00482          MOVE ZERO                TO NDX                          
00483          PERFORM 2500-BUILD-ASSOC-RETR A-NDX TIMES                
00484          INITIALIZE WS-ASSOC-RECS                                 
00485          MOVE ZERO                TO A-NDX.                       
00486                                                                   
00487  1200-RETRIEVE-OUT.                                               
00488 *    DISPLAY CL-CARRIER ' ' CL-CLAIM-NO '  OUT'.                  
00489 *    DISPLAY ' '                                                  
00490                                                                   
00491      PERFORM 3000-BUILD-RETRIEVE.                                 
00492                                                                   
00493      MOVE CL-CLAIM-NO             TO WS-LAST-CLAIM-NO.            
00494                                                                   
00495      GO TO 1000-READNEXT-ELMSTR.                                  
00496                                                                   
00497  1500-SKIP-REC.                                                   
00498      IF A-NDX GREATER ZERO                                        
00499         IF CL-CLAIM-NO = WS-LAST-CLAIM-NO                         
00500            INITIALIZE WS-ASSOC-RECS                               
00501            MOVE ZERO               TO A-NDX                       
00502            MOVE 'X'                TO WS-SKIP-ASSOC-RECS          
00503          ELSE                                                     
00504            MOVE ZERO               TO NDX                         
00505            PERFORM 2500-BUILD-ASSOC-RETR A-NDX TIMES              
00506            INITIALIZE WS-ASSOC-RECS                               
00507            MOVE ZERO                TO A-NDX.                     
00508                                                                   
00509      MOVE CL-CLAIM-NO            TO WS-LAST-CLAIM-NO.             
00510                                                                   
00511      GO TO 1000-READNEXT-ELMSTR.                                  
00512                                                                   
00513  EJECT                                                            
00514                                                                   
00515  E-O-F.                                                           
00516      IF A-NDX GREATER ZERO                                        
00517          MOVE ZERO            TO NDX                              
00518          PERFORM 2500-BUILD-ASSOC-RETR A-NDX TIMES                
00519          INITIALIZE WS-ASSOC-RECS                                 
00520          MOVE ZERO            TO A-NDX.                           
00521                                                                   
00522      PERFORM 8500-END-OF-COMPANY.                                 
00523                                                                   
00524      IF ELCNTL-FILE-STATUS = '10'                                 
00525          GO TO 1500-EXIT.                                         
00526                                                                   
00527      MOVE SPACES                 TO WS-LAST-CLAIM-NO.             
00528                                                                   
00529      GO TO 0500-START-ELCNTL.                                     
00530                                                                   
00531  1500-EXIT.                                                       
00532       EXIT.                                                       
00533                                                                   
00534  EJECT                                                            
00535                                                                   
00536  2500-BUILD-ASSOC-RETR  SECTION.                                  
00537      MOVE CLAIM-MASTER            TO WS-SAVE-ELMSTR.              
00538      ADD 1 TO NDX.                                                
00539                                                                   
00540      MOVE WS-ASSOC-REC  (NDX)     TO CLAIM-MASTER.                
00541                                                                   
00542      PERFORM 3000-BUILD-RETRIEVE.                                 
00543                                                                   
00544      MOVE WS-SAVE-ELMSTR          TO CLAIM-MASTER                 
00545                                      CLMS-MSTR.                   
00546                                                                   
00547  2500-EXIT.                                                       
00548       EXIT.                                                       
00549                                                                   
00550  3000-BUILD-RETRIEVE SECTION.                                     
00551      MOVE CL-CARRIER               TO DL-CARR.                    
00552      MOVE CL-CLAIM-NO              TO DL-CLAIM.                   
00553      MOVE CL-CERT-NO               TO DL-CERT.                    
00554                                                                   
00555      IF CL-ASSOC-CERT-TOTAL NUMERIC                               
00556          MOVE CL-ASSOC-CERT-SEQU   TO DL-SEQU.                    
00557                                                                   
00558      IF CL-ASSOC-CERT-TOTAL NUMERIC  AND                          
00559         CL-ASSOC-CERT-TOTAL GREATER 1                             
00560          MOVE ' OF'                TO DL-OF                       
00561          MOVE CL-ASSOC-CERT-TOTAL  TO DL-TOTAL                    
00562       ELSE                                                        
00563          MOVE SPACES               TO DL-ASS.                     
00564                                                                   
00565      MOVE CL-LAST-CLOSE-DT         TO DC-BIN-DATE-1.              
00566      MOVE ' '                      TO DC-OPTION-CODE.             
00567      PERFORM 8000-DATE-CONVERT.                                   
00568      MOVE DC-GREG-DATE-1-MDY       TO DLC-DATE.                   
00569                                                                   
00570      MOVE WS-BUILD-RETRIEVE-AFTER-MONTHS                          
00571                                    TO DL-MOS.                     
00572                                                                   
00573      PERFORM 7500-PRINT-LINE.                                     
00574                                                                   
00575      PERFORM 6000-WRITE-ELRETR.                                   
00576                                                                   
00577      PERFORM 7000-DELETE-ELMSTR.                                  
00578                                                                   
00579  3000-EXIT.                                                       
00580       EXIT.                                                       
00581                                                                   
00582  EJECT                                                            
00583                                                                   
00584  4000-GET-CARRIER-RECORD  SECTION.                                
00585      IF WS-LAST-CARRIER = WS-CARRIER                              
00586          GO TO 4000-EXIT.                                         
00587                                                                   
00588  4000-GET-CARRIER.                                                
00589      MOVE WS-COMPANY-ID          TO CF-COMPANY-ID.                
00590      MOVE SPACES                 TO CF-ACCESS-OF-CARRIER.         
00591      MOVE '6'                    TO CF-RECORD-TYPE.               
00592      MOVE WS-CARRIER             TO CF-CARRIER-CNTL               
00593                                     WS-LAST-CARRIER.              
00594                                                                   
00595      READ ELCNTL.                                                 
00596                                                                   
00597      IF ELCNTL-FILE-STATUS = '23'                                 
00598 *        DISPLAY '** CARRIER NOT FOUND- ' WS-CARRIER              
00599          GO TO 4000-EXIT.                                         
00600                                                                   
00601      IF ELCNTL-FILE-STATUS NOT = ZERO                             
00602          MOVE 'ERROR OCCURRED READ - ELCNTL'                      
00603                                  TO WS-ABEND-MESSAGE              
00604          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          
00605          GO TO ABEND-PGM.                                         
00606                                                                   
00607      IF CF-BUILD-RETRIEVE-AFTER-MONTHS NOT NUMERIC                
00608          MOVE ZERO      TO CF-BUILD-RETRIEVE-AFTER-MONTHS.        
00609                                                                   
00610 ***********************  TEMP *****************************       
00611 *    MOVE CF-MONTHS-BEFORE-PURGED                                 
00612 *                       TO CF-BUILD-RETRIEVE-AFTER-MONTHS.        
00613 ***********************  TEMP *****************************       
00614                                                                   
00615      MOVE CF-BUILD-RETRIEVE-AFTER-MONTHS  TO                      
00616           WS-BUILD-RETRIEVE-AFTER-MONTHS.                         
00617                                                                   
00618 *    DISPLAY '*** BUILD RETRIEVE FOR CARRIER ' WS-CARRIER         
00619 *            ' AFTER CLOSED FOR '                                 
00620 *              WS-BUILD-RETRIEVE-AFTER-MONTHS ' MONTHS'.          
00621                                                                   
00622  4000-EXIT.                                                       
00623       EXIT.                                                       
00624                                                                   
00625  EJECT                                                            
00626                                                                   
00627  6000-WRITE-ELRETR  SECTION.                                      
00628      MOVE CLAIM-MASTER           TO RETRIEVE-MASTER.              
00629      MOVE 'RL'                   TO RL-RECORD-ID.                 
00630                                                                   
00631      WRITE RETRIEVE-MASTER.                                       
00632                                                                   
00633      IF ELRETR-FILE-STATUS = '00' OR '02'                         
00634          NEXT SENTENCE                                            
00635      ELSE                                                         
00636          IF ELRETR-FILE-STATUS = '22'                             
00637              PERFORM 6010-DELETE-DUPLICATE                        
00638          ELSE                                                     
00639              MOVE 'ERROR OCCURRED WRITE - ELRETR'                 
00640                                  TO WS-ABEND-MESSAGE              
00641              MOVE ELRETR-FILE-STATUS                              
00642                                  TO WS-ABEND-FILE-STATUS          
00643              GO TO ABEND-PGM.                                     
00644                                                                   
00645      ADD 1 TO WS-RECS-BUILT                                       
00646               WS-TOTAL-RECS-BUILT.                                
00647                                                                   
00648  6000-EXIT.                                                       
00649       EXIT.                                                       
00650                                                                   
00651  6010-DELETE-DUPLICATE.                                           
00652                                                                   
00653       DELETE ELRETR RECORD.                                       
00654                                                                   
00655       IF ELRETR-FILE-STATUS NOT = '00'                            
00656           MOVE 'ERROR OCCURRED DURING DELETE - ELRETR'            
00657                                  TO WS-ABEND-MESSAGE              
00658           MOVE ELRETR-FILE-STATUS TO WS-ABEND-FILE-STATUS         
00659           GO TO ABEND-PGM.                                        
00660                                                                   
00661       WRITE RETRIEVE-MASTER.                                      
00662                                                                   
00663       IF ELRETR-FILE-STATUS NOT = '00' AND '02'                   
00664           MOVE 'ERROR OCCURRED DURING DELETE/WRITE - ELRETR'      
00665                                  TO WS-ABEND-MESSAGE              
00666           MOVE ELRETR-FILE-STATUS TO WS-ABEND-FILE-STATUS         
00667           GO TO ABEND-PGM.                                        
00668                                                                   
00669  EJECT                                                            
00670                                                                   
00671  7000-DELETE-ELMSTR SECTION.                                      
00672                                                                   
00673      MOVE CLAIM-MASTER           TO CLMS-MSTR.                    
00674                                                                   
00675      DELETE ELMSTR RECORD.                                        
00676                                                                   
00677      IF ELMSTR-FILE-STATUS NOT = ZERO                             
00678          MOVE 'ERROR OCCURRED DELETE - ELMSTR'                    
00679                                  TO WS-ABEND-MESSAGE              
00680          MOVE ELMSTR-FILE-STATUS TO WS-ABEND-FILE-STATUS          
00681          GO TO ABEND-PGM.                                         
00682                                                                   
00683  7000-EXIT.                                                       
00684       EXIT.                                                       
00685                                                                   
00686  EJECT.                                                           
00687                                                                   
00688  7500-PRINT-LINE  SECTION.                                        
00689      IF LN-CNT GREATER LN-MAX                                     
00690          PERFORM 7600-PRINT-HDG.                                  
00691                                                                   
00692      MOVE DETAIL-LINE              TO PRT.                        
00693      PERFORM 7800-PRINT.                                          
00694      MOVE SPACES                   TO DETAIL-LINE.                
00695      MOVE ' OF'                    TO DL-OF.                      
00696                                                                   
00697  7500-EXIT.                                                       
00698       EXIT.                                                       
00699                                                                   
00700  7600-PRINT-HDG  SECTION.                                         
00701      ADD 1 TO WS-PAGE.                                            
00702      MOVE WS-PAGE          TO HD-PAGE.                            
00703      MOVE +0               TO LN-CNT.                             
00704      MOVE HDG-1            TO PRT.                                
00705      PERFORM 7800-PRINT.                                          
00706                                                                   
00707      MOVE HDG-2            TO PRT.                                
00708      PERFORM 7800-PRINT.                                          
00709                                                                   
00710      MOVE HDG-3            TO PRT.                                
00711      PERFORM 7800-PRINT.                                          
00712                                                                   
00713      MOVE HDG-4            TO PRT.                                
00714      PERFORM 7800-PRINT.                                          
00715                                                                   
00716      ADD +1 TO LN-CNT.                                            
00717                                                                   
00718  7600-EXIT.                                                       
00719       EXIT.                                                       
00720                                                                   
00721  7800-PRINT  SECTION.                                             
00722      MOVE P-CTL            TO X.                                  
00723      PERFORM 7900-PRINT-A-LINE.                                   
00724      ADD +1 TO LN-CNT.                                            
00725      MOVE SPACES           TO PRT.                                
00726                                                                   
00727  7800-EXIT.                                                       
00728       EXIT.                                                       
00729                                                                   
00730  7900-PRINT-A-LINE  SECTION.                                      
00731      MOVE X              TO P-CTL.                                
00732                                                                   
00733      IF P-CTL = ' '                                               
00734          WRITE PRT AFTER ADVANCING 1 LINE                         
00735       ELSE                                                        
00736      IF P-CTL = '0'                                               
00737          WRITE PRT AFTER ADVANCING 2 LINES                        
00738       ELSE                                                        
00739      IF P-CTL = '-'                                               
00740          WRITE PRT AFTER ADVANCING 3 LINES                        
00741       ELSE                                                        
00742          WRITE PRT AFTER ADVANCING PAGE.                          
00743                                                                   
00744  7900-EXIT.                                                       
00745       EXIT.                                                       
00746                                                                   
00747  EJECT                                                            
00748                                                                   
00749  8000-DATE-CONVERT  SECTION.                                      
00750      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   
00751                                                                   
00752  8000-EXIT.                                                       
00753       EXIT.                                                       
00754                                                                   
00755  EJECT                                                            
00756  8500-END-OF-COMPANY SECTION.                                     
00757      IF WS-RECS-INPUT = ZERO                                      
00758          GO TO 8500-EXIT.                                         
00759                                                                   
00760      MOVE WS-RECS-INPUT       TO TL-RECS1.                        
00761      MOVE WS-RECS-BUILT       TO TL-RECS2.                        
00762                                                                   
00763      MOVE TOTAL-LINE1         TO DETAIL-LINE.                     
00764      PERFORM 7500-PRINT-LINE.                                     
00765                                                                   
00766      MOVE TOTAL-LINE2         TO DETAIL-LINE.                     
00767      PERFORM 7500-PRINT-LINE.                                     
00768                                                                   
00769      MOVE 99                  TO LN-CNT.                          
00770      MOVE ZERO                TO WS-RECS-INPUT                    
00771                                  WS-RECS-BUILT.                   
00772                                                                   
00773  8500-EXIT.                                                       
00774       EXIT.                                                       
00775                                                                   
00776  8600-GRAND-TOTALS   SECTION.                                     
00777      MOVE '     *** GRAND TOTALS ***' TO HD-CLIENT.               
00778      MOVE ' '                     TO HDG-4.                       
00779      PERFORM 7600-PRINT-HDG.                                      
00780                                                                   
00781      MOVE GRAND-TOTALS        TO DETAIL-LINE.                     
00782      PERFORM 7500-PRINT-LINE.                                     
00783                                                                   
00784      MOVE WS-TOTAL-RECS-INPUT TO TL-RECS1.                        
00785      MOVE WS-TOTAL-RECS-BUILT TO TL-RECS2.                        
00786                                                                   
00787      MOVE TOTAL-LINE1         TO DETAIL-LINE.                     
00788      PERFORM 7500-PRINT-LINE.                                     
00789                                                                   
00790      MOVE TOTAL-LINE2         TO DETAIL-LINE.                     
00791      PERFORM 7500-PRINT-LINE.                                     
00792                                                                   
00793  8600-EXIT.                                                       
00794       EXIT.                                                       
00795                                                                   
00796  EJECT                                                            
00797                                                                   
00798  CLOSE-FILES  SECTION.                                            
00799      CLOSE ELCNTL                                                 
00800            ELMSTR                                                 
00801            ELRETR                                                 
00802            PRNTR.                                                 
00803                                                                   
00804      IF ELCNTL-FILE-STATUS NOT = ZERO                             
00805          MOVE 'ERROR OCCURRED CLOSE - ELCNTL'                     
00806                                  TO WS-ABEND-MESSAGE              
00807          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          
00808          GO TO ABEND-PGM.                                         
00809                                                                   
00810      IF ELMSTR-FILE-STATUS NOT = ZERO                             
00811          MOVE 'ERROR OCCURRED CLOSE - ELMSTR'                     
00812                                  TO WS-ABEND-MESSAGE              
00813          MOVE ELMSTR-FILE-STATUS TO WS-ABEND-FILE-STATUS          
00814          GO TO ABEND-PGM.                                         
00815                                                                   
00816      IF ELRETR-FILE-STATUS NOT = ZERO                             
00817          MOVE 'ERROR OCCURRED CLOSE - ELRETR'                     
00818                                  TO WS-ABEND-MESSAGE              
00819          MOVE ELRETR-FILE-STATUS TO WS-ABEND-FILE-STATUS          
00820          GO TO ABEND-PGM.                                         
00821                                                                   
00822  ABEND-PGM SECTION.                                               
00823      COPY ELCABEND SUPPRESS.                                      
00824                                                                   
