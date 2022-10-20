00001  ID DIVISION.                                                             
00002                                                                           
00003  PROGRAM-ID.                 EL605.                                       
00007 *                            VMOD=2.001.                                  
00008 *                                                                         
00008 *                                                                         
00009 *AUTHOR.     CENTRAL STATES HEALTH AND LIFE.                              
00010 *            OMAHA, NEBRASKA                                              
00011                                                                           
00012 *DATE-COMPILED.                                                           
00013                                                                           
00014 *SECURITY.   *****************************************************        
00015 *            *                                                   *        
00016 *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *        
00017 *            *                                                   *        
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *        
00019 *            *   OF     CSO     IS EXPRESSLY PROHIBITED WITHOUT  *        
00020 *            *   THE PRIOR WRITTEN PERMISSION OF CSO             *        
00021 *            *                                                   *        
00022 *            *****************************************************        
00023                                                                           
00024 *REMARKS.    TRANSACTION - EXAA - LIFE CLAIM INTEREST MENU                
00025                                                                           
00026  ENVIRONMENT DIVISION.                                                    
00027                                                                           
00028      EJECT                                                                
00029  DATA DIVISION.                                                           
00030  WORKING-STORAGE SECTION.                                                 
00031  77  FILLER  PIC X(32)  VALUE '********************************'.         
00032  77  FILLER  PIC X(32)  VALUE '*    EL605 WORKING STORAGE     *'.         
00033  77  FILLER  PIC X(32)  VALUE '*************V/M 2.001 *********'.         
00034                                                                           
00035  01  WS-DATE-AREA.                                                        
00036      05  SAVE-DATE           PIC X(8)    VALUE SPACES.                    
00037      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.                    
00038                                                                           
00039  01  MISC-WORK-AREAS.                                                     
00040                                                                           
00075      12  MAP-EL605A          PIC X(8)    VALUE 'EL605A'.                  
00076      12  MAPSET-EL605S       PIC X(8)    VALUE 'EL605S'.                  
00077      12  TRANS-EXAA          PIC X(4)    VALUE 'EXAA'.                    
00078      12  PGM-EL605           PIC X(8)    VALUE 'EL605'.                   
00079      12  PGM-NAME            PIC X(8).                                    
00080      12  TIME-IN             PIC S9(7).                                   
00081      12  TIME-OUT-R  REDEFINES TIME-IN.                                   
00082          16  FILLER          PIC X.                                       
00083          16  TIME-OUT        PIC 99V99.                                   
00084          16  FILLER          PIC X(2).                                    
00085      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.                 
00086      12  LINK-EL001          PIC X(8)    VALUE 'EL001'.                   
00087      12  LINK-EL004          PIC X(8)    VALUE 'EL004'.                   
00088                                                                           
00089      12  XCTL-EL005          PIC X(8)    VALUE 'EL005'.                   
00090      12  XCTL-EL010          PIC X(8)    VALUE 'EL010'.                   
00091      12  XCTL-EL102          PIC X(8)    VALUE 'EL102'.                   
00092      12  XCTL-EL103          PIC X(8)    VALUE 'EL103'.                   
00093      12  XCTL-EL104          PIC X(8)    VALUE 'EL104'.                   
00094      12  XCTL-EL105          PIC X(8)    VALUE 'EL105'.                   
00095      12  XCTL-EL106          PIC X(8)    VALUE 'EL106'.                   
00096      12  XCTL-EL107          PIC X(8)    VALUE 'EL107'.                   
00097      12  XCTL-EL108          PIC X(8)    VALUE 'EL108'.                   
00098      12  XCTL-EL110          PIC X(8)    VALUE 'EL110'.                   
00099      12  XCTL-EL111          PIC X(8)    VALUE 'EL111'.                   
00100      12  XCTL-EL112          PIC X(8)    VALUE 'EL112'.                   
00101      12  XCTL-EL119          PIC X(8)    VALUE 'EL119'.                   
00102      12  XCTL-EL126          PIC X(8)    VALUE 'EL800'.                   
00103      12  XCTL-EL158          PIC X(8)    VALUE 'EL158'.                   
00105      12  XCTL-EL602          PIC X(8)    VALUE 'EL602'.                   
00106      12  XCTL-EL603          PIC X(8)    VALUE 'EL603'.                   
00107      12  XCTL-EL604          PIC X(8)    VALUE 'EL604'.                   
00108      12  XCTL-EL605          PIC X(8)    VALUE 'EL605'.                   
00109      12  XCTL-EL606          PIC X(8)    VALUE 'EL606'.                   
00110      12  XCTL-EL607          PIC X(8)    VALUE 'EL607'.                   
00111      12  XCTL-EL608          PIC X(8)    VALUE 'EL608 '.                  
00111      12  XCTL-EL609          PIC X(8)    VALUE 'EL609 '.
00112      12  XCTL-EL610          PIC X(8)    VALUE 'EL610'.                   
00113      12  XCTL-EL612          PIC X(8)    VALUE 'EL612'.                   
00114      12  XCTL-EL613          PIC X(8)    VALUE 'EL613'.                   
00115      12  XCTL-EL626          PIC X(8)    VALUE 'EL626'.                   
00116      12  XCTL-EL650          PIC X(8)    VALUE 'EL650'.                   
00117      12  XCTL-EL651          PIC X(8)    VALUE 'EL651'.                   
00118      12  XCTL-EL652          PIC X(8)    VALUE 'EL652'.                   
00119      12  XCTL-EL653          PIC X(8)    VALUE 'EL653'.                   
00120      12  XCTL-EL654          PIC X(8)    VALUE 'EL654'.                   
00121      12  XCTL-EL656          PIC X(8)    VALUE 'EL656'.                   
00122      12  XCTL-EL658          PIC X(8)    VALUE 'EL658'.                   
00123      12  XCTL-EL659          PIC X(8)    VALUE 'EL659'.                   
00124      12  XCTL-EL1062         PIC X(8)    VALUE 'EL1062'.                  
00125      12  XCTL-EL1064         PIC X(8)    VALUE 'EL1064'.                  
00126                                                                           
00127      12  ER-0002             PIC X(4)    VALUE '0002'.                    
00128      12  ER-2370             PIC X(4)    VALUE '2370'.                    
00129      12  ER-2371             PIC X(4)    VALUE '2371'.                    
00130      12  ER-7000             PIC X(4)    VALUE '7000'.                    
00131      12  ER-7001             PIC X(4)    VALUE '7001'.                    
00132      12  ER-7002             PIC X(4)    VALUE '7002'.                    
00133      12  ER-7003             PIC X(4)    VALUE '7003'.                    
00134      12  ER-7008             PIC X(4)    VALUE '7008'.                    
00135      12  ER-7448             PIC X(4)    VALUE '7448'.                    
00136                                                                           
00137      EJECT                                                                
00138                              COPY ELCLOGOF.                               
00139                                                                           
00140      EJECT                                                                
00141                              COPY ELCDATE.                                
00142                                                                           
00143      EJECT                                                                
00144                              COPY ELCATTR.                                
00145                                                                           
00146      EJECT                                                                
00147                              COPY ELCEMIB.                                
00148                                                                           
00149      EJECT                                                                
00150                              COPY ELCINTF.                                
00151      12  PI-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.                     
00152          16  PI-FILE-ID                    PIC XX.                        
00153          16  FILLER                        PIC X(638).                    
00154                                                                           
00155      EJECT                                                                
00156                              COPY ELCAID.                                 
00157  01  FILLER    REDEFINES DFHAID.                                          
00158      12  FILLER              PIC X(8).                                    
00159      12  PF-VALUES           PIC X       OCCURS 2.                        
00160                                                                           
00161      EJECT                                                                
00162                              COPY EL605S.                                 
00163                                                                           
00164      EJECT                                                                
00165  LINKAGE SECTION.                                                         
00166  01  DFHCOMMAREA             PIC X(1024).                                 
00167                                                                           
00168      EJECT                                                                
00169  PROCEDURE DIVISION.                                                      
00170                                                                           
00171      MOVE DFHCOMMAREA   TO PROGRAM-INTERFACE-BLOCK.                       
00172                                                                           
00173      MOVE EIBDATE                TO DC-JULIAN-YYDDD.                      
00174      MOVE '5'                    TO DC-OPTION-CODE.                       
00175      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                       
00176      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                           
00177      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.                       
00178                                                                           
00179      IF EIBCALEN = 0                                                      
00180          GO TO 8800-UNAUTHORIZED-ACCESS.                                  
00181                                                                           
00182      IF PI-CALLING-PROGRAM NOT = PGM-EL605                                
00183          IF PI-RETURN-TO-PROGRAM NOT = PGM-EL605                          
00184              MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-6            
00185              MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-5            
00186              MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-4            
00187              MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-3            
00188              MOVE PI-SAVED-PROGRAM-1     TO PI-SAVED-PROGRAM-2            
00189              MOVE PI-RETURN-TO-PROGRAM   TO PI-SAVED-PROGRAM-1            
00190              MOVE PI-CALLING-PROGRAM     TO PI-RETURN-TO-PROGRAM          
00191              MOVE PGM-EL605              TO PI-CALLING-PROGRAM            
00192          ELSE                                                             
00193              MOVE PI-RETURN-TO-PROGRAM   TO PI-CALLING-PROGRAM            
00194              MOVE PI-SAVED-PROGRAM-1     TO PI-RETURN-TO-PROGRAM          
00195              MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-1            
00196              MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-2            
00197              MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-3            
00198              MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-4            
00199              MOVE PI-SAVED-PROGRAM-6     TO PI-SAVED-PROGRAM-5            
00200              MOVE SPACES                 TO PI-SAVED-PROGRAM-6.           
00201                                                                           
00202      EXEC CICS HANDLE CONDITION                                           
00203          PGMIDERR  (9600-PGMID-ERROR)                                     
00204          ERROR     (9990-ABEND)                                           
00205      END-EXEC.                                                            
00206                                                                           
00207      IF EIBTRNID  = TRANS-EXAA                                            
00208          GO TO 0100-SAME-TRAN.                                            
00209                                                                           
00210      GO TO 8100-SEND-INITIAL-MAP.                                         
00211                                                                           
00212  0100-SAME-TRAN.                                                          
00213      IF EIBAID = DFHCLEAR                                                 
00214          GO TO 9400-CLEAR.                                                
00215                                                                           
00216      EJECT                                                                
00217  0200-RECEIVE.                                                            
00218      MOVE LOW-VALUES   TO EL605AI.                                        
00219                                                                           
00220      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                               
00221          MOVE -1                 TO SELECTL                               
00222          MOVE ER-7008         TO EMI-ERROR                                
00223          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                         
00224          GO TO 8200-SEND-DATAONLY.                                        
00225                                                                           
00226      EXEC CICS RECEIVE                                                    
00227          MAP      (MAP-EL605A)                                            
00228          MAPSET   (MAPSET-EL605S)                                         
00229          INTO     (EL605AI)                                               
00230      END-EXEC.                                                            
00231                                                                           
00232      MOVE SPACES                 TO PI-ENTRY-CD-1.                        
00233                                                                           
00234      IF EIBAID = DFHPF12                                                  
00235         GO TO 9500-HELP                                                   
00236      ELSE                                                                 
00237         IF EIBAID = DFHPF23                                               
00238            GO TO 8810-PF23                                                
00239         ELSE                                                              
00240            IF EIBAID = DFHPF24                                            
00241               GO TO 9200-PF24.                                            
00242                                                                           
00243      IF PFKEYL = ZEROS                                                    
00244          NEXT SENTENCE                                                    
00245      ELSE                                                                 
00246          IF PFKEYI = '12'                                                 
00247             GO TO 9500-HELP                                               
00248          ELSE                                                             
00249             IF PFKEYI = '23'                                              
00250                GO TO 8810-PF23                                            
00251             ELSE                                                          
00252                IF PFKEYI = '24'                                           
00253                   GO TO 9200-PF24.                                        
00254                                                                           
00255      IF EIBAID NOT = DFHENTER                                             
00256          MOVE ER-0002         TO EMI-ERROR                                
00257          GO TO 0320-INPUT-ERROR.                                          
00258                                                                           
00259      IF SELECTL = 0                                                       
00260          MOVE ER-0002         TO EMI-ERROR                                
00261          GO TO 0320-INPUT-ERROR.                                          
00262                                                                           
00263  0310-CHECK-PFKEYS.                                                       
00264      IF SELECTI = '01'                                                    
00265          MOVE XCTL-EL606 TO PGM-NAME                                      
00266          GO TO 9300-XCTL.                                                 
00267                                                                           
00268      IF SELECTI = '02'                                                    
00269          MOVE XCTL-EL607 TO PGM-NAME                                      
00270          GO TO 9300-XCTL.                                                 
00271                                                                           
00272      IF SELECTI = '03'                                                    
00273          MOVE XCTL-EL608 TO PGM-NAME                                      
00274          GO TO 9300-XCTL.                                                 
00275                                                                           
00276      IF SELECTI = '04'                                                    
00277          MOVE XCTL-EL609 TO PGM-NAME                                      
00278          GO TO 9300-XCTL.                                                 
00279                                                                           
00280      IF SELECTI = '05'                                                    
00281          MOVE XCTL-EL612 TO PGM-NAME                                      
00282          GO TO 9300-XCTL.                                                 
00418                                                                           
00419      MOVE ER-7002                TO EMI-ERROR.                            
00420                                                                           
00421  0320-INPUT-ERROR.                                                        
00422      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                            
00423      MOVE AL-UNBON               TO SELECTA.                              
00424      MOVE -1                     TO SELECTL.                              
00425      GO TO 8200-SEND-DATAONLY.                                            
00426                                                                           
00427      EJECT                                                                
00428  8100-SEND-INITIAL-MAP.                                                   
00429      MOVE LOW-VALUES             TO EL605AO.                              
00430      MOVE EIBTIME                TO TIME-IN.                              
00431      MOVE TIME-OUT               TO TIMEO.                                
00432      MOVE SAVE-DATE              TO DATEO.                                
00460                                                                           
00461      MOVE -1                     TO SELECTL.                              
00462                                                                           
00463      EXEC CICS SEND                                                       
00464          MAP     (MAP-EL605A)                                             
00465          MAPSET  (MAPSET-EL605S)                                          
00466          FROM    (EL605AO)                                                
00467          ERASE                                                            
00468          CURSOR                                                           
00469      END-EXEC.                                                            
00470                                                                           
00471      GO TO 9100-RETURN-TRAN.                                              
00472                                                                           
00473  8200-SEND-DATAONLY.                                                      
00474      MOVE SAVE-DATE              TO DATEO.                                
00475      MOVE EIBTIME                TO TIME-IN.                              
00476      MOVE TIME-OUT               TO TIMEO.                                
00477      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                              
00505                                                                           
00506      EXEC CICS SEND                                                       
00507          MAP      (MAP-EL605A)                                            
00508          MAPSET   (MAPSET-EL605S)                                         
00509          FROM     (EL605AO)                                               
00510          DATAONLY                                                         
00511          CURSOR                                                           
00512      END-EXEC.                                                            
00513                                                                           
00514      GO TO 9100-RETURN-TRAN.                                              
00515                                                                           
00516  8300-SEND-TEXT.                                                          
00517      EXEC CICS SEND TEXT                                                  
00518          FROM     (LOGOFF-TEXT)                                           
00519          LENGTH   (LOGOFF-LENGTH)                                         
00520          ERASE                                                            
00521          FREEKB                                                           
00522      END-EXEC.                                                            
00523                                                                           
00524      EXEC CICS RETURN                                                     
00525      END-EXEC.                                                            
00526                                                                           
00527  8800-UNAUTHORIZED-ACCESS.                                                
00528      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                           
00529      GO TO 8300-SEND-TEXT.                                                
00530                                                                           
00531  8810-PF23.                                                               
00532      MOVE DFHPF23                TO PI-ENTRY-CD-1.                        
00533      MOVE XCTL-EL005             TO PGM-NAME.                             
00534      GO TO 9300-XCTL.                                                     
00535                                                                           
00536  9100-RETURN-TRAN.                                                        
00537      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.                     
00538      MOVE '605A'                 TO PI-CURRENT-SCREEN-NO.                 
00539                                                                           
00540      EXEC CICS RETURN                                                     
00541          TRANSID   (TRANS-EXAA)                                           
00542          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                              
00543          LENGTH    (PI-COMM-LENGTH)                                       
00544      END-EXEC.                                                            
00545                                                                           
00546  9200-PF24.                                                               
00547      MOVE XCTL-EL626             TO PGM-NAME.                             
00548      GO TO 9300-XCTL.                                                     
00549                                                                           
00550  9300-XCTL.                                                               
00551      MOVE SPACES                 TO PI-ENTRY-CD-2                         
00552                                     PI-RETURN-CODES                       
00553                                     PI-UPDATE-BY                          
00554                                     PI-PROGRAM-WORK-AREA.                 
00555      MOVE ZEROS                  TO PI-UPDATE-HHMMSS.                     
00556                                                                           
00557      EXEC CICS XCTL                                                       
00558          PROGRAM    (PGM-NAME)                                            
00559          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                             
00560          LENGTH     (PI-COMM-LENGTH)                                      
00561      END-EXEC.                                                            
00562                                                                           
00563  9400-CLEAR.                                                              
00564      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                             
00565      GO TO 9300-XCTL.                                                     
00566                                                                           
00567  9500-HELP.                                                               
00568      MOVE XCTL-EL010             TO PGM-NAME.                             
00569      GO TO 9300-XCTL.                                                     
00570                                                                           
00571  9600-PGMID-ERROR.                                                        
00572      MOVE PGM-NAME               TO PROGO.                                
00573      MOVE AL-UNBON               TO SELECTA.                              
00574      MOVE ER-7003                TO EMI-ERROR.                            
00575                                                                           
00576      EXEC CICS HANDLE CONDITION                                           
00577          PGMIDERR  (8300-SEND-TEXT)                                       
00578      END-EXEC.                                                            
00579                                                                           
00580      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.                   
00581      MOVE ' '                    TO PI-ENTRY-CD-1.                        
00582      MOVE XCTL-EL005             TO PGM-NAME.                             
00583      MOVE PGM-NAME               TO LOGOFF-PGM.                           
00584      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                          
00585      GO TO 9300-XCTL.                                                     
00586                                                                           
00587  9700-LINK-DATE-CONVERT.                                                  
00588      EXEC CICS LINK                                                       
00589          PROGRAM    (LINK-ELDATCV)                                        
00590          COMMAREA   (DATE-CONVERSION-DATA)                                
00591          LENGTH     (DC-COMM-LENGTH)                                      
00592      END-EXEC.                                                            
00593                                                                           
00594  9700-EXIT.                                                               
00595      EXIT.                                                                
00596                                                                           
00597  9900-ERROR-FORMAT.                                                       
00598      IF NOT EMI-ERRORS-COMPLETE                                           
00599          MOVE LINK-EL001         TO PGM-NAME                              
00600          EXEC CICS LINK                                                   
00601              PROGRAM    (PGM-NAME)                                        
00602              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)                   
00603              LENGTH     (EMI-COMM-LENGTH)                                 
00604          END-EXEC.                                                        
00605                                                                           
00606  9900-EXIT.                                                               
00607      EXIT.                                                                
00608                                                                           
00609  9990-ABEND.                                                              
00610      MOVE LINK-EL004             TO PGM-NAME.                             
00611      MOVE DFHEIBLK               TO EMI-LINE1.                            
00612                                                                           
00613      EXEC CICS LINK                                                       
00614          PROGRAM   (PGM-NAME)                                             
00615          COMMAREA  (EMI-LINE1)                                            
00616          LENGTH    (72)                                                   
00617      END-EXEC.                                                            
00618                                                                           
00619      GO TO 8200-SEND-DATAONLY.                                            
00620                                                                           
