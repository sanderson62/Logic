00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL688 .                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 03/07/95 12:54:34.                 
00007 *                            VMOD=2.022.                          
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
00024 *REMARKS.  TRANSACTION - EXG4                                     
00025                                                                   
00026 *        THIS FUNCTION READS THE CHECK QUEUE FILE AND PRINTS      
00027 *    CHECKS FROM THOSE ENTRIES QUEUED.  QUEUEING IS DONE USING    
00028 *    TIME (HH.MM) BY THE CHECK PRINT RELEASE PROGRAM (EL687).     
00029                                                                   
00030 *    SCREENS     - NONE - USERS PRINTED OUTPUT (CHECKS)           
00031                                                                   
00032 *    ENTERED BY  - EL687  - CHECK WRITER - VIA START              
00033                                                                   
00034 *    EXIT TO     - CICS                                           
00035                                                                   
00036 *    INPUT FILES - NONE                                           
00037                                                                   
00038 *    OUTPUT FILES - NONE                                          
00039                                                                   
00040 *    COMMAREA    - PASSED.                                        
00041                                                                   
00042      EJECT                                                        
00043  ENVIRONMENT DIVISION.                                            
00044                                                                   
00045  DATA DIVISION.                                                   
00046                                                                   
00047  WORKING-STORAGE SECTION.                                         
00048                                                                   
00049  77  FILLER  PIC X(32)  VALUE '********************************'. 
00050  77  FILLER  PIC X(32)  VALUE '*    EL688 WORKING STORAGE     *'. 
00051  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.022 *********'. 
00052                                                                   
00053  01  FILLER                          COMP-3.                      
00054      05  WS-RECORD-COUNT             PIC S9(5)       VALUE ZERO.  
00055      05  WS-TIME-WORK                PIC S9(7)       VALUE ZERO.  
00056      05  WS-TIME                     REDEFINES                    
00057          WS-TIME-WORK                PIC S9(3)V9(4).              
00058      05  WS-HHMM                     REDEFINES                    
00059          WS-TIME-WORK                PIC S9(5)V99.                
00060                                                                   
00061      05  WS-DELAY-INTERVAL           PIC S9(7)       VALUE +10.   
00062                                                                   
00063      05  WS-DATA-SENT-SW             PIC S9          VALUE ZERO.  
00064      05  WS-SW                       PIC S9          VALUE ZERO.  
00065                                                                   
00066      05  WS-AMT-WORK                 PIC S9(7)V99    VALUE ZERO.  
00067  01  FILLER                          COMP                         
00068                                      SYNCHRONIZED.                
00069                                                                   
00070      05  WS-TS-LENGTH                PIC S9(4)       VALUE +1000. 
00071      05  WS-COMM-LENGTH              PIC S9(4)       VALUE +1024. 
00072      05  WS-CHECK-LINES-LENGTH       PIC S9(4)       VALUE +904.  
00073                                                                   
00074  01  FILLER.                                                      
00075      05  THIS-PGM                    PIC X(8)      VALUE 'EL688'. 
00076                                                                   
00077      05  ERMAIL-FILE-ID           PIC X(8)      VALUE 'ERMAIL'.   
00078                                                                   
00079      05  ERMAIL-KEY.                                              
00080          10  ERMAIL-COMPANY-CD    PIC X         VALUE SPACES.     
00081          10  ERMAIL-CARRIER       PIC X         VALUE SPACES.     
00082          10  ERMAIL-GROUPING      PIC X(6)      VALUE SPACES.     
00083          10  ERMAIL-STATE         PIC XX        VALUE SPACES.     
00084          10  ERMAIL-ACCOUNT       PIC X(10)     VALUE SPACES.     
00085          10  ERMAIL-CERT-EFF-DT   PIC XX        VALUE SPACES.     
00086          10  ERMAIL-CERT-NO       PIC X(11)     VALUE SPACES.     
00087                                                                   
00088      05  FIRST-TIME-SW            PIC X              VALUE 'Y'.   
00089          88  FIRST-ALIGNMENT                         VALUE 'Y'.   
00090                                                                   
00091      05  FIRST-CLIENT-SW          PIC X              VALUE 'Y'.   
00092          88  FIRST-HERITAGE-CHECK                    VALUE 'Y'.   
00093          88  FIRST-MADISON-CHECK                     VALUE 'Y'.   
00094          88  FIRST-LAP-CHECK                         VALUE 'Y'.   
00095                                                                   
00096      05  WS-TEXT-MESSAGE-LENGTH      PIC S9(4)       VALUE +70    
00097                                      COMP                         
00098                                      SYNCHRONIZED.                
00099                                                                   
00100      05  WS-TEXT-MESSAGE             PIC X(70)       VALUE SPACES.
00101                                                                   
00102      05  WS-ZIP-CODE-LINE                            VALUE SPACES.
00103          10  WS-ZIP-CHAR             PIC X                        
00104              OCCURS 133 TIMES        INDEXED BY ZIP-INDEX1        
00105                                                 ZIP-INDEX2        
00106                                                 ZIP-INDEX3.       
00107                                                                   
00108      05  WS-DUMP-CODE.                                            
00109          10  FILLER                  PIC X           VALUE 'S'.   
00110          10  WS-DUMP-COUNT           PIC 999         VALUE ZERO.  
00111                                                                   
00112      05  WS-STUB4.                                                
00113          10  WS-STUB4-1-20           PIC X(20)       VALUE SPACES.
00114          10  WS-STUB4-21-50          PIC X(30)       VALUE SPACES.
00115                                                                   
00116      05  WS-CHECK-DATE.                                           
00117          10  WS-CD-MO            PIC  9(02).                      
00118          10  WS-CD-DA            PIC  9(02).                      
00119          10  WS-CD-YR            PIC  9(02).                      
00120                                                                   
00121      05  WS-ZIP-WORK.                                             
00122          10  WS-ZIP-CODE         PIC  X(05).                      
00123          10  WS-ZIP-DASH         PIC  X(01).                      
00124          10  WS-ZIP-PLUS4        PIC  X(04).                      
00125                                                                   
00126      05  WS-MONTH-TABLE.                                          
00127          10  WS-MONTH-01         PIC  X(09)  VALUE '  JANUARY'.   
00128          10  WS-MONTH-02         PIC  X(09)  VALUE ' FEBRUARY'.   
00129          10  WS-MONTH-03         PIC  X(09)  VALUE '    MARCH'.   
00130          10  WS-MONTH-04         PIC  X(09)  VALUE '    APRIL'.   
00131          10  WS-MONTH-05         PIC  X(09)  VALUE '      MAY'.   
00132          10  WS-MONTH-06         PIC  X(09)  VALUE '     JUNE'.   
00133          10  WS-MONTH-07         PIC  X(09)  VALUE '     JULY'.   
00134          10  WS-MONTH-08         PIC  X(09)  VALUE '   AUGUST'.   
00135          10  WS-MONTH-09         PIC  X(09)  VALUE 'SEPTEMBER'.   
00136          10  WS-MONTH-10         PIC  X(09)  VALUE '  OCTOBER'.   
00137          10  WS-MONTH-11         PIC  X(09)  VALUE ' NOVEMBER'.   
00138          10  WS-MONTH-12         PIC  X(09)  VALUE ' DECEMBER'.   
00139      05  WS-MONT-TAB  REDEFINES  WS-MONTH-TABLE.                  
00140          10  WS-MONTH-ENTRY  OCCURS  12  TIMES                    
00141                                  PIC  X(09).                      
00142                              COPY ELCDMD34.                       
00143      EJECT                                                        
00144                              COPY ELCINTF.                        
00145                                                                   
00146                              COPY ERC687PI.                       
00147                                                                   
00148      EJECT                                                        
00149                              COPY ELC176W2.                       
00150                                                                   
00151      EJECT                                                        
00152                              COPY ERC688W1.                       
00153                                                                   
00154      EJECT                                                        
00155                              COPY ERC688CR.                       
00156                                                                   
00157      EJECT                                                        
00186                                                                   
00187      EJECT                                                        
00188  01  CHECK-PRINT-LINES-SAVE-AREA     PIC X(1500) VALUE SPACES.    
00189                                                                   
00190      EJECT                                                        
00191                              COPY ELCDATE.                        
00192                                                                   
00193                              COPY ELPRTCVD.                       
00194                                                                   
00195  LINKAGE SECTION.                                                 
00196                                                                   
00197  01  DFHCOMMAREA                    PIC X(1024).                  
00198                                                                   
00199      EJECT                                                        
00200                              COPY ERCCPA.                         
00201                                                                   
00202      EJECT                                                        
00203                              COPY ERCMAIL.                        
00204                                                                   
00205      EJECT                                                        
00206  PROCEDURE DIVISION.                                              
00207                                                                   
00208      EXEC CICS HANDLE CONDITION                                   
00209          QIDERR  (5000-MAIN-LOGIC)                                
00210          ITEMERR (5000-MAIN-LOGIC)                                
00211          ERROR   (9990-ERROR) END-EXEC.                           
00212                                                                   
00213      MOVE +132                   TO  WS-LINE-LEN.                 
00214      MOVE SPACES                 TO DL34-PROCESS-TYPE.            
00215                                                                   
00216      EJECT                                                        
00217  0010-MAIN-LOGIC.                                                 
00218      EXEC CICS RETRIEVE                                           
00219          INTO   (PROGRAM-INTERFACE-BLOCK)                         
00220          LENGTH (PI-COMM-LENGTH) END-EXEC                         
00221                                                                   
00222      MOVE +1                     TO  PI-TEMP-STORAGE-ITEM.        
00223                                                                   
00224 * DLO034 OPEN WHEN DMD OR CID                                     
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
00226          IF DL34-PROCESS-TYPE IS EQUAL TO SPACES                  
00227              MOVE 'O'                TO DL34-PROCESS-TYPE         
00228              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID           
00229              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID     
00230              MOVE PI-PROCESSOR-ID    TO DL34-USERID               
00231              MOVE SPACES             TO DL34-PRINT-LINE           
00232              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID  
00233                                                                   
00234              EXEC CICS LINK                                       
00235                  PROGRAM    ('DLO034')                            
00236                  COMMAREA   (DLO034-COMMUNICATION-AREA)           
00237                  LENGTH     (DLO034-REC-LENGTH)                   
00238              END-EXEC                                             
00239                                                                   
00240              IF DL34-RETURN-CODE NOT = 'OK'                       
00241                  MOVE  '**DLO034 OPEN ERROR - ABORT**'            
00242                                      TO WS-TEXT-MESSAGE           
00243                  PERFORM 0120-SEND-TEXT                           
00244                  EXEC CICS RETURN                                 
00245                  END-EXEC.                                        
00246                                                                   
00247  0100-MAIN-LOGIC.                                                 
00248      EXEC CICS READQ TS                                           
00249          QUEUE  (PI-TEMP-STORAGE-KEY)                             
00250          ITEM   (PI-TEMP-STORAGE-ITEM)                            
00251          LENGTH (WS-TS-LENGTH)                                    
00252          SET    (ADDRESS OF CHECK-PASS-AREA) END-EXEC             
00253                                                                   
00254      IF WS-TS-LENGTH NOT GREATER THAN +1                          
00255          MOVE ZERO               TO  WS-PROG-END                  
00256          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                      
00257          PERFORM 0110-CLOSE-DLO034                                
00258          EXEC CICS DELETEQ TS                                     
00259              QUEUE (PI-TEMP-STORAGE-KEY) END-EXEC                 
00260          EXEC CICS RETURN                                         
00261              END-EXEC.                                            
00262                                                                   
00284                                                                   
00285      IF CPA-ALIGNMENT = ZERO                                      
00286          GO TO 0200-MAIN-LOGIC.                                   
00287                                                                   
00288      IF FIRST-ALIGNMENT                                           
00289          MOVE 'N'                TO  FIRST-TIME-SW                
00290          MOVE +999999.99         TO  CPL11-CHECK-AMOUNT           
00291                                      SD-PASS-AMOUNT               
00292                                      CPL25-NET-PAYOFF             
00293          PERFORM SPELL-DOLLAR                                     
00294          MOVE SD-PSA-LINE1       TO  CPL11-PAY-LINE-1             
00295          MOVE SD-PSA-LINE2       TO  CPL12-PAY-LINE-2.            
00296                                                                   
00297      IF PI-ASSIGN-CHECK-NUMBERS = 'N'                             
00298          MOVE CPA-CHECK-NUMBER   TO  CPL6-CHECK-NUMBER            
00299        ELSE                                                       
00300          MOVE ALL 'X'            TO  CPL6-CHECK-NUMBER.           
00301                                                                   
00302                                                                   
00303      GO TO 0300-MAIN-LOGIC.                                       
00304  0110-CLOSE-DLO034.                                               
00305                                                                   
00306 * DLO034 CLOSE                                                    
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
00308          MOVE 'C'                TO DL34-PROCESS-TYPE             
00309          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID               
00310          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID         
00311          MOVE PI-PROCESSOR-ID    TO DL34-USERID                   
00312          MOVE SPACES             TO DL34-PRINT-LINE               
00313                                     DL34-OVERRIDE-PRINTER-ID      
00314          EXEC CICS LINK                                           
00315              PROGRAM    ('DLO034')                                
00316              COMMAREA   (DLO034-COMMUNICATION-AREA)               
00317              LENGTH     (DLO034-REC-LENGTH)                       
00318          END-EXEC                                                 
00319                                                                   
00320          IF DL34-RETURN-CODE NOT = 'OK'                           
00321              MOVE  '**DLO034 CLOSE ERROR - ABORT**'               
00322                                  TO WS-TEXT-MESSAGE               
00323              PERFORM  0120-SEND-TEXT                              
00324              EXEC CICS RETURN                                     
00325              END-EXEC.                                            
00326                                                                   
00327      EJECT                                                        
00328                                                                   
00329  0120-SEND-TEXT.                                                  
00330      EXEC CICS SEND TEXT                                          
00331           FROM    (WS-TEXT-MESSAGE)                               
00332           LENGTH  (70)                                            
00333           END-EXEC.                                               
00334                                                                   
00335      EJECT                                                        
00336  0200-MAIN-LOGIC.                                                 
00337      IF CPA-ALIGNMENT NOT = ZERO                                  
00338         MOVE SPACES              TO  CPL5-COMPANY-NAME            
00339                                      CPL6-COMPANY-ADDRESS1        
00340                                      CPL7-COMPANY-ADDRESS2        
00341                                      CPL8-COMPANY-ADDRESS3        
00342                                      CPL9-COMPANY-CITY-ST         
00343                                      CPL9-COMPANY-ZIP-CODE-X      
00344         MOVE CPA-CHECK-NUMBER    TO  CPL6-CHECK-NUMBER            
00345         MOVE +9999999.99         TO  CPL11-CHECK-AMOUNT           
00346                                      CPL25-NET-PAYOFF             
00347                                      SD-PASS-AMOUNT               
00348         MOVE +48                 TO  WS-1ST-LINE-LENGTH           
00349                                      WS-2ND-LINE-LENGTH           
00350         PERFORM SPELL-DOLLAR                                      
00351         MOVE SD-PSA-LINE1        TO  CPL11-PAY-LINE-1             
00352         MOVE SD-PSA-LINE2        TO  CPL12-PAY-LINE-2             
00353         GO TO 0300-MAIN-LOGIC.                                    
00354                                                                   
00355      IF FIRST-ALIGNMENT                                           
00356          MOVE 'N'                TO  FIRST-TIME-SW                
00357      ELSE                                                         
00358          GO TO 0210-MAIN-LOGIC.                                   
00359                                                                   
00360      MOVE SPACES                 TO  CPL5-COMPANY-NAME            
00361                                      CPL6-COMPANY-ADDRESS1        
00362                                      CPL7-COMPANY-ADDRESS2        
00363                                      CPL8-COMPANY-ADDRESS3        
00364                                      CPL9-COMPANY-CITY-ST         
00365                                      CPL9-COMPANY-ZIP-CODE-X.     
00366                                                                   
00367      IF CPL8-COMPANY-ADDRESS3 = SPACES                            
00368          MOVE CPL9-COMPANY-CITY-ST  TO  CPL8-COMPANY-ADDRESS3     
00369          MOVE SPACES                TO  CPL9-COMPANY-CITY-ST.     
00370                                                                   
00371      IF CPL7-COMPANY-ADDRESS2 = SPACES                            
00372          MOVE CPL8-COMPANY-ADDRESS3 TO  CPL7-COMPANY-ADDRESS2     
00373          MOVE CPL9-COMPANY-CITY-ST  TO  CPL8-COMPANY-ADDRESS3     
00374          MOVE SPACES                TO  CPL9-COMPANY-CITY-ST.     
00375                                                                   
00376      IF CPL6-COMPANY-ADDRESS1 = SPACES                            
00377          MOVE CPL7-COMPANY-ADDRESS2 TO  CPL6-COMPANY-ADDRESS1     
00378          MOVE CPL8-COMPANY-ADDRESS3 TO  CPL7-COMPANY-ADDRESS2     
00379          MOVE CPL9-COMPANY-CITY-ST  TO  CPL8-COMPANY-ADDRESS3     
00380          MOVE SPACES                TO  CPL9-COMPANY-CITY-ST.     
00381                                                                   
00382      MOVE CHECK-PRINT-LINES      TO  CHECK-PRINT-LINES-SAVE-AREA. 
00383                                                                   
00384  0210-MAIN-LOGIC.                                                 
00385      MOVE CHECK-PRINT-LINES-SAVE-AREA  TO  CHECK-PRINT-LINES.     
00386                                                                   
00387      MOVE CPA-CHECK-NUMBER        TO  CPL6-CHECK-NUMBER           
00388                                       CPL25-CHECK-NO.             
00389                                                                   
00390      IF CPA-CHECK-DATE NOT = LOW-VALUES                           
00391          MOVE CPA-CHECK-DATE      TO  DC-BIN-DATE-1               
00392          MOVE SPACES              TO  DC-OPTION-CODE              
00393          PERFORM 8500-DATE-CONVERSION                             
00394          IF DC-ERROR-CODE = SPACES                                
00395          MOVE DC-GREG-DATE-1-ALPHA  TO  CPL7-CHECK-DATE.          
00396                                                                   
00397      MOVE CPA-AMOUNT-PAID         TO  SD-PASS-AMOUNT              
00398                                       CPL11-CHECK-AMOUNT          
00399                                       CPL25-NET-PAYOFF.           
00400      PERFORM SPELL-DOLLAR.                                        
00401      MOVE SD-PSA-LINE1            TO  CPL11-PAY-LINE-1.           
00402      MOVE SD-PSA-LINE2            TO  CPL12-PAY-LINE-2.           
00403                                                                   
00404      MOVE CPA-PAYEE-NAME          TO  CPL14-PAYEE-NAME.           
00405      MOVE CPA-PAYEE-IN-CARE-OF    TO  CPL15-PAYEE-ADDRESS1.       
00406      MOVE CPA-PAYEE-ADDRESS-LINE1 TO  CPL16-PAYEE-ADDRESS2.       
00407      MOVE CPA-PAYEE-ADDRESS-LINE2 TO  CPL17-PAYEE-ADDRESS3.       
00408      MOVE CPA-PAYEE-CITY-ST       TO  CPL18-PAYEE-CITY-ST.        
00409      MOVE CPA-PAYEE-ZIP-CODE      TO  CPL18-PAYEE-ZIP-CODE-X.     
00410                                                                   
00411      MOVE CHECK-PRINT-LINE-18     TO  WS-ZIP-CODE-LINE.           
00412      SET ZIP-INDEX1 TO +40.                                       
00413      SET ZIP-INDEX2 TO +42.                                       
00414      PERFORM 5100-MOVE-ZIP-CODE.                                  
00415      MOVE WS-ZIP-CODE-LINE        TO  CHECK-PRINT-LINE-18.        
00416                                                                   
00417      IF CPL17-PAYEE-ADDRESS3 = SPACES                             
00418          MOVE CHECK-PRINT-LINE-18  TO  CHECK-PRINT-LINE-17        
00419          MOVE SPACES              TO  CHECK-PRINT-LINE-18.        
00420                                                                   
00421      IF CPL16-PAYEE-ADDRESS2 = SPACES                             
00422          MOVE CHECK-PRINT-LINE-17  TO  CHECK-PRINT-LINE-16        
00423          MOVE CHECK-PRINT-LINE-18  TO  CHECK-PRINT-LINE-17        
00424          MOVE SPACES             TO  CHECK-PRINT-LINE-18.         
00425                                                                   
00426      IF CPL15-PAYEE-ADDRESS1 = SPACES                             
00427          MOVE CHECK-PRINT-LINE-16  TO  CHECK-PRINT-LINE-15        
00428          MOVE CHECK-PRINT-LINE-17  TO  CHECK-PRINT-LINE-16        
00429          MOVE CHECK-PRINT-LINE-18  TO  CHECK-PRINT-LINE-17        
00430          MOVE SPACES             TO  CHECK-PRINT-LINE-18.         
00431                                                                   
00432      IF CPA-CERT-NO = SPACES                                      
00433          MOVE SPACES             TO  CPL25-CERT-NO-DESC.          
00434                                                                   
00435      MOVE PI-COMPANY-ID          TO  CPL23-COMPANY-ID.            
00436      MOVE CPA-GROUPING           TO  CPL23-GROUP.                 
00437      MOVE CPA-ACCOUNT            TO  CPL23-ACCOUNT.               
00438      MOVE CPA-CERT-NO            TO  CPL25-CERT-NO.               
00439      MOVE CPA-GENERAL-LEDGER     TO  CPL26-GEN-LEDGER.            
00440      MOVE ZEROS                  TO  CPL23-DEPT-NO.               
00441      MOVE SPACES                 TO  CPL26-COMMENTS.              
00442                                                                   
00443      IF CPA-FIN-RESP = SPACES                                     
00444          MOVE '    STATE -'      TO  CPL23-DESC                   
00445          MOVE CPA-STATE          TO  CPL23-FIN-RESP               
00446      ELSE                                                         
00447          MOVE '  FIN-RESP-'      TO  CPL23-DESC                   
00448          MOVE CPA-FIN-RESP       TO  CPL23-FIN-RESP.              
00449                                                                   
00450      MOVE CPA-STUB1              TO  CPL27-STUB.                  
00451      MOVE CPA-STUB2              TO  CPL28-STUB.                  
00452      MOVE CPA-STUB3              TO  CPL29-STUB.                  
00453      MOVE CPA-STUB4              TO  CPL30-STUB.                  
00454      MOVE CPA-STUB5              TO  CPL31-STUB.                  
00455                                                                   
00456  0300-MAIN-LOGIC.                                                 
00457      MOVE CHECK-PRINT-LINE-1     TO  WS-PRINT-AREA.               
00458      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00459                                                                   
00460      MOVE CHECK-PRINT-LINE-3     TO  WS-PRINT-AREA.               
00461      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00462                                                                   
00463      MOVE CHECK-PRINT-LINE-5     TO  WS-PRINT-AREA.               
00464      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00465                                                                   
00466      MOVE CHECK-PRINT-LINE-6     TO  WS-PRINT-AREA.               
00467      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00468                                                                   
00469      MOVE CHECK-PRINT-LINE-7     TO  WS-PRINT-AREA.               
00470      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00471                                                                   
00472      MOVE CHECK-PRINT-LINE-8     TO  WS-PRINT-AREA.               
00473      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00474                                                                   
00475      MOVE CHECK-PRINT-LINE-9     TO  WS-PRINT-AREA.               
00476      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00477                                                                   
00478      MOVE CHECK-PRINT-LINE-11    TO  WS-PRINT-AREA.               
00479      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00480                                                                   
00481      MOVE CHECK-PRINT-LINE-12    TO  WS-PRINT-AREA.               
00482      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00483                                                                   
00484      MOVE CHECK-PRINT-LINE-14    TO  WS-PRINT-AREA.               
00485      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00486                                                                   
00487      MOVE CHECK-PRINT-LINE-15    TO  WS-PRINT-AREA.               
00488      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00489                                                                   
00490      MOVE CHECK-PRINT-LINE-16    TO  WS-PRINT-AREA.               
00491      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00492                                                                   
00493      MOVE CHECK-PRINT-LINE-17    TO  WS-PRINT-AREA.               
00494      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00495                                                                   
00496      MOVE CHECK-PRINT-LINE-18    TO  WS-PRINT-AREA.               
00497      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00498                                                                   
00499      MOVE CHECK-PRINT-LINE-21    TO  WS-PRINT-AREA.               
00500      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00501                                                                   
00502      MOVE CHECK-PRINT-LINE-22    TO  WS-PRINT-AREA.               
00503      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00504                                                                   
00505      MOVE CHECK-PRINT-LINE-23    TO  WS-PRINT-AREA.               
00506      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00507                                                                   
00508      MOVE CHECK-PRINT-LINE-25    TO  WS-PRINT-AREA.               
00509      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00510                                                                   
00511      MOVE CHECK-PRINT-LINE-26    TO  WS-PRINT-AREA.               
00512      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00513                                                                   
00514      MOVE CHECK-PRINT-LINE-27    TO  WS-PRINT-AREA.               
00515      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00516                                                                   
00517      MOVE CHECK-PRINT-LINE-28    TO  WS-PRINT-AREA.               
00518      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00519                                                                   
00520      MOVE CHECK-PRINT-LINE-29    TO  WS-PRINT-AREA.               
00521      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00522                                                                   
00523      MOVE CHECK-PRINT-LINE-30    TO  WS-PRINT-AREA.               
00524      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00525                                                                   
00526      MOVE CHECK-PRINT-LINE-31    TO  WS-PRINT-AREA.               
00527      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         
00528                                                                   
00529      MOVE +1                     TO  WS-DATA-SENT-SW.             
00530                                                                   
00531      ADD +1  TO  PI-TEMP-STORAGE-ITEM.                            
00532                                                                   
00533      GO TO 0100-MAIN-LOGIC.                                       
00534                                                                   
00535      EJECT                                                        
00536                                                                   
02430                                                                   
02431      EJECT                                                        
02432  5000-MAIN-LOGIC  SECTION.                                        
02433      IF WS-DATA-SENT-SW NOT = ZERO                                
02434          MOVE ZERO               TO  WS-PROG-END                  
02435                                      WS-DATA-SENT-SW              
02436          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                     
02437                                                                   
02438      EXEC CICS DELAY                                              
02439          INTERVAL (WS-DELAY-INTERVAL) END-EXEC.                   
02440                                                                   
02441      GO TO 0100-MAIN-LOGIC.                                       
02442                                                                   
02443  5000-EXIT.                                                       
02444       EXIT.                                                       
02445      EJECT                                                        
02446  5100-MOVE-ZIP-CODE SECTION.                                      
02447                                                                   
02448      SET ZIP-INDEX3 TO ZIP-INDEX2.                                
02449      SET ZIP-INDEX3 UP BY +10.                                    
02450                                                                   
02451  5110-MOVE-ZIP-CODE.                                              
02452      IF WS-ZIP-CHAR (ZIP-INDEX1) NOT = SPACES                     
02453          SET ZIP-INDEX1 UP BY +2                                  
02454          GO TO 5120-MOVE-ZIP-CODE.                                
02455                                                                   
02456      IF ZIP-INDEX1 GREATER THAN +1                                
02457          SET ZIP-INDEX1 DOWN BY +1                                
02458          GO TO 5110-MOVE-ZIP-CODE.                                
02459                                                                   
02460      GO TO 5190-MOVE-ZIP-CODE-EXIT.                               
02461                                                                   
02462  5120-MOVE-ZIP-CODE.                                              
02463      MOVE WS-ZIP-CHAR (ZIP-INDEX2) TO WS-ZIP-CHAR (ZIP-INDEX1).   
02464      MOVE SPACES                   TO WS-ZIP-CHAR (ZIP-INDEX2).   
02465                                                                   
02466      IF ZIP-INDEX2 LESS THAN ZIP-INDEX3                           
02467          SET ZIP-INDEX1                                           
02468              ZIP-INDEX2 UP BY +1                                  
02469          GO TO 5120-MOVE-ZIP-CODE.                                
02470                                                                   
02471  5190-MOVE-ZIP-CODE-EXIT.                                         
02472      EXIT.                                                        
02473                                                                   
02474      EJECT                                                        
02475  6000-PRINT-CHECK SECTION.                                        
02476                                                                   
CIDMOD                          COPY ELPRTCVP.
CIDMOD
02477 ******************************************************************
02478 ***                 -ELPRTCVP-                                   *
02479 ***     COPY MEMBER FOR TERMINAL ONLINE PRINT ROUTINE.           *
02480 ***     THIS ROUTINE WILL ACCOMODATE PRINTING TO A 3270          *
02481 ***     TERMINAL PRINTER. A BUFFER OF UP TO 1920 CHARACTERS      *
02482 ***     IS ACCUMULATED AND PRINTED COLLECTIVELY.                 *
02483 ***                                                              *
02484 ***     THIS ROUTINE TO BE USED ONLY WITH ACCOMPANIMENT          *
02485 ***      OF THE WORKING-STORAGE COPY MEMBER ( ELPRTCVD )         *
02486 ***     THE HOST PROGRAM MUST INITIALIZE THE FOLLOWING 3 FIELDS  *
02487 ***      FROM THE ABOVE COPY MEMBER FOR THIS PROCEDURE TO BE     *
02488 ***      SUCCESSFUL.                                             *
02489 ***      05  WS-LINE-LEN    PIC  S9(4)  COMP  VALUE +80.         *
02490 ***                         LENGTH OF THE LINE TO BE PRINTED     *
02491 ***                         DEFAULT IS 80, YOU CAN USE ANY NUMBER*
02492 ***                         UP TO 132.  THIS FIELD IS ONLY ACCEP-*
02493 ***                         TED THE FIRST TIME THRU THE ROUTINE. *
02494 ***      05  WS-PROG-END    PIC  X  VALUE SPACES.                *
02495 ***                         PROGRAM END SWITCH. INITIALIZED      *
02496 ***                         TO SPACE-     MOVE IN ANY NONBLANK   *
02497 ***                         TO IT WHEN PROGRAM IS FINISHED.      *
02498 ***      05  WS-PRINT-AREA.                                      *
02499 ***          10  WS-PASSED-CNTL-CHAR     PIC X.                  *
02500 ***          10  WS-PASSED-DATA          PIC X(132).             *
02501 ***                         USE THE DATA TO BE PRINTED IN THE    *
02502 ***                         WS-PASSED-DATA.                      *
02503 ***                         USE THE STANDARD CARRIAGE CONTROL    *
02504 ***                         CHARACTER IN THE WS-PASSED-CNTL-CHAR *
02505 ***                           SINGLE-SPACE            VALUE ' '  *
02506 ***                           DOUBLE-SPACE            VALUE '0'  *
02507 ***                           TRIPLE-SPACE            VALUE '-'  *
02508 ***                           TOP-PAGE                VALUE '1'  *
02509 ***      NOTE: A LINE COUNT IS PROVIDED IN FIELDNAME -WS-LINE-CNT*
02510 ***            THE USE OF THIS FIELD IS OPTIONAL.                *
02511 ***            THIS ROUTINE WILL ONLY ADD 1, 2, OR 3             *
02512 ***            TO THIS COUNT DEPENDING ON THE WS-PASSED-CNTL-CHAR*
02513 ***            AND RESET THE COUNT TO ZERO WHEN TOP-PAGE         *
02514 ***            CONDITION.                                        *
02515 ***                                                              *
02516 ******************************************************************
02517 *                                                                 
02518 *ELPRTCVP.                                                        
02519 *                                                                 
02520 *    IF PI-COMPANY-ID IS EQUAL TO 'DMD' OR 'CID'                  
02521 *        MOVE 'P'                TO DL34-PROCESS-TYPE             
02522 *        MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID         
02523 *        MOVE PI-PROCESSOR-ID    TO DL34-USERID                   
02524 *        MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID               
02525 *        MOVE WS-PRINT-AREA      TO DL34-PRINT-LINE               
02526 *        MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID      
02527 *                                                                 
02528 *        EXEC CICS LINK                                           
02529 *            PROGRAM    ('DLO034')                                
02530 *            COMMAREA   (DLO034-COMMUNICATION-AREA)               
02531 *            LENGTH     (DLO034-REC-LENGTH)                       
02532 *        END-EXEC                                                 
02533 *                                                                 
02534 *           IF DL34-RETURN-CODE = 'OK'                            
02535 *               GO TO ELPRTCVP-EXIT                               
02536 *           ELSE                                                  
02537 *               MOVE '8339'     TO EMI-ERROR ?????ERROR MESSAGE???
02538 *               GO TO ELPRTCVP-EXIT.                              
02539 *                                                                 
02540 *    IF NOT FIRST-TIME                                            
02541 *        GO TO ELPRTCVP-020.                                      
02542 *                                                                 
02543 *    IF WS-LINE-LEN NOT GREATER THAN ZERO                         
02544 *        GO TO ELPRTCVP-EXIT.                                     
02545 *                                                                 
02546 *    MOVE '2'           TO WS-FIRST-TIME-SW.                      
02547 *    MOVE LOW-VALUES    TO WS-BUFFER-AREA.                        
02548 *                                                                 
02549 *    SET BUFFER-INDEX TO +1.                                      
02550 *                                                                 
02551 *    IF PI-COMPANY-ID EQUAL 'HER' OR 'TIH' OR 'LAP' OR 'RMC' OR   
02552 *                           'CVL' OR 'HAN' OR 'NSL' OR 'CNL'      
02553 *        NEXT SENTENCE                                            
02554 *    ELSE                                                         
02555 *       IF NOT TOP-PAGE                                           
02556 *           MOVE T-TP       TO WS-BUFFER-BYTE (BUFFER-INDEX)      
02557 *           SET BUFFER-INDEX UP BY +1.                            
02558 *                                                                 
02559 *ELPRTCVP-020.                                                    
02560 *    IF WS-PROG-END = SPACES                                      
02561 *        GO TO ELPRTCVP-030.                                      
02562 *                                                                 
02563 *    MOVE '1'               TO WS-FIRST-TIME-SW.                  
02564 *    MOVE SPACES            TO WS-PROG-END.                       
02565 *                                                                 
02566 *    IF BUFFER-INDEX GREATER THAN +1                              
02567 *        PERFORM ELPRTCVP-PRINT-BUFFER THRU ELPRTCVP-PRINT-EXIT.  
02568 *                                                                 
02569 *    GO TO ELPRTCVP-EXIT.                                         
02570 *                                                                 
02571 *ELPRTCVP-030.                                                    
02572 *    IF WS-PASSED-DATA = SPACES                                   
02573 *        SET PRT-INDEX TO +1                                      
02574 *        GO TO ELPRTCVP-050.                                      
02575 *                                                                 
02576 *    SET PRT-INDEX TO WS-LINE-LEN.                                
02577 *                                                                 
02578 *ELPRTCVP-040.                                                    
02579 *    IF WS-PRINT-BYTE (PRT-INDEX) NOT = SPACES                    
02580 *        GO TO ELPRTCVP-050.                                      
02581 *                                                                 
02582 *    IF PRT-INDEX GREATER THAN +1                                 
02583 *        SET PRT-INDEX DOWN BY +1                                 
02584 *        GO TO ELPRTCVP-040.                                      
02585 *                                                                 
02586 *ELPRTCVP-050.                                                    
02587 *    SET WS-LINE-LENGTH TO PRT-INDEX.                             
02588 *    SET BUFFER-INDEX2 TO BUFFER-INDEX.                           
02589 *    SET BUFFER-INDEX2 UP BY WS-LINE-LENGTH.                      
02590 *                                                                 
02591 *    IF BUFFER-INDEX2 IS NOT LESS THAN WS-BUFFER-SIZE             
02592 *        PERFORM ELPRTCVP-PRINT-BUFFER THRU ELPRTCVP-PRINT-EXIT.  
02593 *                                                                 
02594 *    IF TRIPLE-SPACE                                              
02595 *         ADD +2  TO  WS-LINE-CNT                                 
02596 *         MOVE T-SS           TO WS-BUFFER-BYTE (BUFFER-INDEX)    
02597 *                                WS-BUFFER-BYTE (BUFFER-INDEX + 1)
02598 *         SET BUFFER-INDEX UP BY +2.                              
02599 *                                                                 
02600 *    IF DOUBLE-SPACE                                              
02601 *         ADD +1  TO  WS-LINE-CNT                                 
02602 *         MOVE T-SS             TO WS-BUFFER-BYTE (BUFFER-INDEX)  
02603 *         SET BUFFER-INDEX UP BY +1.                              
02604 *                                                                 
02605 *    ADD +1 TO WS-LINE-CNT                                        
02606 ************************************************************      
02607 *     BYPASS NEW LINE SYMBOL                               *      
02608 *        IF FIRST BUFFER SENT AND TOP-OF-FORM SET.         *      
02609 *     OR IF FIRST LINE OF SUBSEQUENT BUFFERS.              *      
02610 ************************************************************      
02611 *                                                                 
02612 *    IF (BUFFER-INDEX GREATER THAN +1 AND                         
02613 *        WS-BUFFER-BYTE (BUFFER-INDEX - 1) = T-TP)                
02614 *        OR   FIRST-LINE-NEXT-BUFFER                              
02615 *        MOVE ZERO               TO WS-FIRST-TIME-SW              
02616 *       ELSE                                                      
02617 *        MOVE T-SS          TO WS-BUFFER-BYTE (BUFFER-INDEX)      
02618 *        SET BUFFER-INDEX UP BY +1.                               
02619 *                                                                 
02620 *    IF TOP-PAGE                                                  
02621 **        NOTE, SINGLE SPACE IS REQUIRED BEFORE TOP PAGE CHAR     
02622 *         MOVE +1                TO WS-LINE-CNT                   
02623 *         MOVE T-TP              TO WS-BUFFER-BYTE (BUFFER-INDEX) 
02624 *         SET BUFFER-INDEX UP BY +1.                              
02625 *                                                                 
02626 *    IF WS-PASSED-DATA = SPACES                                   
02627 *        GO TO ELPRTCVP-EXIT.                                     
02628 *                                                                 
02629 *    SET PRT-INDEX TO +1.                                         
02630 *                                                                 
02631 *ELPRTCVP-060.                                                    
02632 *    MOVE WS-PRINT-BYTE (PRT-INDEX)                               
02633 *                                TO WS-BUFFER-BYTE (BUFFER-INDEX).
02634 *    SET BUFFER-INDEX UP BY +1.                                   
02635 *                                                                 
02636 *    IF PRT-INDEX LESS THAN WS-LINE-LENGTH                        
02637 *        SET PRT-INDEX UP BY +1                                   
02638 *        GO TO ELPRTCVP-060.                                      
02639 *                                                                 
02640 *    SET WS-BUFFER-LENGTH TO BUFFER-INDEX.                        
02641 *                                                                 
02642 *ELPRTCVP-EXIT.                                                   
02643 *    EXIT.                                                        
02644 *                                                                 
02645 *ELPRTCVP-PRINT-BUFFER.                                           
02646 *    IF WS-BUFFER-BYTE (BUFFER-INDEX - 1) = T-SS                  
02647 *       MOVE SPACE               TO WS-BUFFER-BYTE (BUFFER-INDEX) 
02648 *       SET BUFFER-INDEX UP BY 1.                                 
02649 *                                                                 
02650 *    MOVE  T-EM                  TO WS-BUFFER-BYTE (BUFFER-INDEX) 
02651 *    SET WS-BUFFER-LENGTH TO BUFFER-INDEX.                        
02652 *                                                                 
02653 *    ADD +1  TO  WS-DUMP-COUNT.                                   
02654 *                                                                 
02655 *    EXEC CICS SEND                                               
02656 *        FROM    (WS-BUFFER-AREA)                                 
02657 *        LENGTH  (WS-BUFFER-LENGTH)                               
02658 *        CTLCHAR (WS-WCC-CNTL)                                    
02659 *        ERASE   END-EXEC                                         
02660 *                                                                 
02661 *    EXEC CICS SYNCPOINT                                          
02662 *        END-EXEC.                                                
02663 *                                                                 
02664 *    SET BUFFER-INDEX TO +1.                                      
02665 *    MOVE '2'                    TO WS-FIRST-TIME-SW.             
02666 *                                                                 
02667 *ELPRTCVP-PRINT-EXIT.                                             
02668 *    EXIT.                                                        
02669 *                                                                 
02670 *    EJECT                                                        
02671  8500-DATE-CONVERSION SECTION.                                    
02672      EXEC CICS LINK                                               
02673          PROGRAM  ('ELDATCV')                                     
02674          COMMAREA (DATE-CONVERSION-DATA)                          
02675          LENGTH   (DC-COMM-LENGTH) END-EXEC.                      
02676                                                                   
02677  8500-EXIT.                                                       
02678      EXIT.                                                        
02679                                                                   
02680      EJECT                                                        
02681  SPELL-DOLLAR SECTION. COPY ELC176P1.                             
02682                                                                   
02683      EJECT                                                        
02684  POS-SPELL-DOLLAR SECTION.                                        
02685                                                                   
02686 *SDS-NOTE.                                                        
02687 *                                                                 
02688 *    NOTE ******************************************************* 
02689 *         *      THIS SECTION CONVERTS A DOLLAR FIGURE INTO A   * 
02690 *         *  SPELLED OUT AMOUNT.                                * 
02691 *         *******************************************************.
02692                                                                   
02693  SDS-010.                                                         
02694      MOVE SPACES                 TO  WS-SPELLED-AMOUNT            
02695                                      SD-PASS-SPELLED-AMOUNT       
02696                                      WS-SPELLED-LINE1             
02697                                      WS-SPELLED-LINE2.            
02698      MOVE ZERO                   TO  WS-SW.                       
02699                                                                   
02700      SET SA-INDEX TO +1.                                          
02701                                                                   
02702      MOVE SD-PASS-AMOUNT         TO  WS-AMOUNT.                   
02703                                                                   
02704      IF WS-MILLIONS IS GREATER THAN ZERO                          
02705          MOVE WS-MILLIONS        TO  WS-AMOUNT-WORK               
02706          PERFORM POS-SPELL-AMOUNT                                 
02707          MOVE +1                 TO  WS-SW.                       
02708                                                                   
02709      IF WS-THOUSANDS IS GREATER THAN ZERO                         
02710          MOVE WS-THOUSANDS       TO  WS-AMOUNT-WORK               
02711          PERFORM POS-SPELL-AMOUNT                                 
02712          MOVE +1                 TO  WS-SW                        
02713        ELSE                                                       
02714          IF WS-MILLIONS IS GREATER THAN ZERO                      
02715              MOVE 'ZERO'         TO  WS-WORD                      
02716              PERFORM MOVE-WORD 3 TIMES.                           
02717                                                                   
02718      IF WS-HUNDREDS IS GREATER THAN ZERO                          
02719          MOVE WS-HUNDREDS        TO  WS-AMOUNT-WORK               
02720          PERFORM POS-SPELL-AMOUNT                                 
02721          MOVE +1                 TO  WS-SW                        
02722        ELSE                                                       
02723          IF WS-MILLIONS IS GREATER THAN ZERO                      
02724            OR WS-THOUSANDS IS GREATER THAN ZERO                   
02725              MOVE 'ZERO'         TO  WS-WORD                      
02726              PERFORM MOVE-WORD 3 TIMES.                           
02727                                                                   
02728      IF WS-AMOUNT IS LESS THAN +1.00                              
02729          MOVE 'NO'               TO  WS-WORD                      
02730          PERFORM MOVE-WORD.                                       
02731                                                                   
02732      IF WS-CENTS IS NOT GREATER THAN ZERO                         
02733          MOVE 'NO'               TO  WS-CENTS-X.                  
02734                                                                   
02735      MOVE WS-CENTS-X             TO  WS-PENNEYS.                  
02736                                                                   
02737      MOVE WS-DOLLARS-AND-CENTS   TO  WS-WORD.                     
02738      PERFORM MOVE-WORD.                                           
02739                                                                   
02740      INSPECT WS-SPELLED-AMOUNT CONVERTING '-' TO ' '.             
02741                                                                   
02742      MOVE WS-SPELLED-AMOUNT      TO  SD-PASS-SPELLED-AMOUNT.      
02743                                                                   
02744      PERFORM MOVE-SPELLED-AMOUNT.                                 
02745                                                                   
02746  SDS-EXIT.                                                        
02747      EXIT.                                                        
02748                                                                   
02749      EJECT                                                        
02750  POS-SPELL-AMOUNT SECTION.                                        
02751                                                                   
02752 *SAS-NOTE.                                                        
02753 *                                                                 
02754 *    NOTE ******************************************************* 
02755 *         *      THIS SECTION CONVERTS A THREE DIGIT NUMBER     * 
02756 *         *  INTO A SPELLED AMOUNT.                             * 
02757 *         *******************************************************.
02758                                                                   
02759  SAS-010.                                                         
02760      IF WS-HUNDRED IS GREATER THAN ZERO                           
02761          SET SINGLE-INDEX        TO  WS-HUNDRED                   
02762          MOVE WS-SINGLE-DESC (SINGLE-INDEX)  TO  WS-WORD          
02763          PERFORM MOVE-WORD                                        
02764        ELSE                                                       
02765          IF WS-SW NOT = ZERO                                      
02766              MOVE 'ZERO'         TO  WS-WORD                      
02767              PERFORM MOVE-WORD.                                   
02768                                                                   
02769      IF WS-TEN IS GREATER THAN ZERO                               
02770          SET SINGLE-INDEX TO WS-TEN                               
02771          MOVE WS-SINGLE-DESC (SINGLE-INDEX)  TO  WS-WORD          
02772          PERFORM MOVE-WORD                                        
02773        ELSE                                                       
02774          IF WS-HUNDRED IS GREATER THAN ZERO                       
02775            OR WS-SW NOT = ZERO                                    
02776              MOVE 'ZERO'         TO  WS-WORD                      
02777              PERFORM MOVE-WORD.                                   
02778                                                                   
02779      IF WS-ONE IS GREATER THAN ZERO                               
02780          SET SINGLE-INDEX TO WS-ONE                               
02781          MOVE WS-SINGLE-DESC (SINGLE-INDEX)  TO  WS-WORD          
02782          PERFORM MOVE-WORD                                        
02783        ELSE                                                       
02784          IF WS-HUNDRED IS GREATER THAN ZERO                       
02785            OR WS-TEN IS GREATER THAN ZERO                         
02786            OR WS-SW NOT = ZERO                                    
02787              MOVE 'ZERO'         TO  WS-WORD                      
02788              PERFORM MOVE-WORD.                                   
02789                                                                   
02790  SAS-EXIT.                                                        
02791      EXIT.                                                        
02792                                                                   
02793      EJECT                                                        
02794  9990-ERROR SECTION.                                              
02795      EXEC CICS LINK                                               
02796          PROGRAM  ('EL004')                                       
02797          COMMAREA (DFHEIBLK)                                      
02798          LENGTH   (64) END-EXEC.                                  
02799                                                                   
02800  9990-EXIT.                                                       
02801      EXIT.                                                        
02802                                                                   
02803  9999-LAST-PARAGRAPH SECTION.                                     
02804                                                                   
02805      GOBACK.                                                      
