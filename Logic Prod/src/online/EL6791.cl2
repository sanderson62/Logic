00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL6791.                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 02/14/96 07:58:44.                 
00007 *                            VMOD=2.003                           
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
00023 *REMARKS.                                                         
00024 *         THIS PROGRAM IS STARTED  FROM EL679  TO PRINT           
00025 *         THE BILLING STATUS REPORT.                              
00026      EJECT                                                        
00027  ENVIRONMENT DIVISION.                                            
00028  DATA DIVISION.                                                   
00029  WORKING-STORAGE SECTION.                                         
00030  77  FILLER  PIC X(32) VALUE '********************************'.  
00031  77  FILLER  PIC X(32) VALUE '*    EL6791  WORKING-STORAGE   *'.  
00032  77  FILLER  PIC X(32) VALUE '******** VMOD=2.003 ************'.  
00033                                                                   
00034  77  CLEN                        PIC S9(4)  COMP    VALUE +1024.  
00035                                                                   
00036  01  WORK-AREAS.                                                  
00037      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     
00038      12  ELREPT-FILE-ID          PIC X(8)    VALUE 'ELREPT'.      
00039      12  ERBILL-FILE-ID          PIC X(8)    VALUE 'ERBILL'.      
00040      12  ELCNTL-FILE-ID          PIC X(8)    VALUE 'ELCNTL'.      
00041      12  PGM-NAME                PIC X(8)    VALUE SPACES.        
00042      12  PRT-CNT                 PIC 9       VALUE 4.             
00043      12  WS-LINE-NUMBER          PIC 9(5)    VALUE ZERO.          
00044      12  WS-PAGE                 PIC 9(5)    VALUE ZERO.          
00045      12  WS-CURRENT-DATE         PIC X(8).                        
00046      12  WS-TIME                 PIC S9(7).                       
00047      12  FILLER    REDEFINES WS-TIME.                             
00048          16  FILLER              PIC X.                           
00049          16  WS-TIME-6           PIC X(6).                        
00050      12  EMI-LINE1               PIC X(72).                       
00051      12  ABEND-AREA              PIC X(72).                       
00052      12  ERBILL-EOF-SW           PIC X     VALUE SPACE.           
00053          88  ERBILL-EOF      VALUE 'Y'.                           
00054      12  PRINT-CONTROL.                                           
00055          16  SINGLE-SPACE        PIC X     VALUE SPACE.           
00056          16  DOUBLE-SPACE        PIC X     VALUE ZERO.            
00057          16  TRIPLE-SPACE        PIC X     VALUE '-'.             
00058          16  SUPPRESS-SPACE      PIC X     VALUE '+'.             
00059          16  TOP-OF-PAGE         PIC X     VALUE '1'.             
00060      EJECT                                                        
00061  01  ACCESS-KEYS.                                                 
00062      12  ELCNTL-KEY.                                              
00063          16  ELCNTL-COMPANY-ID   PIC XXX   VALUE SPACES.          
00064          16  ELCNTL-REC-TYPE     PIC X     VALUE SPACES.          
00065          16  ELCNTL-FILLER       PIC X(3)  VALUE SPACES.          
00066          16  ELCNTL-CARRIER      PIC X     VALUE SPACES.          
00067          16  ELCNTL-SEQ-NO       PIC S9(4) COMP VALUE ZEROS.      
00068      EJECT                                                        
00069                           COPY ELCREPT.                           
00070      EJECT                                                        
00071                                                                   
00072  01  PRT-LINES.                                                   
00073      12  HDR-1.                                                   
00074          16  FILLER          PIC X(33)   VALUE SPACES.            
00075          16  FILLER          PIC X(14)   VALUE 'BILLING STATUS'.  
00076          16  FILLER          PIC X(24)   VALUE SPACES.            
00077          16  FILLER          PIC X(9)    VALUE 'EL - 6791'.       
00078                                                                   
00079      12  HDR-2.                                                   
00080          16  FILLER          PIC X(25)   VALUE SPACES.            
00081          16  HDR-COMP        PIC X(30)   VALUE SPACES.            
00082          16  FILLER          PIC X(25)   VALUE SPACES.            
00083                                                                   
00084      12  HDR-3.                                                   
00085          16  FILLER          PIC X(31)   VALUE SPACES.            
00086          16  HDR-DATE        PIC X(18)   VALUE SPACES.            
00087          16  FILLER          PIC X(20)   VALUE SPACES.            
00088          16  FILLER          PIC X(05)   VALUE 'PAGE '.           
00089          16  HDR-PAGE        PIC ZZ,ZZ9.                          
00090                                                                   
00091      12  HDR-4.                                                   
00092          16  FILLER      PIC X(10)   VALUE ' CARRIER: '.          
00093          16  HDR-CARRIER PIC X.                                   
00094          16  FILLER      PIC X(8)    VALUE ' GROUP: '.            
00095          16  HDR-GROUP   PIC X(6).                                
00096          16  FILLER      PIC X(10)   VALUE ' ACCOUNT: '.          
00097          16  HDR-ACCT    PIC X(10).                               
00098          16  FILLER      PIC X(11)   VALUE ' FIN RESP: '.         
00099          16  HDR-FINRESP PIC X(10).                               
00100                                                                   
00101      12  DTL-1.                                                   
00102          16  FILLER      PIC X(5)    VALUE SPACES.                
00103          16  FILLER      PIC X(19)   VALUE 'BALANCE FORWARD    '. 
00104          16  BALFRWD     PIC ZZZZ,ZZ9.99-.                        
00105          16  FILLER      PIC X(2)    VALUE SPACES.                
00106          16  FILLER      PIC X(12)   VALUE 'NAME:       '.        
00107          16  DTL1-NAME   PIC X(30)   VALUE SPACES.                
00108                                                                   
00109      12  DTL-2.                                                   
00110          16  FILLER      PIC X(5)    VALUE SPACES.                
00111          16  FILLER      PIC X(19)   VALUE 'PREMIUM WRITTEN  + '. 
00112          16  PREMIUM     PIC ZZZZ,ZZ9.99-.                        
00113          16  FILLER      PIC XX      VALUE SPACES.                
00114          16  FILLER      PIC X(12)   VALUE 'BILL DATE:  '.        
00115          16  BILL-DATE   PIC X(8)    VALUE SPACES.                
00116          16  FILLER      PIC X(22)   VALUE SPACES.                
00117                                                                   
00118      12  DTL-3.                                                   
00119          16  FILLER      PIC X(5)    VALUE SPACES.                
00120          16  FILLER      PIC X(19)   VALUE 'AMOUNTS REMITTED - '. 
00121          16  REMITTED    PIC ZZZZ,ZZ9.99-.                        
00122          16  FILLER      PIC XX      VALUE SPACES.                
00123          16  FILLER      PIC X(12)   VALUE 'PRINT DATE: '.        
00124          16  PRT-DATE    PIC X(8)    VALUE SPACES.                
00125          16  FILLER      PIC X(22)   VALUE SPACES.                
00126                                                                   
00127      12  DTL-REST.                                                
00128          16  FILLER      PIC X(5)    VALUE SPACES.                
00129          16  DTL-LIT     PIC X(19)   VALUE SPACES.                
00130          16  DTL-AMT     PIC ZZZZ,ZZ9.99-.                        
00131          16  FILLER      PIC X(44)   VALUE SPACES.                
00132                                                                   
00133      EJECT                                                        
00134                              COPY ELCDATE.                        
00135      EJECT                                                        
00136                              COPY ELCAID.                         
00137  01  FILLER    REDEFINES DFHAID.                                  
00138      12  FILLER              PIC X(8).                            
00139      12  PF-VALUES           PIC X       OCCURS 2.                
00140      EJECT                                                        
00141                               COPY ELCINTF.                       
00142      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.                
00143          16  PI-SAV-ERBILL-KEY.                                   
00144              20  PI-SAV-CO-CD            PIC X.                   
00145              20  PI-SAV-CARRIER          PIC X.                   
00146              20  PI-SAV-GROUP            PIC X(6).                
00147              20  PI-SAV-ACCOUNT          PIC X(10).               
00148              20  PI-SAV-FIN-RESP         PIC X(10).               
00149              20  PI-SAV-REC-TYPE         PIC X.                   
00150              20  PI-SAV-LINE-SEQ-NO      PIC S9(4)   COMP.        
00151          16  FILLER                      PIC X(609).              
00152                                                                   
00153      EJECT                                                        
00154  LINKAGE SECTION.                                                 
00155  01  DFHCOMMAREA                     PIC X(1024).                 
00156 *01 PARM-LIST .                                                   
00157 *    02  FILLER              PIC S9(8)   COMP.                    
00158 *    02  ERBILL-POINTER      PIC S9(8)   COMP.                    
00159 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                    
00160      EJECT                                                        
00161                           COPY ERCBILL.                           
00162      EJECT                                                        
00163                           COPY ELCCNTL.                           
00164      EJECT                                                        
00165                                                                   
00166  PROCEDURE DIVISION.                                              
00167      MOVE DFHCOMMAREA  TO  PROGRAM-INTERFACE-BLOCK.               
00168                                                                   
00169  0100-START.                                                      
00170      EXEC CICS  HANDLE CONDITION                                  
00171             ERROR    (8300-ABEND)                                 
00172             PGMIDERR (8900-PGMIDERR)                              
00173      END-EXEC.                                                    
00174                                                                   
00175  0200-RECEIVE.                                                    
00176      EXEC CICS RETRIEVE                                           
00177          INTO   (PROGRAM-INTERFACE-BLOCK)                         
00178          LENGTH (CLEN)                                            
00179      END-EXEC.                                                    
00180                                                                   
00181      EXEC CICS  HANDLE CONDITION                                  
00182             NOTFND   (0250-DELETE-TYPE-2)                         
00183      END-EXEC.                                                    
00184                                                                   
00185      MOVE PI-COMPANY-CD TO RF-COMPANY-CD.                         
00186      MOVE 'RF'       TO RF-RECORD-ID.                             
00187      MOVE '1'        TO RF-RECORD-TYPE.                           
00188      MOVE 'EL679'    TO RF-REPORT-ID.                             
00189      EXEC CICS DELETE                                             
00190          DATASET (ELREPT-FILE-ID)                                 
00191          RIDFLD  (RF-CONTROL-PRIMARY)                             
00192          KEYLENGTH (7)                                            
00193          GENERIC                                                  
00194          END-EXEC.                                                
00195                                                                   
00196  0250-DELETE-TYPE-2.                                              
00197      EXEC CICS  HANDLE CONDITION                                  
00198             NOTFND   (1000-WRITE-REPORT)                          
00199      END-EXEC.                                                    
00200                                                                   
00201      MOVE PI-COMPANY-CD TO RF-COMPANY-CD.                         
00202      MOVE 'RF'       TO RF-RECORD-ID.                             
00203      MOVE '2'        TO RF-RECORD-TYPE.                           
00204      MOVE 'EL679'    TO RF-REPORT-ID.                             
00205      EXEC CICS DELETE                                             
00206          DATASET (ELREPT-FILE-ID)                                 
00207          RIDFLD  (RF-CONTROL-PRIMARY)                             
00208          KEYLENGTH (7)                                            
00209          GENERIC                                                  
00210          END-EXEC.                                                
00211                                                                   
00212  1000-WRITE-REPORT.                                               
00213      MOVE EIBDATE                TO DC-JULIAN-YYDDD               
00214      MOVE '5'                    TO DC-OPTION-CODE                
00215      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                     
00216      MOVE DC-GREG-DATE-1-ALPHA   TO HDR-DATE.                     
00217      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DATE.              
00218      PERFORM 3000-READ-CONTROL-FILE THRU 3090-EXIT.               
00219      MOVE CF-CL-MAIL-TO-NAME     TO HDR-COMP.                     
00220      PERFORM 3100-START-BROWSE THRU 3190-EXIT.                    
00221                                                                   
00222  1100-WRITE-REPORT-LOOP.                                          
00223      PERFORM 3200-READ-NEXT-RECORD THRU 3290-EXIT.                
00224      IF ERBILL-EOF                                                
00225          GO TO 1200-WRITE-REPORT-TRAILER.                         
00226      PERFORM 2000-FORMAT-REPORT THRU 2090-EXIT.                   
00227      MOVE '3'                    TO PI-SAV-REC-TYPE.              
00228      MOVE 9999                   TO PI-SAV-LINE-SEQ-NO.           
00229      GO TO 1100-WRITE-REPORT-LOOP.                                
00230                                                                   
00231  1200-WRITE-REPORT-TRAILER.                                       
00232      PERFORM 3300-END-BROWSE THRU 3390-EXIT.                      
00233      MOVE SPACES                 TO RF-TRAILER-RECORD.            
00234      MOVE WS-CURRENT-DATE        TO RF-CURRENT-DATE.              
00235      MOVE EIBTIME                TO WS-TIME.                      
00236      MOVE WS-TIME-6              TO RF-PRINT-HH-MM-SS.            
00237      MOVE '2'                    TO RF-RECORD-TYPE.               
00238      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.             
00239      GO TO 9999-RETURN-CICS.                                      
00240      GOBACK.                                                      
00241      EJECT                                                        
00242  2000-FORMAT-REPORT.                                              
00243      IF PRT-CNT = 4                                               
00244          MOVE ZEROS              TO PRT-CNT                       
00245          PERFORM 2100-FORMAT-HEADINGS THRU 2190-EXIT.             
00246      MOVE TRIPLE-SPACE           TO RF-CTL-CHAR-133.              
00247      MOVE '1'                    TO RF-RECORD-TYPE.               
00248      MOVE BI-CARRIER             TO HDR-CARRIER.                  
00249      MOVE BI-GROUPING            TO HDR-GROUP.                    
00250      MOVE BI-ACCOUNT             TO HDR-ACCT.                     
00251      MOVE BI-FIN-RESP            TO HDR-FINRESP.                  
00252      MOVE HDR-4                  TO RF-DATA-133.                  
00253      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.             
00254                                                                   
00255      MOVE DOUBLE-SPACE           TO RF-CTL-CHAR-133.              
00256      MOVE '1'                    TO RF-RECORD-TYPE.               
00257      MOVE BI-BAL-FRWD            TO BALFRWD.                      
00258      MOVE BI-FIN-RESP-NAME       TO DTL1-NAME.                    
00259      MOVE DTL-1                  TO RF-DATA-133.                  
00260      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.             
00261                                                                   
00262      MOVE SINGLE-SPACE           TO RF-CTL-CHAR-133.              
00263      MOVE '1'                    TO RF-RECORD-TYPE.               
00264      MOVE BI-PREMIUM             TO PREMIUM.                      
00265      MOVE BI-CREATION-DT         TO DC-BIN-DATE-1                 
00266      MOVE SPACE                  TO DC-OPTION-CODE.               
00267      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
00268      MOVE DC-GREG-DATE-1-EDIT    TO BILL-DATE.                    
00269      MOVE DTL-2                  TO RF-DATA-133.                  
00270      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.             
00271                                                                   
00272      MOVE SINGLE-SPACE           TO RF-CTL-CHAR-133.              
00273      MOVE '1'                    TO RF-RECORD-TYPE.               
00274      MOVE BI-REMITTED            TO REMITTED                      
00275      IF BI-INITIAL-PRINT-DATE NOT = LOW-VALUES                    
00276          MOVE BI-INITIAL-PRINT-DATE  TO DC-BIN-DATE-1             
00277          MOVE SPACE                  TO DC-OPTION-CODE            
00278          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                 
00279          MOVE DC-GREG-DATE-1-EDIT    TO PRT-DATE                  
00280      ELSE                                                         
00281          MOVE SPACES             TO PRT-DATE.                     
00282      MOVE DTL-3                  TO RF-DATA-133.                  
00283      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.             
00284                                                                   
00285      MOVE SINGLE-SPACE           TO RF-CTL-CHAR-133.              
00286      MOVE '1'                    TO RF-RECORD-TYPE.               
00287      MOVE 'COMP ON ISSUES   - '  TO DTL-LIT.                      
00288      MOVE BI-TOT-ISS-COMP        TO DTL-AMT.                      
00289      MOVE DTL-REST               TO RF-DATA-133.                  
00290      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.             
00291                                                                   
00292      MOVE SINGLE-SPACE           TO RF-CTL-CHAR-133.              
00293      MOVE '1'                    TO RF-RECORD-TYPE.               
00294      MOVE 'COMP ON CANCELS  + '  TO DTL-LIT.                      
00295      MOVE BI-TOT-CAN-COMP        TO DTL-AMT.                      
00296      MOVE DTL-REST               TO RF-DATA-133.                  
00297      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.             
00298                                                                   
00299      MOVE SINGLE-SPACE           TO RF-CTL-CHAR-133.              
00300      MOVE '1'                    TO RF-RECORD-TYPE.               
00301      MOVE 'ADJUSTMENTS      + '  TO DTL-LIT.                      
00302      MOVE BI-ADJUSTMNTS          TO DTL-AMT.                      
00303      MOVE DTL-REST               TO RF-DATA-133.                  
00304      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.             
00305                                                                   
00306      MOVE SINGLE-SPACE           TO RF-CTL-CHAR-133.              
00307      MOVE '1'                    TO RF-RECORD-TYPE.               
00308      MOVE 'AMOUNT DISBURSED + '  TO DTL-LIT.                      
00309      MOVE BI-DISBURSED           TO DTL-AMT.                      
00310      MOVE DTL-REST               TO RF-DATA-133.                  
00311      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.             
00312                                                                   
00313      MOVE SINGLE-SPACE           TO RF-CTL-CHAR-133.              
00314      MOVE '1'                    TO RF-RECORD-TYPE.               
00315      MOVE 'ENDING BALANCE     '  TO DTL-LIT.                      
00316      MOVE BI-END-BAL             TO DTL-AMT.                      
00317      MOVE DTL-REST               TO RF-DATA-133.                  
00318      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.             
00319                                                                   
00320      ADD 1   TO PRT-CNT.                                          
00321  2090-EXIT.                                                       
00322      EXIT.                                                        
00323      EJECT                                                        
00324  2100-FORMAT-HEADINGS.                                            
00325      ADD 1   TO WS-PAGE.                                          
00326      MOVE TOP-OF-PAGE            TO RF-CTL-CHAR-133.              
00327      MOVE '1'                    TO RF-RECORD-TYPE.               
00328      MOVE HDR-1                  TO RF-DATA-133.                  
00329      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.             
00330                                                                   
00331      MOVE SINGLE-SPACE           TO RF-CTL-CHAR-133.              
00332      MOVE '1'                    TO RF-RECORD-TYPE.               
00333      MOVE HDR-2                  TO RF-DATA-133.                  
00334      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.             
00335                                                                   
00336      MOVE SINGLE-SPACE           TO RF-CTL-CHAR-133.              
00337      MOVE '1'                    TO RF-RECORD-TYPE.               
00338      MOVE WS-PAGE                TO HDR-PAGE.                     
00339      MOVE HDR-3                  TO RF-DATA-133.                  
00340      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.             
00341                                                                   
00342  2190-EXIT.                                                       
00343      EXIT.                                                        
00344      EJECT                                                        
00345  3000-READ-CONTROL-FILE.                                          
00346      MOVE SPACES                 TO ELCNTL-KEY.                   
00347      MOVE '1'                    TO ELCNTL-REC-TYPE.              
00348      MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.            
00349      MOVE +0                     TO ELCNTL-SEQ-NO.                
00350                                                                   
00351      EXEC CICS HANDLE CONDITION                                   
00352          NOTFND   (9999-RETURN-CICS)                              
00353          END-EXEC.                                                
00354                                                                   
00355      EXEC CICS READ                                               
00356          DATASET (ELCNTL-FILE-ID)                                 
00357          SET (ADDRESS OF CONTROL-FILE)                            
00358          RIDFLD (ELCNTL-KEY)                                      
00359          END-EXEC.                                                
00360                                                                   
00361  3090-EXIT.                                                       
00362      EXIT.                                                        
00363      EJECT                                                        
00364  3100-START-BROWSE.                                               
00365      EXEC CICS HANDLE CONDITION                                   
00366          NOTFND (9999-RETURN-CICS)                                
00367          END-EXEC.                                                
00368      MOVE LOW-VALUES             TO PI-SAV-ERBILL-KEY.            
00369      MOVE PI-COMPANY-CD          TO PI-SAV-CO-CD.                 
00370      MOVE '1'                    TO PI-SAV-REC-TYPE.              
00371      MOVE +0                     TO PI-SAV-LINE-SEQ-NO.           
00372      EXEC CICS STARTBR                                            
00373          DATASET (ERBILL-FILE-ID)                                 
00374          RIDFLD (PI-SAV-ERBILL-KEY)                               
00375          END-EXEC.                                                
00376  3190-EXIT.                                                       
00377      EXIT.                                                        
00378      EJECT                                                        
00379  3200-READ-NEXT-RECORD.                                           
00380      EXEC CICS HANDLE CONDITION                                   
00381          ENDFILE (3210-END-OF-FILE)                               
00382          END-EXEC.                                                
00383                                                                   
00384      EXEC CICS READNEXT                                           
00385          SET (ADDRESS OF BILLING-STATEMENT)                       
00386          DATASET (ERBILL-FILE-ID)                                 
00387          RIDFLD (PI-SAV-ERBILL-KEY)                               
00388          END-EXEC.                                                
00389                                                                   
00390      IF BI-COMPANY-CD NOT = PI-COMPANY-CD                         
00391          GO TO 3210-END-OF-FILE.                                  
00392      IF NOT BI-HEADER-DATA                                        
00393          GO TO 3200-READ-NEXT-RECORD.                             
00394      IF BI-ACCOUNT = LOW-VALUES                                   
00395          GO TO 3200-READ-NEXT-RECORD.                             
00396      GO TO 3290-EXIT.                                             
00397                                                                   
00398  3210-END-OF-FILE.                                                
00399      MOVE 'Y'                    TO ERBILL-EOF-SW.                
00400  3290-EXIT.                                                       
00401      EXIT.                                                        
00402      EJECT                                                        
00403  3300-END-BROWSE.                                                 
00404      EXEC CICS ENDBR                                              
00405          DATASET (ERBILL-FILE-ID)                                 
00406          END-EXEC.                                                
00407                                                                   
00408  3390-EXIT.                                                       
00409      EXIT.                                                        
00410      EJECT                                                        
00411  3400-WRITE-REPORT-RECORD.                                        
00412      MOVE PI-COMPANY-CD TO RF-COMPANY-CD.                         
00413      MOVE 'RF'       TO RF-RECORD-ID.                             
00414      MOVE 'EL679'    TO RF-REPORT-ID.                             
00415      ADD 1 TO WS-LINE-NUMBER.                                     
00416      MOVE WS-LINE-NUMBER TO RF-LINE-NUMBER.                       
00417                                                                   
00418      EXEC CICS WRITE                                              
00419          DATASET (ELREPT-FILE-ID)                                 
00420          FROM    (REPORT-SAVE-FILE)                               
00421          RIDFLD  (RF-CONTROL-PRIMARY)                             
00422          END-EXEC.                                                
00423  3490-EXIT.                                                       
00424       EXIT.                                                       
00425      EJECT                                                        
00426  8300-ABEND.                                                      
00427      MOVE DFHEIBLK TO EMI-LINE1.                                  
00428      EXEC CICS LINK                                               
00429          PROGRAM   ('EL004')                                      
00430          COMMAREA  (EMI-LINE1)                                    
00431          LENGTH    (72)                                           
00432      END-EXEC.                                                    
00433      GO TO 9999-RETURN-CICS.                                      
00434                                                                   
00435  8500-DATE-CONVERT.                                               
00436      MOVE LINK-ELDATCV           TO PGM-NAME.                     
00437      EXEC CICS LINK                                               
00438          PROGRAM    (PGM-NAME)                                    
00439          COMMAREA   (DATE-CONVERSION-DATA)                        
00440          LENGTH     (DC-COMM-LENGTH)                              
00441          END-EXEC.                                                
00442  8500-EXIT.                                                       
00443      EXIT.                                                        
00444                                                                   
00445  8900-PGMIDERR.                                                   
00446                                                                   
00447  9999-RETURN-CICS.                                                
00448                                                                   
00449         EXEC CICS  RETURN                                         
00450              END-EXEC.                                            
00451                                                                   
00452  9999-EXIT. EXIT.                                                 
00453                                                                   
