00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL054 .                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 03/04/95 12:41:29.                 
00007 *                            VMOD=2.002                           
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
00023 *REMARKS.    TRANSACTION - EXBT                                   
00024 *         THIS PROGRAM IS STARTED  FROM EL930.  IT'S              
00025 *         FUNCTION IS TO CREATE PENDING BUSINESS BATCHES FROM     
00026 *         DUMMY BATCHES IN THE TEMPORARY PENDING BUSINESS FILE.   
00027      EJECT                                                        
00028  ENVIRONMENT DIVISION.                                            
00029  DATA DIVISION.                                                   
00030  WORKING-STORAGE SECTION.                                         
00031  77  FILLER  PIC X(32) VALUE '********************************'.  
00032  77  FILLER  PIC X(32) VALUE '*     EL054  WORKING-STORAGE   *'.  
00033  77  FILLER  PIC X(32) VALUE '************ V/M 2.002 *********'.  
00034  77  CLEN                        PIC S9(4)  COMP    VALUE +16.    
00035                                                                   
00036  01  WORK-AREAS.                                                  
00037      12  GETMAIN-SPACE           PIC X       VALUE SPACE.         
00038      12  ERPNDB-FILE-ID          PIC X(8)    VALUE 'ERPNDB'.      
00039      12  ERPNDM-FILE-ID          PIC X(8)    VALUE 'ERPNDM'.      
00040      12  ERPNDT-FILE-ID          PIC X(8)    VALUE 'ERPNDT'.      
00041      12  ERPNDT2-FILE-ID         PIC X(8)    VALUE 'ERPNDT2'.     
00042      12  ELCNTL-FILE-ID          PIC X(8)    VALUE 'ELCNTL'.      
00043      12  JOURNAL-FILE-ID         PIC S9(4)   COMP VALUE ZEROS.    
00044      12  THIS-PGM                PIC X(8)    VALUE 'EL054'.       
00045      12  TRANS-ID                PIC X(4)    VALUE 'EXBT'.        
00046      12  EDIT-TRANS              PIC X(4)    VALUE 'EXEB'.        
00047      12  ELCNTL-LENGTH           PIC S9(4)   COMP VALUE +504.     
00048      12  ERPNDB-LENGTH           PIC S9(4)   COMP VALUE +585.     
CIDMOD*    12  ERPNDM-LENGTH           PIC S9(4)   COMP VALUE +250.     
CIDMOD     12  ERPNDM-LENGTH           PIC S9(4)   COMP VALUE +374.     
00050      12  ERPNDT-LENGTH           PIC S9(4)   COMP VALUE +585.     
00051      12  JOURNAL-LENGTH          PIC S9(4)   COMP VALUE ZEROS.    
00052      12  WS-SAVE-BATCH-KEY       PIC X(11)   VALUE LOW-VALUES.    
00053      12  WS-SAVE-ACCOUNT-KEY     PIC X(13)   VALUE LOW-VALUES.    
00054      12  WS-SAVE-CONTROL-PRIMARY PIC X(11)   VALUE LOW-VALUES.    
00055      12  WS-ACCOUNT-CONTROL.                                      
00056          16  WS-AC-COMPANY-CD    PIC X       VALUE LOW-VALUES.    
00057          16  WS-AC-CARRIER       PIC X       VALUE LOW-VALUES.    
00058          16  WS-AC-GROUPING      PIC X(10)   VALUE LOW-VALUES.    
00059          16  WS-AC-STATE         PIC XX      VALUE LOW-VALUES.    
00060          16  WS-AC-ACCOUNT       PIC X(10)   VALUE LOW-VALUES.    
00061      12  WS-BATCH-SEQ-NO         PIC S9(4)   COMP   VALUE +1.     
00062      12  WS-LF-ISS-ENTERED       PIC S9(9)V99 COMP-3 VALUE +0.    
00063      12  WS-LF-CAN-ENTERED       PIC S9(9)V99 COMP-3 VALUE +0.    
00064      12  WS-AH-ISS-ENTERED       PIC S9(9)V99 COMP-3 VALUE +0.    
00065      12  WS-AH-CAN-ENTERED       PIC S9(9)V99 COMP-3 VALUE +0.    
00066      12  WS-ISS-CNT-ENTERED      PIC S9(5)    COMP-3 VALUE +0.    
00067      12  WS-CAN-CNT-ENTERED      PIC S9(5)    COMP-3 VALUE +0.    
00068      12  BATCH-TRAILER-SW        PIC X       VALUE SPACE.         
00069          88  BATCH-TRAILER-BUILT     VALUE 'Y'.                   
00070      12  WS-MAIL-PROCESSING      PIC X       VALUE SPACE.         
00071          88  MAIL-PROC               VALUE 'Y'.                   
00072      12  FIRST-BATCH-REC-SW      PIC X       VALUE 'Y'.           
00073          88  FIRST-TIME-THRU         VALUE 'Y'.                   
00074                                                                   
00075      EJECT                                                        
00076  01  ACCESS-KEYS.                                                 
00077      12  ELCNTL-KEY.                                              
00078          16  ELCNTL-COMPANY-ID       PIC XXX   VALUE SPACES.      
00079          16  ELCNTL-REC-TYPE         PIC X     VALUE SPACES.      
00080          16  ELCNTL-FILLER           PIC X(3)  VALUE SPACES.      
00081          16  ELCNTL-CARRIER          PIC X     VALUE SPACES.      
00082          16  ELCNTL-SEQ-NO           PIC S9(4) COMP  VALUE ZEROS. 
00083      12  ERPNDB-KEY.                                              
00084          16  ERPNDB-COMPANY-CD       PIC X     VALUE SPACES.      
00085          16  ERPNDB-BATCH            PIC X(6)  VALUE SPACES.      
00086          16  ERPNDB-SEQ-NO           PIC S9(4) COMP  VALUE ZEROS. 
00087          16  ERPNDB-SEQ-XX REDEFINES ERPNDB-SEQ-NO PIC XX.        
00088          16  ERPNDB-CHG-SEQ-NO       PIC S9(4) COMP  VALUE ZEROS. 
00089      12  ERPNDT2-KEY.                                             
00090          16  ERPNDT2-COMPANY-CD-A1   PIC X.                       
00091          16  ERPNDT2-CARRIER         PIC X.                       
00092          16  ERPNDT2-GROUPING        PIC X(10).                   
00093          16  ERPNDT2-STATE           PIC XX.                      
00094          16  ERPNDT2-ACCOUNT         PIC X(10).                   
00095          16  ERPNDT2-CERT-EFF-DT     PIC XX.                      
00096          16  ERPNDT2-CERT-NO.                                     
00097              20  ERPNDT2-CERT-PRIME  PIC X(10).                   
00098              20  ERPNDT2-CERT-SFX    PIC X.                       
00099          16  ERPNDT2-ALT-CHG-SEQ-NO  PIC S9(4)     COMP.          
00100                                                                   
00101          16  ERPNDT2-RECORD-TYPE               PIC X.             
00102              88  ERPNDT-MAILING-DATA                VALUE '0'.    
00103              88  ERPNDT-ISSUE                       VALUE '1'.    
00104              88  ERPNDT-CANCELLATION                VALUE '2'.    
00105              88  ERPNDT-BATCH-TRAILER               VALUE '9'.    
00106                                                                   
00107      EJECT                                                        
00108  01  BATCH-TO-PROCESS                VALUE SPACES.                
00109      05  EDIT-COMPANY-CD         PIC X.                           
00110      05  EDIT-BATCH              PIC X(6).                        
00111      05  EDIT-COMPANY-ID         PIC XXX.                         
00112      05  EDIT-RESTART-BATCH      PIC X(6).                        
00113                                                                   
00114  01  NEW-BATCH-TO-PROCESS            VALUE SPACES.                
00115      05  FILLER                  PIC X.                           
00116      05  NEW-EDIT-BATCH          PIC 9(6).                        
00117      05  FILLER                  PIC X(9).                        
00118                                                                   
00119  01  WS-SAVE-PB-REC              PIC X(585)  VALUE SPACES.        
00120                                                                   
00121  01  WS-SAVE-PM-REC              PIC X(250)  VALUE SPACES.        
00122                                                                   
00123      EJECT                                                        
00124                              COPY ELCJPFX.                        
00125                                  PIC X(585).                      
00126                                                                   
00127      EJECT                                                        
00128  LINKAGE SECTION.                                                 
00129  01  DFHCOMMAREA                 PIC X(1).                        
00130 *01 PARM-LIST .                                                   
00131 *    02  FILLER                  PIC S9(8)   COMP.                
00132 *    02  ELCNTL-POINTER          PIC S9(8)   COMP.                
00133 *    02  ERPNDM-POINTER          PIC S9(8)   COMP.                
00134 *    02  ERPNDB-POINTER          PIC S9(8)   COMP.                
00135      EJECT                                                        
00136                           COPY ELCCNTL.                           
00137      EJECT                                                        
00138                              COPY ERCPNDM.                        
00139      EJECT                                                        
00140                              COPY ERCPNDB.                        
00141      EJECT                                                        
00142                                                                   
00143  PROCEDURE DIVISION.                                              
00144                                                                   
00145      EXEC CICS GETMAIN                                            
00146          SET      (ADDRESS OF PENDING-BUSINESS)                   
00147          LENGTH   (585)                                           
00148          INITIMG  (GETMAIN-SPACE)                                 
00149          END-EXEC.                                                
00150                                                                   
00151  0000-RETRIEVE.                                                   
00152      MOVE 'Y'                    TO FIRST-BATCH-REC-SW.           
00153      MOVE LOW-VALUES             TO WS-SAVE-ACCOUNT-KEY.          
00154                                                                   
00155      EXEC CICS HANDLE CONDITION                                   
00156          NOTFND   (9999-RETURN-CICS)                              
00157          ENDDATA  (9999-RETURN-CICS)                              
00158          ERROR    (9999-ABEND)        END-EXEC.                   
00159                                                                   
00160      EXEC CICS RETRIEVE                                           
00161          INTO   (BATCH-TO-PROCESS)                                
00162          LENGTH (CLEN) END-EXEC.                                  
00163                                                                   
00164      MOVE BATCH-TO-PROCESS       TO NEW-BATCH-TO-PROCESS.         
00165                                                                   
00166      MOVE LOW-VALUES             TO ERPNDT2-KEY.                  
00167      MOVE EDIT-COMPANY-CD        TO ERPNDT2-COMPANY-CD-A1.        
00168                                                                   
00169      EJECT                                                        
00170  1000-STARTBR-ERPNDT2.                                            
00171      EXEC CICS HANDLE CONDITION                                   
00172          NOTFND   (9000-END-OF-FILE)                              
00173          END-EXEC.                                                
00174                                                                   
00175      EXEC CICS STARTBR                                            
00176          DATASET  (ERPNDT2-FILE-ID)                               
00177          RIDFLD   (ERPNDT2-KEY)                                   
00178          END-EXEC.                                                
00179                                                                   
00180  1100-READNEXT-ERPNDT2.                                           
00181      EXEC CICS HANDLE CONDITION                                   
00182          NOTFND   (9000-END-OF-FILE)                              
00183          ENDFILE  (9000-END-OF-FILE)                              
00184          END-EXEC.                                                
00185                                                                   
00186  1200-READNEXT-ERPNDT2.                                           
00187      EXEC CICS READNEXT                                           
00188          INTO     (PENDING-BUSINESS)                              
00189          DATASET  (ERPNDT2-FILE-ID)                               
00190          RIDFLD   (ERPNDT2-KEY)                                   
00191          END-EXEC.                                                
00192                                                                   
00193      IF PB-COMPANY-CD-A1 NOT = EDIT-COMPANY-CD                    
00194          GO TO 9000-END-OF-FILE.                                  
00195                                                                   
00196      IF PB-ENTRY-BATCH NOT = EDIT-BATCH                           
00197          GO TO 1200-READNEXT-ERPNDT2.                             
00198                                                                   
00199      MOVE PENDING-BUSINESS       TO WS-SAVE-PB-REC.               
00200                                                                   
00201      EJECT                                                        
00202                                                                   
00203      EXEC CICS ENDBR                                              
00204          DATASET  (ERPNDT2-FILE-ID)                               
00205          END-EXEC.                                                
00206                                                                   
00207      IF PB-BATCH-TRAILER                                          
00208                                                                   
00209          MOVE ERPNDT-FILE-ID     TO JP-FILE-ID                    
00210          MOVE 'D'                TO JP-RECORD-TYPE                
00211          MOVE WS-SAVE-PB-REC     TO JP-RECORD-AREA                
00212          COMPUTE JOURNAL-LENGTH = ERPNDT-LENGTH + 23              
00213                                                                   
00214          EXEC CICS DELETE                                         
00215              DATASET  (ERPNDT-FILE-ID)                            
00216              RIDFLD   (PB-CONTROL-PRIMARY)                        
00217              END-EXEC                                             
00218                                                                   
00219          IF JOURNAL-FILE-ID = ZERO                                
00220              GO TO 9050-END-OF-FILE                               
00221          ELSE                                                     
00222              PERFORM 8400-LOG-JOURNAL-RECORD THRU 8499-EXIT       
00223              GO TO 9050-END-OF-FILE.                              
00224                                                                   
00225      MOVE PB-COMPANY-CD-A1       TO WS-AC-COMPANY-CD.             
00226      MOVE PB-CARRIER             TO WS-AC-CARRIER.                
00227      MOVE PB-GROUPING            TO WS-AC-GROUPING.               
00228      MOVE PB-STATE               TO WS-AC-STATE.                  
00229      MOVE PB-ACCOUNT             TO WS-AC-ACCOUNT.                
00230                                                                   
00231      IF FIRST-TIME-THRU                                           
00232          MOVE SPACE              TO FIRST-BATCH-REC-SW            
00233          GO TO 2000-UPDATE-CNTL-FILE.                             
00234                                                                   
00235      IF WS-ACCOUNT-CONTROL = WS-SAVE-ACCOUNT-KEY                  
00236          GO TO 3000-PROCESS-ERPNDT2.                              
00237                                                                   
00238      PERFORM 5000-UPDATE-BATCH-TRAILER THRU 5099-EXIT.            
00239                                                                   
00240      EXEC CICS SYNCPOINT                                          
00241           END-EXEC.                                               
00242                                                                   
00243      EXEC CICS START                                              
00244          TRANSID       (EDIT-TRANS)                               
00245          FROM          (NEW-BATCH-TO-PROCESS)                     
00246          LENGTH        (CLEN)                                     
00247          END-EXEC.                                                
00248                                                                   
00249      EJECT                                                        
00250  2000-UPDATE-CNTL-FILE.                                           
00251      EXEC CICS HANDLE CONDITION                                   
00252          NOTFND        (9999-RETURN-CICS)                         
00253          NOTOPEN       (9999-RETURN-CICS)                         
00254          END-EXEC.                                                
00255                                                                   
00256      MOVE SPACES                 TO ELCNTL-KEY.                   
00257      MOVE '1'                    TO ELCNTL-REC-TYPE.              
00258      MOVE EDIT-COMPANY-ID        TO ELCNTL-COMPANY-ID.            
00259      MOVE +0                     TO ELCNTL-SEQ-NO.                
00260                                                                   
00261      EXEC CICS READ                                               
00262          DATASET   (ELCNTL-FILE-ID)                               
00263          SET       (ADDRESS OF CONTROL-FILE)                      
00264          RIDFLD    (ELCNTL-KEY)                                   
00265          UPDATE                                                   
00266          END-EXEC.                                                
00267                                                                   
00268      MOVE CF-JOURNAL-FILE-ID     TO JOURNAL-FILE-ID.              
00269      MOVE ELCNTL-FILE-ID         TO JP-FILE-ID.                   
00270      MOVE 'B'                    TO JP-RECORD-TYPE.               
00271      MOVE CONTROL-FILE           TO JP-RECORD-AREA.               
00272      COMPUTE JOURNAL-LENGTH = ELCNTL-LENGTH + 23.                 
00273                                                                   
00274      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8499-EXIT.              
00275                                                                   
00276      IF CF-LAST-BATCH-RESET                                       
00277          MOVE ZEROS              TO CF-LAST-BATCH-NO.             
00278                                                                   
00279      ADD +1                      TO CF-LAST-BATCH-NO.             
00280      MOVE CF-LAST-BATCH-NO       TO NEW-EDIT-BATCH.               
00281      MOVE CF-MAIL-PROCESSING     TO WS-MAIL-PROCESSING.           
00282                                                                   
00283      MOVE ELCNTL-FILE-ID         TO JP-FILE-ID.                   
00284      MOVE 'C'                    TO JP-RECORD-TYPE.               
00285      MOVE CONTROL-FILE           TO JP-RECORD-AREA.               
00286      COMPUTE JOURNAL-LENGTH = ELCNTL-LENGTH + 23.                 
00287                                                                   
00288      EXEC CICS REWRITE                                            
00289          DATASET   (ELCNTL-FILE-ID)                               
00290          FROM      (CONTROL-FILE)                                 
00291          END-EXEC.                                                
00292                                                                   
00293      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8499-EXIT.              
00294                                                                   
00295      EXEC CICS SYNCPOINT                                          
00296           END-EXEC.                                               
00297                                                                   
00298      EJECT                                                        
00299  2500-BUILD-BATCH-TRAILER.                                        
00300      MOVE WS-SAVE-PB-REC         TO PENDING-BUSINESS.             
00301      MOVE WS-ACCOUNT-CONTROL     TO WS-SAVE-ACCOUNT-KEY.          
00302      MOVE SPACES                 TO PB-RECORD-BODY.               
00303      MOVE NEW-EDIT-BATCH         TO PB-ENTRY-BATCH                
00304                                     PB-CERT-NO.                   
00305      MOVE 9999                   TO PB-BATCH-SEQ-NO.              
00306      MOVE HIGH-VALUES            TO PB-CERT-EFF-DT.               
00307      MOVE ZEROS                  TO PB-BATCH-CHG-SEQ-NO           
00308                                     PB-ALT-CHG-SEQ-NO.            
00309      MOVE '9'                    TO PB-RECORD-TYPE.               
00310      MOVE ZEROS                  TO PB-B-LF-ISS-PRM-REMITTED      
00311                                     PB-B-LF-ISS-PRM-ENTERED       
00312                                     PB-B-AH-ISS-PRM-REMITTED      
00313                                     PB-B-AH-ISS-PRM-ENTERED       
00314                                     PB-B-ISSUE-CNT-REMITTED       
00315                                     PB-B-ISSUE-CNT-ENTERED        
00316                                     PB-B-CANCEL-CNT-REMITTED      
00317                                     PB-B-CANCEL-CNT-ENTERED       
00318                                     PB-B-LF-CAN-PRM-REMITTED      
00319                                     PB-B-LF-CAN-PRM-ENTERED       
00320                                     PB-B-AH-CAN-PRM-REMITTED      
00321                                     PB-B-AH-CAN-PRM-ENTERED.      
00322      MOVE ZEROS                  TO PB-B-LF-ISS-PRM-COMPUTED      
00323                                     PB-B-LF-CAN-PRM-COMPUTED      
00324                                     PB-B-AH-ISS-PRM-COMPUTED      
00325                                     PB-B-AH-CAN-PRM-COMPUTED      
00326                                     PB-LF-BILLED-AMTS             
00327                                     PB-AH-BILLED-AMTS             
00328                                     PB-CHG-COUNT                  
00329                                     PB-CALC-TOLERANCE.            
00330      MOVE LOW-VALUES             TO PB-CREDIT-ACCEPT-DT           
00331                                     PB-BILLED-DT                  
00332                                     PB-ACCT-EFF-DT                
00333                                     PB-ACCT-EXP-DT.               
00334      MOVE ZEROS                  TO PB-B-HIGHEST-SEQ-NO.          
00335      MOVE PB-CONTROL-PRIMARY     TO WS-SAVE-BATCH-KEY.            
00336      MOVE PB-CONTROL-PRIMARY     TO PB-CONTROL-BY-ORIG-BATCH.     
00337                                                                   
00338      MOVE ERPNDB-FILE-ID         TO JP-FILE-ID.                   
00339      MOVE 'A'                    TO JP-RECORD-TYPE.               
00340      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
00341      COMPUTE JOURNAL-LENGTH = ERPNDB-LENGTH + 23.                 
00342                                                                   
00343      EXEC CICS WRITE                                              
00344          DATASET  (ERPNDB-FILE-ID)                                
00345          FROM     (PENDING-BUSINESS)                              
00346          RIDFLD   (PB-CONTROL-PRIMARY)                            
00347          END-EXEC.                                                
00348                                                                   
00349      MOVE 'Y'                    TO BATCH-TRAILER-SW.             
00350                                                                   
00351      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8499-EXIT.              
00352                                                                   
00353      EJECT                                                        
00354  3000-PROCESS-ERPNDT2.                                            
00355      MOVE WS-SAVE-PB-REC         TO PENDING-BUSINESS.             
00356                                                                   
00357      MOVE PB-CONTROL-PRIMARY     TO WS-SAVE-CONTROL-PRIMARY.      
00358      MOVE PB-CONTROL-PRIMARY     TO PB-CONTROL-BY-ORIG-BATCH.     
00359                                                                   
00360      MOVE NEW-EDIT-BATCH         TO PB-ENTRY-BATCH.               
00361      MOVE WS-BATCH-SEQ-NO        TO PB-BATCH-SEQ-NO.              
00362                                                                   
00363      IF PB-ISSUE                                                  
00364          ADD PB-I-LF-PREMIUM-AMT TO WS-LF-ISS-ENTERED             
00365          ADD PB-I-AH-PREMIUM-AMT TO WS-AH-ISS-ENTERED             
00366          ADD +1                  TO WS-ISS-CNT-ENTERED            
00367      ELSE                                                         
00368          ADD PB-C-LF-CANCEL-AMT  TO WS-LF-CAN-ENTERED             
00369          ADD PB-C-AH-CANCEL-AMT  TO WS-AH-CAN-ENTERED             
00370          ADD +1                  TO WS-CAN-CNT-ENTERED.           
00371                                                                   
00372      MOVE ERPNDB-FILE-ID         TO JP-FILE-ID.                   
00373      MOVE 'A'                    TO JP-RECORD-TYPE.               
00374      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
00375      COMPUTE JOURNAL-LENGTH = ERPNDB-LENGTH + 23.                 
00376                                                                   
00377      EXEC CICS HANDLE CONDITION                                   
00378          DUPREC   (3020-DUPREC)                                   
00379          END-EXEC.                                                
00380                                                                   
00381  3010-WRITE-PENDING-BUS-REC.                                      
00382      EXEC CICS WRITE                                              
00383          DATASET  (ERPNDB-FILE-ID)                                
00384          FROM     (PENDING-BUSINESS)                              
00385          RIDFLD   (PB-CONTROL-PRIMARY)                            
00386          END-EXEC.                                                
00387                                                                   
00388      GO TO 3030-DELETE-TEMP-BUS-REC.                              
00389                                                                   
00390  3020-DUPREC.                                                     
00391      IF PB-CERT-SFX NOT = '@'                                     
00392         MOVE '@'                 TO PB-CERT-SFX                   
00393         GO TO 3010-WRITE-PENDING-BUS-REC.                         
00394                                                                   
00395  3030-DELETE-TEMP-BUS-REC.                                        
00396      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8499-EXIT.              
00397                                                                   
00398      MOVE ERPNDT-FILE-ID         TO JP-FILE-ID.                   
00399      MOVE 'D'                    TO JP-RECORD-TYPE.               
00400      MOVE WS-SAVE-PB-REC         TO JP-RECORD-AREA.               
00401      COMPUTE JOURNAL-LENGTH = ERPNDT-LENGTH + 23.                 
00402                                                                   
00403      EXEC CICS DELETE                                             
00404          DATASET  (ERPNDT-FILE-ID)                                
00405          RIDFLD   (WS-SAVE-CONTROL-PRIMARY)                       
00406          END-EXEC.                                                
00407                                                                   
00408      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8499-EXIT.              
00409                                                                   
00410      IF NOT PB-ISSUE                                              
00411          GO TO 3100-SKIP-ERPNDM.                                  
00412                                                                   
00413      IF NOT MAIL-PROC                                             
00414          GO TO 3100-SKIP-ERPNDM.                                  
00415                                                                   
00416      EXEC CICS HANDLE CONDITION                                   
00417          NOTFND   (3100-SKIP-ERPNDM)                              
00418          ENDFILE  (3100-SKIP-ERPNDM)                              
00419          END-EXEC.                                                
00420                                                                   
00421      EXEC CICS READ                                               
00422          DATASET  (ERPNDM-FILE-ID)                                
00423          SET      (ADDRESS OF PENDING-MAILING-DATA)               
00424          RIDFLD   (WS-SAVE-CONTROL-PRIMARY)                       
00425          END-EXEC.                                                
00426                                                                   
00427      MOVE PENDING-MAILING-DATA   TO WS-SAVE-PM-REC.               
00428                                                                   
00429      MOVE NEW-EDIT-BATCH         TO PM-ENTRY-BATCH.               
00430      MOVE WS-BATCH-SEQ-NO        TO PM-BATCH-SEQ-NO.              
00431                                                                   
00432      MOVE ERPNDM-FILE-ID         TO JP-FILE-ID.                   
00433      MOVE 'A'                    TO JP-RECORD-TYPE.               
00434      MOVE PENDING-MAILING-DATA   TO JP-RECORD-AREA.               
00435      COMPUTE JOURNAL-LENGTH = ERPNDM-LENGTH + 23.                 
00436                                                                   
00437      EXEC CICS WRITE                                              
00438          DATASET  (ERPNDM-FILE-ID)                                
00439          FROM     (PENDING-MAILING-DATA)                          
00440          RIDFLD   (PM-CONTROL-PRIMARY)                            
00441          END-EXEC.                                                
00442                                                                   
00443      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8499-EXIT.              
00444                                                                   
00445      MOVE ERPNDM-FILE-ID         TO JP-FILE-ID.                   
00446      MOVE 'D'                    TO JP-RECORD-TYPE.               
00447      MOVE WS-SAVE-PM-REC         TO JP-RECORD-AREA.               
00448      COMPUTE JOURNAL-LENGTH = ERPNDM-LENGTH + 23.                 
00449                                                                   
00450      EXEC CICS DELETE                                             
00451          DATASET  (ERPNDM-FILE-ID)                                
00452          RIDFLD   (WS-SAVE-CONTROL-PRIMARY)                       
00453          END-EXEC.                                                
00454                                                                   
00455      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8499-EXIT.              
00456                                                                   
00457  3100-SKIP-ERPNDM.                                                
00458      ADD +1                      TO WS-BATCH-SEQ-NO.              
00459                                                                   
00460      GO TO 1000-STARTBR-ERPNDT2.                                  
00461                                                                   
00462      EJECT                                                        
00463  5000-UPDATE-BATCH-TRAILER.                                       
00464      EXEC CICS READ                                               
00465          DATASET  (ERPNDB-FILE-ID)                                
00466          INTO     (PENDING-BUSINESS)                              
00467          RIDFLD   (WS-SAVE-BATCH-KEY)                             
00468          UPDATE                                                   
00469          END-EXEC.                                                
00470                                                                   
00471      MOVE ERPNDB-FILE-ID         TO JP-FILE-ID.                   
00472      MOVE 'B'                    TO JP-RECORD-TYPE.               
00473      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
00474      COMPUTE JOURNAL-LENGTH = ERPNDB-LENGTH + 23.                 
00475                                                                   
00476      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8499-EXIT.              
00477                                                                   
00478      MOVE WS-LF-ISS-ENTERED      TO PB-B-LF-ISS-PRM-ENTERED.      
00479      MOVE WS-AH-ISS-ENTERED      TO PB-B-AH-ISS-PRM-ENTERED.      
00480      MOVE WS-ISS-CNT-ENTERED     TO PB-B-ISSUE-CNT-ENTERED.       
00481      MOVE WS-CAN-CNT-ENTERED     TO PB-B-CANCEL-CNT-ENTERED.      
00482      MOVE WS-LF-CAN-ENTERED      TO PB-B-LF-CAN-PRM-ENTERED.      
00483      MOVE WS-AH-CAN-ENTERED      TO PB-B-AH-CAN-PRM-ENTERED.      
00484      MOVE WS-BATCH-SEQ-NO        TO PB-B-HIGHEST-SEQ-NO.          
00485                                                                   
00486      MOVE ERPNDB-FILE-ID         TO JP-FILE-ID.                   
00487      MOVE 'C'                    TO JP-RECORD-TYPE.               
00488      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               
00489      COMPUTE JOURNAL-LENGTH = ERPNDB-LENGTH + 23.                 
00490                                                                   
00491      EXEC CICS REWRITE                                            
00492          DATASET  (ERPNDB-FILE-ID)                                
00493          FROM     (PENDING-BUSINESS)                              
00494          END-EXEC.                                                
00495                                                                   
00496      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8499-EXIT.              
00497                                                                   
00498      MOVE ZEROS                  TO WS-LF-ISS-ENTERED             
00499                                     WS-AH-ISS-ENTERED             
00500                                     WS-ISS-CNT-ENTERED            
00501                                     WS-CAN-CNT-ENTERED            
00502                                     WS-LF-CAN-ENTERED             
00503                                     WS-AH-CAN-ENTERED.            
00504      MOVE +1                     TO WS-BATCH-SEQ-NO.              
00505                                                                   
00506  5099-EXIT. EXIT.                                                 
00507                                                                   
00508      EJECT                                                        
00509  8400-LOG-JOURNAL-RECORD.                                         
00510      MOVE EIBTRNID               TO JP-USER-ID.                   
00511      MOVE THIS-PGM               TO JP-PROGRAM-ID.                
00512 *    EXEC CICS JOURNAL                                            
00513 *        JFILEID     (JOURNAL-FILE-ID)                            
00514 *        JTYPEID     ('CL')                                       
00515 *        FROM        (JOURNAL-RECORD)                             
00516 *        LENGTH      (JOURNAL-LENGTH)                             
00517 *        END-EXEC.                                                
00518                                                                   
00519                                                                   
00520  8499-EXIT. EXIT.                                                 
00521                                                                   
00522      EJECT                                                        
00523  9000-END-OF-FILE.                                                
00524      EXEC CICS ENDBR                                              
00525          DATASET  (ERPNDT2-FILE-ID)                               
00526          END-EXEC.                                                
00527                                                                   
00528  9050-END-OF-FILE.                                                
00529      IF BATCH-TRAILER-BUILT                                       
00530          PERFORM 5000-UPDATE-BATCH-TRAILER THRU 5099-EXIT         
00531                                                                   
00532          EXEC CICS START                                          
00533              TRANSID       (EDIT-TRANS)                           
00534              FROM          (NEW-BATCH-TO-PROCESS)                 
00535              LENGTH        (CLEN)                                 
00536              END-EXEC.                                            
00537                                                                   
00538      GO TO 0000-RETRIEVE.                                         
00539                                                                   
00540  9099-EXIT.                                                       
00541      EXIT.                                                        
00542                                                                   
00543  9999-ABEND.                                                      
00544      EXEC CICS ABEND                                              
00545          ABCODE(TRANS-ID)  END-EXEC.                              
00546                                                                   
00547  9999-RETURN-CICS.                                                
00548      EXEC CICS  RETURN                                            
00549          END-EXEC.                                                
00550                                                                   
00551  9999-EXIT. EXIT.                                                 
