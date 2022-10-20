00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL346 .                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 06/22/95 15:48:33.                 
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            
00008 *                            VMOD=2.003.                          
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
00026 *    EL346 WILL PURGE CLAIMS FROM THE RETREIVE FILE (ELRETR),     
00027 *    TRAILER (ELTRLR), CHECK QUE (ELCHKQ), ARCHIEVE (ELARCH),     
00028 *    ACTIVITY (ELACTQ), AND WILL REMOVE THE CLAIM FLAG FROM       
00029 *    THE CERTIFICATE (ELCERT) THAT HAVE BEEN CLOSED FOR MORE      
00030 *    MORE MONTHS THAN INDICATED IN THE CONTROL FILE               
00031 *    (MONTHS-BEFORE-PURGE), HOWEVER, IF THE CLAIM IS ON THE       
00032 *    CLAIM MASTER IT WILL NOT BE PURGED.                          
00033                                                                   
00034 *    INPUT FILES  - ELCNTL - CONTROL FILE                         
00035 *                   ELRETR - RETRIEVE FILE                        
00036 *                   ELMSTR - CLAIM MASTER                         
00037 *                   ELACTQ - ACTIVITY  FILE                       
00038 *                   ELCHKQ - CHECK QUE FILE                       
00039 *                                                                 
00040                                                                   
00041      EJECT                                                        
00042  ENVIRONMENT DIVISION.                                            
00043                                                                   
00044  INPUT-OUTPUT SECTION.                                            
00045                                                                   
00046  FILE-CONTROL.                                                    
00047                                                                   
00048      SELECT ELRETR           ASSIGN TO SYS024-FBA1-ELRETR         
00049                              ORGANIZATION IS INDEXED              
00050                              ACCESS IS DYNAMIC                    
00051                              RECORD KEY IS RL-CONTROL-PRIMARY     
00052                              FILE STATUS IS ELRETR-FILE-STATUS.   
00053                                                                   
00054      SELECT ELCNTL           ASSIGN TO SYS021-FBA1-ELCNTL         
00055                              ORGANIZATION IS INDEXED              
00056                              ACCESS IS DYNAMIC                    
00057                              RECORD KEY IS CF-CONTROL-PRIMARY     
00058                              FILE STATUS IS ELCNTL-FILE-STATUS.   
00059                                                                   
00060      SELECT ELMSTR           ASSIGN TO SYS023-FBA1-ELMSTR         
00061                              ORGANIZATION IS INDEXED              
00062                              ACCESS IS DYNAMIC                    
00063                              RECORD KEY IS CL-CONTROL-PRIMARY     
00064                              FILE STATUS IS ELMSTR-FILE-STATUS.   
00065                                                                   
00066      SELECT ELTRLR           ASSIGN TO SYS025-FBA1-ELTRLR         
00067                              ORGANIZATION IS INDEXED              
00068                              ACCESS IS DYNAMIC                    
00069                              RECORD KEY IS AT-CONTROL-PRIMARY     
00070                              FILE STATUS IS ELTRLR-FILE-STATUS.   
00071                                                                   
00072      SELECT ELARCH           ASSIGN TO SYS026-FBA1-ELARCH         
00073                              ORGANIZATION IS INDEXED              
00074                              ACCESS IS DYNAMIC                    
00075                              RECORD KEY IS LA-CONTROL-PRIMARY     
00076                              FILE STATUS IS ELARCH-FILE-STATUS.   
00077                                                                   
00078      SELECT ELACTQ           ASSIGN TO SYS029-FBA1-ELACTQ         
00079                              ORGANIZATION IS INDEXED              
00080                              ACCESS IS DYNAMIC                    
00081                              RECORD KEY IS AQ-CONTROL-PRIMARY     
00082                              FILE STATUS IS ELACTQ-FILE-STATUS.   
00083                                                                   
00084      SELECT ELCHKQ           ASSIGN TO SYS027-FBA1-ELCHKQ         
00085                              ORGANIZATION IS INDEXED              
00086                              ACCESS IS DYNAMIC                    
00087                              RECORD KEY IS CQ-CONTROL-PRIMARY     
00088                              FILE STATUS IS ELCHKQ-FILE-STATUS.   
00089                                                                   
00090      SELECT ELCERT           ASSIGN TO SYS028-FBA1-ELCERT         
00091                              ORGANIZATION IS INDEXED              
00092                              ACCESS IS DYNAMIC                    
00093                              RECORD KEY IS CM-CONTROL-PRIMARY     
00094                              FILE STATUS IS ELCERT-FILE-STATUS.   
00095                                                                   
00096      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   
00097                                                                   
00098      EJECT                                                        
00099  DATA DIVISION.                                                   
00100                                                                   
00101  FILE SECTION.                                                    
00102                                                                   
00103  FD  ELCNTL.                                                      
00104                                                                   
00105                                  COPY ELCCNTL.                    
00106      EJECT                                                        
00107  FD  ELMSTR.                                                      
00108                                                                   
00109                                  COPY ELCMSTR.                    
00110      EJECT                                                        
00111                                                                   
00112  FD  ELRETR.                                                      
00113                                                                   
00114                                  COPY ELCRETR.                    
00115      EJECT                                                        
00116                                                                   
00117                                                                   
00118  FD  ELTRLR.                                                      
00119                                                                   
00120                                  COPY ELCTRLR.                    
00121      EJECT                                                        
00122                                                                   
00123  FD  ELARCH.                                                      
00124                                                                   
00125                                  COPY ELCARCH.                    
00126      EJECT                                                        
00127                                                                   
00128  FD  ELACTQ.                                                      
00129                                                                   
00130                                  COPY ELCACTQ.                    
00131      EJECT                                                        
00132                                                                   
00133  FD  ELCHKQ.                                                      
00134                                                                   
00135                                  COPY ELCCHKQ.                    
00136      EJECT                                                        
00137                                                                   
00138  FD  ELCERT.                                                      
00139                                                                   
00140                                  COPY ELCCERT.                    
00141      EJECT                                                        
00142                                                                   
00143  FD  PRNTR                       COPY ELCPRTFD.                   
00144      EJECT                                                        
00145                                                                   
00146  WORKING-STORAGE SECTION.                                         
00147  77  FILLER  PIC X(32)   VALUE '********************************'.
00148  77  FILLER  PIC X(32)   VALUE '*     EL346  WORKING STORAGE   *'.
00149  77  FILLER   PIC X(32) VALUE  '******** VMOD=2.003 ************'.
00150                                                                   
00151  77  PGM-SUB                         PIC S999 COMP-3 VALUE +346.  
00152                                                                   
00153  01  SWITCHES.                                                    
00154      05  DETAIL-PRINTED-SW           PIC X       VALUE 'N'.       
00155          88  DETAIL-PRINTED                      VALUE 'Y'.       
00156      05  READNEXT-COMP-SW            PIC X       VALUE 'N'.       
00157          88  READNEXT-COMP                       VALUE 'Y'.       
00158      05  READNEXT-CARR-SW            PIC X       VALUE 'N'.       
00159          88  READNEXT-CARR                       VALUE 'Y'.       
00160      05  FIRST-TIME-SW               PIC X       VALUE 'Y'.       
00161          88  FIRST-TIME                          VALUE 'Y'.       
00162      05  CLAIM-MASTER-SW             PIC X       VALUE 'N'.       
00163          88  HAS-CLAIM-MASTER                    VALUE 'Y'.       
00164      05  PURGE-FILE-SW               PIC X       VALUE 'N'.       
00165          88  PURGE-FILES                         VALUE 'Y'.       
00166      05  DELETE-RETR-SW              PIC X       VALUE 'N'.       
00167          88  DELETE-RETR                         VALUE 'Y'.       
00168      05  CERT-FOUND-SW               PIC X       VALUE 'N'.       
00169          88  CERT-FOUND                          VALUE 'Y'.       
00170          88  CERT-NOT-FOUND                      VALUE 'N'.       
00171      05  CERT-REWRITE-SW             PIC X       VALUE 'N'.       
00172          88  REWRITE-CERT                        VALUE 'Y'.       
00173      05  END-OF-CNTL-SW              PIC X       VALUE 'N'.       
00174          88  END-OF-CNTL                         VALUE 'Y'.       
00175      05  END-OF-JOB-SW               PIC X       VALUE 'N'.       
00176          88  END-OF-JOB                          VALUE 'Y'.       
00177                                                                   
00178  01  MISC-ITEMS.                                                  
00179      05  WS-ABEND-MESSAGE            PIC X(80)   VALUE SPACES.    
00180      05  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZERO.      
00181      05  WS-RETURN-CODE              PIC S9(4)   COMP.            
00182      05  WS-ZERO                     PIC S9      VALUE ZERO.      
00183      05  X                           PIC X.                       
00184      05  LN-CNT                      PIC S99     VALUE +99.       
00185      05  LN-MAX                      PIC S99     VALUE +56.       
00186      05  PAGE-CNT                    PIC S9(5)   VALUE +0.        
00187      05  NINES                       PIC X(20)                    
00188          VALUE '99999999999999999999'.                            
00189                                                                   
00190      05  ELRETR-FILE-STATUS          PIC XX      VALUE ZERO.      
00191      05  ELCNTL-FILE-STATUS          PIC XX      VALUE ZERO.      
00192      05  ELMSTR-FILE-STATUS          PIC XX      VALUE ZERO.      
00193      05  ELCERT-FILE-STATUS          PIC XX      VALUE ZERO.      
00194      05  ELTRLR-FILE-STATUS          PIC XX      VALUE ZERO.      
00195      05  ELARCH-FILE-STATUS          PIC XX      VALUE ZERO.      
00196      05  ELACTQ-FILE-STATUS          PIC XX      VALUE ZERO.      
00197      05  ELCHKQ-FILE-STATUS          PIC XX      VALUE ZERO.      
00198                                                                   
00199      05  WS-DISPLAY-TIME             PIC 99B99B99.                
00200      05  WS-THIS-TIME-OF-DAY.                                     
00201          10  WS-TIME                 PIC 9(6).                    
00202          10  WS-HUN-SEC              PIC 99.                      
00203      05  WS-ACCEPT-DATE              PIC 9(06).                   
00204      05  WS-ACCEPT-DATE-CEN          PIC 9(08).                   
00205      05  WS-ACCEPT-DATE-CEN-R  REDEFINES WS-ACCEPT-DATE-CEN.      
00206          10  WS-AD-CCYY              PIC 9(04).                   
00207          10  WS-AD-CCYR  REDEFINES WS-AD-CCYY.                    
00208              15  WS-AD-CC            PIC 99.                      
00209              15  WS-AD-YY            PIC 99.                      
00210          10  WS-AD-MM                PIC 99.                      
00211          10  WS-AD-DD                PIC 99.                      
00212      05  WS-PREV-CO-CD               PIC X       VALUE SPACES.    
00213      05  WS-COMPANY-CD               PIC X       VALUE SPACES.    
00214      05  WS-PREV-COMPANY-CD          PIC X       VALUE SPACES.    
00215      05  WS-COMPANY-ID               PIC XXX     VALUE SPACES.    
00216      05  WS-CARRIER-CONTROL          PIC X       VALUE SPACES.    
00217      05  WS-LAST-CARRIER             PIC X       VALUE SPACES.    
00218      05  WS-LAST-CLAIM-NO            PIC X(7)    VALUE SPACES.    
00219      05  WS-MONTHS-BEFORE-PURGED     PIC S99     VALUE ZEROS.     
00220      05  WS-CURRENT-BIN-DT           PIC XX      VALUE SPACES.    
00221      05  WS-AS-OF-BIN-DT             PIC XX      VALUE SPACES.    
00222      05  WS-PURGE-DT                 PIC XX      VALUE SPACES.    
00223      05  WS-DEFAULT-PURGE-DT         PIC XX      VALUE SPACES.    
00224      05  WS-EFFECTIVE-DATE           PIC 9(08).                   
00225      05  WS-EFFECTIVE-DATE-R  REDEFINES WS-EFFECTIVE-DATE.        
00226          10  WS-CCYY                 PIC 9(04).                   
00227          10  WS-CCYR REDEFINES  WS-CCYY.                          
00228              15  WS-CC               PIC 99.                      
00229              15  WS-YR               PIC 99.                      
00230          10  WS-MM                   PIC 99.                      
00231          10  WS-DD                   PIC 99.                      
00232      05  WS-WORK-TERM                PIC S999    VALUE ZEROS.     
00233      05  WS-EXPIRE-MOS               PIC S9(9)   VALUE ZEROS.     
00234      05  WS-CURRENT-MOS              PIC S9(9)   VALUE ZEROS.     
00235      05  WS-SAVE-CLAIM-KEY           PIC X(20)   VALUE SPACES.    
00236      05  WS-ASSOC-CLAIM              PIC X(7)    VALUE SPACES.    
00237      05  WS-SAVE-PRT                 PIC X(133)  VALUE SPACES.    
00238      05  WS-SAVE-ELRETR              PIC X(350)  VALUE SPACES.    
00239                                                                   
00240  01  WS-HEADING1.                                                 
00241      05  FILLER                      PIC X       VALUE '1'.       
00242      05  FILLER                      PIC X(41)   VALUE SPACES.    
00243      05  FILLER                      PIC X(50)   VALUE            
00244          'CLAIM PURGE AND DELETE TO ELRETR AND TRAILER FILES'.    
00245      05  FILLER                      PIC X(28)   VALUE SPACES.    
00246      05  FILLER                      PIC X(5)    VALUE 'EL346'.   
00247      05  FILLER                      PIC X(8)    VALUE SPACES.    
00248                                                                   
00249  01  WS-HEADING2.                                                 
00250      05  FILLER                      PIC X       VALUE SPACES.    
00251      05  WS-H2-COMPANY-ID            PIC XXX     VALUE SPACES.    
00252      05  FILLER                      PIC X(48)   VALUE SPACES.    
00253      05  WS-H2-COMPANY-NAME          PIC X(30)   VALUE SPACES.    
00254      05  FILLER                      PIC X(38)   VALUE SPACES.    
00255      05  FILLER                      PIC X(5)    VALUE 'PAGE '.   
00256      05  WS-H2-PAGE-NO               PIC ZZ,ZZ9.                  
00257      05  FILLER                      PIC X       VALUE SPACES.    
00258                                                                   
00259  01  WS-HEADING3.                                                 
00260      05  FILLER                      PIC X(57)   VALUE SPACES.    
00261      05  FILLER                      PIC X(11)                    
00262                                      VALUE  'AS OF DATE '.        
00263      05  WS-H3-AS-OF-DATE            PIC X(08)   VALUE SPACES.    
00264      05  FILLER                      PIC X(44)   VALUE SPACES.    
00265      05  WS-H3-CURRENT-DATE          PIC X(8)    VALUE SPACES.    
00266      05  FILLER                      PIC X(5)    VALUE SPACES.    
00267                                                                   
00268  01  WS-HEADING4.                                                 
00269      05  FILLER                      PIC X(133)  VALUE            
00270          '0          CLAIM      CERTIFICATE   EFFECTIVE'.         
00271                                                                   
00272  01  WS-HEADING5.                                                 
00273      05  FILLER                      PIC X(72)   VALUE            
00274          ' CARRIER   NUMBER       NUMBER        DATE         INSUR
00275 -        'ED'.                                                    
00276      05  FILLER                      PIC X(61)   VALUE            
00277          '  MESSAGE'.                                             
00278                                                                   
00279  01  WS-DETAIL1.                                                  
00280      05  WS-D1-CC                    PIC X.                       
00281      05  FILLER                      PIC XXX.                     
00282      05  WS-D1-CARRIER               PIC X.                       
00283      05  FILLER                      PIC X(6).                    
00284      05  WS-D1-CLAIM-NO              PIC X(7).                    
00285      05  FILLER                      PIC X(6).                    
00286      05  WS-D1-CERT-NO               PIC X(11).                   
00287      05  FILLER                      PIC X(4).                    
00288      05  WS-D1-CERT-EFF-DT           PIC X(8).                    
00289      05  FILLER                      PIC XXX.                     
00290      05  WS-D1-INSURED-NAME          PIC X(20).                   
00291      05  FILLER                      PIC X(4).                    
00292      05  WS-D1-MESSAGE               PIC X(30).                   
00293      05  WS-D2-MESSAGE               PIC X(27).                   
00294                                                                   
00295                                                                   
00296  01  WS-TOTAL-LINE.                                               
00297      05  WS-TOT-CNTL                 PIC X.                       
00298      05  WS-TOT-DESC                 PIC X(30).                   
00299      05  FILLER                      PIC X.                       
00300      05  WS-TOT-COUNT                PIC ZZZ,ZZZ,ZZ9.             
00301                                                                   
00302      COPY  ELCNWA.                                                
00303                                                                   
00304  01  RECORD-COUNTS  COMP-3.                                       
00305      05  RETR-READ-TEMP              PIC S9(7)  VALUE ZEROS.      
00306      05  RETR-READ-CO                PIC S9(7)  VALUE ZEROS.      
00307      05  RETR-PURGED-CO              PIC S9(7)  VALUE ZEROS.      
00308      05  RETR-DELETED-CO             PIC S9(7)  VALUE ZEROS.      
00309      05  RETR-REWRITTEN-CO           PIC S9(7)  VALUE ZEROS.      
00310      05  TRLR-DELETED-CO             PIC S9(7)  VALUE ZEROS.      
00311      05  CHKQ-DELETED-CO             PIC S9(7)  VALUE ZEROS.      
00312      05  ARCH-DELETED-CO             PIC S9(7)  VALUE ZEROS.      
00313      05  ACTQ-DELETED-CO             PIC S9(7)  VALUE ZEROS.      
00314      05  CERT-REWRITTEN-CO           PIC S9(7)  VALUE ZEROS.      
00315  01  RECORD-COUNTS  COMP-3.                                       
00316      05  RETR-READ-FINAL             PIC S9(7)  VALUE ZEROS.      
00317      05  RETR-PURGED-FINAL           PIC S9(7)  VALUE ZEROS.      
00318      05  RETR-DELETED-FINAL          PIC S9(7)  VALUE ZEROS.      
00319      05  RETR-REWRITTEN-FINAL        PIC S9(7)  VALUE ZEROS.      
00320      05  TRLR-DELETED-FINAL          PIC S9(7)  VALUE ZEROS.      
00321      05  CHKQ-DELETED-FINAL          PIC S9(7)  VALUE ZEROS.      
00322      05  ARCH-DELETED-FINAL          PIC S9(7)  VALUE ZEROS.      
00323      05  ACTQ-DELETED-FINAL          PIC S9(7)  VALUE ZEROS.      
00324      05  CERT-REWRITTEN-FINAL        PIC S9(7)  VALUE ZEROS.      
00325                                                                   
00326                                  COPY ELCDATE.                    
00327      EJECT                                                        
00328                                                                   
00329      EJECT                                                        
00330  PROCEDURE DIVISION.                                              
00331                                                                   
00332  0000-MAIN-LOGIC SECTION.                                         
00333                                                                   
00334      PERFORM 0000-DATE-PROCESS.                                   
00335                                                                   
00336      PERFORM 0100-OPEN-FILES.                                     
00337                                                                   
00338      PERFORM 1000-PROCESS-CNTL.                                   
00339                                                                   
00340      PERFORM 2000-PROCESS-RETRIEVES                               
00341              UNTIL END-OF-CNTL.                                   
00342                                                                   
00343      PERFORM 7000-END-RUN.                                        
00344                                                                   
00345      PERFORM 8900-CLOSE-FILES.                                    
00346                                                                   
00347      GOBACK.                                                      
00348                                                                   
00349 ***************************************************************** 
00350 *                        PROCESS RUN DATE                       * 
00351 *****************************************************************.
00352  0000-DATE-PROCESS SECTION.                                       
00353                                                                   
00354      DISPLAY '******* START OF MESSAGES FROM EL346 *********'.    
00355                                                                   
CIDMOD*    ACCEPT WS-ACCEPT-DATE.                                       
CIDMOD     ACCEPT WS-ACCEPT-DATE-CEN FROM SYSIN.                        
CIDMOD     MOVE   WS-ACCEPT-DATE-CEN TO WS-ACCEPT-DATE.                 
00357                                                                   
CIDMOD     DISPLAY ' WS-ACCEPT-DATE-CEN - ' WS-ACCEPT-DATE-CEN.         
CIDMOD                                                                  
00358      IF  WS-ACCEPT-DATE GREATER THAN SPACES                       
00359          DISPLAY ' ACCEPT-DATE - ' WS-ACCEPT-DATE                 
00360          MOVE WS-ACCEPT-DATE     TO  DC-GREG-DATE-1-YMD           
00361          MOVE '3'                TO  DC-OPTION-CODE               
00362          PERFORM 8500-DATE-CONVERSION  THRU  8500-EXIT            
00363          MOVE DC-BIN-DATE-1      TO  WS-AS-OF-BIN-DT              
00364          MOVE DC-GREG-DATE-1-EDIT                                 
00365                                  TO  WS-H3-AS-OF-DATE             
00366          ACCEPT WS-ACCEPT-DATE FROM DATE                          
00367          MOVE WS-ACCEPT-DATE     TO  DC-GREG-DATE-1-YMD           
00368                                      WS-ACCEPT-DATE-CEN           
00369          MOVE '3'                TO  DC-OPTION-CODE               
00370          PERFORM 8500-DATE-CONVERSION  THRU  8500-EXIT            
00371          MOVE DC-BIN-DATE-1      TO  WS-CURRENT-BIN-DT            
00372          MOVE DC-GREG-DATE-1-EDIT                                 
00373                                  TO  WS-H3-CURRENT-DATE           
00374          MOVE DC-ALPHA-CEN-N     TO  WS-AD-CC.                    
00375                                                                   
00376      IF  WS-ACCEPT-DATE NOT GREATER THAN SPACES                   
00377          ACCEPT WS-ACCEPT-DATE FROM DATE                          
00378          MOVE WS-ACCEPT-DATE     TO  DC-GREG-DATE-1-YMD           
00379                                      WS-ACCEPT-DATE-CEN           
00380          MOVE '3'                TO  DC-OPTION-CODE               
00381          PERFORM 8500-DATE-CONVERSION  THRU  8500-EXIT            
00382          MOVE DC-BIN-DATE-1      TO  WS-AS-OF-BIN-DT              
00383                                      WS-CURRENT-BIN-DT            
00384          MOVE DC-GREG-DATE-1-EDIT                                 
00385                                  TO  WS-H3-AS-OF-DATE             
00386                                      WS-H3-CURRENT-DATE           
00387          MOVE DC-ALPHA-CEN-N     TO  WS-AD-CC.                    
00388                                                                   
00389                                                                   
00390      COMPUTE WS-CURRENT-MOS   EQUAL (WS-AD-CCYY * 12) +           
00391                                               WS-AD-MM.           
00392      MOVE WS-AS-OF-BIN-DT        TO  DC-BIN-DATE-1.               
00393      MOVE -1                     TO  DC-ELAPSED-MONTHS.           
00394      MOVE ZEROS                  TO  DC-ELAPSED-DAYS.             
00395      MOVE '6'                    TO  DC-OPTION-CODE.              
00396      PERFORM 8500-DATE-CONVERSION                                 
00397      MOVE DC-BIN-DATE-2          TO  WS-DEFAULT-PURGE-DT.         
00398                                                                   
00399 ***************************************************************** 
00400 *                          OPENS  FILES                         * 
00401 *****************************************************************.
00402  0100-OPEN-FILES SECTION.                                         
00403                                                                   
00404      OPEN I-O    ELRETR                                           
00405                  ELCERT                                           
00406                  ELTRLR                                           
00407                  ELCHKQ                                           
00408                  ELARCH                                           
00409                  ELACTQ.                                          
00410      OPEN INPUT  ELCNTL                                           
00411                  ELMSTR.                                          
00412      OPEN OUTPUT PRNTR.                                           
00413                                                                   
00414                                                                   
00415      IF ELRETR-FILE-STATUS  = '00' OR '97'                        
00416          NEXT SENTENCE                                            
00417      ELSE                                                         
00418          MOVE 'ERROR OCCURRED OPEN - ELRETR'                      
00419                                  TO  WS-ABEND-MESSAGE             
00420          MOVE ELRETR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00421          PERFORM ABEND-PGM.                                       
00422                                                                   
00423      IF ELCERT-FILE-STATUS  = '00' OR '97'                        
00424          NEXT SENTENCE                                            
00425      ELSE                                                         
00426          MOVE 'ERROR OCCURRED OPEN - ELCERT'                      
00427                                  TO  WS-ABEND-MESSAGE             
00428          MOVE ELCERT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00429          PERFORM ABEND-PGM.                                       
00430                                                                   
00431      IF ELTRLR-FILE-STATUS  = '00' OR '97'                        
00432          NEXT SENTENCE                                            
00433      ELSE                                                         
00434          MOVE 'ERROR OCCURRED OPEN - ELTRLR'                      
00435                                  TO  WS-ABEND-MESSAGE             
00436          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00437          PERFORM ABEND-PGM.                                       
00438                                                                   
00439      IF ELCHKQ-FILE-STATUS  = '00' OR '97'                        
00440          NEXT SENTENCE                                            
00441      ELSE                                                         
00442          MOVE 'ERROR OCCURRED OPEN - ELCHKQ'                      
00443                                  TO  WS-ABEND-MESSAGE             
00444          MOVE ELCHKQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00445          PERFORM ABEND-PGM.                                       
00446                                                                   
00447      IF ELARCH-FILE-STATUS  = '00' OR '97'                        
00448          NEXT SENTENCE                                            
00449      ELSE                                                         
00450          MOVE 'ERROR OCCURRED OPEN - ELARCH'                      
00451                                  TO  WS-ABEND-MESSAGE             
00452          MOVE ELARCH-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00453          PERFORM ABEND-PGM.                                       
00454                                                                   
00455      IF ELACTQ-FILE-STATUS  = '00' OR '97'                        
00456          NEXT SENTENCE                                            
00457      ELSE                                                         
00458          MOVE 'ERROR OCCURRED OPEN - ELACTQ'                      
00459                                  TO  WS-ABEND-MESSAGE             
00460          MOVE ELACTQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00461          PERFORM ABEND-PGM.                                       
00462                                                                   
00463      IF ELCNTL-FILE-STATUS  = '00' OR '97'                        
00464          NEXT SENTENCE                                            
00465      ELSE                                                         
00466          MOVE 'ERROR OCCURRED OPEN - ELCNTL'                      
00467                                  TO  WS-ABEND-MESSAGE             
00468          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00469          PERFORM ABEND-PGM.                                       
00470                                                                   
00471      IF ELMSTR-FILE-STATUS  = '00' OR '97'                        
00472          NEXT SENTENCE                                            
00473      ELSE                                                         
00474          MOVE 'ERROR OCCURRED OPEN - ELMSTR'                      
00475                                  TO  WS-ABEND-MESSAGE             
00476          MOVE ELMSTR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00477          PERFORM ABEND-PGM.                                       
00478                                                                   
00479  0100-EXIT.                                                       
00480      EXIT.                                                        
00481                                                                   
00482      EJECT                                                        
00483  1000-PROCESS-CNTL  SECTION.                                      
00484                                                                   
00485      IF READNEXT-COMP OR READNEXT-CARR                            
00486          GO TO 1000-READNEXT.                                     
00487                                                                   
00488      MOVE LOW-VALUES             TO  CF-CONTROL-PRIMARY.          
00489                                                                   
00490      START ELCNTL                                                 
00491          KEY IS GREATER THAN CF-CONTROL-PRIMARY.                  
00492                                                                   
00493      IF ELCNTL-FILE-STATUS = '23'                                 
00494          GO TO 3999-EXIT.                                         
00495                                                                   
00496      IF ELCNTL-FILE-STATUS NOT = '00'                             
00497          MOVE 'ERROR OCCURED START - ELCNTL'                      
00498                                  TO  WS-ABEND-MESSAGE             
00499          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00500          PERFORM ABEND-PGM.                                       
00501                                                                   
00502  1000-READNEXT.                                                   
00503      READ ELCNTL NEXT RECORD.                                     
00504                                                                   
00505      IF ELCNTL-FILE-STATUS = '23'                                 
00506          GO TO 1300-DEFAULT.                                      
00507                                                                   
00508      IF ELCNTL-FILE-STATUS = '10'                                 
00509          MOVE 'Y'                TO  END-OF-CNTL-SW               
00510          GO TO 3999-EXIT.                                         
00511                                                                   
00512      IF ELCNTL-FILE-STATUS NOT = '00'                             
00513          MOVE 'ERROR OCCURED READNEXT - ELCNTL'                   
00514                                  TO  WS-ABEND-MESSAGE             
00515          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00516          PERFORM ABEND-PGM.                                       
00517                                                                   
00518      IF CF-COMPANY-ID = 'NCX' OR 'BAL' OR 'LII'                   
00519          GO TO 1000-READNEXT.                                     
00520                                                                   
00521      IF CF-RECORD-TYPE = '1'                                      
00522          NEXT SENTENCE                                            
00523      ELSE                                                         
00524          GO TO 1000-READNEXT.                                     
00525                                                                   
00526      IF CO-HAS-CLAS-IC-CLAIM                                      
00527          NEXT SENTENCE                                            
00528      ELSE                                                         
00529          GO TO 1000-READNEXT.                                     
00530                                                                   
00531      IF CF-COMPANY-MASTER                                         
00532          ACCEPT WS-THIS-TIME-OF-DAY  FROM  TIME                   
00533          MOVE WS-TIME            TO  WS-DISPLAY-TIME              
00534          INSPECT WS-DISPLAY-TIME  CONVERTING SPACES TO '.'        
00535          DISPLAY 'EL346 BEGAN PROCESSING OF ' CF-CL-MAIL-TO-NAME  
00536                  ' AT ' WS-DISPLAY-TIME UPON CONSOLE              
00537          DISPLAY 'EL346 BEGAN PROCESSING OF ' CF-CL-MAIL-TO-NAME  
00538                  ' AT ' WS-DISPLAY-TIME                           
00539          MOVE CF-COMPANY-ID      TO  WS-H2-COMPANY-ID             
00540                                      WS-COMPANY-ID                
00541          MOVE CF-COMPANY-CD      TO  WS-COMPANY-CD                
00542          MOVE CF-CL-MAIL-TO-NAME TO  WS-H2-COMPANY-NAME           
00543          MOVE CF-CARRIER-CONTROL-LEVEL                            
00544                                  TO  WS-CARRIER-CONTROL           
00545          IF WS-CARRIER-CONTROL = SPACE                            
00546              NEXT SENTENCE                                        
00547          ELSE                                                     
00548              GO TO 1200-READ-CONTROL-CARRIER.                     
00549                                                                   
00550      GO TO 1999-EXIT.                                             
00551                                                                   
00552  1200-READ-CONTROL-CARRIER.                                       
00553                                                                   
00554      MOVE SPACES                 TO  CF-ACCESS-OF-CARRIER.        
00555      MOVE '6'                    TO  CF-RECORD-TYPE.              
00556      MOVE WS-CARRIER-CONTROL     TO  CF-CARRIER-CNTL.             
00557      MOVE ZERO                   TO  CF-SEQUENCE-NO.              
00558                                                                   
00559      READ ELCNTL.                                                 
00560                                                                   
00561      IF ELCNTL-FILE-STATUS = '10' OR '23'                         
00562          GO TO 1300-DEFAULT.                                      
00563                                                                   
00564      IF ELCNTL-FILE-STATUS NOT = '00'                             
00565          MOVE 'ERROR OCCURRED READ - ELCNTL'                      
00566                                  TO  WS-ABEND-MESSAGE             
00567          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00568          GO TO ABEND-PGM.                                         
00569                                                                   
00570      IF CF-MONTHS-BEFORE-PURGED NOT NUMERIC                       
00571          MOVE ZEROS TO CF-MONTHS-BEFORE-PURGED                    
00572          DISPLAY CF-COMPANY-ID ' ' CF-CARRIER-CNTL                
00573                    ' MONTHS-BEFORE-PURGED NOT NUMERIC'.           
00574                                                                   
00575      IF CF-BUILD-RETRIEVE-AFTER-MONTHS NOT NUMERIC                
00576          MOVE ZEROS TO CF-BUILD-RETRIEVE-AFTER-MONTHS             
00577          DISPLAY CF-COMPANY-ID ' ' CF-CARRIER-CNTL                
00578                    ' BUILD RETRIEVE AFTER NOT NUMERIC'.           
00579                                                                   
00580      IF CF-MONTHS-BEFORE-PURGED NOT GREATER THAN ZEROS            
00581          MOVE HIGH-VALUES        TO  WS-PURGE-DT                  
00582      ELSE                                                         
00583          MOVE WS-AS-OF-BIN-DT    TO  DC-BIN-DATE-1                
00584          MULTIPLY CF-MONTHS-BEFORE-PURGED BY -1                   
00585                                  GIVING DC-ELAPSED-MONTHS         
00586          MOVE ZEROS              TO  DC-ELAPSED-DAYS              
00587          MOVE '6'                TO  DC-OPTION-CODE               
00588          PERFORM 8500-DATE-CONVERSION                             
00589          MOVE DC-BIN-DATE-2      TO  DC-BIN-DATE-1                
00590          MULTIPLY CF-BUILD-RETRIEVE-AFTER-MONTHS BY -1            
00591                                  GIVING DC-ELAPSED-MONTHS         
00592          MOVE ZEROS              TO  DC-ELAPSED-DAYS              
00593          MOVE '6'                TO  DC-OPTION-CODE               
00594          PERFORM 8500-DATE-CONVERSION                             
00595          MOVE DC-BIN-DATE-2      TO  WS-PURGE-DT.                 
00596                                                                   
00597      DISPLAY 'CNTL LEV CARR ' CF-CARRIER-CNTL ' ' 'PURGE DATE  '  
00598                      DC-GREG-DATE-1-EDIT.                         
00599                                                                   
00600      GO TO 1999-EXIT.                                             
00601                                                                   
00602  1300-DEFAULT.                                                    
00603                                                                   
00604      DISPLAY CF-COMPANY-ID ' ' CF-CARRIER-CNTL                    
00605                    ' NO CONTROL FILE FOR THIS COMPANY/CARRIER'.   
00606                                                                   
00607      MOVE WS-DEFAULT-PURGE-DT    TO  WS-PURGE-DT.                 
00608                                                                   
00609      MOVE SPACES                 TO  CF-CONTROL-PRIMARY.          
00610      MOVE WS-COMPANY-ID          TO  CF-COMPANY-ID.               
00611      MOVE '1'                    TO  CF-RECORD-TYPE.              
00612      MOVE ZEROS                  TO  CF-SEQUENCE-NO.              
00613                                                                   
00614      READ ELCNTL.                                                 
00615                                                                   
00616      IF ELCNTL-FILE-STATUS NOT = '00'                             
00617          MOVE 'ERROR OCCURED READ - ELCNTL2'                      
00618                                  TO  WS-ABEND-MESSAGE             
00619          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00620          PERFORM ABEND-PGM.                                       
00621                                                                   
00622  1999-EXIT.                                                       
00623      EXIT.                                                        
00624      EJECT                                                        
00625  2000-PROCESS-RETRIEVES  SECTION.                                 
00626                                                                   
00627      MOVE LOW-VALUES             TO  RL-CONTROL-PRIMARY.          
00628      MOVE WS-COMPANY-CD          TO  RL-COMPANY-CD.               
00629                                                                   
00630      START ELRETR                                                 
00631          KEY NOT LESS THAN RL-CONTROL-PRIMARY.                    
00632                                                                   
00633      IF ELRETR-FILE-STATUS = '10' OR '23' OR                      
00634         RL-COMPANY-CD NOT = WS-COMPANY-CD                         
00635          MOVE 'Y'                TO  READNEXT-COMP-SW             
00636              PERFORM 7300-PRINT-HDG  THRU  7300-EXIT              
00637              PERFORM 7200-PRINT-TOTALS                            
00638              PERFORM 1000-PROCESS-CNTL  THRU  1999-EXIT           
00639              MOVE WS-COMPANY-CD  TO  WS-PREV-COMPANY-CD           
00640              GO TO 2000-PROCESS-RETRIEVES.                        
00641                                                                   
00642      IF ELRETR-FILE-STATUS NOT = '00'                             
00643          MOVE 'ERROR OCCURED START - ELRETR'                      
00644                                  TO  WS-ABEND-MESSAGE             
00645          MOVE ELRETR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00646          PERFORM ABEND-PGM.                                       
00647                                                                   
00648  2010-READNEXT-ELRETR.                                            
00649      READ ELRETR NEXT RECORD.                                     
00650                                                                   
00651      IF ELRETR-FILE-STATUS = '10' OR                              
00652         RL-CONTROL-PRIMARY =  NINES                               
00653          PERFORM 7200-PRINT-TOTALS                                
00654          MOVE 'Y'                TO  FIRST-TIME-SW                
00655          MOVE 'Y'                TO  READNEXT-COMP-SW             
00656          PERFORM 1000-PROCESS-CNTL  THRU  1999-EXIT               
00657          GO TO 2000-PROCESS-RETRIEVES.                            
00658                                                                   
00659      IF ELRETR-FILE-STATUS NOT = ZERO                             
00660          MOVE 'ERROR OCCURED READNEXT - ELRETR'                   
00661                                  TO  WS-ABEND-MESSAGE             
00662          MOVE ELRETR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00663          PERFORM ABEND-PGM.                                       
00664                                                                   
00665      IF FIRST-TIME                                                
00666          MOVE RL-COMPANY-CD      TO  WS-PREV-CO-CD                
00667          MOVE 'N'                TO  FIRST-TIME-SW                
00668          PERFORM 7300-PRINT-HDG  THRU  7300-EXIT.                 
00669                                                                   
00670      IF RL-COMPANY-CD NOT = WS-COMPANY-CD                         
00671          PERFORM 7200-PRINT-TOTALS                                
00672          MOVE 'Y'                TO  FIRST-TIME-SW                
00673          MOVE 'Y'                TO  READNEXT-COMP-SW             
00674          PERFORM 1000-PROCESS-CNTL  THRU  1999-EXIT               
00675          GO TO 2000-PROCESS-RETRIEVES.                            
00676                                                                   
00677      IF WS-CARRIER-CONTROL = SPACES                               
00678          IF RL-CARRIER NOT = CF-CARRIER-CNTL                      
00679              PERFORM 8000-READ-CARR-CNTL  THRU  8099-EXIT.        
00680                                                                   
00681      ADD +1 TO RETR-READ-CO.                                      
00682      MOVE '0'                    TO  WS-D1-CC.                    
00683      MOVE RL-CARRIER             TO  WS-D1-CARRIER.               
00684      MOVE RL-CLAIM-NO            TO  WS-D1-CLAIM-NO.              
00685      MOVE RL-CERT-NO             TO  WS-D1-CERT-NO.               
00686      MOVE RL-CERT-EFF-DT         TO  DC-BIN-DATE-1.               
00687      MOVE SPACE                  TO  DC-OPTION-CODE.              
00688      PERFORM 8500-DATE-CONVERSION.                                
00689      MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-CERT-EFF-DT.           
00690      PERFORM 5000-MOVE-NAME.                                      
00691      MOVE WS-NAME-WORK           TO  WS-D1-INSURED-NAME.          
00692                                                                   
00693      IF RL-CLAIM-NO = WS-LAST-CLAIM-NO                            
00694          IF HAS-CLAIM-MASTER                                      
00695              GO TO 2010-READNEXT-ELRETR                           
00696          ELSE                                                     
00697              IF PURGE-FILES                                       
00698                  GO TO 2030-DO-PURGE                              
00699              ELSE                                                 
00700                  IF DELETE-RETR                                   
00701                      PERFORM 3000-DELETE-RETR  THRU  3099-EXIT    
00702                      GO TO 2010-READNEXT-ELRETR.                  
00703                                                                   
00704      MOVE RL-CLAIM-NO            TO  WS-LAST-CLAIM-NO.            
00705      MOVE 'N'                    TO  CLAIM-MASTER-SW              
00706                                      PURGE-FILE-SW                
00707                                      DELETE-RETR-SW.              
00708                                                                   
00709      PERFORM  2100-CHECK-ELMSTR THRU 2199-EXIT.                   
00710                                                                   
00711      IF HAS-CLAIM-MASTER                                          
00712          GO TO 2010-READNEXT-ELRETR.                              
00713                                                                   
00714      IF RL-PURGED-DT = LOW-VALUES                                 
00715          PERFORM 2600-TEST-FOR-PURGE   THRU  2699-EXIT            
00716      ELSE                                                         
00717          PERFORM 2700-TEST-FOR-DELETE  THRU  2799-EXIT.           
00718                                                                   
00719      IF RL-ASSOC-CERT-TOTAL GREATER THAN +1                       
00720          MOVE RL-CONTROL-PRIMARY TO  WS-SAVE-CLAIM-KEY            
00721          MOVE RL-CLAIM-NO        TO  WS-ASSOC-CLAIM               
00722          PERFORM 2500-LOOK-AHEAD  THRU  2599-EXIT                 
00723          IF PURGE-FILES                                           
00724              GO TO 2030-DO-PURGE                                  
00725          ELSE                                                     
00726              IF DELETE-RETR                                       
00727                  PERFORM 3000-DELETE-RETR  THRU  3099-EXIT        
00728              ELSE                                                 
00729                  GO TO 2010-READNEXT-ELRETR                       
00730      ELSE                                                         
00731          IF PURGE-FILES                                           
00732              GO TO 2030-DO-PURGE                                  
00733          ELSE                                                     
00734              IF DELETE-RETR                                       
00735                  PERFORM 3000-DELETE-RETR  THRU  3099-EXIT        
00736              ELSE                                                 
00737                  GO TO 2010-READNEXT-ELRETR.                      
00738                                                                   
00739      GO TO 2010-READNEXT-ELRETR.                                  
00740                                                                   
00741  2030-DO-PURGE.                                                   
00742                                                                   
00743      PERFORM 2200-DELETE-TRAILERS  THRU  2299-EXIT.               
00744                                                                   
00745      MOVE 'CLAIM ACTIVITY WILL BE PURGED '                        
00746                                  TO  WS-D1-MESSAGE.               
00747      MOVE WS-DETAIL1             TO  PRT.                         
00748      MOVE 'Y'                    TO  DETAIL-PRINTED-SW.           
00749                                                                   
00750      PERFORM 7500-PRINT  THRU  7500-EXIT.                         
00751                                                                   
00752      ADD +1 TO RETR-PURGED-CO.                                    
00753                                                                   
00754      MOVE RETRIEVE-MASTER        TO WS-SAVE-ELRETR.               
00755                                                                   
00756      DELETE ELRETR RECORD.                                        
00757                                                                   
00758      IF ELRETR-FILE-STATUS NOT = '00'                             
00759          DISPLAY RL-CONTROL-PRIMARY                               
00760          MOVE 'ERROR OCCURED DELETE - ELRETR'                     
00761                                  TO  WS-ABEND-MESSAGE             
00762          MOVE ELRETR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00763          GO TO ABEND-PGM.                                         
00764                                                                   
00765      ADD +1 TO RETR-DELETED-CO.                                   
00766                                                                   
00767      MOVE WS-SAVE-ELRETR          TO RETRIEVE-MASTER.             
00768                                                                   
00769      GO TO 2010-READNEXT-ELRETR.                                  
00770                                                                   
00771      EJECT                                                        
00772                                                                   
00773  2100-CHECK-ELMSTR.                                               
00774                                                                   
00775      MOVE SPACES                 TO  CL-CONTROL-PRIMARY.          
00776      MOVE RL-CONTROL-PRIMARY     TO  CL-CONTROL-PRIMARY.          
00777                                                                   
00778      READ ELMSTR.                                                 
00779                                                                   
00780      IF ELMSTR-FILE-STATUS = '10' OR '23'                         
00781          GO TO 2199-EXIT.                                         
00782                                                                   
00783      IF ELMSTR-FILE-STATUS NOT = ZEROS                            
00784          MOVE 'ERROR OCCURED READ - ELMSTR'                       
00785                                  TO  WS-ABEND-MESSAGE             
00786          MOVE ELMSTR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00787          GO TO ABEND-PGM.                                         
00788                                                                   
00789      MOVE 'Y'                    TO  CLAIM-MASTER-SW.             
00790                                                                   
00791  2199-EXIT.                                                       
00792      EXIT.                                                        
00793      EJECT                                                        
00794                                                                   
00795  2200-DELETE-TRAILERS.                                            
00796                                                                   
00797      MOVE SPACES                 TO  AT-CONTROL-PRIMARY.          
00798      MOVE RL-CONTROL-PRIMARY     TO  AT-CONTROL-PRIMARY.          
00799      MOVE ZEROS                  TO  AT-SEQUENCE-NO.              
00800                                                                   
00801      START ELTRLR.                                                
00802                                                                   
00803      IF ELTRLR-FILE-STATUS = '10' OR '23'                         
00804          GO TO 2250-DELETE-ELACTQ.                                
00805                                                                   
00806      IF ELTRLR-FILE-STATUS NOT = ZEROS                            
00807          MOVE 'ERROR OCCURED START - ELTRLR'                      
00808                                  TO  WS-ABEND-MESSAGE             
00809          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00810          GO TO ABEND-PGM.                                         
00811                                                                   
00812  2210-READNEXT-ELTRLR.                                            
00813                                                                   
00814      READ ELTRLR NEXT RECORD.                                     
00815                                                                   
00816      IF ELTRLR-FILE-STATUS = '10' OR '23'                         
00817          GO TO 2250-DELETE-ELACTQ.                                
00818                                                                   
00819      IF AT-COMPANY-CD NOT = RL-COMPANY-CD OR                      
00820         AT-CARRIER    NOT = RL-CARRIER    OR                      
00821         AT-CLAIM-NO   NOT = RL-CLAIM-NO   OR                      
00822         AT-CERT-NO    NOT = RL-CERT-NO                            
00823          GO TO 2250-DELETE-ELACTQ.                                
00824                                                                   
00825      IF ELTRLR-FILE-STATUS NOT = ZEROS                            
00826          MOVE 'ERROR OCCURED READ - ELTRLR'                       
00827                                  TO  WS-ABEND-MESSAGE             
00828          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00829          GO TO ABEND-PGM.                                         
00830                                                                   
00831      IF AT-COMPANY-CD NOT = RL-COMPANY-CD OR                      
00832         AT-CARRIER    NOT = RL-CARRIER    OR                      
00833         AT-CLAIM-NO   NOT = RL-CLAIM-NO   OR                      
00834         AT-CERT-NO    NOT = RL-CERT-NO                            
00835          GO TO 2250-DELETE-ELACTQ.                                
00836                                                                   
00837      EVALUATE TRUE                                                
00838         WHEN PAYMENT-TR AND                                       
00839                AT-CHECK-QUE-CONTROL NOT = ZERO AND                
00840                AT-CHECK-QUE-CONTROL NOT = +99999999               
00841            PERFORM 2300-DELETE-CHECK  THRU  2399-EXIT             
00842         WHEN CORRESPONDENCE-TR                                    
00843            PERFORM 2400-DELETE-LETTER-ARCHIVE  THRU  2499-EXIT    
00844         WHEN AT-DIAGNOSIS-TRL                                     
00845                GO TO  2210-READNEXT-ELTRLR                        
00846      END-EVALUATE.                                                
00847                                                                   
00848      DELETE ELTRLR RECORD.                                        
00849                                                                   
00850      IF ELTRLR-FILE-STATUS NOT = '00'                             
00851          MOVE 'ERROR OCCURED DELETE - ELTRLR'                     
00852                                  TO  WS-ABEND-MESSAGE             
00853          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00854          GO TO ABEND-PGM.                                         
00855                                                                   
00856      ADD +1 TO TRLR-DELETED-CO.                                   
00857                                                                   
00858      GO TO  2210-READNEXT-ELTRLR.                                 
00859                                                                   
00860  2250-DELETE-ELACTQ.                                              
00861                                                                   
00862      MOVE RL-CONTROL-PRIMARY     TO  AQ-CONTROL-PRIMARY.          
00863                                                                   
00864      DELETE ELACTQ RECORD.                                        
00865                                                                   
00866      IF ELACTQ-FILE-STATUS = '10' OR '23'                         
00867          GO TO 2299-EXIT.                                         
00868                                                                   
00869      IF ELACTQ-FILE-STATUS NOT = '00'                             
00870          MOVE 'ERROR OCCURED DELETE - ELACTQ'                     
00871                                  TO  WS-ABEND-MESSAGE             
00872          MOVE ELACTQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00873          GO TO ABEND-PGM.                                         
00874                                                                   
00875      ADD +1 TO ACTQ-DELETED-CO.                                   
00876                                                                   
00877  2299-EXIT.                                                       
00878      EXIT.                                                        
00879      EJECT                                                        
00880                                                                   
00881  2300-DELETE-CHECK.                                               
00882                                                                   
00883      MOVE RL-COMPANY-CD          TO  CQ-COMPANY-CD.               
00884      MOVE AT-CHECK-QUE-CONTROL   TO  CQ-CONTROL-NUMBER.           
00885      MOVE AT-CHECK-QUE-SEQUENCE  TO  CQ-SEQUENCE-NUMBER.          
00886                                                                   
00887      READ ELCHKQ.                                                 
00888                                                                   
00889      IF ELCHKQ-FILE-STATUS = '10' OR '23'                         
00890          GO TO 2399-EXIT.                                         
00891                                                                   
00892      IF ELCHKQ-FILE-STATUS NOT = '00'                             
00893          MOVE 'ERROR-OCCURED READ - ELCHKQ'                       
00894                                  TO  WS-ABEND-MESSAGE             
00895          MOVE ELCHKQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00896          GO TO ABEND-PGM.                                         
00897                                                                   
00898      DELETE ELCHKQ RECORD.                                        
00899                                                                   
00900      ADD +1 TO CHKQ-DELETED-CO.                                   
00901                                                                   
00902      IF ELCHKQ-FILE-STATUS NOT = '00'                             
00903          MOVE 'ERROR-OCCURED DELETE - ELCHKQ'                     
00904                                  TO  WS-ABEND-MESSAGE             
00905          MOVE ELCHKQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00906          GO TO ABEND-PGM.                                         
00907  2399-EXIT.                                                       
00908      EXIT.                                                        
00909      EJECT                                                        
00910                                                                   
00911  2400-DELETE-LETTER-ARCHIVE.                                      
00912                                                                   
00913      MOVE LOW-VALUES             TO  LA-CONTROL-PRIMARY.          
00914      MOVE RL-COMPANY-CD          TO  LA-COMPANY-CD.               
00915      MOVE '1'                    TO  LA-RECORD-TYPE.              
00916      MOVE AT-LETTER-ARCHIVE-NO   TO  LA-ARCHIVE-NO.               
00917                                                                   
00918      START ELARCH                                                 
00919            KEY NOT LESS THAN LA-CONTROL-PRIMARY.                  
00920                                                                   
00921      IF ELARCH-FILE-STATUS = '10' OR '23'                         
00922          GO TO 2499-EXIT.                                         
00923                                                                   
00924      IF ELARCH-FILE-STATUS NOT = '00'                             
00925          MOVE 'ERROR-OCCURED START - ELARCH'                      
00926                                  TO  WS-ABEND-MESSAGE             
00927          MOVE ELARCH-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00928          GO TO ABEND-PGM.                                         
00929                                                                   
00930   2450-READ-NEXT.                                                 
00931                                                                   
00932      READ ELARCH NEXT RECORD.                                     
00933                                                                   
00934      IF ELARCH-FILE-STATUS = '10' OR '23'                         
00935          GO TO 2499-EXIT.                                         
00936                                                                   
00937      IF ELARCH-FILE-STATUS NOT = '00'                             
00938          MOVE 'ERROR-OCCURED START - ELARCH'                      
00939                                  TO  WS-ABEND-MESSAGE             
00940          MOVE ELARCH-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00941          GO TO ABEND-PGM.                                         
00942                                                                   
00943      IF (LA-COMPANY-CD NOT = AT-COMPANY-CD) OR                    
00944         (LA-ARCHIVE-NO NOT = AT-LETTER-ARCHIVE-NO)                
00945          GO TO 2499-EXIT.                                         
00946                                                                   
00947      DELETE ELARCH RECORD.                                        
00948                                                                   
00949      ADD +1 TO ARCH-DELETED-CO.                                   
00950                                                                   
00951      IF ELARCH-FILE-STATUS NOT = '00'                             
00952          MOVE 'ERROR-OCCURED DELETE - ELARCH'                     
00953                                  TO  WS-ABEND-MESSAGE             
00954          MOVE ELARCH-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00955          GO TO ABEND-PGM.                                         
00956                                                                   
00957      GO TO 2450-READ-NEXT.                                        
00958                                                                   
00959  2499-EXIT.                                                       
00960      EXIT.                                                        
00961      EJECT                                                        
00962                                                                   
00963  2500-LOOK-AHEAD.                                                 
00964                                                                   
00965      READ ELRETR NEXT RECORD.                                     
00966                                                                   
00967      IF ELRETR-FILE-STATUS = '10'                                 
00968          GO TO 2599-EXIT.                                         
00969                                                                   
00970      IF ELRETR-FILE-STATUS NOT = ZERO                             
00971          MOVE 'ERROR OCCURED READNEXT2 - ELRETR'                  
00972                                  TO  WS-ABEND-MESSAGE             
00973          MOVE ELRETR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00974          PERFORM ABEND-PGM.                                       
00975                                                                   
00976      ADD +1 TO RETR-READ-TEMP.                                    
00977                                                                   
00978      IF RL-CLAIM-NO NOT = WS-ASSOC-CLAIM                          
00979          GO TO 2550-FINISH-ASSOC.                                 
00980                                                                   
00981      IF HAS-CLAIM-MASTER                                          
00982          GO TO 2500-LOOK-AHEAD.                                   
00983                                                                   
00984      PERFORM 2100-CHECK-ELMSTR  THRU  2199-EXIT.                  
00985                                                                   
00986      IF HAS-CLAIM-MASTER                                          
00987          GO TO 2500-LOOK-AHEAD.                                   
00988                                                                   
00989      IF RL-PURGED-DT = LOW-VALUES                                 
00990          PERFORM 2600-TEST-FOR-PURGE   THRU  2699-EXIT            
00991      ELSE                                                         
00992          PERFORM 2700-TEST-FOR-DELETE  THRU  2799-EXIT.           
00993                                                                   
00994      GO TO 2500-LOOK-AHEAD.                                       
00995                                                                   
00996  2550-FINISH-ASSOC.                                               
00997                                                                   
00998      IF HAS-CLAIM-MASTER                                          
00999          ADD RETR-READ-TEMP TO RETR-READ-CO                       
01000          MOVE ZEROS              TO  RETR-READ-TEMP               
01001          GO TO 2599-EXIT.                                         
01002                                                                   
01003      MOVE WS-SAVE-CLAIM-KEY      TO  RL-CONTROL-PRIMARY.          
01004                                                                   
01005      START ELRETR                                                 
01006          KEY EQUAL RL-CONTROL-PRIMARY.                            
01007                                                                   
01008      IF ELRETR-FILE-STATUS NOT = '00'                             
01009          DISPLAY WS-SAVE-CLAIM-KEY                                
01010          MOVE 'ERROR OCCURED START2 - ELRETR'                     
01011                                  TO  WS-ABEND-MESSAGE             
01012          MOVE ELRETR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01013          PERFORM ABEND-PGM.                                       
01014                                                                   
01015      READ ELRETR NEXT RECORD.                                     
01016                                                                   
01017      IF ELRETR-FILE-STATUS NOT = ZERO                             
01018          MOVE 'ERROR OCCURED READNEXT3 - ELRETR'                  
01019                                  TO  WS-ABEND-MESSAGE             
01020          MOVE ELRETR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01021          PERFORM ABEND-PGM.                                       
01022                                                                   
01023  2599-EXIT.                                                       
01024      EXIT.                                                        
01025      EJECT                                                        
01026                                                                   
01027  2600-TEST-FOR-PURGE.                                             
01028                                                                   
01029      IF RL-CLAIM-IS-CLOSED                        AND             
01030         (RL-LAST-MAINT-DT LESS THAN WS-PURGE-DT OR                
01031                          RL-MASTER-WAS-RESTORED)  AND             
01032         RL-NEXT-AUTO-PAY-DT LESS THAN WS-PURGE-DT AND             
01033         RL-NEXT-RESEND-DT   LESS THAN WS-PURGE-DT AND             
01034         RL-NEXT-FOLLOWUP-DT LESS THAN WS-PURGE-DT AND             
01035         RL-LAST-PMT-DT      LESS THAN WS-PURGE-DT AND             
01036         RL-PAID-THRU-DT     LESS THAN WS-PURGE-DT AND             
01037         RL-HISTORY-ARCHIVE-DT NOT = LOW-VALUES    AND             
01038         WS-PURGE-DT NOT = WS-AS-OF-BIN-DT                         
01039          MOVE 'Y'                TO  PURGE-FILE-SW                
01040      ELSE                                                         
01041          MOVE 'N'                TO  PURGE-FILE-SW.               
01042                                                                   
01043  2699-EXIT.                                                       
01044      EXIT.                                                        
01045      EJECT                                                        
01046                                                                   
01047  2700-TEST-FOR-DELETE.                                            
01048                                                                   
01049      MOVE RL-PURGED-DT           TO  DC-BIN-DATE-1.               
01050      MOVE '6'                    TO  DC-OPTION-CODE.              
01051      MOVE +2                     TO  DC-ELAPSED-MONTHS.           
01052      MOVE +0                     TO  DC-ELAPSED-DAYS.             
01053      PERFORM 8500-DATE-CONVERSION.                                
01054                                                                   
01055      IF NO-CONVERSION-ERROR                                       
01056          IF DC-BIN-DATE-2 IS LESS THAN WS-AS-OF-BIN-DT            
01057              NEXT SENTENCE                                        
01058          ELSE                                                     
01059              GO TO 2799-EXIT                                      
01060      ELSE                                                         
01061          GO TO 2799-EXIT.                                         
01062                                                                   
01063      PERFORM 2800-READ-CERT  THRU  2899-EXIT.                     
01064                                                                   
01065      IF CERT-NOT-FOUND                                            
01066          MOVE 'Y'                TO  DELETE-RETR-SW               
01067          GO TO 2799-EXIT.                                         
01068                                                                   
01069      MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.                
01070      MOVE ' '                    TO DC-OPTION-CODE.               
01071      PERFORM 8500-DATE-CONVERSION.                                
01072      MOVE DC-GREG-DATE-1-YMD     TO WS-EFFECTIVE-DATE.            
01073      MOVE DC-ALPHA-CEN-N        TO WS-CC.                         
01074                                                                   
01075      IF CM-LF-ORIG-TERM NOT = ZERO                                
01076         MOVE CM-LF-ORIG-TERM     TO WS-WORK-TERM.                 
01077                                                                   
01078      IF (CM-AH-ORIG-TERM NOT = ZERO) AND                          
01079         ((CM-AH-ORIG-TERM GREATER CM-LF-ORIG-TERM) OR             
01080          (WS-WORK-TERM = ZERO))                                   
01081           MOVE CM-AH-ORIG-TERM   TO WS-WORK-TERM.                 
01082                                                                   
01083      IF (CM-LF-DEATH-DT NOT = LOW-VALUES AND SPACES) OR           
01084         (CM-AH-SETTLEMENT-DT NOT = LOW-VALUES AND SPACES)         
01085             COMPUTE WS-EXPIRE-MOS =                               
01086               (WS-CCYY * 12) + WS-MM + WS-WORK-TERM + 36          
01087      ELSE                                                         
01088             COMPUTE WS-EXPIRE-MOS =                               
01089               (WS-CCYY * 12) + WS-MM + WS-WORK-TERM + 12.         
01090                                                                   
01091      IF WS-WORK-TERM = +1                                         
01092         COMPUTE WS-EXPIRE-MOS =                                   
01093             (WS-CCYY * 12) + WS-MM + WS-WORK-TERM + 36.           
01094                                                                   
01095      IF (WS-EXPIRE-MOS LESS WS-CURRENT-MOS) AND                   
01096         (WS-COMPANY-ID NOT = 'LGX' AND 'AIG' AND 'AUK') AND       
01097         (CL-HISTORY-ARCHIVE-DT NOT = LOW-VALUES)                  
01098          MOVE 'Y'                TO  CERT-REWRITE-SW              
01099          MOVE 'Y'                TO  DELETE-RETR-SW.              
01100                                                                   
01101  2799-EXIT.                                                       
01102      EXIT.                                                        
01103      EJECT                                                        
01104                                                                   
01105  2800-READ-CERT.                                                  
01106                                                                   
01107      MOVE RL-COMPANY-CD          TO  CM-COMPANY-CD.               
01108      MOVE RL-CERT-CARRIER        TO  CM-CARRIER.                  
01109      MOVE RL-CERT-GROUPING       TO  CM-GROUPING.                 
01110      MOVE RL-CERT-STATE          TO  CM-STATE.                    
01111      MOVE RL-CERT-ACCOUNT        TO  CM-ACCOUNT.                  
01112      MOVE RL-CERT-EFF-DT         TO  CM-CERT-EFF-DT.              
01113      MOVE RL-CERT-NO             TO  CM-CERT-NO.                  
01114                                                                   
01115      READ ELCERT.                                                 
01116                                                                   
01117      IF ELCERT-FILE-STATUS = '23' OR '10'                         
01118          MOVE 'N'                TO  CERT-FOUND-SW                
01119          GO TO 2899-EXIT.                                         
01120                                                                   
01121      IF ELCERT-FILE-STATUS NOT = '00'                             
01122          MOVE 'ERROR OCCURED READ - ELCERT'                       
01123                                  TO  WS-ABEND-MESSAGE             
01124          MOVE ELCERT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01125          GO TO ABEND-PGM.                                         
01126                                                                   
01127      MOVE 'Y'                    TO  CERT-FOUND-SW.               
01128                                                                   
01129      IF CM-CLAIM-ATTACHED-COUNT NOT GREATER THAN ZERO             
01130          MOVE ZERO               TO  CM-CLAIM-ATTACHED-COUNT      
01131          MOVE 'Y'                TO  CERT-REWRITE-SW              
01132          IF CM-CLAIM-INTERFACE-SW = '1'                           
01133              MOVE ' '            TO  CM-CLAIM-INTERFACE-SW.       
01134                                                                   
01135  2899-EXIT.                                                       
01136      EXIT.                                                        
01137      EJECT                                                        
01138                                                                   
01139  2900-REWRITE-CERT.                                               
01140                                                                   
01141      REWRITE CERTIFICATE-MASTER.                                  
01142                                                                   
01143      ADD +1 TO CERT-REWRITTEN-CO                                  
01144                                                                   
01145      IF ELCERT-FILE-STATUS NOT = ZERO                             
01146          MOVE 'ERROR OCCURED REWRITE - ELCERT'                    
01147                                  TO  WS-ABEND-MESSAGE             
01148          MOVE ELCERT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01149          GO TO ABEND-PGM.                                         
01150                                                                   
01151      MOVE 'N'                    TO  CERT-REWRITE-SW.             
01152                                                                   
01153  2999-EXIT.                                                       
01154      EXIT.                                                        
01155      EJECT                                                        
01156                                                                   
01157  3000-DELETE-RETR.                                                
01158                                                                   
01159      PERFORM 2800-READ-CERT  THRU  2899-EXIT.                     
01160                                                                   
01161      IF CERT-NOT-FOUND                                            
01162          MOVE 'CLAIM WILL BE DELETED'                             
01163                                  TO  WS-D1-MESSAGE                
01164          MOVE 'CERTIFICATE NOT FOUND      '                       
01165                                  TO  WS-D2-MESSAGE                
01166          MOVE WS-DETAIL1         TO  PRT                          
01167          MOVE 'Y'                TO  DETAIL-PRINTED-SW            
01168          PERFORM 7500-PRINT  THRU  7500-EXIT                      
01169          MOVE SPACES             TO  WS-DETAIL1                   
01170          GO TO 3050-DELETE.                                       
01171                                                                   
01172      IF REWRITE-CERT                                              
01173          SUBTRACT +1 FROM CM-CLAIM-ATTACHED-COUNT                 
01174          IF CM-CLAIM-ATTACHED-COUNT NOT GREATER THAN ZERO         
01175              MOVE ZERO        TO CM-CLAIM-ATTACHED-COUNT          
01176              IF CM-CLAIM-INTERFACE-SW = '1'                       
01177                  MOVE ' '     TO CM-CLAIM-INTERFACE-SW.           
01178                                                                   
01179      IF REWRITE-CERT                                              
01180          PERFORM 2900-REWRITE-CERT  THRU  2999-EXIT.              
01181                                                                   
01182      MOVE '0'                    TO  WS-D1-CC.                    
01183      MOVE RL-CARRIER             TO  WS-D1-CARRIER.               
01184      MOVE RL-CLAIM-NO            TO  WS-D1-CLAIM-NO.              
01185      MOVE RL-CERT-NO             TO  WS-D1-CERT-NO.               
01186      MOVE RL-INSURED-LAST-NAME   TO  WS-D1-INSURED-NAME.          
01187      MOVE RL-CERT-EFF-DT         TO  DC-BIN-DATE-1                
01188      MOVE ' '                    TO  DC-OPTION-CODE.              
01189      PERFORM 8500-DATE-CONVERSION  THRU  8500-EXIT.               
01190      MOVE DC-GREG-DATE-1-EDIT    TO  WS-D1-CERT-EFF-DT.           
01191      MOVE 'CLAIM WILL BE DELETED'                                 
01192                                  TO  WS-D1-MESSAGE.               
01193      MOVE '0'                    TO  WS-D1-CC.                    
01194      MOVE WS-DETAIL1             TO  PRT.                         
01195      MOVE 'Y'                    TO  DETAIL-PRINTED-SW.           
01196      PERFORM 7500-PRINT THRU 7500-EXIT.                           
01197      MOVE SPACES                 TO  WS-DETAIL1.                  
01198                                                                   
01199  3050-DELETE.                                                     
01200                                                                   
01201      MOVE SPACES                 TO  AT-CONTROL-PRIMARY.          
01202      MOVE RL-CONTROL-PRIMARY     TO  AT-CONTROL-PRIMARY.          
01203      MOVE   +90                  TO  AT-SEQUENCE-NO.              
01204                                                                   
01205      DELETE ELTRLR.                                               
01206                                                                   
01207      IF ELTRLR-FILE-STATUS NOT = '00'                             
01208         IF ELTRLR-FILE-STATUS NOT = '23'                          
01209            MOVE 'ERROR OCCURED DELETE ELRETR/TRAILER - ELTRLR'    
01210                                            TO  WS-ABEND-MESSAGE   
01211            MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS       
01212            GO TO ABEND-PGM.                                       
01213                                                                   
PEMMOD     MOVE SPACES                 TO  AT-CONTROL-PRIMARY.          
PEMMOD     MOVE RL-CONTROL-PRIMARY     TO  AT-CONTROL-PRIMARY.          
PEMMOD     MOVE   +91                  TO  AT-SEQUENCE-NO.              
PEMMOD                                                                  
PEMMOD     DELETE ELTRLR.                                               
PEMMOD                                                                  
PEMMOD     IF ELTRLR-FILE-STATUS NOT = '00'                             
PEMMOD        IF ELTRLR-FILE-STATUS NOT = '23'                          
PEMMOD           MOVE 'ERROR OCCURED DELETE ELRETR/TRAILER - ELTRLR'    
PEMMOD                                           TO  WS-ABEND-MESSAGE   
PEMMOD           MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS       
PEMMOD           GO TO ABEND-PGM                                        
PEMMOD        END-IF                                                    
PEMMOD     END-IF                                                       
01214      DELETE ELRETR RECORD.                                        
01215                                                                   
01216      IF ELRETR-FILE-STATUS NOT = '00'                             
01217          DISPLAY RL-CONTROL-PRIMARY                               
01218          MOVE 'ERROR OCCURED DELETE - ELRETR'                     
01219                                  TO  WS-ABEND-MESSAGE             
01220          MOVE ELRETR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01221          GO TO ABEND-PGM.                                         
01222                                                                   
01223      ADD +1 TO RETR-DELETED-CO.                                   
01224                                                                   
01225  3099-EXIT.                                                       
01226      EXIT.                                                        
01227      EJECT                                                        
01228                                                                   
01229  3200-REWRITE-RETR.                                               
01230                                                                   
01231      REWRITE RETRIEVE-MASTER.                                     
01232                                                                   
01233      IF ELRETR-FILE-STATUS NOT = '00'                             
01234          MOVE 'ERROR OCCURED REWRITE - ELRETR'                    
01235                                  TO  WS-ABEND-MESSAGE             
01236          MOVE ELRETR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01237          GO TO ABEND-PGM.                                         
01238                                                                   
01239      ADD +1 TO RETR-REWRITTEN-CO.                                 
01240                                                                   
01241  3299-EXIT.                                                       
01242      EXIT.                                                        
01243      EJECT                                                        
01244                                                                   
01245  3999-EXIT.                                                       
01246      EXIT.                                                        
01247      EJECT                                                        
01248                                                                   
01249  5000-MOVE-NAME    SECTION.                                       
01250 *                                                               * 
01251 ***************************************************************** 
01252 *                     M O V E   N A M E   R O U T I N E         * 
01253 *                                                               * 
01254 *                THE FOLLOWING ROUTINE MOVES THE INSURRED'S     * 
01255 *            NAME FROM THE CLAIM MASTER TO A WORK AREA WITH     * 
01256 *            NO EMBEDDED BLANKS.                                * 
01257 *                                                               * 
01258 *                  FIELD               VALUE                    * 
01259 *                                                               * 
01260 *                LAST NAME (CL15)      SMITH                    * 
01261 *                1ST NAME  (CL12)      JOHN                     * 
01262 *                MID NAME  (CL12)      ALLEN                    * 
01263 *                                                               * 
01264 *                AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30)  * 
01265 *                                                               * 
01266 *                        SMITH, JOHN ALLEN                      * 
01267 *                                                               * 
01268 *                TO USE THIS ROUTINE YOU ALSO NEED A WORKING    * 
01269 *            STORAGE COPYBOOK:                                  * 
01270 *                                                               * 
01271 *                01  WS-NAME-WORK-AREA COPY ELCNWA.             * 
01272 *                                                               * 
01273 *****************************************************************.
01274                                                                   
01275      MOVE SPACES                 TO  WS-NAME-WORK-AREA.           
01276      MOVE ZERO                   TO  WS-NAME-SW.                  
01277      SET NWA-INDEX TO +1.                                         
01278                                                                   
01279      IF RL-INSURED-1ST-NAME = SPACES  AND                         
01280         RL-INSURED-MID-INIT = SPACES                              
01281          MOVE +1                 TO  WS-NAME-SW.                  
01282                                                                   
01283      MOVE RL-INSURED-LAST-NAME  TO  WS-NAME-WORK2.                
01284      PERFORM 5100-MOVE-NAME THRU 5190-EXIT.                       
01285                                                                   
01286      MOVE RL-INSURED-1ST-NAME   TO  WS-NAME-WORK2.                
01287      PERFORM 5100-MOVE-NAME THRU 5190-EXIT.                       
01288                                                                   
01289      SET NWA-INDEX UP BY +1.                                      
01290      MOVE RL-INSURED-MID-INIT   TO  WS-NAME-WORK2.                
01291      PERFORM 5100-MOVE-NAME THRU 5190-EXIT.                       
01292                                                                   
01293  5000-EXIT.                                                       
01294      EXIT.                                                        
01295                                                                   
01296      EJECT                                                        
01297  5100-MOVE-NAME SECTION.                                          
01298      IF WS-NAME-SW GREATER THAN +1                                
01299          GO TO 5190-EXIT.                                         
01300                                                                   
01301      IF WS-NAME-WORK2 = SPACES                                    
01302          GO TO 5190-EXIT.                                         
01303                                                                   
01304      SET NWA-INDEX2 TO +1.                                        
01305      SET NWA-INDEX3 TO +2.                                        
01306                                                                   
01307  5110-MOVE-NAME.                                                  
01308      MOVE WS-NW2 (NWA-INDEX2)  TO  WS-NW (NWA-INDEX).             
01309                                                                   
01310      IF NWA-INDEX LESS THAN +30                                   
01311          SET NWA-INDEX UP BY +1                                   
01312        ELSE                                                       
01313          ADD +2  TO  WS-NAME-SW                                   
01314          GO TO 5190-EXIT.                                         
01315                                                                   
01316      IF NWA-INDEX2 LESS THAN +20                                  
01317          SET NWA-INDEX3 UP BY +1                                  
01318          SET NWA-INDEX2 UP BY +1.                                 
01319                                                                   
01320      IF WS-NW2 (NWA-INDEX2) = SPACES AND                          
01321         WS-NW2 (NWA-INDEX3) = SPACES                              
01322          IF WS-NAME-SW = ZERO                                     
01323              MOVE ','            TO  WS-NW (NWA-INDEX)            
01324              SET NWA-INDEX UP BY +2                               
01325              MOVE +1             TO  WS-NAME-SW                   
01326              GO TO 5190-EXIT                                      
01327            ELSE                                                   
01328              GO TO 5190-EXIT.                                     
01329                                                                   
01330      GO TO 5110-MOVE-NAME.                                        
01331                                                                   
01332  5190-EXIT.                                                       
01333      EXIT.                                                        
01334                                                                   
01335      EJECT                                                        
01336                                                                   
01337  7000-END-RUN       SECTION.                                      
01338                                                                   
01339      MOVE 'Y'                   TO  END-OF-JOB-SW.                
01340      MOVE 'XXX'                 TO  WS-H2-COMPANY-ID.             
01341      MOVE 'OVERALL TOTALS                '                        
01342                                 TO  WS-H2-COMPANY-NAME.           
01343                                                                   
01344      PERFORM 7300-PRINT-HDG.                                      
01345      PERFORM 7200-PRINT-TOTALS.                                   
01346                                                                   
01347  7099-EXIT.                                                       
01348      EXIT.                                                        
01349                                                                   
01350      EJECT                                                        
01351                                                                   
01352  7200-PRINT-TOTALS  SECTION.                                      
01353                                                                   
01354      IF DETAIL-PRINTED                                            
01355          PERFORM 7300-PRINT-HDG                                   
01356          MOVE 'N'                TO  DETAIL-PRINTED-SW.           
01357                                                                   
01358      MOVE '0'                    TO  WS-TOT-CNTL.                 
01359      MOVE 'RETRIEVE MASTERS PROCESSED'                            
01360                                  TO  WS-TOT-DESC.                 
01361      IF END-OF-JOB                                                
01362          MOVE RETR-READ-FINAL    TO  WS-TOT-COUNT                 
01363      ELSE                                                         
01364          MOVE RETR-READ-CO       TO  WS-TOT-COUNT.                
01365                                                                   
01366      MOVE WS-TOTAL-LINE          TO  PRT.                         
01367      PERFORM 7500-PRINT THRU 7500-EXIT.                           
01368                                                                   
01369      MOVE '0'                    TO  WS-TOT-CNTL.                 
01370      MOVE 'RETRIEVE MASTERS PURGED '                              
01371                                  TO  WS-TOT-DESC.                 
01372      IF END-OF-JOB                                                
01373          MOVE RETR-PURGED-FINAL  TO  WS-TOT-COUNT                 
01374      ELSE                                                         
01375          MOVE RETR-PURGED-CO     TO  WS-TOT-COUNT.                
01376                                                                   
01377      MOVE WS-TOTAL-LINE          TO  PRT.                         
01378      PERFORM 7500-PRINT THRU 7500-EXIT.                           
01379                                                                   
01380      MOVE 'RETRIEVE MASTERS DELETED'                              
01381                                  TO  WS-TOT-DESC.                 
01382      IF END-OF-JOB                                                
01383          MOVE RETR-DELETED-FINAL TO  WS-TOT-COUNT                 
01384      ELSE                                                         
01385          MOVE RETR-DELETED-CO    TO  WS-TOT-COUNT.                
01386                                                                   
01387      MOVE WS-TOTAL-LINE          TO  PRT.                         
01388      PERFORM 7500-PRINT THRU 7500-EXIT.                           
01389                                                                   
01390      MOVE '0'                    TO  WS-TOT-CNTL.                 
01391      MOVE 'RETRIEVE MASTERS REWRITTEN'                            
01392                                  TO  WS-TOT-DESC.                 
01393      IF END-OF-JOB                                                
01394          MOVE RETR-REWRITTEN-FINAL                                
01395                                  TO  WS-TOT-COUNT                 
01396      ELSE                                                         
01397          MOVE RETR-REWRITTEN-CO  TO  WS-TOT-COUNT.                
01398                                                                   
01399      MOVE WS-TOTAL-LINE          TO  PRT.                         
01400      PERFORM 7500-PRINT THRU 7500-EXIT.                           
01401                                                                   
01402      MOVE ' '                    TO  WS-TOT-CNTL.                 
01403      MOVE 'TRAILERS DELETED     '                                 
01404                                  TO  WS-TOT-DESC.                 
01405      IF END-OF-JOB                                                
01406          MOVE TRLR-DELETED-FINAL TO  WS-TOT-COUNT                 
01407      ELSE                                                         
01408          MOVE TRLR-DELETED-CO    TO  WS-TOT-COUNT.                
01409                                                                   
01410      MOVE WS-TOTAL-LINE          TO  PRT.                         
01411      PERFORM 7500-PRINT THRU 7500-EXIT.                           
01412                                                                   
01413      MOVE ' '                    TO  WS-TOT-CNTL.                 
01414      MOVE 'CHECK RECORDS DELETED'                                 
01415                                  TO  WS-TOT-DESC.                 
01416      IF END-OF-JOB                                                
01417          MOVE CHKQ-DELETED-FINAL TO  WS-TOT-COUNT                 
01418      ELSE                                                         
01419          MOVE CHKQ-DELETED-CO    TO  WS-TOT-COUNT.                
01420                                                                   
01421      MOVE WS-TOTAL-LINE          TO  PRT.                         
01422      PERFORM 7500-PRINT THRU 7500-EXIT.                           
01423                                                                   
01424      MOVE ' '                    TO  WS-TOT-CNTL.                 
01425      MOVE 'LETTER RECORDS DELETED'                                
01426                                  TO  WS-TOT-DESC.                 
01427      IF END-OF-JOB                                                
01428          MOVE ARCH-DELETED-FINAL TO  WS-TOT-COUNT                 
01429      ELSE                                                         
01430          MOVE ARCH-DELETED-CO    TO  WS-TOT-COUNT.                
01431                                                                   
01432      MOVE WS-TOTAL-LINE          TO  PRT.                         
01433      PERFORM 7500-PRINT THRU 7500-EXIT.                           
01434                                                                   
01435      MOVE ' '                    TO  WS-TOT-CNTL.                 
01436      MOVE 'ACTIVITY RECORDS DELETED'                              
01437                                  TO  WS-TOT-DESC.                 
01438      IF END-OF-JOB                                                
01439          MOVE ACTQ-DELETED-FINAL TO  WS-TOT-COUNT                 
01440      ELSE                                                         
01441          MOVE ACTQ-DELETED-CO    TO  WS-TOT-COUNT.                
01442                                                                   
01443      MOVE WS-TOTAL-LINE          TO  PRT.                         
01444      PERFORM 7500-PRINT THRU 7500-EXIT.                           
01445                                                                   
01446      MOVE '0'                    TO  WS-TOT-CNTL.                 
01447      MOVE 'CERTS WITH CLAIM TAG REMOVED'                          
01448                                  TO  WS-TOT-DESC.                 
01449      IF END-OF-JOB                                                
01450          MOVE CERT-REWRITTEN-FINAL TO WS-TOT-COUNT                
01451      ELSE                                                         
01452          MOVE CERT-REWRITTEN-CO  TO  WS-TOT-COUNT.                
01453                                                                   
01454      MOVE WS-TOTAL-LINE          TO  PRT.                         
01455      PERFORM 7500-PRINT THRU 7500-EXIT.                           
01456                                                                   
01457      IF END-OF-JOB                                                
01458          GO TO 7099-EXIT.                                         
01459                                                                   
01460      ADD RETR-READ-CO      TO  RETR-READ-FINAL.                   
01461      ADD RETR-PURGED-CO    TO  RETR-PURGED-FINAL.                 
01462      ADD RETR-DELETED-CO   TO  RETR-DELETED-FINAL.                
01463      ADD RETR-REWRITTEN-CO TO  RETR-REWRITTEN-FINAL.              
01464      ADD TRLR-DELETED-CO   TO  TRLR-DELETED-FINAL.                
01465      ADD CHKQ-DELETED-CO   TO  CHKQ-DELETED-FINAL.                
01466      ADD ARCH-DELETED-CO   TO  ARCH-DELETED-FINAL.                
01467      ADD ACTQ-DELETED-CO   TO  ACTQ-DELETED-FINAL.                
01468      ADD CERT-REWRITTEN-CO TO  CERT-REWRITTEN-FINAL.              
01469      MOVE ZEROS                  TO  RETR-READ-CO                 
01470                                      RETR-PURGED-CO               
01471                                      RETR-DELETED-CO              
01472                                      RETR-REWRITTEN-CO            
01473                                      TRLR-DELETED-CO              
01474                                      CHKQ-DELETED-CO              
01475                                      ARCH-DELETED-CO              
01476                                      ACTQ-DELETED-CO              
01477                                      CERT-REWRITTEN-CO.           
01478                                                                   
01479  7200-EXIT.                                                       
01480       EXIT.                                                       
01481                                                                   
01482  7300-PRINT-HDG  SECTION.                                         
01483      ADD 1 TO PAGE-CNT.                                           
01484      MOVE PAGE-CNT               TO WS-H2-PAGE-NO.                
01485      MOVE +0                     TO LN-CNT.                       
01486      MOVE WS-HEADING1            TO PRT.                          
01487      PERFORM 7500-PRINT THRU 7500-EXIT.                           
01488                                                                   
01489      MOVE WS-HEADING2            TO PRT.                          
01490      PERFORM 7500-PRINT THRU 7500-EXIT.                           
01491                                                                   
01492      MOVE WS-HEADING3            TO PRT.                          
01493      PERFORM 7500-PRINT THRU 7500-EXIT.                           
01494                                                                   
01495      MOVE WS-HEADING4            TO PRT.                          
01496      PERFORM 7500-PRINT THRU 7500-EXIT.                           
01497                                                                   
01498      MOVE WS-HEADING5            TO PRT.                          
01499      PERFORM 7500-PRINT THRU 7500-EXIT.                           
01500                                                                   
01501      MOVE +7 TO LN-CNT.                                           
01502                                                                   
01503  7300-EXIT.                                                       
01504       EXIT.                                                       
01505                                                                   
01506  7500-PRINT  SECTION.                                             
01507                                                                   
01508      IF LN-CNT GREATER THAN +66                                   
01509          MOVE PRT                TO  WS-SAVE-PRT                  
01510          PERFORM 7300-PRINT-HDG                                   
01511          MOVE WS-SAVE-PRT        TO  PRT.                         
01512                                                                   
01513      MOVE P-CTL                  TO X.                            
01514      PERFORM 7600-PRINT-A-LINE THRU 7600-EXIT.                    
01515      MOVE SPACES                 TO PRT.                          
01516                                                                   
01517  7500-EXIT.                                                       
01518       EXIT.                                                       
01519                                                                   
01520  7600-PRINT-A-LINE  SECTION.                                      
01521                                                                   
01522      MOVE X                      TO  P-CTL.                       
01523                                                                   
01524      IF P-CTL = ' '                                               
01525          WRITE PRT AFTER ADVANCING 1 LINE                         
01526          ADD +1 TO LN-CNT                                         
01527      ELSE                                                         
01528          IF P-CTL = '0'                                           
01529              WRITE PRT AFTER ADVANCING 2 LINES                    
01530              ADD +2 TO LN-CNT                                     
01531          ELSE                                                     
01532              IF P-CTL = '-'                                       
01533                  WRITE PRT AFTER ADVANCING 3 LINES                
01534                  ADD +3 TO LN-CNT                                 
01535              ELSE                                                 
01536                  WRITE PRT AFTER ADVANCING PAGE                   
01537                  MOVE ZEROS      TO  LN-CNT.                      
01538                                                                   
01539  7600-EXIT.                                                       
01540       EXIT.                                                       
01541  EJECT                                                            
01542                                                                   
01543  8000-READ-CARR-CNTL   SECTION.                                   
01544                                                                   
01545      MOVE SPACES                 TO  CF-ACCESS-OF-CARRIER.        
01546      MOVE '6'                    TO  CF-RECORD-TYPE.              
01547      MOVE RL-CARRIER             TO  CF-CARRIER-CNTL.             
01548      MOVE ZERO                   TO  CF-SEQUENCE-NO.              
01549                                                                   
01550      READ ELCNTL.                                                 
01551                                                                   
01552      IF ELCNTL-FILE-STATUS = '10' OR '23'                         
01553          GO TO 8050-DEFAULT.                                      
01554                                                                   
01555      IF ELCNTL-FILE-STATUS NOT = '00'                             
01556          MOVE 'ERROR OCCURED READNEXT - ELCNTL'                   
01557                                  TO  WS-ABEND-MESSAGE             
01558          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01559          PERFORM ABEND-PGM.                                       
01560                                                                   
01561                                                                   
01562      IF CF-MONTHS-BEFORE-PURGED NOT NUMERIC                       
01563          MOVE ZEROS TO CF-MONTHS-BEFORE-PURGED                    
01564          DISPLAY CF-COMPANY-ID ' ' CF-CARRIER-CNTL                
01565                    ' MONTHS-BEFORE-PURGED NOT NUMERIC'.           
01566                                                                   
01567      IF CF-BUILD-RETRIEVE-AFTER-MONTHS NOT NUMERIC                
01568          MOVE ZEROS TO CF-BUILD-RETRIEVE-AFTER-MONTHS             
01569          DISPLAY CF-COMPANY-ID ' ' CF-CARRIER-CNTL                
01570                    ' BUILD RETRIEVE AFTER NOT NUMERIC'.           
01571                                                                   
01572      IF CF-MONTHS-BEFORE-PURGED NOT GREATER THAN ZEROS            
01573          MOVE HIGH-VALUES        TO  WS-PURGE-DT                  
01574      ELSE                                                         
01575          MOVE WS-AS-OF-BIN-DT    TO  DC-BIN-DATE-1                
01576          MULTIPLY CF-MONTHS-BEFORE-PURGED BY -1                   
01577                                  GIVING DC-ELAPSED-MONTHS         
01578          MOVE ZEROS              TO  DC-ELAPSED-DAYS              
01579          MOVE '6'                TO  DC-OPTION-CODE               
01580          PERFORM 8500-DATE-CONVERSION                             
01581          MOVE DC-BIN-DATE-2      TO  DC-BIN-DATE-1                
01582          MULTIPLY CF-BUILD-RETRIEVE-AFTER-MONTHS BY -1            
01583                                  GIVING DC-ELAPSED-MONTHS         
01584          MOVE ZEROS              TO  DC-ELAPSED-DAYS              
01585          MOVE '6'                TO  DC-OPTION-CODE               
01586          PERFORM 8500-DATE-CONVERSION                             
01587          MOVE DC-BIN-DATE-2      TO  WS-PURGE-DT.                 
01588                                                                   
01589      DISPLAY 'CARR ' CF-CARRIER-CNTL ' ' 'PURGE DATE  '           
01590                      DC-GREG-DATE-1-EDIT.                         
01591                                                                   
01592      GO TO 8099-EXIT.                                             
01593                                                                   
01594  8050-DEFAULT.                                                    
01595                                                                   
01596      DISPLAY CF-COMPANY-ID ' ' CF-CARRIER-CNTL                    
01597                    ' NO CONTROL FILE FOR THIS COMPANY/CARRIER'.   
01598                                                                   
01599      MOVE WS-DEFAULT-PURGE-DT    TO  WS-PURGE-DT.                 
01600                                                                   
01601      MOVE SPACES                 TO  CF-CONTROL-PRIMARY.          
01602      MOVE WS-COMPANY-ID          TO  CF-COMPANY-ID.               
01603      MOVE '1'                    TO  CF-RECORD-TYPE.              
01604      MOVE ZEROS                  TO  CF-SEQUENCE-NO.              
01605                                                                   
01606      READ ELCNTL.                                                 
01607                                                                   
01608      IF ELCNTL-FILE-STATUS NOT = '00'                             
01609          MOVE 'ERROR OCCURED READ - ELCNTL3'                      
01610                                  TO  WS-ABEND-MESSAGE             
01611          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01612          PERFORM ABEND-PGM.                                       
01613                                                                   
01614  8099-EXIT.                                                       
01615      EXIT.                                                        
01616      EJECT                                                        
01617  8500-DATE-CONVERSION  SECTION.                                   
01618      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   
01619  8500-EXIT.                                                       
01620      EXIT.                                                        
01621      EJECT                                                        
01622  8900-CLOSE-FILES  SECTION.                                       
01623      CLOSE ELRETR                                                 
01624            ELCNTL                                                 
01625            ELMSTR                                                 
01626            ELTRLR                                                 
01627            ELCHKQ                                                 
01628            ELARCH                                                 
01629            ELACTQ                                                 
01630            PRNTR.                                                 
01631  8900-EXIT.                                                       
01632      EXIT.                                                        
01633      EJECT                                                        
01634  ABEND-PGM SECTION.              COPY ELCABEND SUPPRESS.          
01635                                                                   
