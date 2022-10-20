00001
       IDENTIFICATION DIVISION.
00002                                                                   
00003  PROGRAM-ID.                 EL517 .                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 06/08/94 08:12:06.                 
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            
00008 *                            VMOD=2.040.                          
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
00026 *          THIS PROGRAM IS THE BATCH EDIT ROUTINE                 
00027 *          FOR THE ON-LINE CREDIT SYSTEM PENDING BUSINESS         
062904******************************************************************
062904*                   C H A N G E   L O G
062904*
062904* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
062904*-----------------------------------------------------------------
062904*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
062904* EFFECTIVE    NUMBER
062904*-----------------------------------------------------------------
100703* 100703    2003080800002  PEMA  ADD SUPER GAP PROCESSING
062904* 062904    2001061800003  PEMA  ADD CLM TYPE I AND G
062904* 062904    2004020600012  PEMA  ON CANCELS, CHECK TO SEE IF CLAIM
062904*                                IS OPEN OR CNC DT < PD THRU
062904*                                ALSO ADD ALTERNATE KEY CLAUSE
111204* 111204    2004110300005  PEMA  SPLIT SPP BANK COMMISSION
083106* 083106  CR2006063000001  PEMA  ADD LOAN OFFICER FIELD EDIT
030309* 030309  CR2009021700001  PEMA  ADD EDIT FOR BENE AND INS ADDR
041709* 042709  CR2009031600001  AJRA  ADD VIN UPDATE TO CERT TRAILER
062904******************************************************************
00028                                                                   
00029  ENVIRONMENT DIVISION.                                            
00030                                                                   
00031  INPUT-OUTPUT SECTION.                                            
00032                                                                   
00033  FILE-CONTROL.                                                    
00034                                                                   
00035      SELECT DISK-DATE        ASSIGN TO SYS019-UT-3380-S-SYS019.   
00036                                                                   
00037      SELECT ELCNTL           ASSIGN TO SYS021-3380-ELCNTL         
00038                              ORGANIZATION IS INDEXED              
00039                              ACCESS IS DYNAMIC                    
00040                              RECORD KEY IS CF-CONTROL-PRIMARY     
00041                              FILE STATUS IS ELCNTL-FILE-STATUS.   
00042                                                                   
00043      SELECT ERPNDB           ASSIGN TO SYS022-3380-ERPNDB         
00044                              ORGANIZATION IS INDEXED              
00045                              ACCESS IS SEQUENTIAL                 
00046                              RECORD KEY IS PB-CONTROL-PRIMARY     
00047                              ALTERNATE RECORD KEY IS              
00048                                  PB-CONTROL-BY-ACCOUNT            
00049                              FILE STATUS IS ERPNDB-FILE-STATUS.   
00050                                                                   
00051      SELECT ERREIN           ASSIGN TO SYS022-3380-ERREIN         
00052                              ORGANIZATION IS INDEXED              
00053                              ACCESS IS DYNAMIC                    
00054                              RECORD KEY IS RE-CONTROL-PRIMARY     
00055                              FILE STATUS IS ERREIN-FILE-STATUS.   
00056                                                                   
00057      SELECT ELCERT           COPY ELCCERTS.                       
00058                                                                   
00059      SELECT ELMSTR           ASSIGN TO SYS023-3380-ELMSTR5        
00060                              ORGANIZATION IS INDEXED              
00061                              ACCESS IS DYNAMIC                    
                                   RECORD KEY IS CL-CONTROL-BY-CERT-NO
00063                              FILE STATUS IS ELMSTR-FILE-STATUS.   
00064                                                                   
00065      SELECT ERCTBL           ASSIGN TO SYS022-3380-ERCTBL         
00066                              ORGANIZATION IS INDEXED              
00067                              ACCESS IS DYNAMIC                    
00068                              RECORD KEY IS CT-CONTROL-PRIMARY     
00069                              FILE STATUS IS ERCTBL-FILE-STATUS.   
00070                                                                   
00071      SELECT ERACCT           ASSIGN TO SYS022-3380-ERACCT2        
00072                              ORGANIZATION IS INDEXED              
00073                              ACCESS IS DYNAMIC                    
00074                              RECORD KEY IS AM-CONTROL-BY-VAR-GRP  
00075                              FILE STATUS IS ERACCT-FILE-STATUS.   
00076                                                                   
00077      SELECT ELCERR           ASSIGN TO SYS022-3380-ELERRS         
00078                              ORGANIZATION IS INDEXED              
00079                              ACCESS IS DYNAMIC                    
00080                              RECORD KEY IS EM-CONTROL-PRIMARY     
00081                              FILE STATUS IS ELCERR-FILE-STATUS.   
00082                                                                   
00083      SELECT ERPNDM           ASSIGN TO SYS022-3380-ERPNDM         
00084                              ORGANIZATION IS INDEXED              
00085                              ACCESS IS DYNAMIC                    
00086                              RECORD KEY IS PM-CONTROL-PRIMARY     
00087                              FILE STATUS IS ERPNDM-FILE-STATUS.   
00088                                                                   
00089      SELECT ERMAIL           ASSIGN TO SYS022-3380-ERMAIL         
00090                              ORGANIZATION IS INDEXED              
00091                              ACCESS IS DYNAMIC                    
00092                              RECORD KEY IS MA-CONTROL-PRIMARY     
00093                              FILE STATUS IS ERMAIL-FILE-STATUS.   
00094                                                                   
00095      SELECT ERPLAN           ASSIGN TO SYS022-3380-ERPLAN         
00096                              ORGANIZATION IS INDEXED              
00097                              ACCESS IS DYNAMIC                    
00098                              RECORD KEY IS PL-CONTROL-PRIMARY     
00099                              FILE STATUS IS ERPLAN-FILE-STATUS.   
00100                                                                   
00101      SELECT ERFORM           ASSIGN TO SYS022-3380-ERFORM         
00102                              ORGANIZATION IS INDEXED              
00103                              ACCESS IS DYNAMIC                    
00104                              RECORD KEY IS FO-CONTROL-PRIMARY     
00105                              FILE STATUS IS ERFORM-FILE-STATUS.   
00106                                                                   
00107      SELECT PRINT-FILE       ASSIGN TO SYS008-UR-1403-S-SYS008.   
00108                                                                   
00109      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   
00110                                                                   
00111      SELECT ELREPT           ASSIGN TO SYS010-3380-ELREPT         
00112                              ORGANIZATION IS INDEXED              
00113                              ACCESS IS DYNAMIC                    
00114                              RECORD KEY IS RF-CONTROL-PRIMARY     
00115                              FILE STATUS IS DTE-VSAM-FLAGS.       
00116                                                                   
00117      SELECT ERRESS           ASSIGN TO SYS021-3380-ERRESS         
00118                              ORGANIZATION IS INDEXED              
00119                              ACCESS IS DYNAMIC                    
00120                              RECORD KEY IS ERRESS-PRIMARY-KEY     
00121                              FILE STATUS IS ERRESS-FILE-STATUS.   
00122                                                                   
00123      SELECT ERRESC           ASSIGN TO SYS021-3380-ERRESC         
00124                              ORGANIZATION IS INDEXED              
00125                              ACCESS IS DYNAMIC                    
00126                              RECORD KEY IS ERRESC-RECORD-KEY      
00127                              FILE STATUS IS ERRESC-FILE-STATUS.   

100703     SELECT ERCOMP           ASSIGN TO             ERCOMP         
100703                             ORGANIZATION IS INDEXED              
100703                             ACCESS IS DYNAMIC                    
100703                             RECORD KEY IS CO-CONTROL-PRIMARY     
100703                             FILE STATUS IS ERCOMP-FILE-STATUS.   

083106     SELECT ERLOFC           ASSIGN TO             ERLOFC
083106                             ORGANIZATION IS INDEXED
083106                             ACCESS IS DYNAMIC
083106                             RECORD KEY IS LO-CONTROL-PRIMARY
083106                             FILE STATUS IS ERLOFC-FILE-STATUS.

100703     SELECT ERBXRF           ASSIGN TO             ERBXRF         
100703                             ORGANIZATION IS INDEXED              
100703                             ACCESS IS DYNAMIC                    
100703                             RECORD KEY IS BK-CONTROL-PRIMARY     
100703                             FILE STATUS IS ERBXRF-FILE-STATUS.   

111204     SELECT ERAGTC           ASSIGN TO             ERAGTC         
111204                             ORGANIZATION IS INDEXED              
111204                             ACCESS IS DYNAMIC                    
111204                             RECORD KEY IS AG-CONTROL-PRIMARY     
111204                             FILE STATUS IS ERAGTC-FILE-STATUS.   
042709
042709     SELECT ELCRTT           ASSIGN TO             ELCRTT         
042709                             ORGANIZATION IS INDEXED              
042709                             ACCESS IS DYNAMIC                    
042709                             RECORD KEY IS CS-CONTROL-PRIMARY     
042709                             FILE STATUS IS ELCRTT-FILE-STATUS.   

00128       EJECT                                                       
00129  DATA DIVISION.                                                   
00130                                                                   
00131  FILE SECTION.                                                    
00132                                                                   
00133  FD  DISK-DATE                   COPY ELCDTEFD.                   
00134                                                                   
00135      EJECT                                                        
00136  FD  ELCNTL.                                                      
00137                                  COPY ELCCNTL.                    
00138      EJECT                                                        
00139  FD  ERPNDB.                                                      
00140                                   COPY ERCPNDB.                   
00141      EJECT                                                        
00142  FD  ERREIN.                                                      
00143                                   COPY ERCREIN.                   
00144      EJECT                                                        
00145  FD  ELCERT.                                                      
00146                                   COPY ELCCERT.                   
00147      EJECT                                                        
00148  FD  ELMSTR.                                                      
00149                                   COPY ELCMSTR.                   
00150      EJECT                                                        
00151  FD  ERCTBL.                                                      
00152                                  COPY ERCCTBL.                    
00153                                                                   
00154  FD  ERACCT.                                                      
00155                                  COPY ERCACCT.                    
00156                                                                   
00157  FD  ELCERR.                                                      
00158                                  COPY ELCERRS.                    
00159                                                                   
00160  FD  ERPNDM.                                                      
00161                                  COPY ERCPNDM.                    
00162                                                                   
00163  FD  ERMAIL.                                                      
00164                                  COPY ERCMAIL.                    
00165                                                                   
00166  FD  ERPLAN.                                                      
00167                                  COPY ERCPLAN.                    
00168  FD  ERFORM.                                                      
00169                                  COPY ERCFORM.                    
00170                                                                   
00171  FD  PRINT-FILE                                                   
00172                                  COPY ELCPRTFD.                   
00173  FD  FICH                                                         
00174                                  COPY ELCFCHFD.                   
00175                                                                   
00176  FD  ELREPT.                                                      
00177                                  COPY ELCREPT.                    
00178      EJECT                                                        
00179                                                                   
00180  FD  ERRESS.                                                      
00181                                  COPY ERCRESS.                    
00182                                                                   
00183  FD  ERRESC.                                                      
00184                                  COPY ERCRESC.                    

100703 FD  ERCOMP.                                                      
100703                                 COPY ERCCOMP.                    
100703                                                                  
083106 FD  ERLOFC.
083106                                 COPY ERCLOFC.

100703 FD  ERBXRF.                                                      
100703                                 COPY ERCBXRF.                    
100703                                                                  
111204 FD  ERAGTC.                                                      
111204                                 COPY ERCAGTC.                    
111204                                                    
042709 FD  ELCRTT.
042709                                 COPY ELCCRTT.
042709              
00185      EJECT                                                        
00186  WORKING-STORAGE SECTION.                                         
00187  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      
00188  77  FILLER   PIC X(32) VALUE '********************************'. 
00189  77  FILLER   PIC X(32) VALUE '**   EL517 WORKING STORAGE    **'. 
00190  77  FILLER   PIC X(32) VALUE '*********** VMOD 2.040 *********'. 
00191                                                                   
00192  77  ELCNTL-WS-COMP        PIC S9(4)   COMP  VALUE +0.            
00193  77  ERPLAN-WS-COMP        PIC S9(4)   COMP  VALUE +0.            
00194  77  ERACCT-WS-COMP        PIC S9(4)   COMP  VALUE +0.            
00195  77  ERFORM-WS-COMP        PIC S9(4)   COMP  VALUE +0.            
00196                                                                   
00197  77  WS-ZERO                PIC S9        COMP-3   VALUE ZERO.    
00198  77  WS-RETURN-CODE         PIC S9(4)     COMP     VALUE ZERO.    
00199  77  WS-ABEND-MESSAGE       PIC X(80)              VALUE SPACES.  
00200  77  WS-ABEND-FILE-STATUS   PIC XX                 VALUE ZERO.    
00201  77  PGM-SUB                PIC S9(4)     COMP     VALUE +517.    
00202  77  SUB-X                  PIC S9(4)     COMP     VALUE   +0.    
00203  77  X                      PIC X                  VALUE ' '.     
00204  77  OLC-REPORT-NAME        PIC X(6)               VALUE 'EL517 '.
00205                                                                   
00206  01  TRAN-CT                PIC 9(5)               VALUE 0.       
00207  01  TRAN-CX REDEFINES TRAN-CT.                                   
00208      12  FILLER             PIC XX.                               
00209      12  TCX                PIC 999.                              
00210  01  SEQ-NO                 PIC 9999               VALUE 0.       
00211  01  RPND                   PIC 9(5)               VALUE 0.       
00212  01  WPND                   PIC 9(5)               VALUE 0.       
00213  01  RREI                   PIC 9(5)               VALUE 0.       
00214  01  RCTB                   PIC 9(5)               VALUE 0.       
00215  01  RACC                   PIC 9(5)               VALUE 0.       
00216  01  RCER                   PIC 9(5)               VALUE 0.       
00217  01  RCNT                   PIC 9(5)               VALUE 0.       
00218  01  WCER                   PIC 9(5)               VALUE 0.       
00219  01  ERPNDM-OPEN            PIC X                  VALUE SPACES.  
00220  01  ERMAIL-OPEN            PIC X                  VALUE SPACES.  
00221  01  ELMSTR-OPEN            PIC X                  VALUE SPACES.  
042709 01  ELCRTT-OPEN            PIC X                  VALUE SPACES.
00222  01  EMSG.                                                        
00223      12  WS-FILE-ERROR-MESSAGE.                                   
00224          16  FILLER              PIC X(24)   VALUE                
00225              'ERROR OCCURED OPENING - '.                          
00226          16  WS-FEM-FILE-NAME    PIC X(8).                        
00227                                                                   
00228  01  CMSG.                                                        
00229      12  WS-FILE-ERROR-MESSAGE.                                   
00230          16  FILLER              PIC X(24)   VALUE                
00231              'ERROR OCCURED CLOSING - '.                          
00232          16  WS-CEM-FILE-NAME    PIC X(8).                        
00233                                                                   
00234  01  RMSG.                                                        
00235      12  WS-FILE-ERROR-MESSAGE.                                   
00236          16  FILLER              PIC X(24)   VALUE                
00237              'ERROR OCCURED READING - '.                          
00238          16  WS-REM-FILE-NAME    PIC X(8).                        
00239                                                                   
00240  01  HDG1.                                                        
00241      12  FILLER                  PIC X(50)       VALUE ' '.       
00242      12  FILLER                  PIC X(28)       VALUE            
00243                    'PENDING BUSINESS FILE EDIT '.                 
00244      12  ER-ONLY                 PIC X(47)       VALUE            
00245           'NEW ITEMS AND FATAL ERRORS ONLY'.                      
00246      12  FILLER                  PIC X(7)        VALUE 'EL517A'.  
00247                                                                   
00248  01  HDG2.                                                        
00249      12  FILLER              PIC X(52)              VALUE SPACE.  
00250      12  H2-COMP             PIC X(30).                           
00251      12  FILLER              PIC X(42)              VALUE SPACE.  
00252      12  H2-DATE             PIC X(8).                            
00253                                                                   
00254  01  HDG3.                                                        
00255      12  FILLER              PIC X(54)              VALUE SPACE.  
00256      12  H3-DATE             PIC X(18).                           
00257      12  FILLER              PIC X(49)              VALUE SPACE.  
00258                                                                   
00259  01  SUM1.                                                        
00260      12  FILLER              PIC XX                 VALUE ' '.    
00261      12  SUM-TOT             PIC ZZ,ZZZ.                          
00262      12  FILLER              PIC X(3)               VALUE SPACE.  
00263      12  FILLER              PIC X(30)              VALUE         
00264           'ITEMS EDITED'.                                         
00265      EJECT                                                        
00266                                                                   
00267  01  WX-AREA.                                                     
00268      12  PH-COBOL            PIC X.                               
00269      12  HOLD-ELCNTL         PIC X(750) OCCURS 11 TIMES.          
00270                                                                   
00271  01  STATUS-SWITCH.                                               
00272      12  ELCNTL-FILE-STATUS       PIC XX VALUE SPACES.            
00273      12  ERPNDB-FILE-STATUS       PIC XX VALUE SPACES.            
00274      12  ERREIN-FILE-STATUS       PIC XX VALUE SPACES.            
00275      12  ELCERT-FILE-STATUS       PIC XX VALUE SPACES.            
00276      12  ELMSTR-FILE-STATUS       PIC XX VALUE SPACES.            
00277      12  ERMAIL-FILE-STATUS       PIC XX VALUE SPACES.            
00278      12  ERPLAN-FILE-STATUS       PIC XX VALUE SPACES.            
00279      12  ERFORM-FILE-STATUS       PIC XX VALUE SPACES.            
00280      12  ERPNDM-FILE-STATUS       PIC XX VALUE SPACES.            
00281      12  ERCTBL-FILE-STATUS       PIC XX VALUE SPACES.            
00282      12  ERACCT-FILE-STATUS.                                      
00283          16  ERACCT-FILE-STAT-1   PIC X  VALUE ' '.               
00284          16  ERACCT-FILE-STAT-2   PIC X  VALUE ' '.               
00285      12  ELCERR-FILE-STATUS       PIC XX VALUE SPACES.            
00286      12  ERRESS-FILE-STATUS       PIC XX VALUE SPACES.            
00287      12  ERRESC-FILE-STATUS       PIC XX VALUE SPACES.            
100703     12  ERCOMP-FILE-STATUS       PIC XX VALUE SPACES.
083106     12  ERLOFC-FILE-STATUS       PIC XX VALUE SPACES.
100703     12  ERBXRF-FILE-STATUS       PIC XX VALUE SPACES.
100703     12  ERAGTC-FILE-STATUS       PIC XX VALUE SPACES.
042709     12  ELCRTT-FILE-STATUS       PIC XX VALUE SPACES.
00288                                                                   
00289      12  EMI-WARNING-CTR          PIC S999  COMP-3  VALUE +0.     
00290      12  EMI-FORCABLE-CTR         PIC S999  COMP-3  VALUE +0.     
00291      12  EMI-FATAL-CTR            PIC S999  COMP-3  VALUE +0.     
00292      12  WS-SW-1                  PIC X  VALUE ' '.               
00293      12  WS-SW-2                  PIC X  VALUE ' '.               
00294                                                                   
00295      12  WS-COMPUTED-TOTALS      COMP-3.                          
00296          16  WS-LF-ISS-COMPUTED  PIC S9(9)V99  VALUE ZEROS COMP-3.
00297          16  WS-LF-ISS-ENTERED   PIC S9(9)V99  VALUE ZEROS COMP-3.
00298          16  WS-AH-ISS-COMPUTED  PIC S9(9)V99  VALUE ZEROS COMP-3.
00299          16  WS-AH-ISS-ENTERED   PIC S9(9)V99  VALUE ZEROS COMP-3.
00300          16  WS-LF-CAN-COMPUTED  PIC S9(9)V99  VALUE ZEROS COMP-3.
00301          16  WS-LF-CAN-ENTERED   PIC S9(9)V99  VALUE ZEROS COMP-3.
00302          16  WS-AH-CAN-COMPUTED  PIC S9(9)V99  VALUE ZEROS COMP-3.
00303          16  WS-AH-CAN-ENTERED   PIC S9(9)V99  VALUE ZEROS COMP-3.
00304          16  WS-ISSUE-CNT        PIC 9(5)      VALUE ZEROS COMP-3.
00305          16  WS-CANCEL-CNT       PIC 9(5)      VALUE ZEROS COMP-3.
00306      12  WS-LGX-CLAIM-USER       PIC X         VALUE SPACES.      
00307          88  CO-HAS-CLAIMS                     VALUE '2'.         
00308                                                                   
00309      12  EIBDATE                 PIC S9(7)     COMP-3.            
00310                                                                   
00311      EJECT                                                        
00312                                                                   
00313 * COPYBOOK FOR ADDITIONAL DFHCOMMAREA WK-WORK-AREA.               
00314      COPY ELC50W1.                                                
00315                                                                   
00316          16  WK-CONV-GREG.                                        
00317              24  WCG-CENT             PIC XX.                     
00318              24  WCG-REST             PIC X(4).                   
00319          16  WORK-GREGORIAN-DATE.                                 
00320              24  WK-GREG-CENT         PIC XX.                     
00321              24  WK-GREG-DATE-YYMMDD.                             
00322                  28  WK-GREG-YYMM     PIC X(4).                   
00323                  28  WK-GREG-DD       PIC XX.                     
00324                                                                   
CIDMOD         16  WORK-SELECT-DATE.                                    
CIDMOD             24  WK-SELECT-CENT           PIC XX.                 
CIDMOD             24  WK-SELECT-DATE-YYMMDD.                           
CIDMOD                 28  WK-SELECT-GREG-YYMM  PIC X(4).               
CIDMOD                 28  FILLER               PIC XX.                 
00328                                                                   
00329          16  WK-FORM-NO.                                          
00330              24  WK-FORM-BANK-ID       PIC X(5).                  
00331              24  WK-FORM-UNDRWRTR      PIC X(1).                  
00332              24  WK-FORM-MASTER-POL-NO PIC X(6).                  
00333                                                                   
00334          16  WK-MEMBER-NO.                                        
00335              24  WK-MEMBER-1-6        PIC X(6).                   
00336              24  WK-MEMBER-7-12       PIC X(6).                   
00337                                                                   
00338          16  WK-FULL-CERT-NO.                                     
00339              24  WK-CERT-POS-1-2      PIC X(2).                   
00340              24  WK-CERT-POS-3        PIC X(1).                   
00341              24  WK-CERT-POS-4        PIC X(1).                   
00342              24  WK-CERT-POS-5-6      PIC X(2).                   
00343              24  WK-CERT-POS-7        PIC X(1).                   
00344              24  WK-CERT-POS-8-11     PIC X(4).                   
00345                                                                   
00346          16  WK-VALID-RATE-CODE      PIC X VALUE ' '.             
00347              88  VALID-RATE-CODE     VALUE 'Y'.                   
00348              88  FOUND-TO-BE-INVALID VALUE 'N'.                   
00349                                                                   
00350      12  WK-BATCH-NO.                                             
00351          16  WK-BATCH-PREFIX          PIC XXX       VALUE SPACES. 
00352          16  FILLER                   PIC XXX       VALUE SPACES. 
00353                                                                   
00354                                   COPY ELCEDITC.                  
00355      EJECT                                                        
00356                                   COPY ELC50WS.                   
00357  01  HOLD-WORK     PIC X(2000)    VALUE SPACES.                   
00358      EJECT                                                        
00359 *DLB001                                                           
00360  01  WS-DLB-MASTER-POLICY-FORM.                                   
00361      12  DMPF-PROCESS-TYPE         PIC X.                         
00362          88  DMPF-OPEN-FILES                   VALUE 'O'.         
00363          88  DMPF-REG-PROCESS                  VALUE 'P'.         
00364          88  DMPF-CLOSE-FILES                  VALUE 'C'.         
00365      12  DMPF-POLICY-FORM-NUMBER   PIC X(6).                      
00366      12  DMPF-RETURN-CODE          PIC XX.                        
00367          88  DMPF-SUCCESSFUL-EDIT              VALUE 'OK'.        
00368          88  DMPF-POLICY-NOT-INPUT             VALUE '01'.        
00369          88  DMPF-POLICY-NOT-FOUND             VALUE '02'.        
00370          88  DMPF-POLICY-INACTIVE              VALUE '03'.        
00371          88  DMPF-PROCESS-TYPE-INVALID         VALUE 'Z1'.        
00372          88  DMPF-OPEN-ERROR                   VALUE 'Z2'.        
00373          88  DMPF-CLOSE-ERROR                  VALUE 'Z3'.        
00374                                                                   
00375 *DLB002                                                           
00376  01  WS-DLB-INTERNAL-POLICY-FORM.                                 
00377      12  DIPF-PROCESS-TYPE         PIC X.                         
00378          88  DIPF-OPEN-FILES                   VALUE 'O'.         
00379          88  DIPF-REG-PROCESS                  VALUE 'P'.         
00380          88  DIPF-CLOSE-FILES                  VALUE 'C'.         
00381      12  DIPF-POLICY-FORM-NUMBER   PIC X(6).                      
00382      12  DIPF-RETURN-CODE          PIC XX.                        
00383          88  DIPF-SUCCESSFUL-EDIT              VALUE 'OK'.        
00384          88  DIPF-POLICY-NOT-INPUT             VALUE '01'.        
00385          88  DIPF-POLICY-NOT-FOUND             VALUE '02'.        
00386          88  DIPF-POLICY-INACTIVE              VALUE '03'.        
00387          88  DIPF-PROCESS-TYPE-INVALID         VALUE 'Z1'.        
00388          88  DIPF-OPEN-ERROR                   VALUE 'Z2'.        
00389          88  DIPF-CLOSE-ERROR                  VALUE 'Z3'.        
00390                                                                   
00391 *DLB003                                                           
00392  01  WS-DLB-VALID-BANK-ID.                                        
00393      12  DVBI-PROCESS-TYPE         PIC X.                         
00394          88  DVBI-OPEN-FILES                   VALUE 'O'.         
00395          88  DVBI-REG-PROCESS                  VALUE 'P'.         
00396          88  DVBI-CLOSE-FILES                  VALUE 'C'.         
00397      12  DVBI-BANK-ID              PIC X(5).                      
00398      12  DVBI-BANK-ID-TYPE         PIC X.                         
00399          88  DVBI-BILLING-BANK                 VALUE 'B'.         
00400          88  DVBI-PLAN-BANK                    VALUE 'P'.         
00401          88  DVBI-ENROLL-BANK                  VALUE 'E'.         
00402          88  DVBI-NEGOT-BANK                   VALUE 'N'.         
00403          88  DVBI-PROCESS-CENTER               VALUE 'X'.         
00404      12  DVBI-RETURN-CODE          PIC XX.                        
00405          88  DVBI-SUCCESSFUL-EDIT              VALUE 'OK'.        
00406          88  DVBI-BANK-ID-NOT-INPUT            VALUE '01'.        
00407          88  DVBI-BANK-ID-TYPE-NOT-INPUT       VALUE '02'.        
00408          88  DVBI-BANK-ID-TYPE-INVALID         VALUE '03'.        
00409          88  DVBI-BANK-ID-NOT-FOUND            VALUE '04'.        
00410          88  DVBI-BANK-ID-INACTIVE             VALUE '05'.        
00411          88  DVBI-BANK-ID-TYPE-INCORRECT       VALUE '06'.        
00412          88  DVBI-PROCESS-TYPE-INVALID         VALUE 'Z1'.        
00413          88  DVBI-OPEN-ERROR                   VALUE 'Z2'.        
00414          88  DVBI-CLOSE-ERROR                  VALUE 'Z3'.        
00415                                                                   
00416 *DLB007                                                           
00417  01  WS-DLB-CERT-UNDRWRTR-ASSIGN.                                 
00418      12  DCUA-PROCESS-TYPE         PIC X.                         
00419          88  DCUA-OPEN-FILES                   VALUE 'O'.         
00420          88  DCUA-REG-PROCESS                  VALUE 'P'.         
00421          88  DCUA-CLOSE-FILES                  VALUE 'C'.         
00422      12  DCUA-RES-STATE            PIC XX.                        
00423      12  DCUA-BEN-TYPE             PIC XX.                        
00424      12  DCUA-SYS-IDENT            PIC X.                         
00425          88  DCUA-CLAIM                        VALUE 'C'.         
00426          88  DCUA-PREMIUM                      VALUE 'P'.         
00427      12  DCUA-COVERAGE-RULE        PIC X.                         
00428      12  DCUA-CLIENT-ID            PIC X(10).                     
00429      12  DCUA-EFF-DATE             PIC X(8).                      
00430      12  DCUA-PREV-CERT-NUMBER     PIC X(11).                     
00431      12  DCUA-RETURN-CODE          PIC XX.                        
00432          88  DCUA-SUCCESSFUL-PROCESS           VALUE 'OK'.        
00433          88  DCUA-OPEN-ERROR                   VALUE 'Z2'.        
00434          88  DCUA-CLOSE-ERROR                  VALUE 'Z3'.        
00435      12  DCUA-CERT-NUMBER          PIC X(11).                     
00436      12  DCUA-UNDRWRTR-CODE        PIC XX.                        
00437      12  DCUA-UNDRWRTR-GROUP-CODE  PIC X.                         
00438                                                                   
00439 *DLB011                                                           
00440  01  WS-DLB-PREM-RATES-BEN-TYPE.                                  
00441      12  DPRB-PROCESS-TYPE         PIC X.                         
00442          88  DPRB-OPEN-FILES                   VALUE 'O'.         
00443          88  DPRB-REG-PROCESS                  VALUE 'P'.         
00444          88  DPRB-CLOSE-FILES                  VALUE 'C'.         
00445      12  DPRB-RATE-CODE            PIC X(4).                      
00446      12  DPRB-COVERAGE-CATEG       PIC X.                         
00447      12  DPRB-RETURN-CODE          PIC XX.                        
00448          88  DPRB-SUCCESSFUL-PROCESS           VALUE 'OK'.        
00449          88  DPRB-NO-RATE-CODE                 VALUE '01'.        
00450          88  DPRB-NO-COV-CATEG                 VALUE '02'.        
00451          88  DPRB-NO-TOT-RATE-CODE             VALUE '03'.        
00452          88  DPRB-NO-COV-CAT-RATE-BEN-TYP      VALUE '04'.        
00453          88  DPRB-PROCESS-TYPE-INVALID         VALUE 'Z1'.        
00454          88  DPRB-OPEN-ERROR                   VALUE 'Z2'.        
00455          88  DPRB-CLOSE-ERROR                  VALUE 'Z3'.        
00456      12  DPRB-TOT-RATE-AMT         PIC 9(3)V9(3).                 
00457      12  DPRB-COV-CAT-RATE         PIC 9(2)V9(3).                 
00458      12  DPRB-BEN-TYPE             PIC XX.                        
00459                                                                   
00460 *DLB017                                                           
00461  01  WS-DLB-VALID-RATE-CODE.                                      
00462      12  DVRC-PROCESS-TYPE         PIC X.                         
00463          88  DVRC-OPEN-FILES                   VALUE 'O'.         
00464          88  DVRC-REG-PROCESS                  VALUE 'P'.         
00465          88  DVRC-CLOSE-FILES                  VALUE 'C'.         
00466      12  DVRC-RATE-CODE            PIC X(4).                      
00467      12  DVRC-RETURN-CODE          PIC XX.                        
00468          88  DVRC-SUCCESSFUL-EDIT              VALUE 'OK'.        
00469          88  DVRC-RATE-CODE-NOT-INPUT          VALUE '01'.        
00470          88  DVRC-RATE-CODE-NOT-FOUND          VALUE '02'.        
00471          88  DVRC-RATE-CODE-INACTIVE           VALUE '03'.        
00472          88  DVRC-PROCESS-TYPE-INVALID         VALUE 'Z1'.        
00473          88  DVRC-OPEN-ERROR                   VALUE 'Z2'.        
00474          88  DVRC-CLOSE-ERROR                  VALUE 'Z3'.        
00475                                                                   
00476 *DLB019                                                           
00477  01  WS-DLB-ORIG-COV-CAT.                                         
00478      12  DOCC-PROCESS-TYPE         PIC X.                         
00479          88  DOCC-OPEN-FILES                   VALUE 'O'.         
00480          88  DOCC-REG-PROCESS                  VALUE 'P'.         
00481          88  DOCC-CLOSE-FILES                  VALUE 'C'.         
00482      12  DOCC-ORIG-BEN-TYPE        PIC XX.                        
00483      12  DOCC-RETURN-CODE          PIC XX.                        
00484      12  DOCC-ORIG-COV-CAT         PIC X.                         
00485      12  DOCC-UNDERWRITER          PIC XX.                        
00486      12  DOCC-UNDW-GROUP-CODE      PIC X.                         
00487      12  DOCC-CREDIT-IND           PIC X.                         
00488      12  DOCC-PRODUCT-CD           PIC XX.                        
00489      12  DOCC-TERM                 PIC XXX.                       
00490                                                                   
00491 *DLB021                                                           
00492  01  WS-DLB-VALID-LOAN-OFFICER.                                   
00493      12  DVLO-LOAN-OFFICER         PIC X(3).                      
00494      12  THIS REDEFINES DVLO-LOAN-OFFICER.                        
00495          16  DVLO-BILL-TYPE        PIC X.                         
00496          16  DVLO-BILL-SOURCE      PIC X.                         
00497          16  DVLO-BATCH-TYPE       PIC X.                         
00498      12  DVLO-RETURN-CODE          PIC XX.                        
00499          88  DVLO-SUCCESSFUL-EDIT              VALUE 'OK'.        
00500          88  DVLO-LOAN-OFFCR-NOT-INPUT         VALUE '01'.        
00501          88  DVLO-BILL-TYPE-INVALID            VALUE '02'.        
00502          88  DVLO-BILL-SOURCE-INVALID          VALUE '03'.        
00503          88  DVLO-BATCH-TYPE-M-INVALID         VALUE '04'.        
00504          88  DVLO-BATCH-TYPE-D-INVALID         VALUE '05'.        
00505                                                                   
00506 *DLB022                                                           
00507  01  WS-DLB-VALID-STATE-CODE.                                     
00508      12  DVSC-PROCESS-TYPE         PIC X.                         
00509          88  DVSC-OPEN-FILES                   VALUE 'O'.         
00510          88  DVSC-REG-PROCESS                  VALUE 'P'.         
00511          88  DVSC-CLOSE-FILES                  VALUE 'C'.         
00512      12  DVSC-STATE-CODE           PIC XX.                        
00513      12  DVSC-RETURN-CODE          PIC XX.                        
00514          88  DVSC-SUCCESSFUL-EDIT              VALUE 'OK'.        
00515          88  DVSC-NO-STATE-INPUT               VALUE '01'.        
00516          88  DVSC-STATE-NOT-FOUND              VALUE '02'.        
00517          88  DVSC-STATE-INACTIVE               VALUE '03'.        
00518          88  DVSC-PROCESS-TYPE-INVALID         VALUE 'Z1'.        
00519          88  DVSC-OPEN-ERROR                   VALUE 'Z2'.        
00520          88  DVSC-CLOSE-ERROR                  VALUE 'Z3'.        
00521                                                                   
00522 *DLB023                                                           
00523  01  WS-DLB-VALID-CODE-VALUES.                                    
00524      12  DVCV-PROCESS-TYPE         PIC X.                         
00525          88  DVCV-OPEN-FILES                   VALUE 'O'.         
00526          88  DVCV-REG-PROCESS                  VALUE 'P'.         
00527          88  DVCV-CLOSE-FILES                  VALUE 'C'.         
00528      12  DVCV-SYSTEM-ID            PIC XX.                        
00529          88  DVCV-CLAIMS                       VALUE 'CL'.        
00530          88  DVCV-CREDIT                       VALUE 'CR'.        
00531      12  DVCV-RECORD-TYPE          PIC XX.                        
00532          88  DVCV-DENIAL-CODE                  VALUE 'DN'.        
00533          88  DVCV-EOB-CODE                     VALUE 'EO'.        
00534          88  DVCV-ICD9-CODE                    VALUE 'I9'.        
00535          88  DVCV-OCCUPATION-CODE              VALUE 'OC'.        
00536          88  DVCV-PAYMENT-CODE                 VALUE 'PN'.        
00537          88  DVCV-CAUSE-CODE                   VALUE 'CS'.        
00538          88  DVCV-COV-CAT-CODE                 VALUE 'CV'.        
00539          88  DVCV-RES-STATE-CODE               VALUE 'ST'.        
00540      12  DVCV-RECORD-KEY           PIC X(6).                      
00541      12  DVCV-RETURN-CODE          PIC XX.                        
00542          88  DVCV-SUCCESSFUL-EDIT              VALUE 'OK'.        
00543          88  DVCV-NO-RECORD-TYPE-INPUT         VALUE '01'.        
00544          88  DVCV-NO-SYSTEM-ID-INPUT           VALUE '02'.        
00545          88  DVCV-NO-RECORD-KEY-INPUT          VALUE '03'.        
00546          88  DVCV-INVALID-RECORD-KEY-INPUT     VALUE '04'.        
00547          88  DVCV-INVALID-SYSTEM-ID-INPUT      VALUE '05'.        
00548          88  DVCV-INVALID-RECORD-KEY-INPUT     VALUE '06'.        
00549          88  DVCV-PROCESS-TYPE-INVALID         VALUE 'Z1'.        
00550          88  DVCV-OPEN-ERROR                   VALUE 'Z2'.        
00551          88  DVCV-CLOSE-ERROR                  VALUE 'Z3'.        
00552      12  DVCV-CODE-DESC            PIC X(60).                     
00553      12  DVCV-GEN-DESC-1           PIC X(20).                     
00554      12  DVCV-GEN-DESC-2           PIC X(20).                     
00555      12  DVCV-GEN-DESC-3           PIC X(20).                     
00556                                                                   
00557      EJECT                                                        
00558                                                                   
00559  01  PARMLIST.                                                    
00560      02  ELCNTL-POINTER            PIC S9(4)  COMP VALUE +0.      
00561      02  ERACCT-POINTER            PIC S9(4)  COMP VALUE +0.      
00562      02  ERCTBL-POINTER            PIC S9(4)  COMP VALUE +0.      
CIDVAO**9/17/98: COMMENT OUT ERPLAN-POINTER.  NO LONGER USED BY LOGIC.
CIDVAO**
00563***   02  ERPLAN-POINTER            PIC S9(4)  COMP VALUE +0.      
00564      02  ERFORM-POINTER            PIC S9(4)  COMP VALUE +0.      
00565                                                                   
00566  01  PLAN-RECORDS.                                                
00567      12  LINK-LIFE-PLAN-RECORD    PIC X(420) VALUE SPACES.        
00568      12  LINK-AH-PLAN-RECORD      PIC X(420) VALUE SPACES.        
00569                                                                   
00570                                   COPY ELCDATE.                   
00571                                                                   
00572      EJECT                                                        
00573                                   COPY ELCCALC.                   
00574                                                                   
00575                                   COPY ELCDTECX.                  
00576                                                                   
00577                                   COPY ELCDTEVR.                  
00578                                                                   
00579                                   COPY ELCREINV.                  
00580                                                                   
00581                                   COPY ELCACCTV.                  
00582      EJECT                                                        
00583                                   COPY ELCDIFRD.                  
00584      EJECT                                                        
00585                                   COPY ELCERRWS.                  
00586      EJECT                                                        
00587                                                                   
00588  PROCEDURE DIVISION.                                              
00589                                                                   
00590  00000-LOAD-DATE-CARD.           COPY ELCDTERX.                   
00591                                                                   
00592      MOVE DTE-CLIENT TO E-CLIENT-ID.                              
00593                                                                   
00594      MOVE WS-CD-YY               TO DC-MDY-YEAR.                  
00595      MOVE WS-CD-DD               TO DC-MDY-DAY.                   
00596      MOVE WS-CD-MM               TO DC-MDY-MONTH.                 
00597                                                                   
00598      MOVE 4                      TO DC-OPTION-CODE.               
00599      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
00600      MOVE DC-JULIAN-DATE         TO EIBDATE.                      
00601      MOVE LOW-VALUES             TO WK-RECORD-ADDRESSES.          
00602      MOVE WORK-AREAS             TO HOLD-WORK.                    
00603                                                                   
00604      IF DTE-PRC-OPT = '2'                                         
00605          MOVE 'FULL FILE EDIT' TO ER-ONLY.                        
00606                                                                   
00607      OPEN INPUT ELCNTL.                                           
00608      IF ELCNTL-FILE-STATUS   = '00' OR '97'                       
00609          NEXT SENTENCE                                            
00610        ELSE                                                       
00611          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          
00612          MOVE 'ELCNTL'           TO WS-FEM-FILE-NAME              
00613          MOVE EMSG               TO WS-ABEND-MESSAGE              
00614          GO TO ABEND-PGM.                                         
00615                                                                   
00616      OPEN  I-O ERPNDB.                                            
00617      IF ERPNDB-FILE-STATUS   = '00' OR '97'                       
00618          NEXT SENTENCE                                            
00619        ELSE                                                       
00620          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          
00621          MOVE 'ERPNDB'           TO WS-FEM-FILE-NAME              
00622          MOVE EMSG               TO WS-ABEND-MESSAGE              
00623          GO TO ABEND-PGM.                                         
00624                                                                   
00625      MOVE LOW-VALUES            TO PB-CONTROL-PRIMARY.            
00626      MOVE DTE-CLASIC-COMPANY-CD TO PB-COMPANY-CD.                 
00627                                                                   
00628      START ERPNDB KEY NOT LESS THAN PB-CONTROL-PRIMARY.           
00629                                                                   
00630      IF ERPNDB-FILE-STATUS NOT = '00'                             
00631          GO TO 9999-DONE.                                         
00632                                                                   
00633      OPEN INPUT ERREIN.                                           
00634      IF ERREIN-FILE-STATUS   = '00' OR '97'                       
00635          NEXT SENTENCE                                            
00636        ELSE                                                       
00637          MOVE ERREIN-FILE-STATUS TO WS-ABEND-FILE-STATUS          
00638          MOVE 'ERREIN'           TO WS-FEM-FILE-NAME              
00639          MOVE EMSG               TO WS-ABEND-MESSAGE              
00640          GO TO ABEND-PGM.                                         
00641                                                                   
00642      OPEN I-O   ELCERT.                                           
00643 ******************************************************************
00644 *                                                                *
00645 *    THIS IS THE PI-PROGRAM-WORK-AREA THAT IS USED FOR THE       *
00646 *    REVIEW AND CORRRECTION SUB-SYSTEM.                          *
00647 *                                                                *
00648 *    THE FOLLOWING PROGRAMS USE THIS COPYBOOK.                   *
00649 *                                                                *
00650 *               EL631 - EL6311 - EL6312 - EL6313                 *
00651 *                                                                *
00652 ******************************************************************
00653                                                                   
00654      IF ELCERT-FILE-STATUS  = '00' OR '97'                        
00655          NEXT SENTENCE                                            
00656        ELSE                                                       
00657          MOVE ELCERT-FILE-STATUS TO WS-ABEND-FILE-STATUS          
00658          MOVE 'ELCERT'              TO WS-FEM-FILE-NAME           
00659          MOVE EMSG                  TO WS-ABEND-MESSAGE           
00660          GO TO ABEND-PGM.                                         
00661                                                                   
00662      OPEN INPUT ERCTBL.                                           
00663      IF ERCTBL-FILE-STATUS   = '00' OR '97'                       
00664          NEXT SENTENCE                                            
00665        ELSE                                                       
00666          MOVE ERCTBL-FILE-STATUS TO WS-ABEND-FILE-STATUS          
00667          MOVE 'ERCTBL'           TO WS-FEM-FILE-NAME              
00668          MOVE EMSG               TO WS-ABEND-MESSAGE              
00669          GO TO ABEND-PGM.                                         
00670                                                                   
00671      OPEN INPUT ERACCT.                                           
00672      IF ERACCT-FILE-STATUS   = '00' OR '97'                       
00673          NEXT SENTENCE                                            
00674        ELSE                                                       
00675          MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS          
00676          MOVE 'ERACCT'           TO WS-FEM-FILE-NAME              
00677          MOVE EMSG               TO WS-ABEND-MESSAGE              
00678          GO TO ABEND-PGM.                                         
00679                                                                   
100703     OPEN INPUT ERCOMP.                                           
100703     IF ERCOMP-FILE-STATUS   = '00' OR '97'                       
100703         NEXT SENTENCE                                            
100703       ELSE                                                       
100703         MOVE ERCOMP-FILE-STATUS TO WS-ABEND-FILE-STATUS          
100703         MOVE 'ERCOMP'           TO WS-FEM-FILE-NAME              
100703         MOVE EMSG               TO WS-ABEND-MESSAGE              
100703         GO TO ABEND-PGM.                                         
100703                                                                  
083106     OPEN INPUT ERLOFC
083106     IF ERLOFC-FILE-STATUS   = '00' OR '97'                       
083106        CONTINUE
083106     ELSE                                                       
083106        MOVE ERLOFC-FILE-STATUS  TO WS-ABEND-FILE-STATUS
083106        MOVE 'ERLOFC'            TO WS-FEM-FILE-NAME
083106        MOVE EMSG                TO WS-ABEND-MESSAGE
083106        GO TO ABEND-PGM
083106     END-IF

100703     OPEN INPUT ERBXRF.
100703     IF ERBXRF-FILE-STATUS   = '00' OR '97'                       
100703         NEXT SENTENCE                                            
100703       ELSE                                                       
100703         MOVE ERBXRF-FILE-STATUS TO WS-ABEND-FILE-STATUS          
100703         MOVE 'ERBXRF'           TO WS-FEM-FILE-NAME              
100703         MOVE EMSG               TO WS-ABEND-MESSAGE              
100703         GO TO ABEND-PGM.                                         
100703                                                                  
111204     OPEN INPUT ERAGTC.
111204     IF ERAGTC-FILE-STATUS   = '00' OR '97'                       
111204         NEXT SENTENCE                                            
111204       ELSE                                                       
111204         MOVE ERAGTC-FILE-STATUS TO WS-ABEND-FILE-STATUS          
111204         MOVE 'ERAGTC'           TO WS-FEM-FILE-NAME              
111204         MOVE EMSG               TO WS-ABEND-MESSAGE              
111204         GO TO ABEND-PGM.                                         
111204                                                                  
00680      OPEN INPUT ERPLAN.                                           
00681      IF ERPLAN-FILE-STATUS  = '00' OR '97'                        
00682         NEXT SENTENCE                                             
00683      ELSE                                                         
00684         MOVE ERPLAN-FILE-STATUS  TO WS-ABEND-FILE-STATUS          
00685         MOVE 'ERPLAN '           TO WS-FEM-FILE-NAME              
00686         MOVE EMSG                TO WS-ABEND-MESSAGE              
00687         GO TO ABEND-PGM.                                          
00688                                                                   
00689      OPEN INPUT ERFORM.                                           
00690      IF ERFORM-FILE-STATUS  = '00' OR '97' OR '9+'                        
              OR '9%'
00691         NEXT SENTENCE                                             
00692      ELSE                                                         
00693         MOVE ERFORM-FILE-STATUS  TO WS-ABEND-FILE-STATUS          
00694         MOVE 'ERFORM '           TO WS-FEM-FILE-NAME              
00695         MOVE EMSG                TO WS-ABEND-MESSAGE              
00696         GO TO ABEND-PGM.                                          
00697                                                                   
00698      OPEN INPUT ELCERR.                                           
00699      IF ELCERR-FILE-STATUS   = '00' OR '97'                       
00700          NEXT SENTENCE                                            
00701        ELSE                                                       
00702          MOVE ELCERR-FILE-STATUS TO WS-ABEND-FILE-STATUS          
00703          MOVE 'ELCERR'           TO WS-FEM-FILE-NAME              
00704          MOVE EMSG               TO WS-ABEND-MESSAGE              
00705          GO TO ABEND-PGM.                                         
00706                                                                   
00707      MOVE 'R'                     TO CP-RATE-FILE.                
00708      MOVE 'O'                     TO CP-IO-FUNCTION.              
00709      CALL 'ELRATEX' USING CALCULATION-PASS-AREA.                  
00710                                                                   
00711      IF IO-ERROR                                                  
00712         MOVE SPACE                TO WS-ABEND-FILE-STATUS         
00713         MOVE 'ELRATE'             TO WS-FEM-FILE-NAME             
00714         MOVE EMSG                 TO WS-ABEND-MESSAGE             
00715         GO TO ABEND-PGM.                                          
00716                                                                   
00717      EJECT                                                        
00718 *START*************CUSTOM CODE FOR CLIENT "DMD"*************      
00719 *                                                                 
00720      IF DTE-CLIENT NOT = 'DMD'                                    
00721          GO TO 0100-READ-PENDING.                                 
00722                                                                   
00723      OPEN INPUT ERRESS ERRESC.                                    
00724                                                                   
00725      IF ERRESS-FILE-STATUS   = '00' OR '97'                       
00726          NEXT SENTENCE                                            
00727      ELSE                                                         
00728          MOVE ERRESS-FILE-STATUS TO WS-ABEND-FILE-STATUS          
00729          MOVE 'ERRESS'           TO WS-FEM-FILE-NAME              
00730          MOVE EMSG               TO WS-ABEND-MESSAGE              
00731          GO TO ABEND-PGM.                                         
00732                                                                   
00733      IF ERRESC-FILE-STATUS   = '00' OR '97'                       
00734          NEXT SENTENCE                                            
00735      ELSE                                                         
00736          MOVE ERRESC-FILE-STATUS TO WS-ABEND-FILE-STATUS          
00737          MOVE 'ERRESC'           TO WS-FEM-FILE-NAME              
00738          MOVE EMSG               TO WS-ABEND-MESSAGE              
00739          GO TO ABEND-PGM.                                         
00740                                                                   
00741      MOVE SPACES                 TO WS-DLB-MASTER-POLICY-FORM.    
00742      MOVE 'O'                    TO DMPF-PROCESS-TYPE.            
00743      CALL 'DLB001'  USING  WS-DLB-MASTER-POLICY-FORM.             
00744      IF DMPF-RETURN-CODE EQUAL 'Z2'                               
00745          MOVE DMPF-RETURN-CODE   TO WS-ABEND-FILE-STATUS          
00746          MOVE 'DLB001'           TO WS-FEM-FILE-NAME              
00747          MOVE EMSG               TO WS-ABEND-MESSAGE              
00748          GO TO ABEND-PGM.                                         
00749                                                                   
00750      MOVE SPACES                 TO WS-DLB-INTERNAL-POLICY-FORM.  
00751      MOVE 'O'                    TO DIPF-PROCESS-TYPE.            
00752      CALL 'DLB002'  USING  WS-DLB-INTERNAL-POLICY-FORM.           
00753      IF DIPF-RETURN-CODE EQUAL 'Z2'                               
00754          MOVE DIPF-RETURN-CODE   TO WS-ABEND-FILE-STATUS          
00755          MOVE 'DLB002'           TO WS-FEM-FILE-NAME              
00756          MOVE EMSG               TO WS-ABEND-MESSAGE              
00757          GO TO ABEND-PGM.                                         
00758                                                                   
00759      MOVE SPACES                 TO WS-DLB-VALID-BANK-ID.         
00760      MOVE 'O'                    TO DVBI-PROCESS-TYPE.            
00761      CALL  'DLB003' USING  WS-DLB-VALID-BANK-ID.                  
00762      IF DIPF-RETURN-CODE EQUAL 'Z2'                               
00763          MOVE DVBI-RETURN-CODE   TO WS-ABEND-FILE-STATUS          
00764          MOVE 'DLB003'           TO WS-FEM-FILE-NAME              
00765          MOVE EMSG               TO WS-ABEND-MESSAGE              
00766          GO TO ABEND-PGM.                                         
00767                                                                   
00768      MOVE SPACES                 TO WS-DLB-CERT-UNDRWRTR-ASSIGN.  
00769      MOVE 'O'                    TO DCUA-PROCESS-TYPE.            
00770      CALL 'DLB007'  USING  WS-DLB-CERT-UNDRWRTR-ASSIGN.           
00771      IF DCUA-RETURN-CODE EQUAL 'Z2'                               
00772          MOVE DCUA-RETURN-CODE   TO WS-ABEND-FILE-STATUS          
00773          MOVE 'DLB007'           TO WS-FEM-FILE-NAME              
00774          MOVE EMSG               TO WS-ABEND-MESSAGE              
00775          GO TO ABEND-PGM.                                         
00776                                                                   
00777      MOVE SPACES                 TO WS-DLB-PREM-RATES-BEN-TYPE.   
00778      MOVE 'O'                    TO DPRB-PROCESS-TYPE.            
00779      CALL 'DLB011' USING  WS-DLB-PREM-RATES-BEN-TYPE.             
00780      IF DPRB-RETURN-CODE EQUAL 'Z2'                               
00781          MOVE DPRB-RETURN-CODE   TO WS-ABEND-FILE-STATUS          
00782          MOVE 'DLB011'           TO WS-FEM-FILE-NAME              
00783          MOVE EMSG               TO WS-ABEND-MESSAGE              
00784          GO TO ABEND-PGM.                                         
00785                                                                   
00786      MOVE SPACES                 TO WS-DLB-VALID-RATE-CODE.       
00787      MOVE 'O'                    TO DVRC-PROCESS-TYPE.            
00788      CALL 'DLB017' USING  WS-DLB-VALID-RATE-CODE.                 
00789      IF DVRC-RETURN-CODE EQUAL 'Z2'                               
00790          MOVE DVRC-RETURN-CODE   TO WS-ABEND-FILE-STATUS          
00791          MOVE 'DLB017'           TO WS-FEM-FILE-NAME              
00792          MOVE EMSG               TO WS-ABEND-MESSAGE              
00793          GO TO ABEND-PGM.                                         
00794                                                                   
00795      MOVE SPACES                 TO WS-DLB-VALID-STATE-CODE.      
00796      MOVE 'O'                    TO DVSC-PROCESS-TYPE.            
00797      CALL 'DLB022' USING  WS-DLB-VALID-STATE-CODE.                
00798      IF DVSC-RETURN-CODE EQUAL 'Z2'                               
00799          MOVE DVSC-RETURN-CODE   TO WS-ABEND-FILE-STATUS          
00800          MOVE 'DLB022'           TO WS-FEM-FILE-NAME              
00801          MOVE EMSG               TO WS-ABEND-MESSAGE              
00802          GO TO ABEND-PGM.                                         
00803                                                                   
00804 *                                                                 
00805 *END***************CUSTOM CODE FOR CLIENT "DMD"*************      
00806 *                                                                 
00807  0100-READ-PENDING.                                               
00808                                                                   
00809      MOVE HOLD-WORK TO WORK-AREAS.                                
00810                                                                   
00811      MOVE ZEROS                  TO EMI-WARNING-CTR               
00812                                     EMI-FORCABLE-CTR              
00813                                     EMI-FATAL-CTR.                
00814  0101-READ-PENDING.                                               
00815      READ ERPNDB.                                                 
00816      ADD 1 TO RPND.                                               
00817                                                                   
00818      IF DTE-CLASIC-COMPANY-CD NOT = PB-COMPANY-CD-A1              
00819          GO TO 9999-DONE.                                         
00820                                                                   
00821      IF ERPNDB-FILE-STATUS NOT = '00'                             
00822          GO TO 9999-DONE.                                         
00823                                                                   
00824      MOVE LIFE-OVERRIDE-L1       TO PB-LIFE-OVERRIDE-L1.          
00825      MOVE AH-OVERRIDE-L1         TO PB-AH-OVERRIDE-L1.            
00826      MOVE PB-ENTRY-BATCH         TO WK-BATCH-NO.                  
00827                                                                   
00828      IF PB-BATCH-TRAILER                                          
00829          GO TO 9999-REWRITE-RECORD.                               
00830                                                                   
00831      IF PB-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      
00832         GO TO 0100-READ-PENDING.                                  
00833                                                                   
00834      IF PB-BILLED-DT NOT EQUAL LOW-VALUES                         
00835         PERFORM 9300-ACCUM-BATCH-TOTALS THRU 9400-EXIT            
00836         GO TO 0100-READ-PENDING.                                  
00837                                                                   
00838      IF WK-BATCH-PREFIX = '#CL'                                   
00839         GO TO 0100-READ-PENDING.                                  
00840                                                                   
00841      IF DTE-PRC-OPT = '2'                                         
00842          NEXT SENTENCE                                            
00843      ELSE                                                         
00844          IF NOT PB-FATAL-ERRORS                                   
00845              PERFORM 9300-ACCUM-BATCH-TOTALS THRU 9400-EXIT       
00846              GO TO 0100-READ-PENDING.                             
00847                                                                   
00848      ADD 1 TO TRAN-CT.                                            
00849                                                                   
00850      EJECT                                                        
00851  0000-PROCEDURE-DIV.  COPY ELC50PDT.
00852 *                                                                 
00853 *                                                                 
00854 **START***********************************************************
00855 *      CUSTOM CODING FOR CLIENT "DMD"            PROJECT# 6475    
00856 *            PARAGRAPHS  6600 THRU 6700-COMM-EXIT                 
00857 ******************************************************************
00858 *                                                                 
00859  6600-GET-STATE-MUNICIPAL-TAX.                                    
00860                                                                   
00861      IF PB-COMPANY-ID NOT = 'DMD'                                 
00862          GO TO 6640-EXIT.                                         
00863                                                                   
00864      MOVE PB-COMPANY-CD       TO WS-ERRESS-COMPANY-CD.            
00865      MOVE PB-CERT-NO (1:2)    TO WS-ERRESS-RESIDENT-STATE.        
00866      MOVE PB-CERT-NO (4:1)    TO WS-ERRESS-COVERAGE-CAT.          
00867                                                                   
00868 *CONVERT PB-CERT-EFF-DT TO YYYYMMDD FORMATT TO READ MASTER        
00869      MOVE PB-CERT-EFF-DT      TO DC-BIN-DATE-1.                   
00870      MOVE SPACES TO DC-ALPHA-CENTURY.                             
00871      SET BIN-TO-GREG          TO TRUE.                            
00872      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
00873                                                                   
00874      MOVE DC-GREG-DATE-1-YMD  TO WS-CONTRACT-DATE (3:6).          
00875      MOVE DC-ALPHA-CEN-N      TO WS-CONTRACT-DATE (1:2)           
00876                                                                   
00877      MOVE WS-CONTRACT-DATE    TO WS-ERRESS-EXPIRE-DT.             
00878      MOVE WS-ERRESS-KEY       TO ERRESS-PRIMARY-KEY               
00879                                  WS-SAVE-ERRESS-KEY.              
00880                                                                   
00881 *POSITION FILE AT STARTING KEY RECORD(COMPANY, STATE, CATEGORY)   
00882 *WANT RECORD WITH RES-EXPIRE-DT GREATER THAN PB-CERT-EFF-DT       
00883                                                                   
00884      START ERRESS KEY NOT LESS THAN ERRESS-PRIMARY-KEY.           
00885                                                                   
00886      IF ERRESS-FILE-STATUS NOT EQUAL '00'                         
00887          MOVE ER-8002        TO WS-ERROR                          
00888          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00889          GO TO 6640-EXIT.                                         
00890                                                                   
00891 *GET RECORD WHERE POSITIONED                                      
00892                                                                   
00893  6600-READ-ERRESS-NEXT.                                           
00894      READ ERRESS NEXT RECORD.                                     
00895                                                                   
00896      IF ERRESS-FILE-STATUS NOT EQUAL '00'                         
00897          MOVE ER-8002        TO WS-ERROR                          
00898          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00899          GO TO 6640-EXIT.                                         
00900                                                                   
00901 *GOT GOOD RECORD ??    FIRST 4 POSITIONS MUST MATCH               
00902                                                                   
00903      IF ERRESS-PRIMARY-KEY (1:4) = WS-SAVE-ERRESS-KEY (1:4)       
00904         NEXT SENTENCE                                             
00905      ELSE                                                         
00906         MOVE ER-8002        TO WS-ERROR                           
00907         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
00908         GO TO 6640-EXIT.                                          
00909                                                                   
00910      IF WS-CONTRACT-DATE LESS THAN RES-EXPIRE-DATE                
00911         NEXT SENTENCE                                             
00912      ELSE                                                         
00913         GO TO 6600-READ-ERRESS-NEXT.                              
00914                                                                   
00915 *IS IT LIFE(1) OR IS IT P&C(2) ?????                              
00916 *WS-SUB1 SELECTS LIFE OR P&C RES-TAX-RECORD-DATA                  
00917                                                                   
00918      MOVE +2                     TO WS-SUB1.                      
00919                                                                   
00920      IF PB-CANCELLATION                                           
00921         IF PB-C-POLICY-FORM-NO (6:1) = '1'                        
00922            MOVE +1               TO WS-SUB1.                      
00923                                                                   
00924      IF PB-ISSUE                                                  
00925         IF PB-I-POLICY-FORM-NO (6:1) = '1'                        
00926            MOVE +1               TO WS-SUB1.                      
00927                                                                   
00928 *                                                                 
00929 *CONVERT ANNIVERSARY DATE OF ACCOUNT MASTER TO YYYYMMDD FORMAT.   
00930 *                                                                 
00931      MOVE AM-AN-MO            TO WS-NEXT-CONTRACT-DATE (5:2).     
00932      MOVE AM-AN-DA            TO WS-NEXT-CONTRACT-DATE (7:2).     
00933      MOVE AM-AN-YR            TO WS-NEXT-CONTRACT-DATE (3:2).     
00934      MOVE AM-AN-CC            TO WS-NEXT-CONTRACT-DATE (1:2).     
00935                                                                   
00936                                                                   
00937 *BUMP ANNIVERSARY DATE UP BY 1 YEAR IN WS-NEXT-CONTRACT-DATE      
00938 *TEST IF ANNIVERSARY DATE HAS EXISTED MORE THAN 1 YEAR            
00939 *TEST IF STATE AND MUNICIPAL RENEWAL PERCENT TAX APPLIES          
00940                                                                   
00941      ADD 1                    TO WS-YEAR-YY.                      
00942                                                                   
00943      IF WS-CONTRACT-DATE > WS-NEXT-CONTRACT-DATE                  
00944          MOVE RES-REN-STATE-TAX (WS-SUB1)                         
00945                               TO WS-RES-STATE-TAX                 
00946          MOVE RES-REN-MUNICIPAL-TAX (WS-SUB1)                     
00947                               TO WS-RES-MUNI-TAX                  
00948      ELSE                                                         
00949          MOVE RES-1YR-STATE-TAX (WS-SUB1)                         
00950                               TO WS-RES-STATE-TAX                 
00951          MOVE RES-1YR-MUNICIPAL-TAX (WS-SUB1)                     
00952                               TO WS-RES-MUNI-TAX.                 
00953                                                                   
00954 *COMPUTE TAX DEPENDING ON NON ZERO AMOUNTS                        
00955                                                                   
00956      IF PB-CANCELLATION                                           
00957          GO TO 6640-CANCEL-TAX.                                   
00958                                                                   
00959      IF PB-I-LF-PREMIUM-AMT > ZEROS                               
00960           COMPUTE PB-I-STATE-TAX ROUNDED =                        
00961                                     PB-I-LF-PREMIUM-AMT           
00962                                   * WS-RES-STATE-TAX              
00963           COMPUTE PB-I-MUNI-TAX ROUNDED =                         
00964                                     PB-I-LF-PREMIUM-AMT           
00965                                   * WS-RES-MUNI-TAX               
00966      ELSE                                                         
00967      IF PB-I-AH-PREMIUM-AMT > ZEROS                               
00968           COMPUTE PB-I-STATE-TAX ROUNDED =                        
00969                                     PB-I-AH-PREMIUM-AMT           
00970                                   * WS-RES-STATE-TAX              
00971           COMPUTE PB-I-MUNI-TAX ROUNDED =                         
00972                                     PB-I-AH-PREMIUM-AMT           
00973                                   * WS-RES-MUNI-TAX.              
00974                                                                   
00975      GO TO 6640-EXIT.                                             
00976                                                                   
00977  6640-CANCEL-TAX.                                                 
00978      MOVE CM-POLICY-FORM-NO    TO PB-C-POLICY-FORM-NO.            
00979                                                                   
00980      IF PB-C-LF-CANCEL-AMT > ZEROS                                
00981           COMPUTE PB-CI-STATE-TAX ROUNDED =                       
00982                                     PB-C-LF-CANCEL-AMT            
00983                                   * WS-RES-STATE-TAX              
00984           COMPUTE PB-CI-MUNI-TAX ROUNDED =                        
00985                                     PB-C-LF-CANCEL-AMT            
00986                                   * WS-RES-MUNI-TAX               
00987      ELSE                                                         
00988      IF PB-C-AH-CANCEL-AMT > ZEROS                                
00989           COMPUTE PB-CI-STATE-TAX ROUNDED =                       
00990                                     PB-C-AH-CANCEL-AMT            
00991                                   * WS-RES-STATE-TAX              
00992           COMPUTE PB-CI-MUNI-TAX ROUNDED =                        
00993                                     PB-C-AH-CANCEL-AMT            
00994                                   * WS-RES-MUNI-TAX.              
00995                                                                   
00996  6640-EXIT.                                                       
00997      EXIT.                                                        
00998                                                                   
00999  6650-GET-RES-STATE-COMM.                                         
01000                                                                   
01001      IF PB-COMPANY-ID NOT = 'DMD'                                 
01002          GO TO 6700-COMM-EXIT.                                    
01003                                                                   
01004      MOVE PB-COMPANY-CD       TO WS-ERRESC-COMPANY-CD.            
01005      MOVE PB-SV-CARRIER       TO WS-ERRESC-CARRIER.               
01006      MOVE PB-SV-GROUPING      TO WS-ERRESC-GROUP.                 
01007      MOVE PB-SV-STATE         TO WS-ERRESC-STATE.                 
01008      MOVE PB-ACCOUNT          TO WS-ERRESC-ACCOUNT.               
01009      MOVE AM-AGT (SUB1)       TO WS-ERRESC-AGENT.                 
01010      MOVE PB-CERT-NO (1:2)    TO WS-ERRESC-RES-STATE.             
01011                                                                   
01012 *CONVERT PB-CERT-EFF-DT TO YYYYMMDD FORMATT TO READ MASTER        
01013                                                                   
01014      MOVE PB-CERT-EFF-DT      TO DC-BIN-DATE-1.                   
01015      MOVE SPACES              TO DC-ALPHA-CENTURY.                
01016      SET BIN-TO-GREG          TO TRUE.                            
01017      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    
01018                                                                   
01019      MOVE DC-GREG-DATE-1-YMD  TO WS-CONTRACT-DATE (3:6).          
01020      MOVE DC-ALPHA-CEN-N      TO WS-CONTRACT-DATE (1:2)           
01021                                                                   
01022      MOVE WS-CONTRACT-DATE    TO WS-ERRESC-EXPIRE-DT.             
01023                                                                   
01024 *POSITION FILE AT STARTING KEY RECORD(COMPANY, CARRIER, GROUP, ST,
01025 *ACCOUNT,AGENT, RESIDENT STATE) AND RESC-EXPIRE-DATE SET BY THE   
01026 *VALUE OF PB-CERT-EFF-DT                                          
01027                                                                   
01028      MOVE WS-ERRESC-KEY        TO ERRESC-RECORD-KEY.              
01029      START ERRESC KEY NOT LESS THAN ERRESC-RECORD-KEY.            
01030                                                                   
01031      IF ERRESC-FILE-STATUS NOT EQUAL '00'                         
01032          GO TO 6700-COMM-EXIT.                                    
01033                                                                   
01034 *GET RECORD WHERE POSITIONED                                      
01035                                                                   
01036  6650-READ-ERRESC-NEXT.                                           
01037      READ ERRESC NEXT RECORD.                                     
01038                                                                   
01039      IF ERRESC-FILE-STATUS NOT EQUAL '00'                         
01040          GO TO 6700-COMM-EXIT.                                    
01041                                                                   
01042      IF ERRESC-RECORD-KEY (1:32) = WS-ERRESC-KEY (1:32)           
01043         NEXT SENTENCE                                             
01044      ELSE                                                         
01045          GO TO 6700-COMM-EXIT.                                    
01046                                                                   
01047      IF WS-CONTRACT-DATE IS LESS THAN RESC-EXPIRE-DATE            
01048          NEXT SENTENCE                                            
01049        ELSE                                                       
01050          GO TO 6650-READ-ERRESC-NEXT.                             
01051                                                                   
01052 *CHECK IF CERTIFICATE EFFECTIVE DATE IS WITHIN ACCOUNT RESIDENT ST
01053 *COMMISSION RECORD'S EFFECTIVE TIME PERIOD??                      
01054                                                                   
01055      IF WS-CONTRACT-DATE IS LESS THAN RESC-EFFECTIVE-DATE         
01056          GO TO 6700-COMM-EXIT.                                    
01057                                                                   
01058 * SEARCH FOR MATCHING CATEGORY CODE ENTRY                         
01059                                                                   
01060      MOVE ZEROS               TO WS-SUB WS-SUB1.                  
01061                                                                   
01062      PERFORM 6680-SEARCH-CATEGORY THRU 6680-EXIT.                 
01063                                                                   
01064      IF WS-SUB1 > ZERO                                            
01065          IF RESC-COMMISSION-TAB (WS-SUB) NUMERIC                  
01066              PERFORM 6680-SET-COMMISSION                          
01067          ELSE                                                     
01068              PERFORM 6680-SET-TABLE-CODE.                         
01069                                                                   
01070      GO TO 6700-COMM-EXIT.                                        
01071                                                                   
01072  6680-SET-TABLE-CODE.                                             
01073      IF RESC-COMMISSION-TAB (WS-SUB) EQUAL SPACES                 
01074         MOVE ZEROS             TO WS-SUB1                         
01075      ELSE                                                         
01076 * DEFAULT TO AH-COVERAGE TO READ COMMISSION TABLE(CTBL)           
01077 * TO COMPUTE AH COMMISSION.                                       
01078         MOVE WS-AH-COMP-CD     TO CTBL-BEN-CODE                   
01079         MOVE PB-I-AH-TERM      TO WS-WORK-TERM                    
01080         MOVE PB-AH-OVERRIDE-L1 TO CTBL-BEN-TYPE                   
01081         MOVE RESC-COMMISSION-TAB (WS-SUB) TO CTBL-TABLE           
01082         PERFORM 6680-READ-CTBL-TABLE                              
01083         IF ERCTBL-FILE-STATUS NOT = '00'                          
01084            MOVE ZEROS          TO WS-SUB1.                        
01085                                                                   
01086  6680-READ-CTBL-TABLE.                                            
01087 *                                                                 
01088 * WHEN CTBL-SW = '0540' READ COMMISSION TABLE(CTBL)               
01089 * TO COMPUTE LIFE COMMISSION                                      
01090                                                                   
01091      IF CTBL-SW = '0540'                                          
01092         MOVE WS-LF-COMP-CD     TO CTBL-BEN-CODE                   
01093         MOVE PB-I-LF-TERM      TO WS-WORK-TERM                    
01094         MOVE PB-LIFE-OVERRIDE-L1 TO CTBL-BEN-TYPE.                
01095                                                                   
01096      PERFORM 8300-READ-CTBL-TABLE THRU 8300-EXIT.                 
01097                                                                   
01098      IF CTBL-TABLE-FOUND                                          
01099         IF CTBL-SW = '0530'                                       
01100              COMPUTE WS-COMM-CK-AMT = PB-I-AH-BENEFIT-AMT *       
01101                                         PB-I-AH-TERM              
01102              PERFORM 8800-GET-COMP-RATE THRU 8800-EXIT            
01103              MOVE WS-WK-RATE        TO WS-COMMISSION              
01104                                        PB-I-AH-COMMISSION         
01105         ELSE                                                      
01106              MOVE PB-I-LF-BENEFIT-AMT TO WS-COMM-CK-AMT           
01107              PERFORM 8800-GET-COMP-RATE THRU 8800-EXIT            
01108              MOVE WS-WK-RATE        TO WS-COMMISSION              
01109                                        PB-I-LIFE-COMMISSION       
01110      ELSE                                                         
01111         IF FIRST-TIME-THROUGH                                     
01112             GO TO 6680-READ-CTBL-TABLE                            
01113         ELSE                                                      
01114             MOVE ZEROS         TO WS-SUB1.                        
01115                                                                   
01116  6680-SET-COMMISSION.                                             
01117      IF RESC-COMMISSION-PER (WS-SUB) EQUAL ZEROS                  
01118            NEXT SENTENCE                                          
01119        ELSE                                                       
01120            MOVE RESC-COMMISSION-PER (WS-SUB)                      
01121                              TO WS-COMMISSION                     
01122            IF CTBL-SW = '0540'                                    
01123                MOVE WS-COMMISSION TO PB-I-LIFE-COMMISSION         
01124            ELSE                                                   
01125                MOVE WS-COMMISSION TO PB-I-AH-COMMISSION.          
01126                                                                   
01127  6680-SEARCH-CATEGORY.                                            
01128      ADD +1                   TO WS-SUB.                          
01129      IF WS-SUB > +12                                              
01130          GO TO 6680-EXIT.                                         
01131                                                                   
01132      IF PB-CERT-NO (4:1) = RESC-COVERAGE-CAT (WS-SUB)             
01133           MOVE WS-SUB         TO WS-SUB1                          
01134           GO TO 6680-EXIT.                                        
01135                                                                   
01136      GO TO 6680-SEARCH-CATEGORY.                                  
01137                                                                   
01138  6680-EXIT.                                                       
01139      EXIT.                                                        
01140                                                                   
01141  6700-COMM-EXIT.                                                  
01142      EXIT.                                                        
01143 **END*************************************************************
01144 *      CUSTOM CODING FOR CLIENT "DMD"                             
01145 ******************************************************************
01146 *                                                                 
01147  7000-GET-RATE.                                                   
01148      CALL 'ELRATEX' USING CALCULATION-PASS-AREA.                  
01149                                                                   
01150  7000-EXIT.                                                       
01151      EXIT.                                                        
01152      EJECT                                                        
01153                                                                   
01154      EJECT                                                        
01155  7100-FIND-BENEFIT-IN-STATE.                                      
01156      MOVE 'N'                    TO BEN-SEARCH-SW.                
01157                                                                   
01158      PERFORM 7100-BENEFIT-DUMMY THRU 7100-DUMMY-EXIT              
01159          VARYING SUB3 FROM 1 BY 1 UNTIL                           
01160             ((SUB3 GREATER 50) OR                                 
01161               ((CF-ST-BENEFIT-CD (SUB3) = WS-BEN-CD)              
01162                AND (WS-LOOKUP-TYPE = CF-ST-BENEFIT-KIND (SUB3)))).
01163                                                                   
01164      IF SUB3 NOT = 51                                             
01165          IF CF-ST-BENEFIT-CD (SUB3) NOT = '00'                    
01166              MOVE 'Y'            TO BEN-SEARCH-SW.                
01167                                                                   
01168      GO TO 7199-EXIT.                                             
01169                                                                   
01170  7100-BENEFIT-DUMMY.                                              
01171                                                                   
01172  7100-DUMMY-EXIT.                                                 
01173      EXIT.                                                        
01174                                                                   
01175  7199-EXIT.                                                       
01176      EXIT.                                                        
01177      EJECT                                                        
01178  7200-FIND-BENEFIT.                                               
01179      MOVE 'N'                    TO BEN-SEARCH-SW.                
01180                                                                   
01181      PERFORM 7200-BENEFIT-DUMMY THRU 7200-DUMMY-EXIT              
01182          VARYING SUB3 FROM 1 BY 1 UNTIL                           
01183             ((SUB3 GREATER 8) OR                                  
01184             (CF-BENEFIT-CODE (SUB3) = WS-BEN-CD)).                
01185                                                                   
01186      IF SUB3 NOT = 9                                              
01187          MOVE 'Y'                TO BEN-SEARCH-SW.                
01188                                                                   
01189      GO TO 7200-EXIT.                                             
01190                                                                   
01191  7200-BENEFIT-DUMMY.                                              
01192                                                                   
CIDVAO     EXIT.
01193  7200-DUMMY-EXIT.                                                 
01194      EXIT.                                                        
01195                                                                   
01196  7200-EXIT.                                                       
01197      EXIT.                                                        
01198        EJECT                                                      
01199  7300-CHECK-FOR-OPEN-CLMS.                                        
01200                                                                   
01201      IF ELMSTR-OPEN = ' '                                         
01202          OPEN INPUT ELMSTR                                        
01203          MOVE 'X'                TO ELMSTR-OPEN                   
01204          IF ELMSTR-FILE-STATUS = '00' OR '97'                     
01205              NEXT SENTENCE                                        
01206          ELSE                                                     
01207              MOVE ELMSTR-FILE-STATUS                              
01208                                  TO WS-ABEND-FILE-STATUS          
01209              MOVE 'ELMSTR'       TO WS-FEM-FILE-NAME              
01210              MOVE EMSG           TO WS-ABEND-MESSAGE              
01211              GO TO ABEND-PGM.                                     
01212                                                                   
01213      MOVE PB-COMPANY-CD          TO MSTR-COMP-CD                  
01214                                     W-CL-COMP-CD.                 
01215      MOVE PB-CERT-NO             TO MSTR-CERT-NO                  
01216                                     W-CL-CERT-NO.                 
01217      MOVE ELMSTR-KEY             TO CL-CONTROL-BY-CERT-NO.        
01218                                                                   
01219      START ELMSTR KEY NOT LESS THAN CL-CONTROL-BY-CERT-NO.        
01220                                                                   
01221      IF ELMSTR-FILE-STATUS = '10' OR '23'                         
01222          GO TO 7300-EXIT.                                         
01223                                                                   
01224      IF ELMSTR-FILE-STATUS NOT EQUAL '00' AND '02'                
01225          GO TO 7300-CLAIMS-ERROR.                                 
01226                                                                   
01227      MOVE 'Y'                    TO W-BROWSE-SW.                  
01228                                                                   
01229  7300-NEXT-CLAIM.                                                 
01230                                                                   
01231      READ ELMSTR NEXT.                                            
01232                                                                   
01233      IF ELMSTR-FILE-STATUS = '10'                                 
01234          GO TO 7300-EXIT.                                         
01235                                                                   
01236      IF ELMSTR-FILE-STATUS NOT = '00' AND '02'                    
01237          GO TO 7300-CLAIMS-ERROR.                                 
01238                                                                   
01239      IF MSTR-COMP-CD NOT = CL-COMPANY-CD-A4  OR                   
01240         MSTR-CERT-NO NOT = CL-CERT-NO-A4                          
01241          GO TO 7300-EXIT.                                         
01242                                                                   
01243      IF PB-SV-CARRIER  NOT = CL-CARRIER       OR                  
01244         PB-SV-GROUPING NOT = CL-CERT-GROUPING OR                  
01245         PB-SV-STATE    NOT = CL-CERT-STATE    OR                  
01246         PB-ACCOUNT     NOT = CL-CERT-ACCOUNT  OR                  
01247         PB-CERT-EFF-DT NOT = CL-CERT-EFF-DT                       
01248          GO TO 7300-NEXT-CLAIM.                                   
01249                                                                   
01250      IF CL-CLAIM-TYPE = AH-OVERRIDE-L1
062904        OR 'I' OR 'G'
01251         IF CLAIM-IS-OPEN                                         
01252            MOVE ER-2768          TO WS-ERROR                      
01253            PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT             
01254            GO TO 7300-EXIT                                      
062904        ELSE
062904           IF PB-C-AH-CANCEL-DT < CL-PAID-THRU-DT
062904              MOVE ER-2756       TO WS-ERROR                      
062904              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT             
062904              GO TO 7300-EXIT
062904           END-IF
062904        END-IF
01257      ELSE                                                         
01258         IF CL-CLAIM-TYPE = LIFE-OVERRIDE-L1                      
01259            IF CLAIM-IS-OPEN                                     
01260               MOVE ER-2768       TO WS-ERROR                      
01261               PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT         
062904           ELSE
062904              IF PB-C-LF-CANCEL-DT < CL-PAID-THRU-DT
062904                 MOVE ER-2756    TO WS-ERROR                      
062904                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT             
062904                 GO TO 7300-EXIT
062904              END-IF
062904           END-IF
062904        END-IF
062904     END-IF
01263                                                                   
01264      GO TO 7300-NEXT-CLAIM.                                       
01265                                                                   
01266  7300-CLAIMS-ERROR.                                               
01267                                                                   
01268      MOVE ELMSTR-FILE-STATUS     TO WS-ABEND-FILE-STATUS.         
01269      MOVE 'ELMSTR'               TO WS-REM-FILE-NAME.             
01270      MOVE RMSG                   TO WS-ABEND-MESSAGE.             
01271      GO TO ABEND-PGM.                                             
01272                                                                   
01273  7300-EXIT.                                                       
01274      EXIT.                                                        
01275        EJECT                                                      
01276                                                                   
01277  7400-GET-REMAIN-TERM.                                            
01278      CALL 'ELRTRMX' USING CALCULATION-PASS-AREA.                  
01279                                                                   
01280  7400-EXIT.                                                       
01281      EXIT.                                                        
01282      EJECT                                                        
01283                                                                   
01284  7500-GET-REMAIN-AMOUNT.                                          
01285      CALL 'ELRAMTX' USING CALCULATION-PASS-AREA.                  
01286                                                                   
01287  7500-EXIT.                                                       
01288      EXIT.                                                        
01289      EJECT                                                        
01290                                                                   
01291  7600-GET-REFUND.                                                 
01292      CALL 'ELRFNDX' USING CALCULATION-PASS-AREA.                  
01293                                                                   
01294  7600-EXIT.                                                       
01295      EXIT.                                                        
01296      EJECT                                                        
01297                                                                   
01298  7700-CALL-DL1.                                                   
01299      MOVE 'P'                    TO DMPF-PROCESS-TYPE.            
01300      CALL 'DLB001'  USING  WS-DLB-MASTER-POLICY-FORM.             
01301                                                                   
01302  7700-EXIT.                                                       
01303      EXIT.                                                        
01304                                                                   
01305  7710-CALL-DL2.                                                   
01306      MOVE 'P'                    TO DIPF-PROCESS-TYPE.            
01307      CALL 'DLB002'  USING  WS-DLB-INTERNAL-POLICY-FORM.           
01308                                                                   
01309  7710-EXIT.                                                       
01310      EXIT.                                                        
01311                                                                   
01312  7720-CALL-DL3.                                                   
01313      MOVE 'P'                    TO DVBI-PROCESS-TYPE.            
01314      CALL  'DLB003' USING  WS-DLB-VALID-BANK-ID.                  
01315                                                                   
01316  7720-EXIT.                                                       
01317      EXIT.                                                        
01318                                                                   
01319  7730-CALL-DL7.                                                   
01320      MOVE 'P'                    TO DCUA-PROCESS-TYPE.            
01321      CALL 'DLB007'  USING  WS-DLB-CERT-UNDRWRTR-ASSIGN.           
01322                                                                   
01323  7730-EXIT.                                                       
01324      EXIT.                                                        
01325                                                                   
01326  7740-CALL-DL11.                                                  
01327      MOVE 'P'                    TO DPRB-PROCESS-TYPE.            
01328      CALL 'DLB011' USING  WS-DLB-PREM-RATES-BEN-TYPE.             
01329                                                                   
01330  7740-EXIT.                                                       
01331      EXIT.                                                        
01332                                                                   
01333  7750-CALL-DL17.                                                  
01334      MOVE 'P'                    TO DVRC-PROCESS-TYPE.            
01335      CALL 'DLB017' USING  WS-DLB-VALID-RATE-CODE.                 
01336                                                                   
01337  7750-EXIT.                                                       
01338      EXIT.                                                        
01339                                                                   
01340  7755-CALL-DL19.                                                  
01341      MOVE 'P'                    TO DOCC-PROCESS-TYPE.            
01342      CALL 'DLB019' USING  WS-DLB-ORIG-COV-CAT.                    
01343                                                                   
01344  7755-EXIT.                                                       
01345      EXIT.                                                        
01346                                                                   
01347  7760-CALL-DL21.                                                  
01348      CALL 'DLB021' USING  WS-DLB-VALID-LOAN-OFFICER.              
01349                                                                   
01350  7760-EXIT.                                                       
01351      EXIT.                                                        
01352                                                                   
01353  7770-CALL-DL22.                                                  
01354      MOVE 'P'                    TO DVSC-PROCESS-TYPE.            
01355      CALL 'DLB022' USING  WS-DLB-VALID-STATE-CODE.                
01356                                                                   
01357  7770-EXIT.                                                       
01358      EXIT.                                                        
01360      EJECT                                                        
01361                                                                   
01362  7800-REWRITE-CERT.                                               
01363      REWRITE CERTIFICATE-MASTER.                                  
01364      ADD 1 TO WCER.                                               
01365                                                                   
01366  7800-EXIT.                                                       
01367*      EXIT.                                                       
CIDVAO     EXIT.
01368                                                                   
01369  8000-WRITE-CERT.                                                 
01370                                                                   
01371      WRITE CERTIFICATE-MASTER.                                    
01372      ADD 1 TO WCER.                                               
01373      IF ELCERT-FILE-STATUS NOT = '00'                             
01374          GO TO 8000-FREE-MAIN.                                    
01375                                                                   
01376  8000-FREE-MAIN.                                                  
01377                                                                   
01378  8000-EXIT.                                                       
CIDVAO     EXIT.
01379*      EXIT.                                                       
01380       EJECT                                                       
01381                                                                   
01382  8100-READ-ERPNDM.                                                
01383      IF ERPNDM-OPEN = ' '                                         
01384         OPEN INPUT ERPNDM                                         
01385         MOVE 'X' TO ERPNDM-OPEN                                   
01386         IF ERPNDM-FILE-STATUS = '00' OR '97'                      
01387            NEXT SENTENCE                                          
01388         ELSE                                                      
01389            MOVE ERPNDM-FILE-STATUS TO WS-ABEND-FILE-STATUS        
01390            MOVE 'ERPNDM'              TO WS-FEM-FILE-NAME         
01391            MOVE EMSG TO WS-ABEND-MESSAGE                          
01392            GO TO ABEND-PGM.                                       
01393                                                                   
01394      MOVE SPACES                 TO ERPNDM-FOUND.                 
01395      MOVE PB-CONTROL-PRIMARY     TO PM-CONTROL-PRIMARY.           
01396      READ ERPNDM.                                                 
01397                                                                   
01398      IF ERPNDM-FILE-STATUS  = '00'                                
01399         CONTINUE
01400      ELSE                                                       
01401         MOVE 'X'                 TO ERPNDM-FOUND
01402         MOVE ER-2694             TO WS-ERROR
01403         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE ER-2883             TO WS-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF

           .
01405  8100-EXIT.                                                       
01406      EXIT.                                                        
01407                                                                   
01408  8110-READ-ERMAIL.                                                
01409      IF ERMAIL-OPEN = ' '                                         
01410         OPEN  I-O ERMAIL                                          
01411         MOVE 'X' TO ERMAIL-OPEN                                   
01412         IF ERMAIL-FILE-STATUS = '00' OR '97'                      
01413            NEXT SENTENCE                                          
01414         ELSE                                                      
01415             MOVE ERMAIL-FILE-STATUS TO WS-ABEND-FILE-STATUS       
01416             MOVE 'ERMAIL'              TO WS-FEM-FILE-NAME        
01417             MOVE EMSG TO WS-ABEND-MESSAGE                         
01418             GO TO ABEND-PGM.                                      
01419                                                                   
01420      MOVE SPACES                 TO ERMAIL-FOUND.                 
01421      MOVE PB-CONTROL-BY-ACCOUNT  TO MA-CONTROL-PRIMARY.           
01422      MOVE PB-SV-CARRIER          TO MA-CARRIER.                   
01423      MOVE PB-SV-GROUPING         TO MA-GROUPING.                  
01424      MOVE PB-SV-STATE            TO MA-STATE.                     
01425                                                                   
01426      READ ERMAIL.                                                 
01427                                                                   
01428      IF ERMAIL-FILE-STATUS  = '00'                                
01429          NEXT SENTENCE                                            
01430        ELSE                                                       
01431         MOVE 'X'                 TO ERMAIL-FOUND.                 
01432                                                                   
01433  8110-EXIT.                                                       
01434      EXIT.                                                        
01435                                                                   
01436  8120-REWRITE-ERMAIL.                                             
01437      REWRITE MAILING-DATA.                                        
01438                                                                   
01439  8120-EXIT.                                                       
01440      EXIT.                                                        
01441                                                                   
01442  8130-WRITE-ERMAIL.                                               
01443      WRITE MAILING-DATA.                                          
01444                                                                   
01445  8130-EXIT.                                                       
01446      EXIT.                                                        
01447                                                                   
01448  8140-UNLOCK-ERMAIL.                                              
01449      MOVE SPACES                 TO ERMAIL-FOUND.                 
01450                                                                   
01451  8140-EXIT.                                                       
01452      EXIT.                                                        
01453      EJECT                                                        
01454                                                                   
042709 8180-UPDATE-VIN-NUMBER.
042709
042709     IF ELCRTT-OPEN = ' '
042709         OPEN I-O ELCRTT
042709         MOVE 'X' TO ELCRTT-OPEN
042709         IF ELCRTT-FILE-STATUS   = '00' OR '97'                       
042709             NEXT SENTENCE                                            
042709         ELSE                                                       
042709             MOVE ELCRTT-FILE-STATUS TO WS-ABEND-FILE-STATUS
042709             MOVE 'ELCRTT'           TO WS-FEM-FILE-NAME              
042709             MOVE EMSG               TO WS-ABEND-MESSAGE              
042709             GO TO ABEND-PGM                                         
042709         END-IF
042709     END-IF.                                                         
042709
042709     MOVE CM-CONTROL-PRIMARY TO WS-ELCRTT-PRIMARY.
042709     MOVE 'C'                TO WS-ELCRTT-REC-TYPE.
042709     MOVE WS-ELCRTT-KEY      TO CS-CONTROL-PRIMARY.
042709
042709     READ ELCRTT.
042709
042709     IF ELCRTT-FILE-STATUS  = '00'                                
042709        CONTINUE
042709     ELSE
042709        GO TO 8180-NOTFND
042709     END-IF.
042709
042709     IF PB-I-VIN NOT EQUAL CS-VIN-NUMBER
042709         MOVE PB-I-VIN TO CS-VIN-NUMBER
042709         REWRITE CERTIFICATE-TRAILERS
042709     END-IF.
042709
042709     GO TO 8180-EXIT.
042709
042709 8180-NOTFND.
042709
042709     MOVE SPACES       TO CERTIFICATE-TRAILERS.
042709     MOVE 'CS'         TO CS-RECORD-ID.
042709     MOVE CM-CONTROL-PRIMARY TO WS-ELCRTT-PRIMARY.
042709     MOVE 'C'          TO WS-ELCRTT-REC-TYPE.
042709     MOVE WS-ELCRTT-KEY TO CS-CONTROL-PRIMARY.
042709     MOVE PB-I-VIN     TO CS-VIN-NUMBER.
042709     WRITE CERTIFICATE-TRAILERS.
042709
042709 8180-EXIT.                                                       
042709     EXIT.                                                        
042709                                                                  
01455  8200-READ-REIN-TABLE.                                            
01456      MOVE ERREIN-KEY TO RE-CONTROL-PRIMARY.                       
01457      READ ERREIN.                                                 
01458      ADD 1 TO RREI.                                               
01459                                                                   
01460      IF RE-CODE NOT = 'A'                                         
01461         PERFORM REIN-DATE-LOAD.                                   
01462                                                                   
01463      IF ERREIN-FILE-STATUS NOT = '00'                             
01464           GO TO 8200-NOTFND.                                      
01465                                                                   
01466      GO TO 8200-EXIT.                                             
01467                                                                   
01468  8200-NOTFND.                                                     
01469      MOVE 'N'                TO WS-REIN-READ-SW.                  
01470                                                                   
01471  8200-EXIT.                                                       
CIDVAO     EXIT.
01472*      EXIT.                                                       
01473       EJECT                                                       
01474                                                                   
01475  8300-READ-CTBL-TABLE.                                            
01476      MOVE ERCTBL-KEY TO CT-CONTROL-PRIMARY.                       
           DISPLAY ' ABOUT TO READ ERCTBL ' ERCTBL-KEY ' ' PB-ACCOUNT
              ' ' PB-CERT-NO ' ' PB-ENTRY-BATCH ' 'PB-STATE

01477      READ ERCTBL.                                                 
01478      ADD 1 TO RCTB.                                               
01479                                                                   
01480      IF ERCTBL-FILE-STATUS NOT = '00'                             
01481          GO TO 8300-NOTFND.                                       
01482                                                                   
01483      MOVE 'Y'                          TO WS-CTBL-READ-SW.        
01484                                                                   
01485      GO TO 8300-EXIT.                                             
01486                                                                   
01487  8300-NOTFND.                                                     
01488      IF CTBL-SW = '0530'                                          
01489          MOVE 'AA'                     TO WS-AH-COMP-CD           
01490          MOVE '0550'                   TO CTBL-SW                 
01491          MOVE '1'                      TO WS-CTBL-READ-SW         
01492          GO TO 8300-EXIT.                                         
01493                                                                   
01494      IF CTBL-SW = '0540'                                          
01495          MOVE 'AA'                     TO WS-LF-COMP-CD           
01496          MOVE '0550'                   TO CTBL-SW                 
01497          MOVE '1'                      TO WS-CTBL-READ-SW         
01498          GO TO 8300-EXIT.                                         
01499                                                                   
01500      IF CTBL-SW = '0550'                                          
01501          MOVE '2'                      TO WS-CTBL-READ-SW         
01502          GO TO 8300-EXIT.                                         
01503                                                                   
01504  8300-EXIT.                                                       
01505*      EXIT.                                                       
CIDVAO     EXIT.
01506       EJECT                                                       
01507                                                                   
01508  8400-GETMAIN-ERFORM.                                             
01510 *    ESTABLISH LINKAGE AREA FOR PLAN MASTER.                      
CIDVAO     EXIT.                                                        
01512  8400-EXIT.                                                       
CIDVAO     EXIT.
01513                                                                   
01514  8450-READ-FORM-MASTER.                                           
01515                                                                   
01516      MOVE ERFORM-KEY             TO FO-CONTROL-PRIMARY            
01517                                                                   
01518      START ERFORM KEY NOT LESS THAN FO-CONTROL-PRIMARY.           
01519                                                                   
01520      IF ERFORM-FILE-STATUS NOT EQUAL '00'                         
01521         GO TO 8450-FORM-NOT-FOUND.                                
01522                                                                   
01523      READ ERFORM NEXT.                                            
01524                                                                   
01525      IF ERFORM-FILE-STATUS NOT EQUAL '00'                         
01526         GO TO 8450-FORM-NOT-FOUND.                                
01527                                                                   
01528      MOVE 'Y'                    TO FORM-MASTER-SW.               
01529      GO TO 8450-EXIT.                                             
01530                                                                   
01531  8450-FORM-NOT-FOUND.                                             
01532                                                                   
01533      MOVE 'N'                    TO FORM-MASTER-SW.               
01534                                                                   
01535  8450-EXIT.                                                       
01536      EXIT.                                                        
01537                                                                   
01538  8500-DATE-CONVERT.                                               
01539      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   
01540                                                                   
01541  8500-EXIT.                                                       
01542      EXIT.                                                        
01543      EJECT                                                        
01544                                                                   
01545  8555-RELOAD-CONTROL-FILE.                                        
01546      MOVE HOLD-ELCNTL (ELCNTL-POINTER) TO  CONTROL-FILE.          
01547                                                                   
01548  8555-EXIT.                                                       
01549      EXIT.                                                        
01550                                                                   
01551  8557-GETMAIN-ERPLAN.                                             
01553 *    ESTABLISH LINKAGE AREA FOR PLAN MASTER.                      
CIDVAO     EXIT.
01555  8557-EXIT.                                                       
01556      EXIT.                                                        
01557                                                                   
01558  8560-GETMAIN.                                                    
01559       MOVE 999 TO ERACCT-POINTER.                                 
01560                                                                   
01561  8560-EXIT.                                                       
01562      EXIT.                                                        
01563                                                                   
01564  8570-START-ACCOUNT-MASTER.                                       
01565      MOVE ERACCT-KEY TO AM-CONTROL-BY-VAR-GRP.                    
01566                                                                   
01567  8570-EXIT.                                                       
01568      EXIT.                                                        
01569                                                                   
01570  8580-READ-ACCOUNT-MASTER.                                        
01571      MOVE ERACCT-KEY TO AM-CONTROL-BY-VAR-GRP.                    
01572                                                                   
01573      IF WS-BROWSE-STARTED-SW = 'Y'                                
01574          START ERACCT KEY NOT LESS THAN AM-CONTROL-BY-VAR-GRP.    
01575                                                                   
01576      IF ERACCT-FILE-STATUS = '23'                                 
01577          MOVE HIGH-VALUE TO ERACCT-KEY                            
01578          GO TO 8580-EXIT.                                         
01579                                                                   
01580      IF ERACCT-FILE-STAT-1 = '9'                                  
01581          MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS          
01582          MOVE 'ERACCT'              TO WS-REM-FILE-NAME           
01583          MOVE RMSG TO WS-ABEND-MESSAGE                            
01584          GO TO ABEND-PGM.                                         
01585                                                                   
01586      MOVE SPACE TO WS-BROWSE-STARTED-SW.                          
01587                                                                   
01588      READ ERACCT NEXT.                                            
01589      ADD 1 TO RACC.                                               
01590                                                                   
01591      COPY ELCACCTI.                                               
01592                                                                   
01593      IF ERACCT-FILE-STAT-1 = '9'                                  
01594          MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS          
01595          MOVE 'ERACCT'              TO WS-REM-FILE-NAME           
01596          MOVE RMSG TO WS-ABEND-MESSAGE                            
01597          GO TO ABEND-PGM.                                         
01598                                                                   
01599      IF ERACCT-FILE-STATUS NOT = '00'                             
01600          MOVE HIGH-VALUE TO ERACCT-KEY                            
01601          GO TO 8580-EXIT.                                         
01602                                                                   
01603      MOVE AM-CONTROL-BY-VAR-GRP TO ERACCT-KEY.                    
01604                                                                   
01605  8580-EXIT.                                                       
01606      EXIT.                                                        
01607                                                                   
100703 8582-READ-COMPENSATION-MASTER.                                   
100703     MOVE ERCOMP-KEY TO CO-CONTROL-PRIMARY
100703                                                                  
100703     READ ERCOMP
100703                                                                  
100703     IF ERCOMP-FILE-STATUS NOT = '00'                             
100703        GO TO 8582-COMP-NOT-FOUND
100703     END-IF
100703                                                                  
100703     MOVE 'Y'                    TO COMP-MASTER-SW.               
100703     GO TO 8582-EXIT
100703     .                                                            
100703 8582-COMP-NOT-FOUND.                                             
100703                                                                  
100703     MOVE 'N'                    TO COMP-MASTER-SW.               
100703                                                                  
100703 8582-EXIT.                                                       
100703     EXIT.                                                        
100703                                                                  
111204 8583-READ-ERAGTC.
030905
           MOVE ERAGTC-KEY TO AG-CONTROL-PRIMARY
                                                                        
           START ERAGTC KEY IS NOT < AG-CONTROL-PRIMARY

           IF ERAGTC-FILE-STATUS NOT = '00'
              GO TO 8583-AGTC-NOT-FOUND
           END-IF

           .                                                            
111204 8583-READ-ERAGTC-NEXT.

111204     READ ERAGTC NEXT RECORD
111204                                                                  
111204     IF ERAGTC-FILE-STATUS NOT = '00'                             
111204        GO TO 8583-AGTC-NOT-FOUND
111204     END-IF
111204                                                                  
111204*    MOVE 'Y'                    TO AGTC-MASTER-SW.               
111204     GO TO 8583-EXIT
111204     .                                                            
111204 8583-AGTC-NOT-FOUND.                                             
111204                                                                  
111204     MOVE 'N'                    TO AGTC-MASTER-SW.               
111204                                                                  
111204 8583-EXIT.                                                       
111204     EXIT.                                                        
100703                                                                  
100703 8584-READ-BANK-CROSS-REF.
100703     MOVE ERBXRF-KEY TO BK-CONTROL-PRIMARY
100703                                                                  
100703     READ ERBXRF
100703                                                                  
100703     IF ERBXRF-FILE-STATUS NOT = '00'                             
100703        GO TO 8584-BXRF-NOT-FOUND
100703     END-IF
100703                                                                  
100703     MOVE 'Y'                    TO BXRF-MASTER-SW.               
100703     GO TO 8584-EXIT
100703     .                                                            
100703 8584-BXRF-NOT-FOUND.                                             
100703                                                                  
100703     MOVE 'N'                    TO BXRF-MASTER-SW.               
100703                                                                  
100703 8584-EXIT.                                                       
100703     EXIT.                                                        
100703                                                                  
01608  8585-READ-PLAN-MASTER.                                           
01609                                                                   
01610      MOVE ERPLAN-KEY             TO PL-CONTROL-PRIMARY.           
01611                                                                   
01612      READ ERPLAN.                                                 
01613                                                                   
01614      IF ERPLAN-FILE-STATUS NOT EQUAL '00'                         
01615         GO TO 8585-PLAN-NOT-FOUND.                                
01616                                                                   
01617      MOVE 'Y'                    TO PLAN-MASTER-SW.               
01618      GO TO 8585-EXIT.                                             
01619                                                                   
01620  8585-PLAN-NOT-FOUND.                                             
01621                                                                   
01622      MOVE 'N'                    TO PLAN-MASTER-SW.               
01623                                                                   
01624  8585-EXIT.                                                       
01625      EXIT.                                                        
01626                                                                   
083106 8586-READ-LOAN-OFFICER.

083106     MOVE ERLOFC-KEY             TO LO-CONTROL-PRIMARY

083106     READ ERLOFC
083106                                                                  
083106     IF ERLOFC-FILE-STATUS NOT = '00'                             
083106        MOVE 'N'                 TO LOFC-MASTER-SW
083106     ELSE
083106        MOVE 'Y'                 TO LOFC-MASTER-SW
083106     END-IF

083106     .
083106 8586-EXIT.                                                       
100703     EXIT.                                                        
100703                                                                  
01627  8590-READ-CERT-MASTER.                                           
01628      MOVE ELCERT-KEY TO CM-CONTROL-PRIMARY.                       
01629      READ ELCERT.                                                 
01630      ADD 1 TO RCER.                                               
01631                                                                   
01632      IF ELCERT-FILE-STATUS NOT = '00'                             
01633          NEXT SENTENCE                                            
01634      ELSE                                                         
01635          GO TO 8590-EXIT.                                         
01636                                                                   
01637      MOVE '1'                     TO CERT-STATUS-SW.              
01638                                                                   
01639      IF PB-COMPANY-ID = 'UCL'                                     
01640          IF WS-ERROR = ER-2799                                    
01641              GO TO 8590-EXIT.                                     
01642                                                                   
01643      IF PB-CANCELLATION                                           
01644         MOVE ER-2726              TO  WS-ERROR                    
01645         PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT                 
01646         IF ACCOUNT-MAST-FOUND                                     
01647            MOVE AM-EXPIRATION-DT  TO  PB-ACCT-EXP-DT              
01648            MOVE AM-EFFECTIVE-DT   TO  PB-ACCT-EFF-DT              
01649            GO TO 8590-EXIT                                        
01650         ELSE                                                      
01651            GO TO 8590-EXIT.                                       
01652                                                                   
01653      EJECT                                                        
01654                                                                   
01655  8590-EXIT.                                                       
01656      EXIT.                                                        
01657                                                                   
01658      EJECT                                                        
01659                                                                   
01660  8800-GET-COMP-RATE.                                              
01661      MOVE +1 TO IB.                                               
01662      IF WS-COMM-CK-AMT > CT-TBF (1)                               
01663          ADD +9 TO IB                                             
01664          IF WS-COMM-CK-AMT > CT-TBF (2)                           
01665              ADD +9 TO IB                                         
01666              IF WS-COMM-CK-AMT > CT-TBF (3)                       
01667                 MOVE ZEROS       TO WS-WK-RATE                    
01668                 GO TO 8800-EXIT.                                  
01669                                                                   
01670      IF PB-I-AGE > CT-AGE (1)                                     
01671          ADD +3 TO IB                                             
01672          IF PB-I-AGE > CT-AGE (2)                                 
01673              ADD +3 TO IB                                         
01674              IF PB-I-AGE > CT-AGE (3)                             
01675                 MOVE ZEROS       TO WS-WK-RATE                    
01676                 GO TO 8800-EXIT.                                  
01677                                                                   
01678      IF WS-WORK-TERM > CT-TRM (1)                                 
01679          ADD +1 TO IB                                             
01680          IF WS-WORK-TERM > CT-TRM (2)                             
01681              ADD +1 TO IB                                         
01682              IF WS-WORK-TERM > CT-TRM (3)                         
01683                 MOVE ZEROS       TO WS-WK-RATE                    
01684                 GO TO 8800-EXIT.                                  
01685                                                                   
01686      IF CT-RT (IB) NOT NUMERIC                                    
01687          MOVE CT-RT-R (IB) TO CTBL-TABLE                          
01688          PERFORM 8300-READ-CTBL-TABLE THRU 8300-EXIT              
01689          GO TO 8800-GET-COMP-RATE.                                
01690                                                                   
01691      MOVE CT-RT (IB) TO WS-WK-RATE.                               
01692                                                                   
01693  8800-EXIT.                                                       
01694      EXIT.                                                        
01695      EJECT                                                        

01696  8900-ADDRESS-FILE.                                               
CIDVAO     EXIT.                                                        
01698  8900-ADDRESS-EXIT.                                               
01699      EXIT.                                                        
01700                                                                   
01701  9000-READ-CONTROL.                                               
01702      MOVE ELCNTL-KEY TO CF-CONTROL-PRIMARY.                       
01703      START ELCNTL KEY NOT LESS THAN CF-CONTROL-PRIMARY.           
01704                                                                   
01705      IF ELCNTL-FILE-STATUS NOT = '00'                             
01706          GO TO 9000-NOTFND.                                       
01707                                                                   
01708      READ ELCNTL NEXT.                                            
01709      ADD 1 TO RCNT.                                               
01710                                                                   
01711      IF ELCNTL-FILE-STATUS NOT = '00'                             
01712          GO TO 9000-NOTFND.                                       
01713                                                                   
01714      IF CNTL-REC-TYPE = '1'                                       
01715          MOVE CONTROL-FILE TO HOLD-ELCNTL (1)                     
01716          MOVE 1            TO ELCNTL-POINTER.                     
01717                                                                   
01718      IF CNTL-REC-TYPE = '6'                                       
01719          MOVE CONTROL-FILE TO HOLD-ELCNTL (6)                     
01720          MOVE 6            TO ELCNTL-POINTER.                     
01721                                                                   
01722      GO TO 9000-EXIT.                                             
01723                                                                   
01724  9000-NOTFND.                                                     
01725      IF RC-SW-1 = '1'                                             
01726          MOVE ER-2616             TO WS-ERROR                     
01727          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
01728          GO TO 9000-EXIT.                                         
01729                                                                   
01730      IF RC-SW-1 = '2'                                             
01731          MOVE ER-2602             TO WS-ERROR                     
01732          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
01733          GO TO 9000-EXIT.                                         
01734                                                                   
01735      GO TO 9000-EXIT.                                             
01736                                                                   
01737  9000-EXIT.                                                       
CIDVAO     EXIT.
01738*      EXIT.                                                       
01739                                                                   
01740      EJECT                                                        
01741  9100-READ-CONTROL.                                               
01742      MOVE ELCNTL-KEY TO CF-CONTROL-PRIMARY.                       
01743      START ELCNTL KEY NOT LESS THAN CF-CONTROL-PRIMARY.           
01744                                                                   
01745      IF ELCNTL-FILE-STATUS NOT = '00'                             
01746          GO TO 9100-NOTFND.                                       
01747                                                                   
01748      READ ELCNTL NEXT.                                            
01749      ADD 1 TO RCNT.                                               
01750                                                                   
01751      IF ELCNTL-FILE-STATUS NOT = '00'                             
01752          GO TO 9100-NOTFND.                                       
01753                                                                   
01754      IF CNTL-REC-TYPE = '3'                                       
01755          MOVE CONTROL-FILE TO HOLD-ELCNTL (3)                     
01756          MOVE 3            TO ELCNTL-POINTER                      
01757          GO TO 9100-EXIT.                                         
01758                                                                   
01759      IF CNTL-REC-TYPE = '4'                                       
01760          MOVE CONTROL-FILE TO HOLD-ELCNTL (4)                     
01761          MOVE 4            TO ELCNTL-POINTER                      
01762          GO TO 9100-EXIT.                                         
01763                                                                   
01764      IF CNTL-REC-TYPE = '5'                                       
01765          MOVE CONTROL-FILE TO HOLD-ELCNTL (5)                     
01766          MOVE 5            TO ELCNTL-POINTER                      
01767          GO TO 9100-EXIT.                                         
01768                                                                   
01769      IF CNTL-REC-TYPE = 'L'                                       
01770          MOVE CONTROL-FILE TO HOLD-ELCNTL (10)                    
01771          MOVE 10           TO ELCNTL-POINTER                      
01772          GO TO 9100-EXIT.                                         
01773                                                                   
01774      IF CNTL-REC-TYPE = 'A'                                       
01775          MOVE CONTROL-FILE TO HOLD-ELCNTL (11)                    
01776          MOVE 11           TO ELCNTL-POINTER                      
01777          GO TO 9100-EXIT.                                         
01778                                                                   
01779  9100-NOTFND.                                                     
01780      DISPLAY 'CTL FILE NOT FOUND ' RC-SW-2 ' KEY='  ELCNTL-KEY.   
01781                                                                   
01782      IF RC-SW-2 = '1'                                             
01783         MOVE '1'                 TO LF-BENEFIT-ERROR              
01784         MOVE ER-2604             TO WS-ERROR                      
01785         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
01786         GO TO 9100-EXIT.                                          
01787                                                                   
01788      IF RC-SW-2 = '2'                                             
01789         MOVE '1'                 TO AH-BENEFIT-ERROR              
01790         MOVE ER-2605             TO WS-ERROR                      
01791         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
01792         GO TO 9100-EXIT.                                          
01793                                                                   
01794      IF RC-SW-2 = '3'                                             
01795         MOVE ER-2603             TO WS-ERROR                      
01796         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  
01797         GO TO 9100-EXIT.                                          
01798                                                                   
01799      IF RC-SW-2 = '4'                                             
01800          MOVE PB-I-LF-INPUT-CD   TO PB-I-LF-BENEFIT-CD            
01801          GO TO 9100-EXIT.                                         
01802                                                                   
01803      IF RC-SW-2 = '5'                                             
01804          MOVE PB-I-AH-INPUT-CD   TO WS-AH-INPUT-CD                
01805          IF WS-AH-INPUT-CD-1 = SPACE                              
01806              MOVE WS-AH-INPUT-CD-2-3  TO PB-I-AH-BENEFIT-CD       
01807          ELSE                                                     
01808              MOVE PB-I-AH-INPUT-CD    TO PB-I-AH-BENEFIT-CD.      
01809                                                                   
01810  9100-EXIT.                                                       
CIDVAO     EXIT.
01811*      EXIT.                                                       
01812                                                                   
01813  9200-CONTROL-GETMAIN.                                            
01815 *    ESTABLISH LINKAGE AREA FOR CONTROL FILE.                     
CIDVAO     EXIT.                                                        
01817  9200-EXIT.                                                       
01818      EXIT.                                                        
01819                                                                   
01820  9250-UNLOCK-RETURN.                                              
01821 *  DUMMIED IN THE BATCH EL517 ....                                
CIDVAO     EXIT.
01822  9250-UNLOCK-EXIT.                                                
CIDVAO     EXIT.
01823*      EXIT.                                                       
01824                                                                   
01825  9300-ACCUM-BATCH-TOTALS.                                         
01826          IF PB-ISSUE                                              
01827              ADD PB-I-LF-PREM-CALC       TO WS-LF-ISS-COMPUTED    
01828              ADD PB-I-LF-ALT-PREM-CALC   TO WS-LF-ISS-COMPUTED    
01829              ADD PB-I-LF-PREMIUM-AMT     TO WS-LF-ISS-ENTERED     
01830              ADD PB-I-LF-ALT-PREMIUM-AMT TO WS-LF-ISS-ENTERED     
01831              ADD PB-I-AH-PREM-CALC       TO WS-AH-ISS-COMPUTED    
01832              ADD PB-I-AH-PREMIUM-AMT     TO WS-AH-ISS-ENTERED     
01833              ADD 1                       TO WS-ISSUE-CNT          
01834          ELSE                                                     
01835              ADD PB-C-LF-REF-CALC        TO WS-LF-CAN-COMPUTED    
01836              ADD PB-C-LF-CANCEL-AMT      TO WS-LF-CAN-ENTERED     
01837              ADD PB-C-AH-REF-CALC        TO WS-AH-CAN-COMPUTED    
01838              ADD PB-C-AH-CANCEL-AMT      TO WS-AH-CAN-ENTERED     
01839              ADD 1                       TO WS-CANCEL-CNT.        
01840                                                                   
01841  9400-EXIT.                                                       
01842      EXIT.                                                        
01843                                                                   
01844  9990-RETURN.                                                     
01845 *    END BROUSE OF ACCOUNT FILE                                   
CIDVAO     EXIT.
CIDVAO
01847  9990-EXIT.                                                       
01848      EXIT.                                                        
01849                                                                   
01850  9999-SET-PNDB-ERROR-FLAGS.                                       
01851                                                                   
01852      IF PB-BATCH-TRAILER                                          
01853         GO TO 9999-REWRITE-RECORD.                                
01854                                                                   
01855      IF PB-COMMON-ERRORS = LOW-VALUES                             
01856         GO TO 9999-SET-ERROR-FLAGS.                               
01857                                                                   
01858  9999-FORMAT-ERRORS.                                              
01859                                                                   
01860      MOVE +0                     TO SUB1.                         
01861                                                                   
01862  9999-ERROR-LOOP.                                                 
01863                                                                   
01864      ADD +1                      TO SUB1.                         
01865                                                                   
01866      IF SUB1 GREATER THAN PB-NO-OF-ERRORS                         
01867         GO TO 9999-SET-ERROR-FLAGS.                               
01868                                                                   
01869      MOVE PB-COMMON-ERROR (SUB1) TO EM-MESSAGE-NUMBER.            
01870                                                                   
01871      READ ELCERR.                                                 
01872                                                                   
01873      IF ELCERR-FILE-STATUS NOT = '00'                             
01874         ADD +1                       TO EMI-FATAL-CTR             
01875         GO TO  9999-ERROR-LOOP.                                   
01876                                                                   
01877                                  COPY ELCERRPD.                   
01878                                                                   
01879      IF EM-ERROR-SEVERITY = 'W'                                   
01880         ADD +1                   TO EMI-WARNING-CTR               
01881         GO TO  9999-ERROR-LOOP.                                   
01882                                                                   
01883      IF EM-ERROR-SEVERITY = 'F'                                   
01884         ADD +1                   TO EMI-FORCABLE-CTR              
01885         GO TO  9999-ERROR-LOOP.                                   
01886                                                                   
01887      IF EM-ERROR-SEVERITY = 'X'                                   
01888         ADD +1                   TO EMI-FATAL-CTR.                
01889                                                                   
01890      GO TO 9999-ERROR-LOOP.                                       
01891                                                                   
01892  9999-SET-ERROR-FLAGS.                                            
01893                                                                   
01894      IF EMI-FATAL-CTR NOT = ZEROS                                 
01895         MOVE 'X'                 TO PB-FATAL-FLAG                 
01896         GO TO 9999-CONT-SET-ERROR-FLAGS.                          
01897                                                                   
01898      IF EMI-FORCABLE-CTR NOT = ZEROS                              
01899         IF PB-ISSUE                                               
01900            IF PB-ISSUE-FORCE                                      
01901               MOVE 'F'           TO PB-FORCE-ER-CD                
01902              ELSE                                                 
01903               MOVE 'X'           TO PB-FORCE-ER-CD                
01904           ELSE                                                    
01905            IF PB-CANCEL-FORCE OR PB-ALL-CANCEL-FORCED-NO-FEE      
01906               MOVE 'F'           TO PB-FORCE-ER-CD                
01907              ELSE                                                 
01908               MOVE 'X'           TO PB-FORCE-ER-CD.               
01909                                                                   
01910  9999-CONT-SET-ERROR-FLAGS.                                       
01911                                                                   
01912      IF EMI-WARNING-CTR NOT = ZEROS                               
01913         MOVE 'W'                 TO PB-WARN-ER-CD.                
01914                                                                   
01915      IF PB-UNFORCED-ERRORS OR                                     
01916         PB-FATAL-ERRORS    OR                                     
01917         PB-RECORD-ON-HOLD  OR                                     
01918         PB-RECORD-RETURNED OR                                     
01919         PB-CANCELLATION                                           
01920           NEXT SENTENCE                                           
01921         ELSE                                                      
01922           GO TO 9999-REWRITE-RECORD.                              
01923                                                                   
01924      MOVE PB-CONTROL-BY-ACCOUNT  TO ELCERT-KEY.                   
01925      MOVE PB-SV-CARRIER          TO CERT-CARRIER.                 
01926      MOVE PB-SV-GROUPING         TO CERT-GROUPING.                
01927      MOVE PB-SV-STATE            TO CERT-STATE.                   
01928                                                                   
01929      READ ELCERT.                                                 
01930                                                                   
01931      IF CERT-ADDED-BATCH OR CERT-PURGED-OFFLINE                   
01932         GO TO 9999-REWRITE-CERT-MASTER.                           
01933                                                                   
01934      IF PB-ISSUE                                                  
01935         IF PB-RECORD-RETURNED                                     
01936             MOVE '4'             TO CM-CREDIT-INTERFACE-SW-1      
01937         ELSE                                                      
01938              MOVE '2'            TO CM-CREDIT-INTERFACE-SW-1.     
01939                                                                   
01940      IF PB-CANCELLATION                                           
01941         IF PB-RECORD-RETURNED                                     
01942             MOVE '7'             TO CM-CREDIT-INTERFACE-SW-2      
01943         ELSE                                                      
01944             IF PB-C-LF-CANCEL-VOIDED OR PB-C-AH-CANCEL-VOIDED     
01945                MOVE '6'          TO CM-CREDIT-INTERFACE-SW-2      
01946             ELSE                                                  
01947                MOVE '4'          TO CM-CREDIT-INTERFACE-SW-2.     
01948                                                                   
01949  9999-REWRITE-CERT-MASTER.                                        
01950                                                                   
01951      IF WS-SW-1 = CM-CREDIT-INTERFACE-SW-1 AND                    
01952         WS-SW-2 = CM-CREDIT-INTERFACE-SW-2                        
01953         GO TO 9999-REWRITE-RECORD.                                
01954                                                                   
01955      REWRITE CERTIFICATE-MASTER.                                  
01956      ADD 1 TO WCER.                                               
01957                                                                   
01958  9999-REWRITE-RECORD.                                             
01959      IF NOT PB-BATCH-TRAILER                                      
01960          IF PB-ISSUE                                              
01961              ADD PB-I-LF-PREM-CALC   TO WS-LF-ISS-COMPUTED        
01962              ADD PB-I-LF-ALT-PREM-CALC   TO WS-LF-ISS-COMPUTED    
01963              ADD PB-I-LF-PREMIUM-AMT TO WS-LF-ISS-ENTERED         
01964              ADD PB-I-LF-ALT-PREMIUM-AMT TO WS-LF-ISS-ENTERED     
01965              ADD PB-I-AH-PREM-CALC   TO WS-AH-ISS-COMPUTED        
01966              ADD PB-I-AH-PREMIUM-AMT TO WS-AH-ISS-ENTERED         
01967              ADD 1                   TO WS-ISSUE-CNT              
01968          ELSE                                                     
01969              ADD PB-C-LF-REF-CALC    TO WS-LF-CAN-COMPUTED        
01970              ADD PB-C-LF-CANCEL-AMT  TO WS-LF-CAN-ENTERED         
01971              ADD PB-C-AH-REF-CALC    TO WS-AH-CAN-COMPUTED        
01972              ADD PB-C-AH-CANCEL-AMT  TO WS-AH-CAN-ENTERED         
01973              ADD 1                   TO WS-CANCEL-CNT             
01974      ELSE                                                         
01975          MOVE WS-LF-ISS-COMPUTED     TO PB-B-LF-ISS-PRM-COMPUTED  
01976          MOVE WS-LF-ISS-ENTERED      TO PB-B-LF-ISS-PRM-ENTERED   
01977          MOVE WS-AH-ISS-COMPUTED     TO PB-B-AH-ISS-PRM-COMPUTED  
01978          MOVE WS-AH-ISS-ENTERED      TO PB-B-AH-ISS-PRM-ENTERED   
01979          MOVE WS-LF-CAN-COMPUTED     TO PB-B-LF-CAN-PRM-COMPUTED  
01980          MOVE WS-LF-CAN-ENTERED      TO PB-B-LF-CAN-PRM-ENTERED   
01981          MOVE WS-AH-CAN-COMPUTED     TO PB-B-AH-CAN-PRM-COMPUTED  
01982          MOVE WS-AH-CAN-ENTERED      TO PB-B-AH-CAN-PRM-ENTERED   
01983          MOVE WS-ISSUE-CNT           TO PB-B-ISSUE-CNT-ENTERED    
01984          MOVE WS-CANCEL-CNT          TO PB-B-CANCEL-CNT-ENTERED   
01985          MOVE ZEROS                  TO WS-LF-ISS-COMPUTED        
01986                                         WS-LF-ISS-ENTERED         
01987                                         WS-AH-ISS-COMPUTED        
01988                                         WS-AH-ISS-ENTERED         
01989                                         WS-LF-CAN-COMPUTED        
01990                                         WS-LF-CAN-ENTERED         
01991                                         WS-AH-CAN-COMPUTED        
01992                                         WS-AH-CAN-ENTERED         
01993                                         WS-ISSUE-CNT              
01994                                         WS-CANCEL-CNT             
01995          MOVE SPACE                  TO PB-FATAL-FLAG.            
01996                                                                   
01997      IF PB-BATCH-TRAILER                                          
01998          IF PB-B-LF-ISS-PRM-REMITTED =                            
01999             PB-B-LF-ISS-PRM-ENTERED   AND                         
02000             PB-B-LF-CAN-PRM-REMITTED =                            
02001             PB-B-LF-CAN-PRM-ENTERED   AND                         
02002             PB-B-AH-ISS-PRM-REMITTED =                            
02003             PB-B-AH-ISS-PRM-ENTERED   AND                         
02004             PB-B-AH-CAN-PRM-REMITTED =                            
02005             PB-B-AH-CAN-PRM-ENTERED   AND                         
02006             PB-B-ISSUE-CNT-REMITTED  =                            
02007             PB-B-ISSUE-CNT-ENTERED    AND                         
02008             PB-B-CANCEL-CNT-REMITTED =                            
02009             PB-B-CANCEL-CNT-ENTERED                               
02010              MOVE SPACE          TO PB-OUT-BAL-CD                 
02011            ELSE                                                   
02012              MOVE 'O'            TO PB-OUT-BAL-CD.                
02013                                                                   
02014      REWRITE PENDING-BUSINESS.                                    
02015                                                                   
02016      ADD 1 TO WPND.                                               
02017                                                                   
02018      GO TO 0100-READ-PENDING.                                     
02019                                                                   
02020  9999-EXIT.                                                       
02021      EXIT.                                                        
02022                                                                   
02023 ******************************************************************
02024 *                                                                *
02025 *            E R R O R   F O R M A T   R O U T I N E             *
02026 *                                                                *
02027 ******************************************************************
02028                                                                   
02029  9900-ERROR-FORMAT.                                               
02030                                                                   
02031      IF WS-ERROR = PB-COMMON-ERROR (1)  OR                        
02032                    PB-COMMON-ERROR (2)  OR                        
02033                    PB-COMMON-ERROR (3)  OR                        
02034                    PB-COMMON-ERROR (4)  OR                        
02035                    PB-COMMON-ERROR (5)  OR                        
02036                    PB-COMMON-ERROR (6)  OR                        
02037                    PB-COMMON-ERROR (7)  OR                        
02038                    PB-COMMON-ERROR (8)  OR                        
02039                    PB-COMMON-ERROR (9)  OR                        
02040                    PB-COMMON-ERROR (10)                           
02041         GO TO 9900-EXIT.                                          
02042                                                                   
02043      IF PB-NO-OF-ERRORS = +10                                     
02044         GO TO 9900-EXIT.                                          
02045                                                                   
02046      ADD +1                      TO PB-NO-OF-ERRORS.              
02047      MOVE WS-ERROR               TO PB-COMMON-ERROR               
02048                                        (PB-NO-OF-ERRORS).         
02049                                                                   
02050                                                                   
02051  9900-EXIT.                                                       
02052      EXIT.                                                        
02053                                                                   
02054      EJECT                                                        
02055  9999-DONE.                                                       
02056 *START*************CUSTOM CODE FOR CLIENT "DMD"*************      
02057                                                                   
02058      IF DTE-CLIENT NOT EQUAL 'DMD'                                
02059           GO TO 9999-DONE-CONT.                                   
02060                                                                   
02061      CLOSE ERRESS                                                 
02062            ERRESC.                                                
02063                                                                   
02064      MOVE SPACES                 TO WS-DLB-MASTER-POLICY-FORM.    
02065      MOVE 'C'                    TO DMPF-PROCESS-TYPE.            
02066      CALL 'DLB001'  USING  WS-DLB-MASTER-POLICY-FORM.             
02067      IF DMPF-RETURN-CODE EQUAL 'Z3'                               
02068          MOVE DMPF-RETURN-CODE   TO WS-ABEND-FILE-STATUS          
02069          MOVE 'DLB001'           TO WS-FEM-FILE-NAME              
02070          MOVE CMSG               TO WS-ABEND-MESSAGE              
02071          GO TO ABEND-PGM.                                         
02072                                                                   
02073      MOVE SPACES                 TO WS-DLB-INTERNAL-POLICY-FORM.  
02074      MOVE 'C'                    TO DIPF-PROCESS-TYPE.            
02075      CALL 'DLB002'  USING  WS-DLB-INTERNAL-POLICY-FORM.           
02076      IF DIPF-RETURN-CODE EQUAL 'Z3'                               
02077          MOVE DIPF-RETURN-CODE   TO WS-ABEND-FILE-STATUS          
02078          MOVE 'DLB002'           TO WS-FEM-FILE-NAME              
02079          MOVE CMSG               TO WS-ABEND-MESSAGE              
02080          GO TO ABEND-PGM.                                         
02081                                                                   
02082      MOVE SPACES                 TO WS-DLB-VALID-BANK-ID.         
02083      MOVE 'C'                    TO DVBI-PROCESS-TYPE.            
02084      CALL  'DLB003' USING  WS-DLB-VALID-BANK-ID.                  
02085      IF DIPF-RETURN-CODE EQUAL 'Z3'                               
02086          MOVE DVBI-RETURN-CODE   TO WS-ABEND-FILE-STATUS          
02087          MOVE 'DLB003'           TO WS-FEM-FILE-NAME              
02088          MOVE CMSG               TO WS-ABEND-MESSAGE              
02089          GO TO ABEND-PGM.                                         
02090                                                                   
02091      MOVE SPACES                 TO WS-DLB-CERT-UNDRWRTR-ASSIGN.  
02092      MOVE 'C'                    TO DCUA-PROCESS-TYPE.            
02093      CALL 'DLB007'  USING  WS-DLB-CERT-UNDRWRTR-ASSIGN.           
02094      IF DCUA-RETURN-CODE EQUAL 'Z3'                               
02095          MOVE DCUA-RETURN-CODE   TO WS-ABEND-FILE-STATUS          
02096          MOVE 'DLB007'           TO WS-FEM-FILE-NAME              
02097          MOVE CMSG               TO WS-ABEND-MESSAGE              
02098          GO TO ABEND-PGM.                                         
02099                                                                   
02100      MOVE SPACES                 TO WS-DLB-PREM-RATES-BEN-TYPE.   
02101      MOVE 'C'                    TO DPRB-PROCESS-TYPE.            
02102      CALL 'DLB011' USING  WS-DLB-PREM-RATES-BEN-TYPE.             
02103      IF DPRB-RETURN-CODE EQUAL 'Z3'                               
02104          MOVE DPRB-RETURN-CODE   TO WS-ABEND-FILE-STATUS          
02105          MOVE 'DLB011'           TO WS-FEM-FILE-NAME              
02106          MOVE CMSG               TO WS-ABEND-MESSAGE              
02107          GO TO ABEND-PGM.                                         
02108                                                                   
02109      MOVE SPACES                 TO WS-DLB-VALID-RATE-CODE.       
02110      MOVE 'C'                    TO DVRC-PROCESS-TYPE.            
02111      CALL 'DLB017' USING  WS-DLB-VALID-RATE-CODE.                 
02112      IF DVRC-RETURN-CODE EQUAL 'Z3'                               
02113          MOVE DVRC-RETURN-CODE   TO WS-ABEND-FILE-STATUS          
02114          MOVE 'DLB017'           TO WS-FEM-FILE-NAME              
02115          MOVE CMSG               TO WS-ABEND-MESSAGE              
02116          GO TO ABEND-PGM.                                         
02117                                                                   
02118      MOVE SPACES                 TO WS-DLB-VALID-STATE-CODE.      
02119      MOVE 'C'                    TO DVSC-PROCESS-TYPE.            
02120      CALL 'DLB022' USING  WS-DLB-VALID-STATE-CODE.                
02121      IF DVSC-RETURN-CODE EQUAL 'Z3'                               
02122          MOVE DVSC-RETURN-CODE   TO WS-ABEND-FILE-STATUS          
02123          MOVE 'DLB022'           TO WS-FEM-FILE-NAME              
02124          MOVE CMSG               TO WS-ABEND-MESSAGE              
02125          GO TO ABEND-PGM.                                         
02126                                                                   
02127 *END***************CUSTOM CODE FOR CLIENT "DMD"*************      
02128                                                                   
02129  9999-DONE-CONT.                                                  
02130                                                                   
02131      IF ERPNDM-OPEN = 'X'                                         
02132         CLOSE ERPNDM.                                             
02133                                                                   
02134      IF ERMAIL-OPEN = 'X'                                         
02135         CLOSE ERMAIL.                                             
02136                                                                   
02137      IF ELMSTR-OPEN = 'X'                                         
02138         CLOSE ELMSTR.                                             
02139
042709     IF ELCRTT-OPEN = 'X'
042709        CLOSE ELCRTT
042709     END-IF.
042709
02140      CLOSE      ELCNTL                                            
02141                 ELCERR                                            
02142                 ERPNDB                                            
02143                 ERREIN                                            
02144                 ELCERT                                            
02145                 ERCTBL                                            
02146                 ERPLAN                                            
02147                 ERACCT
100703                ERBXRF
111204                ERAGTC
100703                ERCOMP.
02148                                                                   
02149      IF ELCNTL-FILE-STATUS NOT = '00'                             
02150          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          
02151          MOVE 'ELCNTL'              TO WS-CEM-FILE-NAME           
02152          MOVE CMSG TO WS-ABEND-MESSAGE                            
02153          GO TO ABEND-PGM.                                         
02154                                                                   
02155      IF ERPNDB-FILE-STATUS NOT = '00'                             
02156          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          
02157          MOVE 'ERPNDB'              TO WS-CEM-FILE-NAME           
02158          MOVE CMSG TO WS-ABEND-MESSAGE                            
02159          GO TO ABEND-PGM.                                         
02160                                                                   
02161      IF ERREIN-FILE-STATUS NOT = '00'                             
02162          MOVE ERREIN-FILE-STATUS TO WS-ABEND-FILE-STATUS          
02163          MOVE 'ERREIN'              TO WS-CEM-FILE-NAME           
02164          MOVE CMSG TO WS-ABEND-MESSAGE                            
02165          GO TO ABEND-PGM.                                         
02166                                                                   
02167      IF ELCERT-FILE-STATUS NOT = '00'                             
02168          MOVE ELCERT-FILE-STATUS TO WS-ABEND-FILE-STATUS          
02169          MOVE 'ELCERT'              TO WS-CEM-FILE-NAME           
02170          MOVE CMSG TO WS-ABEND-MESSAGE                            
02171          GO TO ABEND-PGM.                                         
02172                                                                   
02173      IF ERMAIL-OPEN   = 'X'                                       
02174        IF ERMAIL-FILE-STATUS NOT = '00'                           
02175            MOVE ERMAIL-FILE-STATUS TO WS-ABEND-FILE-STATUS        
02176            MOVE 'ERMAIL'              TO WS-CEM-FILE-NAME         
02177            MOVE CMSG TO WS-ABEND-MESSAGE                          
02178            GO TO ABEND-PGM.                                       
02179                                                                   
02180      IF ERPNDM-OPEN   = 'X'                                       
02181        IF ERPNDM-FILE-STATUS NOT = '00'                           
02182             MOVE ERPNDM-FILE-STATUS TO WS-ABEND-FILE-STATUS       
02183             MOVE 'ERPNDM'              TO WS-CEM-FILE-NAME        
02184             MOVE CMSG TO WS-ABEND-MESSAGE                         
02185             GO TO ABEND-PGM.                                      
02186                                                                   
042709     IF ELCRTT-OPEN = 'X'                             
042709         IF ELCRTT-FILE-STATUS NOT = '00'                             
042709             MOVE ELCRTT-FILE-STATUS TO WS-ABEND-FILE-STATUS          
042709             MOVE 'ELCRTT'              TO WS-CEM-FILE-NAME           
042709             MOVE CMSG TO WS-ABEND-MESSAGE                            
042709             GO TO ABEND-PGM
042709         END-IF
042709     END-IF.
042709                                                                  
02187      IF ERCTBL-FILE-STATUS NOT = '00'                             
02188          MOVE ERCTBL-FILE-STATUS TO WS-ABEND-FILE-STATUS          
02189          MOVE 'ERCTBL'              TO WS-CEM-FILE-NAME           
02190          MOVE CMSG TO WS-ABEND-MESSAGE                            
02191          GO TO ABEND-PGM.                                         
02192                                                                   
02193      IF ERPLAN-FILE-STATUS NOT = '00'                             
02194         MOVE ERPLAN-FILE-STATUS  TO WS-ABEND-FILE-STATUS          
02195         MOVE 'ERPLAN '           TO WS-CEM-FILE-NAME              
02196         MOVE CMSG TO WS-ABEND-MESSAGE                             
02197         GO TO ABEND-PGM.                                          
02198                                                                   
02199      IF ERACCT-FILE-STATUS NOT = '00'                             
02200          MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS          
02201          MOVE 'ERACCT'              TO WS-CEM-FILE-NAME           
02202          MOVE CMSG TO WS-ABEND-MESSAGE                            
02203          GO TO ABEND-PGM.                                         
02204                                                                   
100703     IF ERCOMP-FILE-STATUS NOT = '00'                             
100703         MOVE ERCOMP-FILE-STATUS TO WS-ABEND-FILE-STATUS          
100703         MOVE 'ERCOMP'              TO WS-CEM-FILE-NAME           
100703         MOVE CMSG TO WS-ABEND-MESSAGE                            
100703         GO TO ABEND-PGM.                                         
100703                                                                  
100703     IF ERBXRF-FILE-STATUS NOT = '00'                             
100703         MOVE ERBXRF-FILE-STATUS TO WS-ABEND-FILE-STATUS          
100703         MOVE 'ERBXRF'              TO WS-CEM-FILE-NAME           
100703         MOVE CMSG TO WS-ABEND-MESSAGE                            
100703         GO TO ABEND-PGM.                                         
100703                                                                  
111204     IF ERAGTC-FILE-STATUS NOT = '00'                             
111204         MOVE ERAGTC-FILE-STATUS TO WS-ABEND-FILE-STATUS          
111204         MOVE 'ERAGTC'              TO WS-CEM-FILE-NAME           
111204         MOVE CMSG TO WS-ABEND-MESSAGE                            
111204         GO TO ABEND-PGM.                                         
111204                                     
02205      IF ELCERR-FILE-STATUS NOT = '00'                             
02206          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          
02207          MOVE 'ELCERR'              TO WS-CEM-FILE-NAME           
02208          MOVE CMSG TO WS-ABEND-MESSAGE                            
02209          GO TO ABEND-PGM.                                         
02210                                                                   
02211      MOVE 'C'                     TO CP-IO-FUNCTION.              
02212      CALL 'ELRATEX' USING CALCULATION-PASS-AREA.                  
02213                                                                   
02214      IF IO-ERROR                                                  
02215         MOVE SPACE                TO WS-ABEND-FILE-STATUS         
02216         MOVE 'ELRATE'             TO WS-CEM-FILE-NAME             
02217         MOVE CMSG                 TO WS-ABEND-MESSAGE             
02218         GO TO ABEND-PGM.                                          
02219                                                                   
02220      OPEN OUTPUT PRINT-FILE.                                      
02221                                                                   
02222      MOVE CF-CL-MAIL-TO-NAME     TO H2-COMP.                      
02223      MOVE WS-CURRENT-DATE        TO H2-DATE.                      
02224      MOVE ALPH-DATE              TO H3-DATE.                      
02225      MOVE TRAN-CT                TO SUM-TOT.                      
02226                                                                   
02227  7000-PRT-HDG.                                                    
02228      MOVE HDG1 TO PRT   MOVE '1' TO X.                            
02229      PERFORM 7000-PRINT-LINE.                                     
02230      MOVE HDG2 TO PRT   MOVE ' ' TO X.                            
02231      PERFORM 7000-PRINT-LINE.                                     
02232      MOVE HDG3 TO PRT   MOVE ' ' TO X.                            
02233      PERFORM 7000-PRINT-LINE.                                     
02234      MOVE SUM1 TO PRT   MOVE '0' TO X.                            
02235      PERFORM 7000-PRINT-LINE.                                     
02236      GO TO 9999-END-JOB.                                          
02237                                                                   
02238  7000-HDG-XIT.                                                    
02239      EXIT.                                                        
02240                                                                   
02241  7000-PRINT-LINE.                                                 
02242                      COPY ELCPRT2X.                               
02243                                                                   
02244  REIN-DATE-LOAD.                                                  
02245      COPY ELCRENM1.                                               
02246                                                                   
02247  7000-PRINT-LINE-XIT.                                             
02248       EXIT.                                                       
02249                                                                   
02250  9999-END-JOB.                                                    
02251      CLOSE PRINT-FILE.                                            
02252              COPY ELCPRTCX.                                       
02253                                                                   
02254      GOBACK.                                                      
02255                                                                   
02256  ABEND-PGM.   COPY ELCABEND.                                      
02257                                                                   
