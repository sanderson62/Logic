00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL513 .                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 01/09/96 09:10:25.                 
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            
00008 *                           VMOD=2.017.                           
00009                                                                   
00010 *AUTHOR.     LOGIC INC.                                           
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
00026 *       GENERAL FUNCTION IS TO LOAD THE PENDING BUSINESS INPUT    
00027 *       FILE DATA FROM EL512 ONTO THE VSAM PENDING BUSINESS       
00028 *       MASTER FILE.  A SUMMARY REPORT IS GENERATED DISPLAYING    
00029 *       ANY LOAD ERRORS AND TOTALS.                               
00030                                                                   
00031 *       INPUT FILES-    PENDING NEW BUSINESS (ISSUES AND CANCELS) 
00032 *                       DATE CARD FILE                            
00033                                                                   
00034 *       OUTPUT-         DATA LOAD SUMMARY LISTING  (EL-513)       
00035 *                       PENDING BUSINESS MASTER FILE (VSAM)       
00036 *                       MAILING MASTER FILE (VSAM) - PENDING      
00037 *                       REQUEST FILE FOR A/R USERS                
00038                                                                   
011904******************************************************************
011904*                   C H A N G E   L O G
011904*
011904* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
011904*-----------------------------------------------------------------
011904*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
010904* EFFECTIVE    NUMBER
011904*-----------------------------------------------------------------
011904* 011904                   PEMA  ADD TOTAL FEE PROCESSING
032109* 032109    2009021700002  PEMA  ADD CRED BENE TO PNDB REC
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
010716* 010716  CR2015082500001  PEMA  VPP CHANGES
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
011904******************************************************************
00039      EJECT                                                        
00040  ENVIRONMENT DIVISION.                                            
00041  CONFIGURATION SECTION.                                           
00042  SPECIAL-NAMES.                                                   
00043      C02 IS LCP-CH2                                               
00044      C03 IS LCP-CH3                                               
00045      C04 IS LCP-CH4                                               
00046      C05 IS LCP-CH5                                               
00047      C06 IS LCP-CH6                                               
00048      C07 IS LCP-CH7                                               
00049      C08 IS LCP-CH8                                               
00050      C09 IS LCP-CH9                                               
00051      C10 IS LCP-CH10                                              
00052      C11 IS LCP-CH11                                              
00053      C12 IS LCP-CH12                                              
00054      S01 IS LCP-P01                                               
00055      S02 IS LCP-P02.                                              
00056                                                                   
00057  INPUT-OUTPUT SECTION.                                            
00058                                                                   
00059  FILE-CONTROL.                                                    
00060                                                                   
00061      SELECT PENDING-IN       ASSIGN TO SYS010-UT-2400-S-SYS010    
00062                              FILE STATUS IS SYS010-FILE-STATUS.   
00063                                                                   
00064      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   
00065                                                                   
00066      SELECT ERROR-REPORT     ASSIGN TO SYS009-UR-1403-S-SYS009.   
00067                                                                   
00068      SELECT DISK-DATE        ASSIGN TO SYS019-3380-S-SYS019.      
00069                                                                   
00070      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   
00071                                                                   
00072      SELECT ERPNDB           ASSIGN TO SYS021-3380-ERPNDB         
00073                              ORGANIZATION IS INDEXED              
00074                              ACCESS IS DYNAMIC                    
00075                              RECORD KEY IS PB-CONTROL-PRIMARY     
00076                              FILE STATUS IS ERPNDB-FILE-STATUS.   
00077                                                                   
00078      SELECT ELCNTL           ASSIGN TO SYS022-3380-ELCNTL         
00079                              ORGANIZATION IS INDEXED              
00080                              ACCESS IS DYNAMIC                    
00081                              RECORD KEY IS CF-CONTROL-PRIMARY     
00082                              FILE STATUS IS ELCNTL-FILE-STATUS.   
00083                                                                   
00084      SELECT ERACCT           ASSIGN TO SYS023-3380-ERACCT2        
00085                              ORGANIZATION IS INDEXED              
00086                              ACCESS IS DYNAMIC                    
00087                              RECORD KEY IS AM-CONTROL-BY-VAR-GRP  
00088                              FILE STATUS IS ERACCT-FILE-STATUS.   
00089                                                                   
00090      SELECT ELREPT           ASSIGN TO SYS024-3380-ELREPT         
00091                              ORGANIZATION IS INDEXED              
00092                              ACCESS IS DYNAMIC                    
00093                              RECORD KEY IS RF-CONTROL-PRIMARY     
00094                              FILE STATUS IS DTE-VSAM-FLAGS.       
00095                                                                   
00096      SELECT ERPNDM           ASSIGN TO SYS025-3380-ERPNDM         
00097                              ORGANIZATION IS INDEXED              
00098                              ACCESS IS DYNAMIC                    
00099                              RECORD KEY IS PM-CONTROL-PRIMARY     
00100                              FILE STATUS IS ERPNDM-FILE-STATUS.   
00101                                                                   
00102      SELECT ERCOMP           ASSIGN TO SYS026-3380-ERCOMP         
00103                              ORGANIZATION IS INDEXED              
00104                              ACCESS IS DYNAMIC                    
00105                              RECORD KEY IS CO-CONTROL-PRIMARY     
00106                              FILE STATUS IS ERCOMP-FILE-STATUS.   
00107                                                                   
00108      SELECT ERRQST           ASSIGN TO SYS027-3380-ERRQST         
00109                              ORGANIZATION IS INDEXED              
00110                              ACCESS IS DYNAMIC                    
00111                              RECORD KEY IS RQ-CONTROL-PRIMARY     
00112                              FILE STATUS IS ERRQST-FILE-STATUS.   
00113                                                                   
00114      SELECT JOURNAL-LOG-FILE                                      
00115                              ASSIGN TO SYS011-UT-2400-S-SYS011.   
00116                                                                   
00117  DATA DIVISION.                                                   
00118                                                                   
00119  FILE SECTION.                                                    
00120                                                                   
00121  FD  PENDING-IN                                                   
00122      BLOCK CONTAINS 0 RECORDS
00123      RECORDING MODE F.                                            
00124                                                                   
00125  01  PENDING-BUSINESS-IN         PIC X(585).                      
00126                                                                   
00127      EJECT                                                        
00128  FD  PRNTR                   COPY ELCPRTFD.                       
00129      EJECT                                                        
00130  FD  ERROR-REPORT.                                                
00131                                                                   
00132  01  ERROR-RPT.                                                   
00133 **   12  ERR-PRT-CTL             PIC 9(01).                       
CIDMOD     12  ERR-PRT-CTL             PIC X(01).                       
00134      12  ERR-PRT-DATA            PIC X(132).                      
00135                                                                   
00136      EJECT                                                        
00137  FD  DISK-DATE               COPY ELCDTEFD.                       
00138      EJECT                                                        
00139  FD  FICH                    COPY ELCFCHFD.                       
00140      EJECT                                                        
00141  FD  ELREPT                  COPY ELCRPTFD.                       
00142                                                                   
00143      COPY ELCREPT.                                                
00144                                                                   
00145  FD  ERPNDB.                                                      
00146                                                                   
00147      COPY ERCPNDB.                                                
00148      EJECT                                                        
00149                                                                   
00150  FD  ERPNDM.                                                      
00151                                                                   
00152      COPY ERCPNDM.                                                
00153      EJECT                                                        
00154                                                                   
00155  FD  ELCNTL.                                                      
00156                                                                   
00157      COPY ELCCNTL.                                                
00158      EJECT                                                        
00159                                                                   
00160  FD  ERACCT.                                                      
00161                                                                   
00162      COPY ERCACCT.                                                
00163                                                                   
00164      EJECT                                                        
00165  FD  ERCOMP.                                                      
00166                                                                   
00167      COPY ERCCOMP.                                                
00168                                                                   
00169      EJECT                                                        
00170  FD  ERRQST.                                                      
00171                                                                   
00172      COPY ERCRQST.                                                
00173                                                                   
00174      EJECT                                                        
00175  FD  JOURNAL-LOG-FILE                                             
00176      BLOCK CONTAINS 0 RECORDS
00177      RECORDING MODE IS V.                                         
00178                                                                   
00179      COPY ELCSLR.                                                 
00180                                                                   
00181      EJECT                                                        
00182  WORKING-STORAGE SECTION.                                         
00183  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      
00184  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.   
00185  77  LCP-ONCTR-02                  PIC S9(8) COMP-3 VALUE ZERO.   
00186  77  LCP-ASA                       PIC X.                         
00187                                                                   
00188  77  FILLER  PIC X(32) VALUE '********************************'.  
00189  77  FILLER  PIC X(32) VALUE '      EL513 WORKING-STORAGE     '.  
00190  77  FILLER  PIC X(32) VALUE '******** VMOD=2.017 ************'.  
00191                                                                   
00192  01  FILLER                          COMP-3.                      
00193      05  WS-PAGE                     PIC S9(5)   VALUE +0.        
00194      05  WS-LINE-COUNT               PIC S9(3)   VALUE +99.       
00195      05  WS-LINE-COUNT-MAX           PIC S9(3)   VALUE +58.       
00196      05  WS-ERR-PAGE                 PIC S9(5)   VALUE +0.        
00197      05  WS-ERR-LINE-COUNT           PIC S9(3)   VALUE +99.       
00198      05  WS-ERR-LINE-COUNT-MAX       PIC S9(3)   VALUE +58.       
00199      05  WS-ANSWER                   PIC S9(5)   VALUE ZERO.      
00200      05  WS-REMAINDER                PIC S9(5)   VALUE ZERO.      
00201                                                                   
00202      05  WS-ZERO                     PIC S9      VALUE +0.        
00203      05  WS-RETURN-CODE              PIC S9(3)   VALUE +0.        
00204      05  WS-ERPNDB-REC-CNT           PIC S9(7)   VALUE +0.        
00205      05  WS-PRINT-SW                 PIC S9      VALUE +0.        
00206          88  FIRST-BATCH-OF-ACCOUNT              VALUE +0.        
00207                                                                   
00208      05  WS-TIME-WRITTEN             PIC S9(7)   VALUE ZERO.      
00209      05  WS-DATE-WRITTEN             PIC S9(7)   VALUE ZERO.      
00210                                                                   
00211      05  WS-AT-ISS-CNT               PIC S9(5)    VALUE +0.       
00212      05  WS-AT-CAN-CNT               PIC S9(5)    VALUE +0.       
00213      05  WS-AT-ISS-ERR-CNT           PIC S9(5)    VALUE +0.       
00214      05  WS-AT-CAN-ERR-CNT           PIC S9(5)    VALUE +0.       
00215      05  WS-AT-PREMIUM               PIC S9(7)V99 VALUE +0.       
00216      05  WS-AT-REFUND                PIC S9(7)V99 VALUE +0.       
00217      05  WS-AT-ISS-ERR-PREM          PIC S9(7)V99 VALUE +0.       
00218      05  WS-AT-CAN-ERR-REF           PIC S9(7)V99 VALUE +0.       
00219                                                                   
00220      05  WS-BA-ISS-ERR-CNT           PIC S9(5)    VALUE +0.       
00221      05  WS-BA-CAN-ERR-CNT           PIC S9(5)    VALUE +0.       
00222      05  WS-BA-LF-ISS-ERR-PREM       PIC S9(7)V99 VALUE +0.       
00223      05  WS-BA-AH-ISS-ERR-PREM       PIC S9(7)V99 VALUE +0.       
00224      05  WS-BA-LF-CAN-ERR-REF        PIC S9(7)V99 VALUE +0.       
00225      05  WS-BA-AH-CAN-ERR-REF        PIC S9(7)V99 VALUE +0.       
00226                                                                   
00227      05  WS-ISS-PRM-ENTERED          PIC S9(7)V99 VALUE +0.       
00228      05  WS-CAN-PRM-ENTERED          PIC S9(7)V99 VALUE +0.       
00229      05  WS-ISS-ERR-PREMIUM          PIC S9(7)V99 VALUE +0.       
00230      05  WS-CAN-ERR-REFUND           PIC S9(7)V99 VALUE +0.       
00231                                                                   
00232      05  WS-TOT-ISS-CNT              PIC S9(5)    VALUE +0.       
00233      05  WS-TOT-CAN-CNT              PIC S9(5)    VALUE +0.       
00234      05  WS-TOT-ISS-ERR-CNT          PIC S9(5)    VALUE +0.       
00235      05  WS-TOT-CAN-ERR-CNT          PIC S9(5)    VALUE +0.       
00236      05  WS-TOT-PREMIUM              PIC S9(7)V99 VALUE +0.       
00237      05  WS-TOT-REFUND               PIC S9(7)V99 VALUE +0.       
00238      05  WS-TOT-ISS-ERR-PREMIUM      PIC S9(7)V99 VALUE +0.       
00239      05  WS-TOT-CAN-ERR-REFUND       PIC S9(7)V99 VALUE +0.       
00240                                                                   
00241      05  WS-CARR-TOT-ISS-CNT         PIC S9(5)    VALUE +0.       
00242      05  WS-CARR-TOT-CAN-CNT         PIC S9(5)    VALUE +0.       
00243      05  WS-CARR-TOT-PREMIUM         PIC S9(7)V99 VALUE +0.       
00244      05  WS-CARR-TOT-REFUND          PIC S9(7)V99 VALUE +0.       
00245                                                                   
00246  01  FILLER.                                                      
00247      05  WS-WORK-SOC-SEC-NO.                                      
00248          10  WS-WORK-STATE           PIC X(02)   VALUE SPACES.    
00249          10  WS-WORK-ACCOUNT         PIC X(06)   VALUE SPACES.    
00250          10  WS-WORK-INITIAL1        PIC X(01)   VALUE SPACES.    
00251          10  WS-WORK-INITIAL2        PIC X(01)   VALUE SPACES.    
00252          10  WS-WORK-INITIAL3        PIC X(01)   VALUE SPACES.    
00253      05  WS-ABEND-MESSAGE            PIC X(80)   VALUE SPACES.    
00254      05  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZERO.      
00255      05  ERPNDB-FILE-STATUS          PIC XX      VALUE ZERO.      
00256      05  ERPNDM-FILE-STATUS          PIC XX      VALUE ZERO.      
00257      05  ELCNTL-FILE-STATUS          PIC XX      VALUE ZERO.      
00258      05  ERACCT-FILE-STATUS          PIC XX      VALUE ZERO.      
00259      05  ERCOMP-FILE-STATUS          PIC XX      VALUE ZERO.      
00260      05  ERRQST-FILE-STATUS          PIC XX      VALUE ZERO.      
00261      05  SYS010-FILE-STATUS          PIC XX      VALUE ZERO.      
00262                                                                   
00263      05  WS-SAVE-PRINT-RECORD        PIC X(133) VALUE SPACES.     
00264      05  WS-SAVE-PRINT-ERR-RECORD    PIC X(133) VALUE SPACES.     
00265      05  WS-CSR-CODE                 PIC X(4)   VALUE SPACES.     
00266      05  WS-ACCOUNT-NAME             PIC X(30).                   
00267                                                                   
00268      05  WS-SAVE-KEY.                                             
00269          10  WS-SAVE-CARRIER         PIC X      VALUE SPACES.     
00270          10  WS-SAVE-GROUPING        PIC X(6)   VALUE SPACES.     
00271          10  WS-SAVE-STATE           PIC XX     VALUE SPACES.     
00272          10  WS-SAVE-ACCOUNT         PIC X(10)  VALUE SPACES.     
00273          10  WS-SAVE-BATCH-NO        PIC 9(6)   VALUE ZEROS.      
00274          10  WS-SAVE-SEQ-NO          PIC S9(4)  VALUE +0  COMP.   
00275                                                                   
00276      05  WS-HOLD-CARRIER             PIC X      VALUE SPACES.     
00277                                                                   
00278      05  WS-COMPENSATION-CONTROL.                                 
00279          10  WS-COMP-COMPANY-CD                PIC X.             
00280          10  WS-COMP-CONTROL.                                     
00281              15  WS-COMP-CARRIER               PIC X.             
00282              15  WS-COMP-GROUPING              PIC X(6).          
00283              15  WS-COMP-RESP-NO               PIC X(10).         
00284              15  WS-COMP-ACCT-NO               PIC X(10).         
00285                                                                   
00286 **   05  X                           PIC 9     VALUE 0.           
CIDMOD     05  X                           PIC X     VALUE SPACE.       
00287      05  WS-PROCESSOR-ID             PIC X(4)  VALUE 'LGXX'.      
00288      05  WS-SAVE-DATE                PIC XX.                      
00289      05  WS-SAVE-TIME                PIC 9(6).                    
00290                                                                   
00291      05  WS-BATCH-NO                 PIC 9(6)  VALUE ZEROS.       
00292      05  WS-SEQ-NO                   PIC S9(4) COMP  VALUE +0.    
00293      05  WS-LAST-SEQ-NO              PIC S9(4) COMP  VALUE +0.    
00294      05  PGM-SUB                     PIC S9(4) COMP  VALUE +513.  
00295      05  ABEND-CODE                  PIC XXXX  VALUE SPACES.      
00296      05  ABEND-OPTION                PIC X     VALUE SPACES.      
00297      05  OLC-REPORT-NAME             PIC X(5)  VALUE 'EL513'.     
00298                                                                   
00299      05  WS-FIRST-BATCH-SW           PIC S99   VALUE +0.          
00300          88  FIRST-BATCH                       VALUE +0.          
00301          88  LAST-BATCH                        VALUE +99.         
00302                                                                   
00303      05  WS-FIRST-ACCOUNT-SW         PIC S99   VALUE +0.          
00304          88  FIRST-ACCOUNT                     VALUE +0.          
00305          88  LAST-ACCOUNT                      VALUE +99.         
00306                                                                   
00307      05  WS-FIRST-CARRIER-SW         PIC S99   VALUE +0.          
00308          88  FIRST-CARRIER                     VALUE +0.          
00309          88  LAST-CARRIER                      VALUE +99.         
00310                                                                   
00311      05  WS-BYPASS-BATCH-SW          PIC XX    VALUE LOW-VALUES.  
00312          88  BYPASSING-BATCH                   VALUE HIGH-VALUES. 
00313                                                                   
00314      05  WS-ERROR-TABLE.                                          
00315          10  ER-1000                 PIC X(14) VALUE              
00316              '**DUP ISSUE** '.                                    
00317          10  ER-1001                 PIC X(14) VALUE              
00318              '**DUP CANCEL**'.                                    
00319          10  ER-1002                 PIC X(14) VALUE              
00320              '**DUP BATCH **'.                                    
00321          10  ER-1003                 PIC X(14) VALUE              
00322              '**DUP MAIL ***'.                                    
00323          10  ER-1004                 PIC X(32) VALUE              
00324              '*** NO DETAIL RECORDS ADDED ***'.                   
00325          10  ER-1005                 PIC X(32) VALUE              
00326              '***DUP ERRQST RECORD - BYPASS **'.                  
00327          10  ER-1006                 PIC X(25) VALUE              
00328              '*** REISSUED CERT ***'.                             
00329                                                                   
00330      05  WS-AR-AM-ERROR              PIC X     VALUE 'N'.         
00331          88  REQUEST-ERROR                     VALUE 'Y'.         
00332                                                                   
00333      05  WS-SAVE-ACCT-AGT            PIC X(10) VALUE SPACES.      
00334      05  WS-SAVE-FIN-RESP            PIC X(10) VALUE SPACES.      
00335      05  WS-SAVE-SUMMARY             PIC X(6)  VALUE SPACES.      
00336      05  WS-SAVE-REFERENCE           PIC X(12) VALUE SPACES.      
00337      05  WS-NET-REFERENCE.                                        
00338          10  WS-REF-YR               PIC XX    VALUE SPACES.      
00339          10  WS-REF-MO               PIC XX    VALUE SPACES.      
00340          10  WS-REF-DA               PIC XX    VALUE SPACES.      
00341          10  FILLER                  PIC X(6)  VALUE SPACES.      
00342                                                                   
00343      05  WS-SAVE-AR-KEY.                                          
00344          10  WS-SAVE-AR-CARRIER      PIC X.                       
00345          10  WS-SAVE-AR-GROUP        PIC X(6).                    
00346          10  WS-SAVE-AR-STATE        PIC XX.                      
00347          10  WS-SAVE-AR-ACCOUNT      PIC X(10).                   
00348                                                                   
00349      05  SUB1                        PIC S9(3) COMP  VALUE +0.    
00350      05  SUB2                        PIC S9(3) COMP  VALUE +0.    
00351                                                                   
00352      05  AR-TABLE                    PIC X(1440).                 
00353          05  AR-ACCT-TABLE   REDEFINES AR-TABLE                   
00354                              OCCURS 32 TIMES.                     
00355              10  AR-TABLE-KEY.                                    
00356                  15  AR-TABLE-COMPANY    PIC X.                   
00357                  15  AR-TABLE-CARRIER    PIC X.                   
00358                  15  AR-TABLE-GROUP      PIC X(6).                
00359                  15  AR-TABLE-STATE      PIC XX.                  
00360                  15  AR-TABLE-ACCOUNT    PIC X(10).               
00361              10  AR-TABLE-FROM-DT        PIC XX.                  
00362              10  AR-TABLE-TO-DT          PIC XX.                  
00363              10  AR-TABLE-ACCT-AGT       PIC X(10).               
00364              10  AR-TABLE-FIN-RESP       PIC X(10).               
00365              10  AR-TABLE-USED           PIC X.                   
00366                                                                   
00367      EJECT                                                        
00368      COPY ELCDATE.                                                
00369      EJECT                                                        
00370      COPY ELCDTECX.                                               
00371      EJECT                                                        
00372      COPY ELCDTEVR.                                               
00373      EJECT                                                        
00374      COPY ELCACCTV.                                               
00375      EJECT                                                        
00376  01  WS-HEADING1.                                                 
00377      05  FILLER                      PIC X(48) VALUE '1'.         
00378      05  FILLER                      PIC X(26) VALUE              
00379          'DATA LOAD SUMMARY LISTING '.                            
00380      05  FILLER                      PIC X(46) VALUE SPACES.      
00381      05  FILLER                      PIC X(06) VALUE 'EL513A'.    
00382                                                                   
00383  01  WS-HEADING2.                                                 
00384      05  FILLER                      PIC X(45) VALUE SPACES.      
00385      05  WS-H2-CLIENT-NAME           PIC X(30) VALUE SPACES.      
00386      05  FILLER                      PIC X(45) VALUE SPACES.      
00387      05  WS-H2-DATE                  PIC X(8).                    
00388                                                                   
00389  01  WS-HEADING3.                                                 
00390      05  FILLER                      PIC X(52) VALUE SPACES.      
00391      05  WS-H3-DATE                  PIC X(18) VALUE SPACES.      
00392      05  FILLER                      PIC X(50) VALUE SPACES.      
00393      05  FILLER                      PIC X(5) VALUE 'PAGE '.      
00394      05  WS-H3-PAGE                  PIC ZZZZ9.                   
00395                                                                   
00396  01  WS-HEADING4.                                                 
00397      05  FILLER                      PIC X(02)   VALUE SPACES.    
00398      05  FILLER                      PIC X(03)   VALUE 'CAR'.     
00399      05  FILLER                      PIC X(01)   VALUE SPACES.    
00400      05  FILLER                      PIC X(05)   VALUE 'GROUP'.   
00401      05  FILLER                      PIC X(02)   VALUE SPACES.    
00402      05  FILLER                      PIC X(02)   VALUE 'ST'.      
00403      05  FILLER                      PIC X(02)   VALUE SPACES.    
00404      05  FILLER                      PIC X(07)   VALUE 'ACCOUNT'. 
00405      05  FILLER                      PIC X(03)   VALUE SPACES.    
00406      05  FILLER                      PIC X(12)   VALUE            
00407          'ACCOUNT NAME'.                                          
00408      05  FILLER                      PIC X(21)   VALUE SPACES.    
00409      05  FILLER                      PIC X(05)   VALUE 'BATCH'.   
00410      05  FILLER                      PIC X(05)   VALUE SPACES.    
00411      05  FILLER                      PIC X(05)   VALUE 'ISSUE'.   
00412      05  FILLER                      PIC X(04)   VALUE SPACES.    
00413      05  FILLER                      PIC X(06)   VALUE 'CANCEL'.  
00414      05  FILLER                      PIC X(05)   VALUE SPACES.    
00415      05  FILLER                      PIC X(05)   VALUE 'TOTAL'.   
00416      05  FILLER                      PIC X(10)   VALUE SPACES.    
00417      05  FILLER                      PIC X(05)   VALUE 'TOTAL'.   
00418                                                                   
00419  01  WS-HEADING5.                                                 
00420      05  FILLER                      PIC X(60)   VALUE SPACES.    
00421      05  FILLER                      PIC X(06)   VALUE 'NUMBER'.  
00422      05  FILLER                      PIC X(04)   VALUE SPACES.    
00423      05  FILLER                      PIC X(05)   VALUE 'COUNT'.   
00424      05  FILLER                      PIC X(04)   VALUE SPACES.    
00425      05  FILLER                      PIC X(05)   VALUE 'COUNT'.   
00426      05  FILLER                      PIC X(05)   VALUE SPACES.    
00427      05  FILLER                      PIC X(07)   VALUE 'PREMIUM'. 
00428      05  FILLER                      PIC X(09)   VALUE SPACES.    
00429      05  FILLER                      PIC X(06)   VALUE 'REFUND'.  
00430                                                                   
00431  01  WS-DETAIL1.                                                  
00432      05  WS-DETAIL1A.                                             
00433          10  P-DT1-CTL               PIC X.                       
00434          10  FILLER                  PIC XX.                      
00435          10  P-CARRIER               PIC X.                       
00436          10  FILLER                  PIC X(02).                   
00437          10  P-GROUP                 PIC X(6).                    
00438          10  FILLER                  PIC X(01).                   
00439          10  P-STATE                 PIC X(02).                   
00440          10  FILLER                  PIC X(01).                   
00441          10  P-ACCOUNT               PIC X(10).                   
00442          10  FILLER                  PIC X.                       
00443          10  P-ACCOUNT-NAME          PIC X(30).                   
00444          10  FILLER                  PIC X(03).                   
00445          10  P-BATCH                 PIC X(6).                    
00446          10  FILLER                  PIC X(04).                   
00447          10  P-ISS-CNT               PIC ZZZZ9.                   
00448          10  FILLER                  PIC X(04).                   
00449          10  P-CAN-CNT               PIC ZZZZ9.                   
00450          10  FILLER                  PIC XX.                      
00451          10  P-PREMIUM               PIC Z,ZZZ,ZZZ.99-.           
00452          10  FILLER                  PIC XX.                      
00453          10  P-REFUND                PIC Z,ZZZ,ZZZ.99-.           
00454                                                                   
00455  01  WS-TOTAL-LINE1                  REDEFINES                    
00456      WS-DETAIL1.                                                  
00457      05  P-TOT-CTL                   PIC X.                       
00458      05  FILLER                      PIC X(36).                   
00459      05  P-TOT-MSG                   PIC X(20).                   
00460      05  FILLER                      PIC X(12).                   
00461      05  P-TOT-ISS-CNT               PIC ZZ,ZZ9.                  
00462      05  FILLER                      PIC X(03).                   
00463      05  P-TOT-CAN-CNT               PIC ZZ,ZZ9.                  
00464      05  FILLER                      PIC X(02).                   
00465      05  P-TOT-PREMIUM               PIC Z,ZZZ,ZZZ.99-.           
00466      05  FILLER                      PIC X(02).                   
00467      05  P-TOT-REFUND                PIC Z,ZZZ,ZZZ.99-.           
00468                                                                   
00469      EJECT                                                        
00470  01  WS-ERR-HEADING1.                                             
00471      05  FILLER                      PIC X(49) VALUE '1'.         
00472      05  FILLER                      PIC X(26) VALUE              
00473          'DATA LOAD ERROR LISTING '.                              
00474      05  FILLER                      PIC X(45) VALUE SPACES.      
00475      05  FILLER                      PIC X(06) VALUE 'EL513B'.    
00476                                                                   
00477  01  WS-ERR-HEADING2.                                             
00478      05  FILLER                      PIC X(45) VALUE SPACES.      
00479      05  WS-ERR-H2-CLIENT-NAME       PIC X(30) VALUE SPACES.      
00480      05  FILLER                      PIC X(45) VALUE SPACES.      
00481      05  WS-ERR-H2-DATE              PIC X(8).                    
00482                                                                   
00483  01  WS-ERR-HEADING3.                                             
00484      05  FILLER                      PIC X(52) VALUE SPACES.      
00485      05  WS-ERR-H3-DATE              PIC X(18) VALUE SPACES.      
00486      05  FILLER                      PIC X(50) VALUE SPACES.      
00487      05  FILLER                      PIC X(5) VALUE 'PAGE '.      
00488      05  WS-ERR-H3-PAGE              PIC ZZZZ9.                   
00489                                                                   
00490  01  WS-ERR-HEADING4.                                             
00491      05  FILLER                      PIC X(01)   VALUE SPACES.    
00492      05  FILLER                      PIC X(03)   VALUE 'CAR'.     
00493      05  FILLER                      PIC X(01)   VALUE SPACES.    
00494      05  FILLER                      PIC X(05)   VALUE 'GROUP'.   
00495      05  FILLER                      PIC X(02)   VALUE SPACES.    
00496      05  FILLER                      PIC X(02)   VALUE 'ST'.      
00497      05  FILLER                      PIC X(02)   VALUE SPACES.    
00498      05  FILLER                      PIC X(07)   VALUE 'ACCOUNT'. 
00499      05  FILLER                      PIC X(05)   VALUE SPACES.    
00500      05  FILLER                      PIC X(07)   VALUE 'CERT NO'. 
00501      05  FILLER                      PIC X(03)   VALUE SPACES.    
00502      05  FILLER                      PIC X(07)   VALUE 'EFF/CAN'. 
00503      05  FILLER                      PIC X(02)   VALUE SPACES.    
00504      05  FILLER                      PIC X(04)   VALUE 'NAME'.    
00505      05  FILLER                      PIC X(12)   VALUE SPACES.    
00506      05  FILLER                      PIC X(05)   VALUE 'BATCH'.   
00507      05  FILLER                      PIC X(08)   VALUE SPACES.    
00508      05  FILLER                      PIC X(07)   VALUE 'PREMIUM'. 
00509      05  FILLER                      PIC X(08)   VALUE SPACES.    
00510      05  FILLER                      PIC X(06)   VALUE 'REFUND'.  
00511                                                                   
00512  01  WS-ERR-HEADING5.                                             
00513      05  FILLER                      PIC X(40)   VALUE SPACES.    
00514      05  FILLER                      PIC X(04)   VALUE 'DATE'.    
00515      05  FILLER                      PIC X(19)   VALUE SPACES.    
00516      05  FILLER                      PIC X(06)   VALUE 'NUMBER'.  
00517      05  FILLER                      PIC X(07)   VALUE SPACES.    
00518      05  FILLER                      PIC X(06)   VALUE 'AMOUNT'.  
00519      05  FILLER                      PIC X(09)   VALUE SPACES.    
00520      05  FILLER                      PIC X(06)   VALUE 'AMOUNT'.  
00521      05  FILLER                      PIC X(09)   VALUE SPACES.    
00522      05  FILLER                      PIC X(25)   VALUE            
00523          '***** ERROR MESSAGE *****'.                             
00524                                                                   
00525  01  WS-ERR-DETAIL1.                                              
00526      05  WS-ERR-DETAIL1A.                                         
00527          10  P-ERR-DT1-CTL           PIC X(01).                   
00528          10  FILLER                  PIC X(01).                   
00529          10  P-ERR-CARRIER           PIC X(01).                   
00530          10  FILLER                  PIC X(02).                   
00531          10  P-ERR-GROUP             PIC X(06).                   
00532          10  FILLER                  PIC X(01).                   
00533          10  P-ERR-STATE             PIC X(02).                   
00534          10  FILLER                  PIC X(01).                   
00535          10  P-ERR-ACCOUNT           PIC X(10).                   
00536          10  FILLER                  PIC X(01).                   
00537          10  P-ERR-CERT-NO           PIC X(11).                   
00538          10  FILLER                  PIC X(01).                   
00539          10  P-ERR-EFF-DTE           PIC X(08).                   
00540          10  FILLER                  PIC X(01).                   
00541          10  P-ERR-LNAME             PIC X(15).                   
00542          10  FILLER                  PIC X(01).                   
00543          10  P-ERR-BATCH             PIC X(06).                   
00544          10  FILLER                  PIC X(03).                   
00545          10  P-ERR-PREMIUM           PIC ZZZZ,ZZZ.99-.            
00546          10  FILLER                  PIC X(02).                   
00547          10  P-ERR-REFUND            PIC ZZZZ,ZZZ.99-.            
00548          10  FILLER                  PIC X(08).                   
00549          10  P-ERR-DESC              PIC X(25).                   
00550                                                                   
00551  01  WS-TOTAL-ERR-LINE1              REDEFINES                    
00552      WS-ERR-DETAIL1.                                              
00553      05  P-TOT-ERR-CTL               PIC X(01).                   
00554      05  FILLER                      PIC X(33).                   
00555      05  P-TOT-ERR-MSG               PIC X(20).                   
00556      05  FILLER                      PIC X(03).                   
00557      05  P-TOT-ISS-ERR-CNT           PIC ZZ,ZZ9.                  
00558      05  P-TOT-CAN-ERR-CNT           PIC ZZ,ZZ9.                  
00559      05  FILLER                      PIC X(02).                   
00560      05  P-TOT-ERR-PREMIUM           PIC Z,ZZZ,ZZ9.99-.           
00561      05  FILLER                      PIC X(01).                   
00562      05  P-TOT-ERR-REFUND            PIC Z,ZZZ,ZZ9.99-.           
00563                                                                   
00564  01  WS-CARR-DETAIL.                                              
00565      05  P-CARR-CNTL                 PIC X.                       
00566      05  P-CARR-MSG                  PIC X(25).                   
00567      05  P-CARR                      PIC X.                       
00568      05  FILLER                      PIC X(42).                   
00569      05  P-CARR-TOT-ISS-CNT          PIC ZZ,ZZ9.                  
00570      05  FILLER                      PIC X(03).                   
00571      05  P-CARR-TOT-CAN-CNT          PIC ZZ,ZZ9.                  
00572      05  FILLER                      PIC X(02).                   
00573      05  P-CARR-TOT-PREMIUM          PIC Z,ZZZ,ZZZ.99-.           
00574      05  FILLER                      PIC X(02).                   
00575      05  P-CARR-TOT-REFUND           PIC Z,ZZZ,ZZZ.99-.           
00576                                                                   
00577      EJECT                                                        
00578  PROCEDURE DIVISION.                                              
00579                                                                   
00580  0000-LOAD-DATE-CARD.        COPY ELCDTERX SUPPRESS.              
00581                                                                   
00582 **   MOVE RUN-DATE               TO WS-RUN-DATE.                  
LGC189     MOVE RUN-DATE               TO WS-RUN-DATE-N.                
00583                                                                   
00584      MOVE RUN-YR                 TO WS-REF-YR.                    
00585      MOVE RUN-MO                 TO WS-REF-MO.                    
00586      MOVE RUN-DA                 TO WS-REF-DA.                    
00587                                                                   
00588  0000-MAIN-LOGIC SECTION.                                         
00589                                                                   
00590      PERFORM OPEN-FILES.                                          
00591                                                                   
00592      PERFORM 1000-LOAD-ERPNDB-FILE.                               
00593                                                                   
00594      CLOSE ERROR-REPORT.                                          
00595                                                                   
00596      DIVIDE +2 INTO WS-PAGE GIVING WS-ANSWER                      
00597          REMAINDER WS-REMAINDER.                                  
00598                                                                   
00599      IF WS-REMAINDER IS GREATER THAN +0                           
00600          MOVE +1                     TO  WS-LINE-COUNT            
00601          MOVE '1'                    TO  PRT                      
00602          PERFORM WRITE-A-LINE.                                    
00603                                                                   
00604      OPEN INPUT ERROR-REPORT.                                     
00605                                                                   
00606      PERFORM PRINT-ERROR-REPORT.                                  
00607                                                                   
00608      PERFORM CLOSE-FILES.                                         
00609                                                                   
00610      GOBACK.                                                      
00611                                                                   
00612      EJECT                                                        
00613                                                                   
00614  1000-LOAD-ERPNDB-FILE SECTION.                                   
00615      READ PENDING-IN                                              
00616          AT END                                                   
00617              MOVE +99            TO  WS-FIRST-BATCH-SW            
00618                                      WS-FIRST-ACCOUNT-SW          
00619                                      WS-FIRST-CARRIER-SW          
00620              GO TO 1900-TOTALS.                                   
00621                                                                   
00622      MOVE PENDING-BUSINESS-IN    TO  PENDING-BUSINESS.            
00623                                                                   
00624      IF PB-ENTRY-BATCH NOT = WS-BATCH-NO                          
00625          PERFORM 7000-SEARCH-ERPNDB THRU 7000-EXIT                
00626          MOVE PB-ENTRY-BATCH     TO  WS-BATCH-NO                  
00627          MOVE PENDING-BUSINESS-IN                                 
00628                                  TO  PENDING-BUSINESS.            
00629                                                                   
00630      IF PB-CARRIER  NOT = WS-SAVE-CARRIER  OR                     
00631          PB-GROUPING NOT = WS-SAVE-GROUPING OR                    
00632          PB-STATE    NOT = WS-SAVE-STATE    OR                    
00633          PB-ACCOUNT  NOT = WS-SAVE-ACCOUNT                        
00634          PERFORM 4000-ACCOUNT-TOTALS THRU 4000-EXIT.              
00635                                                                   
00636      IF PB-CARRIER  NOT = WS-HOLD-CARRIER                         
00637          PERFORM 4500-CARRIER-TOTALS THRU 4500-EXIT.              
00638                                                                   
00639      IF PB-BATCH-TRAILER                                          
00640          PERFORM 2000-CHECK-BATCH THRU 2000-EXIT.                 
00641                                                                   
00642      IF BYPASSING-BATCH                                           
00643          GO TO 1000-LOAD-ERPNDB-FILE.                             
00644                                                                   
00645      IF PB-MAILING-DATA                                           
00646          IF MAIL-PROCESSING                                       
00647              PERFORM 3000-WRITE-ERPNDM THRU 3000-EXIT             
00648              GO TO 1000-LOAD-ERPNDB-FILE                          
00649          ELSE                                                     
00650              GO TO 1000-LOAD-ERPNDB-FILE.                         
00651                                                                   
00652      PERFORM 6000-WRITE-ERPNDB THRU 6000-EXIT.                    
00653                                                                   
00654      GO TO 1000-LOAD-ERPNDB-FILE.                                 
00655                                                                   
00656  1900-TOTALS.                                                     
00657      IF WS-LINE-COUNT = +99                                       
00658          MOVE '-  ***** NO VALID BATCHES ON FILE' TO PRT          
00659          PERFORM WRITE-A-LINE                                     
00660          GO TO 1900-EXIT.                                         
00661                                                                   
00662      PERFORM 4000-ACCOUNT-TOTALS                                  
00663         THRU 4000-EXIT.                                           
00664                                                                   
00665      PERFORM 4500-CARRIER-TOTALS                                  
00666         THRU 4500-EXIT.                                           
00667                                                                   
00668      MOVE SPACES                 TO  WS-TOTAL-LINE1.              
00669      MOVE ZERO                   TO  P-TOT-CTL.                   
00670      MOVE WS-TOT-ISS-CNT         TO  P-TOT-ISS-CNT.               
00671      MOVE WS-TOT-CAN-CNT         TO  P-TOT-CAN-CNT.               
00672      MOVE WS-TOT-PREMIUM         TO  P-TOT-PREMIUM.               
00673      MOVE WS-TOT-REFUND          TO  P-TOT-REFUND.                
00674      MOVE '*** FINAL TOTAL ***'  TO  P-TOT-MSG.                   
00675      MOVE WS-TOTAL-LINE1         TO  PRT                          
00676                                                                   
00677      PERFORM WRITE-A-LINE.                                        
00678                                                                   
00679      IF WS-TOT-ISS-ERR-CNT IS EQUAL TO +0 AND                     
00680         WS-TOT-CAN-ERR-CNT IS EQUAL TO +0                         
00681          GO TO 1900-EXIT.                                         
00682                                                                   
00683      MOVE SPACES                 TO  WS-TOTAL-ERR-LINE1.          
00684      MOVE '0'                    TO  P-TOT-ERR-CTL.               
00685                                                                   
00686      MOVE '*** FINAL TOTAL ***'  TO  P-TOT-ERR-MSG.               
00687      MOVE WS-TOT-ISS-ERR-CNT     TO  P-TOT-ISS-ERR-CNT.           
00688      MOVE WS-TOT-CAN-ERR-CNT     TO  P-TOT-CAN-ERR-CNT.           
00689      MOVE WS-TOT-ISS-ERR-PREMIUM TO  P-TOT-ERR-PREMIUM.           
00690      MOVE WS-TOT-CAN-ERR-REFUND  TO  P-TOT-ERR-REFUND.            
00691                                                                   
00692      MOVE WS-TOTAL-ERR-LINE1     TO  ERROR-RPT.                   
00693      PERFORM WRITE-ERROR-LINE.                                    
00694                                                                   
00695  1900-EXIT.                                                       
00696      EXIT.                                                        
00697      EJECT                                                        
00698  2000-CHECK-BATCH SECTION.                                        
00699      MOVE SPACES                 TO  WS-DETAIL1                   
00700                                      WS-ERR-DETAIL1.              
00701                                                                   
00702      MOVE ZERO                   TO  P-DT1-CTL                    
00703                                      ERR-PRT-CTL.                 
00704                                                                   
00705      IF WS-BA-ISS-ERR-CNT IS NOT EQUAL TO +0                      
00706          SUBTRACT WS-BA-ISS-ERR-CNT FROM PB-B-ISSUE-CNT-ENTERED.  
00707                                                                   
00708      IF WS-BA-CAN-ERR-CNT IS NOT EQUAL TO +0                      
00709          SUBTRACT WS-BA-CAN-ERR-CNT FROM PB-B-CANCEL-CNT-ENTERED. 
00710                                                                   
00711      IF WS-BA-LF-ISS-ERR-PREM IS NOT EQUAL TO +0                  
00712          SUBTRACT WS-BA-LF-ISS-ERR-PREM FROM                      
00713                   PB-B-LF-ISS-PRM-ENTERED.                        
00714                                                                   
00715      IF WS-BA-AH-ISS-ERR-PREM IS NOT EQUAL TO +0                  
00716          SUBTRACT WS-BA-AH-ISS-ERR-PREM FROM                      
00717                   PB-B-AH-ISS-PRM-ENTERED.                        
00718                                                                   
00719      IF WS-BA-LF-CAN-ERR-REF IS NOT EQUAL TO +0                   
00720          SUBTRACT WS-BA-LF-CAN-ERR-REF FROM                       
00721                   PB-B-LF-CAN-PRM-ENTERED.                        
00722                                                                   
00723      IF WS-BA-AH-CAN-ERR-REF IS NOT EQUAL TO +0                   
00724          SUBTRACT WS-BA-AH-CAN-ERR-REF FROM                       
00725                   PB-B-AH-CAN-PRM-ENTERED.                        
00726                                                                   
00727      ADD PB-B-LF-ISS-PRM-ENTERED                                  
00728          PB-B-AH-ISS-PRM-ENTERED  GIVING WS-ISS-PRM-ENTERED.      
00729                                                                   
00730      ADD PB-B-LF-CAN-PRM-ENTERED                                  
00731          PB-B-AH-CAN-PRM-ENTERED  GIVING WS-CAN-PRM-ENTERED.      
00732                                                                   
00733      IF WS-ERPNDB-REC-CNT NOT GREATER ZERO                        
00734          MOVE ER-1004            TO P-ERR-DESC.                   
00735                                                                   
00736      IF FIRST-BATCH-OF-ACCOUNT OR                                 
00737         WS-LINE-COUNT GREATER WS-LINE-COUNT-MAX                   
00738          MOVE PB-CARRIER         TO  P-CARRIER                    
00739          MOVE PB-GROUPING        TO  P-GROUP                      
00740          MOVE PB-STATE           TO  P-STATE                      
00741          MOVE PB-ACCOUNT         TO  P-ACCOUNT                    
00742          MOVE WS-ACCOUNT-NAME    TO  P-ACCOUNT-NAME               
00743          MOVE +1                 TO  WS-PRINT-SW.                 
00744                                                                   
00745      MOVE PB-ENTRY-BATCH         TO  P-BATCH.                     
00746      MOVE PB-B-ISSUE-CNT-ENTERED TO  P-ISS-CNT.                   
00747      MOVE PB-B-CANCEL-CNT-ENTERED TO P-CAN-CNT.                   
00748      MOVE WS-ISS-PRM-ENTERED     TO  P-PREMIUM.                   
00749                                                                   
00750      MOVE WS-CAN-PRM-ENTERED     TO  P-REFUND.                    
00751                                                                   
00752      MOVE WS-DETAIL1             TO  PRT.                         
00753                                                                   
00754      PERFORM WRITE-A-LINE.                                        
00755                                                                   
00756      MOVE +0                     TO  WS-BA-ISS-ERR-CNT            
00757                                      WS-BA-CAN-ERR-CNT            
00758                                      WS-BA-LF-ISS-ERR-PREM        
00759                                      WS-BA-AH-ISS-ERR-PREM        
00760                                      WS-BA-LF-CAN-ERR-REF         
00761                                      WS-BA-AH-CAN-ERR-REF.        
00762                                                                   
00763      IF BYPASSING-BATCH                                           
00764          GO TO 2000-EXIT.                                         
00765                                                                   
00766      ADD  WS-ISS-PRM-ENTERED       TO  WS-CARR-TOT-PREMIUM        
00767                                        WS-AT-PREMIUM              
00768                                        WS-TOT-PREMIUM.            
00769                                                                   
00770      ADD  WS-CAN-PRM-ENTERED       TO  WS-CARR-TOT-REFUND         
00771                                        WS-AT-REFUND               
00772                                        WS-TOT-REFUND.             
00773                                                                   
00774      ADD  PB-B-ISSUE-CNT-ENTERED   TO  WS-AT-ISS-CNT              
00775                                        WS-TOT-ISS-CNT             
00776                                        WS-CARR-TOT-ISS-CNT.       
00777                                                                   
00778      ADD  PB-B-CANCEL-CNT-ENTERED  TO  WS-CARR-TOT-CAN-CNT        
00779                                        WS-AT-CAN-CNT              
00780                                        WS-TOT-CAN-CNT.            
00781                                                                   
00782  2000-EXIT.                                                       
00783      EXIT.                                                        
00784      EJECT                                                        
00785  3000-WRITE-ERPNDM SECTION.                                       
00786      MOVE SPACES                 TO  PENDING-MAILING-DATA.        
00787                                                                   
00788      MOVE 'PM'                   TO  PM-RECORD-ID.                
00789                                                                   
00790      MOVE PB-COMPANY-CD          TO  PM-COMPANY-CD.               
00791      MOVE PB-ENTRY-BATCH         TO  PM-ENTRY-BATCH.              
00792      MOVE PB-BATCH-SEQ-NO        TO  PM-BATCH-SEQ-NO.             
00793      MOVE PB-BATCH-CHG-SEQ-NO    TO  PM-BATCH-CHG-SEQ-NO.         
00794                                                                   
00795      MOVE 'CR'                   TO  PM-SOURCE-SYSTEM.            
00796      MOVE PB-INPUT-DT            TO  PM-RECORD-ADD-DT.            
00797      MOVE PB-INPUT-BY            TO  PM-RECORD-ADDED-BY.          
00798      MOVE PB-LAST-MAINT-DT       TO  PM-LAST-MAINT-DT.            
00799      MOVE PB-LAST-MAINT-BY       TO  PM-LAST-MAINT-BY.            
00800      MOVE PB-LAST-MAINT-HHMMSS   TO  PM-LAST-MAINT-HHMMSS.        
00801                                                                   
00802      MOVE PB-M-INSURED-LAST-NAME TO  PM-INSURED-LAST-NAME.        
00803      MOVE PB-M-INSURED-FIRST-NAME TO  PM-INSURED-FIRST-NAME.      
00804      MOVE PB-M-INSURED-MID-INIT  TO  PM-INSURED-MIDDLE-INIT.      
00805      MOVE PB-M-INSURED-AGE       TO  PM-INSURED-ISSUE-AGE.        
00806      MOVE PB-M-INSURED-BIRTHDAY  TO  PM-INSURED-BIRTH-DT.         
00807      MOVE PB-M-INSURED-SEX       TO  PM-INSURED-SEX.              
00808      MOVE PB-M-INSURED-SOC-SEC-NO TO  PM-INSURED-SOC-SEC-NO.      
00809                                                                   
00810      MOVE PB-M-INSURED-ADDRESS-1 TO  PM-ADDRESS-LINE-1.           
00811      MOVE PB-M-INSURED-ADDRESS-2 TO  PM-ADDRESS-LINE-2.           
051810     MOVE PB-M-INSURED-CITY      TO PM-CITY
051810     MOVE PB-M-INSURED-STATE     TO PM-STATE
00813      MOVE PB-M-INSURED-ZIP-CODE  TO PM-ZIP.                      
00814      MOVE PB-M-INSURED-PHONE-NO  TO PM-PHONE-NO
           MOVE PB-M-CRED-BENE-NAME    TO PM-CRED-BENE-NAME
           MOVE PB-M-CRED-BENE-ADDR1   TO PM-CRED-BENE-ADDR
      ****  THE NEXT 3 LINES REALLY DON'T DO ANYTHING
      ***   UNLESS WE CHANGE EL512 TO PROCESS A TYPE 9 REC
           MOVE PB-M-CRED-BENE-ADDR2   TO PM-CRED-BENE-ADDR2
           MOVE PB-M-CRED-BENE-CITY    TO PM-CRED-BENE-CITY
           MOVE PB-M-CRED-BENE-STATE   TO PM-CRED-BENE-STATE

00815                                                                   
00816 **   MOVE  0                     TO  ERR-PRT-CTL.                 
CIDMOD     MOVE '0'                    TO  ERR-PRT-CTL.                 
00817      MOVE ER-1003                TO  P-ERR-DESC.                  
00818                                                                   
00819      MOVE 'A'                    TO  SLR-ACTION.                  
00820      MOVE 'SL'                   TO  SLR-PREFIX.                  
00821      MOVE ZERO                   TO  SLR-TASK-NUMBER.             
00822      MOVE SPACES                 TO  SLR-TERM-ID.                 
00823      MOVE 'E513'                 TO  SLR-TRAN-ID.                 
00824                                                                   
00825      MOVE 'ERPNDM  '             TO  SLR-DSID.                    
00826                                                                   
00827      MOVE SPACES TO DC-ALPHA-CENTURY.                             
00828      MOVE WS-CURRENT-DATE        TO  DC-GREG-DATE-1-EDIT.         
00829      MOVE '2'                    TO  DC-OPTION-CODE.              
00830      PERFORM 8500-DATE-CONVERSION.                                
00831      MOVE DC-JULIAN-DATE-1       TO  SLR-DATE-WRITTEN.            
00832                                                                   
00833                                                                   
00834      ACCEPT WS-TIME-OF-DAY   FROM TIME.                           
00835      MOVE WS-TIME                TO  SLR-TIME-WRITTEN.            
00836      MULTIPLY +10 BY SLR-TIME-WRITTEN.                            
00837                                                                   
00838      MOVE +23                    TO  SLR-KEY-LENGTH.              
CIDVAO**9/17/98: COMMENTED OUT STATEMENT. CAUSED OVERLAP WARNING.
CIDVAO**
00839***   MOVE SLR-KEY-LENGTH         TO  SLR-KEY-LENGTH.              
00840      MOVE SPACES                 TO  SLR-KEY.                     
00841                                                                   
00842      MOVE +250                   TO  SLR-RECORD-LENGTH.           
CIDVAO**9/17/98: COMMENTED OUT MOVE STATEMENT. CAUSED OVERLAP WARNING.
CIDVAO**
00843***   MOVE SLR-RECORD-LENGTH      TO  SLR-RECORD-LENGTH.           
00844      MOVE SPACES                 TO  SLR-RECORD-IMAGE.            
00845                                                                   
00846      MOVE PB-CONTROL-PRIMARY     TO  SLR-KEY.                     
00847      MOVE PENDING-MAILING-DATA   TO  SLR-RECORD-IMAGE.            
00848                                                                   
00849      WRITE PENDING-MAILING-DATA.                                  
00850                                                                   
00851      IF ERPNDM-FILE-STATUS = '22'                                 
00852          MOVE PB-CARRIER         TO  P-ERR-CARRIER                
00853          MOVE PB-GROUPING        TO  P-ERR-GROUP                  
00854          MOVE PB-STATE           TO  P-ERR-STATE                  
00855          MOVE PB-ACCOUNT         TO  P-ERR-ACCOUNT                
00856          MOVE PB-CERT-NO         TO  P-ERR-CERT-NO                
00857          MOVE WS-ERR-DETAIL1     TO  ERR-PRT-DATA                 
00858          PERFORM WRITE-ERROR-LINE                                 
00859          GO TO 3000-EXIT.                                         
00860                                                                   
00861      IF ERPNDM-FILE-STATUS NOT = ZEROS                            
00862          MOVE 'ERROR OCCURED WRITE - ERPNDM' TO  WS-ABEND-MESSAGE 
00863          MOVE ERPNDM-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00864          GO TO ABEND-PGM.                                         
00865                                                                   
00866      IF SLR-DATE-WRITTEN GREATER THAN WS-DATE-WRITTEN             
00867          MOVE SLR-DATE-WRITTEN   TO  WS-DATE-WRITTEN              
00868          MOVE ZERO               TO  WS-TIME-WRITTEN.             
00869                                                                   
00870  3000-CHECK-TIME-STAMP.                                           
00871      IF SLR-TIME-WRITTEN NOT GREATER THAN WS-TIME-WRITTEN         
00872          ADD +1  TO  SLR-TIME-WRITTEN                             
00873          GO TO 3000-CHECK-TIME-STAMP.                             
00874                                                                   
00875      MOVE SLR-DATE-WRITTEN       TO  WS-DATE-WRITTEN.             
00876      MOVE SLR-TIME-WRITTEN       TO  WS-TIME-WRITTEN.             
00877                                                                   
00878      WRITE SYSTEM-LOG-RECORD.                                     
00879                                                                   
00880  3000-EXIT.                                                       
00881      EXIT.                                                        
00882      EJECT                                                        
00883  4000-ACCOUNT-TOTALS SECTION.                                     
00884      IF FIRST-ACCOUNT                                             
00885          ADD +1                  TO  WS-FIRST-ACCOUNT-SW          
00886          GO TO 4120-ZERO-TOTALS.                                  
00887                                                                   
00888      MOVE SPACES                 TO  WS-TOTAL-LINE1.              
00889      MOVE ZERO                   TO  P-TOT-CTL.                   
00890      MOVE WS-AT-ISS-CNT          TO  P-TOT-ISS-CNT.               
00891      MOVE WS-AT-CAN-CNT          TO  P-TOT-CAN-CNT.               
00892      MOVE WS-AT-PREMIUM          TO  P-TOT-PREMIUM.               
00893      MOVE WS-AT-REFUND           TO  P-TOT-REFUND.                
00894      MOVE '** ACCOUNT TOTAL **'  TO  P-TOT-MSG.                   
00895      MOVE WS-TOTAL-LINE1         TO  PRT                          
00896                                                                   
00897      PERFORM WRITE-A-LINE.                                        
00898                                                                   
00899      IF WS-AT-ISS-ERR-CNT IS EQUAL TO +0 AND                      
00900         WS-AT-CAN-ERR-CNT IS EQUAL TO +0                          
00901          IF LAST-ACCOUNT                                          
00902              GO TO 4000-EXIT                                      
00903          ELSE                                                     
00904              GO TO 4120-ZERO-TOTALS.                              
00905                                                                   
00906      MOVE SPACES                 TO  WS-TOTAL-ERR-LINE1.          
00907      MOVE '0'                    TO  P-TOT-ERR-CTL.               
00908      MOVE WS-AT-ISS-ERR-CNT      TO  P-TOT-ISS-ERR-CNT.           
00909      MOVE WS-AT-CAN-ERR-CNT      TO  P-TOT-CAN-ERR-CNT.           
00910      MOVE WS-AT-ISS-ERR-PREM     TO  P-TOT-ERR-PREMIUM.           
00911      MOVE WS-AT-CAN-ERR-REF      TO  P-TOT-ERR-REFUND.            
00912      MOVE '** ACCOUNT TOTAL **'  TO  P-TOT-ERR-MSG.               
00913      MOVE WS-TOTAL-ERR-LINE1     TO  ERROR-RPT.                   
00914      PERFORM WRITE-ERROR-LINE.                                    
00915                                                                   
00916      IF LAST-ACCOUNT                                              
00917          GO TO 4000-EXIT.                                         
00918                                                                   
00919  4120-ZERO-TOTALS.                                                
00920      MOVE ZEROS                  TO  WS-AT-ISS-CNT                
00921                                      WS-AT-CAN-CNT                
00922                                      WS-AT-PREMIUM                
00923                                      WS-AT-REFUND                 
00924                                      WS-AT-ISS-ERR-CNT            
00925                                      WS-AT-CAN-ERR-CNT            
00926                                      WS-AT-ISS-ERR-PREM           
00927                                      WS-AT-CAN-ERR-REF.           
00928                                                                   
00929      MOVE PB-CARRIER             TO  WS-SAVE-CARRIER              
00930      MOVE PB-GROUPING            TO  WS-SAVE-GROUPING             
00931      MOVE PB-STATE               TO  WS-SAVE-STATE                
00932      MOVE PB-ACCOUNT             TO  WS-SAVE-ACCOUNT.             
00933                                                                   
00934      MOVE +0                     TO  WS-PRINT-SW.                 
00935                                                                   
00936      MOVE SPACES                 TO AR-TABLE.                     
00937                                                                   
00938      PERFORM 5000-PROCESS-ACCOUNT.                                
00939                                                                   
00940  4000-EXIT.                                                       
00941      EXIT.                                                        
00942      EJECT                                                        
00943                                                                   
00944  4500-CARRIER-TOTALS SECTION.                                     
00945      IF FIRST-CARRIER                                             
00946          ADD +1                  TO  WS-FIRST-CARRIER-SW          
00947          GO TO 4620-ZERO-TOTALS.                                  
00948                                                                   
00949      MOVE SPACES                 TO  WS-CARR-DETAIL.              
00950      MOVE ZERO                   TO  P-CARR-CNTL.                 
00951      MOVE WS-CARR-TOT-ISS-CNT    TO  P-CARR-TOT-ISS-CNT.          
00952      MOVE WS-CARR-TOT-CAN-CNT    TO  P-CARR-TOT-CAN-CNT.          
00953      MOVE WS-CARR-TOT-PREMIUM    TO  P-CARR-TOT-PREMIUM.          
00954      MOVE WS-CARR-TOT-REFUND     TO  P-CARR-TOT-REFUND.           
00955      MOVE '  SUB TOTAL FOR CARRIER  ' TO P-CARR-MSG.              
00956      MOVE WS-HOLD-CARRIER        TO  P-CARR.                      
00957      MOVE WS-CARR-DETAIL         TO  PRT.                         
00958                                                                   
00959      PERFORM WRITE-A-LINE.                                        
00960                                                                   
00961      IF LAST-CARRIER                                              
00962          GO TO 4500-EXIT                                          
00963      ELSE                                                         
00964          PERFORM WRITE-HEADINGS.                                  
00965                                                                   
00966  4620-ZERO-TOTALS.                                                
00967                                                                   
00968      MOVE ZEROS                  TO  WS-CARR-TOT-ISS-CNT          
00969                                      WS-CARR-TOT-CAN-CNT          
00970                                      WS-CARR-TOT-PREMIUM          
00971                                      WS-CARR-TOT-REFUND.          
00972                                                                   
00973      MOVE PB-CARRIER             TO  WS-HOLD-CARRIER.             
00974                                                                   
00975  4500-EXIT.                                                       
00976      EXIT.                                                        
00977      EJECT                                                        
00978  5000-PROCESS-ACCOUNT SECTION.                                    
00979                                                                   
00980      MOVE SPACES                 TO  WS-CSR-CODE.                 
00981      MOVE SPACES                 TO  AM-CONTROL-BY-VAR-GRP.       
00982      MOVE LOW-VALUES             TO  AM-EXPIRATION-DT.            
00983      MOVE CF-COMPANY-CD          TO  AM-COMPANY-CD-A1.            
00984      MOVE PB-ACCOUNT             TO  AM-VG-ACCOUNT.               
00985                                                                   
00986      IF CF-ST-ACCNT-CNTL                                          
00987          MOVE PB-STATE           TO  AM-VG-STATE.                 
00988                                                                   
00989      IF CF-CARR-ACCNT-CNTL                                        
00990          MOVE PB-CARRIER         TO  AM-VG-CARRIER.               
00991                                                                   
00992      IF CF-CARR-ST-ACCNT-CNTL                                     
00993          MOVE PB-CARRIER         TO  AM-VG-CARRIER                
00994          MOVE PB-STATE           TO  AM-VG-STATE.                 
00995                                                                   
00996      IF CF-CARR-GROUP-ST-ACCNT-CNTL                               
00997          MOVE PB-CARRIER         TO  AM-VG-CARRIER                
00998          MOVE PB-GROUPING        TO  AM-VG-GROUPING               
00999          MOVE PB-STATE           TO  AM-VG-STATE.                 
01000                                                                   
01001      START ERACCT                                                 
01002          KEY IS GREATER THAN AM-CONTROL-BY-VAR-GRP.               
01003                                                                   
01004      IF ERACCT-FILE-STATUS = '23'                                 
01005          GO TO 5310-INVALID-ACCOUNT.                              
01006                                                                   
01007      IF ERACCT-FILE-STATUS NOT = ZERO                             
01008          MOVE 'ERROR OCCURED START - ERACCT2' TO WS-ABEND-MESSAGE 
01009          MOVE ERACCT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01010          GO TO ABEND-PGM.                                         
01011                                                                   
01012      READ ERACCT NEXT RECORD.                                     
01013                                                                   
01014      IF ERACCT-FILE-STATUS NOT = ZERO                             
01015          MOVE 'ERROR OCCURED READNEXT - ERACCT2'                  
01016                                  TO  WS-ABEND-MESSAGE             
01017          MOVE ERACCT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01018          GO TO ABEND-PGM.                                         
01019                                                                   
01020      COPY ELCACCTI.                                               
01021                                                                   
01022      IF CF-ACCNT-CNTL                                             
01023          IF PB-ACCOUNT = AM-VG-ACCOUNT                            
01024              GO TO 5320-VALID-ACCOUNT                             
01025          ELSE                                                     
01026              GO TO 5310-INVALID-ACCOUNT.                          
01027                                                                   
01028      IF CF-ST-ACCNT-CNTL                                          
01029          IF PB-ACCOUNT = AM-VG-ACCOUNT AND                        
01030             PB-STATE   = AM-VG-STATE                              
01031              GO TO 5320-VALID-ACCOUNT                             
01032          ELSE                                                     
01033              GO TO 5310-INVALID-ACCOUNT.                          
01034                                                                   
01035      IF CF-CARR-ACCNT-CNTL                                        
01036          IF PB-ACCOUNT = AM-VG-ACCOUNT AND                        
01037             PB-CARRIER = AM-VG-CARRIER                            
01038              GO TO 5320-VALID-ACCOUNT                             
01039          ELSE                                                     
01040              GO TO 5310-INVALID-ACCOUNT.                          
01041                                                                   
01042      IF CF-CARR-ST-ACCNT-CNTL                                     
01043          IF PB-ACCOUNT = AM-VG-ACCOUNT AND                        
01044             PB-STATE   = AM-VG-STATE   AND                        
01045             PB-CARRIER = AM-VG-CARRIER                            
01046              GO TO 5320-VALID-ACCOUNT                             
01047          ELSE                                                     
01048              GO TO 5310-INVALID-ACCOUNT.                          
01049                                                                   
01050      IF CF-CARR-GROUP-ST-ACCNT-CNTL                               
01051          IF PB-ACCOUNT  = AM-VG-ACCOUNT AND                       
01052             PB-STATE    = AM-VG-STATE   AND                       
01053             PB-CARRIER  = AM-VG-CARRIER AND                       
01054             PB-GROUPING = AM-VG-GROUPING                          
01055              GO TO 5320-VALID-ACCOUNT.                            
01056                                                                   
01057  5310-INVALID-ACCOUNT.                                            
01058      MOVE '**INVALID ACCOUNT**'  TO WS-ACCOUNT-NAME               
01059      MOVE 'E'                    TO WS-AR-AM-ERROR                
01060      GO TO 5500-EXIT.                                             
01061                                                                   
01062  5320-VALID-ACCOUNT.                                              
01063                                                                   
01064      MOVE AM-CSR-CODE            TO  WS-CSR-CODE.                 
01065      MOVE AM-NAME                TO  WS-ACCOUNT-NAME.             
01066                                                                   
01067      IF NOT CF-AR-SYSTEM-USED                                     
01068          GO TO 5500-EXIT.                                         
01069                                                                   
01070      MOVE +1                     TO SUB1.                         
01071                                                                   
01072  5400-STORE-AM.                                                   
01073                                                                   
01074      IF SUB1 GREATER THAN 32                                      
01075          MOVE 'A/R TABLE SIZE EXCEEDED         '                  
01076                                  TO  WS-ABEND-MESSAGE             
01077          GO TO ABEND-PGM.                                         
01078                                                                   
01079      MOVE AM-COMPANY-CD          TO AR-TABLE-COMPANY (SUB1).      
01080      MOVE AM-CARRIER             TO AR-TABLE-CARRIER (SUB1).      
01081      MOVE AM-GROUPING            TO AR-TABLE-GROUP (SUB1).        
01082      MOVE AM-STATE               TO AR-TABLE-STATE (SUB1).        
01083      MOVE AM-ACCOUNT             TO AR-TABLE-ACCOUNT (SUB1).      
01084      MOVE AM-EXPIRATION-DT       TO AR-TABLE-TO-DT (SUB1).        
01085      MOVE AM-EFFECTIVE-DT        TO AR-TABLE-FROM-DT (SUB1).      
01086      MOVE AM-AGT (+1)            TO AR-TABLE-ACCT-AGT (SUB1).     
01087      MOVE AM-AGT (AM-REMIT-TO)   TO AR-TABLE-FIN-RESP (SUB1).     
01088                                                                   
01089      MOVE AM-CARRIER             TO WS-COMP-CARRIER.              
01090      MOVE AM-GROUPING            TO WS-COMP-GROUPING.             
01091      MOVE AM-AGT (AM-REMIT-TO)   TO WS-COMP-RESP-NO.              
01092      MOVE AM-AGT (+1)            TO WS-COMP-ACCT-NO.              
01093                                                                   
01094      READ ERACCT NEXT RECORD.                                     
01095                                                                   
01096      IF ERACCT-FILE-STATUS NOT = ZERO                             
01097          MOVE 'ERROR OCCURED READNEXT - ERACCT2'                  
01098                                  TO  WS-ABEND-MESSAGE             
01099          MOVE ERACCT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01100          GO TO ABEND-PGM.                                         
01101                                                                   
01102      IF AM-COMPANY-CD-A1  =  PB-COMPANY-CD-A1  AND                
01103         AM-VG-CARRIER     =  PB-CARRIER  AND                      
01104         AM-VG-GROUPING    =  PB-GROUPING  AND                     
01105         AM-VG-STATE       =  PB-STATE  AND                        
01106         AM-VG-ACCOUNT     =  PB-ACCOUNT                           
01107          ADD +1 TO SUB1                                           
01108          GO TO 5400-STORE-AM.                                     
01109                                                                   
01110  5500-EXIT.                                                       
01111      EXIT.                                                        
01112      EJECT                                                        
01113  6000-WRITE-ERPNDB SECTION.                                       
01114                                                                   
01115      MOVE SPACES                 TO  WS-ERR-DETAIL1.              
01116      MOVE '0'                    TO  P-ERR-DT1-CTL.               
01117                                                                   
01118      IF PB-ISSUE                                                  
01119          MOVE ER-1000            TO  P-ERR-DESC                   
01120      ELSE                                                         
01121          IF PB-CANCELLATION                                       
01122              MOVE ER-1001        TO  P-ERR-DESC.                  
01123                                                                   
01124      IF PB-BATCH-TRAILER                                          
01125          MOVE WS-SAVE-REFERENCE  TO  PB-REFERENCE.                
01126                                                                   

010716     IF DTE-CLIENT NOT = 'CID' AND 'DCC' AND 'AHL'
062121     and 'VPP' and 'FNL'
01127         IF PB-ISSUE
01128            MOVE WS-SAVE-REFERENCE TO  PB-I-REFERENCE
011904        END-IF
011904     END-IF
01129                                                                   
01130      IF PB-CANCELLATION                                           
01131         MOVE WS-SAVE-REFERENCE   TO  PB-C-REFERENCE.              
01132                                                                   
01133      IF PB-ISSUE                                                  
01134         IF PB-I-SOC-SEC-NO EQUAL SPACES OR ZEROS                  
01135                         OR LOW-VALUES OR '000-00-0000'            
01136            MOVE PB-SV-STATE      TO  WS-WORK-STATE                
01137            MOVE PB-ACCOUNT-PRIME TO  WS-WORK-ACCOUNT              
01138            MOVE PB-I-INSURED-LAST-NAME                            
01139                                  TO  WS-WORK-INITIAL3             
01140            MOVE PB-I-INSURED-MIDDLE-INIT                          
01141                                  TO  WS-WORK-INITIAL2             
01142            MOVE PB-I-INSURED-FIRST-NAME                           
01143                                  TO  WS-WORK-INITIAL1             
01144            MOVE WS-WORK-SOC-SEC-NO                                
01145                                  TO  PB-I-SOC-SEC-NO.             
01146                                                                   
01147      MOVE 'A'                    TO  SLR-ACTION.                  
01148      MOVE 'SL'                   TO  SLR-PREFIX.                  
01149      MOVE ZERO                   TO  SLR-TASK-NUMBER.             
01150      MOVE SPACES                 TO  SLR-TERM-ID.                 
01151      MOVE 'E513'                 TO  SLR-TRAN-ID.                 
01152                                                                   
01153      MOVE 'ERPNDB'               TO  SLR-DSID.                    
01154                                                                   
01155      MOVE SPACES                 TO  DC-ALPHA-CENTURY.            
01156      MOVE WS-CURRENT-DATE        TO  DC-GREG-DATE-1-EDIT.         
01157      MOVE '2'                    TO  DC-OPTION-CODE.              
01158      PERFORM 8500-DATE-CONVERSION.                                
01159      MOVE DC-JULIAN-DATE-1       TO  SLR-DATE-WRITTEN.            
01160                                                                   
01161                                                                   
01162      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      
01163                                                                   
01164      MOVE WS-TIME                TO  SLR-TIME-WRITTEN.            
01165      MULTIPLY +10 BY SLR-TIME-WRITTEN.                            
01166                                                                   
01167      MOVE +11                    TO  SLR-KEY-LENGTH.              
CIDVAO**9/17/98: COMMENTED OUT MOVE STATEMENT. CAUSED OVERLAP WARNING.
CIDVAO**
01168***   MOVE SLR-KEY-LENGTH         TO  SLR-KEY-LENGTH.              
01169      MOVE SPACES                 TO  SLR-KEY.                     
01170                                                                   
01171      MOVE +585                   TO  SLR-RECORD-LENGTH.           
CIDVAO**9/17/98: COMMENTED OUT MOVE STATEMENT. CAUSED OVERLAP WARNING.
CIDVAO**
01172***   MOVE SLR-RECORD-LENGTH      TO  SLR-RECORD-LENGTH.           
01173      MOVE SPACES                 TO  SLR-RECORD-IMAGE.            
01174                                                                   
01175      MOVE SPACES                 TO  PB-SV-CARRIER                
01176                                      PB-SV-GROUPING               
01177                                      PB-SV-STATE.                 
01178                                                                   
01179      MOVE PB-CONTROL-PRIMARY     TO PB-CONTROL-BY-ORIG-BATCH.     
01180                                                                   
01181      MOVE PB-COMPANY-CD          TO PB-CSR-COMPANY-CD.            
01182      MOVE PB-ENTRY-BATCH         TO PB-CSR-ENTRY-BATCH.           
01183      MOVE WS-CSR-CODE            TO PB-CSR-ID.                    
01184      MOVE PB-BATCH-SEQ-NO        TO PB-CSR-BATCH-SEQ-NO.          
01185      MOVE PB-BATCH-CHG-SEQ-NO    TO PB-CSR-BATCH-CHG-SEQ-NO.      
01186                                                                   
01187      MOVE PB-CONTROL-PRIMARY     TO  SLR-KEY.                     
01188      MOVE PENDING-BUSINESS       TO  SLR-RECORD-IMAGE.            
01189                                                                   
01190                                                                   
01191      WRITE PENDING-BUSINESS.                                      
01192                                                                   
01193      IF PB-BATCH-TRAILER                                          
01194          MOVE +0                     TO  WS-ERPNDB-REC-CNT        
01195          GO TO 6000-CONT-WRITE-ERPNDB.                            
01196                                                                   
01197      IF ERPNDB-FILE-STATUS IS EQUAL TO '22'                       
01198          GO TO 6000-ERPNDB-ERROR.                                 
01199                                                                   
01200      IF PB-BATCH-ENTRY IS NOT EQUAL TO 'E'                        
01201          ADD +1                      TO  WS-ERPNDB-REC-CNT        
01202          GO TO 6000-CONT-WRITE-ERPNDB.                            
01203                                                                   
01204      MOVE ER-1006                    TO  P-ERR-DESC.              
01205                                                                   
01206  6000-ERPNDB-ERROR.                                               
01207                                                                   
01208      MOVE PB-CARRIER                 TO  P-ERR-CARRIER.           
01209      MOVE PB-GROUPING                TO  P-ERR-GROUP.             
01210      MOVE PB-STATE                   TO  P-ERR-STATE.             
01211      MOVE PB-ACCOUNT                 TO  P-ERR-ACCOUNT.           
01212      MOVE PB-CERT-NO                 TO  P-ERR-CERT-NO.           
01213      MOVE PB-ENTRY-BATCH             TO  P-ERR-BATCH.             
01214      MOVE PB-CERT-EFF-DT             TO  DC-BIN-DATE-1.           
01215      MOVE ' '                        TO  DC-OPTION-CODE.          
01216      PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT.                 
01217      IF NO-CONVERSION-ERROR                                       
01218          MOVE DC-GREG-DATE-1-EDIT    TO  P-ERR-EFF-DTE            
01219      ELSE                                                         
01220          MOVE SPACES                 TO  P-ERR-EFF-DTE.           
01221                                                                   
01222      IF PB-ISSUE                                                  
01223          ADD +1                      TO  WS-BA-ISS-ERR-CNT        
01224                                          WS-AT-ISS-ERR-CNT        
01225                                          WS-TOT-ISS-ERR-CNT       
01226          ADD PB-I-LF-PREMIUM-AMT     TO  WS-ISS-ERR-PREMIUM       
01227                                          WS-BA-LF-ISS-ERR-PREM    
01228                                          WS-AT-ISS-ERR-PREM       
01229                                          WS-TOT-ISS-ERR-PREMIUM   
01230          ADD PB-I-AH-PREMIUM-AMT     TO  WS-ISS-ERR-PREMIUM       
01231                                          WS-BA-AH-ISS-ERR-PREM    
01232                                          WS-AT-ISS-ERR-PREM       
01233                                          WS-TOT-ISS-ERR-PREMIUM   
01234          MOVE PB-I-INSURED-LAST-NAME TO  P-ERR-LNAME              
01235          MOVE WS-ISS-ERR-PREMIUM     TO  P-ERR-PREMIUM.           
01236                                                                   
01237      IF PB-CANCELLATION                                           
01238          ADD +1                      TO  WS-BA-CAN-ERR-CNT        
01239                                          WS-AT-CAN-ERR-CNT        
01240                                          WS-TOT-CAN-ERR-CNT       
01241          ADD PB-C-LF-CANCEL-AMT      TO  WS-CAN-ERR-REFUND        
01242                                          WS-BA-LF-CAN-ERR-REF     
01243                                          WS-AT-CAN-ERR-REF        
01244                                          WS-TOT-CAN-ERR-REFUND    
01245          ADD PB-C-AH-CANCEL-AMT      TO  WS-CAN-ERR-REFUND        
01246                                          WS-BA-AH-CAN-ERR-REF     
01247                                          WS-AT-CAN-ERR-REF        
01248                                          WS-TOT-CAN-ERR-REFUND    
01249          MOVE PB-C-LAST-NAME         TO  P-ERR-LNAME              
01250          MOVE WS-CAN-ERR-REFUND      TO  P-ERR-REFUND.            
01251                                                                   
01252      MOVE WS-ERR-DETAIL1             TO  ERROR-RPT.               
01253      PERFORM WRITE-ERROR-LINE.                                    
01254                                                                   
01255      MOVE +0                         TO  WS-ISS-ERR-PREMIUM       
01256                                          WS-CAN-ERR-REFUND.       
01257      GO TO 6000-EXIT.                                             
01258                                                                   
01259  6000-CONT-WRITE-ERPNDB.                                          
01260                                                                   
01261      IF DTE-CLIENT = 'NCL'                                        
01262         IF PB-BATCH-TRAILER                                       
01263            IF ERPNDB-FILE-STATUS = '22'                           
01264               NEXT SENTENCE                                       
01265            ELSE                                                   
01266               IF ERPNDB-FILE-STATUS NOT = ZEROS                   
01267                  MOVE 'ERROR OCCURED WRITE - ERPNDB' TO           
01268                        WS-ABEND-MESSAGE                           
01269                  MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS 
01270                  GO TO ABEND-PGM                                  
01271               ELSE                                                
01272                  NEXT SENTENCE                                    
01273         ELSE                                                      
01274            IF ERPNDB-FILE-STATUS NOT = ZEROS                      
01275               MOVE 'ERROR OCCURED WRITE - ERPNDB' TO              
01276                     WS-ABEND-MESSAGE                              
01277               MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS    
01278               GO TO ABEND-PGM                                     
01279      ELSE                                                         
01280         IF ERPNDB-FILE-STATUS NOT = ZEROS                         
01281            MOVE 'ERROR OCCURED WRITE - ERPNDB' TO                 
01282                  WS-ABEND-MESSAGE                                 
01283            MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS       
01284            GO TO ABEND-PGM.                                       
01285                                                                   
01286      IF SLR-DATE-WRITTEN GREATER THAN WS-DATE-WRITTEN             
01287          MOVE SLR-DATE-WRITTEN   TO  WS-DATE-WRITTEN              
01288          MOVE ZERO               TO  WS-TIME-WRITTEN.             
01289                                                                   
01290  6000-CHECK-TIME-STAMP.                                           
01291      IF SLR-TIME-WRITTEN NOT GREATER THAN WS-TIME-WRITTEN         
01292          ADD +1  TO  SLR-TIME-WRITTEN                             
01293          GO TO 6000-CHECK-TIME-STAMP.                             
01294                                                                   
01295      MOVE SLR-DATE-WRITTEN       TO  WS-DATE-WRITTEN.             
01296      MOVE SLR-TIME-WRITTEN       TO  WS-TIME-WRITTEN.             
01297                                                                   
01298      WRITE SYSTEM-LOG-RECORD.                                     
01299                                                                   
01300      IF NOT CF-AR-SYSTEM-USED                                     
01301          GO TO 6000-EXIT.                                         
01302                                                                   
01303      IF PB-BATCH-TRAILER                                          
01304          GO TO 6000-REQUEST-PROCESS.                              
01305                                                                   
01306      IF REQUEST-ERROR                                             
01307          GO TO 6000-EXIT.                                         
01308                                                                   
01309      MOVE +0                     TO SUB1.                         
01310  6000-DATE-LOOP.                                                  
01311      ADD +1 TO SUB1.                                              
01312                                                                   
01313      IF SUB1 GREATER THAN 32                                      
01314          GO TO 6000-EXIT.                                         
01315                                                                   
01316      IF ((PB-CERT-EFF-DT = AR-TABLE-FROM-DT (SUB1)) OR            
01317          (PB-CERT-EFF-DT GREATER AR-TABLE-FROM-DT (SUB1)))        
01318        AND                                                        
01319          (PB-CERT-EFF-DT LESS AR-TABLE-TO-DT (SUB1))              
01320          MOVE 'Y'                TO AR-TABLE-USED (SUB1)          
01321          GO TO 6000-EXIT.                                         
01322                                                                   
01323      GO TO 6000-DATE-LOOP.                                        
01324                                                                   
01325  6000-REQUEST-PROCESS.                                            
01326                                                                   
01327      MOVE +0                     TO SUB1.                         
01328      MOVE SPACES                 TO WS-SAVE-ACCT-AGT              
01329                                     WS-SAVE-FIN-RESP              
01330                                     WS-SAVE-AR-KEY.               
01331                                                                   
01332  6000-REQUEST-PROCESS-LOOP.                                       
01333                                                                   
01334      ADD  +1 TO SUB1.                                             
01335                                                                   
01336      IF SUB1 GREATER 32                                           
01337          GO TO 6000-CHECK-AGENTS.                                 
01338                                                                   
01339      IF AR-TABLE-USED (SUB1) = 'Y'                                
01340          MOVE AR-TABLE-ACCT-AGT (SUB1)                            
01341                                  TO WS-SAVE-ACCT-AGT              
01342          MOVE AR-TABLE-FIN-RESP (SUB1)                            
01343                                  TO WS-SAVE-FIN-RESP              
01344          MOVE AR-TABLE-CARRIER (SUB1)                             
01345                                  TO WS-SAVE-AR-CARRIER            
01346          MOVE AR-TABLE-GROUP (SUB1)                               
01347                                  TO WS-SAVE-AR-GROUP              
01348          MOVE AR-TABLE-STATE (SUB1)                               
01349                                  TO WS-SAVE-AR-STATE              
01350          GO TO 6000-CHECK-AGENTS.                                 
01351                                                                   
01352      GO TO 6000-REQUEST-PROCESS-LOOP.                             
01353                                                                   
01354  6000-CHECK-AGENTS.                                               
01355                                                                   
01356      MOVE +0                     TO SUB1.                         
01357  6000-CHECK-AGENTS-LOOP.                                          
01358                                                                   
01359      ADD +1 TO SUB1.                                              
01360                                                                   
01361      IF SUB1 GREATER 32                                           
01362          GO TO 6000-OUTPUT-REQUEST.                               
01363                                                                   
01364      IF AR-TABLE-USED (SUB1) NOT = 'Y'                            
01365          GO TO 6000-CHECK-AGENTS-LOOP.                            
01366                                                                   
01367      IF AR-TABLE-ACCT-AGT (SUB1) NOT = WS-SAVE-ACCT-AGT           
01368             MOVE 'Y'             TO WS-AR-AM-ERROR                
01369             MOVE 'UNKNOWN   '    TO WS-SAVE-ACCT-AGT              
01370             GO TO 6000-OUTPUT-REQUEST.                            
01371                                                                   
01372      IF AR-TABLE-FIN-RESP (SUB1) NOT = WS-SAVE-FIN-RESP           
01373             MOVE 'Y'             TO WS-AR-AM-ERROR                
01374             MOVE 'UNKNOWN   '    TO WS-SAVE-FIN-RESP              
01375             GO TO 6000-OUTPUT-REQUEST.                            
01376                                                                   
01377      IF AR-TABLE-CARRIER (SUB1) NOT = WS-SAVE-AR-CARRIER          
01378             MOVE 'Y'             TO WS-AR-AM-ERROR                
01379             MOVE 'X'             TO WS-SAVE-AR-CARRIER            
01380             GO TO 6000-OUTPUT-REQUEST.                            
01381                                                                   
01382      IF AR-TABLE-GROUP (SUB1) NOT = WS-SAVE-AR-GROUP              
01383             MOVE 'Y'             TO WS-AR-AM-ERROR                
01384             MOVE 'UNKNWN'        TO WS-SAVE-AR-GROUP              
01385             GO TO 6000-OUTPUT-REQUEST.                            
01386                                                                   
01387      IF AR-TABLE-STATE (SUB1) NOT = WS-SAVE-AR-STATE              
01388             MOVE 'Y'             TO WS-AR-AM-ERROR                
01389             MOVE 'XX'            TO WS-SAVE-AR-STATE              
01390             GO TO 6000-OUTPUT-REQUEST.                            
01391                                                                   
01392      GO TO 6000-CHECK-AGENTS-LOOP.                                
01393                                                                   
01394                                                                   
01395  6000-OUTPUT-REQUEST.                                             
01396                                                                   
01397      MOVE SPACES                 TO AR-REQUEST-RECORD.            
01398      MOVE 'RQ'                   TO RQ-RECORD-ID.                 
01399      MOVE PB-INPUT-DT            TO RQ-ENTRY-DT.                  
01400      MOVE PB-CREDIT-SELECT-DT    TO RQ-MO-END-DT                  
01401                                     RQ-CREDIT-SELECT-DT.          
01402      MOVE PB-COMPANY-CD          TO RQ-COMPANY-CD                 
01403                                     RQ-COMPANY-CD-A1              
01404                                     RQ-COMPANY-CD-A2              
01405                                     RQ-COMPANY-CD-A3              
01406                                     RQ-COMPANY-CD-A4.             
01407      MOVE PB-ENTRY-BATCH         TO RQ-ENTRY-BATCH                
01408                                     RQ-BATCH-A1                   
01409                                     RQ-BATCH-A2                   
01410                                     RQ-BATCH-A3                   
01411                                     RQ-BATCH-A4.                  
01412      MOVE WS-SAVE-AR-CARRIER     TO RQ-CARRIER-A1                 
01413                                     RQ-CARRIER-A2                 
01414                                     RQ-CARRIER-A3.                
01415      MOVE WS-SAVE-AR-GROUP       TO RQ-GROUPING-A1                
01416                                     RQ-GROUPING-A2                
01417                                     RQ-GROUPING-A3.               
01418                                                                   
01419      IF CF-ZERO-CARRIER OR CF-ZERO-CAR-GROUP                      
01420          MOVE ZEROS              TO RQ-CARRIER-A2.                
01421      IF CF-ZERO-GROUPING OR CF-ZERO-CAR-GROUP                     
01422          MOVE ZEROS              TO RQ-GROUPING-A2.               
01423                                                                   
01424      MOVE WS-SAVE-AR-STATE       TO RQ-STATE-A1.                  
01425      MOVE PB-ACCOUNT             TO RQ-ACCOUNT-A1                 
01426                                     RQ-ACCOUNT-A4.                
01427      MOVE WS-SAVE-ACCT-AGT       TO RQ-ACCT-AGENT-A2              
01428                                     RQ-ACCT-AGENT-A3.             
01429      MOVE WS-SAVE-FIN-RESP       TO RQ-FIN-RESP-A2.               
01430      MOVE WS-SAVE-SUMMARY        TO RQ-SUMMARY-CODE.              
01431      MOVE WS-SAVE-REFERENCE      TO RQ-REFERENCE-A1               
01432                                     RQ-REFERENCE-A2               
01433                                     RQ-REFERENCE-A4.              
01434      MOVE LOW-VALUES             TO RQ-REVERSAL-DT                
01435                                     RQ-CREDIT-ACCEPT-DT           
01436                                     RQ-STMT-DT                    
01437                                     RQ-REQUEST-DT.                
01438      IF REQUEST-ERROR                                             
01439          MOVE 'E'                TO RQ-STATUS.                    
01440                                                                   
01441      WRITE AR-REQUEST-RECORD.                                     
01442                                                                   
01443      IF ERRQST-FILE-STATUS = ZEROS  OR                            
01444        (ERRQST-FILE-STATUS = '22'  AND  DTE-CLIENT = 'NCL')       
01445          NEXT SENTENCE                                            
01446      ELSE                                                         
01447          MOVE 'ERROR OCCURED WRITE - ERRQST' TO  WS-ABEND-MESSAGE 
01448          MOVE ERRQST-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01449          GO TO ABEND-PGM.                                         
01450                                                                   
01451      IF DTE-CLIENT = 'NCL'                                        
01452        IF ERRQST-FILE-STATUS = '22'                               
01453          MOVE PB-CARRIER         TO P-ERR-CARRIER                 
01454          MOVE PB-GROUPING        TO P-ERR-GROUP                   
01455          MOVE PB-STATE           TO P-ERR-STATE                   
01456          MOVE PB-ACCOUNT         TO P-ERR-ACCOUNT                 
01457          MOVE SPACES             TO P-ERR-CERT-NO                 
01458          MOVE ER-1005            TO P-ERR-DESC                    
01459          MOVE WS-ERR-DETAIL1     TO ERR-PRT-DATA                  
01460          PERFORM WRITE-ERROR-LINE.                                
01461                                                                   
01462      MOVE ' '                    TO WS-AR-AM-ERROR.               
01463      MOVE SPACES                 TO WS-SAVE-SUMMARY.              
01464      MOVE SPACES                 TO WS-SAVE-REFERENCE.            
01465      MOVE SPACES                 TO WS-SAVE-AR-KEY.               
01466      PERFORM 6000-CLEAR-INDICATOR VARYING SUB1 FROM 1 BY 1        
01467          UNTIL SUB1 GREATER 32.                                   
01468                                                                   
01469      GO TO 6000-EXIT.                                             
01470                                                                   
01471  6000-CLEAR-INDICATOR.                                            
01472      MOVE ' '                    TO AR-TABLE-USED (SUB1).         
01473                                                                   
01474  6000-EXIT.                                                       
01475      EXIT.                                                        
01476      EJECT                                                        
01477                                                                   
01478  7000-SEARCH-ERPNDB SECTION.                                      
01479                                                                   
01480      MOVE 9999                   TO  PB-BATCH-SEQ-NO.             
01481      MOVE ZEROS                  TO  PB-BATCH-CHG-SEQ-NO.         
01482                                                                   
01483      READ ERPNDB.                                                 
01484                                                                   
01485      IF (ERPNDB-FILE-STATUS = ZEROS) AND                          
01486         (DTE-CLIENT NOT = 'NCL')                                  
01487            DISPLAY 'BYPASS-SW SET IN 7000-'                       
01488            MOVE HIGH-VALUES        TO  WS-BYPASS-BATCH-SW         
01489         ELSE                                                      
01490            MOVE LOW-VALUES         TO  WS-BYPASS-BATCH-SW.        
01491                                                                   
01492      IF  BYPASSING-BATCH                                          
01493          GO TO 7000-EXIT.                                         
01494                                                                   
01495      IF PB-CARRIER  NOT = WS-SAVE-CARRIER  OR                     
01496         PB-GROUPING NOT = WS-SAVE-GROUPING OR                     
01497         PB-STATE    NOT = WS-SAVE-STATE    OR                     
01498         PB-ACCOUNT  NOT = WS-SAVE-ACCOUNT                         
01499            PERFORM 4000-ACCOUNT-TOTALS THRU 4000-EXIT.            
01500                                                                   
01501      IF NOT CF-AR-SYSTEM-USED                                     
01502         GO TO 7000-EXIT.                                          
01503                                                                   
01504  7000-PROCESS-COMP-MASTER.                                        
01505                                                                   
01506      IF REQUEST-ERROR                                             
01507         MOVE SPACES              TO WS-SAVE-REFERENCE             
01508         GO TO 7000-EXIT.                                          
01509                                                                   
01510      MOVE PB-COMPANY-CD          TO CO-COMPANY-CD.                
01511                                                                   
01512      IF CF-ZERO-CARRIER OR CF-ZERO-CAR-GROUP                      
01513          MOVE ZEROS              TO CO-CARRIER                    
01514      ELSE                                                         
01515          MOVE WS-COMP-CARRIER    TO CO-CARRIER.                   
01516                                                                   
01517      IF CF-ZERO-GROUPING OR CF-ZERO-CAR-GROUP                     
01518          MOVE ZEROS              TO CO-GROUPING                   
01519      ELSE                                                         
01520          MOVE WS-COMP-GROUPING   TO CO-GROUPING.                  
01521                                                                   
01522      MOVE WS-COMP-RESP-NO        TO CO-RESP-NO.                   
01523      MOVE WS-COMP-ACCT-NO        TO CO-ACCOUNT.                   
01524      MOVE 'A'                    TO CO-TYPE.                      
01525                                                                   
01526      READ ERCOMP.                                                 
01527                                                                   
01528      IF ERCOMP-FILE-STATUS = '23'                                 
01529          MOVE 'Y'                TO WS-AR-AM-ERROR                
01530          MOVE 'UNKNOWN   '       TO WS-SAVE-FIN-RESP              
01531          GO TO 7000-EXIT.                                         
01532                                                                   
01533      IF ERCOMP-FILE-STATUS NOT = ZERO                             
01534          MOVE 'ERROR OCCURED READ - ERCOMP'                       
01535                                  TO  WS-ABEND-MESSAGE             
01536          MOVE ERCOMP-FILE-STATUS TO WS-ABEND-FILE-STATUS          
01537          GO TO ABEND-PGM.                                         
01538                                                                   
01539      MOVE CO-AR-SUMMARY-CODE     TO WS-SAVE-SUMMARY.              
01540                                                                   
01541                                                                   
01542  7000-SEARCH-3.                                                   
01543                                                                   
01544      IF CO-AR-GROSS-REPORT                                        
01545          MOVE SPACES             TO WS-SAVE-REFERENCE             
01546      ELSE                                                         
01547          MOVE WS-NET-REFERENCE   TO WS-SAVE-REFERENCE.            
01548                                                                   
01549      IF DTE-CLIENT = 'CRI'                                        
01550          MOVE PB-REFERENCE       TO WS-SAVE-REFERENCE.            
01551                                                                   
01552  7000-EXIT.                                                       
01553      EXIT.                                                        
01554      EJECT                                                        
01555                                                                   
01556  8000-READ-CONTROL-FILE SECTION.                                  
01557      MOVE LOW-VALUES             TO  CF-CONTROL-PRIMARY.          
01558      MOVE DTE-CLIENT             TO  CF-COMPANY-ID                
01559                                      WS-PROCESSOR-ID.             
01560      MOVE '1'                    TO  CF-RECORD-TYPE.              
01561      MOVE SPACES                 TO  CF-ACCESS-CD-GENL.           
01562      MOVE ZEROS                  TO  CF-SEQUENCE-NO.              
01563                                                                   
01564      READ ELCNTL.                                                 
01565                                                                   
01566      IF ELCNTL-FILE-STATUS NOT = ZEROS                            
01567          MOVE 'ERROR OCCURED READ - ELCNTL' TO WS-ABEND-MESSAGE   
01568          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
01569          GO TO ABEND-PGM.                                         
01570                                                                   
01571  8000-EXIT.                                                       
01572      EXIT.                                                        
01573      EJECT                                                        
01574                                                                   
01575  8500-DATE-CONVERSION SECTION.                                    
01576  8510-DATE-CONVERSION.                                            
01577      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   
01578                                                                   
01579  8590-EXIT.                                                       
01580      EXIT.                                                        
01581                                                                   
01582      EJECT                                                        
01583  WRITE-A-LINE SECTION.                                            
01584      IF WS-LINE-COUNT GREATER THAN WS-LINE-COUNT-MAX              
01585          PERFORM WRITE-HEADINGS.                                  
01586                                                                   
01587      IF P-CTL = '1'                                               
01588          MOVE +1  TO  WS-LINE-COUNT                               
01589        ELSE                                                       
01590          IF P-CTL = SPACES                                        
01591              ADD +1  TO  WS-LINE-COUNT                            
01592            ELSE                                                   
01593              IF P-CTL = ZERO                                      
01594                  ADD +2  TO  WS-LINE-COUNT                        
01595                ELSE                                               
01596                  ADD +3  TO  WS-LINE-COUNT.                       
01597                                                                   
01598      PERFORM WRITE-PRINTER.                                       
01599                                                                   
01600  WAL-EXIT.                                                        
01601      EXIT.                                                        
01602                                                                   
01603  WRITE-HEADINGS SECTION.                                          
01604      IF LCP-ONCTR-01 =  0                                         
01605          ADD 1 TO LCP-ONCTR-01                                    
01606          MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   
01607          MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME            
01608          MOVE ALPH-DATE          TO  WS-H3-DATE.                  
01609                                                                   
01610      ADD +1  TO  WS-PAGE.                                         
01611      MOVE WS-PAGE                TO  WS-H3-PAGE.                  
01612                                                                   
01613      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        
01614                                                                   
01615      MOVE ZERO                   TO  WS-LINE-COUNT.               
01616                                                                   
01617      MOVE WS-HEADING1            TO  PRT.                         
01618      PERFORM WRITE-PRINTER.                                       
01619      MOVE WS-HEADING2            TO  PRT.                         
01620      PERFORM WRITE-PRINTER.                                       
01621      MOVE WS-HEADING3            TO  PRT.                         
01622      PERFORM WRITE-PRINTER.                                       
01623      MOVE WS-HEADING4            TO  PRT.                         
01624      PERFORM WRITE-PRINTER.                                       
01625      MOVE WS-HEADING5            TO  PRT.                         
01626      PERFORM WRITE-PRINTER.                                       
01627                                                                   
01628      MOVE +9                     TO WS-LINE-COUNT.                
01629      MOVE WS-SAVE-PRINT-RECORD   TO  PRT                          
01630      MOVE '-'                    TO  P-CTL.                       
01631                                                                   
01632  WHS-EXIT.                                                        
01633      EXIT.                                                        
01634                                                                   
01635      EJECT                                                        
01636  WRITE-PRINTER  SECTION.                                          
01637      MOVE P-CTL                  TO  X.                           
01638                                                                   
01639  WPS-020. COPY ELCPRT2X.                                          
01640                                                                   
01641      EJECT                                                        
01642  WRITE-ERROR-LINE SECTION.                                        
01643      IF WS-ERR-LINE-COUNT GREATER THAN WS-ERR-LINE-COUNT-MAX      
01644          PERFORM WRITE-ERR-HEADINGS.                              
01645                                                                   
01646 **   IF ERR-PRT-CTL =  1                                          
CIDMOD     IF ERR-PRT-CTL = '1'                                         
01647          MOVE +1                     TO  WS-ERR-LINE-COUNT        
01648      ELSE                                                         
01649          IF ERR-PRT-CTL = SPACES                                  
01650              ADD +1                  TO  WS-ERR-LINE-COUNT        
01651          ELSE                                                     
01652 **           IF ERR-PRT-CTL = ZERO                                
CIDMOD             IF ERR-PRT-CTL = '0'                                 
01653                  ADD +2              TO  WS-ERR-LINE-COUNT        
01654              ELSE                                                 
01655                  ADD +3              TO  WS-ERR-LINE-COUNT.       
01656                                                                   
01657      PERFORM WRITE-ERR-PRINTER.                                   
01658                                                                   
01659  WAL-ERR-EXIT.                                                    
01660      EXIT.                                                        
01661                                                                   
01662  WRITE-ERR-HEADINGS SECTION.                                      
01663      IF LCP-ONCTR-02 =  0                                         
01664          ADD 1 TO LCP-ONCTR-02                                    
01665          MOVE WS-CURRENT-DATE        TO  WS-ERR-H2-DATE           
01666          MOVE COMPANY-NAME           TO  WS-ERR-H2-CLIENT-NAME    
01667          MOVE ALPH-DATE              TO  WS-ERR-H3-DATE.          
01668                                                                   
01669      ADD +1                          TO  WS-ERR-PAGE.             
01670      MOVE WS-ERR-PAGE                TO  WS-ERR-H3-PAGE.          
01671                                                                   
01672      MOVE ERROR-RPT                  TO  WS-SAVE-PRINT-ERR-RECORD.
01673                                                                   
01674      MOVE ZERO                       TO  WS-ERR-LINE-COUNT.       
01675                                                                   
01676      MOVE WS-ERR-HEADING1            TO  ERROR-RPT.               
01677      PERFORM WRITE-ERR-PRINTER.                                   
01678      MOVE WS-ERR-HEADING2            TO  ERROR-RPT.               
01679      PERFORM WRITE-ERR-PRINTER.                                   
01680      MOVE WS-ERR-HEADING3            TO  ERROR-RPT.               
01681      PERFORM WRITE-ERR-PRINTER.                                   
01682      MOVE WS-ERR-HEADING4            TO  ERROR-RPT.               
01683      PERFORM WRITE-ERR-PRINTER.                                   
01684      MOVE WS-ERR-HEADING5            TO  ERROR-RPT.               
01685      PERFORM WRITE-ERR-PRINTER.                                   
01686                                                                   
01687      MOVE +9                         TO  WS-ERR-LINE-COUNT.       
01688      MOVE WS-SAVE-PRINT-ERR-RECORD   TO  ERROR-RPT.               
01689 **   MOVE 1                          TO  ERR-PRT-CTL.             
CIDMOD     MOVE '-'                        TO  ERR-PRT-CTL.             
01690                                                                   
01691  WHS-ERR-EXIT.                                                    
01692      EXIT.                                                        
01693                                                                   
01694  WRITE-ERR-PRINTER   SECTION.                                     
01695                                                                   
01696      MOVE ERR-PRT-CTL                TO  X.                       
01697                                                                   
01698      MOVE X TO LCP-ASA                                            
01699      PERFORM LCP-WRITE-POS-ERROR-RPT                              
01700          THRU LCP-WRITE-END-ERROR-RPT.                            
01701                                                                   
01702  WEP-EXIT.                                                        
01703      EXIT.                                                        
01704                                                                   
01705      EJECT                                                        
01706  PRINT-ERROR-REPORT SECTION.                                      
01707                                                                   
01708      READ ERROR-REPORT                                            
01709          AT END GO TO PEP-EXIT.                                   
01710                                                                   
01711      MOVE ERROR-RPT                  TO  PRT.                     
01712      PERFORM WRITE-PRINTER.                                       
01713                                                                   
01714      GO TO PRINT-ERROR-REPORT.                                    
01715                                                                   
01716  PEP-EXIT.                                                        
01717      EXIT.                                                        
01718                                                                   
01719      EJECT                                                        
01720  OPEN-FILES SECTION.                                              
01721                                                                   
01722  OFS-010.                                                         
01723      OPEN INPUT PENDING-IN                                        
01724                 ERACCT                                            
01725                 ELCNTL                                            
01726           I-O   ERPNDB                                            
01727           OUTPUT PRNTR                                            
01728                  ERROR-REPORT                                     
01729                  JOURNAL-LOG-FILE.                                
01730                                                                   
01731      IF SYS010-FILE-STATUS NOT EQUAL '00'                         
01732         DISPLAY ' ****** SYS010 FILE EMPTY ****** '               
01733         MOVE '- *** SYS010 FILE EMPTY ***' TO PRT                 
01734         PERFORM WRITE-A-LINE                                      
01735         STOP RUN.                                                 
01736                                                                   
01737      IF ERPNDB-FILE-STATUS NOT = ('00' AND '97')                  
01738          MOVE 'ERROR OCCURED OPEN - ERPNDB'  TO  WS-ABEND-MESSAGE 
01739          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          
01740          GO TO ABEND-PGM.                                         
01741                                                                   
01742      IF ELCNTL-FILE-STATUS NOT = ('00' AND '97')                  
01743          MOVE 'ERROR OCCURED OPEN - ELCNTL'  TO  WS-ABEND-MESSAGE 
01744          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          
01745          GO TO ABEND-PGM.                                         
01746                                                                   
01747      IF ERACCT-FILE-STATUS NOT = ('00' AND '97')                  
01748          MOVE 'ERROR OCCURED OPEN - ERACCT'  TO  WS-ABEND-MESSAGE 
01749          MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS          
01750          GO TO ABEND-PGM.                                         
01751                                                                   
01752      PERFORM 8000-READ-CONTROL-FILE THRU 8000-EXIT.               
01753                                                                   
01754      IF NOT CF-COMPANY-MASTER                                     
01755          MOVE 'COMPANY RECORD MISSING'  TO  WS-ABEND-MESSAGE      
01756          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          
01757          GO TO ABEND-PGM.                                         
01758                                                                   
01759      IF MAIL-PROCESSING                                           
01760        OPEN I-O ERPNDM                                            
01761        IF ERPNDM-FILE-STATUS NOT = ('00' AND '97')                
01762            MOVE 'ERROR OCCURED OPEN - ERPNDM'  TO WS-ABEND-MESSAGE
01763            MOVE ERPNDM-FILE-STATUS TO WS-ABEND-FILE-STATUS        
01764            GO TO ABEND-PGM.                                       
01765                                                                   
01766      IF CF-AR-SYSTEM-USED                                         
01767        OPEN INPUT ERCOMP                                          
01768        IF ERCOMP-FILE-STATUS NOT = ('00' AND '97')                
01769            MOVE 'ERROR OCCURED OPEN - ERCOMP'  TO WS-ABEND-MESSAGE
01770            MOVE ERCOMP-FILE-STATUS TO WS-ABEND-FILE-STATUS        
01771            GO TO ABEND-PGM.                                       
01772                                                                   
01773      IF CF-AR-SYSTEM-USED                                         
01774        OPEN I-O ERRQST                                            
01775        IF ERRQST-FILE-STATUS NOT = ('00' AND '97')                
01776            MOVE 'ERROR OCCURED OPEN - ERRQST'  TO WS-ABEND-MESSAGE
01777            MOVE ERRQST-FILE-STATUS TO WS-ABEND-FILE-STATUS        
01778            GO TO ABEND-PGM.                                       
01779                                                                   
01780  OFS-EXIT.                                                        
01781      EXIT.                                                        
01782                                                                   
01783      EJECT                                                        
01784  CLOSE-FILES SECTION.                                             
01785                                                                   
01786  CFS-010.                                                         
01787      MOVE SPACES                 TO  WS-DETAIL1.                  
01788                                                                   
01789      MOVE +99                    TO  WS-LINE-COUNT.               
01790                                                                   
01791  CFS-050. COPY ELCPRTCX.                                          
01792                                                                   
01793      CLOSE ERPNDB                                                 
01794            ERACCT                                                 
01795            ELCNTL                                                 
01796            JOURNAL-LOG-FILE                                       
01797            PENDING-IN                                             
01798            ERROR-REPORT                                           
01799            PRNTR.                                                 
01800                                                                   
01801      IF ERPNDB-FILE-STATUS NOT = ZEROS                            
01802          MOVE 'ERROR OCCURED CLOSE - ERPNDB' TO WS-ABEND-MESSAGE  
01803          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          
01804          GO TO ABEND-PGM.                                         
01805                                                                   
01806      IF ELCNTL-FILE-STATUS NOT = ZEROS                            
01807          MOVE 'ERROR OCCURED CLOSE - ELCNTL' TO WS-ABEND-MESSAGE  
01808          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          
01809          GO TO ABEND-PGM.                                         
01810                                                                   
01811      IF ERACCT-FILE-STATUS NOT = ZEROS                            
01812          MOVE 'ERROR OCCURED CLOSE - ERACCT' TO WS-ABEND-MESSAGE  
01813          MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS          
01814          GO TO ABEND-PGM.                                         
01815                                                                   
01816      IF MAIL-PROCESSING                                           
01817        CLOSE ERPNDM                                               
01818        IF ERPNDM-FILE-STATUS NOT = ZEROS                          
01819            MOVE 'ERROR OCCURED CLOSE - ERPNDM' TO WS-ABEND-MESSAGE
01820            MOVE ERPNDM-FILE-STATUS TO WS-ABEND-FILE-STATUS        
01821            GO TO ABEND-PGM.                                       
01822                                                                   
01823      IF CF-AR-SYSTEM-USED                                         
01824        CLOSE ERCOMP                                               
01825        IF ERCOMP-FILE-STATUS NOT = ZEROS                          
01826            MOVE 'ERROR OCCURED CLOSE - ERCOMP' TO WS-ABEND-MESSAGE
01827            MOVE ERCOMP-FILE-STATUS TO WS-ABEND-FILE-STATUS        
01828            GO TO ABEND-PGM.                                       
01829                                                                   
01830      IF CF-AR-SYSTEM-USED                                         
01831        CLOSE ERRQST                                               
01832        IF ERRQST-FILE-STATUS NOT = ZEROS                          
01833            MOVE 'ERROR OCCURED CLOSE - ERRQST' TO WS-ABEND-MESSAGE
01834            MOVE ERRQST-FILE-STATUS TO WS-ABEND-FILE-STATUS        
01835            GO TO ABEND-PGM.                                       
01836                                                                   
01837  CFS-EXIT.                                                        
01838      EXIT.                                                        
01839                                                                   
01840  ABEND-PGM SECTION. COPY ELCABEND SUPPRESS.                       
01841 /                                                                 
01842  LCP-WRITE-POS-PRT SECTION.                                       
01843      IF LCP-ASA = '+'                                             
01844          WRITE PRT AFTER 0 LINE                                   
01845      ELSE                                                         
01846      IF LCP-ASA = ' '                                             
01847          WRITE PRT AFTER ADVANCING 1 LINE                         
01848      ELSE                                                         
01849      IF LCP-ASA = '0'                                             
01850          WRITE PRT AFTER ADVANCING 2 LINE                         
01851      ELSE                                                         
01852      IF LCP-ASA = '-'                                             
01853          WRITE PRT AFTER ADVANCING 3 LINE                         
01854      ELSE                                                         
01855      IF LCP-ASA = '1'                                             
01856          WRITE PRT AFTER ADVANCING PAGE                           
01857      ELSE                                                         
01858      IF LCP-ASA = '2'                                             
01859          WRITE PRT AFTER ADVANCING LCP-CH2                        
01860      ELSE                                                         
01861      IF LCP-ASA = '3'                                             
01862          WRITE PRT AFTER ADVANCING LCP-CH3                        
01863      ELSE                                                         
01864      IF LCP-ASA = '4'                                             
01865          WRITE PRT AFTER ADVANCING LCP-CH4                        
01866      ELSE                                                         
01867      IF LCP-ASA = '5'                                             
01868          WRITE PRT AFTER ADVANCING LCP-CH5                        
01869      ELSE                                                         
01870      IF LCP-ASA = '6'                                             
01871          WRITE PRT AFTER ADVANCING LCP-CH6                        
01872      ELSE                                                         
01873      IF LCP-ASA = '7'                                             
01874          WRITE PRT AFTER ADVANCING LCP-CH7                        
01875      ELSE                                                         
01876      IF LCP-ASA = '8'                                             
01877          WRITE PRT AFTER ADVANCING LCP-CH8                        
01878      ELSE                                                         
01879      IF LCP-ASA = '9'                                             
01880          WRITE PRT AFTER ADVANCING LCP-CH9                        
01881      ELSE                                                         
01882      IF LCP-ASA = 'A'                                             
01883          WRITE PRT AFTER ADVANCING LCP-CH10                       
01884      ELSE                                                         
01885      IF LCP-ASA = 'B'                                             
01886          WRITE PRT AFTER ADVANCING LCP-CH11                       
01887      ELSE                                                         
01888      IF LCP-ASA = 'C'                                             
01889          WRITE PRT AFTER ADVANCING LCP-CH12                       
01890      ELSE                                                         
01891      IF LCP-ASA = 'V'                                             
01892          WRITE PRT AFTER ADVANCING LCP-P01                        
01893      ELSE                                                         
01894      IF LCP-ASA = 'W'                                             
01895          WRITE PRT AFTER ADVANCING LCP-P02                        
01896      ELSE                                                         
01897      DISPLAY 'ASA CODE ERROR'.                                    
01898  LCP-WRITE-END-PRT.                                               
01899      EXIT.                                                        
01900 /                                                                 
01901  LCP-WRITE-POS-ERROR-RPT SECTION.                                 
01902      IF LCP-ASA = '+'                                             
01903          WRITE ERROR-RPT AFTER 0 LINE                             
01904      ELSE                                                         
01905      IF LCP-ASA = ' '                                             
01906          WRITE ERROR-RPT AFTER ADVANCING 1 LINE                   
01907      ELSE                                                         
01908      IF LCP-ASA = '0'                                             
01909          WRITE ERROR-RPT AFTER ADVANCING 2 LINE                   
01910      ELSE                                                         
01911      IF LCP-ASA = '-'                                             
01912          WRITE ERROR-RPT AFTER ADVANCING 3 LINE                   
01913      ELSE                                                         
01914      IF LCP-ASA = '1'                                             
01915          WRITE ERROR-RPT AFTER ADVANCING PAGE                     
01916      ELSE                                                         
01917      IF LCP-ASA = '2'                                             
01918          WRITE ERROR-RPT AFTER ADVANCING LCP-CH2                  
01919      ELSE                                                         
01920      IF LCP-ASA = '3'                                             
01921          WRITE ERROR-RPT AFTER ADVANCING LCP-CH3                  
01922      ELSE                                                         
01923      IF LCP-ASA = '4'                                             
01924          WRITE ERROR-RPT AFTER ADVANCING LCP-CH4                  
01925      ELSE                                                         
01926      IF LCP-ASA = '5'                                             
01927          WRITE ERROR-RPT AFTER ADVANCING LCP-CH5                  
01928      ELSE                                                         
01929      IF LCP-ASA = '6'                                             
01930          WRITE ERROR-RPT AFTER ADVANCING LCP-CH6                  
01931      ELSE                                                         
01932      IF LCP-ASA = '7'                                             
01933          WRITE ERROR-RPT AFTER ADVANCING LCP-CH7                  
01934      ELSE                                                         
01935      IF LCP-ASA = '8'                                             
01936          WRITE ERROR-RPT AFTER ADVANCING LCP-CH8                  
01937      ELSE                                                         
01938      IF LCP-ASA = '9'                                             
01939          WRITE ERROR-RPT AFTER ADVANCING LCP-CH9                  
01940      ELSE                                                         
01941      IF LCP-ASA = 'A'                                             
01942          WRITE ERROR-RPT AFTER ADVANCING LCP-CH10                 
01943      ELSE                                                         
01944      IF LCP-ASA = 'B'                                             
01945          WRITE ERROR-RPT AFTER ADVANCING LCP-CH11                 
01946      ELSE                                                         
01947      IF LCP-ASA = 'C'                                             
01948          WRITE ERROR-RPT AFTER ADVANCING LCP-CH12                 
01949      ELSE                                                         
01950      IF LCP-ASA = 'V'                                             
01951          WRITE ERROR-RPT AFTER ADVANCING LCP-P01                  
01952      ELSE                                                         
01953      IF LCP-ASA = 'W'                                             
01954          WRITE ERROR-RPT AFTER ADVANCING LCP-P02                  
01955      ELSE                                                         
01956      DISPLAY 'ASA CODE ERROR'.                                    
01957  LCP-WRITE-END-ERROR-RPT.                                         
01958      EXIT.                                                        
