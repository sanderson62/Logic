00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL176 .                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 07/13/94 11:11:10.                 
00007 *                            VMOD=2.060                           
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
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *
00021 *            *                                                   *
00022 *            *****************************************************
00023                                                                   
00024 *REMARKS.                                                         
00025 *        IN ORDER TO START THE PRINTING OF CHECKS, THIS FUNCTION  
00026 *    IS USED TO QUALIFY THE CONTROL BATCHES TO BE PRINTED AND     
00027 *    SPECIFY A PRINT TIME.  THE INDIVIDUAL CHECK WRITER PROGRAM   
00028 *    IS STARTED BY THIS PROGRAM.                                  
00029                                                                   
00030 *    SCREENS     - EL176A - CHECK WRITER                          
00031                                                                   
00032 *    ENTERED BY  - EL171  - REPORT MENU                           
00033                                                                   
00034 *    EXIT TO     - EL171  - RESULT OF CLEAR                       
00035                                                                   
00036 *    INPUT FILES - ELCHKQ - CHECK QUEUE                           
00037 *                  ELTRLR - ACTIVITY TRAILERS                     
00038 *                  ELCNTL - CONTROL FILE                          
00039 *                  ELMSTR - CLAIM MASTER                          
00040 *                  ELCERT - CERTIFICATE MASTER                    
00041 *                  ELBENE - BENEFICIARY MASTER                    
00042 *                  ERACCT - ACCOUNT MASTER                        
00043 *                  ERCOMP - COMPENSATION MASTER                   
00044 *                  MPPLAN - CONVENIENCE PRODUCER PLAN MASTER      
00045 *                  MPPLCY - CONVENIENCE POLICY MASTER             
00046 *                  MPPROD - CONVENIENCE PRODUCER MASTER           
00047                                                                   
00048 *    OUTPUT FILES - ELTRLR - PAYMENT TRAILERS                     
00049 *                   ELCHKQ - CHECK QUEUE                          
00050                                                                   
00051 *    COMMAREA    - PASSED.  PRINT TIME PASSED TO CHECK WRITER AS  
00052 *                  A 2 BYTE COMP NUMBER IN THE FIRST TWO BYTES OF 
00053 *                  THE WORK AREA.  FORM TYPE IS PASSED AS         
00054 *                  ENTRY-CD-1.                                    
00055                                                                   
00042 *                                                                 
092602******************************************************************
092602*                   C H A N G E   L O G
092602*
092602* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
092602*-----------------------------------------------------------------
092602*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
092602* EFFECTIVE    NUMBER
092602*-----------------------------------------------------------------
CIDMOD*                - JWBA 7/94 PER IR94025-8010 TO ADD HANDLE       
CIDMOD*   LGC123         CONDITIONS FOR CLOSED/DISABLED MICRFLAG AN     
CIDMOD*                  MICR.DRAFTS FILE.                              
CIDMOD*                                                                 
CIDMOD*                - JWBA 4/95 PER IR94196-8912 - ADD OF "NOTFN     
CIDMOD*   LGC134         HANDLE CONDITION;                              
CSODJN*                                                                 
CSODJN*                - DJNA 4/01/2000 CR#2000030100009                
CSODJN*   CSODJN         DRAFT NUMBER EXPANSION.                        
102902* 102902    2001061800003  PEMA  ADD DCC TO MICR PROCESSING
121902* 121902    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
121203* 121203                   SMVA  ADD PROCESSING FOR NEW CLM TYP G
042704* 042704    2004042700002  PEMA  REMOVE NOTIFY NAME AND ADDRESS
070104* 070104    IR             PEMA  MODIFY LENGTH OF MICRDRFT
011105* 011105    2004071200002  PEMA  ADD ONE COMMENT LINE
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
092602******************************************************************
00056      EJECT                                                        
00057  ENVIRONMENT DIVISION.                                            
00058                                                                   
00059  DATA DIVISION.                                                   
00060                                                                   
00061  WORKING-STORAGE SECTION.                                         
00062  77  LCP-WS-ADDR-COMP              PIC S9(8) COMP.                
00063  77  LCP-WS-ADDR-PNTR              REDEFINES LCP-WS-ADDR-COMP     
00064                                    USAGE POINTER.                 
CIDMOD                                                                  
CIDMOD 01  WS-MOD-FIELDS.                                               
CIDMOD     05  WS-FLAG-KEY                 PIC X(8) VALUE SPACES.       
CIDMOD     05  MICR-CHK-SW                 PIC X VALUE 'N'.             
CIDMOD                                                                  
CIDMOD     05  CSO-DRAFT-420C.                                          
CSODJN         10  CSO-DRAFT-KEY           PIC X(19).                   
CIDDAN         10  FILLER                  PIC X(1162).                 
CIDMOD                                                                  
CSODJN     05  WS-MICR-KEY                 PIC X(19) VALUE SPACES.      
070104*    05  REC-LGTH                    PIC S9(4) COMP VALUE +1181.  
070104*    05  REC-LGTH                    PIC S9(4) COMP VALUE +1184.  
011105     05  REC-LGTH                    PIC S9(4) COMP VALUE +1254.
CIDMOD                                                                  
00066  77  FILLER  PIC X(32)  VALUE '********************************'. 
00067  77  FILLER  PIC X(32)  VALUE '*    EL176 WORKING STORAGE     *'. 
00068  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.060 **********'. 
00069                                                                   
00070                              COPY ELCSCTM.                        
00071                                                                   
00072                              COPY ELCSCRTY.                       
00073                                                                   
00074      EJECT                                                        
00075  01  WS-DATE-AREA.                                                
00076      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            
00077      05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.            
CIDMOD     05  WS-RESPONSE         PIC S9(8)   COMP.
CIDMOD         88  WS-RESP-NORMAL              VALUE +00.
CIDMOD         88  WS-RESP-NOTFND              VALUE +13.
CIDMOD
00078                                                                   
00079  01  FILLER                          COMP-3.                      
00080      05  WS-NOT-FOUND                PIC S9      VALUE ZERO.      
00081      05  WS-PRINTER-STARTED-SW       PIC S9      VALUE ZERO.      
00082      05  WS-READNEXT-SW              PIC S9      VALUE ZERO.      
00083      05  WS-LAST-ERROR-COUNT         PIC S9(3)   VALUE ZERO.      
00084      05  WS-COMPLETED-SUCCESSFUL     PIC S9      VALUE ZERO.      
00085        88  TRANSACTION-SUCCESSFUL                    VALUE +1 +2. 
00086        88  CHECKS-WITHOUT-ADDRESSES                  VALUE +2.    
00087                                                                   
00088      05  TIME-IN                     PIC S9(7)   VALUE ZERO.      
00089      05  TIME-OUT REDEFINES TIME-IN  PIC S9(3)V9(4).              
00090      05  WS-HHMM  REDEFINES TIME-IN  PIC S9(5)V99.                
00091                                                                   
00092      05  WS-ACTIVITY-TRAILERS-BROWSE-SW PIC S9 VALUE ZERO.        
00093      05  WS-CHECK-QUEUE-BROWSE-SW       PIC S9 VALUE ZERO.        
00094                                                                   
00095      05  WS-NOT-RELEASED-COUNT       PIC S9(5)    VALUE ZERO.     
00096      05  WS-NOT-RELEASED-AMOUNT      PIC S9(9)V99 VALUE ZERO.     
00097      05  WS-RELEASED-COUNT           PIC S9(5)    VALUE ZERO.     
00098      05  WS-RELEASED-AMOUNT          PIC S9(9)V99 VALUE ZERO.     
00099                                                                   
00100      EJECT                                                        
00101  01  FILLER      COMP SYNC.                                       
00102      05  WS-PAYMENT-NOTE-SEQ-NO      PIC S9(4)  COMP VALUE +0.    
00103      05  SC-ITEM                     PIC S9(4)   VALUE +0001.     
00104                                                                   
00105      05  WS-TS-LENGTH                PIC S9(4)   VALUE +1158.     
00106                                                                   
00107      05  WS-KEY-LENGTH               PIC S9(4)   VALUE ZERO.      
00108                                                                   
00109      05  WS-CHECK-QUE-COUNTER        PIC S9(8)   VALUE ZERO.      
00110      05  WS-CHECK-COUNTER            PIC S9(4)   VALUE +10.       
00111                                                                   
00112      05  WS-LAST-CONTROL-GROUP       PIC S9(8)   VALUE ZERO.      
00113      05  WS-TIMES-PRINTED            PIC S9(4)   VALUE ZERO.      
00114                                                                   
00115      05  WS-SEQUENCE-NUMBER          PIC S9(4)   VALUE ZERO.      
00116      05  WS-BEGIN-NUMBER             PIC S9(4)   VALUE ZERO.      
00117                                                                   
00118      05  WS-ELCHKQ-POINTER           PIC S9(8)   VALUE ZERO.      
00119      05  WS-INDEX                    PIC S9(4)   VALUE ZERO.      
00120                                                                   
00121      EJECT                                                        
00122  01  FILLER.                                                      
00123      05  WS-ERCOMP-KEY.                                           
00124          10  WS-ERCOMP-COMPANY-CD    PIC X.                       
00125          10  WS-ERCOMP-CARRIER       PIC X.                       
00126          10  WS-ERCOMP-GROUPING      PIC X(6).                    
00127          10  WS-ERCOMP-RESP-NO       PIC X(10).                   
00128          10  WS-ERCOMP-ACCOUNT       PIC X(10).                   
00129          10  WS-ERCOMP-TYPE          PIC X.                       
00130                                                                   
00131      05  WS-WORK-DATE.                                            
00132          10  WS-WORK-MO              PIC XX.                      
00133          10  FILLER                  PIC X.                       
00134          10  WS-WORK-DA              PIC XX.                      
00135          10  FILLER                  PIC X.                       
00136          10  WS-WORK-YR              PIC XX.                      
00137      05  WS-CONTROL-FILE-KEY.                                     
00138          10  WS-CFK-COMPANY-ID       PIC X(3)    VALUE SPACES.    
00139          10  WS-CFK-RECORD-TYPE      PIC X       VALUE SPACES.    
00140          10  FILLER                  PIC XX      VALUE SPACES.    
00141          10  WS-CFK-BENEFIT-NO                   VALUE SPACES.    
00142              15  FILLER              PIC X.                       
00143              15  WS-CFK-CARRIER      PIC X.                       
00144          10  WS-CFK-SEQUENCE-NO      PIC S9(4)   VALUE ZERO COMP. 
00145                                                                   
00146      05  WS-ACTIVITY-TRAILERS-KEY.                                
00147          10  WS-ATK-COMPANY-CD       PIC X.                       
00148          10  WS-ATK-CARRIER          PIC X.                       
00149          10  WS-ATK-CLAIM-NO         PIC X(7).                    
00150          10  WS-ATK-CERT-NO          PIC X(11).                   
00151          10  WS-ATK-SEQUENCE-NO      PIC S9(4)  COMP.             
00152                                                                   
00153      05  WS-CHECK-QUEUE-KEY.                                      
00154          10  WS-CQK-COMPANY-CD       PIC X.                       
00155          10  WS-CQK-CONTROL-NUMBER   PIC S9(8) COMP.              
00156          10  WS-CQK-SEQUENCE-NUMBER  PIC S9(4) COMP.              
00157                                                                   
00158      05  WS-CHECK-AIX-KEY.                                        
00159          10  WS-CQK-COMPANY-CD-A1        PIC X.                   
00160          10  WS-CQK-CONTROL-NUMBER-A1    PIC S9(08)  COMP.        
00161          10  WS-PAYEE-CARRIER            PIC X.                   
00162          10  WS-PAYEE-GROUPING           PIC X(06).               
00163          10  WS-PAYEE-STATE              PIC XX.                  
00164          10  WS-PAYEE-BENE-ACCT          PIC X(10).               
00165          10  WS-CQK-SEQUENCE-NUMBER-A1   PIC S9(04)   COMP.       
00166                                                                   
00167      05  WS-LAST-CHECK-QUEUE-KEY     PIC X(26) VALUE LOW-VALUE.   
00168      05  WS-BROWSE-LENGTH            PIC S9           COMP.       
00169                                                                   
00170      05  WS-CLAIM-MASTER-KEY.                                     
00171          10  WS-CK-COMPANY-CD        PIC X.                       
00172          10  WS-CK-CARRIER           PIC X.                       
00173          10  WS-CK-CLAIM-NO          PIC X(7).                    
00174          10  WS-CK-CERT-NO           PIC X(11).                   
00175                                                                   
00176      05  WS-CERTIFICATE-MASTER-KEY.                               
00177          10  WS-CM-COMPANY-CD        PIC X.                       
00178          10  WS-CM-CARRIER           PIC X.                       
00179          10  WS-CM-GROUPING          PIC X(6).                    
00180          10  WS-CM-STATE             PIC XX.                      
00181          10  WS-CM-ACCOUNT           PIC X(10).                   
00182          10  WS-CM-CERT-EFF-DT       PIC XX.                      
00183          10  WS-CM-CERT-NO           PIC X(11).                   
00184                                                                   
00185      05  WS-ACCOUNT-MASTER-KEY.                                   
00186          10  WS-AK-COMPANY-CD        PIC X.                       
00187          10  WS-AK-CARRIER           PIC X.                       
00188          10  WS-AK-GROUPING          PIC X(6).                    
00189          10  WS-AK-STATE             PIC XX.                      
00190          10  WS-AK-ACCOUNT           PIC X(10).                   
00191          10  WS-AK-EXPIRATION-DT     PIC XX.                      
00192                                                                   
00193      05  WS-ACCOUNT-HOLD-RECORD      PIC X(2000).                 
00194      05  WS-PRODUCER-HOLD-RECORD     PIC X(2000).                 
00195                                                                   
00196      05  WS-BENEFICIARY-KEY.                                      
00197          10  WS-BK-COMPANY-CD        PIC X.                       
00198          10  WS-BK-RECORD-TYPE       PIC X.                       
00199          10  WS-BK-BENEFICIARY       PIC X(10).                   
00200                                                                   
00201      05  WS-POLICY-MASTER-KEY.                                    
00202          10  WS-PM-COMPANY-CD        PIC X.                       
00203          10  WS-PM-CARRIER           PIC X.                       
00204          10  WS-PM-GROUPING          PIC X(06).                   
00205          10  WS-PM-STATE             PIC XX.                      
00206          10  WS-PM-PRODUCER          PIC X(10).                   
00207          10  WS-PM-EFF-DT            PIC XX.                      
00208          10  WS-PM-REFERENCE-NO      PIC X(20).                   
00209                                                                   
00210      05  WS-PRODUCER-MASTER-KEY.                                  
00211          10  WS-PD-COMPANY-CD        PIC X.                       
00212          10  WS-PD-CARRIER           PIC X.                       
00213          10  WS-PD-GROUPING          PIC X(06).                   
00214          10  WS-PD-STATE             PIC XX.                      
00215          10  WS-PD-PRODUCER          PIC X(10).                   
00216          10  WS-PD-EXP-DT            PIC XX.                      
00217                                                                   
00218      05  WS-PLAN-MASTER-KEY.                                      
00219          10  WS-PP-COMPANY-CD        PIC X.                       
00220          10  WS-PP-CARRIER           PIC X.                       
00221          10  WS-PP-GROUPING          PIC X(06).                   
00222          10  WS-PP-STATE             PIC XX.                      
00223          10  WS-PP-PRODUCER          PIC X(10).                   
00224          10  WS-PP-PLAN-CODE         PIC XX.                      
00225          10  WS-PP-REV-NO            PIC 9(03).                   
00226                                                                   
00227      EJECT                                                        
00228      05  WS-MAPSET-NAME              PIC X(8)  VALUE 'EL176S'.    
00229      05  WS-MAP-NAME                 PIC X(8)  VALUE 'EL176A'.    
00230                                                                   
00231      05  FILLER REDEFINES WS-MAP-NAME.                            
00232          20  FILLER                  PIC XX.                      
00233          20  WS-MAP-NUMBER           PIC X(6).                    
00234                                                                   
00235      05  THIS-PGM                    PIC X(8)  VALUE 'EL176'.     
00236                                                                   
00237      05  WS-LAST-CARRIER             PIC X     VALUE LOW-VALUES.  
00238                                                                   
00239      05  WS-CHECK-QUEUE-DSID         PIC X(8) VALUE 'ELCHKQ'.     
00240      05  WS-CHECK-QUEUE-AIX-DSID     PIC X(8) VALUE 'ELCHKQ2'.    
00241      05  WS-ENQ-COMPANY-ID           PIC X(3) VALUE ZERO.         
00242      05  WS-ACTIVITY-TRAILERS-DSID   PIC X(8) VALUE 'ELTRLR'.     
00243      05  WS-CONTROL-FILE-DSID        PIC X(8) VALUE 'ELCNTL'.     
00244      05  WS-CLAIM-MASTER-DSID        PIC X(8) VALUE 'ELMSTR'.     
00245      05  WS-CERTIFICATE-MASTER-DSID  PIC X(8) VALUE 'ELCERT'.     
00246      05  WS-ACCOUNT-MASTER-DSID      PIC X(8) VALUE 'ERACCT'.     
00247      05  WS-BENEFICIARY-MASTER-DSID  PIC X(8) VALUE 'ELBENE'.     
00248      05  WS-POLICY-MASTER-DSID       PIC X(8) VALUE 'MPPLCY'.     
00249      05  WS-PRODUCER-MASTER-DSID     PIC X(8) VALUE 'MPPROD'.     
00250      05  WS-PLAN-MASTER-DSID         PIC X(8) VALUE 'MPPLAN'.     
00251                                                                   
00252      05  WS-LOW-VALUES               PIC X VALUE LOW-VALUES.      
00253      05  WS-SPACES                   PIC X VALUE SPACES.          
00254                                                                   
00255      05  WS-CHECK-WRITER-DATE        PIC XX VALUE LOW-VALUES.     
00256                                                                   
00257      05  WS-TRANS-ID                 PIC X(4)    VALUE 'EX46'.    
00258      05  WS-CHECK-WRITER-TRANS-ID.                                
00259          10  WS-CHECK-TRANSID-1      PIC X       VALUE 'Q'.       
00260          10  WS-CHECK-TRANSID-2      PIC XXX     VALUE SPACES.    
00261                                                                   
00262      05  WS-BENEFIT-NO               PIC XX      VALUE SPACES.    
00263                                                                   
00264      05  WS-AUTO-PAY-SW              PIC X       VALUE 'N'.       
00265                                                                   
00266      05  WS-EMPROD-GETMAIN-SW        PIC X       VALUE 'Y'.       
00267      05  WS-ERACCT-GETMAIN-SW        PIC X       VALUE 'Y'.       
00268      05  WS-ELCHKQ-GETMAIN-SW        PIC X       VALUE 'Y'.       
00269                                                                   
00270      05  FILE-SWITCH                 PIC X(04)   VALUE SPACES.    
00271                                                                   
00272      05  WS-AIG-CREDITOR-NAME        PIC X(30)   VALUE SPACES.    
00273      05  WS-AIG-INS-CO-NAME          PIC X(30)   VALUE SPACES.    
00274                                                                   
00275      05  WS-DMD-FLAG-KEY             PIC X(8)    VALUE SPACES.    
00276      05  WS-DMD-MICR-FLAG            PIC X(18)   VALUE SPACES.    
00277                                                                   
CSODJN     05  WS-DMD-DRFT-KEY             PIC X(19)   VALUE SPACES.    
CSODJN     05  WS-DMD-MICR-DRFT            PIC X(705)  VALUE SPACES.    
00280                                                                   
00281      05  WS-DMD-CNTL-GRP-COUNT       PIC S9  COMP  VALUE ZEROS.   
00282                                                                   
00283      05  WS-COV-TYPE                 PIC X.                       
00284      05  WS-KIND.                                                 
00285          10  WS-RETRO-DAYS           PIC 99.                      
00286          10  WS-RETRO-ELIM           PIC X.                       
00287                                                                   
00288      05  WS-TEXT-MESSAGE-LENGTH      PIC S9(4)   VALUE +70        
00289                                      COMP SYNC.                   
00290                                                                   
00291      05  WS-TEXT-MESSAGE             PIC X(70)   VALUE SPACES.    
00292                                                                   
00293      05  WS-TEMP-STORAGE-ITEM        PIC S9(4)   VALUE ZERO       
00294                                      COMP SYNC.                   
00295                                                                   
00296      05  WS-TEMP-STORAGE-KEY.                                     
00297          10  WS-TSK-TERM-ID          PIC X(4)    VALUE SPACES.    
00298          10  WS-TSK-TIME             PIC S9(7)   VALUE ZERO       
00299                                      COMP-3.                      
00300                                                                   
00301      05  WS-MINUTES                  PIC S99     VALUE ZERO.      
00302                                                                   
00303      05  WS-GREATEST-CHECK-NUMBER    PIC 9(7)    VALUE ZERO.      
00304                                                                   
00305      05  WS-GREATEST-CHECK-NUMBER-X  REDEFINES                    
00306          WS-GREATEST-CHECK-NUMBER    PIC X(7).                    
00307                                                                   
00308      05  WS-CHECK-NUMBER             PIC 9(7)    VALUE ZERO.      
00309                                                                   
00310      05  WS-CHECK-NUMBER-X REDEFINES                              
00311          WS-CHECK-NUMBER             PIC X(7).                    
00312                                                                   
00313      05  WS-ACKNO                    PIC 9(7)    VALUE ZERO.      
00314                                                                   
00315      05  WS-ACKNO-X REDEFINES                                     
00316          WS-ACKNO                    PIC X(7).                    
00317                                                                   
00318      05  WS-PAYMENT-TYPE             PIC X       VALUE ZERO.      
00319      05  WS-PAYEE-CODE.                                           
00320          10  WS-PAYEE-CD             PIC X.                       
00321          10  WS-PAYEE-SEQ            PIC X.                       
00322          10  WS-PAYEE-SEQ-NUM REDEFINES                           
00323              WS-PAYEE-SEQ            PIC 9.                       
00324                                                                   
00325      05  WS-INIT-CONTROL-GROUP.                                   
00326          10  FILLER                  PIC X    VALUE SPACES.       
00327          10  FILLER                  PIC X(7) VALUE LOW-VALUES.   
00328                                                                   
00329      05  WS-SSN.                                                  
00330          10  WS-SSN-STATE            PIC XX.                      
00331          10  WS-SSN-ACCOUNT          PIC X(6).                    
00332          10  WS-SSN-LN3              PIC X(3).                    
00333                                                                   
00334      05  WS-MEMBER-NUMBER.                                        
00335          10  WS-MEMBER-STATE         PIC XX.                      
00336          10  WS-MEMBER-ACCOUNT       PIC X(6).                    
00337          10  WS-MEMBER-LN4           PIC X(4).                    
00338                                                                   
00339      05  WS-GROUP.                                                
00340          10  WS-GRP-1-3              PIC XXX.                     
00341          10  WS-GRP-4-6              PIC XXX.                     
00342                                                                   
00343      05  WS-ZIP-UNPACKED                 PIC 9(9)  VALUE ZEROS.   
00344                                                                   
00345      05  WS-CARRIER-ADDRESS-DATA.                                 
00346          10  WS-CARRIER-MAIL-TO-NAME     PIC X(30).               
00347          10  WS-CARRIER-IN-CARE-OF       PIC X(30).               
00348          10  WS-CARRIER-ADDRESS-LINE-1   PIC X(30).               
00349          10  WS-CARRIER-ADDRESS-LINE-2   PIC X(30).               
00350          10  WS-CARRIER-CITY-STATE       PIC X(30).               
00351          10  WS-CARRIER-ZIP-CODE         PIC X(9).                
00352          10  WS-CARRIER-PHONE-NO         PIC 9(11)     COMP-3.    
00353                                                                   
00354      05  WS-OLD-CHECK-QUEUE-RECORD   PIC X(100)  VALUE SPACES.    
00355      05  WS-NEW-CHECK-QUEUE-RECORD   PIC X(100)  VALUE SPACES.    
00356                                                                   
00357      05  WS-WORK-PHONE               PIC X(10)   VALUE ZEROS.     
00358      05  WS-NUMERIC-PHONE REDEFINES WS-WORK-PHONE                 
00359                                      PIC 9(10).                   
00360                                                                   
00361      EJECT                                                        
00362                                                                   
00363  01  ERROR-MESSAGES.                                              
00364      12  ER-0002                 PIC X(4)  VALUE '0002'.          
00365      12  ER-0004                 PIC X(4)  VALUE '0004'.          
00366      12  ER-0008                 PIC X(4)  VALUE '0008'.          
00367      12  ER-0029                 PIC X(4)  VALUE '0029'.          
00368      12  ER-0042                 PIC X(4)  VALUE '0042'.          
00369      12  ER-0070                 PIC X(4)  VALUE '0070'.          
00370      12  ER-0154                 PIC X(4)  VALUE '0154'.          
00371      12  ER-0168                 PIC X(4)  VALUE '0168'.          
00372      12  ER-0169                 PIC X(4)  VALUE '0169'.          
00373      12  ER-0172                 PIC X(4)  VALUE '0172'.          
00374      12  ER-0330                 PIC X(4)  VALUE '0330'.          
00375      12  ER-0361                 PIC X(4)  VALUE '0361'.          
00376      12  ER-0362                 PIC X(4)  VALUE '0362'.          
00377      12  ER-0364                 PIC X(4)  VALUE '0364'.          
00378      12  ER-0365                 PIC X(4)  VALUE '0365'.          
00379      12  ER-0366                 PIC X(4)  VALUE '0366'.          
00380      12  ER-0367                 PIC X(4)  VALUE '0367'.          
00381      12  ER-0368                 PIC X(4)  VALUE '0368'.          
00382      12  ER-0369                 PIC X(4)  VALUE '0369'.          
00383      12  ER-0370                 PIC X(4)  VALUE '0370'.          
00384      12  ER-0371                 PIC X(4)  VALUE '0371'.          
00385      12  ER-0379                 PIC X(4)  VALUE '0379'.          
00386      12  ER-0380                 PIC X(4)  VALUE '0380'.          
00387      12  ER-0381                 PIC X(4)  VALUE '0381'.          
00388      12  ER-0382                 PIC X(4)  VALUE '0382'.          
00389      12  ER-0383                 PIC X(4)  VALUE '0383'.          
00390      12  ER-0385                 PIC X(4)  VALUE '0385'.          
00391      12  ER-0387                 PIC X(4)  VALUE '0387'.          
00392      12  ER-0389                 PIC X(4)  VALUE '0389'.          
00393      12  ER-0390                 PIC X(4)  VALUE '0390'.          
00394      12  ER-0391                 PIC X(4)  VALUE '0391'.          
00395      12  ER-0392                 PIC X(4)  VALUE '0392'.          
00396      12  ER-0393                 PIC X(4)  VALUE '0393'.          
00397      12  ER-0394                 PIC X(4)  VALUE '0394'.          
00398      12  ER-0395                 PIC X(4)  VALUE '0395'.          
00399      12  ER-0490                 PIC X(4)  VALUE '0490'.          
00400      12  ER-2055                 PIC X(4)  VALUE '2055'.          
00401      12  ER-2370                 PIC X(4)  VALUE '2370'.          
00402      12  ER-2936                 PIC X(4)  VALUE '2936'.          
00403      12  ER-3027                 PIC X(4)  VALUE '3027'.          
00404      12  ER-3028                 PIC X(4)  VALUE '3028'.          
00405      12  ER-3130                 PIC X(4)  VALUE '3130'.          
00406      12  ER-3776                 PIC X(4)  VALUE '3776'.          
00407      12  ER-7675                 PIC X(4)  VALUE '7675'.          
00408      12  ER-9808                 PIC X(4)  VALUE '9808'.          
00409      12  ER-9883                 PIC X(4)  VALUE '9883'.          
00410      12  ER-9886                 PIC X(4)  VALUE '9886'.          
00411                                                                   
00412      EJECT                                                        
00413      COPY ELCINTF.                                                
00414                                                                   
00415      COPY ELC176PI.                                               
00416             16  FILLER PIC XX.                                    
00417                                                                   
00418      EJECT                                                        
00419      COPY ELCCPA.                                                 
00420                                                                   
00421      EJECT                                                        
00422      COPY ELCNWA.                                                 
00423                                                                   
00424      COPY EL176S.                                                 
00425                                                                   
00426  01  FILLER REDEFINES EL176AI.                                    
00427      05  FILLER                      PIC X(35).                   
00428                                                                   
00429      05  FILLER                      OCCURS 4 TIMES               
00430                                      INDEXED BY EL176A-INDEX.     
00431                                                                   
00432          10  EL176A-CONTROL-GROUP-LENGTH   PIC S9(4)              
00433                                            COMP.                  
00434          10  EL176A-CONTROL-GROUP-ATTRB    PIC X.                 
00435          10  EL176A-CONTROL-GROUP          PIC 9(7).              
00436          10  EL176A-CONTROL-GROUP-X        REDEFINES              
00437              EL176A-CONTROL-GROUP          PIC X(7).              
00438                                                                   
00439      EJECT                                                        
00440      COPY ELCJPFX.                                                
00441           PIC X(750).                                             
00442                                                                   
00443      EJECT                                                        
00444      COPY ELCEMIB.                                                
00445                                                                   
00446      EJECT                                                        
00447      COPY ELCDATE.                                                
00448      EJECT                                                        
00449      COPY ELCLOGOF.                                               
00450                                                                   
00451      EJECT                                                        
00452      COPY ELCATTR.                                                
00453                                                                   
00454      EJECT                                                        
00455      COPY ELCAID.                                                 
00456                                                                   
00457  01  FILLER REDEFINES DFHAID.                                     
00458      05  FILLER                      PIC X(8).                    
00459      05  PF-VALUES                   PIC X                        
00460          OCCURS 24 TIMES.                                         
00461                                                                   
00462  LINKAGE SECTION.                                                 
00463                                                                   
00464  01  DFHCOMMAREA                     PIC X(1024).                 
00465                                                                   
00466      EJECT                                                        
00467      COPY ELCCHKQ.                                                
00468                                                                   
00469      EJECT                                                        
00470      COPY ELCCNTL.                                                
00471                                                                   
00472      EJECT                                                        
00473      COPY ELCTRLR.                                                
00474                                                                   
00475      EJECT                                                        
00476      COPY ELCMSTR.                                                
00477                                                                   
00478      EJECT                                                        
00479      COPY ELCCERT.                                                
00480                                                                   
00481      EJECT                                                        
00482      COPY ERCACCT.                                                
00483                                                                   
00484      EJECT                                                        
00485      COPY ELCBENE.                                                
00486                                                                   
00487      EJECT                                                        
00488      COPY ERCCOMP.                                                
00489                                                                   
00490      EJECT                                                        
00491      COPY MPCPLCY.                                                
00492                                                                   
00493      EJECT                                                        
00494      COPY MPCPROD.                                                
00495                                                                   
00496      EJECT                                                        
00497      COPY MPCPLAN.                                                
00498                                                                   
00499      EJECT                                                        
00500  PROCEDURE DIVISION.                                              
00501                                                                   
00502      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               
00503      MOVE '5'                   TO DC-OPTION-CODE.                
00504      PERFORM 8500-DATE-CONVERSION.                                
00505      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    
00506      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE                 
00507                                     WS-CHECK-WRITER-DATE.         
00508                                                                   
00509      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     
00510                                                                   
00511 *    NOTE ******************************************************* 
00512 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * 
00513 *         *  FROM ANOTHER MODULE.                               * 
00514 *         *******************************************************.
00515                                                                   
00516      IF EIBCALEN NOT GREATER THAN ZERO                            
00517          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   
00518          GO TO 8300-SEND-TEXT.                                    
00519                                                                   
00520      EXEC CICS HANDLE CONDITION                                   
00521          PGMIDERR   (9600-PGMIDERR)                               
00522          NOTOPEN    (8800-NOT-OPEN)                               
00523          NOTFND     (0180-MAIN-LOGIC)                             
00524          ENDFILE    (0190-MAIN-LOGIC)                             
00525          TERMIDERR  (0900-TERMIDERR)                              
00526          ENQBUSY    (0910-ENQ-BUSY)                               
00527          ERROR      (9990-ERROR)                                  
00528      END-EXEC.                                                    
00529                                                                   
00530      MOVE +3                     TO  EMI-NUMBER-OF-LINES.         
00531      MOVE +2                     TO  EMI-SWITCH2.                 
00532                                                                   
00533      EJECT                                                        
00534  0010-MAIN-LOGIC.                                                 
00535      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         
00536          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   
00537              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6     
00538              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5     
00539              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4     
00540              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3     
00541              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2     
00542              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1     
00543              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM   
00544              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM     
00545            ELSE                                                   
00546              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM     
00547              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM   
00548              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1     
00549              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2     
00550              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3     
00551              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4     
00552              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5     
00553              MOVE SPACES               TO  PI-SAVED-PROGRAM-6     
00554        ELSE                                                       
00555          GO TO 0040-MAIN-LOGIC.                                   
00556                                                                   
00557  0015-MAIN-LOGIC.                                                 
00558 *    NOTE ******************************************************* 
00559 *         *     INITIALIZE THE WORK FIELDS FOR THE PROGRAM      * 
00560 *         *  INTERFACE BLOCK FOR THIS MODULE.                   * 
00561 *         *******************************************************.
00562                                                                   
00563      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.        
00564                                                                   
00565      MOVE ZEROS                  TO  PI-NUMBER-OF-CONTROL-GROUPS  
00566                                      PI-NUMBER-OF-ALIGNMENT-CHECKS
00567                                      PI-ALIGNMENT-CONTROL-GROUP   
00568                                      PI-ALIGNMENT-SEQUENCE-NO     
00569                                      PI-PROCESSING-SW             
00570                                      PI-CONTROL-GROUP (1)         
00571                                      PI-CONTROL-GROUP (2)         
00572                                      PI-CONTROL-GROUP (3)         
00573                                      PI-CONTROL-GROUP (4)         
00574                                      PI-HIGH-SEQUENCE (1)         
00575                                      PI-HIGH-SEQUENCE (2)         
00576                                      PI-HIGH-SEQUENCE (3)         
00577                                      PI-HIGH-SEQUENCE (4)         
00578                                      PI-COMPANY-ZIP-CODE          
00579                                      PI-COMPANY-PHONE-NUMBER.     
00580                                                                   
00581      MOVE WS-PRINTER-STARTED-SW  TO  PI-PRINTER-STARTED-SW.       
00582      MOVE WS-TEMP-STORAGE-KEY    TO  PI-TEMP-STORAGE-KEY.         
00583                                                                   
00584      MOVE LOW-VALUES             TO  EL176AI.                     
00585      MOVE -1                     TO  AOPTIONL.                    
00586                                                                   
00587      PERFORM 8100-SEND-INITIAL-MAP.                               
00588                                                                   
CIDMOD     MOVE 'N' TO MICR-CHK-SW.                                     
CIDMOD                                                                  
CIDMOD     EJECT                                                        
CIDMOD                                                                  
CIDMOD                                                                  
CIDMOD 0030-CHECK-MICR.                                                 
CIDMOD**   ----------------------------------------                     
CIDMOD**   --> IS.MICR.DRAFTS HANDLE CONDITIONS                         
CIDMOD**   ----------------------------------------                     
CIDMOD                                                                  
CIDMOD     EXEC CICS HANDLE CONDITION                                   
CIDMOD         NOTFND   (0030-MICR-CHK-SW)                              
CIDMOD         NOTOPEN  (5000-MICR-CLOSED)                              
CIDMOD         DISABLED (5000-MICR-CLOSED) END-EXEC.                    
CIDMOD                                                                  
CSODJN*    MOVE '420C00001CCCCCCCCCC' TO WS-MICR-KEY.                   
070104     MOVE 'DCC2000000000000000' TO WS-MICR-KEY.                   
CIDMOD                                                                  
CIDMOD     EXEC CICS READ                                               
CIDMOD         DATASET ('MICRDRFT')                                     
CIDMOD         LENGTH  (REC-LGTH)                                       
CIDMOD         RIDFLD  (WS-MICR-KEY)                                    
CIDMOD         INTO (CSO-DRAFT-420C)                                    
CIDMOD         GTEQ                                                     
CIDMOD     END-EXEC.                                                    
CIDMOD                                                                  
CIDMOD 0030-MICR-CHK-SW.                                                
CIDMOD                                                                  
CIDMOD     MOVE 'Y' TO  MICR-CHK-SW.                                    
CIDMOD                                                                  
CIDMOD 0030-MICR-EXIT.                                                  
CIDMOD     EXIT.                                                        
CIDMOD**   ----------------------------------------                     
CIDMOD                                                                  
CIDMOD                                                                  
00590  0040-MAIN-LOGIC.                                                 
00591 *    NOTE ******************************************************* 
00592 *         *  AFTER THE FIRST TIME THROUGH THE PROPER ATTENTION  * 
00593 *         *  KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY         * 
00594 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         * 
00595 *         *******************************************************.
00596                                                                   
00597      IF EIBAID = DFHCLEAR                                         
00598          GO TO 9400-CLEAR.                                        
00599                                                                   
00600      IF PI-PROCESSOR-ID = 'LGXX'                                  
00601          NEXT SENTENCE                                            
00602      ELSE                                                         
00603          EXEC CICS READQ TS                                       
00604              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  
00605              INTO    (SECURITY-CONTROL)                           
00606              LENGTH  (SC-COMM-LENGTH)                             
00607              ITEM    (SC-ITEM)                                    
00608          END-EXEC                                                 
00609          MOVE SC-CLAIMS-DISPLAY (13)   TO  PI-DISPLAY-CAP         
00610          MOVE SC-CLAIMS-UPDATE  (13)   TO  PI-MODIFY-CAP          
00611          IF NOT MODIFY-CAP                                        
00612              MOVE 'UPDATE'             TO  SM-READ                
00613              PERFORM 9995-SECURITY-VIOLATION                      
00614              MOVE ER-0070              TO  EMI-ERROR              
00615              GO TO 8100-SEND-INITIAL-MAP.                         
00616                                                                   
00617      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       
00618          MOVE LOW-VALUES         TO  EL176AI                      
00619          MOVE ER-0008               TO  EMI-ERROR                 
00620          MOVE -1                 TO  APFKL                        
00621          PERFORM 8200-SEND-DATAONLY.                              
00622                                                                   
00623      EXEC CICS RECEIVE                                            
00624          INTO   (EL176AI)                                         
00625          MAPSET (WS-MAPSET-NAME)                                  
00626          MAP    (WS-MAP-NAME)                                     
00627      END-EXEC.                                                    
00628                                                                   
00629      IF APFKL IS GREATER THAN ZERO                                
00630          IF EIBAID NOT = DFHENTER                                 
00631              MOVE ER-0004        TO  EMI-ERROR                    
00632              MOVE AL-UNBOF       TO  APFKA                        
00633              MOVE -1             TO  APFKL                        
00634              PERFORM 8200-SEND-DATAONLY                           
00635            ELSE                                                   
00636              IF APFKO IS NUMERIC                                  
00637                AND APFKO IS GREATER THAN ZERO                     
00638                AND APFKO IS LESS THAN '25'                        
00639                  MOVE PF-VALUES (APFKI)  TO  EIBAID               
00640                ELSE                                               
00641                  MOVE ER-0029        TO  EMI-ERROR                
00642                  MOVE AL-UNBOF       TO  APFKA                    
00643                  MOVE -1             TO  APFKL                    
00644                  PERFORM 8200-SEND-DATAONLY.                      
00645                                                                   
00646      IF EIBAID = DFHPF12                                          
00647          MOVE 'EL010'            TO  THIS-PGM                     
00648          GO TO 9300-XCTL.                                         
00649                                                                   
00650      IF EIBAID = DFHPF23                                          
00651          GO TO 9000-RETURN-CICS.                                  
00652                                                                   
00653      IF EIBAID = DFHPF24                                          
00654          MOVE 'EL126'            TO  THIS-PGM                     
00655          GO TO 9300-XCTL.                                         
00656                                                                   
00657      IF EIBAID NOT = DFHENTER                                     
00658          MOVE ER-0008               TO  EMI-ERROR                 
00659          MOVE -1                 TO  APFKL                        
00660          PERFORM 8200-SEND-DATAONLY.                              
00661                                                                   
00662      IF PI-PROCESSING-SW NOT = ZERO                               
00663          GO TO 0240-MAIN-LOGIC.                                   
00664                                                                   
00665      EJECT                                                        
00666  0100-MAIN-LOGIC.                                                 
00667 *    NOTE ******************************************************* 
00668 *         *          SYNTAX CHECK THE MAP FIELDS                * 
00669 *         *******************************************************.
00670                                                                   
CIDMOD                                                                  
CIDMOD     IF MICR-CHK-SW = 'Y'                                         
CIDMOD         CONTINUE                                                 
CIDMOD     ELSE                                                         
CIDMOD         PERFORM 0030-CHECK-MICR THRU 0030-MICR-EXIT              
CIDMOD     END-IF.                                                      
CIDMOD                                                                  
00671      IF AOPTIONL NOT GREATER THAN ZERO                            
00672          MOVE -1                    TO  AOPTIONL                  
00673          MOVE AL-UNBON              TO  AOPTIONA                  
00674          MOVE ER-0002               TO  EMI-ERROR                 
00675          PERFORM 8200-SEND-DATAONLY.                              
00676                                                                   
00677      IF (AOPTIONI GREATER THAN ZERO AND                           
00678          AOPTIONI LESS THAN '4')                                  
00679        OR                                                         
00680          (PI-COMPANY-ID = ('POS' OR 'WSL' OR 'MLI') AND           
00681           AOPTIONI = ('2' OR '3'))                                
00682              MOVE AL-UNNON           TO  AOPTIONA                 
00683            ELSE                                                   
00684              MOVE -1                 TO  AOPTIONL                 
00685              MOVE AL-UNBON           TO  AOPTIONA                 
00686              MOVE ER-0330            TO  EMI-ERROR                
00687              PERFORM 9900-ERROR-FORMAT.                           
00688                                                                   
00689      IF AALIGNL GREATER THAN ZERO                                 
00690          IF AALIGNI IS NUMERIC                                    
00691              MOVE AALIGNO      TO  PI-NUMBER-OF-ALIGNMENT-CHECKS  
00692              MOVE AL-UNNON     TO  AALIGNA                        
00693            ELSE                                                   
00694              MOVE ER-0365      TO  EMI-ERROR                      
00695              MOVE -1           TO  AALIGNL                        
00696              MOVE AL-UNBON     TO  AALIGNA                        
00697              PERFORM 9900-ERROR-FORMAT.                           
00698                                                                   
00699      IF ACKNOL GREATER THAN ZERO                                  
00700          IF ACKNOI IS NUMERIC                                     
00701              MOVE AL-UNNON       TO  ACKNOA                       
00702              MOVE ACKNOI         TO  WS-CHECK-NUMBER-X            
00703            ELSE                                                   
00704              MOVE ER-0366        TO  EMI-ERROR                    
00705              MOVE -1             TO  ACKNOL                       
00706              MOVE AL-UNBON       TO  ACKNOA                       
00707              PERFORM 9900-ERROR-FORMAT                            
00708      ELSE                                                         
00709          MOVE ZEROS              TO  WS-CHECK-NUMBER.             
00710                                                                   
00711      IF AACNL GREATER THAN ZERO                                   
00712          MOVE AACNI              TO  PI-ASSIGN-CHECK-NUMBERS      
00713          IF AACNI = 'Y' OR 'N'                                    
00714              MOVE AL-UANON       TO  AACNA                        
00715              MOVE AACNI          TO  PI-ENTRY-CD-1                
00716            ELSE                                                   
00717              MOVE AL-UABON       TO  AACNA                        
00718              MOVE -1             TO  AACNL                        
00719              MOVE ER-0367        TO  EMI-ERROR                    
00720              PERFORM 9900-ERROR-FORMAT                            
00721        ELSE                                                       
00722          MOVE AL-UABOF           TO  AACNA                        
00723          MOVE -1                 TO  AACNL                        
00724          MOVE ER-0368            TO  EMI-ERROR                    
00725          PERFORM 9900-ERROR-FORMAT.                               
00726                                                                   
00727      IF AACNI = 'Y'                                               
00728        AND ACKNOL NOT GREATER THAN ZERO                           
00729          MOVE -1                 TO  ACKNOL                       
00730          MOVE AL-UNBOF           TO  ACKNOA                       
00731          MOVE ER-0392            TO  EMI-ERROR                    
00732          PERFORM 9900-ERROR-FORMAT.                               
00733                                                                   
00734      IF AACNI = 'N'                                               
00735        AND ACKNOL GREATER THAN ZERO                               
00736          MOVE -1                 TO  ACKNOL                       
00737          MOVE AL-UNBON           TO  ACKNOA                       
00738          MOVE ER-0393            TO  EMI-ERROR                    
00739          PERFORM 9900-ERROR-FORMAT.                               
00740                                                                   
00741      EJECT                                                        
00742 *    NOTE ******************************************************* 
00743 *         *      CHECK THE VALIDITY OF ANY CONTROL GROUPS       * 
00744 *         *  ENTERED.                                           * 
00745 *         *******************************************************.
00746                                                                   
00747      SET EL176A-INDEX                                             
00748          PI-INDEX TO +1.                                          
00749                                                                   
00750  0120-MAIN-LOGIC.                                                 
00751                                                                   
00752      IF PI-COMPANY-ID = 'DMD'                                     
00753          GO TO 0150-DMD-CNTL-GRP-EDIT.                            
00754                                                                   
00755      IF EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX)                
00756                                  NOT GREATER THAN ZERO            
00757          MOVE AL-UNNOF  TO  EL176A-CONTROL-GROUP-ATTRB            
00758                                                     (EL176A-INDEX)
00759          GO TO 0190-MAIN-LOGIC.                                   
00760                                                                   
00761      IF EL176A-CONTROL-GROUP (EL176A-INDEX) IS NOT NUMERIC        
00762          MOVE AL-UNBON  TO  EL176A-CONTROL-GROUP-ATTRB            
00763                                                     (EL176A-INDEX)
00764          MOVE -1  TO  EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX)  
00765          MOVE ER-0369               TO  EMI-ERROR                 
00766          PERFORM 9900-ERROR-FORMAT                                
00767          GO TO 0190-MAIN-LOGIC.                                   
00768                                                                   
00769      MOVE EL176A-CONTROL-GROUP (EL176A-INDEX)                     
00770                                  TO  PI-CONTROL-GROUP (PI-INDEX). 
00771      SET PI-INDEX UP BY +1.                                       
00772      MOVE AL-UNNON  TO  EL176A-CONTROL-GROUP-ATTRB (EL176A-INDEX).
00773                                                                   
00774      IF PI-INDEX IS GREATER THAN +2                               
00775        AND PI-CONTROL-GROUP (PI-INDEX - 2)                        
00776                      NOT LESS THAN PI-CONTROL-GROUP (PI-INDEX - 1)
00777          MOVE ER-0385               TO  EMI-ERROR                 
00778          PERFORM 9900-ERROR-FORMAT                                
00779          MOVE -1 TO EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX)    
00780          MOVE AL-UNBON TO EL176A-CONTROL-GROUP-ATTRB              
00781                                                    (EL176A-INDEX).
00782                                                                   
00783  0125-MAIN-LOGIC.                                                 
00784                                                                   
00785      MOVE ZERO                   TO  WS-NOT-FOUND.                
00786                                                                   
00787      MOVE LOW-VALUES             TO  WS-CHECK-AIX-KEY.            
00788                                                                   
00789      MOVE PI-COMPANY-CD          TO  WS-CQK-COMPANY-CD-A1.        
00790                                                                   
00791      IF PI-COMPANY-ID NOT = 'DMD'                                 
00792          MOVE EL176A-CONTROL-GROUP (EL176A-INDEX)                 
00793                                  TO  WS-CQK-CONTROL-NUMBER-A1     
00794          MOVE +5                 TO  WS-BROWSE-LENGTH             
00795      ELSE                                                         
00796          MOVE +1                 TO  WS-BROWSE-LENGTH.            
00797                                                                   
00798      MOVE 'CHKQ'                 TO  FILE-SWITCH.                 
00799                                                                   
00800      IF WS-CHECK-QUEUE-BROWSE-SW = ZERO                           
00801          EXEC CICS STARTBR                                        
00802              DATASET   (WS-CHECK-QUEUE-AIX-DSID)                  
00803              RIDFLD    (WS-CHECK-AIX-KEY)                         
00804              GENERIC   EQUAL                                      
00805              KEYLENGTH (WS-BROWSE-LENGTH)                         
00806          END-EXEC                                                 
00807          MOVE +1                 TO  WS-CHECK-QUEUE-BROWSE-SW     
00808        ELSE                                                       
00809          EXEC CICS RESETBR                                        
00810              DATASET   (WS-CHECK-QUEUE-AIX-DSID)                  
00811              RIDFLD    (WS-CHECK-AIX-KEY)                         
00812              GENERIC   EQUAL                                      
00813              KEYLENGTH (5)                                        
00814          END-EXEC.                                                
00815                                                                   
00816  0130-MAIN-LOGIC.                                                 
00817      EXEC CICS READNEXT                                           
00818          DATASET (WS-CHECK-QUEUE-AIX-DSID)                        
00819          RIDFLD  (WS-CHECK-AIX-KEY)                               
00820          SET     (ADDRESS OF CHECK-QUE)                           
00821      END-EXEC.                                                    
00822                                                                   
00823      IF WS-CQK-COMPANY-CD-A1 NOT = PI-COMPANY-CD                  
00824          GO TO 0170-MAIN-LOGIC.                                   
00825                                                                   
00826      IF PI-COMPANY-ID NOT = 'DMD'                                 
00827          IF WS-CQK-CONTROL-NUMBER-A1 NOT = EL176A-CONTROL-GROUP   
00828                                                     (EL176A-INDEX)
00829              GO TO 0170-MAIN-LOGIC                                
00830          ELSE                                                     
00831              NEXT SENTENCE                                        
00832      ELSE                                                         
00833          IF CQ-CONTROL-NUMBER NOT = PI-CONTROL-GROUP(1) AND       
00834             CQ-CONTROL-NUMBER NOT = PI-CONTROL-GROUP(2) AND       
00835             CQ-CONTROL-NUMBER NOT = PI-CONTROL-GROUP(3) AND       
00836             CQ-CONTROL-NUMBER NOT = PI-CONTROL-GROUP(4)           
00837                GO TO 0130-MAIN-LOGIC.                             
00838                                                                   
00839      IF CQ-ENTRY-TYPE NOT = 'Q'                                   
00840          GO TO 0130-MAIN-LOGIC.                                   
00841                                                                   
00842      IF NOT PI-NO-CARRIER-SECURITY                                
00843         IF CQ-CARRIER NOT = PI-CARRIER-SECURITY                   
00844            MOVE ER-2370 TO EMI-ERROR                              
00845            PERFORM 9900-ERROR-FORMAT                              
00846            MOVE -1 TO EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX)  
00847            MOVE AL-UNBON TO EL176A-CONTROL-GROUP-ATTRB            
00848                                               (EL176A-INDEX)      
00849            GO TO 0190-MAIN-LOGIC.                                 
00850                                                                   
00851      IF PI-COMPANY-ID = 'POS' OR 'TAO' OR 'CSL'                   
00852          MOVE CQ-CARRIER         TO  PI-CARRIER.                  
00853                                                                   
00854      IF AOPTIONI = '2'                                            
00855        AND CQ-TIMES-PRINTED NOT GREATER THAN ZERO                 
00856          GO TO 0190-MAIN-LOGIC.                                   
00857                                                                   
00858      IF AOPTIONI = '3'                                            
00859        AND CQ-TIMES-PRINTED GREATER THAN ZERO                     
00860          GO TO 0190-MAIN-LOGIC.                                   
00861                                                                   
00862      GO TO 0130-MAIN-LOGIC.                                       
00863                                                                   
00864      EJECT                                                        
00865                                                                   
00866 *    NOTE ******************************************************* 
00867 *         *  DMD  - CHECK CONTROL GROUP REQUESTS                * 
00868 *         *******************************************************.
00869                                                                   
00870  0150-DMD-CNTL-GRP-EDIT.                                          
00871                                                                   
00872      IF AOPTIONI = 1                                              
00873          GO TO 0190-MAIN-LOGIC.                                   
00874 ***************************************************************** 
00875 ***       IF PROCESSING BY CONTROL GROUP DETERMINE THAT THERE   * 
00876 ***       IS A LEAST ONE CONTROL GROUP ENTERED.                 * 
00877 ***************************************************************** 
00878                                                                   
00879      IF EL176A-CONTROL-GROUP-LENGTH (1)                           
00880                              NOT GREATER THAN ZERO AND            
00881         EL176A-CONTROL-GROUP-LENGTH (2)                           
00882                              NOT GREATER THAN ZERO AND            
00883         EL176A-CONTROL-GROUP-LENGTH (3)                           
00884                              NOT GREATER THAN ZERO AND            
00885         EL176A-CONTROL-GROUP-LENGTH (4)                           
00886                              NOT GREATER THAN ZERO                
00887          MOVE -1                 TO  ACG01L                       
00888          MOVE AL-UNBOF  TO  ACG01A ACG02A ACG03A ACG04A           
00889          MOVE ER-0370            TO  EMI-ERROR                    
00890          PERFORM 9900-ERROR-FORMAT                                
00891          GO TO 0190-MAIN-LOGIC.                                   
00892                                                                   
00893 ***************************************************************** 
00894 ***       DETERMINE THAT CONTROL GROUPS REQUESTED ARE NUMERIC   * 
00895 ***************************************************************** 
00896                                                                   
00897      IF EL176A-CONTROL-GROUP-LENGTH (1)                           
00898                              GREATER THAN ZERO AND                
00899         EL176A-CONTROL-GROUP (1) NOT NUMERIC                      
00900              MOVE AL-UNBON    TO  EL176A-CONTROL-GROUP-ATTRB (1)  
00901              MOVE -1          TO  EL176A-CONTROL-GROUP-LENGTH (1) 
00902              MOVE ER-0369     TO  EMI-ERROR                       
00903              PERFORM 9900-ERROR-FORMAT                            
00904              GO TO 0190-MAIN-LOGIC.                               
00905                                                                   
00906      IF EL176A-CONTROL-GROUP-LENGTH (2)                           
00907                              GREATER THAN ZERO AND                
00908         EL176A-CONTROL-GROUP (2) NOT NUMERIC                      
00909              MOVE AL-UNBON    TO  EL176A-CONTROL-GROUP-ATTRB (2)  
00910              MOVE -1          TO  EL176A-CONTROL-GROUP-LENGTH (2) 
00911              MOVE ER-0369     TO  EMI-ERROR                       
00912              PERFORM 9900-ERROR-FORMAT                            
00913              GO TO 0190-MAIN-LOGIC.                               
00914                                                                   
00915      IF EL176A-CONTROL-GROUP-LENGTH (3)                           
00916                              GREATER THAN ZERO AND                
00917         EL176A-CONTROL-GROUP (3) NOT NUMERIC                      
00918              MOVE AL-UNBON    TO  EL176A-CONTROL-GROUP-ATTRB (3)  
00919              MOVE -1          TO  EL176A-CONTROL-GROUP-LENGTH (3) 
00920              MOVE ER-0369     TO  EMI-ERROR                       
00921              PERFORM 9900-ERROR-FORMAT                            
00922              GO TO 0190-MAIN-LOGIC.                               
00923                                                                   
00924      IF EL176A-CONTROL-GROUP-LENGTH (4)                           
00925                              GREATER THAN ZERO AND                
00926         EL176A-CONTROL-GROUP (4) NOT NUMERIC                      
00927              MOVE AL-UNBON    TO  EL176A-CONTROL-GROUP-ATTRB (4)  
00928              MOVE -1          TO  EL176A-CONTROL-GROUP-LENGTH (4) 
00929              MOVE ER-0369     TO  EMI-ERROR                       
00930              PERFORM 9900-ERROR-FORMAT                            
00931              GO TO 0190-MAIN-LOGIC.                               
00932                                                                   
00933 ***************************************************************** 
00934 ***       DETERMINE THAT CONTROL GROUPS REQUESTED ARE IN        * 
00935 ***       SEQUENCE.                                             * 
00936 ***************************************************************** 
00937                                                                   
00938      IF EL176A-CONTROL-GROUP-LENGTH (2)                           
00939                              GREATER THAN ZERO AND                
00940         EL176A-CONTROL-GROUP (2) NOT GREATER THAN                 
00941                                  EL176A-CONTROL-GROUP (1)         
00942          MOVE ER-0385            TO  EMI-ERROR                    
00943          PERFORM 9900-ERROR-FORMAT                                
00944          MOVE -1              TO EL176A-CONTROL-GROUP-LENGTH (2)  
00945          MOVE AL-UNBON        TO EL176A-CONTROL-GROUP-ATTRB (2)   
00946          GO TO 0190-MAIN-LOGIC.                                   
00947                                                                   
00948      IF EL176A-CONTROL-GROUP-LENGTH (3)                           
00949                              GREATER THAN ZERO AND                
00950         EL176A-CONTROL-GROUP (3) NOT GREATER THAN                 
00951                                  EL176A-CONTROL-GROUP (2)         
00952          MOVE ER-0385            TO  EMI-ERROR                    
00953          PERFORM 9900-ERROR-FORMAT                                
00954          MOVE -1              TO EL176A-CONTROL-GROUP-LENGTH (3)  
00955          MOVE AL-UNBON        TO EL176A-CONTROL-GROUP-ATTRB (3)   
00956          GO TO 0190-MAIN-LOGIC.                                   
00957                                                                   
00958      IF EL176A-CONTROL-GROUP-LENGTH (4)                           
00959                              GREATER THAN ZERO AND                
00960         EL176A-CONTROL-GROUP (4) NOT GREATER THAN                 
00961                                  EL176A-CONTROL-GROUP (3)         
00962          MOVE ER-0385            TO  EMI-ERROR                    
00963          PERFORM 9900-ERROR-FORMAT                                
00964          MOVE -1              TO EL176A-CONTROL-GROUP-LENGTH (4)  
00965          MOVE AL-UNBON        TO EL176A-CONTROL-GROUP-ATTRB (4)   
00966          GO TO 0190-MAIN-LOGIC.                                   
00967                                                                   
00968 ***************************************************************** 
00969 ***       MOVE REQUESTED CONTROL GROUPS TO THE PI AREA WITH     * 
00970 ***       ANY SKIP CONTROL FIELDS COMPRESSED.                   * 
00971 ***************************************************************** 
00972                                                                   
00973      MOVE +0                     TO  WS-DMD-CNTL-GRP-COUNT.       
00974      SET EL176A-INDEX TO +1.                                      
00975                                                                   
00976  0150-CNTL-LOOP.                                                  
00977                                                                   
00978      IF WS-DMD-CNTL-GRP-COUNT GREATER THAN +2                     
00979          GO TO 0150-END-CNTL-LOOP.                                
00980                                                                   
00981      IF EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX)                
00982                                     NOT GREATER THAN ZERO         
00983          MOVE EL176A-CONTROL-GROUP (EL176A-INDEX + 1)             
00984                                  TO  EL176A-CONTROL-GROUP         
00985                                      (EL176A-INDEX)               
00986          MOVE SPACES             TO EL176A-CONTROL-GROUP-X        
00987                                      (EL176A-INDEX + 1).          
00988      SET EL176A-INDEX UP BY +1.                                   
00989      ADD +1  TO  WS-DMD-CNTL-GRP-COUNT.                           
00990      GO TO 0150-CNTL-LOOP.                                        
00991                                                                   
00992  0150-END-CNTL-LOOP.                                              
00993                                                                   
00994      MOVE EL176A-CONTROL-GROUP (1)  TO  PI-CONTROL-GROUP (1).     
00995      MOVE EL176A-CONTROL-GROUP (2)  TO  PI-CONTROL-GROUP (2).     
00996      MOVE EL176A-CONTROL-GROUP (3)  TO  PI-CONTROL-GROUP (3).     
00997      MOVE EL176A-CONTROL-GROUP (4)  TO  PI-CONTROL-GROUP (4).     
00998                                                                   
00999      GO TO 0125-MAIN-LOGIC.                                       
01000                                                                   
01001  0170-MAIN-LOGIC.                                                 
01002      MOVE ER-0394                   TO  EMI-ERROR.                
01003      PERFORM 9900-ERROR-FORMAT.                                   
01004      MOVE -1 TO EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX).       
01005      MOVE AL-UNBON TO EL176A-CONTROL-GROUP-ATTRB (EL176A-INDEX).  
01006                                                                   
01007      GO TO 0190-MAIN-LOGIC.                                       
01008                                                                   
01009  0180-MAIN-LOGIC.                                                 
01010      MOVE ER-0387                   TO  EMI-ERROR.                
01011      PERFORM 9900-ERROR-FORMAT.                                   
01012      MOVE -1 TO EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX).       
01013      MOVE AL-UNBON TO EL176A-CONTROL-GROUP-ATTRB (EL176A-INDEX).  
01014                                                                   
01015  0190-MAIN-LOGIC.                                                 
01016                                                                   
01017      IF PI-COMPANY-ID = 'DMD'                                     
01018          NEXT SENTENCE                                            
01019      ELSE                                                         
01020          IF EL176A-INDEX LESS THAN +4                             
01021              SET EL176A-INDEX UP BY +1                            
01022              GO TO 0120-MAIN-LOGIC.                               
01023                                                                   
01024      IF WS-CHECK-QUEUE-BROWSE-SW NOT = ZERO                       
01025          MOVE ZERO               TO  WS-CHECK-QUEUE-BROWSE-SW     
01026          EXEC CICS ENDBR                                          
01027              DATASET (WS-CHECK-QUEUE-AIX-DSID)                    
01028          END-EXEC.                                                
01029                                                                   
01030      IF EMI-FATAL-CTR GREATER THAN ZERO                           
01031          PERFORM 8200-SEND-DATAONLY.                              
01032                                                                   
01033      IF PI-COMPANY-ID = 'DMD'                                     
01034          GO TO 0200-MAIN-LOGIC.                                   
01035                                                                   
01036      IF AOPTIONI = ('2' OR '3')                                   
01037        AND PI-INDEX NOT GREATER THAN +1                           
01038          MOVE -1                 TO  ACG01L                       
01039          MOVE AL-UNBOF  TO  ACG01A ACG02A ACG03A ACG04A           
01040          MOVE ER-0370               TO  EMI-ERROR                 
01041          PERFORM 9900-ERROR-FORMAT.                               
01042                                                                   
01043      IF PI-INDEX GREATER THAN +1                                  
01044          NEXT SENTENCE                                            
01045        ELSE                                                       
01046          GO TO 0200-MAIN-LOGIC.                                   
01047                                                                   
01048      SET PI-INDEX DOWN BY +1.                                     
01049      SET PI-NUMBER-OF-CONTROL-GROUPS TO PI-INDEX.                 
01050      SET PI-INDEX                                                 
01051          EL176A-INDEX TO +1.                                      
01052                                                                   
01053  0195-MAIN-LOGIC.                                                 
01054      IF PI-CONTROL-GROUP (PI-INDEX) GREATER THAN ZERO             
01055          MOVE PI-CONTROL-GROUP (PI-INDEX)                         
01056                      TO  EL176A-CONTROL-GROUP (EL176A-INDEX)      
01057          MOVE AL-UNNON TO EL176A-CONTROL-GROUP-ATTRB              
01058                                                     (EL176A-INDEX)
01059      ELSE                                                         
01060          MOVE SPACES TO  EL176A-CONTROL-GROUP-X (EL176A-INDEX)    
01061          MOVE AL-UNNOF TO EL176A-CONTROL-GROUP-ATTRB              
01062                                                    (EL176A-INDEX).
01063                                                                   
01064      IF PI-INDEX LESS THAN +4                                     
01065          SET PI-INDEX                                             
01066              EL176A-INDEX UP BY +1                                
01067          GO TO 0195-MAIN-LOGIC.                                   
01068                                                                   
01069      SET EL176A-INDEX                                             
01070          PI-INDEX TO +1.                                          
01071                                                                   
01072      EJECT                                                        
01073  0200-MAIN-LOGIC.                                                 
01074 *    NOTE ******************************************************* 
01075 *         *      ALL OF THE SYNTAX CHECKS HAVE BEEN SUCCESSFUL. * 
01076 *         *  NOW DO THE PRE-EDIT.                               * 
01077 *         *                                                     * 
01078 *         *      BEFORE A CHECK BATCH IS QUEUED FOR PRINT, A    * 
01079 *         *  PRE-EDIT IS DONE TO ASSURE CONSISTENCY IN          * 
01080 *         *  PROCESSING.  THIS EDIT CONSISTS OF THE FOLLOWING   * 
01081 *         *  STEPS:                                             * 
01082 *         *                                                     * 
01083 *         *  1. IF A STARTING CHECK NUMBER HAS BEEN ENTERED,    * 
01084 *         *     THE NUMBER IS COMPARED TO OTHER NUMBERS IN THE  * 
01085 *         *     FILE FOR OVERLAPS AND GAPS.                     * 
01086 *         *                                                     * 
01087 *         *  2. IF ANY CHECKS IN THE RELEASED GROUPS HAVE       * 
01088 *         *     ALREADY BEEN QUEUED FOR PRINT OR PRINTED.       * 
01089 *         *                                                     * 
01090 *         *  3. IF PRE-NUMBERING IS NOT USED THAT ALL CHECKS    * 
01091 *         *     HAVE A CHECK NUMBER ASSIGNED.                   * 
01092 *         *                                                     * 
01093 *         *  4. IF DUPLICATE CHECK NUMBERS ARE ASSIGNED.        * 
01094 *         *                                                     * 
01095 *         *      BEFORE A CHECK BATCH IS QUEUED FOR RE-PRINT, A * 
01096 *         *  PRE-EDIT IS DONE TO ASSURE CONSISTENCY IN          * 
01097 *         *  PROCESSING.  THIS EDIT CONSISTS OF THE FOLLOWING   * 
01098 *         *  STEPS:                                             * 
01099 *         *                                                     * 
01100 *         *  1. ALL CHECKS IN THE INDICATED GROUP(S) MUST HAVE  * 
01101 *         *     BEEN PREVIOUSLY PRINTED.                        * 
01102 *         *                                                     * 
01103 *         *  2. IF THE PRE-NUMBERING SWITCH IS SET IN ANY RECORD* 
01104 *         *     IT MUST BE SET IN ALL RECORDS.                  * 
01105 *         *                                                     * 
01106 *         *  3. IF A STARTING CHECK NUMBER HAS BEEN ENTERED,    * 
01107 *         *     THE NUMBER IS COMPARED TO OTHER NUMBERS IN THE  * 
01108 *         *     FILE FOR OVERLAPS AND GAPS.                     * 
01109 *         *******************************************************.
01110                                                                   
01111      MOVE PI-COMPANY-ID          TO  WS-ENQ-COMPANY-ID.           
01112                                                                   
01113      EXEC CICS ENQ                                                
01114          RESOURCE (WS-CHECK-QUEUE-DSID)                           
01115          LENGTH   (11)                                            
01116      END-EXEC.                                                    
01117                                                                   
01118      EXEC CICS HANDLE CONDITION                                   
01119          NOTFND  (0225-MAIN-LOGIC)                                
01120          ENDFILE (0230-MAIN-LOGIC)                                
01121      END-EXEC.                                                    
01122                                                                   
01123      MOVE LOW-VALUES             TO  WS-CHECK-AIX-KEY             
01124                                      WS-LAST-CHECK-QUEUE-KEY.     
01125      MOVE +5                     TO  WS-KEY-LENGTH.               
01126                                                                   
01127      MOVE PI-COMPANY-CD          TO  WS-CQK-COMPANY-CD-A1.        
01128      MOVE 'CHKQ'                 TO  FILE-SWITCH.                 
01129                                                                   
01130      EXEC CICS STARTBR                                            
01131          DATASET (WS-CHECK-QUEUE-AIX-DSID)                        
01132          RIDFLD  (WS-CHECK-AIX-KEY)                               
01133          GTEQ                                                     
01134      END-EXEC.                                                    
01135                                                                   
01136  0210-MAIN-LOGIC.                                                 
01137      MOVE EMI-FATAL-CTR         TO  WS-LAST-ERROR-COUNT.          
01138                                                                   
01139      EXEC CICS READNEXT                                           
01140          DATASET (WS-CHECK-QUEUE-AIX-DSID)                        
01141          RIDFLD  (WS-CHECK-AIX-KEY)                               
01142          SET     (ADDRESS OF CHECK-QUE)                           
01143      END-EXEC.                                                    
01144                                                                   
01145      IF CQ-COMPANY-CD NOT = PI-COMPANY-CD                         
01146          GO TO 0230-MAIN-LOGIC.                                   
01147                                                                   
01148      MOVE '1'                   TO  PI-VALID-RCD-SW.              
01149                                                                   
01150 *    NOTE ******************************************************* 
01151 *         *      SAVE THE CHECK NUMBER SO AT THE END OF THE     * 
01152 *         *  BROWSE YOU CAN CHECK FOR GAPS OR OVERLAPS.         * 
01153 *         *******************************************************.
01154                                                                   
01155      IF CQ-CHECK-NUMBER GREATER THAN WS-GREATEST-CHECK-NUMBER-X   
01156          MOVE CQ-CHECK-NUMBER    TO  WS-GREATEST-CHECK-NUMBER-X.  
01157                                                                   
01158 *    NOTE ******************************************************* 
01159 *         *      IF YOU ARE PROCESSING BY GROUPS BYPASS ALL     * 
01160 *         *  RECORDS IF NOT IN SPECEFIED GROUPS.  SAVE THE HIGH * 
01161 *         *  SEQUENCE NUMBER IN EACH GROUP FOR REPRINT.         * 
01162 *         *******************************************************.
01163                                                                   
01164      IF AOPTIONI = '2' OR '3'                                     
01165          NEXT SENTENCE                                            
01166        ELSE                                                       
01167          GO TO 0213-MAIN-LOGIC.                                   
01168                                                                   
01169      SET PI-INDEX                                                 
01170          EL176A-INDEX TO +1.                                      
01171                                                                   
01172  0212-MAIN-LOGIC.                                                 
01173      IF CQ-CONTROL-NUMBER  = PI-CONTROL-GROUP (PI-INDEX)          
01174          IF CQ-SEQUENCE-NUMBER GREATER PI-HIGH-SEQUENCE (PI-INDEX)
01175              MOVE CQ-SEQUENCE-NUMBER                              
01176                                  TO PI-HIGH-SEQUENCE (PI-INDEX)   
01177              GO TO 0215-MAIN-LOGIC                                
01178            ELSE                                                   
01179              GO TO 0215-MAIN-LOGIC.                               
01180                                                                   
01181      IF PI-INDEX LESS THAN +4                                     
01182          SET PI-INDEX                                             
01183              EL176A-INDEX UP BY +1                                
01184          GO TO 0212-MAIN-LOGIC.                                   
01185                                                                   
01186      GO TO 0210-MAIN-LOGIC.                                       
01187                                                                   
01188  0213-MAIN-LOGIC.                                                 
01189 *    NOTE ******************************************************* 
01190 *         *      IF YOU ARE PRINTING ALL CONTROL GROUPS BYPASS  * 
01191 *         *  THE CONTROL GROUPS THAT HAVE ALREADY BEEN PRINTED. * 
01192 *         *******************************************************.
01193                                                                   
01194      IF CQ-CONTROL-NUMBER NOT = WS-LAST-CONTROL-GROUP             
01195          MOVE CQ-CONTROL-NUMBER  TO  WS-LAST-CONTROL-GROUP        
01196          MOVE CQ-TIMES-PRINTED   TO  WS-TIMES-PRINTED.            
01197                                                                   
01198      IF WS-TIMES-PRINTED GREATER THAN ZERO                        
01199          GO TO 0210-MAIN-LOGIC.                                   
01200                                                                   
01201      EJECT                                                        
01202  0215-MAIN-LOGIC.                                                 
01203      IF AOPTIONI = '1' OR '2'                                     
01204          NEXT SENTENCE                                            
01205        ELSE                                                       
01206          GO TO 0220-MAIN-LOGIC.                                   
01207                                                                   
01208      IF CQ-TIMES-PRINTED GREATER THAN ZERO                        
01209         MOVE -1 TO EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX)     
01210         MOVE AL-UNBON TO EL176A-CONTROL-GROUP-ATTRB (EL176A-INDEX)
01211         MOVE ER-0379                TO  EMI-ERROR                 
01212         PERFORM 9900-ERROR-FORMAT.                                
01213                                                                   
01214      IF AACNI = 'Y'                                               
01215        AND CQ-CHECK-NUMBER NOT = SPACES                           
01216          MOVE ER-0382            TO  EMI-ERROR                    
01217          PERFORM 9900-ERROR-FORMAT                                
01218          MOVE -1                 TO  AACNL                        
01219          MOVE AL-UABON           TO  AACNA                        
01220        ELSE                                                       
01221      IF AACNI = 'N'                                               
01222        AND CQ-CHECK-NUMBER = SPACES                               
01223          MOVE -1                 TO  AACNL                        
01224          MOVE AL-UABON           TO  AACNA                        
01225          MOVE ER-0383            TO  EMI-ERROR                    
01226          PERFORM 9900-ERROR-FORMAT.                               
01227                                                                   
01228      GO TO 0210-MAIN-LOGIC.                                       
01229                                                                   
01230  0220-MAIN-LOGIC.                                                 
01231      IF CQ-TIMES-PRINTED NOT GREATER THAN ZERO                    
01232         MOVE -1 TO EL176A-CONTROL-GROUP-LENGTH (EL176A-INDEX)     
01233         MOVE AL-UNBON TO EL176A-CONTROL-GROUP-ATTRB (EL176A-INDEX)
01234         MOVE ER-0389                TO  EMI-ERROR                 
01235         PERFORM 9900-ERROR-FORMAT.                                
01236                                                                   
01237      IF CQ-PRE-NUMBERING-SW = '1'                                 
01238        AND AACNI = 'N'                                            
01239          MOVE -1                 TO  AACNL                        
01240          MOVE AL-UABON           TO  AACNA                        
01241          MOVE ER-0390            TO  EMI-ERROR                    
01242          PERFORM 9900-ERROR-FORMAT.                               
01243                                                                   
01244      IF CQ-PRE-NUMBERING-SW = SPACES                              
01245        AND AACNI = 'Y'                                            
01246          MOVE -1                 TO  AACNL                        
01247          MOVE AL-UABON           TO  AACNA                        
01248          MOVE ER-0391            TO  EMI-ERROR                    
01249          PERFORM 9900-ERROR-FORMAT.                               
01250                                                                   
01251      GO TO 0210-MAIN-LOGIC.                                       
01252                                                                   
01253  0225-MAIN-LOGIC.                                                 
01254      MOVE ER-0490                TO  EMI-ERROR                    
01255      MOVE -1                     TO  AOPTIONL                     
01256      PERFORM 8200-SEND-DATAONLY                                   
01257      PERFORM 9100-RETURN-TRAN.                                    
01258                                                                   
01259      EJECT                                                        
01260  0230-MAIN-LOGIC.                                                 
01261      EXEC CICS ENDBR                                              
01262          DATASET (WS-CHECK-QUEUE-AIX-DSID)                        
01263      END-EXEC.                                                    
01264                                                                   
01265      IF PI-VALID-RCD-SW NOT = '1'                                 
01266          MOVE ER-2936            TO  EMI-ERROR                    
01267          MOVE -1                 TO  AOPTIONL                     
01268          GO TO 0015-MAIN-LOGIC.                                   
01269                                                                   
01270      IF EMI-FATAL-CTR GREATER THAN ZERO                           
01271          PERFORM 8200-SEND-DATAONLY.                              
01272                                                                   
01273 *    NOTE ******************************************************* 
01274 *         *      READ THE COMPANY RECORD FROM THE CONTROL FILE  * 
01275 *         *  TO GET THE CICS/VS PRINTER TERMINAL ID AND CHECK   * 
01276 *         *  TO SEE IF THE PRINTER HAS BEEN SPECIFIED.          * 
01277 *         *******************************************************.
01278                                                                   
01279      MOVE PI-COMPANY-ID          TO  WS-CONTROL-FILE-KEY.         
01280      MOVE '1'                    TO  WS-CFK-RECORD-TYPE.          
01281      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.          
01282      MOVE 'CNTL'                 TO  FILE-SWITCH.                 
01283                                                                   
01284      EXEC CICS READ                                               
01285          DATASET (WS-CONTROL-FILE-DSID)                           
01286          RIDFLD  (WS-CONTROL-FILE-KEY)                            
01287          SET    (ADDRESS OF CONTROL-FILE)                         
01288      END-EXEC.                                                    
01289                                                                   
01290      IF CF-CHECK-PRINTER-ID = SPACES                              
01291          MOVE ER-0371            TO  EMI-ERROR                    
01292          MOVE -1                 TO  APFKL                        
01293          PERFORM 9900-ERROR-FORMAT.                               
01294                                                                   
01295      IF APRTL GREATER THAN ZEROS                                  
01296          MOVE AL-UANON           TO  APRTA                        
01297          MOVE APRTI              TO  PI-CHECK-PRINTER-ID          
01298      ELSE                                                         
01299          MOVE CF-CHECK-PRINTER-ID                                 
01300                                  TO  PI-CHECK-PRINTER-ID.         
01301                                                                   
01302      MOVE CF-COMPANY-ADDRESS     TO  PI-COMPANY-ADDRESS.          
01303      MOVE CF-CURRENT-MONTH-END   TO  PI-MONTH-END-SAVE.           
01304                                                                   
01305      IF PI-COMPANY-ID = 'ADL' OR 'FLB' OR 'ALA' OR 'FND'          
01306         NEXT SENTENCE                                             
01307      ELSE                                                         
01308         GO TO 0235-MAIN-LOGIC.                                    
01309                                                                   
01310      MOVE SAVE-BIN-DATE     TO DC-BIN-DATE-1.                     
01311      MOVE PI-MONTH-END-SAVE TO DC-BIN-DATE-2.                     
01312      MOVE '1'               TO DC-OPTION-CODE.                    
01313      PERFORM 8500-DATE-CONVERSION.                                
01314                                                                   
01315      IF NO-CONVERSION-ERROR                                       
01316         IF DC-ELAPSED-MONTHS GREATER THAN +0                      
01317            MOVE DC-GREG-DATE-2-EDIT  TO WS-WORK-DATE              
01318            MOVE '01'                 TO WS-WORK-DA                
01319            MOVE WS-WORK-DATE         TO DC-GREG-DATE-1-EDIT       
01320            MOVE '2'                  TO DC-OPTION-CODE            
01321            PERFORM 8500-DATE-CONVERSION                           
01322            IF NO-CONVERSION-ERROR                                 
01323               MOVE DC-BIN-DATE-1     TO WS-CHECK-WRITER-DATE      
01324               MOVE ZEROS             TO DC-ELAPSED-MONTHS         
01325               IF DC-DAY-OF-WEEK = +1                              
01326                  MOVE +1             TO DC-ELAPSED-DAYS           
01327                  MOVE '6'            TO DC-OPTION-CODE            
01328                  PERFORM 8500-DATE-CONVERSION                     
01329                  IF NO-CONVERSION-ERROR                           
01330                     MOVE DC-BIN-DATE-2 TO WS-CHECK-WRITER-DATE    
01331                  ELSE                                             
01332                     NEXT SENTENCE                                 
01333               ELSE                                                
01334               IF DC-DAY-OF-WEEK = +7                              
01335                  MOVE +2          TO DC-ELAPSED-DAYS              
01336                  MOVE '6'         TO DC-OPTION-CODE               
01337                  PERFORM 8500-DATE-CONVERSION                     
01338                  IF NO-CONVERSION-ERROR                           
01339                     MOVE DC-BIN-DATE-2 TO WS-CHECK-WRITER-DATE.   
01340                                                                   
01341  0235-MAIN-LOGIC.                                                 
01342                                                                   
01343      IF AACNI = 'Y'                                               
01344          IF WS-GREATEST-CHECK-NUMBER-X NOT LESS THAN ACKNOI       
01345              MOVE ER-0380        TO  EMI-ERROR                    
01346              PERFORM 9900-ERROR-FORMAT                            
01347              MOVE -1             TO  ACKNOL                       
01348              MOVE AL-UNBON       TO  ACKNOA                       
01349            ELSE                                                   
01350              SUBTRACT +1 FROM ACKNOO GIVING WS-ACKNO              
01351              IF WS-GREATEST-CHECK-NUMBER-X NOT = WS-ACKNO-X       
01352                  MOVE ER-0381    TO  EMI-ERROR                    
01353                  PERFORM 9900-ERROR-FORMAT                        
01354                  MOVE -1         TO  ACKNOL                       
01355                  MOVE AL-UNBON   TO  ACKNOA.                      
01356                                                                   
CIDMOD*    IF PI-COMPANY-ID NOT = 'DMD' AND 'LGX'                       
PEMMOD*    IF PI-COMPANY-ID NOT = 'DMD' AND 'LGX' AND 'CID' AND 'CSO'   
01358      GO TO 0237-MAIN-LOGIC.                                       
01359                                                                   
01360      EXEC CICS HANDLE CONDITION                                   
01361          NOTOPEN    (8800-NOT-OPEN)                               
01362          NOTFND     (0236-MAIN-LOGIC)                             
01363      END-EXEC.                                                    
01364                                                                   
01365      MOVE 'MICR'                 TO  WS-DMD-FLAG-KEY.             
01366      MOVE 'FLAG'                 TO  FILE-SWITCH.                 
01367                                                                   
01368      EXEC CICS READ                                               
01369          DATASET ('MICRFLAG')                                     
01370          RIDFLD  (WS-DMD-FLAG-KEY)                                
01371          INTO    (WS-DMD-MICR-FLAG)                               
01372      END-EXEC.                                                    
01373                                                                   
01374  0236-MAIN-LOGIC.                                                 
01375                                                                   
01376      EXEC CICS HANDLE CONDITION                                   
01377          NOTOPEN    (8800-NOT-OPEN)                               
01378          NOTFND     (0237-MAIN-LOGIC)                             
01379      END-EXEC.                                                    
01380                                                                   
01381      MOVE '420E'                 TO  WS-DMD-DRFT-KEY.             
01382      MOVE 'DRFT'                 TO  FILE-SWITCH.                 
01383                                                                   
01384      EXEC CICS READ                                               
01385          DATASET ('MICRDRFT')                                     
01386          RIDFLD  (WS-DMD-DRFT-KEY)                                
01387          INTO    (WS-DMD-MICR-DRFT)                               
01388      END-EXEC.                                                    
01389                                                                   
01390      MOVE '    '                 TO  FILE-SWITCH.                 
01391                                                                   
01392  0237-MAIN-LOGIC.                                                 
01393                                                                   
01394      IF EMI-FATAL-CTR GREATER THAN ZERO                           
01395          PERFORM 8200-SEND-DATAONLY.                              
01396                                                                   
01397      MOVE AL-SANON               TO  AOPTIONA                     
01398                                      ACG01A                       
01399                                      ACG02A                       
01400                                      ACG03A                       
01401                                      ACG04A                       
01402                                      AACNA                        
01403                                      ACKNOA                       
01404                                      APRTA.                       
01405                                                                   
01406      MOVE +1                     TO  PI-PROCESSING-SW.            
01407      MOVE -1                     TO  AALIGNL.                     
01408                                                                   
01409      IF PI-NUMBER-OF-ALIGNMENT-CHECKS GREATER THAN ZERO           
01410          MOVE ER-0361               TO  EMI-ERROR                 
01411        ELSE                                                       
01412          MOVE ER-0362               TO  EMI-ERROR.                
01413                                                                   
01414      PERFORM 8200-SEND-DATAONLY.                                  
01415                                                                   
01416      EXEC CICS HANDLE AID                                         
01417          CLEAR (9400-CLEAR)                                       
01418          PA1   (0040-MAIN-LOGIC)                                  
01419          PA2   (0040-MAIN-LOGIC)                                  
01420          PA3   (0040-MAIN-LOGIC)                                  
01421      END-EXEC.                                                    
01422                                                                   
01423      EXEC CICS SYNCPOINT                                          
01424      END-EXEC.                                                    
01425                                                                   
01426      GO TO 0040-MAIN-LOGIC.                                       
01427                                                                   
01428      EJECT                                                        
01429  0240-MAIN-LOGIC.                                                 
01430      IF AALIGNL GREATER THAN ZERO                                 
01431          IF AALIGNI IS NUMERIC                                    
01432              MOVE AALIGNO      TO  PI-NUMBER-OF-ALIGNMENT-CHECKS  
01433              MOVE AL-UNNON     TO  AALIGNA                        
01434            ELSE                                                   
01435              MOVE ER-0365      TO  EMI-ERROR                      
01436              MOVE -1           TO  AALIGNL                        
01437              MOVE AL-UNBON     TO  AALIGNA                        
01438              PERFORM 8200-SEND-DATAONLY                           
01439              GO TO 0040-MAIN-LOGIC.                               
01440                                                                   
01441      IF PI-NUMBER-OF-ALIGNMENT-CHECKS NOT GREATER THAN ZERO       
01442          GO TO 0300-MAIN-LOGIC.                                   
01443                                                                   
01444      IF PI-ALIGNMENT-CONTROL-GROUP GREATER THAN ZERO              
01445          GO TO 0245-MAIN-LOGIC.                                   
01446                                                                   
01447      MOVE PI-COMPANY-ID          TO  WS-CONTROL-FILE-KEY.         
01448      MOVE '1'                    TO  WS-CFK-RECORD-TYPE.          
01449      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.          
01450      MOVE 'CNTL'                 TO  FILE-SWITCH.                 
01451                                                                   
01452      EXEC CICS READ UPDATE                                        
01453          DATASET (WS-CONTROL-FILE-DSID)                           
01454          RIDFLD  (WS-CONTROL-FILE-KEY)                            
01455          SET    (ADDRESS OF CONTROL-FILE)                         
01456      END-EXEC.                                                    
01457                                                                   
01458      ADD +1  TO  CF-CO-CHECK-QUE-COUNTER.                         
01459                                                                   
01460      IF CO-QUE-COUNT-RESET                                        
01461          MOVE +1                 TO  CF-CO-CHECK-QUE-COUNTER.     
01462                                                                   
01463      MOVE CF-CO-CHECK-QUE-COUNTER TO PI-ALIGNMENT-CONTROL-GROUP.  
01464                                                                   
01465      EXEC CICS REWRITE                                            
01466          DATASET (WS-CONTROL-FILE-DSID)                           
01467          FROM    (CONTROL-FILE)                                   
01468      END-EXEC.                                                    
01469                                                                   
01470      MOVE EIBTRMID               TO  PI-TSK-TERM-ID.              
01471      MOVE EIBTIME                TO  PI-TSK-TIME.                 
01472      MOVE PI-COMPANY-ID          TO  WS-CHECK-TRANSID-2.          
01473      IF PI-COMPANY-ID = 'COM'                                     
01474          MOVE 'COM'              TO  WS-CHECK-TRANSID-2.          
01475                                                                   
01476      IF PI-COMPANY-ID = 'PEM' OR 'CLS' OR 'ACC' OR 'MOD'          
01477          MOVE 'LGX'              TO  WS-CHECK-TRANSID-2.          
01478                                                                   
01479      IF PI-COMPANY-ID = 'KSM'                                     
01480          MOVE 'KSA'              TO  WS-CHECK-TRANSID-2.          
01481                                                                   
01482      IF PI-COMPANY-ID = 'ADL' OR 'DEF' OR 'FLB' OR 'ALA'          
01483          MOVE 'ADL'              TO  WS-CHECK-TRANSID-2.          
01484                                                                   
01485      IF PI-COMPANY-ID = 'FGL'                                     
01486          MOVE 'CGL'              TO  WS-CHECK-TRANSID-2.          
01487                                                                   
01488      IF PI-COMPANY-ID = 'LAP'                                     
01489          MOVE 'RMC'              TO  WS-CHECK-TRANSID-2.          
01490                                                                   
01491      IF PI-COMPANY-ID = 'TII'                                     
01492          MOVE 'TIH'              TO  WS-CHECK-TRANSID-2.          
01493                                                                   
01494      IF PI-COMPANY-ID = 'CVL'  OR  'CNL'                          
01495          MOVE 'MNL'              TO  WS-CHECK-TRANSID-2.          
01496                                                                   
01497      IF PI-COMPANY-ID = 'AUK'                                     
01498          MOVE 'AIG'              TO  WS-CHECK-TRANSID-2.          
01499                                                                   
01500      IF PI-COMPANY-ID = 'BPI'                                     
01501          MOVE 'UCL'              TO  WS-CHECK-TRANSID-2.          
01502                                                                   
01503      IF PI-COMPANY-ID = 'ITY' OR 'FLC' OR 'FRO' OR 'FRN'          
01504                               OR 'FRS' OR 'FRT' OR 'FRH'          
01505                               OR 'OFI' OR 'OFJ' OR 'CAB'          
01506                               OR 'AFL' OR 'AFC' OR 'SRL'          
01507                               OR 'RIC'                            
01508          MOVE 'ITY'              TO  WS-CHECK-TRANSID-2.          
01509                                                                   
01510      IF PI-COMPANY-ID = 'JAL' OR 'JAI'                            
01511         NEXT SENTENCE                                             
01512      ELSE                                                         
030612         IF PI-COMPANY-ID = 'DMD' OR 'CID' OR 'DCC' OR 'AHL'                       
01514              EXEC CICS START                                      
CIDMOD                 TRANSID ('EX47')                                 
01516                  FROM    (PROGRAM-INTERFACE-BLOCK)                
01517                  LENGTH  (PI-COMM-LENGTH)                         
PEMTMP*                TERMID  ('A199')
01518              END-EXEC                                             
01519          ELSE                                                     
01520              EXEC CICS START                                      
01521                  TRANSID (WS-CHECK-WRITER-TRANS-ID)               
01522                  FROM    (PROGRAM-INTERFACE-BLOCK)                
01523                  LENGTH  (PI-COMM-LENGTH)                         
01524                  TERMID  (PI-CHECK-PRINTER-ID)                    
01525              END-EXEC.                                            
01526                                                                   
01527      MOVE +1                     TO  PI-PRINTER-STARTED-SW.       
01528                                                                   
01529      EXEC CICS GETMAIN                                            
01530          SET     (ADDRESS OF CHECK-QUE)                           
01531          LENGTH  (100)                                            
01532          INITIMG (WS-SPACES)                                      
01533      END-EXEC.                                                    
01534                                                                   
01535  0245-MAIN-LOGIC.                                                 
01536      MOVE SPACES                 TO  CHECK-QUE.                   
01537                                                                   
01538      MOVE 'CQ'                   TO  CQ-RECORD-ID.                
01539                                                                   
01540      MOVE LOW-VALUES             TO  CQ-CONTROL-BY-PAYEE.         
01541      MOVE PI-COMPANY-CD          TO  CQ-COMPANY-CD                
01542                                      CQ-COMPANY-CD-A1.            
01543      MOVE 'A'                    TO  CQ-ENTRY-TYPE.               
01544                                                                   
01545      MOVE PI-ALIGNMENT-CONTROL-GROUP                              
01546                                  TO  CQ-CONTROL-NUMBER            
01547                                      CQ-CONTROL-NUMBER-A1.        
01548                                                                   
01549      MOVE ZERO                   TO  CQ-CHECK-AMOUNT.             
01550      MOVE +1                     TO  CQ-TIMES-PRINTED.            
01551      MOVE WS-CHECK-WRITER-DATE   TO  CQ-CHECK-WRITTEN-DT.         
01552                                                                   
01553      MOVE +1760                  TO  CQ-LAST-UPDATED-BY.          
01554      MOVE EIBTIME                TO  CQ-LAST-UPDATED-HHMMSS.      
01555      MOVE LOW-VALUES             TO  CQ-APPLIED-TO-RCON-DT.       
01556      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             
01557      MOVE '5'                    TO  DC-OPTION-CODE.              
01558      PERFORM 8500-DATE-CONVERSION.                                
01559      MOVE DC-BIN-DATE-1          TO  CQ-LAST-UPDATED-DT.          
01560                                                                   
01561  0250-MAIN-LOGIC.                                                 
01562      IF AACNI = 'Y'                                               
01563          MOVE WS-CHECK-NUMBER-X  TO  CQ-CHECK-NUMBER              
01564          ADD +1  TO  WS-CHECK-NUMBER                              
01565          MOVE '1'                TO  CQ-PRE-NUMBERING-SW.         
01566                                                                   
01567      MOVE PI-ALIGNMENT-SEQUENCE-NO  TO  CQ-SEQUENCE-NUMBER        
01568                                  CQ-SEQUENCE-NUMBER-A1.           
01569      ADD +1  TO  PI-ALIGNMENT-SEQUENCE-NO.                        
01570                                                                   
01571      MOVE SPACES                 TO  CHECK-PASS-AREA.             
01572                                                                   
01573      MOVE +1                     TO  CPA-ALIGNMENT.               
01574      MOVE CQ-CHECK-NUMBER        TO  CPA-CHECK-NUMBER.            
01575                                                                   
01576      IF PI-COMPANY-ID = 'CSL'                                     
01577          MOVE PI-CARRIER         TO  CPA-CARRIER.                 
01578                                                                   
01579      PERFORM 0800-PRINT-CHECK.                                    
01580                                                                   
01581      IF AACNI = 'Y'                                               
01582          EXEC CICS WRITE                                          
01583              DATASET (WS-CHECK-QUEUE-DSID)                        
01584              RIDFLD  (CQ-CONTROL-PRIMARY)                         
01585              FROM    (CHECK-QUE)                                  
01586          END-EXEC.                                                
01587                                                                   
01588      SUBTRACT +1 FROM PI-NUMBER-OF-ALIGNMENT-CHECKS.              
01589                                                                   
01590      IF PI-NUMBER-OF-ALIGNMENT-CHECKS IS GREATER THAN ZERO        
01591          GO TO 0250-MAIN-LOGIC.                                   
01592                                                                   
01593      MOVE SPACES                    TO  AALIGNI.                  
01594      MOVE AL-UNNOF                  TO  AALIGNA.                  
01595      MOVE -1                        TO  AALIGNL.                  
01596      MOVE ER-0362                   TO  EMI-ERROR.                
01597      PERFORM 8200-SEND-DATAONLY.                                  
01598                                                                   
01599      EXEC CICS SYNCPOINT                                          
01600      END-EXEC.                                                    
01601                                                                   
01602      GO TO 0040-MAIN-LOGIC.                                       
01603                                                                   
01604      EJECT                                                        
01605  0300-MAIN-LOGIC.                                                 
01606      MOVE PI-COMPANY-ID          TO  WS-CHECK-TRANSID-2.          
01607                                                                   
01608      IF PI-COMPANY-ID = 'COM'                                     
01609          MOVE 'COM'              TO  WS-CHECK-TRANSID-2.          
01610                                                                   
01611      IF PI-COMPANY-ID = 'PEM' OR 'CLS' OR 'ACC' OR 'MOD'          
01612          MOVE 'LGX'              TO  WS-CHECK-TRANSID-2.          
01613                                                                   
01614                                                                   
01615      IF PI-COMPANY-ID = 'KSM'                                     
01616          MOVE 'KSA'              TO  WS-CHECK-TRANSID-2.          
01617                                                                   
01618      IF PI-COMPANY-ID = 'ADL' OR 'DEF' OR 'FLB' OR 'ALA'          
01619          MOVE 'ADL'              TO  WS-CHECK-TRANSID-2.          
01620                                                                   
01621      IF PI-COMPANY-ID = 'FGL'                                     
01622          MOVE 'CGL'              TO  WS-CHECK-TRANSID-2.          
01623                                                                   
01624      IF PI-COMPANY-ID = 'LAP'                                     
01625          MOVE 'RMC'              TO  WS-CHECK-TRANSID-2.          
01626                                                                   
01627      IF PI-COMPANY-ID = 'TII'                                     
01628          MOVE 'TIH'              TO  WS-CHECK-TRANSID-2.          
01629                                                                   
01630      IF PI-COMPANY-ID = 'CVL'  OR  'CNL'                          
01631          MOVE 'MNL'              TO  WS-CHECK-TRANSID-2.          
01632                                                                   
01633      IF PI-COMPANY-ID = 'AUK'                                     
01634          MOVE 'AIG'              TO  WS-CHECK-TRANSID-2.          
01635                                                                   
01636      IF PI-COMPANY-ID = 'BPI'                                     
01637          MOVE 'UCL'              TO  WS-CHECK-TRANSID-2.          
01638                                                                   
01639      IF PI-COMPANY-ID = 'ITY' OR 'FLC' OR 'FRO' OR 'FRN'          
01640                               OR 'FRT' OR 'FRH' OR 'FRS'          
01641                               OR 'OFI' OR 'OFJ' OR 'CAB'          
01642                               OR 'AFL' OR 'AFC' OR 'SRL'          
01643                               OR 'RIC'                            
01644          MOVE 'ITY'              TO  WS-CHECK-TRANSID-2.          
01645                                                                   
01646 *    IF PI-COMPANY-ID = 'CSL'                                     
01647 *        MOVE 'E'                TO  WS-CHECK-TRANSID-1           
01648 *        MOVE 'X47'              TO  WS-CHECK-TRANSID-2.          
01649                                                                   
01650      IF PI-PRINTER-STARTED-SW = ZERO                              
01651         MOVE +1                     TO  PI-PRINTER-STARTED-SW     
01652         MOVE EIBTRMID               TO  PI-TSK-TERM-ID            
01653         MOVE EIBTIME                TO  PI-TSK-TIME               
01654         IF PI-COMPANY-ID = 'JAL' OR 'JAI'                         
01655            NEXT SENTENCE                                          
01656         ELSE                                                      
030612        IF PI-COMPANY-ID = 'DMD' OR 'CID' OR 'DCC' OR 'AHL'                       
01658              EXEC CICS START                                      
CIDMOD                 TRANSID ('EX47')                                 
01660                  FROM    (PROGRAM-INTERFACE-BLOCK)                
01661                  LENGTH  (PI-COMM-LENGTH)                         
PEMTMP*                TERMID  ('A199')
01662              END-EXEC                                             
01663         ELSE                                                      
01664            EXEC CICS START                                        
01665                 TRANSID (WS-CHECK-WRITER-TRANS-ID)                
01666                 FROM    (PROGRAM-INTERFACE-BLOCK)                 
01667                 LENGTH  (PI-COMM-LENGTH)                          
01668                 TERMID  (PI-CHECK-PRINTER-ID)                     
01669            END-EXEC.                                              
01670                                                                   
01671      SET PI-INDEX TO +1.                                          
01672                                                                   
01673      MOVE LOW-VALUES             TO  WS-CHECK-AIX-KEY.            
01674      MOVE PI-COMPANY-CD          TO  WS-CQK-COMPANY-CD-A1.        
01675                                                                   
01676      IF PI-COMPANY-ID NOT = 'DMD'                                 
01677          MOVE +5                 TO  WS-KEY-LENGTH                
01678      ELSE                                                         
01679          MOVE +1                 TO  WS-KEY-LENGTH.               
01680                                                                   
01681  0310-MAIN-LOGIC.                                                 
01682      MOVE LOW-VALUES             TO  WS-LAST-CHECK-QUEUE-KEY.     
01683      MOVE 'CHKQ'                 TO  FILE-SWITCH.                 
01684                                                                   
01685      IF AOPTIONI = '1'                                            
01686          EXEC CICS STARTBR                                        
01687              DATASET (WS-CHECK-QUEUE-AIX-DSID)                    
01688              RIDFLD  (WS-CHECK-AIX-KEY)                           
01689              GTEQ                                                 
01690          END-EXEC                                                 
01691      ELSE                                                         
01692          IF PI-COMPANY-ID NOT = 'DMD'                             
01693              MOVE PI-CONTROL-GROUP (PI-INDEX)                     
01694                                  TO WS-CQK-CONTROL-NUMBER-A1      
01695              ADD +1  PI-HIGH-SEQUENCE (PI-INDEX)                  
01696                                   GIVING WS-SEQUENCE-NUMBER       
01697              MOVE ZERO               TO  WS-CQK-SEQUENCE-NUMBER-A1
01698              END-IF                                               
01699              IF WS-CHECK-QUEUE-BROWSE-SW = ZERO                   
01700                  EXEC CICS STARTBR                                
01701                      DATASET   (WS-CHECK-QUEUE-AIX-DSID)          
01702                      RIDFLD    (WS-CHECK-AIX-KEY)                 
01703                      KEYLENGTH (WS-KEY-LENGTH)                    
01704                      GENERIC   EQUAL                              
01705                  END-EXEC                                         
01706                  MOVE +1         TO  WS-CHECK-QUEUE-BROWSE-SW     
01707              ELSE                                                 
01708                  EXEC CICS RESETBR                                
01709                      DATASET   (WS-CHECK-QUEUE-AIX-DSID)          
01710                      RIDFLD    (WS-CHECK-AIX-KEY)                 
01711                      KEYLENGTH (WS-KEY-LENGTH)                    
01712                      GENERIC   EQUAL                              
01713                  END-EXEC.                                        
01714                                                                   
01715      EJECT                                                        
01716  0320-MAIN-LOGIC.                                                 
01717      EXEC CICS HANDLE CONDITION                                   
01718          NOTFND                                                   
01719          ENDFILE (0390-MAIN-LOGIC)                                
01720      END-EXEC.                                                    
01721                                                                   
01722  0325-MAIN-LOGIC.                                                 
01723      EXEC CICS READNEXT                                           
01724          DATASET (WS-CHECK-QUEUE-AIX-DSID)                        
01725          RIDFLD  (WS-CHECK-AIX-KEY)                               
01726          SET     (ADDRESS OF CHECK-QUE)                           
01727      END-EXEC.                                                    
01728                                                                   
01729      IF CQ-PRE-NUMBERING-SW NOT = '1'                             
01730          IF WS-CHECK-AIX-KEY NOT GREATER WS-LAST-CHECK-QUEUE-KEY  
01731              GO TO 0325-MAIN-LOGIC.                               
01732                                                                   
01733      IF CQ-PRE-NUMBERING-SW NOT = '1'                             
01734          MOVE WS-CHECK-AIX-KEY   TO  WS-LAST-CHECK-QUEUE-KEY.     
01735                                                                   
01736      IF CQ-COMPANY-CD NOT = PI-COMPANY-CD                         
01737          GO TO 0390-MAIN-LOGIC.                                   
01738                                                                   
01739      IF PI-COMPANY-ID NOT = 'DMD'                                 
01740          IF AOPTIONI = '2' OR '3'                                 
01741              IF CQ-CONTROL-NUMBER = PI-CONTROL-GROUP (PI-INDEX)   
01742                  IF CQ-SEQUENCE-NUMBER NOT GREATER THAN           
01743                                       PI-HIGH-SEQUENCE (PI-INDEX) 
01744                      GO TO 0325-CONT-LOGIC                        
01745                  ELSE                                             
01746                      GO TO 0325-MAIN-LOGIC                        
01747              ELSE                                                 
01748                  GO TO 0390-MAIN-LOGIC                            
01749          ELSE                                                     
01750              GO TO 0325-CONT-LOGIC.                               
01751                                                                   
01752      IF AOPTIONI NOT = '2' AND '3'                                
01753          GO TO 0325-CONT-LOGIC.                                   
01754                                                                   
01755      IF CQ-CONTROL-NUMBER = PI-CONTROL-GROUP (1)                  
01756          IF CQ-SEQUENCE-NUMBER NOT GREATER THAN                   
01757                                    PI-HIGH-SEQUENCE (1)           
01758              GO TO 0325-CONT-LOGIC                                
01759          ELSE                                                     
01760              GO TO 0325-MAIN-LOGIC                                
01761      ELSE                                                         
01762          IF CQ-CONTROL-NUMBER = PI-CONTROL-GROUP (2)              
01763              IF CQ-SEQUENCE-NUMBER NOT GREATER THAN               
01764                                        PI-HIGH-SEQUENCE (2)       
01765                  GO TO 0325-CONT-LOGIC                            
01766              ELSE                                                 
01767                  GO TO 0325-MAIN-LOGIC                            
01768          ELSE                                                     
01769              IF CQ-CONTROL-NUMBER = PI-CONTROL-GROUP (3)          
01770                  IF CQ-SEQUENCE-NUMBER NOT GREATER THAN           
01771                                        PI-HIGH-SEQUENCE (3)       
01772                      GO TO 0325-CONT-LOGIC                        
01773                  ELSE                                             
01774                      GO TO 0325-MAIN-LOGIC                        
01775              ELSE                                                 
01776                  IF CQ-CONTROL-NUMBER = PI-CONTROL-GROUP (4)      
01777                      IF CQ-SEQUENCE-NUMBER NOT GREATER THAN       
01778                                        PI-HIGH-SEQUENCE (4)       
01779                          GO TO 0325-CONT-LOGIC                    
01780                      ELSE                                         
01781                          GO TO 0325-MAIN-LOGIC                    
01782                  ELSE                                             
01783                      GO TO 0325-MAIN-LOGIC.                       
01784                                                                   
01785  0325-CONT-LOGIC.                                                 
01786      IF CQ-ENTRY-TYPE NOT = 'Q'                                   
01787          GO TO 0325-MAIN-LOGIC.                                   
01788                                                                   
01789      IF (AOPTIONI NOT = '3' AND                                   
01790          CQ-TIMES-PRINTED GREATER THAN ZERO)                      
01791        OR                                                         
01792         (AOPTIONI = '3' AND                                       
01793          CQ-TIMES-PRINTED NOT GREATER THAN ZERO)                  
01794              GO TO 0325-MAIN-LOGIC.                               
01795                                                                   
01796      IF AOPTIONI = '1'                                            
01797         IF NOT PI-NO-CARRIER-SECURITY                             
01798            IF PI-CARRIER-SECURITY NOT = CQ-CARRIER                
01799               GO TO 0325-MAIN-LOGIC.                              
01800                                                                   
01801      MOVE CQ-CONTROL-PRIMARY         TO  WS-CHECK-QUEUE-KEY.      
01802                                                                   
01803      EXEC CICS ENDBR                                              
01804          DATASET (WS-CHECK-QUEUE-AIX-DSID)                        
01805      END-EXEC.                                                    
01806                                                                   
01807      EXEC CICS READ UPDATE                                        
01808          DATASET (WS-CHECK-QUEUE-DSID)                            
01809          RIDFLD  (WS-CHECK-QUEUE-KEY)                             
01810          SET     (ADDRESS OF CHECK-QUE)                           
01811      END-EXEC.                                                    
01812                                                                   
01813      MOVE CHECK-QUE              TO  WS-OLD-CHECK-QUEUE-RECORD.   
01814                                                                   
01815      IF AACNI = 'Y'                                               
01816          MOVE WS-CHECK-NUMBER-X  TO  CQ-CHECK-NUMBER              
01817          ADD +1  TO  WS-CHECK-NUMBER                              
01818          MOVE '1'                TO  CQ-PRE-NUMBERING-SW.         
01819                                                                   
01820 *    NOTE ******************************************************* 
01821 *         *                PRINT THE CHECK                      * 
01822 *         *******************************************************.
01823                                                                   
01824      MOVE PI-COMPANY-CD          TO  WS-CK-COMPANY-CD             
01825                                      WS-CM-COMPANY-CD.            
01826      MOVE CQ-CARRIER             TO  WS-CK-CARRIER                
01827                                      CPA-CARRIER.                 
01828      MOVE CQ-CLAIM-NO            TO  WS-CK-CLAIM-NO               
01829                                      CPA-CLAIM-NO.                
01830      MOVE CQ-CERT-NO             TO  WS-CK-CERT-NO                
01831                                      CPA-CERT-NO.                 
01832      MOVE 'MSTR'                 TO  FILE-SWITCH.                 
01833                                                                   
01834      IF CQ-CARRIER NOT = WS-LAST-CARRIER                          
01835          PERFORM 1000-GET-CARRIER-NAME.                           
01836                                                                   
01837 *    NOTE ******************************************************* 
01838 *         *            READ THE CLAIM MASTER RECORD             * 
01839 *         *******************************************************.
01840                                                                   
01841      EXEC CICS READ                                               
01842          DATASET (WS-CLAIM-MASTER-DSID)                           
01843          RIDFLD  (WS-CLAIM-MASTER-KEY)                            
01844          SET     (ADDRESS OF CLAIM-MASTER)                        
01845      END-EXEC.                                                    
01846                                                                   
01847 *    NOTE ******************************************************* 
01848 *         *       READ THE CERTIFICATE MASTER RECORD            * 
01849 *         *******************************************************.
01850                                                                   
01851      IF CL-SYSTEM-IDENTIFIER = 'CV'                               
01852          GO TO 0325-READ-EMPLCY.                                  
01853                                                                   
01854      MOVE PI-COMPANY-CD          TO  WS-CM-COMPANY-CD.            
01855      MOVE CL-CERT-CARRIER        TO  WS-CM-CARRIER.               
01856      MOVE CL-CERT-GROUPING       TO  WS-CM-GROUPING.              
01857      MOVE CL-CERT-STATE          TO  WS-CM-STATE.                 
01858      MOVE CL-CERT-ACCOUNT        TO  WS-CM-ACCOUNT.               
01859      MOVE CL-CERT-EFF-DT         TO  WS-CM-CERT-EFF-DT.           
01860      MOVE CL-CERT-NO             TO  WS-CM-CERT-NO.               
01861      MOVE 'CERT'                 TO  FILE-SWITCH.                 
01862                                                                   
01863      EXEC CICS READ                                               
01864          DATASET   (WS-CERTIFICATE-MASTER-DSID)                   
01865          RIDFLD    (WS-CERTIFICATE-MASTER-KEY)                    
01866          SET       (ADDRESS OF CERTIFICATE-MASTER)                
01867      END-EXEC.                                                    
01868                                                                   
01869      GO TO 0328-MAIN-LOGIC.                                       
01870                                                                   
01871 *    NOTE ******************************************************* 
01872 *         *      READ THE CONVENIENCE POLICY MASTER RECORD      * 
01873 *         *******************************************************.
01874                                                                   
01875  0325-READ-EMPLCY.                                                
01876                                                                   
01877      MOVE PI-COMPANY-CD          TO  WS-PM-COMPANY-CD.            
01878      MOVE CL-CERT-CARRIER        TO  WS-PM-CARRIER.               
01879      MOVE CL-CERT-GROUPING       TO  WS-PM-GROUPING.              
01880      MOVE CL-CERT-STATE          TO  WS-PM-STATE.                 
01881      MOVE CL-CERT-ACCOUNT        TO  WS-PM-PRODUCER.              
01882      MOVE CL-CERT-EFF-DT         TO  WS-PM-EFF-DT.                
01883      MOVE CL-CV-REFERENCE-NO     TO  WS-PM-REFERENCE-NO.          
01884      MOVE 'PLCY'                 TO  FILE-SWITCH.                 
01885                                                                   
01886      EXEC CICS READ                                               
01887          DATASET   (WS-POLICY-MASTER-DSID)                        
01888          RIDFLD    (WS-POLICY-MASTER-KEY)                         
01889          SET       (ADDRESS OF POLICY-MASTER)                     
01890      END-EXEC.                                                    
01891                                                                   
01892      EJECT                                                        
01893  0326-READ-EMPROD.                                                
01894                                                                   
01895 *    NOTE ******************************************************* 
01896 *         *     READ THE CONVENIENCE PRODUCER MASTER RECORD     * 
01897 *         *******************************************************.
01898                                                                   
01899      MOVE PI-COMPANY-CD          TO  WS-PD-COMPANY-CD.            
01900      MOVE CL-CERT-CARRIER        TO  WS-PD-CARRIER.               
01901      MOVE CL-CERT-GROUPING       TO  WS-PD-GROUPING.              
01902      MOVE CL-CERT-STATE          TO  WS-PD-STATE.                 
01903      MOVE CL-CERT-ACCOUNT        TO  WS-PD-PRODUCER.              
01904      MOVE CL-CERT-EFF-DT         TO  WS-PD-EXP-DT.                
01905      MOVE SPACES                 TO  WS-PRODUCER-HOLD-RECORD.     
01906      MOVE 'PROD'                 TO  FILE-SWITCH.                 
01907                                                                   
01908  0326-STARTBR-EMPROD.                                             
01909                                                                   
01910      EXEC CICS HANDLE CONDITION                                   
01911          NOTFND   (0326-NOTFND-EMPROD)                            
01912      END-EXEC.                                                    
01913                                                                   
01914      EXEC CICS STARTBR                                            
01915          DATASET   (WS-PRODUCER-MASTER-DSID)                      
01916          RIDFLD    (WS-PRODUCER-MASTER-KEY)                       
01917          GTEQ                                                     
01918      END-EXEC.                                                    
01919                                                                   
01920      IF WS-EMPROD-GETMAIN-SW = 'Y'                                
01921          MOVE 'N'                TO  WS-EMPROD-GETMAIN-SW         
01922          EXEC CICS GETMAIN                                        
01923              SET       (ADDRESS OF PRODUCER-MASTER)               
01924              LENGTH    (2000)                                     
01925              INITIMG   (WS-SPACES)                                
01926          END-EXEC.                                                
01927                                                                   
01928  0326-READNEXT-EMPROD.                                            
01929                                                                   
01930      EXEC CICS READNEXT                                           
01931          DATASET   (WS-PRODUCER-MASTER-DSID)                      
01932          RIDFLD    (WS-PRODUCER-MASTER-KEY)                       
01933          INTO      (PRODUCER-MASTER)                              
01934      END-EXEC.                                                    
01935                                                                   
01936      IF WS-PD-COMPANY-CD NOT = PI-COMPANY-CD     OR               
01937         WS-PD-CARRIER    NOT = CL-CERT-CARRIER   OR               
01938         WS-PD-GROUPING   NOT = CL-CERT-GROUPING  OR               
01939         WS-PD-STATE      NOT = CL-CERT-STATE     OR               
01940         WS-PD-PRODUCER   NOT = CL-CERT-ACCOUNT                    
01941          IF WS-PRODUCER-HOLD-RECORD = SPACES                      
01942              GO TO 0326-NOTFND-EMPROD                             
01943          ELSE                                                     
01944              MOVE WS-PRODUCER-HOLD-RECORD    TO  PRODUCER-MASTER  
01945              GO TO 0326-ENDBR-EMPROD.                             
01946                                                                   
01947      IF WS-PD-EXP-DT = HIGH-VALUES                                
01948          GO TO 0326-ENDBR-EMPROD                                  
01949      ELSE                                                         
01950          MOVE PRODUCER-MASTER    TO  WS-PRODUCER-HOLD-RECORD.     
01951                                                                   
01952      GO TO 0326-READNEXT-EMPROD.                                  
01953                                                                   
01954  0326-NOTFND-EMPROD.                                              
01955                                                                   
01956      MOVE SPACES                 TO  PRODUCER-MASTER.             
01957                                                                   
01958      MOVE ZEROS                  TO  PD-ZIP                       
01959                                      PD-TEL-NO                    
01960                                      PD-TOL-CLM.                  
01961                                                                   
01962      MOVE 'PRODUCER NOT FOUND'   TO  PD-NAME.                     
01963                                                                   
01964  0326-ENDBR-EMPROD.                                               
01965                                                                   
01966      EXEC CICS ENDBR                                              
01967          DATASET   (WS-PRODUCER-MASTER-DSID)                      
01968      END-EXEC.                                                    
01969                                                                   
01970      MOVE SPACES                 TO  CHECK-PASS-AREA.             
01971                                                                   
01972      GO TO 0354-UPDATE-ACTIVITY-TRLRS.                            
01973                                                                   
01974      EJECT                                                        
01975  0328-MAIN-LOGIC.                                                 
01976                                                                   
01977      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                            
01978          NEXT SENTENCE                                            
01979      ELSE                                                         
01980          GO TO 0329-MAIN-LOGIC.                                   
01981                                                                   
01982 *    NOTE ******************************************************* 
01983 *         *     READ THE BENEFICIARY MASTER RECORD              * 
01984 *         *     1ST READ IS TO GET THE CREDITOR'S NAME          * 
01985 *         *******************************************************.
01986                                                                   
01987      MOVE CL-CURRENT-GROUPING        TO  WS-GROUP.                
01988      MOVE WS-GRP-1-3                 TO  WS-BK-BENEFICIARY.       
01989      MOVE 'BENE'                     TO  FILE-SWITCH.             
01990      PERFORM 2000-GET-BENEFICIARY.                                
01991                                                                   
01992      MOVE BE-MAIL-TO-NAME            TO  WS-AIG-CREDITOR-NAME.    
01993                                                                   
01994 *    NOTE ******************************************************* 
01995 *         *     READ THE BENEFICIARY MASTER RECORD              * 
01996 *         *     2ND READ IS TO GET THE INSURANCE CO'S NAME      * 
01997 *         *******************************************************.
01998                                                                   
01999      MOVE CL-BENEFICIARY             TO  WS-BK-BENEFICIARY.       
02000      PERFORM 2000-GET-BENEFICIARY.                                
02001                                                                   
02002      MOVE BE-MAIL-TO-NAME            TO  WS-AIG-INS-CO-NAME.      
02003                                                                   
02004  0329-MAIN-LOGIC.                                                 
02005                                                                   
02006 *    NOTE ******************************************************* 
02007 *         *          READ THE ACCOUNT MASTER RECORD             * 
02008 *         *******************************************************.
02009                                                                   
02010      MOVE PI-COMPANY-CD          TO  WS-AK-COMPANY-CD.            
02011                                                                   
02012      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                            
02013          MOVE CL-CURRENT-CARRIER     TO  WS-AK-CARRIER            
02014          MOVE CL-CURRENT-GROUPING    TO  WS-AK-GROUPING           
02015          MOVE CL-CURRENT-STATE       TO  WS-AK-STATE              
02016          MOVE CL-CURRENT-ACCOUNT     TO  WS-AK-ACCOUNT            
02017          MOVE CL-CERT-EFF-DT         TO  WS-AK-EXPIRATION-DT      
02018      ELSE                                                         
02019          MOVE CL-CERT-CARRIER        TO  WS-AK-CARRIER            
02020          MOVE CL-CERT-GROUPING       TO  WS-AK-GROUPING           
02021          MOVE CL-CERT-STATE          TO  WS-AK-STATE              
02022          MOVE CL-CERT-ACCOUNT        TO  WS-AK-ACCOUNT            
02023          MOVE CL-CERT-EFF-DT         TO  WS-AK-EXPIRATION-DT.     
02024                                                                   
02025      MOVE 'ACCT'                     TO  FILE-SWITCH.             
02026      MOVE SPACES                     TO  WS-ACCOUNT-HOLD-RECORD.  
02027                                                                   
02028      EXEC CICS HANDLE CONDITION                                   
02029          ENDFILE (0340-MAIN-LOGIC)                                
02030          NOTFND  (0340-MAIN-LOGIC)                                
02031      END-EXEC.                                                    
02032                                                                   
02033      EXEC CICS STARTBR                                            
02034          DATASET (WS-ACCOUNT-MASTER-DSID)                         
02035          RIDFLD  (WS-ACCOUNT-MASTER-KEY)                          
02036          GTEQ                                                     
02037      END-EXEC.                                                    
02038                                                                   
02039      IF WS-ERACCT-GETMAIN-SW = 'Y'                                
02040          MOVE 'N'                TO  WS-ERACCT-GETMAIN-SW         
02041          EXEC CICS GETMAIN                                        
02042              SET     (ADDRESS OF ACCOUNT-MASTER)                  
02043              LENGTH  (2000)                                       
02044              INITIMG (WS-SPACES)                                  
02045          END-EXEC.                                                
02046                                                                   
02047  0330-MAIN-LOGIC.                                                 
02048      EXEC CICS READNEXT                                           
02049          DATASET (WS-ACCOUNT-MASTER-DSID)                         
02050          RIDFLD  (WS-ACCOUNT-MASTER-KEY)                          
02051          INTO    (ACCOUNT-MASTER)                                 
02052      END-EXEC.                                                    
02053                                                                   
02054      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                            
02055          IF WS-AK-COMPANY-CD NOT = PI-COMPANY-CD        OR        
02056             WS-AK-CARRIER    NOT = CL-CURRENT-CARRIER   OR        
02057             WS-AK-GROUPING   NOT = CL-CURRENT-GROUPING  OR        
02058             WS-AK-STATE      NOT = CL-CURRENT-STATE     OR        
02059             WS-AK-ACCOUNT    NOT = CL-CURRENT-ACCOUNT             
02060              IF WS-ACCOUNT-HOLD-RECORD = SPACES                   
02061                  GO TO 0340-MAIN-LOGIC                            
02062              ELSE                                                 
02063                  MOVE WS-ACCOUNT-HOLD-RECORD TO  ACCOUNT-MASTER   
02064                  GO TO 0350-MAIN-LOGIC                            
02065          ELSE                                                     
02066              NEXT SENTENCE                                        
02067      ELSE                                                         
02068          IF WS-AK-COMPANY-CD NOT = PI-COMPANY-CD     OR           
02069             WS-AK-CARRIER    NOT = CL-CERT-CARRIER   OR           
02070             WS-AK-GROUPING   NOT = CL-CERT-GROUPING  OR           
02071             WS-AK-STATE      NOT = CL-CERT-STATE     OR           
02072             WS-AK-ACCOUNT    NOT = CL-CERT-ACCOUNT                
02073             IF WS-ACCOUNT-HOLD-RECORD = SPACES                    
02074                GO TO 0340-MAIN-LOGIC                              
02075            ELSE                                                   
02076                MOVE WS-ACCOUNT-HOLD-RECORD TO ACCOUNT-MASTER      
02077                GO TO 0350-MAIN-LOGIC.                             
02078                                                                   
02079      IF WS-AK-EXPIRATION-DT = HIGH-VALUES                         
02080          GO TO 0350-MAIN-LOGIC                                    
02081      ELSE                                                         
02082          MOVE ACCOUNT-MASTER     TO  WS-ACCOUNT-HOLD-RECORD.      
02083                                                                   
02084      GO TO 0330-MAIN-LOGIC.                                       
02085                                                                   
02086  0340-MAIN-LOGIC.                                                 
02087      MOVE SPACES                 TO  ACCOUNT-MASTER.              
02088                                                                   
02089      MOVE ZERO                   TO  AM-ZIP                       
02090                                      AM-TEL-NO                    
02091                                      AM-TOL-PREM                  
02092                                      AM-TOL-REF                   
02093                                      AM-TOL-CLM.                  
02094                                                                   
02095      MOVE 'ACCOUNT NOT FOUND'    TO  AM-NAME.                     
02096                                                                   
02097  0350-MAIN-LOGIC.                                                 
02098      EXEC CICS ENDBR                                              
02099          DATASET (WS-ACCOUNT-MASTER-DSID)                         
02100      END-EXEC.                                                    
02101                                                                   
02102      MOVE SPACES                 TO  CHECK-PASS-AREA.             
02103                                                                   
02104      IF AM-3RD-PARTY-NOTIF-LEVEL NOT NUMERIC                      
02105         MOVE ZEROS               TO  AM-3RD-PARTY-NOTIF-LEVEL.    
02106                                                                   
02107      IF AM-3RD-PARTY-NOTIF-LEVEL = ZEROS                          
02108         GO TO 0354-UPDATE-ACTIVITY-TRLRS.                         
02109                                                                   
042704     IF PI-COMPANY-ID = 'AIG' OR 'AUK' OR 'CID' OR 'DCC'          
030612        OR 'AHL'
02111          GO TO 0354-UPDATE-ACTIVITY-TRLRS.                        
02112                                                                   
02113      EXEC CICS HANDLE CONDITION                                   
02114           NOTFND (0351-READ-COMP-MASTER)                          
02115      END-EXEC.                                                    
02116                                                                   
02117      MOVE WS-CLAIM-MASTER-KEY    TO  WS-ACTIVITY-TRAILERS-KEY.    
02118      MOVE +29                    TO  WS-ATK-SEQUENCE-NO.          
02119      MOVE 'TRLR'                 TO  FILE-SWITCH.                 
02120                                                                   
02121      EXEC CICS READ                                               
02122           DATASET  (WS-ACTIVITY-TRAILERS-DSID)                    
02123           RIDFLD   (WS-ACTIVITY-TRAILERS-KEY)                     
02124           SET      (ADDRESS OF ACTIVITY-TRAILERS)                 
02125      END-EXEC.                                                    
02126                                                                   
02127      MOVE AT-MAIL-TO-NAME    TO  CPA-NOTIFY-NAME.                 
02128      MOVE AT-ADDRESS-LINE-1  TO  CPA-NOTIFY-ADDRESS-LINE1.        
02129      MOVE AT-ADDRESS-LINE-2  TO  CPA-NOTIFY-ADDRESS-LINE2.        
02130      MOVE SPACES             TO  CPA-NOTIFY-ADDRESS-LINE3.        
02131      MOVE AT-CITY-STATE      TO  CPA-NOTIFY-CITY-STATE.           
02132      MOVE AT-ZIP             TO  CPA-NOTIFY-ZIP.                  
02133                                                                   
02134      GO TO 0354-UPDATE-ACTIVITY-TRLRS.                            
02135                                                                   
02136  0351-READ-COMP-MASTER.                                           
02137                                                                   
02138      MOVE PI-COMPANY-CD   TO WS-ERCOMP-COMPANY-CD.                
02139      MOVE AM-CARRIER      TO WS-ERCOMP-CARRIER.                   
02140      MOVE AM-GROUPING     TO WS-ERCOMP-GROUPING.                  
02141      MOVE 'A'             TO WS-ERCOMP-TYPE.                      
02142      MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)                       
02143                           TO WS-ERCOMP-RESP-NO.                   
02144      IF AM-3RD-PARTY-NOTIF-LEVEL = AM-REMIT-TO                    
02145          IF AM-COM-TYP (AM-REMIT-TO) = 'O' OR 'P' OR              
052814                                       'G' OR 'B' or 'S'          
02147              MOVE 'G'            TO  WS-ERCOMP-TYPE               
02148              MOVE LOW-VALUES     TO  WS-ERCOMP-ACCOUNT            
02149          ELSE                                                     
02150              MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)               
02151                                  TO WS-ERCOMP-ACCOUNT             
02152      ELSE                                                         
02153          MOVE 'G'                TO WS-ERCOMP-TYPE                
02154          MOVE LOW-VALUES         TO WS-ERCOMP-ACCOUNT.            
02155                                                                   
02156      IF PI-ZERO-CARRIER OR PI-ZERO-CAR-GROUP                      
02157         MOVE ZEROS TO WS-ERCOMP-CARRIER.                          
02158                                                                   
02159      IF PI-ZERO-GROUPING OR PI-ZERO-CAR-GROUP                     
02160         MOVE ZEROS TO WS-ERCOMP-GROUPING.                         
02161                                                                   
02162      MOVE 'COMP'                 TO  FILE-SWITCH.                 
02163                                                                   
02164      EXEC CICS HANDLE CONDITION                                   
02165           NOTFND    (0354-UPDATE-ACTIVITY-TRLRS)                  
02166      END-EXEC.                                                    
02167                                                                   
02168      EXEC CICS  READ                                              
02169           SET      (ADDRESS OF COMPENSATION-MASTER)               
02170           DATASET  ('ERCOMP')                                     
02171           RIDFLD   (WS-ERCOMP-KEY)                                
02172      END-EXEC.                                                    
02173                                                                   
02174      MOVE CO-ACCT-NAME          TO CPA-NOTIFY-NAME                
02175      IF CO-ACCT-NAME = SPACES                                     
02176         MOVE CO-MAIL-NAME       TO CPA-NOTIFY-NAME.               
02177                                                                   
02178      MOVE CO-ADDR-1             TO CPA-NOTIFY-ADDRESS-LINE1.      
02179      MOVE CO-ADDR-2             TO CPA-NOTIFY-ADDRESS-LINE2.      
02180      MOVE CO-ADDR-3             TO CPA-NOTIFY-CITY-STATE.         
02181      MOVE CO-ZIP                TO CPA-NOTIFY-ZIP.                
02182                                                                   
02183  0354-UPDATE-ACTIVITY-TRLRS.                                      
02184                                                                   
02185      MOVE WS-CLAIM-MASTER-KEY    TO  WS-ACTIVITY-TRAILERS-KEY.    
02186      MOVE CQ-PMT-TRLR-SEQUENCE   TO  WS-ATK-SEQUENCE-NO.          
02187      MOVE 'TRLR'                 TO  FILE-SWITCH.                 
02188                                                                   
02189      EXEC CICS READ UPDATE                                        
02190          DATASET (WS-ACTIVITY-TRAILERS-DSID)                      
02191          RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)                       
02192          SET     (ADDRESS OF ACTIVITY-TRAILERS)                   
02193      END-EXEC.                                                    
02194                                                                   
02195      MOVE ZERO                   TO  CPA-ALIGNMENT.               
02196                                                                   
02197      IF CL-SYSTEM-IDENTIFIER = 'CV'                               
02198          MOVE PM-CARRIER                 TO  CPA-CARRIER          
02199          MOVE PM-GROUPING                TO  CPA-GROUP            
02200          MOVE PM-PRODUCER                TO  CPA-ACCOUNT          
02201          MOVE PM-STATE                   TO  CPA-STATE            
02202          MOVE PM-REFERENCE-NUMBER        TO  CPA-REFERENCE-NO     
02203          MOVE PM-INS-TYPE                TO  CPA-IND-GRP-TYPE     
02204          MOVE PM-POLICY-EFF-DT           TO  CPA-CERT-EFF-DT      
02205      ELSE                                                         
02206          IF PI-COMPANY-ID = 'AIG' OR 'AUK'                        
02207              MOVE CL-CURRENT-CARRIER     TO  CPA-CARRIER          
02208              MOVE CL-CURRENT-GROUPING    TO  CPA-GROUP            
02209              MOVE CL-CURRENT-STATE       TO  CPA-STATE            
02210              MOVE CL-CURRENT-ACCOUNT     TO  CPA-ACCOUNT          
02211              MOVE CM-CERT-NO             TO  CPA-CERT-NO          
02212              MOVE CM-IND-GRP-TYPE        TO  CPA-IND-GRP-TYPE     
02213              MOVE CM-CERT-EFF-DT         TO  CPA-CERT-EFF-DT      
02214          ELSE                                                     
02215              MOVE CM-CARRIER             TO  CPA-CARRIER          
02216              MOVE CM-GROUPING            TO  CPA-GROUP            
02217              MOVE CM-ACCOUNT             TO  CPA-ACCOUNT          
02218              MOVE CM-STATE               TO  CPA-STATE            
02219              MOVE CM-CERT-NO             TO  CPA-CERT-NO          
02220              MOVE CM-IND-GRP-TYPE        TO  CPA-IND-GRP-TYPE     
02221              MOVE CM-CERT-EFF-DT         TO  CPA-CERT-EFF-DT.     
02222                                                                   
02223      MOVE CL-CLAIM-NO                    TO  CPA-CLAIM-NO.        
02224      MOVE CL-CLAIM-STATUS                TO  CPA-CLAIM-STATUS.    
02225      MOVE CL-LAST-CLOSE-REASON           TO  CPA-LAST-CLOSE-REASON
02226                                                                   
02227      PERFORM 5000-MOVE-NAME.                                      
02228      MOVE WS-NAME-WORK           TO  CPA-INSURED-NAME.            
02229                                                                   
02230      MOVE CL-CLAIM-TYPE          TO  CPA-CLAIM-TYPE.              
02231      MOVE AT-PAYMENT-TYPE        TO  CPA-PAYMENT-TYPE.            
02232      MOVE CQ-CHECK-BY-USER       TO  CPA-PAYMENT-BY.              
02233                                                                   
02234      MOVE AT-PAYMENT-NOTE-SEQ-NO TO WS-PAYMENT-NOTE-SEQ-NO.       
02235                                                                   
02236      IF PI-COMPANY-ID = 'LAP' OR 'RMC'                            
02237          MOVE AT-FORM-CTL-SEQ-NO TO CPA-FORM-CTL-SEQ-NO.          
02238                                                                   
02239      MOVE CQ-CHECK-NUMBER        TO  CPA-CHECK-NUMBER             
02240                                      AT-CHECK-NO.                 
02241                                                                   
02242      MOVE CQ-CHECK-AMOUNT        TO  CPA-AMOUNT-PAID.             
02243      MOVE CL-TOTAL-PAID-AMT      TO  CPA-AMOUNT-PAID-TO-DATE.     
02244      MOVE AT-DAYS-IN-PERIOD      TO  CPA-DAYS-PAID.               
02245      MOVE AT-DAILY-RATE          TO  CPA-DAILY-RATE.              
02246      MOVE AT-ELIMINATION-DAYS    TO  CPA-ELIMINATION-DAYS.        
02247      MOVE AT-BENEFIT-TYPE        TO  CPA-BENEFIT-TYPE.            
02248      MOVE AT-EXPENSE-TYPE        TO  CPA-EXPENSE-TYPE.            
02249      MOVE CL-NO-OF-PMTS-MADE     TO  CPA-NO-OF-PMTS-MADE.         
02250      MOVE CL-PROCESSOR-ID        TO  CPA-EXAMINER.                
02251      MOVE AT-PAYMENT-ORIGIN      TO  CPA-PAYMENT-ORIGIN.          
02252      MOVE CL-CCN-A5              TO  CPA-CREDIT-CARD-NO.          
02253                                                                   
02254      IF AT-PAYMENT-ORIGIN = '2'                                   
02255          MOVE 'Y'                TO  WS-AUTO-PAY-SW               
02256      ELSE                                                         
02257          MOVE 'N'                TO  WS-AUTO-PAY-SW.              
02258                                                                   
02259      IF (PI-COMPANY-ID = 'FIM' OR 'FMK' OR 'HER') AND             
02260         (AT-PAYMENT-ORIGIN = '2')                                 
02261          MOVE 'AUTO'             TO  CPA-EXAMINER.                
02262                                                                   
02263 *    NOTE ******************************************************* 
02264 *         *      CLAIM TYPE      MEANING                        * 
02265 *         *          1         DEATH CLAIM (INDIVIDUAL)         * 
02266 *         *          2         DISABILITY CLAIM (INDIVIDUAL)    * 
02267 *         *          3         OUTSTANDING BALANCE (DEATH)      * 
02268 *         *          4         OUTSTANDING BALANCE (DISABILITY) * 
02269 *         *******************************************************.
02270                                                                   
100518     IF CL-CLAIM-TYPE = 'L' OR 'O'
02272          IF CL-CLAIM-PREM-TYPE = '2'                              
02273              MOVE '3'            TO  CPA-CLAIM-CODE               
02274            ELSE                                                   
02275              MOVE '1'            TO  CPA-CLAIM-CODE               
02276        ELSE                                                       
02277          IF CL-CLAIM-PREM-TYPE = '2'                              
02278              MOVE '4'            TO  CPA-CLAIM-CODE               
02279            ELSE                                                   
02280              MOVE '2'            TO  CPA-CLAIM-CODE.              
02281                                                                   
02282 *    NOTE ******************************************************* 
02283 *         *      PAY CODE       MEANING                         * 
02284 *         *         A        ADDITIONAL DEATH CLAIM             * 
02285 *         *         P        PARTIAL PAYMENT                    * 
02286 *         *         F        FINAL PAYMENT                      * 
02287 *         *         S        LUMP SUM DISABILITY                * 
02288 *         *******************************************************.
02289                                                                   
02290      MOVE AT-PAYMENT-TYPE        TO  CPA-PAY-CODE                 
02291                                      WS-PAYMENT-TYPE.             
02292      INSPECT CPA-PAY-CODE CONVERTING '123456789' TO 'PFSA     '.  
02293                                                                   
02294      MOVE CL-INCURRED-DT         TO  CPA-INCURRED-DT.             
02295      MOVE CL-REPORTED-DT         TO  CPA-REPORTED-DT.             
02296                                                                   
02297      IF NOT PI-USES-PAID-TO                                       
02298         MOVE AT-PAID-THRU-DT     TO  CPA-PAID-THRU-DT             
02299      ELSE                                                         
02300         MOVE AT-PAID-THRU-DT     TO  DC-BIN-DATE-1                
02301         MOVE +1                  TO  DC-ELAPSED-DAYS              
02302         MOVE +0                  TO  DC-ELAPSED-MONTHS            
02303         MOVE '6'                 TO  DC-OPTION-CODE               
02304         PERFORM 8500-DATE-CONVERSION                              
02305         IF NO-CONVERSION-ERROR                                    
02306            MOVE DC-BIN-DATE-2    TO  CPA-PAID-THRU-DT.            
02307                                                                   
02308      MOVE AT-PAID-FROM-DT        TO  CPA-PAID-FROM-DT.            
02309      MOVE CL-LAST-PMT-DT         TO  CPA-PAID-DT.                 
02310                                                                   
02311      MOVE WS-CARRIER-ADDRESS-DATA  TO  CPA-CARRIER-ADDRESS-DATA.  
02312                                                                   
02313      IF CL-SYSTEM-IDENTIFIER = 'CV'                               
02314          GO TO 0354-PROCESS-CV-CLAIM.                             
02315                                                                   
02316      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                            
02317          MOVE WS-AIG-INS-CO-NAME TO  CPA-COMPANY-NAME.            
02318                                                                   
02319      MOVE AM-NAME                TO  CPA-ACCOUNT-NAME.            
02320      MOVE AM-PERSON              TO  CPA-ACCOUNT-IN-CARE-OF.      
02321      MOVE AM-ADDRS               TO  CPA-ACCOUNT-ADDRESS-LINE1.   
02322      MOVE SPACES                 TO  CPA-ACCOUNT-ADDRESS-LINE2.   
02323      MOVE AM-CITY                TO  CPA-ACCOUNT-CITY-ST.         
02324      MOVE AM-ZIP                 TO  CPA-ACCOUNT-ZIP-CODE.        
02325      MOVE AM-TEL-NO              TO  WS-WORK-PHONE.               
02326      INSPECT WS-WORK-PHONE CONVERTING SPACES TO '0'.              
02327      MOVE WS-NUMERIC-PHONE       TO  CPA-ACCOUNT-PHONE-NO.        
02328                                                                   
02329      IF CM-SSN-STATE   = CM-STATE AND                             
02330         CM-SSN-ACCOUNT = CM-ACCOUNT-PRIME                         
02331          MOVE SPACES             TO  CPA-SOC-SEC-NO               
02332      ELSE                                                         
02333          MOVE CM-SOC-SEC-NO      TO  CPA-SOC-SEC-NO.              
02334                                                                   
02335      MOVE CM-STATE               TO  WS-SSN-STATE                 
02336                                      WS-MEMBER-STATE.             
02337      MOVE CM-ACCOUNT             TO  WS-SSN-ACCOUNT               
02338                                      WS-MEMBER-ACCOUNT.           
02339      MOVE CM-INSURED-LAST-NAME   TO  WS-SSN-LN3                   
02340                                      WS-MEMBER-LN4.               
02341                                                                   
02342      IF CM-MEMB-STATE   = CM-STATE AND                            
02343         CM-MEMB-ACCOUNT = CM-ACCOUNT-PRIME                        
02344          MOVE SPACES             TO  CPA-MEMBER-NUMBER            
02345      ELSE                                                         
02346          MOVE CM-MEMBER-NO       TO  CPA-MEMBER-NUMBER.           
02347                                                                   
02348      IF PI-COMPANY-ID = 'DMD'                                     
02349          MOVE CM-POLICY-FORM-NO  TO  CPA-MEMBER-NUMBER.           
02350                                                                   
CIDMOD*    MOVE CM-LOAN-NUMBER         TO  CPA-LOAN-NUMBER              
CIDMOD*    MOVE CM-BENEFICIARY         TO  CPA-BENEFICIARY              
02353                                                                   
02354      GO TO 0354-CONT.                                             
02355                                                                   
02356  0354-PROCESS-CV-CLAIM.                                           
02357                                                                   
02358      MOVE PD-NAME               TO  CPA-ACCOUNT-NAME.             
02359      MOVE PD-PERSON             TO  CPA-ACCOUNT-IN-CARE-OF.       
02360      MOVE PD-ADDRS              TO  CPA-ACCOUNT-ADDRESS-LINE1.    
02361      MOVE SPACES                TO  CPA-ACCOUNT-ADDRESS-LINE2.    
02362      MOVE PD-CITY               TO  CPA-ACCOUNT-CITY-ST.          
02363      MOVE PD-ZIP                TO  CPA-ACCOUNT-ZIP-CODE.         
02364      MOVE PD-TEL-NO             TO  WS-WORK-PHONE.                
02365      INSPECT WS-WORK-PHONE CONVERTING SPACES TO '0'.              
02366      MOVE WS-NUMERIC-PHONE      TO  CPA-ACCOUNT-PHONE-NO.         
02367                                                                   
02368      IF PM-SSN-STATE    = PM-STATE AND                            
02369         PM-SSN-PRODUCER = PM-PRODUCER-PRIME                       
02370          MOVE SPACES             TO  CPA-SOC-SEC-NO               
02371      ELSE                                                         
02372          MOVE PM-SOC-SEC-NO      TO  CPA-SOC-SEC-NO.              
02373                                                                   
02374      MOVE PM-LOAN-NUMBER         TO  CPA-CV-LOAN-NUMBER.          
02375                                                                   
02376  0354-CONT.                                                       
02377                                                                   
02378      MOVE AT-PAYEE-TYPE-CD       TO  WS-PAYEE-CODE                
02379                                      CPA-PAYEE-TYPE-CD.           
02380                                                                   
02381      MOVE WS-CHECK-WRITER-DATE   TO  AT-CHECK-WRITTEN-DT          
02382                                      CQ-CHECK-WRITTEN-DT          
02383                                      CPA-CHECK-DATE.              
02384                                                                   
02385      IF AOPTIONI NOT = '3'                                        
02386          MOVE PI-MONTH-END-SAVE  TO  AT-PMT-SELECT-DT.            
02387                                                                   
02388      IF AOPTIONI = '3'                                            
02389        AND CQ-PRE-NUMBERING-SW = '1'                              
02390          MOVE WS-SEQUENCE-NUMBER TO  AT-CHECK-QUE-SEQUENCE.       
02391                                                                   
02392      MOVE +1760                  TO  CQ-LAST-UPDATED-BY.          
02393      MOVE EIBTIME                TO  CQ-LAST-UPDATED-HHMMSS.      
02394      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             
02395      MOVE '5'                    TO  DC-OPTION-CODE.              
02396      PERFORM 8500-DATE-CONVERSION.                                
02397      MOVE DC-BIN-DATE-1          TO  CQ-LAST-UPDATED-DT.          
02398                                                                   
02399      EXEC CICS REWRITE                                            
02400          DATASET (WS-ACTIVITY-TRAILERS-DSID)                      
02401          FROM    (ACTIVITY-TRAILERS)                              
02402      END-EXEC.                                                    
02403                                                                   
02404      IF WS-PAYMENT-NOTE-SEQ-NO GREATER THAN +0 AND                
02405         WS-PAYMENT-NOTE-SEQ-NO LESS THAN +4096                    
02406         MOVE WS-CLAIM-MASTER-KEY    TO  WS-ACTIVITY-TRAILERS-KEY  
02407         MOVE WS-PAYMENT-NOTE-SEQ-NO TO  WS-ATK-SEQUENCE-NO        
02408         MOVE 'TRLR'                 TO  FILE-SWITCH               
02409         EXEC CICS HANDLE CONDITION                                
02410              NOTFND  (0354-CHECK-FOR-AUTO-PAY)                    
02411         END-EXEC                                                  
02412         EXEC CICS READ                                            
02413              DATASET (WS-ACTIVITY-TRAILERS-DSID)                  
02414              RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)                   
02415              SET     (ADDRESS OF ACTIVITY-TRAILERS)               
02416         END-EXEC                                                  
02417         IF AT-TRAILER-TYPE = '6'                                  
02418            MOVE AT-INFO-LINE-1 TO CPA-COMMENT                     
02419            MOVE AT-INFO-LINE-2 TO CPA-COMMENT-2.                  
02420                                                                   
02421  0354-CHECK-FOR-AUTO-PAY.                                         
02422                                                                   
02423      IF WS-AUTO-PAY-SW = 'N'                                      
02424          GO TO 0355-GET-ADDRESS.                                  
02425                                                                   
02426      EXEC CICS HANDLE CONDITION                                   
02427          NOTFND   (0355-GET-ADDRESS)                              
02428      END-EXEC.                                                    
02429                                                                   
02430      MOVE WS-CLAIM-MASTER-KEY        TO  WS-ACTIVITY-TRAILERS-KEY.
02431      MOVE CL-AUTO-PAY-SEQ            TO  WS-ATK-SEQUENCE-NO.      
02432      MOVE 'TRLR'                     TO  FILE-SWITCH.             
02433                                                                   
02434      EXEC CICS READ                                               
02435          DATASET   (WS-ACTIVITY-TRAILERS-DSID)                    
02436          RIDFLD    (WS-ACTIVITY-TRAILERS-KEY)                     
02437          SET       (ADDRESS OF ACTIVITY-TRAILERS)                 
02438      END-EXEC.                                                    
02439                                                                   
02440      IF AT-TRAILER-TYPE = '3'                                     
02441          NEXT SENTENCE                                            
02442      ELSE                                                         
02443          GO TO 0355-GET-ADDRESS.                                  
02444                                                                   
02445      MOVE AT-SCHEDULE-END-DT         TO  CPA-AUTO-PAY-END-DT.     
02446                                                                   
02447  0355-GET-ADDRESS.                                                
02448                                                                   
02449      IF CL-ACCOUNT-ADDR-CNT NOT = ZERO                            
02450         MOVE CL-ACCOUNT-ADDR-CNT TO WS-ATK-SEQUENCE-NO            
02451         ADD +20    TO WS-ATK-SEQUENCE-NO                          
02452         PERFORM 0360-MAIN-LOGIC THRU 0360-EXIT                    
02453         MOVE CPA-PAYEE-NAME          TO  CPA-ACCOUNT-NAME         
02454         MOVE CPA-PAYEE-ADDRESS-LINE2 TO  CPA-ACCOUNT-ADDRESS-LINE1
02455         MOVE CPA-PAYEE-ADDRESS-LINE3 TO  CPA-ACCOUNT-ADDRESS-LINE2
02456         MOVE CPA-PAYEE-CITY-STATE    TO  CPA-ACCOUNT-CITY-ST      
02457         MOVE CPA-PAYEE-ZIP           TO  CPA-ACCOUNT-ZIP-CODE.    
02458                                                                   
030612     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL'
CIDMOD        MOVE +91                 TO WS-ATK-SEQUENCE-NO
CIDMOD        EXEC CICS READ                                            
CIDMOD           DATASET (WS-ACTIVITY-TRAILERS-DSID)                    
CIDMOD           RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)                     
CIDMOD           SET     (ADDRESS OF ACTIVITY-TRAILERS)                 
CIDMOD           RESP    (WS-RESPONSE)
CIDMOD        END-EXEC                                                  
CIDMOD        IF WS-RESP-NORMAL
CIDMOD           MOVE AT-INFO-LINE-1   TO CPA-BENEFICIARY
CIDMOD        ELSE
CIDMOD           MOVE SPACES           TO CPA-BENEFICIARY
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD
02459      MOVE CL-INSURED-ADDR-CNT TO WS-ATK-SEQUENCE-NO.              
02460                                                                   
02461      PERFORM 0360-MAIN-LOGIC THRU 0360-EXIT.                      
02462                                                                   
02463      MOVE CPA-PAYEE-NAME           TO  CPA-INSURED-ADDR-TRLR-NAME.
02464      MOVE CPA-PAYEE-ADDRESS-LINE1  TO  CPA-INSURED-ADDRESS-LINE1. 
02465      MOVE CPA-PAYEE-ADDRESS-LINE2  TO  CPA-INSURED-ADDRESS-LINE2. 
02466      MOVE CPA-PAYEE-ADDRESS-LINE3  TO  CPA-INSURED-ADDRESS-LINE3. 
02467      MOVE CPA-PAYEE-CITY-STATE     TO  CPA-INSURED-CITY-STATE.    
02468      MOVE CPA-PAYEE-ZIP            TO  CPA-INSURED-ZIP.           
02469                                                                   
02470      IF WS-PAYEE-CD = 'I'                                         
02471         GO TO 0370-MAIN-LOGIC.                                    
02472                                                                   
02473      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                            
02474        IF WS-PAYEE-CD = 'B'                                       
02475          IF WS-PAYEE-SEQ-NUM = 0                                  
02476            MOVE CL-BENEFICIARY           TO  WS-BK-BENEFICIARY    
02477            PERFORM 2000-GET-BENEFICIARY                           
02478            GO TO 0370-MAIN-LOGIC                                  
02479          ELSE                                                     
02480            IF WS-PAYEE-SEQ-NUM = 9                                
02481              MOVE WS-AIG-CREDITOR-NAME   TO  CPA-PAYEE-NAME       
02482              GO TO 0370-MAIN-LOGIC                                
02483            ELSE                                                   
02484              MOVE WS-PAYEE-SEQ-NUM       TO  WS-ATK-SEQUENCE-NO   
02485              ADD +10                     TO  WS-ATK-SEQUENCE-NO   
02486              GO TO 0360-MAIN-LOGIC.                               
02487                                                                   
02488      IF WS-PAYEE-CD = 'B'                                         
02489         IF (CL-BENIF-ADDR-CNT = +0) OR                            
02490            (WS-PAYEE-SEQ-NUM = 0 OR 9)                            
02491             MOVE CL-BENEFICIARY        TO  WS-BK-BENEFICIARY      
02492             PERFORM 2000-GET-BENEFICIARY                          
02493             GO TO 0370-MAIN-LOGIC                                 
02494         ELSE                                                      
02495             MOVE WS-PAYEE-SEQ-NUM   TO  WS-ATK-SEQUENCE-NO        
02496             ADD +10                 TO  WS-ATK-SEQUENCE-NO        
02497             GO TO 0360-MAIN-LOGIC.                                
02498                                                                   
02499      IF WS-PAYEE-CD = 'O'                                         
02500         MOVE WS-PAYEE-SEQ-NUM      TO  WS-ATK-SEQUENCE-NO         
02501         ADD +50                    TO  WS-ATK-SEQUENCE-NO         
02502         GO TO 0360-MAIN-LOGIC.                                    
02503                                                                   
02504      IF WS-PAYEE-CD = 'Q'                                         
02505         MOVE WS-PAYEE-SEQ-NUM      TO  WS-ATK-SEQUENCE-NO         
02506         ADD +60                    TO  WS-ATK-SEQUENCE-NO         
02507         GO TO 0360-MAIN-LOGIC.                                    
02508                                                                   
02509      IF WS-PAYEE-CD = 'P'                                         
02510         MOVE WS-PAYEE-SEQ-NUM      TO  WS-ATK-SEQUENCE-NO         
02511         ADD +30                    TO  WS-ATK-SEQUENCE-NO         
02512         GO TO 0360-MAIN-LOGIC.                                    
02513                                                                   
02514      IF WS-PAYEE-CD = 'E'                                         
02515         MOVE WS-PAYEE-SEQ-NUM      TO  WS-ATK-SEQUENCE-NO         
02516         ADD +40                    TO  WS-ATK-SEQUENCE-NO         
02517         GO TO 0360-MAIN-LOGIC.                                    
02518                                                                   
02519      IF (WS-PAYEE-CD = 'A') AND                                   
02520         (CL-ACCOUNT-ADDR-CNT NOT = +0) AND                        
02521         (WS-PAYEE-SEQ-NUM GREATER THAN 0)                         
02522         MOVE WS-PAYEE-SEQ-NUM     TO  WS-ATK-SEQUENCE-NO          
02523         ADD +20                   TO  WS-ATK-SEQUENCE-NO          
02524         GO TO 0360-MAIN-LOGIC.                                    
02525                                                                   
02526      IF CL-SYSTEM-IDENTIFIER = 'CV'                               
02527          MOVE PD-NAME           TO  CPA-PAYEE-NAME                
02528          MOVE PD-PERSON         TO  CPA-PAYEE-ADDRESS-LINE1       
02529          MOVE PD-ADDRS          TO  CPA-PAYEE-ADDRESS-LINE2       
02530          MOVE SPACES            TO  CPA-PAYEE-ADDRESS-LINE3       
02531          MOVE PD-CITY           TO  CPA-PAYEE-CITY-STATE          
02532          MOVE PD-ZIP            TO  CPA-PAYEE-ZIP                 
02533      ELSE                                                         
02534          MOVE AM-NAME           TO  CPA-PAYEE-NAME                
02535          MOVE AM-PERSON         TO  CPA-PAYEE-ADDRESS-LINE1       
02536          MOVE AM-ADDRS          TO  CPA-PAYEE-ADDRESS-LINE2       
02537          MOVE SPACES            TO  CPA-PAYEE-ADDRESS-LINE3       
02538          MOVE AM-CITY           TO  CPA-PAYEE-CITY-STATE          
02539          MOVE AM-ZIP            TO  CPA-PAYEE-ZIP.                
02540                                                                   
02541      GO TO 0370-MAIN-LOGIC.                                       
02542                                                                   
02543  0360-MAIN-LOGIC.                                                 
02544      EXEC CICS HANDLE CONDITION                                   
02545          NOTFND (0360-NO-TRLR)                                    
02546      END-EXEC.                                                    
02547                                                                   
02548      IF WS-ATK-SEQUENCE-NO = +0                                   
02549         GO TO 0360-NO-TRLR                                        
02550      ELSE                                                         
02551         EXEC CICS READ                                            
02552              DATASET (WS-ACTIVITY-TRAILERS-DSID)                  
02553              RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)                   
02554              SET     (ADDRESS OF ACTIVITY-TRAILERS)               
02555         END-EXEC                                                  
02556         MOVE AT-MAIL-TO-NAME    TO  CPA-PAYEE-NAME                
02557         MOVE AT-ADDRESS-LINE-1  TO  CPA-PAYEE-ADDRESS-LINE2       
02558         MOVE AT-ADDRESS-LINE-2  TO  CPA-PAYEE-ADDRESS-LINE3       
02559         MOVE AT-CITY-STATE      TO  CPA-PAYEE-CITY-STATE          
02560         MOVE AT-ZIP             TO  CPA-PAYEE-ZIP.                
02561                                                                   
02562      GO TO 0360-EXIT.                                             
02563                                                                   
02564  0360-NO-TRLR.                                                    
02565      MOVE WS-NAME-WORK           TO  CPA-PAYEE-NAME.              
02566      MOVE ZERO                   TO  CPA-PAYEE-ZIP.               
02567      MOVE SPACES                 TO  CPA-PAYEE-ADDRESS-LINE2      
02568                                      CPA-PAYEE-ADDRESS-LINE3      
02569                                      CPA-PAYEE-CITY-STATE.        
02570      MOVE +1                     TO  WS-COMPLETED-SUCCESSFUL.     
02571  0360-EXIT.                                                       
02572      EXIT.                                                        
02573                                                                   
02574  0370-MAIN-LOGIC.                                                 
02575      IF CL-INSURED-BIRTH-DT NOT = LOW-VALUES                      
02576          MOVE EIBDATE            TO  DC-JULIAN-YYDDD              
02577          MOVE '5'                TO  DC-OPTION-CODE               
02578          PERFORM 8500-DATE-CONVERSION                             
02579          MOVE CL-INSURED-BIRTH-DT TO DC-BIN-DATE-2                
02580          MOVE '1'                TO  DC-OPTION-CODE               
02581          PERFORM 8500-DATE-CONVERSION                             
02582          DIVIDE DC-ELAPSED-MONTHS BY +12 GIVING CPA-INSURED-AGE   
02583        ELSE                                                       
02584          MOVE ZERO               TO  CPA-INSURED-AGE.             
02585                                                                   
02586      IF CL-SYSTEM-IDENTIFIER = 'CV'                               
02587          GO TO 0372-READ-EMPLAN.                                  
02588                                                                   
052614     IF CL-CLAIM-TYPE = 'A' OR 'I' OR 'G' or 'F'
080322                            OR 'B' OR 'H'
02590          MOVE PI-COMPANY-ID      TO  WS-CFK-COMPANY-ID            
02591          MOVE '5'                TO  WS-CFK-RECORD-TYPE           
02592          MOVE CM-AH-BENEFIT-CD   TO  WS-CFK-BENEFIT-NO            
02593                                      WS-BENEFIT-NO                
02594          PERFORM 8700-LOCATE-BENEFIT                              
02595          IF WS-KIND NOT = SPACES                                  
02596             MOVE WS-COV-TYPE        TO  CPA-COVERAGE-TYPE         
02597             MOVE WS-RETRO-ELIM      TO  CPA-BENEFIT-TYPE          
02598             MOVE WS-RETRO-DAYS      TO  CPA-ELIMINATION-DAYS      
02599             MOVE CM-AH-BENEFIT-AMT  TO  CPA-MONTHLY-BENEFIT       
02600           ELSE                                                    
02601             MOVE ZERO               TO  CPA-ELIMINATION-DAYS      
02602                                         CPA-MONTHLY-BENEFIT       
02603        ELSE                                                       
02604          MOVE ZERO               TO  CPA-ELIMINATION-DAYS         
02605                                      CPA-MONTHLY-BENEFIT          
02606          MOVE PI-COMPANY-ID      TO  WS-CFK-COMPANY-ID            
02607          MOVE '4'                TO  WS-CFK-RECORD-TYPE           
02608          MOVE CM-LF-BENEFIT-CD   TO  WS-CFK-BENEFIT-NO            
02609                                      WS-BENEFIT-NO                
02610          PERFORM 8700-LOCATE-BENEFIT                              
02611          IF WS-KIND NOT = SPACES                                  
02612             MOVE WS-COV-TYPE        TO  CPA-COVERAGE-TYPE.        
02613                                                                   
02614      IF CM-CERT-EFF-DT = LOW-VALUES                               
02615          GO TO 0375-MAIN-LOGIC.                                   
02616                                                                   
02617      MOVE CM-CERT-EFF-DT         TO  DC-BIN-DATE-1.               
02618                                                                   
052614     IF CL-CLAIM-TYPE = 'A' OR 'I' OR 'G' or 'F'
080322                            OR 'B' OR 'H'
02620          MOVE CM-AH-ORIG-TERM    TO  DC-ELAPSED-MONTHS            
02621          MOVE CM-AH-BENEFIT-CD   TO  CPA-BENEFIT-CD               
02622        ELSE                                                       
02623          MOVE CM-LF-BENEFIT-AMT  TO  CPA-TOTAL-BENEFIT            
02624          MOVE CM-LF-BENEFIT-CD   TO  CPA-BENEFIT-CD               
02625          MOVE CM-LF-ORIG-TERM    TO  DC-ELAPSED-MONTHS.           
02626                                                                   
02627      MOVE ZERO                   TO  DC-ELAPSED-DAYS              
02628      MOVE '6'                    TO  DC-OPTION-CODE               
02629      PERFORM 8500-DATE-CONVERSION                                 
02630                                                                   
02631      MOVE DC-BIN-DATE-2          TO  CPA-EXPIRE-DT.               
02632                                                                   
02633      GO TO 0375-MAIN-LOGIC.                                       
02634                                                                   
02635      EJECT                                                        
02636  0372-READ-EMPLAN.                                                
02637                                                                   
02638      EXEC CICS HANDLE CONDITION                                   
02639          NOTFND   (0375-MAIN-LOGIC)                               
02640      END-EXEC.                                                    
02641                                                                   
02642      MOVE PM-COMPANY-CD          TO  WS-PP-COMPANY-CD.            
02643      MOVE PM-CARRIER             TO  WS-PP-CARRIER.               
02644      MOVE PM-GROUPING            TO  WS-PP-GROUPING.              
02645      MOVE PM-STATE               TO  WS-PP-STATE.                 
02646      MOVE PM-PRODUCER            TO  WS-PP-PRODUCER.              
02647      MOVE PM-INS-PLAN-CD         TO  WS-PP-PLAN-CODE.             
02648      MOVE PM-INS-PLAN-REVISION   TO  WS-PP-REV-NO.                
02649      MOVE 'PLAN'                 TO  FILE-SWITCH.                 
02650                                                                   
02651      EXEC CICS READ                                               
02652          DATASET   (WS-PLAN-MASTER-DSID)                          
02653          RIDFLD    (WS-PLAN-MASTER-KEY)                           
02654          SET       (ADDRESS OF PRODUCER-PLANS)                    
02655      END-EXEC.                                                    
02656                                                                   
02657      MOVE PP-BENEFIT-TYPE            TO  CPA-COVERAGE-TYPE.       
02658                                                                   
02659      IF PM-AH-MORT-PLAN                                           
02660          MOVE PM-INS-MONTH-BENEFIT   TO  CPA-MONTHLY-BENEFIT      
02661      ELSE                                                         
02662          MOVE PM-INS-TOTAL-BENEFIT   TO  CPA-TOTAL-BENEFIT.       
02663                                                                   
02664      MOVE PM-INS-PLAN-CD             TO  CPA-BENEFIT-CD.          
02665                                                                   
02666      IF PM-POLICY-EFF-DT = LOW-VALUES                             
02667          GO TO 0375-MAIN-LOGIC.                                   
02668                                                                   
02669      MOVE PM-POLICY-EFF-DT           TO  DC-BIN-DATE-1.           
02670      MOVE PM-LOAN-TERM               TO  DC-ELAPSED-MONTHS.       
02671      MOVE +0                         TO  DC-ELAPSED-DAYS.         
02672      MOVE '6'                        TO  DC-OPTION-CODE.          
02673      PERFORM 8500-DATE-CONVERSION.                                
02674      MOVE DC-BIN-DATE-2              TO  CPA-EXPIRE-DT.           
02675      EJECT                                                        
02676  0375-MAIN-LOGIC.                                                 
02677      ADD +1  TO  CQ-TIMES-PRINTED.                                
02678                                                                   
02679      IF AOPTIONI = '3'                                            
02680        AND CQ-PRE-NUMBERING-SW = '1'                              
02681          MOVE CHECK-QUE          TO  WS-NEW-CHECK-QUEUE-RECORD    
02682          MOVE WS-OLD-CHECK-QUEUE-RECORD  TO  CHECK-QUE            
02683          MOVE SPACES             TO  CQ-PAYEE-BENE-ACCT           
02684          MOVE 'S'                TO  CQ-ENTRY-TYPE.               
02685          MOVE LOW-VALUES         TO  CQ-APPLIED-TO-RCON-DT.       
02686                                                                   
02687      MOVE +1760                  TO  CQ-LAST-UPDATED-BY.          
02688      MOVE EIBTIME                TO  CQ-LAST-UPDATED-HHMMSS.      
02689      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             
02690      MOVE '5'                    TO  DC-OPTION-CODE.              
02691      PERFORM 8500-DATE-CONVERSION.                                
02692      MOVE DC-BIN-DATE-1          TO  CQ-LAST-UPDATED-DT.          
02693                                                                   
02694      EXEC CICS REWRITE                                            
02695          DATASET (WS-CHECK-QUEUE-DSID)                            
02696          FROM    (CHECK-QUE)                                      
02697      END-EXEC.                                                    
02698                                                                   
02699      IF AOPTIONI NOT = '3'                                        
02700          GO TO 0380-MAIN-LOGIC.                                   
02701                                                                   
02702      IF WS-ELCHKQ-GETMAIN-SW = 'Y'                                
02703          MOVE 'N'                TO  WS-ELCHKQ-GETMAIN-SW         
02704          EXEC CICS GETMAIN                                        
02705              SET    (WS-ELCHKQ-POINTER)                           
02706              LENGTH (100)                                         
02707          END-EXEC.                                                
02708                                                                   
02709      MOVE WS-ELCHKQ-POINTER             TO LCP-WS-ADDR-COMP       
02710      SET ADDRESS OF CHECK-QUE TO LCP-WS-ADDR-PNTR.                
02711                                                                   
02712      MOVE WS-NEW-CHECK-QUEUE-RECORD  TO  CHECK-QUE                
02713                                                                   
02714      IF CQ-PRE-NUMBERING-SW NOT = '1'                             
02715          GO TO 0380-MAIN-LOGIC.                                   
02716                                                                   
02717      MOVE 'Q'                    TO  CQ-ENTRY-TYPE.               
02718                                                                   
02719      MOVE SPACE                  TO  CQ-LEDGER-FLAG.              
02720                                                                   
02721      MOVE WS-SEQUENCE-NUMBER     TO  CQ-SEQUENCE-NUMBER           
02722                                      CQ-SEQUENCE-NUMBER-A1.       
02723                                                                   
02724      ADD +1  TO  WS-SEQUENCE-NUMBER.                              
02725                                                                   
02726      MOVE +1760                  TO  CQ-LAST-UPDATED-BY.          
02727      MOVE EIBTIME                TO  CQ-LAST-UPDATED-HHMMSS.      
02728      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             
02729      MOVE '5'                    TO  DC-OPTION-CODE.              
02730      PERFORM 8500-DATE-CONVERSION.                                
02731      MOVE DC-BIN-DATE-1          TO  CQ-LAST-UPDATED-DT.          
02732      MOVE LOW-VALUES             TO  CQ-APPLIED-TO-RCON-DT.       
02733                                                                   
02734      EXEC CICS WRITE                                              
02735          DATASET (WS-CHECK-QUEUE-DSID)                            
02736          RIDFLD  (CQ-CONTROL-PRIMARY)                             
02737          FROM    (CHECK-QUE)                                      
02738      END-EXEC.                                                    
02739                                                                   
02740  0380-MAIN-LOGIC.                                                 
02741      PERFORM 0800-PRINT-CHECK.                                    
02742                                                                   
02743      EXEC CICS STARTBR                                            
02744          DATASET (WS-CHECK-QUEUE-AIX-DSID)                        
02745          RIDFLD  (WS-CHECK-AIX-KEY)                               
02746          GTEQ                                                     
02747      END-EXEC.                                                    
02748                                                                   
02749      GO TO 0320-MAIN-LOGIC.                                       
02750                                                                   
02751      EJECT                                                        
02752  0390-MAIN-LOGIC.                                                 
02753      IF AOPTIONI NOT = '1'                                        
02754        AND PI-INDEX LESS THAN PI-NUMBER-OF-CONTROL-GROUPS         
02755          SET PI-INDEX UP BY +1                                    
02756          GO TO 0310-MAIN-LOGIC.                                   
02757                                                                   
02758      EXEC CICS ENDBR                                              
02759          DATASET (WS-CHECK-QUEUE-AIX-DSID)                        
02760      END-EXEC.                                                    
02761                                                                   
02762      IF PI-CHECK-PRINTER-ID NOT = 'R2T7'                          
02763          PERFORM 0700-END-PRINT.                                  
02764                                                                   
02765      MOVE PI-PRINTER-STARTED-SW  TO  WS-PRINTER-STARTED-SW.       
02766      MOVE PI-TEMP-STORAGE-KEY    TO  WS-TEMP-STORAGE-KEY.         
02767      ADD +1  TO  WS-COMPLETED-SUCCESSFUL.                         
02768      GO TO 0015-MAIN-LOGIC.                                       
02769                                                                   
02770  0700-END-PRINT SECTION.                                          
02771      MOVE HIGH-VALUES            TO  CHECK-PASS-AREA.             
02772      MOVE +1                     TO  WS-TS-LENGTH.                
02773                                                                   
02774      PERFORM 0800-PRINT-CHECK.                                    
02775                                                                   
02776      MOVE ZERO                   TO  PI-PRINTER-STARTED-SW.       
02777                                                                   
02778  0700-EXIT.                                                       
02779      EXIT.                                                        
02780                                                                   
02781  0800-PRINT-CHECK SECTION.                                        
02782                                                                   
02783      IF PI-COMPANY-ID = 'JAL' OR 'JAI'                            
02784         NEXT SENTENCE                                             
02785      ELSE                                                         
02786         EXEC CICS WRITEQ TS                                       
02787              QUEUE  (PI-TEMP-STORAGE-KEY)                         
02788              ITEM   (WS-TEMP-STORAGE-ITEM)                        
02789              FROM   (CHECK-PASS-AREA)                             
02790              LENGTH (WS-TS-LENGTH)                                
02791         END-EXEC.                                                 
02792                                                                   
02793  0800-EXIT.                                                       
02794      EXIT.                                                        
02795                                                                   
02796      EJECT                                                        
02797  0900-TERMIDERR SECTION.                                          
02798      EXEC CICS SYNCPOINT                                          
02799          ROLLBACK                                                 
02800      END-EXEC.                                                    
02801                                                                   
02802      MOVE ER-0371                TO  EMI-ERROR.                   
02803      MOVE -1                     TO  APFKL.                       
02804      PERFORM 8200-SEND-DATAONLY.                                  
02805      PERFORM 9100-RETURN-TRAN.                                    
02806                                                                   
02807  0910-ENQ-BUSY.                                                   
02808      MOVE ER-0395                TO  EMI-ERROR.                   
02809      MOVE -1                     TO  AOPTIONL.                    
02810      PERFORM 8200-SEND-DATAONLY.                                  
02811      PERFORM 9100-RETURN-TRAN.                                    
02812                                                                   
02813      EJECT                                                        
02814  1000-GET-CARRIER-NAME SECTION.                                   
02815      EXEC CICS HANDLE CONDITION                                   
02816          NOTFND (1020-CARRIER-NOT-FOUND)                          
02817      END-EXEC.                                                    
02818                                                                   
02819      MOVE PI-COMPANY-ID          TO  WS-CONTROL-FILE-KEY.         
02820      MOVE '6'                    TO  WS-CFK-RECORD-TYPE.          
02821      MOVE CQ-CARRIER             TO  WS-CFK-CARRIER.              
02822      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.          
02823      MOVE 'CNTL'                 TO  FILE-SWITCH.                 
02824                                                                   
02825      EXEC CICS READ                                               
02826          DATASET (WS-CONTROL-FILE-DSID)                           
02827          RIDFLD  (WS-CONTROL-FILE-KEY)                            
02828          SET     (ADDRESS OF CONTROL-FILE)                        
02829      END-EXEC.                                                    
02830                                                                   
02831      MOVE CF-MAIL-TO-NAME        TO  WS-CARRIER-MAIL-TO-NAME.     
02832      MOVE CF-IN-CARE-OF          TO  WS-CARRIER-IN-CARE-OF.       
02833      MOVE CF-ADDRESS-LINE-1      TO  WS-CARRIER-ADDRESS-LINE-1.   
02834      MOVE CF-ADDRESS-LINE-2      TO  WS-CARRIER-ADDRESS-LINE-2.   
02835      MOVE CF-CITY-STATE          TO  WS-CARRIER-CITY-STATE.       
02836                                                                   
02837      IF CF-ZIP-CODE-NUM NOT NUMERIC                               
02838          MOVE ZEROS              TO CF-ZIP-CODE-NUM.              
02839      IF CF-ZIP-CODE-NUM NOT = ZEROS                               
02840          MOVE CF-ZIP-CODE-NUM    TO  WS-ZIP-UNPACKED              
02841          MOVE WS-ZIP-UNPACKED    TO  WS-CARRIER-ZIP-CODE          
02842      ELSE                                                         
02843          MOVE CF-ZIP-CODE        TO  WS-CARRIER-ZIP-CODE.         
02844                                                                   
02845      MOVE CF-PHONE-NO            TO  WS-CARRIER-PHONE-NO.         
02846      GO TO 1090-EXIT.                                             
02847                                                                   
02848  1020-CARRIER-NOT-FOUND.                                          
02849      MOVE SPACES                 TO  WS-CARRIER-ADDRESS-DATA.     
02850                                                                   
02851  1090-EXIT.                                                       
02852      EXIT.                                                        
02853                                                                   
02854      EJECT                                                        
02855  2000-GET-BENEFICIARY SECTION.                                    
02856      EXEC CICS HANDLE CONDITION                                   
02857          NOTFND (2080-GET-BENEFICIARY)                            
02858      END-EXEC.                                                    
02859                                                                   
02860      MOVE PI-COMPANY-CD          TO  WS-BK-COMPANY-CD.            
02861      MOVE 'B'                    TO  WS-BK-RECORD-TYPE.           
02862                                                                   
02863      EXEC CICS READ                                               
02864          DATASET (WS-BENEFICIARY-MASTER-DSID)                     
02865          RIDFLD  (WS-BENEFICIARY-KEY)                             
02866          SET     (ADDRESS OF BENEFICIARY-MASTER)                  
02867      END-EXEC.                                                    
02868                                                                   
02869      MOVE BE-MAIL-TO-NAME        TO  CPA-PAYEE-NAME.              
02870      MOVE BE-ADDRESS-LINE-1      TO  CPA-PAYEE-ADDRESS-LINE1.     
02871      MOVE BE-ADDRESS-LINE-2      TO  CPA-PAYEE-ADDRESS-LINE2.     
02872      MOVE SPACES                 TO  CPA-PAYEE-ADDRESS-LINE3.     
02873      MOVE BE-CITY-STATE          TO  CPA-PAYEE-CITY-STATE.        
02874      MOVE BE-ZIP-CODE            TO  CPA-PAYEE-ZIP.               
02875                                                                   
02876      GO TO 2090-EXIT.                                             
02877                                                                   
02878  2080-GET-BENEFICIARY.                                            
02879      MOVE SPACES                 TO  CPA-PAYEE-NAME               
02880                                      CPA-PAYEE-ADDRESS-LINE1      
02881                                      CPA-PAYEE-ADDRESS-LINE2      
02882                                      CPA-PAYEE-CITY-STATE.        
02883      MOVE ZERO                   TO  CPA-PAYEE-ZIP.               
02884                                                                   
02885  2090-EXIT.                                                       
02886      EXIT.                                                        
02887                                                                   
02888      EJECT                                                        
02889  5000-MOVE-NAME SECTION. COPY ELCMNS.                             
02890                                                                   
CIDMOD                                                                  
CIDMOD 5000-MICR-CLOSED SECTION.                                        
CIDMOD                                                                  
CIDMOD     MOVE +9836 TO EMI-ERROR.                                     
CIDMOD     MOVE ZERO TO PI-PROCESSING-SW.                               
CIDMOD     PERFORM 8200-SEND-DATAONLY.                                  
CIDMOD                                                                  
CIDMOD 5000-MICR-EXIT.                                                  
CIDMOD                                                                  
CIDMOD*5000-FLAG-CLOSED SECTION.                                        
CIDMOD*                                                                 
CIDMOD*    MOVE +9837 TO EMI-ERROR.                                     
CIDMOD*    MOVE ZERO TO PI-PROCESSING-SW.                               
CIDMOD*    PERFORM 8200-SEND-DATAONLY.                                  
CIDMOD*                                                                 
CIDMOD*5000-FLAG-EXIT.                                                  
CIDMOD                                                                  
02891      EJECT                                                        
02892  8100-SEND-INITIAL-MAP SECTION.                                   
02893      IF EMI-ERROR NOT = ZERO                                      
02894          PERFORM 9900-ERROR-FORMAT                                
02895        ELSE                                                       
02896          IF TRANSACTION-SUCCESSFUL                                
02897              PERFORM 9900-ERROR-FORMAT                            
02898              IF CHECKS-WITHOUT-ADDRESSES                          
02899                  MOVE ER-0364       TO  EMI-ERROR                 
02900                  PERFORM 9900-ERROR-FORMAT.                       
02901                                                                   
02902      MOVE EIBTIME                TO  TIME-IN.                     
02903                                                                   
02904      MOVE SAVE-DATE              TO  ADATEO.                      
02905      MOVE TIME-OUT               TO  ATIMEO.                      
02906      MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.                     
02907      MOVE EMI-MESSAGE-AREA (2)   TO  AEMSG2O.                     
02908      MOVE EMI-MESSAGE-AREA (3)   TO  AEMSG3O.                     
02909                                                                   
02910      EXEC CICS SEND                                               
02911          FROM   (EL176AI)                                         
02912          MAPSET (WS-MAPSET-NAME)                                  
02913          MAP    (WS-MAP-NAME)                                     
02914          CURSOR ERASE                                             
02915      END-EXEC.                                                    
02916                                                                   
02917      PERFORM 9100-RETURN-TRAN.                                    
02918                                                                   
02919  8100-EXIT.                                                       
02920      EXIT.                                                        
02921                                                                   
02922      EJECT                                                        
02923  8200-SEND-DATAONLY SECTION.                                      
02924      IF EMI-ERROR NOT = 3130                                      
02925          PERFORM 9900-ERROR-FORMAT                                
02926        ELSE                                                       
02927          IF TRANSACTION-SUCCESSFUL                                
02928              PERFORM 9900-ERROR-FORMAT                            
02929              IF CHECKS-WITHOUT-ADDRESSES                          
02930                  MOVE ER-0364       TO  EMI-ERROR                 
02931                  PERFORM 9900-ERROR-FORMAT.                       
02932                                                                   
02933      MOVE EIBTIME                TO  TIME-IN.                     
02934                                                                   
02935      MOVE SAVE-DATE              TO  ADATEO.                      
02936      MOVE TIME-OUT               TO  ATIMEO.                      
02937      MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.                     
02938      MOVE EMI-MESSAGE-AREA (2)   TO  AEMSG2O.                     
02939      MOVE EMI-MESSAGE-AREA (3)   TO  AEMSG3O.                     
02940                                                                   
02941      EXEC CICS SEND DATAONLY                                      
02942          FROM   (EL176AI)                                         
02943          MAPSET (WS-MAPSET-NAME)                                  
02944          MAP    (WS-MAP-NAME)                                     
02945          CURSOR                                                   
02946      END-EXEC.                                                    
02947                                                                   
02948      IF PI-PROCESSING-SW = ZERO                                   
02949          PERFORM 9100-RETURN-TRAN.                                
02950                                                                   
02951      MOVE ZERO                   TO  EMI-SUB                      
02952                                      EMI-NOTE-CTR                 
02953                                      EMI-WARNING-CTR              
02954                                      EMI-FORCABLE-CTR             
02955                                      EMI-FATAL-CTR.               
02956                                                                   
02957      MOVE '1'                    TO  EMI-SWITCH-AREA-1            
02958                                      EMI-SWITCH-AREA-2.           
02959                                                                   
02960      MOVE SPACES                    TO  EMI-ERROR-LINES.          
02961                                                                   
02962  8200-EXIT.                                                       
02963      EXIT.                                                        
02964                                                                   
02965      EJECT                                                        
02966  8300-SEND-TEXT SECTION.                                          
02967      IF PI-PRINTER-STARTED-SW NOT = ZERO                          
02968          PERFORM 0700-END-PRINT.                                  
02969                                                                   
02970      EXEC CICS SEND TEXT                                          
02971          FROM   (LOGOFF-TEXT)                                     
02972          LENGTH (LOGOFF-LENGTH)                                   
02973          ERASE  FREEKB                                            
02974      END-EXEC.                                                    
02975                                                                   
02976      EXEC CICS RETURN                                             
02977      END-EXEC.                                                    
02978                                                                   
02979  8300-EXIT.                                                       
02980      EXIT.                                                        
02981                                                                   
02982      EJECT                                                        
02983  8500-DATE-CONVERSION SECTION.                                    
02984      EXEC CICS LINK                                               
02985          PROGRAM  ('ELDATCV')                                     
02986          COMMAREA (DATE-CONVERSION-DATA)                          
02987          LENGTH   (DC-COMM-LENGTH)                                
02988      END-EXEC.                                                    
02989                                                                   
02990  8500-EXIT.                                                       
02991      EXIT.                                                        
02992                                                                   
02993      EJECT                                                        
02994  8700-LOCATE-BENEFIT SECTION.                                     
02995      EXEC CICS HANDLE CONDITION                                   
02996          NOTFND (8700-EXIT)                                       
02997      END-EXEC.                                                    
02998                                                                   
02999      MOVE SPACES                 TO  WS-KIND.                     
03000                                                                   
03001      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.           
03002      MOVE WS-BENEFIT-NO          TO  WS-CFK-BENEFIT-NO.           
03003      MOVE 'CNTL'                 TO  FILE-SWITCH.                 
03004                                                                   
03005      EXEC CICS READ                                               
03006          DATASET (WS-CONTROL-FILE-DSID)                           
03007          RIDFLD  (WS-CONTROL-FILE-KEY)                            
03008          SET     (ADDRESS OF CONTROL-FILE)                        
03009          GTEQ                                                     
03010      END-EXEC.                                                    
03011                                                                   
03012      IF WS-CFK-COMPANY-ID NOT = CF-COMPANY-ID                     
03013        OR WS-CFK-RECORD-TYPE NOT = CF-RECORD-TYPE                 
03014          GO TO 8700-EXIT.                                         
03015                                                                   
03016      MOVE +1                     TO  WS-INDEX.                    
03017                                                                   
03018  8700-LOOKUP-BENEFIT.                                             
03019      IF WS-BENEFIT-NO = CF-BENEFIT-CODE (WS-INDEX)                
03020          MOVE CF-BENEFIT-ALPHA (WS-INDEX)    TO WS-KIND           
03021          MOVE CF-LF-COVERAGE-TYPE (WS-INDEX) TO WS-COV-TYPE       
03022          GO TO 8700-EXIT.                                         
03023                                                                   
03024      IF CF-BENEFIT-CODE (WS-INDEX) NOT LESS CF-HI-BEN-IN-REC      
03025          GO TO 8700-EXIT.                                         
03026                                                                   
03027      IF WS-INDEX LESS THAN +8                                     
03028          ADD +1  TO  WS-INDEX                                     
03029          GO TO 8700-LOOKUP-BENEFIT.                               
03030                                                                   
03031  8700-EXIT.                                                       
03032      EXIT.                                                        
03033                                                                   
03034      EJECT                                                        
03035  8800-NOT-OPEN    SECTION.                                        
03036                                                                   
03037      IF FILE-SWITCH = 'BENE'                                      
03038          MOVE ER-7675            TO  EMI-ERROR.                   
03039                                                                   
03040      IF FILE-SWITCH = 'ACCT'                                      
03041          MOVE ER-0168            TO  EMI-ERROR.                   
03042                                                                   
03043      IF FILE-SWITCH = 'CHKQ'                                      
03044          MOVE ER-3776            TO  EMI-ERROR.                   
03045                                                                   
03046      IF FILE-SWITCH = 'MSTR'                                      
03047          MOVE ER-0154            TO  EMI-ERROR.                   
03048                                                                   
03049      IF FILE-SWITCH = 'CERT'                                      
03050          MOVE ER-0169            TO  EMI-ERROR.                   
03051                                                                   
03052      IF FILE-SWITCH = 'PLCY'                                      
03053          MOVE ER-9883            TO  EMI-ERROR.                   
03054                                                                   
03055      IF FILE-SWITCH = 'PROD'                                      
03056          MOVE ER-9886            TO  EMI-ERROR.                   
03057                                                                   
03058      IF FILE-SWITCH = 'TRLR'                                      
03059          MOVE ER-0172            TO  EMI-ERROR.                   
03060                                                                   
03061      IF FILE-SWITCH = 'COMP'                                      
03062          MOVE ER-2055            TO  EMI-ERROR.                   
03063                                                                   
03064      IF FILE-SWITCH = 'PLAN'                                      
03065          MOVE ER-9808            TO  EMI-ERROR.                   
03066                                                                   
03067      IF FILE-SWITCH = 'FLAG'                                      
03068          MOVE ER-3027            TO  EMI-ERROR.                   
03069                                                                   
03070      IF FILE-SWITCH = 'DRFT'                                      
03071          MOVE ER-3028            TO  EMI-ERROR.                   
03072                                                                   
03073      MOVE ZERO                   TO  PI-PROCESSING-SW.            
03074      MOVE -1                     TO  AOPTIONL.                    
03075      GO TO 8200-SEND-DATAONLY.                                    
03076      EJECT                                                        
03077  9000-RETURN-CICS SECTION.                                        
03078      MOVE 'EL005'                TO  THIS-PGM.                    
03079      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               
03080      PERFORM 9300-XCTL.                                           
03081                                                                   
03082  9000-EXIT.                                                       
03083      EXIT.                                                        
03084                                                                   
03085  9100-RETURN-TRAN SECTION.                                        
03086      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            
03087      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.        
03088                                                                   
03089      EXEC CICS RETURN                                             
03090          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       
03091          LENGTH   (PI-COMM-LENGTH)                                
03092          TRANSID  (WS-TRANS-ID)                                   
03093      END-EXEC.                                                    
03094                                                                   
03095  9100-EXIT.                                                       
03096      EXIT.                                                        
03097                                                                   
03098  9300-XCTL SECTION.                                               
03099      IF PI-PRINTER-STARTED-SW NOT = ZERO                          
03100          PERFORM 0700-END-PRINT.                                  
03101                                                                   
03102      MOVE DFHENTER               TO  EIBAID.                      
03103                                                                   
03104      EXEC CICS XCTL                                               
03105          PROGRAM  (THIS-PGM)                                      
03106          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       
03107          LENGTH   (PI-COMM-LENGTH)                                
03108      END-EXEC.                                                    
03109                                                                   
03110  9300-EXIT.                                                       
03111      EXIT.                                                        
03112                                                                   
03113      EJECT                                                        
03114  9400-CLEAR SECTION.                                              
03115      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.                     
03116      PERFORM 9300-XCTL.                                           
03117                                                                   
03118  9400-EXIT.                                                       
03119      EXIT.                                                        
03120                                                                   
03121  9600-PGMIDERR SECTION.                                           
03122      EXEC CICS HANDLE CONDITION                                   
03123          PGMIDERR (8300-SEND-TEXT)                                
03124      END-EXEC.                                                    
03125                                                                   
03126      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM.          
03127                                                                   
03128      MOVE 'EL005'                TO  THIS-PGM                     
03129                                      LOGOFF-PGM.                  
03130      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 
03131      MOVE SPACES                 TO  PI-ENTRY-CD-1.               
03132      PERFORM 9300-XCTL.                                           
03133                                                                   
03134  9600-EXIT.                                                       
03135      EXIT.                                                        
03136                                                                   
03137  9900-ERROR-FORMAT SECTION.                                       
03138      IF EMI-ERRORS-COMPLETE                                       
03139          MOVE ER-3130               TO  EMI-ERROR                 
03140          GO TO 9900-EXIT.                                         
03141                                                                   
03142      EXEC CICS LINK                                               
03143          PROGRAM  ('EL001')                                       
03144          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 
03145          LENGTH   (EMI-COMM-LENGTH)                               
03146      END-EXEC.                                                    
03147                                                                   
03148      MOVE ER-3130                   TO  EMI-ERROR.                
03149                                                                   
03150  9900-EXIT.                                                       
03151      EXIT.                                                        
03152                                                                   
03153      EJECT                                                        
03154  9990-ERROR SECTION.                                              
03155      MOVE DFHEIBLK TO EMI-LINE1.                                  
03156      EXEC CICS LINK                                               
03157          PROGRAM  ('EL004')                                       
03158          COMMAREA (EMI-LINE1)                                     
03159          LENGTH   (72)                                            
03160      END-EXEC.                                                    
03161                                                                   
03162      PERFORM 8200-SEND-DATAONLY.                                  
03163      GO TO 9100-RETURN-TRAN.                                      
03164                                                                   
03165  9990-EXIT.                                                       
03166      EXIT.                                                        
03167                                                                   
03168  9995-SECURITY-VIOLATION.                                         
03169                              COPY ELCSCTP.                        
03170                                                                   
03171  9995-EXIT.                                                       
03172      EXIT.                                                        
03173                                                                   
03174  9999-LAST-PARAGRAPH SECTION.                                     
03175      MOVE ZEROS TO RETURN-CODE.                                   
03176      GOBACK.                                                      
