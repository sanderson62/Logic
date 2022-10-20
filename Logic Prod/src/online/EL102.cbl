00001  ID DIVISION.
00003  PROGRAM-ID.                 EL102.

      * kixcplc WARNING: at or near line# 0
      * PROGRAM-ID: EL102 does not match input file: el102
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 03/20/95 08:33:29.
00007 *                            VMOD=2.057
00008 *
00010 *AUTHOR.     LOGIC,INC.
00011 *            DALLAS, TEXAS.
00013 *DATE-COMPILED.
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00024 *REMARKS.    TRANSACTION - EX08 - COMPANY IDENTIFICATION
00025 *                                 AND CONTROL
00027      EJECT
00028  ENVIRONMENT DIVISION.
00029  DATA DIVISION.
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL102 WORKING STORAGE     *'.
00033  77  FILLER  PIC X(32)  VALUE '********* VMOD=2.057 ***********'.
00035 *    COPY ELCSCTM.
                        
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCTM                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY MESSAGE AREA     *
00007 *                                                                *
00008 ******************************************************************
00009  01  SECURITY-MESSAGE.                                            
00010      12  FILLER                          PIC X(30)                
00011             VALUE '** LOGIC SECURITY VIOLATION -'.                
00012      12  SM-READ                         PIC X(6).                
00013      12  FILLER                          PIC X(5)                 
00014             VALUE ' PGM='.                                        
00015      12  SM-PGM                          PIC X(6).                
00016      12  FILLER                          PIC X(5)                 
00017             VALUE ' OPR='.                                        
00018      12  SM-PROCESSOR-ID                 PIC X(4).                
00019      12  FILLER                          PIC X(6)                 
00020             VALUE ' TERM='.                                       
00021      12  SM-TERMID                       PIC X(4).                
00022      12  FILLER                          PIC XX   VALUE SPACE.    
00023      12  SM-JUL-DATE                     PIC 9(5).                
00024      12  FILLER                          PIC X    VALUE SPACE.    
00025      12  SM-TIME                         PIC 99.99.               
00026                                                                   
00036 *    COPY ELCSCRTY.
                         
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCRTY                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
00007 *        AREA ACQUIRED BY SIGN ON PROGRAM EL125 AND ADDRESS      *
00008 *        SAVED IN PI-SECURITY-ADDRESS.                           *
00009 *                                                                *
00010 ******************************************************************
00011  01  SECURITY-CONTROL.                                            
00012      12  SC-COMM-LENGTH               PIC S9(4) VALUE +144 COMP.  
00013      12  FILLER                       PIC XX    VALUE 'SC'.       
00014      12  SC-CREDIT-CODES.                                         
00015          16  SC-CREDIT-AUTHORIZATION OCCURS 40 TIMES.             
00016              20  SC-CREDIT-DISPLAY    PIC X.                      
00017              20  SC-CREDIT-UPDATE     PIC X.                      
00018      12  SC-CLAIMS-CODES.                                         
00019          16  SC-CLAIMS-AUTHORIZATION OCCURS 30 TIMES.             
00020              20  SC-CLAIMS-DISPLAY    PIC X.                      
00021              20  SC-CLAIMS-UPDATE     PIC X.                      
00037      EJECT
00038  01  WS-DATE-AREA.
00039      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00040      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
00042  01  STANDARD-AREAS.
00043      12  MAP-NAME            PIC X(8)    VALUE 'EL102A'.
00044      12  MAPSET-NAME         PIC X(8)    VALUE 'EL102S'.
00045      12  TRANS-ID            PIC X(4)    VALUE 'EX08'.
00046      12  THIS-PGM            PIC X(8)    VALUE 'EL102'.
00047      12  PGM-NAME            PIC X(8).
00048      12  WS-COMP-CD-R.
00049          16  FILLER          PIC X.
00050          16  WS-COMP-CD-X    PIC X.
00051      12  WS-COMP-CD   REDEFINES WS-COMP-CD-R  PIC S9(4)     COMP.
00052      12  TIME-IN             PIC S9(7).
00053      12  TIME-OUT-R  REDEFINES TIME-IN.
00054          16  FILLER          PIC X.
00055          16  TIME-OUT        PIC 99V99.
00056          16  FILLER          PIC XX.
00057      12  XCTL-005            PIC X(8)    VALUE 'EL005'.
00058      12  XCTL-010            PIC X(8)    VALUE 'EL010'.
00059      12  XCTL-126            PIC X(8)    VALUE 'EL126'.
00060      12  XCTL-626            PIC X(8)    VALUE 'EL626'.
00061      12  XCTL-EM626          PIC X(8)    VALUE 'EM626'.
00062      12  XCTL-400            PIC X(8)    VALUE 'LF400'.
00063      12  XCTL-800            PIC X(8)    VALUE 'GL800'.
00064      12  LINK-001            PIC X(8)    VALUE 'EL001'.
00065      12  LINK-004            PIC X(8)    VALUE 'EL004'.
00066      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
00067      12  FILE-ID             PIC X(8)    VALUE 'ELCNTL'.
00069  01  MISC-WORK-AREAS.
00070      12  W-OPTIONAL-RESERVE-AREAS.
00071          16 W-CREDIBLITY-TABLE   PIC  X(01).
00072          16 W-IBLGMTH            PIC S9(3)   VALUE ZERO.
00073          16 W-CIDADIS            PIC S9V9(4) VALUE ZERO.
00074          16 W-CALCINT            PIC S9V9(4) VALUE ZERO.
00075          16 W-IBNRLFF            PIC S9V9(4) VALUE ZERO.
00076          16 W-IBNRAHF            PIC S9V9(4) VALUE ZERO.
00077          16 W-OPTDTE             PIC XX      VALUE SPACES.
00078      12  WS-ZIP-CODE.
00079          16  WS-ZIP-1            PIC X.
00080              88  WS-CANADIAN-ZIP    VALUE 'A' THRU 'Z'.
00081          16  WS-ZIP-2-3          PIC XX.
00082          16  WS-ZIP-4            PIC X.
00083          16  WS-ZIP-5            PIC X.
00084          16  WS-ZIP-6            PIC X.
00085          16  FILLER              PIC X(4).
00086      12  WS-ZIP-AM-1  REDEFINES  WS-ZIP-CODE.
00087          16  WS-ZIP-AM-1-CODE    PIC X(5).
00088          16  WS-ZIP-AM-1-PLUS4   PIC X(4).
00089          16  FILLER              PIC X.
00090      12  WS-ZIP-AM-2  REDEFINES  WS-ZIP-CODE.
00091          16  WS-ZIP-AM-2-CODE    PIC X(5).
00092          16  WS-ZIP-AM-2-DASH    PIC X.
00093          16  WS-ZIP-AM-2-PLUS4   PIC X(4).
00094      12  WS-ZIP-CAN-1  REDEFINES  WS-ZIP-CODE.
00095          16  WS-ZIP-CAN-1-POST1  PIC X(3).
00096          16  WS-ZIP-CAN-1-POST2  PIC X(3).
00097          16  FILLER              PIC X(4).
00098      12  WS-ZIP-CAN-2  REDEFINES  WS-ZIP-CODE.
00099          16  WS-ZIP-CAN-2-POST1  PIC X(3).
00100          16  FILLER              PIC X.
00101          16  WS-ZIP-CAN-2-POST2  PIC X(3).
00102          16  FILLER              PIC X(3).
00104      12  WS-ZIP-CODE-NUM         PIC 9(9).
00106      12  WS-PHONE-IN         PIC 9(11).
00107      12  WS-PHONE-IN-R  REDEFINES WS-PHONE-IN.
00108          16  FILLER          PIC X.
00109          16  WSPI-AREA       PIC X(3).
00110          16  WSPI-PFX        PIC X(3).
00111          16  WSPI-SFX        PIC X(4).
00112      12  WS-PHONE-OUT.
00113          16  WSPO-AREA       PIC X(3).
00114          16  FILLER          PIC X       VALUE '-'.
00115          16  WSPO-PFX        PIC X(3).
00116          16  FILLER          PIC X       VALUE '-'.
00117          16  WSPO-SFX        PIC X(4).
00118      12  WS-COV-DESCRIPTION.
00119          16  FILLER          PIC X(15)
00120                                 VALUE 'BENEFIT DESC - '.
00121          16  WS-COV-DESC-2   PIC XX.
00122          16  FILLER          PIC XX      VALUE ' :'.
00123      12  TERMINAL-TEST-AREA.
00124          16  TTA-1           PIC X.
00125          16  TTA-2           PIC X.
00126          16  TTA-3           PIC X.
00127          16  TTA-4           PIC X.
00128      12  DEEDIT-FIELD            PIC X(17).
00129      12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(17).
00130      12  SUB1                PIC S9(4)   VALUE +0    COMP.
00131      12  SUB2                PIC S9(4)   VALUE +0    COMP.
00132      12  SV-CLMTOL           PIC 9(3)V99  VALUE ZERO.
00133      12  SV-QTECAL           PIC 9(3)V99  VALUE ZERO.
00134      12  SV-OVSAMT           PIC 9(3)V99  VALUE ZERO.
00135      12  SV-MTOLCAP          PIC 9(3)V99  VALUE ZERO.
00136      12  SV-DAPR             PIC 9(3)V9(4) VALUE ZERO.
00137      12  SV-QTEREF           PIC 9(3)V99  VALUE ZERO.
00138      12  SV-MINPREM          PIC 9(3)V99  VALUE ZERO.
00139      12  SV-COMPWTE          PIC 9(3)V99  VALUE ZERO.
00140      12  SV-PHONE            PIC 9(11)    VALUE ZERO.
00141      12  SV-MORTG-REF-NUM    PIC 9(17)    VALUE ZERO.
00142      12  SV-APPROVAL-LEVELS.
00143          16  SV-LLEV1        PIC 9(7)     VALUE ZERO.
00144          16  SV-LLEV2        PIC 9(7)     VALUE ZERO.
00145          16  SV-LLEV3        PIC 9(7)     VALUE ZERO.
00146          16  SV-ALEV1        PIC 9(7)     VALUE ZERO.
00147          16  SV-ALEV2        PIC 9(7)     VALUE ZERO.
00148          16  SV-ALEV3        PIC 9(7)     VALUE ZERO.
00149      12  DATE-TEST-AREA      PIC 9(6).
00150      12  DATE-TEST-AREA-R  REDEFINES DATE-TEST-AREA.
00151          16  DATE-TEST-MM    PIC 99.
00152          16  DATE-TEST-DD    PIC 99.
00153          16  DATE-TEST-YY    PIC 99.
00154      12  DIVIDE-RESULT       PIC 99.
00155      12  DIVIDE-REMAINDER    PIC 9.
00156      12  WS-TOL-PREM-PCT     PIC S9V9(4) VALUE ZERO.
00157      12  WS-TOL-REFUND-PCT   PIC S9V9(4) VALUE ZERO.
00158      12  WS-CO-OVR-SHT-PCT   PIC S9V9(4) VALUE ZERO.
00159      12  W-EXPRETN           PIC 9          VALUE ZERO.
00161  01  WS-PASSWORD-NUMERIC            PIC X.
00162  01  WS-PASSWORD-SIXDIGIT           PIC X.
00164  01  WS-SAVE-PASSWORD.
00165      12  WS-PASSWORD-X1             PIC X.
00166      12  WS-PASSWORD-X2             PIC X.
00167      12  WS-PASSWORD-X3             PIC X.
00168      12  WS-PASSWORD-X4             PIC X.
00169      12  WS-PASSWORD-X5             PIC X.
00170      12  WS-PASSWORD-X6             PIC X.
00171      12  WS-PASSWORD-X7             PIC X.
00172      12  WS-PASSWORD-X8             PIC X.
00174  01  WS-SAVE-PASSWORD-NUMERIC REDEFINES  WS-SAVE-PASSWORD.
00175      12  WS-PASSWORD-N1             PIC 9.
00176      12  WS-PASSWORD-N2             PIC 9.
00177      12  WS-PASSWORD-N3             PIC 9.
00178      12  WS-PASSWORD-N4             PIC 9.
00179      12  WS-PASSWORD-N5             PIC 9.
00180      12  WS-PASSWORD-N6             PIC 9.
00181      12  WS-PASSWORD-N7             PIC 9.
00182      12  WS-PASSWORD-N8             PIC 9.
00185  01  ACCESS-KEYS.
00186      12  ELCNTL-KEY.
00187          16  CK-COMP-ID      PIC X(3).
00188          16  CK-REC-TYPE     PIC X       VALUE '1'.
00189          16  FILLER          PIC X(4)    VALUE SPACES.
00190          16  CK-SEQ-NO       PIC S9(4)   VALUE +0    COMP.
00191      12  W-WORKING-CNTL-KEY.
00192          16  W-CNTL-COMP-ID  PIC  X(3).
00193          16  W-CNTL-REC-TYPE PIC  X.
00194          16  W-CNTL-TABLE-INDICATOR
00195                              PIC  X.
00196          16  W-CNTL-BENEFIT-TYPE
00197                              PIC  X.
00198          16  W-CNTL-WAITING-PERIOD
00199                              PIC  XX.
00200          16  W-CNTL-SEQUENCE-NO
00201                              PIC S9(4)  COMP.
00203  01  WS-ELCNTL-KEY.
00204      16  WS-COMP-ID      PIC X(3)    VALUE SPACES.
00205      16  WS-TYPE         PIC X       VALUE SPACES.
00206      16  WS-USER         PIC X(4)    VALUE SPACES.
00207      16  WS-SEQ          PIC S9(4)   VALUE +0    COMP.
00209  01  WS-LNAME-SEARCH-CNTL.
00210      03  WS-MIB-LNAME-SEARCH         PIC X.
00211      03  WS-ALP-LNAME-SEARCH         PIC X.
00212  01  WS-FNAME-SEARCH-CNTL.
00213      03  WS-MIB-FNAME-SEARCH         PIC X.
00214      03  WS-ALP-FNAME-SEARCH         PIC X.
00215  01  WS-MNAME-SEARCH-CNTL.
00216      03  WS-MIB-MNAME-SEARCH         PIC X.
00217      03  WS-ALP-MNAME-SEARCH         PIC X.
00218  01  WS-BDATE-SEARCH-CNTL.
00219      03  WS-MIB-BDATE-SEARCH         PIC X.
00220      03  WS-ALP-BDATE-SEARCH         PIC X.
00221  01  WS-BSTATE-SEARCH-CNTL.
00222      03  WS-MIB-BSTATE-SEARCH        PIC X.
00223      03  WS-ALP-BSTATE-SEARCH        PIC X.
00224  01  WS-RSTATE-SEARCH-CNTL.
00225      03  WS-MIB-RSTATE-SEARCH        PIC X.
00226      03  WS-ALP-RSTATE-SEARCH        PIC X.
00228  01  WS-SAVE-DATES.
00229      05  WS-SAVE-CONDTE      PIC XX.
00230      05  WS-SAVE-RTEFILC     PIC XX.
00231      05  WS-SAVE-COMTABC     PIC XX.
00232      05  WS-SAVE-PRDMSTC     PIC XX.
00233      05  WS-SAVE-ACTMSTC     PIC XX.
00234      05  WS-SAVE-RENTABC     PIC XX.
00235      05  WS-SAVE-CMPMSTC     PIC XX.
00236      05  WS-SAVE-RTEFILM     PIC XX.
00237      05  WS-SAVE-COMTABM     PIC XX.
00238      05  WS-SAVE-ACTMSTM     PIC XX.
00239      05  WS-SAVE-PRDMSTM     PIC XX.
00240      05  WS-SAVE-RENTABM     PIC XX.
00241      05  WS-SAVE-CMPMSTM     PIC XX.
00242      05  WS-SAVE-CURRMEDT    PIC XX.
00243      05  WS-SAVE-CREDMEDT    PIC XX.
00244      05  WS-SAVE-MORTMEDT    PIC XX.
00245      05  WS-SAVE-ARMEDT      PIC XX.
00246      05  WS-SAVE-AR860DT     PIC XX.
00247      05  WS-SAVE-COFFDTE     PIC XX.
00248      05  WS-SAVE-PROCDT      PIC XX.
00250  01  CONTROL-FILE-SAVE-AREA.
00251      05  FILLER              PIC X(750).
00253      EJECT
00254 *    COPY ELCDATE.
                        
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDATE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE            
00005 *                            VMOD=2.003                           
00006 *                                                                *
00007 *                                                                *
00008 *   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
00009 *                 LENGTH = 200                                   *
00010 ******************************************************************
00011                                                                   
00012  01  DATE-CONVERSION-DATA.                                        
00013      12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200. 
00014      12  DC-OPTION-CODE                PIC X.                     
00015          88  BIN-TO-GREG                VALUE ' '.                
00016          88  ELAPSED-BETWEEN-BIN        VALUE '1'.                
00017          88  EDIT-GREG-TO-BIN           VALUE '2'.                
00018          88  YMD-GREG-TO-BIN            VALUE '3'.                
00019          88  MDY-GREG-TO-BIN            VALUE '4'.                
00020          88  JULIAN-TO-BIN              VALUE '5'.                
00021          88  BIN-PLUS-ELAPSED           VALUE '6'.                
00022          88  FIND-CENTURY               VALUE '7'.                
00023          88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.                
00024          88  EDIT-GREG-TO-BIN-3         VALUE '9'.                
00025          88  YMD-GREG-TO-BIN-3          VALUE 'A'.                
00026          88  MDY-GREG-TO-BIN-3          VALUE 'B'.                
00027          88  JULIAN-TO-BIN-3            VALUE 'C'.                
00028          88  BIN-PLUS-ELAPSED-3         VALUE 'D'.                
00029          88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.                
00030          88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.                
00031          88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.                
00032          88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.      
00033          88  CHECK-LEAP-YEAR            VALUE 'H'.                
00034          88  BIN-3-TO-GREG              VALUE 'I'.                
00035          88  CYMD-GREG-TO-BIN-3         VALUE 'J'.                
00036          88  MDCY-GREG-TO-BIN-3         VALUE 'K'.                
00037          88  CYMD-GREG-TO-BIN           VALUE 'L'.                
00038          88  MDCY-GREG-TO-BIN           VALUE 'M'.                
00039          88  MDY-GREG-TO-JULIAN         VALUE 'N'.                
00040          88  MDCY-GREG-TO-JULIAN        VALUE 'O'.                
00041          88  YMD-GREG-TO-JULIAN         VALUE 'P'.                
00042          88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.                
00043          88  THREE-CHARACTER-BIN                                  
00044                   VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.    
00045          88  GREGORIAN-TO-BIN                                     
00046                   VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'. 
00047          88  BIN-TO-GREGORIAN                                     
00048                   VALUES ' ' '1' 'I' '8' 'G'.                     
00049          88  JULIAN-TO-BINARY                                     
00050                   VALUES '5' 'C' 'E' 'F'.                         
00051      12  DC-ERROR-CODE                 PIC X.                     
00052          88  NO-CONVERSION-ERROR        VALUE ' '.                
00053          88  DATE-CONVERSION-ERROR                                
00054                   VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.     
00055          88  DATE-IS-ZERO               VALUE '1'.                
00056          88  DATE-IS-NON-NUMERIC        VALUE '2'.                
00057          88  DATE-IS-INVALID            VALUE '3'.                
00058          88  DATE1-GREATER-DATE2        VALUE '4'.                
00059          88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.                
00060          88  DATE-INVALID-OPTION        VALUE '9'.                
00061          88  INVALID-CENTURY            VALUE 'A'.                
00062          88  ONLY-CENTURY               VALUE 'B'.                
00063          88  ONLY-LEAP-YEAR             VALUE 'C'.                
00064          88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.           
00065      12  DC-END-OF-MONTH               PIC X.                     
00066          88  CALCULATE-END-OF-MONTH     VALUE '1'.                
00067      12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.      
00068          88  USE-NORMAL-PROCESS         VALUE ' '.                
00069          88  ADJUST-DOWN-100-YRS        VALUE '1'.                
00070          88  ADJUST-UP-100-YRS          VALUE '2'.                
00071      12  FILLER                        PIC X.                     
00072      12  DC-CONVERSION-DATES.                                     
00073          16  DC-BIN-DATE-1             PIC XX.                    
00074          16  DC-BIN-DATE-2             PIC XX.                    
00075          16  DC-GREG-DATE-1-EDIT       PIC X(08).                 
00076          16  DC-GREG-DATE-1-EDIT-R REDEFINES                      
00077                        DC-GREG-DATE-1-EDIT.                       
00078              20  DC-EDIT1-MONTH        PIC 99.                    
00079              20  SLASH1-1              PIC X.                     
00080              20  DC-EDIT1-DAY          PIC 99.                    
00081              20  SLASH1-2              PIC X.                     
00082              20  DC-EDIT1-YEAR         PIC 99.                    
00083          16  DC-GREG-DATE-2-EDIT       PIC X(08).                 
00084          16  DC-GREG-DATE-2-EDIT-R REDEFINES                      
00085                      DC-GREG-DATE-2-EDIT.                         
00086              20  DC-EDIT2-MONTH        PIC 99.                    
00087              20  SLASH2-1              PIC X.                     
00088              20  DC-EDIT2-DAY          PIC 99.                    
00089              20  SLASH2-2              PIC X.                     
00090              20  DC-EDIT2-YEAR         PIC 99.                    
00091          16  DC-GREG-DATE-1-YMD        PIC 9(06).                 
00092          16  DC-GREG-DATE-1-YMD-R  REDEFINES                      
00093                      DC-GREG-DATE-1-YMD.                          
00094              20  DC-YMD-YEAR           PIC 99.                    
00095              20  DC-YMD-MONTH          PIC 99.                    
00096              20  DC-YMD-DAY            PIC 99.                    
00097          16  DC-GREG-DATE-1-MDY        PIC 9(06).                 
00098          16  DC-GREG-DATE-1-MDY-R REDEFINES                       
00099                       DC-GREG-DATE-1-MDY.                         
00100              20  DC-MDY-MONTH          PIC 99.                    
00101              20  DC-MDY-DAY            PIC 99.                    
00102              20  DC-MDY-YEAR           PIC 99.                    
00103          16  DC-GREG-DATE-1-ALPHA.                                
00104              20  DC-ALPHA-MONTH        PIC X(10).                 
00105              20  DC-ALPHA-DAY          PIC 99.                    
00106              20  FILLER                PIC XX.                    
00107              20  DC-ALPHA-CENTURY.                                
00108                  24 DC-ALPHA-CEN-N     PIC 99.                    
00109              20  DC-ALPHA-YEAR         PIC 99.                    
00110          16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.        
00111          16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.        
00112          16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.        
00113          16  DC-JULIAN-DATE            PIC 9(05).                 
00114          16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE             
00115                                        PIC 9(05).                 
00116          16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.               
00117              20  DC-JULIAN-YEAR        PIC 99.                    
00118              20  DC-JULIAN-DAYS        PIC 999.                   
00119          16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.    
00120          16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3. 
00121          16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3. 
00122      12  DATE-CONVERSION-VARIBLES.                                
00123          16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.         
00124          16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.       
00125              20  FILLER                PIC 9(3).                  
00126              20  HOLD-CEN-1-CCYY.                                 
00127                  24  HOLD-CEN-1-CC     PIC 99.                    
00128                  24  HOLD-CEN-1-YY     PIC 99.                    
00129              20  HOLD-CEN-1-MO         PIC 99.                    
00130              20  HOLD-CEN-1-DA         PIC 99.                    
00131          16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.         
00132              20  HOLD-CEN-1-R-MO       PIC 99.                    
00133              20  HOLD-CEN-1-R-DA       PIC 99.                    
00134              20  HOLD-CEN-1-R-CCYY.                               
00135                  24  HOLD-CEN-1-R-CC   PIC 99.                    
00136                  24  HOLD-CEN-1-R-YY   PIC 99.                    
00137              20  FILLER                PIC 9(3).                  
00138          16  HOLD-CENTURY-1-X.                                    
00139              20  FILLER                PIC X(3)  VALUE SPACES.    
00140              20  HOLD-CEN-1-X-CCYY.                               
00141                  24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.       
00142                  24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.       
00143              20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.       
00144              20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.       
00145          16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.       
00146              20  HOLD-CEN-1-R-X-MO     PIC XX.                    
00147              20  HOLD-CEN-1-R-X-DA     PIC XX.                    
00148              20  HOLD-CEN-1-R-X-CCYY.                             
00149                  24  HOLD-CEN-1-R-X-CC PIC XX.                    
00150                  24  HOLD-CEN-1-R-X-YY PIC XX.                    
00151              20  FILLER                PIC XXX.                   
00152          16  DC-BIN-DATE-EXPAND-1      PIC XXX.                   
00153          16  DC-BIN-DATE-EXPAND-2      PIC XXX.                   
00154          16  DC-JULIAN-DATE-1          PIC 9(07).                 
00155          16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.       
00156              20  DC-JULIAN-1-CCYY.                                
00157                  24  DC-JULIAN-1-CC    PIC 99.                    
00158                  24  DC-JULIAN-1-YR    PIC 99.                    
00159              20  DC-JULIAN-DA-1        PIC 999.                   
00160          16  DC-JULIAN-DATE-2          PIC 9(07).                 
00161          16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.       
00162              20  DC-JULIAN-2-CCYY.                                
00163                  24  DC-JULIAN-2-CC    PIC 99.                    
00164                  24  DC-JULIAN-2-YR    PIC 99.                    
00165              20  DC-JULIAN-DA-2        PIC 999.                   
00166          16  DC-GREG-DATE-A-EDIT.                                 
00167              20  DC-EDITA-MONTH        PIC 99.                    
00168              20  SLASHA-1              PIC X VALUE '/'.           
00169              20  DC-EDITA-DAY          PIC 99.                    
00170              20  SLASHA-2              PIC X VALUE '/'.           
00171              20  DC-EDITA-CCYY.                                   
00172                  24  DC-EDITA-CENT     PIC 99.                    
00173                  24  DC-EDITA-YEAR     PIC 99.                    
00174          16  DC-GREG-DATE-B-EDIT.                                 
00175              20  DC-EDITB-MONTH        PIC 99.                    
00176              20  SLASHB-1              PIC X VALUE '/'.           
00177              20  DC-EDITB-DAY          PIC 99.                    
00178              20  SLASHB-2              PIC X VALUE '/'.           
00179              20  DC-EDITB-CCYY.                                   
00180                  24  DC-EDITB-CENT     PIC 99.                    
00181                  24  DC-EDITB-YEAR     PIC 99.                    
00182          16  DC-GREG-DATE-CYMD         PIC 9(08).                 
00183          16  DC-GREG-DATE-CYMD-R REDEFINES                        
00184                               DC-GREG-DATE-CYMD.                  
00185              20  DC-CYMD-CEN           PIC 99.                    
00186              20  DC-CYMD-YEAR          PIC 99.                    
00187              20  DC-CYMD-MONTH         PIC 99.                    
00188              20  DC-CYMD-DAY           PIC 99.                    
00189          16  DC-GREG-DATE-MDCY         PIC 9(08).                 
00190          16  DC-GREG-DATE-MDCY-R REDEFINES                        
00191                               DC-GREG-DATE-MDCY.                  
00192              20  DC-MDCY-MONTH         PIC 99.                    
00193              20  DC-MDCY-DAY           PIC 99.                    
00194              20  DC-MDCY-CEN           PIC 99.                    
00195              20  DC-MDCY-YEAR          PIC 99.                    
CIDMOD    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.      
CIDMOD        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.        
CIDMOD    12  DC-EL310-DATE                  PIC X(21).                 
CIDMOD    12  FILLER                         PIC X(28).                 
00255      EJECT
00256 *    COPY ELCLOGOF.
                         
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCLOGOF.                           *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             STANDARD CLAS-IC LOGOFF TEXT AREA                  *
00007 *                                                                *
00008 ******************************************************************
00009  01  CLASIC-LOGOFF.                                               
00010      12  LOGOFF-LENGTH       PIC S9(4)   VALUE +185   COMP.       
00011      12  LOGOFF-TEXT.                                             
00012          16  FILLER          PIC X(5)    VALUE SPACES.            
00013          16  LOGOFF-MSG.                                          
00014              20  LOGOFF-PGM  PIC X(8)    VALUE SPACES.            
00015              20  FILLER      PIC X       VALUE SPACES.            
00016              20  LOGOFF-FILL PIC X(66)   VALUE SPACES.            
00017          16  FILLER          PIC X(80)                            
00018            VALUE '* YOU ARE NOW LOGGED OFF'.                      
00019          16  FILLER          PIC X(7)    VALUE '* LOGIC'.         
00020          16  FILLER          PIC X       VALUE QUOTE.             
00021          16  LOGOFF-SYS-MSG  PIC X(17)                            
00022            VALUE 'S CLAS-IC SYSTEM '.                             
00023      12  TEXT-MESSAGES.                                           
00024          16  UNACCESS-MSG    PIC X(29)                            
00025              VALUE  'UNAUTHORIZED ACCESS ATTEMPTED'.              
00026          16  PGMIDERR-MSG    PIC X(17)                            
00027              VALUE 'PROGRAM NOT FOUND'.                           
00257      EJECT
00258 *    COPY ELCATTR.
                        
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCATTR.                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             LIST OF STANDARD ATTRIBUTE VALUES                  *
00007 *                                                                *
00008 *   THE DATA NAMES IN THIS COPY BOOK WERE ASSIGNED AS FOLLOWS:   *
00009 *                                                                *
00010 *                   POS 1   P=PROTECTED                          *
00011 *                           U=UNPROTECTED                        *
00012 *                           S=ASKIP                              *
00013 *                   POS 2   A=ALPHA/NUMERIC                      *
00014 *                           N=NUMERIC                            *
00015 *                   POS 3   N=NORMAL                             *
00016 *                           B=BRIGHT                             *
00017 *                           D=DARK                               *
00018 *                   POS 4-5 ON=MODIFIED DATA TAG ON              *
00019 *                           OF=MODIFIED DATA TAG OFF             *
00020 *                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCATTR                          *
00021 ******************************************************************
00022  01  ATTRIBUTE-LIST.                                              
00023      12  AL-PABOF            PIC X       VALUE 'Y'.               
00024      12  AL-PABON            PIC X       VALUE 'Z'.               
00025      12  AL-PADOF            PIC X       VALUE '%'.               
00026      12  AL-PADON            PIC X       VALUE '_'.               
00027      12  AL-PANOF            PIC X       VALUE '-'.               
00028      12  AL-PANON            PIC X       VALUE '/'.               
00029      12  AL-SABOF            PIC X       VALUE '8'.               
00030      12  AL-SABON            PIC X       VALUE '9'.               
00031      12  AL-SADOF            PIC X       VALUE '@'.               
00032      12  AL-SADON            PIC X       VALUE QUOTE.             
00033      12  AL-SANOF            PIC X       VALUE '0'.               
00034      12  AL-SANON            PIC X       VALUE '1'.               
00035      12  AL-UABOF            PIC X       VALUE 'H'.               
00036      12  AL-UABON            PIC X       VALUE 'I'.               
00037      12  AL-UADOF            PIC X       VALUE '<'.               
00038      12  AL-UADON            PIC X       VALUE '('.               
00039      12  AL-UANOF            PIC X       VALUE ' '.               
00040      12  AL-UANON            PIC X       VALUE 'A'.               
00041      12  AL-UNBOF            PIC X       VALUE 'Q'.               
00042      12  AL-UNBON            PIC X       VALUE 'R'.               
00043      12  AL-UNDOF            PIC X       VALUE '*'.               
00044      12  AL-UNDON            PIC X       VALUE ')'.               
00045      12  AL-UNNOF            PIC X       VALUE '&'.               
00046      12  AL-UNNON            PIC X       VALUE 'J'.               
00259      EJECT
00260 *    COPY ELCEMIB.
                        
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCEMIB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE            
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *    STANDARD CLAS-IC ERROR MESSAGE COMMUNICATIONS AREA          *
00008 *                                                                *
00009 ******************************************************************
00010  01  ERROR-MESSAGE-INTERFACE-BLOCK.                               
00011      12  EMI-COMM-LENGTH         PIC S9(4)    VALUE +400 COMP.    
00012      12  EMI-NUMBER-OF-LINES     PIC 9        VALUE 1.            
00013      12  EMI-ERROR               PIC 9(4)     VALUE ZEROS.        
00014      12  EMI-SUB                 PIC 99       VALUE 1 COMP-3.     
00015      12  EMI-NOTE-CTR            PIC 999      VALUE 0 COMP-3.     
00016      12  EMI-WARNING-CTR         PIC 999      VALUE 0 COMP-3.     
00017      12  EMI-FORCABLE-CTR        PIC 999      VALUE 0 COMP-3.     
00018      12  EMI-FATAL-CTR           PIC 999      VALUE 0 COMP-3.     
00019      12  EMI-SWITCH1             PIC X        VALUE '1'.          
00020          88  EMI-NO-ERRORS                    VALUE '1'.          
00021          88  EMI-ERRORS-NOT-COMPLETE          VALUE '2'.          
00022          88  EMI-ERRORS-COMPLETE              VALUE '3'.          
00023      12  EMI-SWITCH2             PIC X        VALUE '1'.          
00024          88  EMI-FORMAT-CODES-ONLY            VALUE '2'.          
00025      12  EMI-SWITCH-AREA-1       PIC X        VALUE '1'.          
00026          88  EMI-AREA1-EMPTY                  VALUE '1'.          
00027          88  EMI-AREA1-FULL                   VALUE '2'.          
00028      12  EMI-SWITCH-AREA-2       PIC X        VALUE '1'.          
00029          88  EMI-AREA2-EMPTY                  VALUE '1'.          
00030          88  EMI-AREA2-FULL                   VALUE '2'.          
00031      12  EMI-ACTION-SWITCH       PIC X        VALUE ' '.          
00032          88  EMI-PROCESS-ALL-ERRORS           VALUE ' '.          
00033          88  EMI-BYPASS-NOTES                 VALUE 'N'.          
00034          88  EMI-BYPASS-WARNINGS              VALUE 'W'.          
00035          88  EMI-BYPASS-FORCABLES             VALUE 'F'.          
00036          88  EMI-BYPASS-FATALS                VALUE 'X'.          
00037      12  EMI-ERROR-LINES.                                         
00038          16  EMI-LINE1           PIC X(72)   VALUE SPACES.        
00039          16  EMI-LINE2           PIC X(72)   VALUE SPACES.        
00040          16  EMI-LINE3           PIC X(72)   VALUE SPACES.        
00041          16  EMI-CODE-LINE REDEFINES EMI-LINE3.                   
00042              20  EMI-ERR-CODES OCCURS 10 TIMES.                   
00043                  24  EMI-ERR-NUM         PIC X(4).                
00044                  24  EMI-FILLER          PIC X.                   
00045                  24  EMI-SEV             PIC X.                   
00046                  24  FILLER              PIC X.                   
00047              20  FILLER                  PIC X(02).               
00048      12  EMI-ERR-LINES REDEFINES EMI-ERROR-LINES.                 
00049          16  EMI-MESSAGE-AREA OCCURS 3 TIMES INDEXED BY EMI-INDX. 
00050              20  EMI-ERROR-NUMBER    PIC X(4).                    
00051              20  EMI-FILL            PIC X.                       
00052              20  EMI-SEVERITY        PIC X.                       
00053              20  FILLER              PIC X.                       
00054              20  EMI-ERROR-TEXT.                                  
00055                  24  EMI-TEXT-VARIABLE   PIC X(10).               
00056                  24  FILLER          PIC X(55).                   
00057      12  EMI-SEVERITY-SAVE           PIC X.                       
00058          88  EMI-NOTE                    VALUE 'N'.               
00059          88  EMI-WARNING                 VALUE 'W'.               
00060          88  EMI-FORCABLE                VALUE 'F'.               
00061          88  EMI-FATAL                   VALUE 'X'.               
00062      12  EMI-MESSAGE-FLAG            PIC X.                       
00063          88  EMI-MESSAGE-FORMATTED       VALUE 'Y'.               
00064          88  EMI-NO-MESSAGE-FORMATTED    VALUE 'N'.               
00065      12  EMI-ROLL-SWITCH             PIC X       VALUE SPACES.    
00066      12  EMI-LANGUAGE-IND            PIC X       VALUE SPACES.    
00067          88  EMI-LANGUAGE-IS-FR                  VALUE 'F'.       
00068          88  EMI-LANGUAGE-IS-ENG                 VALUE 'E'.       
00069          88  EMI-LANGUAGE-IS-SPAN                VALUE 'S'.       
00070      12  FILLER                      PIC X(137)  VALUE SPACES.    
00071      12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.    
00072      12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.    
00073      12  EMI-LIFE-OVERRIDE-L6        PIC X(6).                    
00074      12  EMI-AH-OVERRIDE-L6          PIC X(6).                    
00262  01  WS-ERROR-MESSAGE-AREA.
00263      12  ER-0000                 PIC 9(4)   VALUE 0000.
00264      12  ER-0004                 PIC 9(4)   VALUE 0004.
00265      12  ER-0007                 PIC 9(4)   VALUE 0007.
00266      12  ER-0008                 PIC 9(4)   VALUE 0008.
00267      12  ER-0021                 PIC 9(4)   VALUE 0021.
00268      12  ER-0029                 PIC 9(4)   VALUE 0029.
00269      12  ER-0042                 PIC 9(4)   VALUE 0042.
00270      12  ER-0043                 PIC 9(4)   VALUE 0043.
00271      12  ER-0052                 PIC 9(4)   VALUE 0052.
00272      12  ER-0053                 PIC 9(4)   VALUE 0053.
00273      12  ER-0054                 PIC 9(4)   VALUE 0054.
00274      12  ER-0055                 PIC 9(4)   VALUE 0055.
00275      12  ER-0056                 PIC 9(4)   VALUE 0056.
00276      12  ER-0060                 PIC 9(4)   VALUE 0060.
00277      12  ER-0062                 PIC 9(4)   VALUE 0062.
00278      12  ER-0063                 PIC 9(4)   VALUE 0063.
00279      12  ER-0068                 PIC 9(4)   VALUE 0068.
00280      12  ER-0070                 PIC 9(4)   VALUE 0070.
00281      12  ER-0071                 PIC 9(4)   VALUE 0071.
00282      12  ER-0072                 PIC 9(4)   VALUE 0072.
00283      12  ER-0089                 PIC 9(4)   VALUE 0089.
00284      12  ER-0228                 PIC 9(4)   VALUE 0228.
00285      12  ER-0248                 PIC 9(4)   VALUE 0248.
00286      12  ER-0314                 PIC 9(4)   VALUE 0314.
00287      12  ER-0340                 PIC 9(4)   VALUE 0340.
00288      12  ER-0451                 PIC 9(4)   VALUE 0451.
00289      12  ER-0496                 PIC 9(4)   VALUE 0496.
00290      12  ER-0587                 PIC 9(4)   VALUE 0587.
00291      12  ER-0600                 PIC 9(4)   VALUE 0600.
00292      12  ER-0601                 PIC 9(4)   VALUE 0601.
00293      12  ER-0602                 PIC 9(4)   VALUE 0602.
00294      12  ER-0603                 PIC 9(4)   VALUE 0603.
00295      12  ER-0604                 PIC 9(4)   VALUE 0604.
00296      12  ER-0605                 PIC 9(4)   VALUE 0605.
00297      12  ER-0606                 PIC 9(4)   VALUE 0606.
00298      12  ER-0607                 PIC 9(4)   VALUE 0607.
00299      12  ER-0608                 PIC 9(4)   VALUE 0608.
00300      12  ER-0610                 PIC 9(4)   VALUE 0610.
00301      12  ER-0611                 PIC 9(4)   VALUE 0611.
00302      12  ER-0612                 PIC 9(4)   VALUE 0612.
00303      12  ER-0613                 PIC 9(4)   VALUE 0613.
00304      12  ER-0615                 PIC 9(4)   VALUE 0615.
00305      12  ER-0631                 PIC 9(4)   VALUE 0631.
00306      12  ER-0633                 PIC 9(4)   VALUE 0633.
00307      12  ER-0634                 PIC 9(4)   VALUE 0634.
00308      12  ER-0635                 PIC 9(4)   VALUE 0635.
00309      12  ER-0680                 PIC 9(4)   VALUE 0680.
00310      12  ER-0762                 PIC 9(4)   VALUE 0762.
00311      12  ER-0771                 PIC 9(4)   VALUE 0771.
00312      12  ER-1887                 PIC 9(4)   VALUE 1887.
00313      12  ER-1888                 PIC 9(4)   VALUE 1888.
00314      12  ER-1890                 PIC 9(4)   VALUE 1890.
00315      12  ER-2001                 PIC 9(4)   VALUE 2001.
00316      12  ER-2003                 PIC 9(4)   VALUE 2003.
00317      12  ER-2004                 PIC 9(4)   VALUE 2004.
00318      12  ER-2010                 PIC 9(4)   VALUE 2010.
00319      12  ER-2011                 PIC 9(4)   VALUE 2011.
00320      12  ER-2012                 PIC 9(4)   VALUE 2012.
00321      12  ER-2013                 PIC 9(4)   VALUE 2013.
00322      12  ER-2014                 PIC 9(4)   VALUE 2014.
00323      12  ER-2015                 PIC 9(4)   VALUE 2015.
00324      12  ER-2016                 PIC 9(4)   VALUE 2016.
00325      12  ER-2017                 PIC 9(4)   VALUE 2017.
00326      12  ER-2018                 PIC 9(4)   VALUE 2018.
00327      12  ER-2019                 PIC 9(4)   VALUE 2019.
00328      12  ER-2020                 PIC 9(4)   VALUE 2020.
00329      12  ER-2021                 PIC 9(4)   VALUE 2021.
00330      12  ER-2022                 PIC 9(4)   VALUE 2022.
00331      12  ER-2023                 PIC 9(4)   VALUE 2023.
00332      12  ER-2024                 PIC 9(4)   VALUE 2024.
00333      12  ER-2028                 PIC 9(4)   VALUE 2028.
00334      12  ER-2029                 PIC 9(4)   VALUE 2029.
00335      12  ER-2030                 PIC 9(4)   VALUE 2030.
00336      12  ER-2034                 PIC 9(4)   VALUE 2034.
00337      12  ER-2035                 PIC 9(4)   VALUE 2035.
00338      12  ER-2036                 PIC 9(4)   VALUE 2036.
00339      12  ER-2037                 PIC 9(4)   VALUE 2037.
00340      12  ER-2357                 PIC 9(4)   VALUE 2357.
00341      12  ER-2549                 PIC 9(4)   VALUE 2549.
00342      12  ER-2518                 PIC 9(4)   VALUE 2518.
00343      12  ER-2565                 PIC 9(4)   VALUE 2565.
00344      12  ER-2610                 PIC 9(4)   VALUE 2610.
00345      12  ER-2904                 PIC 9(4)   VALUE 2904.
00346      12  ER-3035                 PIC 9(4)   VALUE 3035.
00347      12  ER-3143                 PIC 9(4)   VALUE 3143.
00348      12  ER-3156                 PIC 9(4)   VALUE 3156.
00349      12  ER-3187                 PIC 9(4)   VALUE 3187.
00350      12  ER-3736                 PIC 9(4)   VALUE 3736.
00351      12  ER-7099                 PIC 9(4)   VALUE 7099.
00352      12  ER-7219                 PIC 9(4)   VALUE 7219.
00353      12  ER-7228                 PIC 9(4)   VALUE 7228.
00354      12  ER-7229                 PIC 9(4)   VALUE 7229.
00355      12  ER-7355                 PIC 9(4)   VALUE 7355.
00356      12  ER-7356                 PIC 9(4)   VALUE 7356.
00357      12  ER-7380                 PIC 9(4)   VALUE 7380.
00358      12  ER-7532                 PIC 9(4)   VALUE 7532.
00359      12  ER-7533                 PIC 9(4)   VALUE 7533.
00360      12  ER-7692                 PIC 9(4)   VALUE 7692.
00361      12  ER-7720                 PIC 9(4)   VALUE 7720.
00362      12  ER-7721                 PIC 9(4)   VALUE 7721.
00363      12  ER-7722                 PIC 9(4)   VALUE 7722.
00364      12  ER-7723                 PIC 9(4)   VALUE 7723.
00365      12  ER-7724                 PIC 9(4)   VALUE 7724.
00366      12  ER-7735                 PIC 9(4)   VALUE 7735.
00367      12  ER-7738                 PIC 9(4)   VALUE 7738.
00368      12  ER-7739                 PIC 9(4)   VALUE 7739.
00369      12  ER-7740                 PIC 9(4)   VALUE 7740.
00370      12  ER-7741                 PIC 9(4)   VALUE 7741.
00371      12  ER-7742                 PIC 9(4)   VALUE 7742.
00372      12  ER-7744                 PIC 9(4)   VALUE 7744.
00373      12  ER-8129                 PIC 9(4)   VALUE 8129.
00374      12  ER-8130                 PIC 9(4)   VALUE 8130.
00375      12  ER-8132                 PIC 9(4)   VALUE 8132.
00376      12  ER-9001                 PIC 9(4)   VALUE 9001.
00377      12  ER-9192                 PIC 9(4)   VALUE 9192.
00378      12  ER-9193                 PIC 9(4)   VALUE 9193.
00379      12  ER-9194                 PIC 9(4)   VALUE 9194.
00380      12  ER-9322                 PIC 9(4)   VALUE 9322.
00381      12  ER-9417                 PIC 9(4)   VALUE 9417.
00382      12  ER-9446                 PIC 9(4)   VALUE 9446.
00383      12  ER-9460                 PIC 9(4)   VALUE 9460.
00384      12  ER-9533                 PIC 9(4)   VALUE 9533.
00385      12  ER-9576                 PIC 9(4)   VALUE 9576.
00386      12  ER-9598                 PIC 9(4)   VALUE 9598.
00387      12  ER-9790                 PIC 9(4)   VALUE 9790.
00388      12  ER-9791                 PIC 9(4)   VALUE 9791.
00389      12  ER-9909                 PIC 9(4)   VALUE 9909.
00390      EJECT
00391 *    COPY ELCINTF.
                        
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCINTF.                            *
00004 *                            VMOD=2.017                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON DATA AREA                 *
00007 *                                                                *
00008 *       LENGTH = 1024                                            *
00009 *                                                                *
00010 ******************************************************************
00011  01  PROGRAM-INTERFACE-BLOCK.                                     
00012      12  PI-COMM-LENGTH                PIC S9(4) COMP VALUE +1024.
00013      12  PI-CALLING-PROGRAM              PIC X(8).                
00014      12  PI-SAVED-PROGRAM-1              PIC X(8).                
00015      12  PI-SAVED-PROGRAM-2              PIC X(8).                
00016      12  PI-SAVED-PROGRAM-3              PIC X(8).                
00017      12  PI-SAVED-PROGRAM-4              PIC X(8).                
00018      12  PI-SAVED-PROGRAM-5              PIC X(8).                
00019      12  PI-SAVED-PROGRAM-6              PIC X(8).                
00020      12  PI-RETURN-TO-PROGRAM            PIC X(8).                
00021      12  PI-COMPANY-ID                   PIC XXX.                 
00022      12  PI-COMPANY-CD                   PIC X.                   
00023                                                                   
00024      12  PI-COMPANY-PASSWORD             PIC X(8).                
00025                                                                   
00026      12  PI-JOURNAL-FILE-ID              PIC S9(4) COMP.          
00027                                                                   
00028      12  PI-CONTROL-IN-PROGRESS.                                  
00029          16  PI-CARRIER                  PIC X.                   
00030          16  PI-GROUPING                 PIC X(6).                
00031          16  PI-STATE                    PIC XX.                  
00032          16  PI-ACCOUNT                  PIC X(10).               
00033          16  PI-PRODUCER REDEFINES PI-ACCOUNT                     
00034                                          PIC X(10).               
00035          16  PI-CLAIM-CERT-GRP.                                   
00036              20  PI-CLAIM-NO             PIC X(7).                
00037              20  PI-CERT-NO.                                      
00038                  25  PI-CERT-PRIME       PIC X(10).               
00039                  25  PI-CERT-SFX         PIC X.                   
00040              20  PI-CERT-EFF-DT          PIC XX.                  
00041          16  PI-PLAN-DATA REDEFINES PI-CLAIM-CERT-GRP.            
00042              20  PI-PLAN-CODE            PIC X(2).                
00043              20  PI-REVISION-NUMBER      PIC X(3).                
00044              20  PI-PLAN-EFF-DT          PIC X(2).                
00045              20  PI-PLAN-EXP-DT          PIC X(2).                
00046              20  FILLER                  PIC X(11).               
00047          16  PI-OE-REFERENCE-1 REDEFINES PI-CLAIM-CERT-GRP.       
00048              20  PI-OE-REFERENCE-1.                               
00049                  25  PI-OE-REF-1-PRIME   PIC X(18).               
00050                  25  PI-OE-REF-1-SUFF    PIC XX.                  
00051                                                                   
00052      12  PI-SESSION-IN-PROGRESS          PIC X.                   
00053          88  CLAIM-SESSION                   VALUE '1'.           
00054          88  CREDIT-SESSION                  VALUE '2'.           
00055          88  WARRANTY-SESSION                VALUE '3'.           
00056          88  MORTGAGE-SESSION                VALUE '4'.           
00057          88  GENERAL-LEDGER-SESSION          VALUE '5'.           
00058                                                                   
00059                                                                   
00060 *THE FOLLOWING TWO FIELDS ARE USED ONLY WITH MULTI COMPANY CLIENTS
00061                                                                   
00062      12  PI-ORIGINAL-COMPANY-ID          PIC X(3).                
00063      12  PI-ORIGINAL-COMPANY-CD          PIC X.                   
00064                                                                   
00065      12  PI-CREDIT-USER                  PIC X.                   
00066          88  PI-NOT-CREDIT-USER              VALUE 'N'.           
00067          88  PI-HAS-CLAS-IC-CREDIT           VALUE 'Y'.           
00068                                                                   
00069      12  PI-CLAIM-USER                   PIC X.                   
00070          88  PI-NOT-CLAIM-USER               VALUE 'N'.           
00071          88  PI-HAS-CLAS-IC-CLAIM            VALUE 'Y'.           
00072                                                                   
00073      12  PI-PROCESSOR-SYS-ACCESS         PIC X.                   
00074          88  PI-ACCESS-TO-BOTH-SYSTEMS       VALUE ' '.           
00075          88  PI-ACCESS-TO-ALL-SYSTEMS        VALUE ' '.           
00076          88  PI-ACCESS-TO-CLAIM-ONLY         VALUE '1'.           
00077          88  PI-ACCESS-TO-CREDIT-ONLY        VALUE '2'.           
00078          88  PI-ACCESS-TO-MORTGAGE-ONLY      VALUE '3'.           
00079                                                                   
00080      12  PI-PROCESSOR-ID                 PIC X(4).                
00081                                                                   
00082      12  PI-PROCESSOR-PASSWORD           PIC X(11).               
00083                                                                   
00084      12  PI-MEMBER-CAPTION               PIC X(10).               
00085                                                                   
00086      12  PI-PROCESSOR-USER-ALMIGHTY      PIC X.                   
00087          88  PI-USER-ALMIGHTY-YES            VALUE 'Y'.           
00088                                                                   
00089      12  PI-LIFE-OVERRIDE-L1             PIC X.                   
00090      12  PI-LIFE-OVERRIDE-L2             PIC XX.                  
00091      12  PI-LIFE-OVERRIDE-L6             PIC X(6).                
00092      12  PI-LIFE-OVERRIDE-L12            PIC X(12).               
00093                                                                   
00094      12  PI-AH-OVERRIDE-L1               PIC X.                   
00095      12  PI-AH-OVERRIDE-L2               PIC XX.                  
00096      12  PI-AH-OVERRIDE-L6               PIC X(6).                
00097      12  PI-AH-OVERRIDE-L12              PIC X(12).               
00098                                                                   
00099      12  PI-NEW-SYSTEM                   PIC X(2).                
00100                                                                   
00101      12  PI-PRIMARY-CERT-NO              PIC X(11).               
00102      12  PI-CLAIM-PAID-THRU-TO           PIC X(01).               
00103          88  PI-USES-PAID-TO                 VALUE '1'.           
00104      12  PI-CRDTCRD-SYSTEM.                                       
00105          16  PI-CRDTCRD-USER             PIC X.                   
00106              88  PI-NOT-CRDTCRD-USER         VALUE 'N'.           
00107              88  PI-HAS-CLAS-IC-CRDTCRD      VALUE 'Y'.           
00108          16  PI-CC-MONTH-END-DT          PIC XX.                  
00109      12  PI-PROCESSOR-PRINTER            PIC X(4).                
00110                                                                   
00111      12  PI-OE-REFERENCE-2.                                       
00112          16  PI-OE-REF-2-PRIME           PIC X(10).               
00113          16  PI-OE-REF-2-SUFF            PIC X.                   
00114                                                                   
00115      12  PI-REM-TRM-CALC-OPTION          PIC X.                   
00116                                                                   
00117      12  PI-LANGUAGE-TYPE                PIC X.                   
00118              88  PI-LANGUAGE-IS-ENG          VALUE 'E'.           
00119              88  PI-LANGUAGE-IS-FR           VALUE 'F'.           
00120              88  PI-LANGUAGE-IS-SPAN         VALUE 'S'.           
00121                                                                   
00122      12  PI-POLICY-LINKAGE-IND           PIC X.                   
00123          88  PI-USE-POLICY-LINKAGE           VALUE 'Y'.           
00124          88  PI-POLICY-LINKAGE-NOT-USED      VALUE 'N'            
00125                                                    LOW-VALUES.    
00126                                                                   
00127      12  PI-ALT-DMD-PRT-ID               PIC X(4).                
00128      12  PI-CLAIM-PW-SESSION             PIC X(1).                
00129          88  PI-CLAIM-CREDIT                 VALUE '1'.           
00130          88  PI-CLAIM-CONVEN                 VALUE '2'.           
00131      12  FILLER                          PIC X(4).                
00132                                                                   
00133      12  PI-SYSTEM-LEVEL                 PIC X(145).              
00134                                                                   
00135      12  PI-CLAIMS-CREDIT-LEVEL          REDEFINES                
00136          PI-SYSTEM-LEVEL.                                         
00137                                                                   
00138          16  PI-ENTRY-CODES.                                      
00139              20  PI-ENTRY-CD-1           PIC X.                   
00140              20  PI-ENTRY-CD-2           PIC X.                   
00141                                                                   
00142          16  PI-RETURN-CODES.                                     
00143              20  PI-RETURN-CD-1          PIC X.                   
00144              20  PI-RETURN-CD-2          PIC X.                   
00145                                                                   
00146          16  PI-UPDATE-STATUS-SAVE.                               
00147              20  PI-UPDATE-BY            PIC X(4).                
00148              20  PI-UPDATE-HHMMSS        PIC S9(7)     COMP-3.    
00149                                                                   
00150          16  PI-LOWER-CASE-LETTERS       PIC X.                   
00151              88  LOWER-CASE-LETTERS-USED     VALUE 'Y'.           
00152                                                                   
00153 *        16  PI-CLAIM-ACCESS-CONTROL     PIC X.                   
00154 *            88  CLAIM-NO-UNIQUE             VALUE '1'.           
00155 *            88  CARRIER-CLM-CNTL            VALUE '2'.           
00156                                                                   
00157          16  PI-CERT-ACCESS-CONTROL      PIC X.                   
00158              88  ST-ACCNT-CNTL               VALUE ' '.           
00159              88  CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.           
00160              88  CARR-ST-ACCNT-CNTL          VALUE '2'.           
00161              88  ACCNT-CNTL                  VALUE '3'.           
00162              88  CARR-ACCNT-CNTL             VALUE '4'.           
00163                                                                   
00164          16  PI-PROCESSOR-CAP-LIST.                               
00165              20  PI-SYSTEM-CONTROLS.                              
00166                 24 PI-SYSTEM-DISPLAY     PIC X.                   
00167                  88  SYSTEM-DISPLAY-CAP      VALUE 'Y'.           
00168                 24 PI-SYSTEM-MODIFY      PIC X.                   
00169                  88  SYSTEM-MODIFY-CAP       VALUE 'Y'.           
00170              20  FILLER                  PIC XX.                  
00171              20  PI-DISPLAY-CAP          PIC X.                   
00172                  88  DISPLAY-CAP             VALUE 'Y'.           
00173              20  PI-MODIFY-CAP           PIC X.                   
00174                  88  MODIFY-CAP              VALUE 'Y'.           
00175              20  PI-MSG-AT-LOGON-CAP     PIC X.                   
00176                  88  MSG-AT-LOGON-CAP        VALUE 'Y'.           
00177              20  PI-FORCE-CAP            PIC X.                   
00178                  88  FORCE-CAP               VALUE 'Y'.           
00179                                                                   
00180          16  PI-PROGRAM-CONTROLS.                                 
00181              20  PI-PGM-PRINT-OPT        PIC X.                   
00182              20  PI-PGM-FORMAT-OPT       PIC X.                   
00183              20  PI-PGM-PROCESS-OPT      PIC X.                   
00184              20  PI-PGM-TOTALS-OPT       PIC X.                   
00185                                                                   
00186          16  PI-HELP-INTERFACE.                                   
00187              20  PI-LAST-ERROR-NO        PIC X(4).                
00188              20  PI-CURRENT-SCREEN-NO    PIC X(4).                
00189                                                                   
00190          16  PI-CARRIER-CONTROL-LEVEL    PIC X.                   
00191              88  CONTROL-IS-ACTUAL-CARRIER   VALUE SPACE.         
00192                                                                   
00193          16  PI-CR-CONTROL-IN-PROGRESS.                           
00194              20  PI-CR-CARRIER           PIC X.                   
00195              20  PI-CR-GROUPING          PIC X(6).                
00196              20  PI-CR-STATE             PIC XX.                  
00197              20  PI-CR-ACCOUNT           PIC X(10).               
00198              20  PI-CR-FIN-RESP          PIC X(10).               
00199              20  PI-CR-TYPE              PIC X.                   
00200                                                                   
00201          16  PI-CR-BATCH-NUMBER          PIC X(6).                
00202                                                                   
00203          16  PI-CR-MONTH-END-DT          PIC XX.                  
00204                                                                   
00205          16  PI-CAR-GROUP-ACCESS-CNTL    PIC X.                   
00206              88  PI-USE-ACTUAL-CARRIER       VALUE ' '.           
00207              88  PI-ZERO-CARRIER             VALUE '1'.           
00208              88  PI-ZERO-GROUPING            VALUE '2'.           
00209              88  PI-ZERO-CAR-GROUP           VALUE '3'.           
00210                                                                   
00211          16  PI-CARRIER-SECURITY         PIC X.                   
00212              88  PI-NO-CARRIER-SECURITY      VALUE ' '.           
00213                                                                   
00214          16  PI-ACCOUNT-SECURITY         PIC X(10).               
00215              88  PI-NO-ACCOUNT-SECURITY      VALUE SPACES.        
00216              88  PI-NO-PRODUCER-SECURITY     VALUE SPACES.        
00217                                                                   
00218          16  PI-CODE-SECURITY REDEFINES PI-ACCOUNT-SECURITY.      
00219              20  PI-ACCESS-CODE          OCCURS 10 TIMES          
00220                                          INDEXED BY PI-ACCESS-NDX 
00221                                          PIC X.                   
00222                                                                   
00223          16  PI-GA-BILLING-CONTROL       PIC X.                   
00224              88  PI-GA-BILLING               VALUE '1'.           
00225                                                                   
00226          16  PI-MAIL-PROCESSING          PIC X.                   
00227              88  PI-MAIL-YES                 VALUE 'Y'.           
00228                                                                   
00229          16  PI-SECURITY-TEMP-STORE-ID   PIC X(8).                
00230                                                                   
00231          16  PI-AR-SYSTEM.                                        
00232              20  PI-AR-PROCESSING-CNTL   PIC X.                   
00233                  88  PI-AR-PROCESSING        VALUE 'Y'.           
00234              20  PI-AR-SUMMARY-CODE      PIC X(6).                
00235              20  PI-AR-MONTH-END-DT      PIC XX.                  
00236                                                                   
00237          16  PI-MP-SYSTEM.                                        
00238              20  PI-MORTGAGE-USER            PIC X.               
00239                  88  PI-NOT-MORTGAGE-USER            VALUE 'N'.   
00240                  88  PI-HAS-CLAS-IC-MORTGAGE         VALUE 'Y'.   
00241              20  PI-MORTGAGE-ACCESS-CONTROL  PIC X.               
00242                  88  PI-MP-ST-PROD-CNTL              VALUE ' '.   
00243                  88  PI-MP-CARR-GRP-ST-PROD-CNTL     VALUE '1'.   
00244                  88  PI-MP-CARR-ST-PROD-CNTL         VALUE '2'.   
00245                  88  PI-MP-PROD-CNTL                 VALUE '3'.   
00246                  88  PI-MP-CARR-PROD-CNTL            VALUE '4'.   
00247              20  PI-MP-MONTH-END-DT          PIC XX.              
00248              20  PI-MP-REFERENCE-NO.                              
00249                  24  PI-MP-REFERENCE-PRIME   PIC X(18).           
00250                  24  PI-MP-REFERENCE-SFX     PIC XX.              
00251                                                                   
00252          16  PI-LABEL-CONTROL            PIC X(01).               
00253              88  PI-CREATE-LABELS                    VALUE 'Y'.   
00254              88  PI-BYPASS-LABELS                    VALUE 'N'.   
00255                                                                   
00256          16  PI-BILL-GROUPING-CODE       PIC X(01).               
00257              88  PI-CO-HAS-BILL-GROUPING             VALUE 'Y'.   
00258                                                                   
00259          16  PI-RATE-DEV-AUTHORIZATION   PIC X(01).               
00260              88  PI-RATE-DEV-AUTHORIZED              VALUE 'Y'.   
00261              88  PI-RATE-DEV-NOT-AUTHORIZED          VALUE 'N'.   
00262                                                                   
00263          16  FILLER                      PIC X(14).               
00264                                                                   
00265      12  PI-PROGRAM-WORK-AREA            PIC X(640).              
00266 ******************************************************************
00392      12 FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00393         16 PI-MAP-NAME                 PIC X(8).
00394         16 PI-MORTG-CO-UPDATE-BY       PIC X(4).
00395         16 PI-MORTG-CO-UPDATE-HHMMSS   PIC S9(7)   COMP-3.
00396         16 PI-OPTIONAL-RESERVE-SW      PIC X.
00397         16 FILLER                      PIC X(623).
00398      EJECT
00399 *    COPY ELCJPFX.
                        
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCJPFX.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE            
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *    USER DATA FOR SYSTEM JOURNAL RECORDS  JOURNAL I.D. = "EL"   *
00008 *                                                                *
00009 *     ALL RECORDS ARE JOURNALED FOR ERROR RECOVERY               *
00010 *     FILES JOURNALED FOR AUDIT TRAIL (BEFORE CHANGE) ARE -      *
00011 *        ELCNTL - CONTROL FILE                                   *
00012 *        ELMSTR - CLAIM MASTERS                                  *
00013 *        ELTRLR - ACTIVITY TRAILERS                              *
00014 *        ELCHKQ - CHECK QUE                                      *
00015 ******************************************************************
00016  01  JOURNAL-RECORD.                                              
00017      12  JP-USER-ID                  PIC X(4).                    
00018      12  JP-FILE-ID                  PIC X(8).                    
00019      12  JP-PROGRAM-ID               PIC X(8).                    
00020      12  JP-RECORD-TYPE              PIC X.                       
00021          88 JP-ADD              VALUE 'A'.                        
00022          88 JP-BEFORE-CHANGE    VALUE 'B'.                        
00023          88 JP-AFTER-CHANGE     VALUE 'C'.                        
00024          88 JP-DELETE           VALUE 'D'.                        
00025          88 JP-GENERIC-DELETE   VALUE 'G'.                        
00026          88 JP-KEY-CHG-DELETE   VALUE 'K'.                        
00027          88 JP-KEY-CHG-ADD      VALUE 'N'.                        
00028      12  JP-GENERIC-KEY-LENGTH       PIC S9(4)   COMP.            
00029      12  JP-RECORD-AREA                                           
00030                                                                   
00031                                                                   
00400                              PIC X(750).
00402      EJECT
00403 *    COPY ELCAID.
                       
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCAID.                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION:  ATTENTION IDENTIFER CHARACTERS.                *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCAID                           *
00007 ******************************************************************
00008                                                                   
00009  01  DFHAID.                                                      
00010    02  DFHNULL   PIC  X  VALUE  ' '.                              
00011    02  DFHENTER  PIC  X  VALUE  QUOTE.                            
00012    02  DFHCLEAR  PIC  X  VALUE  '_'.                              
00013    02  DFHPEN    PIC  X  VALUE  '='.                              
00014    02  DFHOPID   PIC  X  VALUE  'W'.                              
00015    02  DFHPA1    PIC  X  VALUE  '%'.                              
00016    02  DFHPA2    PIC  X  VALUE  '>'.                              
00017    02  DFHPA3    PIC  X  VALUE  ','.                              
00018    02  DFHPF1    PIC  X  VALUE  '1'.                              
00019    02  DFHPF2    PIC  X  VALUE  '2'.                              
00020    02  DFHPF3    PIC  X  VALUE  '3'.                              
00021    02  DFHPF4    PIC  X  VALUE  '4'.                              
00022    02  DFHPF5    PIC  X  VALUE  '5'.                              
00023    02  DFHPF6    PIC  X  VALUE  '6'.                              
00024    02  DFHPF7    PIC  X  VALUE  '7'.                              
00025    02  DFHPF8    PIC  X  VALUE  '8'.                              
00026    02  DFHPF9    PIC  X  VALUE  '9'.                              
00027    02  DFHPF10   PIC  X  VALUE  ':'.                              
00028    02  DFHPF11   PIC  X  VALUE  '#'.                              
00029    02  DFHPF12   PIC  X  VALUE  '@'.                              
00030    02  DFHPF13   PIC  X  VALUE  'A'.                              
00031    02  DFHPF14   PIC  X  VALUE  'B'.                              
00032    02  DFHPF15   PIC  X  VALUE  'C'.                              
00033    02  DFHPF16   PIC  X  VALUE  'D'.                              
00034    02  DFHPF17   PIC  X  VALUE  'E'.                              
00035    02  DFHPF18   PIC  X  VALUE  'F'.                              
00036    02  DFHPF19   PIC  X  VALUE  'G'.                              
00037    02  DFHPF20   PIC  X  VALUE  'H'.                              
00038    02  DFHPF21   PIC  X  VALUE  'I'.                              
00039    02  DFHPF22   PIC  X  VALUE  'Õ'.                              
00040    02  DFHPF23   PIC  X  VALUE  '.'.                              
00041    02  DFHPF24   PIC  X  VALUE  '<'.                              
00042    02  DFHMSRE   PIC  X  VALUE  'X'.                              
00043    02  DFHSTRF   PIC  X  VALUE  'h'.                              
00044    02  DFHTRIG   PIC  X  VALUE  '"'.                              
00404  01  FILLER    REDEFINES DFHAID.
00405      12  FILLER              PIC X(8).
00406      12  PF-VALUES           PIC X       OCCURS 2.
00408  01  WS-WORK-AREA-2          PIC X(100) VALUE SPACES.
00410      EJECT
00411 *    COPY EL102S.
                       
       01  EL102AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  RUNDTEL PIC S9(0004) COMP.
           05  RUNDTEF PIC  X(0001).
           05  FILLER REDEFINES RUNDTEF.
               10  RUNDTEA PIC  X(0001).
           05  RUNDTEI PIC  X(0008).
      *    -------------------------------
           05  RUNTIMEL PIC S9(0004) COMP.
           05  RUNTIMEF PIC  X(0001).
           05  FILLER REDEFINES RUNTIMEF.
               10  RUNTIMEA PIC  X(0001).
           05  RUNTIMEI PIC  X(0005).
      *    -------------------------------
           05  COAL PIC S9(0004) COMP.
           05  COAF PIC  X(0001).
           05  FILLER REDEFINES COAF.
               10  COAA PIC  X(0001).
           05  COAI PIC  X(0003).
      *    -------------------------------
           05  USERIDAL PIC S9(0004) COMP.
           05  USERIDAF PIC  X(0001).
           05  FILLER REDEFINES USERIDAF.
               10  USERIDAA PIC  X(0001).
           05  USERIDAI PIC  X(0004).
      *    -------------------------------
           05  COMPNMEL PIC S9(0004) COMP.
           05  COMPNMEF PIC  X(0001).
           05  FILLER REDEFINES COMPNMEF.
               10  COMPNMEA PIC  X(0001).
           05  COMPNMEI PIC  X(0030).
      *    -------------------------------
           05  COMPIDL PIC S9(0004) COMP.
           05  COMPIDF PIC  X(0001).
           05  FILLER REDEFINES COMPIDF.
               10  COMPIDA PIC  X(0001).
           05  COMPIDI PIC  X(0003).
      *    -------------------------------
           05  INCAREL PIC S9(0004) COMP.
           05  INCAREF PIC  X(0001).
           05  FILLER REDEFINES INCAREF.
               10  INCAREA PIC  X(0001).
           05  INCAREI PIC  X(0030).
      *    -------------------------------
           05  COMPCDL PIC S9(0004) COMP.
           05  COMPCDF PIC  X(0001).
           05  FILLER REDEFINES COMPCDF.
               10  COMPCDA PIC  X(0001).
           05  COMPCDI PIC  X(0004).
      *    -------------------------------
           05  ADDR1L PIC S9(0004) COMP.
           05  ADDR1F PIC  X(0001).
           05  FILLER REDEFINES ADDR1F.
               10  ADDR1A PIC  X(0001).
           05  ADDR1I PIC  X(0030).
      *    -------------------------------
           05  PASSWDL PIC S9(0004) COMP.
           05  PASSWDF PIC  X(0001).
           05  FILLER REDEFINES PASSWDF.
               10  PASSWDA PIC  X(0001).
           05  PASSWDI PIC  X(0008).
      *    -------------------------------
           05  ADDR2L PIC S9(0004) COMP.
           05  ADDR2F PIC  X(0001).
           05  FILLER REDEFINES ADDR2F.
               10  ADDR2A PIC  X(0001).
           05  ADDR2I PIC  X(0030).
      *    -------------------------------
           05  NEXTCOL PIC S9(0004) COMP.
           05  NEXTCOF PIC  X(0001).
           05  FILLER REDEFINES NEXTCOF.
               10  NEXTCOA PIC  X(0001).
           05  NEXTCOI PIC  X(0003).
      *    -------------------------------
           05  CITYSTL PIC S9(0004) COMP.
           05  CITYSTF PIC  X(0001).
           05  FILLER REDEFINES CITYSTF.
               10  CITYSTA PIC  X(0001).
           05  CITYSTI PIC  X(0030).
      *    -------------------------------
           05  TAXIDL PIC S9(0004) COMP.
           05  TAXIDF PIC  X(0001).
           05  FILLER REDEFINES TAXIDF.
               10  TAXIDA PIC  X(0001).
           05  TAXIDI PIC  X(0011).
      *    -------------------------------
           05  ZIPCODEL PIC S9(0004) COMP.
           05  ZIPCODEF PIC  X(0001).
           05  FILLER REDEFINES ZIPCODEF.
               10  ZIPCODEA PIC  X(0001).
           05  ZIPCODEI PIC  X(0010).
      *    -------------------------------
           05  PHONEL PIC S9(0004) COMP.
           05  PHONEF PIC  X(0001).
           05  FILLER REDEFINES PHONEF.
               10  PHONEA PIC  X(0001).
           05  PHONEI PIC  X(0012).
      *    -------------------------------
           05  CLSYSL PIC S9(0004) COMP.
           05  CLSYSF PIC  X(0001).
           05  FILLER REDEFINES CLSYSF.
               10  CLSYSA PIC  X(0001).
           05  CLSYSI PIC  X(0001).
      *    -------------------------------
           05  MPSYSL PIC S9(0004) COMP.
           05  MPSYSF PIC  X(0001).
           05  FILLER REDEFINES MPSYSF.
               10  MPSYSA PIC  X(0001).
           05  MPSYSI PIC  X(0001).
      *    -------------------------------
           05  RPTCD1L PIC S9(0004) COMP.
           05  RPTCD1F PIC  X(0001).
           05  FILLER REDEFINES RPTCD1F.
               10  RPTCD1A PIC  X(0001).
           05  RPTCD1I PIC  X(0010).
      *    -------------------------------
           05  CRSYSL PIC S9(0004) COMP.
           05  CRSYSF PIC  X(0001).
           05  FILLER REDEFINES CRSYSF.
               10  CRSYSA PIC  X(0001).
           05  CRSYSI PIC  X(0001).
      *    -------------------------------
           05  ARSYSL PIC S9(0004) COMP.
           05  ARSYSF PIC  X(0001).
           05  FILLER REDEFINES ARSYSF.
               10  ARSYSA PIC  X(0001).
           05  ARSYSI PIC  X(0001).
      *    -------------------------------
           05  RPTCD2L PIC S9(0004) COMP.
           05  RPTCD2F PIC  X(0001).
           05  FILLER REDEFINES RPTCD2F.
               10  RPTCD2A PIC  X(0001).
           05  RPTCD2I PIC  X(0010).
      *    -------------------------------
           05  CCSYSL PIC S9(0004) COMP.
           05  CCSYSF PIC  X(0001).
           05  FILLER REDEFINES CCSYSF.
               10  CCSYSA PIC  X(0001).
           05  CCSYSI PIC  X(0001).
      *    -------------------------------
           05  LCCODEL PIC S9(0004) COMP.
           05  LCCODEF PIC  X(0001).
           05  FILLER REDEFINES LCCODEF.
               10  LCCODEA PIC  X(0001).
           05  LCCODEI PIC  X(0001).
      *    -------------------------------
           05  FPIDL PIC S9(0004) COMP.
           05  FPIDF PIC  X(0001).
           05  FILLER REDEFINES FPIDF.
               10  FPIDA PIC  X(0001).
           05  FPIDI PIC  X(0004).
      *    -------------------------------
           05  CPIDL PIC S9(0004) COMP.
           05  CPIDF PIC  X(0001).
           05  FILLER REDEFINES CPIDF.
               10  CPIDA PIC  X(0001).
           05  CPIDI PIC  X(0004).
      *    -------------------------------
           05  EUSYSL PIC S9(0004) COMP.
           05  EUSYSF PIC  X(0001).
           05  FILLER REDEFINES EUSYSF.
               10  EUSYSA PIC  X(0001).
           05  EUSYSI PIC  X(0001).
      *    -------------------------------
           05  SECOPTL PIC S9(0004) COMP.
           05  SECOPTF PIC  X(0001).
           05  FILLER REDEFINES SECOPTF.
               10  SECOPTA PIC  X(0001).
           05  SECOPTI PIC  X(0001).
      *    -------------------------------
           05  JOURNIDL PIC S9(0004) COMP.
           05  JOURNIDF PIC  X(0001).
           05  FILLER REDEFINES JOURNIDF.
               10  JOURNIDA PIC  X(0001).
           05  JOURNIDI PIC  9999.
      *    -------------------------------
           05  MEMBCAPL PIC S9(0004) COMP.
           05  MEMBCAPF PIC  X(0001).
           05  FILLER REDEFINES MEMBCAPF.
               10  MEMBCAPA PIC  X(0001).
           05  MEMBCAPI PIC  X(0010).
      *    -------------------------------
           05  LFDESCL PIC S9(0004) COMP.
           05  LFDESCF PIC  X(0001).
           05  FILLER REDEFINES LFDESCF.
               10  LFDESCA PIC  X(0001).
           05  LFDESCI PIC  X(0019).
      *    -------------------------------
           05  LFOVR1L PIC S9(0004) COMP.
           05  LFOVR1F PIC  X(0001).
           05  FILLER REDEFINES LFOVR1F.
               10  LFOVR1A PIC  X(0001).
           05  LFOVR1I PIC  X(0001).
      *    -------------------------------
           05  LFOVR2L PIC S9(0004) COMP.
           05  LFOVR2F PIC  X(0001).
           05  FILLER REDEFINES LFOVR2F.
               10  LFOVR2A PIC  X(0001).
           05  LFOVR2I PIC  X(0002).
      *    -------------------------------
           05  LFOVR6L PIC S9(0004) COMP.
           05  LFOVR6F PIC  X(0001).
           05  FILLER REDEFINES LFOVR6F.
               10  LFOVR6A PIC  X(0001).
           05  LFOVR6I PIC  X(0006).
      *    -------------------------------
           05  LFOVR12L PIC S9(0004) COMP.
           05  LFOVR12F PIC  X(0001).
           05  FILLER REDEFINES LFOVR12F.
               10  LFOVR12A PIC  X(0001).
           05  LFOVR12I PIC  X(0012).
      *    -------------------------------
           05  LSTUSRL PIC S9(0004) COMP.
           05  LSTUSRF PIC  X(0001).
           05  FILLER REDEFINES LSTUSRF.
               10  LSTUSRA PIC  X(0001).
           05  LSTUSRI PIC  X(0004).
      *    -------------------------------
           05  AHDESCL PIC S9(0004) COMP.
           05  AHDESCF PIC  X(0001).
           05  FILLER REDEFINES AHDESCF.
               10  AHDESCA PIC  X(0001).
           05  AHDESCI PIC  X(0019).
      *    -------------------------------
           05  AHOVR1L PIC S9(0004) COMP.
           05  AHOVR1F PIC  X(0001).
           05  FILLER REDEFINES AHOVR1F.
               10  AHOVR1A PIC  X(0001).
           05  AHOVR1I PIC  X(0001).
      *    -------------------------------
           05  AHOVR2L PIC S9(0004) COMP.
           05  AHOVR2F PIC  X(0001).
           05  FILLER REDEFINES AHOVR2F.
               10  AHOVR2A PIC  X(0001).
           05  AHOVR2I PIC  X(0002).
      *    -------------------------------
           05  AHOVR6L PIC S9(0004) COMP.
           05  AHOVR6F PIC  X(0001).
           05  FILLER REDEFINES AHOVR6F.
               10  AHOVR6A PIC  X(0001).
           05  AHOVR6I PIC  X(0006).
      *    -------------------------------
           05  AHOVR12L PIC S9(0004) COMP.
           05  AHOVR12F PIC  X(0001).
           05  FILLER REDEFINES AHOVR12F.
               10  AHOVR12A PIC  X(0001).
           05  AHOVR12I PIC  X(0012).
      *    -------------------------------
           05  LSTDTEL PIC S9(0004) COMP.
           05  LSTDTEF PIC  X(0001).
           05  FILLER REDEFINES LSTDTEF.
               10  LSTDTEA PIC  X(0001).
           05  LSTDTEI PIC  X(0008).
      *    -------------------------------
           05  LSTTIMEL PIC S9(0004) COMP.
           05  LSTTIMEF PIC  X(0001).
           05  FILLER REDEFINES LSTTIMEF.
               10  LSTTIMEA PIC  X(0001).
           05  LSTTIMEI PIC  X(0005).
      *    -------------------------------
           05  CURRMEDL PIC S9(0004) COMP.
           05  CURRMEDF PIC  X(0001).
           05  FILLER REDEFINES CURRMEDF.
               10  CURRMEDA PIC  X(0001).
           05  CURRMEDI PIC  X(0006).
      *    -------------------------------
           05  CURRMEL PIC S9(0004) COMP.
           05  CURRMEF PIC  X(0001).
           05  FILLER REDEFINES CURRMEF.
               10  CURRMEA PIC  X(0001).
           05  CURRMEI PIC  X(0008).
      *    -------------------------------
           05  CREDMEDL PIC S9(0004) COMP.
           05  CREDMEDF PIC  X(0001).
           05  FILLER REDEFINES CREDMEDF.
               10  CREDMEDA PIC  X(0001).
           05  CREDMEDI PIC  X(0006).
      *    -------------------------------
           05  CREDMEL PIC S9(0004) COMP.
           05  CREDMEF PIC  X(0001).
           05  FILLER REDEFINES CREDMEF.
               10  CREDMEA PIC  X(0001).
           05  CREDMEI PIC  X(0008).
      *    -------------------------------
           05  MORTMEDL PIC S9(0004) COMP.
           05  MORTMEDF PIC  X(0001).
           05  FILLER REDEFINES MORTMEDF.
               10  MORTMEDA PIC  X(0001).
           05  MORTMEDI PIC  X(0011).
      *    -------------------------------
           05  MORTMEL PIC S9(0004) COMP.
           05  MORTMEF PIC  X(0001).
           05  FILLER REDEFINES MORTMEF.
               10  MORTMEA PIC  X(0001).
           05  MORTMEI PIC  X(0008).
      *    -------------------------------
           05  ARMEDL PIC S9(0004) COMP.
           05  ARMEDF PIC  X(0001).
           05  FILLER REDEFINES ARMEDF.
               10  ARMEDA PIC  X(0001).
           05  ARMEDI PIC  X(0003).
      *    -------------------------------
           05  ARMEL PIC S9(0004) COMP.
           05  ARMEF PIC  X(0001).
           05  FILLER REDEFINES ARMEF.
               10  ARMEA PIC  X(0001).
           05  ARMEI PIC  X(0008).
      *    -------------------------------
           05  AR860DTL PIC S9(0004) COMP.
           05  AR860DTF PIC  X(0001).
           05  FILLER REDEFINES AR860DTF.
               10  AR860DTA PIC  X(0001).
           05  AR860DTI PIC  X(0008).
      *    -------------------------------
           05  ERRMSGL PIC S9(0004) COMP.
           05  ERRMSGF PIC  X(0001).
           05  FILLER REDEFINES ERRMSGF.
               10  ERRMSGA PIC  X(0001).
           05  ERRMSGI PIC  X(0072).
      *    -------------------------------
           05  ENTERPFL PIC S9(0004) COMP.
           05  ENTERPFF PIC  X(0001).
           05  FILLER REDEFINES ENTERPFF.
               10  ENTERPFA PIC  X(0001).
           05  ENTERPFI PIC  99.
       01  EL102AO REDEFINES EL102AI.
           05  FILLER            PIC  X(0012).
           05  FILLER            PIC  X(0003).
           05  RUNDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COAO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDAO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPNMEO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INCAREO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPCDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDR1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PASSWDO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDR2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NEXTCOO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CITYSTO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TAXIDO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ZIPCODEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PHONEO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLSYSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MPSYSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTCD1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRSYSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARSYSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTCD2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCSYSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCCODEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FPIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EUSYSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SECOPTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JOURNIDO PIC  9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MEMBCAPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFDESCO PIC  X(0019).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFOVR1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFOVR2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFOVR6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFOVR12O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTUSRO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHDESCO PIC  X(0019).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHOVR1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHOVR2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHOVR6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHOVR12O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CURRMEDO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CURRMEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREDMEDO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREDMEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MORTMEDO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MORTMEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARMEDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARMEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AR860DTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSGO PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENTERPFO PIC  X(0002).
      *    -------------------------------
       01  EL102BI REDEFINES EL102AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  BRUNDTEL PIC S9(0004) COMP.
           05  BRUNDTEF PIC  X(0001).
           05  FILLER REDEFINES BRUNDTEF.
               10  BRUNDTEA PIC  X(0001).
           05  BRUNDTEI PIC  X(0008).
      *    -------------------------------
           05  BRUNTMEL PIC S9(0004) COMP.
           05  BRUNTMEF PIC  X(0001).
           05  FILLER REDEFINES BRUNTMEF.
               10  BRUNTMEA PIC  X(0001).
           05  BRUNTMEI PIC  X(0005).
      *    -------------------------------
           05  COBL PIC S9(0004) COMP.
           05  COBF PIC  X(0001).
           05  FILLER REDEFINES COBF.
               10  COBA PIC  X(0001).
           05  COBI PIC  X(0003).
      *    -------------------------------
           05  USERIDBL PIC S9(0004) COMP.
           05  USERIDBF PIC  X(0001).
           05  FILLER REDEFINES USERIDBF.
               10  USERIDBA PIC  X(0001).
           05  USERIDBI PIC  X(0004).
      *    -------------------------------
           05  CARRCLL PIC S9(0004) COMP.
           05  CARRCLF PIC  X(0001).
           05  FILLER REDEFINES CARRCLF.
               10  CARRCLA PIC  X(0001).
           05  CARRCLI PIC  X(0001).
      *    -------------------------------
           05  AUDCLCGL PIC S9(0004) COMP.
           05  AUDCLCGF PIC  X(0001).
           05  FILLER REDEFINES AUDCLCGF.
               10  AUDCLCGA PIC  X(0001).
           05  AUDCLCGI PIC  X(0001).
      *    -------------------------------
           05  LSTCLML PIC S9(0004) COMP.
           05  LSTCLMF PIC  X(0001).
           05  FILLER REDEFINES LSTCLMF.
               10  LSTCLMA PIC  X(0001).
           05  LSTCLMI PIC  9(8).
      *    -------------------------------
           05  CRDCDINL PIC S9(0004) COMP.
           05  CRDCDINF PIC  X(0001).
           05  FILLER REDEFINES CRDCDINF.
               10  CRDCDINA PIC  X(0001).
           05  CRDCDINI PIC  X(0001).
      *    -------------------------------
           05  LSTARCHL PIC S9(0004) COMP.
           05  LSTARCHF PIC  X(0001).
           05  FILLER REDEFINES LSTARCHF.
               10  LSTARCHA PIC  X(0001).
           05  LSTARCHI PIC  9(8).
      *    -------------------------------
           05  CLMTOLL PIC S9(0004) COMP.
           05  CLMTOLF PIC  X(0001).
           05  FILLER REDEFINES CLMTOLF.
               10  CLMTOLA PIC  X(0001).
           05  CLMTOLI PIC  999V99.
      *    -------------------------------
           05  LSTCHKL PIC S9(0004) COMP.
           05  LSTCHKF PIC  X(0001).
           05  FILLER REDEFINES LSTCHKF.
               10  LSTCHKA PIC  X(0001).
           05  LSTCHKI PIC  9(8).
      *    -------------------------------
           05  CLMREJL PIC S9(0004) COMP.
           05  CLMREJF PIC  X(0001).
           05  FILLER REDEFINES CLMREJF.
               10  CLMREJA PIC  X(0001).
           05  CLMREJI PIC  X(0001).
      *    -------------------------------
           05  LSTQUEL PIC S9(0004) COMP.
           05  LSTQUEF PIC  X(0001).
           05  FILLER REDEFINES LSTQUEF.
               10  LSTQUEA PIC  X(0001).
           05  LSTQUEI PIC  9(8).
      *    -------------------------------
           05  COFFDTL PIC S9(0004) COMP.
           05  COFFDTF PIC  X(0001).
           05  FILLER REDEFINES COFFDTF.
               10  COFFDTA PIC  X(0001).
           05  COFFDTI PIC  X(0008).
      *    -------------------------------
           05  STARCHL PIC S9(0004) COMP.
           05  STARCHF PIC  X(0001).
           05  FILLER REDEFINES STARCHF.
               10  STARCHA PIC  X(0001).
           05  STARCHI PIC  9(8).
      *    -------------------------------
           05  PMTSCHKL PIC S9(0004) COMP.
           05  PMTSCHKF PIC  X(0001).
           05  FILLER REDEFINES PMTSCHKF.
               10  PMTSCHKA PIC  X(0001).
           05  PMTSCHKI PIC  99.
      *    -------------------------------
           05  EXPRETNL PIC S9(0004) COMP.
           05  EXPRETNF PIC  X(0001).
           05  FILLER REDEFINES EXPRETNF.
               10  EXPRETNA PIC  X(0001).
           05  EXPRETNI PIC  9(1).
      *    -------------------------------
           05  THRUTOL PIC S9(0004) COMP.
           05  THRUTOF PIC  X(0001).
           05  FILLER REDEFINES THRUTOF.
               10  THRUTOA PIC  X(0001).
           05  THRUTOI PIC  X(0001).
      *    -------------------------------
           05  APPROVL PIC S9(0004) COMP.
           05  APPROVF PIC  X(0001).
           05  FILLER REDEFINES APPROVF.
               10  APPROVA PIC  X(0001).
           05  APPROVI PIC  X(0001).
      *    -------------------------------
           05  ADDRLBLL PIC S9(0004) COMP.
           05  ADDRLBLF PIC  X(0001).
           05  FILLER REDEFINES ADDRLBLF.
               10  ADDRLBLA PIC  X(0001).
           05  ADDRLBLI PIC  X(0001).
      *    -------------------------------
           05  RECONL PIC S9(0004) COMP.
           05  RECONF PIC  X(0001).
           05  FILLER REDEFINES RECONF.
               10  RECONA PIC  X(0001).
           05  RECONI PIC  X(0001).
      *    -------------------------------
           05  LLEV1L PIC S9(0004) COMP.
           05  LLEV1F PIC  X(0001).
           05  FILLER REDEFINES LLEV1F.
               10  LLEV1A PIC  X(0001).
           05  LLEV1I PIC  9(7).
      *    -------------------------------
           05  ALEV1L PIC S9(0004) COMP.
           05  ALEV1F PIC  X(0001).
           05  FILLER REDEFINES ALEV1F.
               10  ALEV1A PIC  X(0001).
           05  ALEV1I PIC  9(7).
      *    -------------------------------
           05  PROCDTL PIC S9(0004) COMP.
           05  PROCDTF PIC  X(0001).
           05  FILLER REDEFINES PROCDTF.
               10  PROCDTA PIC  X(0001).
           05  PROCDTI PIC  X(0008).
      *    -------------------------------
           05  LLEV2L PIC S9(0004) COMP.
           05  LLEV2F PIC  X(0001).
           05  FILLER REDEFINES LLEV2F.
               10  LLEV2A PIC  X(0001).
           05  LLEV2I PIC  9(7).
      *    -------------------------------
           05  ALEV2L PIC S9(0004) COMP.
           05  ALEV2F PIC  X(0001).
           05  FILLER REDEFINES ALEV2F.
               10  ALEV2A PIC  X(0001).
           05  ALEV2I PIC  9(7).
      *    -------------------------------
           05  LTRMTDTL PIC S9(0004) COMP.
           05  LTRMTDTF PIC  X(0001).
           05  FILLER REDEFINES LTRMTDTF.
               10  LTRMTDTA PIC  X(0001).
           05  LTRMTDTI PIC  X(0002).
      *    -------------------------------
           05  LLEV3L PIC S9(0004) COMP.
           05  LLEV3F PIC  X(0001).
           05  FILLER REDEFINES LLEV3F.
               10  LLEV3A PIC  X(0001).
           05  LLEV3I PIC  9(7).
      *    -------------------------------
           05  ALEV3L PIC S9(0004) COMP.
           05  ALEV3F PIC  X(0001).
           05  FILLER REDEFINES ALEV3F.
               10  ALEV3A PIC  X(0001).
           05  ALEV3I PIC  9(7).
      *    -------------------------------
           05  RSVOPSWL PIC S9(0004) COMP.
           05  RSVOPSWF PIC  X(0001).
           05  FILLER REDEFINES RSVOPSWF.
               10  RSVOPSWA PIC  X(0001).
           05  RSVOPSWI PIC  X(0001).
      *    -------------------------------
           05  OPTDTEL PIC S9(0004) COMP.
           05  OPTDTEF PIC  X(0001).
           05  FILLER REDEFINES OPTDTEF.
               10  OPTDTEA PIC  X(0001).
           05  OPTDTEI PIC  X(0008).
      *    -------------------------------
           05  CIDADISL PIC S9(0004) COMP.
           05  CIDADISF PIC  X(0001).
           05  FILLER REDEFINES CIDADISF.
               10  CIDADISA PIC  X(0001).
           05  CIDADISI PIC  99V9999.
      *    -------------------------------
           05  CRDTBLUL PIC S9(0004) COMP.
           05  CRDTBLUF PIC  X(0001).
           05  FILLER REDEFINES CRDTBLUF.
               10  CRDTBLUA PIC  X(0001).
           05  CRDTBLUI PIC  X(0001).
      *    -------------------------------
           05  IBNRLFFL PIC S9(0004) COMP.
           05  IBNRLFFF PIC  X(0001).
           05  FILLER REDEFINES IBNRLFFF.
               10  IBNRLFFA PIC  X(0001).
           05  IBNRLFFI PIC  99V9999.
      *    -------------------------------
           05  IBNRAHFL PIC S9(0004) COMP.
           05  IBNRAHFF PIC  X(0001).
           05  FILLER REDEFINES IBNRAHFF.
               10  IBNRAHFA PIC  X(0001).
           05  IBNRAHFI PIC  99V9999.
      *    -------------------------------
           05  IBLGMTHL PIC S9(0004) COMP.
           05  IBLGMTHF PIC  X(0001).
           05  FILLER REDEFINES IBLGMTHF.
               10  IBLGMTHA PIC  X(0001).
           05  IBLGMTHI PIC  X(0003).
      *    -------------------------------
           05  CALCINTL PIC S9(0004) COMP.
           05  CALCINTF PIC  X(0001).
           05  FILLER REDEFINES CALCINTF.
               10  CALCINTA PIC  X(0001).
           05  CALCINTI PIC  99V9999.
      *    -------------------------------
           05  BERMSG1L PIC S9(0004) COMP.
           05  BERMSG1F PIC  X(0001).
           05  FILLER REDEFINES BERMSG1F.
               10  BERMSG1A PIC  X(0001).
           05  BERMSG1I PIC  X(0072).
      *    -------------------------------
           05  BERMSG2L PIC S9(0004) COMP.
           05  BERMSG2F PIC  X(0001).
           05  FILLER REDEFINES BERMSG2F.
               10  BERMSG2A PIC  X(0001).
           05  BERMSG2I PIC  X(0072).
      *    -------------------------------
           05  BERMSG3L PIC S9(0004) COMP.
           05  BERMSG3F PIC  X(0001).
           05  FILLER REDEFINES BERMSG3F.
               10  BERMSG3A PIC  X(0001).
           05  BERMSG3I PIC  X(0072).
      *    -------------------------------
           05  ENTRBPFL PIC S9(0004) COMP.
           05  ENTRBPFF PIC  X(0001).
           05  FILLER REDEFINES ENTRBPFF.
               10  ENTRBPFA PIC  X(0001).
           05  ENTRBPFI PIC  99.
       01  EL102BO REDEFINES EL102AI.
           05  FILLER            PIC  X(0012).
           05  FILLER            PIC  X(0003).
           05  BRUNDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRUNTMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COBO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDBO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRCLO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUDCLCGO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTCLMO PIC  9(8).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRDCDINO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTARCHO PIC  9(8).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLMTOLO PIC  Z9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTCHKO PIC  9(8).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLMREJO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTQUEO PIC  9(8).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COFFDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STARCHO PIC  9(8).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PMTSCHKO PIC  Z9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPRETNO PIC  9(1).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  THRUTOO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APPROVO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDRLBLO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECONO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LLEV1O PIC  ZZZ,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALEV1O PIC  ZZZ,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROCDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LLEV2O PIC  ZZZ,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALEV2O PIC  ZZZ,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LTRMTDTO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LLEV3O PIC  ZZZ,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALEV3O PIC  ZZZ,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSVOPSWO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OPTDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CIDADISO PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRDTBLUO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  IBNRLFFO PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  IBNRAHFO PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  IBLGMTHO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CALCINTO PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BERMSG1O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BERMSG2O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BERMSG3O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENTRBPFO PIC  X(0002).
      *    -------------------------------
       01  EL102CI REDEFINES EL102AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  CRUNDTEL PIC S9(0004) COMP.
           05  CRUNDTEF PIC  X(0001).
           05  FILLER REDEFINES CRUNDTEF.
               10  CRUNDTEA PIC  X(0001).
           05  CRUNDTEI PIC  X(0008).
      *    -------------------------------
           05  CRUNTMEL PIC S9(0004) COMP.
           05  CRUNTMEF PIC  X(0001).
           05  FILLER REDEFINES CRUNTMEF.
               10  CRUNTMEA PIC  X(0001).
           05  CRUNTMEI PIC  X(0005).
      *    -------------------------------
           05  COCL PIC S9(0004) COMP.
           05  COCF PIC  X(0001).
           05  FILLER REDEFINES COCF.
               10  COCA PIC  X(0001).
           05  COCI PIC  X(0003).
      *    -------------------------------
           05  USERIDCL PIC S9(0004) COMP.
           05  USERIDCF PIC  X(0001).
           05  FILLER REDEFINES USERIDCF.
               10  USERIDCA PIC  X(0001).
           05  USERIDCI PIC  X(0004).
      *    -------------------------------
           05  PREJCTL PIC S9(0004) COMP.
           05  PREJCTF PIC  X(0001).
           05  FILLER REDEFINES PREJCTF.
               10  PREJCTA PIC  X(0001).
           05  PREJCTI PIC  X(0001).
      *    -------------------------------
           05  AMRTCDL PIC S9(0004) COMP.
           05  AMRTCDF PIC  X(0001).
           05  FILLER REDEFINES AMRTCDF.
               10  AMRTCDA PIC  X(0001).
           05  AMRTCDI PIC  X(0004).
      *    -------------------------------
           05  CONSYSL PIC S9(0004) COMP.
           05  CONSYSF PIC  X(0001).
           05  FILLER REDEFINES CONSYSF.
               10  CONSYSA PIC  X(0001).
           05  CONSYSI PIC  X(0001).
      *    -------------------------------
           05  QTECALL PIC S9(0004) COMP.
           05  QTECALF PIC  X(0001).
           05  FILLER REDEFINES QTECALF.
               10  QTECALA PIC  X(0001).
           05  QTECALI PIC  S9(3)V99.
      *    -------------------------------
           05  PRMPCTL PIC S9(0004) COMP.
           05  PRMPCTF PIC  X(0001).
           05  FILLER REDEFINES PRMPCTF.
               10  PRMPCTA PIC  X(0001).
           05  PRMPCTI PIC  S9(1)V9(4).
      *    -------------------------------
           05  DABILLL PIC S9(0004) COMP.
           05  DABILLF PIC  X(0001).
           05  FILLER REDEFINES DABILLF.
               10  DABILLA PIC  X(0001).
           05  DABILLI PIC  X(0001).
      *    -------------------------------
           05  QTEREFL PIC S9(0004) COMP.
           05  QTEREFF PIC  X(0001).
           05  FILLER REDEFINES QTEREFF.
               10  QTEREFA PIC  X(0001).
           05  QTEREFI PIC  S9(3)V99.
      *    -------------------------------
           05  REFPCTL PIC S9(0004) COMP.
           05  REFPCTF PIC  X(0001).
           05  FILLER REDEFINES REFPCTF.
               10  REFPCTA PIC  X(0001).
           05  REFPCTI PIC  S9(1)V9(4).
      *    -------------------------------
           05  MAILL PIC S9(0004) COMP.
           05  MAILF PIC  X(0001).
           05  FILLER REDEFINES MAILF.
               10  MAILA PIC  X(0001).
           05  MAILI PIC  X(0001).
      *    -------------------------------
           05  OVSAMTL PIC S9(0004) COMP.
           05  OVSAMTF PIC  X(0001).
           05  FILLER REDEFINES OVSAMTF.
               10  OVSAMTA PIC  X(0001).
           05  OVSAMTI PIC  S9(3)V99.
      *    -------------------------------
           05  OVSPCTL PIC S9(0004) COMP.
           05  OVSPCTF PIC  X(0001).
           05  FILLER REDEFINES OVSPCTF.
               10  OVSPCTA PIC  X(0001).
           05  OVSPCTI PIC  S9(1)V9(4).
      *    -------------------------------
           05  JAGEL PIC S9(0004) COMP.
           05  JAGEF PIC  X(0001).
           05  FILLER REDEFINES JAGEF.
               10  JAGEA PIC  X(0001).
           05  JAGEI PIC  X(0001).
      *    -------------------------------
           05  MINPREML PIC S9(0004) COMP.
           05  MINPREMF PIC  X(0001).
           05  FILLER REDEFINES MINPREMF.
               10  MINPREMA PIC  X(0001).
           05  MINPREMI PIC  999V99.
      *    -------------------------------
           05  MINAGEL PIC S9(0004) COMP.
           05  MINAGEF PIC  X(0001).
           05  FILLER REDEFINES MINAGEF.
               10  MINAGEA PIC  X(0001).
           05  MINAGEI PIC  X(0002).
      *    -------------------------------
           05  BINPUTL PIC S9(0004) COMP.
           05  BINPUTF PIC  X(0001).
           05  FILLER REDEFINES BINPUTF.
               10  BINPUTA PIC  X(0001).
           05  BINPUTI PIC  X(0001).
      *    -------------------------------
           05  REFREJL PIC S9(0004) COMP.
           05  REFREJF PIC  X(0001).
           05  FILLER REDEFINES REFREJF.
               10  REFREJA PIC  X(0001).
           05  REFREJI PIC  X(0001).
      *    -------------------------------
           05  MAXTERML PIC S9(0004) COMP.
           05  MAXTERMF PIC  X(0001).
           05  FILLER REDEFINES MAXTERMF.
               10  MAXTERMA PIC  X(0001).
           05  MAXTERMI PIC  X(0003).
      *    -------------------------------
           05  CARGRPOL PIC S9(0004) COMP.
           05  CARGRPOF PIC  X(0001).
           05  FILLER REDEFINES CARGRPOF.
               10  CARGRPOA PIC  X(0001).
           05  CARGRPOI PIC  X(0001).
      *    -------------------------------
           05  COMPWTEL PIC S9(0004) COMP.
           05  COMPWTEF PIC  X(0001).
           05  FILLER REDEFINES COMPWTEF.
               10  COMPWTEA PIC  X(0001).
           05  COMPWTEI PIC  999V99.
      *    -------------------------------
           05  MTOLCAPL PIC S9(0004) COMP.
           05  MTOLCAPF PIC  X(0001).
           05  FILLER REDEFINES MTOLCAPF.
               10  MTOLCAPA PIC  X(0001).
           05  MTOLCAPI PIC  999V99.
      *    -------------------------------
           05  CRTACSL PIC S9(0004) COMP.
           05  CRTACSF PIC  X(0001).
           05  FILLER REDEFINES CRTACSF.
               10  CRTACSA PIC  X(0001).
           05  CRTACSI PIC  X(0001).
      *    -------------------------------
           05  PRCESSFL PIC S9(0004) COMP.
           05  PRCESSFF PIC  X(0001).
           05  FILLER REDEFINES PRCESSFF.
               10  PRCESSFA PIC  X(0001).
           05  PRCESSFI PIC  X(0001).
      *    -------------------------------
           05  DAGEL PIC S9(0004) COMP.
           05  DAGEF PIC  X(0001).
           05  FILLER REDEFINES DAGEF.
               10  DAGEA PIC  X(0001).
           05  DAGEI PIC  X(0002).
      *    -------------------------------
           05  MEMBNOL PIC S9(0004) COMP.
           05  MEMBNOF PIC  X(0001).
           05  FILLER REDEFINES MEMBNOF.
               10  MEMBNOA PIC  X(0001).
           05  MEMBNOI PIC  X(0001).
      *    -------------------------------
           05  CMETHODL PIC S9(0004) COMP.
           05  CMETHODF PIC  X(0001).
           05  FILLER REDEFINES CMETHODF.
               10  CMETHODA PIC  X(0001).
           05  CMETHODI PIC  X(0001).
      *    -------------------------------
           05  DSEXL PIC S9(0004) COMP.
           05  DSEXF PIC  X(0001).
           05  FILLER REDEFINES DSEXF.
               10  DSEXA PIC  X(0001).
           05  DSEXI PIC  X(0001).
      *    -------------------------------
           05  SSNL PIC S9(0004) COMP.
           05  SSNF PIC  X(0001).
           05  FILLER REDEFINES SSNF.
               10  SSNA PIC  X(0001).
           05  SSNI PIC  X(0001).
      *    -------------------------------
           05  REMTRML PIC S9(0004) COMP.
           05  REMTRMF PIC  X(0001).
           05  FILLER REDEFINES REMTRMF.
               10  REMTRMA PIC  X(0001).
           05  REMTRMI PIC  X(0001).
      *    -------------------------------
           05  CONDTEL PIC S9(0004) COMP.
           05  CONDTEF PIC  X(0001).
           05  FILLER REDEFINES CONDTEF.
               10  CONDTEA PIC  X(0001).
           05  CONDTEI PIC  X(0008).
      *    -------------------------------
           05  AGECALCL PIC S9(0004) COMP.
           05  AGECALCF PIC  X(0001).
           05  FILLER REDEFINES AGECALCF.
               10  AGECALCA PIC  X(0001).
           05  AGECALCI PIC  X(0001).
      *    -------------------------------
           05  R78MTHL PIC S9(0004) COMP.
           05  R78MTHF PIC  X(0001).
           05  FILLER REDEFINES R78MTHF.
               10  R78MTHA PIC  X(0001).
           05  R78MTHI PIC  X(0001).
      *    -------------------------------
           05  LSTBTCHL PIC S9(0004) COMP.
           05  LSTBTCHF PIC  X(0001).
           05  FILLER REDEFINES LSTBTCHF.
               10  LSTBTCHA PIC  X(0001).
           05  LSTBTCHI PIC  9(8).
      *    -------------------------------
           05  STCNTLL PIC S9(0004) COMP.
           05  STCNTLF PIC  X(0001).
           05  FILLER REDEFINES STCNTLF.
               10  STCNTLA PIC  X(0001).
           05  STCNTLI PIC  X(0001).
      *    -------------------------------
           05  TRMOPTNL PIC S9(0004) COMP.
           05  TRMOPTNF PIC  X(0001).
           05  FILLER REDEFINES TRMOPTNF.
               10  TRMOPTNA PIC  X(0001).
           05  TRMOPTNI PIC  X(0001).
      *    -------------------------------
           05  DAPRL PIC S9(0004) COMP.
           05  DAPRF PIC  X(0001).
           05  FILLER REDEFINES DAPRF.
               10  DAPRA PIC  X(0001).
           05  DAPRI PIC  9(4)V9(4).
      *    -------------------------------
           05  ARCHPYRL PIC S9(0004) COMP.
           05  ARCHPYRF PIC  X(0001).
           05  FILLER REDEFINES ARCHPYRF.
               10  ARCHPYRA PIC  X(0001).
           05  ARCHPYRI PIC  X(0001).
      *    -------------------------------
           05  ARCHSTL PIC S9(0004) COMP.
           05  ARCHSTF PIC  X(0001).
           05  FILLER REDEFINES ARCHSTF.
               10  ARCHSTA PIC  X(0001).
           05  ARCHSTI PIC  9(8).
      *    -------------------------------
           05  CRLABELL PIC S9(0004) COMP.
           05  CRLABELF PIC  X(0001).
           05  FILLER REDEFINES CRLABELF.
               10  CRLABELA PIC  X(0001).
           05  CRLABELI PIC  X(0001).
      *    -------------------------------
           05  ARCHLSTL PIC S9(0004) COMP.
           05  ARCHLSTF PIC  X(0001).
           05  FILLER REDEFINES ARCHLSTF.
               10  ARCHLSTA PIC  X(0001).
           05  ARCHLSTI PIC  9(8).
      *    -------------------------------
           05  QUCOUNTL PIC S9(0004) COMP.
           05  QUCOUNTF PIC  X(0001).
           05  FILLER REDEFINES QUCOUNTF.
               10  QUCOUNTA PIC  X(0001).
           05  QUCOUNTI PIC  9(8).
      *    -------------------------------
           05  CKCOUNTL PIC S9(0004) COMP.
           05  CKCOUNTF PIC  X(0001).
           05  FILLER REDEFINES CKCOUNTF.
               10  CKCOUNTA PIC  X(0001).
           05  CKCOUNTI PIC  9(8).
      *    -------------------------------
           05  CERMSG1L PIC S9(0004) COMP.
           05  CERMSG1F PIC  X(0001).
           05  FILLER REDEFINES CERMSG1F.
               10  CERMSG1A PIC  X(0001).
           05  CERMSG1I PIC  X(0072).
      *    -------------------------------
           05  CERMSG2L PIC S9(0004) COMP.
           05  CERMSG2F PIC  X(0001).
           05  FILLER REDEFINES CERMSG2F.
               10  CERMSG2A PIC  X(0001).
           05  CERMSG2I PIC  X(0072).
      *    -------------------------------
           05  ENTRCPFL PIC S9(0004) COMP.
           05  ENTRCPFF PIC  X(0001).
           05  FILLER REDEFINES ENTRCPFF.
               10  ENTRCPFA PIC  X(0001).
           05  ENTRCPFI PIC  99.
       01  EL102CO REDEFINES EL102AI.
           05  FILLER            PIC  X(0012).
           05  FILLER            PIC  X(0003).
           05  CRUNDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRUNTMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COCO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDCO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PREJCTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMRTCDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CONSYSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  QTECALO PIC  Z9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRMPCTO PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DABILLO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  QTEREFO PIC  Z9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFPCTO PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAILO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OVSAMTO PIC  Z9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OVSPCTO PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JAGEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MINPREMO PIC  Z9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MINAGEO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BINPUTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFREJO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXTERMO PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARGRPOO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPWTEO PIC  Z9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTOLCAPO PIC  Z9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRTACSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRCESSFO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DAGEO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MEMBNOO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMETHODO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSEXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SSNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REMTRMO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CONDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGECALCO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  R78MTHO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTBTCHO PIC  9(8).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STCNTLO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRMOPTNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DAPRO PIC  ZZ9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCHPYRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCHSTO PIC  9(8).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRLABELO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCHLSTO PIC  9(8).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  QUCOUNTO PIC  9(8).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CKCOUNTO PIC  9(8).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERMSG1O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERMSG2O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENTRCPFO PIC  X(0002).
      *    -------------------------------
       01  EL102DI REDEFINES EL102AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  DRUNDTEL PIC S9(0004) COMP.
           05  DRUNDTEF PIC  X(0001).
           05  FILLER REDEFINES DRUNDTEF.
               10  DRUNDTEA PIC  X(0001).
           05  DRUNDTEI PIC  X(0008).
      *    -------------------------------
           05  DRUNTMEL PIC S9(0004) COMP.
           05  DRUNTMEF PIC  X(0001).
           05  FILLER REDEFINES DRUNTMEF.
               10  DRUNTMEA PIC  X(0001).
           05  DRUNTMEI PIC  X(0005).
      *    -------------------------------
           05  CODL PIC S9(0004) COMP.
           05  CODF PIC  X(0001).
           05  FILLER REDEFINES CODF.
               10  CODA PIC  X(0001).
           05  CODI PIC  X(0003).
      *    -------------------------------
           05  USERIDDL PIC S9(0004) COMP.
           05  USERIDDF PIC  X(0001).
           05  FILLER REDEFINES USERIDDF.
               10  USERIDDA PIC  X(0001).
           05  USERIDDI PIC  X(0004).
      *    -------------------------------
           05  MIBSYML PIC S9(0004) COMP.
           05  MIBSYMF PIC  X(0001).
           05  FILLER REDEFINES MIBSYMF.
               10  MIBSYMA PIC  X(0001).
           05  MIBSYMI PIC  X(0003).
      *    -------------------------------
           05  MIBVERL PIC S9(0004) COMP.
           05  MIBVERF PIC  X(0001).
           05  FILLER REDEFINES MIBVERF.
               10  MIBVERA PIC  X(0001).
           05  MIBVERI PIC  X(0001).
      *    -------------------------------
           05  DAMRTCDL PIC S9(0004) COMP.
           05  DAMRTCDF PIC  X(0001).
           05  FILLER REDEFINES DAMRTCDF.
               10  DAMRTCDA PIC  X(0001).
           05  DAMRTCDI PIC  X(0004).
      *    -------------------------------
           05  MIBCOMML PIC S9(0004) COMP.
           05  MIBCOMMF PIC  X(0001).
           05  FILLER REDEFINES MIBCOMMF.
               10  MIBCOMMA PIC  X(0001).
           05  MIBCOMMI PIC  X(0005).
      *    -------------------------------
           05  MIBCSWL PIC S9(0004) COMP.
           05  MIBCSWF PIC  X(0001).
           05  FILLER REDEFINES MIBCSWF.
               10  MIBCSWA PIC  X(0001).
           05  MIBCSWI PIC  X(0001).
      *    -------------------------------
           05  DRESURL PIC S9(0004) COMP.
           05  DRESURF PIC  X(0001).
           05  FILLER REDEFINES DRESURF.
               10  DRESURA PIC  X(0001).
           05  DRESURI PIC  X(0001).
      *    -------------------------------
           05  RATESWL PIC S9(0004) COMP.
           05  RATESWF PIC  X(0001).
           05  FILLER REDEFINES RATESWF.
               10  RATESWA PIC  X(0001).
           05  RATESWI PIC  X(0001).
      *    -------------------------------
           05  MIBTERML PIC S9(0004) COMP.
           05  MIBTERMF PIC  X(0001).
           05  FILLER REDEFINES MIBTERMF.
               10  MIBTERMA PIC  X(0001).
           05  MIBTERMI PIC  X(0005).
      *    -------------------------------
           05  MIBTSWL PIC S9(0004) COMP.
           05  MIBTSWF PIC  X(0001).
           05  FILLER REDEFINES MIBTSWF.
               10  MIBTSWA PIC  X(0001).
           05  MIBTSWI PIC  X(0001).
      *    -------------------------------
           05  CMPCNTLL PIC S9(0004) COMP.
           05  CMPCNTLF PIC  X(0001).
           05  FILLER REDEFINES CMPCNTLF.
               10  CMPCNTLA PIC  X(0001).
           05  CMPCNTLI PIC  X(0001).
      *    -------------------------------
           05  LABCNTLL PIC S9(0004) COMP.
           05  LABCNTLF PIC  X(0001).
           05  FILLER REDEFINES LABCNTLF.
               10  LABCNTLA PIC  X(0001).
           05  LABCNTLI PIC  X(0001).
      *    -------------------------------
           05  CNVDTEL PIC S9(0004) COMP.
           05  CNVDTEF PIC  X(0001).
           05  FILLER REDEFINES CNVDTEF.
               10  CNVDTEA PIC  X(0001).
           05  CNVDTEI PIC  X(0008).
      *    -------------------------------
           05  AUTOREFL PIC S9(0004) COMP.
           05  AUTOREFF PIC  X(0001).
           05  FILLER REDEFINES AUTOREFF.
               10  AUTOREFA PIC  X(0001).
           05  AUTOREFI PIC  X(0001).
      *    -------------------------------
           05  MGCNTLL PIC S9(0004) COMP.
           05  MGCNTLF PIC  X(0001).
           05  FILLER REDEFINES MGCNTLF.
               10  MGCNTLA PIC  X(0001).
           05  MGCNTLI PIC  X(0001).
      *    -------------------------------
           05  STARCHNL PIC S9(0004) COMP.
           05  STARCHNF PIC  X(0001).
           05  FILLER REDEFINES STARCHNF.
               10  STARCHNA PIC  X(0001).
           05  STARCHNI PIC  X(0008).
      *    -------------------------------
           05  SHFTLNOL PIC S9(0004) COMP.
           05  SHFTLNOF PIC  X(0001).
           05  FILLER REDEFINES SHFTLNOF.
               10  SHFTLNOA PIC  X(0001).
           05  SHFTLNOI PIC  X(0001).
      *    -------------------------------
           05  CHKPMTHL PIC S9(0004) COMP.
           05  CHKPMTHF PIC  X(0001).
           05  FILLER REDEFINES CHKPMTHF.
               10  CHKPMTHA PIC  X(0001).
           05  CHKPMTHI PIC  X(0001).
      *    -------------------------------
           05  ARCHNBRL PIC S9(0004) COMP.
           05  ARCHNBRF PIC  X(0001).
           05  FILLER REDEFINES ARCHNBRF.
               10  ARCHNBRA PIC  X(0001).
           05  ARCHNBRI PIC  X(0008).
      *    -------------------------------
           05  GRPSWL PIC S9(0004) COMP.
           05  GRPSWF PIC  X(0001).
           05  FILLER REDEFINES GRPSWF.
               10  GRPSWA PIC  X(0001).
           05  GRPSWI PIC  X(0001).
      *    -------------------------------
           05  MRECONL PIC S9(0004) COMP.
           05  MRECONF PIC  X(0001).
           05  FILLER REDEFINES MRECONF.
               10  MRECONA PIC  X(0001).
           05  MRECONI PIC  X(0001).
      *    -------------------------------
           05  REFNOL PIC S9(0004) COMP.
           05  REFNOF PIC  X(0001).
           05  FILLER REDEFINES REFNOF.
               10  REFNOA PIC  X(0001).
           05  REFNOI PIC  9(17).
      *    -------------------------------
           05  RPTLANGL PIC S9(0004) COMP.
           05  RPTLANGF PIC  X(0001).
           05  FILLER REDEFINES RPTLANGF.
               10  RPTLANGA PIC  X(0001).
           05  RPTLANGI PIC  X(0001).
      *    -------------------------------
           05  PLCYLNKL PIC S9(0004) COMP.
           05  PLCYLNKF PIC  X(0001).
           05  FILLER REDEFINES PLCYLNKF.
               10  PLCYLNKA PIC  X(0001).
           05  PLCYLNKI PIC  X(0001).
      *    -------------------------------
           05  LNMESERL PIC S9(0004) COMP.
           05  LNMESERF PIC  X(0001).
           05  FILLER REDEFINES LNMESERF.
               10  LNMESERA PIC  X(0001).
           05  LNMESERI PIC  X(0002).
      *    -------------------------------
           05  FNMESERL PIC S9(0004) COMP.
           05  FNMESERF PIC  X(0001).
           05  FILLER REDEFINES FNMESERF.
               10  FNMESERA PIC  X(0001).
           05  FNMESERI PIC  X(0002).
      *    -------------------------------
           05  MNMESERL PIC S9(0004) COMP.
           05  MNMESERF PIC  X(0001).
           05  FILLER REDEFINES MNMESERF.
               10  MNMESERA PIC  X(0001).
           05  MNMESERI PIC  X(0002).
      *    -------------------------------
           05  BCYCLE1L PIC S9(0004) COMP.
           05  BCYCLE1F PIC  X(0001).
           05  FILLER REDEFINES BCYCLE1F.
               10  BCYCLE1A PIC  X(0001).
           05  BCYCLE1I PIC  X(0001).
      *    -------------------------------
           05  BCYCLE2L PIC S9(0004) COMP.
           05  BCYCLE2F PIC  X(0001).
           05  FILLER REDEFINES BCYCLE2F.
               10  BCYCLE2A PIC  X(0001).
           05  BCYCLE2I PIC  X(0001).
      *    -------------------------------
           05  BCYCLE3L PIC S9(0004) COMP.
           05  BCYCLE3F PIC  X(0001).
           05  FILLER REDEFINES BCYCLE3F.
               10  BCYCLE3A PIC  X(0001).
           05  BCYCLE3I PIC  X(0001).
      *    -------------------------------
           05  BCYCLE4L PIC S9(0004) COMP.
           05  BCYCLE4F PIC  X(0001).
           05  FILLER REDEFINES BCYCLE4F.
               10  BCYCLE4A PIC  X(0001).
           05  BCYCLE4I PIC  X(0001).
      *    -------------------------------
           05  BCYCLE5L PIC S9(0004) COMP.
           05  BCYCLE5F PIC  X(0001).
           05  FILLER REDEFINES BCYCLE5F.
               10  BCYCLE5A PIC  X(0001).
           05  BCYCLE5I PIC  X(0001).
      *    -------------------------------
           05  BDATEL PIC S9(0004) COMP.
           05  BDATEF PIC  X(0001).
           05  FILLER REDEFINES BDATEF.
               10  BDATEA PIC  X(0001).
           05  BDATEI PIC  X(0002).
      *    -------------------------------
           05  BSTATEL PIC S9(0004) COMP.
           05  BSTATEF PIC  X(0001).
           05  FILLER REDEFINES BSTATEF.
               10  BSTATEA PIC  X(0001).
           05  BSTATEI PIC  X(0002).
      *    -------------------------------
           05  RSTATEL PIC S9(0004) COMP.
           05  RSTATEF PIC  X(0001).
           05  FILLER REDEFINES RSTATEF.
               10  RSTATEA PIC  X(0001).
           05  RSTATEI PIC  X(0002).
      *    -------------------------------
           05  LSTUSERL PIC S9(0004) COMP.
           05  LSTUSERF PIC  X(0001).
           05  FILLER REDEFINES LSTUSERF.
               10  LSTUSERA PIC  X(0001).
           05  LSTUSERI PIC  X(0004).
      *    -------------------------------
           05  LSTDATEL PIC S9(0004) COMP.
           05  LSTDATEF PIC  X(0001).
           05  FILLER REDEFINES LSTDATEF.
               10  LSTDATEA PIC  X(0001).
           05  LSTDATEI PIC  X(0008).
      *    -------------------------------
           05  LSTTMEL PIC S9(0004) COMP.
           05  LSTTMEF PIC  X(0001).
           05  FILLER REDEFINES LSTTMEF.
               10  LSTTMEA PIC  X(0001).
           05  LSTTMEI PIC  X(0005).
      *    -------------------------------
           05  RATFILML PIC S9(0004) COMP.
           05  RATFILMF PIC  X(0001).
           05  FILLER REDEFINES RATFILMF.
               10  RATFILMA PIC  X(0001).
           05  RATFILMI PIC  X(0008).
      *    -------------------------------
           05  RATFILCL PIC S9(0004) COMP.
           05  RATFILCF PIC  X(0001).
           05  FILLER REDEFINES RATFILCF.
               10  RATFILCA PIC  X(0001).
           05  RATFILCI PIC  X(0008).
      *    -------------------------------
           05  PRDMSTML PIC S9(0004) COMP.
           05  PRDMSTMF PIC  X(0001).
           05  FILLER REDEFINES PRDMSTMF.
               10  PRDMSTMA PIC  X(0001).
           05  PRDMSTMI PIC  X(0008).
      *    -------------------------------
           05  PRDMSTCL PIC S9(0004) COMP.
           05  PRDMSTCF PIC  X(0001).
           05  FILLER REDEFINES PRDMSTCF.
               10  PRDMSTCA PIC  X(0001).
           05  PRDMSTCI PIC  X(0008).
      *    -------------------------------
           05  RENTBLML PIC S9(0004) COMP.
           05  RENTBLMF PIC  X(0001).
           05  FILLER REDEFINES RENTBLMF.
               10  RENTBLMA PIC  X(0001).
           05  RENTBLMI PIC  X(0008).
      *    -------------------------------
           05  RENTBLCL PIC S9(0004) COMP.
           05  RENTBLCF PIC  X(0001).
           05  FILLER REDEFINES RENTBLCF.
               10  RENTBLCA PIC  X(0001).
           05  RENTBLCI PIC  X(0008).
      *    -------------------------------
           05  CHKCNTRL PIC S9(0004) COMP.
           05  CHKCNTRF PIC  X(0001).
           05  FILLER REDEFINES CHKCNTRF.
               10  CHKCNTRA PIC  X(0001).
           05  CHKCNTRI PIC  9(6).
      *    -------------------------------
           05  COMMSTML PIC S9(0004) COMP.
           05  COMMSTMF PIC  X(0001).
           05  FILLER REDEFINES COMMSTMF.
               10  COMMSTMA PIC  X(0001).
           05  COMMSTMI PIC  X(0008).
      *    -------------------------------
           05  COMMSTCL PIC S9(0004) COMP.
           05  COMMSTCF PIC  X(0001).
           05  FILLER REDEFINES COMMSTCF.
               10  COMMSTCA PIC  X(0001).
           05  COMMSTCI PIC  X(0008).
      *    -------------------------------
           05  QUECNTRL PIC S9(0004) COMP.
           05  QUECNTRF PIC  X(0001).
           05  FILLER REDEFINES QUECNTRF.
               10  QUECNTRA PIC  X(0001).
           05  QUECNTRI PIC  9(6).
      *    -------------------------------
           05  DERMSG1L PIC S9(0004) COMP.
           05  DERMSG1F PIC  X(0001).
           05  FILLER REDEFINES DERMSG1F.
               10  DERMSG1A PIC  X(0001).
           05  DERMSG1I PIC  X(0072).
      *    -------------------------------
           05  DERMSG2L PIC S9(0004) COMP.
           05  DERMSG2F PIC  X(0001).
           05  FILLER REDEFINES DERMSG2F.
               10  DERMSG2A PIC  X(0001).
           05  DERMSG2I PIC  X(0072).
      *    -------------------------------
           05  ENTRDPFL PIC S9(0004) COMP.
           05  ENTRDPFF PIC  X(0001).
           05  FILLER REDEFINES ENTRDPFF.
               10  ENTRDPFA PIC  X(0001).
           05  ENTRDPFI PIC  99.
       01  EL102DO REDEFINES EL102AI.
           05  FILLER            PIC  X(0012).
           05  FILLER            PIC  X(0003).
           05  DRUNDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DRUNTMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CODO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MIBSYMO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MIBVERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DAMRTCDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MIBCOMMO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MIBCSWO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DRESURO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATESWO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MIBTERMO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MIBTSWO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPCNTLO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LABCNTLO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNVDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUTOREFO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MGCNTLO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STARCHNO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SHFTLNOO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKPMTHO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCHNBRO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRPSWO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MRECONO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFNOO PIC  Z(16)9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTLANGO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCYLNKO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LNMESERO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FNMESERO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MNMESERO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCYCLE1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCYCLE2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCYCLE3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCYCLE4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCYCLE5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RSTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTUSERO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTTMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATFILMO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATFILCO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRDMSTMO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRDMSTCO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RENTBLMO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RENTBLCO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKCNTRO PIC  9(6).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMMSTMO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMMSTCO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  QUECNTRO PIC  9(6).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DERMSG1O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DERMSG2O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENTRDPFO PIC  X(0002).
      *    -------------------------------
       01  EL102EI REDEFINES EL102AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  ERUNDTEL PIC S9(0004) COMP.
           05  ERUNDTEF PIC  X(0001).
           05  FILLER REDEFINES ERUNDTEF.
               10  ERUNDTEA PIC  X(0001).
           05  ERUNDTEI PIC  X(0008).
      *    -------------------------------
           05  ERUNTMEL PIC S9(0004) COMP.
           05  ERUNTMEF PIC  X(0001).
           05  FILLER REDEFINES ERUNTMEF.
               10  ERUNTMEA PIC  X(0001).
           05  ERUNTMEI PIC  X(0005).
      *    -------------------------------
           05  COEL PIC S9(0004) COMP.
           05  COEF PIC  X(0001).
           05  FILLER REDEFINES COEF.
               10  COEA PIC  X(0001).
           05  COEI PIC  X(0003).
      *    -------------------------------
           05  USERIDEL PIC S9(0004) COMP.
           05  USERIDEF PIC  X(0001).
           05  FILLER REDEFINES USERIDEF.
               10  USERIDEA PIC  X(0001).
           05  USERIDEI PIC  X(0004).
      *    -------------------------------
           05  RTEFILML PIC S9(0004) COMP.
           05  RTEFILMF PIC  X(0001).
           05  FILLER REDEFINES RTEFILMF.
               10  RTEFILMA PIC  X(0001).
           05  RTEFILMI PIC  X(0008).
      *    -------------------------------
           05  RTEFILCL PIC S9(0004) COMP.
           05  RTEFILCF PIC  X(0001).
           05  FILLER REDEFINES RTEFILCF.
               10  RTEFILCA PIC  X(0001).
           05  RTEFILCI PIC  X(0008).
      *    -------------------------------
           05  COMTABML PIC S9(0004) COMP.
           05  COMTABMF PIC  X(0001).
           05  FILLER REDEFINES COMTABMF.
               10  COMTABMA PIC  X(0001).
           05  COMTABMI PIC  X(0008).
      *    -------------------------------
           05  COMTABCL PIC S9(0004) COMP.
           05  COMTABCF PIC  X(0001).
           05  FILLER REDEFINES COMTABCF.
               10  COMTABCA PIC  X(0001).
           05  COMTABCI PIC  X(0008).
      *    -------------------------------
           05  ACTMSTML PIC S9(0004) COMP.
           05  ACTMSTMF PIC  X(0001).
           05  FILLER REDEFINES ACTMSTMF.
               10  ACTMSTMA PIC  X(0001).
           05  ACTMSTMI PIC  X(0008).
      *    -------------------------------
           05  ACTMSTCL PIC S9(0004) COMP.
           05  ACTMSTCF PIC  X(0001).
           05  FILLER REDEFINES ACTMSTCF.
               10  ACTMSTCA PIC  X(0001).
           05  ACTMSTCI PIC  X(0008).
      *    -------------------------------
           05  RENTABML PIC S9(0004) COMP.
           05  RENTABMF PIC  X(0001).
           05  FILLER REDEFINES RENTABMF.
               10  RENTABMA PIC  X(0001).
           05  RENTABMI PIC  X(0008).
      *    -------------------------------
           05  RENTABCL PIC S9(0004) COMP.
           05  RENTABCF PIC  X(0001).
           05  FILLER REDEFINES RENTABCF.
               10  RENTABCA PIC  X(0001).
           05  RENTABCI PIC  X(0008).
      *    -------------------------------
           05  CMPMSTML PIC S9(0004) COMP.
           05  CMPMSTMF PIC  X(0001).
           05  FILLER REDEFINES CMPMSTMF.
               10  CMPMSTMA PIC  X(0001).
           05  CMPMSTMI PIC  X(0008).
      *    -------------------------------
           05  CMPMSTCL PIC S9(0004) COMP.
           05  CMPMSTCF PIC  X(0001).
           05  FILLER REDEFINES CMPMSTCF.
               10  CMPMSTCA PIC  X(0001).
           05  CMPMSTCI PIC  X(0008).
      *    -------------------------------
           05  EERMSG1L PIC S9(0004) COMP.
           05  EERMSG1F PIC  X(0001).
           05  FILLER REDEFINES EERMSG1F.
               10  EERMSG1A PIC  X(0001).
           05  EERMSG1I PIC  X(0072).
      *    -------------------------------
           05  EERMSG2L PIC S9(0004) COMP.
           05  EERMSG2F PIC  X(0001).
           05  FILLER REDEFINES EERMSG2F.
               10  EERMSG2A PIC  X(0001).
           05  EERMSG2I PIC  X(0072).
      *    -------------------------------
           05  ENTREPFL PIC S9(0004) COMP.
           05  ENTREPFF PIC  X(0001).
           05  FILLER REDEFINES ENTREPFF.
               10  ENTREPFA PIC  X(0001).
           05  ENTREPFI PIC  99.
       01  EL102EO REDEFINES EL102AI.
           05  FILLER            PIC  X(0012).
           05  FILLER            PIC  X(0003).
           05  ERUNDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERUNTMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COEO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDEO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTEFILMO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTEFILCO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMTABMO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMTABCO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTMSTMO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTMSTCO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RENTABMO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RENTABCO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPMSTMO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPMSTCO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EERMSG1O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EERMSG2O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENTREPFO PIC  X(0002).
      *    -------------------------------
00413      EJECT
       01  DFHEIV.                                                    
         02  DFHEIV0               PIC X(35).                         
         02  DFHEIV1               PIC X(08).                         
         02  DFHEIV2               PIC X(08).                         
         02  DFHEIV3               PIC X(08).                         
         02  DFHEIV4               PIC X(06).                         
         02  DFHEIV5               PIC X(04).                         
         02  DFHEIV6               PIC X(04).                         
         02  DFHEIV7               PIC X(02).                         
         02  DFHEIV8               PIC X(02).                         
         02  DFHEIV9               PIC X(01).                         
         02  DFHEIV10              PIC S9(7) COMP-3.                  
         02  DFHEIV11              PIC S9(4) COMP SYNC.               
         02  DFHEIV12              PIC S9(4) COMP SYNC.               
         02  DFHEIV13              PIC S9(4) COMP SYNC.               
         02  DFHEIV14              PIC S9(4) COMP SYNC.               
         02  DFHEIV15              PIC S9(4) COMP SYNC.               
         02  DFHEIV16              PIC S9(9) COMP SYNC.               
         02  DFHEIV17              PIC X(04).                         
         02  DFHEIV18              PIC X(04).                         
         02  DFHEIV19              PIC X(04).                         
         02  DFHEIV20              USAGE IS POINTER.                  
         02  DFHEIV21              USAGE IS POINTER.                  
         02  DFHEIV22              USAGE IS POINTER.                  
         02  DFHEIV23              USAGE IS POINTER.                  
         02  DFHEIV24              USAGE IS POINTER.                  
         02  DFHEIV25              PIC S9(9) COMP SYNC.               
         02  DFHEIV26              PIC S9(9) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(01) VALUE SPACE.             
       LINKAGE  SECTION.
       01  dfheiblk.
           02  eibtime          pic s9(7) comp-3.
           02  eibdate          pic s9(7) comp-3.
           02  eibtrnid         pic x(4).
           02  eibtaskn         pic s9(7) comp-3.
           02  eibtrmid         pic x(4).
           02  dfheigdi         pic s9(4) comp.
           02  eibcposn         pic s9(4) comp.
           02  eibcalen         pic s9(4) comp.
           02  eibaid           pic x(1).
           02  eibfiller1       pic x(1).
           02  eibfn            pic x(2).
           02  eibfiller2       pic x(2).
           02  eibrcode         pic x(6).
           02  eibfiller3       pic x(2).
           02  eibds            pic x(8).
           02  eibreqid         pic x(8).
           02  eibrsrce         pic x(8).
           02  eibsync          pic x(1).
           02  eibfree          pic x(1).
           02  eibrecv          pic x(1).
           02  eibsend          pic x(1).
           02  eibatt           pic x(1).
           02  eibeoc           pic x(1).
           02  eibfmh           pic x(1).
           02  eibcompl         pic x(1).
           02  eibsig           pic x(1).
           02  eibconf          pic x(1).
           02  eiberr           pic x(1).
           02  eibfiller4       pic x(1).
           02  eiberrcd         pic x(4).
           02  eibsynrb         pic x(1).
           02  eibnodat         pic x(1).
           02  eibfiller5       pic x(2).
           02  eibresp          pic 9(09) comp.
           02  eibresp2         pic 9(09) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
00415  01  DFHCOMMAREA             PIC X(1024).
00417 *01 PARMLIST .
00418 *    02  FILLER              PIC S9(8)   COMP.
00419 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.
00420      EJECT
00421 *    COPY ELCCNTL.
                        
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCNTL.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE            
00005 *                            VMOD=2.059                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = SYSTEM CONTROL FILE                       *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 750  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCNTL                        RKP=2,LEN=10    *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018 *                                                                 
00019  01  CONTROL-FILE.                                                
00020      12  CF-RECORD-ID                       PIC XX.               
00021          88  VALID-CF-ID                        VALUE 'CF'.       
00022                                                                   
00023      12  CF-CONTROL-PRIMARY.                                      
00024          16  CF-COMPANY-ID                  PIC XXX.              
00025          16  CF-RECORD-TYPE                 PIC X.                
00026              88  CF-COMPANY-MASTER              VALUE '1'.        
00027              88  CF-PROCESSOR-MASTER            VALUE '2'.        
00028              88  CF-STATE-MASTER                VALUE '3'.        
00029              88  CF-LF-BENEFIT-MASTER           VALUE '4'.        
00030              88  CF-AH-BENEFIT-MASTER           VALUE '5'.        
00031              88  CF-CARRIER-MASTER              VALUE '6'.        
00032              88  CF-MORTALITY-MASTER            VALUE '7'.        
00033              88  CF-BUSINESS-TYPE-MASTER        VALUE '8'.        
00034              88  CF-TERMINAL-MASTER             VALUE '9'.        
00035              88  CF-AH-EDIT-MASTER              VALUE 'A'.        
00036              88  CF-CREDIBILITY-FACTOR-MASTER   VALUE 'B'.        
00037              88  CF-CUSTOM-REPORT-MASTER        VALUE 'C'.        
00038              88  CF-MORTGAGE-HT-WT-CHART        VALUE 'H'.        
00039              88  CF-LIFE-EDIT-MASTER            VALUE 'L'.        
00040              88  CF-MORTGAGE-PLAN-MASTER        VALUE 'M'.        
00041              88  CF-MORTGAGE-COMPANY-MASTER     VALUE 'N'.        
00042              88  CF-REMINDERS-MASTER            VALUE 'R'.        
00043              88  CF-AUTO-ACTIVITY-MASTER        VALUE 'T'.        
00044          16  CF-ACCESS-CD-GENL              PIC X(4).             
00045          16  CF-ACCESS-OF-PROCESSOR  REDEFINES CF-ACCESS-CD-GENL. 
00046              20  CF-PROCESSOR               PIC X(4).             
00047          16  CF-ACCESS-OF-STATE  REDEFINES  CF-ACCESS-CD-GENL.    
00048              20  CF-STATE-CODE              PIC XX.               
00049              20  FILLER                     PIC XX.               
00050          16  CF-ACCESS-OF-BENEFIT  REDEFINES  CF-ACCESS-CD-GENL.  
00051              20  FILLER                     PIC XX.               
00052              20  CF-HI-BEN-IN-REC           PIC XX.               
00053          16  CF-ACCESS-OF-CARRIER  REDEFINES  CF-ACCESS-CD-GENL.  
00054              20  FILLER                     PIC XXX.              
00055              20  CF-CARRIER-CNTL            PIC X.                
00056          16  CF-ACCESS-OF-BUS-TYPE REDEFINES  CF-ACCESS-CD-GENL.  
00057              20  FILLER                     PIC XX.               
00058              20  CF-HI-TYPE-IN-REC          PIC 99.               
00059          16  CF-ACCESS-OF-CRDB-TBL REDEFINES  CF-ACCESS-CD-GENL.  
00060              20  CF-CRDB-TABLE-INDICATOR    PIC X.                
00061                  88  CF-CRDB-NAIC-TABLE         VALUE '9'.        
00062              20  CF-CRDB-BENEFIT-TYPE       PIC X.                
00063              20  CF-CRDB-WAITING-PERIOD     PIC XX.               
00064          16  CF-ACCESS-OF-CUST-RPT REDEFINES  CF-ACCESS-CD-GENL.  
00065              20  FILLER                     PIC X.                
00066              20  CF-CUSTOM-REPORT-NO        PIC 999.              
00067          16  CF-ACCESS-OF-PLAN   REDEFINES  CF-ACCESS-CD-GENL.    
00068              20  FILLER                     PIC XX.               
00069              20  CF-MORTGAGE-PLAN           PIC XX.               
00070          16  CF-SEQUENCE-NO                 PIC S9(4)   COMP.     
00071                                                                   
00072      12  CF-LAST-MAINT-DT                   PIC XX.               
00073      12  CF-LAST-MAINT-BY                   PIC X(4).             
00074      12  CF-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.   
00075                                                                   
00076      12  CF-RECORD-BODY                     PIC X(728).           
00077                                                                   
00078                                                                   
00079 ****************************************************************  
00080 *             COMPANY MASTER RECORD                            *  
00081 ****************************************************************  
00082                                                                   
00083      12  CF-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.        
00084          16  CF-COMPANY-ADDRESS.                                  
00085              20  CF-CL-MAIL-TO-NAME         PIC X(30).            
00086              20  CF-CL-IN-CARE-OF           PIC X(30).            
00087              20  CF-CL-ADDR-LINE-1          PIC X(30).            
00088              20  CF-CL-ADDR-LINE-2          PIC X(30).            
00089              20  CF-CL-CITY-STATE           PIC X(30).            
00090              20  CF-CL-ZIP-CODE-NUM         PIC 9(9)    COMP-3.   
00091              20  CF-CL-PHONE-NO             PIC 9(11)   COMP-3.   
00092          16  CF-COMPANY-CD                  PIC X.                
00093          16  CF-COMPANY-PASSWORD            PIC X(8).             
00094          16  CF-SECURITY-OPTION             PIC X.                
00095              88  ALL-SECURITY                   VALUE '1'.        
00096              88  COMPANY-VERIFY                 VALUE '2'.        
00097              88  PROCESSOR-VERIFY               VALUE '3'.        
00098              88  NO-SECURITY                    VALUE '4'.        
00099              88  ALL-BUT-TERM                   VALUE '5'.        
00100          16  CF-CARRIER-CONTROL-LEVEL       PIC X.                
00101              88  USE-ACTUAL-CARRIER             VALUE SPACE.      
00102          16  CF-LGX-INTERFACE-CNTL          PIC X.                
00103              88  LGX-TIME-SHR-COMPANY           VALUE '1'.        
00104          16  CF-INFORCE-LOCATION            PIC X.                
00105              88  CERTS-ARE-ONLINE               VALUE '1'.        
00106              88  CERTS-ARE-OFFLINE              VALUE '2'.        
00107              88  NO-CERTS-AVAILABLE             VALUE '3'.        
00108          16  CF-LOWER-CASE-LETTERS          PIC X.                
00109          16  CF-CERT-ACCESS-CONTROL         PIC X.                
00110              88  CF-ST-ACCNT-CNTL               VALUE ' '.        
00111              88  CF-CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.        
00112              88  CF-CARR-ST-ACCNT-CNTL          VALUE '2'.        
00113              88  CF-ACCNT-CNTL                  VALUE '3'.        
00114              88  CF-CARR-ACCNT-CNTL             VALUE '4'.        
00115                                                                   
00116          16  CF-FORMS-PRINTER-ID            PIC X(4).             
00117          16  CF-CHECK-PRINTER-ID            PIC X(4).             
00118                                                                   
00119          16  CF-LGX-CREDIT-USER             PIC X.                
00120              88  CO-IS-NOT-USER                 VALUE 'N'.        
00121              88  CO-HAS-CLAS-IC-CREDIT          VALUE 'Y'.        
00122                                                                   
00123          16 CF-CREDIT-CALC-CODES.                                 
00124              20  CF-CR-REM-TERM-CALC PIC X.                       
00125                88  CR-EARN-AFTER-15TH           VALUE '1'.        
00126                88  CR-EARN-ON-HALF-MO           VALUE '2'.        
00127                88  CR-EARN-ON-1ST-DAY           VALUE '3'.        
00128                88  CR-EARN-ON-FULL-MO           VALUE '4'.        
00129                88  CR-EARN-WITH-NO-DAYS         VALUE '5'.        
00130                88  CR-EARN-AFTER-14TH           VALUE '6'.        
00131                88  CR-EARN-AFTER-16TH           VALUE '7'.        
00132              20  CF-CR-R78-METHOD           PIC X.                
00133                88  USE-TERM-PLUS-ONE            VALUE SPACE.      
00134                88  DONT-USE-PLUS-ONE            VALUE '1'.        
00135                                                                   
00136          16  CF-CLAIM-CONTROL-COUNTS.                             
00137              20  CF-CO-CLAIM-COUNTER        PIC S9(8)   COMP.     
00138                  88  CO-CLM-COUNT-RESET         VALUE +99999.     
00139                                                                   
00140              20  CF-CO-ARCHIVE-COUNTER      PIC S9(8)   COMP.     
00141                  88  CO-ARCHIVE-COUNT-RESET     VALUE +999999.    
00142                                                                   
00143              20  CF-CO-CHECK-COUNTER        PIC S9(8)   COMP.     
00144                  88  CO-CHECK-COUNT-RESET       VALUE +9999999.   
00145                                                                   
00146              20  CF-CO-CHECK-QUE-COUNTER    PIC S9(8)   COMP.     
00147                  88  CO-QUE-COUNT-RESET         VALUE +9999999.   
00148                                                                   
00149          16  CF-CURRENT-MONTH-END           PIC XX.               
00150                                                                   
00151          16  CF-CO-CALC-QUOTE-TOLERANCE.                          
00152              20  CF-CO-TOL-CLAIM            PIC S999V99   COMP-3. 
00153              20  CF-CO-TOL-PREM             PIC S999V99   COMP-3. 
00154              20  CF-CO-TOL-REFUND           PIC S999V99   COMP-3. 
00155              20  CF-CO-CLAIM-REJECT-SW      PIC X.                
00156                  88 CO-WARN-IF-CLAIM-OUT        VALUE SPACE.      
00157                  88 CO-FORCE-IF-CLAIM-OUT       VALUE '1'.        
00158              20  CF-CO-PREM-REJECT-SW       PIC X.                
00159                  88 CO-WARN-IF-PREM-OUT         VALUE SPACE.      
00160                  88 CO-FORCE-IF-PREM-OUT        VALUE '1'.        
00161              20  CF-CO-REF-REJECT-SW        PIC X.                
00162                  88 CO-WARN-IF-REF-OUT          VALUE SPACE.      
00163                  88 CO-FORCE-IF-REF-OUT         VALUE '1'.        
00164                                                                   
00165          16  CF-CO-REPORTING-DT             PIC XX.               
00166          16  CF-CO-REPORTING-MONTH-DT       PIC XX.               
00167          16  CF-CO-REPORTING-MONTH-END-SW   PIC X.                
00168            88  CF-CO-NOT-MONTH-END              VALUE SPACES.     
00169            88  CF-CO-MONTH-END                  VALUE '1'.        
00170                                                                   
00171          16  CF-LGX-CLAIM-USER              PIC X.                
00172              88  CO-IS-NOT-CLAIM-USER           VALUE 'N'.        
00173              88  CO-HAS-CLAS-IC-CLAIM           VALUE 'Y'.        
00174                                                                   
00175          16  CF-CREDIT-EDIT-CONTROLS.                             
00176              20  CF-MIN-PREMIUM             PIC S999V99   COMP-3. 
00177              20  CF-MIN-AGE                 PIC 99.               
00178              20  CF-DEFAULT-AGE             PIC 99.               
00179              20  CF-MIN-TERM                PIC S999      COMP-3. 
00180              20  CF-MAX-TERM                PIC S999      COMP-3. 
00181              20  CF-DEFAULT-SEX             PIC X.                
00182              20  CF-JOINT-AGE-INPUT         PIC X.                
00183                  88 CF-JOINT-AGE-IS-INPUT       VALUE '1'.        
00184              20  CF-BIRTH-DATE-INPUT        PIC X.                
00185                  88 CF-BIRTH-DATE-IS-INPUT      VALUE '1'.        
00186              20  CF-CAR-GROUP-ACCESS-CNTL   PIC X.                
00187                  88  CF-USE-ACTUAL-CARRIER      VALUE ' '.        
00188                  88  CF-ZERO-CARRIER            VALUE '1'.        
00189                  88  CF-ZERO-GROUPING           VALUE '2'.        
00190                  88  CF-ZERO-CAR-GROUP          VALUE '3'.        
00191              20  CF-EDIT-SW                 PIC X.                
00192                  88  CF-START-EDIT-TONIGHT      VALUE '1'.        
00193              20  CF-EDIT-RESTART-BATCH      PIC X(6).             
00194              20  CF-CR-PR-METHOD            PIC X.                
00195                88  USE-NORMAL-PR-METHOD         VALUE SPACE.      
00196                88  ADJUST-ORIG-TERM-BY-5        VALUE '1'.        
00197              20  FILLER                     PIC X.                
00198                                                                   
00199          16  CF-CREDIT-MISC-CONTROLS.                             
00200              20  CF-REIN-TABLE-SW           PIC X.                
00201                  88 REIN-TABLES-ARE-USED        VALUE '1'.        
00202              20  CF-COMP-TABLE-SW           PIC X.                
00203                  88 COMP-TABLES-ARE-USED        VALUE '1'.        
00204              20  CF-EXPERIENCE-RETENTION-AGE                      
00205                                             PIC S9        COMP-3. 
00206              20  CF-CONVERSION-DT           PIC XX.               
00207              20  CF-COMP-WRITE-OFF-AMT      PIC S999V99   COMP-3. 
00208              20  CF-RUN-FREQUENCY-SW        PIC X.                
00209                  88 CO-IS-PROCESSED-MONTHLY     VALUE SPACE.      
00210                  88 CO-IS-PROCESSED-ON-QTR      VALUE '1'.        
00211                                                                   
00212              20  CF-CR-CHECK-NO-CONTROL.                          
00213                  24  CF-CR-CHECK-NO-METHOD    PIC X.              
00214                      88  CR-CHECK-NO-MANUAL       VALUE '1'.      
00215                      88  CR-CHECK-NO-AUTO-SEQ     VALUE '2'.      
00216                      88  CR-CHECK-NO-AT-PRINT     VALUE '4'.      
00217                  24  CF-CR-CHECK-COUNTER      PIC S9(8)   COMP.   
00218                      88  CR-CHECK-CNT-RESET-VALUE VALUE +999999.  
00219                                                                   
00220                  24  CF-CR-CHECK-COUNT       REDEFINES            
00221                      CF-CR-CHECK-COUNTER      PIC X(4).           
00222                                                                   
00223                  24  CF-CR-CHECK-QUE-COUNTER  PIC S9(8)  COMP.    
00224                      88  CR-QUE-COUNT-RESET      VALUE +9999999.  
00225                                                                   
00226                  24  CF-CR-CHECK-QUE-COUNT   REDEFINES            
00227                      CF-CR-CHECK-QUE-COUNTER  PIC X(4).           
00228                  24  CF-MAIL-PROCESSING       PIC X.              
00229                      88  MAIL-PROCESSING          VALUE 'Y'.      
00230                                                                   
00231          16  CF-MISC-SYSTEM-CONTROL.                              
00232              20  CF-SYSTEM-C                 PIC X.               
00233                  88  CONFIRMATION-SYS-USED       VALUE '1'.       
00234              20  CF-SYSTEM-D                 PIC X.               
00235                  88  DAILY-BILL-SYS-USED         VALUE '1'.       
00236              20  CF-SOC-SEC-NO-SW            PIC X.               
00237                  88  SOC-SEC-NO-USED             VALUE '1'.       
00238              20  CF-MEMBER-NO-SW             PIC X.               
00239                  88  MEMBER-NO-USED              VALUE '1'.       
00240              20  CF-TAX-ID-NUMBER            PIC X(11).           
00241              20  CF-JOURNAL-FILE-ID          PIC S9(4) COMP.      
00242              20  CF-PAYMENT-APPROVAL-SW      PIC X.               
00243                  88  CF-PMT-APPROVAL-USED        VALUE 'Y' 'G'.   
00244                  88  CF-NO-APPROVAL              VALUE ' ' 'N'.   
00245                  88  CF-ALL-APPROVED             VALUE 'Y'.       
00246                  88  CF-GRADUATED-APPROVAL       VALUE 'G'.       
00247              20  CF-SYSTEM-E                 PIC X.               
00248                  88  CF-AR-SYSTEM-USED           VALUE 'Y'.       
00249                                                                   
00250          16  CF-LGX-LIFE-USER               PIC X.                
00251              88  CO-IS-NOT-LIFE-USER            VALUE 'N'.        
00252              88  CO-HAS-CLAS-IC-LIFE            VALUE 'Y'.        
00253                                                                   
00254          16  CF-CR-MONTH-END-DT             PIC XX.               
00255                                                                   
00256          16  CF-FILE-MAINT-DATES.                                 
00257              20  CF-LAST-BATCH-NO           PIC S9(8)   COMP.     
00258                  88  CF-LAST-BATCH-RESET        VALUE +999999.    
00259              20  CF-LAST-BATCH       REDEFINES                    
00260                  CF-LAST-BATCH-NO               PIC X(4).         
00261              20  CF-RATES-FILE-MAINT-DT         PIC XX.           
00262              20  CF-RATES-FILE-CREATE-DT        PIC XX.           
00263              20  CF-COMMISSION-TAB-MAINT-DT     PIC XX.           
00264              20  CF-COMMISSION-TAB-CREATE-DT    PIC XX.           
00265              20  CF-ACCOUNT-MSTR-MAINT-DT       PIC XX.           
00266              20  CF-ACCOUNT-MSTR-CREATE-DT      PIC XX.           
00267              20  CF-REINSURANCE-TAB-MAINT-DT    PIC XX.           
00268              20  CF-REINSURANCE-TAB-CREATE-DT   PIC XX.           
00269              20  CF-COMPENSATION-MSTR-MAINT-DT  PIC XX.           
00270              20  CF-COMPENSATION-MSTR-CREATE-DT PIC XX.           
00271                                                                   
00272          16  CF-NEXT-COMPANY-ID             PIC XXX.              
00273          16  FILLER                         PIC X.                
00274                                                                   
00275          16  CF-ALT-MORT-CODE               PIC X(4).             
00276          16  CF-MEMBER-CAPTION              PIC X(10).            
00277                                                                   
00278          16  CF-LIFE-ACCESS-CONTROL         PIC X.                
00279              88  CF-LIFE-ST-ACCNT-CNTL          VALUE ' '.        
00280              88  CF-LIFE-CARR-GRP-ST-ACCNT-CNTL VALUE '1'.        
00281              88  CF-LIFE-CARR-ST-ACCNT-CNTL     VALUE '2'.        
00282              88  CF-LIFE-ACCNT-CNTL             VALUE '3'.        
00283              88  CF-LIFE-CARR-ACCNT-CNTL        VALUE '4'.        
00284                                                                   
00285          16  CF-STARTING-ARCH-NO            PIC S9(8) COMP.       
00286                                                                   
00287          16  CF-LIFE-OVERRIDE-L1            PIC X.                
00288          16  CF-LIFE-OVERRIDE-L2            PIC XX.               
00289          16  CF-LIFE-OVERRIDE-L6            PIC X(6).             
00290          16  CF-LIFE-OVERRIDE-L12           PIC X(12).            
00291                                                                   
00292          16  CF-AH-OVERRIDE-L1              PIC X.                
00293          16  CF-AH-OVERRIDE-L2              PIC XX.               
00294          16  CF-AH-OVERRIDE-L6              PIC X(6).             
00295          16  CF-AH-OVERRIDE-L12             PIC X(12).            
00296                                                                   
00297          16  CF-REPORT-CD1-CAPTION          PIC X(10).            
00298          16  CF-REPORT-CD2-CAPTION          PIC X(10).            
00299                                                                   
00300          16  CF-CLAIM-CUTOFF-DATE           PIC XX.               
00301          16  CF-AR-LAST-EL860-DT            PIC XX.               
00302          16  CF-MP-MONTH-END-DT             PIC XX.               
00303                                                                   
00304          16  CF-MAX-NUM-PMTS-CHECK          PIC 99.               
00305          16  CF-CLAIM-PAID-THRU-TO          PIC X.                
00306              88  CF-CLAIM-PAID-TO               VALUE '1'.        
00307                                                                   
00308          16  CF-AR-MONTH-END-DT             PIC XX.               
00309                                                                   
00310          16  CF-CRDTCRD-USER                PIC X.                
00311              88  CO-IS-NOT-CRDTCRD-USER         VALUE 'N'.        
00312              88  CO-HAS-CLAS-IC-CRDTCRD         VALUE 'Y'.        
00313                                                                   
00314          16  CF-CC-MONTH-END-DT             PIC XX.               
00315                                                                   
00316          16  CF-PRINT-ADDRESS-LABELS        PIC X.                
00317                                                                   
00318          16  CF-MORTALITY-AGE-CALC-METHOD   PIC X.                
00319              88  CF-USE-TABLE-ASSIGNED-METHOD   VALUE '1' ' '.    
00320              88  CF-USE-ALL-AGE-LAST            VALUE '2'.        
00321              88  CF-USE-ALL-AGE-NEAR            VALUE '3'.        
00322          16  CF-CO-TOL-PREM-PCT             PIC S9V9(4)   COMP-3. 
00323          16  CF-CO-TOL-REFUND-PCT           PIC S9V9(4)   COMP-3. 
00324          16  CF-CO-TOL-CAP                  PIC S9(3)V99  COMP-3. 
00325          16  CF-CO-RESERVE-OPTION-SWITCH    PIC  X.               
00326              88  OPTIONAL-RESERVE-METHOD-AUTH    VALUE 'Y'.       
00327              88  OPTIONAL-RESERVE-MTHD-NOT-AUTH  VALUE ' ' 'N'.   
00328          16  CF-CO-IBNR-LAG-MONTHS          PIC S9(3)     COMP-3. 
00329          16  CF-CO-CIDA-TABLE-DISCOUNT-PCT  PIC S9V9(4)   COMP-3. 
00330          16  CF-CO-CRDB-TABLE-SELECTION     PIC  X.               
00331              88  NIAC-CREDIBILITY-TABLE          VALUE '9'.       
00332          16  CF-CO-ST-CALL-RPT-CNTL         PIC  X.               
00333                                                                   
00334          16  CF-CL-ZIP-CODE.                                      
00335              20  CF-CL-ZIP-PRIME.                                 
00336                  24  CF-CL-ZIP-1ST          PIC X.                
00337                      88  CF-CL-CAN-POST-CODE  VALUE 'A' THRU 'Z'. 
00338                  24  FILLER                 PIC X(4).             
00339              20  CF-CL-ZIP-PLUS4            PIC X(4).             
00340          16  CF-CL-CANADIAN-POSTAL-CODE REDEFINES CF-CL-ZIP-CODE. 
00341              20  CF-CL-CAN-POSTAL-1         PIC XXX.              
00342              20  CF-CL-CAN-POSTAL-2         PIC XXX.              
00343              20  FILLER                     PIC XXX.              
00344                                                                   
00345          16  CF-CO-CALCULATION-INTEREST     PIC S9V9(4)  COMP-3.  
00346          16  CF-CO-IBNR-AH-FACTOR           PIC S9V9(4)  COMP-3.  
00347          16  CF-CO-IBNR-LIFE-FACTOR         PIC S9V9(4)  COMP-3.  
00348          16  CF-CO-OPTION-START-DATE        PIC XX.               
00349          16  CF-REM-TRM-CALC-OPTION         PIC X.                
00350            88  CF-VALID-REM-TRM-OPTION          VALUE '1' '2'     
00351                                                       '3' '4'.    
00352            88  CF-CONSIDER-EXTENSION            VALUE '3' '4'.    
00353            88  CF-30-DAY-MONTH                  VALUE '1' '3'.    
00354            88  CF-NO-EXT-30-DAY-MONTH           VALUE '1'.        
00355            88  CF-NO-EXT-ACTUAL-DAYS            VALUE '2'.        
00356            88  CF-EXT-30-DAY-MONTH              VALUE '3'.        
00357            88  CF-EXT-ACTUAL-DAYS               VALUE '4'.        
00358                                                                   
00359          16  CF-DEFAULT-APR                 PIC S999V9(4) COMP-3. 
00360                                                                   
00361          16  CF-PAYMENT-APPROVAL-LEVELS.                          
00362              20  CF-LIFE-PAY-APP-LEVEL-1    PIC S9(7)   COMP-3.   
00363              20  CF-LIFE-PAY-APP-LEVEL-2    PIC S9(7)   COMP-3.   
00364              20  CF-LIFE-PAY-APP-LEVEL-3    PIC S9(7)   COMP-3.   
00365              20  CF-AH-PAY-APP-LEVEL-1      PIC S9(7)   COMP-3.   
00366              20  CF-AH-PAY-APP-LEVEL-2      PIC S9(7)   COMP-3.   
00367              20  CF-AH-PAY-APP-LEVEL-3      PIC S9(7)   COMP-3.   
00368                                                                   
00369          16  CF-END-USER-REPORTING-USER     PIC X.                
00370              88  CO-NO-END-USER-REPORTING       VALUE 'N'.        
00371              88  CO-USES-END-USER-REPORTING     VALUE 'Y'.        
00372                                                                   
00373          16  CF-CLAIMS-CHECK-RECON-USER     PIC X.                
00374              88  CO-NO-USE-CLAIMS-RECON         VALUE 'N'.        
00375              88  CO-USES-CLAIMS-RECON           VALUE 'Y'.        
00376                                                                   
00377          16  CF-CLAIMS-LAST-PROCESS-DT      PIC XX.               
00378                                                                   
00379          16  FILLER                         PIC X(4).             
00380                                                                   
00381          16  CF-CREDIT-ARCHIVE-CNTL.                              
00382              20  CF-CREDIT-LAST-ARCH-NUM    PIC S9(9)  COMP-3.    
00383              20  CF-CREDIT-START-ARCH-NUM   PIC S9(9)  COMP-3.    
00384              20  CF-CREDIT-ARCH-PURGE-YR    PIC S9     COMP-3.    
00385                                                                   
00386          16  CF-CR-PRINT-ADDRESS-LABELS     PIC X.                
00387                                                                   
00388          16  CF-CLAIMS-AUDIT-CHANGES        PIC X.                
00389              88  CO-NO-USE-AUDIT-CHANGES        VALUE 'N'.        
00390              88  CO-USES-AUDIT-CHANGES          VALUE 'Y'.        
00391                                                                   
00392          16  CF-CLAIMS-CREDIT-CARD-INDEX    PIC X.                
00393              88  CO-NO-USE-CREDIT-CARD-INDEX    VALUE 'N'.        
00394              88  CO-USES-CREDIT-CARD-INDEX      VALUE 'Y'.        
00395                                                                   
00396          16  CF-CLAIMS-LETTER-MAINT-DAYS    PIC 99.               
00397                                                                   
00398          16  CF-CO-ACH-ID-CODE              PIC  X.               
00399              88  CF-CO-ACH-ICD-IRS-EIN          VALUE '1'.        
00400              88  CF-CO-ACH-ICD-DUNS             VALUE '2'.        
00401              88  CF-CO-ACH-ICD-USER-NO          VALUE '3'.        
00402          16  CF-CO-ACH-CLAIM-SEND-NAME      PIC X(23).            
00403          16  CF-CO-ACH-CLAIM-BK-NO          PIC X(09).            
00404          16  CF-CO-ACH-ADMIN-SEND-NAME      PIC X(23).            
00405          16  CF-CO-ACH-ADMIN-NO             PIC X(09).            
00406          16  CF-CO-ACH-RECV-NAME            PIC X(23).            
00407          16  CF-CO-ACH-RECV-NO              PIC X(08).            
00408          16  CF-CO-ACH-ORIGINATOR-NO        PIC X(08).            
00409          16  CF-CO-ACH-COMPANY-ID           PIC X(09).            
00410          16  CF-CO-ACH-TRACE-NO             PIC 9(07) COMP.       
00411                  88  CO-ACH-TRACE-NO-RESET      VALUE 9999999.    
00412          16  CF-CO-ACH-TRACE-SPACE REDEFINES                      
00413                  CF-CO-ACH-TRACE-NO         PIC X(4).             
00414                                                                   
00415          16  CF-CO-OVER-SHORT.                                    
00416              20 CF-CO-OVR-SHT-AMT           PIC S999V99   COMP-3. 
00417              20 CF-CO-OVR-SHT-PCT           PIC S9V9(4)   COMP-3. 
00418                                                                   
00419          16  FILLER                         PIC X(102).           
00420                                                                   
00421 ****************************************************************  
00422 *             PROCESSOR/USER RECORD                            *  
00423 ****************************************************************  
00424                                                                   
00425      12  CF-PROCESSOR-MASTER-REC  REDEFINES  CF-RECORD-BODY.      
00426          16  CF-PROCESSOR-NAME              PIC X(30).            
00427          16  CF-PROCESSOR-PASSWORD          PIC X(11).            
00428          16  CF-PROCESSOR-TITLE             PIC X(26).            
00429          16  CF-MESSAGE-AT-LOGON-CAP        PIC X.                
00430                  88  MESSAGE-YES                VALUE 'Y'.        
00431                  88  MESSAGE-NO                 VALUE ' ' 'N'.    
00432                                                                   
00433 *****************************************************             
00434 ****  OCCURRENCE (1) CREDIT APPLICATIONS         ****             
00435 ****  OCCURRENCE (2) CLAIMS APPLICATIONS         ****             
00436 ****  OCCURRENCE (3) CREDIT CARD APPLICATIONS    ****             
00437 ****  OCCURRENCE (4) ACCT RECV APPLICATIONS      ****             
00438 *****************************************************             
00439                                                                   
00440          16  CF-SYSTEM-SECURITY  OCCURS  4 TIMES.                 
00441              20  CF-ADMINISTRATION-CONTROLS PIC XX.               
00442              20  CF-APPLICATION-FORCE       PIC X.                
00443              20  CF-INDIVIDUAL-APP.                               
00444                  24  CF-APP-SWITCHES  OCCURS  44 TIMES.           
00445                      28  CF-BROWSE-APP      PIC X.                
00446                      28  CF-UPDATE-APP      PIC X.                
00447                                                                   
00448          16  CF-CURRENT-TERM-ON             PIC X(4).             
00449          16  CF-PROCESSOR-LIMITS-CLAIMS.                          
00450              20  CF-PROC-CALC-AMT-TOL       PIC S999V99   COMP-3. 
00451              20  CF-PROC-MAX-REG-PMT        PIC S9(7)V99  COMP-3. 
00452              20  CF-PROC-MAX-REG-DAYS       PIC S999      COMP-3. 
00453              20  CF-PROC-MAX-AUTO-PMT       PIC S9(7)V99  COMP-3. 
00454              20  CF-PROC-MAX-AUTO-MOS       PIC S999      COMP-3. 
00455              20  CF-PROC-CALC-DAYS-TOL      PIC S999      COMP-3. 
00456              20  CF-PROC-MAX-LF-PMT         PIC S9(7)V99  COMP-3. 
00457          16  CF-PROCESSOR-CARRIER           PIC X.                
00458              88  NO-CARRIER-SECURITY            VALUE ' '.        
00459          16  CF-PROCESSOR-ACCOUNT           PIC X(10).            
00460              88  NO-ACCOUNT-SECURITY            VALUE SPACES.     
00461          16  CF-PROCESSOR-LIFE-ACCESS       PIC X.                
00462              88  PROCESSOR-HAS-LIFE-ACCESS      VALUE 'Y'.        
00463          16  CF-PROCESSOR-USER-ALMIGHTY     PIC X.                
00464              88  PROCESSOR-USER-IS-ALMIGHTY     VALUE 'Y'.        
00465                                                                   
00466          16  CF-PROC-SYS-ACCESS-SW.                               
00467              20  CF-PROC-CREDIT-CLAIMS-SW.                        
00468                  24  CF-PROC-SYS-ACCESS-CREDIT  PIC X.            
00469                      88  ACCESS-TO-CREDIT           VALUE 'Y'.    
00470                  24  CF-PROC-SYS-ACCESS-CLAIMS  PIC X.            
00471                      88  ACCESS-TO-CLAIMS           VALUE 'Y'.    
00472              20  CF-PROC-CREDIT-CLAIMS   REDEFINES                
00473                  CF-PROC-CREDIT-CLAIMS-SW       PIC XX.           
00474                  88  ACCESS-TO-CLAIM-CREDIT         VALUE 'YY'.   
00475              20  CF-PROC-LIFE-GNRLDGR-SW.                         
00476                  24  CF-PROC-SYS-ACCESS-LIFE    PIC X.            
00477                      88  ACCESS-TO-LIFE             VALUE 'Y'.    
00478                  24  CF-PROC-SYS-ACCESS-GNRLDGR PIC X.            
00479                      88  ACCESS-TO-GNRLDGR          VALUE 'Y'.    
00480              20  CF-PROC-LIFE-GNRLDGR    REDEFINES                
00481                  CF-PROC-LIFE-GNRLDGR-SW        PIC XX.           
00482                  88  ACCESS-TO-LIFE-GNRLDGR         VALUE 'YY'.   
00483          16  CF-PROC-SYS-ACCESS-ALL      REDEFINES                
00484              CF-PROC-SYS-ACCESS-SW              PIC X(4).         
00485              88  ACCESS-TO-ALL-SYSTEMS              VALUE 'YYYY'. 
00486          16  CF-PROCESSOR-PRINTER               PIC X(4).         
00487                                                                   
00488          16  CF-APPROVAL-LEVEL                  PIC X.            
00489              88  APPROVAL-LEVEL-1                   VALUE '1'.    
00490              88  APPROVAL-LEVEL-2                   VALUE '2'.    
00491              88  APPROVAL-LEVEL-3                   VALUE '3'.    
00492                                                                   
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3. 
00494                                                                   
00495          16  CF-LANGUAGE-TYPE                   PIC X.            
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.    
00497              88  CF-LANG-IS-FR                      VALUE 'F'.    
00498          16  FILLER                             PIC  X(240).      
00499                                                                   
00500 ****************************************************************  
00501 *             PROCESSOR/REMINDERS RECORD                       *  
00502 ****************************************************************  
00503                                                                   
00504      12  CF-PROCESSOR-REMINDER-REC  REDEFINES  CF-RECORD-BODY.    
00505          16  CF-PROCESSOR-REMINDERS  OCCURS 8 TIMES.              
00506              20  CF-START-REMIND-DT         PIC XX.               
00507              20  CF-END-REMIND-DT           PIC XX.               
00508              20  CF-REMINDER-TEXT           PIC X(50).            
00509          16  FILLER                         PIC X(296).           
00510                                                                   
00511                                                                   
00512 ****************************************************************  
00513 *             STATE MASTER RECORD                              *  
00514 ****************************************************************  
00515                                                                   
00516      12  CF-STATE-MASTER-REC  REDEFINES  CF-RECORD-BODY.          
00517          16  CF-STATE-ABBREVIATION          PIC XX.               
00518          16  CF-STATE-NAME                  PIC X(25).            
00519          16  CF-ST-CALC-INTEREST            PIC S9V9(4)   COMP-3. 
00520          16  CF-ST-CALC-QUOTE-TOLERANCE.                          
00521              20  CF-ST-TOL-CLAIM            PIC S999V99   COMP-3. 
00522              20  CF-ST-TOL-PREM             PIC S999V99   COMP-3. 
00523              20  CF-ST-TOL-REFUND           PIC S999V99   COMP-3. 
00524              20  CF-ST-CLAIM-REJECT-SW      PIC X.                
00525                  88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.      
00526                  88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.        
00527              20  CF-ST-PREM-REJECT-SW       PIC X.                
00528                  88 ST-WARN-IF-PREM-OUT         VALUE SPACE.      
00529                  88 ST-FORCE-IF-PREM-OUT        VALUE '1'.        
00530              20  CF-ST-REF-REJECT-SW        PIC X.                
00531                  88 ST-WARN-IF-REF-OUT          VALUE SPACE.      
00532                  88 ST-FORCE-IF-REF-OUT         VALUE '1'.        
00533          16  CF-ST-LF-EXP-PCT               PIC S999V9(4) COMP-3. 
00534          16  CF-ST-AH-EXP-PCT               PIC S999V9(4) COMP-3. 
00535          16  CF-ST-REFUND-RULES.                                  
00536              20  CF-ST-REFUND-MIN           PIC S999V99    COMP-3.
00537              20  CF-ST-REFUND-DAYS-FIRST    PIC 99.               
00538              20  CF-ST-REFUND-DAYS-SUBSEQ   PIC 99.               
00539          16  CF-ST-FST-PMT-EXTENSION.                             
00540              20  CF-ST-FST-PMT-DAYS-MAX     PIC 999.              
00541              20  CF-ST-FST-PMT-DAYS-CHG     PIC X.                
00542                  88  CF-ST-EXT-NO-CHG           VALUE ' '.        
00543                  88  CF-ST-EXT-CHG-LF           VALUE '1'.        
00544                  88  CF-ST-EXT-CHG-AH           VALUE '2'.        
00545                  88  CF-ST-EXT-CHG-LF-AH        VALUE '3'.        
00546          16  CF-ST-STATE-CALL.                                    
00547              20  CF-ST-CALL-UNEARNED        PIC X.                
00548              20  CF-ST-CALL-RPT-CNTL        PIC X.                
00549              20  CF-ST-CALL-RATE-DEV        PIC XXX.              
00550          16  CF-REPLACEMENT-LAW-SW          PIC X.                
00551              88  CF-REPLACEMENT-LAW-APPLIES     VALUE 'Y'.        
00552              88  CF-REPL-LAW-NOT-APPLICABLE     VALUE 'N'.        
00553          16  CF-REPLACEMENT-LETTER          PIC X(4).             
00554          16  CF-ST-TOL-PREM-PCT             PIC S9V9999 COMP-3.   
00555          16  CF-ST-TOL-REF-PCT              PIC S9V9999 COMP-3.   
00556          16  CF-ST-TARGET-LOSS-RATIO        PIC S9V9(4) COMP-3.   
00557          16  CF-ST-SPLIT-PAYMENT            PIC X.                
00558          16  FILLER                         PIC X.                
00559          16  CF-STATE-BENEFIT-CNTL  OCCURS 50 TIMES.              
00560              20  CF-ST-BENEFIT-CD           PIC XX.               
00561              20  CF-ST-BENEFIT-KIND         PIC X.                
00562                  88  CF-ST-LIFE-KIND            VALUE 'L'.        
00563                  88  CF-ST-AH-KIND              VALUE 'A'.        
00564              20  CF-ST-REM-TERM-CALC        PIC X.                
00565                  88  ST-REM-TERM-NOT-USED       VALUE SPACE.      
00566                  88  ST-EARN-AFTER-15TH         VALUE '1'.        
00567                  88  ST-EARN-ON-HALF-MO         VALUE '2'.        
00568                  88  ST-EARN-ON-1ST-DAY         VALUE '3'.        
00569                  88  ST-EARN-ON-FULL-MO         VALUE '4'.        
00570                  88  ST-EARN-WITH-NO-DAYS       VALUE '5'.        
00571                  88  ST-EARN-AFTER-14TH         VALUE '6'.        
00572                  88  ST-EARN-AFTER-16TH         VALUE '7'.        
00573                                                                   
00574              20  CF-ST-REFUND-CALC          PIC X.                
00575                  88  ST-REFUND-NOT-USED         VALUE SPACE.      
00576                  88  ST-REFD-BY-R78             VALUE '1'.        
00577                  88  ST-REFD-BY-PRO-RATA        VALUE '2'.        
00578                  88  ST-REFD-AS-CALIF           VALUE '3'.        
00579                  88  ST-REFD-AS-TEXAS           VALUE '4'.        
00580                  88  ST-REFD-IS-NET-PAY         VALUE '5'.        
00581                  88  ST-REFD-ANTICIPATION       VALUE '6'.        
00582                  88  ST-REFD-UTAH               VALUE '7'.        
00583                  88  ST-REFD-SUM-OF-DIGITS      VALUE '9'.        
00584                  88  ST-REFD-REG-BALLOON        VALUE 'B'.        
00585                                                                   
00586              20  CF-ST-EARNING-CALC         PIC X.                
00587                  88  ST-EARNING-NOT-USED        VALUE SPACE.      
00588                  88  ST-EARN-BY-R78             VALUE '1'.        
00589                  88  ST-EARN-BY-PRO-RATA        VALUE '2'.        
00590                  88  ST-EARN-AS-CALIF           VALUE '3'.        
00591                  88  ST-EARN-AS-TEXAS           VALUE '4'.        
00592                  88  ST-EARN-IS-NET-PAY         VALUE '5'.        
00593                  88  ST-EARN-ANTICIPATION       VALUE '6'.        
00594                  88  ST-EARN-MEAN               VALUE '8'.        
00595                  88  ST-EARN-REG-BALLOON        VALUE 'B'.        
00596                                                                   
00597              20  CF-ST-OVRD-EARNINGS-CALC   PIC X.                
00598                  88  ST-OVERRIDE-NOT-USED       VALUE SPACE.      
00599                  88  ST-OVRD-BY-R78             VALUE '1'.        
00600                  88  ST-OVRD-BY-PRO-RATA        VALUE '2'.        
00601                  88  ST-OVRD-AS-CALIF           VALUE '3'.        
00602                  88  ST-OVRD-AS-TEXAS           VALUE '4'.        
00603                  88  ST-OVRD-IS-NET-PAY         VALUE '5'.        
00604                  88  ST-OVRD-ANTICIPATION       VALUE '6'.        
00605                  88  ST-OVRD-MEAN               VALUE '8'.        
00606                  88  ST-OVRD-REG-BALLOON        VALUE 'B'.        
00607              20  FILLER                     PIC X.                
00608                                                                   
00609          16  CF-ST-COMMISSION-CAPS.                               
00610              20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.   
00611              20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.   
00612              20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.   
00613              20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.   
00614          16  CF-COMM-CAP-LIMIT-TO           PIC X.                
00615                  88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.        
00616                                                                   
00617          16  CF-ST-RES-TAX-PCT              PIC S9V9(4) COMP-3.   
00618                                                                   
00619          16  CF-ST-STATUTORY-INTEREST.                            
00620              20  CF-ST-STAT-DATE-FROM       PIC X.                
00621                  88  ST-STAT-FROM-INCURRED      VALUE 'I'.        
00622                  88  ST-STAT-FROM-REPORTED      VALUE 'R'.        
00623              20  CF-ST-NO-DAYS-ELAPSED      PIC 99.               
00624              20  CF-ST-STAT-INTEREST        PIC S9V9(4) COMP-3.   
00625              20  CF-ST-STAT-INTEREST-1      PIC S9V9(4) COMP-3.   
00626              20  CF-ST-STAT-INTEREST-2      PIC S9V9(4) COMP-3.   
00627              20  CF-ST-STAT-INTEREST-3      PIC S9V9(4) COMP-3.   
00628                                                                   
00629          16  CF-ST-OVER-SHORT.                                    
00630              20 CF-ST-OVR-SHT-AMT           PIC S999V99 COMP-3.   
00631              20 CF-ST-OVR-SHT-PCT           PIC S9V9(4) COMP-3.   
00632                                                                   
00633          16  CF-ST-FREE-LOOK-PERIOD         PIC S9(3)   COMP-3.   
00634                                                                   
CIDMOD         16  CF-ST-RT-CALC                  PIC X.
PEMMOD         16  CF-ST-LF-PREM-TAX              PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-I            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-G            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-RF-LR-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LL-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LN-CALC               PIC X.
PEMMOD         16  CF-ST-RF-AH-CALC               PIC X.
PEMMOD         16  CF-ST-RF-CP-CALC               PIC X.
PEMMOD*        16  FILLER                         PIC X(206).           
CIDMOD         16  FILLER                         PIC X(192).           
00636                                                                   
00637 ****************************************************************  
00638 *             BENEFIT MASTER RECORD                            *  
00639 ****************************************************************  
00640                                                                   
00641      12  CF-BENEFIT-MASTER-REC  REDEFINES  CF-RECORD-BODY.        
00642          16  CF-BENEFIT-CONTROLS  OCCURS 8 TIMES.                 
00643              20  CF-BENEFIT-CODE            PIC XX.               
00644              20  CF-BENEFIT-NUMERIC  REDEFINES                    
00645                  CF-BENEFIT-CODE            PIC XX.               
00646              20  CF-BENEFIT-ALPHA           PIC XXX.              
00647              20  CF-BENEFIT-DESCRIP         PIC X(10).            
00648              20  CF-BENEFIT-COMMENT         PIC X(10).            
00649                                                                   
00650              20  CF-LF-COVERAGE-TYPE        PIC X.                
00651                  88  CF-REDUCING                VALUE 'R'.        
00652                  88  CF-LEVEL                   VALUE 'L' 'P'.    
00653                                                                   
00654              20  CF-SPECIAL-CALC-CD         PIC X.                
00655                  88  CF-ALTERNATE-NET-PAY       VALUE 'A'.        
00656                  88  CF-NP-0-MO-INT             VALUE 'A'.        
00657                  88  CF-OB-OFFLINE-RESERVED     VALUE 'B'.        
00658                  88  CF-CRITICAL-PERIOD         VALUE 'C'.        
00659                  88  CF-TERM-IN-DAYS            VALUE 'D'.        
00660                  88  CF-USE-PREMIUM-AS-ENTERED  VALUE 'E'.        
00661                  88  CF-FARM-PLAN               VALUE 'F'.        
00662                  88  CF-RATE-AS-STANDARD        VALUE 'G'.        
00663                  88  CF-2-MTH-INTEREST          VALUE 'I'.        
00664                  88  CF-3-MTH-INTEREST          VALUE 'J'.        
00665                  88  CF-4-MTH-INTEREST          VALUE 'K'.        
00666                  88  CF-BALLOON-LAST-PMT        VALUE 'L'.        
00667                  88  CF-MORTGAGE-PROCESSING     VALUE 'M'.        
00668                  88  CF-PRUDENTIAL              VALUE 'P'.        
00669                  88  CF-OUTSTANDING-BAL         VALUE 'O'.        
00670                  88  CF-TRUNCATED-LIFE          VALUE 'T'.        
00671                  88  CF-TRUNCATED-LIFE-ONE      VALUE 'U'.        
00672                  88  CF-TRUNCATED-LIFE-TWO      VALUE 'V'.        
00673                  88  CF-NET-PAY-SIMPLE          VALUE 'S'.        
00674                  88  CF-SUMMARY-PROCESSING      VALUE 'Z'.        
00675                                                                   
00676              20  CF-JOINT-INDICATOR         PIC X.                
00677                  88  CF-JOINT-COVERAGE          VALUE 'J'.        
00678                                                                   
00679              20  FILLER                     PIC X(12).            
00680              20  CF-LOAN-TYPE               PIC X(8).             
00681                                                                   
00682              20  CF-CO-REM-TERM-CALC        PIC X.                
00683                  88  CO-EARN-AFTER-15TH         VALUE '1'.        
00684                  88  CO-EARN-ON-HALF-MO         VALUE '2'.        
00685                  88  CO-EARN-ON-1ST-DAY         VALUE '3'.        
00686                  88  CO-EARN-ON-FULL-MO         VALUE '4'.        
00687                  88  CO-EARN-WITH-NO-DAYS       VALUE '5'.        
00688                                                                   
00689              20  CF-CO-EARNINGS-CALC        PIC X.                
00690                  88  CO-EARN-BY-R78             VALUE '1'.        
00691                  88  CO-EARN-BY-PRO-RATA        VALUE '2'.        
00692                  88  CO-EARN-AS-CALIF           VALUE '3'.        
00693                  88  CO-EARN-AS-TEXAS           VALUE '4'.        
00694                  88  CO-EARN-IS-NET-PAY         VALUE '5'.        
00695                  88  CO-EARN-ANTICIPATION       VALUE '6'.        
00696                  88  CO-EARN-AS-MEAN            VALUE '8'.        
00697                  88  CO-EARN-AS-REG-BALLOON     VALUE 'B'.        
00698                                                                   
00699              20  CF-CO-REFUND-CALC          PIC X.                
00700                  88  CO-REFUND-NOT-USED         VALUE SPACE.      
00701                  88  CO-REFD-BY-R78             VALUE '1'.        
00702                  88  CO-REFD-BY-PRO-RATA        VALUE '2'.        
00703                  88  CO-REFD-AS-CALIF           VALUE '3'.        
00704                  88  CO-REFD-AS-TEXAS           VALUE '4'.        
00705                  88  CO-REFD-IS-NET-PAY         VALUE '5'.        
00706                  88  CO-REFD-ANTICIPATION       VALUE '6'.        
00707                  88  CO-REFD-MEAN               VALUE '8'.        
00708                  88  CO-REFD-SUM-OF-DIGITS      VALUE '9'.        
00709                  88  CO-REFD-AS-REG-BALLOON     VALUE 'B'.        
00710                                                                   
00711              20  CF-CO-OVRD-EARNINGS-CALC   PIC X.                
00712                  88  CO-OVERRIDE-NOT-USED       VALUE SPACE.      
00713                  88  CO-OVRD-BY-R78             VALUE '1'.        
00714                  88  CO-OVRD-BY-PRO-RATA        VALUE '2'.        
00715                  88  CO-OVRD-AS-CALIF           VALUE '3'.        
00716                  88  CO-OVRD-AS-TEXAS           VALUE '4'.        
00717                  88  CO-OVRD-IS-NET-PAY         VALUE '5'.        
00718                  88  CO-OVRD-ANTICIPATION       VALUE '6'.        
00719                  88  CO-OVRD-MEAN               VALUE '8'.        
00720                  88  CO-OVRD-AS-REG-BALLOON     VALUE 'B'.        
00721                                                                   
00722              20  CF-CO-BEN-I-G-CD           PIC X.                
00723                  88  CO-BEN-I-G-NOT-USED        VALUE SPACE.      
00724                  88  CO-BEN-I-G-IS-INDV         VALUE 'I'.        
00725                  88  CO-BEN-I-G-IS-GRP          VALUE 'G'.        
00726                                                                   
00727          16  FILLER                         PIC X(304).           
00728                                                                   
00729                                                                   
00730 ****************************************************************  
00731 *             CARRIER MASTER RECORD                            *  
00732 ****************************************************************  
00733                                                                   
00734      12  CF-CARRIER-MASTER-REC  REDEFINES  CF-RECORD-BODY.        
00735          16  CF-ADDRESS-DATA.                                     
00736              20  CF-MAIL-TO-NAME            PIC X(30).            
00737              20  CF-IN-CARE-OF              PIC X(30).            
00738              20  CF-ADDRESS-LINE-1          PIC X(30).            
00739              20  CF-ADDRESS-LINE-2          PIC X(30).            
00740              20  CF-CITY-STATE              PIC X(30).            
00741              20  CF-ZIP-CODE-NUM            PIC 9(9)      COMP-3. 
00742              20  CF-PHONE-NO                PIC 9(11)     COMP-3. 
00743                                                                   
00744          16  CF-CLAIM-NO-CONTROL.                                 
00745              20  CF-CLAIM-NO-METHOD         PIC X.                
00746                  88  CLAIM-NO-MANUAL            VALUE '1'.        
00747                  88  CLAIM-NO-Y-M-SEQ           VALUE '2'.        
00748                  88  CLAIM-NO-SEQ               VALUE '3'.        
00749                  88  CLAIM-NO-ALPHA-SEQ         VALUE '5'.        
00750              20  CF-CLAIM-COUNTER           PIC S9(8)   COMP.     
00751                  88  CLAIM-CNT-RESET-IF-SEQ     VALUE +9999999.   
00752                  88  CLAIM-CNT-RESET-IF-YRMO    VALUE +99999.     
00753                  88  CLAIM-CNT-RESET-IF-YRALPHA VALUE +9999.      
00754                                                                   
00755          16  CF-CHECK-NO-CONTROL.                                 
00756              20  CF-CHECK-NO-METHOD         PIC X.                
00757                  88  CHECK-NO-MANUAL            VALUE '1'.        
00758                  88  CHECK-NO-AUTO-SEQ          VALUE '2'.        
00759                  88  CHECK-NO-CARR-SEQ          VALUE '3'.        
00760                  88  CHECK-NO-AT-PRINT          VALUE '4'.        
00761              20  CF-CHECK-COUNTER           PIC S9(8)   COMP.     
00762                  88  CHECK-CNT-RESET-VALUE      VALUE +999999.    
00763                                                                   
00764          16  CF-DOMICILE-STATE              PIC XX.               
00765                                                                   
00766          16  CF-EXPENSE-CONTROLS.                                 
00767              20  CF-EXPENSE-METHOD          PIC X.                
00768                  88  EXPENSE-CALC-MANUAL        VALUE '1'.        
00769                  88  DOLLARS-PER-PMT            VALUE '2'.        
00770                  88  PERCENT-OF-PAYMENT         VALUE '3'.        
00771                  88  DOLLARS-PER-MONTH          VALUE '4'.        
00772              20  CF-EXPENSE-PERCENT         PIC S999V99   COMP-3. 
00773              20  CF-EXPENSE-DOLLAR          PIC S999V99   COMP-3. 
00774                                                                   
00775          16  CF-CORRESPONDENCE-CONTROL.                           
00776              20  CF-LETTER-RESEND-OPT       PIC X.                
00777                  88  LETTERS-NOT-ARCHIVED       VALUE SPACE.      
00778                  88  LETTERS-ARE-ARCHIVED       VALUE '1'.        
00779              20  FILLER                     PIC X(4).             
00780                                                                   
00781          16  CF-RESERVE-CONTROLS.                                 
00782              20  CF-MANUAL-SW               PIC X.                
00783                  88  CF-MANUAL-RESERVES-USED    VALUE '1'.        
00784              20  CF-FUTURE-SW               PIC X.                
00785                  88  CF-FUTURE-RESERVES-USED    VALUE '1'.        
00786              20  CF-PTC-SW                  PIC X.                
00787                  88  CF-PAY-TO-CURRENT-USED     VALUE '1'.        
00788              20  CF-IBNR-SW                 PIC X.                
00789                  88  CF-IBNR-RESERVES-USED      VALUE '1'.        
00790              20  CF-PTC-LF-SW               PIC X.                
00791                  88  CF-LF-PTC-USED             VALUE '1'.        
00792              20  CF-CDT-ACCESS-METHOD       PIC X.                
00793                  88  CF-CDT-ROUND-NEAR          VALUE '1'.        
00794                  88  CF-CDT-ROUND-HIGH          VALUE '2'.        
00795                  88  CF-CDT-INTERPOLATED        VALUE '3'.        
00796              20  CF-PERCENT-OF-CDT          PIC S999V99   COMP-3. 
00797                                                                   
00798          16  CF-CLAIM-CALC-METHOD           PIC X.                
00799              88  360-PLUS-MONTHS                VALUE '1'.        
00800              88  365-PLUS-MONTHS                VALUE '2'.        
00801              88  FULL-MONTHS-ACTUAL-DAY         VALUE '3'.        
00802              88  360-DAILY                      VALUE '4'.        
00803              88  365-DAILY                      VALUE '5'.        
00804                                                                   
00805          16  CF-LAST-ALPHA-CHARACTER        PIC X.                
00806          16  FILLER                         PIC X(11).            
00807                                                                   
00808          16  CF-LIMIT-AMOUNTS.                                    
00809              20  CF-CALC-AMT-TOL            PIC S999V99   COMP-3. 
00810              20  CF-MAX-REG-PMT             PIC S9(7)V99  COMP-3. 
00811              20  CF-MAX-REG-DAYS            PIC S999      COMP-3. 
00812              20  CF-MAX-AUTO-PMT            PIC S9(7)V99  COMP-3. 
00813              20  CF-MAX-AUTO-MOS            PIC S999      COMP-3. 
00814              20  CF-CALC-DAYS-TOL           PIC S999      COMP-3. 
00815              20  CF-CR-TOL-PREM             PIC S999V99   COMP-3. 
00816              20  CF-CR-TOL-REFUND           PIC S999V99   COMP-3. 
00817              20  CF-CR-TOL-PREM-PCT         PIC S9V9(4)   COMP-3. 
00818              20  CF-CR-TOL-REFUND-PCT       PIC S9V9(4)   COMP-3. 
00819                                                                   
00820          16  CF-DAYS-BEFORE-CLOSED          PIC S999      COMP-3. 
00821          16  CF-MONTHS-BEFORE-PURGED        PIC S999      COMP-3. 
00822          16  CF-IBNR-PERCENT                PIC S9V9(4)   COMP-3. 
00823                                                                   
00824          16  CF-ZIP-CODE.                                         
00825              20  CF-ZIP-PRIME.                                    
00826                  24  CF-ZIP-1ST             PIC X.                
00827                      88  CF-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00828                  24  FILLER                 PIC X(4).             
00829              20  CF-ZIP-PLUS4               PIC X(4).             
00830          16  CF-CANADIAN-POSTAL-CODE REDEFINES CF-ZIP-CODE.       
00831              20  CF-CAN-POSTAL-1            PIC XXX.              
00832              20  CF-CAN-POSTAL-2            PIC XXX.              
00833              20  FILLER                     PIC XXX.              
00834                                                                   
00835          16  CF-IBNR-UEPRM-PERCENT          PIC S9V9(4) COMP-3.   
00836          16  CF-IBNR-R78-PERCENT            PIC S9V9(4) COMP-3.   
00837          16  CF-IBNR-PRO-PERCENT            PIC S9V9(4) COMP-3.   
00838                                                                   
00839          16  CF-RATING-SWITCH               PIC X.                
00840              88  CF-PERFORM-RATING              VALUE ' ' 'Y'.    
00841              88  CF-NO-RATING                   VALUE 'N'.        
00842                                                                   
00843          16  CF-BUILD-RETRIEVE-AFTER-MONTHS PIC 99.               
00844                                                                   
00845          16  CF-CARRIER-OVER-SHORT.                               
00846              20 CF-CR-OVR-SHT-AMT           PIC S999V99   COMP-3. 
00847              20 CF-CR-OVR-SHT-PCT           PIC S9V9(4)   COMP-3. 
00848                                                                   
00849          16  FILLER                         PIC X(456).           
00850                                                                   
00851                                                                   
00852 ****************************************************************  
00853 *             MORTALITY MASTER RECORD                          *  
00854 ****************************************************************  
00855                                                                   
00856      12  CF-MORTALITY-MASTER-REC REDEFINES  CF-RECORD-BODY.       
00857          16  CF-MORT-TABLE-LINE OCCURS  9  TIMES                  
00858                                 INDEXED BY CF-MORT-NDX.           
00859              20  CF-MORT-TABLE              PIC X(5).             
00860              20  CF-MORT-TABLE-TYPE         PIC X.                
00861                  88  CF-MORT-JOINT              VALUE 'J'.        
00862                  88  CF-MORT-SINGLE             VALUE 'S'.        
00863                  88  CF-MORT-COMBINED           VALUE 'C'.        
00864                  88  CF-MORT-TYPE-VALID-C       VALUE 'J' 'S'.    
00865                  88  CF-MORT-TYPE-VALID-M       VALUE 'J' 'S' 'C'.
00866              20  CF-MORT-INTEREST           PIC SV9(4)  COMP-3.   
00867              20  CF-MORT-AGE-METHOD         PIC XX.               
00868                  88  CF-AGE-LAST                VALUE 'AL'.       
00869                  88  CF-AGE-NEAR                VALUE 'AN'.       
00870              20  CF-MORT-RESERVE-ADJUSTMENT PIC S9V9(4) COMP-3.   
00871              20  CF-MORT-ADJUSTMENT-DIRECTION                     
00872                                             PIC X.                
00873                  88  CF-MINUS                   VALUE '-'.        
00874                  88  CF-PLUS                    VALUE '+'.        
00875              20  CF-MORT-JOINT-FACTOR       PIC S9V9(4) COMP-3.   
00876              20  CF-MORT-JOINT-CODE         PIC X.                
00877                  88  CF-VALID-JOINT-CODE        VALUE 'A' 'V'.    
00878              20  CF-MORT-PC-Q               PIC X.                
00879                  88  CF-VALID-PC-Q              VALUE 'Y' 'N' ' '.
00880              20  CF-MORT-TABLE-CODE         PIC X(4).             
00881              20  CF-MORT-COMMENTS           PIC X(15).            
00882              20  FILLER                     PIC X(14).            
00883                                                                   
00884          16  FILLER                         PIC X(251).           
00885                                                                   
00886                                                                   
00887 ****************************************************************  
00888 *             BUSSINESS TYPE MASTER RECORD                     *  
00889 ****************************************************************  
00890                                                                   
00891      12  CF-BUSINESS-TYPE-MASTER-REC REDEFINES  CF-RECORD-BODY.   
00892 * FIRST ENTRY IS TYPE 01.. LAST IS TYPE 20                        
00893 * RECORD 02 IS TYPES 21-40..RECORD 03 IS 41-60..04 IS 61-80       
00894 * AND RECORD 05 IS TYPES 81-99                                    
00895          16  CF-TYPE-DESCRIPTIONS   OCCURS  20  TIMES.            
00896              20  CF-BUSINESS-TITLE          PIC  X(19).           
00897              20  CF-BUS-MOD-ST-TRGT-LOSS-RATIO                    
00898                                             PIC S9V9(4) COMP-3.   
00899              20  CF-BUS-EXCL-ST-CALL        PIC  X.               
00900              20  FILLER                     PIC  X.               
00901          16  FILLER                         PIC  X(248).          
00902                                                                   
00903                                                                   
00904 ****************************************************************  
00905 *             TERMINAL MASTER RECORD                           *  
00906 ****************************************************************  
00907                                                                   
00908      12  CF-TERMINAL-MASTER-REC  REDEFINES  CF-RECORD-BODY.       
00909                                                                   
00910          16  CF-COMPANY-TERMINALS.                                
00911              20  CF-TERMINAL-ID  OCCURS 120 TIMES                 
00912                                   PIC X(4).                       
00913          16  FILLER               PIC X(248).                     
00914                                                                   
00915                                                                   
00916 ****************************************************************  
00917 *             LIFE EDIT MASTER RECORD                          *  
00918 ****************************************************************  
00919                                                                   
00920      12  CF-LIFE-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.       
00921          16  CF-LIFE-EDIT-ENTRIES   OCCURS 120  TIMES.            
00922              20  CF-LIFE-CODE-IN            PIC XX.               
00923              20  CF-LIFE-CODE-OUT           PIC XX.               
00924          16  FILLER                         PIC X(248).           
00925                                                                   
00926                                                                   
00927 ****************************************************************  
00928 *             AH EDIT MASTER RECORD                            *  
00929 ****************************************************************  
00930                                                                   
00931      12  CF-AH-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.         
00932          16  CF-AH-EDIT-ENTRIES   OCCURS  96  TIMES.              
00933              20  CF-AH-CODE-IN              PIC XXX.              
00934              20  CF-AH-CODE-OUT             PIC XX.               
00935          16  FILLER                         PIC X(248).           
00936                                                                   
00937                                                                   
00938 ****************************************************************  
00939 *             CREDIBILITY TABLES                               *  
00940 ****************************************************************  
00941                                                                   
00942      12  CF-CREDIBILITY-MASTER-REC REDEFINES  CF-RECORD-BODY.     
00943          16  CF-CRDB-ENTRY   OCCURS 36 TIMES                      
00944                              INDEXED BY CF-CRDB-NDX.              
00945              20  CF-CRDB-FROM               PIC S9(7)   COMP-3.   
00946              20  CF-CRDB-TO                 PIC S9(7)   COMP-3.   
00947              20  CF-CREDIBILITY-FACTOR      PIC S9V9(4) COMP-3.   
00948          16  FILLER                         PIC  X(332).          
00949                                                                   
00950                                                                   
00951 ****************************************************************  
00952 *             REPORT CUSTOMIZATION RECORD                      *  
00953 ****************************************************************  
00954                                                                   
00955      12  CF-CUSTOM-REPORT-REC  REDEFINES  CF-RECORD-BODY.         
00956          16  CF-ACCOUNT-MASTER-STATUS       PIC X.                
00957              88  CF-ACTIVE-ACCOUNTS             VALUE 'A'.        
00958              88  CF-INACTIVE-ACCOUNTS           VALUE 'I'.        
00959 **** NOTE: INACTIVE WILL INCLUDE ACCOUNT MASTER CODED WITH ****   
00960 ****       A T-TRANSFER.                                   ****   
00961              88  CF-ALL-ACCOUNTS                VALUE 'B'.        
00962                                                                   
00963          16  FILLER                         PIC XX.               
00964                                                                   
00965          16  CF-CARRIER-CNTL-OPT.                                 
00966              20  CF-CARRIER-OPT-SEQ         PIC 9.                
00967                  88  CF-CARRIER-OPT-USED        VALUE 1 THRU 6.   
00968                  88  CF-CARRIER-OPT-NOT-USED    VALUE 0.          
00969              20  CF-CARRIER-SELECT OCCURS 3 TIMES                 
00970                                             PIC X.                
00971          16  CF-GROUP-CNTL-OPT.                                   
00972              20  CF-GROUP-OPT-SEQ           PIC 9.                
00973                  88  CF-GROUP-OPT-USED          VALUE 1 THRU 6.   
00974                  88  CF-GROUP-OPT-NOT-USED      VALUE 0.          
00975              20  CF-GROUP-SELECT OCCURS 3 TIMES                   
00976                                             PIC X(6).             
00977          16  CF-STATE-CNTL-OPT.                                   
00978              20  CF-STATE-OPT-SEQ           PIC 9.                
00979                  88  CF-STATE-OPT-USED          VALUE 1 THRU 6.   
00980                  88  CF-STATE-OPT-NOT-USED      VALUE 0.          
00981              20  CF-STATE-SELECT OCCURS 3 TIMES                   
00982                                             PIC XX.               
00983          16  CF-ACCOUNT-CNTL-OPT.                                 
00984              20  CF-ACCOUNT-OPT-SEQ         PIC 9.                
00985                  88  CF-ACCOUNT-OPT-USED        VALUE 1 THRU 6.   
00986                  88  CF-ACCOUNT-OPT-NOT-USED    VALUE 0.          
00987              20  CF-ACCOUNT-SELECT OCCURS 3 TIMES                 
00988                                             PIC X(10).            
00989          16  CF-BUS-TYP-CNTL-OPT.                                 
00990              20  CF-BUS-TYP-OPT-SEQ         PIC 9.                
00991                  88  CF-BUS-TYP-OPT-USED        VALUE 1 THRU 6.   
00992                  88  CF-BUS-TYP-OPT-NOT-USED    VALUE 0.          
00993              20  CF-BUS-TYP-SELECT OCCURS 3 TIMES                 
00994                                             PIC XX.               
00995          16  CF-LF-TYP-CNTL-OPT.                                  
00996              20  CF-LF-TYP-OPT-SEQ          PIC 9.                
00997                  88  CF-LF-TYP-OPT-USED         VALUE 1 THRU 6.   
00998                  88  CF-LF-TYP-OPT-NOT-USED     VALUE 0.          
00999              20  CF-BUS-LF-SELECT OCCURS 3 TIMES                  
01000                                             PIC XX.               
01001          16  CF-AH-TYP-CNTL-OPT.                                  
01002              20  CF-AH-TYP-OPT-SEQ          PIC 9.                
01003                  88  CF-AH-TYP-OPT-USED         VALUE 1 THRU 6.   
01004                  88  CF-AH-TYP-OPT-NOT-USED     VALUE 0.          
01005              20  CF-BUS-AH-SELECT OCCURS 3 TIMES                  
01006                                             PIC XX.               
01007          16  CF-REPTCD1-CNTL-OPT.                                 
01008              20  CF-REPTCD1-OPT-SEQ         PIC 9.                
01009                  88  CF-REPTCD1-OPT-USED        VALUE 1 THRU 6.   
01010                  88  CF-REPTCD1-OPT-NOT-USED    VALUE 0.          
01011              20  CF-REPTCD1-SELECT OCCURS 3 TIMES                 
01012                                             PIC X(10).            
01013          16  CF-REPTCD2-CNTL-OPT.                                 
01014              20  CF-REPTCD2-OPT-SEQ         PIC 9.                
01015                  88  CF-REPTCD2-OPT-USED        VALUE 1 THRU 6.   
01016                  88  CF-REPTCD2-OPT-NOT-USED    VALUE 0.          
01017              20  CF-REPTCD2-SELECT OCCURS 3 TIMES                 
01018                                             PIC X(10).            
01019          16  CF-USER1-CNTL-OPT.                                   
01020              20  CF-USER1-OPT-SEQ           PIC 9.                
01021                  88  CF-USER1-OPT-USED          VALUE 1 THRU 6.   
01022                  88  CF-USER1-OPT-NOT-USED      VALUE 0.          
01023              20  CF-USER1-SELECT OCCURS 3 TIMES                   
01024                                             PIC X(10).            
01025          16  CF-USER2-CNTL-OPT.                                   
01026              20  CF-USER2-OPT-SEQ           PIC 9.                
01027                  88  CF-USER2-OPT-USED          VALUE 1 THRU 6.   
01028                  88  CF-USER2-OPT-NOT-USED      VALUE 0.          
01029              20  CF-USER2-SELECT OCCURS 3 TIMES                   
01030                                             PIC X(10).            
01031          16  CF-USER3-CNTL-OPT.                                   
01032              20  CF-USER3-OPT-SEQ           PIC 9.                
01033                  88  CF-USER3-OPT-USED          VALUE 1 THRU 6.   
01034                  88  CF-USER3-OPT-NOT-USED      VALUE 0.          
01035              20  CF-USER3-SELECT OCCURS 3 TIMES                   
01036                                             PIC X(10).            
01037          16  CF-USER4-CNTL-OPT.                                   
01038              20  CF-USER4-OPT-SEQ           PIC 9.                
01039                  88  CF-USER4-OPT-USED          VALUE 1 THRU 6.   
01040                  88  CF-USER4-OPT-NOT-USED      VALUE 0.          
01041              20  CF-USER4-SELECT OCCURS 3 TIMES                   
01042                                             PIC X(10).            
01043          16  CF-USER5-CNTL-OPT.                                   
01044              20  CF-USER5-OPT-SEQ           PIC 9.                
01045                  88  CF-USER5-OPT-USED          VALUE 1 THRU 6.   
01046                  88  CF-USER5-OPT-NOT-USED      VALUE 0.          
01047              20  CF-USER5-SELECT OCCURS 3 TIMES                   
01048                                             PIC X(10).            
01049          16  CF-REINS-CNTL-OPT.                                   
01050              20  CF-REINS-OPT-SEQ           PIC 9.                
01051                  88  CF-REINS-OPT-USED          VALUE 1 THRU 6.   
01052                  88  CF-REINS-OPT-NOT-USED      VALUE 0.          
01053              20  CF-REINS-SELECT OCCURS 3 TIMES.                  
01054                  24  CF-REINS-PRIME         PIC XXX.              
01055                  24  CF-REINS-SUB           PIC XXX.              
01056                                                                   
01057          16  CF-AGENT-CNTL-OPT.                                   
01058              20  CF-AGENT-OPT-SEQ           PIC 9.                
01059                  88  CF-AGENT-OPT-USED          VALUE 1 THRU 6.   
01060                  88  CF-AGENT-OPT-NOT-USED      VALUE 0.          
01061              20  CF-AGENT-SELECT OCCURS 3 TIMES                   
01062                                             PIC X(10).            
01063                                                                   
01064          16  FILLER                         PIC X(43).            
01065                                                                   
01066          16  CF-LOSS-RATIO-SELECT.                                
01067              20  CF-SEL-LO-LOSS-RATIO       PIC S999V99  COMP-3.  
01068              20  CF-SEL-HI-LOSS-RATIO       PIC S999V99  COMP-3.  
01069          16  CF-ENTRY-DATE-SELECT.                                
01070              20  CF-SEL-LO-ENTRY-DATE       PIC XX.               
01071              20  CF-SEL-HI-ENTRY-DATE       PIC XX.               
01072          16  CF-EFFECTIVE-DATE-SELECT.                            
01073              20  CF-SEL-LO-EFFECTIVE-DATE   PIC XX.               
01074              20  CF-SEL-HI-EFFECTIVE-DATE   PIC XX.               
01075                                                                   
01076          16  CF-EXCEPTION-LIST-IND          PIC X.                
01077              88  CF-EXCEPTION-LIST-REQUESTED VALUE 'Y'.           
01078                                                                   
01079          16  FILLER                         PIC X(318).           
01080                                                                   
01081 ****************************************************************  
01082 *                  EXCEPTION REPORTING RECORD                  *  
01083 ****************************************************************  
01084                                                                   
01085      12  CF-EXCEPTION-REPORT-REC REDEFINES   CF-RECORD-BODY.      
01086          16  CF-ACCOUNTS-LT-ONE-YEAR        PIC X.                
01087              88  CF-EXCEPTION-ACCTS-WITHIN-ONE  VALUE 'Y'.        
01088                                                                   
01089          16  CF-COMBINED-LIFE-AH-OPT.                             
01090              20  CF-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.
01091              20  CF-SINGLE-MO-PREM-PCT      PIC S9(02).           
01092              20  CF-EARN-PREM-DECR-PCT      PIC S9(02).           
01093              20  CF-CANCELLATION-RATIO      PIC S9(02).           
01094                                                                   
01095          16  CF-LIFE-OPT.                                         
01096              20  CF-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01097              20  CF-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01098              20  CF-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01099              20  CF-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01100              20  CF-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01101              20  CF-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01102              20  CF-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01103              20  CF-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01104              20  CF-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01105              20  CF-LF-AVG-AGE-MAX          PIC S9(02).           
01106                                                                   
01107          16  CF-AH-OPT.                                           
01108              20  CF-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01109              20  CF-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01110              20  CF-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01111              20  CF-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01112              20  CF-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01113              20  CF-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01114              20  CF-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01115              20  CF-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01116              20  CF-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01117              20  CF-AH-AVG-AGE-MAX          PIC S9(02).           
01118                                                                   
01119          16  CF-ACCT-ZERO-MONTH-PRODUCTION PIC X.                 
01120              88  CF-ACCT-CURRENT-MONTH-ACT      VALUE 'A'.        
01121              88  CF-ACCT-WITH-NO-PRODUCTION     VALUE 'B'.        
01122              88  CF-ACCT-WITH-ISSUE-ACTIVITY    VALUE 'C'.        
01123                                                                   
01124          16  CF-RETENTION-LIMIT             PIC S9(7)      COMP-3.
01125                                                                   
01126          16  FILLER                         PIC X(673).           
01127                                                                   
01128                                                                   
01129 ****************************************************************  
01130 *             MORTGAGE SYSTEM PLAN RECORD                      *  
01131 ****************************************************************  
01132                                                                   
01133      12  CF-MORTGAGE-PLAN-MASTER  REDEFINES  CF-RECORD-BODY.      
01134          16  CF-PLAN-TYPE                   PIC X.                
01135              88  CF-LIFE-MORT-PLAN             VALUE 'L'.         
01136              88  CF-DISAB-MORT-PLAN            VALUE 'D'.         
01137              88  CF-AD-D-MORT-PLAN             VALUE 'A'.         
01138          16  CF-PLAN-ABBREV                 PIC XXX.              
01139          16  CF-PLAN-DESCRIPT               PIC X(10).            
01140          16  CF-PLAN-NOTES                  PIC X(20).            
01141          16  CF-PLAN-ESTABLISH-DATE         PIC XX.               
01142          16  CF-PLAN-UNDERWRITING.                                
01143              20  CF-PLAN-TERM-DATA.                               
01144                  24  CF-MINIMUM-TERM        PIC S999      COMP-3. 
01145                  24  CF-MAXIMUM-TERM        PIC S999      COMP-3. 
01146              20  CF-PLAN-AGE-DATA.                                
01147                  24  CF-MINIMUM-AGE         PIC S999      COMP-3. 
01148                  24  CF-MAXIMUM-AGE         PIC S999      COMP-3. 
01149                  24  CF-MAXIMUM-ATTAIN-AGE  PIC S999      COMP-3. 
01150              20  CF-PLAN-BENEFIT-DATA.                            
01151                  24  CF-MINIMUM-BENEFIT     PIC S9(7)V99  COMP-3. 
01152                  24  CF-MAXIMUM-BENEFIT     PIC S9(7)V99  COMP-3. 
01153                  24  CF-MAXIMUM-MONTHLY-BENEFIT                   
01154                                             PIC S9(7)V99  COMP-3. 
01155          16  CF-PLAN-POLICY-FORMS.                                
01156              20  CF-POLICY-FORM             PIC X(12).            
01157              20  CF-MASTER-APPLICATION      PIC X(12).            
01158              20  CF-MASTER-POLICY           PIC X(12).            
01159          16  CF-PLAN-RATING.                                      
01160              20  CF-RATE-CODE               PIC X(5).             
01161              20  CF-SEX-RATING              PIC X.                
01162                  88  CF-PLAN-NOT-SEX-RATED     VALUE '1'.         
01163                  88  CF-PLAN-SEX-RATED         VALUE '2'.         
01164              20  CF-SUB-STD-PCT             PIC S9V9999   COMP-3. 
01165              20  CF-SUB-STD-TYPE            PIC X.                
01166                  88  CF-PCT-OF-PREM            VALUE '1'.         
01167                  88  CF-PCT-OF-BENE            VALUE '2'.         
01168          16  CF-PLAN-PREM-TOLERANCES.                             
01169              20  CF-PREM-TOLERANCE          PIC S999      COMP-3. 
01170              20  CF-PREM-TOLERANCE-PCT      PIC SV999     COMP-3. 
01171          16  CF-PLAN-PYMT-TOLERANCES.                             
01172              20  CF-PYMT-TOLERANCE          PIC S999      COMP-3. 
01173              20  CF-PYMT-TOLERANCE-PCT      PIC SV999     COMP-3. 
01174          16  CF-PLAN-MISC-DATA.                                   
01175              20  FILLER                     PIC X.                
01176              20  CF-FREE-EXAM-DAYS          PIC S999      COMP-3. 
01177              20  CF-RETRO-RETENTION         PIC S9V9999   COMP-3. 
01178          16  CF-MORT-PLAN-USE-CTR           PIC S999      COMP-3. 
01179          16  CF-PLAN-IND-GRP                PIC X.                
01180              88  CF-MORT-INDIV-PLAN            VALUE 'I'          
01181                                                      '1'.         
01182              88  CF-MORT-GROUP-PLAN            VALUE 'G'          
01183                                                      '2'.         
01184          16  CF-MIB-SEARCH-SW               PIC X.                
01185              88  CF-MIB-SEARCH-ALL             VALUE '1'.         
01186              88  CF-MIB-SEARCH-NONE            VALUE '2'.         
01187              88  CF-MIB-SEARCH-EXCEEDED        VALUE '3'.         
01188              88  CF-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'. 
01189          16  CF-ALPHA-SEARCH-SW             PIC X.                
01190              88  CF-MIB-ALPHA-ALL              VALUE '1'.         
01191              88  CF-MIB-ALPHA-NONE             VALUE '2'.         
01192              88  CF-MIB-APLHA-EXCEEDED         VALUE '3'.         
01193              88  CF-CLIENT-ALPHA-ALL           VALUE 'A'.         
01194              88  CF-CLIENT-ALPHA-NONE          VALUE 'B'.         
01195              88  CF-CLIENT-APLHA-EXCEEDED      VALUE 'C'.         
01196              88  CF-BOTH-ALPHA-ALL             VALUE 'X'.         
01197              88  CF-BOTH-ALPHA-NONE            VALUE 'Y'.         
01198              88  CF-BOTH-APLHA-EXCEEDED        VALUE 'Z'.         
01199              88  CF-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'  
01200                                                      'A' 'B' 'C'  
01201                                                      'X' 'Y' 'Z'. 
01202          16  CF-EFF-DT-RULE-SW              PIC X.                
01203              88  CF-EFF-DT-ENTER               VALUE 'E'.         
01204              88  CF-EFF-DT-MONTH               VALUE 'M'.         
01205              88  CF-EFF-DT-QTR                 VALUE 'Q'.         
01206              88  CF-EFF-DT-SEMI                VALUE 'S'.         
01207              88  CF-EFF-DT-ANN                 VALUE 'A'.         
01208          16  FILLER                         PIC X(4).             
01209          16  CF-HEALTH-QUESTIONS            PIC X.                
01210              88  CF-VALID-QUESTIONS-CNT VALUES ARE '0' THRU '9'.  
01211          16  CF-GRACE-PERIOD                PIC S999      COMP-3. 
01212          16  CF-NUMBER-LAPSE-NOTICES        PIC S999      COMP-3. 
01213          16  CF-PLAN-SNGL-JNT               PIC X.                
01214              88  CF-COMBINED-PLAN              VALUE 'C'.         
01215              88  CF-JNT-PLAN                   VALUE 'J'.         
01216              88  CF-SNGL-PLAN                  VALUE 'S'.         
01217          16  CF-DAYS-TO-1ST-NOTICE          PIC  99.              
01218          16  CF-DAYS-TO-2ND-NOTICE          PIC  99.              
01219          16  CF-DAYS-TO-3RD-NOTICE          PIC  99.              
01220          16  CF-DAYS-TO-4TH-NOTICE          PIC  99.              
01221          16  CF-RERATE-CNTL                 PIC  X.               
01222              88  CF-RERATE-WITH-ISSUE-AGE       VALUE '1'.        
01223              88  CF-RERATE-WITH-CURRENT-AGE     VALUE '2'.        
01224              88  CF-DO-NOT-RERATE               VALUE '3' ' '.    
01225              88  CF-AUTO-RECALC                 VALUE '4'.        
01226          16  CF-BENEFIT-TYPE                PIC  X.               
01227              88  CF-BENEFIT-IS-LEVEL            VALUE '1'.        
01228              88  CF-BENEFIT-REDUCES             VALUE '2'.        
01229          16  CF-POLICY-FEE                  PIC S999V99           
01230                                                     COMP-3.       
01231          16  CF-1ST-NOTICE-FORM             PIC  X(04).           
01232          16  CF-2ND-NOTICE-FORM             PIC  X(04).           
01233          16  CF-3RD-NOTICE-FORM             PIC  X(04).           
01234          16  CF-4TH-NOTICE-FORM             PIC  X(04).           
01235          16  FILLER                         PIC  X(32).           
01236          16  CF-TERMINATION-FORM            PIC  X(04).           
01237          16  FILLER                         PIC  X(08).           
01238          16  CF-CLAIM-CAP                   PIC S9(7)V99          
01239                                                        COMP-3.    
01240          16  CF-REOCCURRING-DISABILITY-PRD  PIC S999   COMP-3.    
01241          16  CF-ISSUE-LETTER                PIC  X(4).            
01242          16  CF-YEARS-TO-NEXT-RERATE        PIC  99.              
01243          16  CF-DEPENDENT-COVERAGE          PIC  X.               
01244              88  CF-YES-DEP-COV                 VALUE 'Y'.        
01245              88  CF-NO-DEP-COV             VALUES ARE 'N' ' '.    
01246          16  CF-MP-REFUND-CALC              PIC X.                
01247              88  CF-MP-REFUND-NOT-USED          VALUE SPACE.      
01248              88  CF-MP-REFD-BY-R78              VALUE '1'.        
01249              88  CF-MP-REFD-BY-PRO-RATA         VALUE '2'.        
01250              88  CF-MP-REFD-AS-CALIF            VALUE '3'.        
01251              88  CF-MP-REFD-AS-TEXAS            VALUE '4'.        
01252              88  CF-MP-REFD-IS-NET-PAY          VALUE '5'.        
01253              88  CF-MP-REFD-ANTICIPATION        VALUE '6'.        
01254              88  CF-MP-REFD-MEAN                VALUE '8'.        
01255          16  CF-ALT-RATE-CODE               PIC  X(5).            
01256                                                                   
01257                                                                   
01258          16  FILLER                         PIC X(498).           
01259 ****************************************************************  
01260 *             MORTGAGE COMPANY MASTER RECORD                   *  
01261 ****************************************************************  
01262                                                                   
01263      12  CF-MORTG-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.  
01264          16  CF-MORTG-ALT-MORT-CODE         PIC X(4).             
01265          16  CF-MORTG-ACCESS-CONTROL        PIC X.                
01266              88  CF-MORT-ST-PROD-CNTL                VALUE ' '.   
01267              88  CF-MORT-CARR-GRP-ST-PROD-CNTL       VALUE '1'.   
01268              88  CF-MORT-CARR-ST-PROD-CNTL           VALUE '2'.   
01269              88  CF-MORT-PROD-CNTL                   VALUE '3'.   
01270              88  CF-MORT-CARR-PROD-CNTL              VALUE '4'.   
01271                                                                   
01272          16  CF-MORTG-CONVERSION-DATE       PIC XX.               
01273          16  CF-MORTG-RATE-FILE-MAINT-DATE  PIC XX.               
01274          16  CF-MORTG-RATE-FILE-CREAT-DATE  PIC XX.               
01275          16  CF-MORTG-PROD-FILE-MAINT-DATE  PIC XX.               
01276          16  CF-MORTG-PROD-FILE-CREAT-DATE  PIC XX.               
01277                                                                   
01278          16  CF-MP-POLICY-LINKAGE-IND       PIC X(1).             
01279              88  CF-MP-PLCY-LINKAGE-USED     VALUE 'Y'.           
01280          16  CF-MP-RECON-USE-IND            PIC X(1).             
01281              88  CF-MP-USE-RECON             VALUE 'Y'.           
01282          16  CF-MORTG-CHECK-NO-COUNTER      PIC 9(6).             
01283              88  CF-MP-CHECK-CNT-RESET-VALUE VALUE 999999.        
01284          16  CF-MP-REPORT-LANGUAGE-IND      PIC X(1).             
01285              88  CF-MP-LANGUAGE-IS-ENG       VALUE 'E'.           
01286              88  CF-MP-LANGUAGE-IS-FR        VALUE 'F'.           
01287          16  FILLER                         PIC X(1).             
01288          16  CF-MORTG-CHECK-QUEUE-COUNTER   PIC 9(6).             
01289              88  CF-MP-CHKQ-CNT-RESET-VALUE  VALUE 999999.        
01290          16  CF-MORTG-MIB-VERSION           PIC X.                
01291              88  CF-MORTG-MIB-BATCH         VALUE '1'.            
01292              88  CF-MORTG-MIB-ONLINE        VALUE '2'.            
01293              88  CF-MORTG-MIB-BOTH          VALUE '3'.            
01294          16  CF-MORTG-ALT-MIB-SEARCH-CNTL.                        
01295              20  CF-MORTG-MIB-LNAME-SEARCH  PIC X.                
01296                  88  CF-MIB-LAST-NAME-SEARCH     VALUE 'Y'.       
01297              20  CF-MORTG-MIB-FNAME-SEARCH  PIC X.                
01298                  88  CF-MIB-FIRST-NAME-SEARCH    VALUE 'Y'.       
01299              20  CF-MORTG-MIB-MNAME-SEARCH  PIC X.                
01300                  88  CF-MIB-MIDDLE-NAME-SEARCH   VALUE 'Y'.       
01301              20  CF-MORTG-MIB-BDATE-SEARCH  PIC X.                
01302                  88  CF-MIB-BIRTH-DATE-SEARCH    VALUE 'Y'.       
01303              20  CF-MORTG-MIB-BSTATE-SEARCH PIC X.                
01304                  88  CF-MIB-BIRTH-STATE-SEARCH   VALUE 'Y'.       
01305              20  CF-MORTG-MIB-RSTATE-SEARCH PIC X.                
01306                  88  CF-MIB-RESIDNT-STATE-SEARCH VALUE 'Y'.       
01307          16  CF-MORTG-MIB-COMPANY-SYMBOL    PIC XXX.              
01308          16  FILLER                         PIC X(7).             
01309          16  CF-MORTG-DESTINATION-SYMBOL.                         
01310              20  CF-MORTG-MIB-COMM          PIC X(5).             
01311              20  CF-MORTG-MIB-TERM          PIC X(5).             
01312          16  CF-ASSIGN-POLICY-NO-SW         PIC X(01).            
01313              88  CF-ASSIGN-POLICY-NO             VALUE 'Y'.       
01314          16  FILLER                         PIC X(03).            
01315          16  CF-MP-CHECK-NO-CONTROL.                              
01316              20  CF-MP-CHECK-NO-METHOD      PIC X(01).            
01317                  88  CF-MP-CHECK-NO-MANUAL     VALUE '1'.         
01318                  88  CF-MP-CHECK-NO-AUTO-SEQ   VALUE '2'          
01319                                                 ' ' LOW-VALUES.   
01320                  88  CF-MP-CHECK-NO-PRE-PRINTED                   
01321                                                VALUE '3'.         
01322          16  CF-MORTG-LOAN-SHIFT-IND        PIC X(01).            
01323          16  CF-MORTG-SOLICITATION-NUM      PIC S9(17) COMP-3.    
01324          16  CF-MORTG-ALT-ALPHA-SEARCH-CNTL.                      
01325              20  CF-MORTG-ALP-LNAME-SEARCH  PIC X.                
01326                  88  CF-ALPHA-LAST-NAME-SEARCH      VALUE 'Y'.    
01327              20  CF-MORTG-ALP-FNAME-SEARCH  PIC X.                
01328                  88  CF-ALPHA-FIRST-NAME-SEARCH     VALUE 'Y'.    
01329              20  CF-MORTG-ALP-MNAME-SEARCH  PIC X.                
01330                  88  CF-ALPHA-MIDDLE-NAME-SEARCH    VALUE 'Y'.    
01331              20  CF-MORTG-ALP-BDATE-SEARCH  PIC X.                
01332                  88  CF-ALPHA-BIRTH-DATE-SEARCH     VALUE 'Y'.    
01333              20  CF-MORTG-ALP-BSTATE-SEARCH PIC X.                
01334                  88  CF-ALPHA-BIRTH-STATE-SEARCH    VALUE 'Y'.    
01335              20  CF-MORTG-ALP-RSTATE-SEARCH PIC X.                
01336                  88  CF-ALPHA-RESIDNT-STATE-SEARCH  VALUE 'Y'.    
01337          16  CF-MORTG-BILLING-AREA.                               
01338              20  CF-MORTG-BILL-CYCLE   OCCURS  5  TIMES           
01339                                             PIC X.                
01340          16  CF-MORTG-MONTH-END-DT          PIC XX.               
01341          16  CF-MORTG-CURRENT-ARCH-NUM      PIC S9(8)  COMP.      
01342          16  CF-MORTG-START-ARCH-NUM        PIC S9(8)  COMP.      
01343          16  CF-MORTG-MIB-DEST-SW           PIC X.                
01344              88 CF-MORTG-MIB-COMM-DEST              VALUE '1'.    
01345              88 CF-MORTG-MIB-TERM-DEST              VALUE '2'.    
01346          16  FILLER                         PIC X.                
01347          16  CF-MORTG-LABEL-CONTROL         PIC X.                
01348              88 CF-MORTG-CREATE-LABELS              VALUE 'Y'.    
01349              88 CF-MORTG-BYPASS-LABELS              VALUE 'N'.    
01350          16  CF-ACH-ORIGINATING-DFI-ID      PIC X(8).             
01351          16  FILLER                         PIC X(8).             
01352          16  CF-ACH-SENDING-DFI-NAME        PIC X(23).            
01353          16  CF-ACH-RECVING-DFI-ROUTING-NO  PIC X(8).             
01354          16  CF-ACH-RECVING-DFI-NAME        PIC X(23).            
01355          16  CF-ACH-COMPANY-ID.                                   
01356              20  CF-ACH-ID-CODE-DESIGNATOR  PIC X.                
01357                  88  CF-ACH-ICD-IRS-EIN             VALUE '1'.    
01358                  88  CF-ACH-ICD-DUNS                VALUE '3'.    
01359                  88  CF-ACH-ICD-USER-ASSIGNED-NO    VALUE '9'.    
01360              20  CF-ACH-COMPANY-ID-NO       PIC X(9).             
01361          16  CF-MORTG-BILL-GROUPING-CODE    PIC X.                
01362              88  CF-MORTG-CO-HAS-GROUPING           VALUE 'Y'.    
01363          16  CF-RATE-DEV-AUTHORIZATION      PIC X.                
01364              88  CF-RATE-DEV-AUTHORIZED             VALUE 'Y'.    
01365              88  CF-RATE-DEV-NOT-AUTHORIZED         VALUE 'N'.    
01366          16  CF-ACH-SENDING-DFI-ROUTING-NO  PIC X(9).             
01367          16  CF-CBA-FILE-CREATE-NUM         PIC 9(4).             
01368          16  FILLER                         PIC X(536).           
01369                                                                   
01370 ****************************************************************  
01371 *             MORTGAGE HEIGHT - WEIGHT CHARTS                  *  
01372 ****************************************************************  
01373                                                                   
01374      12  CF-FEMALE-HT-WT-REC  REDEFINES CF-RECORD-BODY.           
01375          16  CF-FEMALE-HT-WT-INFO OCCURS 30 TIMES.                
01376              20  CF-FEMALE-HEIGHT.                                
01377                  24  CF-FEMALE-FT           PIC 99.               
01378                  24  CF-FEMALE-IN           PIC 99.               
01379              20  CF-FEMALE-MIN-WT           PIC 999.              
01380              20  CF-FEMALE-MAX-WT           PIC 999.              
01381          16  FILLER                         PIC X(428).           
01382                                                                   
01383      12  CF-MALE-HT-WT-REC    REDEFINES CF-RECORD-BODY.           
01384          16  CF-MALE-HT-WT-INFO   OCCURS 30 TIMES.                
01385              20  CF-MALE-HEIGHT.                                  
01386                  24  CF-MALE-FT             PIC 99.               
01387                  24  CF-MALE-IN             PIC 99.               
01388              20  CF-MALE-MIN-WT             PIC 999.              
01389              20  CF-MALE-MAX-WT             PIC 999.              
01390          16  FILLER                         PIC X(428).           
01391 ******************************************************************
01392 *             AUTOMATIC ACTIVITY RECORD                          *
01393 ******************************************************************
01394      12  CF-AUTO-ACTIVITY-REC REDEFINES CF-RECORD-BODY.           
01395          16  CF-SYSTEM-DEFINED-ACTIVITY OCCURS 09 TIMES.          
01396              20  CF-SYS-ACTIVE-SW           PIC X(01).            
01397              20  CF-SYS-LETTER-ID           PIC X(04).            
01398              20  CF-SYS-RESEND-DAYS         PIC 9(03).            
01399              20  CF-SYS-FOLLOW-UP-DAYS      PIC 9(03).            
01400              20  CF-SYS-RESET-SW            PIC X(01).            
01401              20  CF-SYS-REPORT-DAYS         PIC 9(03).            
01402              20  CF-SYS-EACH-DAY-AFTER-SW   PIC X(01).            
01403                                                                   
01404          16  FILLER                         PIC X(50).            
01405                                                                   
01406          16  CF-USER-DEFINED-ACTIVITY  OCCURS 08 TIMES.           
01407              20  CF-USER-ACTIVE-SW          PIC X(01).            
01408              20  CF-USER-LETTER-ID          PIC X(04).            
01409              20  CF-USER-RESEND-DAYS        PIC 9(03).            
01410              20  CF-USER-FOLLOW-UP-DAYS     PIC 9(03).            
01411              20  CF-USER-RESET-SW           PIC X(01).            
01412              20  CF-USER-REPORT-DAYS        PIC 9(03).            
01413              20  CF-USER-EACH-DAY-AFTER-SW  PIC X(01).            
01414              20  CF-USER-ACTIVITY-DESC      PIC X(20).            
01415                                                                   
01416          16  FILLER                         PIC X(246).           
00422      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL102' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00425      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00426      MOVE '5'                   TO DC-OPTION-CODE.
00427      PERFORM 8000-CONVERT-DATE THRU 8000-EXIT.
00428      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00429      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00431      MOVE DFHCOMMAREA           TO PROGRAM-INTERFACE-BLOCK.
00433      MOVE +3                    TO EMI-NUMBER-OF-LINES.
00435      IF EIBCALEN = 0
00436          GO TO 8800-UNAUTHORIZED-ACCESS.
00438      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00439          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00440              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00441              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00442              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00443              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00444              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00445              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00446              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00447              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00448              MOVE 'EL102A'             TO PI-MAP-NAME
00449          ELSE
00450              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00451              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00452              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00453              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00454              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00455              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00456              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00457              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00459      EJECT
00460      
      * EXEC CICS HANDLE CONDITION
00461 *        NOTOPEN  (8870-NOTOPEN)
00462 *        NOTFND   (8880-NOT-FOUND)
00463 *        PGMIDERR (9600-PGMID-ERROR)
00464 *        ERROR    (9990-ABEND)
00465 *    END-EXEC.
           MOVE '"$JIL.                ! " #00004627' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00467      MOVE PI-MAP-NAME TO MAP-NAME.
00469      IF NOT SYSTEM-DISPLAY-CAP
00470         NEXT SENTENCE
00471       ELSE
00472      IF EIBTRNID NOT = TRANS-ID
00473          GO TO 7000-BUILD-INITIAL-MAP.
00475      IF EIBAID = DFHCLEAR
00476          GO TO 9400-CLEAR.
00478      IF NOT SYSTEM-DISPLAY-CAP
00479          MOVE 'READ'    TO SM-READ
00480          PERFORM 9995-SECURITY-VIOLATION
00481          MOVE ER-0070 TO EMI-ERROR
00482          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00483          GO TO 7000-BUILD-INITIAL-MAP.
00485      EJECT
00486  0200-RECEIVE.
00487      MOVE LOW-VALUES TO EL102AI.
00489      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00490          MOVE ER-0008 TO EMI-ERROR
00491          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00492          MOVE -1 TO COMPNMEL
00493          GO TO 8200-SEND-DATAONLY.
00495      
      * EXEC CICS RECEIVE
00496 *        MAP   (MAP-NAME)
00497 *        MAPSET(MAPSET-NAME)
00498 *        INTO  (EL102AI)
00499 *    END-EXEC.
           MOVE LENGTH OF EL102AI TO DFHEIV11
           MOVE '8"T I  L              ''   #00004655' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL102AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00501      IF PI-MAP-NAME = 'EL102A'
00502         GO TO 0290-MAP-A.
00504      IF PI-MAP-NAME = 'EL102B'
00505         GO TO 0250-MAP-B.
00507      IF PI-MAP-NAME = 'EL102C'
00508         GO TO 0260-MAP-C.
00510      IF PI-MAP-NAME = 'EL102D'
00511         GO TO 0270-MAP-D.
00513      IF PI-MAP-NAME = 'EL102E'
00514         GO TO 0280-MAP-E.
00516  0250-MAP-B.
00517      IF ENTRBPFL = 0
00518          GO TO 0300-CHECK-PFKEYS.
00520      IF EIBAID NOT = DFHENTER
00521          MOVE ER-0004 TO EMI-ERROR
00522          GO TO 0320-INPUT-ERROR.
00524      IF (ENTRBPFI NUMERIC) AND (ENTRBPFI GREATER 0 AND LESS 25)
00525          MOVE PF-VALUES (ENTRBPFI) TO EIBAID
00526      ELSE
00527          MOVE ER-0029 TO EMI-ERROR
00528          GO TO 0320-INPUT-ERROR.
00530      GO TO 0300-CHECK-PFKEYS.
00532  0260-MAP-C.
00533      IF ENTRCPFL = 0
00534          GO TO 0300-CHECK-PFKEYS.
00536      IF EIBAID NOT = DFHENTER
00537          MOVE ER-0004 TO EMI-ERROR
00538          GO TO 0320-INPUT-ERROR.
00540      IF (ENTRCPFI NUMERIC) AND (ENTRCPFI GREATER 0 AND LESS 25)
00541          MOVE PF-VALUES (ENTRCPFI) TO EIBAID
00542      ELSE
00543          MOVE ER-0029 TO EMI-ERROR
00544          GO TO 0320-INPUT-ERROR.
00546      GO TO 0300-CHECK-PFKEYS.
00548  0270-MAP-D.
00549      IF ENTRDPFL = 0
00550          GO TO 0300-CHECK-PFKEYS.
00552      IF EIBAID NOT = DFHENTER
00553          MOVE ER-0004 TO EMI-ERROR
00554          GO TO 0320-INPUT-ERROR.
00556      IF (ENTRDPFI NUMERIC) AND (ENTRDPFI GREATER 0 AND LESS 25)
00557          MOVE PF-VALUES (ENTRDPFI) TO EIBAID
00558      ELSE
00559          MOVE ER-0029 TO EMI-ERROR
00560          GO TO 0320-INPUT-ERROR.
00562      GO TO 0300-CHECK-PFKEYS.
00564  0280-MAP-E.
00565      IF ENTREPFL = 0
00566          GO TO 0300-CHECK-PFKEYS.
00568      IF EIBAID NOT = DFHENTER
00569          MOVE ER-0004 TO EMI-ERROR
00570          GO TO 0320-INPUT-ERROR.
00572      IF (ENTREPFI NUMERIC) AND (ENTREPFI GREATER 0 AND LESS 25)
00573          MOVE PF-VALUES (ENTREPFI) TO EIBAID
00574      ELSE
00575          MOVE ER-0029 TO EMI-ERROR
00576          GO TO 0320-INPUT-ERROR.
00578      GO TO 0300-CHECK-PFKEYS.
00580  0290-MAP-A.
00581      IF ENTERPFL = 0
00582          GO TO 0300-CHECK-PFKEYS.
00584      IF EIBAID NOT = DFHENTER
00585          MOVE ER-0004 TO EMI-ERROR
00586          GO TO 0320-INPUT-ERROR.
00588      IF (ENTERPFI NUMERIC) AND (ENTERPFI GREATER 0 AND LESS 25)
00589          MOVE PF-VALUES (ENTERPFI) TO EIBAID
00590      ELSE
00591          MOVE ER-0029 TO EMI-ERROR
00592          GO TO 0320-INPUT-ERROR.
00594      EJECT
00595  0300-CHECK-PFKEYS.
00596      IF EIBAID = DFHPF23
00597          GO TO 8810-PF23.
00598      IF EIBAID = DFHPF24
00599          GO TO 9200-RETURN-MAIN-MENU.
00600      IF EIBAID = DFHPF12
00601          GO TO 9500-PF12.
00603      IF EIBAID = DFHENTER AND NOT SYSTEM-MODIFY-CAP
00604          MOVE 'UPDATE'  TO SM-READ
00605          PERFORM 9995-SECURITY-VIOLATION
00606          MOVE ER-0070 TO EMI-ERROR
00607          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00608          IF PI-MAP-NAME = 'EL102A'
00609             GO TO 7000-BUILD-INITIAL-MAP
00610          ELSE
00611          IF PI-MAP-NAME = 'EL102B'
00612             GO TO 7200-BUILD-INITIAL-MAP
00613          ELSE
00614          IF PI-MAP-NAME = 'EL102C'
00615             GO TO 7300-BUILD-INITIAL-MAP
00616          ELSE
00617          IF PI-MAP-NAME = 'EL102D'
00618             GO TO 7400-BUILD-INITIAL-MAP
00619          ELSE
00620             GO TO 7500-BUILD-INITIAL-MAP.
00622      IF EIBAID = DFHENTER AND PI-MAP-NAME = 'EL102A'
00623          GO TO 0330-EDIT-DATA.
00624      IF EIBAID = DFHENTER AND PI-MAP-NAME = 'EL102B'
00625          GO TO 0530-EDIT-DATA.
00626      IF EIBAID = DFHENTER AND PI-MAP-NAME = 'EL102C'
00627          GO TO 0630-EDIT-DATA.
00628      IF EIBAID = DFHENTER AND PI-MAP-NAME = 'EL102D'
00629          GO TO 0730-EDIT-DATA.
00630      IF EIBAID = DFHENTER AND PI-MAP-NAME = 'EL102E'
00631          GO TO 0760-EDIT-DATA.
00633      IF EIBAID = DFHPF1
00634         MOVE 'EL102A'   TO PI-MAP-NAME MAP-NAME
00635         GO TO 7000-BUILD-INITIAL-MAP.
00637      IF EIBAID = DFHPF2
00638         MOVE 'EL102B'   TO PI-MAP-NAME MAP-NAME
00639         GO TO 7200-BUILD-INITIAL-MAP.
00641      IF EIBAID = DFHPF3
00642         MOVE 'EL102C'   TO PI-MAP-NAME MAP-NAME
00643         GO TO 7300-BUILD-INITIAL-MAP.
00645      IF EIBAID = DFHPF4
00646         MOVE 'EL102D'   TO PI-MAP-NAME MAP-NAME
00647         GO TO 7400-BUILD-INITIAL-MAP.
00649      IF EIBAID = DFHPF5
00650         MOVE 'EL102E'   TO PI-MAP-NAME MAP-NAME
00651         GO TO 7500-BUILD-INITIAL-MAP.
00653      MOVE ER-0029 TO EMI-ERROR.
00654  0320-INPUT-ERROR.
00655      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00656      MOVE AL-UNBON TO ENTERPFA.
00657      MOVE -1       TO ENTERPFL.
00659      IF PI-MAP-NAME = 'EL102B'
00660         GO TO 8210-SEND-DATAONLY.
00662      IF PI-MAP-NAME = 'EL102C'
00663         GO TO 8220-SEND-DATAONLY.
00665      IF PI-MAP-NAME = 'EL102D'
00666         GO TO 8230-SEND-DATAONLY.
00668      IF PI-MAP-NAME = 'EL102E'
00669         GO TO 8240-SEND-DATAONLY.
00671      GO TO 8200-SEND-DATAONLY.
00673      EJECT
00674  0330-EDIT-DATA.
00675 *    EDIT MAP A GENERAL DATA
00677      IF ZIPCODEL NOT = ZERO
00678          MOVE AL-UANON               TO ZIPCODEA.
00680      IF PHONEL NOT = ZERO
00681          MOVE PHONEI  TO DEEDIT-FIELD
00682          PERFORM 0800-DEEDIT THRU 0800-EXIT
00683          IF DEEDIT-FIELD-V0 GREATER 9999999999  OR
00684             DEEDIT-FIELD-V0 LESS    0000000000
00685              MOVE -1       TO PHONEL
00686              MOVE AL-UNBON TO PHONEA
00687              MOVE ER-0053  TO EMI-ERROR
00688              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00689          ELSE
00690              MOVE DEEDIT-FIELD       TO SV-PHONE
00691              MOVE AL-UNNON TO PHONEA.
00693      IF JOURNIDL NOT = ZERO
00694         IF JOURNIDI GREATER 99
00695            MOVE ER-0451 TO EMI-ERROR
00696            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00697            MOVE -1 TO JOURNIDL
00698            MOVE AL-UNBON TO JOURNIDA
00699         ELSE
00700            MOVE AL-UNNON TO JOURNIDA.
00702      IF SECOPTL NOT = ZERO
00703         IF SECOPTI NOT = '1' AND '2' AND '3' AND '4' AND '5'
00704             MOVE -1       TO SECOPTL
00705             MOVE AL-UABON TO SECOPTA
00706             MOVE ER-0055  TO EMI-ERROR
00707             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00708          ELSE
00709             MOVE AL-UANON TO SECOPTA.
00711      IF FPIDL NOT = ZERO
00712          MOVE FPIDI TO TERMINAL-TEST-AREA
00713          IF TERMINAL-TEST-AREA = SPACES
00714              MOVE AL-UANON TO FPIDA
00715          ELSE
00716              IF ' ' = TTA-1 OR TTA-2 OR TTA-3 OR TTA-4
00717                  MOVE -1       TO FPIDL
00718                  MOVE AL-UABON TO FPIDA
00719                  MOVE ER-0062  TO EMI-ERROR
00720                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00721              ELSE
00722                  MOVE AL-UANON TO FPIDA.
00724      IF CPIDL NOT = ZERO
00725          MOVE CPIDI TO TERMINAL-TEST-AREA
00726          IF TERMINAL-TEST-AREA = SPACES
00727              MOVE AL-UANON TO CPIDA
00728          ELSE
00729              IF ' ' = TTA-1 OR TTA-2 OR TTA-3 OR TTA-4
00730                  MOVE -1       TO CPIDL
00731                  MOVE AL-UABON TO CPIDA
00732                  MOVE ER-0062  TO EMI-ERROR
00733                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00734              ELSE
00735                  MOVE AL-UANON TO CPIDA.
00737      IF PASSWDL  GREATER ZERO
00738          MOVE SPACES TO  WS-SAVE-PASSWORD
00739          MOVE 'N'    TO  WS-PASSWORD-NUMERIC
00740          MOVE 'N'    TO  WS-PASSWORD-SIXDIGIT
00741          MOVE PASSWDI  TO  WS-SAVE-PASSWORD
00742          IF (WS-PASSWORD-X1 > SPACE) AND
00743             (WS-PASSWORD-X2 > SPACE) AND
00744             (WS-PASSWORD-X3 > SPACE) AND
00745             (WS-PASSWORD-X4 > SPACE) AND
00746             (WS-PASSWORD-X5 > SPACE) AND
00747             (WS-PASSWORD-X6 > SPACE)
00748             MOVE 'Y' TO  WS-PASSWORD-SIXDIGIT
00749          END-IF
00750          IF (WS-PASSWORD-N1  NUMERIC) OR
00751             (WS-PASSWORD-N2  NUMERIC) OR
00752             (WS-PASSWORD-N3  NUMERIC) OR
00753             (WS-PASSWORD-N4  NUMERIC) OR
00754             (WS-PASSWORD-N5  NUMERIC) OR
00755             (WS-PASSWORD-N6  NUMERIC) OR
00756             (WS-PASSWORD-N7  NUMERIC) OR
00757             (WS-PASSWORD-N8  NUMERIC)
00758             MOVE 'Y' TO  WS-PASSWORD-NUMERIC
00759          END-IF
00760          IF WS-PASSWORD-NUMERIC = 'N'  OR
00761             WS-PASSWORD-SIXDIGIT = 'N'
00762             MOVE -1             TO PASSWDL
00763             MOVE AL-UADON       TO PASSWDA
00764             MOVE ER-2549        TO EMI-ERROR
00765             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00766          ELSE
00767             MOVE AL-UADON       TO PASSWDA.
00770      IF NEXTCOL NOT = ZERO
00771         IF PI-PROCESSOR-ID = 'LGXX' OR SYSTEM-MODIFY-CAP
00772            IF NEXTCOI NOT = ZERO
00773               MOVE CONTROL-FILE TO CONTROL-FILE-SAVE-AREA
00774               MOVE NEXTCOI TO WS-COMP-ID
00775               MOVE '1'     TO WS-TYPE
00776               PERFORM 1500-VERIFY-NEXT-COMPANY THRU 1599-EXIT
00777            ELSE
00778               MOVE -1       TO NEXTCOL
00779               MOVE AL-UABON TO NEXTCOA
00780               MOVE ER-0089  TO EMI-ERROR
00781               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00782         ELSE
00783            MOVE -1       TO NEXTCOL
00784            MOVE AL-UABON TO NEXTCOA
00785            MOVE ER-0070  TO EMI-ERROR
00786            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00788      
      * EXEC CICS HANDLE CONDITION
00789 *        NOTFND(8880-NOT-FOUND)
00790 *    END-EXEC.
           MOVE '"$I                   ! # #00004899' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00792      IF CRSYSL NOT = ZERO
00793          IF CRSYSI NOT = 'N' AND 'Y'
00794              MOVE -1       TO CRSYSL
00795              MOVE AL-UNBON TO CRSYSA
00796              MOVE ER-2001  TO EMI-ERROR
00797              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00798          ELSE
00799              MOVE AL-UNNON TO CRSYSA
00800      ELSE
00801          MOVE ER-2565        TO EMI-ERROR
00802          MOVE -1             TO CRSYSL
00803          MOVE AL-UABON       TO CRSYSA
00804          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00806      IF CLSYSL NOT = ZERO
00807          IF CLSYSI NOT = 'N' AND 'Y'
00808              MOVE -1       TO CLSYSL
00809              MOVE AL-UNBON TO CLSYSA
00810              MOVE ER-2001  TO EMI-ERROR
00811              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00812          ELSE
00813              MOVE AL-UNNON TO CLSYSA
00814      ELSE
00815          MOVE ER-2565        TO EMI-ERROR
00816          MOVE -1             TO CLSYSL
00817          MOVE AL-UABON       TO CLSYSA
00818          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00820      IF MPSYSL NOT = ZERO
00821          IF MPSYSI  = 'N' OR 'Y'
00822              MOVE AL-UNNON TO MPSYSA
00823          ELSE
00824              MOVE -1       TO MPSYSL
00825              MOVE AL-UNBON TO MPSYSA
00826              MOVE ER-7219  TO EMI-ERROR
00827              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00829      IF CCSYSL NOT = ZERO
00830          IF CCSYSI  = 'N' OR 'Y'
00831              MOVE AL-UANON TO CCSYSA
00832          ELSE
00833              MOVE -1       TO CCSYSL
00834              MOVE AL-UABON TO CCSYSA
00835              MOVE ER-7228  TO EMI-ERROR
00836              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00838      IF EUSYSL NOT = ZERO
00839          IF PI-PROCESSOR-ID = 'LGXX'
00840              IF EUSYSI = 'Y' OR 'N' OR ' '
00841                  MOVE AL-UANON TO EUSYSA
00842              ELSE
00843                  MOVE -1       TO EUSYSL
00844                  MOVE AL-UABON TO EUSYSA
00845                  MOVE ER-3736  TO EMI-ERROR
00846                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00847          ELSE
00848              MOVE -1       TO EUSYSL
00849              MOVE AL-UABON TO EUSYSA
00850              MOVE ER-0007  TO EMI-ERROR
00851              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00853      IF LCCODEL NOT = ZERO
00854          IF LCCODEI = 'N' OR 'Y' OR ' '
00855              MOVE AL-UANON TO LCCODEA
00856          ELSE
00857              MOVE -1       TO LCCODEL
00858              MOVE AL-UABON TO LCCODEA
00859              MOVE ER-7229  TO EMI-ERROR
00860              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00862      IF ARSYSL NOT = ZERO
00863          IF PI-PROCESSOR-ID = 'LGXX'
00864              IF ARSYSI = 'Y' OR 'N' OR ' '
00865                  MOVE AL-UANON TO ARSYSA
00866              ELSE
00867                  MOVE -1       TO ARSYSL
00868                  MOVE AL-UABON TO ARSYSA
00869                  MOVE ER-0633  TO EMI-ERROR
00870                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00871          ELSE
00872              MOVE -1       TO ARSYSL
00873              MOVE AL-UABON TO ARSYSA
00874              MOVE ER-3143  TO EMI-ERROR
00875              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00877      IF PI-PROCESSOR-ID = 'LGXX'
00878          IF LFOVR1L NOT = ZERO
00879              IF LFOVR1I NOT = SPACES
00880                  MOVE AL-UNNON TO LFOVR1A
00881              ELSE
00882                  MOVE -1       TO LFOVR1L
00883                  MOVE AL-UNBON TO LFOVR1A
00884                  MOVE ER-2610  TO EMI-ERROR
00885                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00887      IF PI-PROCESSOR-ID = 'LGXX'
00888          IF LFOVR2L NOT = ZERO
00889              IF LFOVR2I NOT = SPACES
00890                  MOVE AL-UNNON TO LFOVR2A
00891              ELSE
00892                  MOVE -1       TO LFOVR2L
00893                  MOVE AL-UNBON TO LFOVR2A
00894                  MOVE ER-2610  TO EMI-ERROR
00895                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00897      IF PI-PROCESSOR-ID = 'LGXX'
00898          IF LFOVR6L NOT = ZERO
00899              IF LFOVR6I NOT = SPACES
00900                  MOVE AL-UNNON TO LFOVR6A
00901              ELSE
00902                  MOVE -1       TO LFOVR6L
00903                  MOVE AL-UNBON TO LFOVR6A
00904                  MOVE ER-2610  TO EMI-ERROR
00905                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00907      IF PI-PROCESSOR-ID = 'LGXX'
00908          IF LFOVR12L NOT = ZERO
00909              IF LFOVR12I NOT = SPACES
00910                  MOVE AL-UNNON TO LFOVR12A
00911              ELSE
00912                  MOVE -1       TO LFOVR12L
00913                  MOVE AL-UNBON TO LFOVR12A
00914                  MOVE ER-2610  TO EMI-ERROR
00915                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00917      IF PI-PROCESSOR-ID = 'LGXX'
00918          IF AHOVR1L NOT = ZERO
00919              IF AHOVR1I NOT = SPACES
00920                  MOVE AL-UNNON TO AHOVR1A
00921              ELSE
00922                  MOVE -1       TO AHOVR1L
00923                  MOVE AL-UNBON TO AHOVR1A
00924                  MOVE ER-2610  TO EMI-ERROR
00925                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00927      IF PI-PROCESSOR-ID = 'LGXX'
00928          IF AHOVR2L NOT = ZERO
00929              IF AHOVR2I NOT = SPACES
00930                  MOVE AL-UNNON TO AHOVR2A
00931              ELSE
00932                  MOVE -1       TO AHOVR2L
00933                  MOVE AL-UNBON TO AHOVR2A
00934                  MOVE ER-2610  TO EMI-ERROR
00935                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00937      IF PI-PROCESSOR-ID = 'LGXX'
00938          IF AHOVR6L NOT = ZERO
00939              IF AHOVR6I NOT = SPACES
00940                  MOVE AL-UNNON TO AHOVR6A
00941              ELSE
00942                  MOVE -1       TO AHOVR6L
00943                  MOVE AL-UNBON TO AHOVR6A
00944                  MOVE ER-2610  TO EMI-ERROR
00945                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00947      IF PI-PROCESSOR-ID = 'LGXX'
00948          IF AHOVR12L NOT = ZERO
00949              IF AHOVR12I NOT = SPACES
00950                  MOVE AL-UNNON TO AHOVR12A
00951              ELSE
00952                  MOVE -1       TO AHOVR12L
00953                  MOVE AL-UNBON TO AHOVR12A
00954                  MOVE ER-2610  TO EMI-ERROR
00955                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00957      IF CLSYSI = 'Y'
00958          MOVE AL-UNNON TO CURRMEA
00959          MOVE AL-SABOF TO CURRMEDA
00960      ELSE
00961          MOVE AL-SADOF TO CURRMEA
00962                           CURRMEDA
00963          GO TO 0350-EDIT-CREDIT-MONTH-END.
00965      IF CURRMEL = ZERO
00966         GO TO 0340-DAY-ERROR.
00968      MOVE AL-UNNON     TO CURRMEA.
00969      MOVE AL-SABOF     TO CURRMEDA.
00970      MOVE CURRMEI      TO DEEDIT-FIELD.
00971      PERFORM 0800-DEEDIT THRU 0800-EXIT.
00972      MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY.
00973      MOVE '4'          TO DC-OPTION-CODE.
00974      MOVE LINK-ELDATCV TO PGM-NAME.
00975      
      * EXEC CICS LINK
00976 *         PROGRAM (PGM-NAME)
00977 *         COMMAREA(DATE-CONVERSION-DATA)
00978 *         LENGTH  (DC-COMM-LENGTH)
00979 *    END-EXEC.
           MOVE '."C                   ''   #00005068' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00981      IF DATE-CONVERSION-ERROR
00982         GO TO 0340-DAY-ERROR.
00984      MOVE DC-BIN-DATE-1          TO WS-SAVE-CURRMEDT.
00985      MOVE DC-GREG-DATE-1-EDIT    TO CURRMEI.
00986      MOVE DC-GREG-DATE-1-MDY     TO DATE-TEST-AREA.
00988      IF DATE-TEST-DD = DC-DAYS-IN-MONTH OR 01
00989         GO TO 0350-EDIT-CREDIT-MONTH-END.
00991  0340-DAY-ERROR.
00992      MOVE -1       TO CURRMEL.
00993      MOVE AL-UABON TO CURRMEA.
00994      MOVE ER-0340  TO EMI-ERROR.
00995      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00997  0350-EDIT-CREDIT-MONTH-END.
00998      IF CRSYSI = 'Y'
00999          MOVE AL-UNNON TO CREDMEA
01000          MOVE AL-SABOF TO CREDMEDA
01001      ELSE
01002          MOVE AL-SADOF TO CREDMEA
01003                           CREDMEDA
01004          GO TO 0375-EDIT-MORTGAGE-MONTH-END.
01006      IF CREDMEL = ZERO
01007         GO TO 0370-DAY-ERROR.
01009      MOVE AL-UNNON     TO CREDMEA.
01010      MOVE AL-SABOF     TO CREDMEDA.
01011      MOVE CREDMEI      TO DEEDIT-FIELD.
01012      PERFORM 0800-DEEDIT THRU 0800-EXIT.
01013      MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY.
01014      MOVE '4'          TO DC-OPTION-CODE.
01015      MOVE LINK-ELDATCV TO PGM-NAME.
01016      
      * EXEC CICS LINK
01017 *         PROGRAM (PGM-NAME)
01018 *         COMMAREA(DATE-CONVERSION-DATA)
01019 *         LENGTH  (DC-COMM-LENGTH)
01020 *    END-EXEC.
           MOVE '."C                   ''   #00005102' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01022      IF DATE-CONVERSION-ERROR
01023          GO TO 0370-DAY-ERROR.
01025      MOVE DC-BIN-DATE-1          TO WS-SAVE-CREDMEDT.
01026      MOVE DC-GREG-DATE-1-EDIT    TO CREDMEI.
01027      MOVE DC-GREG-DATE-1-MDY     TO DATE-TEST-AREA.
01028      IF DATE-TEST-DD = DC-DAYS-IN-MONTH OR 01
01029         GO TO 0375-EDIT-MORTGAGE-MONTH-END.
01031  0370-DAY-ERROR.
01032      MOVE -1       TO CREDMEL.
01033      MOVE AL-UABON TO CREDMEA.
01034      MOVE ER-0587  TO EMI-ERROR.
01035      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01037  0375-EDIT-MORTGAGE-MONTH-END.
01039      IF MORTMEL = ZERO
01040         GO TO 0395-EDIT-AR-MONTH-END.
01042      IF MPSYSI = 'Y'
01043          MOVE AL-UNNON TO MORTMEA
01044          MOVE AL-SABOF TO MORTMEDA
01045      ELSE
01046          MOVE AL-SADOF TO MORTMEA
01047                           MORTMEDA
01048          GO TO 0395-EDIT-AR-MONTH-END.
01050      MOVE AL-UNNON     TO MORTMEA.
01051      MOVE AL-SABOF     TO MORTMEDA.
01052      MOVE MORTMEI      TO DEEDIT-FIELD.
01053      PERFORM 0800-DEEDIT THRU 0800-EXIT.
01054      MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY.
01055      MOVE '4'          TO DC-OPTION-CODE.
01056      MOVE LINK-ELDATCV TO PGM-NAME.
01057      
      * EXEC CICS LINK
01058 *         PROGRAM (PGM-NAME)
01059 *         COMMAREA(DATE-CONVERSION-DATA)
01060 *         LENGTH  (DC-COMM-LENGTH)
01061 *    END-EXEC.
           MOVE '."C                   ''   #00005136' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01063      IF DATE-CONVERSION-ERROR
01064         GO TO 0380-DAY-ERROR.
01066      MOVE DC-BIN-DATE-1          TO WS-SAVE-MORTMEDT.
01067      MOVE DC-GREG-DATE-1-EDIT    TO MORTMEI.
01068      MOVE DC-GREG-DATE-1-MDY     TO DATE-TEST-AREA.
01070      IF DATE-TEST-DD = DC-DAYS-IN-MONTH OR 01
01071         GO TO 0395-EDIT-AR-MONTH-END.
01073  0380-DAY-ERROR.
01074      MOVE -1       TO MORTMEL.
01075      MOVE AL-UABON TO MORTMEA.
01076      MOVE ER-9001  TO EMI-ERROR.
01077      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01079  0395-EDIT-AR-MONTH-END.
01080      IF PI-PROCESSOR-ID = 'LGXX' AND
01081         (PI-AR-PROCESSING OR ARSYSI = 'Y')
01082          MOVE AL-UNNON TO ARMEA
01083          MOVE AL-SABOF TO ARMEDA
01084          MOVE AL-UNNON TO AR860DTA
01085      ELSE
01086          MOVE AL-SADOF TO ARMEA
01087                           ARMEDA
01088                           AR860DTA
01089          GO TO 0400-EDIT-DATA-CONTINUE.
01091      IF AR860DTL = ZERO
01092         NEXT SENTENCE
01093      ELSE
01094          MOVE AR860DTI     TO DEEDIT-FIELD
01095          PERFORM 0800-DEEDIT THRU 0800-EXIT
01096          MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY
01097          MOVE '4'          TO DC-OPTION-CODE
01098          MOVE LINK-ELDATCV TO PGM-NAME
01099          
      * EXEC CICS LINK
01100 *             PROGRAM (PGM-NAME)
01101 *             COMMAREA(DATE-CONVERSION-DATA)
01102 *             LENGTH  (DC-COMM-LENGTH)
01103 *        END-EXEC
           MOVE '."C                   ''   #00005172' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01104          IF DATE-CONVERSION-ERROR
01105              MOVE -1       TO AR860DTL
01106              MOVE AL-UABON TO AR860DTA
01107              MOVE ER-3187  TO EMI-ERROR
01108              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01109              GO TO 0400-EDIT-DATA-CONTINUE
01110          ELSE
01111              MOVE DC-BIN-DATE-1          TO WS-SAVE-AR860DT
01112              MOVE DC-GREG-DATE-1-EDIT    TO AR860DTI.
01114      IF ARMEL = ZERO
01115         GO TO 0397-DAY-ERROR.
01117      MOVE AL-UNNON     TO ARMEA.
01118      MOVE AL-SABOF     TO ARMEDA.
01119      MOVE AL-SANOF     TO AR860DTA.
01120      MOVE ARMEI        TO DEEDIT-FIELD.
01121      PERFORM 0800-DEEDIT THRU 0800-EXIT.
01122      MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY.
01123      MOVE '4'          TO DC-OPTION-CODE.
01124      MOVE LINK-ELDATCV TO PGM-NAME.
01125      
      * EXEC CICS LINK
01126 *         PROGRAM (PGM-NAME)
01127 *         COMMAREA(DATE-CONVERSION-DATA)
01128 *         LENGTH  (DC-COMM-LENGTH)
01129 *    END-EXEC.
           MOVE '."C                   ''   #00005196' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01131      IF DATE-CONVERSION-ERROR
01132         GO TO 0397-DAY-ERROR.
01134      MOVE DC-BIN-DATE-1          TO WS-SAVE-ARMEDT.
01135      MOVE DC-GREG-DATE-1-EDIT    TO ARMEI.
01136      MOVE DC-GREG-DATE-1-MDY     TO DATE-TEST-AREA.
01138      IF DATE-TEST-DD = DC-DAYS-IN-MONTH OR 01
01139         GO TO 0400-EDIT-DATA-CONTINUE.
01141  0397-DAY-ERROR.
01142      MOVE -1       TO ARMEL.
01143      MOVE AL-UABON TO ARMEA.
01144      MOVE ER-3156  TO EMI-ERROR.
01145      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01147  0400-EDIT-DATA-CONTINUE.
01149      IF EMI-NO-ERRORS
01150          GO TO 1000-UPDATE-FILE.
01152      GO TO 8200-SEND-DATAONLY.
01154      EJECT
01155  0530-EDIT-DATA.
01156 *    EDIT EL102 MAP B CLAIM DATA
01158      IF APPROVL NOT = +0
01159          IF APPROVI NOT = 'N' AND 'Y' AND 'G'
01160              MOVE -1       TO APPROVL
01161              MOVE AL-UABON TO APPROVA
01162              MOVE ER-1888  TO EMI-ERROR
01163              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01164          ELSE
01165              MOVE AL-UANON TO APPROVA.
01167      IF  LLEV1L NOT = ZERO
01168          MOVE LLEV1I              TO DEEDIT-FIELD
01169          PERFORM 0800-DEEDIT THRU 0800-EXIT
01170          IF  DEEDIT-FIELD-V0 NOT NUMERIC
01171              MOVE -1               TO LLEV1L
01172              MOVE AL-UNBON         TO LLEV1A
01173              MOVE ER-1890          TO EMI-ERROR
01174              PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT
01175          ELSE
01176              IF  DEEDIT-FIELD-V0 = ZEROS
01177                      AND
01178                  APPROVI = 'G'
01179                  MOVE -1           TO LLEV1L
01180                  MOVE AL-UNBON     TO LLEV1A
01181                  MOVE ER-1890      TO EMI-ERROR
01182                  PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT
01183              ELSE
01184                  MOVE DEEDIT-FIELD-V0
01185                                    TO SV-LLEV1
01186                                       LLEV1O
01187                  MOVE AL-UNNON     TO LLEV1A
01188      ELSE
01189          IF  APPROVI = 'G'
01190              MOVE -1               TO LLEV1L
01191              MOVE AL-UNBON         TO LLEV1A
01192              MOVE ER-1887          TO EMI-ERROR
01193              PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
01195      IF  LLEV2L NOT = ZERO
01196          MOVE LLEV2I               TO DEEDIT-FIELD
01197          PERFORM 0800-DEEDIT THRU 0800-EXIT
01198          IF  DEEDIT-FIELD-V0 NOT NUMERIC
01199              MOVE -1               TO LLEV2L
01200              MOVE AL-UNBON         TO LLEV2A
01201              MOVE ER-1890          TO EMI-ERROR
01202              PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT
01203          ELSE
01204              IF  DEEDIT-FIELD-V0 = ZEROS
01205                      AND
01206                  APPROVI = 'G'
01207                  MOVE -1           TO LLEV2L
01208                  MOVE AL-UNBON     TO LLEV2A
01209                  MOVE ER-1890      TO EMI-ERROR
01210                  PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT
01212              ELSE
01213                  MOVE DEEDIT-FIELD-V0
01214                                    TO SV-LLEV2
01215                                       LLEV2O
01216                  MOVE AL-UNNON     TO LLEV2A
01217      ELSE
01218          IF  APPROVI = 'G'
01219              MOVE -1               TO LLEV2L
01220              MOVE AL-UNBON         TO LLEV2A
01221              MOVE ER-1887          TO EMI-ERROR
01222              PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
01224      IF  LLEV3L NOT = ZERO
01225          MOVE LLEV3I               TO DEEDIT-FIELD
01226          PERFORM 0800-DEEDIT THRU 0800-EXIT
01227          IF  DEEDIT-FIELD-V0 NOT NUMERIC
01228              MOVE -1               TO LLEV3L
01229              MOVE AL-UNBON         TO LLEV3A
01230              MOVE ER-1890          TO EMI-ERROR
01231              PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT
01232          ELSE
01233              IF  DEEDIT-FIELD-V0 = ZEROS
01234                      AND
01235                  APPROVI = 'G'
01236                  MOVE -1           TO LLEV3L
01237                  MOVE AL-UNBON     TO LLEV3A
01238                  MOVE ER-1890      TO EMI-ERROR
01239                  PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT
01241              ELSE
01242                  MOVE DEEDIT-FIELD-V0
01243                                    TO SV-LLEV3
01244                                       LLEV3O
01245                  MOVE AL-UNNON     TO LLEV3A
01246      ELSE
01247          IF  APPROVI = 'G'
01248              MOVE -1               TO LLEV3L
01249              MOVE AL-UNBON         TO LLEV3A
01250              MOVE ER-1887          TO EMI-ERROR
01251              PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
01253      IF  ALEV1L NOT = ZERO
01254          MOVE ALEV1I              TO DEEDIT-FIELD
01255          PERFORM 0800-DEEDIT THRU 0800-EXIT
01256          IF  DEEDIT-FIELD-V0 NOT NUMERIC
01257              MOVE -1              TO ALEV1L
01258              MOVE AL-UNBON        TO ALEV1A
01259              MOVE ER-1890         TO EMI-ERROR
01260              PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT
01261          ELSE
01262              IF  DEEDIT-FIELD-V0 = ZEROS
01263                      AND
01264                  APPROVI = 'G'
01265                  MOVE -1           TO ALEV1L
01266                  MOVE AL-UNBON     TO ALEV1A
01267                  MOVE ER-1890      TO EMI-ERROR
01268                  PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT
01270              ELSE
01271                  MOVE DEEDIT-FIELD-V0
01272                                    TO SV-ALEV1
01273                                       ALEV1O
01274                  MOVE AL-UNNON     TO ALEV1A
01275      ELSE
01276          IF  APPROVI = 'G'
01277              MOVE -1              TO ALEV1L
01278              MOVE AL-UNBON        TO ALEV1A
01279              MOVE ER-1887         TO EMI-ERROR
01280              PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
01282      IF  ALEV2L NOT = ZERO
01283          MOVE ALEV2I              TO DEEDIT-FIELD
01284          PERFORM 0800-DEEDIT THRU 0800-EXIT
01285          IF  DEEDIT-FIELD-V0 NOT NUMERIC
01286              MOVE -1              TO ALEV2L
01287              MOVE AL-UNBON        TO ALEV2A
01288              MOVE ER-1890         TO EMI-ERROR
01289              PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT
01290          ELSE
01291              IF  DEEDIT-FIELD-V0 = ZEROS
01292                      AND
01293                  APPROVI = 'G'
01294                  MOVE -1           TO ALEV2L
01295                  MOVE AL-UNBON     TO ALEV2A
01296                  MOVE ER-1890      TO EMI-ERROR
01297                  PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT
01299              ELSE
01300                  MOVE DEEDIT-FIELD-V0
01301                                    TO SV-ALEV2
01302                                       ALEV2O
01303                  MOVE AL-UNNON     TO ALEV2A
01304      ELSE
01305          IF  APPROVI = 'G'
01306              MOVE -1              TO ALEV2L
01307              MOVE AL-UNBON        TO ALEV2A
01308              MOVE ER-1887         TO EMI-ERROR
01309              PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
01311      IF  ALEV3L NOT = ZERO
01312          MOVE ALEV3I              TO DEEDIT-FIELD
01313          PERFORM 0800-DEEDIT THRU 0800-EXIT
01314          IF  DEEDIT-FIELD-V0 NOT NUMERIC
01315              MOVE -1              TO ALEV3L
01316              MOVE AL-UNBON        TO ALEV3A
01317              MOVE ER-1890         TO EMI-ERROR
01318              PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT
01319          ELSE
01320              IF  DEEDIT-FIELD-V0 = ZEROS
01321                      AND
01322                  APPROVI = 'G'
01323                  MOVE -1           TO ALEV3L
01324                  MOVE AL-UNBON     TO ALEV3A
01325                  MOVE ER-1890      TO EMI-ERROR
01326                  PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT
01328              ELSE
01329                  MOVE DEEDIT-FIELD-V0
01330                                    TO SV-ALEV3
01331                                       ALEV3O
01332                  MOVE AL-UNNON     TO ALEV3A
01333      ELSE
01334          IF  APPROVI = 'G'
01335              MOVE -1              TO ALEV3L
01336              MOVE AL-UNBON        TO ALEV3A
01337              MOVE ER-1887         TO EMI-ERROR
01338              PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
01340  0535-CK-PROC.
01341      IF PI-PROCESSOR-ID = 'LGXX'
01342          IF LSTCLML NOT = ZERO
01343              IF LSTCLMI NOT NUMERIC
01344                  MOVE -1       TO LSTCLML
01345                  MOVE AL-UNBON TO LSTCLMA
01346                  MOVE ER-0610  TO EMI-ERROR
01347                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01348              ELSE
01349                  MOVE AL-UNNON               TO LSTCLMA.
01351      IF PI-PROCESSOR-ID = 'LGXX'
01352          IF LSTARCHL NOT = ZERO
01353              IF LSTARCHI NOT NUMERIC
01354                  MOVE -1       TO LSTARCHL
01355                  MOVE AL-UNBON TO LSTARCHA
01356                  MOVE ER-0611  TO EMI-ERROR
01357                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01358              ELSE
01359                  MOVE AL-UNNON               TO LSTARCHA.
01361      IF PI-PROCESSOR-ID = 'LGXX'
01362          IF LSTCHKL NOT = ZERO
01363              IF LSTCHKI NOT NUMERIC
01364                  MOVE -1       TO LSTCHKL
01365                  MOVE AL-UNBON TO LSTCHKA
01366                  MOVE ER-0612  TO EMI-ERROR
01367                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01368              ELSE
01369                  MOVE AL-UNNON               TO LSTCHKA.
01371      IF PI-PROCESSOR-ID = 'LGXX'
01372          IF STARCHL NOT = ZERO
01373              IF STARCHI NOT NUMERIC
01374                  MOVE -1       TO STARCHL
01375                  MOVE AL-UNBON TO STARCHA
01376                  MOVE ER-0611  TO EMI-ERROR
01377                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01378              ELSE
01379                  MOVE AL-UNNON               TO STARCHA.
01381      IF CLMTOLL NOT = ZERO
01382          
      * EXEC CICS BIF
01383 *            DEEDIT
01384 *            FIELD (CLMTOLI)
01385 *            LENGTH(5)
01386 *        END-EXEC
           MOVE 5 TO DFHEIV11
           MOVE '@"L                   #   #00005428' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMTOLI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01387          IF CLMTOLI NOT NUMERIC
01388              MOVE -1               TO CLMTOLL
01389              MOVE AL-UNBON         TO CLMTOLA
01390              MOVE ER-0496          TO EMI-ERROR
01391              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01392            ELSE
01393              MOVE CLMTOLI          TO SV-CLMTOL
01394                                       CLMTOLO
01395              MOVE AL-UNNON         TO CLMTOLA.
01397      IF PI-PROCESSOR-ID = 'LGXX'
01398          IF LSTQUEL NOT = ZERO
01399              IF LSTQUEI NOT NUMERIC
01400                  MOVE -1       TO LSTQUEL
01401                  MOVE AL-UNBON TO LSTQUEA
01402                  MOVE ER-0613  TO EMI-ERROR
01403                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01404              ELSE
01405                  MOVE AL-UNNON               TO LSTQUEA.
01407      IF CLMREJL NOT = ZERO
01408          IF CLMREJI NOT = '1' AND ' '
01409              MOVE -1       TO CLMREJL
01410              MOVE AL-UABON TO CLMREJA
01411              MOVE ER-2024  TO EMI-ERROR
01412              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01413          ELSE
01414              MOVE AL-UANON TO CLMREJA.
01416      IF PMTSCHKL GREATER THAN +0
01417         IF PMTSCHKI NUMERIC
01418            NEXT SENTENCE
01419         ELSE
01420            MOVE -1       TO PMTSCHKL
01421            MOVE AL-UNBON TO PMTSCHKA
01422            MOVE ER-0631  TO EMI-ERROR
01423            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01425      IF THRUTOL GREATER THAN +0
01426         IF THRUTOI = '1' OR ' '
01427            NEXT SENTENCE
01428         ELSE
01429            MOVE -1       TO THRUTOL
01430            MOVE AL-UNBON TO THRUTOA
01431            MOVE ER-0634  TO EMI-ERROR
01432            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01434      IF ADDRLBLL IS GREATER THAN +0
01435          IF ADDRLBLI NOT = ' ' AND 'Y' AND 'N'
01436              MOVE -1             TO  ADDRLBLL
01437              MOVE AL-UABON       TO  ADDRLBLA
01438              MOVE ER-0680        TO  EMI-ERROR
01439              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01440          ELSE
01441              MOVE AL-UANON       TO  ADDRLBLA.
01443      IF RECONL IS GREATER THAN +0
01444          IF (RECONI NOT = ' ' AND 'Y' AND 'N')
01445              MOVE ER-0762        TO  EMI-ERROR
01446              MOVE -1             TO  RECONL
01447              MOVE AL-UABON       TO  RECONA
01448              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01449          ELSE
01450              MOVE AL-UANON       TO  RECONA.
01452      IF PI-PROCESSOR-ID = 'LGXX'
01453          IF PROCDTL NOT = ZERO
01454              MOVE PROCDTI        TO  DEEDIT-FIELD
01455              PERFORM 0800-DEEDIT THRU 0800-EXIT
01456              MOVE DEEDIT-FIELD   TO  DC-GREG-DATE-1-MDY
01457              MOVE '4'            TO  DC-OPTION-CODE
01458              PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
01459              IF NO-CONVERSION-ERROR
01460                  MOVE DC-GREG-DATE-1-EDIT
01461                                  TO  PROCDTI
01462                  MOVE DC-BIN-DATE-1  TO  WS-SAVE-PROCDT
01463              ELSE
01464                  MOVE ER-0021    TO  EMI-ERROR
01465                  MOVE -1         TO  PROCDTL
01466                  MOVE AL-UABON   TO  PROCDTA
01467                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01469      MOVE LOW-VALUES TO WS-SAVE-COFFDTE.
01471      IF COFFDTL EQUAL +0
01472         GO TO 0530-MAPB.
01474      MOVE COFFDTI      TO DEEDIT-FIELD.
01475      PERFORM 0800-DEEDIT THRU 0800-EXIT.
01477      IF DEEDIT-FIELD EQUAL ZEROS
01478         MOVE LOW-VALUES TO WS-SAVE-COFFDTE
01479         MOVE SPACES TO COFFDTI
01480         GO TO 0530-MAPB.
01482      MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY.
01483      MOVE '4'          TO DC-OPTION-CODE.
01484      PERFORM 8000-CONVERT-DATE.
01486      IF DATE-CONVERSION-ERROR
01487         MOVE -1       TO COFFDTL
01488         MOVE AL-UABON TO COFFDTA
01489         MOVE ER-0314  TO EMI-ERROR
01490         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01491         GO TO 0530-MAPB.
01493      MOVE DC-BIN-DATE-1          TO WS-SAVE-COFFDTE.
01494      MOVE DC-GREG-DATE-1-EDIT    TO COFFDTI.
01496  0530-MAPB.
01497      IF  EXPRETNL GREATER THAN +0
01498          IF  EXPRETNI NOT NUMERIC
01499                  OR
01500              EXPRETNI LESS THAN 1
01501                  OR
01502              EXPRETNI GREATER THAN 6
01503              MOVE -1             TO  EXPRETNL
01504              MOVE AL-UABON       TO  EXPRETNA
01505              MOVE ER-7744        TO  EMI-ERROR
01506              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01507          ELSE
01508              MOVE EXPRETNI       TO  W-EXPRETN
01509                                      EXPRETNO
01510              MOVE AL-UNNON       TO  EXPRETNA.
01512      IF  RSVOPSWL NOT = ZERO
01513              AND
01514          RSVOPSWI NOT EQUAL PI-OPTIONAL-RESERVE-SW
01516          IF  PI-PROCESSOR-ID NOT = 'LHJ '
01517              MOVE 'N'            TO RSVOPSWI
01518              MOVE -1             TO RSVOPSWL
01519              MOVE AL-UABON       TO RSVOPSWA
01520              MOVE ER-7720        TO EMI-ERROR
01521              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01523          ELSE
01524              IF  RSVOPSWI EQUAL 'Y' OR 'N'
01525                  MOVE AL-UANON   TO RSVOPSWA
01526                  MOVE RSVOPSWI   TO PI-OPTIONAL-RESERVE-SW
01528              ELSE
01529                  MOVE -1         TO RSVOPSWL
01530                  MOVE AL-UABON   TO RSVOPSWA
01531                  MOVE ER-7721    TO EMI-ERROR
01532                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01534      IF  PI-OPTIONAL-RESERVE-SW EQUAL 'N'
01535          MOVE -1                 TO RSVOPSWL
01536          MOVE AL-UABON           TO RSVOPSWA
01537          MOVE ER-7724            TO EMI-ERROR
01538          MOVE ZEROS              TO IBLGMTHO
01539                                     W-IBLGMTH
01540                                     CIDADISO
01541                                     W-CIDADIS
01542                                     CALCINTO
01543                                     W-CALCINT
01544                                     IBNRLFFO
01545                                     W-IBNRAHF
01546                                     IBNRAHFO
01547                                     W-IBNRAHF
01548          MOVE LOW-VALUES         TO CRDTBLUO
01549                                     OPTDTEO
01550                                     W-OPTDTE
01551          MOVE AL-UNNON           TO CALCINTA
01552                                     CIDADISA
01553                                     IBLGMTHA
01554                                     IBNRAHFA
01555                                     IBNRLFFA
01556          MOVE AL-UANON           TO RSVOPSWA
01557                                     CRDTBLUA
01558                                     OPTDTEA
01559          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01560          GO TO 0530-SCRNB-CONT.
01562      IF  OPTDTEL GREATER THAN +0
01563          MOVE LOW-VALUES         TO W-OPTDTE
01564          
      * EXEC CICS BIF
01565 *             DEEDIT
01566 *             FIELD  (OPTDTEI)
01567 *             LENGTH (08)
01568 *        END-EXEC
           MOVE 08 TO DFHEIV11
           MOVE '@"L                   #   #00005589' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 OPTDTEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01570          IF  OPTDTEI NUMERIC
01572              IF  OPTDTEI EQUAL ZEROS
01573                  MOVE SPACES     TO OPTDTEI
01575              ELSE
01576                  MOVE OPTDTEI    TO DC-GREG-DATE-1-MDY
01577                  MOVE '4'        TO DC-OPTION-CODE
01578                  PERFORM 8000-CONVERT-DATE
01580                  IF  DATE-CONVERSION-ERROR
01581                      MOVE -1     TO OPTDTEL
01582                      MOVE AL-UABON
01583                                  TO OPTDTEA
01584                      MOVE ER-7740
01585                                  TO EMI-ERROR
01586                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01588                  ELSE
01589                      MOVE DC-BIN-DATE-1
01590                                  TO W-OPTDTE
01591                      MOVE DC-GREG-DATE-1-EDIT
01592                                  TO OPTDTEO
01593                      MOVE AL-UANON
01594                                  TO OPTDTEA
01596          ELSE
01597              MOVE -1             TO OPTDTEL
01598              MOVE AL-UABON       TO OPTDTEA
01599              MOVE ER-7740        TO EMI-ERROR
01600              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01602      IF  CIDADISL GREATER THAN ZEROS
01603          MOVE ZERO               TO W-CIDADIS
01604          
      * EXEC CICS BIF
01605 *             DEEDIT
01606 *             FIELD  (CIDADISI)
01607 *             LENGTH (06)
01608 *        END-EXEC
           MOVE 06 TO DFHEIV11
           MOVE '@"L                   #   #00005622' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CIDADISI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01610          IF  CIDADISI NUMERIC
01611              MOVE AL-UNNON       TO CIDADISA
01612              MOVE CIDADISI       TO W-CIDADIS
01613                                     CIDADISO
01615          ELSE
01616              MOVE ZEROS          TO W-CIDADIS
01617              MOVE -1             TO CIDADISL
01618              MOVE AL-UABON       TO CIDADISA
01619              MOVE ER-7723        TO EMI-ERROR
01620              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01622      IF  CRDTBLUL GREATER THAN ZEROS
01623          PERFORM 0560-CHECK-TABLE THRU 0560-EXIT.
01625      IF  IBNRLFFL GREATER THAN ZEROS
01626          MOVE ZERO               TO W-IBNRLFF
01627          
      * EXEC CICS BIF
01628 *             DEEDIT
01629 *             FIELD  (IBNRLFFI)
01630 *             LENGTH (06)
01631 *        END-EXEC
           MOVE 06 TO DFHEIV11
           MOVE '@"L                   #   #00005641' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 IBNRLFFI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01633          IF  IBNRLFFI NUMERIC
01634              MOVE AL-UNNON       TO IBNRLFFA
01635              MOVE IBNRLFFI       TO W-IBNRLFF
01636                                     IBNRLFFO
01638          ELSE
01639              MOVE ZEROS          TO W-IBNRLFF
01640              MOVE -1             TO IBNRLFFL
01641              MOVE AL-UABON       TO IBNRLFFA
01642              MOVE ER-7742        TO EMI-ERROR
01643              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01645      IF  IBNRAHFL GREATER THAN ZEROS
01646          MOVE ZERO               TO W-IBNRAHF
01647          
      * EXEC CICS BIF
01648 *             DEEDIT
01649 *             FIELD  (IBNRAHFI)
01650 *             LENGTH (06)
01651 *        END-EXEC
           MOVE 06 TO DFHEIV11
           MOVE '@"L                   #   #00005658' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 IBNRAHFI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01653          IF  IBNRAHFI NUMERIC
01654              MOVE AL-UNNON       TO IBNRAHFA
01655              MOVE IBNRAHFI       TO W-IBNRAHF
01656                                     IBNRAHFO
01658          ELSE
01659              MOVE ZEROS          TO W-IBNRAHF
01660              MOVE -1             TO IBNRAHFL
01661              MOVE AL-UABON       TO IBNRAHFA
01662              MOVE ER-7741        TO EMI-ERROR
01663              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01665      IF  IBLGMTHL GREATER THAN ZEROS
01666          MOVE ZERO               TO W-IBLGMTH
01667          
      * EXEC CICS BIF
01668 *             DEEDIT
01669 *             FIELD  (IBLGMTHI)
01670 *             LENGTH (03)
01671 *        END-EXEC
           MOVE 03 TO DFHEIV11
           MOVE '@"L                   #   #00005675' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 IBLGMTHI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01673          IF  IBLGMTHI NUMERIC
01674              MOVE AL-UNNON       TO IBLGMTHA
01675              MOVE IBLGMTHI       TO W-IBLGMTH
01676                                     IBLGMTHO
01678          ELSE
01679              MOVE ZEROS          TO W-IBLGMTH
01680              MOVE -1             TO IBLGMTHL
01681              MOVE AL-UABON       TO IBLGMTHA
01682              MOVE ER-7722        TO EMI-ERROR
01683              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01685      IF  CALCINTL GREATER THAN ZEROS
01686          MOVE ZERO               TO W-CALCINT
01687          
      * EXEC CICS BIF
01688 *             DEEDIT
01689 *             FIELD  (CALCINTI)
01690 *             LENGTH (06)
01691 *        END-EXEC
           MOVE 06 TO DFHEIV11
           MOVE '@"L                   #   #00005692' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CALCINTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01693          IF  CALCINTI NUMERIC
01694              MOVE AL-UNNON       TO CALCINTA
01695              MOVE CALCINTI       TO W-CALCINT
01696                                     CALCINTO
01698          ELSE
01699              MOVE ZEROS          TO W-CALCINT
01700              MOVE -1             TO CALCINTL
01701              MOVE AL-UABON       TO CALCINTA
01702              MOVE ER-7735        TO EMI-ERROR
01703              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01705  0530-SCRNB-CONT.
01707      IF EMI-NO-ERRORS
01708             OR
01709         EMI-FATAL-CTR EQUAL ZERO
01710         GO TO 2000-UPDATE-FILE.
01712      GO TO 8210-SEND-DATAONLY.
01714  0530-EXIT.
01715      EXIT.
01716                                  EJECT
01717  0560-CHECK-TABLE.
01719      IF  CRDTBLUI EQUAL SPACES
01720          MOVE '9'                TO CRDTBLUI.
01722      
      * EXEC CICS HANDLE CONDITION
01723 *        NOTOPEN (8870-NOTOPEN)
01724 *        NOTFND  (0560-NOT-FOUND)
01725 *        ENDFILE (0560-NOT-FOUND)
01726 *    END-EXEC.
           MOVE '"$JI''                 ! $ #00005719' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01728      MOVE LOW-VALUES             TO W-WORKING-CNTL-KEY.
01729      MOVE PI-COMPANY-ID          TO W-CNTL-COMP-ID.
01730      MOVE 'B'                    TO W-CNTL-REC-TYPE.
01731      MOVE CRDTBLUI               TO W-CNTL-TABLE-INDICATOR
01732                                     W-CREDIBLITY-TABLE.
01734      
      * EXEC CICS STARTBR
01735 *        DATASET ('ELCNTL')
01736 *        RIDFLD  (W-WORKING-CNTL-KEY)
01737 *        GTEQ
01738 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE 0 TO DFHEIV11
           MOVE '&,         G          &   #00005729' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 W-WORKING-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01740      
      * EXEC CICS READNEXT
01741 *        DATASET ('ELCNTL')
01742 *        RIDFLD  (W-WORKING-CNTL-KEY)
01743 *        SET     (ADDRESS OF CONTROL-FILE)
01744 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE 0 TO DFHEIV11
           MOVE '&.S                   )   #00005734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-WORKING-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01746      IF  CRDTBLUI EQUAL W-CNTL-TABLE-INDICATOR
01747          MOVE AL-UNNON           TO CRDTBLUA
01748          GO TO 0560-EXIT.
01750  0560-NOT-FOUND.
01752      MOVE -1                     TO CRDTBLUL.
01753      MOVE AL-UABON               TO CRDTBLUA.
01754      MOVE ER-7738                TO EMI-ERROR.
01755      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01757  0560-EXIT.
01758      EXIT.
01759      EJECT
01760  0630-EDIT-DATA.
01761      IF PI-PROCESSOR-ID = 'LGXX'
01762          IF CRTACSL NOT = ZERO
01763              IF CRTACSI NOT = ' ' AND '1' AND '2' AND '3' AND '4'
01764                  MOVE -1       TO CRTACSL
01765                  MOVE AL-UABON TO CRTACSA
01766                  MOVE ER-0056  TO EMI-ERROR
01767                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01768              ELSE
01769                  MOVE AL-UANON TO CRTACSA.
01771      IF REMTRML NOT = ZERO
01772          IF REMTRMI NOT = '1' AND '2' AND '3' AND '4'
01773                     AND '5' AND '6' AND '7'
01774              MOVE -1       TO REMTRML
01775              MOVE AL-UABON TO REMTRMA
01776              MOVE ER-0771  TO EMI-ERROR
01777              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01778          ELSE
01779              MOVE AL-UANON TO REMTRMA.
01781      IF R78MTHL NOT = ZERO
01782          IF R78MTHI NOT = ' ' AND '1'
01783              MOVE -1       TO R78MTHL
01784              MOVE AL-UABON TO R78MTHA
01785              MOVE ER-0072  TO EMI-ERROR
01786              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01787          ELSE
01788              MOVE AL-UANON TO R78MTHA.
01790      IF TRMOPTNL NOT = ZERO
01791          IF TRMOPTNI NOT = '1' AND '2' AND '3' AND '4' AND ' '
01792              MOVE -1       TO TRMOPTNL
01793              MOVE AL-UABON TO TRMOPTNA
01794              MOVE ER-0071  TO EMI-ERROR
01795              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01796          ELSE
01797              MOVE AL-UANON TO TRMOPTNA.
01799      IF DABILLL NOT = ZERO
01800          IF DABILLI NOT = 'Y' AND 'N' AND ' '
01801              MOVE -1       TO DABILLL
01802              MOVE AL-UABON TO DABILLA
01803              MOVE ER-2003  TO EMI-ERROR
01804              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01805          ELSE
01806              MOVE AL-UANON TO DABILLA.
01808      IF SSNL NOT = ZERO
01809          IF SSNI NOT = 'Y' AND 'N' AND ' '
01810              MOVE -1       TO SSNL
01811              MOVE AL-UABON TO SSNA
01812              MOVE ER-0607  TO EMI-ERROR
01813              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01814          ELSE
01815              MOVE AL-UANON TO SSNA.
01817      IF AGECALCL NOT = ZERO
01818          IF AGECALCI EQUAL '1' OR '2' OR '3'
01819              MOVE AL-UANON TO AGECALCA
01820          ELSE
01821              MOVE -1       TO AGECALCL
01822              MOVE AL-UABON TO AGECALCA
01823              MOVE ER-7692  TO EMI-ERROR
01824              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01826      IF CONSYSL NOT = ZERO
01827          IF CONSYSI NOT = 'Y' AND 'N' AND ' '
01828              MOVE -1       TO CONSYSL
01829              MOVE AL-UABON TO CONSYSA
01830              MOVE ER-2004  TO EMI-ERROR
01831              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01832          ELSE
01833              MOVE AL-UANON TO CONSYSA.
01835      IF MEMBNOL NOT = ZERO
01836          IF MEMBNOI NOT = 'Y' AND 'N' AND ' '
01837              MOVE -1       TO MEMBNOL
01838              MOVE AL-UABON TO MEMBNOA
01839              MOVE ER-0608  TO EMI-ERROR
01840              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01841          ELSE
01842              MOVE AL-UANON TO MEMBNOA.
01844      IF MAILL NOT = ZERO
01845          IF MAILI = 'Y' OR 'N' OR ' '
01846              MOVE AL-UANON TO MAILA
01847          ELSE
01848              MOVE -1       TO MAILL
01849              MOVE AL-UABON TO MAILA
01850              MOVE ER-0608  TO EMI-ERROR
01851              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01853      IF AMRTCDL NOT = ZERO
01854         IF AMRTCDI NOT = SPACES
01855            PERFORM 0850-CHECK-ALT-MORT THRU 0850-XIT.
01857      IF QTECALL NOT = ZERO
01858          
      * EXEC CICS BIF
01859 *            DEEDIT
01860 *            FIELD (QTECALI)
01861 *            LENGTH(5)
01862 *        END-EXEC
           MOVE 5 TO DFHEIV11
           MOVE '@"L                   #   #00005837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QTECALI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01863          IF QTECALI NOT NUMERIC
01864              MOVE -1               TO QTECALL
01865              MOVE AL-UNBON         TO QTECALA
01866              MOVE ER-2010          TO EMI-ERROR
01867              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01868            ELSE
01869              MOVE QTECALI          TO SV-QTECAL
01870                                       QTECALO
01871              MOVE AL-UNNON         TO QTECALA.
01873      IF OVSAMTL NOT = ZERO
01874          
      * EXEC CICS BIF
01875 *            DEEDIT
01876 *            FIELD (OVSAMTI)
01877 *            LENGTH(5)
01878 *        END-EXEC
           MOVE 5 TO DFHEIV11
           MOVE '@"L                   #   #00005852' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 OVSAMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01879          IF OVSAMTI NUMERIC
01880              MOVE AL-UNNON           TO OVSAMTA
01881              MOVE OVSAMTI            TO SV-OVSAMT
01882                                         OVSAMTO
01883          ELSE
01884              MOVE ER-7533            TO EMI-ERROR
01885              MOVE -1                 TO OVSAMTL
01886              MOVE AL-UNBON           TO OVSAMTA
01887              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01889      IF OVSPCTL NOT = ZERO
01890          
      * EXEC CICS BIF
01891 *            DEEDIT
01892 *            FIELD (OVSPCTI)
01893 *            LENGTH(5)
01894 *        END-EXEC
           MOVE 5 TO DFHEIV11
           MOVE '@"L                   #   #00005867' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 OVSPCTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01895          IF OVSPCTI NUMERIC
01896             MOVE AL-UNNON           TO OVSPCTA
01897             MOVE OVSPCTI            TO WS-CO-OVR-SHT-PCT
01898                                        OVSPCTO
01899          ELSE
01900             MOVE ER-7532            TO EMI-ERROR
01901             MOVE -1                 TO OVSPCTL
01902             MOVE AL-UNBON           TO OVSPCTA
01903             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01905      IF MINAGEL NOT = ZERO
01906          IF MINAGEI NOT NUMERIC
01907             MOVE -1              TO MINAGEL
01908             MOVE AL-UNBON        TO MINAGEA
01909             MOVE ER-2011         TO EMI-ERROR
01910             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01911          ELSE
01912             MOVE AL-UNNON        TO MINAGEA.
01914      IF PREJCTL NOT = ZERO
01915          IF PREJCTI NOT = ' ' AND '1'
01916             MOVE -1              TO PREJCTL
01917             MOVE AL-UABON        TO PREJCTA
01918             MOVE ER-2012         TO EMI-ERROR
01919             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01920          ELSE
01921             MOVE AL-UANON        TO PREJCTA.
01923      IF MAXTERML NOT = ZERO
01924          IF MAXTERMI NOT NUMERIC
01925             MOVE -1 TO MAXTERML
01926             MOVE AL-UABON TO MAXTERMA
01927             MOVE ER-2013 TO EMI-ERROR
01928             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01929          ELSE
01930             MOVE AL-UANON TO MAXTERMA.
01932      IF QTEREFL NOT = ZERO
01933          
      * EXEC CICS BIF
01934 *            DEEDIT
01935 *            FIELD (QTEREFI)
01936 *            LENGTH(5)
01937 *        END-EXEC
           MOVE 5 TO DFHEIV11
           MOVE '@"L                   #   #00005906' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QTEREFI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01938          IF QTEREFI NOT NUMERIC
01939              MOVE -1               TO QTEREFL
01940              MOVE AL-UNBON         TO QTEREFA
01941              MOVE ER-2014          TO EMI-ERROR
01942              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01943            ELSE
01944              MOVE QTEREFI          TO SV-QTEREF
01945                                       QTEREFO
01946              MOVE AL-UNNON         TO QTEREFA.
01948      IF REFREJL NOT = ZERO
01949          IF REFREJI NOT = ' ' AND '1'
01950             MOVE -1              TO REFREJL
01951             MOVE AL-UABON        TO REFREJA
01952             MOVE ER-2028         TO EMI-ERROR
01953             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01954          ELSE
01955             MOVE AL-UANON        TO REFREJA.
01957      IF DAGEL NOT = ZERO
01958          IF DAGEI NOT NUMERIC
01959             MOVE -1       TO DAGEL
01960             MOVE AL-UABON TO DAGEA
01961             MOVE ER-2015  TO EMI-ERROR
01962             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01963          ELSE
01964             MOVE AL-UANON TO DAGEA.
01966      IF DSEXL NOT = ZERO
01967          IF DSEXI NOT = 'M' AND 'F' AND ' '
01968             MOVE -1       TO DSEXL
01969             MOVE AL-UABON TO DSEXA
01970             MOVE ER-2016  TO EMI-ERROR
01971             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01972          ELSE
01973             MOVE AL-UANON TO DSEXA.
01975      IF DAPRL NOT = ZERO
01976          
      * EXEC CICS BIF
01977 *            DEEDIT
01978 *            FIELD (DAPRI)
01979 *            LENGTH(8)
01980 *        END-EXEC
           MOVE 8 TO DFHEIV11
           MOVE '@"L                   #   #00005945' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DAPRI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01981          IF DAPRI NOT NUMERIC
01982              MOVE -1               TO DAPRL
01983              MOVE AL-UNBON         TO DAPRA
01984              MOVE ER-0248          TO EMI-ERROR
01985              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01986           ELSE
01987              MOVE DAPRI            TO SV-DAPR
01988                                       DAPRO
01989              MOVE AL-UNNON TO DAPRA.
01991      IF JAGEL NOT = ZERO
01992          IF JAGEI NOT = 'Y' AND 'N' AND ' '
01993             MOVE -1       TO JAGEL
01994             MOVE AL-UABON TO JAGEA
01995             MOVE ER-2017  TO EMI-ERROR
01996             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01997          ELSE
01998             MOVE AL-UANON TO JAGEA.
02000      IF PRCESSFL NOT = ZERO
02001          IF PRCESSFI NOT = 'Q' AND 'M' AND ' '
02002             MOVE -1              TO PRCESSFL
02003             MOVE AL-UABON        TO PRCESSFA
02004             MOVE ER-2029         TO EMI-ERROR
02005             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02006          ELSE
02007             MOVE AL-UANON        TO PRCESSFA.
02009      IF BINPUTL NOT = ZERO
02010          IF BINPUTI NOT = 'Y' AND 'N'      AND ' '
02011             MOVE -1       TO BINPUTL
02012             MOVE AL-UABON TO BINPUTA
02013             MOVE ER-2018  TO EMI-ERROR
02014             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02015          ELSE
02016             MOVE AL-UANON TO BINPUTA.
02018      IF PRMPCTL NOT = ZEROS
02019         
      * EXEC CICS BIF
02020 *           DEEDIT
02021 *           FIELD (PRMPCTI)
02022 *           LENGTH(5)
02023 *       END-EXEC
           MOVE 5 TO DFHEIV11
           MOVE '@"L                   #   #00005984' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PRMPCTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02024        IF PRMPCTI NUMERIC
02025             MOVE AL-UNNON           TO PRMPCTA
02026             MOVE PRMPCTI            TO WS-TOL-PREM-PCT
02027                                        PRMPCTO
02028         ELSE
02029             MOVE ER-7532            TO EMI-ERROR
02030             MOVE -1                 TO PRMPCTL
02031             MOVE AL-UNBON           TO PRMPCTA
02032             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02034      IF REFPCTL NOT = ZEROS
02035         
      * EXEC CICS BIF
02036 *           DEEDIT
02037 *           FIELD (REFPCTI)
02038 *           LENGTH (5)
02039 *       END-EXEC
           MOVE 5 TO DFHEIV11
           MOVE '@"L                   #   #00005999' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REFPCTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02040        IF REFPCTI NUMERIC
02041             MOVE AL-UNNON           TO REFPCTA
02042             MOVE REFPCTI            TO WS-TOL-REFUND-PCT
02043                                        REFPCTO
02044        ELSE
02045             MOVE ER-7532            TO EMI-ERROR
02046             MOVE -1                 TO REFPCTL
02047             MOVE AL-UNBON           TO REFPCTA
02048             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02050      IF MTOLCAPL NOT = ZERO
02051          
      * EXEC CICS BIF
02052 *            DEEDIT
02053 *            FIELD (MTOLCAPI)
02054 *            LENGTH(5)
02055 *        END-EXEC
           MOVE 5 TO DFHEIV11
           MOVE '@"L                   #   #00006014' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MTOLCAPI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02056         IF MTOLCAPI NOT NUMERIC
02057            MOVE -1               TO MTOLCAPL
02058            MOVE AL-UNBON         TO MTOLCAPA
02059            MOVE ER-7533          TO EMI-ERROR
02060            PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT
02061         ELSE
02062         IF MTOLCAPI GREATER THAN 9999
02063            MOVE -1               TO MTOLCAPL
02064            MOVE AL-UNBON         TO MTOLCAPA
02065            MOVE ER-7533          TO EMI-ERROR
02066            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02067         ELSE
02068            MOVE MTOLCAPI          TO SV-MTOLCAP
02069                                      MTOLCAPO
02070            MOVE AL-UNNON          TO MTOLCAPA.
02072      IF MINPREML NOT = ZERO
02073          
      * EXEC CICS BIF
02074 *            DEEDIT
02075 *            FIELD (MINPREMI)
02076 *            LENGTH(5)
02077 *        END-EXEC
           MOVE 5 TO DFHEIV11
           MOVE '@"L                   #   #00006035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MINPREMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02078         IF MINPREMI NOT NUMERIC
02079            MOVE -1               TO MINPREML
02080            MOVE AL-UNBON         TO MINPREMA
02081            MOVE ER-2020          TO EMI-ERROR
02082            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02083         ELSE
02084            MOVE MINPREMI          TO SV-MINPREM
02085                                      MINPREMO
02086            MOVE AL-UNNON          TO MINPREMA.
02088      IF COMPWTEL NOT = ZERO
02089          
      * EXEC CICS BIF
02090 *            DEEDIT
02091 *            FIELD (COMPWTEI)
02092 *            LENGTH(5)
02093 *        END-EXEC
           MOVE 5 TO DFHEIV11
           MOVE '@"L                   #   #00006050' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMPWTEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02094         IF COMPWTEI NOT NUMERIC
02095            MOVE -1               TO COMPWTEL
02096            MOVE AL-UNBON         TO COMPWTEA
02097            MOVE ER-2023          TO EMI-ERROR
02098            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02099         ELSE
02100            MOVE COMPWTEI         TO SV-COMPWTE
02101                                     COMPWTEO
02102            MOVE AL-UNNON         TO COMPWTEA.
02104      IF CONDTEL NOT = ZERO
02105          MOVE AL-UNNON     TO CONDTEA
02106          MOVE CONDTEI      TO DEEDIT-FIELD
02107          PERFORM 0800-DEEDIT THRU 0800-EXIT
02108          MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY
02109          MOVE '4'          TO DC-OPTION-CODE
02110          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
02111          IF DATE-CONVERSION-ERROR
02112              MOVE -1       TO CONDTEL
02113              MOVE AL-UABON TO CONDTEA
02114              MOVE ER-2022  TO EMI-ERROR
02115              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02116          ELSE
02117              MOVE DC-GREG-DATE-1-EDIT    TO CONDTEI
02118              MOVE DC-BIN-DATE-1          TO WS-SAVE-CONDTE.
02120      IF PI-PROCESSOR-ID = 'LGXX'
02121          IF LSTBTCHL NOT = ZERO
02122              IF LSTBTCHI NOT NUMERIC
02123                  MOVE -1       TO LSTBTCHL
02124                  MOVE AL-UNBON TO LSTBTCHA
02125                  MOVE ER-0615  TO EMI-ERROR
02126                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02127              ELSE
02128                  MOVE AL-UNNON               TO LSTBTCHA.
02130      IF CMETHODL NOT = ZERO
02131          IF CMETHODI = '1' OR '2' OR '4'
02132              MOVE AL-UNNON TO CMETHODA
02133          ELSE
02134              MOVE AL-UNBON TO CMETHODA
02135              MOVE -1       TO CMETHODL
02136              MOVE ER-2357  TO EMI-ERROR
02137              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02139      IF PI-PROCESSOR-ID = 'LGXX'
02140          IF CARGRPOL NOT = ZERO
02141              IF CARGRPOI = ' ' OR '1' OR '2' OR '3'
02142                  MOVE AL-UANON TO CARGRPOA
02143              ELSE
02144                  MOVE AL-UABON TO CARGRPOA
02145                  MOVE -1       TO CARGRPOL
02146                  MOVE ER-0635  TO EMI-ERROR
02147                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02149      IF STCNTLL NOT EQUAL ZEROS
02150          IF STCNTLI EQUAL '1' OR '2' OR '3' OR '4' OR
02151                           '5' OR '6' OR '7' OR '8' OR
02152                           '9' OR 'A' OR 'B' OR 'X'
02153             NEXT SENTENCE
02154          ELSE
02155             MOVE -1         TO STCNTLL
02156             MOVE AL-UABON   TO STCNTLA
02157             MOVE ER-3035    TO EMI-ERROR
02158             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02160      IF PI-PROCESSOR-ID = 'LGXX'
02161          IF ARCHSTL NOT = ZERO
02162              IF ARCHSTI NOT NUMERIC
02163                  MOVE -1         TO ARCHSTL
02164                  MOVE AL-UNBON   TO ARCHSTA
02165                  MOVE ER-0611    TO EMI-ERROR
02166                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02167              ELSE
02168                  MOVE AL-UNNON   TO ARCHSTA.
02170      IF PI-PROCESSOR-ID = 'LGXX'
02171          IF ARCHLSTL NOT = ZERO
02172              IF ARCHLSTI NOT NUMERIC
02173                  MOVE -1         TO ARCHLSTL
02174                  MOVE AL-UNBON   TO ARCHLSTA
02175                  MOVE ER-0611    TO EMI-ERROR
02176                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02177              ELSE
02178                  MOVE AL-UNNON   TO ARCHLSTA.
02180      IF ARCHPYRL NOT = ZERO
02181          IF ARCHPYRI NOT NUMERIC
02182              MOVE -1             TO ARCHPYRL
02183              MOVE AL-UNBON       TO ARCHPYRA
02184              MOVE ER-7356        TO EMI-ERROR
02185              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02186          ELSE
02187              MOVE AL-UNNON       TO ARCHPYRA.
02189      IF PI-PROCESSOR-ID = 'LGXX'
02190          IF CKCOUNTL NOT = ZERO
02191              IF CKCOUNTI NOT NUMERIC
02192                  MOVE -1       TO CKCOUNTL
02193                  MOVE AL-UNBON TO CKCOUNTA
02194                  MOVE ER-0605  TO EMI-ERROR
02195                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02196              ELSE
02197                  MOVE AL-UNNON               TO CKCOUNTA.
02199      IF PI-PROCESSOR-ID = 'LGXX'
02200          IF QUCOUNTL NOT = ZERO
02201              IF QUCOUNTI NOT NUMERIC
02202                  MOVE -1       TO QUCOUNTL
02203                  MOVE AL-UNBON TO QUCOUNTA
02204                  MOVE ER-0606  TO EMI-ERROR
02205                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02206              ELSE
02207                  MOVE AL-UNNON               TO QUCOUNTA.
02209      IF CRLABELL NOT = ZERO
02210          IF CRLABELI NOT = 'Y' AND 'N'
02211              MOVE -1             TO CRLABELL
02212              MOVE AL-UABON       TO CRLABELA
02213              MOVE ER-7380        TO EMI-ERROR
02214              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02215          ELSE
02216              MOVE AL-UANON       TO CRLABELA.
02218  0630-MAPC.
02219      IF EMI-NO-ERRORS
02220          GO TO 3000-UPDATE-FILE.
02222      GO TO 8220-SEND-DATAONLY.
02224  0630-EXIT.
02225      EXIT.
02226      EJECT
02227  0730-EDIT-DATA.
02229      IF MIBVERL NOT = ZERO
02230          IF MIBVERI = '0'
02231              IF MIBCSWI EQUAL 'Y' OR
02232                 MIBTSWI EQUAL 'Y'
02233                  MOVE AL-UABON TO MIBCSWA
02234                  MOVE -1       TO MIBCSWL
02235                  MOVE ER-9460  TO EMI-ERROR
02236                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02238      IF MIBVERL NOT = ZERO
02239          IF MIBVERI = '0' OR '1' OR '2' OR '3'
02240              MOVE AL-UANON TO MIBVERA
02241          ELSE
02242              MOVE AL-UABON TO MIBVERA
02243              MOVE -1       TO MIBVERL
02244              MOVE ER-9193  TO EMI-ERROR
02245              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02247      IF DAMRTCDL NOT = ZERO
02248         IF DAMRTCDI NOT = SPACES
02249            PERFORM 0950-CHECK-ALT-MORT THRU 0950-XIT.
02251      IF MIBCSWL NOT = ZERO AND
02252         MIBTSWL NOT = ZERO
02253          IF MIBCSWI EQUAL 'Y' AND
02254             MIBTSWI EQUAL 'Y'
02255              MOVE AL-UABON TO MIBCSWA
02256              MOVE -1       TO MIBCSWL
02257              MOVE ER-9322  TO EMI-ERROR
02258              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02260      IF MIBCSWL NOT = ZERO
02261          IF MIBCSWI EQUAL 'N' AND
02262             MIBTSWL = ZERO
02263              MOVE AL-UABON TO MIBCSWA
02264              MOVE -1       TO MIBCSWL
02265              MOVE ER-9322  TO EMI-ERROR
02266              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02268      IF MIBTSWL NOT = ZERO
02269          IF MIBTSWI EQUAL 'N' AND
02270             MIBCSWL = ZERO
02271              MOVE AL-UABON TO MIBCSWA
02272              MOVE -1       TO MIBCSWL
02273              MOVE ER-9322  TO EMI-ERROR
02274              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02276      IF DRESURL NOT = ZERO
02277          IF DRESURI NOT = 'Y' AND 'N' AND ' '
02278             MOVE -1       TO DRESURL
02279             MOVE AL-UABON TO DRESURA
02280             MOVE ER-2019  TO EMI-ERROR
02281             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02282          ELSE
02283             MOVE AL-UANON TO DRESURA.
02285      IF RATESWL NOT = ZERO
02286          IF RATESWI = ' ' OR 'N' OR 'Y'
02287              NEXT SENTENCE
02288          ELSE
02289              MOVE AL-UABON TO RATESWA
02290              MOVE -1       TO RATESWL
02291              MOVE ER-9446  TO EMI-ERROR
02292              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02294      IF PI-PROCESSOR-ID = 'LGXX'
02295          IF CMPCNTLL NOT = ZERO
02296              IF CMPCNTLI = ' ' OR '1' OR '2' OR '3'
02297                  MOVE AL-UANON TO CMPCNTLA
02298              ELSE
02299                  MOVE AL-UABON TO CMPCNTLA
02300                  MOVE -1       TO CMPCNTLL
02301                  MOVE ER-0635  TO EMI-ERROR
02302                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02305      IF CNVDTEL NOT = ZERO
02306          MOVE AL-UNNON     TO CNVDTEA
02307          MOVE CNVDTEI      TO DEEDIT-FIELD
02308          PERFORM 0800-DEEDIT THRU 0800-EXIT
02309          MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY
02310          MOVE '4'          TO DC-OPTION-CODE
02311          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
02312          IF DATE-CONVERSION-ERROR
02313              MOVE -1       TO CNVDTEL
02314              MOVE AL-UABON TO CNVDTEA
02315              MOVE ER-2022  TO EMI-ERROR
02316              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02317          ELSE
02318              MOVE DC-GREG-DATE-1-EDIT    TO CNVDTEI
02319              MOVE DC-BIN-DATE-1          TO WS-SAVE-CONDTE.
02321      IF PI-PROCESSOR-ID = 'LGXX'
02322          IF AUTOREFL NOT = ZERO
02323              IF AUTOREFI EQUAL 'Y' OR 'N'
02324                  MOVE AL-UANON   TO AUTOREFA
02325              ELSE
02326                  MOVE -1         TO AUTOREFL
02327                  MOVE AL-UABON   TO AUTOREFA
02328                  MOVE ER-9909    TO EMI-ERROR
02329                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02331      IF PI-PROCESSOR-ID = 'LGXX'
02332          IF MGCNTLL NOT = ZERO
02333              IF MGCNTLI NOT = ' ' AND '1' AND '2' AND '3'
02334                 MOVE -1       TO MGCNTLL
02335                 MOVE AL-UABON TO MGCNTLA
02336                 MOVE ER-9192  TO EMI-ERROR
02337                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02338              ELSE
02339                 MOVE AL-UANON TO MGCNTLA.
02341      IF PI-PROCESSOR-ID = 'LGXX'
02342          IF SHFTLNOL NOT = ZERO
02343              IF SHFTLNOI EQUAL 'L' OR 'R'
02344                  MOVE AL-UANON   TO SHFTLNOA
02345              ELSE
02346                  MOVE -1         TO SHFTLNOL
02347                  MOVE AL-UABON   TO SHFTLNOA
02348                  MOVE ER-9791    TO EMI-ERROR
02349                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02351      IF PI-PROCESSOR-ID = 'LGXX'
02352          IF CHKPMTHL NOT = ZERO
02353              IF CHKPMTHI EQUAL '1' OR '2' OR '3'
02354                  MOVE AL-UANON   TO CHKPMTHA
02355              ELSE
02356                  MOVE -1         TO CHKPMTHL
02357                  MOVE AL-UABON   TO CHKPMTHA
02358                  MOVE ER-9791    TO EMI-ERROR
02359                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02361      IF PI-PROCESSOR-ID = 'LGXX'
02362          IF MRECONL NOT = ZERO
02363              IF MRECONI EQUAL 'Y' OR 'N'
02364                  MOVE AL-UANON   TO MRECONA
02365              ELSE
02366                  MOVE -1         TO MRECONL
02367                  MOVE AL-UABON   TO MRECONA
02368                  MOVE ER-9533    TO EMI-ERROR
02369                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02371      IF RPTLANGL IS GREATER THAN +0
02372          IF RPTLANGI NOT = ' ' AND 'E' AND 'F'
02373              MOVE ER-9598        TO EMI-ERROR
02374              MOVE -1             TO RPTLANGL
02375              MOVE AL-UABON       TO RPTLANGA
02376              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02377          ELSE
02378              MOVE AL-UANON       TO  RPTLANGA.
02380      IF PLCYLNKL IS GREATER THAN +0
02381          IF PLCYLNKI NOT = ' ' AND 'Y' AND 'N'
02382              MOVE ER-9576        TO EMI-ERROR
02383              MOVE -1             TO PLCYLNKL
02384              MOVE AL-UABON       TO PLCYLNKA
02385              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02386          ELSE
02387              MOVE AL-UANON       TO  PLCYLNKA.
02389      IF GRPSWL  NOT = ZERO
02390          IF GRPSWI = ' ' OR 'N' OR 'Y'
02391              NEXT SENTENCE
02392          ELSE
02393              MOVE AL-UABON TO GRPSWA
02394              MOVE -1       TO GRPSWL
02395              MOVE ER-9417  TO EMI-ERROR
02396              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02398      IF PI-PROCESSOR-ID = 'LGXX'
02399          IF REFNOL NOT = ZERO
02400              MOVE REFNOI         TO DEEDIT-FIELD
02401              PERFORM 0800-DEEDIT THRU 0800-EXIT
02402              IF DEEDIT-FIELD-V0 NOT NUMERIC
02403                  MOVE -1         TO REFNOL
02404                  MOVE AL-UNBON   TO REFNOA
02405                  MOVE ER-9790    TO EMI-ERROR
02406                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02407              ELSE
02408                  MOVE DEEDIT-FIELD
02409                                  TO SV-MORTG-REF-NUM
02410                  MOVE AL-UNNON   TO REFNOA.
02412      IF LNMESERL NOT = ZERO
02413          MOVE LNMESERI TO WS-LNAME-SEARCH-CNTL
02414          IF WS-MIB-LNAME-SEARCH = ' ' OR 'N' OR 'Y'
02415              MOVE AL-UANON TO LNMESERA
02416          ELSE
02417              MOVE AL-UABON TO LNMESERA
02418              MOVE -1       TO LNMESERL
02419              MOVE ER-9194  TO EMI-ERROR
02420              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02422      IF LNMESERL NOT = ZERO
02423          IF WS-ALP-LNAME-SEARCH = ' ' OR 'N' OR 'Y'
02424              MOVE AL-UANON TO LNMESERA
02425          ELSE
02426              MOVE AL-UABON TO LNMESERA
02427              MOVE -1       TO LNMESERL
02428              MOVE ER-9194  TO EMI-ERROR
02429              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02431      IF FNMESERL NOT = ZERO
02432          MOVE FNMESERI TO WS-FNAME-SEARCH-CNTL
02433          IF WS-MIB-FNAME-SEARCH = ' ' OR 'N' OR 'Y'
02434              MOVE AL-UANON TO FNMESERA
02435          ELSE
02436              MOVE AL-UABON TO FNMESERA
02437              MOVE -1       TO FNMESERL
02438              MOVE ER-9194  TO EMI-ERROR
02439              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02441      IF FNMESERL NOT = ZERO
02442          IF WS-ALP-FNAME-SEARCH = ' ' OR 'N' OR 'Y'
02443              MOVE AL-UANON TO FNMESERA
02444          ELSE
02445              MOVE AL-UABON TO FNMESERA
02446              MOVE -1       TO FNMESERL
02447              MOVE ER-9194  TO EMI-ERROR
02448              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02450      IF MNMESERL NOT = ZERO
02451          MOVE MNMESERI TO WS-MNAME-SEARCH-CNTL
02452          IF WS-MIB-MNAME-SEARCH = ' ' OR 'N' OR 'Y'
02453              MOVE AL-UANON TO MNMESERA
02454          ELSE
02455              MOVE AL-UABON TO MNMESERA
02456              MOVE -1       TO MNMESERL
02457              MOVE ER-9194  TO EMI-ERROR
02458              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02460      IF MNMESERL NOT = ZERO
02461          IF WS-ALP-MNAME-SEARCH = ' ' OR 'N' OR 'Y'
02462              MOVE AL-UANON TO MNMESERA
02463          ELSE
02464              MOVE AL-UABON TO MNMESERA
02465              MOVE -1       TO MNMESERL
02466              MOVE ER-9194  TO EMI-ERROR
02467              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02469      IF BDATEL NOT = ZERO
02470          MOVE BDATEI TO WS-BDATE-SEARCH-CNTL
02471          IF WS-MIB-BDATE-SEARCH = ' ' OR 'N' OR 'Y'
02472              MOVE AL-UANON TO BDATEA
02473          ELSE
02474              MOVE AL-UABON TO BDATEA
02475              MOVE -1       TO BDATEL
02476              MOVE ER-9194  TO EMI-ERROR
02477              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02479      IF BDATEL NOT = ZERO
02480          IF WS-ALP-BDATE-SEARCH = ' ' OR 'N' OR 'Y'
02481              MOVE AL-UANON TO BDATEA
02482          ELSE
02483              MOVE AL-UABON TO BDATEA
02484              MOVE -1       TO BDATEL
02485              MOVE ER-9194  TO EMI-ERROR
02486              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02488      IF BSTATEL NOT = ZERO
02489          MOVE BSTATEI TO WS-BSTATE-SEARCH-CNTL
02490          IF WS-MIB-BSTATE-SEARCH = ' ' OR 'N' OR 'Y'
02491              MOVE AL-UANON TO BSTATEA
02492          ELSE
02493              MOVE AL-UABON TO BSTATEA
02494              MOVE -1       TO BSTATEL
02495              MOVE ER-9194  TO EMI-ERROR
02496              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02498      IF BSTATEL NOT = ZERO
02499          IF WS-ALP-BSTATE-SEARCH = ' ' OR 'N' OR 'Y'
02500              MOVE AL-UANON TO BSTATEA
02501          ELSE
02502              MOVE AL-UABON TO BSTATEA
02503              MOVE -1       TO BSTATEL
02504              MOVE ER-9194  TO EMI-ERROR
02505              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02507      IF RSTATEL NOT = ZERO
02508          MOVE RSTATEI TO WS-RSTATE-SEARCH-CNTL
02509          IF WS-MIB-RSTATE-SEARCH = ' ' OR 'N' OR 'Y'
02510              MOVE AL-UANON TO RSTATEA
02511          ELSE
02512              MOVE AL-UABON TO RSTATEA
02513              MOVE -1       TO RSTATEL
02514              MOVE ER-9194  TO EMI-ERROR
02515              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02517      IF RSTATEL NOT = ZERO
02518          IF WS-ALP-RSTATE-SEARCH = ' ' OR 'N' OR 'Y'
02519              MOVE AL-UANON TO RSTATEA
02520          ELSE
02521              MOVE AL-UABON TO RSTATEA
02522              MOVE -1       TO RSTATEL
02523              MOVE ER-9194  TO EMI-ERROR
02524              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02526      IF PI-PROCESSOR-ID = 'LGXX'
02527          IF RATFILML NOT = ZERO
02528              IF RATFILMI EQUAL SPACES
02529                  MOVE LOW-VALUES   TO WS-SAVE-RTEFILM
02530              ELSE
02531                  MOVE AL-UNNON     TO RATFILMA
02532                  MOVE RATFILMI     TO DEEDIT-FIELD
02533                  PERFORM 0800-DEEDIT THRU 0800-EXIT
02534                  MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY
02535                  MOVE '4'          TO DC-OPTION-CODE
02536                  PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
02537                  IF DATE-CONVERSION-ERROR
02538                      MOVE -1       TO RATFILML
02539                      MOVE AL-UABON TO RATFILMA
02540                      MOVE ER-0600  TO EMI-ERROR
02541                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02542                  ELSE
02543                      MOVE DC-GREG-DATE-1-EDIT TO RATFILMI
02544                      MOVE DC-BIN-DATE-1       TO WS-SAVE-RTEFILM.
02546      IF RATFILCL NOT = ZERO
02547          MOVE AL-UNNON     TO RATFILCA
02548          MOVE RATFILCI     TO DEEDIT-FIELD
02549          PERFORM 0800-DEEDIT THRU 0800-EXIT
02550          MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY
02551          MOVE '4'          TO DC-OPTION-CODE
02552          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
02553          IF DATE-CONVERSION-ERROR
02554              MOVE -1       TO RATFILCL
02555              MOVE AL-UABON TO RATFILCA
02556              MOVE ER-2034  TO EMI-ERROR
02557              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02558          ELSE
02559              MOVE DC-GREG-DATE-1-EDIT    TO RATFILCI
02560              MOVE DC-BIN-DATE-1          TO WS-SAVE-RTEFILC.
02562      IF PI-PROCESSOR-ID = 'LGXX'
02563          IF CHKCNTRL NOT = ZERO
02564              IF CHKCNTRI NOT NUMERIC
02565                  MOVE -1       TO CHKCNTRL
02566                  MOVE AL-UNBON TO CHKCNTRA
02567                  MOVE ER-0605  TO EMI-ERROR
02568                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02569              ELSE
02570                  MOVE AL-UNNON               TO CHKCNTRA.
02572      IF PI-PROCESSOR-ID = 'LGXX'
02573          IF PRDMSTML NOT = ZERO
02574              IF PRDMSTMI EQUAL SPACES
02575                  MOVE LOW-VALUES   TO WS-SAVE-PRDMSTM
02576              ELSE
02577                  MOVE AL-UNNON     TO PRDMSTMA
02578                  MOVE PRDMSTMI     TO DEEDIT-FIELD
02579                  PERFORM 0800-DEEDIT THRU 0800-EXIT
02580                  MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY
02581                  MOVE '4'          TO DC-OPTION-CODE
02582                  PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
02583                  IF DATE-CONVERSION-ERROR
02584                      MOVE -1       TO PRDMSTML
02585                      MOVE AL-UABON TO PRDMSTMA
02586                      MOVE ER-0602  TO EMI-ERROR
02587                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02588                  ELSE
02589                      MOVE DC-GREG-DATE-1-EDIT TO PRDMSTMI
02590                      MOVE DC-BIN-DATE-1       TO WS-SAVE-PRDMSTM.
02592      IF PRDMSTCL NOT = ZERO
02593          MOVE AL-UNNON     TO PRDMSTCA
02594          MOVE PRDMSTCI     TO DEEDIT-FIELD
02595          PERFORM 0800-DEEDIT THRU 0800-EXIT
02596          MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY
02597          MOVE '4'          TO DC-OPTION-CODE
02598          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
02599          IF DATE-CONVERSION-ERROR
02600              MOVE -1       TO PRDMSTCL
02601              MOVE AL-UABON TO PRDMSTCA
02602              MOVE ER-2036  TO EMI-ERROR
02603              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02604          ELSE
02605              MOVE DC-GREG-DATE-1-EDIT    TO PRDMSTCI
02606              MOVE DC-BIN-DATE-1          TO WS-SAVE-PRDMSTC.
02608      IF PI-PROCESSOR-ID = 'LGXX'
02609          IF QUECNTRL NOT = ZERO
02610              IF QUECNTRI NOT NUMERIC
02611                  MOVE -1       TO QUECNTRL
02612                  MOVE AL-UNBON TO QUECNTRA
02613                  MOVE ER-0606  TO EMI-ERROR
02614                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02615              ELSE
02616                  MOVE AL-UNNON               TO QUECNTRA.
02618      IF PI-PROCESSOR-ID = 'LGXX'
02619          IF RENTBLML NOT = ZERO
02620              IF RENTBLMI EQUAL SPACES
02621                  MOVE LOW-VALUES TO WS-SAVE-RENTABM
02622              ELSE
02623                  MOVE AL-UNNON TO RENTBLMA
02624                  MOVE RENTBLMI TO DEEDIT-FIELD
02625                  PERFORM 0800-DEEDIT THRU 0800-EXIT
02626                  MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY
02627                  MOVE '4'          TO DC-OPTION-CODE
02628                  PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
02629                  IF DATE-CONVERSION-ERROR
02630                      MOVE -1       TO RENTBLML
02631                      MOVE AL-UABON TO RENTBLMA
02632                      MOVE ER-0603  TO EMI-ERROR
02633                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02634                  ELSE
02635                      MOVE DC-GREG-DATE-1-EDIT TO RENTBLMI
02636                      MOVE DC-BIN-DATE-1       TO WS-SAVE-RENTABM.
02638      IF RENTBLCL NOT = ZERO
02639          MOVE AL-UNNON     TO RENTBLCA
02640          MOVE RENTBLCI     TO DEEDIT-FIELD
02641          PERFORM 0800-DEEDIT THRU 0800-EXIT
02642          MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY
02643          MOVE '4'          TO DC-OPTION-CODE
02644          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
02645          IF DATE-CONVERSION-ERROR
02646              MOVE -1       TO RENTBLCL
02647              MOVE AL-UABON TO RENTBLCA
02648              MOVE ER-2037  TO EMI-ERROR
02649              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02650          ELSE
02651              MOVE DC-GREG-DATE-1-EDIT    TO RENTBLCI
02652              MOVE DC-BIN-DATE-1          TO WS-SAVE-RENTABC.
02654      IF PI-PROCESSOR-ID = 'LGXX'
02655          IF COMMSTML NOT = ZERO
02656              IF COMMSTMI EQUAL SPACES
02657                  MOVE LOW-VALUES   TO WS-SAVE-CMPMSTM
02658              ELSE
02659                  MOVE AL-UNNON     TO COMMSTMA
02660                  MOVE COMMSTMI     TO DEEDIT-FIELD
02661                  PERFORM 0800-DEEDIT THRU 0800-EXIT
02662                  MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY
02663                  MOVE '4'          TO DC-OPTION-CODE
02664                  PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
02665                  IF DATE-CONVERSION-ERROR
02666                      MOVE -1       TO COMMSTML
02667                      MOVE AL-UABON TO COMMSTMA
02668                      MOVE ER-0604  TO EMI-ERROR
02669                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02670                  ELSE
02671                      MOVE DC-GREG-DATE-1-EDIT TO COMMSTMI
02672                      MOVE DC-BIN-DATE-1       TO WS-SAVE-CMPMSTM.
02674      IF COMMSTCL NOT = ZERO
02675          MOVE AL-UNNON     TO COMMSTCA
02676          MOVE COMMSTCI     TO DEEDIT-FIELD
02677          PERFORM 0800-DEEDIT THRU 0800-EXIT
02678          MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY
02679          MOVE '4'          TO DC-OPTION-CODE
02680          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
02681          IF DATE-CONVERSION-ERROR
02682              MOVE -1       TO COMMSTCL
02683              MOVE AL-UABON TO COMMSTCA
02684              MOVE ER-2037  TO EMI-ERROR
02685              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02686          ELSE
02687              MOVE DC-GREG-DATE-1-EDIT    TO COMMSTCI
02688              MOVE DC-BIN-DATE-1          TO WS-SAVE-CMPMSTC.
02690  0730-MAPD.
02691      IF EMI-NO-ERRORS
02692          GO TO 4000-UPDATE-FILE.
02694      GO TO 8230-SEND-DATAONLY.
02695  0730-EXIT.
02696      EXIT.
02698      EJECT
02699  0760-EDIT-DATA.
02700      IF PI-PROCESSOR-ID = 'LGXX' or 'PEMA'
02701          IF RTEFILML NOT = ZERO
02702              IF RTEFILMI EQUAL SPACES
02703                  MOVE LOW-VALUES   TO WS-SAVE-RTEFILM
02704              ELSE
02705                  MOVE AL-UNNON     TO RTEFILMA
02706                  MOVE RTEFILMI     TO DEEDIT-FIELD
02707                  PERFORM 0800-DEEDIT THRU 0800-EXIT
02708                  MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY
02709                  MOVE '4'          TO DC-OPTION-CODE
02710                  PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
02711                  IF DATE-CONVERSION-ERROR
02712                      MOVE -1       TO RTEFILML
02713                      MOVE AL-UABON TO RTEFILMA
02714                      MOVE ER-0600  TO EMI-ERROR
02715                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02716                  ELSE
02717                      MOVE DC-GREG-DATE-1-EDIT TO RTEFILMI
02718                      MOVE DC-BIN-DATE-1       TO WS-SAVE-RTEFILM.
02720      IF RTEFILCL NOT = ZERO
02721          MOVE AL-UNNON     TO RTEFILCA
02722          MOVE RTEFILCI     TO DEEDIT-FIELD
02723          PERFORM 0800-DEEDIT THRU 0800-EXIT
02724          MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY
02725          MOVE '4'          TO DC-OPTION-CODE
02726          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
02727          IF DATE-CONVERSION-ERROR
02728              MOVE -1       TO RTEFILCL
02729              MOVE AL-UABON TO RTEFILCA
02730              MOVE ER-2034  TO EMI-ERROR
02731              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02732          ELSE
02733              MOVE DC-GREG-DATE-1-EDIT    TO RTEFILCI
02734              MOVE DC-BIN-DATE-1          TO WS-SAVE-RTEFILC.
02736      IF PI-PROCESSOR-ID = 'LGXX' or 'PEMA'
02737          IF COMTABML NOT = ZERO
02738              IF COMTABMI EQUAL SPACES
02739                  MOVE LOW-VALUES   TO WS-SAVE-COMTABM
02740              ELSE
02741                  MOVE AL-UNNON     TO COMTABMA
02742                  MOVE COMTABMI     TO DEEDIT-FIELD
02743                  PERFORM 0800-DEEDIT THRU 0800-EXIT
02744                  MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY
02745                  MOVE '4'          TO DC-OPTION-CODE
02746                  PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
02747                  IF DATE-CONVERSION-ERROR
02748                      MOVE -1       TO COMTABML
02749                      MOVE AL-UABON TO COMTABMA
02750                      MOVE ER-0601  TO EMI-ERROR
02751                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02752                  ELSE
02753                      MOVE DC-GREG-DATE-1-EDIT TO COMTABMI
02754                      MOVE DC-BIN-DATE-1      TO WS-SAVE-COMTABM.
02756      IF COMTABCL NOT = ZERO
02757          MOVE AL-UNNON     TO COMTABCA
02758          MOVE COMTABCI     TO DEEDIT-FIELD
02759          PERFORM 0800-DEEDIT THRU 0800-EXIT
02760          MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY
02761          MOVE '4'          TO DC-OPTION-CODE
02762          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
02763          IF DATE-CONVERSION-ERROR
02764              MOVE -1       TO COMTABCL
02765              MOVE AL-UABON TO COMTABCA
02766              MOVE ER-2035  TO EMI-ERROR
02767              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02768          ELSE
02769              MOVE DC-GREG-DATE-1-EDIT    TO COMTABCI
02770              MOVE DC-BIN-DATE-1          TO WS-SAVE-COMTABC.
02772      IF PI-PROCESSOR-ID = 'LGXX' OR 'PEMA'
02773          IF ACTMSTML NOT = ZERO
02774              IF ACTMSTMI EQUAL SPACES
02775                  MOVE LOW-VALUES   TO WS-SAVE-ACTMSTM
02776              ELSE
02777                  MOVE AL-UNNON     TO ACTMSTMA
02778                  MOVE ACTMSTMI     TO DEEDIT-FIELD
02779                  PERFORM 0800-DEEDIT THRU 0800-EXIT
02780                  MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY
02781                  MOVE '4'          TO DC-OPTION-CODE
02782                  PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
02783                  IF DATE-CONVERSION-ERROR
02784                      MOVE -1       TO ACTMSTML
02785                      MOVE AL-UABON TO ACTMSTMA
02786                      MOVE ER-0602  TO EMI-ERROR
02787                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02788                  ELSE
02789                      MOVE DC-GREG-DATE-1-EDIT TO ACTMSTMI
02790                      MOVE DC-BIN-DATE-1       TO WS-SAVE-ACTMSTM.
02792      IF ACTMSTCL NOT = ZERO
02793          MOVE AL-UNNON     TO ACTMSTCA
02794          MOVE ACTMSTCI     TO DEEDIT-FIELD
02795          PERFORM 0800-DEEDIT THRU 0800-EXIT
02796          MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY
02797          MOVE '4'          TO DC-OPTION-CODE
02798          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
02799          IF DATE-CONVERSION-ERROR
02800              MOVE -1       TO ACTMSTCL
02801              MOVE AL-UABON TO ACTMSTCA
02802              MOVE ER-2036  TO EMI-ERROR
02803              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02804          ELSE
02805              MOVE DC-GREG-DATE-1-EDIT    TO ACTMSTCI
02806              MOVE DC-BIN-DATE-1          TO WS-SAVE-ACTMSTC.
02808      IF PI-PROCESSOR-ID = 'LGXX' or 'PEMA'
02809          IF RENTABML NOT = ZERO
02810              IF RENTABMI EQUAL SPACES
02811                  MOVE LOW-VALUES TO WS-SAVE-RENTABM
02812              ELSE
02813                  MOVE AL-UNNON TO RENTABMA
02814                  MOVE RENTABMI TO DEEDIT-FIELD
02815                  PERFORM 0800-DEEDIT THRU 0800-EXIT
02816                  MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY
02817                  MOVE '4'          TO DC-OPTION-CODE
02818                  PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
02819                  IF DATE-CONVERSION-ERROR
02820                      MOVE -1       TO RENTABML
02821                      MOVE AL-UABON TO RENTABMA
02822                      MOVE ER-0603  TO EMI-ERROR
02823                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02824                  ELSE
02825                      MOVE DC-GREG-DATE-1-EDIT TO RENTABMI
02826                      MOVE DC-BIN-DATE-1       TO WS-SAVE-RENTABM.
02828      IF RENTABCL NOT = ZERO
02829          MOVE AL-UNNON     TO RENTABCA
02830          MOVE RENTABCI     TO DEEDIT-FIELD
02831          PERFORM 0800-DEEDIT THRU 0800-EXIT
02832          MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY
02833          MOVE '4'          TO DC-OPTION-CODE
02834          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
02835          IF DATE-CONVERSION-ERROR
02836              MOVE -1       TO RENTABCL
02837              MOVE AL-UABON TO RENTABCA
02838              MOVE ER-2037  TO EMI-ERROR
02839              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02840          ELSE
02841              MOVE DC-GREG-DATE-1-EDIT    TO RENTABCI
02842              MOVE DC-BIN-DATE-1          TO WS-SAVE-RENTABC.
02844      IF PI-PROCESSOR-ID = 'LGXX' or 'PEMA'
02845          IF CMPMSTML NOT = ZERO
02846              IF CMPMSTMI EQUAL SPACES
02847                  MOVE LOW-VALUES   TO WS-SAVE-CMPMSTM
02848              ELSE
02849                  MOVE AL-UNNON     TO CMPMSTMA
02850                  MOVE CMPMSTMI     TO DEEDIT-FIELD
02851                  PERFORM 0800-DEEDIT THRU 0800-EXIT
02852                  MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY
02853                  MOVE '4'          TO DC-OPTION-CODE
02854                  PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
02855                  IF DATE-CONVERSION-ERROR
02856                      MOVE -1       TO CMPMSTML
02857                      MOVE AL-UABON TO CMPMSTMA
02858                      MOVE ER-0604  TO EMI-ERROR
02859                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02860                  ELSE
02861                      MOVE DC-GREG-DATE-1-EDIT TO CMPMSTMI
02862                      MOVE DC-BIN-DATE-1       TO WS-SAVE-CMPMSTM.
02864      IF CMPMSTCL NOT = ZERO
02865          MOVE AL-UNNON     TO CMPMSTCA
02866          MOVE CMPMSTCI     TO DEEDIT-FIELD
02867          PERFORM 0800-DEEDIT THRU 0800-EXIT
02868          MOVE DEEDIT-FIELD TO DC-GREG-DATE-1-MDY
02869          MOVE '4'          TO DC-OPTION-CODE
02870          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
02871          IF DATE-CONVERSION-ERROR
02872              MOVE -1       TO CMPMSTCL
02873              MOVE AL-UABON TO CMPMSTCA
02874              MOVE ER-2037  TO EMI-ERROR
02875              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02876          ELSE
02877              MOVE DC-GREG-DATE-1-EDIT    TO CMPMSTCI
02878              MOVE DC-BIN-DATE-1          TO WS-SAVE-CMPMSTC.
02880  0760-MAPE.
02881      IF EMI-NO-ERRORS
02882          GO TO 5000-UPDATE-FILE.
02884      GO TO 8240-SEND-DATAONLY.
02886  0760-EXIT.
02887      EXIT.
02889      EJECT
02890  0800-DEEDIT.
02891      
      * EXEC CICS BIF
02892 *         DEEDIT
02893 *         FIELD (DEEDIT-FIELD)
02894 *         LENGTH(17)
02895 *     END-EXEC.
           MOVE 17 TO DFHEIV11
           MOVE '@"L                   #   #00006780' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02897  0800-EXIT.
02898      EXIT.
02899      EJECT
02901  0850-CHECK-ALT-MORT.
02902      
      * EXEC CICS HANDLE CONDITION
02903 *            NOTFND(0850-NOT-FOUND)
02904 *    END-EXEC.
           MOVE '"$I                   ! % #00006789' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02906  0850-READ-MORT.
02907      MOVE PI-COMPANY-ID TO CK-COMP-ID.
02908      MOVE '7'           TO CK-REC-TYPE.
02910      
      * EXEC CICS READ
02911 *        DATASET('ELCNTL')
02912 *        SET    (ADDRESS OF CONTROL-FILE)
02913 *        RIDFLD (ELCNTL-KEY)
02914 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&"S        E          (   #00006795' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02916      SET CF-MORT-NDX             TO +1.
02917      SEARCH CF-MORT-TABLE-LINE
02918          VARYING CF-MORT-NDX
02919          AT END
02920              ADD +1              TO CK-SEQ-NO
02921              GO TO 0850-READ-MORT
02922          WHEN
02923              CF-MORT-TABLE-CODE (CF-MORT-NDX) GREATER THAN AMRTCDI
02924                  OR
02925              CF-MORT-TABLE-CODE (CF-MORT-NDX) = LOW-VALUES
02926              GO TO 0850-NOT-FOUND
02927          WHEN
02928              CF-MORT-TABLE-CODE (CF-MORT-NDX) EQUAL AMRTCDI
02929              MOVE +0       TO CK-SEQ-NO
02930              MOVE AL-UNNON TO AMRTCDA
02931              GO TO 0850-XIT.
02933  0850-NOT-FOUND.
02934      MOVE +0       TO CK-SEQ-NO.
02935      MOVE -1       TO AMRTCDL.
02936      MOVE AL-UNBON TO AMRTCDA.
02937      MOVE ER-2904  TO EMI-ERROR.
02938      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02940  0850-XIT.
02941      EXIT.
02942      EJECT
02944  0950-CHECK-ALT-MORT.
02945      
      * EXEC CICS HANDLE CONDITION
02946 *            NOTFND(0950-NOT-FOUND)
02947 *    END-EXEC.
           MOVE '"$I                   ! & #00006826' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02949  0950-READ-MORT.
02950      MOVE PI-COMPANY-ID TO CK-COMP-ID.
02951      MOVE '7'           TO CK-REC-TYPE.
02953      
      * EXEC CICS READ
02954 *        DATASET('ELCNTL')
02955 *        SET    (ADDRESS OF CONTROL-FILE)
02956 *        RIDFLD (ELCNTL-KEY)
02957 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&"S        E          (   #00006832' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02959      SET CF-MORT-NDX             TO +1.
02960      SEARCH CF-MORT-TABLE-LINE
02961          VARYING CF-MORT-NDX
02962          AT END
02963              ADD +1              TO CK-SEQ-NO
02964              GO TO 0950-READ-MORT
02965          WHEN
02966              CF-MORT-TABLE-CODE (CF-MORT-NDX)
02967                  GREATER THAN DAMRTCDI
02968                  OR
02969              CF-MORT-TABLE-CODE (CF-MORT-NDX) = LOW-VALUES
02970              GO TO 0950-NOT-FOUND
02971          WHEN
02972              CF-MORT-TABLE-CODE (CF-MORT-NDX) EQUAL DAMRTCDI
02973              MOVE +0       TO CK-SEQ-NO
02974              MOVE AL-UNNON TO DAMRTCDA
02975              GO TO 0950-XIT.
02977  0950-NOT-FOUND.
02978      MOVE +0       TO CK-SEQ-NO.
02979      MOVE -1       TO DAMRTCDL.
02980      MOVE AL-UNBON TO DAMRTCDA.
02981      MOVE ER-2904  TO EMI-ERROR.
02982      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02984  0950-XIT.
02985      EXIT.
02987      EJECT
02989  1000-UPDATE-FILE.
02990 *    UPDATE FIELDS FROM MAP A
02992      MOVE PI-COMPANY-ID TO CK-COMP-ID.
02993      MOVE '1'           TO CK-REC-TYPE.
02995      
      * EXEC CICS READ
02996 *        UPDATE
02997 *        DATASET('ELCNTL')
02998 *        SET    (ADDRESS OF CONTROL-FILE)
02999 *        RIDFLD (ELCNTL-KEY)
03000 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&"S        EU         (   #00006867' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03002      IF CF-LAST-MAINT-BY     NOT = PI-UPDATE-BY     OR
03003         CF-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS
03004          
      * EXEC CICS UNLOCK
03005 *            DATASET('ELCNTL')
03006 *        END-EXEC
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&*                    #   #00006875' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03007          MOVE ER-0068 TO EMI-ERROR
03008          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03009          GO TO 7000-BUILD-INITIAL-MAP.
03011      MOVE 'B'          TO JP-RECORD-TYPE.
03012      MOVE CONTROL-FILE TO JP-RECORD-AREA.
03013      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
03015      IF COMPNMEL NOT = ZERO
03016          MOVE COMPNMEI   TO CF-CL-MAIL-TO-NAME.
03017      IF INCAREL NOT = ZERO
03018          MOVE INCAREI    TO CF-CL-IN-CARE-OF.
03019      IF ADDR1L NOT = ZERO
03020          MOVE ADDR1I     TO CF-CL-ADDR-LINE-1.
03021      IF ADDR2L NOT = ZERO
03022          MOVE ADDR2I     TO CF-CL-ADDR-LINE-2.
03023      IF CITYSTL NOT = ZERO
03024          MOVE CITYSTI    TO CF-CL-CITY-STATE.
03026      IF CF-CL-ZIP-CODE-NUM NUMERIC  AND
03027         CF-CL-ZIP-CODE-NUM NOT = ZEROS
03028          MOVE CF-CL-ZIP-CODE-NUM       TO WS-ZIP-CODE-NUM
03029          MOVE WS-ZIP-CODE-NUM          TO CF-CL-ZIP-CODE.
03031      IF ZIPCODEL NOT = ZERO
03032          MOVE ZIPCODEI                 TO  WS-ZIP-CODE
03033          MOVE ZEROS                    TO  CF-CL-ZIP-CODE-NUM
03034      ELSE
03035          GO TO 1010-CONTINUE-UPDATE.
03037      IF WS-CANADIAN-ZIP
03038          IF WS-ZIP-4 = SPACE  OR  '-'
03039              MOVE WS-ZIP-CAN-2-POST1   TO CF-CL-CAN-POSTAL-1
03040              MOVE WS-ZIP-CAN-2-POST2   TO CF-CL-CAN-POSTAL-2
03041          ELSE
03042              MOVE WS-ZIP-CAN-1-POST1   TO CF-CL-CAN-POSTAL-1
03043              MOVE WS-ZIP-CAN-1-POST2   TO CF-CL-CAN-POSTAL-2
03044      ELSE
03045          IF WS-ZIP-6 = SPACE  OR  '-'
03046              MOVE WS-ZIP-AM-2-CODE     TO CF-CL-ZIP-PRIME
03047              MOVE WS-ZIP-AM-2-PLUS4    TO CF-CL-ZIP-PLUS4
03048          ELSE
03049              MOVE WS-ZIP-AM-1-CODE     TO CF-CL-ZIP-PRIME
03050              MOVE WS-ZIP-AM-1-PLUS4    TO CF-CL-ZIP-PLUS4.
03052  1010-CONTINUE-UPDATE.
03053      IF PHONEL NOT = ZERO
03054          MOVE SV-PHONE   TO CF-CL-PHONE-NO.
03055      IF JOURNIDL NOT = ZERO
03056         MOVE JOURNIDI    TO CF-JOURNAL-FILE-ID.
03057      IF PASSWDL NOT = ZERO
03058          MOVE PASSWDI    TO CF-COMPANY-PASSWORD.
03059      IF SECOPTL NOT = ZERO
03060          MOVE SECOPTI    TO CF-SECURITY-OPTION.
03061      IF FPIDL NOT = ZERO
03062          MOVE FPIDI      TO CF-FORMS-PRINTER-ID.
03063      IF CPIDL NOT = ZERO
03064          MOVE CPIDI      TO CF-CHECK-PRINTER-ID.
03065      IF NEXTCOL NOT = ZERO
03066         MOVE NEXTCOI     TO CF-NEXT-COMPANY-ID.
03067      IF CLMREJL NOT = ZERO
03068          MOVE CLMREJI    TO CF-CO-CLAIM-REJECT-SW.
03070      IF ARSYSL NOT = ZERO
03071         IF ARSYSI = 'Y' OR 'N' OR ' '
03072             MOVE ARSYSI          TO PI-AR-PROCESSING-CNTL
03073                                     CF-SYSTEM-E
03074             MOVE LOW-VALUES      TO CF-AR-LAST-EL860-DT
03075                                     CF-AR-MONTH-END-DT.
03076      IF TAXIDL NOT = ZERO
03077         MOVE TAXIDI              TO CF-TAX-ID-NUMBER.
03078      IF CLSYSL NOT = ZERO
03079         MOVE CLSYSI              TO CF-LGX-CLAIM-USER.
03080      IF MPSYSL NOT = ZERO
03081         MOVE MPSYSI              TO CF-LGX-LIFE-USER.
03082      IF CCSYSL NOT = ZERO
03083         MOVE CCSYSI              TO CF-CRDTCRD-USER.
03085      IF EUSYSL NOT = ZERO
03086         IF EUSYSI = 'Y' OR 'N' OR ' '
03087             MOVE EUSYSI          TO CF-END-USER-REPORTING-USER.
03089      IF LCCODEL NOT = ZERO
03090         MOVE LCCODEI             TO CF-LOWER-CASE-LETTERS.
03091      IF CRSYSL NOT = ZERO
03092         MOVE CRSYSI              TO CF-LGX-CREDIT-USER.
03093      IF MEMBCAPL NOT = ZERO
03094         MOVE MEMBCAPI            TO CF-MEMBER-CAPTION.
03095      IF LFOVR1L NOT = ZERO
03096          MOVE LFOVR1I            TO CF-LIFE-OVERRIDE-L1
03097                                     PI-LIFE-OVERRIDE-L1.
03098      IF LFOVR2L NOT = ZERO
03099          MOVE LFOVR2I            TO CF-LIFE-OVERRIDE-L2
03100                                     PI-LIFE-OVERRIDE-L2.
03101      IF LFOVR6L NOT = ZERO
03102          MOVE LFOVR6I            TO CF-LIFE-OVERRIDE-L6
03103                                     PI-LIFE-OVERRIDE-L6.
03104      IF LFOVR12L NOT = ZERO
03105          MOVE LFOVR12I           TO CF-LIFE-OVERRIDE-L12
03106                                     PI-LIFE-OVERRIDE-L12.
03107      IF AHOVR1L NOT = ZERO
03108          MOVE AHOVR1I            TO CF-AH-OVERRIDE-L1
03109                                     PI-AH-OVERRIDE-L1.
03110      IF AHOVR2L NOT = ZERO
03111          MOVE AHOVR2I            TO CF-AH-OVERRIDE-L2
03112                                     PI-AH-OVERRIDE-L2.
03113      IF AHOVR6L NOT = ZERO
03114          MOVE AHOVR6I            TO CF-AH-OVERRIDE-L6
03115                                     PI-AH-OVERRIDE-L6.
03116      IF AHOVR12L NOT = ZERO
03117          MOVE AHOVR12I           TO CF-AH-OVERRIDE-L12
03118                                     PI-AH-OVERRIDE-L12.
03119      IF RPTCD1L NOT = ZERO
03120         MOVE RPTCD1I             TO CF-REPORT-CD1-CAPTION.
03121      IF RPTCD2L NOT = ZERO
03122         MOVE RPTCD2I             TO CF-REPORT-CD2-CAPTION.
03124      IF MORTMEL NOT = ZERO
03125          MOVE WS-SAVE-MORTMEDT   TO CF-MP-MONTH-END-DT.
03127      IF CURRMEL NOT = ZERO
03128          MOVE WS-SAVE-CURRMEDT   TO CF-CURRENT-MONTH-END.
03129      IF CREDMEL NOT = ZERO
03130          MOVE WS-SAVE-CREDMEDT   TO CF-CR-MONTH-END-DT.
03131      IF ARMEL   NOT = ZERO
03132          MOVE WS-SAVE-ARMEDT     TO CF-AR-MONTH-END-DT.
03133      IF AR860DTL NOT = ZERO
03134          MOVE WS-SAVE-AR860DT    TO CF-AR-LAST-EL860-DT.
03136      MOVE PI-PROCESSOR-ID TO CF-LAST-MAINT-BY
03137                              PI-UPDATE-BY.
03138      MOVE EIBTIME         TO CF-LAST-MAINT-HHMMSS
03139                              PI-UPDATE-HHMMSS.
03141      MOVE SAVE-BIN-DATE TO CF-LAST-MAINT-DT.
03142      MOVE 'C'           TO JP-RECORD-TYPE.
03143      MOVE CONTROL-FILE  TO JP-RECORD-AREA.
03144      
      * EXEC CICS REWRITE
03145 *        DATASET('ELCNTL')
03146 *        FROM   (CONTROL-FILE)
03147 *    END-EXEC.
           MOVE LENGTH OF CONTROL-FILE TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&& L                  %   #00007002' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03149      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
03150      MOVE ER-0000 TO EMI-ERROR.
03151      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03153 * UPDATE MORTGAGE COMPANY RECORD IF M/E DATE CHANGED
03154      IF MORTMEL = ZERO
03155          GO TO 7000-BUILD-INITIAL-MAP.
03157      MOVE PI-COMPANY-ID TO CK-COMP-ID.
03158      MOVE 'N'           TO CK-REC-TYPE.
03160      
      * EXEC CICS READ
03161 *        UPDATE
03162 *        DATASET('ELCNTL')
03163 *        SET    (ADDRESS OF CONTROL-FILE)
03164 *        RIDFLD (ELCNTL-KEY)
03165 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&"S        EU         (   #00007014' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03167      IF (CF-LAST-MAINT-BY     NOT = PI-MORTG-CO-UPDATE-BY)
03168                            OR
03169         (CF-LAST-MAINT-HHMMSS NOT = PI-MORTG-CO-UPDATE-HHMMSS)
03170          
      * EXEC CICS UNLOCK
03171 *            DATASET('ELCNTL')
03172 *        END-EXEC
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&*                    #   #00007023' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03173          MOVE ER-0068 TO EMI-ERROR
03174          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03175          GO TO 7000-BUILD-INITIAL-MAP.
03177      MOVE 'B'          TO JP-RECORD-TYPE.
03178      MOVE CONTROL-FILE TO JP-RECORD-AREA.
03179      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
03181      MOVE WS-SAVE-MORTMEDT   TO CF-MORTG-MONTH-END-DT.
03182      MOVE SPACES TO CF-MORTG-BILLING-AREA.
03184      MOVE PI-PROCESSOR-ID TO CF-LAST-MAINT-BY
03185                              PI-MORTG-CO-UPDATE-BY.
03186      MOVE EIBTIME         TO CF-LAST-MAINT-HHMMSS
03187                              PI-MORTG-CO-UPDATE-HHMMSS.
03189      MOVE SAVE-BIN-DATE TO CF-LAST-MAINT-DT.
03190      MOVE 'C'           TO JP-RECORD-TYPE.
03191      MOVE CONTROL-FILE  TO JP-RECORD-AREA.
03192      
      * EXEC CICS REWRITE
03193 *        DATASET('ELCNTL')
03194 *        FROM   (CONTROL-FILE)
03195 *    END-EXEC.
           MOVE LENGTH OF CONTROL-FILE TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&& L                  %   #00007041' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03197      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
03198      MOVE ER-0000 TO EMI-ERROR.
03199      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03201      GO TO 7000-BUILD-INITIAL-MAP.
03203      EJECT
03204  2000-UPDATE-FILE.
03205 *    UPDATE FIELDS FROM MAP B
03207      IF ZERO = APPROVL  AND
03208                LSTCLML  AND
03209                LSTARCHL AND
03210                STARCHL  AND
03211                LSTCHKL  AND
03212                CLMTOLL  AND
03213                LSTQUEL  AND
03214                CARRCLL  AND
03215                CLMREJL  AND
03216                COFFDTL  AND
03217                PMTSCHKL AND
03218                THRUTOL  AND
03219                RSVOPSWL AND
03220                IBLGMTHL AND
03221                CIDADISL AND
03222                CRDTBLUL AND
03223                CALCINTL AND
03224                IBNRLFFL AND
03225                IBNRAHFL AND
03226                OPTDTEL  AND
03227                LLEV1L   AND
03228                LLEV2L   AND
03229                LLEV3L   AND
03230                ALEV1L   AND
03231                ALEV2L   AND
03232                ALEV3L
03233         GO TO 7200-BUILD-INITIAL-MAP.
03235      MOVE PI-COMPANY-ID TO CK-COMP-ID.
03236      MOVE '1'           TO CK-REC-TYPE.
03237      
      * EXEC CICS READ
03238 *        UPDATE
03239 *        DATASET('ELCNTL')
03240 *        SET    (ADDRESS OF CONTROL-FILE)
03241 *        RIDFLD (ELCNTL-KEY)
03242 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&"S        EU         (   #00007081' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03244      IF CF-LAST-MAINT-BY     NOT = PI-UPDATE-BY     OR
03245         CF-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS
03246          
      * EXEC CICS UNLOCK
03247 *            DATASET('ELCNTL')
03248 *        END-EXEC
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&*                    #   #00007089' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03249          MOVE ER-0068 TO EMI-ERROR
03250          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03251          GO TO 7200-BUILD-INITIAL-MAP.
03253      MOVE 'B'          TO JP-RECORD-TYPE.
03254      MOVE CONTROL-FILE TO JP-RECORD-AREA.
03255      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
03257      IF APPROVL NOT = ZERO
03258         MOVE APPROVI     TO CF-PAYMENT-APPROVAL-SW.
03260      IF CF-PAYMENT-APPROVAL-SW NOT = 'G'
03261         MOVE ZEROS       TO CF-LIFE-PAY-APP-LEVEL-1
03262                             CF-LIFE-PAY-APP-LEVEL-2
03263                             CF-LIFE-PAY-APP-LEVEL-3
03264                             CF-AH-PAY-APP-LEVEL-1
03265                             CF-AH-PAY-APP-LEVEL-2
03266                             CF-AH-PAY-APP-LEVEL-3
03267         GO TO 2010-CONTINUE.
03269      IF LLEV1L NOT = ZERO
03270         MOVE SV-LLEV1    TO CF-LIFE-PAY-APP-LEVEL-1.
03271      IF LLEV2L NOT = ZERO
03272         MOVE SV-LLEV2    TO CF-LIFE-PAY-APP-LEVEL-2.
03273      IF LLEV3L NOT = ZERO
03274         MOVE SV-LLEV3    TO CF-LIFE-PAY-APP-LEVEL-3.
03276      IF ALEV1L NOT = ZERO
03277         MOVE SV-ALEV1    TO CF-AH-PAY-APP-LEVEL-1.
03278      IF ALEV2L NOT = ZERO
03279         MOVE SV-ALEV2    TO CF-AH-PAY-APP-LEVEL-2.
03280      IF ALEV3L NOT = ZERO
03281         MOVE SV-ALEV3    TO CF-AH-PAY-APP-LEVEL-3.
03283  2010-CONTINUE.
03284      IF LSTCLML NOT = ZERO
03285          MOVE LSTCLMI    TO CF-CO-CLAIM-COUNTER.
03286      IF LSTARCHL NOT = ZERO
03287          MOVE LSTARCHI   TO CF-CO-ARCHIVE-COUNTER.
03288      IF STARCHL NOT = ZERO
03289          MOVE STARCHI    TO CF-STARTING-ARCH-NO.
03290      IF LSTCHKL NOT = ZERO
03291          MOVE LSTCHKI    TO CF-CO-CHECK-COUNTER.
03292      IF CLMTOLL NOT = ZERO
03293          MOVE SV-CLMTOL  TO CF-CO-TOL-CLAIM.
03294      IF LSTQUEL NOT = ZERO
03295          MOVE LSTQUEI    TO CF-CO-CHECK-QUE-COUNTER.
03296      IF CARRCLL NOT = ZERO
03297         MOVE CARRCLI     TO CF-CARRIER-CONTROL-LEVEL.
03298      IF CLMREJL NOT = ZERO
03299         MOVE CLMREJI     TO CF-CO-CLAIM-REJECT-SW.
03300      IF COFFDTL NOT = ZERO
03301         MOVE WS-SAVE-COFFDTE TO CF-CLAIM-CUTOFF-DATE.
03302      IF PMTSCHKL NOT = ZERO
03303         MOVE PMTSCHKI TO CF-MAX-NUM-PMTS-CHECK.
03304      IF THRUTOL NOT = ZERO
03305         MOVE THRUTOI TO CF-CLAIM-PAID-THRU-TO.
03307      IF ADDRLBLL NOT = ZERO
03308         IF ADDRLBLI = ' ' OR 'N'
03309             MOVE 'N'             TO  CF-PRINT-ADDRESS-LABELS
03310         ELSE
03311             MOVE 'Y'             TO  CF-PRINT-ADDRESS-LABELS.
03313      IF RECONL NOT = ZERO
03314         IF (RECONI = ' ' OR 'N')
03315             MOVE 'N'             TO  CF-CLAIMS-CHECK-RECON-USER
03316         ELSE
03317             MOVE 'Y'             TO  CF-CLAIMS-CHECK-RECON-USER.
03319      IF PROCDTL NOT = ZERO
03320          MOVE WS-SAVE-PROCDT     TO  CF-CLAIMS-LAST-PROCESS-DT.
03322      IF AUDCLCGL NOT = ZERO
03323         IF (AUDCLCGI = ' ' OR 'N')
03324             MOVE 'N'             TO  CF-CLAIMS-AUDIT-CHANGES
03325         ELSE
03326            IF  AUDCLCGI = 'Y'
03327                MOVE 'Y'          TO  CF-CLAIMS-AUDIT-CHANGES
03328            ELSE
03329                MOVE -1           TO AUDCLCGL
03330                MOVE AL-UNBON     TO AUDCLCGA
03331                MOVE ER-8129      TO EMI-ERROR
03332                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03334      IF CRDCDINL NOT = ZERO
03335         IF (CRDCDINI = ' ' OR 'N')
03336             MOVE 'N'             TO  CF-CLAIMS-CREDIT-CARD-INDEX
03337         ELSE
03338             IF CRDCDINI = 'Y'
03339                MOVE 'Y'          TO  CF-CLAIMS-CREDIT-CARD-INDEX
03340             ELSE
03341                MOVE -1           TO CRDCDINL
03342                MOVE AL-UNBON     TO CRDCDINA
03343                MOVE ER-8132      TO EMI-ERROR
03344                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03346      IF LTRMTDTL NOT = ZERO
03347         IF LTRMTDTI NUMERIC
03348            MOVE LTRMTDTI         TO CF-CLAIMS-LETTER-MAINT-DAYS
03349         ELSE
03350            MOVE -1               TO LTRMTDTL
03351            MOVE AL-UNBON         TO LTRMTDTA
03352            MOVE ER-8130          TO EMI-ERROR
03353            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03355      IF  EXPRETNL NOT = ZERO
03356          MOVE W-EXPRETN          TO CF-EXPERIENCE-RETENTION-AGE.
03358      IF  RSVOPSWL NOT = ZERO
03360          IF  RSVOPSWI = ' ' OR 'N' OR LOW-VALUES
03361              MOVE 'N'            TO CF-CO-RESERVE-OPTION-SWITCH
03363          ELSE
03364              MOVE 'Y'            TO CF-CO-RESERVE-OPTION-SWITCH.
03366      IF  IBLGMTHL NOT = ZERO
03367          MOVE W-IBLGMTH          TO CF-CO-IBNR-LAG-MONTHS.
03369      IF  CIDADISL NOT = ZERO
03370          MOVE W-CIDADIS          TO CF-CO-CIDA-TABLE-DISCOUNT-PCT.
03372      IF  CRDTBLUL NOT = ZERO
03374          IF  CF-CO-RESERVE-OPTION-SWITCH EQUAL 'Y'
03375                  AND
03376              CRDTBLUI NOT GREATER THAN SPACES
03377              MOVE '9'            TO CF-CO-CRDB-TABLE-SELECTION
03379          ELSE
03380              MOVE CRDTBLUI       TO CF-CO-CRDB-TABLE-SELECTION.
03382      IF  CALCINTL NOT = ZERO
03383          MOVE W-CALCINT          TO CF-CO-CALCULATION-INTEREST.
03385      IF  IBNRLFFL NOT = ZERO
03386          MOVE W-IBNRLFF          TO CF-CO-IBNR-LIFE-FACTOR.
03388      IF  IBNRAHFL NOT = ZERO
03389          MOVE W-IBNRAHF          TO CF-CO-IBNR-AH-FACTOR.
03391      IF  OPTDTEL NOT = ZERO
03392          MOVE W-OPTDTE           TO CF-CO-OPTION-START-DATE.
03394      MOVE PI-PROCESSOR-ID TO CF-LAST-MAINT-BY  PI-UPDATE-BY.
03395      MOVE EIBTIME TO CF-LAST-MAINT-HHMMSS      PI-UPDATE-HHMMSS.
03397      MOVE SAVE-BIN-DATE TO CF-LAST-MAINT-DT.
03398      MOVE 'C'           TO JP-RECORD-TYPE.
03399      MOVE CONTROL-FILE  TO JP-RECORD-AREA.
03400      
      * EXEC CICS REWRITE
03401 *        DATASET('ELCNTL')
03402 *        FROM   (CONTROL-FILE)
03403 *    END-EXEC.
           MOVE LENGTH OF CONTROL-FILE TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&& L                  %   #00007216' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03405      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
03407      IF  EMI-FATAL-CTR GREATER THAN ZEROS
03408          GO TO 8210-SEND-DATAONLY.
03410      MOVE ER-0000 TO EMI-ERROR.
03411      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03412      MOVE -1                        TO CARRCLL.
03414      GO TO 8210-SEND-DATAONLY.
03416      EJECT
03417  3000-UPDATE-FILE.
03418 *    UPDATE FIELDS FROM MAP C
03420      
      * EXEC CICS HANDLE CONDITION
03421 *        NOTFND(8880-NOT-FOUND)
03422 *    END-EXEC.
           MOVE '"$I                   ! '' #00007230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03424      MOVE PI-COMPANY-ID TO CK-COMP-ID.
03425      MOVE '1'           TO CK-REC-TYPE.
03426      
      * EXEC CICS READ
03427 *        UPDATE
03428 *        DATASET('ELCNTL')
03429 *        SET    (ADDRESS OF CONTROL-FILE)
03430 *        RIDFLD (ELCNTL-KEY)
03431 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&"S        EU         (   #00007235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03433      IF CF-LAST-MAINT-BY     NOT = PI-UPDATE-BY     OR
03434         CF-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS
03435          
      * EXEC CICS UNLOCK
03436 *            DATASET('ELCNTL')
03437 *        END-EXEC
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&*                    #   #00007243' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03438          MOVE ER-0068 TO EMI-ERROR
03439          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03440          GO TO 7300-BUILD-INITIAL-MAP.
03442      MOVE 'B'          TO JP-RECORD-TYPE.
03443      MOVE CONTROL-FILE TO JP-RECORD-AREA.
03444      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
03446      IF CRTACSL NOT = ZERO
03447          MOVE CRTACSI    TO CF-CERT-ACCESS-CONTROL.
03448      IF REMTRML NOT = ZERO
03449          MOVE REMTRMI    TO CF-CR-REM-TERM-CALC.
03450      IF R78MTHL NOT = ZERO
03451          MOVE R78MTHI    TO CF-CR-R78-METHOD.
03452      IF TRMOPTNL NOT = ZERO
03453          MOVE TRMOPTNI   TO CF-REM-TRM-CALC-OPTION
03454                             PI-REM-TRM-CALC-OPTION.
03455      IF DABILLL NOT = ZERO
03456         IF DABILLI = 'Y'
03457            MOVE '1'      TO CF-SYSTEM-D
03458         ELSE
03459            MOVE ' '      TO CF-SYSTEM-D.
03461      IF SSNL NOT = ZERO
03462         IF SSNI = 'Y'
03463            MOVE '1'      TO CF-SOC-SEC-NO-SW
03464         ELSE
03465            MOVE ' '      TO CF-SOC-SEC-NO-SW.
03467      IF  AGECALCL NOT = ZERO
03468          MOVE AGECALCI   TO CF-MORTALITY-AGE-CALC-METHOD.
03470      IF CONSYSL NOT = ZERO
03471         IF CONSYSI = 'Y'
03472            MOVE '1'      TO CF-SYSTEM-C
03473         ELSE
03474            MOVE ' '      TO CF-SYSTEM-C.
03476      IF MEMBNOL NOT = ZERO
03477         IF MEMBNOI = 'Y'
03478            MOVE '1'      TO CF-MEMBER-NO-SW
03479         ELSE
03480            MOVE ' '      TO CF-MEMBER-NO-SW.
03482      IF MAILL NOT = ZERO
03483         IF MAILI = 'Y' OR 'N' OR ' '
03484             MOVE MAILI           TO PI-MAIL-PROCESSING
03485                                     CF-MAIL-PROCESSING.
03486      IF AMRTCDL NOT = ZERO
03487          MOVE AMRTCDI    TO CF-ALT-MORT-CODE.
03488      IF QTECALL NOT = ZERO
03489          MOVE SV-QTECAL  TO CF-CO-TOL-PREM.
03490      IF QTEREFL NOT = ZERO
03491          MOVE SV-QTEREF  TO CF-CO-TOL-REFUND.
03492      IF OVSAMTL > +0
03493         MOVE SV-OVSAMT  TO CF-CO-OVR-SHT-AMT
03494                            OVSAMTO.
03496      IF MINPREML NOT = ZERO
03497          MOVE SV-MINPREM TO CF-MIN-PREMIUM.
03498      IF COMPWTEL NOT = ZERO
03499          MOVE SV-COMPWTE TO CF-COMP-WRITE-OFF-AMT.
03500      IF MINAGEL NOT = ZERO
03501         MOVE MINAGEI     TO CF-MIN-AGE.
03503      MOVE ZERO           TO CF-MIN-TERM.
03505      IF MAXTERML NOT = ZERO
03506         MOVE MAXTERMI    TO CF-MAX-TERM.
03507      IF PREJCTL NOT = ZERO
03508         MOVE PREJCTI     TO CF-CO-PREM-REJECT-SW.
03509      IF REFREJL NOT = ZERO
03510         MOVE REFREJI     TO CF-CO-REF-REJECT-SW.
03511      IF DAGEL NOT = ZERO
03512         MOVE DAGEI       TO CF-DEFAULT-AGE.
03513      IF DSEXL NOT = ZERO
03514         MOVE DSEXI       TO CF-DEFAULT-SEX.
03516      IF DAPRL NOT = ZERO
03517         MOVE SV-DAPR     TO CF-DEFAULT-APR.
03519      IF JAGEL NOT = ZERO
03520         IF JAGEI = 'Y'
03521            MOVE '1'                 TO CF-JOINT-AGE-INPUT
03522           ELSE
03523            MOVE SPACE               TO CF-JOINT-AGE-INPUT.
03525      IF PRCESSFL NOT = ZERO
03526         IF PRCESSFI = 'Q'
03527            MOVE '1'                 TO CF-RUN-FREQUENCY-SW
03528           ELSE
03529            MOVE SPACE               TO CF-RUN-FREQUENCY-SW.
03531      IF BINPUTL NOT = ZERO
03532         IF BINPUTI = 'Y'
03533            MOVE '1'                 TO CF-BIRTH-DATE-INPUT
03534           ELSE
03535            MOVE SPACE               TO CF-BIRTH-DATE-INPUT.
03537      IF MTOLCAPL NOT = ZERO
03538          MOVE SV-MTOLCAP      TO CF-CO-TOL-CAP.
03540      IF PRMPCTL NOT = ZERO
03541         MOVE WS-TOL-PREM-PCT     TO CF-CO-TOL-PREM-PCT.
03543      IF OVSPCTL > +0
03544         MOVE WS-CO-OVR-SHT-PCT TO CF-CO-OVR-SHT-PCT
03545                                   OVSPCTO.
03547      IF REFPCTL NOT = ZERO
03548         MOVE WS-TOL-REFUND-PCT   TO   CF-CO-TOL-REFUND-PCT.
03550      IF CONDTEL NOT = ZERO
03551         MOVE WS-SAVE-CONDTE   TO CF-CONVERSION-DT.
03553      IF LSTBTCHL NOT = ZERO
03554          MOVE LSTBTCHI        TO CF-LAST-BATCH-NO.
03556      IF CMETHODL NOT = ZERO
03557         MOVE CMETHODI         TO CF-CR-CHECK-NO-METHOD.
03559      IF CARGRPOL NOT = ZERO
03560         MOVE CARGRPOI         TO CF-CAR-GROUP-ACCESS-CNTL.
03562      IF STCNTLL NOT = ZERO
03563         MOVE STCNTLI          TO CF-CO-ST-CALL-RPT-CNTL.
03565      IF ARCHSTL NOT = ZERO
03566         MOVE ARCHSTI          TO CF-CREDIT-START-ARCH-NUM
03568      ELSE
03569          IF  CF-CREDIT-START-ARCH-NUM NOT NUMERIC
03570              MOVE ZEROS       TO CF-CREDIT-START-ARCH-NUM.
03572      IF ARCHLSTL NOT = ZERO
03573         MOVE ARCHLSTI         TO CF-CREDIT-LAST-ARCH-NUM
03575      ELSE
03576          IF  CF-CREDIT-LAST-ARCH-NUM NOT NUMERIC
03577              MOVE ZEROS       TO CF-CREDIT-LAST-ARCH-NUM.
03579      IF ARCHPYRL NOT = ZERO
03580         MOVE ARCHPYRI         TO CF-CREDIT-ARCH-PURGE-YR.
03582      IF CKCOUNTL NOT = ZERO
03583         MOVE CKCOUNTI         TO CF-CR-CHECK-COUNTER.
03585      IF QUCOUNTL NOT = ZERO
03586         MOVE QUCOUNTI         TO CF-CR-CHECK-QUE-COUNTER.
03588      IF CRLABELL NOT = ZERO
03589         MOVE CRLABELI         TO CF-CR-PRINT-ADDRESS-LABELS.
03591      MOVE PI-PROCESSOR-ID TO CF-LAST-MAINT-BY
03592                              PI-UPDATE-BY.
03593      MOVE EIBTIME         TO CF-LAST-MAINT-HHMMSS
03594                              PI-UPDATE-HHMMSS.
03596      MOVE SAVE-BIN-DATE TO CF-LAST-MAINT-DT.
03597      MOVE 'C'           TO JP-RECORD-TYPE.
03598      MOVE CONTROL-FILE  TO JP-RECORD-AREA.
03599      
      * EXEC CICS REWRITE
03600 *        DATASET('ELCNTL')
03601 *        FROM   (CONTROL-FILE)
03602 *    END-EXEC.
           MOVE LENGTH OF CONTROL-FILE TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&& L                  %   #00007374' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03604      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
03605      MOVE ER-0000 TO EMI-ERROR.
03606      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03607      MOVE -1                     TO PREJCTL.
03609      GO TO 8220-SEND-DATAONLY.
03611      EJECT
03612  4000-UPDATE-FILE.
03613 *    UPDATE FIELDS FROM MAP D
03615      
      * EXEC CICS HANDLE CONDITION
03616 *        NOTFND(8880-NOT-FOUND)
03617 *    END-EXEC.
           MOVE '"$I                   ! ( #00007386' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03619      MOVE PI-COMPANY-ID TO CK-COMP-ID.
03620      MOVE 'N'           TO CK-REC-TYPE.
03621      
      * EXEC CICS READ
03622 *        UPDATE
03623 *        DATASET('ELCNTL')
03624 *        SET    (ADDRESS OF CONTROL-FILE)
03625 *        RIDFLD (ELCNTL-KEY)
03626 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&"S        EU         (   #00007391' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03628      IF CF-LAST-MAINT-BY     NOT = PI-MORTG-CO-UPDATE-BY    OR
03629         CF-LAST-MAINT-HHMMSS NOT = PI-MORTG-CO-UPDATE-HHMMSS
03630          
      * EXEC CICS UNLOCK
03631 *            DATASET('ELCNTL')
03632 *        END-EXEC
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&*                    #   #00007399' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03633          MOVE ER-0068 TO EMI-ERROR
03634          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03635          GO TO 7400-BUILD-INITIAL-MAP.
03637      MOVE 'B'          TO JP-RECORD-TYPE.
03638      MOVE CONTROL-FILE TO JP-RECORD-AREA.
03639      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
03641      IF MIBVERL NOT EQUAL ZERO
03642          MOVE MIBVERI TO CF-MORTG-MIB-VERSION.
03644      IF DAMRTCDL NOT EQUAL ZERO
03645          MOVE DAMRTCDI TO CF-MORTG-ALT-MORT-CODE.
03647      IF LNMESERL NOT EQUAL ZERO
03648          MOVE LNMESERI TO WS-LNAME-SEARCH-CNTL
03649          MOVE WS-MIB-LNAME-SEARCH TO CF-MORTG-MIB-LNAME-SEARCH
03650          MOVE WS-ALP-LNAME-SEARCH TO CF-MORTG-ALP-LNAME-SEARCH.
03652      IF FNMESERL NOT EQUAL ZERO
03653          MOVE FNMESERI TO WS-FNAME-SEARCH-CNTL
03654          MOVE WS-MIB-FNAME-SEARCH TO CF-MORTG-MIB-FNAME-SEARCH
03655          MOVE WS-ALP-FNAME-SEARCH TO CF-MORTG-ALP-FNAME-SEARCH.
03657      IF MNMESERL NOT EQUAL ZERO
03658          MOVE MNMESERI TO WS-MNAME-SEARCH-CNTL
03659          MOVE WS-MIB-MNAME-SEARCH TO CF-MORTG-MIB-MNAME-SEARCH
03660          MOVE WS-ALP-MNAME-SEARCH TO CF-MORTG-ALP-MNAME-SEARCH.
03662      IF BDATEL NOT EQUAL ZERO
03663          MOVE BDATEI TO WS-BDATE-SEARCH-CNTL
03664          MOVE WS-MIB-BDATE-SEARCH TO CF-MORTG-MIB-BDATE-SEARCH
03665          MOVE WS-ALP-BDATE-SEARCH TO CF-MORTG-ALP-BDATE-SEARCH.
03667      IF BSTATEL NOT EQUAL ZERO
03668          MOVE BSTATEI TO WS-BSTATE-SEARCH-CNTL
03669          MOVE WS-MIB-BSTATE-SEARCH TO CF-MORTG-MIB-BSTATE-SEARCH
03670          MOVE WS-ALP-BSTATE-SEARCH TO CF-MORTG-ALP-BSTATE-SEARCH.
03672      IF RSTATEL NOT EQUAL ZERO
03673          MOVE RSTATEI TO WS-RSTATE-SEARCH-CNTL
03674          MOVE WS-MIB-RSTATE-SEARCH TO CF-MORTG-MIB-RSTATE-SEARCH
03675          MOVE WS-ALP-RSTATE-SEARCH TO CF-MORTG-ALP-RSTATE-SEARCH.
03677      IF MIBSYML NOT EQUAL ZERO
03678          MOVE MIBSYMI TO CF-MORTG-MIB-COMPANY-SYMBOL.
03680      IF MGCNTLL NOT EQUAL ZERO
03681          MOVE MGCNTLI TO CF-MORTG-ACCESS-CONTROL.
03683      IF MIBCOMML NOT EQUAL ZERO
03684          MOVE MIBCOMMI TO CF-MORTG-MIB-COMM.
03686      IF MIBCSWL NOT EQUAL ZERO
03687          IF MIBCSWI EQUAL 'Y'
03688              MOVE '1' TO CF-MORTG-MIB-DEST-SW.
03690      IF MIBTSWL NOT EQUAL ZERO
03691          IF MIBTSWI EQUAL 'Y'
03692              MOVE '2' TO CF-MORTG-MIB-DEST-SW.
03694      IF (MIBCSWL NOT EQUAL ZERO) AND
03695         (MIBTSWL NOT EQUAL ZERO)
03696          IF (MIBCSWI EQUAL 'N') AND
03697             (MIBTSWI EQUAL 'N')
03698              MOVE '0' TO CF-MORTG-MIB-DEST-SW.
03700      IF MIBTERML NOT EQUAL ZERO
03701          MOVE MIBTERMI TO CF-MORTG-MIB-TERM.
03703      IF CF-MORTG-MIB-VERSION EQUAL '0'
03704         IF CF-MORTG-MIB-DEST-SW EQUAL '1' OR '2'
03705             
      * EXEC CICS UNLOCK
03706 *              DATASET('ELCNTL')
03707 *           END-EXEC
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&*                    #   #00007457' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03708             MOVE AL-UABON TO MIBCSWA
03709             MOVE -1       TO MIBCSWL
03710             MOVE ER-9460  TO EMI-ERROR
03711             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03712             GO TO 8230-SEND-DATAONLY.
03714      IF GRPSWL NOT EQUAL ZERO
03715          MOVE GRPSWI             TO CF-MORTG-BILL-GROUPING-CODE.
03716      IF RATESWL NOT EQUAL ZERO
03717          MOVE RATESWI            TO CF-RATE-DEV-AUTHORIZATION.
03718      IF RATFILCL NOT = ZERO
03719         MOVE WS-SAVE-RTEFILC  TO CF-MORTG-RATE-FILE-CREAT-DATE.
03721      IF PI-USER-ALMIGHTY-YES
03722          IF STARCHNL NOT = ZERO
03723              IF STARCHNI NOT NUMERIC
03724                  MOVE -1         TO STARCHNL
03725                  MOVE AL-UNBON   TO STARCHNA
03726                  MOVE ER-0611    TO EMI-ERROR
03727                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03728              ELSE
03729                  MOVE AL-UNNON   TO STARCHNA
03730                  MOVE STARCHNI   TO CF-MORTG-START-ARCH-NUM.
03732      IF PI-USER-ALMIGHTY-YES
03733          IF ARCHNBRL NOT = ZERO
03734              IF ARCHNBRI NOT NUMERIC
03735                  MOVE -1         TO ARCHNBRL
03736                  MOVE AL-UNBON   TO ARCHNBRA
03737                  MOVE ER-0611    TO EMI-ERROR
03738                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03739              ELSE
03740                  MOVE AL-UNNON   TO ARCHNBRA
03741                  MOVE ARCHNBRI   TO CF-MORTG-CURRENT-ARCH-NUM.
03743      IF PI-PROCESSOR-ID = 'LGXX'
03744         IF LABCNTLL NOT EQUAL ZERO
03745             MOVE LABCNTLI        TO CF-MORTG-LABEL-CONTROL.
03747      IF PI-PROCESSOR-ID = 'LGXX'
03748         IF BCYCLE1L NOT EQUAL ZERO
03749             MOVE BCYCLE1I TO CF-MORTG-BILL-CYCLE (1).
03751      IF PI-PROCESSOR-ID = 'LGXX'
03752         IF BCYCLE2L NOT EQUAL ZERO
03753             MOVE BCYCLE2I TO CF-MORTG-BILL-CYCLE (2).
03755      IF PI-PROCESSOR-ID = 'LGXX'
03756         IF BCYCLE3L NOT EQUAL ZERO
03757             MOVE BCYCLE3I TO CF-MORTG-BILL-CYCLE (3).
03759      IF PI-PROCESSOR-ID = 'LGXX'
03760         IF BCYCLE4L NOT EQUAL ZERO
03761             MOVE BCYCLE4I TO CF-MORTG-BILL-CYCLE (4).
03763      IF PI-PROCESSOR-ID = 'LGXX'
03764         IF BCYCLE5L NOT EQUAL ZERO
03765             MOVE BCYCLE5I TO CF-MORTG-BILL-CYCLE (5).
03767      IF  PI-PROCESSOR-ID = 'LGXX'
03768          MOVE SV-MORTG-REF-NUM
03769                              TO CF-MORTG-SOLICITATION-NUM.
03771      IF  PI-PROCESSOR-ID = 'LGXX'
03772          MOVE AUTOREFI       TO CF-ASSIGN-POLICY-NO-SW.
03774      IF  PI-PROCESSOR-ID = 'LGXX'
03775          MOVE SHFTLNOI       TO CF-MORTG-LOAN-SHIFT-IND.
03777      IF  PI-PROCESSOR-ID = 'LGXX'
03778          MOVE CHKPMTHI       TO CF-MP-CHECK-NO-METHOD.
03780      IF  PI-PROCESSOR-ID = 'LGXX'
03781          MOVE MRECONI        TO CF-MP-RECON-USE-IND.
03783      IF  RPTLANGA NOT = ZERO
03784          MOVE RPTLANGI       TO CF-MP-REPORT-LANGUAGE-IND.
03786      IF  PLCYLNKA NOT = ZERO
03787          MOVE PLCYLNKI       TO CF-MP-POLICY-LINKAGE-IND.
03789      IF RATFILML NOT = ZERO
03790         MOVE WS-SAVE-RTEFILM  TO CF-MORTG-RATE-FILE-MAINT-DATE.
03792      IF PRDMSTCL NOT = ZERO
03793         MOVE WS-SAVE-PRDMSTC  TO CF-MORTG-PROD-FILE-CREAT-DATE.
03795      IF PRDMSTML NOT = ZERO
03796         MOVE WS-SAVE-PRDMSTM  TO CF-MORTG-PROD-FILE-MAINT-DATE.
03798      IF CNVDTEL NOT = ZERO
03799         MOVE WS-SAVE-CONDTE   TO CF-MORTG-CONVERSION-DATE.
03801      IF CHKCNTRL NOT = ZERO
03802         MOVE CHKCNTRI         TO CF-MORTG-CHECK-NO-COUNTER.
03804      IF QUECNTRL NOT = ZERO
03805         MOVE QUECNTRI         TO CF-MORTG-CHECK-QUEUE-COUNTER.
03807      MOVE PI-PROCESSOR-ID TO CF-LAST-MAINT-BY
03808                              PI-MORTG-CO-UPDATE-BY.
03809      MOVE EIBTIME         TO CF-LAST-MAINT-HHMMSS
03810                              PI-MORTG-CO-UPDATE-HHMMSS.
03812      MOVE SAVE-BIN-DATE TO CF-LAST-MAINT-DT.
03813      MOVE 'C'           TO JP-RECORD-TYPE.
03814      MOVE CONTROL-FILE  TO JP-RECORD-AREA.
03815      
      * EXEC CICS REWRITE
03816 *        DATASET('ELCNTL')
03817 *        FROM   (CONTROL-FILE)
03818 *    END-EXEC.
           MOVE LENGTH OF CONTROL-FILE TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&& L                  %   #00007543' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03820      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
03821      MOVE ER-0000 TO EMI-ERROR.
03822      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03823      MOVE -1                     TO MIBSYML.
03825      IF RENTBLCL  = ZERO AND
03826         COMMSTCL  = ZERO AND
03827         RENTBLML  = ZERO AND
03828         COMMSTML  = ZERO AND
03829         DRESURL   = ZERO AND
03830         CMPCNTLL  = ZERO
03831          GO TO 8230-SEND-DATAONLY.
03833      
      * EXEC CICS HANDLE CONDITION
03834 *        NOTFND(8880-NOT-FOUND)
03835 *    END-EXEC.
           MOVE '"$I                   ! ) #00007558' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03837      MOVE PI-COMPANY-ID TO CK-COMP-ID.
03838      MOVE '1'           TO CK-REC-TYPE.
03839      
      * EXEC CICS READ
03840 *        UPDATE
03841 *        DATASET('ELCNTL')
03842 *        SET    (ADDRESS OF CONTROL-FILE)
03843 *        RIDFLD (ELCNTL-KEY)
03844 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&"S        EU         (   #00007563' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03846      IF CF-LAST-MAINT-BY     NOT = PI-UPDATE-BY     OR
03847         CF-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS
03848          
      * EXEC CICS UNLOCK
03849 *            DATASET('ELCNTL')
03850 *        END-EXEC
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&*                    #   #00007571' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03851          MOVE ER-0068 TO EMI-ERROR
03852          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03853          GO TO 7400-BUILD-INITIAL-MAP.
03855      MOVE 'B'          TO JP-RECORD-TYPE.
03856      MOVE CONTROL-FILE TO JP-RECORD-AREA.
03857      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
03859      IF DRESURL NOT = ZERO
03860         IF DRESURI = 'Y'
03861            MOVE '1'              TO CF-REIN-TABLE-SW
03862           ELSE
03863            MOVE ' '              TO CF-REIN-TABLE-SW.
03865      IF CMPCNTLL NOT = ZERO
03866         MOVE CMPCNTLI         TO CF-CAR-GROUP-ACCESS-CNTL.
03868      IF RENTBLCL NOT = ZERO
03869         MOVE WS-SAVE-RENTABC  TO CF-REINSURANCE-TAB-CREATE-DT.
03871      IF COMMSTCL NOT = ZERO
03872         MOVE WS-SAVE-CMPMSTC  TO CF-COMPENSATION-MSTR-CREATE-DT.
03874      IF RENTBLML NOT = ZERO
03875         MOVE WS-SAVE-RENTABM  TO CF-REINSURANCE-TAB-MAINT-DT.
03877      IF COMMSTML NOT = ZERO
03878         MOVE WS-SAVE-CMPMSTM  TO CF-COMPENSATION-MSTR-MAINT-DT.
03880      MOVE PI-PROCESSOR-ID TO CF-LAST-MAINT-BY
03881                              PI-UPDATE-BY.
03882      MOVE EIBTIME         TO CF-LAST-MAINT-HHMMSS
03883                              PI-UPDATE-HHMMSS.
03885      MOVE SAVE-BIN-DATE TO CF-LAST-MAINT-DT.
03886      MOVE 'C'           TO JP-RECORD-TYPE.
03887      MOVE CONTROL-FILE  TO JP-RECORD-AREA.
03888      
      * EXEC CICS REWRITE
03889 *        DATASET('ELCNTL')
03890 *        FROM   (CONTROL-FILE)
03891 *    END-EXEC.
           MOVE LENGTH OF CONTROL-FILE TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&& L                  %   #00007602' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03893      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
03894      MOVE ER-0000 TO EMI-ERROR.
03895      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03896      MOVE -1                     TO MIBSYML.
03898      GO TO 8230-SEND-DATAONLY.
03900      EJECT
03901  5000-UPDATE-FILE.
03902 *    UPDATE FIELDS FROM MAP E
03904      
      * EXEC CICS HANDLE CONDITION
03905 *        NOTFND(8880-NOT-FOUND)
03906 *    END-EXEC.
           MOVE '"$I                   ! * #00007614' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03908      MOVE PI-COMPANY-ID TO CK-COMP-ID.
03909      MOVE '1'           TO CK-REC-TYPE.
03910      
      * EXEC CICS READ
03911 *        UPDATE
03912 *        DATASET('ELCNTL')
03913 *        SET    (ADDRESS OF CONTROL-FILE)
03914 *        RIDFLD (ELCNTL-KEY)
03915 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&"S        EU         (   #00007619' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03917      IF CF-LAST-MAINT-BY     NOT = PI-UPDATE-BY     OR
03918         CF-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS
03919          
      * EXEC CICS UNLOCK
03920 *            DATASET('ELCNTL')
03921 *        END-EXEC
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&*                    #   #00007627' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03922          MOVE ER-0068 TO EMI-ERROR
03923          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03924          GO TO 7500-BUILD-INITIAL-MAP.
03926      MOVE 'B'          TO JP-RECORD-TYPE.
03927      MOVE CONTROL-FILE TO JP-RECORD-AREA.
03928      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
03930      IF COMTABCL NOT = ZERO
03931         MOVE WS-SAVE-COMTABC  TO CF-COMMISSION-TAB-CREATE-DT.
03933      IF RTEFILCL NOT = ZERO
03934         MOVE WS-SAVE-RTEFILC  TO CF-RATES-FILE-CREATE-DT.
03936      IF ACTMSTCL NOT = ZERO
03937         MOVE WS-SAVE-ACTMSTC  TO CF-ACCOUNT-MSTR-CREATE-DT.
03939      IF RENTABCL NOT = ZERO
03940         MOVE WS-SAVE-RENTABC  TO CF-REINSURANCE-TAB-CREATE-DT.
03942      IF CMPMSTCL NOT = ZERO
03943         MOVE WS-SAVE-CMPMSTC  TO CF-COMPENSATION-MSTR-CREATE-DT.
03945      IF RTEFILML NOT = ZERO
03946         MOVE WS-SAVE-RTEFILM  TO CF-RATES-FILE-MAINT-DT.
03948      IF COMTABML NOT = ZERO
03949         MOVE WS-SAVE-COMTABM  TO CF-COMMISSION-TAB-MAINT-DT.
03951      IF ACTMSTML NOT = ZERO
03952         MOVE WS-SAVE-ACTMSTM  TO CF-ACCOUNT-MSTR-MAINT-DT.
03954      IF RENTABML NOT = ZERO
03955         MOVE WS-SAVE-RENTABM  TO CF-REINSURANCE-TAB-MAINT-DT.
03957      IF CMPMSTML NOT = ZERO
03958         MOVE WS-SAVE-CMPMSTM  TO CF-COMPENSATION-MSTR-MAINT-DT.
03960      MOVE PI-PROCESSOR-ID TO CF-LAST-MAINT-BY
03961                              PI-UPDATE-BY.
03962      MOVE EIBTIME         TO CF-LAST-MAINT-HHMMSS
03963                              PI-UPDATE-HHMMSS.
03965      MOVE SAVE-BIN-DATE TO CF-LAST-MAINT-DT.
03966      MOVE 'C'           TO JP-RECORD-TYPE.
03967      MOVE CONTROL-FILE  TO JP-RECORD-AREA.
03968      
      * EXEC CICS REWRITE
03969 *        DATASET('ELCNTL')
03970 *        FROM   (CONTROL-FILE)
03971 *    END-EXEC.
           MOVE LENGTH OF CONTROL-FILE TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&& L                  %   #00007663' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03973      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
03974      MOVE ER-0000 TO EMI-ERROR.
03975      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03976      MOVE -1                     TO RTEFILML.
03978      GO TO 8240-SEND-DATAONLY.
03980      EJECT
03981  1500-VERIFY-NEXT-COMPANY.
03982      IF NEXTCOI = SPACES
03983         GO TO 1599-EXIT.
03985      
      * EXEC CICS HANDLE CONDITION
03986 *         NOTFND(1550-NOT-FOUND)
03987 *    END-EXEC.
           MOVE '"$I                   ! + #00007676' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03989      
      * EXEC CICS READ
03990 *         DATASET('ELCNTL')
03991 *         SET    (ADDRESS OF CONTROL-FILE)
03992 *         RIDFLD (WS-ELCNTL-KEY)
03993 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&"S        E          (   #00007679' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03995      IF PI-PROCESSOR-ID NOT = 'LGXX'
03996         MOVE '2'             TO WS-TYPE
03997         MOVE PI-PROCESSOR-ID TO WS-USER
03998         MOVE NEXTCOI         TO WS-COMP-ID
03999         MOVE +0              TO WS-SEQ
04000         
      * EXEC CICS READ
04001 *         DATASET('ELCNTL')
04002 *         SET    (ADDRESS OF CONTROL-FILE)
04003 *         RIDFLD (WS-ELCNTL-KEY)
04004 *       END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&"S        E          (   #00007689' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04006      MOVE CONTROL-FILE-SAVE-AREA TO CONTROL-FILE.
04007      MOVE AL-UANON               TO NEXTCOA.
04008      GO TO 1599-EXIT.
04010  1550-NOT-FOUND.
04011      IF WS-TYPE = '1'
04012         MOVE ER-0089 TO EMI-ERROR
04013      ELSE
04014         MOVE ER-0228 TO EMI-ERROR.
04016      MOVE -1       TO NEXTCOL.
04017      MOVE AL-UABON TO NEXTCOA.
04018      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04019      GO TO 8200-SEND-DATAONLY.
04021  1599-EXIT.
04022      EXIT.
04024      EJECT
04025  7000-BUILD-INITIAL-MAP.
04026 *    BUILD MAP A
04028      MOVE LOW-VALUES TO EL102AO.
04029      MOVE PI-COMPANY-ID TO CK-COMP-ID.
04030      MOVE '1'           TO CK-REC-TYPE.
04031      
      * EXEC CICS READ
04032 *        DATASET('ELCNTL')
04033 *        SET    (ADDRESS OF CONTROL-FILE)
04034 *        RIDFLD (ELCNTL-KEY)
04035 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&"S        E          (   #00007714' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04037      MOVE LOW-VALUES         TO WS-COMP-CD-R.
04038      MOVE CF-COMPANY-CD      TO WS-COMP-CD-X.
04039      MOVE WS-COMP-CD         TO COMPCDO.
04040      MOVE CF-CL-ADDR-LINE-1  TO ADDR1O.
04041      MOVE CF-CL-ADDR-LINE-2  TO ADDR2O.
04042      MOVE CF-CL-CITY-STATE   TO CITYSTO.
04043      MOVE CF-JOURNAL-FILE-ID TO JOURNIDO.
04045      IF PI-PROCESSOR-ID = 'LGXX'
04046         MOVE AL-UNNON TO JOURNIDA
04047         MOVE AL-UANON TO LFOVR1A
04048                          LFOVR2A
04049                          LFOVR6A
04050                          LFOVR12A
04051                          AHOVR1A
04052                          AHOVR2A
04053                          AHOVR6A
04054                          AHOVR12A
04055      ELSE
04056         MOVE AL-SANON TO CRSYSA
04057         MOVE AL-SANON TO MPSYSA
04058         MOVE AL-SANOF TO JOURNIDA.
04060      MOVE ' '              TO DC-OPTION-CODE.
04061      MOVE CF-LAST-MAINT-DT TO DC-BIN-DATE-1.
04062      MOVE LINK-ELDATCV     TO PGM-NAME.
04063      
      * EXEC CICS LINK
04064 *        PROGRAM (PGM-NAME)
04065 *        COMMAREA(DATE-CONVERSION-DATA)
04066 *        LENGTH  (DC-COMM-LENGTH)
04067 *    END-EXEC.
           MOVE '."C                   ''   #00007743' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04069      MOVE CF-LAST-MAINT-BY       TO LSTUSRO.
04070      MOVE DC-GREG-DATE-1-EDIT    TO LSTDTEO.
04071      MOVE CF-LAST-MAINT-HHMMSS   TO TIME-IN.
04072      MOVE TIME-OUT               TO LSTTIMEO.
04074      IF CF-CL-ZIP-CODE-NUM NUMERIC  AND
04075         CF-CL-ZIP-CODE-NUM NOT = ZEROS
04076          MOVE CF-CL-ZIP-CODE-NUM   TO WS-ZIP-CODE-NUM
04077          MOVE WS-ZIP-CODE-NUM      TO CF-CL-ZIP-CODE.
04079      MOVE SPACES                   TO WS-ZIP-CODE.
04080      IF CF-CL-CAN-POST-CODE
04081          MOVE CF-CL-CAN-POSTAL-1   TO WS-ZIP-CAN-2-POST1
04082          MOVE CF-CL-CAN-POSTAL-2   TO WS-ZIP-CAN-2-POST2
04083      ELSE
04084          MOVE CF-CL-ZIP-PRIME      TO WS-ZIP-AM-2-CODE
04085          IF CF-CL-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
04086              MOVE '-'              TO WS-ZIP-AM-2-DASH
04087              MOVE CF-CL-ZIP-PLUS4  TO WS-ZIP-AM-2-PLUS4.
04089      MOVE WS-ZIP-CODE              TO ZIPCODEO.
04091      MOVE CF-CL-PHONE-NO         TO WS-PHONE-IN.
04092      MOVE WSPI-AREA              TO WSPO-AREA.
04093      MOVE WSPI-PFX               TO WSPO-PFX.
04094      MOVE WSPI-SFX               TO WSPO-SFX.
04095      MOVE WS-PHONE-OUT           TO PHONEO.
04096      MOVE CF-SECURITY-OPTION     TO SECOPTO.
04097      MOVE CF-FORMS-PRINTER-ID    TO FPIDO.
04098      MOVE CF-CHECK-PRINTER-ID    TO CPIDO.
04099      MOVE CF-NEXT-COMPANY-ID     TO NEXTCOO.
04100      MOVE CF-SYSTEM-E            TO ARSYSO
04101                                     PI-AR-PROCESSING-CNTL.
04102      MOVE CF-TAX-ID-NUMBER       TO TAXIDO.
04103      MOVE AL-UANON               TO TAXIDA.
04104      MOVE CF-LGX-CREDIT-USER     TO CRSYSO.
04105      MOVE CF-LGX-LIFE-USER       TO MPSYSO.
04106      MOVE CF-CRDTCRD-USER        TO CCSYSO.
04107      MOVE CF-END-USER-REPORTING-USER
04108                                  TO EUSYSO.
04110      IF CF-LOWER-CASE-LETTERS = 'Y' OR 'N' OR ' '
04111         MOVE CF-LOWER-CASE-LETTERS  TO LCCODEO
04112         ELSE
04113         MOVE SPACES                 TO LCCODEO.
04115      MOVE CF-LGX-CLAIM-USER      TO CLSYSO.
04117      IF CF-MEMBER-CAPTION = SPACES OR LOW-VALUES
04118          MOVE 'MEMBER NO.'          TO MEMBCAPO
04119      ELSE
04120          MOVE CF-MEMBER-CAPTION     TO MEMBCAPO.
04122      IF CF-REPORT-CD1-CAPTION = SPACES OR LOW-VALUES
04123          MOVE 'RPT CODE 1'          TO RPTCD1O
04124      ELSE
04125          MOVE CF-REPORT-CD1-CAPTION TO RPTCD1O.
04127      IF CF-REPORT-CD2-CAPTION = SPACES OR LOW-VALUES
04128          MOVE 'RPT CODE 2'          TO RPTCD2O
04129      ELSE
04130          MOVE CF-REPORT-CD2-CAPTION TO RPTCD2O.
04132      MOVE AL-UANON               TO CRSYSA
04133                                     CLSYSA
04134                                     MPSYSA
04135                                     CCSYSA
04136                                     LCCODEA.
04138      IF CO-HAS-CLAS-IC-CLAIM
04139          MOVE ' '                  TO DC-OPTION-CODE
04140          MOVE CF-CURRENT-MONTH-END TO DC-BIN-DATE-1
04141          MOVE LINK-ELDATCV         TO PGM-NAME
04142          
      * EXEC CICS LINK
04143 *            PROGRAM (PGM-NAME)
04144 *            COMMAREA(DATE-CONVERSION-DATA)
04145 *            LENGTH  (DC-COMM-LENGTH)
04146 *        END-EXEC
           MOVE '."C                   ''   #00007810' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04147          MOVE DC-GREG-DATE-1-EDIT TO CURRMEO
04148          MOVE AL-UNNON            TO CURRMEA
04149          MOVE AL-SABOF            TO CURRMEDA
04150      ELSE
04151          MOVE AL-SADOF TO CURRMEA
04152                           CURRMEDA.
04154      IF CO-HAS-CLAS-IC-CREDIT
04155          IF CF-CR-MONTH-END-DT = SPACES
04156              MOVE AL-SABOF       TO CREDMEDA
04157          ELSE
04158              MOVE ' '                TO DC-OPTION-CODE
04159              MOVE CF-CR-MONTH-END-DT TO DC-BIN-DATE-1
04160              MOVE LINK-ELDATCV       TO PGM-NAME
04161              
      * EXEC CICS LINK
04162 *                PROGRAM (PGM-NAME)
04163 *                COMMAREA(DATE-CONVERSION-DATA)
04164 *                LENGTH  (DC-COMM-LENGTH)
04165 *            END-EXEC
           MOVE '."C                   ''   #00007828' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04166              MOVE DC-GREG-DATE-1-EDIT TO CREDMEO
04167              MOVE AL-UNNON            TO CREDMEA
04168              MOVE AL-SABOF            TO CREDMEDA
04169      ELSE
04170          MOVE AL-SADOF TO CREDMEA
04171                           CREDMEDA.
04173      IF CF-AR-SYSTEM-USED
04174          IF CF-AR-MONTH-END-DT = SPACES
04175              MOVE AL-SABOF           TO ARMEDA
04176          ELSE
04177              MOVE ' '                TO DC-OPTION-CODE
04178              MOVE CF-AR-MONTH-END-DT TO DC-BIN-DATE-1
04179              MOVE LINK-ELDATCV       TO PGM-NAME
04180              
      * EXEC CICS LINK
04181 *                PROGRAM (PGM-NAME)
04182 *                COMMAREA(DATE-CONVERSION-DATA)
04183 *                LENGTH  (DC-COMM-LENGTH)
04184 *            END-EXEC
           MOVE '."C                   ''   #00007846' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04185              MOVE DC-GREG-DATE-1-EDIT TO ARMEO
04186              MOVE AL-SABOF            TO ARMEDA
04187              MOVE CF-AR-LAST-EL860-DT TO DC-BIN-DATE-1
04188              MOVE LINK-ELDATCV       TO PGM-NAME
04189              
      * EXEC CICS LINK
04190 *                PROGRAM (PGM-NAME)
04191 *                COMMAREA(DATE-CONVERSION-DATA)
04192 *                LENGTH  (DC-COMM-LENGTH)
04193 *            END-EXEC
           MOVE '."C                   ''   #00007855' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04194              MOVE DC-GREG-DATE-1-EDIT TO AR860DTO
04195              MOVE AL-SANOF            TO AR860DTA
04196              IF  PI-PROCESSOR-ID = 'LGXX'
04197                  MOVE AL-UNNON            TO ARMEA
04198                  MOVE AL-UNNON            TO AR860DTA
04199              ELSE
04200                  MOVE AL-SANOF            TO ARMEA
04201      ELSE
04202          MOVE AL-SADOF TO ARMEA
04203                           ARMEDA
04204                           AR860DTA.
04206      MOVE -1                   TO COMPNMEL.
04207      MOVE CF-LAST-MAINT-BY     TO PI-UPDATE-BY.
04208      MOVE CF-LAST-MAINT-HHMMSS TO PI-UPDATE-HHMMSS.
04209      MOVE CF-CL-MAIL-TO-NAME   TO COMPNMEO.
04210      MOVE CF-COMPANY-ID        TO COMPIDO.
04211      MOVE CF-CL-IN-CARE-OF     TO INCAREO.
PEMMOD*    MOVE AL-UANOF             TO INCAREA.
04214      IF NOT CO-HAS-CLAS-IC-LIFE
04215          MOVE AL-SADOF TO MORTMEA
04216                           MORTMEDA
04217          GO TO 8100-SEND-INITIAL-MAP.
04219      MOVE PI-COMPANY-ID          TO CK-COMP-ID.
04220      MOVE 'N'                    TO CK-REC-TYPE.
04221      
      * EXEC CICS READ
04222 *        DATASET   ('ELCNTL')
04223 *        SET       (ADDRESS OF CONTROL-FILE)
04224 *        RIDFLD    (ELCNTL-KEY)
04225 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&"S        E          (   #00007884' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04227      MOVE ' '              TO DC-OPTION-CODE.
04228      MOVE CF-LAST-MAINT-DT TO DC-BIN-DATE-1.
04229      MOVE LINK-ELDATCV     TO PGM-NAME.
04230      
      * EXEC CICS LINK
04231 *        PROGRAM (PGM-NAME)
04232 *        COMMAREA(DATE-CONVERSION-DATA)
04233 *        LENGTH  (DC-COMM-LENGTH)
04234 *    END-EXEC.
           MOVE '."C                   ''   #00007892' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04236      MOVE CF-LAST-MAINT-BY       TO PI-MORTG-CO-UPDATE-BY.
04237      MOVE CF-LAST-MAINT-HHMMSS   TO PI-MORTG-CO-UPDATE-HHMMSS.
04239      IF CF-MORTG-MONTH-END-DT = SPACES
04240          MOVE AL-SABOF       TO MORTMEDA
04241      ELSE
04242          MOVE ' '                TO DC-OPTION-CODE
04243          MOVE CF-MORTG-MONTH-END-DT TO DC-BIN-DATE-1
04244          MOVE LINK-ELDATCV       TO PGM-NAME
04245          
      * EXEC CICS LINK
04246 *            PROGRAM (PGM-NAME)
04247 *            COMMAREA(DATE-CONVERSION-DATA)
04248 *            LENGTH  (DC-COMM-LENGTH)
04249 *        END-EXEC
           MOVE '."C                   ''   #00007905' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04250          MOVE DC-GREG-DATE-1-EDIT TO MORTMEO
04251          MOVE AL-UNNOF            TO MORTMEA
04252          MOVE AL-SABOF            TO MORTMEDA.
04254      GO TO 8100-SEND-INITIAL-MAP.
04256      EJECT
04257  7200-BUILD-INITIAL-MAP.
04258 *    BUILD MAP B
04260      MOVE LOW-VALUES    TO EL102BO.
04261      MOVE PI-COMPANY-ID TO CK-COMP-ID.
04262      MOVE '1'           TO CK-REC-TYPE.
04264      
      * EXEC CICS READ
04265 *        DATASET('ELCNTL')
04266 *        SET    (ADDRESS OF CONTROL-FILE)
04267 *        RIDFLD (ELCNTL-KEY)
04268 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&"S        E          (   #00007920' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04270      MOVE -1                       TO CARRCLL.
04272      IF CF-PAYMENT-APPROVAL-SW NOT = 'G' AND 'Y'
04273          MOVE 'N'                  TO APPROVO
04274          GO TO 7720-CONTINUE
04275      ELSE
04276          MOVE CF-PAYMENT-APPROVAL-SW
04277                                    TO APPROVO
04278          IF CF-PAYMENT-APPROVAL-SW = 'Y'
04279              GO TO 7720-CONTINUE.
04281      IF CF-LIFE-PAY-APP-LEVEL-1 NUMERIC
04282          MOVE CF-LIFE-PAY-APP-LEVEL-1  TO LLEV1O
04283          MOVE AL-UNNON                 TO LLEV1A.
04285      IF CF-LIFE-PAY-APP-LEVEL-2 NUMERIC
04286          MOVE CF-LIFE-PAY-APP-LEVEL-2  TO LLEV2O
04287          MOVE AL-UNNON                 TO LLEV2A.
04289      IF CF-LIFE-PAY-APP-LEVEL-3 NUMERIC
04290          MOVE CF-LIFE-PAY-APP-LEVEL-3  TO LLEV3O.
04291          MOVE AL-UNNON                 TO LLEV3A.
04293      IF CF-AH-PAY-APP-LEVEL-1  NUMERIC
04294          MOVE CF-AH-PAY-APP-LEVEL-1    TO ALEV1O
04295          MOVE AL-UNNON                 TO ALEV1A.
04297      IF CF-AH-PAY-APP-LEVEL-2  NUMERIC
04298          MOVE CF-AH-PAY-APP-LEVEL-2    TO ALEV2O
04299          MOVE AL-UNNON                 TO ALEV2A.
04301      IF CF-AH-PAY-APP-LEVEL-3 NUMERIC
04302          MOVE CF-AH-PAY-APP-LEVEL-3    TO ALEV3O
04303          MOVE AL-UNNON                 TO ALEV3A.
04305  7720-CONTINUE.
04307      MOVE CF-CARRIER-CONTROL-LEVEL TO CARRCLO.
04308      MOVE CF-CO-CLAIM-COUNTER      TO LSTCLMO.
04309      MOVE CF-CO-ARCHIVE-COUNTER    TO LSTARCHO.
04310      MOVE CF-STARTING-ARCH-NO      TO STARCHO.
04311      MOVE CF-CO-CHECK-COUNTER      TO LSTCHKO.
04312      MOVE CF-CO-CHECK-QUE-COUNTER  TO LSTQUEO.
04313      MOVE CF-CO-TOL-CLAIM          TO CLMTOLO.
04314      MOVE CF-CO-CLAIM-REJECT-SW    TO CLMREJO.
04316      IF CF-CLAIM-CUTOFF-DATE EQUAL SPACES OR LOW-VALUES
04317         MOVE SPACES TO COFFDTO
04318      ELSE
04319         MOVE CF-CLAIM-CUTOFF-DATE TO DC-BIN-DATE-1
04320         MOVE ' '                  TO DC-OPTION-CODE
04321         PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04322         IF NO-CONVERSION-ERROR
04323            MOVE DC-GREG-DATE-1-EDIT TO COFFDTO.
04325      IF CF-MAX-NUM-PMTS-CHECK NOT NUMERIC
04326         MOVE ZEROS                 TO PMTSCHKO
04327      ELSE
04328         MOVE CF-MAX-NUM-PMTS-CHECK TO PMTSCHKO.
04330      IF CF-CLAIM-PAID-THRU-TO EQUAL '1'
04331         MOVE '1'     TO THRUTOO
04332      ELSE
04333         MOVE SPACES  TO THRUTOO.
04335      IF CF-PRINT-ADDRESS-LABELS = ' '
04336          MOVE 'N'                        TO  ADDRLBLO
04337      ELSE
04338          MOVE CF-PRINT-ADDRESS-LABELS    TO  ADDRLBLO.
04340      IF CF-CLAIMS-CHECK-RECON-USER = ' '
04341          MOVE 'N'                        TO  RECONO
04342      ELSE
04343          MOVE CF-CLAIMS-CHECK-RECON-USER TO  RECONO.
04345      IF (CF-CLAIMS-LAST-PROCESS-DT EQUAL SPACES OR LOW-VALUES)
04346          MOVE SPACES                     TO  PROCDTO
04347      ELSE
04348          MOVE CF-CLAIMS-LAST-PROCESS-DT  TO  DC-BIN-DATE-1
04349          MOVE ' '                        TO  DC-OPTION-CODE
04350          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04351          IF NO-CONVERSION-ERROR
04352              MOVE DC-GREG-DATE-1-EDIT    TO  PROCDTO
04353          ELSE
04354              MOVE SPACES                 TO  PROCDTO.
04356      IF CF-CLAIMS-AUDIT-CHANGES = ' '
04357         MOVE 'N'                         TO AUDCLCGO
04358      ELSE
04359         MOVE CF-CLAIMS-AUDIT-CHANGES     TO AUDCLCGO.
04361      IF CF-CLAIMS-CREDIT-CARD-INDEX = ' '
04362         MOVE 'N'                         TO CRDCDINO
04363      ELSE
04364         MOVE CF-CLAIMS-CREDIT-CARD-INDEX TO CRDCDINO.
04366      IF CF-CLAIMS-LETTER-MAINT-DAYS NOT NUMERIC
04367         MOVE ZEROS                       TO LTRMTDTO
04368      ELSE
04369         MOVE CF-CLAIMS-LETTER-MAINT-DAYS TO LTRMTDTO.
04371      IF  CF-EXPERIENCE-RETENTION-AGE IS NUMERIC
04372              AND
04373          CF-EXPERIENCE-RETENTION-AGE GREATER THAN ZERO
04374              AND
04375          CF-EXPERIENCE-RETENTION-AGE LESS THAN 7
04376          MOVE CF-EXPERIENCE-RETENTION-AGE
04377                                          TO  EXPRETNO
04378      ELSE
04379          MOVE 2                          TO  EXPRETNO.
04381      IF  CF-CO-RESERVE-OPTION-SWITCH NOT EQUAL 'Y'
04382          MOVE 'N'                TO RSVOPSWO
04383                                     PI-OPTIONAL-RESERVE-SW
04384      ELSE
04385          MOVE CF-CO-RESERVE-OPTION-SWITCH
04386                                  TO RSVOPSWO
04387                                     PI-OPTIONAL-RESERVE-SW.
04389      IF  CF-CO-IBNR-LAG-MONTHS NOT NUMERIC
04390          MOVE ZEROS              TO IBLGMTHO
04391      ELSE
04392          MOVE CF-CO-IBNR-LAG-MONTHS
04393                                  TO IBLGMTHO.
04395      IF  CF-CO-CIDA-TABLE-DISCOUNT-PCT NOT NUMERIC
04396          MOVE ZEROS              TO CIDADISO
04397      ELSE
04398          MOVE CF-CO-CIDA-TABLE-DISCOUNT-PCT
04399                                  TO CIDADISO.
04401      IF  CF-CO-CRDB-TABLE-SELECTION NOT GREATER THAN SPACES
04402          MOVE SPACES             TO CRDTBLUO
04403      ELSE
04404          MOVE CF-CO-CRDB-TABLE-SELECTION
04405                                  TO CRDTBLUO.
04407      IF  CF-CO-CALCULATION-INTEREST NOT NUMERIC
04408          MOVE ZEROS              TO CALCINTO
04409      ELSE
04410          MOVE CF-CO-CALCULATION-INTEREST
04411                                  TO CALCINTO.
04413      IF  CF-CO-IBNR-LIFE-FACTOR NOT NUMERIC
04414          MOVE ZEROS              TO IBNRLFFO
04415      ELSE
04416          MOVE CF-CO-IBNR-LIFE-FACTOR
04417                                  TO IBNRLFFO.
04419      IF  CF-CO-IBNR-AH-FACTOR NOT NUMERIC
04420          MOVE ZEROS              TO IBNRAHFO
04421      ELSE
04422          MOVE CF-CO-IBNR-AH-FACTOR
04423                                  TO IBNRAHFO.
04425      IF CF-CO-OPTION-START-DATE = SPACES OR LOW-VALUES
04426          MOVE SPACES             TO OPTDTEO
04427      ELSE
04428          MOVE CF-CO-OPTION-START-DATE
04429                                  TO DC-BIN-DATE-1
04430          MOVE ' '                TO DC-OPTION-CODE
04431          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04432          IF  NO-CONVERSION-ERROR
04433              MOVE DC-GREG-DATE-1-EDIT
04434                                  TO OPTDTEO
04435          ELSE
04436              MOVE SPACES         TO OPTDTEO.
04438      MOVE AL-UNNON               TO CALCINTA
04439                                     CIDADISA
04440                                     IBLGMTHA
04441                                     IBNRAHFA
04442                                     IBNRLFFA.
04443      MOVE AL-UANON               TO RSVOPSWA
04444                                     CRDTBLUA
04445                                     OPTDTEA.
04447      IF PI-PROCESSOR-ID = 'LGXX'
04448          MOVE AL-UNNON          TO LSTCLMA
04449                                    LSTARCHA
04450                                    STARCHA
04451                                    LSTCHKA
04452                                    LSTQUEA
04453                                    PROCDTA.
04454      GO TO 8110-SEND-INITIAL-MAP.
04456      EJECT
04458  7300-BUILD-INITIAL-MAP.
04459 *    BUILD MAP C
04461      MOVE LOW-VALUES             TO EL102CO.
04462      MOVE PI-COMPANY-ID          TO CK-COMP-ID.
04463      MOVE '1'                    TO CK-REC-TYPE.
04464      
      * EXEC CICS READ
04465 *        DATASET   ('ELCNTL')
04466 *        SET       (ADDRESS OF CONTROL-FILE)
04467 *        RIDFLD    (ELCNTL-KEY)
04468 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&"S        E          (   #00008087' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04470      MOVE -1                     TO PREJCTL.
04472      MOVE CF-CERT-ACCESS-CONTROL TO CRTACSO.
04473      MOVE CF-CR-REM-TERM-CALC    TO REMTRMO.
04474      MOVE CF-CR-R78-METHOD       TO R78MTHO.
04476      IF CF-SYSTEM-D = '1'
04477         MOVE 'Y' TO DABILLO
04478      ELSE
04479         MOVE 'N' TO DABILLO.
04481      IF CF-SYSTEM-C = '1'
04482         MOVE 'Y' TO CONSYSO
04483      ELSE
04484         MOVE 'N' TO CONSYSO.
04486      IF CF-SOC-SEC-NO-SW = '1'
04487         MOVE 'Y' TO SSNO
04488      ELSE
04489         MOVE 'N' TO SSNO.
04491      IF  CF-MORTALITY-AGE-CALC-METHOD EQUAL '1' OR '2' OR '3'
04492          MOVE CF-MORTALITY-AGE-CALC-METHOD
04493                                  TO AGECALCO
04494      ELSE
04495          MOVE '1'                TO AGECALCO.
04497      IF CF-MEMBER-NO-SW = '1'
04498         MOVE 'Y' TO MEMBNOO
04499      ELSE
04500         MOVE 'N' TO MEMBNOO.
04502      MOVE CF-MAIL-PROCESSING     TO MAILO.
04504      IF CF-ALT-MORT-CODE NOT = SPACES
04505          MOVE CF-ALT-MORT-CODE   TO AMRTCDO.
04506      IF CF-CO-TOL-PREM NUMERIC
04507          MOVE CF-CO-TOL-PREM     TO QTECALO.
04508      IF CF-CO-TOL-REFUND NUMERIC
04509          MOVE CF-CO-TOL-REFUND   TO QTEREFO.
04510      IF CF-CO-OVR-SHT-AMT NUMERIC
04511         MOVE CF-CO-OVR-SHT-AMT   TO OVSAMTO.
04513      IF CF-CO-OVR-SHT-PCT NUMERIC
04514         MOVE CF-CO-OVR-SHT-PCT   TO OVSPCTO.
04516      IF CF-MIN-PREMIUM NUMERIC
04517          MOVE CF-MIN-PREMIUM     TO MINPREMO.
04518      IF CF-COMP-WRITE-OFF-AMT NUMERIC
04519          MOVE CF-COMP-WRITE-OFF-AMT  TO COMPWTEO.
04520      IF CF-MIN-TERM NUMERIC
04521          MOVE CF-MIN-AGE         TO MINAGEO.
04522      IF CF-MAX-TERM NUMERIC
04523         MOVE CF-MAX-TERM         TO MAXTERMO.
04525      MOVE CF-CO-PREM-REJECT-SW   TO PREJCTO.
04526      MOVE CF-CO-REF-REJECT-SW    TO REFREJO.
04528      IF CO-IS-PROCESSED-ON-QTR
04529         MOVE 'Q'                 TO PRCESSFO
04530        ELSE
04531         MOVE 'M'                 TO PRCESSFO.
04533      MOVE CF-DEFAULT-AGE         TO DAGEO.
04534      MOVE CF-DEFAULT-SEX         TO DSEXO.
04536      IF CF-DEFAULT-APR NUMERIC
04537          MOVE CF-DEFAULT-APR     TO DAPRO.
04539      IF CF-VALID-REM-TRM-OPTION
04540          MOVE CF-REM-TRM-CALC-OPTION
04541                                  TO TRMOPTNO.
04543      IF CF-JOINT-AGE-IS-INPUT
04544         MOVE 'Y'                 TO JAGEO
04545        ELSE
04546         MOVE 'N'                 TO JAGEO.
04548      IF CF-BIRTH-DATE-IS-INPUT
04549         MOVE 'Y'                 TO BINPUTO
04550        ELSE
04551         MOVE 'N'                 TO BINPUTO.
04553      IF CF-CO-TOL-PREM-PCT NUMERIC
04554         IF CF-CO-TOL-PREM-PCT NOT EQUAL ZEROS
04555             MOVE CF-CO-TOL-PREM-PCT  TO  PRMPCTO.
04557      IF CF-CO-TOL-REFUND-PCT NUMERIC
04558         IF CF-CO-TOL-REFUND-PCT NOT EQUAL ZEROS
04559             MOVE CF-CO-TOL-REFUND-PCT TO REFPCTO.
04561      IF CF-CO-TOL-CAP NUMERIC
04562         IF CF-CO-TOL-CAP NOT EQUAL ZEROS
04563             MOVE CF-CO-TOL-CAP     TO MTOLCAPO.
04565      IF CF-LAST-BATCH NOT = LOW-VALUES
04566          MOVE CF-LAST-BATCH-NO TO LSTBTCHO.
04568      MOVE CF-CR-CHECK-NO-METHOD    TO CMETHODO.
04569      MOVE CF-CAR-GROUP-ACCESS-CNTL TO CARGRPOO.
04571      IF CF-CONVERSION-DT NOT = LOW-VALUES
04572          MOVE ' '                 TO DC-OPTION-CODE
04573          MOVE CF-CONVERSION-DT    TO DC-BIN-DATE-1
04574          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04575          MOVE DC-GREG-DATE-1-EDIT TO CONDTEO.
04577      MOVE CF-CO-ST-CALL-RPT-CNTL             TO STCNTLO.
04579      IF  CF-CREDIT-START-ARCH-NUM NUMERIC
04580          MOVE CF-CREDIT-START-ARCH-NUM
04581                                  TO ARCHSTO
04582      ELSE
04583          MOVE ZEROS              TO ARCHSTO
04584          MOVE +8                 TO ARCHSTL
04585          MOVE AL-UNNON           TO ARCHSTA.
04587      IF  CF-CREDIT-LAST-ARCH-NUM NUMERIC
04588          MOVE CF-CREDIT-LAST-ARCH-NUM
04589                                  TO ARCHLSTO
04590      ELSE
04591          MOVE ZEROS              TO ARCHLSTO
04592          MOVE +8                 TO ARCHLSTL
04593          MOVE AL-UNNON           TO ARCHLSTA.
04595      IF  CF-CREDIT-ARCH-PURGE-YR NUMERIC
04596          MOVE AL-UNNON           TO ARCHPYRA
04597          MOVE CF-CREDIT-ARCH-PURGE-YR
04598                                  TO ARCHPYRO.
04600      IF CF-CR-CHECK-COUNT NOT = LOW-VALUES
04601          MOVE CF-CR-CHECK-COUNTER TO CKCOUNTO.
04603      IF CF-CR-CHECK-QUE-COUNT NOT = LOW-VALUES
04604          MOVE CF-CR-CHECK-QUE-COUNTER TO QUCOUNTO.
04606      IF  CF-CR-PRINT-ADDRESS-LABELS = 'Y' OR 'N'
04607          MOVE CF-CR-PRINT-ADDRESS-LABELS
04608                                 TO CRLABELO
04609          MOVE AL-UANON          TO CRLABELA
04610      ELSE
04611          MOVE AL-UANON          TO CRLABELA
04612          MOVE 'N'               TO CRLABELO.
04614      IF PI-PROCESSOR-ID = 'LGXX'
04615          MOVE AL-UNNON          TO CKCOUNTA
04616                                    LSTBTCHA
04617                                    QUCOUNTA
04618                                    ARCHSTA
04619                                    ARCHLSTA
04620      ELSE
04621          MOVE AL-PANOF          TO CARGRPOA
04622                                    QUCOUNTA
04623                                    CRTACSA
04624                                    ARCHSTA
04625                                    ARCHLSTA.
04627  7300-CONT.
04628      GO TO 8120-SEND-INITIAL-MAP.
04630      EJECT
04631  7400-BUILD-INITIAL-MAP.
04632 *    BUILD MAP D
04634      MOVE LOW-VALUES             TO EL102DO.
04635      MOVE PI-COMPANY-ID          TO CK-COMP-ID.
04636      MOVE 'N'                    TO CK-REC-TYPE.
04637      
      * EXEC CICS READ
04638 *        DATASET   ('ELCNTL')
04639 *        SET       (ADDRESS OF CONTROL-FILE)
04640 *        RIDFLD    (ELCNTL-KEY)
04641 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&"S        E          (   #00008225' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04643      MOVE ' '              TO DC-OPTION-CODE.
04644      MOVE CF-LAST-MAINT-DT TO DC-BIN-DATE-1.
04645      MOVE LINK-ELDATCV     TO PGM-NAME.
04646      
      * EXEC CICS LINK
04647 *        PROGRAM (PGM-NAME)
04648 *        COMMAREA(DATE-CONVERSION-DATA)
04649 *        LENGTH  (DC-COMM-LENGTH)
04650 *    END-EXEC.
           MOVE '."C                   ''   #00008233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04652      MOVE CF-LAST-MAINT-BY       TO PI-MORTG-CO-UPDATE-BY.
04653      MOVE CF-LAST-MAINT-HHMMSS   TO PI-MORTG-CO-UPDATE-HHMMSS.
04654      MOVE CF-LAST-MAINT-BY       TO LSTUSERO.
04655      MOVE DC-GREG-DATE-1-EDIT    TO LSTDATEO.
04656      MOVE CF-LAST-MAINT-HHMMSS   TO TIME-IN.
04657      MOVE TIME-OUT               TO LSTTMEO.
04658      MOVE -1                     TO MIBSYML.
04659      MOVE CF-MORTG-ALT-MORT-CODE TO DAMRTCDO.
04660      MOVE CF-MORTG-MIB-VERSION   TO MIBVERO.
04661      MOVE CF-MORTG-MIB-LNAME-SEARCH TO WS-MIB-LNAME-SEARCH.
04662      MOVE CF-MORTG-ALP-LNAME-SEARCH TO WS-ALP-LNAME-SEARCH.
04663      MOVE WS-LNAME-SEARCH-CNTL   TO LNMESERO.
04664      MOVE CF-MORTG-MIB-FNAME-SEARCH TO WS-MIB-FNAME-SEARCH.
04665      MOVE CF-MORTG-ALP-FNAME-SEARCH TO WS-ALP-FNAME-SEARCH.
04666      MOVE WS-FNAME-SEARCH-CNTL   TO FNMESERO.
04667      MOVE CF-MORTG-MIB-MNAME-SEARCH TO WS-MIB-MNAME-SEARCH.
04668      MOVE CF-MORTG-ALP-MNAME-SEARCH TO WS-ALP-MNAME-SEARCH.
04669      MOVE WS-MNAME-SEARCH-CNTL   TO MNMESERO.
04670      MOVE CF-MORTG-MIB-BDATE-SEARCH TO WS-MIB-BDATE-SEARCH.
04671      MOVE CF-MORTG-ALP-BDATE-SEARCH TO WS-ALP-BDATE-SEARCH.
04672      MOVE WS-BDATE-SEARCH-CNTL   TO BDATEO.
04673      MOVE CF-MORTG-MIB-BSTATE-SEARCH TO BSTATEO.
04674      MOVE CF-MORTG-MIB-BSTATE-SEARCH TO WS-MIB-BSTATE-SEARCH.
04675      MOVE CF-MORTG-ALP-BSTATE-SEARCH TO WS-ALP-BSTATE-SEARCH.
04676      MOVE WS-BSTATE-SEARCH-CNTL  TO BSTATEO.
04677      MOVE CF-MORTG-MIB-RSTATE-SEARCH TO WS-MIB-RSTATE-SEARCH.
04678      MOVE CF-MORTG-ALP-RSTATE-SEARCH TO WS-ALP-RSTATE-SEARCH.
04679      MOVE WS-RSTATE-SEARCH-CNTL  TO RSTATEO.
04680      MOVE CF-MORTG-MIB-COMM      TO MIBCOMMO.
04681      MOVE CF-MORTG-MIB-TERM      TO MIBTERMO.
04682      MOVE CF-MORTG-MIB-COMPANY-SYMBOL TO MIBSYMO.
04683      MOVE CF-MORTG-BILL-GROUPING-CODE TO GRPSWO.
04684      MOVE CF-RATE-DEV-AUTHORIZATION   TO RATESWO.
04686      IF CF-MORTG-MIB-COMM-DEST
04687          MOVE 'Y' TO MIBCSWO
04688          MOVE 'N' TO MIBTSWO
04689      ELSE
04690      IF CF-MORTG-MIB-TERM-DEST
04691          MOVE 'N' TO MIBCSWO
04692          MOVE 'Y' TO MIBTSWO
04693      ELSE
04694          MOVE 'N' TO MIBCSWO
04695                      MIBTSWO.
04697      MOVE CF-MORTG-ACCESS-CONTROL TO MGCNTLO.
04699      IF  CF-MORTG-SOLICITATION-NUM NUMERIC
04700          MOVE CF-MORTG-SOLICITATION-NUM
04701                                  TO REFNOO
04702      ELSE
04703          MOVE ZEROS              TO REFNOO.
04705      IF  CF-ASSIGN-POLICY-NO-SW EQUAL 'Y' OR 'N'
04706          MOVE CF-ASSIGN-POLICY-NO-SW
04707                                  TO AUTOREFO
04708      ELSE
04709          MOVE 'N'                TO AUTOREFO.
04711      IF  CF-MORTG-LOAN-SHIFT-IND EQUAL 'L' OR 'R'
04712          MOVE CF-MORTG-LOAN-SHIFT-IND
04713                                  TO SHFTLNOO
04714      ELSE
04715          MOVE 'L'                TO SHFTLNOO.
04717      IF  CF-MP-CHECK-NO-METHOD EQUAL '1' OR '2' OR '3'
04718          MOVE CF-MP-CHECK-NO-METHOD
04719                                  TO CHKPMTHO
04720      ELSE
04721          MOVE '2'                TO CHKPMTHO.
04723      IF  CF-MP-RECON-USE-IND EQUAL 'Y' OR 'N'
04724          MOVE CF-MP-RECON-USE-IND TO MRECONO
04725      ELSE
04726          MOVE 'N'                TO MRECONO.
04728      IF  CF-MP-REPORT-LANGUAGE-IND EQUAL 'E' OR 'F'
04729          MOVE CF-MP-REPORT-LANGUAGE-IND
04730                                  TO RPTLANGO
04731      ELSE
04732          MOVE 'E'                TO RPTLANGO.
04734      IF  CF-MP-POLICY-LINKAGE-IND EQUAL 'Y' OR 'N'
04735          MOVE CF-MP-POLICY-LINKAGE-IND
04736                                  TO PLCYLNKO
04737      ELSE
04738          MOVE 'N'                TO PLCYLNKO.
04740      MOVE CF-MORTG-START-ARCH-NUM TO STARCHNO.
04741      MOVE CF-MORTG-CURRENT-ARCH-NUM
04742                                   TO ARCHNBRO.
04743      MOVE CF-MORTG-LABEL-CONTROL  TO LABCNTLO.
04744      MOVE CF-MORTG-BILL-CYCLE (1) TO BCYCLE1O.
04745      MOVE CF-MORTG-BILL-CYCLE (2) TO BCYCLE2O.
04746      MOVE CF-MORTG-BILL-CYCLE (3) TO BCYCLE3O.
04747      MOVE CF-MORTG-BILL-CYCLE (4) TO BCYCLE4O.
04748      MOVE CF-MORTG-BILL-CYCLE (5) TO BCYCLE5O.
04750      IF CF-MORTG-CONVERSION-DATE NOT = LOW-VALUES
04751          MOVE ' '                 TO DC-OPTION-CODE
04752          MOVE CF-MORTG-CONVERSION-DATE TO DC-BIN-DATE-1
04753          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04754          MOVE DC-GREG-DATE-1-EDIT TO CNVDTEO.
04756      IF CF-MORTG-RATE-FILE-MAINT-DATE NOT = LOW-VALUES
04757          MOVE ' '                    TO DC-OPTION-CODE
04758          MOVE CF-MORTG-RATE-FILE-MAINT-DATE TO DC-BIN-DATE-1
04759          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04760          MOVE DC-GREG-DATE-1-EDIT    TO RATFILMO.
04762      IF CF-MORTG-RATE-FILE-CREAT-DATE NOT = LOW-VALUES
04763          MOVE ' '                     TO DC-OPTION-CODE
04764          MOVE CF-MORTG-RATE-FILE-CREAT-DATE TO DC-BIN-DATE-1
04765          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04766          MOVE DC-GREG-DATE-1-EDIT     TO RATFILCO.
04768      IF CF-MORTG-PROD-FILE-MAINT-DATE NOT = LOW-VALUES
04769          MOVE ' '                      TO DC-OPTION-CODE
04770          MOVE CF-MORTG-PROD-FILE-MAINT-DATE TO DC-BIN-DATE-1
04771          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04772          MOVE DC-GREG-DATE-1-EDIT      TO PRDMSTMO.
04774      IF CF-MORTG-PROD-FILE-CREAT-DATE NOT = LOW-VALUES
04775          MOVE ' '                      TO DC-OPTION-CODE
04776          MOVE CF-MORTG-PROD-FILE-CREAT-DATE TO DC-BIN-DATE-1
04777          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04778          MOVE DC-GREG-DATE-1-EDIT      TO PRDMSTCO.
04780      IF CF-MORTG-CHECK-NO-COUNTER NOT = LOW-VALUES
04781          MOVE CF-MORTG-CHECK-NO-COUNTER TO CHKCNTRO.
04783      IF CF-MORTG-CHECK-QUEUE-COUNTER NOT = LOW-VALUES
04784          MOVE CF-MORTG-CHECK-QUEUE-COUNTER TO QUECNTRO.
04786      MOVE PI-COMPANY-ID          TO CK-COMP-ID.
04787      MOVE '1'                    TO CK-REC-TYPE.
04788      
      * EXEC CICS READ
04789 *        DATASET   ('ELCNTL')
04790 *        SET       (ADDRESS OF CONTROL-FILE)
04791 *        RIDFLD    (ELCNTL-KEY)
04792 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&"S        E          (   #00008356' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04794      MOVE CF-LAST-MAINT-BY       TO PI-UPDATE-BY.
04795      MOVE CF-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.
04796      IF REIN-TABLES-ARE-USED
04797         MOVE 'Y' TO DRESURO
04798        ELSE
04799         MOVE 'N' TO DRESURO.
04801      MOVE CF-CAR-GROUP-ACCESS-CNTL TO CMPCNTLO.
04803      IF CF-REINSURANCE-TAB-MAINT-DT = LOW-VALUES
04804          NEXT SENTENCE
04805      ELSE
04806          MOVE ' '                         TO DC-OPTION-CODE
04807          MOVE CF-REINSURANCE-TAB-MAINT-DT TO DC-BIN-DATE-1
04808          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04809          MOVE DC-GREG-DATE-1-EDIT         TO RENTBLMO.
04811      IF CF-REINSURANCE-TAB-CREATE-DT = LOW-VALUES
04812          NEXT SENTENCE
04813      ELSE
04814          MOVE ' '                          TO DC-OPTION-CODE
04815          MOVE CF-REINSURANCE-TAB-CREATE-DT TO DC-BIN-DATE-1
04816          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04817          MOVE DC-GREG-DATE-1-EDIT          TO RENTBLCO.
04819      IF CF-COMPENSATION-MSTR-MAINT-DT = LOW-VALUES
04820          NEXT SENTENCE
04821      ELSE
04822          MOVE ' '                           TO DC-OPTION-CODE
04823          MOVE CF-COMPENSATION-MSTR-MAINT-DT TO DC-BIN-DATE-1
04824          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04825          MOVE DC-GREG-DATE-1-EDIT           TO COMMSTMO.
04827      IF CF-COMPENSATION-MSTR-CREATE-DT = LOW-VALUES
04828          NEXT SENTENCE
04829      ELSE
04830          MOVE ' '                            TO DC-OPTION-CODE
04831          MOVE CF-COMPENSATION-MSTR-CREATE-DT TO DC-BIN-DATE-1
04832          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04833          MOVE DC-GREG-DATE-1-EDIT            TO COMMSTCO.
04835      IF PI-PROCESSOR-ID = 'LGXX'
04836          MOVE AL-UNNON          TO RATFILMA
04837                                    STARCHNA
04838                                    ARCHNBRA
04839                                    LABCNTLA
04840                                    REFNOA
04841                                    BCYCLE1A
04842                                    BCYCLE2A
04843                                    BCYCLE3A
04844                                    BCYCLE4A
04845                                    BCYCLE5A
04846                                    PRDMSTMA
04847                                    RENTBLMA
04848                                    COMMSTMA
04849                                    CHKCNTRA
04850                                    QUECNTRA
04852      ELSE
04853          MOVE AL-PANOF          TO RATFILMA
04854                                    MGCNTLA
04855                                    CMPCNTLA
04856                                    STARCHNA
04857                                    ARCHNBRA
04858                                    LABCNTLA
04859                                    REFNOA
04860                                    BCYCLE1A
04861                                    BCYCLE2A
04862                                    BCYCLE3A
04863                                    BCYCLE4A
04864                                    BCYCLE5A
04865                                    PRDMSTMA
04866                                    RENTBLMA
04867                                    COMMSTMA
04868                                    CHKCNTRA
04869                                    QUECNTRA.
04871      MOVE AL-UANON              TO AUTOREFA
04872                                    SHFTLNOA
04873                                    CHKPMTHA
04874                                    MRECONA
04875                                    RPTLANGA
04876                                    PLCYLNKA.
04878      IF PI-USER-ALMIGHTY-YES
04879          MOVE AL-UNNON          TO STARCHNA
04880                                    ARCHNBRA.
04882  7400-CONT.
04883      GO TO 8130-SEND-INITIAL-MAP.
04885  7500-BUILD-INITIAL-MAP.
04886 *    BUILD MAP E
04888      MOVE LOW-VALUES             TO EL102EO.
04889      MOVE PI-COMPANY-ID          TO CK-COMP-ID.
04890      MOVE '1'                    TO CK-REC-TYPE.
04891      
      * EXEC CICS READ
04892 *        DATASET   ('ELCNTL')
04893 *        SET       (ADDRESS OF CONTROL-FILE)
04894 *        RIDFLD    (ELCNTL-KEY)
04895 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE '&"S        E          (   #00008446' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04897      MOVE -1                     TO RTEFILML.
04899      IF CF-RATES-FILE-MAINT-DT = LOW-VALUES OR SPACES
04900          NEXT SENTENCE
04901      ELSE
04902          MOVE ' '                    TO DC-OPTION-CODE
04903          MOVE CF-RATES-FILE-MAINT-DT TO DC-BIN-DATE-1
04904          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04905          MOVE DC-GREG-DATE-1-EDIT    TO RTEFILMO.
04907      IF CF-RATES-FILE-CREATE-DT = LOW-VALUES OR SPACES
04908          NEXT SENTENCE
04909      ELSE
04910          MOVE ' '                     TO DC-OPTION-CODE
04911          MOVE CF-RATES-FILE-CREATE-DT TO DC-BIN-DATE-1
04912          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04913          MOVE DC-GREG-DATE-1-EDIT     TO RTEFILCO.
04915      IF CF-COMMISSION-TAB-MAINT-DT = LOW-VALUES OR SPACES
04916          NEXT SENTENCE
04917      ELSE
04918          MOVE ' '                        TO DC-OPTION-CODE
04919          MOVE CF-COMMISSION-TAB-MAINT-DT TO DC-BIN-DATE-1
04920          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04921          MOVE DC-GREG-DATE-1-EDIT        TO COMTABMO.
04923      IF CF-COMMISSION-TAB-CREATE-DT = LOW-VALUES OR SPACES
04924          NEXT SENTENCE
04925      ELSE
04926          MOVE ' '                         TO DC-OPTION-CODE
04927          MOVE CF-COMMISSION-TAB-CREATE-DT TO DC-BIN-DATE-1
04928          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04929          MOVE DC-GREG-DATE-1-EDIT         TO COMTABCO.
04931      IF CF-ACCOUNT-MSTR-MAINT-DT = LOW-VALUES OR SPACES
04932          NEXT SENTENCE
04933      ELSE
04934          MOVE ' '                      TO DC-OPTION-CODE
04935          MOVE CF-ACCOUNT-MSTR-MAINT-DT TO DC-BIN-DATE-1
04936          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04937          MOVE DC-GREG-DATE-1-EDIT      TO ACTMSTMO.
04939      IF CF-ACCOUNT-MSTR-CREATE-DT = LOW-VALUES OR SPACES
04940          NEXT SENTENCE
04941      ELSE
04942          MOVE ' '                       TO DC-OPTION-CODE
04943          MOVE CF-ACCOUNT-MSTR-CREATE-DT TO DC-BIN-DATE-1
04944          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04945          MOVE DC-GREG-DATE-1-EDIT       TO ACTMSTCO.
04947      IF CF-REINSURANCE-TAB-MAINT-DT = LOW-VALUES OR SPACES
04948          NEXT SENTENCE
04949      ELSE
04950          MOVE ' '                         TO DC-OPTION-CODE
04951          MOVE CF-REINSURANCE-TAB-MAINT-DT TO DC-BIN-DATE-1
04952          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04953          MOVE DC-GREG-DATE-1-EDIT         TO RENTABMO.
04955      IF CF-REINSURANCE-TAB-CREATE-DT = LOW-VALUES OR SPACES
04956          NEXT SENTENCE
04957      ELSE
04958          MOVE ' '                          TO DC-OPTION-CODE
04959          MOVE CF-REINSURANCE-TAB-CREATE-DT TO DC-BIN-DATE-1
04960          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04961          MOVE DC-GREG-DATE-1-EDIT          TO RENTABCO.
04963      IF CF-COMPENSATION-MSTR-MAINT-DT = LOW-VALUES OR SPACES
04964          NEXT SENTENCE
04965      ELSE
04966          MOVE ' '                           TO DC-OPTION-CODE
04967          MOVE CF-COMPENSATION-MSTR-MAINT-DT TO DC-BIN-DATE-1
04968          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04969          MOVE DC-GREG-DATE-1-EDIT           TO CMPMSTMO.
04971      IF CF-COMPENSATION-MSTR-CREATE-DT = LOW-VALUES OR SPACES
04972          NEXT SENTENCE
04973      ELSE
04974          MOVE ' '                            TO DC-OPTION-CODE
04975          MOVE CF-COMPENSATION-MSTR-CREATE-DT TO DC-BIN-DATE-1
04976          PERFORM 8000-CONVERT-DATE THRU 8000-EXIT
04977          MOVE DC-GREG-DATE-1-EDIT            TO CMPMSTCO.
04979      IF PI-PROCESSOR-ID = 'LGXX' OR 'PEMA'
04980          MOVE AL-UNNON          TO RTEFILMA
04981                                    COMTABMA
04982                                    ACTMSTMA
04983                                    RENTABMA
04984                                    CMPMSTMA.
04987  7500-CONT.
04988      GO TO 8140-SEND-INITIAL-MAP.
04990      EJECT
04991  8000-CONVERT-DATE.
04992      MOVE LINK-ELDATCV TO PGM-NAME.
04993      
      * EXEC CICS LINK
04994 *        PROGRAM    (PGM-NAME)
04995 *        COMMAREA   (DATE-CONVERSION-DATA)
04996 *        LENGTH     (DC-COMM-LENGTH)
04997 *    END-EXEC.
           MOVE '."C                   ''   #00008533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04999  8000-EXIT.
05000      EXIT.
05001      EJECT
05003  8100-SEND-INITIAL-MAP.
05004      MOVE SAVE-DATE            TO RUNDTEO.
05005      MOVE EIBTIME              TO TIME-IN.
05006      MOVE TIME-OUT             TO RUNTIMEO.
PEMTST     IF PI-COMPANY-ID NOT = 'CID'
PEMTST        MOVE AL-UANON          TO COMPNMEA
PEMTST                                  INCAREA
                                        ADDR1A
                                        ADDR2A
                                        CITYSTA
                                        ZIPCODEA
           END-IF
05008      MOVE PI-COMPANY-ID        TO COAO.
PEMTST     MOVE PI-PROCESSOR-ID      TO USERIDAO
05009      MOVE PI-LIFE-OVERRIDE-L2  TO WS-COV-DESC-2.
05010      MOVE WS-COV-DESCRIPTION   TO LFDESCO.
05011      MOVE PI-LIFE-OVERRIDE-L1  TO LFOVR1O.
05012      MOVE PI-LIFE-OVERRIDE-L2  TO LFOVR2O.
05013      MOVE PI-LIFE-OVERRIDE-L6  TO LFOVR6O.
05014      MOVE PI-LIFE-OVERRIDE-L12 TO LFOVR12O.
05015      MOVE PI-AH-OVERRIDE-L2    TO WS-COV-DESC-2.
05016      MOVE WS-COV-DESCRIPTION   TO AHDESCO.
05017      MOVE PI-AH-OVERRIDE-L1    TO AHOVR1O.
05018      MOVE PI-AH-OVERRIDE-L2    TO AHOVR2O.
05019      MOVE PI-AH-OVERRIDE-L6    TO AHOVR6O.
05020      MOVE PI-AH-OVERRIDE-L12   TO AHOVR12O.
05022      MOVE EMI-MESSAGE-AREA (1) TO ERRMSGO.
05024      
      * EXEC CICS SEND
05025 *        MAP   (MAP-NAME)
05026 *        MAPSET(MAPSET-NAME)
05027 *        FROM  (EL102AO)
05028 *        ERASE
05029 *        CURSOR
05030 *    END-EXEC.
           MOVE LENGTH OF EL102AO TO DFHEIV12
           MOVE -1 TO DFHEIV11
           MOVE '8$     CT  E    H L F ,   #00008568' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL102AO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05032      MOVE '102A' TO PI-CURRENT-SCREEN-NO.
05034      GO TO 9100-RETURN-TRAN.
05036      EJECT
05037  8110-SEND-INITIAL-MAP.
05038      MOVE PI-COMPANY-ID        TO COBO.
PEMTST     MOVE PI-PROCESSOR-ID      TO USERIDBO
05039      MOVE SAVE-DATE            TO BRUNDTEO.
05040      MOVE EIBTIME              TO TIME-IN.
05041      MOVE TIME-OUT             TO BRUNTMEO.
05042      MOVE EMI-MESSAGE-AREA (1) TO BERMSG1O.
05043      
      * EXEC CICS SEND
05044 *        MAP   (MAP-NAME)
05045 *        MAPSET(MAPSET-NAME)
05046 *        FROM  (EL102BO)
05047 *        ERASE
05048 *        CURSOR
05049 *    END-EXEC.
           MOVE LENGTH OF EL102BO TO DFHEIV12
           MOVE -1 TO DFHEIV11
           MOVE '8$     CT  E    H L F ,   #00008585' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL102BO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05051      MOVE '102B' TO PI-CURRENT-SCREEN-NO.
05053      GO TO 9100-RETURN-TRAN.
05055      EJECT
05056  8120-SEND-INITIAL-MAP.
05057      MOVE PI-COMPANY-ID        TO COCO
05058                                   COEO.
PEMTST     MOVE PI-PROCESSOR-ID      TO USERIDCO
PEMTST                                  USERIDEO
05059      MOVE SAVE-DATE            TO CRUNDTEO.
05060      MOVE EIBTIME              TO TIME-IN.
05061      MOVE TIME-OUT             TO CRUNTMEO.
05062      MOVE EMI-MESSAGE-AREA (1) TO CERMSG1O.
05063      
      * EXEC CICS SEND
05064 *        MAP   (MAP-NAME)
05065 *        MAPSET(MAPSET-NAME)
05066 *        FROM  (EL102CO)
05067 *        ERASE
05068 *        CURSOR
05069 *    END-EXEC.
           MOVE LENGTH OF EL102CO TO DFHEIV12
           MOVE -1 TO DFHEIV11
           MOVE '8$     CT  E    H L F ,   #00008604' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL102CO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05071      MOVE '102C' TO PI-CURRENT-SCREEN-NO.
05073      GO TO 9100-RETURN-TRAN.
05075      EJECT
05076  8130-SEND-INITIAL-MAP.
05077      MOVE PI-COMPANY-ID        TO CODO.
PEMTST     MOVE PI-PROCESSOR-ID      TO USERIDDO
05078      MOVE SAVE-DATE            TO DRUNDTEO.
05079      MOVE EIBTIME              TO TIME-IN.
05080      MOVE TIME-OUT             TO DRUNTMEO.
05081      MOVE EMI-MESSAGE-AREA (1) TO DERMSG1O.
05082      
      * EXEC CICS SEND
05083 *        MAP   (MAP-NAME)
05084 *        MAPSET(MAPSET-NAME)
05085 *        FROM  (EL102DO)
05086 *        ERASE
05087 *        CURSOR
05088 *    END-EXEC.
           MOVE LENGTH OF EL102DO TO DFHEIV12
           MOVE -1 TO DFHEIV11
           MOVE '8$     CT  E    H L F ,   #00008621' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL102DO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05090      MOVE '102D' TO PI-CURRENT-SCREEN-NO.
05092      GO TO 9100-RETURN-TRAN.
05094      EJECT
05096  8140-SEND-INITIAL-MAP.
05097      MOVE PI-COMPANY-ID        TO COEO.
PEMTST     MOVE PI-PROCESSOR-ID      TO USERIDEO
05098      MOVE SAVE-DATE            TO ERUNDTEO.
05099      MOVE EIBTIME              TO TIME-IN.
05100      MOVE TIME-OUT             TO ERUNTMEO.
05101      MOVE EMI-MESSAGE-AREA (1) TO EERMSG1O.
05102      
      * EXEC CICS SEND
05103 *        MAP   (MAP-NAME)
05104 *        MAPSET(MAPSET-NAME)
05105 *        FROM  (EL102EO)
05106 *        ERASE
05107 *        CURSOR
05108 *    END-EXEC.
           MOVE LENGTH OF EL102EO TO DFHEIV12
           MOVE -1 TO DFHEIV11
           MOVE '8$     CT  E    H L F ,   #00008638' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL102EO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05109      MOVE '102E' TO PI-CURRENT-SCREEN-NO.
05111      GO TO 9100-RETURN-TRAN.
05113      EJECT
05114  8200-SEND-DATAONLY.
PEMTST     IF PI-COMPANY-ID NOT = 'CID'
PEMTST        MOVE AL-UANON          TO COMPNMEA
PEMTST                                  INCAREA
                                        ADDR1A
                                        ADDR2A
                                        CITYSTA
                                        ZIPCODEA
           END-IF
05115      MOVE PI-COMPANY-ID        TO COAO.
PEMTST     MOVE PI-PROCESSOR-ID      TO USERIDAO
05116      MOVE SAVE-DATE            TO RUNDTEO.
05117      MOVE EIBTIME              TO TIME-IN.
05118      MOVE TIME-OUT             TO RUNTIMEO.
05120      MOVE PI-LIFE-OVERRIDE-L2  TO WS-COV-DESC-2.
05121      MOVE WS-COV-DESCRIPTION   TO LFDESCO.
05122      MOVE PI-LIFE-OVERRIDE-L1  TO LFOVR1O.
05123      MOVE PI-LIFE-OVERRIDE-L2  TO LFOVR2O.
05124      MOVE PI-LIFE-OVERRIDE-L6  TO LFOVR6O.
05125      MOVE PI-LIFE-OVERRIDE-L12 TO LFOVR12O.
05126      MOVE PI-AH-OVERRIDE-L2    TO WS-COV-DESC-2.
05127      MOVE WS-COV-DESCRIPTION   TO AHDESCO.
05128      MOVE PI-AH-OVERRIDE-L1    TO AHOVR1O.
05129      MOVE PI-AH-OVERRIDE-L2    TO AHOVR2O.
05130      MOVE PI-AH-OVERRIDE-L6    TO AHOVR6O.
05131      MOVE PI-AH-OVERRIDE-L12   TO AHOVR12O.
05133      MOVE EMI-MESSAGE-AREA (1) TO ERRMSGO.
05135      
      * EXEC CICS SEND
05136 *        MAP   (MAP-NAME)
05137 *        MAPSET(MAPSET-NAME)
05138 *        FROM  (EL102AO)
05139 *        DATAONLY
05140 *        CURSOR
05141 *    END-EXEC.
           MOVE LENGTH OF EL102AO TO DFHEIV12
           MOVE -1 TO DFHEIV11
           MOVE '8$D    CT       H L F ,   #00008675' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL102AO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05143      MOVE '102A' TO PI-CURRENT-SCREEN-NO.
05145      GO TO 9100-RETURN-TRAN.
05147      EJECT
05148  8210-SEND-DATAONLY.
05149      MOVE PI-COMPANY-ID        TO COBO.
PEMTST     MOVE PI-PROCESSOR-ID      TO USERIDBO
05150      MOVE SAVE-DATE            TO BRUNDTEO.
05151      MOVE EIBTIME              TO TIME-IN.
05152      MOVE TIME-OUT             TO BRUNTMEO.
05153      MOVE EMI-MESSAGE-AREA (1) TO BERMSG1O.
05154      MOVE EMI-MESSAGE-AREA (2) TO BERMSG2O.
05155      MOVE EMI-MESSAGE-AREA (3) TO BERMSG3O.
05156      
      * EXEC CICS SEND
05157 *        MAP   (MAP-NAME)
05158 *        MAPSET(MAPSET-NAME)
05159 *        FROM  (EL102BO)
05160 *        DATAONLY
05161 *        CURSOR
05162 *    END-EXEC.
           MOVE LENGTH OF EL102BO TO DFHEIV12
           MOVE -1 TO DFHEIV11
           MOVE '8$D    CT       H L F ,   #00008694' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL102BO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05164      MOVE '102B' TO PI-CURRENT-SCREEN-NO.
05166      GO TO 9100-RETURN-TRAN.
05168      EJECT
05169  8220-SEND-DATAONLY.
05170      MOVE PI-COMPANY-ID        TO COCO.
PEMTST     MOVE PI-PROCESSOR-ID      TO USERIDCO
05171      MOVE SAVE-DATE            TO CRUNDTEO.
05172      MOVE EIBTIME              TO TIME-IN.
05173      MOVE TIME-OUT             TO CRUNTMEO.
05174      MOVE EMI-MESSAGE-AREA (1) TO CERMSG1O.
05175      
      * EXEC CICS SEND
05176 *        MAP   (MAP-NAME)
05177 *        MAPSET(MAPSET-NAME)
05178 *        FROM  (EL102CO)
05179 *        DATAONLY
05180 *        CURSOR
05181 *    END-EXEC.
           MOVE LENGTH OF EL102CO TO DFHEIV12
           MOVE -1 TO DFHEIV11
           MOVE '8$D    CT       H L F ,   #00008711' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL102CO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05183      MOVE '102C' TO PI-CURRENT-SCREEN-NO.
05185      GO TO 9100-RETURN-TRAN.
05187      EJECT
05188  8230-SEND-DATAONLY.
05189      MOVE PI-COMPANY-ID        TO CODO.
PEMTST     MOVE PI-PROCESSOR-ID      TO USERIDDO
05190      MOVE SAVE-DATE            TO DRUNDTEO.
05191      MOVE EIBTIME              TO TIME-IN.
05192      MOVE TIME-OUT             TO DRUNTMEO.
05193      MOVE EMI-MESSAGE-AREA (1) TO DERMSG1O.
05194      
      * EXEC CICS SEND
05195 *        MAP   (MAP-NAME)
05196 *        MAPSET(MAPSET-NAME)
05197 *        FROM  (EL102DO)
05198 *        DATAONLY
05199 *        CURSOR
05200 *    END-EXEC.
           MOVE LENGTH OF EL102DO TO DFHEIV12
           MOVE -1 TO DFHEIV11
           MOVE '8$D    CT       H L F ,   #00008728' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL102DO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05202      MOVE '102D' TO PI-CURRENT-SCREEN-NO.
05204      GO TO 9100-RETURN-TRAN.
05205      EJECT
05206  8240-SEND-DATAONLY.
05207      MOVE PI-COMPANY-ID        TO COEO.
PEMTST     MOVE PI-PROCESSOR-ID      TO USERIDEO
05208      MOVE SAVE-DATE            TO ERUNDTEO.
05209      MOVE EIBTIME              TO TIME-IN.
05210      MOVE TIME-OUT             TO ERUNTMEO.
05211      MOVE EMI-MESSAGE-AREA (1) TO EERMSG1O.
05212      
      * EXEC CICS SEND
05213 *        MAP   (MAP-NAME)
05214 *        MAPSET(MAPSET-NAME)
05215 *        FROM  (EL102EO)
05216 *        DATAONLY
05217 *        CURSOR
05218 *    END-EXEC.
           MOVE LENGTH OF EL102EO TO DFHEIV12
           MOVE -1 TO DFHEIV11
           MOVE '8$D    CT       H L F ,   #00008745' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL102EO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05220      MOVE '102E' TO PI-CURRENT-SCREEN-NO.
05222      GO TO 9100-RETURN-TRAN.
05223      EJECT
05224  8300-SEND-TEXT.
05225      
      * EXEC CICS SEND TEXT
05226 *        FROM  (LOGOFF-TEXT)
05227 *        LENGTH(LOGOFF-LENGTH)
05228 *        ERASE
05229 *        FREEKB
05230 *    END-EXEC.
           MOVE '8&      T  E F  H   F -   #00008756' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LOGOFF-TEXT, 
                 LOGOFF-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05232      
      * EXEC CICS RETURN
05233 *    END-EXEC.
           MOVE '.(                    $   #00008762' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05235      EJECT
05236  8400-LOG-JOURNAL-RECORD.
05237 *    IF PI-JOURNAL-FILE-ID = 0
05238 *        GO TO 8400-EXIT.
05239 *
05240 *    MOVE PI-PROCESSOR-ID TO JP-USER-ID.
05241 *    MOVE FILE-ID         TO JP-FILE-ID.
05242 *    MOVE THIS-PGM        TO JP-PROGRAM-ID.
05243 *    EXEC CICS JOURNAL
05244 *        JFILEID(PI-JOURNAL-FILE-ID)
05245 *        JTYPEID('EL')
05246 *        FROM   (JOURNAL-RECORD)
05247 *        LENGTH (773)
05248 *    END-EXEC.
05249 *
05250  8400-EXIT.
05251      EXIT.
05253  8800-UNAUTHORIZED-ACCESS.
05254      MOVE UNACCESS-MSG TO LOGOFF-MSG.
05255      GO TO 8300-SEND-TEXT.
05257  8810-PF23.
05258      MOVE EIBAID   TO PI-ENTRY-CD-1.
05259      MOVE XCTL-005 TO PGM-NAME.
05260      GO TO 9300-XCTL.
05262  8870-NOTOPEN.
05263      MOVE ER-0042 TO EMI-ERROR.
05264      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05265      MOVE -1 TO ENTERPFL.
05266      IF EIBTRNID NOT = TRANS-ID
05267          GO TO 8100-SEND-INITIAL-MAP.
05269      GO TO 8200-SEND-DATAONLY.
05271  8880-NOT-FOUND.
05272      MOVE ER-0043 TO EMI-ERROR.
05273      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05274      MOVE -1 TO ENTERPFL.
05275      IF EIBTRNID NOT = TRANS-ID
05276          GO TO 8100-SEND-INITIAL-MAP.
05278      GO TO 8200-SEND-DATAONLY.
05280  9000-RETURN-CICS.
05281      
      * EXEC CICS RETURN
05282 *    END-EXEC.
           MOVE '.(                    $   #00008803' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05284  9100-RETURN-TRAN.
05285      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.
05286      
      * EXEC CICS RETURN
05287 *        TRANSID (TRANS-ID)
05288 *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
05289 *        LENGTH  (PI-COMM-LENGTH)
05290 *    END-EXEC.
           MOVE '.(CT                  $   #00008807' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05292  9200-RETURN-MAIN-MENU.
05294      MOVE XCTL-626               TO PGM-NAME.
05296      IF CLAIM-SESSION
05297          MOVE XCTL-126           TO PGM-NAME.
05299      IF MORTGAGE-SESSION
05300          MOVE XCTL-EM626         TO PGM-NAME.
05302      IF GENERAL-LEDGER-SESSION
05303          MOVE XCTL-800           TO PGM-NAME.
05305  9300-XCTL.
05306      
      * EXEC CICS XCTL
05307 *        PROGRAM (PGM-NAME)
05308 *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
05309 *        LENGTH  (PI-COMM-LENGTH)
05310 *        END-EXEC.
           MOVE '.$C                   $   #00008821' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05312  9400-CLEAR.
05313      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.
05314      GO TO 9300-XCTL.
05316  9500-PF12.
05317      MOVE XCTL-010 TO PGM-NAME.
05318      GO TO 9300-XCTL.
05320  9600-PGMID-ERROR.
05321      
      * EXEC CICS HANDLE CONDITION
05322 *        PGMIDERR(8300-SEND-TEXT)
05323 *    END-EXEC.
           MOVE '"$L                   ! , #00008833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05325      MOVE PGM-NAME     TO PI-CALLING-PROGRAM.
05326      MOVE ' '          TO PI-ENTRY-CD-1.
05327      MOVE XCTL-005     TO PGM-NAME.
05328      MOVE PGM-NAME     TO LOGOFF-PGM.
05329      MOVE PGMIDERR-MSG TO LOGOFF-FILL.
05330      GO TO 9300-XCTL.
05332  9900-ERROR-FORMAT.
05333      IF NOT EMI-ERRORS-COMPLETE
05334          MOVE LINK-001 TO PGM-NAME
05335          
      * EXEC CICS LINK
05336 *            PROGRAM (PGM-NAME)
05337 *            COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)
05338 *            LENGTH  (EMI-COMM-LENGTH)
05339 *        END-EXEC.
           MOVE '."C                   ''   #00008845' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05341  9900-EXIT.
05342      EXIT.
05344  9990-ABEND.
05345      MOVE LINK-004 TO PGM-NAME.
05346      MOVE DFHEIBLK               TO EMI-LINE1.
05347      
      * EXEC CICS LINK
05348 *        PROGRAM   (PGM-NAME)
05349 *        COMMAREA  (EMI-LINE1)
05350 *        LENGTH    (72)
05351 *    END-EXEC.
           MOVE 72 TO DFHEIV11
           MOVE '."C                   ''   #00008855' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05353      GO TO 8200-SEND-DATAONLY.
05355  9995-SECURITY-VIOLATION.
05356 *              COPY ELCSCTP.
                                  
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCSCTP                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION = C.I.C.S. COMMON SECURITY-MESSAGE LINK          *
00007 ******************************************************************
00008                                                                   
00008                                                                   
00009      MOVE EIBDATE          TO SM-JUL-DATE.                        
00010      MOVE EIBTRMID         TO SM-TERMID.                          
00011      MOVE THIS-PGM         TO SM-PGM.                             
00012      MOVE EIBTIME          TO TIME-IN.                            
00013      MOVE TIME-OUT         TO SM-TIME.                            
00014      MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.                    
00015                                                                   
00016      
      * EXEC CICS LINK                                               
00017 *         PROGRAM  ('EL003')                                      
00018 *         COMMAREA (SECURITY-MESSAGE)                             
00019 *         LENGTH   (80)                                           
00020 *    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80 TO DFHEIV11
           MOVE '."C                   ''   #00008880' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
                                                               
00021                                                                   
00022 ******************************************************************
00023                                                                   
05358  9995-EXIT.
05359       EXIT.
05361  9999-LAST-PARAGRAPH SECTION.
05362      MOVE ZEROS  TO RETURN-CODE.
05362      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL102' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
0
       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL102' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8870-NOTOPEN,
                     8880-NOT-FOUND,
                     9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 8880-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8870-NOTOPEN,
                     0560-NOT-FOUND,
                     0560-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 0850-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 0950-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8880-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8880-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 8880-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 8880-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 1550-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL102' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
