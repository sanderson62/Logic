00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL1782.
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 02/13/96 09:36:39.                 
00007 *                            VMOD=2.011                           
00008 *                                                                 
00009 *AUTHOR.           LOGIC,INC.                                     
00010 *                  DALLAS,TEXAS.                                  
00011 *#################################################################
      *
00024 *REMARKS. TRANSACTION EX56 - LETTER PRINTER.                      
00025 *        THIS PROGRAM IS USED TO PRINT THE STORED LETTERS AND     
00026 *        LABELS  DEPENDING ON THE VALUE OF THE PI-ENTRY-CODES.    
00027 *
00028 *        PRINT INITIAL LETTERS          CODE-1 = 1    
00029 *                                       CODE-2 = 1   
00030 * 
00031 *        PRINT FOLLOW-UP LETTERS        CODE-1 = 1  
00032 *                                       CODE-2 = 2 
00033 *
110402*        RE-PRINT LETTERS               CODE-1 = ' ' 
00035 *                                       CODE-2 = 3  
00036 *
110402*        PRINT ADDRESS LABELS           CODE-1 = ' '
00038 *                                       CODE-2 = 2 
00036 *
110402*        RE-PRINT LETTERS FOR A CLAIM   CODE-1 = ' '
110402*                                       CODE-2 = 4  
      *
00011 *#################################################################
110402*
110402*                        C H A N G E   L O G
110402*-----------------------------------------------------------------
110402*  CHANGE   CHANGE REQUEST  PGMR  DESCRIPTION OF CHANGE
110402* EFFECTIVE    NUMBER
110402*-----------------------------------------------------------------
110402* 110402    2001031200008   SMVA  ADD REPRINT OF LETTERS FOR A  
110402*                                 SPECIFIC CLAIM 
110402******************************************************************
00039      EJECT                                                        
00040  ENVIRONMENT DIVISION.                                            
00041  DATA DIVISION.                                                   
00042  WORKING-STORAGE SECTION.                                         
       01  DFH-START PIC X(04).
00043  77  THIS-PGM PIC X(6)  VALUE 'EL1782'.                           
00044  77  FILLER  PIC X(32)  VALUE '********************************'. 
00045  77  FILLER  PIC X(32)  VALUE '*   EL1782 WORKING STORAGE     *'. 
00046  77  FILLER  PIC X(32)  VALUE '********* V/M 2.011 ************'. 
00047                                                                   
00048  01  WS-DATA-SHIFT-AREA.                                          
00049      05  WS-DATA-FIL         PIC X(7).                            
00050      05  WS-DATA-SHIFT       PIC X(125).                          
00051                                                                   
00052  01  WS-DATE-AREA.                                                
00053      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            
00054      05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.            
00055                                                                   
00056  01  WS-CONSTANTS.                                                
00057      12  PGM-EL1782              PIC X(8)    VALUE 'EL1782'.      
00058      12  PGM-NAME                PIC X(8).                        
00059      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     
00060      12  ARCH-ID                 PIC X(8)    VALUE 'ELARCH'.      
00061      12  ARCH-ID2                PIC X(8)    VALUE 'ELARCH2'.     
00062      12  ACTV-ID                 PIC X(8)    VALUE 'ELTRLR'.      
00063      12  CNTL-ID                 PIC X(8)    VALUE 'ELCNTL'.      
CIDMOD     12  PI-CHECK-PRINTER-ID     PIC X(4)    VALUE SPACES.        
00064                                                                   
00065      12  CNTL-KEY.                                                
00066          16  CNTL-CO             PIC X(3).                        
00067          16  CNTL-RECORD-TYPE    PIC X       VALUE '1'.           
00068          16  CNTL-GENL.                                           
00069            18 CNTL-GEN1          PIC XX      VALUE SPACES.        
00070            18 CNTL-GEN2.                                          
00071              20 CNTL-GEN3         PIC X       VALUE SPACES.       
00072              20 CNTL-GEN4         PIC X       VALUE SPACES.       
00073          16  CNTL-SEQ             PIC S9(4)   VALUE +0    COMP.   
00074                                                                   
00075      12  ARCH-KEY.                                                
00076          16  ARCH-PARTIAL-KEY.                                    
00077              20  ARCH-CO          PIC X.                          
00078              20  ARCH-NUMBER      PIC S9(8)      COMP.            
00079              20  ARCH-REC-TYPE    PIC X.                          
00080          16  ARCH-SEQ             PIC S9(4)      COMP VALUE +0.   
00081                                                                   
00082      12  ARCH-KEY2.                                               
00083          16  ARCH-PARTIAL-KEY2.                                   
00084              20  ARCH-CO2         PIC X.                          
00085              20  ARCH-REC-TYPE2   PIC X.                          
00086          16  ARCH-NUMBER2         PIC S9(8)      COMP.            
00087          16  ARCH-SEQ2            PIC S9(4)      COMP VALUE +0.   
00088                                                                   
00089      12  ACTV-KEY.                                                
110402         16  ACTV-PARTIAL-KEY.
00090              20  ACTV-CO          PIC X.                          
00091              20  ACTV-CARRIER     PIC X.                          
00092              20  ACTV-CLAIM       PIC X(7).                       
00093              20  ACTV-CERT-NO     PIC X(11).                      
00094          16  ACTV-SEQ             PIC S9(4)   COMP.               
00095                                                                   
110402     12  WS-SAVE-ACTV-PARTIAL-KEY PIC X(20).
110402     12  WS-ARCHIVE-NUM-TBL.
110402         16  WS-ARCHIVE-NUM      PIC S9(08)   COMP
110402                                              OCCURS 40 TIMES.
00096      12  CURRENT-SAVE            PIC XX.                          
00097      12  ARCH-SAVE-KEY           PIC X(6).                        
00098      12  ERROR-LINE              PIC X(80).                       
00099      12  TEXT-BROWSE-STARTED     PIC X VALUE 'N'.                 
00100      12  HEADER-BROWSE-STARTED   PIC X VALUE 'N'.                 
00101      12  WS-PRINT-SW   COMP-3    PIC S9              VALUE ZERO.  
00102      12  WS-LETTER-FORM          PIC X(4)            VALUE SPACES.
110402     12  SUB                     PIC S9(04) COMP     VALUE ZEROS. 
110402     12  WS-SAVE-FINAL-SUB       PIC S9(04) COMP     VALUE ZEROS.
00104      12  WS-SKIP                 PIC 99.                          
00105      12  WS-COPIES               PIC 9.                           
00106      12  TOP-FORM-SW             PIC X VALUE SPACE.               
00107          88  TOP-FORM-SET        VALUE 'T'.                       
00108                                                                   
00109      12  HEADER-SW               PIC X VALUE SPACE.               
00110          88  HEADER-REC-FOUND    VALUE SPACE.                     
00111                                                                   
00112      12  CORRESPOND-SW           PIC X VALUE SPACE.               
00113          88  CORR-REC-FOUND      VALUE SPACE.                     
00114                                                                   
00115      12  ADDR-SW                 PIC X VALUE SPACE.               
00116          88  ADDRESS-REC-FOUND   VALUE SPACE.                     
00117                                                                   
110402     12  WS-TRLR-BROWSE-SW       PIC X VALUE ' '.                 
110402         88  TRLR-BROWSE-STARTED       VALUE 'Y'.                 
110402         88  TRLR-BROWSE-ENDED         VALUE 'N'.                 
110402                                                                  
00118      12  END-BROWSE-SW           PIC X VALUE 'N'.                 
00119          88  BROWSE-ENDED              VALUE 'Y'.                 
00120                                                                   
00121      12  WS-RECORD-COUNT         PIC S9(4)   VALUE +0.            
00122                                                                   
00123      12  WS-DELAY-INTERVAL       PIC S9(7)   VALUE +2  COMP-3.    
00124                                                                   
00125      12  ARCHIVE-SAVE            PIC S9(8)   COMP.                
00126      12  OPTION-CODES            PIC XX.                          
00127          88  PRINT-INITIAL       VALUE '11'.                      
00128          88  PRINT-FOLLOW-UP     VALUE '12'.                      
00129          88  PRINT-LABELS        VALUE ' 2'.                      
00130          88  REPRINT-LETTERS     VALUE ' 3'.                      
00130          88  REPRINT-FOR-CLAIM   VALUE ' 4'.   
00131                                                                   
00132      12  WDS-PRINT-LINE.                                          
00133          16  FILLER              PIC X(6)    VALUE 'CLAIM-'.      
00134          16  WDS-CLAIM-NO        PIC X(7).                        
00135          16  FILLER              PIC X(7)    VALUE '  CERT-'.     
00136          16  WDS-CERT-NO         PIC X(11).                       
00137                                                                   
00138      12  J-ARCH-LENGTH           PIC S9(4)   COMP VALUE +113.     
00139      12  J-ACTV-LENGTH           PIC S9(4)   COMP VALUE +189.     
00140      12  JOURNAL-LENGTH          PIC S9(4)   COMP.                
00141                                                                   
00142      12  SAVE-ARCH-NO            PIC S9(8)   COMP VALUE +0.       
00143                                                                   
00144      12  WS-LABEL-HOLD-AREA.                                      
00145          16  WS-LABEL-LINES OCCURS 6 TIMES INDEXED BY L-INDX.     
00146            18  WS-LABEL-ZIP.                                      
00147              20  WS-LABEL-1ST-ZIP    PIC X(4).                    
00148              20  WS-LABEL-2ND-ZIP    PIC X(5).                    
00149            18  FILLER                PIC X(12).                   
00150            18  WS-LAST-ZIP.                                       
00151              20  WS-LAST-1ST-ZIP     PIC X(4).                    
00152              20  WS-LAST-2ND-ZIP     PIC X(5).                    
00153 *                                    COPY ELCDMD34.
                                                                        
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDMD34.                           *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = DMD DLO034 PARAMETER AREA                 *
00007 *                                                                *
00008 *    LENGTH = 272    RECFRM = FIXED                              *
00009 *                                                                *
00010 ******************************************************************
00011  01  DLO034-COMMUNICATION-AREA.                                   
00012      12  DL34-PROCESS-TYPE             PIC X.                     
00013      12  DL34-COMPANY-ID               PIC XXX.                   
00014      12  DL34-PRINT-PROGRAM-ID         PIC X(8).                  
00015      12  DL34-USERID                   PIC X(4).                  
00016      12  DL34-PRINT-LINE               PIC X(250).                
00017      12  DL34-OVERRIDE-PRINTER-ID      PIC X(4).                  
00018      12  DL34-RETURN-CODE              PIC XX.                    
00019  01  DLO034-REC-LENGTH                 PIC S9(4) COMP VALUE +272. 
00154      EJECT                                                        
00155 *                                    COPY ELCINTF.
                                                                        
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
00156                                                                   
00157      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.                    
00158          16  PI-PRINT-DATE       PIC X(8).                        
00159          16  PI-PRINT-DATE-BIN   PIC XX.                          
00160          16  PI-PRINT-ID         PIC X(4).                        
00161          16  PI-STARTING-ARCH-NO PIC S9(8) COMP.                  
00162          16  PI-PRINT-BY-CARR    PIC X.                           
00163              88  PRINT-BY-CARR             VALUE 'Y'.             
00164          16  PI-PRINT-CARRIER    PIC X.                           
00165          16  PI-LETTER-TYPE      PIC X.                           
00166          16  FILLER              PIC X(619).                      
00167      EJECT                                                        
00168 *                                COPY ELCJPFX.
                                                                        
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
00169                                  PIC X(520).                      
00170      EJECT                                                        
00171 *                                COPY ELPRTCVD.
                                                                        
00001 ***************************************************************** 
00002 *                                                               * 
00003 *                            ELPRTCVD.                          * 
00004 *                            VMOD=2.001                         * 
00005 *****************************************************************.
00006                                                                   
00007 ******************************************************************
00008 ***   WORK AREAS  FOR TERMINAL ONLINE PRINT ROUTINE               
00009 ***                 -ELPRTCVD-                                    
00010 ***   TO BE USED WITH PROCEDURE COPY MEMBER -ELPRTCVP-            
00011 ******************************************************************
00012                                                                   
00013  01  S-WORK-AREA                     SYNC.                        
00014      12  WS-LINE-LEN                 PIC S9(4)       VALUE +80    
00015                                      COMP.                        
00016                                                                   
00017      12  WS-LINE-LENGTH              PIC S9(4)       VALUE ZERO   
00018                                      COMP.                        
00019                                                                   
00020      12  WS-BUFFER-SIZE              PIC S9(4)       VALUE +1916  
00021                                      COMP.                        
00022                                                                   
00023      12  WS-BUFFER-LENGTH            PIC S9(4)       VALUE ZERO   
00024                                      COMP.                        
00025                                                                   
00026      12  WS-PROG-END                 PIC X           VALUE SPACES.
00027                                                                   
00028      12  WS-PRINT-AREA.                                           
00029          16  WS-PASSED-CNTL-CHAR     PIC X           VALUE SPACES.
00030            88  SINGLE-SPACE                          VALUE ' '.   
00031            88  DOUBLE-SPACE                          VALUE '0'.   
00032            88  TRIPLE-SPACE                          VALUE '-'.   
00033            88  TOP-PAGE                              VALUE '1'.   
00034                                                                   
00035          16  WS-PASSED-DATA.                                      
00036              20  WS-PRINT-BYTE       PIC X                        
00037                  OCCURS 132 TIMES    INDEXED BY PRT-INDEX.        
00038                                                                   
00039      12  WS-LINE-CNT                 PIC S9(3)        VALUE ZERO  
00040                                      COMP-3.                      
00041      12  WS-WCC-CNTL                 PIC X(1)         VALUE 'H'.  
00042                                                                   
00043      12  WS-EM                       PIC S9(4)        VALUE +25   
00044                                      COMP.                        
00045      12  FILLER   REDEFINES WS-EM.                                
00046          16  FILLER                  PIC X.                       
00047          16  T-EM                    PIC X.                       
00048                                                                   
00049      12  WS-SS                       PIC S9(4)        VALUE +21   
00050                                      COMP.                        
00051      12  FILLER   REDEFINES WS-SS.                                
00052          16  FILLER                  PIC X.                       
00053          16  T-SS                    PIC X.                       
00054                                                                   
00055      12  WS-TP                       PIC S9(4)      VALUE +12     
00056                                      COMP.                        
00057      12  FILLER   REDEFINES WS-TP.                                
00058          16  FILLER                  PIC X.                       
00059          16  T-TP                    PIC X.                       
00060                                                                   
00061      12  WS-FIRST-TIME-SW            PIC X           VALUE '1'.   
00062          88  FIRST-TIME                              VALUE '1'.   
00063          88  FIRST-LINE-NEXT-BUFFER                  VALUE '2'.   
00064                                                                   
00065      12  WS-BUFFER-AREA.                                          
00066          16  WS-BUFFER-BYTE          PIC X                        
00067              OCCURS 1920 TIMES       INDEXED BY BUFFER-INDEX      
00068                                                 BUFFER-INDEX2.    
00069                                                                   
00070 ******************************************************************
00172      EJECT                                                        
00173 *                                COPY ELCDATE.
                                                                        
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
00174      EJECT                                                        
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
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
       LINKAGE  SECTION.
00176 *                            COPY ELCARCH.
                                                                        
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCARCH.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE            
00005 *                            VMOD=2.007                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = LETTERS SENT TO ARCHIVE FILE              *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 090  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELARCH                        RKP=2,LEN=8     *
00013 *       ALTERNATE PATH1 = ELARCH2 (RECORD TYPE)  RKP=10,LEN=8    *
00014 *                                                                *
00015 *   LOG = NO                                                     *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
CIDMOD*                                                                *
CIDMOD*  THERE ARE CID MODS IN COPYBOOK ELCARCH                        *
00017 ******************************************************************
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
       01  DFHCOMMAREA       PIC X(01).
00018  01  LETTER-ARCHIVE.                                              
00019      12  LA-RECORD-ID                PIC XX.                      
00020          88  VALID-LA-ID                VALUE 'LA'.               
00021                                                                   
00022      12  LA-CONTROL-PRIMARY.                                      
00023          16  LA-COMPANY-CD           PIC X.                       
00024          16  LA-ARCHIVE-NO           PIC S9(8)     COMP.          
00025          16  LA-RECORD-TYPE          PIC X.                       
00026              88  LA-HEADER-DATA         VALUE '1'.                
00027              88  LA-ADDRESS-DATA        VALUE '2'.                
00028              88  LA-TEXT-DATA           VALUE '3'.                
00029              88  LA-FORM-CONTROL-HDR    VALUE '4'.                
00030          16  LA-LINE-SEQ-NO          PIC S9(4)     COMP.          
00031                                                                   
00032      12  LA-CONTROL-BY-TYPE.                                      
00033          16  LA-COMPANY-CD-A1        PIC X.                       
00034          16  LA-RECORD-TYPE-A1       PIC X.                       
00035          16  LA-ARCHIVE-NO-A1        PIC S9(8)     COMP.          
00036          16  LA-LINE-SEQ-NO-A1       PIC S9(4)     COMP.          
00037                                                                   
00038      12  LA-TEXT-RECORD.                                          
00039          16  LA-SKIP-CONTROL         PIC XX.                      
00040              88  NO-LINES-SKIPPED       VALUE SPACES.             
00041              88  SKIP-TO-NEXT-PAGE      VALUE '99'.               
00042          16  LA-TEXT-LINE            PIC X(70).                   
00043                                                                   
00044      12  LA-ADDRESS-RECORD  REDEFINES  LA-TEXT-RECORD.            
00045          16  FILLER                  PIC XX.                      
00046          16  LA-ADDRESS-LINE         PIC X(30).                   
00047          16  FILLER                  PIC X(40).                   
00048                                                                   
00049      12  LA-HEADER-RECORD  REDEFINES  LA-TEXT-RECORD.             
00050          16  FILLER                  PIC XX.                      
00051          16  LA-CARRIER              PIC X.                       
00052          16  LA-CLAIM-NO             PIC X(7).                    
00053          16  LA-CERT-NO.                                          
00054              20  LA-CERT-PRIME       PIC X(10).                   
00055              20  LA-CERT-SFX         PIC X.                       
00056          16  LA-NO-OF-COPIES         PIC S9.                      
00057          16  LA-RESEND-DATE          PIC XX.                      
00058          16  LA-PROCESSOR-CD         PIC X(4).                    
00059          16  LA-CREATION-DT          PIC XX.                      
00060          16  LA-INITIAL-PRINT-DATE   PIC XX.                      
00061          16  LA-RESEND-PRINT-DATE    PIC XX.                      
00062          16  LA-CORR-TRLR-SEQ        PIC S9(4)    COMP.           
00063          16  LA-1ST-RESEND-PRINT-DT  PIC XX.                      
CIDMOD*                                                                 
00064 * -----  16  LA-DMD-ADDITIONAL-FIELDS.                            
00065 *   I        20  LA-DMD-LETTER-FORM      PIC X(4).                
00066 *   I        20  LA-DMD-PROD-CODE        PIC XX.                  
00067 *   I        20  LA-DMD-RES-ST           PIC XX.                  
00068 *   I        20  LA-DMD-CORR-TRLR-SEQ    PIC S9(4)    COMP.       
00069 *   I        20  LA-DMD-LETTER-STATUS    PIC X.                   
00070 *  NEW           88  LA-DMD-LETTER-ONLINE   VALUE '1'.            
00071 *  DMD           88  LA-DMD-LETTER-PURGED   VALUE '2'.            
00072 *  CHGS          88  LA-DMD-LETTER-RELOADED VALUE '3'.            
00073 *   I        20  LA-DMD-LETTER-PURGE-DT  PIC XX.                  
00074 *   I        20  LA-DMD-LETTER-RELOAD-DT PIC XX.                  
00075 *   I        20  LA-DMD-UND-CODE         PIC XX.                  
00076 *   I        20  LA-DMD-BEN-CODE         PIC XX.                  
00077 *   V    16  FILLER                  PIC X(15).                   
CIDMOD* -----                                                           
CIDMOD*                                                                 
CIDMOD* REINSERTED  CSO  MODS                                           
CIDMOD*                                                                 
CIDMOD         16  FILLER.                                              
CIDMOD             20  FILLER                  PIC X(29).               
CIDMOD             20  LA-CSO-LETTER-STATUS    PIC X.                   
CIDMOD                 88  LA-CSO-LETTER-ONLINE   VALUE '1'.            
CIDMOD                 88  LA-CSO-LETTER-PURGED   VALUE '2'.            
CIDMOD                 88  LA-CSO-LETTER-RELOADED VALUE '3'.            
CIDMOD             20  LA-CSO-LETTER-PURGE-DT  PIC XX.                  
CIDMOD             20  LA-CSO-LETTER-RELOAD-DT PIC XX.                  
CIDMOD*                                                                 
00078                                                                   
00079      12  LA-FORM-CONTROL-HEADER REDEFINES  LA-TEXT-RECORD.        
00080          16  FILLER                  PIC XX.                      
00081          16  LA4-CARRIER             PIC X.                       
00082          16  LA4-CLAIM-NO            PIC X(7).                    
00083          16  LA4-CERT-NO.                                         
00084              20  LA4-CERT-PRIME      PIC X(10).                   
00085              20  LA4-CERT-SFX        PIC X.                       
00086          16  LA4-NO-OF-COPIES        PIC S9.                      
00087          16  LA4-RESEND-DATE         PIC XX.                      
00088          16  LA4-PROCESSOR-CD        PIC X(4).                    
00089          16  LA4-CREATION-DT         PIC XX.                      
00090          16  LA4-INITIAL-PRINT-DATE  PIC XX.                      
00091          16  LA4-RESEND-PRINT-DATE   PIC XX.                      
00092          16  LA4-FORM-TRLR-SEQ       PIC S9(4)    COMP.           
00093          16  LA4-FORM-TYPE           PIC X.                       
00094              88  LA4-INITIAL-FORM    VALUE '1'.                   
00095              88  LA4-PROGRESS-FORM   VALUE '2'.                   
00096          16  LA4-FORM-REM-PRINT-DT   PIC X(02).                   
00097          16  LA4-STATE               PIC X(02).                   
00098          16  FILLER                  PIC X(31).                   
00099 ******************************************************************
00177      EJECT                                                        
00178 *                            COPY ELCTRLR.
                                                                        
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCTRLR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE            
00005 *                            VMOD=2.014                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ACTIVITY TRAILER FILE                     *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 200    RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELTRLR             RKP=2,LEN=22          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  ACTIVITY-TRAILERS.                                           
00019      12  AT-RECORD-ID                    PIC XX.                  
00020          88  VALID-AT-ID                       VALUE 'AT'.        
00021                                                                   
00022      12  AT-CONTROL-PRIMARY.                                      
00023          16  AT-COMPANY-CD               PIC X.                   
00024          16  AT-CARRIER                  PIC X.                   
00025          16  AT-CLAIM-NO                 PIC X(7).                
00026          16  AT-CERT-NO.                                          
00027              20  AT-CERT-PRIME           PIC X(10).               
00028              20  AT-CERT-SFX             PIC X.                   
00029          16  AT-SEQUENCE-NO              PIC S9(4)     COMP.      
00030              88  AT-1ST-TRL-AVAIL             VALUE +4095.        
00031              88  AT-LAST-TRL-AVAIL            VALUE +100.         
00032              88  AT-RESV-EXP-HIST-TRL         VALUE +0.           
00033              88  AT-INSURED-ADDR-TRL          VALUE +1 THRU +9.   
00034              88  AT-BENEFICIARY-ADDR-TRL      VALUE +11 THRU +19. 
00035              88  AT-ACCOUNT-ADDR-TRL          VALUE +21 THRU +29. 
00036              88  AT-PHYSICIAN-ADDR-TRL        VALUE +31 THRU +39. 
00037              88  AT-EMPLOYERS-ADDR-TRL        VALUE +41 THRU +49. 
00038              88  AT-OTHER-1-ADDR-TRL          VALUE +51 THRU +59. 
00039              88  AT-OTHER-2-ADDR-TRL          VALUE +61 THRU +69. 
00040              88  AT-DIAGNOSIS-TRL             VALUE +90.          
00041                                                                   
00042      12  AT-TRAILER-TYPE                 PIC X.                   
00043          88  RESERVE-EXPENSE-TR               VALUE '1'.          
00044          88  PAYMENT-TR                       VALUE '2'.          
00045          88  AUTO-PAY-TR                      VALUE '3'.          
00046          88  CORRESPONDENCE-TR                VALUE '4'.          
00047          88  ADDRESS-TR                       VALUE '5'.          
00048          88  GENERAL-INFO-TR                  VALUE '6'.          
00049          88  AUTO-PROMPT-TR                   VALUE '7'.          
00050          88  DENIAL-TR                        VALUE '8'.          
00051          88  INCURRED-CHG-TR                  VALUE '9'.          
00052          88  FORM-CONTROL-TR                  VALUE 'A'.          
00053                                                                   
00054      12  AT-RECORDED-DT                  PIC XX.                  
00055      12  AT-RECORDED-BY                  PIC X(4).                
00056      12  AT-LAST-MAINT-HHMMSS            PIC S9(6)     COMP-3.    
00057                                                                   
00058      12  AT-TRAILER-BODY                 PIC X(165).              
00059                                                                   
00060      12  AT-RESERVE-EXPENSE-TR  REDEFINES  AT-TRAILER-BODY.       
00061          16  AT-RESERVE-CONTROLS.                                 
00062              20  AT-MANUAL-SW            PIC X.                   
00063                  88  AT-MANUAL-RESERVES-USED VALUE '1'.           
00064              20  AT-FUTURE-SW            PIC X.                   
00065                  88  AT-FUTURE-RESERVES-USED VALUE '1'.           
00066              20  AT-PTC-SW               PIC X.                   
00067                  88  AT-PAY-TO-CURRENT-USED  VALUE '1'.           
00068              20  AT-IBNR-SW              PIC X.                   
00069                  88  AT-IBNR-RESERVES-USED   VALUE '1'.           
00070              20  AT-PTC-LF-SW            PIC X.                   
00071                  88  AT-LF-PTC-USED          VALUE '1'.           
00072              20  AT-CDT-ACCESS-METHOD    PIC X.                   
00073                  88  AT-CDT-ROUND-NEAR       VALUE '1'.           
00074                  88  AT-CDT-ROUND-HIGH       VALUE '2'.           
00075                  88  AT-CDT-INTERPOLATED     VALUE '3'.           
00076              20  AT-PERCENT-OF-CDT       PIC S9(3)V99    COMP-3.  
00077          16  AT-LAST-COMPUTED-DT         PIC XX.                  
00078          16  AT-FUTURE-RESERVE           PIC S9(5)V99    COMP-3.  
00079          16  AT-PAY-CURRENT-RESERVE      PIC S9(5)V99    COMP-3.  
00080          16  AT-IBNR-RESERVE             PIC S9(5)V99    COMP-3.  
00081          16  AT-INITIAL-MANUAL-RESERVE   PIC S9(5)V99    COMP-3.  
00082          16  AT-CURRENT-MANUAL-RESERVE   PIC S9(5)V99    COMP-3.  
00083          16  AT-ITD-ADDITIONAL-RESERVE   PIC S9(5)V99    COMP-3.  
00084          16  AT-EXPENSE-CONTROLS.                                 
00085              20  AT-EXPENSE-METHOD       PIC X.                   
00086                  88  NO-EXPENSE-CALCULATED    VALUE '1'.          
00087                  88  FLAT-DOLLAR-PER-PMT      VALUE '2'.          
00088                  88  PERCENT-OF-PMT           VALUE '3'.          
00089                  88  DOLLAR-PER-OPEN-MONTH    VALUE '4'.          
00090              20  AT-EXPENSE-PERCENT      PIC S9(3)V99    COMP-3.  
00091              20  AT-EXPENSE-DOLLAR       PIC S9(3)V99    COMP-3.  
00092          16  AT-ITD-PAID-EXPENSES        PIC S9(5)V99    COMP-3.  
00093          16  AT-ITD-CHARGEABLE-EXPENSE   PIC S9(5)V99    COMP-3.  
00094                                                                   
00095          16  AT-ITD-LIFE-REFUNDS         PIC S9(5)V99    COMP-3.  
00096          16  AT-ITD-AH-REFUNDS           PIC S9(5)V99    COMP-3.  
00097                                                                   
00098          16  FILLER                      PIC X(53).               
00099                                                                   
00100          16  AT-RESERVES-LAST-MAINT-DT   PIC XX.                  
00101          16  AT-RESERVES-LAST-UPDATED-BY PIC X(4).                
00102                                                                   
00103          16  AT-OPEN-CLOSE-HISTORY OCCURS 6 TIMES.                
00104              20  AT-OPEN-CLOSE-DATE      PIC XX.                  
00105              20  AT-OPEN-CLOSE-TYPE      PIC X.                   
00106 *                    C = CLOSED                                   
00107 *                    O = OPEN                                     
00108              20  AT-OPEN-CLOSE-REASON    PIC X(5).                
00109 *                   REASONS = ALTER, AUTO, FINAL, NEW, FORCE      
00110                                                                   
00111      12  AT-PAYMENT-TR  REDEFINES  AT-TRAILER-BODY.               
00112          16  AT-PAYMENT-TYPE             PIC X.                   
00113              88  PARTIAL-PAYMENT                VALUE '1'.        
00114              88  FINAL-PAYMENT                  VALUE '2'.        
00115              88  LUMP-SUM-PAYMENT               VALUE '3'.        
00116              88  ADDITIONAL-PAYMENT             VALUE '4'.        
00117              88  CHARGEABLE-EXPENSE             VALUE '5'.        
00118              88  NON-CHARGEABLE-EXPENSE         VALUE '6'.        
00119              88  VOIDED-PAYMENT                 VALUE '9'.        
00120              88  TRANSFER                       VALUE 'T'.        
00121                                                                   
00122          16  AT-CLAIM-TYPE               PIC X.                   
00123              88  PAID-FOR-AH                    VALUE 'A'.        
00124              88  PAID-FOR-LIFE                  VALUE 'L'.        
00124              88  PAID-FOR-IUI                   VALUE 'I'.        
00125          16  AT-CLAIM-PREM-TYPE          PIC X.                   
00126              88  AT-SINGLE-PREMIUM              VALUE '1'.        
00127              88  AT-O-B-COVERAGE                VALUE '2'.        
00128              88  AT-OPEN-END-COVERAGE           VALUE '3'.        
00129          16  AT-AMOUNT-PAID              PIC S9(7)V99  COMP-3.    
00130          16  AT-CHECK-NO                 PIC X(7).                
00131          16  AT-PAID-FROM-DT             PIC XX.                  
00132          16  AT-PAID-THRU-DT             PIC XX.                  
00133          16  AT-DAYS-IN-PERIOD           PIC S9(4)     COMP.      
00134          16  FILLER                      PIC X.                   
00135          16  AT-PAYEES-NAME              PIC X(30).               
00136          16  AT-PAYMENT-ORIGIN           PIC X.                   
00137              88  ONLINE-MANUAL-PMT              VALUE '1'.        
00138              88  ONLINE-AUTO-PMT                VALUE '2'.        
00139              88  OFFLINE-PMT                    VALUE '3'.        
00140          16  AT-CHECK-WRITTEN-DT         PIC XX.                  
00141          16  AT-TO-BE-WRITTEN-DT         PIC XX.                  
00142          16  AT-VOID-DATA.                                        
00143              20  AT-VOID-DT              PIC XX.                  
00144              20  AT-VOID-REASON          PIC X(30).               
00145          16  AT-ADDL-RESERVE             PIC S9(5)V99  COMP-3.    
00146          16  AT-EXPENSE-PER-PMT          PIC S9(5)V99  COMP-3.    
00147          16  AT-CREDIT-INTERFACE.                                 
00148              20  AT-PMT-SELECT-DT        PIC XX.                  
00149                  88  PAYMENT-NOT-SELECTED  VALUE LOW-VALUE.       
00150              20  AT-PMT-ACCEPT-DT        PIC XX.                  
00151                  88  PAYMENT-NOT-ACCEPTED  VALUE LOW-VALUE.       
00152              20  AT-VOID-SELECT-DT       PIC XX.                  
00153                  88  VOID-NOT-SELECTED     VALUE LOW-VALUE.       
00154              20  AT-VOID-ACCEPT-DT       PIC XX.                  
00155                  88  VOID-NOT-ACCEPTED     VALUE LOW-VALUE.       
00156                                                                   
00157          16  AT-CHECK-QUE-CONTROL        PIC S9(8)     COMP.      
00158                  88  PAYMENT-NOT-QUEUED           VALUE ZERO.     
00159                  88  CONVERSION-PAYMENT           VALUE +99999999.
00160          16  AT-CHECK-QUE-SEQUENCE       PIC S9(4)     COMP.      
00161                                                                   
00162          16  AT-FORCE-CONTROL            PIC X.                   
00163              88  PAYMENT-WAS-FORCED           VALUE '1'.          
00164          16  AT-PREV-LAST-PMT-DT         PIC XX.                  
00165          16  AT-PREV-PAID-THRU-DT        PIC XX.                  
00166          16  AT-PREV-LAST-PMT-AMT        PIC S9(7)V99  COMP-3.    
00167          16  AT-ELIMINATION-DAYS         PIC S999      COMP-3.    
00168          16  AT-DAILY-RATE               PIC S9(3)V99  COMP-3.    
00169          16  AT-BENEFIT-TYPE             PIC X.                   
00170                                                                   
00171          16  AT-EXPENSE-TYPE             PIC X.                   
00172          16  AT-PAYMENT-APPROVAL-SW      PIC X.                   
00173                                                                   
00174          16  AT-PAYEE-TYPE-CD.                                    
00175              20  AT-PAYEE-TYPE           PIC X.                   
00176                  88  INSURED-PAID           VALUE 'I'.            
00177                  88  BENEFICIARY-PAID       VALUE 'B'.            
00178                  88  ACCOUNT-PAID           VALUE 'A'.            
00179                  88  OTHER-1-PAID           VALUE 'O'.            
00180                  88  OTHER-2-PAID           VALUE 'Q'.            
00181                  88  DOCTOR-PAID            VALUE 'P'.            
00182                  88  EMPLOYER-PAID          VALUE 'E'.            
00183              20  AT-PAYEE-SEQ            PIC X.                   
00184                                                                   
00185          16  AT-CASH-PAYMENT             PIC X.                   
00186          16  AT-GROUPED-PAYMENT          PIC X.                   
00187          16  AT-PAYMENT-NOTE-SEQ-NO      PIC S9(4)       COMP.    
00188          16  AT-APPROVAL-LEVEL-REQD      PIC X.                   
00189          16  AT-APPROVED-LEVEL           PIC X.                   
00190          16  AT-VOID-TYPE                PIC X.                   
00191              88  AT-PAYMENT-WAS-STOPPED     VALUE 'S'.            
00192              88  AT-PAYMENT-WAS-VOIDED      VALUE 'V'.            
00193          16  AT-AIG-UNEMP-IND            PIC X.                   
00194              88  AT-AIG-UNEMPLOYMENT-PMT    VALUE 'U'.            
00195          16  AT-ASSOCIATES               PIC X.                   
00196              88  AT-AIG-INTERFACE           VALUE 'I' 'N'.        
00197              88  AT-AIG-NON-INTERFACE       VALUE 'A' 'M'.        
00198                                                                   
00199          16  AT-FORM-CTL-SEQ-NO          PIC S9(4)       COMP.    
00200          16  AT-CV-PMT-CODE              PIC X.                   
00201              88  FULL-DEATH-PAYMENT         VALUE '1'.            
00202              88  HALF-DEATH-PAYMENT         VALUE '2'.            
00203              88  FULL-ADD-PAYMENT           VALUE '3'.            
00204              88  HALF-ADD-PAYMENT           VALUE '4'.            
00205              88  FULL-RIDER-PAYMENT         VALUE '5'.            
00206              88  HALF-RIDER-PAYMENT         VALUE '6'.            
00207              88  NON-CHG-EXP-PAYMENT        VALUE '7'.            
00208              88  ADDL-PAYMENT               VALUE '8'.            
00209                                                                   
00210          16  AT-EOB-CODE1                PIC XXX.                 
00211          16  AT-EOB-CODE2                PIC XXX.                 
00212          16  AT-EOB-CODE3                PIC XXX.                 
00213          16  AT-EOB-CODE4                PIC XXX.                 
00214          16  AT-EOB-CODE5                PIC XXX.                 
00215                                                                   
00216          16  FILLER                      PIC X.                   
00217                                                                   
00218          16  AT-PAYMENT-LAST-MAINT-DT    PIC XX.                  
00219          16  AT-PAYMENT-LAST-UPDATED-BY  PIC X(4).                
00220                                                                   
00221      12  AT-AUTO-PAY-TR  REDEFINES  AT-TRAILER-BODY.              
00222          16  AT-SCHEDULE-START-DT        PIC XX.                  
00223          16  AT-SCHEDULE-END-DT          PIC XX.                  
00224          16  AT-TERMINATED-DT            PIC XX.                  
00225          16  AT-LAST-PMT-TYPE            PIC X.                   
00226              88  LAST-PMT-IS-FINAL              VALUE 'F'.        
00227              88  LAST-PMT-IS-PARTIAL            VALUE 'P'.        
00228          16  AT-FIRST-PMT-DATA.                                   
00229              20  AT-FIRST-PMT-AMT        PIC S9(7)V99  COMP-3.    
00230              20  AT-DAYS-IN-1ST-PMT      PIC S9(4)     COMP.      
00231              20  AT-1ST-PAY-THRU-DT      PIC XX.                  
00232          16  AT-REGULAR-PMT-DATA.                                 
00233              20  AT-REGULAR-PMT-AMT      PIC S9(7)V99  COMP-3.    
00234              20  AT-DAYS-IN-REG-PMT      PIC S9(4)     COMP.      
00235              20  AT-INTERVAL-MONTHS      PIC S9(4)     COMP.      
00236          16  AT-AUTO-PAYEE-CD.                                    
00237              20  AT-AUTO-PAYEE-TYPE      PIC X.                   
00238                  88  INSURED-PAID-AUTO      VALUE 'I'.            
00239                  88  BENEFICIARY-PAID-AUTO  VALUE 'B'.            
00240                  88  ACCOUNT-PAID-AUTO      VALUE 'A'.            
00241                  88  OTHER-1-PAID-AUTO      VALUE 'O'.            
00242                  88  OTHER-2-PAID-AUTO      VALUE 'Q'.            
00243              20  AT-AUTO-PAYEE-SEQ       PIC X.                   
00244          16  AT-AUTO-PAY-DAY             PIC 99.                  
00245          16  AT-AUTO-CASH                PIC X.                   
00246              88  AT-CASH                      VALUE 'Y'.          
00247              88  AT-NON-CASH                  VALUE 'N'.          
00248          16  FILLER                      PIC X(129).              
00249                                                                   
00250          16  AT-AUTO-PAY-LAST-MAINT-DT   PIC XX.                  
00251          16  AT-AUTO-PAY-LAST-UPDATED-BY PIC X(4).                
00252                                                                   
00253      12  AT-CORRESPONDENCE-TR  REDEFINES  AT-TRAILER-BODY.        
00254          16  AT-LETTER-SENT-DT           PIC XX.                  
00255          16  AT-RECEIPT-FOLLOW-UP        PIC XX.                  
00256          16  AT-AUTO-RE-SEND-DT          PIC XX.                  
00257          16  AT-LETTER-ANSWERED-DT       PIC XX.                  
00258          16  AT-LETTER-ARCHIVE-NO        PIC S9(8)     COMP.      
00259          16  AT-LETTER-ORIGIN            PIC X.                   
00260              88  ONLINE-CREATION              VALUE '1'.          
00261              88  OFFLINE-CREATION             VALUE '2'.          
00262          16  AT-STD-LETTER-FORM          PIC X(4).                
00263          16  AT-REASON-TEXT              PIC X(70).               
00264          16  AT-ADDRESS-REC-SEQ-NO       PIC S9(4)     COMP.      
00265          16  AT-ADDRESEE-TYPE            PIC X.                   
00266               88  INSURED-ADDRESEE            VALUE 'I'.          
00267               88  BENEFICIARY-ADDRESEE        VALUE 'B'.          
00268               88  ACCOUNT-ADDRESEE            VALUE 'A'.          
00269               88  PHYSICIAN-ADDRESEE          VALUE 'P'.          
00270               88  EMPLOYER-ADDRESEE           VALUE 'E'.          
00271               88  OTHER-ADDRESEE-1            VALUE 'O'.          
00272               88  OTHER-ADDRESEE-2            VALUE 'Q'.          
00273          16  AT-ADDRESSEE-NAME           PIC X(30).               
00274          16  AT-INITIAL-PRINT-DATE       PIC XX.                  
00275          16  AT-RESEND-PRINT-DATE        PIC XX.                  
00276          16  AT-CORR-SOL-UNSOL           PIC X.                   
00277          16  AT-LETTER-PURGED-DT         PIC XX.                  
CIDMOD*                                                                 
CIDMOD*FOLLOWING CID CHGS REENTERED AS DMD CHGS OVERLAID THEM.          
CIDMOD*                                                                 
CIDMOD         16  AT-CSO-REDEFINITION.                                 
CIDMOD             20  FILLER                  PIC X(27).               
CIDMOD             20  AT-CSO-LETTER-STATUS    PIC X.                   
CIDMOD                 88  AT-CSO-LETTER-ONLINE    VALUE '1'.           
CIDMOD                 88  AT-CSO-LETTER-PURGED    VALUE '2'.           
CIDMOD                 88  AT-CSO-LETTER-RELOADED  VALUE '3'.           
CIDMOD             20  AT-CSO-LETTER-PURGE-DATE   PIC XX.               
CIDMOD             20  AT-CSO-LETTER-RELOAD-DATE  PIC XX.               
CIDMOD*                                                                 
CIDMOD*FOLLOWING DMD CHGS COMMENTED OUT AS THEY OVERLAY CID MODS NEEDED 
CIDMOD*                                                                 
CIDMOD*        16  FILLER                      PIC X(26).               
CIDMOD*                                                                 
CIDMOD*        16  AT-DMD-BSR-CODE             PIC X.                   
CIDMOD*            88  AT-AUTOMATED-BSR              VALUE 'A'.         
CIDMOD*            88  AT-NON-AUTOMATED-BSR          VALUE 'B' ' '.     
CIDMOD*                                                                 
CIDMOD*        16  AT-DMD-LETTER-STATUS        PIC X.                   
CIDMOD*            88  AT-DMD-LETTER-ONLINE          VALUE '1'.         
CIDMOD*            88  AT-DMD-LETTER-PURGED          VALUE '2'.         
CIDMOD*            88  AT-DMD-LETTER-RELOADED        VALUE '3'.         
CIDMOD*        16  AT-DMD-LETTER-PURGE-DT      PIC XX.                  
CIDMOD*        16  AT-DMD-LETTER-RELOAD-DT     PIC XX.                  
00290                                                                   
00291          16  AT-CORR-LAST-MAINT-DT       PIC XX.                  
00292          16  AT-CORR-LAST-UPDATED-BY     PIC X(4).                
00293                                                                   
00294      12  AT-ADDRESS-TR  REDEFINES  AT-TRAILER-BODY.               
00295          16  AT-ADDRESS-TYPE             PIC X.                   
00296              88  INSURED-ADDRESS               VALUE 'I'.         
00297              88  BENEFICIARY-ADDRESS           VALUE 'B'.         
00298              88  ACCOUNT-ADDRESS               VALUE 'A'.         
00299              88  PHYSICIAN-ADDRESS             VALUE 'P'.         
00300              88  EMPLOYER-ADDRESS              VALUE 'E'.         
00301              88  OTHER-ADDRESS-1               VALUE 'O'.         
00302              88  OTHER-ADDRESS-2               VALUE 'Q'.         
00303          16  AT-MAIL-TO-NAME             PIC X(30).               
00304          16  AT-ADDRESS-LINE-1           PIC X(30).               
00305          16  AT-ADDRESS-LINE-2           PIC X(30).               
00306          16  AT-CITY-STATE               PIC X(30).               
00307          16  AT-ZIP.                                              
00308              20  AT-ZIP-CODE.                                     
00309                  24  AT-ZIP-1ST          PIC X.                   
00310                      88  AT-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00311                  24  FILLER              PIC X(4).                
00312              20  AT-ZIP-PLUS4            PIC X(4).                
00313          16  AT-CANADIAN-POSTAL-CODE  REDEFINES  AT-ZIP.          
00314              20  AT-CAN-POSTAL-1         PIC XXX.                 
00315              20  AT-CAN-POSTAL-2         PIC XXX.                 
00316              20  FILLER                  PIC XXX.                 
00317          16  AT-PHONE-NO                 PIC 9(11)     COMP-3.    
00318          16  FILLER                      PIC X(23).               
00319          16  AT-ADDRESS-LAST-MAINT-DT    PIC XX.                  
00320          16  AT-ADDRESS-LAST-UPDATED-BY  PIC X(4).                
00321                                                                   
00322      12  AT-GENERAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.          
00323          16  AT-INFO-LINE-1              PIC X(60).               
00324          16  AT-INFO-LINE-2              PIC X(60).               
00325          16  AT-INFO-TRAILER-TYPE        PIC X.                   
00326              88  AT-PAYMENT-NOTE         VALUE 'P'.               
00327              88  AT-CALL-NOTE            VALUE 'C'.               
00328              88  AT-MAINT-NOTE           VALUE 'M'.               
00329              88  AT-CERT-CHANGE          VALUE 'X'.               
00330          16  AT-CALL-TYPE                PIC X.                   
00331              88  AT-PHONE-CALL-IN        VALUE 'I'.               
00332              88  AT-PHONE-CALL-OUT       VALUE 'O'.               
00333          16  AT-NOTE-CONTINUATION        PIC X.                   
00334              88  AT-CONTINUED-NOTE       VALUE 'X'.               
00335          16  FILLER                      PIC X(36).               
00336          16  AT-GEN-INFO-LAST-MAINT-DT   PIC XX.                  
00337          16  AT-GEN-INFO-LAST-UPDATED-BY PIC X(4).                
00338                                                                   
00339      12  AT-AUTO-PROMPT-TR  REDEFINES  AT-TRAILER-BODY.           
00340          16  AT-PROMPT-LINE-1            PIC X(60).               
00341          16  AT-PROMPT-LINE-2            PIC X(60).               
00342          16  AT-PROMPT-START-DT          PIC XX.                  
00343          16  AT-PROMPT-END-DT            PIC XX.                  
00344          16  FILLER                      PIC X(35).               
00345          16  AT-PROMPT-LAST-MAINT-DT     PIC XX.                  
00346          16  AT-PROMPT-LAST-UPDATED-BY   PIC X(4).                
00347                                                                   
00348      12  AT-DENIAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.           
00349          16  AT-DENIAL-INFO-1            PIC X(60).               
00350          16  AT-DENIAL-INFO-2            PIC X(60).               
00351          16  AT-DENIAL-DT                PIC XX.                  
00352          16  AT-RETRACTION-DT            PIC XX.                  
00353          16  AT-DENIAL-REASON-CODE       PIC X(4).                
00354          16  FILLER                      PIC X(31).               
00355          16  AT-DENIAL-LAST-MAINT-DT     PIC XX.                  
00356          16  AT-DENIAL-LAST-UPDATED-BY   PIC X(4).                
00357                                                                   
00358      12  AT-INCURRED-CHG-TR  REDEFINES  AT-TRAILER-BODY.          
00359          16  AT-OLD-INCURRED-DT          PIC XX.                  
00360          16  AT-OLD-REPORTED-DT          PIC XX.                  
00361          16  AT-OLD-ESTABLISHED-DT       PIC XX.                  
00362          16  AT-OLD-TOTAL-PAID           PIC S9(7)V99     COMP-3. 
00363          16  AT-OLD-DAYS-PAID            PIC S9(4)        COMP.   
00364          16  AT-OLD-NO-OF-PMTS           PIC S9(3)        COMP-3. 
00365          16  AT-OLD-PAID-THRU-DT         PIC XX.                  
00366          16  AT-LAST-PMT-MADE-DT         PIC XX.                  
00367          16  FILLER                      PIC X(26).               
00368          16  AT-OLD-DIAG-CODE            PIC X(6).                
00369          16  AT-TRAILER-CNT-AT-CHG       PIC S9(4)        COMP.   
00370          16  AT-OLD-ITD-PAID-EXPENSE     PIC S9(5)V99     COMP-3. 
00371          16  AT-OLD-CHARGABLE-EXPENSE    PIC S9(5)V99     COMP-3. 
00372          16  AT-OLD-INIT-MAN-RESV        PIC S9(7)V99     COMP-3. 
00373          16  AT-OLD-CURRENT-MAN-RESV     PIC S9(7)V99     COMP-3. 
00374          16  AT-OLD-ADDL-MAN-RESV        PIC S9(7)V99     COMP-3. 
00375          16  AT-OLD-DIAG-DESCRIP         PIC X(60).               
00376          16  FILLER                      PIC X(25).               
00377          16  AT-INCURRED-LAST-UPDATED-BY PIC X(4).                
00378                                                                   
00379      12  AT-FORM-CONTROL-TR  REDEFINES  AT-TRAILER-BODY.          
00380          16  AT-FORM-SEND-ON-DT          PIC XX.                  
00381          16  AT-FORM-FOLLOW-UP-DT        PIC XX.                  
00382          16  AT-FORM-RE-SEND-DT          PIC XX.                  
00383          16  AT-FORM-ANSWERED-DT         PIC XX.                  
00384          16  AT-FORM-PRINTED-DT          PIC XX.                  
00385          16  AT-FORM-REPRINT-DT          PIC XX.                  
00386          16  AT-FORM-TYPE                PIC X.                   
00387              88  INITIAL-FORM                  VALUE '1'.         
00388              88  PROGRESS-FORM                 VALUE '2'.         
00389          16  AT-INSTRUCT-LN-1            PIC X(28).               
00390          16  AT-INSTRUCT-LN-2            PIC X(28).               
00391          16  AT-INSTRUCT-LN-3            PIC X(28).               
00392          16  AT-FORM-ADDR-SEQ-NO         PIC S9(4)      COMP.     
00393          16  AT-FORM-ADDRESS             PIC X.                   
00394              88  FORM-TO-INSURED              VALUE 'I'.          
00395              88  FORM-TO-ACCOUNT              VALUE 'A'.          
00396              88  FORM-TO-OTHER-1              VALUE 'O'.          
00397              88  FORM-TO-OTHER-2              VALUE 'Q'.          
00398          16  AT-RELATED-1.                                        
00399              20 AT-REL-CARR-1            PIC X.                   
00400              20 AT-REL-CLAIM-1           PIC X(7).                
00401              20 AT-REL-CERT-1            PIC X(11).               
00402          16  AT-RELATED-2.                                        
00403              20 AT-REL-CARR-2            PIC X.                   
00404              20 AT-REL-CLAIM-2           PIC X(7).                
00405              20 AT-REL-CERT-2            PIC X(11).               
00406          16  AT-EMP-FORM-SEND-ON-DT      PIC XX.                  
00407          16  AT-PHY-FORM-SEND-ON-DT      PIC XX.                  
00408          16  AT-EMP-FORM-ANSWERED-DT     PIC XX.                  
00409          16  AT-PHY-FORM-ANSWERED-DT     PIC XX.                  
00410          16  AT-FORM-REM-PRINT-DT        PIC XX.                  
00411                                                                   
00412          16  FILLER                      PIC X(11).               
00413          16  AT-FORM-LAST-MAINT-DT       PIC XX.                  
00414          16  AT-FORM-LAST-UPDATED-BY     PIC X(4).                
00415 ******************************************************************
00179      EJECT                                                        
00180 *                            COPY ELCCNTL.
                                                                        
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
00181      EJECT                                                        
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA LETTER-ARCHIVE
                                ACTIVITY-TRAILERS CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1782' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00183                                                                   
00184      MOVE EIBDATE               TO DC-JULIAN-YYDDD
00185      MOVE '5'                   TO DC-OPTION-CODE
00186      PERFORM 9700-DATE-LINK     THRU 9700-EXIT
00187      MOVE DC-GREG-DATE-1-EDIT   TO SAVE-DATE
00188      MOVE DC-BIN-DATE-1         TO SAVE-BIN-DATE
00189      MOVE SPACES                TO DL34-PROCESS-TYPE
110402     .
00191                                                                   
00192  0100-RETRIEVE-LOOP.                                              
00193      
      * EXEC CICS HANDLE CONDITION                                   
00194 *         ENDDATA(200-END-DATA)                                   
00195 *         NOTFND (300-NOT-FOUND)                                  
00196 *    END-EXEC
           MOVE '"$&I                  ! " #00002850' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00197                                                                   
00198      
      * EXEC CICS RETRIEVE                                           
00199 *         INTO  (PROGRAM-INTERFACE-BLOCK)                         
00200 *         LENGTH(PI-COMM-LENGTH)                                  
00201 *    END-EXEC
           MOVE '0*I L                 &   #00002855' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00202                                                                   
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                            
00205          IF DL34-PROCESS-TYPE IS EQUAL TO SPACES                  
00206              MOVE 'O'                TO DL34-PROCESS-TYPE         
00207              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID           
00208              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID     
00209              MOVE PI-PROCESSOR-ID    TO DL34-USERID               
00210              MOVE SPACES             TO DL34-PRINT-LINE           
00211              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID  
00212              
      * EXEC CICS LINK                                       
00213 *                PROGRAM    ('DLO034')                            
00214 *                COMMAREA   (DLO034-COMMUNICATION-AREA)           
00215 *                LENGTH     (DLO034-REC-LENGTH)                   
00216 *            END-EXEC 
           MOVE 'DLO034' TO DFHEIV1
           MOVE '."C                   ''   #00002868' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                                                       
00217              IF DL34-RETURN-CODE NOT = 'OK'                       
00218                  MOVE  '**DLO034 OPEN ERROR - ABORT**'            
00219                                      TO ERROR-LINE                
00220                  PERFORM 400-SEND-TEXT                            
00221                  
      * EXEC CICS RETURN                                 
00222 *                END-EXEC
           MOVE '.(                    $   #00002877' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
110402             END-IF
110402         END-IF
110402     END-IF
00223                                                                   
00203                                                                   
00224      PERFORM 1000-INITIALIZE         THRU 1000-EXIT
110402     
110402     IF REPRINT-FOR-CLAIM
110402         MOVE PI-PRINT-CARRIER       TO ACTV-CARRIER    
110402         MOVE PI-CLAIM-NO            TO ACTV-CLAIM  
110402         MOVE PI-CERT-NO             TO ACTV-CERT-NO   
110402         MOVE ZEROS                  TO ACTV-SEQ  
110402         MOVE ACTV-PARTIAL-KEY       TO WS-SAVE-ACTV-PARTIAL-KEY 
110402         
      * EXEC CICS HANDLE CONDITION  
110402*             NOTOPEN (8870-ACTV-NOT-OPEN)  
110402*             NOTFND (8880-ACTV-REC-NOTFND)   
110402*        END-EXEC
           MOVE '"$JI                  ! # #00002892' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
110402         
      * EXEC CICS STARTBR 
110402*             DATASET (ACTV-ID) 
110402*             RIDFLD (ACTV-KEY) 
110402*        END-EXEC
           MOVE 0 TO DFHEIV11
           MOVE '&,         G          &   #00002896' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
110402                         
110402         SET TRLR-BROWSE-STARTED     TO TRUE
110402         
      * EXEC CICS HANDLE CONDITION  
110402*             ENDFILE (1200-READ-ARCH-NOW)
110402*        END-EXEC
           MOVE '"$''                   ! $ #00002902' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
110402         PERFORM 1100-GET-ARCH-NUMS  THRU 1100-EXIT
110402             UNTIL TRLR-BROWSE-ENDED
110402         PERFORM 1200-READ-ARCH-NOW  THRU 1200-EXIT
110402****** Processing will never return here, there is a      
110402******   go to 200-end-data in 1200-read-arch-now because of the
110402******   possibility of an endfile handle cond branching there
110402     END-IF
00225      PERFORM 6000-BROWSE-ARCHIVE-HEADERS THRU 6099-EXIT
           .
00226                                                                   
00227  200-END-DATA.                                                    
00228                                                                   
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                            
00231          MOVE 'C'                TO DL34-PROCESS-TYPE             
00232          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID               
00233          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID         
00234          MOVE PI-PROCESSOR-ID    TO DL34-USERID                   
00235          MOVE SPACES             TO DL34-PRINT-LINE               
00236                                     DL34-OVERRIDE-PRINTER-ID      
00237          
      * EXEC CICS LINK                                           
00238 *            PROGRAM    ('DLO034')                                
00239 *            COMMAREA   (DLO034-COMMUNICATION-AREA)               
00240 *            LENGTH     (DLO034-REC-LENGTH)                       
00241 *        END-EXEC 
           MOVE 'DLO034' TO DFHEIV1
           MOVE '."C                   ''   #00002924' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                                                           
00242          IF DL34-RETURN-CODE NOT = 'OK'                           
00243              MOVE  '**DLO034 CLOSE ERROR - ABORT**'               
00244                                  TO ERROR-LINE                    
00245              PERFORM 400-SEND-TEXT
110402         END-IF
110402     END-IF
00246                                                                   
00247      
      * EXEC CICS RETURN                                             
00248 *    END-EXEC
           MOVE '.(                    $   #00002936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
110402     .
00249                                                                   
00250  300-NOT-FOUND.                                                   
00251      MOVE 'NO COMMUNICATION AREA FOUND' TO ERROR-LINE
00252      PERFORM 400-SEND-TEXT
00253      GO TO 200-END-DATA
110402     .
00254                                                                   
00255  400-SEND-TEXT.                                                   
00256      
      * EXEC CICS SEND TEXT                                          
00257 *        FROM  (ERROR-LINE)                                       
00258 *        LENGTH(70)                                               
00259 *    END-EXEC
           MOVE 70 TO DFHEIV11
           MOVE '8&      T       H   F -   #00002947' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERROR-LINE, 
                 DFHEIV11, 
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
110402     .
00261  1000-INITIALIZE.                                                 
00262      MOVE +80                    TO WS-LINE-LEN
00263      MOVE SAVE-BIN-DATE          TO CURRENT-SAVE
00264                                                                   
00265      MOVE PI-COMPANY-CD          TO ARCH-CO2                      
00266                                     ACTV-CO                       
00267                                     ARCH-CO
00268                                                                   
00269      MOVE PI-ENTRY-CODES         TO OPTION-CODES
110402     .
00270                                                                   
110402 1000-EXIT.
110402     EXIT.
110402 1100-GET-ARCH-NUMS.
110402     
      * EXEC CICS READNEXT  
110402*         DATASET (ACTV-ID)     
110402*         RIDFLD (ACTV-KEY) 
110402*         SET (ADDRESS OF ACTIVITY-TRAILERS)  
110402*    END-EXEC
           MOVE 0 TO DFHEIV11
           MOVE '&.S                   )   #00002966' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
110402     IF AT-CONTROL-PRIMARY (1:20) = WS-SAVE-ACTV-PARTIAL-KEY
110402         IF (CORRESPONDENCE-TR  AND
110402             AT-LETTER-ARCHIVE-NO > ZEROS) 
110402             ADD +1                    TO SUB
110402             MOVE AT-LETTER-ARCHIVE-NO TO WS-ARCHIVE-NUM (SUB)    
110402         END-IF
110402     ELSE
110402         SET TRLR-BROWSE-ENDED         TO TRUE
110402     END-IF
110402     .
110402 1100-EXIT.
110402     EXIT.
110402 1200-READ-ARCH-NOW.
110402     
      * EXEC CICS ENDBR       
110402*         DATASET(ACTV-ID) 
110402*    END-EXEC
           MOVE 0 TO DFHEIV11
           MOVE '&2                    $   #00002984' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
110402     IF SUB > +0 
110402         CONTINUE
110402     ELSE
110402         MOVE 'NO ARCHIVE LETTERS TO PRINT FOR THIS CLAIM' TO     
110402                                                       ERROR-LINE 
110402         PERFORM 400-SEND-TEXT
110402         GO TO 200-END-DATA
110402     END-IF
110402        
110402     MOVE SUB                            TO WS-SAVE-FINAL-SUB 
110402     MOVE +1                             TO SUB
110402     PERFORM 6000-BROWSE-ARCHIVE-HEADERS THRU 6099-EXIT
110402         UNTIL SUB > WS-SAVE-FINAL-SUB
110402     GO TO 200-END-DATA
110402     .
110402 1200-EXIT.
110402     EXIT.
00272  6000-BROWSE-ARCHIVE-HEADERS.                                     
00273      MOVE '1'                      TO ARCH-REC-TYPE2
00274      MOVE ZEROS                    TO ARCH-NUMBER2 
00275                                       ARCH-SEQ2
00276      IF PRINT-INITIAL OR PRINT-FOLLOW-UP         
00277          MOVE PI-STARTING-ARCH-NO  TO ARCH-NUMBER2
110402     END-IF
110402     IF REPRINT-FOR-CLAIM
110402         MOVE WS-ARCHIVE-NUM (SUB) TO ARCH-NUMBER2
110402         ADD +1                    TO SUB
110402     END-IF
110402     .
00278                                                                   
00279  6005-START-BROWSE.                                               
00280      
      * EXEC CICS HANDLE CONDITION                                   
00281 *         NOTFND (6099-EXIT)                                      
00282 *         NOTOPEN(8860-ARCH2-NOT-OPEN)                            
00283 *    END-EXEC
           MOVE '"$IJ                  ! % #00003018' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00284                                                                   
00285      
      * EXEC CICS STARTBR                                            
00286 *         DATASET(ARCH-ID2)                                       
00287 *         RIDFLD (ARCH-KEY2)                                      
00288 *    END-EXEC
           MOVE 0 TO DFHEIV11
           MOVE '&,         G          &   #00003023' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID2, 
                 ARCH-KEY2, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00289                                                                   
00290      MOVE 'Y' TO HEADER-BROWSE-STARTED
110402     .
00291                                                                   
00292  6010-READ-NEXT.                                                  
00293      
      * EXEC CICS HANDLE CONDITION                                   
00294 *         NOTFND  (6050-END-BR)                                   
00295 *         ENDFILE (6050-END-BR)                                   
00296 *         NOTOPEN (8860-ARCH2-NOT-OPEN)                           
00297 *    END-EXEC
           MOVE '"$I''J                 ! & #00003032' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00298                                                                   
00299      
      * EXEC CICS READNEXT                                           
00300 *         DATASET (ARCH-ID2)                                      
00301 *         RIDFLD (ARCH-KEY2)          
00302 *         SET (ADDRESS OF LETTER-ARCHIVE) 
00303 *    END-EXEC
           MOVE 0 TO DFHEIV11
           MOVE '&.S                   )   #00003038' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID2, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ARCH-KEY2, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00304                                                                   
00305      IF (PI-COMPANY-CD = ARCH-CO2 AND   
00306          ARCH-REC-TYPE2 = '1')
110402         CONTINUE                                                 
110402*        NEXT SENTENCE                                            
00308      ELSE
00309          GO TO 6050-END-BR
110402     END-IF
00310                                                                   
00311      ADD +1                      TO WS-RECORD-COUNT
00312      IF WS-RECORD-COUNT IS GREATER THAN +50                       
00313          MOVE +0                 TO WS-RECORD-COUNT               
00314          
      * EXEC CICS DELAY                                          
00315 *            INTERVAL  (WS-DELAY-INTERVAL)                        
00316 *        END-EXEC
           MOVE '0$I                   &   #00003055' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DELAY-INTERVAL, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
110402     END-IF
00317                                                                   
00318      IF PRINT-BY-CARR 
00319          IF LA-CARRIER IS EQUAL  TO PI-PRINT-CARRIER  
110402             CONTINUE                                             
110402*            NEXT SENTENCE                                        
00321          ELSE                                                     
00322              GO TO 6010-READ-NEXT
110402         END-IF
110402     END-IF
110402     IF REPRINT-FOR-CLAIM
110402         IF LA-CLAIM-NO = PI-CLAIM-NO
110402             IF LA-INITIAL-PRINT-DATE NOT = LOW-VALUES
110402                 MOVE ARCH-NUMBER2             TO ARCHIVE-SAVE    
110402                 PERFORM 8200-END-BROWSE THRU 8200-EXIT       
110402                 PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7299-EXIT
110402                 MOVE 'N'                      TO END-BROWSE-SW   
020503             ELSE
020503                 PERFORM 8200-END-BROWSE THRU 8200-EXIT       
110402             END-IF
110402         END-IF
110402         GO TO 6099-EXIT
110402     END-IF
00323                                                                   
00324      MOVE ARCH-NUMBER2           TO ARCHIVE-SAVE
00325                                                                   
00326      MOVE SPACES                 TO WS-LETTER-FORM
00327                                                                   
00328      IF PRINT-INITIAL                                             
00329          PERFORM 6900-INITIAL-CHECKS THRU 6999-EXIT               
00330          IF BROWSE-ENDED                                          
00331              MOVE 'N'            TO END-BROWSE-SW                 
00332              ADD 1               TO ARCH-NUMBER2                  
00333              GO TO 6005-START-BROWSE                              
00334          ELSE                                                     
00335              GO TO 6010-READ-NEXT
110402         END-IF
110402     END-IF
00336                                                                   
00337      IF PRINT-FOLLOW-UP                                           
00338          PERFORM 6100-FOLLOW-UP-CHECKS THRU 6199-EXIT             
CIDMOD         IF BROWSE-ENDED                                          
CIDMOD             MOVE 'N'            TO END-BROWSE-SW                 
CIDMOD             ADD 1               TO ARCH-NUMBER2                  
CIDMOD             GO TO 6005-START-BROWSE                              
CIDMOD         ELSE                                                     
00344              GO TO 6010-READ-NEXT
110402         END-IF
110402     END-IF
00345                                                                   
00346      IF REPRINT-LETTERS                                           
00347         PERFORM 6200-REPRINT-CHECKS THRU 6299-EXIT                
CIDMOD         IF BROWSE-ENDED                                          
CIDMOD             MOVE 'N'            TO END-BROWSE-SW                 
CIDMOD             ADD 1               TO ARCH-NUMBER2                  
CIDMOD             GO TO 6005-START-BROWSE                              
CIDMOD         ELSE                                                     
00353              GO TO 6010-READ-NEXT
110402         END-IF
110402     END-IF
00354                                                                   
00355      PERFORM 6300-LABEL-CHECKS THRU 6399-EXIT
CIDMOD     IF BROWSE-ENDED                                              
CIDMOD         MOVE 'N'                TO END-BROWSE-SW                 
CIDMOD         ADD 1                   TO ARCH-NUMBER2                  
CIDMOD         GO TO 6005-START-BROWSE                                  
CIDMOD     ELSE                                                         
00361          GO TO 6010-READ-NEXT
110402     END-IF
00362                                                                   
110402     .
00363  6050-END-BR.                                                     
00364      IF PRINT-LABELS                                              
00365         MOVE 'X'                 TO WS-PROG-END
              PERFORM ELPRTCVP         THRU ELPRTCVP-EXIT
110402     END-IF
00367                                                                   
00368      IF HEADER-BROWSE-STARTED = 'Y'                               
00369         MOVE 'N'                 TO HEADER-BROWSE-STARTED         
00370         
      * EXEC CICS ENDBR                                           
00371 *            DATASET(ARCH-ID2)                                    
00372 *       END-EXEC
           MOVE 0 TO DFHEIV11
           MOVE '&2                    $   #00003137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID2, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
110402     END-IF
110402     .
00373                                                                   
00374  6099-EXIT.                                                       
00375       EXIT.                                                       
00376                                                                   
00378  6100-FOLLOW-UP-CHECKS.                                           
CIDMOD     IF LA-INITIAL-PRINT-DATE = LOW-VALUES AND                    
CIDMOD        SAVE-ARCH-NO = ZEROS                                      
CIDMOD          MOVE ARCH-NUMBER2 TO SAVE-ARCH-NO
110402     END-IF
00382                                                                   
00383      IF LA-RESEND-DATE = LOW-VALUES                               
00384         GO TO 6199-EXIT
110402     END-IF
00385                                                                   
00386      IF LA-RESEND-DATE NOT GREATER CURRENT-SAVE AND               
00387         LA-RESEND-PRINT-DATE = LOW-VALUES                         
110402        CONTINUE                                                  
110402*       NEXT SENTENCE                                             
00389      ELSE                                                         
00390         GO TO 6199-EXIT
110402     END-IF
00391                                                                   
CIDMOD     IF PI-PRINT-DATE-BIN NOT = LOW-VALUES                        
CIDMOD         IF LA-RESEND-DATE NOT = PI-PRINT-DATE-BIN                
CIDMOD             GO TO 6199-EXIT
110402         END-IF
110402     END-IF 
CIDMOD                                                                  
CIDMOD                                                                  
CIDMOD     PERFORM 8200-END-BROWSE THRU 8200-EXIT
CIDMOD                                                                  
CIDMOD     IF SAVE-ARCH-NO = ZEROS                                      
CIDMOD        MOVE ARCH-NUMBER2 TO SAVE-ARCH-NO
110402     END-IF
CIDMOD                                                                  
00402      MOVE SPACES                 TO CORRESPOND-SW                 
00403                                     HEADER-SW
00404                                                                   
00405      PERFORM 8100-READ-HEADER THRU 8199-EXIT
00406                                                                   
00407      MOVE LA-NO-OF-COPIES        TO WS-COPIES
00408                                                                   
00409      PERFORM 8000-READ-CORRESPOND THRU 8099-EXIT
00410                                                                   
00411      IF AT-LETTER-ANSWERED-DT = LOW-VALUES AND                    
00412         AT-AUTO-RE-SEND-DT = LA-RESEND-DATE                       
00413         PERFORM 6500-UPDATE-RESEND-PRINT   THRU 6599-EXIT         
00414         PERFORM 6800-REWRITE-HEADER        THRU 6899-EXIT         
00415         PERFORM 6600-UPDATE-CORR-TRLR      THRU 6699-EXIT         
00416         PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7299-EXIT         
00417                 WS-COPIES TIMES                                   
00418         PERFORM 7500-SYNCPOINT             THRU 7599-EXIT         
00419         GO TO 6199-EXIT
110402     END-IF
00420                                                                   
00421      IF AT-LETTER-ANSWERED-DT NOT = LOW-VALUES                    
00422         MOVE LOW-VALUES          TO  LA-RESEND-DATE               
00423                                      AT-AUTO-RE-SEND-DT           
CIDMOD*       PERFORM 6550-REWRITE-TRLR-NO-UP THRU 6550-EXIT            
CIDMOD        PERFORM 6800-REWRITE-HEADER        THRU 6899-EXIT         
CIDMOD        PERFORM 6600-UPDATE-CORR-TRLR      THRU 6699-EXIT         
00426         GO TO 6199-EXIT
110402     END-IF
00427                                                                   
00428      MOVE AT-AUTO-RE-SEND-DT     TO LA-RESEND-DATE
00429                                                                   
00430      IF LA-RESEND-DATE NOT GREATER CURRENT-SAVE                   
110402        CONTINUE                                                  
110402*       NEXT SENTENCE                                             
00432      ELSE
00433         PERFORM 6800-REWRITE-HEADER THRU 6899-EXIT                
00434         PERFORM 7900-RELEASE-CORR   THRU 7900-EXIT                
00435         GO TO 6199-EXIT
110402     END-IF
00436                                                                   
00437      PERFORM 6500-UPDATE-RESEND-PRINT   THRU 6599-EXIT
00438      PERFORM 6800-REWRITE-HEADER        THRU 6899-EXIT
00439      PERFORM 6600-UPDATE-CORR-TRLR      THRU 6699-EXIT
00440      PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7299-EXIT
00441      PERFORM 7500-SYNCPOINT             THRU 7599-EXIT
110402     .
00442                                                                   
00443  6199-EXIT.                                                       
00444       EXIT.                                                       
00446  6200-REPRINT-CHECKS.                                             
00447      IF PI-LETTER-TYPE = 'I'                                      
00448          IF LA-RESEND-PRINT-DATE GREATER THAN LOW-VALUES          
00449              GO TO 6299-EXIT
110402         END-IF
110402     END-IF
00450                                                                   
00451      IF PI-LETTER-TYPE = 'P'                                      
00452          IF LA-RESEND-PRINT-DATE = LOW-VALUES                     
00453              GO TO 6299-EXIT
110402         END-IF
110402     END-IF
00454                                                                   
00455      IF LA-INITIAL-PRINT-DATE   = PI-PRINT-DATE-BIN OR            
00456         LA-RESEND-PRINT-DATE    = PI-PRINT-DATE-BIN               
00457         MOVE ARCH-NUMBER2        TO ARCHIVE-SAVE                  
00458         MOVE LA-NO-OF-COPIES     TO WS-COPIES                     
CIDMOD        PERFORM 8200-END-BROWSE THRU 8200-EXIT                    
00460         PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7299-EXIT         
00461                 WS-COPIES TIMES                                   
00462         PERFORM 7500-SYNCPOINT             THRU 7599-EXIT
110402     END-IF
110402     .
00463                                                                   
00464  6299-EXIT.                                                       
00465       EXIT.                                                       
00467  6300-LABEL-CHECKS.                                               
00468      IF FIRST-TIME                                                
00469         PERFORM 6400-ALIGNMENT-PRINT THRU 6450-EXIT
110402     END-IF
00470                                                                   
00471      MOVE SPACES TO WS-LABEL-HOLD-AREA
00472                                                                   
00473      IF LA-INITIAL-PRINT-DATE   = PI-PRINT-DATE-BIN OR            
00474         LA-RESEND-PRINT-DATE    = PI-PRINT-DATE-BIN               
00475          MOVE ARCH-NUMBER2        TO ARCHIVE-SAVE                 
CIDMOD         MOVE LA-CLAIM-NO         TO WDS-CLAIM-NO                 
CIDMOD         MOVE LA-CERT-NO          TO WDS-CERT-NO                  
CIDMOD         PERFORM 8200-END-BROWSE THRU 8200-EXIT                   
00479          PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7299-EXIT        
00480          PERFORM 7500-SYNCPOINT             THRU 7599-EXIT
110402     END-IF
110402     .
00481                                                                   
00482  6399-EXIT.                                                       
00483       EXIT.                                                       
00485  6400-ALIGNMENT-PRINT.                                            
00486      MOVE ALL '*'                TO WS-LABEL-HOLD-AREA
00487      MOVE SPACES                 TO WS-PASSED-CNTL-CHAR           
00488                                     WS-LABEL-LINES (6)
00489                                                                   
00490      PERFORM 6480-MOVE-TO-PRINT THRU 6499-EXIT 6 TIMES
00491                                                                   
110402     .
00492  6450-EXIT.                                                       
00493       EXIT.                                                       
00494                                                                   
00495  6480-MOVE-TO-PRINT.                                              
00496      MOVE SPACES                 TO  WS-PASSED-CNTL-CHAR
00497      MOVE WS-LABEL-LINES (1)     TO WS-PASSED-DATA
00498      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00499      MOVE WS-LABEL-LINES (2)     TO WS-PASSED-DATA
00500      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00501      MOVE WS-LABEL-LINES (3)     TO WS-PASSED-DATA
00502      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00503      MOVE WS-LABEL-LINES (4)     TO WS-PASSED-DATA
00504      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00505      MOVE WS-LABEL-LINES (5)     TO WS-PASSED-DATA
00506      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00507      MOVE WS-LABEL-LINES (6)     TO WS-PASSED-DATA                
00508      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
110402     . 
00509                                                                   
00510  6499-EXIT.                                                       
00511       EXIT.                                                       
00513  6500-UPDATE-RESEND-PRINT.                                        
00514                                                                   
00515      MOVE CURRENT-SAVE           TO  LA-RESEND-PRINT-DATE         
00516                                      LA-1ST-RESEND-PRINT-DT       
00517                                      AT-RESEND-PRINT-DATE
00518                                                                   
00519      MOVE LOW-VALUES             TO  LA-RESEND-DATE               
00520                                      AT-AUTO-RE-SEND-DT
00521                                                                   
00522      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')                
110402         CONTINUE
110402*        NEXT SENTENCE                                            
00524      ELSE                                                         
00525          GO TO 6599-EXIT
110402     END-IF
00526                                                                   
00527      MOVE LA-INITIAL-PRINT-DATE  TO  DC-BIN-DATE-1
00528      MOVE LA-RESEND-PRINT-DATE   TO  DC-BIN-DATE-2
00529      MOVE '1'                    TO  DC-OPTION-CODE
00530      MOVE +0                     TO  DC-ELAPSED-MONTHS            
00531                                      DC-ELAPSED-DAYS
00532      PERFORM 9700-DATE-LINK   THRU    9700-EXIT
00533                                                                   
00534      IF DC-ELAPSED-DAYS IS NOT EQUAL TO +30                       
00535          GO TO 6599-EXIT
110402     END-IF
00536                                                                   
00537      MOVE CURRENT-SAVE           TO  DC-BIN-DATE-1
00538      MOVE '6'                    TO  DC-OPTION-CODE
00539      MOVE +0                     TO  DC-ELAPSED-MONTHS
00540      MOVE +30                    TO  DC-ELAPSED-DAYS
00541      PERFORM 9700-DATE-LINK   THRU    9700-EXIT
00542      IF NO-CONVERSION-ERROR                                       
00543          MOVE DC-BIN-DATE-1      TO  LA-RESEND-DATE               
00544                                      AT-AUTO-RE-SEND-DT           
00545          MOVE LOW-VALUES         TO  LA-RESEND-PRINT-DATE         
00546                                      AT-RESEND-PRINT-DATE
110402     END-IF
110402     .
00547                                                                   
00548  6599-EXIT.                                                       
00549       EXIT.                                                       
CIDMOD 6550-REWRITE-TRLR-NO-UP.                                         
CIDMOD     IF CORR-REC-FOUND                                            
CIDMOD         CONTINUE                                                 
CIDMOD     ELSE                                                         
CIDMOD         GO TO 6550-EXIT                                          
CIDMOD     END-IF
CIDMOD                                                                  
CIDMOD     
      * EXEC CICS REWRITE                                            
CIDMOD*         DATASET (ACTV-ID)                                       
CIDMOD*         FROM    (ACTIVITY-TRAILERS)                             
CIDMOD*    END-EXEC
           MOVE LENGTH OF ACTIVITY-TRAILERS TO DFHEIV11
           MOVE '&& L                  %   #00003350' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
110402     .
CIDMOD 6550-EXIT.                                                       
CIDMOD     EXIT.                                                        
CIDMOD                                                                  
00551  6600-UPDATE-CORR-TRLR.                                           
00552      IF CORR-REC-FOUND                                            
110402         CONTINUE                                                 
110402*        NEXT SENTENCE                                            
00554      ELSE 
00555          GO TO 6699-EXIT
110402     END-IF
00556                                                                   
00557      
      * EXEC CICS REWRITE                                            
00558 *         DATASET(ACTV-ID)                                        
00559 *         FROM(ACTIVITY-TRAILERS)                                 
00560 *    END-EXEC
           MOVE LENGTH OF ACTIVITY-TRAILERS TO DFHEIV11
           MOVE '&& L                  %   #00003366' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
110402     .
00561                                                                   
00562  6699-EXIT.                                                       
00563       EXIT.                                                       
00565  6800-REWRITE-HEADER.                                             
00566      
      * EXEC CICS HANDLE CONDITION                                   
00567 *        DUPKEY (6899-EXIT)                                       
00568 *    END-EXEC
           MOVE '"$$                   ! '' #00003375' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00569                                                                   
00570      
      * EXEC CICS REWRITE                                            
00571 *         DATASET(ARCH-ID)                                        
00572 *         FROM   (LETTER-ARCHIVE)                                 
00573 *    END-EXEC
           MOVE LENGTH OF LETTER-ARCHIVE TO DFHEIV11
           MOVE '&& L                  %   #00003379' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
110402     .
00574                                                                   
00575  6899-EXIT.                                                       
00576       EXIT.                                                       
00578  6900-INITIAL-CHECKS.                                             
CIDMOD     IF LA-RESEND-DATE NOT   = LOW-VALUES AND                     
CIDMOD        LA-RESEND-PRINT-DATE = LOW-VALUES AND                     
CIDMOD        SAVE-ARCH-NO = ZEROS                                      
CIDMOD          MOVE ARCH-NUMBER2 TO SAVE-ARCH-NO
110402     END-IF
CIDMOD                                                                  
CIDMOD     IF PI-PRINT-DATE-BIN NOT = LOW-VALUES                        
CIDMOD         IF LA-CREATION-DT  NOT =  PI-PRINT-DATE-BIN              
CIDMOD             GO TO 6999-EXIT
110402         END-IF
110402     END-IF
CIDMOD                                                                  
00588      IF LA-INITIAL-PRINT-DATE = LOW-VALUES                        
00589         MOVE ARCH-NUMBER2       TO ARCHIVE-SAVE                   
CIDMOD        PERFORM 8200-END-BROWSE THRU 8200-EXIT                    
00591         PERFORM 7300-UPDATE-ARCHIVE-HEADER THRU 7399-EXIT         
00592         IF HEADER-REC-FOUND                                       
00593            PERFORM 7200-PRINT-ARCHIVE-RECORDS THRU 7299-EXIT      
00594                    WS-COPIES TIMES                                
00595            PERFORM 7400-UPDATE-CORRESPOND-TRLR THRU 7499-EXIT     
00596            PERFORM 7500-SYNCPOINT             THRU 7599-EXIT
110402        END-IF
110402     END-IF
110402     .
00597                                                                   
00598  6999-EXIT.                                                       
00599       EXIT.                                                       
00600                                                                   
00602  7200-PRINT-ARCHIVE-RECORDS.                                      
00603      MOVE PI-COMPANY-CD          TO ARCH-CO
00604      MOVE SPACES                 TO WS-PROG-END
00605      MOVE ARCHIVE-SAVE           TO ARCH-NUMBER
00606                                                                   
00607      IF PRINT-LABELS                                              
00608         MOVE '2'                 TO ARCH-REC-TYPE                 
00609         SET L-INDX               TO 1                             
00610      ELSE
00611         MOVE '3'                 TO ARCH-REC-TYPE
110402     END-IF
00612                                                                   
00613      MOVE ZEROS                  TO ARCH-SEQ
00614                                                                   
00615      
      * EXEC CICS HANDLE CONDITION                                   
00616 *         NOTFND  (7250-CHECK-FIRST-SW)                           
00617 *         ENDFILE (7250-CHECK-FIRST-SW)                           
00618 *         NOTOPEN (8890-ARCH-NOT-OPEN)                            
00619 *    END-EXEC
           MOVE '"$I''J                 ! ( #00003430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00620                                                                   
00621      
      * EXEC CICS STARTBR                                            
00622 *         DATASET (ARCH-ID)                                       
00623 *         RIDFLD  (ARCH-KEY)                                      
00624 *    END-EXEC
           MOVE 0 TO DFHEIV11
           MOVE '&,         G          &   #00003436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID, 
                 ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00625                                                                   
00626      MOVE ARCH-PARTIAL-KEY       TO ARCH-SAVE-KEY
00627      MOVE 'Y'                    TO TEXT-BROWSE-STARTED
00628                                                                   
00629      MOVE '1'                    TO  WS-PRINT-AREA
00630      MOVE ZERO                   TO  WS-PRINT-SW
110402     .
00631                                                                   
00632  7210-READ-NEXT.                                                  
00633      
      * EXEC CICS READNEXT                                           
00634 *         DATASET(ARCH-ID)                                        
00635 *         RIDFLD (ARCH-KEY)                                       
00636 *         SET (ADDRESS OF LETTER-ARCHIVE) 
00637 *    END-EXEC
           MOVE 0 TO DFHEIV11
           MOVE '&.S                   )   #00003449' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00638                                                                   
00639      IF ARCH-PARTIAL-KEY NOT = ARCH-SAVE-KEY                      
00640         GO TO 7250-CHECK-FIRST-SW
110402     END-IF
00641                                                                   
00649      IF PRINT-LABELS                                              
00650         MOVE LA-ADDRESS-LINE     TO WS-LABEL-LINES (L-INDX)       
00651         SET L-INDX UP BY 1                                        
00652         GO TO 7210-READ-NEXT
110402     END-IF
00653                                                                   
110402*    IF PI-COMPANY-ID NOT = 'FIA'                                 
110402*        GO TO 7220-NOT-FIA
110402*    END-IF
00656                                                                   
110402*    IF WS-PRINT-SW NOT = ZERO                                    
110402*        GO TO 7220-NOT-FIA
110402*    END-IF
00659                                                                   
110402*    IF WS-LETTER-FORM NOT = 'EMPS'                               
110402*        GO TO 7220-NOT-FIA
110402*    END-IF
00662                                                                   
110402*    IF LA-TEXT-LINE  = SPACES                                    
110402*        GO TO 7210-READ-NEXT
110402*    END-IF
00665                                                                   
110402*    MOVE +1                     TO  WS-PRINT-SW
110402*7220-NOT-FIA.                                                    
110402*    IF PI-COMPANY-ID  = 'MON'                                    
110402*        MOVE SPACES                 TO WS-DATA-SHIFT-AREA        
110402*        MOVE LA-TEXT-LINE           TO WS-DATA-SHIFT             
110402*        MOVE WS-DATA-SHIFT-AREA     TO WS-PASSED-DATA            
110402*    ELSE
00674      MOVE LA-TEXT-LINE               TO WS-PASSED-DATA
110402*    END-IF
00675                                                                   
00676      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00677                                                                   
00678      IF SKIP-TO-NEXT-PAGE                                         
00679          MOVE '1'                TO  WS-PRINT-AREA                
00680      ELSE 
00681          MOVE SPACES             TO  WS-PRINT-AREA
110402     END-IF
00682                                                                   
00683      IF LA-SKIP-CONTROL GREATER '00' AND LESS '99'                
00684         MOVE SPACES              TO WS-PRINT-AREA                 
00685         MOVE LA-SKIP-CONTROL     TO WS-SKIP                       
00686         PERFORM ELPRTCVP THRU ELPRTCVP-EXIT   WS-SKIP TIMES
110402     END-IF
00687                                                                   
00688      GO TO 7210-READ-NEXT
110402     .
00689                                                                   
00690  7250-CHECK-FIRST-SW.                                             
00691      IF PRINT-LABELS                                              
00692         GO TO 7260-LABEL-PRINT
110402     END-IF
00693                                                                   
00694      MOVE 'X'                 TO WS-PROG-END
CIDMOD     PERFORM ELPRTCVP THRU ELPRTCVP-EXIT
00696                                                                   
00697      GO TO 7290-END-BR
110402     . 
00698                                                                   
00699  7260-LABEL-PRINT.                                                
00700      IF L-INDX = 1 OR                                             
00701         WS-LABEL-HOLD-AREA = SPACES                               
00702         GO TO 7290-END-BR
110402     END-IF
00703                                                                   
00704      PERFORM 6480-MOVE-TO-PRINT THRU 6499-EXIT
00705                                                                   
00706      GO TO 7290-END-BR
110402     .
00708                                                                   
00709  7290-END-BR.                                                     
00710      IF TEXT-BROWSE-STARTED = 'Y'                                 
00711         MOVE 'N'                 TO TEXT-BROWSE-STARTED           
00712         
      * EXEC CICS ENDBR                                           
00713 *            DATASET(ARCH-ID)                                     
00714 *       END-EXEC
           MOVE 0 TO DFHEIV11
           MOVE '&2                    $   #00003533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
110402     END-IF
00715                                                                   
110402     .
00716  7299-EXIT.                                                       
00717       EXIT.                                                       
00718  7300-UPDATE-ARCHIVE-HEADER.                                      
CIDMOD     IF SAVE-ARCH-NO = ZEROS                                      
CIDMOD        MOVE ARCH-NUMBER2 TO SAVE-ARCH-NO
110402     END-IF
CIDMOD                                                                  
00722      PERFORM 8100-READ-HEADER THRU 8199-EXIT
00723                                                                   
00724      IF HEADER-REC-FOUND                                          
110402        CONTINUE                                                  
110402*       NEXT SENTENCE                                             
00726      ELSE
00727         GO TO 7399-EXIT
110402     END-IF
00728                                                                   
00729      MOVE CURRENT-SAVE           TO LA-INITIAL-PRINT-DATE
00730      MOVE LA-NO-OF-COPIES        TO WS-COPIES
00731                                                                   
00732      
      * EXEC CICS HANDLE CONDITION                                   
00733 *        DUPKEY (7399-EXIT)                                       
00734 *    END-EXEC
           MOVE '"$$                   ! ) #00003558' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00735                                                                   
00736      
      * EXEC CICS REWRITE                                            
00737 *        DATASET (ARCH-ID)                                        
00738 *        FROM    (LETTER-ARCHIVE)                                 
00739 *    END-EXEC
           MOVE LENGTH OF LETTER-ARCHIVE TO DFHEIV11
           MOVE '&& L                  %   #00003562' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
110402     .
00740                                                                   
00741  7399-EXIT.                                                       
00742       EXIT.                                                       
00744  7400-UPDATE-CORRESPOND-TRLR.                                     
00745      MOVE SPACE                  TO CORRESPOND-SW
00746      PERFORM 8000-READ-CORRESPOND THRU 8099-EXIT
00747                                                                   
00748      IF CORR-REC-FOUND                                            
110402        CONTINUE                                                  
110402*       NEXT SENTENCE                                             
00750      ELSE
00751         GO TO 7499-EXIT
110402     END-IF
00752                                                                   
00753      MOVE CURRENT-SAVE           TO AT-INITIAL-PRINT-DATE
00754                                                                   
00755      
      * EXEC CICS REWRITE                                            
00756 *        DATASET(ACTV-ID)                                         
00757 *        FROM(ACTIVITY-TRAILERS)                                  
00758 *    END-EXEC
           MOVE LENGTH OF ACTIVITY-TRAILERS TO DFHEIV11
           MOVE '&& L                  %   #00003583' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
110402     .
00759                                                                   
00760                                                                   
00761  7499-EXIT.                                                       
00762       EXIT.                                                       
00764  7500-SYNCPOINT.                                                  
00765                                                                   
00766      
      * EXEC CICS SYNCPOINT                                          
00767 *         END-EXEC
           MOVE '6"                    !   #00003594' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
110402     .
00768                                                                   
00769  7599-EXIT.                                                       
00770       EXIT.                                                       
00771                                                                   
CIDMOD 7900-RELEASE-CORR.                                               
CIDMOD     
      * EXEC CICS UNLOCK                                             
CIDMOD*        DATASET(ACTV-ID)                                         
CIDMOD*    END-EXEC
           MOVE '&*                    #   #00003602' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
110402     .
CIDMOD                                                                  
CIDMOD 7900-EXIT.                                                       
CIDMOD      EXIT.                                                       
CIDMOD                                                                  
00780  8000-READ-CORRESPOND.                                            
00781      
      * EXEC CICS HANDLE CONDITION                                   
00782 *         NOTOPEN(8870-ACTV-NOT-OPEN)                             
00783 *         NOTFND (8050-REC-NOT-FOUND)                             
00784 *    END-EXEC
           MOVE '"$JI                  ! * #00003611' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00785                                                                   
00786      
      * EXEC CICS READ                                               
00787 *         DATASET(ACTV-ID)                                        
00788 *         RIDFLD (ACTV-KEY)                                       
00789 *         SET (ADDRESS OF ACTIVITY-TRAILERS)    
00790 *         UPDATE                                                  
00791 *    END-EXEC
           MOVE '&"S        EU         (   #00003616' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00792                                                                   
00793      MOVE AT-STD-LETTER-FORM     TO  WS-LETTER-FORM
00794                                                                   
00795      GO TO 8099-EXIT
110402     .
00796                                                                   
00797  8050-REC-NOT-FOUND.                                              
00798      MOVE '1'                    TO CORRESPOND-SW
110402     .
00799                                                                   
00800  8099-EXIT.                                                       
00801       EXIT.                                                       
00803  8100-READ-HEADER.                                                
00804      MOVE ARCHIVE-SAVE           TO ARCH-NUMBER
00805      MOVE '1'                    TO ARCH-REC-TYPE
00806      MOVE ZEROS                  TO ARCH-SEQ
00807                                                                   
00808      
      * EXEC CICS HANDLE CONDITION                                   
00809 *         NOTOPEN(8890-ARCH-NOT-OPEN)                             
00810 *         NOTFND (8150-NOT-FOUND)                                 
00811 *    END-EXEC
           MOVE '"$JI                  ! + #00003639' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00812                                                                   
00813      
      * EXEC CICS READ                                               
00814 *        DATASET (ARCH-ID)                                        
00815 *        RIDFLD  (ARCH-KEY)                                       
00816 *        SET     (ADDRESS OF LETTER-ARCHIVE)                      
00817 *        UPDATE                                                   
00818 *    END-EXEC
           MOVE '&"S        EU         (   #00003644' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00819                                                                   
00820      MOVE LA-CARRIER             TO ACTV-CARRIER
00821      MOVE LA-CLAIM-NO            TO ACTV-CLAIM
00822      MOVE LA-CERT-NO             TO ACTV-CERT-NO
00823      MOVE LA-CORR-TRLR-SEQ       TO ACTV-SEQ
00824      GO TO 8199-EXIT
110402     . 
00825                                                                   
00826  8150-NOT-FOUND.                                                  
00827      MOVE '1'                    TO HEADER-SW
110402     .
00828                                                                   
00829  8199-EXIT.                                                       
00830       EXIT.                                                       
00831                                                                   
CIDMOD 8200-END-BROWSE.                                                 
CIDMOD                                                                  
CIDMOD     
      * EXEC CICS ENDBR                                              
CIDMOD*        DATASET (ARCH-ID2)    
CIDMOD*    END-EXEC
           MOVE 0 TO DFHEIV11
           MOVE '&2                    $   #00003667' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID2, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
CIDMOD                                                                  
CIDMOD     MOVE 'Y'                    TO END-BROWSE-SW
110402     .
CIDMOD                                                                  
CIDMOD 8200-EXIT.                                                       
CIDMOD     EXIT.                                                        
00842                                                                   
00843  8860-ARCH2-NOT-OPEN.                                             
00844      MOVE 'LETTER ARCHIVE FILE NOT OPEN - ELARCH2' TO ERROR-LINE
00845      PERFORM 400-SEND-TEXT
00846      GO TO 200-END-DATA
110402     .
00848  8870-ACTV-NOT-OPEN.                                              
00849      MOVE 'ACTIVITY TRAILER FILE NOT OPEN - ELTRLR' TO ERROR-LINE
00850      PERFORM 400-SEND-TEXT
00851      GO TO 200-END-DATA
110402     .
00852                                                                   
110402 8880-ACTV-REC-NOTFND. 
110402     MOVE 'ACTIVITY TRLR RECORD NOT FOUND FOR THIS CLAIM' TO
110402                                                       ERROR-LINE 
110402     PERFORM 400-SEND-TEXT
110402     GO TO 200-END-DATA
110402     .
00852                                                                   
00853  8890-ARCH-NOT-OPEN.                                              
00854      MOVE 'LETTER ARCHIVE FILE NOT OPEN - ELARCH' TO ERROR-LINE
00855      PERFORM 400-SEND-TEXT
00856      GO TO 200-END-DATA
110402     .
00857                                                                   
00859  9700-DATE-LINK.                                                  
00860      MOVE LINK-ELDATCV TO PGM-NAME
00861      
      * EXEC CICS LINK                                               
00862 *        PROGRAM (PGM-NAME)                                       
00863 *        COMMAREA(DATE-CONVERSION-DATA)                           
00864 *        LENGTH  (DC-COMM-LENGTH)                                 
00865 *    END-EXEC
           MOVE '."C                   ''   #00003703' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
110402     .
00866                                                                   
00867  9700-EXIT.                                                       
00868       EXIT.                                                       
00871                                                                   
CIDMOD*                                COPY ELPRTCVP.
                                                                        
00001 ******************************************************************
00002 ***                                                              *
00003 ***                          ELPRTCVP.                           *
00004 ***                          VMOD=2.003                          *
00005 ***                                                              *
00006 ***     COPY MEMBER FOR TERMINAL ONLINE PRINT ROUTINE.           *
00007 ***     THIS ROUTINE WILL ACCOMODATE PRINTING TO A 3270          *
00008 ***     TERMINAL PRINTER. A BUFFER OF UP TO 1920 CHARACTERS      *
00009 ***     IS ACCUMULATED AND PRINTED COLLECTIVELY.                 *
00010 ***                                                              *
00011 ***     THIS ROUTINE TO BE USED ONLY WITH ACCOMPANIMENT          *
00012 ***      OF THE WORKING-STORAGE COPY MEMBER ( ELPRTCVD )         *
00013 ***     THE HOST PROGRAM MUST INITIALIZE THE FOLLOWING 3 FIELDS  *
00014 ***      FROM THE ABOVE COPY MEMBER FOR THIS PROCEDURE TO BE     *
00015 ***      SUCCESSFUL.                                             *
00016 ***      05  WS-LINE-LEN    PIC  S9(4)  COMP  VALUE +80.         *
00017 ***                         LENGTH OF THE LINE TO BE PRINTED     *
00018 ***                         DEFAULT IS 80, YOU CAN USE ANY NUMBER*
00019 ***                         UP TO 132.  THIS FIELD IS ONLY ACCEP-*
00020 ***                         TED THE FIRST TIME THRU THE ROUTINE. *
00021 ***      05  WS-PROG-END    PIC  X  VALUE SPACES.                *
00022 ***                         PROGRAM END SWITCH. INITIALIZED      *
00023 ***                         TO SPACE-     MOVE IN ANY NONBLANK   *
00024 ***                         TO IT WHEN PROGRAM IS FINISHED.      *
00025 ***      05  WS-PRINT-AREA.                                      *
00026 ***          10  WS-PASSED-CNTL-CHAR     PIC X.                  *
00027 ***          10  WS-PASSED-DATA          PIC X(132).             *
00028 ***                         USE THE DATA TO BE PRINTED IN THE    *
00029 ***                         WS-PASSED-DATA.                      *
00030 ***                         USE THE STANDARD CARRIAGE CONTROL    *
00031 ***                         CHARACTER IN THE WS-PASSED-CNTL-CHAR *
00032 ***                           SINGLE-SPACE            VALUE ' '  *
00033 ***                           DOUBLE-SPACE            VALUE '0'  *
00034 ***                           TRIPLE-SPACE            VALUE '-'  *
00035 ***                           TOP-PAGE                VALUE '1'  *
00036 ***      NOTE: A LINE COUNT IS PROVIDED IN FIELDNAME -WS-LINE-CNT*
00037 ***            THE USE OF THIS FIELD IS OPTIONAL.                *
00038 ***            THIS ROUTINE WILL ONLY ADD 1, 2, OR 3             *
00039 ***            TO THIS COUNT DEPENDING ON THE WS-PASSED-CNTL-CHAR*
00040 ***            AND RESET THE COUNT TO ZERO WHEN TOP-PAGE         *
00041 ***            CONDITION.                                        *
00042 ***                                                              *
00043 ******************************************************************
00044                                                                   
00045  ELPRTCVP.                                                        
00046                                                                   
pemuni*    IF PI-COMPANY-ID IS EQUAL TO 'DMD' OR 'CID'                  
pemuni     IF PI-COMPANY-ID IS EQUAL TO 'DMD' OR 'XXX'                  
00048          MOVE 'P'                TO DL34-PROCESS-TYPE             
00049          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID         
00050          MOVE PI-PROCESSOR-ID    TO DL34-USERID                   
00051          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID               
00052          MOVE WS-PRINT-AREA      TO DL34-PRINT-LINE               
00053          MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID      
00054                                                                   
00055          
      * EXEC CICS LINK                                           
00056 *            PROGRAM    ('DLO034')                                
00057 *            COMMAREA   (DLO034-COMMUNICATION-AREA)               
00058 *            LENGTH     (DLO034-REC-LENGTH)                       
00059 *        END-EXEC 
           MOVE 'DLO034' TO DFHEIV1
           MOVE '."C                   ''   #00003770' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DLO034-COMMUNICATION-AREA, 
                 DLO034-REC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                                                           
00060                                                                   
00061             IF DL34-RETURN-CODE = 'OK'                            
00062                 GO TO ELPRTCVP-EXIT                               
00063             ELSE                                                  
00064 *               MOVE '8339'     TO EMI-ERROR ?????ERROR MESSAGE???
00065                 GO TO ELPRTCVP-EXIT.                              
00066                                                                   
00067      IF NOT FIRST-TIME                                            
00068          GO TO ELPRTCVP-020.                                      
00069                                                                   
00070      IF WS-LINE-LEN NOT GREATER ZERO                              
00071          GO TO ELPRTCVP-EXIT.                                     
00072                                                                   
00073      MOVE '2'                    TO WS-FIRST-TIME-SW.             
00074      MOVE LOW-VALUES             TO WS-BUFFER-AREA.               
00075                                                                   
00076      SET BUFFER-INDEX TO +1                                       
00077                                                                   
00078      IF EIBTRMID IS EQUAL TO 'AFLP'                               
00079          NEXT SENTENCE                                            
00080      ELSE                                                         
00081          IF NOT TOP-PAGE                                          
00082              MOVE T-TP           TO WS-BUFFER-BYTE (BUFFER-INDEX) 
00083              SET BUFFER-INDEX UP BY +1.                           
00084                                                                   
00085  ELPRTCVP-020.                                                    
00086      IF WS-PROG-END = SPACES                                      
00087          GO TO ELPRTCVP-030.                                      
00088                                                                   
00089      MOVE SPACES                 TO WS-PROG-END.                  
00090                                                                   
00091      IF BUFFER-INDEX GREATER +1                                   
00092          PERFORM ELPRTCVP-PRINT-BUFFER THRU ELPRTCVP-PRINT-EXIT.  
00093                                                                   
00094      MOVE '1'                    TO WS-FIRST-TIME-SW.             
00095                                                                   
00096      GO TO ELPRTCVP-EXIT.                                         
00097                                                                   
00098  ELPRTCVP-030.                                                    
00099      IF WS-PASSED-DATA = SPACES                                   
00100          SET PRT-INDEX TO +1                                      
00101          GO TO ELPRTCVP-050.                                      
00102                                                                   
00103      SET PRT-INDEX TO WS-LINE-LEN.                                
00104                                                                   
00105  ELPRTCVP-040.                                                    
00106      IF WS-PRINT-BYTE (PRT-INDEX) NOT = SPACES                    
00107          GO TO ELPRTCVP-050.                                      
00108                                                                   
00109      IF PRT-INDEX GREATER +1                                      
00110          SET PRT-INDEX DOWN BY +1                                 
00111          GO TO ELPRTCVP-040.                                      
00112                                                                   
00113  ELPRTCVP-050.                                                    
00114      SET WS-LINE-LENGTH TO PRT-INDEX.                             
00115      SET BUFFER-INDEX2 TO BUFFER-INDEX.                           
00116      SET BUFFER-INDEX2 UP BY WS-LINE-LENGTH.                      
00117                                                                   
00118      IF BUFFER-INDEX2 NOT LESS WS-BUFFER-SIZE                     
00119          PERFORM ELPRTCVP-PRINT-BUFFER THRU ELPRTCVP-PRINT-EXIT.  
00120                                                                   
00121      IF TRIPLE-SPACE                                              
00122           ADD +2  TO  WS-LINE-CNT                                 
00123           MOVE T-SS           TO WS-BUFFER-BYTE (BUFFER-INDEX)    
00124                                  WS-BUFFER-BYTE (BUFFER-INDEX + 1)
00125           SET BUFFER-INDEX UP BY +2.                              
00126                                                                   
00127      IF DOUBLE-SPACE                                              
00128           ADD +1  TO  WS-LINE-CNT                                 
00129           MOVE T-SS             TO WS-BUFFER-BYTE (BUFFER-INDEX)  
00130           SET BUFFER-INDEX UP BY +1.                              
00131                                                                   
00132      ADD +1 TO WS-LINE-CNT                                        
00133 ************************************************************      
00134 *     BYPASS NEW LINE SYMBOL                               *      
00135 *        IF FIRST BUFFER SENT AND TOP-OF-FORM SET.         *      
00136 *     OR IF FIRST LINE OF SUBSEQUENT BUFFERS.              *      
00137 ************************************************************      
00138                                                                   
00139      IF (BUFFER-INDEX GREATER +1 AND                              
00140          WS-BUFFER-BYTE (BUFFER-INDEX - 1) = T-TP)  OR            
00141          FIRST-LINE-NEXT-BUFFER                                   
00142          MOVE ZERO               TO WS-FIRST-TIME-SW              
00143      ELSE                                                         
00144          MOVE T-SS               TO WS-BUFFER-BYTE (BUFFER-INDEX) 
00145          SET BUFFER-INDEX UP BY +1.                               
00146                                                                   
00147 **   NOTE, SINGLE SPACE IS REQUIRED BEFORE TOP PAGE CHAR          
00148                                                                   
00149      IF TOP-PAGE                                                  
00150          MOVE +1                TO WS-LINE-CNT                    
00151          MOVE T-TP              TO WS-BUFFER-BYTE (BUFFER-INDEX)  
00152          SET BUFFER-INDEX UP BY +1.                               
00153                                                                   
00154      SET PRT-INDEX TO +1.                                         
00155                                                                   
00156  ELPRTCVP-060.                                                    
00157      MOVE WS-PRINT-BYTE (PRT-INDEX)                               
00158                                  TO WS-BUFFER-BYTE (BUFFER-INDEX).
00159      SET BUFFER-INDEX UP BY +1.                                   
00160                                                                   
00161      IF PRT-INDEX LESS WS-LINE-LENGTH                             
00162          SET PRT-INDEX UP BY +1                                   
00163          GO TO ELPRTCVP-060.                                      
00164                                                                   
00165  ELPRTCVP-EXIT.                                                   
00166      EXIT.                                                        
00167                                                                   
00168  ELPRTCVP-PRINT-BUFFER.                                           
00169      IF WS-BUFFER-BYTE (BUFFER-INDEX - 1) = T-SS                  
00170         MOVE SPACE               TO WS-BUFFER-BYTE (BUFFER-INDEX) 
00171         SET BUFFER-INDEX UP BY 1.                                 
00172                                                                   
00173      MOVE  T-EM                  TO  WS-BUFFER-BYTE (BUFFER-INDEX)
00174      SET WS-BUFFER-LENGTH TO BUFFER-INDEX.                        
00175                                                                   
00176      
      * EXEC CICS SEND                                               
00177 *        FROM    (WS-BUFFER-AREA)                                 
00178 *        LENGTH  (WS-BUFFER-LENGTH)                               
00179 *        CTLCHAR (WS-WCC-CNTL)                                    
00180 *        ERASE                                                    
00181 *    END-EXEC.
           MOVE '$$    C E         L F ,   #00003891' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-BUFFER-AREA, 
                 WS-BUFFER-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 WS-WCC-CNTL, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
                                                               
00182                                                                   
00183      SET BUFFER-INDEX TO +1.                                      
00184      MOVE '2'                    TO WS-FIRST-TIME-SW.             
00185                                                                   
00186  ELPRTCVP-PRINT-EXIT.                                             
00187      EXIT.                                                        
00188                                                                   
0
       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1782' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 200-END-DATA,
                     300-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 8870-ACTV-NOT-OPEN,
                     8880-ACTV-REC-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1200-READ-ARCH-NOW
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 6099-EXIT,
                     8860-ARCH2-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 6050-END-BR,
                     6050-END-BR,
                     8860-ARCH2-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 6899-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 7250-CHECK-FIRST-SW,
                     7250-CHECK-FIRST-SW,
                     8890-ARCH-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 7399-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 8870-ACTV-NOT-OPEN,
                     8050-REC-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 8890-ARCH-NOT-OPEN,
                     8150-NOT-FOUND
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1782' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
