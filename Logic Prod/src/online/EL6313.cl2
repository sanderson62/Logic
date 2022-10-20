00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL6313.                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 12/07/94 16:01:04.                 
00007 *                            VMOD=2.017.                          
00008 *                                                                 
00009 *AUTHOR.     LOGIC,INC.                                           
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
00023 *REMARKS. TRANSACTION - EXB3 - NEW BUSINESS REVIEW AND CORRECTIONS
00024 *                              SECOND SCREEN.                     
00025                                                                   
013107******************************************************************
013107*                   C H A N G E   L O G
013107*
013107* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
013107*-----------------------------------------------------------------
013107*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
013107* EFFECTIVE    NUMBER
013107*-----------------------------------------------------------------
013107* 013107  CR2007010300001  PEMA  ADD PERFORM STATEMENT
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
072312* 072312  CR2011022800001  AJRA  NAPERSOFT MISC
121712* 121712  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
011413* 011413  IR2012122700003  AJRA  ADD CRTO ISSUE/CANCEL INDICATOR
100213* 100213  CR2013090300001  AJRA  NAPERSOFT PHASE 2
121713* 121713  CR2013090300001  AJRA  NO CHANGES TO PRIMARY NAME
100217* 100217  CR2016091600001  PEMA  ADD EDIT FOR ZIP CODE
041320* 041320  CR2020040200001  PEMA  PENDING BUSINESS JOURNALING
013107******************************************************************
00026  ENVIRONMENT DIVISION.                                            
00027                                                                   
00028      EJECT                                                        
00029  DATA DIVISION.                                                   
00030  WORKING-STORAGE SECTION.                                         
00031  77  FILLER  PIC X(32)  VALUE '********************************'. 
00032  77  FILLER  PIC X(32)  VALUE '*    EL6313 WORKING STORAGE    *'. 
00033  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.017 *********'. 
121712 77  WS-CERT-TRL-REC-NOT-FOUND   PIC S9       VALUE +0.
121712     88  CERT-TRL-REC-NOT-FOUND     VALUE +1.
100217 77  a1                          pic s999 comp-3 value +0.

00034                                                                   
00035      COPY ELCSCTM.                                                
00036      COPY ELCSCRTY.                                               
00037                                                                   
100217 01  P pointer.
100217 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
100217 01  var-ptr pointer.
100217 01  env-var-len                 pic 9(4)  binary.
100217 01  rc                          pic 9(9)  binary.
100217
100217 01  WS-KIXSYS.
100217     05  WS-KIX-FIL1             PIC X(10).
100217     05  WS-KIX-APPS             PIC X(10).
100217     05  WS-KIX-ENV              PIC X(10).
100217     05  WS-KIX-MYENV            PIC X(10).
100217     05  WS-KIX-SYS              PIC X(10).

00038  01  STANDARD-AREAS.                                              
00039      12  GETMAIN-SPACE       PIC X       VALUE SPACE.             
00040      12  EL631F              PIC X(8)    VALUE 'EL631F'.          
00041      12  MAPSET-EL6313S      PIC X(8)    VALUE 'EL6313S'.         
00042      12  SCREEN-NUMBER       PIC X(6)    VALUE 'EL631F'.          
00043      12  TRANS-EXB3          PIC X(4)    VALUE 'EXB3'.            
00044      12  THIS-PGM            PIC X(8)    VALUE 'EL6313'.          
00045      12  PGM-NAME            PIC X(8).                            
00046      12  TIME-IN             PIC S9(7).                           
00047      12  TIME-OUT-R  REDEFINES TIME-IN.                           
00048          16  FILLER          PIC X.                               
00049          16  TIME-OUT        PIC 99V99.                           
00050          16  FILLER          PIC XX.                              
00051      12  LINK-EL001          PIC X(8)    VALUE 'EL001'.           
00052      12  LINK-EL004          PIC X(8)    VALUE 'EL004'.           
00053      12  LINK-EL050        PIC X(8)      VALUE 'EL050'.           
00054      12  XCTL-EL005          PIC X(8)    VALUE 'EL005'.           
00055      12  XCTL-EL010          PIC X(8)    VALUE 'EL010'.           
00056      12  XCTL-EL626          PIC X(8)    VALUE 'EL626'.           
00057      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.         
00058      12  FILE-ID-ERPNDB      PIC X(8)    VALUE 'ERPNDB'.          
00059      12  FILE-ID-ERPNDM      PIC X(8)    VALUE 'ERPNDM'.          
00060      12  FILE-ID-ELCERT      PIC X(8)    VALUE 'ELCERT'.          
00061      12  FILE-ID-ELCNTL      PIC X(8)    VALUE 'ELCNTL'.          
121712     12  CRTT-ID             PIC X(8)    VALUE 'ELCRTT'.
00062      12  WS-CURRENT-DT       PIC X(8)    VALUE SPACES.            
00063      12  WS-CURRENT-BIN-DT   PIC XX      VALUE SPACES.            
00064      12  WS-TERM-IN-DAYS-SW  PIC X.                               
00065          88  WS-TERM-IN-DAYS-FOUND       VALUE 'Y'.               
00066      12  WS-ADD-ADDRESS-SW   PIC X       VALUE 'N'.               
00067          88  WS-ADDRESS-ADDED            VALUE 'Y'.               
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL              VALUE +00.
               88  RESP-ERROR               VALUE +01.
               88  RESP-NOTFND              VALUE +13.
               88  RESP-NOTOPEN             VALUE +19.
               88  RESP-ENDFILE             VALUE +20.
00068                                                                   
00069      EJECT                                                        
00070                                                                   
00071  01  ERROR-MESSAGES.                                              
00072      12  ER-0004                 PIC X(4)  VALUE '0004'.          
00073      12  ER-0008                 PIC X(4)  VALUE '0008'.          
00074      12  ER-0029                 PIC X(4)  VALUE '0029'.          
00075      12  ER-0042                 PIC X(4)  VALUE '0042'.          
00076      12  ER-0070                 PIC X(4)  VALUE '0070'.          
           12  ER-2209                 PIC X(4)  VALUE '2209'.
00077      12  ER-2223                 PIC X(4)  VALUE '2223'.          
           12  ER-2228                 PIC X(4)  VALUE '2228'.
00078      12  ER-2239                 PIC X(4)  VALUE '2239'.          
00079      12  ER-2600                 PIC X(4)  VALUE '2600'.          
00080      12  ER-2625                 PIC X(4)  VALUE '2625'.          
00081      12  ER-2695                 PIC X(4)  VALUE '2695'.          
00082      12  ER-2725                 PIC X(4)  VALUE '2725'.          
100213     12  ER-3269                 PIC X(4)  VALUE '3269'.
00083      12  ER-7822                 PIC X(4)  VALUE '7822'.          
00084      12  ER-8204                 PIC X(4)  VALUE '8204'.          
00085                                                                   
00086      EJECT                                                        
00087                                                                   
00088  01  ACCESS-KEYS.                                                 
00089      12  ERPNDB-KEY.                                              
00090          16  ERPNDB-COMP-CD          PIC X     VALUE SPACE.       
00091          16  ERPNDB-ENTRY-BATCH      PIC X(6)  VALUE SPACES.      
00092          16  ERPNDB-BATCH-SEQ        PIC S9(4) VALUE +1 COMP.     
00093          16  ERPNDB-BATCH-CHG-SEQ    PIC S9(4) VALUE +0 COMP.     
00094                                                                   
00095      12  ERPNDB-RECORD-LENGTH        PIC S9(4) COMP VALUE +585.   
00096                                                                   
00097      12  ERPNDM-KEY.                                              
00098          16  ERPNDM-COMP-CD          PIC X     VALUE SPACE.       
00099          16  ERPNDM-ENTRY-BATCH      PIC X(6)  VALUE SPACES.      
00100          16  ERPNDM-BATCH-SEQ        PIC S9(4) VALUE +1 COMP.     
00101          16  ERPNDM-BATCH-CHG-SEQ    PIC S9(4) VALUE +0 COMP.     
00102                                                                   
CIDMOD*    12  ERPNDM-RECORD-LENGTH        PIC S9(4) COMP VALUE +250.   
CIDMOD     12  ERPNDM-RECORD-LENGTH        PIC S9(4) COMP VALUE +374.   
00104                                                                   
           12  ELCRTO-KEY.                                              
               16  ELCRTO-COMPANY-CD       PIC X.     
               16  ELCRTO-CARRIER          PIC X.     
               16  ELCRTO-GROUPING         PIC X(6).  
               16  ELCRTO-STATE            PIC XX.    
               16  ELCRTO-ACCOUNT          PIC X(10). 
               16  ELCRTO-CERT-EFF-DT      PIC XX.    
               16  ELCRTO-CERT-NO.                    
                   20  ELCRTO-CERT-PRIME   PIC X(10). 
                   20  ELCRTO-CERT-SFX     PIC X.
               16  ELCRTO-RECORD-TYPE      PIC X.
               16  ELCRTO-SEQ-NO           PIC 9(4)  BINARY.     

00105      12  ELCERT-KEY.                                              
00106          16  ELCERT-COMPANY-CD       PIC X     VALUE SPACE.       
00107          16  ELCERT-CARRIER          PIC X     VALUE SPACE.       
00108          16  ELCERT-GROUPING         PIC X(6)  VALUE SPACE.       
00109          16  ELCERT-STATE            PIC XX    VALUE SPACE.       
00110          16  ELCERT-ACCOUNT          PIC X(10) VALUE SPACE.       
00111          16  ELCERT-CERT-EFF-DT      PIC XX    VALUE SPACE.       
00112          16  ELCERT-CERT-NO.                                      
00113              20  ELCERT-CERT-PRIME   PIC X(10) VALUE SPACE.       
00114              20  ELCERT-CERT-SFX     PIC X     VALUE SPACE.       
00115                                                                   
00116      12  ELCERT-RECORD-LENGTH        PIC S9(4) COMP VALUE +450.   
00117                                                                   
121712
121712     12  ELCRTT-KEY.
121712         16  ELCRTT-PRIMARY          PIC X(33).
121712         16  ELCRTT-REC-TYPE         PIC X(1).
121712     12  ELCRTT-RECORD-LENGTH        PIC S9(4) COMP VALUE +552.
121712     12  ELCRTT-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +575.
041320     12  ERPNDB-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +585.
041320     12  WS-JOURNAL-RECORD-LENGTH    PIC S9(4) COMP VALUE +0000.
121712
           12  ELCNTL-KEY.
               16  ELCNTL-COMPANY-ID   PIC X(3)  VALUE SPACES.
               16  ELCNTL-REC-TYPE     PIC X     VALUE SPACES.
               16  ELCNTL-ACCESS.
                   20  FILLER          PIC XX.
                   20  FILLER          PIC XX.
               16  ELCNTL-SEQ          PIC S9(4) VALUE +0 COMP.

00125  01  WORK-AREA.                                                   
00126      12  DEEDIT-FIELD            PIC X(15).                       
00127      12  FILLER REDEFINES DEEDIT-FIELD.                           
00128          16  FILLER              PIC X(4).                        
00129          16  DEEDIT-FIELD-X11    PIC X(11).                       
00130      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD PIC S9(15).       
CIDMOD     12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD PIC S9(13)V99.    
CIDMOD     12  DEEDIT-FIELD-V3 REDEFINES DEEDIT-FIELD PIC S9(12)V9(3).  
CIDMOD     12  DEEDIT-FIELD-V4 REDEFINES DEEDIT-FIELD PIC S9(11)V9(4).  
CIDMOD     12  DEEDIT-FIELD-V5 REDEFINES DEEDIT-FIELD PIC S9(10)V9(5).  
00131      12  FILLER REDEFINES DEEDIT-FIELD.                           
00132          16  FILLER              PIC X(8).                        
00133          16  DEEDIT-FIELD-RATE   PIC S99V9(5).                    
00134      12  FILLER REDEFINES DEEDIT-FIELD.                           
00135          16  FILLER              PIC X(7).                        
00136          16  WS-XRATE-OUT        PIC X(8).                        
00137      12  FILLER REDEFINES DEEDIT-FIELD.                           
00138          16  FILLER              PIC X(7).                        
00139          16  WS-RATE-OUT         PIC ZZ.99999.                    
00140                                                                   
00141      12  WS-EDIT-CODE                PIC X(4)  VALUE SPACES.      
           12  WS-CONVERTED-BIRTH      PIC XX    VALUE LOW-VALUES.

           12  CENTURY-ADJ             PIC S9(08) VALUE +38400 COMP.
           12  WS-WORK-BIN-RED         PIC S9(08) VALUE +0 COMP.
           12  FILLER REDEFINES WS-WORK-BIN-RED.
               16  FILLER              PIC XX.
               16  WS-WORK-BIN-DT      PIC XX.

00143      12  WS-SAVE-INPUT-FIELDS.                                    
00144                                                                   
00145          16  WS-JNTAGE               PIC S99     VALUE +0  COMP-3.
00146          16  WS-PHONE                PIC S9(12)  VALUE +0  COMP-3.
00147                                                                   
00148      12  WS-TIME                     PIC 9(6)     VALUE 0.        
00149      12  WS-HR-MINS-SECS REDEFINES WS-TIME.                       
00150          16  WS-HR-MINS              PIC 99V99.                   
00151          16  FILLER                  PIC XX.                      
00152                                                                   
00153      12  WS-EXP-DT-EDIT              PIC 99B99B99.                
00154                                                                   
00155      12  WS-ZIP-CODE.                                             
00156          16  WS-ZIP-1            PIC X.                           
00157              88  WS-CANADIAN-ZIP    VALUE 'A' THRU 'Z'.           
00158          16  WS-ZIP-2-3          PIC XX.                          
00159          16  WS-ZIP-4            PIC X.                           
00160          16  WS-ZIP-5            PIC X.                           
00161          16  WS-ZIP-6            PIC X.                           
00162          16  FILLER              PIC X(4).                        
00163      12  WS-ZIP-AM-1  REDEFINES  WS-ZIP-CODE.                     
00164          16  WS-ZIP-AM-1-CODE    PIC X(5).                        
00165          16  WS-ZIP-AM-1-PLUS4   PIC X(4).                        
00166          16  FILLER              PIC X.                           
00167      12  WS-ZIP-AM-2  REDEFINES  WS-ZIP-CODE.                     
00168          16  WS-ZIP-AM-2-CODE    PIC X(5).                        
00169          16  WS-ZIP-AM-2-DASH    PIC X.                           
00170          16  WS-ZIP-AM-2-PLUS4   PIC X(4).                        
00171      12  WS-ZIP-CAN-1  REDEFINES  WS-ZIP-CODE.                    
00172          16  WS-ZIP-CAN-1-POST1  PIC XXX.                         
00173          16  WS-ZIP-CAN-1-POST2  PIC XXX.                         
00174          16  FILLER              PIC X(4).                        
00175      12  WS-ZIP-CAN-2  REDEFINES  WS-ZIP-CODE.                    
00176          16  WS-ZIP-CAN-2-POST1  PIC XXX.                         
00177          16  FILLER              PIC X.                           
00178          16  WS-ZIP-CAN-2-POST2  PIC XXX.                         
00179          16  FILLER              PIC XXX.                         
00180                                                                   
00181      12  WS-LFRTHDG.                                              
00182          16  WS-LFRT-OVERRIDE-L2     PIC XX       VALUE 'LF'.     
00183          16  FILLER                  PIC X(10)    VALUE           
00184              '-RATE DEV.'.                                        
00185                                                                   
00186      12  WS-AHRTHDG.                                              
00187          16  WS-AHRT-OVERRIDE-L2     PIC XX       VALUE 'AH'.     
00188          16  FILLER                  PIC X(10)    VALUE           
00189              '-RATE DEV.'.                                        
00190                                                                   
00191      12  WS-LFDVHDG.                                              
00192          16  WS-LFDV-OVERRIDE-L2     PIC XX       VALUE 'LF'.     
00193          16  FILLER                  PIC X(12)    VALUE           
00194              '-RATE DEV. %'.                                      
00195                                                                   
00196      12  WS-AHDVHDG.                                              
00197          16  WS-AHDV-OVERRIDE-L2     PIC XX       VALUE 'AH'.     
00198          16  FILLER                  PIC X(12)    VALUE           
00199              '-RATE DEV. %'.                                      
00200                                                                   
00201      12  WS-LFCMHDG.                                              
00202          16  WS-LFCM-OVERRIDE-L2     PIC XX       VALUE 'LF'.     
00203          16  FILLER                  PIC X(6)     VALUE           
00204              '-COMM.'.                                            
00205                                                                   
00206      12  WS-AHCMHDG.                                              
00207          16  WS-AHCM-OVERRIDE-L2     PIC XX       VALUE 'AH'.     
00208          16  FILLER                  PIC X(12)    VALUE           
00209              '-COMM.'.                                            
00210                                                                   
00211      12  WS-SAVE-COMMON-ERRORS       PIC X(20)   VALUE SPACES.    
00212                                                                   
00213      12  WS-ERRORS-PRESENT-SW        PIC X        VALUE 'N'.      
00214          88 WS-ERRORS-PRESENT                     VALUE 'Y'.      
00215          88 WS-ERRORS-NOT-PRESENT                 VALUE 'N'.      
00216                                                                   
00217                                                                   
00218      12  WS-EDIT-PASS-LENGTH         PIC S9(4) VALUE +1024 COMP.  
00219      12  WS-SUB                      PIC S9(4) COMP VALUE +0000.  
00220      12  WS-SUB4                     PIC S9(4) COMP VALUE +0000.  
00221      12  WS-ERR-SW                   PIC X.                       
00222          88  STD-ERRORS                        VALUE 'S'.         
00223          88  TRN-ERRORS                        VALUE 'T'.         
00224                                                                   
00225      12  WS-SW-1                     PIC X     VALUE SPACE.       
00226      12  WS-SW-2                     PIC X     VALUE SPACE.       
00227                                                                   
00228      12  WS-DLO-RESIDENT-STATE.                                   
00229          16  DRES-STATE             PIC XX.                       
00230          16  DRES-RETURN-CODE       PIC XX.                       
00231      12  DRES-COMM-LENGTH           PIC S9(4) COMP VALUE +4.      
00232                                                                   
00233      12  WS-PFRATE                  PIC 9(4)V9(4).                
121712
121712     12  WS-AGE-FIELDS.
121712         16  WS-INS-AGE-SET             PIC X VALUE 'N'.
121712         16  WS-JNT-AGE-SET             PIC X VALUE 'N'.
121712         16  WS-INS-AGE-DEFAULTED       PIC X VALUE 'N'.
121712         16  WS-JNT-AGE-DEFAULTED       PIC X VALUE 'N'.
00234                                                                   
00235      EJECT                                                        
                                       COPY ELCCRTO.

00236      COPY ELCDATE.                                                
00237      EJECT                                                        
00238      COPY ELCLOGOF.                                               
00239      EJECT                                                        
00240      COPY ELCATTR.                                                
00241      EJECT                                                        
00242      COPY ELCEMIB.                                                
00243      EJECT                                                        
00244      COPY ELCINTF.                                                
00245      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                   
00246      COPY ELC631PI.                                               
00247             20 FILLER                 PIC X(94).                  
00248                                                                   
00249      EJECT                                                        
00250                                                                   
00251 ******************************************************************
00252 *         P A S S   A R E A   F O R   E D I T                    *
00253 ******************************************************************
00254                                                                   
00255  01  PASSED-RECORD.                                               
00256      12  RECORD-SAVE                  PIC X(585).                 
00257      COPY ELC50W1.                                                
00258      EJECT                                                        
00259      COPY ELCEDITC.                                               
00260                                                                   
00261      EJECT                                                        

041320                                 COPY ELCJPFX.
041320                                 PIC X(825).

00262      COPY ELCAID.                                                 
00263  01  FILLER    REDEFINES DFHAID.                                  
00264      12  FILLER              PIC X(8).                            
00265      12  PF-VALUES           PIC X       OCCURS 2.                
00266                                                                   
00267      EJECT                                                        
00268      COPY EL6313S.                                                
00269      EJECT                                                        
00270  LINKAGE SECTION.                                                 
00271  01  DFHCOMMAREA             PIC X(1300).                         
00272                                                                   
00273      EJECT                                                        
00274          COPY ERCPNDB.                                            
00275      EJECT                                                        
00276          COPY ERCPNDM.                                            
00277      EJECT                                                        
00278          COPY ELCCERT.                                            
00279          COPY ELCCNTL.
121712
121712         COPY ELCCRTT.
100217 01  var  pic x(30).

00281  PROCEDURE DIVISION.                                              
00282                                                                   
00283      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      
00284      MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6.         
00285      MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6.           
00286                                                                   
00287      MOVE +2                     TO EMI-NUMBER-OF-LINES.          
00288      MOVE 2                      TO EMI-SWITCH2.                  

100217     set P to address of KIXSYS
100217     CALL "getenv" using by value P returning var-ptr
100217     if var-ptr = null then
100217        display ' kixsys not set '
100217     else
100217        set address of var to var-ptr
100217        move 0 to env-var-len
100217        inspect var tallying env-var-len
100217          for characters before X'00' 
100217        unstring var (1:env-var-len) delimited by '/'
100217           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
100217              WS-KIX-SYS
100217        end-unstring
100217     end-if
100217     perform varying a1 from +1 by +1 until a1 > +10
100217        if ws-kix-myenv (a1:1) = low-values or high-values
100217           display ' found low or hi val '
100217           move spaces to ws-kix-myenv (a1:1)
100217        end-if
100217     end-perform

00289                                                                   
00290      IF EIBCALEN = 0                                              
00291          GO TO 8800-UNAUTHORIZED-ACCESS.                          
00292                                                                   
00293      IF PI-LIFE-OVERRIDE-L2 GREATER SPACES                        
00294          MOVE PI-LIFE-OVERRIDE-L2 TO WS-LFRT-OVERRIDE-L2          
00295          MOVE PI-LIFE-OVERRIDE-L2 TO WS-LFCM-OVERRIDE-L2          
00296          MOVE PI-LIFE-OVERRIDE-L2 TO WS-LFDV-OVERRIDE-L2.         
00297                                                                   
00298      IF PI-AH-OVERRIDE-L2 GREATER SPACES                          
00299          MOVE PI-AH-OVERRIDE-L2  TO WS-AHRT-OVERRIDE-L2           
00300          MOVE PI-AH-OVERRIDE-L2  TO WS-AHCM-OVERRIDE-L2           
00301          MOVE PI-AH-OVERRIDE-L2  TO WS-AHDV-OVERRIDE-L2.          
00302                                                                   
00303      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              
00304      MOVE '5'                    TO DC-OPTION-CODE.               
00305      PERFORM 9700-DATE-LINK.                                      
00306      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.            
00307      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.                
00308                                                                   
00309      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         
00310          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   
00311              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      
00312              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      
00313              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      
00314              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      
00315              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      
00316              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      
00317              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    
00318              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      
00319          ELSE                                                     
00320              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      
00321              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    
00322              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      
00323              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      
00324              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      
00325              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      
00326              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      
00327              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     
00328                                                                   
00329      MOVE PI-PREV-CONTROL-PRIMARY TO ERPNDB-KEY.                  
00330                                                                   
00331      IF EIBTRNID NOT = TRANS-EXB3                                 
00332          MOVE LOW-VALUES          TO EL631FI                      
00333          GO TO 7000-DISPLAY-ISSUES.                               
00334                                                                   
00335      EXEC CICS HANDLE CONDITION                                   
00336          PGMIDERR  (9600-PGMID-ERROR)                             
00337          ERROR     (9990-ABEND)                                   
00338      END-EXEC.                                                    
00339                                                                   
00340      IF EIBAID = DFHCLEAR                                         
00341          GO TO 9400-CLEAR.                                        
00342                                                                   
00343      EJECT                                                        
00344  0200-RECEIVE.                                                    
00345      IF EIBAID = DFHPA1 OR                                        
00346                  DFHPA2 OR                                        
00347                  DFHPA3                                           
00348          MOVE ER-0008            TO EMI-ERROR                     
00349          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00350          MOVE -1                 TO FPFENTRL                      
00351          GO TO 8200-SEND-DATAONLY.                                
00352                                                                   
00353      EXEC CICS RECEIVE                                            
00354          MAP      (PI-MAP-NAME)                                   
00355          MAPSET   (MAPSET-EL6313S)                                
00356          INTO     (EL631FI)                                       
00357      END-EXEC.                                                    
00358                                                                   
00359      INSPECT EL631FI CONVERTING '_' TO ' '.                       
00360                                                                   
00361      IF PI-MAP-NAME = EL631F                                      
00362          IF FPFENTRL GREATER ZERO                                 
00363              IF EIBAID NOT = DFHENTER                             
00364                  MOVE ER-0004    TO EMI-ERROR                     
00365                  MOVE AL-UNBOF   TO FPFENTRA                      
00366                  MOVE -1         TO FPFENTRL                      
00367                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
00368                  GO TO 8200-SEND-DATAONLY                         
00369              ELSE                                                 
00370                  IF FPFENTRI NUMERIC  AND                         
00371                    (FPFENTRI GREATER 0 AND LESS 25)               
00372                      MOVE PF-VALUES (FPFENTRI) TO EIBAID          
00373                  ELSE                                             
00374                      MOVE ER-0029  TO EMI-ERROR                   
00375                      MOVE AL-UNBOF TO FPFENTRA                    
00376                      MOVE -1       TO FPFENTRL                    
00377                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     
00378                      GO TO 8200-SEND-DATAONLY.                    
00379                                                                   
00380      EJECT                                                        
00381  0300-CHECK-PFKEYS.                                               
00382      IF EIBAID = DFHENTER                                         
00383          GO TO 1000-EDIT-MAPF.                                    
00384                                                                   
00385      IF EIBAID = DFHPF12                                          
00386          GO TO 9500-PF12.                                         
00387                                                                   
00388      IF EIBAID = DFHPF23                                          
00389          GO TO 8810-PF23.                                         
00390                                                                   
00391      IF EIBAID = DFHPF24                                          
00392          GO TO 9200-RETURN-MAIN-MENU.                             
00393                                                                   
00394      MOVE ER-0029 TO EMI-ERROR.                                   
00395      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00396                                                                   
00397      MOVE -1                     TO FPFENTRL.                     
00398      GO TO 8200-SEND-DATAONLY.                                    
00399                                                                   
00400      EJECT                                                        
00401                                                                   
00402  1000-EDIT-MAPF.                                                  
00403                                                                   
00404      IF PI-COMPANY-ID NOT = 'DMD'                                 
00405         MOVE ZEROS             TO RATEL                           
00406                                   PFRATEL                         
00407                                   RTCODEL                         
00408                                   RESSTL.                         
00409                                                                   
00410      IF LASTNML  GREATER +0 OR                                    
00411         FIRSTNML GREATER +0 OR                                    
00412         INITL    GREATER +0 OR                                    
00413         SOCSECL  GREATER +0 OR                                    
CIDMOD        CBNAMEL        > +0 OR
CIDMOD        CBADDRL        > +0 OR
CIDMOD        CBCITYL        > +0 OR
CIDMOD        CBSTATEL       > +0 OR
CIDMOD        CBZIPL         > +0 OR
00414         ADDRS1L  GREATER +0 OR                                    
00415         ADDRS2L  GREATER +0 OR                                    
00416         ICITYL   GREATER +0 OR                                    
00416         ISTATEL  GREATER +0 OR                                    
00417         ZIPCDEL  GREATER +0 OR                                    
00418         PHONEL   GREATER +0 OR                                    
00419         JNT1STL  GREATER +0 OR                                    
00420         JNTINITL GREATER +0 OR                                    
00421         JNTLSTL  GREATER +0 OR                                    
00422         JNTAGEL  GREATER +0 OR                                    
00423         BENFARYL GREATER +0 OR                                    
00424         POLFRML  GREATER +0 OR                                    
00425         PFRATEL  GREATER +0 OR                                    
00426         RTCODEL  GREATER +0 OR                                    
00427         RESSTL   GREATER +0 OR
              JNTDOBL        > +0
00428            NEXT SENTENCE                                          
00429         ELSE                                                      
00430            GO TO 7000-DISPLAY-ISSUES.                             
00431                                                                   
00432      IF NOT MODIFY-CAP                                            
00433          MOVE 'UPDATE'       TO SM-READ                           
00434          PERFORM 9995-SECURITY-VIOLATION                          
00435          MOVE ER-0070        TO EMI-ERROR                         
00436          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
00437          GO TO 8100-SEND-INITIAL-MAP-CONT.                        
00438                                                                   
00439      IF JNT1STL  GREATER +0                                       
00440         MOVE AL-UANON          TO JNT1STA.                        
00441                                                                   
00442      IF JNTINITL GREATER +0                                       
00443         MOVE AL-UANON          TO JNTINITA.                       
00444                                                                   
00445      IF JNTLSTL  GREATER +0                                       
00446         MOVE AL-UANON          TO JNTLSTA.                        
00447                                                                   
00448      IF JNTAGEL  GREATER +0                                       
00449         IF JNTAGEI = '  '                                         
00450            MOVE AL-UANON          TO JNTAGEA                      
00451            MOVE +0             TO WS-JNTAGE                       
00452         ELSE                                                      
00453            IF JNTAGEI  NUMERIC                                    
00454               MOVE JNTAGEI        TO WS-JNTAGE                    
00455               MOVE AL-UNNON       TO JNTAGEA                      
00456            ELSE                                                   
00457               MOVE -1             TO JNTAGEL                      
00458               MOVE ER-2223        TO EMI-ERROR                    
00459               MOVE AL-UNBON       TO JNTAGEA                      
00460               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           
100213
100213     IF JNTLSTL NOT = ZERO  OR
100213        JNT1STL NOT = ZERO  OR
100213        JNTINITL NOT = ZERO
100213         IF JNTAGEL = ZEROS
100213             MOVE -1                 TO JNTAGEL
100213             MOVE ER-3269            TO EMI-ERROR
100213             MOVE AL-UNBON           TO JNTAGEA
100213             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
100213         END-IF
100213     END-IF.
00461                                                                   
           IF JNTDOBL > +0
              IF JNTDOBI NOT = ZEROS AND SPACES
                 MOVE JNTDOBI          TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT
                 MOVE DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-MDY
                 MOVE AL-UNNON         TO JNTDOBA
                 MOVE '4'              TO DC-OPTION-CODE
                 PERFORM 9700-DATE-LINK
                 IF DATE-CONVERSION-ERROR
                    MOVE -1            TO JNTDOBL
                    MOVE ER-2228       TO EMI-ERROR                     
                    MOVE AL-UNBON      TO JNTDOBA
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 END-IF
                                                                        
                 IF (NO-CONVERSION-ERROR)
                    AND (DC-BIN-DATE-1 > WS-CURRENT-BIN-DT)
                    MOVE DC-BIN-DATE-1 TO WS-WORK-BIN-DT
                    SUBTRACT CENTURY-ADJ FROM WS-WORK-BIN-RED
                    MOVE WS-WORK-BIN-DT
                                       TO DC-BIN-DATE-1
                                          WS-CONVERTED-BIRTH
                    MOVE AL-UANON      TO JNTDOBA
                 ELSE
                    MOVE DC-BIN-DATE-1 TO WS-CONVERTED-BIRTH
                    MOVE AL-UANON      TO JNTDOBA
                 END-IF
              ELSE
                 MOVE LOW-VALUES       TO WS-CONVERTED-BIRTH
              END-IF
           END-IF

00462      IF BENFARYL GREATER +0                                       
00463          MOVE AL-UANON           TO BENFARYA.                     
00464                                                                   
00465      IF POLFRML GREATER +0                                        
00466          MOVE AL-UANON           TO POLFRMA.                      
00467      IF RATEL   GREATER +0                                        
00468          MOVE AL-SANON           TO RATEA.                        
00469                                                                   
00470      IF PFRATEL GREATER +0                                        
00471          EXEC CICS BIF DEEDIT                                     
00472              FIELD   (PFRATEI)                                    
00473              LENGTH  (8)                                          
00474          END-EXEC                                                 
00475          IF PFRATEI  NUMERIC                                      
00476             MOVE PFRATEI        TO WS-PFRATE                      
00477             MOVE AL-UNNON       TO PFRATEA                        
00478           ELSE                                                    
00479             MOVE -1             TO PFRATEL                        
00480             MOVE ER-7822        TO EMI-ERROR                      
00481             MOVE AL-UNBON       TO PFRATEA                        
00482             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             
00483                                                                   
00484      IF RTCODEL GREATER +0                                        
00485          MOVE AL-UANON           TO RTCODEA.                      
00486                                                                   
00487      IF RESSTL  GREATER +0                                        
00488        IF RESSTI NOT = SPACES                                     
00489          MOVE RESSTI                 TO DRES-STATE                
00490          EXEC CICS LINK                                           
00491              PROGRAM    ('DLO022')                                
00492              COMMAREA   (WS-DLO-RESIDENT-STATE)                   
00493              LENGTH     (DRES-COMM-LENGTH)                        
00494          END-EXEC                                                 
00495          IF DRES-RETURN-CODE = 'OK'                               
00496              MOVE AL-UANON           TO RESSTA                    
00497            ELSE                                                   
00498              MOVE -1                 TO RESSTL                    
00499              MOVE AL-UABON           TO RESSTA                    
00500              MOVE ER-8204            TO EMI-ERROR                 
00501              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
00502                                                                   
CIDMOD     IF  CBNAMEL > +0                                             
CIDMOD         MOVE AL-UANON           TO CBNAMEA                       
CIDMOD     END-IF
CIDMOD                                                                  
CIDMOD     IF  CBADDRL > +0                                             
CIDMOD         MOVE AL-UANON           TO CBADDRA                       
CIDMOD     END-IF
CIDMOD                                                                  
CIDMOD     IF  CBCITYL > +0                                            
CIDMOD         MOVE AL-UANON           TO CBCITYA
CIDMOD     END-IF
CIDMOD                                                                  
      *    IF CBSTATEL > +0                                            
      *       MOVE SPACES              TO ELCNTL-KEY
      *       MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
      *       MOVE '3'                 TO ELCNTL-REC-TYPE
      *       MOVE CBSTATEI            TO ELCNTL-ACCESS
      *       MOVE +0                  TO ELCNTL-SEQ
      *       EXEC CICS READ
      *          DATASET   (FILE-ID-ELCNTL)
      *          SET       (ADDRESS OF CONTROL-FILE)
      *          RIDFLD    (ELCNTL-KEY)
      *          RESP      (WS-RESPONSE)
      *       END-EXEC
      *       IF RESP-NORMAL
      *          MOVE AL-UANON         TO CBSTATEA
      *       ELSE
      *          MOVE ER-2209          TO EMI-ERROR
      *          MOVE -1               TO CBSTATEL
      *          MOVE AL-UABON         TO CBSTATEA
      *          PERFORM 9900-ERROR-FORMAT
      *                                THRU 9900-EXIT
      *       END-IF
      *    END-IF

CIDMOD     IF  CBZIPL   > +0                                            
CIDMOD         MOVE AL-UANON           TO CBZIPA                        
CIDMOD     END-IF
CIDMOD                                                                  
00503      IF  ADDRS1L GREATER +0                                       
00504          MOVE AL-UANON           TO ADDRS1A.                      
00505                                                                   
00506      IF  ADDRS2L GREATER +0                                       
00507          MOVE AL-UANON           TO ADDRS2A.                      
00508                                                                   
00509      IF  ICITYL  GREATER +0                                       
00510          MOVE AL-UANON           TO ICITYA.

      *    IF ISTATEL > +0                                       
      *       MOVE SPACES              TO ELCNTL-KEY
      *       MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
      *       MOVE '3'                 TO ELCNTL-REC-TYPE
      *       MOVE ISTATEI             TO ELCNTL-ACCESS
      *       MOVE +0                  TO ELCNTL-SEQ
      *       EXEC CICS READ
      *          DATASET   (FILE-ID-ELCNTL)
      *          SET       (ADDRESS OF CONTROL-FILE)
      *          RIDFLD    (ELCNTL-KEY)
      *          RESP      (WS-RESPONSE)
      *       END-EXEC
      *       IF RESP-NORMAL
      *          MOVE AL-UANON         TO ISTATEA
      *       ELSE
      *          MOVE ER-2209          TO EMI-ERROR
      *          MOVE -1               TO ISTATEL
      *          MOVE AL-UABON         TO ISTATEA
      *          PERFORM 9900-ERROR-FORMAT
      *                                THRU 9900-EXIT
      *       END-IF
      *    END-IF

00512      IF  ZIPCDEL GREATER +0                                       
00513          MOVE AL-UANON           TO ZIPCDEA.                      
00514                                                                   
           IF JNTDOBL > +0
              MOVE AL-UANON            TO JNTDOBA
           END-IF

00515      IF  PHONEL  GREATER +0                                       
00516          MOVE PHONEI             TO DEEDIT-FIELD                  
00517          PERFORM 8600-DEEDIT                                      
00518          MOVE DEEDIT-FIELD-V0 TO WS-PHONE                         
00519          MOVE AL-UANON       TO PHONEA.                           
00520                                                                   
00521      IF  EMI-ERROR = ZEROS                                        
00522          GO TO 1300-CHANGE-ISSUE-ROUTINE                          
00523      ELSE                                                         
00524          GO TO 8200-SEND-DATAONLY.                                
00525                                                                   
00526      EJECT                                                        
00527                                                                   
00528  1300-CHANGE-ISSUE-ROUTINE.                                       
00529      MOVE 'C'                    TO PI-MAINT-FUNCTION.            
00530                                                                   
00531      IF  PI-MAIL-YES                                              
00532          NEXT SENTENCE                                            
00533       ELSE                                                        
00534          GO TO 1325-UPDATE-ISSUE-REC.                             
00535                                                                   
00536      IF LASTNML  GREATER +0 OR                                    
00537         FIRSTNML GREATER +0 OR                                    
00538         INITL    GREATER +0 OR                                    
00539         SOCSECL  GREATER +0 OR                                    
CIDMOD        CBNAMEL        > +0 OR
CIDMOD        CBADDRL        > +0 OR
CIDMOD        CBCITYL        > +0 OR
              CBSTATEL       > +0 OR
CIDMOD        CBZIPL         > +0 OR
00540         ADDRS1L  GREATER +0 OR                                    
00541         ADDRS2L  GREATER +0 OR                                    
00542         ICITYL   GREATER +0 OR                                    
00542         ISTATEL  GREATER +0 OR                                    
00543         ZIPCDEL  GREATER +0 OR                                    
00544         PHONEL   GREATER +0 OR                                    
00545         JNT1STL  GREATER +0 OR                                    
00546         JNTINITL GREATER +0 OR                                    
00547         JNTLSTL  GREATER +0 OR                                    
00548         BENFARYL GREATER +0 OR                                    
00549         POLFRML  GREATER +0 OR                                    
00550         PFRATEL  GREATER +0 OR                                    
00551         RTCODEL  GREATER +0 OR                                    
00552         RESSTL   GREATER +0 OR
              JNTDOBL > +0
00553            NEXT SENTENCE                                          
00554         ELSE                                                      
00555            GO TO 1325-UPDATE-ISSUE-REC.                           
00556                                                                   
00557      EXEC CICS HANDLE CONDITION                                   
00558           NOTFND    (1400-ADD-MAIL-RECORD)                        
00559      END-EXEC.                                                    
00560                                                                   
00744      EXEC CICS GETMAIN                                            
00745           SET       (ADDRESS OF PENDING-MAILING-DATA)             
00746           LENGTH    (ERPNDM-RECORD-LENGTH)                        
00747           INITIMG   (GETMAIN-SPACE)                               
00748      END-EXEC.                                                    
00749                                                                   

00561      EXEC CICS READ                                               
00562           DATASET   (FILE-ID-ERPNDM)                              
00563           RIDFLD    (ERPNDB-KEY)                                  
00564           INTO      (PENDING-MAILING-DATA)             
00565           UPDATE                                                  
00566      END-EXEC.                                                    
00567                                                                   
00568      MOVE PI-PROCESSOR-ID        TO PM-LAST-MAINT-BY.             
00569      MOVE EIBTIME                TO PM-LAST-MAINT-HHMMSS.         
00570      MOVE WS-CURRENT-BIN-DT      TO PM-LAST-MAINT-DT.             
00571                                                                   
00572      IF LASTNML  GREATER ZEROS                                    
00573         MOVE LASTNMI             TO PM-INSURED-LAST-NAME.         
00574                                                                   
00575      IF FIRSTNML GREATER ZEROS                                    
00576         MOVE FIRSTNMI            TO PM-INSURED-FIRST-NAME.        
00577                                                                   
00578      IF INITL    GREATER ZEROS                                    
00579         MOVE INITI               TO PM-INSURED-MIDDLE-INIT.       
00580                                                                   
00581      IF SOCSECL  GREATER ZEROS                                    
00582         MOVE SOCSECI             TO PM-INSURED-SOC-SEC-NO.        
00583                                                                   
CIDMOD     IF CBNAMEL > ZEROS
CIDMOD        MOVE CBNAMEI             TO  PM-CRED-BENE-NAME
CIDMOD     END-IF
CIDMOD
CIDMOD     IF CBADDRL > ZEROS
CIDMOD        MOVE CBADDRI             TO  PM-CRED-BENE-ADDR
CIDMOD     END-IF
CIDMOD
CIDMOD     IF CBCITYL > ZEROS
CIDMOD        MOVE CBCITYI             TO  PM-CRED-BENE-CITY
CIDMOD     END-IF

CIDMOD     IF CBSTATEL > ZEROS
CIDMOD        MOVE CBSTATEI            TO  PM-CRED-BENE-STATE
CIDMOD     END-IF

CIDMOD     IF CBZIPL   > ZEROS
CIDMOD        MOVE CBZIPI              TO  WS-ZIP-CODE
CIDMOD        IF WS-CANADIAN-ZIP                                        
CIDMOD           IF WS-ZIP-4 = SPACE  OR  '-'                           
CIDMOD              MOVE WS-ZIP-CAN-2-POST1   TO PM-CB-CAN-POST1        
CIDMOD              MOVE WS-ZIP-CAN-2-POST2   TO PM-CB-CAN-POST2        
CIDMOD           ELSE                                                   
CIDMOD              MOVE WS-ZIP-CAN-1-POST1   TO PM-CB-CAN-POST1        
CIDMOD              MOVE WS-ZIP-CAN-1-POST2   TO PM-CB-CAN-POST2        
CIDMOD           END-IF
CIDMOD        ELSE                                                      
CIDMOD           IF WS-ZIP-6 = SPACE  OR  '-'                           
CIDMOD              MOVE WS-ZIP-AM-2-CODE     TO PM-CB-ZIP-CODE         
CIDMOD              MOVE WS-ZIP-AM-2-PLUS4    TO PM-CB-ZIP-PLUS4        
CIDMOD           ELSE                                                   
CIDMOD              MOVE WS-ZIP-AM-1-CODE     TO PM-CB-ZIP-CODE         
CIDMOD              MOVE WS-ZIP-AM-1-PLUS4    TO PM-CB-ZIP-PLUS4        
CIDMOD           END-IF
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD                                                                  
00584      IF ADDRS1L GREATER ZEROS                                     
00585         MOVE ADDRS1I             TO  PM-ADDRESS-LINE-1.           
00586                                                                   
00587      IF ADDRS2L GREATER ZEROS                                     
00588         MOVE ADDRS2I             TO  PM-ADDRESS-LINE-2.           
00589                                                                   

100217     if (icityl > zeros)
100217        or (istatel > zeros)
100217        or (zipcdel > zeros)
100217        move spaces              to pm-city-st-zip-verified
100217     end-if

00590      IF ICITYL GREATER ZEROS                                     
00591         MOVE ICITYI              TO  PM-CITY.

00590      IF ISTATEL GREATER ZEROS
00591         MOVE ISTATEI             TO  PM-STATE.

           IF JNTDOBL > +0
              MOVE WS-CONVERTED-BIRTH  TO PM-JOINT-BIRTH-DT
           END-IF

00593      IF ZIPCDEL GREATER ZEROS                                     
00594          MOVE ZIPCDEI            TO  WS-ZIP-CODE                  
00595      ELSE                                                         
00596          GO TO 1310-CONTINUE.                                     
00597                                                                   
00598      IF WS-CANADIAN-ZIP                                           
00599          IF WS-ZIP-4 = SPACE  OR  '-'                             
00600              MOVE WS-ZIP-CAN-2-POST1   TO PM-CAN-POST1            
00601              MOVE WS-ZIP-CAN-2-POST2   TO PM-CAN-POST2            
00602          ELSE                                                     
00603              MOVE WS-ZIP-CAN-1-POST1   TO PM-CAN-POST1            
00604              MOVE WS-ZIP-CAN-1-POST2   TO PM-CAN-POST2            
00605      ELSE                                                         
00606          IF WS-ZIP-6 = SPACE  OR  '-'                             
00607              MOVE WS-ZIP-AM-2-CODE     TO PM-ZIP-CODE             
00608              MOVE WS-ZIP-AM-2-PLUS4    TO PM-ZIP-PLUS4            
00609          ELSE                                                     
00610              MOVE WS-ZIP-AM-1-CODE     TO PM-ZIP-CODE             
00611              MOVE WS-ZIP-AM-1-PLUS4    TO PM-ZIP-PLUS4.           
00612                                                                   
00613  1310-CONTINUE.                                                   
00614                                                                   
00615      IF PHONEL  GREATER ZEROS                                     
00616         MOVE WS-PHONE            TO  PM-PHONE-NO.                 
00617                                                                   
00618      EXEC CICS REWRITE                                            
00619           DATASET   (FILE-ID-ERPNDM)                              
00620           FROM      (PENDING-MAILING-DATA)                        
00621      END-EXEC.                                                    
00807      MOVE 'Y'                    TO WS-ADD-ADDRESS-SW.            
00622                                                                   
00623  1325-UPDATE-ISSUE-REC.                                           
00624                                                                   
00625      IF JNT1STL  GREATER +0 OR                                    
00626         JNTINITL GREATER +0 OR                                    
00627         JNTLSTL  GREATER +0 OR                                    
00628         JNTAGEL  GREATER +0 OR                                    
00629         BENFARYL GREATER +0 OR                                    
00630         POLFRML  GREATER +0 OR                                    
00631         PFRATEL  GREATER +0 OR                                    
00632         RTCODEL  GREATER +0 OR                                    
00633         RESSTL   GREATER +0 OR                                    
00634         LASTNML  GREATER +0 OR                                    
00635         FIRSTNML GREATER +0 OR                                    
00636         INITL    GREATER +0 OR                                    
00637         SOCSECL  GREATER +0 OR
              JNTDOBL        > +0 OR
00638         WS-ADDRESS-ADDED                                          
00639            NEXT SENTENCE                                          
00640         ELSE                                                      
00641            GO TO 7000-DISPLAY-ISSUES.                             
00642                                                                   
00643      EXEC CICS HANDLE CONDITION                                   
00644           NOTFND    (1385-REC-NOT-FOUND)                          
00645      END-EXEC.                                                    
00646                                                                   
00647      EXEC CICS READ                                               
00648           DATASET   (FILE-ID-ERPNDB)                              
00649           RIDFLD    (ERPNDB-KEY)                                  
00650           SET       (ADDRESS OF PENDING-BUSINESS)                 
00651           UPDATE                                                  
00652      END-EXEC.                                                    
00653                                                                   

041320     MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH
041320     MOVE FILE-ID-ERPNDB         TO JP-FILE-ID
041320     MOVE 'B'                    TO JP-RECORD-TYPE
041320     MOVE PENDING-BUSINESS       TO JP-RECORD-AREA

00655      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.             
00656      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         
00657      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT.             
00658                                                                   
00659      IF PI-MAIL-YES                                               
00660         MOVE '1'                 TO PB-I-MAIL-ADDRS-SW.           
00661                                                                   
00662      IF LASTNML  GREATER ZEROS                                    
00663         MOVE LASTNMI             TO PB-I-INSURED-LAST-NAME.       
00664                                                                   
00665      IF FIRSTNML GREATER ZEROS                                    
00666         MOVE FIRSTNMI            TO PB-I-INSURED-FIRST-NAME.      
00667                                                                   
00668      IF INITL    GREATER ZEROS                                    
00669         MOVE INITI               TO PB-I-INSURED-MIDDLE-INIT.     
00670                                                                   
00671      IF SOCSECL  GREATER ZEROS                                    
00672         MOVE SOCSECI             TO PB-I-SOC-SEC-NO.              
00673                                                                   
00674      IF JNT1STL  GREATER ZEROS                                    
00675         MOVE JNT1STI             TO PB-I-JOINT-FIRST-NAME.        
00676                                                                   
00677      IF JNTINITL GREATER ZEROS                                    
00678         MOVE JNTINITI            TO PB-I-JOINT-MIDDLE-INIT.       
00679                                                                   
00680      IF JNTLSTL  GREATER ZEROS                                    
00681         MOVE JNTLSTI             TO PB-I-JOINT-LAST-NAME.         
00682                                                                   
00683      IF JNTAGEL  GREATER ZEROS                                    
00684         MOVE WS-JNTAGE           TO PB-I-JOINT-AGE.               

           IF JNTDOBL > +0
              MOVE WS-CONVERTED-BIRTH  TO PB-I-JOINT-BIRTHDAY
           END-IF
121712
121712     MOVE 'N'                    TO WS-INS-AGE-SET
121712                                    WS-JNT-AGE-SET.
121712     IF JNTAGEL  GREATER THAN ZEROS
121712        MOVE 'Y'                TO WS-JNT-AGE-SET
121712        IF WS-JNTAGE = ZERO AND 
121712          PB-I-JOINT-INSURED > SPACES AND
121712          (PB-I-JOINT-BIRTHDAY = LOW-VALUES OR SPACES)
121712           MOVE 'Y'             TO WS-JNT-AGE-DEFAULTED
121712           MOVE 'AGE*'          TO JAGEDEFO
121712        ELSE
121712           MOVE 'N'             TO WS-JNT-AGE-DEFAULTED
121712           MOVE 'AGE '          TO JAGEDEFO
121712        END-IF
121712        PERFORM 1386-UPDATE-AGE-FLAGS THRU 1386-EXIT
121712     END-IF.

00686      IF BENFARYL GREATER ZEROS                                    
00687         MOVE BENFARYI            TO PB-I-BENEFICIARY-NAME.        
00688                                                                   
00689      IF POLFRML GREATER +0                                        
00690         MOVE POLFRMI             TO PB-I-POLICY-FORM-NO.          
00691                                                                   
00692      IF RTCODEL GREATER +0                                        
00693         MOVE RTCODEI             TO PB-I-RATE-CODE.               
00694                                                                   
00695      IF PFRATEL GREATER +0                                        
00696          IF PB-VALID-LIFE                                         
00697             MOVE WS-PFRATE      TO PB-I-AH-RATE                   
00698           ELSE                                                    
00699             MOVE WS-PFRATE      TO PB-I-LF-RATE.                  
00700                                                                   
00701      IF RESSTL  GREATER +0                                        
00702         MOVE RESSTI              TO PB-I-RESIDENT-STATE.          
00703                                                                   
00704      MOVE PB-COMMON-ERRORS       TO WS-SAVE-COMMON-ERRORS.        
00705                                                                   
00706      PERFORM 9800-LINK-PENDING-EDIT THRU 9800-EXIT.               
           PERFORM 7300-FORMAT-ERRORS  THRU 7399-EXIT

041320     PERFORM 8400-LOG-JOURNAL-RECORD  *> Before Issue Image

041320     MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH
041320     MOVE FILE-ID-ERPNDB         TO JP-FILE-ID
041320     MOVE 'C'                    TO JP-RECORD-TYPE
041320     MOVE PENDING-BUSINESS       TO JP-RECORD-AREA

00708      MOVE PB-SV-CARRIER          TO PI-SV-CARRIER.                
00709      MOVE PB-SV-GROUPING         TO PI-SV-GROUPING.               
00710      MOVE PB-SV-STATE            TO PI-SV-STATE.                  
00711                                                                   
00712      EXEC CICS REWRITE                                            
00713           DATASET   (FILE-ID-ERPNDB)                              
00714           FROM      (PENDING-BUSINESS)                            
00715      END-EXEC.                                                    

041320     PERFORM 8400-LOG-JOURNAL-RECORD  *> After Issue Image

072312******************************************************************
072312*       U P D A T E   T H E   O R I G   C E R T   I N F O        *
072312******************************************************************
072312
072312     display ' made it to update orig cert '
121712
121712     PERFORM 1495-READ-CERT-TRAILER THRU 1495-EXIT
072312
072312     MOVE PB-CONTROL-BY-ACCOUNT (1:33)
072312                                 TO ELCRTO-KEY
072312     MOVE 'I'                    TO ELCRTO-RECORD-TYPE
072312     MOVE +0                     TO ELCRTO-SEQ-NO
072312
072312     EXEC CICS READ
072312        DATASET   ('ELCRTO')
072312        INTO      (ORIGINAL-CERTIFICATE)
072312        RIDFLD    (ELCRTO-KEY)
072312        GTEQ
072312        RESP      (WS-RESPONSE)
072312     END-EXEC
072312     
072312     display ' just read gteq ' ws-response
072312     IF RESP-NORMAL
072312        AND (OC-CONTROL-PRIMARY (1:33) =
072312                 PB-CONTROL-BY-ACCOUNT (1:33))
072312        AND (OC-RECORD-TYPE = 'I')
072312        display ' resp norm, key =, type i '
072312        IF (OC-ENDORSEMENT-PROCESSED-DT = LOW-VALUES)
072312           display ' end proc blank '
072312           display ' csr edit session ' PI-CSR-SESSION-SW
072312           IF NOT CSR-EDIT-SESSION
072312              EXEC CICS READ
072312                 DATASET   ('ELCRTO')
072312                 INTO      (ORIGINAL-CERTIFICATE)
072312                 RIDFLD    (OC-CONTROL-PRIMARY)
072312                 UPDATE
072312                 RESP      (WS-RESPONSE)
072312              END-EXEC
072312              DISPLAY ' JUST DID READ UPD ' WS-RESPONSE
072312              IF RESP-NORMAL
072312                 display ' good read upd '
072312                 MOVE PB-I-INSURED-LAST-NAME TO 
072312                         OC-INS-LAST-NAME   
072312                 MOVE PB-I-INSURED-FIRST-NAME TO 
072312                         OC-INS-FIRST-NAME  
072312                 MOVE PB-I-INSURED-MIDDLE-INIT TO 
072312                         OC-INS-MIDDLE-INIT 
072312                 MOVE PB-I-AGE         TO OC-INS-AGE
072312                 MOVE PB-I-JOINT-LAST-NAME   TO 
072312                         OC-JNT-LAST-NAME
072312                 MOVE PB-I-JOINT-FIRST-NAME  TO 
072312                         OC-JNT-FIRST-NAME  
072312                 MOVE PB-I-JOINT-MIDDLE-INIT TO 
072312                         OC-JNT-MIDDLE-INIT 
072312                 MOVE PB-I-JOINT-AGE   TO OC-JNT-AGE
072312                 MOVE PB-I-LF-BENEFIT-CD TO OC-LF-BENCD
072312                 MOVE PB-I-LF-TERM     TO OC-LF-TERM
072312                 MOVE PB-I-LF-BENEFIT-AMT TO OC-LF-BEN-AMT
072312                 MOVE PB-I-LF-PREMIUM-AMT TO OC-LF-PRM-AMT
072312                 MOVE PB-I-LF-ALT-BENEFIT-AMT TO 
072312                         OC-LF-ALT-BEN-AMT
072312                 MOVE PB-I-LF-ALT-PREMIUM-AMT TO 
072312                         OC-LF-ALT-PRM-AMT  
072312                 MOVE PB-I-LF-EXPIRE-DT TO OC-LF-EXP-DT
072312                 MOVE PB-I-LIFE-COMMISSION TO OC-LF-COMM-PCT
072312                 MOVE LOW-VALUES       TO OC-LF-CANCEL-DT
072312                 MOVE +0               TO OC-LF-CANCEL-AMT
072312                                          OC-LF-ITD-CANCEL-AMT
072312                 MOVE PB-I-AH-BENEFIT-CD TO OC-AH-BENCD
072312                 MOVE PB-I-AH-TERM     TO OC-AH-TERM
072312                 MOVE PB-I-AH-BENEFIT-AMT TO OC-AH-BEN-AMT
072312                 MOVE PB-I-AH-PREMIUM-AMT TO OC-AH-PRM-AMT
072312                 MOVE PB-I-AH-EXPIRE-DT TO OC-AH-EXP-DT
072312                 MOVE PB-I-AH-COMMISSION TO OC-AH-COMM-PCT
072312                 MOVE PB-I-AH-CRIT-PER TO OC-AH-CP
072312                 MOVE LOW-VALUES       TO OC-AH-CANCEL-DT
072312                 MOVE +0               TO OC-AH-CANCEL-AMT
072312                                          OC-AH-ITD-CANCEL-AMT
072312                 MOVE PB-I-1ST-PMT-DT  TO OC-1ST-PMT-DT
011413                 MOVE 'Y'              TO OC-ISSUE-TRAN-IND
072312
072312                 MOVE PI-PROCESSOR-ID  TO OC-LAST-MAINT-BY
072312                 MOVE EIBTIME          TO OC-LAST-MAINT-HHMMSS
072312                 MOVE WS-CURRENT-BIN-DT TO OC-LAST-MAINT-DT
                       IF WS-ADDRESS-ADDED
072312                    MOVE PM-CRED-BENE-NAME
072312                                 TO OC-CRED-BENE-NAME
                       END-IF
121712                 IF NOT CERT-TRL-REC-NOT-FOUND
121712                    MOVE CS-INS-AGE-DEFAULT-FLAG TO 
121712                                  OC-INS-AGE-DEFAULT-FLAG
121712                    MOVE CS-JNT-AGE-DEFAULT-FLAG TO 
121712                                  OC-JNT-AGE-DEFAULT-FLAG
121712                 END-IF
072312                 EXEC CICS REWRITE
072312                    DATASET   ('ELCRTO')
072312                    FROM      (ORIGINAL-CERTIFICATE)
072312                    RESP      (WS-RESPONSE)
072312                 END-EXEC
072312                 display ' just rewrote ' ws-response
072312              END-IF
072312           END-IF
072312        ELSE
072312           SUBTRACT +1 FROM OC-KEY-SEQ-NO
072312           MOVE 'OC'             TO OC-RECORD-ID
072312           MOVE PI-PROCESSOR-ID  TO OC-LAST-MAINT-BY
072312           MOVE EIBTIME          TO OC-LAST-MAINT-HHMMSS
072312           MOVE WS-CURRENT-BIN-DT TO OC-LAST-MAINT-DT
072312           MOVE PB-I-INSURED-LAST-NAME TO OC-INS-LAST-NAME
072312           MOVE PB-I-INSURED-FIRST-NAME TO OC-INS-FIRST-NAME
072312           MOVE PB-I-INSURED-MIDDLE-INIT TO OC-INS-MIDDLE-INIT
072312           MOVE PB-I-AGE         TO OC-INS-AGE
072312           MOVE PB-I-JOINT-LAST-NAME TO OC-JNT-LAST-NAME
072312           MOVE PB-I-JOINT-FIRST-NAME TO OC-JNT-FIRST-NAME  
072312           MOVE PB-I-JOINT-MIDDLE-INIT TO OC-JNT-MIDDLE-INIT 
072312           MOVE PB-I-JOINT-AGE   TO OC-JNT-AGE
072312           MOVE PB-I-LF-BENEFIT-CD TO OC-LF-BENCD
072312           MOVE PB-I-LF-TERM     TO OC-LF-TERM
072312           MOVE PB-I-LF-BENEFIT-AMT TO OC-LF-BEN-AMT
072312           MOVE PB-I-LF-PREMIUM-AMT TO OC-LF-PRM-AMT
072312           MOVE PB-I-LF-ALT-BENEFIT-AMT TO OC-LF-ALT-BEN-AMT
072312           MOVE PB-I-LF-ALT-PREMIUM-AMT TO OC-LF-ALT-PRM-AMT
072312           MOVE PB-I-LF-EXPIRE-DT TO OC-LF-EXP-DT
072312           MOVE PB-I-LIFE-COMMISSION TO OC-LF-COMM-PCT
072312           MOVE LOW-VALUES       TO OC-LF-CANCEL-DT
072312           MOVE +0               TO OC-LF-CANCEL-AMT
072312                                    OC-LF-ITD-CANCEL-AMT
072312           MOVE PB-I-AH-BENEFIT-CD TO OC-AH-BENCD
072312           MOVE PB-I-AH-TERM     TO OC-AH-TERM
072312           MOVE PB-I-AH-BENEFIT-AMT TO OC-AH-BEN-AMT
072312           MOVE PB-I-AH-PREMIUM-AMT TO OC-AH-PRM-AMT
072312           MOVE PB-I-AH-EXPIRE-DT TO OC-AH-EXP-DT
072312           MOVE PB-I-AH-COMMISSION TO OC-AH-COMM-PCT
072312           MOVE PB-I-AH-CRIT-PER TO OC-AH-CP
072312           MOVE LOW-VALUES       TO OC-AH-CANCEL-DT
072312           MOVE +0               TO OC-AH-CANCEL-AMT
072312                                    OC-AH-ITD-CANCEL-AMT
072312           MOVE PB-I-1ST-PMT-DT  TO OC-1ST-PMT-DT
011413           MOVE 'Y'              TO OC-ISSUE-TRAN-IND
011413           MOVE 'N'              TO OC-CANCEL-TRAN-IND
072312           MOVE PM-CRED-BENE-NAME 
072312                                 TO OC-CRED-BENE-NAME
121712           IF NOT CERT-TRL-REC-NOT-FOUND
121712              MOVE CS-INS-AGE-DEFAULT-FLAG TO 
121712                            OC-INS-AGE-DEFAULT-FLAG
121712              MOVE CS-JNT-AGE-DEFAULT-FLAG TO 
121712                            OC-JNT-AGE-DEFAULT-FLAG
121712           END-IF
072312           MOVE LOW-VALUES       TO OC-ENDORSEMENT-PROCESSED-DT
072312           EXEC CICS WRITE
072312              DATASET   ('ELCRTO')
072312              FROM      (ORIGINAL-CERTIFICATE)
072312              RIDFLD    (OC-CONTROL-PRIMARY)
072312              RESP      (WS-RESPONSE)
072312           END-EXEC
072312        END-IF
072312     ELSE
072312        MOVE SPACES              TO ORIGINAL-CERTIFICATE
072312        MOVE 'OC'                TO OC-RECORD-ID
072312        MOVE PB-CONTROL-BY-ACCOUNT (1:33)
072312                                 TO OC-CONTROL-PRIMARY (1:33)
072312        MOVE 'I'                 TO OC-RECORD-TYPE
072312        MOVE +4096               TO OC-KEY-SEQ-NO
072312        MOVE PI-PROCESSOR-ID     TO OC-LAST-MAINT-BY
072312        MOVE EIBTIME             TO OC-LAST-MAINT-HHMMSS
072312        MOVE WS-CURRENT-BIN-DT   TO OC-LAST-MAINT-DT
072312        MOVE PB-I-INSURED-LAST-NAME TO OC-INS-LAST-NAME
072312        MOVE PB-I-INSURED-FIRST-NAME TO OC-INS-FIRST-NAME
072312        MOVE PB-I-INSURED-MIDDLE-INIT TO OC-INS-MIDDLE-INIT
072312        MOVE PB-I-AGE            TO OC-INS-AGE
072312        MOVE PB-I-JOINT-LAST-NAME TO OC-JNT-LAST-NAME
072312        MOVE PB-I-JOINT-FIRST-NAME TO OC-JNT-FIRST-NAME  
072312        MOVE PB-I-JOINT-MIDDLE-INIT TO OC-JNT-MIDDLE-INIT 
072312        MOVE PB-I-JOINT-AGE      TO OC-JNT-AGE
072312        MOVE PB-I-LF-BENEFIT-CD  TO OC-LF-BENCD
072312        MOVE PB-I-LF-TERM        TO OC-LF-TERM
072312        MOVE PB-I-LF-BENEFIT-AMT TO OC-LF-BEN-AMT
072312        MOVE PB-I-LF-PREMIUM-AMT TO OC-LF-PRM-AMT
072312        MOVE PB-I-LF-ALT-BENEFIT-AMT TO OC-LF-ALT-BEN-AMT
072312        MOVE PB-I-LF-ALT-PREMIUM-AMT TO OC-LF-ALT-PRM-AMT
072312        MOVE PB-I-LF-EXPIRE-DT   TO OC-LF-EXP-DT
072312        MOVE PB-I-LIFE-COMMISSION TO OC-LF-COMM-PCT
072312        MOVE LOW-VALUES          TO OC-LF-CANCEL-DT
072312        MOVE +0                  TO OC-LF-CANCEL-AMT
072312                                    OC-LF-ITD-CANCEL-AMT
072312        MOVE PB-I-AH-BENEFIT-CD  TO OC-AH-BENCD
072312        MOVE PB-I-AH-TERM        TO OC-AH-TERM
072312        MOVE PB-I-AH-BENEFIT-AMT TO OC-AH-BEN-AMT
072312        MOVE PB-I-AH-PREMIUM-AMT TO OC-AH-PRM-AMT
072312        MOVE PB-I-AH-EXPIRE-DT   TO OC-AH-EXP-DT
072312        MOVE PB-I-AH-COMMISSION  TO OC-AH-COMM-PCT
072312        MOVE PB-I-AH-CRIT-PER    TO OC-AH-CP
072312        MOVE LOW-VALUES          TO OC-AH-CANCEL-DT
072312        MOVE +0                  TO OC-AH-CANCEL-AMT
072312                                    OC-AH-ITD-CANCEL-AMT
072312        MOVE PB-I-1ST-PMT-DT     TO OC-1ST-PMT-DT
011413        MOVE 'Y'                 TO OC-ISSUE-TRAN-IND
011413        MOVE 'N'                 TO OC-CANCEL-TRAN-IND
072312        MOVE PM-CRED-BENE-NAME 
072312                                 TO OC-CRED-BENE-NAME
121712        IF NOT CERT-TRL-REC-NOT-FOUND
121712           MOVE CS-INS-AGE-DEFAULT-FLAG TO 
121712                         OC-INS-AGE-DEFAULT-FLAG
121712           MOVE CS-JNT-AGE-DEFAULT-FLAG TO 
121712                         OC-JNT-AGE-DEFAULT-FLAG
121712        END-IF
072312        MOVE LOW-VALUES          TO OC-ENDORSEMENT-PROCESSED-DT
072312
072312        EXEC CICS WRITE
072312           DATASET   ('ELCRTO')
072312           FROM      (ORIGINAL-CERTIFICATE)
072312           RIDFLD    (OC-CONTROL-PRIMARY)
072312           RESP      (WS-RESPONSE)
072312        END-EXEC
072312     END-IF
072312
00717      GO TO 7000-DISPLAY-ISSUES.                                   
00718                                                                   
00719  1385-REC-NOT-FOUND.                                              
00720      MOVE -1                     TO MAINTL.                       
00721      MOVE ER-2239                TO EMI-ERROR.                    
00722      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00723      GO TO 8200-SEND-DATAONLY.                                    
00724                                                                   
121712 1386-UPDATE-AGE-FLAGS.
121712
121712     EXEC CICS GETMAIN
121712         SET     (ADDRESS OF CERTIFICATE-TRAILERS)
121712         LENGTH  (ELCRTT-RECORD-LENGTH)
121712         INITIMG (GETMAIN-SPACE)
121712     END-EXEC
121712
121712     EXEC CICS HANDLE CONDITION
121712         NOTFND   (1386-NOTFND)
121712     END-EXEC.
121712
121712     MOVE PB-CONTROL-BY-ACCOUNT (1:33) TO ELCRTT-PRIMARY
121712     MOVE 'C'                TO ELCRTT-REC-TYPE
121712
121712     EXEC CICS READ
121712         UPDATE
121712         DATASET  (CRTT-ID)
121712         RIDFLD   (ELCRTT-KEY)
121712         INTO     (CERTIFICATE-TRAILERS)
121712     END-EXEC.
121712
121712     IF WS-INS-AGE-SET = 'Y'
121712        MOVE WS-INS-AGE-DEFAULTED TO CS-INS-AGE-DEFAULT-FLAG
121712     END-IF
121712     IF WS-JNT-AGE-SET = 'Y'
121712        MOVE WS-JNT-AGE-DEFAULTED TO CS-JNT-AGE-DEFAULT-FLAG
121712     END-IF
121712
121712     EXEC CICS REWRITE
121712        DATASET  (CRTT-ID)
121712        FROM     (CERTIFICATE-TRAILERS)
121712     END-EXEC.
121712
121712     GO TO 1386-EXIT.
121712
121712 1386-NOTFND.
121712
121712     MOVE SPACES       TO CERTIFICATE-TRAILERS.
121712     MOVE 'CS'         TO CS-RECORD-ID.
121712     MOVE PB-CONTROL-BY-ACCOUNT (1:33) TO ELCRTT-PRIMARY.
121712     MOVE 'C'          TO ELCRTT-REC-TYPE.
121712     MOVE ELCRTT-KEY TO CS-CONTROL-PRIMARY.
121712     MOVE WS-INS-AGE-DEFAULTED TO CS-INS-AGE-DEFAULT-FLAG.
121712     MOVE WS-JNT-AGE-DEFAULTED TO CS-JNT-AGE-DEFAULT-FLAG.
121712     EXEC CICS WRITE
121712        DATASET  (CRTT-ID)
121712        RIDFLD   (ELCRTT-KEY)
121712        FROM     (CERTIFICATE-TRAILERS)
121712     END-EXEC.
121712
121712 1386-EXIT.                                                       
121712     EXIT.                                                        
121712
00725  1400-ADD-MAIL-RECORD.                                            
00726                                                                   
00727      IF  (ADDRS1L NOT GREATER ZEROS)                              
00728              AND                                                  
00729          (ADDRS2L NOT GREATER ZEROS)                              
00730              AND                                                  
00731          (ICITYL NOT GREATER ZEROS)                              
CIDMOD             AND                                                  
00731          (ISTATEL NOT GREATER ZEROS)                              
CIDMOD             AND                                                  
CIDMOD         (CBNAMEL NOT > +0)
CIDMOD             AND
CIDMOD         (CBADDRL NOT > +0)
CIDMOD             AND
CIDMOD         (CBCITYL  NOT > +0)
CIDMOD             AND
CIDMOD         (CBSTATEL NOT > +0)
CIDMOD             AND
CIDMOD         (CBZIPL   NOT > +0)
                   AND
               (JNTDOBL NOT > +0)
00732          GO TO 1325-UPDATE-ISSUE-REC.                             
00733                                                                   
00734      EXEC CICS HANDLE CONDITION                                   
00735           NOTFND    (1485-REC-NOT-FOUND)                          
00736      END-EXEC.                                                    
00737                                                                   
00738      EXEC CICS READ                                               
00739           DATASET   (FILE-ID-ERPNDB)                              
00740           RIDFLD    (ERPNDB-KEY)                                  
00741           SET       (ADDRESS OF PENDING-BUSINESS)                 
00742      END-EXEC.                                                    
00743                                                                   
00744      EXEC CICS GETMAIN                                            
00745           SET       (ADDRESS OF PENDING-MAILING-DATA)             
00746           LENGTH    (ERPNDM-RECORD-LENGTH)                        
00747           INITIMG   (GETMAIN-SPACE)                               
00748      END-EXEC.                                                    
00749                                                                   
00750      MOVE 'PM'                       TO PM-RECORD-ID.             
00751      MOVE 'ER'                       TO PM-SOURCE-SYSTEM.         
00752                                                                   
00753      MOVE PI-PROCESSOR-ID            TO PM-LAST-MAINT-BY          
00754                                         PM-RECORD-ADDED-BY.       
00755                                                                   
00756      MOVE WS-CURRENT-BIN-DT          TO PM-LAST-MAINT-DT.         
00757      MOVE EIBTIME                    TO PM-LAST-MAINT-HHMMSS.     
00758                                                                   
00759      MOVE ERPNDB-KEY                 TO PM-CONTROL-PRIMARY        
00760      MOVE PB-I-INSURED-LAST-NAME     TO PM-INSURED-LAST-NAME.     
00761      MOVE PB-I-INSURED-FIRST-NAME    TO PM-INSURED-FIRST-NAME.    
00762      MOVE PB-I-INSURED-MIDDLE-INIT   TO PM-INSURED-MIDDLE-INIT.   
00763      MOVE PB-I-AGE                   TO PM-INSURED-ISSUE-AGE.     
00764      MOVE PB-I-BIRTHDAY              TO PM-INSURED-BIRTH-DT
           MOVE PB-I-JOINT-BIRTHDAY        TO PM-JOINT-BIRTH-DT
00765                                                                   
CIDMOD     IF CBNAMEL > ZEROS
CIDMOD        MOVE CBNAMEI             TO  PM-CRED-BENE-NAME
CIDMOD     END-IF
CIDMOD
CIDMOD     IF CBADDRL > ZEROS
CIDMOD        MOVE CBADDRI             TO  PM-CRED-BENE-ADDR
CIDMOD     END-IF
CIDMOD
CIDMOD     IF CBCITYL > ZEROS
CIDMOD        MOVE CBCITYI             TO  PM-CRED-BENE-CITY
CIDMOD     END-IF

CIDMOD     IF CBSTATEL > ZEROS
CIDMOD        MOVE CBSTATEI            TO  PM-CRED-BENE-STATE
CIDMOD     END-IF

CIDMOD     IF CBZIPL   > ZEROS
CIDMOD        MOVE CBZIPI              TO  WS-ZIP-CODE
CIDMOD        IF WS-CANADIAN-ZIP                                        
CIDMOD           IF WS-ZIP-4 = SPACE  OR  '-'                           
CIDMOD              MOVE WS-ZIP-CAN-2-POST1   TO PM-CB-CAN-POST1        
CIDMOD              MOVE WS-ZIP-CAN-2-POST2   TO PM-CB-CAN-POST2        
CIDMOD           ELSE                                                   
CIDMOD              MOVE WS-ZIP-CAN-1-POST1   TO PM-CB-CAN-POST1        
CIDMOD              MOVE WS-ZIP-CAN-1-POST2   TO PM-CB-CAN-POST2        
CIDMOD           END-IF
CIDMOD        ELSE                                                      
CIDMOD           IF WS-ZIP-6 = SPACE  OR  '-'                           
CIDMOD              MOVE WS-ZIP-AM-2-CODE     TO PM-CB-ZIP-CODE         
CIDMOD              MOVE WS-ZIP-AM-2-PLUS4    TO PM-CB-ZIP-PLUS4        
CIDMOD           ELSE                                                   
CIDMOD              MOVE WS-ZIP-AM-1-CODE     TO PM-CB-ZIP-CODE         
CIDMOD              MOVE WS-ZIP-AM-1-PLUS4    TO PM-CB-ZIP-PLUS4        
CIDMOD           END-IF
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD
00766      IF ADDRS1L GREATER ZEROS                                     
00767         MOVE ADDRS1I             TO  PM-ADDRESS-LINE-1.           
00768                                                                   
00769      IF ADDRS2L GREATER ZEROS                                     
00770         MOVE ADDRS2I             TO  PM-ADDRESS-LINE-2.           
00771                                                                   
00772      IF ICITYL  GREATER ZEROS                                     
00773         MOVE ICITYI              TO  PM-CITY.

00772      IF ISTATEL  GREATER ZEROS                                     
00773         MOVE ISTATEI             TO  PM-STATE.

           IF JNTDOBL > +0
              MOVE WS-CONVERTED-BIRTH  TO PM-JOINT-BIRTH-DT
           END-IF

00775      IF ZIPCDEL GREATER ZEROS                                     
00776          MOVE ZIPCDEI            TO  WS-ZIP-CODE                  
00777      ELSE                                                         
00778          GO TO 1410-CONTINUE.                                     
00779                                                                   
00780      IF WS-CANADIAN-ZIP                                           
00781          IF WS-ZIP-4 = SPACE  OR  '-'                             
00782              MOVE WS-ZIP-CAN-2-POST1   TO PM-CAN-POST1            
00783              MOVE WS-ZIP-CAN-2-POST2   TO PM-CAN-POST2            
00784          ELSE                                                     
00785              MOVE WS-ZIP-CAN-1-POST1   TO PM-CAN-POST1            
00786              MOVE WS-ZIP-CAN-1-POST2   TO PM-CAN-POST2            
00787      ELSE                                                         
00788          IF WS-ZIP-6 = SPACE  OR  '-'                             
00789              MOVE WS-ZIP-AM-2-CODE     TO PM-ZIP-CODE             
00790              MOVE WS-ZIP-AM-2-PLUS4    TO PM-ZIP-PLUS4            
00791          ELSE                                                     
00792              MOVE WS-ZIP-AM-1-CODE     TO PM-ZIP-CODE             
00793              MOVE WS-ZIP-AM-1-PLUS4    TO PM-ZIP-PLUS4.           
00794                                                                   
00795  1410-CONTINUE.                                                   
00796                                                                   
00797      IF PHONEL  GREATER ZEROS                                     
00798         MOVE WS-PHONE            TO  PM-PHONE-NO.                 
00799                                                                   
00800                                                                   
00801      EXEC CICS WRITE                                              
00802           DATASET   (FILE-ID-ERPNDM)                              
00803           FROM      (PENDING-MAILING-DATA)                        
00804           RIDFLD    (PM-CONTROL-PRIMARY)                          
00805      END-EXEC.                                                    
00806                                                                   
00807      MOVE 'Y'                    TO WS-ADD-ADDRESS-SW.            
00808                                                                   
00809      GO TO 1325-UPDATE-ISSUE-REC.                                 
00810                                                                   
00811  1485-REC-NOT-FOUND.                                              
00812      MOVE -1                     TO MAINTL.                       
00813      MOVE ER-2239                TO EMI-ERROR.                    
00814      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
00815      GO TO 8200-SEND-DATAONLY.                                    
00816                                                                   
121712
121712 1495-READ-CERT-TRAILER.
121712
121712     EXEC CICS GETMAIN
121712         SET     (ADDRESS OF CERTIFICATE-TRAILERS)
121712         LENGTH  (ELCRTT-RECORD-LENGTH)
121712         INITIMG (GETMAIN-SPACE)
121712     END-EXEC
121712
121712     MOVE +0                     TO  WS-CERT-TRL-REC-NOT-FOUND.
121712     MOVE PB-CONTROL-BY-ACCOUNT (1:33) TO  ELCRTT-PRIMARY.
121712     MOVE 'C'                    TO  ELCRTT-REC-TYPE.
121712
121712     EXEC CICS HANDLE CONDITION
121712         NOTFND (1495-CERT-TRL-REC-NOTFND)
121712     END-EXEC.
121712
121712     EXEC CICS READ
121712         DATASET  (CRTT-ID)
121712         RIDFLD   (ELCRTT-KEY)
121712         INTO     (CERTIFICATE-TRAILERS)
121712     END-EXEC.
121712
121712     GO TO 1495-EXIT.
121712
121712 1495-CERT-TRL-REC-NOTFND.
121712     MOVE +1                     TO WS-CERT-TRL-REC-NOT-FOUND.
121712
121712 1495-EXIT.
121712     EXIT.
121712
00817  EJECT                                                            
00818  7000-DISPLAY-ISSUES.                                             
00819      MOVE EL631F                 TO PI-MAP-NAME.                  
00820      MOVE LOW-VALUES             TO EL631FI.                      
121713*     MOVE -1                     TO LASTNML.
121713     MOVE -1                     TO CBNAMEL.
00822                                                                   
00823      EXEC CICS HANDLE CONDITION                                   
00824           NOTFND (7090-REC-NOT-FOUND)                             
00825      END-EXEC.                                                    
00826                                                                   
00827      EXEC CICS READ                                               
00828           DATASET  (FILE-ID-ERPNDB)                               
00829           RIDFLD   (ERPNDB-KEY)                                   
00830           SET      (ADDRESS OF PENDING-BUSINESS)                  
00831      END-EXEC.                                                    
00832                                                                   
00833      MOVE PB-CERT-PRIME              TO CERTO.                    
00834      MOVE PB-CERT-SFX                TO SUFIXO.                   
00835      MOVE PB-CERT-EFF-DT             TO DC-BIN-DATE-1.            
00836      MOVE SPACE                      TO DC-OPTION-CODE.           
00837      PERFORM 9700-DATE-LINK.                                      
00838                                                                   
00839      MOVE DC-GREG-DATE-1-EDIT        TO EFFDTO.                   
00840                                                                   
00841      MOVE PB-LAST-MAINT-BY           TO MAINTBYO.                 
00842      MOVE PB-LAST-MAINT-HHMMSS       TO WS-TIME.                  
00843      MOVE WS-HR-MINS                 TO MAINTATO.                 
00844      MOVE PB-INPUT-DT                TO DC-BIN-DATE-1.            
00845      MOVE SPACE                      TO DC-OPTION-CODE.           
00846      PERFORM 9700-DATE-LINK.                                      
00847                                                                   
00848      MOVE DC-GREG-DATE-1-EDIT        TO ENTERDTO.                 
00849      MOVE PB-INPUT-BY                TO ENTERBYO.                 
00850                                                                   
00851      MOVE PB-BATCH-SEQ-NO            TO SEQO.                     
00852                                                                   
00853      IF PB-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      
00854         MOVE PB-CREDIT-ACCEPT-DT     TO DC-BIN-DATE-1             
00855         MOVE SPACE                   TO DC-OPTION-CODE            
00856         PERFORM 9700-DATE-LINK                                    
00857         MOVE DC-GREG-DATE-1-EDIT     TO MAINTDTO                  
00858         MOVE 'INACTIVE'              TO MOENDDTO                  
00859      ELSE                                                         
00860         MOVE PB-LAST-MAINT-DT        TO DC-BIN-DATE-1             
00861         MOVE SPACE                   TO DC-OPTION-CODE            
00862         PERFORM 9700-DATE-LINK                                    
00863         MOVE DC-GREG-DATE-1-EDIT     TO MAINTDTO                  
00864                                                                   
00865      MOVE PB-CREDIT-SELECT-DT        TO DC-BIN-DATE-1.            
00866      MOVE SPACE                      TO DC-OPTION-CODE.           
00867      PERFORM 9700-DATE-LINK.                                      
00868      MOVE DC-GREG-DATE-1-EDIT        TO MOENDDTO.                 
00869                                                                   
00870      MOVE PB-I-INSURED-LAST-NAME     TO LASTNMO.                  
00871      MOVE PB-I-INSURED-FIRST-NAME    TO FIRSTNMO.                 
00872      MOVE PB-I-INSURED-MIDDLE-INIT   TO INITO.                    
00873      MOVE PB-I-SOC-SEC-NO            TO SOCSECO.                  
00874                                                                   
00875      IF  PB-I-JOINT-AGE GREATER ZEROS                             
00876          MOVE PB-I-JOINT-AGE         TO JNTAGEO.                  
121712
121712     PERFORM 1495-READ-CERT-TRAILER THRU 1495-EXIT
121712     IF NOT CERT-TRL-REC-NOT-FOUND
121712         IF CS-JNT-AGE-DEFAULT-FLAG = 'Y'
121712             MOVE 'AGE*'             TO JAGEDEFO
121712         ELSE
121712             MOVE 'AGE '             TO JAGEDEFO
121712         END-IF
121712     ELSE
121712         MOVE 'AGE '             TO JAGEDEFO
121712     END-IF

           IF PB-I-JOINT-BIRTHDAY NOT = SPACES AND LOW-VALUES
              MOVE PB-I-JOINT-BIRTHDAY TO DC-BIN-DATE-1
              MOVE SPACES              TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              MOVE DC-GREG-DATE-1-EDIT     TO JNTDOBO
           END-IF

00878      IF  PB-I-JOINT-FIRST-NAME  GREATER SPACES                    
00879          MOVE PB-I-JOINT-FIRST-NAME  TO JNT1STO.                  
00880                                                                   
00881      IF  PB-I-JOINT-MIDDLE-INIT GREATER SPACES                    
00882          MOVE PB-I-JOINT-MIDDLE-INIT TO JNTINITO.                 
00883                                                                   
00884      IF  PB-I-JOINT-LAST-NAME   GREATER SPACES                    
00885          MOVE PB-I-JOINT-LAST-NAME   TO JNTLSTO.                  
00886                                                                   
00887      IF  PB-I-BENEFICIARY-NAME  GREATER SPACES                    
00888          MOVE PB-I-BENEFICIARY-NAME  TO BENFARYO.                 
00889                                                                   
00890      IF PB-I-POLICY-FORM-NO GREATER SPACES                        
00891          MOVE PB-I-POLICY-FORM-NO    TO POLFRMO.                  
00892                                                                   
00893 *    IF PB-I-STATE-TAX NUMERIC                                    
00894 *        MOVE PB-I-STATE-TAX         TO STTAXO.                   
00895                                                                   
00896 *    IF PB-I-MUNI-TAX NUMERIC                                     
00897 *        MOVE PB-I-MUNI-TAX          TO MUNITXO.                  
00898                                                                   
00899      IF PB-I-RESIDENT-STATE GREATER SPACES                        
00900          MOVE PB-I-RESIDENT-STATE    TO RESSTO.                   
00901                                                                   
00902      IF PB-I-RATE-CODE GREATER SPACES                             
00903          MOVE PB-I-RATE-CODE         TO RTCODEO.                  
00904                                                                   
00905      IF PB-VALID-LIFE                                             
00906          MOVE PB-I-LF-RATE           TO WS-RATE-OUT               
00907          MOVE WS-XRATE-OUT           TO RATEO                     
00908        ELSE                                                       
00909          MOVE PB-I-AH-RATE           TO WS-RATE-OUT               
00910          MOVE WS-XRATE-OUT           TO RATEO.                    
00911                                                                   
00912      IF PB-VALID-LIFE                                             
00913          MOVE PB-I-AH-RATE           TO WS-RATE-OUT               
00914          MOVE WS-XRATE-OUT           TO PFRATEO                   
00915        ELSE                                                       
00916          MOVE PB-I-LF-RATE           TO WS-RATE-OUT               
00917          MOVE WS-XRATE-OUT           TO PFRATEO.                  
00918                                                                   
00919      IF PB-I-REIN-TABLE GREATER SPACES                            
00920         MOVE PB-I-REIN-TABLE         TO REINTBLO.                 
00921                                                                   
00922      IF PB-I-RATE-DEVIATION-LF GREATER THAN SPACES                
00923         MOVE PB-I-RATE-DEVIATION-LF  TO LFRTDEVO.                 
00924                                                                   
00925      IF PB-I-RATE-DEV-PCT-LF GREATER ZEROS                        
00926         MOVE PB-I-RATE-DEV-PCT-LF    TO LFRTPRCO.                 
00927                                                                   
00928      IF PB-I-RATE-DEVIATION-AH GREATER THAN SPACES                
00929         MOVE PB-I-RATE-DEVIATION-AH  TO AHRTDEVO.                 
00930                                                                   
00931      IF PB-I-RATE-DEV-PCT-AH GREATER ZEROS                        
00932         MOVE PB-I-RATE-DEV-PCT-AH    TO AHRTPRCO.                 
00933                                                                   
00934      IF PB-I-LIFE-COMMISSION GREATER ZEROS                        
00935         MOVE PB-I-LIFE-COMMISSION    TO LFCOMMSO.                 
00936                                                                   
00937      IF PB-I-AH-COMMISSION GREATER ZEROS                          
00938         MOVE PB-I-AH-COMMISSION      TO AHCOMMSO.                 
00939                                                                   
00940      IF PB-ACCT-EFF-DT NOT = LOW-VALUES                           
00941         MOVE PB-ACCT-EFF-DT          TO DC-BIN-DATE-1             
00942         MOVE SPACE                   TO DC-OPTION-CODE            
00943         PERFORM 9700-DATE-LINK                                    
00944         IF NO-CONVERSION-ERROR                                    
00945            MOVE DC-GREG-DATE-1-EDIT  TO AEFFDTO.                  
00946                                                                   
00947      IF PB-ACCT-EXP-DT = HIGH-VALUES                              
00948         MOVE 999999                  TO WS-EXP-DT-EDIT            
00949         INSPECT WS-EXP-DT-EDIT CONVERTING SPACES TO '/'           
00950         MOVE WS-EXP-DT-EDIT          TO AEXPDTO                   
00951      ELSE                                                         
00952         IF PB-ACCT-EXP-DT NOT = LOW-VALUES                        
00953            MOVE PB-ACCT-EXP-DT          TO DC-BIN-DATE-1          
00954            MOVE SPACE                   TO DC-OPTION-CODE         
00955            PERFORM 9700-DATE-LINK                                 
00956            IF NO-CONVERSION-ERROR                                 
00957               MOVE DC-GREG-DATE-1-EDIT  TO AEXPDTO.               
00958                                                                   
00959      IF PI-MAIL-YES                                               
00960         NEXT SENTENCE                                             
00961      ELSE                                                         
00962         MOVE AL-SANOF                TO ADDRS1A                   
00963                                         ADDRS2A                   
00964                                         ICITYA
                                              ISTATEA
00965                                         ZIPCDEA                   
00966                                         PHONEA                    
CIDMOD                                        CBNAMEA
CIDMOD                                        CBADDRA
CIDMOD                                        CBCITYA
                                              CBSTATEA
CIDMOD                                        CBZIPA
00967         GO TO 8100-SEND-INITIAL-MAP.                              
00968                                                                   
00969      EXEC CICS HANDLE CONDITION                                   
00970           NOTFND (8100-SEND-INITIAL-MAP)                          
00971      END-EXEC.                                                    
00972                                                                   
00973      EXEC CICS READ                                               
00974           DATASET  (FILE-ID-ERPNDM)                               
00975           RIDFLD   (ERPNDB-KEY)                                   
00976           SET      (ADDRESS OF PENDING-MAILING-DATA)              
00977      END-EXEC.                                                    
00978                                                                   
CIDMOD     IF PM-CRED-BENE-NAME > SPACES
CIDMOD        MOVE PM-CRED-BENE-NAME       TO CBNAMEO
CIDMOD     END-IF
CIDMOD
CIDMOD     IF PM-CRED-BENE-ADDR > SPACES
CIDMOD        MOVE PM-CRED-BENE-ADDR       TO CBADDRO
CIDMOD     END-IF
CIDMOD
CIDMOD     IF PM-CRED-BENE-CITY > SPACES
CIDMOD        MOVE PM-CRED-BENE-CITY       TO CBCITYO
CIDMOD     END-IF

CIDMOD     IF PM-CRED-BENE-STATE > SPACES
CIDMOD        MOVE PM-CRED-BENE-STATE      TO CBSTATEO
CIDMOD     END-IF

CIDMOD     IF PM-CRED-BENE-ZIP > SPACES                                 
CIDMOD        MOVE SPACES              TO WS-ZIP-CODE                   
CIDMOD        IF PM-CB-CANADIAN-POST-CODE                               
CIDMOD           MOVE PM-CB-CAN-POST1  TO WS-ZIP-CAN-2-POST1            
CIDMOD           MOVE PM-CB-CAN-POST2  TO WS-ZIP-CAN-2-POST2            
CIDMOD           MOVE WS-ZIP-CODE      TO CBZIPO                        
CIDMOD        ELSE                                                      
CIDMOD           MOVE PM-CB-ZIP-CODE   TO WS-ZIP-AM-2-CODE              
CIDMOD           MOVE WS-ZIP-CODE      TO CBZIPO                        
CIDMOD           IF PM-CB-ZIP-PLUS4 NOT = SPACES  AND  ZEROS            
CIDMOD              MOVE '-'           TO WS-ZIP-AM-2-DASH              
CIDMOD              MOVE PM-CB-ZIP-PLUS4                                
CIDMOD                                 TO WS-ZIP-AM-2-PLUS4             
CIDMOD              MOVE WS-ZIP-CODE   TO CBZIPO                        
CIDMOD           END-IF
CIDMOD        END-IF
CIDMOD     END-IF
01001                                                                   
00979      IF PM-ADDRESS-LINE-1 GREATER SPACES                          
00980         MOVE PM-ADDRESS-LINE-1       TO ADDRS1O.                  
00981                                                                   
00982      IF PM-ADDRESS-LINE-2 GREATER SPACES                          
00983         MOVE PM-ADDRESS-LINE-2       TO ADDRS2O.                  
00984                                                                   
00985      IF PM-CITY           GREATER SPACES                          
00986         MOVE PM-CITY                 TO ICITYO.

00985      IF PM-STATE          GREATER SPACES                          
00986         MOVE PM-STATE                TO ISTATEO.

           IF PM-JOINT-BIRTH-DT NOT = LOW-VALUES
              MOVE PM-JOINT-BIRTH-DT   TO DC-BIN-DATE-1
              MOVE SPACE               TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO JNTDOBO
              END-IF
           END-IF

00988      IF PM-ZIP            GREATER SPACES                          
00989          MOVE SPACES               TO WS-ZIP-CODE                 
00990          IF PM-CANADIAN-POST-CODE                                 
00991              MOVE PM-CAN-POST1     TO WS-ZIP-CAN-2-POST1          
00992              MOVE PM-CAN-POST2     TO WS-ZIP-CAN-2-POST2          
00993              MOVE WS-ZIP-CODE      TO ZIPCDEO                     
00994          ELSE                                                     
00995              MOVE PM-ZIP-CODE      TO WS-ZIP-AM-2-CODE            
00996              MOVE WS-ZIP-CODE      TO ZIPCDEO                     
00997              IF PM-ZIP-PLUS4 NOT = SPACES  AND  ZEROS             
00998                  MOVE '-'          TO WS-ZIP-AM-2-DASH            
00999                  MOVE PM-ZIP-PLUS4 TO WS-ZIP-AM-2-PLUS4           
01000                  MOVE WS-ZIP-CODE  TO ZIPCDEO.                    
01001                                                                   
01002      IF PM-PHONE-NO NUMERIC                                       
01003         IF PM-PHONE-NO GREATER ZEROS                              
01004            MOVE PM-PHONE-NO          TO PHONEO                    
01005            INSPECT PHONEO CONVERTING ' ' TO '-'.                  
01006                                                                   
01007      GO TO 8100-SEND-INITIAL-MAP.                                 
01008                                                                   
01009  7090-REC-NOT-FOUND.                                              
01010      MOVE -1                     TO MAINTL.                       
01011      MOVE ER-2239                TO EMI-ERROR.                    
01012      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01013      GO TO 8100-SEND-INITIAL-MAP-CONT.                            
01014                                                                   
01015      EJECT                                                        
01016                                                                   
01017      EJECT                                                        
01018  7300-FORMAT-ERRORS.                                              
01019                                                                   
01020      IF PB-COMMON-ERRORS = LOW-VALUES                             
01021         IF PB-FATAL-ERRORS                                        
01022            MOVE ER-2695          TO EMI-ERROR                     
01023            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
01024            GO TO 7399-EXIT.                                       
01025                                                                   
01026      MOVE +0                     TO WS-SUB4.                      
01027                                                                   
01028  7310-ERROR-LOOP.                                                 
01029                                                                   
01030      ADD +1                      TO WS-SUB4.                      
01031                                                                   
01032      IF WS-SUB4 GREATER THAN PB-NO-OF-ERRORS                      
01033         GO TO 7350-SET-ERROR-FLAGS.                               
01034                                                                   
01035      MOVE PB-COMMON-ERROR (WS-SUB4) TO EMI-ERROR.                 
01036      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
01037                                                                   
01038      GO TO 7310-ERROR-LOOP.                                       
01039                                                                   
01040  7350-SET-ERROR-FLAGS.                                            
01041                                                                   
01042      IF EMI-FATAL-CTR NOT = ZEROS                                 
01043         MOVE 'Y'                 TO WS-ERRORS-PRESENT-SW          
01044         MOVE 'X'                 TO PB-FATAL-FLAG                 
01045         GO TO 7355-SET-ERROR-FLAGS.                               
01046                                                                   
01047      IF EMI-FORCABLE-CTR NOT = ZEROS                              
01048         MOVE 'Y'                 TO WS-ERRORS-PRESENT-SW          
01049         IF PB-ISSUE                                               
01050            IF PB-ISSUE-FORCE                                      
01051               MOVE 'F'           TO PB-FORCE-ER-CD                
01052               MOVE ER-2600       TO EMI-ERROR                     
01053               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            
01054              ELSE                                                 
01055               MOVE 'X'           TO PB-FORCE-ER-CD                
01056           ELSE                                                    
01057            IF PB-CANCEL-FORCE                                     
01058               MOVE 'F'           TO PB-FORCE-ER-CD                
01059               MOVE ER-2600       TO EMI-ERROR                     
01060               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            
01061              ELSE                                                 
01062               MOVE 'X'           TO PB-FORCE-ER-CD.               
01063                                                                   
01064  7355-SET-ERROR-FLAGS.                                            
01065                                                                   
01066      IF EMI-WARNING-CTR NOT = ZEROS                               
01067         MOVE 'Y'                 TO WS-ERRORS-PRESENT-SW          
01068         MOVE 'W'                 TO PB-WARN-ER-CD.                
01069                                                                   
01070      IF PB-UNFORCED-ERRORS OR                                     
01071         PB-FATAL-ERRORS    OR                                     
01072         PB-RECORD-ON-HOLD  OR                                     
01073         PB-RECORD-RETURNED OR                                     
01074         PB-CANCELLATION                                           
01075           NEXT SENTENCE                                           
01076         ELSE                                                      
01077           GO TO 7399-EXIT.                                        
01078                                                                   
01079      EXEC CICS  HANDLE CONDITION                                  
01080             NOTFND   (7399-EXIT)                                  
01081      END-EXEC.                                                    
01082                                                                   
01083      MOVE PB-CONTROL-BY-ACCOUNT  TO ELCERT-KEY.                   
01084      MOVE PB-SV-CARRIER          TO ELCERT-CARRIER.               
01085      MOVE PB-SV-GROUPING         TO ELCERT-GROUPING.              
01086      MOVE PB-SV-STATE            TO ELCERT-STATE.                 
01087                                                                   
01088      EXEC CICS READ                                               
01089          SET     (ADDRESS OF CERTIFICATE-MASTER)                  
01090          DATASET (FILE-ID-ELCERT)                                 
01091          RIDFLD  (ELCERT-KEY)                                     
01092          UPDATE                                                   
01093      END-EXEC.                                                    
01094                                                                   
01095      MOVE CM-CREDIT-INTERFACE-SW-1         TO WS-SW-1             
01096      MOVE CM-CREDIT-INTERFACE-SW-2         TO WS-SW-2             
01097                                                                   
01098      IF (CERT-ADDED-BATCH OR CERT-PURGED-OFFLINE) AND PB-ISSUE    
01099         GO TO 7360-REWRITE-CERT-MASTER.                           
01100                                                                   
01101      IF PB-ISSUE                                                  
01102         IF PB-RECORD-RETURNED                                     
01103             MOVE '4'  TO CM-CREDIT-INTERFACE-SW-1                 
01104             GO TO 7360-REWRITE-CERT-MASTER                        
01105         ELSE                                                      
01106             MOVE '2' TO CM-CREDIT-INTERFACE-SW-1                  
01107             GO TO 7360-REWRITE-CERT-MASTER.                       
01108                                                                   
01109      IF PB-RECORD-RETURNED                                        
01110          MOVE '7'      TO CM-CREDIT-INTERFACE-SW-2                
01111          GO TO 7360-REWRITE-CERT-MASTER.                          
01112                                                                   
01113      IF PB-C-LF-CANCEL-VOIDED                                     
01114         IF (PB-UNFORCED-ERRORS OR                                 
01115             PB-FATAL-ERRORS    OR                                 
01116             PB-RECORD-ON-HOLD)                                    
01117             MOVE '6'             TO CM-CREDIT-INTERFACE-SW-2      
01118             GO TO 7360-REWRITE-CERT-MASTER                        
01119         ELSE                                                      
01120             MOVE '5'             TO CM-CREDIT-INTERFACE-SW-2      
01121             GO TO 7360-REWRITE-CERT-MASTER.                       
01122                                                                   
01123      IF (PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS                    
01124                             OR PB-RECORD-ON-HOLD)                 
01125         MOVE '4'                 TO CM-CREDIT-INTERFACE-SW-2      
01126      ELSE                                                         
01127         MOVE '1'                 TO CM-CREDIT-INTERFACE-SW-2.     
01128                                                                   
01129  7360-REWRITE-CERT-MASTER.                                        
01130      IF WS-SW-1 = CM-CREDIT-INTERFACE-SW-1 AND                    
01131         WS-SW-2 = CM-CREDIT-INTERFACE-SW-2                        
01132         EXEC CICS UNLOCK                                          
01133              DATASET    (FILE-ID-ELCERT)                          
01134         END-EXEC                                                  
01135         GO TO 7399-EXIT.                                          
01136                                                                   
01137      EXEC CICS REWRITE                                            
01138           DATASET    (FILE-ID-ELCERT)                             
01139           FROM       (CERTIFICATE-MASTER)                         
01140      END-EXEC.                                                    
01141                                                                   
01142  7399-EXIT.                                                       
01143       EXIT.                                                       
01144                                                                   
01145      EJECT                                                        
01146  8100-SEND-INITIAL-MAP.                                           
01147      PERFORM 7300-FORMAT-ERRORS  THRU 7399-EXIT.                  
01148                                                                   
01149  8100-SEND-INITIAL-MAP-CONT.                                      
01150      MOVE EIBTIME                TO TIME-IN.                      
01151                                                                   
01152      MOVE WS-LFRTHDG             TO LFRTHDGO.                     
01153      MOVE WS-AHRTHDG             TO AHRTHDGO.                     
01154      MOVE WS-LFDVHDG             TO LFDVHDGO.                     
01155      MOVE WS-AHDVHDG             TO AHDVHDGO.                     
01156      MOVE WS-LFCMHDG             TO LFCMHDGO.                     
01157      MOVE WS-AHCMHDG             TO AHCMHDGO.                     
01158      MOVE WS-CURRENT-DT          TO DATEO.                        
01159      MOVE TIME-OUT               TO TIMEO.                        
01160      MOVE EMI-MESSAGE-AREA (1)   TO ERMSG1O.                      
01161      MOVE EMI-MESSAGE-AREA (2)   TO ERMSG2O.                      
01162      MOVE PB-ENTRY-BATCH         TO BATCHO.                       
01163      MOVE PB-CARRIER             TO CARRO.                        
01164      MOVE PB-GROUPING            TO GROUPO.                       
01165      MOVE PB-ACCOUNT             TO ACCTO.                        
01166      MOVE PB-STATE               TO STATEO.                       
01167                                                                   
           IF CSR-EDIT-SESSION
              MOVE '- CUSTOMER SERVICE REVIEW/CORRECTION -'
                                       TO HEADO
           END-IF
100217     move pi-company-id        to compido
100217     move function upper-case(ws-kix-myenv)
100217                               to sysenvo
100217     move pi-processor-id      to procido
01168 *    IF PI-COMPANY-ID = 'DMD'                                     
01169 *       MOVE AL-SANOF          TO LIT1A LIT2A STTAXA MUNITXA      
01170 *       MOVE AL-SANON          TO RATEA                           
01171 *       MOVE AL-UANOF          TO PFRATEA RTCODEA RESSTA.         
121713
121713***DO NOT ALLOW PRIMARY NAME TO CHANGE ON THIS SCREEN
121713     MOVE AL-SANOF             TO LASTNMA FIRSTNMA INITA.
01172                                                                   
01173      EXEC CICS SEND                                               
01174          MAP      (PI-MAP-NAME)                                   
01175          MAPSET   (MAPSET-EL6313S)                                
01176          FROM     (EL631FI)                                       
01177          ERASE                                                    
01178          CURSOR                                                   
01179      END-EXEC.                                                    
01180                                                                   
01181      GO TO 9100-RETURN-TRAN.                                      
01182                                                                   
01183      EJECT                                                        
01184                                                                   
01185  8200-SEND-DATAONLY.                                              
01186      MOVE WS-CURRENT-DT          TO DATEO.                        
01187      MOVE EIBTIME                TO TIME-IN.                      
01188      MOVE TIME-OUT               TO TIMEO.                        
01189      MOVE EMI-MESSAGE-AREA (1)   TO ERMSG1O.                      
01190      MOVE EMI-MESSAGE-AREA (2)   TO ERMSG2O.                      
01191                                                                   
01192 *    IF PI-COMPANY-ID = 'DMD'                                     
01193 *       MOVE AL-SANOF          TO LIT1A LIT2A STTAXA MUNITXA      
01194 *                                 RATEA                           
01195 *       MOVE AL-UANOF          TO PFRATEA RTCODEA RESSTA.         
121713
121713***DO NOT ALLOW PRIMARY NAME TO CHANGE ON THIS SCREEN
121713     MOVE AL-SANOF             TO LASTNMA FIRSTNMA INITA.
01196                                                                   
01197      EXEC CICS SEND                                               
01198           MAP      (PI-MAP-NAME)                                  
01199           MAPSET   (MAPSET-EL6313S)                               
01200           FROM     (EL631FI)                                      
01201           DATAONLY                                                
01202           CURSOR                                                  
01203      END-EXEC.                                                    
01204                                                                   
01205      GO TO 9100-RETURN-TRAN.                                      
01206                                                                   
01207      EJECT                                                        
01208                                                                   
01209  8300-SEND-TEXT.                                                  
01210      EXEC CICS SEND TEXT                                          
01211          FROM     (LOGOFF-TEXT)                                   
01212          LENGTH   (LOGOFF-LENGTH)                                 
01213          ERASE                                                    
01214          FREEKB                                                   
01215      END-EXEC.                                                    
01216                                                                   
01217      EXEC CICS RETURN                                             
01218      END-EXEC

           .
043120 8400-LOG-JOURNAL-RECORD.                                         
043120
043120     display ' made it to 8400- '
043120
043120     if (pi-journal-file-id > 0)
043120        and (jp-file-id = file-id-erpndb)
043120        move eibdate             to jp-date
043120        move eibtime             to jp-time
043120*       move FILE-ID-ERPNDB      TO JP-FILE-ID
043120        MOVE PI-PROCESSOR-ID     TO JP-USER-ID
043120        MOVE 03                  TO PI-JOURNAL-FILE-ID
043120        if csr-edit-session
043120           move 'EL6311C'        to jp-program-id
043120        else
043120           MOVE THIS-PGM         TO JP-PROGRAM-ID
043120        end-if
043120        
043120**      length is 585 plus 30 extra for jrnl stuff
043120**      system already accounts for the 34
043120        
043120        EXEC CICS JOURNAL
043120           JFILEID   (PI-JOURNAL-FILE-ID)
043120           JTYPEID   ('EL')
043120           FROM      (JOURNAL-RECORD)
043120           LENGTH    (615)
043120           resp      (ws-response)
043120        END-EXEC
043120        
043120        if resp-normal
043120           continue
043120        else
043120           display ' error-el6313-journal ' ws-response
043120        end-if
043120     end-if

043120     .
043120 8400-exit.
043120     exit.

01219  8600-DEEDIT.                                                     
01220      EXEC CICS BIF DEEDIT                                         
01221          FIELD   (DEEDIT-FIELD)                                   
01222          LENGTH  (15)                                             
01223      END-EXEC.                                                    
01224                                                                   
01225  8600-EXIT.                                                       
01226      EXIT.                                                        
01227                                                                   
01228  8800-UNAUTHORIZED-ACCESS.                                        
01229      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   
01230      GO TO 8300-SEND-TEXT.                                        
01231                                                                   
01232  8810-PF23.                                                       
01233      MOVE EIBAID                 TO PI-ENTRY-CD-1.                
01234      MOVE XCTL-EL005             TO PGM-NAME.                     
01235      GO TO 9300-XCTL.                                             
01236                                                                   
01237  9100-RETURN-TRAN.                                                
01238      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             
01239      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.         
01240      EXEC CICS RETURN                                             
01241          TRANSID    (TRANS-EXB3)                                  
01242          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     
01243          LENGTH     (1536)                                        
01244      END-EXEC.                                                    
01245                                                                   
01246  9200-RETURN-MAIN-MENU.                                           
01247      MOVE XCTL-EL626             TO PGM-NAME.                     
01248      GO TO 9300-XCTL.                                             
01249                                                                   
01250  9300-XCTL.                                                       
01251      EXEC CICS XCTL                                               
01252          PROGRAM    (PGM-NAME)                                    
01253          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     
01254          LENGTH     (1536)                                        
01255      END-EXEC.                                                    
01256                                                                   
01257  9400-CLEAR.                                                      
01258      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME                      
01259      GO TO 9300-XCTL.                                             
01260                                                                   
01261  9500-PF12.                                                       
01262      MOVE XCTL-EL010             TO PGM-NAME.                     
01263      GO TO 9300-XCTL.                                             
01264                                                                   
01265  9600-PGMID-ERROR.                                                
01266      EXEC CICS HANDLE CONDITION                                   
01267          PGMIDERR    (8300-SEND-TEXT)                             
01268      END-EXEC.                                                    
01269                                                                   
01270      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           
01271      MOVE ' '                    TO PI-ENTRY-CD-1.                
01272      MOVE XCTL-EL005             TO PGM-NAME.                     
01273      MOVE PGM-NAME               TO LOGOFF-PGM.                   
01274      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  
01275      GO TO 9300-XCTL.                                             
01276                                                                   
01277  9700-DATE-LINK.                                                  
01278      EXEC CICS LINK                                               
01279          PROGRAM  (LINK-ELDATCV)                                  
01280          COMMAREA (DATE-CONVERSION-DATA)                          
01281          LENGTH   (DC-COMM-LENGTH)                                
01282      END-EXEC.                                                    
01283                                                                   
01284  9700-EXIT.                                                       
01285      EXIT.                                                        
01286                                                                   
01287  9800-LINK-PENDING-EDIT.                                          
01288      MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.          
01289      MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.            
01290                                                                   
01291      MOVE LINK-EL050             TO PGM-NAME.                     
01292      MOVE PENDING-BUSINESS       TO PASSED-RECORD.                
01293      MOVE ZEROS                  TO WK-CO-MAX-CAP                 
01294                                     WK-CO-TOL-CLAIM               
01295                                     WK-CO-TOL-PREM                
01296                                     WK-CO-TOL-REFUND              
01297                                     WK-CO-TOL-PREM-PCT            
01298                                     WK-CO-TOL-REFUND-PCT          
01299                                     WK-REFUND-OVS-AMT             
01300                                     WK-REFUND-OVS-PCT             
01301                                     WK-ACCT-ADDR                  
01302                                     WK-LIFE-EDIT-ADDR             
01303                                     WK-AH-EDIT-ADDR               
01304                                     WK-LIFE-BEN-ADDR              
01305                                     WK-AH-BEN-ADDR                
01306                                     WK-STATE-ADDR                 
01307                                     WK-PLAN-ADDR                  
01308                                     WK-FORM-ADDR.                 
01309                                                                   
01310      MOVE '6'                    TO WK-ENTRY-SW.                  
01311                                                                   
01312      MOVE SPACES                 TO EDIT-CRITERIA-DATA.           
01313                                                                   
01314      MOVE LOW-VALUES             TO EC-CO-MONTH-END-DT            
01315                                     EC-AM-EXPIRATION-DT           
01316                                     EC-AM-EFFECTIVE-DT            
01317                                     EC-CM-LF-CANCEL-DT            
01318                                     EC-CM-AH-CANCEL-DT            
01319                                     EC-CM-DEATH-DT.               
01320                                                                   
01321      MOVE ZEROS                  TO EC-CO-TOL-PREM                
01322                                     EC-CO-TOL-REFUND              
01323                                     EC-CO-MIN-AGE                 
01324                                     EC-CO-MIN-PREMIUM             
01325                                     EC-CO-MIN-TERM                
01326                                     EC-CO-MAX-TERM                
01327                                     EC-ST-TOL-PREM                
01328                                     EC-ST-TOL-REFUND              
01329                                     EC-AM-LF-TOL-PREM             
01330                                     EC-AM-AH-TOL-PREM             
01331                                     EC-AM-LF-MAX-ATT-AGE          
01332                                     EC-AM-LF-MAX-AGE              
01333                                     EC-AM-LF-MAX-TERM             
01334                                     EC-AM-LF-MAX-TOT-BEN          
01335                                     EC-AM-AH-MAX-ATT-AGE          
01336                                     EC-AM-AH-MAX-AGE              
01337                                     EC-AM-AH-MAX-TERM             
01338                                     EC-AM-AH-MAX-TOT-BEN          
01339                                     EC-AM-AH-MAX-MON-BEN          
01340                                     EC-RT-LF-MAX-ATT-AGE          
01341                                     EC-RT-LF-MAX-AGE              
01342                                     EC-RT-LF-MAX-TERM             
01343                                     EC-RT-LF-MAX-TOT-BEN          
01344                                     EC-RT-AH-MAX-ATT-AGE          
01345                                     EC-RT-AH-MAX-AGE              
01346                                     EC-RT-AH-MAX-TERM             
01347                                     EC-RT-AH-MAX-TOT-BEN          
01348                                     EC-RT-AH-MAX-MON-BEN          
01349                                     EC-RT-AH-RATE                 
01350                                     EC-RT-LF-RATE                 
01351                                     EC-RT-LF-NSP-RATE             
01352                                     EC-RT-AH-NSP-RATE             
01353                                     EC-CM-LF-PRIOR-REFUND         
01354                                     EC-CM-AH-PRIOR-REFUND.        
01355                                                                   
01356      EXEC CICS LINK                                               
01357           PROGRAM   (PGM-NAME)                                    
01358           COMMAREA  (PASSED-RECORD)                               
01359           LENGTH    (WS-EDIT-PASS-LENGTH)                         
01360       END-EXEC.                                                   
01361                                                                   
01362      MOVE '1'                    TO PI-EDIT-SW.                   
01363      MOVE EDIT-CRITERIA-DATA     TO PI-CRITERIA-DATA.             
01364                                                                   
01365      MOVE PASSED-RECORD          TO PENDING-BUSINESS.             
01366                                                                   
01367      IF VALID-PB-ID                                               
01368         GO TO 9800-EXIT.                                          
01369                                                                   
01370      MOVE PENDING-BUSINESS       TO EMI-LINE1.                    
01371                                                                   
01372      MOVE -1                     TO MAINTL.                       
01373                                                                   
01374      GO TO 8200-SEND-DATAONLY.                                    
01375                                                                   
01376  9800-EXIT.                                                       
01377       EXIT.                                                       
01378                                                                   
01379  9900-ERROR-FORMAT.                                               
01380      MOVE 2                      TO EMI-NUMBER-OF-LINES           
01381      MOVE PI-COMPANY-ID          TO EMI-CLIENT-ID.                
01382                                                                   
01383      IF NOT EMI-ERRORS-COMPLETE                                   
01384          MOVE LINK-EL001         TO PGM-NAME                      
01385          EXEC CICS LINK                                           
01386              PROGRAM    (PGM-NAME)                                
01387              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           
01388              LENGTH     (EMI-COMM-LENGTH)                         
01389          END-EXEC.                                                
01390                                                                   
01391  9900-EXIT.                                                       
01392      EXIT.                                                        
01393                                                                   
01394  9990-ABEND.                                                      
01395      MOVE LINK-EL004             TO PGM-NAME.                     
01396      MOVE DFHEIBLK               TO EMI-LINE1                     
01397      EXEC CICS LINK                                               
01398          PROGRAM   (PGM-NAME)                                     
01399          COMMAREA  (EMI-LINE1)                                    
01400          LENGTH    (72)                                           
01401      END-EXEC.                                                    
01402                                                                   
01403       MOVE -1                    TO FPFENTRL                      
01404                                                                   
01405      GO TO 8200-SEND-DATAONLY.                                    
01406                                                                   
01407  9995-SECURITY-VIOLATION.                                         
01408                              COPY ELCSCTP.                        
01409                                                                   
