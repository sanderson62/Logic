00001  ID DIVISION.                                              
00002                                                                   EL6522
00003  PROGRAM-ID.                 EL6522.    
00009 *AUTHOR.                     SUZAN VUKOV. 
00022 *                                                                 EL6522
00023 *REMARKS.     TRANSACTION - EXDC - COMPENSATION NOTE MAINTENANCE.
00022 *                                                                 EL6522
022803******************************************************************
022803*                   C H A N G E   L O G
022803*
022803* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
022803*-----------------------------------------------------------------
022803*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
022803* EFFECTIVE    NUMBER
022803*-----------------------------------------------------------------
022803* 022803    2002032200002  SMVA  NEW ONLINE PROGRAM                     
021511* 021511  IR2011021400003  AJRA  FIX ACCOUNT NAME
022803******************************************************************

00025  ENVIRONMENT DIVISION.                                            EL6522
00026                                                                   EL6522
00027      EJECT                                                        EL6522
00028  DATA DIVISION.                                                   EL6522
00029  WORKING-STORAGE SECTION.                                         EL6522
00030  77  FILLER  PIC X(32)  VALUE '********************************'. EL6522
00031  77  FILLER  PIC X(32)  VALUE '*     EL6522 WORKING STORAGE   *'. EL6522
00032  77  FILLER  PIC X(32)  VALUE '********* VMOD=2.005 ***********'.    CL**5
00033                                                                   EL6522
00034      COPY ELCSCTM.                                                EL6522
00035      COPY ELCSCRTY.                                               EL6522
00036                                                                   EL6522
00037      EJECT                                                        EL6522
00038  01  STANDARD-AREAS.                                              EL6522
00039      12  GETMAIN-SPACE               PIC X       VALUE SPACE.    
00040      12  MAP-NAME                    PIC X(8)    VALUE 'EL6522A'. 
00041      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL6522S '.
00042      12  SCRN-NUMBER                 PIC X(4)    VALUE '652C'.
00043      12  TRANS-ID                    PIC X(4)    VALUE 'EXDC'.
00044      12  W-PRINT-TRANS               PIC X(4)    VALUE 'EXDD'. 
00045      12  THIS-PGM                    PIC X(8)    VALUE 'EL6522'.
00046      12  PGM-NAME                    PIC X(8).                
00047      12  PGM-EL652                   PIC X(8)    VALUE 'EL652'.
00048                                                                   EL6522
00049      12  ERCONT-FILE-ID              PIC X(8)    VALUE 'ERCONT'.
00050      12  ELCNTL-FILE-ID              PIC X(8)    VALUE 'ELCNTL'.
00051      12  QID.                                                     EL6522
00052          16  QID-TERM                PIC X(4).               
00053          16  FILLER                  PIC X(4)    VALUE '652C'.
00054      12  QID-ITEM                    PIC S9(4)   COMP VALUE +0.
00055      12  SC-ITEM                     PIC S9(4)   COMP VALUE +1. 
00056                                                                   EL6522
00057  01  WORK-AREA.                                                   EL6522
00058      12  SAVE-DATE                   PIC X(8)    VALUE SPACES.
00059      12  SAVE-BIN-DATE               PIC X(2)    VALUE SPACES. 
00060                                                                   EL6522
00061      12  FN-CONVERT                  PIC XX.                    
00062      12  FN-BIN REDEFINES FN-CONVERT PIC S9(4)  COMP.            
00063                                                                   EL6522
00064      12  ERCONT-LENGTH               PIC S9(4)   COMP VALUE +128. 
00065      12  ERCONT-KEY-LENGTH           PIC S9(4)   COMP VALUE +31.
00066      12  ERCONT-START-LENGTH         PIC S9(4)   COMP VALUE +29.

00067      12  ERCONT-KEY.                                              EL6522
00068          16  ERCONT-PARTIAL-KEY.                                  EL6522
00069              20 ERCONT-COMPANY-CD    PIC X.                       EL6522
00071              20 ERCONT-CARRIER       PIC X.                       EL6522
00072              20 ERCONT-GROUPING      PIC X(06).                   EL6522
00073              20 ERCONT-FINRESP       PIC X(10).                   EL6522
00074              20 ERCONT-ACCOUNT       PIC X(10).                   EL6522
                   20 ERCONT-REC-TYP       PIC X(01).
00075          16 ERCONT-SEQ               PIC S9(4) COMP.       

00076      12  SV-PRIOR-KEY.                                            EL6522
00077          20 SV-COMPANY-CD            PIC X.                       EL6522
00079          20 SV-CARRIER               PIC X.                       EL6522
00080          20 SV-GROUPING              PIC X(06).                   EL6522
00081          20 SV-FINRESP               PIC X(10). 
00081          20 SV-ACCOUNT               PIC X(10). 
00078          20 SV-REC-TYP               PIC X.                       EL6522
00083          20 SV-SEQ                   PIC S9(4) COMP.              EL6522

00094      12  ELCNTL-KEY.                                              EL6522
00095          16  ELCNTL-COMPANY          PIC XXX.                  
00096          16  ELCNTL-REC-TYPE         PIC X.                   
00097          16  FILLER                  PIC X(4).               
00098          16  ELCNTL-SEQ-NO           PIC S9(4)   COMP.      

00099      12  TIME-IN                     PIC S9(7).            
00100      12  TIME-SPLIT REDEFINES TIME-IN.                            EL6522
00101          16  FILLER                  PIC X.               
00102          16  TIME-OUT                PIC 99V99.          
00103          16  FILLER                  PIC 9(2).          

00104      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.
00105      12  XCTL-126                    PIC X(8)    VALUE 'EL126'.
00106      12  LINK-001                    PIC X(8)    VALUE 'EL001'.
00107      12  LINK-004                    PIC X(8)    VALUE 'EL004'.    
00108      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'. 
00109      12  MAX-LINES                   PIC 9(03)   VALUE 300.      
00110      12  NUM-LINES-PER-SCREEN        PIC 9(02)   VALUE 12.      
00111      12  TS-NUM-REC-IN-GROUP         PIC 9(02)   VALUE 50.     
00112      12  TS-GROUP-WORK               PIC 9(5)    VALUE 0  COMP-3. 
00113      12  TS-LENGTH                   PIC S9(4)   VALUE +3600 COMP. 
00114      12  ROLL-COUNTER                PIC S999    VALUE +0 COMP-3.     
00115      12  TEMP-CURR-LINE              PIC S9(3)   COMP-3.             

00116      EJECT                                                        EL6522
00117  01  ERROR-MESSAGES.                                              EL6522
00118      12  ER-0000                     PIC X(04)       VALUE '0000'.      
00119      12  ER-0004                     PIC X(04)       VALUE '0004'.     
00120      12  ER-0006                     PIC X(04)       VALUE '0006'.    
00121      12  ER-0008                     PIC X(04)       VALUE '0008'.   
00122      12  ER-0023                     PIC X(04)       VALUE '0023'.  
00123      12  ER-0029                     PIC X(04)       VALUE '0029'. 
00124      12  ER-0030                     PIC X(04)       VALUE '0030'.
00125      12  ER-0031                     PIC X(04)       VALUE '0031'.   
00126      12  ER-0032                     PIC X(04)       VALUE '0032'.  
00127      12  ER-0033                     PIC X(04)       VALUE '0033'. 
00128      12  ER-0041                     PIC X(04)       VALUE '0041'.
00129      12  ER-0044                     PIC X(04)       VALUE '0044'.       
00130      12  ER-0045                     PIC X(04)       VALUE '0045'.      
00131      12  ER-0047                     PIC X(04)       VALUE '0047'.     
00132      12  ER-0048                     PIC X(04)       VALUE '0048'.    
00133      12  ER-0049                     PIC X(04)       VALUE '0049'.   
00134      12  ER-0050                     PIC X(04)       VALUE '0050'.  
00135      12  ER-0051                     PIC X(04)       VALUE '0051'. 
00136      12  ER-0066                     PIC X(04)       VALUE '0066'.       
00137      12  ER-0067                     PIC X(04)       VALUE '0067'.      
00138      12  ER-0069                     PIC X(04)       VALUE '0069'.     
00139      12  ER-0070                     PIC X(04)       VALUE '0070'.    
00140      12  ER-0140                     PIC X(04)       VALUE '0140'.   
00141      12  ER-0412                     PIC X(04)       VALUE '0412'.  
00142      12  ER-0413                     PIC X(04)       VALUE '0413'. 
00143      12  ER-3784                     PIC X(04)       VALUE '3784'.
00144      12  ER-3786                     PIC X(04)       VALUE '3786'.       
00145      12  ER-3787                     PIC X(04)       VALUE '3787'.      
00146      12  ER-3788                     PIC X(04)       VALUE '3788'.     
00147      EJECT                                                        EL6522
00148                         COPY ELCLOGOF.                            EL6522
00149      EJECT                                                        EL6522
00150                         COPY ELCAID.                              EL6522

00151  01  FILLER  REDEFINES DFHAID.                                    EL6522
00152      12  FILLER                      PIC X(8).   
00153      12  PF-VALUES OCCURS 24 TIMES   PIC X.           
00154      EJECT                                                        EL6522
00155                         COPY ELCEMIB.                             EL6522
00156      EJECT                                                        EL6522
00157                         COPY ELCINTF.                             EL6522
           12  PI-WORK-AREA  REDEFINES  PI-PROGRAM-WORK-AREA.
               16  PI-CHECK-MAINT-TYPE     PIC X.
                   88  VALID-MAINT-TYPE            VALUE 'S' 'A'
                                                         'C' 'D'.
                   88  ADD-FUNCTION                VALUE 'A'.
                   88  SHOW-FUNCTION               VALUE 'S'.
                   88  DELETE-FUNCTION             VALUE 'D'.
                   88  CHANGE-FUNCTION             VALUE 'C'.
               16  PI-CHECK-TYPE           PIC X.
                   88  VALID-TYPE                  VALUE 'A' 'G'
                                                         'C'.
               16  PI-CHECK-CARRY-BAL      PIC X.
                   88  VALID-CARRY-BAL             VALUE 'Y' 'N'.
               16  PI-FIRST-TIME-SW        PIC X.
  
               16  PI-ERCOMP-EOF-SW        PIC X.
                   88  ERCOMP-EOF                  VALUE 'Y'.
               16  PI-SAVE-PHONE           PIC X(10).
               16  PI-SAVE-PHONE-RED REDEFINES
                                     PI-SAVE-PHONE  PIC 9(10).
               16  PI-ERC-END-BAL          PIC S9(9)V99   COMP-3.
               16  PI-ERCOMP-KEY.
                   20  PI-ERC-COMPANY-CD   PIC X.
                   20  PI-ERC-CARRIER      PIC X.
                   20  PI-ERC-GROUP        PIC X(6).
                   20  PI-ERC-RESP         PIC X(10).
                   20  PI-ERC-ACCT         PIC X(10).
                   20  PI-ERC-TYPE         PIC X.
               16  PI-SAVE-ERCOMP-KEY      PIC X(29).
               16  PI-SAVE-FAXNO           PIC X(10).
               16  PI-SAVE-FAXNO-RED REDEFINES
                                     PI-SAVE-FAXNO  PIC 9(10).
               16  PI-SAVE-ADDR2           PIC X(30).
021511         16  PI-SAVE-CITYST.
021511             20 PI-SAVE-CITY         PIC X(28).
021511             20  PI-SAVE-STATE       PIC XX.
               16  PI-SAVE-ACCT-NAME       PIC X(30).
               16  PI-EL652-DEL-SW         PIC X.
               16  PI-UPDATE-SW            PIC X.
                   88  PI-CHANGES-MADE                 VALUE '1'.
               16  PI-TOTAL-LINES          PIC S99. 
               16  PI-CURRENT-LINE         PIC S9(3)   COMP-3.
               16  PI-TEMP-STOR-ITEMS      PIC S9(4)   COMP.
021511         16  FILLER                  PIC X(453).

00161                         COPY ELCATTR.   
00162      EJECT                             
00163                         COPY ELCDATE. 
00164      EJECT                           
00165                         COPY EL6522S.  
00166      EJECT                                                        EL6522

00167  01  EL6522R REDEFINES EL6522AI.                                  EL6522
022803     12  FILLER                      PIC X(126).    
00169      12  SC-ALL-LINES.                               
00170       14 SC-LINES OCCURS 12 TIMES INDEXED BY SC-INDX.        
00171          16  SC-LINL                 PIC S9(4)   COMP.          
00172          16  SC-LINA                 PIC X.                    
00173          16  SC-LIN                  PIC Z99.                 
00174          16  SC-TEXTL                PIC S9(4)   COMP.       
00175          16  SC-TEXTA                PIC X.                 
00176          16  SC-TEXT                 PIC X(60).            
00177          16  SC-MAINT-BYL            PIC S9(4)   COMP.    
00178          16  SC-MAINT-BYA            PIC X.              
00179          16  SC-MAINT-BY             PIC XXXX.          
00180          16  SC-MAINT-DTL            PIC S9(4)   COMP. 
00181          16  SC-MAINT-DTA            PIC X.           
00182          16  SC-MAINT-DT             PIC X(6).      
00183      12  FILLER                      PIC X(23).    

00184      EJECT                                                        EL6522
00185  01  RECORD-TABLE                    PIC X(21600) VALUE SPACES.

00186  01  REC-TABLE  REDEFINES RECORD-TABLE.                           EL6522
00187      12  TS-GROUP OCCURS 6 TIMES INDEXED BY TS-INDX PIC X(3600).  EL6522

00188  01  REC-ENTRIES REDEFINES RECORD-TABLE.                          EL6522
00189      12  REC-ENT OCCURS 300 TIMES INDEXED BY TB-INDX TB-INDX1.    EL6522
00190          16  REC-TEXT                PIC X(60).     
00191          16  REC-LAST-MAINT-BY       PIC XXXX.       
00192          16  REC-LAST-MAINT-DT       PIC XX.          
00193          16  REC-LAST-MAINT-HHMMSS   PIC S9(7) COMP-3. 
00194          16  REC-PC                  PIC X(2).        
00195                                                                   EL6522
00196  01  TS-WORK-AREA                    PIC X(3600).      
00197      EJECT                                              
00198                                     
00199  LINKAGE SECTION.                                                 EL6522
00200  01  DFHCOMMAREA                     PIC X(1500).                     
00201                                                                   EL6522
00203      EJECT                                                        EL6522
00204                         COPY ERCCOMP. 
00203      EJECT                                                        EL6522
00204                         COPY ERCCONT. 
00205      EJECT                                                        EL6522
00206                         COPY ELCCNTL.     
00207      EJECT                                                        EL6522

00208  PROCEDURE DIVISION.
00209                                                                   EL6522
00211      MOVE EIBDATE                     TO DC-JULIAN-YYDDD
00212      MOVE '5'                         TO DC-OPTION-CODE
00213      PERFORM 9700-LINK-DATE-CONVERT   THRU 9700-EXIT
00214      MOVE DC-GREG-DATE-1-EDIT         TO SAVE-DATE
00215      MOVE DC-BIN-DATE-1               TO SAVE-BIN-DATE
00216                                                                   EL6522
00217      MOVE DFHCOMMAREA                 TO PROGRAM-INTERFACE-BLOCK        
00210      MOVE EIBTRMID                    TO QID-TERM

00218      IF EIBCALEN = ZEROS                                          EL6522
00219          GO TO 8800-UNAUTHORIZED-ACCESS
           END-IF
00220                                                                   EL6522
00221      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL6522
00223          MOVE PI-SAVED-PROGRAM-5         TO PI-SAVED-PROGRAM-6
00224          MOVE PI-SAVED-PROGRAM-4         TO PI-SAVED-PROGRAM-5
00225          MOVE PI-SAVED-PROGRAM-3         TO PI-SAVED-PROGRAM-4
00226          MOVE PI-SAVED-PROGRAM-2         TO PI-SAVED-PROGRAM-3
00227          MOVE PI-SAVED-PROGRAM-1         TO PI-SAVED-PROGRAM-2
00228          MOVE PI-RETURN-TO-PROGRAM       TO PI-SAVED-PROGRAM-1 
00229          MOVE PI-CALLING-PROGRAM         TO PI-RETURN-TO-PROGRAM
00230          MOVE THIS-PGM                   TO PI-CALLING-PROGRAM
           END-IF
00240                                                                   EL6522
CIDMOD     MOVE LOW-VALUES                     TO EL6522AI
00241      MOVE SPACES                         TO ERCONT-KEY
00242                                                                   EL6522
00247      MOVE PI-CR-CARRIER                  TO ERCONT-CARRIER
00250      MOVE PI-CR-GROUPING                 TO ERCONT-GROUPING 
           MOVE PI-CR-FIN-RESP                 TO ERCONT-FINRESP

           IF PI-CR-ACCOUNT = SPACES OR LOW-VALUES
               MOVE PI-CR-FIN-RESP             TO ERCONT-ACCOUNT
           ELSE
00256          MOVE PI-CR-ACCOUNT              TO ERCONT-ACCOUNT
           END-IF

00245      MOVE PI-CR-TYPE                     TO ERCONT-REC-TYP

00282      MOVE PI-COMPANY-CD                  TO ERCONT-COMPANY-CD
00283      MOVE ZEROS                          TO ERCONT-SEQ
00284      MOVE ERCONT-PARTIAL-KEY             TO SV-PRIOR-KEY
00285                                                                   EL6522
00286      IF EIBTRNID NOT = TRANS-ID                                   EL6522
00287          MOVE '0'                        TO PI-UPDATE-SW 
00289          EXEC CICS READQ TS                   
00290               QUEUE (PI-SECURITY-TEMP-STORE-ID)
00291               INTO (SECURITY-CONTROL)           
00292               LENGTH (SC-COMM-LENGTH)            
00293               ITEM (SC-ITEM)                      
00294          END-EXEC                         
00295          MOVE SC-CREDIT-DISPLAY (05)  TO PI-DISPLAY-CAP
00296          MOVE SC-CREDIT-UPDATE (05)   TO PI-MODIFY-CAP
           END-IF
00297                                                       
00298      IF NOT DISPLAY-CAP                                
00299          MOVE 'READ'                     TO SM-READ            
00300          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT  
00301          MOVE ER-0070                    TO EMI-ERROR         
00302          PERFORM 9900-ERROR-FORMAT       THRU 9900-EXIT    
00303          GO TO 8100-SEND-INITIAL-MAP
           END-IF
00304                             
00305      EXEC CICS HANDLE AID    
00306           CLEAR (9400-CLEAR)   
00307      END-EXEC                 
00308                                
00309      EXEC CICS HANDLE CONDITION 
00310           ERROR (9990-ABEND) 
00311           PGMIDERR (9600-PGMID-ERROR) 
00312      END-EXEC                                                        CL**2
00313                                                                      CL**2
00314      IF EIBTRNID NOT = TRANS-ID                                      CL**2
00315          GO TO 7000-BUILD-TABLE                                      CL**2
           END-IF

00316      .
 
00318  2000-RECEIVE.                                                    EL6522

00319      IF EIBAID = DFHPA1 OR                                        EL6522
00320         EIBAID = DFHPA2 OR                                        EL6522
00321         EIBAID = DFHPA3                                           EL6522
00322          MOVE ER-0008                  TO EMI-ERROR         
00323          PERFORM 9900-ERROR-FORMAT     THRU 9900-EXIT    
00324          GO TO 8200-SEND-DATAONLY                   
           END-IF
00325                                                                   EL6522
00326      EXEC CICS RECEIVE                                            EL6522
00327           MAP(MAP-NAME)                                           EL6522
00328           MAPSET(MAPSET-NAME)                                     EL6522
00329           INTO(EL6522AI)                                          EL6522
00330      END-EXEC
00331                                                                   EL6522
00332      IF PFENTERL = ZEROS                                          EL6522
00333          GO TO 2001-CHECK-PFKEYS
           END-IF
00334                                                                   EL6522
00335      IF EIBAID NOT = DFHENTER                                     EL6522
00336          MOVE ER-0004                  TO EMI-ERROR 
00337          GO TO 2002-INPUT-ERROR
           END-IF
00338                                                
00339      IF PFENTERI NUMERIC AND                    
00340         (PFENTERI > 00 AND  < 25)                
00341          MOVE PF-VALUES (PFENTERI)     TO EIBAID      
00342      ELSE   
00343          MOVE ER-0029                  TO EMI-ERROR     
00344          GO TO 2002-INPUT-ERROR
           END-IF
00345                                                                   EL6522
           .

00346  2001-CHECK-PFKEYS.                                               EL6522

00347      IF EIBAID = DFHPF23                                          EL6522
00348          GO TO 9000-RETURN-CICS
           END-IF
00349                                                                   EL6522
00350      IF EIBAID = DFHPF24                                          EL6522
00351          GO TO 9200-RETURN-MAIN-MENU
           END-IF
00352                            
00353      IF EIBAID = DFHPF12    
00354          GO TO 9500-PF12
           END-IF
00355                              
00356      IF FUNCTL NOT = ZEROS AND
              EIBAID NOT = DFHENTER  
00357          IF FUNCTI = 'A' OR = SPACES               
                   CONTINUE
00359          ELSE                                   
00360              MOVE ER-0050              TO EMI-ERROR     
00361              MOVE -1 TO FUNCTL                 
00362              MOVE AL-UABON             TO FUNCTA PFENTERA 
00363              GO TO 2002-INPUT-ERROR
               END-IF
           END-IF
00364                                              
00365      IF EIBAID = DFHPF1                     
00366          MOVE NUM-LINES-PER-SCREEN     TO ROLL-COUNTER 
00367          GO TO 7400-PAGE-ROUTINE
           END-IF
00368                                                                   EL6522
00369      IF EIBAID = DFHPF2                                           EL6522
00370          SUBTRACT NUM-LINES-PER-SCREEN FROM ROLL-COUNTER 
00371          GO TO 7400-PAGE-ROUTINE
           END-IF
00372                                                                   EL6522
00373      IF EIBAID = DFHPF3                                           EL6522
00374          MOVE 5                        TO ROLL-COUNTER  
00375          GO TO 7400-PAGE-ROUTINE
           END-IF
00376                                                                   EL6522
00377      IF EIBAID = DFHPF4                                           EL6522
00378          MOVE -5                       TO ROLL-COUNTER 
00379          GO TO 7400-PAGE-ROUTINE
           END-IF
00380                                                                   EL6522
00384      IF EIBAID = DFHENTER                                         EL6522
00385          GO TO 2003-EDIT-DATA
           END-IF
00386                                                                   EL6522
00387      MOVE ER-0029                      TO EMI-ERROR

           .
00388                                                                   EL6522
00389  2002-INPUT-ERROR.                                                EL6522

00390      MOVE -1                           TO PFENTERL       
00391      MOVE AL-UNBON                     TO PFENTERA      
00392      PERFORM 9900-ERROR-FORMAT         THRU 9900-EXIT    
00393      GO TO 8200-SEND-DATAONLY

           .
00394                                                                   EL6522
00395  2003-EDIT-DATA.                                                  EL6522
00396                                                                   EL6522
00397      IF FUNCTI = 'L'                                              EL6522
               CONTINUE
00399      ELSE                                                         EL6522
00400          IF NOT MODIFY-CAP                                
00401              MOVE 'UPDATE'                   TO SM-READ               
00402              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT 
00403              MOVE ER-0070                    TO EMI-ERROR          
00404              PERFORM 9900-ERROR-FORMAT       THRU 9900-EXIT      
00405              GO TO 8100-SEND-INITIAL-MAP
               END-IF
           END-IF
00406                                               
00407      IF FUNCTL = ZEROS OR FUNCTI = SPACES      
00408          GO TO 4000-CHANGE-ROUTINE
           END-IF
00409                                                  
00410      IF (FUNCTI = 'S' OR = 'D' OR = 'Q' OR        
00411                 = 'I' OR = 'A' OR = 'L')           
               CONTINUE
00413      ELSE                                            
00414          MOVE ER-0023                  TO EMI-ERROR         
00415          PERFORM 9900-ERROR-FORMAT     THRU 9900-EXIT      
00416          MOVE AL-UABON                 TO FUNCTA 
00417          MOVE -1                       TO FUNCTL  
00418          GO TO 8200-SEND-DATAONLY
           END-IF
00419                                               
00420      IF FUNCTI = 'D'  OR = 'I' OR = 'L'        
00421          PERFORM 2500-LINE-CHECK       THRU 2599-EXIT  
00422      ELSE                                    
00423          IF (LINE1L NOT = ZEROS OR                 
00424              LINE2L NOT = ZEROS)                     
00425              MOVE ER-0030              TO EMI-ERROR      
00426              MOVE -1                   TO LINE1L          
00427              MOVE AL-UNBON             TO LINE1A LINE2A    
00428              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT   
00429              GO TO 8200-SEND-DATAONLY
               END-IF
           END-IF
00430                                                                      CL**4
00431      IF FUNCTI = 'A'                                              EL6522
00432          GO TO 5000-ADD-NEW-LINES
           END-IF

00433      IF FUNCTI = 'Q'                                              EL6522
00434          GO TO 9410-RETURN
           END-IF

00435      IF FUNCTI = 'S'                             
00436          GO TO 4500-SAVE-DATA
           END-IF

00437      IF PI-TOTAL-LINES = 0                        
00438          MOVE ER-0048                 TO EMI-ERROR       
00439          MOVE -1                      TO FUNCTL           
00440          MOVE AL-UNBON                TO FUNCTA            
00441          PERFORM 9900-ERROR-FORMAT    THRU 9900-EXIT   
00442          GO TO 8200-SEND-DATAONLY
           END-IF

00443      IF FUNCTI = 'L'                                              EL6522
00444          GO TO 5500-LOOKUP
           END-IF

00445      IF FUNCTI = 'D'                                              EL6522
00446          GO TO 3000-DELETE-LINES
           END-IF
00447                                                                      CL**4
00448      GO TO 3500-INSERT-LINES

           . 

00450  2500-LINE-CHECK.                                                 EL6522

00451      IF (LINE1L = ZEROS AND   
00452          LINE2L = ZEROS)                         
00453          MOVE ER-0069                 TO EMI-ERROR   
00454          PERFORM 9900-ERROR-FORMAT    THRU 9900-EXIT 
00455          MOVE -1                      TO LINE1L    
00456          GO TO 8200-SEND-DATAONLY
           END-IF
00457                                                
00458      IF LINE1L NOT = ZEROS                      
00459          IF (LINE1I NOT NUMERIC OR                
00460              LINE1I > PI-TOTAL-LINES)              
00461              MOVE ER-0031              TO EMI-ERROR    
00462              MOVE AL-UNBON             TO LINE1A        
00463              MOVE -1                   TO LINE1L         
00464              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT 
00465              GO TO 8200-SEND-DATAONLY             
00466          ELSE                                      
00467              IF LINE2L = ZEROS                      
00468                  MOVE 1                TO LINE2I        
                   END-IF

00469              IF FUNCTI = 'I'            
00470                  GO TO 2510-MAX-CHECK    
                   END-IF

00476              IF LINE2I NOT NUMERIC                   
00477                  MOVE AL-UNBON             TO LINE2A              
00478                  MOVE ER-0032              TO EMI-ERROR            
00479                  MOVE -1                   TO LINE2L                
00480                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00481                  GO TO 8200-SEND-DATAONLY
00482              ELSE                       
                       CONTINUE
                   END-IF
               END-IF 
00484      ELSE                                        
00485          IF LINE2L = ZEROS                        
                   CONTINUE
00487          ELSE                                      
00488              MOVE -1                   TO LINE2L    
00489              MOVE ER-0041              TO EMI-ERROR  
00490              MOVE AL-UNBON             TO LINE2A      
00491              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT  
00492              GO TO 8200-SEND-DATAONLY
               END-IF
           END-IF

00493      GO TO 2599-EXIT

           .

00494  2510-MAX-CHECK.                                                  EL6522

00495      IF LINE2I NOT NUMERIC                                        EL6522
00496          MOVE -1                       TO LINE2L   
00497          MOVE ER-0032                  TO EMI-ERROR
00498          MOVE AL-UNBON                 TO LINE2A  
00499          PERFORM 9900-ERROR-FORMAT     THRU 9900-EXIT
00500          GO TO 8200-SEND-DATAONLY                 
00501      ELSE                                        
00502          COMPUTE ROLL-COUNTER = LINE2I + PI-TOTAL-LINES   
00503          IF ROLL-COUNTER > MAX-LINES                  
00504              MOVE -1                   TO LINE2L     
00505              MOVE ER-0044              TO EMI-ERROR 
00506              MOVE AL-UNBON             TO LINE2A   
00507              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT 
00508              GO TO 8200-SEND-DATAONLY
               END-IF
           END-IF

           .     

00509  2599-EXIT.           
00510      EXIT.           

00512  3000-DELETE-LINES.                                               EL6522

00513      IF LINE2L = ZEROS AND LINE2I = 1                             EL6522
00514          MOVE LINE1I                 TO LINE2I
           END-IF
00515                                                                   EL6522
00516      IF LINE2I > PI-TOTAL-LINES OR < LINE1I                       EL6522
00517          MOVE ER-0049                TO EMI-ERROR   
00518          MOVE AL-UNBON               TO LINE2A       
00519          MOVE -1                     TO LINE2L        
00520          PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT    
00521          GO TO 8200-SEND-DATAONLY
           END-IF
00522                                                                      CL**4
00523      PERFORM 7450-SET-INDX           THRU 7450-EXIT

00524      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL6522
00525              VARYING SC-INDX FROM 1 
                   BY 1
                   UNTIL SC-INDX > NUM-LINES-PER-SCREEN  

00527      SET TB-INDX TO LINE1I
00528                                                                      CL**4
00529      PERFORM 3050-CHECK-FOR-CURRENT-DATE                          EL6522
00530              UNTIL TB-INDX > LINE2I
00531                                                                      CL**4
00532      IF NOT EMI-NO-ERRORS                                         EL6522
00533          GO TO 8200-SEND-DATAONLY
           END-IF
00534                                                                      CL**4
00535      SET TB-INDX TO LINE1I                                        EL6522
00536      COMPUTE ROLL-COUNTER = LINE2I - LINE1I + 1
00537                                                                      CL**4
00538      IF LINE2I NOT = PI-TOTAL-LINES                               EL6522
00539          SET TB-INDX1 TO LINE2I    
00540          SET TB-INDX1 UP BY 1      
00541          PERFORM 3100-DELETE-TABLE-ENTRIES  
00542                  UNTIL TB-INDX1 > PI-TOTAL-LINES
           END-IF
00543                                                                      CL**4
00544      PERFORM 3150-BLANK-TABLE-ENTRIES                             EL6522
00545              ROLL-COUNTER TIMES                                   EL6522

00546      SUBTRACT ROLL-COUNTER FROM PI-TOTAL-LINES                    EL6522
00547                                                                      CL**4
00548      IF PI-CURRENT-LINE > PI-TOTAL-LINES                          EL6522
00549          MOVE PI-TOTAL-LINES         TO PI-CURRENT-LINE     
00550          SUBTRACT 1 FROM PI-CURRENT-LINE
           END-IF
00551                                                                   EL6522
00552      SET TB-INDX                     TO PI-CURRENT-LINE        
00553      MOVE LOW-VALUES                 TO EL6522AI
00554                                                                      CL**4
00555      IF PI-CURRENT-LINE > ZERO                                       CL**4
00556          PERFORM 7100-FORMAT-SCREEN  THRU 7100-EXIT    
00557                  VARYING SC-INDX FROM 1 
                       BY 1
                       UNTIL SC-INDX > NUM-LINES-PER-SCREEN 
00559          PERFORM 7200-PUT-TEMP-STOR  THRU 7249-EXIT
           END-IF
00560                                                                      CL**4
00561      MOVE '1'                        TO PI-UPDATE-SW

00562      IF PI-TOTAL-LINES = ZEROS                                    EL6522
00563          MOVE ZEROS                  TO PI-CURRENT-LINE
           END-IF

00564      GO TO 8100-SEND-INITIAL-MAP

           . 
00566                                                                   EL6522
00567  3050-CHECK-FOR-CURRENT-DATE.                                     EL6522

00568      IF REC-LAST-MAINT-DT (TB-INDX) = SAVE-BIN-DATE               EL6522
               CONTINUE
00570      ELSE                                                         EL6522
00571          MOVE ER-3786                TO EMI-ERROR
00572          MOVE AL-UNBON               TO FUNCTA    
00573          MOVE -1                     TO FUNCTL     
00574          PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT 
00575          GO TO 8200-SEND-DATAONLY
           END-IF
00576                                                                   EL6522
00577      IF REC-LAST-MAINT-BY (TB-INDX) = PI-PROCESSOR-ID             EL6522
               CONTINUE
00579      ELSE                                                         EL6522
00580          MOVE ER-3787                TO EMI-ERROR
00581          MOVE AL-UNBON               TO FUNCTA    
00582          MOVE -1                     TO FUNCTL     
00583          PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT 
00584          GO TO 8200-SEND-DATAONLY
           END-IF
00585                                                                   EL6522
00586      SET TB-INDX TB-INDX1 UP BY 1

           .
00587                                                                   EL6522
00588  3100-DELETE-TABLE-ENTRIES.                                       EL6522

00589      MOVE REC-ENT (TB-INDX1)         TO REC-ENT (TB-INDX)    
00590      SET TB-INDX TB-INDX1 UP BY 1

           .
00591                                                                   EL6522
00592  3150-BLANK-TABLE-ENTRIES.                                        EL6522

00593      MOVE SPACES               TO REC-ENT (TB-INDX)
00594      MOVE LOW-VALUES           TO REC-LAST-MAINT-DT (TB-INDX)
00595      MOVE ZERO                 TO REC-LAST-MAINT-HHMMSS (TB-INDX)         
00596      SET TB-INDX UP BY 1

           .

00598  3500-INSERT-LINES.                                               EL6522
00599                                                                      CL**4
00600      PERFORM 7450-SET-INDX                 THRU 7450-EXIT  
00601      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL6522
00602              VARYING SC-INDX FROM 1 
                   BY 1
                   UNTIL SC-INDX > NUM-LINES-PER-SCREEN
00604                                                                      CL**4
00605      IF NOT EMI-NO-ERRORS                                         EL6522
00606          GO TO 8200-SEND-DATAONLY
           END-IF
00607                                                                      CL**4
00608      SET TB-INDX TO PI-TOTAL-LINES                                EL6522
00609      ADD LINE2I TO PI-TOTAL-LINES                                 EL6522
00610      SET TB-INDX1 TO PI-TOTAL-LINES                               EL6522
00611      PERFORM 3600-INSERT-TABLE-ENTRIES                            EL6522
00612              UNTIL TB-INDX = LINE1I                               EL6522

00613      SET TB-INDX UP BY 1
00614                                                                   EL6522
00615      COMPUTE ROLL-COUNTER = PI-CURRENT-LINE +
                                  NUM-LINES-PER-SCREEN

00616      IF TB-INDX NOT < ROLL-COUNTER OR     
00617                     < PI-CURRENT-LINE   
00618          SET SC-INDX TO 1               
00619          SET SC-INDX DOWN BY 1         
00620      ELSE                            
00621          SET ROLL-COUNTER TO TB-INDX 
00622          COMPUTE ROLL-COUNTER = ROLL-COUNTER - PI-CURRENT-LINE
00623                   + 1                                       
00624          SET SC-INDX TO ROLL-COUNTER
           END-IF
00625                                                                   EL6522
00626      PERFORM 3150-BLANK-TABLE-ENTRIES 
                   LINE2I TIMES    

00627      SET TB-INDX TO PI-CURRENT-LINE                               EL6522
00628      MOVE LOW-VALUES                 TO EL6522AI  
00629                                                                      CL**4
00630      IF SC-INDX NOT = ZERO                                        EL6522
00631         MOVE -1                      TO SC-TEXTL (SC-INDX)
           END-IF
00632                                                                      CL**4
00633      PERFORM 7100-FORMAT-SCREEN      THRU 7100-EXIT  
00634             VARYING SC-INDX FROM 1 
                  BY 1
                  UNTIL SC-INDX > NUM-LINES-PER-SCREEN

00636      PERFORM 7200-PUT-TEMP-STOR      THRU 7249-EXIT 

00637      MOVE '1'                        TO PI-UPDATE-SW

00638      GO TO 8100-SEND-INITIAL-MAP

           .
00639                                                                   EL6522
00640  3600-INSERT-TABLE-ENTRIES.                                       EL6522

00641      MOVE REC-ENT (TB-INDX)          TO REC-ENT (TB-INDX1)    
00642      SET TB-INDX TB-INDX1 DOWN BY 1

           .
00644                                                                      CL**4
00645  4000-CHANGE-ROUTINE.                                             EL6522

00646      PERFORM 7450-SET-INDX                 THRU 7450-EXIT   

00647      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL6522
00648              VARYING SC-INDX FROM 1 
                   BY 1
                   UNTIL SC-INDX > NUM-LINES-PER-SCREEN
00650                                                                      CL**4
00651      IF NOT EMI-NO-ERRORS                                         EL6522
00652          GO TO 8200-SEND-DATAONLY
           END-IF
00653                                                                      CL**4
00654      PERFORM 7200-PUT-TEMP-STOR      THRU 7249-EXIT     

00655      MOVE SPACES                     TO ERRMSGBO 
00656      GO TO 8200-SEND-DATAONLY

           .
00657                                                                   EL6522
00659  4500-SAVE-DATA.                                                  EL6522

00660      PERFORM 7450-SET-INDX                 THRU 7450-EXIT
00661      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL6522
00662              VARYING SC-INDX FROM 1 
                   BY 1
                   UNTIL SC-INDX > NUM-LINES-PER-SCREEN

00664      IF NOT EMI-NO-ERRORS                                         EL6522
00665          GO TO 8200-SEND-DATAONLY
           END-IF
00666                                                                   EL6522
00667      EXEC CICS HANDLE CONDITION                                   EL6522
00668           NOTFND(4610-ENDBR)                                      EL6522
00669           NOTOPEN(6000-NOT-OPEN)                                  EL6522
00670           ENDFILE(4610-ENDBR)                                     EL6522
00671      END-EXEC

           .
00672                                                                   EL6522
00673  4610-LOOP.                                                       EL6522

00674      EXEC CICS READ                                               EL6522
00675           DATASET (ERCONT-FILE-ID)        
00676           RIDFLD (ERCONT-KEY)            
00677           SET (ADDRESS OF COMP-NOTE-FILE)
00678           GTEQ                          
00679      END-EXEC
00680                                                                   EL6522
00681      MOVE NF-CONTROL-PRIMARY         TO ERCONT-KEY
00682                                                                   EL6522
00683      IF ERCONT-PARTIAL-KEY NOT = SV-PRIOR-KEY                     EL6522
00684          MOVE SV-PRIOR-KEY           TO ERCONT-PARTIAL-KEY        EL6522
00685          GO TO 4610-ENDBR
           END-IF
00686                                                                   EL6522
00687      EXEC CICS DELETE                                             EL6522
00688          DATASET (ERCONT-FILE-ID)                                 EL6522
00689          RIDFLD (ERCONT-KEY)       
00690      END-EXEC
00691                                                                   EL6522
00692      GO TO 4610-LOOP

           .

00693  4610-ENDBR.                                                      EL6522

00694      EXEC CICS GETMAIN                                            EL6522
00695           LENGTH(ERCONT-LENGTH)                                   EL6522
00696           SET(ADDRESS OF COMP-NOTE-FILE)  
00697           INITIMG(GETMAIN-SPACE)                                  EL6522
00698      END-EXEC
00699                                                                   EL6522
00700      PERFORM 4700-WRITE-FILE         THRU 4799-EXIT   
00701              VARYING TB-INDX FROM 1 
                   BY 1
                   UNTIL TB-INDX > PI-TOTAL-LINES
00703                                                                   EL6522
00707      GO TO 9410-RETURN

           .
00708                                                                   EL6522
00709  4700-WRITE-FILE.                                                 EL6522

00710      MOVE SPACES                     TO COMP-NOTE-FILE
00711      ADD 1                           TO ERCONT-SEQ   
00712      MOVE ERCONT-KEY                 TO NF-CONTROL-PRIMARY
00713      MOVE  'NF'                      TO NF-FILE-ID
00714      MOVE REC-TEXT (TB-INDX)         TO NF-NOTE-LINE    
00715      MOVE REC-LAST-MAINT-BY (TB-INDX)                             EL6522
00716                                      TO NF-LAST-MAINT-BY
00717      MOVE REC-LAST-MAINT-HHMMSS (TB-INDX)                         EL6522
00718                                      TO NF-LAST-MAINT-HHMMSS
00719      MOVE REC-LAST-MAINT-DT (TB-INDX)                             EL6522
00720                                      TO NF-LAST-MAINT-DT
00721                                                                   EL6522
00722      EXEC CICS WRITE                                              EL6522
00723           DATASET(ERCONT-FILE-ID)                                 EL6522
00724           FROM(COMP-NOTE-FILE) 
00725           RIDFLD(ERCONT-KEY)                                      EL6522
00726      END-EXEC

           .
00727  4799-EXIT.                                                       EL6522
00728       EXIT.                                                       EL6522
00729                                                                   EL6522
00731  5000-ADD-NEW-LINES.                                              EL6522

00732      PERFORM 7450-SET-INDX                 THRU 7450-EXIT  
00733      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL6522
00734              VARYING SC-INDX FROM 1 
                   BY 1
                   UNTIL SC-INDX > NUM-LINES-PER-SCREEN 
00736                                                
00737      IF NOT EMI-NO-ERRORS                                         EL6522
00738          GO TO 8200-SEND-DATAONLY
           END-IF
00739                                                    
00740      MOVE PI-TOTAL-LINES             TO PI-CURRENT-LINE 
00741      PERFORM 7200-PUT-TEMP-STOR      THRU 7249-EXIT
00742      MOVE LOW-VALUES                 TO EL6522AI     
00743      SET TB-INDX                     TO PI-CURRENT-LINE
00744      MOVE 'A'                        TO FUNCTI      
00745      MOVE -1                         TO SC-TEXTL (2) 
00746      MOVE AL-UANON                   TO FUNCTA   
00747      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                    EL6522
00748              VARYING SC-INDX FROM 1
                   BY 1 
                   UNTIL SC-INDX > NUM-LINES-PER-SCREEN
00750      MOVE '1'                        TO PI-UPDATE-SW
00751      GO TO 8100-SEND-INITIAL-MAP

           .

00753  5500-LOOKUP.                                                     EL6522

00754      PERFORM 7500-READ-TS            THRU 7599-EXIT 
00755      SET TB-INDX                     TO PI-CURRENT-LINE
00756      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL6522
00757              VARYING SC-INDX FROM 1
                   BY 1
                   UNTIL SC-INDX > NUM-LINES-PER-SCREEN  
00759                                                                      CL**4
00760      IF NOT EMI-NO-ERRORS                                         EL6522
00761          GO TO 8200-SEND-DATAONLY
           END-IF
00762                                                                      CL**4
00763      MOVE LINE1I                     TO PI-CURRENT-LINE
00764      SET TB-INDX                     TO PI-CURRENT-LINE   
00765      MOVE LOW-VALUES                 TO EL6522AI 
00766      PERFORM 7100-FORMAT-SCREEN      THRU 7100-EXIT
00767              VARYING SC-INDX FROM 1 
                   BY 1     
00768              UNTIL SC-INDX > NUM-LINES-PER-SCREEN                 EL6522
00769      PERFORM 7200-PUT-TEMP-STOR      THRU 7249-EXIT
00770      GO TO 8100-SEND-INITIAL-MAP

           .

00772  6000-NOT-OPEN.                                                   EL6522

00773      MOVE ER-3788                    TO EMI-ERROR
00774      PERFORM 9900-ERROR-FORMAT       THRU 9900-EXIT 
00775                                                                      CL**4
00776      IF EIBAID = DFHCLEAR                                            CL**2
00777          GO TO 9410-RETURN                                           CL**2
00778      ELSE                                                         EL6522
00779          GO TO 8100-SEND-INITIAL-MAP
           END-IF

           .

00781  7000-BUILD-TABLE.                                                EL6522
00782                                                                      CL**4
00783      SET TB-INDX                     TO 1
00784      MOVE ZEROS                      TO PI-TOTAL-LINES   
00785                                         PI-CURRENT-LINE 
00786                                         PI-TEMP-STOR-ITEMS 
00787                                         PI-UPDATE-SW     
00788      MOVE LOW-VALUES                 TO EL6522AI        
00789      PERFORM 7500-READ-TS            THRU 7599-EXIT    
00790                                                                      CL**4
00791      IF PI-TEMP-STOR-ITEMS NOT = ZERO                             EL6522
00792          MOVE ER-0140                TO EMI-ERROR    
00793          PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
00794          MULTIPLY PI-TEMP-STOR-ITEMS BY TS-NUM-REC-IN-GROUP 
                    GIVING PI-TOTAL-LINES                                
00796          MOVE 1                      TO PI-CURRENT-LINE      
00797          SET TB-INDX TO 1                                 
00798          PERFORM 7100-FORMAT-SCREEN  THRU 7100-EXIT       
00799                  VARYING SC-INDX FROM 1                 
00800                  BY 1 
                       UNTIL SC-INDX > NUM-LINES-PER-SCREEN 
00801          GO TO 8100-SEND-INITIAL-MAP                     
           END-IF
00802                                                         
00803      EXEC CICS HANDLE CONDITION                                   EL6522
00804           NOTFND(7010-ENDBR)                                      EL6522
00805           NOTOPEN(6000-NOT-OPEN)                                  EL6522
00806           ENDFILE(7010-ENDBR)                                     EL6522
00807      END-EXEC
00808                                                                   EL6522
00809      EXEC CICS STARTBR                                            EL6522
00810           DATASET(ERCONT-FILE-ID)                                 EL6522
00811           RIDFLD(ERCONT-KEY)                                      EL6522
00812           KEYLENGTH(ERCONT-START-LENGTH)                          EL6522
00813           GENERIC                                                 EL6522
00814           GTEQ                                                    EL6522
00815      END-EXEC

           .
00816                                                                   EL6522
00817  7001-LOOP.                                                       EL6522

00818      EXEC CICS READNEXT                                           EL6522
00819           SET(ADDRESS OF COMP-NOTE-FILE)
00820           DATASET(ERCONT-FILE-ID)                                 EL6522
00821           RIDFLD(ERCONT-KEY)                                      EL6522
00822      END-EXEC
00823                                                                   EL6522
00824      IF NF-COMPANY-CD NOT = SV-COMPANY-CD                         EL6522
00825          GO TO 7010-ENDBR
           END-IF
00826                                                                   EL6522
00827      IF (NF-CARRIER = SV-CARRIER) AND                             EL6522
00828         (NF-GROUPING = SV-GROUPING) AND                           EL6522
00829         (NF-FIN-RESP-NO = SV-FINRESP) AND
00830         (NF-ACCOUNT = SV-ACCOUNT)                                 EL6522
00831          MOVE NF-NOTE-LINE         TO REC-TEXT (TB-INDX)   
00832          MOVE NF-LAST-MAINT-BY     TO REC-LAST-MAINT-BY (TB-INDX)
00833          MOVE NF-LAST-MAINT-DT     TO REC-LAST-MAINT-DT (TB-INDX)
00834          MOVE NF-LAST-MAINT-HHMMSS TO                       
00835                                   REC-LAST-MAINT-HHMMSS (TB-INDX)   
00836          SET TB-INDX UP BY 1                              
00837          GO TO 7001-LOOP
           END-IF 

           . 

00838  7010-ENDBR.                                                      EL6522

00839      IF TB-INDX = 1                                               EL6522
00840          MOVE ER-0006                TO EMI-ERROR  
00841          MOVE 'A'                    TO FUNCTI    
00842          MOVE -1                     TO SC-TEXTL (1)  
00843          MOVE AL-UANON               TO FUNCTA       
00844          PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT 
00845          MOVE ZEROS                  TO PI-TOTAL-LINES  
00846          GO TO 8100-SEND-INITIAL-MAP
           END-IF

00848      EXEC CICS ENDBR                             
00849           DATASET(ERCONT-FILE-ID)               
00850      END-EXEC
00851                                               
00852      SET TB-INDX DOWN BY 1
00853      SET PI-TOTAL-LINES              TO TB-INDX
00854      MOVE 1                          TO PI-CURRENT-LINE

           .
00855                                                   
00856  7050-FORMAT-LINES.                              

00857      SET TB-INDX                     TO PI-CURRENT-LINE
00858      PERFORM 7100-FORMAT-SCREEN      THRU 7100-EXIT 
00859              VARYING SC-INDX FROM 1           
00860              BY 1
                   UNTIL SC-INDX > NUM-LINES-PER-SCREEN
00861      PERFORM 7200-PUT-TEMP-STOR      THRU 7249-EXIT
00862      GO TO 8100-SEND-INITIAL-MAP

           .

00864  7100-FORMAT-SCREEN.                                              EL6522

00865      IF TB-INDX > PI-TOTAL-LINES                                  EL6522
00866          IF FUNCTI NOT = 'A'     
00867              MOVE AL-PANON           TO SC-TEXTA (SC-INDX)
               END-IF 
           END-IF
00868                                                                   EL6522
00869      IF TB-INDX > PI-TOTAL-LINES                                  EL6522
00870          GO TO 7100-EXIT
           END-IF
00871                                                                   EL6522
00872      MOVE REC-TEXT (TB-INDX)          TO SC-TEXT (SC-INDX)
00873      MOVE REC-LAST-MAINT-BY (TB-INDX) TO SC-MAINT-BY (SC-INDX)
00875      PERFORM 7110-FORMAT-DATE         THRU 7110-EXIT
00876      SET ROLL-COUNTER                 TO TB-INDX
00877      MOVE ROLL-COUNTER                TO SC-LIN (SC-INDX)
00878                                                                   EL6522
00879      IF NOT MODIFY-CAP                                            EL6522
00880          MOVE AL-PANOF                TO SC-TEXTA (SC-INDX) 
00881          SET TB-INDX UP BY 1                                      EL6522
00882          GO TO 7100-EXIT
           END-IF 
00883                                                                   EL6522
00884      IF (REC-LAST-MAINT-BY (TB-INDX) NOT = SPACES AND LOW-VALUES)    CL**2
00885           AND                                                     EL6522
00886         (REC-LAST-MAINT-BY (TB-INDX) NOT EQUAL PI-PROCESSOR-ID)   EL6522
00887          MOVE AL-PANOF                TO SC-TEXTA (SC-INDX)
00888          SET TB-INDX UP BY 1     
00889          GO TO 7100-EXIT
           END-IF
00890                                                                   EL6522
00891      IF (REC-LAST-MAINT-DT (TB-INDX) = SAVE-BIN-DATE OR           EL6522
00892                               LOW-VALUES OR SPACES)               EL6522
00893          SET TB-INDX UP BY 1   
00894      ELSE  
00895          MOVE AL-PANOF                TO SC-TEXTA (SC-INDX) 
00896          SET TB-INDX UP BY 1
           END-IF

           .
00898  7100-EXIT.                                                       EL6522
00899       EXIT.                                                       EL6522
00900                                                                   EL6522
00901  7110-FORMAT-DATE.                                                EL6522

00902      IF REC-LAST-MAINT-DT (TB-INDX) = LOW-VALUES OR SPACES        EL6522
               CONTINUE
00904      ELSE   
00905          MOVE REC-LAST-MAINT-DT (TB-INDX) TO DC-BIN-DATE-1 
00907          MOVE SPACES                      TO DC-OPTION-CODE
00908          PERFORM 9700-LINK-DATE-CONVERT   THRU 9700-EXIT    
00909          MOVE DC-GREG-DATE-1-MDY          TO
                                                SC-MAINT-DT (SC-INDX)        
           END-IF

           .
00911  7110-EXIT.
           EXIT. 
00912                                                                   EL6522
00913  7200-PUT-TEMP-STOR.                                              EL6522

00914      PERFORM 7250-DELETE-TEMP-STOR    THRU 7299-EXIT  
00915      SET TS-INDX                      TO 1                                
00916      MOVE 0                           TO PI-TEMP-STOR-ITEMS
00917      PERFORM 7300-WRITE-TS            THRU 7399-EXIT  
00918              VARYING TS-GROUP-WORK FROM 0
                   BY TS-NUM-REC-IN-GROUP          
00919              UNTIL TS-GROUP-WORK NOT LESS THAN PI-TOTAL-LINES

           .
00920  7249-EXIT.                                                    
00921      EXIT. 

00922  7250-DELETE-TEMP-STOR.                                           EL6522

00923      EXEC CICS HANDLE CONDITION                                   EL6522
00924           QIDERR(7299-EXIT)                                       EL6522
00925      END-EXEC

00926      EXEC CICS DELETEQ TS                                         EL6522
00927           QUEUE(QID)                                              EL6522
00928      END-EXEC

           .
00929  7299-EXIT.                                                       EL6522
00930      EXIT.                                                        EL6522

00932  7300-WRITE-TS.                                                   EL6522

00933      MOVE TS-GROUP (TS-INDX)          TO TS-WORK-AREA   
00934      SET TS-INDX UP BY 1                               
00935      ADD 1                            TO PI-TEMP-STOR-ITEMS 
00936      EXEC CICS WRITEQ TS                                          EL6522
00937           FROM(TS-WORK-AREA)                                      EL6522
00938           QUEUE(QID)                                              EL6522
00939           LENGTH(TS-LENGTH)                                       EL6522
00940           ITEM(PI-TEMP-STOR-ITEMS)                                EL6522
00941      END-EXEC

           .
00942  7399-EXIT.                                                       EL6522
00943      EXIT.                                                        EL6522

00945  7400-PAGE-ROUTINE.                                               EL6522
00946                                                                      CL**4
00947      IF PFENTERL NOT = ZEROS   
00948          MOVE -1                           TO PFENTERL  
00949      ELSE                                     
00950          MOVE -1                           TO FUNCTL
           END-IF
00951                                                                      CL**4
00952      IF PI-TOTAL-LINES = 0                                        EL6522
00953          MOVE ER-0047                      TO EMI-ERROR 
00954          MOVE -1                           TO FUNCTL   
00955          PERFORM 9900-ERROR-FORMAT         THRU 9900-EXIT
00956          GO TO 8200-SEND-DATAONLY
           END-IF
00957                                                                      CL**4
00958      COMPUTE TEMP-CURR-LINE = PI-CURRENT-LINE + ROLL-COUNTER
00959                                           
00960      IF TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS         EL6522
00961          MOVE ER-0067                      TO EMI-ERROR   
00962          PERFORM 9900-ERROR-FORMAT         THRU 9900-EXIT  
00963          MOVE 1                            TO TEMP-CURR-LINE
           END-IF
00964                                                                      CL**4
00965      IF TEMP-CURR-LINE > PI-TOTAL-LINES  
00966          MOVE ER-0066                      TO EMI-ERROR  
00967          PERFORM 9900-ERROR-FORMAT         THRU 9900-EXIT 
00968          COMPUTE TEMP-CURR-LINE = PI-TOTAL-LINES + 1 
00969                                - NUM-LINES-PER-SCREEN 
00970          IF TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS 
00971              MOVE 1                        TO TEMP-CURR-LINE
               END-IF
           END-IF
00972                                                                      CL**4
00973      PERFORM 7450-SET-INDX                 THRU 7450-EXIT    
00974      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT   
00975              VARYING SC-INDX FROM 1 
                   BY 1 
                   UNTIL SC-INDX > NUM-LINES-PER-SCREEN
00977                                                          
00978      IF EMI-ERROR = ER-0066 OR = ER-0067 OR = ZEROS     
               CONTINUE
00980      ELSE                                          
00981          GO TO 8200-SEND-DATAONLY
           END-IF
00982                                                     
00983      MOVE TEMP-CURR-LINE                   TO PI-CURRENT-LINE 
00984      SET TB-INDX                           TO PI-CURRENT-LINE        
00985      MOVE LOW-VALUES                       TO EL6522AI
00986      PERFORM 7100-FORMAT-SCREEN            THRU 7100-EXIT   
00987              VARYING SC-INDX FROM 1 
                   BY 1        
00988              UNTIL SC-INDX > NUM-LINES-PER-SCREEN 
00989      PERFORM 7200-PUT-TEMP-STOR            THRU 7249-EXIT

00990      GO TO 8100-SEND-INITIAL-MAP

           .
00992                                                                      CL**4
00993  7450-SET-INDX.                                                   EL6522

00994      IF PI-CURRENT-LINE = 0 AND PI-TOTAL-LINES = 0                EL6522
00995          SET TB-INDX                       TO 1 
00996      ELSE                                                         EL6522
00997          PERFORM 7500-READ-TS              THRU 7599-EXIT   
00998          IF PI-CURRENT-LINE = 0  
00999              SET TB-INDX                   TO 1 
01000          ELSE                       
01001              SET TB-INDX                   TO PI-CURRENT-LINE
               END-IF
           END-IF

           .
01002  7450-EXIT.                                                       EL6522
01003      EXIT.

01005  7500-READ-TS.                                                    EL6522

01006      EXEC CICS HANDLE CONDITION                                   EL6522
01007           QIDERR(7590-TS-QIDERR)                                  EL6522
01008           ITEMERR(7585-QID-ITEMERR)                               EL6522
01009      END-EXEC

01010      SET TS-INDX                           TO 1
01011      MOVE 1                                TO QID-ITEM
           PERFORM 7501-LOOP THRU 7501-EXIT 
00918              VARYING TS-GROUP-WORK FROM 0
                   BY TS-NUM-REC-IN-GROUP          
00919              UNTIL TS-GROUP-WORK NOT < PI-TOTAL-LINES

01025      IF EIBTRNID NOT = TRANS-ID                                   EL6522
01026          SUBTRACT +1 FROM QID-ITEM 
01027          MOVE QID-ITEM                     TO PI-TEMP-STOR-ITEMS
           END-IF

           GO TO 7599-EXIT
           .
       7500-EXIT.
           EXIT. 

01012  7501-LOOP.                                                       EL6522

01013      EXEC CICS READQ TS                                           EL6522
01014           INTO (TS-WORK-AREA)    
01015           QUEUE (QID)          
01016           LENGTH (TS-LENGTH)  
01017           ITEM (QID-ITEM)    
01018      END-EXEC

01019      MOVE TS-WORK-AREA                     TO TS-GROUP (TS-INDX)       
01020      SET TS-INDX UP BY 1
01021      ADD +1                                TO QID-ITEM

           .
       7501-EXIT.
           EXIT.
01023                                                                   EL6522
01024  7585-QID-ITEMERR.                                                EL6522

01025      IF EIBTRNID NOT = TRANS-ID                                   EL6522
01026          SUBTRACT 1 FROM QID-ITEM 
01027          MOVE QID-ITEM                     TO PI-TEMP-STOR-ITEMS
01028          GO TO 7599-EXIT
           END-IF
01029                                                                   EL6522
           .

01030  7590-TS-QIDERR.                                                  EL6522

01031      IF EIBTRNID = TRANS-ID                                       EL6522
01032          AND EIBAID = DFHCLEAR  
01033          GO TO 9410-RETURN
           END-IF

01034      IF EIBTRNID = TRANS-ID                                       EL6522
01035          MOVE ER-0033                      TO EMI-ERROR   
01036          PERFORM 9900-ERROR-FORMAT         THRU 9900-EXIT         
01037          GO TO 8100-SEND-INITIAL-MAP
           END-IF

           .
01039  7599-EXIT.                                                       EL6522
01040      EXIT.
01041                                                                   EL6522
01043  7600-UPDATE-TABLE-FROM-SCREEN.                                   EL6522
01044                                                                   EL6522
01045      IF SC-TEXTL (SC-INDX) NOT = ZEROS                            EL6522
01046          IF TB-INDX NOT > PI-TOTAL-LINES                          EL6522
01047              PERFORM 7700-MOVE-DATA        THRU 7700-EXIT  
01048              SET TB-INDX UP BY 1                                  EL6522
01049          ELSE                                                     EL6522
01050              IF PI-TOTAL-LINES = MAX-LINES                        EL6522
01051                  MOVE ER-0051              TO EMI-ERROR   
01052                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01053                  GO TO 8200-SEND-DATAONLY                         EL6522
01054              ELSE                                                 EL6522
01055                  PERFORM 7700-MOVE-DATA    THRU 7700-EXIT   
01056                  SET TB-INDX UP BY 1                              EL6522
01057                  ADD 1                     TO PI-TOTAL-LINES 
                   END-IF
               END-IF
01058      ELSE                                                         EL6522
01059          IF TB-INDX NOT > PI-TOTAL-LINES 
01060              SET TB-INDX UP BY 1
               END-IF
           END-IF

           .
01062  7699-EXIT.                                                       EL6522
01063      EXIT.
01064                                                                   EL6522
01065  7700-MOVE-DATA.                                                  EL6522

01066      MOVE '1'                              TO PI-UPDATE-SW
01067                                                                   EL6522
01068      IF SC-TEXTL (SC-INDX) NOT = ZEROS                            EL6522
01069          MOVE SC-TEXT (SC-INDX)            TO REC-TEXT (TB-INDX)       
           END-IF
01070                                                                   EL6522
01071      IF REC-LAST-MAINT-DT (TB-INDX) = LOW-VALUES OR SPACES        EL6522
01072          MOVE PI-PROCESSOR-ID  TO REC-LAST-MAINT-BY (TB-INDX)
01073                                   SC-MAINT-BY (SC-INDX)   
01074          MOVE EIBTIME          TO REC-LAST-MAINT-HHMMSS (TB-INDX)
01075          MOVE SAVE-BIN-DATE    TO REC-LAST-MAINT-DT (TB-INDX)
           END-IF
01076                                                                   EL6522
01077      PERFORM 7110-FORMAT-DATE

           .
01079  7700-EXIT.                                                       EL6522
01080      EXIT.   

01082  8100-SEND-INITIAL-MAP.                                           EL6522

01083      MOVE SAVE-DATE                        TO DATEO
01084      MOVE EIBTIME                          TO TIME-IN
01085      MOVE TIME-OUT                         TO TIMEO
022803     MOVE PI-COMPANY-ID                    TO CMPNYIDO
022803     MOVE PI-PROCESSOR-ID                  TO USERIDO
01086      MOVE EMI-MESSAGE-AREA (1)             TO ERRMSGBO
01087      MOVE PI-CR-CARRIER                    TO CARRO
01088      MOVE PI-CR-GROUPING                   TO GROUPO
           MOVE PI-CR-TYPE                       TO TYPEO
01090      MOVE PI-CR-FIN-RESP                   TO FINRESPO
01090      MOVE PI-CR-ACCOUNT                    TO ACCTO
01092      MOVE PI-SAVE-ACCT-NAME                TO NAMEO
01091      MOVE PI-TOTAL-LINES                   TO TOTO
01093      MOVE -1                               TO FUNCTL
01094                                                                   EL6522
01095      EXEC CICS SEND                                               EL6522
01096           MAP(MAP-NAME)                                           EL6522
01097           MAPSET(MAPSET-NAME)                                     EL6522
01098           FROM(EL6522AO)                                          EL6522
01099           ERASE                                                   EL6522
01100           CURSOR                                                  EL6522
01101      END-EXEC
01102                                                                   EL6522
01103      GO TO 9100-RETURN-TRAN

           .
01104                                                                   EL6522
01105  8200-SEND-DATAONLY.                                              EL6522

01106      MOVE EIBTIME                          TO TIME-IN    
01107      MOVE TIME-OUT                         TO TIMEO     
022803     MOVE PI-COMPANY-ID                    TO CMPNYIDO
022803     MOVE PI-PROCESSOR-ID                  TO USERIDO
01108      MOVE PI-TOTAL-LINES                   TO TOTO     
01109                                                                   EL6522
01110      IF NOT EMI-NO-ERRORS                                         EL6522
01111          MOVE EMI-MESSAGE-AREA (1)         TO ERRMSGBO 
01112      ELSE                                 
01113          MOVE -1                           TO FUNCTL
           END-IF
01114                                                                      CL**4
01115      EXEC CICS SEND                                               EL6522
01116           MAP(MAP-NAME)                                           EL6522
01117           MAPSET(MAPSET-NAME)                                     EL6522
01118           FROM(EL6522AO)                                          EL6522
01119           DATAONLY                                                EL6522
01120           CURSOR                                                  EL6522
01121      END-EXEC
01122                                                                   EL6522
01123      GO TO 9100-RETURN-TRAN

           . 
01124                                                                   EL6522
01125  8300-SEND-TEXT.                                                  EL6522

01126      EXEC CICS SEND TEXT                                          EL6522
01127           FROM(LOGOFF-TEXT)                                       EL6522
01128           ERASE                                                   EL6522
01129           FREEKB                                                  EL6522
01130           LENGTH(LOGOFF-LENGTH)                                   EL6522
01131      END-EXEC
01132                                                                   EL6522
01133      PERFORM 7250-DELETE-TEMP-STOR         THRU 7299-EXIT
01134                                                                   EL6522
01135      EXEC CICS RETURN                                             EL6522
01136      END-EXEC

           .
01137                                                                   EL6522
01138  8800-UNAUTHORIZED-ACCESS.                                        EL6522

01139      MOVE UNACCESS-MSG                     TO LOGOFF-MSG  
01140      GO TO 8300-SEND-TEXT

           .
01141                                               
01142  9000-RETURN-CICS.                                                EL6522

01143      IF PI-CHANGES-MADE                                           EL6522
01144          MOVE ER-0045                      TO EMI-ERROR  
01145          MOVE -1                           TO FUNCTL    
01146          MOVE SPACES                       TO PFENTERO 
01147          MOVE AL-UNNOF                     TO PFENTERA
01148          PERFORM 9900-ERROR-FORMAT         THRU 9900-EXIT 
01149          GO TO 8200-SEND-DATAONLY
           END-IF
01150                                                                      CL**4
01151      MOVE EIBAID                           TO PI-ENTRY-CD-1     
01152      MOVE XCTL-005                         TO PGM-NAME
01153      GO TO 9300-XCTL

           .
01154                                                                   EL6522
01155  9100-RETURN-TRAN.                                                EL6522

01156      MOVE SCRN-NUMBER                    TO PI-CURRENT-SCREEN-NO          
01157      MOVE EMI-ERROR-NUMBER (1)           TO PI-LAST-ERROR-NO    

01158      EXEC CICS RETURN                                             EL6522
01159           TRANSID(TRANS-ID)                                       EL6522
01160           COMMAREA(PROGRAM-INTERFACE-BLOCK)                       EL6522
01161           LENGTH(PI-COMM-LENGTH)                                     CL**5
01162      END-EXEC
01163                                                                   EL6522
           .

01165  9200-RETURN-MAIN-MENU.                                           EL6522

01166      IF PI-CHANGES-MADE                                           EL6522
01167          MOVE -1                         TO FUNCTL   
01168          MOVE SPACES                     TO PFENTERO     
01169          MOVE AL-UNNOF                   TO PFENTERA    
01170          MOVE ER-0045                    TO EMI-ERROR  
01171          PERFORM 9900-ERROR-FORMAT       THRU 9900-EXIT 
01172          GO TO 8200-SEND-DATAONLY
           END-IF
01173                                                                   EL6522
01174      MOVE XCTL-126                       TO PGM-NAME

           .
01175                                                                   EL6522
01176  9300-XCTL.                                                       EL6522

01177      PERFORM 7250-DELETE-TEMP-STOR       THRU 7299-EXIT
01178      EXEC CICS XCTL                                               EL6522
01179           PROGRAM  (PGM-NAME)                                     EL6522
01180           COMMAREA (PROGRAM-INTERFACE-BLOCK)                      EL6522
01181           LENGTH   (PI-COMM-LENGTH)   
01182      END-EXEC

           .
01183                                                                   EL6522
01184  9400-CLEAR.                                                      EL6522
01185                                        
01186      IF PI-CHANGES-MADE                                           EL6522
01187          MOVE ER-0045                    TO EMI-ERROR 
01188          PERFORM 9900-ERROR-FORMAT       THRU 9900-EXIT
01189          IF PI-CURRENT-LINE > ZERO     
01190              PERFORM 7500-READ-TS        THRU 7599-EXIT 
01191              SET TB-INDX                 TO PI-CURRENT-LINE     
01192              PERFORM 7100-FORMAT-SCREEN  THRU 7100-EXIT  
01193                      VARYING SC-INDX FROM 1 
                           BY 1
                           UNTIL SC-INDX GREATER NUM-LINES-PER-SCREEN   
01195          END-IF                                        
01197          GO TO 8100-SEND-INITIAL-MAP
           END-IF

           .
01198                                                                   EL6522
01199  9410-RETURN.                                                     EL6522

01200      MOVE PI-RETURN-TO-PROGRAM           TO PGM-NAME      
01201      GO TO 9300-XCTL

           .
01202                                                                   EL6522
01203  9500-PF12.                                                       EL6522

01204      IF PI-CHANGES-MADE                                           EL6522
01205          MOVE -1                         TO FUNCTL 
01206          MOVE SPACES                     TO PFENTERO      
01207          MOVE AL-UNNOF                   TO PFENTERA     
01208          MOVE ER-0045                    TO EMI-ERROR   
01209          PERFORM 9900-ERROR-FORMAT       THRU 9900-EXIT
01210          GO TO 8200-SEND-DATAONLY
           END-IF
01211                                         
01212      MOVE 'EL010'                        TO PGM-NAME  
01213      GO TO 9300-XCTL

           .
01214                                                                   EL6522
01216  9550-START-PRINT.                                                EL6522

01217      PERFORM 7200-PUT-TEMP-STOR          THRU 7249-EXIT
01218                                                                   EL6522
01219      MOVE SPACES                         TO ELCNTL-KEY
01220      MOVE PI-COMPANY-ID                  TO ELCNTL-COMPANY
01221      MOVE '1'                            TO ELCNTL-REC-TYPE
01222      MOVE ZEROS                          TO ELCNTL-SEQ-NO
01223                                                                   EL6522
01224      EXEC CICS HANDLE CONDITION                                   EL6522
01225           NOTFND(9410-RETURN) 
01226      END-EXEC
01227                                                                   EL6522
01228      EXEC CICS READ DATASET (ELCNTL-FILE-ID)
01229           SET (ADDRESS OF CONTROL-FILE)     
01230           RIDFLD (ELCNTL-KEY)  
01231      END-EXEC
01232                                                                   EL6522
01233      EXEC CICS HANDLE CONDITION   
01234           TERMIDERR (9560-BAD-TERMID) 
01235           TRANSIDERR (9570-BAD-TRANSID) 
01236      END-EXEC
01237                                                                   EL6522
01247      EXEC CICS START                  
01248           TRANSID (W-PRINT-TRANS)      
01249           TERMID (CF-FORMS-PRINTER-ID)  
01250           FROM (PROGRAM-INTERFACE-BLOCK) 
01251           LENGTH (PI-COMM-LENGTH)         
01252      END-EXEC
01253                                                                   EL6522
01254      MOVE -1                             TO LINE1L
01255      MOVE ER-3784                        TO EMI-ERROR 
01256      PERFORM 9900-ERROR-FORMAT           THRU 9900-EXIT
01257      GO TO 8200-SEND-DATAONLY

           .
01258                                                                   EL6522
01259  9560-BAD-TERMID.                                                 EL6522

01260      MOVE ER-0412                        TO EMI-ERROR
01261      PERFORM 9900-ERROR-FORMAT           THRU 9900-EXIT
01262      GO TO 8200-SEND-DATAONLY

           .
01263                                                                   EL6522
01264  9570-BAD-TRANSID.                                                EL6522

01265      MOVE ER-0413                        TO EMI-ERROR  
01266      PERFORM 9900-ERROR-FORMAT           THRU 9900-EXIT
01267      GO TO 8200-SEND-DATAONLY

           .
01268                                                                   EL6522
01270  9600-PGMID-ERROR.                                                EL6522

01271      EXEC CICS HANDLE CONDITION                                   EL6522
01272           PGMIDERR (8300-SEND-TEXT)  
01273      END-EXEC
01274                                                                   EL6522
01275      MOVE PGM-NAME                       TO PI-CALLING-PROGRAM
01276      MOVE SPACES                         TO PI-ENTRY-CD-1
01277      MOVE XCTL-005                       TO PGM-NAME
01278      MOVE PGM-NAME                       TO LOGOFF-PGM
01279      MOVE PGMIDERR-MSG                   TO LOGOFF-FILL
01280                                                                   EL6522
01281      GO TO 9300-XCTL

           .
01282                                                                   EL6522
01283  9700-LINK-DATE-CONVERT.                                          EL6522
01284                                                                   EL6522
01285      MOVE LINK-ELDATCV                   TO PGM-NAME
01286      EXEC CICS LINK                                               EL6522
01287           PROGRAM (PGM-NAME)       
01288           COMMAREA (DATE-CONVERSION-DATA)     
01289           LENGTH (DC-COMM-LENGTH)            
01290      END-EXEC

           .
01292  9700-EXIT.                                                       EL6522
01293      EXIT.                                                        EL6522
01294                                                                   EL6522
01295  9900-ERROR-FORMAT.                                               EL6522

01296      IF NOT EMI-ERRORS-COMPLETE                                   EL6522
01297          MOVE LINK-001                   TO PGM-NAME    
01298          EXEC CICS LINK                          
01299               PROGRAM (PGM-NAME)                 
01300               COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK) 
01301               LENGTH (EMI-COMM-LENGTH)                
01302          END-EXEC
           END-IF

           .
01303  9900-EXIT.
           EXIT.
01304                                                                   EL6522
01305  9990-ABEND.                                                      EL6522

01306      MOVE LINK-004                       TO PGM-NAME
01307      MOVE DFHEIBLK                       TO EMI-LINE1
01308                                                                   EL6522
01309      EXEC CICS LINK                                               EL6522
01310           PROGRAM (PGM-NAME)                
01311           COMMAREA (EMI-LINE1)              
01312           LENGTH (72)                    
01313      END-EXEC
01314                                                                   EL6522
01315      MOVE EMI-MESSAGE-AREA (1)           TO ERRMSGBO
01316      MOVE -1                             TO FUNCTL
01317                                                                   EL6522
01318      GO TO 8100-SEND-INITIAL-MAP
01319                                                                   EL6522
01320      GOBACK

           .            
01321                                                                   EL6522
01322  9995-SECURITY-VIOLATION.                                         EL6522
01323                                  COPY ELCSCTP.                    EL6522
01324                                                                   EL6522
01325  9995-EXIT.                                                       EL6522
01326      EXIT.                                                        EL6522
