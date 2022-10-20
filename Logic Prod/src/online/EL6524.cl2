       IDENTIFICATION DIVISION.
                                                                                
       PROGRAM-ID.                 EL6524.
      *                                                                         
      *AUTHOR.     CENTRAL STATES HEALTH AND LIFE.                              
      *            OMAHA, NEBRASKA.                                             
                                                                                
      *DATE-COMPILED.                                                           
      *SECURITY.   *****************************************************        
      *            *                                                   *        
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *        
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *        
      *            *   OF CSO.        IS EXPRESSLY PROHIBITED WITHOUT  *        
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *        
      *            *                                                   *        
      *            *****************************************************        
                                                                                
      *REMARKS.    TRANSACTION - EXDE - REPRESENTATIVE                        
      *                                 MAINTENANCE.
                                                                                
                                                                                
       ENVIRONMENT DIVISION.                                                    
           EJECT                                                                
       DATA DIVISION.                                                           
       WORKING-STORAGE SECTION.                                                 
       77  FILLER  PIC X(32)  VALUE '********************************'.         
       77  FILLER  PIC X(32)  VALUE '*    EL6524 WORKING STORAGE    *'.         
       77  FILLER  PIC X(32)  VALUE '******** VMOD=2.001 ************'.         
                                                                                
       77  NDX     PIC S9(5)  COMP-3 VALUE +1.
       77  WSUB1   PIC S9(5)  COMP-3 VALUE +0.
       77  WSUB2   PIC S9(5)  COMP-3 VALUE +0.
       77  WSUB3   PIC S9(5)  COMP-3 VALUE +0.
       77  WS-TOT-FEES                 PIC S9(3)V99 COMP-3 VALUE +0.
       77  WS-TOT-LFEES                PIC S9(3)V99 COMP-3 VALUE +0.
       77  ERAGTC-LENGTH               PIC S9(4) COMP VALUE +450.
       77  TABLE-SW                    PIC X.
           88  TABLE-OVERLAP                   VALUE 'Y'.
       77  WS-CHECK-DATE-SW            PIC X   VALUE SPACES.
           88  DATE-CHECK-OVER                 VALUE 'Y'.
       77  WS-EFF-SW                   PIC X   VALUE SPACES.
           88  EFF-DT-CHANGE                   VALUE 'Y'.
       77  WS-EXP-SW                   PIC X   VALUE SPACES.
           88  EXP-DT-CHANGE                   VALUE 'Y'.
       77  WS-SKIP-KEY                 PIC X(21) VALUE LOW-VALUES.
       01  WS-DATE-AREA.                                                        
           05  SAVE-DATE           PIC X(8)    VALUE SPACES.                    
           05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.                    
                                                                                
       01  WS-SAVE-ERAGTC              PIC X(450).
       01  WS-ERCOMP-KEY.
           05  WS-COMP-COMPANY-CD      PIC X.                                       
           05  WS-COMP-CARRIER         PIC X.                                       
           05  WS-COMP-GROUPING        PIC X(6).                                    
           05  WS-COMP-FINRESP         PIC X(10).
           05  WS-COMP-ACCOUNT         PIC X(10).
           05  WS-COMP-TYPE            PIC X.
           
       01  WS-DATE-TABLE.
           05  FILLER OCCURS 40.
               10  WS-TBL-EFF-DT       PIC XX.
               10  WS-TBL-EXP-DT       PIC XX.
       01  WS-CONTROL-PRIMARY.                                                  
           05  WS-COMPANY-CD       PIC X.                                       
           05  WS-CARRIER          PIC X.                                       
           05  WS-GROUP            PIC X(6).                                    
           05  WS-BANK             PIC X(10).
           05  WS-EXP-DT           PIC XX.
           05  WS-TYPE             PIC X.
                                                                                
           05  WS-OPEN-COUNT       PIC S9(4)    COMP-3 VALUE ZEROS.             
                                                                                
                                   COPY ELCATTR SUPPRESS.                       
                                                                                
           EJECT                                                                
                                                                                
       01  ERROR-MESSAGES.
           12  ER-0000                 PIC X(4)  VALUE '0000'.
           12  ER-0004                 PIC X(4)  VALUE '0004'.
           12  ER-0029                 PIC X(4)  VALUE '0029'.
           12  ER-0033                 PIC X(4)  VALUE '0033'.
           12  ER-0034                 PIC X(4)  VALUE '0034'.
           12  ER-0068                 PIC X(4)  VALUE '0068'.
           12  ER-0070                 PIC X(4)  VALUE '0070'.
           12  ER-0130                 PIC X(4)  VALUE '0130'.
           12  ER-0131                 PIC X(4)  VALUE '0131'.
           12  ER-0142                 PIC X(4)  VALUE '0142'.
           12  ER-0193                 PIC X(4)  VALUE '0193'.
           12  ER-0226                 PIC X(4)  VALUE '0226'.
           12  ER-0234                 PIC X(4)  VALUE '0234'.
           12  ER-0235                 PIC X(4)  VALUE '0235'.
           12  ER-0348                 PIC X(4)  VALUE '0348'.
           12  ER-0454                 PIC X(4)  VALUE '0454'.
           12  ER-1162                 PIC X(4)  VALUE '1162'.
           12  ER-1164                 PIC X(4)  VALUE '1164'.
           12  ER-2039                 PIC X(4)  VALUE '2039'.
           12  ER-2042                 PIC X(4)  VALUE '2042'.
           12  ER-2051                 PIC X(4)  VALUE '2051'.
           12  ER-2053                 PIC X(4)  VALUE '2053'.
           12  ER-2056                 PIC X(4)  VALUE '2056'.
           12  ER-2057                 PIC X(4)  VALUE '2057'.
           12  ER-2173                 PIC X(4)  VALUE '2173'.
           12  ER-2230                 PIC X(4)  VALUE '2230'.
           12  ER-2237                 PIC X(4)  VALUE '2237'.
           12  ER-2238                 PIC X(4)  VALUE '2238'.
           12  ER-2339                 PIC X(4)  VALUE '2339'.
           12  ER-2370                 PIC X(4)  VALUE '2370'.
           12  ER-2947                 PIC X(4)  VALUE '2947'.
           12  ER-2717                 PIC X(4)  VALUE '2717'.
           12  ER-3112                 PIC X(4)  VALUE '3112'.
           12  ER-5004                 PIC X(4)  VALUE '5004'.
           12  ER-5005                 PIC X(4)  VALUE '5005'.
           12  ER-6508                 PIC X(4)  VALUE '6508'.
 
                                 COPY ELCSCTM.
                                 COPY ELCSCRTY.
 
       01  STANDARD-AREAS.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.
           12  WS-CHANGE-SW            PIC X  VALUE ' '.
               88  CHANGES-MADE               VALUE 'Y'.
               88  NO-CHANGES-MADE            VALUE 'N'.
           12  WS-LINE-CHANGE-SW       PIC X  VALUE ' '.
               88  LINE-CHANGES               VALUE 'Y'.
               88  NO-LINE-CHANGES            VALUE 'N'.
           12  GETMAIN-SPACE           PIC X     VALUE SPACE.
           12  DEEDIT-FIELD        PIC  X(08).
           12  DEEDIT-FIELD-V0  REDEFINES
               DEEDIT-FIELD        PIC 9(08).
           12  WS-DEEDIT-FIELD         PIC X(10) VALUE SPACES.
           12  WS-DT-DEEDIT-FIELD REDEFINES WS-DEEDIT-FIELD
                                            PIC X(10).
           12  WS-DEEDIT-FIELD-DATE REDEFINES WS-DT-DEEDIT-FIELD.
               16  FILLER                   PIC X(4).
               16  WS-DEEDIT-FIELD-DATE-OUT PIC X(6).
           12  SC-ITEM-CL-CR       PIC S9(4)         VALUE +1   COMP.   
           12  WS-ACCESS.                                                       
               16  FILLER          PIC  X(3)         VALUE SPACES.              
               16  WS-ACARRIER     PIC  X.                                      
           12  ELCNTL-KEY.                                                      
               16  CNTL-COMP-ID    PIC  X(3)         VALUE SPACES.              
               16  CNTL-REC-TYPE   PIC  X            VALUE SPACES.              
               16  CNTL-ACCESS     PIC  X(4)         VALUE SPACES.              
               16  CNTL-SEQ-NO     PIC S9(4)         VALUE +0   COMP.           
           12  QID.                                                             
               16  QID-TERM        PIC X(4).                                    
               16  FILLER          PIC X(4)    VALUE '652E'.                    
           12  RETURNED-FROM       PIC X(8)    VALUE SPACES.                    
           12  QID-MAP-LENGTH      PIC S9(4)   VALUE +1376   COMP.              
           12  MAP-NAME                PIC X(8)    VALUE 'EL652E'.              
           12  MAPSET-NAME             PIC X(8)    VALUE 'EL6524S'.             
           12  TRANS-ID                PIC X(4)    VALUE 'EXDE'.                
           12  EL652-TRANS-ID          PIC X(4)    VALUE 'EXD4'.
           12  EL650-TRANS-ID          PIC X(4)    VALUE 'EXC4'.                
           12  PGM-NAME                PIC X(8)    VALUE SPACES.                
           12  THIS-PGM                PIC X(8)    VALUE 'EL6524'.              
           12  XCTL-650                PIC X(8)    VALUE 'EL650'.               
           12  AGTC-FILE-ID            PIC X(8)    VALUE 'ERAGTC'.              
           12  CNTL-FILE-ID            PIC X(8)    VALUE 'ELCNTL'.              
           12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.             
           12  LINK-001                PIC X(8)    VALUE 'EL001'.               
           12  LINK-004                PIC X(8)    VALUE 'EL004'.               
           12  XCTL-005                PIC X(8)    VALUE 'EL005'.               
           12  XCTL-010                PIC X(8)    VALUE 'EL010'.               
           12  XCTL-626                PIC X(8)    VALUE 'EL626'.               
           12  TIME-IN                 PIC S9(7).                               
           12  TIME-OUT-R   REDEFINES TIME-IN.                                  
               16  FILLER              PIC X.                                   
               16  TIME-OUT            PIC 99V99.                               
               16  FILLER              PIC X(2).                                
           12  BROWSE-STARTED-SW       PIC X       VALUE ' '.                   
               88  BROWSE-STARTED      VALUE 'Y'.                               
                                                                                
       01  WS-RECORD-LENGTH COMP       SYNCHRONIZED.                            
           12  AGTC-REC-LENGTH         PIC S9(4)    VALUE +0.                   
                                                                                
                                       COPY ELCDATE.
                                       COPY ELCEMIB SUPPRESS.
                                                                                
                         COPY ELCLOGOF SUPPRESS.                                
                                                                                
                                       COPY ELCINTF.
           12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.                        
               16  PI-ERAGTC-KEY.                                               
                   20  PI-AGTC-COMP-CD     PIC X.                               
                   20  PI-AGTC-CARRIER     PIC X.                               
                   20  PI-AGTC-GROUPING    PIC X(6).                            
                   20  PI-AGTC-BANK        PIC X(10).
                   20  PI-AGTC-EXP-DT      PIC XX.
                   20  PI-AGTC-TYPE        PIC X.
               16  PI-TOP-NDX              PIC S9(5)  COMP-3.
               16  PI-NDX                  PIC S9(5)  COMP-3.                   
               16  PI-EOF-SW               PIC X.                               
                   88  PI-FILE-EOF             VALUE 'Y'.                       
               16  PI-CHECK-MAINT-TYPE     PIC  X.                              
                   88  VALID-MAINT-TYPE            VALUE 'S' 'A'                
                                                         'C' 'D'.               
                   88  ADD-FUNCTION                VALUE 'A'.                   
                   88  SHOW-FUNCTION               VALUE 'S'.                   
                   88  DELETE-FUNCTION             VALUE 'D'.                   
                   88  CHANGE-FUNCTION             VALUE 'C'.                   
               16  PI-ERC-KEY.
                   20  PI-ERC-COMPANY-CD   PIC  X.                              
                   20  PI-ERC-CARRIER      PIC  X.                              
                   20  PI-ERC-GROUP        PIC  X(6).                           
                   20  PI-ERC-BANK         PIC  X(10).
                   20  PI-ERC-EXP-DT       PIC  XX.
                   20  PI-ERC-TYPE         PIC  X.
               16  PI-SAVE-ERAGTC-KEY      PIC  X(21).
               16  FILLER                  PIC X(617).                          
           EJECT                                                                
                                   COPY ELCAID SUPPRESS.                        
       01  FILLER    REDEFINES DFHAID.                                          
           12  FILLER              PIC X(8).                                    
           12  PF-VALUES           PIC X       OCCURS 24.                       

       01  WS-BIN-EFF-DT               PIC XX VALUE LOW-VALUES.
       01  WS-BIN-EXP-DT               PIC XX VALUE LOW-VALUES.
                                                                                
                                   COPY EL6524S.                                

00405  01  MAP-R REDEFINES EL652EI.
100703     12  FILLER                  PIC X(105).
00407      12  MAP-AGENT-AREA.
00408          16  AGENT-AREA OCCURS 10 TIMES INDEXED BY M-INDEX.
                   20  REPRL           PIC S9(4) COMP.
                   20  REPRA           PIC X.
                   20  REPR            PIC X(10).
00412              20  ATYPEL          PIC S9(4) COMP.
00413              20  ATYPEA          PIC X.
00414              20  ATYPE           PIC X.
00415              20  FEESL           PIC S9(4) COMP.
00416              20  FEESA           PIC X.
00417              20  FEES-COMM-T     PIC X(8).
00418              20  FEES-COMM-N     REDEFINES FEES-COMM-T.
00419                  24  FEES-COMM-R PIC 9(6)V99.
00421              20  FEES-COMM-O     REDEFINES FEES-COMM-T
00422                                  PIC ZZZZ9.99.
00439              20  RECALL          PIC S9(4) COMP.
00440              20  RECALA          PIC X.
00441              20  RECAL           PIC X.
092205             20  LFEESL          PIC S9(4) COMP.
092205             20  LFEESA          PIC X.
092205             20  LFEES-COMM-T    PIC X(8).
092205             20  LFEES-COMM-N    REDEFINES LFEES-COMM-T.
092205                 24  LFEES-COMM-R PIC 9(6)V99.
092205             20  LFEES-COMM-O    REDEFINES LFEES-COMM-T
092205                                 PIC ZZZZ9.99.
092205             20  LRCALL          PIC S9(4) COMP.
092205             20  LRCALA          PIC X.
092205             20  LRCAL           PIC X.

       LINKAGE SECTION.                                                         
       01  DFHCOMMAREA                 PIC X(1024).
 
                                       COPY ELCCNTL.
                                       COPY ERCAGTC.
                                       COPY ERCCOMP.
 
       PROCEDURE DIVISION.                                                      
           MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                         
                                                                                
           MOVE EIBDATE               TO DC-JULIAN-YYDDD.                       
           MOVE '5'                   TO DC-OPTION-CODE.                        
           PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                            
           MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                            
           MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                        
                                                                                
           MOVE EIBTRMID              TO QID-TERM.                              
                                                                                
           EXEC CICS HANDLE CONDITION                                           
               ERROR     (9990-ABEND)                                           
               MAPFAIL   (8100-SEND-INITIAL-MAP)                                
           END-EXEC.
                                                                                
           IF PI-CALLING-PROGRAM NOT = THIS-PGM                                 
               IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                           
      *  THIS IS THE FIRST TIME THRU
                   MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6              
                   MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5              
                   MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4              
                   MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3              
                   MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2              
                   MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1              
                   MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM            
                   MOVE THIS-PGM             TO PI-CALLING-PROGRAM              
                   PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT
               ELSE                                                             
      * THIS IS WHEN I COME BACK FROM WHERE I XCTL'D TO
                   MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM              
                   MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM            
                   MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1              
                   MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2              
                   MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3              
                   MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4              
                   MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5              
                   MOVE SPACES               TO PI-SAVED-PROGRAM-6
               END-IF
      * ALL SUBSEQUENT TIMES
           END-IF
 
           IF EIBTRNID = TRANS-ID                                               
               IF EIBAID = DFHCLEAR                                             
                   GO TO 9400-CLEAR                                             
               ELSE                                                             
                   GO TO 0200-RECEIVE-MAP.                                      
                                                                                
           IF EIBTRNID  = EL652-TRANS-ID
      * THIS IS WHEN I COME FROM COMP MAINT
               MOVE DFHENTER       TO EIBAID
               MOVE PI-CR-CARRIER  TO CARRIERI
               MOVE PI-CR-GROUPING TO GROUPI
               MOVE PI-CR-FIN-RESP TO BANKI
               MOVE PI-CR-TYPE     TO TYPEI
               MOVE 'S'            TO MAINTYPI
               MOVE 1              TO CARRIERL
               MOVE 6              TO GROUPL
               MOVE 10             TO BANKL
               MOVE 1              TO TYPEL
               MOVE 1              TO MAINTYPL
               MOVE AL-UANON       TO CARRIERA GROUPA TYPEA
                                      BANKA MAINTYPA
               GO TO 4000-EDIT-MAINT
           END-IF
                                                                                
           MOVE LOW-VALUES       TO PI-ERAGTC-KEY    EL652EI.                   
           MOVE PI-COMPANY-CD    TO PI-AGTC-COMP-CD  WS-COMPANY-CD.             
                                                                                
           IF EIBTRNID  = EL650-TRANS-ID                                        
      * THIS IS WHEN I CAME FROM ACCOUNT MAINT
               GO TO 0600-RECOVER-TEMP-STORAGE.                                 
                                                                                
           GO TO 8100-SEND-INITIAL-MAP.                                         
                                                                                
           EJECT                                                                
                                                                                
       0200-RECEIVE-MAP.
 
           MOVE LOW-VALUES             TO EL652EI
 
           EXEC CICS RECEIVE
               MAP (MAP-NAME)
               MAPSET (MAPSET-NAME)
               INTO (EL652EI)
           END-EXEC
 
           IF PFENTERL = 0
              GO TO 0300-CHECK-PFKEYS
           END-IF
 
           IF EIBAID NOT = DFHENTER                                             
              MOVE ER-0004             TO EMI-ERROR
              GO TO 0320-INPUT-ERROR
           END-IF
 
           IF (PFENTERI NUMERIC) AND (PFENTERI > 0 AND < 25)                    
              MOVE PF-VALUES (PFENTERI)
                                       TO EIBAID
           ELSE                                                                 
              MOVE ER-0029             TO EMI-ERROR
              GO TO 0320-INPUT-ERROR
           END-IF
           .                                                                    
       0300-CHECK-PFKEYS.                                                       
 
           IF EIBAID = DFHPF23                                                  
               GO TO 8810-PF23.                                                 
                                                                                
           IF EIBAID = DFHPF24                                                  
               GO TO 9200-RETURN-MAIN-MENU.                                     
 
           IF EIBAID = DFHPF12                                                  
               GO TO 9500-PF12.                                                 
                                                                                
           IF EIBAID = DFHPF1 OR DFHPF2
              GO TO 5000-BROWSE-FILE
      *       GO TO 4000-EDIT-MAINT
           END-IF
                                                                                
           IF EIBAID = DFHENTER                                                 
              GO TO 4000-EDIT-MAINT
           END-IF
                                                                                
           MOVE ER-0029                TO EMI-ERROR
           .
       0320-INPUT-ERROR.                                                        
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                            
           MOVE -1                     TO CARRIERL.                             
           GO TO 8200-SEND-DATAONLY.                                            
           EJECT                                                                
       0500-CREATE-TEMP-STORAGE.                                                
                                                                                
           EXEC CICS WRITEQ TS                                                  
               QUEUE   (QID)                                                    
               FROM    (EL652EI)                                                
               LENGTH  (QID-MAP-LENGTH)                                         
               END-EXEC.                                                        
                                                                                
       0599-EXIT.                                                               
            EXIT.                                                               
                                                                                
       0600-RECOVER-TEMP-STORAGE.                                               
                                                                                
           EXEC CICS HANDLE CONDITION                                           
               NOTFND  (1500-AGTC-NOT-FOUND)                                    
               QIDERR  (0690-QIDERR)                                            
               END-EXEC.                                                        
                                                                                
           EXEC CICS READQ TS                                                   
               QUEUE    (QID)                                                   
               INTO     (EL652EI)                                               
               LENGTH   (QID-MAP-LENGTH)                                        
               END-EXEC.                                                        
                                                                                
           EXEC CICS DELETEQ TS                                                 
               QUEUE   (QID)                                                    
           END-EXEC
                                                                                
           IF CARRIERL NOT = 0                                                  
               MOVE AL-UANON TO CARRIERA.                                       
                                                                                
           IF GROUPL NOT = 0                                                    
               MOVE AL-UANON TO GROUPA.                                         
                                                                                
           IF BANKL NOT = 0
               MOVE AL-UANON TO BANKA
           END-IF

           IF TYPEL NOT = 0
               MOVE AL-UANON           TO TYPEA
           END-IF

           MOVE PI-COMPANY-CD          TO WS-COMPANY-CD
           MOVE CARRIERO               TO WS-CARRIER
           MOVE GROUPO                 TO WS-GROUP
           MOVE BANKO                  TO WS-BANK
           MOVE TYPEO                  TO WS-TYPE
                                                                                
           GO TO 1050-READ-AGTC.                                                
 
       0690-QIDERR.
 
           MOVE ER-0033                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           GO TO 8100-SEND-INITIAL-MAP
 
           .
       4000-EDIT-MAINT.
 
           IF MAINTYPL > ZERO
              MOVE MAINTYPI            TO PI-CHECK-MAINT-TYPE
              IF VALID-MAINT-TYPE
                 MOVE AL-UANON         TO MAINTYPA
              ELSE
                 MOVE -1               TO MAINTYPL
                 MOVE AL-UABON         TO MAINTYPA
                 MOVE ER-2039          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU  9900-EXIT
              END-IF
           ELSE
              MOVE -1                  TO MAINTYPL
              MOVE AL-UABON            TO MAINTYPA
              MOVE ER-2039             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF
 
           MOVE PI-COMPANY-CD          TO PI-ERC-COMPANY-CD
           IF (NOT MODIFY-CAP)
              AND (NOT SHOW-FUNCTION)
              MOVE 'UPDATE'            TO SM-READ
              PERFORM 9995-SECURITY-VIOLATION
                                       THRU 9995-EXIT
              MOVE ER-0070             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              GO TO 8100-SEND-INITIAL-MAP
           END-IF
 
           IF CARRIERL > ZERO
              IF PI-CARRIER-SECURITY > SPACES
                 IF CARRIERI = PI-CARRIER-SECURITY
                    CONTINUE
                 ELSE
                    MOVE -1            TO CARRIERL
                    MOVE ER-2370       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                    MOVE AL-UABON      TO CARRIERA
                    GO TO 8200-SEND-DATAONLY
                 END-IF
              END-IF
           END-IF
                                                                                
           IF CARRIERL > ZERO                                                   
               IF ADD-FUNCTION                                                  
                   IF PI-ZERO-CARRIER                                           
                     OR PI-ZERO-CAR-GROUP                                       
                       MOVE ZEROS      TO  PI-ERC-CARRIER                       
                                           CARRIERI                             
                       MOVE AL-UANON   TO  CARRIERA                             
                   ELSE                                                         
                       MOVE CARRIERI   TO  WS-ACARRIER
                                           PI-ERC-CARRIER
                       MOVE '6'        TO  CNTL-REC-TYPE                        
                       PERFORM 7400-READ-CONTROL-FILE  THRU  7499-EXIT          
               ELSE                                                             
                   IF PI-ZERO-CARRIER                                           
                     OR PI-ZERO-CAR-GROUP                                       
                       MOVE ZEROS      TO  PI-ERC-CARRIER                       
                                           CARRIERI                             
                       MOVE AL-UANON   TO  CARRIERA                             
                   ELSE                                                         
                       MOVE AL-UANON   TO  CARRIERA                             
                       MOVE CARRIERI   TO  PI-ERC-CARRIER                       
           ELSE                                                                 
               IF ADD-FUNCTION                                                  
                   IF PI-ZERO-CARRIER                                           
                     OR PI-ZERO-CAR-GROUP                                       
                       MOVE ZEROS      TO  PI-ERC-CARRIER                       
                                           CARRIERI                             
                       MOVE AL-UANON   TO  CARRIERA                             
                   ELSE                                                         
                       MOVE -1         TO  CARRIERL                             
                       MOVE AL-UABON   TO  CARRIERA                             
                       MOVE ER-0193    TO  EMI-ERROR                            
                       PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               
               ELSE                                                             
                   MOVE -1             TO  CARRIERL                             
                   MOVE AL-UABON       TO  CARRIERA                             
                   MOVE ER-0193        TO  EMI-ERROR                            
                   PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  
                                                                                
           IF GROUPL > ZERO                                                     
               IF PI-ZERO-GROUPING                                              
                 OR PI-ZERO-CAR-GROUP                                           
                   MOVE ZEROS          TO  PI-ERC-GROUP                         
                                           GROUPI                               
                   MOVE AL-UANON       TO  GROUPA                               
               ELSE                                                             
                   MOVE AL-UANON       TO  GROUPA                               
                   MOVE GROUPI         TO  PI-ERC-GROUP                         
           ELSE                                                                 
               IF ADD-FUNCTION                                                  
                   IF PI-ZERO-GROUPING                                          
                     OR PI-ZERO-CAR-GROUP                                       
                       MOVE ZEROS      TO  PI-ERC-GROUP                         
                                           GROUPI                               
                       MOVE AL-UANON   TO  GROUPA                               
                   ELSE                                                         
                       MOVE LOW-VALUES  TO  PI-ERC-GROUP                        
               ELSE                                                             
                   MOVE LOW-VALUES     TO  PI-ERC-GROUP.                        
                                                                                
           IF BANKL > ZERO
               MOVE AL-UANON           TO BANKA
               MOVE BANKI              TO PI-ERC-BANK
           ELSE                                                                 
               MOVE LOW-VALUES         TO PI-ERC-BANK
           END-IF

           IF TYPEL > ZERO
               MOVE AL-UANON           TO TYPEA
               MOVE TYPEI              TO PI-ERC-TYPE
           ELSE                                                                 
               MOVE LOW-VALUES         TO PI-ERC-TYPE
           END-IF

           IF (EFFDTL NOT = ZEROS)
              AND (EFFDTI NOT = SPACES)
              MOVE EFFDTI                TO DEEDIT-FIELD
              PERFORM 8600-DEEDIT
              MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY
              MOVE '4'                    TO DC-OPTION-CODE
              PERFORM 9700-DATE-CONVERSION
              IF DATE-CONVERSION-ERROR
                 MOVE ER-0348             TO EMI-ERROR
                 MOVE -1                  TO EFFDTL
                 MOVE AL-UABON            TO EFFDTA
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              ELSE
                 MOVE DC-BIN-DATE-1    TO WS-BIN-EFF-DT
                 MOVE DC-GREG-DATE-1-EDIT TO EFFDTI
                 MOVE AL-UANON            TO EFFDTA
                 SET CHANGES-MADE         TO TRUE
                 IF CHANGE-FUNCTION
                    SET EFF-DT-CHANGE     TO TRUE
                 END-IF
              END-IF
           ELSE
              IF ADD-FUNCTION
                 MOVE ER-0348             TO EMI-ERROR
                 MOVE -1                  TO EFFDTL
                 MOVE AL-UABON            TO EFFDTA
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              END-IF
           END-IF

           IF (EXPDTL NOT = ZEROS)
              AND (EXPDTI NOT = SPACES)
              MOVE EXPDTI              TO DEEDIT-FIELD
              PERFORM 8600-DEEDIT
              MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
              MOVE '4'                 TO DC-OPTION-CODE
              IF DC-GREG-DATE-1-MDY = 999999
                 MOVE HIGH-VALUES      TO WS-BIN-EXP-DT
                                          PI-ERC-EXP-DT
                 MOVE '99/99/99'       TO EXPDTI
                 MOVE AL-UANON         TO EXPDTA
              ELSE
                 PERFORM 9700-DATE-CONVERSION
                 IF DATE-CONVERSION-ERROR
                    MOVE ER-0454       TO EMI-ERROR
                    MOVE -1            TO EXPDTL
                    MOVE AL-UABON      TO EXPDTA
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 ELSE
                    MOVE DC-BIN-DATE-1 TO WS-BIN-EXP-DT
                                          PI-ERC-EXP-DT
                    MOVE DC-GREG-DATE-1-EDIT
                                       TO EXPDTI
                    MOVE AL-UANON         TO EXPDTA
                 END-IF
              END-IF
           ELSE
              IF ADD-FUNCTION
                 MOVE ER-0454             TO EMI-ERROR
                 MOVE -1                  TO EFFDTL
                 MOVE AL-UABON            TO EFFDTA
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              END-IF
           END-IF

           IF NOT MODIFY-CAP                                                    
               IF SHOW-FUNCTION                                                 
                   GO TO 1000-EDIT-INPUT
               ELSE                                                             
                   MOVE 'UPDATE'       TO SM-READ                               
                   PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT               
                   MOVE ER-0070        TO  EMI-ERROR                            
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     
                   GO TO 8100-SEND-INITIAL-MAP.                                 
                                                                                
           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              IF EIBTRNID NOT = TRANS-ID
                 GO TO 8100-SEND-INITIAL-MAP
              ELSE
                 GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF 
                                                                                
           IF CHANGE-FUNCTION                                                   
              GO TO 4400-CHANGE
           END-IF
                                                                                
           IF DELETE-FUNCTION                                                   
              GO TO 4600-DELETE
           END-IF
                                                                                
           IF SHOW-FUNCTION                                                     
              GO TO 1000-EDIT-INPUT
           END-IF
                                                                                
           IF EMI-NO-ERRORS                                                     
              CONTINUE
           ELSE                                                                 
              GO TO 8200-SEND-DATAONLY
           END-IF
                                                                                
           IF ADD-FUNCTION                                                      
              GO TO 4200-ADD
           END-IF
                                                                                
           MOVE -1                     TO MAINTYPL
           MOVE ER-2056                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
                                                                                
           GO TO 8200-SEND-DATAONLY
           .
                                                                                
       4000-EXIT.                                                               
           EXIT.                                                                
       EJECT                                                                    
       1000-EDIT-INPUT.                                                         
 
           IF CARRIERL NOT = ZEROS
              MOVE CARRIERI            TO WS-CARRIER
           ELSE
              MOVE ER-0234             TO EMI-ERROR
              MOVE -1                  TO CARRIERL
              MOVE AL-UABON            TO CARRIERA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF
 
           IF GROUPL NOT = ZEROS
              MOVE GROUPI              TO WS-GROUP
           ELSE
              MOVE ER-0235             TO EMI-ERROR
              MOVE -1                  TO GROUPL
              MOVE AL-UABON            TO GROUPA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF
 
           IF BANKL  NOT = ZEROS
              MOVE BANKI               TO WS-BANK
           ELSE
              MOVE ER-6508             TO EMI-ERROR
              MOVE -1                  TO BANKL
              MOVE AL-UABON            TO BANKA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF

           IF TYPEL     NOT = ZEROS
              MOVE TYPEI               TO WS-TYPE
           ELSE
              MOVE ER-2042             TO EMI-ERROR
              MOVE -1                  TO TYPEL
              MOVE AL-UABON            TO TYPEA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF

           IF EXPDTL    NOT = ZEROS
              MOVE WS-BIN-EXP-DT       TO WS-EXP-DT
           ELSE
              MOVE LOW-VALUES          TO WS-EXP-DT
           END-IF

           IF NOT EMI-NO-ERRORS
              GO TO 8200-SEND-DATAONLY
           END-IF
 
           MOVE PI-COMPANY-CD          TO WS-COMPANY-CD

           .
       1050-READ-AGTC.
 
           EXEC CICS HANDLE CONDITION
                NOTFND (1500-AGTC-NOT-FOUND)
           END-EXEC

           EXEC CICS STARTBR
               DATASET (AGTC-FILE-ID)
               RIDFLD (WS-CONTROL-PRIMARY)
           END-EXEC
 
           MOVE 'Y'                    TO BROWSE-STARTED-SW
           MOVE +1                     TO PI-NDX
 
           .
       1060-READ-LOOP.                                                          
 
           EXEC CICS READNEXT
               DATASET (AGTC-FILE-ID)
               SET     (ADDRESS OF AGENT-COMMISSIONS)
               RIDFLD  (WS-CONTROL-PRIMARY)
           END-EXEC
 
           IF (AG-COMPANY-CD NOT = PI-COMPANY-CD)
              OR (WS-CARRIER NOT = PI-ERC-CARRIER)
              OR (WS-GROUP   NOT = PI-ERC-GROUP)
              OR (WS-BANK    NOT = PI-ERC-BANK)
              OR (WS-TYPE    NOT = PI-ERC-TYPE)
              GO TO 1500-AGTC-NOT-FOUND
           END-IF

           MOVE AG-LAST-MAINT-USER     TO PI-UPDATE-BY
 
           IF AG-LAST-MAINT-HHMMSS NUMERIC
              MOVE AG-LAST-MAINT-HHMMSS
                                       TO PI-UPDATE-HHMMSS
           ELSE
              MOVE ZEROS               TO PI-UPDATE-HHMMSS
           END-IF
           MOVE AG-CONTROL-PRIMARY     TO PI-SAVE-ERAGTC-KEY
           MOVE +1                     TO PI-NDX
                                          PI-TOP-NDX
           MOVE ER-0000                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT

           GO TO 5200-FORMAT-SCREEN

           .
       1500-AGTC-NOT-FOUND.                                                     
 
           MOVE ER-0142                TO EMI-ERROR
           MOVE -1                     TO CARRIERL
           MOVE AL-UABON               TO CARRIERA
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           IF EIBTRNID  = EL652-TRANS-ID
              GO TO 8100-SEND-INITIAL-MAP
           ELSE
              GO TO 8200-SEND-DATAONLY
           END-IF
           .
                                                                                
       3000-BLD-LINE.                                                           
 
           PERFORM VARYING PI-NDX FROM +1 BY +1 UNTIL
              (PI-NDX > +10)
              IF AG-AGT (PI-NDX) NOT = SPACES AND ZEROS
                 MOVE AG-AGT (PI-NDX)  TO REPR (PI-NDX)
                 MOVE AG-COM-TYP (PI-NDX)
                                       TO ATYPE (PI-NDX)
                 IF AG-SPP-FEES (PI-NDX) NOT = ZEROS
                    MOVE AG-SPP-FEES (PI-NDX)
                                       TO FEES-COMM-O (PI-NDX)
                 END-IF
                 MOVE AG-RECALC-LV-INDIC (PI-NDX)
                                       TO RECAL (PI-NDX)
                 IF AG-SPP-LFEES (PI-NDX) NOT NUMERIC
                    MOVE ZEROS         TO AG-SPP-LFEES (PI-NDX)
                 END-IF
                 IF AG-SPP-LFEES (PI-NDX) NOT = ZEROS
                    MOVE AG-SPP-LFEES (PI-NDX)
                                       TO LFEES-COMM-O (PI-NDX)
                 END-IF
                 MOVE AG-LRCALC-LV-INDIC (PI-NDX)
                                       TO LRCAL (PI-NDX)
              END-IF
           END-PERFORM
                                                                                
           .
       3000-XIT.                                                                
           EXIT.                                                                

       3100-GET-BANK-DATES.

           EXEC CICS STARTBR
               DATASET (AGTC-FILE-ID)
               RIDFLD  (WS-CONTROL-PRIMARY)
               RESP    (WS-RESPONSE)
           END-EXEC
 
           IF NOT RESP-NORMAL
              GO TO 3100-EXIT
           END-IF

           MOVE 'Y'                    TO BROWSE-STARTED-SW
           MOVE +1                     TO PI-NDX
 
           .
       3100-READ-LOOP.                                                          
 
           EXEC CICS READNEXT
               DATASET (AGTC-FILE-ID)
               SET     (ADDRESS OF AGENT-COMMISSIONS)
               RIDFLD  (WS-CONTROL-PRIMARY)
               RESP    (WS-RESPONSE)
           END-EXEC
 
           IF RESP-NORMAL
              AND (AG-COMPANY-CD = PI-COMPANY-CD)
              AND (WS-CARRIER = PI-ERC-CARRIER)
              AND (WS-GROUP   = PI-ERC-GROUP)
              AND (WS-BANK    = PI-ERC-BANK)
              AND (WS-TYPE    = PI-ERC-TYPE)
              MOVE AG-EFF-DT           TO WS-TBL-EFF-DT (PI-NDX)
              MOVE AG-EXP-DT           TO WS-TBL-EXP-DT (PI-NDX)
              ADD +1                   TO PI-NDX
              GO TO 3100-READ-LOOP
           END-IF

           .
       3100-EXIT.
           EXIT.

       4200-ADD.
 
           IF WS-BIN-EXP-DT NOT > WS-BIN-EFF-DT
              MOVE ER-2053             TO EMI-ERROR
              MOVE -1                  TO EFFDTL
              MOVE AL-UABON            TO EFFDTA
                                          EXPDTA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF

           IF EMI-NO-ERRORS                                                     
              CONTINUE
           ELSE                                                                 
              GO TO 8200-SEND-DATAONLY
           END-IF

           EXEC CICS HANDLE CONDITION                                           
               NOTOPEN  (9990-ABEND)                                            
               NOTFND   (4250-CONT)                                             
           END-EXEC.                                                            
                                                                                
           MOVE PI-ERC-KEY             TO WS-CONTROL-PRIMARY
           PERFORM 7050-READ-ERAGTC    THRU 7050-EXIT
                                                                                
           MOVE ER-2057                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
                                                                                
           MOVE LOW-VALUES             TO PI-SAVE-ERAGTC-KEY
                                                                                
           MOVE -1                     TO  MAINTYPL
                                                                                
           GO TO 8200-SEND-DATAONLY
           .
       4250-CONT.                                                               
 
           MOVE LOW-VALUES             TO WS-DATE-TABLE
           MOVE ' '                    TO TABLE-SW
           
           MOVE WS-CONTROL-PRIMARY     TO PI-SAVE-ERAGTC-KEY
           MOVE LOW-VALUES             TO PI-ERC-EXP-DT
           MOVE PI-ERC-KEY             TO WS-CONTROL-PRIMARY

           PERFORM 3100-GET-BANK-DATES THRU 3100-EXIT
           
           PERFORM VARYING PI-NDX FROM +1 BY +1 UNTIL
              (WS-TBL-EXP-DT (PI-NDX) = LOW-VALUES)
              OR (PI-NDX > +40)
              IF ((WS-BIN-EXP-DT > WS-TBL-EFF-DT (PI-NDX))
                 AND (WS-BIN-EXP-DT NOT > WS-TBL-EXP-DT (PI-NDX)))
                           OR
                 ((WS-BIN-EFF-DT > WS-TBL-EFF-DT (PI-NDX))
                 AND (WS-BIN-EFF-DT < WS-TBL-EXP-DT (PI-NDX)))
                 SET TABLE-OVERLAP        TO TRUE
              END-IF
           END-PERFORM

           IF BROWSE-STARTED
              EXEC CICS ENDBR
                   DATASET  (AGTC-FILE-ID)
              END-EXEC
              MOVE ' '                 TO BROWSE-STARTED-SW
           END-IF

           IF TABLE-OVERLAP
              MOVE ER-2051             TO EMI-ERROR
              MOVE -1                  TO EFFDTL
              MOVE AL-UABON            TO EFFDTA
                                          EXPDTA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF

           MOVE PI-SAVE-ERAGTC-KEY     TO WS-CONTROL-PRIMARY
                                          PI-ERC-KEY
           EXEC CICS GETMAIN                                                    
               SET      (ADDRESS OF AGENT-COMMISSIONS)
               INITIMG  (GETMAIN-SPACE)                                         
               LENGTH   (ERAGTC-LENGTH)
           END-EXEC
                                                                                
           MOVE SPACES                 TO AGENT-COMMISSIONS
 
           MOVE PI-COMPANY-CD          TO AG-COMPANY-CD
           MOVE 'AG'                   TO AG-RECORD-ID
 
           MOVE PI-ERC-KEY             TO AG-CONTROL-PRIMARY
           PERFORM VARYING PI-NDX FROM +1 BY +1 UNTIL
              PI-NDX > +10
              MOVE +0                  TO AG-SPP-FEES (PI-NDX)
              MOVE +0                  TO AG-SPP-LFEES (PI-NDX)
           END-PERFORM
           MOVE +1                     TO PI-NDX
           SET NO-CHANGES-MADE         TO TRUE
           MOVE WS-BIN-EFF-DT          TO AG-EFF-DT
           PERFORM 7300-EDIT-DATA      THRU 7300-EXIT
 
           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              EXEC CICS UNLOCK
                   DATASET  (AGTC-FILE-ID)
              END-EXEC
              GO TO 8200-SEND-DATAONLY
           END-IF 

           MOVE +0                     TO WS-TOT-FEES
                                          WS-TOT-LFEES
           PERFORM VARYING PI-NDX FROM +2 BY +1 UNTIL
              PI-NDX > +10
              COMPUTE WS-TOT-FEES = WS-TOT-FEES +
                 AG-SPP-FEES (PI-NDX)
              COMPUTE WS-TOT-LFEES = WS-TOT-LFEES +
                 AG-SPP-LFEES (PI-NDX)
           END-PERFORM

           MOVE LOW-VALUES             TO WS-ERCOMP-KEY
           MOVE PI-ERC-KEY (1:18)      TO WS-ERCOMP-KEY (1:18)
           MOVE PI-ERC-TYPE            TO WS-COMP-TYPE

           PERFORM 7350-VERIFY-ERCOMP  THRU 7350-EXIT

           IF RESP-NORMAL
              IF CO-MAX-BANK-FEE NOT NUMERIC
                 MOVE +0               TO CO-MAX-BANK-FEE
              END-IF
           ELSE
              GO TO 8200-SEND-DATAONLY
           END-IF

           IF CO-MAX-BANK-FEE = ZEROS
              CONTINUE
           ELSE
              IF WS-TOT-FEES > CO-MAX-BANK-FEE
                 MOVE ER-2717          TO EMI-ERROR
                 MOVE -1               TO CARRIERL
                 MOVE AL-UABON         TO CARRIERA
                                          GROUPA
                                          TYPEA
                                          BANKA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              ELSE
                 COMPUTE AG-SPP-FEES (1) = CO-MAX-BANK-FEE
                    - WS-TOT-FEES
              END-IF
           END-IF

           IF CO-MAX-BANK-FEE-LEASE = ZEROS
              CONTINUE
           ELSE
              IF WS-TOT-LFEES > CO-MAX-BANK-FEE-LEASE
                 MOVE ER-2717          TO EMI-ERROR
                 MOVE -1               TO CARRIERL
                 MOVE AL-UABON         TO CARRIERA
                                          GROUPA
                                          TYPEA
                                          BANKA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              ELSE
                 COMPUTE AG-SPP-LFEES (1) = CO-MAX-BANK-FEE-LEASE
                    - WS-TOT-LFEES
              END-IF
           END-IF

           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              EXEC CICS UNLOCK
                   DATASET  (AGTC-FILE-ID)
              END-EXEC
              GO TO 8200-SEND-DATAONLY
           END-IF 

           MOVE PI-PROCESSOR-ID        TO AG-LAST-MAINT-USER
           MOVE EIBTIME                TO AG-LAST-MAINT-HHMMSS
           MOVE SAVE-BIN-DATE          TO AG-LAST-MAINT-DT
 
           EXEC CICS WRITE                                                      
               DATASET  (AGTC-FILE-ID)                                          
               FROM     (AGENT-COMMISSIONS)
               RIDFLD   (AG-CONTROL-PRIMARY)
           END-EXEC
                                                                                
           GO TO 1050-READ-AGTC
           
      *    MOVE LOW-VALUES             TO EL652EO
      *    MOVE 'S'                    TO MAINTYPO
      *    MOVE +1                     TO MAINTYPL
      *    MOVE AL-UANON               TO MAINTYPA
      *    MOVE PI-ERC-CARRIER         TO CARRIERO
      *    MOVE AL-UANON               TO CARRIERA
      *    MOVE +1                     TO CARRIERL
      *    IF PI-ERC-GROUP NOT = SPACES
      *        MOVE PI-ERC-GROUP       TO GROUPO
      *        MOVE AL-UANON           TO GROUPA
      *        MOVE +6                 TO GROUPL
      *    END-IF
      *                                                                         
      *    IF PI-ERC-BANK NOT = SPACES
      *       MOVE PI-ERC-BANK         TO BANKO
      *       MOVE AL-UANON            TO BANKA
      *       MOVE +10                 TO BANKL
      *    END-IF

      *    IF PI-ERC-TYPE NOT = SPACES
      *       MOVE PI-ERC-TYPE         TO TYPEO
      *       MOVE AL-UANON            TO TYPEA
      *       MOVE +1                  TO TYPEL
      *    END-IF

      *    GO TO 1000-EDIT-INPUT
           .
       4200-EXIT.
           EXIT.
 
       4400-CHANGE.
 
           IF PI-ERC-KEY = PI-SAVE-ERAGTC-KEY
              CONTINUE
           ELSE
              IF PI-ERC-KEY (1:18) = PI-SAVE-ERAGTC-KEY (1:18)
                 PERFORM 4650-CHG-EXPDT THRU 4650-EXIT
              ELSE
                 MOVE ER-2056          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 MOVE -1               TO MAINTYPL
                 GO TO 8200-SEND-DATAONLY
              END-IF   
           END-IF

           IF EFF-DT-CHANGE
              PERFORM 4660-DATE-CHANGE THRU 4660-EXIT
              IF NOT EMI-NO-ERRORS
                 GO TO 8200-SEND-DATAONLY
              END-IF 
           END-IF 

           MOVE PI-ERC-KEY             TO WS-CONTROL-PRIMARY
           PERFORM 7200-READ-ERAGTC-UPDATE
                                       THRU 7200-EXIT
 
           IF (AG-LAST-MAINT-USER = PI-UPDATE-BY)
              AND (AG-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS)
              CONTINUE
           ELSE
              EXEC CICS UNLOCK
                   DATASET  (AGTC-FILE-ID)
              END-EXEC
              MOVE ER-0068             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU  9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF
           
           MOVE PI-TOP-NDX             TO PI-NDX
      *    SET NO-CHANGES-MADE         TO TRUE
           PERFORM 7300-EDIT-DATA      THRU 7300-EXIT
 
           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              EXEC CICS UNLOCK
                   DATASET  (AGTC-FILE-ID)
              END-EXEC
              GO TO 8200-SEND-DATAONLY
           END-IF 
 
           IF (NO-CHANGES-MADE)
              AND (NO-LINE-CHANGES)
              MOVE ER-3112             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO MAINTYPL
              GO TO 8200-SEND-DATAONLY
           END-IF

           MOVE +0                     TO WS-TOT-FEES
                                          WS-TOT-LFEES
           PERFORM VARYING PI-NDX FROM +2 BY +1 UNTIL
              PI-NDX > +10
              COMPUTE WS-TOT-FEES = WS-TOT-FEES +
                 AG-SPP-FEES (PI-NDX)
              COMPUTE WS-TOT-LFEES = WS-TOT-LFEES +
                 AG-SPP-LFEES (PI-NDX)
           END-PERFORM

           MOVE LOW-VALUES             TO WS-ERCOMP-KEY
           MOVE PI-ERC-KEY (1:18)      TO WS-ERCOMP-KEY (1:18)
           MOVE PI-ERC-TYPE            TO WS-COMP-TYPE

           PERFORM 7350-VERIFY-ERCOMP  THRU 7350-EXIT

           IF RESP-NORMAL
              IF CO-MAX-BANK-FEE NOT NUMERIC
                 MOVE +0               TO CO-MAX-BANK-FEE
              END-IF
           ELSE
              GO TO 8200-SEND-DATAONLY
           END-IF

           IF CO-MAX-BANK-FEE = ZEROS
              CONTINUE
           ELSE
              IF WS-TOT-FEES > CO-MAX-BANK-FEE
                 MOVE ER-2717          TO EMI-ERROR
                 MOVE -1               TO CARRIERL
                 MOVE AL-UABON         TO CARRIERA
                                          GROUPA
                                          TYPEA
                                          BANKA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              ELSE
                 COMPUTE AG-SPP-FEES (1) = CO-MAX-BANK-FEE
                    - WS-TOT-FEES
              END-IF
           END-IF

           IF CO-MAX-BANK-FEE-LEASE = ZEROS
              CONTINUE
           ELSE
              IF WS-TOT-LFEES > CO-MAX-BANK-FEE-LEASE
                 MOVE ER-2717          TO EMI-ERROR
                 MOVE -1               TO CARRIERL
                 MOVE AL-UABON         TO CARRIERA
                                          GROUPA
                                          TYPEA
                                          BANKA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              ELSE
                 COMPUTE AG-SPP-LFEES (1) = CO-MAX-BANK-FEE-LEASE
                    - WS-TOT-LFEES
              END-IF
           END-IF

           IF EFFDTL NOT = ZEROS
              MOVE WS-BIN-EFF-DT       TO AG-EFF-DT
           END-IF
           
           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              EXEC CICS UNLOCK
                   DATASET  (AGTC-FILE-ID)
              END-EXEC
              GO TO 8200-SEND-DATAONLY
           END-IF 

           MOVE PI-PROCESSOR-ID        TO AG-LAST-MAINT-USER
           MOVE EIBTIME                TO AG-LAST-MAINT-HHMMSS
           MOVE SAVE-BIN-DATE          TO AG-LAST-MAINT-DT
 
           EXEC CICS REWRITE
               DATASET  (AGTC-FILE-ID)
               FROM     (AGENT-COMMISSIONS)
           END-EXEC
 
           GO TO 1050-READ-AGTC
      *    GO TO 1000-EDIT-INPUT
           .
 
       4600-DELETE.
 
           IF PI-ERC-KEY = PI-SAVE-ERAGTC-KEY
              CONTINUE
           ELSE
              MOVE ER-2056             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO MAINTYPL
              GO TO 8200-SEND-DATAONLY
           END-IF
 
           MOVE PI-ERC-KEY             TO WS-CONTROL-PRIMARY
           PERFORM 7200-READ-ERAGTC-UPDATE
                                       THRU 7200-EXIT
 
           IF (AG-LAST-MAINT-USER = PI-UPDATE-BY)
              AND (AG-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS)
              CONTINUE
           ELSE
              EXEC CICS UNLOCK
                   DATASET  (AGTC-FILE-ID)
              END-EXEC
              MOVE ER-0068             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU  9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF
 
           EXEC CICS DELETE
               DATASET  (AGTC-FILE-ID)
           END-EXEC
                                                                                
           MOVE ER-0000                TO  EMI-ERROR                            
           PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                           
                                                                                
           MOVE LOW-VALUES             TO  EL652EO                              
           MOVE PI-ERC-CARRIER         TO  CARRIERO                             
           MOVE AL-UANON               TO  CARRIERA                             
                                                                                
           IF PI-ERC-GROUP NOT = SPACES                                         
               MOVE PI-ERC-GROUP       TO  GROUPO                               
               MOVE AL-UANON           TO  GROUPA                               
           END-IF
                                                                                
           IF PI-ERC-BANK    NOT = SPACES                                          
              MOVE PI-ERC-BANK        TO  BANKO
              MOVE AL-UANON           TO  BANKA
           END-IF

           IF PI-ERC-TYPE NOT = SPACES                                          
              MOVE PI-ERC-TYPE        TO  TYPEO
              MOVE AL-UANON           TO  TYPEA
           END-IF

           MOVE LOW-VALUES             TO  PI-SAVE-ERAGTC-KEY                   
                                                                                
           GO TO 8100-SEND-INITIAL-MAP                                          
           .                                                                    
 
       4600-EXIT.                                                               
           EXIT.                                                                
       4650-CHG-EXPDT.
       
           MOVE PI-ERC-KEY             TO WS-CONTROL-PRIMARY

           EXEC CICS READ
                SET     (ADDRESS OF AGENT-COMMISSIONS)
                DATASET ('ERAGTC')
                RIDFLD  (WS-CONTROL-PRIMARY)
                RESP    (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              MOVE ER-2057             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE LOW-VALUES          TO PI-SAVE-ERAGTC-KEY
              MOVE -1                  TO  MAINTYPL
              GO TO 8200-SEND-DATAONLY
           END-IF

           MOVE PI-SAVE-ERAGTC-KEY     TO WS-CONTROL-PRIMARY
           
           PERFORM 7200-READ-ERAGTC-UPDATE
                                       THRU 7200-EXIT
 
           IF (AG-LAST-MAINT-USER = PI-UPDATE-BY)
              AND (AG-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS)
              CONTINUE
           ELSE
              EXEC CICS UNLOCK
                   DATASET  (AGTC-FILE-ID)
              END-EXEC
              MOVE ER-0068             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU  9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF
 
           MOVE AGENT-COMMISSIONS     TO WS-SAVE-ERAGTC

           EXEC CICS DELETE
               DATASET  (AGTC-FILE-ID)
           END-EXEC
                                                                                
           MOVE WS-SAVE-ERAGTC         TO AGENT-COMMISSIONS
           MOVE PI-ERC-KEY             TO AG-CONTROL-PRIMARY
           MOVE PI-PROCESSOR-ID        TO AG-LAST-MAINT-USER
           MOVE EIBTIME                TO AG-LAST-MAINT-HHMMSS
           MOVE SAVE-BIN-DATE          TO AG-LAST-MAINT-DT
 
           EXEC CICS WRITE                                                      
               DATASET  (AGTC-FILE-ID)                                          
               FROM     (AGENT-COMMISSIONS)
               RIDFLD   (AG-CONTROL-PRIMARY)
           END-EXEC
                                                                                
           MOVE PI-ERC-KEY             TO WS-CONTROL-PRIMARY
           GO TO 1050-READ-AGTC

           .
       4650-EXIT.
           EXIT.
           
       4660-DATE-CHANGE.

           MOVE PI-SAVE-ERAGTC-KEY     TO WS-SKIP-KEY
           
           MOVE LOW-VALUES             TO WS-CONTROL-PRIMARY
           MOVE PI-SAVE-ERAGTC-KEY (1:18)
                                       TO WS-CONTROL-PRIMARY (1:18)
           EXEC CICS STARTBR
               DATASET (AGTC-FILE-ID)
               RIDFLD  (WS-CONTROL-PRIMARY)
               GTEQ
               RESP    (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              GO TO 1500-AGTC-NOT-FOUND
           END-IF
           
           MOVE 'Y'                    TO BROWSE-STARTED-SW
           MOVE SPACES                 TO WS-CHECK-DATE-SW
           PERFORM UNTIL
              (DATE-CHECK-OVER)
              PERFORM 4095-READNEXT-ERAGTC
                                       THRU 4095-EXIT
              IF RESP-NORMAL
                 IF AG-CONTROL-PRIMARY (1:18) =
                                 PI-SAVE-ERAGTC-KEY (1:18)
                    IF AG-CONTROL-PRIMARY NOT = WS-SKIP-KEY
                       IF (WS-BIN-EFF-DT NOT < AG-EFF-DT)
                          AND (WS-BIN-EFF-DT < AG-EXP-DT)
                          MOVE ER-0348 TO EMI-ERROR
                          MOVE -1      TO EFFDTL
                          MOVE AL-UABON
                                       TO EFFDTA
                          PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                          SET DATE-CHECK-OVER TO TRUE
                       END-IF
                    END-IF
                 ELSE
                    SET DATE-CHECK-OVER TO TRUE
                 END-IF
              ELSE
                 SET DATE-CHECK-OVER   TO TRUE
              END-IF
           END-PERFORM
                          
           IF BROWSE-STARTED
              EXEC CICS ENDBR
                   DATASET  (AGTC-FILE-ID)
              END-EXEC
              MOVE ' '                 TO BROWSE-STARTED-SW
           END-IF

           .
       4660-EXIT.
           EXIT.
           
       4095-READNEXT-ERAGTC.
       
           EXEC CICS READNEXT
               DATASET (AGTC-FILE-ID)
               SET     (ADDRESS OF AGENT-COMMISSIONS)
               RIDFLD  (WS-CONTROL-PRIMARY)
           END-EXEC

           .
       4095-EXIT.
           EXIT.
           
       5000-BROWSE-FILE.                                                        
 
           MOVE SPACE                  TO BROWSE-STARTED-SW
           EXEC CICS HANDLE CONDITION
               NOTFND (5800-NO-RECORD)
               ENDFILE (5900-END-OF-FILE)
           END-EXEC
                                                                                
           MOVE PI-SAVE-ERAGTC-KEY     TO WS-CONTROL-PRIMARY
                                          PI-ERC-KEY
 
           EXEC CICS STARTBR
               DATASET (AGTC-FILE-ID)
               RIDFLD (WS-CONTROL-PRIMARY)
           END-EXEC

           MOVE 'Y'                    TO BROWSE-STARTED-SW
 
           IF EIBAID = DFHPF2
              GO TO 5100-BROWSE-BKWD
           END-IF
           .
       5010-READ-LOOP.                                                          
 
           PERFORM 4095-READNEXT-ERAGTC
                                       THRU 4095-EXIT
           MOVE +1                     TO PI-NDX
 
           IF AG-COMPANY-CD NOT = PI-COMPANY-CD
              GO TO 5900-END-OF-FILE
           END-IF
 
      *    IF EIBAID = DFHPF1
      *       IF CARRIERO          = AG-CARRIER
      *          AND GROUPO        = AG-GROUPING
      *          AND BANKO         = AG-BANK
      *          AND TYPEO         = AG-TYPE
      *          AND WS-BIN-EXP-DT = AG-EXP-DT
      *          GO TO 5010-READ-LOOP
      *       END-IF
      *    END-IF

           IF EIBAID = DFHPF1
              IF PI-ERC-CARRIER    = AG-CARRIER
                 AND PI-ERC-GROUP  = AG-GROUPING
                 AND PI-ERC-BANK   = AG-BANK
                 AND PI-ERC-TYPE   = AG-TYPE
                 AND PI-ERC-EXP-DT = AG-EXP-DT
                 GO TO 5010-READ-LOOP
              END-IF
           END-IF

           MOVE AG-LAST-MAINT-USER     TO PI-UPDATE-BY
 
           IF AG-LAST-MAINT-HHMMSS NUMERIC
              MOVE AG-LAST-MAINT-HHMMSS
                                       TO PI-UPDATE-HHMMSS
           ELSE
              MOVE ZEROS               TO PI-UPDATE-HHMMSS
           END-IF
           MOVE AG-CONTROL-PRIMARY     TO PI-SAVE-ERAGTC-KEY
           GO TO 5200-FORMAT-SCREEN
           .
       5100-BROWSE-BKWD.
 
           EXEC CICS HANDLE CONDITION
                NOTFND (5900-END-OF-FILE)
           END-EXEC
 
           EXEC CICS READPREV
               DATASET (AGTC-FILE-ID)
               SET     (ADDRESS OF AGENT-COMMISSIONS)
               RIDFLD  (WS-CONTROL-PRIMARY)
           END-EXEC
 
           IF PI-FILE-EOF
              MOVE SPACE               TO PI-EOF-SW
           ELSE
              EXEC CICS READPREV
                   DATASET (AGTC-FILE-ID)
                   SET     (ADDRESS OF AGENT-COMMISSIONS)
                   RIDFLD  (WS-CONTROL-PRIMARY)
              END-EXEC
           END-IF
 
           MOVE +1                     TO PI-NDX
 
           IF AG-COMPANY-CD NOT = PI-COMPANY-CD
              GO TO 5900-END-OF-FILE
           END-IF
 
           MOVE AG-LAST-MAINT-USER     TO PI-UPDATE-BY
 
           IF AG-LAST-MAINT-HHMMSS NUMERIC
              MOVE AG-LAST-MAINT-HHMMSS
                                       TO PI-UPDATE-HHMMSS
           ELSE
              MOVE ZEROS               TO PI-UPDATE-HHMMSS
           END-IF
           MOVE AG-CONTROL-PRIMARY     TO PI-SAVE-ERAGTC-KEY

           .
       5200-FORMAT-SCREEN.                                                      
 
           MOVE LOW-VALUES             TO EL652EI
                                                                                
           MOVE AG-CONTROL-PRIMARY     TO PI-ERAGTC-KEY
                                                                                
           MOVE AG-CARRIER             TO CARRIERO
           MOVE AG-GROUPING            TO GROUPO
           MOVE AG-BANK                TO BANKO
           MOVE AG-TYPE                TO TYPEO

           IF AG-LAST-MAINT-DT NOT = SPACES AND LOW-VALUES AND ZEROS
              MOVE AG-LAST-MAINT-DT    TO DC-BIN-DATE-1
              MOVE SPACE               TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
              MOVE DC-GREG-DATE-1-EDIT TO MAINTDTO
           ELSE
              MOVE '00/00/00'          TO MAINTDTO
           END-IF

           MOVE AG-LAST-MAINT-USER     TO MAINTBYO
 
           IF AG-EXP-DT NOT = HIGH-VALUES
              MOVE AG-EXP-DT           TO DC-BIN-DATE-1
              MOVE SPACE               TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
              MOVE DC-GREG-DATE-1-EDIT TO EXPDTO
           ELSE
              MOVE '99/99/99'          TO EXPDTO
           END-IF

           IF AG-EFF-DT NOT = LOW-VALUES AND ZEROS AND SPACES
              MOVE AG-EFF-DT           TO DC-BIN-DATE-1
              MOVE SPACE               TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
              MOVE DC-GREG-DATE-1-EDIT TO EFFDTO
           ELSE
              MOVE '00/00/00'          TO EFFDTO
           END-IF

           PERFORM 3000-BLD-LINE       THRU 3000-XIT
 
           MOVE AL-UANON               TO CARRIERA GROUPA
                                          BANKA TYPEA EXPDTA
           GO TO 8100-SEND-INITIAL-MAP
           .
           
       5800-NO-RECORD.                                                          

           MOVE -1                     TO CARRIERL
           MOVE AL-UANON               TO CARRIERA  GROUPA
                                           BANKA TYPEA
           MOVE ER-1164                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           IF BROWSE-STARTED
              EXEC CICS ENDBR
                   DATASET  (AGTC-FILE-ID)
              END-EXEC
           END-IF

           GO TO 8200-SEND-DATAONLY
           .
       5900-END-OF-FILE.                                                        

           IF EIBAID = DFHPF1                                                   
              MOVE 'Y'                 TO PI-EOF-SW
              MOVE ER-2237             TO EMI-ERROR
           ELSE                                                                 
              MOVE LOW-VALUES          TO PI-ERAGTC-KEY                         
              MOVE PI-COMPANY-CD       TO PI-AGTC-COMP-CD                       
              MOVE ER-2238             TO EMI-ERROR
           END-IF
                                                                                
           MOVE -1                     TO CARRIERL
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           IF BROWSE-STARTED
              EXEC CICS ENDBR
                   DATASET  (AGTC-FILE-ID)
              END-EXEC
           END-IF
                                                                                
           MOVE SPACE                  TO BROWSE-STARTED-SW
           GO TO 8200-SEND-DATAONLY
           .
       7050-READ-ERAGTC.
       
           EXEC CICS READ
                SET     (ADDRESS OF AGENT-COMMISSIONS)
                DATASET ('ERAGTC')
                RIDFLD  (WS-CONTROL-PRIMARY)
           END-EXEC
 
           .
       7050-EXIT.
           EXIT.
           
 
       7200-READ-ERAGTC-UPDATE.
       
           EXEC CICS HANDLE CONDITION
               NOTFND (7200-AGTC-NOT-FOUND)
           END-EXEC
 
           EXEC CICS READ
                SET     (ADDRESS OF AGENT-COMMISSIONS)
                DATASET ('ERAGTC')
                RIDFLD  (WS-CONTROL-PRIMARY)
                UPDATE
           END-EXEC
 
           .
       7200-EXIT.
           EXIT.
           .
 
       7200-AGTC-NOT-FOUND.
 
           MOVE ER-0142                TO EMI-ERROR
           MOVE -1                     TO CARRIERL
           MOVE AL-UABON               TO CARRIERA
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
 
           IF EIBTRNID  = EL652-TRANS-ID
              GO TO 8100-SEND-INITIAL-MAP
           ELSE
              GO TO 8200-SEND-DATAONLY
           END-IF
 
           .
       7300-EDIT-DATA.
       
           SET NO-LINE-CHANGES         TO TRUE
           PERFORM VARYING NDX FROM +1 BY +1 UNTIL
              (NDX > +10)
              IF (REPRL      (NDX) NOT > +0)
                 AND (ATYPEL  (NDX) NOT > +0)
                 AND (FEESL   (NDX) NOT > +0)
                 AND (RECALL   (NDX) NOT > +0)
092205           AND (LFEESL   (NDX) NOT > +0)
092205           AND (LRCALL   (NDX) NOT > +0)                 
                 CONTINUE
              ELSE
               IF REPRL (NDX) > +0
                 IF REPR (NDX) = SPACES OR ZEROS
                    MOVE SPACES        TO ATYPE (NDX)
                                          RECAL (NDX)
                                          LRCAL (NDX)
                    MOVE ZEROS         TO FEES-COMM-R (NDX)
                                          LFEES-COMM-R (NDX)
                    MOVE +1            TO ATYPEL  (NDX)
                                          FEESL   (NDX)
                                          RECALL  (NDX)
                                          LFEESL  (NDX)
                                          LRCALL  (NDX)
                 ELSE
                    MOVE WS-CONTROL-PRIMARY (1:8)
                                       TO WS-ERCOMP-KEY (1:8)
                    MOVE REPR (NDX)    TO WS-COMP-FINRESP
                    MOVE LOW-VALUES    TO WS-COMP-ACCOUNT
                    MOVE ATYPE (NDX)   TO WS-COMP-TYPE
                    PERFORM 7350-VERIFY-ERCOMP
                                       THRU 7350-EXIT
                 END-IF
                 SET LINE-CHANGES      TO TRUE
                 MOVE REPR (NDX)       TO AG-AGT (NDX)
               END-IF
               IF ATYPEL (NDX) > +0
                 SET LINE-CHANGES      TO TRUE
                 MOVE ATYPE (NDX)      TO AG-COM-TYP (NDX)
               END-IF
               IF FEESL   (NDX) > +0
                 SET LINE-CHANGES      TO TRUE
                 EXEC CICS BIF
                      DEEDIT
                      FIELD   (FEES-COMM-R (NDX))
                      LENGTH  (8)
                 END-EXEC
                 IF FEES-COMM-R (NDX) NUMERIC
                    MOVE FEES-COMM-R (NDX)
                                       TO AG-SPP-FEES (NDX)
                 END-IF
               END-IF
               IF RECALL (NDX) > +0
                 SET LINE-CHANGES      TO TRUE
                 MOVE RECAL (NDX)      TO AG-RECALC-LV-INDIC (NDX)
               END-IF
               IF LFEESL   (NDX) > +0
                 SET LINE-CHANGES      TO TRUE
                 EXEC CICS BIF
                      DEEDIT
                      FIELD   (LFEES-COMM-R (NDX))
                      LENGTH  (8)
                 END-EXEC
                 IF LFEES-COMM-R (NDX) NUMERIC
                    MOVE LFEES-COMM-R (NDX)
                                       TO AG-SPP-LFEES (NDX)
                 END-IF
               END-IF
               IF LRCALL (NDX) > +0
                 SET LINE-CHANGES      TO TRUE
                 MOVE LRCAL (NDX)      TO AG-LRCALC-LV-INDIC (NDX)
               END-IF
               IF AG-AGT (NDX) NOT = SPACES AND ZEROS
                  IF AG-COM-TYP (NDX) = SPACES OR ZEROS
                     MOVE ER-2173      TO EMI-ERROR
                     MOVE -1           TO ATYPEL (NDX)
                     MOVE AL-UABON     TO ATYPEA (NDX)
                     PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                  END-IF
               END-IF
              END-IF
           END-PERFORM

           .
       7300-EXIT.
           EXIT.
           
       7350-VERIFY-ERCOMP.

           EXEC CICS READ
                DATASET  ('ERCOMP')
                SET      (ADDRESS OF COMPENSATION-MASTER)
                RIDFLD   (WS-ERCOMP-KEY)
                RESP     (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              MOVE ER-2230             TO EMI-ERROR
              MOVE -1                  TO REPRL (NDX)
              MOVE AL-UABON            TO REPRA (NDX)
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF

           .
       7350-EXIT.
           EXIT.

       7375-READ-ERCOMP.

           MOVE +0                     TO WS-TOT-FEES
                                          WS-TOT-LFEES
           MOVE WS-CONTROL-PRIMARY     TO WS-ERCOMP-KEY

           EXEC CICS READ
                DATASET  ('ERCOMP')
                SET      (ADDRESS OF COMPENSATION-MASTER)
                RIDFLD   (WS-ERCOMP-KEY)
                RESP     (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              CONTINUE
           ELSE
              MOVE ER-2230             TO EMI-ERROR
              MOVE -1                  TO REPRL (NDX)
              MOVE AL-UABON            TO REPRA (NDX)
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF

           .
       7375-EXIT.
           EXIT.

       7400-READ-CONTROL-FILE.
 
           MOVE PI-COMPANY-ID          TO CNTL-COMP-ID
           MOVE WS-ACCESS              TO CNTL-ACCESS
 
           EXEC CICS HANDLE CONDITION
               NOTFND  (7490-NOT-FOUND)
               ERROR   (9990-ABEND)
           END-EXEC
 
           EXEC CICS READ
               DATASET  (CNTL-FILE-ID)
               SET      (ADDRESS OF CONTROL-FILE)
               RIDFLD   (ELCNTL-KEY)
           END-EXEC
 
           IF CNTL-REC-TYPE = '6'
              MOVE AL-UANON           TO CARRIERA
              MOVE CARRIERI           TO PI-ERC-CARRIER
              GO TO 7499-EXIT
           END-IF
           .
       7490-NOT-FOUND.
 
           IF CNTL-REC-TYPE = '6'
              MOVE -1                  TO CARRIERL
              MOVE AL-UABON            TO CARRIERA
              MOVE ER-0193             TO EMI-ERROR
           END-IF
 
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           .
       7499-EXIT.
           EXIT.

       8100-SEND-INITIAL-MAP.                                                   
           MOVE SAVE-DATE              TO DATEO.                                
           MOVE EIBTIME                TO TIME-IN.                              
           MOVE TIME-OUT               TO TIMEO.                                
           MOVE -1                     TO MAINTYPL                              
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                              
                                                                                
           EXEC CICS SEND                                                       
               MAP    (MAP-NAME)                                                
               MAPSET (MAPSET-NAME)                                             
               FROM   (EL652EO)                                                 
               ERASE                                                            
               CURSOR                                                           
               END-EXEC.                                                        
           GO TO 9100-RETURN-TRAN.                                              
                                                                                
       8200-SEND-DATAONLY.                                                      
           MOVE SAVE-DATE              TO DATEO.                                
           MOVE EIBTIME                TO TIME-IN.                              
           MOVE TIME-OUT               TO TIMEO.                                
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                              
           EXEC CICS SEND                                                       
               MAP    (MAP-NAME)                                                
               MAPSET (MAPSET-NAME)                                             
               FROM   (EL652EO)                                                 
               DATAONLY                                                         
               CURSOR                                                           
           END-EXEC.
           GO TO 9100-RETURN-TRAN.                                              
                                                                                
           EJECT                                                                
       8500-DATE-CONVERT.                                                       
           MOVE LINK-ELDATCV           TO PGM-NAME.                             
           EXEC CICS LINK                                                       
               PROGRAM    (PGM-NAME)                                            
               COMMAREA   (DATE-CONVERSION-DATA)                                
               LENGTH     (DC-COMM-LENGTH)                                      
               END-EXEC.                                                        
       8500-EXIT.                                                               
           EXIT.                                                                
                                                                                
       8600-DEEDIT.
           EXEC CICS  BIF DEEDIT
               FIELD   (DEEDIT-FIELD)
               LENGTH  (8)
           END-EXEC.
       8600-EXIT.
            EXIT.
 
       8810-PF23.                                                               
           MOVE EIBAID                 TO PI-ENTRY-CD-1.                        
           MOVE XCTL-005               TO PGM-NAME.                             
           GO TO 9300-XCTL.                                                     
                                                                                
       9100-RETURN-TRAN.                                                        
           MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.                       
           MOVE '652E'               TO PI-CURRENT-SCREEN-NO.                   
           EXEC CICS RETURN                                                     
               TRANSID(TRANS-ID)                                                
               COMMAREA(PROGRAM-INTERFACE-BLOCK)                                
               LENGTH(PI-COMM-LENGTH)                                           
               END-EXEC.                                                        
                                                                                
           GOBACK.                                                              
                                                                                
       9910-INITIALIZE-SECURITY.                                                
      ******************************************************************        
      *                                                                *        
      *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *        
      *       USER SECURITY RECORD SET UP BY EL125.  THIS PROGRAM      *        
      *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *        
      *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *        
      *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *        
      *       ERROR CONDITION AND EXITS THE PROGRAM.                   *        
      *                                                                *        
      ******************************************************************        
                                                                                
           IF PI-PROCESSOR-ID NOT = 'LGXX'
 
              EXEC CICS    READQ TS
                   QUEUE   (PI-SECURITY-TEMP-STORE-ID)
                   INTO    (SECURITY-CONTROL)
                   LENGTH  (SC-COMM-LENGTH)
                   ITEM    (SC-ITEM-CL-CR)
              END-EXEC
 
              MOVE SC-CREDIT-DISPLAY (05)
                                       TO PI-DISPLAY-CAP
              MOVE SC-CREDIT-UPDATE  (05)
                                       TO PI-MODIFY-CAP
 
              IF  NOT DISPLAY-CAP
                  MOVE 'READ'          TO SM-READ
                  PERFORM 9995-SECURITY-VIOLATION
                                       THRU 9995-EXIT
                  MOVE ER-0070         TO EMI-ERROR
                  PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                  GO TO 8100-SEND-INITIAL-MAP
              END-IF
           END-IF
           .
       9910-EXIT.
           EXIT.
 
       9200-RETURN-MAIN-MENU.                                                   
           MOVE XCTL-626               TO PGM-NAME.                             
           GO TO 9300-XCTL.                                                     
                                                                                
           EJECT                                                                
                                                                                
       9300-XCTL.                                                               
           EXEC CICS XCTL                                                       
               PROGRAM    (PGM-NAME)                                            
               COMMAREA   (PROGRAM-INTERFACE-BLOCK)                             
               LENGTH     (PI-COMM-LENGTH)                                      
               END-EXEC.                                                        
                                                                                
       9400-CLEAR.                                                              
           MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                             
           GO TO 9300-XCTL.                                                     
                                                                                
       9500-PF12.                                                               
           MOVE XCTL-010               TO PGM-NAME.                             
           GO TO 9300-XCTL.                                                     
 
       9700-DATE-CONVERSION.
 
           EXEC CICS LINK                                                       
               PROGRAM   ('ELDATCV')                                            
               COMMAREA  (DATE-CONVERSION-DATA)                                 
               LENGTH    (DC-COMM-LENGTH)                                       
           END-EXEC.                                                            
                                                                                
       9700-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9900-ERROR-FORMAT.                                                       
           IF NOT EMI-ERRORS-COMPLETE                                           
               MOVE LINK-001 TO PGM-NAME                                        
               EXEC CICS LINK                                                   
                   PROGRAM(PGM-NAME)                                            
                   COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)                      
                   LENGTH(EMI-COMM-LENGTH)                                      
                   END-EXEC.                                                    
       9900-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9990-ABEND.                                                              
           MOVE LINK-004 TO PGM-NAME.                                           
           MOVE DFHEIBLK TO EMI-LINE1.                                          
           EXEC CICS LINK                                                       
               PROGRAM   (PGM-NAME)                                             
               COMMAREA  (EMI-LINE1)                                            
               LENGTH    (72)                                                   
               END-EXEC.                                                        
           GO TO 8200-SEND-DATAONLY.                                            
 
       9995-SECURITY-VIOLATION.                                                 
                                       COPY ELCSCTP.
       9995-EXIT.                                                               
            EXIT.                                                               
