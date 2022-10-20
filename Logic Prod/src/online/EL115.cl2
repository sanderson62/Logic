       ID DIVISION.                                                     
                                                                        
       PROGRAM-ID.                 EL115.
      *                            VMOD=2.001.
      *                                                                 
      *                                                                 
      *AUTHOR.     PABLO
      *            COLLEYVILLE, TEXAS.                                       
                                                                        
      *DATE-COMPILED.                                                   
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CENTRAL STATES  *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CENTRAL STATES IS EXPRESSLY PROHIBITED       *
      *            *   WITHOUT THE PRIOR WRITTEN PERMISSION OF         *
      *            *   CENTRAL STATES                                  *
      *            *                                                   *
      *            *****************************************************
                                                                        
      *REMARKS.    TRANSACTION- EX40 - STATE MAINTENANCE                
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * Changes are marked by the Change Effective date.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 122810    2010012700001  PEMA  NEW PROGRAM
      ******************************************************************

       ENVIRONMENT DIVISION.                                            
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
       77  FILLER  PIC X(32)  VALUE '********************************'. 
       77  FILLER  PIC X(32)  VALUE '*    EL115 WORKING STORAGE     *'. 
       77  FILLER  PIC X(32)  VALUE '********* VMOD=2.001 ***********'. 
                                                                        
                                       COPY ELCSCTM.
                                       COPY ELCSCRTY.

       01  FILLER.
           12  WS-VALID-REF-CODES      PIC X VALUE SPACES.
               88  VALID-REF-CODE         VALUE ' ' '1' THRU '6'
                                                '8' '9'.
           12  WS-NUMVAL.
               16  WS-NUMVAL-OF-DEEDIT     PIC 9(11) VALUE ZEROS.
               16  WS-NUMVAL-V4 REDEFINES WS-NUMVAL-OF-DEEDIT
                                           PIC 9(7)V9999.
           12  DEEDIT-FIELD                PIC X(11).
           12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD
                                           PIC S9(11).
           12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD
                                           PIC S9(09)V99.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL              VALUE +00.
               88  RESP-ERROR               VALUE +01.
               88  RESP-TERMIDERR           VALUE +11.
               88  RESP-NOTFND              VALUE +13.
               88  RESP-DUPREC              VALUE +14.
               88  RESP-DUPKEY              VALUE +15.
               88  RESP-INVREQ              VALUE +16.
               88  RESP-NOTOPEN             VALUE +19.
               88  RESP-ENDFILE             VALUE +20.
               88  RESP-ILLOGIC             VALUE +21.
               88  RESP-LENGERR             VALUE +22.

       01  WS-DATE-AREA.                                                
           05  SAVE-DATE           PIC X(8)    VALUE SPACES.            
           05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.            
                                                                        
       01  WS.                                                          
           05  W-APPL-SCRTY-NDX    PIC S9(04) COMP   VALUE +29.
           05  SC-ITEM-CL-CR       PIC S9(4)         VALUE +1   COMP.
           05  WS-COMM-LENGTH      PIC S9(4) COMP VALUE +1500.          
           05  WS-MAP-LENGTH       PIC S9(4) COMP VALUE +500.           
           05  RETURNED-FROM       PIC X(8)    VALUE SPACES.            
           05  QID.                                                     
               16  QID-TERM        PIC X(4).                            
               16  FILLER          PIC X(4)    VALUE '115A'.            
                                                                        
           05  WS-ST-LF-PREM-TAX   PIC S9V9(4)    VALUE +0.             
           05  WS-ST-AH-PREM-TAX-I PIC S9V9(4)    VALUE +0.             
           05  WS-ST-AH-PREM-TAX-G PIC S9V9(4)    VALUE +0.             
                                                                        
       01  STANDARD-AREAS.                                              
           12  MAP-NAME            PIC X(8)    VALUE 'EL115A'.          
           12  MAPSET-NAME         PIC X(8)    VALUE 'EL115S'.          
           12  TRANS-ID            PIC X(4)    VALUE 'EX40'.            
           12  PGM-NAME            PIC X(8).                            
           12  TIME-IN             PIC S9(7).                           
           12  TIME-OUT-R  REDEFINES TIME-IN.                           
               16  FILLER          PIC X.                               
               16  TIME-OUT        PIC 99V99.                           
               16  FILLER          PIC XX.                              
           12  XCTL-EL005          PIC X(8)    VALUE 'EL005'.           
           12  XCTL-EL010          PIC X(8)    VALUE 'EL010'.           
           12  XCTL-EL126          PIC X(8)    VALUE 'EL126'.           
           12  XCTL-EL626          PIC X(8)    VALUE 'EL626'.           
           12  XCTL-EL1151         PIC X(8)    VALUE 'EL1151'.          
           12  XCTL-GL800          PIC X(8)    VALUE 'GL800'.           
           12  LINK-EL001          PIC X(8)    VALUE 'EL001'.           
           12  LINK-EL004          PIC X(8)    VALUE 'EL004'.           
           12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.         
           12  THIS-PGM            PIC X(8)    VALUE 'EL115'.           
           12  ELCNTL-ID           PIC X(8)    VALUE 'ELCNTL'.
           12  ELSTAT-ID           PIC X(8)    VALUE 'ELSTAT'.
           12  SUB                 PIC S9(4)   COMP.                    
           12  GETMAIN-SPACE       PIC X       VALUE SPACE.             
           12  WS-FIRST-TIME-SW    PIC X       VALUE 'Y'.               
               88  FIRST-TIME                  VALUE 'Y'.               
           12  WS-DISPLAY-SW       PIC X       VALUE 'N'.               
               88  RETURN-DISPLAY              VALUE 'Y'.               
                                                                        
       01  ACCESS-KEYS.                                                 
           12  ELCNTL-KEY.                                              
               16  ELCNTL-COMP-ID      PIC XXX.
               16  ELCNTL-REC-TYPE     PIC X.
               16  ELCNTL-ACCESS       PIC XXXX.
               16  ELCNTL-SEQ-NO       PIC S9(4)  COMP.
           12  COMP-KEY                PIC X(20).
           12  STATE-KEY.
               16  STATE-COMP-ID       PIC XXX.
               16  STATE-CARRIER       PIC X.
               16  STATE-GROUPING      PIC X(6).
               16  STATE-STATE-CD      PIC XX.
               16  STATE-EXP-DT        PIC X(8).
               16  STATE-EXP-DT-NUM REDEFINES STATE-EXP-DT
                                       PIC 9(8).
                                                                        
       01  ERROR-MESSAGES.                                              
           12  ER-0000                 PIC X(4)  VALUE '0000'.
           12  ER-0004                 PIC X(4)  VALUE '0004'.
           12  ER-0013                 PIC X(4)  VALUE '0013'.
           12  ER-0023                 PIC X(4)  VALUE '0023'.
           12  ER-0029                 PIC X(4)  VALUE '0029'.
           12  ER-0042                 PIC X(4)  VALUE '0042'.
           12  ER-0050                 PIC X(4)  VALUE '0050'.
           12  ER-0068                 PIC X(4)  VALUE '0068'.
           12  ER-0070                 PIC X(4)  VALUE '0070'.
           12  ER-0131                 PIC X(4)  VALUE '0131'.
           12  ER-0141                 PIC X(4)  VALUE '0141'.
           12  ER-0144                 PIC X(4)  VALUE '0144'.
           12  ER-0145                 PIC X(4)  VALUE '0145'.
           12  ER-0146                 PIC X(4)  VALUE '0146'.
           12  ER-0147                 PIC X(4)  VALUE '0147'.
           12  ER-0148                 PIC X(4)  VALUE '0148'.
           12  ER-0149                 PIC X(4)  VALUE '0149'.
           12  ER-0150                 PIC X(4)  VALUE '0150'.
           12  ER-0151                 PIC X(4)  VALUE '0151'.
           12  ER-0152                 PIC X(4)  VALUE '0152'.
           12  ER-0153                 PIC X(4)  VALUE '0153'.
           12  ER-0159                 PIC X(4)  VALUE '0159'.
           12  ER-0160                 PIC X(4)  VALUE '0160'.
           12  ER-0161                 PIC X(4)  VALUE '0161'.
           12  ER-0235                 PIC X(4)  VALUE '0235'.
           12  ER-0582                 PIC X(4)  VALUE '0582'.
           12  ER-0805                 PIC X(4)  VALUE '0805'.
           12  ER-1614                 PIC X(4)  VALUE '1614'.
           12  ER-2009                 PIC X(4)  VALUE '2009'.
           12  ER-2010                 PIC X(4)  VALUE '2010'.
           12  ER-2012                 PIC X(4)  VALUE '2012'.
           12  ER-2014                 PIC X(4)  VALUE '2014'.
           12  ER-2024                 PIC X(4)  VALUE '2024'.
           12  ER-2028                 PIC X(4)  VALUE '2028'.
           12  ER-2032                 PIC X(4)  VALUE '2032'.
           12  ER-2033                 PIC X(4)  VALUE '2033'.
           12  ER-2082                 PIC X(4)  VALUE '2082'.
           12  ER-2084                 PIC X(4)  VALUE '2084'.
           12  ER-2137                 PIC X(4)  VALUE '2137'.
           12  ER-2298                 PIC X(4)  VALUE '2298'.
           12  ER-2299                 PIC X(4)  VALUE '2299'.
           12  ER-2603                 PIC X(4)  VALUE '2603'.
           12  ER-2845                 PIC X(4)  VALUE '2845'.
           12  ER-3030                 PIC X(4)  VALUE '3030'.
           12  ER-3031                 PIC X(4)  VALUE '3031'.
           12  ER-3032                 PIC X(4)  VALUE '3032'.
           12  ER-3033                 PIC X(4)  VALUE '3033'.
           12  ER-3034                 PIC X(4)  VALUE '3034'.
           12  ER-3035                 PIC X(4)  VALUE '3035'.
           12  ER-3036                 PIC X(4)  VALUE '3036'.
           12  ER-7008                 PIC X(4)  VALUE '7008'.
           12  ER-7346                 PIC X(4)  VALUE '7346'.
           12  ER-7531                 PIC X(4)  VALUE '7531'.
           12  ER-7532                 PIC X(4)  VALUE '7532'.
           12  ER-7536                 PIC X(4)  VALUE '7536'.
           12  ER-7717                 PIC X(4)  VALUE '7717'.
           12  ER-7735                 PIC X(4)  VALUE '7735'.
           12  ER-8008                 PIC X(4)  VALUE '8008'.
           12  ER-8159                 PIC X(4)  VALUE '8159'.
           12  ER-9074                 PIC X(4)  VALUE '9074'.
           12  ER-9097                 PIC X(4)  VALUE '9097'.
           12  ER-9447                 PIC X(4)  VALUE '9447'.
           12  ER-9448                 PIC X(4)  VALUE '9448'.
           12  ER-9478                 PIC X(4)  VALUE '9478'.
           12  ER-9999                 PIC X(4)  VALUE '9999'.

                                       COPY ELCCNTL.
                                       COPY ELCSTATE.
                                       COPY ELCDATE.
                                       COPY ELCLOGOF.
                                       COPY ELCATTR.
                                       COPY ELCEMIB.
                                       COPY ELCJPFX.
                                       PIC X(530).

                                       COPY ELCINTF.
           12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.                
               16  PI-PREV-KEY.
                   20  PI-PREV-COMP-ID PIC XXX.
                   20  PI-PREV-CARR    PIC X.
                   20  PI-PREV-GROUP   PIC X(6).
                   20  PI-PREV-STATE   PIC XX.
                   20  PI-PREV-EXP-DT  PIC 9(8).
               16  FILLER              PIC X(620).

                                       COPY ELCAID.
       01  FILLER REDEFINES DFHAID.                                     
           12  FILLER              PIC X(8).                            
           12  PF-VALUES           PIC X       OCCURS 2.                

                                       COPY EL115S.

       LINKAGE SECTION.                                                 
       01  DFHCOMMAREA             PIC X(1500).                         
      *01 PARMLIST .                                                    
      *    02  FILLER              PIC S9(8)   COMP.                    
      *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                    
      *    02  ELLETR-POINTER      PIC S9(8)   COMP.                    


       PROCEDURE DIVISION.                                              

           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
           MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE
           MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE

           MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK
           MOVE 2                      TO EMI-NUMBER-OF-LINES
                                          EMI-SWITCH2

           MOVE EIBTRMID               TO QID-TERM

           IF EIBCALEN = 0
              GO TO 8800-UNAUTHORIZED-ACCESS
           END-IF
                                                                        
           IF PI-RETURN-TO-PROGRAM = THIS-PGM
              MOVE PI-CALLING-PROGRAM TO RETURNED-FROM
           ELSE
              MOVE SPACES             TO RETURNED-FROM
           END-IF
                                                                        
           IF PI-CALLING-PROGRAM NOT = THIS-PGM
              IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
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
                 MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
                 MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
                 MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
                 MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
                 MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
                 MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
                 MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
                 MOVE SPACES               TO PI-SAVED-PROGRAM-6
              END-IF
           END-IF
                                                                        
           MOVE 'N' TO WS-DISPLAY-SW.                                   
                                                                        
           EXEC CICS HANDLE CONDITION                                   
               NOTOPEN (8870-NOTOPEN)                                   
               PGMIDERR(9600-PGMID-ERROR)                               
               ERROR   (9990-ABEND)                                     
           END-EXEC.                                                    

           IF EIBTRNID NOT = TRANS-ID
              MOVE LOW-VALUES          TO EL115AO
              GO TO 8100-SEND-INITIAL-MAP
           END-IF

           IF EIBAID = DFHCLEAR
              GO TO 9400-CLEAR
           END-IF
                                                                        
           IF NOT DISPLAY-CAP                                    
              MOVE 'READ'              TO SM-READ
              PERFORM 9995-SECURITY-VIOLATION
              MOVE ER-0070             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              GO TO 8100-SEND-INITIAL-MAP
           END-IF

           .
       0200-RECEIVE.

           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
              MOVE LOW-VALUES          TO EL115AI
              MOVE ER-7008             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              GO TO 8200-SEND-DATAONLY
           END-IF

           EXEC CICS RECEIVE
               MAP   (MAP-NAME)
               MAPSET(MAPSET-NAME)
               INTO  (EL115AI)
           END-EXEC

           IF ENTERPFL = 0
              GO TO 0300-CHECK-PFKEYS
           END-IF

           IF EIBAID NOT = DFHENTER
              MOVE ER-0004             TO EMI-ERROR
              GO TO 0320-INPUT-ERROR
           END-IF

           IF (ENTERPFI NUMERIC) AND (ENTERPFI > 0 AND < 25)
              MOVE PF-VALUES (ENTERPFI) TO EIBAID
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
                                                                        
           IF MAINTL NOT = 0  AND                                       
              EIBAID NOT = DFHENTER                                     
                MOVE ER-0050 TO EMI-ERROR                               
                GO TO 0320-INPUT-ERROR.                                 
                                                                        
           IF EIBAID = DFHPF1                                           
               GO TO 5000-FIND-NEXT-STATE.                              
                                                                        
           IF EIBAID = DFHPF2                                           
               GO TO 5500-FIND-PREV-STATE.                              

           IF EIBAID = DFHENTER                                         
               GO TO 0330-EDIT-DATA.                                    
                                                                        
           MOVE ER-0029 TO EMI-ERROR.                                   

       0320-INPUT-ERROR.                                                

           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           MOVE AL-UNBON               TO ENTERPFA

           IF ENTERPFL = 0
              MOVE -1                  TO MAINTL
           ELSE
              MOVE -1                  TO ENTERPFL
           END-IF

           GO TO 8200-SEND-DATAONLY

           .
       0330-EDIT-DATA.                                                  

           IF MAINTI = 'S'
              GO TO 1000-SHOW-STATE
           END-IF

           IF MODIFY-CAP
              CONTINUE
           ELSE
              IF MAINTI = 'A' OR 'C' OR 'D'
                 MOVE 'UPDATE'         TO SM-READ
                 PERFORM 9995-SECURITY-VIOLATION
                 MOVE ER-0070          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 MOVE LOW-VALUES       TO EL115AO
                 GO TO 8100-SEND-INITIAL-MAP
              END-IF
           END-IF

           IF MAINTI = 'C'                                              
               GO TO 2000-CHANGE-STATE.                                 
                                                                        
           IF MAINTI = 'A'                                              
               GO TO 3000-ADD-STATE.                                    
                                                                        
           IF MAINTI = 'D'                                              
               GO TO 4000-DELETE-STATE.                                 
                                                                        
           MOVE ER-0023 TO EMI-ERROR.                                   
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE -1       TO MAINTL.                                     
           MOVE AL-UABON TO MAINTA.                                     
           GO TO 8200-SEND-DATAONLY.                                    

           .
       0700-BUILD-KEY.

           MOVE PI-COMPANY-ID          TO STATE-COMP-ID
           MOVE CARRI                  TO STATE-CARRIER
           MOVE GROUPI                 TO STATE-GROUPING
           MOVE STATEI                 TO STATE-STATE-CD

           MOVE EXPDTI                 TO DEEDIT-FIELD
           PERFORM 8600-DEEDIT         THRU 8600-EXIT
           IF WS-NUMVAL-OF-DEEDIT >= 99000000
              MOVE 99999999            TO STATE-EXP-DT
           ELSE
              STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
                 DELIMITED BY SIZE INTO STATE-EXP-DT
              END-STRING
           END-IF
pemtst*    display ' 0700 build ' expdti ' ' ws-numval ' ' state-exp-dt

           .
       0700-EXIT.
           EXIT.

       0750-EDIT-KEY.

           MOVE SPACES                 TO ELCNTL-ACCESS
           MOVE PI-COMPANY-ID          TO ELCNTL-COMP-ID
           MOVE '6'                    TO ELCNTL-REC-TYPE
           MOVE STATE-CARRIER          TO ELCNTL-ACCESS (4:1)
           MOVE +0                     TO ELCNTL-SEQ-NO
           EXEC CICS READ
               DATASET (ELCNTL-ID)
               INTO    (CONTROL-FILE)
               RIDFLD  (ELCNTL-KEY)
               RESP    (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              MOVE ER-2845             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO CARRL
              MOVE AL-UABON            TO CARRA
           END-IF

           IF (GROUPL = ZEROS)
              OR (GROUPI = SPACES OR LOW-VALUES)
              MOVE ER-0235             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO CARRL
              MOVE AL-UABON            TO CARRA
           END-IF


           MOVE SPACES                 TO ELCNTL-ACCESS
           MOVE PI-COMPANY-ID          TO ELCNTL-COMP-ID
           MOVE '3'                    TO ELCNTL-REC-TYPE
           MOVE STATE-STATE-CD         TO ELCNTL-ACCESS (1:2)
           MOVE +0                     TO ELCNTL-SEQ-NO
           EXEC CICS READ
               DATASET (ELCNTL-ID)
               INTO    (CONTROL-FILE)
               RIDFLD  (ELCNTL-KEY)
               RESP    (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              MOVE ER-2603             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO STATEL
              MOVE AL-UABON            TO STATEA
           END-IF

           IF STATE-EXP-DT-NUM NOT = 99999999
              MOVE STATE-EXP-DT-NUM    TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              MOVE +0                  TO DC-ELAPSED-DAYS
                                          DC-ELAPSED-MONTHS
              PERFORM 9700-LINK-DATE-CONVERT
                                       THRU 9700-EXIT
              IF NOT NO-CONVERSION-ERROR
                 MOVE ER-8008          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 MOVE -1               TO EXPDTL
                 MOVE AL-UABON         TO EXPDTA
              END-IF
           END-IF

           .
       0750-EXIT.
           EXIT.

       1000-SHOW-STATE.

           PERFORM 0700-BUILD-KEY      THRU 0700-EXIT

           EXEC CICS READ
               DATASET (ELSTAT-ID)
               INTO    (STATE-FILE)
               RIDFLD  (STATE-KEY)
               RESP    (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              MOVE STATE-KEY           TO PI-PREV-KEY
              GO TO 7000-BUILD-OUTPUT-MAP
           END-IF

           MOVE ER-0149                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           MOVE -1                     TO STATEL
           MOVE AL-UABON               TO STATEA
           GO TO 8200-SEND-DATAONLY

           .
       1050-UPDATE-RECORD.

           MOVE PI-PROCESSOR-ID        TO SF-LAST-MAINT-BY
           MOVE EIBTIME                TO SF-LAST-MAINT-HHMMSS
           MOVE SAVE-BIN-DATE          TO SF-LAST-MAINT-DT

           IF LFREDL > ZEROS
              MOVE LFREDI               TO SF-ST-RF-LR-CALC
           END-IF

           IF LFLEVL NOT = ZEROS
              MOVE LFLEVI               TO SF-ST-RF-LL-CALC
           END-IF

           IF LFNETL NOT = ZEROS
              MOVE LFNETI               TO SF-ST-RF-LN-CALC
           END-IF

           IF AHAHL NOT = ZEROS
              MOVE AHAHI                TO SF-ST-RF-AH-CALC
           END-IF

           IF AHCPL NOT = ZEROS
              MOVE AHCPI                TO SF-ST-RF-CP-CALC
           END-IF

           IF LFTAXL > +0
              MOVE WS-ST-LF-PREM-TAX   TO SF-ST-LF-PREM-TAX             
           END-IF

           IF AHITAXL > +0
              MOVE WS-ST-AH-PREM-TAX-I TO SF-ST-AH-PREM-TAX-I           
           END-IF

           IF AHGTAXL > +0
              MOVE WS-ST-AH-PREM-TAX-G TO SF-ST-AH-PREM-TAX-G           
           END-IF

           .
       1050-EXIT.
           EXIT.

       2000-CHANGE-STATE.

           PERFORM 0700-BUILD-KEY      THRU 0700-EXIT

           IF STATE-KEY NOT = PI-PREV-KEY
              MOVE ER-0145             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO STATEL
              MOVE AL-UABON            TO STATEA
              GO TO 8200-SEND-DATAONLY
           END-IF

           PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT

           IF NOT EMI-NO-ERRORS
              GO TO 8200-SEND-DATAONLY
           END-IF

           EXEC CICS READ                                               
               UPDATE                                                   
               DATASET(ELSTAT-ID)                                       
               INTO   (STATE-FILE)
               RIDFLD (STATE-KEY)                                      
               RESP   (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              MOVE ER-0149             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO STATEL
              MOVE AL-UABON            TO STATEA
              GO TO 8200-SEND-DATAONLY
           END-IF

           IF (SF-LAST-MAINT-BY NOT = PI-UPDATE-BY)
              OR (SF-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS)
              EXEC CICS UNLOCK                                         
                   DATASET(ELSTAT-ID)                                   
               END-EXEC                                                 
               MOVE ER-0068 TO EMI-ERROR                                
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
               GO TO 1000-SHOW-STATE
           END-IF

           PERFORM 1050-UPDATE-RECORD  THRU 1050-EXIT

           EXEC CICS REWRITE                                            
               DATASET (ELSTAT-ID)                                       
               FROM    (STATE-FILE)
               RESP    (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              GO TO 9990-ABEND
           END-IF

           MOVE ER-0000                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           GO TO 1000-SHOW-STATE

           .
       3000-ADD-STATE.                                                  

           PERFORM 0700-BUILD-KEY      THRU 0700-EXIT
           PERFORM 0750-EDIT-KEY       THRU 0750-EXIT

           IF NOT EMI-NO-ERRORS                                         
               GO TO 8200-SEND-DATAONLY.                                

           PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT
                                                                        
           IF NOT EMI-NO-ERRORS                                         
               GO TO 8200-SEND-DATAONLY
           END-IF

           MOVE 'SF'                   TO STATE-FILE
           MOVE PI-COMPANY-ID          TO SF-COMPANY-ID
           MOVE STATE-CARRIER          TO SF-CARRIER-CODE
           MOVE STATE-GROUPING         TO SF-GROUPING
           MOVE STATE-STATE-CD         TO SF-STATE-CODE
           MOVE STATE-EXP-DT-NUM       TO SF-EXPIRE-DATE
           MOVE ZEROS                  TO SF-ST-LF-PREM-TAX
                                          SF-ST-AH-PREM-TAX-I
                                          SF-ST-AH-PREM-TAX-G

           PERFORM 1050-UPDATE-RECORD  THRU 1050-EXIT

           EXEC CICS WRITE                                            
               DATASET (ELSTAT-ID)
               FROM    (STATE-FILE)
               RIDFLD  (STATE-KEY)
               RESP    (WS-RESPONSE)
           END-EXEC

           IF RESP-DUPREC
              MOVE ER-0147             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO STATEL
              MOVE AL-UABON            TO STATEA
              GO TO 8200-SEND-DATAONLY
           END-IF
                                                                        
           IF NOT RESP-NORMAL
              GO TO 9990-ABEND
           END-IF                                                                        
                                                                        
           MOVE ER-0000                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           GO TO 1000-SHOW-STATE

           .
       4000-DELETE-STATE.                                               

           PERFORM 0700-BUILD-KEY      THRU 0700-EXIT

           IF STATE-KEY NOT = PI-PREV-KEY
              MOVE ER-0145             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO STATEL
              MOVE AL-UABON            TO STATEA
              GO TO 8200-SEND-DATAONLY
           END-IF

                                                                        
           EXEC CICS READ                                               
               UPDATE                                                   
               DATASET(ELSTAT-ID)                                       
               INTO   (STATE-FILE)
               RIDFLD (STATE-KEY)                                      
               RESP   (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              MOVE ER-0149             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO STATEL
              MOVE AL-UABON            TO STATEA
              GO TO 8200-SEND-DATAONLY
           END-IF

           IF (SF-LAST-MAINT-BY NOT = PI-UPDATE-BY)
              OR (SF-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS)
              EXEC CICS UNLOCK                                         
                   DATASET(ELSTAT-ID)                                   
               END-EXEC                                                 
               MOVE ER-0068 TO EMI-ERROR                                
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
               GO TO 1000-SHOW-STATE
           END-IF

           MOVE ER-0000                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           MOVE LOW-VALUES              TO EL115AO
           GO TO 8100-SEND-INITIAL-MAP

           .
       5000-FIND-NEXT-STATE.

           PERFORM 0700-BUILD-KEY      THRU 0700-EXIT

           EXEC CICS STARTBR
               DATASET  (ELSTAT-ID)
               RIDFLD   (STATE-KEY)
               RESP     (WS-RESPONSE)
               GTEQ
           END-EXEC

           IF RESP-ENDFILE
              MOVE ER-0148             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              GO TO 8200-SEND-DATAONLY
           ELSE
              IF RESP-NOTFND
                 MOVE ER-0149          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 MOVE -1               TO MAINTL
                 GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF

           IF NOT RESP-NORMAL
              GO TO 9990-ABEND
           END-IF

           .
       5050-READ-NEXT.

           EXEC CICS READNEXT
               DATASET  (ELSTAT-ID)
               INTO     (STATE-FILE)
               RIDFLD   (STATE-KEY)
               RESP     (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL AND STATE-KEY = PI-PREV-KEY
              GO TO 5050-READ-NEXT
           END-IF

           EXEC CICS ENDBR
               DATASET   (ELSTAT-ID)
           END-EXEC

           IF (RESP-ENDFILE)
              OR (PI-COMPANY-ID NOT = STATE-COMP-ID)
              MOVE ER-0148             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              GO TO 8200-SEND-DATAONLY
           ELSE
              IF RESP-NOTFND
                 MOVE ER-0149          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 MOVE -1               TO MAINTL
                 GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF

           IF NOT RESP-NORMAL
              GO TO 9990-ABEND
           END-IF

           MOVE STATE-KEY              TO PI-PREV-KEY
           GO TO 7000-BUILD-OUTPUT-MAP

           .
       5500-FIND-PREV-STATE.                                            

           PERFORM 0700-BUILD-KEY      THRU 0700-EXIT

           EXEC CICS STARTBR
               DATASET  (ELSTAT-ID)
               RIDFLD   (STATE-KEY)
               RESP     (WS-RESPONSE)
               GTEQ
           END-EXEC

           IF RESP-ENDFILE
              MOVE ER-0131             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              GO TO 8200-SEND-DATAONLY
           ELSE
              IF RESP-NOTFND
                 MOVE ER-0149          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 MOVE -1               TO MAINTL
                 GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF

           IF NOT RESP-NORMAL
              GO TO 9990-ABEND
           END-IF

           .
       5600-READ-PREV.

           EXEC CICS READPREV                                           
               DATASET  (ELSTAT-ID)
               INTO     (STATE-FILE)
               RIDFLD   (STATE-KEY)
               RESP     (WS-RESPONSE)
           END-EXEC
                                                                        
           IF RESP-NORMAL AND STATE-KEY = PI-PREV-KEY
              GO TO 5600-READ-PREV
           END-IF

           EXEC CICS ENDBR
               DATASET   (ELSTAT-ID)
           END-EXEC

           IF (RESP-ENDFILE)
              OR (PI-COMPANY-ID NOT = STATE-COMP-ID)
              MOVE ER-0148             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              GO TO 8200-SEND-DATAONLY
           ELSE
              IF RESP-NOTFND
                 MOVE ER-0149          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 MOVE -1               TO MAINTL
                 GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF

           IF NOT RESP-NORMAL
              GO TO 9990-ABEND
           END-IF

           MOVE STATE-KEY              TO PI-PREV-KEY
           GO TO 7000-BUILD-OUTPUT-MAP

           .
       6000-EDIT-INPUT-DATA.                                            

           IF LFTAXL > ZEROS
              MOVE LFTAXI              TO DEEDIT-FIELD
              PERFORM 8600-DEEDIT      THRU 8600-EXIT
pemtst*       display ' lf tax ' lftaxi ' ' ws-numval ' ' deedit-field
              MOVE WS-NUMVAL-V4        TO WS-ST-LF-PREM-TAX
           END-IF

           IF AHITAXL > ZEROS
              MOVE AHITAXI             TO DEEDIT-FIELD
              PERFORM 8600-DEEDIT      THRU 8600-EXIT
              MOVE WS-NUMVAL-V4        TO WS-ST-AH-PREM-TAX-I
           END-IF

           IF AHGTAXL > ZEROS
              MOVE AHGTAXI             TO DEEDIT-FIELD
              PERFORM 8600-DEEDIT      THRU 8600-EXIT
              MOVE WS-NUMVAL-V4        TO WS-ST-AH-PREM-TAX-G
           END-IF

           IF LFREDL > ZEROS
              MOVE LFREDI              TO WS-VALID-REF-CODES
              IF NOT VALID-REF-CODE
                 MOVE -1               TO LFREDL
                 MOVE AL-UABON         TO LFREDA
                 MOVE ER-0582          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF LFLEVL > ZEROS
              MOVE LFLEVI              TO WS-VALID-REF-CODES
              IF NOT VALID-REF-CODE
                 MOVE -1               TO LFLEVL
                 MOVE AL-UABON         TO LFLEVA
                 MOVE ER-0582          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF LFNETL > ZEROS
              MOVE LFNETI              TO WS-VALID-REF-CODES
              IF NOT VALID-REF-CODE
                 MOVE -1               TO LFNETL
                 MOVE AL-UABON         TO LFNETA
                 MOVE ER-0582          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF AHAHL > ZEROS
              MOVE AHAHI               TO WS-VALID-REF-CODES
              IF NOT VALID-REF-CODE
                 MOVE -1               TO AHAHL
                 MOVE AL-UABON         TO AHAHA
                 MOVE ER-0582          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF AHCPL > ZEROS
              MOVE AHCPI               TO WS-VALID-REF-CODES
              IF NOT VALID-REF-CODE
                 MOVE -1               TO AHCPL
                 MOVE AL-UABON         TO AHCPA
                 MOVE ER-0582          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           .                                                            
       6000-EXIT.                                                       
           EXIT.                                                        

      ***************************************************************   
      *                                                             *   
      *     BUILD THE OUTPUT SCREEN TO BE DISPLAYED                 *   
      *                                                             *   
      ***************************************************************   
       7000-BUILD-OUTPUT-MAP.

           MOVE LOW-VALUES             TO EL115AO

           MOVE SF-CARRIER-CODE        TO CARRO
           MOVE SF-GROUPING            TO GROUPO
           MOVE SF-STATE-CODE          TO STATEO
           STRING SF-EXPIRE-DATE (5:2) '/' SF-EXPIRE-DATE (7:2) '/'
              SF-EXPIRE-DATE (1:4) DELIMITED BY SIZE
              INTO EXPDTO
           END-STRING

           MOVE SF-LAST-MAINT-DT       TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-DAYS
                                          DC-ELAPSED-MONTHS
           PERFORM 9700-LINK-DATE-CONVERT
                                       THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO LSTDTEO
           ELSE
              MOVE LOW-VALUES          TO LSTDTEO
           END-IF
           MOVE SF-LAST-MAINT-BY       TO LSTUSRO
                                          PI-UPDATE-BY
           MOVE SF-LAST-MAINT-HHMMSS   TO TIME-IN
                                          PI-UPDATE-HHMMSS
           MOVE TIME-OUT               TO LSTTIMEO

           MOVE SF-ST-LF-PREM-TAX      TO LFTAXO
           MOVE SF-ST-AH-PREM-TAX-I    TO AHITAXO
           MOVE SF-ST-AH-PREM-TAX-G    TO AHGTAXO

           MOVE SF-ST-RF-LR-CALC       TO LFREDO
           MOVE SF-ST-RF-LL-CALC       TO LFLEVO
           MOVE SF-ST-RF-LN-CALC       TO LFNETO
           MOVE SF-ST-RF-AH-CALC       TO AHAHO
           MOVE SF-ST-RF-CP-CALC       TO AHCPO

           MOVE -1                     TO MAINTL
           MOVE AL-UANOF               TO MAINTA
           MOVE AL-UANON               TO CARRA
                                          GROUPA
                                          STATEA
                                          EXPDTA

           GO TO 8200-SEND-DATAONLY

           .
       8100-SEND-INITIAL-MAP.                                           

           MOVE SAVE-DATE              TO RUNDTEO
           MOVE EIBTIME                TO TIME-IN
           MOVE TIME-OUT               TO RUNTIMEO
           MOVE -1                     TO MAINTL
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO

           EXEC CICS SEND                                               
               MAP   (MAP-NAME)                                         
               MAPSET(MAPSET-NAME)                                      
               FROM  (EL115AO)                                          
               ERASE                                                    
               CURSOR                                                   
           END-EXEC

           GO TO 9100-RETURN-TRAN

           .
       8200-SEND-DATAONLY.

           MOVE SAVE-DATE              TO RUNDTEO
           MOVE EIBTIME                TO TIME-IN
           MOVE TIME-OUT               TO RUNTIMEO

           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO

           EXEC CICS SEND                                               
               MAP      (MAP-NAME)
               MAPSET   (MAPSET-NAME)
               FROM     (EL115AO)
               DATAONLY
               ERASEAUP
               CURSOR
           END-EXEC

           GO TO 9100-RETURN-TRAN

           .
       8300-SEND-TEXT.                                                  
           EXEC CICS SEND TEXT                                          
               FROM  (LOGOFF-TEXT)                                      
               LENGTH(LOGOFF-LENGTH)                                    
               ERASE                                                    
               FREEKB                                                   
           END-EXEC.                                                    
                                                                        
           EXEC CICS RETURN                                             
           END-EXEC

           .
       8600-DEEDIT.

           INSPECT DEEDIT-FIELD
              REPLACING ALL '.' BY SPACES

           MOVE FUNCTION NUMVAL(DEEDIT-FIELD)
                                       TO WS-NUMVAL-OF-DEEDIT

           .
       8600-EXIT.                                                       
           EXIT.                                                        

       8800-UNAUTHORIZED-ACCESS.                                        
           MOVE UNACCESS-MSG TO LOGOFF-MSG.                             
           GO TO 8300-SEND-TEXT.                                        
                                                                        
       8810-PF23.                                                       
           MOVE EIBAID   TO PI-ENTRY-CD-1.                              
           MOVE XCTL-EL005 TO PGM-NAME.                                 
           GO TO 9300-XCTL.                                             
                                                                        
       8870-NOTOPEN.                                                    
           MOVE ER-0042 TO EMI-ERROR.                                   
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE -1 TO MAINTL.                                           
           GO TO 8200-SEND-DATAONLY.                                    

       9000-RETURN-CICS.                                                
           EXEC CICS RETURN                                             
           END-EXEC.                                                    
                                                                        
       9100-RETURN-TRAN.                                                
           MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             
           MOVE '115A'                 TO PI-CURRENT-SCREEN-NO.         
                                                                        
           EXEC CICS RETURN                                             
               TRANSID (TRANS-ID)                                       
               COMMAREA(PROGRAM-INTERFACE-BLOCK)                        
               LENGTH  (WS-COMM-LENGTH)                                 
           END-EXEC.                                                    
                                                                        
       9200-RETURN-MAIN-MENU.

           IF CREDIT-SESSION
              MOVE XCTL-EL626          TO PGM-NAME
           ELSE
              IF CLAIM-SESSION
                 MOVE XCTL-EL126       TO PGM-NAME
              ELSE
                 IF GENERAL-LEDGER-SESSION
                    MOVE XCTL-GL800    TO PGM-NAME
                 END-IF
              END-IF
           END-IF

           GO TO 9300-XCTL
           .
       9300-XCTL.                                                       
           EXEC CICS XCTL                                               
               PROGRAM (PGM-NAME)                                       
               COMMAREA(PROGRAM-INTERFACE-BLOCK)                        
               LENGTH  (WS-COMM-LENGTH)                                 
           END-EXEC.                                                    
                                                                        
       9400-CLEAR.                                                      
           MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     
           GO TO 9300-XCTL.                                             
                                                                        
       9500-PF12.                                                       
           MOVE XCTL-EL010 TO PGM-NAME.                                 
           GO TO 9300-XCTL.                                             
                                                                        
       9600-PGMID-ERROR.                                                
           EXEC CICS HANDLE CONDITION                                   
               PGMIDERR(8300-SEND-TEXT)                                 
           END-EXEC.                                                    
                                                                        
           MOVE PGM-NAME      TO PI-CALLING-PROGRAM.                    
           MOVE ' '           TO PI-ENTRY-CD-1.                         
           MOVE XCTL-EL005    TO PGM-NAME.                              
           MOVE PGM-NAME      TO LOGOFF-PGM.                            
           MOVE PGMIDERR-MSG  TO LOGOFF-FILL.                           
           GO TO 9300-XCTL.                                             
                                                                        
       9700-LINK-DATE-CONVERT.

           EXEC CICS LINK
               PROGRAM    ('ELDATCV')
               COMMAREA   (DATE-CONVERSION-DATA)
               LENGTH     (DC-COMM-LENGTH)
           END-EXEC

           .
       9700-EXIT.                                                       
           EXIT.                                                        
                                                                        
       9900-ERROR-FORMAT.                                               
           IF NOT EMI-ERRORS-COMPLETE                                   
               MOVE LINK-EL001 TO PGM-NAME                              
               EXEC CICS LINK                                           
                   PROGRAM (PGM-NAME)                                   
                   COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)              
                   LENGTH  (EMI-COMM-LENGTH)                            
               END-EXEC.                                                
                                                                        
       9900-EXIT.                                                       
           EXIT.                                                        
                                                                        
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
              EXEC CICS  READQ TS                                  
                 QUEUE   (PI-SECURITY-TEMP-STORE-ID)              
                 INTO    (SECURITY-CONTROL)                       
                 LENGTH  (SC-COMM-LENGTH)                         
                 ITEM    (SC-ITEM-CL-CR)                          
              END-EXEC                                         

              MOVE SC-CREDIT-DISPLAY (29)                          
                                       TO PI-DISPLAY-CAP                
              MOVE SC-CREDIT-UPDATE  (29)                          
                                       TO PI-MODIFY-CAP                 

              IF NOT DISPLAY-CAP                                  
                 MOVE 'READ'           TO SM-READ
                 PERFORM 9995-SECURITY-VIOLATION
                                       THRU 9995-EXIT
                 MOVE ER-0070          TO  EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 GO TO 8100-SEND-INITIAL-MAP
              END-IF
           END-IF

           .
       9910-EXIT.                                                       
           EXIT.                                                        
                                                                        
       9990-ABEND.                                                      
           MOVE LINK-EL004             TO PGM-NAME.                     
           MOVE DFHEIBLK               TO EMI-LINE1.                    
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
                                                                        
