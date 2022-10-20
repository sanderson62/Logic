       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 EL614 .
      *                            VMOD=2.001.                          
      *                                                                 
      *                                                                 
      *AUTHOR.    PABLO.                                                
      *           COLLEYVILLE TX.                                     
                                                                        
      *DATE-COMPILED.                                                   
                                                                        
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF    CSO      IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************
                                                                        
      *REMARKS.                                                         
      *        THIS PROGRAM PROVIDES THE MAINTENANCE FUNCTIONS NEEDED   
      *    FOR THE  EOB   CODES IN THE CLAIM SYSTEM.
      *                                                                 
      *    SCREENS     - EL614A - EOB CODE MAINTENANCE
      *                                                                 
      *    ENTERED BY  - EL101 - SYSTEM ADMINISTRATION MENU             
      *                  EL156 - PAYMENT WORKSHEET
      *                  EL6317 - CERTIFICATE VERIFICATION                      
      *    EXIT TO     - EL101 - SYSTEM ADMINISTRATION MENU             
      *                  EL156 - PAYMENT WORKSHEET                      
      *                  EL6317 - CERTIFICATE VERIFICATION                      
      *    COMMAREA    - PASSED                                         
      *                                                                 
      *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON     
      *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE 
      *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
      *                  ENTRIES (XCTL FROM CICS VIA EX  ) THE SCREEN   
      *                  WILL BE READ AND ACTION WILL BE BASED ON THE   
      *                  MAINTENANCE TYPE INDICATED.                    
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 083110    2009122800001  PEMA  NEW PROGRAM
011811* 011811    2009122800001  AJRA  FIX SELECTION      
042211* 042211    2011040600001  PEMA  FIX ISSUE NO RECORDS FOUND
081511* 081511    2011022800001  PEMA  ADMIN SERV NAPERSOFT CHANGES
121012* 121012    2012101700002  AJRA  HIGHLIGHT REASON CODES
112113* 112113    2013090300001  AJRA  NAPERSOFT PHASE 2
120318* 120318  IR2018112800001  PEMA  Fix memory problem
      ******************************************************************

       ENVIRONMENT DIVISION.                                            
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
                                                                        
       77  FILLER  PIC X(32)  VALUE '********************************'. 
       77  FILLER  PIC X(32)  VALUE '*   EL614  WORKING STORAGE     *'. 
       77  FILLER  PIC X(32)  VALUE '***********VMOD=2.001 **********'. 
       77  i1                 pic s999  comp-3 value +0.
       77  S1                 PIC S999  COMP-3 VALUE +0.
121012 77  S2                 PIC S999  COMP-3 VALUE +0.
       77  E1                 PIC S999  COMP-3 VALUE +0.
       77  WS-DUP-EOB-SW      PIC X VALUE ' '.
           88  FOUND-DUP-EOB     VALUE 'Y'.

           COPY ELCSCTM.                                                
           COPY ELCSCRTY.                                               
                                                                        
       01  WS-PASSED-FROM-CALL-PGM     PIC X(60)  VALUE SPACES.
       01  WS-DATE-AREA.
           05  SAVE-DATE               PIC X(8)    VALUE SPACES.
           05  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.

       01  WS-SELECTED-EOB-CODES.
           05  WS-SELECTED-CODES OCCURS 12.
               10  WS-SEL-EOB-CODE     PIC XXXX.
               10  FILLER              PIC X.

       01  FILLER                          COMP-3.                      
           05  TIME-IN                     PIC S9(7)   VALUE ZERO.      
           05  TIME-OUT                    REDEFINES                    
               TIME-IN                     PIC S9(3)V9(4).              
                                                                        
       01  FILLER                          COMP SYNC.                   
           05  WS-INDEX                    PIC S9(4)   VALUE ZERO.      
           05  WS-JOURNAL-FILE-ID          PIC S9(4)   VALUE +1.        
           05  WS-JOURNAL-RECORD-LENGTH    PIC S9(4)   VALUE +773.      

112113 01  FILLER.
112113     05  QID.
112113         10  QID-TERM            PIC X(4).
112113         10  FILLER              PIC X(4)    VALUE '614A'.
112113     05  QID-ITEM                PIC S9(4)   COMP VALUE +0.
112113     05  NUM-LINES-PER-SCREEN    PIC 99      VALUE 17.
112113     05  TS-LENGTH               PIC S9(4)   VALUE +3360 COMP.
                                                                        
       01  ws-endbr-sw                 pic x value ' '.
           88  true-endbr               value 'Y'.
       01  FILLER.                                                      
           05  DEEDIT-FIELD        PIC  X(10).
           05  DEEDIT-FIELD-V0  REDEFINES
               DEEDIT-FIELD        PIC S9(10).
           05  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL              VALUE +00.
               88  RESP-ERROR               VALUE +01.
               88  RESP-NOTFND              VALUE +13.
               88  RESP-NOTOPEN             VALUE +19.
               88  RESP-ENDFILE             VALUE +20.
           05  WS-ELEOBC-KEY.
               10  WS-COMPANY-CD           PIC X      VALUE LOW-VALUES.
               10  WS-EOB-REC-TYPE         PIC X      VALUE LOW-VALUES.
               10  WS-EOB-CODE             PIC X(4)   VALUE LOW-VALUES.
               10  FILLER                  PIC X(9)   VALUE LOW-VALUES.
                                                                        
           05  WS-MAPSET-NAME              PIC X(8)  VALUE 'EL614S'.
           05  WS-MAP-NAME                 PIC X(8)  VALUE 'EL614A'.
                                                                        
           05  FILLER                      REDEFINES                    
               WS-MAP-NAME.                                             
               10  FILLER                  PIC XX.                      
               10  WS-MAP-NUMBER           PIC X(4).                    
               10  FILLER                  PIC XX.                      
                                                                        
           05  THIS-PGM                    PIC X(8)  VALUE 'EL614'.     
                                                                        
           05  WS-JOURNAL-TYPE-ID          PIC XX      VALUE 'EL'.      
                                                                        
           05  WS-TRANS-ID                 PIC X(4)    VALUE 'EXAI'.
                                                                        
           05  WS-TEMP-STORAGE-KEY.                                     
               10  WS-TS-TERM-ID           PIC X(4)    VALUE 'XXXX'.    
               10  FILLER                  PIC X(4)    VALUE '614'.

121012     05  WS-REASON-CODE-PASSED       PIC X(26).
121012     05  WS-TABLE-CODE.
121012         10  WS-TAB-CODE-1           PIC X(1).
121012         10  WS-TAB-CODE-2-4         PIC X(3).
121012     05  WS-SCREEN-CODE.
121012         10  WS-SCR-CODE-1-3         PIC X(3).
121012         10  WS-SCR-CODE-4           PIC X(1).

           05  WS-ERROR-MESSAGE-AREA.                                   
               10  ER-0004             PIC 9(4)   VALUE 0004.
               10  ER-0006             PIC 9(4)   VALUE 0006.
               10  ER-0008             PIC 9(4)   VALUE 0008.
               10  ER-0023             PIC 9(4)   VALUE 0023.
               10  ER-0029             PIC 9(4)   VALUE 0029.
112113         10  ER-0033             PIC 9(4)   VALUE 0033.
112113         10  ER-1698             PIC 9(4)   VALUE 1698.
112113         10  ER-1699             PIC 9(4)   VALUE 1699.

                                       COPY ELCINTF.
           12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                   
               16  PI-SELECTED-EOB-CODES.
                   20  PI-SELECTED-CODES OCCURS 12.
                       24  PI-EOB-CODE     PIC XXXX.
                       24  FILLER          PIC X.

               16  PI-1ST-TIME-SW          PIC S9     COMP-3.           
               16  PI-MODE                 PIC X.
                   88  ADD-FUNCTION         VALUE 'A'.
               16  PI-DN-RCODE             PIC XXXX.
               16  PI-DN-TOP-KEY           PIC X(15).
               16  PI-DN-BOT-KEY           PIC X(15).
               16  PI-DN-KEYS OCCURS 17.
                   20  PI-DN-KEY           PIC X(15).
               16  PI-LINE-COUNT           PIC S9(3)  COMP-3.           
               16  PI-BROWSE-SW            PIC S9     COMP-3.           
               16  PI-SHOW-SW              PIC S9     COMP-3.           
               16  PI-CHANGE-SW            PIC S9     COMP-3.
               16  PI-RECORD-TYPE          PIC X.
                                                                        
112113         16  pi-FILLER               PIC X(244).
112113         16  PI-TOTAL-LINES          PIC S9(3)   comp-3.
112113         16  PI-CURRENT-LINE         PIC S9(3)   COMP-3.
112113         16  PI-TOP-LINE-NUM         PIC S9(3)   COMP-3.
112113         16  PI-BOT-LINE-NUM         PIC S9(3)   COMP-3.
112113         16  PI-TEMP-STOR-ITEMS      PIC S9(4)   COMP.
121012         16  PI-REASON-CODE-IND.
121012             20 PI-REASON-CODE-FLAG  OCCURS 26 TIMES PIC X(1).
121012         16  PI-EXPANDED-VIEW        PIC X(1).
               16  pi-6318-pass            PIC X.
               16  filler                  pic x.

                                       COPY EL614S.

       01  EL614AO-R REDEFINES EL614AI.
           05  FILLER                      PIC X(37).
           05  WS-MAP-LINE                 OCCURS 17
121012                                     INDEXED BY M1 M2 N1.
               10  SELL                    PIC S9(4)  COMP.
               10  SELA                    PIC X.
               10  SELI                    PIC X.
               10  RCODEL                  PIC S9(4)  COMP.             
               10  RCODEA                  PIC X.                       
               10  RCODEO                  PIC XXXX.
               10  RCODEI                  REDEFINES                    
                   RCODEO                  PIC XXXX.
               10  DESCL                   PIC S9(4)   COMP.            
               10  DESCA                   PIC X.                       
               10  DESCO                   PIC X(60).
               10  DESCI                   REDEFINES                    
                   DESCO                   PIC X(60).
121012     05  FILLER                      PIC X(137).

                                       COPY ELCJPFX.
                                       PIC X(750).

                                       COPY ELCEOBC.
                                       COPY ELCEMIB.
                                       COPY ELCDATE.
                                       COPY ELCLOGOF.
                                       COPY ELCATTR.
                                       COPY ELCAID.
       01  FILLER    REDEFINES DFHAID.                                  
           05  FILLER                      PIC X(8).                    
           05  PF-VALUES                   PIC X                        
               OCCURS 24 TIMES.                                         

112113 01  TS-WORK-AREA                PIC X(3348).
                                                                        
       LINKAGE SECTION.                                                 
       01  DFHCOMMAREA                 PIC X(1024).

       PROCEDURE DIVISION.                                              
                                                                        
           MOVE EIBDATE               TO DC-JULIAN-YYDDD.               
           MOVE '5'                   TO DC-OPTION-CODE.                
           PERFORM 8500-DATE-CONVERSION
                                      THRU 8500-EXIT
           MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    
           MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                
                                                                        
           MOVE DFHCOMMAREA           TO  PROGRAM-INTERFACE-BLOCK.      
                                                                        
           MOVE +1                    TO  EMI-NUMBER-OF-LINES           
                                          EMI-SWITCH2.                  
                                                                        
           IF EIBCALEN NOT > ZERO
              MOVE UNACCESS-MSG        TO LOGOFF-MSG
              GO TO 8300-SEND-TEXT
           END-IF
                                                                        
           EXEC CICS HANDLE CONDITION
               PGMIDERR (9600-PGMIDERR)
               ERROR    (9990-ERROR)
           END-EXEC

           IF PI-CALLING-PROGRAM NOT = THIS-PGM
              IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
                 MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
                 MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
                 MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
                 MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
                 MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
                 MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
                 MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
                 MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
              ELSE
                 MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
                 MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
                 MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
                 MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
                 MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
                 MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
                 MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
                 MOVE SPACES               TO  PI-SAVED-PROGRAM-6
              END-IF
           END-IF

           IF EIBTRNID NOT = WS-TRANS-ID
              GO TO 1000-INITIAL-SCREEN
           END-IF

           IF EIBAID = DFHCLEAR                                         
              GO TO 9400-CLEAR
           END-IF

           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
              MOVE LOW-VALUES          TO EL614AO
              MOVE -1                  TO APFKL
              MOVE ER-0008             TO EMI-ERROR
              GO TO 8200-SEND-DATAONLY
           END-IF

           EXEC CICS RECEIVE
               INTO   (EL614AO)
               MAPSET (WS-MAPSET-NAME)
               MAP    (WS-MAP-NAME)
           END-EXEC

           IF APFKL > ZERO
              IF EIBAID NOT = DFHENTER
                 MOVE ER-0004          TO EMI-ERROR
                 MOVE AL-UNBOF         TO APFKA
                 MOVE -1               TO APFKL
                 GO TO 8200-SEND-DATAONLY
              ELSE
                 IF APFKO NUMERIC AND
                    (APFKO > ZERO AND LESS '25')
                    MOVE PF-VALUES (APFKI)
                                       TO  EIBAID
                 ELSE
                    MOVE ER-0029       TO  EMI-ERROR
                    MOVE AL-UNBOF      TO  APFKA
                    MOVE -1            TO  APFKL
                    GO TO 8200-SEND-DATAONLY
                 END-IF
              END-IF
           END-IF

           IF EIBAID = DFHPF12
              MOVE 'EL010'             TO THIS-PGM
              GO TO 9300-XCTL
           END-IF

           IF EIBAID = DFHPF23
              GO TO 9000-RETURN-CICS
           END-IF

           IF EIBAID = DFHPF24
              IF CREDIT-SESSION
                 MOVE 'EL626'          TO THIS-PGM
                 GO TO 9300-XCTL
              ELSE
                 MOVE 'EL126'          TO THIS-PGM
                 GO TO 9300-XCTL
              END-IF
           END-IF

           IF EIBAID = DFHENTER OR DFHPF1 OR DFHPF2
121012        OR DFHPF3 OR DFHPF4 OR DFHPF5
              CONTINUE
           ELSE
              MOVE ER-0008             TO EMI-ERROR
              MOVE -1                  TO APFKL
              GO TO 8200-SEND-DATAONLY
           END-IF

           IF EIBAID = DFHPF1 OR DFHPF2 OR DFHPF3
              OR DFHPF4 OR DFHENTER OR DFHPF5
              MOVE 'S'                 TO PI-MODE
           ELSE
              MOVE AL-UABOF            TO APFKA
              MOVE -1                  TO APFKL
              MOVE ER-0023             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
           END-IF

           IF EMI-FATAL-CTR > ZERO
              GO TO 8200-SEND-DATAONLY
           END-IF

           IF PI-MODE EQUAL 'S'                                         
              GO TO 2000-PROCESS-SHOW
           END-IF

           MOVE 'LOGIC ERROR HAS OCCURRED - PROGRAM EL614'              
                                       TO LOGOFF-MSG
           GO TO 8300-SEND-TEXT

           .
       1000-INITIAL-SCREEN.

           MOVE PI-PROGRAM-WORK-AREA (1:60)
                                       TO WS-PASSED-FROM-CALL-PGM
121012     MOVE PI-REASON-CODE-IND     TO WS-REASON-CODE-PASSED

           EVALUATE TRUE
              WHEN PI-RETURN-TO-PROGRAM = 'EL156'
                 MOVE WS-PASSED-FROM-CALL-PGM
                                       TO PI-PROGRAM-WORK-AREA
                 MOVE '1'              TO PI-RECORD-TYPE
              WHEN PI-RETURN-TO-PROGRAM = 'EL6316'
                 MOVE WS-PASSED-FROM-CALL-PGM
                                       TO PI-PROGRAM-WORK-AREA
                 MOVE '4'              TO PI-RECORD-TYPE
              WHEN PI-RETURN-TO-PROGRAM = 'EL6317'
                 MOVE WS-PASSED-FROM-CALL-PGM
                                       TO PI-PROGRAM-WORK-AREA
                 MOVE '2'              TO PI-RECORD-TYPE
              WHEN PI-RETURN-TO-PROGRAM = 'EL6318'
                 MOVE WS-PASSED-FROM-CALL-PGM
                                       TO PI-PROGRAM-WORK-AREA
121012           MOVE WS-REASON-CODE-PASSED TO PI-REASON-CODE-IND
121012           MOVE 'N'              TO PI-EXPANDED-VIEW
                 MOVE '3'              TO PI-RECORD-TYPE
              WHEN OTHER
                 MOVE SPACES           TO PI-PROGRAM-WORK-AREA
                                          WS-PASSED-FROM-CALL-PGM
                                          PI-RECORD-TYPE
           END-EVALUATE

112113     MOVE PI-COMPANY-CD          TO WS-ELEOBC-KEY
           MOVE PI-RECORD-TYPE         TO WS-EOB-REC-TYPE

           MOVE ZERO                   TO PI-1ST-TIME-SW
                                          PI-LINE-COUNT
                                          PI-BROWSE-SW
                                          PI-SHOW-SW
                                          PI-CHANGE-SW

           MOVE LOW-VALUES             TO EL614AO

112113     GO TO 7000-BUILD-TABLE

           .
       2000-PROCESS-SHOW.

      **  THE FIRST THING TO DO IS CHECK
      **  FOR PF4 (CLEAR SELECTED CODES) IF PRESSED
      **  SPACE OUT THE PRIOR SELECTIONS. NEXT CHECK
      **  THE SELECTED CODES 
      **  AGAINST THE TABLE AND SYNC THEM UP.
      **  IF THEY SPACE ONE OUT THEN GET RID OF IT FROM THE 
      **  TABLE

           IF EIBAID = DFHPF4
              MOVE SPACES              TO PI-SELECTED-EOB-CODES
              move pi-dn-TOP-key    to WS-ELEOBC-KEY
              GO TO 7000-browse-fwd
           END-IF
121012
121012     IF EIBAID = DFHPF5
121012         IF PI-EXPANDED-VIEW = 'Y'
121012             MOVE 'N'        TO PI-EXPANDED-VIEW
121012         ELSE
121012             MOVE 'Y'        TO PI-EXPANDED-VIEW
121012         END-IF
               move pi-dn-TOP-key    to WS-ELEOBC-KEY
               GO TO 7000-browse-fwd
121012     END-IF

           IF PI-RETURN-TO-PROGRAM = 'EL156' OR 'EL6317' OR 'EL6318'
              OR 'EL6316'
              PERFORM VARYING M1 FROM +1 BY +1 UNTIL
                 M1 > +17
                 IF SELL (M1) > +0
                    IF SELI (M1) = SPACES
                       PERFORM VARYING E1 FROM +1 BY +1 UNTIL
                          E1 > +12
                          IF PI-EOB-CODE (E1) = RCODEI (M1)
                             MOVE SPACES TO PI-EOB-CODE (E1)
                          END-IF
                       END-PERFORM
                    ELSE
                       MOVE ' '       TO WS-DUP-EOB-SW
                       PERFORM VARYING E1 FROM +1 BY +1 UNTIL
                          E1 > +12
011811                    IF RCODEI (M1) = PI-EOB-CODE (E1)
                             SET FOUND-DUP-EOB TO TRUE
                          END-IF
                       END-PERFORM
                       IF NOT FOUND-DUP-EOB
                          PERFORM VARYING E1 FROM +1 BY +1 UNTIL
                             (PI-EOB-CODE (E1) = SPACES)
                             OR (E1 > +12)
                          END-PERFORM
                          IF E1 < +13
                             MOVE RCODEI (M1) TO PI-EOB-CODE (E1)
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-PERFORM
           END-IF

           EVALUATE TRUE
              WHEN EIBAID = DFHPF2
112113           MOVE PI-TOP-LINE-NUM TO PI-BOT-LINE-NUM
112113                                   PI-CURRENT-LINE
                 move pi-dn-top-key    to WS-ELEOBC-KEY
                 GO TO 7150-browse-bwd
              WHEN EIBAID = DFHPF1
112113           MOVE PI-BOT-LINE-NUM TO PI-TOP-LINE-NUM
112113                                   PI-CURRENT-LINE
                 move pi-dn-bot-key    to WS-ELEOBC-KEY
                 GO TO 7000-browse-fwd
              WHEN EIBAID = DFHPF3
                 PERFORM 6100-FORMAT-EOB-CODES
                                       THRU 6100-EXIT
                 GO TO 9400-CLEAR
              
              WHEN OTHER
      *          MOVE PI-COMPANY-CD    TO WS-ELEOBC-KEY
      *          GO TO 7000-BROWSE-FWD
                 MOVE -1 TO SELL (1)
                 GO TO 8200-SEND-DATAONLY
           END-EVALUATE

           .
       6100-FORMAT-EOB-CODES.

      **  THIS ROUTINE JUST ELIMINATES ANY EOB CODES THAT
      **  WERE "UN SELECTED"

           MOVE SPACES                 TO WS-SELECTED-EOB-CODES
           MOVE +1                     TO E1 S1
           PERFORM UNTIL E1 > +12
              IF PI-EOB-CODE (E1) NOT = SPACES
                 MOVE PI-EOB-CODE (E1) TO WS-SEL-EOB-CODE (S1)
                 ADD +1                TO S1
              END-IF
              ADD +1                   TO E1
           END-PERFORM
           MOVE WS-SELECTED-EOB-CODES  TO PI-SELECTED-EOB-CODES

           .
       6100-EXIT.
           EXIT.
           
112113
112113 7000-BUILD-TABLE.

112113     MOVE ZEROS                  TO  PI-TOTAL-LINES
112113                                     PI-CURRENT-LINE
112113                                     PI-TEMP-STOR-ITEMS
                                           i1
112113     MOVE ZERO                   TO PI-LINE-COUNT
112113     MOVE +1                     TO PI-BROWSE-SW
           move ' '                    to pi-6318-pass

112113****IF TEMP STORAGE EXISTS, DELETE IT.
112113     PERFORM 7500-READ-TS THRU 7599-EXIT
112113
112113     IF PI-TEMP-STOR-ITEMS NOT = ZERO
112113        PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT
112113     END-IF.

       7000-browse-fwd.

           set m1 to +1
           move +1 to pi-browse-sw
           move low-values to el614ao
112113
112113     EXEC CICS STARTBR
112113         DATASET   ('ELEOBC')
112113         RIDFLD    (WS-ELEOBC-KEY)
112113         GTEQ
112113         RESP      (WS-RESPONSE)
112113     END-EXEC.
112113
112113     EVALUATE TRUE
112113        WHEN RESP-ENDFILE
112113           GO TO 7030-ENDBR
112113        WHEN RESP-NORMAL
112113           CONTINUE
112113        WHEN RESP-NOTFND
112113           GO TO 7030-ENDBR
112113     END-EVALUATE.
112113
112113***IF COMING FROM EL6318 WE WANT TO LOAD THE HIGHLIGHTED REASON CODES FIRST
112113***FOLLOWED BY THE NON HIGHLIGHTED. FOR ALL OTHER SCREENS WE JUST LOAD ALL
112113***REASON CODES IN ORDER.
112113     IF PI-RETURN-TO-PROGRAM NOT EQUAL 'EL6318'
112113         GO TO 7020-LOOP
112113     END-IF.

           perform varying s2 from +1 by +1 until
              (s2 > +26)
              or (WS-EOB-CODE(1:1) = pi-reason-code-flag (s2))
           end-perform
           if s2 <= +26
              move '1'                 to pi-6318-pass
           else
              move '2'                 to pi-6318-pass
           end-if

           if pi-6318-pass = '2'
              go to 7010-NON-HIGHLIGHT-LOOP
           end-if

           .
112113 7001-HIGHLIGHT-LOOP.
112113     EXEC CICS READNEXT
112113         INTO    (EOB-CODES)
112113         DATASET ('ELEOBC')
112113         RIDFLD  (WS-ELEOBC-KEY)
112113         RESP    (WS-RESPONSE)
112113     END-EXEC
112113
112113     EVALUATE TRUE
112113        WHEN RESP-ENDFILE
112113           GO TO 7005-ENDBR
112113        WHEN (PI-COMPANY-CD NOT = WS-COMPANY-CD)
112113           OR ((PI-RECORD-TYPE NOT = EO-RECORD-TYPE)
112113               AND (PI-RECORD-TYPE NOT = SPACES))
112113           GO TO 7005-ENDBR
112113        WHEN RESP-NORMAL
112113           CONTINUE
112113        WHEN RESP-NOTFND
112113           GO TO 7005-ENDBR
112113        WHEN EO-COMPANY-CD NOT = PI-COMPANY-CD
112113           GO TO 7005-ENDBR
112113     END-EVALUATE
112113
112113     PERFORM VARYING S2 FROM +1 BY +1 UNTIL
112113        (S2 > +26)
              or (eo-eob-code (1:1) = pi-reason-code-flag (s2))
           end-perform

           if s2 > +26
              go to 7001-highlight-loop
           end-if

           MOVE eo-eob-code            TO WS-TABLE-CODE
           MOVE WS-TAB-CODE-1          TO WS-SCR-CODE-4
           MOVE WS-TAB-CODE-2-4        TO WS-SCR-CODE-1-3
           MOVE WS-SCREEN-CODE         TO RCODEO (M1)
           MOVE eo-DESCRIPTION         TO DESCO (M1)
           MOVE AL-SABON               TO RCODEA (M1)
           MOVE AL-SABOF               TO DESCA (M1)

           PERFORM VARYING E1 FROM +1 BY +1 UNTIL
              E1 > +12
              MOVE PI-EOB-CODE (E1)    TO WS-SCREEN-CODE
              MOVE WS-SCR-CODE-1-3     TO WS-TAB-CODE-2-4
              MOVE WS-SCR-CODE-4       TO WS-TAB-CODE-1
              IF eo-EOB-CODE  = WS-TABLE-CODE
                 MOVE 'S'              TO SELI (M1)
                 MOVE +1               TO SELL (M1)
              END-IF
           END-PERFORM

           if m1 = +1
              move ws-eleobc-key to pi-dn-top-key
           else
              move ws-eleobc-key to pi-dn-bot-key
           end-if

           IF PI-EXPANDED-VIEW = 'Y' 
               IF eo-DESCRIPTION (61:60) > SPACES 
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (61:60) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
               IF eo-DESCRIPTION (121:60) > SPACES 
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (121:60) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
               IF eo-DESCRIPTION (181:60) > SPACES
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (181:60) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
               IF Eo-DESCRIPTION (241:35) > SPACES
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (241:35) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
           END-IF

           if m1 < num-lines-per-screen
              set m1 up by +1
              go to 7001-HIGHLIGHT-LOOP
           end-if

           .
112113 7005-ENDBR.

           if m1 = num-lines-per-screen   *> filled up the screen
              go to 7030-endbr
           end-if

      *** did not fill up screen, now load non highlighted stuff

112113     EXEC CICS ENDBR
112113         DATASET ('ELEOBC')
112113     END-EXEC

           move '2'                    to pi-6318-pass
112113
112113     MOVE LOW-VALUES             TO WS-ELEOBC-KEY
112113     MOVE PI-COMPANY-CD          TO WS-ELEOBC-KEY
112113     MOVE PI-RECORD-TYPE         TO WS-EOB-REC-TYPE
112113     EXEC CICS STARTBR
112113         DATASET   ('ELEOBC')
112113         RIDFLD    (WS-ELEOBC-KEY)
112113         GTEQ
112113         RESP      (WS-RESPONSE)
112113     END-EXEC.
112113
112113     EVALUATE TRUE
112113        WHEN RESP-ENDFILE
112113           GO TO 7030-ENDBR
112113        WHEN RESP-NORMAL
112113           CONTINUE
112113        WHEN RESP-NOTFND
112113           GO TO 7030-ENDBR
112113     END-EVALUATE.
112113
112113 7010-NON-HIGHLIGHT-LOOP.
112113     EXEC CICS READNEXT
112113         INTO    (EOB-CODES)
112113         DATASET ('ELEOBC')
112113         RIDFLD  (WS-ELEOBC-KEY)
112113         RESP    (WS-RESPONSE)
112113     END-EXEC
112113
112113     EVALUATE TRUE
112113        WHEN RESP-ENDFILE
112113           GO TO 7030-ENDBR
112113        WHEN (PI-COMPANY-CD NOT = WS-COMPANY-CD)
112113           OR ((PI-RECORD-TYPE NOT = EO-RECORD-TYPE)
112113               AND (PI-RECORD-TYPE NOT = SPACES))
112113           GO TO 7030-ENDBR
112113        WHEN RESP-NORMAL
112113           CONTINUE
112113        WHEN RESP-NOTFND
112113           GO TO 7030-ENDBR
112113        WHEN EO-COMPANY-CD NOT = PI-COMPANY-CD
112113           GO TO 7030-ENDBR
112113     END-EVALUATE
112113
112113     PERFORM VARYING S2 FROM +1 BY +1 UNTIL
112113        (S2 > +26)
              or EO-EOB-CODE (1:1) = PI-REASON-CODE-FLAG (S2)
112113     END-PERFORM

           if s2 <= +26  *> found one previously loaded on screen
              go to 7010-non-highlight-loop
           end-if

           MOVE eo-eob-code            TO WS-TABLE-CODE
           MOVE WS-TAB-CODE-1          TO WS-SCR-CODE-4
           MOVE WS-TAB-CODE-2-4        TO WS-SCR-CODE-1-3
           MOVE WS-SCREEN-CODE         TO RCODEO (M1)
           MOVE eo-DESCRIPTION         TO DESCO (M1)

           PERFORM VARYING E1 FROM +1 BY +1 UNTIL
              E1 > +12
              MOVE PI-EOB-CODE (E1)    TO WS-SCREEN-CODE
              MOVE WS-SCR-CODE-1-3     TO WS-TAB-CODE-2-4
              MOVE WS-SCR-CODE-4       TO WS-TAB-CODE-1
              IF eo-EOB-CODE  = WS-TABLE-CODE
                 MOVE 'S'              TO SELI (M1)
                 MOVE +1               TO SELL (M1)
              END-IF
           END-PERFORM

           if m1 = +1
              move ws-eleobc-key to pi-dn-top-key
           else
              move ws-eleobc-key to pi-dn-bot-key
           end-if

           IF PI-EXPANDED-VIEW = 'Y' 
               IF eo-DESCRIPTION (61:60) > SPACES 
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (61:60) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
               IF eo-DESCRIPTION (121:60) > SPACES 
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (121:60) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
               IF eo-DESCRIPTION (181:60) > SPACES
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (181:60) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
               IF Eo-DESCRIPTION (241:35) > SPACES
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (241:35) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
           END-IF

           if m1 < num-lines-per-screen
              set m1 up by +1
              go to 7010-NON-HIGHLIGHT-LOOP
           end-if

      ***  filled up the screen
           go to 7030-endbr

           .
112113 7020-LOOP.

112113     EXEC CICS READNEXT
112113         INTO    (EOB-CODES)
112113         DATASET ('ELEOBC')
112113         RIDFLD  (WS-ELEOBC-KEY)
112113         RESP    (WS-RESPONSE)
112113     END-EXEC
112113
112113     EVALUATE TRUE
112113        WHEN RESP-ENDFILE
112113           GO TO 7030-ENDBR
112113        WHEN (PI-COMPANY-CD NOT = WS-COMPANY-CD)
112113           OR ((PI-RECORD-TYPE NOT = EO-RECORD-TYPE)
112113               AND (PI-RECORD-TYPE NOT = SPACES))
112113           GO TO 7030-ENDBR
112113        WHEN RESP-NORMAL
112113           CONTINUE
112113        WHEN RESP-NOTFND
112113           GO TO 7030-ENDBR
112113        WHEN EO-COMPANY-CD NOT = PI-COMPANY-CD
112113           GO TO 7030-ENDBR
112113     END-EVALUATE

           IF PI-RETURN-TO-PROGRAM = 'EL6318'
               MOVE eo-eob-code        TO WS-TABLE-CODE
               MOVE WS-TAB-CODE-1      TO WS-SCR-CODE-4
               MOVE WS-TAB-CODE-2-4    TO WS-SCR-CODE-1-3
               MOVE WS-SCREEN-CODE     TO RCODEO (M1)
           ELSE
               MOVE eo-eob-code        TO RCODEO (M1)
           END-IF
           MOVE eo-DESCRIPTION         TO DESCO (M1)
      
      ** Highlighting done here I think

           IF PI-RETURN-TO-PROGRAM = 'EL6318'
              PERFORM VARYING S2 FROM +1 BY +1 UNTIL
                S2 > +26
                  IF RCODEO (M1)(4:1) = PI-REASON-CODE-FLAG (S2)
                     MOVE AL-SABON TO RCODEA (M1)
                     MOVE AL-SABOF TO DESCA (M1)
                  END-IF
              END-PERFORM
           END-IF

           IF PI-RETURN-TO-PROGRAM = 'EL156' OR 'EL6317'
              OR 'EL6316'
              PERFORM VARYING E1 FROM +1 BY +1 UNTIL
                 E1 > +12
                 IF EO-EOB-CODE = PI-EOB-CODE (E1)
                    MOVE 'S'           TO SELI (M1)
                    MOVE +1            TO SELL (M1)
                 END-IF
              END-PERFORM
           END-IF
      
           IF PI-RETURN-TO-PROGRAM = 'EL6318'
              PERFORM VARYING E1 FROM +1 BY +1 UNTIL
                 E1 > +12
                 MOVE PI-EOB-CODE (E1) TO WS-SCREEN-CODE
                 MOVE WS-SCR-CODE-1-3  TO WS-TAB-CODE-2-4
                 MOVE WS-SCR-CODE-4    TO WS-TAB-CODE-1
                 IF eo-EOB-CODE  = WS-TABLE-CODE
                    MOVE 'S'           TO SELI (M1)
                    MOVE +1            TO SELL (M1)
                 END-IF
              END-PERFORM
           END-IF
      
           if m1 = +1
              move ws-eleobc-key to pi-dn-top-key
           else
              move ws-eleobc-key to pi-dn-bot-key
           end-if

           IF PI-EXPANDED-VIEW = 'Y' 
               IF eo-DESCRIPTION (61:60) > SPACES 
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (61:60) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
               IF eo-DESCRIPTION (121:60) > SPACES 
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (121:60) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
               IF eo-DESCRIPTION (181:60) > SPACES
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (181:60) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
               IF Eo-DESCRIPTION (241:35) > SPACES
                 AND M1 < +17
                   SET N1 TO M1
                   SET M1 UP BY +1
                   MOVE eo-DESCRIPTION (241:35) TO DESCO (M1)
                   MOVE DESCA (N1)    TO DESCA (M1)
                   MOVE AL-SADOF      TO SELA (M1)
               END-IF
           END-IF

           if m1 < num-lines-per-screen
              set m1 up by +1
              go to 7020-loop
           end-if

           .
112113 7030-ENDBR.

           IF PI-BROWSE-SW = +1
              EXEC CICS ENDBR
                 DATASET ('ELEOBC')
              END-EXEC
              MOVE +0                  TO PI-BROWSE-SW
           END-IF

           IF m1 = 0
               MOVE ER-0006            TO EMI-ERROR
               MOVE -1                 TO APFKL
               MOVE AL-UABOF           TO APFKA
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               MOVE ZEROS              TO PI-TOTAL-LINES
               GO TO 8100-SEND-INITIAL-MAP
           END-IF.

           IF m1 < num-lines-per-screen
              MOVE ER-1699             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           END-IF.

           go to 8100-send-initial-map

           .
       7150-browse-bwd.

           set m1 to num-lines-per-screen
           move +1 to pi-browse-sw
           move low-values to el614ao

           EXEC CICS STARTBR
               DATASET   ('ELEOBC')
               RIDFLD    (WS-ELEOBC-KEY)
               GTEQ
               RESP      (WS-RESPONSE)
           END-EXEC.

       7150-check-startbr.

           EVALUATE TRUE
              WHEN RESP-ENDFILE
                 GO TO 7180-ENDBR
              WHEN RESP-NORMAL
                 CONTINUE
              WHEN RESP-NOTFND
                 GO TO 7180-ENDBR
           END-EVALUATE.

      *** 6318-pass is just if we came from 6318
      *** pass one = highligted ones, pass two = all others

           IF PI-RETURN-TO-PROGRAM = 'EL6318'
              PERFORM VARYING S2 FROM +1 BY +1 UNTIL
                (S2 > +26)
                 or (ws-eob-code(1:1) = PI-REASON-CODE-FLAG (S2))
              END-PERFORM
              if s2 <= +26
                 move '1'              to pi-6318-pass
              end-if
           END-IF

           .
       7160-LOOP.

           EXEC CICS READPREV
               INTO    (EOB-CODES)
               DATASET ('ELEOBC')
               RIDFLD  (WS-ELEOBC-KEY)
               RESP    (WS-RESPONSE)
           END-EXEC
      
           EVALUATE TRUE
              WHEN RESP-ENDFILE
                 set true-endbr to true
                 GO TO 7180-ENDBR
              WHEN (PI-COMPANY-CD NOT = WS-COMPANY-CD)
                 OR ((PI-RECORD-TYPE NOT = EO-RECORD-TYPE)
                     AND (PI-RECORD-TYPE NOT = SPACES))
                 set true-endbr to true
                 GO TO 7180-ENDBR
              WHEN EO-COMPANY-CD NOT = PI-COMPANY-CD
                 set true-endbr to true
                 GO TO 7180-ENDBR
              WHEN RESP-NORMAL
                 CONTINUE
              WHEN RESP-NOTFND
                 set true-endbr to true
                 GO TO 7180-ENDBR
           END-EVALUATE

           IF PI-RETURN-TO-PROGRAM = 'EL6318'
              PERFORM VARYING S2 FROM +1 BY +1 UNTIL
                (S2 > +26)
                 or (eo-eob-code(1:1) = PI-REASON-CODE-FLAG (S2))
              END-PERFORM
              if s2 <= +26
                 if pi-6318-pass = '1'
                    continue
                 else
                    go to 7160-loop
                 end-if
              else
                 if pi-6318-pass = '2'
                    continue
                 else
                    go to 7160-loop
                 end-if
              end-if
           end-if

           if m1 = num-lines-per-screen
              move ws-eleobc-key to pi-dn-bot-key
           else
              move ws-eleobc-key to pi-dn-top-key
           end-if

           IF PI-EXPANDED-VIEW = 'Y'
              EVALUATE TRUE
                WHEN EO-DESCRIPTION (241:35) > SPACES
                  IF M1 < +5
                      GO TO 7180-ENDBR
                  ELSE
                      SET M1 DOWN BY +4
                  END-IF
                WHEN EO-DESCRIPTION (181:60) > SPACES
                  IF M1 < +4
                      GO TO 7180-ENDBR
                  ELSE
                      SET M1 DOWN BY +3
                  END-IF
                WHEN EO-DESCRIPTION (121:60) > SPACES
                  IF M1 < +3
                      GO TO 7180-ENDBR
                  ELSE
                      SET M1 DOWN BY +2
                  END-IF
                WHEN EO-DESCRIPTION (61:60) > SPACES
                  IF M1 < +2
                      GO TO 7180-ENDBR
                  ELSE
                      SET M1 DOWN BY +1
                  END-IF
              END-EVALUATE
           END-IF

           IF PI-RETURN-TO-PROGRAM = 'EL6318'
               MOVE eo-eob-code        TO WS-TABLE-CODE
               MOVE WS-TAB-CODE-1      TO WS-SCR-CODE-4
               MOVE WS-TAB-CODE-2-4    TO WS-SCR-CODE-1-3
               MOVE WS-SCREEN-CODE     TO RCODEO (M1)
           ELSE
               MOVE eo-eob-code        TO RCODEO (M1)
           END-IF
           MOVE eo-DESCRIPTION         TO DESCO (M1)
           IF PI-RETURN-TO-PROGRAM = 'EL6318'
              PERFORM VARYING S2 FROM +1 BY +1 UNTIL
                S2 > +26
                  IF RCODEO (M1)(4:1) = PI-REASON-CODE-FLAG (S2)
                     MOVE AL-SABON TO RCODEA (M1)
                     MOVE AL-SABOF TO DESCA (M1)
                  END-IF
              END-PERFORM
           END-IF

           IF PI-RETURN-TO-PROGRAM = 'EL156' OR 'EL6317'
              OR 'EL6316'
              PERFORM VARYING E1 FROM +1 BY +1 UNTIL
                 E1 > +12
                 IF eo-EOB-CODE = PI-EOB-CODE (E1)
                    MOVE 'S'           TO SELI (M1)
                    MOVE +1            TO SELL (M1)
                 END-IF
              END-PERFORM
           END-IF
      
           IF PI-RETURN-TO-PROGRAM = 'EL6318'
              PERFORM VARYING E1 FROM +1 BY +1 UNTIL
                 E1 > +12
                 MOVE PI-EOB-CODE (E1) TO WS-SCREEN-CODE
                 MOVE WS-SCR-CODE-1-3  TO WS-TAB-CODE-2-4
                 MOVE WS-SCR-CODE-4    TO WS-TAB-CODE-1
                 IF eo-EOB-CODE = WS-TABLE-CODE
                    MOVE 'S'           TO SELI (M1)
                    MOVE +1            TO SELL (M1)
                 END-IF
              END-PERFORM
           END-IF

           SET M2 TO M1
           IF PI-EXPANDED-VIEW = 'Y'
              IF EO-DESCRIPTION (61:60) > SPACES
                 SET N1 TO M1
                 SET M1 UP BY +1
                 MOVE EO-DESCRIPTION (61:60) TO DESCO(M1)
                 MOVE DESCA (N1)       TO DESCA (M1)
                 MOVE AL-SADOF         TO SELA (M1)
              END-IF
              IF EO-DESCRIPTION (121:60) > SPACES
                 SET N1 TO M1
                 SET M1 UP BY +1
                 MOVE EO-DESCRIPTION (121:60) TO DESCO(M1)
                 MOVE DESCA (N1)       TO DESCA (M1)
                 MOVE AL-SADOF         TO SELA (M1)
              END-IF
              IF EO-DESCRIPTION (181:60) > SPACES
                 SET N1 TO M1
                 SET M1 UP BY +1
                 MOVE EO-DESCRIPTION (181:60) TO DESCO(M1)
                 MOVE DESCA (N1)       TO DESCA (M1)
                 MOVE AL-SADOF         TO SELA (M1)
              END-IF
              IF EO-DESCRIPTION (241:35) > SPACES
                 SET N1 TO M1
                 SET M1 UP BY +1
                 MOVE EO-DESCRIPTION (241:35) TO DESCO(M1)
                 MOVE DESCA (N1)       TO DESCA (M1)
                 MOVE AL-SADOF         TO SELA (M1)
              END-IF
           END-IF
           SET M1 TO M2

           if m1 > +1
              set m1 down by +1
              go to 7160-loop
           end-if

           .
112113 7180-ENDBR.
           
           if pi-return-to-program = 'EL6318'
              if pi-6318-pass = '2'
                 and true-endbr
                 if m1 > +1
                    move '1' to pi-6318-pass
                    EXEC CICS ENDBR
                       DATASET ('ELEOBC')
                    END-EXEC
                    MOVE +0            TO PI-BROWSE-SW
                    move '4' to ws-eob-rec-type
                    move spaces to ws-eob-code
                    EXEC CICS STARTBR
                        DATASET   ('ELEOBC')
                        RIDFLD    (WS-ELEOBC-KEY)
                        LTEQ
                        RESP      (WS-RESPONSE)
                    END-EXEC
                    move ' ' to ws-endbr-sw
                    go to 7150-check-startbr
                 end-if
              end-if
           end-if

           IF PI-BROWSE-SW = +1
              EXEC CICS ENDBR
                 DATASET ('ELEOBC')
              END-EXEC
              MOVE +0                  TO PI-BROWSE-SW
           END-IF
      
           IF m1 = 0
               MOVE ER-0006            TO EMI-ERROR
               MOVE -1                 TO APFKL
               MOVE AL-UABOF           TO APFKA
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               MOVE ZEROS              TO PI-TOTAL-LINES
               GO TO 8100-SEND-INITIAL-MAP
           END-IF.

           if true-endbr
              and pi-6318-pass not = '2'
              MOVE ER-1698             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           end-if

           IF m1 > +1
              move pi-dn-top-key to ws-eleobc-key
              go to 7000-browse-fwd
           END-IF

           go to 8100-send-initial-map

           .
112113 7250-DELETE-TEMP-STOR.
112113     EXEC CICS HANDLE CONDITION
112113          QIDERR(7299-EXIT)
112113     END-EXEC.
112113     EXEC CICS DELETEQ TS
112113          QUEUE(QID)
112113     END-EXEC.
112113 7299-EXIT.
112113     EXIT.
112113
112113 7500-READ-TS.
112113     EXEC CICS HANDLE CONDITION
112113          QIDERR(7590-TS-QIDERR)
112113          ITEMERR(7585-QID-ITEMERR)
112113     END-EXEC.
112113     MOVE 1                      TO QID-ITEM.
112113 7501-LOOP.
112113     EXEC CICS READQ TS
112113          INTO(TS-WORK-AREA)
112113          QUEUE(QID)
112113          LENGTH(TS-LENGTH)
112113          ITEM(QID-ITEM)
112113     END-EXEC.
112113     ADD 1 TO QID-ITEM.
112113     GO TO 7501-LOOP.
112113
112113 7585-QID-ITEMERR.
112113     IF EIBTRNID NOT = WS-TRANS-ID
112113        SUBTRACT 1 FROM QID-ITEM
112113        MOVE QID-ITEM            TO PI-TEMP-STOR-ITEMS
112113     END-IF.
112113     GO TO 7599-EXIT.
112113
112113 7590-TS-QIDERR.
112113     IF EIBTRNID = WS-TRANS-ID
112113        AND EIBAID = DFHCLEAR
112113           GO TO 9100-RETURN-TRAN
112113     END-IF.
112113     IF EIBTRNID = WS-TRANS-ID
112113        MOVE ER-0033             TO EMI-ERROR
112113        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
112113        GO TO 8100-SEND-INITIAL-MAP
112113     END-IF.
112113
112113 7599-EXIT.
112113      EXIT.

      ***************************************************************** 
       8100-SEND-INITIAL-MAP.
      ***************************************************************** 
           MOVE -1                     TO  APFKL.                     
           MOVE ZERO                   TO  PI-BROWSE-SW.                
                                                                        
           MOVE SAVE-DATE              TO  ADATEO.                      
           MOVE EIBTIME                TO  TIME-IN.                     
           MOVE TIME-OUT               TO  ATIMEO.                      
                                                                        
           IF EMI-ERROR NOT = ZERO                                      
               PERFORM 9900-ERROR-FORMAT                                
           END-IF
                                                                        
           MOVE EMI-MESSAGE-AREA (1)    TO  AEMSGO

           IF PI-RETURN-TO-PROGRAM NOT = 'EL156' AND 'EL6317' AND
                 'EL6318' AND 'EL6316'
              MOVE AL-SADOF            TO SELHA
                                          F3KEYA
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 S1 > +17
                 MOVE AL-SADOF         TO SELA (S1)
              END-PERFORM
           END-IF
121012
121012     IF PI-EXPANDED-VIEW = 'Y'
121012         MOVE 'PF5=UNEXPAND DESCRIPTIONS' TO F5KEYO
121012     ELSE
121012         MOVE 'PF5=EXPAND DESCRIPTIONS  ' TO F5KEYO
121012     END-IF

           EXEC CICS SEND                                               
               FROM   (EL614AO)                                         
               MAPSET (WS-MAPSET-NAME)                                  
               MAP    (WS-MAP-NAME)                                     
               CURSOR ERASE                                             
           END-EXEC.                                                    
                                                                        
           GO TO 9100-RETURN-TRAN
           .
       8100-EXIT.                                                       
            EXIT.                                                       

      ***************************************************************** 
       8200-SEND-DATAONLY.
      ***************************************************************** 
                                                                        
           MOVE SAVE-DATE              TO  ADATEO.                      
           MOVE EIBTIME                TO  TIME-IN.                     
           MOVE TIME-OUT               TO  ATIMEO.                      
                                                                        
           IF EMI-ERROR NOT = ZERO                                      
               PERFORM 9900-ERROR-FORMAT.                               
                                                                        
           MOVE EMI-MESSAGE-AREA (1)    TO  AEMSGO.                    
                                                                        
           IF PI-RETURN-TO-PROGRAM NOT = 'EL156' AND 'EL6317' AND
                 'EL6318' AND 'EL6316'
              MOVE AL-SADOF            TO SELHA
                                          F3KEYA
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 S1 > +17
                 MOVE AL-SADOF         TO SELA (S1)
              END-PERFORM
           END-IF

           EXEC CICS SEND DATAONLY                                      
               FROM   (EL614AO)                                         
               MAPSET (WS-MAPSET-NAME)                                  
               MAP    (WS-MAP-NAME)                                     
               CURSOR                                                   
           END-EXEC.                                                    
                                                                        
           GO TO 9100-RETURN-TRAN
           .
       8200-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
      ***************************************************************** 
       8300-SEND-TEXT.
      ***************************************************************** 
                                                                        
           EXEC CICS SEND TEXT                                          
               FROM   (LOGOFF-TEXT)                                     
               LENGTH (LOGOFF-LENGTH)                                   
               ERASE  FREEKB                                            
           END-EXEC.                                                    
                                                                        
           EXEC CICS RETURN                                             
           END-EXEC.                                                    
                                                                        
       8300-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
      ***************************************************************** 
       8400-LOG-JOURNAL-RECORD.
      ***************************************************************** 
                                                                        
           IF PI-JOURNAL-FILE-ID = 0                                    
               GO TO 8400-EXIT.                                         
                                                                        
           MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                  
           MOVE 'ELCNTL'               TO  JP-FILE-ID.                  
           MOVE THIS-PGM               TO  JP-PROGRAM-ID.               
                                                                        
      *    EXEC CICS JOURNAL                                            
      *        JFILEID (PI-JOURNAL-FILE-ID)                             
      *        JTYPEID (WS-JOURNAL-TYPE-ID)                             
      *        FROM    (JOURNAL-RECORD)                                 
      *        LENGTH  (WS-JOURNAL-RECORD-LENGTH)                       
      *    END-EXEC.                                                    
                                                                        
       8400-EXIT.                                                       
            EXIT.                                                       
                                                                        
      ***************************************************************** 
       8500-DATE-CONVERSION.
      ***************************************************************** 
                                                                        
           EXEC CICS LINK                                               
               PROGRAM  ('ELDATCV')                                     
               COMMAREA (DATE-CONVERSION-DATA)                          
               LENGTH   (DC-COMM-LENGTH)                                
           END-EXEC.                                                    
                                                                        
       8500-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
      ***************************************************************** 
       9000-RETURN-CICS.
      ***************************************************************** 
                                                                        
           MOVE 'EL005'                TO  THIS-PGM.                    
           MOVE EIBAID                 TO  PI-ENTRY-CD-1.               
           PERFORM 9300-XCTL.                                           
                                                                        
       9000-EXIT.                                                       
            EXIT.                                                       
                                                                        
      ***************************************************************** 
       9100-RETURN-TRAN.
      ***************************************************************** 
                                                                        
           MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            
           MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.        
                                                                        
           EXEC CICS RETURN                                             
               COMMAREA (PROGRAM-INTERFACE-BLOCK)                       
               LENGTH   (PI-COMM-LENGTH)                                
               TRANSID  (WS-TRANS-ID)                                   
           END-EXEC.                                                    
                                                                        
       9100-EXIT.                                                       
            EXIT.                                                       
                                                                        
      ***************************************************************** 
       9300-XCTL.
      ***************************************************************** 
                                                                        
           MOVE DFHENTER               TO  EIBAID.                      
                                                                        
           EXEC CICS XCTL                                               
               PROGRAM  (THIS-PGM)                                      
               COMMAREA (PROGRAM-INTERFACE-BLOCK)                       
               LENGTH   (PI-COMM-LENGTH)                                
           END-EXEC.                                                    
                                                                        
       9300-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
      ***************************************************************** 
       9400-CLEAR.
      ***************************************************************** 
                                                                        
           MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.                     
           PERFORM 9300-XCTL.                                           
                                                                        
       9400-EXIT.                                                       
            EXIT.                                                       
                                                                        
      ***************************************************************** 
       9600-PGMIDERR.
      ***************************************************************** 
                                                                        
           EXEC CICS HANDLE CONDITION                                   
               PGMIDERR (8300-SEND-TEXT)                                
           END-EXEC.                                                    
                                                                        
           MOVE THIS-PGM               TO  PI-CALLING-PROGRAM           
                                           LOGOFF-PGM.                  
                                                                        
           MOVE 'EL005'                TO  THIS-PGM.                    
           MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 
           MOVE SPACES                 TO  PI-ENTRY-CD-1.               
           PERFORM 9300-XCTL.                                           
                                                                        
       9600-EXIT.                                                       
            EXIT.                                                       
                                                                        
      ***************************************************************** 
       9900-ERROR-FORMAT.
      ***************************************************************** 
                                                                        
           IF EMI-ERRORS-COMPLETE                                       
               ADD +1             TO  EMI-FATAL-CTR                     
               MOVE ZERO          TO  EMI-ERROR                         
               GO TO 9900-EXIT.                                         
                                                                        
           EXEC CICS LINK                                               
               PROGRAM  ('EL001')                                       
               COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 
               LENGTH   (EMI-COMM-LENGTH)                               
           END-EXEC.                                                    
                                                                        
           MOVE ZERO              TO  EMI-ERROR.                        
                                                                        
       9900-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
      ***************************************************************** 
       9990-ERROR.
      ***************************************************************** 
                                                                        
           MOVE DFHEIBLK               TO EMI-LINE1.                    
           EXEC CICS LINK                                               
               PROGRAM   ('EL004')                                      
               COMMAREA  (EMI-LINE1)                                    
               LENGTH    (72)                                           
           END-EXEC.                                                    
                                                                        
           PERFORM 8200-SEND-DATAONLY.                                  
           GO TO 9100-RETURN-TRAN.                                      
                                                                        
       9990-EXIT.                                                       
            EXIT.                                                       
                                                                        
      ***************************************************************** 
       9995-SECURITY-VIOLATION.                                         
      ***************************************************************** 
                                                                        
                  COPY ELCSCTP.                                         
                                                                        
       9995-EXIT.                                                       
            EXIT.                                                       
                                                                        
       9999-LAST-PARAGRAPH SECTION.                                     
           GOBACK.                                                      
