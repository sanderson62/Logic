       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 EL609 .                              
      *                            VMOD=2.001.                          
      *                                                                 
      *                                                                 
      *AUTHOR.    CSO.                                                  
      *           OMAHA, NEBRASKA.                                      
                                                                        
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
      *    FOR THE LIFE CLAIM INTEREST SELECTION CRITERIA.              
      *                                                                 
      *    SCREENS     - EL609A - INTEREST RATE MAINTENANCE
      *                                                                 
      *    ENTERED BY  - EL605 - LIFE CLAIM INTEREST MENU               
      *                                                                 
      *    EXIT TO     - EL605 - LIFE CLAIM INTEREST MENU               
      *                                                                 
      *    COMMAREA    - PASSED                                         
      *                                                                 
      *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL605.  ON     
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
      * 052605    2004040700004  PEMA  NEW PROGRAM
      ******************************************************************
           EJECT                                                        
       ENVIRONMENT DIVISION.                                            
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
                                                                        
       77  FILLER  PIC X(32)  VALUE '********************************'. 
       77  FILLER  PIC X(32)  VALUE '*   EL609  WORKING STORAGE     *'. 
       77  FILLER  PIC X(32)  VALUE '***********VMOD=2.001 **********'. 
       77  CI1                PIC S999  COMP-3 VALUE +0.
       77  B1                 PIC S999  COMP-3 VALUE +0.

           COPY ELCSCTM.                                                
           COPY ELCSCRTY.                                               
                                                                        
       01  WS-DATE-AREA.                                                
           05  SAVE-DATE           PIC X(8)    VALUE SPACES.            
          05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            
                                                                        
       01  FILLER                          COMP-3.                      
           05  WS-RECORD-COUNT             PIC S9(3)   VALUE ZERO.      
           05  WS-READNEXT-SW              PIC S9      VALUE ZERO.      
           05  WS-LAST-ERROR-COUNT         PIC S9(3)   VALUE ZERO.      
           05  WS-UPDATE-SW                PIC S9      VALUE ZERO.      
           05  WS-COMPLETED-SUCCESSFUL     PIC S9      VALUE ZERO.      
             88  TRANSACTION-SUCCESSFUL                    VALUE +1.    
             88  INITIAL-TRANSACTION                       VALUE +2.    
             88  CHANGE-SUCCESSFUL                         VALUE +3.    
                                                                        
           05  TIME-IN                     PIC S9(7)   VALUE ZERO.      
           05  TIME-OUT                    REDEFINES                    
               TIME-IN                     PIC S9(3)V9(4).              
                                                                        
       01  FILLER                          COMP SYNC.                   
           05  WS-INDEX                    PIC S9(4)   VALUE ZERO.      
           05  WS-JOURNAL-FILE-ID          PIC S9(4)   VALUE +1.        
           05  WS-JOURNAL-RECORD-LENGTH    PIC S9(4)   VALUE +773.      
           05  ELCIIR-LENGTH               PIC S9(4)   VALUE +100.
           05  GETMAIN-SPACE               PIC  X      VALUE SPACE.
                                                                        
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
           05  WS-CONTROL-PRIMARY.
               10  WS-COMPANY-CD           PIC X      VALUE SPACES.
               10  WS-INT-RATE-CODE        PIC XXX    VALUE SPACES.
                                                                        
           05  WS-MAPSET-NAME              PIC X(8)  VALUE 'EL609S'.    
           05  WS-MAP-NAME                 PIC X(8)  VALUE 'EL609A'.    
                                                                        
           05  FILLER                      REDEFINES                    
               WS-MAP-NAME.                                             
               10  FILLER                  PIC XX.                      
               10  WS-MAP-NUMBER           PIC X(4).                    
               10  FILLER                  PIC XX.                      
                                                                        
           05  THIS-PGM                    PIC X(8)  VALUE 'EL609'.     
                                                                        
           05  WS-JOURNAL-TYPE-ID          PIC XX      VALUE 'EL'.      
                                                                        
           05  WS-LOW-VALUES               PIC X VALUE LOW-VALUES.      
           05  WS-SPACE                    PIC X       VALUE SPACE.     
                                                                        
           05  WS-TRANS-ID                 PIC X(4)    VALUE 'EXAE'.    
                                                                        
           05  WS-TEMP-STORAGE-KEY.                                     
               10  WS-TS-TERM-ID           PIC X(4)    VALUE 'XXXX'.    
               10  FILLER                  PIC X(4)    VALUE '609'.     

           05  WS-ERROR-MESSAGE-AREA.                                   
               10  ER-0000                 PIC 9(4)   VALUE 0000.
               10  ER-0004                 PIC 9(4)   VALUE 0004.
               10  ER-0006                 PIC 9(4)   VALUE 0006.
               10  ER-0008                 PIC 9(4)   VALUE 0008.
               10  ER-0023                 PIC 9(4)   VALUE 0023.
               10  ER-0029                 PIC 9(4)   VALUE 0029.
               10  ER-0070                 PIC 9(4)   VALUE 0070.
               10  ER-0150                 PIC 9(4)   VALUE 0150.
               10  ER-0491                 PIC 9(4)   VALUE 0491.
               10  ER-1079                 PIC 9(4)   VALUE 1079.
               10  ER-1257                 PIC 9(4)   VALUE 1257.
               10  ER-1258                 PIC 9(4)   VALUE 1258.
               10  ER-2261                 PIC 9(4)   VALUE 2261.
               10  ER-3800                 PIC 9(4)   VALUE 3800.
               10  ER-3801                 PIC 9(4)   VALUE 3801.
               10  ER-3802                 PIC 9(4)   VALUE 3802.
               10  ER-3803                 PIC 9(4)   VALUE 3803.
               10  ER-3804                 PIC 9(4)   VALUE 3804.
               10  ER-3805                 PIC 9(4)   VALUE 3805.
               10  ER-3806                 PIC 9(4)   VALUE 3806.
               10  ER-3807                 PIC 9(4)   VALUE 3807.
               10  ER-3810                 PIC 9(4)   VALUE 3810.
               10  ER-7220                 PIC 9(4)   VALUE 7220.
               10  ER-7698                 PIC 9(4)   VALUE 7698.
               10  ER-9999                 PIC 9(4)   VALUE 9999.

           COPY ELCINTF.                                                
           12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                   
               16  PI-1ST-TIME-SW          PIC S9     COMP-3.           
               16  PI-MODE                 PIC X.
                   88  ADD-FUNCTION         VALUE 'A'.
               16  PI-CI-RCODE             PIC XXX.
               16  PI-CI-TOP-KEY           PIC X(4).
               16  PI-CI-BOT-KEY           PIC X(4).
               16  PI-CI-KEYS OCCURS 10.
                   20  PI-CI-KEY           PIC X(4).
               16  PI-LAST-BENEFIT-NUMBER  PIC XX.                      
               16  PI-NEXT-BENEFIT-NUMBER  PIC XX.                      
               16  PI-LINE-COUNT           PIC S9(3)  COMP-3.           
               16  PI-BROWSE-SW            PIC S9     COMP-3.           
               16  PI-SHOW-SW              PIC S9     COMP-3.           
               16  PI-CHANGE-SW            PIC S9     COMP-3.           
                                                                        
               16  FILLER                  PIC X(553).                  
                                                                        
           EJECT                                                        
                                                                        
           COPY EL609S.                                                 
                                                                        
       01  EL609AO-R REDEFINES EL609AI.                                 
           05  FILLER                      PIC X(35).
           05  WS-MAP-LINE                 OCCURS 10
                                           INDEXED BY M1.
               10  RCODEL                  PIC S9(4)  COMP.             
               10  RCODEA                  PIC X.                       
               10  RCODEO                  PIC XXX.
               10  RCODEI                  REDEFINES                    
                   RCODEO                  PIC XXX.
               10  DESCL                   PIC S9(4)   COMP.            
               10  DESCLA                  PIC X.                       
               10  DESCO                   PIC X(25).
               10  DESCI                   REDEFINES                    
                   DESCO                   PIC X(25).
               10  RATEL                   PIC S9(4)   COMP.            
               10  RATEA                   PIC X.                       
               10  RATEO                   PIC 9.99999.
               10  RATEI                   REDEFINES                    
                   RATEO                   PIC 99V99999.
                                                                        
           COPY ELCJPFX.                                                
                                           PIC X(750).                  
           COPY ELCEMIB.                                                
           COPY ELCDATE.                                                
           COPY ELCLOGOF.                                               
           COPY ELCATTR.                                                
           COPY ELCAID.                                                 
       01  FILLER    REDEFINES DFHAID.                                  
           05  FILLER                      PIC X(8).                    
           05  PF-VALUES                   PIC X                        
               OCCURS 24 TIMES.                                         
                                                                        
       LINKAGE SECTION.                                                 
       01  DFHCOMMAREA                     PIC X(1024).                 
                                                                        
           COPY ELCCIIR.
                                                                        
       PROCEDURE DIVISION.                                              
                                                                        
           MOVE EIBDATE               TO DC-JULIAN-YYDDD.               
           MOVE '5'                   TO DC-OPTION-CODE.                
           PERFORM 8500-DATE-CONVERSION
                                      THRU 8500-EXIT
           MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    
           MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                
                                                                        
           MOVE DFHCOMMAREA           TO  PROGRAM-INTERFACE-BLOCK.      
                                                                        
           MOVE +2                    TO  EMI-NUMBER-OF-LINES           
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

           IF NOT SYSTEM-DISPLAY-CAP
              MOVE 'READ'              TO SM-READ
              PERFORM 9995-SECURITY-VIOLATION
              MOVE ER-0070             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              GO TO 8100-SEND-INITIAL-MAP
           END-IF

           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
              MOVE LOW-VALUES          TO EL609AO
              MOVE -1                  TO APFKL
              MOVE ER-0008             TO EMI-ERROR
              GO TO 8200-SEND-DATAONLY
           END-IF

           EXEC CICS RECEIVE
               INTO   (EL609AO)
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
              CONTINUE
           ELSE
              MOVE ER-0008             TO EMI-ERROR
              MOVE -1                  TO APFKL
              GO TO 8200-SEND-DATAONLY
           END-IF

           IF AMAINTL > ZERO
              IF AMAINTI = 'A' OR 'C' OR 'D' OR 'S'
                 MOVE AL-UANON         TO AMAINTA
                 MOVE AMAINTI          TO PI-MODE
              ELSE
                 MOVE AL-UABOF         TO AMAINTA
                 MOVE -1               TO AMAINTL
                 MOVE ER-0023          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
              END-IF
           ELSE
              IF EIBAID = DFHPF1 OR DFHPF2
                 MOVE 'S'              TO PI-MODE
              ELSE
                 MOVE AL-UABOF         TO AMAINTA
                 MOVE -1               TO AMAINTL
                 MOVE ER-0023          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
              END-IF
           END-IF

           IF SYSTEM-MODIFY-CAP
              CONTINUE
           ELSE
              IF AMAINTI = 'A' OR 'C' OR 'D'
                 MOVE 'UPDATE'         TO SM-READ
                 PERFORM 9995-SECURITY-VIOLATION
                 MOVE ER-0070          TO EMI-ERROR
                 MOVE -1               TO  AMAINTL
                 MOVE AL-UABON         TO  AMAINTA
                 GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF
                                                                        
           IF EMI-FATAL-CTR > ZERO
              GO TO 8200-SEND-DATAONLY
           END-IF
                                                                        
           IF PI-MODE EQUAL 'S'                                         
              GO TO 2000-PROCESS-SHOW
           END-IF
                                                                        
           IF PI-MODE EQUAL 'C'                                         
              GO TO 3000-PROCESS-CHANGE
           END-IF
                                                                        
           IF PI-MODE EQUAL 'A'                                         
              GO TO 5000-PROCESS-ADD
           END-IF
                                                                        
           IF PI-MODE EQUAL 'D'                                         
              GO TO 6000-PROCESS-DELETE
           END-IF
                                                                        
           MOVE 'LOGIC ERROR HAS OCCURRED - PROGRAM EL609'              
                                       TO  LOGOFF-MSG.                  
           GO TO 8300-SEND-TEXT.                                        
           .
       1000-INITIAL-SCREEN.

           MOVE SPACES                 TO PI-PROGRAM-WORK-AREA
           MOVE PI-COMPANY-CD          TO PI-CI-TOP-KEY (1:1)
                                          PI-CI-BOT-KEY (1:1)

           MOVE ZERO                   TO PI-1ST-TIME-SW
                                          PI-LINE-COUNT
                                          PI-BROWSE-SW
                                          PI-SHOW-SW
                                          PI-CHANGE-SW

           MOVE LOW-VALUES             TO EL609AO

           MOVE -1                     TO AMAINTL
           MOVE +2                     TO WS-COMPLETED-SUCCESSFUL
           GO TO 8100-SEND-INITIAL-MAP

           .
       2000-PROCESS-SHOW.

      *IF THE USER PRIMED THE FIRST RATE  CODE THEN USE IT TO BROWSE

           IF RCODEL (1) > +0
              MOVE RCODEI (1)          TO PI-CI-BOT-KEY (2:3)
                                          PI-CI-TOP-KEY (2:3)
           END-IF

           EVALUATE EIBAID
              WHEN DFHPF2
                 MOVE PI-CI-TOP-KEY    TO WS-CONTROL-PRIMARY
                 GO TO 8000-BROWSE-BWD
              WHEN DFHPF1
                 MOVE PI-CI-BOT-KEY    TO WS-CONTROL-PRIMARY
                 GO TO 7000-BROWSE-FWD
              WHEN OTHER
                 MOVE PI-COMPANY-CD    TO WS-CONTROL-PRIMARY
                 GO TO 7000-BROWSE-FWD
           END-EVALUATE

           MOVE +1                     TO  PI-SHOW-SW
                                                                        
           GO TO 7000-BROWSE-FWD
                                                                        
           .
      ***************************************************************** 
       3000-PROCESS-CHANGE.
      ***************************************************************** 

           SET M1                      TO +1

           PERFORM UNTIL
              M1 > +10
              IF (DESCL (M1) > +0)
                 OR (RATEL (M1) > +0)
                 PERFORM 6010-EDIT-DATA
                                       THRU 6010-EXIT
              END-IF
              SET M1                   UP BY +1
           END-PERFORM

           IF NOT EMI-NO-ERRORS
              GO TO 8200-SEND-DATAONLY
           END-IF

           SET M1                      TO +1

           PERFORM UNTIL
              M1 > +10
              IF (DESCL (M1) > +0)
                 OR (RATEL (M1) > +0)
                 PERFORM 6030-UPDATE-RECORD
                                       THRU 6030-EXIT
              END-IF
              SET M1                   UP BY +1
           END-PERFORM

           MOVE PI-CI-TOP-KEY          TO WS-CONTROL-PRIMARY
           GO TO 7000-BROWSE-FWD

           .
      ***************************************************************** 
       5000-PROCESS-ADD.
      ***************************************************************** 

           SET M1                      TO +1

           PERFORM UNTIL
              M1 > +10
              IF (RCODEL    (M1) > +0)
                 PERFORM 6020-EDIT-KEY-DATA
                                       THRU 6020-EXIT
                 PERFORM 6010-EDIT-DATA
                                       THRU 6010-EXIT
              END-IF
              SET M1                   UP BY +1
           END-PERFORM

           IF NOT EMI-NO-ERRORS
              GO TO 8200-SEND-DATAONLY
           END-IF

           SET M1                      TO +1

           PERFORM UNTIL
              M1 > +10
              IF RCODEL (M1) > +0
                 PERFORM 6060-ADD-RECORD
                                       THRU 6060-EXIT
              END-IF
              SET M1                   UP BY +1
              ADD +1                   TO CI1
           END-PERFORM

           SET M1                      TO +1
           MOVE +1                     TO CI1

           MOVE PI-CI-TOP-KEY          TO WS-CONTROL-PRIMARY
           GO TO 7000-BROWSE-FWD

           .
      ***************************************************************** 
       6000-PROCESS-DELETE.                                             
      ***************************************************************** 
                                                                        
           SET M1                      TO +1

      ** THE WAY THIS WORKS IS YOU HAVE TO SPACE OUT THE RATE  CODE
      ** FOR THE DELETE TO WORK
      
           PERFORM UNTIL
              M1 > +10
              IF ((RCODEL   (M1) > +0)
                 AND(RCODEI (M1) = SPACES))
                 PERFORM 6050-DELETE-RECORD
                                       THRU 6050-EXIT
              END-IF
              SET M1                   UP BY +1
           END-PERFORM

           MOVE PI-CI-TOP-KEY          TO WS-CONTROL-PRIMARY
           GO TO 7000-BROWSE-FWD

           .
       6010-EDIT-DATA.

           IF RATEL (M1) > +0
              EXEC CICS BIF DEEDIT
                  FIELD   (RATEI (M1))
                  LENGTH  (7)
              END-EXEC
              IF RATEI (M1) NOT NUMERIC
                 MOVE ER-7698          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO RATEA (M1)
                 MOVE -1               TO RATEL (M1)
              END-IF
           END-IF

           .
       6010-EXIT.
           EXIT.
           
           
       6020-EDIT-KEY-DATA.
        
           IF (RCODEL    (M1) > +0)
              CONTINUE
           ELSE
              MOVE ER-3810          TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO RCODEA (M1)
              MOVE -1                  TO RCODEL (M1)
              GO TO 6020-EXIT
           END-IF

           .
       6020-EXIT.
           EXIT.
           
       6030-UPDATE-RECORD.

           SET CI1                     TO M1
           MOVE PI-CI-KEY (CI1)        TO WS-CONTROL-PRIMARY

           EXEC CICS READ
              UPDATE
              DATASET ('ELCIIR')
              SET     (ADDRESS OF CLAIM-INTEREST-IR)
              RIDFLD  (WS-CONTROL-PRIMARY)
              RESP    (WS-RESPONSE)
           END-EXEC
           
           IF NOT RESP-NORMAL
              GO TO 6030-UPDATE-ERROR
           END-IF
           
           IF DESCL (M1) > +0
              MOVE DESCI (M1)          TO IR-DESCRIPTION
           END-IF
           
           IF RATEL (M1) > +0
              MOVE RATEI (M1)          TO IR-INT-RATE
           END-IF

           MOVE PI-PROCESSOR-ID        TO IR-LAST-MAINT-BY
           MOVE EIBTIME                TO IR-LAST-MAINT-HHMMSS
           MOVE SAVE-BIN-DATE          TO IR-LAST-MAINT-DT

           EXEC CICS REWRITE
              DATASET    ('ELCIIR')
              FROM       (CLAIM-INTEREST-IR)
              RESP       (WS-RESPONSE)
           END-EXEC
           
           IF RESP-NORMAL
              GO TO 6030-EXIT
           END-IF

           .
       6030-UPDATE-ERROR.

           MOVE ER-3807                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT
           MOVE AL-UABON               TO RCODEA (M1)
           MOVE -1                     TO RCODEL (M1)
           GO TO 8200-SEND-DATAONLY
           
           .
       6030-EXIT.
           EXIT.

       6050-DELETE-RECORD.

           SET CI1                     TO M1
           MOVE PI-CI-KEY (CI1)        TO WS-CONTROL-PRIMARY
           EXEC CICS READ
              UPDATE
              DATASET ('ELCIIR')
              SET     (ADDRESS OF CLAIM-INTEREST-IR)
              RIDFLD  (WS-CONTROL-PRIMARY)
              RESP    (WS-RESPONSE)
           END-EXEC
           
           IF NOT RESP-NORMAL
              GO TO 6050-DELETE-ERROR
           END-IF

           EXEC CICS DELETE
              DATASET ('ELCIIR')
              RESP    (WS-RESPONSE)
           END-EXEC
           
           IF RESP-NORMAL
              GO TO 6050-EXIT
           END-IF

           .
       6050-DELETE-ERROR.

           MOVE ER-1079                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT
           MOVE AL-UABON               TO RCODEA (M1)
           MOVE -1                     TO RCODEL (M1)

           .
       6050-EXIT.
           EXIT.

       6060-ADD-RECORD.

           SET CI1                     TO M1
           MOVE PI-COMPANY-CD          TO WS-COMPANY-CD
           MOVE RCODEI (M1)            TO WS-INT-RATE-CODE
           
           EXEC CICS READ
              DATASET ('ELCIIR')
              SET     (ADDRESS OF CLAIM-INTEREST-IR)
              RIDFLD  (WS-CONTROL-PRIMARY)
              RESP    (WS-RESPONSE)
           END-EXEC
           
           IF RESP-NORMAL
              GO TO 6060-ADD-ERROR
           END-IF
           
           EXEC CICS GETMAIN
              LENGTH   (ELCIIR-LENGTH)
              SET      (ADDRESS OF CLAIM-INTEREST-IR)
              INITIMG  (GETMAIN-SPACE)
           END-EXEC

           MOVE PI-COMPANY-CD          TO IR-COMPANY-CD
           
           MOVE RCODEI (M1)            TO IR-INT-RATE-CODE
           IF DESCL (M1) > +0
              MOVE DESCI (M1)          TO IR-DESCRIPTION
           END-IF
           MOVE RATEI  (M1)            TO IR-INT-RATE

           MOVE PI-PROCESSOR-ID        TO IR-LAST-MAINT-BY
           MOVE EIBTIME                TO IR-LAST-MAINT-HHMMSS
           MOVE SAVE-BIN-DATE          TO IR-LAST-MAINT-DT

           EXEC CICS WRITE
              DATASET    ('ELCIIR')
              FROM       (CLAIM-INTEREST-IR)
              RIDFLD     (IR-CONTROL-PRIMARY)
              RESP       (WS-RESPONSE)
           END-EXEC
           
           IF RESP-NORMAL
              GO TO 6060-EXIT
           END-IF

           .
       6060-ADD-ERROR.

           MOVE ER-3806                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT
           MOVE AL-UABON               TO RCODEA (M1)
           MOVE -1                     TO RCODEL (M1)

           .
       6060-EXIT.
           EXIT.

       7000-BROWSE-FWD.

           SET M1  TO +1
                                                                       
           EXEC CICS STARTBR                                            
               DATASET   ('ELCIIR')
               RIDFLD    (WS-CONTROL-PRIMARY)
               GTEQ
               RESP      (WS-RESPONSE)
           END-EXEC

           EVALUATE TRUE
              WHEN RESP-ENDFILE
                 GO TO 7030-END-FILE
              WHEN RESP-NORMAL
                 CONTINUE
              WHEN RESP-NOTFND
                 GO TO 7040-NOT-FOUND
           END-EVALUATE

           MOVE LOW-VALUES             TO EL609AO
           MOVE ZERO                   TO PI-LINE-COUNT
           MOVE +1                     TO PI-BROWSE-SW
           MOVE +0                     TO CI1
           .
       7010-READ-NEXT.

           EXEC CICS READNEXT
               SET     (ADDRESS OF CLAIM-INTEREST-IR)
               DATASET ('ELCIIR')
               RIDFLD  (WS-CONTROL-PRIMARY)
               RESP    (WS-RESPONSE)
           END-EXEC

           EVALUATE TRUE
              WHEN RESP-ENDFILE
                 GO TO 7030-END-FILE
              WHEN RESP-NORMAL
                 CONTINUE
              WHEN RESP-NOTFND
                 GO TO 7040-NOT-FOUND
           END-EVALUATE

           IF IR-COMPANY-CD NOT = PI-COMPANY-CD
              GO TO 7030-END-FILE
           END-IF
           
           ADD +1                      TO WS-RECORD-COUNT
                                          CI1

           MOVE IR-CONTROL-PRIMARY     TO PI-CI-BOT-KEY
                                          PI-CI-KEY (CI1)

           MOVE IR-INT-RATE-CODE       TO RCODEO (M1)
           MOVE IR-DESCRIPTION         TO DESCO (M1)
           MOVE IR-INT-RATE            TO RATEO (M1)

           IF M1 = +1
              MOVE IR-CONTROL-PRIMARY  TO PI-CI-TOP-KEY
           END-IF

           IF M1 < +10
              SET M1 UP BY +1
              GO TO 7010-READ-NEXT
           END-IF

           IF PI-BROWSE-SW = +1
              EXEC CICS ENDBR
                   DATASET ('ELCIIR')
              END-EXEC
              MOVE +0                  TO PI-BROWSE-SW
           END-IF

           MOVE ER-0000                TO EMI-ERROR
           GO TO 8100-SEND-INITIAL-MAP

           .
       7030-END-FILE.

           IF EMI-ERROR = ER-1257
              MOVE ER-7220             TO  EMI-ERROR
           ELSE
              MOVE ER-1258             TO  EMI-ERROR
           END-IF

           MOVE -1                     TO  AMAINTL
           MOVE AL-UABOF               TO  AMAINTA
           GO TO 8100-SEND-INITIAL-MAP

           .
       7040-NOT-FOUND.

           MOVE ER-0006                TO  EMI-ERROR
           MOVE -1                     TO  AMAINTL
           MOVE AL-UABOF               TO  AMAINTA
           GO TO 8200-SEND-DATAONLY

           .
      ***************************************************************** 
       8000-BROWSE-BWD.
      ***************************************************************** 
                                                                        
           SET M1                      TO +10
           MOVE +11                    TO CI1
                                                                       
           EXEC CICS STARTBR                                            
               DATASET ('ELCIIR')
               RIDFLD  (WS-CONTROL-PRIMARY)
               GTEQ
               RESP    (WS-RESPONSE)
           END-EXEC

           EVALUATE TRUE
              WHEN RESP-ENDFILE
                 GO TO 8020-END-FILE
              WHEN RESP-NORMAL
                 CONTINUE
              WHEN RESP-NOTFND
                 GO TO 8030-NOT-FOUND
           END-EVALUATE

           MOVE LOW-VALUES             TO EL609AO
           MOVE ZERO                   TO PI-LINE-COUNT
           MOVE +1                     TO PI-BROWSE-SW
                                                                        
           .
       8010-READ-PREV.

           EXEC CICS READPREV
               SET     (ADDRESS OF CLAIM-INTEREST-IR)
               DATASET ('ELCIIR')
               RIDFLD  (WS-CONTROL-PRIMARY)
               RESP    (WS-RESPONSE)
           END-EXEC

           EVALUATE TRUE
              WHEN RESP-ENDFILE
                 GO TO 8020-END-FILE
              WHEN RESP-NORMAL
                 CONTINUE
              WHEN RESP-NOTFND
                 GO TO 8030-NOT-FOUND
           END-EVALUATE

           IF IR-COMPANY-CD NOT = PI-COMPANY-CD
              GO TO 8020-END-FILE
           END-IF
           
           ADD +1                      TO WS-RECORD-COUNT
           SUBTRACT +1                 FROM CI1

           MOVE IR-CONTROL-PRIMARY     TO PI-CI-TOP-KEY
                                          PI-CI-KEY (CI1)

           MOVE IR-INT-RATE-CODE       TO RCODEO (M1)
           MOVE IR-DESCRIPTION         TO DESCO  (M1)
           MOVE IR-INT-RATE            TO RATEO  (M1)

           IF M1 = +10
              MOVE IR-CONTROL-PRIMARY  TO PI-CI-BOT-KEY
           END-IF

           IF M1 > +1
              SET M1                   DOWN BY +1
              GO TO 8010-READ-PREV
           END-IF

           IF PI-BROWSE-SW = +1
              EXEC CICS ENDBR
                   DATASET ('ELCIIR')
              END-EXEC
              MOVE +0                  TO PI-BROWSE-SW
           END-IF

           MOVE ER-0000                TO EMI-ERROR
           GO TO 8100-SEND-INITIAL-MAP

           .
       8020-END-FILE.

           IF M1 > +1
              MOVE PI-CI-TOP-KEY       TO WS-CONTROL-PRIMARY
              MOVE ER-1257             TO EMI-ERROR
              GO TO 7000-BROWSE-FWD
           END-IF
           MOVE ER-1257                TO EMI-ERROR
           MOVE -1                     TO  AMAINTL
           MOVE AL-UABOF               TO  AMAINTA
           GO TO 8100-SEND-INITIAL-MAP

           .
       8030-NOT-FOUND.

           MOVE ER-0006                TO  EMI-ERROR
           MOVE -1                     TO  AMAINTL
           MOVE AL-UABOF               TO  AMAINTA
           GO TO 8200-SEND-DATAONLY

           .
      ***************************************************************** 
       8100-SEND-INITIAL-MAP.
      ***************************************************************** 
                                                                        
           MOVE -1                     TO  AMAINTL.                     
           MOVE ZERO                   TO  PI-BROWSE-SW.                
                                                                        
           MOVE SAVE-DATE              TO  ADATEO.                      
           MOVE EIBTIME                TO  TIME-IN.                     
           MOVE TIME-OUT               TO  ATIMEO.                      
                                                                        
           IF EMI-ERROR NOT = ZERO                                      
               PERFORM 9900-ERROR-FORMAT                                
           END-IF
                                                                        
           MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.                    
           MOVE EMI-MESSAGE-AREA (2)    TO  AEMSG2O.                    
                                                                        
           EXEC CICS SEND                                               
               FROM   (EL609AO)                                         
               MAPSET (WS-MAPSET-NAME)                                  
               MAP    (WS-MAP-NAME)                                     
               CURSOR ERASE                                             
           END-EXEC.                                                    
                                                                        
           GO TO 9100-RETURN-TRAN
           .
       8100-EXIT.                                                       
            EXIT.                                                       
                                                                        
           EJECT                                                        
      ***************************************************************** 
       8200-SEND-DATAONLY.
      ***************************************************************** 
                                                                        
           MOVE SAVE-DATE              TO  ADATEO.                      
           MOVE EIBTIME                TO  TIME-IN.                     
           MOVE TIME-OUT               TO  ATIMEO.                      
                                                                        
           IF EMI-ERROR NOT = ZERO                                      
               PERFORM 9900-ERROR-FORMAT.                               
                                                                        
           MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.                    
           MOVE EMI-MESSAGE-AREA (2)    TO  AEMSG2O.                    
                                                                        
           EXEC CICS SEND DATAONLY                                      
               FROM   (EL609AO)                                         
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
