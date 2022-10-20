       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 EL607 .                              
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
      *    SCREENS     - EL607A - STATE BREAKOUT
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
       77  FILLER  PIC X(32)  VALUE '*   EL607  WORKING STORAGE     *'. 
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
           05  ELCISB-LENGTH               PIC S9(4)   VALUE +100.
           05  GETMAIN-SPACE               PIC  X      VALUE SPACE.
                                                                        
       01  FILLER.                                                      
           12  DEEDIT-FIELD        PIC  X(10).
           12  DEEDIT-FIELD-V0  REDEFINES
               DEEDIT-FIELD        PIC S9(10).
           05  WS-BIN-DATES OCCURS 10.
               10  WS-BIN-DATE         PIC XX.
           05  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL              VALUE +00.
               88  RESP-ERROR               VALUE +01.
               88  RESP-TERMID              VALUE +11.
               88  RESP-NOTFND              VALUE +13.
               88  RESP-NOTOPEN             VALUE +19.
               88  RESP-ENDFILE             VALUE +20.
               88  RESP-MAPFAIL             VALUE +36.
           05  WS-CONTROL-PRIMARY.
               10  WS-COMPANY-CD           PIC X      VALUE SPACES.
               10  WS-STATE                PIC XX     VALUE SPACES.
               10  WS-PROD                 PIC XX     VALUE SPACES.
               10  WS-COV-TYPE             PIC XX     VALUE SPACES.
               10  WS-BREAKOUT-CODE        PIC XX     VALUE SPACES.
               10  WS-CALC-END             PIC 999    VALUE ZEROS.
           05  WS-ST-CONTROL-PRIMARY.
               10  WS-ST-COMPANY-CD        PIC X      VALUE SPACES.
               10  WS-ST-SCHED-CODE        PIC XXX    VALUE SPACES.
               10  WS-ST-END-DT            PIC XX     VALUE SPACES.
           05  WS-CONTROL-FILE-KEY.                                     
               10  WS-CFK-COMPANY-ID       PIC X(3)    VALUE SPACES.    
               10  WS-CFK-RECORD-TYPE      PIC X       VALUE SPACES.    
               10  WS-CFK-STATE            PIC XX      VALUE SPACES.    
               10  WS-CFK-BENEFIT-CD       PIC XX      VALUE SPACES.    
               10  WS-CFK-SEQUENCE-NO      PIC S9(4)   VALUE ZERO COMP. 
                                                                        
           05  WS-MAPSET-NAME              PIC X(8)  VALUE 'EL607S'.    
           05  WS-MAP-NAME                 PIC X(8)  VALUE 'EL607A'.    
                                                                        
           05  FILLER                      REDEFINES                    
               WS-MAP-NAME.                                             
               10  FILLER                  PIC XX.                      
               10  WS-MAP-NUMBER           PIC X(4).                    
               10  FILLER                  PIC XX.                      
                                                                        
           05  THIS-PGM                    PIC X(8)  VALUE 'EL607'.     
                                                                        
           05  WS-JOURNAL-TYPE-ID          PIC XX      VALUE 'EL'.      
                                                                        
           05  WS-LOW-VALUES               PIC X VALUE LOW-VALUES.      
           05  WS-SPACE                    PIC X       VALUE SPACE.     
                                                                        
           05  WS-TRANS-ID                 PIC X(4)    VALUE 'EXAC'.    
                                                                        
           05  WS-TEMP-STORAGE-KEY.                                     
               10  WS-TS-TERM-ID           PIC X(4)    VALUE 'XXXX'.    
               10  FILLER                  PIC X(4)    VALUE '607'.     

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
               10  ER-3808                 PIC 9(4)   VALUE 3808.
               10  ER-7220                 PIC 9(4)   VALUE 7220.
               10  ER-9999                 PIC 9(4)   VALUE 9999.
                                                                        
           EJECT                                                        
           COPY ELCINTF.                                                
           12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                   
               16  PI-1ST-TIME-SW          PIC S9     COMP-3.           
               16  PI-MODE                 PIC X.
                   88  ADD-FUNCTION         VALUE 'A'.
               16  PI-CI-STATE             PIC XX.                      
               16  PI-CI-PROD              PIC XX.                      
               16  PI-CI-COV-TYPE          PIC XX.
               16  PI-CI-BREAKOUT-CODE     PIC XX.
               16  PI-CI-CALC-END          PIC 999.
               16  PI-CI-TOP-KEY           PIC X(12).
               16  PI-CI-BOT-KEY           PIC X(12).
               16  PI-CI-KEYS OCCURS 10.
                   20  PI-CI-KEY           PIC X(12).
               16  PI-LAST-BENEFIT-NUMBER  PIC XX.                      
               16  PI-NEXT-BENEFIT-NUMBER  PIC XX.                      
               16  PI-LINE-COUNT           PIC S9(3)  COMP-3.           
               16  PI-BROWSE-SW            PIC S9     COMP-3.           
               16  PI-SHOW-SW              PIC S9     COMP-3.           
               16  PI-CHANGE-SW            PIC S9     COMP-3.           
                                                                        
               16  FILLER                  PIC X(553).                  
                                                                        
           EJECT                                                        
                                                                        
           COPY EL607S.                                                 
                                                                        
       01  EL607AO-R REDEFINES EL607AI.                                 
           05  FILLER                      PIC X(35).
           05  WS-MAP-LINE                 OCCURS 10
                                           INDEXED BY M1.
               10  STATEL                  PIC S9(4)  COMP.             
               10  STATEA                  PIC X.                       
               10  STATEO                  PIC XX.
               10  STATEI                  REDEFINES                    
                   STATEO                  PIC XX.
               10  PRODL                   PIC S9(4)   COMP.            
               10  PRODA                   PIC X.                       
               10  PRODO                   PIC XX.
               10  PRODI                   REDEFINES                    
                   PRODO                   PIC XX.
               10  COVL                    PIC S9(4)   COMP.            
               10  COVA                    PIC X.                       
               10  COVO                    PIC XX.                       
               10  COVI                    REDEFINES                    
                   COVO                    PIC XX.                       
               10  SBCDL                   PIC S9(4)   COMP.            
               10  SBCDA                   PIC X.                       
               10  SBCDO                   PIC XX.                       
               10  SBCDI                   REDEFINES                    
                   SBCDO                   PIC XX.                       
               10  STARTL                  PIC S9(4)   COMP.            
               10  STARTA                  PIC X.                       
               10  STARTO                  PIC 999.
               10  STARTI                  REDEFINES                    
                   STARTO                  PIC XXX.
               10  ENDL                    PIC S9(4)   COMP.            
               10  ENDA                    PIC X.                       
               10  ENDO                    PIC 999.                     
               10  ENDI                    REDEFINES                    
                   ENDO                    PIC XXX.
               10  CODEL                   PIC S9(4)   COMP.            
               10  CODEA                   PIC X.                       
               10  CODEO                   PIC XXX.
               10  CODEI                   REDEFINES                    
                   CODEO                   PIC XXX.                     
                                                                        
           EJECT                                                        
           COPY ELCJPFX.                                                
                                           PIC X(750).                  
           EJECT                                                        
           COPY ELCEMIB.                                                
                                                                        
           EJECT                                                        
           COPY ELCDATE.                                                
                                                                        
           EJECT                                                        
           COPY ELCLOGOF.                                               
                                                                        
           EJECT                                                        
           COPY ELCATTR.                                                
                                                                        
           EJECT                                                        
           COPY ELCAID.                                                 
                                                                        
       01  FILLER    REDEFINES DFHAID.                                  
           05  FILLER                      PIC X(8).                    
           05  PF-VALUES                   PIC X                        
               OCCURS 24 TIMES.                                         
                                                                        
                                       COPY ELCCIST.

       LINKAGE SECTION.                                                 
       01  DFHCOMMAREA                 PIC X(1024).                 
                                                                        
                                                                        
           EJECT                                                        
           COPY ELCCISB.
           COPY ELCCNTL.
                                                                        
           EJECT                                                        
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
              MOVE LOW-VALUES          TO EL607AO
              MOVE -1                  TO APFKL
              MOVE ER-0008             TO EMI-ERROR
              GO TO 8200-SEND-DATAONLY
           END-IF

           EXEC CICS RECEIVE
               INTO   (EL607AO)
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
                                                                        
           MOVE 'LOGIC ERROR HAS OCCURRED - PROGRAM EL607'              
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

           MOVE LOW-VALUES             TO EL607AO

           MOVE -1                     TO AMAINTL
           MOVE +2                     TO WS-COMPLETED-SUCCESSFUL
           GO TO 8100-SEND-INITIAL-MAP

           .
       2000-PROCESS-SHOW.

      *IF THE USER PRIMED THE FIRST STATE THEN USE IT TO BROWSE WITH

           IF STATEL (1) > +0
              MOVE STATEI (1)          TO PI-CI-BOT-KEY (2:2)
                                          PI-CI-TOP-KEY (2:2)
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
           MOVE +1                     TO CI1

           PERFORM UNTIL
              M1 > +10
              IF (STARTL (M1) > +0)
                 OR (CODEL (M1) > +0)
                 PERFORM 6010-EDIT-DATA
                                       THRU 6010-EXIT
              END-IF
              SET M1                   UP BY +1
              ADD +1                   TO CI1
           END-PERFORM

           IF NOT EMI-NO-ERRORS
              GO TO 8200-SEND-DATAONLY
           END-IF

           SET M1                      TO +1
           MOVE +1                     TO CI1

           PERFORM UNTIL
              M1 > +10
              IF (STARTL (M1) > +0)
                 OR (CODEL (M1) > +0)
                 PERFORM 6030-UPDATE-RECORD
                                       THRU 6030-EXIT
              END-IF
              SET M1                   UP BY +1
              ADD +1                   TO CI1
           END-PERFORM

           MOVE PI-CI-TOP-KEY          TO WS-CONTROL-PRIMARY
           GO TO 7000-BROWSE-FWD

           .
      ***************************************************************** 
       5000-PROCESS-ADD.
      ***************************************************************** 

           SET M1                      TO +1
           MOVE +1                     TO CI1

           PERFORM UNTIL
              M1 > +10
              IF (STATEL    (M1) > +0)
                 OR (PRODL  (M1) > +0)
                 OR (COVL   (M1) > +0)
                 OR (SBCDL  (M1) > +0)
                 OR (ENDL   (M1) > +0)
                 PERFORM 6020-EDIT-KEY-DATA
                                       THRU 6020-EXIT
                 PERFORM 6010-EDIT-DATA
                                       THRU 6010-EXIT
              END-IF
              SET M1                   UP BY +1
              ADD +1                   TO CI1
           END-PERFORM

           IF NOT EMI-NO-ERRORS
              GO TO 8200-SEND-DATAONLY
           END-IF

           SET M1                      TO +1
           MOVE +1                     TO CI1

           PERFORM UNTIL
              M1 > +10
              IF STATEL (M1) > +0
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
           MOVE +1                     TO CI1

      ** THE WAY THIS WORKS IS YOU HAVE TO SPACE OUT MOST OF THE KEY
      ** FOR THE DELETE TO WORK
      
           PERFORM UNTIL
              M1 > +10
              IF ((STATEL   (M1) > +0)
                 AND(STATEI (M1) = SPACES))
                        AND
                 ((PRODL    (M1) > +0)
                 AND (PRODI (M1) = SPACES))
                        AND
                 ((COVL     (M1) > +0)
                 AND (COVI  (M1) = SPACES))
                 PERFORM 6050-DELETE-RECORD
                                       THRU 6050-EXIT
              END-IF
              SET M1                   UP BY +1
              ADD +1                   TO CI1
           END-PERFORM

           MOVE PI-CI-TOP-KEY          TO WS-CONTROL-PRIMARY
           GO TO 7000-BROWSE-FWD

           .
       6010-EDIT-DATA.

           IF STARTL (M1) > +0
              IF STARTI (M1) NUMERIC
                 CONTINUE
              ELSE
                 MOVE ER-0491          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO STARTA (M1)
                 MOVE -1               TO STARTL (M1)
              END-IF
           ELSE
              IF ADD-FUNCTION
                 MOVE ER-0491          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO STARTA (M1)
                 MOVE -1               TO STARTL (M1)
              END-IF
           END-IF

           IF CODEL (M1) > +0
              IF CODEI (M1) NOT = '   '
                 PERFORM 6015-EDIT-SCODE
                                       THRU 6015-EXIT
              ELSE
                 MOVE ER-3808          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO CODEA (M1)
                 MOVE -1               TO CODEL (M1)
              END-IF
           ELSE
              IF ADD-FUNCTION
                 MOVE ER-3808          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO CODEA (M1)
                 MOVE -1               TO CODEL (M1)
              END-IF
           END-IF

           .
       6010-EXIT.
           EXIT.
           
       6015-EDIT-SCODE.
       
           MOVE PI-COMPANY-CD          TO WS-ST-COMPANY-CD
           MOVE CODEI (M1)             TO WS-ST-SCHED-CODE
           MOVE LOW-VALUES             TO WS-ST-END-DT

           EXEC CICS READ GTEQ
              DATASET ('ELCIST')
              INTO    (CLAIM-INTEREST-ST)
              RIDFLD  (WS-ST-CONTROL-PRIMARY)
              RESP    (WS-RESPONSE)
           END-EXEC
           
           IF (RESP-NORMAL)
              AND (ST-COMPANY-CD = PI-COMPANY-CD)
              AND (ST-SCHED-CODE = CODEI (M1))
              CONTINUE
           ELSE
              MOVE ER-3808             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO CODEA (M1)
              MOVE -1                  TO CODEL (M1)
           END-IF

           .
       6015-EXIT.
           EXIT.

       6020-EDIT-KEY-DATA.
        
           IF (STATEL    (M1) > +0)
              AND (PRODL (M1) > +0)
              AND (COVL  (M1) > +0)
              AND (ENDL  (M1) > +0)
              CONTINUE
           ELSE
              MOVE ER-3804             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO STATEA (M1)
              MOVE AL-UABON            TO PRODA  (M1)
              MOVE AL-UABON            TO COVA   (M1)
              MOVE -1                  TO STATEL (M1)
              GO TO 6020-EXIT
           END-IF

           MOVE SPACES                 TO WS-CONTROL-FILE-KEY
           MOVE PI-COMPANY-ID          TO WS-CFK-COMPANY-ID
           MOVE '3'                    TO WS-CFK-RECORD-TYPE
           MOVE STATEI (M1)            TO WS-CFK-STATE
           MOVE +0                     TO WS-CFK-SEQUENCE-NO

           EXEC CICS READ
               DATASET ('ELCNTL')
               SET     (ADDRESS OF CONTROL-FILE)
               RIDFLD  (WS-CONTROL-FILE-KEY)
               RESP    (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              CONTINUE
           ELSE
              MOVE ER-2261             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO STATEA (M1)
              MOVE -1                  TO STATEL (M1)
           END-IF

           IF PRODI (M1) = '**'
              CONTINUE
           ELSE
              PERFORM 6040-EDIT-BENEFIT-CD
                                       THRU 6040-EXIT
           END-IF

           IF COVI (M1) = 'LF'
              CONTINUE
           ELSE
              MOVE ER-3805             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO COVA   (M1)
              MOVE -1                  TO COVL   (M1)
           END-IF

      * I JUST DID THE FOLLOWING TO MAKE SURE THERE ARE SPACES
      * IN THE BREAKOUT CODE IF THEY DON'T ENTER ANYTHING  :)

           IF SBCDL (M1) = +0
              MOVE +2                  TO SBCDL (M1)
              MOVE '  '                TO SBCDI (M1)
           END-IF

           IF ENDL (M1) > +0
              IF (ENDI (M1) NUMERIC)
                 AND (ENDO (M1) > ZEROS)
                 CONTINUE
              ELSE
                 MOVE ER-0491          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO ENDA (M1)
                 MOVE -1               TO ENDL (M1)
              END-IF
           ELSE
              MOVE ER-0491             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO ENDA (M1)
              MOVE -1                  TO ENDL (M1)
           END-IF

           .
       6020-EXIT.
           EXIT.
           
       6030-UPDATE-RECORD.

           MOVE PI-CI-KEY (CI1)        TO WS-CONTROL-PRIMARY
           EXEC CICS READ
              UPDATE
              DATASET ('ELCISB')
              SET     (ADDRESS OF CLAIM-INTEREST-SB)
              RIDFLD  (WS-CONTROL-PRIMARY)
              RESP    (WS-RESPONSE)
           END-EXEC
           
           IF NOT RESP-NORMAL
              GO TO 6030-UPDATE-ERROR
           END-IF
           
           IF STARTL (M1) > +0
              MOVE STARTI (M1)         TO SB-CALC-START
           END-IF

           IF CODEL (M1) > +0
              MOVE CODEI (M1)          TO SB-SCHED-CODE
           END-IF
              
           MOVE PI-PROCESSOR-ID        TO SB-LAST-MAINT-BY
           MOVE EIBTIME                TO SB-LAST-MAINT-HHMMSS
           MOVE SAVE-BIN-DATE          TO SB-LAST-MAINT-DT

           EXEC CICS REWRITE
              DATASET    ('ELCISB')
              FROM       (CLAIM-INTEREST-SB)
              RESP       (WS-RESPONSE)
           END-EXEC
           
           IF RESP-NORMAL
              GO TO 6030-EXIT
           END-IF

           .
       6030-UPDATE-ERROR.

           MOVE ER-3807                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT
           MOVE AL-UABON               TO STATEA (M1)
           MOVE -1                     TO STATEL (M1)
           GO TO 8200-SEND-DATAONLY
           
           .
       6030-EXIT.
           EXIT.

       6040-EDIT-BENEFIT-CD.
       
           MOVE SPACES                 TO WS-CONTROL-FILE-KEY
           MOVE PI-COMPANY-ID          TO WS-CFK-COMPANY-ID
           MOVE '4'                    TO WS-CFK-RECORD-TYPE
           MOVE PRODI (M1)             TO WS-CFK-BENEFIT-CD
           MOVE +0                     TO WS-CFK-SEQUENCE-NO

           EXEC CICS READ
               DATASET ('ELCNTL')
               GTEQ
               SET     (ADDRESS OF CONTROL-FILE)
               RIDFLD  (WS-CONTROL-FILE-KEY)
               RESP    (WS-RESPONSE)
           END-EXEC

           IF (RESP-NORMAL)
              AND (CF-COMPANY-ID = PI-COMPANY-ID)
              AND (CF-RECORD-TYPE = '4')
              PERFORM VARYING B1 FROM +1 BY +1 UNTIL
                 (B1 > +8)
                 OR (CF-BENEFIT-CODE (B1) = PRODI (M1))
              END-PERFORM
              IF B1 NOT > +8
                 GO TO 6040-EXIT
              END-IF
           END-IF

           .
       6040-BENEFIT-CD-ERROR.

           MOVE ER-0150                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT
           MOVE AL-UABON               TO PRODA (M1)
           MOVE -1                     TO PRODL (M1)

           .
       6040-EXIT.
           EXIT.

       6050-DELETE-RECORD.

           MOVE PI-CI-KEY (CI1)       TO WS-CONTROL-PRIMARY
           EXEC CICS READ
              UPDATE
              DATASET ('ELCISB')
              SET     (ADDRESS OF CLAIM-INTEREST-SB)
              RIDFLD  (WS-CONTROL-PRIMARY)
              RESP    (WS-RESPONSE)
           END-EXEC
           
           IF NOT RESP-NORMAL
              GO TO 6050-DELETE-ERROR
           END-IF

           EXEC CICS DELETE
              DATASET ('ELCISB')
              RESP    (WS-RESPONSE)
           END-EXEC
           
           IF RESP-NORMAL
              GO TO 6050-EXIT
           END-IF

           .
       6050-DELETE-ERROR.

           MOVE ER-1079                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT
           MOVE AL-UABON               TO STATEA (M1)
           MOVE -1                     TO STATEL (M1)

           .
       6050-EXIT.
           EXIT.

       6060-ADD-RECORD.

           MOVE PI-COMPANY-CD          TO WS-COMPANY-CD
           MOVE STATEI (M1)            TO WS-STATE
           MOVE PRODI  (M1)            TO WS-PROD
           MOVE COVI   (M1)            TO WS-COV-TYPE
           MOVE SBCDI  (M1)            TO WS-BREAKOUT-CODE
           MOVE ENDI   (M1)            TO WS-CALC-END
           
           EXEC CICS READ
              DATASET ('ELCISB')
              SET     (ADDRESS OF CLAIM-INTEREST-SB)
              RIDFLD  (WS-CONTROL-PRIMARY)
              RESP    (WS-RESPONSE)
           END-EXEC
           
           IF RESP-NORMAL
              GO TO 6060-ADD-ERROR
           END-IF
           
           EXEC CICS GETMAIN
              LENGTH   (ELCISB-LENGTH)
              SET      (ADDRESS OF CLAIM-INTEREST-SB)
              INITIMG  (GETMAIN-SPACE)
           END-EXEC

           MOVE 'SB'                   TO SB-RECORD-ID
           MOVE PI-COMPANY-CD          TO SB-COMPANY-CD
           MOVE STATEI (M1)            TO SB-STATE
           MOVE PRODI  (M1)            TO SB-PRODUCT
           MOVE COVI   (M1)            TO SB-COVERAGE
           MOVE SBCDI  (M1)            TO SB-BREAKOUT-CODE
           MOVE ENDI   (M1)            TO SB-CALC-END

           MOVE STARTI (M1)            TO SB-CALC-START
           MOVE CODEI  (M1)            TO SB-SCHED-CODE

           MOVE PI-PROCESSOR-ID        TO SB-LAST-MAINT-BY
           MOVE EIBTIME                TO SB-LAST-MAINT-HHMMSS
           MOVE SAVE-BIN-DATE          TO SB-LAST-MAINT-DT

           EXEC CICS WRITE
              DATASET    ('ELCISB')
              FROM       (CLAIM-INTEREST-SB)
              RIDFLD     (SB-CONTROL-PRIMARY)
              RESP       (WS-RESPONSE)
           END-EXEC
           
           IF RESP-NORMAL
              GO TO 6060-EXIT
           END-IF

           .
       6060-ADD-ERROR.

           MOVE ER-3806                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT
           MOVE AL-UABON               TO STATEA (M1)
           MOVE -1                     TO STATEL (M1)

           .
       6060-EXIT.
           EXIT.

       7000-BROWSE-FWD.

           SET M1  TO +1
                                                                       
           EXEC CICS STARTBR                                            
               DATASET   ('ELCISB')
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

           MOVE LOW-VALUES             TO EL607AO
           MOVE ZERO                   TO PI-LINE-COUNT
           MOVE +1                     TO PI-BROWSE-SW
           MOVE +0                     TO CI1
           .
       7010-READ-NEXT.

           EXEC CICS READNEXT
               SET     (ADDRESS OF CLAIM-INTEREST-SB)
               DATASET ('ELCISB')
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

           IF SB-COMPANY-CD NOT = PI-COMPANY-CD
              GO TO 7030-END-FILE
           END-IF
           
           ADD +1                      TO WS-RECORD-COUNT
                                          CI1

           MOVE SB-CONTROL-PRIMARY     TO PI-CI-BOT-KEY
                                          PI-CI-KEY (CI1)

           MOVE SB-STATE               TO STATEO (M1)
           MOVE SB-PRODUCT             TO PRODO  (M1)
           MOVE SB-COVERAGE            TO COVO   (M1)
           MOVE SB-BREAKOUT-CODE       TO SBCDO  (M1)
           MOVE SB-CALC-END            TO ENDO   (M1)

           MOVE SB-CALC-START          TO STARTO (M1)
           MOVE SB-SCHED-CODE          TO CODEO  (M1)

           IF M1 = +1
              MOVE SB-CONTROL-PRIMARY  TO PI-CI-TOP-KEY
           END-IF

           IF M1 < +10
              SET M1 UP BY +1
              GO TO 7010-READ-NEXT
           END-IF

           IF PI-BROWSE-SW = +1
              EXEC CICS ENDBR
                   DATASET ('ELCISB')
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
               DATASET ('ELCISB')
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

           MOVE LOW-VALUES             TO EL607AO
           MOVE ZERO                   TO PI-LINE-COUNT
           MOVE +1                     TO PI-BROWSE-SW
                                                                        
           .
       8010-READ-PREV.

           EXEC CICS READPREV
               SET     (ADDRESS OF CLAIM-INTEREST-SB)
               DATASET ('ELCISB')
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

           IF SB-COMPANY-CD NOT = PI-COMPANY-CD
              GO TO 8020-END-FILE
           END-IF
           
           ADD +1                      TO WS-RECORD-COUNT
           SUBTRACT +1                 FROM CI1

           MOVE SB-CONTROL-PRIMARY     TO PI-CI-TOP-KEY
                                          PI-CI-KEY (CI1)

           MOVE SB-STATE               TO STATEO (M1)
           MOVE SB-PRODUCT             TO PRODO  (M1)
           MOVE SB-COVERAGE            TO COVO   (M1)
           MOVE SB-BREAKOUT-CODE       TO SBCDO  (M1)
           MOVE SB-CALC-END            TO ENDO   (M1)

           MOVE SB-CALC-START          TO STARTO (M1)
           MOVE SB-SCHED-CODE          TO CODEO  (M1)

           IF M1 = +10
              MOVE SB-CONTROL-PRIMARY  TO PI-CI-BOT-KEY
           END-IF

           IF M1 > +1
              SET M1                   DOWN BY +1
              GO TO 8010-READ-PREV
           END-IF

           IF PI-BROWSE-SW = +1
              EXEC CICS ENDBR
                   DATASET ('ELCISB')
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
               FROM   (EL607AO)                                         
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
               FROM   (EL607AO)                                         
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
