       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 EL606 .                              
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
      *    SCREENS     - EL606A - SELECTION CRITERIA                    
      *                                                                 
      *    ENTERED BY  - EL605 - LIFE CLAIM INTEREST MENU               
      *                                                                 
      *    EXIT TO     - EL605 - LIFE CLAIM INTEREST MENU               
      *                                                                 
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
      * 052605    2004040700004  PEMA  NEW PROGRAM
      ******************************************************************
           EJECT                                                        
       ENVIRONMENT DIVISION.                                            
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
                                                                        
       77  FILLER  PIC X(32)  VALUE '********************************'. 
       77  FILLER  PIC X(32)  VALUE '*   EL606  WORKING STORAGE     *'. 
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
           05  ELCISC-LENGTH               PIC S9(4)   VALUE +100.
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
               10  WS-EXCESS-DAYS          PIC S999   VALUE ZEROS.
           05  WS-CONTROL-FILE-KEY.                                     
               10  WS-CFK-COMPANY-ID       PIC X(3)    VALUE SPACES.    
               10  WS-CFK-RECORD-TYPE      PIC X       VALUE SPACES.    
               10  WS-CFK-STATE            PIC XX      VALUE SPACES.    
               10  WS-CFK-BENEFIT-CD       PIC XX      VALUE SPACES.    
               10  WS-CFK-SEQUENCE-NO      PIC S9(4)   VALUE ZERO COMP. 
                                                                        
           05  WS-MAPSET-NAME              PIC X(8)  VALUE 'EL606S'.    
           05  WS-MAP-NAME                 PIC X(8)  VALUE 'EL606A'.    
                                                                        
           05  FILLER                      REDEFINES                    
               WS-MAP-NAME.                                             
               10  FILLER                  PIC XX.                      
               10  WS-MAP-NUMBER           PIC X(4).                    
               10  FILLER                  PIC XX.                      
                                                                        
           05  THIS-PGM                    PIC X(8)  VALUE 'EL606'.     
                                                                        
           05  WS-JOURNAL-TYPE-ID          PIC XX      VALUE 'EL'.      
                                                                        
           05  WS-LOW-VALUES               PIC X VALUE LOW-VALUES.      
           05  WS-SPACE                    PIC X       VALUE SPACE.     
                                                                        
           05  WS-TRANS-ID                 PIC X(4)    VALUE 'EXAB'.    
                                                                        
           05  WS-TEMP-STORAGE-KEY.                                     
               10  WS-TS-TERM-ID           PIC X(4)    VALUE 'XXXX'.    
               10  FILLER                  PIC X(4)    VALUE '606'.     

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
               10  ER-7220                 PIC 9(4)   VALUE 7220.
               10  ER-9999                 PIC 9(4)   VALUE 9999.

           COPY ELCINTF.                                                
           12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                   
               16  PI-1ST-TIME-SW          PIC S9     COMP-3.           
               16  PI-MODE                 PIC X.
                   88  ADD-FUNCTION         VALUE 'A'.
               16  PI-CI-STATE             PIC XX.                      
               16  PI-CI-PROD              PIC XX.                      
               16  PI-CI-COV-TYPE          PIC XX.
               16  PI-CI-EXCESS-DAYS       PIC S999.
               16  PI-CI-TOP-KEY           PIC X(10).
               16  PI-CI-BOT-KEY           PIC X(10).
               16  PI-CI-KEYS OCCURS 10.
                   20  PI-CI-KEY           PIC X(10).
               16  PI-LAST-BENEFIT-NUMBER  PIC XX.                      
               16  PI-NEXT-BENEFIT-NUMBER  PIC XX.                      
               16  PI-LINE-COUNT           PIC S9(3)  COMP-3.           
               16  PI-BROWSE-SW            PIC S9     COMP-3.           
               16  PI-SHOW-SW              PIC S9     COMP-3.           
               16  PI-CHANGE-SW            PIC S9     COMP-3.           
               16  PI-UPDATE-KEY           PIC X(10).                   
                                                                        
               16  FILLER                  PIC X(553).                  
                                                                        
           EJECT                                                        
                                                                        
           COPY EL606S.                                                 
                                                                        
       01  EL606AO-R REDEFINES EL606AI.                                 
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
               10  TYPEL                   PIC S9(4)   COMP.            
               10  TYPEA                   PIC X.                       
               10  TYPEO                   PIC X.
               10  TYPEI                   REDEFINES                    
                   TYPEO                   PIC X.
               10  DATEL                   PIC S9(4)   COMP.            
               10  DATEA                   PIC X.                       
               10  DATEO                   PIC X(10).                   
               10  DATEI                   REDEFINES                    
                   DATEO                   PIC X(10).                   
               10  RTYPEL                  PIC S9(4)   COMP.            
               10  RTYPEA                  PIC X.                       
               10  RTYPEO                  PIC X.                       
               10  RTYPEI                  REDEFINES                    
                   RTYPEO                  PIC X.                       
               10  PDAYSL                  PIC S9(4)   COMP.            
               10  PDAYSA                  PIC X.                       
               10  PDAYSO                  PIC 999.                     
               10  PDAYSI                  REDEFINES                    
                   PDAYSO                  PIC XXX.
               10  EDAYSL                  PIC S9(4)   COMP.            
               10  EDAYSA                  PIC X.                       
               10  EDAYSO                  PIC 999.                     
               10  EDAYSI                  REDEFINES                    
                   EDAYSO                  PIC XXX.
               10  STYPEL                  PIC S9(4)   COMP.            
               10  STYPEA                  PIC X.                       
               10  STYPEO                  PIC X.                       
               10  STYPEI                  REDEFINES                    
                   STYPEO                  PIC X.                       
               10  SDAYSL                  PIC S9(4)   COMP.            
               10  SDAYSA                  PIC X.                       
               10  SDAYSO                  PIC 999.                     
               10  SDAYSI                  REDEFINES                    
                   SDAYSO                  PIC XXX.
               10  ETYPEL                  PIC S9(4)   COMP.            
               10  ETYPEA                  PIC X.                       
               10  ETYPEO                  PIC X.                       
               10  ETYPEI                  REDEFINES                    
                   ETYPEO                  PIC X.                       
               10  SBCDL                   PIC S9(4)  COMP.             
               10  SBCDA                   PIC X.                       
               10  SBCDO                   PIC XX.
               10  SBCDI                   REDEFINES                    
                   SBCDO                   PIC XX.
                                                                        
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
                                                                        
           EJECT                                                        
       LINKAGE SECTION.                                                 
       01  DFHCOMMAREA                     PIC X(1024).                 
                                                                        
                                                                        
           EJECT                                                        
           COPY ELCCISC.
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
              MOVE LOW-VALUES          TO EL606AO
              MOVE -1                  TO APFKL
              MOVE ER-0008             TO EMI-ERROR
              GO TO 8200-SEND-DATAONLY
           END-IF

           EXEC CICS RECEIVE
               INTO   (EL606AO)
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
                                                                        
           MOVE 'LOGIC ERROR HAS OCCURRED - PROGRAM EL606'              
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

           MOVE LOW-VALUES             TO EL606AO

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

           MOVE +1                     TO  PI-SHOW-SW.                  
                                                                        
           GO TO 7000-BROWSE-FWD
                                                                        
           .
      ***************************************************************** 
       3000-PROCESS-CHANGE.
      ***************************************************************** 

           SET M1                      TO +1
           MOVE +1                     TO CI1

           PERFORM UNTIL
              M1 > +10
              IF (TYPEL (M1) > +0)
                 OR (DATEL  (M1) > +0)
                 OR (RTYPEL (M1) > +0)
                 OR (PDAYSL (M1) > +0)
                 OR (STYPEL (M1) > +0)
                 OR (SDAYSL (M1) > +0)
                 OR (ETYPEL (M1) > +0)
                 OR (SBCDL  (M1) > +0)
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
              IF (TYPEL (M1) > +0)
                 OR (DATEL  (M1) > +0)
                 OR (RTYPEL (M1) > +0)
                 OR (PDAYSL (M1) > +0)
                 OR (STYPEL (M1) > +0)
                 OR (SDAYSL (M1) > +0)
                 OR (ETYPEL (M1) > +0)
                 OR (SBCDL  (M1) > +0)
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
                 OR (EDAYSL   (M1) > +0)
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
           
           IF TYPEL (M1) > +0
              IF TYPEI (M1) = 'I' OR 'R' OR 'P'
                 CONTINUE
              ELSE
                 MOVE ER-3800          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO TYPEA (M1)
                 MOVE -1               TO TYPEL (M1)
              END-IF
           ELSE
              IF ADD-FUNCTION
                 MOVE ER-3800          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO TYPEA (M1)
                 MOVE -1               TO TYPEL (M1)
              END-IF
           END-IF

           IF DATEL (M1) > +0
              MOVE DATEI (M1)          TO DEEDIT-FIELD
              EXEC CICS BIF DEEDIT
                  FIELD   (DEEDIT-FIELD)
                  LENGTH  (10)
              END-EXEC
              IF DEEDIT-FIELD-V0 NUMERIC
                 MOVE DEEDIT-FIELD-V0  TO DC-GREG-DATE-MDCY
                 MOVE 'M'              TO DC-OPTION-CODE
                 PERFORM 8500-DATE-CONVERSION
                                       THRU 8500-EXIT
                 IF NO-CONVERSION-ERROR
                    MOVE DC-BIN-DATE-1 TO WS-BIN-DATE (CI1)
                 ELSE
                    MOVE ER-3801       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT
                    MOVE DEEDIT-FIELD  TO DATEO (M1)
                    MOVE AL-UABON      TO DATEA (M1)
                    MOVE -1            TO DATEL (M1)
                 END-IF
              ELSE
                 MOVE ER-3801          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE DEEDIT-FIELD     TO DATEO (M1)
                 MOVE AL-UABON         TO DATEA (M1)
                 MOVE -1               TO DATEL (M1)
              END-IF
           ELSE
              IF ADD-FUNCTION
                 MOVE ER-3801          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO DATEA (M1)
                 MOVE -1               TO DATEL (M1)
              END-IF
           END-IF
                    
           IF RTYPEL (M1) > +0
              IF RTYPEI (M1) = 'I' OR 'R' OR 'P'
                 CONTINUE
              ELSE
                 MOVE ER-3800          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO RTYPEA (M1)
                 MOVE -1               TO RTYPEL (M1)
              END-IF
           ELSE
              IF ADD-FUNCTION
                 MOVE ER-3800          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO RTYPEA (M1)
                 MOVE -1               TO RTYPEL (M1)
              END-IF
           END-IF

           IF PDAYSL (M1) > +0
              IF PDAYSI (M1) NUMERIC
                 CONTINUE
              ELSE
                 MOVE ER-0491          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO PDAYSA (M1)
                 MOVE -1               TO PDAYSL (M1)
              END-IF
           ELSE
              IF ADD-FUNCTION
                 MOVE ER-0491          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO PDAYSA (M1)
                 MOVE -1               TO PDAYSL (M1)
              END-IF
           END-IF

           IF STYPEL (M1) > +0
              IF STYPEI (M1) = 'I' OR 'R' OR 'P'
                 CONTINUE
              ELSE
                 MOVE ER-3800          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO STYPEA (M1)
                 MOVE -1               TO STYPEL (M1)
              END-IF
           ELSE
              IF ADD-FUNCTION
                 MOVE ER-3800          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO STYPEA (M1)
                 MOVE -1               TO STYPEL (M1)
              END-IF
           END-IF

           IF SDAYSL (M1) > +0
              IF SDAYSI (M1) NUMERIC
                 CONTINUE
              ELSE
                 MOVE ER-0491          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO SDAYSA (M1)
                 MOVE -1               TO SDAYSL (M1)
              END-IF
           ELSE
              IF ADD-FUNCTION
                 MOVE ER-0491          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO SDAYSA (M1)
                 MOVE -1               TO SDAYSL (M1)
              END-IF
           END-IF

           IF ETYPEL (M1) > +0
              IF ETYPEI (M1) = 'P'
                 CONTINUE
              ELSE
                 MOVE ER-3802          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO ETYPEA (M1)
                 MOVE -1               TO ETYPEL (M1)
              END-IF
           ELSE
              IF ADD-FUNCTION
                 MOVE ER-3802          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO ETYPEA (M1)
                 MOVE -1               TO ETYPEL (M1)
              END-IF
           END-IF

           IF SBCDL (M1) > +0
              CONTINUE
           ELSE
              IF ADD-FUNCTION
                 MOVE ER-3803          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO SBCDA (M1)
                 MOVE -1               TO SBCDL (M1)
              END-IF
           END-IF

           .
       6010-EXIT.
           EXIT.
           
           
       6020-EDIT-KEY-DATA.
        
           IF (STATEL     (M1) > +0)
              AND (PRODL  (M1) > +0)
              AND (COVL   (M1) > +0)
              AND (EDAYSL (M1) > +0)
              CONTINUE
           ELSE
              MOVE ER-3804             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO STATEA (M1)
              MOVE AL-UABON            TO PRODA  (M1)
              MOVE AL-UABON            TO COVA   (M1)
              MOVE AL-UNBON            TO EDAYSA (M1)
              MOVE -1                  TO STATEL (M1)
              GO TO 8200-SEND-DATAONLY
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
              GO TO 8200-SEND-DATAONLY
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
              GO TO 8200-SEND-DATAONLY
           END-IF

           IF EDAYSI (M1) NUMERIC
              CONTINUE
           ELSE
              MOVE ER-0491             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO EDAYSA (M1)
              MOVE -1                  TO EDAYSL (M1)
              GO TO 8200-SEND-DATAONLY
           END-IF

           .
       6020-EXIT.
           EXIT.
           
       6030-UPDATE-RECORD.

           MOVE PI-CI-KEY (CI1)        TO WS-CONTROL-PRIMARY
           EXEC CICS READ
              UPDATE
              DATASET ('ELCISC')
              SET     (ADDRESS OF CLAIM-INTEREST-SC)
              RIDFLD  (WS-CONTROL-PRIMARY)
              RESP    (WS-RESPONSE)
           END-EXEC
           
           IF NOT RESP-NORMAL
              GO TO 6030-UPDATE-ERROR
           END-IF
           
           IF TYPEL (M1) > +0
              MOVE TYPEI (M1)          TO SC-S-TYPE
           END-IF

           IF DATEL (M1) > +0
              MOVE WS-BIN-DATE (CI1)   TO SC-SA-DATE
           END-IF
                    
           IF RTYPEL (M1) > +0
              MOVE RTYPEI (M1)         TO SC-R-TYPE
           END-IF

           IF PDAYSL (M1) > +0
              MOVE PDAYSI (M1)         TO SC-P-DAYS
           END-IF

           IF STYPEL (M1) > +0
              MOVE STYPEI (M1)         TO SC-CS-TYPE
           END-IF

           IF SDAYSL (M1) > +0
              MOVE SDAYSI (M1)         TO SC-CS-DAYS
           END-IF

           IF ETYPEL (M1) > +0
              MOVE ETYPEI (M1)         TO SC-CE-TYPE
           END-IF

           IF SBCDL  (M1) > +0
              MOVE SBCDI  (M1)         TO SC-BREAKOUT-CODE
           END-IF

           MOVE PI-PROCESSOR-ID        TO SC-LAST-MAINT-BY
           MOVE EIBTIME                TO SC-LAST-MAINT-HHMMSS
           MOVE SAVE-BIN-DATE          TO SC-LAST-MAINT-DT
              
           EXEC CICS REWRITE
              DATASET    ('ELCISC')
              FROM       (CLAIM-INTEREST-SC)
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

           MOVE PI-CI-KEY (CI1)        TO WS-CONTROL-PRIMARY
           EXEC CICS READ
              UPDATE
              DATASET ('ELCISC')
              SET     (ADDRESS OF CLAIM-INTEREST-SC)
              RIDFLD  (WS-CONTROL-PRIMARY)
              RESP    (WS-RESPONSE)
           END-EXEC
           
           IF NOT RESP-NORMAL
              GO TO 6050-DELETE-ERROR
           END-IF

           EXEC CICS DELETE
              DATASET ('ELCISC')
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
           MOVE EDAYSI (M1)            TO WS-EXCESS-DAYS
           
           EXEC CICS READ
              DATASET ('ELCISC')
              SET     (ADDRESS OF CLAIM-INTEREST-SC)
              RIDFLD  (WS-CONTROL-PRIMARY)
              RESP    (WS-RESPONSE)
           END-EXEC
           
           IF RESP-NORMAL
              GO TO 6060-ADD-ERROR
           END-IF
           
           EXEC CICS GETMAIN
              LENGTH   (ELCISC-LENGTH)
              SET      (ADDRESS OF CLAIM-INTEREST-SC)
              INITIMG  (GETMAIN-SPACE)
           END-EXEC

           MOVE 'SC'                   TO SC-RECORD-ID
           MOVE PI-COMPANY-CD          TO SC-COMPANY-CD
           MOVE STATEI (M1)            TO SC-STATE
           MOVE PRODI  (M1)            TO SC-PRODUCT
           MOVE COVI   (M1)            TO SC-COVERAGE
           MOVE TYPEI  (M1)            TO SC-S-TYPE
           MOVE EDAYSI (M1)            TO SC-EXCESS-DAYS
           MOVE WS-BIN-DATE (CI1)      TO SC-SA-DATE
           MOVE RTYPEI (M1)            TO SC-R-TYPE
           MOVE PDAYSI (M1)            TO SC-P-DAYS
           MOVE STYPEI (M1)            TO SC-CS-TYPE
           MOVE SDAYSI (M1)            TO SC-CS-DAYS
           MOVE ETYPEI (M1)            TO SC-CE-TYPE
           MOVE SBCDI  (M1)            TO SC-BREAKOUT-CODE
           MOVE PI-PROCESSOR-ID        TO SC-LAST-MAINT-BY
           MOVE EIBTIME                TO SC-LAST-MAINT-HHMMSS
           MOVE SAVE-BIN-DATE          TO SC-LAST-MAINT-DT
           
           EXEC CICS WRITE
              DATASET    ('ELCISC')
              FROM       (CLAIM-INTEREST-SC)
              RIDFLD     (SC-CONTROL-PRIMARY)
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
               DATASET   ('ELCISC')
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

           MOVE LOW-VALUES             TO EL606AO
           MOVE ZERO                   TO PI-LINE-COUNT
           MOVE +1                     TO PI-BROWSE-SW
           MOVE +0                     TO CI1
           .
       7010-READ-NEXT.

           EXEC CICS READNEXT
               SET     (ADDRESS OF CLAIM-INTEREST-SC)
               DATASET ('ELCISC')
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

           IF SC-COMPANY-CD NOT = PI-COMPANY-CD
              GO TO 7030-END-FILE
           END-IF
           
           ADD +1                      TO WS-RECORD-COUNT
                                          CI1

           MOVE SC-CONTROL-PRIMARY     TO PI-CI-BOT-KEY
                                          PI-CI-KEY (CI1)

           MOVE SC-STATE               TO STATEO (M1)
           MOVE SC-PRODUCT             TO PRODO  (M1)
           MOVE SC-COVERAGE            TO COVO   (M1)
           MOVE SC-S-TYPE              TO TYPEO  (M1)
           MOVE SC-EXCESS-DAYS         TO EDAYSO (M1)

           MOVE SC-SA-DATE             TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO DATEO  (M1)
           END-IF

           MOVE SC-R-TYPE              TO RTYPEO (M1)
           MOVE SC-P-DAYS              TO PDAYSO (M1)
           MOVE SC-CS-TYPE             TO STYPEO (M1)
           MOVE SC-CS-DAYS             TO SDAYSO (M1)
           MOVE SC-CE-TYPE             TO ETYPEO (M1)
           MOVE SC-BREAKOUT-CODE       TO SBCDO  (M1)

           IF M1 = +1
              MOVE SC-CONTROL-PRIMARY  TO PI-CI-TOP-KEY
           END-IF

           IF M1 < +10
              SET M1 UP BY +1
              GO TO 7010-READ-NEXT
           END-IF

           IF PI-BROWSE-SW = +1
              EXEC CICS ENDBR
                   DATASET ('ELCISC')
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
               DATASET ('ELCISC')
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

           MOVE LOW-VALUES             TO EL606AO
           MOVE ZERO                   TO PI-LINE-COUNT
           MOVE +1                     TO PI-BROWSE-SW
                                                                        
           .
       8010-READ-PREV.

           EXEC CICS READPREV
               SET     (ADDRESS OF CLAIM-INTEREST-SC)
               DATASET ('ELCISC')
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

           IF SC-COMPANY-CD NOT = PI-COMPANY-CD
              GO TO 8020-END-FILE
           END-IF
           
           ADD +1                      TO WS-RECORD-COUNT
           SUBTRACT +1                 FROM CI1

           MOVE SC-CONTROL-PRIMARY     TO PI-CI-TOP-KEY
                                          PI-CI-KEY (CI1)

           MOVE SC-STATE               TO STATEO (M1)
           MOVE SC-PRODUCT             TO PRODO  (M1)
           MOVE SC-COVERAGE            TO COVO   (M1)
           MOVE SC-S-TYPE              TO TYPEO  (M1)
           MOVE SC-EXCESS-DAYS         TO EDAYSO (M1)

           MOVE SC-SA-DATE             TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO DATEO  (M1)
           END-IF

           MOVE SC-R-TYPE              TO RTYPEO (M1)
           MOVE SC-P-DAYS              TO PDAYSO (M1)
           MOVE SC-CS-TYPE             TO STYPEO (M1)
           MOVE SC-CS-DAYS             TO SDAYSO (M1)
           MOVE SC-CE-TYPE             TO ETYPEO (M1)
           MOVE SC-BREAKOUT-CODE       TO SBCDO  (M1)

           IF M1 = +10
              MOVE SC-CONTROL-PRIMARY  TO PI-CI-BOT-KEY
           END-IF

           IF M1 > +1
              SET M1                   DOWN BY +1
              GO TO 8010-READ-PREV
           END-IF

           IF PI-BROWSE-SW = +1
              EXEC CICS ENDBR
                   DATASET ('ELCISC')
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
               FROM   (EL606AO)                                         
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
               FROM   (EL606AO)                                         
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
                                                                        
       9999-LAST-PARAGRAPH.                                     
           GOBACK.                                                      
