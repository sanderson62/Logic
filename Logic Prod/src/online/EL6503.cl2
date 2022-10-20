       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 EL6503.
      *                            VMOD=2.001.                          
      *AUTHOR.    PABLO.                                                
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
      *    FOR THE BRANCH LOCATION AND SHIPPING ADDRESSES FOR PRODUCERS
      *                                                                 
      *    SCREENS     - EL650I - BRANCH LOCAL AND SHIPPING ADDR MAINT  
      *                                                                 
      *    ENTERED BY  - EL650 - ACCOUNT MAINTENANCE MENU               
      *                                                                 
      *    EXIT TO     - EL650 - ACCOUNT MAINTENANCE MENU               
      *                                                                 
      *    COMMAREA    - PASSED                                         
      *                                                                 
      *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL650.  ON     
      *                  FIRST ENTRY, READ ERACNT TO DISPLAY INFO,  THE 
      *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
      *                  ENTRIES (XCTL FROM CICS VIA EXC7) THE SCREEN   
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
      * 110706  CR2006071700004  PEMA  NEW PROGRAM
111407* 111407  CR2007010300001  PEMA  USE ACCT SEC FOR THIS PROGRAM
052918* 052918  CR2018040600002  PEMA  ADD REFUND DIRECT INDICATOR
      ******************************************************************

       ENVIRONMENT DIVISION.                                            
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
                                                                        
       77  FILLER  PIC X(32)  VALUE '********************************'. 
       77  FILLER  PIC X(32)  VALUE '*   EL6503 WORKING STORAGE     *'. 
       77  FILLER  PIC X(32)  VALUE '***********VMOD=2.001 **********'. 
       77  CI1                PIC S999  COMP-3 VALUE +0.
       77  B1                 PIC S999  COMP-3 VALUE +0.
       77  WS-BROWSE-SW                PIC X   VALUE SPACES.
           88  ERACNT-BROWSE-STARTED         VALUE 'Y'.

           COPY ELCSCTM.                                                
           COPY ELCSCRTY.                                               
                                                                        
       01  WS-DATE-AREA.                                                
           05  SAVE-DATE           PIC X(8)    VALUE SPACES.            
           05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            
                                                                        
       01  WS-INC-DATE                 PIC XX  VALUE LOW-VALUES.
       01  WS-RPT-DATE                 PIC XX  VALUE LOW-VALUES.
       01  WS-EST-DATE                 PIC XX  VALUE LOW-VALUES.
       01  WS-PRF-DATE                 PIC XX  VALUE LOW-VALUES.
       01  WS-LSTPD-DATE               PIC XX  VALUE LOW-VALUES.
       
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
           05  ERACNT-LENGTH               PIC S9(4)   VALUE +120.
           05  GETMAIN-SPACE               PIC  X      VALUE SPACE.
                                                                        
       01  FILLER.                                                      
           12  DEEDIT-FIELD        PIC  X(10).
           12  DEEDIT-FIELD-V0  REDEFINES
               DEEDIT-FIELD        PIC S9(10).
           12  DEEDIT-FIELD-V2  REDEFINES
               DEEDIT-FIELD        PIC 9(8)V99.
           05  WS-BIN-DATES OCCURS 10.
               10  WS-BIN-DATE         PIC XX.
           05  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL              VALUE +00.
               88  RESP-ERROR               VALUE +01.
               88  RESP-NOTFND              VALUE +13.
               88  RESP-NOTOPEN             VALUE +19.
               88  RESP-ENDFILE             VALUE +20.
           05  WS-SAVE-ERACNT-KEY      PIC X(23)   VALUE LOW-VALUES.
           05  WS-CONTROL-PRIMARY.
               10  WS-COMPANY-CD           PIC X      VALUE SPACES.
               10  WS-CARRIER              PIC X      VALUE SPACES.
               10  WS-GROUPING             PIC X(6)   VALUE SPACES.
               10  WS-STATE                PIC XX     VALUE SPACES.
               10  WS-ACCOUNT              PIC X(10)  VALUE SPACES.
               10  WS-REC-TYPE             PIC X      VALUE SPACES.
               10  WS-SEQ-NO               PIC S9(4)  VALUE +0 COMP.
           05  WS-CONTROL-FILE-KEY.
               10  WS-CFK-COMPANY-ID       PIC X(3)    VALUE SPACES.
               10  WS-CFK-RECORD-TYPE      PIC X       VALUE SPACES.    
               10  WS-CFK-STATE            PIC XX      VALUE SPACES.    
               10  WS-CFK-BENEFIT-CD       PIC XX      VALUE SPACES.    
               10  WS-CFK-SEQUENCE-NO      PIC S9(4)   VALUE ZERO COMP. 
                                                                        
           05  WS-MAPSET-NAME              PIC X(8)  VALUE 'EL6503S'.    
           05  WS-MAP-NAME                 PIC X(8)  VALUE 'EL6503A'.    
                                                                        
           05  FILLER                      REDEFINES                    
               WS-MAP-NAME.                                             
               10  FILLER                  PIC XX.                      
               10  WS-MAP-NUMBER           PIC X(4).                    
               10  FILLER                  PIC XX.                      
                                                                        
           05  THIS-PGM                    PIC X(8)  VALUE 'EL6503'.
                                                                        
           05  WS-JOURNAL-TYPE-ID          PIC XX      VALUE 'EL'.      
                                                                        
           05  WS-LOW-VALUES               PIC X VALUE LOW-VALUES.      
           05  WS-SPACE                    PIC X       VALUE SPACE.     
                                                                        
           05  WS-TRANS-ID                 PIC X(4)    VALUE 'EXC7'.    
                                                                        
           05  WS-TEMP-STORAGE-KEY.                                     
               10  WS-TS-TERM-ID           PIC X(4)    VALUE 'XXXX'.    
               10  FILLER                  PIC X(4)    VALUE '6503'.

           05  WS-ERROR-MESSAGE-AREA.                                   
               10  ER-0000             PIC 9(4)   VALUE 0000.
               10  ER-0004             PIC 9(4)   VALUE 0004.
               10  ER-0006             PIC 9(4)   VALUE 0006.
               10  ER-0008             PIC 9(4)   VALUE 0008.
               10  ER-0023             PIC 9(4)   VALUE 0023.
               10  ER-0029             PIC 9(4)   VALUE 0029.
               10  ER-0070             PIC 9(4)   VALUE 0070.
052918         10  ER-3271             PIC 9(4)   VALUE 3271.
               10  ER-9999             PIC 9(4)   VALUE 9999.

                                       COPY ELCINTF.
                                       COPY ELC650PI.
                                       COPY EL6503S.
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
                                                                        
                                       COPY ERCACNT.
           EJECT                                                        
       LINKAGE SECTION.                                                 

       01  DFHCOMMAREA                 PIC X(1024).


       PROCEDURE DIVISION.                                              
                                                                        
           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8500-EXIT
           MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE
           MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE
                                                                        
           MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK
                                                                        
           MOVE +1                     TO EMI-NUMBER-OF-LINES
                                          EMI-SWITCH2

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

111407     IF NOT DISPLAY-CAP
111407        MOVE 'READ'              TO SM-READ
111407        PERFORM 9995-SECURITY-VIOLATION
111407        MOVE ER-0070             TO EMI-ERROR
111407        PERFORM 9900-ERROR-FORMAT
111407                                 THRU 9900-EXIT
111407        GO TO 8100-SEND-INITIAL-MAP
111407     END-IF

           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
              MOVE LOW-VALUES          TO EL6503AO
              MOVE -1                  TO PFKEYL
              MOVE ER-0008             TO EMI-ERROR
              GO TO 8200-SEND-DATAONLY
           END-IF

           EXEC CICS RECEIVE
               INTO   (EL6503AI)
               MAPSET (WS-MAPSET-NAME)
               MAP    (WS-MAP-NAME)
           END-EXEC

           IF PFKEYL > ZERO
              IF EIBAID NOT = DFHENTER
                 MOVE ER-0004          TO EMI-ERROR
                 MOVE AL-UNBOF         TO PFKEYA
                 MOVE -1               TO PFKEYL
                 GO TO 8200-SEND-DATAONLY
              ELSE
                 IF PFKEYO NUMERIC AND
                    (PFKEYO > ZERO AND LESS '25')
                    MOVE PF-VALUES (PFKEYI)
                                       TO  EIBAID
                 ELSE
                    MOVE ER-0029       TO  EMI-ERROR
                    MOVE AL-UNBOF      TO  PFKEYA
                    MOVE -1            TO  PFKEYL
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

           IF EIBAID = DFHENTER
              CONTINUE
           ELSE
              MOVE ER-0008             TO EMI-ERROR
              MOVE -1                  TO PFKEYL
              GO TO 8200-SEND-DATAONLY
           END-IF

111407     IF NOT MODIFY-CAP
111407        MOVE 'UPDATE'            TO SM-READ
111407        PERFORM 9995-SECURITY-VIOLATION
111407                                 THRU 9995-EXIT
111407        MOVE ER-0070             TO EMI-ERROR
111407        PERFORM 9900-ERROR-FORMAT
111407                                 THRU 9900-EXIT
111407        MOVE AL-UANON            TO CARRIERA GROUPA
111407                                    STATEA  ACCTNOA
111407        GO TO 8100-SEND-INITIAL-MAP
111407     END-IF

           EVALUATE MAINTYPI
              WHEN 'D'
                 PERFORM 2000-DELETE-ERACNT
                                       THRU 2000-EXIT
                 MOVE ER-0000          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 GO TO 8100-SEND-INITIAL-MAP
              WHEN 'C'
                 PERFORM 3000-CHANGE-ERACNT
                                       THRU 3000-EXIT
                 MOVE ER-0000          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              WHEN 'A'
                 PERFORM 4000-ADD-ERACNT
                                       THRU 4000-EXIT
                 MOVE ER-0000          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              WHEN OTHER
                 MOVE ER-0023          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT

                 MOVE -1               TO MAINTYPL
                 MOVE AL-UABON         TO MAINTYPA
           END-EVALUATE

           .
       1000-INITIAL-SCREEN.

      *    MOVE SPACES                 TO PI-PROGRAM-WORK-AREA
           MOVE PI-COMPANY-CD          TO WS-COMPANY-CD
           MOVE PI-ACCT-CARRIER        TO WS-CARRIER
                                          CARRIERO
           MOVE PI-ACCT-GROUPING       TO WS-GROUPING
                                          GROUPO
           MOVE PI-ACCT-STATE          TO WS-STATE
                                          STATEO
           MOVE PI-ACCT-ACCOUNT        TO WS-ACCOUNT
                                          ACCTNOO

           MOVE AL-UANON               TO CARRIERA
                                          GROUPA
                                          STATEA
                                          ACCTNOA
           MOVE ' '                    TO MAINTYPO
           MOVE -1                     TO MAINTYPL

           MOVE '2'                    TO WS-REC-TYPE
           MOVE +0                     TO WS-SEQ-NO
           MOVE WS-CONTROL-PRIMARY     TO WS-SAVE-ERACNT-KEY

           EXEC CICS STARTBR                                            
               DATASET   ('ERACNT')
               RIDFLD    (WS-CONTROL-PRIMARY)
               GTEQ
               RESP      (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              GO TO 8100-SEND-INITIAL-MAP
           END-IF

           SET ERACNT-BROWSE-STARTED   TO TRUE

           EXEC CICS READNEXT
               DATASET  ('ERACNT')
               INTO     (NOTE-FILE)
               RIDFLD   (WS-CONTROL-PRIMARY)
               RESP     (WS-RESPONSE)
           END-EXEC

           IF (RESP-NORMAL)
              AND (WS-CONTROL-PRIMARY (1:20) =
                 WS-SAVE-ERACNT-KEY (1:20))
              IF (WS-REC-TYPE = '2')
                 AND (WS-SEQ-NO = 1)
                 MOVE NT-BRANCH-LOC-LINE
                                       TO BLINE1O
                 PERFORM 1010-BUILD-LAST-MAINT-INFO
                                       THRU 1010-EXIT
                 EXEC CICS READNEXT
                    DATASET  ('ERACNT')
                    INTO     (NOTE-FILE)
                    RIDFLD   (WS-CONTROL-PRIMARY)
                    RESP     (WS-RESPONSE)
                 END-EXEC
              END-IF
           ELSE
              MOVE ER-0006             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF

           IF (RESP-NORMAL)
              AND (WS-CONTROL-PRIMARY (1:20) =
                 WS-SAVE-ERACNT-KEY (1:20))
              IF (WS-REC-TYPE = '2')
                 AND (WS-SEQ-NO = 2)
                 MOVE NT-BRANCH-LOC-LINE
                                       TO BLINE2O
                 PERFORM 1010-BUILD-LAST-MAINT-INFO
                                       THRU 1010-EXIT
                 EXEC CICS READNEXT
                    DATASET  ('ERACNT')
                    INTO     (NOTE-FILE)
                    RIDFLD   (WS-CONTROL-PRIMARY)
                    RESP     (WS-RESPONSE)
                 END-EXEC
              END-IF
           END-IF

052918     IF (RESP-NORMAL)
052918        AND (WS-CONTROL-PRIMARY (1:20) =
052918           WS-SAVE-ERACNT-KEY (1:20))
052918        IF (WS-REC-TYPE = '2')
052918           AND (WS-SEQ-NO = 3)
052918           MOVE NT-account-special TO actspco
052918           PERFORM 1010-BUILD-LAST-MAINT-INFO
052918                                 THRU 1010-EXIT
052918           EXEC CICS READNEXT
052918              DATASET  ('ERACNT')
052918              INTO     (NOTE-FILE)
052918              RIDFLD   (WS-CONTROL-PRIMARY)
052918              RESP     (WS-RESPONSE)
052918           END-EXEC
052918        END-IF
052918     END-IF

           IF (RESP-NORMAL)
              AND (WS-CONTROL-PRIMARY (1:20) =
                 WS-SAVE-ERACNT-KEY (1:20))
              IF (WS-REC-TYPE = '3')
                 AND (WS-SEQ-NO = 1)
                 MOVE NT-SHIPPING-LINE TO SNAME1O
                 PERFORM 1010-BUILD-LAST-MAINT-INFO
                                       THRU 1010-EXIT
                 EXEC CICS READNEXT
                    DATASET  ('ERACNT')
                    INTO     (NOTE-FILE)
                    RIDFLD   (WS-CONTROL-PRIMARY)
                    RESP     (WS-RESPONSE)
                 END-EXEC
              END-IF
           END-IF

           IF (RESP-NORMAL)
              AND (WS-CONTROL-PRIMARY (1:20) =
                 WS-SAVE-ERACNT-KEY (1:20))
              IF (WS-REC-TYPE = '3')
                 AND (WS-SEQ-NO = 2)
                 MOVE NT-SHIPPING-LINE TO SNAME2O
                 PERFORM 1010-BUILD-LAST-MAINT-INFO
                                       THRU 1010-EXIT
                 EXEC CICS READNEXT
                    DATASET  ('ERACNT')
                    INTO     (NOTE-FILE)
                    RIDFLD   (WS-CONTROL-PRIMARY)
                    RESP     (WS-RESPONSE)
                 END-EXEC
              END-IF
           END-IF

           IF (RESP-NORMAL)
              AND (WS-CONTROL-PRIMARY (1:20) =
                 WS-SAVE-ERACNT-KEY (1:20))
              IF (WS-REC-TYPE = '3')
                 AND (WS-SEQ-NO = 3)
                 MOVE NT-SHIPPING-LINE TO SADDR1O
                 PERFORM 1010-BUILD-LAST-MAINT-INFO
                                       THRU 1010-EXIT
                 EXEC CICS READNEXT
                    DATASET  ('ERACNT')
                    INTO     (NOTE-FILE)
                    RIDFLD   (WS-CONTROL-PRIMARY)
                    RESP     (WS-RESPONSE)
                 END-EXEC
              END-IF
           END-IF

           IF (RESP-NORMAL)
              AND (WS-CONTROL-PRIMARY (1:20) =
                 WS-SAVE-ERACNT-KEY (1:20))
              IF (WS-REC-TYPE = '3')
                 AND (WS-SEQ-NO = 4)
                 MOVE NT-SHIPPING-LINE TO SADDR2O
                 PERFORM 1010-BUILD-LAST-MAINT-INFO
                                       THRU 1010-EXIT
                 EXEC CICS READNEXT
                    DATASET  ('ERACNT')
                    INTO     (NOTE-FILE)
                    RIDFLD   (WS-CONTROL-PRIMARY)
                    RESP     (WS-RESPONSE)
                 END-EXEC
              END-IF
           END-IF

           IF (RESP-NORMAL)
              AND (WS-CONTROL-PRIMARY (1:20) =
                 WS-SAVE-ERACNT-KEY (1:20))
              IF (WS-REC-TYPE = '3')
                 AND (WS-SEQ-NO = 5)
                 MOVE NT-SHIPPING-LINE TO SADDR3O
                 PERFORM 1010-BUILD-LAST-MAINT-INFO
                                       THRU 1010-EXIT
                 EXEC CICS READNEXT
                    DATASET  ('ERACNT')
                    INTO     (NOTE-FILE)
                    RIDFLD   (WS-CONTROL-PRIMARY)
                    RESP     (WS-RESPONSE)
                 END-EXEC
              END-IF
           END-IF

           IF (RESP-NORMAL)
              AND (WS-CONTROL-PRIMARY (1:20) =
                 WS-SAVE-ERACNT-KEY (1:20))
              IF (WS-REC-TYPE = '3')
                 AND (WS-SEQ-NO = 6)
                 MOVE NT-SHIPPING-LINE TO SCITYO
                 MOVE NT-SHIP-STATE    TO SSTATEO
                 MOVE NT-SHIP-ZIP      TO SZIPO
                 PERFORM 1010-BUILD-LAST-MAINT-INFO
                                       THRU 1010-EXIT
              END-IF
           END-IF

           IF ERACNT-BROWSE-STARTED
              EXEC CICS ENDBR
                 DATASET    ('ERACNT')
              END-EXEC
           END-IF

           MOVE -1                     TO MAINTYPL
           GO TO 8100-SEND-INITIAL-MAP

           .
       1000-EXIT.
           EXIT.
       1010-BUILD-LAST-MAINT-INFO.

           MOVE NT-LAST-MAINT-BY       TO LSTUSRO
           MOVE NT-LAST-MAINT-HHMMSS   TO TIME-IN
           MOVE TIME-OUT               TO LSTTIMEO

           MOVE NT-LAST-MAINT-DT       TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO LSTDTEO
           END-IF

           .
       1010-EXIT.
           EXIT.

       2000-DELETE-ERACNT.

           MOVE PI-COMPANY-CD          TO WS-COMPANY-CD
           MOVE PI-ACCT-CARRIER        TO WS-CARRIER
           MOVE PI-ACCT-GROUPING       TO WS-GROUPING
           MOVE PI-ACCT-STATE          TO WS-STATE
           MOVE PI-ACCT-ACCOUNT        TO WS-ACCOUNT
           MOVE '2'                    TO WS-REC-TYPE
           MOVE +1                     TO WS-SEQ-NO

           EXEC CICS READ
              UPDATE
              DATASET  ('ERACNT')
              INTO     (NOTE-FILE)
              RIDFLD   (WS-CONTROL-PRIMARY)
              RESP     (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              EXEC CICS DELETE
                 DATASET    ('ERACNT')
                 RESP       (WS-RESPONSE)
              END-EXEC
           END-IF

           MOVE PI-COMPANY-CD          TO WS-COMPANY-CD
           MOVE PI-ACCT-CARRIER        TO WS-CARRIER
           MOVE PI-ACCT-GROUPING       TO WS-GROUPING
           MOVE PI-ACCT-STATE          TO WS-STATE
           MOVE PI-ACCT-ACCOUNT        TO WS-ACCOUNT
           MOVE '2'                    TO WS-REC-TYPE
           MOVE +2                     TO WS-SEQ-NO

           EXEC CICS READ
              UPDATE
              DATASET  ('ERACNT')
              INTO     (NOTE-FILE)
              RIDFLD   (WS-CONTROL-PRIMARY)
              RESP     (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              EXEC CICS DELETE
                 DATASET    ('ERACNT')
                 RESP       (WS-RESPONSE)
              END-EXEC
           END-IF

052918     MOVE PI-COMPANY-CD          TO WS-COMPANY-CD
052918     MOVE PI-ACCT-CARRIER        TO WS-CARRIER
052918     MOVE PI-ACCT-GROUPING       TO WS-GROUPING
052918     MOVE PI-ACCT-STATE          TO WS-STATE
052918     MOVE PI-ACCT-ACCOUNT        TO WS-ACCOUNT
052918     MOVE '2'                    TO WS-REC-TYPE
052918     MOVE +3                     TO WS-SEQ-NO
052918
052918     EXEC CICS READ
052918        UPDATE
052918        DATASET  ('ERACNT')
052918        INTO     (NOTE-FILE)
052918        RIDFLD   (WS-CONTROL-PRIMARY)
052918        RESP     (WS-RESPONSE)
052918     END-EXEC
052918
052918     IF RESP-NORMAL
052918        EXEC CICS DELETE
052918           DATASET    ('ERACNT')
052918           RESP       (WS-RESPONSE)
052918        END-EXEC
052918     END-IF

           PERFORM VARYING B1 FROM +1 BY +1 UNTIL
              (B1 > +6)
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE B1                  TO WS-SEQ-NO

              EXEC CICS READ
                 UPDATE
                 DATASET  ('ERACNT')
                 INTO     (NOTE-FILE)
                 RIDFLD   (WS-CONTROL-PRIMARY)
                 RESP     (WS-RESPONSE)
              END-EXEC

              IF RESP-NORMAL
                 EXEC CICS DELETE
                    DATASET    ('ERACNT')
                    RESP       (WS-RESPONSE)
                 END-EXEC
              END-IF
           END-PERFORM

           .
       2000-EXIT.
           EXIT.

       3000-CHANGE-ERACNT.

052918     perform 3500-edit-screen-data
052918                                 thru 3500-exit
052918
052918     if not emi-no-errors
052918        go to 8200-send-dataonly
052918     end-if

           IF BLINE1L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '2'                 TO WS-REC-TYPE
              MOVE +1                  TO WS-SEQ-NO

              EXEC CICS READ
                 UPDATE
                 DATASET  ('ERACNT')
                 INTO     (NOTE-FILE)
                 RIDFLD   (WS-CONTROL-PRIMARY)
                 RESP     (WS-RESPONSE)
              END-EXEC

              IF RESP-NORMAL
                 MOVE BLINE1I          TO NT-BRANCH-LOC-LINE
                 PERFORM 5020-REWRITE-ERACNT
                                       THRU 5020-EXIT
              ELSE
                 PERFORM 4010-ADD-BLINE1
                                       THRU 4010-EXIT
              END-IF
           END-IF

           IF BLINE2L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '2'                 TO WS-REC-TYPE
              MOVE +2                  TO WS-SEQ-NO

              EXEC CICS READ
                 UPDATE
                 DATASET  ('ERACNT')
                 INTO     (NOTE-FILE)
                 RIDFLD   (WS-CONTROL-PRIMARY)
                 RESP     (WS-RESPONSE)
              END-EXEC

              IF RESP-NORMAL
                 MOVE BLINE2I          TO NT-BRANCH-LOC-LINE
                 PERFORM 5020-REWRITE-ERACNT
                                       THRU 5020-EXIT
              ELSE
                 PERFORM 4012-ADD-BLINE2
                                       THRU 4012-EXIT
              END-IF
           END-IF

052918     IF actspcl > ZEROS
052918        MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
052918        MOVE PI-ACCT-CARRIER     TO WS-CARRIER
052918        MOVE PI-ACCT-GROUPING    TO WS-GROUPING
052918        MOVE PI-ACCT-STATE       TO WS-STATE
052918        MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
052918        MOVE '2'                 TO WS-REC-TYPE
052918        MOVE +3                  TO WS-SEQ-NO
052918
052918        EXEC CICS READ
052918           UPDATE
052918           DATASET  ('ERACNT')
052918           INTO     (NOTE-FILE)
052918           RIDFLD   (WS-CONTROL-PRIMARY)
052918           RESP     (WS-RESPONSE)
052918        END-EXEC
052918
052918        IF RESP-NORMAL
052918           MOVE actspci          TO NT-account-special
052918           PERFORM 5020-REWRITE-ERACNT
052918                                 THRU 5020-EXIT
052918        ELSE
052918           PERFORM 4013-ADD-actspc
052918                                 THRU 4013-EXIT
052918        END-IF
052918     END-IF

           IF SNAME1L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +1                  TO WS-SEQ-NO

              EXEC CICS READ
                 UPDATE
                 DATASET  ('ERACNT')
                 INTO     (NOTE-FILE)
                 RIDFLD   (WS-CONTROL-PRIMARY)
                 RESP     (WS-RESPONSE)
              END-EXEC

              IF RESP-NORMAL
                 MOVE SNAME1I          TO NT-SHIPPING-LINE
                 PERFORM 5020-REWRITE-ERACNT
                                       THRU 5020-EXIT
              ELSE
                 PERFORM 4014-ADD-SNAME1
                                       THRU 4014-EXIT
              END-IF
           END-IF

           IF SNAME2L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +2                  TO WS-SEQ-NO

              EXEC CICS READ
                 UPDATE
                 DATASET  ('ERACNT')
                 INTO     (NOTE-FILE)
                 RIDFLD   (WS-CONTROL-PRIMARY)
                 RESP     (WS-RESPONSE)
              END-EXEC

              IF RESP-NORMAL
                 MOVE SNAME2I          TO NT-SHIPPING-LINE
                 PERFORM 5020-REWRITE-ERACNT
                                       THRU 5020-EXIT
              ELSE
                 PERFORM 4016-ADD-SNAME2
                                       THRU 4016-EXIT
              END-IF
           END-IF

           IF SADDR1L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +3                  TO WS-SEQ-NO

              EXEC CICS READ
                 UPDATE
                 DATASET  ('ERACNT')
                 INTO     (NOTE-FILE)
                 RIDFLD   (WS-CONTROL-PRIMARY)
                 RESP     (WS-RESPONSE)
              END-EXEC

              IF RESP-NORMAL
                 MOVE SADDR1I          TO NT-SHIPPING-LINE
                 PERFORM 5020-REWRITE-ERACNT
                                       THRU 5020-EXIT
              ELSE
                 PERFORM 4018-ADD-SADDR1
                                       THRU 4018-EXIT
              END-IF
           END-IF

           IF SADDR2L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +4                  TO WS-SEQ-NO

              EXEC CICS READ
                 UPDATE
                 DATASET  ('ERACNT')
                 INTO     (NOTE-FILE)
                 RIDFLD   (WS-CONTROL-PRIMARY)
                 RESP     (WS-RESPONSE)
              END-EXEC

              IF RESP-NORMAL
                 MOVE SADDR2I          TO NT-SHIPPING-LINE
                 PERFORM 5020-REWRITE-ERACNT
                                       THRU 5020-EXIT
              ELSE
                 PERFORM 4020-ADD-SADDR2
                                       THRU 4020-EXIT
              END-IF
           END-IF

           IF SADDR3L    > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +5                  TO WS-SEQ-NO

              EXEC CICS READ
                 UPDATE
                 DATASET  ('ERACNT')
                 INTO     (NOTE-FILE)
                 RIDFLD   (WS-CONTROL-PRIMARY)
                 RESP     (WS-RESPONSE)
              END-EXEC

              IF RESP-NORMAL
                 MOVE SADDR3I          TO NT-SHIPPING-LINE
                 PERFORM 5020-REWRITE-ERACNT
                                       THRU 5020-EXIT
              ELSE
                 PERFORM 4022-ADD-SADDR3
                                       THRU 4022-EXIT
              END-IF
           END-IF

           IF (SCITYL     > ZEROS)
              OR (SSTATEL > ZEROS)
              OR (SZIPL   > ZEROS)
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +6                  TO WS-SEQ-NO

              EXEC CICS READ
                 UPDATE
                 DATASET  ('ERACNT')
                 INTO     (NOTE-FILE)
                 RIDFLD   (WS-CONTROL-PRIMARY)
                 RESP     (WS-RESPONSE)
              END-EXEC

              IF RESP-NORMAL
                 IF SCITYL > ZEROS
                    MOVE SCITYI        TO NT-SHIPPING-LINE
                 END-IF
                 IF SSTATEL > ZEROS
                    MOVE SSTATEI       TO NT-SHIP-STATE
                 END-IF
                 IF SZIPL > ZEROS
                    MOVE SZIPI         TO NT-SHIP-ZIP
                 END-IF
                 PERFORM 5020-REWRITE-ERACNT
                                       THRU 5020-EXIT
              ELSE
                 PERFORM 4024-ADD-SCITYSTZP
                                       THRU 4024-EXIT
              END-IF
           END-IF

           .
       3000-EXIT.
           EXIT.

052918 3500-edit-screen-data.
052918
052918     if actspcl > zeros
052918        if actspci <> 'Y' and 'N' and ' '
052918           move al-uabon         to actspca
052918           move -1               to actspcl
052918           move er-3271          to emi-error
052918           perform 9900-error-format
052918                                 thru 9900-exit
052918        else
052918           move al-uanon         to actspca
052918        end-if
052918     end-if
052918     
052918
052918     .
052918 3500-exit.
052918     exit.

       4000-ADD-ERACNT.

052918     perform 3500-edit-screen-data
052918                                 thru 3500-exit
052918
052918     if not emi-no-errors
052918        go to 8200-send-dataonly
052918     end-if

           PERFORM 4010-ADD-BLINE1     THRU 4010-EXIT
           PERFORM 4012-ADD-BLINE2     THRU 4012-EXIT
052918     PERFORM 4013-ADD-actspc     THRU 4013-EXIT

           PERFORM 4014-ADD-SNAME1     THRU 4014-EXIT
           PERFORM 4016-ADD-SNAME2     THRU 4016-EXIT

           PERFORM 4018-ADD-SADDR1     THRU 4018-EXIT
           PERFORM 4020-ADD-SADDR2     THRU 4020-EXIT
           PERFORM 4022-ADD-SADDR3     THRU 4022-EXIT

           PERFORM 4024-ADD-SCITYSTZP  THRU 4024-EXIT

           .
       4000-EXIT.
           EXIT.

       4010-ADD-BLINE1.

           IF BLINE1L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '2'                 TO WS-REC-TYPE
              MOVE +1                  TO WS-SEQ-NO

              EXEC CICS READ
                 DATASET  ('ERACNT')
                 INTO     (NOTE-FILE)
                 RIDFLD   (WS-CONTROL-PRIMARY)
                 RESP     (WS-RESPONSE)
              END-EXEC

              IF RESP-NOTFND
                 PERFORM 5010-BUILD-ERACNT
                                       THRU 5010-EXIT
                 MOVE BLINE1I          TO NT-BRANCH-LOC-LINE
                 PERFORM 5000-WRITE-ERACNT
                                       THRU 5000-EXIT
              END-IF
           END-IF

           .
       4010-EXIT.
           EXIT.

       4012-ADD-BLINE2.

           IF BLINE2L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '2'                 TO WS-REC-TYPE
              MOVE +2                  TO WS-SEQ-NO

              EXEC CICS READ
                 DATASET  ('ERACNT')
                 INTO     (NOTE-FILE)
                 RIDFLD   (WS-CONTROL-PRIMARY)
                 RESP     (WS-RESPONSE)
              END-EXEC

              IF RESP-NOTFND
                 PERFORM 5010-BUILD-ERACNT
                                       THRU 5010-EXIT
                 MOVE BLINE2I          TO NT-BRANCH-LOC-LINE
                 PERFORM 5000-WRITE-ERACNT
                                       THRU 5000-EXIT
              END-IF
           END-IF

           .
       4012-EXIT.
           EXIT.

052918 4013-ADD-actspc.
052918
052918     IF actspcL > ZEROS
052918        MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
052918        MOVE PI-ACCT-CARRIER     TO WS-CARRIER
052918        MOVE PI-ACCT-GROUPING    TO WS-GROUPING
052918        MOVE PI-ACCT-STATE       TO WS-STATE
052918        MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
052918        MOVE '2'                 TO WS-REC-TYPE
052918        MOVE +3                  TO WS-SEQ-NO
052918
052918        EXEC CICS READ
052918           DATASET  ('ERACNT')
052918           INTO     (NOTE-FILE)
052918           RIDFLD   (WS-CONTROL-PRIMARY)
052918           RESP     (WS-RESPONSE)
052918        END-EXEC
052918
052918        IF RESP-NOTFND
052918           PERFORM 5010-BUILD-ERACNT
052918                                 THRU 5010-EXIT
052918           MOVE actspci          TO NT-account-special
052918           PERFORM 5000-WRITE-ERACNT
052918                                 THRU 5000-EXIT
052918        END-IF
052918     END-IF
052918
052918     .
052918 4013-EXIT.
052918     EXIT.

       4014-ADD-SNAME1.

           IF SNAME1L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +1                  TO WS-SEQ-NO

              EXEC CICS READ
                 DATASET  ('ERACNT')
                 INTO     (NOTE-FILE)
                 RIDFLD   (WS-CONTROL-PRIMARY)
                 RESP     (WS-RESPONSE)
              END-EXEC

              IF RESP-NOTFND
                 PERFORM 5010-BUILD-ERACNT
                                       THRU 5010-EXIT
                 MOVE SNAME1I          TO NT-SHIPPING-LINE
                 PERFORM 5000-WRITE-ERACNT
                                       THRU 5000-EXIT
              END-IF
           END-IF

           .
       4014-EXIT.
           EXIT.

       4016-ADD-SNAME2.

           IF SNAME2L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +2                  TO WS-SEQ-NO

              EXEC CICS READ
                 DATASET  ('ERACNT')
                 INTO     (NOTE-FILE)
                 RIDFLD   (WS-CONTROL-PRIMARY)
                 RESP     (WS-RESPONSE)
              END-EXEC

              IF RESP-NOTFND
                 PERFORM 5010-BUILD-ERACNT
                                       THRU 5010-EXIT
                 MOVE SNAME2I          TO NT-SHIPPING-LINE
                 PERFORM 5000-WRITE-ERACNT
                                       THRU 5000-EXIT
              END-IF
           END-IF

           .
       4016-EXIT.
           EXIT.

       4018-ADD-SADDR1.

           IF SADDR1L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +3                  TO WS-SEQ-NO

              EXEC CICS READ
                 DATASET  ('ERACNT')
                 INTO     (NOTE-FILE)
                 RIDFLD   (WS-CONTROL-PRIMARY)
                 RESP     (WS-RESPONSE)
              END-EXEC

              IF RESP-NOTFND
                 PERFORM 5010-BUILD-ERACNT
                                       THRU 5010-EXIT
                 MOVE SADDR1I          TO NT-SHIPPING-LINE
                 PERFORM 5000-WRITE-ERACNT
                                       THRU 5000-EXIT
              END-IF
           END-IF

           .
       4018-EXIT.
           EXIT.

       4020-ADD-SADDR2.

           IF SADDR2L > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +4                  TO WS-SEQ-NO

              EXEC CICS READ
                 DATASET  ('ERACNT')
                 INTO     (NOTE-FILE)
                 RIDFLD   (WS-CONTROL-PRIMARY)
                 RESP     (WS-RESPONSE)
              END-EXEC

              IF RESP-NOTFND
                 PERFORM 5010-BUILD-ERACNT
                                       THRU 5010-EXIT
                 MOVE SADDR2I          TO NT-SHIPPING-LINE
                 PERFORM 5000-WRITE-ERACNT
                                       THRU 5000-EXIT
              END-IF
           END-IF

          .
       4020-EXIT.
           EXIT.

       4022-ADD-SADDR3.

           IF SADDR3L    > ZEROS
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +5                  TO WS-SEQ-NO

              EXEC CICS READ
                 DATASET  ('ERACNT')
                 INTO     (NOTE-FILE)
                 RIDFLD   (WS-CONTROL-PRIMARY)
                 RESP     (WS-RESPONSE)
              END-EXEC

              IF RESP-NOTFND
                 PERFORM 5010-BUILD-ERACNT
                                       THRU 5010-EXIT
                 MOVE SADDR3I          TO NT-SHIPPING-LINE
                 PERFORM 5000-WRITE-ERACNT
                                       THRU 5000-EXIT
              END-IF
           END-IF

           .
       4022-EXIT.
           EXIT.

       4024-ADD-SCITYSTZP.

           IF (SCITYL     > ZEROS)
              OR (SSTATEL > ZEROS)
              OR (SZIPL   > ZEROS)
              MOVE PI-COMPANY-CD       TO WS-COMPANY-CD
              MOVE PI-ACCT-CARRIER     TO WS-CARRIER
              MOVE PI-ACCT-GROUPING    TO WS-GROUPING
              MOVE PI-ACCT-STATE       TO WS-STATE
              MOVE PI-ACCT-ACCOUNT     TO WS-ACCOUNT
              MOVE '3'                 TO WS-REC-TYPE
              MOVE +6                  TO WS-SEQ-NO

              EXEC CICS READ
                 DATASET  ('ERACNT')
                 INTO     (NOTE-FILE)
                 RIDFLD   (WS-CONTROL-PRIMARY)
                 RESP     (WS-RESPONSE)
              END-EXEC

              IF RESP-NOTFND
                 PERFORM 5010-BUILD-ERACNT
                                       THRU 5010-EXIT
                 IF SCITYL > ZEROS
                    MOVE SCITYI        TO NT-SHIPPING-LINE
                 END-IF
                 IF SSTATEL > ZEROS
                    MOVE SSTATEI       TO NT-SHIP-STATE
                 END-IF
                 IF SZIPL > ZEROS
                    MOVE SZIPI         TO NT-SHIP-ZIP
                 END-IF
                 PERFORM 5000-WRITE-ERACNT
                                       THRU 5000-EXIT
              END-IF
           END-IF

           .
       4024-EXIT.
           EXIT.

       5000-WRITE-ERACNT.

           MOVE SAVE-BIN-DATE          TO NT-LAST-MAINT-DT
           MOVE EIBTIME                TO NT-LAST-MAINT-HHMMSS
           MOVE PI-PROCESSOR-ID        TO NT-LAST-MAINT-BY

           EXEC CICS WRITE
                FROM    (NOTE-FILE)
                DATASET ('ERACNT')
                RIDFLD  (WS-CONTROL-PRIMARY)
                RESP    (WS-RESPONSE)
           END-EXEC

           .
       5000-EXIT.
           EXIT.

       5010-BUILD-ERACNT.

           MOVE SPACES                 TO NOTE-FILE
           MOVE 'NT'                   TO NT-FILE-ID
           MOVE WS-CONTROL-PRIMARY     TO NT-CONTROL-PRIMARY
           MOVE SAVE-BIN-DATE          TO NT-LAST-MAINT-DT
           MOVE EIBTIME                TO NT-LAST-MAINT-HHMMSS
           MOVE PI-PROCESSOR-ID        TO NT-LAST-MAINT-BY

           .
       5010-EXIT.
           EXIT.

       5020-REWRITE-ERACNT.

           MOVE SAVE-BIN-DATE          TO NT-LAST-MAINT-DT
           MOVE EIBTIME                TO NT-LAST-MAINT-HHMMSS
           MOVE PI-PROCESSOR-ID        TO NT-LAST-MAINT-BY

           EXEC CICS REWRITE
                FROM    (NOTE-FILE)
                DATASET ('ERACNT')
                RESP    (WS-RESPONSE)
           END-EXEC

           .
       5020-EXIT.
           EXIT.

       8100-SEND-INITIAL-MAP.

           MOVE -1                     TO  MAINTYPL
           MOVE SAVE-DATE              TO  RUNDATEO
           MOVE EIBTIME                TO  TIME-IN
           MOVE TIME-OUT               TO  RUNTIMEO
                                                                        
           IF EMI-ERROR NOT = ZERO                                      
               PERFORM 9900-ERROR-FORMAT                                
           END-IF
                                                                        
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O
                                                                        
           EXEC CICS SEND                                               
               FROM   (EL6503AO)                                         
               MAPSET (WS-MAPSET-NAME)                                  
               MAP    (WS-MAP-NAME)                                     
               CURSOR ERASE                                             
           END-EXEC.                                                    
                                                                        
           GO TO 9100-RETURN-TRAN
           .
       8100-EXIT.                                                       
            EXIT.                                                       
                                                                        
       8200-SEND-DATAONLY.

           MOVE SAVE-DATE              TO  RUNDATEO
           MOVE EIBTIME                TO  TIME-IN
           MOVE TIME-OUT               TO  RUNTIMEO
                                                                        
           IF EMI-ERROR NOT = ZERO                                      
              PERFORM 9900-ERROR-FORMAT
           END-IF
                                                                        
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O
                                                                        
           EXEC CICS SEND DATAONLY                                      
               FROM   (EL6503AO)                                         
               MAPSET (WS-MAPSET-NAME)                                  
               MAP    (WS-MAP-NAME)                                     
               CURSOR                                                   
           END-EXEC.                                                    
                                                                        
           GO TO 9100-RETURN-TRAN
           .
       8200-EXIT.                                                       
            EXIT.                                                       
                                                                        
       8300-SEND-TEXT.

           EXEC CICS SEND TEXT                                          
               FROM   (LOGOFF-TEXT)                                     
               LENGTH (LOGOFF-LENGTH)                                   
               ERASE  FREEKB                                            
           END-EXEC.                                                    
                                                                        
           EXEC CICS RETURN                                             
           END-EXEC.                                                    
                                                                        
       8300-EXIT.                                                       
            EXIT.                                                       
                                                                        
       8400-LOG-JOURNAL-RECORD.

           IF PI-JOURNAL-FILE-ID = 0
              GO TO 8400-EXIT
           END-IF

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
                                                                        
       8500-DATE-CONVERSION.

           EXEC CICS LINK                                               
               PROGRAM  ('ELDATCV')                                     
               COMMAREA (DATE-CONVERSION-DATA)                          
               LENGTH   (DC-COMM-LENGTH)                                
           END-EXEC.                                                    
                                                                        
       8500-EXIT.                                                       
            EXIT.                                                       
                                                                        
       9000-RETURN-CICS.

           MOVE 'EL005'                TO  THIS-PGM.                    
           MOVE EIBAID                 TO  PI-ENTRY-CD-1.               
           PERFORM 9300-XCTL.                                           
                                                                        
       9000-EXIT.                                                       
            EXIT.                                                       

       9100-RETURN-TRAN.

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
                                                                        
           MOVE PI-RETURN-TO-PROGRAM   TO THIS-PGM
           PERFORM 9300-XCTL
           .
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
