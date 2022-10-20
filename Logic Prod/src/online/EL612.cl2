       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 EL612 .                              
      *                            VMOD=2.001.                          
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
      *    SCREENS     - EL612A - SELECTION CRITERIA                    
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
060608* 060608    2008040300002  PEMA  ADD EFF DT TO ELCLMI PASS AREA
      ******************************************************************
           EJECT                                                        
       ENVIRONMENT DIVISION.                                            
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
                                                                        
       77  FILLER  PIC X(32)  VALUE '********************************'. 
       77  FILLER  PIC X(32)  VALUE '*   EL612  WORKING STORAGE     *'. 
       77  FILLER  PIC X(32)  VALUE '***********VMOD=2.001 **********'. 
       77  CI1                PIC S999  COMP-3 VALUE +0.
       77  B1                 PIC S999  COMP-3 VALUE +0.

           COPY ELCSCTM.                                                
           COPY ELCSCRTY.                                               
                                                                        
       01  WS-DATE-AREA.                                                
           05  SAVE-DATE           PIC X(8)    VALUE SPACES.            
           05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            
                                                                        
       01  WS-INC-DATE                 PIC XX  VALUE LOW-VALUES.
       01  WS-RPT-DATE                 PIC XX  VALUE LOW-VALUES.
       01  WS-EST-DATE                 PIC XX  VALUE LOW-VALUES.
       01  WS-PRF-DATE                 PIC XX  VALUE LOW-VALUES.
       01  WS-EFF-DATE                 PIC XX  VALUE LOW-VALUES.
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
           05  ELCISC-LENGTH               PIC S9(4)   VALUE +100.
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
           05  WS-CONTROL-PRIMARY.
               10  WS-COMPANY-CD           PIC X      VALUE SPACES.
               10  WS-STATE                PIC XX     VALUE SPACES.
               10  WS-PROD                 PIC XX     VALUE SPACES.
               10  WS-COV-TYPE             PIC XX     VALUE SPACES.
           05  WS-CONTROL-FILE-KEY.                                     
               10  WS-CFK-COMPANY-ID       PIC X(3)    VALUE SPACES.    
               10  WS-CFK-RECORD-TYPE      PIC X       VALUE SPACES.    
               10  WS-CFK-STATE            PIC XX      VALUE SPACES.    
               10  WS-CFK-BENEFIT-CD       PIC XX      VALUE SPACES.    
               10  WS-CFK-SEQUENCE-NO      PIC S9(4)   VALUE ZERO COMP. 
                                                                        
           05  WS-MAPSET-NAME              PIC X(8)  VALUE 'EL612S'.    
           05  WS-MAP-NAME                 PIC X(8)  VALUE 'EL612A'.    
                                                                        
           05  FILLER                      REDEFINES                    
               WS-MAP-NAME.                                             
               10  FILLER                  PIC XX.                      
               10  WS-MAP-NUMBER           PIC X(4).                    
               10  FILLER                  PIC XX.                      
                                                                        
           05  THIS-PGM                    PIC X(8)  VALUE 'EL612'.     
                                                                        
           05  WS-JOURNAL-TYPE-ID          PIC XX      VALUE 'EL'.      
                                                                        
           05  WS-LOW-VALUES               PIC X VALUE LOW-VALUES.      
           05  WS-SPACE                    PIC X       VALUE SPACE.     
                                                                        
           05  WS-TRANS-ID                 PIC X(4)    VALUE 'EXAG'.    
                                                                        
           05  WS-TEMP-STORAGE-KEY.                                     
               10  WS-TS-TERM-ID           PIC X(4)    VALUE 'XXXX'.    
               10  FILLER                  PIC X(4)    VALUE '612'.

           05  WS-ERROR-MESSAGE-AREA.                                   
               10  ER-0000                 PIC 9(4)   VALUE 0000.
               10  ER-0004                 PIC 9(4)   VALUE 0004.
               10  ER-0006                 PIC 9(4)   VALUE 0006.
               10  ER-0008                 PIC 9(4)   VALUE 0008.
               10  ER-0023                 PIC 9(4)   VALUE 0023.
               10  ER-0029                 PIC 9(4)   VALUE 0029.
               10  ER-0070                 PIC 9(4)   VALUE 0070.
               10  ER-0150                 PIC 9(4)   VALUE 0150.
               10  ER-0196                 PIC 9(4)   VALUE 0196.
               10  ER-0419                 PIC 9(4)   VALUE 0419.
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
               10  ER-3811                 PIC 9(4)   VALUE 3811.
               10  ER-3812                 PIC 9(4)   VALUE 3812.
               10  ER-3813                 PIC 9(4)   VALUE 3813.
               10  ER-3814                 PIC 9(4)   VALUE 3814.
               10  ER-3815                 PIC 9(4)   VALUE 3815.
               10  ER-3816                 PIC 9(4)   VALUE 3816.
               10  ER-3817                 PIC 9(4)   VALUE 3817.
               10  ER-3818                 PIC 9(4)   VALUE 3818.
               10  ER-3819                 PIC 9(4)   VALUE 3819.
               10  ER-7220                 PIC 9(4)   VALUE 7220.
               10  ER-7698                 PIC 9(4)   VALUE 7698.
               10  ER-9999                 PIC 9(4)   VALUE 9999.

                                       COPY ELCINTF.

           12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                   
               16  PI-1ST-TIME-SW          PIC S9     COMP-3.           
               16  PI-MODE                 PIC X.
                   88  ADD-FUNCTION         VALUE 'A'.
               16  PI-CI-STATE             PIC XX.                      
               16  PI-CI-PROD              PIC XX.                      
               16  PI-CI-COV-TYPE          PIC XX.
               16  PI-CI-TOP-KEY           PIC X(7).
               16  PI-CI-BOT-KEY           PIC X(7).
               16  PI-CI-KEYS OCCURS 10.
                   20  PI-CI-KEY           PIC X(7).
               16  PI-LAST-BENEFIT-NUMBER  PIC XX.                      
               16  PI-NEXT-BENEFIT-NUMBER  PIC XX.                      
               16  PI-LINE-COUNT           PIC S9(3)  COMP-3.           
               16  PI-BROWSE-SW            PIC S9     COMP-3.           
               16  PI-SHOW-SW              PIC S9     COMP-3.           
               16  PI-CHANGE-SW            PIC S9     COMP-3.           
               16  PI-UPDATE-KEY           PIC X(10).                   
                                                                        
               16  FILLER                  PIC X(553).                  
                                                                        
           EJECT                                                        
                                                                        
                                       COPY EL612S.
                                       COPY ELCICALC.
                                                                        
                                                                        
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
                                                                        
           EJECT                                                        
       LINKAGE SECTION.                                                 

       01  DFHCOMMAREA                     PIC X(1024).                 

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
              MOVE LOW-VALUES          TO EL612AO
              MOVE -1                  TO APFKL
              MOVE ER-0008             TO EMI-ERROR
              GO TO 8200-SEND-DATAONLY
           END-IF

           EXEC CICS RECEIVE
               INTO   (EL612AO)
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

           IF EIBAID = DFHENTER
              CONTINUE
           ELSE
              MOVE ER-0008             TO EMI-ERROR
              MOVE -1                  TO APFKL
              GO TO 8200-SEND-DATAONLY
           END-IF

           IF SYSTEM-MODIFY-CAP
              CONTINUE
           ELSE
              MOVE 'UPDATE'            TO SM-READ
              PERFORM 9995-SECURITY-VIOLATION
              MOVE ER-0070             TO EMI-ERROR
              MOVE -1                  TO STATEL
              MOVE AL-UABON            TO STATEA
              GO TO 8200-SEND-DATAONLY
           END-IF
                                                                        
           IF EMI-FATAL-CTR > ZERO
              GO TO 8200-SEND-DATAONLY
           END-IF
                                                                        
           GO TO 3000-PROCESS-TEST
                                                                        
           MOVE 'LOGIC ERROR HAS OCCURRED - PROGRAM EL612'              
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

           MOVE LOW-VALUES             TO EL612AO

           MOVE -1                     TO STATEL
           MOVE +2                     TO WS-COMPLETED-SUCCESSFUL
           GO TO 8100-SEND-INITIAL-MAP

           .
      ***************************************************************** 
       3000-PROCESS-TEST.
      ***************************************************************** 

           PERFORM 6010-EDIT-DATA      THRU 6010-EXIT

           IF NOT EMI-NO-ERRORS
              GO TO 8200-SEND-DATAONLY
           END-IF

           MOVE PI-COMPANY-CD          TO CP-COMPANY-CD
           MOVE STATEI                 TO CP-STATE
           MOVE PRODI                  TO CP-PRODUCT
           MOVE COVI                   TO CP-COVERAGE
           MOVE WS-INC-DATE            TO CP-INC-DT
           MOVE WS-EST-DATE            TO CP-EST-DT
           MOVE WS-LSTPD-DATE          TO CP-LSTPD-DT
           MOVE WS-RPT-DATE            TO CP-RPT-DT
           MOVE WS-PRF-DATE            TO CP-PRF-DT
060608     MOVE WS-EFF-DATE            TO CP-EFF-DT
      *    MOVE CLMAMTI                TO CP-CLAIM-AMT
           
           PERFORM 7000-LINK-ELCLMI    THRU 7000-EXIT
      *    MOVE +123.45                TO CP-CLM-INT-AMT

           MOVE CP-CLM-INT-SW          TO QUALO
      *    IF CP-CLM-INT-SW = 'N'
      *       MOVE 'N'                 TO QUALO
      *    ELSE
      *       MOVE 'Y'                 TO QUALO
      *    END-IF

           IF NO-CI-ERROR
              MOVE CP-CLM-INT-AMT      TO INTAMTO
              MOVE CP-CLM-INT-RATE     TO INTRATO
              MOVE CP-CLM-INT-NODAYS   TO NODAYSO
              MOVE ZEROS               TO EMI-ERROR

              GO TO 8100-SEND-INITIAL-MAP
           END-IF

           EVALUATE CI-RETURN-CODE
              WHEN '1'
                 MOVE ER-3811          TO EMI-ERROR
              WHEN '2'
                 MOVE ER-3812          TO EMI-ERROR
              WHEN '3'
                 MOVE ER-3813          TO EMI-ERROR
              WHEN '4'
                 MOVE ER-3814          TO EMI-ERROR
              WHEN '5'
                 MOVE ER-3815          TO EMI-ERROR
              WHEN '6'
                 MOVE ER-3816          TO EMI-ERROR
              WHEN '7'
                 MOVE ER-3817          TO EMI-ERROR
              WHEN '8'
                 MOVE ER-3818          TO EMI-ERROR
              WHEN OTHER
                 MOVE ER-9999          TO EMI-ERROR
           END-EVALUATE

           MOVE AL-UABON               TO STATEA
           MOVE -1                     TO STATEL
           MOVE CI-RETURN-CODE         TO STATEO
           MOVE ZEROS                  TO INTAMTO
                                          INTRATO
                                          NODAYSO
           GO TO 8200-SEND-DATAONLY

           .
       6010-EDIT-DATA.
           
           IF STATEL > +0
              MOVE STATEI              TO CP-STATE
              MOVE AL-UANON            TO STATEA
           ELSE
              MOVE ER-0196             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO STATEA
              MOVE -1                  TO STATEL
           END-IF

           IF PRODL > +0
              MOVE PRODI               TO CP-PRODUCT
              MOVE AL-UANON            TO PRODA
           ELSE
              MOVE ER-3819             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO PRODA
              MOVE -1                  TO PRODL
           END-IF

           IF COVL > +0
              MOVE COVI                TO CP-COVERAGE
              MOVE AL-UANON            TO COVA
           ELSE
              MOVE ER-3819             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO COVA
              MOVE -1                  TO COVL
           END-IF

           IF INCDTL > +0
              MOVE INCDTI              TO DEEDIT-FIELD
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
                    MOVE AL-UANON      TO INCDTA
                    MOVE DC-BIN-DATE-1 TO WS-INC-DATE
                    MOVE DC-GREG-DATE-A-EDIT TO INCDTO
                 ELSE
                    MOVE ER-3801       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT
                    MOVE DEEDIT-FIELD  TO INCDTO
                    MOVE AL-UABON      TO INCDTA
                    MOVE -1            TO INCDTL
                 END-IF
              ELSE
                 MOVE ER-3801          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE DEEDIT-FIELD     TO INCDTO
                 MOVE AL-UABON         TO INCDTA
                 MOVE -1               TO INCDTL
              END-IF
           ELSE
              MOVE ER-3819             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO INCDTA
              MOVE -1                  TO INCDTL
           END-IF

           IF ESTDTL > +0
              MOVE ESTDTI              TO DEEDIT-FIELD
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
                    MOVE AL-UANON      TO ESTDTA
                    MOVE DC-BIN-DATE-1 TO WS-EST-DATE
                    MOVE DC-GREG-DATE-A-EDIT TO ESTDTO
                 ELSE
                    MOVE ER-3801       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT
                    MOVE DEEDIT-FIELD  TO ESTDTO
                    MOVE AL-UABON      TO ESTDTA
                    MOVE -1            TO ESTDTL
                 END-IF
              ELSE
                 MOVE ER-3801          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE DEEDIT-FIELD     TO ESTDTO
                 MOVE AL-UABON         TO ESTDTA
                 MOVE -1               TO ESTDTL
              END-IF
           ELSE
              MOVE ER-3819             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO ESTDTA
              MOVE -1                  TO ESTDTL
           END-IF

           IF LSTPDDTL > +0
              MOVE LSTPDDTI            TO DEEDIT-FIELD
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
                    MOVE AL-UANON      TO LSTPDDTA
                    MOVE DC-BIN-DATE-1 TO WS-LSTPD-DATE
                    MOVE DC-GREG-DATE-A-EDIT TO LSTPDDTO
                 ELSE
                    MOVE ER-3801       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT
                    MOVE DEEDIT-FIELD  TO LSTPDDTO
                    MOVE AL-UABON      TO LSTPDDTA
                    MOVE -1            TO LSTPDDTL
                 END-IF
              ELSE
                 MOVE ER-3801          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE DEEDIT-FIELD     TO LSTPDDTO
                 MOVE AL-UABON         TO LSTPDDTA
                 MOVE -1               TO LSTPDDTL
              END-IF
           ELSE
              MOVE SAVE-BIN-DATE       TO WS-LSTPD-DATE
           END-IF

           IF RPTDTL > +0
              MOVE RPTDTI              TO DEEDIT-FIELD
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
                    MOVE AL-UANON      TO RPTDTA
                    MOVE DC-BIN-DATE-1 TO WS-RPT-DATE
                    MOVE DC-GREG-DATE-A-EDIT TO RPTDTO
                 ELSE
                    MOVE ER-3801       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT
                    MOVE DEEDIT-FIELD  TO RPTDTO
                    MOVE AL-UABON      TO RPTDTA
                    MOVE -1            TO RPTDTL
                 END-IF
              ELSE
                 MOVE ER-3801          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE DEEDIT-FIELD     TO RPTDTO
                 MOVE AL-UABON         TO RPTDTA
                 MOVE -1               TO RPTDTL
              END-IF
           ELSE
              MOVE ER-3819             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO RPTDTA
              MOVE -1                  TO RPTDTL
           END-IF

           IF PRFDTL > +0
              MOVE PRFDTI              TO DEEDIT-FIELD
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
                    MOVE AL-UANON      TO PRFDTA
                    MOVE DC-BIN-DATE-1 TO WS-PRF-DATE
                    MOVE DC-GREG-DATE-A-EDIT TO PRFDTO
                 ELSE
                    MOVE ER-3801       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT
                    MOVE DEEDIT-FIELD  TO PRFDTO
                    MOVE AL-UABON      TO PRFDTA
                    MOVE -1            TO PRFDTL
                 END-IF
              ELSE
                 MOVE ER-3801          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE DEEDIT-FIELD     TO PRFDTO
                 MOVE AL-UABON         TO PRFDTA
                 MOVE -1               TO PRFDTL
              END-IF
           ELSE
              MOVE ER-3819             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO PRFDTA
              MOVE -1                  TO PRFDTL
           END-IF

           IF EFFDTL > +0
              MOVE EFFDTI              TO DEEDIT-FIELD
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
                    MOVE AL-UANON      TO EFFDTA
                    MOVE DC-BIN-DATE-1 TO WS-EFF-DATE
                    MOVE DC-GREG-DATE-A-EDIT TO EFFDTO
                 ELSE
                    MOVE ER-3801       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT
                    MOVE DEEDIT-FIELD  TO EFFDTO
                    MOVE AL-UABON      TO EFFDTA
                    MOVE -1            TO EFFDTL
                 END-IF
              ELSE
                 MOVE ER-3801          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE DEEDIT-FIELD     TO EFFDTO
                 MOVE AL-UABON         TO EFFDTA
                 MOVE -1               TO EFFDTL
              END-IF
           ELSE
              IF CP-STATE = 'UT'
                 MOVE ER-3819             TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON            TO EFFDTA
                 MOVE -1                  TO EFFDTL
              END-IF
           END-IF

           IF CLMAMTL > +0
      *       MOVE CLMAMTI             TO DEEDIT-FIELD
              EXEC CICS BIF DEEDIT
                  FIELD   (CLMAMTI)
                  LENGTH  (10)
              END-EXEC
              IF CLMAMTI NOT NUMERIC
                 MOVE ER-0419          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO CLMAMTA
                 MOVE -1               TO CLMAMTL
              ELSE
                 MOVE CLMAMTI          TO CP-CLAIM-AMT
                 MOVE CP-CLAIM-AMT     TO CLMAMTO
                 MOVE AL-UNNON         TO CLMAMTA
              END-IF
           ELSE
              MOVE ER-3819             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
              MOVE AL-UABON            TO CLMAMTA
              MOVE -1                  TO CLMAMTL
           END-IF

           IF ZINTRATL > +0
              EXEC CICS BIF DEEDIT
                  FIELD   (ZINTRATI)
                  LENGTH  (08)
              END-EXEC
              IF ZINTRATI NOT NUMERIC
                 MOVE ER-0419          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO ZINTRATA
                 MOVE -1               TO ZINTRATL
              ELSE
                 MOVE ZINTRATI         TO CP-INT-RATE
                 MOVE CP-INT-RATE      TO ZINTRATO
                 MOVE AL-UNNON         TO ZINTRATA
              END-IF
           ELSE
              MOVE ZEROS               TO CP-INT-RATE
           END-IF

           .
       6010-EXIT.
           EXIT.
           
       7000-LINK-ELCLMI.
       
           EXEC CICS LINK
              PROGRAM   ('ELCLMI')
              COMMAREA  (CLAIM-INT-PASS-AREA)
              LENGTH    (CP-CLAIM-LENGTH)
           END-EXEC

           .
       7000-EXIT.
           EXIT.

       8100-SEND-INITIAL-MAP.
      ***************************************************************** 
                                                                        
           MOVE ZERO                   TO  PI-BROWSE-SW.                
                                                                        
           MOVE -1                     TO  STATEL
           MOVE SAVE-DATE              TO  ADATEO.                      
           MOVE EIBTIME                TO  TIME-IN.                     
           MOVE TIME-OUT               TO  ATIMEO.                      
                                                                        
           IF EMI-ERROR NOT = ZERO                                      
               PERFORM 9900-ERROR-FORMAT                                
           END-IF
                                                                        
           MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.                    
           MOVE EMI-MESSAGE-AREA (2)    TO  AEMSG2O.                    
                                                                        
           EXEC CICS SEND                                               
               FROM   (EL612AO)                                         
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
               FROM   (EL612AO)                                         
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
